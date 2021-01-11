# AWS BMT by Kelsey

## Table of Contents

+ [1. DB Migration](#db-migration)
  + [1.1 Install docker](#11-install-docker)
  + [1.2 Get centos container](#12-get-centos-container)
  + [1.3 Install OpenFrame](#13-install-openframe)
      + [1.3.1 Pre settings](#131-pre-settings)
      + [1.3.2 JAVA installation](#132-java-installation)
      + [1.3.3 Tibero installation](#133-tibero-installation)
      + [1.3.4 UnixODBC installation](#134-unixodbc-installation)
      + [1.3.5 OFCOBOL installation](#135-ofcobol-installation)
      + [1.3.6 PROSORT installation](#136-prosort-installation)
      + [1.3.7 Base installation](#137-base-installation)
      + [1.3.8 Batch installation](#138-batch-installation)
      + [1.3.9 TACF installation](#139-tacf-installation)
      + [1.3.10 OSC installation](#1310-osc-installation)
      + [1.3.11 JEUS installation](#1311-jeus-installation)
      + [1.3.12 OFGW installation](#1312-ofgw-installation)
      + [1.3.13 OFManager installation](#1313-ofmanager-installation)
      + [1.3.14 OFMiner installation](#1314-ofminer-installation)
  + [1.4 Create OpenFrame image](#14-create-openframe-image)
  + [1.5 Use OpenFrame image](#15-use-openframe-image)
+ [2. Azure Service](#step-2-azure-service)
  + [2.1 Add Azure Kubernetes service(AKS)](#21-add-azure-kubernetes-serviceaks)
  + [2.2 Set Pods](#22-set-pods)
  + [2.3 Connect to the running Pod](#23-connect-to-the-running-pod)
  + [2.4 Set services](#23-set-services)
  + [2.5 Network configuration](#24-network-configuration)


## 1. DB Migration

### 1.1 Generate DB bulk data using EgenLoader

```
oftibr@OFDB1:/opt2/tmaxdb/EGENLOADER/bin /> ./EGenLoader -h
EGen v1.14.0
Usage:
EGenLoader [options]

 Where
  Option                       Default     Description
   -b number                   1           Beginning customer ordinal position
   -c number                   5000        Number of customers (for this instance)
   -t number                   5000        Number of customers (total in the database)
   -f number                   500         Scale factor (customers per 1 tpsE)
   -w number                   300          Number of Workdays (8-hour days) of
                                           initial trades to populate
   -i dir                      flat_in/    Directory for input files
   -l [FLAT|ODBC|CUSTOM|NULL]  FLAT        Type of load
   -m [APPEND|OVERWRITE]       OVERWRITE   Flat File output mode
   -o dir                      flat_out/   Directory for output files

   -x                          -x          Generate all tables
   -xf                                     Generate all fixed-size tables
   -xd                                     Generate all scaling and growing tables
                                           (equivalent to -xs -xg)
   -xs                                     Generate scaling tables
                                           (except BROKER)
   -xg                                     Generate growing tables and BROKER
   -g                                      Disable caching when generating growing tables
```

```
oftibr@OFDB1:/opt2/tmaxdb/EGENLOADER/bin /> ./EGenLoader -c 150000 -xf -w 60
EGen v1.14.0


Using the following settings.

        Load Type:              Flat File
        Output Mode:            OVERWRITE
        Out Directory:          flat_out/
        In Directory:           flat_in/
        Start From Customer:    1
        Customer Count:         150000
        Total customers:        150000
        Load Unit:              1000
        Scale Factor:           500
        Initial Trade Days:     60
        Caching Enabled:        true


Generating CHARGE table...loaded.

Generating COMMISSION_RATE table...loaded.

Generating EXCHANGE table...loaded.

Generating INDUSTRY table...loaded.

Generating SECTOR table...loaded.

Generating STATUS_TYPE table...loaded.

Generating TAXRATE table...loaded.

Generating TRADE_TYPE table...loaded.

Generating ZIP_CODE table...loaded.


Generate and load time: 00:00:06
```

```
oftibr@OFDB1:/opt2/tmaxdb/EGENLOADER/bin /> ./EGenLoader -c 150000 -xs -w 60
EGen v1.14.0


Using the following settings.

        Load Type:              Flat File
        Output Mode:            OVERWRITE
        Out Directory:          flat_out/
        In Directory:           flat_in/
        Start From Customer:    1
        Customer Count:         150000
        Total customers:        150000
        Load Unit:              1000
        Scale Factor:           500
        Initial Trade Days:     60
        Caching Enabled:        true


Generating ADDRESS table..............loaded.

Generating CUSTOMER table..........loaded.

Generating CUSTOMER_ACCOUNT table and ACCOUNT_PERMISSION table......................loaded.

Generating CUSTOMER_TAX_RATE table..................loaded.

Generating WATCH_LIST table and WATCH_ITEM table...........................loaded.

Generating COMPANY table...loaded.

Generating COMPANY_COMPETITOR table...loaded.

Generating DAILY_MARKET table....loaded.

Generating FINANCIAL table....................................loaded.

Generating LAST TRADE table...loaded.

Generating NEWS_ITEM and NEWS_XREF table...........................loaded.

Generating SECURITY table........loaded.


Generate and load time: 00:16:22
```

```
oftibr@OFDB1:/opt2/tmaxdb/EGENLOADER/bin /> ./EGenLoader -c 50000 -xg -w 60
EGen v1.14.0


Using the following settings.

        Load Type:              Flat File
        Output Mode:            OVERWRITE
        Out Directory:          flat_out/
        In Directory:           flat_in/
        Start From Customer:    1
        Customer Count:         50000
        Total customers:        50000
        Load Unit:              1000
        Scale Factor:           500
        Initial Trade Days:     60
        Caching Enabled:        true


Generating TRADE, SETTLEMENT, TRADE HISTORY, CASH TRANSACTION, HOLDING_HISTORY, HOLDING_SUMMARY, HOLDING, and BROKER tables...........50.loaded.


Generate and load time: 01:22:58
```

- Total 35 tables.
```
ACCOUNT_PERMISSION
ADDRESS01
BROKER
CASH_TRANSACTION
CHARGE
COMMISSION_RATE
COMPANY
COMPANY_COMPETITOR
CUSTOMER
CUSTOMER_ACCOUNT
CUSTOMER_TAXRATE
DAILY_MARKET
EXCHANGE
FINANCIAL
HOLDING
HOLDING_HISTORY
HOLDING_SUMMARY
INDUSTRY
LAST_TRADE
NEWS_ITEM
NEWS_XREF
NEXT_ID
SECTOR
SECURITY
SETTLEMENT
STATUS_TYPE
TAXRATE
TRADE
TRADE_HISTORY
TRADE_REQUEST
TRADE_TYPE
TRADEX
WATCH_ITEM
WATCH_LIST
ZIP_CODE
```

- Those tables are empty. They do not have any data in it.

```
TRADE_REQUEST
NEXT_ID
TRADEX
```

__A.__ 


Create empty tables with the correct column and key information.

