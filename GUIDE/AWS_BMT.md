# AWS BMT by Kelsey

## Table of Contents

+ [1. DB Migration](#db-migration)
  + [1.1 Install docker](#11-install-docker)
  + [1.2 Get centos container](#12-get-centos-container)
  + [1.3 Install OpenFrame](#13-install-openframe)
      + [1.3.1 Pre settings](#131-pre-settings)
  + [1.4 Create OpenFrame image](#14-create-openframe-image)
  + [1.5 Use OpenFrame image](#15-use-openframe-image)
+ [2. Batch Unit Test](#step-2-azure-service)
  + [2.1 Add Azure Kubernetes service(AKS)](#21-add-azure-kubernetes-serviceaks)
  + [2.2 Set Pods](#22-set-pods)
  + [2.3 Connect to the running Pod](#23-connect-to-the-running-pod)
+ [3. Online Unit Test](#step-2-azure-service)
  + [3.1 Add Azure Kubernetes service(AKS)](#21-add-azure-kubernetes-serviceaks)
  + [3.2 Set Pods](#22-set-pods)
  + [3.3 Connect to the running Pod](#23-connect-to-the-running-pod)

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

1) Create tablespaces (seperate)

- Create Data tablepace
```
DROP TABLESPACE ZREF_DATA INCLUDING CONTENTS AND DATAFILES; 

CREATE TABLESPACE ZREF_DATA DATAFILE
 '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_DATA.dbf' 
SIZE 16M AUTOEXTEND 
ON NEXT 5M 
MAXSIZE 34M
LOGGING
ONLINE
PERMANENT
EXTENT MANAGEMENT LOCAL AUTOALLOCATE;
```

- Add datafile
```
ALTER TABLESPACE ZREF_DATA ADD DATAFILE '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_DATA02.dbf' SIZE 20G;
ALTER TABLESPACE ZREF_DATA ADD DATAFILE '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_DATA03.dbf' SIZE 20G;
ALTER TABLESPACE ZREF_DATA ADD DATAFILE '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_DATA04.dbf' SIZE 20G;
ALTER TABLESPACE ZREF_DATA ADD DATAFILE '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_DATA05.dbf' SIZE 20G;
```

- Create Index tablepace
```
DROP TABLESPACE ZREF_INDEX_TS INCLUDING CONTENTS AND DATAFILES; 

CREATE TABLESPACE ZREF_INDEX_TS DATAFILE
 '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_IDX.dbf' 
SIZE 8M AUTOEXTEND 
ON NEXT 5M 
MAXSIZE 34M
LOGGING
ONLINE
PERMANENT
EXTENT MANAGEMENT LOCAL AUTOALLOCATE;
```

- Add datafile
```
ALTER TABLESPACE ZREF_INDEX_TS ADD DATAFILE '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_IDX02.dbf' SIZE 20G;
ALTER TABLESPACE ZREF_INDEX_TS ADD DATAFILE '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_IDX03.dbf' SIZE 20G;
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

__A.__ Create empty tables with the correct column and key information.

__B.__ Rebuild the indexes. 

- Total 56.

```
ALTER INDEX IDX_B_ID_NAME_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_B_NAME_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_C_ID_TIER_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_C_TAX_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_CA_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_CO_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_CO_NAME_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_CP_ID  REBUILD PARALLEL 4;
ALTER INDEX IDX_CX_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_DM_SYMB_DATE_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_HH_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_ID_SYMB_DTS_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_IN_NAME_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_NE_KEY_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_NX_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_S_ID_ISS_EX_SYMB_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_SC_NAME_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_T_ID_DTS_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_T_SYMB_DTS_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_TR_ID_SYMB_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_TR_SYMB_ID_BID_QTY_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_TT_ID_MKT_SELL_NM_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX IDX_WL_ID_UNIQ  REBUILD PARALLEL 4;
ALTER INDEX PK_AD  REBUILD PARALLEL 4;
ALTER INDEX PK_AP  REBUILD PARALLEL 4;
ALTER INDEX PK_B  REBUILD PARALLEL 4;
ALTER INDEX PK_C  REBUILD PARALLEL 4;
ALTER INDEX PK_CA  REBUILD PARALLEL 4;
ALTER INDEX PK_CH  REBUILD PARALLEL 4;
ALTER INDEX PK_CO  REBUILD PARALLEL 4;
ALTER INDEX PK_CP  REBUILD PARALLEL 4;
ALTER INDEX PK_CR  REBUILD PARALLEL 4;
ALTER INDEX PK_CT  REBUILD PARALLEL 4;
ALTER INDEX PK_CX  REBUILD PARALLEL 4;
ALTER INDEX PK_DM  REBUILD PARALLEL 4;
ALTER INDEX PK_EX  REBUILD PARALLEL 4;
ALTER INDEX PK_FI  REBUILD PARALLEL 4;
ALTER INDEX PK_H  REBUILD PARALLEL 4;
ALTER INDEX PK_HH  REBUILD PARALLEL 4;
ALTER INDEX PK_HS  REBUILD PARALLEL 4;
ALTER INDEX PK_IN  REBUILD PARALLEL 4;
ALTER INDEX PK_LT  REBUILD PARALLEL 4;
ALTER INDEX PK_NI  REBUILD PARALLEL 4;
ALTER INDEX PK_NX  REBUILD PARALLEL 4;
ALTER INDEX PK_S  REBUILD PARALLEL 4;
ALTER INDEX PK_SC  REBUILD PARALLEL 4;
ALTER INDEX PK_SE  REBUILD PARALLEL 4;
ALTER INDEX PK_ST  REBUILD PARALLEL 4;
ALTER INDEX PK_T  REBUILD PARALLEL 4;
ALTER INDEX PK_TH  REBUILD PARALLEL 4;
ALTER INDEX PK_TR  REBUILD PARALLEL 4;
ALTER INDEX PK_TT  REBUILD PARALLEL 4;
ALTER INDEX PK_TX  REBUILD PARALLEL 4;
ALTER INDEX PK_WI  REBUILD PARALLEL 4;
ALTER INDEX PK_WL  REBUILD PARALLEL 4;
ALTER INDEX PK_ZC  REBUILD PARALLEL 4;
```
__C.__ Turn off the Foreign keys.

__D.__ Turn on the Foreign keys.

__E.__ 

__F.__ 

__G.__ 

