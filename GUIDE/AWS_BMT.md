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


__B.__ Disable the Foreign keys.

- Total 49.

```
ALTER TABLE ACCOUNT_PERMISSION DISABLE CONSTRAINT FK_CA_AP_CC;
ALTER TABLE ADDRESS01 DISABLE CONSTRAINT FK_ZC_AD_CC;
ALTER TABLE BROKER DISABLE CONSTRAINT FK_ST_B_CC;
ALTER TABLE CASH_TRANSACTION DISABLE CONSTRAINT FK_T_CT_CC;
ALTER TABLE CHARGE DISABLE CONSTRAINT FK_TT_CH_CC;
ALTER TABLE COMMISSION_RATE DISABLE CONSTRAINT FK_EX_CR_CC;
ALTER TABLE COMMISSION_RATE DISABLE CONSTRAINT FK_TT_CR_CC;
ALTER TABLE COMPANY DISABLE CONSTRAINT FK_AD_CO_CC;
ALTER TABLE COMPANY DISABLE CONSTRAINT FK_IN_CO_CC;
ALTER TABLE COMPANY DISABLE CONSTRAINT FK_ST_CO_CC;
ALTER TABLE COMPANY_COMPETITOR DISABLE CONSTRAINT FK_CO_CP_CC;
ALTER TABLE COMPANY_COMPETITOR DISABLE CONSTRAINT FK_CO_CP_CC;
ALTER TABLE COMPANY_COMPETITOR DISABLE CONSTRAINT FK_IN_CP_CC;
ALTER TABLE CUSTOMER DISABLE CONSTRAINT FK_AD_C_CC;
ALTER TABLE CUSTOMER DISABLE CONSTRAINT FK_ST_C_CC;
ALTER TABLE CUSTOMER_ACCOUNT DISABLE CONSTRAINT FK_B_CA_CC;
ALTER TABLE CUSTOMER_ACCOUNT DISABLE CONSTRAINT FK_C_CA_CC;
ALTER TABLE CUSTOMER_TAXRATE DISABLE CONSTRAINT FK_C_CX_CC;
ALTER TABLE CUSTOMER_TAXRATE DISABLE CONSTRAINT FK_TX_CX_CC;
ALTER TABLE DAILY_MARKET DISABLE CONSTRAINT FK_S_DM_CC;
ALTER TABLE EXCHANGE DISABLE CONSTRAINT FK_AD_EX_CC;
ALTER TABLE FINANCIAL DISABLE CONSTRAINT FK_CO_FI_CC;
ALTER TABLE HOLDING DISABLE CONSTRAINT FK_HS_H_CC;
ALTER TABLE HOLDING DISABLE CONSTRAINT FK_T_H_CC;
ALTER TABLE HOLDING_HISTORY DISABLE CONSTRAINT FK_T_HH_CC;
ALTER TABLE HOLDING_HISTORY DISABLE CONSTRAINT FK_T_HH_CC;
ALTER TABLE HOLDING_SUMMARY DISABLE CONSTRAINT FK_CA_HS_CC;
ALTER TABLE HOLDING_SUMMARY DISABLE CONSTRAINT FK_S_HS_CC;
ALTER TABLE INDUSTRY DISABLE CONSTRAINT FK_SC_IN_CC;
ALTER TABLE LAST_TRADE DISABLE CONSTRAINT FK_S_LT_CC;
ALTER TABLE NEWS_XREF DISABLE CONSTRAINT FK_CO_NX_CC;
ALTER TABLE NEWS_XREF DISABLE CONSTRAINT FK_NI_NX_CC;
ALTER TABLE SECURITY DISABLE CONSTRAINT FK_CO_S_CC;
ALTER TABLE SECURITY DISABLE CONSTRAINT FK_EX_S_CC;
ALTER TABLE SECURITY DISABLE CONSTRAINT FK_ST_S_CC;
ALTER TABLE SETTLEMENT DISABLE CONSTRAINT FK_T_SE_CC;
ALTER TABLE TRADE DISABLE CONSTRAINT FK_CA_T_CC;
ALTER TABLE TRADE DISABLE CONSTRAINT FK_S_T_CC;
ALTER TABLE TRADE DISABLE CONSTRAINT FK_ST_T_CC;
ALTER TABLE TRADE DISABLE CONSTRAINT FK_TT_T_CC;
ALTER TABLE TRADE_HISTORY DISABLE CONSTRAINT FK_ST_TH_CC;
ALTER TABLE TRADE_HISTORY DISABLE CONSTRAINT FK_T_TH_CC;
ALTER TABLE TRADE_REQUEST DISABLE CONSTRAINT FK_B_TR_CC;
ALTER TABLE TRADE_REQUEST DISABLE CONSTRAINT FK_S_TR_CC;
ALTER TABLE TRADE_REQUEST DISABLE CONSTRAINT FK_T_TR_CC;
ALTER TABLE TRADE_REQUEST DISABLE CONSTRAINT FK_TT_TR_CC;
ALTER TABLE WATCH_ITEM DISABLE CONSTRAINT FK_S_WI_CC;
ALTER TABLE WATCH_ITEM DISABLE CONSTRAINT FK_WL_WI_CC;
ALTER TABLE WATCH_LIST DISABLE CONSTRAINT FK_C_WL_CC;
```
__C.__ Load the data.

- oftibr@OFDB1:/opt2/tmaxdb/zrefdata/azure_load_resutlts /> cat tbloader.sh

```
#!/bin/bash
#
# Generic load using tbloader. Load files are comma-delimited exported from MS ACCESS.
# Uses sed commands to massage data before loading.
#
# EXIT CODES
# 0 - Success.
#
# Writen by Dave Regan david.regan@tmaxsoft.com Dec. 2017
#
# TODO: Use Temporary Files
##########################################################################

#-------------------------------
#
# FUNCTION: Create .ctl file.
#
# PARM #1 - Table Name.
#
# RETURN 0
# /> tbloader -h
#
#tbLoader 6
#
#TmaxData Corporation Copyright (c) 2008-. All rights reserved.
#
#Usage : tbloader [options] [controls]
#
#Options:
# -h|--help Display the more detailed information.
# -c|--charset Display usable character sets in this version.
# -v|--version Display the version information.
#
#Controls:
# userid=<username/password@dbname>
# Logon information to connect. The dbname has to
# be matched with the name described in tbdsn.tbr exactly.
#
# control=<filename>
# The name of the control file. Allowed to use a absolute path and
# a relative path.
#
# data=<filename>
# The name of the data file to load. Allowed to use a absolute path
# and a relative path.
#
# log=<filename>
# The name of the log file. This is for logging the entire loading
# process. Allowed to use a absolute path and a relative path. If this
# parameter is omitted, it is used <control filename>.log as default.
#
# bad=<filename>
# The name of the bad file. This is for logging the records to be failed
# to load. Allowed to use a absolute path and a relative path. If this
# parameter is omitted, it is used <data filename>.bad as default.
#
# discard=<filename>
# The name of the discard file. This is for logging the records to be
# discarded. Allowed to use a absolute path and a relative path. If this
# parameter is omitted, the records does not be logged.
#
# skip=<n>
# The count of line to skip in data file from the start line.
# The default is 0.
#
# errors=<n>
# The count of error to allow. If error is raised more than this value,
# abort the loading immediately. The default is 50.
#
# rows=<n>
# The count of row for each commit. If this value is smaller, the
# performance is lower. The default is 'commit after the end'.
#
# message=<n>
# The count of records to process to print out. The value is smaller,
# the performance is slower. The default is 0.
#
# readsize=<n>
# The size of buffer to read from the data file. This value is allowed
# to use the maximum 2M(2,097,152) byte. The default is 65536 byte.
#
# disable_idx=<Y|N>
# Whether to disable indexes before loading. The default is 'N'.
#
# direct=<Y|N>
# Whether to use the Direct Path Loading. If you choose 'y|Y', load by
# the Direct Path Loading. Or load by the conventional loading.
# The default is 'N'.
#
# dpl_log=<Y|N>
# Whether to log about the Direct Path Loading.
# If you choose 'y|Y', be logged the loading process and use it by
# recovery. But the performance is low. If you choose 'n|N', do not
# be logged. The default is 'N'.
#
# parallel=<n>
# The count of threads to execute Parallel Direct Path Loading.
# The default is 1.
#
# bindsize=<n>
# The size of buffer to read in the Direct Path Loading. This value is
# allowed to use the maximum 15M(15,728,640) byte.
# The default is 65536 byte.
#
# dpl_parallel=<Y|N>
# Deprecated.
#
# multithread=<Y|N>
# Deprecated.
#
#-------------------------------
function create_ctl_file () {

CTL_FILE="loader_$1.ctl"

# Have to create before for some reason...
touch $CTL_FILE
chmod 775 $CTL_FILE

echo "
LOAD DATA
--INFILE '/home/oframe7/Dave/DRAWING.txt'
INFILE '$1.txt'
LOGFILE '$1.log'
BADFILE '$1.bad'
TRUNCATE
--REPLACE
--APPEND
INTO TABLE ZREF.$1
--INTO TABLE TIBERO.$1
--FIELDS TERMINATED BY ','
FIELDS TERMINATED BY '|'
OPTIONALLY ENCLOSED BY '\"'
-- ESCAPED BY '\\\\'
--LINES TERMINATED BY '\n'
--IGNORE 1 LINES
--DIRECT=Y
TRAILING NULLCOLS" > $CTL_FILE

#chmod 775 $CTL_FILE

return 0
}

#-------------------------------
#
# FUNCTION: Load table via tbloader.
#
# PARM #1 - Table Name.
# PARM #2 - Additional Parms.
#
# RETURN 0
#
#-------------------------------
function load_table () {
echo
echo "**** Loading data for $1 ****"

# Have to create before for some reason...
touch $1.log
touch $1.bad

chmod 775 $1.log
chmod 775 $1.bad

CTL_FILE="loader_$1.ctl"

#TB_CMD="tbloader userid=sys/tibero@oframe control=$CTL_FILE $2"
#TB_CMD="tbloader userid=tibero/tmax@TVSAM control=$CTL_FILE $2"
# TB_CMD="tbloader userid=tibero/tmax@TVSAM control=$CTL_FILE $2 message=1000000 rows=10000000"
# TB_CMD="tbloader userid=tibero/tmax@TVSAM control=$CTL_FILE $2 message=1000000 rows=1000000"
# TB_CMD="tbloader userid=tibero/tmax@TVSAM control=$CTL_FILE $2 message=1000000 rows=1000000 direct=Y bindsize=10000000 parallel=1"
TB_CMD="tbloader userid=tibero/tmax@TVSAM control=$CTL_FILE $2 message=10000000 rows=10000000 direct=Y disable_idx=Y"
echo $TB_CMD
$TB_CMD

#chmod 775 $1.log
#chmod 775 $1.bad

cat $1.log

return 0
}

#-------------------------------
#
# FUNCTION: Massage data prior to loading - does it in place - may want to back up original.
#
# sed -i 's/original/new/g' file.txt
# Explanation:
# sed = Stream EDitor
# -i = in-place (i.e. save back to the original file)
# The command string:
# • s = the substitute command
# • original = a regular expression describing the word to replace (or just the word itself)
# • new = the text to replace it with
# • g = global (i.e. replace all and not just the first occurrence)
# file.txt = the file name
#
#
# PARM #1 - Table Name.
#
# RETURN 0
#
#-------------------------------
function massage_data () {

echo
echo "**** Massaging data for $1 ****"

# Escape double quotes inside double quotes at start of field...
# ,""" -> ,"\"
sed -i 's/,"""/,"\\"/g' $1.txt

# Escape double quotes inside double quotes at end of field...
# """, -> \"",
sed -i 's/""",/\\"",/g' $1.txt

# ABOVE 2 DON'T ACCOUNT FOR FIRST FIELD OR LAST FIELD!!!

# Escape double quotes inside double quotes...
# "" -> \"
sed -i 's/""/\\"/g' $1.txt

# Remove time portion of timestamp...
sed -i 's/ 0:00:00//g' $1.txt

# For HIST only - SEQ field cannot be NULL...
if [ "$1" = "HIST" ];then
sed -i 's/,," "/," "," "/g' $1.txt
fi

return 0
}

#-------------------------------
#
# FUNCTION: Count # rows and put in file for later retrieval.
#
# PARM #1 - Table Name.
#
# RETURN 0
#
#-------------------------------
function count_rows () {

echo

return 0
}

#-------------------------------
#
# FUNCTION: Prep and load the data. 1) Massage load file 2) Create control file 3) Load it.
#
# PARM #1 - Table Name.
#
# RETURN 0
#
#-------------------------------
function load_data () {

echo
echo "**** Processing Table $1 ****"
echo "**** Processing Table $1 ****"
echo "**** Processing Table $1 ****"

now=$(date +"%T")
echo "Current Time : $now"
echo

#massage_data $1
create_ctl_file $1
load_table $1 $2
#count_rows $1

return 0
}

##############################################################################################
# MAIN Program
#
# Load each table for a particular database...
# Phsically add extra parms if needed.
#
# rows=10000000 - limit to 10 million rows loading.
# message=1000000 - display every million rows loaded.
#
# TBR-7156: Referenced by foreign keys. (Take FK constraints off??)
#
##############################################################################################

echo 'Process ID: ' $$

#load_data COMP rows=100000
#load_data ACT_COMP
#load_data BROKER
#load_data HOLDING
#load_data HOLDING_HISTORY
#load_data HOLDING_SUMMARY
#load_data SETTLEMENT
#load_data TRADE
#load_data TRADE_HISTORY
#load_data TRADE_REQUEST
#load_data CASH_TRANSACTION
#load_data DAILY_MARKET

#load_data DAILY_MARKET2
#load_data SECTOR
#load_data NEWS_ITEM
#load_data COMPANY
#load_data WATCH_ITEM
#load_data HOLDING_HISTORY load=11000000
#load_data SETTLEMENT

now=$(date +"%T")
echo
echo "Current Time : $now"

#exit 0;

echo "LEVEL 1 LOADS..."
#load_data NEXT_ID
load_data SECTOR
load_data STATUS_TYPE
load_data TAXRATE
load_data TRADE_TYPE
load_data ZIP_CODE
#load_data NEWS_ITEM

echo "LEVEL 2 LOADS..."
#load_data ADDRESS
load_data ADDRESS01
load_data BROKER
load_data CHARGE
load_data CUSTOMER
load_data INDUSTRY

echo "LEVEL 3 LOADS..."
load_data COMPANY
load_data CUSTOMER_ACCOUNT
load_data CUSTOMER_TAXRATE
load_data EXCHANGE
load_data WATCH_LIST

echo "LEVEL 4 LOADS..."
load_data ACCOUNT_PERMISSION
load_data COMMISSION_RATE
load_data COMPANY_COMPETITOR
load_data FINANCIAL
load_data NEWS_XREF
load_data SECURITY

echo "LEVEL 5 LOADS..."
load_data LAST_TRADE
load_data HOLDING_SUMMARY
load_data WATCH_ITEM
load_data TRADE ## 1.5 Hours to load
load_data DAILY_MARKET ## Many hours to load 134M,

echo "LEVEL 6 LOADS..."
load_data CASH_TRANSACTION ## 2.5 Hours to load
load_data HOLDING
load_data HOLDING_HISTORY ## Hours to load
load_data SETTLEMENT ## Hours to load
load_data TRADE_HISTORY ## 4.5 Hours to load

echo "Done Loading"

now=$(date +"%T")
echo
echo "Current Time : $now"
echo

exit 0;
```


__D.__ Rebuild the indexes. 

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
__C.__ Create Foreign keys.




__D.__ Create the Foreign keys.



__F.__ Enable the Foreign keys.

- Total 49.

```
ALTER TABLE ACCOUNT_PERMISSION ENABLE CONSTRAINT FK_CA_AP_CC;
ALTER TABLE ADDRESS01 ENABLE CONSTRAINT FK_ZC_AD_CC;
ALTER TABLE BROKER ENABLE CONSTRAINT FK_ST_B_CC;
ALTER TABLE CASH_TRANSACTION ENABLE CONSTRAINT FK_T_CT_CC;
ALTER TABLE CHARGE ENABLE CONSTRAINT FK_TT_CH_CC;
ALTER TABLE COMMISSION_RATE ENABLE CONSTRAINT FK_EX_CR_CC;
ALTER TABLE COMMISSION_RATE ENABLE CONSTRAINT FK_TT_CR_CC;
ALTER TABLE COMPANY ENABLE CONSTRAINT FK_AD_CO_CC;
ALTER TABLE COMPANY ENABLE CONSTRAINT FK_IN_CO_CC;
ALTER TABLE COMPANY ENABLE CONSTRAINT FK_ST_CO_CC;
ALTER TABLE COMPANY_COMPETITOR ENABLE CONSTRAINT FK_CO_CP_CC;
ALTER TABLE COMPANY_COMPETITOR ENABLE CONSTRAINT FK_CO_CP_CC;
ALTER TABLE COMPANY_COMPETITOR ENABLE CONSTRAINT FK_IN_CP_CC;
ALTER TABLE CUSTOMER ENABLE CONSTRAINT FK_AD_C_CC;
ALTER TABLE CUSTOMER ENABLE CONSTRAINT FK_ST_C_CC;
ALTER TABLE CUSTOMER_ACCOUNT ENABLE CONSTRAINT FK_B_CA_CC;
ALTER TABLE CUSTOMER_ACCOUNT ENABLE CONSTRAINT FK_C_CA_CC;
ALTER TABLE CUSTOMER_TAXRATE ENABLE CONSTRAINT FK_C_CX_CC;
ALTER TABLE CUSTOMER_TAXRATE ENABLE CONSTRAINT FK_TX_CX_CC;
ALTER TABLE DAILY_MARKET ENABLE CONSTRAINT FK_S_DM_CC;
ALTER TABLE EXCHANGE ENABLE CONSTRAINT FK_AD_EX_CC;
ALTER TABLE FINANCIAL ENABLE CONSTRAINT FK_CO_FI_CC;
ALTER TABLE HOLDING ENABLE CONSTRAINT FK_HS_H_CC;
ALTER TABLE HOLDING ENABLE CONSTRAINT FK_T_H_CC;
ALTER TABLE HOLDING_HISTORY ENABLE CONSTRAINT FK_T_HH_CC;
ALTER TABLE HOLDING_HISTORY ENABLE CONSTRAINT FK_T_HH_CC;
ALTER TABLE HOLDING_SUMMARY ENABLE CONSTRAINT FK_CA_HS_CC;
ALTER TABLE HOLDING_SUMMARY ENABLE CONSTRAINT FK_S_HS_CC;
ALTER TABLE INDUSTRY ENABLE CONSTRAINT FK_SC_IN_CC;
ALTER TABLE LAST_TRADE ENABLE CONSTRAINT FK_S_LT_CC;
ALTER TABLE NEWS_XREF ENABLE CONSTRAINT FK_CO_NX_CC;
ALTER TABLE NEWS_XREF ENABLE CONSTRAINT FK_NI_NX_CC;
ALTER TABLE SECURITY ENABLE CONSTRAINT FK_CO_S_CC;
ALTER TABLE SECURITY ENABLE CONSTRAINT FK_EX_S_CC;
ALTER TABLE SECURITY ENABLE CONSTRAINT FK_ST_S_CC;
ALTER TABLE SETTLEMENT ENABLE CONSTRAINT FK_T_SE_CC;
ALTER TABLE TRADE ENABLE CONSTRAINT FK_CA_T_CC;
ALTER TABLE TRADE ENABLE CONSTRAINT FK_S_T_CC;
ALTER TABLE TRADE ENABLE CONSTRAINT FK_ST_T_CC;
ALTER TABLE TRADE ENABLE CONSTRAINT FK_TT_T_CC;
ALTER TABLE TRADE_HISTORY ENABLE CONSTRAINT FK_ST_TH_CC;
ALTER TABLE TRADE_HISTORY ENABLE CONSTRAINT FK_T_TH_CC;
ALTER TABLE TRADE_REQUEST ENABLE CONSTRAINT FK_B_TR_CC;
ALTER TABLE TRADE_REQUEST ENABLE CONSTRAINT FK_S_TR_CC;
ALTER TABLE TRADE_REQUEST ENABLE CONSTRAINT FK_T_TR_CC;
ALTER TABLE TRADE_REQUEST ENABLE CONSTRAINT FK_TT_TR_CC;
ALTER TABLE WATCH_ITEM ENABLE CONSTRAINT FK_S_WI_CC;
ALTER TABLE WATCH_ITEM ENABLE CONSTRAINT FK_WL_WI_CC;
ALTER TABLE WATCH_LIST ENABLE CONSTRAINT FK_C_WL_CC;
```
__G.__ 

