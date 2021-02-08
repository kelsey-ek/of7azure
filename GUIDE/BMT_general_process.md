# AWS BMT by Kelsey

## Table of Contents

+ [1. Check process of BMT](#db-migration)
+ [2. Check the target result of BMT](#db-migration)
+ [3. Follow the process](#step-2-azure-service)
  + [3.1 Planning](#21-add-azure-kubernetes-serviceaks)
  + [3.2 Pre-setting](#22-set-pods)
  + [3.3 Configure OpenFrame](#23-connect-to-the-running-pod)
    + [3.3.1 Online](#331-connect-to-the-running-pod)
    + [3.3.2 Batch](#332-connect-to-the-running-pod)
  + [3.4 Prepare applications](#23-connect-to-the-running-pod)
    + [3.4.1 Online](#331-connect-to-the-running-pod)
    + [3.4.2 Batch](#332-connect-to-the-running-pod)    
  + [3.5 DB migration](23-connect-to-the-running-pod)

### Installation

- vi /etc/hosts (Both)

- Use private ip.


```
127.0.0.1   localhost localhost.localdomain localhost4 localhost4.localdomain4
::1         localhost6 localhost6.localdomain6
10.0.1.212  OFAPP1 OFAPP1
10.0.2.78   OFDB1  OFDB1
```

```
sudo hostname OFDB1
```


g. Add user as hostname

groupadd mqm -g 10000
useradd -d /home/oframe -g mqm -s /bin/bash -m oframe -u 10001
groupadd dba -g 10005
useradd -d /home/oftibr -g dba -s /bin/bash -m oftibr -u 10002

[ec2-user@ip-10-0-1-73 ~]$ sudo passwd oftibr
Changing password for user oftibr.
New password: 
Retype new password: 
passwd: all authentication tokens updated successfully.



- OpenFrame bash_profile

```
# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
. ~/.bashrc
fi
PS1=$HOSTNAME'@$LOGNAME:$PWD>'
LS_COLORS="di=01;35:or=1;40;31:*.gz=1;35:*.bz2=1;35:*.tar=1;35:*.h=1;37:*.c=1;37"
export LS_COLORS
alias cdof='cd $OPENFRAME_HOME'
alias ulog='cd $OPENFRAME_HOME/log/tmax/ulog'
alias sysjcl='cd $OPENFRAME_HOME/volume_default/SYS1.JCLLIB'
alias defvol='cd $OPENFRAME_HOME/volume_default'
alias tmaxsw='cd /opt2/tmaxsw'
alias tmaxapp='cd /opt2/tmaxapp/'
alias tmaxdb='cd /opt2/tmaxdb'
alias tmaxui='cd /opt2/tmaxui'
alias ofminer='cd /opt2/tmaxapp/OFMiner/repository'
alias jeuslog='cd /opt2/tmaxapp/jeus7/domains/jeus_domain/servers/server1/logs'
alias ofgwlog='cd /opt2/tmaxapp/jeus7/domains/jeus_domain/servers/server1/lib/application/logs'
alias zref='cd /opt2/tmaxapp/zref'
alias tmax='cd /opt2/tmaxapp/zref/Tmaxwork'
alias test='cd /opt2/tmaxapp/zref/Tmaxwork/TEST'
alias kelsey='cd /opt2/tmaxapp/zref/Tmaxwork/KELSEY'
alias sel='cd /opt2/tmaxapp/zref/Tmaxwork/SELENA'
alias bat05='cd /opt2/tmaxapp/zref/Tmaxwork/SELENA/BATTX05'
alias dave='cd /opt2/tmaxapp/zref/Tmaxwork/DAVE'
alias bat='cd /opt2/tmaxapp/zref/Tmaxwork/TEST/COBOL/batch'
alias cic='cd /opt2/tmaxapp/zref/Tmaxwork/TEST/COBOL/cics'

export boost_path=/opt2/tmaxapp/zref/HPC/boost_1_57_0
export PATH=$PATH:/opt2/tmaxapp/zref/HPC/boost_1_57_0
#User specific environment and startup programs
export DOTNET_ROOT=/opt2/tmaxapp/zref/HPC/DOTNET
export PATH=$PATH:/opt2/tmaxapp/zref/HPC/DOTNET

PATH=$PATH:$HOME/.local/bin:$HOME/bin
export PATH

# shell script path
PATH=$PATH:/opt2/tmaxapp/zref/script/
export PATH

#CURRENT_TIMESTAMP
#export TB_NLS_TIMESTAMP_FORMAT='YYYY-MM-DD-HH24.MI.SS.FF6'

# UNIX ODBC ENV
export ODBC_HOME=/opt2/tmaxapp/unixODBC
export PATH=$ODBC_HOME/bin:$PATH
export LD_LIBRARY_PATH=$ODBC_HOME/lib:$LD_LIBRARY_PATH
export ODBCINI=$ODBC_HOME/etc/odbc.ini
export ODBCSYSINI=$ODBC_HOME

# Tibero6 ENV
export TB_HOME=/opt2/tmaxapp/tibero6
export TB_SID=TVSAM
export TB_PROF_DIR=$TB_HOME/bin/prof
export LD_LIBRARY_PATH=$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH_64=$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH_64
export PATH=$TB_HOME/bin:$TB_HOME/client/bin:$PATH:$TB_HOME/script

# New environment setting added by OpenFrame COBOL 4.0 on Mon Apr 13 07:28:43 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1867721042.
# Do NOT modify these lines; they are used to uninstall.
OFCOB_HOME=/opt2/tmaxapp/OFCOBOL
export OFCOB_HOME
# End comments by InstallAnywhere on Mon Apr 13 07:28:43 UTC 2020 1.

# New environment setting added by OpenFrame COBOL 4.0 on Mon Apr 13 07:28:43 UTC 2020 2.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1867721042.
# Do NOT modify these lines; they are used to uninstall.
COBPARSER_HOME=$OFCOB_HOME/cobolparser
export COBPARSER_HOME
# End comments by InstallAnywhere on Mon Apr 13 07:28:43 UTC 2020 2.

# New environment setting added by OpenFrame COBOL 4.0 on Mon Apr 13 07:28:43 UTC 2020 3.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1867721042.
# Do NOT modify these lines; they are used to uninstall.
LLVM_HOME=$OFCOB_HOME/llvm
export LLVM_HOME
# End comments by InstallAnywhere on Mon Apr 13 07:28:43 UTC 2020 3.

# New environment setting added by OpenFrame COBOL 4.0 on Mon Apr 13 07:28:43 UTC 2020 4.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1867721042.
# Do NOT modify these lines; they are used to uninstall.
PATH="${PATH}:$OFCOB_HOME/bin:$COBPARSER_HOME/bin:$LLVM_HOME/bin"
export PATH
# End comments by InstallAnywhere on Mon Apr 13 07:28:43 UTC 2020 4.

# New environment setting added by OpenFrame COBOL 4.0 on Mon Apr 13 07:28:43 UTC 2020 5.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1867721042.
# Do NOT modify these lines; they are used to uninstall.
LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:$OFCOB_HOME/lib:$COBPARSER_HOME/lib:$LLVM_HOME/lib"
export LD_LIBRARY_PATH
# End comments by InstallAnywhere on Mon Apr 13 07:28:43 UTC 2020 5.

# New environment setting added by OpenFrame COBOL 4.0 on Mon Apr 13 07:28:43 UTC 2020 6.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1867721042.
# Do NOT modify these lines; they are used to uninstall.
OFCOBCPY="${OFCOBCPY}:$OFCOB_HOME/copybook"
export OFCOBCPY
# End comments by InstallAnywhere on Mon Apr 13 07:28:43 UTC 2020 6.

# OFPLI ENV
export OFPLI_HOME=/opt2/tmaxapp/OFPLI
export PATH="${PATH}:$OFPLI_HOME/bin:"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:$OFPLI_HOME/lib"

# OFASM ENV
export OFASM_HOME=/opt2/tmaxapp/OFASM
export OFASM_MACLIB=$OFASM_HOME/maclib/ofmac
export PATH="${PATH}:$OFASM_HOME/bin:"
export LD_LIBRARY_PATH="./:$OFASM_HOME/lib:$LD_LIBRARY_PATH"

# PROSORT ENV
export PROSORT_HOME=/opt2/tmaxapp/prosort
export PROSORT_SID=gbg
export PATH=$PATH:$PROSORT_HOME/bin
export LD_LIBRARY_PATH=$PROSORT_HOME/lib:$LD_LIBRARY_PATH
export LIBPATH=$PROSORT_HOME/lib:$LD_LIBRARY_PATH

# New environment setting added by OpenFrame_PLI on Mon Apr 13 08:44:03 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile26784584.
# Do NOT modify these lines; they are used to uninstall.
OFPLI_HOME=/opt2/tmaxapp/OFPLI
export OFPLI_HOME
# End comments by InstallAnywhere on Mon Apr 13 08:44:03 UTC 2020 1.

# New environment setting added by OpenFrame_PLI on Mon Apr 13 08:44:03 UTC 2020 2.
# The unmodified version of this file is saved in /home/oframe/.bash_profile26784584.
# Do NOT modify these lines; they are used to uninstall.
PATH="${PATH}:$OFPLI_HOME/bin:"
export PATH
# End comments by InstallAnywhere on Mon Apr 13 08:44:03 UTC 2020 2.

# New environment setting added by OpenFrame_PLI on Mon Apr 13 08:44:03 UTC 2020 3.
# The unmodified version of this file is saved in /home/oframe/.bash_profile26784584.
# Do NOT modify these lines; they are used to uninstall.
LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:$OFPLI_HOME/lib"
export LD_LIBRARY_PATH
# End comments by InstallAnywhere on Mon Apr 13 08:44:03 UTC 2020 3.

# New environment setting added by OpenFrame_ASM on Mon Apr 13 08:48:03 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile123115485.
# Do NOT modify these lines; they are used to uninstall.
OFASM_HOME=/opt2/tmaxapp/OFASM
export OFASM_HOME
# End comments by InstallAnywhere on Mon Apr 13 08:48:03 UTC 2020 1.

# New environment setting added by OpenFrame_ASM on Mon Apr 13 08:48:03 UTC 2020 2.
# The unmodified version of this file is saved in /home/oframe/.bash_profile123115485.
# Do NOT modify these lines; they are used to uninstall.
PATH="${PATH}:$OFASM_HOME/bin:"
export PATH
# End comments by InstallAnywhere on Mon Apr 13 08:48:03 UTC 2020 2.

# New environment setting added by OpenFrame_ASM on Mon Apr 13 08:48:03 UTC 2020 3.
# The unmodified version of this file is saved in /home/oframe/.bash_profile123115485.
# Do NOT modify these lines; they are used to uninstall.
LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:$OFASM_HOME/lib"
export LD_LIBRARY_PATH
# End comments by InstallAnywhere on Mon Apr 13 08:48:03 UTC 2020 3.

# New environment setting added by OpenFrame_ASM on Mon Apr 13 08:48:03 UTC 2020 4.
# The unmodified version of this file is saved in /home/oframe/.bash_profile123115485.
# Do NOT modify these lines; they are used to uninstall.
OFASM_MACLIB=$OFASM_HOME/maclib/ofmac:$OFASM_HOME/maclib/
export OFASM_MACLIB
# End comments by InstallAnywhere on Mon Apr 13 08:48:03 UTC 2020 4.


# JEUS ENV
export JEUS_HOME=/opt2/tmaxapp/jeus7

# JAVA ENV
export JAVA_HOME=/usr/java/jdk1.7.0_79/
export PATH=$JAVA_HOME/bin:$PATH
export CLASSPATH=$CLASSPATH:$JAVA_HOME/jre/lib/ext:$JAVA_HOME/lib/tools.jar

alias dsboot='startDomainAdminServer -domain jeus_domain -u administrator -p tmax123'
alias msboot1='startManagedServer -domain jeus_domain -server server1 -u administrator -p tmax123'
alias msboot2='startManagedServer -domain jeus_domain -server server2 -u administrator -p tmax123'
alias msdown1='stopServer -u administrator -p tmax123 -host localhost:9936'
alias msdown2='stopServer -u administrator -p tmax123 -host localhost:9636'
alias dsdown='stopServer -u administrator -p tmax123 -host localhost:9736'

# End comments by InstallAnywhere on Mon Apr 13 10:32:20 UTC 2020 1.
alias python=/usr/local/bin/python3.7
alias pip=/usr/local/bin/pip3.7

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
OPENFRAME_HOME=/opt2/tmaxapp/OpenFrame
export OPENFRAME_HOME
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 1.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 2.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
PATH="${PATH}:${OPENFRAME_HOME}/bin:${OPENFRAME_HOME}/util"
export PATH
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 2.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 3.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:$OPENFRAME_HOME/lib"
export LD_LIBRARY_PATH
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 3.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 4.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
LD_LIBRARY_PATH_64="${LD_LIBRARY_PATH_64}:$OPENFRAME_HOME/lib"
export LD_LIBRARY_PATH_64
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 4.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 5.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
TMAXDIR=/opt2/tmaxapp/OpenFrame/core
export TMAXDIR
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 5.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 6.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
TMAX_HOST_ADDR=127.0.0.1
export TMAX_HOST_ADDR
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 6.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 7.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
TMAX_HOST_PORT=8001
export TMAX_HOST_PORT
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 7.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 8.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
PATH="${PATH}:$TMAXDIR/bin"
export PATH
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 8.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 9.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
FDLFILE=$TMAXDIR/fdl/oframe.fdl
export FDLFILE
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 9.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 10.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
TDLDIR=$TMAXDIR/tdl
export TDLDIR
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 10.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 11.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:$TMAXDIR/lib"
export LD_LIBRARY_PATH
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 11.

# New environment setting added by OpenFrame_Base7_Fix3 on Tue Dec 01 01:39:25 UTC 2020 12.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365660301.
# Do NOT modify these lines; they are used to uninstall.
LD_LIBRARY_PATH_64="${LD_LIBRARY_PATH_64}:$TMAXDIR/lib64"
export LD_LIBRARY_PATH_64
# End comments by InstallAnywhere on Tue Dec 01 01:39:25 UTC 2020 12.




# New environment setting added by JEUS7.0 on Tue Dec 01 05:15:37 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1469689962.
# Do NOT modify these lines; they are used to uninstall.
PATH="/opt2/tmaxapp/jeus7/bin:/opt2/tmaxapp/jeus7/lib/system:/opt2/tmaxapp/jeus7/webserver/bin:${PATH}"
export PATH
# End comments by InstallAnywhere on Tue Dec 01 05:15:37 UTC 2020 1.

# New environment setting added by OpenFrame_GW_7_Fix1 on Tue Dec 01 06:04:36 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1617093246.
# Do NOT modify these lines; they are used to uninstall.
OFGW_HOME=/opt2/tmaxapp/jeus7/domains/jeus_domain/servers/server1/lib/application
export OFGW_HOME
# End comments by InstallAnywhere on Tue Dec 01 06:04:36 UTC 2020 1.

# New environment setting added by OpenFrame_Manager7.1 on Tue Dec 01 06:45:42 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile1365346253.
# Do NOT modify these lines; they are used to uninstall.
OFMANAGER_HOME=/opt2/tmaxapp/jeus7/domains/jeus_domain/servers/server2/lib/application/ofmanager
export OFMANAGER_HOME
# End comments by InstallAnywhere on Tue Dec 01 06:45:42 UTC 2020 1.


# New environment setting added by OpenFrame_Miner_7_Fix1 on Tue Dec 01 07:07:07 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile626870240.
# Do NOT modify these lines; they are used to uninstall.
OFMINER_HOME=/opt2/tmaxapp/OFMiner
export OFMINER_HOME
# End comments by InstallAnywhere on Tue Dec 01 07:07:07 UTC 2020 1.

# New environment setting added by OpenFrame_OSC7_Fix3 on Tue Dec 01 23:22:58 UTC 2020 1.
# The unmodified version of this file is saved in /home/oframe/.bash_profile405696543.
# Do NOT modify these lines; they are used to uninstall.
OFCOBCPY="${OFCOBCPY}:${OPENFRAME_HOME}/osc/copybook:${OPENFRAME_HOME}/osc/region/OSCOIVP1/map/symbolic"
export OFCOBCPY
# End comments by InstallAnywhere on Tue Dec 01 23:22:58 UTC 2020 1.

# New environment setting added by OpenFrame_OSC7_Fix3 on Tue Dec 01 23:22:58 UTC 2020 2.
# The unmodified version of this file is saved in /home/oframe/.bash_profile405696543.
# Do NOT modify these lines; they are used to uninstall.
TCACHECONF=${TMAXDIR}/config/pfmtcache.cfg
export TCACHECONF
# End comments by InstallAnywhere on Tue Dec 01 23:22:58 UTC 2020 2.

# New environment setting added by OpenFrame_OSC7_Fix3 on Tue Dec 01 23:22:58 UTC 2020 3.
# The unmodified version of this file is saved in /home/oframe/.bash_profile405696543.
# Do NOT modify these lines; they are used to uninstall.
PFM_ADM_LOG_DIR=${OPENFRAME_HOME}/log/sys
export PFM_ADM_LOG_DIR
# End comments by InstallAnywhere on Tue Dec 01 23:22:58 UTC 2020 3.
```

- TIBERO bash_profile

```
# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin

export PATH

HOSTNAME=`hostname`
USERID=`whoami`
export PS1='${USERID}@${HOSTNAME}:${PWD} /> '

################################################################################
#
#       TSAM/TIBERO (or TIBERO VSAM)
#
################################################################################

TB_HOME=/opt2/tmaxdb/tibero6; export TB_HOME
TB_SID=TVSAM; export TB_SID
SEM_KEY=148050; export SEM_KEY
TB_PROF_DIR=$TB_HOME/bin/prof; export TB_PROF_DIR
PATH=$TB_HOME/script:$TB_HOME/bin:$TB_HOME/client/bin:$PATH; export PATH
LD_LIBRARY_PATH=$TB_HOME/lib:$TB_HOME/client/lib:/lib:$LD_LIBRARY_PATH; export LD_LIBRARY_PATH
LD_LIBRARY_PATH_64=$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH_64; export LD_LIBRARY_PATH_64
export TB_NLS_DATE_FORMAT="YYYY-MM-DD HH24:MI:SS"
TBMON_HOME=$TB_HOME/tbmon; export TBMON_HOME
AIXTHREAD_SCOPE=S; export AIXTHREAD_SCOPE

#TB_CONN_TIMEOUT=10; export TB_CONN_TIMEOUT
#TB_READ_TIMEOUT=180; export TB_READ_TIMEOUT
```

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

__A.__ Fixed tables.

```
1. CHARGE
2. COMMISSION_RATE
3. EXCHANGE
4. INDUSTRY
5. SECTOR
6. STATUS_TYPE
7. TAXRATE
8. TRADE_TYPE
9. ZIP_CODE
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

__B.__ Scaling tables.

```
1. ADDRESS
2. CUSTOMER
3. CUSTOMER_ACCOUNT 
4. ACCOUNT_PERMISSION
5. CUSTOMER_TAX_RATE
6. WATCH_LIST
7. WATCH_ITEM 
8. COMPANY
9. COMPANY_COMPETITOR
10. DAILY_MARKET
11. FINANCIAL
12. LAST TRADE
13. NEWS_ITEM
14. NEWS_XREF 
15. SECURITY
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

__C.__ Growing tables.

```
1. TRADE
2. SETTLEMENT
3. TRADE HISTORY
4. CASH TRANSACTION
5. HOLDING_HISTORY
6. HOLDING_SUMMARY
7. HOLDING
8. BROKER
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

- Create Data tablepace.
    - Becareful not to use autoextend. It will later cause the 'TBR-21004: No more extent available in tablespace' issue.
```
DROP TABLESPACE ZREF_DATA INCLUDING CONTENTS AND DATAFILES; 

CREATE TABLESPACE ZREF_DATA DATAFILE
 '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_DATA01.dbf' 
SIZE 20G
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

- Create Index tablepace.
    - Becareful not to use autoextend. It will later cause the 'TBR-21004: No more extent available in tablespace' issue.
```
DROP TABLESPACE ZREF_INDEX_TS INCLUDING CONTENTS AND DATAFILES; 

CREATE TABLESPACE ZREF_INDEX_TS DATAFILE
 '/opt2/tmaxdb/tibero6/database/TVSAM/ZREF_IDX.dbf' 
SIZE 20G
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

1. ACCOUNT_PERMISSION
```
CREATE TABLE ACCOUNT_PERMISSION (
 AP_CA_ID NUMBER(19) NOT NULL,
 AP_ACL CHAR(4) NOT NULL,
 AP_TAX_ID VARCHAR(20) NOT NULL,
 AP_L_NAME VARCHAR(25) NOT NULL,
 AP_F_NAME VARCHAR(20) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

2. ADDRESS01
```
CREATE TABLE ADDRESS01 (
 AD_ID NUMBER(19) NOT NULL,
 AD_LINE1 VARCHAR(80),
 AD_LINE2 VARCHAR(80),
 AD_ZC_CODE VARCHAR(12) NOT NULL,
 AD_CTRY VARCHAR(80)
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

3. BROKER
```
CREATE TABLE BROKER (
 B_ID NUMBER(19) NOT NULL,
 B_ST_ID CHAR(4) NOT NULL,
 B_NAME VARCHAR(49) NOT NULL,
 B_NUM_TRADES NUMBER(9) NOT NULL,
 B_COMM_TOTAL NUMBER(12,2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

4. CASH_TRANSACTION
```
CREATE TABLE CASH_TRANSACTION (
 CT_T_ID NUMBER(19) NOT NULL,
 CT_DTS TIMESTAMP(6) DEFAULT current_timestamp,
 CT_AMT NUMBER(10,2) NOT NULL,
 CT_NAME VARCHAR(100)
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

5. CHARGE
```
CREATE TABLE CHARGE (
 CH_TT_ID CHAR(3) NOT NULL,
 CH_C_TIER NUMBER(3) NOT NULL,
 CH_CHRG NUMBER(10,2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

6. COMMISSION_RATE
```
CREATE TABLE COMMISSION_RATE (
 CR_C_TIER NUMBER(3) NOT NULL,
 CR_TT_ID CHAR(3) NOT NULL,
 CR_EX_ID CHAR(6) NOT NULL,
 CR_FROM_QTY NUMBER(6) NOT NULL,
 CR_TO_QTY NUMBER(6) NOT NULL,
 CR_RATE NUMBER(5,2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

7. COMPANY
```
CREATE TABLE COMPANY (
 CO_ID NUMBER(19) NOT NULL,
 CO_ST_ID CHAR(4) NOT NULL,
 CO_NAME VARCHAR(60) NOT NULL,
 CO_IN_ID CHAR(2) NOT NULL,
 CO_SP_RATE CHAR(4) NOT NULL,
 CO_CEO VARCHAR(46) NOT NULL,
 CO_AD_ID NUMBER(19) NOT NULL,
 CO_DESC VARCHAR(150) NOT NULL,
 CO_OPEN_DATE DATE NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

8. COMPANY_COMPETITOR
```
CREATE TABLE COMPANY_COMPETITOR (
 CP_CO_ID NUMBER(19) NOT NULL,
 CP_COMP_CO_ID NUMBER(19) NOT NULL,
 CP_IN_ID CHAR(2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

9. CUSTOMER
```
CREATE TABLE CUSTOMER (
 C_ID NUMBER(19) NOT NULL,
 C_TAX_ID VARCHAR(20) NOT NULL,
 C_ST_ID CHAR(4) NOT NULL,
 C_L_NAME VARCHAR(25) NOT NULL,
 C_F_NAME VARCHAR(20) NOT NULL,
 C_M_NAME CHAR(1),
 C_GNDR CHAR(1),
 C_TIER NUMBER(3) NOT NULL,
 C_DOB DATE NOT NULL,
 C_AD_ID NUMBER(19) NOT NULL,
 C_CTRY_1 CHAR(3),
 C_AREA_1 CHAR(3),
 C_LOCAL_1 VARCHAR(10),
 C_EXT_1 VARCHAR(5),
 C_CTRY_2 CHAR(3),
 C_AREA_2 CHAR(3),
 C_LOCAL_2 VARCHAR(10),
 C_EXT_2 VARCHAR(5),
 C_CTRY_3 CHAR(3),
 C_AREA_3 CHAR(3),
 C_LOCAL_3 VARCHAR(10),
 C_EXT_3 VARCHAR(5),
 C_EMAIL_1 VARCHAR(50),
 C_EMAIL_2 VARCHAR(50)
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

10. CUSTOMER_ACCOUNT
```
CREATE TABLE CUSTOMER_ACCOUNT (
 CA_ID NUMBER(19) NOT NULL,
 CA_B_ID NUMBER(19) NOT NULL,
 CA_C_ID NUMBER(19) NOT NULL,
 CA_NAME VARCHAR(50),
 CA_TAX_ST NUMBER(3) NOT NULL,
 CA_BAL NUMBER(12,2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

11. CUSTOMER_TAXRATE
```
CREATE TABLE CUSTOMER_TAXRATE (
 CX_TX_ID CHAR(4) NOT NULL,
 CX_C_ID NUMBER(19) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

12. DAILY_MARKET
```
CREATE TABLE DAILY_MARKET (
 DM_DATE DATE NOT NULL,
 DM_S_SYMB VARCHAR(15) NOT NULL,
 DM_CLOSE NUMBER(8,2) NOT NULL,
 DM_HIGH NUMBER(8,2) NOT NULL,
 DM_LOW NUMBER(8,2) NOT NULL,
 DM_VOL NUMBER(12) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

13. EXCHANGE
```
CREATE TABLE EXCHANGE (
 EX_ID CHAR(6) NOT NULL,
 EX_NAME CHAR(100) NOT NULL,
 EX_NUM_SYMB NUMBER(6) NOT NULL,
 EX_OPEN NUMBER NOT NULL,
 EX_CLOSE NUMBER NOT NULL,
 EX_DESC CHAR(150),
 EX_AD_ID NUMBER(19) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

14. FINANCIAL
```
CREATE TABLE FINANCIAL (
 FI_CO_ID NUMBER(19) NOT NULL,
 FI_YEAR NUMBER NOT NULL,
 FI_QTR NUMBER(3) NOT NULL,
 FI_QTR_START_DATE DATE NOT NULL,
 FI_REVENUE NUMBER(15,2) NOT NULL,
 FI_NET_EARN NUMBER(15,2) NOT NULL,
 FI_BASIC_EPS NUMBER(10,2) NOT NULL,
 FI_DILUT_EPS NUMBER(10,2) NOT NULL,
 FI_MARGIN NUMBER(10,2) NOT NULL,
 FI_INVENTORY NUMBER(15,2) NOT NULL,
 FI_ASSETS NUMBER(15,2) NOT NULL,
 FI_LIABILITY NUMBER(15,2) NOT NULL,
 FI_OUT_BASIC NUMBER(12) NOT NULL,
 FI_OUT_DILUT NUMBER(12) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

15. HOLDING
```
CREATE TABLE HOLDING (
 H_T_ID NUMBER(19) NOT NULL,
 H_CA_ID NUMBER(19) NOT NULL,
 H_S_SYMB VARCHAR(15) NOT NULL,
 H_DTS TIMESTAMP(6) DEFAULT current_timestamp,
 H_PRICE NUMBER(8,2) NOT NULL,
 H_QTY NUMBER(6) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

16. HOLDING_HISTORY
```
CREATE TABLE HOLDING_HISTORY (
 HH_H_T_ID NUMBER(19) NOT NULL,
 HH_T_ID NUMBER(19) NOT NULL,
 HH_BEFORE_QTY NUMBER(6) NOT NULL,
 HH_AFTER_QTY NUMBER(6) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

17. HOLDING_SUMMARY
```
CREATE TABLE HOLDING_SUMMARY (
 HS_CA_ID NUMBER(19) NOT NULL,
 HS_S_SYMB VARCHAR(15) NOT NULL,
 HS_QTY NUMBER(6) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

18. INDUSTRY
```
CREATE TABLE INDUSTRY (
 IN_ID CHAR(2) NOT NULL,
 IN_NAME CHAR(50) NOT NULL,
 IN_SC_ID CHAR(2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

19. LAST_TRADE
```
CREATE TABLE LAST_TRADE (
 LT_S_SYMB VARCHAR(15) NOT NULL,
 LT_DTS TIMESTAMP(6) DEFAULT current_timestamp,
 LT_PRICE NUMBER(8,2) NOT NULL,
 LT_OPEN_PRICE NUMBER(8,2) NOT NULL,
 LT_VOL NUMBER(12) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

20. NEWS_ITEM
```
CREATE TABLE NEWS_ITEM (
 NI_ID NUMBER(19) NOT NULL,
 NI_HEADLINE CHAR(80) NOT NULL,
 NI_SUMMARY CHAR(255) NOT NULL,
 NI_ITEM BLOB,
 NI_DTS TIMESTAMP(6) DEFAULT current_timestamp,
 NI_SOURCE VARCHAR(30) NOT NULL,
 NI_AUTHOR VARCHAR(30)
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

21. NEWS_XREF
```
CREATE TABLE NEWS_XREF (
 NX_NI_ID NUMBER(19) NOT NULL,
 NX_CO_ID NUMBER(19) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

22. SECTOR
```
CREATE TABLE SECTOR (
 SC_ID CHAR(2) NOT NULL,
 SC_NAME CHAR(30) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

23. SECURITY
```
CREATE TABLE SECURITY (
 S_SYMB VARCHAR(15) NOT NULL,
 S_ISSUE CHAR(6) NOT NULL,
 S_ST_ID CHAR(4) NOT NULL,
 S_NAME VARCHAR(70) NOT NULL,
 S_EX_ID CHAR(6) NOT NULL,
 S_CO_ID NUMBER(19) NOT NULL,
 S_NUM_OUT NUMBER(12) NOT NULL,
 S_START_DATE DATE NOT NULL,
 S_EXCH_DATE DATE NOT NULL,
 S_PE NUMBER(10,2) NOT NULL,
 S_52WK_HIGH NUMBER(8,2) NOT NULL,
 S_52WK_HIGH_DATE DATE NOT NULL,
 S_52WK_LOW NUMBER(8,2) NOT NULL,
 S_52WK_LOW_DATE DATE NOT NULL,
 S_DIVIDEND NUMBER(10,2) NOT NULL,
 S_YIELD NUMBER(5,2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

24. SETTLEMENT
```
CREATE TABLE SETTLEMENT (
 SE_T_ID NUMBER(19) NOT NULL,
 SE_CASH_TYPE VARCHAR(40) NOT NULL,
 SE_CASH_DUE_DATE DATE NOT NULL,
 SE_AMT NUMBER(10,2) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

25. STATUS_TYPE
```
CREATE TABLE STATUS_TYPE (
 ST_ID CHAR(4) NOT NULL,
 ST_NAME CHAR(10) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

26. TAXRATE
```
CREATE TABLE TAXRATE (
 TX_ID CHAR(4) NOT NULL,
 TX_NAME CHAR(50) NOT NULL,
 TX_RATE NUMBER(6,5) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

27. TRADE
```
CREATE TABLE TRADE (
 T_ID NUMBER(19) NOT NULL,
 T_DTS TIMESTAMP(6) DEFAULT current_timestamp,
 T_ST_ID CHAR(4) NOT NULL,
 T_TT_ID CHAR(3) NOT NULL,
 T_IS_CASH NUMBER(3) NOT NULL,
 T_S_SYMB VARCHAR(15) NOT NULL,
 T_QTY NUMBER(6) NOT NULL,
 T_BID_PRICE NUMBER(8,2) NOT NULL,
 T_CA_ID NUMBER(19) NOT NULL,
 T_EXEC_NAME VARCHAR(49) NOT NULL,
 T_TRADE_PRICE NUMBER(8,2),
 T_CHRG NUMBER(10,2) NOT NULL,
 T_COMM NUMBER(10,2) NOT NULL,
 T_TAX NUMBER(10,2) NOT NULL,
 T_LIFO NUMBER(3) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

28. TRADE_HISTORY
```
CREATE TABLE TRADE_HISTORY (
 TH_T_ID NUMBER(19) NOT NULL,
 TH_DTS TIMESTAMP(6) DEFAULT current_timestamp,
 TH_ST_ID CHAR(4) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

29. TRADE_TYPE
```
CREATE TABLE TRADE_TYPE (
 TT_ID CHAR(3) NOT NULL,
 TT_NAME CHAR(12) NOT NULL,
 TT_IS_SELL NUMBER(3) NOT NULL,
 TT_IS_MRKT NUMBER(3) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

30. WATCH_ITEM
```
CREATE TABLE WATCH_ITEM (
 WI_WL_ID NUMBER(19) NOT NULL,
 WI_S_SYMB VARCHAR(15) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

31. WATCH_LIST
```
CREATE TABLE WATCH_LIST (
 WL_ID NUMBER(19) NOT NULL,
 WL_C_ID NUMBER(19) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

32. ZIP_CODE
```
CREATE TABLE ZIP_CODE (
 ZC_CODE VARCHAR(12) NOT NULL,
 ZC_TOWN VARCHAR(80) NOT NULL,
 ZC_DIV VARCHAR(80) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

33. TRADE_REQUEST
```
CREATE TABLE TRADE_REQUEST (
 TR_T_ID NUMBER(19) NOT NULL,
 TR_TT_ID CHAR(3) NOT NULL,
 TR_S_SYMB VARCHAR(15),
 TR_QTY NUMBER(6) NOT NULL,
 TR_BID_PRICE NUMBER(8,2) NOT NULL,
 TR_B_ID NUMBER(19) NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

34. NEXT_ID
```
CREATE TABLE NEXT_ID (
 KEY1 CHAR(8) NOT NULL,
 KEY2 CHAR(12) NOT NULL,
 ID_VALUE NUMBER(19) NOT NULL,
 ORDINAL NUMBER NOT NULL
)
TABLESPACE ZREF_DATA
PCTFREE 10
INITRANS 2
STORAGE (
 MAXEXTENTS UNLIMITED
)
LOGGING
NOPARALLEL;
```

__B.__ Create indexes for each table.

- Total 56.

```
CREATE UNIQUE INDEX PK_AP ON ACCOUNT_PERMISSION ( AP_CA_ID ASC, AP_TAX_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_AD ON ADDRESS01 ( AD_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_B_NAME_ID_UNIQ ON BROKER ( B_NAME ASC, B_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_B_ID_NAME_UNIQ ON BROKER ( B_ID ASC, B_NAME ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_B ON BROKER ( B_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_CT ON CASH_TRANSACTION ( CT_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_CH ON CHARGE ( CH_TT_ID ASC, CH_C_TIER ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_CR ON COMMISSION_RATE ( CR_C_TIER ASC, CR_TT_ID ASC, CR_EX_ID ASC, CR_FROM_QTY ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_CO_NAME_ID_UNIQ ON COMPANY ( CO_NAME ASC, CO_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_CO_ID_UNIQ ON COMPANY ( CO_IN_ID ASC, CO_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_CO ON COMPANY ( CO_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_CP_ID ON COMPANY_COMPETITOR ( CP_CO_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_CP ON COMPANY_COMPETITOR ( CP_CO_ID ASC, CP_COMP_CO_ID ASC, CP_IN_ID ASC) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_C_TAX_ID_UNIQ ON CUSTOMER ( C_TAX_ID ASC, C_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_C_ID_TIER_UNIQ ON CUSTOMER ( C_ID ASC, C_TIER ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_C ON CUSTOMER ( C_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_CA_ID_UNIQ ON CUSTOMER_ACCOUNT ( CA_C_ID ASC, CA_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_CA ON CUSTOMER_ACCOUNT ( CA_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_CX_ID_UNIQ ON CUSTOMER_TAXRATE ( CX_C_ID ASC, CX_TX_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_CX ON CUSTOMER_TAXRATE ( CX_TX_ID ASC, CX_C_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_DM ON DAILY_MARKET ( DM_DATE ASC, DM_S_SYMB ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_DM_SYMB_DATE_UNIQ ON DAILY_MARKET ( DM_S_SYMB ASC, DM_DATE ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_EX ON EXCHANGE ( EX_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_FI ON FINANCIAL ( FI_CO_ID ASC, FI_YEAR ASC, FI_QTR ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_ID_SYMB_DTS_UNIQ ON HOLDING ( H_CA_ID ASC, H_S_SYMB ASC, H_DTS ASC, H_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_H ON HOLDING ( H_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_HH_ID_UNIQ ON HOLDING_HISTORY ( HH_T_ID ASC, HH_H_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_HH ON HOLDING_HISTORY ( HH_H_T_ID ASC, HH_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_HS ON HOLDING_SUMMARY ( HS_CA_ID ASC, HS_S_SYMB ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_IN_NAME_ID_UNIQ ON INDUSTRY ( IN_NAME ASC, IN_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_IN ON INDUSTRY ( IN_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_LT ON LAST_TRADE ( LT_S_SYMB ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_NI ON NEWS_ITEM ( NI_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_NX_ID_UNIQ ON NEWS_XREF ( NX_CO_ID ASC, NX_NI_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_NX ON NEWS_XREF ( NX_NI_ID ASC, NX_CO_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_SC_NAME_ID_UNIQ ON SECTOR ( SC_NAME ASC, SC_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_SC ON SECTOR ( SC_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_S_ID_ISS_EX_SYMB_UNIQ ON SECURITY ( S_CO_ID ASC, S_ISSUE ASC, S_EX_ID ASC, S_SYMB ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_S ON SECURITY ( S_SYMB ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_SE ON SETTLEMENT ( SE_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_ST ON STATUS_TYPE ( ST_ID ASC ) LOGGING  TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_TX ON TAXRATE ( TX_ID ASC ) LOGGING  TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_T_ID_DTS_UNIQ ON TRADE ( T_CA_ID ASC, T_DTS ASC, T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_T_SYMB_DTS_ID_UNIQ ON TRADE ( T_S_SYMB ASC, T_DTS ASC, T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_T ON TRADE ( T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_TH ON TRADE_HISTORY ( TH_T_ID ASC, TH_ST_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_TT_ID_MKT_SELL_NM_UNIQ ON TRADE_TYPE ( TT_ID ASC, TT_IS_MRKT ASC, TT_IS_SELL ASC, TT_NAME ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_TT ON TRADE_TYPE ( TT_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_WI ON WATCH_ITEM ( WI_WL_ID ASC, WI_S_SYMB ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_WL_ID_UNIQ ON WATCH_LIST ( WL_C_ID ASC, WL_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_WL ON WATCH_LIST ( WL_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_ZC ON ZIP_CODE ( ZC_CODE ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_TR_ID_SYMB_UNIQ ON TRADE_REQUEST ( TR_B_ID ASC, TR_S_SYMB ASC, TR_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_TR_SYMB_ID_BID_QTY_UNIQ ON TRADE_REQUEST ( TR_S_SYMB ASC, TR_T_ID ASC, TR_TT_ID ASC, TR_BID_PRICE ASC, TR_QTY ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX PK_TR ON TRADE_REQUEST ( TR_T_ID ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
CREATE UNIQUE INDEX IDX_NE_KEY_UNIQ ON NEXT_ID ( KEY1 ASC, KEY2 ASC ) LOGGING TABLESPACE ZREF_INDEX_TS PCTFREE 10 INITRANS 2;
```

__C.__ Add constraints.

- Total 56.

```
ALTER TABLE ACCOUNT_PERMISSION ADD CONSTRAINT PK_AP PRIMARY KEY ( AP_CA_ID, AP_TAX_ID );
ALTER TABLE ADDRESS01 ADD CONSTRAINT PK_AD PRIMARY KEY ( AD_ID );
ALTER TABLE BROKER ADD CONSTRAINT PK_B PRIMARY KEY ( B_ID );
ALTER TABLE CASH_TRANSACTION ADD CONSTRAINT PK_CT PRIMARY KEY ( CT_T_ID );
ALTER TABLE CHARGE ADD CONSTRAINT PK_CH PRIMARY KEY ( CH_TT_ID, CH_C_TIER );
ALTER TABLE CHARGE ADD CONSTRAINT CK_CH_CHRG CHECK (CH_CHRG >= 0);
ALTER TABLE CHARGE ADD CONSTRAINT CK_CH_C_TIER CHECK (CH_C_TIER IN (1,2,3));
ALTER TABLE COMMISSION_RATE ADD CONSTRAINT PK_CR PRIMARY KEY ( CR_C_TIER, CR_TT_ID, CR_EX_ID, CR_FROM_QTY);
ALTER TABLE COMMISSION_RATE ADD CONSTRAINT CK_CR_C_TIER CHECK (CR_C_TIER in (1,2,3));
ALTER TABLE COMMISSION_RATE ADD CONSTRAINT CK_CR_FROM_QTY CHECK (CR_FROM_QTY >= 0);
ALTER TABLE COMMISSION_RATE ADD CONSTRAINT CK_CR_TO_QTY CHECK (CR_TO_QTY > CR_FROM_QTY);
ALTER TABLE COMMISSION_RATE ADD CONSTRAINT CK_CR_RATE CHECK (CR_RATE >= 0);
ALTER TABLE COMPANY ADD CONSTRAINT PK_CO PRIMARY KEY ( CO_ID );
ALTER TABLE COMPANY_COMPETITOR ADD CONSTRAINT PK_CP PRIMARY KEY ( CP_CO_ID, CP_COMP_CO_ID, CP_IN_ID);
ALTER TABLE CUSTOMER ADD CONSTRAINT PK_C PRIMARY KEY ( C_ID );
ALTER TABLE CUSTOMER ADD CONSTRAINT CK_C_TIER CHECK (C_TIER IN (1,2,3));
ALTER TABLE CUSTOMER ADD CONSTRAINT CK_C_GNDR CHECK (C_GNDR IN ('M', 'F'));
ALTER TABLE CUSTOMER_ACCOUNT ADD CONSTRAINT PK_CA PRIMARY KEY ( CA_ID);
ALTER TABLE CUSTOMER_ACCOUNT ADD CONSTRAINT CK_CA_TAX_ST CHECK (CA_TAX_ST IN (0,1,2));
ALTER TABLE CUSTOMER_TAXRATE ADD CONSTRAINT PK_CX PRIMARY KEY ( CX_TX_ID, CX_C_ID );
ALTER TABLE DAILY_MARKET ADD CONSTRAINT PK_DM PRIMARY KEY ( DM_DATE, DM_S_SYMB );
ALTER TABLE EXCHANGE ADD CONSTRAINT PK_EX PRIMARY KEY ( EX_ID );
ALTER TABLE FINANCIAL ADD CONSTRAINT PK_FI PRIMARY KEY ( FI_CO_ID, FI_YEAR, FI_QTR);
ALTER TABLE FINANCIAL ADD CONSTRAINT CK_FINANCIAL_QTR CHECK (FI_QTR IN (1,2,3,4));
ALTER TABLE HOLDING ADD CONSTRAINT PK_H PRIMARY KEY ( H_T_ID );
ALTER TABLE HOLDING ADD CONSTRAINT CK_H_PRICE CHECK (H_PRICE > 0);
ALTER TABLE HOLDING_HISTORY ADD CONSTRAINT PK_HH PRIMARY KEY ( HH_H_T_ID, HH_T_ID );
ALTER TABLE HOLDING_SUMMARY ADD CONSTRAINT PK_HS PRIMARY KEY ( HS_CA_ID, HS_S_SYMB );
ALTER TABLE INDUSTRY ADD CONSTRAINT PK_IN PRIMARY KEY ( IN_ID );
ALTER TABLE LAST_TRADE ADD CONSTRAINT PK_LT PRIMARY KEY ( LT_S_SYMB );
ALTER TABLE NEWS_ITEM ADD CONSTRAINT PK_NI PRIMARY KEY ( NI_ID );
ALTER TABLE NEWS_XREF ADD CONSTRAINT PK_NX PRIMARY KEY ( NX_NI_ID, NX_CO_ID );
ALTER TABLE SECTOR ADD CONSTRAINT PK_SC PRIMARY KEY ( SC_ID );
ALTER TABLE SECURITY ADD CONSTRAINT PK_S PRIMARY KEY ( S_SYMB );
ALTER TABLE SETTLEMENT ADD CONSTRAINT PK_SE PRIMARY KEY ( SE_T_ID );
ALTER TABLE STATUS_TYPE ADD CONSTRAINT PK_ST PRIMARY KEY ( ST_ID );
ALTER TABLE TAXRATE ADD CONSTRAINT PK_TX PRIMARY KEY ( TX_ID );
ALTER TABLE TAXRATE ADD CONSTRAINT CK_TX_RATE CHECK (TX_RATE >= 0);
ALTER TABLE TRADE ADD CONSTRAINT PK_T PRIMARY KEY ( T_ID );
ALTER TABLE TRADE ADD CONSTRAINT CK_T_IS_CASH CHECK (T_IS_CASH in (0, 1));
ALTER TABLE TRADE ADD CONSTRAINT CK_T_LIFO CHECK (T_LIFO in (0, 1));
ALTER TABLE TRADE ADD CONSTRAINT CK_T_BID_PRICE CHECK (T_BID_PRICE > 0);
ALTER TABLE TRADE ADD CONSTRAINT CK_T_TAX CHECK (T_TAX >= 0);
ALTER TABLE TRADE ADD CONSTRAINT CK_T_COMM CHECK (T_COMM >= 0);
ALTER TABLE TRADE ADD CONSTRAINT CK_T_CHRG CHECK (T_CHRG >= 0);
ALTER TABLE TRADE ADD CONSTRAINT CK_T_QTY CHECK (T_QTY > 0);
ALTER TABLE TRADE_HISTORY ADD CONSTRAINT PK_TH PRIMARY KEY ( TH_T_ID, TH_ST_ID );
ALTER TABLE TRADE_TYPE ADD CONSTRAINT PK_TT PRIMARY KEY ( TT_ID );
ALTER TABLE TRADE_TYPE ADD CONSTRAINT CK_TRADE_TYPE_IS_SELL CHECK (TT_IS_SELL in (0, 1));
ALTER TABLE TRADE_TYPE ADD CONSTRAINT CK_TRADE_TYPE_IS_MRKT CHECK (TT_IS_MRKT IN (0, 1));
ALTER TABLE WATCH_ITEM ADD CONSTRAINT PK_WI PRIMARY KEY ( WI_WL_ID, WI_S_SYMB );
ALTER TABLE WATCH_LIST ADD CONSTRAINT PK_WL PRIMARY KEY ( WL_ID );
ALTER TABLE ZIP_CODE ADD CONSTRAINT PK_ZC PRIMARY KEY ( ZC_CODE );
ALTER TABLE TRADE_REQUEST ADD CONSTRAINT PK_TR PRIMARY KEY ( TR_T_ID );
ALTER TABLE TRADE_REQUEST ADD CONSTRAINT CK_TR_BID_PRICE CHECK (TR_BID_PRICE > 0);
ALTER TABLE TRADE_REQUEST ADD CONSTRAINT CK_TR_QTY CHECK (TR_QTY > 0);
```

__D.__ Alter tables.

- Total 49.
```
ALTER TABLE ACCOUNT_PERMISSION ADD CONSTRAINT FK_CA_AP_CC FOREIGN KEY ( AP_CA_ID ) REFERENCES CUSTOMER_ACCOUNT ( CA_ID ) ON DELETE CASCADE;
ALTER TABLE ADDRESS01 ADD CONSTRAINT FK_ZC_AD_CC FOREIGN KEY ( AD_ZC_CODE ) REFERENCES ZIP_CODE ( ZC_CODE ) ON DELETE CASCADE;
ALTER TABLE BROKER ADD CONSTRAINT FK_ST_B_CC FOREIGN KEY ( B_ST_ID ) REFERENCES STATUS_TYPE ( ST_ID ) ON DELETE CASCADE;
ALTER TABLE CASH_TRANSACTION ADD CONSTRAINT FK_T_CT_CC FOREIGN KEY ( CT_T_ID ) REFERENCES TRADE ( T_ID ) ON DELETE CASCADE;
ALTER TABLE CHARGE ADD CONSTRAINT FK_TT_CH_CC FOREIGN KEY ( CH_TT_ID ) REFERENCES TRADE_TYPE ( TT_ID ) ON DELETE CASCADE;
ALTER TABLE COMMISSION_RATE ADD CONSTRAINT FK_EX_CR_CC FOREIGN KEY ( CR_EX_ID ) REFERENCES EXCHANGE ( EX_ID ) ON DELETE CASCADE;
ALTER TABLE COMMISSION_RATE ADD CONSTRAINT FK_TT_CR_CC FOREIGN KEY ( CR_TT_ID ) REFERENCES TRADE_TYPE ( TT_ID ) ON DELETE CASCADE;
ALTER TABLE COMPANY ADD CONSTRAINT FK_AD_CO_CC FOREIGN KEY ( CO_AD_ID ) REFERENCES ADDRESS01 ( AD_ID ) ON DELETE CASCADE;
ALTER TABLE COMPANY ADD CONSTRAINT FK_IN_CO_CC FOREIGN KEY ( CO_IN_ID ) REFERENCES INDUSTRY ( IN_ID ) ON DELETE CASCADE;
ALTER TABLE COMPANY ADD CONSTRAINT FK_ST_CO_CC FOREIGN KEY ( CO_ST_ID ) REFERENCES STATUS_TYPE ( ST_ID ) ON DELETE CASCADE;
ALTER TABLE COMPANY_COMPETITOR ADD CONSTRAINT FK_CO_CP_CC FOREIGN KEY ( CP_CO_ID ) REFERENCES COMPANY ( CO_ID ) ON DELETE CASCADE;
ALTER TABLE COMPANY_COMPETITOR ADD CONSTRAINT FK_CO_CP_CC2 FOREIGN KEY ( CP_COMP_CO_ID ) REFERENCES COMPANY ( CO_ID ) ON DELETE CASCADE;
ALTER TABLE COMPANY_COMPETITOR ADD CONSTRAINT FK_IN_CP_CC FOREIGN KEY ( CP_IN_ID ) REFERENCES INDUSTRY ( IN_ID ) ON DELETE CASCADE;
ALTER TABLE CUSTOMER ADD CONSTRAINT FK_AD_C_CC FOREIGN KEY ( C_AD_ID ) REFERENCES ADDRESS01 ( AD_ID ) ON DELETE CASCADE;
ALTER TABLE CUSTOMER ADD CONSTRAINT FK_ST_C_CC FOREIGN KEY ( C_ST_ID ) REFERENCES STATUS_TYPE ( ST_ID ) ON DELETE CASCADE;
ALTER TABLE CUSTOMER_ACCOUNT ADD CONSTRAINT FK_B_CA_CC FOREIGN KEY ( CA_B_ID ) REFERENCES BROKER ( B_ID ) ON DELETE CASCADE;
ALTER TABLE CUSTOMER_ACCOUNT ADD CONSTRAINT FK_C_CA_CC FOREIGN KEY ( CA_C_ID ) REFERENCES CUSTOMER ( C_ID ) ON DELETE CASCADE;
ALTER TABLE CUSTOMER_TAXRATE ADD CONSTRAINT FK_C_CX_CC FOREIGN KEY ( CX_C_ID ) REFERENCES CUSTOMER ( C_ID ) ON DELETE CASCADE;
ALTER TABLE CUSTOMER_TAXRATE ADD CONSTRAINT FK_TX_CX_CC FOREIGN KEY ( CX_TX_ID ) REFERENCES TAXRATE ( TX_ID ) ON DELETE CASCADE;
ALTER TABLE DAILY_MARKET ADD CONSTRAINT FK_S_DM_CC FOREIGN KEY ( DM_S_SYMB ) REFERENCES SECURITY ( S_SYMB ) ON DELETE CASCADE;
ALTER TABLE EXCHANGE ADD CONSTRAINT FK_AD_EX_CC FOREIGN KEY ( EX_AD_ID ) REFERENCES ADDRESS01 ( AD_ID ) ON DELETE CASCADE;
ALTER TABLE FINANCIAL ADD CONSTRAINT FK_CO_FI_CC FOREIGN KEY ( FI_CO_ID ) REFERENCES COMPANY ( CO_ID ) ON DELETE CASCADE;
ALTER TABLE HOLDING ADD CONSTRAINT FK_HS_H_CC FOREIGN KEY ( H_CA_ID, H_S_SYMB ) REFERENCES HOLDING_SUMMARY ( HS_CA_ID, HS_S_SYMB ) ON DELETE CASCADE;
ALTER TABLE HOLDING ADD CONSTRAINT FK_T_H_CC FOREIGN KEY ( H_T_ID ) REFERENCES TRADE ( T_ID ) ON DELETE CASCADE;
ALTER TABLE HOLDING_HISTORY ADD CONSTRAINT FK_T_HH_CC FOREIGN KEY ( HH_H_T_ID ) REFERENCES TRADE ( T_ID ) ON DELETE CASCADE;
ALTER TABLE HOLDING_HISTORY ADD CONSTRAINT FK_T_HH_CC2 FOREIGN KEY ( HH_T_ID ) REFERENCES TRADE ( T_ID ) ON DELETE CASCADE;
ALTER TABLE HOLDING_SUMMARY ADD CONSTRAINT FK_CA_HS_CC FOREIGN KEY ( HS_CA_ID ) REFERENCES CUSTOMER_ACCOUNT ( CA_ID ) ON DELETE CASCADE;
ALTER TABLE HOLDING_SUMMARY ADD CONSTRAINT FK_S_HS_CC FOREIGN KEY ( HS_S_SYMB ) REFERENCES SECURITY ( S_SYMB ) ON DELETE CASCADE;
ALTER TABLE INDUSTRY ADD CONSTRAINT FK_SC_IN_CC FOREIGN KEY ( IN_SC_ID ) REFERENCES SECTOR ( SC_ID ) ON DELETE CASCADE;
ALTER TABLE LAST_TRADE ADD CONSTRAINT FK_S_LT_CC FOREIGN KEY ( LT_S_SYMB ) REFERENCES SECURITY ( S_SYMB ) ON DELETE CASCADE;
ALTER TABLE NEWS_XREF ADD CONSTRAINT FK_CO_NX_CC FOREIGN KEY ( NX_CO_ID ) REFERENCES COMPANY ( CO_ID ) ON DELETE CASCADE;
ALTER TABLE NEWS_XREF ADD CONSTRAINT FK_NI_NX_CC FOREIGN KEY ( NX_NI_ID ) REFERENCES NEWS_ITEM ( NI_ID ) ON DELETE CASCADE;
ALTER TABLE SECURITY ADD CONSTRAINT FK_CO_S_CC FOREIGN KEY ( S_CO_ID ) REFERENCES COMPANY ( CO_ID ) ON DELETE CASCADE;
ALTER TABLE SECURITY ADD CONSTRAINT FK_EX_S_CC FOREIGN KEY ( S_EX_ID ) REFERENCES EXCHANGE ( EX_ID ) ON DELETE CASCADE;
ALTER TABLE SECURITY ADD CONSTRAINT FK_ST_S_CC FOREIGN KEY ( S_ST_ID ) REFERENCES STATUS_TYPE ( ST_ID ) ON DELETE CASCADE;
ALTER TABLE SETTLEMENT ADD CONSTRAINT FK_T_SE_CC FOREIGN KEY ( SE_T_ID ) REFERENCES TRADE ( T_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE ADD CONSTRAINT FK_CA_T_CC FOREIGN KEY ( T_CA_ID ) REFERENCES CUSTOMER_ACCOUNT ( CA_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE ADD CONSTRAINT FK_S_T_CC FOREIGN KEY ( T_S_SYMB ) REFERENCES SECURITY ( S_SYMB ) ON DELETE CASCADE;
ALTER TABLE TRADE ADD CONSTRAINT FK_ST_T_CC FOREIGN KEY ( T_ST_ID ) REFERENCES STATUS_TYPE ( ST_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE ADD CONSTRAINT FK_TT_T_CC FOREIGN KEY ( T_TT_ID ) REFERENCES TRADE_TYPE ( TT_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE_HISTORY ADD CONSTRAINT FK_ST_TH_CC FOREIGN KEY ( TH_ST_ID ) REFERENCES STATUS_TYPE ( ST_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE_HISTORY ADD CONSTRAINT FK_T_TH_CC FOREIGN KEY ( TH_T_ID ) REFERENCES TRADE ( T_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE_REQUEST ADD CONSTRAINT FK_B_TR_CC FOREIGN KEY ( TR_B_ID ) REFERENCES BROKER ( B_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE_REQUEST ADD CONSTRAINT FK_S_TR_CC FOREIGN KEY ( TR_S_SYMB ) REFERENCES SECURITY ( S_SYMB ) ON DELETE CASCADE;
ALTER TABLE TRADE_REQUEST ADD CONSTRAINT FK_T_TR_CC FOREIGN KEY ( TR_T_ID ) REFERENCES TRADE ( T_ID ) ON DELETE CASCADE;
ALTER TABLE TRADE_REQUEST ADD CONSTRAINT FK_TT_TR_CC FOREIGN KEY ( TR_TT_ID ) REFERENCES TRADE_TYPE ( TT_ID ) ON DELETE CASCADE;
ALTER TABLE WATCH_ITEM ADD CONSTRAINT FK_S_WI_CC FOREIGN KEY ( WI_S_SYMB ) REFERENCES SECURITY ( S_SYMB ) ON DELETE CASCADE;
ALTER TABLE WATCH_ITEM ADD CONSTRAINT FK_WL_WI_CC FOREIGN KEY ( WI_WL_ID ) REFERENCES WATCH_LIST ( WL_ID ) ON DELETE CASCADE;
ALTER TABLE WATCH_LIST ADD CONSTRAINT FK_C_WL_CC FOREIGN KEY ( WL_C_ID ) REFERENCES CUSTOMER ( C_ID ) ON DELETE CASCADE;
```

__E.__ Disable the Foreign keys.

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
ALTER TABLE COMPANY_COMPETITOR DISABLE CONSTRAINT FK_CO_CP_CC2;
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
ALTER TABLE HOLDING_HISTORY DISABLE CONSTRAINT FK_T_HH_CC2;
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

- Check if the constraints are disabled.

```
select constraint_name, table_name, status, r_owner from dba_constraints
where constraint_name like 'FK_%' order by status;
```

- Results.

```
CONSTRAINT_NAME TABLE_NAME STATUS R_OWNER
FK_AD_C_CC CUSTOMER DISABLED TIBERO
FK_AD_CO_CC COMPANY DISABLED TIBERO
FK_AD_EX_CC EXCHANGE DISABLED TIBERO
FK_B_CA_CC CUSTOMER_ACCOUNT DISABLED TIBERO
FK_B_TR_CC TRADE_REQUEST DISABLED TIBERO
FK_C_CA_CC CUSTOMER_ACCOUNT DISABLED TIBERO
FK_C_CX_CC CUSTOMER_TAXRATE DISABLED TIBERO
FK_C_WL_CC WATCH_LIST DISABLED TIBERO
FK_CA_AP_CC ACCOUNT_PERMISSION DISABLED TIBERO
FK_CA_HS_CC HOLDING_SUMMARY DISABLED TIBERO
FK_CA_T_CC TRADE DISABLED TIBERO
FK_CO_CP_CC COMPANY_COMPETITOR DISABLED TIBERO
FK_CO_CP_CC2 COMPANY_COMPETITOR ENABLED TIBERO -> cannot be disabled.
FK_CO_FI_CC FINANCIAL DISABLED TIBERO
FK_CO_NX_CC NEWS_XREF DISABLED TIBERO
FK_CO_S_CC SECURITY DISABLED TIBERO
FK_EX_CR_CC COMMISSION_RATE DISABLED TIBERO
FK_EX_S_CC SECURITY DISABLED TIBERO
FK_IN_CO_CC COMPANY DISABLED TIBERO
FK_IN_CP_CC COMPANY_COMPETITOR DISABLED TIBERO
FK_NI_NX_CC NEWS_XREF DISABLED TIBERO
FK_S_DM_CC DAILY_MARKET DISABLED TIBERO
FK_S_HS_CC HOLDING_SUMMARY DISABLED TIBERO
FK_S_LT_CC LAST_TRADE DISABLED TIBERO
FK_S_T_CC TRADE DISABLED TIBERO
FK_S_TR_CC TRADE_REQUEST DISABLED TIBERO
FK_S_WI_CC WATCH_ITEM DISABLED TIBERO
FK_SC_IN_CC INDUSTRY DISABLED TIBERO
FK_ST_B_CC BROKER DISABLED TIBERO
FK_ST_C_CC CUSTOMER DISABLED TIBERO
FK_ST_CO_CC COMPANY DISABLED TIBERO
FK_ST_S_CC SECURITY DISABLED TIBERO
FK_ST_T_CC TRADE DISABLED TIBERO
FK_ST_TH_CC TRADE_HISTORY DISABLED TIBERO
FK_T_CT_CC CASH_TRANSACTION DISABLED TIBERO
FK_T_H_CC HOLDING DISABLED TIBERO
FK_T_HH_CC HOLDING_HISTORY DISABLED TIBERO
FK_T_HH_CC2 HOLDING_HISTORY ENABLED TIBERO -> cannot be disabled.
FK_T_SE_CC SETTLEMENT DISABLED TIBERO
FK_T_TH_CC TRADE_HISTORY DISABLED TIBERO
FK_T_TR_CC TRADE_REQUEST DISABLED TIBERO
FK_TT_CH_CC CHARGE DISABLED TIBERO
FK_TT_CR_CC COMMISSION_RATE DISABLED TIBERO
FK_TT_T_CC TRADE DISABLED TIBERO
FK_TT_TR_CC TRADE_REQUEST DISABLED TIBERO
FK_TX_CX_CC CUSTOMER_TAXRATE DISABLED TIBERO
FK_WL_WI_CC WATCH_ITEM DISABLED TIBERO
FK_ZC_AD_CC ADDRESS01 DISABLED TIBERO
```

__F.__ Load the data.

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

**How to use tbloader.sh**

1) HOLDING_SUMMARY is the table name.

```
oftibr@OFDB1:/opt2/tmaxdb/zrefdata/azure_load_resutlts /> ll *HOLDING_SUMMARY*
-rwxrwxr-x. 1 oftibr dba        0 Aug 11 23:03 HOLDING_SUMMARY.bad
-rwxrwxr-x. 1 oftibr dba      736 Aug 11 23:03 HOLDING_SUMMARY.log
-rw-r--r--. 1 oftibr dba 60207234 Jun 18 16:07 HOLDING_SUMMARY.txt
-rwxrwxr-x. 1 oftibr dba      417 Aug 11 23:03 loader_HOLDING_SUMMARY.ctl
```

2) HOLDING_SUMMARY.txt is the data to be loaded. (generated by EGenLoader)

- Change table name to .txt file.

```
mv AccountPermission.txt ACCOUNT_PERMISSION.txt
mv Address.txt ADDRESS01.txt
mv Broker.txt BROKER.txt
mv Charge.txt CHARGE.txt
mv CommissionRate.txt COMMISSION_RATE.txt
mv Company.txt COMPANY.txt
mv CompanyCompetitor.txt COMPANY_COMPETITOR.txt
mv Customer.txt CUSTOMER.txt
mv CustomerAccount.txt CUSTOMER_ACCOUNT.txt
mv CustomerTaxrate.txt CUSTOMER_TAXRATE.txt
mv DailyMarket.txt DAILY_MARKET.txt
mv Exchange.txt EXCHANGE.txt
mv Financial.txt FINANCIAL.txt
mv Industry.txt INDUSTRY.txt
mv LastTrade.txt LAST_TRADE.txt
mv NewsItem.txt NEWS_ITEM.txt
mv NewsXRef.txt NEWS_XREF.txt
mv Sector.txt SECTOR.txt
mv Security.txt SECURITY.txt
mv StatusType.txt STATUS_TYPE.txt
mv TaxRate.txt TAXRATE.txt
mv TradeType.txt TRADE_TYPE.txt
mv WatchItem.txt WATCH_ITEM.txt
mv WatchList.txt WATCH_LIST.txt
mv ZipCode.txt ZIP_CODE.txt
mv CashTransaction.txt CASH_TRANSACTION.txt
mv Holding.txt HOLDING.txt
mv HoldingHistory.txt HOLDING_HISTORY.txt
mv HoldingSummary.txt HOLDING_SUMMARY.txt
mv Settlement.txt SETTLEMENT.txt
mv Trade.txt TRADE.txt
mv TradeHistory.txt TRADE_HISTORY.txt
```

3) HOLDING_SUMMARY.ctl is the commands/parms.

```
oftibr@OFDB1:/opt2/tmaxdb/zrefdata/azure_load_resutlts /> cat loader_HOLDING_SUMMARY.ctl

LOAD DATA
--INFILE  '/home/oframe7/Dave/DRAWING.txt'
INFILE 'HOLDING_SUMMARY.txt'
LOGFILE 'HOLDING_SUMMARY.log'
BADFILE 'HOLDING_SUMMARY.bad'
TRUNCATE
--REPLACE
--APPEND
INTO TABLE ZREF.HOLDING_SUMMARY
--INTO TABLE TIBERO.HOLDING_SUMMARY
--FIELDS TERMINATED BY ','
FIELDS TERMINATED BY '|'
           OPTIONALLY ENCLOSED BY '"'
--         ESCAPED BY '\\'
--LINES TERMINATED BY '\n'
--IGNORE 1 LINES
--DIRECT=Y
TRAILING NULLCOLS
```

4) HOLDING_SUMMARY.log is the result.

```
oftibr@OFDB1:/opt2/tmaxdb/zrefdata/azure_load_resutlts /> cat HOLDING_SUMMARY.log

tbLoader 6

TmaxData Corporation Copyright (c) 2008-. All rights reserved.

Data File    : HOLDING_SUMMARY.txt
Bad File     : HOLDING_SUMMARY.bad
Discard File : HOLDING_SUMMARY.dsc

Table ZREF.HOLDING_SUMMARY, was loaded from the data file.

COLUMN_NAME                      POSITION DATATYPE
------------------------------ ---------- ----------------
HS_CA_ID                                1 CHARACTER
HS_S_SYMB                               2 CHARACTER
HS_QTY                                  3 CHARACTER

Table ZREF.HOLDING_SUMMARY :
        2470468 Rows were requested to load.
        2470468 Rows were loaded successfully.
        0 Rows were failed to load because of some errors

Total Elapsed Time was    : 00:00:02.756465
```

5) HOLDING_SUMMARY.bad are rejected rows. These are generated by tbloader script (except data to load HOLDING_SUMMARY.txt).


### time ./tbloader.sh

__G.__ Rebuild the indexes. 

- Total 56.

### set timing on

```
ALTER INDEX IDX_B_ID_NAME_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_B_NAME_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_C_ID_TIER_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_C_TAX_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_CA_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_CO_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_CO_NAME_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_CP_ID REBUILD PARALLEL 4;
ALTER INDEX IDX_CX_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_DM_SYMB_DATE_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_HH_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_ID_SYMB_DTS_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_IN_NAME_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_NE_KEY_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_NX_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_S_ID_ISS_EX_SYMB_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_SC_NAME_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_T_ID_DTS_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_T_SYMB_DTS_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_TR_ID_SYMB_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_TR_SYMB_ID_BID_QTY_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_TT_ID_MKT_SELL_NM_UNIQ REBUILD PARALLEL 4;
ALTER INDEX IDX_WL_ID_UNIQ REBUILD PARALLEL 4;
ALTER INDEX PK_AD REBUILD PARALLEL 4;
ALTER INDEX PK_AP REBUILD PARALLEL 4;
ALTER INDEX PK_B REBUILD PARALLEL 4;
ALTER INDEX PK_C REBUILD PARALLEL 4;
ALTER INDEX PK_CA REBUILD PARALLEL 4;
ALTER INDEX PK_CH REBUILD PARALLEL 4;
ALTER INDEX PK_CO REBUILD PARALLEL 4;
ALTER INDEX PK_CP REBUILD PARALLEL 4;
ALTER INDEX PK_CR REBUILD PARALLEL 4;
ALTER INDEX PK_CT REBUILD PARALLEL 4;
ALTER INDEX PK_CX REBUILD PARALLEL 4;
ALTER INDEX PK_DM REBUILD PARALLEL 4;
ALTER INDEX PK_EX REBUILD PARALLEL 4;
ALTER INDEX PK_FI REBUILD PARALLEL 4;
ALTER INDEX PK_H REBUILD PARALLEL 4;
ALTER INDEX PK_HH REBUILD PARALLEL 4;
ALTER INDEX PK_HS REBUILD PARALLEL 4;
ALTER INDEX PK_IN REBUILD PARALLEL 4;
ALTER INDEX PK_LT REBUILD PARALLEL 4;
ALTER INDEX PK_NI REBUILD PARALLEL 4;
ALTER INDEX PK_NX REBUILD PARALLEL 4;
ALTER INDEX PK_S REBUILD PARALLEL 4;
ALTER INDEX PK_SC REBUILD PARALLEL 4;
ALTER INDEX PK_SE REBUILD PARALLEL 4;
ALTER INDEX PK_ST REBUILD PARALLEL 4;
ALTER INDEX PK_T REBUILD PARALLEL 4;
ALTER INDEX PK_TH REBUILD PARALLEL 4;
ALTER INDEX PK_TR REBUILD PARALLEL 4;
ALTER INDEX PK_TT REBUILD PARALLEL 4;
ALTER INDEX PK_TX REBUILD PARALLEL 4;
ALTER INDEX PK_WI REBUILD PARALLEL 4;
ALTER INDEX PK_WL REBUILD PARALLEL 4;
ALTER INDEX PK_ZC REBUILD PARALLEL 4;
```

__H.__ Enable the Foreign keys.

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
ALTER TABLE COMPANY_COMPETITOR ENABLE CONSTRAINT FK_CO_CP_CC2;
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
ALTER TABLE HOLDING_HISTORY ENABLE CONSTRAINT FK_T_HH_CC2;
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

- Check if the constraints are enabled.

```
select constraint_name, table_name, status, r_owner from dba_constraints
where constraint_name like 'FK_%' order by status;
```

```
CONSTRAINT_NAME TABLE_NAME STATUS R_OWNER
FK_AD_C_CC CUSTOMER ENABLED TIBERO
FK_AD_CO_CC COMPANY ENABLED TIBERO
FK_AD_EX_CC EXCHANGE ENABLED TIBERO
FK_B_CA_CC CUSTOMER_ACCOUNT ENABLED TIBERO
FK_B_TR_CC TRADE_REQUEST ENABLED TIBERO
FK_C_CA_CC CUSTOMER_ACCOUNT ENABLED TIBERO
FK_C_CX_CC CUSTOMER_TAXRATE ENABLED TIBERO
FK_C_WL_CC WATCH_LIST ENABLED TIBERO
FK_CA_AP_CC ACCOUNT_PERMISSION ENABLED TIBERO
FK_CA_HS_CC HOLDING_SUMMARY ENABLED TIBERO
FK_CA_T_CC TRADE ENABLED TIBERO
FK_CO_CP_CC COMPANY_COMPETITOR ENABLED TIBERO
FK_CO_CP_CC2 COMPANY_COMPETITOR ENABLED TIBERO
FK_CO_FI_CC FINANCIAL ENABLED TIBERO
FK_CO_NX_CC NEWS_XREF ENABLED TIBERO
FK_CO_S_CC SECURITY ENABLED TIBERO
FK_EX_CR_CC COMMISSION_RATE ENABLED TIBERO
FK_EX_S_CC SECURITY ENABLED TIBERO
FK_HS_H_CC HOLDING ENABLED TIBERO
FK_IN_CO_CC COMPANY ENABLED TIBERO
FK_IN_CP_CC COMPANY_COMPETITOR ENABLED TIBERO
FK_NI_NX_CC NEWS_XREF ENABLED TIBERO
FK_S_DM_CC DAILY_MARKET ENABLED TIBERO
FK_S_HS_CC HOLDING_SUMMARY ENABLED TIBERO
FK_S_LT_CC LAST_TRADE ENABLED TIBERO
FK_S_T_CC TRADE ENABLED TIBERO
FK_S_TR_CC TRADE_REQUEST ENABLED TIBERO
FK_S_WI_CC WATCH_ITEM ENABLED TIBERO
FK_SC_IN_CC INDUSTRY ENABLED TIBERO
FK_ST_B_CC BROKER ENABLED TIBERO
FK_ST_C_CC CUSTOMER ENABLED TIBERO
FK_ST_CO_CC COMPANY ENABLED TIBERO
FK_ST_S_CC SECURITY ENABLED TIBERO
FK_ST_T_CC TRADE ENABLED TIBERO
FK_ST_TH_CC TRADE_HISTORY ENABLED TIBERO
FK_T_CT_CC CASH_TRANSACTION ENABLED TIBERO
FK_T_H_CC HOLDING ENABLED TIBERO
FK_T_HH_CC HOLDING_HISTORY ENABLED TIBERO
FK_T_HH_CC2 HOLDING_HISTORY ENABLED TIBERO
FK_T_SE_CC SETTLEMENT ENABLED TIBERO
FK_T_TH_CC TRADE_HISTORY ENABLED TIBERO
FK_T_TR_CC TRADE_REQUEST ENABLED TIBERO
FK_TT_CH_CC CHARGE ENABLED TIBERO
FK_TT_CR_CC COMMISSION_RATE ENABLED TIBERO
FK_TT_T_CC TRADE ENABLED TIBERO
FK_TT_TR_CC TRADE_REQUEST ENABLED TIBERO
FK_TX_CX_CC CUSTOMER_TAXRATE ENABLED TIBERO
FK_WL_WI_CC WATCH_ITEM ENABLED TIBERO
FK_ZC_AD_CC ADDRESS01 ENABLED TIBERO
```

__I.__ Check the rows of each table.

- Total 35.

```
select count(*) from ACCOUNT_PERMISSION;
select count(*) from ADDRESS01;
select count(*) from BROKER;
select count(*) from CASH_TRANSACTION;
select count(*) from CHARGE;
select count(*) from COMMISSION_RATE;
select count(*) from COMPANY;
select count(*) from COMPANY_COMPETITOR;
select count(*) from CUSTOMER;
select count(*) from CUSTOMER_ACCOUNT;
select count(*) from CUSTOMER_TAXRATE;
select count(*) from DAILY_MARKET;
select count(*) from EXCHANGE;
select count(*) from FINANCIAL;
select count(*) from HOLDING;
select count(*) from HOLDING_HISTORY;
select count(*) from HOLDING_SUMMARY;
select count(*) from INDUSTRY;
select count(*) from LAST_TRADE;
select count(*) from NEWS_ITEM;
select count(*) from NEWS_XREF;
select count(*) from SECTOR;
select count(*) from SECURITY;
select count(*) from SETTLEMENT;
select count(*) from STATUS_TYPE;
select count(*) from TAXRATE;
select count(*) from TRADE;
select count(*) from TRADE_HISTORY;
select count(*) from TRADE_REQUEST;
select count(*) from TRADE_TYPE;
select count(*) from WATCH_ITEM;
select count(*) from WATCH_LIST;
select count(*) from ZIP_CODE;
select count(*) from NEXT_ID;
select count(*) from TRADEX;
```

- Correct results.

```
ACCOUNT_PERMISSION  1065476
ADDRESS01           225004
BROKER              1500
CASH_TRANSACTION    158978081
CHARGE              15
COMMISSION_RATE     240
COMPANY             75000
COMPANY_COMPETITOR  225000
CUSTOMER            150000
CUSTOMER_ACCOUNT    750000
CUSTOMER_TAXRATE    300000
DAILY_MARKET        134088750
EXCHANGE            4
FINANCIAL           1500000
HOLDING             19890073
HOLDING_HISTORY     227854510
HOLDING_SUMMARY     2470468
INDUSTRY            102
LAST_TRADE          102750
NEWS_ITEM           150000
NEWS_XREF           150000
SECTOR              12
SECURITY            102750
SETTLEMENT          172800000
STATUS_TYPE         5
TAXRATE             320
TRADE               172981915
TRADE_HISTORY       414717295
TRADE_REQUEST       0
TRADE_TYPE          5
WATCH_ITEM          15005112
WATCH_LIST          150000
ZIP_CODE            14741
NEXT_ID             1
TRADEX              TRADEX 없음
```

__J.__ Remaining issues.

1) Broker table only has 500 rows.

- ADD 1000 BROKER rows. The ZREF generator only generated 500 BROKER rows and other tables expected 1500 BROKER tables to be generated.

```
insert into broker
select b_id + 500,
b_st_id,
b_name,
b_num_trades,
b_comm_total
from broker
where b_id < 4300000501;


insert into broker
select b_id + 1000,
b_st_id,
b_name,
b_num_trades,
b_comm_total
from broker
where b_id < 4300000501;

commit;

ALTER TABLE ZREF.CUSTOMER_ACCOUNT ENABLE CONSTRAINT FK_B_CA_CC;
```

2) NEXT_ID table should be inserted like below.

```
INSERT INTO NEXT_ID (KEY1, KEY2, ID_VALUE, ORDINAL) values ('TRADE', 'BMK-JOB00000', 0, 0);

INSERT INTO NEXT_ID (KEY1, KEY2, ID_VALUE, ORDINAL) VALUES ('TRADE', 'BASE', 200100000000000, 0);
```

3) NEWS_ITEM table data exceeds the column length.

```
awk -F'|' 'BEGIN{OFS="|";} {print $1,$2,$3," ",$5,$6,$7;}' NEWS_ITEM.txt > NEWS_ITEM3.txt

-rw-r--r--. 1 oftibr dba    57696889 Jun 22 21:52 NEWS_ITEM3.txt
```

```
cp loader_NEWS_ITEM.ctl loader_NEWS_ITEM3.ctl
```

```
vi loader_NEWS_ITEM3.ctl

LOAD DATA
INFILE 'NEWS_ITEM3.txt'
LOGFILE 'NEWS_ITEM3.log'
BADFILE 'NEWS_ITEM3.bad'
TRUNCATE
INTO TABLE TIBERO.NEWS_ITEM
FIELDS TERMINATED BY '|'
           OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
```

```
tbloader userid=tibero/tmax@TVSAM control=loader_NEWS_ITEM3.ctl message=10000000 rows=10000000
```

```
NEWS_ITEM3.txt

Table TIBERO.NEWS_ITEM, was loaded from the data file.

COLUMN_NAME                      POSITION DATATYPE
------------------------------ ---------- ----------------
NI_ID                                   1 CHARACTER
NI_HEADLINE                             2 CHARACTER
NI_SUMMARY                              3 CHARACTER
NI_ITEM                                 4 CHARACTER
NI_DTS                                  5 CHARACTER
NI_SOURCE                               6 CHARACTER
NI_AUTHOR                               7 CHARACTER

Table TIBERO.NEWS_ITEM :
        150000 Rows were requested to load.
        150000 Rows were loaded successfully.
        0 Rows were failed to load because of some errors

Total Elapsed Time was    : 00:00:31.371371
```



# zref

## Table of Contents <!-- omit in toc -->

- [zref](#zref)
  - [1. Overview](#1-overview)
  - [2. Environment](#2-environment)
    - [2.1. directory structure](#21-directory-structure)
    - [2.2. Online scenario](#22-online-scenario)
  - [3. Issues](#3-issues)
    - [3.1. Compilation issue](#31-compilation-issue)
    - [3.2. Runtime issue](#32-runtime-issue)
    - [3.3. VARCHAR type column](#33-varchar-type-column)
    - [3.4. Copybooks](#34-copybooks)
    - [3.5. Invalid char in source](#35-invalid-char-in-source)
    - [3.6. modification on JCL](#36-modification-on-jcl)
    - [3.7. SD modification](#37-sd-modification)
    - [3.8. update VTAM info](#38-update-vtam-info)
    - [3.9. Increase region process number](#39-increase-region-process-number)
    - [3.10. Transaction file](#30-transaction-file)
  - [4. Oftest](#4-oftest)
    - [4.1. Usage](#41-usage)
    - [4.2. run test script](#42-run-test-script)

## 1. Overview

ZREF BMT: https://docs.google.com/spreadsheets/d/1kMBK1A1tQn2g0cn2J7YKh66Q9tuVhgQyo7XNKqxKE2c/edit#gid=392064127

## 2. Environment

### 2.1. Directory Structure

- COBOL: cd /opt2/tmaxapp/zref/Tmaxwork/TEST
- COPYBOOK: cd /opt2/tmaxapp/zref/Tmaxwork/TEST/copybook
- CICS compile script: cics/compile.sh
- Batch compile script: batch/compile.sh

## 3. Unit Test 

### 3.1 Transaction file

- Transaction files are proviede by the customer.
  - These files are modified to match the DB data.
    - Use transaction file to generate the iput dataset for running batch jobs.
        - command line for generating a dataset using transaction file.
    - Transaction file is an input for the online scenarios.
        - Oftest tool uses this file as an input.

### 3.2 COBOL Complie

- Modifications are needed both for Batch and Online programs.

3.2.1 COMMENT OUT(DOES NOT WORK in OF)

- WITH (ROWLOCK) & WITH (ROWLOCK,UPDLOCK)
- $IF  APPDBMS = "DB2"
- $IF PLATFORM = "MFSEE"

3.2.2 SELECT TOP ~ clause

- SELECT TOP n    

  ```bash
  SELECT * FROM (SELECT
  ~~~~~~~~~~~~~~~~~~~
  ) WHERE ROWNUM <= n
  ```
  **After tbdb2cblcv, take "" out from ROWNUM**

  ```bash
  WHERE ("ROWNUM" <= N)
   WHERE (ROWNUM <= N)
  ```

3.2.3 BLOB 

  ```bash
  KELSEY      05  W-NI-ITEM       PIC X(102400).
  KELSEY*      05  W-NI-ITEM       USAGE IS SQL TYPE IS BLOB(102400).
  ```

3.2.4 Change ADDRESS table name to ADDRESS01
```
 FROM
         SECURITY,
         COMPANY,
         ADDRESS01 CA,
         ADDRESS01 EA,
         ZIP_CODE ZCA,
         ZIP_CODE ZEA,
         EXCHANGE
 WHERE

 *> -------------------------------------------
 *> DECLARE TABLE for ADDRESS
 *> -------------------------------------------
    EXEC SQL DECLARE TABLE ADDRESS01
    ( AD_ID                 bigint       NOT NULL
    , AD_LINE1              varchar(80)
    , AD_LINE2              varchar(80)
    , AD_ZC_CODE            varchar(12)  NOT NULL
    , AD_CTRY               varchar(80)
    ) END-EXEC.
 *> -------------------------------------------
```
  
3.2.5 DATE format of SD transaction from Batch(SDFRBTCH.cob)
  - MM/DD/YYYY from transaction file should be converted to YYYY-MM-DD
    ```
    01 WS-START-DATE-TEMP.
      03 WS-YYYY                   PIC X(4).
      03 WS-SEP1                   PIC X(1) VALUE '-'.
      03 WS-MM                     PIC X(2).
      03 WS-SEP2                   PIC X(1) VALUE '-'.
      03 WS-DD                     PIC X(2).

    01 WS-START-DATE                PIC X(10) VALUE SPACE.

    MOVE MWF1-IN-START-DAY(1:2) TO WS-MM.
    MOVE MWF1-IN-START-DAY(4:2) TO WS-DD.
    MOVE MWF1-IN-START-DAY(7:4) TO WS-YYYY.

    MOVE WS-START-DATE-TEMP     TO WS-START-DATE.

    DM_DATE   >= :WS-START-DATE
    ```

3.2.6 SET ADDRESS does not work in runtime.(TOF4BTCH.cob)

      *$IF PLATFORM = "MFSEE"
      *       REQUIRED MICRO FOCUS CALL FOR NOAMODE COMPILE
      *       CALL 'MFJZLPSA' RETURNING MFSEE-PSA-PTR
      *       SET ADDRESS OF IHAPSA TO MFSEE-PSA-PTR

      *       BELOW IS NOT WORKING FOR MFSEE (UNASSIGNED LINKAGE)
      *       SO USE DUMMY VALUES
               MOVE 'JOB00000'       TO CURRENT-JOBID
               MOVE 'BMK-BTCH'       TO CURRENT-SYSTEMID
      *$ELSE
      *         SET ADDRESS OF IHAPSA   TO NULL
      *         SET ADDRESS OF CVT      TO FLCCVT
      *         SET ADDRESS OF IKJTCB   TO PSATOLD
      *         SET ADDRESS OF IEFTIOT1 TO TCBTIO
      *         SET ADDRESS OF IEZJSCB TO TCBJSCB
      *         SET ADDRESS OF IEFJSSIB TO JSCBSSIB
      *         SET ADDRESS OF IEESMCA TO CVTSMCA
      *         MOVE   SSIBJBID         TO CURRENT-JOBID
      *         MOVE   SMCASID          TO CURRENT-SYSTEMID
      *$END
      *     END-IF

3.2.7 VARCHAR type column

- COPYBOOK modification
  - Generate new copybook by the name of "####-VAR"
  - Host variables for VARCHAR column should be in a structure format.(LEN + TEXT)
    ```
    10  ##########.
        49  ##########-LEN   PIC S9(04)  COMP-5.
        49  ##########-TEXT  PIC X(15).
    ```
  - Batch driver program modification
    ```
     MOVE ######      TO ######-TEXT.
     MOVE 0 TO ######-LEN.
     INSPECT ######-TEXT
     TALLYING ######-LEN
        FOR CHARACTERS BEFORE INITIAL SPACE.
    ```     
  - Frame programs, Business logic program modification
    - Change the copybook name from ### to ###-VAR from EXEC INCLUDE or COPY statement.
      ```
      BVF1-VAR
      BVTX-VAR
      CPF1-VAR
      CPTX-VAR
      DMF1-VAR
      DMTX-VAR
      MFF1-VAR
      MFMSGIN-VAR
      MFTX-VAR
      MSTQ-VAR
      MSTQXP-VAR
      SDF1-VAR
      SDTX-VAR
      TLF3-VAR
      TLTX-VAR
      TOF1-VAR
      TOF2-VAR
      TOF3-VAR
      TOF4-VAR
      TOTX-VAR
      TRF1-VAR
      TRF2-VAR
      TRF4-VAR
      TRF6-VAR
      TSF1-VAR
      TUF3-VAR
      TUTX-VAR
      ```
     - Modification01
      ```
      * MOVE SDF1-IN-SYMBOL         TO  W-SYMBOL-TEXT 
      MOVE SDF1-IN-SYMBOL         TO  W-SYMBOL

     * MOVE CO-DESC-TEXT(1:CO-DESC-LEN)  TO SDF1-OUT-CO-DESC
      MOVE CO-DESC-TEXT(1:CO-DESC-LEN)  TO SDF1-OUT-CO-DESC-TEXT
      
      * MOVE  SYMBOL1I              TO MAP1-SYMBOL.
       MOVE  SYMBOL1I              TO MAP1-SYMBOL-TEXT.
       MOVE  SYMBOL1L              TO MAP1-SYMBOL-LEN.
     
       05  MAP1-SYMBOL.
          49  MAP1-SYMBOL-LEN      PIC S9(04)  COMP-5.
          49  MAP1-SYMBOL-TEXT     PIC X(15).
        
      * MOVE  MAP1-SYMBOL           TO TUTX-IN-SYMBOL.
       MOVE  MAP1-SYMBOL-LEN           TO TUTX-IN-SYMBOL-LEN.
       MOVE  MAP1-SYMBOL-TEXT           TO TUTX-IN-SYMBOL-TEXT.
      ```

     - Modification02
      ```
      IF (TOF3-IN-SYMBOL EQUAL TO SPACES  OR
         TOF3-IN-SYMBOL EQUAL TO LOW-VALUES)
      IF (TOF3-IN-SYMBOL-TEXT EQUAL TO SPACES  OR
          TOF3-IN-SYMBOL-TEXT EQUAL TO LOW-VALUES)
      ```
      - Modification03
      ```
      IF SQL-ERR-COUNT = 120000
      DISPLAY 'ERROR LIMIT (120000) REACHED IN TO ROUTINE '
      ```
      
3.2.8 Copybooks

- Delete ^Z from copybook.

- Variables which should be expanded by ofcbpp.
  ```
  *EXEC SQL INCLUDE ERRWA END-EXEC. (BTCHDRVR.COB)
  COPY ERRWA.

  *EXEC SQL INCLUDE SDF1 END-EXEC. (SDFRBTCH.COB)
  COPY SDF1.

  *EXEC SQL INCLUDE TUF3 END-EXEC. (TUF3BTCH.COB)
  COPY TUF3. 
  ```

- Seperate a copybook into two parts.(TABLENAME_TABLE.cpy , TABLENAME_DATA.cpy)
  - DECLARE TABLE (COPY TABLENAME_DATA from WORKING-STORAGE SECTION)
  - EXEC SQL DECLARE **TABLENAME TABLE** -> EXEC SQL DECLARE **TABLE TABLENAME**
    ```
     *> -------------------------------------------
     *> DECLARE TABLE for TABLENAME
     *> -------------------------------------------
        EXEC SQL DECLARE TABLE TABLENAME( 
        ) END-EXEC.
    ```
  - COBOL HOST VARIABLES FOR TABLE (COPY TABLENAME_TABLE PROCEDURE DIVISION)
    ```
    *> -------------------------------------------
    *> COBOL HOST VARIABLES FOR TABLE TABLENAME
    *> -------------------------------------------
    
     *> -------------------------------------------
     *> COBOL INDICATOR VARIABLES FOR TABLE TABLENAME
     *> -------------------------------------------
    ```
    - EXTCACCT.COB - CUSTOMER_ACCOUNT.cpy
    - EXTTRADE.COB - TRADE.cpy
    - EXTCUST1.COB - CUSTOMER.cpy
    - EXTADDR1.COB - ADDRESS.cpy
    
- Put an empty line in between 

```bash 
     EXEC SQL INCLUDE TOF4 END-EXEC.
     (put an empty line in between)
     EXEC SQL INCLUDE ERRWA END-EXEC.  

     EXEC SQL INCLUDE TSF1 END-EXEC.
     (put an empty line in between)
     EXEC SQL INCLUDE ERRWA END-EXEC.
```
    
    
3.2.9 SQLCA
  - SQLCODE was defined as COMP-4 from the old one but it should be changed to COMP-5.
  

### Compile and Deploy 

>> Note that you need to remove .cob extension on PROGRAN-NAME
  
- Batch script
    - Need to check tbpcb and ofcob options.

```
#!/bin/sh
#set -x
CPYPATH=/opt2/tmaxapp/zref/Tmaxwork/TEST/copybook

CBPPOPT="--enable-include --enable-declare"
OFCBOPT="--enable-debug -g --trace"

COBDIR=`pwd`
#COBLIST=`ls -al ${COBDIR}/*.cob | awk '{print $9}' | awk -F["/"] '{print $NF}' | awk -F["."] '{print $1}'`
COBLIST=${1}

cd ${COBDIR}

for cobfile in ${COBLIST}
do
  echo "-------------- OFCBPP STEP : ${cobfile}.cob --------------"
  egrep "EXEC.*SQL| COPY " ${cobfile}.cob > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
    echo "[SKIP] OFCBPP STEP"
    cp ${cobfile}.cob ${cobfile}.cbpp
  else
    cmd="ofcbpp -i ${cobfile}.cob -o ${cobfile}.cbpp -copypath ${CPYPATH}"
    $cmd
    if [ $? -ne 0 ]; then
       echo "[FAIL] OFCBPP STEP : $cmd"
       exit 1;
     else
       echo "[SUCCESS] OFCBPP STEP"
    fi
  fi

  echo "------------ TBDB2CBLCV STEP : ${cobfile}.cbpp ------------"
  grep "EXEC.*SQL" ${cobfile}.cbpp > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
   echo "[SKIP] TBDB2CBLCV STEP"
   cp ${cobfile}.cbpp ${cobfile}.cbl
  else
   tbdb2cblcv ${cobfile}.cbpp > ${cobfile}.cbl
   sed -i 's/"ROWNUM"/ROWNUM/g' ${cobfile}.cbl
   if [ $? -ne 0 ]; then
      echo "[FAIL] TBDB2CBLCV STEP"
      exit 1;
     else
       echo "[SUCCESS] TBDB2CBLCV STEP"
  fi
 fi

  echo "-------------   TBPCB STEP : ${cobfile}.cbl   -------------"
  grep "EXEC.*SQL" ${cobfile}.cbl > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
   echo "[SKIP] TBPCB STEP"
   cp ${cobfile}.cbl ${cobfile}_tbpcb.cbl
  else
#  cmd="tbpcb INCLUDE=${CPYPATH} END_OF_FETCH=100 COMP5=NO CODE=COBOL UNSAFE_NULL=YES VARCHAR=YES DB2_SYNTAX=YES INAME=${cobfile}.cbl ONAME=${cobfile}_tbpcb.cbl LOG_LVL=TRACE"
  cmd="tbpcb INCLUDE=${CPYPATH} END_OF_FETCH=100 COMP5=NO CODE=COBOL UNSAFE_NULL=YES VARCHAR=YES DB2_SYNTAX=YES INAME=${cobfile}.cbl ONAME=${cobfile}_tbpcb.cbl"
  $cmd
  if [ $? -ne 0 ]; then
     echo "[FAIL] TBPCB STEP : $cmd"
     exit 1;
    else
       echo "[SUCCESS] TBPCB STEP"
  fi
 fi
 

  echo "------------ OSCCBLPP STEP : ${cobfile}_tbpcb.cbl ------------"
  grep "EXEC.*CICS" ${cobfile}_tbpcb.cbl > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
    echo "[SKIP] OSCCBLPP STEP"
    cp ${cobfile}_tbpcb.cbl ${cobfile}_osccblpp.cbl
  else
    cmd="osccblpp -o ${cobfile}_osccblpp.cbl ${cobfile}_tbpcb.cbl"
    $cmd
    if [ $? -ne 0 ]; then
     echo "[FAIL] OSCCBLPP STEP : $cmd"
     exit 1;
    else
       echo "[SUCCESS] OSCCBLPP STEP"
  fi
 fi


  echo "------------ OFCOB STEP : ${cobfile}_osccblpp.cbl -------------"
#  cmd="ofcob -U -o ${cobfile}.so ${cobfile}_osccblpp.cbl -L$OPENFRAME_HOME/lib --trace -lofcee -ltextfh -ltextsm -L$TB_HOME/client/lib/ -ltbertl -L$ODBC_HOME/lib -lodbc -ltbertl_odbc -lclientcommon -lofcom -g --force-trace2 --enable-debug"
  cmd="ofcob -U -o ${cobfile}.so ${cobfile}_osccblpp.cbl -L$OPENFRAME_HOME/lib --trace -lofcee -ltextfh -ltextsm -L$TB_HOME/client/lib/ -ltbertl -L$ODBC_HOME/lib -lodbc -ltbertl_odbc -lclientcommon -lofcom -g "
#  cmd="ofcob -U -x  ${cobfile}_osccblpp.cbl -L$OPENFRAME_HOME/lib --trace -lofcee -ltextfh -ltextsm -L$TB_HOME/client/lib/ -ltbertl -L$ODBC_HOME/lib -lodbc -ltbertl_odbc -lclientcommon -lofcom -g --force-trace2 --enable-debug"
#  cmd="ofcob -U -o ${cobfile}.so ${cobfile}_osccblpp.cbl -L$OPENFRAME_HOME/lib --trace -lofcee -ltextfh -ltextsm -L$TB_HOME/client/lib/ -ltbertl -L$ODBC_HOME/lib -lodbc -lclientcommon -lofcom -g --force-trace2 --enable-debug"
  $cmd
  if [ $? -ne 0 ]; then
    echo "[FAIL] OFCOB STEP : $cmd"
    exit 1;
    else
       echo "[SUCCESS] OFCOB STEP"
  fi

  echo "-------------    DEPLOY STEP : ${cobfile}.so   --------------"
  cp ${cobfile}.so $OPENFRAME_HOME/volume_default/PPLIP.ZREF.LIBLOAD
  cp ${cobfile}.so $OPENFRAME_HOME/volume_default/SYS1.USERLIB

done
```

- Online script
    - Need to check tbpcb and ofcob options.
```
#!/bin/sh
#set -x
CPYPATH=/opt2/tmaxapp/zref/Tmaxwork/TEST/copybook

CBPPOPT="--enable-include --enable-declare"
OFCBOPT="--enable-debug -g --trace"

COBDIR=`pwd`
#COBLIST=`ls -al ${COBDIR}/*.cob | awk '{print $9}' | awk -F["/"] '{print $NF}' | awk -F["."] '{print $1}'`
COBLIST=${1}

cd ${COBDIR}

for cobfile in ${COBLIST}
do
  echo "-------------- OFCBPP STEP : ${cobfile}.cob --------------"
  egrep "EXEC.*SQL| COPY " ${cobfile}.cob > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
    echo "[SKIP] OFCBPP STEP"
    cp ${cobfile}.cob ${cobfile}.cbpp
  else
    cmd="ofcbpp -i ${cobfile}.cob -o ${cobfile}.cbpp -copypath ${CPYPATH}"
    $cmd
    if [ $? -ne 0 ]; then
       echo "[FAIL] OFCBPP STEP : $cmd"
       exit 1;
     else
       echo "[SUCCESS] OFCBPP STEP"
    fi
  fi

  echo "------------ TBDB2CBLCV STEP : ${cobfile}.cbpp ------------"
  grep "EXEC.*SQL" ${cobfile}.cbpp > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
   echo "[SKIP] TBDB2CBLCV STEP"
   cp ${cobfile}.cbpp ${cobfile}.cbl
  else
   tbdb2cblcv ${cobfile}.cbpp > ${cobfile}.cbl
   sed -i 's/"ROWNUM"/ROWNUM/g' ${cobfile}.cbl
   if [ $? -ne 0 ]; then
      echo "[FAIL] TBDB2CBLCV STEP"
      exit 1;
     else
       echo "[SUCCESS] TBDB2CBLCV STEP"
  fi
 fi

  echo "-------------   TBPCB STEP : ${cobfile}.cbl   -------------"
  grep "EXEC.*SQL" ${cobfile}.cbl > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
   echo "[SKIP] TBPCB STEP"
   cp ${cobfile}.cbl ${cobfile}_tbpcb.cbl
  else
# cmd="tbpcb INCLUDE=${CPYPATH} END_OF_FETCH=100 COMP5=NO CODE=COBOL UNSAFE_NULL=YES VARCHAR=YES DB2_SYNTAX=YES INAME=${cobfile}.cbl ONAME=${cobfile}_tbpcb.cbl LOG_LVL=TRACE RUNTIME_MODE=ODBC"
  cmd="tbpcb INCLUDE=${CPYPATH} END_OF_FETCH=100 COMP5=NO CODE=COBOL UNSAFE_NULL=YES VARCHAR=YES DB2_SYNTAX=YES INAME=${cobfile}.cbl ONAME=${cobfile}_tbpcb.cbl"
#  cmd="tbpcb INCLUDE=${CPYPATH} END_OF_FETCH=100 COMP5=NO CODE=COBOL UNSAFE_NULL=YES VARCHAR=YES DB2_SYNTAX=YES INAME=${cobfile}.cbl ONAME=${cobfile}_tbpcb.cbl  RUNTIME_MODE=ODBC"
  $cmd
  if [ $? -ne 0 ]; then
     echo "[FAIL] TBPCB STEP : $cmd"
     exit 1;
    else
       echo "[SUCCESS] TBPCB STEP"
  fi
 fi
 

  echo "------------ OSCCBLPP STEP : ${cobfile}_tbpcb.cbl ------------"
  grep "EXEC.*CICS" ${cobfile}_tbpcb.cbl > /dev/null; RC=`echo ${?}`
  if [ ${RC} != 0 ]; then
    echo "[SKIP] OSCCBLPP STEP"
    cp ${cobfile}_tbpcb.cbl ${cobfile}_osccblpp.cbl
  else
    cmd="osccblpp -o ${cobfile}_osccblpp.cbl ${cobfile}_tbpcb.cbl"
    $cmd
    if [ $? -ne 0 ]; then
     echo "[FAIL] OSCCBLPP STEP : $cmd"
     exit 1;
    else
       echo "[SUCCESS] OSCCBLPP STEP"
  fi
 fi


  echo "------------ OFCOB STEP : ${cobfile}_osccblpp.cbl -------------"
  cmd="ofcob -U -o ${cobfile}.so ${cobfile}_osccblpp.cbl -L$OPENFRAME_HOME/lib --trace -lofcee -ltextfh -ltextsm -L$TB_HOME/client/lib/ -ltbertl -L$ODBC_HOME/lib -lodbc -ltbertl_odbc -lclientcommon -lofcom -g"
#  cmd="ofcob -U -o ${cobfile}.so ${cobfile}_osccblpp.cbl -L$OPENFRAME_HOME/lib --trace -lofcee -ltextfh -ltextsm -L$TB_HOME/client/lib/ -ltbertl -L$ODBC_HOME/lib -lodbc -lclientcommon -lofcom -g --force-trace2 --enable-debug"
  $cmd
  if [ $? -ne 0 ]; then
    echo "[FAIL] OFCOB STEP : $cmd"
    exit 1;
    else
       echo "[SUCCESS] OFCOB STEP"
  fi

  echo "-------------    TDLUPDATE STEP : ${cobfile}.so   --------------"
#  cp ${cobfile}.so $OPENFRAME_HOME/osc/region/ZREFCE/tdl/mod
  cp ${cobfile}.so $OPENFRAME_HOME/osc/region/ZREFMEE/tdl/mod
#  cmd="osctdlupdate ZREFCE ${cobfile}"
  cmd="osctdlupdate ZREFMEE ${cobfile}"
  $cmd
   if [ $? -ne 0 ]; then
   echo "[FAIL] TDLUPDATE STEP : $cmd"
else
  echo "[SUCCESS] TDLUPDATE  STEP"
fi


done
```

- If all programs are modified and compiled successfully, you only need to move the complied modules like below.

```
[Online]

cp ${cobfile}.so $OPENFRAME_HOME/osc/region/ZREFMEE/tdl/mod

cp ${cobfile}.so $OPENFRAME_HOME/osc/region/ZREFCE/tdl/mod

[Batch]

cp ${cobfile}.so $OPENFRAME_HOME/volume_default/PPLIP.ZREF.LIBLOAD

cp ${cobfile}.so $OPENFRAME_HOME/volume_default/SYS1.USERLIB
```

-> tdlupdate will automatically occurs when you boot up the region, or you need to use 

```
osctdlupdate ZREFCE ${cobfile}

osctdlupdate ZREFMEE ${cobfile}
```  

### 3.3 Batch

3.3.1 Prepare JCL

1) SETUP JCL

- dos2unix ALL JCL

    - Total 7.
    
          DEFAUDB.JCL
          DEFAUDB1.JCL
          DEFAUDOL.JCL
          DEFCONFG.JCL
          DEFTXNFL.JCL
          DEFTXNLB.JCL
          GDGDEFINE.JCL
          
- In case of JCL that uses "idcams define", copybook is needed for the dataset.
  - OpenFrame/tsam/copybook
     - ZREF.KSDS.CONFIG.cpy
          ```
          01 APP-CONFIG-REC.
            05 ACR-KEY PIC X(8).
            05 ACR-DATA.
              10 ACR-GLOBAL-CONFIG.
                15 ACR-GC-WRITE-AUDIT-FLAG PIC X.
                15 ACR-GC-EXPLICIT-DBCONN-FLAG PIC X.
                15 ACR-GC-DBCONN-NAME PIC X(20).
                15 FILLER PIC X(8).
              10 ACR-MSTQ-CONFIG.
                15 ACR-MSTQ-SEND-METHOD PIC X.
                15 ACR-MSTQ-SEND-TO-MACHINE PIC X(50).
                15 ACR-MSTQ-SEND-TO-PORT PIC X(5).
                15 FILLER PIC X(14).
            05 FILLER PIC X(92).
          ```
      - ZREF.ESDS.AUDTRAIL.cpy &  PPLIP.ZREF.BAT##.AUDTRAIL.cpy (PPLIP.ZREF.BAT01.AUDTRAIL.cpy ~ PPLIP.ZREF.BAT10.AUDTRAIL.cpy)
          ```
          004100 01 FD-AUDREC.
          004200     05 AUDIT-KEY.
          004300       10 A-TRANS-ID.
          004400         15 A-TRAN-ID PIC X(02).
          004500         15 FILLER-ID PIC X(02).
          004600       10 A-DATE.
          004700         15 A-DTE PIC X(08).
          004800         15 FILLER PIC X(02).
          004900       10 A-TIME PIC X(06).
          005000     05 A-DATA-AREA PIC X(2413).
          005000     05 A-DATA-FILLER PIC X(4).
          ```
- Modification
  - RECFM=LSEQ -> RECFM=FB
  - Delete "//MFE:" line.
  - VOLUMES(PIPV01) ->  VOLUMES(DEFVOL)
  
2) Transaction JCL

- dos2unix ALL JCL

    - Total 5 (2 Pairs).
    
          BATBDRVR.JCL
          BATBDR##.JCL
          BATBDRVA##.JCL
          INDXWT#.JCL
          RPTONLY#.JCL

- Modification
  - When the first time you run the BATBR**.JCL, modify the JCL to report the dataset as below.
     - BATBDR**.JCL   
         ```
         AS IS
         //BVREPORT  DD DUMMY,DCB=(RECFM=FBA,LRECL=133)             
         //*VREPORT  DD DSN=PPLIP.ZREF.REPT.BV,                     
         //*            DISP=(NEW,CATLG,DELETE),                    
         //*            DISP=OLD,                                   
         //*            UNIT=SYSDA,                                 
         //*            SPACE=(CYL,(111,22),RLSE),                  
         //*            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0,BUFNO=60)

         TO BE
         //*BVREPORT  DD DUMMY,DCB=(RECFM=FBA,LRECL=133)           
         //BVREPORT  DD DSN=PPLIP.ZREF.REPT.BV,                    
         //            DISP=(NEW,CATLG,DELETE),                    
         //*            DISP=OLD,                                  
         //            UNIT=SYSDA,                                 
         //            SPACE=(CYL,(111,22),RLSE),                  
         //            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0,BUFNO=60)
        ```

### 3.4 Online

1) Online region build

- $OPENFRAME_HOME/tsam/copybook
   - Need to generate the correct copybooks for SD datasets.
   ```
   OFAPP1@oframe:/opt2/tmaxapp/OpenFrame/tsam/copybook>ls -al
	-rwxr-xr-x 1 oframe mqm  394 Dec  1 23:23 OSC.OIVP.FILE.cpy
	-rwxr-xr-x 1 oframe mqm  394 Dec  1 23:23 OSC.PIVP.FILE.cpy
	-rwxr-xr-x 1 oframe mqm  154 Dec  1 23:23 OSC.SCSVRLIB.OSCOIVP1.cpy
	-rwxr-xr-x 1 oframe mqm  153 Dec  1 23:23 OSC.SDLIB.OSCOIVP1.cpy
	-rwxr-xr-x 1 oframe mqm  153 Dec  1 21:05 OSC.SDLIB.TEST.cpy
	-rwxr-xr-x 1 oframe mqm  153 Dec  1 21:05 OSC.SDLIB.ZREFCE.cpy
	-rwxr-xr-x 1 oframe mqm  153 Dec  1 21:05 OSC.SDLIB.ZREFMEE.cpy
	-rwxr-xr-x 1 oframe mqm  154 Dec  1 23:23 OSC.TDQLIB.INTRA.cpy
	-rwxr-xr-x 1 oframe mqm  155 Dec  1 23:23 OSC.TSQLIB.DATA.cpy
	-rwxr-xr-x 1 oframe mqm  149 Dec  1 23:23 OSC.TSQLIB.KEY.cpy
	-rwxr-xr-x 1 oframe mqm  153 Dec  1 23:23 OSC.VTAM.DEF.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT01.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT02.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT03.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT04.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT05.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT06.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT07.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT08.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT09.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT10.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT12.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 PPLIP.ZREF.BAT19.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  116 Dec  1 21:05 SYS1.VTAMLST.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 ZREFB.ESDS.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  430 Dec  1 21:05 ZREF.ESDS.AUDTRAIL.cpy
	-rwxr-xr-x 1 oframe mqm  773 Dec  1 21:05 ZREF.KSDS.CONFIG.cpy
   ``` 
- Region build
```
oscbuild -o LINUX64 -d TIBERO -s ZREFCE -b OFCOBOL
oscbuild -o LINUX64 -d TIBERO -s ZREFMEE -b OFCOBOL

cp -a ZREFCE $TMAXDIR/appbin/
cp -a ZREFMEE $TMAXDIR/appbin/
```

- Online datasets.

   - SD dataset
	```
	01 VSAM-RECORD.
	    03 VSAM-KEY  PIC X(18).
	    03 VSAM-DATA PIC X(4078).
	    03 VSAM-DUMMY REDEFINES VSAM-DATA PIC X(4078).
	``` 
	```
	idcams define -t CL -n [SD dataset name] -o KS -k 18,0 -b 32768 -l 4096,4096 -s 1024,128,128 -v DEFVOL
	```

   - TDQ dataset
	```
	01 VSAM-RECORD.
	    03 VSAM-KEY  PIC X(8).
	    03 VSAM-DATA PIC X(32752).
	    03 VSAM-DUMMY REDEFINES VSAM-DATA PIC X(32752).
	``` 
	```
	idcams define -t CL -n [TDQ dataset name] -o KS -k 8,0 -l 128,32760 -b 32767 -s 1024,128,128 -v DEFVOL
	```

   - TSQ(KEY and DATA) dataset
	```
	01 VSAM-RECORD.
	    03 VSAM-KEY  PIC X(16).
	    03 VSAM-DATA PIC X(48).
	    03 VSAM-DUMMY REDEFINES VSAM-DATA PIC X(48).
	``` 
	```
	idcams define -t CL -n [TSQ KEY dataset name] -o KS -k 16,0 -l 64,64 -s 1024,128,128 -v DEFVOL
	```
	```
	01 VSAM-RECORD.
	    03 VSAM-KEY  PIC X(18).
	    03 VSAM-DATA PIC X(32742).
	    03 VSAM-DUMMY REDEFINES VSAM-DATA PIC X(32742).
	```
	```	
	idcams define -t CL -n [TSQ DATA dataset name] -o KS -k 18,0 -l 128,32760 -b 32767 -s 1024,128,128 -v DEFVOL
	```
[REGION BUILD] PDF file
<img src="./reference_images/Region build - Kelsey.pdf" title="Region build">

  - SD modification.
      - ADD missing PROGRAMS in each region
	  ```
	  DEFINE PROGRAM(##BL0001) GROUP(ZREFCE) LANGUAGE(COBOL)
	  DEFINE PROGRAM(##FR000#) GROUP(ZREFCE) LANGUAGE(COBOL)
	  ```

     - ADD TERMINAL (For loading testing, a lot of terminals are needed T000~T999)
	  ```
	  DEFINE TERMINAL(T000) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TESTT000) INSERVICE(YES)
	  DEFINE TERMINAL(T001) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TESTT001) INSERVICE(YES)

	  DEFINE TERMINAL(TRM9) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TESTTRM9) INSERVICE(YES)
	  DEFINE TERMINAL(TTRM) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TESTTERM) INSERVICE(YES)
	  ```

- Register the CSD resource definition to SD dataset.
```
Register system online resource
oscsdgen -c -d [SD dataset name] $OPENFRAME_HOME/osc/resource/osc.dat


Register User CSD
oscsdgen -c -d [SD dataset name] [user resource file]
```




0) Map complie

- mscasmc [map file]

- mcsmapc [map file] -r [region name]

- masmapupdate
	```
	mscmapupdate version 7.0.3(5) obuild@tplinux64:ofsrc7/osc(#1) 2019-12-17 16:18:32
	MSC Map dynamic Update Utility

	Usage: mscmapupdate [options]
	     | mscmapupdate <region>
	     | mscmapupdate <region> -f <file_name> [-n]
	     | mscmapupdate <region> -l <mapset> [-n]
	     | mscmapupdate <region> -r <mapset> [-n]
	     <region>              Specify region name
				   (if no options used, display loaded mapset list)
	    -f <physical map file> Load specified file as mapset file
				   (can not use with '-n' option)
	    -l <mapset>            Load mapset from default mapset directory
	    -r <mapset>            Release mapset
	Options:
	    -h                     Display this information
	    -v                     Display version information
	    -n (optional)          Syncronize mapupdate in multi-clustering environment
	```

1) SD modification

2) Scenarios
- ZREFMEE region (HPMEE3270)
  - TR01: SQL ERR:-654657|23000|TR2Z| UNIQUE constraint violation ('TIBERO'.'PK_HH'). -> Need to use different scenarios each time.
  - MF01: OK

- ZREFCE region (HPCE)
  - BV01: OK
  - CP01: OK
  - SD01: OK
  - MW01: OK
  - TO01: OK
  - TS01: OK
  - TU01: OK
  - TL01: OK
  
  
 3) Update VTAM Setting

- VTAMDUMP -> VTAMGEN (*modify the dump file and generate a new vtam definition.*)
  - TESTT000..TESTT999.. -> match it with the TERMINAL NETNAME from SD.
  - FFFFFNNN -> match the format
    ```
      BEGINVTAM
      PORT 5556
      LUGROUP LUGRP1 TESTT000..TESTT999..FFFFFNNN ENDLUGROUP
      LUGROUP LUGRP2 TESTTERM ENDLUGROUP
      IPGROUP IPGRP1 1.1.1.1..100.100.100.100 ENDIPGROUP
      IPGROUP IPGRP2 100.100.100.101..255.255.255.255 ENDIPGROUP
      LUMAP LUGRP1 IPGRP1
      LUMAP LUGRP2 IPGRP2
        ENDVTAM
    ```
  
### 3.2. Runtime issue

3.2.1 TIP file modification
```
#---------------------------------------------------------
## DATE & TIME FORMAT
###---------------------------------------------------------
NLS_TIME_FORMAT="HH24.MI.SSXFF"
NLS_TIMESTAMP_FORMAT="YYYY-MM-DD-HH24.MI.SSXFF"
NLS_TIMESTAMP_TZ_FORMAT="YYYY-MM-DD-HH24.MI.SSXFF"
NLS_DATE_FORMAT="YYYY-MM-DD"
```

```
SQL> SELECT DISTINCT(SYSTIMESTAMP) from dual; -> before the change

SYSTIMESTAMP
---------------------------------------------
2020/04/23 05:51:26.456306 Etc/Universal
1 row selected.


SQL> SELECT DISTINCT(SYSTIMESTAMP) from dual; -> after the change

SYSTIMESTAMP
----------------------------------------------
2020-04-23-06.23.00.301

1 row selected.
```



### 3.7. 

### 3.8. 

### 3.9. Increase region process number

- modify oframe.m and increase MIN of the region process
```
ZREFMEE_TCL1   SVGNAME = svgtboiv,
                TARGET = ZREFMEE,
                SVRTYPE = STD_DYN,
                MIN = 20,
                MAX = 128,
                ASQCOUNT = 1,
                MAXQCOUNT = -1,
                MAXRSTART = -1,
                GPERIOD = 86400,
                LIFESPAN = IDLE_600,
                CLOPT = "-n -o $(SVR)_$(CDATE).out -e $(SVR)_$(CDATE).err"
```

## 4. Oftest

### 4.1. Usage

```bash
jangwon@jangwon:~/git/zref$ oftest -h
usage: oftest [-h] -i INPUT -s SERVER [-w WAIT] [-l LOG] [--ssl] [--visible]

optional arguments:
  -h, --help            show this help message and exit
  -i INPUT, --input INPUT
                        test input as JSON format
  -s SERVER, --server SERVER
                        3270 server URL <ip>:<port> ex) 127.0.0.1:5556
  -w WAIT, --wait WAIT  wait given seconds after the test is over
  -l LOG, --log LOG     set log level (DEBUG|INFO|WARNING|ERROR|CRITICAL).
                        default is INFO
  --ssl                 enable ssl
  --visible             make 3270 emulator visible
```

### 4.2. run test script

- sequence.sh
- parallel.sh

### 5. TPC-E

- TPC-E is an On-Line Transaction Processing Benchmark
  - Approved in February of 2007, TPC Benchmark E is an on-line transaction processing (OLTP) benchmark. TPC-E is more complex than previous OLTP benchmarks such as TPC-C because of its diverse transaction types, more complex database and overall execution structure. TPC-E involves a mix of twelve concurrent transactions of different types and complexity, either executed on-line or triggered by price or time criteria. The database is comprised of thirty-three tables with a wide range of columns, cardinality, and scaling properties. TPC-E is measured in transactions per second (tpsE). While the benchmark portrays the activity of a stock brokerage firm, TPC-E is not limited to the activity of any particular business segment, but rather represents any industry that must report upon and execute transactions of a financial nature.

