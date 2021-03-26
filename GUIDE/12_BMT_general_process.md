# BMT general process by Kelsey

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

### 1. Server Setting.

Hardware Spec.

A. APP Servers

```
ncpu - 16
memory - 32G
size - 500G 
```

<details>
	<summary>Size Info example</summary>
	
```
Filesystem                     Size  Used Avail Use% Mounted on
devtmpfs                        16G     0   16G   0% /dev
tmpfs                           16G   60M   16G   1% /dev/shm
tmpfs                           16G  508K   16G   1% /run
tmpfs                           16G     0   16G   0% /sys/fs/cgroup
/dev/nvme0n1p1                 8.0G  6.5G  1.5G  82% /
/dev/mapper/appsvrvg-appsvrlv  450G  259M  450G   1% /opt2
tmpfs                          3.1G     0  3.1G   0% /run/user/10001
```
</details>


B. DB Servers

```
ncpu - 16
memory - 128G
size - 1TB
```

_Azure, AWS BMT OpenFrame and Tibero Binary information._

<details>
	<summary>IMS#210963</summary>
	
```
IMS#210963 was used for Azure BMT, Please use the same one for AWS BMT.

Please use the Tibero binary from IMS#202734, Action No.   1314675
tibero6-bin-FS06_CS_1902-linux64-172015-opt-20191122153758-tested.tar.gz(467,472 KB) - 2019.12.03 15:28:41.
```
</details>


Others 

A. Ip Setting. Use private ip.

```
vi /etc/hosts (Both)

127.0.0.1   localhost localhost.localdomain localhost4 localhost4.localdomain4
::1         localhost6 localhost6.localdomain6
10.0.1.212  OFAPP1 OFAPP1
10.0.2.78   OFDB1  OFDB1
```

B. Add user as hostname

```
groupadd mqm -g 10000
useradd -d /home/oframe -g mqm -s /bin/bash -m oframe -u 10001

groupadd dba -g 10005
useradd -d /home/oftibr -g dba -s /bin/bash -m oftibr -u 10002


sudo passwd oftibr
Changing password for user oftibr.
New password: 
Retype new password: 
passwd: all authentication tokens updated successfully.
```

C. bash_profile

<details>
	<summary>OpenFrame</summary>

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
</details>

<details>
	<summary>Tibero</summary>

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

TB_HOME=/opt2/tmaxdb/tibero6; export TB_HOME
TB_SID=TVSAM; export TB_SID
PATH=$TB_HOME/script:$TB_HOME/bin:$TB_HOME/client/bin:$PATH; export PATH
LD_LIBRARY_PATH=$TB_HOME/lib:$TB_HOME/client/lib:/lib:$LD_LIBRARY_PATH; export LD_LIBRARY_PATH
LD_LIBRARY_PATH_64=$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH_64; export LD_LIBRARY_PATH_64
export TB_NLS_DATE_FORMAT="YYYY-MM-DD HH24:MI:SS"
TBMON_HOME=$TB_HOME/tbmon; export TBMON_HOME
AIXTHREAD_SCOPE=S; export AIXTHREAD_SCOPE

#TB_CONN_TIMEOUT=10; export TB_CONN_TIMEOUT
#TB_READ_TIMEOUT=180; export TB_READ_TIMEOUT
```
</details>

## BMT Overview

ZREF BMT: https://docs.google.com/spreadsheets/d/1kMBK1A1tQn2g0cn2J7YKh66Q9tuVhgQyo7XNKqxKE2c/edit#gid=392064127

_First, you need to checl the directory structure._

```
- COBOL: cd /opt2/tmaxapp/zref/Tmaxwork/TEST
- COPYBOOK: cd /opt2/tmaxapp/zref/Tmaxwork/TEST/copybook
- CICS compile script: cics/compile.sh
- Batch compile script: batch/compile.sh
```
### 2. DB Migration

[DB Migration 방법 링크 추가]

### 3. Transaction file

- Transaction files are proviede by the customer.
  - These files are modified to match the DB data.

3.1 Batch - Prepare input datasets using transaction file.

- Use transaction file to generate the iput dataset for running batch jobs.
  - command line for generating a dataset using transaction file.

- From the JCL using transaction file as an input.

<pre>
//TXNFILE   DD DSN=PPLIP1.ZREF.LIBBATTX(BATTX##),DISP=SHR
</pre>

A. Check the PDS information before generating input datasets.

<pre>
listcat -a PPLIP1.ZREF.LIBBATTX
<details>
<summary>result</summary>
List Catalog Entry Information

-----------------------------------------------------------------------------
  Data Set Name . . . : PPLIP1.ZREF.LIBBATTX
  Data Set Type . . . : NONVSAM
  Catalog Name  . . . : SYS1.MASTER.ICFCAT

  Management Class  . : 
    Creation Date . . : 2020/05/11      Data Set Owner  . : oframe
    Expiration Date . : ***None***

  Storage Class . . . : 
    Volume Serial . . : DEFVOL          Device Type . . . : 3380

  Data Class  . . . . : 
    Organization  . . : PO              Record Format . . : LB
    KEYLEN  . . . . . : 0               Record Length . . : 32760
    KEYPOS  . . . . . : 0               Block Size  . . . : 32768

  Current Allocation
    Primary Space . . : N.A.            Number of Extents : 
    Secondary Space . : N.A.            Data Set Size . . : 0

  Last Access Date
    Last Access Date  : 2021/02/09      Last Access Time  : 10:51:28

  Members
  ------------------------------------------------------------------
  Name                  Owner     Size           Last Access Date
  ------------------------------------------------------------------
  BATTX00                         8413800        2020/07/13 04:34:26
  BATTX05                         8413800        2020/07/07 21:35:09
  BATTX06                         8411083        2020/07/07 21:35:18
  BATTX07                         8405258        2020/07/07 21:35:26
  BATTX12                         8407441        2020/07/07 21:35:34
  BATTX19                         8401445        2020/07/07 21:35:41
-----------------------------------------------------------------------------
* Total 1 entries in catalog SYS1.MASTER.ICFCAT printed.
</details>
</pre>
	
B. Create empty datasets and use dssave to load the transaction file to it.
	
<pre>
dscreate PPLIP1.ZREF.LIBBATTX -f LB -l 32760 -b 32768 -o PO
</pre>

<pre>
dscreate "PPLIP1.ZREF.LIBBATTX(BATTX05)"
</pre>

<pre>
dssave "PPLIP1.ZREF.LIBBATTX(BATTX05)" -s $PWD/BATTX05.DAT -d "\r\n"
<details>
    <summary>log</summary>
dssave version 7.0.3(7) obuild@tplinux64:ofsrc7/base(#1) 2019-12-10 15:05:02
Dataset Save Program for External Editor

DSSAVE
Source File        : [/home/oframe/KELSEY/Tmaxwork/TEST/TXNFILES/BATTX05.DAT]
Destination Dataset: [PPLIP1.ZREF.LIBBATTX(BATTX05)]
Destination Member : []
User Catalog       : []
Volume Serial      : []
Delimiter          : [\r\n]

OFRUISVRDSSAVE: Dataset Is Saved Successfully
COMPLETED SUCCESSFULLY.
</details>
</pre>	
	
3.2 Online - Oftest tool uses transaction files as an input

- Transaction file is an input for the online scenarios.

[Oftest tool 사용법 링크 추가]

### 4. COBOL Sources

4.1 COBOL complie

4.1.1 Modifications for compile.

- Modifications are needed both for Batch and Online programs.

A. COMMENT OUT(DOES NOT WORK in OF)

<pre>
- WITH (ROWLOCK) & WITH (ROWLOCK,UPDLOCK)
- $IF  APPDBMS = "DB2"
- $IF PLATFORM = "MFSEE"
</pre>

B. SELECT TOP ~ clause
    - SELECT TOP n    

<pre>
  SELECT * FROM (SELECT
  ~~~~~~~~~~~~~~~~~~~
  ) WHERE ROWNUM <= n
</pre>

	After tbdb2cblcv, take "" out from ROWNUM

<pre>
WHERE ("ROWNUM" <= N)
WHERE (ROWNUM <= N)
</pre>


C. BLOB

<pre>
KELSEY      05  W-NI-ITEM       PIC X(102400).
KELSEY*      05  W-NI-ITEM       USAGE IS SQL TYPE IS BLOB(102400).
</pre>

D. Change ADDRESS table name to ADDRESS01

- From Cobol
<pre>
 FROM
         SECURITY,
         COMPANY,
         ADDRESS01 CA,
         ADDRESS01 EA,
         ZIP_CODE ZCA,
         ZIP_CODE ZEA,
         EXCHANGE
</pre>

- From copybooks
<pre>
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
</pre>
  
E. DATE format of SD transaction from Batch(SDFRBTCH.cob)
- MM/DD/YYYY from transaction file should be converted to YYYY-MM-DD.
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

F. SET ADDRESS does not work in runtime.
- TOF4BTCH.cob
```
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
```

G. VARCHAR type column
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
      
H. Copybooks

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
    <details>
	<summary>copybook list</summary>
	EXTCACCT.COB - CUSTOMER_ACCOUNT.cpy
	EXTTRADE.COB - TRADE.cpy
	EXTCUST1.COB - CUSTOMER.cpy
	EXTADDR1.COB - ADDRESS.cpy
    </details>
    
    ```
    *> -------------------------------------------
    *> COBOL HOST VARIABLES FOR TABLE TABLENAME
    *> -------------------------------------------
    
     *> -------------------------------------------
     *> COBOL INDICATOR VARIABLES FOR TABLE TABLENAME
     *> -------------------------------------------
    ```
    
- Put an empty line in between.

```bash 
     EXEC SQL INCLUDE TOF4 END-EXEC.
     (put an empty line in between)
     EXEC SQL INCLUDE ERRWA END-EXEC.  

     EXEC SQL INCLUDE TSF1 END-EXEC.
     (put an empty line in between)
     EXEC SQL INCLUDE ERRWA END-EXEC.
```
      
I. SQLCA
  - SQLCODE was defined as COMP-4 from the old one but it should be changed to COMP-5.
  - Add the missing part in $TB_HOME/client/include/SQLCA.
    ```
    002000    05 SQLEXT.
    002100        10 SQLWARN8          PIC X.
    002200        10 SQLWARN9          PIC X.
    002300        10 SQLWARNA          PIC X.
    002400        10 SQLSTATE          PIC X(5).
    ```
    
4.2.2 Program compile

>> Note that you need to remove .cob extension on PROGRAN-NAME

<details>
	<summary>Batch script (click here)</summary>
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
</details>

<details>
	<summary>Online script (click here)</summary>
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
</details>

4.2 COBOL Deploy

- If all programs are modified and compiled successfully, you only need to move the complied modules like below.

A.Batch

<pre>
cp ${cobfile}.so $OPENFRAME_HOME/volume_default/PPLIP.ZREF.LIBLOAD
cp ${cobfile}.so $OPENFRAME_HOME/volume_default/SYS1.USERLIB
</pre>

B. Online

<pre>
cp ${cobfile}.so $OPENFRAME_HOME/osc/region/ZREFMEE/tdl/mod
cp ${cobfile}.so $OPENFRAME_HOME/osc/region/ZREFCE/tdl/mod

-> tdlupdate will automatically occurs when you boot up the region, or you need to use 

osctdlupdate ZREFCE ${cobfile}
osctdlupdate ZREFMEE ${cobfile}
</pre>
  

### 5. Unit Test

5.1 Batch

A. Prepare PDS and Batch modules.
<pre>
1) Create a correct PDS for locating batch modules.

OFAPP1@oframe:/home/oframe/KELSEY/Tmaxwork>pdsgen PPLIP.ZREF.LIBLOAD DEFVOL -l 2062
pdsgen version 7.0.3(2) obuild@tplinux64:ofsrc7/base(#1) 2019-12-10 15:05:02
PDS Dataset Generation Program

pdsgen: *** PDS PPLIP.ZREF.LIBLOAD is created.
</pre>

<pre>
2) Deploy batch modules under the created PDS.

listcat -a PPLIP.ZREF.LIBLOAD
<details>
<summary>PPLIP.ZREF.LIBLOAD information</summary>
	List Catalog Entry Information

	-----------------------------------------------------------------------------
	  Data Set Name . . . : PPLIP.ZREF.LIBLOAD
	  Data Set Type . . . : NONVSAM
	  Catalog Name  . . . : SYS1.MASTER.ICFCAT

	  Management Class  . : 
	    Creation Date . . : 2020/04/20      Data Set Owner  . : oframe
	    Expiration Date . : ***None***

	  Storage Class . . . : 
	    Volume Serial . . : DEFVOL          Device Type . . . : 3380

	  Data Class  . . . . : 
	    Organization  . . : PO              Record Format . . : FB
	    KEYLEN  . . . . . : 0               Record Length . . : 2062
	    KEYPOS  . . . . . : 0               Block Size  . . . : 4096

	  Current Allocation
	    Primary Space . . : 2048(MB)        Number of Extents : 0
	    Secondary Space . : 0(KB)           Data Set Size . . : 0

	  Last Access Date
	    Last Access Date  : 2020/04/20      Last Access Time  : 02:22:03

	  Members
	  ------------------------------------------------------------------
	  Name                  Owner     Size           Last Access Date
	  ------------------------------------------------------------------
	  ACINSTAL.so                     44120          2020/05/17 02:32:13
	  ACUI0001.so                     91992          2020/05/17 02:32:13
	  AUDTRAIL.so                     44584          2020/05/17 02:32:13
	  BTCHDRVA.so                     176528         2020/06/26 13:50:28
	  BTCHDRVR.so                     345616         2020/07/01 19:24:08
	  BTCHDRVR1.so                    243056         2020/05/13 02:55:27
	  BVBL0001.so                     51568          2020/05/17 02:32:12
	  BVBLBTCH.so                     37832          2020/06/26 13:50:30
	  BVFR0001.so                     131248         2020/05/17 02:32:12
	  BVFRBTCH.so                     242640         2020/06/26 13:50:31
	  BVINBTCH.so                     18656          2020/06/26 13:50:31
	  BVUI0001.so                     89816          2020/05/17 02:32:11
	  CPBL0001.so                     71928          2020/05/17 02:32:11
	  CPBLBTCH.so                     45392          2020/06/26 13:50:31
	  CPF1.so                         67544          2020/06/08 06:54:52
	  CPF1BTCH.so                     233512         2020/06/26 13:50:33
	  CPF1BTCH.so_ORIG                233104         2020/04/28 07:07:04
	  CPF1BTCH_sample.so              67592          2020/06/08 06:48:07
	  CPF1BTCH_test.so                41944          2020/05/04 05:02:31
	  CPF1TEST.so                     88040          2020/06/05 00:56:23
	  CPF2BTCH.so                     155784         2020/06/26 13:50:33
	  CPF3BTCH.so                     25912          2020/06/26 13:50:34
	  CPFR0003.so                     37664          2020/05/17 02:32:10
	  CPINBTCH.so                     20400          2020/06/26 13:50:34
	  DAVEDRVR.so                     176080         2020/06/02 23:19:21
	  DAVEEXTR.so                     34496          2020/06/24 01:34:12
	  DAVESCHM.so                     27328          2020/06/22 16:12:47
	  EXTADDR1.so                     70448          2020/06/26 13:50:34
	  EXTCACCT.so                     74592          2020/06/26 13:50:34
	  EXTCUST1.so                     132944         2020/06/26 13:50:35
	  EXTTRADE.so                     158712         2020/06/26 13:50:36
	  KELSDRVR.so                     91648          2020/05/13 08:27:48
	  MRGCADDR.so                     32608          2020/06/26 13:50:36
	  MRGCCACT.so                     32504          2020/06/26 13:50:36
	  MRGCTRAD.so                     56456          2020/06/26 13:50:36
	  MRGREPT.so                      27512          2020/06/26 13:50:37
	  MRGTRRPT.so                     57016          2020/06/26 13:50:37
	  MWBLBTCH.so                     20456          2020/06/26 13:50:37
	  MWFRBTCH.so                     278144         2020/06/26 13:50:39
	  MWINBTCH.so                     18408          2020/06/26 13:50:39
	  SDBLBTCH.so                     439968         2020/06/26 18:27:21
	  SDFRBTCH.so                     803008         2020/06/26 13:50:41
	  SDINBTCH.so                     18432          2020/06/26 13:50:42
	  TLBLBTCH.so                     138032         2020/06/26 13:50:42
	  TLF1BTCH.so                     189944         2020/07/01 22:37:43
	  TLF2BTCH.so                     207752         2020/06/26 13:50:44
	  TLF3BTCH.so                     226128         2020/06/26 13:50:45
	  TLF4BTCH.so                     147672         2020/06/26 13:50:46
	  TLINBTCH.so                     18920          2020/06/26 13:50:46
	  TOBLBTCH.so                     42824          2020/06/26 13:50:46
	  TOBLBTCH_selena.so              40752          2020/05/12 07:22:29
	  TODRVR.so                       86952          2020/05/13 09:04:41
	  TOF1BTCH.so                     108200         2020/06/26 13:50:47
	  TOF2BTCH.so                     56368          2020/06/26 13:50:47
	  TOF3BTCH.so                     525648         2020/06/26 13:50:50
	  TOF4BTCH.so                     302984         2020/06/26 19:34:33
	  TOF4BTCH_final.so               315344         2020/05/18 05:29:53
	  TOF5BTCH.so                     29304          2020/06/26 13:50:52
	  TOF6BTCH.so                     20512          2020/06/26 13:50:52
	  TOFR0006.so                     20128          2020/05/15 22:38:07
	  TOINBTCH.so                     19656          2020/06/26 13:50:52
	  TOUI0001.so                     85992          2020/05/14 00:22:33
	  TSBLBTCH.so                     56880          2020/06/26 13:50:52
	  TSF1BTCH.so                     204928         2020/06/26 13:50:53
	  TSINBTCH.so                     17960          2020/06/26 13:50:54
	  TUBLBTCH.so                     121240         2020/06/26 19:49:48
	  TUF1BTCH.so                     266752         2020/07/14 16:46:27
	  TUF2BTCH.so                     377440         2020/07/14 16:44:32
	  TUF3BTCH.so                     372128         2020/07/14 16:38:18
	  TUINBTCH.so                     18920          2020/06/26 19:49:30
	-----------------------------------------------------------------------------
	* Total 1 entries in catalog SYS1.MASTER.ICFCAT printed.
</details>
</pre>	
	
B. Prepare JCL.
<pre>
1) SETUP JCL
  - dos2unix ALL JCL.
  - Total 7.
     DEFAUDB.JCL
     DEFAUDB1.JCL
     DEFAUDOL.JCL
     DEFCONFG.JCL
     DEFTXNFL.JCL
     DEFTXNLB.JCL
     GDGDEFINE.JCL
  - In case of JCL that uses "idcams define", copybook is needed for the dataset. ( $OPENFRAME_HOME/tsam/copybook)
    <details>
    <summary>ZREF.KSDS.CONFIG.cpy</summary>
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
    </details>
</pre>

		  
	      - ZREF.ESDS.AUDTRAIL.cpy &  PPLIP.ZREF.BAT##.AUDTRAIL.cpy (PPLIP.ZREF.BAT01.AUDTRAIL.cpy ~ PPLIP.ZREF.BAT10.AUDTRAIL.cpy)
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
		


	
3.3.4 Set the DB connection as described in the JCL.
	
	ikjeft01.conf

	[SYSTEM:ZREF]
	    DBTYPE=TIBERO
	    DBAUTH=PUBLIC
	    DATABASE=TVSAM
	    INSTANCE=TIBERO
	    USERNAME=tibero
	    PASSWORD=tmax
	

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

     - ADD TERMINAL (For loading testing, a lot of terminals - 5000 in this case.. are needed)
	  ```
	  DEFINE TERMINAL(0001) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TEST0001) INSERVICE(YES)
	  DEFINE TERMINAL(0002) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TEST0002) INSERVICE(YES)

	  DEFINE TERMINAL(5000) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TEST5000) INSERVICE(YES)
	  DEFINE TERMINAL(TTRM) GROUP(CONN) TYPETERM(TESTTTRM) NETNAME(TESTTERM) INSERVICE(YES)
	  ```

- Register the CSD resource definition to SD dataset.
```
Register system online resource
oscsdgen -c -d [SD dataset name] $OPENFRAME_HOME/osc/resource/osc.dat


Register User CSD
oscsdgen -c -d [SD dataset name] [user resource file]
```

2) Map complie

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

3) Scenarios
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
  
4) Update VTAM Setting

- vtamdump -> vtamgen <file name> (*modify the dump file and generate a new vtam definition.*)
  - TEST0001..TEST5000.. -> match it with the TERMINAL NETNAME from SD.
  - FFFFNNN -> match the format
    ```
	BEGINVTAM
	 PORT 5556
	 LUGROUP LUGRP1 TESTTERM ENDLUGROUP
	 LUGROUP LUGRP2 TESTTRM0..TESTTRM9..FFFFFFFN ENDLUGROUP
	 LUGROUP LUGRP3 TEST0001..TEST5000..FFFFNNNN ENDLUGROUP
	 IPGROUP IPGRP1 1.1.1.1..255.255.255.255 ENDIPGROUP
	 LUMAP LUGRP1 IPGRP1
	 LUMAP LUGRP2 IPGRP1
	 LUMAP LUGRP3 IPGRP1
	ENDVTAM
    ```
  
### 3.2. Runtime issue

3.2.1 TIP file modification

- Match the DATE&TIME format from the TABLE.txt file.
```
00000000000002|2005-01-03 09:00:00.778|SBMT
00000000000002|2005-01-03 09:00:02.141|CMPT
00000000000003|2005-01-03 09:00:01.381|SBMT
00000000000003|2005-01-03 09:00:02.859|CMPT

2000-01-03
2000-01-04
2000-01-05
2000-01-06
```

- Add DATE&TIME format from tip file.
```
#---------------------------------------------------------
#DATE & TIME FORMAT
#---------------------------------------------------------
NLS_TIME_FORMAT="HH24:MI:SS.FF3"
NLS_TIMESTAMP_FORMAT="YYYY-MM-DD HH24:MI:SS.FF3"
NLS_TIMESTAMP_TZ_FORMAT="YYYY-MM-DD HH24:MI:SS.FF3"
NLS_DATE_FORMAT="YYYY-MM-DD"
```

- Check the result.
```
SQL> SELECT DISTINCT(SYSTIMESTAMP) from dual; -> before the change

SYSTIMESTAMP
---------------------------------------------
2020/04/23 05:51:26.456306 Etc/Universal
1 row selected.


SQL> SELECT DISTINCT(SYSTIMESTAMP) from dual; -> after the change

SYSTIMESTAMP
--------------------------------------------------------------------------------
2021-02-09 07:52:34.275

1 row selected.
```

3.2.2 OpenFrame Configuration

- $OPENFRAME_HOME/config

```
osc.ZREFCETL.conf
osc.ZREFMEETL.conf
osc.ZREFMEE.conf
osc.ZREFCE.conf
```

```
osc.region.list

Add region
```

```
osc.conf
[OSCMCSVR]                           
        REGION=3                     
        REGION_OSCOIVP1=0-10,100-30  
        REGION_ZREFCE=0-10,100-30    
        REGION_ZREFMEE=0-10,100-30   
```

```
tjes.conf
[INITDEF]
    INITNUM=280
    INIT0000-0029 = INIT00,ABCD,ACTIVE
    INIT0030-0059 = INIT01,EFGH,ACTIVE
    INIT0060-0089 = INIT02,IJKL,ACTIVE
    INIT0090-0119 = INIT02,MNOP,ACTIVE
    INIT0120-0149 = INIT03,QRST,ACTIVE
    INIT0150-0179 = INIT03,UVWX,ACTIVE
    INIT0180-0219 = INIT04,YZ01,ACTIVE
    INIT0220-0249 = INIT05,2345,ACTIVE
    INIT0250-0279 = INIT05,6789,ACTIVE
```

```
ftp.conf	
[DATASET_DEFAULT]      
    CHECK_DSAUTH_V2=NO 
```

### 3.7. 

- vi /etc/security/limits.conf

```
oftibr           soft    nofile          65536
oftibr           hard    nofile          65536
oftibr           soft    nproc           unlimited
oftibr           hard    nproc           unlimited

oframe           soft    nofile          65536
oframe           hard    nofile          65536
oframe           soft    nproc           unlimited
oframe           hard    nproc           unlimited
oframe           soft    core            unlimited
oframe           hard    core            unlimited
```

### 3.8. 

- vi /etc/sysctl.conf 

```
kernel.shmall = 8589934592
kernel.shmmax = 107374182400
kernel.shmmni = 4096
kernel.sem = 100000 32000 10000 10000
fs.file-max=6815744
net.ipv4.ip_local_port_range = 1024 65000

net.core.rmem_default=362144
net.core.rmem_max=4194304
net.core.wmem_default=362144
net.core.wmem_max=1048576
fs.aio-max-nr = 1048576

```

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

### 3.10.

- tbdsn.tbr
 - If tibero is used as client, just untar the tibero binary and generate tbdsn.tbr under $TB_HOME/client/config.
 - Check firewall and shut it down.
```
tac1=(
    (INSTANCE=(HOST=172.31.47.43)
              (PORT=8629)
              (DB_NAME=tac)
    )
    (INSTANCE=(HOST=172.31.38.209)
              (PORT=8629)
              (DB_NAME=tac)
    )
    (USE_FAILOVER=Y)
)
```

```
tac2=(
    (INSTANCE=(HOST=172.31.38.209)
              (PORT=8629)
              (DB_NAME=tac)
    )
    (INSTANCE=(HOST=172.31.47.43)
              (PORT=8629)
              (DB_NAME=tac)
    )
    (USE_FAILOVER=Y)

)
```

### 3.11.

- ofgw.properties
```
dbqueue.maxsize = 100 -> 4096	
translate.threadPool.core = 5 -> 500	
translate.threadPool.max = 30 -> 5000	
tmax.node.NODENAME.max = 1024 -> 4096
```

### 3.12.

- oframe.m
```
*DOMAIN                      
domain                       

#   MAXUSER     = 256, (comment out)       
    MINCLH      = 1 -> 4,         
    MAXCLH      = 3 -> 10,        
    CPC         = 2,         
    BLOCKTIME   = 60 -> 600,       
    MAXCPC      = 256 -> 1000,      
#   TXTIME      = 60, (comment out)       
    MAXSPR      = 512 -> 4000,      
    MAXSVR      = 128 -> 1000,      
```

```
*DOMAIN
domain
    SHMKEY      = 80111,
#   MAXUSER     = 256,
    MINCLH      = 4,
    MAXCLH      = 8,
    CPC= 2,
    BLOCKTIME   = 600,
    MAXCPC      = 1000,
#   TXTIME      = 60,
    MAXSPR      = 4000,
    MAXSVR      = 1000,
    MAXSVC      = 2048,
    DOMAINID    = 4,
    IPCPERM     = 0777,
    TIPSVC      = TIPSVC,
    MAXSACALL   = 1024,
    MAXCACALL   = 1024
```    

- Change the MIN and MAX number of [_region_name_]_TCL1 server.
```
[region_name]_TCL1   SVGNAME = svgtboiv,                                            
                     TARGET = ZREFCE,                                             
                     SVRTYPE = STD_DYN,                                           
                     MIN = 256,                                                   
                     MAX = 256,                                                   
                     ASQCOUNT = 1,                                                
                     MAXQCOUNT = -1,                                              
                     MAXRSTART = -1,                                              
                     GPERIOD = 86400,                                             
                     LIFESPAN = IDLE_600,                                         
                     CLOPT = "-n -o $(SVR)_$(CDATE).out -e $(SVR)_$(CDATE).err"   
```

_If you need detailed information on each option, please check manuals. (TMAX, OFGW manual.)_

### 3.13.

- domain.xml

- Identify the data source for ofgw and increase the connection pool settings.

```
      <data-source>
         <database>
            <data-source-id>ofgw</data-source-id>
            <export-name>ofgw</export-name>
            <data-source-class-name>com.tmax.tibero.jdbc.ext.TbConnectionPoolDataSource</data-source-class-name>
            <data-source-type>ConnectionPoolDataSource</data-source-type>
            <vendor>tibero</vendor>
            <server-name>172.16.0.5</server-name>
            <port-number>8629</port-number>
            <database-name>TVSAM</database-name>
            <user>tibero</user>
            <password>tmax</password>
            <login-timeout>0</login-timeout>
            <auto-commit>DRIVER</auto-commit>
            <stmt-query-timeout>0</stmt-query-timeout>
            <pool-destroy-timeout>10000</pool-destroy-timeout>
            <support-xa-emulation>false</support-xa-emulation>
            <connection-pool>
               <pooling>
                  <min>**2 -> 100**</min> 
                  <max>**30 -> 1000**</max> 
                  <step>1</step>
                  <period>3600000</period>
               </pooling>
```

### 3.14.

- logback.xml

```
        <root level="DEBUG -> OFF">  
                <appender-ref ref="ROLLING" />
                <appender-ref ref="STDOUT"/>
        </root>
```


# Copyrighted by Kelsey
