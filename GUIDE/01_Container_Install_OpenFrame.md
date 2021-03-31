# Container Install OpenFrame by Kelsey

## Table of Contents

+ [Create Image of OpenFrame](#create-image-of-openframe)
+ [1. Install Docker](#1-install-docker)
+ [2. Get CentOS Container](#2-get-centos-container)
+ [3. Install OpenFrame](#3-install-openframe)
  + [3.1 Pre Settings](#31-pre-settings)
  + [3.2 JAVA Installation](#32-java-installation)
  + [3.3 Tibero Installation](#33-tibero-installation)
  + [3.4 UnixODBC Installation](#34-unixodbc-installation)
  + [3.5 OFCOBOL Installation](#35-ofcobol-installation)
  + [3.6 PROSORT Installation](#36-prosort-installation)
  + [3.7 Base Installation](#37-base-installation)
  + [3.8 Batch Installation](#38-batch-installation)
  + [3.9 TACF Installation](#39-tacf-installation)
  + [3.10 OSC Installation](#310-osc-installation)
  + [3.11 JEUS Installation](#311-jeus-installation)
  + [3.12 OFGW Installation](#312-ofgw-installation)
  + [3.13 OFManager Installation](#313-ofmanager-installation)
  + [3.14 OFMiner Installation](#314-ofminer-installation)
+ [4. Create OpenFrame Image](#4-create-openframe-image)
+ [5. Use OpenFrame Image](#5-use-openframe-image)

# Create Image of OpenFrame

# 1. Install Docker

First, you need to get the OpenFrame image to use the AKS service. To create it, you need Docker account and install Docker in your VM. Your account will be needed when you push/pull the images in your Dockerhub repository.

```bash
sudo apt-get update
sudo apt-get remove docker docker-engine docker.io
sudo apt install docker.io
sudo systemctl start docker
sudo systemctl enable docker
sudo docker –version
```

# 2. Get CentOS Container

## A. Run an empty Centos container to install OpenFrame.
* Search the official Centos image and pull it on your VM. Use it to run a container.

## B. Set the hostname with -h option when you run it.
* OpenFrame will need a hostname to get the licenses or set the environment.

* Ubuntu
```bash
sudo docker search centos
sudo docker pull centos
sudo docker run -h [hostname] -i -t centos
```

* CentOS
```bash
sudo yum check-update
sudo yum update
sudo yum install -y yum-utils device-mapper-persistent-data lvm2
sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
sudo yum install docker
sudo systemctl start docker
sudo systemctl enable docker
sudo systemctl status docker0
```

Other docker commands :

| COMMAND                    | DESCRIPTION                     |
|----------------------------|---------------------------------|
| docker ps                  | check running containers        |
| docker ps -a               | check all containers            |
| docker exec [container ID] | execute a running container     |
| docker stop [container ID] | stop the container              |
| docker rm   [container ID] | remove the container            |

**docker run and docker exec are different!!**

Example :

```sudo docker ps -a | grep centos```
     
     CONTAINER ID  IMAGE   COMMAND      CREATED        STATUS       PORTS  NAMES          
     fc58fa646357  centos  "/bin/bash"  2 minutes ago  Up 2 minutes        keen_poitras

```sudo docker exec -i -t fc58fa646357 /bin/bash```

```sudo docker stop fc58fa646357```


# 3. Install OpenFrame

## 3.1 Pre Settings

### A. Required package installation

- If you see "error: rpmdbNextIterator: skipping h#" error message, please use this command below.
    - It happens when followed package that was having problems when querying the rpm database for a package that was installed which cause meta tag mess up:

``` rpm --rebuilddb ```

```bash 
yum install -y wget
yum install -y  dos2unix
yum install -y  glibc*
yum install -y  glibc.i686 glibc.x86_64
yum install -y *libtermcap*
yum install -y  gcc
yum install -y  gcc-c++
yum install -y libncurses*
yum install ncurses*
yum update
```

* Packages for running Tibero
```bash
yum install libaio
yum install libnsl
```

* Extra packages if needed
```bash
yum install strace
yum install ltrace
yum install gdb 
yum install nano 
yum install vim-enhanced 
yum install git 
yum install htop
```

### B. Create symbolic link
```bash
ln -s /usr/lib64/libncurses.so.5.9 /usr/lib/libtermcap.so
ln -s /usr/lib64/libncurses.so.5.9 /usr/lib/libtermcap.so.2
```

### C. Kernel parameters modification 

vi /etc/sysctl.conf  

```bash
kernel.shmall = 2097152
kernel.shmmax = 4294967295
kernel.shmmni = 4096
kernel.sem = 100000 32000 10000 10000
fs.file-max = 65536
net.ipv4.ip_local_port_range = 1024 65000  
```
* The parameters below are not supported in a container environment, so you can discard those.
```bash
net.core.rmem_default=262144
net.core.wmem_default=262144
net.core.rmem_max=262144
net.core.wmem_max=262144
```
```bash
sysctl: cannot stat /proc/sys/net/core/rmem_default: No such file or directory
sysctl: cannot stat /proc/sys/net/core/wmem_default: No such file or directory
sysctl: cannot stat /proc/sys/net/core/rmem_max: No such file or directory
sysctl: cannot stat /proc/sys/net/core/wmem_max: No such file or directory
```

* Refresh the kernel parameters.
```bash
/sbin/sysctl -p 
```

### D. Firewall setting
* Firewall does not work in the container. Instead, you can use port forwarding option(-p) when you run the container. I will talk about this later in 'use OpenFrame image' part.

```
chkconfig iptables off
```

### E. Prepare licenses from Technet
* Use the correct hostname for downloading license files from Technet website.
* You need to check hostname and the number of cores.

### F. Set hostname
* Use -h option when you run the container. It automatically sets the hostname for the container.
* Check /etc/hosts file to see if the hostname sets correctly.

### G. Add user as hostname
``` 
groupadd mqm -g 10000
useradd -d /home/of7azure -g mqm -s /bin/bash -m of7azure -u 10001
```

```
groupadd dba -g 10005
useradd -d /home/oftibr -g dba -s /bin/bash -m oftibr -u 10002
```

### H. Add information in bash_profile
```
# clear screen
clear


echo ""
echo "**********************************************"
echo "***          ##OF 7 DEMO ENV ##            ***"
echo "**********************************************"
echo "***              OF-CS TEAM                  *"
echo "**********************************************"
echo "***  account : oframe7                     ***"
echo "***  Download binary : IMS-222950          ***"
echo "***  Installed Product :                   ***"
echo "***    - java version  1.7.0_79            ***"
echo "***                                        ***"
echo "***                            2020.09.14  ***"
echo "**********************************************"
echo ""
```

**Copy OpenFrame binary files from host to container**
```sudo docker cp [filename] [containername]:[path]```

## 3.2 JAVA Installation

### A. Prepare JAVA rpm file
```
rpm -ivh jdk-7u79-linux-x64.rpm
```

### B. Add the part below to .bash_profile
```
# JAVA ENV
export JAVA_HOME=/usr/java/jdk1.7.0_79/
export PATH=$JAVA_HOME/bin:$PATH
export CLASSPATH=$CLASSPATH:$JAVA_HOME/jre/lib/ext:$JAVA_HOME/lib/tools.jar
```
- Check the JAVA version
```
java -version
java version "1.7.0_79"
Java(TM) SE Runtime Environment (build 1.7.0_79-b15)
Java HotSpot(TM) 64-Bit Server VM (build 24.79-b02, mixed mode)
```

## 3.3 Tibero Installation

```bash
tar -xzvf [tibero tar file]
mv license.xml tibero6/license/
```
```bash
vi .bash_profile

# Tibero6 ENV
export TB_HOME=$HOME/tibero6
export TB_SID=oframe
export TB_PROF_DIR=$TB_HOME/bin/prof
export LD_LIBRARY_PATH=$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH
export PATH=$TB_HOME/bin:$TB_HOME/client/bin:$PATH

source ~/.bash_profile
```
```
sh $TB_HOME/config/gen_tip.sh

vi $TB_HOME/config/$TB_SID.tip
```
```bash
DB_NAME=oframe
LISTENER_PORT=8629
CONTROL_FILES="/home/oframe7/tbdata/c1.ctl"
DB_CREATE_FILE_DEST="/home/oframe7/tbdata" # match the directory CONTROL_FILES
#CERTIFICATE_FILE="/home/oframe7/tibero6/config/svr_wallet/oframe.crt"
#PRIVKEY_FILE="/home/oframe7/tibero6/config/svr_wallet/oframe.key"
#WALLET_FILE="/home/oframe7/tibero6/config/svr_wallet/WALLET"
#EVENT_TRACE_MAP="/home/oframe7/tibero6/config/event.map"
MAX_SESSION_COUNT=100
TOTAL_SHM_SIZE=2G
MEMORY_TARGET=3G 
THROW_WHEN_GETTING_OSSTAT_FAIL = N # THIS IS IMPORTANT (network Kernel Parameters)
```
```
tbboot nomount 
    
tbsql sys/tibero

SQL> CREATE DATABASE
USER SYS IDENTIFIED BY TIBERO
MAXINSTANCES 8                                            
MAXDATAFILES 4096                                         
CHARACTER SET MSWIN949
LOGFILE   GROUP 1 ('redo001.redo') SIZE 512M,            
          GROUP 2 ('redo002.redo') SIZE 512M,            
          GROUP 3 ('redo003.redo') SIZE 512M,            
          GROUP 4 ('redo004.redo') SIZE 512M,            
          GROUP 5 ('redo005.redo') SIZE 512M             
MAXLOGGROUPS 255                                          
MAXLOGMEMBERS 8                                           
NOARCHIVELOG                                              
DATAFILE 'system001.dtf' SIZE 200M autoextend on maxsize 1G
DEFAULT TABLESPACE USR                                    
DATAFILE 'usr001.dtf' SIZE 200M  autoextend on maxsize 1G 
DEFAULT TEMPORARY TABLESPACE TEMP                         
TEMPFILE 'temp001.dtf' SIZE 200M autoextend on maxsize 1G 
UNDO TABLESPACE UNDO0                                     
DATAFILE 'undo001.dtf' SIZE 200M autoextend on maxsize 1G; 
```
```
tbboot
    
sh $TB_HOME/scripts/system.sh 

SYS password : tibero
SYSCAT password : syscat
```
```
tbsql tibero/tmax

create tablespace "DEFVOL" datafile 'DEFVOL.dbf' size 100M autoextend on;
create tablespace "TACF00" datafile 'TACF00.dbf' size 50M  autoextend on;
create tablespace "OFM_REPOSITORY" datafile 'OFM_REPOSITORY.dbf' size 50M  autoextend on;
create tablespace "OFMLOG" datafile 'OFM_LOG.dbf' size 300M  autoextend on next 300M;
create tablespace "OFMGR01" datafile 'OFMGR01.DBF'  size 100M autoextend on  next 50M;
```

### When you need to reinstall the modules.

```
drop tablespace "DEFVOL" including contents and datafiles;
drop tablespace "TACF00" including contents and datafiles;
drop tablespace "OFM_REPOSITORY" including contents and datafiles;
drop tablespace "OFMLOG" including contents and datafiles;
drop tablespace "OFMGR01" including contents and datafiles;
```


## 3.4 UnixODBC Installation

### Container environment does not have make file under /usr/bin.

* Install make,wget packages.

```yum install make```
```yum install wget```

* unixODBC-2.3.7 is suitable for CentOS8 

```bash
wget ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.7.tar.gz
tar -zxvf unixODBC-2.3.7.tar.gz
cd unixODBC-2.3.7
./configure --prefix=$HOME/unixODBC --sysconfdir=$HOME/unixODBC/etc
make
make install
```
```
vi ~/.bash_profile

# UNIX ODBC ENV
export ODBC_HOME=$HOME/unixODBC
export PATH=$ODBC_HOME/bin:$PATH
export LD_LIBRARY_PATH=$ODBC_HOME/lib:$LD_LIBRARY_PATH
export ODBCINI=$HOME/unixODBC/etc/odbc.ini
export ODBCSYSINI=$HOME

source ~/.bash_profile
```    

```
odbcinst -j

unixODBC 2.3.7
DRIVERS............: /home/of7azure/odbcinst.ini
SYSTEM DATA SOURCES: /home/of7azure/odbc.ini
FILE DATA SOURCES..: /home/of7azure/ODBCDataSources
USER DATA SOURCES..: /home/of7azure/unixODBC/etc/odbc.ini
SQLULEN Size.......: 8
SQLLEN Size........: 8
SQLSETPOSIROW Size.: 8
```

```
ln $ODBC_HOME/lib/libodbc.so $ODBC_HOME/lib/libodbc.so.1
ln $ODBC_HOME/lib/libodbcinst.so $ODBC_HOME/lib/libodbcinst.so.1
```

### odbc.ini

```
[oframe]
Description = Tibero ODBC driver for Tibero6
Driver = Tibero
DSN = oframe
SID = oframe
User = tibero
Password = tmax
```

### odbcinst.ini

```
[Tibero]
Description = Tibero ODBC driver for Tibero6
Driver = /home/of7azure/tibero6/client/lib/libtbodbc.so
Setup =
FileUsage =
CPTimeout =
CPReuse =
Driver Logging = 7

[ODBC]
Trace = NO
TraceFile = /home/of7azure/odbc.log
ForceTrace = Yes
Pooling = No
DEBUG = 1
```

- Check the connection using the command below.
```
[oframe7@oframe7 ~]$ isql oframe tibero tmax
+---------------------------------------+
| Connected!                            |
|                                       |
| sql-statement                         |
| help [tablename]                      |
| quit                                  |
|                                       |
+---------------------------------------+
SQL> quit
```

## 3.5 OFCOBOL Installation

```
./OpenFrame_COBOL_4_0_732_Linux_x86_64.bin

source ~/.bash_profile

cp licofcob.dat $OFCOB_HOME/license

ofcob --version
```

```
vi HELLO.cob

000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.     HELLO.
000300 ENVIRONMENT DIVISION.
000400 DATA DIVISION.
000500 WORKING-STORAGE SECTION.
000600 PROCEDURE DIVISION.
000600     DISPLAY "Hello world!"

ofcob -x HELLO.cob 

./HELLO
```

## 3.6 PROSORT Installation

```
tar -xzvf prosort-bin-prosort_2sp3-linux64-2209-opt.tar.gz
```

```
vi ~/.bash_profile

# PROSORT ENV
export PROSORT_HOME=$HOME/prosort
export PROSORT_SID=gbg
export PATH=$PATH:$PROSORT_HOME/bin
export LD_LIBRARY_PATH=$PROSORT_HOME/lib:$LD_LIBRARY_PATH
export LIBPATH=$PROSORT_HOME/lib:$LD_LIBRARY_PATH

source ~/.bash_profile
```

```
mkdir $PROSORT_HOME/license

cp license.xml $PROSORT_HOME/license

$PROSORT_HOME/config/gen_tip.sh
```

```
prosort -h
Usage: prosort [options] [sort script files]

options
-------
  -h             Display this information
  -v             Display version information
  -s             Display state information
  -j             Display profile information
  -x             Use SyncSort compatible mode
```

## 3.7 Base Installation

- Copy OpenFrame and Tmax licenses under each directory.

- base.properties
```
OPENFRAME_HOME=/home/oframe7/OpenFrame
TP_HOST_NAME=oframe7
TP_HOST_IP=127.0.0.1
ODBC_USERNAME=tibero
ODBC_PASSWORD=tmax
ODBC_DATABASE=oframe
TSAM_USERNAME=tibero
TSAM_PASSWORD=tmax
TSAM_DATABASE=oframe
OPENFRAME_LICENSE_PATH=/home/oframe9/Installer/license/OPENFRAME
TMAX_LICENSE_PATH=/home/oframe9/Installer/license/TMAX

TSAM_INCLUDE1=/usr/include
TSAM_INCLUDE2=/usr/local/include
TSAM_INCLUDE3=/usr/lib/gcc/x86_64-redhat-linux/8/include
```

* Please check the include directory!!!!!

```
./OpenFrame_Base7_Fix3R_Linux_x86_64.bin -f base.properties

source ~/.bash_profile

cp /usr/lib/libtermcap.so.2 $TMAXDIR/lib -> use this only if needed.
```

```
tmboot

tmadmin

--- Welcome to Tmax Admin (Type "quit" to leave) --- 

$$1 NODE1 (tmadm): si
------------------------------------------------------------------------
  clh   svrname    (svri)   status     count   qcount   qpcount  emcount
------------------------------------------------------------------------
    0   ofrsasvr   (   4)      RDY         0        0         0        0
    0   ofrlhsvr   (   5)      RDY         0        0         0        0
    0   ofrdmsvr   (   6)      RDY         0        0         0        0
    0   ofrdsedt   (   7)      RDY         0        0         0        0
    0   ofrcmsvr   (   8)      RDY         0        0         0        0
    0   ofruisvr   (   9)      RDY         0        0         0        0
    0   ofrsmlog   (  10)      RDY         0        0         0        0
    0   vtammgr    (  11)      RDY         0        0         0        0
    
quit

tmdown

```

## 3.8 Batch Installation

- batch.properties
```
OPENFRAME_HOME=/home/oframe1/OpenFrame
ODBC_USERNAME=tibero
ODBC_PASSWORD=tmax
ODBC_DATABASE=oframe
```

```
./OpenFrame_Batch7_Fix3_MVS_Linux_x86_64.bin -f batch.properties

source ~/.bash_profile
```

```
tmboot

tmadmin

--- Welcome to Tmax Admin (Type "quit" to leave) --- 

$$1 NODE1 (tmadm): si
------------------------------------------------------------------------
  clh   svrname    (svri)   status     count   qcount   qpcount  emcount
------------------------------------------------------------------------
    0   ofrsasvr   (   4)      RDY         0        0         0        0
    0   ofrlhsvr   (   5)      RDY         0        0         0        0
    0   ofrdmsvr   (   6)      RDY         0        0         0        0
    0   ofrdsedt   (   7)      RDY         0        0         0        0
    0   ofrcmsvr   (   8)      RDY         0        0         0        0
    0   ofruisvr   (   9)      RDY         0        0         0        0
    0   ofrsmlog   (  10)      RDY         0        0         0        0
    0   vtammgr    (  11)      RDY         0        0         0        0
    0   obmjmsvr   (  12)      RDY         0        0         0        0
    0   obmjschd   (  13)      RDY         1        0         0        0
    0   obmjinit   (  14)      RDY         2        0         0        0
    0   obmjhist   (  15)      RDY         0        0         0        0
    0   obmjspbk   (  16)      RDY         0        0         0        0
    0   ofrpmsvr   (  17)      RDY         0        0         0        0
    0   obmtsmgr   (  18)      RDY         0        0         0        0
    0   obmjtimr   (  19)      RDY         0        0         0        0

quit

tmdown

```

For running a JOB, please check the LIBPATH from tjclrun.conf

```bash
[SYSLIB]
    BIN_PATH=${OPENFRAME_HOME}/bin:${OPENFRAME_HOME}/util:${COBDIR}/bin:/usr/local/bin:/bin:${OPENFRAME_HOME}/volume_default/SYS1.USERLIB
    LIB_PATH=${OPENFRAME_HOME}/lib:${OPENFRAME_HOME}/core/lib:${TB_HOME}/client/lib:${COBDIR}/lib:/usr/lib:/lib:/lib/i686:/usr/local/lib:${PROSORT_HOME}/lib:/opt/FSUNbsort/lib:${ODBC_HOME}/lib:${OFCOB_HOME}/lib:${OFASM_HOME}/lib
    COB_PATH=${COBPATH}

```

## 3.9 TACF Installation
- tacf.properties
```
OPENFRAME_HOME=/home/oframe1/OpenFrame
```

```
./OpenFrame_Tacf7_Fix3_Linux_x86_64.bin -f tacf.properties

source ~/.bash_profile
```

```
tmboot

tmadmin

--- Welcome to Tmax Admin (Type "quit" to leave) --- 

$$1 NODE1 (tmadm): si
------------------------------------------------------------------------
  clh   svrname    (svri)   status     count   qcount   qpcount  emcount
------------------------------------------------------------------------
    0   ofrsasvr   (   4)      RDY         0        0         0        0
    0   ofrlhsvr   (   5)      RDY         0        0         0        0
    0   ofrdmsvr   (   6)      RDY         0        0         0        0
    0   ofrdsedt   (   7)      RDY         0        0         0        0
    0   ofrcmsvr   (   8)      RDY         0        0         0        0
    0   ofruisvr   (   9)      RDY         0        0         0        0
    0   ofrsmlog   (  10)      RDY         0        0         0        0
    0   vtammgr    (  11)      RDY         0        0         0        0
    0   obmjmsvr   (  12)      RDY         0        0         0        0
    0   obmjschd   (  13)      RDY         1        0         0        0
    0   obmjinit   (  14)      RDY         2        0         0        0
    0   obmjhist   (  15)      RDY         0        0         0        0
    0   obmjspbk   (  16)      RDY         0        0         0        0
    0   ofrpmsvr   (  17)      RDY         0        0         0        0
    0   obmtsmgr   (  18)      RDY         0        0         0        0
    0   obmjtimr   (  19)      RDY         0        0         0        0
    0   tmsvr      (  20)      RDY         0        0         0        0

quit

```

```
tacfmgr

Input USERNAME  : ROOT
Input PASSWORD  : SYS1

TACFMGR: TACF MANAGER START!!!
QUIT
TACFMGR: TACF MANAGER END!!!

tmdown

```

```
cd $OPENFRAME_HOME/lib

ln -s $ODBC_HOME/lib/libodbc.so.2.0.0 libodbc.so

```

## 3.10 OSC Installation

- osc.properties
```
OPENFRAME_HOME=/home/oframe7/OpenFrame

TSAM_USERNAME=tibero
TSAM_PASSWORD=tmax
TSAM_DATABASE=oframe

VTAM_PORT=5556
```

```
./OpenFrame_OSC7_Fix3_Linux_x86_64.bin -f osc.properties

source ~/.bash_profile
```

```
oscboot -C

oscboot -r OSCOIVP1

tmadmin
--- Welcome to Tmax Admin (Type "quit" to leave) --- 

$$1 NODE1 (tmadm): si
------------------------------------------------------------------------
  clh   svrname    (svri)   status     count   qcount   qpcount  emcount
------------------------------------------------------------------------
    0   ofrsasvr   (   4)      RDY         0        0         0        0
    0   ofrlhsvr   (   5)      RDY        10        0         0        0
    0   ofrdmsvr   (   6)      RDY         0        0         0        0
    0   ofrdsedt   (   7)      RDY         0        0         0        0
    0   ofrcmsvr   (   8)      RDY         0        0         0        0
    0   ofruisvr   (   9)      RDY         0        0         0        0
    0   ofrsmlog   (  10)      RDY         0        0         0        0
    0   vtammgr    (  11)      RDY         0        0         0        0
    0   obmjmsvr   (  12)      RDY         0        0         0        0
    0   obmjschd   (  13)      RDY         1        0         0        0
    0   obmjinit   (  14)      RDY         2        0         0        0
    0   obmjhist   (  15)      RDY         0        0         0        0
    0   obmjspbk   (  16)      RDY         0        0         0        0
    0   ofrpmsvr   (  17)      RDY         0        0         0        0
    0   obmtsmgr   (  18)      RDY         0        0         0        0
    0   obmjtimr   (  19)      RDY         0        0         0        0
    0   tmsvr      (  20)      RDY         0        0         0        0
    0   oscmgr     (  21)      RDY         2        0         0        0
    0   oscmcsvr   (  22)      RDY         0        0         0        0
    0   oscmnsvr   (  23)      RDY         0        0         0        0
    0   oscncsvr   (  24)      RDY         0        0         0        0
    0   oscscsvr   (  25)      RDY         2        0         0        0
    0   oscdfsvr   (  26)      RDY         0        0         0        0
    0   oscjcsvr   (  27)      RDY         0        0         0        0
    0   OSCOIVP1   (  28)      RDY         1        0         0        0
    0   OSCOIVP1C  (  29)      RDY         0        0         0        0
    0   OSCOIVP1OMC (  30)      RDY         0        0         0        0
    0   OSCOIVP1TL (  31)      RDY         2        0         0        0
    0   OSCOIVP1_TCL1 (  32)      RDY         0        0         0        0
    0   TPFMAGENT  (  33)      RDY        12        0         0        0

quit

oscdown
```

## 3.11 JEUS Installation

```
./jeus7_unix_generic_ko.bin

vi ~/.bash_profile

# JEUS ENV
export JEUS_HOME=$HOME/jeus7
PATH="$HOME/jeus7/bin:$HOME/jeus7/lib/system:$HOME/jeus7/webserver/bin:${PATH}"
export PATH

# JEUS alias
alias dsboot='startDomainAdminServer -domain jeus_domain -u administrator -p TmaxFall2020'
alias msboot='startManagedServer -domain jeus_domain -server server1 -u administrator -p TmaxFall2020'
alias msdown='stopServer -u administrator -p jeusadmin -host 192.168.96.195:9936' -> check port number
alias dsdown='stopServer -u administrator -p jeusadmin -host 192.168.96.195:9736' -> check port number

source ~/.bash_profile
```

```
/home/oframe7/jeus7/setup/domain-config-template.properties

# If you want to set encrypted password, change it by set-password command with algorithm option in jeusadmin
jeus.password=jeusadmin
jeus.username=administrator
# Node configuration
nodename=oframe
```

- License 
```
cp license.bin $JEUS_HOME/license/license
```

```
systemctl stop firewalld

systemctl disable firewalld
```

- Connect to JEUS administrator
```
http://192.168.55.33:9736/webadmin
```

## 3.12 OFGW Installation

```
Jeus Domain Name (DEFAULT: jeus_domain): 

Jeus Server Name (DEFAULT: server1): 

Jeus Administrator Password (DEFAULT: ): tmax123

Jeus DAS IP (DEFAULT: 10.0.2.15): 

Jeus DAS Base Port (DEFAULT: 9736): 

DataBase Server Name (DEFAULT: 10.0.2.15):

DataBase Port (DEFAULT: ): 8629      

DataBase SID (DEFAULT: oframe): 

DataBase User ID (DEFAULT: ): tibero

DataBase User PW (DEFAULT: ): tmax

Enter requested information

Gateway Name (DEFAULT: ofgw): 

Gateway PORT (DEFAULT: 5556): 

Datasource ID (DEFAULT: ofgw): 

Application Name (DEFAULT: webterminal): 

Enter requested information
($OPENFRAME_HOME/core/config/oframe.m)

Tmax NODE Name (DEFAULT: NODE1): 

Tmax NODE IP (DEFAULT: 10.0.2.15):

Tmax NODE PORT (DEFAULT: 7001): 

Enter requested information
ex) /home/user/license/OFGWLicense

OpenFrame GW License Path (DEFAULT: ): /home/oframe7/BINARY/LICENSE/OFGWLicense
```

```
http://192.168.55.33:8088/webterminal/
```

## 3.13 OFManager Installation

### ADD A NEW SERVER TO JEUS

```
* jeusadmin 콘솔 툴을 이용하여 서버 추가 방법

1. DAS 기동
$startDomainAdminServer -u <user-name> -p <password>
사용 예) startDomainAdminServer -u administrator -p 1111111

2. jps명령어로 DAS 기동상태 확인
$jps
62936 DomainAdminServerBootstrapper
48116 Jps

3. jeusadmin 툴에 접속
$jeusadmin -u <user-name> -p <password> -p <DAS base port>
사용 예)jeusadmin -u administrator -p 1111111 -port 9736

4. jeusadmin툴에 접속하여 server2라는 이름으로 서버 추가 
[DAS]jeus_domain.adminServer> add-server <SERVER_NAME> -addr <JEUS_IP> -baseport <Server_BasePort> -node <DAS_Nodename> -jvm "-Xmx512m -XX:MaxPermSize=128m"
사용 예) add-server server2 -addr 192.168.105.196 -baseport 9636 -node ofLinux64 -jvm "-Xmx512m -XX:MaxPermSize=128m"

5. server2에 listener 추가
[DAS]jeus_domain.adminServer> add-listener -server <SERVER_NAME> -name <LISTENER_NAME> -port <LISTENER_PORT>
사용 예) add-listener -server server2 -name http-server2 -port 8087

6. server2에 http listener 추가
[DAS]jeus_domain.adminServer> add-web-listener -name <HTTP_NAME> -server <SERVER_NAME> -slref <LISTENER_NAME> -tmin 10
사용 예) add-web-listener -name http2 -server server2 -slref http-server2 -tmin 10

7. JEUS 재기동
Domain.xml에 server2가 추가된 것을 확인



위 명령어를 수행한 예시 내용입니다.
$ jeusadmin -u administrator -p 1111111 -port 9736
Attempting to connect to 127.0.0.1:9736.
The connection has been established to Domain Administration Server adminServer in the domain jeus_domain.
JEUS7 Administration Tool
To view help, use the 'help' command.

[DAS]jeus_domain.adminServer>add-server server2 -addr 192.168.105.196 -baseport 9636 -node ofLinux64 -jvm "-Xmx512m -XX:MaxPermSize=128m"
Successfully performed the ADD operation for server (server2).
Check the results using "list-servers or add-server"

[DAS]qa_domain.adminServer>add-listener -server server2 -name http-server2 -port 8087
Executed successfully, but some configurations were not applied dynamically. It might be necessary to restart the server.
Check the result using 'list-server-listeners -server server2 -name http-server2.

[DAS]qa_domain.adminServer>add-web-listener -name http2 -server server2 -slref http-server2 -tmin 10
Successfully changed only the XML.
Restart the server to apply the changes.
For detailed web connection information, use the 'show-web-engine-configuration -cn' command.
```

```
JEUS DomainAdminServer Password (DEFAULT: ): tmax123

JEUS DomainAdminServer IP (DEFAULT: 10.0.2.15): 

JEUS DomainAdminServer Base Port (DEFAULT: 9736): 

DataBase Server Name (DEFAULT: 10.0.2.15): 

DataBase Port (DEFAULT: ): 8629

DataBase Name (DEFAULT: oframe): 

DataBase User ID (DEFAULT: ): tibero

DataBase User PW (DEFAULT: ): tmax

Datasource ID (DEFAULT: ds_ofm1): 

Application Name (DEFAULT: ofmanager): 

TABLESPACE Name (DEFAULT: OFMGR01): 

Webterminal IP (DEFAULT: 10.0.2.15): 

Webtermianl Application Name (DEFAULT: webterminal): 

OFGW LU PORT (DEFAULT: 5556): 

OFGW Name (DEFAULT: ofgw): 

Enter requested information
ex) /home/user/license/OFManagerLicense

OFManager License Path (DEFAULT: ): /home/oframe7/BINARY/LICENSE/OFManagerLicense
```

```
http://192.168.55.33:8088/ofmanager
```

## 3.14 OFMiner Installation

```
Plase Enter the Domain Name and the Server name
(Installation Server Folder)

Jeus Server Name (DEFAULT: server1): 

Jeus Administrator PW (DEFAULT: ): tmax123

Jeus PORT (DEFAULT: ): 9736

DataBase IP (DEFAULT: 10.0.2.15): 

DataBase PORT (DEFAULT: 8629): 

DataBase NAME (DEFAULT: oframe): 

DataBase User ID (DEFAULT: tibero): 

DataBase User PW (DEFAULT: tmax): 

Enter requested information
ex) /home/user1/license/OFMinerLicense

OFMiner License Path (DEFAULT: ): /home/oframe7/BINARY/LICENSE/OFMinerLicense
```

```
cp OFMinerLicense $OFMINER_HOME/license
```

```
http://192.168.55.33:8088/OFMiner/
```

# 4. Create OpenFrame Image

## A. Exit(stop) the container and commit the current container.

```sudo docker ps -a | grep centos```

    CONTAINER ID  IMAGE    COMMAND      CREATED         STATUS          PORTS   NAMES  
    fc58fa646357  centos   "/bin/bash"  16 hours ago    Up 30 minutes            keen_poitras

``` sudo docker stop fc58fa646357 ```

``` sudo docker commit -a "kelsey" -m "of7azure" keen_poitras kelsey92/of7azurefinal:of7azure ```

## B. You need to name it with the rule below for pushing/pulling the image through Dockerhub.

```bash
username/repository:tag
kelsey92/of7azurefinal:of7azure
```

```sudo docker login```

```bash
Login with your Docker ID to push and pull images from Docker Hub. If you don't have a Docker ID, head over to https://hub.docker.com to create one.
Username: kelsey92
Password: 
WARNING! Your password will be stored unencrypted in /home/of7azure/.docker/config.json.
Configure a credential helper to remove this warning. See
https://docs.docker.com/engine/reference/commandline/login/#credentials-store

Login Succeeded
````

``` sudo docker push kelsey92/of7azurefinal:of7azure ```

``` The push refers to a repository [docker.io/kelsey92/of7azurefinal]```

* If you want to cut down the layers of the image

```sudo docker export f3b5881af3b7 | sudo docker import - kelsey92/of7azurefinal:of7azure```

# 5. Use OpenFrame Image

## A. Docker should be installed and you need to login to Dockerhub.
[Install docker](#1-install-docker)

```bash
sudo docker login
dockerhub username
password
```

## B. Pull the image from the Dockerhub repository.

```sudo docker pull kelsey92/of7azurefinal:of7azure```

## C. Check if the image is successfully pulled in your VM.

```sudo docker images | grep kelsey```

## D. Run the container with OpenFrame image.

- Port forwarding with -p option when you run the container. Use multiple options with all ports you need.

```sudo docker run -i -t -h of7azure -p 9736:9736 -p 8088:8088 -p 8087:8087 kelsey92/of7azurefinal:of7azure /bin/bash```

### If you run more than one container.

- Docker container IP address changes if you run more than one container.

- First container
```bash
18: eth0@if19: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default 
    link/ether 02:42:ac:11:00:03 brd ff:ff:ff:ff:ff:ff link-netnsid 0
    inet 172.17.0.2/16 brd 172.17.255.255 scope global eth0
       valid_lft forever preferred_lft forever
```

- Second container
```bash
18: eth0@if19: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default 
    link/ether 02:42:ac:11:00:03 brd ff:ff:ff:ff:ff:ff link-netnsid 0
    inet 172.17.0.3/16 brd 172.17.255.255 scope global eth0
       valid_lft forever preferred_lft forever
```

**a. Those environment variables from bash_profile should be changed.**

    vi ~/.bash_profile
```bash
TMAX_HOST_ADDR= 127.0.0.1 # localhost ip address
export TMAX_HOST_ADDR
alias msdown1='stopServer -u administrator -p tmax123 -host localhost:9936'
alias msdown2='stopServer -u administrator -p tmax123 -host localhost:9636'
alias dsdown='stopServer -u administrator -p tmax123 -host localhost:9736'
```
    source ~/.bash_profile
    
**b. Region configuration file should be modified.**

    vi osc.OSCOIVP1.conf
```bash
[TDQ]
        TDQ_INTRA_DSNAME=OSC.TDQLIB.INTRA
        TDQ_LOG_ADDRESS=127.0.0.1:8896 # localhost ip address
```

**c. Webterminal setting should be modified.**

    vi ofgw.properties
```bash
tmax.retrytime = 60000
#tmax.node.list = NODE1,NODE2
tmax.node.list = NODE1
tmax.node.NODE1.name = NODE1
tmax.node.NODE1.ip = 127.0.0.1 # localhost ip address
tmax.node.NODE1.port = 8001
tmax.node.NODE1.min = 5
tmax.node.NODE1.max = 1024
tmax.node.NODE1.rate = 2
tmax.node.NODE1.timeout = 20000
tmax.node.NODE1.idletime = 90
```

**d. OFManager setting should be modified.**

    vi ofmanager.properties
```bash
# OFGW Property
openframe.webterminal.url = 127.0.0.1:5556/webterminal # localhost ip address
openframe.webterminal.name= ofgw

# Tmax Property
openframe.tmax.ip= 127.0.0.1 # localhost ip address
openframe.tmax.port= 8001
```

**e. JEUS setting should be modified.**

*Change 172.17.0.3 to 0.0.0.0 from data-resource section to use localhost ip address.*

    vi domain.xml
```bash
   <resources>
      <data-source>
         <database>
            <data-source-id>ofgw</data-source-id>
            <export-name>ofgw</export-name>
            <data-source-class-name>com.tmax.tibero.jdbc.ext.TbConnectionPoolDataSource</data-source-class-name>
            <data-source-type>ConnectionPoolDataSource</data-source-type>
            <vendor>tibero</vendor>
            <server-name>172.17.0.3</server-name> # 0.0.0.0
            <port-number>8629</port-number>
            <database-name>oframe</database-name>
            <user>tibero</user>
            <password>tmax</password>

           (continues)

         <database>
            <data-source-id>ds_ofm1</data-source-id>
            <export-name>ds_ofm1</export-name>
            <data-source-class-name>com.tmax.tibero.jdbc.ext.TbConnectionPoolDataSource</data-source-class-name>
            <data-source-type>ConnectionPoolDataSource</data-source-type>
            <vendor>tibero</vendor>
            <server-name>172.17.0.3</server-name> # 0.0.0.0
            <port-number>8629</port-number>
            <database-name>oframe</database-name>
            <user>tibero</user>
            <password>tmax</password>
```

# Copyrighted by Kelsey
