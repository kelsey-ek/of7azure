# Table of Contents

- [Create image of OpenFrame](#step-1-create-image-of-openframe)
	* [Install docker](#11-install-docker)
	* [Get centos container](#12-get-centos-container)
	* [Install OpenFrame](#13-install-openframe)
	* [Create OpenFrame image](#14-create-openframe-image)
        * [Use OpenFrame image](#15-use-openframe-image)
- [Azure](#step-2-azure)
	* [add AKS service](#21-usage)
	* [connect to the Node](#22-full-example)
	* []

## Step 1. Create image of OpenFrame

### 1.1 Install docker

First, you need to get the OpenFrame image to use the AKS service. To create the image, you need to install Docker and create your own account. Your account will be needed when you push/pull the images in your repository in Dockerhub.

```bash
sudo apt-get update
sudo apt-get remove docker docker-engine docker.io
sudo apt install docker.io
sudo systemctl start docker
sudo systemctl enable docker
sudo docker –version
```

### 1.2 Get CentOs container

**Run an empty Centos container to install OpenFrame.** Search the official Centos image and pull it on your VM. Use the image to run a container.

**Set the hostname with -h option when you run it.** OpenFrame will need the hostname to get the licenses or set the envionment.

```bash
sudo docker search centos
sudo docker pull centos
sudo docker run -h [hostname] -i -t centos
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


### 1.3 Install OpenFrame

- Required Package Installation

```bash 
yum install -y  dos2unix
yum install -y  glibc*
yum install -y  glibc.i686 glibc.x86_64
yum install -y *libtermcap*
yum install -y  gcc
yum install -y  gcc-c++
yum install -y libncurses*
yum install ncurses*
```
-- Packages for running tibero
```bash
yum install libaio
yum install libnsl
```
```bash
yum update
```

- Create symbolic link
```bash
ln -s /usr/lib64/libncurses.so.5.9 /usr/lib/libtermcap.so
ln -s /usr/lib64/libncurses.so.5.9 /usr/lib/libtermcap.so.2
```

- Kernel Parameters Modification 
```bash
kernel.shmall = 2097152
kernel.shmmax = 4294967295
kernel.shmmni = 4096
kernel.sem = 100000 32000 10000 10000
fs.file-max = 65536
net.ipv4.ip_local_port_range = 1024 65000  
```
-- The parameters below are not supported in Centos container, so you can discard those.
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
```bash
/sbin/sysctl –p 
```

- Firewall does not work in the container. Instead, you can use port forwarding option(-p) when you run the container. I will talk about this later in 'use OpenFrame image' part.

- Prepare all licenses from Technet with the correct hostname.



1. Tibero Installation

```bash
tar -xzvf [tibero tar file]
mv license.xml tibero6/license/
```
    vi .bash_profile
```bash
# Tibero6 ENV
export TB_HOME=$HOME/tibero6
export TB_SID=oframe
export TB_PROF_DIR=$TB_HOME/bin/prof
export LD_LIBRARY_PATH=$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH
export PATH=$TB_HOME/bin:$TB_HOME/client/bin:$PATH
```
    source ~/.bash_profile

    sh $TB_HOME/config/gen_tip.sh

    vi $TB_HOME/config/$TB_SID.tip
```bash
DB_NAME=oframe
LISTENER_PORT=8629
CONTROL_FILES="/home/oframe7/tbdata/c1.ctl"
DB_CREATE_FILE_DEST="/home/oframe7/tbdata" -> match the directory CONTROL_FILES
#CERTIFICATE_FILE="/home/oframe7/tibero6/config/svr_wallet/oframe.crt"
#PRIVKEY_FILE="/home/oframe7/tibero6/config/svr_wallet/oframe.key"
#WALLET_FILE="/home/oframe7/tibero6/config/svr_wallet/WALLET"
#EVENT_TRACE_MAP="/home/oframe7/tibero6/config/event.map"
MAX_SESSION_COUNT=100
TOTAL_SHM_SIZE=2G
MEMORY_TARGET=3G 
THROW_WHEN_GETTING_OSSTAT_FAIL = N -> THIS IS IMPORTANT (network Kernel Parameters)
```
    tbboot nomount 
    
    tbsql sys/tibero
```bash
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
    tbboot
    
    sh $TB_HOME/scripts/system.sh 
    SYS password : tibero
    SYSCAT password : syscat
    
    tbsql tibero/tmax
```bash
create tablespace "DEFVOL" datafile 'DEFVOL.dbf' size 100M autoextend on;
create tablespace "TACF00" datafile 'TACF00.dbf' size 50M  autoextend on;
create tablespace "OFM_REPOSITORY" datafile 'OFM_REPOSITORY.dbf' size 50M  autoextend on;
create tablespace "OFMLOG" datafile 'OFM_LOG.dbf' size 300M  autoextend on next 300M;
create tablespace "OFMGR01" datafile 'OFMGR01.DBF'  size 100M autoextend on  next 50M;
```

2. UnixODBC Installation

- Copy make(usr/bin) file from the host to the container.
```bash
wget ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-2.3.4.tar.gz
tar -zxvf unixODBC-2.3.4.tar.gz
cd unixODBC-2.3.4
./configure --prefix=$HOME/unixODBC --sysconfdir=$HOME/unixODBC/etc
make
make install
```
    vi ~/.bash_profile
```bash
# UNIX ODBC ENV
export ODBC_HOME=$HOME/unixODBC
export PATH=$ODBC_HOME/bin:$PATH
export LD_LIBRARY_PATH=$ODBC_HOME/lib:$LD_LIBRARY_PATH
export ODBCINI=$HOME/unixODBC/etc/odbc.ini
export ODBCSYSINI=$HOME
```
    source ~/.bash_profile
    
    odbcinst -j
```bash
# UNIX ODBC ENV
export ODBC_HOME=$HOME/unixODBC
export PATH=$ODBC_HOME/bin:$PATH
export LD_LIBRARY_PATH=$ODBC_HOME/lib:$LD_LIBRARY_PATH
export ODBCINI=$HOME/unixODBC/etc/odbc.ini
export ODBCSYSINI=$HOME
```

3. OFCOBOL Installation

4. PROSORT Installation

5. Base Installation

6. Batch Installation

7. TACF Installation

8. OSC Installation

9. JEUS Installation

10. OFGW Installation

11. OFManager Installation


### 1.4 Create OpenFrame image

**Exit(stop) the container and commit the current container.**

```sudo docker ps -a | grep centos```

    CONTAINER ID  IMAGE    COMMAND      CREATED         STATUS          PORTS   NAMES  
    fc58fa646357  centos   "/bin/bash"  16 hours ago    Up 30 minutes            keen_poitras

``` sudo docker stop fc58fa646357 ```

``` sudo docker commit -a "kelsey" -m "of7azure" keen_poitras kelsey92/of7azurefinal:of7azure ```

**You need to name it with the rule below for pushung/pulling the image through Dockerhub.**

```bash
username/repository:tag
kelsey92/of7azurefinal:of7azure
```

``` sudo docker push kelsey92/of7azurefinal:of7azure ```

``` The push refers to repository [docker.io/kelsey92/of7azurefinal]```

### 1.5 Use OpenFrame image

**Docker should be installed and you need to login to Dockerhub.** 
[Install docker](#11-install-docker)

```bash
sudo docker login
dockerhub username
password
```

**Pull the image from the Dockerhub repository**

```sudo docker pull kelsey92/of7azurefinal:of7azure```

**Check if the image is successfully pulled in your VM**

```sudo docker images | grep kelsey```

**Run the container with OpenFrame image**

- Port forwarding with -p option when you run the container. Use multiple options with all ports you need.

```sudo docker run -i -t -h of7azure -p 9736:9736 -p 8088:8088 -p 8087:8087 kelsey92/of7azurefinal:of7azure```

#### If you run more than one containers.

- Docker container ip address changes if you run more than one containers.

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

**Those environment variables from bash_profile shoule be changed.**

    vi ~/.bash_profile
```bash
TMAX_HOST_ADDR=172.17.0.3
export TMAX_HOST_ADDR
alias msdown1='stopServer -u administrator -p tmax123 -host 172.17.0.3:9936'
alias msdown2='stopServer -u administrator -p tmax123 -host 172.17.0.3:9636'
alias dsdown='stopServer -u administrator -p tmax123 -host 172.17.0.3:9736'
```
    source ~/.bash_profile
    
**Region configuration file should be modified.**

    vi osc.OSCOIVP1.conf
```bash
[TDQ]
        TDQ_INTRA_DSNAME=OSC.TDQLIB.INTRA
        TDQ_LOG_ADDRESS=172.17.0.3:8896
```

**Webterminal setting should be modified**

    vi ofgw.properties
```bash
tmax.retrytime = 60000
#tmax.node.list = NODE1,NODE2
tmax.node.list = NODE1
tmax.node.NODE1.name = NODE1
tmax.node.NODE1.ip = 172.17.0.3
tmax.node.NODE1.port = 8001
tmax.node.NODE1.min = 5
tmax.node.NODE1.max = 1024
tmax.node.NODE1.rate = 2
tmax.node.NODE1.timeout = 20000
tmax.node.NODE1.idletime = 90
```

**OFManager setting should be modified**

    vi ofmanager.properties
```bash
# OFGW Property
openframe.webterminal.url = 172.17.0.3:5556/webterminal
openframe.webterminal.name= ofgw

# Tmax Property
openframe.tmax.ip= 172.17.0.3
openframe.tmax.port= 8001
```

**JEUS setting should be modified**

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
            <server-name>172.17.0.3</server-name>
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
            <server-name>172.17.0.3</server-name>
            <port-number>8629</port-number>
            <database-name>oframe</database-name>
            <user>tibero</user>
            <password>tmax</password>
```




## Step 2. Azure

### 2.1 Add Azure Kubernetes service(AKS)

```az aks get-credentials --resource-group [resource_group_name] --name [AKS_cluster_name]```

*Merged "AKSOF7azure" as current context in /home/kelsey/.kube/config*

```kubectl get nodes```
```bash
    NAME                       STATUS   ROLES   AGE    VERSION
    aks-agentpool-13644011-1   Ready    agent   115s   v1.15.10
```

### 2.2 Set the NAT inbound 

### 2.3 Set the pod

**Crate yaml file**

```bash
apiVersion: v1
kind: Pod
metadata:
  name: of7azure
  labels:
    of7azurefinal: of7azure
spec:
  containers:
  - name: of7azure
    image: kelsey92/of7azurefinal:of7azure
    ports:
      - containerPort: 9736
    command: ["/bin/sh", "-ec", "while :; do echo '.'; sleep 5 ; done"]
```







