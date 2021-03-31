# AKS automatic boot by Kelsey

## Table of Contents

+ [1. Concept](#concept)
+ [2. Process](#process)

## 1. Concept

### To make Tibero, OpenFrame, Jeus running in the first place when the pod is created.

## 2. Process

### A. Create Azure image repository.

### B. Generate the image of OpenFrame and push it to the Azure image repository.

*in this case, I created the image as the name below.*

```
of7container.azurecr.io/of7pvimage-v1:boot2
```

### C. Write the yaml file for creating a pod.


```
apiVersion: apps/v1
kind: Deployment
metadata:
  name: of7aks
  labels:
    run: of7aks
spec:
  replicas: 1
  selector:
    matchLabels:
      run: of7aks
  template:
    metadata:
      labels:
       run: of7aks
    spec:
      hostname: of7aks
      containers:
      - name: of7aks
        image: of7container.azurecr.io/of7pvimage-v1:boot2
        ports:
        - containerPort: 6606
        command: ["/bin/sh", "-ec", "while :; do echo '.'; sleep 5 ; done"]
        lifecycle:
          postStart:
           exec:
            command: ["/bin/bash", "-c", "/home/oftibr/new_of_boot.sh"]
        volumeMounts:
        - name: oframe
          mountPath: "/mnt/oframe"
        - name: oftibr
          mountPath: "/mnt/oftibr"
      volumes:
      - name: oframe
        persistentVolumeClaim:
             claimName: of7pvcoframe
      - name: oftibr
        persistentVolumeClaim:
             claimName: of7pvcoftibr
```

- Write a booting script that should be located under the designated path. (/home/oftibr in this case)

```
          postStart:
           exec:
            command: ["/bin/bash", "-c", "/home/oftibr/new_of_boot.sh"]
```        

- Use oftibr user for booting up Tibero, oframe user for booting up OpenFrame and Jeus.

```
#!/bin/bash

################
# ODBC
################

export ODBC_HOME="/opt/tmaxapp/unixODBC"
export PATH="$ODBC_HOME/bin:$PATH"
export LD_LIBRARY_PATH="$ODBC_HOME/lib:$LD_LIBRARY_PATH"
export ODBCINI="/opt/tmaxapp/unixODBC/etc/odbc.ini"
export ODBCSYSINI="/opt/tmaxapp/unixODBC"

################
# TMAX
################

export OPENFRAME_HOME="/opt/tmaxapp/OpenFrame"
export TMAX_HOST_ADDR="127.0.0.1"
export PATH="=$PATH:$HOME/.local/bin:$HOME/bin:$OPENFRAME_HOME/bin:$OPENFRAME_HOME/util"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/tmaxapp/OpenFrame/osc/region/OSCOIVP1/asmo:$OPENFRAME_HOME/lib"
export OPENFRAME_HOME="/opt/tmaxapp/OpenFrame"
export LD_LIBRARY_PATH_64="$LD_LIBRARY_PATH_64:$OPENFRAME_HOME/lib"
export TMAXDIR="/opt/tmaxapp/OpenFrame/core"
export TMAX_HOST_PORT="8001"
export PATH="$PATH:$TMAXDIR/bin"
export FDLFILE="$TMAXDIR/fdl/oframe.fdl"
export TDLDIR="TDLDIR=$TMAXDIR/tdl"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$TMAXDIR/lib"
export LD_LIBRARY_PATH_64="$LD_LIBRARY_PATH_64:$TMAXDIR/lib64"
export OFCOB_HOME="/opt/tmaxapp/OpenFrame/OFCOBOL"
export COBPARSER_HOME="$OFCOB_HOME/cobolparser"
export LLVM_HOME="$OFCOB_HOME/llvm"
export PATH="$PATH:$OFCOB_HOME/bin:$COBPARSER_HOME/bin:$LLVM_HOME/bin"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$OFCOB_HOME/lib:$COBPARSER_HOME/lib:$LLVM_HOME/lib"
export OFCOBCPY="$OFCOBCPY:$OFCOB_HOME/copybook"
export OFCOBCPY="$OFCOBCPY:$OPENFRAME_HOME/osc/copybook:$OPENFRAME_HOME/osc/region/OSCOIVP1/map/symbolic"
export OFPLI_INC="$OFPLI_INC:$OPENFRAME_HOME/osc/oivp/inc:$OPENFRAME_HOME/osc/region/OSCOIVP1/map/symbolic"
export TCACHECONF="$TMAXDIR/config/pfmtcache.cfg"
export PATH="$TB_HOME/script:$TB_HOME/bin:$TB_HOME/client/bin:$PATH"
export LD_LIBRARY_PATH="$TB_HOME/lib:$TB_HOME/client/lib:/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH_64="$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH_64"

###################
#TIBERO
###################

export TB_HOME="/opt/tmaxdb/tibero6"
export TB_SID="TVSAM"
#export SEM_KEY="148050"
export TB_PROF_DIR="$TB_HOME/bin/prof"
export PATH="$TB_HOME/script:$TB_HOME/bin:$TB_HOME/client/bin:$PATH"
export LD_LIBRARY_PATH="$TB_HOME/lib:$TB_HOME/client/lib:/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH_64="$TB_HOME/lib:$TB_HOME/client/lib:$LD_LIBRARY_PATH_64"
export TB_NLS_DATE_FORMAT="YYYY/MM/DD HH24:MI:SS"
export TBMON_HOME="$TB_HOME/tbmon"
export AIXTHREAD_SCOPE="S"
export TB_CONN_TIMEOUT="10"
export TB_READ_TIMEOUT="180"
export TCS_META_USER="tibero"
export TCS_META_PWD="tmax"

#################
# JEUS
#################

export JAVA_HOME="/usr/java/jdk1.7.0_79/"
export PATH="$JAVA_HOME/bin:$PATH"
export CLASSPATH="$CLASSPATH:$JAVA_HOME/jre/lib/ext:$JAVA_HOME/lib/tools.jar"
export ANT_HOME="/opt/tmaxui/ant"
export PATH="/opt/tmaxui/ant/bin:$PATH"
export JEUS_HOME="/opt/tmaxui/jeus7"
export PATH="$JEUS_HOME/bin:$PATH"
export JEUS_HOME=/opt/tmaxui/jeus7
export OFGW_HOME="/opt/tmaxui/jeus7/domains/jeus_domain/servers/server1/lib/application"
export OFMANAGER_HOME="/opt/tmaxui/jeus7/domains/jeus_domain/servers/server2/lib/application/ofmanager"
export OFMINER_HOME="/opt/tmaxui/ofminer"
export PROTRIEVE_HOME="/opt/tmaxapp/OpenFrame/protrieve"
export PATH="$PATH:$PROTRIEVE_HOME/bin"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$PROTRIEVE_HOME/lib"

main_proc()
{
  doTBclean;
  doTBboot;
  doOfClean;
  doOnlineBoot;
  doBatchBoot;
  doDsdown;
  doMsdown1;
  doMsdown2;
  doDsboot;
  doMsboot1;
  doMsboot2;
    exit 0;
}

doTBclean()
{

  su - oftibr -c '$TB_HOME/bin/tbdown clean'

}

doTBboot()
{

  su - oftibr -c '$TB_HOME/bin/tbboot'

}

doOfClean()
{
  su - oframe -c 'oscdown'

  su - oframe -c 'osctdlinit OSCOIVP1'

  su - oframe -c 'oscdown -r OSCOIVP1 -m'
}


doOnlineBoot()
{
  su - oframe -c 'oscboot -C'

  su - oframe -c 'oscboot -r OSCOIVP1'
}


doBatchBoot()
{
  su - oframe -c 'tjesmgr boot'
}


doDsdown()
{
  su - oframe -c 'stopServer -u administrator -p tmax1234 -host localhost:9736'
}


doMsdown1()
{
  su - oframe -c 'stopServer -u administrator -p tmax1234 -host localhost:9936'
}

doMsdown2()
{
  su - oframe -c 'stopServer -u administrator -p tmax1234 -host localhost:9636'
}

doDsboot()
{
  su - oframe -c 'startDomainAdminServer -domain jeus_domain -u administrator -p tmax1234'
}

doMsboot1()
{
  su - oframe -c 'startManagedServer -domain jeus_domain -server server1 -u administrator -p tmax1234'
}

doMsboot2()
{
  su - oframe -c 'startManagedServer -domain jeus_domain -server server2 -u administrator -p tmax1234'
}

main_proc $@;
```

# Copyrighted by Kelsey
