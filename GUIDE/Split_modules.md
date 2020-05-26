# Azure service with OpenFrame by Kelsey

## Table of Contents

+ [1. Create image of OpenFrame](#step-1-create-image-of-openframe)
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
  + [1.4 Create OpenFrame image](#14-create-openframe-image)
  + [1.5 Use OpenFrame image](#15-use-openframe-image)
+ [2. Azure Service](#step-2-azure-service)
  + [2.1 Add Azure Kubernetes service(AKS)](#21-add-azure-kubernetes-serviceaks)
  + [2.2 Set Pods](#22-set-pods)
  + [2.3 Connect to the running Pod](#23-connect-to-the-running-pod)
  + [2.4 Set services](#23-set-services)
  + [2.5 Network configuration](#24-network-configuration)


## Step 1. Create image of OpenFrame

### 1.1 Install docker

First, you need to get the OpenFrame image to use the AKS service. To create it, you need Docker account and install Docker in your VM. Your account will be needed when you push/pull the images in your Dockerhub repository.

```bash
sudo apt-get update



Use two containers(work as seperate VMs) for installing OpenFrame for each.

Hostname should be different for each, but username should be the same. (When you check the spool, username should be the same for checking it.)


TMAX configuration file 

```bash
[of7azure@of7azure01 config]$ cat oframe.m 
################################################################################
#                                                                              #
#                    Openframe TMAX Configuration Sample                       #
#                    ===================================                       #
#                                                                              #
#      Copyright (c) 2006, 2007 Tmax Soft Co., Ltd. All rights reserved.       #
#                                                                              #
################################################################################
# At least following items should be configured to your system.                #
# - @HOSTNAME@ should be replaced with actual hostname.                        #
# - @TMAXDIR@ should be replaced with actual tmax directory.                   #
################################################################################
*DOMAIN
domain
    SHMKEY      = 80111,
    MAXUSER     = 256,
    MINCLH      = 1,
    MAXCLH      = 3,
    CPC         = 2,
    BLOCKTIME   = 60,
    MAXCPC      = 256,
    TXTIME      = 60,
    MAXSPR      = 512,
    MAXSVR      = 128,
    MAXSVC      = 2048,
    DOMAINID    = 4,
    IPCPERM     = 0777,
    TIPSVC      = TIPSVC,
    MAXSACALL   = 1024,
    MAXCACALL   = 1024

#
# HOSTNAME should be replaced with actual hostname.
#

*NODE
DEFAULT:
#    HOSTNAME = "of7azure01",
    DOMAINNAME = "domain"

# TMAXDIR should be replaced with actual tmax directory.
#

NODE1
    HOSTNAME = "of7azure01",
    TMAXDIR = "/home/of7azure/OpenFrame/core",
    APPDIR  = "/home/of7azure/OpenFrame/core/appbin",
    TLOGDIR = "/home/of7azure/OpenFrame/log/tmax/tlog",
    ULOGDIR = "/home/of7azure/OpenFrame/log/tmax/ulog",
    SLOGDIR = "/home/of7azure/OpenFrame/log/tmax/slog",
    CLHOPT  = " -o /home/of7azure/OpenFrame/log/tmax/clh.log -e /home/of7azure/OpenFrame/log/tmax/clh.err",
    TPORTNO = 8001, SHMKEY = 80111,
    TMAXPORT = "8001, 8050",
    CLLUNBLKPORT = "8050",
    CLLBLOCK = NO,
     RACPORT = 9450


NODE2
    HOSTNAME = "of7azure",
    TMAXDIR = "/home/of7azure/OpenFrame/core",
    APPDIR  = "/home/of7azure/OpenFrame/core/appbin",
    TLOGDIR = "/home/of7azure/OpenFrame/log/tmax/tlog",
    ULOGDIR = "/home/of7azure/OpenFrame/log/tmax/ulog",
    SLOGDIR = "/home/of7azure/OpenFrame/log/tmax/slog",
    CLHOPT  = " -o /home/of7azure/OpenFrame/log/tmax/clh.log -e /home/of7azure/OpenFrame/log/tmax/clh.err",
    TPORTNO = 8001, SHMKEY = 80111,
    TMAXPORT = "8001, 8050",
    CLLUNBLKPORT = "8050",
    CLLBLOCK = NO,
    RACPORT = 9450

*SVRGROUP
svg_domain
    NODENAME = "NODE1"
svg_domain2
    NODENAME = "NODE2"
svg_node1
    NODENAME = "NODE1",COUSIN = "svg_node2", LOAD = 0
svg_node2
    NODENAME = "NODE2"

################################################################################
# Sample Tmax Configuration for OpenFrame Base.                                #
#                                                                              #
# (OpenFrame Base product developer)                                           #
# - This file is controlled under CVS.                                         #
#   For local changes, edit Tmax config file in $TMAXDIR/config.               #
# - When you change Base servers or services,                                  #
#   you should change this file also in according to your changes.             #
################################################################################
*SERVER
################################################################################
# OpenFrame Base Servers                                                       #
# - TACF Server                 (UCS)                                          #
# - Lock Server                 (UCS)                                          #
# - Lock Worker                                                                #
# - Dataset Management Server   (UCS)                                          #
# - Dataset Edit Server         (CONV)                                         #
# - Console Server              (UCS)                                          #
# - User Interface Server                                                      #
# - System Management Logger                                                   #
# - VTAM server                                                                #
################################################################################
ofrsasvr    SVGNAME = svg_node1,   MIN = 1, MAX = 1, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
ofrlhsvr    SVGNAME = svg_node1,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
ofrdmsvr    SVGNAME = svg_node1,   MIN = 1, MAX = 1, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
ofrdsedt    SVGNAME = svg_node1,    CONV=Y,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
ofrcmsvr    SVGNAME = svg_node1,   MIN = 1, MAX = 1, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
ofruisvr    SVGNAME = svg_node1,    MIN = 2, MAX = 5,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
ofrsmlog    SVGNAME = svg_node1,	MIN	= 1, MAX = 1,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
vtammgr     SVGNAME = svg_node1, MIN = 1, MAX = 1, RESTART=NO, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
*SERVICE
################################################################################
#   ofrsasvr                                                                   #
################################################################################
SAFX_STAT_CLS   SVRNAME = ofrsasvr
SAFX_AUTH_CHK   SVRNAME = ofrsasvr
SAFX_TKN_CRE    SVRNAME = ofrsasvr
SAFX_TKN_CHG    SVRNAME = ofrsasvr
SAFX_TKN_DEL    SVRNAME = ofrsasvr
SAFX_GET_PASSWD SVRNAME = ofrsasvr
SAFX_GET_SACEE  SVRNAME = ofrsasvr
SAFX_ADD_GRP    SVRNAME = ofrsasvr
SAFX_DEL_GRP    SVRNAME = ofrsasvr
SAFX_ALT_GRP    SVRNAME = ofrsasvr
SAFX_LIST_GRP   SVRNAME = ofrsasvr
SAFX_GET_DAYS   SVRNAME = ofrsasvr
SAFX_PASSWORD   SVRNAME = ofrsasvr
SAFX_GET_CLASS  SVRNAME = ofrsasvr
SAFX_GET_CODE   SVRNAME = ofrsasvr
SAFX_GETGROUP   SVRNAME = ofrsasvr
SAFX_LISTGROUP  SVRNAME = ofrsasvr
SAFX_GETDSD     SVRNAME = ofrsasvr
SAFX_LISTDSD    SVRNAME = ofrsasvr
SAFX_ADDDSD     SVRNAME = ofrsasvr
SAFX_ALTDSD     SVRNAME = ofrsasvr
SAFX_DELDSD     SVRNAME = ofrsasvr
SAFX_GETPACC    SVRNAME = ofrsasvr
SAFX_LISTPACC   SVRNAME = ofrsasvr
SAFX_PERMIT     SVRNAME = ofrsasvr
SAFX_GETRLIST   SVRNAME = ofrsasvr
SAFX_RLIST      SVRNAME = ofrsasvr
SAFX_RDEFINE    SVRNAME = ofrsasvr
SAFX_RALTER     SVRNAME = ofrsasvr
SAFX_RDELETE    SVRNAME = ofrsasvr
SAFX_GETUSERS   SVRNAME = ofrsasvr
SAFX_LISTUSER   SVRNAME = ofrsasvr
SAFX_ADDUSER    SVRNAME = ofrsasvr
SAFX_ALTUSER    SVRNAME = ofrsasvr
SAFX_DELUSER    SVRNAME = ofrsasvr
SAFX_GETCO      SVRNAME = ofrsasvr
SAFX_LISTCO     SVRNAME = ofrsasvr
SAFX_CONNECT    SVRNAME = ofrsasvr
SAFX_REMOVE     SVRNAME = ofrsasvr
SAFX_ALTERCO    SVRNAME = ofrsasvr
SAFX_ALTER_PE   SVRNAME = ofrsasvr
SAFX_DELETE_PE  SVRNAME = ofrsasvr
SAFX_GET_UATTR  SVRNAME = ofrsasvr
################################################################################
#   ofrlhsvr                                                                   #
################################################################################
OFRLHALLOC      SVRNAME = ofrlhsvr
OFRDBMSLOCKLST  SVRNAME = ofrlhsvr
OFRDBMSLOCKLST2 SVRNAME = ofrlhsvr
################################################################################
#   ofrlmsvr                                                                   #
################################################################################
#OFRTKNCRE       SVRNAME = ofrlmsvr
#OFRREPLYCLI     SVRNAME = ofrlmsvr
#OFRLOCK         SVRNAME = ofrlmsvr
#OFRLOCKALL      SVRNAME = ofrlmsvr
#OFRTESTNLOCK    SVRNAME = ofrlmsvr
#OFRUNLOCK       SVRNAME = ofrlmsvr
#OFRLOCKCHECK    SVRNAME = ofrlmsvr
#OFRLOCKCHKJOBID SVRNAME = ofrlmsvr
#OFRLOCKCLEANUP  SVRNAME = ofrlmsvr
#OFRLOCKCLR      SVRNAME = ofrlmsvr
#OFRLOCKCLRJOBID SVRNAME = ofrlmsvr
#OFRLOCKCLRNODE  SVRNAME = ofrlmsvr
#OFRLOCKLST      SVRNAME = ofrlmsvr
#OFRLOCKREQLST   SVRNAME = ofrlmsvr
#OFRCLICLOSED    SVRNAME = ofrlmsvr
#OFRLOCKCHK      SVRNAME = ofrlmsvr
#OFRLOCKLST2     SVRNAME = ofrlmsvr
################################################################################
#   ofrlmwrk                                                                   #
################################################################################
#OFRLMWRKLOCK     SVRNAME = ofrlmwrk
#OFRLMWRKUNLOCK   SVRNAME = ofrlmwrk
#OFRLMWRKCLEAR    SVRNAME = ofrlmwrk
#OFRLMWRKCLEANUP  SVRNAME = ofrlmwrk
#OFRLMWRKLIST     SVRNAME = ofrlmwrk
#OFRLMWRKCHECK    SVRNAME = ofrlmwrk
#OFRLMWRKLOGCLNUP SVRNAME = ofrlmwrk
#OFRLMWRKDELTOK   SVRNAME = ofrlmwrk
################################################################################
#   ofrdsedt                                                                   #
################################################################################
OFRDSEDTDSVIEW  SVRNAME = ofrdsedt
################################################################################
#   ofrcmsvr                                                                   #
################################################################################
OFRCMSVRDISPLAY SVRNAME = ofrcmsvr
OFRCMSVRAREQ    SVRNAME = ofrcmsvr
OFRCMSVRCLRREQ  SVRNAME = ofrcmsvr
OFRCMSVRAREPLY  SVRNAME = ofrcmsvr
OFRCMSVRCREPLY  SVRNAME = ofrcmsvr
OFRCMSVRCONN    SVRNAME = ofrcmsvr
OFRCMSVRDISCONN SVRNAME = ofrcmsvr
OFRCMSVRALIST   SVRNAME = ofrcmsvr
OFRCMSVRPUTMSG  SVRNAME = ofrcmsvr
OFRCMSVRHISTORY SVRNAME = ofrcmsvr
OFRCMSVRCHECK   SVRNAME = ofrcmsvr
OFRCMSVRCOMMAND SVRNAME = ofrcmsvr
OFRCMSVRRETCMD  SVRNAME = ofrcmsvr
OFRCMSVRCRILIST SVRNAME = ofrcmsvr
OFRCMSVRCRIMSG  SVRNAME = ofrcmsvr
################################################################################
#   ofruisvr                                                                   #
################################################################################
OFRUISVRPRINTER SVRNAME = ofruisvr
OFRUISVRCONFIG  SVRNAME = ofruisvr
OFRUISVRTSAM    SVRNAME = ofruisvr
OFRUISVRERRMSG  SVRNAME = ofruisvr
OFRUISVRSPPATH  SVRNAME = ofruisvr
OFRUISVRDSPATH  SVRNAME = ofruisvr
OFRUISVRDSCRE   SVRNAME = ofruisvr
OFRUISVRDSDEL   SVRNAME = ofruisvr
OFRUISVRDSCOPY  SVRNAME = ofruisvr
OFRUISVRDSCOPY2 SVRNAME = ofruisvr
OFRUISVRDSKEY   SVRNAME = ofruisvr
OFRUISVRDSKEYREC  SVRNAME = ofruisvr
OFRUISVRDSKEYRAND SVRNAME = ofruisvr
OFRUISVRDSMOVE  SVRNAME = ofruisvr
OFRUISVRDSMOVE2 SVRNAME = ofruisvr
OFRUISVRDSLOAD  SVRNAME = ofruisvr
OFRUISVRDSSAVE  SVRNAME = ofruisvr
OFRUISVRDSEXP   SVRNAME = ofruisvr
OFRUISVRDSIMP   SVRNAME = ofruisvr
OFRUISVRDSTOUCH SVRNAME = ofruisvr
OFRUISVRGDGCRE  SVRNAME = ofruisvr
OFRUISVRGDGDEL  SVRNAME = ofruisvr
OFRUISVRPSVOL   SVRNAME = ofruisvr
OFRUISVRDSLIST  SVRNAME = ofruisvr
OFRUISVRPSDS2   SVRNAME = ofruisvr
OFRUISVRPSCAT2  SVRNAME = ofruisvr
OFRUISVRPSENT2  SVRNAME = ofruisvr
OFRUISVRPSCAT3  SVRNAME = ofruisvr
OFRUISVRSCHEMA  SVRNAME = ofruisvr
OFRUISVRRDFILE  SVRNAME = ofruisvr
OFRUISVRRDDIR   SVRNAME = ofruisvr
OFRUISVRWRJCL   SVRNAME = ofruisvr
OFRUISVRDLJCL   SVRNAME = ofruisvr
OFRUISVREDJCL   SVRNAME = ofruisvr
OFRUISVRDSVIEW  SVRNAME = ofruisvr
OFRUISVRPROFILE SVRNAME = ofruisvr
OFRUISVRCOMMAND SVRNAME = ofruisvr
OFRUISVRDEPLOY  SVRNAME = ofruisvr
OFRUISVRDSLISTALL  SVRNAME = ofruisvr
OFRUISVRWEBDECPY   SVRNAME = ofruisvr
OFRUISVRWEBDEVUE   SVRNAME = ofruisvr
OFRUISVRWEBDEEDT   SVRNAME = ofruisvr
OFRUISVRWEBDECNV   SVRNAME = ofruisvr
################################################################################
#   ofrsmlog                                                                   #
################################################################################
OFRSMLOGWRITE	SVRNAME = ofrsmlog
OFRSMLOGSTATUS	SVRNAME = ofrsmlog
OFRSMLOGCONTROL	SVRNAME = ofrsmlog
################################################################################
#   VTAM                                                                       #
################################################################################
VTAMMGRSVC	SVRNAME = vtammgr
VTAMMGRRELAY	SVRNAME = vtammgr
##################### TJES Configuration Start #################################
# Sample Tmax Configuration for OpenFrame TJES.                                #
#                                                                              #
# (OpenFrame TJES product developer)                                           #
# - This file is controlled under CVS.                                         #
#   For local changes, edit Tmax config file in $TMAXDIR/config.               #
# - When you change TJES servers or services,                                  #
#   you should change this file also in according to your changes.             #
################################################################################
*SERVER
################################################################################
# OpenFrame TJES Servers                                                       #
# - Job Mangement Server                                                       #
# - Job Scheduling Server       (UCS)                                          #
# - Job Initiator Server        (UCS)                                          #
# - Job History Server                                                         #
# - Spool Backup Server         (UCS)                                          #
# - Print Management Server     (UCS)                                          #
################################################################################
obmjmsvr    SVGNAME = svg_domain2,        MIN = 10, MAX = 10,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
obmjschd    SVGNAME = svg_domain2,
            MIN = 1, MAX = 1, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
obmjinit    SVGNAME = svg_domain2,
            MIN = 1, MAX = 1, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
obmjhist    SVGNAME = svg_domain2,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
obmjspbk	SVGNAME = svg_domain2,   MIN = 1, MAX = 1, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
ofrpmsvr    SVGNAME = svg_domain2,   MIN = 1, MAX = 1, SVRTYPE=UCS,
            CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
obmtsmgr    SVGNAME = svg_domain2,   MIN = 1, MAX = 1, SVRTYPE=UCS,
			CLOPT="-o $(SVR)$(DATE).out -e $(SVR)$(DATE).err"
obmjtimr    SVGNAME = svg_domain2,   MIN = 1, MAX = 1, SVRTYPE=UCS,
			CLOPT="-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
*SERVICE
################################################################################
#   obmjmsvr                                                                   #
################################################################################
OBMJMSVRPO      SVRNAME = obmjmsvr
OBMJMSVRPOJOB   SVRNAME = obmjmsvr
OBMJMSVRPOJST   SVRNAME = obmjmsvr
OBMJMSVRPOLIST  SVRNAME = obmjmsvr
OBMJMSVRCHCLS   SVRNAME = obmjmsvr
OBMJMSVRENQUEUE SVRNAME = obmjmsvr
OBMJMSVRCICSENQ SVRNAME = obmjmsvr
OBMJMSVRPURGE   SVRNAME = obmjmsvr
OBMJMSVRRELEASE SVRNAME = obmjmsvr
OBMJMSVRJSUBMIT SVRNAME = obmjmsvr
OBMJMSVRJREMOVE SVRNAME = obmjmsvr
OBMJMSVRJLIST   SVRNAME = obmjmsvr
OBMJMSVRJLIST2  SVRNAME = obmjmsvr
OBMJMSVRJDETAIL SVRNAME = obmjmsvr
OBMJMSVRJSPOOL  SVRNAME = obmjmsvr
OBMJMSVRJCLSST	SVRNAME = obmjmsvr
OBMJMSVRJRELAY  SVRNAME = obmjmsvr
OBMJMSVROUTCLASS SVRNAME = obmjmsvr
OBMJMSVROUTQERR SVRNAME = obmjmsvr
OBMJMSVRIRDRENQ SVRNAME = obmjmsvr
OBMJMSVRUSERSPL SVRNAME = obmjmsvr
################################################################################
#   obmjschd                                                                   #
################################################################################
OBMJSCHDBOOT    SVRNAME = obmjschd
OBMJSCHDDOWN    SVRNAME = obmjschd
OBMJSCHDSTART	SVRNAME = obmjschd
OBMJSCHDSTOP	SVRNAME = obmjschd
OBMJSCHDJOBID   SVRNAME = obmjschd
OBMJSCHDSUBMIT	SVRNAME = obmjschd
OBMJSCHDCANCEL	SVRNAME = obmjschd
OBMJSCHDCHANGE	SVRNAME = obmjschd
OBMJSCHDNODEST  SVRNAME = obmjschd
OBMJSCHDREQSCHD SVRNAME = obmjschd
OBMJSCHDOUTID   SVRNAME = obmjschd
OBMJSCHDCLASS   SVRNAME = obmjschd
OBMJSCHDNODECLR SVRNAME = obmjschd
OBMJSCHDJOBGREG SVRNAME = obmjschd
OBMJSCHDJOBGPS  SVRNAME = obmjschd
OBMJSCHDJOBGCTL SVRNAME = obmjschd
OBMJSCHDJOBGDEL SVRNAME = obmjschd
OBMJSCHDOUTQINFO SVRNAME = obmjschd
################################################################################
#   obmjinit                                                                   #
################################################################################
OBMJINITCHECK	SVRNAME = obmjinit
OBMJINITCTLNODE	SVRNAME = obmjinit
OBMJINITCTLJOB	SVRNAME = obmjinit
OBMJINITCTLINIT	SVRNAME = obmjinit 
OBMJINITRUN	    SVRNAME = obmjinit
OBMJINITSTATUS	SVRNAME = obmjinit
################################################################################
#   obmjhist                                                                   #
################################################################################
OBMJHISTJOBLOG	SVRNAME = obmjhist
################################################################################
#   obmjspbk                                                                   #
################################################################################
OBMJSPBKBACKUP	SVRNAME = obmjspbk
OBMJSPBKLIST	SVRNAME = obmjspbk
OBMJSPBKRESTORE	SVRNAME = obmjspbk
OBMJSPBKDETAIL	SVRNAME = obmjspbk
OBMJSPBKCLEAN	SVRNAME = obmjspbk
OBMJSPBKSPOOL	SVRNAME = obmjspbk
OBMJSPBKSTOP    SVRNAME = obmjspbk
OBMJSPBKSTAT    SVRNAME = obmjspbk
################################################################################
#   obmtsmgr                                                                   #
################################################################################
TSO                 SVRNAME = obmtsmgr
OBMTSMGRREADDATA    SVRNAME = obmtsmgr
OBMTSMGRREADPSAM    SVRNAME = obmtsmgr
OBMTSMGRREADDATA2   SVRNAME = obmtsmgr
OBMTSMGRCHECK       SVRNAME = obmtsmgr
OBMTSMGRLIST        SVRNAME = obmtsmgr
################################################################################
#   obmjtimr                                                                   #
################################################################################
OBMJTIMRREG		SVRNAME = obmjtimr
OBMJTIMRDEREG	SVRNAME = obmjtimr
###################### TJES Configuration End ##################################
#################### TACF Configuration Start ##################################
# Sample Tmax Configuration for OpenFrame TACF.                                #
#                                                                              #
# (OpenFrame TACF product developer)                                           #
# - This file is controlled under CVS.                                         #
#   For local changes, edit Tmax config file in $TMAXDIR/config.               #
# - When you change TACF servers or services,                                  #
#   you should change this file also in according to your changes.             #
################################################################################
*SERVER
################################################################################
# OpenFrame TACF  Servers                                                      #
# - TACF Mangement Server                                                      #
################################################################################
tmsvr		SVGNAME = svg_domain2,
			MIN = 1, MAX = 1,
			CLOPT="-o $(SVR)$(DATE).out -e $(SVR)$(DATE).err"

*SERVICE
################################################################################
#   tmsvr                                                                      #
################################################################################
TMADDGRP    	SVRNAME = tmsvr 
TMDELGRP    	SVRNAME = tmsvr 
TMALTGRP    	SVRNAME = tmsvr 
TMGETCLASS      SVRNAME = tmsvr 
TMGETCODE   	SVRNAME = tmsvr 
TMGETGROUP   	SVRNAME = tmsvr 
TMLISTGROUP  	SVRNAME = tmsvr 
TMGETDSD     	SVRNAME = tmsvr 
TMLISTDSD    	SVRNAME = tmsvr 
TMADDDSD     	SVRNAME = tmsvr 
TMALTDSD     	SVRNAME = tmsvr 
TMDELDSD     	SVRNAME = tmsvr 
TMGETPACC    	SVRNAME = tmsvr 
TMLISTPACC   	SVRNAME = tmsvr 
TMPERMIT     	SVRNAME = tmsvr 
TMALTERPE   	SVRNAME = tmsvr 
TMDELETEPE  	SVRNAME = tmsvr 
TMGETRLIST   	SVRNAME = tmsvr 
TMRLIST      	SVRNAME = tmsvr 
TMRDEFINE    	SVRNAME = tmsvr 
TMRALTER     	SVRNAME = tmsvr 
TMRDELETE    	SVRNAME = tmsvr 
TMGETUSERS   	SVRNAME = tmsvr 
TMLISTUSER   	SVRNAME = tmsvr 
TMADDUSER    	SVRNAME = tmsvr 
TMALTUSER    	SVRNAME = tmsvr 
TMDELUSER    	SVRNAME = tmsvr 
TMGETCO      	SVRNAME = tmsvr 
TMLISTCO     	SVRNAME = tmsvr 
TMCONNECT    	SVRNAME = tmsvr 
TMREMOVE     	SVRNAME = tmsvr 
TMALTERCO    	SVRNAME = tmsvr 
###################### TACF Configuration End ##################################
###### OSC Configuration Start  ################################################
#                                                                              #
#                OpenFrame OSC Configuration File for Tmax System              #
#               ==================================================             #
#                                                                              #
#               Copyright(c) 2015 TmaxSoft Inc. All rights reserved            #
#                                                                              #
################################################################################
        
*SVRGROUP
svgotpn         NODENAME = "NODE1"
#svgotpb         NODENAME = "NODE1"
#svgbiz          NODENAME = "NODE1"


*SERVER

#
# OSC Servers
#


################################################################################
#   OpenFrame Online System Servers (OSC)                                      #
################################################################################

######## OSC svgotp: MIN/MAX user-defined

######## OSC svgotpu: Unique for domain


######## OSC svgotpn: Unique for node
oscmgr           SVGNAME = svg_domain, MAX = 1, SVRTYPE = UCS,
                 CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
oscmcsvr         SVGNAME = svg_domain,
                 CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
oscmnsvr         SVGNAME = svg_domain, MAX = 1, SVRTYPE = UCS,
                 CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"

######## OSC svgotpb: BACKUP setting possible

oscncsvr         SVGNAME = svg_domain, MAX = 1, SVRTYPE = UCS,
                 CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
oscscsvr         SVGNAME = svg_domain, MAX = 1, SVRTYPE = UCS,
                 CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
oscdfsvr         SVGNAME = svg_domain, MAX = 1, SVRTYPE = UCS,
                 CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
oscjcsvr         SVGNAME = svg_domain, MAX = 1, SVRTYPE = UCS,
                 CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"


################################################################################
#   OpenFrame OSC User Application                                             #
################################################################################
OSCOIVP1        SVGNAME = svg_domain,
                MIN = 3, 
                MAX = 10, 
                SCHEDULE = FA,
                CLOPT = "-n -o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"

OSCOIVP1C       SVGNAME = svg_domain,
                TARGET = OSCOIVP1,
                CONV = O,
                MAX = 1, 
                SCHEDULE = FA,
                CLOPT = "-n -o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"

OSCOIVP1OMC     SVGNAME = svg_domain,
                TARGET = oscossvr,
                MIN = 1, 
                MAX = 5, 
                SCHEDULE = FA,
                CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err -x OSCOSSVRSVC1:OSCOIVP1_OMC1,OSCOSSVRSVC2:OSCOIVP1_OMC2,OSCOSSVRMON:OSCOIVP1_MON,OSCOSSVR_ST:OSCOIVP1_ST"

OSCOIVP1TL      SVGNAME = svg_domain, MAX = 1, SVRTYPE = UCS, target=osctlsvr,
                CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err -x OSCTLSVRSVC:OSCOIVP1_TL"

OSCOIVP1_TCL1   SVGNAME= svg_domain, 
                TARGET = OSCOIVP1,
                MIN = 1,
                CLOPT = "-n -o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"


*SERVICE
################################################################################
#   oscmgr                                                                     #
################################################################################
OSCMGRSVC               SVRNAME = oscmgr
OSCMGRREGLIST           SVRNAME = oscmgr
OSCMGRTERMLIST          SVRNAME = oscmgr
OSCMGRDISCONN           SVRNAME = oscmgr
OSCMGRHOURGLASS         SVRNAME = oscmgr 

################################################################################
#   oscmcsvr                                                                   #
################################################################################
OSCMCSVRSMFW     SVRNAME = oscmcsvr

################################################################################
#   oscscsvr                                                                   #
################################################################################
OSCSCSVRSTART   SVRNAME = oscscsvr
OSCSCSVRDELAY   SVRNAME = oscscsvr
OSCSCSVRCANCEL  SVRNAME = oscscsvr
OSCSCSVRINFO    SVRNAME = oscscsvr
OSCSCSVRCONTROL SVRNAME = oscscsvr

################################################################################
#   oscncsvr                                                                   #
################################################################################
OSCNCSVRDEFINE  SVRNAME = oscncsvr
OSCNCSVRDELETE  SVRNAME = oscncsvr
OSCNCSVRGET     SVRNAME = oscncsvr
OSCNCSVRQUERY   SVRNAME = oscncsvr
OSCNCSVRREWIND  SVRNAME = oscncsvr
OSCNCSVRUPDATE  SVRNAME = oscncsvr
OSCNCSVRBROWSE  SVRNAME = oscncsvr

################################################################################
#   oscdfsvr                                                                   #
################################################################################
OSCDFSVRBRKE    SVRNAME = oscdfsvr
OSCDFSVRBRIN    SVRNAME = oscdfsvr
OSCDFSVRCHCK    SVRNAME = oscdfsvr
OSCDFSVRRESP    SVRNAME = oscdfsvr
OSCDFSVRREIN    SVRNAME = oscdfsvr
OSCDFSVRSETF    SVRNAME = oscdfsvr

################################################################################
#   oscjcsvr                                                                   #
################################################################################
OSCJCSVRFLUSH           SVRNAME = oscjcsvr
OSCJCSVRWRITE           SVRNAME = oscjcsvr

################################################################################
#   OSC USER APPLICATION SERVER DEFAULT                                        #
################################################################################
OSCOIVP1P               SVRNAME = OSCOIVP1
OSCOIVP1                SVRNAME = OSCOIVP1
OSCOIVP1M               SVRNAME = OSCOIVP1C
OSCOIVP1C               SVRNAME = OSCOIVP1C
OSCOIVP1_TL             SVRNAME = OSCOIVP1TL
OSCOIVP1_OMC1           SVRNAME = OSCOIVP1OMC
OSCOIVP1_OMC2           SVRNAME = OSCOIVP1OMC
OSCOIVP1_MON            SVRNAME = OSCOIVP1OMC
OSCOIVP1_ST             SVRNAME = OSCOIVP1OMC

################################################################################
#   OSC USER APPLICATION TRANCLASS SERVER                                      #
################################################################################
OSCOIVP1_TCL1            SVRNAME=OSCOIVP1_TCL1

*SVRGROUP
OPFMGRP01               NODENAME = NODE1

*SERVER
TPFMAGENT               SVGNAME = OPFMGRP01, MIN = 1, MAX = 1,
                        CLOPT = "-o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
*SERVICE
SPFMAGENT               SVRNAME = TPFMAGENT

 
################################################################################
#   OpenFrame Online XA Sample                                                 #
################################################################################	
#*SVRGROUP
#svgtboiv0      
#	 NODENAME = "NODE1",
#        TMSNAME = "tms_tbr", TMSTYPE = STD,
#        TMSOPT = "-o tms_tbr.out -e tms_tbr.err",
#        TMSRECOVERY = N, TMSRANGE = DOMAIN, DBNAME = TIBERO,
#        RESTART = Y,
#        OPENINFO = "TIBERO_XA:user=tibero,pwd=tmax,sestm=60,db=TVSAM",
#        SVGTYPE = STMAX,
#        RMID = 0
#svgtboiv1       
#	 NODENAME = "NODE1",
#        TMSNAME = "tms_tbr", TMSTYPE = STD,
#        TMSOPT = "-o tms_tbr.out -e tms_tbr.err",
#        TMSRECOVERY = N, TMSRANGE = DOMAIN, DBNAME = TIBERO,
#        RESTART = Y,
#        OPENINFO = "TIBERO_XA:user=tibero,pwd=tmax,sestm=60,db=TVSAM,conn_id=TVSAM",
#        SVGTYPE = STMAX,
#        RMID = 1
#
#svgtboiv  
#	 NODENAME = "NODE1",
#        SVGLIST="svgtboiv0,svgtboiv1",
#        SVGTYPE = MTMAX
#*SERVER
#OSCOIVP1        SVGNAME = svgtboiv,,,
#OSCOIVP1C       SVGNAME = svgtboiv,,,
#OSCOIVP1_TCL1   SVGNAME= svgtboiv,

################################################################################
#                  OpenFrame OSC Configuration END                             #
#                 =================================                            #
###### OSC Configuration End ###################################################
```bash


