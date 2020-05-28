# Azure service with OpenFrame by Kelsey

## Table of Contents

+ [1. Create image of OpenFrame](#step-1-create-image-of-openframe)
  + [1.1 Install docker](#11-install-docker)
  + [1.2 Get centos container](#12-get-centos-container)
  + [1.3 Install OpenFrame](#13-install-openframe)
      + [1.3.1 Pre settings](#131-pre-settings)
      + [1.3.2 JAVA installation](#132-java-installation)
      + [1.3.3 Tibero installation](#133-tibero-installation)
+ [2. Azure Service](#step-2-azure-service)
  + [2.1 Add Azure Kubernetes service(AKS)](#21-add-azure-kubernetes-serviceaks)


## Step 1. Create image of OpenFrame

### 1.1 Install docker

Use thee containers(work as seperate VMs) for installing OSC, Batch and Tibero for each.



Hostname should be different for each, but username should be the same. (When you check the spool, username should be the same for checking it.)

Here are a few important things when you creat 

__a.__ Hostname should be different by the NODE.

TMAX configuration file 

```bash

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
*SERVER
################################################################################
# OpenFrame TACF  Servers                                                      #
# - TACF Mangement Server                                                      #
################################################################################
tmsvr		SVGNAME = svg_domain2,
			MIN = 1, MAX = 1,
			CLOPT="-o $(SVR)$(DATE).out -e $(SVR)$(DATE).err"
################################################################################
#   OpenFrame Online System Servers (OSC)                                      #
################################################################################
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
	
```
