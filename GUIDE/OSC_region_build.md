# OSC Region Build Guide

Follow this guide to build a new OSC Region.

## Table of Contents

1. [Check Resources of SD Macro File](#Check-the-resources-of-the-region-from-the-sd-macro-file)
2. [Build the region and configure it in TMAX configuration](#build-the-region-and-configure-it-in-tmax-configuration)
3. [Generate Online System VSAM Files TDQ, TSQ, SD for the Region](#generate-online-system-vsam-files-tdq-tsq-sd-for-the-region)
4. [Create and edit Online Region Configuration Files](#create-and-edit-online-region-configuration-files-copy---rename---edit)
5. [Create MAPDIR, TBLDIR, TDLDIR Directory for the Region](#create-mapdir-tbldir-tdldir-directory-for-the-region)
6. [Modify the TDL Configuration File for the Region](#modify-the-tdl-configuration-file-for-the-region)
7. [Create the Region Memory](#create-the-region-memory)
8. [Add the Region to the Region List for Booting the Region when using oscboot](#add-the-region-to-the-region-list-for-booting-the-region-up-when-using-oscboot)
9. [Reboot OpenFrame to Verify](#reboot-openframe-to-make-the-region-server-setting-effective-and-start-the-region-server)

## Steps

**Firstly:** Customer should provide a System Definition (SD) Macro File for each region.

### Check the Resources of the Region from the SD Macro File

1. Check which tranclasses are used for each region.

```
DEFINE TRANCLASS(**transaction_id**)
	   GROUP(resourcegroup_name)
	   MAXACTIVE(number)
	   PURGETHRESH(NO | number)
```

2. Check which group lists are used for each region.

```
ADD GROUP (resourcegroup_name01) LIST(grouplist)
ADD GROUP (resourcegroup_name02) LIST(grouplist)
ADD GROUP (resourcegroup_name03) LIST(grouplist)
```

### Build the region and configure it in TMAX configuration

1. Build the region with oscbuild tool. (Options may vary)

```bash
oscbuild -o LINUX64 -d TIBERO -s region_name -b OFCOBOL
```

Above, _LINUX64_ describes the Operating System Architecture, _TIBERO_ describes the Database, _region\_name_ describes the name of the OSC region we are trying to build, and _OFCOBOL_ describes the type of COBOL Compiler you are using.

You can issue ```oscbuild``` command without any parameters to see the usage.

2. Deploy the region server module to APPDIR directory. (from oframe.m)

```bash
cp -a ${region_name} ${TMAXDIR}/appbin/
```

**Note:** The APPDIR noted above refers to ${OPENFRAME_HOME}/core/appbin

3. Set the tmax configuration for the region server. (${TMAXDIR}/config)

Each option should be configured (Refer to 3.2.4 SERVER from TMAX_5_SP2_FIX#1_Administrator_s_Guide_v2.1.5.1_en) For the specific setting it needs to be discussed with the customer.

**Use the server setting from the default region (OSCOIVP1)**. (copy-> rename -> edit)

Example.)

**Note: region_name should be substituted for the actual Region Name**

<pre>
region_name 			SVGNAME = svgbiz,
						MIN = 3,
						MAX = 10,
						SCHEDULE = FA,
						CLOPT = "-n -o $(SVR)_$(CDATE).out -e $(SVR)_$(CDATE).err"
region_nameC 			SVGNAME = svgbiz,
						TARGET = region_name,
						CONV = O,
						MIN = 1,
						MAX = 128,
						SCHEDULE = FA,
						CLOPT = "-n -o $(SVR)_$(CDATE).out -e 	$(SVR)_$(CDATE).err"
region_nameOMC 			SVGNAME = svgbiz,
						TARGET = oscossvr,
						MIN = 1,
						MAX = 5,
						SCHEDULE = FA,
						CLOPT = "-o $(SVR)_$(CDATE).out -e $(SVR)_$(CDATE).err -x OSCOSSVRSVC1:region_name_OMC1,OSCOSSVRSVC2:region_name_OMC2,OSCOSSVRMON:region_name_MON,OSCOSSVR_ST:region_name_ST" <- This should match the one from default region.
region_nameTL 			SVGNAME = svgbiz, MAX = 1, SVRTYPE = UCS, target=osctlsvr,
						CLOPT = "-o $(SVR)_$(CDATE).out -e $(SVR)_$(CDATE).err -x OSCTLSVRSVC:region_name_TL"
						region_name_tranclass SVGNAME= svgbiz,
						TARGET = region_name
						MIN = 1,
						CLOPT = "-n -o $(SVR)$(CDATE).out -e $(SVR)$(CDATE).err"
</pre>

<pre>
region_name 			SVRNAME = region_name
region_nameP 			SVRNAME = region_name
region_nameC 			SVRNAME = region_nameC
region_nameM 			SVRNAME = region_nameC
region_name_TL 			SVRNAME = region_nameTL
region_name_OMC1 		SVRNAME = region_nameOMC
region_name_OMC2 		SVRNAME = region_nameOMC
region_name_MON 		SVRNAME = region_nameOMC
region_name_ST 			SVRNAME = region_nameOMC
region_name_tranclass 	SVRNAME = region_name_tranclass
</pre>

4. Compile the tmax Configuration file.

<pre>
	cfl -i oframe.m
</pre>

### Generate online system VSAM files (TDQ, TSQ, SD) for the region

1. Generate the SD Dataset

<pre>
	idcams define -t CL -n ${SD_Dataset_Name} -o KS -k 18,0 -b 32768 -l 128,32760 -s 1024,128,128 -v DEFVOL
</pre>

2. Generate TDQ Dataset

<pre>
	idcams define -t CL -n ${TDQ_Dataset_Name} -o KS -k 8,0 -l 128,32760 -b 32767 -s 1024,128,128 -v DEFVOL
</pre>

3. Generate TSQ (KEY and DATA) dataset.

<pre>
	idcams define -t CL -n ${TSQ_KEY_Dataset_Name} -o KS -k 16,0 -l 64,64 -s 1024,128,128 -v DEFVOL

	idcams define -t CL -n ${TSQ_DATA_Dataset_Name} -o KS -k 18,0 -l 128,32760 -b 32767 -s 1024,128,128 -v DEFVOL
</pre>

4. Register the CSD resource definition to SD dataset.

Register system online resource

<pre>
	oscsdgen -c -d ${SD_Dataset_Name} ${OPENFRAME_HOME}/osc/resource/osc.dat
</pre>

Register User CSD

<pre>
	oscsdgen -c -d ${SD_Dataset_Name} ${User_Resource_File}
</pre>

<pre>
oscsdgen version 7.0.3(10) obuild@tplinux64:ofsrc7/osc(#1) 2017-11-29 20:51:10
OSC System Definition(OSCSD) Update Utility
	Usage: oscsdgen -c {-r <region>|-d <dataset>} <file>
| oscsdgen -f {-r <region>|-d <dataset>} <group_name>:<resource_name>:<resource_type>
| oscsdgen [options]
<file> Specify input file
-c 					Gererate SD
-f 					Delete SD resource specified
-d <dataset> 		Specify OSCSD dataset name
-r <region> 		Specify OSC region name
<group_name> 		SD GROUP name of specified resource
<resource_name> 	SD resource name
<resource_type> 	SD type of specified resource

{connection|file|journalmodel|pipeline|program|tdq|terminal|transaction|tsmodel|typeterm|webservice|tranclass|
enqmodel|lsrpool|mapset|partitionset|profile|sessions|tcpipservice|urimap|library}

Options:
-h 					Display this information
-v 					Display version information
</pre>

### Create and Edit Online Region Configuration Files. (Copy -> Rename -> Edit)

1. Copy the default region (OSCOIVP1) Configuration files and rename it. ${OPENFRAME_HOME}/config

osc.${Region_Name}.conf & osc.${Region_Name}TL.conf

<pre>
	cp osc.OSCOIVP1.conf osc.${Region_Name}.conf
	cp osc.OSCOIVP1TL.conf osc.${Region_Name}TL.conf
</pre>

2. Edit the region configuration file.

**_YOU NEED TO MAKE SURE THE SHARED MEMORY KEY AND PORT SETTINGS ARE DIFFERENT FOR EACH REGION_**

osc.${Region_Name}.conf

<pre>
[GENERAL]
	JOBID=STD00001
	SYS_SHMKEY=70454 				<- system shared memory key
	USR_SHMKEY=70554 				<- user shared memory key
	TC_PATH=$OPENFRAME_HOME/temp/region_name_TC
	MAPDIR=$OPENFRAME_HOME/osc/region/region_name/map <- check $OPENFRAME_HOME path
	TBLDIR=$OPENFRAME_HOME/osc/region/region_name/tbl <- check $OPENFRAME_HOME path
	TDLDIR=$OPENFRAME_HOME/osc/region/region_name/tdl <- check $OPENFRAME_HOME path
[SD]
	DSNAME=SD dataset name
	GRPLIST=BASELIST,CICBLIST
	[TDQ]
	TDQ_INTRA_DSNAME=TDQ dataset name
	TDQ_LOG_ADDRESS=127.0.0.1:5954 	<- TDQ log address port
[TSQ]
	QDATA_DSNAME=TSQ DATA dataset name
	QINFO_DSNAME=TSQ KEY dataset name
	TSQ_SHMKEY=70654
[TRANCLASS]
	DEFAULT_TRANCLASS=DFHTCL00 		<- default tranclass * check the default tranclass for the region.
</pre>

osc.${Region_Name}TL.conf

<pre>
[OSCTLSVR]
	PORT=5954		<- TL server port (Use TDQ_LOG_ADDRESS port)
</pre>

### Create MAPDIR, TBLDIR, TDLDIR directory for the region

You can copy the default MAPDIR, TBLDIR, TDLDIR directories

<pre>
	cp -a ${OPENFRAME_HOME}/osc/region/OSCOIVP1 ${OPENFRAME_HOME}/osc/region/${Region_Name}
</pre>

### Modify the TDL configuration file for the region

${OPENFRAME_HOME}/osc/region/${Region_Name}/tdl/config/tdl.cfg

<pre>
	#  shared memory key
	SHMKEY=70740					<- It should be different from any other region
</pre>

### Create the Region Memory

Use osctdlinit tool.

<pre>
	osctdlinit ${Region_Name}
</pre>

### Add the region to the region list for booting the region up when using oscboot

${OPENFRAME_HOME}/config/osc.region.list

<pre>
	${Region_Name}
</pre>

### Reboot OpenFrame to make the region server setting effective and start the region server.

<pre>
	oscdown						<- OpenFrame server down
	oscboot						<- OpenFrame server boot
</pre>
