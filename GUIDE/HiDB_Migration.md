# How to Migrate IMS DB Database to HiDB

In this guide, we will go over how to migrate an IMS DB Database to HiDB (OpenFrame High performance Hierarchical Database). If you want a shortened version, and already have a good idea of how this works, I recommend just skipping to each full example section. Otherwise, please read through each step carefully. Depending on the access type of the DBD, the steps may vary. For example, hdpcdf01 step can be skipped for anything except a DEDB. 

# Table of Contents

- [HIDBINIT](#step-1-hidbinit)
	* [hidbinit usage](#11-usage)
	* [hidbinit full example](#12-full-example)
- [DBDGEN](#step-2-dbdgen)
	* [dbdgen usage](#21-usage)
	* [dbdgen full example](#22-full-example)
- [HDGENSCH](#step-3-hdgensch)
	* [hdgensch usage](#31-usage)
	* [hdgensch full example](#32-full-example)
- [HIDBMGR](#step-4-hidbmgr)
	* [hidbmgr usage](#41-usage)
	* [hidbmgr full example](#42-full-example)
- [COMPILE](#step-5-compile)
	* [compile full example](#51-full-example)
- [HDPCDF01](#step-6-hdpcdf01)
	* [hdpcdf01 usage](#61-usage)
	* [hdpcdf01 full example](#62-full-example)
- [DSMIGIN](#step-7-dsmigin)
	* [dsmigin full example](#71-full-example)
- [HDLOAD](#step-8-hdload)
	* [hdload usage](#81-usage)
	* [hdload full example](#82-full-example)
	
## Step 1. HIDBINIT

To get started with OpenFrame HiDB, you need to run this tool before any other HiDB tool. This creates the 	meta tables required to operate the database.
	
### 1.1 Usage

```hidbinit <command> <connection> [options]```
	
commands: 

| COMMAND  | DESCRIPTION                           |
|----------|---------------------------------------|
| create   | creates a meta table                  |
| drop     | deletes a meta table                  |
| truncate | removes all meta data from meta table |

**connection** - Name of the section for your database login in the 	ofsys.conf file
			Example: TVSAM

**options**

| OPTION          | DESCRIPTION                                                                                                                |
|-----------------|----------------------------------------------------------------------------------------------------------------------------|
| -t <tablespace> | Name of a tablespace. The name is used only when you are creating a meta table  (Required for Tibero and Oracle Databases) |


### 1.2 Full Example 

```hidbinit create TVSAM -t DEFVOL```

## Step 2. DBDGEN

Used to define the OpenFrame HiDB Database Schema. You can execute this command from command line, without running it as a JOB. This tool stores the database schema information from the DBD assembler macro in meta tables. 

**To use this command, you need to have done the hidbinit 	command first!**

This step needs to be done for a Physical DBD First, then any logical children next.
	
It is recommended that you store all DBDs in one directory. Once all the DBDs are in one directory, you can create subdirectories under for Logical Childs and Secondary Indexes to more easily understand the underlying structure of the database.

### 2.1 Usage 

```dbdgen [options] <filename>```

**options**

| OPTION | DESCRIPTION                                                         |
|--------|---------------------------------------------------------------------|
| -f     | Deletes the existing DBD meta data and objects and creates new ones |

**filename** - Name of the file that contains the DBD control statements (i.e. the DBD assembler macro). When there are multiple files to process, enter the file names one after another.

### 2.2 Full Example 

```dbdgen -f ${dbdname}```

## Step 3. HDGENSCH

Performs two main tasks:

1. ```hdgensch meta``` is for creating a Database Table. 
2. ```hdgensch schema``` is for creating a .conv file which is required for ```dsmigin```

 You can create schema files with cobgensch or pligensch tool and use tham as input to the data set migration tool dsmigin. In OpenFrame HiDB however, you must use the hdgensch tool to create schema files. This is because general data sets use one-to-one mapping between a segment and copybook while there can be one-to-many mappings between a segment and copybooks in OpenFrame HiDB. hdgensch internally calls cobgensch or pligensch to process copybooks for each segment and then mergest them into a single schema file. It also saves the cobol layout, which is used to create the tables and DML files for each segment, in the OFM_HIDB_DBD_COLUMN meta table.

 After a scucessful run of hdgensch, a schema file is created with the DBD name and '.conv' extension and saved under the default schema directory set in the hdgensch ds.conf file and the cobol layout for each segment is saved in the meta table. The syntax of the schema file matches that of the cobgensch or pligensch tool.

### 3.1 General Usage 

```hdgensch [command] [options] <dbd-name> <copybook-dir-name>```

**commands**
	
| COMMAND | DESCRIPTION                                                                                         |
|---------|-----------------------------------------------------------------------------------------------------|
| schema  | Creates the schema file for the DBD                                                                 |
| meta    | Saves the segment layout information of the DBD in the meta table by referencing the COBOL copybook |
| all     | Executes both schema and meta commands                                                              |

**options**
	
| OPTIONS  | DESCRIPTION                                                                                                     |
|----------|-----------------------------------------------------------------------------------------------------------------|
| -v       | Displays the hdgensch version                                                                                   |
| -t <n\|b> | Specifies how to save information about a column specified in an OCCURS clause                                  |
| &nbsp;&nbsp;&nbsp;-n    | Stands for Normal (save each column as meta data by assigning a number to it)                                   |
| &nbsp;&nbsp;&nbsp;-b    | Stands for Bulk (save all columns as a single column meta data. The total length equals the sum of all columns) |

**copybook-dir-name**

| copybook-dir-name  |  DESCRIPTION                                                                                                    |
|--------------------|-----------------------------------------------------------------------------------------------------------------|
|                    | Specifies the directory name where COBOL copybooks that contain HiDB dataset field information are stored. The COBOL copybook file name must follow the following naming convention: '<Segment Name - __specified in DBD__.cpy'. The PL/I copybook file name must follow the following naming convention: '<Segment Name - __specified in DBD__.inc'.  |

**NOTE** 
For hdgensch schema, a different command should be used for HDAM. (with **-he**)

### 3.2 Full Example

#### DEDB

```hdgensch meta ${dbdname} -f```

```hdgensch schema ${dbdname}```

#### HDAM

```hdgensch meta ${dbdname} -f```

```hdgensch schema ${dbdname} -he```

## Step 4. HIDBMGR

Hidbmgr manages HiDB meta data, user data, and libraries used at HiDB startup. It also performs the following functions:
			
	- Prints or deletes DBD meta data, which was derived using dbdgen, or creates DLI library to use for data loading
	
	- Prints or deletes PSB meta data, which was derived using psbgen, or created DLI library to use in HiDB programs
	
	- Creates, deletes, or initializes tables for DBD segments

### 4.1 Usage 

```hidbmgr <dbd|psb> <command> <dbd_name|psb_name> [options]```

**Parameters**

| PARAMETERS         | DESCRIPTION                        |
|--------------------|------------------------------------|
| dbd\|psb           | Control Block Type (DBD or PSB)    |
| dbd_name\|psb_name | The name of the Control Block File |

**Commands**

| COMMAND               | DESCRIPTION                                                                                                                  |
|-----------------------|------------------------------------------------------------------------------------------------------------------------------|
| display               | Prints information about the control block                                                                                   |
| dligen                | Creates DLI library to load the control block for use in a program. The file is saved in the path set in the hidb.conf file. |
| segm create <dbdname> | Creates an empty table for the given DBD                                                                                     |
| delete                | Deletes meta data of the control block                                                                                       |

**Options**

| OPTIONS       | DESCRIPTION                                                                                                        |
|---------------|--------------------------------------------------------------------------------------------------------------------|
| -vr <dbd_ver> | DBD version. (This is only applicable for DBDs)                                                                    |
| -l <lib_name> | DBD or PSB library name. (Default: DBDLIB_NAME\|PSBLIB_NAME item in the [IMS_DEFAULT] section of the ims.conf file |
| -f            | Option to first delete database objects before executing create on the DBD                                         |

### 4.2 Full Example

```hidbmgr segm create ${dbdname} -f```

```hidbmgr dbd dligen ${dbdname}```

## Step 5. Compile

During this step, we will compile the pc files generated in step 4.

During installation, HiDB comes equipped with a compile script. Simply execute the script like so:

### 5.1 Full example

```bash
cd $OPENFRAME_HOME/hidb/hidb_sch
sh compile.sh
```

## Step 6. HDPCDF01

**THIS STEP IS FOR DEDBs ONLY!!!** 

If you are not migrating a DEDB in this step, skip this step and go to step 7

hdpcdf01 is only for DEDB, and it should be used with **-dedb** option.

hdpcdf01 Takes the data sets unloaded by the IMS HD Reorganization Unload (DFSURGU0) utility to create OpenFrame/HiDB standard format data sets that can be processed by the hdload tool.

You **CANNOT** reload the datasets unloaded by DFSURGU0 to the HiDB database. In order to reload them, you must process them with the hdpcdf01 tool, which analyzes the internal format of the DFSURGU0 created datasets, removes the header and trailer, and formats them to fit the HiDB database reload format. 

### 6.1 Usage

```hdpcdf01 -dedb [options] [format] if=<input-file> of=<output-file> dbd=<dbd-name>```


| OPTIONS      | DESCRIPTION                                                                                                                                                              |
|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| -v           | Displays the hdpcdf01 version                                                                                                                                            |
| -h           | Prints help for hdpcdf01                                                                                                                                                 |
| -d           | Displays the current processing position in the input file during hdpcdf01 execution                                                                                     |
| -hp <number> | Used to specify the header length in bytes to analyze the input file format                                                                                              |
| -dp <number> | Used to specify the segment data length in bytes to analyze the input file format                                                                                        |
| -rdw         | Specify the input file has variable-length data sets containing 4-byte RDW                                                                                               |
| -addpk       | Adds the key area of a parent segment to beginning of its child segment record. The added key area is referenced to convert the child segment during data set migration. |

| FORMAT       | DESCRIPTION                                                                                                                                                              |
|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| -f1          | Specify if the input file has data sets unloaded from a SHISAM database                                                                                                  |

| PARAMETERS       | DESCRIPTION                                                                                                                                                                                                                                                       |
|-------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| if=\<input-file>  | hdpcdf01 input file, which is the data set created by DFSURGU0 (This is the unloaded data file from the mainframe)                                                                                                                                                |
| of=\<output-file> | hdpcdf01 output file. The output file we are creating here will be the input to the dsmigin tool. (This can be any given name, but it is recommended to use the same name as the input file with a different extension to signify)                                |
| dbd=\<dbd-name>   | Name of the DBD that defines the database of the input file. You must define the DBD in the default DBDLIB before running hdpcdf01. The name of the DBD can be up to 8 characters long. For more information about defining a DBDLIB refer to the DBDGEN section. |



### 6.2 Full Example

```hdpcdf01 -dedb if=tmax01uld.dat of=tmaxo01uld.hdb dbd=TMAX01PD```

The output will be placed in the default schema directory. The default schema directory can be found in the [DATASET_DIRECTORY] section of the ds.conf file.
	
Example:

```
	[DATASET_DIRECTORY]
	SCHEMA_DIR=<schema_dir>
```

## Step 7. DSMIGIN

dsmigin is an OpenFrame tool that converts EBCDIC data to ASCII. This step takes the unloaded data from the mainframe, either preformatted from hdpcdf01 or not, and converts it to ASCII so that it can be loaded.

For more information about DSMIGIN, please refer to the OpenFrame Tool Reference Guide.

### 7.1 Full Example

```dsmigin <input-file> <output-file> -sosi 6 -s ${dbdname}.conv -f V -C -D 2```

The input file is the unloaded data from the mainframe.

**If the unload file is for a DEDB, the input-file is the output from step 6**

output-file can be any given name, but again, it is recommended that you use the same name as the unload file, with a different extension to signify that it is the output from dsmigin.

Lastly, the schema file you pass ${dbdname}.conv, this is the converted copybook (schema file) generated from Step 3.





## Step 8. HDLOAD 

hdload takes data sets in the OpenFrame standard ASCII code format and loads them to the HiDB database. The input data set should be in the format described in the following figure. The format is the same as that of the unload data set created by the DFSURGU0 utility. Only use this with DEDB databases.

### 8.1 Usage

```hdload [options] <dbd-name> <load-data-file> <ddlist-file> [res-file]```

**Options**

| OPTIONS         | DESCRIPTION                                                                                                                                                                                                                                                                                               |
|-----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| -v              | Displays the hdload version                                                                                                                                                                                                                                                                               |
| -h              | Prints help for hdload                                                                                                                                                                                                                                                                                    |
| -c \| --catalog | Name of the catalog of the data set specified in the ddlist file. The name can be up to 32 characters long. If you want to specify multiple names, use a colon(:) as a delimiter                                                                                                                          |
| -q              | Loads data without inserting secondary index table                                                                                                                                                                                                                                                        |
| -mk             | Creates a file in a format to load the data by using tbLoader (sqlldr in Oracle). Set a path where the file is created in HIDB_IMPORT_DIR in the HIDB_DEFAULT section of hidb.conf                                                                                                                        |
| -im             | Loads data created with the -mk option in conventional method by using tbimport API                                                                                                                                                                                                                       |
| -im2            | Loads data created with the -mk option in DPL method by using tbimport API                                                                                                                                                                                                                                |
| -ld             | Loads data created with the -mk option in conventional method by using tbloader (sqlldr in Oracle).                                                                                                                                                                                                       |
| -ld2            | Loads data created with the -mk option in DPL method by using tbloader (sqlldr in Oracle).                                                                                                                                                                                                                |
| -md             | Merges data created with the -mk option by using tbLoader (sqlldr in Oracle). If the key field of data to load is the same as that of data already loaded, REPLACE is executed. OCC_ID is not updated. If the key field of data to load is not the same as that of dat already loaded, INSERT is executed |
| -qmg            | Executes both -mk and -mg options. Specify dbd-name and load-data-file                                                                                                                                                                                                                                    |
| -rmpk           | Deletes parent key data from the migrated segment data. Used with the --adpk option is specified in hdpcdf01                                                                                                                                                                                              |

**Parameters**

| PARAMETERS       | DESCRIPTION                                                                                                                                                                                                                                                                                               |
|------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| \<dbd-name>      | Name of the DBD that contains the database definition of the input file. You must register the DBD in the DBDLIB before running hdload. The name can be up to 8 characters long. For more information about registering DBDLOG, refer to DBDGEN.                                                          |
| \<load-data-file>| Target input file to be loaded by hdload                                                                                                                                                                                                                                                                  |
| \<ddlist-file>   | Name of the ddlist file to allocate data sets of the target database to load. The name must be in the format '<DDname> + <space> + <dataset name>'                                                                                                                                                        |
| \<res-file>      | File to use for the prefix resolution task while you load a database that contains segments with logical relationships. This file is used as an input for the hdpxres tool, which is a prefix resolution tool.                                                                                            |
| log=\<log-file>   | Log file where loading will be logged. With this parameter, you can identify the cause of a reloading error because the log data set stores records of the segment sequence, name, key value, and status code for the target dataset being reloaded.                                                      |

### 8.2 Full Example
			
```hdload <dbdname> <output_from_dsmigin> log=<log-dir>/DEDB_<line>_hdload.log``` 
