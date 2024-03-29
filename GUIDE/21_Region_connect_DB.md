### Use OSCBUILD to make DB Connection

+ [1. Set the DB connection from configuration files](#1-set-the-db-connection-from-configuration-files)
+ [2. Write five program](#2-write-five-programs)
+ [3. Compile five programs](#3-compile-five-programs)
+ [4. Deploy five programs](#4-deploy-five-programs)
+ [5. Modify the OSC00001.c file](#5-modify-the-osc00001c-file)
+ [6. Build region processes](#6-build-region-processes)
+ [7. Deploy the region process](#7-deploy-the-region-process)
+ [8. Use the right compile option](#8-use-the-right-compile-option)
+ [9. Appendix](#9-appendix)


## 1. Set the DB connection from configuration files.

1) osc.conf

```
[DB_INFO]
    DATABASE=TVSAM
    USERNAME=tibero
    PASSWORD=tmax
    ENPASSWD=
```

2) $ODBCINI

```
[TVSAM]
Description = Tibero ODBC driver for Tibero6
Driver = /opt2/tmaxapp/tibero6/client/lib/libtbodbc.so
SERVER = 10.0.1.73
PORT = 8629
DSN=TVSAM
TB_SID=TVSAM
User = tibero
Password = tmax
DATABASE= TVSAM
```

## 2. Write five programs.

1) CONNECT.cob
- Make DB connection.

```
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         CONNECT.
       AUTHOR.             US-CB.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       IBM.
       OBJECT-COMPUTER.       IBM.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 DATABASE  PIC X(32).
       01 USERNAME  PIC X(32).
       01 PASSWORD  PIC X(32).
       01 OF-RC     PIC S9(8) COMP-5 VALUE 0.

       PROCEDURE       DIVISION.
       M0-MAIN                SECTION.
           PERFORM GET-OF-INFO
           PERFORM M1-DBCONN
           PERFORM M2-CHECK
           GOBACK.
       M0-MAIN-EXIT.
           EXIT.
       ERR-RTN.
           MOVE 8 TO RETURN-CODE.
           GOBACK.
       ERR-RTN-EXIT.
           EXIT.
       GET-OF-INFO.
           CALL "cics_env_get_dbinfo"
                USING BY REFERENCE DATABASE USERNAME PASSWORD
                RETURNING OF-RC.
           PERFORM CHECK-RC.
       GET-OF-INFO-EXIT.
           EXIT.
       CHECK-RC.
           IF OF-RC < 0 THEN
              DISPLAY "[ERROR] cics_env_get_dbinfo rc : " OF-RC
              PERFORM ERR-RTN
           ELSE IF DATABASE(1:1) = LOW-VALUE
              DISPLAY "[ERROR] check the DATABASE value in [DB_INFO] "
      -               "at osc.conf"
              PERFORM ERR-RTN
           ELSE IF USERNAME(1:1) = LOW-VALUE
              DISPLAY "[ERROR] check the USERNAME value in [DB_INFO] "
      -               "at osc.conf"
              PERFORM ERR-RTN
           ELSE IF PASSWORD(1:1) = LOW-VALUE
              DISPLAY "[ERROR] check the PASSWORD value in [DB_INFO] "
      -               "at osc.conf"
              PERFORM ERR-RTN
           END-IF.
       CHECK-RC-EXIT.
           EXIT.
       M1-DBCONN              SECTION.
           DISPLAY "[INFO] Create a DB connection.".
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED
               BY :PASSWORD
               USING :DATABASE
           END-EXEC.
           EVALUATE  SQLCODE
             WHEN  ZERO
                   CONTINUE
             WHEN  OTHER
                   DISPLAY "[ERROR] DB connection creation failed.!!"
                   DISPLAY "SQLCODE  [" SQLCODE "]"
                   DISPLAY "SQLERRMC [" SQLERRMC "]"
                   PERFORM ERR-RTN
           END-EVALUATE.
       M1-DBCONN-EXIT.
           EXIT.
       M2-CHECK               SECTION.
           DISPLAY "[INFO] Check DB connection."
           EXEC SQL
               SELECT 1 FROM DUAL
           END-EXEC.
           EVALUATE  SQLCODE
             WHEN  ZERO
                   DISPLAY "[INFO] DB is connected normally."
                   CONTINUE
             WHEN  OTHER
                   DISPLAY "[INFO] The DB connection is already"
      -                    " disconnected."
                   DISPLAY "SQLCODE  [" SQLCODE "]"
                   DISPLAY "SQLERRMC [" SQLERRMC "]"
                   PERFORM ERR-RTN
           END-EVALUATE.
       M2-CHECK-EXIT.
           EXIT.
```

2) RECONNECT.cob
- Do the reconnection.

```
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         RECONNECT.
       AUTHOR.             US-CB.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       IBM.
       OBJECT-COMPUTER.       IBM.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01 DATABASE  PIC X(32).
       01 USERNAME  PIC X(32).
       01 PASSWORD  PIC X(32).
       01 OF-RC     PIC S9(8) COMP-5 VALUE 0.

       PROCEDURE       DIVISION.
       M0-MAIN                SECTION.
           PERFORM GET-OF-INFO
           PERFORM M1-CHECK.
           GOBACK.
       M0-MAIN-EXIT.
           EXIT.
       ERR-RTN.
           MOVE 8 TO RETURN-CODE.
           GOBACK.
       ERR-RTN-EXIT.
           EXIT.
       GET-OF-INFO.
           CALL "cics_env_get_dbinfo"
                USING BY REFERENCE DATABASE USERNAME PASSWORD
                RETURNING OF-RC.
           PERFORM CHECK-RC.
       GET-OF-INFO-EXIT.
           EXIT.
       CHECK-RC.
           IF OF-RC < 0 THEN
              DISPLAY "[ERROR] cics_env_get_dbinfo rc : " OF-RC
              PERFORM ERR-RTN
           ELSE IF DATABASE(1:1) = LOW-VALUE
              DISPLAY "[ERROR] check the DATABASE value in [DB_INFO] "
      -               "at osc.conf"
              PERFORM ERR-RTN
           ELSE IF USERNAME(1:1) = LOW-VALUE
              DISPLAY "[ERROR] check the USERNAME value in [DB_INFO] "
      -               "at osc.conf"
              PERFORM ERR-RTN
           ELSE IF PASSWORD(1:1) = LOW-VALUE
              DISPLAY "[ERROR] check the PASSWORD value in [DB_INFO] "
      -               "at osc.conf"
              DISPLAY "ofcom_conf_get_value rc : " OF-RC
              PERFORM ERR-RTN
           END-IF.
       CHECK-RC-EXIT.
           EXIT.
       M1-CHECK               SECTION.
           DISPLAY "[INFO] Check DB connection."
           EXEC SQL
               SELECT 1 FROM DUAL
           END-EXEC.
           EVALUATE  SQLCODE
             WHEN  ZERO
                   DISPLAY "[INFO] DB is connected normally."
                   CONTINUE
             WHEN  OTHER
                   DISPLAY "[INFO] The DB connection is already"
      -                    " disconnected."
                   PERFORM M2-DBCONN
           END-EVALUATE.
       M1-CHECK-EXIT.
       M2-DBCONN              SECTION.
           DISPLAY "[INFO] Create a DB connection.".
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED
               BY :PASSWORD
               USING :DATABASE
           END-EXEC.
           EVALUATE  SQLCODE
             WHEN  ZERO
                   CONTINUE
             WHEN  OTHER
                   DISPLAY "[ERROR] DB connection creation failed.!!"
                   DISPLAY "SQLCODE  [" SQLCODE "]"
                   DISPLAY "SQLERRMC [" SQLERRMC "]"
                   PERFORM ERR-RTN
           END-EVALUATE.
       M2-DBCONN-EXIT.
           EXIT.
```

3) COMMIT.cob
- Do commit.

```
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         COMMIT.
       AUTHOR.             US-CB.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       IBM.
       OBJECT-COMPUTER.       IBM.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE       DIVISION.
       M0-MAIN                SECTION.
           PERFORM  M1-MAIN.
       M0-MAIN-EXIT.
           GOBACK.
       M1-MAIN                SECTION.
           DISPLAY "[INFO] Starting DB commit.".
           EXEC  SQL  COMMIT END-EXEC.
           EVALUATE  SQLCODE
                   WHEN  ZERO
                         CONTINUE
                   WHEN  OTHER
                         DISPLAY "[ERROR] DB commit failed.!!"
                         DISPLAY "SQLCODE [" SQLCODE "]"
                         DISPLAY "SQLERRMC [" SQLERRMC "]"
           END-EVALUATE.
       M1-MAIN-EXIT.
           EXIT.
```

4) TROLLBACK.cob
- Do Rollback.

```
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         TROLLBACK.
       AUTHOR.             US-CB.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       IBM.
       OBJECT-COMPUTER.       IBM.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE       DIVISION.
       M0-MAIN                SECTION.
           PERFORM  M1-MAIN.
       M0-MAIN-EXIT.
           GOBACK.
       M1-MAIN                SECTION.
           DISPLAY "[INFO] Starting DB rollback.".
           EXEC SQL ROLLBACK END-EXEC.
           EVALUATE  SQLCODE
                   WHEN  ZERO
                         CONTINUE
                   WHEN  OTHER
                         DISPLAY "[ERROR] DB rollback failed.!!"
                         DISPLAY "SQLCODE [" SQLCODE "]"
                         DISPLAY "SQLERRMC [" SQLERRMC "]"
           END-EVALUATE.
       M1-MAIN-EXIT.
           EXIT.
```

5) DISCONN.cob
- Do Disconncetion.

```
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         DISCONN.
       AUTHOR.             US-CB.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       IBM.
       OBJECT-COMPUTER.       IBM.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE       DIVISION.
       M0-MAIN                SECTION.
           PERFORM  M1-MAIN.
       M0-MAIN-EXIT.
           GOBACK.
       M1-MAIN                SECTION.
           DISPLAY "[INFO] Disconnect the DB connection.".
           EXEC SQL COMMIT WORK RELEASE END-EXEC.
           EVALUATE  SQLCODE
                   WHEN  ZERO
                         CONTINUE
                   WHEN  OTHER
                         DISPLAY "[ERROR] DB disconnection failed.!!"
                         DISPLAY "SQLCODE [" SQLCODE "]"
                         DISPLAY "SQLERRMC [" SQLERRMC "]"
           END-EVALUATE.
       M1-MAIN-EXIT.
           EXIT.
```

## 3. Compile five programs.

1) CONNECT.cob

```
tbpcb CONNECT.cob RUNTIME_MODE=ODBC INCLUDE=/home/oframe/tibero6/client/include ONAME=tbpcb_CONNECT.cbl

ofcob tbpcb_CONNECT.cbl -o libCONNECT.so -L$OPENFRAME_HOME/lib -ltextfh -ltextsm -lconcli -ladjust -L$ODBC_HOME/lib -lodbc -L$TB_HOME/client/lib -ltbertl_odbc -lclientcommon -ltbertl 
```

2) RECONNECT.cob

```
tbpcb RECONNECT.cob RUNTIME_MODE=ODBC INCLUDE=/home/oframe/tibero6/client/include ONAME=tbpcb_RECONNECT.cbl

ofcob tbpcb_RECONNECT.cbl -o libRECONNECT.so -L$OPENFRAME_HOME/lib -ltextfh -ltextsm -lconcli -ladjust -L$ODBC_HOME/lib -lodbc -L$TB_HOME/client/lib -ltbertl_odbc -lclientcommon -ltbertl 
```

3) COMMIT.cob

```
tbpcb COMMIT.cob RUNTIME_MODE=ODBC INCLUDE=/home/oframe/tibero6/client/include ONAME=tbpcb_COMMIT.cbl

ofcob tbpcb_COMMIT.cbl -o libCOMMIT.so -L$OPENFRAME_HOME/lib -ltextfh -ltextsm -lconcli -ladjust -L$ODBC_HOME/lib -lodbc -L$TB_HOME/client/lib -ltbertl_odbc -lclientcommon -ltbertl 
```

4) TROLLBACK.cob

```
tbpcb TROLLBACK.cob RUNTIME_MODE=ODBC INCLUDE=/home/oframe/tibero6/client/include ONAME=tbpcb_TROLLBACK.cbl

ofcob tbpcb_TROLLBACK.cbl  -o libTROLLBACK.so -L$OPENFRAME_HOME/lib -ltextfh -ltextsm -lconcli -ladjust -L$ODBC_HOME/lib -lodbc -L$TB_HOME/client/lib -ltbertl_odbc -lclientcommon -ltbertl 
```

5) DISCONN.cob

```
tbpcb DISCONN.cob RUNTIME_MODE=ODBC INCLUDE=/home/oframe/tibero6/client/include ONAME=tbpcb_DISCONN.cbl

ofcob tbpcb_DISCONN.cbl -o libDISCONN.so -L$OPENFRAME_HOME/lib -ltextfh -ltextsm -lconcli -ladjust -L$ODBC_HOME/lib -lodbc -L$TB_HOME/client/lib -ltbertl_odbc -lclientcommon -ltbertl 
```

- Name of shared object files for linking should start with 'lib'. ex) libCOMMIT.so

- Do not use the reserved words for tbpcb because it can make syntax error. ex) ROLLBACK -> TROLLBACK 

```
oframe@aebdas-tmax01.sccompanies.com:/home/oframe/nick/DBTEST /> ll object/
total 96
-rwxr-xr-x. 1 oframe mqm 22392 Jun  1 16:39  libCOMMIT.so
-rwxr-xr-x. 1 oframe mqm 47192 Jun  1 16:47  libCONNECT.so
-rwxr-xr-x. 1 oframe mqm 27008 Sep 12 02:55  libRECONNECT.so
-rwxr-xr-x. 1 oframe mqm 22408 Jun  1 16:39  libTROLLBACK.so
-rwxr-xr-x. 1 oframe mqm 18432 Jun 14 00:25  libDISCONN.so
```

## 4. Deploy five programs.

- Deploy the compiled five shared object under $OPENFRAME_HOME/lib

```
cp libCOMMIT.so libCONNECT.so libRECONNECT.so libTROLLBACK.so libDISCONN.so $OPENFRAME_HOME/lib/
```

## 5. Modify the OSC00001.c file.

- $OPENFRAME_HOME/osc/build/OSC00001.c

```
/*
 * OpenFrame Online System type C(OSC) C Source File
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int otpsvrinit(int argc, char *argv[])
{
    CONNECT();
    return 0;
}

int otpsvcinit()
{
    RECONNECT();
    return 0;
}

 int otpsvrcommit()
{
    COMMIT();
    return 0;
}

int otpsvrrollback()
{
    TROLLBACK();
    return 0;
}

int otpsvrdone()
{
    DISCONN();
    return 0;
}
```

## 6. Build region processes.

```
oscbuild -o LINUX64 -s OSCOIVP1 -b OFCOBOL -d TIBERO -f OSC00001.c -l '-L$OPENFRAME_HOME/lib -lCONNECT -lCOMMIT -lTROLLBACK -lDISCONN -lRECONNECT'
```

## 7. Deploy the region process.

```
cp -a OSCOIVP1 $TMAXDIR/appbin/
```

## 8. Use the right compile option.
- Do not forget to use the correct compile option for using application programs.

**tbpcb**
       
```
RUNTIME_MODE=ODBC
```


## 9. Appendix

- Test the modules only.

```
cmd : ofcbpp -i CONNECT.cob -o ofcbpp_CONNECT.cob
cmd : tbpcb INAME=ofcbpp_CONNECT.cob RUNTIME_MODE=ODBC VARCHAR=YES INCLUDE=/tmaxapp/common/copy/ END_OF_FETCH=100 ONAME=tbpcb_CONNECT.cbl
cmd : ofcob -x tbpcb_CONNECT.cbl -o CONNECT -L/opt/tmaxapp/unixODBC/lib -lodbc -L/opt/tmaxdb/tibero6/client/lib -ltbertl_odbc -lclientcommon -ltbertl --force-trace
oframe@aebdas-tmax01.sccompanies.com:/home/oframe/nick/DBTEST /> ./object/CONNECT
M0-MAIN
M0-CHECK
[INFO] Check DB connection.
[INFO] The DB connection is already disconnected.
M1-DBCONN
[INFO] Create a DB connection.
M1-DBCONN-EXIT
M0-CHECK-EXIT
M0-MAIN-EXIT
```

