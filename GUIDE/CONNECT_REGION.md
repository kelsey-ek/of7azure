DB Connection 

- CONNECT.cob

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

- RECONNECT.cob

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

- COMMIT.cob

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

- TROLLBACK.cob

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

- DISCONN.cob

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

<CONNECT.cob>

```
tbpcb CONNECT.cob RUNTIME_MODE=ODBC INCLUDE=/tmaxapp/common/copy/ ONAME=tbpcb_CONNECT.cbl
ofcob tbpcb_CONNECT.cbl -o libCONNECT.so -L/opt/tmaxapp/OpenFrame/lib -ltextfh -ltextsm -lconcli -ladjust -L/opt/tmaxapp/unixODBC/lib -lodbc -L/opt/tmaxdb/tibero6/client/lib -ltbertl_odbc -lclientcommon -ltbertl 
```

<RECONNECT.cob>

```
tbpcb RECONNECT.cob RUNTIME_MODE=ODBC INCLUDE=/tmaxapp/common/copy/ ONAME=tbpcb_RECONNECT.cbl
ofcob tbpcb_RECONNECT.cbl -o libRECONNECT.so -L/opt/tmaxapp/OpenFrame/lib -ltextfh -ltextsm -lconcli -ladjust -L/opt/tmaxapp/unixODBC/lib -lodbc -L/opt/tmaxdb/tibero6/client/lib -ltbertl_odbc -lclientcommon –ltbertl
```

<COMMIT.cob>

```
tbpcb COMMIT.cob RUNTIME_MODE=ODBC INCLUDE=/tmaxapp/common/copy/ ONAME=tbpcb_COMMIT.cbl
ofcob tbpcb_COMMIT.cbl -o libCOMMIT.so -L/opt/tmaxapp/OpenFrame/lib -ltextfh -ltextsm -lconcli -ladjust -L/opt/tmaxapp/unixODBC/lib -lodbc -L/opt/tmaxdb/tibero6/client/lib -ltbertl_odbc -lclientcommon –ltbertl
```

<TROLLBACK.cob>

```
tbpcb TROLLBACK.cob RUNTIME_MODE=ODBC INCLUDE=/tmaxapp/common/copy/ ONAME=tbpcb_TROLLBACK.cbl
ofcob tbpcb_TROLLBACK.cbl  -o libTROLLBACK.so -L/opt/tmaxapp/OpenFrame/lib -ltextfh -ltextsm -lconcli -ladjust -L/opt/tmaxapp/unixODBC/lib -lodbc -L/opt/tmaxdb/tibero6/client/lib -ltbertl_odbc -lclientcommon -ltbertl
```

<DISCONN.cob>

```
tbpcb DISCONN.cob RUNTIME_MODE=ODBC INCLUDE=/tmaxapp/common/copy/ ONAME=tbpcb_DISCONN.cbl
ofcob tbpcb_DISCONN.cbl -o libDISCONN.so -L/opt/tmaxapp/OpenFrame/lib -ltextfh -ltextsm -lconcli -ladjust -L/opt/tmaxapp/unixODBC/lib -lodbc -L/opt/tmaxdb/tibero6/client/lib -ltbertl_odbc -lclientcommon -ltbertl
```


* link거는 shared object의 파일명은 lib으로 시작해야한다.
* tbcpb에서 예약어사용은 syntax error를 발생시키기 때문에 피한다 .( ROLLBACK -> TROLLBACK )
oframe@aebdas-tmax01.sccompanies.com:/home/oframe/nick/DBTEST /> ll object/
total 96
-rwxr-xr-x. 1 oframe mqm 22392 Jun  1 16:39  libCOMMIT.so
-rwxr-xr-x. 1 oframe mqm 47192 Jun  1 16:47  libCONNECT.so
-rwxr-xr-x. 1 oframe mqm 27008 Sep 12 02:55 libRECONNECT.so
-rwxr-xr-x. 1 oframe mqm 22408 Jun  1 16:39  libTROLLBACK.so
-rwxr-xr-x. 1 oframe mqm 18432 Jun 14 00:25  libDISCONN.so


