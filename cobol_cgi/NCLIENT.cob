      *****************************************************************
      * Program name:    NCLIENT                               
      * Original author: KERESTES                                
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 11/06/24  KERESTES      Created for COBOL new client insertion         
      *                                                               
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 NCLIENT.
       AUTHOR.                     ALEXANDRE KERESTES.
       DATE-WRITTEN.               2024-06-11.
      *****************************************************************
       ENVIRONMENT DIVISION. 
      *----------------------------------------------------------------
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT WEBINPUT ASSIGN TO KEYBOARD
	         FILE STATUS IS POST-STATUS.
      *****************************************************************     
       DATA DIVISION.
      *----------------------------------------------------------------
       
       FILE SECTION. 
       
       FD  WEBINPUT.
       01  POSTCHUNK PIC X(2048).
      *----------------------------------------------------------------
       
       WORKING-STORAGE SECTION. 

           COPY JSONCPYBK.

      *****************************************************************
      *                  HTTP VARIABLES
      ***************************************************************** 

       01  REQ_METHOD           PIC X(4).
       77  NEWLINE              PIC X VALUE x"0a".

      *****************************************************************
      *                    POST BODY VARIABLES
      ***************************************************************** 

       01  POST-STATUS          PIC 99.
           88 OK                VALUE 00.
       01  JSON-STRING          PIC X(2048).

      *----------------------------------------------------------------
      *                        SQL DECLARATION
      *---------------------------------------------------------------- 

       01  SQL-ERROR               PIC X(80) VALUE SPACES.

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       
       01  CLIENT_INFO.
         02  ADRESSE.
           03 ADRESSE_ID           PIC 9(9).
           03 VILLE                PIC X(50).
           03 NUM                  PIC S9(5).
           03 ADRESSE_NOM          PIC X(100).
           03 CODE_POSTAL          PIC S9(7).
           03 PAYS                 PIC X(50).

         02  CLIENT.
           03 CLIENT_ID            PIC 9(9).
           03 NOM                  PIC X(100).
           03 PRENOM               PIC X(50).
           03 MAIL                 PIC X(200).
           03 TELEPHONE            PIC X(20).
           03 N_IDENTITE           PIC X(20).
           03 P_ADRESSE_ID         PIC 9(9).

OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

      *****************************************************************
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(116) VALUE "SELECT adresse_id FROM adresse"
OCESQL  &  " WHERE ville = $1 AND adresse_nom = $2 AND num = $3 AND co"
OCESQL  &  "de_postal = $4 AND pays = $5".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(094) VALUE "INSERT INTO adresse (ville, ad"
OCESQL  &  "resse_nom, num, code_postal, pays) VALUES ( $1, $2, $3, $4"
OCESQL  &  ", $5 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(117) VALUE "INSERT INTO client (nom, preno"
OCESQL  &  "m, mail, telephone, n_identite, principal_adresse_id) VALU"
OCESQL  &  "ES ( $1, $2, $3, $4, $5, $6 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.

           ACCEPT
             REQ_METHOD FROM ENVIRONMENT "REQUEST_METHOD"
           END-ACCEPT.

           IF REQ_METHOD NOT EQUAL "POST"
              DISPLAY "STATUS: 405 METHOD NOT ALLOWED"
              GOBACK
           END-IF

           PERFORM 1000-JSON-STRING
           PERFORM 1010-EXTRAIT-JSON
           PERFORM 1020-PREPARE-SQL-VAR
           PERFORM 1030-DB-CONNECT
           PERFORM 1040-VERIFY-ADRESSE
           PERFORM 1050-INSERT-CLIENT
           PERFORM 1060-CLOSE-DB
           PERFORM 1070-SEND-RESPONSE

           GOBACK.

      ******************************************************************
       1000-JSON-STRING.
      ******************************************************************
           OPEN INPUT WEBINPUT
           IF NOT OK 
              DISPLAY "STATUS: 500 INTERNAL SERVER ERROR"
              GOBACK
           END-IF
              
           READ WEBINPUT 
              MOVE POSTCHUNK TO JSON-STRING 
           CLOSE WEBINPUT.     

      ******************************************************************
       1010-EXTRAIT-JSON.
      ******************************************************************
           CALL "PARSEJ" USING BY REFERENCE JSON-CP-STRING JSON-STRING.

      ******************************************************************
       1020-PREPARE-SQL-VAR.
      ******************************************************************
           IF JSON-TABLE-NAME(1) EQUAL "client"
              MOVE ITEM-STRING-VALUE(1 1) TO NOM
              MOVE ITEM-STRING-VALUE(1 2) TO PRENOM
              MOVE ITEM-STRING-VALUE(1 3) TO MAIL 
              MOVE ITEM-STRING-VALUE(1 4) TO TELEPHONE 
              MOVE ITEM-STRING-VALUE(1 5) TO N_IDENTITE

              MOVE ITEM-STRING-VALUE(2 1) TO VILLE 
              MOVE ITEM-STRING-VALUE(2 2) TO ADRESSE_NOM 
              MOVE ITEM-STRING-VALUE(2 3) TO PAYS 
              MOVE ITEM-INT-VALUE(2 1) TO NUM 
              MOVE ITEM-INT-VALUE(2 2) TO CODE_POSTAL
           ELSE
              MOVE ITEM-STRING-VALUE(2 1) TO NOM
              MOVE ITEM-STRING-VALUE(2 2) TO PRENOM
              MOVE ITEM-STRING-VALUE(2 3) TO MAIL 
              MOVE ITEM-STRING-VALUE(2 4) TO TELEPHONE 
              MOVE ITEM-STRING-VALUE(2 5) TO N_IDENTITE

              MOVE ITEM-STRING-VALUE(1 1) TO VILLE 
              MOVE ITEM-STRING-VALUE(1 2) TO ADRESSE_NOM 
              MOVE ITEM-STRING-VALUE(1 3) TO PAYS 
              MOVE ITEM-INT-VALUE(1 1) TO NUM 
              MOVE ITEM-INT-VALUE(1 2) TO CODE_POSTAL
           END-IF.

      ******************************************************************
       1030-DB-CONNECT.
      ******************************************************************
           
           ACCEPT DBNAME FROM ENVIRONMENT "DB_DATABASE"
           ACCEPT USERNAME FROM ENVIRONMENT "DB_USER"
           ACCEPT PASSWD FROM ENVIRONMENT "DB_PASSWORD"

      *     MOVE  "cobbank@localhost"  TO   DBNAME.
      *     MOVE  "postgres"        TO   USERNAME.
      *     MOVE  "rootroot"             TO   PASSWD.

OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.
           IF  SQLCODE NOT = ZERO 
              MOVE "STATUS: 500 INTERNAL SERVER ERROR - CONNECTION" 
                                                        TO SQL-ERROR 
              PERFORM 1070-SEND-RESPONSE
           END-IF.

      ******************************************************************
       1040-VERIFY-ADRESSE.
      ******************************************************************
           
OCESQL*    EXEC SQL 
OCESQL*       SELECT adresse_id INTO :ADRESSE_ID FROM adresse 
OCESQL*             WHERE ville = :VILLE AND adresse_nom = :ADRESSE_NOM
OCESQL*             AND num = :NUM AND code_postal = :CODE_POSTAL
OCESQL*             AND pays = :PAYS
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 9
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE ADRESSE_ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE VILLE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 100
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE ADRESSE_NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 3
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE NUM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 3
OCESQL          BY VALUE 7
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE CODE_POSTAL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE PAYS
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL          BY VALUE 5
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

           IF SQLCODE EQUAL 100
              PERFORM 1041-INSERT-ADRESSE
           ELSE IF SQLCODE = +0
              MOVE ADRESSE_ID TO P_ADRESSE_ID
           ELSE
              MOVE "STATUS: 500 INTERNAL SERVER ERROR - SELECT" 
                                                        TO SQL-ERROR 
              PERFORM 1070-SEND-RESPONSE
           END-IF.
      
      ******************************************************************
       1041-INSERT-ADRESSE.
      ******************************************************************
           
OCESQL*    EXEC SQL
OCESQL*       INSERT INTO adresse (ville, adresse_nom, num, code_postal, 
OCESQL*             pays) VALUES (:VILLE,:ADRESSE_NOM, :NUM,
OCESQL*             :CODE_POSTAL, :PAYS)
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE VILLE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 100
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE ADRESSE_NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 3
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE NUM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 3
OCESQL          BY VALUE 7
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE CODE_POSTAL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE PAYS
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 5
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
           IF SQLCODE EQUAL ZERO
OCESQL*          EXEC SQL COMMIT WORK END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
                 PERFORM 1040-VERIFY-ADRESSE
           ELSE 
              MOVE "STATUS: 500 INTERNAL SERVER ERROR - INSERT ADRESSE" 
                                                        TO SQL-ERROR 
              PERFORM 1070-SEND-RESPONSE
           END-IF.
           
      ******************************************************************
       1050-INSERT-CLIENT.
      ******************************************************************
OCESQL*    EXEC SQL 
OCESQL*       INSERT INTO client (nom, prenom, mail, telephone,
OCESQL*             n_identite, principal_adresse_id) VALUES (:NOM,
OCESQL*             :PRENOM, :MAIL, :TELEPHONE, :N_IDENTITE, 
OCESQL*                :P_ADRESSE_ID)
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 100
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 200
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE MAIL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 20
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE TELEPHONE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 20
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE N_IDENTITE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 9
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE P_ADRESSE_ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 6
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

           IF SQLCODE NOT EQUAL ZERO 
              IF SQLCODE EQUAL -400 AND SQLSTATE EQUAL"22P05"
                 MOVE "RECORD ALREADY INSERTED " TO SQL-ERROR
              ELSE
                MOVE "STATUS: 500 INTERNAL SERVER ERROR - INSERT CLIENT" 
                                                        TO SQL-ERROR 
              PERFORM 1070-SEND-RESPONSE
           ELSE
OCESQL*       EXEC SQL COMMIT WORK END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
           END-IF.
      
      ******************************************************************
       1060-CLOSE-DB.
      ******************************************************************

OCESQL*    EXEC SQL
OCESQL*        DISCONNECT ALL
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
      
      ******************************************************************
       1070-SEND-RESPONSE.
      ******************************************************************
           
           IF SQL-ERROR NOT EQUAL SPACES
              DISPLAY "Content-Type: application/json" NEWLINE NEWLINE
              DISPLAY "{error: " SQL-ERROR "}"
              PERFORM 1060-CLOSE-DB
           ELSE
              DISPLAY "Content-Type: application/json" NEWLINE NEWLINE
              DISPLAY '{request: "successfull }'
           END-IF 
           GOBACK.
           GOBACK.
           GOBACK.
