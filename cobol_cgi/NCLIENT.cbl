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

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

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

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      *****************************************************************
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

           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           IF  SQLCODE NOT = ZERO 
              MOVE "STATUS: 500 INTERNAL SERVER ERROR - CONNECTION" 
                                                        TO SQL-ERROR 
              PERFORM 1070-SEND-RESPONSE
           END-IF.

      ******************************************************************
       1040-VERIFY-ADRESSE.
      ******************************************************************
           
           EXEC SQL 
              SELECT adresse_id INTO :ADRESSE_ID FROM adresse 
                    WHERE ville = :VILLE AND adresse_nom = :ADRESSE_NOM
                    AND num = :NUM AND code_postal = :CODE_POSTAL
                    AND pays = :PAYS
           END-EXEC

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
           
           EXEC SQL
              INSERT INTO adresse (ville, adresse_nom, num, code_postal, 
                    pays) VALUES (:VILLE,:ADRESSE_NOM, :NUM,
                    :CODE_POSTAL, :PAYS)
           END-EXEC
           IF SQLCODE EQUAL ZERO
                 EXEC SQL COMMIT WORK END-EXEC
                 PERFORM 1040-VERIFY-ADRESSE
           ELSE 
              MOVE "STATUS: 500 INTERNAL SERVER ERROR - INSERT ADRESSE" 
                                                        TO SQL-ERROR 
              PERFORM 1070-SEND-RESPONSE
           END-IF.
           
      ******************************************************************
       1050-INSERT-CLIENT.
      ******************************************************************
           EXEC SQL 
              INSERT INTO client (nom, prenom, mail, telephone,
                    n_identite, principal_adresse_id) VALUES (:NOM,
                    :PRENOM, :MAIL, :TELEPHONE, :N_IDENTITE, 
                       :P_ADRESSE_ID)
           END-EXEC

           IF SQLCODE NOT EQUAL ZERO 
              IF SQLCODE EQUAL -400 AND SQLSTATE EQUAL"22P05"
                 MOVE "RECORD ALREADY INSERTED " TO SQL-ERROR
              ELSE
                MOVE "STATUS: 500 INTERNAL SERVER ERROR - INSERT CLIENT" 
                                                        TO SQL-ERROR 
              PERFORM 1070-SEND-RESPONSE
           ELSE
              EXEC SQL COMMIT WORK END-EXEC
           END-IF.
      
      ******************************************************************
       1060-CLOSE-DB.
      ******************************************************************

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.
      
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
