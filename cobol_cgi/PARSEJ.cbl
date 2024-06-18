      *****************************************************************
      * Program name:    PARSEJ                             
      * Original author: ALEXANDRE RODRIGUES KERESTES                           
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 12/06/248 KERESTES      Created for COBOL Json parse         
      *                                                               
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PARSEJ.
       AUTHOR. KERESTES. 
      *****************************************************************
       DATA DIVISION. 

       WORKING-STORAGE SECTION. 

       01  JSON-TEMP.
         02 JSON-TEMP-NAME          PIC X(50).

       01  COUNTER                  PIC 9999.
       01  COUNTER-2                PIC 9999.
       01  COUNTER-CPYBK            PIC 9999 VALUE 1.
       01  COUNTER-INIT             PIC 999 VALUE 0.
       01  COUNTER-END              PIC 999 VALUE 0.

       01  END-FILE-FLAG            PIC X VALUE 'N'.
       01  LEVEL-NESTED-FLAG        PIC 99 VALUE ZERO.
       01  COUNTER-NESTED-FLAG       PIC 99 VALUE ZERO.

       01  NAME-OR-VALUE            PIC 9 VALUE 0.
           88  IS-NONE              VALUE 0.
           88  IS-NAME              VALUE 1.
           88  IS-VALUE             VALUE 2.
       
       01  VALUE-TYPE               PIC 9 VALUE 0.
           88  IS-NONE-TYPE         VALUE 0.
           88  IS-STRING            VALUE 1.
           88  IS-INT               VALUE 2.
           88  IS-LONG              VALUE 3.
       
       LINKAGE SECTION.

           COPY JSONCPYBK.

       01  JSON-STRING     PIC X(2048).

       PROCEDURE DIVISION USING JSON-CP-STRING, JSON-STRING.

       MAIN-PARA.

           PERFORM 1000-PARSE-JSON.
           GOBACK.

       1000-PARSE-JSON.
           PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL 
                                                  END-FILE-FLAG  = 'Y'

              EVALUATE JSON-STRING(COUNTER:1)
                  WHEN "{"
                     ADD 1 TO LEVEL-NESTED-FLAG
                     IF IS-VALUE
                       PERFORM 1010-VERIFY-NAME-TABLE
                     END-IF
                  WHEN "}"
                     SUBTRACT 1 FROM LEVEL-NESTED-FLAG
                     IF IS-VALUE AND COUNTER-END EQUAL ZERO 
                       COMPUTE COUNTER-END = COUNTER - COUNTER-INIT
                       PERFORM 1020-INSERT-NAME-VALUE
                     END-IF
                     IF LEVEL-NESTED-FLAG = 0
                       MOVE 'Y' TO END-FILE-FLAG 
                     ELSE IF COUNTER-NESTED-FLAG > ZERO 
                       SUBTRACT 1 FROM COUNTER-CPYBK
                       SUBTRACT 1 FROM COUNTER-NESTED-FLAG 
                     END-IF 

                  WHEN ":"
                     IF IS-NAME
                       COMPUTE COUNTER-END = COUNTER - COUNTER-INIT
                       PERFORM 1020-INSERT-NAME-VALUE
                       SET IS-VALUE TO TRUE
                     END-IF 

                  WHEN ","
                     IF IS-VALUE
                       IF COUNTER-END EQUAL ZERO 
                          COMPUTE COUNTER-END = COUNTER - COUNTER-INIT
                          PERFORM 1020-INSERT-NAME-VALUE
                       END-IF
                     END-IF 

                  WHEN "."
                     IF IS-INT 
                       SET IS-LONG TO TRUE
                     END-IF
                  
                  WHEN '"'
                    IF IS-NONE-TYPE 
                       SET IS-STRING TO TRUE 
                       COMPUTE COUNTER-INIT = COUNTER + 1
                    ELSE IF IS-STRING
                       COMPUTE COUNTER-2 = COUNTER - 1
                       IF JSON-STRING(COUNTER-2: 1) NOT EQUAL "\"
                          COMPUTE COUNTER-END = COUNTER - COUNTER-INIT
                          PERFORM 1020-INSERT-NAME-VALUE
                       END-IF
                    END-IF

                  WHEN NOT SPACE 
                     IF IS-VALUE AND COUNTER-INIT EQUAL ZERO
                       SET IS-INT TO TRUE
                       MOVE COUNTER TO COUNTER-INIT
                     ELSE IF IS-NONE
                       SET IS-NAME TO TRUE
                       MOVE COUNTER TO COUNTER-INIT
                     END-IF
              END-EVALUATE           

           END-PERFORM
           .

       1010-VERIFY-NAME-TABLE.
           IF JSON-TABLE-NAME(COUNTER-CPYBK) EQUAL SPACES 
              MOVE JSON-TEMP-NAME TO 
                                   JSON-TABLE-NAME(COUNTER-CPYBK)
           ELSE
              PERFORM VARYING COUNTER-2 FROM 1 BY 1 
                       UNTIL ITEM-NESTED-NAME(COUNTER-CPYBK COUNTER-2) 
                                                           EQUAL SPACES
                     CONTINUE 
               END-PERFORM

               MOVE JSON-TEMP-NAME TO 
                              ITEM-NESTED-NAME(COUNTER-CPYBK COUNTER-2)
               SET ITEM-NESTED-VALUE(COUNTER-CPYBK COUNTER-2) TO ADDRESS 
                                     OF JSON-TABLE(COUNTER-CPYBK + 1)
               SET NESTED-FATHER(COUNTER-CPYBK + 1) TO ADDRESS OF 
                                         JSON-TABLE(COUNTER-CPYBK)
                             
               ADD 1 TO COUNTER-CPYBK
               ADD 1 TO COUNTER-NESTED-FLAG
               SET YES-NESTED(COUNTER-CPYBK) TO TRUE
               MOVE JSON-TEMP-NAME TO 
                                     JSON-TABLE-NAME(COUNTER-CPYBK)
             
           END-IF

           SET IS-NONE TO TRUE     
           SET IS-NONE-TYPE TO TRUE
           MOVE 0 TO COUNTER-INIT 
           MOVE 0 TO COUNTER-END
           .

       1020-INSERT-NAME-VALUE.
           IF IS-NAME 
              MOVE SPACES TO JSON-TEMP-NAME
              MOVE JSON-STRING(COUNTER-INIT:COUNTER-END) TO  
                                            JSON-TEMP-NAME
           ELSE
              EVALUATE VALUE-TYPE
                 WHEN 1 
                    PERFORM 1021-INSERT-STRING

                 WHEN 2
                    PERFORM 1022-INSERT-INT

                 WHEN 3 
                    PERFORM 1023-INSERT-LONG

              END-EVALUATE

           END-IF

           SET IS-NONE TO TRUE     
           SET IS-NONE-TYPE TO TRUE
           MOVE 0 TO COUNTER-INIT 
           MOVE 0 TO COUNTER-END
           .
      
       1021-INSERT-STRING.
           PERFORM VARYING COUNTER-2 FROM 1 BY 1 UNTIL 
                 ITEM-STRING-NAME(COUNTER-CPYBK COUNTER-2) EQUAL SPACES
              CONTINUE
           END-PERFORM

           MOVE JSON-TEMP-NAME TO 
                              ITEM-STRING-NAME (COUNTER-CPYBK COUNTER-2)
           MOVE JSON-STRING(COUNTER-INIT:COUNTER-END) TO 
                            ITEM-STRING-VALUE (COUNTER-CPYBK COUNTER-2).
             
       1022-INSERT-INT.
           PERFORM VARYING COUNTER-2 FROM 1 BY 1 UNTIL
             ITEM-INT-NAME(COUNTER-CPYBK COUNTER-2) EQUAL SPACES
               CONTINUE
           END-PERFORM
           MOVE JSON-TEMP-NAME TO
                                  ITEM-INT-NAME(COUNTER-CPYBK COUNTER-2)
           MOVE JSON-STRING(COUNTER-INIT:COUNTER-END) TO
                                ITEM-INT-VALUE(COUNTER-CPYBK COUNTER-2).

       1023-INSERT-LONG.
           PERFORM VARYING COUNTER-2 FROM 1 BY 1 UNTIL
             ITEM-LONG-NAME(COUNTER-CPYBK COUNTER-2) EQUAL SPACES
               CONTINUE
           END-PERFORM
           MOVE JSON-TEMP-NAME TO
                                ITEM-LONG-NAME(COUNTER-CPYBK COUNTER-2)
           MOVE JSON-STRING(COUNTER-INIT:COUNTER-END) TO
                               ITEM-LONG-VALUE(COUNTER-CPYBK COUNTER-2).
