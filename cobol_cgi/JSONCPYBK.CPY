       01  JSON-CP-STRING.
         02 JSON-TABLE OCCURS 200 TIMES.
           03 IS-NESTED              PIC X VALUE 'N'.
             88 NON-NESTED           VALUE 'N'.
             88 YES-NESTED           VALUE 'Y'.
           03 NESTED-FATHER          USAGE IS POINTER.
           03 JSON-TABLE-NAME        PIC X(50) VALUE SPACES.
           03 ITEM-ID                PIC 9(9).
           03 ITEM-STRING OCCURS 100 TIMES.
              04 ITEM-STRING-NAME    PIC X(50) VALUE SPACES.
              04 ITEM-STRING-VALUE   PIC X(255).
           03 ITEM-INT OCCURS 100 TIMES.
              04 ITEM-INT-NAME    PIC X(50) VALUE SPACES.
              04 ITEM-INT-VALUE   PIC 9(12).
           03 ITEM-LONG OCCURS 100 TIMES.
              04 ITEM-LONG-NAME    PIC X(50) VALUE SPACES.
              04 ITEM-LONG-VALUE   PIC 9(12)V9(5).
           03 ITEM-NESTED OCCURS 100 TIMES.
              04 ITEM-NESTED-NAME    PIC X(50) VALUE SPACES.
              04 ITEM-NESTED-ID      PIC 9(9) VALUE ZEROS.
              04 ITEM-NESTED-VALUE USAGE POINTER.
