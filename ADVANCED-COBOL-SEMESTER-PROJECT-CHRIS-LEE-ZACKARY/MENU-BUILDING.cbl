       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUILDING-MENU AS "BUILDING-MENU" IS INITIAL PROGRAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DATE.
           05  WS-CURRENT-YEAR     PIC 9999.
           05  WS-CURRENT-MONTH    PIC 99.
           05  WS-CURRENT-DAY      PIC 99.
       01  DISPLAY-DATE.
           03  MONTH-DISPLAY       PIC 99.
           03  FILLER              PIC X           VALUE "/".
           03  DAY-DISPLAY         PIC 99.
           03  FILLER              PIC X           VALUE "/".
           03  YEAR-DISPLAY        PIC 9999.
       
       
       01  WS-SEL               PIC X.
       01  WS-CONFIRM-EXIT      PIC X VALUE 'N'.
       
       SCREEN SECTION.
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "BUILDING-MENU".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.

       01  SCR-MENU.
           03  LINE 01 COL 01 VALUE "MENU-BUILDING".
           03  LINE 07 COL 32 VALUE " BUILDING MENU".
           03  LINE 09 COL 32 VALUE " 1) INQ".
           03  LINE 10 COL 32 VALUE " 2) ADD".
           03  LINE 11 COL 32 VALUE " 3) CHANGE".
       01  SCR-SEL.
           03  LINE 17 COL 37 VALUE "Selection (X = EXIT)".
           03  LINE 17 COL 35 PIC X TO WS-SEL AUTO.

       01  SCR-CON.
           03  LINE 20 COL 33 "CONFIRM EXIT (Y/N)".
           03  LINE 20 COL 31 PIC X TO WS-CONFIRM-EXIT AUTO.
           
       PROCEDURE DIVISION.
       000-MAIN.
       
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
       

           PERFORM UNTIL WS-CONFIRM-EXIT IS EQUAL TO 'Y' OR 'y'
               DISPLAY SCR-TITLE
               DISPLAY SCR-MENU
               DISPLAY SCR-SEL
               ACCEPT  SCR-SEL
               EVALUATE WS-SEL
                   WHEN '1' CALL 'BLDG-INQ'
                   WHEN '2' CALL 'BLDG-ADD'
                   WHEN '3' CALL 'BLDG-CHANGE'
               END-EVALUATE
               IF WS-SEL EQUALS 'X' or 'x' THEN 
                   DISPLAY SCR-CON
                   ACCEPT  SCR-CON
                   DISPLAY SPACES LINE 12 ERASE LINE
               END-IF
           END-PERFORM
       END PROGRAM BUILDING-MENU.
