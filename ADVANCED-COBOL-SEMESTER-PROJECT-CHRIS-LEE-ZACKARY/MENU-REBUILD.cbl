       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU-REBUILD AS "MENU-REBUILD" IS INITIAL PROGRAM.

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
           03  LINE 1 COL 1  VALUE "MENU-REBUILD".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.

       01  SCR-MENU.
           03  LINE 01 COL 01 VALUE "MENU-REBUILD".
           03  LINE 07 COL 32 VALUE " REBUILD MENU".
           03  LINE 09 COL 32 VALUE " 1) REBUILD INSTRUCTORS".
           03  LINE 10 COL 32 VALUE " 2) REBUILD BUILDINGS".
           03  LINE 11 COL 32 VALUE " 3) REBUILD COURSES".
           03  LINE 12 COL 32 VALUE " 4) REBUILD STUDENTS".
           03  LINE 13 COL 32 VALUE " 5) REBUILD SCHEDULE".
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
                   WHEN '1' CALL 'INSTRUC-BUILD'
                   WHEN '2' CALL 'BLDG-BUILD'
                   WHEN '3' CALL 'COURSE-BUILD'
                   WHEN '4' CALL 'STUD-BUILD'
                   WHEN '5' CALL 'SCHED-BUILD'
               END-EVALUATE
               IF WS-SEL EQUALS 'X' OR 'x' THEN
                   DISPLAY SCR-CON
                   ACCEPT  SCR-CON
                   DISPLAY SPACES LINE 12 ERASE LINE
               END-IF
           END-PERFORM
       END PROGRAM MENU-REBUILD.
