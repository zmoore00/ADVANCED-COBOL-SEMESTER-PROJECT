              
      ******************************************************************
      *PROGRAM : MENU-INSTRUCTOR                                       *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 02/24/2015                                            *
      *ABSTRACT: MENU FOR INSTRUCTOR ACTIONS                           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID MENU-REPORTS IS INITIAL PROGRAM.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
      *-----------------------------------------------------------------
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
       
       
       01  WS-VARS.
           03  WS-SELECTION                PIC X.
           03  WS-EXIT                     PIC X       VALUE 'N'.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "REPORTS-MENU".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  INSTRUCTORMENU.
           03  MENU.
               05  LINE 01 COL 01 VALUE "MENU-REPORTS".
               05  LINE 07 COL 32 VALUE " REPORTS MENU".
               05  LINE 09 COL 32 VALUE " 1) STUDENT SCHEDULE".
               05  LINE 10 COL 32 VALUE " 2) INSTRUCTOR SCHEDULE".
               05  LINE 11 COL 32 VALUE " 3) CLASS ROLE".
               05  LINE 12 COL 32 VALUE " 4) MASTER LIST".
               05  LINE 17 COL 37 VALUE "Selection (X = EXIT)".
               05  LINE 17 COL 35 PIC X TO WS-SELECTION AUTO.
       
       01  EXIT-SCREEN.
           03  LINE 20 COL 33 "CONFIRM EXIT (Y/N)".
           03  LINE 20 COL 31 PIC X TO WS-EXIT AUTO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           MOVE SPACES TO WS-SELECTION.
           PERFORM UNTIL WS-EXIT = 'Y' OR 'y'
               DISPLAY SCR-TITLE
               DISPLAY INSTRUCTORMENU
               ACCEPT INSTRUCTORMENU
               EVALUATE WS-SELECTION
                   WHEN '1' CALL 'REPORTS-STUD-SCHED'
                   WHEN '2' CALL 'REPORTS-INSTRUC-SCHED'
                   WHEN '3' CALL 'REPORTS-CLASS-ROLE'
                   WHEN '4' CALL 'REPORTS-MASTER-LIST'
               END-EVALUATE
               IF WS-SELECTION = 'X' OR 'x'
                   DISPLAY EXIT-SCREEN
                   ACCEPT EXIT-SCREEN
               END-IF
           END-PERFORM
       END PROGRAM MENU-REPORTS.