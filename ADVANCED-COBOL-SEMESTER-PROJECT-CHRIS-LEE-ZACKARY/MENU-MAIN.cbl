       
      ******************************************************************
      *PROGRAM : MAIN-MENU                                             *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 02/24/2015                                            *
      *ABSTRACT: MAIN MENU FOR SEMESTER PROJECT                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID MAIN-MENU.
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
           03  LINE 1 COL 1  VALUE "BUILDING-MENU".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM  DISPLAY-DATE.
           
       01  MENUSCREEN.
           03  MENU.
               05  LINE 01 COL 01 VALUE "MENU-MAIN".
               05  LINE 07 COL 32 VALUE " STUDENT SYSTEM".
               05  LINE 09 COL 32 VALUE " 1) STUDENT".
               05  LINE 10 COL 32 VALUE " 2) COURSE".
               05  LINE 11 COL 32 VALUE " 3) SCHEDULE".
               05  LINE 12 COL 32 VALUE " 4) INSTRUCTOR".
               05  LINE 13 COL 32 VALUE " 5) BUILDING".
               05  LINE 14 COL 32 VALUE " 6) REPORTS".
               05  LINE 15 COL 32 VALUE " 7) REGISTER ".
               05  LINE 17 COL 37 VALUE "Selection (X = EXIT)".
               05  LINE 17 COL 35 PIC X TO WS-SELECTION AUTO.
       
       01  EXIT-SCREEN.
           03  LINE 20 COL 33 "CONFIRM EXIT (Y/y)".
           03  LINE 20 COL 31 PIC X TO WS-EXIT AUTO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
       
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
       
           MOVE SPACES TO WS-SELECTION.
           PERFORM UNTIL WS-SELECTION = 'X' OR 'x'
               DISPLAY SCR-TITLE
               DISPLAY MENUSCREEN
               ACCEPT MENUSCREEN
               EVALUATE WS-SELECTION
                   WHEN '1' CALL 'MENU-STUDENT'
                   WHEN '2' CALL 'MENU-COURSE'
                   WHEN '3' CALL 'MENU-SCHEDULE'
                   WHEN '4' CALL 'MENU-INSTRUCTOR'
                   WHEN '5' CALL 'MENU-BUILDING'
                   WHEN '6' CALL 'MENU-REPORTS'
                   WHEN '7' CALL 'MENU-REGISTER'
                   WHEN '9' CALL 'MENU-REBUILD'
               END-EVALUATE
           END-PERFORM
           
           STOP RUN.
                   
               