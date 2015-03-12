      ******************************************************************
      *PROGRAM : COURSE-INQ                                            *
      *AUTHOR  : CHIRS JEONG                                           *
      *DATE    : 02/26/2015                                            *
      *ABSTRACT: COURSE INQ. FOR COURSE ACTIONS                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-INQ.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       
      *-----------------------------------------------------------------
       DATA DIVISION.
       
      *-----------------------------------------------------------------
       FILE SECTION.
       
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-STORAGE.
           05  WS-COURSE           PIC X(8).
       
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
           03  LINE 1 COL 1  VALUE "COURSE-INQ".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  INSTRUCTORMENU.
           03  MENU.
               05  LINE 07 COL 32 VALUE "COURSE SEARCH".
               05  LINE 09 COL 36 VALUE "COURSE".
               05  LINE 10 COL 36 VALUE "NAME".
               05  LINE 11 COL 36 VALUE "NAME2".
               05  LINE 12 COL 36 VALUE "HRS".
               05  LINE 17 COL 37 VALUE "Selection (X = EXIT)".
               05  LINE 17 COL 35 PIC X TO WS-SELECTION AUTO.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           MOVE SPACES TO WS-SELECTION.
           PERFORM 200-MAIN-LOOP UNTIL WS-SELECTION = 'X' OR 'x'.
       
           EXIT PROGRAM.
            
       
      *-----------------------------------------------------------------
       200-MAIN-LOOP.
       
           DISPLAY SCR-TITLE
           DISPLAY INSTRUCTORMENU
           ACCEPT INSTRUCTORMENU


      
       
      *----------------------------------------------------------------- 