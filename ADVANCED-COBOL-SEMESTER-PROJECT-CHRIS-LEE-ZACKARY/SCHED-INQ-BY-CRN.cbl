      ******************************************************************
      *PROGRAM : BLDG-INQ.CBL                                          *
      *AUTHOR  : Lee Hawthorne                                         *
      *DATE    : 2/17/2015                                             *
      *ABSTRACT: This program inquires into the BUILDING-ISAM.DAT FILE *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHED-INQ-BY-CRN IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-BLDG-IN ASSIGN TO "../BUILDING-ISAM.DAT"         
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM    
                               RECORD KEY    IS ISAM-IN-KEY
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-BLDG-IN.
       01  ISAM-REC-IN.
           03  ISAM-IN-KEY.
               05  ISAM-IN-BLDG PIC X(7).
               05  ISAM-IN-ROOM PIC X(5).
           03  ISAM-IN-SEATS    PIC X(4).
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
       
       01  MISC-VARS.
           03  WS-MSG                  PIC X(40)   VALUE SPACES.
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  CONT-FLAG               PIC X       VALUE 'Y'.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-BLDG     PIC X(7)        VALUE SPACES.
               05  WS-ROOM     PIC X(5)        VALUE SPACES.
           03  WS-SEATS        PIC X(4)        VALUE SPACES.
           03  WS-ANOTHER      PIC X.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "SCHED-INQ-BY-CRN".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCRN-KEY-REQ.
           05  LINE 07 COL 32 VALUE "SCHEDULE SEARCH BY CRN".
           03  LINE 09 COL 35                       VALUE '     CRN:'.
           03  LINE 09 COL 45 PIC X(7)  TO WS-BLDG  AUTO.
           03  LINE 10 COL 35                       VALUE '  SEM/YR:'. 
           03  LINE 10 COL 45 PIC X(5)  TO WS-ROOM  AUTO.
           03  LINE 12 COL 35                       VALUE '  (X=EXIT)'.
           03  LINE 13 COL 35 PIC X(40) FROM WS-MSG.
           
       01  SCRN-BLDG-DATA.
           03  LINE 09 COL 35                        VALUE ' CRN:'.     
           03  LINE 09 COL 45 PIC X(7) FROM WS-BLDG  VALUE SPACES.
           03  LINE 10 COL 35                        VALUE '   SEM/YR:'.
           03  LINE 10 COL 45 PIC X(5) FROM WS-ROOM  VALUE SPACES.
           03  LINE 11 COL 35                        VALUE 'MAX SEATS:'.
           03  LINE 11 COL 45 PIC XXXX FROM WS-SEATS VALUE SPACES.
           03  LINE 13 COL 45                        VALUE'Y/N? '.
           03  LINE 14 COL 45 PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN INPUT ISAM-BLDG-IN.
           
           PERFORM UNTIL (WS-BLDG='X' OR 'x') OR (WS-ROOM='X' OR 'x')
               DISPLAY SCR-TITLE
               DISPLAY SCRN-KEY-REQ
               ACCEPT  SCRN-KEY-REQ
               MOVE WS-KEY TO ISAM-IN-KEY
               READ ISAM-BLDG-IN
                   INVALID KEY
                       
                   NOT INVALID KEY
                       
               END-READ
           END-PERFORM.

           CLOSE ISAM-BLDG-IN.
           EXIT PROGRAM.
           STOP RUN.