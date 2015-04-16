      ******************************************************************
      *PROGRAM : INSTRUC-inq.CBL                                       *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 3/16/2015                                             *
      *ABSTRACT: This program lists from the INSTRUCTOR-MASTER FILE    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BLDG-LIST IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-BUILD-IN ASSIGN TO "../BUILDING-ISAM.DAT"      
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL    
                               RECORD KEY    IS ISAM-IN-KEY
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-BUILD-IN.
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
           03  WS-ANOTHER              PIC X       VALUE "Y".
           03  WS-EOF                  PIC X       VALUE "N".
           03  WS-CTR                  PIC 99      VALUE ZEROS.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-BLDG     PIC X(7)        VALUE SPACES.
               05  WS-ROOM     PIC X(5)        VALUE SPACES.
           03  WS-SEATS        PIC X(4)        VALUE SPACES.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "INDTRUC-LIST".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN INPUT ISAM-BUILD-IN.
           
           DISPLAY SCR-TITLE
           
           PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ ISAM-BUILD-IN
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM 100-DISPLAY
           END-PERFORM

           CLOSE ISAM-BUILD-IN.
           EXIT PROGRAM.
           STOP RUN.
      *-----------------------------------------------------------------
       100-DISPLAY.
           ADD  1          TO WS-CTR
           
           IF WS-CTR GREATER THAN 20
               DISPLAY SPACES
               DISPLAY 'PRESS ENTER TO CONTINUE, TYPE X TO EXIT'
               ACCEPT WS-RESP
               IF WS-RESP EQUALS 'X' OR 'x'
                   EXIT PROGRAM
               END-IF
               DISPLAY BLANK-SCREEN
               DISPLAY SCR-TITLE
               DISPLAY SPACES
               MOVE 1 TO WS-CTR.

               MOVE ISAM-IN-KEY     TO WS-KEY.
               MOVE ISAM-IN-SEATS   TO WS-SEATS.

               DISPLAY WS-REC.

