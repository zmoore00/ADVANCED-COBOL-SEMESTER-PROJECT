      ******************************************************************
      *PROGRAM : INSTRUC-inq.CBL                                       *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 3/16/2015                                             *
      *ABSTRACT: This program lists from the INSTRUCTOR-MASTER FILE    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-LIST IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-COURSE-IN ASSIGN TO "../COURSE-MASTER.TXT"      
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL    
                               RECORD KEY    IS ISAM-IN-KEY
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-COURSE-IN.
       01  ISAM-REC-IO.
           05  ISAM-IN-KEY.
               10  ISAM-IO-SUBJ    PIC X(4).
               10  ISAM-IO-CRSE    PIC X(5).
           05  FILLER              PIC X            VALUE SPACE.
           05  ISAM-IO-TITLE       PIC X(30).
           05  FILLER              PIC X            VALUE SPACE.
           05  ISAM-IO-CREDITS     PIC X(3). 
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
           05  WS-KEY.
               10  WS-COURSE-SUBJ PIC X(4)              VALUE SPACES.
               10  WS-COURSE-CRSE PIC X(5)              VALUE SPACES.
           05  FILLER              PIC X            VALUE SPACE.
           05  WS-COURSE-TITLE     PIC X(30).
           05  FILLER              PIC X                VALUE SPACE.
           05  WS-COURSE-CREDITS  PIC X(3).
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "COURSE-LIST".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN INPUT ISAM-COURSE-IN.
           
           DISPLAY SCR-TITLE
           
           PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ ISAM-COURSE-IN
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM 100-DISPLAY
           END-PERFORM
           
           DISPLAY SPACES
           DISPLAY 'PRESS ENTER TO CONTINUE, TYPE X TO EXIT'
           ACCEPT WS-RESP

           CLOSE ISAM-COURSE-IN.
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
               MOVE ISAM-IO-TITLE   TO WS-COURSE-TITLE.
               MOVE ISAM-IO-CREDITS TO WS-COURSE-CREDITS.

               DISPLAY WS-REC.


                   
               
               
