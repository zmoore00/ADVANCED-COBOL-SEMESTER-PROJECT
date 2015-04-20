      ******************************************************************
      *PROGRAM : INSTRUC-inq.CBL                                       *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 3/16/2015                                             *
      *ABSTRACT: This program lists from the INSTRUCTOR-MASTER FILE    *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-LIST IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-STUD-IN ASSIGN TO "../STUDENT-MASTER.DAT"  
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL    
                               RECORD KEY    IS ISAM-STUD-KEY
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-STUD-IN.
       01  ISAM-REC-IO.
           03  ISAM-STUD-KEY.
               05  ISAM-IO-ID      PIC 9(4).
           03  FILLER              PIC X.
           03  ISAM-STUD-LNAME     PIC X(15).
           03  ISAM-SUTD-FNAME     PIC X(15).
           03  ISAM-STUD-ADDRESS   PIC X(25).
           03  ISAM-STUD-ZIP       PIC X(5).
           03  ISAM-STUD-HPHONE    PIC X(10).
           03  ISAM-STUD-CPHONE    PIC X(10).
           03  ISAM-STUD-WPHONE    PIC X(10).
           03  ISAM-STUD-GENDER    PIC X.
           03  ISAM-STUD-ACTIVE    PIC X.
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
           03  BLANK-LINE              PIC X(80)   VALUE SPACES.
               
       01  WS-REC.
           03  WS-LINE-1.
               05  WS-KEY.
                   07  WS-STUD-ID       PIC 9999        VALUE 9999.
               05  FILLER               PIC X.
               05  WS-STUD-LNAME        PIC X(15).
               05  WS-SUTD-FNAME        PIC X(15).
               05  WS-STUD-ADDRESS      PIC X(25).
               05  WS-STUD-ZIP          PIC X(5).
           03  WS-LINE-2.
               05  WS-STUD-HPHONE.
                   07  WS-STUD-HPHONE1  PIC X(3).
                   07  WS-STUD-HPHONE2  PIC X(3).
                   07  WS-STUD-HPHONE3  PIC X(4).               
               05  WS-STUD-CPHONE.
                   07  WS-STUD-CPHONE1  PIC X(3).
                   07  WS-STUD-CPHONE2  PIC X(3).
                   07  WS-STUD-CPHONE3  PIC X(4).
               05  WS-STUD-WPHONE.
                   07  WS-STUD-WPHONE1  PIC X(3).
                   07  WS-STUD-WPHONE2  PIC X(3).
                   07  WS-STUD-WPHONE3  PIC X(4).
               05  WS-STUD-GENDER       PIC X.       
               05  WS-STUD-ACTIVE       PIC X.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "STUDENT-LIST".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
       
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN INPUT ISAM-STUD-IN.
           
           DISPLAY SCR-TITLE
           
           PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ ISAM-STUD-IN
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM 100-DISPLAY
           END-PERFORM
           
           DISPLAY SPACES
           DISPLAY 'PRESS ENTER TO CONTINUE, TYPE X TO EXIT'
           ACCEPT WS-RESP
           
           CLOSE ISAM-STUD-IN.
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

               MOVE ISAM-REC-IO     TO WS-REC.

               DISPLAY WS-LINE-1
               DISPLAY '('WS-STUD-HPHONE1')'WS-STUD-HPHONE2'-'
                   WS-STUD-HPHONE3' '
                   '('WS-STUD-CPHONE1')'WS-STUD-CPHONE2'-'
                   WS-STUD-CPHONE3' '
                   '('WS-STUD-WPHONE1')'WS-STUD-WPHONE2'-'
                   WS-STUD-WPHONE3'  '
                   WS-STUD-GENDER' '   
                   WS-STUD-ACTIVE
                   
               DISPLAY BLANK-LINE.

