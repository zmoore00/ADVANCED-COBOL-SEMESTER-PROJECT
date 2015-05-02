      ******************************************************************
      *PROGRAM : INSTRUC-ADD.CBL                                       *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 3/16/2015                                             *
      *ABSTRACT: This program adds to the INSTRUCTOR-MASTER FILE       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSTRUC-ADD AS "INSTRUC-ADD" IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-INSTRUC-IO ASSIGN TO "../INSTRUCTOR-MASTER.DAT"  
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC    
                               RECORD KEY    IS ISAM-IO-KEY
                               FILE STATUS   IS WS-STAT.
           SELECT IO-REC       ASSIGN TO "../INSTRUC-LAST-ID.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-INSTRUC-IO.
       01  ISAM-REC-IO.
           03  ISAM-IO-KEY.
               05  ISAM-IO-ID   PIC 9999.
           03  FILLER       PIC X           VALUE SPACES.
           03  ISAM-IO-NAME PIC X(22).
           
       FD  IO-REC.
       01  LAST-ID          PIC 9999.
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
           03  WS-CONT                 PIC X       VALUE 'Y'.
           03  WS-EOF                  PIC X       VALUE 'N'.
           03  WS-COUNT                PIC 9       VALUE 0.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-INSTRUC-ID       PIC 9999        VALUE 9999.
               05  WS-FILLER           PIC X           VALUE SPACES.
               05  WS-INSTRUC-NAME     PIC X(22)       VALUE SPACES.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "INDTRUC-ADD".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-INSTRUC-NAME.
           03  LINE 07 COL 32 VALUE "ADD INSTRUCTOR".
           03  LINE 09 COL 32 VALUE 'NAME:'.
           03  LINE 09 COL 40 PIC X(22) TO WS-INSTRUC-NAME  AUTO.
           03  LINE 11 COL 35 PIC X(40) FROM WS-MSG.

       01  SCRN-CONFIRM-ADD.
           03  LINE 12 COL 35                    VALUE 
               'ARE YOU SURE YOU WANT TO ADD'.
           03  LINE 13 COL 35 PIC 9999 FROM LAST-ID.
           03  LINE 13 COL 43 PIC X(22) FROM WS-INSTRUC-NAME.
           03  LINE 14 COL 35 PIC X TO WS-RESP AUTO.
           
       01  SCRN-ADD-ANOTHER.
           03  LINE 13 COL 33                     VALUE 'ADD ANOTHER?:'.
           03  LINE 14 COL 33                     VALUE '(Y/N)'.
           03  LINE 13 COL 45 PIC X  TO WS-CONT   AUTO.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           
           OPEN I-O ISAM-INSTRUC-IO.
           DISPLAY BLANK-SCREEN
           
           
           
           
           
           PERFORM UNTIL WS-CONT='n' OR 'N'
               OPEN I-O IO-REC
               DISPLAY SCR-TITLE
               DISPLAY SCR-INSTRUC-NAME
               ACCEPT  SCR-INSTRUC-NAME
               
               PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ IO-REC
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   ADD 1 TO LAST-ID GIVING LAST-ID
                   MOVE LAST-ID TO WS-INSTRUC-ID
                   REWRITE LAST-ID
               END-PERFORM
               MOVE 'N' TO WS-EOF
               
               MOVE WS-KEY TO ISAM-REC-IO
               DISPLAY SCRN-CONFIRM-ADD
               ACCEPT SCRN-CONFIRM-ADD
               IF WS-RESP EQUALS 'Y' OR 'y'
               WRITE ISAM-REC-IO
               END-IF
               
               DISPLAY SPACES AT LINE 12 COL 1
               DISPLAY SPACE AT LINE 13 COL 1
               DISPLAY SPACE AT LINE 14 COL 1
               
               DISPLAY SCRN-ADD-ANOTHER
               ACCEPT  SCRN-ADD-ANOTHER
               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
                   MOVE "IN" TO WS-MSG
               END-PERFORM
             CLOSE IO-REC
           END-PERFORM.
           
           
           
           CLOSE ISAM-INSTRUC-IO.
           EXIT PROGRAM.
           STOP RUN.

