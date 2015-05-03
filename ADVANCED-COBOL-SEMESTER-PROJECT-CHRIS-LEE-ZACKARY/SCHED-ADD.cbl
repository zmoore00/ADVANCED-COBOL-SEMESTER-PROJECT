      ******************************************************************
      *PROGRAM : BLDG-INQ.CBL                                          *
      *AUTHOR  : ZACK MOORE                                            *
      *DATE    : 4/20/2015                                             *
      *ABSTRACT: FINDS A RECORD IN SCHEDULE MASTER BY CRN SEM AND YR   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHED-ADD IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-SCHED-IO ASSIGN TO "../SCHEDULE-MASTER.DAT"      
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC
                               RECORD KEY    IS CRN-KEY=ISAM-IO-KEY CRN 
                               ALTERNATE KEY IS CRSE-KEY=ISAM-IO-KEY
                                   CRSE
                                   WITH DUPLICATES
                               ALTERNATE KEY IS INSTRUC-KEY=ISAM-IO-KEY
                                   INSTRUCTOR
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-STAT.
           SELECT IO-REC       ASSIGN TO "../SCHED-LAST-CRN.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-SCHED-IO.
       01  ISAM-REC-IO.
           03  ISAM-IO-KEY.
               05  YEAR            PIC XXXX.
               05  SEMESTER        PIC XX.
           03  CRN                 PIC X(4).
           03  FILLER              PIC XX.
           03  SUBJ                PIC X(4).
           03  FILLER              PIC X           VALUE SPACES.
           03  CRSE                PIC X(5).
           03  FILLER              PIC X           VALUE SPACES.
           03  TIME-DAY            PIC X(20).
           03  BLDG                PIC X(6).
           03  FILLER              PIC X           VALUE SPACES.
           03  ROOM                PIC X(5).
           03  FILLER              PIC X           VALUE SPACES.
           03  INSTRUCTOR          PIC X(22).
           
       FD  IO-REC.
       01  LAST-CRN          PIC 9(4).
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
           03  WS-CONT                 PIC X.
           03  WS-EOF                  PIC X       VALUE 'N'.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-YEAR            PIC XXXX.
               05  WS-SEMESTER        PIC XX.
           03  WS-CRN                 PIC X(4).
           03  FILLER                 PIC XX.
           03  WS-SUBJ                PIC X(4).
           03  FILLER                 PIC X           VALUE SPACES.
           03  WS-CRSE                PIC X(5).
           03  FILLER                 PIC X           VALUE SPACES.
           03  WS-TIME-DAY            PIC X(20).
           03  WS-BLDG                PIC X(6).
           03  FILLER              PIC X           VALUE SPACES.
           03  WS-ROOM                PIC X(5).
           03  FILLER              PIC X           VALUE SPACES.
           03  WS-INSTRUCTOR          PIC X(22).
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "SCHED-ADD".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCRN-KEY-REQ.
           05  LINE 07 COL 32 VALUE "ADD TO SCHEDULE".
           03  LINE 10 COL 30                        VALUE '    SEM:'.
           03  LINE 10 COL 45 PIC XX TO WS-SEMESTER AUTO.
           03  LINE 11 COL 30                        VALUE '    YR:'.   
           03  LINE 11 COL 45 PIC XXXX TO WS-YEAR AUTO.
           03  LINE 12 COL 30                        VALUE '    SUBJ:'.
           03  LINE 12 COL 45 PIC X(4) TO WS-SUBJ.
           03  LINE 13 COL 30                        VALUE '    CRSE:'.
           03  LINE 13 COL 45 PIC X(5) TO WS-CRSE.
           03  LINE 14 COL 30                  VALUE '    TIME/DAY:'.
           03  LINE 14 COL 45 PIC X(20) TO WS-TIME-DAY.
           03  LINE 15 COL 30                  VALUE '    BLDG:'.
           03  LINE 15 COL 45 PIC X(6) TO WS-BLDG.
           03  LINE 16 COL 30                  VALUE '    ROOM:'.
           03  LINE 16 COL 45 PIC X(5) TO WS-ROOM.
           03  LINE 17 COL 30                  VALUE '    INSTRUC:'.
           03  LINE 17 COL 45 PIC X(20) TO WS-INSTRUCTOR.
           03  LINE 19 COL 35 PIC X(40) FROM WS-MSG.
           
       01  SCRN-DATA-TITLE.
           05  LINE 07 COL 35 VALUE "RECORD FOUND".
           
       01  SCRN-CONFIRM-ADD.
           03  LINE 21 COL 35                    VALUE 
               'ARE YOU SURE YOU WANT TO ADD'.
           03  LINE 22 COL 35 PIC 9(4) FROM CRN.
           03  LINE 22 COL 43 PIC XX FROM SEMESTER.
           03  LINE 22 COL 46 PIC XXXX FROM YEAR.
           03  LINE 23 COL 35 PIC X TO WS-RESP AUTO.
           
       01  SCRN-ADD-ANOTHER.
           03  LINE 23 COL 33                  VALUE 'ENTER ANOTHER?:'.
           03  LINE 24 COL 33                     VALUE '(Y/N)'.
           03  LINE 23 COL 45 PIC X  TO WS-CONT   AUTO.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN I-O ISAM-SCHED-IO.
           
           PERFORM UNTIL WS-CONT EQUALS "N" OR "n"
               OPEN I-O IO-REC
               DISPLAY SCR-TITLE
               DISPLAY SCRN-KEY-REQ
               ACCEPT  SCRN-KEY-REQ
               
               PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ IO-REC
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   ADD 1 TO LAST-CRN GIVING LAST-CRN
                   MOVE LAST-CRN TO WS-CRN
                   REWRITE LAST-CRN
               END-PERFORM
               MOVE 'N' TO WS-EOF
               
               MOVE WS-REC TO ISAM-REC-IO
               DISPLAY SCRN-CONFIRM-ADD
               ACCEPT SCRN-CONFIRM-ADD
               IF WS-RESP EQUALS 'Y' OR 'y'
               WRITE ISAM-REC-IO
               END-IF
               
                DISPLAY SPACES AT LINE 21 COL 1
                DISPLAY SPACE AT LINE 22 COL 1
                DISPLAY SPACE AT LINE 23 COL 1

               
               DISPLAY SCRN-ADD-ANOTHER
               ACCEPT  SCRN-ADD-ANOTHER
               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
                   MOVE "ENTER Y OR N" TO WS-MSG
               END-PERFORM
               CLOSE IO-REC
           END-PERFORM.

           CLOSE ISAM-SCHED-IO.
           EXIT PROGRAM.
           STOP RUN.
