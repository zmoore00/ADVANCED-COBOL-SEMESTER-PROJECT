      ******************************************************************
      *PROGRAM : BLDG-INQ.CBL                                          *
      *AUTHOR  : Lee Hawthorne                                         *
      *DATE    : 2/17/2015                                             *
      *ABSTRACT: This program inquires into the BUILDING-ISAM.DAT FILE *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTS-STUD-SCHED INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-SCHED-IN ASSIGN TO "../SCHEDULE-MASTER.DAT"
                                ORGANIZATION  IS INDEXED
                                ACCESS        IS DYNAMIC
                               RECORD KEY    IS CRN-KEY=ISAM-IN-KEY CRN
                               ALTERNATE KEY IS CRSE-KEY=ISAM-IN-KEY
                                   CRSE
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-STAT3.
       
       SELECT OPTIONAL ISAM-REG-IO   ASSIGN TO "../REG-ISAM.DAT"            
                                ORGANIZATION  IS INDEXED
                                ACCESS        IS DYNAMIC
                                RECORD KEY    IS REG-IO-KEY
                                ALTERNATE KEY IS REG-STUD-ID-KEY=
                                   REG-IO-SEM, REG-IO-YR, REG-IO-STUD-ID
                                   WITH DUPLICATES
                                ALTERNATE KEY IS REG-CRN-KEY=
                                   REG-IO-SEM, REG-IO-YR, REG-IO-CRN
                                   WITH DUPLICATES
                                FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-SCHED-IN.
       01  SCHED-REC-IN.
           03  ISAM-IN-KEY.
               05  YEAR            PIC XXXX.
               05  SEMESTER        PIC XX.
           03  CRN                 PIC X(4).
           03  FILLER              PIC X           VALUE SPACES.
           03  SUBJ                PIC X(5).
           03  CRSE                PIC X(5).
           03  TIME-DAY            PIC X(20).
           03  BLDG                PIC X(7).
           03  ROOM                PIC X(6).
           03  INSTRUCTOR          PIC X(22).
           
       FD  ISAM-REG-IO.
       01  REG-REC-IO.
           03  REG-IO-KEY.
               05  REG-IO-SEM     PIC X(2).
               05  REG-IO-YR      PIC X(4).
               05  REG-IO-CRN     PIC X(4).
               05  REG-IO-STUD-ID PIC 9(4).
           
           
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
           03  WS-STAT3                PIC XX      VALUE SPACES.
           03  WS-CONT                 PIC X       VALUE 'Y'.
           03  EOF-FLAG                PIC X.
               88 EOF        VALUE '1'.
           03  WS-COUNT                PIC 9       VALUE 0.
           
       01  WS-REC.
           03  WS-KEY.
               05  WS-SEM      PIC X(2)        VALUE SPACES.
               05  WS-YR       PIC X(4)        VALUE SPACES.
               05  WS-CRN      PIC X(4)        VALUE SPACES.
               05  WS-STU-ID   PIC X(4)        VALUE ZEROS.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "REPORTS-STUD-SCHED".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCRN-SEM-REQ.
           03  LINE 09 COL 35                       VALUE '    SEM/YR:'.
           03  LINE 09 COL 46 PIC X(2)  TO WS-SEM   AUTO.
           03  LINE 09 COL 48 VALUE '/'.
           03  LINE 09 COL 49 PIC X(4)  TO WS-YR   AUTO.
           03  LINE 16 COL 35 PIC X(40) FROM WS-MSG.
           
       01  SCRN-STUD-ID-REQ.
           03  LINE 11 COL 35                       VALUE 'STUDENT ID:'.
           03  LINE 11 COL 46 PIC X(4)  TO WS-STU-ID AUTO.
           
       01  SCRN-ADD-ANOTHER.
           03  LINE 14 COL 33                     VALUE 'ADD ANOTHER?:'.
           03  LINE 15 COL 33                     VALUE '(Y/N)'.
           03  LINE 15 COL 45 PIC X  TO WS-CONT   AUTO.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
                   
           OPEN I-O ISAM-REG-IO.
           OPEN I-O ISAM-SCHED-IN.
           DISPLAY BLANK-SCREEN
           
           PERFORM UNTIL WS-CONT='n' OR 'N'
               MOVE SPACES TO WS-CONT
               DISPLAY SCR-TITLE
               DISPLAY SCRN-SEM-REQ
               DISPLAY SCRN-STUD-ID-REQ
               ACCEPT  SCRN-SEM-REQ
               ACCEPT  SCRN-STUD-ID-REQ
               MOVE WS-SEM TO REG-IO-SEM
               MOVE WS-YR TO REG-IO-YR
               MOVE WS-STU-ID TO REG-IO-STUD-ID
               READ ISAM-REG-IO KEY IS REG-STUD-ID-KEY
                  INVALID KEY
                       MOVE 'INVALID ID' TO WS-MSG
                  NOT INVALID KEY
                   MOVE REG-IO-SEM TO SEMESTER
                   MOVE REG-IO-YR TO YEAR
                   MOVE REG-IO-CRN TO CRN
                  READ ISAM-SCHED-IN
                      INVALID KEY
                          MOVE 'BAD RECORD' TO WS-MSG
                      NOT INVALID KEY
                          DISPLAY SCHED-REC-IN
                  END-READ
                  PERFORM UNTIL WS-COUNT EQUALS 8
                      
                       READ ISAM-REG-IO NEXT RECORD
                           AT END
                               MOVE 1 TO EOF-FLAG
                               
                           NOT AT END
                               MOVE REG-IO-SEM TO SEMESTER
                               MOVE REG-IO-YR TO YEAR
                               MOVE REG-IO-CRN TO CRN
                               
                               READ ISAM-SCHED-IN
                                   INVALID KEY
                                       MOVE 'BAD RECORD' TO WS-MSG
                                   NOT INVALID KEY
                                       DISPLAY SCHED-REC-IN
                                       DISPLAY 'END'
                                       ACCEPT WS-MSG
                               END-READ
                       END-READ
                       ADD 1 TO WS-COUNT GIVING WS-COUNT
                  END-PERFORM
               END-READ
                   DISPLAY SPACES
                      DISPLAY REG-IO-SEM, REG-IO-YR, REG-IO-STUD-ID
                      DISPLAY WS-STAT
           ACCEPT WS-MSG
               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-SEM-REQ
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
               END-PERFORM
               
           END-PERFORM.