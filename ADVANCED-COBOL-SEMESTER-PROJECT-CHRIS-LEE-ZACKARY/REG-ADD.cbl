      ******************************************************************
      *PROGRAM : BLDG-ADD.CBL                                          *
      *AUTHOR  : Lee Hawthorne                                         *
      *DATE    : 2/17/2015                                             *
      *ABSTRACT: This program adds to the BUILDING-ISAM.DAT FILE       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG-ADD IS INITIAL PROGRAM.
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
                               
           SELECT ISAM-STUD-IN  ASSIGN TO "../STUDENT-MASTER.DAT"     
                                ORGANIZATION  IS INDEXED
                                ACCESS        IS RANDOM    
                                RECORD KEY    IS ISAM-STUD-KEY
                                FILE STATUS   IS WS-STAT2.
           
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
       FD  ISAM-STUD-IN.
       01  STUD-REC-IN.
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
               05  REG-IO-CRN     PIC X(6).
               05  REG-IO-STUD-ID PIC 9(4).
      *----------------------------------------------------------------- 
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-MSG                  PIC X(40)   VALUE SPACES.
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  WS-STAT2                PIC XX      VALUE SPACES.
           03  WS-STAT3                PIC XX      VALUE SPACES.
           03  WS-CONT                 PIC X       VALUE 'Y'.
           
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
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-SEM      PIC X(2)        VALUE SPACES.
               05  WS-YR       PIC X(4)        VALUE SPACES.
               05  WS-CRN      PIC X(4)        VALUE SPACES.
               05  WS-STU-ID   PIC 9(4)        VALUE ZEROS.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           05  LINE 07 COL 32 VALUE "REGISTRATION ADD".
           03  LINE 1  COL 1  VALUE "REG-ADD".
           03  LINE 1  COL 37 VALUE "UAFS".
           03  LINE 1  COL 71 FROM DISPLAY-DATE.
           
       01  SCRN-SEM-REQ.
           03  LINE 09 COL 35                       VALUE '    SEM/YR:'.
           03  LINE 09 COL 46 PIC X(2)  TO WS-SEM   AUTO.
           03  LINE 09 COL 48 VALUE '/'.
           03  LINE 09 COL 49 PIC X(4)  TO WS-YR   AUTO.
           03  LINE 16 COL 35 PIC X(40) FROM WS-MSG.
           
       01  SCRN-CRN-REQ.
           03  LINE 10 COL 35                       VALUE '       CRN:'.
           03  LINE 10 COL 46 PIC X(6)  TO WS-CRN   AUTO.
           
       01  SCRN-STUD-ID-REQ.
           03  LINE 11 COL 35                       VALUE 'STUDENT ID:'.
           03  LINE 11 COL 46 PIC X(4)  TO WS-STU-ID AUTO.
           
       01  SCRN-SCHED-DATA.
           03  LINE 12 COL 10           FROM SCHED-REC-IN.
           
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
           OPEN I-O ISAM-STUD-IN.
           
           DISPLAY BLANK-SCREEN
           PERFORM UNTIL WS-CONT='n' OR 'N'
               MOVE SPACES TO WS-CONT
               DISPLAY SCR-TITLE
               DISPLAY SCRN-SEM-REQ
               DISPLAY SCRN-CRN-REQ
               DISPLAY SCRN-STUD-ID-REQ
               ACCEPT  SCRN-SEM-REQ
               ACCEPT  SCRN-CRN-REQ
               ACCEPT  SCRN-STUD-ID-REQ
               MOVE WS-KEY TO REG-IO-KEY
               READ ISAM-REG-IO
                   INVALID KEY
                       WRITE REG-REC-IO
                           INVALID KEY
                               MOVE   'INVALID ID' TO WS-MSG
                           NOT INVALID KEY
                               MOVE WS-SEM TO SEMESTER
                               MOVE WS-YR  TO YEAR
                               MOVE WS-CRN TO CRN
        
                               READ ISAM-SCHED-IN
                                   INVALID KEY
                                       MOVE 
                                        'INVALID SCHEDULE ID' TO WS-MSG
                                   NOT INVALID KEY
                                       DISPLAY SCRN-SCHED-DATA
                               END-READ
                                   
                               STRING REG-IO-KEY ' ADDED' INTO WS-MSG
                       END-WRITE
                       DISPLAY SCRN-ADD-ANOTHER
                       ACCEPT  SCRN-ADD-ANOTHER
                   NOT INVALID KEY
                       MOVE   'ID ALREADY EXISTS' TO WS-MSG
               END-READ
               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-SEM-REQ
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
               END-PERFORM
               
           END-PERFORM.
           
          
           
           EXIT PROGRAM.
           STOP RUN.
