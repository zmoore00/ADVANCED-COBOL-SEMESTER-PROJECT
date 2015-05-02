      ******************************************************************
      *PROGRAM : BLDG-INQ.CBL                                          *
      *AUTHOR  : Lee Hawthorne                                         *
      *DATE    : 2/17/2015                                             *
      *ABSTRACT: This program inquires into the BUILDING-ISAM.DAT FILE *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTS-CLASS-ROLE INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
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
                                
           SELECT ISAM-STUD-IO ASSIGN TO "../STUDENT-MASTER.DAT"     
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM   
                               RECORD KEY    IS ISAM-STUD-KEY
                               FILE STATUS   IS WS-STAT3.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FD  ISAM-REG-IO.
       01  REG-REC-IO.
           03  REG-IO-KEY.
               05  REG-IO-SEM     PIC X(2).
               05  REG-IO-YR      PIC X(4).
               05  REG-IO-CRN     PIC X(4).
               05  REG-IO-STUD-ID PIC 9(4).
               
       FD  ISAM-STUD-IO.
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
           03  Y                       PIC 99      VALUE 13.
           03  WS-MSG                  PIC X(40)   VALUE SPACES.
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  CONT-FLAG               PIC X       VALUE 'Y'.
           03  WS-STAT3                PIC XX      VALUE SPACES.
           03  WS-CONT                 PIC X       VALUE 'Y'.
           03  WS-CTR                  PIC 99      VALUE ZEROS.           
           03  EOF-FLAG                PIC X.
               88 EOF        VALUE '1'.
           03  WS-COUNT                PIC 99      VALUE 0.
           
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
           03  LINE 1 COL 1  VALUE "REPORTS-CLASS-ROLE".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCRN-SEM-REQ.
           03  LINE 09 COL 35                       VALUE '    SEM/YR:'.
           03  LINE 09 COL 46 PIC X(2)  TO WS-SEM   AUTO.
           03  LINE 09 COL 48 VALUE '/'.
           03  LINE 09 COL 49 PIC X(4)  TO WS-YR   AUTO.
           03  LINE 13 COL 35 PIC X(40) FROM WS-MSG.
           
       01  SCRN-STUD-ID-REQ.
           03  LINE 11 COL 35                       VALUE '      CRN:'.
           03  LINE 11 COL 46 PIC X(4)  TO WS-CRN AUTO.           
           
       01  SCRN-ADD-ANOTHER.
           03 SCRN-ADD-ANR-1 COL 33  VALUE 'CHECK ANOTHER?:'.
           03 SCRN-ADD-ANR-2 COL 33  VALUE '(Y/N)'.
           03 SCRN-ADD-ANR-3 COL 45 PIC X  TO WS-CONT AUTO.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN I-O ISAM-REG-IO.
           OPEN I-O ISAM-STUD-IO.
           
           DISPLAY BLANK-SCREEN
           PERFORM UNTIL WS-CONT='n' OR 'N'
               MOVE SPACES TO WS-MSG
               DISPLAY SCR-TITLE
               DISPLAY SCRN-SEM-REQ
               DISPLAY SCRN-STUD-ID-REQ
               ACCEPT  SCRN-SEM-REQ
               ACCEPT  SCRN-STUD-ID-REQ
               MOVE WS-SEM TO REG-IO-SEM
               MOVE WS-YR TO REG-IO-YR
               MOVE WS-CRN TO REG-IO-CRN
               READ ISAM-REG-IO KEY IS REG-CRN-KEY
                  INVALID KEY
                       MOVE 'INVALID ID' TO WS-MSG
                       DISPLAY SCRN-SEM-REQ
                  NOT INVALID KEY
                  ADD 2 TO Y
                  DISPLAY REG-IO-STUD-ID AT LINE Y COL 42

                  PERFORM UNTIL WS-COUNT EQUALS 30

                       READ ISAM-REG-IO NEXT RECORD
                           AT END
                               MOVE 1 TO EOF-FLAG
                           NOT AT END
                               IF WS-CRN EQUALS REG-IO-CRN
                               THEN
                                   DISPLAY REG-IO-STUD-ID " " 
                                   AT LINE Y COL 32
                                   END-DISPLAY
                                   MOVE REG-IO-STUD-ID TO ISAM-STUD-KEY
                                   READ ISAM-STUD-IO
                                       INVALID KEY
                                       NOT INVALID KEY
                                           
                                           DISPLAY 
                                           ISAM-STUD-KEY "  "           
                                           ISAM-STUD-LNAME 
                                               ISAM-SUTD-FNAME
                                   END-READ
                                   ADD 1 TO Y
                               ELSE
                                   MOVE 29 TO WS-COUNT
                               END-IF
                       END-READ
                       ADD 1 TO WS-COUNT GIVING WS-COUNT
                  END-PERFORM
               END-READ

               DISPLAY SPACES
               ADD 2 TO Y
               DISPLAY 'ADD ANOTHER?:' AT LINE Y COL 35
               ADD 1 TO Y
               DISPLAY '(Y/N)'         AT LINE Y COL 35
               SUBTRACT 1 FROM Y
               ACCEPT  WS-CONT AT LINE Y COL 48 AUTO
               SUBTRACT 1 FROM Y
               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   DISPLAY SPACES AT LINE Y COL 1
                   DISPLAY 'PLEASE ENTER Y OR N' AT LINE Y COL 35
                   ADD 1 TO Y
                   DISPLAY SPACES AT LINE Y COL 1
                   DISPLAY 'ADD ANOTHER?:' AT LINE Y COL 35
                   ADD 1 TO Y
                   DISPLAY SPACES AT LINE Y COL 1
                   DISPLAY '(Y/N)'         AT LINE Y COL 35
                   SUBTRACT 1 FROM Y
                   ACCEPT  WS-CONT AT LINE Y COL 48 AUTO
                   SUBTRACT 1 FROM Y
               END-PERFORM



           END-PERFORM
           EXIT PROGRAM.
           STOP RUN.

