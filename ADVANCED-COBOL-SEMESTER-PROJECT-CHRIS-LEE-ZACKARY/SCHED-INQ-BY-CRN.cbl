      ******************************************************************
      *PROGRAM : BLDG-INQ.CBL                                          *
      *AUTHOR  : ZACK MOORE                                            *
      *DATE    : 4/20/2015                                             *
      *ABSTRACT: FINDS A RECORD IN SCHEDULE MASTER BY CRN SEM AND YR   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHED-INQ-BY-CRN IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-STUD-IN ASSIGN TO "../SCHEDULE-MASTER.DAT"       
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM    
                               RECORD KEY    IS ISAM-IN-KEY
                               ALTERNATE KEY IS CRSE
                                           WITH DUPLICATES
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-STUD-IN.
       01  ISAM-REC-IN.
           03  ISAM-IN-KEY.
               05  YEAR            PIC XXXX.
               05  SEMESTER        PIC XX.
               05  CRN             PIC X(6).
           03  SUBJ                PIC X(5).
           03  CRSE                PIC X(6).
           03  TIME-DAY            PIC X(20).
           03  BLDG                PIC X(7).
           03  ROOM                PIC X(6).
           03  INSTRUCTOR          PIC X(22).
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
           03  WS-ANOTHER              PIC X.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-YEAR            PIC XXXX.
               05  WS-SEMESTER        PIC XX.
               05  WS-CRN             PIC X(6).
           03  WS-SUBJ                PIC X(5).
           03  WS-CRSE                PIC X(6).
           03  WS-TIME-DAY            PIC X(20).
           03  WS-BLDG                PIC X(7).
           03  WS-ROOM                PIC X(6).
           03  WS-INSTRUCTOR          PIC X(22).
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
           03  LINE 09 COL 45 PIC X(6)  TO WS-CRN   AUTO.
           03  LINE 10 COL 35                       VALUE '     SEM:'. 
           03  LINE 10 COL 45 PIC X(2)  TO WS-SEMESTER  AUTO.
           03  LINE 11 COL 35                       VALUE '     YR:'. 
           03  LINE 11 COL 45 PIC X(4)  TO WS-YEAR  AUTO.
           03  LINE 13 COL 35                       VALUE '  (X=EXIT)'.
           03  LINE 14 COL 35 PIC X(40) FROM WS-MSG.
           
       01  SCRN-DATA-TITLE.
           05  LINE 07 COL 35 VALUE "RECORD FOUND". 
           
       01  SCRN-SCHED-DATA.
           03  LINE 09 COL 30                        VALUE '    CRN:'.  
           03  LINE 09 COL 45 PIC X(6) FROM WS-CRN   VALUE SPACES.
           03  LINE 10 COL 30                        VALUE '    SEM:'.
           03  LINE 10 COL 45 PIC X(5) FROM WS-SEMESTER  VALUE SPACES.
           03  LINE 11 COL 30                        VALUE '    YR:'.   
           03  LINE 11 COL 45 PIC XXXX FROM WS-YEAR VALUE SPACES.
           03  LINE 12 COL 30                        VALUE '    SUBJ:'.
           03  LINE 12 COL 45 PIC X(5) FROM WS-SUBJ VALUE SPACES.
           03  LINE 13 COL 30                        VALUE '    CRSE:'.
           03  LINE 13 COL 45 PIC X(6) FROM WS-CRSE VALUE SPACES.
           03  LINE 14 COL 30                  VALUE '    TIME/DAY:'.
           03  LINE 14 COL 45 PIC X(20) FROM WS-TIME-DAY VALUE SPACES.
           03  LINE 15 COL 30                  VALUE '    BLDG:'.
           03  LINE 15 COL 45 PIC X(7) FROM WS-BLDG VALUE SPACES.
           03  LINE 16 COL 30                  VALUE '    ROOM:'.
           03  LINE 16 COL 45 PIC X(20) FROM WS-ROOM VALUE SPACES.
           03  LINE 17 COL 30                  VALUE '    INSTRUC:'.
           03  LINE 17 COL 45 PIC X(20) FROM WS-INSTRUCTOR VALUE SPACES.
           03  LINE 19 COL 35              VALUE'ENTER ANOTHER Y/N '.
           03  LINE 20 COL 43 PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN INPUT ISAM-STUD-IN.
           
           PERFORM UNTIL (WS-CRN='X' OR 'x') OR (WS-SEMESTER='X' OR 'x')
               DISPLAY SCR-TITLE
               DISPLAY SCRN-KEY-REQ
               ACCEPT  SCRN-KEY-REQ
               MOVE WS-KEY TO ISAM-IN-KEY
               READ ISAM-STUD-IN
                   INVALID KEY
                       MOVE "INVALID ID" TO WS-MSG
                   NOT INVALID KEY
                       MOVE ISAM-REC-IN TO WS-REC
                       DISPLAY SCR-TITLE
                       DISPLAY SCRN-DATA-TITLE
                       DISPLAY SCRN-SCHED-DATA
                       ACCEPT WS-ANOTHER
                       IF WS-ANOTHER EQUALS 'N' OR 'n'
                           EXIT PROGRAM
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE ISAM-STUD-IN.
           EXIT PROGRAM.
           STOP RUN.
