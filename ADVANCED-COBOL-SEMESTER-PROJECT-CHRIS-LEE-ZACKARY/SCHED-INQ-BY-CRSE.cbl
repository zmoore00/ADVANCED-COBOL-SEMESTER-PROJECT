      ******************************************************************
      *PROGRAM : BLDG-INQ.CBL                                          *
      *AUTHOR  : ZACK MOORE                                            *
      *DATE    : 4/20/2015                                             *
      *ABSTRACT: FINDS A RECORD IN SCHEDULE MASTER BY CRSE SEM AND YR  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHED-INQ-BY-CRSE IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-SCHED-IN ASSIGN TO "../SCHEDULE-MASTER.DAT"      
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC  
      *                                       | THIS IS A SPLIT KEY, 
      *                                       V OTHER COMPILERS HAVE DIF
      *                                         WAYS OF MAKING THEM
                               RECORD KEY    IS CRN-KEY=ISAM-IN-KEY CRN 
                               ALTERNATE KEY IS CRSE-KEY=ISAM-IN-KEY
                                   CRSE
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-SCHED-IN.
       01  ISAM-REC-IN.
           03  ISAM-IN-KEY.
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
           03  EOF-FLAG                PIC X.
               88 EOF        VALUE '1'.
           03  WS-MSG                  PIC X(40)   VALUE SPACES.
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  CONT-FLAG               PIC X       VALUE 'Y'.
           03  WS-ANOTHER              PIC X.
               
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
           03  FILLER                 PIC X           VALUE SPACES.
           03  WS-ROOM                PIC X(5).
           03  FILLER                 PIC X           VALUE SPACES.
           03  WS-INSTRUCTOR          PIC X(22).
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "SCHED-INQ-BY-CRSE".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCRN-KEY-REQ.
           05  LINE 07 COL 32 VALUE "SCHEDULE SEARCH BY CRSE".
           03  LINE 09 COL 35                       VALUE '     CRSE:'.
           03  LINE 09 COL 45 PIC X(4)  TO WS-CRSE   AUTO.
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
           03  LINE 09 COL 45 PIC X(5) FROM WS-CRN   VALUE SPACES.
           03  LINE 10 COL 30                        VALUE '    SEM:'.
           03  LINE 10 COL 45 PIC X(5) FROM WS-SEMESTER  VALUE SPACES.
           03  LINE 11 COL 30                        VALUE '    YR:'.   
           03  LINE 11 COL 45 PIC XXXX FROM WS-YEAR VALUE SPACES.
           03  LINE 12 COL 30                        VALUE '    SUBJ:'.
           03  LINE 12 COL 45 PIC X(4) FROM WS-SUBJ VALUE SPACES.
           03  LINE 13 COL 30                        VALUE '    CRSE:'.
           03  LINE 13 COL 45 PIC X(4) FROM WS-CRSE VALUE SPACES.
           03  LINE 14 COL 30                  VALUE '    TIME/DAY:'.
           03  LINE 14 COL 45 PIC X(20) FROM WS-TIME-DAY VALUE SPACES.
           03  LINE 15 COL 30                  VALUE '    BLDG:'.
           03  LINE 15 COL 45 PIC X(6) FROM WS-BLDG VALUE SPACES.
           03  LINE 16 COL 30                  VALUE '    ROOM:'.
           03  LINE 16 COL 45 PIC X(5) FROM WS-ROOM VALUE SPACES.
           03  LINE 17 COL 30                  VALUE '    INSTRUC:'.
           03  LINE 17 COL 45 PIC X(20) FROM WS-INSTRUCTOR VALUE SPACES.
       01  SCRN-SCHED-ANOTHER.
           03  LINE 19 COL 35              VALUE'ENTER ANOTHER Y/N '.
           03  LINE 20 COL 43 PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE   
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN INPUT ISAM-SCHED-IN.
           
           PERFORM UNTIL (WS-CRN='X' OR 'x') OR (WS-SEMESTER='X' OR 'x')
               DISPLAY SCR-TITLE
               DISPLAY SCRN-KEY-REQ
               ACCEPT  SCRN-KEY-REQ
               
               MOVE WS-KEY TO ISAM-IN-KEY
               MOVE WS-CRSE TO CRSE 
               READ ISAM-SCHED-IN KEY IS CRSE-KEY
                   INVALID KEY
                       MOVE "INVALID ID" TO WS-MSG
                   NOT INVALID KEY
                       MOVE ISAM-REC-IN TO WS-REC
                       DISPLAY SCR-TITLE
                       DISPLAY SCRN-DATA-TITLE
                       DISPLAY SCRN-SCHED-DATA
                       DISPLAY 'PRESS ENTER TO CONTINUE'
                           AT LINE 19 COL 34
                           WITH NO ADVANCING
                       ACCEPT  WS-RESP
                       PERFORM UNTIL EOF
                       READ ISAM-SCHED-IN NEXT RECORD
                           AT END
                               MOVE 1 TO EOF-FLAG
                           NOT AT END
                               IF CRSE = WS-CRSE
                                   MOVE ISAM-REC-IN TO WS-REC
                                   DISPLAY SCR-TITLE
                                   DISPLAY SCRN-DATA-TITLE
                                   DISPLAY SCRN-SCHED-DATA
                                   DISPLAY 'PRESS ENTER TO CONTINUE'
                                       AT LINE 19 COL 34
                                   WITH NO ADVANCING
                                   ACCEPT  WS-RESP
                               ELSE
                                   MOVE 1 TO EOF-FLAG
                               END-IF
                       END-READ
                       END-PERFORM
                       MOVE 0 TO EOF-FLAG
                       DISPLAY SPACES AT LINE 19 COL 1 
                       DISPLAY SCRN-SCHED-ANOTHER
                       ACCEPT  SCRN-SCHED-ANOTHER
                       IF WS-ANOTHER EQUALS 'N' OR 'n'
                           EXIT PROGRAM
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ISAM-SCHED-IN.
           EXIT PROGRAM.
           STOP RUN.
