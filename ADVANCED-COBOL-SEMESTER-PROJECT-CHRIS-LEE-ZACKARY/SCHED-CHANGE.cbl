      ******************************************************************
      *PROGRAM : BLDG-INQ.CBL                                          *
      *AUTHOR  : ZACK MOORE                                            *
      *DATE    : 4/20/2015                                             *
      *ABSTRACT: FINDS A RECORD IN SCHEDULE MASTER BY CRN SEM AND YR   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHED-CHANGE IS INITIAL PROGRAM.
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
           03  WS-MSG                  PIC X(40)   VALUE SPACES.
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  CONT-FLAG               PIC X       VALUE 'Y'.
           03  WS-CONT                 PIC X.
           03  WS-EOF                  PIC X       VALUE 'N'.
           03  WS-ANOTHER              PIC X.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-YEAR            PIC XXXX     VALUE SPACES.
               05  WS-SEMESTER        PIC XX       VALUE SPACES.
           03  WS-CRN                 PIC X(4)     VALUE SPACES.
           03  FILLER                 PIC XX.
           03  WS-SUBJ                PIC X(4)     VALUE SPACES.
           03  FILLER                 PIC X           VALUE SPACES.
           03  WS-CRSE                PIC X(5)     VALUE SPACES.
           03  WS-TIME-DAY            PIC X(20)    VALUE SPACES.
           03  WS-BLDG                PIC X(6)     VALUE SPACES.
           03  FILLER              PIC X           VALUE SPACES.
           03  WS-ROOM                PIC X(5)     VALUE SPACES.
           03  FILLER              PIC X           VALUE SPACES.
           03  WS-INSTRUCTOR          PIC X(22)    VALUE SPACES.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "SCHED-ADD".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           03  LINE 07 COL 32 VALUE "SCHEDULE CHANGE".
           
       01  SCR-SCHED-CRN.
           
           03  LINE 09 COL 35                       VALUE '     CRN:'.
           03  LINE 09 COL 45 PIC X(4)  TO WS-CRN   AUTO.
           03  LINE 10 COL 35                       VALUE '     SEM:'. 
           03  LINE 10 COL 45 PIC X(2)  TO WS-SEMESTER  AUTO.
           03  LINE 11 COL 35                       VALUE '     YR:'. 
           03  LINE 11 COL 45 PIC X(4)  TO WS-YEAR  AUTO.
           
       01  SCRN-PREV-DATA.
           03  LINE 09 COL 20                        VALUE '    CRN:'.  
           03  LINE 09 COL 35 PIC X(4) FROM CRN   VALUE SPACES.
           03  LINE 10 COL 20                        VALUE '    SEM:'.
           03  LINE 10 COL 35 PIC X(5) FROM SEMESTER  VALUE SPACES.
           03  LINE 11 COL 20                        VALUE '    YR:'.   
           03  LINE 11 COL 35 PIC XXXX FROM YEAR VALUE SPACES.
           03  LINE 12 COL 20                        VALUE '    SUBJ:'.
           03  LINE 12 COL 35 PIC X(4) FROM SUBJ VALUE SPACES.
           03  LINE 13 COL 20                        VALUE '    CRSE:'.
           03  LINE 13 COL 35 PIC X(5) FROM CRSE VALUE SPACES.
           03  LINE 14 COL 20                  VALUE '    TIME/DAY:'.
           03  LINE 14 COL 35 PIC X(20) FROM TIME-DAY VALUE SPACES.
           03  LINE 15 COL 20                  VALUE '    BLDG:'.
           03  LINE 15 COL 35 PIC X(6) FROM BLDG VALUE SPACES.
           03  LINE 16 COL 20                  VALUE '    ROOM:'.
           03  LINE 16 COL 35 PIC X(5) FROM ROOM VALUE SPACES.
           03  LINE 17 COL 20                  VALUE '    INSTRUC:'.
           03  LINE 17 COL 35 PIC X(20) FROM INSTRUCTOR VALUE SPACES.
          
          
          
       01  SCRN-CONFIRM-ADD.
           03  LINE 21 COL 35                    VALUE 
               'ARE YOU SURE YOU WANT TO CHANGE'.
           03  LINE 22 COL 35 PIC 9(4) FROM CRN.
           03  LINE 22 COL 43 PIC XX FROM SEMESTER.
           03  LINE 22 COL 46 PIC XXXX FROM YEAR.
           03  LINE 23 COL 35 PIC X TO WS-RESP AUTO.
       
       01  SCRN-NEW-DATA.
      *     03  LINE 10 COL 55 PIC X(5) TO WS-SEMESTER  VALUE SPACES.
      *     03  LINE 11 COL 55 PIC XXXX TO WS-YEAR VALUE SPACES.
           03  LINE 12 COL 55 PIC X(4) TO WS-SUBJ VALUE SPACES.
           03  LINE 13 COL 55 PIC X(5) TO WS-CRSE VALUE SPACES.
           03  LINE 14 COL 55 PIC X(20) TO WS-TIME-DAY VALUE SPACES.
           03  LINE 15 COL 55 PIC X(6) TO WS-BLDG VALUE SPACES.
           03  LINE 16 COL 55 PIC X(5) TO WS-ROOM VALUE SPACES.
           03  LINE 17 COL 55 PIC X(20) TO WS-INSTRUCTOR VALUE SPACES.

           
       01  SCRN-ADD-ANOTHER.
           03  LINE 19 COL 30 PIC X(40) FROM WS-MSG.
           03  LINE 20 COL 30 VALUE'ENTER ANOTHER Y/N? '.
           03  LINE 21 COL 45 PIC X TO WS-ANOTHER    AUTO.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN I-O ISAM-SCHED-IN.
           
           PERFORM UNTIL WS-ANOTHER EQUALS "N" OR "n"
               DISPLAY SCR-TITLE
               DISPLAY SCR-SCHED-CRN
               ACCEPT  SCR-SCHED-CRN
               MOVE WS-KEY TO ISAM-IN-KEY
               MOVE WS-CRN TO CRN
               
               READ ISAM-SCHED-IN
                   INVALID KEY
                   MOVE 'IN' TO WS-MSG
                       MOVE 'INVALID ID' TO WS-MSG
                   NOT INVALID KEY
                   MOVE 'IN2' TO WS-MSG
                       DISPLAY BLANK-SCREEN
                       DISPLAY SCR-TITLE
                       DISPLAY SCRN-PREV-DATA
                       DISPLAY SCRN-NEW-DATA
                       ACCEPT SCRN-NEW-DATA
                       

                       
                       MOVE WS-KEY TO ISAM-IN-KEY
                       
                       IF WS-SUBJ NOT EQUAL SPACES
                           MOVE WS-SUBJ TO SUBJ
                       ELSE
                           MOVE SUBJ TO SUBJ
                       END-IF
                           
                       IF WS-CRSE NOT EQUAL SPACES
                           MOVE WS-CRSE TO CRSE
                       ELSE
                           MOVE CRSE TO CRSE
                       END-IF

                       IF WS-TIME-DAY NOT EQUAL SPACES
                           MOVE WS-TIME-DAY TO TIME-DAY
                       ELSE
                           MOVE TIME-DAY TO TIME-DAY
                       END-IF

                       IF WS-BLDG NOT EQUAL SPACES
                           MOVE WS-BLDG TO BLDG
                       ELSE
                           MOVE BLDG TO BLDG
                       END-IF
                           
                       IF WS-ROOM NOT EQUAL SPACES
                           MOVE WS-ROOM TO ROOM
                       ELSE
                           MOVE ROOM TO ROOM
                       END-IF
                           
                       IF WS-INSTRUCTOR NOT EQUAL SPACES
                           MOVE WS-INSTRUCTOR TO INSTRUCTOR
                       ELSE
                           MOVE INSTRUCTOR TO INSTRUCTOR
                       END-IF

                       DISPLAY SCRN-CONFIRM-ADD
                       ACCEPT SCRN-CONFIRM-ADD
                       IF WS-RESP EQUALS 'Y' OR 'y'
                       REWRITE ISAM-REC-IN
                           INVALID KEY
                               MOVE   'INVALID ID' TO WS-MSG
                           NOT INVALID KEY
                               STRING ISAM-IN-KEY ' UPDATED' INTO 
                               WS-MSG
                               
                       END-REWRITE
                       END-IF
                       
                       DISPLAY SPACES AT LINE 21 COL 1
                       DISPLAY SPACE AT LINE 22 COL 1
                       DISPLAY SPACE AT LINE 23 COL 1
                       
                       IF WS-ANOTHER EQUALS 'N' OR 'n'
                           EXIT PROGRAM
                       END-IF
               END-READ
               


               
               DISPLAY SCRN-ADD-ANOTHER
               ACCEPT  SCRN-ADD-ANOTHER
               PERFORM UNTIL WS-ANOTHER='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
                   MOVE "ENTER Y OR N" TO WS-MSG
               END-PERFORM
           END-PERFORM.

           CLOSE ISAM-SCHED-IN.
           EXIT PROGRAM.
           STOP RUN.
