      ******************************************************************
      *PROGRAM : SCHED-BUILD                                           *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 04/06/2015                                            *
      *ABSTRACT: READ FILE AND PRODUCE A SCHEDULE MASTER               * 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID SCHED-BUILD IS INITIAL PROGRAM
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STU-FILE     ASSIGN TO "../201501SCHEDULE.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
           
      *     SELECT SORT-WORK    ASSIGN TO "SORT-WORK.TXT".

           SELECT STU-FILE2    ASSIGN TO "../201405SCHEDULE.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEST-OUT     ASSIGN TO "../TEST-OUT.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE     ASSIGN TO "../SCHEDULE-MASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS ISAM-OUT-KEY
                               FILE STATUS   IS WS-OUT-STATUS.
      *----------------------------------------------------------------- 
       DATA DIVISION.
       FILE SECTION.
       FD  STU-FILE.
       01  STU-REC.
           03  IN-CRN              PIC X(6).
           03  IN-SUBJ             PIC X(5).
           03  IN-CRSE             PIC X(6).
           03  FILLER              PIC X(44)       VALUE SPACES.
           03  IN-TIME-DAY         PIC X(20).
           03  IN-BLDG             PIC X(7).
           03  IN-ROOM             PIC X(6).
           03  IN-FINITIAL         PIC X.
           03  DECIMAL-TEST        PIC X.
           03  IN-INSTRUC          PIC X(20).
           03  FILLER              PIC X(10).
           
       FD  STU-FILE2.
       01  STU-REC.
           03  IN-CRN2              PIC X(6).
           03  IN-SUBJ2             PIC X(5).
           03  IN-CRSE2             PIC X(6).
           03  FILLER              PIC X(44)       VALUE SPACES.
           03  IN-TIME-DAY2         PIC X(20).
           03  IN-BLDG2             PIC X(7).
           03  IN-ROOM2             PIC X(6).
           03  IN-FINITIAL2         PIC X.
           03  DECIMAL-TEST2        PIC X.
           03  IN-INSTRUC2          PIC X(20).
           03  FILLER              PIC X(10).
       
      * SD  SORT-WORK.
      * 01  SORT-REC.
      *     03  SORT-ID             PIC 9999.
      *     03  SORT-FILLER         PIC X           VALUE SPACES.
      *     03  SORT-INSTRUC        PIC X(22).
           
       
       FD  OUT-FILE.
       01  STU-OUT.
           03  ISAM-OUT-KEY.
               05  YEAR            PIC XXXX.
               05  SEMESTER        PIC XX.
               05  CRN             PIC X(6).
      *     03  FILLER              PIC X           VALUE SPACES.
           03  SUBJ                PIC X(5).
           03  CRSE                PIC X(6).
           03  TIME-DAY            PIC X(20).
           03  BLDG                PIC X(7).
           03  ROOM                PIC X(6).
           03  INSTRUCTOR          PIC X(22).
           
       FD  TEST-OUT.
       01  OUT-REC                 PIC X(80).
       
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
       
       01  WS-FORMATTED-OUT-2015.
           03  WS-YEAR             PIC X(4)            VALUE "2015".
           03  WS-SEMESTER         PIC X(2)            VALUE "01".
           03  WS-CRN              PIC X(6).
           03  WS-SUBJ             PIC X(5).
           03  WS-CRSE             PIC X(6).
           03  WS-TIME-DAY         PIC X(20).
           03  WS-BLDG             PIC X(7).
           03  WS-ROOM             PIC X(6).
           03  WS-INSTRUCTOR.
               05  WS-FINITIAL         PIC X.
               05  WS-DECIMAL-TEST     PIC X.
               05  WS-INSTRUC          PIC X(20).
               
       01  WS-FORMATTED-OUT-2014.
           03  WS-YEAR2             PIC X(4)           VALUE "2014".
           03  WS-SEMESTER2         PIC X(2)           VALUE "05".
           03  WS-CRN2              PIC X(6).
           03  WS-SUBJ2             PIC X(5).
           03  WS-CRSE2             PIC X(6).
           03  WS-TIME-DAY2         PIC X(20).
           03  WS-BLDG2             PIC X(7).
           03  WS-ROOM2             PIC X(6).
           03  WS-INSTRUCTOR2.
               05  WS-FINITIAL2         PIC X.
               05  WS-DECIMAL-TEST2     PIC X.
               05  WS-INSTRUC2          PIC X(20).
       
       01  WS-EOF                  PIC X           VALUE 'N'.
       01  WS-EOF2                 PIC X           VALUE 'N'.
       01  WS-TEMP1                PIC X(22).
       01  WS-TEMP2                PIC X(22).
       01  WS-EXIT                 PIC X           VALUE 'N'.
       01  WS-OUT-STATUS           PIC XX.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "REBUILD-SCHEDULE".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-INFO.
           03  LINE 10 COL 28 VALUE "SCHEDULE-MASTER CREATED".
       
       01  EXIT-SCREEN.
           03  LINE 20 COL 33 "PRESS ENTER TO RETURN".
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT STU-FILE.
           OPEN INPUT STU-FILE2.
           OPEN OUTPUT TEST-OUT.
           OPEN OUTPUT OUT-FILE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           DISPLAY BLANK-SCREEN.
           DISPLAY SCR-TITLE.
           
          
      *         SORT SORT-WORK
      *             ON ASCENDING KEY SORT-INSTRUC
      *             INPUT PROCEDURE 200-FORMAT
      *             OUTPUT PROCEDURE 300-SORT
           
               PERFORM 200-FORMAT
               DISPLAY SCR-INFO.
               DISPLAY EXIT-SCREEN.
               ACCEPT WS-EXIT.
           
           CLOSE STU-FILE.
           CLOSE STU-FILE2.
           CLOSE OUT-FILE.
           EXIT PROGRAM.
           
           

      *-----------------------------------------------------------------      
       200-FORMAT.
       
           PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ STU-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF DECIMAL-TEST EQUALS '.'
                           AND IN-INSTRUC NOT EQUAL 'TBA'
                           AND IN-CRN NOT EQUAL SPACE
                               MOVE IN-CRN TO WS-CRN
                               MOVE IN-SUBJ TO WS-SUBJ
                               MOVE IN-CRSE TO WS-CRSE
                               MOVE IN-TIME-DAY TO WS-TIME-DAY
                               MOVE IN-BLDG TO WS-BLDG
                               MOVE IN-ROOM TO WS-ROOM
                               MOVE IN-FINITIAL TO WS-FINITIAL
                               MOVE DECIMAL-TEST TO WS-DECIMAL-TEST
                               MOVE IN-INSTRUC TO WS-INSTRUC
                               
      *                         WRITE OUT-REC FROM WS-FORMATTED-OUT-2015
                               
      *                         MOVE WS-FORMATTED-OUT-2015 TO SORT-REC
                                WRITE STU-OUT FROM WS-FORMATTED-OUT-2015
      *                         RELEASE SORT-REC
                       END-IF
               END-READ
           END-PERFORM.
           
           PERFORM UNTIL WS-EOF2 EQUALS 'Y'
               READ STU-FILE2
                  AT END
                       MOVE 'Y' TO WS-EOF2
                   NOT AT END
                       IF DECIMAL-TEST2 EQUALS '.'
                           AND IN-INSTRUC2 NOT EQUAL 'TBA'
                           AND IN-CRN NOT EQUAL SPACE
                               MOVE IN-CRN2 TO WS-CRN2
                               MOVE IN-SUBJ2 TO WS-SUBJ2
                               MOVE IN-CRSE2 TO WS-CRSE2
                               MOVE IN-TIME-DAY2 TO WS-TIME-DAY2
                               MOVE IN-BLDG2 TO WS-BLDG2
                               MOVE IN-ROOM2 TO WS-ROOM2
                               MOVE IN-FINITIAL2 TO WS-FINITIAL2
                               MOVE DECIMAL-TEST2 TO WS-DECIMAL-TEST2
                               MOVE IN-INSTRUC2 TO WS-INSTRUC2
                               
                               WRITE OUT-REC FROM WS-FORMATTED-OUT-2014
      *                         MOVE WS-FORMATTED-OUT-2015 TO SORT-REC
                                WRITE STU-OUT FROM WS-FORMATTED-OUT-2014
      *                         RELEASE SORT-REC
                       END-IF
               END-READ
           END-PERFORM.
           
           

      *-----------------------------------------------------------------  


               
