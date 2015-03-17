      ******************************************************************
      *PROGRAM : P04-CTR-BREAK                                         *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 11/24/2014                                            *
      *ABSTRACT: READ FILE AND PRODUCE A FORMATTED REPORT              * 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID INSTRUC-BUILD IS INITIAL PROGRAM
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STU-FILE     ASSIGN TO "../201501SCHEDULE.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT SORT-WORK    ASSIGN TO "SORT-WORK.TXT".

           SELECT STU-FILE2    ASSIGN TO "../2014SCHEDULE.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE     ASSIGN TO "../INSTRUCTOR-MASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS ISAM-OUT-KEY
                               FILE STATUS   IS WS-OUT-STATUS.
      *----------------------------------------------------------------- 
       DATA DIVISION.
       FILE SECTION.
       FD  STU-FILE.
       01  STU-REC.
           03  FILLER              PIC X(58)       VALUE SPACES.
           03  DECIMAL-TEST        PIC X.
           03  FILLER              PIC X(35)       VALUE SPACES.
           03  IN-INSTRUC          PIC X(22).
           03  FILLER              PIC X(13).
           
       FD  STU-FILE2.
       01  STU-REC.
           03  FILLER              PIC X(58)       VALUE SPACES.
           03  DECIMAL-TEST2       PIC X.
           03  FILLER              PIC X(35)       VALUE SPACES.
           03  IN-INSTRUC2         PIC X(22).
           03  FILLER              PIC X(15).
       
       SD  SORT-WORK.
       01  SORT-REC.
           03  SORT-ID             PIC 9999.
           03  SORT-FILLER         PIC X           VALUE SPACES.
           03  SORT-INSTRUC        PIC X(22).
           
       
       FD  OUT-FILE.
       01  STU-OUT.
           03  ISAM-OUT-KEY.
               05  INSTRUC-ID      PIC 9999.
           03  FILLER              PIC X           VALUE SPACES.
           03  INSTRUC-NAME        PIC X(22).
           
           
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
       
       01  WS-FORMATTED-OUTPUT.
           03  WS-INSTRUC-ID       PIC 9999        VALUE 6999.
           03  FILLER              PIC X           VALUE SPACES.
           03  WS-INSTRUC          PIC X(22).
       
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
           03  LINE 1 COL 1  VALUE "REBUILD-INSTRUC".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-INFO.
           03  LINE 10 COL 28 VALUE "INSTRUCTOR-MASTER CREATED".
       
       01  EXIT-SCREEN.
           03  LINE 20 COL 33 "PRESS ENTER TO RETURN".
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT STU-FILE.
           OPEN INPUT STU-FILE2.
           OPEN OUTPUT OUT-FILE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           DISPLAY BLANK-SCREEN.
           DISPLAY SCR-TITLE.
           
          
               SORT SORT-WORK
                   ON ASCENDING KEY SORT-INSTRUC
                   INPUT PROCEDURE 200-FORMAT
                   OUTPUT PROCEDURE 300-SORT
           

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
                           AND IN-INSTRUC NOT EQUAL SPACE
                               MOVE IN-INSTRUC TO WS-INSTRUC

                               MOVE WS-FORMATTED-OUTPUT TO SORT-REC
                               RELEASE SORT-REC
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
                           AND IN-INSTRUC2 NOT EQUAL SPACE
                               MOVE IN-INSTRUC2 TO WS-INSTRUC

                               MOVE WS-FORMATTED-OUTPUT TO SORT-REC
                               RELEASE SORT-REC
                       END-IF
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------  
       300-SORT.
       MOVE 'N' TO WS-EOF.
           PERFORM UNTIL WS-EOF = 'Y'
               RETURN SORT-WORK 
                   AT END 
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE SORT-REC TO STU-OUT
                       IF SORT-INSTRUC NOT EQUAL WS-TEMP1
                           ADD 1 TO WS-INSTRUC-ID GIVING WS-INSTRUC-ID
                           MOVE WS-INSTRUC-ID TO INSTRUC-ID
                           WRITE STU-OUT
                           
                       END-IF
                       MOVE SORT-INSTRUC TO WS-TEMP1
               END-RETURN
           END-PERFORM.

               