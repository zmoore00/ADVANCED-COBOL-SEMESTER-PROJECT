      ******************************************************************
      *PROGRAM : P04-CTR-BREAK                                         *
      *AUTHOR  : ZACKARY MOORE LEE HAWTHORNE                           *
      *DATE    : 11/24/2014                                            *
      *ABSTRACT: READ FILE AND PRODUCE A FORMATTED REPORT              * 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID STUD-BUILD IS INITIAL PROGRAM
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STU-FILE     ASSIGN TO "../student-starter.txt"
                               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUD-LAST-ID ASSIGN TO "../STUD-LAST-ID.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE     ASSIGN TO "../STUDENT-MASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS ISAM-OUT-KEY
                               FILE STATUS   IS WS-OUT-STATUS.
      *----------------------------------------------------------------- 
       DATA DIVISION.
       FILE SECTION.
       FD  STU-FILE.
       01  STU-REC.
           03  IN-LNAME            PIC X(15).
           03  IN-FNAME            PIC X(15).
           03  IN-ADDRESS          PIC X(25).
           03  FILLER              PIC X(22).
           03  IN-ZIP              PIC 9(5).
           03  IN-HPHONE           PIC 9(10).
           03  IN-CPHONE           PIC 9(10).
           03  IN-WPHONE           PIC 9(10).
           03  IN-GENDER           PIC 9.
                 
       
       FD  OUT-FILE.
       01  STU-OUT.
           03  ISAM-OUT-KEY.
               05  STUD-ID         PIC 9999        VALUE 0000.
           03  FILLER              PIC X           VALUE SPACE.
           03  OUT-LNAME           PIC X(15).
           03  OUT-FNAME           PIC X(15).
           03  OUT-ADDRES          PIC X(25).
           03  OUT-ZIP             PIC 9(5).
           03  OUT-HPHONE          PIC 9(10).
           03  OUT-CPHONE          PIC 9(10).
           03  OUT-WPHONE          PIC 9(10).
           03  OUT-GENDER          PIC X.
           03  OUT-ACTIVE          PIC X.
       FD  STUD-LAST-ID.
           01 OUT-REC              PIC X(4).
           
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
           03  LINE 1 COL 1  VALUE "REBUILD-STUDENT".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-INFO.
           03  LINE 10 COL 28 VALUE "STUDENT-MASTER CREATED".
       
       01  EXIT-SCREEN.
           03  LINE 20 COL 33 "PRESS ENTER TO RETURN".
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT STU-FILE.
           OPEN OUTPUT STUD-LAST-ID.
           OPEN OUTPUT OUT-FILE.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           DISPLAY BLANK-SCREEN.
           DISPLAY SCR-TITLE.
                                             
               PERFORM 200-FORMAT
           

               DISPLAY SCR-INFO.
               DISPLAY EXIT-SCREEN.
               ACCEPT WS-EXIT.
           
           CLOSE STU-FILE.
           CLOSE OUT-FILE.
           EXIT PROGRAM.
           
           

      *-----------------------------------------------------------------      
       200-FORMAT.
       
           PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ STU-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO STUD-ID   GIVING STUD-ID                 
                       MOVE IN-LNAME      TO     OUT-LNAME 
                       MOVE IN-FNAME      TO     OUT-FNAME 
                       MOVE IN-ADDRESS    TO     OUT-ADDRES
                       MOVE IN-ZIP        TO     OUT-ZIP   
                       MOVE IN-HPHONE     TO     OUT-HPHONE
                       MOVE IN-CPHONE     TO     OUT-CPHONE
                       MOVE IN-WPHONE     TO     OUT-WPHONE
                                               
                       IF IN-GENDER EQUALS 0
                           MOVE "M" TO OUT-GENDER
                           MOVE "Y" TO OUT-ACTIVE
                       END-IF
                       
                       IF IN-GENDER EQUALS 1
                           MOVE "F" TO OUT-GENDER
                           MOVE "N" TO OUT-ACTIVE
                       END-IF
                           
                       WRITE STU-OUT
                       
      *                 display WS-FORMATTED-OUT
      *                  display stu-out
               END-READ
           END-PERFORM.
           
           WRITE OUT-REC FROM STUD-ID.


               
