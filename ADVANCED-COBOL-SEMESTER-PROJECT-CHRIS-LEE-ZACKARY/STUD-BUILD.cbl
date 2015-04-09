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
           03  IN-HPHONE1          PIC 9(3).
           03  IN-HPHONE2          PIC 9(3).
           03  IN-HPHONE3          PIC 9(4).
           03  IN-CPHONE1          PIC 9(3).
           03  IN-CPHONE2          PIC 9(3).
           03  IN-CPHONE3          PIC 9(4).
           03  IN-WPHONE1          PIC 9(3).
           03  IN-WPHONE2          PIC 9(3).
           03  IN-WPHONE3          PIC 9(4).
           03  IN-GENDER           PIC 9.
                          
       
       FD  OUT-FILE.
       01  STU-OUT.
           03  ISAM-OUT-KEY.
               05  STUD-ID         PIC 9999.
           03  FILLER              PIC X           VALUE SPACES.
           03  OUT-LNAME           PIC X(15).
           03  OUT-FNAME           PIC X(15).
           03  OUT-ADDRES          PIC X(25).
           03  FILLER              PIC X(22).
           03  OUT-ZIP             PIC 9(5).
           03  OUT-HPHONE          PIC 9(10).
           03  OUT-CPHONE          PIC 9(10).
           03  OUT-WPHONE          PIC 9(10).
           03  OUT-GENDER          PIC 9.
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
           
       01  WS-FORMATTED-OUT.
           03  WS-STUDID           PIC 9999        VALUE 0000.
           03  WS-LNAME            PIC X(15).
           03  WS-FNAME            PIC X(15).
           03  WS-ADDRESS          PIC X(25).
           03  FILLER              PIC X(22).
           03  WS-ZIP              PIC 9(5).
           03  FILLER              PIC X           VALUE "(".
           03  WS-HPHONE1          PIC 9(3).
           03  FILLER              PIC X           VALUE ")".
           03  WS-HPHONE2          PIC 9(3).
           03  FILLER              PIC X           VALUE "-".
           03  WS-HPHONE3          PIC 9(4).
           03  FILLER              PIC X           VALUE "(".
           03  WS-CPHONE1          PIC 9(3).
           03  FILLER              PIC X           VALUE ")".
           03  WS-CPHONE2          PIC 9(3).
           03  FILLER              PIC X           VALUE "-".
           03  WS-CPHONE3          PIC 9(4).
           03  FILLER              PIC X           VALUE "(".
           03  WS-WPHONE1          PIC 9(3).
           03  FILLER              PIC X           VALUE ")".
           03  WS-WPHONE2          PIC 9(3).
           03  FILLER              PIC X           VALUE "-".
           03  WS-WPHONE3          PIC 9(4).
           03  WS-GENDER           PIC X.
           03  WS-ACTIVE           PIC X.
       
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
                       MOVE IN-LNAME      TO     WS-LNAME   
                       MOVE IN-FNAME      TO     WS-FNAME   
                       MOVE IN-ADDRESS    TO     WS-ADDRESS 
                       MOVE IN-ZIP        TO     WS-ZIP     
                       MOVE IN-HPHONE1    TO     WS-HPHONE1 
                       MOVE IN-HPHONE2    TO     WS-HPHONE2 
                       MOVE IN-HPHONE3    TO     WS-HPHONE3 
                       MOVE IN-CPHONE1    TO     WS-CPHONE1 
                       MOVE IN-CPHONE2    TO     WS-CPHONE2 
                       MOVE IN-CPHONE3    TO     WS-CPHONE3 
                       MOVE IN-WPHONE1    TO     WS-WPHONE1 
                       MOVE IN-WPHONE2    TO     WS-WPHONE2             
                       MOVE IN-WPHONE3    TO     WS-WPHONE3             
                       MOVE IN-GENDER     TO     WS-GENDER              
                       
                       IF IN-GENDER EQUALS 0
                           MOVE "M" TO WS-GENDER
                           MOVE "Y" TO WS-ACTIVE
                       END-IF
                       
                       IF IN-GENDER EQUALS 1
                           MOVE "F" TO WS-GENDER
                           MOVE "N" TO WS-ACTIVE
                       END-IF
                           
                       ADD 1 TO WS-STUDID GIVING WS-STUDID
               END-READ
           END-PERFORM.
           
           WRITE OUT-REC FROM WS-STUDID.


               
