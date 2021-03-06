      ******************************************************************
      *PROGRAM : BLDG-BUILD.CBL                                        *
      *AUTHOR  : Lee Hawthorne                                         *
      *DATE    : 2/17/2015                                             *
      *ABSTRACT: This program builds the BUILDING-ISAM.DAT FILE        *
      ******************************************************************
       PROGRAM-ID. BLDG-BUILD AS "BLDG-BUILD" IS INITIAL PROGRAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BLDG-IN-FILE  ASSIGN TO WS-IN-FILE
                              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-WORK    ASSIGN TO "SORTWORK.TXT".
                            
           SELECT ISAM-BLDG-OUT ASSIGN TO "../BUILDING-ISAM.DAT"
                              ORGANIZATION  IS INDEXED
                              ACCESS        IS SEQUENTIAL
                              RECORD KEY    IS ISAM-OUT-KEY
                              FILE STATUS   IS WS-OUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  BLDG-IN-FILE.
       01  BLDG-REC-IN.
           03  FILLER      PIC X(81).
           03  IN-BLDG     PIC X(7).
           03  IN-ROOM     PIC X(5).
           03  FILLER      PIC X(2).
           03  IN-PERIOD   PIC X.
           03  FILLER      PIC X(20).
           03  IN-SEATS    PIC X(4).
           
       SD  SORT-WORK.
       01  SORT-REC.
           03  FILLER        PIC X(81).
           03  SORT-BLDG     PIC X(7).
           03  SORT-ROOM     PIC X(5).
           03  FILLER        PIC X(2).
           03  SORT-PERIOD   PIC X.
           03  FILLER        PIC X(20).
           03  SORT-SEATS    PIC X(4).
       
       FD  ISAM-BLDG-OUT.
       01  ISAM-REC-OUT.
           03  ISAM-OUT-KEY.
               05  ISAM-OUT-BLDG PIC X(7).
               05  ISAM-OUT-ROOM PIC X(5).
           03  ISAM-OUT-SEATS    PIC X(4).
       
               
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
       
       01  EOF-FLAG                PIC X       VALUE "N". 
           88  EOF                             VALUE "Y".
       01  WS-OUT-STATUS           PIC XX.
       
       01  WS-BLDG                 PIC X(7)    VALUE SPACES.
       01  WS-ROOM                 PIC X(5)    VALUE SPACES.
       01  WS-SEATS                PIC X(4)    VALUE SPACES.
       
       01  WS-IN-FILE              PIC X(25).
       01  WS-EXIT                 PIC X           VALUE 'N'.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "REBUILD-INSTRUC".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-INFO.
           03  LINE 10 COL 28 VALUE "BUILDING-MASTER CREATED".
       
       01  EXIT-SCREEN.
           03  LINE 20 COL 33 "PRESS ENTER TO RETURN".
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
           
       PROCEDURE DIVISION.
       000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           DISPLAY BLANK-SCREEN.
           DISPLAY SCR-TITLE.
      *----Sort by keys, did descending on seats to get max seats first.
      *----The ISAM write only takes the first record that matches the
      *----key so it only took the max seats since it got it first.
      *----could have also used a control break.
           SORT SORT-WORK
               ON ASCENDING KEY SORT-BLDG, SORT-ROOM, 
               DESCENDING KEY SORT-SEATS
               INPUT  PROCEDURE  100-SORT-IN
               OUTPUT PROCEDURE  200-WRITE-TXT.
               DISPLAY SCR-INFO
               DISPLAY EXIT-SCREEN
               ACCEPT WS-EXIT
           EXIT PROGRAM.
           
           
       100-SORT-IN.
      *This switches between files and resets eof-flag
           MOVE "../201405schedule.txt" TO WS-IN-FILE
           OPEN  INPUT  BLDG-IN-FILE
           PERFORM 300-READ-FILE.
           CLOSE BLDG-IN-FILE.
           MOVE 'N' TO EOF-FLAG
           MOVE "../201501schedule.txt" TO WS-IN-FILE
           OPEN  INPUT  BLDG-IN-FILE
           PERFORM 300-READ-FILE.
           CLOSE BLDG-IN-FILE.
           
           
       200-WRITE-TXT.
      *writes records, the ISAM file will only write the first record it
      *sees for duplicate keys.
           OPEN OUTPUT ISAM-BLDG-OUT
           MOVE 'N' TO EOF-FLAG.
           PERFORM UNTIL EOF
               RETURN SORT-WORK
               AT END
                   MOVE 'Y' TO EOF-FLAG
               NOT AT END
                   MOVE SORT-BLDG  TO ISAM-OUT-BLDG
                   MOVE SORT-ROOM  TO ISAM-OUT-ROOM
                   MOVE SORT-SEATS TO ISAM-OUT-SEATS
                   WRITE ISAM-REC-OUT
               END-RETURN
           END-PERFORM.
           CLOSE ISAM-BLDG-OUT.
           
       300-READ-FILE.
      *reads records, skips bad records and combines incomplete records
           PERFORM UNTIL EOF
               READ BLDG-IN-FILE
               AT END
                   MOVE 'Y' TO EOF-FLAG
               NOT AT END
      *            if no '.' then it's a bad record
                   IF IN-PERIOD EQUALS '.'
                   THEN
      *                if building is not blank and not 'TBA'
                       IF  IN-BLDG  NOT EQUALS SPACES AND 
                           IN-BLDG  NOT EQUALS 'TBA'
                       THEN
      *                    store BLDG and ROOM parts of record
                           MOVE IN-BLDG TO WS-BLDG
                           MOVE IN-ROOM TO WS-ROOM
                       END-IF
      *                if seats is not blank
                       IF  IN-SEATS NOT EQUALS SPACES
                       THEN
      *                    store seats part of record
                           MOVE IN-SEATS TO WS-SEATS
                       END-IF
      *               if both of the record parts are there, send record
      *               to sort and reset temporary record variables.
                       IF WS-BLDG NOT EQUALS SPACES 
                       AND WS-SEATS NOT EQUALS SPACES
                       THEN
                           MOVE WS-BLDG  TO SORT-BLDG
                           MOVE WS-ROOM  TO SORT-ROOM
                           IF   WS-SEATS(2:1) EQUALS ' '
                           THEN
                               STRING ' ' WS-SEATS(1:1) INTO SORT-SEATS
                           ELSE
                               MOVE WS-SEATS TO SORT-SEATS
                           END-IF
                           RELEASE SORT-REC
      *                    store spaces in temp record so it will fail
      *                    the empty test if new data is not found.
                           MOVE SPACES   TO WS-BLDG
                           MOVE SPACES   TO WS-ROOM
                           MOVE SPACES   TO WS-SEATS
                       END-IF
                   END-IF
               END-READ
           END-PERFORM.
