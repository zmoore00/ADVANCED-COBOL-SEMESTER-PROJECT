      ******************************************************************
      *PROGRAM : Pull out specific course information from schedule                      *
      *AUTHOR  : Chris Jeong                                          *
      *DATE    : 02-15-2015                                            *
      *ABSTRACT: Builds course master file                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-BUILD.
      *-----------------------------------------------------------------       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSE-01-MST-IN    ASSIGN TO "../201501schedule.TXT"
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COURSE-05-MST-IN    ASSIGN TO "../201405schedule.TXT"
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-WORK        ASSIGN TO "SORT-WORK.TXT".
                                                   
           SELECT COURSE-MST-OUT   ASSIGN TO "../COURSE-MASTER.TXT"
                                   ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD COURSE-01-MST-IN.
       01  COURSE-01-RECI.
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-01-SUBJ-IN   PIC X(4).
      *         10  CSUBJ-01-PRE   PIC X(2).
      *         10  CSUBJ-01-SUF   PIC X(2).
           05  FILLER             PIC X            VALUE SPACE.
           05  COURSE-01-CRSE-IN   PIC X(4).
      *         10  CCRSE-01-PRE   PIC X(3).
      *         10  CCRSE-01-SUF   PIC X(1).
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-01-TITLE-IN    PIC X(30).
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-01-CREDITS-IN  PIC X(3).
           
       FD COURSE-05-MST-IN.
       01  COURSE-05-RECI.
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-05-SUBJ-IN   PIC X(4).
      *         10  CSUBJ-05-PRE   PIC X(2).
      *         10  CSUBJ-05-SUF   PIC X(2).           
           05  FILLER             PIC X            VALUE SPACE.
           05  COURSE-05-CRSE-IN   PIC X(4).
      *         10  CCRSE-01-PRE   PIC X(3).
      *         10  CCRSE-01-SUF   PIC X(1).           
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-05-TITLE-IN    PIC X(30).
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-05-CREDITS-IN  PIC X(3).           
       
       SD SORT-WORK.
       01 SORT-REC.
           05  SORT-SUBJ           PIC X(4).
           05  FILLER              PIC X            VALUE SPACE.
           05  SORT-CRSE           PIC X(4).
           05  FILLER              PIC X            VALUE SPACE.
           05  SORT-TITLE          PIC X(30).
           05  FILLER              PIC X            VALUE SPACE.
           05  SORT-CREDITS        PIC X(3).
           05  FILLER              PIC X(36)        VALUE SPACE.
        
           
           
       FD COURSE-MST-OUT.
       01  COURSE-REC.
           05  COURSE-COURSE-COMBINE.
               10  COURSE-SUBJ-OUT PIC X(4).
               10  FILLER              PIC X            VALUE SPACE.
               10  COURSE-CRSE-OUT PIC X(4).
               10  FILLER              PIC X            VALUE SPACE.
           05  COURSE-TITLE-OUT    PIC X(30).
           05  FILLER              PIC X                VALUE SPACE.
           05  COURSE-CREDITS-OUT  PIC X(3).
           05  FILLER              PIC X(36)            VALUE SPACE.
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
       
       01  WS-EXIT                 PIC X           VALUE 'N'.
       
       01  WS-TITLE-COMP           PIC X(30).
       
       01  WS-VARS.
           05  WS-EOF            PIC X           VALUE 'N'.
               88  EOF                         VALUE 'Y'.       
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "COURSE-BUILD".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM  DISPLAY-DATE.
           
       01  SCR-INFO.
           03  LINE 10 COL 28 VALUE "COURSE-MASTER CREATED".
       
       01  EXIT-SCREEN.
           03  LINE 20 COL 33 "CONFIRM EXIT (Y/N)".
           03  LINE 20 COL 31 PIC X TO WS-EXIT AUTO.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
              
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT COURSE-01-MST-IN.
           OPEN INPUT COURSE-05-MST-IN.
           OPEN OUTPUT COURSE-MST-OUT.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           DISPLAY BLANK-SCREEN.
           DISPLAY SCR-TITLE.
           
           SORT SORT-WORK
                ON ASCENDING KEY SORT-SUBJ
                INPUT  PROCEDURE 100-FILE-IN
                OUTPUT PROCEDURE 200-FILE-OUT
                
                
               DISPLAY EXIT-SCREEN
               ACCEPT EXIT-SCREEN                
      *     DISPLAY "PROGRAM TERMINATED".
      *     DISPLAY "PRESS ENTER TO CLOSE".
           
           CLOSE COURSE-01-MST-IN.
           CLOSE COURSE-05-MST-IN.
           CLOSE COURSE-MST-OUT.
           
           EXIT PROGRAM.           
       
       
       
      *-----------------------------------------------------------------
       100-FILE-IN.
       
           PERFORM UNTIL EOF
               READ COURSE-01-MST-IN
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF COURSE-01-SUBJ-IN(1:2) IS ALPHABETIC
                       AND COURSE-01-CRSE-IN(1:3) IS NUMERIC
                       THEN
                           MOVE COURSE-01-SUBJ-IN      TO SORT-SUBJ
                           MOVE COURSE-01-CRSE-IN      TO SORT-CRSE
                           MOVE COURSE-01-TITLE-IN     TO SORT-TITLE
                           MOVE COURSE-01-CREDITS-IN   TO SORT-CREDITS
                           RELEASE SORT-REC   
                   END-IF        
               END-READ
           END-PERFORM.
           
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL EOF
               READ COURSE-05-MST-IN
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF COURSE-05-SUBJ-IN(1:2) IS ALPHABETIC
                       AND COURSE-05-CRSE-IN(1:3) IS NUMERIC
                       THEN
                           MOVE COURSE-05-SUBJ-IN      TO SORT-SUBJ
                           MOVE COURSE-05-CRSE-IN      TO SORT-CRSE
                           MOVE COURSE-05-TITLE-IN     TO SORT-TITLE
                           MOVE COURSE-05-CREDITS-IN   TO SORT-CREDITS
                           RELEASE SORT-REC
                   END-IF               

               END-READ
           END-PERFORM.               
      *-----------------------------------------------------------------
       200-FILE-OUT.
           MOVE 'N' TO WS-EOF
           PERFORM UNTIL EOF
               RETURN SORT-WORK
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE SORT-REC TO COURSE-REC
                   IF SORT-TITLE IS NOT EQUAL WS-TITLE-COMP
                       WRITE COURSE-REC
                   END-IF
                   MOVE SORT-TITLE TO WS-TITLE-COMP
         
               END-RETURN
           END-PERFORM.    
                       
       
       
      *-----------------------------------------------------------------
 
