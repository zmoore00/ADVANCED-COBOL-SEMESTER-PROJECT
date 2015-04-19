      ******************************************************************
      *PROGRAM : Pull out specific course information from schedule    *                  
      *AUTHOR  : Chris Jeong                                          *
      *DATE    : 02-15-2015                                            *
      *ABSTRACT: Builds course master file                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-BUILD IS INITIAL PROGRAM.
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
                                   ORGANIZATION  IS INDEXED
                                   ACCESS        IS RANDOM
                                   RECORD KEY    IS ISAM-OUT-KEY
                                   FILE STATUS   IS WS-OUT-STATUS.                                   
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD COURSE-01-MST-IN.
       01  COURSE-01-RECI.
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-01-SUBJ-IN   PIC X(4).
           05  FILLER             PIC X            VALUE SPACE.
           05  COURSE-01-CRSE-IN   PIC X(4).
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-01-TITLE-IN    PIC X(30).
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-01-CREDITS-IN  PIC X(3).
           
       FD COURSE-05-MST-IN.
       01  COURSE-05-RECI.
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-05-SUBJ-IN   PIC X(4).         
           05  FILLER             PIC X            VALUE SPACE.
           05  COURSE-05-CRSE-IN   PIC X(4).     
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-05-TITLE-IN    PIC X(30).
           05  FILLER             PIC X(6)         VALUE SPACES.
           05  COURSE-05-CREDITS-IN  PIC X(3).           
       
       SD SORT-WORK.
       01 SORT-REC.
           05  SORT-SUBJ           PIC X(4).
           05  SORT-CRSE           PIC X(4).
           05  FILLER              PIC X            VALUE SPACE.
           05  SORT-TITLE          PIC X(30).
           05  FILLER              PIC X            VALUE SPACE.
           05  SORT-CREDITS        PIC X(3).
      *     05  FILLER              PIC X(36)        VALUE SPACE.
           
       FD COURSE-MST-OUT.
       01  COURSE-REC.
           05  ISAM-OUT-KEY.
               10  COURSE-SUBJ-OUT PIC X(4).
               10  COURSE-CRSE-OUT PIC X(4).
           05  FILLER              PIC X            VALUE SPACE.    
           05  COURSE-TITLE-OUT    PIC X(30).
           05  FILLER              PIC X                VALUE SPACE.
           05  COURSE-CREDITS-OUT  PIC X(3).
      *     05  FILLER              PIC X(36)            VALUE SPACE.
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
       01  WS-TEMP1                PIC X(22).       

       01  WS-TEST                 PIC X(4)    VALUE "TEST".

       01  WS-OUT-STATUS           PIC XX.
       01  WS-VARS.
           05  WS-EOF            PIC X           VALUE "N".
           05  WS-EOF2            PIC X           VALUE "N".
           05  WS-EOF3             PIC X           VALUE "N".
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
           03  LINE 20 COL 33 "PRESS ENTER TO RETURN".
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
                
               DISPLAY SCR-INFO
               DISPLAY EXIT-SCREEN
               ACCEPT WS-EXIT              
           
           CLOSE COURSE-01-MST-IN.
           CLOSE COURSE-05-MST-IN.
           CLOSE COURSE-MST-OUT.
           
           EXIT PROGRAM.           
       
       
       
      *-----------------------------------------------------------------
       100-FILE-IN.
           PERFORM UNTIL WS-EOF EQUALS 'Y'
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
      *                     DISPLAY SORT-REC
                           RELEASE SORT-REC   
                   END-IF        
               END-READ
           END-PERFORM.
           
           PERFORM UNTIL WS-EOF2 EQUALS 'Y'
               READ COURSE-05-MST-IN
               AT END
                   MOVE 'Y' TO WS-EOF2
               NOT AT END
                   IF COURSE-05-SUBJ-IN(1:2) IS ALPHABETIC
                       AND COURSE-05-CRSE-IN(1:3) IS NUMERIC
                       THEN
                           MOVE COURSE-05-SUBJ-IN      TO SORT-SUBJ
                           MOVE COURSE-05-CRSE-IN      TO SORT-CRSE
                           MOVE COURSE-05-TITLE-IN     TO SORT-TITLE
                           MOVE COURSE-05-CREDITS-IN   TO SORT-CREDITS
      *                     DISPLAY SORT-REC
                           RELEASE SORT-REC
                   END-IF               

               END-READ
           END-PERFORM.               
      *-----------------------------------------------------------------
       200-FILE-OUT.
           MOVE 'N' TO WS-EOF3
           PERFORM UNTIL WS-EOF3 = 'Y'
               RETURN SORT-WORK
                   AT END
                       MOVE 'Y' TO WS-EOF3
                   NOT AT END
                       MOVE SORT-REC TO COURSE-REC
      *                  DISPLAY WS-TEST
      *                  DISPLAY COURSE-REC
                        WRITE COURSE-REC
                        MOVE SORT-TITLE TO COURSE-TITLE-OUT
         
               END-RETURN
           END-PERFORM.    
                       
       
       
      *-----------------------------------------------------------------
 
