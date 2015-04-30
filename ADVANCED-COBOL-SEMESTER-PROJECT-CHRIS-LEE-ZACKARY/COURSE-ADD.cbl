      ******************************************************************
      *PROGRAM : COURSE-ADD                                            *
      *AUTHOR  : CHRIS JEONG                                           *
      *DATE    : 02/26/2015                                            *
      *ABSTRACT: COURSE ADD FOR COURSE ACTIONS                         *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-ADD AS "COURSE-ADD" IS INITIAL PROGRAM.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-COURSE-IN ASSIGN TO "../COURSE-MASTER.TXT"       
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM    
                               RECORD KEY    IS ISAM-IN-KEY
                               FILE STATUS   IS WS-STAT.       
      *-----------------------------------------------------------------
       DATA DIVISION.
     
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  ISAM-COURSE-IN.
       01  ISAM-REC-IO.
           05  ISAM-IN-KEY.
               10  ISAM-IO-SUBJ PIC X(4).
               10  ISAM-IO-CRSE PIC X(5).
           05  FILLER              PIC X            VALUE SPACE.
           05  ISAM-IO-TITLE     PIC X(30).
           05  FILLER              PIC X                VALUE SPACE.
           05  ISAM-IO-CREDITS  PIC X(3).       
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-DATE.
           05  WS-CURRENT-YEAR     PIC 9999.
           05  WS-CURRENT-MONTH    PIC 99.
           05  WS-CURRENT-DAY      PIC 99.
       01  DISPLAY-DATE.
           05  MONTH-DISPLAY       PIC 99.
           05  FILLER              PIC X           VALUE "/".
           05  DAY-DISPLAY         PIC 99.
           05  FILLER              PIC X           VALUE "/".
           05  YEAR-DISPLAY        PIC 9999.
           
       01  MISC-VARS.
           05  WS-MSG                  PIC X(43)   VALUE SPACES.
           05  WS-RESP                 PIC X       VALUE SPACES.
           05  WS-STAT                 PIC XX      VALUE SPACES.
           05  WS-CONT                 PIC X       VALUE 'Y'.
               
       01  WS-REC.
           05  WS-KEY.
               10  WS-COURSE-SUBJ     PIC X(4)         VALUE SPACES.
               10  WS-COURSE-CRSE     PIC X(5)         VALUE SPACES.
               10  WS-FILLER           PIC X           VALUE SPACE.
               10  WS-COURSE-TITLE     PIC X(30)       VALUE SPACES.
               10  WS-FILLER1           PIC X          VALUE SPACE.
               10  WS-COURSE-CREDITS  PIC X(3)         VALUE SPACES.     
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLANK-SCREEN.
           05  BLANK SCREEN.
           
       01  SCR-TITLE.
           05  BLANK SCREEN.
           05  LINE 1 COL 1  VALUE "COURSE-ADD".
           05  LINE 1 COL 37 VALUE "UAFS".
           05  LINE 1 COL 71 FROM DISPLAY-DATE.       
           
       01  SCR-SUBJ-NAME.
           05  LINE 07 COL 32 VALUE "ADD COURSE".
           05  LINE 09 COL 32 VALUE 'SUBJECT:'.
           05  LINE 09 COL 40 PIC X(4) TO WS-COURSE-SUBJ  AUTO.
           
           
       01  SCR-CRSE-NAME.
           05  LINE 10 COL 32 VALUE 'COURSE:'.
           05  LINE 10 COL 40 PIC X(5) TO WS-COURSE-CRSE  AUTO.
           
       01  SCR-TITLE-NAME.
           05  LINE 11 COL 32 VALUE 'TITLE:'.
           05  LINE 11 COL 40 PIC X(30) TO WS-COURSE-TITLE  AUTO.     
                                                                          
       01  SCR-CREDITS.
           05  LINE 12 COL 32 VALUE 'CREDIT:'.
           05  LINE 12 COL 40 PIC X(3) TO WS-COURSE-CREDITS  AUTO. 
           05  LINE 13 COL 35 PIC X(40) FROM WS-MSG.   
           
       01  SCRN-CONFIRM-ADD.
           03  LINE 14 COL 35                    VALUE 
               'ARE YOU SURE YOU WANT TO ADD'.
           03  LINE 15 COL 35 PIC X(5) FROM WS-COURSE-CRSE.
           03  LINE 15 COL 43 PIC X(30) FROM WS-COURSE-TITLE.
           03  LINE 16 COL 35 PIC X TO WS-RESP AUTO.                                                               

           
       01  SCRN-ADD-ANOTHER.
           05  LINE 14 COL 33                     VALUE 'ADD ANOTHER?:'.
           05  LINE 15 COL 33                     VALUE '(Y/N)'.
           05  LINE 15 COL 45 PIC X  TO WS-CONT   AUTO.
       

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN I-O ISAM-COURSE-IN.
           DISPLAY BLANK-SCREEN
           PERFORM UNTIL WS-CONT='n' OR 'N'
               DISPLAY SCR-TITLE
               DISPLAY SCR-SUBJ-NAME
               DISPLAY SCR-CRSE-NAME
               DISPLAY SCR-TITLE-NAME
               DISPLAY SCR-CREDITS
               ACCEPT SCR-SUBJ-NAME
               ACCEPT SCR-CRSE-NAME
               ACCEPT SCR-TITLE-NAME
               ACCEPT SCR-CREDITS               
               
               MOVE WS-KEY TO ISAM-REC-IO
               
               DISPLAY SCRN-CONFIRM-ADD
               ACCEPT SCRN-CONFIRM-ADD
               IF WS-RESP EQUALS 'Y' OR 'y'
               WRITE ISAM-REC-IO
               END-IF
               
               DISPLAY SPACES AT LINE 14 COL 1
               DISPLAY SPACE AT LINE 15 COL 1
               DISPLAY SPACE AT LINE 16 COL 1

               
               DISPLAY SCRN-ADD-ANOTHER
               ACCEPT  SCRN-ADD-ANOTHER
               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
               END-PERFORM
           END-PERFORM.
           
           
           
           CLOSE ISAM-COURSE-IN.
           EXIT PROGRAM.
           STOP RUN.
            
       
      *-----------------------------------------------------------------

   