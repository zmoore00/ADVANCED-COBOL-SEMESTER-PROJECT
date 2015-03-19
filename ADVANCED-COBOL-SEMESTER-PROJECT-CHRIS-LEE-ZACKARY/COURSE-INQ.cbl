      ******************************************************************
      *PROGRAM : COURSE-INQ                                            *
      *AUTHOR  : CHIRS JEONG                                           *
      *DATE    : 02/26/2015                                            *
      *ABSTRACT: COURSE INQ. FOR COURSE ACTIONS                        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURCE-INQ AS "COURSE-INQ" IS INITIAL PROGRAM.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-COURSE-IN ASSIGN TO "../COURSE-MASTER.DAT"       
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
               10  ISAM-IO-CRSE PIC X(4).
           05  FILLER              PIC X            VALUE SPACE.
           05  ISAM-IO-TITLE     PIC X(30).
           05  FILLER              PIC X            VALUE SPACE.
           05  ISAM-IO-CREDITS  PIC X(3).  
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
           
       01  WS-VARS.
           03  WS-MSG                  PIC X(43)   VALUE SPACES.
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  WS-ANOTHER              PIC X.
       01  WS-REC.
           05  WS-KEY.
               10  WS-COURSE-SUBJ PIC X(4)              VALUE SPACES.
               10  WS-COURSE-CRSE PIC X(4)              VALUE SPACES.
           05  FILLER              PIC X            VALUE SPACE.
           05  WS-COURSE-TITLE     PIC X(30).
           05  FILLER              PIC X                VALUE SPACE.
           05  WS-COURSE-CREDITS  PIC X(3).              
           
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLANK-SCREEN.
           05  BLANK SCREEN.    
           
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "COURSE-INQ".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCRN-KEY-REQ.    
           05  LINE 07 COL 32 VALUE "COURSE SEARCH".
           05  LINE 09 COL 34 VALUE "SUBJECT:".        
           05  LINE 09 COL 43 PIC X(4)  TO WS-COURSE-SUBJ  AUTO.      
           05  LINE 10 COL 35 VALUE "COURSE:".
           05  LINE 10 COL 43 PIC X(4)  TO WS-COURSE-CRSE  AUTO.
           03  LINE 14 COL 35 VALUE '  (X=EXIT)'.
           03  LINE 15 COL 35 PIC X(80) FROM WS-MSG.       
           
       01  SCRN-COURSE-DATA.    
           03  LINE 09 COL 34                        VALUE 'SUBJECT:'.  
           03  LINE 09 COL 43 PIC X(4) 
                               FROM WS-COURSE-SUBJ VALUE SPACES.
           03  LINE 10 COL 35                        VALUE 'COURSE:'.   
           03  LINE 10 COL 43 PIC X(4) 
                               FROM WS-COURSE-CRSE  VALUE SPACES.
           03  LINE 11 COL 36                        VALUE 'TITLE:'.    
           03  LINE 11 COL 45 PIC X(30) 
                               FROM WS-COURSE-TITLE  VALUE SPACES.
           03  LINE 12 COL 35                        VALUE 'CREDIT:'.   
           03  LINE 12 COL 45 PIC X(3) 
                               FROM WS-COURSE-CREDITS  VALUE SPACES.
           03  LINE 13 COL 45 VALUE 'ENTER ANOTHER Y/N?'.
           03  LINE 14 COL 45 PIC X TO WS-ANOTHER  AUTO.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN INPUT ISAM-COURSE-IN.
                                                                        
           PERFORM UNTIL (WS-COURSE-SUBJ='X' OR 'x')                    
                                       OR (WS-COURSE-CRSE='X' OR 'x')   
               DISPLAY SCR-TITLE
               DISPLAY SCRN-KEY-REQ
               ACCEPT  SCRN-KEY-REQ
               MOVE WS-KEY TO ISAM-IN-KEY
               
               READ ISAM-COURSE-IN
                   INVALID KEY
                       MOVE   'INVALID ID' TO WS-MSG
                   NOT INVALID KEY
                       MOVE ISAM-IO-CRSE  TO WS-COURSE-CRSE
                       MOVE ISAM-IO-SUBJ  TO WS-COURSE-SUBJ
                       MOVE ISAM-IO-TITLE TO WS-COURSE-TITLE
                       MOVE ISAM-IO-CREDITS TO WS-COURSE-CREDITS
                       DISPLAY SCR-TITLE
                       DISPLAY SCRN-COURSE-DATA
                       ACCEPT WS-ANOTHER
                       IF WS-ANOTHER EQUALS 'N' OR 'n'
                           EXIT PROGRAM
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE ISAM-COURSE-IN.
           EXIT PROGRAM.
           STOP RUN.           

       
      *-----------------------------------------------------------------
