      ******************************************************************
      *PROGRAM : COURSE-CHANGE                                         *
      *AUTHOR  : CHRIS JEONG                                           *
      *DATE    : 02/26/2015                                            *
      *ABSTRACT: COURSE CHANGE FOR COURSE ACTIONS                      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-CHANGE AS "COURSE-CHANGE" IS INITIAL PROGRAM.
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
           05  FILLER              PIC X            VALUE SPACE.
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
           05  WS-ANOTHER              PIC X.
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
           05  LINE 1 COL 1  VALUE "COURSE-CHANGE".
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

       01  SCRN-INSTRUC-ANOTHER.
           05  LINE 15 COL 30                        
                                             VALUE'ENTER ANOTHER Y/N? '.
           05  LINE 16 COL 45 PIC X TO WS-ANOTHER    AUTO.
       
       01  SCRN-MSG.  
           05  LINE 18 COL 35 PIC X(40) FROM WS-MSG.
       

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY

           OPEN I-O ISAM-COURSE-IN.

           DISPLAY BLANK-SCREEN

           PERFORM UNTIL WS-ANOTHER = 'n' OR 'N'
               DISPLAY SCR-TITLE
               DISPLAY SCR-SUBJ-NAME
               DISPLAY SCR-CRSE-NAME

               ACCEPT SCR-SUBJ-NAME
               ACCEPT SCR-CRSE-NAME
      

               MOVE WS-KEY TO ISAM-REC-IO

               READ ISAM-COURSE-IN
                   INVALID KEY
                       MOVE   'ID NOT FOUND' TO WS-MSG
                       DISPLAY SCRN-MSG
                   NOT INVALID KEY
                       DISPLAY SCR-TITLE-NAME
                       DISPLAY SCR-CREDITS
                       ACCEPT SCR-TITLE-NAME
                       ACCEPT SCR-CREDITS 
                       MOVE WS-COURSE-TITLE TO ISAM-IO-TITLE
                       MOVE WS-COURSE-CREDITS TO ISAM-IO-CREDITS
                       REWRITE ISAM-REC-IO
                           INVALID KEY
                               MOVE   'INVALID ID' TO WS-MSG
                               DISPLAY SCRN-MSG
                           NOT INVALID KEY
                               STRING ISAM-IN-KEY ' UPDATED' INTO 
                               WS-MSG
                               DISPLAY SCRN-MSG
                               
                       END-REWRITE
                       DISPLAY SCRN-INSTRUC-ANOTHER
                       ACCEPT  SCRN-INSTRUC-ANOTHER
           END-PERFORM.
       
       CLOSE ISAM-COURSE-IN.

      *-----------------------------------------------------------------
       