      ******************************************************************
      *PROGRAM : INSTRUC-CHANGE.CBL                                   *
      *AUTHOR  : ZACKARY MOORE                                         *
      *DATE    : 3/16/2015                                             *
      *ABSTRACT: This program ALLOWS TO CHANGE INSTRUCTOR-MASTER FILE  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSTRUC-ADD AS "INSTRUC-CHANGE" IS INITIAL PROGRAM.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-INSTRUC-IN ASSIGN TO "../INSTRUCTOR-MASTER.DAT"  
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM    
                               RECORD KEY    IS ISAM-IN-KEY
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
       FD  ISAM-INSTRUC-IN.
       01  ISAM-REC-IO.
           03  ISAM-IN-KEY.
               05  ISAM-IO-ID   PIC 9999.
           03  FILLER       PIC X           VALUE SPACES.
           03  ISAM-IO-NAME PIC X(22).
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
           
       01  MISC-VARS.
           03  WS-MSG                  PIC X(40)   VALUE SPACES.
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  WS-ANOTHER              PIC X.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-INSTRUC-ID       PIC 9999        VALUE ZEROS.
               05  WS-FILLER           PIC X           VALUE SPACES.
               05  WS-INSTRUC-NAME     PIC X(22)       VALUE SPACES.
      *----------------------------------------------------------------- 
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "INDTRUC-CHANGE".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-INSTRUC-ID.
           03  LINE 07 COL 32 VALUE "INSTRUCTOR CHANGE".
           03  LINE 09 COL 32 VALUE 'INSTRUCTOR ID:'.
           03  LINE 09 COL 48 PIC X(4) TO WS-INSTRUC-ID  AUTO.
           
       01  SCR-INSTRUC-NAME.
           03  LINE 10 COL 32 VALUE '  CHANGE NAME: '.
           03  LINE 10 COL 48 PIC X(22) TO WS-INSTRUC-NAME AUTO.
           
       01  SCRN-INSTRUC-ANOTHER.
           03  LINE 12 COL 30                        
                                             VALUE'ENTER ANOTHER Y/N? '.
           03  LINE 13 COL 45 PIC X TO WS-ANOTHER    AUTO.
       
       01  SCRN-MSG.  
           03  LINE 15 COL 35 PIC X(40) FROM WS-MSG.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN I-O ISAM-INSTRUC-IN.

           PERFORM UNTIL (WS-ANOTHER = 'n' OR 'N')
               DISPLAY SCR-TITLE
               DISPLAY SCR-INSTRUC-ID
               ACCEPT  SCR-INSTRUC-ID
               MOVE WS-KEY TO ISAM-IN-KEY
               
               READ ISAM-INSTRUC-IN
                   INVALID KEY
                       MOVE   'ID NOT FOUND' TO WS-MSG
                       DISPLAY SCRN-MSG
                   NOT INVALID KEY
                       DISPLAY SCR-INSTRUC-NAME
                       ACCEPT  SCR-INSTRUC-NAME
                       MOVE WS-INSTRUC-NAME TO ISAM-IO-NAME
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
       
       CLOSE ISAM-INSTRUC-IN.


