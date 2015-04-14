      ******************************************************************
      *PROGRAM : STUDENT-CHANGE.CBL                                    *
      *AUTHOR  : CHRIS JEONG                                           *
      *DATE    : 4/5/2015                                              *
      *ABSTRACT: This program changes info. of STUDENT-MASTER FILE     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-CHANGE  IS INITIAL PROGRAM.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-INSTRUC-IO ASSIGN TO "../STUDENT-MASTER.DAT"     
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM    
                               RECORD KEY    IS ISAM-STUD-KEY
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *-----------------------------------------------------------------        
       FILE SECTION.
       FD  ISAM-INSTRUC-IO.
       01  ISAM-REC-IO.
           03  ISAM-STUD-KEY.
               05  ISAM-IO-ID      PIC 9999.
           03  ISAM-STUD-LNAME     PIC X(15).
           03  ISAM-SUTD-FNAME     PIC X(15).
           03  ISAM-STUD-ADDRESS   PIC X(25).
           03  ISAM-STUD-ZIP       PIC X(5).
           03  ISAM-STUD-HPHONE    PIC X(10).
           03  ISAM-STUD-CPHONE    PIC X(10).
           03  ISAM-STUD-WPHONE    PIC X(10).
           03  ISAM-STUD-GENDER    PIC X.
           03  ISAM-STUD-ACTIVE    PIC X.

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
           03  WS-CONT                 PIC X       VALUE 'Y'.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-STUD-ID       PIC 9999        VALUE 9999.
               05  WS-STUD-NAME     PIC X(22)       VALUE SPACES.
           03  WS-STUD-LNAME        PIC X(15).
           03  WS-SUTD-FNAME        PIC X(15).
           03  WS-STUD-ADDRESS      PIC X(25).
           
           03  WS-STUD-CITY         PIC X(10).
           03  WS-STUD-STATE        PIC X(2).
           
           03  WS-STUD-ZIP          PIC X(5).
           03  WS-STUD-HPHONE       PIC X(10).
           03  WS-STUD-CPHONE       PIC X(10).
           03  WS-STUD-WPHONE       PIC X(10).
           03  WS-STUD-GENDER       PIC X.         
           03  WS-STUD-ACTIVE       PIC X.
               
      *-----------------------------------------------------------------        
       SCREEN SECTION.
       01  BLANK-SCREEN.
           03  BLANK SCREEN.
       
       01  SCR-TITLE.
           03  BLANK SCREEN.
           03  LINE 1 COL 1  VALUE "STUDENT-ADD".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-STUD-DATA.
           03  LINE 07 COL 32 VALUE "INQ STUDENT".
           03  LINE 08 COL 32 VALUE 'ID           :'.
           03  LINE 08 COL 47 PIC X(4) FROM WS-STUD-ID AUTO.
           03  LINE 09 COL 32 VALUE 'FIRST NAME   :'.
           03  LINE 09 COL 47 PIC X(15) FROM WS-SUTD-FNAME  AUTO.
           03  LINE 10 COL 32 VALUE 'LAST NAME    :'.
           03  LINE 10 COL 47 PIC X(15) FROM WS-STUD-LNAME  AUTO.       
           03  LINE 11 COL 32 VALUE 'ADDRESS      :'.
           03  LINE 11 COL 47 PIC X(25) FROM WS-STUD-ADDRESS  AUTO.
           03  LINE 12 COL 32 VALUE 'ZIP          :'.
           03  LINE 12 COL 47 PIC X(5) FROM WS-STUD-ZIP AUTO.
           03  LINE 13 COL 32 VALUE 'PH(Primary)  :'.
           03  LINE 13 COL 47 PIC X(13) FROM   WS-STUD-HPHONE AUTO.
           03  LINE 14 COL 32 VALUE 'PH(Cell)     :'.
           03  LINE 14 COL 47 PIC X(13) FROM WS-STUD-CPHONE  AUTO.
           03  LINE 15 COL 32 VALUE 'PH(Emergency):'.
           03  LINE 15 COL 47 PIC X(13) FROM WS-STUD-WPHONE  AUTO.      
           03  LINE 16 COL 32 VALUE 'Gender       :'.
           03  LINE 16 COL 47 PIC X     FROM WS-STUD-GENDER  AUTO.
           03  LINE 17 COL 32 VALUE 'Status       :'.
           03  LINE 17 COL 47 PIC X    FROM WS-STUD-ACTIVE  AUTO.
           03  LINE 19 COL 35 PIC X(40) FROM WS-MSG.
           03  LINE 20 COL 35 VALUE 'ENTER ANOTHER Y/N?'.
           03  LINE 20 COL 55 PIC X TO WS-CONT  AUTO.

           
       01  SCRN-ADD-ANOTHER.
           03  LINE 11 COL 33                     VALUE 'ADD ANOTHER?:'.
           03  LINE 12 COL 33                     VALUE '(Y/N)'.
           03  LINE 11 COL 45 PIC X  TO WS-CONT   AUTO.
      *-----------------------------------------------------------------        
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN I-O ISAM-INSTRUC-IO.
           DISPLAY BLANK-SCREEN
           PERFORM UNTIL WS-CONT='n' OR 'N'
               DISPLAY SCR-TITLE
               DISPLAY SCR-STUD-DATA
               ACCEPT  SCR-STUD-DATA
               
               MOVE WS-KEY TO ISAM-REC-IO
               WRITE ISAM-REC-IO

               
      *         DISPLAY SCRN-ADD-ANOTHER
      *         ACCEPT  SCRN-ADD-ANOTHER
               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
               END-PERFORM
           END-PERFORM.
           
           
           
           CLOSE ISAM-INSTRUC-IO.
           EXIT PROGRAM.
           STOP RUN.
       
       
       
       
       
       
       
                                                                                