      ******************************************************************
      *PROGRAM : STUDENT-ADD.CBL                                       *
      *AUTHOR  : CHRIS JEONG                                           *
      *DATE    : 4/5/2015                                              *
      *ABSTRACT: This program adds to the STUDENT-MASTER FILE          *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-ADD AS "STUDENT-ADD" IS INITIAL PROGRAM.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    
           SELECT ISAM-STUD-IO ASSIGN TO "../STUDENT-MASTER.DAT"       
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM    
                               RECORD KEY    IS ISAM-STUD-KEY
                               FILE STATUS   IS WS-STAT.
           SELECT STUD-LAST-ID ASSIGN TO "../STUD-LAST-ID.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.           
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *-----------------------------------------------------------------        
       FILE SECTION.
       FD  ISAM-STUD-IO.
       01  ISAM-REC-IO.
           03  ISAM-STUD-KEY.
               05  ISAM-IO-ID      PIC 9999.
           03  FILLER              PIC X       VALUE SPACE.
           03  ISAM-STUD-LNAME     PIC X(15).
           03  ISAM-SUTD-FNAME     PIC X(15).
           03  ISAM-STUD-ADDRESS   PIC X(25).
           03  ISAM-STUD-ZIP       PIC X(5).
           03  ISAM-STUD-HPHONE    PIC X(10).
           03  ISAM-STUD-CPHONE    PIC X(10).
           03  ISAM-STUD-WPHONE    PIC X(10).
           03  ISAM-STUD-GENDER    PIC X.
           03  ISAM-STUD-ACTIVE    PIC X.
       
       FD  STUD-LAST-ID.
           01 LAST-ID              PIC 9(4).
       

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
           03  WS-EOF                  PIC X       VALUE 'N'.           
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-STUD-ID       PIC 9999        VALUE 9999.
               05  WS-FILLER            PIC X.
               05  WS-STUD-LNAME        PIC X(15).
               05  WS-SUTD-FNAME        PIC X(15).
               05  WS-STUD-ADDRESS      PIC X(25).
               05  WS-STUD-ZIP          PIC X(5).
               05  WS-STUD-HPHONE.
                   10  WS-STUD-HPHONE1  PIC X(3).
                   10  WS-STUD-HPHONE2  PIC X(3).
                   10  WS-STUD-HPHONE3  PIC X(4).               
               05  WS-STUD-CPHONE.
                   10  WS-STUD-CPHONE1  PIC X(3).
                   10  WS-STUD-CPHONE2  PIC X(3).
                   10  WS-STUD-CPHONE3  PIC X(4).
               05  WS-STUD-WPHONE.
                   10  WS-STUD-WPHONE1  PIC X(3).
                   10  WS-STUD-WPHONE2  PIC X(3).
                   10  WS-STUD-WPHONE3  PIC X(4).
               05  WS-STUD-GENDER       PIC X.       
               05  WS-STUD-ACTIVE       PIC X.       
               
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
           03  LINE 07 COL 32 VALUE "ADD STUDENT".
      *     03  LINE 08 COL 32 VALUE 'ID           :'.
      *     03  LINE 08 COL 47 PIC X(4) TO WS-STUD-ID AUTO.
           03  LINE 09 COL 32 VALUE 'FIRST NAME   :'.
           03  LINE 09 COL 47 PIC X(15) TO WS-SUTD-FNAME  AUTO.
           03  LINE 10 COL 32 VALUE 'LAST NAME    :'.
           03  LINE 10 COL 47 PIC X(15) TO WS-STUD-LNAME  AUTO.         
           03  LINE 11 COL 32 VALUE 'ADDRESS      :'.
           03  LINE 11 COL 47 PIC X(25) TO WS-STUD-ADDRESS  AUTO.
           03  LINE 12 COL 32 VALUE 'ZIP          :'.
           03  LINE 12 COL 47 PIC X(5) TO WS-STUD-ZIP AUTO.
           03  LINE 13 COL 32 VALUE 'PH(Primary)  :'.
           03  LINE 13 COL 47 VALUE '('.
           03          COL 48 PIC X(3) TO  WS-STUD-HPHONE1 AUTO.
           03          COL 51 VALUE ')'.
           03          COL 52 PIC X(3) TO  WS-STUD-HPHONE2 AUTO.
           03          COL 55 VALUE '-'.
           03          COL 56 PIC X(4) TO  WS-STUD-HPHONE3 AUTO.
           03  LINE 14 COL 32 VALUE 'PH(Cell)     :'.
           03  LINE 14 COL 47 VALUE '('.
           03          COL 48 PIC X(3) TO  WS-STUD-CPHONE1 AUTO.
           03          COL 51 VALUE ')'.
           03          COL 52 PIC X(3) TO  WS-STUD-CPHONE2 AUTO.
           03          COL 55 VALUE '-'.
           03          COL 56 PIC X(4) TO  WS-STUD-CPHONE3 AUTO.
           03  LINE 15 COL 32 VALUE 'PH(Emergency):'.
           03  LINE 15 COL 47 VALUE '('.
           03          COL 48 PIC X(3) TO  WS-STUD-WPHONE1 AUTO.
           03          COL 51 VALUE ')'.
           03          COL 52 PIC X(3) TO  WS-STUD-WPHONE2 AUTO.
           03          COL 55 VALUE '-'.
           03          COL 56 PIC X(4) TO  WS-STUD-WPHONE3 AUTO.        
           03  LINE 16 COL 32 VALUE 'Gender       :'.
           03  LINE 16 COL 47 PIC X     TO WS-STUD-GENDER  AUTO.
           03  LINE 17 COL 32 VALUE 'Status       :'.
           03  LINE 17 COL 47 PIC X    TO WS-STUD-ACTIVE  AUTO.
      *     03  LINE 19 COL 35 PIC X(40) FROM WS-MSG.
           03  LINE 20 COL 35 VALUE 'ADD ANOTHER Y/N?'.
           03  LINE 20 COL 55 PIC X TO WS-CONT  AUTO.

           
       01  SCRN-ADD-ANOTHER.
           03  LINE 20 COL 33                     VALUE 'ADD ANOTHER?:'.
           03  LINE 21 COL 33                     VALUE '(Y/N)'.
           03  LINE 20 COL 45 PIC X  TO WS-CONT   AUTO.
      *-----------------------------------------------------------------        
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN OUTPUT ISAM-STUD-IO.
           OPEN I-O STUD-LAST-ID.
           
           DISPLAY BLANK-SCREEN
           
           PERFORM UNTIL WS-EOF EQUALS 'Y'
               READ STUD-LAST-ID
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   ADD 1 TO LAST-ID GIVING LAST-ID
                   MOVE LAST-ID TO WS-STUD-ID
                   REWRITE LAST-ID
           END-PERFORM.           
           
           PERFORM UNTIL WS-CONT='n' OR 'N'
               DISPLAY SCR-TITLE
               DISPLAY SCR-STUD-DATA
               ACCEPT  SCR-STUD-DATA
               
               MOVE WS-KEY TO ISAM-REC-IO
               WRITE ISAM-REC-IO

               PERFORM UNTIL WS-CONT='y' OR 'Y' OR 'n' OR 'N'
                   MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER
               END-PERFORM
           END-PERFORM.
           
           
           
           CLOSE ISAM-STUD-IO.
           CLOSE STUD-LAST-ID.
           EXIT PROGRAM.
           STOP RUN.
       
       
       
       
       
       
       
                                                                                