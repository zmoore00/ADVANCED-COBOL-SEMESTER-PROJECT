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
           SELECT ISAM-STUDENT-IO ASSIGN TO "../STUDENT-MASTER.DAT"     
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC   
                               RECORD KEY    IS ISAM-STUD-KEY
                               FILE STATUS   IS WS-STAT.
      *----------------------------------------------------------------- 
       DATA DIVISION.
      *-----------------------------------------------------------------        
       FILE SECTION.
       FD  ISAM-STUDENT-IO.
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
           03  WS-MSGS                  PIC X(40)   VALUE SPACES.          
           03  WS-RESP                 PIC X       VALUE SPACES.
           03  WS-STAT                 PIC XX      VALUE SPACES.
           03  WS-CONT                 PIC X       VALUE 'Y'.
           03  WS-CON                  PIC X       VALUE 'Y'.
               
       01  WS-REC.
           03  WS-KEY.
               05  WS-STUD-ID       PIC 9999        VALUE 9999.
               05  WS-FILLER            PIC X.
               05  WS-STUD-LNAME        PIC X(15).
               05  WS-STUD-FNAME        PIC X(15).
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
           03  LINE 1 COL 1  VALUE "STUDENT-CHANGE".
           03  LINE 1 COL 37 VALUE "UAFS".
           03  LINE 1 COL 71 FROM DISPLAY-DATE.
           
       01  SCR-STUD-DATA.
           03  LINE 06 COL 22 VALUE "CHANGE STUDENT".
           03  LINE 07 COL 22 VALUE "ENTER ID     :".
           03  LINE 07 COL 37 PIC X(4) TO WS-STUD-ID AUTO.
      *     03  LINE 08 COL 32 VALUE 'ID           :'.
      *     03  LINE 08 COL 47 PIC X(4) FROM ISAM-IO-ID.
           03  LINE 09 COL 22 VALUE 'FIRST NAME   :'.
           03  LINE 09 COL 37 PIC X(15) FROM ISAM-SUTD-FNAME.
           03  LINE 10 COL 22 VALUE 'LAST NAME    :'.
           03  LINE 10 COL 37 PIC X(15) FROM ISAM-STUD-LNAME.
           03  LINE 11 COL 22 VALUE 'ADDRESS      :'.
           03  LINE 11 COL 37 PIC X(25) FROM ISAM-STUD-ADDRESS.
           03  LINE 12 COL 22 VALUE 'ZIP          :'.
           03  LINE 12 COL 37 PIC X(5) FROM ISAM-STUD-ZIP.
           03  LINE 13 COL 22 VALUE 'PH(Primary)  :'.
           03  LINE 13 COL 37 PIC X(13) FROM   ISAM-STUD-HPHONE.
           03  LINE 14 COL 22 VALUE 'PH(Cell)     :'.
           03  LINE 14 COL 37 PIC X(13) FROM ISAM-STUD-CPHONE.
           03  LINE 15 COL 22 VALUE 'PH(Emergency):'.
           03  LINE 15 COL 37 PIC X(13) FROM ISAM-STUD-WPHONE.
           03  LINE 16 COL 22 VALUE 'Gender       :'.
           03  LINE 16 COL 37 PIC X     FROM ISAM-STUD-GENDER.
           03  LINE 17 COL 22 VALUE 'Status       :'.
           03  LINE 17 COL 37 PIC X    FROM ISAM-STUD-ACTIVE.
           03  LINE 19 COL 25 PIC X(40) FROM WS-MSG.
           03  LINE 20 COL 32 VALUE "ENTER ID OR X TO EXIT".
      *     03  LINE 20 COL 35 VALUE 'ENTER ANOTHER Y/N?'.
      *     03  LINE 20 COL 55 PIC X TO WS-CONT  AUTO.
            
       01  SCR-STUD-CHANGE.

      *     03  LINE 09 COL 32 VALUE 'FIRST NAME   :'.
           03  LINE 09 COL 55 PIC X(15) TO WS-STUD-FNAME.
      *     03  LINE 10 COL 32 VALUE 'LAST NAME    :'.
           03  LINE 10 COL 55 PIC X(15) TO WS-STUD-LNAME.
      *     03  LINE 11 COL 32 VALUE 'ADDRESS      :'.
           03  LINE 11 COL 55 PIC X(25) TO WS-STUD-ADDRESS.
      *     03  LINE 12 COL 32 VALUE 'ZIP          :'.
           03  LINE 12 COL 55 PIC X(5) TO WS-STUD-ZIP.
      *     03  LINE 13 COL 32 VALUE 'PH(Primary)  :'.
           03  LINE 13 COL 55 PIC X(13) TO   WS-STUD-HPHONE.
      *     03  LINE 14 COL 32 VALUE 'PH(Cell)     :'.
           03  LINE 14 COL 55 PIC X(13) TO WS-STUD-CPHONE.
      *     03  LINE 15 COL 32 VALUE 'PH(Emergency):'.
           03  LINE 15 COL 55 PIC X(13) TO WS-STUD-WPHONE.
      *     03  LINE 16 COL 32 VALUE 'Gender       :'.
           03  LINE 16 COL 55 PIC X     TO WS-STUD-GENDER.
      *     03  LINE 17 COL 32 VALUE 'Status       :'.
           03  LINE 17 COL 55 PIC X    TO WS-STUD-ACTIVE.
           03  LINE 19 COL 53 PIC X(40) FROM WS-MSGS. 
           03  LINE 21 COL 35 VALUE "TYPE UPDATED DATA".
            
           
       01  SCR-WANT-CHANGE.
           03  LINE 21 COL 35  VALUE "ENTER Y IF YOU WANT TO CHANGE".
           03          COL 65  PIC X   TO WS-CONT AUTO.
           
           
       01  SCRN-ADD-ANOTHER.
           03  LINE 20 COL 35               VALUE 'ENTER ANOTHER?:'.
           03  LINE 20 COL 54               VALUE '(Y/N)'.
           03  LINE 20 COL 52 PIC X  TO WS-CONT   AUTO.
      *-----------------------------------------------------------------        
       PROCEDURE DIVISION.
       000-MAIN-MODULE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-CURRENT-MONTH TO MONTH-DISPLAY
           MOVE WS-CURRENT-DAY   TO DAY-DISPLAY
           MOVE WS-CURRENT-YEAR  TO YEAR-DISPLAY
           
           OPEN I-O ISAM-STUDENT-IO
           DISPLAY BLANK-SCREEN

           DISPLAY SCR-TITLE
           DISPLAY SCR-STUD-DATA
           ACCEPT  SCR-STUD-DATA
           
           PERFORM 100-READ-LOOP UNTIL WS-STUD-ID = "X" OR "x"
      *     DISPLAY SCR-WANT-CHANGE
      *     ACCEPT SCR-WANT-CHANGE                     
      *     PERFORM UNTIL WS-CONT='n' OR 'N'
      *     END-PERFORM.
           CLOSE ISAM-STUDENT-IO.
           EXIT PROGRAM.
           STOP RUN.
      *----------------------------------------------------------------- 
       100-READ-LOOP.
           MOVE SPACES TO WS-MSG
           MOVE WS-STUD-ID TO ISAM-STUD-KEY
           START ISAM-STUDENT-IO KEY EQUAL TO ISAM-STUD-KEY
               INVALID KEY
                   MOVE "NOT MATCHED" TO WS-MSG
                   DISPLAY SCR-STUD-DATA
                   ACCEPT SCR-STUD-DATA
      *             DISPLAY WS-MSG
      *             DISPLAY SCRN-ADD-ANOTHER
      *             ACCEPT SCRN-ADD-ANOTHER
      *             DISPLAY BLANK-SCREEN                   
               NOT INVALID KEY
      *             DISPLAY SCR-WANT-CHANGE
                   READ ISAM-STUDENT-IO
                   DISPLAY SCR-STUD-DATA
      *             DISPLAY SCR-WANT-CHANGE
      *             ACCEPT SCR-WANT-CHANGE
                   DISPLAY SCR-STUD-CHANGE     
                   ACCEPT SCR-STUD-CHANGE
                   MOVE WS-STUD-LNAME       TO  ISAM-STUD-LNAME  
                   MOVE WS-STUD-FNAME       TO  ISAM-SUTD-FNAME  
                   MOVE WS-STUD-ADDRESS     TO  ISAM-STUD-ADDRESS
                   MOVE WS-STUD-ZIP         TO  ISAM-STUD-ZIP    
                   MOVE WS-STUD-HPHONE      TO  ISAM-STUD-HPHONE 
                   MOVE WS-STUD-CPHONE      TO  ISAM-STUD-CPHONE 
                   MOVE WS-STUD-WPHONE      TO  ISAM-STUD-WPHONE 
                   MOVE WS-STUD-GENDER      TO  ISAM-STUD-GENDER
                   MOVE WS-STUD-ACTIVE      TO  ISAM-STUD-ACTIVE
                   REWRITE ISAM-REC-IO
                       INVALID KEY
                           MOVE   'INVALID ID' TO WS-MSGS
                           DISPLAY SCR-STUD-CHANGE
                       NOT INVALID KEY
                           STRING ISAM-STUD-KEY ' UPDATED' INTO 
                           WS-MSGS
                           DISPLAY SCR-STUD-CHANGE
                           
                   END-REWRITE
                   ACCEPT SCR-STUD-DATA

           END-START


           
           
      *-----------------------------------------------------------------          

           
       
       
       
                                                                                