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
           03  ISAM-STUD-FNAME     PIC X(15).
           03  ISAM-STUD-ADDRESS   PIC X(25).
           03  ISAM-STUD-ZIP       PIC X(5).
           03  ISAM-STUD-HPHONE.
               05  ISAM-STUD-HPHONE1  PIC X(3).
               05  ISAM-STUD-HPHONE2  PIC X(3).
               05  ISAM-STUD-HPHONE3  PIC X(4).            
           03  ISAM-STUD-CPHONE.
               05  ISAM-STUD-CPHONE1  PIC X(3).
               05  ISAM-STUD-CPHONE2  PIC X(3).
               05  ISAM-STUD-CPHONE3  PIC X(4).
           03  ISAM-STUD-WPHONE.
               05  ISAM-STUD-WPHONE1  PIC X(3).
               05  ISAM-STUD-WPHONE2  PIC X(3).
               05  ISAM-STUD-WPHONE3  PIC X(4).           
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
           03  WS-FILLER15             PIC X(15)   VALUE SPACES.
           03  WS-FILLER25             PIC X(25)   VALUE SPACES.
           03  WS-FILLER10             PIC X(10)   VALUE SPACES.
           03  WS-FILLER05             PIC X(5)    VALUE SPACES.        
           03  WS-FILLER01             PIC X       VALUE SPACE.
           03  WS-ANOTHER              PIC X.               
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
           03  LINE 09 COL 22 VALUE 'FIRST NAME   :'.
           03  LINE 09 COL 37 PIC X(15) FROM ISAM-STUD-FNAME.
           03  LINE 10 COL 22 VALUE 'LAST NAME    :'.
           03  LINE 10 COL 37 PIC X(15) FROM ISAM-STUD-LNAME.
           03  LINE 11 COL 22 VALUE 'ADDRESS      :'.
           03  LINE 11 COL 37 PIC X(25) FROM ISAM-STUD-ADDRESS.
           03  LINE 12 COL 22 VALUE 'ZIP          :'.
           03  LINE 12 COL 37 PIC X(5) FROM ISAM-STUD-ZIP.
           03  LINE 13 COL 22 VALUE 'PH(Primary)  :'.
           03  LINE 13 COL 37 VALUE '('.   
           03          COL 38 PIC X(3) FROM  ISAM-STUD-HPHONE1.
           03          COL 41 VALUE ')'.
           03          COL 42 PIC X(3) FROM  ISAM-STUD-HPHONE2.
           03          COL 45 VALUE '-'.
           03          COL 46 PIC X(4) FROM  ISAM-STUD-HPHONE3.
           03  LINE 14 COL 22 VALUE 'PH(Cell)     :'.
           03  LINE 14 COL 37 VALUE '('.           
           03          COL 38 PIC X(3) FROM  ISAM-STUD-CPHONE1.
           03          COL 41 VALUE ')'.
           03          COL 42 PIC X(3) FROM  ISAM-STUD-CPHONE2.
           03          COL 45 VALUE '-'.
           03          COL 46 PIC X(4) FROM  ISAM-STUD-CPHONE3.
           03  LINE 15 COL 22 VALUE 'PH(Emergency):'.
           03          COL 37 VALUE '('.
           03          COL 38 PIC X(3) FROM  ISAM-STUD-WPHONE1.
           03          COL 41 VALUE ')'.
           03          COL 42 PIC X(3) FROM  ISAM-STUD-WPHONE2.
           03          COL 45 VALUE '-'.
           03          COL 46 PIC X(4) FROM  ISAM-STUD-WPHONE3.
           03  LINE 16 COL 22 VALUE 'Gender       :'.
           03  LINE 16 COL 37 PIC X     FROM ISAM-STUD-GENDER.
           03  LINE 17 COL 22 VALUE 'Status       :'.
           03  LINE 17 COL 37 PIC X    FROM ISAM-STUD-ACTIVE.
           03  LINE 19 COL 25 PIC X(40) FROM WS-MSG.
      *     03  LINE 20 COL 32 VALUE "ENTER ID OR X TO EXIT".
            
       01  SCR-STUD-CHANGE.

           03  LINE 09 COL 55 PIC X(15) TO WS-STUD-FNAME.
           03  LINE 10 COL 55 PIC X(15) TO WS-STUD-LNAME.
           03  LINE 11 COL 55 PIC X(25) TO WS-STUD-ADDRESS.
           03  LINE 12 COL 55 PIC X(5) TO WS-STUD-ZIP.
           03  LINE 13 COL 55 VALUE '('.   
           03          COL 56 PIC X(3) TO  WS-STUD-HPHONE1.
           03          COL 59 VALUE ')'.
           03          COL 60 PIC X(3) TO  WS-STUD-HPHONE2.
           03          COL 63 VALUE '-'.
           03          COL 64 PIC X(4) TO  WS-STUD-HPHONE3.
           03  LINE 14 COL 55 VALUE '('.   
           03          COL 56 PIC X(3) TO  WS-STUD-CPHONE1.
           03          COL 59 VALUE ')'.
           03          COL 60 PIC X(3) TO  WS-STUD-CPHONE2.
           03          COL 63 VALUE '-'.
           03          COL 64 PIC X(4) TO  WS-STUD-CPHONE3.
           03  LINE 15 COL 55 VALUE '('.   
           03          COL 56 PIC X(3) TO  WS-STUD-WPHONE1.
           03          COL 59 VALUE ')'.
           03          COL 60 PIC X(3) TO  WS-STUD-WPHONE2.
           03          COL 63 VALUE '-'.
           03          COL 64 PIC X(4) TO  WS-STUD-WPHONE3.
           03  LINE 16 COL 55 PIC X     TO WS-STUD-GENDER.
           03  LINE 17 COL 55 PIC X    TO WS-STUD-ACTIVE.
           03  LINE 19 COL 35 PIC X(40) FROM WS-MSGS. 
      *     03  LINE 21 COL 35 VALUE "TYPE UPDATED DATA".
            
           
      * 01  SCR-WANT-CHANGE.
      *     03  LINE 21 COL 35  VALUE "ENTER Y IF YOU WANT TO CHANGE".
      *     03          COL 65  PIC X   TO WS-CONT AUTO.
           

       01  SCRN-CONFIRM-ADD.
           03  LINE 20 COL 35                    VALUE 
               'ARE YOU SURE YOU WANT TO UPDATE   <Y/N>'.
           03  LINE 20 COL 67 PIC X TO WS-RESP AUTO.
           
       01  SCRN-ADD-ANOTHER.
           03  LINE 21 COL 35               VALUE 'ENTER ANOTHER?:'.
           03  LINE 21 COL 54               VALUE '(Y/N)'.
           03  LINE 21 COL 52 PIC X  TO WS-ANOTHER   AUTO.
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
           
           PERFORM 100-READ-LOOP UNTIL WS-ANOTHER EQUALS "N" OR "n"
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
               NOT INVALID KEY
                   READ ISAM-STUDENT-IO
                   DISPLAY SCR-STUD-DATA
                   DISPLAY SCR-STUD-CHANGE       
                   ACCEPT SCR-STUD-CHANGE

                   IF WS-STUD-FNAME = WS-FILLER15
                   ELSE
                       MOVE WS-STUD-FNAME       TO  ISAM-STUD-FNAME
                   END-IF
                   
                   IF WS-STUD-LNAME = WS-FILLER15
                   ELSE
                       MOVE WS-STUD-LNAME       TO  ISAM-STUD-LNAME
                   END-IF                       

                   IF WS-STUD-ADDRESS = WS-FILLER25
                   ELSE
                       MOVE WS-STUD-ADDRESS     TO  ISAM-STUD-ADDRESS
                   END-IF   

                   IF WS-STUD-ZIP = WS-FILLER05
                   ELSE
                       MOVE WS-STUD-ZIP         TO  ISAM-STUD-ZIP 
                   END-IF 
                   
                   IF WS-STUD-HPHONE = WS-FILLER10
                   ELSE
                       MOVE WS-STUD-HPHONE      TO  ISAM-STUD-HPHONE 
                   END-IF 

                   IF WS-STUD-CPHONE = WS-FILLER10
                   ELSE
                       MOVE WS-STUD-CPHONE      TO  ISAM-STUD-CPHONE 
                   END-IF          
                   
                   IF WS-STUD-WPHONE = WS-FILLER10
                   ELSE
                       MOVE WS-STUD-WPHONE      TO  ISAM-STUD-WPHONE 
                   END-IF                    

                   IF WS-STUD-GENDER = WS-FILLER01
                   ELSE
                       MOVE WS-STUD-GENDER      TO  ISAM-STUD-GENDER
                   END-IF                    

                   IF WS-STUD-ACTIVE = WS-FILLER01
                   ELSE
                       MOVE WS-STUD-ACTIVE      TO  ISAM-STUD-ACTIVE
                   END-IF
                   DISPLAY SCRN-CONFIRM-ADD
                   ACCEPT SCRN-CONFIRM-ADD
                   IF WS-RESP EQUALS 'Y' OR 'y'
                   REWRITE ISAM-REC-IO
                       INVALID KEY
                           MOVE   'INVALID ID' TO WS-MSGS
                           DISPLAY SCR-STUD-CHANGE
                       NOT INVALID KEY
                           STRING ISAM-STUD-KEY ' UPDATED' INTO 
                           WS-MSGS
                           DISPLAY SCR-STUD-CHANGE
                           
                   END-REWRITE
                   END-IF
                   IF WS-ANOTHER EQUALS 'N' OR 'n'
                       EXIT PROGRAM
                   END-IF
                   DISPLAY SCRN-ADD-ANOTHER
                   ACCEPT  SCRN-ADD-ANOTHER

                   DISPLAY SPACES AT LINE 19 COL 1
                   DISPLAY SPACES AT LINE 20 COL 1
                   DISPLAY SPACES AT LINE 21 COL 1

                
                   PERFORM UNTIL WS-ANOTHER='y' OR 'Y' OR 'n' OR 'N'
                       MOVE 'PLEASE ENTER Y OR N' TO WS-MSG
                       DISPLAY SCRN-ADD-ANOTHER
                       ACCEPT  SCRN-ADD-ANOTHER
                       MOVE "ENTER Y OR N" TO WS-MSG
                   END-PERFORM
                   ACCEPT SCR-STUD-DATA                    

           END-START
           

      *-----------------------------------------------------------------          
           
       
       
      *----------------------------------------------------------------- 
                                                                                