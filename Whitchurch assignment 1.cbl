       program-id. Program1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         SELECT TABLE-FILE ASSIGN TO "INFILE1.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
         SELECT TRANS-FILE ASSIGN TO "INFILE2.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
         SELECT REPORT-FILE ASSIGN TO "OUTPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
   
       data division.
       FILE SECTION.
       
       FD TABLE-FILE.
           01 CLERK-REC.
               05 CLERK-NUM                PIC 9(9).
               05 CLERK-NAME               PIC X(16).
               05 CLERK-COM                PIC V99.
               
       FD TRANS-FILE.
           01 TRANS-REC.
               05 T-CLERK-NUM              PIC 9(9).
               05 T-SALES                  PIC 9(6).
               05 T-MONTH                  PIC XX.
       
       FD REPORT-FILE.
           01 REPORT-REC                   pic X(52).
           
       
       working-storage section.
       
       01 EOF                              pic X VALUE "N".
       01 BS-HOLD.
           05 BS-CLERK-NUM                PIC 9(9).
           05 BS-CLERK-NAME               PIC X(16).
           05 BS-CLERK-COM                PIC V99.          
           
       01 CLERK-TABLE.
           05 T-CLERK OCCURS 10 TIMES
               INDEXED BY C-IDX.
               10 T-NUM                PIC 9(9).
               10 T-NAME               PIC X(16).
               10 T-COM                PIC V99.
       
       01 WS-COM                       pic V99.
       
       01 WS-CTR                       pic 9.
       01 EXC-FLAG                     pic X.
       
       
       
       01 BLANK-LINE                   pic X(52) VALUE SPACES.
       
       01 HEADER-1.
           05 FILLER                   PIC X(19) VALUE SPACES.
           05 FILLER                   PIC X(12) VALUE "SALES REPORT".
       
       01 HEADER-2.
           05 FILLER                   PIC X(12) VALUE "CLERK NUMBER".
           05 FILLER                   PIC X(9) VALUE SPACES.
           05 FILLER                   PIC X(4) VALUE "NAME".
           05 FILLER                   PIC X(9) VALUE SPACES.
           05 FILLER                   PIC X(18) VALUE "MONTH   COMMISSION".
           
       01 HEADER-3.
           05 FILLER                   PIC X(14) VALUE SPACES.
           05 FILLER                   PIC X(16) VALUE "COMMISSION TABLE".
           
       
       01 HEADER-4.
           05 FILLER                   PIC X(12) VALUE "CLERK NUMBER".
           05 FILLER                   PIC X(9) VALUE SPACES.
           05 FILLER                   PIC X(4) VALUE "NAME".
           05 FILLER                   PIC X(9) VALUE SPACES.
           05 FILLER                   PIC X(10) VALUE "PERCENTAGE".
       
       01 COM-REPORT.
           05 FILLER                   PIC X(2) VALUE SPACES.
           05 R-NUM                    PIC 9(9).
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 R-NAME                   PIC X(16).
           05 FILLER                   PIC X(5) VALUE SPACES.
           05 R-MONTH                  PIC 99.
           05 FILLER                   PIC X(4) value spaces.
           05 R-COM                    PIC $$$,$$9V99.
           
       01 CLERK-REPORT.
           05 FILLER                   PIC XX VALUE SPACES.
           05 CR-NUM                   PIC 9(9).
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 CR-NAME                  PIC X(16).
           05 FILLER                   PIC X(6) VALUE SPACES.
           05 CR-COM                   PIC V99.
               
       procedure division.
       
       100-MAIN.
       
              open input TABLE-FILE, TRANS-FILE
                   output REPORT-FILE.
           
              perform 200-INIT-COM
              perform 250-HEADERS
       
              perform until EOF = "Y"
              read TRANS-FILE
                  AT END move "Y" to EOF
                  not AT END 
                  
       
                  perform 300-COM-SEARCH
                  perform 350-CALC
                
               END-READ.
           STOP-RUN.
               
       
       
       
       
       200-INIT-COM.
           perform varying C-IDX from 1 by 1 until C-IDX > 10
               read TABLE-FILE
               AT END move "Y" to EOF
               NOT AT END 
               move CLERK-NUM to T-NUM(C-IDX)
               move CLERK-NAME to T-CLERK(C-IDX)
               move CLERK-COM to T-COM(C-IDX)
           end-read.
           
           perform varying WS-CTR FROM 10 BY -1
               UNTIL WS-CTR = 0 OR EXC-FLAG = "N"
               move "N" to EXC-FLAG
               set C-IDX to 1
               perform WS-CTR times
                   if T-NUM(C-IDX) > T-NUM (C-IDX + 1)
                       move T-CLERK(C-IDX) to BS-HOLD
                       move T-CLERK(C-IDX + 1) TO T-CLERK(C-IDX)
                       move BS-HOLD to T-CLERK(C-IDX + 1)
                       move "Y" to EXC-FLAG
                   END-IF
               set C-IDX up by 1
               END-PERFORM
           END-PERFORM.
                 
       250-HEADERS.
           write REPORT-REC from BLANK-LINE
           write REPORT-REC from HEADER-1
           write REPORT-REC from BLANK-LINE
           write REPORT-REC from HEADER-2
           write REPORT-REC from BLANK-LINE.
           
       
       275-CLERK-HEADERS.
           write REPORT-REC from BLANK-LINE
           write REPORT-REC from BLANK-LINE
           write REPORT-REC FROM HEADER-3
           write REPORT-REC from BLANK-LINE
           write REPORT-REC from HEADER-4
           
           perform varying C-IDX from 1 by 1 until C-IDX >10
               move T-NUM(C-IDX) to CR-NUM
               move T-NAME(C-IDX) to CR-NAME
               move T-COM(C-IDX) to CR-COM
               write REPORT-REC from CLERK-REPORT
               move spaces to CLERK-REPORT
           END-PERFORM.
           
       300-COM-SEARCH.
       
           set C-IDX to 1.
           search T-CLERK
           WHEN T-NUM(C-IDX)  = T-CLERK-NUM 
               move T-NUM(C-IDX) to R-NUM
               move T-NAME(C-IDX) to R-NAME
               move T-MONTH to R-MONTH
               move T-COM(C-IDX) to WS-COM.   
               
       350-CALC.
       compute CR-COM = T-SALES * WS-COM
       write REPORT-REC from COM-REPORT.
       
       
       
       
           
       
       
       
           