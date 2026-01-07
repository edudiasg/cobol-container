      ******************************************************************
      * PROGRAM-ID : EX01
      * TYPE       : BATCH
      * PURPOSE    : Read input file (first-name, salary), apply +10%
      *              and write output file. Display updated values.
      *
      * INPUT      : data/input/ex01.dat  (LINE SEQUENTIAL)
      *              Layout: FIRST-NAME (1-30), SALARY (31-40, 8V99)
      * OUTPUT     : data/output/ex01.out (LINE SEQUENTIAL)
      *              Same layout as input.
      *
      * AUTHOR     : Eduardo Dias Gusmão
      * CREATED    : 2024-06-10
      *
      * CHANGE LOG :
      *  - 2024-06-10 EDG - Initial version
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX01.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO 'data/input/ex01.dat'
                ORGANIZATION IS LINE SEQUENTIAL
                ACCESS MODE  IS SEQUENTIAL
                FILE STATUS  IS WS-IN-FILE-STATUS.

           SELECT OUT-FILE ASSIGN TO 'data/output/ex01.out'
                ORGANIZATION IS LINE SEQUENTIAL
                ACCESS MODE  IS SEQUENTIAL
                FILE STATUS  IS WS-OUT-FILE-STATUS.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *-----------------------------------------------------------------
       FD  IN-FILE.
       01  IN-REC.
           03 IN-FIRST-NAME               PIC X(030).
           03 IN-SALARY                   PIC 9(008)V99.

       FD  OUT-FILE.
       01  OUT-REC.
           03 OUT-FIRST-NAME              PIC X(030).
           03 OUT-SALARY                  PIC 9(008)V99.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
       01  WS-IN-REC.
           03 WS-FIRST-NAME               PIC X(030)     VALUE SPACES.
           03 WS-SALARY                   PIC 9(008)V99  VALUE ZEROS.

       01  WS-EOF-FLAG                    PIC X          VALUE 'N'.
           88  WS-EOF                                   VALUE 'Y'.
           88  WS-NOT-EOF                               VALUE 'N'.

       01  WS-IN-FILE-STATUS              PIC X(02)      VALUE SPACES.
           88  WS-IN-OK                                VALUE '00'.

       01  WS-OUT-FILE-STATUS             PIC X(02)      VALUE SPACES.
           88  WS-OUT-OK                               VALUE '00'.

       01  PSC-ERRO-PGM                   PIC 9(04)      VALUE ZEROS.
	   
	   01  DISPLAY-SALARY                 PIC  -ZZZ,ZZZ,ZZZ.ZZ.

       PROCEDURE DIVISION.

      *-----------------------------------------------------------------
       000000-MAIN                         SECTION.
      *-----------------------------------------------------------------
           MOVE 0001                        TO PSC-ERRO-PGM

           PERFORM 100000-OPEN-FILES
           PERFORM 200000-PROCESS-FILES
           PERFORM 900000-CLOSE-FILES

           STOP RUN
           .
       000999-EXIT-MAIN.
           EXIT.

      *-----------------------------------------------------------------
       100000-OPEN-FILES                   SECTION.
      *-----------------------------------------------------------------
           MOVE 0002                        TO PSC-ERRO-PGM

           OPEN INPUT  IN-FILE
                OUTPUT OUT-FILE

           IF NOT WS-IN-OK
           OR NOT WS-OUT-OK
               PERFORM 999900-ABEND
           END-IF
           .
       100999-EXIT-OPEN-FILES.
           EXIT.

      *-----------------------------------------------------------------
       200000-PROCESS-FILES                SECTION.
      *-----------------------------------------------------------------
           MOVE 0003                        TO PSC-ERRO-PGM

           PERFORM 210000-READ-IN-FILE

           PERFORM UNTIL WS-EOF
               PERFORM 300000-PROCESS-RECORD
               PERFORM 400000-WRITE-OUT-FILE
			   
               MOVE SALARY TO DISPLAY-SALARY
               DISPLAY FIRST-NAME ' ' DISPLAY-SALARY		   
			   
               PERFORM 210000-READ-IN-FILE
           END-PERFORM
           .
       200999-EXIT-PROCESS-FILES.
           EXIT.

      *-----------------------------------------------------------------
       210000-READ-IN-FILE                 SECTION.
      *-----------------------------------------------------------------
           MOVE 0004                        TO PSC-ERRO-PGM

           READ IN-FILE
                INTO WS-IN-REC
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   IF NOT WS-IN-OK
                       PERFORM 999900-ABEND
                   END-IF
           END-READ
           .
       210999-EXIT-READ-IN-FILE.
           EXIT.

      *-----------------------------------------------------------------
       300000-PROCESS-RECORD               SECTION.
      *-----------------------------------------------------------------
           MOVE 0005                        TO PSC-ERRO-PGM

           PERFORM 310000-APPLY-INCREASE

           MOVE WS-FIRST-NAME TO OUT-FIRST-NAME
           MOVE WS-SALARY     TO OUT-SALARY
           .
       300999-EXIT-PROCESS-RECORD.
           EXIT.

      *-----------------------------------------------------------------
       310000-APPLY-INCREASE               SECTION.
      *-----------------------------------------------------------------
           MOVE 0006                        TO PSC-ERRO-PGM

           COMPUTE WS-SALARY = WS-SALARY * 1.10
               ON SIZE ERROR
                   PERFORM 999900-ABEND
           END-COMPUTE
           .
       310999-EXIT-APPLY-INCREASE.
           EXIT.

      *-----------------------------------------------------------------
       400000-WRITE-OUT-FILE               SECTION.
      *-----------------------------------------------------------------
           MOVE 0007                        TO PSC-ERRO-PGM

           WRITE OUT-REC

           IF NOT WS-OUT-OK
               PERFORM 999900-ABEND
           END-IF
           .
       400999-EXIT-WRITE-OUT-FILE.
           EXIT.

      *-----------------------------------------------------------------
       900000-CLOSE-FILES                  SECTION.
      *-----------------------------------------------------------------
           MOVE 0008                        TO PSC-ERRO-PGM

           CLOSE IN-FILE
                 OUT-FILE

           IF NOT WS-IN-OK
           OR NOT WS-OUT-OK
               PERFORM 999900-ABEND
           END-IF
           .
       900999-EXIT-CLOSE-FILES.
           EXIT.

      *-----------------------------------------------------------------
       999900-ABEND                        SECTION.
      *-----------------------------------------------------------------
           DISPLAY 'ABEND - PSC=' PSC-ERRO-PGM
                   ' FS-IN='  WS-IN-FILE-STATUS
                   ' FS-OUT=' WS-OUT-FILE-STATUS
                   ' MSG='    WS-ABEND-MSG
           STOP RUN
           .
       999999-EXIT-ABEND.
           EXIT.
