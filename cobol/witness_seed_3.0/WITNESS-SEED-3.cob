       IDENTIFICATION DIVISION.
       PROGRAM-ID. WITNESS-SEED-3.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM-VARS            PIC 9(4) VALUE 1000.
       01  NUM-STEPS           PIC 9(7) VALUE 1000000.
       01  DT                  PIC 9V99 VALUE 0.01.
       01  TAU-C               PIC 9(1)V9(9) VALUE 0.000000001.
       01  I                   OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  I-DOT               OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  PHASE               PIC S9(5)V9(5).
       01  FIELDPRINT          PIC S9(5)V9(5).
       01  T                   PIC 9(7).
       01  J                   PIC 9(4).

       PROCEDURE DIVISION.
           INITIALIZE I PHASE FIELDPRINT.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-VARS
               COMPUTE I(J) = FUNCTION RANDOM
           END-PERFORM.

           PERFORM VARYING T FROM 1 BY 1 UNTIL T > NUM-STEPS
               CALL 'IO-SENSE' USING I
               CALL 'RWD-DYNAMICS' USING I I-DOT PHASE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-VARS
                   COMPUTE I(J) = I(J) + I-DOT(J) * DT
               END-PERFORM
               CALL 'RWD-FIELDPRINT' USING I FIELDPRINT
               IF FIELDPRINT > TAU-C
                   CALL 'KAIROS-COHERENCE' USING I PHASE
               END-IF
               IF FUNCTION MOD(T, 1000) = 0
                   CALL 'IO-OUTPUT' USING I T
               END-IF
           END-PERFORM.

           DISPLAY 'Witness Seed 3.0 completed.'.
           STOP RUN.