       IDENTIFICATION DIVISION.
       PROGRAM-ID. RWD-DYNAMICS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OMEGA               PIC 9V9(5) VALUE 1.0.
       01  K                   PIC 9V9(5) VALUE 0.1.
       LINKAGE SECTION.
       01  I                   OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  I-DOT               OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  PHASE               PIC S9(5)V9(5).
       PROCEDURE DIVISION USING I I-DOT PHASE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 1000
               COMPUTE I-DOT(J) = OMEGA * I(J)
               PERFORM VARYING K FROM 1 BY 1 UNTIL K > 1000
                   COMPUTE I-DOT(J) = I-DOT(J) +
                       K * FUNCTION SIN(I(K) - I(J))
               END-PERFORM
               COMPUTE PHASE = PHASE + DT * FUNCTION SIN(I(J))
           END-PERFORM.
           GOBACK.
       END PROGRAM RWD-DYNAMICS.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RWD-FIELDPRINT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SUM                 PIC S9(5)V9(5).
       LINKAGE SECTION.
       01  I                   OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  FIELDPRINT          PIC S9(5)V9(5).
       PROCEDURE DIVISION USING I FIELDPRINT.
           MOVE 0 TO SUM.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 1000
               COMPUTE SUM = SUM + FUNCTION ABS(I(J))
           END-PERFORM.
           COMPUTE FIELDPRINT = SUM / 1000.
           GOBACK.
       END PROGRAM RWD-FIELDPRINT.