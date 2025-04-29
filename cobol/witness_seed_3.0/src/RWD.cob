       IDENTIFICATION DIVISION.
       PROGRAM-ID. RWD-DYNAMICS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  LOCAL-OMEGA            PIC 9V9(5) VALUE 1.0.
       01  LOCAL-K                PIC 9V9(5) VALUE 0.1.
       01  J                      PIC 9(4).
       01  K-INDEX                PIC 9(4).
       01  SUMSIN                 PIC S9(7)V9(5).
       LINKAGE SECTION.
       01  I                      OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  I-DOT                  OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  PHASE                  PIC S9(5)V9(5).

       PROCEDURE DIVISION USING I I-DOT PHASE.
       ENTRY "RWD-DYNAMICS" USING I I-DOT PHASE.
           MOVE 0 TO SUMSIN
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 1000
               COMPUTE I-DOT(J) ROUNDED = LOCAL-OMEGA * I(J)
               PERFORM VARYING K-INDEX FROM 1 BY 1 UNTIL K-INDEX > 1000
                   COMPUTE I-DOT(J) ROUNDED = I-DOT(J) + LOCAL-K * FUNCTION SIN(I(K-INDEX) - I(J))
               END-PERFORM
               COMPUTE SUMSIN = SUMSIN + FUNCTION SIN(I(J))
           END-PERFORM
           COMPUTE PHASE = PHASE + 0.01 * SUMSIN
           GOBACK.
       END PROGRAM RWD-DYNAMICS.
