       IDENTIFICATION DIVISION.
       PROGRAM-ID. KAIROS-COHERENCE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  J                      PIC 9(4).
       LINKAGE SECTION.
       01  I                      OCCURS 1000 TIMES PIC S9(5)V9(5).
       01  PHASE                  PIC S9(5)V9(5).

       PROCEDURE DIVISION USING I PHASE.
       ENTRY "KAIROS-COHERENCE" USING I PHASE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 1000
               COMPUTE I(J) ROUNDED = I(J) * FUNCTION COS(PHASE)
           END-PERFORM
           GOBACK.
       END PROGRAM KAIROS-COHERENCE.
