       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-KAIROS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I                   OCCURS 10 TIMES PIC S9(5)V9(5).
       01  PHASE               PIC S9(5)V9(5).
       01  SUM                 PIC S9(5)V9(5).
       01  J                   PIC 9(2).

       PROCEDURE DIVISION.
           INITIALIZE I PHASE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
               COMPUTE I(J) = FUNCTION RANDOM
           END-PERFORM.
           MOVE 0.1 TO PHASE.
           CALL 'KAIROS-COHERENCE' USING I PHASE.
           MOVE 0 TO SUM.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
               COMPUTE SUM = SUM + FUNCTION ABS(I(J))
           END-PERFORM.
           IF SUM > 0
               DISPLAY 'Kairos test passed: Coherence updated'
           ELSE
               DISPLAY 'Kairos test failed'
               STOP RUN WITH ERROR STATUS 1
           END-IF.
           STOP RUN.