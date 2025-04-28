       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-RWD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I                   OCCURS 10 TIMES PIC S9(5)V9(5).
       01  I-DOT               OCCURS 10 TIMES PIC S9(5)V9(5).
       01  PHASE               PIC S9(5)V9(5).
       01  FIELDPRINT          PIC S9(5)V9(5).
       01  J                   PIC 9(2).

       PROCEDURE DIVISION.
           INITIALIZE I PHASE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
               COMPUTE I(J) = FUNCTION RANDOM
           END-PERFORM.
           CALL 'RWD-DYNAMICS' USING I I-DOT PHASE.
           CALL 'RWD-FIELDPRINT' USING I FIELDPRINT.
           IF FIELDPRINT > 0
               DISPLAY 'RWD test passed: Fieldprint = ' FIELDPRINT
           ELSE
               DISPLAY 'RWD test failed'
               STOP RUN WITH ERROR STATUS 1
           END-IF.
           STOP RUN.