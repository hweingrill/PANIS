      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PAREZ.
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WT-CALL                     PIC X(8).
       COPY "WHCREG.CPY".
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           call "parlgkor" using "10" wh-creg on overflow next sentence.
           cancel "parlgkor".
       B.  CALL "PARVORL" USING "VORLAUF" WH-CREG.
           CANCEL "PARVORL".
       C.  IF WH-PG = 99 GO B.
           MOVE WT-TX TO WT-CALL.
           IF WT-TX NOT = SPACE CALL WT-CALL USING WT-TX(9:15) WH-CREG
                                CANCEL WT-CALL.
           IF WH-PG = 49 GO C.
           MOVE 9 TO WH-PG.
           GO B.
       Z.  EXIT.
