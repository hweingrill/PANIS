      $SET LINKCOUNT "128" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PAKOLAG.
      ******************************************************************
      *              Lagerbuchhaltung mit Waageanschlu·                *
      *----------------------------------------------------------------*
      *   Scheinfaktura je Firma Åber Druck und Bildschirm
      *   Rezeptbilanz + Fehlerprotokoll * Anzahl wird eingegeben
      *   Direktentnahme (sollte eher als Lfsch. im BÅro eingegeben w.)
      *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
       01  WT-CALL                     PIC X(8).
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DAT"  WH-CREG.
      *    MOVE WH-DATUM TO WM-DATUM.
           CALL "CAUP" USING "0701012280030" WH-CREG.
           DISPLAY SPACES UPON CRT.
       B.  CALL "PAKOVORL" USING "99VORLAUF" WH-CREG.
           CANCEL "PAKOVORL".
       C.  MOVE WT-TX TO WT-CALL.
           IF WT-TX NOT = SPACE CALL WT-CALL USING WT-TX(9:15) WH-CREG
                                CANCEL WT-CALL.
           IF WH-PG = 49 GO C.
           MOVE 9 TO WH-PG.
           GO B.
       Z.  EXIT.
