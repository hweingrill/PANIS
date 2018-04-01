      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PANARC.
      ******************************************************************
      *        Archivabrufeprogramm Rechnung und Liefershceine         *
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KONSTANT   ASSIGN TO DISK "PANEKONS.DAT"
                             ORGANIZATION RELATIVE ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-AZ               PIC 99.
               05  WL-VL               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-VP               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY "WHCREG.CPY".
       01  WH-TX                       PIC X(51).
       01  WD-X                        PIC X.
       01  WH-PANEXT      external.
           03  WK-EAN                  PIC 9(8)      COMP.
           03  WK-MON                  PIC 9(8)      COMP.
           03  WA-DAT                  PIC 9(8)      COMP.
           03  WE-MAXDAT               PIC 9(8)      COMP.
           03  WE-MINDAT               PIC 9(8)      COMP.
           03  WH-KEY                  PIC 9(5).
           03  WH-BUKEY                PIC 9(5).
      *------------------> Version, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-VER               PIC 99        COMP.
               05 WM-KO                PIC 99        COMP.
               05 WM-PR                PIC 99        COMP.
               05 WM-RB                PIC 99        COMP.
           03  WE-LFS                  PIC 99        COMP.
           03  WN-FKON                 PIC X(35).
           03  WN-BUEB                 PIC X(35).
           03  WE-ARSYM                PIC 99        COMP.
           03  WE-GSSYM                PIC 99        COMP.
           03  WE-PANIS                PIC 99        COMP.
           03  WM-DRU                  PIC 99        COMP.
           03  WE-PARAM       OCCURS 10.
               05 WE-DRU               PIC 99        COMP.
               05 WE-KOPF              PIC 99        COMP.
               05 WE-WO                PIC 99        COMP.
               05 WE-TYP               PIC 99        COMP.
           03  WE-STG                  PIC X(40)    OCCURS  10.
           03  WH-P                    PIC 99        COMP.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       DECLARATIVES.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000C:" WH-CREG.
           OPEN I-O KONSTANT.
           PERFORM VORLAUF.
       X.  CLOSE KONSTANT.
           CALL "CAUP" USING "1301012480" WH-CREG.
           DISPLAY "Programm beendet" AT 1228.
           STOP RUN.
       Z.  EXIT.
      *****************************************************************
       VORLAUF SECTION.
       A.  MOVE "    Archivierung" TO WK-GEB.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WH-DATUM TO WM-DATUM.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
       B.  MOVE 1 TO WH-KEY.
           READ KONSTANT NO LOCK NOT INVALID GO J.
           IF ZUGRIF PERFORM BESETZT GO B.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Bitte Kopf anlegen!" AT 2401.
           INITIALIZE KO-SATZ.
       C.  ACCEPT KO-FIRMA WITH HIGHLIGHT AT 0127.
           IF KO-FIRMA = SPACE GO C.
           DISPLAY KO-FIRMA WITH HIGHLIGHT AT 0127.
           MOVE "pane" TO KO-SPERRE.
           MOVE 20 TO KO-UST(1).
           MOVE 10 TO KO-UST(2).
           MOVE 30 TO KO-UST(3).
           MOVE 0 TO KO-UST(4) KO-UST(5) KO-UST(6).
           MOVE SPACE TO KO-ORT KO-TX.
           MOVE WH-DATUM TO WZ-DATUM.
           MOVE 31 TO WZ-TAG.
           MOVE WZ-DATUM TO WV-DATUM.
       D.  CALL "CAUP" USING "05DATPRUEF" WH-CREG
           IF WZ-DATUM = 0 SUBTRACT 1 FROM WV-DATUM
               MOVE WV-DATUM TO WZ-DATUM GO D.
           MOVE WZ-DATUM TO KO-MON.
           MOVE 1 TO KO-RENUM KO-NUM WM-KO WM-PR WM-RB.
           MOVE WK-M TO KO-MERK.
           WRITE KO-SATZ.
           MOVE SPACE TO KO-SATZ.
           MOVE 10 TO WH-KEY KO-NUM.
           MOVE "StkKg daggr KtnSckNtzPckPkg" TO KO-TM.
           WRITE KO-SATZ.
           MOVE SPACE TO KO-SATZ.
           MOVE 6 TO WH-KEY KO-NUM.
           MOVE 0 TO KO-ERST KO-DATUM KO-ERLOES(1) KO-ERLOES(2)
               KO-ERLOES(3) KO-ERLOES(4) KO-ERLOES(5) KO-ERLOES(6)
               KO-ERLOES(7) KO-ERLOES(8) KO-ERLOES(9) KO-ERLOES(10).
           WRITE KO-SATZ.
           GO B.
       J.  MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           MOVE KO-MERK TO WK-M.
           MOVE KO-EANNR TO WK-EAN.
           MOVE 1 TO WK-BS.
           MOVE KO-ORT TO WT-TX.
           MOVE KO-TX TO WH-TX.
           SET TY FY TO 15.
       K.  IF WR-TX(TY) NOT = SPACE GO L.
           IF TY > 1 SET TY DOWN BY 1 GO K.
       L.  MOVE WR-TX(TY) TO WR-TX(FY).
           IF TY NOT = FY MOVE SPACE TO WR-TX(TY).
           IF TY > 1 SET TY FY DOWN BY 1 GO L.
           MOVE WT-TX TO VDU-ORT.
           MOVE WH-DATUM TO WM-DATUM WZ-DATUM.
           MOVE 1 TO WX.
       M.  MOVE KO-UST(WX) TO WT-UST(WX).
           IF WX < 6 ADD 1 TO WX GO M.
           MOVE 10 TO WH-KEY.
       N.  READ KONSTANT NO LOCK.
           IF ZUGRIF PERFORM BESETZT GO N.
           MOVE KO-TM TO WT-TM.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           GO P.
       O.  IF WX-CODE(2) = WX-CODE(1) GO P.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           MOVE 9 TO WH-PG.
           IF ESC GO Z.
           MOVE WT-TX TO WX-CODE(1).
           GO O.
       P.  CALL "PANARCH" USING "99ABFRAG" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" USING "1323012480000" WH-CREG.
       Z.  EXIT.
