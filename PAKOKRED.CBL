      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PAKOKRED.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PARSELFT.CPY.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
           COPY PARFDLFT.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR.
               05 DRA-KTONR            PIC ZZZZZZ,Z-.
               05 DRA-BELNR            PIC ZZZZZZ-.
               05 FILLER               PIC X.
               05 DRA-BELDAT           PIC X(8).
               05 DRA-REFNR            PIC ZZZZZZ-.
               05 DRA-SYM              PIC XX.
               05 DRA-GGKTO            PIC ZZZZZZ,Z-.
               05 DRA-TX               PIC X(40).
               05 DRA-UST              PIC ZZ-.
               05 DRA-MWSKT            PIC ZZZZZ.ZZZ,ZZ-.
               05 DRA-SOLL             PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-HABEN            PIC ZZ.ZZZ.ZZZ,ZZ-.
           03  FILLER                  PIC X.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY "WHCREG.CPY".
       01  WF-REG.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-LST                  PIC X(12).
           03  WH-PX                   PIC XX        OCCURS 2.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WD-SEITE                PIC ZZ9.
           03  WM-MERKER               PIC 9        COMP-3 VALUE ZERO.
                   88 ANLAGE   VALUE 1 3.      88  AEND   VALUE 0 2 3.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(15)         OCCURS 15.
           03  WD-KTO                  PIC ZZ.ZZ9.
           03  WD-BANK                 PIC ZZ9.
           03  WD-BLZ                  PIC 99.999.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WD-X                    PIC X.
           03  WD-TG                   PIC ZZ9.
           03  WD-PZ                   PIC 9,9.
       COPY "PANEXT.CPY".
       DECLARATIVES.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       DECL-Q SECTION.         USE AFTER ERROR PROCEDURE ON KNOTEN.
       A.  CALL "CADECL" USING "PAKOKNOT.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 20
               WHEN 21
               WHEN 22 PERFORM KRED.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              PERFORM WEITER GO A.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerr�ckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU not = 1 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularh�he *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO C.
           PERFORM DRU-OK.
           OPEN OUTPUT DRUCKER.
           MOVE 1 TO WM-OPEN.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
           IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO D.
           IF WM-DRU not = 0 GO G.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
       G.  MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       KRED SECTION.
       A.  CALL "CAUP" USING "0711211155000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Verwaltung Kreditoren " with highlight AT VDU-LP.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           DISPLAY "Nr.:" AT VDU-LP.
           IF WL-CA = 21 GO D.
           DISPLAY "<ret>= Anlage/�ndern, <ret>+ alpha = suchen, <esc>=
      -        "Ende, < >= Stammdatendruck" AT 2301.
           CALL "CAUP" USING "1002086006" WH-CREG.
           IF ESC OR WOLI MOVE 0 TO KR-KTONR;
               EVALUATE WL-CA
                   WHEN 20 MOVE "PAKOSTAM60STAM" TO WT-TX GO X
                   WHEN 21 GO X
                   WHEN 22 MOVE "PARVORL 60VORL" TO WT-TX GO X.
           IF AB PERFORM STAMDRUCK GO C.
           IF NOT RET GO C.
           IF WH-NUM = 0 MOVE 3 TO WH-NUM
               CALL "PAKOANZ" USING "40KRESUCH" WH-CREG
               CANCEL "PAKOANZ"
               IF KR-KTONR = 0 GO C.
       D.  MOVE WH-NUM TO KR-KTONR.
           MOVE 0 TO WM-MERKER.
       E.  READ KREDIT INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO E.
       F.  PERFORM KR-ANL.
           UNLOCK KREDIT.
           IF WL-CA = 20 GO C.
           IF ESC MOVE 0 TO KR-KTONR.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KR-BILD SECTION.
       A.  MOVE WH-WERT TO WD-KTO.
           ADD 214 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Konto-Nr.: " AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bezeichnung:" AT VDU-LP.
           ADD 223 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kurzname:" AT VDU-LP.
           ADD 803 VDU-ECK GIVING VDU-LP.
           DISPLAY "Tel.: " AT VDU-LP.
           ADD 903 VDU-ECK GIVING VDU-LP.
           DISPLAY "Fax.: " AT VDU-LP.
           ADD 1003 VDU-ECK GIVING VDU-LP.
           DISPLAY "Anm.: " AT VDU-LP.
           IF AEND GO D.
           INITIALIZE KR-SATZ.
           MOVE WH-NUM TO KR-KTONR.
           MOVE 1 TO WX.
       D.  ADD 214 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           PERFORM HOLADR.
           ADD 233 VDU-ECK GIVING VDU-LP.
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV05" WH-CREG.
           DISPLAY WT-TX with highlight SIZE 5 AT VDU-LP.
           ADD 809 VDU-ECK GIVING VDU-LP.
           DISPLAY KR-TELE with highlight AT VDU-LP.
           ADD 909 VDU-ECK GIVING VDU-LP.
           DISPLAY KR-FAX with highlight AT VDU-LP.
           ADD 1009 VDU-ECK GIVING VDU-LP.
           DISPLAY KR-ANMERK with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       BEDMES SECTION.
       A.  DISPLAY "<esc>= Abbruch, <ret-leer>= keine �nderung, <ende>=
      -        "Ende" AT 2301.
       Z.  EXIT.
      ******************************************************************
       KR-ANL SECTION.
       A.  PERFORM KR-BILD.
           DISPLAY "<esc>= Abbruch" AT 2301.
           IF ANLAGE DISPLAY "Lieferant fehlt!" AT 2401.
       C.  PERFORM HOLADR.
           CALL "CAUP" USING "1203150534" WH-CREG.
           IF WOLI OR ESC MOVE 0 TO KR-KTONR GO Z.
           IF WT-BEZ = SPACE DISPLAY "Bezeichnung mu� sein!" AT 2401
               PERFORM WEITER GO C.
           MOVE WT-BEZ TO KR-BEZ.
       E.  DISPLAY "(<-)= zur�ck, <esc>= Abbruch, <ret>= Eingabe, <ret-
      -        "leer>= Wiederholung" AT 2301.
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV05" WH-CREG.
           ADD 233 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-TX WITH SIZE 5 HIGHLIGHT AT VDU-LP.
           CALL "CAUP" USING "1202330105" WH-CREG.
           IF WOLI GO C.
           IF ESC GO R.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO KR-MCODE.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
       K.  MOVE KR-TELE TO WT-TX.
           DISPLAY "Telefonnummer" AT 2401.
           CALL "CAUP" USING "1208090125" WH-CREG.
           IF ESC GO R.
           IF WOLI GO E.
           IF NOT RET GO K.
           MOVE WT-TX TO KR-TELE.
           DISPLAY KR-TELE with highlight AT VDU-LP.
       N.  MOVE KR-FAX TO WT-TX.
           DISPLAY "Faxnummer" AT 2301.
           CALL "CAUP" USING "1209090125" WH-CREG.
           IF WOLI OR ESC GO K.
           MOVE WT-TX TO KR-FAX.
           DISPLAY KR-FAX with highlight AT VDU-LP.
       O.  MOVE KR-ANMERK TO WT-TX.
           DISPLAY "Anmerkungen" AT 2301.
           CALL "CAUP" USING "1210090145" WH-CREG.
           IF WOLI OR ESC GO R.
           MOVE WT-TX TO KR-ANMERK.
           DISPLAY KR-ANMERK with highlight AT VDU-LP.
       R.  DISPLAY "<tab>= l�schen, <ret>= speichern, <esc>= Abbruch, <
      -        ">= zur�ck" AT 2301.
           CALL "CAUP" USING "0023630000" WH-CREG.
           IF ESC UNLOCK KREDIT GO Z.
           IF WOLI GO N.
           IF TABL GO Y.
           IF NOT RET GO R.
       X.  WRITE KR-SATZ INVALID REWRITE KR-SATZ.
           IF ZUGRIF PERFORM BESETZT GO X.
           GO Z.
       Y.  ADD 316 VDU-ECK GIVING VDU-LP.
           DISPLAY "    ** gel�scht **    " with highlight AT VDU-LP.
           DELETE KREDIT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO Y.
       Z.  EXIT.
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION.
       A.  ADD 315 VDU-ECK GIVING VDU-LP.
           MOVE KR-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 5
               DISPLAY WR-ADR(WR) with SIZE 34 highlight AT VDU-LP
               ADD 100 TO VDU-LP.
       Z.  EXIT.
      *********************************************** Druck Stammdaten *
       STAMDRUCK SECTION.
       A.  CALL "CAUP" USING "0710100466000" WH-CREG.
           MOVE WE-DRU(4) TO WM-DRU.
           MOVE "LIEFER.LST" TO WH-DRUNAM.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Stammdatendruck " AT VDU-LP.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Nummerndruck, <tab>= Matchcod
      -        "edruck < >" AT VDU-LP.
           CALL "CAUP" USING "1102630000" WH-CREG.
           IF ESC GO X.
           IF NOT RETA GO C.
           IF RET MOVE 0 TO KR-KTONR
               START KREDIT KEY NOT < KR-KEY INVALID GO C
           else MOVE LOW-VALUES TO KR-MCODE
               START KREDIT KEY NOT < KR-MCODE INVALID GO C.
       E.  READ KREDIT NEXT IGNORE LOCK AT END GO W.
           ADD 302 VDU-ECK GIVING VDU-LP.
           PERFORM KREDKOPF.
           MOVE KR-KTONR TO WD-KTO.
           MOVE WD-KTO TO DRA-STR(1:10).
           INSPECT KR-BEZ REPLACING ALL "#" BY ",".
           MOVE KR-BEZ TO DRA-STR(10:62).
           DISPLAY WD-KTO with highlight AT VDU-LP DRA-STR(09:50).
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DRA-STR(73:8).
           PERFORM DRUCK.
           GO E.
       W.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KREDKOPF SECTION.
       A.  IF WZ-ZEILEN > 63 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "PREISE.LST" TO WM-LST.
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "Kreditorenliste" TO DRA-STR(33:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(57:10).
           MOVE "Seite:" TO DRA-STR(67:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRA-STR(74:).
           PERFORM DRUCK.
           MOVE ALL "�" TO DRA-STR(1:80).
           PERFORM DRUCK.
           MOVE "Konto-Nr Bezeichnung
      -       "                 M-code" TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "�" TO DRA-STR(1:80).
           PERFORM DRUCK.
       Z.  EXIT.
