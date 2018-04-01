      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER CALLFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANARCH.
      ************************* Archivierung Rechnung u. Lieferscheine *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEKON.CPY.
           SELECT HILFPOS    ASSIGN TO "DRUHILF.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
           SELECT ARCHIV     ASSIGN TO WH-ARCHIV
                             ORGANIZATION BINARY SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT DRUDAT     ASSIGN TO "PANDRUCK.LST"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
      $SET DATACOMPRESS "1"
           SELECT RE-ARCHIV  ASSIGN TO WN-REARC
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY REA-KEY
                             ALTERNATE KEY REA-AKEY
                             FILE STATUS WF-STATUS.
           SELECT LF-ARCHIV  ASSIGN TO WN-LFARC
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY LFA-KEY
                             FILE STATUS WF-STATUS.
      $SET NO DATACOMPRESS
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
      ******************************************************************
       FD  ARCHIV      external.
       01  AC-SATZ.
           03  AC-RENUM                PIC 9(5).
           03  AC-KTONR                PIC 9(5).
           03  AC-MCODE.
               05 AC-DATUM             PIC 999999.
               05 AC-FIL               PIC 99.
           03  AC-REST                 PIC X(1006).
      ************************************************ Druckhilfsdatei *
       FD  DRUDAT                      LABEL RECORD OMITTED.
       01  PA-SATZ.
           03 PA-ZEIL                  PIC X(132).
      ************************************************ Rechnungsarchiv *
       FD  RE-ARCHIV   external        LABEL RECORD STANDARD
                                       RECORDING MODE IS VARIABLE.
       01  REA-SATZ.
           03  REA-AKEY.
               05 REA-KTONR            PIC 9(5)     COMP.
               05 REA-KEY.
                  07 REA-NUM           PIC 9(5)     COMP.
                  07 REA-LFD           PIC 99       COMP.
           03 REA-REST                 PIC X(1024).
      ********************************************* Lieferscheinarchiv *
       FD  LF-ARCHIV   external        LABEL RECORD STANDARD
                                       RECORDING MODE IS VARIABLE.
       01  LFA-SATZ.
           03  LFA-KEY.
               05 LFA-KTONR            PIC 9(5)     COMP.
               05 LFA-DATUM            PIC 9(6)     COMP.
               05 LFA-NUM              PIC 9(5)     COMP.
               05 LFA-LFD              PIC 99       COMP.
           03  LFA-REST                PIC X(1024).
      ******************************************************************
       FD  HILFPOS             LABEL RECORD STANDARD
                               RECORD IS VARYING IN SIZE FROM 1 TO 80
                               DEPENDING ON WS-HILF.
       01  HI-SATZ                     PIC X(80).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(132).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-VL               PIC 99.
               05  WL-VP               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-AZ               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY "WHCREG.CPY".
       01  WH-ACSIZE                   PIC 9(5)      COMP.
       01  WH-SIZE                     PIC 99.
       01  WH-ARCHIV.
           03  FILLER                  PIC XXX       VALUE "PAC".
           03  WH-ARCNR                PIC 9(5).
           03  FILLER                  PIC XXXX      VALUE ".DAT".
       01  WN-REARC.
           03  FILLER                  PIC X(6)  VALUE "PANREA".
           03  WN-RJ                   PIC 99    VALUE 95.
           03  FILLER                  PIC X(4)  VALUE ".DAT".
       01  WN-LFARC.
           03  FILLER                  PIC X(6)  VALUE "PANLFA".
           03  WN-LJ                   PIC 99    VALUE 95.
           03  FILLER                  PIC X(4)  VALUE ".DAT".
       01  WH-REG.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 10.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WH-PX                   PIC XX         OCCURS 2.
           03  WM-MERKER               PIC 9         COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WM-LFOPEN               PIC 9         COMP   VALUE ZERO.
           03  WM-REOPEN               PIC 9         COMP   VALUE ZERO.
           03  WV-LJ                   PIC 99.
           03  WV-RJ                   PIC 99.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP-3 VALUE ZERO.
           03  WH-L                    PIC 99        COMP.
           03  WK-F                    PIC 99      COMP OCCURS 10.
           03  WD-ZL                   PIC Z9.
           03  WD-SIZE                 PIC ZZ.ZZZ.ZZ9.
           03  WI                      PIC 9999      COMP VALUE 0.
           03  WE                      PIC 99      COMP VALUE 0.
           03  IX                      PIC 99      COMP VALUE 0.
           03  WL                      PIC 99      COMP VALUE 0.
           03  WH-LR                   PIC XXXX.
           03  VDU-ZEIT.
               05 VDU-STD              PIC Z9.
               05 VDU-DD               PIC X.
               05 VDU-MIN              PIC 99.
           03  WH-TX                   PIC X(51).
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WH-MOD                  PIC 99        COMP.
           03  WK-D                    PIC 9999      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-LFNUM                PIC 9(6)      COMP.
           03  WV-NUM                  PIC 9(5)      COMP.
           03  WA-DATUM                PIC 9(6)      COMP.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WD-POS                  PIC ZZ9.
           03  WD-ART                  PIC ZZZ9.
           03  WD-PROZ                 PIC Z9,9.
           03  WD-TG                   PIC ZZ9.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WD-PREIS                PIC Z.ZZ9,99-.
           03  WD-APREIS               PIC Z.ZZ9,999-.
           03  WD-X                    PIC X.
           03  WD-UPON                 PIC X(60).
           03  WS-HILF                 PIC 99      COMP  VALUE ZERO.
           03  WK-J                    PIC 99.
           03  WM-J                    PIC 99.
           03  WD-J                    PIC 9999.
      *----------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
           03  WH-FNAME                PIC X(12).
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)    COMP-X.
               05 WF-TT                PIC 99      COMP-X.
               05 WF-MO                PIC 99      COMP-X.
               05 WF-YY                PIC 9999    COMP-X.
               05 WF-HH                PIC 99      COMP-X.
               05 WF-MM                PIC 99      COMP-X.
               05 WF-SS                PIC 99      COMP-X.
               05 WF-HS                PIC 99      COMP-X.
           03  WR-DNAM                 PIC X(12)  OCCURS 10.
           03  wn-verdicht             pic x(16).
       01  WT-DK     external.
           03  WR-TN                PIC X(10)  OCCURS 7.
           03  WT-KON               PIC 99     OCCURS 12.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON HILFPOS.
       A.  CALL "CADECL" USING "DRUHILF.DAT " WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARCHIV.
       A.  CALL "CADECL" USING WH-ARCHIV      WH-CREG.
       DECL-Q SECTION.         USE AFTER ERROR PROCEDURE ON DRUDAT.
       A.  CALL "CADECL" USING "PANDRUCK.LST" WH-CREG.
       DECL-S SECTION.         USE AFTER ERROR PROCEDURE ON RE-ARCHIV.
       A.  CALL "CADECL" USING WN-REARC       WH-CREG.
       DECL-T SECTION.         USE AFTER ERROR PROCEDURE ON LF-ARCHIV.
       A.  CALL "CADECL" USING WN-LFARC       WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE WH-KEY TO WK-D.
           MOVE 1 TO WH-KEY.
           READ KONSTANT WITH NO LOCK INVALID KEY STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO A.
           MOVE KO-MERK TO WK-M.
           MOVE 0 TO WE-LFS.
           IF WM-VER > 10 ADD -10 TO WM-VER
                          MOVE 1 TO WE-LFS.              *> Lfsch. Kern
           MOVE KO-TX TO WH-TX.
           MOVE 10 TO WH-KEY.
       C.  READ KONSTANT WITH NO LOCK INVALID KEY STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF KO-WJAHR(1:2) = SPACE MOVE 0701 TO KO-WJAHR.
           MOVE WM-DATUM TO WZ-DATUM.
           PERFORM BILDJAHR.
           PERFORM ARCH-WAHL.
       Z.  EXIT PROGRAM.
      ************************************* Bilde Ende Wirtschaftsjahr *
       BILDJAHR SECTION.
       A.  MOVE WZ-JAHR TO WK-J.
           COMPUTE WA-DATUM = WK-J * 10000 + KO-WJAHR.
           MOVE WA-DATUM TO WC-DATUM.
           IF WZ-DATUM < WC-DATUM;
               IF WC-TAG not = 0 SUBTRACT 1 FROM WC-TAG
                            else MOVE 99 TO WC-TAG.
           MOVE WC-DATUM TO WA-DATUM WZ-DATUM.
           ADD -1 TO WZ-MONAT.
           IF WZ-MONAT = 0 MOVE 12 TO WZ-MONAT
                      else ADD 1 TO WZ-JAHR.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE 32 TO WZ-TAG.
       C.  ADD -1 TO WZ-TAG.
           MOVE WZ-DATUM TO WS-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE WS-DATUM TO WZ-DATUM GO C.
           MOVE WZ-DATUM TO WS-DATUM.
           MOVE WZ-JAHR TO WK-J WM-J.
           IF WK-J < 50 ADD 2000 WK-J GIVING WD-J
                   else ADD 1900 WK-J GIVING WD-J.
       Z.  EXIT.
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
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO VDU-L.
           IF VDU-L < 20 GO Z.
       B.  CALL "CAUP" USING "1407012180000" WH-CREG.
           SUBTRACT 1 FROM VDU-L.
           SUBTRACT 1 FROM WH-L.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) NOT = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              GO A.
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
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  IF WH-PG NOT = 3 WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO C.
           MOVE 1 TO WM-OPEN.
           OPEN OUTPUT DRUCKER.
           PERFORM DRU-OK.
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
       ARCH-WAHL SECTION.
       A.  MOVE "   Archivierung" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WL-CA = 11 MOVE 1 TO WH-PG GO E.    *> autom. archivieren
       C.  DISPLAY "Programmwahl" AT 0624.
           DISPLAY "컴컴컴컴컴컴" AT 0724.
           DISPLAY "1 - Archivieren"     AT 0924
                   "2 - Rechnung"        AT 0024
                   "3 - Lieferscheine"   AT 0024
                   " "                   AT 0024
                   "<ret-leer>= Ende"    AT 0024
                   "Bitte w꼑len Sie:  " AT 0034
           CALL "CAUP" USING "0014521001" WH-CREG.
           IF ESC OR WH-NUM = 0 GO Z.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-PG.
           IF WH-PG = 9 CALL "PANREARC" USING "10" WH-CREG GO A.
           IF WH-PG > 3 and wh-pg not = 9 GO C.
       E.  EVALUATE WH-PG
               WHEN 1 PERFORM ARCHIVIEREN
               WHEN 2 PERFORM RECH-ARCHIV
               WHEN 3 PERFORM LFS-ARCHIV.
           IF WL-CA NOT = 11 GO C.
       Z.  EXIT PROGRAM.
      ******************************************************************
       RECH-ARCHIV SECTION.
       A.  CALL "CAUP" USING "0709101160000" WH-CREG.
           MOVE WM-DATUM TO WZ-DATUM.
           MOVE WE-DRU(1) TO WM-DRU.
           MOVE 0 TO WS-HILF.
       B.  PERFORM BILDJAHR.
           PERFORM OPEN-REARC.
           IF WF-STATUS = 05 CLOSE RE-ARCHIV
               MOVE 0 TO WM-REOPEN
               ADD 1 TO WS-HILF
               DELETE FILE RE-ARCHIV
               IF WZ-JAHR = 0 MOVE 99 TO WZ-JAHR
               else ADD -1 TO WZ-JAHR
               end-if
               IF WS-HILF not = 3 GO B
               else DISPLAY "keine Jahre" with highlight AT 2401
               PERFORM WEITER
               GO X.
           CLOSE RE-ARCHIV.
       C.  MOVE 0 TO WM-REOPEN WS-HILF.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Rechnungsabfrage " with highlight AT VDU-LP.
           ADD 140 VDU-ECK GIVING VDU-LP.
           DISPLAY " WJ:      " AT VDU-LP.
           ADD 5 TO VDU-LP.
           DISPLAY WD-J with highlight foreground-color 4 AT VDU-LP.
       D.  MOVE WM-DATUM TO WZ-DATUM.
           MOVE WD-J TO WZ-JAHR.
           PERFORM OPEN-REARC.
           IF WF-STATUS = 05 CLOSE RE-ARCHIV
               MOVE 0 TO WM-REOPEN
               DELETE FILE RE-ARCHIV
               DISPLAY "falsches Jahr" with highlight AT 2401
               PERFORM WEITER
               MOVE WM-DATUM TO WZ-DATUM
               GO B.
       G.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "   Nr.:        " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, < / >= Jahr +/- 1, <ret>= Rechnungsn
      -        "r., Nr.+<Bild  >= Kto.Nr." AT 2301.
           MOVE WA-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 2401 " - ".
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0000 " = "
                   WM-J with highlight foreground-color 4.
           CALL "CAUP" USING "1002116006" WH-CREG.
           IF AUF ADD 1 TO WD-J.
           IF AB ADD -1 TO WD-J.
           IF AUF or AB MOVE WD-J TO WK-J
                   ADD 145 VDU-ECK GIVING VDU-LP
                   DISPLAY WD-J with highlight foreground-color 4
                   AT VDU-LP GO D.
           IF ESC GO X.
           IF NOT RET and not BAB GO G.
           IF WH-NUM = 0 GO G.
           IF WM-REOPEN = 0
               DISPLAY "falsches Jahr" with highlight AT 2401
               PERFORM WEITER GO G.
      *------------------------------------------> BAB = nach Kto.Nr. <-
           IF BAB PERFORM SUCH-KTO.
           IF WH-NUM = 0 GO G.
           MOVE WH-NUM TO REA-NUM WV-NUM.
           MOVE 0 TO REA-LFD.
           ADD -8 TO VDU-LP.
           DISPLAY "Re-Nr.:        " AT VDU-LP.
           ADD 8 TO VDU-LP.
           MOVE WH-NUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           START RE-ARCHIV KEY NOT < REA-KEY INVALID
               PERFORM NO-REC GO G.
       H.  READ RE-ARCHIV NEXT AT END PERFORM NO-REC GO G.
           IF REA-NUM NOT = WV-NUM PERFORM NO-REC GO G.
           MOVE REA-NUM TO WD-NUM.
           DIVIDE 10 INTO REA-KTONR GIVING WD-KTO.
           DISPLAY WD-NUM with highlight AT VDU-LP
               "  Kunden-Nr.: " WD-KTO with highlight.
       I.  DISPLAY "<Ende>= drucken <esc/ret>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023370000" WH-CREG.
           IF ESC OR RET GO G.
           IF NOT ENDE GO I.
           PERFORM RE-DRUCK.
           GO G.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WM-REOPEN = 1 CLOSE RE-ARCHIV
               MOVE 0 TO WM-REOPEN.
       Z.  EXIT.
      ************************************************* Rechnubgsdruck *
       RE-DRUCK SECTION.
       A.  MOVE "PANDRUCK.LST" TO WH-ARCHIV.
           OPEN OUTPUT ARCHIV.
       K.  MOVE REA-REST TO AC-SATZ.
           READ RE-ARCHIV NEXT AT END GO N.
           IF WV-NUM NOT = REA-NUM GO N.
           WRITE AC-SATZ.
           GO K.
      *----------------------------------------------------> Ausdruck <-
       N.  PERFORM VARYING WI FROM 1006 BY -1 UNTIL WI < 2
               OR AC-REST(WI:1) NOT = SPACE CONTINUE.
           ADD 1 TO WI.
           IF WI < 1003 MOVE x"1A1A1A1A" TO AC-REST(WI:4).
           WRITE AC-SATZ.
           CLOSE ARCHIV.
           OPEN INPUT DRUDAT.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           IF WE-DRU(1) = 1 MOVE 5 TO WH-P.
           MOVE "KOPIE.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE 0 TO WZ-SCHALT.
           READ DRUDAT NEXT AT END GO W.
       Q.  READ DRUDAT NEXT AT END GO W.
           MOVE PA-ZEIL TO DRA-SATZ.
           PERFORM DRUCK.
           GO Q.
       W.  PERFORM END-DRU.
           CLOSE DRUDAT.
           DELETE FILE DRUDAT.
       Z.  EXIT.
      ******************************************************************
       ALLANZ SECTION.
       A.  ADD 202 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 9
               MOVE WI TO WD-POS
               DISPLAY WD-POS WITH HIGHLIGHT AT VDU-REL
               ADD 100 TO VDU-REL.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF.
           MOVE 3 TO WI.
       Z.  EXIT.
      *****************************************************************
       WASNUN SECTION.
       A.  IF WX = 0 PERFORM NO-REC GO Z.
       B.  DISPLAY "Zeile+<ret>= Auswahl, <ret-leer>/< >= weiterlisten,
      -        "< >= zur갷klisten" AT 2301.
           IF FINE DISPLAY "* keine Daten mehr *"
               with highlight AT 2458.
           DISPLAY "<esc> oder <#>= Ende, Zeile+<Ende>= drucken"
               AT 2401.
           CALL "CAUP" USING "0023711001" WH-CREG.
           MOVE 1 TO WR.
           IF ESC OR KIST MOVE 0 TO WH-NUM GO Z.
           IF WH-NUM > 0; IF WT-KEY(WH-NUM) = LOW-VALUE GO B ELSE GO Z.
           IF RET SET AB TO TRUE.
           IF FINE; IF AB  AND WI = 8 GO B
               ELSE IF AUF AND WI = 2 GO B.
           IF NOT AB AND NOT AUF GO A.
       Z.  EXIT.
      ************************************* nach Kontonummern anzeigen *
       SUCH-KTO SECTION.
       A.  IF WH-NUM = 0 GO Z.
       A.  CALL "CAUP" USING "0710111160001" WH-CREG.
           COMPUTE WH-NUM = WH-NUM * 10 + 2.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Kto.Nr:          " with background-color 1
               AT VDU-LP.
           MOVE WH-NUM TO REA-KTONR WV-NUM.
           DIVIDE 10 INTO REA-KTONR GIVING WD-KTO.
           ADD 9 TO VDU-LP.
           DISPLAY WD-KTO AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM ALLANZ.
           START RE-ARCHIV KEY not < REA-KTONR INVALID
                PERFORM NO-REC GO X.
       E.  READ RE-ARCHIV NEXT WITH NO LOCK AT END MOVE 8 TO WI GO L.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF REA-LFD not = 0 GO E.
           IF REA-KTONR not = WV-NUM MOVE 8 TO WI
               SET FINE TO TRUE GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF REA-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM REA-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ENDE and WH-NUM not = 0 GO Q.
       M.  IF ESC OR KIST MOVE 0 TO WH-NUM GO X.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO REA-KEY
               MOVE REA-NUM TO WH-NUM WH-WERT GO X.
           IF AB GO E.
       N.  READ RE-ARCHIV PREVIOUS NO LOCK AT END MOVE 2 TO WI GO L.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF REA-LFD not = 0 GO N.
           IF REA-KTONR not = WV-NUM MOVE 2 TO WI
               SET FINE TO TRUE GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF REA-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM REA-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
      *------------------------------------> Rechnungsdruck angew꼑lt <-
       Q.  MOVE WT-KEY(WH-NUM) TO REA-KEY.
           READ RE-ARCHIV INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO Q.
           MOVE REA-NUM TO WV-NUM.
           PERFORM RE-DRUCK.
           MOVE WT-KEY(1) TO REA-KEY.
           READ RE-ARCHIV INVALID GO X.
           MOVE REA-KTONR TO WV-NUM.
           GO C.
       X.  ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Rechnungsabfrage " with highlight AT VDU-LP.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       REA-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE REA-NUM TO WD-NUM.
           DISPLAY WD-NUM with highlight foreground-color 3 AT VDU-REL.
           ADD 8 TO VDU-REL.
           IF REA-LFD not = 0 MOVE REA-LFD TO WD-POS
               DISPLAY WD-POS AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE REA-KEY TO WT-KEY(WX).
       Z.  EXIT.
      ******************************************************************
       LFS-ARCHIV SECTION.
       A.  CALL "CAUP" USING "071010086000" WH-CREG.
           MOVE WE-DRU(2) TO WM-DRU.
           MOVE 0 TO WM-LFOPEN.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lieferscheinabfrage " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kd-Nr.: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Kundennummer" AT 2301.
           CALL "CAUP" USING "1002116006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WH-NUM = 0 GO C.
           COMPUTE LFA-KTONR = WH-NUM * 10 + 2.
           DIVIDE 10 INTO LFA-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           MOVE LFA-KTONR TO WV-KTONR.
       E.  ADD 220 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lf.-Datum:         " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Lieferscheindatum" AT 2301.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1102316006" WH-CREG.
           IF WOLI GO C.
           IF ESC GO X.
           IF NOT RET GO E.
           IF WZ-DATUM = 0 GO E.
           MOVE WZ-DATUM TO WV-DATUM LFA-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WZ-DATUM TO AC-DATUM.
           PERFORM BILDJAHR.
           PERFORM OPEN-LFARC.
           IF WF-STATUS = 05 CLOSE LF-ARCHIV
               MOVE 0 TO WM-LFOPEN
               DELETE FILE LF-ARCHIV
               DISPLAY "Archivierungsjahr fehlt" with highlight AT 2401
               PERFORM WEITER
               GO E.
           MOVE 0 TO LFA-NUM LFA-LFD WV-LFNUM.
           START LF-ARCHIV KEY NOT < LFA-KEY INVALID GO H.
       G.  READ LF-ARCHIV NEXT AT END GO H.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF WV-KTONR NOT = LFA-KTONR GO H.
           IF WV-LFNUM = LFA-NUM GO G.
           MOVE LFA-NUM TO WV-LFNUM.
           ADD 211 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO LFA-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 231 VDU-ECK GIVING VDU-LP.
           MOVE LFA-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 0242 VDU-ECK GIVING VDU-LP.
           MOVE LFA-NUM TO WD-NUM.
           DISPLAY "Lfs-Nr.: " AT VDU-LP WD-NUM with highlight.
           GO I.
       H.  DISPLAY "** Dateiende **" with highlight AT 2465.
       I.  DISPLAY "<Ende>= drucken, < >= weiter anzeigen, <esc/ret>= Ab
      -        "bruch < >"AT 2301.
           CALL "CAUP" USING "0023600000" WH-CREG.
           IF WF-STATUS = 10 OR WF-STATUS = 23 GO C.
           IF AB CALL "CAUP" USING "16CLRFEN" WH-CREG
                 ADD 203 VDU-ECK GIVING VDU-LP
                 DISPLAY "Kd-Nr.: " AT VDU-LP
                 ADD 220 VDU-ECK GIVING VDU-LP
                 DISPLAY "Lf.-Datum: " AT VDU-LP
                 GO G.
           IF ESC OR RET GO C.
           IF NOT ENDE GO I.
           IF WV-KTONR NOT = LFA-KTONR GO C.
           MOVE "PANDRUCK.LST" TO WH-ARCHIV.
           OPEN OUTPUT ARCHIV.
       K.  MOVE LFA-REST TO AC-SATZ.
           READ LF-ARCHIV NEXT AT END GO N.
           IF WV-LFNUM NOT = LFA-NUM or WV-KTONR NOT = LFA-KTONR GO N.
           WRITE AC-SATZ.
           GO K.
      *----------------------------------------------------> Ausdruck <-
       N.  PERFORM VARYING WI FROM 1006 BY -1 UNTIL WI < 2
               OR AC-REST(WI:1) NOT = SPACE CONTINUE.
           ADD 1 TO WI.
           IF WI < 1003 MOVE x"1A1A1A1A" TO AC-REST(WI:4).
           WRITE AC-SATZ.
           CLOSE ARCHIV.
           OPEN INPUT DRUDAT.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "ARCHIV.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE 0 TO WZ-SCHALT.
           READ DRUDAT NEXT AT END GO W.
       Q.  READ DRUDAT NEXT AT END GO W.
           MOVE PA-ZEIL TO DRA-SATZ.
           PERFORM DRUCK.
           GO Q.
       W.  PERFORM END-DRU.
           CLOSE DRUDAT.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WM-LFOPEN = 1 CLOSE LF-ARCHIV
               MOVE 0 TO WM-LFOPEN.
       Z.  EXIT.
      ******************************************************************
       COMLINE SECTION.
       A.  DISPLAY WD-UPON UPON COMMAND-LINE.
           DISPLAY LOW-VALUE AT 2301.
           CALL X"91" USING RESULT FUNKT PARAM.
           CALL "CAUP" USING "1323012480" WH-CREG.
       Z.  EXIT.
      ****************************************** Druck von LST-Dateien *
       ARCHIVIEREN SECTION.
       A.  IF WL-CA = 99 GO Z.
           MOVE "DIR PAC*.DAT >DRUHILF.DAT" TO WD-UPON
           PERFORM COMLINE.
           MOVE 0 TO WH-MOD WM-LFOPEN WM-REOPEN WV-LJ WV-RJ.
      *------------------------------------------> ob-Datei vorhanden <-
           MOVE "DRUHILF.DAT" TO WH-FNAME.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           IF WF-SIZE = 0 OPEN OUTPUT HILFPOS
               CLOSE HILFPOS.
           OPEN INPUT HILFPOS.
       E.  PERFORM OBDATEN.
           IF WM-REOPEN = 1 CLOSE RE-ARCHIV.
       H.  MOVE "DIR LF*.* >DRUHILF.DAT" TO WD-UPON
           PERFORM COMLINE.
           MOVE 1 TO WH-MOD.
      *------------------------------------------> ob-Datei vorhanden <-
           MOVE "DRUHILF.DAT" TO WH-FNAME.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           IF WF-SIZE = 0 OPEN OUTPUT HILFPOS
               CLOSE HILFPOS.
           OPEN INPUT HILFPOS.
       E.  PERFORM OBDATEN.
           IF WM-LFOPEN = 1 CLOSE LF-ARCHIV.
           DELETE FILE HILFPOS.
       Z.  EXIT.
      ******************************************************************
       OBDATEN SECTION.
       A.  CALL "CAUP" USING "0705120548020" WH-CREG.
           ADD 103  VDU-ECK GIVING VDU-LP.
           DISPLAY " Archivierungsdateien " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Nr. Bezeichnung      Gr붳e  Erstellungsdaten"
               AT VDU-LP
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL "�" with SIZE 46 AT VDU-LP.
           MOVE 0 TO IX WE.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
       C.  READ HILFPOS NEXT AT END CLOSE HILFPOS
               MOVE 0 TO WE
               OPEN INPUT HILFPOS GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF HI-SATZ(1:3) not = "PAC" and HI-SATZ(1:2) not = "LF" GO C.
           MOVE HI-SATZ(1:8) TO WH-FNAME.
           MOVE "." TO WH-FNAME(9:1).
           MOVE HI-SATZ(10:3) TO WH-FNAME(10:3).
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           IF WF-SIZE = 0 GO B.
                ADD 403 VDU-ECK GIVING VDU-LP.
                MOVE IX TO WD-ZL
                DISPLAY WD-ZL AT VDU-LP
                ADD 4 TO VDU-P
                DISPLAY WH-FNAME with highlight AT VDU-LP
                MOVE WF-SIZE TO WD-SIZE
                ADD 12 TO VDU-P
                DISPLAY WD-SIZE AT VDU-LP
                MOVE WF-TT TO VDU-TAG
                MOVE WF-MO TO VDU-MONAT
                MOVE WF-YY TO VDU-JAHR
                MOVE WF-HH TO VDU-STD
                MOVE ":" TO VDU-DD
                MOVE WF-MM TO VDU-MIN
                ADD 12 TO VDU-P
                DISPLAY VDU-DATUM with highlight AT VDU-LP " -"
                ADD 11 TO VDU-P
                DISPLAY VDU-ZEIT with highlight AT VDU-LP.
           IF WH-MOD = 0 PERFORM RE-ABLEGEN
                    else PERFORM LF-ABLEGEN.
           DELETE FILE ARCHIV.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE HILFPOS.
       Z.  EXIT.
      *********************************************** Rechnung ablegen *
       RE-ABLEGEN SECTION.
       A.  MOVE WH-FNAME TO WH-ARCHIV.
           OPEN INPUT ARCHIV.
       C.  READ ARCHIV NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE AC-RENUM TO REA-NUM.
           MOVE AC-KTONR TO REA-KTONR.
           MOVE AC-DATUM TO WZ-DATUM.
           PERFORM BILDJAHR.
           PERFORM OPEN-REARC.
           MOVE 0 TO REA-LFD.
       E.  MOVE AC-SATZ TO REA-REST.
           WRITE REA-SATZ INVALID KEY REWRITE REA-SATZ.
       F.  MOVE SPACE TO AC-SATZ.
           ADD 1 TO REA-LFD.
       G.  READ ARCHIV NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO G.
           GO E.
       X.  CLOSE ARCHIV.
       Z.  EXIT.
      ****************************************** Lieferscheine ablegen *
       LF-ABLEGEN SECTION.
       A.  MOVE WH-FNAME TO WH-ARCHIV.
           OPEN INPUT ARCHIV.
       C.  READ ARCHIV NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE AC-RENUM TO LFA-NUM.
           MOVE AC-KTONR TO LFA-KTONR.
           MOVE AC-DATUM TO WZ-DATUM.
           PERFORM BILDJAHR.
           PERFORM OPEN-LFARC.
           MOVE 0 TO LFA-LFD.
       E.  MOVE AC-SATZ TO LFA-REST.
       F.  WRITE LFA-SATZ INVALID ADD 10 TO LFA-NUM GO F.
           MOVE SPACE TO AC-SATZ.
           ADD 1 TO LFA-LFD.
       G.  READ ARCHIV NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO G.
           GO E.
       X.  CLOSE ARCHIV.
       Z.  EXIT.
      *************************** Jahreseinstellung Lieferscheinarchiv *
       OPEN-LFARC SECTION.
       A.  MOVE WK-J TO WV-LJ.
           MOVE AC-DATUM TO LFA-DATUM.
           IF WV-LJ NOT = WN-LJ;
               IF WM-LFOPEN = 1 CLOSE LF-ARCHIV
                   MOVE 0 TO WM-LFOPEN.
           MOVE WV-LJ TO WN-LJ.
           IF WM-LFOPEN = 0 OPEN I-O LF-ARCHIV
               MOVE 1 TO WM-LFOPEN.
       Z.  EXIT.
      ****************************** Jahreseinstellung Rechnungsarchiv *
       OPEN-REARC SECTION.
       A.  MOVE WK-J TO WV-RJ.
           IF WV-RJ NOT = WN-RJ;
               IF WM-REOPEN = 1 CLOSE RE-ARCHIV
                   MOVE 0 TO WM-REOPEN.
           MOVE WV-RJ TO WN-RJ.
           IF WM-REOPEN not = 0 GO Z.
      *    OPEN INPUT RE-ARCHIV.
      *    IF WF-STATUS NOT = 00
      *    CLOSE RE-ARCHIV.
           OPEN I-O RE-ARCHIV.
           MOVE 1 TO WM-REOPEN.
       Z.  EXIT.
