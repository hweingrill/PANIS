      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PAKOJOUR.
      ******************************************************************
      *        Journalende mit autom. Sammelbuchungen
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY PAKOSEC.CPY.
       COPY PARSEART.CPY.
           SELECT DRUCKER   ASSIGN TO PRINTER WH-DRUNAM
                            ORGANIZATION RECORD SEQUENTIAL
                            FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PAKOFD.CPY.
       COPY PARFDART.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR.
               05 DRA-KTONR            PIC ZZZ.ZZZ-.
               05 DRA-BELNR            PIC ZZZ.ZZZ-.
               05 DRA-BELDAT           PIC X(9).
               05 DRA-BET              PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-KOA              PIC ZZ9-.
               05 DRA-BEZ              PIC X(16).
               05 DRA-MENGE            PIC ZZZ.ZZ9,99-.
               05 DRA-PREIS            PIC ZZZ.ZZ9,99-.
           03  FILLER                  PIC X.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WH-SIZE                 PIC 99        COMP   VALUE 63.
           03  WM-L                    PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WX-J                    PIC 9999      COMP.
           03  WK-MCODE                PIC X(10).
           03  WH-BZSATZ               PIC X(128).
           03  WV-BAS                  PIC S9(9)V99  COMP.
           03  WH-TEIL                 PIC S999V9(7) COMP.
           03  WK-BET                  PIC S9(9)V99  COMP.
           03  WK-SKTO                 PIC S9(9)V99  COMP.
           03  WK-MWST                 PIC S9(7)V99  COMP.
           03  WK-BASIS                PIC S9(9)V99  COMP.
           03  WK-SOLL                 PIC S9(9)V99  COMP.
           03  WK-HABEN                PIC S9(9)V99  COMP.
           03  WK-BRUTTO               PIC S9(9)V99  COMP.
           03  WK-NETTO                PIC S9(9)V99  COMP.
           03  WK-BUBET                PIC S9(9)V99  COMP.
           03  WS-SAMMEL               PIC S9(9)V99  COMP.
           03  WV-BAKEY                PIC 999       COMP.
           03  WM-ARSYM                PIC 99        COMP.
           03  WM-GSSYM                PIC 99        COMP.
           03  WM-ZASYM                PIC 99        COMP.
           03  WH-AKAT                 PIC 9         COMP.
           03  WH-ABN                  PIC 99        COMP.
           03  WH-GKAT                 PIC 9         COMP.
           03  WH-GBN                  PIC 99        COMP.
           03  WH-BN                   PIC 99        COMP.
           03  WM-USE                  PIC 9         COMP.
           03  WM-GB                   PIC 99        COMP.
             88 OHNE      VALUE 0.   88 EINZEL     VALUE 1.
             88 SAMMEL    VALUE 2.   88 AUFTEIL    VALUE 4.
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(6)      COMP.
               05 WH-LFD               PIC 99        COMP-X.
           03  WH-JOKEY                PIC X(4).
           03  WH-KAT                  PIC 9         COMP.
           03  WC-UST                  PIC S99V9999  COMP.
           03  WH-UST                  PIC S99V99    COMP.
           03  WH-JS                   PIC 9(7)      COMP.
           03  WT-SH.
               05 WR-SH                PIC 9     OCCURS 36.
           03  WK-JS                   PIC 9(7).
           03  WR-JS REDEFINES WK-JS.
               05 WK-SEITE             PIC 99999.
               05 WK-ZEIL              PIC 99.
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WD-SEITE                PIC ZZ9.
       01  WO-TSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  WO-KTONR                PIC 9(6)     COMP   OCCURS 20.
       COPY PAKOEXT.CPY.
       COPY PAKODECL.CPY.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 50 PERFORM JOUREND.
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
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 07 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       NODATA SECTION.
       A.  DISPLAY "** keine Daten mehr **" with highlight blink
               AT 2458.
           SET FINE TO TRUE.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) NOT = "LPT" GO Z.
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
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE X"1B210000" TO DRA-SATZ(1:4).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  PERFORM DRU-OK.
           IF WM-OPEN > 0 GO C.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE WK-STG(WH-P1) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           MOVE WK-STG(WH-P2) TO DRA-SATZ.
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       LADE-DRU SECTION.
       A.  MOVE 12 TO WH-KEY.
           READ KONSLAG WITH NO LOCK NOT INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************************************
       JOUREND SECTION.
       A.  CALL "CAUP" USING "0710050673000" WH-CREG.
           MOVE "JOURNAL.LST" TO WH-DRUNAM.
           PERFORM LADE-DRU.
           OPEN EXTEND DRUCKER.
           MOVE 9 TO WH-PG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Journalabschlu· " AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           PERFORM LAST-JOUR.
           IF ESC GO X.
           MOVE LB-BUDAT TO WE-PER.
           ADD WE-USE 1 GIVING WH-KEY.
       C.  READ KONSLAG INVALID INITIALIZE KL-BSATZ
               MOVE WH-KEY TO KL-NUM WRITE KL-SATZ GO C.
           IF ZUGRIF STOP RUN.
           DISPLAY "Journaldatum: " AT VDU-LP.
           MOVE LB-BUDAT TO WC-DATUM WZ-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0000.
           MOVE LB-BUDAT TO WZ-DATUM.
           GO F.
      *-----------------------------------------> neues Buchungsdatum <-
       E.  DISPLAY "<ret>= Datum, <esc>= Abbruch" AT 2301.
           MOVE LB-BUDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1102176006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       F.  MOVE WE-PER TO WC-DATUM.
           IF WZ-JAHR NOT = WC-TAG OR WZ-MONAT NOT = WC-MONAT
               DISPLAY "Datumsfehler" with BLINK highlight AT 2401
               PERFORM WEITER
               MOVE KL-BUDAT TO WC-DATUM
               PERFORM DATDREH
               ADD 217 VDU-ECK GIVING VDU-LP
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               GO E.
           MOVE WZ-DATUM TO LB-BUDAT KL-BUDAT WS-DATUM.
           REWRITE KL-SATZ.
       K.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "S:                " AT VDU-LP.
           MOVE 0 TO WH-WERT.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               ADD KL-GRUPPE(WX) TO WH-WERT.
           MOVE WH-WERT TO WD-SALDO.
           ADD 3 TO VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
      *    DISPLAY " Kein Journalabschlu· erforderlich! " with blink
      *        highlight AT VDU-LP.
      *    DISPLAY LOW-VALUE AT 2401.
      *    PERFORM WEITER.
      *    GO X.
       Q.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Journalabschlu· durchfÅhren? < >, <esc>= nein, <Ende
      -        ">= ja, <>= Datum" AT VDU-LP.
           CALL "CAUP" USING "1004330000" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT ENDE GO Q.
           DISPLAY ALL SPACES with size 50 at 2501.
           MOVE WH-DATUM TO WC-DATUM WZ-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           ADD 1900 WZ-JAHR GIVING WX-J.
           IF WX-J < 1990 ADD 100 TO WX-J.
           PERFORM JOURDRUCK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
           UNLOCK LAGERBZ.
       Z.  EXIT.
      **************************************** Buchungszeile speichern *
       LB-WRITE SECTION.
       A.  PERFORM VARYING WS-BUCH FROM 118 BY -1 UNTIL WS-BUCH = 68
               OR LB-SATZ(WS-BUCH:1) NOT = SPACE CONTINUE.
           MOVE LB-SATZ TO WH-BZSATZ.
           MOVE LB-KEY TO WH-BKEY.
           MOVE 253 TO LB-LFD.
           START LAGERBZ KEY < LB-KEY INVALID
               MOVE 1 TO WH-LFD GO E.
       C.  READ LAGERBZ PREVIOUS WITH NO LOCK AT END
               MOVE 1 TO WH-LFD GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 1 LB-LFD TO WH-LFD.
           IF LB-KTONR NOT = WH-KTONR OR LB-BUDAT NOT = WH-BUDAT
               MOVE 1 TO WH-LFD.
       E.  MOVE WH-BZSATZ TO LB-SATZ.
           MOVE WH-BKEY TO LB-KEY.
           MOVE WS-BUCH TO WZ-BUCH.
           ADD 1 TO WH-JS.
           MOVE WH-JS TO LB-SEITE.
           WRITE LB-SATZ.
       Z.  EXIT.
      ******************************************************************
       LAST-JOUR SECTION.
       A.  MOVE 1 TO IX.
           MOVE 999999 TO LB-SEITE.
           MOVE WE-USE TO LB-USE.
           START LAGERBZ KEY < LB-JKEY INVALID MOVE 99 TO LB-USE GO C.
           MOVE SPACE TO LB-SATZ(12:).
           READ LAGERBZ PREVIOUS NO LOCK AT END MOVE 99 TO LB-USE GO C.
       C.  IF WE-USE = LB-USE MOVE LB-SEITE TO WH-JS GO Z.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Keine Journaldaten vorhanden!" with highlight
               AT VDU-LP " < > weiter mit <ret>".
           CALL "CAUP" USING "1003340000" WH-CREG.
           SET ESC TO TRUE.
       Z.  EXIT.
      ******************************************************************
       FIRST-JOUR SECTION.
       A.  MOVE WE-USE TO LB-USE.
           MOVE 0 TO LB-SEITE.
           START LAGERBZ KEY NOT < LB-JKEY INVALID
               MOVE 0 TO LB-SEITE GO E.
           MOVE SPACE TO LB-SATZ(12:).
       C.  READ LAGERBZ NEXT NO LOCK AT END MOVE 0 TO LB-SEITE.
           IF ZUGRIF PERFORM BESETZT GO C.
       E.  IF LB-USE NOT = WE-USE SET ESC TO TRUE GO Z.
       Z.  EXIT.
      ****************************** suchen der aktuellen Journalseite *
       JOUR-SUCH SECTION.
       A.  MOVE WE-WKZ TO LB-USE.
           MOVE 8000000 TO LB-SEITE.
           MOVE 0 TO WK-SEITE.
           START LAGERBZ KEY <= LB-JKEY INVALID GO X.
           MOVE SPACE TO LB-TX.
       C.  READ LAGERBZ PREVIOUS AT END GO X.
      *C.  READ LAGERBZ NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LB-USE = WE-WKZ MOVE LB-SEITE TO WK-JS.
       X.  MOVE 0 TO WK-ZEIL.
       Z.  EXIT.
      *************************************************** Journaldruck *
       JOURDRUCK SECTION.
       A.  ADD 511 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WK-SOLL WK-HABEN.
           MOVE 0 TO WV-DATUM WZ-ZEILEN.
           PERFORM JOUR-SUCH.
           PERFORM FIRST-JOUR.
           MOVE LB-JKEY TO WH-JOKEY.
       E.  MOVE WH-JOKEY TO LB-JKEY.
           START LAGERBZ KEY NOT < LB-JKEY INVALID GO Q.
           MOVE SPACE TO LB-TX.
       G.  READ LAGERBZ NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF LB-USE NOT = WE-USE GO Q.
           MOVE LB-JKEY TO WH-JOKEY.
           PERFORM JOURKOPF.
           IF WV-DATUM NOT = LB-BUDAT MOVE LB-BUDAT TO WC-DATUM WV-DATUM
               PERFORM DATDREH
               MOVE "Buchungsdatum:" TO DRA-STR(1:)
               MOVE VDU-DATUM TO DRA-STR(16:10)
               PERFORM DRUCK.
      *    MOVE 0 TO LB-USE.
           MOVE WE-WKZ TO LB-USE.
           ADD 1 TO WK-ZEIL.
           MOVE WK-JS TO LB-SEITE.
           MOVE LB-KTONR TO DRA-KTONR.
           MOVE LB-BELNR TO DRA-BELNR.
           IF LB-BELDAT = 0 MOVE LB-BUDAT TO LB-BELDAT.
           MOVE LB-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-BELDAT.
           MOVE LB-TX TO DRA-BEZ.
           MOVE LB-MENGE TO DRA-MENGE.
           IF LB-PREIS NOT = 0 MOVE LB-PREIS TO DRA-PREIS.
           DISPLAY DRA-STR(1:40) with highlight AT VDU-LP.
           PERFORM DRUCK.
           REWRITE LB-SATZ.
           GO E.
       Q.  MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "Journalsummen" TO DRA-BEZ.
           MOVE KL-JSUM TO DRA-BET.
           PERFORM DRUCK.
      *    PERFORM GRUP-SUM.
           MOVE 1 TO WH-KEY.
       S.  READ KONSLAG INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO S.
           MOVE WK-SEITE TO KL-JONUM.
           REWRITE KL-SATZ.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************************************* Gruppenjournalsummen *
       GRUP-SUM SECTION.
       A.  PERFORM DRUCK
           MOVE 1 TO WX.
       C.  IF KL-GRUPPE(WX) = 0 AND KL-GRUPPE(WX + 20) GO G.
           PERFORM JOURKOPF.
           ADD WX 20 GIVING WH-KEY.
       D.  READ KONSLAG INVALID MOVE " - fehlt -" TO KL-GRPTX.
           IF ZUGRIF PERFORM BESETZT GO D.
           MOVE WX TO DRA-KOA.
           MOVE KL-GRPTX TO DRA-BEZ.
           ADD WE-USE 1 GIVING WH-KEY.
       F.  READ KONSLAG INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO F.
           MOVE KL-GRUPPE(WX) TO DRA-BET.
           MOVE KL-GRUPPE(WX + 20) TO DRA-MENGE.
           PERFORM DRUCK.
       G.  IF WX < 20 ADD 1 TO WX GO C.
           MOVE 0 TO KL-JSUM KL-BUDAT.
           PERFORM VARYING WX FROM  1 BY 1 UNTIL WX > 20
               MOVE 0 TO KL-GRUPPE(WX) KL-GRUPPE(WX + 20).
           REWRITE KL-SATZ.
       Z.  EXIT.
      **************************************************** Journalkopf *
       JOURKOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                   ELSE MOVE 4 TO WH-P2
                        MOVE 63 TO WH-SIZE.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "Journal per:" TO DRA-STR(36:).
           MOVE LB-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(49:10).
           MOVE "Seite:" TO DRA-STR(64:).
           ADD 1 TO WK-SEITE.
           MOVE 0 TO WK-ZEIL.
           MOVE WK-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRA-STR(71:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "Konto-Nr Bel-Nr  Bel-Dat.       Betrag KOA Bezeichnung
      -       "         Menge      Preis " TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR.
           PERFORM DRUCK.
       Z.  EXIT.
