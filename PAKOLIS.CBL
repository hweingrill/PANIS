      $SET LINKCOUNT "240" ANS85 AUTOLOCK BOUND NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PAKOLIS.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY PAKOSEC.CPY.
       COPY PARSEART.CPY.
       COPY PARSELFT.CPY.
       COPY PAKSEWAG.CPY.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PAKOFD.CPY.
       COPY PARFDART.CPY.
       COPY PARFDLFT.CPY.
       COPY PAKFDWAG.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
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
               05 DRA-UMS              PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-MENGE            PIC ZZ.ZZZ.ZZZ,ZZ-.
           03  FILLER                  PIC X(14).
       01  DRB-SATZ.
           03  FILLER                  PIC X(7).
           03  DRB-STR.
               05 DRB-KONUM            PIC ZZZ9-.
               05 DRB-BEZ              PIC X(27).
               05 DRB-STAND            PIC X(10).
               05 DRB-MIND             PIC X(10).
               05 DRB-BEST             PIC X(10).
               05 DRB-MEH              PIC XXX.
       01  DRD-SATZ.
           03  FILLER                  PIC X(5).
           03  DRD-STR.
               05 DRD-KONUM            PIC ZZZ9-.
               05 DRD-BEZ              PIC X(27).
               05 DRD-INHALT           PIC X(10).
               05 DRD-IMEH             PIC X(7).
               05 DRD-BEST             PIC X(10).
               05 DRD-MEH              PIC XXX.
       01  DRC-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRC-STR.
               05 DRC-KTONR            PIC ZZZZZ-.
               05 DRC-BEZ.
                  07 DRC-ATX           PIC XXX.
                  07 DRC-ASTAND        PIC ZZZ.ZZ9,99-.
                  07 FILLER            PIC X.
                  07 DRC-BTX           PIC X(5).
                  07 DRC-BSTAND        PIC ZZZ.ZZ9,99-.
               05 DRC-KST              PIC XXXX.
               05 DRC-SUM                           OCCURS 5.
                  07 DRC-KOTX.
                     09 DRC-BET        PIC ZZ.ZZZ.ZZ9-.
                     09 DRC-STRI       PIC X.
                     09 DRC-EH         PIC ZZZ9,99-.
       01  DRF-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRF-STR.
               05 DRF-ARTNR            PIC ZZ.ZZ9-.
               05 DRF-DATUM            PIC X(9).
               05 DRF-RPNR             PIC ZZ.ZZ9-.
               05 DRF-RPBEZ            PIC X(16).
               05 DRF-ARBEZ            PIC X(26).
               05 DRF-SOLL             PIC ZZ9.999-.
               05 DRF-IST              PIC ZZ9.999-.
       01  DRI-SATZ.
           03  FILLER                  PIC X(10).
           03  DRI-STR.
               05 DRI-LGNUM            PIC Z.ZZ9-.
               05 DRI-ACD              PIC X(5).
               05 DRI-BEZ              PIC X(27).
               05 DRI-BCD              PIC X(4).
               05 DRI-LFT              PIC X(9).
               05 DRI-SORT             PIC ZZ9,99-.
               05 DRI-USTR             PIC X(12).
               05 DRI-FILL             PIC X.
               05 DRI-MEH              PIC XXX.
               05 DRI-FILLER           PIC X(4).
       01  DRJ-SATZ.
           03  FILLER                  PIC X(6).
           03  DRJ-STR.
               05 DRJ-LGNUM            PIC Z.ZZ9-.
               05 DRJ-BEZ              PIC X(27).
               05 DRJ-LFT              PIC X(6).
               05 DRJ-MENGE            PIC X(10).
               05 DRJ-MEH              PIC XXXX.
               05 DRJ-PREIS            PIC ZZ.ZZ9,99-.
               05 DRJ-BET              PIC ZZ.ZZZ.ZZ9,99-.
       01  DRK-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRK-STR.
               05 FILLER               PIC X.
               05 DRK-BUDAT            PIC X(9).
               05 DRK-BELNR            PIC Z(7)-.
               05 DRK-BELDAT           PIC X(9).
               05 DRK-SYM              PIC XXX.
               05 DRK-GGKTO            PIC ZZZZZ,Z-.
               05 DRK-GGART            PIC XX.
               05 DRK-JS               PIC ZZZ9-.
               05 DRK-BEZ              PIC X(41).
               05 DRK-MWSKT            PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 FILLER               PIC X.
               05 DRK-UST              PIC ZZ-.
               05 DRK-SOLL             PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRK-HABEN            PIC ZZ.ZZZ.ZZZ,ZZ-.
       01  DRL-SATZ.
           03  FILLER                  PIC XXX.
           03  DRL-STR.
               05 DRL-LGNUM            PIC Z.ZZ9-.
               05 DRL-BEZ              PIC X(26).
               05 DRL-LFT              PIC X(6).
               05 DRL-LAGER            PIC X(6).
               05 DRL-SORT             PIC ZZ9,99-.
               05 DRL-PREIS            PIC ZZ.ZZ9,99-.
               05 DRL-BMENGE           PIC X(10).
               05 DRL-MSTAND           PIC X(10).
               05 DRL-STAND            PIC X(10).
       01  DRM-SATZ.
           03  FILLER                  PIC X(6).
           03  DRM-STR.
               05 DRM-LGNUM            PIC Z.ZZ9-.
               05 DRM-BEZ              PIC X(27).
               05 DRM-LFT              PIC X(6).
               05 DRM-SMENGE           PIC X(10).
               05 DRM-MEH              PIC XXXX.
               05 DRM-IMENGE           PIC X(10).
               05 DRM-DIFFER           PIC X(10).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WH-BZSATZ               PIC X(64).
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 999       COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WH-SIZE                 PIC 99        COMP   VALUE 63.
           03  WH-MEH                  PIC 99        COMP.
           03  WH-NK                   PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WK-KO                   PIC 99        COMP.
           03  WM-ART                  PIC 99        COMP.
           03  WM-WJAHR                PIC 9(6)      COMP   OCCURS 2.
           03  WM-ZEIL                 PIC 99        COMP.
           03  WB                      PIC 99        COMP.
           03  WP                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  WG                      PIC 999       COMP.
           03  WV-BISJ                 PIC 9(5)      COMP.
           03  WZ-UMS                  PIC S9(9)V99  COMP.
           03  WZ-MENGE                PIC S9(9)V99  COMP.
           03  WG-UMS                  PIC S9(9)V99  COMP.
           03  WG-MENGE                PIC S9(9)V99  COMP.
           03  WH-EH                   PIC S9(9)V99  COMP.
           03  WS-SUM                  PIC S9(9)V99  COMP.
           03  WV-WTOL                 PIC 9999V99   COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WM-UMS                  PIC S9(9)V99  COMP.
           03  WV-UMS                  PIC S9(9)V99  COMP.
           03  WS-ART                  PIC 9(5)      COMP   OCCURS 3.
           03  wj-zeil                 pic 9(7)      COMP.
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(6)      COMP.
               05 WH-LFD               PIC 99        COMP-X.
           03  WH-KAT                  PIC 9         COMP.
           03  WK-JS                   PIC 9(7).
           03  WR-JS REDEFINES WK-JS.
               05 WK-SEITE             PIC 99999.
               05 WK-ZEIL              PIC 99.
           03  WV-SEITE                PIC 99999.
           03  WV-WBEG                 PIC 9(6)      COMP.
           03  WV-WEND                 PIC 9(6)      COMP.
           03  WV-WKZ                  PIC 99        COMP.
           03  WV-LFT                  PIC X(5).
           03  WV-LAGER                PIC X(5).
           03  WV-ABCODE               PIC X(5).
           03  WV-BISCODE              PIC X(5).
           03  WV-ALT                  PIC X(10).
      *-----------------------------------------------> Anzeigefelder <-
           03  VDU-ALT                 PIC 9999.
           03  WD-JONUM                PIC Z.ZZ9-.
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-UMSATZ               PIC ZZZ.ZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WD-PZ                   PIC 99.
           03  WD-SEITE                PIC ZZ9.
           03  WH-MENGE                PIC S9(7)V99   COMP.
           03  WS-MENGE                PIC S9(7)V99   COMP.
           03  WI-MENGE                PIC S9(7)V99   COMP.
           03  WD-MENGA                PIC ZZ.ZZ9,99-.
           03  WD-MENGB                PIC ZZ.ZZ9,9-.
           03  WD-MENGC                PIC ZZZ.ZZ9-.
           03  WD-MENGE                PIC X(10).
           03  WT-DRC.
               05 WT-DR                PIC X(30)     OCCURS 2.
           03  WT-GRP.
               05 WS-NUM               PIC 9(5)       COMP  OCCURS 99.
               05 WS-GRP               PIC S9(7)V99   COMP  OCCURS 99.
       COPY PAKOEXT.CPY.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON PROTWAAG.
       A.  CALL "CADECL" USING "PAKWAAG.DAT " WH-CREG.
       COPY PAKODECL.CPY.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 70 PERFORM LI-WAHL.
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
           IF WM-DRU = 0 MOVE X"1B210000" TO DRA-SATZ(1:4).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  PERFORM DRU-OK.
           IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           IF WH-PG = 1 OPEN EXTEND DRUCKER
                   else OPEN OUTPUT DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE WK-STG(WH-P1) TO DRA-SATZ.
           IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           IF WM-DRU = 1 GO X.
           MOVE WK-STG(WH-P2) TO DRA-SATZ.
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
       X.  MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       LADE-DRU SECTION.
       A.  MOVE 12 TO WH-KEY.
           READ KONSLAG WITH NO LOCK NOT INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************************************
       LI-WAHL SECTION.
       A.  CALL "CAUP" USING "0705121848000" WH-CREG.
       B.  PERFORM LADE-DRU.
           MOVE 9 TO WH-PG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Auswertungen " with highlight AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Bestellung"           AT 0717.
           DISPLAY "2 - Bestellvorschlag"     AT 0017.
           DISPLAY "3 - Lieferanten"          AT 0017.
           DISPLAY "4 - Inventuraufnahme"     AT 0017.
           DISPLAY "5 - Inv-Istliste"         AT 0017.
           DISPLAY "6 - Matchcode  "          AT 0017.
           DISPLAY "7 - Nummern    "          AT 0017.
           DISPLAY "8 - Journaldruck"         AT 0017.
           DISPLAY "9 - Soll-Ist-Vergleich"   AT 0017.
           DISPLAY "10 - Lîschung"            AT 0016.
           DISPLAY "11 - Verbrauchsliste"     AT 0016.
           DISPLAY "<ret-leer>= Programmende" AT 0018.
           DISPLAY " " AT 0018.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT 0032.
       G.  CALL "CAUP" USING "1016392002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC OR WH-PG = 0 GO X.
           IF NOT RET GO G.
           EVALUATE WH-PG
               WHEN 1
               WHEN 2 PERFORM BESTVOR GO A
               WHEN 3
               WHEN 4
               WHEN 5
               WHEN 6
               WHEN 7
               WHEN 9 PERFORM LAGLIST
               WHEN 8 MOVE "PAKOJOUR50UEBE" TO WT-TX
                      MOVE 49 TO WH-PG GO X
               WHEN 10 PERFORM LOESCHEN
               WHEN 11 PERFORM ART-LIST.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
           UNLOCK LAGERBZ.
       Z.  EXIT.
      ************************************************ LB-SATZ lîschen *
       INIT-BZ SECTION.
       A.  MOVE LB-SATZ(1:11) TO WH-MCODE.
           INITIALIZE LB-SATZ.
           MOVE WH-MCODE TO LB-SATZ(1:11).
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       KS-RECH SECTION.
       A.  IF WH-NUM = 0 MOVE 0 TO WM-ART
                    else MOVE 1 TO WM-ART.
           MOVE 0 TO LG-NUM.
           START LAGARTIK KEY NOT < LG-NUM INVALID GO Z.
       G.  READ LAGARTIK NEXT NO LOCK AT END MOVE 0 TO LB-BUDAT GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE LG-NUM TO LB-KTONR.
           DISPLAY LG-NUM AT 1901.
           MOVE 0 TO LB-LFD WM-UMS WB WV-UMS.
           IF WH-PG = 6 MOVE WV-WBEG TO LB-BUDAT
                   else MOVE WE-WJAHR(1) TO LB-BUDAT.
           START LAGERBZ KEY NOT < LB-KEY INVALID GO Z.
       H.  READ LAGERBZ NEXT AT END MOVE 99 TO WB GO K.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF LB-SYM = 99 GO H.
           IF LG-NUM NOT = LB-KTONR GO K.
           IF WH-PG = 6; IF LB-BUDAT > WV-WEND GO I
               else ADD LB-MENGE TO WV-UMS
                    end-if GO H.
       I.  ADD LB-MENGE TO WM-UMS.
           GO H.
      *----------------------------------------> Jahresabschlu·summen <-
       K.  IF WH-PG NOT = 6 GO Q.
           IF WV-UMS = 0 GO Q.
           PERFORM INIT-BZ.
           MOVE LG-NUM TO LB-KTONR.
           MOVE WV-WEND TO LB-BUDAT LB-BELDAT.
           ADD 10 TO LB-BUDAT.
           MOVE WV-WKZ TO LB-USE.
           ADD 9000000 LB-KTONR GIVING LB-SEITE.
           MOVE 99 TO LB-SYM.
           MOVE WV-UMS TO LB-MENGE.
           MOVE 68 TO WZ-BUCH.                          *> Grî·e def.
           WRITE LB-SATZ INVALID REWRITE LB-SATZ.
           MOVE 0 TO WV-UMS.
       Q.  IF LB-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE LG-SATZ.
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
      *************************************************** Journaldruck *
       JOURDRUCK SECTION.
       A.  CALL "CAUP" USING "0710100550000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WM-UMS WZ-ZEILEN WK-JS wj-zeil WV-SEITE.
           DISPLAY " Journaldruck " with highlight AT VDU-LP.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "ab Journalseite:       bis: " AT VDU-LP.
           CALL "CAUP" USING "1003204004" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO B.
           MOVE WH-NUM TO WD-JONUM WK-SEITE.
           DISPLAY WD-JONUM with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1003314004" WH-CREG.
           IF ESC GO X.
           IF KIST OR TABL
               MOVE WX-TASTE TO WS-TASTE SET RET TO TRUE.
           IF NOT RET GO C.
           MOVE WH-NUM TO WD-JONUM WV-BISJ.
           DISPLAY WD-JONUM with highlight AT VDU-LP.
           IF WV-BISJ < WV-SEITE GO B.
           MOVE "JOURNAL.LST" TO WH-DRUNAM.
           ADD 403 VDU-ECK GIVING VDU-LP.
       D.  MOVE WK-JS TO LB-SEITE.
           MOVE WE-WKZ TO LB-USE.
       F.  START LAGERBZ KEY NOT < LB-JKEY INVALID
               MOVE 0 TO WS-TASTE GO W.
       G.  MOVE SPACE TO LB-TX.
           READ LAGERBZ NEXT AT END move 0 to wv-bisj GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE LB-SEITE TO WK-JS.
           IF WK-SEITE > WV-BISJ GO Q.
           MOVE LB-KTONR TO WD-KZ.
           IF WS-TASTE < 6; IF WD-KZ = 1 GO Q
                            else PERFORM JO-DRU GO G.
           move LB-jkey to wh-bkey.
           move we-use to LB-use.
           add 2 to wj-zeil.
           move wj-zeil to LB-seite.
           display LB-seite at 1020.
           rewrite LB-satz.
           move wh-bkey to LB-jkey.
           if wd-kz = 1 go s.
           go f.
       Q.  MOVE ALL "ƒ" TO DRA-STR(1:140).
           PERFORM DRUCK.
           MOVE "Journalsummen" TO DRA-TX.
           MOVE WM-UMS TO DRA-UMS.
           PERFORM DRUCK.
           MOVE WM-UMS TO WH-WERT.
           IF WH-WERT NOT = 0 MOVE "Differenz" TO DRA-TX
               IF WH-WERT > 0 MOVE WH-WERT TO DRA-MENGE
                         else MOVE WH-WERT TO DRA-UMS
               end-if PERFORM DRUCK.
      *-------------------------------> Ausdruck Bemessungsgrundlagen <-
       S.  IF WD-KZ NOT = 1 GO W.
           IF WS-TASTE < 6 PERFORM JO-DRU GO T.
           move LB-jkey to wh-bkey.
           move we-use to LB-use.
           add 2 to wj-zeil.
           move wj-zeil to LB-seite.
           display LB-seite at 1020.
           rewrite LB-satz.
           move wh-bkey to LB-jkey.
       T.  MOVE SPACE TO LB-TX.
           READ LAGERBZ NEXT AT END move 0 to wv-bisj GO W.
           IF ZUGRIF PERFORM BESETZT GO T.
           MOVE LB-SEITE TO WK-JS.
           IF WK-SEITE > WV-BISJ GO W.
           MOVE LB-KTONR TO WD-KZ.
           GO S.
       W.  IF WS-TASTE = 5; IF WK-SEITE < WV-BISJ;
                IF WK-ZEIL > 01 ADD 1 TO WK-SEITE end-if
                MOVE 0 TO WM-UMS WZ-ZEILEN
                MOVE 0 TO WK-ZEIL GO D.
           PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* Druck Journalzeile *
       JO-DRU SECTION.
       A.  PERFORM JOURKOPF.
           IF WV-DATUM NOT = LB-BUDAT MOVE LB-BUDAT TO WC-DATUM WV-DATUM
               PERFORM DATDREH
               MOVE "Buchungsdatum:" TO DRA-STR(1:)
               MOVE VDU-DATUM TO DRA-STR(16:10)
               PERFORM DRUCK.
           MOVE 0 TO LB-USE.
           DIVIDE 10 INTO LB-KTONR GIVING DRA-KTONR.
           MOVE LB-BELNR TO DRA-BELNR.
           MOVE LB-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE WZ-DATUM TO DRA-BELDAT.
           MOVE WE-SYM(LB-SYM + 1) TO DRA-SYM.
      *    IF LB-Z = 0 GO H.
      *    IF LB-GGKTO > 999997 MOVE "sabu" TO DRA-GGKTO(2:)
      *         MOVE LB-KTONR TO WH-NUM
      *         PERFORM LIESKTO
      *         MOVE KS-BEZ TO DRA-TX
      *    else DIVIDE 10 INTO LB-GGKTO GIVING DRA-GGKTO
      *         MOVE LB-TX TO DRA-TX.
       H.
      *    EVALUATE LB-VM
      *        WHEN 0 CONTINUE
      *        WHEN 1 IF LB-U1 NOT = 0 AND LB-MW NOT = 0
      *                  MOVE WT-UST(LB-U1) TO DRA-UST
      *                  MOVE "m" TO DRA-UST(3:)
      *                  MOVE LB-MW TO DRA-MWSKT
      *        WHEN 2 IF LB-U1 NOT = 0 MOVE WT-UST(LB-U1) TO DRA-UST
      *               end-if IF LB-MW NOT = 0 MOVE "v" TO DRA-UST(3:)
      *                                MOVE LB-MW TO DRA-MWSKT
      *        WHEN 3 IF LB-MW NOT = 0 or LB-B1 NOT = 0 or BZ-B2 NOT = 0
      *                   MOVE "Ske" TO DRA-UST(1:)
      *                   ADD LB-MW LB-B1 LB-B2 GIVING WH-WERT
      *                   ADD WH-WERT TO WM-UMS
      *                   MOVE WH-WERT TO DRA-MWSKT
      *        WHEN 4 IF LB-MW NOT = 0 or LB-B1 NOT = 0 or BZ-B2 NOT = 0
      *                   MOVE "Ska" TO DRA-UST(1:)
      *                   ADD LB-MW LB-B1 LB-B2 GIVING WH-WERT
      *                   MOVE WH-WERT TO DRA-MWSKT.
           MOVE LB-MENGE TO DRA-UMS.
           DISPLAY DRA-STR(1:40) with highlight AT VDU-LP.
           PERFORM DRUCK.
       Z.  EXIT.
      **************************************************** Journalkopf *
       JOURKOPF SECTION.
       A.  IF WK-SEITE NOT = WV-SEITE;
               IF WZ-ZEILEN > 0 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 2 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 5 TO WH-P
                             MOVE 60 TO WH-SIZE.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "J o u r n a l   per:" TO DRA-STR(36:).
           MOVE LB-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(57:10).
           MOVE "Seite:" TO DRA-STR(97:).
           MOVE WK-SEITE TO WD-NUM WV-SEITE.
           MOVE WD-NUM TO DRA-STR(103:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR(1:140).
           PERFORM DRUCK.
           MOVE "Konto-Nr Bel-Nr Bel-Dat. Ref.Nr Sy Geg.Kto Buchungstext
      -       "                             Kz  Ust/Skto(s)  Umsatz Soll
      -       "   Umsatz Haben " TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR(1:140).
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  IF WH-NUM = 0 GO X.
           MOVE WH-NUM TO WD-KZ.
           MOVE WD-KZ TO WH-KAT.
           DIVIDE 10 INTO WH-NUM GIVING WD-KTO.
           MOVE WH-NUM TO LG-NUM.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ LAGARTIK NOT INVALID MOVE LG-BEZ TO WT-TX
               ADD WE-UMS TO WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           IF LG-NUM = 0 GO X ELSE GO C.
       X.  SET FEHLER TO TRUE.
           MOVE "Konto fehlt" TO WT-TX.
       Z.  EXIT.
      ************************************************* Jahresabschlu· *
       JAHRABS SECTION.
       A.  CALL "CAUP" USING "0710100760000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Jahresabschlu· "  with highlight AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY "Neues Wirtschaftsjahr mu· eingetragen sein!"
                with highlight AT VDU-LP.
           MOVE 9 TO WH-KEY.
       C.  READ KONSLAG NO LOCK INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM VARYING WI FROM 4 BY -1 UNTIL WI = 1 OR
               KL-WKZ(WI) NOT = 0 CONTINUE.
           MOVE KL-WKZ(WI) TO WD-PZ.
           ADD 225 VDU-ECK GIVING VDU-LP.
           MOVE KL-WBEG(WI) TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY WD-PZ with highlight AT VDU-LP " / "
                VDU-DATUM with highlight " - ".
           MOVE KL-WEND(WI) TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0000.
           IF WI = 1 DISPLAY "Neues oder altes Wirtschaftsjahr fehlt"
                with BLINK at 2401
                PERFORM WEITER GO X.
           MOVE KL-WBEG(WI - 1) TO WV-WBEG.
           MOVE KL-WEND(WI - 1) TO WV-WEND.
           MOVE KL-WKZ(WI - 1) TO WV-WKZ.
           ADD 50 TO WV-WKZ.
       E.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
           CALL "CAUP" USING "1005331001" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           EVALUATE WH-NUM
              WHEN 0 PERFORM KS-RECH
              WHEN OTHER GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** lîschen LAGARTIKsalden *
       LOESCHEN SECTION.
       A.  CALL "CAUP" USING "0710100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Jahreslîschung " AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lîschung bis: " AT VDU-LP.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 317 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1103176006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WV-DATUM.
       E.  DISPLAY "<Entf>= Start, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023320000" WH-CREG.
           IF ESC GO X.
           IF not ENTF GO E.
           MOVE 0 TO LG-NUM.
           START LAGARTIK KEY NOT < LG-NUM INVALID GO Z.
       G.  READ LAGARTIK NEXT NO LOCK AT END MOVE 0 TO LB-BUDAT GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE LG-NUM TO LB-KTONR.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-NUM AT VDU-LP " " LG-BEZ WITH SIZE 40.
           MOVE 0 TO LB-BUDAT LB-LFD WM-UMS.
           START LAGERBZ KEY NOT < LB-KEY INVALID GO G.
       H.  READ LAGERBZ NEXT AT END MOVE 0 TO LB-BUDAT GO Q.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF LG-NUM NOT = LB-KTONR GO Q.
           IF LB-BUDAT > WV-DATUM GO Q.
           DELETE LAGERBZ.
           GO H.
       Q.  REWRITE LG-SATZ.
           IF LB-BUDAT not = 0 GO G.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************* Sachbuchkonten *
       LAG-KONT SECTION.
       A.  CALL "CAUP" USING "07070805710000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           IF WH-DRUNAM NOT = "LPT1:" MOVE "KOSTKONT.LST" TO WH-DRUNAM.
           DISPLAY " Kostenkontendruck " with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret-leer>= alle, <Kto.Nr + ret>= einzelnes Konto <
      -        "         > " AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "<esc>= Abbruch, <#>= alle mit Saldo" AT VDU-LP.
           MOVE 0 TO WE WH-WERT.
           CALL "CAUP" USING "1003546110" WH-CREG.
           IF ESC GO X.
           IF KIST MOVE 1 TO WE
               SET RET TO TRUE.
           IF NOT RET GO C.
           IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY " alle " with highlight AT VDU-LP GO D.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PAKOANZ" USING "99SUCH" WH-CREG
               CANCEL "PAKOANZ"
               IF WH-NUM = 0 GO C.
       D.  ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WS-SUM WZ-ZEILEN WZ-SEITE WB.
           IF WH-NUM NOT = 0 MOVE WH-NUM TO WV-KTONR
                PERFORM LIESKTO
                IF FEHLER GO C ELSE GO H.
           MOVE 0 TO LG-NUM WV-KTONR.
           START LAGARTIK KEY NOT < LG-NUM INVALID GO Z.
       G.  READ LAGARTIK NEXT NO LOCK AT END MOVE 99 TO WB
               GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
       H.  DIVIDE 10 INTO LG-NUM GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP " " LG-BEZ(1:47).
           IF WE = 1 GO G.
           MOVE LG-NUM TO LB-KTONR.
           MOVE LG-BEZ TO WT-TX.
           MOVE 0 TO LB-LFD WM-ZEIL.
           START LAGERBZ KEY NOT < LB-KEY INVALID GO G.
       I.  MOVE SPACE TO LB-TX.
           READ LAGERBZ NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF LB-BUDAT > WM-WJAHR(2) GO Q.
           IF LB-KTONR NOT = LG-NUM GO Q.
           PERFORM KTO-ZEIL.
           GO I.
       Q.  PERFORM KTO-ENDE.
           IF WV-KTONR NOT = 0 GO W.
           IF WB NOT = 99 GO G.
       W.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************************** Kontozeile *
       KTO-ZEIL SECTION.
       A.  PERFORM KTO-KOPF.
           MOVE 1 TO WM-ZEIL.
           MOVE LB-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRK-BUDAT.
           MOVE LB-BELNR TO DRK-BELNR.
           MOVE LB-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRK-BELDAT.
           EVALUATE LB-SYM
               WHEN 99    MOVE SPACE TO DRA-SATZ
                          GO Z
               WHEN 98    MOVE "SV" TO DRK-SYM
                          MOVE "Saldovortrag" TO DRK-BEZ
               WHEN 97    MOVE "SV" TO DRK-SYM
                          MOVE "SaldoÅbernahme" TO DRK-BEZ
               WHEN OTHER MOVE WE-SYM(LB-SYM + 1) TO DRK-SYM.
           DIVIDE 100 INTO LB-SEITE GIVING DRK-JS.
           MOVE LB-TX TO DRK-BEZ.
           MOVE LB-MENGE TO DRK-SOLL.
           ADD LB-MENGE TO WM-UMS.
           PERFORM DRUCK.
       Z. EXIT.
      ****************************************************** Kontoende *
       KTO-ENDE SECTION.
       A.  IF WM-ZEIL = 0 GO Z.
           MOVE ALL "ƒ" TO DRK-STR(1:) PERFORM DRUCK.
           PERFORM DRUCK.
       W.  MOVE 0 TO WM-UMS WZ-SEITE.
           MOVE 98 TO WZ-ZEILEN.
       Z.  EXIT.
      ***************************************************** Kontenkopf *
       KTO-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE;
               IF WM-UMS NOT = 0
                   MOVE ALL "ƒƒƒ" TO DRK-STR(1:) PERFORM DRUCK
                   PERFORM UEBERTRAG
               end-if
               WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-SEITE = 0 GO C.
           IF WZ-ZEILEN > 0 GO Z.
       C.  MOVE 2 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 2 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 5 TO WH-P
                             MOVE 60 TO WH-SIZE.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WZ-ZEILEN > 0; IF WL = 1 MOVE 2 TO WZ-SCHALT.
           MOVE "Kto.:" TO DRK-STR(1:5).
           MOVE WD-KTO TO DRK-STR(7:8).
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           MOVE WT-TX TO DRK-STR(15:).
           MOVE SPACE TO DRK-STR(89:1).
           MOVE WK-FIRMA TO DRK-STR(86:24).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRK-STR(111:10).
           MOVE "Seite:" TO DRK-STR(121:).
           MOVE WZ-SEITE TO WD-NUM.
           MOVE WD-NUM(4:) TO DRK-STR(128:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRK-STR(1:) PERFORM DRUCK.
           MOVE " Buch.Dat Bel-Num. Bel.Dat Sym Gegkto.    JS T  e  x  t
      -       "                                Steuer/Skto.  kz.
      -       " SOLL         HABEN" TO DRK-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRK-STR(1:) PERFORM DRUCK.
           IF WM-UMS NOT = 0 PERFORM UEBERTRAG.
       Z.  EXIT.
      ******************************************************* öbertrag *
       UEBERTRAG SECTION.
       A.  MOVE "öbertrag:" TO DRK-BEZ(27:).
           MOVE WM-UMS TO DRK-SOLL.
           PERFORM DRUCK.
       Z.  EXIT.
      *****************************************************************
       WD-MG SECTION.
       A.  EVALUATE WH-NK
               WHEN 2 MOVE WH-MENGE TO WD-MENGA
                      MOVE WD-MENGA(1:) TO WD-MENGE
               WHEN 1 MOVE WH-MENGE TO WD-MENGB
                      MOVE WD-MENGB(1:) TO WD-MENGE
               WHEN 0 MOVE WH-MENGE TO WD-MENGC
                      MOVE WD-MENGC(1:) TO WD-MENGE.
       Z.  EXIT.
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION.
       A.  ADD 315 VDU-ECK GIVING VDU-LP.
           MOVE KR-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
      *    PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 5
      *        DISPLAY WR-ADR(WR) with SIZE 34 highlight AT VDU-LP
      *        ADD 100 TO VDU-LP.
       Z.  EXIT.
      *************************************************** Rezeptbilanz *
      *REZ-BIL SECTION.
      *A.  CALL "CAUP" USING "0707101060000" WH-CREG.
      *    ADD 103 VDU-ECK GIVING VDU-LP.
      *    DISPLAY " öbernahme u. Rezeptbilanz " with highlight
      *        AT VDU-LP.
      *C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
      *    ADD 304 VDU-ECK GIVING VDU-LP.
      *    DISPLAY "1 - öbernahme Wiegedaten" AT VDU-LP.
      *    ADD 200 TO VDU-LP.
      *    DISPLAY "2 - Druck Rezeptbilanz" AT VDU-LP.
      *    ADD 200 TO VDU-LP.
      *    DISPLAY "<esc>= Abbruch, <ret>= Auswahl < >" AT VDU-LP.
      *    CALL "CAUP" USING "1007361001" WH-CREG.
      *    IF ESC OR WH-NUM = 0 GO X.
      *    IF NOT RET GO C.
      *    IF WH-NUM = 1 PERFORM WAAGUEB GO C.
      *------------------------------> Rezeptbilanz / Fehlerprotokoll <-
      *X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
      *Z.  EXIT.
      ******************************************* öbernahme Wiegedaten *
      *WAAGUEB SECTION.
      *A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
      *    ADD 303 VDU-ECK GIVING VDU-LP.
      *    DISPLAY "öbernahme Wiegedaten bis: " AT VDU-LP.
      *    MOVE WM-DATUM TO WC-DATUM.
      *    PERFORM DATDREH.
      *    ADD 330 VDU-ECK GIVING VDU-LP.
      *    DISPLAY VDU-DATUM with highlight AT VDU-LP.
      *    MOVE WM-DATUM TO WZ-DATUM WH-WERT.
      *    CALL "CAUP" USING "1103306006" WH-CREG.
      *    IF ESC GO Z.
      *    IF NOT RET GO A.
      *    IF WZ-DATUM = 0 GO A.
      *    MOVE WZ-DATUM TO WV-DATUM.
      *    DISPLAY VDU-DATUM with highlight AT VDU-LP.
      *    MOVE 0 TO WZ-ZEILEN WZ-SEITE WK-SEITE.
      *    MOVE "WAAGFEHL.LST" TO WH-DRUNAM.
      *    ADD 240 VDU-ECK GIVING VDU-LP.
      *    DISPLAY WH-DRUNAM with highlight AT VDU-LP.
      *    OPEN I-O PROTWAAG.
      *    MOVE LOW-VALUES TO PWA-AKEY.
      *    START PROTWAAG KEY NOT < PWA-AKEY INVALID GO X.
      *E.  READ PROTWAAG NEXT AT END GO X.
      *    IF ZUGRIF PERFORM BESETZT GO E.
      *    IF PWA-DAT > WV-DATUM OR PWA-SA > 9 GO Q.
      *    MOVE 1 TO WX.
      *----------------------------------> Lagerbuchungen durchfÅhren <-
      *G.  IF PWA-LGNUM(WX) = 0 GO O.
      *    IF PWA-IST(WX) = 0 PERFORM FEHLPROT GO O.
      *    MOVE PWA-LGNUM(WX) TO LG-NUM.
      *I.  READ LAGARTIK INVALID DISPLAY LG-NUM AT 2401
      *        " Artikel bitte anlegen"
      *        PERFORM WEITER GO G.
      *    IF ZUGRIF PERFORM BESETZT GO G.
      *    MOVE LG-NUM TO LB-KTONR.
      *    MOVE PWA-DAT TO LB-BUDAT.
      *    MOVE 1 TO LB-LFD.
      *K.  READ LAGERBZ INVALID PERFORM INIT-BZ.
      *    IF ZUGRIF PERFORM BESETZT GO K.
      *    MOVE WE-USE TO LB-USE.
      *    MOVE PWA-DAT TO LB-BELNR LB-BELDAT.
      *    MOVE 3 TO LB-SYM.
      *    MOVE LG-MEH TO LB-MEH.
      *    MOVE LG-NK TO LB-NK.
      *    MOVE LG-PREIS TO LB-PREIS.
      *    DIVIDE 1000 INTO PWA-IST(WX) GIVING WH-WERT.
      *    MOVE WH-WERT TO LB-MENGE.
      *    SUBTRACT WH-WERT FROM LG-STAND.
      *    REWRITE LG-SATZ.
      *    PERFORM LB-WRITE.
      *    PERFORM FEHLPROT.
      *O.  IF WX < 20 ADD 1 TO WX GO G.
      *    MOVE 10 TO PWA-SA.
      *    REWRITE PWA-SATZ.
      *    GO E.
      *Q.  PERFORM END-DRU.
      *X.  CLOSE PROTWAAG.
      *Z.  EXIT.
      **************************************** Buchungszeile speichern *
      *LB-WRITE SECTION.
      *B.  MOVE WE-USE TO LB-USE.
      *    ADD 1 TO LB-SEITE.
      *    MOVE LB-SATZ TO WH-BZSATZ.
      *    MOVE LB-KEY TO WH-BKEY.
      *    MOVE 253 TO LB-LFD.
      *    START LAGERBZ KEY < LB-KEY INVALID MOVE 1 TO WH-LFD GO E.
      *C.  READ LAGERBZ PREVIOUS WITH NO LOCK AT END
      *        MOVE 1 TO WH-LFD GO E.
      *    IF ZUGRIF PERFORM BESETZT GO C.
      *    IF LB-KTONR NOT = WH-KTONR OR LB-BUDAT NOT = WH-BUDAT
      *        MOVE 1 TO WH-LFD GO E.
      *    IF LB-SYM NOT = 3 GO E.
      *    ADD WH-WERT TO LB-MENGE.
      *    REWRITE LB-SATZ.
      *    GO Z.
      *E.  MOVE WH-BZSATZ TO LB-SATZ.
      *    MOVE WH-BKEY TO LB-KEY.
      *    WRITE LB-SATZ INVALID GO B.
      *    IF KL-JOKEY = LOW-VALUE MOVE LB-JKEY TO KL-JOKEY.
      *Z.  EXIT.
      ************************************************ Fehlerprotokoll *
      *FEHLPROT SECTION.
      *A.  IF PWA-IST(WX) = 0 GO C.
      *    SUBTRACT PWA-IST(WX) FROM PWA-SOLL(WX) GIVING WH-WERT.
      *    MULTIPLY 100 BY WH-WERT.
      *    DIVIDE PWA-SOLL(WX) INTO WH-WERT GIVING WV-WTOL.
      *    IF WV-WTOL < WE-WTOL GO Z.
      *C.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
      *        MOVE 0 TO WZ-ZEILEN.
      *    IF WZ-ZEILEN > 0 GO G.
      *    MOVE 2 TO WH-P1.
      *    IF WM-KO = 0 MOVE 5 TO WH-P2
      *                 MOVE 39 TO WH-SIZE
      *            ELSE MOVE 4 TO WH-P2
      *                 MOVE 63 TO WH-SIZE.
      *    PERFORM BEG-DRU.
      *    MOVE 1 TO WZ-SCHALT.
      *    MOVE WK-FIRMA TO DRF-STR(1:30).
      *    MOVE "Fehlerprotokoll per:" TO DRF-STR(36:).
      *    MOVE WV-DATUM TO WC-DATUM.
      *    PERFORM DATDREH.
      *    MOVE VDU-DATUM TO DRF-STR(57:10).
      *    MOVE "Seite:" TO DRF-STR(70:).
      *    ADD 1 TO WK-SEITE.
      *    MOVE WK-SEITE TO WD-NUM WV-SEITE.
      *    MOVE WD-NUM TO DRF-STR(76:).
      *    PERFORM DRUCK.
      *    MOVE ALL "ƒƒƒ" TO DRF-STR PERFORM DRUCK.
      *    MOVE "Art-Nr    Datum Rzp-Nr Rez.Bezeichnung Art.Bezeichnung
      *       "          Soll  -Gew-  Ist " TO DRF-STR(1:).
      *    PERFORM DRUCK.
      *    MOVE ALL "ƒƒƒ" TO DRF-STR PERFORM DRUCK.
      *G.  MOVE LB-KTONR TO DRF-ARTNR .
      *    MOVE LB-BUDAT TO WC-DATUM.
      *    PERFORM DATDREH.
      *    MOVE VDU-DATUM TO DRF-DATUM.
      *    MOVE PWA-RENUM TO DRF-RPNR.
      *    MOVE PWA-RPBEZ TO DRF-RPBEZ.
      *    MOVE LG-BEZ TO DRF-ARBEZ.
      *    MOVE PWA-SOLL(WX) TO DRF-SOLL.
      *    MOVE PWA-IST(WX) TO DRF-IST.
      *    PERFORM DRUCK.
      *Z.  EXIT.
      *********************************************** Bestellvorschlag *
       BESTVOR SECTION.
       A.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           EVALUATE WH-PG
               WHEN 1 MOVE "     Bestellung" TO WK-GEB
               WHEN 2 MOVE "  Bestellvorschlag" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "0707100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           EVALUATE WH-PG
               WHEN 1 MOVE "BESTELL.LST" TO WH-DRUNAM
                      DISPLAY " Bestellung " with highlight AT VDU-LP
               WHEN 2 MOVE "BESTVOR.LST" TO WH-DRUNAM
                      DISPLAY " Bestellvorschlag " with highlight AT
                          VDU-LP.
           MOVE 0 TO WZ-ZEILEN WK-KO WV-KTONR.
      *-----------------------------------------------> von Lieferant <-
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "von Lft.:           bis Lft.:" AT VDU-LP.
           MOVE SPACE TO WT-TX.
           DISPLAY "<esc>= Abbruch, <ret-leer>= ab Anfang" AT 2301.
           CALL "CAUP" USING "1203130105" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           CALL "CAUP" USING "20CONV05" WH-CREG.
           MOVE WT-TX TO KR-MCODE WV-MCODE.
           CALL "CAUP" USING "21CONV05" WH-CREG.
           IF WT-TX = SPACE
                DISPLAY "ab Anfang" with highlight AT VDU-LP GO F.
           DISPLAY WT-TX(1:5) with highlight AT VDU-LP.
           START KREDIT KEY NOT < KR-MCODE INVALID PERFORM NO-REC GO C.
       E.  READ KREDIT NEXT NO LOCK AT END PERFORM NO-REC GO C.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE KR-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           DISPLAY WT-TX(1:55) with highlight AT VDU-LP.
           MOVE KR-MCODE TO WT-TX WV-ABCODE.
           CALL "CAUP" USING "21CONV05" WH-CREG.
           ADD 313 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-TX(1:5) with highlight AT VDU-LP.
      *-----------------------------------------------> bis Lieferant <-
       F.  DISPLAY "<esc>= Abbruch, <ret-leer>= bis Ende" AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1203330105" WH-CREG.
           IF ESC GO X.
           IF WOLI GO C.
           IF NOT RET GO F.
           CALL "CAUP" USING "20CONV05" WH-CREG.
           MOVE WT-TX TO KR-MCODE WV-BISCODE.
           CALL "CAUP" USING "21CONV05" WH-CREG.
           IF WT-TX = SPACE
                DISPLAY "bis Ende" with highlight AT VDU-LP
                MOVE "zzzzz" TO WV-BISCODE
           else DISPLAY WT-TX(1:5) with highlight AT VDU-LP.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO G.
           MOVE SPACE TO WV-ALT.
           IF WV-KTONR NOT = 0 GO J.
           MOVE WV-BISCODE TO WT-TX.
           MOVE SPACE TO WV-ALT.
           CALL "CAUP" USING "20CONV05" WH-CREG.
           MOVE WT-TX TO WV-BISCODE.
           IF WH-PG = 1 CALL "CAUP" USING "0703101360020" WH-CREG
           else ADD 403 VDU-ECK GIVING VDU-LP
                DISPLAY "-- Bitte warten --" with highlight AT VDU-LP
                CALL "CAUP" USING "0712100660020" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " zu bestellende Artikel " with highlight AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY "  Art.  Bezeichnung                     Bestellmenge
      -        "      " with reverse-video AT VDU-LP.
           MOVE WV-ABCODE TO KR-MCODE.
       H.  START KREDIT KEY NOT < KR-MCODE INVALID GO W.
       I.  READ KREDIT NEXT NO LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF KR-MCODE = SPACE GO I.
           IF KR-MCODE > WV-BISCODE GO W.
           IF WV-KTONR NOT = 0; IF WV-KTONR NOT = KR-KTONR GO W.
           IF WM-OPEN = 0 MOVE 0 TO WZ-ZEILEN.
       J.  ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE KR-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           CALL "CAUP" USING "16CLOFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-TX(1:55) with highlight foreground-color 3
               AT VDU-LP.
           MOVE 1 TO WI IX.
           MOVE 0 TO WH-MOD.
           MOVE LOW-VALUE TO WT-GRP.
      *-------------------------------> ob Stand kleiner Mindeststand <-
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV05" WH-CREG.
           MOVE WT-TX TO LG-LFT.
           MOVE 0 TO LG-NUM WH-MENGE.
           START LAGARTIK KEY NOT < LG-CKEY INVALID GO I.
       O.  READ LAGARTIK NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO O.
           IF LG-LFT NOT = WT-TX(1:5);
               IF WH-MOD = 0 GO Q
               else IF WH-PG = 1 MOVE 2 TO WH-MOD
                        PERFORM ZUSART
                        IF WH-NUM = 0 GO Q else GO P.
           IF LG-STAND = 0 and LG-STAND = 0 and LG-BMENGE = 0 GO O.
           IF LG-MIND < LG-STAND GO O.
           IF WH-PG = 1 MOVE 1 TO WH-MOD.
       P.  IF WH-PG = 1 PERFORM OB-BESTELL.
           IF ESC GO W.
           IF ENDE GO Q.
           IF APOS GO H.
           IF BAUF MOVE WV-ALT TO KR-MCODE GO H.
           IF NOT RET GO O.
           ADD 1 TO WI.
           PERFORM BESTKOPF.
           MOVE KR-MCODE TO WV-ALT.
           MOVE LG-NUM TO DRB-KONUM.
           MOVE LG-BEZ TO DRB-BEZ.
           MOVE LG-NK TO WH-NK.
           MOVE LG-MEH TO WH-MEH.
           IF WH-MENGE not = 0 MOVE WH-MENGE TO LG-BMENGE.
           IF WH-PG = 2 MOVE LG-STAND TO WH-MENGE
               PERFORM WD-MG
               MOVE WD-MENGE TO DRB-STAND
               MOVE LG-MIND TO WH-MENGE
               PERFORM WD-MG
               MOVE WD-MENGE TO DRB-MIND.
           MOVE LG-BNK TO WH-NK.
           MOVE LG-BMEH TO WH-MEH.
           MOVE LG-BMENGE TO WH-MENGE.
           PERFORM WD-MG.
           IF WH-PG = 2 MOVE WD-MENGE TO DRB-BEST
               MOVE WT-MEH(WH-MEH + 1) TO DRB-MEH.
           IF WH-PG = 1 MOVE WD-MENGE TO DRD-BEST
               MOVE WT-MEH(WH-MEH + 1) TO DRD-MEH.
           PERFORM DRUCK.
           GO O.
       Q.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 99
               IF WS-NUM(WX) not = 0 and WS-GRP(wx) not = 0
                   PERFORM BEST-DRU.
           IF WM-OPEN = 0 and WK-KO = 0 GO R.
           MOVE 20 TO WH-KEY.
           IF WZ-ZEILEN > (WH-SIZE + 3) MOVE 80 TO WZ-ZEILEN
               PERFORM BESTKOPF.
           MOVE 2 TO WZ-SCHALT.
           READ KONSLAG IGNORE LOCK INVALID GO R.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 3
               IF KL-BEST(WX) not = SPACE MOVE KL-BEST(WX) TO DRD-STR
                   PERFORM DRUCK.
       R.  IF WM-OPEN NOT = 0 and WK-KO NOT = 0
               IF WH-PG = 1 MOVE ALL "ƒ" TO DRD-STR
                   PERFORM DRUCK
                   MOVE WH-DATUM TO WC-DATUM
                   PERFORM DATDREH
                   MOVE VDU-DATUM TO DRD-STR
                   PERFORM DRUCK
                   COMPUTE WZ-SCHALT = WH-SIZE - 2 - WZ-ZEILEN
                   MOVE "Tel: " TO DRB-STR
                   MOVE KR-TELE TO DRB-STR(6:)
                   MOVE "Fax: " TO DRB-STR(31:)
                   MOVE KR-FAX TO DRB-STR(37:)
                   PERFORM DRUCK
                   PERFORM END-DRU
               else MOVE ALL "ƒ" TO DRB-STR
                   PERFORM DRUCK
                   MOVE 4 TO WZ-SCHALT.
           MOVE 0 TO WK-KO.
           GO I.
       W.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       X.  PERFORM END-DRU
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CANCEL "PARKOMKA".
       Z.  EXIT.
      ************************************** Druck fertiger Bestellung *
       BEST-DRU SECTION.
       A.  PERFORM BESTKOPF.
           MOVE KR-MCODE TO WV-ALT.
           MOVE WS-NUM(WX) TO LG-NUM.
       C.  READ LAGARTIK NO LOCK INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE LG-NUM TO DRD-KONUM.
           MOVE LG-BEZ TO DRD-BEZ.
           IF LG-MEH not = LG-BMEH and LG-ANZ not = 0
               MOVE LG-NK TO WH-NK
               MOVE LG-MEH TO WH-MEH
               MOVE LG-ANZ TO WH-MENGE
               PERFORM WD-MG
               IF WH-PG = 1 MOVE WD-MENGE TO DRD-INHALT
                   MOVE WT-MEH(WH-MEH + 1) TO DRD-IMEH.
           MOVE WS-GRP(WX) TO WH-MENGE LG-BMENGE.
           MOVE LG-BNK TO WH-NK.
           MOVE LG-BMEH TO WH-MEH.
           PERFORM WD-MG.
           IF WH-PG = 1 MOVE WD-MENGE TO DRD-BEST
               MOVE WT-MEH(WH-MEH + 1) TO DRD-MEH.
           PERFORM DRUCK.
       Z.  EXIT.
      ****************************************** Korrekt. Bestellmenge *
       OB-BESTELL SECTION.
       A.  IF WI > 9 CALL "CAUP" USING "17SCROLL" WH-CREG
               MOVE 9 TO WI.
           IF WV-ALT = SPACE MOVE KR-MCODE TO WV-ALT.
           COMPUTE VDU-LP = WI * 100 + 303 + VDU-ECK.
           MOVE LG-NUM TO WD-JONUM.
           DISPLAY WD-JONUM AT VDU-LP " " LG-BEZ with highlight.
           COMPUTE VDU-LP = WI * 100 + 340 + VDU-ECK.
           DISPLAY "          " AT VDU-LP.
           IF WS-GRP(IX) = 0 and WS-NUM(IX) = 0
               MOVE LG-BMENGE TO WH-WERT WH-MENGE
           else MOVE WS-GRP(IX) TO WH-WERT WH-MENGE.
           MOVE LG-BNK TO WH-NK.
           MOVE LG-BMEH TO WH-MEH.
           PERFORM WD-MG.
           IF WH-MENGE = 0 CALL "CAUP" USING "1300000059" WH-CREG
                           DISPLAY WD-MENGE AT VDU-LP
                      else DISPLAY WD-MENGE with highlight AT VDU-LP.
           ADD 11 TO VDU-LP.
           DISPLAY WT-MEH(WH-MEH + 1) with highlight
                foreground-color 5 AT VDU-LP.
       F.  DISPLAY "<esc>= Abbruch, <ret>= Menge, <Entf/0+<ret>= keine M
      -        "enge." AT 2301.
           IF WH-MOD = 2 DISPLAY ", <>= zurÅck" AT 0000.
           DISPLAY "Lft: <Pos 1>= Wiederholung, <Ende>= nÑchster, <Bild-
      -        " >= zuletzt bearbeitet" AT 2401.
           DISPLAY "<#>= anzeigen Artikelstamm, <Bild- >= Ende und man.
      -        "Artikel" AT 2501.
           COMPUTE VDU-LP = WI * 100 + 340.
           CALL "CAUP" USING "1000405208" WH-CREG.
       H.  EVALUATE TRUE
               WHEN KIST CALL "PARKOMKA" USING "40ZEIG" WH-CREG
               WHEN AUF PERFORM PREV-BEST
                        IF LG-NUM = 0 GO F else GO A
               WHEN AB  PERFORM NEXT-BEST
                        IF LG-NUM not = 0 GO A
                        else GO Q
               WHEN BAB; IF WH-MOD = 1 GO Q
                         else GO F
               WHEN WOLI
               WHEN BAUF
               WHEN ENDE
               WHEN ESC
               WHEN APOS GO Z.
           IF NOT RET and NOT ENTF GO F.
           IF ENTF MOVE 0 TO WH-WERT.
           IF WH-WERT = 0 SET ENTF TO TRUE.
           MOVE WH-WERT TO WH-MENGE.
           PERFORM WD-MG.
           COMPUTE VDU-LP = WI * 100 + 340 + VDU-ECK.
           IF WH-MENGE = 0 CALL "CAUP" USING "1300000059" WH-CREG
                           DISPLAY WD-MENGE AT VDU-LP
                      else DISPLAY WD-MENGE with highlight AT VDU-LP.
           MOVE LG-NUM TO WS-NUM(IX).
           MOVE WH-MENGE TO WS-GRP(IX).
           PERFORM NEXT-BEST.
           IF LG-NUM not = 0 GO A.
      *-----------------------------------> zusÑtzliche Artikelzeilen <-
       Q.  MOVE 2 TO WH-MOD.
           PERFORM ZUSART.
           EVALUATE TRUE
              WHEN AUF
              WHEN AB MOVE 1 TO WH-MOD GO H
              WHEN RET; IF LG-NUM not = 0 GO A
                        else GO H.
           GO H.
       Z.  EXIT.
      **************************************** alte Bestellzeile lesen *
       PREV-BEST SECTION.
       A.  IF WS-GRP(IX) = 0
              COMPUTE VDU-LP = WI * 100 + 302 + VDU-ECK
              IF IX = 1 and WS-GRP(2) not = 0
                        CALL "CAUP" USING "17SCROL" WH-CREG
                        PERFORM VARYING WX FROM 2 BY 1 UNTIL WX > 99
                            MOVE WS-NUM(WX) TO WS-NUM(WX - 1)
                            MOVE WS-GRP(WX) TO WS-GRP(WX - 1)
                        end-perform
                   else IF WS-NUM(IX) = 0
                        DISPLAY SPACE with SIZE 58 AT VDU-LP.
           IF IX = 1; IF WS-NUM(IX) = 0 GO G
                      else MOVE WS-NUM(IX) TO LG-NUM GO G.
           ADD -1 TO IX.
           MOVE WS-NUM(IX) TO LG-NUM.
       C.  READ LAGARTIK NO LOCK INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO Z.
           ADD -1 TO WI.
           IF WI = 0 CALL "CAUP" USING "22SCROLAB" WH-CREG
                     MOVE 1 TO WI.
           GO Z.
      *-------------------------------------------------> zurÅcklesen <-
       G.  MOVE WT-TX TO LG-LFT.
           START LAGARTIK KEY < LG-CKEY INVALID MOVE 0 TO LG-NUM GO Z.
       I.  READ LAGARTIK PREVIOUS AT END MOVE 0 TO LG-NUM GO Z.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF LG-LFT NOT = WT-TX(1:5) MOVE 0 TO LG-NUM GO Z.
           IF LG-STAND = 0 and LG-STAND = 0 and LG-BMENGE = 0 GO I.
           IF LG-MIND < LG-STAND GO I.
           IF IX = 1 CALL "CAUP" USING "22SCROLL" WH-CREG
               PERFORM VARYING WX FROM 98 BY -1 UNTIL WX = 0
                   MOVE WS-NUM(WX) TO WS-NUM(WX + 1)
                   MOVE WS-GRP(WX) TO WS-GRP(WX + 1)
               end-perform MOVE 0 TO WS-GRP(1) WS-NUM(1).
       Z.  EXIT.
      ************************************* nÑchste Bestellzeile lesen *
       NEXT-BEST SECTION.
       A.  IF WS-GRP(IX) = 0
              COMPUTE VDU-LP = WI * 100 + 302 + VDU-ECK
              IF IX = 1 and WS-GRP(2) not = 0
                  CALL "CAUP" USING "17SCROL" WH-CREG
                  PERFORM VARYING WX FROM 2 BY 1 UNTIL WX > 99
                      MOVE WS-NUM(WX) TO WS-NUM(WX - 1)
                      MOVE WS-GRP(WX) TO WS-GRP(WX - 1)
                  end-perform GO B
                  else IF WS-NUM(IX) = 0
                           DISPLAY SPACE with SIZE 58 AT VDU-LP.
           IF WS-NUM(IX) = 0; IF IX = 1 GO I else GO G.
           ADD 1 TO IX.
           ADD 1 TO WI.
       B.  MOVE WS-NUM(IX) TO LG-NUM.
           IF LG-NUM = 0; IF IX = 1 GO G
                          else MOVE WS-NUM(IX - 1) TO LG-NUM GO G.
       C.  READ LAGARTIK NO LOCK INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO Z.
           GO Z.
      *----------------------------------------------> nÑchsten lesen <-
       G.  IF WH-MOD = 2 MOVE 0 TO LG-NUM GO Z.
           MOVE WT-TX TO LG-LFT.
           START LAGARTIK KEY > LG-CKEY INVALID MOVE 0 TO LG-NUM GO Z.
       I.  READ LAGARTIK NEXT AT END MOVE 0 TO LG-NUM GO Z.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF LG-LFT NOT = WT-TX(1:5) MOVE 0 TO LG-NUM GO Z.
           IF LG-STAND = 0 and LG-STAND = 0 and LG-BMENGE = 0 GO I.
           IF LG-MIND < LG-STAND GO I.
       Z.  EXIT.
      *************************************** man. angewÑhlter Artikel *
       ZUSART SECTION.
       A.  IF WI > 9 CALL "CAUP" USING "17SCROLL" WH-CREG
               MOVE 9 TO WI.
           COMPUTE VDU-LP = WI * 100 + 303 + VDU-ECK.
           DISPLAY ALL " " with SIZE 57 AT VDU-LP.
       E.  COMPUTE VDU-LP = WI * 100 + 303.
           DISPLAY "<ret>= zus. Artikel, <ret-leer/Ende>= Ende Lieferant
      -        "" AT 2301.
           CALL "CAUP" USING "1000034004" WH-CREG.
           IF RET and WH-NUM = 0 and WH-MCODE = SPACE SET ENDE TO TRUE.
           IF ENDE GO Z.
           IF ESC or AUF or AB GO Z.
           IF NOT RET GO E.
           IF WH-NUM = 0 AND WH-MCODE not = SPACE
               CALL "PANANZ" USING "30SUCH" WH-CREG
               CANCEL "PANANZ"
               IF WH-NUM = 0 GO E.
           MOVE WH-NUM TO LG-NUM WD-JONUM.
           DISPLAY WD-JONUM with highlight foreground-color 2 AT VDU-LP.
       G.  MOVE WH-NUM TO LG-NUM.
           READ LAGARTIK NO LOCK INVALID PERFORM NO-REC GO E.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO E.
       Z.  EXIT.
      ****************************************** Kopf Bestellvorschlag *
       BESTKOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO C.
           MOVE 1 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 1 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 4 TO WH-P
                             MOVE 60 TO WH-SIZE.
           PERFORM BEG-DRU.
           IF WH-PG = 2 MOVE 2 TO WZ-SCHALT
               MOVE "Bestellvorschlagliste per: " TO DRB-STR
               MOVE WM-DATUM TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRB-STR(28:8)
               PERFORM DRUCK
               MOVE ALL "ƒ" TO DRB-STR
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
           else PERFORM FA-KOPF.
       C.  IF WK-KO = 0 PERFORM HOLADR
               PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 5
                   MOVE WR-ADR(WR) TO DRB-STR
                   PERFORM DRUCK
               end-perform
               MOVE 1 TO WK-KO
               MOVE 4 TO WZ-SCHALT
               IF WH-PG = 2
                   MOVE "  L-Stand   Mindest   B-Menge" TO DRB-STR(36:)
                   PERFORM DRUCK
               else MOVE "B e s t e l l u n g " TO DRD-STR
                   MOVE "Inhalt" TO DRD-STR(38:)
                   MOVE "Bestellmenge" TO DRD-STR(51:)
                   PERFORM DRUCK
                   MOVE ALL "ƒ" TO DRD-STR
                   PERFORM DRUCK.
       Z.  EXIT.
      ****************************************** Kopf bei Nadeldrucker *
       NADEL-KOPF SECTION.
       A.  MOVE X"1B21" TO DRA-SATZ(1:).
           EVALUATE WH-KEY
               WHEN 2 MOVE X"3B00" TO DRA-SATZ(3:2)
               WHEN 3 MOVE X"3100" TO DRA-SATZ(3:2)
               WHEN 4 MOVE X"3100" TO DRA-SATZ(3:2)
               WHEN 5 MOVE X"0100" TO DRA-SATZ(3:2).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           IF WH-KEY = 5 MOVE KL-KOPF TO DRB-SATZ(6:)
                    else MOVE KL-KOPF TO DRB-SATZ(4:)
                    IF WH-KEY = 2 PERFORM OB-TEILG.
       Z.  EXIT.
      ************************************************ ob Druckteilung *
       OB-TEILG SECTION.
       A.  PERFORM VARYING WS FROM 1 BY 1 UNTIL WS > 80
              IF DRA-SATZ(WS:1) = "*" MOVE DRA-SATZ(WS:) TO WT-TX
                  IF WM-DRU = 1 MOVE "(s25v0s3b4101T" TO DRA-SATZ(WS:)
                                ADD 15 TO WS
                  end-if MOVE WT-TX(2:) TO DRA-SATZ(WS:)
                         MOVE 84 TO WS.
       Z.  EXIT.
      ****************************************** Kopf bei Laserdrucker *
       LASER-KOPF SECTION.
       A.  MOVE 0 TO WZ-SCHALT.
           EVALUATE WH-KEY
               WHEN 2 MOVE "  (s1p18v4s3b4101T" TO DRA-SATZ
                      PERFORM DRUCK
                      MOVE KL-KOPF TO DRB-SATZ(9:)
                      PERFORM OB-TEILG
                      MOVE 0 TO WZ-SCHALT
               WHEN 3 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KL-KOPF TO DRB-SATZ(9:)
               WHEN 4 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KL-KOPF TO DRB-SATZ(9:)
               WHEN 5 MOVE "  (s0p0s0b0T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KL-KOPF TO DRB-SATZ(14:).
       Z.  EXIT.
      ***************************************************** Firmenkopf *
       FA-KOPF SECTION.
       A.  MOVE 1 TO WZ-SCHALT.
           MOVE 2 TO WH-KEY.
       B.  READ KONSLAG IGNORE LOCK.
           EVALUATE WM-DRU
               WHEN 0 PERFORM NADEL-KOPF
               WHEN 1 PERFORM LASER-KOPF.
           PERFORM DRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO B.
           MOVE ALL "ƒ" TO DRB-STR.
       C.  PERFORM DRUCK.
           IF WM-DRU = 1 MOVE "(s0p10h3b0s6T" TO DRA-SATZ.
           MOVE 5 TO WZ-SCHALT.
           PERFORM DRUCK.
       Z.  EXIT.
      ****************************************************** Lagerlist *
       LAGLIST SECTION.
       A.  MOVE 0 TO WZ-ZEILEN WZ-SEITE.
           SUBTRACT 2 FROM WH-PG.
           MOVE WH-PG TO WH-MOD.
           MOVE 0 TO WS-SUM WM-UMS WG-UMS WZ-UMS.
           CALL "CAUP" USING "0707100645000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Listungen " with highlight AT VDU-LP.
           ADD 125 VDU-ECK GIVING VDU-LP.
           EVALUATE WH-MOD
               WHEN 1 DISPLAY " Lieferanten  " AT VDU-LP
                      MOVE SPACE TO LG-LFT WV-LFT
                      MOVE 0 TO LG-NUM
                      START LAGARTIK KEY NOT < LG-CKEY INVALID GO C
                      end-start
               WHEN 2 DISPLAY " Inv.Aufnahme " AT VDU-LP
                      MOVE SPACE TO LG-LAGER WV-LAGER
                      MOVE 0 TO LG-SORT
                      START LAGARTIK KEY NOT < LG-BKEY INVALID GO C
                      end-start
               WHEN 3 DISPLAY " Inv.Istliste " AT VDU-LP
                      MOVE SPACE TO LG-LAGER WV-LAGER
                      MOVE 0 TO LG-SORT
                      START LAGARTIK KEY NOT < LG-BKEY INVALID GO C
                      end-start
               WHEN 7 DISPLAY " Soll-Ist Liste " AT VDU-LP
                      MOVE SPACE TO LG-LAGER WV-LAGER
                      MOVE 0 TO LG-SORT
                      START LAGARTIK KEY NOT < LG-BKEY INVALID GO C
                      end-start
               WHEN 4 DISPLAY "  Matchcode   " AT VDU-LP
                      MOVE SPACE TO LG-AKEY WV-MCODE
                      START LAGARTIK KEY NOT < LG-AKEY INVALID GO C
                      end-start
               WHEN 5 DISPLAY "    Nummer    " AT VDU-LP
                      MOVE 0 TO LG-NUM
                      START LAGARTIK KEY NOT < LG-KEY INVALID GO C
                      end-start.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 000 TO WZ.
           IF WH-MOD NOT = 2 and WH-MOD not = 3 and WH-MOD not = 7 GO E.
           MOVE 200 TO WZ.
           MOVE WM-DATUM TO WV-DATUM.
       D.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Inventurdatum: " AT VDU-LP.
           ADD 318 VDU-ECK GIVING VDU-LP.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1103186006" WH-CREG.
           IF ESC GO X.
           IF NOT RET OR WZ-DATUM = 0 GO D.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WZ-DATUM TO WV-DATUM.
       E.  ADD WZ 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
           ADD WZ 300 GIVING VDU-LP.
           CALL "CAUP" USING "1000330000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           MOVE LOW-VALUE TO WT-GRP.
       G.  READ LAGARTIK NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-BEZ AT VDU-LP.
           IF LG-ART > 0 GO G.
           EVALUATE WH-MOD
               WHEN 1 IF WV-LFT = SPACE MOVE LG-LFT TO WV-LFT
                      end-if
                      IF WV-LFT NOT = LG-LFT MOVE 2 TO WZ-SCHALT
                          MOVE 0 TO WK-KO
               WHEN 2 GO L
               WHEN 3 GO N
               WHEN 7 GO R.
           PERFORM LAGKOPF.
           MOVE LG-NUM TO DRL-LGNUM.
           MOVE LG-BEZ TO DRL-BEZ.
           if lg-preis = 0 divide lg-anz into lg-ekp giving lg-preis
               rewrite lg-satz.
           MOVE LG-PREIS TO DRL-PREIS.
           MOVE LG-LAGER TO DRL-LAGER.
           MOVE LG-SORT TO DRL-SORT.
           MOVE "-" TO DRL-SORT(4:1).
           MOVE LG-LFT TO DRL-LFT WV-LFT.
           MOVE LG-STAND TO WH-MENGE.
           MOVE LG-MEH TO WH-MEH.
           MOVE LG-NK TO WH-NK.
           PERFORM WD-MG.
           MOVE WD-MENGE TO DRL-STAND.
           MOVE LG-MIND TO WH-MENGE.
           PERFORM WD-MG.
           MOVE WD-MENGE TO DRL-MSTAND.
           MOVE LG-BMENGE TO WH-MENGE.
           MOVE LG-BMEH TO WH-MEH.
           MOVE LG-BNK TO WH-NK.
           PERFORM WD-MG.
           MOVE WD-MENGE TO DRL-BMENGE.
           PERFORM DRUCK.
           GO G.
      *--------------------------------------------> Inventuraufnahme <-
       L.  IF LG-LAGER = SPACE GO G.
           IF WV-LAGER = SPACE MOVE LG-LAGER TO WV-LAGER.
           IF WV-LAGER NOT = LG-LAGER and WZ-ZEILEN not = 0
                MOVE 79 TO WZ-ZEILEN
                MOVE 0 TO WK-KO.
           MOVE LG-LAGER TO WV-LAGER.
           PERFORM INVKOPF.
           MOVE LG-NUM TO DRI-LGNUM.
           MOVE x"1B213400" TO DRI-ACD.
           MOVE LG-BEZ TO DRI-BEZ.
           MOVE WK-STG(WH-P1) TO DRI-BCD.
           MOVE LG-LFT TO DRI-LFT.
           MOVE LG-SORT TO DRI-SORT.
           MOVE "-" TO DRI-SORT(4:1).
           MOVE "  ________________" TO DRI-USTR.
           MOVE WT-MEH(LG-BMEH + 1) TO DRI-MEH.
           MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
           GO G.
      *-------------------------------------------> Inventur-Istliste <-
       N.  IF LG-LAGER = SPACE GO G.
           IF WV-LAGER = SPACE MOVE LG-LAGER TO WV-LAGER.
           IF WV-LAGER NOT = LG-LAGER and WZ-ZEILEN not = 0
                MOVE 79 TO WZ-ZEILEN
                MOVE 0 TO WK-KO.
           MOVE LG-LAGER TO WV-LAGER.
           PERFORM ISTKOPF.
           MOVE LG-NUM TO DRJ-LGNUM.
           MOVE LG-BEZ TO DRJ-BEZ.
           MOVE LG-LFT TO DRJ-LFT.
      *--------------------------------------------> Inv-Symbol lesen <-
           MOVE LG-NUM TO LB-KTONR.
           MOVE WV-DATUM TO LB-BUDAT.
           MOVE 0 TO LB-LFD.
           START LAGERBZ KEY NOT < LB-KEY
               INVALID MOVE 0 TO WH-MENGE GO P.
       O.  READ LAGERBZ NEXT NO LOCK AT END MOVE 0 TO WH-MENGE GO P.
           IF ZUGRIF PERFORM BESETZT GO O.
           IF LB-KTONR not = LG-NUM or LB-BUDAT not = WV-DATUM
               MOVE 0 TO WH-MENGE GO P.
           IF LB-SYM = 02 MOVE LB-MENGE TO WH-MENGE
           else GO O.
       P.  IF LB-ANZ not = 0 MOVE LB-BNK TO WH-NK
                             MOVE LB-BMEH TO WH-MEH
                             MOVE LG-EKP TO WH-WERT
                        else MOVE LB-NK TO WH-NK
                             MOVE LB-MEH TO WH-MEH
                             DIVIDE LG-ANZ INTO LG-EKP GIVING WH-WERT.
           PERFORM WD-MG.
           IF WH-MENGE NOT = 0 MOVE WD-MENGE TO DRJ-MENGE
               MOVE WT-MEH(WH-MEH + 1) TO DRJ-MEH.
           MOVE WH-WERT TO DRJ-PREIS.
           COMPUTE WM-UMS = WH-MENGE * WH-WERT.
           ADD WM-UMS TO WS-SUM WG-UMS WZ-UMS.
           IF LG-GRP NOT = 0 ADD WM-UMS TO WS-GRP(LG-GRP).
           IF WM-UMS NOT = 0 MOVE WM-UMS TO DRJ-BET.
           PERFORM DRUCK.
           GO G.
      *------------------------------------------> Soll-Ist Vergleich <-
       R.  IF LG-LAGER = SPACE GO G.
           IF WV-LAGER = SPACE MOVE LG-LAGER TO WV-LAGER.
           IF WV-LAGER NOT = LG-LAGER and WZ-ZEILEN not = 0
                MOVE 79 TO WZ-ZEILEN
                MOVE 0 TO WK-KO.
           MOVE LG-LAGER TO WV-LAGER.
           PERFORM OLD-STAND.
           GO G.
       W.  IF WH-MOD = 3 PERFORM ISTSEIT
               MOVE ALL "=" TO DRJ-BET(1:)
               PERFORM DRUCK
               PERFORM GRPSUM.
           PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************* Ermittlung bis letzte Inventur *
       OLD-STAND SECTION.
       A.  MOVE 0 TO WS-MENGE WI-MENGE.
           MOVE LG-NUM TO LB-KTONR.
           MOVE WV-DATUM TO LB-BUDAT.
           MOVE 0 TO LB-LFD.
           START LAGERBZ KEY > LB-KEY INVALID GO G.
       C.  READ LAGERBZ PREVIOUS NO LOCK AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LB-KTONR not = LG-NUM or LB-BUDAT not = WV-DATUM GO G.
           IF LB-SYM not = 02 GO C.
           MOVE LB-MENGE TO WI-MENGE.        *> akt. Inventur-stand IST
       E.  READ LAGERBZ PREVIOUS NO LOCK AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LB-KTONR not = LG-NUM GO G.
           IF LB-ANZ not = 0
               MULTIPLY LB-ANZ BY LB-MENGE GIVING LB-MENGE.
           ADD LB-MENGE TO WS-MENGE.                          *>   SOLL
           IF LB-SYM not = 02 GO E.
      *-----------------------------------------------> ob Diffenrenz <-
       G.  IF WI-MENGE = WS-MENGE GO Z.
           PERFORM SOLLKOPF.
           MOVE LG-NUM TO DRM-LGNUM.
           MOVE LG-BEZ TO DRM-BEZ.
           MOVE LG-LFT TO DRM-LFT.
           MOVE WS-MENGE TO WH-MENGE.
           MOVE LG-MEH TO WH-MEH.
           MOVE WT-MEH(WH-MEH + 1) TO DRM-MEH.
           MOVE LG-NK TO WH-NK.
           PERFORM WD-MG.
           MOVE WD-MENGE TO DRM-SMENGE.
           MOVE WI-MENGE TO WH-MENGE.
           PERFORM WD-MG.
           MOVE WD-MENGE TO DRM-IMENGE.
           SUBTRACT WI-MENGE FROM WS-MENGE GIVING WH-MENGE.
           PERFORM WD-MG.
           MOVE WD-MENGE TO DRM-DIFFER.
           PERFORM DRUCK.
       Z.  EXIT.
      ********************************************* Gruppensummendruck *
       GRPSUM SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 31
               or WS-GRP(WX) NOT = 0 CONTINUE.
           IF WX > 31 GO Z.
           WRITE DRA-SATZ BEFORE PAGE.
           MOVE 0 TO WZ-ZEILEN WS-SUM.
           PERFORM LAGKOPF.
           MOVE 2 TO WZ-SCHALT.
           MOVE "Gruppensummen" TO DRJ-BEZ.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       C.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 30
               IF WS-GRP(WX) NOT = 0 MOVE WS-GRP(WX) TO WH-WERT
                   PERFORM READ-GRP
                   MOVE KL-GRPTX TO DRJ-BEZ
                   MOVE WX TO DRJ-LGNUM
                   MOVE WH-WERT TO DRJ-BET
                   ADD WH-WERT TO WS-SUM
                   PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRJ-BET(1:).
           PERFORM DRUCK.
           MOVE WS-SUM TO DRJ-BET.
           PERFORM DRUCK.
           MOVE ALL "=" TO DRJ-BET(1:).
           PERFORM DRUCK.
       Z.  EXIT.
      ********************************************** Gruppentext lesen *
       READ-GRP SECTION.
       A.  ADD WX 20 GIVING WH-KEY.
           READ KONSLAG INVALID MOVE SPACE TO KL-GRPTX.
           IF ZUGRIF PERFORM BESETZT GO A.
           UNLOCK KONSLAG.
       Z.  EXIT.
      ********************************************** Kopf Lagerlistung *
       LAGKOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 2 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 5 TO WH-P
                             MOVE 60 TO WH-SIZE.
           MOVE "LAGLIST.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE "Lagerliste nach             per: " TO DRL-STR.
           EVALUATE WH-MOD
               WHEN 1 MOVE "Lieferanten" TO DRL-STR(17:11)
               WHEN 2 MOVE "Lagerplatz " TO DRL-STR(17:11)
               WHEN 3 MOVE "Inventur   " TO DRL-STR(17:11)
               WHEN 4 MOVE "Matchcode  " TO DRL-STR(17:11)
               WHEN 5 MOVE "Nummern    " TO DRL-STR(17:11).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRL-STR(34:10).
           MOVE "Seite:" TO DRL-STR(77:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRL-STR(84:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRL-STR PERFORM DRUCK.
           MOVE "Ar.Nr B e z e i c h n u n g Lieferant Platz  Sort.
      -       "Preis   Bestell   Mindest     Lager" TO DRL-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRL-STR.
           MOVE " " TO DRL-STR(47:24).
           MOVE WK-FIRMA TO DRL-STR(48:22).
           PERFORM DRUCK.
       Z.  EXIT
      ********************************************** Kopf Lagerlistung *
       INVKOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 2 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 5 TO WH-P
                             MOVE 60 TO WH-SIZE.
           MOVE "INVLIST.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE "Inventuraufnahme per:             Lagerplatz:"
               TO DRI-STR.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRI-STR(23:10).
           MOVE WV-LAGER TO DRI-STR(47:10).
           MOVE "Seite:" TO DRI-STR(68:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRI-STR(75:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRI-STR PERFORM DRUCK
           MOVE "Ar.Nr  B e z e i c h n u n g            Liefer.    Sort
      -       ".   M e n g e  Meh" TO DRI-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRI-STR.
           MOVE " " TO DRI-STR(47:24).
           MOVE WK-FIRMA TO DRI-STR(48:22).
           PERFORM DRUCK.
       Z.  EXIT
      *********************************************** Ist-Seitensummen *
       ISTSEIT SECTION.
       A.  MOVE ALL "ƒ" TO DRJ-BET(1:).
           PERFORM DRUCK.
           MOVE "S-Su:" TO DRJ-STR(1:).
           MOVE WS-SUM TO DRJ-BET.
           MOVE DRJ-BET(1:) TO DRJ-STR(7:).
           MOVE "L-Su:" TO DRJ-STR(28:).
           MOVE WG-UMS TO DRJ-BET.
           MOVE DRJ-BET(1:) TO DRJ-STR(34:).
           MOVE "Gesamt:" TO DRJ-STR(55:).
           MOVE WZ-UMS TO DRJ-BET.
           PERFORM DRUCK.
           IF WZ-ZEILEN > 78 MOVE 0 TO WG-UMS.
           MOVE 0 TO WZ-ZEILEN WS-SUM.
       Z.  EXIT.
      ***************************************** Kopf Inventur-Istliste *
       ISTKOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE PERFORM ISTSEIT
               WRITE DRA-SATZ BEFORE PAGE.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 2 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 5 TO WH-P
                             MOVE 60 TO WH-SIZE.
           MOVE "ISTLIST.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE "Inventuristliste per:             Lagerplatz:"
               TO DRJ-STR.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRJ-STR(23:10).
           MOVE WV-LAGER TO DRJ-STR(47:10).
           MOVE "Seite:" TO DRJ-STR(65:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRJ-STR(72:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRJ-STR PERFORM DRUCK
           MOVE "Ar.Nr B e z e i c h n u n g      Liefer.   Menge Meh
      -         "  Preis        Betrag" TO DRJ-STR PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRJ-STR.
           MOVE " " TO DRJ-STR(47:24).
           MOVE WK-FIRMA TO DRJ-STR(48:22).
           PERFORM DRUCK.
       Z.  EXIT
      ********************************************* Kopf Soll-Istliste *
       SOLLKOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE PERFORM ISTSEIT
               WRITE DRA-SATZ BEFORE PAGE.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 2 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 5 TO WH-P
                             MOVE 60 TO WH-SIZE.
           MOVE "SOLLIST.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE "Soll-Ist-Liste per:               Lagerplatz:"
               TO DRM-STR.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRM-STR(21:10).
           MOVE WV-LAGER TO DRM-STR(47:10).
           MOVE "Seite:" TO DRM-STR(62:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRM-STR(69:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRM-STR PERFORM DRUCK
           MOVE "Ar.Nr B e z e i c h n u n g      Liefer. S-Menge Meh  I
      -         "-Menge Differenz" TO DRM-STR PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRM-STR.
           MOVE " " TO DRM-STR(47:24).
           MOVE WK-FIRMA TO DRM-STR(48:22).
           PERFORM DRUCK.
       Z.  EXIT
      ************************************** Auslistung Waageprotokoll *
       ART-LIST SECTION.
       A.  CALL "CAUP" USING "0707100559000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Verbrauchliste " with highlight AT VDU-LP.
           MOVE "PAKWAAG.DAT" TO WN-BUEB.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY WN-BUEB(1:20) with highlight AT VDU-LP.
           MOVE "VERBRAU.LST" TO WH-DRUNAM.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Artikelnummer: " AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, < >= alter Stand, <ret>= Artikel-Nr.
      -        "" AT 2401.
           CALL "CAUP" USING "1003184004" WH-CREG.
           IF ESC GO Y.
           IF AB; IF WN-BUEB(1:4) not = "PAKW"
                      MOVE "PAKWAAG.DAT" TO WN-BUEB
                  else MOVE "C:\MIST\PAKWAAG.DAT" TO WN-BUEB.
           IF NOT RET GO B.
           IF WH-NUM = 0 GO B.
           MOVE WH-NUM TO LG-NUM WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
       D.  READ LAGARTIK INVALID PERFORM NO-REC GO B.
           IF ZUGRIF PERFORM BESETZT GO D.
           ADD 7 TO VDU-LP.
           DISPLAY LG-BEZ with highlight AT VDU-LP.
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "ab Datum:" AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1104136006" WH-CREG.
           IF ESC GO B.
           IF not RET GO E.
           IF WZ-DATUM = 0 GO E.
           MOVE WZ-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       H.  DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO B.
           IF not RET GO H.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WK-SEITE.
           OPEN I-O PROTWAAG.
           MOVE LOW-VALUES TO PWA-AKEY.
           START PROTWAAG KEY NOT < PWA-AKEY INVALID GO X.
       K.  READ PROTWAAG NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE PWA-DAT TO WC-DATUM.
           PERFORM DATDREH.
           ADD 425 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM AT VDU-LP.
           IF PWA-DAT < WV-DATUM GO K.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               IF PWA-LGNUM(WX) = LG-NUM PERFORM VERBLIST.
           GO K.
       Q.  PERFORM END-DRU.
       X.  CLOSE PROTWAAG.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************************** Druck *
       VERBLIST SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE 2 TO WH-P1.
           EVALUATE WM-KO also WM-DRU
               WHEN 0 also 0 MOVE 5 TO WH-P2
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 0 MOVE 4 TO WH-P2
                             MOVE 63 TO WH-SIZE
               WHEN 0 also 1 MOVE 2 TO WH-P
                             MOVE 39 TO WH-SIZE
               WHEN 1 also 1 MOVE 5 TO WH-P
                             MOVE 60 TO WH-SIZE.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRF-STR(1:30).
           MOVE "Verbrauchsprotokoll per:" TO DRF-STR(32:).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRF-STR(57:10).
           MOVE "Seite:" TO DRF-STR(70:).
           ADD 1 TO WK-SEITE.
           MOVE WK-SEITE TO WD-NUM WV-SEITE.
           MOVE WD-NUM TO DRF-STR(76:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRF-STR PERFORM DRUCK.
           MOVE "Art-Nr    Datum Rzp-Nr Rez.Bezeichnung Art.Bezeichnung
      -       "              Istgewicht " TO DRF-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRF-STR PERFORM DRUCK.
       G.  MOVE LG-NUM TO DRF-ARTNR .
           MOVE PWA-DAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRF-DATUM.
           MOVE PWA-RENUM TO DRF-RPNR.
           MOVE PWA-RPBEZ TO DRF-RPBEZ.
           MOVE PWA-KOBEZ(WX) TO DRF-ARBEZ.
           MOVE PWA-IST(WX) TO DRF-IST.
           PERFORM DRUCK.
       Z.  EXIT.
