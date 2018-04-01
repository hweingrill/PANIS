      $SET LINKCOUNT "240" ANS85 AUTOLOCK BOUND NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PAKOBIL.
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
       COPY PARSEREZ.CPY.
           SELECT SUMMDAT    ASSIGN TO "PAKSUMM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SU-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PAKOFD.CPY.
       COPY PARFDART.CPY.
       COPY PARFDLFT.CPY.
       COPY PAKFDWAG.CPY.
       COPY PARREZEP.CPY.
      ************************************************* Rezeptursummen *
       FD  SUMMDAT                     LABEL RECORD STANDARD.
       01  SU-SATZ.
           03  SU-KEY.
               05 SU-NUM               PIC 9(6)        COMP.
           03  SU-SOLL                 PIC S9(9)       COMP.
           03  SU-IST                  PIC S9(9)       COMP.
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
           03  FILLER                  PIC X(5).
           03  DRB-STR.
               05 DRB-KONUM            PIC ZZZ9-.
               05 DRB-BEZ              PIC X(30).
               05 DRB-STAND            PIC X(10).
               05 DRB-MIND             PIC X(10).
               05 DRB-BEST             PIC X(10).
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
       01  DRG-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRG-STR.
               05 DRG-ARTNR            PIC ZZ.ZZ9-.
               05 DRG-ARBEZ            PIC X(26).
               05 DRG-SOLL             PIC ZZ.ZZ9,999-.
               05 DRG-IST              PIC ZZZ.ZZ9,999-.
               05 DRG-ABWKG            PIC ZZ.ZZ9,999-.
               05 DRG-ABWPZ            PIC ZZ9,99-.
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
           03  FILLER                  PIC XXXX.
           03  DRL-STR.
               05 DRL-LGNUM            PIC Z.ZZ9-.
               05 DRL-BEZ              PIC X(26).
               05 DRL-LFT              PIC X(11).
               05 DRL-LAGER            PIC X(6).
               05 DRL-SORT             PIC ZZ9,99-.
               05 DRL-PREIS            PIC ZZ.ZZ9,99-.
               05 DRL-BMENGE           PIC X(10).
               05 DRL-MSTAND           PIC X(10).
               05 DRL-STAND            PIC X(10).
           03  FILLER                  PIC X(10).
       01  DRP-SATZ.
           03  FILLER                  PIC X(9).
           03  DRP-STR.
               05 DRP-KTONR            PIC ZZ.999,9-.
               05 DRP-BEZ              PIC X(51).
               05 DRP-KZ               PIC XX.
               05 DRP-SOLL             PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-HABEN            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-SALDO            PIC ZZZ.ZZZ.ZZ9,99-.
       01  DRO-SATZ.
           03  FILLER                  PIC X(18).
           03  DRO-DAT                 PIC X(9).
           03  DRO-SYM                 PIC XXX.
           03  DRO-REFNR               PIC ZZZZZZ9-.
           03  DRO-REBET               PIC ZZZZZ.ZZ9,99-.
           03  DRO-ZABET               PIC ZZZZZ.ZZ9,99-.
           03  DRO-OFFEN               PIC ZZZZZ.ZZ9,99-.
           03  FILLER                  PIC XX.
           03  DRO-FAELLIG             PIC X(9).
           03  DRO-ZAHL                PIC XXX.
       01  DRS-SATZ.
           03  FILLER                  PIC X(4).
           03  DRS-STR.
               05 DRS-KTONR            PIC 99.999,9-.
               05 DRS-BEZ              PIC X(51).
               05 DRS-KZ               PIC XX.
               05 DRS-MSOLL            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-MHABEN           PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-GSOLL            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-GHABEN           PIC ZZZ.ZZZ.ZZ9,99-.
       01  DRX-SATZ.
           03  FILLER                  PIC X(4).
           03  DRX-STR.
               05 DRX-KONR             PIC Z9-.
               05 FILLER               PIC X.
               05 DRX-KOTX                   OCCURS 10.
                  07 DRX-KOART         PIC ZZ.ZZZ.ZZ9-.
                  07 FILLER            PIC X.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WH-BZSATZ               PIC X(66).
           03  WH-PWAKEY               PIC X(8).
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
           03  WH-MOD                  PIC 99        COMP.
           03  WK-KO                   PIC 99        COMP.
           03  WM-ART                  PIC 99        COMP.
           03  WB                      PIC 99        COMP.
           03  WP                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
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
           03  WX-KOART                PIC X(11)    OCCURS 20.
           03  WY-KOART                PIC X(15)    OCCURS 20.
           03  WM-UMS                  PIC S9(9)V99  COMP.
           03  WV-UMS                  PIC S9(9)V99  COMP.
           03  WS-ART                  PIC 9(5)      COMP   OCCURS 3.
           03  wj-zeil                 pic 9(7)      COMP.
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(8)      COMP.
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
           03  WV-ABDAT                PIC 9(8).
           03  WV-BISDAT               PIC 9(8).
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-JONUM                PIC Z.ZZ9-.
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-UMSATZ               PIC ZZZ.ZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-RENUM                PIC ZZ.ZZ9.
           03  WD-ANZ                  PIC Z.ZZ9.
           03  WD-KG                   PIC ZZ9,999-.
           03  WD-ABW                  PIC ZZ9,99-.
           03  WD-ZEIT                 PIC 99,99.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-SEITE                PIC ZZ9-.
           03  WD-KZ                   PIC 9.
           03  WD-PZ                   PIC 99.
           03  WH-MENGE                PIC S9(7)V99   COMP.
           03  WD-MENGA                PIC ZZ.ZZ9,99-.
           03  WD-MENGB                PIC ZZ.ZZ9,9-.
           03  WD-MENGC                PIC ZZZ.ZZ9-.
           03  WD-MENGE                PIC X(10).
           03  WT-DRC.
               05 WT-DR                PIC X(30)     OCCURS 2.
           03  KS-UMS                  PIC 9(7)V99   COMP.
       01  WT-DK    external.
           03  WR-TN                   PIC X(10)  OCCURS 7.
           03  WT-KON                  PIC 99     OCCURS 12.
       COPY PAKOEXT.CPY.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON PROTWAAG.
       A.  CALL "CADECL" USING "PAKWAAG.DAT " WH-CREG.
       DECL-R SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       COPY PAKODECL.CPY.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           OPEN INPUT REZEPT.
           OPEN I-O PROTWAAG.
           EVALUATE WL-CA
               WHEN 30 PERFORM LI-WAHL.
           MOVE WH-CREG TO WL-CREG.
           CLOSE REZEPT.
           CLOSE PROTWAAG.
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
       A.  DISPLAY "** Datenende **" with highlight blink AT 2466.
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
           OPEN OUTPUT DRUCKER.
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
       LI-WAHL SECTION.
       A.  MOVE " Waageprotokolle" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "0705121352000" WH-CREG.
       B.  PERFORM LADE-DRU.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 9 TO WH-PG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Waage-Protokolle " with highlight AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - öbernahme Waage"       AT 0817.
           DISPLAY "2 - Abfrage Rezepte (Nr.)" AT 0017.
           DISPLAY "3 - Abfrage Rezepte (Datum)" AT 0017.
           DISPLAY "4 - Rezeptbilanz"          AT 0017.
           DISPLAY "5 - Protokoll-Lîschung"    AT 0017.
           DISPLAY "<ret-leer>= Programmende"  AT 0017.
           DISPLAY " " AT 0017.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT 0032.
       G.  CALL "CAUP" USING "1011391001" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC OR WH-PG = 0 GO X.
           IF NOT RET GO G.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           EVALUATE WH-PG
               WHEN 1 PERFORM WAAGUEB
               WHEN 2
               WHEN 3 PERFORM BIL-ABF
               WHEN 4 PERFORM REZ-BIL
               WHEN 5 PERFORM PROTLOE.
           GO A.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
           UNLOCK LAGERBZ.
           UNLOCK PROTWAAG.
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
           SUBTRACT WV-UMS FROM KS-UMS.
           MOVE 0 TO WV-UMS.
       Q.  IF WM-UMS NOT = KS-UMS
               DISPLAY WM-UMS AT 2020
               IF WH-PG NOT = 6 AND WM-ART = 1
                    CALL "CAUP" USING "0023300000" WH-CREG.
           IF LB-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE LG-SATZ.
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
               MOVE KS-UMS TO WE-UMS
               ADD WE-UMS TO WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           IF LG-NUM = 0 GO X ELSE GO C.
       X.  SET FEHLER TO TRUE.
           MOVE "Konto fehlt" TO WT-TX.
       Z.  EXIT.
      ******************************************************* öbertrag *
       UEBERTRAG SECTION.
       A.  MOVE "öbertrag:" TO DRK-BEZ(27:).
           MOVE WM-UMS TO DRK-SOLL.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************* öbernahme Wiegedaten *
       WAAGUEB SECTION.
       A.  CALL "CAUP" USING "0707100552000" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "öbernahme Wiegedaten bis: " AT VDU-LP.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 330 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
       C.  DISPLAY "<ret>= Start, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "1103306006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO A.
           IF WZ-DATUM = 0 GO A.
           MOVE WX-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WH-NUM NOT = 0 GO C.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WK-SEITE.
           MOVE "WAAGFEHL.LST" TO WH-DRUNAM.
           ADD 237 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           MOVE LOW-VALUES TO PWA-AKEY.
           START PROTWAAG KEY NOT < PWA-AKEY INVALID GO X.
       E.  READ PROTWAAG NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF PWA-DAT > WV-DATUM OR PWA-SA > 9 GO Q.
           MOVE 1 TO WX.
      *----------------------------------> Lagerbuchungen durchfÅhren <-
       G.  IF PWA-LGNUM(WX) = 0 GO O.
           IF PWA-IST(WX) = 0 or PWA-ANZ = 0
               PERFORM FEHLPROT.
           MOVE PWA-LGNUM(WX) TO LG-NUM.
       I.  READ LAGARTIK INVALID DISPLAY LG-NUM AT 2401
               " Artikel bitte anlegen"
               PERFORM WEITER GO G.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE LG-NUM TO LB-KTONR.
           MOVE PWA-DAT TO LB-BUDAT.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY LB-BUDAT AT VDU-LP " " LB-KTONR.
           MOVE 1 TO LB-LFD.
       K.  READ LAGERBZ INVALID PERFORM INIT-BZ.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE WE-USE TO LB-USE.
           MOVE 4 TO LB-SYM.
           MOVE SPACE TO LB-SRECH.          *> SUPPRESS bei altn.Key
           MOVE LG-MEH TO LB-MEH.
           MOVE LG-NK TO LB-NK.
           MOVE LG-BMEH TO LB-BMEH.
           MOVE LG-BNK TO LB-BNK.
           MOVE LG-PREIS TO LB-PREIS.
           DIVIDE 1000 INTO PWA-IST(WX) GIVING WH-WERT.
           MULTIPLY -1 BY WH-WERT GIVING LB-MENGE.
           SUBTRACT WH-WERT FROM LG-STAND.
           REWRITE LG-SATZ.
           IF LB-MENGE NOT = 0 PERFORM LB-WRITE.
           PERFORM FEHLPROT.
       O.  IF WX < 20 ADD 1 TO WX GO G.
           MOVE 10 TO PWA-SA.
           REWRITE PWA-SATZ.
           GO E.
       Q.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************** Buchungszeile speichern *
       LB-WRITE SECTION.
       A.  MOVE WE-USE TO LB-USE.
           ADD 1 TO LB-SEITE.
           MOVE LB-SATZ TO WH-BZSATZ.
           MOVE LB-KEY TO WH-BKEY.
           MOVE 253 TO LB-LFD.
           START LAGERBZ KEY < LB-KEY INVALID MOVE 1 TO WH-LFD GO E.
       C.  READ LAGERBZ PREVIOUS WITH NO LOCK AT END
               MOVE 1 TO WH-LFD GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LB-KTONR NOT = WH-KTONR OR LB-BUDAT NOT = WH-BUDAT
               MOVE 1 TO WH-LFD GO E.
           ADD 1 LB-LFD GIVING WH-LFD.
           IF LB-SYM NOT = 4 GO E.
           SUBTRACT WH-WERT FROM LB-MENGE.
           REWRITE LB-SATZ.
           GO Z.
       E.  MOVE WH-BZSATZ TO LB-SATZ.
           MOVE WH-BKEY TO LB-KEY.
           WRITE LB-SATZ INVALID GO A.
           IF KL-JOKEY = LOW-VALUE MOVE LB-JKEY TO KL-JOKEY.
       Z.  EXIT.
      ************************************************ Fehlerprotokoll *
       FEHLPROT SECTION.
       A.  IF PWA-IST(WX) = 0 GO C.
           SUBTRACT PWA-IST(WX) FROM PWA-SOLL(WX) GIVING WH-WERT.
           MULTIPLY 100 BY WH-WERT.
           DIVIDE PWA-SOLL(WX) INTO WH-WERT GIVING WV-WTOL.
           IF WV-WTOL < WE-WTOL GO Z.
       C.  IF WZ-ZEILEN > 63 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 39 TO WH-SIZE
                   ELSE MOVE 4 TO WH-P2
                        MOVE 63 TO WH-SIZE.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Fehlerprotokoll per:" TO DRF-STR.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRF-STR(22:10).
           MOVE "Seite:" TO DRF-STR(70:).
           ADD 1 TO WK-SEITE.
           MOVE WK-SEITE TO WD-NUM WV-SEITE.
           MOVE WD-NUM TO DRF-STR(76:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRF-STR PERFORM DRUCK.
           MOVE "Art-Nr    Datum Rzp-Nr Rez.Bezeichnung Art.Bezeichnung
      *          12.345 95-01-01 12.345 123451234567890 123451234567890
      -       "          Soll  -Gew-  Ist " TO DRF-STR(1:).
      *       "234567890 123,129- 345,679-
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRF-STR PERFORM DRUCK.
       G.  MOVE LB-KTONR TO DRF-ARTNR .
           MOVE LB-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRF-DATUM.
           MOVE PWA-RENUM TO DRF-RPNR.
           MOVE PWA-RPBEZ TO DRF-RPBEZ.
           MOVE LG-BEZ TO DRF-ARBEZ.
           MOVE PWA-SOLL(WX) TO DRF-SOLL.
           MOVE PWA-IST(WX) TO DRF-IST.
           PERFORM DRUCK.
       Z.  EXIT.
      ****************************************** Waageprotokollabfrage *
       BIL-ABF SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Rezeptur: " AT 0301.
           IF WH-PG = 2 GO C.
           DISPLAY "vom:          /      " AT 0348.
           MOVE WM-DATUM TO WV-DATUM WC-DATUM.
           PERFORM DATDREH.
           MOVE WV-DATUM TO WH-WERT WZ-DATUM.
           DISPLAY "<ret>= Åbernommene Rezepte, < >= nicht Åbern. Rez."
               AT 2301.
           CALL "CAUP" USING "0103536006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET and NOT AB GO A.
           IF WX-DATUM = 0 GO A.
           MOVE WX-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE LOW-VALUES TO PWA-SATZ.
           MOVE WV-DATUM TO PWA-DAT.
           IF AB MOVE 0 TO PWA-SA
            else MOVE 10 TO PWA-SA.                 *> falls Åbernommen
           START PROTWAAG KEY NOT < PWA-AKEY INVALID
                PERFORM NODATA GO A.
           GO I.
       C.  IF WH-PG = 2 GO D.
           DISPLAY "<esc>= Abbruch, " AT 2301.
           EVALUATE WH-MOD
               WHEN 1 DISPLAY "< >= vorheriges Rezept" AT 0000
               WHEN 2 DISPLAY "< >= nÑchstes Rezept" AT 0000.
           CALL "CAUP" USING "0003110000" WH-CREG.
           CALL "CAUP" USING "1305012580" WH-CREG.
           IF ESC GO A.
           IF AB GO M.
           IF AUF GO O.
           GO C.
       D.  DISPLAY "<esc>= Abbruch, alpha+<ret>= suchen Rezeptur"
               AT 2301.
           CALL "CAUP" USING "0003114010" WH-CREG.
           CALL "CAUP" USING "1305012580" WH-CREG.
           IF ESC GO Z.
           IF AB GO M.
           IF AUF GO O.
           IF NOT RET GO C.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PANANZ" USING "40RPSUCH" WH-CREG
               CANCEL "PANANZ".
           MOVE WH-NUM TO RE-NUM.
       E.  IF RE-NUM = 0 GO C.
           MOVE RE-NUM TO WD-NUM PWA-RENUM.
           MOVE WM-DATUM TO PWA-DAT.
           IF RE-NUM = 9999 MOVE "Entnahme o. Rezeptur" TO RE-BEZ GO G.
       F.  READ REZEPT INVALID PERFORM NO-REC GO C.
           IF ZUGRIF PERFORM BESETZT GO F.
       G.  DISPLAY WD-NUM with highlight AT VDU-LP.
           ADD 7 TO VDU-LP.
           DISPLAY RE-BEZ with highlight AT VDU-LP.
           ADD 30 TO VDU-LP.
           DISPLAY "vom:          /      " AT VDU-LP.
           MOVE RE-NUM TO PWA-RENUM.
       H.  DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret>= Datum" AT 2301.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0353.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0103536006" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO C.
           IF NOT RET GO H.
           MOVE 0 TO WH-MOD.
           MOVE WZ-DATUM TO PWA-DAT.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE 11 TO PWA-SA.
           START PROTWAAG KEY NOT < PWA-KEY INVALID PERFORM NO-REC GO Z.
       I.  READ PROTWAAG NEXT AT END PERFORM NO-REC GO Z.
           IF ZUGRIF PERFORM BESETZT GO I.
           EVALUATE WH-PG
               WHEN 2 IF RE-NUM NOT = PWA-RENUM PERFORM NODATA GO A
               WHEN 3 IF WV-DATUM NOT = PWA-DAT PERFORM NODATA GO A.
       K.  MOVE PWA-AKEY TO WH-PWAKEY.
           MOVE 0 TO WH-MOD.
           MOVE PWA-RENUM TO WD-NUM.
           MOVE 311 TO VDU-LP.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           ADD 7 TO VDU-LP.
           DISPLAY PWA-RPBEZ with highlight AT VDU-LP.
           ADD 30 TO VDU-LP.
           DISPLAY "vom:          /      " AT VDU-LP.
           MOVE PWA-DAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0353.
           DIVIDE 100 INTO PWA-ZEIT GIVING WD-ZEIT.
           MOVE ":" TO WD-ZEIT(3:1).
           DISPLAY WD-ZEIT AT 0364.
           MOVE PWA-ANZ TO WD-ANZ.
           DISPLAY "Menge: " AT 0460 WD-ANZ with highlight.
           DISPLAY " Art.Nr Bezeichnung        Soll  -Gew-  Ist    Abwei
      -         "chg. " with reverse-video AT 0401.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               OR PWA-LGNUM(WX) = 0
               COMPUTE VDU-LP = WX * 100 + 402
               MOVE PWA-LGNUM(WX) TO WD-RENUM
               DISPLAY WD-RENUM AT VDU-LP
               ADD 7 TO VDU-LP
               DISPLAY PWA-KOBEZ(WX) with highlight AT VDU-LP
               ADD 19 TO VDU-LP
               DIVIDE 1000 INTO PWA-SOLL(WX) GIVING WD-KG
               DISPLAY WD-KG AT VDU-LP
               ADD 9 TO VDU-LP
               DIVIDE 1000 INTO PWA-IST(WX) GIVING WD-KG
               DISPLAY WD-KG with highlight AT VDU-LP "kg "
               SUBTRACT PWA-SOLL(WX) FROM PWA-IST(WX) GIVING WH-WERT
               COMPUTE WD-ABW = WH-WERT * 100 / PWA-SOLL(WX)
               DISPLAY WD-ABW with highlight AT 0000 "%".
           DISPLAY "< / >= nÑchstes/vorheriges Protokoll" AT 2401.
           GO C.
       M.  IF FINE AND WH-MOD = 1 GO Q.
           MOVE 1 TO WH-MOD.
           READ PROTWAAG NEXT AT END GO Q.
           EVALUATE WH-PG
               WHEN 2 IF RE-NUM NOT = PWA-RENUM GO Q
               WHEN 3 IF WV-DATUM NOT = PWA-DAT PERFORM NODATA GO C.
           GO K.
       O.  IF FINE AND WH-MOD = 2 GO Q.
           MOVE 2 TO WH-MOD.
           READ PROTWAAG PREVIOUS AT END GO Q.
           EVALUATE WH-PG
               WHEN 2 IF RE-NUM NOT = PWA-RENUM GO Q
               WHEN 3 IF WV-DATUM NOT = PWA-DAT PERFORM NODATA GO C.
           GO K.
       Q.  PERFORM NODATA.
           GO C.
       Z.  EXIT.
      ****************************** Erstellung und Druck Rezeptbilanz *
       REZ-BIL SECTION.
       A.  CALL "CAUP" USING "0707100653000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Rezeptbilanz " with highlight AT VDU-LP.
           MOVE WH-DATUM TO WC-DATUM.
           EVALUATE WC-MONAT
               WHEN 1 MOVE 12 TO WC-MONAT
                      ADD -1 TO WC-JAHR
               WHEN OTHER ADD -1 TO WC-MONAT.
           MOVE 1 TO WC-JAHR.
       C.  MOVE WC-DATUM TO WV-ABDAT WZ-DATUM.
       E.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE WV-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Summenbildung ab:           bis:" AT VDU-LP.
           ADD 321 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WV-ABDAT TO WZ-DATUM WH-WERT.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1103216006" WH-CREG.
           IF WZ-DATUM = 0 OR WZ-DATUM > WM-DATUM
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               DISPLAY "falsches Datum" AT 2401
               PERFORM WEITER GO E.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF ESC OR WOLI GO X.
           IF NOT RET GO G.
           MOVE WX-DATUM TO WV-ABDAT.
           MOVE 32 TO WZ-TAG.
       H.  MOVE WZ-DATUM TO WV-BISDAT.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE WV-BISDAT TO WZ-DATUM
               ADD -1 TO WZ-TAG GO H.
       I.  ADD 336 VDU-ECK GIVING VDU-LP.
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WV-BISDAT TO WZ-DATUM WH-WERT.
       J.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1103366006" WH-CREG.
           IF RET; IF WX-DATUM = 0 OR WX-DATUM > WM-DATUM
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               DISPLAY "falsches Datum" AT 2401
               PERFORM WEITER GO I.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF ESC GO X.
           IF WOLI GO E.
           IF NOT RET GO J.
           MOVE WX-DATUM TO WV-BISDAT.
           IF WV-ABDAT > WV-BISDAT GO J.
           ADD 240 VDU-ECK GIVING VDU-LP.
           MOVE "REZBIL.LST" TO WH-DRUNAM.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO K.
      *--------------------------------------------> Start Summierung <-
           OPEN OUTPUT SUMMDAT.
           CLOSE SUMMDAT.
           OPEN I-O SUMMDAT.
           MOVE 10 TO PWA-SA.
           MOVE WV-ABDAT TO PWA-DAT
           MOVE 0 TO PWA-RENUM.
           ADD 403 VDU-ECK GIVING VDU-LP.
           START PROTWAAG KEY NOT < PWA-AKEY INVALID GO X.
       L.  READ PROTWAAG NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO L.
           MOVE PWA-RENUM TO WD-RENUM.
           DISPLAY WD-RENUM AT VDU-LP " " PWA-RPBEZ with highlight.
           IF PWA-DAT > WV-BISDAT GO Q.
           MOVE 0 TO WX.
           ADD 503 VDU-ECK GIVING VDU-LP.
       M.  ADD 1 TO WX.
           IF PWA-LGNUM(WX) = 0 GO P.
           MOVE PWA-LGNUM(WX) TO SU-NUM.
       N.  READ SUMMDAT INVALID MOVE 0 TO SU-SOLL SU-IST.
           IF ZUGRIF PERFORM BESETZT GO N.
           MOVE SU-NUM TO WD-RENUM.
           DISPLAY WD-RENUM AT VDU-LP " " PWA-KOBEZ(WX) with highlight.
           ADD PWA-SOLL(WX) TO SU-SOLL.
           ADD PWA-IST(WX) TO SU-IST.
           REWRITE SU-SATZ INVALID WRITE SU-SATZ.
       P.  IF WX < 20 GO M else GO L.
      *------------------------------------------> Druck Rezeptbilanz <-
       Q.  PERFORM BILDRU.
           PERFORM END-DRU.
           DELETE FILE SUMMDAT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BILDRU SECTION.
       A.  MOVE 0 TO SU-NUM WZ-ZEILEN WK-SEITE WZ-SEITE.
           START SUMMDAT KEY NOT < SU-KEY INVALID GO Z.
       C.  READ SUMMDAT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE SU-NUM TO WD-RENUM LG-NUM.
       E.  READ LAGARTIK INVALID MOVE "-nicht angelegt-" TO LG-BEZ.
           IF ZUGRIF PERFORM BESETZT GO E.
           DISPLAY WD-RENUM AT VDU-LP " " LG-BEZ with highlight.
           IF WZ-ZEILEN > 63 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 39 TO WH-SIZE
                   ELSE MOVE 4 TO WH-P2
                        MOVE 63 TO WH-SIZE.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Rezeptbilanz vom:          bis: " TO DRG-STR.
           MOVE WV-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRG-STR(19:8).
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRG-STR(33:8).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRG-STR(55:10).
           MOVE "Seite:" TO DRF-STR(65:).
           ADD 1 TO WK-SEITE.
           MOVE WK-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRF-STR(71:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRG-STR PERFORM DRUCK.
           MOVE "Art-Nr Komponentenbezeichnung      Soll-Gew.    Ist-Gew
      *          12.345 1234512345678901234567890 12.345,679- 12.345,678
      -        " Abweichung  %-Abw." TO DRG-STR.
      *          12.345,123- 123,45-
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRG-STR PERFORM DRUCK.
       G.  MOVE SU-NUM TO DRG-ARTNR.
           MOVE LG-BEZ TO DRG-ARBEZ.
           DIVIDE 1000 INTO SU-SOLL GIVING DRG-SOLL.
           DIVIDE 1000 INTO SU-IST  GIVING DRG-IST.
           SUBTRACT SU-IST FROM SU-SOLL GIVING WH-WERT.
           DIVIDE 1000 INTO WH-WERT GIVING DRG-ABWKG.
           MULTIPLY 100 BY WH-WERT.
           COMPUTE DRG-ABWPZ ROUNDED = WH-WERT / SU-SOLL.
           PERFORM DRUCK.
           GO C.
       Z.  EXIT.
      *************************************** lîschen alter Protokolle *
       PROTLOE SECTION.
       A.  CALL "CAUP" USING "0707100550000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lîschung Waage-Protokolle " with highlight
                AT VDU-LP.
           MOVE WH-DATUM TO WC-DATUM.
           EVALUATE WC-MONAT
               WHEN 2 MOVE 12 TO WC-MONAT
                      ADD -1 TO WC-TAG
               WHEN 1 MOVE 11 TO WC-MONAT
                      ADD -1 TO WC-TAG
               WHEN OTHER ADD -2 TO WC-MONAT.
           MOVE 32 TO WC-JAHR.
       C.  MOVE WC-DATUM TO WV-BISDAT WZ-DATUM
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WX-DATUM = 0 MOVE WV-BISDAT TO WC-DATUM
               ADD -1 TO WC-JAHR GO C.
           MOVE WX-DATUM TO WV-BISDAT.
       E.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Lîschung bis inkl.: " AT VDU-LP
               VDU-DATUM with highlight.
           MOVE WV-BISDAT TO WZ-DATUM WH-WERT.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Datum, <Entf>= Start Lîschung
      -         "" AT 2301.
           CALL "CAUP" USING "1103236006" WH-CREG.
           IF RET; IF WX-DATUM = 0 OR WX-DATUM > WV-BISDAT
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               DISPLAY "falsches Datum" AT 2401
               PERFORM WEITER GO E
               else DISPLAY VDU-DATUM with highlight AT VDU-LP GO G.
           IF ESC GO X.
           IF NOT ENTF GO G.
           MOVE WX-DATUM TO WV-BISDAT.
      *-----------------------------------> Lîschung alter Protokolle <-
           MOVE 10 TO PWA-SA.
           MOVE 0 TO PWA-DAT PWA-RENUM.
           ADD 403 VDU-ECK GIVING VDU-LP.
           START PROTWAAG KEY NOT < PWA-AKEY INVALID GO X.
       I.  READ PROTWAAG NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE PWA-RENUM TO WD-RENUM.
           DISPLAY WD-RENUM AT VDU-LP " " PWA-RPBEZ with highlight.
           IF PWA-DAT > WV-BISDAT GO X.
       K.  DELETE PROTWAAG INVALID GO I.
           IF ZUGRIF PERFORM BESETZT GO K.
           GO I.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
