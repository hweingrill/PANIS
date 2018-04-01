      $SET LINKCOUNT"512" ANS85 BOUND AUTOLOCK NOALTER
      $SET SEGSIZE "50000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANTUED.
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSEKON.CPY.
           COPY PANSEART.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEDAU.CPY.
           COPY PANSESTA.CPY.
           COPY PARSEREZ.CPY.
           SELECT SUMMEN     ASSIGN TO "PANESUMM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SU-KEY
                             ALTERNATE RECORD KEY SU-AKEY
                             FILE STATUS IS WF-STATUS.
           SELECT PROTOK     ASSIGN TO "PANEPROT.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT ARCHIV     ASSIGN TO WH-ARCHIV
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT BACKZHB    ASSIGN TO "\BA40V3\BZ"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT HBTXT      ASSIGN TO "\BA40V3\OUT_RSTA.TXT"
      *                      ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT BACKZET    ASSIGN TO "ERZGVZ.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY BZ-KEY
                             ALTERNATE RECORD KEY BZ-AKEY
                             FILE STATUS IS WF-STATUS.
           SELECT LAGER      ASSIGN TO "PANELAG.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY LA-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
       COPY PANSTAT.CPY.
       COPY PANLIEF.CPY.
       COPY PANSUMM.CPY.
       COPY PARREZEP.CPY.
      ********************************************* Lieferscheinarchiv *
       FD  ARCHIV                      LABEL RECORD OMITTED.
       01  AC-ASATZ.
           03  AC-LFNUM                PIC 9(5).
           03  AC-KTONR                PIC 9(5).
           03  AC-MCODE.
               05 AC-LFDAT             PIC 999999.
               05 AC-FIL               PIC 99.
       01  AC-SATZ                     PIC X(132).
      ************************************************** HB-Backzettel *
       FD  BACKZHB                     LABEL RECORD STANDARD.
       01  HB-SATZ.
           03  HB-NUM                  PIC 999.
           03  HB-RPBEZ                PIC X(25).
           03  HB-GESGEW               PIC 9(8).
           03  HB-KNET                 PIC 9.
           03  HB-ZEIT                 PIC 9999.
           03  HB-CHARGEW              PIC 9(6).
           03  HB-ZUGANG               PIC 9(5).
      *************************************************** HB-Textdatei *
       FD  HBTXT                       LABEL RECORD STANDARD.
       01  HT-SATZ.
           03  HT-NUM                  PIC 999.
           03  HT-RPBEZ                PIC X(27).
           03  HT-PRZ                  PIC 999.
           03  FILLER                  PIC XX.
           03  HT-DAT                  PIC 9(6).
           03  HT-ZEIT                 PIC X(5).
           03  FILLER                  PIC XX.
      ************************************************** HB-Backzettel *
       FD  BACKZET                     LABEL RECORD STANDARD.
       01  BZ-SATZ.
           03  BZ-AKEY.
               05 BZ-WI                PIC 99        COMP.
               05 BZ-KEY.
                  07 BZ-SRT            PIC 9999      COMP.
                  07 BZ-NUM            PIC 9999      COMP.
                  07 BZ-ZEIT           PIC 9999      COMP.
           03  BZ-RPBEZ                PIC X(25).
           03  BZ-GRAMM                PIC 9(8)      COMP.
           03  BZ-KNET                 PIC 9.
           03  BZ-CHARGEW              PIC 9(6)      COMP.
           03  BZ-ZUGANG               PIC 9(5)      COMP.
           03  BZ-CHMAX                PIC 9(6)      COMP.
           03  BZ-CHKZ                 PIC 9.
           03  BZ-CHGEW                PIC 9(6)      COMP.
      ********************************************** Artikelstatistik *
       FD  LAGER      external         LABEL RECORD STANDARD
                                       RECORD 192 CHARACTERS.
       01  LA-SATZ.
           03  LA-KEY.
               05 LA-KZ                PIC 99        COMP.
               05 LA-ARNUM             PIC 9(4)      COMP.
           03  LA-PERIOD               PIC 9(6)      COMP.
           03  LA-DATUM                PIC 9(6)      COMP.
           03  LA-PDAT                 PIC 9(6)      COMP.
           03  LA-MENGEN               PIC S9(7)V99  COMP-3  OCCURS 36.
      *-----> LA-MENGEN: 34= Produktion *** 35= Verkauf *** 36= Stand <-
      *----------------------------------------> Record '0' Tageliste <-
       01  LA-BSATZ.
           03  FILLER                  PIC X(70).
           03  LA-JAHR                 PIC 9999      COMP.
           03  LA-TN.
               05 LA-TGTGN                 OCCURS 30.
                  07 LA-TAGE           PIC 9(6)     COMP.
                  07 LA-TGN            PIC 99       COMP.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  DRA-NO                  PIC XX.
           03  DRA-TEXT.
               05 DRA-KTONR            PIC ZZ.ZZ9,9.
               05 DRA-ARNUM            PIC ZZZZ.ZZ9-.
               05 DRA-ART              PIC ZZZ9-.
               05 DRA-MON              PIC ZZZ9/99-.
               05 FILLER               PIC X(210).
       01  DRC-SATZ.
           03  FILLER                  PIC X(6).
           03  DRC-TX.
               05 FILLER               PIC X.
               05 DRC-ARNUM            PIC Z(12)9-.
               05 DRC-BEZ              PIC X(27).
               05 DRC-MENGE            PIC X(12).
               05 DRC-MEH              PIC XXX.
               05 DRC-RETMG            PIC X(12).
               05 FILLER               PIC X(4).
               05 DRC-BETRAG           PIC ZZZZ.ZZ9,99-.
           03  FILLER                  PIC X(44).
       01  DRH-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRH-STR.
               05 DRH-ARNUM            PIC ZZ9-.
               05 DRH-ARBEZ            PIC X(27).
               05 DRH-MEH              PIC XXX.
               05 DRH-NK               PIC Z9-.
               05 DRH-PL               PIC 9/.
               05 DRH-TB               PIC 9.
               05 DRH-PR               PIC X(10)          OCCURS 3.
               05 DRH-UST              PIC Z9/.
               05 DRH-MWST             PIC 99.
       01  DRI-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRI-STR.
               05 FILLER               PIC X(26).
               05 DRI-MG               PIC X(7)      OCCURS 15.
       01  DRM-SATZ.
           03  FILLER                  PIC X(10).
           03  DRM-STR.
               05 DMX.
                  07 DRM-NUM           PIC ZZZ9-.
                  07 DRM-BEZ           PIC X(27).
               05 DMY REDEFINES DMX.
                  07 DRM-ABEZ          PIC X(26).
                  07 DRM-ANUM          PIC ZZZZ9-.
               05 DRM-MG               PIC X(10).
               05 FILLER               PIC X.
               05 DRM-MEH              PIC XXX.
               05 DRM-GEW              PIC ZZZZ.ZZ9,99-.
               05 DRM-STK REDEFINES
                  DRM-GEW              PIC ZZZZZZZ.ZZZ-.
               05 DRM-FEHL             PIC XXX.
               05 DRM-TGRP             PIC ZZ9.
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
       01  WH-ARCHIV.
           03  FILLER                  PIC XX        VALUE "LF".
           03  WH-ARCNR                PIC 9(5).
           03  WH-ARCLF                PIC 9.999.
       01  WH-REG.
           03  WH-PX                   PIC XX        OCCURS 2.
           03  WM-OPEN                 PIC 9         COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-TNR                  PIC 99        COMP   VALUE ZERO.
           03  WH-FAKT                 PIC 99        COMP   VALUE ZERO.
           03  WM-TA                   PIC 99        COMP.
           03  WK-TG                   PIC 999       COMP.
           03  WK-SRT                  PIC 999       COMP.
           03  WK-D                    PIC 9999      COMP.
           03  WK-S                    PIC 9999      COMP.
           03  IP                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WE                      PIC 999       COMP.
           03  WT-TG.
               05 WT-TGRP              PIC 99        COMP-X  OCCURS 250.
           03  WT-ERFTAB.
               05  WT-RKEY             PIC 9999      COMP  OCCURS 25.
           03  WK-DS.
               05 FILLER               PIC X(5)       VALUE "per:".
               05 WK-DAT               PIC X(10).
               05 FILLER               PIC X(7)       VALUE "Seite: ".
               05 WK-SEITE             PIC ZZ9-.
           03  WH-VON                  PIC 9999.
           03  WK-VON                  PIC 9999      COMP.
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1:".
           03  WV-DAT                  PIC 9(6)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-LFNUM                PIC 9(5)      COMP.
           03  WH-MENGE                PIC S9(7)     COMP-3.
           03  WH-ANZ                  PIC S9(8)V99  COMP.
           03  WS-ANZ                  PIC S9(8)V99  COMP.
           03  WA-ANZ                  PIC S9(8)V99  COMP.
           03  WH-ANZ1                 PIC S9999V9.
           03  WH-ANZ2                 PIC S999V99.
           03  WH-L                    PIC 99        COMP.
           03  WM-NEU                  PIC 99        COMP.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-LFNUM                PIC ZZ.ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-BKZ                  PIC 9(7).
           03  WD-PZ                   PIC 9,9-.
           03  WD-UST                  PIC Z9,99-.
           03  WD-TNR                  PIC Z9.
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
           03  WD-KZ                   PIC 9.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
           03  WS-GEW                  PIC S9(8)      COMP-3.
           03  WM-ANZ                  PIC S9(7)V99   COMP-3.
           03  WH-MEH                  PIC 99         COMP.
           03  WB-GRAMM                PIC 9(6)       COMP.
           03  WV-CHMAX                PIC 9(6)       COMP.
           03  WV-CHKZ                 PIC 9.
           03  WV-CHGEW                PIC 9(6)       COMP.
           03  WV-BZKEY.
               05 WV-BZSRT             PIC 9999       COMP.
               05 WV-BZNUM             PIC 9999       COMP.
               05 WV-BZZEIT            PIC 9999       COMP.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WD-X                    PIC X.
           03  WHT-SATZ                          OCCURS 15.
               05 WHT-NUM              PIC 999.
               05 WHT-RPBEZ            PIC X(27).
               05 WHT-PRZ              PIC 999.
               05 FILLER               PIC XX.
               05 WHT-DAT              PIC 9(6).
               05 WHT-ZEIT             PIC X(5).
      *----------------------------------------------> Command-Line <-
           03  WD-UPON                 PIC X(30).
           03  RESULT                  PIC 99      COMP-X.
           03  FUNKT                   PIC 99      COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99      COMP-X VALUE 0.
               05 PAR                  PIC X(40)   VALUE SPACE.
      *--------------------------------> fÅr Lieferscheinarchivierung <-
           03  WV-ARNUM                PIC 9(4)   COMP   VALUE ZERO.
           03  WM-KTONR                PIC 9(6)   COMP   VALUE ZERO.
           03  VDU-AB                  PIC 99     COMP.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  WM-EAN                  PIC 9(8)   VALUE ZERO.
           03  WT-EAN REDEFINES WM-EAN.
               05  WT-7                PIC 9      OCCURS 8 INDEXED IX.
      *-----------------------------------------> Feiertagsermittlung <-
           03  WO-TAGE                 PIC 99         COMP  OCCURS 31.
           03  WT-FTAG.
               05  FILLER              PIC 9(4)    COMP   OCCURS 4.
               05  FILLER              PIC 9(4)    COMP   VALUE 0101.
               05  FILLER              PIC 9(4)    COMP   VALUE 0106.
               05  FILLER              PIC 9(4)    COMP   VALUE 0501.
               05  FILLER              PIC 9(4)    COMP   VALUE 0815.
               05  FILLER              PIC 9(4)    COMP   VALUE 1026.
               05  FILLER              PIC 9(4)    COMP   VALUE 1101.
               05  FILLER              PIC 9(4)    COMP   VALUE 1208.
               05  FILLER              PIC 9(4)    COMP   VALUE 1225.
               05  FILLER              PIC 9(4)    COMP   VALUE 1226.
           03  WF-FTAG REDEFINES
               WT-FTAG                 PIC 9(4)    COMP  OCCURS 13.
           03  WK-FTAG                 PIC 9(6)    COMP  OCCURS 20.
           03  WK-JAHR                 PIC 9(4)    COMP.
           03  IY                      PIC 99        COMP.
           03  WI-TG                   PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WH-ABDAT                PIC 9(8)      COMP.
      *---------------- AUS PANLAG
           03  WM-TAG                  PIC S9(7).
           03  WR-TG REDEFINES WM-TAG.
               05 WR-TAG               PIC 9(4).
               05 WR-WOTG              PIC 99.
               05 WR-DIF               PIC S9.
           03  WD-JAHR                 PIC 9999.
           03  WV-JAHR                 PIC 9999     COMP.

       01  WT-DK     external.
           03  WR-TN                PIC X(10)  OCCURS 7.
           03  WT-KON               PIC 99     OCCURS 12.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING "PANEFAKT.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING "PANELFS.DAT "  WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "PANDEBIT.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  CALL "CADECL" USING "PANESTAT.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON SUMMEN.
       A.  CALL "CADECL" USING "PANESUMM.DAT" WH-CREG.
       Z.  EXIT.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON LAGER.
       A.  CALL "CADECL" USING "PANELAG.DAT "  WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON DAUER.
       A.  CALL "CADECL" USING "PANEBEST.DAT" WH-CREG.
       DECL-Q SECTION.         USE AFTER ERROR PROCEDURE ON ARCHIV.
       A.  CALL "CADECL" USING WH-ARCHIV      WH-CREG.
       DECL-R SECTION.         USE AFTER ERROR PROCEDURE ON BACKZHB.
       A.  CALL "CADECL" USING "BZ.DAT      " WH-CREG.
       DECL-S SECTION.         USE AFTER ERROR PROCEDURE ON BACKZET.
       A.  CALL "CADECL" USING "BACKZETT.DAT" WH-CREG.
       Z.  EXIT.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WH-PG = 8 PERFORM PRUEF    GO X.
           IF WH-PG = 2 PERFORM UEBFAK   GO X.
           IF WH-PG = 4 PERFORM UEBFAK   GO X.
           IF WH-PG = 6 PERFORM UEBFAK   GO X.
           IF WH-PG = 7 PERFORM UEBFAK   GO X.
           IF WH-PG = 9 PERFORM UEBFAK   GO X.
       X.  MOVE WH-CREG TO WL-CREG.
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
       A.  DISPLAY " weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
       X.  CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ********************************************** loeschen Zeile 23 *
       LOE23 SECTION.
       A.  CALL "CAUP" USING "1323012380000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) not = "LPT" GO Z.
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
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE SPACE TO DRA-SATZ.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************** DruckerrÅckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU not = 1 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ****** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhîhe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           PERFORM DRU-OK.
           IF WH-DRUNAM(1:3) = "LPT" OPEN OUTPUT DRUCKER
           else OPEN EXTEND DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
           IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           IF WM-DRU = 1 GO X.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
       X.  MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ***************************************** Mengenkommaumwandlung *
       MG SECTION.
       A.  EVALUATE WF-NK
               WHEN 2 DIVIDE 100 INTO WH-ANZ
                   MOVE WH-ANZ TO WD-ANZ2 WD-MGC
                   MOVE WD-MGC TO WD-MG
                   MOVE WD-ANZ2 TO WD-MENGE
               WHEN 1 DIVIDE 10 INTO WH-ANZ
                   MOVE WH-ANZ TO WD-ANZ1 WD-MGB
                   MOVE WD-MGB TO WD-MG
                   MOVE WD-ANZ1 TO WD-MENGE
               WHEN 0 MOVE WH-ANZ TO WD-ANZ WD-MGA
                   MOVE WD-MGA TO WD-MG
                   MOVE WD-ANZ TO WD-MENGE
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      ************************* Dauerlieferscheine / Uebernahme Fakt. *
       UEBFAK SECTION.
       A.  MOVE WH-DATUM TO WV-DATUM WC-DATUM.
           PERFORM DATDREH.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               INITIALIZE WHT-SATZ(WX).
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE WH-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE 0 TO WY.
           EVALUATE WH-PG
               WHEN 2 DISPLAY "alle Lieferscheine bis einschl.:" AT 0510
                               "werden Åbernommen." AT 0710
                      MOVE WH-DATUM TO WZ-DATUM GO B
               WHEN 4 DISPLAY "     alle Dauerbestellungen fÅr:" AT 0510
                        "werden zu Lieferscheinen umgewandelt." AT 0714
               WHEN 6 DISPLAY "          alle Lieferschein fÅr:" AT 0510
                              "werden gedruckt." AT 0719
               WHEN 7 DISPLAY "Back- und Tourenaufstellung fÅr:" AT 0510
               IF WM-KO = 3 and WE-PANIS not = 1
                   DISPLAY "ACHTUNG! " with highlight foreground-color 4
                      AT 1310 "Backzettel fÅr öbertragung erfordert"
                         with highlight foreground-color 2
                      "auch GÑrverzîgerliste fÅr nÑchsten Arbeitstag!"
                          with highlight foreground-color 2 AT 1410
               WHEN = 9 DISPLAY "Druck Kommissionsscheine fÅr:" AT 0513.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WM-DATUM).
           ADD 1 TO WA-DAT.
           DIVIDE 7 INTO WA-DAT GIVING WH-NUM REMAINDER WO-TGN.
           IF WO-TGN = 0 ADD 1 TO WO-TGN WA-DAT.
           ADD 1 TO WO-TGN.
           MOVE WR-TN(WO-TGN) TO WK-TAG WO-TAG.
           COMPUTE WZ-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT)
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE WX-DATUM TO WV-DATUM.
           IF WE-WO(2) = 0 DISPLAY "LFDRU.LST" with highlight AT 0369
                      else DISPLAY "Direktdruck" with highlight AT 0365.
           PERFORM OB-FEIERTAG.
       B.  DISPLAY "<ret>= Datum, <esc>= Abbruch, <ret-leer>= wiederhole
      -        "n" AT 2301.
           IF WH-PG = 7
               DISPLAY "<#>= Dampflliste + GÑrverzîgererliste" AT 2401.
           MOVE WV-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT 0543.
           DISPLAY WR-TN(WO-TGN + 1) with highlight foreground-color 3
                AT 0553.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0105436006" WH-CREG.
           MOVE WX-TASTE TO WM-TA.
           IF KIST DISPLAY "Dampflliste" with highlight AT 0360
              ELSE CALL "CAUP" USING "1303600380" WH-CREG.
           IF ESC GO Z.
           IF WZ-DATUM = 0 GO B.
           MOVE WX-DATUM TO WV-DATUM LF-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT 0543.
           DISPLAY WR-TN(WO-TGN + 1) with highlight foreground-color 3
                AT 0553.
           IF WV-DATUM not > WM-DATUM; IF WH-PG = 4
               DISPLAY "Datum richtig?" AT 2401 PERFORM WEITER
           else IF WH-PG > 4
               DISPLAY "Datum muss grîsser sein!" AT 2401
               PERFORM WEITER GO B.
           MOVE 0 TO LF-TOUR LF-SUB LF-KTONR LF-NUM LF-ARNUM
               WV-LFNUM WV-KTONR WV-DAT.
           IF WV-DATUM > WE-MAXDAT DISPLAY "Datum zu gro·" AT 2401
               PERFORM WEITER GO B.
           IF WH-PG = 4 MOVE WV-DATUM TO WZ-DATUM
               PERFORM DAUERLFS GO Z.
       C.  DISPLAY "<Entf>= L., <ret>= Start, <esc>= Abbruch" AT 2301.
           MOVE 2342 TO VDU-LP.
           IF WM-KO = 2 DISPLAY ", <tab>= nur EAN-Lfsch." AT 0000.
           IF WH-PG NOT = 7 DISPLAY "<ret-leer>= alle, <Kd.Nr.+ret>= nur
      -        " diese Kundennr." AT 2401
               MOVE 2453 TO VDU-LP
               IF WH-PG = 6 DISPLAY "ab Kundennr. <       >" AT 2433
                            MOVE 2447 TO VDU-P.
           CALL "CAUP" USING "0000005005" WH-CREG.
           IF ENTF PERFORM STORNO GO Z.
           IF WOLI or AUF GO A.
           IF ESC GO Z.
           IF TABL SET RET TO TRUE.
           IF NOT RET GO C.
           IF WH-PG = 9 MOVE 7 TO WH-PG GO Z.
           IF WH-PG = 7 PERFORM TOURBACK
              MOVE 1 TO WH-PG GO Z.
           IF NOT RET GO C.
           IF WH-NUM = 0 MOVE 2 TO LF-KTONR WV-KTONR
                    ELSE COMPUTE DE-KTONR = WH-NUM * 10 + 2
                         MOVE DE-KTONR TO LF-KTONR WV-KTONR.
           IF WH-NUM = 0; IF WH-PG = 2 GO E ELSE GO Z.
           MOVE 1 TO DE-FNR.
           READ DEBITOR IGNORE LOCK INVALID KEY GO C.
           MOVE DE-TOUR TO LF-TOUR.
           MOVE DE-SUB TO LF-SUB.
           IF WH-PG = 6 MOVE 2 TO WV-KTONR GO Z.
       E.  CLOSE DAUER.
      *-----------------------------------> Uebernahme Lieferscheine <-
       F.  OPEN I-O STATISTIK.
           IF WV-KTONR = 2 MOVE 0 TO LF-DATUM.
           OPEN EXTEND PROTOK.
           MOVE 0 TO PR-ARNUM wm-ktonr wv-arnum wk wz-zeilen.
           MOVE 61 TO VDU-AB.
           MOVE WV-DATUM TO PR-MENGE.                  *> im ersten Satz
           IF WM-KO = 3 WRITE PR-SATZ.
           START LFSCHEIN KEY > LF-AKEY INVALID PERFORM NO-REC GO X.
       K.  READ LFSCHEIN NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO K.
           IF WV-KTONR > 2 AND WV-KTONR NOT = LF-KTONR GO K.
           IF LF-DATUM > WV-DATUM GO X.
      *-----> Ende eines Lieferscheines <-> vorbereiten fÅr Lfsarchiv <-
           IF WV-LFNUM = 0 MOVE LF-NUM TO WV-LFNUM.
           IF WM-KTONR = 0 MOVE LF-KTONR TO WM-KTONR.
           IF LF-KTONR not = WM-KTONR or LF-NUM not = WV-LFNUM;
               IF WZ-ZEILEN > 0 PERFORM END-ACDRU
                   MOVE 0 TO WM-KTONR WK WZ-ZEILEN
                   MOVE LF-NUM TO WV-LFNUM
                   MOVE LF-KTONR TO WM-KTONR.
           MOVE LF-NUM TO WD-NUM.
           MOVE LF-KTONR TO FA-KTONR.
           IF LF-STAT = 94 or LF-STAT = 95
               MOVE 0 TO LF-MENGE(1) LF-MENGE(2) LF-MENGE(3).
           DIVIDE 10 INTO LF-KTONR GIVING WD-KTO.
           MOVE LF-DATUM TO WC-DATUM WZ-DATUM.
           COMPUTE WH-VON = LF-DATUM / 100.
           MOVE WH-VON TO WK-VON.
           MOVE WZ-JAHR TO ST-JAHR.
           MOVE LF-ARNUM TO WD-ART FA-ARNUM ST-ARNUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 1020.
           DISPLAY WD-KTO with highlight AT 1030.
           DISPLAY WD-ART with highlight AT 1038.
           DIVIDE 100 INTO LF-NUM GIVING WK-ANZ REMAINDER WK-ANZ.
           IF WK-ANZ > 90 DELETE LFSCHEIN GO K.
       L.  READ FAKTDAT INVALID PERFORM LOEFAK.
           IF ZUGRIF PERFORM BESETZT GO L.
           MOVE LF-DATUM TO WZ-DATUM.
           SET OX TO WZ-TAG.
           IF FA-MON = 0 DIVIDE 100 INTO WK-MON GIVING WH-VON
                         MOVE WH-VON TO FA-MON.
           IF WK-VON > FA-MON SET OX UP BY 31.
      *-----------------------------------------> Lf-Datum absichern <-
           ADD LF-MENGE(1) LF-MENGE(3) TO FA-MENGE(OX).
           ADD LF-MENGE(2) TO FA-RETOUR(OX).
           MOVE LF-FART TO FA-ART.
150812     MOVE LF-SGRP TO FA-SGRP.                   *> wg. Sortierung
150812     IF FA-SGRP > 0 MOVE LF-ARBEZ TO FA-ARBEZ.
           REWRITE FA-SATZ INVALID WRITE FA-SATZ.
           MOVE LF-ARNUM TO PR-ARNUM.
           MOVE LF-MENGE(1) TO PR-MENGE.
           MOVE LF-MENGE(2) TO PR-RETMG.
           IF WM-KO = 3 or WM-VER = 3 WRITE PR-SATZ
                PERFORM LFSARCHIV.
           DELETE LFSCHEIN.
           IF LF-STAT = 94 or LF-STAT = 95 GO K.     *> nicht lieferbar
      *------------------------------> Uebernahme in Tourenstatistik <-
           IF LF-TOUR > 20 GO K.
           COMPUTE ST-KTONR = LF-TOUR + 99999900.
       N.  READ STATISTIK INVALID MOVE 1 TO WX GO O.
           IF ZUGRIF PERFORM BESETZT GO N.
           GO P.
       O.  MOVE 0 TO ST-MENGEN(WX) ST-RET(WX)
                     ST-UMSATZ(WX) ST-GEWINN(WX).
           IF WX < 12 ADD 1 TO WX GO O.
       P.  MOVE WZ-MONAT TO WX.
           ADD LF-MENGE(1) LF-MENGE(3) TO ST-MENGEN(WX).
           ADD LF-MENGE(2) TO ST-RET(WX).
      *    subtract LF-MENGE(1) LF-MENGE(3) from ST-MENGEN(WX).
      *    subtract LF-MENGE(2) from ST-RET(WX).
           REWRITE ST-SATZ INVALID WRITE ST-SATZ.
           GO K.
       X.  PERFORM END-ACDRU.
           CLOSE STATISTIK PROTOK.
           OPEN I-O DAUER.
       Z.  EXIT.
      ******************************************* Feiertagsermittlung *
       FEIER SECTION.
       A.  COMPUTE WZ-DATUM = WK-JAHR * 10000 + 0315.
           MOVE 0 TO WZ.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           ADD 29 WX TO WZ.
           MOVE 3 TO WF.
           IF WF-FTAG(WI) > 0 ADD WR-WOTG TO WZ.
       C.  IF WZ > WT-KON(WF) SUBTRACT WT-KON(WF) FROM WZ
               ADD 1 TO WF GO C.
       D.  IF WF-FTAG(WI) NOT < 0 GO G.
           IF WZ < WR-WOTG SUBTRACT WZ FROM WR-WOTG
                           SUBTRACT 1 FROM WF
                           MOVE WT-KON(WF) TO WZ GO D
                      else SUBTRACT WR-WOTG FROM WZ
                           IF WZ = 0  SUBTRACT 1 FROM WF
                               MOVE WT-KON(WF) TO WZ.
       G.  MOVE WF TO WZ-MONAT.
           MOVE WZ TO WZ-TAG.
       Z.  EXIT.
      ********************************************* Sonntage ermitteln *
       OB-FEIERTAG SECTION.
       A.  MOVE WM-DATUM TO WS-DATUM.
           PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 20
               MOVE 0 TO WK-FTAG(IY).
           MOVE WV-DATUM TO WH-ABDAT.
           MOVE WH-ABDAT TO WZ-DATUM.
           MOVE WT-KON(WZ-MONAT) TO WI-TG.
           COMPUTE WK-JAHR = WV-DATUM / 10000.
           if wz-monat = 2
               divide 4 into wk-jahr giving wh-num remainder wi-tg
               if wi-tg = 0 move 29 to wi-tg
                       else move 28 to wi-tg.
      *-----------------------------------------> Feiertagsermittlung <-
           CALL "CAUP" USING "03DATUM" WH-CREG.
      *---> Ostermon. * Chr. Himmelfahrt * Pfingstmon. * Frohnleichnam *
           MOVE 1 TO WY.
           ADD 29 WX GIVING WZ PERFORM TG.
           ADD 67 WX GIVING WZ PERFORM TG.
           ADD 78 WX GIVING WZ PERFORM TG.
           ADD 88 WX GIVING WZ PERFORM TG.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 13
               MOVE WF-FTAG(WY) TO WZ-DATUM WK-FTAG(WY)
               MOVE WK-JAHR TO WZ-JAHR
               MOVE WZ-DATUM TO WK-FTAG(WY).
           PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > WI-TG
               MOVE WH-ABDAT TO WZ-DATUM
               MOVE IY TO WZ-TAG
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 13
                   or WZ-DATUM = WK-FTAG(WY) CONTINUE
               end-perform
               IF WY > 13 CALL "CAUP" USING "03DATUM" WH-CREG
                          IF WO-TGN = 7 MOVE 1 TO WO-TAGE(IY)
                                   else MOVE 0 TO WO-TAGE(IY)
                          end-if
               else MOVE 1 TO WO-TAGE(IY).
           MOVE WS-DATUM TO WM-DATUM.       *> RÅckstellung s. 1. Zeile
       Z.  EXIT.
      ********************************************* Tageermittlung ab *
       TG SECTION.
       A.  MOVE 3 TO WF.
       B.  IF WZ > WT-KON(WF) SUBTRACT WT-KON(WF) FROM WZ
               ADD 1 TO WF GO B.
           MULTIPLY WF BY 100 GIVING WF-FTAG(WY).
           ADD WZ TO WF-FTAG(WY).
           ADD 1 TO WY.
       Z.  EXIT.
      *************************** Storno eines falsch Åbernommen Tages *
       STORNO SECTION.
       A. CALL "CAUP" USING "0715150350000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lîschung öbernahme " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Alle Werte vom:          werden gelîscht! < >"
               AT VDU-LP.
           ADD 219 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       C.  DISPLAY "<Entf>= Start, <esc >= Abbruch" AT 2301.
           CALL "CAUP" USING "1002460000" WH-CREG.
           IF ESC GO X.
           IF NOT ENTF GO C.
           COMPUTE WH-VON = WV-DATUM / 100.
           MOVE WH-VON TO WK-VON.
           OPEN I-O STATISTIK.
           MOVE LOW-VALUES TO FA-SATZ.
           START FAKTDAT KEY NOT < FA-KEY INVALID GO W.
280711*----------------------------------------------> Start Lîschung <-
       G.  READ FAKTDAT NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF WM-DATUM = 20110728 and FA-KTONR = 1652 GO G.
           DISPLAY FA-KTONR AT 1820 " " FA-ARNUM.
           MOVE WV-DATUM TO WZ-DATUM.
           SET OX TO WZ-TAG.
           IF FA-MON = 0 DIVIDE 100 INTO WK-MON GIVING FA-MON.
           IF WK-VON > FA-MON SET OX UP BY 31.
           INITIALIZE LF-SATZ.
      *-----------------------------------------> Lf-Datum absichern <-
           MOVE FA-MENGE(OX) TO LF-MENGE(1).
           MOVE FA-RETOUR(OX) TO LF-MENGE(2).
           MOVE 0 TO FA-MENGE(OX) FA-RETOUR(OX).
           IF LF-MENGE(1) = 0 and LF-MENGE(2) = 0 GO G.
           REWRITE FA-SATZ.
           MOVE FA-KTONR TO DE-KTONR.
           MOVE 1 TO DE-FNR.
       K.  READ DEBITOR IGNORE LOCK INVALID GO G.
           MOVE DE-TOUR TO LF-TOUR.
           IF LF-TOUR > 20 GO G.
           COMPUTE ST-KTONR = LF-TOUR + 99999900.
       N.  READ STATISTIK INVALID GO PA.
           IF ZUGRIF PERFORM BESETZT GO N.
       P.  MOVE WZ-MONAT TO WX.
           SUBTRACT LF-MENGE(1) FROM ST-MENGEN(WX).
           SUBTRACT LF-MENGE(2) FROM ST-RET(WX).
           REWRITE ST-SATZ INVALID WRITE ST-SATZ.
       PA.
      *--------------------------------------------> Lieferscheinzeilen
           INITIALIZE DA-SATZ.
           MOVE FA-KTONR TO DA-KTONR.
           MOVE FA-ARNUM TO DA-ARNUM.
           READ DAUER INVALID NEXT SENTENCE.
           MOVE DA-ARNUM TO AR-NUM.
       Q.  READ ARTIKEL IGNORE LOCK INVALID NEXT SENTENCE.
           MOVE AR-GRM TO WF-GRM.
           DISPLAY AR-BEZ with highlight AT 1110.
           MOVE AR-BEZ TO LF-BEZ.
           IF DA-DAT(2) NOT = 0; IF WV-DATUM NOT < DA-DAT(2);
               IF WV-DATUM NOT > DA-DAT(3) GO G.
           COMPUTE LF-NUM = WZ-TAG * 100.
           MOVE 0 TO LF-PREIS.
           IF WV-DATUM < AR-ADAT MOVE AR-VKP TO LF-PREIS
                            ELSE MOVE AR-AVKP TO LF-PREIS
               IF LF-PREIS = 0 MOVE AR-VKP TO LF-PREIS.
       E.  MOVE DA-KTONR TO LF-KTONR.
           MOVE DA-ARNUM TO LF-ARNUM.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE AR-BEZ   TO LF-BEZ.
           MOVE DA-EAN   TO LF-EAN.
           MOVE DE-TOUR  TO LF-TOUR.
           MOVE DE-SUB   TO LF-SUB.
           MOVE DE-FAKART TO LF-FART.
           MOVE AR-GRM   TO LF-GRM.
           MOVE AR-SGRP TO LF-SGRP.
           IF AR-SGRP > 0 MOVE AR-BEZ TO LF-ARBEZ.
           IF LF-MENGE(1) = 0 GO G.
           WRITE LF-SATZ INVALID KEY GO G.
           DIVIDE 10 INTO LF-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 1510.
           GO G.
       W.  CLOSE STATISTIK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       DAMPFL SECTION.
       A.  MOVE 0 TO AR-GVZ WR.
           START ARTIKEL KEY > AR-BKEY INVALID GO Z.
       C.  READ ARTIKEL NEXT IGNORE LOCK AT END GO Z.
           IF AR-NUM > 999 OR AR-GVZ > 2 GO Z.
           MOVE AR-NUM TO LF-ARNUM.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE 0 TO SU-NUM WZ-SEITE WZ-ZEILEN WK-D.
           START LFSCHEIN KEY NOT < LF-ARNUM INVALID GO C.
       E.  READ LFSCHEIN NEXT IGNORE LOCK AT END MOVE 0 TO LF-ARNUM.
           IF LF-ARNUM NOT = AR-NUM;
               IF WR = 1 WRITE SU-SATZ;
               END-IF
               MOVE 0 TO WR GO C.
           MOVE LF-GRM TO WF-GRM.
      *------------------------> BÑckereiware in Konditorei einzÑhlen <-
           IF WF-TB > 3 MOVE 2 TO WF-TB.
           IF LF-DATUM NOT = WV-DATUM GO E.
       G.  IF SU-NUM = LF-ARNUM GO I.
           IF WR = 1 WRITE SU-SATZ.
           MOVE 0 TO WR.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 23
               MOVE 0 TO SU-MG(WX).
           MOVE LF-ARNUM TO SU-NUM SU-ANUM.
           MOVE LF-BEZ TO SU-BEZ.
           MOVE WF-MEH TO SU-MEH.
           MOVE WF-NK TO SU-NK.
           MOVE WF-TB TO SU-TB.
           IF WF-GRP > 250 MOVE 250 TO WF-GRP.
           MOVE WF-GRP TO SU-GRP SU-SRT WE.
           MOVE AR-GVZ TO SU-GVZ.
           DISPLAY LF-BEZ AT 1010.
           IF WM-KO = 3 COMPUTE SU-GRP = SU-TB * 1000 + WF-GRP;
              IF (WE not = 0 and WT-TGRP(WE) not = 0)
                   MOVE WT-TGRP(WE) TO WF-GRP
              end-if
              COMPUTE SU-SRT = SU-TB * 1000 + WF-GRP.
       I.  MOVE LF-MENGE(1) TO WH-MENGE.
           ADD WH-MENGE TO SU-MG(23).
           MOVE 1 TO WR.
           GO E.
       Z.  EXIT.
      ******************************************* Teigruppensortierung *
       TG-SORT SECTION.
       A.  MOVE LOW-VALUE TO WT-TG.
           MOVE 37 TO WH-KEY.
       C.  READ KONSTANT IGNORE LOCK INVALID GO E.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 100
               MOVE KO-TGRP(WE) TO WT-TGRP(WE).
       E.  MOVE 38 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO G.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 100
               MOVE KO-TGRP(WE) TO WT-TGRP(WE + 100).
       G.  MOVE 39 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO Z.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 50
               MOVE KO-TGRP(WE) TO WT-TGRP(WE + 200).
       Z.  EXIT.
      ************************************************** Artikel lesen *
       READ-ARTIK SECTION.
       A.  IF LF-ARNUM = AR-NUM GO Z.
           MOVE LF-ARNUM TO AR-NUM RE-NUM.
           READ ARTIKEL IGNORE LOCK INVALID MOVE LF-GRM TO WF-GRM GO Z.
           MOVE AR-GRM TO WF-GRM.
           DISPLAY AR-BEZ AT 1010.
           READ REZEPT IGNORE LOCK INVALID INITIALIZE RE-SATZ.
           go z.                       *> vorÅbergehend nicht durchgef.
           IF WE-LFS = 0 GO Z.
      *---------------------------------------> Mengenumrechnung Kern <-
           MOVE 0 TO WH-FAKT WH-NUM.
           PERFORM VARYING WF FROM 25 BY -1 UNTIL WF = 1
               MOVE AR-BEZ(WF:4) TO WH-MCODE(1:)
               IF AR-BEZ(WF:1) is NUMERIC and WH-MCODE(2:3) = "er "
                   MOVE AR-BEZ(WF:1) TO WH-NUM(13:1)
                   IF WF > 1 ADD -1 TO WF
                       IF AR-BEZ(WF:1) is NUMERIC
                           MOVE AR-BEZ(WF:1) TO WH-NUM(12:1)
                       end-if
                       EXIT PERFORM.
           MOVE WH-NUM TO WH-FAKT.
       Z.  EXIT.
      ********************************** Back- u. Tourzettelerstellung *
       TOURBACK SECTION.
       A.  CLOSE DAUER.
           DELETE FILE SUMMEN.
           OPEN I-O SUMMEN.
           OPEN INPUT REZEPT.
           PERFORM TG-SORT.
           MOVE LOW-VALUE TO WT-ERFTAB.
           IF WM-TA = 6 PERFORM DAMPFL GO F.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE 0 TO SU-NUM WZ-SEITE WZ-ZEILEN WK-D WR LF-ARNUM.
           START LFSCHEIN KEY NOT < LF-ARNUM INVALID PERFORM NO-REC
               GO Y.
       B.  READ LFSCHEIN NEXT IGNORE LOCK AT END GO F.
           PERFORM READ-ARTIK.
      *------------------------> BÑckereiware in Konditorei einzÑhlen <-
           IF WF-TB > 3 MOVE 2 TO WF-TB.
           IF WF-TB = 0 AND WF-GRP = 0 GO B.
           IF LF-DATUM NOT = WV-DATUM GO B.
           IF LF-TOUR > 25 GO B.
       C.  IF SU-NUM = LF-ARNUM GO E.
           IF WR = 1 WRITE SU-SATZ.
           MOVE 0 TO WR.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 26
               MOVE 0 TO SU-MG(WX).
           MOVE LF-ARNUM TO SU-NUM SU-ANUM.
           MOVE LF-BEZ TO SU-BEZ.
           MOVE WF-MEH TO SU-MEH.
           MOVE WF-NK TO SU-NK.
           MOVE WF-TB TO SU-TB.
           IF AR-TB > 2 MOVE SU-BEZ TO SU-KONDIT
                   else MOVE LOW-VALUE TO SU-KONDIT.
           IF WF-GRP > 250 MOVE 250 TO WF-GRP.
           MOVE WF-GRP TO SU-GRP SU-SRT WE.
           MOVE 0 TO SU-GVZ SU-TGGEW.
      *-------------------------> Teiggruppenumsortierung lt. KO-TGRP <-
           IF WM-KO = 3 or WM-KO = 1
              COMPUTE SU-GRP = SU-TB * 1000 + WF-GRP;
              IF (WE not = 0 and WT-TGRP(WE) not = 0)
                   MOVE WT-TGRP(WE) TO WF-GRP
              end-if
              COMPUTE SU-SRT = SU-TB * 1000 + WF-GRP.
       E.  IF LF-TOUR = 0 MOVE 20 TO LF-TOUR.
           DIVIDE 100 INTO LF-NUM GIVING WK-ANZ REMAINDER WK-ANZ.
           IF WK-ANZ > 90 MOVE 22 TO LF-TOUR.
           MOVE LF-TOUR TO WT-RKEY(LF-TOUR).
           MOVE LF-MENGE(1) TO WH-MENGE.
           IF WH-FAKT not = 0 COMPUTE WH-MENGE = WH-MENGE * WH-FAKT.
           ADD WH-MENGE TO SU-MG(LF-TOUR).
           ADD WH-MENGE TO SU-MG(23).
           MOVE LF-NUM TO WD-KZ.
           IF WD-KZ = 1 or WD-KZ = 2 ADD WD-KZ 24 GIVING WX
                                else MOVE 24 TO WX.
           ADD WH-MENGE TO SU-MG(WX).
           MOVE 1 TO WR.
      *---------------------------------------> Teiggewicht ermitteln <-
           COMPUTE WH-WERT = WH-MENGE * RE-GEW / 1000.
           EVALUATE WF-NK
               WHEN 1 DIVIDE 10 INTO WH-WERT
               WHEN 2 DIVIDE 100 INTO WH-WERT.
           ADD WH-WERT TO SU-TGGEW.
           GO B.
      *-----------------------------------------> alle Lfsch. gelesen <-
       F.  IF WR = 1 WRITE SU-SATZ.
           MOVE 1 TO WI.                          *> Backzettel
           IF WM-KO NOT = 3 or WE-PANIS not = 0
               PERFORM BAERZ GO G.
           IF WM-TA = 6 MOVE 3 TO WI              *> GÑrverzîgererliste
                        PERFORM BAERZ
                        MOVE 4 TO WI              *> Dampfliste
                        PERFORM BAERZ
                   else OPEN INPUT LAGER
                        PERFORM BAERZ
                        CLOSE LAGER
                        MOVE 2 TO WI              *> Erzeugungsliste
                        PERFORM BAERZ.
       G.  IF WE-LFS = 1 GO X.                    *> Kern ohne Touren
           MOVE "TOUREN.LST" TO WH-DRUNAM.
           DELETE FILE DRUCKER.
       K.  IF WZ-ZEILEN NOT = 0 PERFORM END-DRU.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
           IF WM-TA = 6 GO X.
      *------------------------------------------------> Tourenzettel <-
           IF WM-KO = 2 GO X.
           MOVE 1 TO LF-TOUR WR.
           IF WE-PANIS = 1 MOVE 2 TO WI GO L.
           IF WM-KO = 3 OR WM-VER = 0 GO O.
       L.  IF WT-RKEY(LF-TOUR) NOT = 0 MOVE 0 TO WT-RKEY(LF-TOUR) GO M.
           IF LF-TOUR < 20 ADD 1 TO LF-TOUR GO L.
           GO X.
       M.  ADD LF-TOUR 10 GIVING WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID
               MOVE "Tour nicht angelgt" TO KO-TOUR.
           MOVE 1 TO SU-NUM SU-TB.
           MOVE 9 TO WK-D.
           START SUMMEN KEY NOT < SU-KEY INVALID KEY GO X.
       N.  READ SUMMEN NEXT IGNORE LOCK AT END GO K.
           IF SU-MG(LF-TOUR) = 0 GO N.
           IF WK-D = 9 MOVE SU-TB TO WK-D.
           IF WK-D NOT = SU-TB AND WZ-ZEILEN > 0 MOVE 88 TO WZ-ZEILEN
               MOVE SU-TB TO WK-D.
           IF WM-KO = 2 AND SU-TB > 1 GO K.
           MOVE SU-MG(LF-TOUR) TO SU-MG(23).
           PERFORM BTDRU.
           GO N.
      *-----------------------------------> Tourenliste fÅr 20 Touren <-
       O.  DISPLAY "<ret>= weiterlisten, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0023370000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO O.
           MOVE 1 TO SU-NUM SU-TB WR.
           MOVE 12 TO WK-D.
           START SUMMEN KEY NOT < SU-KEY INVALID KEY GO X.
       P.  READ SUMMEN NEXT IGNORE LOCK AT END GO X.
           MOVE 0 TO WS-ANZ.
           IF WK-D = 12 MOVE SU-TB TO WK-D.
           IF WK-D NOT = SU-TB AND WZ-ZEILEN > 0 MOVE 88 TO WZ-ZEILEN
               MOVE SU-TB TO WK-D.
       R.  IF WZ-ZEILEN > 41 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO T.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"0008" TO WH-PX(2).
           MOVE "TOUREN.LST" TO WH-DRUNAM.
           MOVE WE-DRU(3) TO WM-DRU.
           IF WM-DRU = 1 MOVE 8 TO WH-P.
           PERFORM BEG-DRU.
           IF WM-DRU = 1 MOVE 0 TO WZ-SCHALT
                   else MOVE 1 TO WZ-SCHALT.
           MOVE "T o u r e n l i s t e   fÅr den:" TO DRI-STR.
           MOVE VDU-DATUM TO WK-DAT.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
           MOVE WK-DS(6:) TO DRA-SATZ(116:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRI-STR(1:).
           MOVE "-grÅnƒƒƒ-rotƒƒ-blauƒƒ-gelbƒƒ-grauƒƒ-schwƒƒ-wei·ƒ"
               TO DRI-STR(28:48).
           PERFORM DRUCK.
           MOVE "Art.B e z e i c h n u n g" TO DRI-STR.
           MOVE 35 TO WI.
           PERFORM VARYING WH-TNR FROM 1 BY 1 UNTIL WH-TNR > 15
               MOVE WH-TNR TO WD-TNR
               MOVE WD-TNR TO DRA-SATZ(WI:)
               SET WI UP BY 7.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRI-STR.
           MOVE 27 TO WR.
           PERFORM FAHRER VARYING WH-TNR FROM 1 BY 1 UNTIL WH-TNR > 15.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       T.  MOVE SU-NUM TO DRH-ARNUM.
           MOVE SU-BEZ TO DRH-ARBEZ.
           MOVE SU-NK TO WF-NK.
           MOVE SU-TB TO WF-TB.
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 15
               MOVE SU-MG(WR) TO WH-ANZ
               ADD WH-ANZ TO WS-ANZ
               IF WH-ANZ NOT = 0 PERFORM MG
                   MOVE WD-MG TO DRI-MG(WR).
           MOVE DRI-SATZ TO WT-TX.
           DISPLAY WT-TX with SIZE 80 AT 1001.
           PERFORM DRUCK.
           MOVE ALL "-  -  " TO DRI-STR.
           MOVE WS-ANZ TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MG TO DRI-SATZ(11:7).
           PERFORM DRUCK.
           GO P.
       X.  IF WZ-ZEILEN NOT = 0 PERFORM END-DRU.
           IF WM-KO = 3 MOVE 20 TO WH-PG.
       Y.  CLOSE SUMMEN.
           DELETE FILE SUMMEN.
           OPEN I-O DAUER.
           MOVE 0 TO LF-ARNUM LF-NUM LF-TOUR LF-SUB LF-KTONR
               WV-LFNUM WV-KTONR.
           MOVE WV-DATUM TO LF-DATUM.
       Z.  EXIT.
      ******************************************************************
       BAERZ SECTION.
      *---------------> 1 = Backzettel (alle Produkte f. Ausliefg.)
      *---------------> 2 = Erzeungsliste ohne GÑrverzîgererartikel
      *---------------> 3 = GÑrverzîgererliste     SU-GVZ/AR-TK = 1
      *---------------> 4 = Dampflliste            SU-GVZ/AR-TK = 2
       A.  EVALUATE WI
               WHEN 1 MOVE "BACKZ.LST"  TO WH-DRUNAM
               WHEN 2 MOVE "ERZEUG.LST" TO WH-DRUNAM
               WHEN 3 MOVE "GAERVZ.LST" TO WH-DRUNAM
               WHEN 4 MOVE "DAMPFL.LST" TO WH-DRUNAM.
           DELETE FILE DRUCKER.
           MOVE LOW-VALUES TO SU-KONDIT.
           MOVE 0 TO WR SU-SRT SU-NUM SU-ANUM SU-GRP SU-TB
                     WS-BET WK-S WS-GEW.
           IF WM-KO = 3 and WE-PANIS = 0;
               EVALUATE WI
                   WHEN 1 PERFORM HB-OUT
                   WHEN 2
                   WHEN 3 PERFORM LOE-BACK
                          PERFORM AUTO-HB.
           MOVE 9999 TO WK-D.
           START SUMMEN KEY > SU-AKEY INVALID PERFORM NO-REC
              MOVE 6 TO WM-TA GO X.
       C.  IF WR = 9 GO M.
           READ SUMMEN NEXT IGNORE LOCK AT END MOVE 9876 TO WK-D
               MOVE 9 TO WR GO E.
           IF WM-KO = 3 OR WM-VER = 3;
               IF (WI = 2 AND SU-GVZ = 1)
               OR (WI = 3 AND SU-GVZ NOT = 1)
               OR (WI = 4 AND SU-GVZ NOT = 2) GO C.
       E.  DIVIDE 1000 INTO SU-GRP GIVING WF-TB REMAINDER WF-GRP.
           IF WF-GRP = 0 GO C.
           IF WM-KO = 3 AND WM-TA NOT = 6;
               IF WI > 1 AND WF-TB > 2 MOVE 9876 TO WK-D
               MOVE 9 TO WR.
           IF WK-D = 9999 MOVE SU-GRP TO WK-D WK-TG
                          MOVE SU-SRT TO WK-SRT
               MOVE SU-TB TO WK-S.
130208     IF WK-D = SU-GRP GO K.
      *    IF WK-D = SU-GRP and SU-TB = WK-S GO K.
      *------------------------------------> Summe Teigruppe ausgeben <-
           MOVE 0 TO WZ-SCHALT.

           move 1 to wk-s.                  *> ob bei anderen auch ok?

           MOVE WK-TG TO DRM-TGRP.
           IF WK-S = 1 MOVE WS-BET TO DRM-GEW
                  else MOVE WS-BET TO DRM-STK.
           IF WM-KO = 3 and WE-PANIS not = 1 MOVE WS-BET TO DRM-GEW
               MOVE 1 TO WZ-SCHALT
               IF WM-OPEN = 1 PERFORM T-DRUCK.
           IF WI = 2 or WI = 3 PERFORM HB-BACK.     *> ob HB-Backzettel
           IF WM-OPEN = 1; IF WI = 1 and WM-KO not = 3 PERFORM T-DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE 0 TO WS-BET WS-GEW.
           MOVE SU-GRP TO WK-D WK-TG.
           MOVE SU-SRT TO WK-SRT.
           IF WM-TA = 6 MOVE SU-TB TO WK-S.
060907*    IF SU-TB NOT = WK-S MOVE 65 TO WZ-ZEILEN
      *        MOVE SU-TB TO WK-S.
           IF WR = 9 GO M.
       K.  PERFORM BTDRU.
           GO C.
       M.  IF WZ-ZEILEN NOT = 0 PERFORM END-DRU.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
           IF WM-KO = 3 and WE-PANIS not = 1
               EVALUATE WI
                   WHEN 2
                   WHEN 3 PERFORM BACKDATEI.
       X.  CLOSE REZEPT.
       Z.  EXIT.
      ************************ automatisch fÅr HB- Backzettel ausgeben *
       AUTO-HB SECTION.
       A.  MOVE 1 TO WX.
       B.  MOVE WHT-NUM(WX) TO WK-TG.
           IF WK-TG = 0 GO Z.
           MOVE WT-TGRP(WK-TG) TO WK-SRT.
           ADD WK-TG 3000 GIVING RE-NUM.
       G.  READ REZEPT IGNORE LOCK INVALID GO W.
           MOVE WI TO BZ-WI.
           MOVE WK-SRT TO BZ-SRT.
           MOVE RE-NUM TO BZ-NUM.
           MOVE 9999 TO BZ-ZEIT.
           MOVE RE-BEZ TO WT-TX.
           PERFORM VARYING IP FROM 1 BY 1 UNTIL IP > 21
               IF WT-TX(IP:1) = "·" MOVE "s" TO WT-TX(IP:1)
                   MOVE WT-TX TO BZ-RPBEZ
                   MOVE WT-TX(IP:) TO BZ-RPBEZ((IP + 1):)
                   MOVE BZ-RPBEZ TO WT-TX.
           CALL "CAUP" USING "20CONV25" WH-CREG.
           CALL "CAUP" USING "21CONV25" WH-CREG.
           MOVE WT-TX TO BZ-RPBEZ.
           IF RE-KNET = 0 MOVE 1 TO BZ-KNET
               else MOVE RE-KNET TO BZ-KNET.
           COMPUTE BZ-GRAMM = WHT-PRZ(WX) * 1000.
           MOVE RE-GEW TO BZ-CHARGEW.
           MOVE RE-CHKZ TO BZ-CHKZ.
           MOVE RE-CHMAX TO BZ-CHMAX.
           MOVE RE-CHGEW TO BZ-CHGEW.
           MOVE 0 TO BZ-ZUGANG.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
       W.  IF WX not = 15 ADD 1 TO WX GO B.
       Z.  EXIT.
      ************************************ fÅr HB- Backzettel ausgeben *
       HB-BACK SECTION.
       A.  IF WM-KO not = 3 or WE-PANIS = 1 GO Z.
           IF WS-BET = 0 GO Z.
           IF WK-TG = 83 or 23 or 40 or 41 or 42 GO Z.
           ADD WK-TG 3000 GIVING RE-NUM.
       G.  READ REZEPT IGNORE LOCK INVALID
               MOVE " F" TO DRM-FEHL GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE WI TO BZ-WI.
           MOVE WK-SRT TO BZ-SRT.
           MOVE RE-NUM TO BZ-NUM.
           MOVE 9999 TO BZ-ZEIT.
           MOVE BZ-KEY TO WV-BZKEY.
           READ BACKZET INVALID INITIALIZE BZ-SATZ
               MOVE WI TO BZ-WI
               MOVE WV-BZKEY TO BZ-KEY.
           MOVE RE-BEZ TO WT-TX.
           PERFORM VARYING IP FROM 1 BY 1 UNTIL IP > 21
               IF WT-TX(IP:1) = "·" MOVE "s" TO WT-TX(IP:1)
                   MOVE WT-TX TO BZ-RPBEZ
                   MOVE WT-TX(IP:) TO BZ-RPBEZ((IP + 1):)
                   MOVE BZ-RPBEZ TO WT-TX.
           CALL "CAUP" USING "20CONV25" WH-CREG.
           CALL "CAUP" USING "21CONV25" WH-CREG.
           MOVE WT-TX TO BZ-RPBEZ.
      *    COMPUTE BZ-GRAMM = BZ-GRAMM + (WS-BET * 1000).
           COMPUTE BZ-GRAMM = BZ-GRAMM + WS-GEW.         *> Teiggewicht
           IF RE-KNET = 0 MOVE 1 TO BZ-KNET
               else MOVE RE-KNET TO BZ-KNET.
           MOVE RE-GEW TO BZ-CHARGEW.
           MOVE RE-CHKZ TO BZ-CHKZ.
           MOVE RE-CHMAX TO BZ-CHMAX.
           MOVE RE-CHGEW TO BZ-CHGEW.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
       Z.  EXIT.
      **************************************** Backzetteldatei lîschen *
       LOE-BACK SECTION.
       A.  IF WI = 3 DELETE FILE BACKZET
                     OPEN I-O BACKZET
                     GO X.
           OPEN I-O BACKZET.
           MOVE LOW-VALUE TO BZ-KEY.
           MOVE 2 TO BZ-WI.
           DISPLAY "-- Bitte warten! --" with highlight BLINK AT VDU-LP.
           START BACKZET KEY not < BZ-AKEY INVALID GO X.
       C.  READ BACKZET NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BZ-WI > 2 GO X.
           DELETE BACKZET INVALID NEXT SENTENCE.
           GO C.
       X.  CALL "CAUP" USING "1323012480" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       HB-OUT SECTION.
       A.  CALL "CAUP" USING "0710100560000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               INITIALIZE WHT-SATZ(WX).
           DISPLAY "Bitte im HB-Programm Pkt. 34/5 Bilanz lesen und"
                with highlight foreground-color 3 AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "35/5 Rezeptbilanz exportieren - durchfÅhren"
                 with highlight foreground-color 3 AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Datum Rezeptbilanz: " AT VDU-LP
           ADD 423 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WC-DATUM TO WZ-DATUM WH-WERT.
       C.  DISPLAY "<ret>= Datum, <ret-leer>= ok" AT 2301.
           CALL "CAUP" USING "1104236006" WH-CREG.
           IF not RET GO C.
           MOVE WZ-DATUM TO WV-DAT.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WH-MCODE not = SPACE GO C.
           PERFORM HB.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE 0 TO WR.
           OPEN INPUT HBTXT REVERSED.
           IF WF-STATUS not = 35 GO E.
       D.  DISPLAY "Export-datei fehlt! < > <ret>= wiederholen, <esc>= o
      -        "hne weiterarbeiten" AT 2301.
           CALL "CAUP" USING "0023220000" WH-CREG.
           IF ESC MOVE 0 TO WV-DAT GO Z.
           IF RET GO A.
           GO D.
       E.  MOVE 40 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO G.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 22
               IF KO-BART(WY) not = 0 PERFORM READ-REZ.
       G.  READ HBTXT AT END GO W.
           IF HT-DAT < WV-DAT GO W.
           IF HT-DAT not = WV-DAT GO G.
           INSPECT HT-NUM REPLACING ALL " " BY "0".
           INSPECT HT-PRZ REPLACING ALL " " BY "0".
           EVALUATE HT-NUM
               WHEN 34 MOVE 83 TO HT-NUM
               WHEN 22 MOVE 23 TO HT-NUM
               WHEN 144 MOVE 40 TO HT-NUM
               WHEN 141 MOVE 41 TO HT-NUM
               WHEN 140 MOVE 42 TO HT-NUM
               WHEN OTHER GO G.
           PERFORM BACK-TEIL.
           GO G.
       W.  CLOSE HBTXT.
           DELETE FILE HBTXT.
       Z.  EXIT.
      ******************************************************************
       READ-REZ SECTION.
       A.  MOVE KO-BART(WY) TO HT-NUM RE-NUM.
           READ REZEPT IGNORE LOCK INVALID GO Z.
           MOVE RE-BEZ TO HT-RPBEZ.
           MOVE KO-BPRZ(WY) TO HT-PRZ.
           PERFORM BACK-TEIL.
       Z.  EXIT.
      ******************************************* Teildruck Backzettel *
       BACK-TEIL SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               IF WHT-NUM(WX) = 0
                   MOVE HT-SATZ TO WHT-SATZ(WX).
      *            PERFORM BT-KOPF
      *            MOVE HT-NUM TO DRM-NUM
      *            MOVE HT-RPBEZ TO DRM-BEZ
      *            MOVE HT-PRZ TO WD-POS
      *            MOVE WD-POS TO DRM-STR(35:)
      *            MOVE " %" TO DRM-STR(38:)
      *            PERFORM T-DRUCK
      *            MOVE 16 TO WX.
       Z.  EXIT.
      **************************************** HB-Backzettel erstellen *
       BACKDATEI SECTION.
       A.  IF WI = 3 CLOSE BACKZET GO Z.
           OPEN OUTPUT BACKZHB.
           INITIALIZE HB-SATZ.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE WR-TN(WO-TGN + 1) TO WT-TX.
           CALL "CAUP" USING "20CONV10" WH-CREG.
           CALL "CAUP" USING "21CONV10" WH-CREG.
           PERFORM VARYING WY FROM 13 BY -1 UNTIL WY = 1
               or WT-TX(WY:) not = SPACE CONTINUE.
           ADD 2 TO WY.
           MOVE VDU-DATUM TO WT-TX(WY:).
           MOVE WT-TX TO HB-RPBEZ.
           WRITE HB-SATZ.
           MOVE LOW-VALUE TO BZ-SATZ.
           MOVE HIGH-VALUE TO WV-BZKEY.
           START BACKZET KEY not < BZ-KEY INVALID GO X.
       C.  READ BACKZET NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WV-BZKEY = HIGH-VALUE MOVE BZ-KEY TO WV-BZKEY
               MOVE BZ-CHKZ TO WV-CHKZ
               MOVE BZ-CHMAX TO WV-CHMAX
               MOVE BZ-CHGEW TO WV-CHGEW.
           IF WV-BZKEY = BZ-KEY GO E.
           IF WV-CHKZ not = 0 PERFORM CH-AUFTEIL
                         else WRITE HB-SATZ.
           MOVE BZ-KEY TO WV-BZKEY.
           MOVE BZ-CHKZ TO WV-CHKZ.
           MOVE BZ-CHMAX TO WV-CHMAX.
           MOVE BZ-CHGEW TO WV-CHGEW.
           INITIALIZE HB-SATZ.
       E.  MOVE BZ-NUM TO HB-NUM.
           MOVE BZ-RPBEZ TO HB-RPBEZ.
           ADD BZ-GRAMM TO HB-GESGEW.
           MOVE BZ-KNET TO HB-KNET.
           MOVE BZ-ZEIT TO HB-ZEIT.
           MOVE BZ-CHARGEW TO HB-CHARGEW.
           MOVE BZ-ZUGANG TO HB-ZUGANG.
           DISPLAY HB-RPBEZ with highlight AT 2020.
           GO C.
       G.  IF HB-NUM not = 0 WRITE HB-SATZ.
       X.  CALL "CAUP" USING "1323012480" WH-CREG.
           CLOSE BACKZHB.
           CLOSE BACKZET.
           DELETE FILE BACKZET.
           CALL "CAUP" USING "0710100548000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bitte im HB-Programm Pkt. 36 + 37 Backz-"
                 with highlight foreground-color 2 AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "zettel erstellen + Åbertragen - durchfÅhren"
                  with highlight foreground-color 2 AT VDU-LP.
           DISPLAY "*" AT 2401.
           PERFORM WEITER.
           PERFORM HB.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       HB SECTION.
       A.  MOVE "HB.BAT" TO WD-UPON.
           DISPLAY WD-UPON UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.
       Z.  EXIT.
      ********************************************** Chargenaufteilung *
       CH-AUFTEIL SECTION.
      *-> 1= wenn CHMAX Åberschritten wird dann kleinere Teilung
      *->    z.B. Gew.= 255, max = 250 u. gew = 200 = 200 + 55 kg
      *-> 1= wenn CHMAX Åberschritten wird dann %- Teilung z.b 50%
      *-> 3= wenn keine CHMAX-Angabe dann immer volle Charge lt. REZ
      *-----------------------------------------------------------------
       A.  MOVE HB-GESGEW TO WB-GRAMM.
           IF WV-CHKZ not = 1 GO E.
       C.  IF WB-GRAMM > WV-CHMAX SUBTRACT WV-CHGEW FROM WB-GRAMM
               MOVE WV-CHGEW TO HB-CHARGEW               *> Teilcharge
               WRITE HB-SATZ
               end-write GO C.
980704*    MOVE WB-GRAMM TO HB-CHARGEW.                  *> Restgewicht
980704*    WRITE HB-SATZ.
           GO Z.
      *-----------------------------------------------------------------
       E.  IF WV-CHKZ not = 2 GO H.
           DIVIDE 100000 INTO WV-CHGEW GIVING WH-WERT.
       F.  IF WB-GRAMM > WV-CHMAX
               MULTIPLY WH-WERT BY WB-GRAMM GIVING HB-CHARGEW
               IF HB-CHARGEW > WV-CHMAX
                   DIVIDE 2 INTO WH-WERT GIVING WH-WERT GO F.
       G.  SUBTRACT HB-CHARGEW FROM WB-GRAMM.
           WRITE HB-SATZ.
980704*    IF WB-GRAMM >= HB-CHARGEW GO G.
           GO Z.
      *-----------------------------------------------------------------
       H.  IF WV-CHKZ not = 3 GO Z.
           DIVIDE HB-CHARGEW INTO WB-GRAMM GIVING WX REMAINDER WH-WERT.
           IF WH-WERT not = 0 ADD 1 TO WX.
980704*    PERFORM VARYING WX FROM WX BY -1 UNTIL WX = 0
               WRITE HB-SATZ.
       Z.  EXIT.
      ********************************* Kopf fÅr Back- u. Tourenzettel *
       BT-KOPF SECTION.
       A.  IF WZ-ZEILEN > 60 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"3400" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE WE-DRU(3) TO WM-DRU.
           IF WM-DRU = 1 MOVE 7 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           IF WR = 1 MOVE LF-TOUR TO DRM-NUM
               MOVE KO-TOUR TO DRM-BEZ
           ELSE EVALUATE WI
                WHEN 1 MOVE "B a c k z e t t e l" TO DRM-STR
                WHEN 2 MOVE "Erzeugungsliste" TO DRM-STR
                WHEN 3 MOVE "GÑrverzîgererliste" TO DRM-STR
                WHEN 4 MOVE "Dampflliste" TO DRM-STR
                END-EVALUATE
                    MOVE WR-TN(WO-TGN + 1) TO DRM-STR(23:9).
      *             MOVE "fÅr den:" TO DRM-STR(23:9).
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO WK-DAT.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
           MOVE WK-DS(6:) TO DRA-SATZ(51:).
           PERFORM T-DRUCK.
           MOVE ALL "ƒ" TO DRM-STR.
           PERFORM T-DRUCK.
           MOVE "Art. B e z e i c h n u n g          Menge  Meh   Tg.Gew
      -        ". F Tgrp." TO DRM-STR.
      *----------------------------------------> WE-PANIS: 2 = J.Kern <-
           IF WE-PANIS = 2
               MOVE "B e z e i c h n u n g     (Art)" TO DRM-STR(1:33).
           IF WI = 1 and WM-KO = 3 and WE-PANIS = 0
               MOVE "4:0 Uhr  5:0 Uhr 12:0 Uhr" TO DRM-STR(37:).
           PERFORM T-DRUCK.
           MOVE ALL "ƒ" TO DRM-STR.
           PERFORM T-DRUCK.
       Z.  EXIT.
      *********************************** UP fÅr Back- u. Tourenzettel *
       BTDRU SECTION.
       I.  PERFORM BT-KOPF.
ALT   *    IF WE-LFS = 0 MOVE SU-NUM TO DRM-NUM.
170807     IF WE-LFS = 1 MOVE SU-NUM TO DRM-NUM.
           MOVE SU-BEZ TO DRM-BEZ.
           IF WE-PANIS = 2 MOVE SU-BEZ TO DRM-ABEZ
               MOVE SU-NUM TO DRM-ANUM
               MOVE "(" TO DRM-ANUM(1:1)
               MOVE ")" TO DRM-ANUM(6:1)
               IF DRM-ANUM(2:1)= " " MOVE DRM-ANUM(3:) TO DRM-ANUM(2:).
           ADD 1 SU-MEH GIVING WH-MEH.
           MOVE WT-MEH(WH-MEH) TO DRM-MEH.
           MOVE SU-NK TO WF-NK.
           MOVE SU-TB TO WF-TB.
           IF WI = 1 and WM-KO = 3
               MOVE SU-MG(24) TO WH-ANZ
               PERFORM MG
               IF WH-ANZ not = 0 MOVE WD-MENGE TO DRM-STR(33:)
               end-if
               MOVE SU-MG(25) TO WH-ANZ
               PERFORM MG
               IF WH-ANZ not = 0 MOVE WD-MENGE TO DRM-STR(43:)
               end-if
               MOVE SU-MG(26) TO WH-ANZ
               PERFORM MG
               IF WH-ANZ not = 0 MOVE WD-MENGE TO DRM-STR(53:)
               end-if
           else MOVE SU-MG(23) TO WH-ANZ
                PERFORM MG
                MOVE WD-MENGE TO DRM-MG.
           MOVE DRM-SATZ TO WT-TX.
           DISPLAY WT-TX with SIZE 80 AT 1001.
           ADD 1 TO WZ-SCHALT.
           IF WR = 1 PERFORM T-DRUCK GO Z.
           MOVE SU-NUM TO AR-NUM.
       J.  READ ARTIKEL IGNORE LOCK INVALID MOVE 0 TO AR-ANZ.
           IF WM-KO = 3 AND AR-GVZ = 1 AND WI = 1
               MOVE 1 TO SU-GVZ
               REWRITE SU-SATZ
               MOVE WH-WERT TO WA-ANZ
               PERFORM READ-GVZ
               MOVE WA-ANZ TO WH-WERT.
           MOVE WH-WERT TO WM-ANZ.
           IF WF-TB = 1 or WF-TB = 2
               COMPUTE WH-WERT = WH-WERT * AR-ANZ / 1000.
           IF SU-TGGEW not = 0 MOVE SU-TGGEW TO WH-WERT.
           ADD WH-WERT TO WS-BET.
           PERFORM T-DRUCK.
      *-----------------------------------------> Rezeptgewicht lesen <-
           IF WM-KO not = 3 or WE-PANIS = 1 GO Z.
           IF WI not = 2 and WI not = 3 GO Z.
           MOVE SU-NUM TO RE-NUM.
       M.  READ REZEPT IGNORE LOCK INVALID GO Z.
           DIVIDE 10 INTO RE-ART GIVING RE-ART REMAINDER RE-ART.
           IF RE-ART = 0 COMPUTE WM-ANZ = WM-ANZ * RE-TEIL
           else COMPUTE WM-ANZ = RE-GEW / RE-TEIL * WM-ANZ.
           ADD WM-ANZ TO WS-GEW.
       Z.  EXIT.
      ****************************************** GVZ-Produktion lesen <-
       READ-GVZ SECTION.
       A.  IF WE-PANIS not = 0 GO Z.
           MOVE AR-NUM TO LA-ARNUM.
           READ LAGER IGNORE LOCK INVALID GO Z.
           IF LA-MENGEN(33) = 0 or LA-PDAT not = WM-DATUM GO Z.
           PERFORM T-DRUCK.
           MOVE SU-MG(23) TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MENGE TO DRM-MG.
           MOVE LA-MENGEN(33) TO WH-ANZ.
           PERFORM MG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 11
               or WD-MENGE(WZ:1) not = SPACE
               end-perform
               MOVE WD-MENGE(WZ:) TO DRM-STR(48:)
           SUBTRACT SU-MG(23) FROM LA-MENGEN(33) GIVING WH-ANZ.
           PERFORM MG.
           PERFORM VARYING WZ FROM 65 BY -1 UNTIL WZ < 48
               or DRM-STR(WZ:1) not = SPACE
               end-perform
               ADD 2 TO WZ
               MOVE " gvz" TO DRM-STR(WZ:)
               ADD 6 TO WZ
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 11
               or WD-MENGE(WY:1) not = SPACE
               end-perform
               MOVE WD-MENGE(WY:) TO DRM-STR(WZ:).
       Z.  EXIT.
      ******************************************************************
       T-DRUCK SECTION.
       A.  IF WM-DRU = 1 MOVE DRM-STR TO DRM-SATZ(5:)
                    else MOVE DRM-STR TO DRM-SATZ(3:).
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       FAHRER SECTION.
       A.  ADD 10 WH-TNR GIVING WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE SPACE TO KO-TOUR.
           IF KO-TOUR NOT = SPACE MOVE KO-TOUR TO DRI-STR(WR:6).
           ADD 7 TO WR.
       Z.  EXIT.
      ******************************************************************
       LOEFAK SECTION.
       A.  SET OX PX TO 1.
           MOVE 0 TO FA-KUARNUM FA-MENGE(1) FA-RETOUR(1) FA-MON.
       B.  SET OX UP BY 1.
           MOVE FA-AR(1) TO FA-AR(OX).
           IF OX < 6 MOVE 0 TO FA-PREIS(PX) FA-AB(PX) FA-BIS(PX)
               FA-RET(PX) FA-RAB(PX) SET PX UP BY 1.
           IF OX < 40 GO B.
       Z.  EXIT.
      ********************************** Ueberpruefung ob FAKDAT leer *
       PRUEF SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "1 - ob alles fakturiert" AT 1010
                   "2 - UPDATE Dauerbestellungen" AT 1110
                   "3 - PrÅfung Rechnungsperiode" AT 1210
                   "4 - Update Lfs.-Sortierung  " AT 1310
                   "leer = Programmende" AT 1410
                   " bitte wÑhlen Sie: " with highlight AT 1639.
           CALL "CAUP" USING "0016582002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF WH-PG = 4 PERFORM LFS-UPDATE
                        GO Z.
           IF WH-PG = 1 MOVE "offene Lieferscheine" TO WK-GEB
               DISPLAY "PrÅfung des vergangenen Monats auf durchgefÅhrte
      -           "n Rechnungslauf " AT 0310.
           IF WH-PG = 2 MOVE "  Update Dauerbest." TO WK-GEB
               DISPLAY " énderungen im Debitoren- oder Artikelstamm bezo
      -           "gen auf Dauerbestellungen" AT 0301.
           IF WH-PG = 0 GO Z.
           IF WH-PG > 3; if wh-pg not = 99 GO A.
           CALL "CAUP" USING "1310012480000" WH-CREG.
           DISPLAY "<ret>= Start, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           IF WH-PG = 2 GO O.
           MOVE 1 TO WH-KEY.
           MOVE 0 TO WZ-ZEILEN.
       B.  READ KONSTANT IGNORE LOCK.
           MOVE KO-MON TO WK-MON.
           COMPUTE WH-VON = WK-MON / 100.
           MOVE WH-VON TO WK-VON.
           if wh-pg = 99 perform KUNEIN
               if esc go a
               else move de-ktonr to fa-ktonr
                    move 0 to fa-arnum go g1.
           MOVE 0 TO FA-KTONR FA-ARNUM WX.
           START FAKTDAT KEY NOT < FA-KEY INVALID GO Z.
       C.  READ FAKTDAT NEXT IGNORE LOCK AT END GO G.
           IF FA-MON > WK-VON GO C.
           DIVIDE 10 INTO FA-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0510.
           SET OX TO 1.
           if wh-pg = 3; move 1 to wx
               if fa-mon = 3304 move 9912 to fa-mon
                    rewrite fa-satz invalid next sentence
                    end-rewrite
                    go c
                else go c.
       E.  IF FA-MENGE(OX) NOT = 0 OR FA-RETOUR(OX) NOT = 0 GO F.
           IF OX < 31 SET OX UP BY 1 GO E.
           GO C.
       F.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN = 0 MOVE X"0100" TO WH-PX(1)
               MOVE X"000C" TO WH-PX(2)
               MOVE "LPT1" TO WH-DRUNAM
               MOVE WE-DRU(3) TO WM-DRU
               IF WM-DRU = 1 MOVE 5 TO WH-P
                             MOVE "PRUEF.LST" TO WH-DRUNAM
               end-if
               PERFORM BEG-DRU
               MOVE 1 TO WZ-SCHALT
               MOVE "ausstÑndige Fakturen" TO DRA-TEXT
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT.
           DIVIDE 10 INTO FA-KTONR GIVING DRA-KTONR.
           MOVE FA-ARNUM TO DRA-ARNUM.
           MOVE FA-ART TO DRA-ART.
           MOVE FA-MON TO DRA-MON.
           MOVE DRA-SATZ TO WT-TX.
           DISPLAY WT-TX with SIZE 30 AT 1010.
           PERFORM DRUCK.
           MOVE 1 TO WX.
           GO C.
       G.  IF WX = 1 DISPLAY "Fakturen ausstÑndig!" with highlight
               AT 2401 PERFORM WEITER GO X.
      *-------------------------------------------------> alles leer <-
       g1. MOVE WK-MON TO WZ-DATUM.
           ADD 1 TO WZ-MONAT.
           IF WZ-MONAT > 12 ADD 1 TO WZ-JAHR
               MOVE 1 TO WZ-MONAT.
           MOVE WT-KON(WZ-MONAT) TO WZ-TAG.
           MOVE WZ-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           MOVE WX-DATUM TO WK-MON KO-MON.
           COMPUTE WH-VON = WK-MON / 100.
           MOVE WH-VON TO WK-VON.
           if wh-pg not = 99 MOVE 0 TO FA-KTONR FA-ARNUM WX.
           START FAKTDAT KEY NOT < FA-KEY.
       H.  READ FAKTDAT NEXT AT END GO M.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF FA-MON = WK-VON GO H.
           if wh-pg = 99; if fa-ktonr not = de-ktonr
              rewrite ko-satz go z.
           DIVIDE 10 INTO FA-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0610 " " FA-ARNUM.
           SET OX TO 1.
       I.  MOVE FA-MENGE(OX + 31) TO FA-MENGE(OX).
           MOVE FA-RETOUR(OX + 31) TO FA-RETOUR(OX).
           MOVE 0 TO FA-MENGE(OX + 31) FA-RETOUR(OX + 31).
           IF OX < 9 SET OX UP BY 1 GO I.
           SET OX PX TO 1.
           MOVE WK-VON TO FA-MON.
           if wh-pg = 99 perform varying ox from 1 by 1 until ox > 31
               move 0 to FA-MENGE(OX) FA-RETOUR(OX).
       J.  IF FA-MENGE(OX) NOT = 0 OR FA-RETOUR(OX) NOT = 0 GO L.
           IF OX < 10 SET OX UP BY 1 GO J.
           MOVE 0 TO FA-MON.
       K.  IF FA-PREIS(PX) NOT = 0 OR FA-RET(PX) NOT = 0
               OR FA-AB(PX) NOT = 0 OR FA-BIS(PX) NOT = 0
               OR FA-RAB(PX) NOT = 0 OR FA-KUARNUM NOT = 0 GO L.
           IF PX < 4 SET PX UP BY 1 GO K.
           DELETE FAKTDAT.
           GO H.
       L.  REWRITE FA-SATZ.
           GO H.
       M.  MOVE 1 TO WH-KEY.
           READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE WX-DATUM TO WK-MON KO-MON.
           REWRITE KO-SATZ.
           DISPLAY "PrÅflauf durchfÅhrt!" AT 1024.
      *-----------------------------------> Update Dauerbestellungen <-
       O.  MOVE 0 TO DA-KTONR DA-ARNUM.
           START DAUER KEY NOT < DA-KEY INVALID GO Z.
       P.  READ DAUER NEXT AT END DISPLAY "Update durchgefÅhrt!"
               AT 2401 PERFORM WEITER GO Z.
           IF ZUGRIF PERFORM BESETZT GO P.
           MOVE DA-ARNUM TO AR-NUM.
       Q.  READ ARTIKEL IGNORE LOCK INVALID GO P.
           IF AR-NUM > 999 GO P.
           MOVE AR-GRM TO WF-GRM.
           DISPLAY AR-BEZ with highlight AT 1110.
           MOVE AR-BEZ TO DA-BEZ.
           IF DA-KTONR = DE-KTONR GO S.
           MOVE DA-KTONR TO DE-KTONR.
           MOVE 1 TO DE-FNR.
       R.  READ DEBITOR IGNORE LOCK INVALID GO P.
       S.  MOVE DE-STATIS TO WF-SON.
           MOVE WF-GRM TO DA-GRM.
           MOVE DE-TOUR TO DA-TOUR.
           MOVE DE-SUB TO DA-SUB.
           MOVE DE-EAN TO DA-EAN.
           MOVE DE-FAKART TO DA-FART.
           REWRITE DA-SATZ.
           GO P.
       X.  IF WZ-ZEILEN > 0 PERFORM END-DRU.
       Z.  EXIT.
      ************************************* Lfs.-Update wg. Sortierung *
       LFS-UPDATE SECTION.
       A.  CALL "CAUP" USING "271010055000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lfs-Update wg. Sortierung "
               with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <Einfg>= Start Update < >"
               AT VDU-LP.
           CALL "CAUP" USING "1003420000" WH-CREG.
           IF ESC GO X.
           IF not EINF GO C.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lfs-Nr.: " AT VDU-LP.
           INITIALIZE LF-SATZ.
           START LFSCHEIN KEY > LF-KEY INVALID STOP RUN.
       E.  READ LFSCHEIN NEXT AT END GO G.
           IF LF-DATUM not > WM-DATUM GO E.
           MOVE LF-ARNUM TO AR-NUM.
           READ ARTIKEL IGNORE LOCK INVALID GO E.
           MOVE AR-SGRP TO LF-SGRP.
           IF AR-SGRP > 0 MOVE AR-BEZ TO LF-ARBEZ.
           REWRITE LF-SATZ.
           DISPLAY LF-NUM  with highlight AT VDU-LP " " LF-KTONR.
           GO E.
       G.  DISPLAY "Kto-Nr.: " AT VDU-LP.
           INITIALIZE FA-SATZ.
           START FAKTDAT KEY > FA-KEY INVALID STOP RUN.
       I.  READ FAKTDAT NEXT AT END GO X.
           MOVE FA-ARNUM TO AR-NUM.
           READ ARTIKEL IGNORE LOCK INVALID GO E.
           MOVE AR-SGRP TO FA-SGRP.
           IF AR-SGRP > 0 MOVE AR-BEZ TO FA-ARBEZ.
           REWRITE FA-SATZ.
           DISPLAY FA-KTONR with highlight AT VDU-LP " " FA-ARNUM.
           GO I.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KUNEIN SECTION.
       A.  DISPLAY "Kundennummer:" AT 2301.
           CALL "CAUP" USING "0023155005" WH-CREG.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2
           MOVE 1 TO DE-FNR.
       D.  READ DEBITOR IGNORE LOCK INVALID KEY GO a.
           display de-bez at 1001.
       E.  display "<ret>= ok, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0023270000" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO E.
       Z.  EXIT.
      ********************************* Erstellung Grundlieferscheine *
       DAUERLFS SECTION.
       A.  CALL "CAUP" USING "03DATUM" WH-CREG.
       B.  MOVE WO-TGN TO WY.
           IF WY = 0 MOVE 7 TO WY.
           MOVE WY TO WD-KZ.
           CALL "CAUP" USING "1305520580" WH-CREG.
           DISPLAY WD-KZ with highlight AT 0554 "-".
           IF WY < 7 DISPLAY WR-TN(WO-TGN + 1) AT 0556.
           IF WY = 7 DISPLAY WR-TN(1) AT 0556
                         " Achtung!" with highlight BLINK AT 0500.
           IF WY > 7 DISPLAY " * Sonder *" AT 0500
              IF WM-KO = 3 DISPLAY "ACHTUNG! Besondere Verwendung"
                               with highlight BLINK AT 0330.
           DISPLAY "<ret-leer>= ok, <ret>= Prog. 1 - 9, <esc>= Abbruch"
               AT 2301.
           CALL "CAUP" USING "0005541001" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           IF WH-NUM NOT = 0;
      *       IF WH-NUM < 7 COMPUTE WO-TGN = 7 - WH-NUM
              IF WH-NUM = 7 MOVE 0 TO WO-TGN
                  else MOVE WH-NUM TO WO-TGN WD-KZ
              end-if GO B.
           MOVE 0 TO DA-KTONR DA-ARNUM WH-WERT.
           ADD 9 WY GIVING WX.
           START DAUER KEY NOT < DA-KEY INVALID KEY GO X.
       C.  READ DAUER NEXT IGNORE LOCK AT END GO X.
           MOVE DA-GRM TO WF-GRM.
           IF DA-DAT(1) NOT < WV-DATUM GO C.
           IF DA-DAT(2) NOT = 0; IF WV-DATUM NOT < DA-DAT(2);
               IF WV-DATUM NOT > DA-DAT(3) GO C.
           IF WY = 9 AND WM-KO > 1
               COMPUTE LF-NUM = WZ-TAG * 100 + 10
               else COMPUTE LF-NUM = WZ-TAG * 100
                   IF WF-SON > 1 ADD 99 TO LF-NUM.
           MOVE DA-ANZ(WY) TO LF-MENGE(1).
           MOVE 0 TO LF-PREIS.
           MOVE DA-ARNUM TO AR-NUM.
       D.  READ ARTIKEL IGNORE LOCK INVALID KEY
               MOVE "Artikel gelîscht" TO AR-BEZ.
           IF WM-PR = 0 GO E.
           IF WV-DATUM < AR-ADAT MOVE AR-VKP TO LF-PREIS
                            ELSE MOVE AR-AVKP TO LF-PREIS
               IF LF-PREIS = 0 MOVE AR-VKP TO LF-PREIS.
       E.  MOVE DA-KTONR TO LF-KTONR.
           MOVE DA-ARNUM TO LF-ARNUM.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE DA-BEZ   TO LF-BEZ.
           MOVE DA-EAN   TO LF-EAN.
           MOVE DA-TOUR  TO LF-TOUR.
           MOVE DA-SUB   TO LF-SUB.
           MOVE DA-FART  TO LF-FART.
           MOVE DA-GRM   TO LF-GRM WF-GRM.
           MOVE 0 TO LF-MENGE(2) LF-MENGE(3).
           MOVE AR-SGRP TO LF-SGRP.
           IF AR-SGRP > 0 MOVE AR-BEZ TO LF-ARBEZ.
      *--------------------------------> Kennzeichen Dauerbestellung <-
           MOVE 99 TO LF-STAT.
           IF LF-MENGE(1) = 0 GO F.
           WRITE LF-SATZ INVALID KEY GO F.
           DIVIDE 10 INTO LF-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 1510.
           DISPLAY DA-BEZ AT 1520.
       F.  IF DA-ANZ(WX) = 0 AND DA-ANZ(WX + 9) = 0 GO C.
           IF WY = 9 AND WM-KO > 1 GO C.
           IF WF-SON > 1 ADD 97 TO LF-NUM.
           IF DA-ANZ(WX) NOT = 0 COMPUTE LF-NUM = WZ-TAG * 100 + 1
               MOVE DA-ANZ(WX) TO LF-MENGE(1)
               MOVE 0 TO DA-ANZ(WX) GO E.
           IF DA-ANZ(WX + 9) NOT = 0 COMPUTE LF-NUM = WZ-TAG * 100 + 2
               MOVE DA-ANZ(WX + 9) TO LF-MENGE(1)
               MOVE 0 TO DA-ANZ(WX + 9).
           GO E.
       X.  DISPLAY "Grundlieferscheine erstellt" with highlight AT 2401.
           PERFORM WEITER.
           MOVE 0 TO LF-KTONR WV-KTONR LF-TOUR LF-SUB LF-ARNUM.
           MOVE WV-DATUM TO LF-DATUM.
           IF WY = 9 AND WM-KO > 1 MOVE 10 TO WY
               ELSE MOVE 0 TO WY.
       Z.  EXIT.
      *************************************** Lieferscheinarchivierung *
       LFSARCHIV SECTION.
       A.  IF DE-KTONR = LF-KTONR GO C.
           MOVE 1 TO DE-FNR.
           MOVE LF-KTONR TO DE-KTONR.
       B.  READ DEBITOR IGNORE LOCK
               INVALID MOVE "Kunde fehlt" TO DE-BEZ.
       C.  IF DE-RAST = 3 GO Z.
           DIVIDE 100 INTO LF-NUM GIVING WK-ANZ REMAINDER WK-ANZ.
           IF WK-ANZ > 90 GO Z.
           IF WK = 0 PERFORM LFKOPF
           else IF WZ-ZEILEN > VDU-AB
                    MOVE "U e b e r t r a g" TO DRC-SATZ(26:)
                    PERFORM ACDRUCK
                    WRITE AC-SATZ FROM DRC-SATZ AFTER PAGE
                    MOVE 0 TO WZ-ZEILEN
                    PERFORM LFKOPF
                    MOVE "U e b e r t r a g" TO DRC-SATZ(26:)
                    PERFORM ACDRUCK.
           IF WM-PR = 1 MOVE LF-PREIS TO DRC-BETRAG
                    END-IF PERFORM EAN.
           MOVE LF-ARNUM TO DRC-ARNUM.
           MOVE LF-BEZ TO DRC-BEZ.
           EVALUATE LF-STAT
               WHEN 98 MOVE "*" TO DRC-SATZ(47:)
               WHEN 95
               WHEN 94 MOVE "Nicht lieferbar!" TO DRC-TX(43:).
      *    IF LF-STAT = 98 MOVE "*" TO DRC-SATZ(47:).
           MOVE LF-MENGE(1) TO WH-ANZ.
           MOVE LF-GRM TO WF-GRM.
           PERFORM MG.
           MOVE WD-MENGE TO DRC-MENGE.
           ADD 1 WF-MEH GIVING WH-MEH.
           MOVE WT-MEH(WH-MEH) TO DRC-MEH.
           IF LF-MENGE(2) NOT = 0 MOVE LF-MENGE(2) TO WH-ANZ
               PERFORM MG
               MOVE WD-MENGE TO DRC-RETMG.
           MOVE DRC-SATZ TO WT-ADR.
      *------------------------------------------------> ob EAN-Druck <-
           IF DE-EAN = 2 AND FA-KUARNUM NOT = 0
               MOVE WH-EAN TO DRC-ARNUM.
           PERFORM ACDRUCK.
           IF WM-KO = 3 MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      **************************************************** EAN-Nummer *
       EAN SECTION.
       A.  IF DE-EAN = 0 OR DE-EAN = 4 MOVE WV-ARNUM TO DRC-ARNUM GO Z.
           IF DE-EAN = 2 OR DE-EAN = 3 GO D.
           COMPUTE WH-EAN = WK-EAN * 1000000 + (WV-ARNUM * 10).
       B.  ADD WR-EAN(2) WR-EAN(4) WR-EAN(6) WR-EAN(8) WR-EAN(10)
               WR-EAN(12) GIVING WK-ANZ.
           MULTIPLY 3 BY WK-ANZ.
           ADD WR-EAN(1) WR-EAN(3) WR-EAN(5) WR-EAN(7) WR-EAN(9)
               WR-EAN(11) TO WK-ANZ.
           DIVIDE 10 INTO WK-ANZ GIVING WK-ANZ REMAINDER WK-ANZ.
           SUBTRACT WK-ANZ FROM 10 GIVING WR-EAN(13).
           MOVE WH-EAN TO DRC-ARNUM.
           GO Z.
      *----------------------> lesen v. kundeneigenen Artikelnummern <-
       D.  IF WV-ARNUM = FA-ARNUM AND DE-KTONR = FA-KTONR GO F.
           MOVE WV-ARNUM TO FA-ARNUM.
           MOVE DE-KTONR TO FA-KTONR.
       E.  READ FAKTDAT INVALID KEY MOVE 0 TO FA-KUARNUM GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
       F.  IF DE-EAN = 3; IF FA-KUARNUM = 0
               MOVE WV-ARNUM TO DRC-ARNUM GO Z
           ELSE MOVE FA-KUARNUM TO DRC-ARNUM GO Z.
           MOVE FA-KUARNUM TO WH-EAN.
           MOVE WH-EAN TO WM-EAN.
           ADD WT-7(1) WT-7(3) WT-7(5) WT-7(7) GIVING WI.
           MULTIPLY 3 BY WI.
           ADD WT-7(2) WT-7(4) WT-7(6) TO WI.
           DIVIDE 10 INTO WI GIVING WI REMAINDER WI.
           SUBTRACT WI FROM 10 GIVING WT-7(8).
           MOVE WM-EAN TO WH-EAN.
       Z.  EXIT.
      ******************************************************************
       LFKOPF SECTION.
       A.  IF DE-KTONR = LF-KTONR GO C.
           MOVE 1 TO DE-FNR.
           MOVE LF-KTONR TO DE-KTONR.
       B.  READ DEBITOR IGNORE LOCK
                INVALID MOVE "Kunde fehlt" TO DE-BEZ.
       C.  MOVE DE-BEZ TO WT-BEZ.
           IF WM-KO = 3 AND DE-RAST = 3 GO Z.
      *----------------------------------------> abstellen Archivkopf <-
           IF WK = 0 DIVIDE 10 INTO DE-KTONR GIVING WH-ARCNR
               MOVE LF-NUM TO WH-ARCLF
               OPEN OUTPUT ARCHIV
               MOVE 1 TO WM-OPEN
               MOVE LF-NUM TO AC-LFNUM
               MOVE DE-KTONR TO AC-KTONR
               MOVE DE-MCODE TO AC-MCODE
               MOVE LF-DATUM TO AC-LFDAT
               MOVE AC-SATZ TO DRA-SATZ
               MOVE 0 TO WZ-SCHALT
               PERFORM ACDRUCK.
           MOVE 1 TO WK.
           IF WM-KO = 3 MOVE 5 TO WZ-SCHALT GO E.
           IF WM-KO = 0 MOVE 8 TO WZ-SCHALT GO E.
           MOVE 1 TO WZ-SCHALT.
           MOVE 2 TO WH-KEY.
       D.  READ KONSTANT IGNORE LOCK.
           MOVE X"1B21" TO DRC-SATZ(1:)
           EVALUATE WH-KEY
               WHEN 2 MOVE X"3B00" TO DRC-SATZ(3:2) PERFORM ACDRUCK
               WHEN 3 MOVE X"3100" TO DRC-SATZ(3:2) PERFORM ACDRUCK
               WHEN 5 MOVE X"0100" TO DRC-SATZ(3:2) PERFORM ACDRUCK.
           IF WH-KEY = 5 MOVE KO-KOPF TO DRC-SATZ(7:)
               ELSE MOVE KO-KOPF TO DRC-SATZ(4:).
           PERFORM ACDRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO D.
           MOVE ALL "-" TO DRC-TX.
       E.  PERFORM ACDRUCK.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE 3 TO WZ-SCHALT.
           MOVE WT-TXT(DE-ANR + 1) TO DRC-SATZ(17:).
           PERFORM ACDRUCK.
           MOVE SPACE TO WR-ADR(3).
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
              MOVE WR-ADR(TX) TO DRC-SATZ(17:)
              PERFORM ACDRUCK.
           MOVE 4 TO WZ-SCHALT.
           MOVE 0 TO WK-ANZ.
       G.  MOVE X"1B213100"  TO DRC-SATZ(1:).
           IF WK-ANZ > 89 OR (WM-VER = 2 AND WM-KO = 1)
               MOVE "  Bestellung" TO DRC-SATZ(8:)
           ELSE MOVE "Lieferschein" TO DRC-SATZ(8:).
           MOVE X"1B210100" TO DRC-SATZ(20:4).
           MOVE "Nr.:" TO DRC-SATZ(27:).
           MOVE LF-NUM TO WD-LFNUM.
           MOVE WD-LFNUM TO DRC-SATZ(36:6).
           MOVE "/" TO DRC-SATZ(43:).
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRC-SATZ(44:8).
           MOVE VDU-GZDAT TO DRC-SATZ(52:).
           MOVE LF-TOUR TO WD-POS.
           MOVE WD-POS TO DRC-SATZ(81:3).
           PERFORM ACDRUCK.
           MOVE "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒ¬ƒ
      -        "ƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒø" TO DRC-TX
               PERFORM ACDRUCK.
           IF WM-PR = 0 MOVE "≥Artikelnummer≥ B e z e i c h n u n g    ≥
      -        "     Menge ≥Meh≥   Retour   ≥  Nachliefg. ≥" TO DRC-TX
                   ELSE MOVE "≥Artikelnummer≥ B e z e i c h n u n g    ≥
      -        "     Menge ≥Meh≥            ≥  empf.Preis ≥" TO DRC-TX.
                PERFORM ACDRUCK.
           MOVE "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒ¡ƒ
      -        "ƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒŸ" TO DRC-TX
                PERFORM ACDRUCK.
           PERFORM ACDRUCK.
       Z.  EXIT.
      ******************************************************* Drucker *
       ACDRUCK SECTION.
       C.  WRITE AC-SATZ FROM DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO AC-SATZ DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       END-ACDRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
       B.  WRITE AC-SATZ FROM DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ AC-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE ARCHIV.
           MOVE 0 TO WM-KTONR WK WZ-ZEILEN.
       Z.  EXIT.
