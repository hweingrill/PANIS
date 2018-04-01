      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
      $set segsize "45000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANLIST.
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSEART.CPY.
           COPY PANSEKON.CPY.
           COPY PANSEDAU.CPY.
           COPY PANSESTA.CPY.
           SELECT TEMPSTA   ASSIGN TO "PANEHILF.DAT"
                            ORGANIZATION INDEXED ACCESS DYNAMIC
                            RECORD KEY TP-KEY
                            FILE STATUS IS WF-STATUS.
           SELECT ABCDATEI  ASSIGN TO "PANEABC.DAT"
                            ORGANIZATION INDEXED ACCESS DYNAMIC
                            RECORD KEY AB-KEY
                            FILE STATUS IS WF-STATUS.
           SELECT EDIFACT   ASSIGN TO WN-EDIFACT         *> DatentrÑger
                            ORGANIZATION LINE SEQUENTIAL
                            ACCESS SEQUENTIAL
                            FILE STATUS WF-STATUS.
           SELECT DRUCKER   ASSIGN TO PRINTER WH-DRUNAM
                            FILE STATUS WF-STATUS.
           SELECT EXCELDAT  ASSIGN TO WH-EXCEL
                            FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
       COPY PANSTAT.CPY.
      ******************************************************************
       FD  EDIFACT                     LABEL RECORD STANDARD.
       01  EDF-SATZ                    PIC X(120).
      ********************************************** Artikelstatistik *
       FD  TEMPSTA       external      LABEL RECORD STANDARD.
       01  TP-SATZ.
           03  TP-KEY.
               05 TP-KTONR             PIC 9(8)       COMP.
               05 TP-ARNUM             PIC 9(4)       COMP.
           03  TP-LJAHR                PIC 99         COMP.
           03  TP-LSUM                 PIC S9(9)V99   COMP OCCURS 4.
           03  TP-VJAHR                PIC 99         COMP.
           03  TP-VSUM                 PIC S9(9)V99   COMP OCCURS 4.
           03  TP-BET                  PIC S9(7)V99   COMP.
           03  TP-MWST                 PIC S9(7)V99   COMP.
      ********************************************** Artikelstatistik *
       FD  ABCDATEI                   LABEL RECORD STANDARD.
       01  AB-SATZ.
           03  AB-KEY.
               05 AB-BASIS             PIC 9(9)       COMP.
               05 AB-ARNUM             PIC 9(4)       COMP.
           03  AB-ARBEZ                PIC X(25).
           03  AB-SUM.
               05 AB-MG                PIC X(11)              OCCURS 4.
               05 AB-PZ                PIC S999V99    COMP    OCCURS 3.
               05 AB-BET               PIC S9(8)      COMP-3.
               05 AB-PR                PIC S9(5)V99   COMP-3.
      ******************************** P3800 = PRINTER **** PC = LPT1 *
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(128).
       01  DRE-SATZ.
           03  FILLER                  PIC X(6).
           03  DRE-STR.
               05 FILLER               PIC X(6).
               05 DRE-TOUR             PIC X(69).
       01  DRF-SATZ.
           03  FILLER                  PIC X(6).
           03  DRF-ARNUM               PIC ZZ9-.
           03  DRF-ARBEZ               PIC X(25).
           03  DRF-MG                  PIC X(11)          OCCURS 2.
           03  DRF-PREIS               PIC ZZ.ZZ9,99-.
           03  DRF-BET                 PIC ZZ.ZZZ.ZZ9,99-.
       01  DRG-SATZ.
           03  FILLER                  PIC X(4).
           03  DRG-STR.
               05 DRG-ARNUM            PIC ZZZ9-.
               05 DRG-ARBEZ            PIC X(24).
               05 DRG-MG               PIC X(11)          OCCURS 4.
               05 DRG-BET              PIC BZZ.ZZZ.ZZ9-.
               05 DRG-PZ               PIC ZZ9,99-        OCCURS 3.
               05 DRG-PR               PIC Z.ZZ9,99-.
       01  DRH-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRH-STR.
               05 DRH-ARNUM            PIC ZZZ9-.
               05 DRH-ARBEZ            PIC X(27).
               05 DRH-MEH              PIC XXX.
               05 DRH-NK               PIC Z9-.
               05 DRH-PL               PIC 9/.
               05 DRH-TB               PIC 9.
               05 DRH-PR               PIC X(10)          OCCURS 3.
               05 DRH-UST              PIC Z9/.
               05 DRH-MWST             PIC 99-.
               05 DRH-RAB              PIC X.
               05 DRH-OEPZ             PIC BZ9.
               05 DRH-TGRP             PIC ZZ9.
       01  DRK-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRK-STR.
               05 DRK-KTONR            PIC ZZ.ZZ9,9-.
               05 FILLER               PIC X(7).
               05 DRK-RAB              PIC ZZZ9,9-.
               05 DRK-RET              PIC ZZZZ9,9-.
               05 DRK-RBASIS           PIC ZZZZ9,9-.
               05 DRK-BONUS            PIC ZZZZ9,9-.
               05 FILLER               PIC X.
               05 DRK-MCODE            PIC X(9).
               05 DRK-ANR              PIC ZZ9-.
               05 DRK-UST              PIC ZZ9-.
               05 DRK-ZK               PIC Z9-.
               05 DRK-EAN              PIC Z9-.
               05 DRK-REKTO            PIC ZZZ.ZZ9,9-.
               05 DRK-TOUR             PIC ZZ9/.
               05 DRK-SUB              PIC ZZZZ.
               05 DRK-STAT             PIC ZZ9-.
               05 DRK-FA               PIC ZZ9/.
               05 DRK-FAX              PIC X(6).
               05 DRK-RAST             PIC 9-.
       01  DRN-SATZ.
           03  FILLER                  PIC X(7).
           03  DRN-STR.
               05 FILLER               PIC X(40).
               05 DRN-MG               PIC X(7)      OCCURS 9.
               05 FILLER               PIC X.
               05 DRN-AB               PIC X(9).
               05 DRN-BIS              PIC X(8).
       01  DRO-SATZ.
           03  FILLER                  PIC X(5).
           03  DRO-STR.
               05 DRO-KTONR            PIC ZZ.ZZ9,9-.
               05 DRO-NAME.
                  07 FILLER            PIC X(59).
                  07 DRO-RAB           PIC ZZZ9,9-.
                  07 DRO-PZ            PIC X(9).
       01  DRP-SATZ.
           03  FILLER                  PIC X(5).
           03  DRP-STR.
               05 DRP-ARNUM            PIC Z(12)9-.
               05 DRP-BEZ              PIC X(27).
               05 DRP-ABDAT            PIC X(10).
               05 DRP-BISDAT           PIC X(9).
               05 DRP-PREIS            PIC X(10).
               05 DRP-RETP             PIC ZZZ9,999-.
               05 DRP-RAB              PIC ZZ9,9.
               05 DRP-KUARN            PIC Z(10).
       01  DRR-SATZ.
           03  FILLER                  PIC X(7).
           03  DRR-STR.
               05 DRR-ARNUM            PIC ZZZ9-.
               05 DRR-ARBEZ            PIC X(29).
               05 DRR-MG               PIC X(12)          OCCURS 2.
               05 DRR-BET              PIC BZZ.ZZZ.ZZ9-.
               05 DRR-PZ               PIC BZ9,99-.
       01  DRS-SATZ.
           03  FILLER                  PIC X(4).
           03  DRS-STR.
               05 DRS-ARNUM            PIC ZZZ9-.
               05 DRS-ARBEZ            PIC X(25).
               05 DRS-MPKG             PIC ZZZZ.ZZ9,99-.
               05 DRS-MPS              PIC ZZ.ZZZ.ZZ9,99-.
               05 DRS-JPKG             PIC ZZZZ.ZZ9,99-.
               05 DRS-JPS              PIC ZZ.ZZZ.ZZ9,99-.
       01  DRV-SATZ.
           03  FILLER                  PIC X(48).
           03  DRV-PREIS               PIC X(14).
           03  DRV-VKP                 PIC Z.ZZ9,99.
           03  DRV-UST                 PIC Z(9)9.
      ********************************************** Exel-Dateiausgabe *
       FD  EXCELDAT                    LABEL RECORD STANDARD.
       01  EXC-SATZ                    PIC X(128).
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
       01  WH-REG.
           03  WN-EDIFACT              PIC X(20).
           03  WH-DRUNAM               PIC X(12)  VALUE "LPT1:".
           03  WH-EXCEL                PIC X(15).
           03  WM-LST                  PIC X(12).
           03  WH-PX                   PIC XX        OCCURS 2.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99     COMP-X.
           03  WX-PRSTAT               PIC 99     COMP-X.
           03  WZ-SEITE                PIC 99     COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99     COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99     COMP-3 VALUE ZERO.
           03  WK-DS.
               05 FILLER               PIC X(5)       VALUE "per:".
               05 WK-DAT               PIC X(10).
               05 FILLER               PIC X(7)       VALUE "Seite: ".
               05 WK-SEITE             PIC ZZ9-.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 999       COMP.
           03  WK-S                    PIC 99        COMP.
           03  wm-l                    pic 99        comp.
           03  WH-RAB                  PIC S99V9     COMP-3.
           03  WH-PRZ                  PIC S999V99   COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WM-KTONR                PIC 9(6)      COMP.
           03  WS-JAHR                 PIC 9999      COMP.
           03  WV-JAHR                 PIC 9999      COMP.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WA-ANZ                  PIC 9(5)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(5).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WS-MG                   PIC X(11).
           03  WC-MG                   PIC S9(9)V99.
           03  WS-MGA                  PIC ZZZZ.ZZ9-.
           03  WS-MGB                  PIC ZZZZ.ZZ9,9-.
           03  WD-PREIS                PIC Z.ZZ9,99-.
           03  WD-APREIS               PIC Z.ZZ9,999-.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WH-PREIS                PIC S9(5)V999  COMP-3.
           03  WR-PR REDEFINES WH-PREIS.
               05 FILLER               PIC X(4).
               05 WR-GR                PIC S9         COMP-3.
           03  WM-BS                   PIC 99.
           03  WV-OEPZ                 PIC 99         COMP.
           03  WH-BET                  PIC S9(9)V99   COMP-3.
           03  WH-RET                  PIC S9(9)V99   COMP-3.
           03  WS-RET                  PIC S9(9)V99   COMP-3.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
           03  WS-GEW                  PIC S9(9)V99   COMP-3.
           03  WZ-BET                  PIC S9(9)V99   COMP-3.
           03  WH-MWST                 PIC S9(9)V99   COMP-3.
           03  WS-SUM                  PIC S9(9)V99   COMP-3 OCCURS 9.
           03  WS-GSUM                 PIC S9(9)V99   COMP-3 OCCURS 8.
           03  WH-MEH                  PIC 99         COMP.
           03  WH-MOD                  PIC 99         COMP.
           03  WH-BMS                  PIC 99         COMP.
           03  WH-DRU                  PIC 99         COMP.
           03  WD-NUM                  PIC Z.ZZ9-.
           03  WM-CD                   PIC X(35).
           03  WH-CNT                  PIC 9999.
           03  WH-SEQ                  PIC 9999.
           03  WE-ANZ                  PIC ZZZ9.
           03  WH-TIME.
               05  WH-HHMM             PIC 9(4)      VALUE 0.
               05  WH-SSHH             PIC 9(4)      VALUE 0.
           03  WH-KONTROL.
               05 WH-KO                PIC XXX.
               05 WH-KOTG              PIC 9(6).
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WST-SUM.
               05 WST-MENGEN           PIC S9(7)      COMP.
               05 WST-RET              PIC S9(7)      COMP.
               05 WST-UMSATZ           PIC S9(8)V9    COMP.
               05 WST-GEWINN           PIC S9(8)V9    COMP.
           03  WD-X                    PIC X.
           03  WM-R-2                  PIC S9(5)V99   COMP-3.
           03  WM-R-3                  PIC S9(5)V999  COMP-3.
           03  WE-UST                  PIC Z9,99.
           03  WE-PREIS                PIC ZZZ9,999.
           03  WE-DATUM                PIC 9(8).
      *-----------------------------------------------> Excel-Azsgabe <-
           03  WD-TRENN                PIC 9.
           03  WXK-NAME1               PIC X(35).
           03  WXK-NAME2               PIC X(35).
           03  WXK-NAME3               PIC X(35).
           03  WXK-STR                 PIC X(35).
           03  WXK-PLZL                PIC X(10).
           03  WXK-ORT                 PIC X(35).
           03  WXK-LANDKZ              PIC X(5).
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING  "PANEFAKT.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING  "PANARTIK.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING  "PANDEBIT.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING  "PANEKONS.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  CALL "CADECL" USING  "PANESTAT.DAT" WH-CREG.
       EXIT.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON ABCDATEI.
       A.  CALL "CADECL" USING  "PANEABC.DAT " WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON DAUER.
       A.  CALL "CADECL" USING  "PANEBEST.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING  "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 50 PERFORM ETKDRU GO X.
           MOVE 0 TO WH-DRU.
           IF WL-CA = 01 PERFORM LIWAHL GO X.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      *********************************************** Listladeroutine *
       DASEIT SECTION.
       A.  MOVE VDU-DATUM TO WK-DAT.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
       B.  MOVE WK-DS(WI:) TO DRA-SATZ(WL:).
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
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 07 TO WX-TASTE.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRU = 1 MOVE WM-LST TO WH-DRUNAM.
           IF WH-DRUNAM(1:3) not = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
       C.  DISPLAY "Drucker nicht bereit: " with highlight AT 2301
               "<ret>= Fehler beheben <Einfg>= Druckdatei < >".
           DISPLAY "Dateiname: " AT 2401 WM-LST with highlight.
           CALL "CAUP" USING "0023660000" WH-CREG.
           IF RET GO A.
           IF EINF MOVE WM-LST TO WH-DRUNAM GO A.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
           IF WM-DRU = 1 MOVE DRA-SATZ(3:) TO DRA-SATZ(1:).
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** DruckerrÅckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU not = 1 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhîhe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           PERFORM DRU-OK.
           IF WH-DRUNAM(1:3) = "LPT" OPEN OUTPUT DRUCKER
                                else OPEN EXTEND DRUCKER.
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
      ***************************************** Mengenkommaumwandlung *
       MG SECTION.
       A.  IF WF-NK = 2 DIVIDE 100 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ2 WD-MGC
               MOVE WD-MGC TO WD-MG
               MOVE WD-ANZ2 TO WD-MENGE.
           IF WF-NK = 1 DIVIDE 10 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ1 WD-MGB
               MOVE WD-MGB TO WD-MG
               MOVE WD-ANZ1 TO WD-MENGE.
           IF WF-NK = 0 MOVE WH-ANZ TO WD-ANZ WD-MGA
               MOVE WD-MGA TO WD-MG
               MOVE WD-ANZ TO WD-MENGE.
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      ******************************************************************
       KOPF SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WH-DRU = 1 DISPLAY "Druckdatei " with highlight AT 0355
                    else DISPLAY "Direktdruck" with highlight at 0355.
       Z.  EXIT.
      ********************************************** Auswahl Listungen *
       LIWAHL SECTION.
       A.  MOVE "  Listungen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE WE-DRU(4) TO WM-DRU.
           IF WE-WO(4) = 9 MOVE 0 TO WH-DRU
                      else MOVE 1 TO WH-DRU.
       B.  DISPLAY "1 - Sonderpreise"      AT 0724
                   "2 - Dauerbestellungen" AT 0024
                   "3 - Kundenpreisliste"  AT 0024
                   "4 - allg. Preisliste"  AT 0024
                   "5 - Artikeldaten/PRICAT" AT 0024
                   "6 - Kundenstammdaten"  AT 0024
                   "7 - Touren"            AT 0024
                   "8 - Artikel"           AT 0024
                   "9 - Kunden"            AT 0024
                   "10 - Retouren"         AT 0023
                   "11 - Konditoreiartikel" AT 0023
                   " bitte wÑhlen Sie: "   AT 0039
           DISPLAY "<esc/ret-leer>= Programmende, < >= Wechsel Druckausg
      -        "abe" AT 2301.
           IF WH-DRU = 1 DISPLAY "Druckdatei " with highlight AT 0365
                    else DISPLAY "Direktdruck" with highlight at 0365.
           CALL "CAUP" USING "0018582002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF AB; IF WH-DRU = 0 MOVE 1 TO WH-DRU GO B
                           else MOVE 0 TO WH-DRU GO B.
           IF WH-PG = 0 OR ESC GO Z.
           IF NOT RET GO B.
           MOVE "LPT1:" TO WH-DRUNAM.
           EVALUATE WH-PG
               WHEN 1 MOVE "  Sonderpreise"     TO WK-GEB
                   PERFORM SONDAU GO A
               WHEN 2 MOVE " Dauerbestellungen" TO WK-GEB
                   PERFORM SONDAU GO A
               WHEN 3 MOVE " Kundenpreisliste"  TO WK-GEB
                   PERFORM PR-LI  GO A
               WHEN 4 MOVE "  allg. Preisliste" TO WK-GEB
                   PERFORM PR-LI  GO A
               WHEN 5 MOVE " Artikelstammdaten" TO WK-GEB
                   PERFORM AR-LI  GO A
               WHEN 6 MOVE "  Kundenstammdaten" TO WK-GEB
                   PERFORM KU-LI  GO A
               WHEN 7 MOVE "  Tourenstatistik"  TO WK-GEB
               WHEN 8 MOVE "  Artikelstatistik" TO WK-GEB
               WHEN 9 MOVE "  Kundenstatistik"  TO WK-GEB
               WHEN 10 MOVE "  Retourstatistik" TO WK-GEB
               WHEN 11 MOVE "  Kond.-Artikelstat." TO WK-GEB
               WHEN 59 PERFORM STATKOR GO B
               WHEN OTHER GO B.
           PERFORM KOPF.
           MOVE 0 TO WH-BMS.
       C.  CALL "CAUP" USING "1304012480" WH-CREG.
           DISPLAY "Statistikausdruck per:" AT 0510.
           DISPLAY "<esc>= Abbruch, <ret>= Monatsendedatum" AT 2301.
           IF WH-PG = 9;
              IF WH-BMS = 1 DISPLAY " BS " AT 0202
                       else DISPLAY ", < >= Bildschirmausgabe " AT 0000.
           DISPLAY "(strg-apos)" at 2401.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0105336006" WH-CREG.
           IF AB MOVE 1 TO WH-BMS GO C.
           IF ESC OR WOLI GO A.
           move 0 to wm-l.
           if sapo move 1 to wm-l set ret to true
               display "**" at 0578.
           IF NOT RET OR WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT 0533.
           MOVE WX-DATUM TO WV-DATUM.
           MOVE 0 TO WV-KTONR WM-KTONR.
           IF WH-PG < 9 or = 11 GO H.
       E.  DISPLAY "Kunden-Nr.:" AT 0710.
           DISPLAY "<esc>= Abbruch, alpha+<ret>= Suchen, Nr.+<ret>= Kund
      -        "ennummer, <ret-leer>= alle" AT 2301.
           CALL "CAUP" USING "0007226006" WH-CREG.
           IF ESC OR WOLI GO C.
           IF NOT RET GO E.
           IF WH-NUM = 0; IF WH-MCODE = SPACE 
               DISPLAY " alles " AT 0270
               DISPLAY "ƒƒƒƒƒƒƒƒ" AT 0201
               MOVE 0 TO WH-BMS
               CALL "CAUP" USING "1308012480" WH-CREG GO X
           ELSE CALL "PANANZ" USING "10DEBSUCH" WH-CREG.
           IF WH-NUM = 0 GO E.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 0722.
       F.  READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO E.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ WITH SIZE 70 AT 0810.
           MOVE DE-KTONR TO WM-KTONR.
       H.  MOVE 0 TO WH-MOD.
           IF WH-PG NOT = 8 GO X.
           DISPLAY "0 - norm. Listung" AT 1010.
           DISPLAY "1 - ABC-Liste Mengen" AT 1210.
           DISPLAY "2 - ABC-Liste Umsatz" AT 1410.
           DISPLAY "< >" with highlight AT 1335.
           DISPLAY "3 - Prod. Statistik" AT 1610.
           DISPLAY "<esc>= Abbruch, <ret>= Auswahl" AT 2301.
           CALL "CAUP" USING "0013361001" WH-CREG.
           IF ESC GO Z.
           MOVE WH-NUM TO WH-MOD.
           IF WH-MOD > 3 GO H.
           IF NOT RET GO H.
           CALL "CAUP" USING "1310012480" WH-CREG.
           EVALUATE WH-MOD
               WHEN 0 DISPLAY "norm. Liste" AT 0370 GO X
               WHEN 1 DISPLAY "ABC-Mengen " AT 0370
               WHEN 2 DISPLAY "ABC-Umsatz " AT 0370
               WHEN 3 DISPLAY "Prod.Stat. " AT 0370 GO X.
           IF WH-MOD = 0 GO X.
      *------------------------------------------> Sortdatei aufbauen <-
           OPEN OUTPUT ABCDATEI.
           CLOSE ABCDATEI.
           OPEN I-O ABCDATEI.
       X.  IF WH-PG = 10 AND WH-NUM = 0 DISPLAY "Kunden-Nr. fehlt"
               AT 2401 PERFORM WEITER GO E.
           OPEN INPUT STATISTIK.
           EVALUATE WH-PG
               WHEN  7 PERFORM TOURSTA GO A
               WHEN  8 PERFORM ARTSTA  GO A
               WHEN  9 PERFORM KUNDSTA
                       IF WM-KTONR < 2 GO A
                       else MOVE 0 TO WV-KTONR WM-KTONR
                            GO E
               WHEN 10 PERFORM RETSTAT GO C
               WHEN 11 PERFORM ARTSTA  GO A.
       Z.  EXIT.
      ******************************************************************
       SONDAU SECTION.
       A.  PERFORM KOPF.
       B.  DISPLAY "Kunden-Nr.:" AT 0301.
           UNLOCK DEBITOR.
           DISPLAY "alpha+<ret>= Suchen, Nr.+<ret>= Kundennummer, <ret-l
      -        "eer>= alles, <esc>= Ende" AT 2301.
           CALL "CAUP" USING "0003136009" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO B.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PANANZ" USING "10DEBSUCH" WH-CREG
                   IF WH-NUM = 0 GO A.
           CALL "CAUP" USING "1303012465" WH-CREG.
           CALL "CAUP" USING "1304012480" WH-CREG.
           DISPLAY "Nr.: " AT 0301.
           MOVE SPACE TO DE-MCODE.
           IF WH-NUM = 0 MOVE 0 TO WV-KTONR GO D.
           ADD 0,2 WH-NUM GIVING WD-KTO.
           DISPLAY WD-KTO AT 0306.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
       C.  READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO A.
           MOVE DE-KTONR TO WV-KTONR.
           GO F.
       D.  START DEBITOR KEY NOT < DE-MCODE INVALID PERFORM NO-REC GO A.
       E.  READ DEBITOR NEXT IGNORE LOCK AT END GO Z.
           IF DE-FNR NOT = 1 GO E.
       F.  MOVE 0 TO WZ-SEITE WZ-ZEILEN.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 0306.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ WITH SIZE 64 AT 0406.
           IF WH-PG = 1 PERFORM SOND-LI.
           IF WH-PG = 2 PERFORM DAU-LI.
       W.  IF WV-KTONR = 0; IF WZ-SEITE = 0 GO E
               ELSE PERFORM END-DRU GO E.
           IF WZ-SEITE = 0 PERFORM NO-REC GO A
               ELSE PERFORM END-DRU GO A.
       Z.  EXIT.
      ********************************************** Sonderpreisliste *
       SOND-LI SECTION.
       A.  MOVE DE-KTONR TO FA-KTONR.
           MOVE 0 TO FA-ARNUM.
           MOVE "&l26a1h6d0O(s0p12.8h0b0s6T" TO WE-STG(10).
           START FAKTDAT KEY NOT < FA-KEY INVALID GO Z.
       G.  READ FAKTDAT NEXT IGNORE LOCK AT END GO Z.
       H.  IF WV-KTONR NOT = 0; IF FA-KTONR NOT = WV-KTONR GO Z.
           IF DE-KTONR NOT = FA-KTONR GO Z.
           SET PX TO 1.
       I.  IF FA-PREIS(PX) = 0 AND FA-RET(PX) = 0 AND FA-RAB(PX) = 0;
               IF PX < 4 SET PX UP BY 1 GO I ELSE GO G.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN = 0 MOVE X"0100" TO WH-PX(1)
               MOVE X"000C" TO WH-PX(2)
               MOVE "SONDPR.LST" TO WM-LST
               IF WM-DRU = 1 MOVE 10 TO WH-P
               end-if
               PERFORM BEG-DRU
               MOVE 1 TO WZ-SCHALT
               IF WM-DRU = 1 MOVE 0 TO WZ-SCHALT
               end-if
               MOVE "Sonderpreisvereinbarungen fÅr:" TO DRP-STR
               MOVE WM-DATUM TO WC-DATUM
               PERFORM DATDREH
               MOVE 79 TO WL
               MOVE 6 TO WI PERFORM DASEIT
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               DIVIDE 10 INTO DE-KTONR GIVING DRO-KTONR
               MOVE WT-BEZ TO DRO-NAME
               PERFORM DRUCK
               MOVE ALL "ƒ" TO DRP-STR
               PERFORM DRUCK
               MOVE "Artikelnummer B e z e i c h n u n g            ab -
      *           " bis        Preis    K.Vkp.   Rab  K-Art.Nr"
      -           " bis        Preis      Euro   Rab  K-Art.Nr"
                    TO DRP-STR
               PERFORM DRUCK
               MOVE ALL "ƒ" TO DRP-STR
               PERFORM DRUCK.
           MOVE FA-ARNUM TO AR-NUM.
       J.  READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel gelîscht!" TO AR-BEZ.
           MOVE AR-GRM TO WF-GRM.
           SET PX TO 1.
           PERFORM VARYING wx FROM 4 by -1 until wx = 1
               or fa-preis(wx) NOT = 0 or fa-rab(wx) not = 0
               continue.
           set px to wx.
       K.  IF FA-PREIS(PX) NOT = 0 OR FA-RET(PX) NOT = 0
               OR FA-RAB(PX) NOT = 0 GO M.
       L.  IF PX < 4 SET PX UP BY 1 GO K.
           MOVE 2 TO WZ-SCHALT.
           GO G.
      *M.  IF PX = 1 MOVE AR-NUM TO DRP-ARNUM
       M.      MOVE AR-NUM TO DRP-ARNUM
               MOVE AR-BEZ TO DRP-BEZ
               MOVE FA-KUARNUM TO DRP-KUARN.
           MOVE FA-AB(PX) TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRP-ABDAT.
           IF FA-BIS(PX) NOT = 0 MOVE FA-BIS(PX) TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRP-BISDAT.
           MOVE FA-PREIS(PX) TO WH-PREIS WD-PREIS WD-APREIS.
           IF FA-PREIS(PX) NOT = 0;
               IF WR-GR = 0 MOVE WD-PREIS TO DRP-PREIS
                      ELSE MOVE WD-APREIS TO DRP-PREIS.
           MOVE WH-PREIS TO DRP-RETP.
           IF FA-RAB(PX) NOT = 0 MOVE FA-RAB(PX) TO DRP-RAB.
           MOVE 1 TO WZ-SCHALT.
           MOVE DRO-STR TO WT-TX.
           DISPLAY WT-TX WITH SIZE 80 AT 2001.
           PERFORM DRUCK.
           GO L.
       Z.  EXIT.
      *************************************** Liste Dauerbestellungen *
       DAU-LI SECTION.
       A.  MOVE DE-KTONR TO DA-KTONR.
           MOVE 0 TO DA-ARNUM.
           MOVE "&l26a1h6d0O(s0p16.6h0b0s6T" TO WE-STG(10).
           START DAUER KEY NOT < DA-KEY INVALID GO Z.
       G.  READ DAUER NEXT IGNORE LOCK AT END GO Z.
       H.  IF WV-KTONR NOT = 0; IF DA-KTONR NOT = WV-KTONR GO Z.
           IF DE-KTONR NOT = DA-KTONR GO Z.
           SET PX TO 1.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN = 0 MOVE X"0400" TO WH-PX(1)
               MOVE X"000C" TO WH-PX(2)
               MOVE "DAUER.LST" TO WM-LST
               IF WM-DRU = 1 MOVE 10 TO WH-P
               end-if
               PERFORM BEG-DRU
               IF WM-DRU = 1 MOVE 0 TO WZ-SCHALT
               end-if
               MOVE 1 TO WZ-SCHALT
               MOVE "Dauerbestellungen fÅr:" TO DRN-STR
               MOVE WM-DATUM TO WC-DATUM
               PERFORM DATDREH
               MOVE 108 TO WL
               MOVE 6 TO WI PERFORM DASEIT
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               DIVIDE 10 INTO DE-KTONR GIVING DRO-KTONR
               MOVE WT-BEZ TO DRO-NAME
               PERFORM DRUCK
               MOVE ALL "ƒ" TO DRN-STR
               PERFORM DRUCK
               MOVE "Artikelnummer B e z e i c h n u n g       1-Mo   2-
      -            "Di   3-Mi   4-Do   5-Fr   6-Sa   7-So    8/+    9/- 
      -            " nicht ab bis inkl" TO DRN-STR
               PERFORM DRUCK
               MOVE ALL "ƒ" TO DRN-STR
               PERFORM DRUCK.
           MOVE DA-ARNUM TO AR-NUM.
       J.  READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel gelîscht!" TO AR-BEZ.
           MOVE AR-GRM TO WF-GRM.
           MOVE AR-NUM TO DRP-ARNUM.
           MOVE AR-BEZ TO DRP-BEZ.
           MOVE 1 TO WY.
           MOVE 7 TO VDU-L.
           MOVE 20 TO VDU-P.
       K.  MOVE DA-ANZ(WY) TO WH-ANZ.
           PERFORM MG.
           IF DA-ANZ(WY) NOT = 0 MOVE WD-MG TO DRN-MG(WY).
           IF WY < 9 ADD 1 TO WY GO K.
           IF DA-DAT(2) > 0 MOVE DA-DAT(2) TO WC-DATUM PERFORM DATDREH
               MOVE VDU-DATUM TO DRN-AB.
           IF DA-DAT(3) > 0 MOVE DA-DAT(3) TO WC-DATUM PERFORM DATDREH
               MOVE VDU-DATUM TO DRN-BIS.
           MOVE DRN-STR TO WT-TX.
           DISPLAY WT-TX WITH SIZE 80 AT 2001.
           PERFORM DRUCK.
           MOVE 1 TO WX.
           ADD 1 TO WY.
       L.  MOVE DA-ANZ(WY) TO WH-ANZ.
           PERFORM MG.
           IF DA-ANZ(WY) NOT = 0 MOVE WD-MG TO DRN-MG(WX).
           ADD 1 TO WX.
           IF WY < 18 ADD 1 TO WY GO L.
           IF DRN-SATZ NOT = SPACE
               MOVE "-2.Lfg.-" TO DRN-AB PERFORM DRUCK.
           MOVE 1 TO WX.
           ADD 1 TO WY.
       N.  MOVE DA-ANZ(WY) TO WH-ANZ.
           PERFORM MG.
           IF DA-ANZ(WY) NOT = 0 MOVE WD-MG TO DRN-MG(WX).
           ADD 1 TO WX.
           IF WY < 27 ADD 1 TO WY GO N.
           IF DRN-SATZ NOT = SPACE
               MOVE "-3.Lfg.-" TO DRN-AB PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           GO G.
       Z.  EXIT.
      **************************************************** Preisliste *
       PR-LI SECTION.
       A.  PERFORM KOPF.
      *    MOVE 1 TO WH-KEY.
      *    READ KONSTANT IGNORE LOCK.
      *    MOVE KO-MERK TO WK-M.
           DISPLAY "Preilistendruck per:" AT 0510.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM AT 0532.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0105326006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET OR WZ-DATUM = 0 GO A.
           DISPLAY VDU-DATUM AT 0532.
           MOVE WX-DATUM TO WV-DATUM.
           MOVE 0 TO WV-KTONR WM-KTONR WK-S.
           IF WH-PG NOT = 3 GO G.
       C.  DISPLAY "Kunden-Nr.:" AT 0710.
           DISPLAY "<esc>= Abbruch, Nr.+<ret>= Kd.Nr., <ret-leer>= kein 
      -        "Kunde" AT 2301.
           DISPLAY "nur Kundenpreise: Nr.+<#>= Kd.Nr., <#-leer>= alle Ku
      -        "nden" AT 2401.
           MOVE 0 TO WK-S.
           CALL "CAUP" USING "0007226008" WH-CREG.
           IF ESC GO A.
           IF KIST MOVE 1 TO WK-S
               SET RET TO TRUE.
           IF NOT RET GO C.
           MOVE WH-NUM TO WV-KTONR.
           IF WK-S = 1 DISPLAY "Kundensonderpreise" AT 0555.
           IF WH-NUM = 0 DISPLAY " allgemein" AT 0270
               CALL "CAUP" USING "1308012480" WH-CREG GO G.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
       E.  READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO C.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO WITH HIGHLIGHT AT 0722.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ WITH SIZE 70 AT 0810.
           MOVE DE-KTONR TO WM-KTONR.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start(num), <tab>= Start(alph
      -        "a)" AT 2301.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN AR-NUM.
           MOVE SPACE TO AR-AKEY WV-MCODE.
           CALL "CAUP" USING "0023560000" WH-CREG.
           IF ESC GO Z.
           MOVE 13 TO WS-TASTE WR.
      *    IF RET PERFORM PREILIS GO Z.
           IF TABL MOVE 0 TO WR
               MOVE 5 TO WS-TASTE
               MOVE SPACE TO AR-SA.
           IF NOT RETA GO G.
           IF WK-S = 0 and WH-PG = 4 PERFORM PREILIS GO A.
      *    IF WV-KTONR NOT = 0 PERFORM PREILIS GO A.
           MOVE 1 TO DE-FNR.
           MOVE 0 TO DE-KTONR.
           START DEBITOR KEY NOT < DE-MCODE INVALID PERFORM NO-REC GO A.
       H.  READ DEBITOR NEXT IGNORE LOCK AT END GO Z.
      *    IF DE-FNR NOT = 1 GO Z.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO WITH HIGHLIGHT AT 0722.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ WITH SIZE 70 AT 0810.
           MOVE DE-KTONR TO WM-KTONR.
           PERFORM PREILIS.
           GO H.
       Z.  EXIT.
      ****************************************** Kopf bei Nadeldrucker *
       NADEL-KOPF SECTION.
       A.  MOVE X"1B21" TO DRA-SATZ(1:).
           EVALUATE WH-KEY
               WHEN 2 MOVE X"3B00" TO DRA-SATZ(3:2)
               WHEN 3 MOVE X"3100" TO DRA-SATZ(3:2)
               WHEN 5 MOVE X"0100" TO DRA-SATZ(3:2).
           PERFORM DRUCK.
           IF WH-KEY = 5 MOVE KO-KOPF TO DRA-SATZ(6:)
                    else MOVE KO-KOPF TO DRA-SATZ(4:)
                    IF WH-KEY = 2 PERFORM OB-TEILG.
       Z.  EXIT.
      ************************************************ ob Druckteilung *
       OB-TEILG SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 80
              IF DRA-SATZ(WX:1) = "*" MOVE DRA-SATZ(WX:) TO WT-TX
                  IF WM-DRU = 1 MOVE "(s25v0s3b4101T" TO DRA-SATZ(WX:)
                                ADD 15 TO WX
                  end-if MOVE WT-TX(2:) TO DRA-SATZ(WX:)
                         MOVE 84 TO WX.
       Z.  EXIT.
      ****************************************** Kopf bei Laserdrucker *
       LASER-KOPF SECTION.
       A.  MOVE 0 TO WZ-SCHALT.
           EVALUATE WH-KEY
               WHEN 2 MOVE "  (s1p18v4s3b4101T" TO DRA-SATZ
                      PERFORM DRUCK
                      MOVE KO-KOPF TO DRA-SATZ(9:)
                      PERFORM OB-TEILG
                      MOVE 0 TO WZ-SCHALT
               WHEN 3 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KO-KOPF TO DRA-SATZ(9:)
               WHEN 4 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KO-KOPF TO DRA-SATZ(9:)
               WHEN 5 MOVE "  (s0p0s0b0T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KO-KOPF TO DRA-SATZ(14:).
       Z.  EXIT.
      ******************************************************************
       PREILIS SECTION.
       A.  MOVE 0 TO AR-NUM.
           MOVE SPACE TO AR-AKEY.
           IF WS-TASTE = 13 GO I.
           START ARTIKEL KEY NOT < AR-AKEY INVALID PERFORM NO-REC GO Z.
           GO K.
       I.  START ARTIKEL KEY NOT < AR-KEY INVALID PERFORM NO-REC GO Z.
       K.  READ ARTIKEL NEXT IGNORE LOCK AT END GO X.
           IF AR-NUM = 0 GO K.
           MOVE AR-GRM TO WF-GRM.
980430     IF WF-TB > 3 MOVE 2 TO WF-TB.             *> BÑckereiartikel
           IF AR-SA NOT = SPACE MOVE 10 TO WF-STATUS GO X.
           PERFORM KUPREI.
           IF WL = 99 AND WK-S = 1 GO K.
           IF WH-PREIS NOT = 0 OR WH-RAB NOT = 0 GO L.
           IF WH-PG = 3 AND AR-PL NOT = 1 GO K.
      *------------------------------------> Aufteilung nach Gruppen <-
      *L.  IF WR = 0 AND WF-TB > 1 GO K.
       L.  IF WR = 0 AND WF-TB > 0 GO K.
           IF WR = 1 AND WF-TB > 1 GO K.
           IF WR = 2 AND WF-TB NOT = 2 GO K.
           IF WR = 3 AND WF-TB NOT = 3 GO K.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Q.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "PREISE.LST" TO WM-LST.
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE 2 TO WH-KEY.
       M.  READ KONSTANT IGNORE LOCK.
           EVALUATE WM-DRU
               WHEN 0 PERFORM NADEL-KOPF
               WHEN 1 PERFORM LASER-KOPF.
           PERFORM DRUCK.
           IF WH-KEY = 2 ADD 2 TO WH-KEY GO M.
           MOVE 0 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE "(s0p0s0b4101T" TO DRA-SATZ(3:)
                    else MOVE X"1B21" TO DRA-SATZ(1:2)
                         MOVE X"0900" TO DRA-SATZ(3:2)
           end-if PERFORM DRUCK.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE 63 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           MOVE 3 TO WZ-SCHALT.
           PERFORM DRUCK.
           IF WM-KTONR NOT = 0 DIVIDE 10 INTO DE-KTONR GIVING DRO-KTONR
               MOVE WT-BEZ TO DRO-NAME
               PERFORM DRUCK
               MOVE ALL "ƒ" TO DRO-STR
               PERFORM DRUCK
               MOVE "Artikelnummer B e z e i c h n u n g           Preis
      -            "     " TO DRO-STR
               GO N.
           MOVE ALL "ƒ" TO DRO-STR.
           PERFORM DRUCK.
           MOVE "Artikelnummer B e z e i c h n u n g           Preis     
      -        " empf.Vkf.      Mwst." TO DRO-STR.
       N.  PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRO-STR.
           PERFORM DRUCK.
       Q.  MOVE AR-BEZ TO WV-MCODE.
           IF WS-TASTE = 13 MOVE WV-MC(1) TO WH-X.
           IF WH-X NOT = WV-MC(1) PERFORM DRUCK
               MOVE WV-MC(1) TO WH-X.
           MOVE AR-NUM TO DRP-ARNUM.
           MOVE AR-BEZ TO DRP-BEZ.
           IF WH-PREIS NOT = 0 GO V.
       U.  IF AR-ADAT = 0 OR AR-ADAT > WV-DATUM
                    MOVE AR-PREIS TO WH-PREIS
               ELSE MOVE AR-APREIS TO WH-PREIS.
       V.  MOVE WH-PREIS TO WD-PREIS WD-APREIS.
           IF WR-GR = 0 MOVE WD-PREIS TO DRV-PREIS
                  ELSE MOVE WD-APREIS TO DRV-PREIS.
           IF WM-KTONR NOT = 0 AND WH-PREIS = 0 
               MOVE SPACE TO DRA-SATZ GO K.
           IF WM-RB = 0; IF WH-RAB = 0 MOVE DE-RAB TO WH-RAB.
           IF WM-KTONR NOT = 0; IF WH-RAB = 0 GO W
               ELSE MULTIPLY -1 BY WH-RAB GIVING DRO-RAB
                    MOVE "%" TO DRO-PZ
                    MOVE DRA-SATZ(72:) TO DRA-SATZ(60:) GO W.
           IF AR-ADAT = 0 OR AR-ADAT > WV-DATUM
                    MOVE AR-VKP TO WH-PREIS
               ELSE MOVE AR-AVKP TO WH-PREIS.
           IF WH-PREIS NOT = 0 MOVE WH-PREIS TO DRV-VKP.
           MOVE WT-UST(AR-UST) TO DRV-UST.
       W.  MOVE 1 TO WZ-SCHALT.
           MOVE DRO-STR TO WT-TX.
           DISPLAY WT-TX WITH SIZE 80 AT 2001.
           PERFORM DRUCK.
           GO K.
       X.  IF WZ-ZEILEN > 0 PERFORM END-DRU.
           IF WH-PG = 4 and WR > 2 GO Z.
           IF WH-PG not = 4 and WR > 1 GO Z.
           ADD 1 TO WR.
           MOVE SPACE TO AR-AKEY AR-SA.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
           START ARTIKEL KEY NOT < AR-AKEY INVALID GO Z.
           GO K.
       Z.  EXIT.
      ******************************************************************
       KUPREI SECTION.
       A.  MOVE 0 TO WH-PREIS WH-RAB WL.
           IF WK-S = 0; IF WM-KTONR = 0 GO Z.
           MOVE DE-RAB TO WH-RAB.
           MOVE DE-KTONR TO FA-KTONR.
           MOVE AR-NUM TO FA-ARNUM.
       B.  READ FAKTDAT INVALID KEY MOVE 99 TO WL GO Z.
           IF ZUGRIF PERFORM BESETZT GO B.
           SET PX TO 4.
       S.  IF FA-AB(PX) = 0 OR FA-AB(PX) > WV-DATUM GO T.
           IF FA-BIS(PX) = 0 OR FA-BIS(PX) NOT < WV-DATUM
               MOVE FA-PREIS(PX) TO WH-PREIS
               MOVE FA-RAB(PX) TO WH-RAB GO Z.
       T.  IF PX > 1 SET PX DOWN BY 1 GO S.
           IF WK-S = 1 MOVE 99 TO WL.
       Z.  EXIT.
      ***************************************************** Zeilenende *
       ZEILEND SECTION.
       A.  MOVE "'" TO WM-CD(1:).
           PERFORM ANHANG.
           WRITE EDF-SATZ BEFORE 1.
           MOVE SPACE TO EDF-SATZ.
           ADD 1 TO WH-CNT.
       Z.  EXIT.
      ******************************************************* anhÑngen *
       ANHANG SECTION.
       A.  PERFORM VARYING WX FROM 70 BY -1
               UNTIL EDF-SATZ(WX:1) not = SPACE continue.
           ADD 1 TO WX.
           MOVE WM-CD TO EDF-SATZ(WX:).
       Z.  EXIT.
      ******************************************************************
       PRFZ SECTION.
       B.  COMPUTE WH-EAN = WK-EAN * 1000000.
           ADD WR-EAN(2) WR-EAN(4) WR-EAN(6) WR-EAN(8) WR-EAN(10)
               WR-EAN(12) GIVING WK-ANZ.
           MULTIPLY 3 BY WK-ANZ.
           ADD WR-EAN(1) WR-EAN(3) WR-EAN(5) WR-EAN(7) WR-EAN(9)
               WR-EAN(11) TO WK-ANZ.
           DIVIDE 10 INTO WK-ANZ GIVING WK-ANZ REMAINDER WK-ANZ.
           SUBTRACT WK-ANZ FROM 10 GIVING WR-EAN(13).
           COMPUTE WS-DATUM = FUNCTION INTEGER-OF-DATE(WM-DATUM).
           MOVE "ART" TO WH-KONTROL.
           MOVE WS-DATUM TO WH-KOTG.
       Z.  EXIT.
      ******************************************************************
       KUNDENNR SECTION.
       A.  CALL "CAUP" USING "0710100560000" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "fÅr Kunden: " AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, alpha+<ret>= suchen, <ret>= Kunden-N
      -        "r." AT 2301.
           CALL "CAUP" USING "1003155008" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO C.
           IF WH-NUM = 0 CALL "PANANZ" USING "10DEBSUCH" WH-CREG
                         CANCEL "PANANZ".
           IF WH-NUM = 0 GO A.
           ADD 0,2 WH-NUM GIVING WD-KTO.
           IF WH-PG not = 5
               DISPLAY WD-KTO with highlight AT 0313.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
       E.  READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO A.
           MOVE DE-KTONR TO WM-KTONR.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ with SIZE 55 AT VDU-LP.
           MOVE DE-KTONR TO FA-KTONR WV-KTONR.
           MOVE 0 TO FA-ARNUM.
           START FAKTDAT KEY NOT < FA-KEY INVALID PERFORM NO-REC GO C.
       F.  READ FAKTDAT NEXT IGNORE LOCK AT END PERFORM NO-REC GO C.
           IF FA-KTONR NOT = WV-KTONR PERFORM NO-REC GO C.
           MOVE DE-KTONR TO FA-KTONR WV-KTONR.
           MOVE 0 TO FA-ARNUM.
           START FAKTDAT KEY NOT < FA-KEY INVALID PERFORM NO-REC GO C.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO G.
       Z.  EXIT.
      ************************************************ Edifact-Ausgabe *
       PRICAT SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           MOVE "EDIDAT\PRICAT.EDI" TO WN-EDIFACT.
           DISPLAY WN-EDIFACT with highlight AT VDU-LP.
           PERFORM KUNDENNR.
           IF ESC GO X.
           IF DE-REKTO = DE-KTONR GO C.
           MOVE DE-REKTO TO DE-KTONR
           READ DEBITOR IGNORE LOCK INVALID GO Z.
       C.  MOVE 0 TO WH-CNT WH-SEQ.
           MOVE "ART4711" TO WH-KONTROL.
           PERFORM PRFZ.
           OPEN OUTPUT EDIFACT.
           MOVE "UNA:+,? '" TO EDF-SATZ.
           WRITE EDF-SATZ BEFORE 1.
           MOVE "UNB-UNOC:3+" TO EDF-SATZ.
           MOVE WH-EAN TO WM-CD.                          *> Versender
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           IF DE-GLN(1:1) not = 0 MOVE DE-GLN TO WM-CD WH-EAN
           else MOVE DE-GLN(2:) TO WM-CD WH-EAN.
           IF DE-REKTO not = DE-KTONR PERFORM READ-REKTO.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-DATUM TO WC-DATUM.
           MOVE WC-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           ACCEPT WH-TIME FROM TIME.
           MOVE WH-HHMM TO WM-CD(1:4).
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "UNH+" TO EDF-SATZ.
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE "PRICAT" TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE "D:" TO WM-CD.
           PERFORM ANHANG.
           MOVE "01B:" TO WM-CD.
           PERFORM ANHANG.
           MOVE "UN:EAN006" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "BGM+9+" TO EDF-SATZ.
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG.
           MOVE "+9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "DTM+137:" TO EDF-SATZ.
           MOVE WX-DATUM TO WM-CD.
           PERFORM ANHANG.
      *    MOVE WH-HHMM TO WM-CD.
      *    PERFORM ANHANG.
      *    MOVE ":204" TO WM-CD.
           MOVE ":102" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE 0 TO WH-EAN(8:).
           PERFORM PRFZ.
           MOVE "NAD+BY+9006140000000" TO EDF-SATZ.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+SU+" TO EDF-SATZ.
           MOVE WH-EAN TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "PGI+9" TO EDF-SATZ.
           PERFORM ZEILEND.
      *-------------------------------------> Preise fÅr Kunden lesen <-
       F.  READ FAKTDAT NEXT IGNORE LOCK AT END GO W.
           IF FA-KTONR NOT = WV-KTONR GO W.
           SET PX TO 4.
       G.  IF FA-AB(PX) NOT = 0 OR FA-BIS(PX) NOT = 0
               OR FA-PREIS(PX) NOT = 0 OR FA-RET(PX) NOT = 0
               OR (PX = 1 and FA-KUARNUM NOT = 0) GO I.
           IF PX > 1 SET PX DOWN BY 1 GO G.
       I.  MOVE FA-ARNUM TO AR-NUM.
           READ ARTIKEL IGNORE LOCK INVALID GO F.
           MOVE AR-GRM TO WF-GRM.
           ADD 220 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-BEZ AT VDU-LP.
           INSPECT AR-BEZ REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹".
           IF AR-SA not = SPACE GO W.
           MOVE "LIN+" TO EDF-SATZ.
           ADD 1 TO WH-SEQ.
           MOVE WH-SEQ TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+1+" TO WM-CD.
           PERFORM ANHANG.
           MOVE AR-NUM TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE ":SA" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "PIA+1+101:GD::91" TO EDF-SATZ.
           PERFORM ZEILEND.
           MOVE "IMD+F++:::" TO EDF-SATZ.
           MOVE AR-BEZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "TAX+7+VAT+++:::" TO EDF-SATZ.
           MOVE WT-UST(AR-UST) TO WE-UST.
           MOVE WE-UST TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "PRI+AAB:" TO EDF-SATZ.
           MOVE FA-PREIS(PX) TO WE-PREIS WH-WERT.
           IF WH-WERT = 0 MOVE AR-APREIS TO WE-PREIS WH-WERT.
           IF WH-WERT = 0 MOVE AR-PREIS TO WE-PREIS WH-WERT.
           IF WE-PREIS(8:1) = 0 MOVE " " TO WE-PREIS(8:1).
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-PREIS(1:1) = " " MOVE WE-PREIS(2:) TO WE-PREIS(1:).
           MOVE WE-PREIS TO WM-CD.
           PERFORM ANHANG.
           MOVE "::::" TO WM-CD.
           PERFORM ANHANG.
           EVALUATE WF-MEH
               WHEN 1 MOVE "KGM" TO WM-CD
               WHEN OTHER MOVE "STK" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "CUX+2:EUR:8"TO EDF-SATZ.
           PERFORM ZEILEND.
           IF FA-AB(PX) not = 0 MOVE FA-AB(PX) TO WE-DATUM
               MOVE "DTM+194:" TO EDF-SATZ
               MOVE WE-DATUM TO WM-CD
               PERFORM ANHANG
               MOVE ":102" TO WM-CD
               PERFORM ANHANG
               PERFORM ZEILEND.
           IF FA-BIS(PX) not = 0 MOVE FA-BIS(PX) TO WE-DATUM
               MOVE "DTM+206:" TO EDF-SATZ
               MOVE WE-DATUM TO WM-CD
               PERFORM ANHANG
               MOVE ":102" TO WM-CD
               PERFORM ANHANG
               PERFORM ZEILEND.
           GO F.
      *-----------------------------------------------> Kunde zu Ende <-
       W.  MOVE "UNT+" TO EDF-SATZ.
           MOVE WH-CNT TO WD-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WD-ANZ(1:1) = " " MOVE WD-ANZ(2:) TO WD-ANZ(1:).
           MOVE WD-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "UNZ+1+" TO EDF-SATZ.
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           CLOSE EDIFACT.
           DISPLAY "Datei 'PRICAT.EDI' erstellt!" with highlight
                AT 2301 ", weiter mit ret < >".
           CALL "CAUP" USING "0023470000" WH-CREG.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************* Rechnungskonto lesen *
       READ-REKTO SECTION.
       A.  MOVE DE-REKTO TO DE-KTONR.
           READ DEBITOR INVALID STOP RUN.
           IF DE-GLN(1:1) not = 0 MOVE DE-GLN TO WM-CD
           else MOVE DE-GLN(2:) TO WM-CD.
       Z.  EXIT.
      *********************************************  Artikelstammliste *
       AR-LI SECTION.
       A.  PERFORM KOPF.
       B.  DISPLAY "<esc>= Abbruch, <ret>= Start(num), <tab>= Start(alph
      -        "a)" AT 2301.
           DISPLAY "<Ende>= Art.Ausgabe (PRICAT) < >" AT 2401.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN AR-NUM.
           MOVE SPACE TO AR-AKEY AR-SA.
           CALL "CAUP" USING "0024310000" WH-CREG.
           EVALUATE TRUE
               WHEN ESC GO Z
               WHEN RET START ARTIKEL KEY NOT < AR-KEY INVALID GO X
               WHEN ENDE PERFORM PRICAT GO Z
               WHEN TABL START ARTIKEL KEY NOT < AR-AKEY INVALID GO X
               WHEN OTHER GO B.
       D.  READ ARTIKEL NEXT IGNORE LOCK AT END GO W.
           MOVE AR-GRM TO WF-GRM.
           IF AR-SA NOT = SPACE MOVE 10 TO WF-STATUS GO W.
           IF WH-PG = 3 AND AR-PL = 0 GO D.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           MOVE "ARTIKEL.LST" TO WM-LST.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Artikelstammdatenliste" TO DRH-STR.
           MOVE KO-FIRMA TO DRH-STR(28:).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE 55 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRH-STR.
           PERFORM DRUCK.
           MOVE " Nr. B e z e i c h n u n g      Meh N P/T   Preis
      -        " Ekp. empf.Vkf. Mwst. R Pkz.T-Grp" TO DRH-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRH-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  MOVE AR-NUM TO DRH-ARNUM.
           MOVE AR-BEZ TO DRH-ARBEZ.
           ADD 1 WF-MEH GIVING WH-MEH.
           MOVE WT-MEH(WH-MEH) TO DRH-MEH.
           MOVE WF-NK TO DRH-NK.
           MOVE AR-PL TO DRH-PL.
           MOVE WF-TB TO DRH-TB.
           IF AR-APREIS NOT = 0 MOVE AR-APREIS TO AR-PREIS
                                MOVE AR-AVKP TO AR-VKP.
           MOVE AR-PREIS TO WH-PREIS WD-PREIS WD-APREIS.
           IF WR-GR = 0 MOVE WD-PREIS TO DRH-PR(1)
                  ELSE MOVE WD-APREIS TO DRH-PR(1).
           IF AR-EKP NOT = 0 MOVE AR-EKP TO WD-PREIS
                MOVE WD-PREIS TO DRH-PR(2).
           IF AR-VKP NOT = 0 MOVE AR-VKP TO WD-PREIS
                MOVE WD-PREIS TO DRH-PR(3).
           MOVE AR-UST TO DRH-UST.
           MOVE WT-UST(AR-UST) TO DRH-MWST.
           IF AR-RAB = 1 MOVE "n" TO DRH-RAB
                    else MOVE "j" TO DRH-RAB.
           MOVE AR-OEPZ TO DRH-OEPZ.
           MOVE DRH-STR TO WT-TX.
           MOVE WF-GRP TO DRH-TGRP.
           DISPLAY WT-TX WITH SIZE 80 AT 2001.
           PERFORM DRUCK.
           GO D.
       W.  PERFORM END-DRU.
           GO Z.
       X.  PERFORM NO-REC.
       Z.  EXIT.
      **************************************** Kundenstammdatenlisste *
       KU-LI SECTION.
       A.  PERFORM KOPF.
           DISPLAY "Auswahl" AT 0524
                   "ƒƒƒƒƒƒƒ" AT 0024
                   "1 - Stammdaten (alpa.)"  AT 0824
                   "2 - Stammdaten (num.)"   AT 0024
                   "3 - nur Namen (alph.)"   AT 1124
                   "4 - nur Namen (num.)"    AT 0024
                   " bitte wÑhlen Sie: "     AT 1439.
           DISPLAY "<esc/ret-leer>= Abbruch" AT 2301.
           CALL "CAUP" USING "0014581001" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC or WH-NUM = 0 GO W.
           IF not RET GO A.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN DE-KTONR.
           MOVE 1 TO DE-FNR.
           MOVE SPACE TO DE-MCODE WH-EXCEL.
           EVALUATE WH-PG
               WHEN 1
               WHEN 3
               WHEN 5 START DEBITOR KEY NOT < DE-MCODE INVALID
                          PERFORM NO-REC GO Z
                      end-start GO D
               WHEN 2
               WHEN 4
               WHEN 6 START DEBITOR KEY NOT < DE-KEY INVALID
                          PERFORM NO-REC GO Z.
       D.  READ DEBITOR NEXT IGNORE LOCK AT END GO W.
           IF DE-FNR not = 1 GO D.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE X"0400" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "KUNDEN.LST" TO WM-LST.
      *-----------------------------------------> Excelausgabe Kunden <-
           IF WH-EXCEL = SPACE MOVE "EXCKUND.LST" TO WH-EXCEL WT-TX
      *        CALL "CA-TAGE" USING "95CLOSE" WH-CREG
               MOVE "Nummer;Name-1;Name-2;Name-3;etc" TO WT-ADR
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           IF WM-DRU = 1 MOVE 6 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Kundenstammdatenliste" TO DRK-STR.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE 85 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRK-STR PERFORM DRUCK.
           MOVE "Konto-Nr. B e z e i c h n u n g" TO DRK-STR.
           PERFORM DRUCK.
           IF WH-PG > 2 GO E.
           MOVE "Rabatt  Ret.-%  -Basis  -Bonus  Kurzname  Anr Gr ZK EAN
      -        "   Re-Kto.  Tour  Stat. F-Art. Rs." TO DRK-STR(17:).
           PERFORM DRUCK.
       E.  MOVE ALL "ƒ" TO DRK-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  DIVIDE 10 INTO DE-KTONR GIVING DRK-KTONR.
           MOVE DE-BEZ TO DRK-STR(10:).
           INSPECT DRK-STR REPLACING ALL "#" BY ",".
           MOVE DRK-STR TO WT-TX.
           DISPLAY WT-TX WITH SIZE 79 AT 2001.
           PERFORM DRUCK.
           IF WH-PG > 2 GO D.
           MOVE DE-ANR TO DRK-ANR.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DRK-MCODE.
           MOVE DE-KTOART TO DRK-UST.
           MOVE DE-EAN TO DRK-EAN.
           MOVE DE-KOND TO DRK-ZK.
           DIVIDE 10 INTO DE-REKTO GIVING DRK-REKTO.
           MOVE DE-TOUR TO DRK-TOUR.
           MOVE DE-SUB TO DRK-SUB.
           MOVE DE-STATIS TO DRK-STAT.
           MOVE DE-FAKART TO DRK-FA.
           MOVE WT-FA(DE-FAKART) TO DRK-FAX.
           MOVE DE-RAST TO DRK-RAST.
           IF DE-RAB not = 0 MOVE DE-RAB TO DRK-RAB.
           IF DE-RET not = 0 MOVE DE-RET TO DRK-RET.
           IF DE-RBASIS not = 0 MOVE DE-RBASIS TO DRK-RBASIS.
           IF DE-BONUS not = 0 MOVE DE-BONUS TO DRK-BONUS.
           PERFORM DRUCK.
           PERFORM ADR-KUND.
      *------------------------------------->  Ausgabe in Excel-Datei <-
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 47
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 DIVIDE 10 INTO DE-KTONR GIVING WH-NUM
                           MOVE WH-NUM TO WH-WERT
                   WHEN  2 MOVE WXK-NAME1 TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  3 MOVE WXK-NAME2 TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  4 MOVE WXK-NAME3 TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  5 MOVE WXK-STR TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  6 MOVE WXK-PLZL TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  7 MOVE WXK-ORT TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  8 DIVIDE 10 INTO DE-REKTO GIVING WH-NUM
                           MOVE WH-NUM TO WH-WERT
                   WHEN  9 MOVE DE-MCODE TO WT-TX
                           CALL "CAUP" USING "21CONV08" WH-CREG
                   WHEN 10
                   WHEN 11
                   WHEN 12
                   WHEN 13
                   WHEN 14 MOVE SPACE TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 15 DIVIDE 10 INTO DE-REKTO GIVING WH-NUM
                           MOVE WH-NUM TO WH-WERT
                   WHEN 16
                   WHEN 17
                   WHEN 18
                   WHEN 19
                   WHEN 20
                   WHEN 21 MOVE SPACE TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 22 MOVE DE-UID TO WT-TX            *> Uid-Nr.
                   WHEN 23 MOVE DE-ANMERK TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 24
                   WHEN 25
                   WHEN 26
                   WHEN 27
                   WHEN 28
                   WHEN 29 MOVE SPACE TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 30 MOVE DE-GLN TO WH-WERT
                   WHEN 31 MOVE DE-GLN TO WH-WERT
                   WHEN 32 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           GO D.
       W.  PERFORM END-DRU.
           IF WH-EXCEL not = space MOVE SPACE TO WH-EXCEL.
              CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       UMLAUTE SECTION.
       A.  INSPECT WT-TX REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹"
                    ALL "+" by "&".
       Z.  EXIT.
      **************************************** Softline-Export-Ausgabe *
       EXC-AUSGABE SECTION.
       A.  EVALUATE WH-X
               WHEN "N" CALL "CA-TAGE" USING "91NUMER" WH-CREG
               WHEN "A" CALL "CA-TAGE" USING "92ALPHA" WH-CREG
               WHEN "E" CALL "CA-TAGE" USING "94EAN14" WH-CREG
               WHEN "U" CALL "CA-TAGE" USING "96UMLAU" WH-CREG
               WHEN "D" CALL "CA-TAGE" USING "98DATUM" WH-CREG
               WHEN "W" CALL "CA-TAGE" USING "93WRITE" WH-CREG
                            MOVE 50 TO WX.
       Z.  EXIT.
      ************************************** Adressen an Fibu Softline *
       ADR-KUND SECTION.
       A.  MOVE SPACE TO WT-ADR.
           MOVE SPACE TO WXK-PLZL.
           MOVE SPACE TO WXK-ORT WXK-STR WXK-NAME3.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT DE-BEZ REPLACING ALL X"00" BY " ".
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(1) TO WXK-NAME1.
           MOVE WR-ADR(2) TO WXK-NAME2.
           PERFORM HOL-PLZL.
           IF WY > 0 MOVE WR-ADR(WY - 1) TO WXK-STR.
           IF WR-ADR(3) not = space and WR-ADR(3) not = WXK-STR
              MOVE WR-ADR(3) TO WXK-NAME3.
       Z.  EXIT.
      **************************** Postleitzhal und LÑnderkennzeichen *
       HOL-PLZL SECTION.
       A.  MOVE 0 TO WY.
           PERFORM VARYING WZ FROM 3 BY 1 UNTIL WZ > 6
               EVALUATE TRUE
                   WHEN WR-ADR(WZ)(1:2) = "A-" MOVE " " TO WXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:2) = "D-" MOVE "DE" TO WXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:2) = "H-" MOVE "HU" TO WXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:3) = "SL-" MOVE "SL" TO WXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:3) = "HU-" MOVE "HU" TO WXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:4) = "SLO-" MOVE "SL" TO WXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:4) is numeric
                        PERFORM PLZL-ORT
                        MOVE " " TO WXK-LANDKZ.
       Z.  EXIT.
      ***************************************** PLZL und ORT abstellen *
       PLZL-ORT SECTION.
       A.  MOVE WR-ADR(WZ) TO WT-TX.
           MOVE WZ TO WY.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               or WT-TX(WX:1) = SPACE or LOW-VALUES MOVE WX TO WZ.
           MOVE WT-TX(1:WZ) TO WXK-PLZL.
           ADD 2 TO WZ.
           MOVE WT-TX(WZ:) TO WXK-ORT.
       Z.  EXIT.
      *****************************************************************
       TOURSTA SECTION.
       A.  MOVE 99999900 TO ST-KTONR.
           MOVE 0 TO ST-ARNUM WS-BET WZ-SEITE WZ-ZEILEN.
           MOVE WZ-JAHR TO ST-JAHR.
           START STATISTIK KEY NOT < ST-KEY INVALID KEY GO X.
       B.  READ STATISTIK NEXT IGNORE LOCK AT END
               MOVE 9999 TO WM-KTONR GO W.
           IF ST-JAHR NOT = WZ-JAHR GO B.
           IF ST-KTONR < 90000000 MOVE 9999 TO WM-KTONR GO W.
           SUBTRACT 99999900 FROM ST-KTONR GIVING WV-KTONR.
       C.  IF WM-KTONR = 0 MOVE WV-KTONR TO WM-KTONR.
           IF WV-KTONR NOT = WM-KTONR MOVE 0 TO WM-KTONR GO W.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO E.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "TOURST.LST" TO WM-LST.
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "Tour:" TO DRE-STR.
           ADD WM-KTONR 10 GIVING WH-KEY.
       D.  READ KONSTANT INVALID MOVE "Tour gelîscht" TO KO-TOUR.
           IF ZUGRIF PERFORM BESETZT GO D.
           MOVE KO-TOUR TO DRE-TOUR.
           MOVE 56 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRE-STR PERFORM DRUCK.
           MOVE "Art B e z e i c h n u n g         Monat       Jahr
      -        "Preis        Betrag" TO DRE-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRE-STR PERFORM DRUCK.
       E.  MOVE ST-ARNUM TO AR-NUM.
           READ ARTIKEL INVALID KEY MOVE "Artikel gelîscht" TO AR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE AR-GRM TO WF-GRM.
           MOVE AR-NUM TO DRF-ARNUM.
           MOVE AR-BEZ TO DRF-ARBEZ.
           DISPLAY AR-BEZ AT 1020.
           MOVE WZ-MONAT TO WX.
           IF WF-NK = 0 MOVE ST-MENGEN(WX) TO WH-ANZ WS-MGA
               MOVE WS-MGA TO WS-MG.
           IF WF-NK = 1 DIVIDE 10 INTO ST-MENGEN(WX) GIVING WH-ANZ
               MOVE WH-ANZ TO WS-MGB
               MOVE WS-MGB TO WS-MG.
           IF WF-NK = 2 DIVIDE 100 INTO ST-MENGEN(WX) GIVING WH-ANZ
               MOVE WH-ANZ TO WS-MGB
               MOVE WS-MGB TO WS-MG.
           MOVE WS-MG TO DRF-MG(1).
           MOVE 1 TO WY.
           MOVE 0 TO WH-BET.
       F.  ADD ST-MENGEN(WY) TO WH-BET.
           IF WY < WX ADD 1 TO WY GO F.
           IF WF-NK = 0 MOVE WH-BET TO WS-MGA
               MOVE WS-MGA TO DRF-MG(2).
           IF WF-NK = 1 DIVIDE 10 INTO WH-BET GIVING WS-MGB
               MOVE WS-MGB TO DRF-MG(2).
           IF WF-NK = 2 DIVIDE 100 INTO WH-BET GIVING WS-MGB
               MOVE WS-MGB TO DRF-MG(2).
           IF AR-ADAT = 0 OR AR-ADAT > WV-DATUM
                    MOVE AR-PREIS TO WH-PREIS
               ELSE MOVE AR-APREIS TO WH-PREIS.
           MOVE WH-PREIS TO DRF-PREIS.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET.
           ADD WH-BET TO WS-BET.
           MOVE WH-BET TO DRF-BET.
           PERFORM DRUCK.
           GO B.
       W.  IF WZ-ZEILEN NOT = 0 MOVE ALL "ƒ" TO DRE-STR
               PERFORM DRUCK
               MOVE "          S u m m e" TO DRF-ARBEZ
               MOVE WS-BET TO DRF-BET
               PERFORM DRUCK.
           MOVE 0 TO WZ-ZEILEN WS-BET.
           IF WM-KTONR = 0 WRITE DRA-SATZ AFTER PAGE GO C.
           PERFORM END-DRU.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
      *****************************************************************
       KUNDSTA SECTION.
       A.  IF WH-BMS = 1
               DISPLAY "- bitte warten -" with BLINK highlight AT 2331.
           MOVE WM-KTONR TO ST-KTONR WV-KTONR.
           IF WM-KTONR = 0 MOVE 1 TO ST-KTONR.
           MOVE WZ-MONAT TO WX.
           MOVE WZ-JAHR TO WC-JAHR.                        *> lfd. Jahr
           IF WZ-JAHR = 00 MOVE 99 TO WZ-JAHR
           else SUBTRACT 1 FROM WZ-JAHR GIVING WZ-JAHR.
           MOVE WZ-JAHR TO ST-JAHR.                        *> Vorjahr
       B.  MOVE 0 TO WZ WS-BET WS-RET WS-GEW WZ-ZEILEN WZ-SEITE
                        WH-BET WH-MWST.
           DELETE FILE TEMPSTA.
           OPEN I-O TEMPSTA.
           MOVE 0 TO WZ WS-BET WS-RET WS-GEW WZ-ZEILEN WZ-SEITE
                        WH-BET WH-MWST.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 8
               MOVE 0 TO WS-GSUM(WI).
           MOVE 0 TO ST-ARNUM.
           START STATISTIK KEY NOT < ST-KEY INVALID KEY GO Z.
       C.  READ STATISTIK NEXT IGNORE LOCK AT END
                MOVE 1 TO WM-KTONR GO E.
           IF WV-KTONR = 0 MOVE ST-KTONR TO WV-KTONR.
           IF ST-KTONR not = WV-KTONR GO E.                *> Kd-Ende
           IF ST-JAHR not = WZ-JAHR and not = WC-JAHR GO C.
           MOVE ST-KTONR TO TP-KTONR.
           MOVE ST-ARNUM TO TP-ARNUM.
           MOVE ST-ARNUM TO WD-NUM.
           DISPLAY WD-NUM AT 1110.
           READ TEMPSTA INVALID INITIALIZE TP-SATZ
               MOVE ST-KTONR TO TP-KTONR
               MOVE ST-ARNUM TO TP-ARNUM.
           IF ST-JAHR = WZ-JAHR MOVE ST-JAHR TO TP-VJAHR
               ADD ST-MENGEN(WX) TO TP-VSUM(1)
               ADD ST-RET(WX) TO TP-VSUM(3)
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > WX
                   ADD ST-MENGEN(WY) TO TP-VSUM(2)
                   ADD ST-RET(WY) TO TP-VSUM(4).
           IF ST-JAHR = WC-JAHR MOVE ST-JAHR TO TP-LJAHR
               ADD ST-MENGEN(WX) TO TP-LSUM(1)
               ADD ST-RET(WX) TO TP-LSUM(3)
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > WX
                   ADD ST-MENGEN(WY) TO TP-LSUM(2)
                   ADD ST-RET(WY) TO TP-LSUM(4)
                   ADD ST-UMSATZ(WY) TO TP-BET
                   ADD ST-GEWINN(WY) TO TP-MWST.
           IF TP-LSUM(1) = 0 and TP-LSUM(2) = 0 and TP-LSUM(3) = 0
               and TP-LSUM(4) = 0 and TP-VSUM(1) = 0 and TP-VSUM(2) = 0
               and TP-VSUM(3) = 0 and TP-VSUM(4) = 0 GO C.
           WRITE TP-SATZ INVALID REWRITE TP-SATZ.
           GO C.
       E.  MOVE 0 TO WZ WS-BET WS-RET WS-GEW WZ-ZEILEN WZ-SEITE.
           MOVE LOW-VALUE TO TP-SATZ.
           MOVE WV-KTONR TO TP-KTONR.
           START TEMPSTA KEY not < TP-KEY INVALID GO T.
       G.  READ TEMPSTA NEXT IGNORE LOCK AT END GO T.
           IF WF-STATUS not = "00" GO T.
           MOVE TP-ARNUM TO AR-NUM.
           READ ARTIKEL IGNORE LOCK INVALID MOVE "gelîscht" TO AR-BEZ.
           MOVE AR-GRM TO WF-GRM.
           DISPLAY AR-BEZ AT 1010.
           IF WH-BMS = 0 PERFORM STAKO.
           MOVE AR-NUM TO DRR-ARNUM.
           MOVE AR-BEZ TO DRR-ARBEZ.
           MOVE 1 TO WZ.
           ADD TP-LSUM(3) TP-LSUM(1) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(1).                        *> lfd. Mon
           ADD TP-LSUM(4) TP-LSUM(2) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(2).                        *> lfd. Jahr
           IF TP-LSUM(4) NOT = 0
              COMPUTE DRG-PZ(2) ROUNDED = TP-LSUM(4) / TP-LSUM(2) * 100.
           ADD TP-VSUM(3) TP-VSUM(1) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(3).                        *> Vj. Monat
           ADD TP-VSUM(4) TP-VSUM(2) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(4).                        *> Vorjahr
           IF TP-VSUM(4) NOT = 0
              COMPUTE DRG-PZ(3) ROUNDED = TP-VSUM(4) / TP-VSUM(2) * 100.
           ADD TP-LSUM(4) TP-LSUM(2) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WC-MG TO WZ-BET.
           MOVE TP-BET TO DRG-BET.
           ADD TP-BET TO WS-GSUM(WF-TB + 1).
      *------------------------------------------> Durchschnittspreis <-
           DIVIDE WC-MG INTO TP-BET GIVING DRG-PR WH-PREIS.
           ADD WH-BET TO WS-BET.
           MULTIPLY WH-PREIS BY WZ-BET GIVING WZ-BET.
           ADD WZ-BET TO WS-RET.                     *> Wert Retourware
      *------------------------------------> geÑndert fÅr LOH 26.5.94 <-
           COMPUTE TP-MWST = TP-BET - (AR-EKP * WC-MG).
           ADD TP-MWST TO WS-GEW.
           COMPUTE DRG-PZ(1) ROUNDED = WH-MWST / WH-BET * 100.
           IF WH-BMS = 0 PERFORM DRUCK.
           GO G.
       T.  IF WH-BMS = 1 PERFORM BS-AUSGAB GO X.
           IF WZ = 1 MOVE ALL "ƒ" TO DRG-STR
               PERFORM DRUCK
               MOVE "         S u m m e" TO DRF-ARBEZ
               MOVE WS-GEW TO DRG-BET
               MOVE DRG-BET TO DRG-STR(55:13)
               MOVE WS-BET TO DRG-BET
               COMPUTE DRG-PZ(1) ROUNDED = WS-GEW / WS-BET * 100
               MOVE WS-RET TO WD-BET
               MOVE WD-BET TO DRG-STR(104:)
               PERFORM DRUCK
               PERFORM DRU-AUFST.
           CLOSE TEMPSTA.
           DELETE FILE TEMPSTA.
           IF ST-KTONR > 99999900 GO W.
           IF WM-KTONR = 0 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WV-KTONR GO B.
       W.  PERFORM END-DRU.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
      *****************************************************************
       KUNDST-ALT SECTION.
       A.  IF WH-BMS = 1
               DISPLAY "- bitte warten -" with BLINK highlight AT 2331.
           MOVE 0 TO WZ WS-BET WS-RET WS-GEW WZ-ZEILEN WZ-SEITE.
           MOVE WM-KTONR TO ST-KTONR WV-KTONR.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 8
               MOVE 0 TO WS-GSUM(WI).
           MOVE 0 TO ST-ARNUM.
           IF WM-KTONR = 0 MOVE 1 TO ST-KTONR.
           MOVE WZ-MONAT TO WX.
           MOVE WZ-JAHR TO ST-JAHR.
           IF ST-JAHR = 00 MOVE 99 TO ST-JAHR
           else SUBTRACT 1 FROM ST-JAHR GIVING ST-JAHR.
           IF ST-JAHR < 50 ADD 2000 ST-JAHR GIVING WV-JAHR
                      else ADD 1900 ST-JAHR GIVING WV-JAHR.
           START STATISTIK KEY NOT < ST-KEY INVALID KEY GO Z.
       B.  READ STATISTIK NEXT AT END MOVE 1 TO WM-KTONR GO T.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE ST-JAHR TO WS-JAHR.
           IF WS-JAHR < 50 ADD 2000 ST-JAHR GIVING WS-JAHR
                      else ADD 1900 ST-JAHR GIVING WS-JAHR.
           IF WS-JAHR < WV-JAHR GO B.
      *    IF ST-JAHR < WC-JAHR GO B.
       C.  IF WV-KTONR = 0 MOVE ST-KTONR TO WV-KTONR
               MOVE 0 TO WZ WS-BET WS-RET WS-GEW WZ-ZEILEN WZ-SEITE.
           IF ST-KTONR NOT = WV-KTONR GO T.
           MOVE ST-ARNUM TO AR-NUM.
       D.  READ ARTIKEL INVALID MOVE "gelîscht" TO AR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO D.
           MOVE AR-GRM TO WF-GRM.
           DISPLAY AR-BEZ AT 1010.
      *    IF ST-JAHR NOT = WC-JAHR GO G.
           IF WS-JAHR NOT = WV-JAHR GO G.
           IF WH-BMS = 0 PERFORM STAKO.
           MOVE 0 TO WS-SUM(1) WS-SUM(2) WS-SUM(3) WS-SUM(4).
      *---------------------------------------------> Vorjahreswerte <-
           MOVE ST-MENGEN(WX) TO WS-SUM(1).
           MOVE ST-RET(WX) TO WS-SUM(3).
           MOVE 1 TO WY.
       E.  ADD ST-MENGEN(WY) TO WS-SUM(2).
           ADD ST-RET(WY) TO WS-SUM(4).
           IF WY < WX ADD 1 TO WY GO E.
           ADD WS-SUM(3) WS-SUM(1) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(3).
           ADD WS-SUM(4) WS-SUM(2) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(4).
           IF WS-SUM(4) NOT = 0 
              COMPUTE DRG-PZ(3) ROUNDED = WS-SUM(4) / WS-SUM(2) * 100.
      *-------------------------------------------> Summen lfd. Jahr <-
       F.  READ STATISTIK NEXT AT END MOVE 0 TO WZ-DATUM WV-KTONR GO R.
           IF ZUGRIF PERFORM BESETZT GO F.
           MOVE ST-JAHR TO WS-JAHR.
           IF WS-JAHR < 50 ADD 2000 ST-JAHR GIVING WS-JAHR
                      else ADD 1900 ST-JAHR GIVING WS-JAHR.
           IF WS-JAHR NOT = WV-JAHR GO R.
      *    IF ST-JAHR NOT = WZ-JAHR GO R.
           IF ST-KTONR NOT = WV-KTONR GO R.
       G.  IF WH-BMS = 0 PERFORM STAKO.
           MOVE 0 TO WS-SUM(1) WS-SUM(2) WS-SUM(3) WS-SUM(4) 
                     WH-BET WH-MWST.
           MOVE ST-MENGEN(WX) TO WS-SUM(1).
           MOVE ST-RET(WX) TO WS-SUM(3).
           MOVE 1 TO WY.
       J.  MOVE ST-SUM(WY) TO WST-SUM.
           ADD WST-MENGEN TO WS-SUM(2).
           ADD WST-RET TO WS-SUM(4).
           ADD WST-UMSATZ TO WH-BET.
           ADD WST-GEWINN TO WH-MWST.
           IF WY < WX ADD 1 TO WY GO J.
           ADD WS-SUM(3) WS-SUM(1) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(1).
      *--------------------------------> Retourmenge gesamt abstellen <-
           MOVE WS-SUM(4) TO WH-ANZ.
           PERFORM MENGE
           MOVE WC-MG TO WZ-BET.
           ADD WS-SUM(4) WS-SUM(2) GIVING WH-ANZ.        *> Menge
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(2).
           IF WS-SUM(4) NOT = 0
              COMPUTE DRG-PZ(2) ROUNDED = WS-SUM(4) / WS-SUM(2) * 100.
           MOVE WH-BET TO DRG-BET.
           ADD WH-BET TO WS-GSUM(WF-TB + 1).
      *------------------------------------------> Durchschnittspreis <-
           DIVIDE WC-MG INTO WH-BET GIVING DRG-PR WH-PREIS.
           ADD WH-BET TO WS-BET.
           MULTIPLY WH-PREIS BY WZ-BET GIVING WZ-BET.
           ADD WZ-BET TO WS-RET.                     *> Wert Retourware
      *------------------------------------> geÑndert fÅr LOH 26.5.94 <-
           COMPUTE WH-MWST = WH-BET - (AR-EKP * WC-MG).
           ADD WH-MWST TO WS-GEW.
           COMPUTE DRG-PZ(1) ROUNDED = WH-MWST / WH-BET * 100.
       R.  MOVE AR-NUM TO DRG-ARNUM.
           MOVE AR-BEZ TO DRG-ARBEZ.
           MOVE 1 TO WZ.
           IF WH-BMS = 0 PERFORM DRUCK.
           IF WV-KTONR = ST-KTONR GO B.
       T.  IF WH-BMS = 1 PERFORM BS-AUSGAB GO X.
           IF WZ = 1 MOVE ALL "ƒ" TO DRG-STR
               PERFORM DRUCK
               MOVE "         S u m m e" TO DRF-ARBEZ
               MOVE WS-GEW TO DRG-BET
               MOVE DRG-BET TO DRG-STR(55:13)
               MOVE WS-BET TO DRG-BET
               COMPUTE DRG-PZ(1) ROUNDED = WS-GEW / WS-BET * 100
               MOVE WS-RET TO WD-BET
               MOVE WD-BET TO DRG-STR(104:)
               PERFORM DRUCK
               PERFORM DRU-AUFST.
           IF ST-KTONR > 99999900 GO W.
           IF WM-KTONR = 0 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WV-KTONR GO C.
       W.  PERFORM END-DRU.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
      ****************************** Bildschrimausgabe Kundenstatistik *
       BS-AUSGAB SECTION.
       A.  CALL "CAUP" USING "1323012580" WH-CREG.
           CALL "CAUP" USING "0707100865000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " StatstikÅbersicht " AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kd-Nr.: " AT VDU-LP WD-KTO with highlight.
           ADD 1 TO VDU-L.
           MOVE DE-BEZ TO DRG-STR.
           INSPECT DRG-STR REPLACING ALL "#" BY ",".
           DISPLAY DRG-STR(1:60) with highlight AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "  Umsatz-lfd.    Rohgewinn    %    Retourwert    %"
                AT VDU-LP.
           ADD 603 VDU-ECK GIVING VDU-LP.
           MOVE WS-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           ADD 617 VDU-ECK GIVING VDU-LP.
           MOVE WS-GEW TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           COMPUTE WD-MGC ROUNDED = WS-GEW / WS-BET * 100
           ADD 631 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-MGC with highlight AT VDU-LP.
           ADD 636 VDU-ECK GIVING VDU-LP.
           MOVE WS-RET TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           ADD 650 VDU-ECK GIVING VDU-LP.
           COMPUTE WD-MGC ROUNDED = WS-RET / WS-BET * 100.
           DISPLAY WD-MGC with highlight AT VDU-LP.
           DISPLAY "weiter mit beliebiger Taste < >" AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************* Kundenstatistikkopf *
       STAKO SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0400" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "KD-STAT.LST" TO WM-LST.
           MOVE "&l26a1h6d0O(s0p14.8h0b0s6T" TO WE-STG(10).
           IF WM-DRU = 1 MOVE 10 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "K u n d e n s t a t i s t i k" TO DRG-STR.
           MOVE 94 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE 1 TO DE-FNR.
           MOVE WV-KTONR TO DE-KTONR.
       C.  READ DEBITOR INVALID MOVE "Kunde fehlt" TO DE-BEZ.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE DE-BEZ TO DRG-STR.
           INSPECT DRG-STR REPLACING ALL "#" BY ",".
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 0722.
           MOVE DRG-STR TO WT-TX.
           DISPLAY WT-TX WITH SIZE 70 AT 0810.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRG-STR PERFORM DRUCK.
           MOVE "Art  Bezeichnung                  Monat       Jahr   Vj
      -        "-Monat    Vorjahr  Umsatz lfd. Rohgew. Rwa.   RwVJ. D-Pr
      -        "eis" TO DRG-STR PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRG-STR PERFORM DRUCK.
       Z.  EXIT.
      *****************************************************************
       RETSTAT SECTION.
       A.  MOVE 0 TO ST-ARNUM.
           MOVE WM-KTONR TO ST-KTONR.
           IF WM-KTONR = 0 MOVE 1 TO ST-KTONR.
           MOVE WZ-MONAT TO WX.
           MOVE WZ-JAHR TO ST-JAHR.
           START STATISTIK KEY NOT < ST-KEY INVALID KEY GO Z.
       B.  READ STATISTIK NEXT AT END MOVE 1 TO WM-KTONR GO T.
           IF ZUGRIF PERFORM BESETZT GO B.
       C.  IF WV-KTONR = 0 MOVE ST-KTONR TO WV-KTONR
               MOVE 0 TO WZ WZ-BET WS-BET WS-RET WZ-ZEILEN WZ-SEITE.
           IF WM-KTONR NOT = 0; IF WM-KTONR NOT = ST-KTONR GO T.
           IF ST-KTONR NOT = WV-KTONR GO T.
           IF ST-JAHR NOT = WZ-JAHR GO B.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 12
               IF ST-MENGEN(WY) NOT = 0 or ST-RET(WY) NOT = 0 or
                   ST-UMSATZ(WY) NOT = 0 or ST-GEWINN(WY) NOT = 0
                   MOVE 50 TO WY
               end-perform
               IF WY < 50 close statistik
                          open i-o statistik
                          DELETE STATISTIK
                          close statistik
                          open input statistik.
           MOVE ST-ARNUM TO AR-NUM.
       D.  READ ARTIKEL INVALID MOVE "gelîscht" TO AR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO D.
           MOVE AR-GRM TO WF-GRM.
           DISPLAY AR-BEZ AT 1010.
           PERFORM RETKO.
           MOVE ST-SUM(WX) TO WST-SUM.
           MOVE WST-MENGEN TO WH-ANZ.
           PERFORM MENGE MOVE WS-MG TO DRR-MG(1).
           MOVE WST-RET TO WH-ANZ.
           PERFORM MENGE MOVE WS-MG TO DRR-MG(2).
           ADD WST-MENGEN WST-RET GIVING WH-ANZ.
           MOVE WST-UMSATZ TO DRR-BET.
           ADD WST-UMSATZ TO WZ-BET.
           COMPUTE WH-PREIS ROUNDED = WST-UMSATZ / WH-ANZ.
           COMPUTE WH-BET ROUNDED = WST-MENGEN * WH-PREIS.
           ADD WH-BET TO WS-BET.
           COMPUTE WH-RET ROUNDED = WST-RET * WH-PREIS.
           ADD WH-RET TO WS-RET.
           IF WH-RET NOT = 0
              COMPUTE DRR-PZ ROUNDED = WH-RET / WH-BET * 100.
       R.  MOVE AR-NUM TO DRR-ARNUM.
           MOVE AR-BEZ TO DRR-ARBEZ.
           MOVE 1 TO WZ.
           PERFORM DRUCK.
           if wm-l = 1
               move 0 to WST-MENGEN WST-RET WST-UMSATZ WST-GEWINN
               move wst-sum to st-sum(wx)
               rewrite st-satz.
           IF WV-KTONR = ST-KTONR GO B.
       T.  IF WZ = 1 MOVE ALL "ƒ" TO DRR-STR
               PERFORM DRUCK
               MOVE "         S u m m e" TO DRF-ARBEZ
               MOVE WS-BET TO DRR-BET
               MOVE DRR-BET TO DRR-MG(1)
               MOVE WS-RET TO DRR-BET
               MOVE DRR-BET TO DRR-MG(2)
               MOVE WZ-BET TO DRR-BET
               COMPUTE DRR-PZ ROUNDED = WS-RET / WS-BET * 100
               PERFORM DRUCK.
           IF WM-KTONR = 0 WRITE DRA-SATZ AFTER PAGE GO C.
       W.  PERFORM END-DRU.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
      ************************************ Mengenumwandlung-Statistik *
       MENGE SECTION.
       A.  EVALUATE WF-NK
               WHEN 0 MOVE WH-ANZ TO WS-MGA WC-MG
                      MOVE WS-MGA TO WS-MG
               WHEN 1 DIVIDE 10 INTO WH-ANZ GIVING WS-MGB WC-MG
                      MOVE WS-MGB TO WS-MG
               WHEN 2 DIVIDE 100 INTO WH-ANZ GIVING WS-MGB WC-MG
                      MOVE WS-MGB TO WS-MG.
       Z.  EXIT.
      ****************************************** Retourenstatistikkopf *
       RETKO SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "RETOUR.LST" TO WM-LST.
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "R e t o u r e n s t a t i s t i k" TO DRR-STR.
           MOVE 59 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE 1 TO DE-FNR.
           MOVE ST-KTONR TO DE-KTONR.
       C.  READ DEBITOR INVALID MOVE "Kunde fehlt" TO DE-BEZ.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE DE-BEZ TO DRR-STR.
           INSPECT DRR-STR REPLACING ALL "#" BY ",".
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 0722.
           MOVE DRR-STR TO WT-TX.
           DISPLAY WT-TX WITH SIZE 70 AT 0810.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRR-STR PERFORM DRUCK.
           MOVE "Art Bezeichnung                  geliefert      retour
      -        "        Umsatz   RW-%" TO DRR-STR PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRR-STR PERFORM DRUCK.
       Z.  EXIT.
      *****************************************************************
       ARTSTA SECTION.
       A.  MOVE 0 TO WS-BET WS-RET WZ-SEITE WZ-ZEILEN.
           if wm-l = 1 close statistik
                       open i-o statistik.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 8
               MOVE 0 TO WS-GSUM(WI).
           MOVE WZ-MONAT TO WX.
           IF WH-MOD = 3 PERFORM PRODSTA GO Z.
           MOVE SPACE TO AR-AKEY.
           START ARTIKEL KEY NOT < AR-AKEY INVALID KEY GO X.
       B.  READ ARTIKEL NEXT IGNORE LOCK AT END GO K.
           MOVE AR-GRM TO WF-GRM.
980430     IF WF-TB > 3 MOVE 2 TO WF-TB.             *> BÑckereiartikel
           IF WH-PG = 11 and WF-TB not = 3 GO B.
           IF AR-SA NOT = SPACE MOVE 10 TO WF-STATUS GO K.
           DISPLAY AR-BEZ AT 2010.
           MOVE 0 TO WS-SUM(1) WS-SUM(2) WS-SUM(3) WS-SUM(4).
           INITIALIZE AB-SATZ.
           MOVE AR-NUM TO ST-ARNUM.
           MOVE 0 TO ST-KTONR.
           IF WZ-JAHR = 0 MOVE 99 TO ST-JAHR WV-JAHR
           else SUBTRACT 1 FROM WZ-JAHR GIVING ST-JAHR WV-JAHR.
       C.  READ STATISTIK INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE ST-MENGEN(WX) TO WS-SUM(1).
           MOVE ST-RET(WX) TO WS-SUM(3).
           MOVE 1 TO WY.
       D.  ADD ST-MENGEN(WY) TO WS-SUM(2).
           ADD ST-RET(WY) TO WS-SUM(4).
           IF WY < WX ADD 1 TO WY GO D.
       E.  IF ST-JAHR = 99 MOVE 0 TO ST-JAHR
                      else ADD 1 TO ST-JAHR.
       F.  READ STATISTIK INVALID GO B.
           IF ZUGRIF PERFORM BESETZT GO F.
           IF WH-MOD = 0 PERFORM AR-KO.
       H.  MOVE AR-NUM TO DRG-ARNUM AB-ARNUM.
           MOVE AR-BEZ TO DRG-ARBEZ AB-ARBEZ.
           ADD WS-SUM(3) WS-SUM(1) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(3) AB-MG(3).
           ADD WS-SUM(4) WS-SUM(2) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(4) AB-MG(4).
           IF WS-SUM(4) NOT = 0 
              COMPUTE WH-PRZ ROUNDED = WS-SUM(4) / WS-SUM(2) * 100
              MOVE WH-PRZ TO DRG-PZ(3) AB-PZ(3).
      *--------------------------------------------> Summen lfd. Jahr <-
           MOVE 0 TO WS-SUM(1) WS-SUM(2) WS-SUM(3) WS-SUM(4) 
                     WH-BET WH-MWST.
           MOVE ST-MENGEN(WX) TO WS-SUM(1).
           MOVE ST-RET(WX) TO WS-SUM(3).
           MOVE 1 TO WY.
       J.  MOVE ST-SUM(WY) TO WST-SUM.
           ADD WST-MENGEN TO WS-SUM(2).
           ADD WST-RET TO WS-SUM(4).
           if wm-l = 1 add wst-mengen wst-ret giving wh-wert
               multiply ar-ekp by wh-wert giving wh-wert
               subtract wh-wert from wst-umsatz giving wst-gewinn
               move wst-sum to st-sum(wy).
           ADD WST-UMSATZ TO WH-BET.
           ADD WST-GEWINN TO WH-MWST.
           IF WY < WX ADD 1 TO WY GO J.
           if wm-l = 1 rewrite st-satz.
           ADD WS-SUM(3) WS-SUM(1) GIVING WH-ANZ.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(1) AB-MG(1).
           ADD WS-SUM(4) WS-SUM(2) GIVING WH-ANZ AB-BASIS.
           PERFORM MENGE.
           MOVE WS-MG TO DRG-MG(2) AB-MG(2).
           IF WS-SUM(4) NOT = 0
              COMPUTE WH-PRZ ROUNDED = WS-SUM(4) / WS-SUM(2) * 100
              MOVE WH-PRZ TO DRG-PZ(2) AB-PZ(2).
           MOVE WH-BET TO DRG-BET AB-BET.
           ADD WS-SUM(4) WS-SUM(2) GIVING WH-ANZ.
           DIVIDE WC-MG INTO WH-BET GIVING DRG-PR AB-PR.
           ADD WH-BET TO WS-BET.
      *    IF WF-TB > 0 ADD WH-BET TO WS-GSUM(WF-TB).
           ADD WH-BET TO WS-GSUM(WF-TB + 1).
           ADD WH-MWST TO WS-RET.
           COMPUTE WH-PRZ ROUNDED = WH-MWST / WH-BET * 100.
           MOVE WH-PRZ TO DRG-PZ(1) AB-PZ(1).
           IF WH-MOD = 0 PERFORM DRUCK GO B.
           IF WH-MOD = 2 MOVE AB-BET   TO AB-BASIS.
           IF AB-BASIS = 0 GO B.
           SUBTRACT AB-BASIS FROM 1000000000 GIVING AB-BASIS.
           WRITE AB-SATZ.
           GO B.
       K.  IF WH-MOD = 0 or WH-MOD = 3 GO W.
           MOVE LOW-VALUE TO AB-SATZ.
           START ABCDATEI KEY NOT < AB-KEY INVALID GO X.
       M.  READ ABCDATEI NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO M.
           PERFORM AR-KO.
           MOVE AB-ARNUM TO DRG-ARNUM.
           MOVE AB-ARBEZ TO DRG-ARBEZ.
           MOVE AB-MG(3) TO DRG-MG(3).
           MOVE AB-MG(4) TO DRG-MG(4).
           MOVE AB-PZ(3) TO DRG-PZ(3).
           MOVE AB-MG(1) TO DRG-MG(1).
           MOVE AB-MG(2) TO DRG-MG(2).
           MOVE AB-PZ(2) TO DRG-PZ(2).
           MOVE AB-BET TO DRG-BET.
           MOVE AB-PR TO DRG-PR.
           MOVE AB-PZ(1) TO DRG-PZ(1).
           PERFORM DRUCK.
           GO M.
       Q.  CLOSE ABCDATEI.
           DELETE FILE ABCDATEI.
       W.  PERFORM AR-ENDE.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
      ****************************************** Ende Artikelstatisitk *
       AR-ENDE SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE ALL "ƒ" TO DRG-STR PERFORM DRUCK.
           MOVE "         S u m m e" TO DRF-ARBEZ.
           MOVE WS-BET TO DRG-BET.
           COMPUTE DRG-PZ(1) ROUNDED = WS-RET / WS-BET * 100.
           PERFORM DRUCK.
           PERFORM DRU-AUFST.
           PERFORM END-DRU.
       Z.  EXIT.
      ********************************************* Summen-Aufstellung *
       DRU-AUFST SECTION.
       A.  MOVE 2 TO WZ-SCHALT.
           MOVE "Sonst." TO DRG-STR(86:).
           MOVE WS-GSUM(1) TO DRG-BET.
           PERFORM DRUCK.
           MOVE "Brot" TO DRG-STR(86:).
           MOVE WS-GSUM(2) TO DRG-BET.
           PERFORM DRUCK.
           MOVE "GebÑck" TO DRG-STR(86:).
           MOVE WS-GSUM(3) TO DRG-BET.
           PERFORM DRUCK.
           MOVE "Konditorei" TO DRG-STR(86:).
           MOVE WS-GSUM(4) TO DRG-BET.
           MOVE 0 TO WS-GSUM(1) WS-GSUM(2) WS-GSUM(3) WS-GSUM(4).
           PERFORM DRUCK.
       Z.  EXIT.
      ****************************************** Kopf Artikelstatistik *
       AR-KO SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0400" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "ARTSTAT.LST" TO WM-LST.
           MOVE "&l26a1h6d0O(s0p14.8h0b0s6T" TO WE-STG(10).
           IF WM-DRU = 1 MOVE 10 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "A r t i k e l s t a t i s t i k" TO DRG-STR.
           IF WH-MOD = 1 MOVE "ABC-Mengen" TO DRG-STR(37:12).
           IF WH-MOD = 2 MOVE "ABC-Umsatz" TO DRG-STR(37:12).
           MOVE 94 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRG-STR PERFORM DRUCK.
           MOVE "Art Bezeichnung                   Monat       Jahr   Vj
      -        "-Monat    Vorjahr  Umsatz lfd. Rohgew. Rwa.   RwVJ. D-Pr
      -        "eis" TO DRG-STR PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRG-STR PERFORM DRUCK.
       Z.  EXIT.
      *************************************************** Gruppensumme *
       GRUPSUM SECTION.
       A.  MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
           MOVE WV-OEPZ TO DRS-ARNUM.
           MOVE "*" TO DRS-ARNUM(1:2).
           ADD WV-OEPZ 10 GIVING WH-KEY.
       C.  READ KONSTANT INVALID MOVE SPACE TO KO-SBEZ KO-STATNR.
           IF ZUGRIF PERFORM BESETZT GO C.
           UNLOCK KONSTANT.
           IF KO-SBEZ = SPACE MOVE "- Gruppe fehlt -" TO KO-SBEZ.
           MOVE KO-SBEZ TO DRS-ARBEZ.
           MOVE WS-GSUM(1) TO DRS-MPKG.
           MOVE WS-GSUM(2) TO DRS-MPS.
           MOVE WS-GSUM(3) TO DRS-JPKG.
           MOVE WS-GSUM(4) TO DRS-JPS.
           PERFORM DRUCK.
           MOVE WS-GSUM(5) TO DRS-MPKG.
           MOVE WS-GSUM(6) TO DRS-MPS.
           MOVE WS-GSUM(7) TO DRS-JPKG.
           MOVE WS-GSUM(8) TO DRS-JPS.
           PERFORM DRUCK.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 8
               MOVE 0 TO WS-GSUM(WI).
           MOVE 2 TO WZ-SCHALT.
           MOVE AR-OEPZ TO WV-OEPZ.
       Z.  EXIT.
      ******************************************************************
       PRODSTA SECTION.
       A.  MOVE 0 TO WV-OEPZ.
           MOVE 1 TO AR-OEPZ.
           START ARTIKEL KEY NOT < AR-CKEY INVALID KEY GO X.
       B.  READ ARTIKEL NEXT IGNORE LOCK AT END GO K.
           IF WV-OEPZ = 0 MOVE AR-OEPZ TO WV-OEPZ.
           IF WV-OEPZ not = AR-OEPZ PERFORM GRUPSUM.
      *--------------------------------> Gruppensumme drucken <-
           MOVE AR-GRM TO WF-GRM.
           IF AR-SA NOT = SPACE MOVE 10 TO WF-STATUS GO K.
           DISPLAY AR-BEZ AT 2010.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 8
               MOVE 0 TO WS-SUM(WI).
           MOVE AR-NUM TO ST-ARNUM.
           MOVE 0 TO ST-KTONR.
           MOVE WZ-JAHR TO ST-JAHR.
       C.  READ STATISTIK INVALID MOVE 0 TO WV-OEPZ GO B.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE ST-MENGEN(WX) TO WS-SUM(1).              *> Monatssumme
           MOVE ST-RET(WX) TO WS-SUM(2).
           MOVE ST-UMSATZ(WX) TO WS-SUM(3).
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > WX
               ADD ST-MENGEN(WY) TO WS-SUM(4)            *> Jahressumme
               ADD ST-RET(WY) TO WS-SUM(5)
               ADD ST-UMSATZ(WY) TO WS-SUM(6).
           PERFORM PSTAT-KO.
           MOVE AR-NUM TO DRS-ARNUM.
           MOVE AR-BEZ TO DRS-ARBEZ.
      *----------------------- Produktion ----> Monatssumme kg und îS <-
           MOVE WS-SUM(1) TO WH-ANZ.
           PERFORM MENGE.
           COMPUTE WS-GEW = WC-MG * AR-ANZ / 1000.
           MOVE WS-GEW TO DRS-MPKG.
           ADD WS-GEW TO WS-GSUM(1).
           ADD WS-SUM(1) WS-SUM(2) GIVING WH-ANZ.
           COMPUTE WS-BET = WS-SUM(3) / WH-ANZ * WS-SUM(1).
           MOVE WS-BET TO DRS-MPS.
           ADD WS-BET TO WS-GSUM(2).
      *----------------------- Produktion ----> Jahressumme kg und îS <-
           MOVE WS-SUM(4) TO WH-ANZ.
           PERFORM MENGE.
           COMPUTE WS-GEW = WC-MG * AR-ANZ / 1000.
           MOVE WS-GEW TO DRS-JPKG.
           ADD WS-GEW TO WS-GSUM(3).
           ADD WS-SUM(4) WS-SUM(5) GIVING WH-ANZ.
           COMPUTE WS-BET = WS-SUM(6) / WH-ANZ * WS-SUM(4).
           MOVE WS-BET TO DRS-JPS.
           ADD WS-BET TO WS-GSUM(4).
           PERFORM DRUCK.
      *----------------------- Verkauf -------> Monatssumme kg und îS <-
           ADD WS-SUM(1) WS-SUM(2) GIVING WH-ANZ.
           PERFORM MENGE.
           COMPUTE WS-GEW = WC-MG * AR-ANZ / 1000.
           MOVE WS-GEW TO DRS-MPKG.
           ADD WS-GEW TO WS-GSUM(5).
           MOVE WS-SUM(3) TO DRS-MPS.
           ADD WS-SUM(3) TO WS-GSUM(6).
      *----------------------- Verkauf -------> Jahressumme kg und îS <-
           ADD WS-SUM(4) WS-SUM(5) GIVING WH-ANZ.
           PERFORM MENGE.
           COMPUTE WS-GEW = WC-MG * AR-ANZ / 1000.
           MOVE WS-GEW TO DRS-JPKG.
           ADD WS-GEW TO WS-GSUM(7).
           MOVE WS-SUM(6) TO DRS-JPS.
           ADD WS-SUM(6) TO WS-GSUM(8).
           PERFORM DRUCK.
           GO B.
       K.  PERFORM GRUPSUM.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-STR.
           PERFORM DRUCK.
           PERFORM END-DRU.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
      ****************************************** Kopf Artikelstatistik *
       PSTAT-KO SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "OEPZSTAT.LST" TO WM-LST.
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "Statistisches Zentralamt" TO DRG-STR.
           MOVE 60 TO WL.
           MOVE 1 TO WI PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
           MOVE "Art. Bezeichnung    Produktion     kg   monatlich   îS
      -        "      kg   jÑhrlich    îS " TO DRS-STR PERFORM DRUCK.
           MOVE "                    Verkauf        kg   monatlich   îS
      -        "      kg   jÑhrlich    îS " TO DRS-STR PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
       Z.  EXIT.
      ******************************************* Kundenetikettendruck *
       ETKDRU SECTION.
       A.  CALL "CAUP" USING "0707201048000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druck Kundenetiketten " with highlight AT VDU-LP.
       C.  ADD 306 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - alle Kunden"  AT VDU-LP.
           MOVE 0 TO VDU-L.
           DISPLAY "2 - Einzelkunden" AT VDU-LP
                "3 - nach Touren"  AT VDU-LP
                "leer = Programmende" AT VDU-LP
                "         bitte wÑhlen Sie:  " with highlight AT VDU-LP.
       D.  CALL "CAUP" USING "1007331001" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC OR WH-PG = 0 OR WH-PG > 3 GO X.
           IF NOT RET GO D.
       E.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 305 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO DE-KTONR WX.
           MOVE 1 TO DE-FNR.
           IF WH-PG = 1 DISPLAY "alle Kunden" AT VDU-LP GO Q.
           IF WH-PG = 2 DISPLAY "ab Kd-Nr.:           bis Kd-Nr.:"
               AT VDU-LP GO G.
           IF WH-PG = 3 DISPLAY "Tour:" AT VDU-LP.
       F.  DISPLAY "<ret>= Tournummer (1 - 20)" AT 2301.
           CALL "CAUP" USING "1003112002" WH-CREG.
           IF WH-NUM > 20 OR < 1 GO F.
           IF ESC GO X.
           IF NOT RET GO F.
           MOVE WH-NUM TO WV-KTONR.
           GO Q.
       G.  DISPLAY "<ret>= Kunden-Nr., <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "1003166006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO G.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       H.  DISPLAY "<ret>= Kunden-Nr., <esc>= Abbruch, < >= zurÅck"
               AT 2301.
           CALL "CAUP" USING "1003386006" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RET GO G.
           COMPUTE WV-KTONR = WH-NUM * 10 + 2.
           DIVIDE 10 INTO WV-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           IF WV-KTONR < DE-KTONR GO H.
       Q.  DISPLAY "<ret>= Start, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO Q.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           START DEBITOR KEY > DE-KEY INVALID PERFORM NO-REC GO W.
       R.  READ DEBITOR NEXT IGNORE LOCK AT END GO W.
           IF WH-PG = 3; IF WV-KTONR NOT = DE-TOUR GO R.
           IF WH-PG = 2; IF DE-KTONR > WV-KTONR GO X.
           ADD 503 VDU-ECK GIVING VDU-LP.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ with SIZE 40 AT VDU-LP.
           MOVE DE-BEZ TO WT-BEZ.
           SET FX TO 5.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE 1 TO WZ-SCHALT.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRA-SATZ(31:).
           PERFORM DRUCK.
           SET TX TO 1.
           MOVE WT-TXT(DE-ANR + 1) TO DRA-SATZ(8:).
           PERFORM DRUCK.
       S.  MOVE WR-ADR(TX) TO DRA-SATZ(8:).
           IF TX = 3 MOVE SPACE TO DRA-SATZ.
           PERFORM DRUCK.
           IF TX < 5 SET TX UP BY 1 GO S.
           MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
           GO R.
       W.  IF WM-OPEN = 1 CLOSE DRUCKER
               MOVE 0 TO WM-OPEN.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       STATKOR SECTION.
       A.  OPEN I-O STATISTIK.
           MOVE 0 TO ST-KTONR ST-ARNUM ST-JAHR.
           DISPLAY " -- bitte warten --" with highlight BLINK AT 2425.
           START STATISTIK KEY NOT < ST-KEY INVALID GO X.
       C.  READ STATISTIK NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           DISPLAY ST-ARNUM AT 2220 " " ST-KTONR.
           MOVE ST-ARNUM TO AR-NUM.
       E.  READ ARTIKEL INVALID MOVE 0 TO AR-EKP.
           IF ZUGRIF PERFORM BESETZT GO E.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 12
               MOVE ST-SUM(WY) TO WST-SUM
               ADD WST-MENGEN WST-RET GIVING WH-WERT
               MULTIPLY AR-EKP BY WH-WERT GIVING WH-WERT
               SUBTRACT WH-WERT FROM WST-UMSATZ GIVING WST-GEWINN
               MOVE WST-SUM TO ST-SUM(WY).
           REWRITE ST-SATZ.
           GO C.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
