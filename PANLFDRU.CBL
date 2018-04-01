      $SET LINKCOUNT"384" ANS85 BOUND AUTOLOCK NOALTER
      $SET SEGSIZE "50000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PANLFDRU.
      ******************************************************************
      *  Bei vorhandenem SET-Parameter PANBS werden die Lieferscheine
      *  getrennt nach "B" = Brot/GebÑck, und "K" = Konditorei
      *  gedruckt.
      *  Version Kern: WE-LFS = 1
      *  DUSD.BAT + DUSD.FTP = ftp-Dateien fÅr
      *                        FTP-öbetragung DUSSMANN desadv
      *  DUSI.BAT + DUSI.FTP = ftp-Dateien fÅr
      *                        FTP-öbetragung DUSSMANN invoic
      *  bei KôNIG u. ALTEBURGER LF-SORT = 0 U. LF-ARBEZ = SPACE
      *-----------------------------------------------------------------
      *  Lieferscheine auf Nadeldrucker                               <-
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                         ENVIRONMENT-NAME IS ENV-SET
                         ENVIRONMENT-VALUE IS ENV-WERT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEART.CPY.
           COPY PANSETIK.CPY.
           COPY PANSEDEB.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSEKON.CPY.
           COPY PANSELFS.CPY.
           SELECT KOMMSCH    ASSIGN TO "KERNKOM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY KS-KEY
                             ALTERNATE RECORD KS-KOMKEY =
                                     KS-SORT KS-ARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT LIEFKO     ASSIGN TO "DRUKOPF.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT LISTMOD    ASSIGN TO WN-PL
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT ZUSHILF    ASSIGN TO "PANHILF.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ZH-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT MODEMDAT   ASSIGN TO "MODEM.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANFDART.CPY.
       COPY PANFETIK.CPY.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANLIEF.CPY.
       COPY PANFAKT.CPY.
      ******************************************************************
       FD  ZUSHILF                     LABEL RECORD STANDARD.
       01  ZH-SATZ.
           03  ZH-KEY.
               05 ZH-KTONR             PIC 9(6)      COMP.
           03  ZH-HMG                  PIC 9(5)V99   COMP  OCCURS 40.
           03  ZH-FILBEZ               PIC X(12).
      ************************************************ Kern - Filliste *
       FD  KOMMSCH                     LABEL RECORD STANDARD.
       01  KS-SATZ.
           03  KS-SORT.
               05 KS-SGRP              PIC 99        COMP-X.
               05 KS-ARBEZ             PIC X(9).
           03  KS-KEY.
               05 KS-ARNUM             PIC 9(4)      COMP.
           03  KS-TAB.
               05 KS-KTONR             PIC 9(6)      COMP OCCURS 15.
               05 KS-MENGE             PIC S9(5)     COMP OCCURS 15.
               05 KS-MENGEB            PIC S9(5)     COMP OCCURS 15.
      *********************************************** Lieferscheinkopf *
       FD  LIEFKO                      LABEL RECORD STANDARD.
       01  TK-SATZ.
           03  TK-KOPF                 PIC X(78).
      ********************************* Listmodule Hofer / Mondo / etc *
       FD  LISTMOD                     LABEL RECORD STANDARD.
       01  LM-SATZ.
           03  LM-SA                   PIC X.
           03  LM-ZUS                  PIC X.
           03  LM-TX                   PIC X(78).
      ******************************************* Modemdaten fÅr HOFER *
       FD  MODEMDAT                    LABEL RECORD STANDARD.
       01  MD-ASATZ.
           03  MD-SA                   PIC 99.
           03  MD-FILLER               PIC 99.
           03  MD-DATUM                PIC 9(6).
           03  FILLER                  PIC X(118).
       01  MD-BSATZ.
           03  FILLER                  PIC 9(5).
           03  MD-FILNR                PIC 99.
           03  FILLER                  PIC X(121).
       01  MD-CSATZ.
           03  FILLER                  PIC 99.
           03  MD-LFDAT                PIC 9(6).
       01  MD-CSATZ.
           03  FILLER                  PIC 99.
           03  MD-ART                  PIC 9999.
           03  MD-ANZ                  PIC 9999V9.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(6).
           03  DRA-TX.
               05 DRA-ARNUM            PIC Z(12)9-.
               05 FILLER               PIC X.
               05 DRA-BEZ              PIC X(26).           *> kern
               05 DRA-MENGE            PIC X(12).
               05 DRA-MEH              PIC XXX.
               05 FILLER               PIC X(17).           *> kern
               05 DRA-BETRAG           PIC ZZZZ.ZZ9,99-.
           03  FILLER                  PIC X(94).
       01  DRT-SATZ.
           03  FILLER                  PIC X(4).
           03  DRT-STR.
               05 DRT-KTONR            PIC ZZ.ZZ9,9-.
               05 DRT-TSUB             PIC ZZZ9-.
               05 DRT-BEZ              PIC X(70).
       01  DRH-SATZ.
           03  FILLER                  PIC XXX.
           03  DRH-STR.
               05 DRH-BEZ              PIC X(16).
               05 DRH-MENGE            PIC X(10)         OCCURS 9.
       01  DRE-SATZ                    PIC X(70).
       01  DRK-SATZ.
           03  FILLER                  PIC XX.
           03  DRK-STR.
               05 DRK-SGRP             PIC ZZ9-.
               05 DRK-ARNUM            PIC ZZ9-.
               05 DRK-BEZ              PIC X(25).
               05 DRK-MG               PIC X(8)          OCCURS 6.
               05 FILLER               PIC X.
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
       01  WT-DK     external.
           03  WR-TN                   PIC X(10)  OCCURS 7.
           03  WT-KON                  PIC 99     OCCURS 12.
       01  WH-REG.
           03  WDR-TST.
               05 WDR-TOUR                               OCCURS 20.
                  07 WDR-STAT          PIC 999    COMP   OCCURS 3.
           03  WDR-ANZ                 PIC 999.
           03  WE-EXT                  PIC XXX.         *> EDIFACT-Ext.
           03  WE-EXTN                 PIC X(7).
           03  WE-EXTV.
               05 WR-VAL               PIC 9.
               05 WR-ZUS               PIC X.
           03  WM-PANBS                PIC 99     COMP.
           03  WM-LFS                  PIC 9(3)   COMP.
           03  WM-LIKO                 PIC X(15).
           03  WM-L                    PIC 99     COMP.
           03  WN-PL.
               05 WN-FIL               PIC X(8)    VALUE "PANLIMO.".
               05 WN-KZ                PIC 9.
           03  WH-PX                   PIC XX      OCCURS 2.
           03  WM-OPEN                 PIC 9      COMP-3 VALUE ZERO.
           03  VDU-AB                  PIC 99     COMP.
           03  WH-KB                   PIC 99     COMP.
           03  WM-Z                    PIC 99     COMP-X.
           03  WX-PRNO                 PIC 99     COMP-X VALUE ZERO.
           03  WX-PRSTAT               PIC 99     COMP-X.
           03  WZ-SEITE                PIC 99     COMP-X VALUE ZERO.
           03  WZ-SCHALT               PIC 99     COMP-X VALUE ZERO.
           03  WZ-ZEILEN               PIC 99     COMP-X VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WV-ARNUM                PIC 9(4)   COMP   VALUE ZERO.
           03  WV-TOUR                 PIC 99     COMP   VALUE ZERO.
           03  WV-KTONR                PIC 9(6)   COMP   VALUE ZERO.
           03  WM-KTONR                PIC 9(6)   COMP   VALUE ZERO.
           03  WV-LFNUM                PIC 9(5)   COMP   VALUE ZERO.
           03  WM-NUM.
               05 WM-KTO               PIC 9(5).
           03  WM-VERGL                PIC 9(5)      COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WH-ANZ                  PIC S9(8)V99  COMP.
           03  WS-ETIK                 PIC S9(8)V99  COMP.
           03  WD-ETIK                 PIC Z.ZZ9-.
           03  WI                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  WM-KZ                   PIC X.
           03  WD-KZ                   PIC 9.
           03  WD-LF                   PIC 99.
           03  WD-LFNUM                PIC ZZ.ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WD-NUM                  PIC ZZZ.ZZ9-.
           03  WD-ART                  PIC ZZZ9.
           03  WD-POS                  PIC ZZ9.
           03  WDR-SUM                 PIC Z.ZZ9.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  WH-MEH                  PIC 99         COMP.
           03  WM-KOND                 PIC 99         COMP.
           03  WH-LFSATZ               PIC X(75).
           03  WK-LFSATZ               PIC X(75).
           03  WK-GS                   PIC 999.
           03  WK-GSNR                 PIC 9(4).
           03  WK-DAT                  PIC X(8).
           03  WF-FILTAB                                  OCCURS 14.
               05 WF-FNR               PIC 9(4)     COMP.
               05 WF-FBEZ              PIC X(7).
               05 WF-SEITE             PIC 9.
      *----------------------------------------------> Command-Line <-
           03  WD-UPON                 PIC X(60).
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WD-GRP                  PIC ZZ.
           03  WH-PARM                 PIC X(45).
           03  WD-X                    PIC X.
           03  WT-HART.
               05 WT-FILNUM            PIC 9(6)       COMP  OCCURS 30.
               05 WT-FILBEZ            PIC X(12)            OCCURS 30.
               05 WT-ARBEZ             PIC X(10)            OCCURS 40.
               05 WT-ARNUM             PIC 9(4)       COMP  OCCURS 40.
               05 WT-ARNK              PIC 99         COMP  OCCURS 40.
               05 WT-HMG               PIC S9(6)V99   COMP  OCCURS 40.
           03  WM-REST                 PIC 99         COMP.
           03  WM-STG                  PIC 99         COMP.
           03  WD-ET                   PIC 999.
      *-------------------------------------------> Byte-Stream-DATEI <-
           03  WB-SATZ                 PIC X(160).
           03  WB-AC                   PIC X     COMP-X.
           03  WB-DE                   PIC X     COMP-X.
           03  WB-DV                   PIC X     COMP-X.
           03  WB-FH                   PIC X(4).
           03  WB-OFS                  PIC X(8)  COMP-x.
           03  WB-CNT                  PIC X(4)  COMP-x.
           03  WB-FLG                  PIC X     COMP-x.
      *--------------------------------------------> check_file_exist <-
           03  WH-FNAME                PIC X(20).
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)   COMP-X.
               05 WF-TT                PIC 99     COMP-X.
               05 WF-MO                PIC 99     COMP-X.
               05 WF-YY                PIC 9999   COMP-X.
               05 WF-HH                PIC 99     COMP-X.
               05 WF-MM                PIC 99     COMP-X.
               05 WF-SS                PIC 99     COMP-X.
               05 WF-HS                PIC 99     COMP-X.
           03  WV-SIZE                PIC X(8)   COMP-X.
           03  WZ-BREIT                PIC 999        COMP.
           03  WZ-GROSS                PIC 9999.
       01  WD-ETK.
      *---------------------------------> Etiketten-Druckersteuerung <-
           03  WH-ESC.
               05  FILLER              PIC X             VALUE "".
               05  WH-ST1              PIC 9999   COMP-X VALUE  0.
               05  WR-ST REDEFINES WH-ST1.
                   07  WH-ST2          PIC X.
                   07  WK-CR           PIC X.
           03  WK-LNG.
               05  FILLER              PIC X.
               05  WH-LNG1             PIC 99   COMP-X.
           03  WR-LEN REDEFINES WK-LNG.
               05  WH-LNG2             PIC 99   COMP-X.
               05  FILLER              PIC X.
           03  WR-LNG REDEFINES WK-LNG.
               05  FILLER              PIC X.
               05  WH-LNG              PIC XX.
           03  WB-BC.
               05  WC-LSL              PIC XXX   OCCURS 3.
               05  WC-CR               PIC X.
               05  WC-BIT11            PIC XXX.
               05  WC-BIT12            PIC XX.
               05  WC-STR.
                   07  WT-ST-X         PIC X OCCURS 120 INDEXED X2.
           03  WS-MUSTER.
               05  WT-MU               PIC X(108)        OCCURS 2.
           03  WH-STRI REDEFINES
               WS-MUSTER               PIC XX OCCURS 108 INDEXED X1.
      *-----------------------------------------> Etikettenparameter <-
           03  ET-HOE                  PIC 9       COMP.
           03  ET-LIN                  PIC 999     COMP.
       01  WS-STR.
      *-----------------------------------------------> Barcode Werte <-
           03  WH-BL           VALUE "0110010010011011110101000110110001
      -         "0101111011101101101110001011000110101100110110110010000
      -         "101011100100111010100001000100100100011101001110010".
               05  WH-LIWERT           OCCURS 10.
                   07  WH-LI           PIC 9 OCCURS 7 INDEXED X3.
               05  WH-REWERT           OCCURS 10.
                   07  WH-RE           PIC 9 OCCURS 7.
           03  WH-ANFANG               VALUE "001010".
               05  WH-ANF              PIC 9 OCCURS 6.
           03  WH-ENDE                 VALUE "0101".
               05  WH-END              PIC 9 OCCURS 4.
           03  WH-MITTE                VALUE "01010".
               05  WH-MIT              PIC 9 OCCURS 5.
           03  WX-FF              VALUE "??".
               05  WR-FF               PIC X.
               05  FILLER              PIC X.
           03  WS-LR                   PIC 99     VALUE ZERO COMP-3.
           03  WM-EAN                  PIC 9(8)   VALUE ZERO.
           03  WT-EAN REDEFINES WM-EAN.
               05  WT-7                PIC 9      OCCURS 8 INDEXED IX.
       01  WH-DESAZ.
      *-------------------------------------------- Byte-Stream-Files <-
           03  WH-TIME.
               05  WH-HHMM             PIC 9(4)      VALUE 0.
               05  WH-SSHH             PIC 9(4)      VALUE 0.
           03  WV-REART                PIC 99.
           03  WN-EDICUS               PIC X(24).
           03  WN-SICHA                PIC X(24).
           03  WN-SICHB                PIC X(24).
           03  WM-CD                   PIC X(35).
           03  WH-CST                  PIC 99999.     *> CST+ ZÑhler
           03  WH-UNH                  PIC 9999.      *> UNH+ ZÑhler
           03  WH-SEG                  PIC 9999.      *> Pos. im LIN+
           03  WH-LIN                  PIC 9999.      *> LIN+ ZÑhler
           03  WH-KONTROL              PIC X(15).
           03  WK-UNZ                  PIC X(15).
           03  WH-KAT                  PIC 99        COMP.
           03  WM-UID                  PIC X(15).
           03  WE-ANZ                  PIC Z(6)9.
           03  WE-BET                  PIC Z(6)9,99.
           03  WS-MOA                  PIC 9(7).
           03  WA-TAGE                 PIC 9(9)      COMP.
           03  WM-REDAS                PIC 9.
           03  WN-SEDAS                PIC X(20).
           03  WE-OPEN                 PIC 99        COMP.
           03  WC-POS                  PIC 9(4)      COMP.
           03  WH-LKNUM                PIC 9(7).
           03  WK-EANNR                PIC 9(13).
           03  WK-GLN                  PIC 9(13).
           03  WV-GLN                  PIC 9(13).
           03  WV-ILN                  PIC 9(13).
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING "PANEFAKT.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING "PANELFS.DAT "  WH-CREG.
       Z.  EXIT.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING  "PANARTIK.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "PANDEBIT.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           DISPLAY "PANBS" UPON ENV-SET.
           ACCEPT WE-EXTV FROM ENV-WERT.
           IF WE-EXTV = SPACE MOVE 0 TO WR-VAL.
           MOVE WR-VAL TO WM-PANBS.
           MOVE WH-NUM TO WK-EAN.
           MOVE 2 TO WV-KTONR.
           MOVE 0 TO WM-LFS.
           IF WL-CA = 95 PERFORM HOLDATEN GO X.            *> Testdruck
           IF WL-CA = 60 or 70 PERFORM HOLDATEN GO X.
           IF WL-CA = 50 PERFORM TAGTOUR GO X.
           IF WL-CA = 01 PERFORM LFDRU
                         IF WH-PG not = 7 PERFORM FIL-LISTE
                                          PERFORM DESAZEIL
                         end-if
                         ADD 90000 WM-LFS GIVING WH-BUKEY
                         GO X.
           IF WL-CA = 10 MOVE DE-KTONR TO LF-KTONR WV-KTONR
                MOVE LF-NUM TO WV-LFNUM
                MOVE DE-SUB TO LF-SUB
                MOVE DE-TOUR TO LF-TOUR
                MOVE WV-DATUM TO LF-DATUM
                MOVE 0 TO LF-ARNUM LF-SGRP WY
                MOVE SPACE TO  LF-ARBEZ
                PERFORM LFDRU GO X.
           GO Z.
       W.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
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
              PERFORM WEITER GO A.
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
           IF WM-DRU = 0 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           IF WH-PG = 3 AND WM-KO = 3 AND WH-PARM NOT = SPACE
               DISPLAY " " AT 1820
               DISPLAY "ENDCAP" UPON COMMAND-LINE
               PERFORM COMM
               CALL "CAUP" USING "1318012480" WH-CREG.
           IF WH-PG = 6 and WH-DRUNAM(1:3) not = "LPT" CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               IF WH-PG = 3 AND WM-KO = 3 AND WH-PARM NOT = SPACE
                   DISPLAY WH-PARM UPON COMMAND-LINE
                   PERFORM COMM
               end-if
               IF WH-DRUNAM(1:3) = "LPT" OPEN OUTPUT DRUCKER
                                    else OPEN EXTEND DRUCKER
               end-if
               PERFORM DRU-OK.
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
           IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
           IF WM-DRU = 1 GO G.
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
       G.  MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ***************************** KO-PARM = SPACE - kein Netzbetrieb *
       OBNETZ SECTION.
       A.  MOVE 35 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE SPACE TO WH-PARM.
           MOVE SPACE TO WH-PARM.
           EVALUATE KO-PARM
               WHEN 2 MOVE "CAPTURE P=0 Q=PRINTQ_1 NB TI=0 L=1 NFF"
                         TO WH-PARM
               WHEN 1 MOVE "CAPTURE P=0 Q=PRINTQ_0 NB TI=0 L=1 NFF"
                         TO WH-PARM
               WHEN OTHER MOVE "Druck auf lokalem Drucker" TO WH-PARM.
       Z.  EXIT.
      ******************************************* MS-DOS/NETZ-Commands *
       COMM SECTION.
       A.  CALL X"91" USING RESULT FUNKT PARAM.
       Z.  EXIT.
      ****************************************** Text 'Unterbrechnung' *
       UNTER SECTION.
       A.  DISPLAY "Unterbrechung mîglich" AT 2301.
       Z.  EXIT.
      ******************************************************************
       KB-STAT SECTION.
       A.  SET RET TO TRUE.
           CALL "CBL_GET_KBD_STATUS" using WH-KB.
           IF WH-KB = 0 GO Z.
       C.  DISPLAY "Programm unterbrochen!" WITH HIGHLIGHT AT 2401
              "<esc>= Abbruch, <ret>= Fortsetzung " AT 2424.
           CALL "CAUP" USING "0024591000" WH-CREG.
           PERFORM UNTER.
           IF NOT RET AND NOT ESC GO C.
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
      *********************************** Addition in Komm-Schein KERN *
       ADD-KOMM SECTION.
       A.  OPEN I-O KOMMSCH.
           MOVE LF-NUM TO WV-LFNUM WD-LF.
           IF WD-LF = 0 MOVE 1 TO WI                *> ERSTE Lieferung
                   else MOVE 2 TO WI.               *> ZWEITE Lieferung
       E.  MOVE LF-ARNUM TO KS-ARNUM.
           READ KOMMSCH INVALID
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 13
                   COMPUTE KS-KTONR(WY)= WF-FNR(WY) * 10 + 2
                   MOVE 0 TO KS-MENGE(WY) KS-MENGEB(WY).
           MOVE LF-SGRP TO KS-SGRP.
           MOVE LF-ARBEZ TO KS-ARBEZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 13
               IF LF-KTONR = KS-KTONR(WX);
                   IF WI = 1 ADD LF-MENGE(1) TO KS-MENGE(WX)
                        else ADD LF-MENGE(1) TO KS-MENGEB(WX).
           REWRITE KS-SATZ INVALID WRITE KS-SATZ.
           READ LFSCHEIN NEXT AT END GO X.
           IF LF-NUM = WV-LFNUM and LF-KTONR = DE-KTONR GO E.
       X.  CLOSE KOMMSCH.
       Z.  EXIT.
      *********************************** Kunden bei Kom.-Schein lesen *
       LIES-KUND SECTION.
       A.  IF LF-KTONR = DE-KTONR GO Z.
           MOVE LF-KTONR TO DE-KTONR.
           READ DEBITOR IGNORE LOCK INVALID MOVE 0 TO DE-RAST.
       Z.  EXIT.
      ********************************** autom. Lieferscheinschreibung *
       LFDRU SECTION.
       A.  MOVE 60 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 13
               MOVE KF-FILTAB(WX) TO WF-FILTAB(WX).
           COMPUTE WH-EAN = KO-EANNR * 1000000.
           PERFORM PRFZ.
           MOVE WH-EAN TO WK-EANNR.
           MOVE 0 TO WM-REDAS WH-LKNUM.
           DELETE FILE KOMMSCH.
           MOVE LOW-VALUES TO WDR-TST.
           MOVE 0 TO WDR-ANZ.
           PERFORM OBNETZ.
           IF WH-PG = 6 or WH-PG = 3 or WH-PG = 1;
               IF WE-WO(2) = 0 MOVE "LIEFDRU.LST" TO WH-DRUNAM.
           MOVE SPACE TO WH-LFSATZ WK-LFSATZ.
           IF WH-PG = 7 MOVE WE-DRU(5) TO WM-DRU
               MOVE "KOMSCH.LST" TO WH-DRUNAM
               IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM UNTER.
           MOVE 0 TO WM-KTONR WZ-ZEILEN WK WM-KOND.
           MOVE 1 TO WI.
           IF WM-KO = 3 MOVE 2 TO WI.
           IF WM-PANBS not = 0 MOVE WM-PANBS TO WI.
           IF WM-KO = 2 AND WS-TASTE = 9 MOVE 99 TO WS-TASTE
               MOVE 1 TO WM-KO.
           MOVE 61 TO VDU-AB.
           IF WH-PG NOT = 7; IF WM-KO = 2 MOVE 40 TO VDU-AB
               IF WY = 10 MOVE LF-NUM TO WV-LFNUM.
           IF WH-PG = 3 START LFSCHEIN KEY > LF-KEY INVALID GO Z
                        END-START GO C.
           START LFSCHEIN KEY > LF-AKEY INVALID PERFORM NO-REC GO Z.
       B.  START LFSCHEIN KEY > LF-AKEY INVALID GO X.
       C.  READ LFSCHEIN NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
       D.  IF FINE GO X.
040812     PERFORM LIES-KUND.                       *> wg. KERN Filllist
           IF WE-LFS = 1 and DE-RAST = 5 and WH-PG = 6
               PERFORM ADD-KOMM
               GO D.
           MOVE LF-GRM TO WF-GRM.
      *---------------------------> wieder lîschen wg. Lf-Sortg. raus <-
310712*    IF WE-LFS = 1 and lf-arnum = 81 move 3 to wf-tb.
      *-------------------------------------------------> bis hierher <-
           IF WH-PG = 7 and WM-VER = 3;
               IF WF-TB < 3 GO C.                       *> Konditorei
           IF WF-TB > 3 MOVE 3 TO WF-TB.                *> BÑckereiware
           IF WH-PG = 7 PERFORM LIES-KUND
               IF DE-RAST = 5 GO C.
           IF WH-PG = 6; IF WS-TASTE = 99; IF LF-EAN NOT = 4
               AND LF-EAN NOT = 2 GO C.
           IF WH-PG = 6; IF WM-KO = 2; IF LF-EAN = 4 OR LF-EAN = 2 GO C.
           IF WY = 10 AND LF-NUM NOT = WV-LFNUM GO C.
           DIVIDE 100 INTO LF-NUM GIVING WK-ANZ REMAINDER WK-ANZ.
           IF WM-VER = 2 AND WM-KO = 1 GO E.
           IF WH-PG NOT = 7; IF WK-ANZ > 90
                MOVE 9999 TO LF-ARNUM GO B ELSE GO E.
      *--------------------------------> nur bei Kommissionsscheinen <-
           IF WM-KO = 2; IF WF-TB = 3 GO E
                         else MOVE 0 TO WM-KTONR WV-LFNUM GO C.
           IF WM-KO = 3; IF WI = WF-TB GO E ELSE GO C.
       E.  IF WM-PANBS not = 0;                 *> "B" und "K" Trennung
               EVALUATE WM-PANBS
                   WHEN 2 IF WF-TB = 3 GO C
                   WHEN 3 IF WF-TB not = 3 GO C.
           IF WV-KTONR > 2; IF WV-KTONR NOT = LF-KTONR
               OR LF-NUM NOT = WV-LFNUM
               OR LF-DATUM NOT = WV-DATUM GO X.
           IF LF-DATUM > WV-DATUM GO X.
           IF WV-LFNUM = 0 MOVE LF-NUM TO WV-LFNUM.
           IF WM-KTONR = 0 MOVE LF-KTONR TO WM-KTONR.
           IF LF-KTONR = WM-KTONR AND LF-NUM = WV-LFNUM GO F.
      *----------------------------------> Ende eines Lieferscheines <-
           IF WH-LFSATZ not = SPACE MOVE LF-SATZ TO WK-LFSATZ
               MOVE WH-LFSATZ TO LF-SATZ
               MOVE SPACE TO WH-LFSATZ
               GO G.                                    *> nur bei Kern
       EZ. IF WH-PG NOT = 7;
              IF WZ-ZEILEN > 0;
                 IF DRA-SATZ not = SPACE
      *              MOVE "¿ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒŸ" TO DRA-TX(58:)
                     MOVE x"1B32" TO DRA-SATZ(95:)
                     PERFORM DRUCK
                 end-if
                 PERFORM END-DRU
                 MOVE 0 TO WZ-ZEILEN WM-KOND
              else MOVE 0 TO WZ-ZEILEN
           else IF WM-VER = 3;
                   IF WZ-ZEILEN < 31 COMPUTE WZ-SCHALT = 32 - WZ-ZEILEN
                                else COMPUTE WZ-SCHALT = 60 - WZ-ZEILEN
                end-if
                PERFORM DRUCK
                IF WZ-ZEILEN > 56 WRITE DRA-SATZ AFTER PAGE
                   MOVE 0 TO WZ-ZEILEN.
      *    IF WM-REDAS > 1 PERFORM DESA-UNT.
           PERFORM DRU-STAT.
           MOVE 0 TO WK.
           MOVE LF-KTONR TO WM-KTONR.
           IF WY NOT = 10 MOVE LF-NUM TO WV-LFNUM.
       F.  IF WE-LFS = 1 and WH-PG = 7 and WK = 0 PERFORM KOMSCHEIN;
               IF WM-KOND = 1 MOVE 0 TO WM-KOND
                              GO D
                         else MOVE 0 TO WM-KOND.
310712*-------------------------> wg. Lieferscheinsortg. rausgenommen <-
      *    IF WE-LFS = 1 and WH-PG = 6 and LF-ARNUM = 81
      *        MOVE LF-SATZ TO WH-LFSATZ
      *        GO C.
       G.  IF WK = 0 PERFORM LFKOPF
                     PERFORM LFTEXT
      *              IF ESC SET RET TO TRUE GO C
      *              end-if
                     MOVE 1 TO WK
           else IF WZ-ZEILEN > VDU-AB;
                   IF DRA-SATZ not = SPACE
      *               MOVE "¿ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒŸ" TO DRA-TX(58:)
                      MOVE x"1B32" TO DRA-SATZ(95:)
                      PERFORM DRUCK
                      end-if
                   MOVE "öbertrag" TO DRA-SATZ(46:)
                   PERFORM DRUCK
                   WRITE DRA-SATZ AFTER PAGE
                   MOVE 0 TO WZ-ZEILEN
                   PERFORM LFKOPF
                   IF WE-LFS not = 1 MOVE "öbertrag" TO DRA-SATZ(46:)
                       PERFORM DRUCK.
           IF DRA-SATZ not = SPACE PERFORM DRUCK.
           IF ESC GO Z.
           MOVE LF-ARNUM TO WV-ARNUM.
           ADD 1 TO WDR-ANZ.
           IF WM-KO = 3 AND DE-RAST = 3 MOVE 9999 TO LF-ARNUM
               MOVE 0 TO WM-KTONR WV-LFNUM WK GO B.
           IF WH-PG = 7 MOVE LF-ARNUM TO DRA-ARNUM
               else IF WM-PR = 1 MOVE LF-PREIS TO DRA-BETRAG
                    END-IF PERFORM EAN.
           IF LF-SGRP not = 0 MOVE LF-SGRP TO WD-GRP
               MOVE WD-GRP TO DRA-ARNUM(1:2).
           MOVE LF-BEZ TO DRA-BEZ.
           IF LF-STAT = 98 MOVE "*" TO DRA-SATZ(47:1).
           MOVE LF-MENGE(1) TO WH-ANZ.
           MOVE LF-GRM TO WF-GRM.
           PERFORM MG.
           MOVE WD-MENGE TO DRA-MENGE.
           ADD 1 WF-MEH GIVING WH-MEH.
           MOVE WT-MEH(WH-MEH) TO DRA-MEH.
310712*-------------------------> wg. Lieferscheinsortg. rausgenommen <-
      *    IF WE-LFS = 1 and WH-PG = 6 and LF-ARNUM = 81
      *        IF WM-KOND = 0 MOVE "*B*" TO DRA-ARNUM(5:3).
           IF WM-PANBS > 0;
              EVALUATE LF-STAT
                  WHEN 97
                  WHEN 96 MOVE "Wird nachgeliefert!" TO DRA-TX(59:)
                  WHEN 95
                  WHEN 94 MOVE "Nicht lieferbar!" TO DRA-TX(43:).
           IF WM-KO = 1 or WM-KO = 2 or WM-VER = 3 or WE-TYP(2) = 1
               IF WS-TASTE = 99 MOVE 2 TO WZ-SCHALT
               else MOVE "__________   __________" TO DRA-SATZ(66:).
           IF WE-LFS = 1 MOVE SPACE TO DRA-SATZ(66:)
               MOVE DRA-SATZ(4:) TO DRA-SATZ(1:).
      *        IF WE-LFS = 1 and WH-PG = 6
      *            MOVE "≥        ≥        ≥        ≥" TO DRA-TX(58:).
           MOVE DRA-SATZ TO WT-TX WT-ADR.
           DISPLAY WT-TX(8:55) with SIZE 80 AT 2301.
           MOVE DRA-SATZ TO WT-ADR.
           IF DE-EAN = 2 AND WH-PG NOT = 7
               MOVE 0 TO WL
               MOVE 3 TO ET-HOE
               MOVE 9 TO ET-LIN
               IF FA-KUARNUM NOT = 0 PERFORM EANDRU GO C.
      *----------------------------------------> Trennung Lfsch. Kern <-
            IF WE-LFS = 1 and (WH-PG = 6 or 1 or 3)
               MOVE x"1B3316"TO DRA-SATZ
               MOVE WT-ADR TO DRA-SATZ(7:)
               MOVE 71 TO VDU-AB
               PERFORM DRUCK
               PERFORM DESADATEN              *> Desadv-Daten erstellen
      *        MOVE "√ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ¥" TO DRA-TX(58:)
               MOVE x"1B32" TO DRA-SATZ(95:)
               IF WF-TB = 3 ADD 1 TO WM-KOND
               end-if
               IF WK-LFSATZ not = SPACE MOVE 0 TO WM-KOND
                  MOVE WK-LFSATZ TO LF-SATZ
                  MOVE SPACE TO WK-LFSATZ
                  GO EZ
               end-if
               GO C.
           IF WM-KO = 1 or WM-KO = 2;
               MOVE x"1B30" TO DRA-SATZ(1:)
               MOVE WT-ADR TO DRA-SATZ(4:)
               PERFORM DRUCK
               MOVE x"1B32" TO DRA-SATZ
               MOVE 69 TO VDU-AB.
           PERFORM DRUCK.
           IF WM-KO = 3 MOVE 2 TO WZ-SCHALT.
           GO C.
      *--------------------------> Druckausgabe kleiner Lieferschein <-
       K.  MOVE SPACE TO DRA-SATZ.
           MOVE LF-ARNUM TO WD-POS.
           MOVE WD-POS TO DRA-SATZ(9:4).
           MOVE LF-BEZ TO DRA-SATZ(13:26).
           MOVE WD-MENGE TO DRA-SATZ(36:10).
           PERFORM DRUCK.
           GO C.
       X.  IF DRA-SATZ not = SPACE
               MOVE 1 TO WZ-SCHALT
      *        MOVE "¿ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒŸ" TO DRA-TX(58:)
               MOVE x"1B32" TO DRA-SATZ(95:)
               PERFORM DRUCK.
           IF WS-TASTE = 99 MOVE 2 TO WM-KO
               MOVE 9 TO WS-TASTE.
           IF WZ-ZEILEN > 0 PERFORM END-DRU.
           IF WM-REDAS = 1 PERFORM EDIFUS
                           PERFORM STAT-ANZ.
           UNLOCK LFSCHEIN.
           IF WH-PG NOT = 7 GO Z.
           IF WM-KO = 3 AND WI = 2 GO Z.
           IF WM-KO = 2 OR WM-KO = 1 GO Z.
           ADD 1 TO WI.
           IF WM-KO = 3; IF WI = 4 GO Z.
           IF WM-KO < 2; IF WI = 3 GO Z.
           MOVE 0 TO WM-KTONR WZ-ZEILEN LF-ARNUM LF-KTONR
               LF-NUM LF-TOUR LF-SUB WK.
           MOVE WV-DATUM TO LF-DATUM.
           GO B.
       Z.  EXIT.
      ************************************ Lieferscheintext ausdrucken *
       LFTEXT SECTION.
       A.  OPEN INPUT TX-LFTEXT.
           INITIALIZE TX-SATZ.
           MOVE LF-KTONR TO TX-KTONR.
           MOVE LF-DATUM TO TX-LFDAT.
           MOVE LF-NUM TO TX-LFNUM.
           READ TX-LFTEXT INVALID INITIALIZE TX-SATZ.
           CLOSE TX-LFTEXT.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 2
              IF TX-TEXT(WZ) not = SPACE
                  MOVE TX-TEXT(WZ) TO DRA-SATZ(10:)
                  PERFORM DRUCK.
       Z.  EXIT.
      ********************** OPEN DESADV * konvertierte EDIFACT-DAtei <-
       OPEN-DESADV SECTION.
       A.  MOVE "KEDESADV.HOG" TO WN-SEDAS.
           MOVE HD-EXT TO WN-SEDAS(10:3) WE-EXT.
           MOVE 0 TO WC-POS.
           MOVE 0 TO WH-UNH.
           MOVE WN-SEDAS TO WN-EDICUS.
      *-------------------------------------> lîschen und create file <-
           CALL "CBL_DELETE_FILE" USING WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS WH-SEG.
           MOVE 1 TO WE-OPEN.                         *> Datei geîffnet
           MOVE "UNA:+,? '" TO WB-SATZ.
           MOVE 9 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE "UNB+UNOA:3+" TO WB-SATZ.
           MOVE WK-EANNR TO WM-CD.
           PERFORM ANHANG.
           MOVE ":14+" TO WM-CD.                           *> Versender
           PERFORM ANHANG.
           MOVE HD-GLN TO WM-CD.                           *> EmpfÑnger
           PERFORM ANHANG.
           MOVE ":14+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WM-DATUM TO WC-DATUM.
           MOVE WC-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           ACCEPT WH-TIME FROM TIME.
           MOVE WH-HHMM TO WM-CD(1:4).
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE "KELFS" TO WH-KONTROL
           MOVE WV-DATUM TO WH-KONTROL(6:).
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG
           MOVE "++DESADV++++0" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
       Z.  EXIT.
      ******************************************* DESADV-Rechnungskopf *
       DESAKOPF SECTION.
       A.  MOVE "UNH+" TO WB-SATZ.
           MOVE WV-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE "+DESADV:D:96A:UN:EAN005" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "BGM+351+" TO WB-SATZ.
           MOVE HD-LFNUM TO WH-LKNUM.
           MOVE WH-LKNUM TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "DTM+137:" TO WB-SATZ.                 *> Lfs.-Datum
           MOVE WV-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":102" TO WM-CD.                       *> fÅr JJJJMMTT
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "DTM+17:" TO WB-SATZ.                  *> Lieferdatum
           MOVE WV-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":102" TO WM-CD.                       *> fÅr JJJJMMTT
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+BY+" TO WB-SATZ.
           MOVE HD-GLN TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+SU+" TO WB-SATZ.
           MOVE WK-EANNR TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+DP+" TO WB-SATZ.
           MOVE HD-ILN TO WM-CD.
      *    MOVE WM-CD(2:) TO WM-CD(1:).
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "CPS+1" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE 0 TO WH-LIN.
       Z.  EXIT.
      ************************************* Daten fÅr DESADV erstellen *
       DESADATEN SECTION.
       A.  IF DE-GLN = 0 GO Z.
           OPEN I-O HILFDESA.
           INITIALIZE HD-SATZ.
           MOVE WE-EXT TO HD-EXT.
           MOVE WK-GLN TO HD-GLN.
           MOVE DE-GLN TO HD-ILN.
           MOVE LF-ARNUM TO HD-ARNUM.
           MOVE LF-BEZ TO HD-BEZ.
           MOVE LF-KTONR TO HD-KTONR.
           MOVE LF-DATUM TO HD-DATUM.
           MOVE LF-NUM TO HD-NUM HD-LFNUM.
           COMPUTE WH-NUM = LF-KTONR / 10.
           COMPUTE HD-LFNUM = WH-NUM * 10000 + LF-NUM.
           COMPUTE HD-ANZ = LF-MENGE(1) + LF-MENGE (2) + LF-MENGE(3).
           EVALUATE LF-MEH
               WHEN 0 MOVE "STK" TO HD-MEH
               WHEN 1 MOVE "KGM" TO HD-MEH
               WHEN OTHER MOVE "STK" TO HD-MEH.
           WRITE HD-SATZ INVALID REWRITE HD-SATZ.
           CLOSE HILFDESA.
       Z.  EXIT.
      ******************************************* DESADV-Lieferrzeilen *
       DESAZEIL SECTION.
       A.  OPEN I-O HILFDESA.
           IF WF-STATUS not = "00" GO W.
           MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           COMPUTE WH-EAN = KO-EANNR * 1000000.
           PERFORM PRFZ.
           MOVE WH-EAN TO WK-EANNR.
           MOVE 0 TO WM-REDAS WV-ILN WV-GLN.
           INITIALIZE HD-SATZ.
           MOVE WV-DATUM TO HD-DATUM.
           START HILFDESA KEY > HD-KEY INVALID GO W.
       E.  READ HILFDESA NEXT AT END GO Q.
           IF WV-DATUM not = HD-DATUM GO E.
           IF HD-GLN not = WV-GLN and WM-REDAS > 1 PERFORM DESA-UNT
                                                   PERFORM EDIFUS.
           IF HD-ILN not = WV-ILN and WM-REDAS > 1 PERFORM DESA-UNT.
           IF HD-ILN = WV-ILN and HD-LFNUM not = WH-LKNUM
                 PERFORM DESA-UNT.
           IF WM-REDAS = 0 PERFORM OPEN-DESADV
               MOVE HD-GLN TO WV-GLN
               MOVE 1 TO WM-REDAS.
           IF WM-REDAS not = 2 PERFORM DESAKOPF
               MOVE HD-ILN TO WV-ILN
               MOVE 2 TO WM-REDAS.
           ADD 1 TO WC-POS.
      *------------------------> Ausgabe der 'fertigen' EDIFACT-Datei <-
           ADD 1 TO WH-LIN.
           MOVE "LIN+" TO WB-SATZ.
           MOVE WH-LIN TO WE-ANZ.                     *> Anz. LIN+ Pos.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "++" TO WM-CD.
           PERFORM ANHANG.
           MOVE HD-ARNUM TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE ":SA" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
      *--------------------------------------------------> Bezeichung <-
           MOVE "IMD+F++:::" TO WB-SATZ.
           MOVE HD-BEZ TO WM-CD.
           PERFORM UMLAUTE.
           PERFORM ZEILEND.
      *--------------------------------> gelieferte/verrechenb. Menge <-
           MOVE "QTY+12:" TO WB-SATZ.
           MOVE HD-ANZ TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
                IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
      *------------------------------------> ob nur STK oder auch KGM <-
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE HD-MEH TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           GO E.
       Q.  IF WM-REDAS > 1 PERFORM DESA-UNT
                           PERFORM EDIFUS.
           GO X.
       W.  DISPLAY "keine EDI-Daten vorhanden" AT 2401.
           PERFORM WEITER
           GO X.
       X.  CLOSE HILFDESA.
           DELETE FILE HILFDESA.
       Z.  EXIT.
      ******************************************* DESADV-Lieferrzeilen *
       DESAOLD SECTION.
       A.  IF DE-GLN = 0 GO Z.
           IF WM-REDAS = 0 PERFORM OPEN-DESADV
               MOVE 1 TO WM-REDAS.
           IF WM-REDAS not = 2 PERFORM DESAKOPF
               MOVE 2 TO WM-REDAS.
           ADD 1 TO WC-POS.
      *------------------------> Ausgabe der 'fertigen' EDIFACT-Datei <-
           ADD 1 TO WH-LIN.
           MOVE "LIN+" TO WB-SATZ.
           MOVE WH-LIN TO WE-ANZ.                     *> Anz. LIN+ Pos.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "++" TO WM-CD.
           PERFORM ANHANG.
           MOVE LF-ARNUM TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE ":SA" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
      *--------------------------------------------------> Bezeichung <-
           MOVE "IMD+F++:::" TO WB-SATZ.
           MOVE LF-BEZ TO WM-CD.
           PERFORM UMLAUTE.
           PERFORM ZEILEND.
      *--------------------------------> gelieferte/verrechenb. Menge <-
           MOVE "QTY+12:" TO WB-SATZ.
           COMPUTE WE-ANZ = LF-MENGE(1) + LF-MENGE (2) + LF-MENGE(3).
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
                IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
      *------------------------------------> ob nur STK oder auch KGM <-
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           EVALUATE LF-MEH
               WHEN 0 MOVE "STK" TO WM-CD
               WHEN 1 MOVE "KGM" TO WM-CD
               WHEN OTHER MOVE "STK" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
       Z.  EXIT.
      ********************************************** PrÅfziffer rechen *
       PRFZ SECTION.
       A.  COMPUTE WR-EAN(13) = (WR-EAN(2) + WR-EAN(4) + WR-EAN(6)
                      + WR-EAN(8) + WR-EAN(10) + WR-EAN(12)) * 3
                      + (WR-EAN(1) + WR-EAN(3) + WR-EAN(5)
                      + WR-EAN(7) + WR-EAN(9) + WR-EAN(11)).
           COMPUTE WR-EAN(13) = 10 - WR-EAN(13).
       Z.  EXIT.
      ******************************************************************
       DESA-UNT SECTION.
       A.  MOVE "UNT+" TO WB-SATZ.
           MOVE WH-SEG TO WE-ANZ.            *> Anz. Pos. im UNB+ Segm.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-LKNUM TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE 1 TO WH-SEG.
           ADD 1 TO WH-UNH.
           MOVE 1 TO WM-REDAS.
       Z.  EXIT.
      ******************************************************************
       EDIFUS SECTION.
       A.  IF WH-UNH = 0 GO Z.
           MOVE "UNZ+" TO WB-SATZ.
           MOVE WH-UNH TO WE-ANZ.                 *> Anz. UNH+ Segmente
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WK-UNZ TO WM-CD.                  *> Kennung aus UNB+
           PERFORM ANHANG.
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
           MOVE "DESADV\KEDESADV.HOG " TO WN-SICHA.
           MOVE WN-EDICUS TO WN-SICHA(8:12).
           MOVE "DESADV.xxx" TO WN-SICHB.                *> fÅr Versand
           INSPECT WN-SICHB REPLACING ALL "xxx" BY WE-EXT.
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE WZ-DATUM(1:4) TO WN-SICHA(12:4).         *> Sich.-Stand
           CALL "CBL_COPY_FILE" USING WN-EDICUS WN-SICHA.
           CALL "CBL_COPY_FILE" USING WN-EDICUS WN-SICHB.
           CALL "CBL_DELETE_FILE" USING WN-EDICUS.
           EVALUATE WE-EXT
               WHEN "HOG" PERFORM DES-BLAT
               WHEN "DUS" PERFORM MACH-FTP.
           MOVE 0 TO WM-REDAS.
       Z.  EXIT.
      ************************************* BAT-Datei fÅr mailsend.bat *
       DES-BLAT SECTION.
       A.  MOVE "BLAT\MAILSEND.BAT" TO WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           MOVE "blat -t edifact@hogast.at -c office@baeckerei-kern.at -
      -       "s DESADV" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "-body ""DESADV fÅr Lieferung 12.12.12""" TO WB-SATZ.
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM TO WB-SATZ(29:8).
           PERFORM COUNT-WRITE.
           MOVE "-attach f:\bug\DESADV.EDI -server bsmtp.telekom.at -f o
      -        "ffice@baeckerei-kern.at -log mailog.dat" TO WB-SATZ.
           MOVE WN-SICHB TO WB-SATZ(16:10).
           PERFORM COUNT-WRITE.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
       Z.  EXIT.
      ******************************************************************
       MACH-FTP SECTION.
       A.  GO Z.
           MOVE "DUS.BAT" TO WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
      *---------------------------------------> Ftp-Dateien erstellen <-
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           MOVE "ftp -s:DUS.FTP" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "fc desadv.dus ftp\desadv.dus" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "if errorlevel = 1 goto FALSCH" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "@echo  öbertragung ok!" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "goto ENDE" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE ":FASCH" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "@echo  öbertragung DUSSMANN fehlerhaft!" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE ":ENDE" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
      *---------------------------------> erst BAT-File dann SCR-File <-
           MOVE "DUS.FTP" TO WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           MOVE "open ftp.necta.at" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "1050_15" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "s4PpLEDA" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "asc" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "cd /desadv" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "put DESADV.DUS." TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "lcd FTP" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "get DESADV.DUS" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "lcd .." TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           MOVE "quit" TO WB-SATZ.
           PERFORM COUNT-X-WRITE.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
      *-------------------------------------------> BAT-File erledigt <-
           CALL "CAUP" USING "0703011980000" WH-CREG.
           CALL "CAUP" USING "1301012580" WH-CREG.
      *-----------------------------------------------> FTP-ausfÅhren <-
           MOVE "DUS.BAT" TO WD-UPON.
           DISPLAY LOW-VALUE AT 1801.
           DISPLAY WD-UPON UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.
      *------------------------> öbertragung kontrollieren u. lîschen <-
           MOVE "DESADV.DUS" TO WH-FNAME WN-EDICUS.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           MOVE WF-SIZE TO WV-SIZE.
           MOVE "FTP\DESADV.DUS" TO WH-FNAME.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           IF WF-SIZE = WV-SIZE
      *-----------> WH-FNAME = Retourdatei WN-EDICAUS = *> Org.-Datei <-
               CALL "CBL_DELETE_FILE" USING WH-FNAME
               CALL "CBL_DELETE_FILE" USING WN-EDICUS
               GO X.
           DISPLAY "öbertragung an DUSSMANN fehlgeschlagen!" AT 2003
           DISPLAY "Bitte Datei 'DUSS.BAT' in F:\BUG durchfÅhren!"
                       AT 2103.
           PERFORM WEITER.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       COUNT-X-WRITE SECTION.
       A.  PERFORM VARYING WB-CNT FROM 99 BY -1 UNTIL
               WB-SATZ(WB-CNT:1) not = SPACE CONTINUE.
           ADD 1 TO WB-CNT.
           move x"0d0a" TO WB-SATZ(WB-CNT:2).
           add 1 to wb-cnt.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
       Z.  EXIT.
      ******************************************************************
       COUNT-WRITE SECTION.
       A.  PERFORM VARYING WB-CNT FROM 99 BY -1 UNTIL
               WB-SATZ(WB-CNT:1) not = SPACE CONTINUE.
           ADD 1 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
       Z.  EXIT.
      ***************************************************** Zeilenende *
       ZEILEND SECTION.
       A.  MOVE "'" TO WM-CD(1:).
      *    move x"0d0a" TO WM-CD(2:).              *> raus
           PERFORM ANHANG.
      *    add 2 to wx.                            *> raus
           MOVE WX TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE SPACE TO WB-SATZ.
           ADD 1 TO WH-SEG.
       Z.  EXIT.
      ******************************************************* anhÑngen *
       ANHANG SECTION.
       A.  PERFORM VARYING WX FROM 160 BY -1
               UNTIL WB-SATZ(WX:1) not = SPACE continue.
           ADD 1 TO WX.
           MOVE WM-CD TO WB-SATZ(WX:).
       Z.  EXIT.
      ******************************************************************
       UMLAUTE SECTION.
       A.  INSPECT WM-CD REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹".
           PERFORM ANHANG.
       Z.  EXIT.
      ********************************** autom. Lieferscheinschreibung *
       KOMSCHEIN SECTION.
       A.  IF LF-ARNUM not = 81 MOVE 2 TO WM-KOND GO Z.
           IF DE-RAST = 5 GO Z.
           MOVE LF-SATZ TO WK-LFSATZ.
           MOVE 1 TO WM-KOND.
       C.  READ LFSCHEIN NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE LF-GRM TO WF-GRM.
      *----------------------------------------------> wieder lîschen <-
      *    IF WE-LFS = 1 and lf-arnum = 81 move 3 to wf-tb.
      *-------------------------------------------------> bis hierher <-
           IF WV-KTONR > 2; IF WV-KTONR NOT = LF-KTONR
               OR LF-NUM NOT = WV-LFNUM
               OR LF-DATUM NOT = WV-DATUM GO X.
           IF LF-DATUM > WV-DATUM GO X.
           IF WV-LFNUM = 0 MOVE LF-NUM TO WV-LFNUM.
           IF WM-KTONR = 0 MOVE LF-KTONR TO WM-KTONR.
           IF LF-KTONR = WM-KTONR AND LF-NUM = WV-LFNUM GO F.
           GO X.
       F.  IF WF-TB < 3 GO C.                           *> Konditorei
           MOVE 2 TO WM-KOND.
           GO C.
      *----------------------------------> Ende eines Lieferscheines <-
       X.  IF WM-KOND = 1 MOVE 0 TO WV-LFNUM WM-KTONR
                          GO Z.
           MOVE WK-LFSATZ TO LF-SATZ.
           READ LFSCHEIN KEY IS LF-AKEY INVALID NEXT SENTENCE.
       Z.  EXIT.
      ***************************************************** EAN-Nummer *
       EAN SECTION.
       A.  IF DE-EAN = 0 OR DE-EAN = 4 MOVE WV-ARNUM TO DRA-ARNUM GO Z.
           IF DE-EAN = 2 OR DE-EAN = 3 GO D.
           COMPUTE WH-EAN = WK-EAN * 1000000 + (WV-ARNUM * 10).
       B.  ADD WR-EAN(2) WR-EAN(4) WR-EAN(6) WR-EAN(8) WR-EAN(10)
               WR-EAN(12) GIVING WK-ANZ.
           MULTIPLY 3 BY WK-ANZ.
           ADD WR-EAN(1) WR-EAN(3) WR-EAN(5) WR-EAN(7) WR-EAN(9)
               WR-EAN(11) TO WK-ANZ.
           DIVIDE 10 INTO WK-ANZ GIVING WK-ANZ REMAINDER WK-ANZ.
           SUBTRACT WK-ANZ FROM 10 GIVING WR-EAN(13).
           MOVE WH-EAN TO DRA-ARNUM.
           GO Z.
      *----------------------> lesen v. kundeneigenen Artikelnummern <-
       D.  IF WV-ARNUM = FA-ARNUM AND DE-KTONR = FA-KTONR GO F.
           MOVE WV-ARNUM TO FA-ARNUM.
           MOVE DE-KTONR TO FA-KTONR.
       E.  READ FAKTDAT INVALID KEY MOVE 0 TO FA-KUARNUM GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
       F.  IF DE-EAN = 3; IF FA-KUARNUM = 0
               MOVE WV-ARNUM TO DRA-ARNUM GO Z
           ELSE MOVE FA-KUARNUM TO DRA-ARNUM GO Z.
           MOVE FA-KUARNUM TO WH-EAN.
           PERFORM BAR.
       Z.  EXIT.
      ************************************** Lieferschein mit EAN-Code *
       EANDRU SECTION.
       A.  MOVE SPACE TO WB-BC.
           SUBTRACT WL FROM ET-LIN GIVING WZ-SCHALT.
           ADD WZ-SCHALT TO WL.
           MULTIPLY 3 BY WZ-SCHALT GIVING WH-ST1.
           MOVE 0 TO WZ-SCHALT.
           MOVE "3" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(1).
           IF ET-HOE = 1 ADD 09 TO WH-ST1
               MOVE WH-ESC TO WC-LSL(1)
               MOVE X"1B210100" TO DRA-SATZ(1:)
               PERFORM DRUCK
               MOVE 0 TO WH-ST1
               MOVE "l" TO WH-ST2
               WRITE DRA-SATZ FROM WH-ESC AFTER 0
               MOVE WT-ADR TO DRA-SATZ
               WRITE DRA-SATZ.
           WRITE DRA-SATZ FROM WB-BC BEFORE 1.
           MOVE 0 TO WH-ST1.
           MOVE "3" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(1).
           MOVE 1 TO WH-ST1.
           MOVE "*" TO WH-ST2.
           MOVE WH-ESC TO WC-BIT11.
           MOVE 0 TO WH-LNG1.
           MOVE 120 TO WH-LNG2.
           MOVE WK-LNG TO WC-BIT12.
           MOVE 0 TO WH-ST1.
           MOVE "!" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(2).
           MOVE 13 TO WH-ST1.
           MOVE WK-CR TO WC-CR.
           IF ET-HOE = 1 MOVE LOW-VALUE TO WS-MUSTER.
           MOVE 5 TO WH-ST1.
           MOVE WT-MU(1) TO WC-STR.
           PERFORM ANZAHL.
           MOVE 14 TO WH-ST1.
           MOVE WT-MU(2) TO WC-STR.
           PERFORM ANZAHL.
           IF ET-HOE < 2 PERFORM ZAHLEN.
           SUBTRACT 1 FROM ET-HOE.
           ADD 6 TO ET-LIN.
           IF ET-HOE > 0 GO A.
           MOVE 0 TO WH-ST1.
           MOVE "2" TO WH-ST2.
           WRITE DRA-SATZ FROM WH-ESC AFTER 0.
           ADD 3 TO WZ-ZEILEN.
           MOVE X"1B210100" TO DRA-SATZ(1:).
           WRITE DRA-SATZ AFTER 0.
           MOVE 1 TO WZ-SCHALT.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       ANZAHL SECTION.
       A.  MOVE "l" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(3).
           MOVE 0 TO WH-LNG1.
           MOVE 120 TO WH-LNG2.
           SET X2 TO 120.
       C.  IF WT-ST-X(X2) NOT = "?" AND
              WT-ST-X(X2) NOT = LOW-VALUE SUBTRACT 1 FROM WH-LNG2
              IF X2 > 1 SET X2 DOWN BY 1 GO C.
           MOVE WK-LNG TO WC-BIT12.
           WRITE DRA-SATZ FROM WB-BC AFTER 0.
       Z.  EXIT.
      ***********************************************************************
       ZAHLEN SECTION.
       A.  MOVE LOW-VALUE TO WC-STR.
           MOVE 7 TO WH-ST1.
           MOVE "l" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(3).
           SET X2 TO 1.
           SET EX TO 6.
       C.  MOVE WR-EAN(EX) TO WT-ST-X(X2).
           IF X2 < 8 SET EX X2 UP BY 1 GO C.
           MOVE 0 TO WH-LNG1.
           MOVE 0 TO WH-LNG2.
           MOVE WK-LNG TO WC-BIT12.
           WRITE DRA-SATZ FROM WB-BC AFTER 0.
           MOVE 0 TO WH-ST1.
           MOVE "l" TO WH-ST2.
           WRITE DRA-SATZ FROM WH-ESC AFTER 0.
       Z.  EXIT.
      ************************************ GLN der Rechnungsadr. lesen *
       RE-ADR SECTION.
       A.  IF DE-GLN = 0 GO Z.
           MOVE DE-REKTO TO DE-KTONR.
           READ DEBITOR IGNORE LOCK INVALID
               DISPLAY "Rechnungsadresse fehlt! " AT 2401
               PERFORM WEITER
               STOP RUN.
           MOVE DE-GLN TO WK-GLN.
           MOVE LF-KTONR TO DE-KTONR.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO WE-EXT.
           READ DEBITOR IGNORE LOCK INVALID MOVE "- fehlt -" TO DE-BEZ.
       Z.  EXIT.
      ******************************************************************
       LFKOPF SECTION.
       A.  PERFORM KB-STAT.
           IF ESC GO Z.
           MOVE 1 TO DE-FNR.
           MOVE LF-KTONR TO DE-KTONR.
       B.  READ DEBITOR IGNORE LOCK INVALID MOVE "- fehlt -" TO DE-BEZ.
           IF DE-REKTO not = DE-KTONR PERFORM RE-ADR.
           MOVE DE-BEZ TO WT-BEZ.
040812     IF WM-KO = 1 and DE-STATIS = 3 GO Z.
           IF WH-PG NOT = 3 DIVIDE 10 INTO DE-KTONR GIVING WD-KTO
               CALL "CAUP" USING "1303220380000" WH-CREG
               DISPLAY "Kd-Nr.: " AT 0322
               DISPLAY WD-KTO LINE 3 POSITION 0.
           IF WZ-ZEILEN = 0 MOVE X"0100" TO WH-PX(1)
               MOVE X"000C" TO WH-PX(2)
               PERFORM BEG-DRU.
      *-------------------------------------------> ob andere Version <-
           IF WH-PG NOT = 7; IF WM-KO = 2 GO K
                             else IF WE-TYP(2) = 1 GO N.
           IF WH-PG NOT = 7; IF WM-KO = 0 MOVE 8 TO WZ-SCHALT GO E
                        ELSE IF WM-KO = 3 MOVE 5 TO WZ-SCHALT GO E.
           IF WH-PG not = 7;
               IF WE-KOPF(2) = 0 MOVE 5 TO WZ-SCHALT
                   IF WE-LFS = 1 and WH-PG = 6 MOVE 5 TO WZ-SCHALT
               end-if GO E.
           MOVE 2 TO WH-KEY.
           OPEN INPUT LIEFKO.
           MOVE 1 TO WZ-SCHALT.
           MOVE SPACE TO TK-KOPF.
       D.  READ LIEFKO.
           IF WF-STATUS = "00" MOVE TK-KOPF TO KO-KOPF GO D1.
           READ KONSTANT IGNORE LOCK.
       D1. MOVE X"1B21" TO DRA-SATZ(1:)
           IF WH-PG =  7;
               IF WM-DRU = 0 MOVE X"0900" TO DRA-SATZ(3:2)
                             PERFORM DRUCK
               else MOVE SPACE TO DRA-SATZ
               INSPECT WT-BEZ REPLACING ALL "#" BY ","
               MOVE WT-BEZ TO DRA-TX
               PERFORM DRUCK
               DIVIDE 100 INTO LF-NUM GIVING WK-ANZ REMAINDER WK-ANZ
               GO G.
           EVALUATE WH-KEY
               WHEN 2 MOVE X"3B00" TO DRA-SATZ(3:2) PERFORM DRUCK
               WHEN 3 MOVE X"3100" TO DRA-SATZ(3:2) PERFORM DRUCK
               WHEN 5 MOVE X"0100" TO DRA-SATZ(3:2) PERFORM DRUCK.
           IF WH-KEY = 5 MOVE KO-KOPF TO DRA-SATZ(7:)
               ELSE MOVE KO-KOPF TO DRA-SATZ(4:).
           PERFORM DRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO D.
           CLOSE LIEFKO.
           MOVE ALL "-" TO DRA-TX.
       E.  PERFORM DRUCK.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE 3 TO WZ-SCHALT.
           MOVE WT-TXT(DE-ANR + 1) TO DRA-SATZ(17:).
           PERFORM DRUCK.
           MOVE SPACE TO WR-ADR(3).
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
              MOVE WR-ADR(TX) TO DRA-SATZ(17:)
              IF TX = 5 ADD 1 TO WM-LFS
                  MOVE WM-LFS TO WD-LFNUM
                  MOVE WD-LFNUM TO DRA-SATZ(50:)
                  DISPLAY "/ " AT 0340 WD-LFNUM
              end-if
              PERFORM DRUCK.
           MOVE 4 TO WZ-SCHALT.
           IF WE-LFS = 1 MOVE 3 TO WZ-SCHALT            *> Version Kern
               IF WK = 1 MOVE 1 TO WZ-SCHALT
                         MOVE " öbertrag" TO DRA-SATZ(46:)
                         PERFORM DRUCK
                         MOVE 2 TO WZ-SCHALT.
           MOVE 0 TO WK-ANZ.
       G.  MOVE X"1B213100" TO DRA-SATZ(1:).
           IF WK-ANZ > 89 OR (WM-VER = 2 AND WM-KO = 1)
                MOVE "  Bestellung" TO DRA-SATZ(8:)
           else MOVE "Lieferschein" TO DRA-SATZ(8:).
           MOVE X"1B210100" TO DRA-SATZ(20:4).
           MOVE "Nr.:" TO DRA-SATZ(27:).
           EVALUATE WM-PANBS
               WHEN 2 MOVE "B" TO DRA-SATZ(32:1)
               WHEN 3 MOVE "K" TO DRA-SATZ(32:1).
           MOVE LF-NUM TO WD-LFNUM.
           MOVE WD-LFNUM TO DRA-SATZ(34:6).
           MOVE "/" TO DRA-SATZ(41:).
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRA-SATZ(42:8).
           MOVE VDU-GZDAT TO DRA-SATZ(52:).
           MOVE LF-TOUR TO WD-POS.
           MOVE WD-POS TO DRA-SATZ(81:3).
           PERFORM DRUCK.
       I.  IF WH-PG = 7 MOVE 2 TO WZ-SCHALT GO Z.
030330     IF WE-LFS = 1 MOVE 4 TO WZ-SCHALT GO Z.
           MOVE "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒ¬ƒ
      -        "ƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒø" TO DRA-TX.
      *    IF WE-LFS = 1
      *        MOVE "ƒƒ¬ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒø" TO DRA-TX(56:).
           PERFORM DRUCK.
           IF WM-PR = 0 MOVE "≥Artikelnummer≥ B e z e i c h n u n g    ≥     
      -        "     Menge ≥Meh≥   Retour   ≥  Nachliefg. ≥" TO DRA-TX
                   ELSE MOVE "≥Artikelnummer≥ B e z e i c h n u n g    ≥
      -        "     Menge ≥Meh≥            ≥  empf.Preis ≥" TO DRA-TX.
      *    IF WE-LFS = 1
      *        MOVE "≥ Nachlieferungen ≥ Retour ≥" TO DRA-TX(58:).
           PERFORM DRUCK.
           MOVE "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒ¡ƒ
      -             "ƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒŸ" TO DRA-TX.
      *    IF WE-LFS = 1
      *        MOVE "ƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ¥" TO DRA-TX(56:).
           PERFORM DRUCK.
           IF WE-LFS not = 1 PERFORM DRUCK.
           GO Z.
      *----------------------------------------> kleiner Lieferschein <-
       K.  MOVE 0 TO WZ-SCHALT.
           MOVE LF-NUM TO WD-LFNUM.
           MOVE WD-LFNUM TO DRA-SATZ(27:6).
           MOVE "/" TO DRA-SATZ(36:1).
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRA-SATZ(38:8).
           PERFORM DRUCK.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE 3 TO WZ-SCHALT.
           MOVE WT-TXT(DE-ANR + 1) TO DRA-SATZ(11:)
           PERFORM DRUCK.
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
               MOVE WR-ADR(TX) TO DRA-SATZ(11:)
               IF TX NOT = 3 PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE 0 TO WK-ANZ.
           MOVE VDU-GZDAT TO DRA-SATZ.
           PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
           PERFORM DRUCK.
           GO Z.
      *-----------------------------------------------> WE-TYP(2) = 1 <-
       N.  MOVE 1 TO WZ-SCHALT.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE 3 TO WZ-SCHALT.
           MOVE WT-TXT(DE-ANR + 1) TO DRA-SATZ(11:)
           PERFORM DRUCK.
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
               MOVE WR-ADR(TX) TO DRA-SATZ(11:)
               IF TX NOT = 3 PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE 0 TO WK-ANZ.
           GO G.
       Z.  EXIT.
      ******************************************************************
       BAR SECTION.
       A.  MOVE WH-EAN TO WM-EAN.
           ADD WT-7(1) WT-7(3) WT-7(5) WT-7(7) GIVING WI.
           MULTIPLY 3 BY WI.
           ADD WT-7(2) WT-7(4) WT-7(6) TO WI.
           DIVIDE 10 INTO WI GIVING WI REMAINDER WI.
           SUBTRACT WI FROM 10 GIVING WT-7(8).
           MOVE WM-EAN TO WH-EAN.
           MOVE 3 TO ET-HOE.
           MOVE 9 TO ET-LIN.
           SET X3 TO 1.
           MOVE LOW-VALUE TO WS-MUSTER.
           SET X1 IX TO 1.
           MOVE 1 TO WI.
       B.  IF WH-ANF(WI) = 1 MOVE WX-FF TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF WI < 6 ADD 1 TO WI GO B.
       E.  IF WT-7(IX) = 0 MOVE 10 TO WS-LR
                ELSE MOVE WT-7(IX) TO WS-LR.
       F.  IF IX > 4; IF WH-RE(WS-LR X3) = 1 MOVE WX-FF TO WH-STRI(X1)
                      ELSE MOVE  LOW-VALUE TO WH-STRI(X1)
                 ELSE IF WH-LI(WS-LR X3) = 1 MOVE WX-FF TO WH-STRI(X1)
                      ELSE MOVE LOW-VALUE TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF X3 < 7 SET X3 UP BY 1 GO F.
           SET X3 TO 1.
           IF IX NOT = 4 GO I.
           MOVE 2 TO WI.
       H.  IF WH-MIT(WI) = 1 MOVE WX-FF TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF WI < 5 ADD 1 TO WI GO H.
       I.  SET IX UP BY 1.
           IF IX NOT > 8 GO E.
           MOVE 2 TO WI.
       K.  IF WH-END(WI) = 1 MOVE WX-FF TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF WI < 4 ADD 1 TO WI GO K.
       Z.  EXIT.
      *********************************************** Tagestourenliste *
       TAGTOUR SECTION.
       A.  CALL "CAUP" USING "0707100550000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Tagestourenliste " with highlight AT VDU-LP.
           MOVE 0 TO WV-TOUR WV-KTONR WZ-ZEILEN WZ-SEITE.
           MOVE "TAGTOUR.LST" TO WH-DRUNAM.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 235 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Tourenaufstellung fÅr:" AT VDU-LP.
           ADD 326 VDU-ECK GIVING VDU-LP.
           MOVE WM-DATUM TO WC-DATUM WV-DATUM.
           CALL "CAUP" USING "04DATD" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
       E.  CALL "CAUP" USING "1103266006" WH-CREG.
           IF ESC GO X.
           IF NOT RET OR WZ-DATUM = 0 GO C.
           MOVE WZ-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WH-MCODE NOT = SPACE GO E.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE 0 TO LF-TOUR LF-SUB LF-KTONR LF-NUM WV-LFNUM.
           START LFSCHEIN KEY NOT < LF-AKEY INVALID GO X.
       G.  READ LFSCHEIN NEXT IGNORE LOCK AT END GO X.
           IF WV-DATUM NOT = LF-DATUM GO Q.
           IF LF-TOUR > 20 GO Q.
           IF WV-TOUR = 0 MOVE LF-TOUR TO WV-TOUR.
           IF WV-TOUR NOT = LF-TOUR MOVE 79 TO WZ-ZEILEN
               MOVE 0 TO WV-KTONR
               MOVE LF-TOUR TO WV-TOUR.
           IF LF-NUM = WV-LFNUM AND LF-KTONR = WV-KTONR GO G.
           MOVE LF-NUM TO WV-LFNUM.
           MOVE LF-KTONR TO DE-KTONR WV-KTONR.
           MOVE 1 TO DE-FNR.
       I.  READ DEBITOR IGNORE LOCK INVALID MOVE "- fehlt -" TO DE-BEZ.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-BEZ(1:45) AT VDU-LP.
           PERFORM TOURKOPF.
           DIVIDE 10 INTO DE-KTONR GIVING DRT-KTONR.
           MOVE LF-SUB TO DRT-TSUB.
           MOVE WT-BEZ TO DRT-BEZ.
           PERFORM DRUCK.
           GO G.
       Q.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
      ************************************************ Tagestourenkopf *
       TOURKOPF SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"0008" TO WH-PX(2).
           MOVE "&l26a1h6d0O(s0p12h0b0s6T" TO WE-STG(10).
           MOVE WE-DRU(4) TO WM-DRU.
           IF WM-DRU = 1 MOVE 10 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Tagestourenliste " TO DRT-STR.
           ADD LF-TOUR 10 GIVING WH-KEY.
           MOVE LF-TOUR TO WD-POS.
           MOVE WD-POS TO DRT-STR(18:3).
       C.  READ KONSTANT IGNORE LOCK INVALID
                MOVE "Tour fehlt" TO KO-TOUR.
           MOVE KO-TOUR TO DRT-STR(22:25).
           MOVE "vom:" TO DRT-STR(55:5).
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATD" WH-CREG.
           MOVE VDU-DATUM TO DRT-STR(60:8).
           MOVE "Seite:" TO DRT-STR(75:8).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-POS.
           MOVE WD-POS TO DRT-STR(82:4).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-STR PERFORM DRUCK.
           MOVE "Kd.Nr.  Tsub. Bezeichnung" TO DRT-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************* Ean-Etikettenauswahl *
       EAN-AUSWAHL SECTION.
       A.  GO Z.
           CALL "CAUP" USING "07070850000" WH-CREG.
           DISPLAY " EAN-Etikettendruck " with highlight AT VDU-LP
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - alle Kunden" AT VDU-LP.
      *    DISPLAY "2 -
       Z.  EXIT.
      ************************************ DatenÅbernahme Hofer-MÑrkte *
       HOLDATEN SECTION.
       A.  IF WM-VER not = 3 GO Z.                              *> Lohr
           MOVE 1 TO WH-KEY.
           READ KONSTANT INVALID STOP RUN.
           IF KO-ETKDRU not = 1 and not = 2
               DISPLAY "Kein Drucker eingetragen" AT 2401
               PERFORM WEITER
               GO Z.
      *-----------------------------------------> Testdruckaufteilung <-
           IF WL-CA = 95;
               EVALUATE KO-ETKDRU
                   WHEN 1 PERFORM TESTDRUCK
                   WHEN 2 CALL "PANETIK" USING "95TEST" WH-CREG
                          CANCEL "PANETIK"
               end-evaluate
               GO Z.
           CALL "CAUP" USING "0707050670000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           IF WL-CA = 70 PERFORM EAN-AUSWAHL
      *        DISPLAY " Vorbereitungslistung " with highlight AT VDU-LP
               DISPLAY "EAN-Etikettendruck " with highlight AT VDU-LP
               GO C
           else DISPLAY " Hofer-Modemdaten " with highlight AT VDU-LP.
           MOVE 36 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           IF KO-MODEM = SPACE DISPLAY "Pkt. 14 falsch" AT 2401
                               PERFORM WEITER
                               GO X.
           CALL "CBL_COPY_FILE" using KO-MODEM "MODEM.DAT ".
           IF RETURN-CODE not = 0
               DISPLAY "Keine Datei vorhanden!" at 2401
               PERFORM WEITER
               GO X.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE WM-DATUM TO WV-DATUM.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "fÅr Lieferung am: " AT VDU-LP.
       D.  COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WV-DATUM).
           ADD 1 TO WA-DAT.
       E.  COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT).
           MOVE WV-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           IF WO-TGN = 0; IF AB ADD -1 TO WA-DAT GO E
                          else GO D.
           MOVE WX-DATUM TO WV-DATUM.
           ADD 0321 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
               WR-TN(WO-TGN + 1) with highlight foreground-color 3.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
       F.  DISPLAY "<ret>= Datum, <esc>= Abbruch, <ret-leer>= wiederhole
      -        "n, <Ende>= Einzeletikett" AT 2301.
           DISPLAY "< >= nÑchster, < >= vorheriger Tag" AT 2401.
           DISPLAY "<Pos-1>= Gutscheindruck" with highlight AT 2501.
           CALL "CAUP" USING "1103216006" WH-CREG.
           IF ESC GO Z.
           IF AUF GO D.
           IF APOS PERFORM GUTSCHEIN GO X.
           IF ENDE and WH-PG = 13 PERFORM EINZ-ETIK GO X.
           IF AB ADD -1 TO WA-DAT GO E.
           IF not RET GO F.
           IF WZ-DATUM = 0 GO F.
           MOVE WX-DATUM TO WV-DATUM LF-DATUM WS-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
               WR-TN(WO-TGN + 1) with highlight foreground-color 3.
           IF WV-DATUM not > WM-DATUM;
               DISPLAY "<Einfg>= ok < >" AT 2301
               DISPLAY "Datum muss grîsser sein!" AT 2401
               CALL "CAUP" USING "0023140000" WH-CREG
               IF not EINF GO F.
           MOVE WV-DATUM TO WZ-DATUM.
      *    IF WH-PG = 13 PERFORM HOFLIST GO X.
       G.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunden-Nr.:       " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Kuden-Nr., alpha+<ret>= Kunde
      -        "n suchen" AT 2301.
           DISPLAY "<ret-leer>= alle Kunden, Nr.+<ret>= ab dieser Kunden
      -        "-Nr." AT 2401.
           CALL "CAUP" USING "1004155010 " WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO C.
           IF not RET GO G.
           IF WH-NUM = 0; IF WV-MCODE not = SPACE
               MOVE WV-MCODE TO DE-MCODE
               CALL "PANANZ" USING "10DEB-ANZ" WH-CREG
               CANCEL "PANANZ"
               IF ESC or WH-NUM = 0 GO G.
           IF WH-NUM = 0 DISPLAY "alle Kunden" AT VDU-LP
               MOVE 0 TO WM-KTONR
               GO H.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR WM-KTONR = WH-NUM * 10 + 2.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP " ".
           READ DEBITOR IGNORE LOCK INVALID GO G.
           UNLOCK DEBITOR.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with SIZE 45 AT 0000.
       H.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF not RET and not EINF GO H.
           MOVE 0 TO WH-MOD.
           IF WH-PG = 13 and KO-ETKDRU = 2
               CALL "PANETIK" USING "10ETIKETT" WH-CREG
               CANCEL "PANETIK"
               GO X.
           IF WH-PG = 13 and KO-ETKDRU = 1 PERFORM EAN-ETIK
               GO X.
           OPEN INPUT MODEMDAT.
           IF WF-STATUS = "35" GO W.
       I.  READ MODEMDAT AT END GO Q.
           IF WH-MOD = 1 and MD-SA not = 0 GO I.
           EVALUATE MD-SA
               WHEN 16 IF MD-LFDAT not = WC-DATUM PERFORM DATVER
                           EVALUATE TRUE
                               WHEN ESC GO Q
                               WHEN WOLI MOVE 1 TO WH-MOD
                               WHEN AB MOVE 0 TO WH-MOD
                               WHEN AUF MOVE 0 TO WH-MOD
                           end-evaluate
                       end-if GO I
               WHEN 00
               WHEN 70 PERFORM LF-KOPF GO I
               WHEN 97
               WHEN 98
               WHEN 99 GO I.
           MOVE MD-ART TO FA-KUARNUM WD-NUM.
           MOVE MD-ANZ TO LF-MENGE(1).
           MOVE DE-KTONR TO FA-KTONR.
       K.  READ FAKTDAT IGNORE LOCK KEY FA-BKEY INVALID
               DISPLAY "Artikel:" AT 2401 WD-NUM " fehlt beim Kunden! -
      -            "bitte anlegen!" with highlight foreground-color 6
               PERFORM WEITER
               GO K.
           MOVE FA-ARNUM TO LF-ARNUM WD-ART AR-NUM.
       M.  READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel fehlt" TO AR-BEZ.
           MOVE AR-BEZ TO LF-BEZ.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-NUM AT VDU-LP " - " WD-ART.
           MOVE AR-GRM TO LF-GRM.
           MOVE WS-DATUM TO LF-DATUM WZ-DATUM.
           COMPUTE LF-NUM = WZ-TAG * 100.
           WRITE LF-SATZ INVALID REWRITE LF-SATZ.
           GO I.
       Q.  CLOSE MODEMDAT.
           GO X.
       W.  DISPLAY "Keine Daten vorhanden" with highlight
                         foreground-color 4 AT 2401.
           PERFORM WEITER.
           GO X.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************************** Lieferscheinkopfdaten *
       LF-KOPF SECTION.
       A.  COMPUTE DE-KTONR = (2000 + MD-FILNR) * 10 + 2.
           INITIALIZE LF-SATZ.
           MOVE DE-KTONR TO LF-KTONR.
           MOVE WS-DATUM TO LF-DATUM WZ-DATUM.
           COMPUTE LF-NUM = WZ-TAG * 100.
       C.  READ DEBITOR IGNORE LOCK INVALID
              DISPLAY "Kunde nicht angelegt!" with highlight at 2401
              PERFORM WEITER
              GO C.
           MOVE DE-TOUR TO LF-TOUR.
           MOVE DE-SUB TO LF-SUB.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO
           DISPLAY WD-KTO with highlight AT VDU-LP " " DE-BEZ(1:50).
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(2)(9:) TO WT-TX.
           MOVE 0 TO WH-MOD.
       Z.  EXIT.
      ************************************************** Listung Hofer *
       HOFLIST SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Vorbereitungsliste fÅr:          wird gedruckt!"
               AT VDU-LP.
           ADD 327 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight foreground-color 6
                AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Kundengruppe (2-9)" AT 2301.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kundengruppe: " AT VDU-LP.
           CALL "CAUP" USING "1004171001" WH-CREG.
           IF ESC GO Z.
           IF not RET GO C.
           IF WH-WERT < 2 GO C.
           PERFORM LIST-INIT.
           IF ESC GO C.
           OPEN OUTPUT ZUSHILF.
           CLOSE ZUSHILF.
           OPEN I-O ZUSHILF.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WM-OPEN WL.
           MOVE "PRODUKT.LST" TO WH-DRUNAM.
           MOVE 0 TO WI.                              *> Schleifenstart
       E.  ADD 1 TO WI.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE 0 TO WT-HMG(WX).
           IF WT-FILNUM(WI) = 0 GO K.
           MOVE LOW-VALUE TO LF-SATZ.
           MOVE WT-FILNUM(WI) TO LF-KTONR DE-KTONR.
           ADD 503 VDU-ECK GIVING VDU-LP.
           START LFSCHEIN KEY NOT < LF-KEY INVALID GO E.
       G.  READ LFSCHEIN NEXT IGNORE LOCK AT END GO I.
           MOVE DE-KTONR TO WD-KTO.
           DISPLAY WD-KTO AT VDU-LP.
           IF DE-KTONR not = LF-KTONR GO I.
           IF WV-DATUM NOT = LF-DATUM GO G.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               IF LF-ARNUM = WT-ARNUM(WX) ADD LF-MENGE(1) TO WT-HMG(WX)
                   MOVE LF-GRM TO WF-GRM
                   MOVE WF-NK TO WT-ARNK(WX)
                   MOVE 41 TO WX.
           GO G.
      *---------------------------------------------> Filiale beendet <-
       I.  INITIALIZE ZH-SATZ.
           MOVE WT-FILBEZ(WI) TO ZH-FILBEZ.
           MOVE DE-KTONR TO ZH-KTONR.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE WT-HMG(WX) TO ZH-HMG(WX).
           WRITE ZH-SATZ.
           GO E.
      *-----------------------------------------------> Start Listung <-
       K.  MOVE 9 TO WS.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               COMPUTE WI = WX + WL
               IF WT-ARBEZ(WI) = SPACE ADD -1 WX GIVING WS
                                       MOVE 10 TO WX.
           MOVE LOW-VALUE TO ZH-SATZ.
           START ZUSHILF KEY NOT < ZH-KEY INVALID GO W.
       L.  READ ZUSHILF NEXT IGNORE LOCK AT END GO N.
           PERFORM HOLIKO.
           MOVE "≥" TO DRH-SATZ(3:1).
           MOVE "≥" TO DRH-SATZ(20:1).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WI = WX + WL
               COMPUTE WR = WX * 10 + 20
               MOVE "≥" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
           MOVE ZH-FILBEZ TO DRH-BEZ.
           MOVE "≥" TO DRH-SATZ(3:1).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               COMPUTE WI = WX + WL
               IF WI = WM-STG PERFORM STEIGEN
               else IF ZH-HMG(WI) not = 0 MOVE WT-ARNK(WI) TO WF-NK
                       MOVE ZH-HMG(WI) TO WH-ANZ
                       ADD ZH-HMG(WI) TO WT-HMG(WI)
                       PERFORM MG
                       MOVE WD-MENGE TO DRH-MENGE(WX).
           MOVE "≥" TO DRH-SATZ(20:1).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WI = WX + WL
               COMPUTE WR = WX * 10 + 20
               MOVE "≥" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
           MOVE "√" TO DRH-SATZ(3:1).
           MOVE ALL "ƒ" TO DRH-STR(1:20).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WI = WX + WL
               COMPUTE WR = WX * 10 + 10
               MOVE "≈ƒƒƒƒƒƒƒƒƒ" TO DRH-SATZ(WR:10)
               end-perform
               ADD 10 TO WR
               MOVE "¥" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
           GO L.
       N.  MOVE 0 TO WZ-SCHALT.
           PERFORM RASTRI.
           MOVE "≥" TO DRH-SATZ(3:1).
           MOVE "G E S A M T:" TO DRH-BEZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               COMPUTE WI = WX + WL
               IF WT-HMG(WI) not = 0 MOVE WT-ARNK(WI) TO WF-NK
                   MOVE WT-HMG(WI) TO WH-ANZ
                   PERFORM MG
                   MOVE WD-MENGE TO DRH-MENGE(WX).
           MOVE "≥" TO DRH-SATZ(20:1).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WI = WX + WL
               COMPUTE WR = WX * 10 + 20
               MOVE "≥" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
           MOVE "¿" TO DRH-SATZ(3:1).
           MOVE ALL "ƒ" TO DRH-STR(1:20).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WR = WX * 10 + 10
               MOVE "¡ƒƒƒƒƒƒƒƒƒ" TO DRH-SATZ(WR:10)
               end-perform
               ADD 10 TO WR
               MOVE "Ÿ" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
           IF WL = ((WD-KZ - 1) * 10) GO W.
           ADD 10 TO WL.
           MOVE 70 TO WZ-ZEILEN.
           GO K.
       W.  PERFORM END-DRU.
           CLOSE ZUSHILF.
       Z.  EXIT.
      ******************************************************************
       RASTRI SECTION.
       A.  COMPUTE WL-GROSS = WS * 300  + 510.
           MOVE "&f0S*p-10X*p-15Y*c3180a100b20g2P&f1S" TO DRH-STR.
           MOVE WL-GROSS TO DRH-STR(23:4).
           IF WE-STG(10)(30:1) = "6"
               MOVE "75b20g2P&f1S" TO DRH-STR(28:).    *> bei 8 Zeilen
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       STEIGEN SECTION.
       A.  DIVIDE 6 INTO ZH-HMG(WI + 1) GIVING WH-NUM REMAINDER WM-REST.
           MOVE WH-NUM TO ZH-HMG(WI).
           MOVE "+" TO WM-KZ.
           IF WM-REST > 3 ADD 1 TO ZH-HMG(WI)
               MOVE "-" TO WM-KZ
               SUBTRACT WM-REST FROM 6 GIVING WM-REST.
           MOVE 0 TO WF-NK.
           MOVE ZH-HMG(WI) TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MENGE(4:) TO DRH-MENGE(WX).
           IF WM-REST = 0 GO Z.
           MOVE WM-REST TO WD-POS.
           MOVE WD-POS TO DRH-MENGE(WX)(6:).
           MOVE WM-KZ TO DRH-MENGE(WX)(6:1).
       Z.  EXIT.
      ******************************************************************
       LIST-INIT SECTION.
       A.  MOVE WH-WERT TO WN-KZ.
           OPEN INPUT LISTMOD.
           IF WF-STATUS = "35" PERFORM NO-REC
              SET ESC TO TRUE GO Z.
           INITIALIZE WT-HART.
       C.  READ LISTMOD AT END GO Z.
           IF LM-SATZ = SPACE GO X.
           IF LM-SA = "9" GO Q.
           IF LM-ZUS = "-" GO C.
           IF LM-SA = "0" PERFORM FILTAB GO C.              *> Filialen
           IF LM-SA = "1" PERFORM ARTTAB GO C.              *> Artikel
           IF LM-SA = "2" PERFORM ARTTAB GO C.              *> Artikel
           IF LM-SA = "3" PERFORM ARTTAB GO C.              *> Artikel
           IF LM-SA = "4" PERFORM ARTTAB GO C.              *> Artikel
           MOVE LM-SATZ TO WM-LIKO.
           DISPLAY WN-KZ with highlight foreground-color 6 AT VDU-LP
                " " LM-SATZ(1:20) with highlight foreground-color 3.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO E.
           GO C.
       Q.  MOVE "&l26a1h6d1O(s0p10h2b0s6T&l8C" TO WE-STG(10).
           MOVE 38 TO WM-Z.
           MOVE 1 TO WE-STG(10)(11:1).                    *> quer
           MOVE "8" TO WE-STG(10)(30:1).                  *> 6 Zeilen
           IF LM-SATZ(3:1) = "h" MOVE 61 TO WM-Z
               MOVE 0 TO WE-STG(10)(11:1).                *> hoch
           IF LM-SATZ(5:1) = "8" MOVE 51 TO WM-Z
               MOVE "6" TO WE-STG(10)(30:1).              *> 8 Zeilen
       X.  CLOSE LISTMOD.
       Z.  EXIT.
      ******************************************************* Filialen *
       FILTAB SECTION.
       A.  MOVE 0 TO WM-KTO.
           MOVE LM-TX(1:5) TO WM-NUM.
           MOVE WM-KTO TO WM-VERGL.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 30
               IF WT-FILNUM(WX) = 0
                  MOVE WM-VERGL TO WT-FILNUM(WX)
                  MOVE LM-TX(6:) TO WT-FILBEZ(WX)
                  MOVE 31 TO WX.
       Z.  EXIT.
      ******************************************************** Artikel *
       ARTTAB SECTION.
       A.  MOVE 0 TO WM-KTO.
           MOVE LM-TX(1:3) TO WM-NUM(3:).
           MOVE WM-KTO TO WM-VERGL.
           MOVE LM-SA TO WD-KZ.
           COMPUTE WX = WD-KZ * 10 - 9.
           PERFORM VARYING WX FROM WX BY 1 UNTIL WX > 40
               IF WT-ARNUM(WX) = 0
                  MOVE WM-VERGL TO WT-ARNUM(WX)
                  MOVE LM-TX(4:) TO WT-ARBEZ(WX)
                  IF WM-VERGL = 998
                      MOVE WX TO WM-STG                *> Steigenspalte
                  end-if
                  MOVE 41 TO WX.
       Z.  EXIT.
      ******************************************************************
       HOLIKO SECTION.
       A.  IF WZ-ZEILEN > WM-Z WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           IF WM-OPEN = 0 MOVE 10 TO WH-P
               MOVE WE-DRU(4) TO WM-DRU
               PERFORM BEG-DRU.
           MOVE "(s1p10h17v3b0s4101T" TO DRH-STR.
           MOVE WM-LIKO TO DRH-STR(21:).
           PERFORM VARYING WM-L FROM 40 BY -1 UNTIL WM-L = 1
               or DRH-STR(WM-L:1) not = SPACE CONTINUE.
           ADD 7 TO WM-L.
           MOVE VDU-DATUM TO DRH-STR(WM-L:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-POS.
           ADD 10 TO WM-L.
           MOVE "/" TO DRH-STR(WM-L:).
           ADD 2 TO WM-L.
           MOVE WD-POS TO DRH-STR(WM-L:).
           ADD 5 TO WM-L.
           MOVE "(s0p10h2b0s6T" TO DRH-STR(WM-L:).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           PERFORM RASTRI.
           MOVE 0 TO WZ-SCHALT.
           MOVE ALL "ƒ" TO DRH-STR(1:20).
           MOVE "⁄" TO DRH-SATZ(3:1).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WR = WX * 10 + 10
               MOVE "¬ƒƒƒƒƒƒƒƒƒ" TO DRH-SATZ(WR:10)
               end-perform
               ADD 10 TO WR
               MOVE "ø" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
           MOVE "≥" TO DRH-SATZ(3:1).
           MOVE "≥" TO DRH-SATZ(20:1).
           MOVE "Filiale" TO DRH-BEZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WI = WX + WL
               MOVE WT-ARBEZ(WI) TO DRH-MENGE(WX)(2:10)
               COMPUTE WR = WX * 10 + 20
               MOVE "≥" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRH-STR(1:20).
           MOVE "√" TO DRH-SATZ(3:1).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WS
               COMPUTE WR = WX * 10 + 10
               MOVE "≈ƒƒƒƒƒƒƒƒƒ" TO DRH-SATZ(WR:10)
               end-perform
               ADD 10 TO WR
               MOVE "¥" TO DRH-SATZ(WR:1).
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       DATVER SECTION.
       A.  CALL "CAUP" USING "1323012380000" WH-CREG.
           MOVE MD-LFDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           DISPLAY "Datum lt. Bestellung: " AT 2301
               VDU-DATUM with highlight " richtig wÑre: ".
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           DISPLAY VDU-DATUM with highlight foreground-color 6 AT 0000.
           IF MD-LFDAT < WC-DATUM
               DISPLAY "Lieferdatum zu klein" AT 2401
               PERFORM WEITER
               SET ESC TO TRUE
               GO Z.
           DISPLAY "<esc>= Abbruch, <>= abweisen, < >= Datumskorr./Anna
      -        "hme, < >= Lieferdatum ok < >" AT 2401.
           CALL "CAUP" USING "0024790000" WH-CREG.
           MOVE WV-DATUM TO WS-DATUM.
           IF ESC GO Z.
           IF AB GO Z.
           IF AUF MOVE MD-LFDAT TO WS-DATUM GO Z.
           IF not WOLI GO A.
       Z.  EXIT.
      ********************************************* EAN-Etikettendruck *
       EAN-ETIK SECTION.
       A.  IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               OPEN OUTPUT DRUCKER
               OPEN INPUT ETIKETT.
           MOVE 0 TO WV-KTONR WS-ETIK.
           INITIALIZE DE-SATZ.
           MOVE 1 TO DE-ETIK.
           MOVE WM-KTONR TO DE-KTONR.
           START DEBITOR KEY not < DE-EKEY INVALID GO Z.
       C.  READ DEBITOR NEXT IGNORE LOCK AT END GO Q.
           IF DE-ETIK not = 1 GO C.
           IF WV-KTONR not = 0 DISPLAY "Kundenwechsel < >" AT 2301
               DISPLAY "<esc>= Abruch, <ret>= weiter" AT 2401
               CALL "CAUP" USING "0023160000" WH-CREG
               IF ESC GO Z.
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE DE-KTONR TO LF-KTONR.
           ADD 415 VDU-ECK GIVING VDU-LP.
           COMPUTE WD-KTO = DE-KTONR / 10.
           DISPLAY WD-KTO with highlight AT VDU-LP " ".
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with SIZE 45 AT 0000.
           MOVE WS-DATUM TO WZ-DATUM.
           COMPUTE LF-NUM = WZ-TAG * 100.
           MOVE 0 TO LF-ARNUM.
           START LFSCHEIN KEY not < LF-KEY INVALID GO C.
       E.  READ LFSCHEIN NEXT IGNORE LOCK AT END GO C.
           ADD 520 VDU-ECK GIVING VDU-LP.
           IF DE-KTONR not = LF-KTONR GO C.
           IF WS-DATUM not = LF-DATUM GO E.
       I.  EVALUATE LF-NK
               WHEN 0 MOVE LF-MENGE(1) TO WD-ET
               WHEN 1 COMPUTE WD-ET = LF-MENGE(1) / 10
               WHEN 2 COMPUTE WD-ET = LF-MENGE(1) / 100.
           MOVE LF-ARNUM TO ET-NUM.
           MOVE "E" TO ET-SA.
           READ ETIKETT IGNORE LOCK INVALID GO E.
           IF ET-SONDER = "S" GO E.
           MOVE LF-ARNUM TO WD-NUM.
           DISPLAY WD-NUM AT VDU-LP.
           IF WV-KTONR not = LF-KTONR MOVE LF-KTONR TO WV-KTONR
               PERFORM NAM-ETIK
               MOVE 1 TO WH-ANZ
               PERFORM ADD-DIS.
           MOVE WD-ET TO WH-ANZ.
           PERFORM ADD-DIS.
           PERFORM ETIK-DRU.
           GO E.
       Q.  MOVE WS-ETIK TO WD-ETIK.
           ADD 540 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ETIK with highlight AT 2301 " Etiketten gedruckt!
      -        "weiter mit <ret> < >".
           CALL "CAUP" USING "0023460000" WH-CREG.
       Z.  EXIT.
      ************************************************ Anzahl ausgeben *
       ADD-DIS SECTION.
       A.  ADD WH-ANZ TO WS-ETIK.
           MOVE WS-ETIK TO WD-ETIK.
           ADD 540 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ETIK AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       ETIK-DRU SECTION.
       A.  PERFORM ETIK-START.
           MOVE "^LH30,330" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F025,10^CF0,51,35^FD" TO DRE-SATZ.
           MOVE ET-BEZ-A TO DRE-SATZ(22:).
           PERFORM FS-ZEI.
           MOVE "^LH30,400" TO DRE-SATZ.
           PERFORM DRUCK
           MOVE "^F020,10^A0,21^FD" TO DRE-SATZ.
           MOVE ET-ZU-A TO DRE-SATZ(18:).
           IF DRE-SATZ = SPACE PERFORM DRUCK
           else PERFORM FS-ZEI.
           MOVE "^LH30,420" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,50^A0,21^FD" TO DRE-SATZ.
           MOVE ET-ZU-B TO DRE-SATZ(18:).
           IF DRE-SATZ = SPACE PERFORM DRUCK
           else PERFORM FS-ZEI.
           MOVE "^LH30,440" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,50^A0,21^FD" TO DRE-SATZ.
           MOVE ET-ZU-C TO DRE-SATZ(18:).
           IF DRE-SATZ = SPACE PERFORM DRUCK
           else PERFORM FS-ZEI.
      *-----------------------------------------------> abgepackt am: <-
           MOVE "^LH30,490" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,199^A0,21^FDabgep. am:" TO DRE-SATZ.
           MOVE WS-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM(1:6) TO DRE-SATZ(30:).
           MOVE "20" TO DRE-SATZ(36:).
           MOVE VDU-DATUM(7:2) TO DRE-SATZ(38:).
           PERFORM FS-ZEI.
      *------------------------------------> mindestends haltbar bis: <-
           MOVE "^LH30,510" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,199^A0,21^FDmindestens haltbar bis:" TO DRE-SATZ.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WS-DATUM).
           ADD ET-MHD TO WA-DAT.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT).
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM(1:6) TO DRE-SATZ(43:).
           MOVE "20" TO DRE-SATZ(49:).
           MOVE VDU-DATUM(7:2) TO DRE-SATZ(51:).
           PERFORM FS-ZEI.
      *----------------------------------------------> EAN-Strichcode <-
           MOVE "^LH50,555" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F025,450^BY3" TO DRE-SATZ.                *> Ean-Code
           PERFORM DRUCK.
           MOVE "^BEN,65,Y,N" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^FD9005793000030^FS" TO DRE-SATZ.
           MOVE ET-EAN TO DRE-SATZ(4:13).
           PERFORM DRUCK.
           MOVE "^PQ" TO DRE-SATZ.
           MOVE WD-ET(3:1) TO DRE-SATZ(4:).
           MOVE ",1,,Y" TO DRE-SATZ(5:).
           IF WD-ET > 9 MOVE WD-ET(2:2) TO DRE-SATZ(4:)
               MOVE ",1,,Y" TO DRE-SATZ(6:).
           IF WD-ET > 99 MOVE WD-ET(1:3) TO DRE-SATZ(4:)
               MOVE ",1,,Y" TO DRE-SATZ(7:).
           PERFORM DRUCK.
           MOVE "^MCN^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^XA^MCY^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       GUTSCHEIN SECTION.
       A.  ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " EAN-Gutscheindruck " with highlight AT VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WS-DATUM).
           ADD 365 TO WA-DAT.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT).
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM TO WK-DAT.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Anzahl:     zu 5,-- Euro" AT VDU-LP.
           ADD 330 VDU-ECK GIVING VDU-LP.
           DISPLAY "Start-Nr.:     " AT VDU-LP.
           DISPLAY "Gutscheinanzahl max. 99 Stk." AT 2301.
           CALL "CAUP" USING "1003112002" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           IF WH-WERT = 0 GO A.
           MOVE WH-WERT TO WK-GS.
       C.  DISPLAY "nÑchste Gutschein-Nr." AT 2401.
           DISPLAY "<esc>= Abbruch, <ret>= Nummer, < />= zurÅck"
               AT 2301.
           CALL "CAUP" USING "1003414004" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO A.
           IF not RET GO C.
           IF WH-WERT = 0 GO C.
           MOVE WH-WERT TO WK-GSNR WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           MOVE WH-DATUM TO WC-DATUM.
           IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               OPEN OUTPUT DRUCKER.
           PERFORM VARYING WK-GS FROM WK-GS BY -1 UNTIL WK-GS = 0
               PERFORM GSE-DRU
               ADD 1 TO WK-GSNR.
       Z.  EXIT.
      ******************************************** Gutschein-Etiketten *
       GSE-DRU SECTION.
       A.  PERFORM ETIK-START.
           MOVE "^LH15,320" TO DRE-SATZ
           PERFORM DRUCK.
           MOVE "^F085,10^CF0,50,60^FD" TO DRE-SATZ.
           MOVE "Eur 5,-" TO DRE-SATZ(27:)
           PERFORM FS-ZEI.
           PERFORM DRUCK.
      *
           MOVE "^LH15,370" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F075,10^CF0,20,20^FD" TO DRE-SATZ.
           MOVE "(Euro fÅnf)" TO DRE-SATZ(44:)
           PERFORM FS-ZEI.
           PERFORM DRUCK.
      *
           MOVE "^LH15,420" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F025,10^CF0,20,25^FD" TO DRE-SATZ.
           MOVE "Einlîsbar in allen Filialen der" TO DRE-SATZ(27:)
           PERFORM FS-ZEI.
           PERFORM DRUCK.
      *
           MOVE "^LH15,450" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F025,10^CF0,30,35^FD" TO DRE-SATZ.
           MOVE " BÑckerei KERN GesmbH." TO DRE-SATZ(22:)
           PERFORM FS-ZEI.
           PERFORM DRUCK.
      *
           MOVE "^LH15,510" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F025,10^CF0,25,20^FD" TO DRE-SATZ.
           MOVE "gÅltig bis: " TO DRE-SATZ(41:).
           MOVE WK-DAT TO DRE-SATZ(54:).
           PERFORM FS-ZEI.
           PERFORM DRUCK.
      *
           MOVE "^LH85,560" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F085,450^BY3" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^B2N,60,Y,N,N" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^FD00000000^FS" TO DRE-SATZ(10:).
           MOVE WC-DATUM(1:4) TO DRE-SATZ(13:4).
           MOVE WK-GSNR TO DRE-SATZ(17:4).
           PERFORM DRUCK.
           MOVE "^PQ1,1,,Y" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^MCN^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^XA^MCY^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
           PERFORM FS-ZEI
           PERFORM DRUCK.
       Z.  EXIT.
      **************************************** ^FS anhÑgen und drucken *
       FS-ZEI SECTION.
       A.  PERFORM VARYING WZ FROM 70 BY -1 UNTIL
                DRE-SATZ(WZ:1) not = SPACE CONTINUE.
           ADD 1 TO WZ.
           MOVE "^FS" TO DRE-SATZ(WZ:).
           PERFORM DRUCK.
       Z.  EXIT.
      ************************************************* Etikettenstart *
       ETIK-START SECTION.
       A.  MOVE "~EF~EG" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^XA^MMT~TA000~TA0^LT000^LT0^MNY^MTT^LH0,0^MD15^JMA^PW38
      -         "6^PR3,3^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^XA" TO DRE-SATZ.
           PERFORM DRUCK
           MOVE "^MCY^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^XA" TO DRE-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
      ************************************************* Namens-Etikett *
       NAM-ETIK SECTION.
       A.  PERFORM ETIK-START.
           PERFORM VARYING WS FROM 1 BY 1 UNTIL WS > 5
               EVALUATE WS
                   WHEN 1 MOVE "^LH30,360" TO DRE-SATZ
                   WHEN 2 MOVE "^LH30,410" TO DRE-SATZ
                   WHEN 3 MOVE "^LH30,460" TO DRE-SATZ
                   WHEN 4 MOVE "^LH30,510" TO DRE-SATZ
                   WHEN 5 MOVE "^LH30,560" TO DRE-SATZ
               end-evaluate
               PERFORM DRUCK
               MOVE "^F025,10^CF0,45,30^FD" TO DRE-SATZ
               MOVE WR-ADR(WS) TO DRE-SATZ(22:)
               PERFORM FS-ZEI
               PERFORM DRUCK.
           MOVE "^PQ1,1,,Y" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^MCN^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^XA^MCY^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
      ********************************************* EAN-Etikettendruck *
       TESTDRUCK SECTION.
       A.  OPEN OUTPUT DRUCKER.
           MOVE WM-DATUM TO WS-DATUM.
           MOVE 1 TO WD-ET.
           PERFORM ETIK-DRU.
           CLOSE DRUCKER.
       Z.  EXIT.
      ************************************* Einzel-Etiketen je Artikel *
       EINZ-ETIK SECTION.
       A.  OPEN INPUT ETIKETT.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE WM-DATUM TO WS-DATUM WV-DATUM.
           ADD 0240 VDU-ECK GIVING VDU-LP.
           DISPLAY "abgep.: " AT VDU-LP.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WV-DATUM).
       B.  COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT).
           MOVE WV-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           IF WO-TGN = 0; IF AB ADD -1 TO WA-DAT
                          else ADD 1 TO WA-DAT
                          end-if GO B.
           MOVE WX-DATUM TO WV-DATUM.
           ADD 0248 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
               WR-TN(WO-TGN + 1) with highlight foreground-color 3.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
       C.  DISPLAY "<ret>= Datum, <esc>= Abbruch, <ret-leer>= wiederhole
      -        "n" AT 2301.
           DISPLAY "< >= nÑchster, < >= vorheriger Tag" AT 2401.
           CALL "CAUP" USING "1102486006" WH-CREG.
           IF ESC GO X.
           IF AUF ADD 1 TO WA-DAT GO B.
           IF AB ADD -1 TO WA-DAT GO B.
           IF not RET GO C.
           IF WZ-DATUM = 0 GO C.
           MOVE WX-DATUM TO WV-DATUM LF-DATUM WS-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
               WR-TN(WO-TGN + 1) with highlight foreground-color 3.
           MOVE WV-DATUM TO WZ-DATUM.
       D.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Artikel:        Etikettenanzahl:      " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Artikel-Nr., alpha+<ret>= suc
      -        "hen, < />= zurÅck" AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1003134010" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO B.
           IF not RET GO D.
           IF WH-NUM = 0 MOVE WV-MCODE TO AR-ABEZ
               MOVE SPACE TO AR-SA
               CALL "PANANZ" USING "20ARTSUCH" WH-CREG
               CANCEL "PANANZ".
           MOVE WH-NUM TO AR-NUM.
           IF WH-NUM = 0 GO D.
           READ ARTIKEL IGNORE LOCK INVALID GO D.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-BEZ AT VDU-LP.
           MOVE AR-NUM TO ET-NUM.
           MOVE "E" TO ET-SA.
           READ ETIKETT IGNORE LOCK INVALID DISPLAY "Kein Etikett angele
      -        "gt!" AT 2401
               PERFORM WEITER
               GO A.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Anzahl, < />= zurÅck"
               AT 2301.
           DISPLAY "ACHTUNG! max. 150 Etiketten mîglich" AT 2401.
           CALL "CAUP" USING "1003373003" WH-CREG.
           IF ESC GO D.
           IF WOLI or AUF GO D.
           IF not RET GO E.
           IF WH-NUM = 0 GO E.
           MOVE WH-WERT TO WD-ET WM.
           DISPLAY WD-ET with highlight AT VDU-LP.
       G.  DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Start < >"
               AT 2301.
           CALL "CAUP" USING "0023460000" WH-CREG.
           IF WOLI or AUF GO E.
           IF ESC GO D.
           IF not RET GO G.
           IF KO-ETKDRU = 2 CALL "PANETIK" USING "20EINZEL" WH-CREG
                CANCEL "PANETIK"
           else OPEN OUTPUT DRUCKER
                PERFORM ETIK-DRU
                CLOSE DRUCKER.
           GO D.
       X.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           CLOSE ETIKETT.
       Z.  EXIT.
      ******************************************************************
       DRU-STAT SECTION.
       A.  MOVE DE-TOUR TO WE
           EVALUATE WDR-ANZ
               WHEN < 7 ADD 1 TO WDR-STAT(WE, 1)
               WHEN < 13 ADD 1 TO WDR-STAT(WE, 2)
               WHEN OTHER ADD 1 TO WDR-STAT(WE, 3).
       X.  MOVE 0 TO WDR-ANZ.
       Z.  EXIT.
      ******************************************************************
       STAT-ANZ SECTION.
       A.  CALL "CAUP" USING "1303012580" WH-CREG.
           CALL "CAUP" USING "270210222500013" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Tour  < 7 < 12  >12 " with highlight AT VDU-LP.
      *               12   123  123  123
      *               5    0    5    0
           MOVE 0 TO WX.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 20
               IF WDR-TOUR(WE) not = LOW-VALUES ADD 1 TO WX
                   MOVE WE TO WD-ART
                   COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK
                   DISPLAY WD-ART AT VDU-LP " "
                   MOVE WDR-STAT(WE 1) TO WDR-SUM
                   DISPLAY WDR-SUM with highlight AT 0000
                   MOVE WDR-STAT(WE 2) TO WDR-SUM
                   DISPLAY WDR-SUM with highlight AT 0000
                   MOVE WDR-STAT(WE 3) TO WDR-SUM
                   DISPLAY WDR-SUM with highlight AT 0000.
           DISPLAY "Statistik" AT 2401
           PERFORM WEITER.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** Filial-Liste Kern *
       FIL-LISTE SECTION.
       A.  MOVE "FILLISTE.LST" TO WH-DRUNAM.
           OPEN INPUT KOMMSCH.
           IF WF-STATUS = "35" GO Z.
           MOVE "&l26a4h6d0O(s0p10.6h1b0s3T" TO WE-STG(10).
           MOVE 1 TO WL.                               *> ListenzÑhler
           MOVE 60 TO WH-KEY.
       C.  READ KONSTANT INVALID INITIALIZE KF-SATZ.
           MOVE 1 TO WI.
       E.  MOVE 0 TO WZ WE.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 14
                INITIALIZE WF-FILTAB(WX).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 13
               IF KF-SEITE(WX) = WL ADD 1 TO WZ
                   COMPUTE WF-FNR(WZ)= KF-FNR(WX) * 10 + 2
                   MOVE KF-FBEZ(WX) TO WF-FBEZ(WZ).
           INITIALIZE KS-SATZ.
           START KOMMSCH KEY not < KS-KOMKEY INVALID GO W.
       G.  READ KOMMSCH NEXT AT END GO W.
           MOVE KS-ARNUM TO AR-NUM.
           READ ARTIKEL IGNORE LOCK INVALID GO G.
           MOVE AR-GRM TO WF-GRM.
           IF WF-TB > 2 GO G.
           IF AR-SGRP = 0 GO G.
           MOVE 0 TO WS.
           IF WI = 1 PERFORM ERSTE-MG
                else PERFORM ZWEITE-MG.
           IF WS = 1 PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 7
                         COMPUTE WR = WY * 8 + 26
                         MOVE "≥" TO DRK-STR(WR:1)
                     end-perform
                     MOVE KS-SGRP TO DRK-SGRP
                     MOVE AR-NUM TO DRK-ARNUM
                     MOVE AR-BEZ TO DRK-BEZ
                     MOVE DRK-STR TO WT-BEZ
                     MOVE SPACE TO DRA-SATZ
                     PERFORM FILKOPF
                     PERFORM RASTER
                     MOVE WT-BEZ TO DRK-STR
                     PERFORM DRUCK
                     MOVE 0 TO WS.
           GO G.
       W.  MOVE SPACE TO DRA-SATZ.
           IF WL < 4 ADD 1 TO WL
                     PERFORM END-DRU
                     MOVE 0 TO WZ-SEITE
                     GO E.
           IF WI = 1 MOVE 2 TO WI                   *> ZWEITE Lieferung
                     MOVE 1 TO WL
                     GO E.
           INITIALIZE LF-SATZ.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE 2 TO LF-KTONR.
           PERFORM END-DRU.
       Z.  EXIT.
      ************************************** ERSTE Lieferung abstellen *
       ERSTE-MG SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WF-FNR(WX) = 0
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 15
                   IF WF-FNR(WX) = KS-KTONR(WZ) and KS-MENGE(WZ) not = 0
                       MOVE KS-MENGE(WZ) TO WH-ANZ
                       PERFORM MG
                       MOVE WD-MENGE(3:) TO DRK-MG(WX)
                       MOVE 1 TO WS.
       Z.  EXIT.
      ************************************* ZWEITE Lieferung abstellen *
       ZWEITE-MG SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WF-FNR(WX) = 0
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 15
                  IF WF-FNR(WX) = KS-KTONR(WZ) and KS-MENGEB(WZ) not = 0
                       MOVE KS-MENGEB(WZ) TO WH-ANZ
                       PERFORM MG
                       MOVE WD-MENGE(3:) TO DRK-MG(WX)
                       MOVE 1 TO WS.
       Z.  EXIT.
      ******************************************** Kopftdruck FILLISTE *
       FILKOPF SECTION.
       A.  IF WM-OPEN = 0 MOVE 10 TO WH-P
               MOVE 1 TO WM-DRU
               PERFORM BEG-DRU.
           IF WZ-ZEILEN > 62 WRITE DRK-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           IF WZ-ZEILEN not = 0 GO Z.
           MOVE "BÑckerei Kern   -   EinzÑhlliste fÅr: " TO DRK-STR.
           MOVE VDU-DATUM TO DRK-STR(39:).
           IF WI = 1 MOVE "** ERSTE **" TO DRK-STR(53:)
                else MOVE "** ZWEITE **" TO DRK-STR(52:).
           MOVE "Seite: " TO DRK-STR(74:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-POS.
           MOVE WD-POS TO DRK-STR(80:).
           MOVE WL TO WD-POS.
           MOVE WD-POS TO DRK-STR(67:3).
           PERFORM DRUCK.
           MOVE "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒ
      -         "ƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒø" TO DRK-STR.
           PERFORM DRUCK.
           MOVE "Grp.Art.Bezeichnung" TO DRK-STR.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               IF WF-FBEZ(WX) not = SPACE COMPUTE WZ = WX * 8 + 27
                   MOVE WF-FBEZ(WX) TO DRK-STR(WZ:7).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 7
               COMPUTE WZ = WX * 8 + 26
               MOVE "≥" TO DRK-STR(WZ:1).
           PERFORM DRUCK.
           MOVE "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒ
      -         "ƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒ¥" TO DRK-STR.
           PERFORM DRUCK.
           MOVE 0 TO WE.
       Z.  EXIT.
      ******************************************************************
       RASTER SECTION.
       A.  IF WE = 1 MOVE 0 TO WE GO Z.
           ADD 1 TO WE.
           COMPUTE WZ-GROSS = 92 / 12 * 300.
           MOVE "&f0S*p-04X*p+16Y*c3050a48b20g2P&f1S" TO DRK-STR.
           MOVE WZ-GROSS TO DRK-STR(23:4).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
       Z.  EXIT.
