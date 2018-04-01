      $SET LINKCOUNT "728" ANS85 BOUND AUTOLOCK NOALTER
      $SET SEGSIZE(40000)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TILIST.
      ***************************** Anlage/énderung/Lîschen Stammdaten *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    PC.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORTIER  ASSIGN TO "TISORT.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY SR-KEY,
                           ALTERNATE RECORD SR-AKEY DUPLICATES
                           ALTERNATE RECORD SR-BKEY =
                                SR-AKEY SR-KEY DUPLICATES
                           FILE STATUS IS WF-STATUS.
       COPY SE-TOUR.CPY.
       COPY SE-ARTIK.CPY.
       COPY SE-KONST.CPY.
       COPY SE-PREIS.CPY.
       COPY SE-PRWAR.CPY.
       COPY SE-KUND.CPY.
       COPY SE-LIEF.CPY.
           SELECT EXCELIMP ASSIGN TO PRINTER WN-EXCEL
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS IS WF-STATUS.
           SELECT EXCELDRU ASSIGN TO PRINTER WN-EXCEL
                           FILE STATUS IS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FD-TOUR.CPY.
       COPY FD-ARTIK.CPY.
       COPY FD-KONST.CPY.
       COPY FD-PREIS.CPY.
       COPY FD-PRWAR.CPY.
       COPY FD-KUND.CPY.
       COPY FD-LIEF.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORDS OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(4).
           03  DRA-STR.
               05 FILLER               PIC X(42).
               05 DRA-DATTEXT          PIC X(7).
               05 DRA-DATUM            PIC 99.99.99.
               05 FILLER               PIC X(12).
               05 DRA-SEITE            PIC X(6).
               05 DRA-SEITENNR         PIC ZZZZ9.
           03  FILLER                  PIC X(41).
           03  DRA-SEITE1              PIC X(6).
           03  DRA-SEITENNR1           PIC ZZZZ9.
           03  FILLER                  PIC X(128).
       01  DRB-SATZ.
           03  FILLER                  PIC X(4).
           03  DRB-TEXT                PIC X(25).
           03  DRB-NR                  PIC ZZZZZ9-.
           03  DRB-BEZ                 PIC X(50).
       01  DRC-SATZ.
           03  FILLER                  PIC X(4).
           03  FILLER                  PIC X(25).
           03  DRC                     OCCURS 4.
               05  DRC-NR              PIC ZZZZ9-.
               05  DRC-ERLKTO          PIC ZZZZZ9-.
       01  DRD-SATZ.
           03  FILLER                  PIC X(4).
           03  DRD-STR.
               05 DRD-ARTNR            PIC ZZZZ9-.
               05 DRD-ARTBEZ           PIC X(21).
               05 DRD-ZUSBEZ           PIC X(21).
               05 DRD-KGEW             PIC ZZ,ZZ-.
               05 DRD-BGEW             PIC ZZ,ZZ-.
               05 DRD-KURZBEZ          PIC X(11).
               05 DRD-ERLKTO           PIC ZZZ9-.
               05 DRD-VP               PIC Z9-.
               05 DRD-STAT             PIC ZZZZ9-.
               05 DRD-MWST             PIC Z9-.
               05 FILLER               PIC X.
               05 DRD-EAN              PIC ZZZZZZ-.
               05 FILLER               PIC X.
               05 DRD-ZUSCHL           PIC X.
               05 DRD-NK               PIC ZZ9-.
               05 DRD-VERNR            PIC ZZZZZ-.
               05 DRD-OEPZ             PIC ZZ-.
               05 DRD-ANZ              PIC ZZZ-.
               05 DRD-PREIS            PIC ZZZ9,99-.
       01  DRE-SATZ.
           03  FILLER                  PIC X(4).
           03  DRE-NR                  PIC ZZZZ9-.
           03  DRE-BEZ                 PIC X(111).
           03  DRE-MCODE               PIC X(15).
       01  DRE1-SATZ.
           03  FILLER                  PIC X(30).
           03  DRE-PREIS               PIC ZZZZ9-.
           03  DRE-REART               PIC ZZZZ9-.
           03  DRE-PK                  PIC ZZZZZ9-.
           03  FILLER                  PIC XX.
           03  DRE-DRUCK-KZ            PIC Z99-.
           03  DRE-BAN                 PIC Z(8)9-.
           03  DRE-VERS                PIC ZZZ9-.
           03  DRE-RAB                 PIC ZZZ9,99-.
           03  DRE-BONUS               PIC ZZ,ZZ-.
           03  DRE-READR               PIC ZZZZZ9-.
           03  DRE-LIEFADR             PIC ZZZZZZ9-.
           03  DRE-FIBU                PIC ZZZZZZ9-.
      *    03  DRE-FOLGE               PIC ZZZZZZ9-.
           03  DRE-STAT                PIC ZZ9-.
           03  DRE-SCAN                PIC ZZZZ9-.
           03  DRE-EMB                 PIC ZZ9-.
           03  DRE-PEMB                PIC ZZ9-.
           03  DRE-EINZ                PIC ZZZZ9-.
       01  DRF-SATZ.
           03  FILLER                  PIC X(4).
           03  DRF-ARTNR               PIC ZZZ9.
           03  DRF-ZUSNR               PIC ZZ9-.
           03  DRF-BEZ.
               05  DRF-EAN             PIC Z(12)9.
               05  FILLER              PIC X(13).
           03  DRF-PREIS               PIC ZZ9,99-.
           03  FILLER                  PIC X(4).
           03  DRF-AKTION              PIC ZZ9,99-.
           03  DRF-VON                 PIC X(10).
           03  DRF-BIS                 PIC X(9).
           03  DRF-KZ                  PIC X.
       01  DRG-SATZ.                                 *> AR-Listung hoch
           03  FILLER                  PIC X(4).
           03  DRG-STR.
               05 DRG-ARTNR            PIC ZZZZ9-.
               05 DRG-ARTBEZ           PIC X(21).
               05 DRG-KURZBEZ          PIC X(11).
               05 DRG-ERLKTO           PIC ZZZ9-.
               05 DRG-VP               PIC Z9-.
               05 DRG-MWST             PIC Z9-.
               05 DRG-EAN              PIC ZZZZZZZ-.
               05 FILLER               PIC X.
               05 DRG-ZUSCHL           PIC X.
               05 DRG-NK               PIC ZZ9-.
               05 DRG-VERNR            PIC ZZZZZ-.
               05 DRG-OEPZ             PIC ZZ-.
               05 DRG-KGEW             PIC ZZ,ZZ-.
               05 DRG-ANZ              PIC ZZZ-.
       01  DRH-SATZ.
           03  FILLER                  PIC XX.
           03  DRH-FILNR               PIC Z.ZZ9-.
           03  DRH-GRUPPE              PIC X(6).
           03  FILLER                  PIC X.
           03  DRH-BEZ                 PIC X(39).
           03  FILLER                  PIC X.
           03  DRH-TONR                                OCCURS 7.
               05 DRH-KBEZ             PIC XXX.
               05 DRH-LFD              PIC 999-.
       01  DRI-SATZ.
           03  FILLER                  PIC XXX.
           03  DRI-STR.
               05 FILLER               PIC X(31).
               05 DRI-KDGRP            PIC ZZ.ZZ9.
               05 FILLER               PIC X(39).
               05 DRI-DATUM            PIC X(9).
               05 DRI-SST              PIC XXX.
               05 DRI-SEITE            PIC ZZ9-.
       01  DRK-SATZ.
           03  FILLER                  PIC X(4).
           03  FILLER                  PIC XXX.
           03  DRK-STR.
               05 DRK-ARTNR            PIC ZZ.ZZ9,99-.
               05 DRK-BEZ              PIC X(33).
               05 DRK-PREIS            PIC ZZ.ZZ9,99-.
               05 DRK-RAB              PIC ZZ9,99-.
               05 DRK-PZ1              PIC X.
               05 DRK-BONUS            PIC ZZ9,99-.
               05 DRK-PZ2              PIC X.
               05 DRK-SKTO             PIC ZZ9,99-.
               05 DRK-PZ3              PIC X.
               05 DRK-NTTO             PIC ZZ.ZZ9,99-.
       01  DRP-SATZ.
           03  FILLER                  PIC X(4).
           03  DRP-NR                  PIC ZZZZZ9-.
           03  DRP-BEZ                 PIC X(110).
           03  FILLER                  PIC XX.
           03  DRP-EMB                 PIC Z.ZZZ.ZZ9-.
           03  DRP-PEMB                PIC Z.ZZZ.ZZ9-.
       01  DRQ-SATZ.
           03  FILLER                  PIC X(4).
           03  DRQ-STR.
               05 DRQ-ARNUM            PIC ZZZZ9-.
               05 DRQ-BEZ              PIC X(27).
               05 DRQ-MEH              PIC X(4).
               05 DRQ-GEW              PIC Z.ZZZ.ZZ9,99-.
               05 DRQ-BET              PIC ZZ.ZZZ.ZZ9,99-.
       01  DRS-SATZ.
           03  FILLER                  PIC X(4).
           03  DRS-ARTNR               PIC ZZZZ9-.
           03  DRS-ARTBEZ              PIC X(27).
           03  DRS-PREIS               PIC ZZ9,99-.
           03  FILLER                  PIC X.
           03  DRS-RABATT              PIC Z9,99-.
           03  DRS-PROZ                PIC XXX.
           03  DRS-AKTION              PIC ZZ9,99-.
           03  FILLER                  PIC X.
           03  DRS-VON                 PIC X(10).
           03  DRS-BIS                 PIC X(9).
           03  DRS-KZ                  PIC X.
           03  DRS-DRAUFGABE           PIC ZZZZ9,999-.
           03  DRS-MEH1                PIC X(4).
           03  DRS-AB-MENGE            PIC ZZZZ9,999-.
           03  DRS-MEH2                PIC X(4).
           03  DRS-SPEZ-ARTNR          PIC X(15).
       01  DRS1-SATZ.
           03  FILLER                  PIC X(4).
           03  DRS1-PERSNR             PIC ZZZZ9-.
           03  DRS1-BEZ                PIC X(126).
       01  DRT-SATZ.
           03  FILLER                  PIC X(4).
           03  DRT-LFD                 PIC ZZ999-.
           03  DRT-BEZ                 PIC X(15).
           03  DRT-TO                  PIC 999-.
           03  DRT-BEZ1                PIC X(18).
           03  DRT-ZUS                 PIC XXX.
           03  DRT-XX                  PIC X(40).
       01  DRU-SATZ.
           03  FILLER                  PIC X(20).
           03  DRU-TO                  PIC 999-.
           03  DRU-BEZ1                PIC X(18).
           03  DRU-ZUS                 PIC XXX.
           03  DRU-XX                  PIC X(140).
       01  DRV-SATZ.
           03  FILLER                  PIC X(4).
           03  DRV-ARTNR               PIC ZZZZZ9-.
           03  DRV-BEZ                 PIC X(26).
           03  DRV-PREIS               PIC ZZ9,99-.
           03  DRV-VON                 PIC X(10).
           03  DRV-BIS                 PIC X(10).
           03  DRV-GCODE               PIC ZZZZZZ-.
           03  DRV-EAN                 PIC Z(13).
           03  DRV-EC REDEFINES DRV-EAN.
               05 DRV-PCODE            PIC ZZZZZZ.
               05 FILLER               PIC X(7).
       01  DRW-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRW-STR.
               05 DRW-KTONR            PIC ZZ.ZZ9-.
               05 DRW-BEZ              PIC X(70).
               05 FILLER               PIC XX.
               05 DRW-GRP              PIC ZZ.ZZ9-.
       01  DRX-SATZ                    PIC X(480).
       01  DRY-SATZ.
           03  DRY-EAN                 PIC X(31)      OCCURS 62.
       01  DRZ-SATZ.
           03  DRZ-NR                  PIC ZZZZ9.
           03  DRZ-TA                  PIC X.
           03  DRZ-GRP                 PIC ZZZZ9.
           03  DRZ-TB                  PIC X.
           03  DRZ-MCODE               PIC X(15).
           03  DRZ-TC                  PIC X.
           03  DRZ-TELE                PIC X(15).
           03  DRZ-TD                  PIC X.
           03  DRZ-BEZ                 PIC X(110).
      ******************************************************************
       FD  EXCELIMP                    LABEL RECORD OMITTED.
       01  EXI-SATZ                    PIC X(120).
      ******************************************************************
       FD  EXCELDRU                    LABEL RECORD OMITTED.
       01  EXA-SATZ.
           03  EXA-NUM                 PIC ZZZZ9.
           03  FILLER                  PIC X.
           03  EXA-BEZ                 PIC X(20).
           03  FILLER                  PIC X.
           03  EXA-ZUSBEZ              PIC X(20).
           03  FILLER                  PIC X.
           03  EXA-EAN                 PIC 9(13).
           03  FILLER                  PIC X.
           03  EXA-BESTNR              PIC 9(13).
       01  EX-SATZ.
           03  EX-NUM                  PIC ZZZZ9.
           03  EX-KAL                  PIC 999.
           03  FILLER                  PIC X.
           03  EX-BEZ                  PIC X(20).
           03  FILLER                  PIC X.
           03  EX-ZUSBEZ               PIC X(20).
           03  FILLER                  PIC X.
           03  EX-KURZBEZ              PIC X(10).
           03  FILLER                  PIC X.
           03  EX-EAN                  PIC ZZZZZZ.
           03  FILLER                  PIC X.
           03  EX-BESTNR               PIC Z(13).
           03  FILLER                  PIC X.
           03  EX-ERLGRP               PIC ZZZZ.
           03  FILLER                  PIC X.
           03  EX-ZUSCHLAG             PIC 9.
           03  FILLER                  PIC X.
           03  EX-MEH                  PIC X(3).
           03  FILLER                  PIC X.
           03  EX-UST                  PIC 99.
           03  FILLER                  PIC X.
           03  EX-NK                   PIC 99.
           03  FILLER                  PIC X.
           03  EX-VP-ART               PIC 99.
           03  FILLER                  PIC X.
           03  EX-GEWICHT              PIC ZZZZ9.
           03  FILLER                  PIC X.
           03  EX-PREIS                PIC ZZ9,99.
           03  FILLER                  PIC X.
           03  EX-VERNR                PIC ZZZZZ9.
           03  FILLER                  PIC X.
           03  EX-ANZEH                PIC Z9.
           03  FILLER                  PIC X.
           03  EX-GEW                  PIC Z9,99.
           03  FILLER                  PIC X.
           03  EX-BGEW                 PIC Z9,99.
           03  FILLER                  PIC X.
           03  EX-SCAN                 PIC Z9.
           03  FILLER                  PIC X.
           03  EX-STATKZ               PIC ZZZZ9.
           03  FILLER                  PIC X.
           03  EX-OEPZ                 PIC Z9.
           03  FILLER                  PIC X.
           03  EX-LGB                  PIC 9.
           03  FILLER                  PIC X.
           03  EX-Z                    PIC X(64).
      *------------------------------------------> Ausgabe Preisliste <-
       01  EXP-SATZ.
           03  EXP-ARTNR               PIC ZZZZ9.
           03  FILLER                  PIC XX.
           03  EXP-ARBEZ               PIC X(50).
           03  EXP-PREIS               PIC ZZ9,99.
      *************************************************** Sortierdatei *
       FD  SORTIER      external       LABEL RECORD STANDARD.
       01  SR-SATZ.
           03  SR-KEY.
               05 SR-NR                PIC 9(3)    COMP.
               05 SR-LFD               PIC 9(3)V9  COMP.
           03  SR-AKEY.
               05 SR-KUNR              PIC 9(5)    COMP.
           03  SR-KBEZ                 PIC X(4).
           03  SR-BEZ                  PIC X(49).
           03  SR-FILNR                PIC Z.ZZ9-.
           03  SR-GRUPPE               PIC X(6).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WS-REG.
           03  WZ-BREIT                PIC 999       COMP.
           03  WZ-GROSS                PIC 9(4).
           03  WH-DRUNAM.
               05 WH-FAKT1             PIC X(4).
               05 WH-FAKT2             PIC 99.
               05 WH-FAKT3             PIC X(6).
           03  WN-EXCEL                PIC X(25).
           03  WT-PRBEZ                PIC X(80).
           03  WM-EXC                  PIC 9.
           03  WM-SIZE                 PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WM-OPEN                 PIC 9         COMP  VALUE ZERO.
           03  WH-PX                   PIC XX     OCCURS 2.
           03  WH-ZEI                  PIC S999V9    COMP.
           03  WS-PKT                  PIC 999.
           03  WZ-SEITE                PIC S9(5)     COMP  VALUE ZERO.
           03  WZ-ZEILEN               PIC S999V9    COMP  VALUE 99.
               88  UEBERTRAG           VALUE 60 THRU 99.
           03  WZ-SCHALT               PIC S999      COMP  VALUE 1.
           03  WS-PK                   PIC S9(7)     COMP.
           03  ZX                      PIC 99        COMP.
           03  WI                      PIC S999      COMP.
           03  WS                      PIC 99        COMP.
           03  WL                      PIC S999      COMP.
           03  WX-LG                   PIC X(4)      COMP-5.
           03  WV-KZ                   PIC 99        COMP.
           03  WM-KZ                   PIC 99        COMP.
           03  WM-KOPF                 PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WH-VAR                  PIC 99        COMP.
           03  WH-SPER                 PIC 99        COMP.
           03  WM-KOP                  PIC 99        COMP.
           03  VDU-AB                  PIC 9(4)      COMP.
           03  WH-EAN1                 PIC 9(13)   VALUE ZERO.
           03  WC-EAN                  PIC 99        COMP.
           03  WD-SEITE                PIC ZZ9-.
           03  WD-SNR                  PIC 99.
           03  WD-KTO                  PIC ZZZZ9-  VALUE ZERO.
           03  WH-ANZ-VPEH             PIC 999     VALUE ZERO.
           03  WD-ARTNR                PIC ZZZZ9.
           03  WD-NR                   PIC ZZ.ZZ9.
           03  WD-PREIS                PIC Z(4)9,99.
           03  WD-TONR                 PIC ZZ9.
           03  WV-NR                   PIC S9(5)    VALUE ZERO.
           03  WH-VNR                  PIC 9(5)     VALUE ZERO.
           03  WH-BNR                  PIC 9(5)     VALUE ZERO.
           03  WM-KUNR                 PIC 9(5)     VALUE ZERO.
           03  WM-TO                   PIC 9(3)     VALUE ZERO.
           03  WM-BEZ                  PIC X(15)    VALUE SPACE.
           03  WM-ZUS                  PIC XXX      VALUE SPACE.
           03  WH-TOURNR               PIC 9(6)V9   VALUE ZERO.
           03  WR-TO REDEFINES WH-TOURNR.
               05  WH-TO               PIC 999.
               05  WH-LFD              PIC 9(3)V9.
           03  WD-PERSNR               PIC ZZZZ9.
           03  WD-EAN                  PIC 999BB9999BB9.
      *    03  WD-BAR                  PIC 999B9999B9.
           03  WD-BAR                  PIC 999B9999.
           03  WK-SEITE                PIC ZZZ9.
           03  WV-OEPZ                 PIC S99      COMP.
           03  WX-NR                   PIC 9(5).
           03  WT-TABL.
               05 WT-KDGRP             PIC 9(5)     COMP  OCCURS 21.
               05 WT-TOUR              PIC 9(3)     COMP  OCCURS 21.
           03  WXK-NAME1               PIC X(35).
           03  WXK-NAME2               PIC X(35).
           03  WXK-NAME3               PIC X(35).
           03  WXK-STR                 PIC X(35).
           03  WXK-PLZL                PIC X(10).
           03  WXK-ORT                 PIC X(35).
           03  WXK-LANDKZ              PIC X(5).
      *---------------------------------------------> Konditionentabelle
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WV-PREIS                PIC 9(6)V99   COMP.
           03  WH-NTTO                 PIC 9(6)V99   COMP.
           03  WH-BONUS                PIC 9(6)V99   COMP.
           03  WH-SKTO                 PIC 9(6)V99   COMP.
           03  WG-GEW                  PIC 9(8)V99   COMP.
           03  WS-GEW                  PIC 9(8)V99   COMP.
           03  WG-BET                  PIC 9(8)V99   COMP.
           03  WS-BET                  PIC 9(8)V99   COMP.
           03  WX-TX                   PIC X(40).
      *----------------------------------> Etiketten-Druckersteuerung <-
       01  WD-ETK.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  WH-MEH                  PIC 99         COMP.
           03  WD-TRENN                PIC 9.
           03  WH-ESC.
               05  FILLER              PIC X               VALUE "".
               05  WH-ST1              PIC 9999   COMP-X   VALUE  0.
               05  WR-ST REDEFINES WH-ST1.
                   07 WH-ST2           PIC X.
                   07 FILLER           PIC X.
           03  WB-BC.
               05  WC-LSL              PIC XXX       OCCURS 3.
               05  WC-CR               PIC X.
               05  WC-BIT11            PIC XXX.
               05  WC-LNG              PIC 9999    COMP-5.
               05  WC-STR.
                   07 WC-X             PIC X    OCCURS 450 INDEXED X2.
           03  WN-MUSTER.                               *> Nadeldrucker
               05 WN-STRI              PIC X(6) OCCURS  69 INDEXED X1.
           03  WL-MUSTER.                               *> Laserdrucker
               05 WL-STRI              PIC XX   OCCURS  72 INDEXED X4.
           03  WS-BAR                  PIC X(31).
      *-----------------------------------------> Etikettenparameter <-
           03  ET-HOE                  PIC 9       COMP.
           03  ET-LIN                  PIC 999     COMP.
       01  WS-BCREG.
      *-----------------------------------------------> Barcode Werte <-
           03  WH-BL           VALUE "0110010010011011110101000110110001
      -         "0101111011101101101110001011000110101100110110110010000
      -         "101011100100111010100001000100100100011101001110010".
               05  WH-LIWERT           OCCURS 10.
                   07  WH-LI           PIC 9 OCCURS 7 INDEXED X3.
               05  WH-REWERT           OCCURS 10.
                   07  WH-RE           PIC 9 OCCURS 7.
           03  WH-ANFANG                   VALUE "001010".
               05 WH-ANF               PIC 9 OCCURS 6.
           03  WH-ENDE                     VALUE "0101".
               05 WH-END               PIC 9 OCCURS 4.
           03  WH-MITTE                    VALUE "01010".
               05 WH-MIT               PIC 9 OCCURS 5.
           03  WX-FF                   PIC X(6)  VALUE X"FFFFFFFFFFFF".
           03  WS-LR                   PIC 99    VALUE 0 COMP-3.
           03  WM-EAN                  PIC 9(8)  VALUE 0.
           03  WT-EAN REDEFINES WM-EAN.
               05  WT-7                PIC 9      OCCURS 8 INDEXED IX.
       COPY TIEXT.CPY.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
      ******************************************************************
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "TIARTIK.DAT " WH-CREG.
       Z.  EXIT.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON KUNDEN.
       A.  CALL "CADECL" USING "TIKUNDEN.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "TIKONST.DAT " WH-CREG.
       DECL-T SECTION.         USE AFTER ERROR PROCEDURE ON S-TOUR.
       A.  CALL "CADECL" USING "TITOSTAM.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON PREISLI.
       A.  CALL "CADECL" USING "TIPREISE.DAT" WH-CREG.
       Z.  EXIT.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           OPEN I-O S-TOUR.
       D.  MOVE "   Stammdatendruck" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Programmwahl"      AT 0524
                   "ƒƒƒƒƒƒƒƒƒƒƒƒ"      AT 0024
               "1 = Artikel"                  AT 0824
               "2 = Kunden"                   AT 0024
               "3 = Preislisten"              AT 0024
               "4 = Touren"                   AT 0024
               "5 = Sonderkonditionen"        AT 0024
               "6 = Plastikkistenstand"       AT 0024
               "7 = Tourliste mit Strichcode" AT 0024
               "8 = Kundenpreisliste"         AT 0024
               "9 = Telefonliste fÅr Excel"   AT 0024
              "10 = Kundenverkaufspreise"     AT 0023
              "11 = Tourenspiegel"            AT 0023
              "12 = Kd. je Stat./Kd-Grp"      AT 0023
              "13 = Bestell-Nr. (Brief)"      AT 0023
               "bitte wÑhlen Sie:" with highlight AT 2235.
       E.  DISPLAY "<ret-leer/esc>= Ende" AT 2301.
           CALL "CAUP" USING "0022532002" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM.
           MOVE WH-NUM TO WH-PG.
           IF WH-PG > 13 GO E.
           MOVE 61 TO WM-SIZE.
           IF WH-PG = 0 SET ESC TO TRUE.
           IF ESC MOVE "TISTAMM 90STAM" TO WT-TX
                  MOVE 49 TO WH-PG GO X.
           IF NOT RET GO E.
       F.  PERFORM DRUSTAMM.
           GO D.
       X.  MOVE WH-CREG TO WL-CREG.
           CLOSE S-TOUR.
       Z.  EXIT PROGRAM.
      ******************************************************************
       DATDREH SECTION.
       A.  CALL "CAUP" USING "04DATDREH" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO A.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ***************************************** Drucker Seitenvorschub *
       PAGE-DRU SECTION.
           WRITE DRA-SATZ BEFORE PAGE.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WZ-ZEILEN.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU = 0 MOVE "E" TO DRA-SATZ(1:)
                         WRITE DRA-SATZ AFTER 0
                         GO C
                    else MOVE x"1B210000" TO DRA-SATZ(1:).
           PERFORM PAGE-DRU.
       C.  MOVE 46 TO WX-PRNO.
           CALL X"91" USING WX-PRSTAT WX-PRNO DRUCKER.
           MOVE 0 TO WM-OPEN WZ-ZEILEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 0 TO WZ-ZEILEN.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
           MOVE 47 TO WX-PRNO.
           CALL X"91" USING WX-PRSTAT WX-PRNO DRUCKER.
       C.  IF WZ-ZEILEN > 0 PERFORM PAGE-DRU.
           MOVE 0 TO WZ-SCHALT.
      *    MOVE X"1B21" TO DRA-SATZ(1:).
      *    MOVE WH-PX(1) TO DRA-SATZ(3:2).
      *    IF WM-DRU = 0 MOVE WE-STG(WX-DR) TO DRA-SATZ GO E.
           MOVE WE-STG(WX-DR) TO DRA-SATZ.
      *D.  WRITE DRA-SATZ BEFORE 0.
      *    IF WF-STATUS = 27 GO D.
      *    MOVE X"1B43" TO DRA-SATZ(1:).
      *    MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       DAT-DREH SECTION.                                                       DAT-DREH SECTION 60.
       A.  MOVE WC-TAG TO WZ-JAHR VDU-JAHR.
           MOVE WC-JAHR  TO WZ-TAG VDU-TAG.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
       Z.  EXIT.
      ******************************************************************
       DRUSTAMM SECTION.
       A.  OPEN I-O PREISLI.
           MOVE "STAMxx.LST" TO WH-DRUNAM.
           MOVE WK-BS TO WH-FAKT2.
           MOVE 0 TO WH-VAR WH-SPER.
           CALL "CAUP" USING "0710100760000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           EVALUATE WH-PG
               WHEN 1 DISPLAY " Artikeldruck " with highlight AT VDU-LP
               WHEN 2 DISPLAY " Kundendruck " with highlight AT VDU-LP
               WHEN 3 DISPLAY " Preislistendruck " with highlight
                          AT VDU-LP
               WHEN 4 DISPLAY " Tourendruck " with highlight AT VDU-LP
               WHEN 5 DISPLAY " Sonderkonditionen " with highlight
                          AT VDU-LP
               WHEN 6 DISPLAY " Plastikkistenau·enstandsliste"
                          with highlight AT VDU-LP
               WHEN 7 DISPLAY " Tourenstrichcode " with highlight
                          AT VDU-LP
               WHEN 8 DISPLAY " Kundenpreisliste " with highlight
                          AT VDU-LP
               WHEN 9 DISPLAY " Telefonliste " with highlight AT VDU-LP
               WHEN 10 DISPLAY " Kundenverkaufspreise"
                                               with highlight AT VDU-LP
               WHEN 11 DISPLAY " Tourenspiegel" with highlight
                           AT VDU-LP
                       PERFORM T-SPIEGEL GO X
               WHEN 12 DISPLAY " Stat./Kd.Grp. " with highlight
                           AT VDU-LP
                       PERFORM STATGRP GO X
               WHEN 13 DISPLAY " Kd.-Bestellnr." with highlight
                           AT VDU-LP.
       B.  EVALUATE WH-PG
               WHEN 1 DISPLAY "Artikelnummer" AT 2301
               WHEN 2 DISPLAY "Kundennummer"  AT 2301
               WHEN 3 DISPLAY "Preislistennummer" AT 2301
               WHEN 4 DISPLAY "Tournummer"    AT 2301
               WHEN 5 DISPLAY "Kundennummer"  AT 2301
               WHEN 6 DISPLAY "Kundennummer"  AT 2301
               WHEN 7 DISPLAY "Tournummer"    AT 2301
               WHEN 8 DISPLAY "Kundenpreisliste" AT 2301
               WHEN 9 DISPLAY "Telefonliste " AT 2301
               WHEN 10 DISPLAY "Kundenverkaufspreise" AT 2301
               WHEN 11 DISPLAY "Tourenspiegel" AT 2301
               WHEN 13 DISPLAY "TBestell-Nr.Liste" AT 2301.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           IF WH-PG = 8 or 13
                        MOVE 0 TO WH-VAR
                        PERFORM KUNDENR
                        IF ESC GO X else GO H.
           IF WH-PG = 10 PERFORM ZEITRAUM.
               IF ESC GO X.
       D.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 140 VDU-ECK GIVING VDU-LP.
           IF WH-PG = 3 or WH-PG = 8;
               IF WH-VAR = 1
                  DISPLAY " ohne Subart. " with highlight AT VDU-LP
                  DISPLAY "< >= mit Subartikel" AT 2401
               else DISPLAY " mit Subart. " with highlight AT VDU-LP
                  DISPLAY "< >= ohne Subartikel" AT 2401.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "von Nummer:         bis Nummer:" AT VDU-LP.
           DISPLAY "<ret-leer>= ab Anfang, <esc/Ende>= Ende" AT 2301.
           IF WH-PG = 2 DISPLAY ", <Bild  >= nur Sparkd." AT 0000
               DISPLAY "<Einfg>= Excel-Import" AT 2401.
           MOVE 0 TO WH-WERT WH-MOD.
           CALL "CAUP" USING "1003156006" WH-CREG.
           IF ENDE SET ESC TO TRUE.
           IF EINF PERFORM KUND-IMP GO D.
           IF AUF; IF WH-VAR = 0 MOVE 1 TO WH-VAR
                   else MOVE 0 TO WH-VAR.
           IF BAB DISPLAY " Sparkunden 653 + 654" with highlight
                      AT VDU-LP
                  MOVE 9 TO WH-MOD
                  MOVE 99999 TO WH-BNR
                  GO F.
           IF ESC GO X.
           IF NOT RET GO D.
           IF WH-WERT = 0 MOVE 1 TO WH-WERT.
           MOVE WH-WERT TO WH-VNR WD-NR.
           DISPLAY WD-NR with highlight AT VDU-LP.
       E.  MOVE 0 TO WH-WERT.
           IF WH-PG = 10; IF WOLI GO D
                          else MOVE WH-VNR TO WH-BNR WD-NR
                               ADD 0335 VDU-ECK GIVING VDU-LP
                               DISPLAY WD-NR with highlight AT VDU-LP
                               GO H.
           DISPLAY "<ret-leer>= bis Ende, <>= zurÅck" AT 2301.
           CALL "CAUP" USING "1003356006" WH-CREG.
           IF WOLI GO D.
           IF NOT RET GO E.
           IF WH-WERT = 0 MOVE 99999 TO WH-WERT.
           MOVE WH-WERT TO WH-BNR WD-NR.
           IF WH-BNR < WH-VNR GO B.
           DISPLAY WD-NR with highlight AT VDU-LP.
           IF WH-PG not = 1 GO H.
       F.  DISPLAY "<ret>= hoch, < >= quer, < >= Moberf, < >= Ean-Best-C
      -        "ode" AT 2301.
           DISPLAY "<Bild- >= nach int. Best.Nr." at 2401.
           DISPLAY "<Strg+Pos1> = EXC-SUB.TXT" at 2501.
           ADD 0403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Listung: " AT VDU-LP.
           CALL "CAUP" USING "1004120000" WH-CREG.
           IF WOLI GO D.
           IF ESC GO X.
           IF SAPO PERFORM EXC-KALIBER GO F.
           IF BAB MOVE 6 TO WH-MOD
                  GO H.
           IF AUF  MOVE 5 TO WH-MOD
                   MOVE "MOBERF.TXT" TO WN-EXCEL
                   ADD 030 VDU-ECK GIVING VDU-LP
                   DISPLAY WN-EXCEL with highlight AT VDU-LP
                   GO H.
           IF WORE MOVE 2 TO WH-MOD
                   MOVE "ARTEAN.TXT" TO WN-EXCEL
                   ADD 030 VDU-ECK GIVING VDU-LP
                   DISPLAY WN-EXCEL with highlight AT VDU-LP
                   DISPLAY "hoch" with highlight AT VDU-LP
                   GO H.
           IF RET MOVE 1 TO WH-MOD
                  MOVE 61 TO WM-SIZE
                  DISPLAY "hoch" with highlight AT VDU-LP
             else MOVE 0 TO WH-MOD
                  MOVE 40 TO WM-SIZE
                  DISPLAY "quer" with highlight AT VDU-LP.
           IF not RET and not AB GO F.
       H.  DISPLAY "<ret>= Start, <esc/Ende>= Abbruch < >" AT 2301.
           DISPLAY "<Strg+Ende>= mit SOFTLINE-Ausgabe" AT 2501.
           IF WH-PG = 3 DISPLAY "<#> statt <ret>= nur Kîpfe" AT 2401.
           CALL "CAUP" USING "0023360000" WH-CREG.
           MOVE 0 TO WM-KOPF WM-EXC.
           IF KIST and WH-PG = 3 MOVE 1 TO WM-KOPF
               SET RET TO TRUE.
           IF SEPO SET RET TO TRUE
               MOVE 1 TO WM-EXC.
           IF ESC or ENDE GO X.
           IF NOT RET GO H.
           IF WH-PG = 13 MOVE 1 TO WH-VAR.
       I.  MOVE 0 TO WZ-SEITE WZ-ZEILEN WM-OPEN.
           EVALUATE WH-PG
               WHEN 1 PERFORM ARTIK
               WHEN 2 PERFORM KUNDE
               WHEN 3 PERFORM PREIS
               WHEN 4 PERFORM TOUR
               WHEN 5 PERFORM PREIS
               WHEN 6 PERFORM PKBEW
               WHEN 7 PERFORM ETIKDRU
               WHEN 8 PERFORM KUPREIS
               WHEN 9 PERFORM EXTEL
               WHEN 10 PERFORM PREIS
               WHEN 13 PERFORM KUPREIS.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE PREISLI.
       Z.  EXIT.
      ************************************ Excel-Ausgabe KALIBER-Datei *
       EXC-KALIBER SECTION.
       A.  MOVE 0 TO WM-OPEN.
           OPEN INPUT KALIBER.
           INITIALIZE KA-SATZ.
           START KALIBER KEY not < KA-KEY INVALID STOP RUN.
       C.  READ KALIBER NEXT IGNORE LOCK AT END GO X.
           IF KA-ARNUM > 9999 GO X.
           IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               MOVE "Q:EXC-SUB.TXT" TO WT-TX
               MOVE "Art-Nr.;Z-Bez;R-Bez;V-Gew;B-Gew;Anzahl;B-Num;"
                   to WT-ADR
               INSPECT WT-ADR REPLACING ALL ";" BY "|"
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           DISPLAY KA-ARNUM AT 2320.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 40 continue
               IF KA-BNUM(WI) not = 0
               PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 COMPUTE WH-WERT = KA-ARNUM * 100 + WI
                   WHEN  2 MOVE KA-ZUSBEZ(WI) TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  3 MOVE KA-RESBEZ(WI) TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  4 MOVE KA-GEW(WI) TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN  5 MOVE KA-BGEW(WI) TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN  6 MOVE KA-ANZEH(WI) TO WH-WERT
                   WHEN  7 MOVE KA-BNUM(WI) TO WH-WERT
                   WHEN  8 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           GO C.
       X.  CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
           CLOSE KALIBER.
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
      ********************************************* Import Kundendaten *
       KUND-IMP SECTION.
       A.  CALL "CAUP" USING "271320034000009" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE "KU-IMP.TXT" TO WN-EXCEL.
           DISPLAY " Import Stammdaten " AT VDU-LP.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT VDU-LP.
           CALL "CAUP" USING "1002330000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO C.
           OPEN I-O EXCELIMP.
           IF WF-STATUS not = "00" ADD 203 VDU-ECK GIVING VDU-LP
               DISPLAY "Keine Daten vorhanden < >" AT VDU-LP
               CALL "CAUP" USING "0023230000" WH-CREG
               GO Z.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunden-Nr.: " AT VDU-LP.
           READ EXCELIMP AT END GO X.
       E.  READ EXCELIMP AT END GO X.
           MOVE EXI-SATZ(1:8) TO WH-MCODE.
           PERFORM HOL-WERT.
           MOVE WH-NUM TO KU-KTONR WD-KTO.
           MOVE "K" TO KU-SA.
       G.  READ KUNDEN INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE EXI-SATZ(9:8) TO WH-MCODE.
           PERFORM HOL-WERT.
           MOVE WH-NUM TO KU-AWKZ-A.
           MOVE EXI-SATZ(17:8) TO WH-MCODE.
           PERFORM HOL-WERT.
           MOVE WH-NUM TO KU-AWKZ-B.
           REWRITE KU-SATZ.
           ADD 215 VDU-ECK GIVING VDU-LP.
           DISPLAY KU-KTONR with highlight AT VDU-LP.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE EXCELDRU.
       Z.  EXIT.
      **************************************************** Werte holen *
       HOL-WERT SECTION.
       A.  MOVE 13 TO WI.
           MOVE 0 TO WH-NUM.
           PERFORM VARYING WX FROM 13 BY -1 UNTIL WX = 0
               IF WH-MCODE(WX:1) not = SPACE
                   MOVE WH-MCODE(WX:1) TO WH-NUM(WI:1)
                   ADD -1 TO WI.
       Z.  EXIT.
      ******************************************************* Zeitraum *
       ZEITRAUM SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "ab Liefertag: " AT VDU-LP.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 217 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
       C.  CALL "CAUP" USING "1102176006" WH-CREG.
           IF ESC GO Z.
           IF not RET  GO C.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight foreground-color 6 AT VDU-LP
           MOVE WX-DATUM TO WV-DATUM.
       Z.  EXIT.
      *************************************************** Kundennummer *
       KUNDENR SECTION.
       A.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunden-Nr.:        " AT VDU-LP.
           DISPLAY "<ret>= Kunden-Nr., alpha+<ret>= suchen, < >= extern,
      -        " < >= "
              AT 2301.
           ADD 130 VDU-ECK GIVING VDU-LP.
           IF WH-VAR = 0 DISPLAY " intern " with highlight
               foreground-color 3 AT VDU-LP
               DISPLAY "extern" with highlight foreground-color 6
                    AT 2346
           else DISPLAY " extern " with highlight foreground-color 6
                   AT VDU-LP
               DISPLAY "intern" with highlight foreground-color 3
                   AT 2346.
           ADD 142 VDU-ECK GIVING VDU-LP.
           IF WH-SPER = 0 DISPLAY " ohne Sperre " with highlight
               foreground-color 4 AT VDU-LP
               DISPLAY "mit Sperre" with highlight foreground-color 5
                    AT 2359
           else DISPLAY " mit Sperre " with highlight foreground-color 5
                   AT VDU-LP
               DISPLAY "ohne Sperre" with highlight foreground-color 4
                   AT 2359.
           DISPLAY "<Strg+Ende>= mit SOFTLINE-Ausgabe" AT 2501.
           CALL "CAUP" USING "1003156006" WH-CREG.
           IF ESC GO Z.
           IF SEPO PERFORM EXC-BESTELL
                   GO A.
           IF AUF; IF WH-VAR = 0 MOVE 1 TO WH-VAR
                   else MOVE 0 TO WH-VAR.
           IF AB; IF WH-SPER = 0 MOVE 1 TO WH-SPER
                  else MOVE 0 TO WH-SPER.
           IF NOT RET GO A.
           IF WH-NUM = 0 AND WH-WERT NOT = SPACE
               CALL "TIANZEIG" USING "01DEBANZ" WH-CREG
               CANCEL "TIANZEIG".
           IF WH-WERT = 0 GO A.
           MOVE WH-WERT TO KU-KTONR WD-NR.
           DISPLAY WD-NR with highlight AT VDU-LP.
           MOVE "K" TO KU-SA.
       C.  READ KUNDEN IGNORE LOCK INVALID GO A.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE KU-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ(1:55) AT VDU-LP.
           MOVE KU-PREISE TO WH-VNR WH-BNR.
       Z.  EXIT.
      ************************************** BestellnummernverknÅpfung *
       EXC-BESTELL SECTION.
       A.  OPEN INPUT BESTNUM.
           MOVE 0 TO BN-BNUM.
           MOVE SPACE TO WN-EXCEL.
           START BESTNUM KEY = BN-AKEY INVALID
               START BESTNUM KEY > BN-AKEY INVALID GO I.
       C.  READ BESTNUM NEXT AT END GO I.
           MOVE BN-NUM TO AR-NUM.
       G.  ADD 509 VDU-ECK GIVING VDU-LP.
           DISPLAY BN-BNUM AT VDU-LP " " BN-NUM.
           IF WN-EXCEL = SPACE MOVE "EXC-BN.TXT" TO WN-EXCEL
               MOVE "Q:EXC-BN.TXT" TO WT-TX
               MOVE "Bestell-Nr.;Art-Nr;" TO WT-ADR
               INSPECT WT-ADR REPLACING ALL ";" BY "|"
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 3
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 COMPUTE WH-EAN =
                              9001456000000 + (BN-BNUM * 10)
                           PERFORM PRFZ
                           MOVE WH-EAN TO WT-TX
                           MOVE "E" TO WH-X
                   WHEN  2 MOVE BN-NUM TO WH-WERT
                   WHEN  3 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           GO C.
       I.  CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
           CLOSE BESTNUM.
      *--------------------------------------> Ausgabe EAN zu Artikel <-
           MOVE 0 TO BN-NUM.
           OPEN INPUT KNOTEN.
           MOVE SPACE TO WN-EXCEL.
           START KNOTEN KEY = KN-AKEY INVALID
               START KNOTEN KEY > KN-EAN INVALID GO X.
       K.  READ KNOTEN NEXT AT END GO X.
           MOVE KN-NUM TO AR-NUM.
       M.  ADD 509 VDU-ECK GIVING VDU-LP.
           DISPLAY KN-EAN AT VDU-LP " " KN-NUM.
           IF WN-EXCEL = SPACE MOVE "EXC-KN.TXT" TO WN-EXCEL
               MOVE "Q:EXC-KN.TXT" TO WT-TX
               MOVE "KD-Ean-Nr.;Art-Nr;" TO WT-ADR
               INSPECT WT-ADR REPLACING ALL ";" BY "|"
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 3
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 MOVE KN-EAN TO WT-TX
                           MOVE "E" TO WH-X
                   WHEN  2 MOVE KN-NUM TO WH-WERT
                   WHEN  3 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           GO K.
       X.  CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
           CLOSE KNOTEN.
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
       KOPFDRUCK SECTION.
       A.  IF WZ-ZEILEN > WM-SIZE PERFORM PAGE-DRU.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           IF WM-DRU = 0;
               EVALUATE WH-PG
                   WHEN 7 MOVE 9 TO WX-DR
                   WHEN 2 MOVE 2 TO WX-DR
                          MOVE 41 TO WM-SIZE
                   WHEN OTHER MOVE 5 TO WX-DR.
           PERFORM BEG-DRU.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           IF WM-DRU = 1 MOVE 2 TO WZ-SCHALT
                    else MOVE 0 TO WZ-SCHALT.
           EVALUATE WH-PG
               WHEN 1 MOVE "Artikeldruck" TO DRA-STR
               WHEN 2 MOVE "Kundendruck" TO DRA-STR
               WHEN 3 MOVE "Preislistendruck fÅr: " TO DRA-STR
                      MOVE PR-NR TO WD-NR
                      MOVE WD-NR TO DRA-STR(23:6)
                      MOVE WT-PRBEZ TO DRA-STR(30:30)
                      MOVE "Datum:" TO DRA-STR(60:)
                      MOVE VDU-DATUM TO DRA-STR(67:) GO E
               WHEN 4 GO H
               WHEN 5 MOVE "Sonderkonditionendruck" TO DRA-STR
                      MOVE PR-NR TO WD-NR
                      MOVE WD-NR TO DRA-STR(33:6)
                      MOVE WT-PRBEZ TO DRA-STR(40:30)
                      MOVE "Datum:" TO DRA-STR(60:)
                      MOVE VDU-DATUM TO DRA-STR(67:) GO E
               WHEN 6 MOVE "Plastikkistenau·enstandliste" TO DRA-STR
               WHEN 7 GO K
               WHEN 10 MOVE "Kundenverkaufspreisliste per:" TO DRA-STR
                       MOVE WV-DATUM TO WC-DATUM
                       PERFORM DATDREH
                       MOVE VDU-DATUM TO DRA-STR(31:)
                       GO C.
       B.  MOVE "Datum:" TO DRA-DATTEXT.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-DATUM.
       C.  ADD 1 TO WZ-SEITE.
           IF WH-PG NOT = 7 AND WH-PG NOT = 10
                MOVE "Seite:" TO DRA-SEITE1
                MOVE WZ-SEITE TO DRA-SEITENNR1
           else MOVE "Seite:" TO DRA-SEITE
                MOVE WZ-SEITE TO DRA-SEITENNR.
       E.  PERFORM DRUCK.
           IF WH-PG = 3 MOVE ALL "ƒ" TO DRA-STR
               else MOVE ALL "ƒ" TO DRA-SATZ(5:)
           PERFORM DRUCK.
           IF WH-PG = 1
           MOVE "Artnr Artikelbezeichnung   Zusatzbezeichnung    Kurzbez
      -         ".   Erl Vpeh Stat Ust EAN-Nr Zu NK  Code ôPZ K-Gew Anz
      -         " Preis" TO DRA-SATZ(5:) GO F.
           IF WH-PG = 2
           MOVE "Kunr. Bezeichnung
      -         "
      -         "      Kurzbezeichnung" TO DRA-STR
                PERFORM DRUCK
           MOVE "                           Li. Reart Pk-Ver. D.Kz  ean-
      -         "BBN   Vers. Rab. Bonus Readr. Liefadr. Fibuad. Stat
      -         " Art.1 Art.2 Art.3" TO DRA-SATZ(5:) GO F.
           IF WH-PG = 3
           MOVE " Artnr. Artikelbezeichnung     Eur-Preis     Aktion  v
      -         "o n     b i s G-Rab" TO DRA-STR GO F.
           IF WH-PG = 4
               MOVE "Tour    Kunr. Bezeichnung" TO DRA-STR GO F.
           IF WH-PG = 5
           MOVE "Artnr Artikelbezeichnung      Eur-Preis          Aktio
      -      "n  von      bis  G-Rab Draufgabe      ab Menge     spez.Ar
      -      "tnr." TO DRA-STR GO F.
           IF WH-PG = 6
           MOVE "Ktonr. Bezeichnung
      -      "
      -      "     Kistensaldo" TO DRA-STR.
           IF WH-PG = 8
           MOVE " Artnr Artikelbezeichnung             Preis  Rabatt Net
      -         "ttopreis" TO DRA-STR.
           IF WH-PG = 10
           MOVE " Artnr Artikelbezeichnung         Preis   v o n     b i
      -         " s   P-code G-code" TO DRA-STR GO F.
       F.  PERFORM DRUCK.
           IF WH-PG = 3 MOVE ALL "ƒ" TO DRA-STR
               else MOVE ALL "ƒ" TO DRA-SATZ(5:).
           IF WH-PG = 10 PERFORM DRUCK
                         MOVE WD-PERSNR TO DRV-ARTNR
                         MOVE WT-TX TO DRV-BEZ
                         MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           GO Z.
       H.  MOVE 3 TO WZ-SCHALT.
           MOVE TS-NR TO DRT-TO.
           MOVE TS-KURZBEZ TO DRT-BEZ1.
           MOVE TS-ALKEY TO DRT-ZUS.
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE WM-BEZ TO DRT-BEZ1.
           PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
           GO Z.
       K.  IF WM-DRU = 0 MOVE 0 TO WZ-SCHALT
               IF WM-KOP = 0 MOVE 1 TO WM-KOP
                 MOVE "(s1p12h1b14v0s4102T&a6LFahrer:" TO DRU-SATZ(5:)
                 MOVE "Lkw-Nr.:" TO DRU-SATZ(115:)
                 PERFORM DRUCK
               end-if
               MOVE "(s1p12h1b22v0s4102T&a6L" TO DRU-XX
               PERFORM DRUCK.
           IF WM-DRU = 1 MOVE 1 TO WZ-SCHALT
               MOVE X"1B5701" TO DRT-SATZ(18:3).
           MOVE WM-TO TO DRU-TO.
           MOVE WM-BEZ TO DRU-BEZ1.
           MOVE WM-ZUS TO DRU-ZUS.
           IF WM-DRU = 1 MOVE X"1B5700" TO DRU-XX.
           IF WM-DRU = 0 MOVE "(s0p12h0b20v0s4102T&a6L" TO DRU-XX.
           PERFORM DRUCK.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       OPEN-XLS SECTION.
       A.  MOVE "\PREISE\KD00000.TXT" TO WN-EXCEL.
           MOVE KU-KTONR TO WX-NR.
           MOVE WX-NR TO WN-EXCEL(11:5).
           OPEN OUTPUT EXCELDRU.
           MOVE " A-Nr. Artikelbezeichnung                           Pre
      -        "is je Kg" TO EXP-SATZ.
           WRITE EXP-SATZ.
           MOVE SPACE TO EXP-SATZ.
       Z.  EXIT.
      ******************************************************************
       PREIKOPF SECTION.
       A.  IF WZ-ZEILEN > 60 PERFORM PAGE-DRU.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE "PREISL00.LST" TO WH-DRUNAM.
           IF WH-PG = 13 MOVE "PRBESTxx.LST" TO WH-DRUNAM.
           MOVE WK-BS(1:2) TO WH-DRUNAM(7:2).
           IF WM-DRU = 0 MOVE 5 TO WX-DR.
           PERFORM BEG-DRU.
           PERFORM OPEN-XLS.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           IF WM-DRU = 1 MOVE 2 TO WZ-SCHALT
                    else MOVE 0 TO WZ-SCHALT.
           MOVE "Preisliste fÅr: " TO DRK-STR.
           IF WH-PG not = 0
               MOVE PR-NR TO WD-NR
               MOVE WD-NR TO DRK-STR(17:6)
               MOVE WT-TX TO DRK-STR(24:30).
           MOVE "Datum:" TO DRK-STR(57:)
           MOVE VDU-DATUM TO DRK-STR(64:).
           ADD 1 TO WZ-SEITE.
           MOVE "Seite:" TO DRK-STR(73:6).
           MOVE WZ-SEITE TO WK-SEITE.
           MOVE WK-SEITE TO DRK-STR(80:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRK-STR.
           PERFORM DRUCK.
           MOVE " Artnr Artikelbezeichnung...................P
      -         "tt   Bonus Skonto Nettopreis" TO DRK-STR.
           IF WH-PG = 8 MOVE " Artnr.   Artikelbezeichnung
      -        " Eur-Preis " TO DRK-STR.
           IF WH-PG = 13 MOVE " Artnr.   Artikelbezeichnung
      -        "  Eur-Preis   Bestellnummer " TO DRK-STR.
       C.  PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRK-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE KU-KTONR TO WD-NR.
           MOVE WD-NR TO DRK-STR(1:).
           MOVE KU-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           MOVE WT-TX TO DRK-STR(8:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       ARTKO SECTION.
       A.  IF WZ-ZEILEN > WM-SIZE PERFORM PAGE-DRU.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE x"0100" TO WH-PX(1).
           IF WH-MOD = 1 MOVE x"000C" TO WH-PX(2)
                    else MOVE x"0008" TO WH-PX(2).
           IF WM-DRU = 0 MOVE 5 TO WX-DR
              IF WH-MOD = 0 MOVE 2 TO WX-DR
                 MOVE "e(10U&l26a8c1O(s0p12.5h15v0b0s3T"
                     TO WE-STG(9).
           PERFORM BEG-DRU.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           IF WM-DRU = 1 MOVE 2 TO WZ-SCHALT
                    else MOVE 0 TO WZ-SCHALT.
           IF WH-MOD = 1 MOVE "Artikeldaten" TO DRD-STR
                    else MOVE "Artikelstammdaten" TO DRG-STR.
       B.  MOVE "Datum:" TO DRD-STR(30:).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRD-STR(37:).
       C.  ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           IF WH-MOD = 1 MOVE "Seite:" TO DRG-STR(72:)
                MOVE WD-SEITE TO DRG-STR(79:)
                PERFORM DRUCK
                MOVE ALL "ƒ" TO DRG-STR
                PERFORM DRUCK
                MOVE "Artnr Artikelbezeichnung   Kurzbez.   Erl. Vp Ust
      -           "EAN-Nr Zu NK  Code ôP K-Gew Anz" TO DRG-STR
                PERFORM DRUCK
                MOVE ALL "ƒ" TO DRG-STR
           else MOVE "Seite:" TO DRD-STR(113:)
                MOVE WD-SEITE TO DRD-STR(120:)
                PERFORM DRUCK
                MOVE ALL "ƒ" TO DRD-STR
                PERFORM DRUCK
                MOVE "Artnr Artikelbezeichnung   Zusatzbezeichnung    K-
      -            "Gew K-Gew Kurzbez.   Erl Vpeh Stat Ust EAN-Nr Zu NK
      -            " Code ôPZ Anz  Preis" TO DRD-STR
                PERFORM DRUCK
                MOVE ALL "ƒ" TO DRD-STR.
           MOVE 0 TO WR.
       E.  PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       PKBEW SECTION.
       A.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunde:" with highlight AT VDU-LP.
           ADD 7 TO VDU-LP.
           MOVE 0 TO WS-PK.
           MOVE "K" TO KU-SA.
           MOVE WH-VNR TO KU-KTONR.
           START KUNDEN KEY NOT < KU-KEY INVALID GO X.
       C.  READ KUNDEN NEXT IGNORE LOCK AT END GO X.
           IF KU-SA NOT = "K" GO X.
           IF KU-KTONR > WH-BNR GO X.
           PERFORM KOPFDRUCK.
           INSPECT KU-BEZ REPLACING ALL "#" BY SPACE.
           MOVE KU-KTONR TO DRP-NR.
           MOVE KU-KTONR TO WD-KTO.
           DISPLAY WD-KTO AT VDU-LP " " KU-MCODE.
           MOVE KU-BEZ TO DRP-BEZ.
           MOVE KU-EMB-STAND TO DRP-EMB.
           ADD KU-EMB-STAND TO WS-PK.
           MOVE KU-EMB-PSTAND TO DRP-PEMB.
           PERFORM DRUCK.
           GO C.
       X.  MOVE ALL "-" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "S u m m e" TO DRP-BEZ.
           MOVE WS-PK TO DRP-EMB.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       TOUR SECTION.
       A.  ADD 503 VDU-ECK GIVING VDU-LP.
           MOVE SPACE TO WN-EXCEL.
           DISPLAY "Tour:" with highlight AT VDU-LP.
           ADD 6 TO VDU-LP.
           MOVE 0 TO TS-LFD WZ-ZEILEN.
           MOVE -1 TO WV-NR.
           MOVE WH-VNR TO TS-NR WH-TO.
           if wm-exc = 1 go g.
           START S-TOUR KEY NOT < TS-KEY INVALID GO Z.
       B.  READ S-TOUR NEXT IGNORE LOCK AT END GO G.
           IF TS-NR > WH-BNR GO G.
           MOVE TS-NR TO WD-TONR.
           ADD 509 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-TONR AT VDU-LP " " TS-KURZBEZ.
           IF WV-NR = -1 MOVE TS-NR TO WV-NR.
           IF WV-NR NOT = TS-NR MOVE TS-NR TO KU-KTONR WM-TO
               MOVE KU-MCODE TO WM-BEZ.
           PERFORM KOPFDRUCK.
           IF TS-LFD = 0 ADD 533 VDU-ECK GIVING VDU-LP
                DISPLAY TS-KURZBEZ AT VDU-LP " " TS-ALKEY
                GO B.
           MOVE TS-LFD TO DRT-LFD.
           MOVE TS-KURZBEZ TO DRT-BEZ.
           MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
           GO B.
      *------------------------------------->  Ausgabe in Excel-Datei <-
       G.  ADD 509 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with size 50 at vdu-lp.
           OPEN INPUT TOUREN.
           INITIALIZE TO-SATZ.
           MOVE -1 TO WV-NR.
           MOVE WH-VNR TO TO-NR WH-TO.
           START TOUREN KEY NOT < TO-KEY INVALID GO Z.
       I.  READ TOUREN NEXT IGNORE LOCK AT END
                CLOSE TOUREN
                GO X.
           IF TO-KUNR = SPACE MOVE 0 TO TO-KUNR.
           IF TO-LFD = 0 and TO-KURZBEZ = SPACE GO I.
           DIVIDE 2 INTO TO-LFD GIVING WH-NUM REMAINDER WH-NEG.
           IF WH-NEG not = 0 GO I.
           ADD 509 VDU-ECK GIVING VDU-LP.
           DISPLAY TO-NR with highlight AT VDU-LP " " TO-LFD.
           IF WN-EXCEL = SPACE MOVE "EXC-TO.TXT" TO WN-EXCEL WT-TX
               MOVE "Q:EXC-TO.TXT" TO WT-TX
               MOVE "Tour;Folge;Kd-Nr;Bez.;K-Bez;" TO WT-ADR
               INSPECT WT-ADR REPLACING ALL ";" BY "|"
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 MOVE TO-NR TO WH-WERT
                   WHEN  2 MOVE TO-LFD TO WH-WERT
                   WHEN  3 IF TO-LFD not = 0 MOVE TO-KUNR TO WH-WERT
                                        else MOVE 0 TO WH-WERT
                   WHEN  4 MOVE SPACE TO WT-TX
                           IF TO-LFD = 0 MOVE TO-KURZBEZ TO WT-TX
                               MOVE "U" TO WH-X
                           else MOVE "A" TO WH-X
                   WHEN  5 MOVE SPACE TO WT-TX
                           IF TO-LFD = 0 MOVE TO-AKEY TO WT-TX
                                         MOVE "U" TO WH-X
                                    else MOVE "A" TO WH-X
                   WHEN  6 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           GO I.
       X.  PERFORM END-DRU.
           CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       ARTIK SECTION.
       A.  IF WH-MOD = 2 PERFORM BESTART GO Z.
           OPEN INPUT KALIBER.
           OPEN INPUT BESTNUM.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Artikel:" with highlight AT VDU-LP.
           ADD 512 VDU-ECK GIVING VDU-LP.
           MOVE "A" TO AR-ART.
           MOVE WH-VNR TO AR-NUM.
           MOVE "ARTIKEL.TXT" TO WN-EXCEL.
           IF WH-MOD = 5 MOVE "MOBERF.TXT" TO WN-EXCEL.
           OPEN OUTPUT EXCELDRU.
           MOVE "Art.Nr.;Ar-Bez.;Zus-bez.;Kurzbez.;Ean;Best.-Nr.;Erl-Grp
      -         ".;Zus.;Meh;Ust;TKF;Nk-Kz;Vp-Art;Gew.;" TO EX-SATZ.
           MOVE "Preis;Vernr;Anz-Eh;Min.Gew.;Max.Gew.;Scan;Stat.;Oepz;La
      -         "g.;Folge;Min-Eh;Pr-Kz;Text;erl.Preis;I-Stat;AmaGrp;B-Br
      -         ";B-Grp;OGT;AMA" TO EX-SATZ(93:).
           IF WH-MOD not = 5
               INSPECT EX-SATZ REPLACING ALL ";" BY x"09"
               WRITE EX-SATZ.
           MOVE "Q:EXC-ART.TXT" TO WT-TX.
           MOVE "Art.Nr.;Ar-Bez.;Zus-Bez.;Ean;Erl-Grp;AZus;VpKz;Ust;TKF;
      -         "Nk-Kz;Vp-Art;Gew.;Preis;KonjSt;" TO WT-ADR
           MOVE "Anz-Eh;Min.Gew;Max.Gew;Scan;Stat;Oepz;Folge;Pr-Kz;Text;
      -         "Best-Nr;Lg-B;I-Stat;AmaStat;B-Grp;OGT;AMA;BIO;non-DHL;T
      -         "itz-Art;" TO WT-ADR(87:).
           CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           IF WH-MOD not = 6 GO E.
           MOVE 0 TO BN-BNUM.
           START BESTNUM KEY = BN-AKEY INVALID
               START BESTNUM KEY > BN-AKEY INVALID GO X.
       C.  IF WH-MOD not = 6 GO G.
           READ BESTNUM NEXT AT END GO X.
           MOVE BN-NUM TO AR-NUM.
           IF BN-NUM not = AR-NUM PERFORM KALIB-ART GO C.
       E.  START ARTIKEL KEY NOT < AR-KEY INVALID GO X.
       G.  READ ARTIKEL NEXT IGNORE LOCK AT END GO X.
           IF WH-MOD = 6 and AR-NUM > 9849 GO G.
           IF AR-ART NOT = "A" GO X.
           IF AR-NUM > WH-BNR GO X.
           IF WH-MOD = 5 MOVE SPACE TO EX-SATZ
               MOVE AR-NUM TO EXA-NUM
               MOVE AR-BEZ TO EXA-BEZ
               MOVE AR-ZUSBEZ TO EXA-ZUSBEZ
               MOVE AR-EAN TO EXA-EAN
               INSPECT EXA-EAN(1:7) REPLACING ALL "0" BY " "
               WRITE EXA-SATZ
               end-write
               GO C.
      *-----------------------------------------------------------------
           PERFORM ARTKO.
           IF WH-MOD = 0 MOVE 123 TO WZ-BREIT
                    else MOVE 82 TO WZ-BREIT.
           PERFORM RASTRI.
           MOVE AR-NUM TO DRD-ARTNR WD-ARTNR.
           DISPLAY WD-ARTNR AT VDU-LP " " AR-BEZ.
           MOVE AR-BEZ TO DRD-ARTBEZ.
           MOVE AR-KURZBEZ TO WT-TX.
           CALL "CAUP" USING "21CONV10" WH-CREG.
           MOVE WT-TX TO WH-MCODE.
           IF WH-MOD = 0 or 6 MOVE AR-ZUSBEZ TO DRD-ZUSBEZ
               MOVE WT-TX TO DRD-KURZBEZ
               MOVE AR-ERLGRP TO DRD-ERLKTO
               MOVE AR-VP-ART TO DRD-VP
               MOVE AR-STATKZ TO DRD-STAT
               MOVE AR-UST TO DRD-MWST
               MOVE AR-EAN TO DRD-EAN
               MOVE AR-ZUSCHLAG TO DRD-ZUSCHL
               MOVE AR-NK TO DRD-NK
               MOVE AR-VERNR TO DRD-VERNR
               MOVE AR-OEPZ TO DRD-OEPZ
               IF AR-GEW not = 0 MOVE AR-GEW TO DRD-KGEW
                   IF DRD-KGEW(2:1) = " " MOVE "o" TO DRD-KGEW(2:1)
                   end-if
               end-if
               IF AR-BGEW not = 0 MOVE AR-BGEW TO DRD-BGEW
                   IF DRD-BGEW(2:1) = " " MOVE "o" TO DRD-BGEW(2:1)
                   end-if
               end-if
               MOVE AR-ANZEH TO DRD-ANZ
               MOVE AR-PREIS TO DRD-PREIS
               MOVE AR-NUM TO KA-ARNUM
               READ KALIBER IGNORE LOCK INVALID INITIALIZE KA-SATZ
               end-read
           else MOVE WT-TX TO DRG-KURZBEZ
               MOVE AR-ERLGRP TO DRG-ERLKTO
               MOVE AR-VP-ART TO DRG-VP
               MOVE AR-UST TO DRG-MWST
               MOVE AR-EAN TO DRG-EAN
               MOVE AR-ZUSCHLAG TO DRG-ZUSCHL
               MOVE AR-NK TO DRG-NK
               MOVE AR-VERNR TO DRG-VERNR
               MOVE AR-OEPZ TO DRG-OEPZ
               MOVE AR-GEW TO DRG-KGEW
               MOVE AR-ANZEH TO DRG-ANZ.
           IF AR-NUM < 100 GO G.
           IF AR-BEZ = "FEHLER" GO G.
           MOVE 10 TO WX-LG.
           CALL "CBL_TOUPPER" USING WH-MCODE by value WX-LG.
           IF WH-MCODE(1:5) = "FREI " GO G.
      *------------------------------------->  Ausgabe in Excel-Datei <-
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 39
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 MOVE AR-NUM TO WH-WERT
                   WHEN  2 MOVE AR-BEZ TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  3 MOVE AR-ZUSBEZ TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  4 MOVE WH-MCODE TO WT-TX
                           MOVE "q" TO WH-X
                   WHEN  5 MOVE AR-EAN TO WT-TX(1:)
                           MOVE "E" TO WH-X
                   WHEN  6 MOVE AR-ERLGRP TO WH-WERT
                   WHEN  7 MOVE AR-ZUSCHLAG TO WH-WERT
                   WHEN  8 DIVIDE 10 INTO AR-NK GIVING WD-TRENN
                           MOVE WD-TRENN TO WH-WERT
                   WHEN  9 MOVE AR-UST TO WH-WERT
                   WHEN 10 MOVE AR-TKF-KZ TO WH-WERT
                   WHEN 11 MOVE AR-NK TO WD-TRENN
                           MOVE WD-TRENN TO WH-WERT
                   WHEN 12 MOVE AR-VP-ART TO WH-WERT
                   WHEN 13 MOVE AR-GEWICHT TO WH-WERT
                   WHEN 14 MOVE AR-PREIS TO WH-WERT
                           MOVE 1 TO WH-NEG
                           IF AR-PREIS-ERL = 1
                               COMPUTE WH-WERT = WH-WERT * -1
                           end-if
                   WHEN 15 MOVE AR-VERNR TO WH-WERT
                           IF AR-VERNR > 20000 ADD 200000 TO WH-WERT
                           end-if
                   WHEN 16 MOVE AR-ANZEH TO WH-WERT
                   WHEN 17 MOVE AR-GEW TO WH-WERT
                   WHEN 18 MOVE AR-BGEW TO WH-WERT
                   WHEN 19 MOVE AR-SCAN TO WH-WERT
                   WHEN 20 MOVE AR-ANZ-VPEH TO WH-WERT
                           MOVE "q" TO WH-X
                   WHEN 21 MOVE AR-STATKZ TO WH-WERT
                   WHEN 22 MOVE AR-OEPZ TO WH-WERT
                   WHEN 23 MOVE AR-FOLGE TO WH-WERT
                   WHEN 24 MOVE AR-ZMINEH TO WH-WERT
                           MOVE "q" TO WH-X
                   WHEN 25 MOVE SPACE TO WT-TX             *> immer ausg
                           MOVE AR-PRKZ TO WT-TX(1:1)
                           MOVE "A" TO WH-X
                   WHEN 26 MOVE AR-TXT TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 27 MOVE AR-PREIS-ERL TO WH-WERT
                           move "q" to wh-x
                   WHEN 28 MOVE AR-BESTNR TO WH-WERT
                   WHEN 29 MOVE AR-LAGBED TO WH-WERT
                   WHEN 30 MOVE AR-ISTAT TO WH-WERT
                   WHEN 31 MOVE AR-AMAKZ TO WH-WERT
                   WHEN 32 MOVE AR-BTAG TO WH-WERT
                   WHEN 33 MOVE AR-OGT TO WH-WERT
                   WHEN 34 MOVE AR-AMA TO WH-WERT
                   WHEN 35 MOVE 42 TO WX-LG
                           MOVE AR-BEZ TO WX-TX
                           MOVE AR-ZUSBEZ TO WX-TX(21:)
                           CALL "CBL_TOUPPER" USING WX-TX by value WX-LG
                           MOVE 0 TO WR
                           INSPECT WX-TX TALLYING WR FOR ALL "BIO"
                           MOVE WR TO WH-WERT
                   WHEN 36 EVALUATE AR-NUM
                              WHEN 9740 MOVE 1 TO WH-WERT
                              WHEN 9741 MOVE 1 TO WH-WERT
                              WHEN 9742 MOVE 1 TO WH-WERT
                              WHEN 9743 MOVE 1 TO WH-WERT
                              WHEN 9744 MOVE 1 TO WH-WERT
                              WHEN 9745 MOVE 1 TO WH-WERT
                              WHEN 9746 MOVE 1 TO WH-WERT
                              WHEN 9747 MOVE 1 TO WH-WERT
                              WHEN 5971 MOVE 1 TO WH-WERT
                              WHEN 7562 MOVE 1 TO WH-WERT
                              WHEN 7965 MOVE 1 TO WH-WERT
                              WHEN 7145 MOVE 1 TO WH-WERT
                              WHEN 9791 MOVE 1 TO WH-WERT
                              WHEN 9793 MOVE 1 TO WH-WERT
                              WHEN 7566 MOVE 1 TO WH-WERT
                              WHEN 7105 MOVE 1 TO WH-WERT
                              WHEN 7126 MOVE 1 TO WH-WERT
                              WHEN 7146 MOVE 1 TO WH-WERT
                              WHEN OTHER MOVE 0 TO WH-WERT
                           end-evaluate
                   WHEN 37 IF AR-BTAG = 1 MOVE 1 TO WH-WERT
                           end-if
                   WHEN 38 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           PERFORM DRUCK.
           IF WH-MOD = 0 and KA-ARNUM not = 0
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 12
                   IF KA-ZUSBEZ(WY) not = SPACE
                       MOVE WY TO WX
                       PERFORM KALIB-AUS.
           GO G.
      *-----------------------------------------------------------------
           MOVE ALL x"09" TO EX-SATZ.                 *> Tab. Trennung
           MOVE SPACE TO EX-Z.
           MOVE AR-NUM TO EX-NUM.
           MOVE ",  " TO EX-KAL(1:).
           MOVE AR-BEZ TO EX-BEZ.
           MOVE AR-ZUSBEZ TO EX-ZUSBEZ.
           MOVE WT-TX TO EX-KURZBEZ.
           MOVE AR-EAN TO EX-EAN.
           MOVE AR-BESTNR TO WH-EAN.
           PERFORM BEST-NR.
           IF WH-EAN not = 0 MOVE WH-EAN TO EX-BESTNR
           else MOVE SPACE TO EX-BESTNR(1:).
           MOVE AR-ERLGRP TO EX-ERLGRP.
           MOVE AR-ZUSCHLAG TO EX-ZUSCHLAG.
           MOVE AR-MEH TO EX-MEH.
           MOVE AR-UST TO EX-UST.
           MOVE AR-NK TO EX-NK.
           MOVE AR-VP-ART TO EX-VP-ART.
           MOVE AR-GEWICHT TO EX-GEWICHT.
           MOVE AR-PREIS TO EX-PREIS.
           MOVE AR-VERNR TO EX-VERNR.
           MOVE AR-ANZEH TO EX-ANZEH.
           MOVE AR-GEW TO EX-GEW.
           MOVE AR-BGEW TO EX-BGEW.
           MOVE AR-SCAN TO EX-SCAN.
           MOVE AR-STATKZ TO EX-STATKZ.
           MOVE AR-OEPZ TO EX-OEPZ.
           MOVE AR-LAGBED TO EX-LGB.
           WRITE EX-SATZ.
           PERFORM DRUCK.
           IF WH-MOD = 0 and KA-ARNUM not = 0
               PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 12
                   IF KA-ZUSBEZ(WX) not = SPACE
                       PERFORM KALIB-AUS.
           GO C.
       X.  PERFORM END-DRU.
           CLOSE EXCELDRU.
           IF WH-MOD not = 5 DELETE FILE EXCELDRU.
           CLOSE KALIBER.
           CLOSE BESTNUM.
           CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KALIB-ART SECTION.
       A.  MOVE BN-NUM TO KA-ARNUM.
           READ KALIBER IGNORE LOCK INVALID INITIALIZE KA-SATZ.
           COMPUTE WK = (KA-ARNUM - BN-NUM) * 100.
           MOVE WK TO WX.
           PERFORM KALIB-AUS.
       Z.  EXIT.
      ************************************************ Kaliber-Ausgabe *
       KALIB-AUS SECTION.
       A.  PERFORM ARTKO.
           MOVE 54 TO WZ-BREIT.
           PERFORM RAST.
           MOVE WX TO WD-ARTNR.
           MOVE WD-ARTNR(4:2) TO DRD-STR(7:3).
           MOVE KA-ZUSBEZ(WX) TO DRD-STR(10:25).
           MOVE KA-GEW(WX) TO DRD-KGEW.
           MOVE KA-BGEW(WX) TO DRD-BGEW.
           IF DRD-KGEW(2:1) = " " MOVE "o" TO DRD-KGEW(2:1).
           IF DRD-BGEW(2:1) = " " MOVE "o" TO DRD-BGEW(2:1).
           PERFORM DRUCK.
           MOVE WX TO EX-KAL.
           MOVE "," TO EX-KAL(1:1).
           MOVE KA-ZUSBEZ(WX) TO EX-BEZ.
           MOVE KA-RESBEZ(WX) TO EX-ZUSBEZ.
           MOVE KA-GEW(WX) TO EX-GEW.
           MOVE KA-BGEW(WX) TO EX-BGEW.
           MOVE KA-ANZEH(WX) TO EX-ANZEH.
           MOVE KA-BNUM(WX) TO WH-EAN.
           MOVE KA-ARNUM TO EX-NUM.
           PERFORM BEST-NR.
           MOVE SPACE TO EX-BESTNR(1:).
           IF WH-EAN not = 0 MOVE WH-EAN TO EX-BESTNR.
           WRITE EX-SATZ.
       Z.  EXIT.
      ********************************************* Titz Bestellnummer *
       BEST-NR SECTION.
       A.  IF WH-EAN = 0 GO Z.
           COMPUTE WH-EAN = WH-EAN * 10 + 9001456000000.
           COMPUTE WR-EAN(13) = (WR-EAN(2) + WR-EAN(4) + WR-EAN(6)
                      + WR-EAN(8) + WR-EAN(10) + WR-EAN(12)) * 3
                      + (WR-EAN(1) + WR-EAN(3) + WR-EAN(5)
                      + WR-EAN(7) + WR-EAN(9) + WR-EAN(11)).
           COMPUTE WR-EAN(13) = 10 - WR-EAN(13).
       Z.  EXIT.
      ********************************* excel-Liste Artikelbestellcode *
       BESTART SECTION.
       A.  ADD 412 VDU-ECK GIVING VDU-LP.
           MOVE "A" TO AR-ART.
           MOVE WH-VNR TO AR-NUM.
           OPEN OUTPUT EXCELDRU.
           OPEN INPUT KNOTEN.
           OPEN INPUT KALIBER.
           OPEN INPUT BESTNUM.
           MOVE "Art.Nr.;Ar-Bez.;Zus-bez.;Best.-Ean.;Best-Nr."
               TO EXA-SATZ.
           INSPECT EXA-SATZ REPLACING ALL ";" BY x"09".
           WRITE EXA-SATZ.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO X.
       C.  READ ARTIKEL NEXT IGNORE LOCK AT END GO X.
           IF AR-ART NOT = "A" GO X.
           IF AR-NUM > WH-BNR GO X.
           MOVE AR-NUM TO KN-NUM WD-ARTNR.
           START KNOTEN KEY not < KN-AKEY INVALID GO X.
       E.  READ KNOTEN NEXT IGNORE LOCK
                AT END MOVE LOW-VALUES TO KN-SATZ.
           IF AR-NUM not = KN-NUM MOVE LOW-VALUE TO KN-SATZ.
       G.  MOVE ALL x"09" TO EX-SATZ.                 *> Tab. Trennung
           MOVE AR-NUM TO EXA-NUM.
           MOVE AR-BEZ TO EXA-BEZ.
           MOVE AR-ZUSBEZ TO EXA-ZUSBEZ.
           IF KN-NUM = AR-NUM MOVE KN-EAN TO EXA-EAN.
      *                  else MOVE 0 TO EXA-EAN.
           MOVE AR-BESTNR TO WH-EAN.
           PERFORM BEST-NR.
           IF WH-EAN not = 0 MOVE WH-EAN TO EX-BESTNR
           else MOVE SPACE TO EX-BESTNR(1:).
           WRITE EXA-SATZ.
           DISPLAY WD-ARTNR AT VDU-LP.
           READ KNOTEN NEXT IGNORE LOCK
                AT END MOVE LOW-VALUES TO KN-SATZ.
           IF AR-NUM not = KN-NUM GO C.
           GO G.
       X.  CLOSE EXCELDRU.
           CLOSE KNOTEN.
           CLOSE KALIBER.
           CLOSE BESTNUM.
       Z.  EXIT.
      ******************************************************************
       KUNDE SECTION.
       A.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunde:" with highlight AT VDU-LP.
           ADD 7 TO VDU-LP.
           MOVE WH-VNR TO KU-KTONR.
           MOVE 0 TO WM-OPEN.
           MOVE "K" TO KU-SA.
           START KUNDEN KEY NOT < KU-KEY INVALID GO X.
       B.  READ KUNDEN NEXT IGNORE LOCK AT END GO X.
           IF KU-SA not = "K" and WM-OPEN = 1 MOVE 2 TO WM-OPEN
               CALL "CA-TAGE" USING "95CLOSE" WH-CREG
               MOVE "Q:EXC-KRE.TXT" TO WT-TX
               MOVE "Lft-Nr;Telefon;TKV-Art;Zahl-Adr;Ust-Kz;" TO WT-ADR
               INSPECT WT-ADR REPLACING ALL ";" BY "|"
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           IF WH-MOD = 9;
              IF KU-PREISE not = 653 and not = 654 GO B.
           IF KU-KTONR > WH-BNR GO X.
           MOVE KU-KTONR TO WD-PERSNR.
           DISPLAY WD-PERSNR AT VDU-LP " " KU-MCODE.
           IF WM-EXC = 1 GO G.                     *> nur Excel-Ausgabe
           PERFORM KOPFDRUCK.
           MOVE KU-KTONR TO WD-PERSNR.
           DISPLAY WD-PERSNR AT VDU-LP " " KU-MCODE.
           MOVE KU-KTONR TO DRE-NR.
           MOVE KU-BEZ TO DRE-BEZ WT-BEZ.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE KU-MCODE TO DRE-MCODE WT-TX.
           PERFORM DRUCK.
           MOVE KU-DRUCK-KZ TO DRE-DRUCK-KZ.
           MOVE KU-PREISE TO DRE-PREIS.
           MOVE KU-REART TO DRE-REART.
           MOVE KU-FOLGE TO DRE-FIBU.
           MOVE KU-PLASTKI-VERR TO DRE-PK.
           MOVE KU-BAN TO DRE-BAN.
           MOVE KU-VERSART TO DRE-VERS.
           MOVE KU-RAB TO DRE-RAB.
           MOVE KU-BONUS TO DRE-BONUS.
           MOVE KU-READR TO DRE-READR.
           MOVE KU-LIEFERADR TO DRE-LIEFADR.
           MOVE KU-FIBU TO DRE-FIBU.
           MOVE KU-STAT TO DRE-STAT.
           MOVE KU-SCAN TO DRE-SCAN.
           MOVE KU-EMB-STAND TO DRE-EMB.
           MOVE KU-EMB-PSTAND TO DRE-PEMB.
           MOVE KU-RABKZ TO DRE-EINZ.
           PERFORM DRUCK.
           GO B.
       G.  MOVE KU-KTONR TO WH-NUM.
           IF KU-SA not = "K" PERFORM MAESTER
                              GO B.
           IF WM-OPEN = 0 PERFORM EXCELKOPF.
           IF KU-BESTKZ(1:) = SPACE MOVE 0 TO KU-BESTKZ.
           IF KU-RABADR(1:) = SPACE MOVE 0 TO KU-RABADR.
           IF KU-EMBADR(1:) = SPACE MOVE 0 TO KU-EMBADR.
           IF WH-NUM > 299999 MOVE 0 TO KU-BAN KU-NAD-DP.
           IF KU-BAN(1:) = SPACE MOVE 0 TO KU-BAN.
           IF KU-NAD-DP(1:) = SPACE MOVE 0 TO KU-NAD-DP.
           INSPECT KU-TEL REPLACING ALL X"00" BY SPACE.
           INSPECT KU-ANMERK REPLACING ALL X"00" BY SPACE.
           INSPECT KU-SL REPLACING ALL X"00" BY SPACE.
           INSPECT KU-BEMERK REPLACING ALL X"00" BY SPACE.
           INSPECT KU-TRANS REPLACING ALL X"00" BY SPACE.
           INSPECT KU-EMAIL REPLACING ALL X"00" BY SPACE.
      *    ADD 200000 TO KU-LIEFERADR KU-FIBU KU-READR KU-EMBADR.
      *    IF KU-RABADR not = 0 ADD 200000 TO KU-RABADR.
           IF KU-FILNR(1:2) = SPACE or KU-FILNR = x"0020"
           MOVE 0 TO KU-FILNR.
           PERFORM ADR-KUND.
      *------------------------------------->  Ausgabe in Excel-Datei <-
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 47
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 MOVE WH-NUM TO WH-WERT
                   WHEN  2 COMPUTE WD-TRENN = KU-DRUCK-KZ / 10
                           MOVE WD-TRENN TO WH-WERT
                   WHEN  3 MOVE KU-PREISE TO WH-WERT
                   WHEN  4 MOVE KU-REART TO WH-WERT
                   WHEN  5 MOVE KU-FOLGE TO WH-WERT
                   WHEN  6 MOVE KU-PLASTKI-VERR TO WH-WERT
                   WHEN  7 MOVE KU-BAN TO WT-TX
                           MOVE "E" TO WH-X
                   WHEN  8 MOVE KU-VERSART TO WH-WERT
                   WHEN  9 MOVE KU-RAB TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN 10 MOVE KU-BONUS TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN 11 MOVE KU-AMASTAT TO WH-WERT
                   WHEN 12 MOVE KU-READR TO WH-WERT
                   WHEN 13 MOVE KU-LIEFERADR TO WH-WERT
                   WHEN 14 MOVE KU-FIBU TO WH-WERT
                   WHEN 15 MOVE KU-RABKZ TO WH-WERT
                   WHEN 16 MOVE KU-STAT TO WH-WERT         *> ob 200000
                   WHEN 17 MOVE KU-LFKZ TO WD-TRENN
                           MOVE WD-TRENN TO WH-WERT
                   WHEN 18 MOVE KU-SAMKZ TO WH-WERT
                   WHEN 19 MOVE KU-TEL TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 20 MOVE KU-ANMERK TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 21 MOVE KU-NAD-DP TO WT-TX
                           MOVE "E" TO WH-X
                   WHEN 22 MOVE KU-EMBADR TO WH-WERT
                   WHEN 23 MOVE KU-RABADR TO WH-WERT
                   WHEN 24 MOVE KU-SUBART TO WH-WERT
                   WHEN 25 MOVE KU-BESTKZ TO WH-WERT
                   WHEN 26 MOVE KU-SCAN TO WH-WERT
                   WHEN 27 MOVE KU-LFKOP TO WD-TRENN
                           MOVE WD-TRENN TO WH-WERT
                   WHEN 28 MOVE KU-SL TO  WT-TX
                           MOVE "A" TO WH-X
                   WHEN 29 MOVE KU-BEMERK TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 30 MOVE KU-TRANS TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 31 MOVE KU-EMAIL TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 32 MOVE KU-FILNR TO WH-WERT
                   WHEN 33 MOVE KU-TX TO WH-WERT
                   WHEN 34 MOVE KU-AMA TO WH-WERT
                   WHEN 35 MOVE KU-OGT TO WH-WERT
                   WHEN 36 MOVE KU-DRUCK-KZ TO WD-TRENN
                           MOVE WD-TRENN TO WH-WERT
                   WHEN 37 COMPUTE WD-TRENN = KU-LFKZ / 10
                           MOVE WD-TRENN TO WH-WERT
                   WHEN 38 COMPUTE WD-TRENN = KU-LFKOP / 10
                           MOVE WD-TRENN TO WH-WERT
                   WHEN 39 MOVE WXK-NAME1 TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 40 MOVE WXK-NAME2 TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 41 MOVE WXK-NAME3 TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 42 MOVE WXK-STR TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 43 MOVE WXK-PLZL TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 44 MOVE WXK-ORT TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 45 MOVE WXK-LANDKZ TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 46 MOVE KU-MCODE TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN 47 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           GO B.
       X.  IF WM-EXC = 0 PERFORM END-DRU.
           MOVE 0 TO WM-OPEN.
           CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
       Z.  EXIT.
      ************************************** Adressen an Fibu Softline *
       ADR-KUND SECTION.
       A.  MOVE SPACE TO WT-ADR.
           MOVE SPACE TO WXK-PLZL.
           MOVE SPACE TO WXK-ORT WXK-STR WXK-NAME3.
           MOVE KU-BEZ TO WT-BEZ.
           INSPECT KU-BEZ REPLACING ALL X"00" BY " ".
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
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
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
      *************************************************** MÑster-Daten *
       MAESTER SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 MOVE WH-NUM TO WH-WERT
                   WHEN  2 MOVE KU-TEL TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  3 MOVE KU-READR TO WH-WERT
                   WHEN  4 MOVE 0 TO WH-WERT
                           IF KU-FIBU > 0 MOVE KU-FIBU TO WH-WERT
                           end-if
                   WHEN  5 MOVE KU-LFKZ TO WH-WERT
                           IF WH-WERT = 0 MOVE 1 TO WH-WERT
                           end-if
                   WHEN  6 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
       Z.  EXIT.
      ******************************************************************
       UMLAUTE SECTION.
       A.  INSPECT WT-TX REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹"
                    ALL "+" by "&".
       Z.  EXIT.
      ****************************************** Kopf fÅr Excelausgabe *
       EXCELKOPF SECTION.
       A.  MOVE "Q:EXC-KD.TXT" TO WT-TX.
           MOVE "Kunr;Dru-Re;Pr-Grp;Re-Art;Folge;Pk-Ver;GLN;Versand;Rab-
      -         "%;Bonus;Ama-Stat;Re.Adr;Lf.Adr;Fib.Adr;Rab-Kz;Stat;Lf-k
      -         "z;Samm-kz;Tele;Anmerk;" TO WT-ADR.
           MOVE "NAD;Emb-Adr;Rab-Adr;Sub-Art;Best-Kz;Scan;Lf-Kop;Sl;Beme
      -         "rk;Trans;Email;FilNr.;Ku-tx;AMA;oGT;Dru-Lf;ARA;Stk-Kz"
                     TO WT-ADR(133:)
           MOVE ";Name1;Name2;Name3;Strasse;Plzl;Ort;Land;M-Code"
               TO WT-BEZ.
           INSPECT WT-ADR REPLACING ALL ";" BY "|".
           INSPECT WT-BEZ REPLACING ALL ";" BY "|".
           CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           MOVE 1 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       PREIS SECTION.
       A.  OPEN INPUT KALIBER.
           ADD 503 VDU-ECK GIVING VDU-LP.
           MOVE 61 TO WM-SIZE.
           DISPLAY "Preisliste:" with highlight AT VDU-LP.
           ADD 12 TO VDU-LP.
           MOVE WH-VNR TO PR-NR.
           MOVE 0 TO PR-ARTNR.
           MOVE -1 TO WV-NR.
           IF WH-PG = 3 or 10
                        MOVE "P" TO PR-SA
                   else MOVE "S" TO PR-SA.
           START PREISLI KEY NOT < PR-KEY INVALID GO X.
           IF WM-EXC = 1 GO H.
       B.  READ PREISLI NEXT IGNORE LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO B.
      *    IF WH-PG = 3 and PR-SA NOT = "P" GO X.
      *    IF WH-PG = 5 and PR-SA NOT = "S" GO X.
           IF PR-NR > WH-BNR GO I.
           MOVE PR-NR TO WD-PERSNR.
           IF WM-KOPF = 1;
               IF PR-ARTNR = 0 PERFORM NURKOPF
               end-if GO B.
           IF PR-ARTNR = 0 MOVE PR-BEZ TO WT-PRBEZ
               ADD 515 VDU-ECK GIVING VDU-LP
               DISPLAY WD-PERSNR AT VDU-LP " " WT-TX(1:15)
               IF WM-EXC = 1 PERFORM EXC-PREIS
               end-if
               GO B.
           IF WV-NR = -1 MOVE PR-NR TO WV-NR.
           IF PR-NR NOT = WV-NR MOVE 70 TO WZ-ZEILEN
               MOVE PR-NR TO WV-NR.
      *    IF PR-BEWDAT > 29000000 GO B.
           PERFORM KOPFDRUCK.
           MOVE PR-ARTNR TO DRF-ARTNR AR-NUM KA-ARNUM.
           COMPUTE WD-SNR = PR-ARTNR * 100.
           IF WH-VAR = 1 and WD-SNR not = ZEROS GO B.
           MOVE "A" TO AR-ART.
       C.  READ ARTIKEL IGNORE LOCK INVALID MOVE " -fehlt-" TO AR-BEZ.
           MOVE AR-BEZ TO DRF-BEZ.
           ADD 540 VDU-ECK GIVING VDU-LP.
           DISPLAY DRF-ARTNR with highlight AT VDU-LP.
           IF WD-SNR not = ZEROS
               READ KALIBER IGNORE LOCK INVALID GO B.
           IF WD-SNR not = ZEROS MOVE WD-SNR TO WX DRF-ZUSNR
               MOVE "-" TO DRF-ZUSNR(1:1)
               MOVE KA-ZUSBEZ(WX) TO DRF-BEZ.
           IF WH-PG = 10 PERFORM KUVKP GO G.
           MOVE PR-PREIS TO DRF-PREIS.
           IF PR-AKTION not = 0 MOVE PR-AKTION TO DRF-AKTION
               IF PR-VDAT not = 0 MOVE PR-VDAT TO WC-DATUM
                   PERFORM DATDREH
                   MOVE VDU-DATUM TO DRF-VON
                   MOVE PR-BDAT TO WC-DATUM
                   PERFORM DATDREH
                   MOVE VDU-DATUM TO DRF-BIS.
           PERFORM DRUCK.
           MOVE PR-EAN TO DRF-EAN.
           MOVE PR-FPREIS TO DRF-PREIS.
           IF PR-FAKTION not = 0 MOVE PR-FAKTION TO DRF-AKTION
               MOVE PR-AKTION-KZ TO DRF-KZ
               MOVE PR-FVDAT TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRF-VON
               MOVE PR-FBDAT TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRF-BIS.
       G.  PERFORM DRUCK.
           GO B.
      *------------------------------------->  Ausgabe in Excel-Datei <-
       H.  READ PREISLI NEXT IGNORE LOCK AT END GO I.
           ADD 515 VDU-ECK GIVING VDU-LP.
           IF PR-NR > WH-BNR GO I.
           MOVE PR-NR TO WD-NR.
           MOVE PR-ARTNR TO WD-PREIS.
           DISPLAY WD-NR AT VDU-LP " " WD-PREIS.
           PERFORM EXC-PREIS.
           GO H.
      *----------------------------------------> Preiswartung starten <-
       I.  OPEN INPUT PRWART.
           INITIALIZE PW-SATZ.
           MOVE WH-VNR TO PW-NR.
           MOVE "P" TO PW-SA.
           START PRWART KEY NOT < PW-KEY INVALID GO W.
       K.  READ PRWART NEXT IGNORE LOCK AT END GO W.
           IF PW-BEWDAT < 20100000 or > 20130000 GO K.
           IF PW-NR > WH-BNR GO W.
           MOVE PW-NR TO WD-PERSNR.
           MOVE PW-ARTNR TO WD-ARTNR.
271112     ADD 20000000 PW-DATUM GIVING WS-DATUM.
271112     IF PW-BEWDAT > WS-DATUM MOVE WS-DATUM TO PW-BEWDAT.
           DISPLAY WD-PERSNR AT 2320 " " WD-ARTNR.
           MOVE PW-SA TO PR-SA.
           move "A" TO PR-SA.
           MOVE PW-NR TO PR-NR.
           MOVE PW-ARTNR TO PR-ARTNR.
           MOVE PW-SUBNR TO PR-SUBNR.
           MOVE PW-EAN TO PR-EAN
           MOVE PW-PREIS TO PR-PREIS
           MOVE PW-AKTION TO PR-AKTION.
           MOVE PW-VDAT TO PR-VDAT
           MOVE PW-BDAT TO PR-BDAT
           MOVE PW-FPREIS TO PR-FPREIS
           MOVE PW-FAKTION TO PR-FAKTION
           MOVE PW-FVDAT TO PR-FVDAT
           MOVE PW-FBDAT TO PR-FBDAT
           MOVE PW-AKTION-KZ TO PR-AKTION-KZ
           MOVE PW-FARTNR TO PR-FARTNR
           MOVE PW-BEWDAT TO PR-BEWDAT
           MOVE PW-KIKTN TO PR-KIKTN
           MOVE PW-STKVER TO PR-STKVER
           MOVE PW-MHD TO PR-MHD
           MOVE PW-VGEW TO PR-VGEW
           MOVE PW-BGEW TO PR-BGEW
           MOVE PW-VSTK TO PR-VSTK
           MOVE PW-PLU TO PR-PLU.
           IF PR-MHD(1:) = SPACE
              MOVE 0 TO PR-MHD PR-VGEW PR-BGEW PR-PLU.
           IF PR-VSTK(1:) = SPACE MOVE 0 TO PR-VSTK.
           IF WM-EXC = 1 PERFORM EXC-PREIS.
           GO K.
       W.  CLOSE PRWART.
           MOVE 0 TO WM-OPEN.
       X.  IF WH-PG = 3 and WM-KOPF = 1 MOVE 0 TO WM-OPEN.
           IF WH-PG = 10 and WM-OPEN not = 0
               MOVE ALL "ƒ" TO DRA-STR
               PERFORM DRUCK
               MOVE WM-DATUM TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRA-STR
               PERFORM DRUCK.
           PERFORM END-DRU.
           CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
           CLOSE KALIBER.
       Z.  EXIT.
      ******************************************************************
       NURKOPF SECTION.
       A.  IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               MOVE "PRKOPF.TXT" TO WN-EXCEL
               OPEN OUTPUT EXCELDRU
               MOVE "Pr-Nr.;Bezeichung" TO EXA-SATZ
               WRITE EXA-SATZ.
           MOVE PR-NR TO WD-PERSNR.
           MOVE WD-PERSNR TO EXA-SATZ(1:).
           MOVE ";" TO EXA-SATZ(6:1).
           INSPECT PR-BEZ REPLACING ALL ";" BY SPACE.
           MOVE PR-BEZ TO EXA-SATZ(7:).
           IF PR-BEZ = SPACE GO Z.
           WRITE EXA-SATZ.
           DISPLAY WD-PERSNR AT VDU-LP " " PR-BEZ.
       Z.  EXIT.
      *********************************************** Ausgabe in Excel *
       EXC-PREIS SECTION.
       A.  IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               MOVE "Q:EXC-PR.TXT" TO WT-TX
               MOVE "KD-Nr.;Satzart;Art-Nr;EAN;Preis;A-Preis;abDat;bisDa
      -             "t;Kd-Vkp;Kd-AVkp;VabDat;VbisDat;" TO WT-ADR
               MOVE "gesRab;Kd-Art;Sperre;Datum;KiKtn;Stkver;MHD;vonGew;
      -             "bisGew;Inhalt;PLU-Nr;Bezeichnung;Pr-SAN;"
                         TO WT-ADR(85:)
               INSPECT WT-ADR REPLACING ALL ";" BY "|"
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
       G.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 35
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG WS-DATUM
               IF PR-ARTNR = 0 and WX = 30 MOVE ALL ";" TO WT-TX
                   MOVE PR-BEZ TO WT-TX(22:)
                   MOVE "U" TO WH-X
               end-if
               IF PR-ARTNR = 0 and WX = 31 MOVE SPACE TO WT-TX
                   MOVE "A" TO WH-X
                   CALL "CA-TAGE" USING "92ALPHA" WH-CREG
                   MOVE 26 TO WX
               end-if
               EVALUATE WX
                   WHEN  1 MOVE PR-NR TO WH-WERT
                   WHEN  2 MOVE PR-SA TO WT-TX
                           MOVE "A" TO WH-X
                           IF PR-ARTNR = 0 MOVE 29 TO WX
                           end-if
                   WHEN  3 MOVE PR-ARTNR TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN  4 MOVE PR-EAN TO WT-TX
                           MOVE "E" TO WH-X
                   WHEN  5 MOVE PR-PREIS TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN  6 MOVE PR-AKTION TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN  7 MOVE PR-VDAT TO WS-DATUM
                           MOVE "D" TO WH-X
                   WHEN  8 MOVE PR-BDAT TO WS-DATUM
                           MOVE "D" TO WH-X
                   WHEN  9 MOVE PR-FPREIS TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN 10 MOVE PR-FAKTION TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN 11 MOVE PR-FVDAT TO WS-DATUM
                           MOVE "D" TO WH-X
                   WHEN 12 MOVE PR-FBDAT TO WS-DATUM
                           MOVE "D" TO WH-X
                   WHEN 13 MOVE PR-AKTION-KZ TO WT-TX
                           MOVE "A" TO WH-X
                   WHEN 14 MOVE PR-FARTNR TO WH-WERT
                   WHEN 15 IF PR-BEWDAT > 29999999 MOVE 1 TO WH-WERT
                           end-if
                   WHEN 16 MOVE PR-BEWDAT TO WS-DATUM
                           IF WS-DATUM > 29999999
                               ADD -10000000 TO WS-DATUM
                           end-if
                           MOVE "D" TO WH-X
                   WHEN 17 MOVE PR-KIKTN TO WT-TX
                           MOVE "A" TO WH-X
                   WHEN 18 MOVE PR-STKVER TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN 19 MOVE PR-MHD TO WH-WERT
                   WHEN 20 MOVE PR-VGEW TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN 21 MOVE PR-BGEW TO WH-WERT
                           MOVE 1 TO WH-NEG
                   WHEN 22 MOVE PR-VSTK TO WH-WERT
                   WHEN 23 COMPUTE PR-PLU = PR-NR * 1000000 + PR-ARTNR
                           MOVE PR-PLU TO WH-WERT
                   WHEN 24 MOVE SPACE TO WT-TX
                           MOVE "A" TO WH-X
                   WHEN 25 MOVE PR-SUBNR TO WT-TX
                           MOVE "E" TO WH-X
                   WHEN 26 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
       Z.  EXIT.
      *************************************** Kundenverkaufspreisliste *
       KUVKP SECTION.
       A.  MOVE PR-ARTNR TO DRV-ARTNR AR-NUM.
           MOVE AR-BEZ TO DRV-BEZ.
           MOVE PR-FPREIS TO DRV-PREIS.
           IF PR-AKTION not = 0 and
                      (WV-DATUM not < PR-VDAT and not > PR-BDAT)
               MOVE PR-FAKTION TO DRV-PREIS
               MOVE PR-VDAT TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRV-VON
               MOVE PR-BDAT TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRV-BIS.
           MOVE PR-SUBNR TO DRV-GCODE.
           IF PR-EAN > 999999 MOVE PR-EAN TO DRV-EAN
                         else MOVE PR-EAN TO DRV-PCODE.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       KUPREIS SECTION.
       A.  ADD 503 VDU-ECK GIVING VDU-LP.
           OPEN INPUT KALIBER.
           DISPLAY "Preisliste:" with highlight AT VDU-LP.
           ADD 12 TO VDU-LP.
           MOVE WH-VNR TO PR-NR.
           MOVE 0 TO PR-ARTNR.
           MOVE -1 TO WV-NR.
           IF WH-PG = 3 or 8 or 13 MOVE "P" TO PR-SA
                   else MOVE "S" TO PR-SA.
           START PREISLI KEY NOT < PR-KEY INVALID GO X.
       C.  READ PREISLI NEXT IGNORE LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WH-PG = 3 and PR-SA NOT = "P" GO X.
           IF WH-PG = 5 and PR-SA NOT = "S" GO X.
           IF PR-NR > WH-BNR GO X.
           MOVE PR-NR TO WD-PERSNR.
           IF PR-ARTNR = 0 MOVE PR-BEZ TO WT-TX
               DISPLAY WD-PERSNR AT VDU-LP " " WT-TX(1:15) GO C.
           IF WH-PG = 8 and WH-SPER = 0 and PR-BEWDAT > 20500000 GO C.
           IF WV-NR = -1 MOVE PR-NR TO WV-NR.
           IF PR-NR NOT = WV-NR MOVE 70 TO WZ-ZEILEN
               MOVE PR-NR TO WV-NR.
           PERFORM PREIKOPF.
           MOVE PR-ARTNR TO DRK-ARTNR AR-NUM.
           IF PR-BEWDAT > 20500000 MOVE "*" TO DRK-STR(3:1).
           MOVE SPACE TO DRK-ARTNR(7:3).
           IF WH-PG = 13 GO K.
           IF PR-ARTNR NOT = AR-NUM GO C.
           MOVE "A" TO AR-ART.
       G.  READ ARTIKEL IGNORE LOCK INVALID MOVE "-fehlt-" TO AR-BEZ.
           MOVE AR-BEZ TO DRK-BEZ.
           IF WH-PG = 8 MOVE AR-NUM TO EXP-ARTNR
                      MOVE AR-TXT TO EXP-ARBEZ
                      MOVE PR-PREIS TO EXP-PREIS
                      INSPECT EXP-ARBEZ REPLACING ALL "·" BY "ﬂ"
                      INSPECT EXP-ARBEZ REPLACING ALL "é" BY "ƒ"
                      INSPECT EXP-ARBEZ REPLACING ALL "ô" BY "÷"
                      INSPECT EXP-ARBEZ REPLACING ALL "ö" BY "‹"
                      INSPECT EXP-ARBEZ REPLACING ALL "Ñ" BY "‰"
                      INSPECT EXP-ARBEZ REPLACING ALL "î" BY "ˆ"
                      INSPECT EXP-ARBEZ REPLACING ALL "Å" BY "¸"
                      WRITE EXP-SATZ
                      MOVE SPACE TO EXP-SATZ.
           IF WH-PG = 8;
               IF PR-ARTNR not = AR-NUM and WV-PREIS = PR-PREIS GO C.
           MOVE PR-PREIS TO DRK-PREIS WH-NTTO WV-PREIS.
           MOVE 0 TO WH-BONUS WH-SKTO.
           IF WH-VAR = 1 GO K.
           IF KU-RAB not = 0 MOVE KU-RAB TO DRK-RAB
               MOVE "%" TO DRK-PZ1
               COMPUTE WH-NTTO rounded = PR-PREIS * KU-RAB / 100
               COMPUTE WH-NTTO = PR-PREIS - WH-NTTO.
           IF KU-BONUS not = 0 MOVE KU-BONUS TO DRK-BONUS
               MOVE "%" TO DRK-PZ2
               COMPUTE WH-BONUS rounded = WH-NTTO * KU-BONUS / 100.
           MOVE KU-KOND TO WH-KK.
           IF WH-SK1 not = 0 MOVE WH-SK1 TO DRK-SKTO
               MOVE "%" TO DRK-PZ3
               COMPUTE WH-SKTO rounded = WH-NTTO * WH-SK1 / 100
           else MOVE 0 TO WH-SKTO.
           COMPUTE DRK-NTTO = WH-NTTO - WH-SKTO - WH-BONUS.
           PERFORM DRUCK.
           GO C.
      *--------------------------------> Kundenpreiliste + Bestellnr. <-
       K.  IF PR-ARTNR not = AR-NUM GO O.
           MOVE "A" TO AR-ART.
       M.  READ ARTIKEL IGNORE LOCK INVALID MOVE "-fehlt-" TO AR-BEZ.
           MOVE AR-BEZ TO DRK-BEZ.
           MOVE PR-PREIS TO DRK-PREIS.
           MOVE AR-BESTNR TO WH-EAN.
           PERFORM BEST-NR.
           IF WH-EAN not = 0 MOVE WH-EAN TO DRK-STR(56:).
           PERFORM DRUCK.
           IF PR-ARTNR = AR-NUM GO C.
       O.  MOVE PR-ARTNR TO KA-ARNUM.
           COMPUTE WX = (PR-ARTNR - AR-NUM) * 100.
           READ KALIBER INVALID GO C.
           MOVE PR-ARTNR TO DRK-ARTNR.
           MOVE KA-ZUSBEZ(WX) TO DRK-BEZ.
           MOVE PR-PREIS TO DRK-PREIS.
           MOVE KA-BNUM(WX) TO WH-EAN.
           PERFORM BEST-NR.
           IF WH-EAN not = 0 MOVE WH-EAN TO DRK-STR(56:).
           PERFORM DRUCK.
           GO C.
       X.  IF WM-OPEN = 0 GO Y.
           MOVE ALL "ƒ" TO DRK-STR.
           PERFORM DRUCK.
           PERFORM END-DRU.
           IF WH-PG = 8 CLOSE EXCELDRU.
       Y.  CLOSE KALIBER.
       Z.  EXIT.
      *********************************************** Barcodeetiketten *
       ETIKDRU SECTION.
       A.  ADD 503 VDU-ECK GIVING VDU-LP.
           MOVE "TOUR" TO WH-DRUNAM(1:4).
           MOVE "e&l1s26a7d0O(s0p12h0b20v0s4102T&a6L" TO WE-STG(9).
           DISPLAY "Tour:" with highlight AT VDU-LP.
           ADD 6 TO VDU-LP.
           MOVE 0 TO TS-LFD WM-KOP.
           MOVE -1 TO WV-NR.
           MOVE WH-VNR TO TS-NR WH-TO.
           START S-TOUR KEY NOT < TS-KEY INVALID GO Z.
       B.  READ S-TOUR NEXT IGNORE LOCK AT END GO X.
           IF TS-NR > WH-BNR GO X.
           MOVE TS-NR TO WD-TONR.
           DISPLAY WD-TONR AT VDU-LP " " TS-KURZBEZ.
           IF TS-LFD = 0 MOVE TS-KURZBEZ TO WM-BEZ
                         MOVE TS-NR TO WM-TO
                         MOVE TS-ALKEY TO WM-ZUS GO B.
           IF WV-NR = -1 MOVE TS-NR TO WV-NR.
           IF WV-NR NOT = TS-NR;
               IF WM-OPEN > 0 MOVE "&l0S" TO DRA-SATZ
                   WRITE DRA-SATZ AFTER 0
                   PERFORM END-DRU
                   MOVE TS-NR TO WV-NR
                   MOVE 0 TO WZ-ZEILEN WZ-SCHALT WM-KOP.
           IF WZ-ZEILEN > 59 MOVE 63 TO WZ-ZEILEN.
           PERFORM KOPFDRUCK.
           MOVE TS-NR TO WH-TO.
           MOVE TS-LFD TO WH-LFD.
           MULTIPLY 10 BY WH-TOURNR GIVING WH-EAN.
           MOVE "K" TO KU-SA.
           MOVE TS-KUNR TO KU-KTONR.
       C.  READ KUNDEN IGNORE LOCK INVALID NEXT SENTENCE.
      *--------------------------------------------> Barcode ausgeben <-
           MOVE 0 TO WL.
           IF WM-DRU = 1 PERFORM N-BAR
                         PERFORM N-EANDRU
           else PERFORM L-BAR
                PERFORM L-EANDRU.
           GO B.
       X.  MOVE "&l0S" TO DRA-SATZ.
           WRITE DRA-SATZ AFTER 0.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       L-BAR SECTION.
       A.  MULTIPLY 10 BY WH-EAN GIVING WM-EAN.
           ADD WT-7(1) WT-7(3) WT-7(5) WT-7(7) GIVING WI.
           MULTIPLY 3 BY WI.
           ADD WT-7(2) WT-7(4) WT-7(6) TO WI.
           DIVIDE 10 INTO WI GIVING WI REMAINDER WI.
           SUBTRACT WI FROM 10 GIVING WT-7(8).
           MOVE WM-EAN TO WH-EAN.
           SET X4 X3 X1 IX TO 1.
           MOVE ZEROS TO WL-MUSTER.
           MOVE 1 TO WI.
       B.  IF WH-ANF(WI) = 1 MOVE "11" TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF WI < 6 ADD 1 TO WI GO B.
       E.  IF WT-7(IX) = 0 MOVE 10 TO WS-LR
                else MOVE WT-7(IX) TO WS-LR.
       F.  IF IX > 4; IF WH-RE(WS-LR X3) = 1 MOVE "11" TO WL-STRI(X4)
                      else MOVE ZEROS TO WL-STRI(X4)
                 else IF WH-LI(WS-LR X3) = 1 MOVE "11" TO WL-STRI(X4)
                      else MOVE ZEROS TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF X3 < 7 SET X3 UP BY 1 GO F.
           SET X3 TO 1.
           IF IX NOT = 4 GO I.
           MOVE 2 TO WI.
       H.  IF WH-MIT(WI) = 1 MOVE "11" TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF WI < 5 ADD 1 TO WI GO H.
       I.  SET IX UP BY 1.
           IF IX NOT > 8 GO E.
           MOVE 2 TO WI.
       K.  IF WH-END(WI) = 1 MOVE "11" TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF WI < 4 ADD 1 TO WI GO K.
           MOVE 13 TO WI.
           MOVE "*b000Y*b18W" TO WS-BAR.
      *--------------------------------------------------> Umwandlung <-
           PERFORM VARYING WX FROM 1 BY 8 UNTIL WX > 144
               MOVE 0 TO WC-EAN
               ADD 1 TO WI
               MOVE WL-MUSTER(WX:8) TO WV-MCODE(1:)
               IF WV-MC(1) = "1"   ADD 128 TO WC-EAN
                   end-if
               IF WV-MC(2) = "1" ADD  64 TO WC-EAN
                   end-if
               IF WV-MC(3) = "1" ADD  32 TO WC-EAN
                   end-if
               IF WV-MC(4) = "1" ADD  16 TO WC-EAN
                   end-if
               IF WV-MC(5) = "1" ADD   8 TO WC-EAN
                   end-if
               IF WV-MC(6) = "1" ADD   4 TO WC-EAN
                   end-if
               IF WV-MC(7) = "1" ADD   2 TO WC-EAN
                   end-if
               IF WV-MC(8) = "1" ADD   1 TO WC-EAN
                   end-if
               MOVE WC-EAN(1:) TO WS-BAR(WI:1).
       Z.  EXIT.
      ******************************************************************
       L-EANDRU SECTION.
       A.  MOVE WZ-ZEILEN TO WH-ZEI.
      *    MOVE "*t150R" TO DRA-SATZ(1:).
      *    MOVE 0 TO WZ-SCHALT.
      *    PERFORM DRUCK.
      *    MOVE "*r1A" TO DRA-SATZ(1:).
      *    MOVE 0 TO WZ-SCHALT.
      *    PERFORM DRUCK.
      *    PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 52
      *        MOVE WS-BAR TO DRY-EAN(WX).
      *    MOVE 0 TO WZ-SCHALT.
      *    MOVE "*rC" TO DRY-EAN(WX).
           MOVE ALL "ƒƒƒƒ" TO DRA-SATZ(2:80).
           PERFORM DRUCK.
           MOVE SPACE TO DRY-SATZ.
      *    ADD 2,0 WH-ZEI GIVING WZ-ZEILEN.
           COMPUTE WD-BAR = WH-EAN / 10.
      *    MOVE WH-EAN TO WD-BAR.
           MOVE WD-BAR TO DRX-SATZ(2:).
           MOVE TS-KURZBEZ TO DRX-SATZ(15:).
           MOVE KU-SL TO DRX-SATZ(35:1).
091001*-----------------------------> KANN WIEDER ENTFERNT WERDEN <-
           inspect ku-anmerk replacing all "  S" by "   ".
           MOVE KU-ANMERK TO DRX-SATZ(37:).
           PERFORM DRUCK.
           PERFORM DRUCK.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       N-BAR SECTION.
      *A.  MULTIPLY 10 BY WH-EAN GIVING WM-EAN.
      *    ADD WT-7(1) WT-7(3) WT-7(5) WT-7(7) GIVING WI.
      *    MULTIPLY 3 BY WI.
      *    ADD WT-7(2) WT-7(4) WT-7(6) TO WI.
      *    DIVIDE 10 INTO WI GIVING WI REMAINDER WI.
      *    SUBTRACT WI FROM 10 GIVING WT-7(8).
      *    MOVE WM-EAN TO WH-EAN.
      *    MOVE 5 TO ET-HOE.                        *> Barcodehîhe
      *    MOVE 10 TO ET-LIN.                       *> Ausgleichszeilen
      *    SET X3 X1 IX TO 1.
      *    MOVE LOW-VALUE TO WN-MUSTER.
      *    MOVE 1 TO WI.
      *B.  IF WH-ANF(WI) = 1 MOVE WX-FF TO WN-STRI(X1).
      *    SET X1 UP BY 1.
      *    IF WI < 6 ADD 1 TO WI GO B.
      *E.  IF WT-7(IX) = 0 MOVE 10 TO WS-LR
      *         ELSE MOVE WT-7(IX) TO WS-LR.
      *F.  IF IX > 4; IF WH-RE(WS-LR X3) = 1 MOVE WX-FF TO WN-STRI(X1)
      *               ELSE MOVE  LOW-VALUE TO WN-STRI(X1)
      *          ELSE IF WH-LI(WS-LR X3) = 1 MOVE WX-FF TO WN-STRI(X1)
      *               ELSE MOVE LOW-VALUE TO WN-STRI(X1).
      *    SET X1 UP BY 1.
      *    IF X3 < 7 SET X3 UP BY 1 GO F.
      *    SET X3 TO 1.
      *    IF IX NOT = 4 GO I.
      *    MOVE 2 TO WI.
      *H.  IF WH-MIT(WI) = 1 MOVE WX-FF TO WN-STRI(X1).
      *    SET X1 UP BY 1.
      *    IF WI < 5 ADD 1 TO WI GO H.
      *I.  SET IX UP BY 1.
      *    IF IX NOT > 8 GO E.
      *    MOVE 2 TO WI.
      *K.  IF WH-END(WI) = 1 MOVE WX-FF TO WN-STRI(X1).
      *    SET X1 UP BY 1.
      *    IF WI < 4 ADD 1 TO WI GO K.
      *    SET X1 TO 138.
       Z.  EXIT.
      ***********************************************************************
       ZAHLEN SECTION.
      *A.  MOVE LOW-VALUE TO WC-STR.
      *    MOVE 3 TO WH-ST1.
      *    MOVE "l" TO WH-ST2.
      *    MOVE WH-ESC TO WC-LSL(3).
      **   MOVE x"1B6C03" TO WC-LSL(3).
      *    SET X2 TO 1.
      *    SET EX TO 6.
      *C.  MOVE WR-EAN(EX) TO WC-X(X2).
      *    IF X2 < 8 SET EX X2 UP BY 1 GO C.
      *    MOVE 0 TO WC-LNG.
      *    WRITE DRX-SATZ FROM WB-BC AFTER 0.
      *    MOVE 0 TO WH-ST1.
      *    MOVE "l" TO WH-ST2.
      *    WRITE DRX-SATZ FROM WH-ESC AFTER 0.
       Z.  EXIT.
      ************************************** Lieferschein mit EAN-Code *
       N-EANDRU SECTION.
      *A.  MOVE SPACE TO WB-BC.
      *    SUBTRACT WL FROM ET-LIN GIVING WZ-SCHALT.
      *    ADD WZ-SCHALT TO WL.
      *    MULTIPLY 3 BY WZ-SCHALT GIVING WH-ST1.
      *    MOVE 0 TO WZ-SCHALT.
      *    MOVE "3" TO WH-ST2.
      *    MOVE WH-ESC TO WC-LSL(1).
      *    IF ET-HOE = 1 ADD 09 TO WH-ST1
      *        MOVE WH-ESC TO WC-LSL(1)
      *        MOVE 0 TO WH-ST1
      *        MOVE "l" TO WH-ST2
      *        WRITE DRX-SATZ FROM WH-ESC AFTER 0
      *        MOVE TS-KURZBEZ TO DRX-SATZ(20:)
      *        MOVE KU-ANMERK TO DRX-SATZ(40:)
      *        WRITE DRX-SATZ
      *        GO G.
      *    WRITE DRX-SATZ FROM WB-BC BEFORE 1.
      *    MOVE 0 TO WH-ST1.
      *    MOVE "3" TO WH-ST2.
      *    MOVE WH-ESC TO WC-LSL(1).
      *    MOVE 33 TO WH-ST1.                               *> Modus 33
      *    MOVE "*" TO WH-ST2.
      *    MOVE WH-ESC TO WC-BIT11.
      *    MOVE 120 TO WC-LNG.
      *    MOVE 0 TO WH-ST1.
      *    MOVE "!" TO WH-ST2.
      *    MOVE WH-ESC TO WC-LSL(2).
      *    MOVE x"13" TO WC-CR.
      *    IF ET-HOE = 1 MOVE LOW-VALUE TO WN-MUSTER.
      *--------------------------------------*> linker Rand erster Teil
      **   MOVE 2 TO WH-ST1.
      **   MOVE "l" TO WH-ST2.
      **   MOVE WH-ESC TO WC-LSL(3).
      *    MOVE SPACE TO WC-LSL(3).
      *    MOVE WN-MUSTER TO WC-STR.
      *    MOVE 138 TO WC-LNG.
      *    WRITE DRX-SATZ FROM WB-BC AFTER 0.
      **   PERFORM ANZAHL.
      *    IF ET-HOE < 2 PERFORM ZAHLEN.         *> ohne Tourennummer
      *    SUBTRACT 1 FROM ET-HOE.
      **   ADD 6 TO ET-LIN.
      *    ADD 7 TO ET-LIN.                      *> öberlappung
      *    IF ET-HOE > 0 GO A.
      *G.  MOVE 0 TO WH-ST1.
      *    MOVE "2" TO WH-ST2.
      *    WRITE DRX-SATZ FROM WH-ESC AFTER 0.
      *    ADD 3 TO WZ-ZEILEN.
      *    MOVE X"1B210000" TO DRX-SATZ(1:).
      *    WRITE DRX-SATZ AFTER 0.
      *    MOVE 1 TO WZ-SCHALT.
      *    MOVE SPACE TO DRX-SATZ.
      *    MOVE WH-EAN TO WD-EAN.
      *    MOVE WD-EAN TO DRX-SATZ(4:).
      *    PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
      *ANZAHL SECTION.
      *A.  MOVE "l" TO WH-ST2.
      *    MOVE WH-ESC TO WC-LSL(3).
      *C.  PERFORM VARYING WC-LNG FROM 430 BY -1 UNTIL WC-LNG < 50
      *        or WC-X(WC-LNG) NOT = SPACE CONTINUE.
      *    IF WH-ST1 > 31 DIVIDE 3 INTO WC-LNG.
      *    WRITE DRX-SATZ FROM WB-BC AFTER 0.
      *Z.  EXIT.
      ***************************************** Telefonliste fÅr Excel *
       EXTEL SECTION.
       A.  MOVE "TELELIST.EXC" TO WH-DRUNAM.
           OPEN OUTPUT DRUCKER.
           ADD 520 VDU-ECK GIVING VDU-LP.
           MOVE LOW-VALUES TO KU-SATZ.
           MOVE WH-VNR TO KU-PREISE.
           MOVE "K" TO KU-SA.
           START KUNDEN KEY NOT < KU-PKEY INVALID GO X.
       C.  READ KUNDEN NEXT IGNORE LOCK AT END GO TO X.
           IF KU-SA NOT = "K" GO X.
           IF KU-PREISE > WH-BNR GO X.
           MOVE KU-KTONR TO DRZ-NR WD-PERSNR.
           MOVE KU-PREISE TO DRZ-GRP.
           DISPLAY WD-PERSNR AT VDU-LP " " KU-MCODE.
           INSPECT DRE-BEZ REPLACING ALL "#" BY SPACE.
           MOVE KU-MCODE TO DRZ-MCODE.
           MOVE KU-TEL TO DRZ-TELE.
           MOVE KU-BEZ TO DRZ-BEZ.
           INSPECT DRZ-BEZ REPLACING ALL "#" BY ";".
           MOVE ";" TO DRZ-TA DRZ-TB DRZ-TC DRZ-TD.
           WRITE DRZ-SATZ.
           GO C.
       X.  CLOSE DRUCKER.
       Z.  EXIT.
      ************************************** Listung Kd-Stat./Kd-Grp. *
       STATGRP SECTION.
       A.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Gruppe: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, < >= Preis-, < >= Statistikgruppen"
               AT 2301.
           DISPLAY "<ret-leer>= alle" AT 2401.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1003115005" WH-CREG.
           IF ESC GO Z.
           ADD 303 VDU-ECK GIVING VDU-LP.
           INITIALIZE KU-SATZ.
           IF AUF DISPLAY "Preisgruppe: " AT VDU-LP
                  MOVE "KDPREI00.LST" TO WH-DRUNAM
                  MOVE WH-WERT TO KU-PREISE
                  MOVE 0 TO WH-MOD
                  START KUNDEN KEY not < KU-SKEY INVALID GO Z
                  end-start.
           IF AB DISPLAY "Statistikgruppe: " AT VDU-LP
                  MOVE "KDSTAT00.LST" TO WH-DRUNAM
                  MOVE WH-WERT TO KU-STAT
                  MOVE 1 TO WH-MOD
                  START KUNDEN KEY not < KU-RKEY INVALID GO Z
                  end-start.
           MOVE WK-BS(1:2) TO WH-DRUNAM(7:2).
           IF not AUF and not AB GO A.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WM-OPEN WM-KUNR.
           IF WH-WERT = 0 DISPLAY "alle" with highlight AT 0000
           else MOVE WH-WERT TO WD-KTO WV-NR
                DISPLAY WD-KTO with highlight AT 0000.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO C.
       E.  READ KUNDEN NEXT IGNORE LOCK AT END GO W.
           IF KU-SA not = "K" GO E.
           IF WH-MOD = 0 MOVE KU-PREISE TO WH-VNR
                    else MOVE KU-STAT TO WH-VNR.
           IF WV-NR not = 0 and WV-NR not = WH-VNR GO W.
           IF WM-KUNR = 0 MOVE WH-VNR TO WM-KUNR.
           IF WV-NR = 0 and WH-VNR not = WM-KUNR
               MOVE 70 TO WZ-ZEILEN
               MOVE WH-VNR TO WM-KUNR.
           IF WZ-ZEILEN > 60 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WM-OPEN = 0; IF WM-DRU = 0 MOVE 5 TO WX-DR
                           else MOVE x"0100" TO WH-PX(1)
                                MOVE x"0008" TO WH-PX(2)
                           end-if
                           PERFORM BEG-DRU.
           IF WZ-ZEILEN > 0 GO K.
      *---------------------------------------------------> Kopfdruck <-
           IF WH-MOD = 0 MOVE "Kunden der Preisgruppe:" TO DRW-STR
                         MOVE WH-VNR TO WD-KTO
                         MOVE WD-KTO TO DRW-STR(25:)
                    else MOVE "Kunden der Statistikgruppe:" TO DRW-STR
                         MOVE WH-VNR TO WD-KTO
                         MOVE WD-KTO TO DRW-STR(29:).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRW-STR(65:).
           MOVE "/ Seite:" TO DRW-STR(75:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRW-STR(84:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRW-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       K.  MOVE KU-KTONR TO DRW-KTONR.
           MOVE KU-BEZ TO DRW-BEZ.
           INSPECT DRW-BEZ REPLACING ALL "#" BY ",".
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY DRW-BEZ(1:50) AT VDU-LP.
           IF WH-MOD = 0 MOVE KU-STAT TO WD-KTO
                    else MOVE KU-PREISE TO WD-KTO.
           MOVE WD-KTO TO DRW-GRP.
           PERFORM DRUCK.
           GO E.
       W.  IF WM-OPEN = 1 and WZ-ZEILEN < 60
               MOVE ALL "ƒ" TO DRW-STR PERFORM DRUCK.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       T-SPIEGEL SECTION.
       A.  INITIALIZE KU-SATZ.
           MOVE "E(10U&l26a8c0O(s0p12.5h15v0b0s3T" TO WE-STG(9).
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WM-OPEN.
           MOVE LOW-VALUES TO WT-TABL.
           MOVE "TOURSPxx.LST" TO WH-DRUNAM.
           MOVE WK-BS(1:2) TO WH-DRUNAM(7:2).
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kd.Grp.: " AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "   Tour: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Kundengruppe" AT 2301.
           CALL "CAUP" USING "1003126006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           IF WH-WERT = 0 GO A.
           MOVE WH-WERT TO WD-NR WT-KDGRP(1).
           DISPLAY WD-NR with highlight AT VDU-LP.
       C.  DISPLAY "<ret-leer>= Ende, < >= bis Kd.Grp., <ret>= weitere K
      -        "undengrp." AT 2401.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1003196006" WH-CREG.
           IF ESC GO Z.
           IF not RET and not WORE GO C.
           IF WH-WERT = 0 GO G.
           MOVE WH-WERT TO WD-NR WT-KDGRP(2).
           IF WORE ADD 319 VDU-ECK GIVING VDU-LP
               DISPLAY "bis Kd.Grp.: " AT VDU-LP
               ADD 13 TO VDU-LP
               MOVE 1 TO WT-KDGRP(21)
               DISPLAY WD-NR with highlight AT VDU-LP GO G.
           MOVE 2 TO WX.
           PERFORM GRPTAB.
           IF ESC GO Z.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kd.Grp.:          " AT VDU-LP.
           ADD 312 VDU-ECK GIVING VDU-LP.
           MOVE WT-KDGRP(1) TO WD-NR.
           DISPLAY WD-NR with highlight AT VDU-LP " (Tabelle)".
       G.  DISPLAY "<ret-leer>= ab Anfang, <esc/Ende>= Ende" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1004123003" WH-CREG.
           IF ENDE SET ESC TO TRUE.
           IF ESC GO Z.
           IF NOT RET GO G.
           IF WH-WERT = 0 MOVE 1 TO WH-WERT.
           MOVE WH-WERT TO WH-VNR WD-NR WT-TOUR(1).
           DISPLAY WD-NR with highlight AT VDU-LP.
           PERFORM READ-TOUR.
           IF ESC GO G.
       I.  DISPLAY "<ret-leer>= Ende, < >= bis Tour, <ret>= weitere Tour
      -        "en" AT 2401.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1004196006" WH-CREG.
           IF ESC GO Z.
           IF not RET and not WORE GO C.
           IF WH-WERT = 0 GO J.
           INITIALIZE TO-SATZ.
           MOVE WH-WERT TO WD-NR WT-TOUR(2).
           IF WORE ADD 419 VDU-ECK GIVING VDU-LP
               DISPLAY "bis Tour: " AT VDU-LP
               ADD 13 TO VDU-LP
               MOVE 1 TO WT-TOUR(21)
               DISPLAY WD-NR with highlight AT VDU-LP GO J.
           MOVE 2 TO WX.
           PERFORM TOURTAB.
           IF ESC GO Z.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "   Tour:          " AT VDU-LP.
           ADD 9 TO VDU-LP.
           MOVE WT-TOUR(1) TO WD-NR.
           DISPLAY WD-NR with highlight AT VDU-LP " (Tabelle)".
      *---------------------------------------------> Tabellenausgabe <-
       J.  MOVE 0 TO WL.
           MOVE VDU-ECK TO VDU-AB.
           IF WT-KDGRP(21) = 1 GO K.
           MOVE 1 TO WL.
           CALL "CAUP" USING "07055918100000" WH-CREG.
           ADD 102 VDU-ECK GIVING VDU-LP.
           DISPLAY " Gruppe " with highlight AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               IF WT-KDGRP(WX) not = 0 MOVE WT-KDGRP(WX) TO WD-NR
                   COMPUTE VDU-LP = WX * 100 + 102 + VDU-ECK
                   DISPLAY WD-NR with highlight AT VDU-LP.
       K.  IF WT-TOUR(21) = 1 GO L.
           ADD 1 TO WL.
           CALL "CAUP" USING "07057018100000" WH-CREG.
           ADD 102 VDU-ECK GIVING VDU-LP.
           DISPLAY " Touren " with highlight AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               IF WT-TOUR(WX) not = 0 MOVE WT-TOUR(WX) TO WD-NR
                   COMPUTE VDU-LP = WX * 100 + 102 + VDU-ECK
                   DISPLAY WD-NR with highlight AT VDU-LP.
       L.  MOVE VDU-AB TO VDU-ECK.
           DISPLAY "<esc>= Abbruch, <#/ret>= Start < >" AT 2301.
           DISPLAY "<#> statt <ret>= sortiert nach Tourfolge" AT 2401.
           CALL "CAUP" USING "0023330000" WH-CREG.
           MOVE 0 TO WH-MOD.
           IF ESC GO Z.
           IF KIST SET RET TO TRUE
                   COMPUTE WM-KZ = WT-TOUR(1) / 100
                   PERFORM VARYING WX FROM 2 BY 1 UNTIL WX > 15
                       IF WT-TOUR(WX) not = 0
                           COMPUTE WV-KZ = WT-TOUR(WX) / 100
                           IF WM-KZ not = WV-KZ exit perform GO L
                   end-perform
                   MOVE 1 TO WH-MOD
                   DELETE FILE SORTIER
                   OPEN I-O SORTIER WITH LOCK
                   IF WF-STATUS > "09" GO L.
           IF not RET GO K.
           MOVE SPACE TO DRA-SATZ.
           MOVE 1 TO WS.
           MOVE WT-KDGRP(WS) TO KU-PREISE WV-NR.
           MOVE 0 TO KU-BAN.
           OPEN INPUT TOUREN.
           IF WV-NR = 0 GO S.
           START KUNDEN KEY not < KU-FILKEY INVALID GO A.
       M.  READ KUNDEN NEXT IGNORE LOCK AT END GO S.
      *--------------------------------------> Kundengruppen abfragen <-
           IF WT-KDGRP(21) = 1 and KU-PREISE > WT-KDGRP(2) GO S.
           IF WT-KDGRP(21) = 0;
               IF WV-NR not = KU-PREISE and WH-MOD = 0 go s.

           IF WT-KDGRP(21) = 0;
               IF WV-NR not = KU-PREISE and WH-MOD = 1
                   ADD 1 TO WS
                   MOVE WT-KDGRP(WS) TO KU-PREISE WV-NR
                   IF KU-PREISE not = 0 MOVE 0 TO KU-BAN
                       START KUNDEN KEY not < KU-FILKEY INVALID GO S
                       end-start GO M
                   end-if
                GO S.
           INITIALIZE TO-SATZ.
           MOVE KU-KTONR TO TO-KUNR TO-KUNR.
           MOVE 0 TO TO-NR TO-LFD.
           START TOUREN KEY not < TO-BKEY INVALID GO A.
       O.  READ TOUREN NEXT IGNORE LOCK AT END GO S.
           IF KU-KTONR not = TO-KUNR;
               IF DRA-SATZ not = SPACE PERFORM DRUCK
               end-if
               GO M.
      *---------------------------------------------> Touren abfragen <-
           IF WT-TOUR(21) = 1;
               IF TO-NR > WT-TOUR(2) or TO-NR < WT-TOUR(1) GO O.
           IF WT-TOUR(21) = 0;
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 15
                   IF WT-TOUR(WY) = TO-NR exit perform
               end-perform
               IF WY = 16 GO O.
           MOVE LOW-VALUE TO TS-SATZ.
           MOVE TO-NR TO TS-NR.
           READ S-TOUR IGNORE LOCK INVALID GO O.
           IF WH-MOD not = 1 PERFORM TSPIKOPF.
           MOVE KU-PREISE TO PR-NR.
           MOVE 0 TO PR-ARTNR.
           MOVE "P" TO PR-SA.
           READ PREISLI IGNORE LOCK INVALID NEXT SENTENCE.
           IF KU-BAN > 99999 COMPUTE DRH-FILNR = KU-BAN / 10
                        else MOVE KU-BAN TO DRH-FILNR.
           MOVE PR-BEZ TO DRH-GRUPPE.
           MOVE KU-BEZ TO DRH-BEZ.
           MOVE KU-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           PERFORM VARYING WX FROM 6 BY -1 UNTIL WX = 1
               IF WR-ADR(WX) not = SPACE exit perform.
           MOVE WR-ADR(WX) TO DRH-BEZ.
           IF DRH-BEZ(6:1) = SPACE MOVE DRH-BEZ(7:) TO DRH-BEZ(6:).
           ADD -1 TO WX.
           PERFORM VARYING WZ FROM 45 BY -1 UNTIL WZ = 1
               IF DRH-BEZ(WZ:1) not = SPACE
                   MOVE WR-ADR(WX) TO DRH-BEZ((WZ + 2):)
                   exit perform.
           COMPUTE WX = TO-NR / 100.
           MOVE TO-LFD TO DRH-TONR(WX).
           MOVE TS-ALKEY TO DRH-TONR(WX)(1:3).
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY DRH-BEZ(1:45) AT VDU-LP.
           PERFORM VARYING ZX FROM 39 BY -1 UNTIL ZX = 1
               IF DRH-BEZ(ZX:1) not = SPACE exit perform
               else MOVE "_" TO DRH-BEZ(ZX:1)
               end-perform
               ADD 1 TO ZX
               IF ZX < 40 MOVE SPACE TO DRH-BEZ(ZX:1).
           IF WH-MOD = 0 GO O.
      *---------------------------------------> Sortierdatei aufbauen <-
           MOVE TO-NR TO SR-NR.
           MOVE TO-LFD TO SR-LFD.
           IF KU-BAN > 99999 COMPUTE SR-FILNR = KU-BAN / 10
                        else MOVE KU-BAN TO SR-FILNR.
           MOVE PR-BEZ TO SR-GRUPPE.
           MOVE DRH-BEZ TO SR-BEZ.
           MOVE KU-KTONR TO SR-KUNR.
           MOVE TS-ALKEY TO SR-KBEZ.
           WRITE SR-SATZ.
           MOVE SPACE TO DRA-SATZ.
           GO M.
       Q.  INITIALIZE SR-SATZ.
           START SORTIER KEY > SR-KEY INVALID GO T.
       R.  READ SORTIER NEXT AT END GO T.
           PERFORM TSPIKOPF.
           MOVE SR-FILNR TO DRH-FILNR.
           MOVE SR-GRUPPE TO DRH-GRUPPE.
           MOVE SR-BEZ TO DRH-BEZ.
           COMPUTE WX = SR-NR / 100.
           MOVE SR-KBEZ TO DRH-KBEZ(WX).
           MOVE SR-LFD TO DRH-LFD(WX).
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY DRH-BEZ(1:45) with foreground-color 3 AT VDU-LP.
           PERFORM DRUCK.
           GO R.
       S.  IF WH-MOD = 1 GO Q.
           IF WT-KDGRP(21) = 1 GO T.
           ADD 1 TO WS.
           IF WT-KDGRP(WS) = 0 GO T.
           MOVE WT-KDGRP(WS) TO KU-PREISE.
           MOVE 0 TO KU-BAN.
           START KUNDEN KEY not < KU-FILKEY INVALID GO S.
           GO M.
       T.  IF WL not = 0 ADD -1 TO WL
               CALL "CAUP" USING "08CLOFEN" WH-CREG
               GO T.
           IF WH-MOD = 1 CLOSE SORTIER.
       W.  CLOSE TOUREN.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************************************** Tabellenaufstellung *
       GRPTAB SECTION.
       A.  CALL "CAUP" USING "07047018100000" WH-CREG.
           ADD 102 VDU-ECK GIVING VDU-LP.
           DISPLAY " Gruppe " with highlight AT VDU-LP.
       C.  MOVE WH-WERT TO WT-KDGRP(WX).
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               IF WT-KDGRP(WX) not = 0 MOVE WT-KDGRP(WX) TO WD-NR
                   COMPUTE VDU-LP = WX * 100 + 102 + VDU-ECK
                   DISPLAY WD-NR with highlight AT VDU-LP
               else exit perform.
       G.  COMPUTE VDU-LP = WX * 100 + 102.
           MOVE 0 TO WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret-leer>= Ende" AT 2301.
           CALL "CAUP" USING "1000006006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO G.
           IF WH-WERT = 0 GO X.
           IF WX > 15 GO G.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 15
               IF WH-WERT = WT-KDGRP(WY) DISPLAY "ACHTUNG!" with
                    highlight AT 2401 " doppelt" AT 0000
                    PERFORM WEITER GO G.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************** Tabellenaufstellung *
       TOURTAB SECTION.
       A.  CALL "CAUP" USING "07047018100000" WH-CREG.
           ADD 102 VDU-ECK GIVING VDU-LP.
           DISPLAY " Touren " with highlight AT VDU-LP.
       C.  MOVE WH-WERT TO WT-TOUR(WX).
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               IF WT-TOUR(WX) not = 0 MOVE WT-TOUR(WX) TO WD-NR
                   COMPUTE VDU-LP = WX * 100 + 102 + VDU-ECK
                   DISPLAY WD-NR with highlight AT VDU-LP
               else exit perform.
       G.  COMPUTE VDU-LP = WX * 100 + 102.
           MOVE 0 TO WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret-leer>= Ende" AT 2301.
           CALL "CAUP" USING "1000006006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO G.
           IF WH-WERT = 0 GO X.
           IF WX > 15 GO G.
           PERFORM READ-TOUR.
           IF ESC GO G.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 15
               IF WH-WERT = WT-TOUR(WY) DISPLAY "ACHTUNG!" with
                    highlight AT 2401 " doppelt" AT 0000
                    PERFORM WEITER GO G.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** ob Tour vorhanden *
       READ-TOUR SECTION.
       A.  MOVE WH-WERT TO TS-NR.
           MOVE 0 TO TS-LFD.
           READ S-TOUR IGNORE LOCK INVALID SET ESC TO TRUE GO Z.
       Z.  EXIT.
      ********************************************** Tourenspiegelkopf *
       TSPIKOPF SECTION.
       A.  IF WM-OPEN = 0 MOVE x"0100" TO WH-PX(1)
                          MOVE x"000C" TO WH-PX(2)
                          IF WM-DRU = 0 MOVE 9 TO WX-DR
                          end-if
                          PERFORM BEG-DRU.
           IF WZ-ZEILEN > 61 MOVE 0 TO WZ-ZEILEN
               WRITE DRA-SATZ AFTER PAGE.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE "Tourenspiegel" TO DRI-STR.
           MOVE "Johann TITZ GesmbH., 8330 Rohr/R" TO DRI-STR(23:).
           ADD 1 TO WZ-SEITE.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRI-DATUM.
           MOVE "/" TO DRI-SST.
           MOVE WZ-SEITE TO DRI-SEITE.
           PERFORM DRUCK.
           MOVE "Kd.Grp.:" TO DRI-STR.
           MOVE WT-KDGRP(1) TO WD-ARTNR.
           MOVE WD-ARTNR TO DRI-STR(10:).
           IF WT-KDGRP(21) = 1 MOVE WT-KDGRP(2) TO WD-ARTNR
               MOVE "-" TO DRI-STR(16:)
               MOVE WD-ARTNR TO DRI-STR(18:)
               MOVE 24 TO WI
           else MOVE 15 TO WI
               PERFORM VARYING WY FROM 2 BY 1 UNTIL WY > 11
                   IF WT-KDGRP(WY) not = 0 MOVE WT-KDGRP(WY) TO WD-ARTNR
                      MOVE "," TO DRI-STR(WI:1)
                      ADD 2 TO WI
                      MOVE WD-ARTNR TO DRI-STR(WI:)
                      ADD 5 TO WI.
           ADD 3 TO WI.
      *-------------------------------------------------> Tourenliste <-
           PERFORM VARYING WI FROM 92 BY -1 UNTIL WI = 18
               IF DRI-STR(WI:1) not = SPACE exit perform.
           ADD 3 TO WI.
           MOVE "Tour:" TO WT-TX.
           MOVE WT-TOUR(1) TO WD-TONR.
           MOVE WD-TONR TO WT-TX(7:).
           IF WT-TOUR(21) = 1 MOVE "-" TO WT-TX(12:)
                MOVE WT-TOUR(2) TO WD-TONR
                MOVE WD-TONR TO WT-TX(15:)
                MOVE 18 TO WZ
           else MOVE 11 TO WZ
               PERFORM VARYING WY FROM 2 BY 1 UNTIL WY > 15
                   IF WT-TOUR(WY) not = 0 MOVE WT-TOUR(WY) TO WD-TONR
                      MOVE "," TO WT-TX(WZ:1)
                      ADD 2 TO WZ
                      MOVE WD-TONR TO WT-TX(WZ:)
                      ADD 3 TO WI.
           IF (WZ + WI) > 93 PERFORM DRUCK
                             MOVE WT-TX TO DRI-STR(1:)
                        else MOVE WT-TX TO DRI-STR(WI:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRI-STR PERFORM DRUCK.
           MOVE "Fil. Gruppe B e z e i c h n u n g
      -         "Mo     Di     Mi     Do     Fr     Sa" TO DRI-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRI-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       RASTRI SECTION.
       A.  IF WM-DRU = 1 GO Z.
           IF WR = 1 MOVE 0 TO WR GO Z.
           MOVE 1 TO WR.
           MOVE "&f0S*p-01X*p+16Y*c3050a48b15g2P&f1S" TO DRD-STR.
           COMPUTE WZ-GROSS = WZ-BREIT / 12 * 300.
           MOVE WZ-GROSS TO DRD-STR(23:4).
       Q.  MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       RAST SECTION.
       A.  IF WM-DRU = 1 GO Z.
           IF WR = 1 MOVE 0 TO WR GO Z.
           MOVE 1 TO WR.
           MOVE "&f0S*p-01X*p+16Y*c3050a48b15g2P&f1S"
               TO DRD-STR(7:).
           COMPUTE WZ-GROSS = WZ-BREIT / 12 * 300.
           MOVE WZ-GROSS TO DRD-STR(29:4).
       Q.  MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
       Z.  EXIT.
