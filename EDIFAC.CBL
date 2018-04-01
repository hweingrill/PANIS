      $SET LINKCOUNT "640" ANS85 BOUND AUTOLOCK NOALTER
      $SET SEGSIZE(56000)
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TIEDIFAC.
      *----------------------------------> DatenÅbernahme aus EDIFACT <-
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    PC.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EDIFACT  ASSIGN TO WN-EDIFAC
                           ORGANIZATION IS LINE SEQUENTIAL
                           FILE STATUS IS WF-STATUS.
           SELECT EDISICH  ASSIGN TO WN-EDISIC
                           ORGANIZATION IS LINE SEQUENTIAL
                           FILE STATUS IS WF-STATUS.
       COPY SE-TOUR.CPY.
       COPY SE-ARTIK.CPY.
       COPY SE-KONST.CPY.
       COPY SE-PREIS.CPY.
       COPY SE-KUND.CPY.
       COPY SE-LIEF.CPY.
       COPY SE-SBEST.CPY.
       COPY SE-SEDAS.CPY.
       COPY SE-KONTR.CPY.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           FILE STATUS IS WF-STATUS.
           SELECT BATFILE  ASSIGN TO WH-BATNAM
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS IS WF-STATUS.
           SELECT BESTDRU  ASSIGN TO PRINTER WB-DRUNAM
                           FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FD-TOUR.CPY.
       COPY FD-ARTIK.CPY.
       COPY FD-KONST.CPY.
       COPY FD-PREIS.CPY.
       COPY FD-KUND.CPY.
       COPY FD-LIEF.CPY.
       COPY FD-SBEST.CPY.
       COPY FD-SEDAS.CPY.
       COPY FD-KONTR.CPY.
      ******************************************************************
       FD  BATFILE                     LABEL RECORDS OMITTED.
       01  BTF-SATZ                    PIC X(80).
      ******************************************************************
       FD  BESTDRU                     LABEL RECORDS OMITTED.
       01  BDA-SATZ.
           03  FILLER                  PIC XX.
           03  BDA-BEZ                 PIC X(30).
           03  FILLER                  PIC XX.
           03  BDA-GLN                 PIC 9(13).
           03  BDA-KDNUM               PIC ZZZ.ZZ9-.
           03  BDA-BNUM                PIC Z(12).
           03  FILLER                  PIC XX.
           03  BDA-BESTDAT             PIC X(12).
           03  BDA-LIEFDAT             PIC X(12).
           03  FILLER                  PIC X(14).
       01  DBD-SATZ.
           03  DBD-POS                 PIC ZZZZ.ZZ9-.
           03  FILLER                  PIC X.
           03  DBD-ARNUM               PIC ZZ.ZZ9,99-.
           03  DBD-BEZ                 PIC X(28).
           03  DBD-MG                  PIC ZZ9,99-.
           03  FILLER                  PIC X.
           03  DBD-MEH                 PIC XXX.
           03  FILLER                  PIC X(3).
           03  DBD-EAN                 PIC 9(13).
           03  FILLER                  PIC XX.
           03  DBD-BESNR               PIC X(18).
       01  DBG-SATZ.
           03  FILLER                  PIC X(6).
           03  DBG-STR.
               05 DBG-KTONR            PIC ZZ.ZZ9-.
               05 DBG-KUBEZ            PIC X(17).
               05 DBG-DHLLFS           PIC X(13).
               05 DBG-LFSNR            PIC ZZZ.ZZ9-.
               05 DBG-LFDAT            PIC X(9).
               05 DBG-ORDNR            PIC ZZZ9-.
               05 DBG-ARNUM            PIC ZZZZ9,99-.
               05 DBG-EAN              PIC X(13)B.
               05 DBG-MG               PIC ZZZ9,99-.
               05 DBG-RMG REDEFINES
                  DBG-MG.
                  07 DBG-ANZ           PIC ZZZ9-.
                  07 FILLER            PIC XXX.
               05 DBG-BANZ             PIC Z.ZZ9-.
               05 DBG-MEH              PIC XXX.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORDS OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(4).
           03  DRA-KTONR               PIC ZZZ.ZZ9-.
           03  DRA-MCODE               PIC X(30).
           03  DRA-BETX                PIC X(11).
           03  DRA-BEDAT               PIC X(10).
           03  DRA-LFTX                PIC X(11).
           03  DRA-LFDAT               PIC X(12).
           03  DRA-LEER                PIC X(40).
       01  DRC-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRC-STR.
               05 DRC-EANK             PIC 9(13).
               05 FILLER               PIC X.
               05 DRC-EANA             PIC 9(13).
               05 FILLER               PIC X.
               05 DRC-BEZ              PIC X(25).
               05 DRC-MENGE            PIC ZZ.ZZZ,ZZ-.
               05 DRC-ANMERK           PIC X(27).
       01  DRB-SATZ.
           03  FILLER                  PIC X(4).
           03  DRB-ARNUM               PIC ZZ.ZZ9,99-.
           03  DRB-BEZ                 PIC X(28).
           03  DRB-MG                  PIC ZZ9,99-.
           03  FILLER                  PIC X(3).
           03  DRB-EAN                 PIC 9(13).
           03  FILLER                  PIC XX.
           03  DRB-BESNR               PIC X(16).
       01  DRF-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRF-STR.
               05 DRF-LFSNR            PIC ZZZ.ZZ9-.
               05 DRF-KTONR            PIC ZZ.ZZ9-.
               05 DRF-ARNUM            PIC ZZZ9-.
               05 DRF-MENGE            PIC ZZZ9,99-.
               05 DRF-PREIS            PIC ZZZ9,99-.
               05 DRF-EANK             PIC 9(14).
               05 DRF-EANA             PIC 9(14).
               05 DRF-LFSDHL           PIC X(16).
       01  DRR-SATZ.
           03  FILLER                  PIC X(4).
           03  DRR-KTONR               PIC ZZZ.ZZ9-.
           03  DRR-MCODE               PIC X(30).
           03  DRR-LFSNR               PIC X(11).
           03  DRR-LFSNUM              PIC X(10).
           03  DRR-LFTX                PIC X(11).
           03  DRR-LFDAT               PIC X(12).
      ******************************************************************
       FD  EDIFACT                     LABEL RECORD STANDARD
                                       RECORD 1 TO 4096.
       01  EDI-SATZ.
           03  EDI-Z                   PIC X    OCCURS 4096.
      ******************************************************************
       FD  EDISICH                     LABEL RECORD STANDARD
                                       RECORD 1 TO 128.
       01  EDS-SATZ                    PIC X(128).
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WS-REG.
           03  WH-ANZPOS               PIC 9999       COMP.
           03  WH-FELD                 PIC X(35).
           03  WR-D REDEFINES WH-FELD.
               05 WR-DAT               PIC 9(8).
               05 WR-ZEIT              PIC 9(4).
               05 FILLER               PIC X(23).
           03  WR-E REDEFINES WH-FELD.
               05 WR-EANR              PIC 9(13).
               05 FILLER               PIC X(22).
           03  WK-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES
               WK-EAN                  PIC 9      OCCURS 13.
           03  WS-PLMG                 PIC S9(6)V99 COMP.
           03  WD-MENGE                PIC X(10).
           03  WH-ANZ                  PIC S9(5)V99.
           03  WD-ANZG                 PIC ZZ.ZZ9-.
           03  WD-ANZ2                 PIC ZZ.ZZ9,99-.
           03  WH-NK                   PIC 99.
           03  WR-KK REDEFINES WH-NK.
               05 WR-KI                PIC 9.
               05 WR-NK                PIC 9.
           03  WH-MOD                  PIC 99       COMP.
           03  WM-MAX                  PIC 999      COMP.
           03  WQ                      PIC 999      COMP   VALUE 0.
           03  WS                      PIC 999      COMP   VALUE 0.
           03  WM-LOEKO                PIC 99       COMP.
           03  WH-BK-SATZ              PIC X(32).
           03  VDU-ZEIT.
               05 VDU-STD              PIC Z9.
               05 VDU-DD               PIC X.
               05 VDU-MIN              PIC 99.
           03  WM-ZL                   PIC 99       VALUE 0.
           03  WH-LFD-01               PIC 9(4)     VALUE 0.
           03  WH-LFD-03               PIC 9(5)     VALUE 0.
           03  WH-LFD-04               PIC 9(5)     VALUE 0.
           03  WA-TAGE                 PIC 9(8)       COMP.
           03  WZ-SEQ                  PIC 9(5)            VALUE 0.
           03  WZ-DUP                  PIC 9(5).
           03  WD-ARTNR                PIC ZZZZZ,ZZ.
           03  KT-KEY                  PIC 9(3).
           03  WH-BEST-KEY             PIC 9(6)     VALUE 0.
           03  WH-BATNAM               PIC X(24).
           03  WH-DRUNAM               PIC X(20).
           03  WB-DRUNAM               PIC X(20).
           03  WM-OPEN                 PIC 9          COMP.
           03  WB-OPEN                 PIC 9          COMP.
           03  WM-KOPF                 PIC 9          COMP.
           03  WZ-SEITE                PIC S9(5)      COMP.
           03  WZ-ZEILEN               PIC S999V9     COMP.
           03  WZ-SCHALT               PIC S999       COMP.
           03  WB-ZEILEN               PIC S999V9     COMP.
           03  WB-SCHALT               PIC S999       COMP.
           03  WB-SEITE                PIC S9(5)      COMP.
           03  WD-SEITE                PIC ZZ9.
           03  WD-NUM                  PIC ZZ.ZZ9.
           03  WD-ARNUM                PIC ZZ.ZZ9,99.
           03  WK-ARNUM                PIC 9(5)V99.
                          88 STRAUS    VALUE 9740 THRU 9749.
           03  WH-BGM                  PIC X(16).
           03  WH-DTM                  PIC 9(8).
           03  WH-LFDAT                PIC 9(8).
           03  WN-EDIFAC               PIC X(50).
           03  WN-EDISIC               PIC X(16).
           03  WN-SICH                 PIC X(30).
           03  WH-FTPERL               PIC 9.
           03  WH-DP                   PIC 9(13).
           03  WH-BY                   PIC 9(13).
           03  WH-OB                   PIC 9(13).
           03  WH-IV                   PIC 9(13).
           03  WH-UC                   PIC 9(13).
           03  WH-CN                   PIC 9(13).
           03  WH-SU                   PIC 9(13).
           03  WH-EMPF                 PIC 9(13).
           03  WH-EAN                  PIC 9(13).
           03  WT-KUND                 PIC X(320).
           03  WT-ART                  PIC X(128).
           03  WP-POS                  PIC 9(3).
           03  WU-Z                    PIC X.
           03  WU-SEG                  PIC XXX.
           03  WU-QUAL                 PIC XXX.
           03  WU-POS                  PIC 9(6).
           03  WU-ANZ                  PIC 9(4).
           03  WU-GVA                  PIC XXX.
           03  WU-MEH                  PIC XXX.
           03  WU-GNR                  PIC XXXX.
           03  WU-BEST                 PIC X(26).
           03  WU-TEXT                 PIC X(30).
           03  WU-ARTNR                PIC 9(4).
           03  WU-EAN                  PIC 9(14).
           03  WU-BNUM                 PIC 9(10).
           03  WU-LIEFDAT              PIC 9(8).
           03  WU-BESTDAT              PIC 9(8).
           03  WU-GLN                  PIC 9(13).
           03  WU-BGLN                 PIC 9(13).
           03  WU-KUTX                 PIC X(30).
           03  WX-BNUM                 PIC 9(5).
           03  WU-RFFUC.
               05 WR-KTONR             PIC 9(5).
               05 WR-ORDNR             PIC 9(6).
           03  WQ-DAT                  PIC 9(8).
           03  ZA                      PIC 9999    COMP.
           03  IC                      PIC 9999    COMP.
           03  IQ                      PIC 9999    COMP.
           03  IP                      PIC 9999    COMP.
           03  WT-GRP.
               05 WT-EDI               PIC X      OCCURS 100.
           03  WH-MG                   PIC 99999V999.
           03  WX-MG                   PIC X(8).
           03  WH-TIME                 PIC 9(8).
           03  WD-TIME                 PIC 99B99.
           03  WD-KDNUM                PIC 99999.
           03  WD-KZ                   PIC 9.
           03  WX-KZ                   PIC X.
           03  WC-KZ REDEFINES
               WX-KZ                   PIC X       COMP-X.
           03  WT-BTAB.
               05 WT-BARNUM            PIC 9(6)      COMP   OCCURS 25.
               05 WT-BBEZ              PIC X(26)            OCCURS 25.
               05 WT-MG                PIC 9(6)V99   COMP   OCCURS 25.
               05 WT-BMG               PIC 9(6)      COMP   OCCURS 25.
               05 WT-ANZ               PIC 9(6)      COMP   OCCURS 25.
               05 WT-STKVER            PIC 9         COMP   OCCURS 25.
               05 WT-BEAN              PIC X(13)            OCCURS 25.
      *--------------------------------------------> check_file_exist <-
           03  WN-SEDAS                PIC X(20).
           03  WN-BEST                 PIC X(45).
           03  WM-BENR                 PIC X(16).
           03  WM-BEDAT                PIC 9(6)    COMP.
           03  WD-SIZE                 PIC ZZ.ZZZ.ZZ9.
           03  WH-GESF.
               05  WH-DIR              PIC X(9).
               05  WH-FNAME            PIC X(13).
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)    COMP-X.
               05 WF-TT                PIC 99      COMP-X.
               05 WF-MO                PIC 99      COMP-X.
               05 WF-YY                PIC 9999    COMP-X.
               05 WF-HH                PIC 99      COMP-X.
               05 WF-MM                PIC 99      COMP-X.
               05 WF-SS                PIC 99      COMP-X.
               05 WF-HS                PIC 99      COMP-X.
      *-------------------------------------------- Byte-Stream-Files <-
           03  WN-EDICUS               PIC X(30).
           03  WB-SATZ                 PIC X(160).
           03  WB-AC                   PIC X     COMP-X.
           03  WB-DE                   PIC X     COMP-X.
           03  WB-DV                   PIC X     COMP-X.
           03  WB-FH                   PIC X(4).
           03  WB-OFS                  PIC X(8)  COMP-x.
           03  WB-CNT                  PIC X(4)  COMP-x.
           03  WB-FLG                  PIC X     COMP-x.
      *--------------------------------------------> Dateien auslesen <-
           03  WL-FUNKT                PIC X       COMP-X   VALUE 69.
           03  WL-PARM.
               07 WL-ACT               PIC X       COMP-X.
               07 WL-ATTRIB            PIC X       COMP-X.
               07 WL-FILEIN            PIC X(50).
      *------------------------------------------------> Command-Line <-
           03  WD-UPON                 PIC X(60).
           03  RESULT                  PIC 99      COMP-X.
           03  FUNKT                   PIC 99      COMP-X  VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99      COMP-X  VALUE 0.
               05 PAR                  PIC X(40)   VALUE SPACE.
      *---------------------------------------------> Directory lesen <-
           03  WL-RESULT.
               05 WL-ERR               PIC X       COMP-X.
               05 WL-HANDLE            PIC XX      COMP-X.
               05 WL-ATTROUT           PIC X       COMP-X.
               05 WL-TIME              PIC XX      COMP-X.
               05 WL-DATE              PIC XX      COMP-X.
               05 WL-SIZE              PIC X(4)    COMP-X.
               05 WL-FOUT.
                  07 WL-FNAM           PIC X(9).
                  07 WL-FIL            PIC 999.
                  07 WL-REST           PIC X(5).
       01  DT-SATZ.
           03  DT-DATE                 PIC 9(8)    COMP.
           03  DT-TIME                 PIC 9(6)    COMP.
           03  DT-NAME                 PIC X(14).
           03  DT-SIZE                 PIC Z(8).
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
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON BESTKOPF.
       A.  CALL "CADECL" USING "TIBEKOPF.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON BESTPOS.
       A.  CALL "CADECL" USING "TIBEPOSI.DAT" WH-CREG.
       DECL-BA SECTION.        USE AFTER ERROR PROCEDURE ON SIBEKOPF.
       A.  CALL "CADECL" USING "S:TIBEKOPF" WH-CREG.
       DECL-CA SECTION.        USE AFTER ERROR PROCEDURE ON SIBEPOS.
       A.  CALL "CADECL" USING "S:TIBEPOS" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON KUNDEN.
       A.  CALL "CADECL" USING "TIKUNDEN.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "TIKONST.DAT " WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON LBEWEG.
       A.  CALL "CADECL" USING "TILBEWEG.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON LKOPF.
       A.  CALL "CADECL" USING "TILKOPF.DAT " WH-CREG.
       DECL-FA SECTION.        USE AFTER ERROR PROCEDURE ON SLBEWEG.
       A.  CALL "CADECL" USING "S:TILBEW.DAT" WH-CREG.
       DECL-GA SECTION.        USE AFTER ERROR PROCEDURE ON SLKOPF.
       A.  CALL "CADECL" USING "S:TILKOP.DAT " WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON EDIFACT.
       A.  CALL "CADECL" USING WN-EDIFAC  WH-CREG.
       Z.  EXIT.
       D-K SECTION.            USE AFTER ERROR PROCEDURE ON SEDAS.
       A.  CALL "CADECL" USING "BESTELL.DAT " WH-CREG.
       DECL-T SECTION.         USE AFTER ERROR PROCEDURE ON TOUREN.
       A.  CALL "CADECL" USING "TITOUREN.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           OPEN I-O BESTKOPF BESTPOS TOUREN.
           OPEN I-O SIBEKOPF SIBEPOS.
           OPEN INPUT PREISLI KALIBER KNOTEN.
           MOVE "   Bestell-öbernahme" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           EVALUATE WL-CA
               WHEN 10 PERFORM ORDERS-EINLESEN
                       PERFORM EMAIL-EINLESEN
                       PERFORM EDI-RECADV
               WHEN 20 PERFORM EDI-ORDERS
               WHEN 30 PERFORM DHL-DESADV.
       X.  MOVE "TIBEST  10BEST" TO WT-TX.
           CLOSE BESTKOPF BESTPOS TOUREN.
           CLOSE SIBEKOPF SIBEPOS.
           CLOSE PREISLI KALIBER KNOTEN.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************* Drucker *
       B-DRUCK SECTION.
       A.  WRITE BDA-SATZ AFTER WB-SCHALT.
           MOVE SPACE TO BDA-SATZ.
           ADD WB-SCHALT TO WB-ZEILEN.
           MOVE 1 TO WB-SCHALT.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  WRITE DRA-SATZ AFTER WZ-SCHALT.
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
      ******************************************************************
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE "E" TO DRA-SATZ(1:).
           WRITE DRA-SATZ AFTER 0.
       C.  MOVE 0 TO WM-OPEN WZ-ZEILEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ******************************************************************
       END-BDRU SECTION.
       A.  IF WB-OPEN = 0 GO Z.
           MOVE "E" TO BDA-SATZ(1:).
           WRITE BDA-SATZ AFTER 0.
       C.  MOVE 0 TO WB-OPEN WB-ZEILEN.
           CLOSE BESTDRU.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
           MOVE WE-STG(5) TO DRA-SATZ.
           WRITE DRA-SATZ AFTER 0.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      *************************************** Orders lesen und drucken *
       EDI-ORDERS SECTION.
       A.  CALL "CAUP" USING "0710100960000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bestellung aus Edifact " with highlight AT VDU-LP.
      *    OPEN INPUT KNOTEN.
           MOVE WM-DATUM TO WS-DATUM.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY "Datum:" AT VDU-LP.
           MOVE WS-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           ADD 7 TO VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       D.  MOVE WS-DATUM TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= EDI-Abholdatum" AT 2301.
           CALL "CAUP" USING "1102476006" WH-CREG.
           IF ESC GO X.
           IF not RET GO D.
           IF WX-DATUM = 0 GO D.
           MOVE WX-DATUM TO WS-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       F.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Kunden-Nr., alpha+<ret>= such
      -        "en, < />= zurÅck" AT 2301.
           DISPLAY "Kunden-Nr.: " AT VDU-LP.
           CALL "CAUP" USING "1004156006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO D.
           IF not RET GO F.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
                CALL "TIANZEIG" USING "01KDSUCH" WH-CREG
                CANCEL "TIANZEIG"
                IF WH-NUM = 0 GO F.
           MOVE WH-NUM TO KU-KTONR WD-NUM.
           MOVE "K" TO KU-SA.
           READ KUNDEN IGNORE LOCK INVALID GO F.
           DISPLAY WD-NUM with highlight foreground-color 2 AT VDU-LP
               " " KU-MCODE with highlight foreground-color 6.
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY "GLN: " AT VDU-LP " " KU-BAN with highlight.
           MOVE 0 TO WM-OPEN WM-KOPF.
           PERFORM SUCH-DATEI.
           PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
      *    CLOSE KNOTEN.
           MOVE 9 TO WH-PG.
           MOVE "TIBEST  10BEST" TO WT-TX.
       Z.  EXIT.
      **************************************************** GILN suchen *
       SUCH-DATEI SECTION.
       A.  MOVE 0 TO WL-ACT WL-ATTRIB.
      *    MOVE "G:\programme\tie\ep\main\archive\*.*" TO WN-EDIFAC.
           MOVE "G:\*.*" TO WL-FILEIN.
       C.  CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0 GO Z.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           PERFORM RAUS.
           IF DT-DATE < WS-DATUM GO C.
           MOVE "G:\" TO WN-EDIFAC.
           MOVE WL-FOUT TO WN-EDIFAC(4:).
      *    MOVE "G:\programme\tie\ep\main\archive\" TO WN-EDIFAC.
      *    MOVE WL-FOUT TO WN-EDIFAC(34:).
           OPEN INPUT EDIFACT.
           MOVE SPACE TO EDI-SATZ.
           READ EDIFACT AT END GO Z.
           EVALUATE EDI-SATZ(1:5)
               WHEN "UNB+U"
               WHEN "UNA:+" NEXT SENTENCE
               WHEN OTHER GO K.                         *> kein Edifact
           MOVE 1 TO IQ IC.
           MOVE 0 TO WH-DTM WH-LFDAT WH-DP WH-BY
                     WH-UC WH-CN WH-IV WH-UC WH-OB WH-EMPF.
       G.  MOVE SPACE TO WT-GRP.
           PERFORM VARYING IC FROM IC BY 1 UNTIL EDI-Z(IC) = "'"
               CONTINUE.
           COMPUTE WX = IC - IQ + 1.
           MOVE EDI-SATZ(IQ:WX) TO WT-GRP.
           COMPUTE IQ = IC + 1.
           ADD 1 TO IC.
           SET RET TO TRUE.
           EVALUATE WT-GRP(1:4)
               WHEN "UNB+" NEXT SENTENCE
               WHEN "UNH+" PERFORM OB-ORDERS
               WHEN "BGM+" PERFORM BGM-NR                *> Bestell-Nr.
               WHEN "DTM+" PERFORM DTM-DAT               *> Datum
               WHEN "NAD+" PERFORM NAD-S
               WHEN "LIN+" PERFORM BEST-AR
               WHEN "QTY+" PERFORM BEST-MG
               WHEN "UNS+"
               WHEN "UNT+"
               WHEN "UNZ+" MOVE 0 TO WM-KOPF
                           SET ESC TO TRUE.
           IF not ESC GO G.
       K.  CLOSE EDIFACT.
           GO C.
       Z.  EXIT.
      ******************************************************************
       BGM-NR SECTION.
       A.  MOVE 0 TO WH-BGM.
           MOVE 1 TO WX.
           MOVE 9 TO WZ.
           EVALUATE WT-GRP(5:3)
               WHEN 220
               WHEN 351 UNSTRING WT-GRP DELIMITED BY "+"
                                 INTO WU-SEG WU-QUAL WU-TEXT WT-TX
               WHEN 352 PERFORM VARYING WZ FROM WZ BY 1 UNTIL WZ > 19
                           IF WT-GRP(WZ:1) = "+" ADD 1 TO WZ
                               MOVE WT-GRP(WZ:) TO WH-BGM(WX:)
               WHEN "AAK" MOVE 8 TO WZ
                          PERFORM VARYING WZ FROM WZ BY 1 UNTIL WZ > 19
                              IF WT-GRP(WZ:1) = ":" ADD 1 TO WZ
                                 MOVE WT-GRP(WZ:) TO WH-BGM(WX:)
                          end-perform
                          MOVE 0 TO WH-NUM
                          MOVE 14 TO WX
                          PERFORM VARYING WZ FROM 16 BY -1 UNTIL WZ = 0
                              IF WH-BGM(WZ:1) IS NUMERIC ADD -1 TO WX
                                  MOVE WH-BGM(WZ:1) TO WH-NUM(WX:1)
               WHEN OTHER GO Z.
       Z.  EXIT.
      ******************************************************************
       RFF-NR SECTION.
       A.  MOVE 0 TO WR-ORDNR.
           PERFORM VARYING WX FROM 30 BY -1 UNTIL WX = 1
               IF WT-GRP((WX - 1):2) not = SPACE
                   MOVE ":" TO WT-GRP(WX:2)
               exit perform.
           EVALUATE WT-GRP(5:2)
               WHEN "UC" UNSTRING WT-GRP DELIMITED ":" or "+"
                            INTO WU-SEG WU-QUAL WU-RFFUC WT-TX.
      *------------------------------> wegen fehlender RFF+UC von DHL <-
           INSPECT WR-KTONR REPLACING ALL " " BY "0".
           IF WR-KTONR not = 0
               MOVE WR-KTONR TO KU-KTONR.
       Z.  EXIT.
      ******************************************************************
       DTM-DAT SECTION.
       A.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = ":"
                CONTINUE.
           ADD 1 TO WZ.
           MOVE 0 TO WX.
           PERFORM VARYING WZ FROM WZ BY 1 UNTIL WT-GRP(WZ:1) = ":"
               or WZ > 19 ADD 1 TO WX
               EVALUATE WT-GRP(5:1)
                   WHEN 1 MOVE WT-GRP(WZ:1) TO WH-DTM(WX:1)
                   WHEN 2 MOVE WT-GRP(WZ:1) TO WH-LFDAT(WX:1).
       Z.  EXIT.
      ******************************************************************
       OB-ORDERS SECTION.
       A.  SET ESC TO TRUE.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = "'"
               IF WT-GRP(WZ:6) = "ORDERS" SET RET TO TRUE
               exit perform.
       Z.  EXIT.
      ******************************************************************
       NAD-S SECTION.
       A.  EVALUATE WT-GRP(5:3)
               WHEN "BY+" MOVE WT-GRP(8:13) TO WH-BY
               WHEN "OB+" MOVE WT-GRP(8:13) TO WH-OB
               WHEN "IV+" MOVE WT-GRP(8:13) TO WH-IV
               WHEN "UC+" MOVE WT-GRP(8:13) TO WH-UC WH-EMPF
                          GO C
               WHEN "CN+" MOVE WT-GRP(8:13) TO WH-CN WH-EMPF GO C
               WHEN "SU+" MOVE WT-GRP(8:13) TO WH-SU
               WHEN "DP+" MOVE WT-GRP(8:13) TO WH-DP WH-EMPF
                          GO C.
           GO Z.
      *-----------------------------------------> Lieferadresse lesen <-
       C.  MOVE WH-EMPF TO KU-BAN.
           MOVE "K" TO KU-SA.
           READ KUNDEN IGNORE LOCK KEY IS KU-EANK INVALID
               MOVE "Kunde fehlt!" TO KU-BEZ.
       Z.  EXIT.
      ************************************************** Bestellzeilen *
       BEST-AR SECTION.
       A.  IF WH-EMPF not = KU-BAN SET ESC TO TRUE GO Z.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = ":"
               continue.
           ADD -13 TO WZ.
           MOVE WT-GRP(WZ:13) TO WH-EAN.
           MOVE WH-EAN TO KN-EAN.
           COMPUTE WX-BNUM = WH-EAN / 10.             *> ob Bestell-Nr.
           MOVE WX-BNUM TO BN-BNUM.
           MOVE 0 TO KN-NUM BN-NUM.
           IF BN-BNUM > 59999 GO E.
           READ KNOTEN IGNORE LOCK
                   INVALID MOVE "EAN nicht gelistet" TO AR-BEZ GO Z.
           MOVE KN-NUM TO AR-NUM BN-NUM.
           GO G.
      *--------------------------------------> nach Bestell-Nr. lesen <-
       E.  READ BESTNUM IGNORE LOCK KEY IS BN-AKEY
               INVALID MOVE "Best.Nr. nicht gelistet" TO AR-BEZ GO Z.
           MOVE BN-NUM TO AR-NUM WK-ARNUM.
       G.  MOVE "A" TO AR-ART.
           READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel fehlt" TO AR-BEZ.
           MOVE AR-NUM TO WK-ARNUM.
       Z.  EXIT.
      ******************************************************************
       BEST-MG SECTION.
       A.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = ":"
               continue.
           MOVE 0 TO WH-MG WR.
       C.  ADD 1 TO WZ.
           PERFORM VARYING WZ FROM WZ BY 1 UNTIL WT-GRP(WZ:1) = "'"
               EVALUATE WT-GRP(WZ:1)
                   WHEN "." MOVE 6 TO WR
                   WHEN "'" exit perform
                   WHEN 0 THRU 9
                          IF WR = 0 COMPUTE WH-MG = WH-MG * 10
                               MOVE WT-GRP(WZ:1) TO WH-MG(5:1)
                          else MOVE WT-GRP(WZ:1) TO WH-MG(WR:1)
                               ADD 1 TO WR.
       G.  IF WM-OPEN = 0 MOVE "EDIBEST.LST" TO WH-DRUNAM
               PERFORM BEG-DRU.
           IF WZ-ZEILEN > 59 WRITE DRA-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN = 0 or WM-KOPF= 0 MOVE KU-KTONR TO DRA-KTONR
               MOVE KU-MCODE TO DRA-MCODE
               MOVE "  B-Datum:" TO DRA-BETX
               MOVE WH-DTM TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE VDU-DATUM TO DRA-BEDAT
               MOVE "Lf.-Datum:" TO DRA-LFTX
               MOVE WH-LFDAT TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE VDU-DATUM TO DRA-LFDAT
               IF WZ-ZEILEN = 0 MOVE 0 TO WZ-SCHALT
               else MOVE 2 TO WZ-SCHALT
               end-if
               PERFORM DRUCK
               MOVE WL-FOUT TO DRA-MCODE(18:)
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 1 TO WM-KOPF.
           MOVE BN-NUM TO DRB-ARNUM.
           DISPLAY DRB-ARNUM AT 2301 " " AR-BEZ.
           MOVE AR-BEZ TO DRB-BEZ.
           MOVE WH-EAN TO DRB-EAN.
           MOVE WH-BGM TO DRB-BESNR.
           MOVE WH-MG TO DRB-MG.
           IF DRB-MG(4:3) = ",00" MOVE SPACE TO DRB-MG(4:).
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       RAUS SECTION.
       A.  COMPUTE ZA = WL-DATE / 512 + 1980.
           MOVE 0 TO WZ-JAHR.
           COMPUTE WZ-MONAT = (WL-DATE - ((ZA - 1980) * 512)) / 32.
           COMPUTE WZ-TAG =
                   WL-DATE - (WZ-MONAT * 32 +  (ZA - 1980) * 512).
           COMPUTE WC-TAG = WL-TIME / 2048.
           COMPUTE WC-MONAT = (WL-TIME - (WC-TAG * 2048)) / 32.
           COMPUTE WC-JAHR =
                      (WL-TIME - (WC-TAG * 2048 + WC-MONAT * 32)) * 2.
           MOVE WL-SIZE TO DT-SIZE.
           COMPUTE DT-DATE = ZA * 10000 + WZ-DATUM.
      *    IF DT-DATE not < WV-DATUM GO Z.
           MOVE WC-DATUM TO DT-TIME.
           MOVE DT-DATE TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           ADD 803 VDU-ECK GIVING VDU-LP.
           DISPLAY WL-FOUT AT VDU-LP " " VDU-DATUM " " DT-TIME
               " " DT-SIZE.
       Z.  EXIT.
      ************************************************ Recadv - prÅfen *
       EDI-RECADV SECTION.
       A.  MOVE "K:\RECADV\RECADV.*" TO WL-FILEIN.
      *    OPEN INPUT KNOTEN.
      *----------------------------------> prÅfen ob RECADV vorhanden <-
           MOVE 0 TO WL-ACT WL-ATTRIB WM-OPEN.
           MOVE LOW-VALUES TO WL-RESULT.
       C.  CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0 GO Z.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           PERFORM RAUS.
           IF DT-DATE < WS-DATUM GO C.
           MOVE WL-FOUT TO WN-EDIFAC.
           OPEN INPUT EDIFACT.
           MOVE SPACE TO EDI-SATZ.
           READ EDIFACT AT END GO Z.
           EVALUATE EDI-SATZ(1:5)
               WHEN "UNB+U"
               WHEN "UNA:+" NEXT SENTENCE
               WHEN OTHER GO K.                         *> kein Edifact
           MOVE 1 TO IQ IC.
       E.  MOVE 0 TO WM-KOPF.
           MOVE 0 TO WH-DTM WH-LFDAT WH-DP WH-BY
                     WH-UC WH-CN WH-IV WH-UC WH-OB WH-EMPF.
       G.  MOVE SPACE TO WT-GRP.
           PERFORM VARYING IC FROM IC BY 1 UNTIL EDI-Z(IC) = "'"
               CONTINUE.
           MOVE " " TO EDI-Z(IC).
           COMPUTE WX = IC - IQ + 1.
           MOVE EDI-SATZ(IQ:WX) TO WT-GRP.
           COMPUTE IQ = IC + 1.
           ADD 1 TO IC.
           SET RET TO TRUE.
           EVALUATE WT-GRP(1:4)
               WHEN "UNB+" NEXT SENTENCE
               WHEN "UNH+" PERFORM OB-RECADV
               WHEN "BGM+" PERFORM BGM-NR                *> Bestell-Nr.
               WHEN "DTM+" PERFORM DTM-DAT               *> Datum
               WHEN "RFF+" PERFORM BGM-NR                *> Lfs.-Nr.
               WHEN "NAD+" PERFORM NAD-S
               WHEN "LIN+" PERFORM BEST-AR
               WHEN "QVA+" PERFORM REC-MG
               WHEN "UNS+"
               WHEN "UNT+" MOVE ALL "ƒ" TO DRA-SATZ(5:)
                           PERFORM DRUCK
                           MOVE 2 TO WZ-SCHALT
                           PERFORM DRUCK
                           GO E
               WHEN "UNZ+" MOVE 0 TO WM-KOPF
                           SET ESC TO TRUE.
           IF not ESC GO G.
       K.  CLOSE EDIFACT.
           GO C.
      *    CLOSE KNOTEN.
           MOVE 9 TO WH-PG.
           MOVE "TIBEST  10BEST" TO WT-TX.
       Z.  EXIT.
      ******************************************************************
       OB-RECADV SECTION.
       A.  SET ESC TO TRUE.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = "'"
               IF WT-GRP(WZ:6) = "RECADV" SET RET TO TRUE
               exit perform.
       Z.  EXIT.
      ****************************************** Mengen aus DESADV-DHL *
       QTY-MG SECTION.
       A.  MOVE 0 TO WH-MG WR WH-NEG.
           UNSTRING WT-GRP DELIMITED BY ":" or "+"
                           INTO WU-SEG WU-QUAL WH-MCODE WU-MEH.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WH-MCODE(WZ:1) = " "
               EVALUATE WH-MCODE(WZ:1)
                   WHEN "." MOVE 6 TO WR
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN ":" exit perform
                   WHEN 0 THRU 9
                          IF WR = 0 COMPUTE WH-MG = WH-MG * 10
                               MOVE WH-MCODE(WZ:1) TO WH-MG(5:1)
                          else MOVE WH-MCODE(WZ:1) TO WH-MG(WR:1)
                               ADD 1 TO WR.
           IF WH-NEG = 0 MOVE WH-MG TO WH-WERT
           else COMPUTE WH-WERT = WH-MG * -1.
       Z.  EXIT.
      ********************************************** Mengen aus RECADV *
       REC-MG SECTION.
       A.  MOVE 0 TO WH-MG WR WH-NEG.
           MOVE 4 TO WZ.
           PERFORM VARYING WZ FROM WZ BY 1 UNTIL WT-GRP(WZ:1) = ":"
               EVALUATE WT-GRP(WZ:1)
                   WHEN "." MOVE 6 TO WR
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN ":" exit perform
                   WHEN 0 THRU 9
                          IF WR = 0 COMPUTE WH-MG = WH-MG * 10
                               MOVE WT-GRP(WZ:1) TO WH-MG(5:1)
                          else MOVE WT-GRP(WZ:1) TO WH-MG(WR:1)
                               ADD 1 TO WR.
           IF WH-NEG = 0 MOVE WH-MG TO WH-WERT
           else COMPUTE WH-WERT = WH-MG * -1.
       G.  IF WM-OPEN = 0 MOVE "EDIREC.LST" TO WH-DRUNAM
               PERFORM BEG-DRU
               MOVE "Reklamationen: " TO DRA-SATZ(5:)
               MOVE WL-FOUT TO DRA-SATZ(20:)
               MOVE "vom: " TO DRA-SATZ(35:)
               MOVE DT-DATE TO WC-DATUM
               CALL "CAUP" USING "04DATPRF" WH-CREG
               MOVE VDU-DATUM TO DRA-SATZ(40:)
               MOVE "erstellt: " TO DRA-SATZ(63:)
               MOVE WH-DATUM TO WC-DATUM
               CALL "CAUP" USING "04DATPRF" WH-CREG
               MOVE VDU-DATUM TO DRA-SATZ(73:)
               MOVE "/" TO DRA-SATZ(81:1)
               ACCEPT WH-TIME FROM TIME
               COMPUTE WD-TIME = WH-TIME / 10000
               MOVE ":" TO WD-TIME(3:1)
               MOVE WD-TIME TO DRA-SATZ(82:)
               PERFORM DRUCK
               MOVE ALL "ƒ" TO DRA-SATZ(5:)
               PERFORM DRUCK
               PERFORM DRUCK.
           IF WZ-ZEILEN > 59 WRITE DRA-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN = 0 or WM-KOPF = 0 MOVE KU-KTONR TO DRR-KTONR
               MOVE KU-MCODE TO DRR-MCODE
               MOVE "  Lfs.Nr.: " TO DRR-LFSNR
               MOVE WH-BGM TO DRR-LFSNUM
               MOVE "Lf.-Datum:" TO DRR-LFTX
               MOVE WH-DTM TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE VDU-DATUM TO DRR-LFDAT
               IF WZ-ZEILEN = 0 MOVE 0 TO WZ-SCHALT
               else MOVE 2 TO WZ-SCHALT
               end-if
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 1 TO WM-KOPF.
           MOVE BN-NUM TO DRB-ARNUM.
           DISPLAY DRB-ARNUM AT 2301 " " AR-BEZ.
           IF DRB-ARNUM(7:3) = ",00" MOVE SPACE TO DRB-ARNUM(7:).
           MOVE AR-BEZ TO DRB-BEZ.
           MOVE WH-EAN TO DRB-EAN.
           MOVE WH-WERT TO DRB-MG.
           IF DRB-MG(4:3) = ",00" MOVE SPACE TO DRB-MG(4:)
               IF WH-WERT < 0 MOVE "-" TO DRB-MG(4:).
           PERFORM DRUCK.
       Z.  EXIT.
      **************************** Anzahl E-Mail-Bestellungen anzeigen *
       ANZ-EMAIL SECTION.
       A.  CALL "CAUP" USING "0708110359000" WH-CREG.
           MOVE 0 TO WL-ACT WL-ATTRIB WH-NUM.
           MOVE "K:\*.*" TO WL-FILEIN.
       C.  CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0 GO E.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           IF WL-FOUT(1:2) = "MD" ADD 1 TO WH-NUM.
           GO C.
       E.  IF WH-NUM = 0 GO X.
           ADD 203 VDU-ECK GIVING VDU-LP.
           MOVE WH-NUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP " e-Mail-Bestellungen
      -        "vorhanden!"  with highlight foreground-color 6.
           DISPLAY "<ret>= Åbernehmen, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023360000" WH-CREG.
           IF ESC GO X.
           IF not RET GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *********************************** auslesen E-Mail Bestellungen *
       EMAIL-EINLESEN SECTION.
       A.  MOVE " Email-Bestellungen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "270505097000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " E-Mail Bestellungen " with highlight AT VDU-LP.
           PERFORM ANZ-EMAIL.
           IF ESC GO Q.
           MOVE 0 TO WL-ACT WL-ATTRIB WM-OPEN WB-OPEN WZ-ZEILEN.
           MOVE "K:\*.*" TO WL-FILEIN.
       C.  CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0 GO Q.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           IF WL-FOUT(1:2) not = "MD" GO C.
           PERFORM RAUS.
           MOVE "K:\" TO WN-SEDAS.
           MOVE WL-FOUT  TO WN-SEDAS(4:).
           MOVE 1 TO WB-AC.                               *> read only
           MOVE 0 TO WB-DE WB-DV WB-FLG WB-OFS IC.
           MOVE SPACE TO WB-FH.
           MOVE 1 TO WB-CNT IQ.
           MOVE 10 TO WX.
           MOVE SPACE TO EDI-SATZ WN-EDISIC.
           PERFORM AUFTEILEN.
           CALL "CBL_DELETE_FILE" USING WN-SEDAS.
           GO C.
       Q.  IF WB-OPEN not = 0 PERFORM END-BDRU.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WM-OPEN = 1 PERFORM END-DRU
               ADD 005 VDU-ECK GIVING VDU-LP
               DISPLAY "Bitte Fehlerprotokoll drucken" with highlight
                   AT VDU-LP
               DISPLAY " " AT 2401
               PERFORM WEITER.
       Z.  EXIT.
      ********************* Emaildatei auteilen auf DATEN u. SONSTIGES *
       AUFTEILEN SECTION.
       A.  OPEN INPUT SEDAS.
           MOVE 0 TO WR.
       C.  READ SEDAS NEXT AT END GO X.
           IF SEDAS-SATZ(1:2) not = "K;" GO C.
      *----------------------------------------> Kopfsatz verarbeiten <-
           MOVE 1 TO WQ.
       E.  MOVE SEDAS-SATZ TO EDI-SATZ(WQ:) EDS-SATZ.
           IF WN-EDISIC = SPACE MOVE "EMAIL.MSG" TO WN-EDISIC
                                OPEN OUTPUT EDISICH.
           WRITE EDS-SATZ BEFORE 1.
           ADD WQ 81 GIVING WS.
           PERFORM VARYING WS FROM WS BY -1 UNTIL WS = WQ
               IF EDI-SATZ(WS:1) not = " " MOVE " " TO EDI-SATZ(WS:1)
               exit perform.
           MOVE WS TO WQ.
       G.  READ SEDAS NEXT AT END GO X.
           IF SEDAS-SATZ(1:2) = "P;" GO I.
           IF SEDAS-SATZ(1:2) = "  " GO G.
           GO E.
      *---------------------------------------> neue Dateie erstellen <-
       I.  MOVE EDI-SATZ TO WT-KUND.
           MOVE SEDAS-SATZ TO EDS-SATZ.
           PERFORM KUND-POS.
           GO K.
       J.  MOVE SEDAS-SATZ TO EDS-SATZ.
           PERFORM ART-POS.
           MOVE SPACE TO WT-ART.
           MOVE EDS-SATZ TO SEDAS-SATZ.
       K.  IF SEDAS-SATZ(1:2) = "K;" GO R.               *> neuer Kunde
      *----------------------------------------> Position verarbeiten <-
           MOVE 1 TO WQ.
       M.  MOVE SEDAS-SATZ TO EDI-SATZ(WQ:) EDS-SATZ.
           WRITE EDS-SATZ BEFORE 1.
           ADD WQ 81 GIVING WS.
           PERFORM VARYING WS FROM WS BY -1 UNTIL WS = WQ
               IF EDI-SATZ(WS:1) not = " " MOVE " " TO EDI-SATZ(WS:1)
               exit perform.
           MOVE EDI-SATZ TO WT-ART.
           MOVE WS TO WQ.
       O.  READ SEDAS NEXT AT END GO Q.
           IF SEDAS-SATZ(1:2) = "P;" GO J.
           IF SEDAS-SATZ(1:2) = "K;" GO J.
           IF SEDAS-SATZ(1:50) = SPACE GO O.
           IF SEDAS-SATZ(1:12) not = "--PART.BOUND" GO M.
      *-----------------------------------> letzte Position schreiben <-
       Q.  IF WT-ART = " " GO R.
           PERFORM ART-POS.
           MOVE SPACE TO EDS-SATZ WT-ART.
      *-----------------------------------------> Kunden abschliessen <-
       R.  CLOSE EDISICH.
           PERFORM EMAIL-SICH.                 *> in EMAILBES abstellen
           CALL "CBL_COPY_FILE" USING WN-EDISIC WH-GESF.
           CALL "CBL_DELETE_FILE" USING WN-EDISIC.
           MOVE SPACE TO WN-EDIFAC WN-EDISIC.
           MOVE 1 TO WQ.
           IF SEDAS-SATZ(1:2) = "K;" GO E.               *> neuer Kunde
       X.  CLOSE SEDAS.
       Z.  EXIT.
      ********************************* nÑchste Sicherungsdatei suchen *
       EMAIL-SICH SECTION.
       A.  MOVE "EMAILBES\" TO WH-DIR.
           MOVE WD-KDNUM TO WH-FNAME.
           MOVE WU-LIEFDAT TO WZ-DATUM.
           MOVE "-mm.tt0" TO WH-FNAME(6:).
           MOVE WZ-MONAT TO WH-FNAME(7:2).
           MOVE WZ-TAG TO WH-FNAME(10:2).
           GO E.
       C.  MOVE WH-FNAME(12:1) TO WX-KZ.
           EVALUATE WC-KZ
               WHEN 57 MOVE 65 TO WC-KZ
               WHEN OTHER ADD 1 TO WC-KZ.
           MOVE WX-KZ TO WH-FNAME(12:1).
       E.  MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-GESF WH-FDET.
           IF WF-SIZE not = 0 GO C.
       Z.  EXIT.
      ****************************************************** auswerten *
       AUSWERT SECTION.
       A.  IF WT-TX(1:10) = "--PART.BOU" or WT-TX(1:10) = "----------"
               IF WT-ART(1:2) = "P;" PERFORM ART-POS
                  MOVE "*Ende*" TO WT-TX
                  GO Z.
           EVALUATE EDI-SATZ(1:WX)
               WHEN "--PART.BOU"
               WHEN "----------" MOVE 2 TO WX
               WHEN "P;" IF WT-TX(1:2) = "P;" or WT-TX(1:2) = "K;"
                             PERFORM ART-POS
                             MOVE WT-TX TO EDI-SATZ
                             MOVE IC TO IQ
                         end-if
                         MOVE SPACE TO WT-TX
                         MOVE EDI-SATZ TO WT-ART
                         MOVE 1 TO IC
                         GO Z
               WHEN "K;" IF WT-TX(1:2) = "P;" PERFORM KUND-POS
                            MOVE WT-TX TO EDI-SATZ
                            MOVE IC TO IQ
                         end-if
                         MOVE SPACE TO WT-TX
                         MOVE EDI-SATZ TO WT-KUND
                         MOVE 1 TO IC
                         GO Z.
       G.  MOVE SPACE TO EDI-SATZ WT-TX.
           MOVE 1 TO IQ IC.
       Z.  EXIT.
      *********************************** Artikel-Position aufarbeiten *
       ART-POS SECTION.
       A.  INSPECT WT-ART REPLACING ALL "GVE 1" BY "     ".
           PERFORM VARYING WY FROM 128 BY -1 UNTIL WT-ART(WY:1)
               not = " " CONTINUE.
           ADD 1 TO WY.
           MOVE ";" TO WT-ART(WY:).
           MOVE WT-ART(3:) TO WT-ART(1:).
           UNSTRING WT-ART DELIMITED BY ";" INTO WU-POS WU-ANZ WU-GVA
                WU-MEH WU-GNR WU-BEST WU-TEXT WU-ARTNR WU-EAN.
           MOVE WU-POS TO DBD-POS.
           MOVE WU-ARTNR TO DBD-ARNUM.
           IF DBD-ARNUM(7:3) = ",00" MOVE SPACE TO DBD-ARNUM(7:3).
           MOVE WU-TEXT TO DBD-BEZ.
           MOVE WU-ANZ TO DBD-MG.
           MOVE WU-MEH TO DBD-MEH.
           MOVE WU-EAN TO DBD-EAN.
           MOVE WU-BEST TO DBD-BESNR.
           MOVE WU-ARTNR TO AR-NUM.
           MOVE WU-EAN TO KN-EAN WK-EAN.
           MOVE SPACE TO EF-TXT.
           MOVE AR-NUM TO EF-IARTNR(1:).
           MOVE 0 TO EF-IMENGE.
           PERFORM ART-PRF.
           IF AR-NUM = 9999 MOVE " *Fehler*" TO DBD-BEZ(19:).
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY DBD-SATZ(1:50) AT VDU-LP.
           PERFORM B-DRUCK.
           IF AR-NUM = 9999 GO Z.
      *-------------------------------------------> oder vorher lesen <-
           PERFORM LAST-BSX.
           ADD 1 BP-POS GIVING WH-ANZPOS.
           INITIALIZE BP-SATZ.
           MOVE BK-KDNUM TO BP-KDNUM.
           MOVE BK-LIEFDAT TO BP-LIEFDAT.
           ADD 1 TO BK-ANZPOS.
           MOVE WU-ARTNR TO BP-ARTNR BP-SORT.
           MOVE AR-BEZ TO BP-ARTBEZ.
           MOVE WU-ANZ TO BP-MENGE.
           MOVE AR-MEH TO BP-MEH.
           MOVE 0 TO BP-MIN.
           MOVE 99999 TO BP-MAX.
           MOVE AR-SCAN TO BP-SCAN.
           IF BP-MAX = 0 MOVE 9999 TO BP-MAX.
      *    MOVE WU-BEST(3:) TO BP-BESNR.            *> wg. Stellendiff.
           MOVE WU-BNUM TO BP-BESNR.
           MOVE WU-BESTDAT TO BP-BESDAT.
       G.  MOVE WH-ANZPOS TO BP-POS.
           WRITE BP-SATZ INVALID ADD 1 TO WH-ANZPOS GO G.
           MOVE BP-SATZ TO SBP-SATZ.
           WRITE SBP-SATZ INVALID REWRITE SBP-SATZ.
           REWRITE BK-SATZ INVALID WRITE BK-SATZ.       *> wg. ANZ.POS.
           MOVE BK-SATZ TO SBK-SATZ.
           REWRITE SBK-SATZ INVALID WRITE SBK-SATZ.     *> wg. ANZ.POS.
       Q.  READ BESTKOPF INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Q.
       Z.  EXIT.
      ******************************************************************
       LAST-BSX SECTION.
       A.  MOVE 9999 TO BP-POS.
           MOVE BK-KDNUM TO BP-KDNUM.
           MOVE BK-ZWO TO BP-ZWO.
           MOVE BK-LIEFDAT TO BP-LIEFDAT.
           START BESTPOS KEY < BP-KEY INVALID MOVE 0 TO BP-KDNUM GO C.
       B.  MOVE SPACE TO BP-SATZ(10:).
           READ BESTPOS PREVIOUS IGNORE LOCK AT END MOVE 0 TO BP-KDNUM.
       C.  IF BP-KDNUM not = BK-KDNUM or BP-ZWO not = BK-ZWO
              or BP-LIEFDAT not = WE-LIEFDAT INITIALIZE BP-SATZ
                                       MOVE LOW-VALUE TO BP-KEY
                                       MOVE BK-ANZPOS TO BP-POS.
       Z.  EXIT.
      ************************************ Kunden-Position aufarbeiten *
       KUND-POS SECTION.
       A.  IF WB-OPEN = 0 MOVE 0 TO WB-ZEILEN
               MOVE "BESTPROT.LST" TO WB-DRUNAM
               MOVE 0 TO WB-ZEILEN WB-SCHALT
               MOVE 1 TO WB-OPEN
               OPEN EXTEND BESTDRU
               MOVE WE-STG(5) TO BDA-SATZ
               WRITE BDA-SATZ AFTER 0.
               MOVE SPACE TO BDA-SATZ.
           IF WB-ZEILEN > 58 MOVE "-- Fortsetzung --" TO BDA-SATZ
               PERFORM B-DRUCK
               WRITE BDA-SATZ BEFORE PAGE
               MOVE SPACE TO BDA-SATZ
               MOVE 0 TO WB-ZEILEN.
           MOVE 1 TO WY.
           MOVE 0 TO WQ-DAT WU-BESTDAT WU-LIEFDAT WU-GLN.
           MOVE SPACE TO WU-KUTX.
           PERFORM VARYING IP FROM 1 BY 1 UNTIL IP > 320
               IF WT-KUND(IP:1) = ";" ADD 1 TO WY
                                      MOVE 1 TO WR
               else EVALUATE WY
                       WHEN 2 PERFORM BESTNR
                       WHEN 4
                       WHEN 5 PERFORM BELFDAT
                       WHEN 19 PERFORM KUND-BEZ
                       WHEN 24
                       WHEN 38 PERFORM BEST-GLN.
           IF WB-ZEILEN > 1 MOVE 2 TO WB-SCHALT.
           INITIALIZE KU-SATZ.
           MOVE WU-BGLN TO BDA-GLN KU-BAN.
           READ KUNDEN KEY KU-EANK INVALID DISPLAY "fehlerhafte GLN: "
               with highlight foreground-color 6 AT 2401
               DISPLAY BDA-GLN AT 0000
               MOVE "MD" TO EF-SA
               PERFORM WEITER
               MOVE 99900 TO WD-KDNUM BDA-KDNUM
               PERFORM EMAIL-SICH
               PERFORM FEHLPROT
               GO G.
           MOVE KU-KTONR TO WD-KDNUM BDA-KDNUM.
       G.  MOVE WU-KUTX TO BDA-BEZ.
           MOVE WU-BNUM TO BDA-BNUM.
           MOVE WU-BESTDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDR" WH-CREG.
           MOVE VDU-DATUM TO BDA-BESTDAT.
           MOVE WU-LIEFDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDR" WH-CREG.
           MOVE VDU-DATUM TO BDA-LIEFDAT.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY BDA-SATZ(1:50) AT VDU-LP.
           PERFORM B-DRUCK.
           MOVE ALL "ƒ" TO DBD-SATZ(3:).
           PERFORM B-DRUCK.
           INITIALIZE BK-SATZ.
           IF KU-KTONR = 0 GO Z.                 *> kein Kunde gefunden
           MOVE KU-KTONR TO BK-KDNUM.
           MOVE WU-LIEFDAT TO BK-LIEFDAT.
           MOVE WU-BESTDAT TO BK-BEDAT.
           READ BESTKOPF INVALID INITIALIZE BK-SATZ
               MOVE KU-KTONR TO BK-KDNUM
               MOVE WU-LIEFDAT TO BK-LIEFDAT
               MOVE WU-BESTDAT TO BK-BEDAT.
           MOVE WU-BNUM TO BK-BENR.
           REWRITE BK-SATZ INVALID WRITE BK-SATZ.
           MOVE BK-SATZ TO SBK-SATZ.
           REWRITE SBK-SATZ INVALID WRITE SBK-SATZ.
       Q.  READ BESTKOPF INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Q.
       Z.  EXIT.
      ******************************************  Bestell-Nr. auslesen *
       BESTNR SECTION.
       A.  MOVE WT-KUND(IP:1) TO WU-BNUM(WR:1).
           ADD 1 TO WR.
       Z.  EXIT.
      ****************************************** Bestell- /Lieferdatum *
       BELFDAT SECTION.
       A.  IF WT-KUND(IP:1) not = "."
               MOVE WT-KUND(IP:1) TO WQ-DAT(WR:1)
               ADD 1 TO WR.
           MOVE WQ-DAT(5:4) TO WR-DAT(1:4).
           MOVE WQ-DAT(3:4) TO WR-DAT(5:4).
           MOVE WQ-DAT(1:2) TO WR-DAT(7:4).
           IF WY = 4 MOVE WR-DAT TO WU-BESTDAT
                else MOVE WR-DAT TO WU-LIEFDAT.
       Z.  EXIT.
      *************************************************** GLN-Zentrale *
       BEST-GLN SECTION.
       A.  MOVE WT-KUND(IP:1) TO WU-GLN(WR:1).
           ADD 1 TO WR.
           IF WY = 24 MOVE WU-GLN TO WU-BGLN.
       Z.  EXIT.
      ********************************************* Kunden-Bezeichnung *
       KUND-BEZ SECTION.
       A.  MOVE WT-KUND(IP:1) TO WU-KUTX(WR:).
           ADD 1 TO WR.
       Z.  EXIT.
      **************************** verarbeitbare Bestellungen anzeigen *
       B-ANZEIGEN SECTION.
       A.  CALL "CAUP" USING "270504107400010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " elektron. Bestellungen " with highlight AT VDU-LP.
      *    DISPLAY " Orders/E-mail/Recadv " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Orders: " with highlight foreground-color 6
                AT VDU-LP.
           PERFORM ORDERS-EINLESEN.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "E-Mail: " with highlight foreground-color 3
                AT VDU-LP.
           PERFORM EMAIL-EINLESEN.
           ADD 703 VDU-ECK GIVING VDU-LP.
           DISPLAY "Recadv: " with highlight foreground-color 4
                AT VDU-LP.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       ORDERS-EINLESEN SECTION.
       A.  CALL "CAUP" USING "0706101148000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Einlesen Bestelldatei " with highlight AT VDU-LP.
           MOVE 0 TO WB-OPEN WM-OPEN WZ-ZEILEN.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           PERFORM HOL-SEDAS.
           IF ESC GO Y.
           OPEN INPUT SEDAS.
           IF WF-STATUS = "35" GO U.
           MOVE SPACE TO SEDAS-SATZ.
           READ SEDAS AT END GO U.
           IF SEDAS-SATZ = SPACE GO U.
      *-----------------------------------------------------------------
           OPEN I-O KONTROLL WITH LOCK.
       C.  MOVE 1 TO KT-KEY.
           READ KONTROLL INVALID PERFORM LIEF-NEU
                                 WRITE KT-SATZ.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "1. Lieferdatum:    " AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "2. Lieferdatum:    " AT VDU-LP.
           ADD 703 VDU-ECK GIVING VDU-LP.
           DISPLAY "3. Lieferdatum:    " AT VDU-LP.
           IF KT-NUMMER not = 0 PERFORM DISP-DAT
               GO I.
           IF WM-DATUM > KT-BEDAT PERFORM LIEF-NEU.
           MOVE KT-DAT-A TO WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 PERFORM LIEF-NEU.
           PERFORM DISP-DAT.
      *----------------------------------------------> 1. Lieferdatum <-
       E.  IF KT-DAT-A = 0 MOVE WM-DATUM TO KT-DAT-A.
           MOVE KT-DAT-A TO WC-DATUM.
           PERFORM DATDREH.
           ADD 323 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE KT-DAT-A TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1103236006" WH-CREG.
           IF ESC CLOSE SEDAS KONTROLL GO X.
           IF NOT RET GO E.
           IF WZ-DATUM = 0 GO E.
           PERFORM DATDRU.
           MOVE WX-DATUM TO KT-DAT-A WE-LIEFDAT.
      *----------------------------------------------> 2. Lieferdatum <-
       F.  IF KT-DAT-B = 0 MOVE KT-DAT-A TO KT-DAT-B.
           MOVE KT-DAT-B TO WC-DATUM.
           PERFORM DATDREH.
           ADD 523 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE KT-DAT-B TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Datum"
               AT 2301.
           CALL "CAUP" USING "1105236006" WH-CREG.
           IF ESC CLOSE SEDAS KONTROLL GO X.
           IF WOLI GO E.
           IF NOT RET GO F.
           IF WX-DATUM = 0 MOVE KT-DAT-A TO WZ-DATUM.
           PERFORM DATDRU.
           MOVE WX-DATUM TO KT-DAT-B.
      *----------------------------------------------> 3. Lieferdatum <-
       G.  IF KT-DAT-C = 0 MOVE KT-DAT-A TO KT-DAT-C.
           MOVE KT-DAT-C TO WC-DATUM.
           PERFORM DATDREH.
           ADD 723 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE KT-DAT-C TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Datum"
               AT 2301.
           CALL "CAUP" USING "1107236006" WH-CREG.
           IF ESC CLOSE SEDAS KONTROLL GO X.
           IF WOLI GO E.
           IF NOT RET GO F.
           IF WX-DATUM = 0 MOVE KT-DAT-B TO WZ-DATUM.
           PERFORM DATDRU.
           MOVE WX-DATUM TO KT-DAT-C.
           MOVE WM-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
       H.  ADD 1003 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start Einlesen, <>= zurÅck < >" AT VDU-LP.
           CALL "CAUP" USING "1010390000" WH-CREG.
           IF WOLI GO E.
           IF NOT RET GO H.
           ADD 803 VDU-ECK GIVING VDU-LP.
           DISPLAY "Einlesen gestartet!" with foreground-color 3
               AT VDU-LP.
           ADD 1003 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1300000050" WH-CREG.
           ADD 430 VDU-ECK GIVING VDU-LP.
       I.  MOVE 0 TO WZ-DUP.
      *    OPEN I-O KNOTEN.
           PERFORM BESTELLUNG-UEBERTRAGEN.
           IF WM-OPEN = 1 PERFORM END-DRU
               ADD 005 VDU-ECK GIVING VDU-LP
               DISPLAY "Bitte Fehlerprotokoll drucken" with highlight
                   AT VDU-LP.
           CALL "CAUP" USING "0714100360000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Einlesen BESTELLDATEI beendet! Weiter mit <ret> < >"
                with foreground-color 2 AT VDU-LP.
           CALL "CAUP" USING "1002520000" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE SEDAS.
           PERFORM SEDASICH
           DELETE FILE SEDAS.
       S.  MOVE 1 TO KT-KEY.
           READ KONTROLL.
           MOVE 0 TO KT-NUMMER.
           REWRITE KT-SATZ.
           CLOSE KONTROLL.
           GO X.
      *-----------------------------------------------> Fehlerausgabe <-
       U.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bestelldatei <BESTELL.DAT> ist leer oder nicht vorha
      -       "nden!" with highlight foreground-color 6 AT VDU-LP.
           ADD 316 VDU-ECK GIVING VDU-LP.
           DISPLAY "<BESTELL.DAT>" with background-color 3
                foreground-color 4 AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Weiter mit <ret> < >" AT VDU-LP.
       W.  CALL "CAUP" USING "1004210000" WH-CREG.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
      *    CLOSE KNOTEN.
       Y.  MOVE WM-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
       Z.  EXIT.
      ******************************************* Liefertage ermitteln *
       LIEF-NEU SECTION.
       A.  MOVE 0 TO KT-NUMMER.
           MOVE WM-DATUM TO KT-BEDAT.                     *> Bestelltag
           MOVE WM-DATUM TO KT-DAT-A WA-TAGE.
           PERFORM NEXT-TAG.
           MOVE WS-DATUM TO KT-DAT-B WA-TAGE.
           PERFORM NEXT-TAG.
           MOVE WS-DATUM TO KT-DAT-C.
       Z.  EXIT.
      *********************************************** nÑchster Werktag *
       NEXT-TAG SECTION.
       A.  COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WA-TAGE).
       C.  ADD 1 TO WA-TAGE.
           COMPUTE WS-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           MOVE WS-DATUM TO WZ-DATUM WV-DATUM.
           CALL "CAUP" USING "03DATPRF" WH-CREG.
           IF WO-TGN = 0 GO C.                          *> weil Sonntag
      *    PERFORM KALENDA.
           IF WY = 1 GO C.
       Z.  EXIT.
      ******************************************************************
       DATDRU SECTION.
       A.  MOVE WX-DATUM TO WV-DATUM.
      *    PERFORM KALENDA.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD -4 TO VDU-LP.
           MOVE WV-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           DISPLAY WO-TAG with highlight foreground-color 2 AT VDU-LP.
           ADD 4 TO VDU-LP.
           IF WY = 0 and WO-TGN not = 7
                DISPLAY VDU-DATUM with highlight AT VDU-LP
           else DISPLAY VDU-DATUM with highlight foreground-color 4
                    AT VDU-LP.
           MOVE WV-DATUM TO WZ-DATUM.
       Z.  EXIT.
      ******************************************************************
       DISP-DAT SECTION.
       A.  ADD 323 VDU-ECK GIVING VDU-LP.
           MOVE KT-DAT-A TO WC-DATUM.
           PERFORM DATDREH.
           MOVE KT-DAT-A TO WZ-DATUM.
           PERFORM DATDRU.
           ADD 523 VDU-ECK GIVING VDU-LP.
           MOVE KT-DAT-B TO WC-DATUM.
           PERFORM DATDREH.
           MOVE KT-DAT-B TO WZ-DATUM.
           PERFORM DATDRU.
           ADD 723 VDU-ECK GIVING VDU-LP.
           MOVE KT-DAT-C TO WC-DATUM.
           PERFORM DATDREH.
           MOVE KT-DAT-C TO WZ-DATUM.
           PERFORM DATDRU.
       Z.  EXIT.
      *************************************** Sedas/Ecodex-Datei holen *
       HOL-SEDAS SECTION.
       A.  MOVE "N:\EDI-TIE\EMG\EMG_W.40\ORDERS.IN\ORDERS" TO WN-BEST.
           PERFORM VARYING WX FROM 40 BY -1 UNTIL WX = 0
              IF WN-BEST(WX:1) = "\" exit perform.
           IF WX = 0 MOVE WN-BEST(WX:12) TO WN-BEST.
           ADD 1 TO WX.
           MOVE "ORDERS.DAT" TO WN-SEDAS.
           MOVE LOW-VALUES TO WH-FDET.
           CALL "CBL_CHECK_FILE_EXIST" USING WN-SEDAS WH-FDET.
           IF WF-MO = 0 GO G.
           CALL "CAUP" USING "0708110359000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " alte Bestelldatei " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY WN-SEDAS with highlight AT VDU-LP.
           ADD 15 TO VDU-LP.
           MOVE WF-SIZE TO WD-SIZE.
           DISPLAY WD-SIZE AT VDU-LP.
           MOVE WF-TT TO VDU-TAG.
           MOVE WF-MO TO VDU-MONAT.
           MOVE WF-YY TO VDU-JAHR.
           MOVE WF-HH TO VDU-STD.
           MOVE ":" TO VDU-DD.
           MOVE WF-MM TO VDU-MIN.
           ADD 12 TO VDU-P.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " -".
           ADD 11 TO VDU-P.
           DISPLAY VDU-ZEIT with highlight AT VDU-LP " " " vorhanden! "
                with highlight foreground-color 6 blink.
       E.  DISPLAY "<ret>= Åbernehmen, <Entf>= lîschen, <esc>= Abbruch <
      -        " >" AT 2301.
           CALL "CAUP" USING "0023530000" WH-CREG.
           IF ESC GO X.
           IF ENTF DELETE FILE SEDAS
                   CALL "CAUP" USING "08CLOFEN" WH-CREG
                   GO G.
           IF RET GO X.
       G.  MOVE LOW-VALUES TO WH-FDET.
           CALL "CBL_CHECK_FILE_EXIST" USING
               WN-BEST WH-FDET RETURN-CODE.
           IF WF-MO NOT = 0 GO R.
           CALL "CAUP" USING "0705050461000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 12 or
               WN-SEDAS(WX:1) = SPACE CONTINUE.
           ADD -1 TO WX.
           DISPLAY "Bestell-Datei <           > ist leer oder nicht vorh
      -        "anden!" with highlight foreground-color 6 AT VDU-LP.
           ADD 218 VDU-ECK GIVING VDU-LP.
           DISPLAY WN-SEDAS(1:WX) with background-color 2
                foreground-color 4 AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Weiter mit <ret> < >" AT VDU-LP.
           CALL "CAUP" USING "1003210000" WH-CREG.
           SET ESC TO TRUE.
           GO X.
       R.  CALL "CBL_COPY_FILE" using WN-BEST WN-SEDAS RETURN-CODE.
           CALL "CBL_DELETE_FILE" USING WN-BEST.
           SET RET TO TRUE.
           GO Z.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************** Sedas/Ecodex-Datei holen *
       HOL-SEDA SECTION.
       A.  MOVE "N:\EDI-TIE\EMG\EMG_W.40\ORDERS.IN\ORDERS" TO WN-BEST.
           PERFORM VARYING WX FROM 40 BY -1 UNTIL WX = 0
              IF WN-BEST(WX:1) = "\" exit perform.
           IF WX = 0 MOVE WN-BEST(WX:12) TO WN-BEST.
           ADD 1 TO WX.
           MOVE "ORDERS.DAT" TO WN-SEDAS.
           MOVE LOW-VALUES TO WH-FDET.
           CALL "CBL_CHECK_FILE_EXIST" USING WN-SEDAS WH-FDET.
           IF WF-MO = 0 GO G.
           CALL "CAUP" USING "0708110359000" WH-CREG.
           ADD 311 VDU-ECK GIVING VDU-LP.
           DISPLAY " alte " with highlight foreground-color 4 AT VDU-LP.
           ADD 317 VDU-ECK GIVING VDU-LP.
           DISPLAY "ORDERS.DAT" with highlight AT VDU-LP.
           ADD 12 TO VDU-LP.
           MOVE WF-SIZE TO WD-SIZE.
           DISPLAY WD-SIZE AT VDU-LP.
           MOVE WF-TT TO VDU-TAG.
           MOVE WF-MO TO VDU-MONAT.
           MOVE WF-YY TO VDU-JAHR.
           MOVE WF-HH TO VDU-STD.
           MOVE ":" TO VDU-DD.
           MOVE WF-MM TO VDU-MIN.
           ADD 12 TO VDU-P.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " -".
           ADD 11 TO VDU-P.
           DISPLAY VDU-ZEIT with highlight AT VDU-LP " " " vorhanden! "
                with highlight foreground-color 6 blink.
       E.  DISPLAY "<ret>= Åbernehmen, <Entf>= lîschen, <esc>= Abbruch <
      -        " >" AT 2301.
           CALL "CAUP" USING "0023530000" WH-CREG.
           IF ESC ADD 357 VDU-ECK GIVING VDU-LP
               DISPLAY " abgebrochen! " with highlight
                   foreground-color 6 AT VDU-LP
               GO X.
           IF ENTF DELETE FILE SEDAS
                   CALL "CAUP" USING "08CLOFEN" WH-CREG
                   GO G.
           IF RET GO X.
       G.  MOVE LOW-VALUES TO WH-FDET.
           CALL "CBL_CHECK_FILE_EXIST" USING
               WN-BEST WH-FDET RETURN-CODE.
           IF WF-MO NOT = 0 GO R.
           CALL "CAUP" USING "0705050461000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 12 or
               WN-SEDAS(WX:1) = SPACE CONTINUE.
           ADD -1 TO WX.
           DISPLAY "Bestell-Datei <           > ist leer oder nicht vorh
      -        "anden!" with highlight foreground-color 6 AT VDU-LP.
           ADD 218 VDU-ECK GIVING VDU-LP.
           DISPLAY WN-SEDAS(1:WX) with background-color 2
                foreground-color 4 AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Weiter mit <ret> < >" AT VDU-LP.
           CALL "CAUP" USING "1003210000" WH-CREG.
           SET ESC TO TRUE.
           GO X.
       R.  CALL "CBL_COPY_FILE" using WN-BEST WN-SEDAS RETURN-CODE.
           CALL "CBL_DELETE_FILE" USING WN-BEST.
           SET RET TO TRUE.
           GO Z.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *********************************** Sicherung SEDAS-Bestelldatei *
       SEDASICH SECTION.
       A.  MOVE LOW-VALUES TO WH-FDET.
           CALL "CBL_CHECK_FILE_EXIST" USING WN-SEDAS WH-FDET.
           MOVE WF-TT TO WZ-TAG.
           MOVE WF-MO TO WZ-MONAT.
           PERFORM VARYING WX FROM 12 BY -1 UNTIL WX = 0 or
               WN-SEDAS(WX:1) NOT = SPACE CONTINUE.
           ADD -1 TO WX.
           MOVE "ORDERS.DAT" TO WN-BEST.
           MOVE 0 TO WM-ZL.
       C.  MOVE WM-ZL TO WN-BEST(WX:2).
           MOVE LOW-VALUES TO WH-FDET.
           CALL "CBL_CHECK_FILE_EXIST" USING WN-BEST WH-FDET.
           IF WF-TT = WZ-TAG and WF-MO = WZ-MONAT
               ADD 1 TO WM-ZL GO C.
           CALL "CBL_COPY_FILE" USING WN-SEDAS WN-BEST RETURN-CODE.
       Z.  EXIT.
      ********************************************* Artikel Sortierung *
       KUSORT SECTION.
       A.  IF KU-SCAN not = 1
               and KU-SCAN not = 2
               and KU-SCAN not = 3
               and KU-SCAN not = 5                        *> geÑndert
               and KU-SCAN not = 9                        *> geÑndert
               and KU-SCAN not = 474
               and KU-SCAN not = 651
               and KU-SCAN not = 652
               and KU-SCAN not = 653
               and KU-SCAN not = 654
               and KU-SCAN not = 658 MOVE 0 TO BP-SORT.
       Z.  EXIT.
      ******************************************************************
       BESTELLUNG-UEBERTRAGEN SECTION.
       A.  MOVE 0 TO WH-LFD-01 WH-LFD-03 WH-LFD-04.
           MOVE 0 TO WH-BEST-KEY.
           MOVE 0 TO WZ-SEQ WS.
           MOVE SPACE TO WH-BK-SATZ.
       C.  IF WH-BEST-KEY = 0 MOVE 1 TO WH-BEST-KEY GO E.
           MOVE SPACE TO SEDAS-SATZ.
           READ SEDAS END GO Q.
           ADD 1 TO WH-BEST-KEY.
       E.  ADD 1003 VDU-ECK GIVING VDU-LP.
           MOVE WH-BEST-KEY TO WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
      *----------------------------------------> Sequenznummer prÅfen <-
           EVALUATE EF-SA                                    *> Edifact
                    WHEN "UNB" PERFORM BESTVORL
                    WHEN "HDR" PERFORM BEST-KOPF
                    WHEN "ITM" PERFORM BEST-ZEIL.
           IF EF-SA = "FIN" GO Z.
       G.  IF WH-BEST-KEY > KT-NUMMER
              MOVE WH-BEST-KEY TO KT-NUMMER
              REWRITE KT-SATZ.
           GO C.
       Q.  PERFORM OB-WRITE-BK.               *> letzten Kopf schreiben
       Z.  EXIT.
      ************************************************ Fehlerprotokoll *
       FEHLPROT SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO C.
           IF WM-OPEN = 0
               DISPLAY "Fehlerprotkoll! " with highlight
                   foreground-color 4 AT 0350
               MOVE "FEHLxx.LST" TO WH-DRUNAM
               MOVE WK-BS TO WH-DRUNAM(5:2)
               IF WM-DRU = 0 MOVE 5 TO WX-DR
                        else MOVE 0 TO WX-DR
               end-if
               PERFORM BEG-DRU
               MOVE 0 TO WZ-ZEILEN.
           MOVE 0 TO WZ-SCHALT.
           MOVE "Edifact-Fehlerprotokoll: " TO DRC-STR.
           MOVE WM-DATUM TO WC-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE VDU-DATUM TO DRC-STR(26:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRC-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       C.  IF EF-TXT not = SPACE MOVE KU-KTONR TO DRC-EANA
               MOVE KU-MCODE TO DRC-BEZ
               MOVE EF-TXT TO DRC-ANMERK
               PERFORM DRUCK
               GO Z.
           IF EF-SA = "FEL" MOVE "Artikel fehlt" TO AR-BEZ
               MOVE "Kd.Nr.:" TO DRC-ANMERK
               MOVE KU-KTONR TO WD-NUM
               MOVE WD-NUM TO DRC-ANMERK(9:)
               GO G.
           IF EF-SA = "MD" MOVE KU-BAN TO DRC-EANK
               MOVE "GLN-Nr. falsch" TO DRC-ANMERK
               MOVE "siehe Datei 'EMAILBES\" TO DRC-STR(15:)
               MOVE WH-GESF TO DRC-STR(27:22)
               PERFORM DRUCK
               GO Z.
           MOVE KU-BAN TO DRC-EANK.
           MOVE "GLN-Nr. falsch" TO DRC-BEZ.
           MOVE "Kunde nicht vorhanden!" TO DRC-ANMERK.
           PERFORM DRUCK.
       E.  MOVE SPACE TO SEDAS-SATZ.
           READ SEDAS END MOVE "FIN" TO EF-SA GO Z.
           ADD 1 TO WH-BEST-KEY.
           EVALUATE EF-SA
              WHEN "UNB"
              WHEN "HDR" GO Z.
           INSPECT EF-IARTNR REPLACING ALL SPACES BY ZEROS.
           MOVE EF-IARTNR(1:13) TO KN-EAN.
           MOVE 0 TO KN-NUM.
           MOVE KN-NUM TO AR-NUM.
           READ KNOTEN IGNORE LOCK
               INVALID MOVE "Artikel nicht gelistet" TO AR-BEZ GO G.
           MOVE "A" TO AR-ART.
           READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel fehlt" TO AR-BEZ.
       G.  MOVE KN-EAN TO DRC-EANA.
           MOVE AR-BEZ TO DRC-BEZ.
           INSPECT EF-IMENGE REPLACING ALL SPACES BY ZEROS.
           MOVE EF-IMENGE TO DRC-MENGE.
           IF KU-KTONR = 0 MOVE DBD-SATZ TO DRC-SATZ.
           PERFORM DRUCK.
           IF EF-SA not = "FEL" GO E.
       Z.  EXIT.
      ****************************************** Edifact Bestellheader *
       BESTVORL SECTION.
       A.  MOVE EF-MBSEND(1:13) TO WH-NUM(1:).
           MOVE WH-NUM TO KU-BAN.
       Z.  EXIT.
      ***************************************** Werte von alpha -> num *
       HOLWERT SECTION.
       A.  MOVE 0 TO WH-NUM.
           PERFORM VARYING WX FROM WM-MAX BY -1 UNTIL WX = 0
               or WT-TX(WX:1) not = SPACE CONTINUE.
           MOVE 13 TO WY.
           PERFORM VARYING WX FROM WX BY -1 UNTIL WX = 0
               MOVE WT-TX(WX:1) TO WH-NUM(WY:1)
               ADD -1 TO WY.
       Z.  EXIT.
      ********************************** Bestellkopf am Ende schreiben *
       OB-WRITE-BK SECTION.
       A.  IF WH-BK-SATZ = SPACE GO Z.
           MOVE WH-BK-SATZ TO BK-SATZ.
           PERFORM WRITE-BKOPF.
           MOVE SPACE TO WH-BK-SATZ.
       Z.  EXIT.
      ******************************************** Edifact Bestellkopf *
       BEST-KOPF SECTION.
       A.  PERFORM OB-WRITE-BK.
           MOVE 9 TO WH-MOD WM-LOEKO.
           INSPECT EF-BBNEMPF REPLACING ALL SPACES BY ZEROS.
           IF EF-BBNBEST(1:13) = EF-BBNKND(1:13)
                MOVE EF-BBNEMPF(1:13) TO WH-NUM
           else MOVE EF-BBNBEST(1:13) TO WH-NUM.
      *------------------------------> bei SPAR steht der Besteller da -
           IF EF-GLNWE not = SPACE,
SPAR           IF EF-GLNWE(1:13) not = EF-BBNBEST(1:13)
SPAR               MOVE EF-GLNWE(1:13) TO WH-NUM.
           MOVE WH-NUM TO KU-BAN.
           READ KUNDEN IGNORE LOCK KEY IS KU-EANK NOT INVALID GO G.
           PERFORM FEHLPROT.                       *> wenn Kunde falsch
           GO X.
       G.  INITIALIZE BK-SATZ.
           MOVE EF-MESREF TO WT-TX.
           MOVE 13 TO WM-MAX.
           PERFORM HOLWERT.
           MOVE EF-BESTDAT(3:8) TO WZ-DATUM(1:) WC-DATUM(1:).
           IF WZ-DATUM(1:) = SPACE MOVE 0 TO WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE WX-DATUM TO WS-DATUM BK-BEDAT WM-BEDAT.
           MOVE EF-BESTNR TO BK-BENR WM-BENR.
           MOVE KU-KTONR TO BK-KDNUM.
           MOVE EF-BESTTER(3:8) TO WZ-DATUM(1:).
           IF WZ-DATUM(1:) = SPACE MOVE 0 TO WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE WX-DATUM TO WS-DATUM BK-LIEFDAT.
           MOVE 0 TO WS.
           MOVE BK-SATZ TO WH-BK-SATZ.
           IF EF-TXT not = SPACE PERFORM FEHLPROT.
       X.  UNLOCK BESTKOPF.
       Z.  EXIT.
      ******************************************* Edifact Artikelzeile *
       BEST-ZEIL SECTION.
       A.  INITIALIZE BP-SATZ.
           INSPECT EF-IARTNR REPLACING ALL SPACES BY ZEROS.
           MOVE EF-IARTNR(1:13) TO KN-EAN WK-EAN.
           PERFORM ART-PRF.                        *> Art./ Preis-Prfg.
       I.  INSPECT EF-IMENGE REPLACING ALL SPACES BY ZEROS.
           IF WH-BEST-KEY NOT > KT-NUMMER GO Z.
           IF EF-ILFDAT = SPACE MOVE 0 TO BP-LIEFDAT
           else MOVE EF-ILFDAT TO WH-FELD
                INSPECT WH-FELD REPLACING ALL SPACES BY ZEROS
                MOVE WR-DAT TO BP-LIEFDAT.
           PERFORM LIEFDAT.
           IF WH-MOD = 2 MOVE KT-DAT-A TO BP-LIEFDAT.
           IF WH-MOD = 3 MOVE KT-DAT-B TO BP-LIEFDAT.
           IF WH-MOD = 4 MOVE KT-DAT-C TO BP-LIEFDAT.
           MOVE EF-IMENGE TO BP-MENGE.
           SET RET TO TRUE.
           IF WH-MOD = 1;
               IF WM-LOEKO = 9 PERFORM LOE-KOPF
               end-if
               GO Z.
           IF BK-LIEFDAT not = BP-LIEFDAT
               PERFORM LOE-KOPF
               MOVE BP-LIEFDAT TO BK-LIEFDAT
               PERFORM OB-FOLGE
               IF ENTF GO Z
               end-if
               MOVE BK-SATZ TO WH-BK-SATZ
           else PERFORM OB-FOLGE
                IF ENTF GO Z.
           MOVE WH-BK-SATZ TO BK-SATZ.
           ADD 1 TO BK-ANZPOS.
           MOVE BK-KDNUM TO BP-KDNUM.
           MOVE BK-ZWO TO BP-ZWO.
           MOVE BK-ANZPOS TO BP-POS.             *> Achtung ob sinnvoll
           MOVE BP-ARTNR TO BP-SORT.
           IF AR-NUM = 9999 GO Z.
      *----------------------------------> Text von SUB-Nr. abstellen <-
           IF BP-ARTNR not = AR-NUM PERFORM READ-KALIB.
           MOVE WM-BEDAT TO BK-BEDAT BP-BESDAT.
           MOVE WM-BENR TO BK-BENR BP-BESNR.
           PERFORM KUSORT.

      *    if wm-datum = 20060829 go z.

       K.  WRITE BP-SATZ INVALID ADD 1 TO BP-POS GO K.
           MOVE BP-SATZ TO SBP-SATZ.
           WRITE SBP-SATZ INVALID REWRITE SBP-SATZ.
           MOVE BK-SATZ TO WH-BK-SATZ.
           MOVE 1 TO WS.
           PERFORM ADD-PLAN.                           *> Planungsdatei
       Z.  EXIT.
      **************************************** Artikel u. Preis prÅfen *
       ART-PRF SECTION.
       A.  IF WK-EAN(1:7) not = 9001456 GO E.           *>TITZ Best-Nr.
           IF WK-EAN(8:5) < 60000 GO E.
           MOVE WK-EAN(8:5) TO WH-NUM(9:5).
           MOVE WH-NUM TO BN-BNUM.
           READ BESTNUM IGNORE LOCK KEY IS BN-AKEY INVALID
               MOVE "FEL" TO EF-SA
               PERFORM FEHLPROT                  *> wenn Artikel falsch
               MOVE 9999 TO AR-NUM BN-BNUM.
           MOVE BN-NUM TO AR-NUM PR-ARTNR BP-ARTNR.
           GO G.
       E.  READ KNOTEN IGNORE LOCK INVALID MOVE "FEL" TO EF-SA
               PERFORM FEHLPROT                  *> wenn Artikel falsch
               MOVE 9999 TO AR-NUM KN-NUM.
           MOVE KN-NUM TO AR-NUM PR-ARTNR BP-ARTNR.
       G.  MOVE SPACE TO BP-ARTBEZ BP-MEH.
           PERFORM READ-ARTIK.
      *--------------------------------------> und prÅfen ob gelistet <-
           MOVE KU-PREISE TO PR-NR.                     *> mu· so sein!
           MOVE "P" TO PR-SA.
           READ PREISLI IGNORE LOCK not INVALID GO Z.
           IF AR-PREIS not = 999 GO Z.
           MOVE "FEL" TO EF-SA.
           PERFORM FEHLPROT.
           MOVE 9999 TO AR-NUM KN-NUM.
       Z.  EXIT.
      ****************************************** Kalibertext abstellen *
       READ-KALIB SECTION.
       A.  IF AR-NUM = 9999 GO Z.
           MOVE AR-NUM TO KA-ARNUM.
           READ KALIBER IGNORE LOCK INVALID INITIALIZE KA-SATZ.
           COMPUTE WH-WERT = BP-ARTNR - AR-NUM.
           COMPUTE WQ = WH-WERT * 100.
           IF KA-ZUSBEZ(WQ) not = SPACE MOVE KA-ZUSBEZ(WQ) TO BP-ARTBEZ.
       Z.  EXIT.
      ********************************************* ob Folgebestellung *
       OB-FOLGE SECTION.
       A.  READ BESTKOPF INVALID GO Z.
           MOVE BK-SATZ TO WH-BK-SATZ.
           IF KU-BESTKZ = 1 MOVE 1 TO BK-ZWO.
      *-------------------------------------> Ausgabe Folgebestellung <-
           CALL "CAUP" USING "27100704600011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Nachbestellung " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunde: " AT VDU-LP.
           MOVE KU-KTONR TO WD-NUM.
           DISPLAY WD-NUM with highlight AT 0000 " "
               KU-MCODE with highlight foreground-color 6.
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE AR-NUM TO WD-ARTNR.
           MOVE AR-NK TO WH-NK.
           MOVE BP-MENGE TO WH-ANZ.
           PERFORM MG.
           DISPLAY WD-ARTNR with highlight AT VDU-LP " "
               AR-BEZ with highlight WD-MENGE with highlight.

      *    if wm-datum = 20060829 SET ENTF TO TRUE GO X.

       Q.  DISPLAY "<Entf>= lîschen, <ret>= weiter < >" AT 2301.
           CALL "CAUP" USING "0023330000" WH-CREG.
           IF RET GO X.
           IF not ENTF GO Q.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************** Sicherungsdatei schreiben *
       WRITE-BKOPF SECTION.
       A.  WRITE BK-SATZ INVALID REWRITE BK-SATZ.
           MOVE BK-SATZ TO SBK-SATZ.
           WRITE SBK-SATZ INVALID REWRITE SBK-SATZ.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  CALL "CAUP" USING "04DATDR" WH-CREG.
       Z.  EXIT.
      *************************************************** Kopf lîschen *
       LOE-KOPF SECTION.
       A.  MOVE BK-SATZ TO SBK-SATZ.
           DELETE SIBEKOPF INVALID NEXT SENTENCE.
           DELETE BESTKOPF INVALID NEXT SENTENCE.
       Z.  EXIT.
      ******************************************************************
       READ-ARTIK SECTION.
       A.  MOVE "A" TO AR-ART.
           MOVE BP-ARTNR TO AR-NUM.
           READ ARTIKEL IGNORE LOCK
              INVALID MOVE "nicht angelegt!" TO AR-BEZ.
           MOVE AR-BEZ TO BP-ARTBEZ.
           MOVE AR-MEH TO BP-MEH.
           MOVE 0 TO BP-MIN.
           MOVE 99999 TO BP-MAX.
           MOVE AR-SCAN TO BP-SCAN.
       Z.  EXIT.
      ******************************************* énderung Lieferdatum *
       LIEFDAT SECTION.
       A.  IF BP-LIEFDAT = 0 MOVE WS-DATUM TO BP-LIEFDAT.
           IF BP-LIEFDAT = KT-DAT-A or KT-DAT-B or KT-DAT-C GO Z.
           IF WH-MOD not = 9 GO Z.
           CALL "CAUP" USING "0714100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lieferdatumkontrolle " with highlight reverse-video
               AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kd.-Nr.: " AT VDU-LP.
           MOVE BK-KDNUM TO KU-KTONR.
           MOVE "K" TO KU-SA.
           READ KUNDEN IGNORE LOCK
                INVALID MOVE "Kunde fehlt" TO KU-MCODE.
           MOVE KU-KTONR TO WD-NUM.
           ADD 212 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           ADD 10 TO VDU-LP.
           DISPLAY KU-MCODE with highlight foreground-color 6 AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Datum lt. Bestellung:           Lieferdatum:"
               AT VDU-LP.
           ADD 325 VDU-ECK GIVING VDU-LP.
           MOVE BP-LIEFDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 348 VDU-ECK GIVING VDU-LP.
           MOVE KT-DAT-A TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE KU-KTONR TO WH-NUM.
           CALL "TIANZEIG" USING "15TOSUCH" WH-CREG.
           CANCEL "TIANZEIG".
       E.  ADD 403 1309 GIVING VDU-LP.
           MOVE 0 TO WH-MOD.
           DISPLAY "<Esc>= lîschen, <ret>= ok, <Einfg>= Datum Ñndern < >
      -        " " AT VDU-LP.
           DISPLAY "3+<Einfg>= 3. Liefer-, 2+<Einfg>= 2. Liefer-, <Einfg
      -        ">= 1. Lieferdatum" with highlight AT 2401.
           CALL "CAUP" USING "0017621001" WH-CREG.
           IF VDU-ECK not = 1309 CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF ESC MOVE 1 TO WH-MOD GO X.
           IF EINF;
              EVALUATE WH-NUM
                  WHEN = 0 MOVE 2 TO WH-MOD GO X
                  WHEN = 2 MOVE 3 TO WH-MOD GO X
                  WHEN = 3 MOVE 4 TO WH-MOD GO X
                  WHEN OTHER GO E.
           IF NOT RET GO E.
           IF BP-LIEFDAT = 0 GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       MG SECTION.
       A.  EVALUATE WR-NK
               WHEN 0
               WHEN 1 MOVE WH-ANZ TO WD-ANZG
                      MOVE WD-ANZG TO WD-MENGE
                      DISPLAY WD-MENGE(1:7) with highlight AT VDU-LP
               WHEN 2 MOVE WH-ANZ TO WD-ANZ2
                      MOVE WD-ANZ2 TO WD-MENGE
                      DISPLAY WD-MENGE with highlight AT VDU-LP.
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      ******************************************** Addition Planmengen *
       ADD-PLAN SECTION.
       A.  IF AR-SCAN = 1 or 6 and (KU-SCAN > 995 and < 999) GO C.
           GO Z.
       C.  MOVE "TIPRPLAN.DAT" TO WN-PLAN.
           OPEN I-O PLANUNG.
           MOVE BP-ARTNR TO PL-ARTNR.
           MOVE BK-LIEFDAT TO PL-DATUM.
           MOVE KU-KTONR TO PL-KTONR.
           READ PLANUNG INVALID MOVE 0 TO PL-BEST PL-ERL
               GO G.
           SUBTRACT WS-PLMG FROM PL-BEST.
       G.  ADD BP-MENGE TO PL-BEST.
           MOVE KU-MCODE TO PL-KUMCODE.
           ACCEPT WH-NUM FROM DATE.
           MOVE WH-NUM TO PL-EDAT.
           ACCEPT WH-NUM FROM TIME.
           COMPUTE PL-ZEIT = WH-NUM / 10000.
       X.  REWRITE PL-SATZ INVALID WRITE PL-SATZ.
           IF PL-BEST = 0 and PL-ERL = 0
               DELETE PLANUNG INVALID NEXT SENTENCE.
           CLOSE PLANUNG.
       Z.  EXIT.
      ************************************* DESADV-Billa von DHL lesen *
       DHL-DESADV SECTION.
       A.  OPEN I-O LKOPF LBEWEG SLKOPF SLBEWEG.
           MOVE "    DESADV" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "270505097000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Desadv-DHL " with highlight AT VDU-LP.
           MOVE 0 TO WL-ACT WL-ATTRIB WM-OPEN WB-OPEN WZ-ZEILEN.
           MOVE 0 TO WH-FTPERL.
           INITIALIZE WT-BTAB.
       B.  MOVE "DHLK\*.*" TO WL-FILEIN.
       C.  CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0;
               IF WH-FTPERL = 0 PERFORM HOL-DESADV
                                GO B
               else DISPLAY "Keine weiteren DESADV's (DHL)!" AT 2401
                    PERFORM WEITER
                    GO X.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           IF WL-FOUT(1:3) not = "TDE" GO C.
           PERFORM RAUS.
           MOVE "DHLK\" TO WN-EDIFAC.
           MOVE WL-FOUT  TO WN-EDIFAC(6:).
           PERFORM OB-VERARBEITEN.
           MOVE 1 TO WH-FTPERL.
           IF ESC GO K.
      *---------------------------------> verarbeite DESADV's sichern <-
           MOVE 1 TO WB-AC.                               *> read only
           MOVE 0 TO WB-DE WB-DV WB-FLG WB-OFS IC.
           MOVE SPACE TO WB-FH.
           MOVE 1 TO WB-CNT IQ.
           MOVE 10 TO WX.
           MOVE SPACE TO EDI-SATZ.
           MOVE 0 TO WM-KOPF.
           MOVE 0 TO WH-DTM WH-LFDAT WH-DP WH-BY
                     WH-UC WH-CN WH-IV WH-UC WH-OB WH-EMPF.
           CALL "CBL_OPEN_FILE" USING WN-EDIFAC WB-AC WB-DE WB-DV WB-FH.
       E.  MOVE SPACE TO WU-Z.
           CALL "CBL_READ_FILE" USING WB-FH WB-OFS WB-CNT WB-FLG WU-Z.
           IF RETURN-CODE not = 0 GO K.
           ADD WB-CNT TO WB-OFS.
           EVALUATE WU-Z
               WHEN x"0D"
               WHEN x"0A" NEXT SENTENCE
               WHEN  "'"  GO G
               WHEN OTHER MOVE WU-Z TO EDI-SATZ(IQ:1)
                          ADD 1 TO IQ.
           GO E.
      *-----------------------------------------> Segmente abarbeiten <-
       G.  MOVE 1 TO IQ IC.
           IF WB-OFS < 50
               EVALUATE EDI-SATZ(1:5)
                   WHEN "UNB+U"
                   WHEN "UNA:+" GO E
                   WHEN OTHER GO X.                     *> kein Edifact
           MOVE EDI-SATZ TO WT-GRP.
           MOVE SPACE TO WU-SEG WU-QUAL.
           SET RET TO TRUE.
           EVALUATE WT-GRP(1:4)
               WHEN "UNB+" NEXT SENTENCE
               WHEN "UNH+" PERFORM OB-DESADV
                           INITIALIZE LK-SATZ
               WHEN "BGM+" PERFORM BGM-NR                *> Lfs.-Nr.
               WHEN "DTM+" PERFORM DTM-DAT               *> Datum
               WHEN "RFF+" PERFORM RFF-NR                *> Lfs.-Nr.
               WHEN "NAD+" PERFORM NAD-DES
               WHEN "LIN+" PERFORM BEST-AR
                           PERFORM LFS-KOPF
               WHEN "QTY+" PERFORM QTY-MG
                           PERFORM LFS-ZEIL
               WHEN "UNS+"                           *> Kunde abgeschl.
               WHEN "UNT+" IF LK-NUM > 0 and LK-NUM < 999999
                               MOVE ALL "ƒ" TO DRA-SATZ(5:)
                               PERFORM DRUCK
                               MOVE 2 TO WZ-SCHALT
                               PERFORM DRUCK
                               GO E
               WHEN "UNZ+" MOVE 0 TO WM-KOPF.
           MOVE SPACE TO EDI-SATZ.
           IF not ESC GO E.
       K.  CALL "CBL_CLOSE_FILE" USING WB-FH.
           IF ESC GO X.
           CALL "CBL_DELETE_FILE" USING WN-EDIFAC.          *> lîschen
           GO C.
       X.  CLOSE LKOPF LBEWEG SLKOPF SLBEWEG.
           IF WB-OPEN = 1 PERFORM DES-SUM
                          PERFORM END-BDRU.
           IF WM-OPEN = 1 PERFORM END-DRU
               DISPLAY WH-DRUNAM with highlight foreground-color 4
                   AT 2401
               DISPLAY "Bitte Fehlerprotokoll drucken < >" AT 2301
               CALL "CAUP" USING "0023320000" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE 9 TO WH-PG.
           MOVE "TIBEST  10BEST" TO WT-TX.
       Z.  EXIT.
      ************************************************** Desadv-Summen *
       DES-SUM SECTION.
       A.  MOVE ALL "ƒ" TO DBG-STR.
           PERFORM B-DRUCK.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 25
               IF WT-BARNUM(WX) not = 0
                   MOVE WT-BBEZ(WX) TO DBG-STR(30:)
                   MOVE WT-BMG(WX) TO DBG-BANZ
                   MOVE WT-BARNUM(WX) TO DBG-ARNUM
                   IF DBG-ARNUM(6:3) = ",00"
                       MOVE SPACE TO DBG-ARNUM(6:3)
                   end-if
                   IF WT-STKVER(WX) = 1 MOVE WT-ANZ(WX) TO DBG-ANZ
                                        MOVE "Pk." TO DBG-MEH
                                   else MOVE WT-MG(WX) TO DBG-MG
                                        MOVE "Ts." TO DBG-MEH
                   end-if
                   MOVE WT-BEAN(WX) TO DBG-EAN
                   PERFORM B-DRUCK.
       Z.  EXIT.
      ******************************************************************
       NAD-DES SECTION.
       A.  PERFORM NAD-S.
           IF EDI-SATZ(1:6) = "NAD+CN" ADD 303 VDU-ECK GIVING VDU-LP
               DISPLAY "Kunden-GLN: " AT VDU-LP
                   WH-BY with highlight " " KU-MCODE with highlight.
       Z.  EXIT.
      ******************************************************************
       OB-VERARBEITEN SECTION.
       A.  MOVE "DESADV\TDE00000.DHL" TO WN-SICH.
           MOVE WN-EDIFAC(6:8) TO WN-SICH(8:8).
           MOVE LOW-VALUES TO WH-FDET.
           CALL "CBL_CHECK_FILE_EXIST" USING WN-SICH WH-FDET.
           IF WF-SIZE = 0 GO G.
           DISPLAY "Bereits verarbeitet! < >" with highlight
                               foreground-color 4 AT 2301
              " <Einfg>= trotzdem verarbeiten, <ret>= Abbruch".
           CALL "CAUP" USING "0023230000" WH-CREG.
           IF EINF GO X.
           SET ESC TO TRUE.
           GO X.
       G.  DISPLAY "verarbeiten? < >"  with highlight AT 2301
              " <esc>= Abbruch, <ret>= Verarbeitung starten".
           CALL "CAUP" USING "0023150000" WH-CREG.
           IF ESC GO X.
           IF not RET GO G.
           CALL "CBL_COPY_FILE" USING WN-EDIFAC WN-SICH.
       X.  CALL "CAUP" USING "1323012580" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       OB-DESADV SECTION.
       A.  SET ESC TO TRUE.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = "'"
               IF WT-GRP(WZ:6) = "DESADV" SET RET TO TRUE
               exit perform.
       Z.  EXIT.
      ***************************************** Tour suchen + einfÅgen *
       HOL-TOUR SECTION.
       A.  INITIALIZE TO-SATZ.
           MOVE KU-KTONR TO TO-KUNR.
           MOVE 810 TO TO-NR.
           START TOUREN KEY > TO-BKEY INVALID GO G.
       C.  READ TOUREN NEXT AT END GO G.
           IF TO-NR not = 810 GO G.
           IF KU-KTONR not = TO-KUNR GO C.
           GO Z.
       G.  INITIALIZE TO-SATZ.
       Z.  EXIT.
      ***************************************** Lieferschein erstellen *
       LFS-KOPF SECTION.
       A.  IF LK-NUM not = 0 or STRAUS GO Z.
           IF KU-BEZ(1:12) = "Kunde fehlt!" MOVE 999999 TO KU-KTONR
               GO X.
      *--------------------------------------> Tour suchen + einfÅgen <-
           PERFORM HOL-TOUR.
           MOVE "O" TO LK-STATUS.
           COMPUTE LK-TOUR = TO-NR * 1000 + TO-LFD.
           MOVE KU-KTONR TO LK-KUNDE.
           MOVE KU-MCODE TO LK-MCODE.            *> nicht bei REART 25
           MOVE WH-LFDAT TO LK-LIEFDAT WS-DATUM.
           MOVE WU-TEXT TO LK-MCODE.             *> nur DHL Lfs-Nr.
           MOVE KU-KOND TO LK-KONDIT.
           MOVE LK-SATZ TO SLK-SATZ.
           START LKOPF KEY not < LK-RKEY INVALID GO G.
       C.  READ LKOPF NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LK-KUNDE not = KU-KTONR and WH-LFDAT not = LK-LIEFDAT
              GO G.
           IF LK-MCODE = SLK-MCODE
               DISPLAY "bereits verarbeitet" AT 2401
               PERFORM WEITER
               INITIALIZE LK-SATZ
               MOVE 999999 TO LK-NUM
               GO Z
           else GO C.
      *-------------------------------------------> Belegnummer holen <-
       G.  MOVE SLK-SATZ TO LK-SATZ.
           MOVE 1 TO WH-KEY.
           READ KONSTANT INVALID KEY
               DISPLAY "Konstantendatei fehlt" AT 2401 STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF WS-DATUM < KO-NEUJAHR
                MOVE KO-LFNUM TO LK-NUM
                ADD 1 TO KO-LFNUM
           else MOVE KO-NLFNUM TO LK-NUM
                ADD 1 TO KO-NLFNUM.
           REWRITE KO-SATZ.
           WRITE LK-SATZ INVALID GO G.               *> doch nicht frei
       K.  READ LKOPF.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE LK-SATZ TO SLK-SATZ.
           WRITE SLK-SATZ INVALID REWRITE SLK-SATZ.
       Q.  READ LKOPF INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Q.
           MOVE 1 TO WP-POS.
       X.  MOVE KU-BAN TO WH-EMPF.
      *    PERFORM DES-KOPF.
       Z.  EXIT.
      ******************************************************************
       DHL-DES-KOPF SECTION.
       A.  MOVE "DESADV von DHL - Lieferdatum: " TO DBG-STR.
           MOVE WH-LFDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DBG-STR(31:).
           MOVE "Seite:" TO DBG-STR(85:).
           MOVE WB-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DBG-STR(92:).
           MOVE 0 TO WB-SCHALT.
           PERFORM B-DRUCK.
           MOVE "Kto.Nr Kurzbezeichnung  DHL-lFS.Nr.   Lfs.Nr.    Datum
      *          12.123 123456789012345  1234567890112 123.456 12.12.45
      -         "lfd  Artikel      EAN-CODE   Menge Menge Meh"
                TO DBG-STR.
      *          345    2.345 1234567890123 1234,12-1.234-Pk
           PERFORM B-DRUCK.
           MOVE ALL "ƒ" TO DBG-STR.
           PERFORM B-DRUCK.
       Z.  EXIT
      *************************************-> DESADV-Protkoll drucken <-
       DES-ZEILEN SECTION.
       A.  IF WB-OPEN = 0 MOVE 0 TO WB-ZEILEN
               MOVE "DHL-DESA.LST" TO WB-DRUNAM
               MOVE 0 TO WB-ZEILEN WB-SCHALT
               MOVE 1 TO WB-OPEN WB-SEITE
               OPEN EXTEND BESTDRU
               MOVE "E(10U&l26a8c0O(s0p14h15v0b0s3T" TO DBG-STR
               WRITE DBG-SATZ AFTER 0.
               MOVE SPACE TO DBG-SATZ.
           IF WB-ZEILEN > 58 MOVE "-- Fortsetzung --" TO DBG-STR
               PERFORM B-DRUCK
               WRITE DBG-SATZ BEFORE PAGE
               MOVE SPACE TO DBG-SATZ
               MOVE 0 TO WB-ZEILEN
               ADD 1 TO WB-SEITE.
           IF WB-ZEILEN = 0 PERFORM DHL-DES-KOPF.
           IF WM-KOPF = 1 GO Q.
           MOVE 1 TO WM-KOPF.
           MOVE KU-KTONR TO DBG-KTONR.
           MOVE KU-MCODE TO DBG-KUBEZ.
           INSPECT DBG-KUBEZ REPLACING ALL "#" BY ",".
           MOVE WU-TEXT TO DBG-DHLLFS.
           MOVE LK-NUM TO DBG-LFSNR.
           MOVE WR-ORDNR TO DBG-ORDNR.
           MOVE WH-LFDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DBG-LFDAT.
       Q.  MOVE AR-NUM TO DBG-ARNUM.
           IF DBG-ARNUM(6:3) = ",00" MOVE SPACE TO DBG-ARNUM(6:3).
           MOVE KU-BAN TO DBG-EAN.
           MOVE 0 TO WH-NUM WH-WERT.
           EVALUATE LP-STKVER
               WHEN 0 MOVE LP-MENGE TO DBG-MG WH-WERT
      *               MOVE "kg" TO DBG-MEH
                      MOVE LP-MEH TO DBG-MEH
               WHEN 1 MOVE LP-ANZEFF TO DBG-MG WH-NUM
                      MOVE "   " TO DBG-MG(5:3)
                      MOVE LP-MEH TO DBG-MEH.
           MOVE WH-MG TO DBG-BANZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 25
               IF AR-NUM = WT-BARNUM(WX) or WT-BARNUM(WX) = 0
                   MOVE AR-NUM TO WT-BARNUM(WX)
                   ADD WH-WERT TO WT-MG(WX)                 *> kg-Summe
                   ADD WH-NUM  TO WT-ANZ(WX)                *> Pk-Summe
                   MOVE AR-BEZ TO WT-BBEZ(WX)
                   MOVE WH-EAN TO WT-BEAN(WX)
                   MOVE LP-STKVER TO WT-STKVER(WX)
                   ADD WH-MG TO WT-BMG(WX)                  *> B-Menge
               exit perform.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY DBD-SATZ(1:60) AT VDU-LP.
           PERFORM B-DRUCK.
       Z.  EXIT.
      ********************************************** Lieferscheinzeile *
       LFS-ZEIL SECTION.
       A.  IF STRAUS GO Z.                       *> kein Straus-Lfsch.
           IF LK-NUM = 999999 GO Z.              *> bereits verarbeitet
           IF WU-QUAL = "21 " MOVE WH-MG TO LP-ANZEFF
               GO Q.
           INITIALIZE LP-SATZ.
           IF STRAUS GO Z.
           MOVE LK-NUM TO LP-NUM.
           MOVE WP-POS TO LP-POS.
           MOVE AR-NUM TO LP-ARTNR.
           COMPUTE LP-MENGE rounded = WH-WERT.
           MOVE AR-NK TO LP-NK.
           PERFORM PREIS-BESTIMMEN.
           PERFORM OB-BESTELL-POSITION.
           MOVE BP-MENGE TO LP-ANZAHL LP-ANZEFF.
           MOVE AR-MEH TO LP-MEH.
           EVALUATE AR-NUM
               WHEN 4420 MOVE 10 TO WH-NUM
               WHEN 4021 MOVE 4 TO WH-NUM
               WHEN OTHER MOVE 1 TO WH-NUM.
           IF LP-STKVER = 1 MOVE LP-MENGE TO LP-ANZEFF
               COMPUTE LP-ANZEFF = LP-ANZEFF * WH-NUM
               COMPUTE LP-GEW = LP-ANZEFF * (AR-GEW + AR-BGEW / 2)
               MOVE LP-GEW TO LP-MENGE
               COMPUTE LP-BETRAG ROUNDED = LP-ANZEFF * LP-PREIS
           else COMPUTE LP-BETRAG ROUNDED = LP-MENGE * LP-PREIS.
           MOVE 0 TO LP-CHARGE.
           MOVE 0 TO LP-HALTG.
           IF WU-MEH = "KGM" GO Z.
           IF LP-PREIS = 999,99 or LP-BETRAG = 0 PERFORM DES-FEHL.
       Q.  WRITE LP-SATZ.
           ADD 1 LP-POS GIVING WP-POS.
           MOVE LP-SATZ TO SLP-SATZ.
           WRITE SLP-SATZ INVALID REWRITE SLP-SATZ.
           IF LP-POS = 1 MOVE LP-BESTNR TO LK-BESTNR
               MOVE LP-BESDAT TO LK-BESDAT
               REWRITE LK-SATZ
               end-rewrite
               MOVE LK-SATZ TO SLK-SATZ
               REWRITE SLK-SATZ
               end-rewrite.
      *-------------------------------------> DESADV-Protkoll drucken <-
           PERFORM DES-ZEILEN.
       Z.  EXIT.
      ****************************************** Fehlerprotokol DESADV *
       DES-FEHL SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO C.
           IF WM-OPEN = 0
               DISPLAY "Fehlerprotkoll! " with highlight
                   foreground-color 4 AT 0350
               MOVE "FEHLDExx.LST" TO WH-DRUNAM
               MOVE WK-BS TO WH-DRUNAM(7:2)
               IF WM-DRU = 0 MOVE 5 TO WX-DR
                        else MOVE 0 TO WX-DR
               end-if
               PERFORM BEG-DRU
               MOVE 0 TO WZ-ZEILEN.
           MOVE 0 TO WZ-SCHALT.
           MOVE "DHL-Desadv-Fehlerprotokoll: " TO DRC-STR.
           MOVE WM-DATUM TO WC-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE VDU-DATUM TO DRC-STR(26:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRC-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       C.  MOVE KU-KTONR TO DRF-KTONR.
           MOVE LK-NUM TO DRF-LFSNR.
           MOVE LP-ARTNR TO DRF-ARNUM.
           MOVE LP-MENGE TO DRF-MENGE.
           MOVE LP-PREIS TO DRF-PREIS.
           MOVE LK-MCODE TO DRF-LFSDHL.
           MOVE WH-EMPF TO DRF-EANK.
           MOVE WH-EAN TO DRF-EANA.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       PREIS-BESTIMMEN SECTION.
       A.  MOVE 0 TO LP-RABATT.
      *    MOVE "A" TO WM-SA.
           MOVE AR-PREIS TO LP-PREIS.         *> Artikelstammdatenpreis
           IF LK-KUNDE = -1 GO E.
           MOVE KU-PREISE TO PR-NR.                     *> Kundengruppe
           MOVE AR-NUM TO PR-ARTNR.
           MOVE "P" TO PR-SA.
       C.  READ PREISLI IGNORE LOCK INVALID GO E.
           MOVE PR-PREIS TO LP-PREIS.              *> Kundengruppenpreis
           IF WS-DATUM not < PR-VDAT AND WS-DATUM not > PR-BDAT
               MOVE PR-AKTION-KZ TO LP-AKTION-KZ
               MOVE PR-AKTION TO LP-PREIS.
           MOVE PR-STKVER TO LP-STKVER.
           MOVE PR-EAN TO LP-FEAN.
           MOVE PR-FPREIS TO LP-FPREIS.
           IF WS-DATUM not < PR-FVDAT AND WS-DATUM not > PR-FBDAT
               MOVE PR-FAKTION TO LP-FPREIS.
           IF KU-DRUCK-KZ = 22 and PR-FARTNR not = 0
               MOVE PR-FARTNR TO LP-FEAN.
      *--------------------------------> suchen bei Sonderkonditionen <-
       E.  IF LK-KUNDE = -1 GO Q.
           MOVE KU-KTONR TO PR-NR.                     *> Kunden-Nummer
           MOVE AR-NUM TO PR-ARTNR.
           MOVE "S" TO PR-SA.
       G.  READ PREISLI IGNORE LOCK INVALID KEY GO Q.
           IF PR-PREIS not = 0 MOVE PR-PREIS TO LP-PREIS
               MOVE PR-FPREIS TO LP-FPREIS.
           IF PR-AKTION = 0 GO I.
           IF WS-DATUM not < PR-VDAT AND WS-DATUM not > PR-BDAT
               MOVE PR-AKTION-KZ TO LP-AKTION-KZ
               MOVE PR-AKTION TO LP-PREIS.
      *        MOVE "S" TO WM-SA.
       I.  IF PR-FAKTION = 0 GO Q.
           IF WS-DATUM not < PR-FVDAT AND WS-DATUM not > PR-FBDAT
               MOVE PR-FAKTION TO LP-FPREIS.
       Q.  IF LP-PREIS = 999,00
               DISPLAY "Artikel nicht gelistet!" AT 2301
               DISPLAY "auf anderem Schirm anlegen!" AT 2401
               PERFORM WEITER
               GO A.
       Z.  EXIT.
      ******************************************************************
       OB-BESTELL-POSITION SECTION.
       A.  MOVE KU-KTONR TO BP-KDNUM.
           MOVE WH-LFDAT TO BP-LIEFDAT.
           MOVE 0 TO BP-SORT BP-ARTNR BP-POS BP-ERL BP-ZWO.
           START BESTPOS KEY NOT < BP-AKEY INVALID GO G.
       C.  READ BESTPOS NEXT IGNORE LOCK END GO G.
           IF BP-KDNUM not = KU-KTONR GO G.
           IF BP-LIEFDAT not = WH-LFDAT GO G.
           IF BP-ARTNR not = AR-NUM GO C.
      *-------------------------------------> Bestelldaten fÅr LFSCH. <-
           MOVE BP-BESNR TO LP-BESTNR LK-BESTNR.
           MOVE BP-BESDAT TO LP-BESDAT LK-BESDAT.
      *--------------> Bestellkopf f. Bestellnr. lesen bei nicht-SPAR <-
       G.  IF KU-KTONR = BK-KDNUM and WS-DATUM = BK-LIEFDAT GO Z.
           MOVE KU-KTONR TO BK-KDNUM.
           MOVE WS-DATUM TO BK-LIEFDAT.
       H.  READ BESTKOPF IGNORE LOCK INVALID GO Z.
       Z.  EXIT.
      ********************************************* DHL-DESADV abholen *
       HOL-DESADV SECTION.
       A.  IF WH-FTPERL > 0 GO Z.
      *-------------------------------------> PSFtp-Dateien erstellen <-
           MOVE "dhl.bat" TO WH-BATNAM.
           OPEN OUTPUT BATFILE.
           Move "psftp 80.121.146.245 -l titz -pw ttiktlz -b dhl.scr"
               TO BTF-SATZ.
           WRITE BTF-SATZ BEFORE 1.
           CLOSE BATFILE.
      *---------------------------------> erst BAT-File dann SCR-File <-
           MOVE "dhl.scr" TO WH-BATNAM.
           OPEN OUTPUT BATFILE.
           MOVE "lcd dhlk" TO BTF-SATZ.            *> ins Verz. DHL lok.
           WRITE BTF-SATZ BEFORE 1.
           MOVE "cd out" TO BTF-SATZ.              *> am DHL-Ftp-Server
           WRITE BTF-SATZ BEFORE 1.
           MOVE "cd desadv" TO BTF-SATZ.           *> am DHL-Ftp-Server
           WRITE BTF-SATZ BEFORE 1.
           MOVE "mget *.*" TO BTF-SATZ.            *> OUT -> DHL (ftp)
           WRITE BTF-SATZ BEFORE 1.
           MOVE "lcd .." TO BTF-SATZ.              *> eine Stufe zurÅck
           WRITE BTF-SATZ BEFORE 1.
           MOVE "mv *.* ../" TO BTF-SATZ.          *> move -> DESADV
           WRITE BTF-SATZ BEFORE 1.
           MOVE "quit" TO BTF-SATZ.
           WRITE BTF-SATZ BEFORE 1.
           CLOSE BATFILE.
           CALL "CAUP" USING "0703011980000" WH-CREG.
           CALL "CAUP" USING "1301012580" WH-CREG.
      *-----------------------------------------------> FTP-ausfÅhren <-
           MOVE "DHL.BAT" TO WD-UPON.
           DISPLAY LOW-VALUE AT 1801.
           DISPLAY WD-UPON UPON COMMAND-LINE.
      *-----------------------------------------------> Druck starten <-
           CALL X"91" USING RESULT FUNKT PARAM.         *> command-line
           MOVE 1 TO WH-FTPERL.
           MOVE "DIR DHLK\*.* >DHL-D.RES" TO WD-UPON.
           DISPLAY LOW-VALUE AT 1801.
           DISPLAY WD-UPON UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.         *> command-line
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Q.  CALL "CAUP" USING "16CRLFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
       Z.  EXIT.
