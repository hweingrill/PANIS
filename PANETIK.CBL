      $SET LINKCOUNT"384" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANETIK.
      *              SPAR Etikettendruck                              <-
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
           03  WV-KTONR                PIC 9(6)   COMP   VALUE ZERO.
           03  WM-KTONR                PIC 9(6)   COMP   VALUE ZERO.
           03  WM-NUM.
               05 WM-KTO               PIC 9(5).
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
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-NUM                  PIC ZZZ.ZZ9-.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  WH-MEH                  PIC 99         COMP.
           03  WM-KOND                 PIC 99         COMP.
           03  WK-GS                   PIC 999.
           03  WK-GSNR                 PIC 9(4).
           03  WK-DAT                  PIC X(8).
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WD-X                    PIC X.
           03  WD-ET                   PIC 999.
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
           MOVE 1 TO WZ-SCHALT.
           MOVE "ETIK.LST" TO WH-DRUNAM.
           IF WL-CA = 10 PERFORM NEU-ETIK.
           IF WL-CA = 20 MOVE WM TO WD-ET
                         PERFORM OPEN-DRU
                         PERFORM ETIK-START
                         PERFORM ART-ETIK
                         PERFORM COPY-DRU.
           IF WL-CA = 95 PERFORM TESTDRUCK GO X.
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
      ********************************************* EAN-Etikettendruck *
       NEU-ETIK SECTION.
       A.  MOVE 0 TO WV-KTONR WS-ETIK.
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
       E.  READ LFSCHEIN NEXT IGNORE LOCK AT END GO P.
           ADD 520 VDU-ECK GIVING VDU-LP.
           IF DE-KTONR not = LF-KTONR GO P.
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
               PERFORM OPEN-DRU
               PERFORM NAM-ETIK
               MOVE 1 TO WH-ANZ
               PERFORM ADD-DIS.
           MOVE WD-ET TO WH-ANZ.
           PERFORM ADD-DIS.
           PERFORM ART-ETIK.
           GO E.
      *-----------------------------------------> Druckdatei kopieren <-
       P.  PERFORM COPY-DRU.
           GO C.
       Q.  MOVE WS-ETIK TO WD-ETIK.
           ADD 540 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ETIK with highlight AT 2301 " Etiketten gedruckt!
      -        "weiter mit <ret> < >".
           CALL "CAUP" USING "0023460000" WH-CREG.
       Z.  EXIT.
      ******************************************** Druckdatei kopieren *
       COPY-DRU SECTION.
       A.  IF WM-OPEN = 1 MOVE 0 TO WM-OPEN
               CLOSE DRUCKER.
           CALL "CBL_COPY_FILE" USING "ETIK.LST " "LPT1 ".
       Z.  EXIT.
      ********************************************** Druckdatei îffnen *
       OPEN-DRU SECTION.
       A.  IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               DELETE FILE DRUCKER
               OPEN OUTPUT DRUCKER
               MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ************************************************ Anzahl ausgeben *
       ADD-DIS SECTION.
       A.  ADD WH-ANZ TO WS-ETIK.
           MOVE WS-ETIK TO WD-ETIK.
           ADD 540 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ETIK AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       ART-ETIK SECTION.
       A.  MOVE "N" TO DRE-SATZ
           PERFORM DRUCK.
           MOVE "A15,275,0,4,1,2,N,""" TO DRE-SATZ.
           MOVE ET-BEZ-A TO DRE-SATZ(20:).
           PERFORM FS-ZEI.
           MOVE "A15,350,0,1,1,2,N,""" TO DRE-SATZ.
           MOVE ET-ZU-A TO DRE-SATZ(20:).
           PERFORM FS-ZEI.
           MOVE "A15,375,0,1,1,2,N,""" TO DRE-SATZ.
           MOVE ET-ZU-B TO DRE-SATZ(20:).
           PERFORM FS-ZEI.
           MOVE "A15,400,0,1,1,2,N,""" TO DRE-SATZ.
           MOVE ET-ZU-C TO DRE-SATZ(20:).
           PERFORM FS-ZEI.
      *-----------------------------------------------> abgepackt am: <-
           MOVE "A15,440,0,2,1,1,N,""" TO DRE-SATZ.
           MOVE "abgep. am:" TO DRE-SATZ(20:).
           MOVE WS-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM(1:6) TO DRE-SATZ(31:).
           MOVE "20" TO DRE-SATZ(37:).
           MOVE VDU-DATUM(7:2) TO DRE-SATZ(39:).
           PERFORM FS-ZEI.
      *------------------------------------> mindestends haltbar bis: <-
           MOVE "A15,460,0,1,1,2,N,""" TO DRE-SATZ.
           MOVE "mindestens haltbar bis:" TO DRE-SATZ(20:).
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WS-DATUM).
           ADD ET-MHD TO WA-DAT.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT).
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM(1:6) TO DRE-SATZ(44:).
           MOVE "20" TO DRE-SATZ(50:).
           MOVE VDU-DATUM(7:2) TO DRE-SATZ(52:).
           PERFORM FS-ZEI.
      *----------------------------------------------> EAN-Strichcode <-
           MOVE "B15,520,0,E30,3,4,80,B,""" TO DRE-SATZ.
           MOVE ET-EAN TO DRE-SATZ(25:13).
           PERFORM FS-ZEI.
           MOVE "P" TO DRE-SATZ.
           MOVE WD-ET(1:3) TO DRE-SATZ(2:).
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
           MOVE """" TO DRE-SATZ(WZ:).
           PERFORM DRUCK.
       Z.  EXIT.
      ************************************************* Etikettenstart *
       ETIK-START SECTION.
       A.  MOVE "I8,0,049" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "Q633,32" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "q381" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "rN" TO DRE-SATZ.                      *> no double Buff
           PERFORM DRUCK
           MOVE "OD" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "S4" TO DRE-SATZ.                      *> Geschwindigkt
           PERFORM DRUCK
           MOVE "D9" TO DRE-SATZ.                      *> Density
           PERFORM DRUCK
           MOVE "ZB" TO DRE-SATZ.                      *> Druckrichtung
           PERFORM DRUCK.
       Z.  EXIT.
      ************************************************* Namens-Etikett *
       NAM-ETIK SECTION.
       A.  PERFORM ETIK-START.
           MOVE "N" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "A250,260,0,4,1,2,N,""" TO DRE-SATZ.
           COMPUTE WD-NUM = DE-KTONR / 10.
           MOVE WD-NUM(5:3) TO DRE-SATZ(21:).
           PERFORM FS-ZEI.
           PERFORM VARYING WS FROM 1 BY 1 UNTIL WS > 5
               EVALUATE WS
                   WHEN 1 MOVE "A20,320" TO DRE-SATZ
                   WHEN 2 MOVE "A20,380" TO DRE-SATZ
                   WHEN 3 MOVE "A20,440" TO DRE-SATZ
                   WHEN 4 MOVE "A20,500" TO DRE-SATZ
                   WHEN 5 MOVE "A20,560" TO DRE-SATZ
               end-evaluate
               MOVE ",0,4,1,2,N,""" TO DRE-SATZ(8:)
               MOVE WR-ADR(WS) TO DRE-SATZ(20:)
               PERFORM FS-ZEI.
           MOVE "P1" TO DRE-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
      ********************************************* EAN-Etikettendruck *
       TESTDRUCK SECTION.
       A.  PERFORM OPEN-DRU.
           MOVE WM-DATUM TO WS-DATUM.
           MOVE 1 TO WD-ET.
           PERFORM ART-ETIK.
           PERFORM COPY-DRU.
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
           MOVE WH-WERT TO WD-ET.
           DISPLAY WD-ET with highlight AT VDU-LP.
       G.  DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Start < >"
               AT 2301.
           CALL "CAUP" USING "0023460000" WH-CREG.
           IF WOLI or AUF GO E.
           IF ESC GO D.
           IF not RET GO G.
           OPEN OUTPUT DRUCKER.
           PERFORM ART-ETIK.
           CLOSE DRUCKER.
           GO D.
       X.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           CLOSE ETIKETT.
       Z.  EXIT.
