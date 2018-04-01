      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER SUPFF
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PANBSKOM.
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
           COPY PANSELFS.CPY.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANLIEF.CPY.
       COPY PANFAKT.CPY.
      ******************************** P3800 = PRINTER **** PC = LPT1 *
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(6).
           03  DRA-TX.
               05 FILLER               PIC X.
               05 DRA-ARNUM            PIC Z(12)9-.
               05 DRA-BEZ              PIC X(27).
               05 DRA-MENGE            PIC X(12).
               05 DRA-MEH              PIC XXX.
               05 FILLER               PIC X(16).
               05 DRA-BETRAG           PIC ZZZZ.ZZ9,99-.
           03  FILLER                  PIC X(44).
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
       01  WH-REG.
           03  WH-PX                   PIC XX      OCCURS 2.
           03  WM-OPEN                 PIC 9      COMP-3 VALUE ZERO.
           03  WH-KB                   PIC 99     COMP.
           03  WX-PRNO                 PIC 99     COMP-X VALUE ZERO.
           03  WX-PRSTAT               PIC 99     COMP-X.
           03  WZ-SEITE                PIC 99     COMP-X VALUE ZERO.
           03  WZ-SCHALT               PIC 99     COMP-X VALUE ZERO.
           03  WZ-ZEILEN               PIC 99     COMP-X VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WV-ARNUM                PIC 9(4)   COMP   VALUE ZERO.
           03  WV-TOUR                 PIC 9(4)   COMP   VALUE ZERO.
           03  WV-SUB                  PIC 9(4)   COMP   VALUE ZERO.
           03  WV-KTONR                PIC 9(6)   COMP   VALUE ZERO.
           03  WM-KTONR                PIC 9(6)   COMP   VALUE ZERO.
           03  WV-LFNUM                PIC 9(5)   COMP   VALUE ZERO.
           03  WH-ANZ                  PIC S9(8)V99  COMP-3.
           03  WA-ANZ                  PIC 9(5)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(5).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9999V9.
           03  WH-ANZ2                 PIC S999V99.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WLN                     PIC 99        COMP.
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
           03  WD-POS                  PIC ZZ9.
           03  WH-MEH                  PIC 99         COMP.
      *----------------------------------------------> Command-Line <-
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
           03  WK-ANR                  PIC 9.
           03  WH-PARM                 PIC X(45).
           03  WK-ART                  PIC 9.
           03  WD-X                    PIC X.
           03  VDU-POS.
               05 VL                   PIC X   COMP-X.
               05 VP                   PIC X   COMP-X.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING "PANEFAKT.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING "PANELFS.DAT "  WH-CREG.
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
           MOVE WH-NUM TO WK-ART.
           MOVE 2 TO WV-KTONR.
           IF WL-CA = 10 PERFORM BSKOM.
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
           CALL "CAUP" USING "1301010140000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 0101.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO VDU-L.
           IF VDU-L < 12 GO Z.
       B.  CALL "CAUP" USING "1403011240000" WH-CREG.
           SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ******************************************* MS-DOS/NETZ-Commands *
       COMM SECTION.
       A.  CALL X"91" USING RESULT FUNKT PARAM.
       Z.  EXIT.
      ***************************************** Mengenkommaumwandlung *
       MG SECTION.
       A.  IF WF-NK = 2 DIVIDE 100 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ2 WD-MGC
               MOVE WD-MGC TO WD-MG
               DISPLAY WD-MGC with foreground-color 14 AT VDU-LP
               MOVE WD-ANZ2 TO WD-MENGE.
           IF WF-NK = 1 DIVIDE 10 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ1 WD-MGB
               MOVE WD-MGB TO WD-MG
               DISPLAY WD-MGB with foreground-color 14 AT VDU-LP
               MOVE WD-ANZ1 TO WD-MENGE.
           IF WF-NK = 0 MOVE WH-ANZ TO WD-ANZ WD-MGA
               MOVE WD-MGA TO WD-MG
               DISPLAY WD-MGA with foreground-color 14 AT VDU-LP
               MOVE WD-ANZ TO WD-MENGE.
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      ************************************* Bildschirmkommissionierung *
       BSKOM SECTION.
       A.  IF WM-KO NOT = 3 GO Y.
           MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID KEY STOP RUN.
           MOVE KO-MERK TO WK-M.
           MOVE 0 TO WE-LFS.
           IF WM-VER > 10 ADD -10 TO WM-VER
                          MOVE 1 TO WE-LFS.              *> Lfsch. Kern
           MOVE 0 TO WM-KTONR WK WH-KB.
           MOVE WM-DATUM TO WV-DATUM.
           DIVIDE 100 INTO WV-DATUM GIVING WV-LFNUM REMAINDER WV-LFNUM.
           COMPUTE WV-LFNUM = WV-LFNUM * 100.
           IF WK-ART < 3 ADD WK-ART TO WV-LFNUM.
           MOVE 1 TO WI.
           IF WM-KO = 3 MOVE 2 TO WI.
           DISPLAY "MODE BW40" UPON COMMAND-LINE.
           PERFORM COMM.
           MOVE 0 TO VDU-L.
           MOVE 0 TO LF-TOUR LF-SUB LF-KTONR LF-NUM
                     LF-SGRP LF-ARNUM WLN WL.
       B.  MOVE WV-DATUM TO LF-DATUM.
           START LFSCHEIN KEY > LF-AKEY INVALID PERFORM NO-REC GO Y.
       C.  START LFSCHEIN KEY not < LF-AKEY INVALID GO X.
       D.  READ LFSCHEIN NEXT IGNORE LOCK AT END MOVE 66 TO WL GO L.
           IF WK-ART NOT > 2 MOVE LF-NUM TO WR-A2;
               IF WR-A2 NOT = WK-ART GO D.
           IF WK = 9; IF WV-TOUR NOT = LF-TOUR
                          MOVE 0 TO LF-SUB LF-KTONR LF-NUM LF-ARNUM
                          WM-KTONR WK VDU-L WV-LFNUM
                          MOVE WV-TOUR TO LF-TOUR GO B
                      ELSE IF WM-KTONR NOT = LF-KTONR
                               MOVE 9999 TO LF-ARNUM
                               MOVE WV-DATUM TO LF-DATUM GO C
                           ELSE MOVE 0 TO WK VDU-L.
           MOVE LF-GRM TO WF-GRM.
           IF WK = 0 PERFORM LFKOPF.
       E.  IF WV-KTONR > 2; IF WV-KTONR NOT = LF-KTONR
               OR LF-NUM NOT = WV-LFNUM
               OR LF-DATUM NOT = WV-DATUM MOVE 66 TO WL GO L.
           IF LF-DATUM > WV-DATUM MOVE 66 TO WL GO L.
           IF WM-KTONR = 0 MOVE LF-KTONR TO WM-KTONR.
           IF WV-LFNUM = 0 MOVE LF-NUM TO WV-LFNUM.
           IF LF-KTONR NOT = WM-KTONR MOVE 3 TO WLN GO L.
      *------------------------------------> Einz„hlplatz korrigieren <-
980430     IF WF-TB > 3 MOVE 3 TO WF-TB.   *> B„ckereiw. i. Kondit. einz
           IF WI = WF-TB AND LF-NUM = WV-LFNUM GO H.
           IF WK-ART < 3 MOVE 1 TO WL GO D.
           IF LF-NUM NOT = WV-LFNUM MOVE 3 TO WLN GO L.
           IF WI NOT = WF-TB MOVE 1 TO WL.
           GO D.
      *----------------------------------> Ende eines Lieferscheines <-
       G.  MOVE 0 TO WK.
      *-------------------------------------------> LF-AKEY abstellen <-
      *    MOVE LF-AKEY TO WH-LFAKEY.             > alte Version
           MOVE LF-DTKEY TO WQ-DTKEY.
           MOVE LF-PRIME TO WQ-PRIME.
           MOVE LF-SORT TO WQ-SORT.
           MOVE LF-ARNUM TO WQ-ARNUM.
      *-----------------------------------------------------------------
           MOVE WV-TOUR TO LF-TOUR.
           MOVE WV-LFNUM TO LF-NUM.
           MOVE WV-SUB TO LF-SUB.
           MOVE 0 TO VDU-LP.
           IF WH-KB = 0;
               IF RET CALL "PANLFDRU" USING "10SOFDRU" WH-CREG.
      *    MOVE WH-LFAKEY TO LF-AKEY.             > alte Version
           MOVE WQ-DTKEY TO LF-DTKEY.
           MOVE WQ-PRIME TO LF-PRIME.
           MOVE WQ-SORT TO LF-SORT.
           MOVE WQ-ARNUM TO LF-ARNUM.
           CALL "CAUP" USING "1301012540" WH-CREG.
           IF LF-DATUM > WV-DATUM MOVE 66 TO WL.
           IF WL = 66 MOVE 0101 TO VDU-LP
               DISPLAY "keine weiteren Daten -" AT VDU-LP
               PERFORM WEITER GO Y.
           IF WLN = 3 MOVE 0 TO WV-LFNUM.
           MOVE 0 TO WH-KB WK WL WLN.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE 0 TO WM-KTONR.
           GO C.
960106 H.  IF VDU-L > 10 DISPLAY  "***" with highlight BLINK AT 0237
960106*H.  IF VDU-L > 20 DISPLAY  "***" with highlight BLINK AT 0237
               MOVE 1 TO WH-KB
               GO L.
           IF WK = 0 PERFORM LFKOPF
               MOVE LF-NUM TO WV-LFNUM.
      *    ADD 2 TO VDU-L.
           ADD 1 TO VDU-L.
           MOVE LF-ARNUM TO WV-ARNUM.
930104*    IF WM-KO = 3 AND DE-RAST = 3 MOVE 9999 TO LF-ARNUM
930104*        MOVE 0 TO WM-KTONR WV-LFNUM WK GO B.
           MOVE LF-ARNUM TO WD-POS.
           MOVE 1 TO VDU-P.
           DISPLAY WD-POS with foreground-color 15 AT VDU-LP.
           ADD 4 TO VDU-LP.
           DISPLAY LF-BEZ with foreground-color 14 AT VDU-LP.
           ADD 26 TO VDU-LP.
           MOVE LF-GRM TO WF-GRM.
       K.  MOVE LF-MENGE(1) TO WH-ANZ.
           MOVE 30 TO VDU-P.
           PERFORM MG.
           ADD 1 WF-MEH GIVING WH-MEH.
           ADD 7 TO VDU-LP.
           IF LF-STAT = 98
               DISPLAY WT-MEH(WH-MEH) with highlight BLINK AT VDU-LP
           ELSE DISPLAY WT-MEH(WH-MEH) with highlight AT VDU-LP.
           GO D.
       L.  IF VDU-L = 00 AND WL = 66 GO Y.
           IF VDU-L = 2 ADD 1 TO VDU-L
               MOVE 10 TO VDU-P
               DISPLAY "kein Geb„ck" with foreground-color 14 AT VDU-LP
               MOVE 2 TO WK.
           MOVE 30 TO VDU-P.
           DISPLAY "<ret>= Druck, <esc>= Abbruch" AT 2301.
           DISPLAY "< >= Kunde, < >= Tour, <Ende>= Ende" AT 2401.
           IF WF-NK = 0 CALL "CAUP" USING "0000006007" WH-CREG.
           IF WF-NK = 1 CALL "CAUP" USING "0000005107" WH-CREG.
           IF WF-NK = 2 CALL "CAUP" USING "0000004207" WH-CREG.
           IF ENDE MOVE 0 TO WH-NUM GO M.
           IF ESC GO Y.
           IF AUF AND WH-NUM NOT = 0;
               COMPUTE WM-KTONR = WH-NUM * 10 + 2
               MOVE LF-TOUR TO WV-TOUR
               MOVE 0 TO LF-NUM LF-SUB LF-KTONR LF-ARNUM  WV-LFNUM
               MOVE 9 TO WK GO B.
           IF AB AND WH-NUM NOT = 0;
               MOVE WH-NUM TO LF-TOUR
               MOVE 0 TO LF-SUB LF-KTONR LF-NUM LF-ARNUM
                         WM-KTONR WK WV-LFNUM VDU-L GO B.
           IF NOT RET GO L.
       M.  IF WK = 2 GO G.
      *    MOVE WH-WERT TO WH-ANZ.
      *    MOVE LF-MENGE(1) TO WH-ANZ.
      *    PERFORM DISP-MENGE.
      *    IF ESC GO K.
      *    MOVE WH-ANZ TO LF-MENGE(1).
           GO G.
       X.  IF WS-TASTE = 99 MOVE 2 TO WM-KO
               MOVE 9 TO WS-TASTE.
           UNLOCK LFSCHEIN.
           IF WM-KO = 3 AND WI = 2 GO Y.
           IF WM-KO = 2 OR WM-KO = 1 GO Y.
           ADD 1 TO WI.
           IF WM-KO = 3; IF WI = 4 GO Y.
           IF WM-KO < 2; IF WI = 3 GO Y.
           MOVE 0 TO WM-KTONR WZ-ZEILEN LF-ARNUM LF-KTONR
               LF-NUM LF-TOUR LF-SUB WK.
           MOVE WV-DATUM TO LF-DATUM.
           GO C.
       Y.  DISPLAY "MODE CO80" UPON COMMAND-LINE.
           PERFORM COMM.
           CANCEL "PANLFDRU".
       Z.  EXIT.
      ******************************************************************
       LFKOPF SECTION.
       A.  MOVE 1 TO WK.
           IF ESC GO Z.
           IF DE-KTONR = LF-KTONR GO C.
           MOVE 1 TO DE-FNR.
           MOVE LF-KTONR TO DE-KTONR.
       B.  READ DEBITOR IGNORE LOCK
              INVALID MOVE "Kunde fehlt" TO DE-BEZ.
       C.  MOVE DE-BEZ TO WT-BEZ.
           PERFORM BS-ZEIL.
           CALL "CAUP" USING "1300012540" WH-CREG.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE 3 TO WZ-SCHALT.
           MOVE 0 TO WK-ANZ.
           MOVE LF-NUM TO WD-LFNUM.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE LF-TOUR TO WD-POS WV-TOUR.
           MOVE LF-SUB TO WV-SUB.
           MOVE 1 TO VDU-P.
       M.  ADD LF-TOUR 10 GIVING WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID
                MOVE "Tour nicht angelgt" TO KO-TOUR.
           DISPLAY WD-KTO AT VDU-LP " / " WD-POS ": " KO-TOUR(1:21)
               with highlight.
           DISPLAY WK-ART with highlight foreground-color 12 at 0140.
           IF WK-ART > 2 DISPLAY WD-LFNUM(6:1) with highlight
                foreground-color 13 at 0139.
           PERFORM BS-ZEIL.
           DISPLAY WR-ADR(1) with foreground-color 10 AT VDU-LP.
960106     PERFORM BS-ZEIL.
       Z.  EXIT.
      *****************************************************************
       DISP-MENGE SECTION.
       A.  MOVE WH-ANZ TO WA-ANZ.
           IF WF-NK = 0 GO C.
           IF WF-NK = 1 GO B.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ2 ON SIZE ERROR GO X.
           MOVE WH-ANZ2 TO WD-ANZ2 WD-MGC.
           MOVE WD-ANZ2 TO WD-MENGE.
           MOVE WD-MGC  TO WD-MG.
           MULTIPLY 100 BY WH-ANZ.
           DISPLAY WD-MGC with foreground-color 14 AT VDU-LP.
           GO Z.
       B.  IF WR-A2 NOT = 0 GO X.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ1 ON SIZE ERROR GO X.
           MOVE WH-ANZ1 TO WD-ANZ1 WD-MGB.
           MOVE WD-ANZ1 TO WD-MENGE.
           MOVE WD-MGB  TO WD-MG.
           MULTIPLY 10 BY WH-ANZ.
           DISPLAY WD-MGB with foreground-color 14 AT VDU-LP.
           GO Z.
       C.  IF WH-ANZ > 999999 OR WH-ANZ < -999999 GO X.
           IF WR-A2 NOT = 0 AND WR-A1 NOT = 0 GO X.
           MOVE WH-ANZ TO WD-ANZ WD-MGA.
           MOVE WD-ANZ TO WD-MENGE.
           MOVE WD-MGA TO WD-MG.
           DISPLAY WD-MGA with foreground-color 14 AT VDU-LP.
           GO Z.
       X.  DISPLAY "Menge zu gross" AT 2401
               PERFORM WEITER.
       Z.  EXIT.
