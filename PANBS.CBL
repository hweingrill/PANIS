      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER SUPFF
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PANBS.
      ******************************************************************
      *  PANBS = 9: es werden alle Artikel angezeigt!                  *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                         ENVIRONMENT-NAME IS ENV-SET
                         ENVIRONMENT-VALUE IS ENV-WERT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSELFS.CPY.
           SELECT KONSTANT   ASSIGN TO DISK "PANEKONS.DAT"
                             ORGANIZATION RELATIVE ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT HILFPOS    ASSIGN TO "ENDE"
                             FILE STATUS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANLIEF.CPY.
       COPY PANFAKT.CPY.
      ******************************************************************
       FD  HILFPOS                     LABEL RECORD STANDARD.
       01  HI-SATZ                     PIC X(30).
      ******************************************************************
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
           03  WE-EXTN                 PIC X(7).
           03  WE-EXTV.
               05 WR-VAL               PIC 9.
               05 WR-ZUS               PIC X.
           03  WM-PANBS                PIC 99     COMP.
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
           03  WT-ERFTAB.
               05 WT-KEY               PIC X(15)    OCCURS 10.
           03  WH-ANZ                  PIC S9(8)V99  COMP-3.
           03  WA-ANZ                  PIC 9(5)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(5).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9999V9.
           03  WH-ANZ2                 PIC S999V99.
           03  IX                      PIC 99        COMP.
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
           03  WF-END                  PIC X(5)     VALUE "ENDE".
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WK-ART                  PIC 9.
           03  WH-LFAKEY               PIC X(16).
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
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           DISPLAY "PANBS" UPON ENV-SET.
           ACCEPT WE-EXTV FROM ENV-WERT.
           IF WE-EXTV(1:1) = SPACE MOVE 0 TO WR-VAL.
           IF WE-EXTV(2:1) = SPACE MOVE 0 TO WR-ZUS.
           MOVE WR-VAL TO WM-PANBS.
           IF WM-PANBS not = 3 CALL "CBL_DELETE_FILE" USING WF-END.
           CALL "CADECL" USING "99AUFRUF" WH-CREG.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
      *    MOVE WH-DATUM TO WM-DATUM.
           OPEN INPUT DEBITOR KONSTANT FAKTDAT.
           OPEN I-O LFSCHEIN.
       B.  MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           MOVE KO-MERK TO WK-M.
           MOVE 0 TO WE-LFS.
           IF WM-VER > 10 ADD -10 TO WM-VER
                          MOVE 1 TO WE-LFS.              *> Lfsch. Kern
           MOVE KO-EANNR TO WK-EAN.
           MOVE 1 TO WK-BS.
           MOVE KO-ORT TO WT-TX.
           PERFORM VARYING WX FROM 15 BY -1 UNTIL WX < 1
               or WR-TX(WX) NOT = SPACE CONTINUE.
           MOVE 15 TO WY.
           PERFORM VARYING WX FROM WX BY -1 UNTIL WX = 0
                MOVE WR-TX(WX) TO VDU-ORT(WY:1)
                ADD -1 TO WY.
      *    MOVE WH-DATUM TO WM-DATUM WZ-DATUM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE KO-UST(WX) TO WT-UST(WX).
       C.  CALL "CAUP" USING "1301012540" WH-CREG.
           DISPLAY WK-FIRMA with highlight AT 0101.
       E.  DISPLAY VDU-DATUM with highlight AT 0132.
           DISPLAY "Tourfolge: " AT 0503.
           DISPLAY "<esc>= Ende, <#>= Folge, < >= Datum" AT 2301.
           CALL "CAUP" USING "0005151001" WH-CREG.
           IF ESC GO X.
           IF AB PERFORM NEUDAT GO E.
           IF NOT KIST GO E.
           CALL "CAUP" USING "1301012540" WH-CREG.
           MOVE WH-NUM TO WK-ART.
           MOVE 2 TO WV-KTONR.
           MOVE 0 TO DE-KTONR.
           PERFORM BSKOM.
           GO C.
       X.  CLOSE DEBITOR LFSCHEIN KONSTANT FAKTDAT.
       Z.  STOP RUN.
      ******************************************************************
       NEUDAT SECTION.
       A.  MOVE WM-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0101326006" WH-CREG
           IF NOT RET GO A.
           IF WZ-DATUM = 0 GO A.
           MOVE WX-DATUM TO WM-DATUM.
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
           MOVE 0 TO WM-KTONR WK WH-KB.
           MOVE WM-DATUM TO WV-DATUM.
           DIVIDE 100 INTO WV-DATUM GIVING WV-LFNUM REMAINDER WV-LFNUM.
           COMPUTE WV-LFNUM = WV-LFNUM * 100.
           IF WK-ART < 3 ADD WK-ART TO WV-LFNUM.
           IF WM-PANBS = 0 MOVE 1 TO WI;
               IF WM-KO = 3 MOVE 2 TO WI.
           IF WM-PANBS not = 0 MOVE WM-PANBS TO WI.
           MOVE 0 TO VDU-L.
           MOVE 0 TO LF-TOUR LF-SUB LF-KTONR LF-NUM LF-ARNUM WLN WL.
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
      *--------------------------> B„ck.Art. in Konditorei eingez„hlt <-
           IF WF-TB = 4 MOVE 3 TO WF-TB.
           IF WK = 0 PERFORM LFKOPF.
       E.  IF WV-KTONR > 2; IF WV-KTONR NOT = LF-KTONR
               OR LF-NUM NOT = WV-LFNUM
               OR LF-DATUM NOT = WV-DATUM MOVE 66 TO WL GO L.
           IF LF-DATUM > WV-DATUM MOVE 66 TO WL GO L.
           IF WM-KTONR = 0 MOVE LF-KTONR TO WM-KTONR.
           IF WV-LFNUM = 0 MOVE LF-NUM TO WV-LFNUM.
           IF LF-KTONR NOT = WM-KTONR MOVE 3 TO WLN GO L.
           IF WM-PANBS = 9 MOVE WF-TB TO WI.
           IF WI = WF-TB AND LF-NUM = WV-LFNUM GO H.
           IF WK-ART < 3 MOVE 1 TO WL GO D.
           IF LF-NUM NOT = WV-LFNUM MOVE 3 TO WLN GO L.
           IF WI NOT = WF-TB MOVE 1 TO WL.
           GO D.
      *----------------------------------> Ende eines Lieferscheines <-
       G.  MOVE 0 TO WK.
           MOVE LF-SATZ(1:16) TO WH-LFAKEY.
           MOVE WV-TOUR TO LF-TOUR.
           MOVE WV-LFNUM TO LF-NUM.
           MOVE WV-SUB TO LF-SUB.
           MOVE 0 TO VDU-LP.
           IF WH-KB = 0;
               IF RET CALL "PANLFDRU" USING "10SOFDRU" WH-CREG.
           MOVE WH-LFAKEY TO LF-SATZ.
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
       H.  IF VDU-L > 20 DISPLAY  "***" with highlight BLINK AT 0237
               MOVE 1 TO WH-KB
               GO L.
           IF WK = 0 PERFORM LFKOPF
               MOVE LF-NUM TO WV-LFNUM.
           ADD 2 TO VDU-L.
           MOVE LF-ARNUM TO WV-ARNUM.
           MOVE LF-ARNUM TO WD-POS.
           MOVE 1 TO VDU-P.
           DISPLAY WD-POS with foreground-color 15 AT VDU-LP.
           ADD 4 TO VDU-LP.
           DISPLAY LF-BEZ with foreground-color 14 AT VDU-LP.
           ADD 26 TO VDU-LP.
           MOVE LF-GRM TO WF-GRM.
      *--------------------------> B„ck.Art. in Konditorei eingez„hlt <-
           IF WF-TB = 4 MOVE 3 TO WF-TB.
       K.  MOVE LF-MENGE(1) TO WH-ANZ.
           MOVE 30 TO VDU-P.
           PERFORM MG.
           PERFORM DIS-MEH.
           ADD 1 TO IX.
           MOVE LF-SATZ(1:15) TO WT-KEY(IX).
           GO D.
       L.  UNLOCK LFSCHEIN.
           IF VDU-L = 00 AND WL = 66 GO Y.
      *    IF VDU-L = 2 ADD 1 TO VDU-L
           IF VDU-L = 3 MOVE 0 TO WH-NUM GO M.
           IF VDU-L = 3 ADD 1 TO VDU-L
               MOVE 10 TO VDU-P
               DISPLAY "kein Geb„ck" with foreground-color 14 AT VDU-LP
               MOVE 2 TO WK.
           MOVE 30 TO VDU-P.
           DISPLAY "<ret>= Druck, <esc>= Abbruch" AT 2301.
           DISPLAY "< >= Kunde, < >= Tour, <Ende>= Ende" AT 2401.
           IF WF-NK = 0 CALL "CAUP" USING "0000006007" WH-CREG.
           IF WF-NK = 1 CALL "CAUP" USING "0000005107" WH-CREG.
           IF WF-NK = 2 CALL "CAUP" USING "0000004207" WH-CREG.
           IF ENDE PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
                       IF WT-KEY(IX) not = LOW-VALUE
                           MOVE WT-KEY(IX) TO LF-SATZ(1:15)
                   end-perform
                   ADD 1 TO LF-ARNUM
                   MOVE 0 TO WH-NUM GO M.
           IF ESC GO Y.
           IF ENTF or EINF PERFORM AENDERUNG GO L.
           IF AUF AND WH-NUM NOT = 0;
               COMPUTE WM-KTONR = WH-NUM * 10 + 2
               MOVE LF-TOUR TO WV-TOUR
               MOVE 0 TO LF-NUM LF-SUB LF-KTONR LF-ARNUM  WV-LFNUM
               MOVE 9 TO WK GO B.
           IF AUF and WH-NUM = 0;
               IF VDU-L not = 5 ADD -2 TO VDU-L
                                ADD -1 TO IX GO L.
           IF AB AND WH-NUM NOT = 0;
               MOVE WH-NUM TO LF-TOUR
               MOVE 0 TO LF-SUB LF-KTONR LF-NUM LF-ARNUM
                         WM-KTONR WK WV-LFNUM VDU-L GO B.
           IF AB and WH-NUM = 0; IF WT-KEY(IX + 1) not = LOW-VALUE
                                     ADD 2 TO VDU-L
                                     ADD 1 TO IX GO L.
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
       Y.  CANCEL "PANLFDRU".
       Z.  EXIT.
      ******************************************************* Žnderung *
       AENDERUNG SECTION.
       A.  MOVE WT-KEY(IX) TO LF-SATZ(1:15).
           START LFSCHEIN KEY = LF-AKEY INVALID GO Z.
       C.  READ LFSCHEIN NEXT AT END STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Z.
           EVALUATE LF-STAT also TRUE
               WHEN 00 also ENTF MOVE 97 TO LF-STAT
               WHEN 00 also EINF MOVE 95 TO LF-STAT
               WHEN 99 also ENTF ADD -2 TO LF-STAT
               WHEN 99 also EINF ADD -4 TO LF-STAT
               WHEN 98 also ENTF ADD -2 TO LF-STAT
               WHEN 98 also EINF ADD -4 TO LF-STAT
               WHEN 97 also ENTF ADD +2 TO LF-STAT
               WHEN 96 also ENTF ADD +2 TO LF-STAT
               WHEN 95 also EINF ADD +4 TO LF-STAT
               WHEN 94 also EINF ADD +4 TO LF-STAT
               WHEN OTHER GO Z.
           REWRITE LF-SATZ.
           PERFORM DIS-MEH.
       Z.  EXIT.
      ******************************************************************
       DIS-MEH SECTION.
           ADD 1 WF-MEH GIVING WH-MEH.
           ADD 7 TO VDU-LP.
           EVALUATE LF-STAT
               WHEN 98 DISPLAY WT-MEH(WH-MEH) with highlight BLINK
                           AT VDU-LP
               WHEN 97
               WHEN 96 DISPLAY "nlf" with highlight BLINK AT VDU-LP
               WHEN 95
               WHEN 94 DISPLAY "STO" with highlight BLINK AT VDU-LP
               WHEN OTHER DISPLAY WT-MEH(WH-MEH) with highlight
                              AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       LFKOPF SECTION.
       A.  MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO IX.
           MOVE 1 TO WK.
           IF ESC GO Z.
           IF DE-KTONR = LF-KTONR GO C.
           MOVE 1 TO DE-FNR.
           MOVE LF-KTONR TO DE-KTONR.
       B.  READ DEBITOR IGNORE LOCK
               INVALID MOVE "Kunde fehlt" TO DE-BEZ.
       C.  MOVE DE-BEZ TO WT-BEZ.
930104*    IF WM-KO = 3 AND DE-RAST = 3 MOVE 0 TO WK VDU-L
930104*         GO Z.
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
           READ KONSTANT IGNORE LOCK
               INVALID MOVE "Tour nicht angelgt" TO KO-TOUR.
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
