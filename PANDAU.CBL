      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PANDAU.
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
           COPY PANSEDAU.CPY.
           SELECT EDIFACT   ASSIGN TO WN-EDIFACT         *> DatentrÑger
                            ORGANIZATION LINE SEQUENTIAL
                            ACCESS SEQUENTIAL
                            FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
      ******************************************************************
       FD  EDIFACT                     LABEL RECORD STANDARD.
       01  EDF-SATZ                    PIC X(120).
      *****************************************************************
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
       COPY WHCREG.CPY.
       01  WF-REG.
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
           03  WN-EDIFACT              PIC X(20).
           03  WE-UST                  PIC Z9,99.
           03  WE-PREIS                PIC ZZZ9,999.
           03  WE-DATUM                PIC 9(8).
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 15.
           03  VDU-AB                  PIC 9999          COMP.
           03  VDU-HL                  PIC 999           COMP.
           03  WH-XTAB                 PIC X(80).
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WM-KTONR                PIC 9(6)      COMP.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  WD-MENGE                PIC X(11).
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WD-PZ                   PIC ZZ9-.
           03  WD-POS                  PIC Z9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-PROZ                 PIC Z9,9.
           03  WD-ARNUM                PIC ZZ9.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-ART                  PIC ZZZ9.
           03  WD-APREIS               PIC ZZZ.ZZ9,999-.
           03  WH-PROZ                 PIC S99V99     COMP VALUE ZERO.
           03  WD-PRZ                  PIC Z9,99-.
           03  WH-PREISDAT             PIC 9(8)       COMP VALUE ZERO.
           03  WV-AB                   PIC 9(8)       COMP.
           03  WH-PREIS                PIC S9(5)V999  COMP-3.
           03  WR-PR REDEFINES WH-PREIS.
               05 FILLER               PIC X(4).
               05 WR-GR                PIC S9         COMP-3.
           03  WI                      PIC 99         COMP.
           03  WD-X                    PIC X.
           03  WD-EAN                  PIC Z(8).
           03  WV-ARNUM                PIC 9(6)       COMP.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING "PANEFAKT.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "PANDEBIT.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON DAUER.
       A.  CALL "CADECL" USING "PANEBEST.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE 0 TO WF.
           PERFORM DAUWAHL.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  CALL "CAUP" USING "04DATPRF" WH-CREG.
      *    MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
      *    MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
      *    MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           SET ESC TO TRUE.
       Z.  EXIT.
      *****************************************************************
       WASNUN SECTION.
       A.  IF WX = 0 PERFORM NO-REC GO Z.
           DISPLAY "<ret>= Auswahl, <ret-leer>= weiterlisten" AT 2301.
           IF WF-STATUS = 10 DISPLAY "*** keine Daten mehr ***"
               with highlight AT 2455.
           DISPLAY "<esc> oder <#>= Ende" AT 2401.
           CALL "CAUP" USING "0024223003" WH-CREG.
           IF ESC OR KIST MOVE 0 TO WH-NUM.
           IF RET AND WV-MCODE = SPACE; IF WF-STATUS NOT = 10 GO Z
               ELSE GO A.
           IF KIST GO Z.
           IF RET AND WH-NUM = 0 SET ESC TO TRUE.
           IF NOT ESC AND NOT RET GO A.
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
       DAUWAHL SECTION.
       A.  CALL "CAUP" USING "0707150735000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Auswahl " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Preise" AT 0920
           DISPLAY "2 - Dauerbestellung < >" AT 1020.
           DISPLAY "3 - Aktionspreise" AT 1120.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Wahl" AT 2301.
           CALL "CAUP" USING "0110411001" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM
                  SET RET TO TRUE.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 GO X
               WHEN 1 MOVE 3 TO WH-PG
               WHEN 2 MOVE 4 TO WH-PG
               WHEN 3 PERFORM AKTIONEN GO X
               WHEN OTHER GO C.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           PERFORM DAUPREI.
           GO A.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************* Aktionen *
       AKTIONEN SECTION.
       A.  CALL "CAUP" USING "0707100845000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Aktionen " with highlight foreground-color 3
               AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Artikel: " AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "von:            bis:" AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Preis: " AT VDU-LP.
       E.  DISPLAY "<ret>= Artikel-Nr., <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "1003126006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           IF WH-NUM = 0 AND WV-MCODE NOT = SPACE
               MOVE SPACE TO AR-SA
               MOVE WV-MCODE TO AR-ABEZ
               CALL "PANANZ" USING "20ART-ANZ" WH-CREG
               CANCEL "PANANZ"
               IF AR-NUM = 0 GO E.
           MOVE WH-NUM TO AR-NUM WD-ART.
           DISPLAY WD-ART AT VDU-LP " ".
       G.  READ ARTIKEL IGNORE LOCK INVALID PERFORM NO-REC GO E.
           DISPLAY AR-BEZ with highlight AT 0000.
       I.  DISPLAY "Aktionszeitraum von: <ret>= Datum, <esc>= Abbruch, <
      -        " />= zurÅck" AT 2301.
           ADD 619 VDU-ECK GIVING VDU-LP.
           DISPLAY " " with SIZE 25 AT VDU-LP.
           CALL "CAUP" USING "1104086006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO C.
           IF NOT RET or WX-DATUM = 0 GO I.
           MOVE WX-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY "aktueller Preis:" AT VDU-LP.
           ADD 619 VDU-ECK GIVING VDU-LP.
           MOVE AR-APREIS TO WH-PREIS.
           IF AR-ADAT not = 0;
               IF WV-DATUM < AR-ADAT MOVE AR-PREIS TO WH-PREIS.
           IF WH-PREIS = 0 MOVE AR-PREIS TO WH-PREIS.
           MOVE WH-PREIS TO WD-PREIS.
           DISPLAY WD-PREIS with highlight foreground-color 2 AT VDU-LP.
           IF AR-ADAT not = 0 MOVE AR-ADAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY " / " AT 0000 VDU-DATUM with highlight.
       K.  DISPLAY "Aktionszeitraum bis: <ret>= Datum, <esc>= Abbruch, <
      -        " />= zurÅck" AT 2301.
           CALL "CAUP" USING "1104246006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO I.
           IF NOT RET or WX-DATUM = 0 GO I.
           MOVE WX-DATUM TO WS-DATUM.
           IF WS-DATUM < WV-DATUM GO I.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       M.  DISPLAY "<ret>= Preis, <esc>= Abbruch, < />= zurÅck, 0+<Entf
      -        ">= lîschen" AT 2301.
           CALL "CAUP" USING "1005103206" WH-CREG.
           IF ESC GO X.
           IF WOLI OR AUF GO K.
           IF RET and WH-WERT = 0 GO M.
           IF not RET and not ENTF GO M.
           MOVE WH-WERT TO WD-PREIS WH-PREIS.
           IF WH-WERT not = 0 DISPLAY WD-PREIS with highlight
                                  foreground-color 6 AT VDU-LP
                         else DISPLAY "Preise lîschen" with highlight
                                  foreground-color 6 AT VDU-LP.
        N. DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Start < >"
               AT 2301.
           CALL "CAUP" USING "0023460000" WH-CREG.
           IF ESC GO X.
           IF WOLI OR AUF GO M.
           IF NOT RET GO N.
           MOVE LOW-VALUE TO FA-SATZ.
      *-----------------------------------------------> PreisÑnderung <-
           START FAKTDAT KEY NOT < FA-KEY INVALID GO X.
       P.  READ FAKTDAT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO P.
           IF FA-ARNUM not = AR-NUM GO P.
           DIVIDE 10 INTO FA-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 2020.
      *    IF FA-KTONR < 212 GO P.
           PERFORM VARYING WX FROM 4 BY -1 UNTIL WX = 0
               or FA-PREIS(WX) not = 0 or FA-RAB(WX) not = 0
               or FA-RET(WX) not = 0 CONTINUE.
           IF WH-PREIS not = 0 GO Q.
      *----------------------------------------------------> Lîschung <-
           IF WX = 0 GO P.
           IF FA-AB(WX) = WV-DATUM and FA-BIS(WX) = WS-DATUM
                MOVE 0 TO FA-PREIS(WX) FA-AB(WX) FA-BIS(WX)
                          FA-RET(WX) FA-RAB(WX).
           GO W.
       Q.  ADD 1 TO WX.
           IF WX > 4 PERFORM VARYING WX FROM 2 BY 1 UNTIL WX > 4
               MOVE FA-TAB(WX) TO FA-TAB(WX - 1)
               end-perform
               MOVE 4 TO WX.
           MOVE WH-PREIS TO FA-PREIS(WX) FA-RET(WX).
           MOVE WV-DATUM TO FA-AB(WX).
           MOVE WS-DATUM TO FA-BIS(WX).
           MOVE 0 TO FA-RAB(WX).
       W.  REWRITE FA-SATZ.
           GO P.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *********************************** lîschen einer Lieferposition *
       LOESCH SECTION.
       A.  MOVE WH-NUM TO FA-ARNUM.
           MOVE WM-KTONR TO FA-KTONR.
           START FAKTDAT KEY NOT < FA-KEY INVALID GO Z.
       C.  READ FAKTDAT NEXT AT END GO Z.
           DIVIDE 10 INTO FA-KTONR GIVING WD-KTO.
           MOVE FA-ARNUM TO WD-ARNUM.
           IF WM-KTONR not = FA-KTONR GO Z.
           DISPLAY WD-KTO AT 2401 " / " WD-ARNUM.
           DISPLAY "Codewort:" AT 2301.
           CALL "CAUP" USING "0023114004" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           IF WH-NUM NOT = 4711 GO A.
       F.  DELETE FAKTDAT NOT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO F.
           DISPLAY "nicht gelîscht" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ************************** loeschen Dauerbestellungen u. Preise *
       DAULOE SECTION.
       A.  DISPLAY "ACHTUNG!" with highlight AT 2010 " Lîschung aller "
           IF WH-PG = 3 DISPLAY "Sonderpreise" AT 0000
              ELSE DISPLAY "Dauerbestellungen" AT 0000.
           DISPLAY "<ret>= Abbruch, <tab>= ausfÅhren " AT 2301.
           MOVE WM-KTONR TO DA-KTONR FA-KTONR WV-KTONR.
           MOVE 0 TO DA-ARNUM FA-ARNUM.
           CALL "CAUP" USING "0023340000" WH-CREG.
           IF RET GO X.
           IF NOT TABL GO A.
           IF WH-PG = 3 GO E.
      *---------------------------------> loeschen Dauerbestellungen <-
           START DAUER KEY NOT < DA-KEY INVALID PERFORM NO-REC GO X.
       C.  READ DAUER NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF DA-KTONR NOT = WV-KTONR GO W.
           DELETE DAUER.
           GO C.
      *--------------------------------------> loeschen Sonderpreise <-
       E.  START FAKTDAT KEY NOT < FA-KEY INVALID PERFORM NO-REC GO X.
       F.  READ FAKTDAT NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO F.
           IF FA-KTONR NOT = WV-KTONR GO W.
           SET PX TO 1.
       G.  MOVE 0 TO FA-BIS(PX) FA-AB(PX) FA-PREIS(PX) FA-RET(PX)
               FA-RAB(PX).
           IF PX < 4 SET PX UP BY 1 GO G.
           REWRITE FA-SATZ.
           GO F.
       W.  DISPLAY "gelîscht" AT 2401.
           PERFORM WEITER.
       X.  CALL "CAUP" USING "132001248000" WH-CREG.
       Z.  EXIT.
      ********************************* duplizieren von Kundenpreisen *
       DUPLO SECTION.
       A.  CALL "CAUP" USING "1500006006" WH-CREG.
           DISPLAY "Nr.:" AT 0401.
           MOVE 0 TO WX-TASTE.
           IF WH-NUM = 0 GO Z.
           ADD 0,2 WH-NUM GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0406.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           IF WM-KTONR = DE-KTONR GO Z.
       B.  READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO Z.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ with SIZE 64 AT 0415.
           MOVE DE-KTONR TO FA-KTONR.
           MOVE 0 TO FA-ARNUM.
           DISPLAY "-- bitte warten --" AT 2320.
       C.  START FAKTDAT KEY > FA-KEY INVALID GO Z.
       D.  READ FAKTDAT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF FA-KTONR NOT = DE-KTONR GO Z.
           MOVE FA-XTAB TO WH-XTAB.
           MOVE FA-KUARNUM TO WH-EAN.
           PERFORM LOEFAK.
           MOVE WM-KTONR TO FA-KTONR.
           IF DE-EAN > 1 MOVE WH-EAN TO FA-KUARNUM.
           MOVE WH-XTAB TO FA-XTAB.
           WRITE FA-SATZ INVALID KEY GO G.
           GO H.
       G.  READ FAKTDAT.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE WH-XTAB TO FA-XTAB.
           MOVE DE-FAKART TO FA-ART.
           IF DE-EAN > 1 MOVE WH-EAN TO FA-KUARNUM.
           REWRITE FA-SATZ.
       H.  MOVE DE-KTONR TO FA-KTONR.
           GO C.
       Z.  EXIT.
      **************************************** prozent. Preiserhîhung *
       NEUPREIS SECTION.
       A.  CALL "CAUP" USING "0708120540" WH-CREG.
           ADD 103  VDU-ECK GIVING VDU-LP.
           DISPLAY " %-Preiserhîhung " AT VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "ab:          mit       %" AT VDU-LP.
           ADD 4 TO VDU-LP.
       C.  MOVE WM-DATUM TO WC-DATUM WX-DATUM WH-WERT WV-DATUM.
           IF WH-PREISDAT NOT = 0 MOVE WH-PREISDAT TO WC-DATUM
      *        MOVE WV-DATUM TO WZ-DATUM WH-WERT WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP GO G.
       E.  CALL "CAUP" USING "1103076006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           MOVE WX-DATUM TO WH-PREISDAT WV-AB.
           IF WX-DATUM = 0 DISPLAY "        " AT VDU-LP
           else DISPLAY VDU-DATUM with highlight AT VDU-LP.
       G.  MOVE WH-PROZ TO WH-WERT WD-PRZ.
           ADD 0320 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PRZ with highlight AT VDU-LP.
           CALL "CAUP" USING "1003204205" WH-CREG.
           IF ESC GO X.
           IF WOLI MOVE WH-PREISDAT TO WZ-DATUM WH-WERT GO E.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-PROZ WD-PRZ.
           DISPLAY WD-PRZ with highlight AT VDU-LP.
      *-----------------------------------------------------------------
           MOVE DE-KTONR TO FA-KTONR.
           MOVE 0 TO FA-ARNUM.
           START FAKTDAT KEY > FA-KEY INVALID PERFORM NO-REC GO X.
       H.  READ FAKTDAT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF DE-KTONR NOT = FA-KTONR GO X.
       I.  PERFORM VARYING PX FROM 4 BY -1 UNTIL PX = 0
               OR FA-PREIS(PX) NOT = 0 OR FA-RET(PX) NOT = 0 CONTINUE.
           IF PX = 0 GO H.
           SET WI TO PX.
           IF FA-AB(WI) NOT < WV-AB
               PERFORM VARYING WI FROM WI BY 1 UNTIL WI > 4
                   MOVE 0 TO FA-AB(WI) FA-BIS(WI) FA-PREIS(WI)
                             FA-RET(WI) FA-RAB(WI)
               END-PERFORM GO I.
           IF PX = 4 MOVE FA-XTAB(19:) TO FA-XTAB(1:)
               MOVE 0 TO FA-AB(4) FA-BIS(4) FA-RAB(4)
                         FA-PREIS(4) FA-RET(4)
                         SET WI DOWN BY 1
           ELSE SET PX UP BY 1.
           MULTIPLY WH-PROZ BY 0,01 GIVING WH-WERT.
           ADD 1 TO WH-WERT.
       K.  IF FA-BIS(WI) NOT = 0 ADD -1 TO WI;
               IF WI < 1 GO H ELSE GO K.
           IF WH-PROZ = 0 GO L.
           MOVE WH-PREISDAT TO FA-AB(PX).
           COMPUTE WH-ANZ ROUNDED = FA-PREIS(WI) * WH-WERT.
           MOVE WH-ANZ TO FA-PREIS(PX).
           COMPUTE WH-ANZ ROUNDED = FA-RET(WI) * WH-WERT.
           MOVE WH-ANZ TO FA-RET(PX).
           MOVE FA-RAB(PX - 1) TO FA-RAB(PX).
           MOVE FA-PREIS(PX) TO WD-PREIS.
       L.  MOVE FA-ARNUM TO WD-ARNUM.
           DISPLAY WD-ARNUM AT 1515 " " WD-PREIS.
           REWRITE FA-SATZ.
           GO H.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************* Verwaltung Kundensonderpreise *
       DAUPREI SECTION.
       A.  MOVE "  Sonderkonditionen" TO WK-GEB.
           IF WH-PG = 4 MOVE "  Dauerbestellungen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
       B.  DISPLAY "Kunden-Nr.:" AT 0301.
           UNLOCK DEBITOR.
           UNLOCK ARTIKEL.
           UNLOCK DAUER.
           UNLOCK FAKTDAT.
           DISPLAY "alpha+<ret>= suchen, Nr.+<ret>= Kundennummer, <esc>=
      -        " Ende" AT 2301.
           CALL "CAUP" USING "0003135008" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO B.
           IF WH-NUM = 0 CALL "PANANZ" USING "10DEBSUCH" WH-CREG
                         CANCEL "PANANZ".
           IF WH-NUM = 0 GO A.
           ADD 0,2 WH-NUM GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0313.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
       C.  READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO A.
           MOVE DE-KTONR TO WM-KTONR.
           CALL "CAUP" USING "1303012480" WH-CREG.
           DISPLAY "Nr.:" AT 0301.
           DISPLAY WD-KTO with highlight AT 0306.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ with SIZE 64 AT 0315.
           MOVE DE-SPERR TO WC-DATUM.
           PERFORM DATDREH.
           IF WZ-DATUM NOT = 0 DISPLAY ALL" " with SIZE 10 AT 0370
               DISPLAY VDU-DATUM with highlight BLINK AT 0372.
       D.  CALL "CAUP" USING "1304010480000" WH-CREG.
           CALL "CAUP" USING "1323012480000" WH-CREG.
           DISPLAY "Art-Nr.:" AT 0401.
           DISPLAY "<ret>= Art.Nr., <ret-leer>= anzeigen, <Entf>= lîsche
      -        "n, <esc>= Abbruch" AT 2301.
           IF WH-PG = 4 DISPLAY "<>= Sperrdatum" AT 2401.
           IF WH-PG = 3 DISPLAY "<>= von Kdn-Nr. duplizieren, < >= nÑch
      -        "ster -, < >= vorh. Kunde," AT 2401
               " < >= %-Erhg." with highlight.
           IF WH-PG = 3 DISPLAY "0/1+<Einfg>= ohne/mit S-Preise an Sonn-
      -        "onn- /Feiertag, " AT 2501 "<Ende>= PRICAT erstellen"
                    with highlight foreground-color 6.
           CALL "CAUP" USING "0004106006" WH-CREG.
           CALL "CAUP" USING "1304100480" WH-CREG.
           IF ENDE PERFORM PRICAT GO D.
           IF HELP; IF WH-PG = 4
              CALL "CAUP" USING "190003000401" WH-CREG GO D
               ELSE IF WH-PG = 3
              CALL "CAUP" USING "190005000601" WH-CREG GO D.
           IF WH-PG = 3 AND AB AND DE-KTONR NOT = 0 AND NOT FINE
               READ DEBITOR NEXT AT END GO C
               END-READ DIVIDE 10 INTO DE-KTONR GIVING WD-KTO GO C.
           IF WH-PG = 3 AND AUF AND DE-KTONR NOT = 0 AND NOT FINE
               READ DEBITOR PREVIOUS IGNORE LOCK AT END GO C
               END-READ DIVIDE 10 INTO DE-KTONR GIVING WD-KTO GO C.
           IF ESC GO A.
           IF WH-PG = 3 AND ENTF PERFORM LOESCH GO D.
           IF ENTF PERFORM DAULOE GO A.
           IF WH-PG = 3; IF WOLI PERFORM DUPLO
               MOVE WM-KTONR TO DE-KTONR
               DIVIDE 10 INTO DE-KTONR GIVING WD-KTO GO C
               else IF WORE PERFORM NEUPREIS GO C.
           IF WH-NUM = 0 AND WV-MCODE NOT = SPACE
               MOVE SPACE TO AR-SA
               MOVE WV-MCODE TO AR-ABEZ
               CALL "PANANZ" USING "20ART-ANZ" WH-CREG
               CANCEL "PANANZ"
               IF AR-NUM = 0 GO D.
           IF WH-PG = 3 OR NOT WOLI GO E.
      *-----------------------------------------> Eingabe Sperrdatum <-
           MOVE 0 TO WX-TASTE.
           MOVE DE-SPERR TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0103726006" WH-CREG.
           MOVE WX-DATUM TO DE-SPERR.
           DISPLAY " " with SIZE 9 AT 0370.
           IF WX-DATUM NOT = 0 DISPLAY VDU-DATUM with highlight AT 0372.
           REWRITE DE-SATZ.
           MOVE 0 TO DA-ARNUM.
           MOVE DE-KTONR TO DA-KTONR WV-KTONR. 
           START DAUER KEY NOT < DA-KEY INVALID PERFORM NO-REC GO D.
           DISPLAY "bitte warten" AT 2460.
       DA. READ DAUER NEXT AT END GO D.
           IF ZUGRIF PERFORM BESETZT GO DA.
           IF DA-KTONR NOT = WV-KTONR GO D.
           MOVE DE-SPERR TO DA-DAT(1).
           MOVE DE-FAKART TO DA-FART.
           MOVE DE-TOUR TO DA-TOUR.
           MOVE DE-SUB TO DA-SUB.
           MOVE DE-EAN TO DA-EAN.
           REWRITE DA-SATZ.
           GO DA.
       E.  IF FEHLER GO D.
           MOVE DE-KTONR TO DA-KTONR FA-KTONR WV-KTONR.
           CALL "CAUP" USING "1307012480" WH-CREG.
           IF WH-PG = 4 DISPLAY ALL "ƒ" with SIZE 80 AT 1101
               DISPLAY "Art-Nr  1-Mo   2-Di   3-Mi   4-Do   5-Fr   6-Sa
      -           "  7-So   8-/+   9-/- Bezeichnung" AT 1001
               "nicht ab:           bis inkl.:" AT 0939
                GO P.
           IF WH-PG = 3 DISPLAY ALL "ƒ" with SIZE 55 AT 1101
               DISPLAY "Art-Nr     ab Datum bis        Preis  Ret.Preis 
      -            " Rabatt" AT 1001.
           IF DE-EAN > 1 DISPLAY "Kundenart.Nr." AT 1058
               DISPLAY ALL "ƒ" with SIZE 15 AT 1156.
      *------------------------------------> Verwaltung Sonderpreise <-
           MOVE WH-NUM TO FA-ARNUM.
           IF FA-ARNUM > 0 GO I.
      *---------------------------------------------> anzeigen aller <-
           MOVE 0 TO FA-ARNUM WX.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 11 TO VDU-AB VDU-L.
           START FAKTDAT KEY NOT < FA-KEY INVALID PERFORM NO-REC GO D.
       F.  READ FAKTDAT NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO F.
           IF FA-KTONR NOT = WV-KTONR MOVE 10 TO WF-STATUS GO G.
           SET PX TO 4.
       FA. IF FA-AB(PX) NOT = 0 OR FA-BIS(PX) NOT = 0 
               OR FA-PREIS(PX) NOT = 0 OR FA-RET(PX) NOT = 0
               OR (PX = 1 and FA-KUARNUM NOT = 0) GO FB.
           IF PX > 1 SET PX DOWN BY 1 GO FA.
           GO F.    
       FB. ADD 1 TO VDU-L.
           IF VDU-L < 21 GO H.
       G.  PERFORM WASNUN.
           CALL "CAUP" USING "1312012480" WH-CREG.
           IF ESC OR KIST GO D.
           IF WV-MCODE NOT = SPACE GO E.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE VDU-AB TO VDU-L.
           MOVE 0 TO WX.
           ADD 100 TO VDU-LP.
       H.  MOVE FA-ARNUM TO WD-ART AR-NUM.
           IF VDU-L = 12 OR VDU-L > 12 AND WX NOT = 99
               MOVE 2 TO VDU-P
               DISPLAY WD-ART with highlight AT VDU-LP.
           IF FA-AB(PX) NOT = 0 MOVE FA-AB(PX) TO WC-DATUM
               PERFORM DATDREH
               MOVE 9 TO VDU-P
               IF WF = 1 AND PX = 1 DISPLAY VDU-DATUM AT VDU-LP
               ELSE DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF FA-BIS(PX) NOT = 0 MOVE FA-BIS(PX) TO WC-DATUM
               PERFORM DATDREH
               MOVE 18 TO VDU-P
               DISPLAY "/" AT 0000
               IF WF = 1 AND PX = 1 DISPLAY VDU-DATUM AT VDU-LP
               ELSE DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF FA-PREIS(PX) NOT = 0 
               MOVE FA-PREIS(PX) TO WH-PREIS WD-PREIS WD-APREIS
               MOVE 28 TO VDU-P
               IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                      ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
           IF FA-RET(PX) NOT = 0
               MOVE FA-RET(PX) TO WH-PREIS WD-PREIS WD-APREIS
               MOVE 39 TO VDU-P
               IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                      ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
           IF FA-RAB(PX) NOT = 0 MOVE FA-RAB(PX) TO WD-PROZ
               MOVE 52 TO VDU-P
               DISPLAY WD-PROZ with highlight AT VDU-LP.
           IF PX = 1; IF DE-EAN > 1 MOVE FA-KUARNUM TO WK-EAN WD-EAN
               MOVE 62 TO VDU-P
               DISPLAY WD-EAN with highlight AT VDU-LP.
           IF WX = 99; IF PX < 4 SET PX UP BY 1
               ADD 1 TO VDU-L GO H ELSE GO J.
           ADD 1 TO WX.
      *    MOVE FA-KTONR TO WK-KTONR.
      *    MOVE FA-ARNUM TO WK-ARNUM.
      *    MOVE WK-KEY TO WT-KEY(WX).
           GO F.
      *---------------------------------------> anlegen oder aendern <-
       I.  MOVE 99 TO WX.
           READ FAKTDAT INVALID PERFORM LOEFAK
                                MOVE 0 TO WX.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE 12 TO VDU-L.
           PERFORM VARYING PX FROM 4 BY -1 UNTIL
               PX = 1 OR FA-AB(PX) > 0 CONTINUE.
           IF PX = 4 MOVE 1 TO WF
                ELSE MOVE 0 TO WF.
           SET PX TO 1.
           IF WX = 99 GO H.
       J.  MOVE FA-ARNUM TO WD-ART AR-NUM.
           PERFORM VARYING PX FROM 4 BY -1 UNTIL
               PX = 0 OR FA-AB(PX) > 0 CONTINUE.
           IF PX = 4 MOVE FA-TAB(2) TO FA-TAB(1)
                     MOVE FA-TAB(3) TO FA-TAB(2)
                     MOVE FA-TAB(4) TO FA-TAB(3)
               MOVE 0 TO FA-AB(4) FA-BIS(4) FA-RAB(4)
                         FA-PREIS(4) FA-RET(4)
           ELSE SET PX UP BY 1.
           READ ARTIKEL INVALID KEY PERFORM NO-REC GO OD.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO D.
           DISPLAY WD-ART with highlight AT 1202.
           DISPLAY WD-ART with highlight AT 0410.
           DISPLAY AR-BEZ with highlight AT 0416.
       K.  SET VDU-L TO PX.
           ADD 11 WF TO VDU-L.
           DISPLAY "<ret-leer>= wiederholen, <esc/#>= Ende, < />= zurÅc
      -        "k, <Entf>= lîschen" AT 2301.
           MOVE FA-AB(PX) TO WZ-DATUM WH-WERT.
           IF WZ-DATUM = 0 MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0100096006" WH-CREG.
           IF WOLI or AUF; IF PX = 1 GO K
               ELSE SET PX DOWN BY 1 GO K.
           IF ESC OR KIST OR ENDE GO O.
           IF ENTF; IF WX > 0 PERFORM REORG
               MOVE 12 TO VDU-L
               SET PX TO 1
               MOVE 99 TO WX
               GO H else GO K.
           IF NOT RET GO K.
           MOVE WX-DATUM TO FA-AB(PX).
           IF WX-DATUM = 0 DISPLAY "        " AT VDU-LP
           else DISPLAY VDU-DATUM with highlight AT VDU-LP.
       L.  DISPLAY "kein Datum = ohne EinschrÑnkung" AT 2401.
           MOVE FA-BIS(PX) TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0100186006" WH-CREG.
           IF WOLI GO K.
           IF ESC GO O.
           MOVE WX-DATUM TO FA-BIS(PX).
           IF WZ-DATUM = 0 DISPLAY "        " AT VDU-LP
               else DISPLAY "/"
                    DISPLAY VDU-DATUM with highlight AT VDU-LP.
       M.  MOVE FA-PREIS(PX) TO WH-WERT.
           CALL "CAUP" USING "0000284308" WH-CREG.
           IF WOLI GO L.
           IF ESC GO O.
           IF FEHLER GO M.
           IF FA-PREIS(PX) NOT = WH-WERT MOVE WH-WERT TO FA-RET(PX).
           MOVE WH-WERT TO FA-PREIS(PX) WH-PREIS WD-PREIS WD-APREIS.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                  ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
       N.  MOVE FA-RET(PX) TO WH-WERT.
           CALL "CAUP" USING "0000394308" WH-CREG.
           IF WOLI GO M.
           IF ESC GO O.
           IF FEHLER GO N.
           MOVE WH-WERT TO FA-RET(PX) WH-PREIS WD-PREIS WD-APREIS.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                  ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
       NA. MOVE FA-RAB(PX) TO WH-WERT.
           CALL "CAUP" USING "0000522105" WH-CREG.
           IF WOLI GO N.
           IF ESC GO O.
           IF FEHLER GO NA.
           MOVE WH-WERT TO FA-RAB(PX) WD-PROZ.
           IF FA-RAB(PX) NOT = 0
               DISPLAY WD-PROZ with highlight AT VDU-LP
               ELSE DISPLAY "      " AT VDU-LP.
           IF FA-RAB(PX) = 0 AND FA-RET(PX) = 0 AND FA-PREIS(PX) = 0
               MOVE 0 TO FA-AB(PX) FA-BIS(PX) GO K.
           IF PX < 4 SET PX UP BY 1 GO K.
           GO O.
       NB. DISPLAY "Kundenartikelnummer" AT 2401.
           MOVE FA-KUARNUM TO WH-WERT.
           CALL "CAUP" USING "0012628008" WH-CREG.
           IF WOLI GO NA.
           IF ESC GO O.
           IF FEHLER GO NB.
           MOVE WH-WERT TO FA-KUARNUM WD-EAN.
           DISPLAY WD-EAN AT 1262.
       O.  DISPLAY "<ret>= speichern, <Entf>= lîschen, <esc>= Abbruch <
      -        ">" AT 2301.
           IF DE-EAN > 1 AND WH-PG = 3 DISPLAY "<>= Anl./éndg. Kundenar
      -        "rtikelnummer" AT 2401.
           CALL "CAUP" USING "0023520000" WH-CREG.
           IF WOLI AND WH-PG = 3 GO NB.
           CALL "CAUP" USING "1312012480" WH-CREG.
           IF ESC GO D.
           IF ENTF; IF WH-PG = 4 GO OA
               ELSE SET PX TO 1 GO OD.
           IF NOT RET GO O.
           IF WH-PG = 4; IF WX = 99 REWRITE DA-SATZ
               ELSE WRITE DA-SATZ.
           IF WH-PG = 3; IF WX = 99 REWRITE FA-SATZ
               ELSE WRITE FA-SATZ.
           GO D.
      *--------------------------------------------> Einzelloeschung <-
       OA. DELETE DAUER INVALID KEY GO D.
           GO D.
       OD. MOVE 0 TO FA-BIS(PX) FA-AB(PX) FA-PREIS(PX)
                     FA-RET(PX) FA-RAB(PX).
           IF PX < 4 SET PX UP BY 1 GO OD.
           REWRITE FA-SATZ INVALID KEY GO D.
           GO D.
      *------------------------------->  Verwaltung Dauerbestellungen *
       P.  MOVE WH-NUM TO DA-ARNUM.
           IF DA-ARNUM > 0 GO U.
      *---------------------------------------------> anzeigen aller <-
           MOVE 0 TO DA-ARNUM WX.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 11 TO VDU-AB VDU-L.
           START DAUER KEY NOT < DA-KEY INVALID PERFORM NO-REC GO D.
       Q.  READ DAUER NEXT IGNORE LOCK AT END GO R.
           IF ZUGRIF PERFORM BESETZT GO Q.
           MOVE DA-GRM TO WF-GRM.
           IF DA-KTONR NOT = WV-KTONR MOVE 10 TO WF-STATUS GO R.
           ADD 1 TO VDU-L.
           IF VDU-L < 21 GO S.
       R.  PERFORM WASNUN.
           CALL "CAUP" USING "1312012480000" WH-CREG.
           IF ESC OR KIST GO D.
           IF WV-MCODE NOT = SPACE GO E.
      *    IF RET MOVE WT-KEY(WH-HILF) TO DA-KEY GO U.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE VDU-AB TO VDU-L.
           ADD 1 TO VDU-L.
           MOVE 0 TO WX.
       S.  MOVE DA-ARNUM TO WD-ART AR-NUM.
           MOVE 2 TO VDU-P.
           DISPLAY WD-ART AT VDU-LP.
           MOVE 70 TO VDU-P.
           DISPLAY DA-BEZ with SIZE 11 AT VDU-LP.
           MOVE 1 TO WY.
       T.  PERFORM BSPOS.
           IF WX NOT = 99 and WY = 10 ADD 1 TO VDU-L.
           IF DA-ANZ(WY) NOT = 0 MOVE DA-ANZ(WY) TO WH-ANZ
               PERFORM MG
               DISPLAY WD-MG with highlight AT VDU-LP.
           IF WX = 99; IF WY < 27 ADD 1 TO WY GO T ELSE GO V.
           IF WY < 9 ADD 1 TO WY GO T.
           ADD 1 TO WX.
           MOVE DA-KEY TO WT-KEY(WX).
           GO Q.
      *---------------------------------------> anlegen oder aendern <-
       U.  MOVE 99 TO WX.
           READ DAUER INVALID MOVE DA-ARNUM TO WH-NUM
               MOVE 0 TO WX
               INITIALIZE DA-SATZ
               MOVE DE-KTONR TO DA-KTONR
               MOVE WH-NUM TO DA-ARNUM.
           IF ZUGRIF PERFORM BESETZT GO U.
           MOVE 12 TO VDU-L.
           MOVE DA-GRM TO WF-GRM.
           IF WX = 99 GO S.
       V.  MOVE DA-ARNUM TO WD-ART AR-NUM.
           READ ARTIKEL IGNORE LOCK INVALID PERFORM NO-REC GO OA.
           MOVE AR-GRM TO WF-GRM.
           MOVE DE-STATIS TO WF-SON.
           MOVE WF-GRM TO DA-GRM.
           DISPLAY WD-ART AT 1202.
           DISPLAY WD-ART with highlight AT 0410.
           DISPLAY AR-BEZ with highlight AT 0416.
           MOVE AR-BEZ TO DA-BEZ.
           MOVE DE-SPERR TO DA-DAT(1).
           MOVE DE-TOUR TO DA-TOUR.
           MOVE DE-SUB TO DA-SUB.
           MOVE DE-EAN TO DA-EAN.
           MOVE DE-FAKART TO DA-FART.
           MOVE 1 TO WY.
           DISPLAY "           " AT 1270.
           IF DA-DAT(2) > 0 MOVE DA-DAT(2) TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM AT 0949.
           IF DA-DAT(3) > 0 MOVE DA-DAT(3) TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM AT 0970.
       W.  PERFORM BSPOS.
           MOVE DA-ANZ(WY) TO WH-ANZ.
           PERFORM MG.
           EVALUATE WF-NK
               WHEN 0 CALL "CAUP" USING "0000006007" WH-CREG
               WHEN 1 CALL "CAUP" USING "0000005107" WH-CREG
               WHEN 2 CALL "CAUP" USING "0000004207" WH-CREG.
           IF ESC GO O.
           IF WOLI; IF WY > 1 SUBTRACT 1 FROM WY GO W.
           IF FEHLER GO W.
           MOVE WH-WERT TO DA-ANZ(WY).
           IF WF-NK = 1 MULTIPLY 10 BY WH-WERT GIVING DA-ANZ(WY).
           IF WF-NK = 2 MULTIPLY 100 BY WH-WERT GIVING DA-ANZ(WY).
           MOVE DA-ANZ(WY) TO WH-ANZ.
           PERFORM MG.
           DISPLAY WD-MG with highlight AT VDU-LP.
           IF WX NOT = 0; IF WY < 27 ADD 1 TO WY GO W ELSE GO X.
           MOVE 98 TO WX.
       WA. COMPUTE VDU-P = WY * 7.
           MOVE DA-ANZ(1) TO DA-ANZ(WY) WH-ANZ.
           PERFORM MG.
           MOVE 12 TO VDU-L.
           DISPLAY WD-MG AT VDU-LP.
           IF WY < 9 ADD 1 TO WY GO WA.
           MOVE 2 TO WY GO W.
       X.  MOVE DA-DAT(2) TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0109498008" WH-CREG.
           IF ESC GO O.
           IF WOLI MOVE 9 TO WY GO W.
           MOVE WX-DATUM TO DA-DAT(2).
           IF DA-DAT(2) = 0 MOVE 0 TO DA-DAT(3)
               DISPLAY ALL " " with SIZE 9 AT 0948
           ELSE DISPLAY VDU-DATUM with highlight AT 0949.
       Y.  MOVE DA-DAT(3) TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0109708008" WH-CREG.
           IF WOLI GO X.
           IF DA-DAT(2) NOT = 0 AND WX-DATUM = 0 GO Y.
           MOVE WX-DATUM TO DA-DAT(3).
           IF WX-DATUM > 0 DISPLAY VDU-DATUM with highlight AT 0970
               ELSE CALL "CAUP" USING "1309700980" WH-CREG.
           GO O.
       Z.  EXIT.
      ******************************************************************
       KUNDENNR SECTION.
       A.  CALL "CAUP" USING "0710100560000" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "fÅr Kunden: " AT VDU-LP.
           ADD 12 TO VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ with SIZE 55 AT VDU-LP.
           MOVE DE-KTONR TO FA-KTONR WV-KTONR.
           MOVE 0 TO FA-ARNUM.
           START FAKTDAT KEY NOT < FA-KEY INVALID PERFORM NO-REC GO Z.
           READ FAKTDAT NEXT IGNORE LOCK AT END PERFORM NO-REC GO Z.
           IF FA-KTONR NOT = WV-KTONR PERFORM NO-REC GO Z.
           MOVE DE-KTONR TO FA-KTONR WV-KTONR.
           MOVE 0 TO FA-ARNUM.
           ADD 0010 VDU-ECK GIVING VDU-LP.
           MOVE "EDIDAT\PRICAT.EDI" TO WN-EDIFACT.
           DISPLAY WN-EDIFACT with highlight AT VDU-LP.
           START FAKTDAT KEY NOT < FA-KEY INVALID GO Z.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO G.
       Z.  EXIT.
      ************************************************ Edifact-Ausgabe *
       PRICAT SECTION.
       A.  PERFORM KUNDENNR.
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
           MOVE "UNB+UNOC:3+" TO EDF-SATZ.
           MOVE WH-EAN TO WM-CD.                          *> Versender
           PERFORM ANHANG.
           MOVE ":14:" TO WM-CD.
           PERFORM ANHANG.
           IF DE-GLN(1:1) not = 0 MOVE DE-GLN TO WM-CD
           else MOVE DE-GLN(2:) TO WM-CD.
           PERFORM ANHANG.
           MOVE ":14+" TO WM-CD.
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
           MOVE "96A:" TO WM-CD.
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
           MOVE WH-HHMM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":204" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
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
           MOVE WE-UST(1:2) TO WM-CD.
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
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-KONTROL TO WM-CD.
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
       BSPOS SECTION.
       A.  IF WY > 18 COMPUTE VDU-P = (WY - 18) * 7
                      IF WX > 90 MOVE 14 TO VDU-L
                      END-IF  GO Z.
           IF WY > 09 COMPUTE VDU-P = (WY - 9) * 7
                      IF WX > 90 MOVE 13 TO VDU-L
                      END-IF GO Z.
           COMPUTE VDU-P = WY * 7.
           IF WX > 90 MOVE 12 TO VDU-L.
       Z.  EXIT.
      ************************************ Reorganisation Sonderpreise *
       REORG SECTION.
       A.  IF PX > 3 GO G.
           MOVE FA-TAB(PX + 1) TO FA-TAB(PX).
           SET PX UP BY 1 GO A.
       G.  MOVE 0 TO FA-AB(4) FA-BIS(4) FA-PREIS(4) FA-RET(4) FA-RAB(4).
           CALL "CAUP" USING "1312012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       LOEFAK SECTION.
       A.  MOVE FA-ARNUM TO WV-ARNUM.
           INITIALIZE FA-SATZ.
           MOVE DE-KTONR TO FA-KTONR.
           MOVE WV-ARNUM TO FA-ARNUM.
       Z.  EXIT.
