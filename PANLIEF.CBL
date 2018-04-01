      $SET LINKCOUNT"256" ANS85 BOUND AUTOLOCK NOALTER
      $set segsize "52000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANLIEF.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEART.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSEKON.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEINK.CPY.
           SELECT HOFERART   ASSIGN TO "HOFART.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY HA-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT MODEMDAT   ASSIGN TO "MODEM.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANLIEF.CPY.
       COPY PANFDINK.CPY.
      ********************************************* Artikelliste HOFER *
       FD  HOFERART       external     LABEL RECORD STANDARD.
       01  HA-SATZ.
           03  HA-KEY.
               05 HA-KTONR             PIC 9(5)      COMP.
           03  HA-BEZ                  PIC X(20).
           03  HA-DATUM                PIC 9(8)      COMP.
           03  HA-ARTAB        OCCURS 40.
               05 HA-ARNUM             PIC 9(4)      COMP.
               05 HA-MENGE             PIC 9(5)V99   COMP.
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
           03  WH-PX                   PIC 9999    COMP    OCCURS 2.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  VDU-AB                  PIC 9999.
           03  VR REDEFINES VDU-AB.
               05 VDU-LA               PIC 99.
               05 VDU-PA               PIC 99.
           03  WH-IKEY                 PIC 9(5).
           03  WH-LFKEY                PIC X(15).
           03  WI                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  IZ                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  IY                      PIC 99        COMP.
           03  IQ                      PIC 99        COMP.
           03  WH-RAS                  PIC 99        COMP.
           03  WK-NUM                  PIC 9(6)      COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WK-D                    PIC 9999      COMP.
           03  WM-LFNUM                PIC 9(6)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-LFNUM                PIC 9(5)      COMP.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WA-ANZ                  PIC 9(5)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(5).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9999V9.
           03  WH-ANZ2                 PIC S999V99.
           03  WH-MENGE                PIC S9(5)     COMP-3.
           03  WH-ART                  PIC 99        COMP-3.
           03  WS-LIEF                 PIC S9(5)     COMP-3.
           03  WS-RET                  PIC S9(5)     COMP-3.
           03  WH-L                    PIC 99        COMP.
           03  WM-NEU                  PIC 99        COMP.
           03  WD-RMON                 PIC Z9/99.
           03  WD-NUM                  PIC ZZZ.ZZ9.
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
           03  WD-ART                  PIC ZZZ9.
           03  WD-TG                   PIC Z9.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WM-BS                   PIC 99.
           03  WS-SUM                  PIC S9(9)V99   COMP-3 OCCURS 3.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
           03  WH-MEH                  PIC 99         COMP.
           03  WH-REST                 PIC S9(9)V99   COMP.
           03  WD-SALDO                PIC ZZZZZ.ZZ9,99-.
           03  WT-HART.
               05 WT-ARNUM             PIC 9(4)       COMP  OCCURS 40.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WD-X                    PIC X.
           03  WX-PRSTAT               PIC 99   COMP-X.
      *--------------------------------------------> check_file_exist <-
           03  WH-FNAME                PIC X(12).
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)   COMP-X.
               05 WF-TT                PIC 99     COMP-X.
               05 WF-MO                PIC 99     COMP-X.
               05 WF-YY                PIC 9999   COMP-X.
               05 WF-HH                PIC 99     COMP-X.
               05 WF-MM                PIC 99     COMP-X.
               05 WF-SS                PIC 99     COMP-X.
               05 WF-HS                PIC 99     COMP-X.
       01  WT-DK     external.
           03  WR-TN                   PIC X(10)  OCCURS 7.
           03  WT-KON                  PIC 99     OCCURS 12.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING  "PANEFAKT.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING  "PANELFS.DAT"  WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING  "PANARTIK.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING  "PANDEBIT.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING  "PANEKONS.DAT" WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON INKASSO.
       A.  CALL "CADECL" USING  "PANINKAS.DAT" WH-CREG.
       DECL-J SECTION.         USE AFTER ERROR PROCEDURE ON TX-LFTEXT.
       A.  CALL "CADECL" USING  "PANLFTX.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 10 PERFORM LIEFER.
           IF WL-CA = 20 PERFORM VKFABRE.
           IF WL-CA = 30 PERFORM LFWAHL.
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
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO VDU-L.
           IF VDU-L < 21 GO Z.
       B.  CALL "CAUP" USING "1407012180000" WH-CREG.
           SUBTRACT 1 FROM VDU-L.
           SUBTRACT 1 FROM WH-L.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
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
           GO E.
       B.  IF WR-A2 NOT = 0 GO X.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ1 ON SIZE ERROR GO X.
           MOVE WH-ANZ1 TO WD-ANZ1 WD-MGB.
           MOVE WD-ANZ1 TO WD-MENGE.
           MOVE WD-MGB  TO WD-MG.
           MULTIPLY 10 BY WH-ANZ.
           GO E.
       C.  IF WH-ANZ > 999999 OR WH-ANZ < -999999 GO X.
           IF WR-A2 NOT = 0 AND WR-A1 NOT = 0 GO X.
           MOVE WH-ANZ TO WD-ANZ WD-MGA.
           MOVE WD-ANZ TO WD-MENGE.
           MOVE WD-MGA TO WD-MG.
       E.  DISPLAY WD-MENGE WITH HIGHLIGHT AT VDU-LP.
           GO Z.
       X.  DISPLAY "Menge zu gross" AT 2401
               PERFORM WEITER.
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
      ************************************** Verkaufsfahrerabrechnung *
       VKFABRE SECTION.
       A.  MOVE "  Vkf-Abrechnung" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE WM-DATUM TO WV-DATUM.
       B.  DISPLAY "Tour: " AT 0439.
           DISPLAY "<ret>= Nummer, <esc>= Ende" AT 2301.
           CALL "CAUP" USING "0004452002" WH-CREG.
           IF ESC UNLOCK KONSTANT GO Z.
           IF NOT RET OR WH-NUM > 20 OR WH-NUM = 0 GO B.
       C.  MOVE WH-NUM TO WD-POS DE-TOUR WK-D.
           DISPLAY WD-POS with highlight AT 0445.
           ADD 10 WH-NUM GIVING WH-KEY.
       D.  READ KONSTANT INVALID KEY PERFORM NO-REC GO A.
           IF ZUGRIF PERFORM BESETZT GO D.
           DISPLAY KO-TOUR with highlight AT 0449.
       E.  PERFORM LIEFER.
           MOVE WM-DATUM TO WV-DATUM.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM NACH
              IF ESC GO A.
              IF RET GO C.
      *------------------------------------> Rechnung im Vorlaufmenue <-
       Z.  EXIT.
      *************************************** Lieferscheinbearbeitung *
       LIEFER SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE 0 TO WH-NUM.
           IF WH-PG = 10 MOVE 9 TO WH-PG
               MOVE DE-TOUR TO WD-POS
               DISPLAY WD-POS with highlight AT 0275.
       B.  DISPLAY "Kunden-Nr.:" AT 0301.
           UNLOCK DEBITOR.
           UNLOCK ARTIKEL.
           UNLOCK LFSCHEIN.
           DISPLAY "alpha+<ret>= suchen, Nr.+<ret>= Kundennummer, <esc>=
      -        " Ende" AT 2301.
           DISPLAY "<#>= letzte Nr." AT 2401.
           CALL "CAUP" USING "0003135010" WH-CREG.
           IF ESC GO X.
           IF NOT RET AND NOT KIST GO B.
           IF KIST AND DE-SATZ NOT = LOW-VALUE
                DIVIDE 10 INTO DE-KTONR GIVING WH-NUM
           ELSE IF WH-NUM = 0 MOVE WV-MCODE TO DE-MCODE
                   CALL "PANANZ" USING "10DEB-ANZ" WH-CREG
                   CANCEL "PANANZ"
                   IF ESC GO B.
           IF WH-NUM = 0 GO B.
           IF WH-PG = 9; IF WH-NUM < 21 GO B.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0313.
           MOVE 0 TO WM-MERKER.
           READ DEBITOR IGNORE LOCK INVALID MOVE 1 TO WM-MERKER
               CALL "PANDEBAN" USING "61DEB-ANL" WH-CREG
               CANCEL "PANDEBAN".
           IF DE-KTONR = 0 GO A.
           IF ZUGRIF PERFORM BESETZT GO B.
           UNLOCK DEBITOR.
       C.  DISPLAY "Nr.: " with SIZE 20 AT 0301.
           DISPLAY WD-KTO with highlight AT 0306.
           MOVE DE-BEZ TO WT-BEZ.
           IF WH-PG = 9 MOVE WK-D TO DE-TOUR.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with highlight SIZE 65 AT 0315.
           DISPLAY "Lfs-Nr.:" AT 0401.
       D.  DISPLAY "<ret>= Lfschein-Nr., <esc>= Abbruch, <ret-leer>= Lfs
      -        "cheine anzeigen" AT 2301.
           DISPLAY "<#/ >= nÑchste Lfsch.-Nr., < >= Hilfe" AT 2401.
           CALL "CAUP" USING "0004105005" WH-CREG.
           IF HELP CALL "CAUP" USING "190007000801" WH-CREG GO D.
           MOVE WS-TASTE TO WX-TASTE.
       E.  IF ESC GO A.
           MOVE DE-KTONR TO LF-KTONR.
           MOVE 0 TO LF-NUM LF-DATUM LF-ARNUM WM-NEU.
           IF KIST OR AB MOVE 9 TO WM-NEU GO K.
           IF NOT RET GO D.
           MOVE WH-NUM TO WM-LFNUM LF-NUM WD-NUM.
           IF LF-NUM = 0; IF WH-MCODE NOT = SPACE GO D
               ELSE CALL "PANANZ" USING "50LFSANZ" WH-CREG
                    CANCEL "PANANZ"
                    MOVE LF-NUM TO WD-NUM WM-LFNUM
                    IF WH-NUM = 0 AND LF-NUM = 0 GO F ELSE
                       IF VDU-L NOT = 4 GO E.
           CALL "CAUP" USING "1304090480" WH-CREG.
           DISPLAY WD-NUM with highlight AT 0409.
       F.  START LFSCHEIN KEY NOT < LF-KEY INVALID KEY
               MOVE WH-NUM TO LF-NUM GO K.
       G.  READ LFSCHEIN NEXT AT END GO D.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF AB MOVE LF-NUM TO WM-LFNUM WD-NUM
               IF LF-KTONR NOT = DE-KTONR SET KIST TO TRUE GO E
           ELSE DISPLAY WD-NUM with highlight AT 0409.
           IF LF-KTONR NOT = DE-KTONR OR LF-NUM NOT = WM-LFNUM
               MOVE WH-NUM TO LF-NUM GO K.
           MOVE 1 TO WM-NEU.
           DISPLAY "vom: " AT 0420.
           MOVE LF-DATUM TO WC-DATUM WV-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0425.
           DISPLAY "Tour: " AT 0439.
           MOVE LF-TOUR TO DE-TOUR.
       H.  MOVE DE-TOUR TO WD-TG.
           IF WH-PG = 9; IF WV-DATUM NOT = WM-DATUM GO K
               ELSE MOVE WK-D TO WD-TG DE-TOUR
                    DISPLAY WD-TG with highlight AT 0446
                    DISPLAY KO-TOUR AT 0449
               ELSE DISPLAY WD-TG with highlight AT 0446.
           MOVE 6 TO VDU-L.
           PERFORM LFZEILE.
           IF AB CALL "CAUP" USING "1307012480" WH-CREG
               MOVE DE-KTONR TO LF-KTONR
               ADD 1 WM-LFNUM GIVING LF-NUM
               MOVE 0 TO LF-DATUM LF-ARNUM WM-NEU
               MOVE LF-NUM TO WH-NUM WD-NUM
               CALL "CAUP" USING "1304090480" WH-CREG GO F.
      *-----------------------------------> Sofortdruck bei WH-PG = 3 <-
           IF (WH-PG = 3 and not KIST) or (WH-PG = 1 and KIST)
               MOVE WM-LFNUM TO LF-NUM
               CALL "PANLFDRU" USING "10SOFDRU" WH-CREG
               CANCEL "PANLFDRU".
           GO A.
       K.  DISPLAY "<ret-leer>= wiederholen, <esc>= Abbruch" AT 2301.
           DISPLAY "vom: " AT 0420.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0425.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0104256006" WH-CREG.
           IF ESC GO A.
           IF not RET GO K.
           IF WX-DATUM = 0 GO K.
           MOVE WX-DATUM TO LF-DATUM WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT 0425.
           IF WV-DATUM > WE-MAXDAT DISPLAY "Datum zu gross" AT 2401
               PERFORM WEITER GO K.
           IF WV-DATUM < WE-MINDAT DISPLAY "Datum zu klein" AT 2401
               PERFORM WEITER GO K.
           MOVE WV-DATUM TO WZ-DATUM.
           IF LF-NUM = 0 COMPUTE LF-NUM = WZ-TAG * 100
               IF DE-STATIS > 1 ADD 99 TO LF-NUM
               end-if
               MOVE LF-NUM TO WD-NUM WM-LFNUM
               DISPLAY WD-NUM with highlight AT 0409.
           IF WM-NEU = 9 MOVE 0 TO WM-NEU GO M.
       L.  MOVE DE-TOUR TO WD-TG WH-WERT.
           DISPLAY "Tour:  " AT 0439 WD-TG.
           IF WH-PG = 9 GO H.
           CALL "CAUP" USING "0004462002" WH-CREG.
           IF ESC GO A.
           IF WOLI GO K.
           IF NOT RET GO L.
           IF WH-WERT = 0 OR WH-WERT > 20 GO L.
           MOVE WH-WERT TO DE-TOUR WD-TG.
           GO H.
       M.  START LFSCHEIN KEY NOT < LF-KEY INVALID KEY GO L.
       N.  READ LFSCHEIN NEXT IGNORE LOCK AT END GO L.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF LF-KTONR NOT = DE-KTONR OR LF-DATUM NOT = WV-DATUM
               MOVE WM-LFNUM TO LF-NUM GO L.
           DIVIDE 100 INTO LF-NUM GIVING WK-ANZ REMAINDER WK-ANZ.
           IF WK-ANZ > 90 GO L.
           ADD 1 TO LF-NUM.
           MOVE 0 TO LF-ARNUM.
           MOVE LF-NUM TO WD-NUM WM-LFNUM.
           DISPLAY WD-NUM with highlight AT 0409.
           GO M.
       X.  UNLOCK KONSTANT.
           UNLOCK ARTIKEL.
           UNLOCK LFSCHEIN.
       Z.  EXIT.
      ****************************************** Text auf Lieferschein *
       LF-TEXTE SECTION.
       A.  CALL "CAUP" USING "270710066400011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lieferscheintext " with highlight AT VDU-LP.
           OPEN I-O TX-LFTEXT.
           INITIALIZE TX-SATZ.
           MOVE DE-KTONR TO TX-KTONR.
           MOVE WM-LFNUM TO TX-LFNUM.
           MOVE WV-DATUM TO TX-LFDAT.
       C.  READ TX-LFTEXT INVALID NEXT SENTENCE.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO TX-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT VDU-LP.
           MOVE TX-LFNUM TO WD-NUM.
           ADD 10 TO VDU-LP.
           DISPLAY WD-NUM AT VDU-LP.
           MOVE TX-LFDAT TO WC-DATUM.
           PERFORM DATDREH.
           ADD 10 TO VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           IF TX-TEXT(1) not = SPACE or TX-TEXT(2) not = SPACE
               DISPLAY TX-TEXT(1) with highlight AT VDU-LP
               ADD 100 TO VDU-LP
               DISPLAY TX-TEXT(2) with highlight AT VDU-LP.
           MOVE 1 TO WZ.
           PERFORM WITH TEST AFTER UNTIL WZ > 2 or ESC
                   or ENDE
               DISPLAY "<esc>= lîschen, <ret>= Text, <Ende>= fertig"
                   AT 2301
               COMPUTE VDU-LP = WZ * 100 + 303 + VDU-ECK
               DISPLAY TX-TEXT(WZ) with highlight AT VDU-LP
               COMPUTE VDU-LP = WZ * 100 + 303
               MOVE TX-TEXT(WZ) TO WT-TX
               CALL "CAUP" USING "1200000160" WH-CREG
               IF RET or ENDE MOVE WT-TX TO TX-TEXT(WZ)
               end-if
               IF ESC MOVE SPACE TO TX-TEXT(1) TX-TEXT(2)
                      MOVE 3 TO WZ
               end-if
               IF RET ADD 1 TO WZ.
       L.  IF TX-TEXT(1) = SPACE and TX-TEXT(2) = SPACE GO O.
           WRITE TX-SATZ INVALID REWRITE TX-SATZ.
           GO X.
       O.  DELETE TX-LFTEXT INVALID NEXT SENTENCE.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE TX-LFTEXT.
           IF TX-TEXT(1) = SPACE and TX-TEXT(2) = SPACE
               DISPLAY ALL "ƒ" with SIZE 50 AT 0601
           else DISPLAY " Text "
                   with highlight foreground-color 4 AT 0628.
       Z.  EXIT.
      ********************************************** ob Text vorhanden *
       OB-TXT SECTION.
       A.  OPEN I-O TX-LFTEXT.
           DISPLAY ALL "ƒ" with SIZE 50 AT 0601
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WM-DATUM).
           ADD -40 TO WA-DAT.
           COMPUTE WA-DAT = FUNCTION DATE-OF-INTEGER(WA-DAT).
           INITIALIZE TX-SATZ.
           MOVE DE-KTONR TO TX-KTONR.
           START TX-LFTEXT KEY > TX-KEY INVALID GO G.
       C.  READ TX-LFTEXT NEXT AT END GO G.
           IF DE-KTONR not =TX-KTONR GO G.
           IF WA-DAT < TX-LFDAT GO G.
           DELETE TX-LFTEXT INVALID NEXT SENTENCE.
           GO C.
       G.  INITIALIZE TX-SATZ.
           MOVE DE-KTONR TO TX-KTONR.
           MOVE WV-DATUM TO TX-LFDAT.
           MOVE WM-LFNUM TO TX-LFNUM.
           READ TX-LFTEXT INVALID INITIALIZE TX-SATZ.
           IF TX-TEXT(1) not = SPACE or TX-TEXT(2) not = SPACE
               DISPLAY " Text "
                   with highlight foreground-color 4 AT 0628.
       X.  CLOSE TX-LFTEXT.
       Z.  EXIT.
      ********************************************** Lieferscheinzeile *
       LFZEILE SECTION.
       A.  IF VDU-L = 6 DISPLAY "Art-Nr B e z e i c h n u n g          M
      -        "enge  Meh    Retour    Nachlfg." AT 0501
               DISPLAY ALL "ƒ" with SIZE 80 AT 0601
               IF WH-PG = 5 DISPLAY " Retour " with highlight AT 0670
               ELSE DISPLAY " Liefg. " with highlight AT 0670.
           PERFORM BS-ZEIL.
           MOVE VDU-L TO WH-L.
           IF WX-TASTE = 99 GO F.
           PERFORM OB-TXT.
       B.  MOVE SPACE TO WT-BEZ.
           UNLOCK ARTIKEL.
           CALL "CAUP" USING "1300012480" WH-CREG.
           DISPLAY "alpha+<ret>= suchen, Nr.+<ret>= Artikel, < >= Raster
      -        ", <ret-leer>= anzeigen" AT 2301.
           DISPLAY "<esc>= Ende, <>= Retouren, <#>= kein Druck, < >= Hi
      -        "lfe, < >= nÑchster Lfsch." AT 2401.
           IF WH-PG = 1 DISPLAY " mit" AT 2434.
           DISPLAY "<Entf>= Lfsch. lîschen, <Pos 1>= TourÑnderung, <Einf
      -        "g>= Text" AT 2501.
           CALL "CAUP" USING "0000014010" WH-CREG.
           MOVE WS-TASTE TO WX-TASTE.
           IF HELP CALL "CAUP" USING "1900040004010" WH-CREG GO B.
           IF EINF PERFORM LF-TEXTE
                   GO B.
           IF ENTF PERFORM LOELFS GO X.
       C.  IF TABL or (RET and WH-MCODE = SPACE) PERFORM LFSCHANZ
               IF WH-PG > 9 SUBTRACT 10 FROM WH-PG GO A else GO A.
           IF ESC MOVE SPACE TO WV-MCODE GO X.
           IF WOLI; IF WH-PG < 10 ADD 10 TO WH-PG
               DISPLAY " Retour " with highlight AT 0670 GO A.
           IF WORE SET RET TO TRUE
               CALL "CAUP" USING "1500004004" WH-CREG.
           IF KIST OR AB GO X.
           IF APOS PERFORM TOURWECHSEL
                   IF NOT ESC MOVE DE-TOUR TO WD-TG
                       DISPLAY WD-TG with highlight AT 0446
                       GO B.
           IF NOT RET GO B.
           MOVE SPACE TO AR-SA.
           IF WH-NUM = 0 MOVE WV-MCODE TO AR-ABEZ
               CALL "PANANZ" USING "20ARTANZ" WH-CREG
               CANCEL "PANANZ"
               DISPLAY "       " AT VDU-LP
               IF WH-NUM = 0 GO B.
       D.  IF VDU-L > 20 PERFORM B IN BS-ZEIL GO D.
           MOVE WH-NUM TO AR-NUM LF-ARNUM WD-ART.
           DISPLAY WD-ART AT VDU-LP.
           MOVE DE-KTONR TO LF-KTONR.
           MOVE DE-FAKART TO LF-FART.
           MOVE WM-LFNUM TO LF-NUM.
           MOVE WV-DATUM TO LF-DATUM.
           MOVE 1 TO WM-NEU.
       E.  READ LFSCHEIN INVALID KEY MOVE 0 TO WM-NEU LF-PREIS 
               LF-STAT LF-GRM LF-MENGE(1) LF-MENGE(2) LF-MENGE(3)
               MOVE SPACE TO LF-BEZ.
           IF ZUGRIF PERFORM BESETZT GO B.
           MULTIPLY -1 BY LF-MENGE(2) GIVING WS-SUM(2).
           MULTIPLY -1 BY LF-MENGE(3) GIVING WS-SUM(3).
       F.  READ ARTIKEL IGNORE LOCK INVALID PERFORM NO-REC GO B.
           IF ZUGRIF PERFORM BESETZT GO F.
           IF AR-BEZ(25:1) = "*" DISPLAY "Kein Bestellartikel" AT 2401
               PERFORM WEITER GO B.
           MOVE AR-GRM TO LF-GRM WF-GRM.
           MOVE DE-TOUR TO LF-TOUR.
           MOVE DE-EAN TO LF-EAN.
           MOVE DE-SUB TO LF-SUB.
           MOVE AR-SGRP TO LF-SGRP.
           ADD 1 WF-MEH GIVING WH-MEH.
           MOVE AR-BEZ TO LF-BEZ.
           MOVE AR-SGRP TO LF-SGRP.
           IF AR-SGRP > 0 MOVE AR-BEZ TO LF-ARBEZ.          *> nur Kern
           IF WS-TASTE = 2 PERFORM ANZFAKT GO B.
           IF LF-DATUM < AR-ADAT MOVE AR-VKP TO LF-PREIS
                            ELSE MOVE AR-AVKP TO LF-PREIS
               IF LF-PREIS = 0 MOVE AR-VKP TO LF-PREIS.
           MOVE 8 TO VDU-P.
           DISPLAY LF-BEZ AT VDU-LP.
           MOVE 46 TO VDU-P.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           MOVE 1 TO WX.
       G.  COMPUTE VDU-P = WX * 15 + 20.
           IF WX = 3 MOVE 61 TO VDU-P.
           IF LF-MENGE(WX) NOT = 0 MOVE LF-MENGE(WX) TO WH-ANZ
               PERFORM MG
               DISPLAY WD-MENGE with highlight AT VDU-LP.
           IF WX < 3 ADD 1 TO WX GO G.
           MOVE 1 TO WX.
           IF WH-PG > 4 MOVE 2 TO WX.
       H.  DISPLAY "<ret>= Menge, (alle Mengen 0 = loeschen)" AT 2301.
           COMPUTE VDU-P = WX * 15 + 20.
           IF WX = 3 MOVE 61 TO VDU-P.
           MOVE LF-MENGE(WX) TO WH-ANZ.
           PERFORM MG.
           DISPLAY WD-MENGE with highlight AT VDU-LP.
           IF WX = 2 MULTIPLY -1 BY WH-WERT.
           IF WF-NK = 0 CALL "CAUP" USING "0000006007" WH-CREG.
           IF WF-NK = 1 CALL "CAUP" USING "0000005107" WH-CREG.
           IF WF-NK = 2 CALL "CAUP" USING "0000004207" WH-CREG.
           IF WOLI OR ESC GO B.
           IF ENTF AND WH-NUM = 99999 GO I.
           IF FEHLER GO H.
           MOVE WH-WERT TO WH-ANZ.
           IF WX = 2 MULTIPLY WH-WERT BY -1 GIVING WH-ANZ.
           PERFORM DISP-MENGE.
           IF ESC GO H.
           MOVE WH-ANZ TO LF-MENGE(WX).
           IF WH-PG = 9; IF WX < 3 ADD 1 TO WX GO H.
           IF WH-PG = 9 ADD 30 DE-TOUR GIVING LF-TOUR
               ADD LF-MENGE(2) TO WS-SUM(2)
               ADD LF-MENGE(3) TO WS-SUM(3).
           IF LF-STAT = 99 MOVE 98 TO LF-STAT.
           IF LF-MENGE(1) NOT = 0 OR LF-MENGE(2) NOT = 0
              OR LF-MENGE(3) NOT = 0 GO J.
      *---------------------------------> loeschen Lieferscheinzeile <-
           IF WM-NEU = 0 GO B.
       I.  DELETE LFSCHEIN INVALID KEY DISPLAY "nicht lîschbar!"
               AT 2401 PERFORM WEITER GO G.
           MOVE 36 TO VDU-P.
           CALL "CAUP" USING "1300350080" WH-CREG.
           DISPLAY "-- gelîscht --" AT VDU-LP.
           IF WH-PG = 9 GO Q.
           GO A.
       J.  WRITE LF-SATZ INVALID KEY REWRITE LF-SATZ.
           MOVE 2 TO WM-NEU.
           IF WH-PG = 9 GO Q.
           GO A.
      *----------------------------------------> Vkfsfahrerabrechnung <-
       Q.  COMPUTE LF-KTONR = DE-TOUR * 10 + 2.
           COMPUTE LF-TOUR = DE-TOUR + 60.
           MOVE 6 TO LF-FART.
           MOVE WV-DATUM TO WZ-DATUM.
           COMPUTE LF-NUM = WZ-TAG * 100 + 9.
       R.  READ LFSCHEIN INVALID KEY MOVE 0 TO LF-STAT
               LF-MENGE(1) LF-MENGE(2) LF-MENGE(3).
           IF ZUGRIF PERFORM BESETZT GO E.
           SUBTRACT WS-SUM(2) FROM LF-MENGE(2).
           SUBTRACT WS-SUM(3) FROM LF-MENGE(3).
      *---------------------------------> Vkfs.-Fahrer Satz loeschen <-
           IF LF-MENGE(1) NOT = 0 OR LF-MENGE(2) NOT = 0
              OR LF-MENGE(3) NOT = 0 GO S.
           DELETE LFSCHEIN INVALID KEY GO A.
           GO A.
       S.  REWRITE LF-SATZ INVALID KEY WRITE LF-SATZ.
           GO A.
       X.  IF WH-PG > 9 SUBTRACT 10 FROM WH-PG.
       Z.  EXIT.
      ************************************ Nachlass- u. Inkassosummen *
       NACH SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
       B.  MOVE WK-D TO WD-POS WH-WERT.
           DISPLAY "Tour: " AT 0439.
           DISPLAY WD-POS with highlight AT 0445.
           DISPLAY KO-TOUR with highlight AT 0449.
           DISPLAY "<esc>= Ende, <#>= anzeigen, < >= Abrechnung, <ret-le
      -        "er>= weiter " AT 2301.
           MOVE 5 TO WX.
           CALL "CAUP" USING "0023650000" WH-CREG.
           IF ESC GO Z.
           IF RET MOVE WK-D TO WH-NUM GO Z.
           IF NOT KIST AND NOT AB GO C.
       C.  MOVE 1 TO DE-FNR.
           MOVE WK-D TO DE-TOUR.
           COMPUTE DE-KTONR = DE-TOUR * 10 + 2.
           MOVE DE-KTONR TO WV-KTONR.
       D.  READ DEBITOR IGNORE LOCK INVALID KEY GO B.
           IF KIST PERFORM UMANZ GO A.
           MOVE 0 TO KO-SUM(1) KO-SUM(2) KO-SUM(3).
      *------------------------------> Rabatte, Ablieferung, Inkassi <-
           MOVE 5 TO WX.
           CALL "CAUP" USING "1310012480" WH-CREG.
           DISPLAY "Rabatte......:" AT 1001.
           DISPLAY "Ablieferung..:" AT 1101.
           DISPLAY "Kundeninkassi:" AT 1201.
       F.  DISPLAY "<esc>= zurÅck, <ret-leer>= Ende, <tab>= lîschen"
               AT 2301.
           ADD 5 WX GIVING VDU-L.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "0000305208" WH-CREG.
           IF WM-KO = 3 AND WX = 7 AND AB PERFORM AUFTEIL
               MOVE WS-BET TO WH-WERT
               MOVE 16 TO VDU-P
               DISPLAY WD-BET with highlight AT VDU-LP
               MOVE 7 TO WX
               SET RET TO TRUE.
           IF ESC; IF WX > 5 SUBTRACT 1 FROM WX GO F ELSE GO A.
           IF TABL CALL "CAUP" USING "1300160080" WH-CREG
               MOVE 0 TO KO-SUM(WX - 4) GO F.
           IF NOT RET GO F.
           IF WH-WERT = 0 GO H.
           MOVE WH-WERT TO WD-BET.
           MOVE 30 TO VDU-P.
           DISPLAY WD-BET with highlight AT VDU-LP.
           ADD WH-WERT TO KO-SUM(WX - 4).
           MOVE KO-SUM(WX - 4) TO WD-BET.
           MOVE 16 TO VDU-P.
           DISPLAY WD-BET with highlight AT VDU-LP.
           GO F.
       H.  IF WM-KO NOT = 3; IF WX < 7 ADD 1 TO WX GO F.
           IF WM-KO = 3; IF WX < 6 ADD 1 TO WX GO F
                         else GO K.
       I.  MOVE 1 TO WX-TASTE.
           IF KO-SUM(1) = 0 GO J.
           COMPUTE LF-KTONR = WK-D * 10 + 2.
           COMPUTE LF-TOUR = WK-D + 60.
           MOVE 6 TO LF-FART.
           MOVE WV-DATUM TO LF-DATUM WZ-DATUM.
           COMPUTE LF-NUM = WZ-TAG * 100 + 9.
           MOVE 999 TO LF-ARNUM.
           MOVE 0 TO LF-STAT LF-MENGE(1) LF-MENGE(2) LF-MENGE(3).
           MOVE SPACE TO LF-BEZ.
           DIVIDE -1,1 INTO KO-SUM(1) GIVING LF-PREIS.
           WRITE LF-SATZ INVALID KEY REWRITE LF-SATZ.
       J.  REWRITE KO-SATZ.
           GO Z.
       K.  PERFORM AUFTEIL.
           IF ESC MOVE 6 TO WX GO F.
           MOVE WS-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT 1230.
           MOVE 7 TO WX.
       L.  DISPLAY "<esc>= zurÅck, <ret-leer>= Ende" AT 2301.
           CALL "CAUP" USING "0023330000" WH-CREG.
           IF ESC MOVE 6 TO WX GO F.
           IF NOT RET GO L.
           MOVE WS-BET TO KO-SUM(WX - 4).
           MOVE KO-SUM(WX - 4) TO WD-BET.
           DISPLAY WD-BET with highlight AT 1216.
           MOVE WV-KTONR TO DE-KTONR.
       M.  READ DEBITOR IGNORE LOCK INVALID KEY GO B.
           GO I.
       Z.  EXIT.
      **************************************** anzeigen d. Umbuchungen *
       UMANZ SECTION.
       O.  DISPLAY "Art-Nr B e z e i c h n u n g          Menge  Meh
      -        "Retour    Nachlfg.   Kunde" AT 0501.
           DISPLAY ALL "ƒ" with SIZE 80 AT 0601.
           MOVE WM-DATUM TO LF-DATUM WV-DATUM.
           ADD WK-D 30 GIVING LF-TOUR.
           MOVE LF-TOUR TO DE-TOUR.
           MOVE 0 TO LF-SUB LF-KTONR LF-NUM LF-ARNUM.
           START LFSCHEIN KEY NOT < LF-AKEY INVALID KEY GO X.
           MOVE 6 TO VDU-L.
       P.  READ LFSCHEIN NEXT IGNORE LOCK AT END GO X.
           MOVE LF-GRM TO WF-GRM.
           IF LF-TOUR NOT = DE-TOUR OR LF-DATUM NOT = WV-DATUM GO X.
           IF VDU-L < 18 ADD 1 TO VDU-L GO R.
       Q.  DISPLAY "<esc>= Ende, <tab>= weiterlisten, <F1>= ab Anfang "
               AT 2301.
           CALL "CAUP" USING "0023512002" WH-CREG.
           IF WOLI CALL "CAUP" USING "1307012480" WH-CREG GO O.
           IF ESC GO X.
           IF NOT TABL GO Q.
           MOVE 7 TO VDU-L.
           CALL "CAUP" USING "1307012480" WH-CREG.
       R.  MOVE LF-ARNUM TO WD-ART.
           MOVE 1 TO VDU-P.
           DISPLAY WD-ART with highlight AT VDU-LP.
           MOVE 8 TO VDU-P.
           DISPLAY LF-BEZ with highlight AT VDU-LP.
           ADD 1 WF-MEH GIVING WH-MEH.
           MOVE 46 TO VDU-P.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           MOVE 1 TO WX.
       S.  COMPUTE VDU-P = WX * 15 + 20.
           IF WX = 3 MOVE 61 TO VDU-P.
           IF LF-MENGE(WX) NOT = 0 MOVE LF-MENGE(WX) TO WH-ANZ
               PERFORM MG
               DISPLAY WD-MENGE AT VDU-LP.
           IF WX < 3 ADD 1 TO WX GO S.
           COMPUTE WD-KTO = LF-KTONR / 10.
           MOVE 72 TO VDU-P.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           GO P.
       X.  DISPLAY "keine weiteren Daten" AT 2401.
           PERFORM WEITER.
           CALL "CAUP" USING "1307012480" WH-CREG.
           MOVE WK-D TO DE-TOUR.
       Z.  EXIT.
      ****************************************** Lieferschein lîschen *
       LOELFS SECTION.
       A.  CALL "CAUP" USING "0707100353000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Wirklich lîschen? < > <Einfg>= ja, <esc>= Abbruch"
               AT VDU-LP.
       C.  CALL "CAUP" USING "1002220000" WH-CREG.
           IF ESC GO X.
           IF NOT EINF GO C.
           MOVE WM-LFNUM TO LF-NUM.
           MOVE DE-KTONR TO LF-KTONR.
           MOVE 0 TO LF-ARNUM WI.
           START LFSCHEIN KEY NOT < LF-KEY INVALID KEY GO Z.
       E.  READ LFSCHEIN NEXT WITH NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF LF-KTONR NOT = DE-KTONR OR LF-NUM NOT = WM-LFNUM GO X.
           DELETE LFSCHEIN INVALID NEXT SENTENCE.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* Wechsel Tournummer *
       TOURWECHSEL SECTION.
       A.  CALL "CAUP" USING "0707080353000" WH-CREG.
           IF WH-NUM not = 0 and WH-NUM < 21
               ADD 203 VDU-ECK GIVING VDU-LP
               DISPLAY "Tourwechsel bitte warten" AT VDU-LP
               GO D.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Tournummer <  >" AT VDU-LP.
           MOVE DE-TOUR TO WH-WERT.
           CALL "CAUP" USING "1002382002" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WH-WERT = 0 or WH-WERT > 20 GO C.
       D.  MOVE WH-WERT TO WI DE-TOUR.
           MOVE WM-LFNUM TO LF-NUM.
           MOVE DE-KTONR TO LF-KTONR.
           MOVE 0 TO LF-ARNUM.
           START LFSCHEIN KEY NOT < LF-KEY INVALID KEY GO Z.
       E.  READ LFSCHEIN NEXT WITH NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF LF-KTONR NOT = DE-KTONR OR LF-NUM NOT = WM-LFNUM GO X.
           MOVE WI TO LF-TOUR.
           REWRITE LF-SATZ.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       LFWAHL SECTION.
       A.  MOVE "   Lieferscheine" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE "PANELAG.DAT" TO WH-FNAME.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
       B.  DISPLAY "1 - Lieferscheinerfassung"  AT 0724
                   "2 - öberleitung Faktura"    AT 0024
                   "3 - Sofortlieferschein"     AT 0024
                   "4 - Grundlieferscheine"     AT 0024
                   "5 - Retourenerfassung"     AT 0024
                   "6 - Druck Lieferscheine"    AT 0024
                   "7 - Druck Backzettel"       AT 0024
                   "8 - PrÅflauf"               AT 0024
                   "9 - nur Kommissionsscheine" AT 0024.
           IF WF-SIZE > 0 DISPLAY "10 - öbern.Lfg./ Prod.Sta./ krit. Tag
      -        "e" AT 0023
               DISPLAY "11 - Tagestourenliste"  AT 0023.
           IF WM-VER = 3
               DISPLAY "12 - Modemdaten "          AT 0023
               DISPLAY "13 - EAN-Etiketten" AT 0023.
           IF WH-BUKEY > 90000 DISPLAY "gedr.: " AT 1255 WH-BUKEY(3:)
               MOVE 0 TO WH-BUKEY.
           DISPLAY " bitte wÑhlen Sie: " with highlight AT 2039.
           DISPLAY "<esc>/<ret-leer>= Programmende" AT 2301.
           CALL "CAUP" USING "0020582002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC MOVE 0 TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 GO Z
               WHEN 1 MOVE " Lieferscheinerfassung" TO WK-GEB
                      PERFORM LIEFER GO A
               WHEN 2 MOVE " öberleitung i. Fakt."  TO WK-GEB
               WHEN 3 MOVE " Sofortlieferschein"    TO WK-GEB
                      PERFORM LIEFER GO A
               WHEN 4 MOVE " Grundlieferscheine"    TO WK-GEB
               WHEN 5 MOVE " Retourenerfassung"     TO WK-GEB
                      PERFORM LIEFER GO A
               WHEN 6 MOVE " Druck Lieferscheine"   TO WK-GEB
               WHEN 7 MOVE "  Druck Backzettel"     TO WK-GEB
               WHEN 8 MOVE "     PrÅflauf"          TO WK-GEB
               WHEN 9 MOVE " Kommissionsscheine"    TO WK-GEB
               WHEN 10; IF WF-SIZE = 0 GO A
               WHEN 11 MOVE "PANLFDRU50TAGTOUR" TO WT-TX
               WHEN 12 MOVE "PANLFDRU60HOFMODE" TO WT-TX
               WHEN 13 MOVE "PANLFDRU70HOFLIST" TO WT-TX
               WHEN OTHER GO B.
       Z.  EXIT.
      ***************************************** anzeigen Fakturendatei *
       ANZFAKT SECTION.
       A.  MOVE DE-KTONR TO FA-KTONR.
           MOVE 0 TO WH-RAS.
           MOVE WH-NUM TO FA-ARNUM.
           READ FAKTDAT WITH NO LOCK INVALID PERFORM NO-REC GO Z.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0710011279000" WH-CREG.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
       B.  ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Raster " with highlight AT VDU-LP.
           ADD 41 TO VDU-P.
           MOVE AR-NUM TO WD-POS.
           DISPLAY "     " AT VDU-LP.
           ADD 1 TO VDU-P.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
           ADD 5 TO VDU-P.
           DISPLAY " " AT VDU-LP
               AR-BEZ with highlight foreground-color 6.
           SET OX TO 1.
           ADD 204 VDU-ECK GIVING VDU-LP.
       C.
      *    IF FA-MENGE(OX) = 0 AND FA-RETOUR(OX) = 0 GO E.
           SET WH-ART TO OX.
           IF OX > 31 SUBTRACT 31 FROM WH-ART
               MOVE WH-ART TO WD-TG
               DISPLAY WD-TG AT VDU-LP "* "
           ELSE MOVE WH-ART TO WD-TG
               DISPLAY WD-TG AT VDU-LP ": ".
           ADD 3 TO VDU-LP.
           MOVE FA-MENGE(OX) TO WH-ANZ.
           PERFORM MG.
           IF WH-RAS > 0 and WH-RAS <= OX MOVE 0 TO WY
               PERFORM KORREIN
               EVALUATE TRUE
               WHEN RET MOVE WH-ANZ TO FA-MENGE(OX)
                      PERFORM MG
               WHEN ESC GO X.
           IF WH-ANZ = 0 MOVE SPACE TO WD-MG.
           DISPLAY WD-MG with highlight AT VDU-LP "/".
           ADD 8 TO VDU-LP.
           MOVE 0 TO WH-ANZ WH-WERT.
           MOVE FA-RETOUR(OX) TO WH-ANZ.
           PERFORM MG.
           IF WH-RAS > 0 and WH-RAS <= OX MOVE 2 TO WY
               PERFORM KORREIN
               EVALUATE TRUE
               WHEN RET MOVE WH-ANZ TO FA-RETOUR(OX)
                     PERFORM MG
               WHEN ESC GO X.
           IF WH-ANZ = 0 MOVE SPACE TO WD-MG.
           DISPLAY WD-MG with highlight AT VDU-LP.
           ADD 8 TO VDU-P.
           IF VDU-P > 70 ADD 1 TO VDU-L
               MOVE 4 TO VDU-P.
           IF OX < 40 SET OX UP BY 1 GO C.
       G.  DISPLAY "<ret-leer>= Ende, <Ende>= speichern, Tag+<ret>= Korr
      -        "ektur <  >" AT 2301.
           DISPLAY "<Bild- >= Vorperiode" AT 2401.
           CALL "CAUP" USING "0023602002" WH-CREG.
           IF ESC GO X.
           IF BAB PERFORM VOR-RASTER.
           IF ENDE GO S.
           IF AB GO I.
           IF AUF GO L.
           IF NOT RET GO G.
           IF WH-NUM > 0 AND WH-NUM < 40
               MOVE WH-NUM TO WH-RAS GO B.
           GO X.
      *----------------------------------------------> weiterblÑttern <-
       H.  DISPLAY "** keine Daten **" with highlight BLINK AT 2401.
       I.  READ FAKTDAT NEXT AT END GO J.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF DE-KTONR = FA-KTONR GO N.
       J.  DISPLAY "** keine Daten **" with highlight BLINK AT 2401.
       L.  READ FAKTDAT PREVIOUS AT END GO H.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF DE-KTONR not = FA-KTONR GO H.
      *-----------------------------------------------> Artikel lesen <-
       N.  MOVE FA-ARNUM TO AR-NUM.
           READ ARTIKEL WITH NO LOCK INVALID
               MOVE "-- gelîscht --" TO AR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO P.
           MOVE 0 TO WH-RAS WY.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           GO B.
       S.  REWRITE FA-SATZ.
           MOVE WH-L TO VDU-L.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** anzeigen Fakturendatei *
       VOR-RASTER SECTION.
       A.  MOVE 1 TO IQ.
           CALL "CAUP" USING "271001127900011" WH-CREG.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
       B.  ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Raster " with highlight AT VDU-LP.
           ADD 8 TO VDU-LP.
           IF FA-RMON(IQ) not = 0
               MOVE FA-RMON(IQ) TO WD-RMON.
           DISPLAY WD-RMON with highlight foreground-color 4 AT VDU-LP.
           DISPLAY  " " AT 0000.
           ADD 33 TO VDU-P.
           MOVE AR-NUM TO WD-POS.
           DISPLAY "     " AT VDU-LP.
           ADD 1 TO VDU-P.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
           ADD 5 TO VDU-P.
           DISPLAY " " AT VDU-LP
               AR-BEZ with highlight foreground-color 6.
           SET OX TO 1.
           ADD 204 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WS-LIEF WS-RET.
       C.  SET WH-ART TO OX.
           IF OX > 31 SUBTRACT 31 FROM WH-ART
                MOVE WH-ART TO WD-TG
                DISPLAY WD-TG AT VDU-LP "* "
           else MOVE WH-ART TO WD-TG
                DISPLAY WD-TG AT VDU-LP ": ".
           ADD 3 TO VDU-LP.
           MOVE FA-RMENGE(IQ, OX) TO WH-ANZ.
           PERFORM MG.
           IF WH-ANZ = 0 MOVE SPACE TO WD-MG.
           DISPLAY WD-MG with highlight AT VDU-LP "/".
           ADD 8 TO VDU-LP.
           ADD WH-ANZ TO WS-LIEF.
           MOVE 0 TO WH-ANZ WH-WERT.
           MOVE FA-RRETOUR(IQ, OX) TO WH-ANZ.
           PERFORM MG.
           IF WH-ANZ = 0 MOVE SPACE TO WD-MG.
           DISPLAY WD-MG with highlight AT VDU-LP.
           ADD WH-ANZ TO WS-RET.
           ADD 8 TO VDU-P.
           IF VDU-P > 70 ADD 1 TO VDU-L
               MOVE 4 TO VDU-P.
           IF OX < 31 SET OX UP BY 1 GO C.
           ADD 1 TO VDU-L.
           MOVE 10 TO VDU-P.
           DISPLAY WS-LIEF AT VDU-LP "  " WS-RET.
       G.  DISPLAY "<ret-leer>= Ende, <Bild- >= Periode -1, <Bild- >= Pe
      -        "ride +1 < >" AT 2301.
           CALL "CAUP" USING "0023620000" WH-CREG.
           IF ESC GO X.
           IF BAUF; IF IQ > 1 ADD -1 TO IQ GO B.
           IF BAB; IF IQ < 3 and FA-RMON(IQ + 1) not = 0
                       ADD 1 TO IQ GO B.
           IF AB GO I.
           IF AUF GO L.
           IF NOT RET GO G.
      *    IF WH-NUM > 0 AND WH-NUM < 32
      *        MOVE WH-NUM TO WH-RAS GO B.
           GO X.
      *----------------------------------------------> weiterblÑttern <-
       H.  DISPLAY "** keine Daten **" with highlight BLINK AT 2401.
       I.  READ FAKTDAT NEXT AT END GO J.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF DE-KTONR = FA-KTONR GO N.
       J.  DISPLAY "** keine Daten **" with highlight BLINK AT 2401.
       L.  READ FAKTDAT PREVIOUS AT END GO H.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF DE-KTONR not = FA-KTONR GO H.
      *-----------------------------------------------> Artikel lesen <-
       N.  MOVE FA-ARNUM TO AR-NUM.
           READ ARTIKEL WITH NO LOCK INVALID
               MOVE "-- gelîscht --" TO AR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO P.
           MOVE 0 TO WH-RAS WY.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KORREIN SECTION.
       A.  DISPLAY "<ret>= Menge, <esc>= Abbruch, <Ende>= Ende" AT 2301.
           SUBTRACT VDU-ECK FROM VDU-LP.
           IF WY = 2 MULTIPLY -1 BY WH-WERT.
           EVALUATE WF-NK
               WHEN 0 CALL "CAUP" USING "1000006007" WH-CREG
               WHEN 1 CALL "CAUP" USING "1000005107" WH-CREG
               WHEN 2 CALL "CAUP" USING "1000004207" WH-CREG.
           IF ESC GO Z.
           IF ENDE MOVE 0 TO WH-RAS GO Z.
           IF NOT RET GO A.
           MOVE WH-WERT TO WH-ANZ.
           IF WY = 2 MULTIPLY WH-WERT BY -1 GIVING WH-ANZ.
           EVALUATE WF-NK
               WHEN 1 MULTIPLY 10 BY WH-ANZ
               WHEN 2 MULTIPLY 100 BY WH-ANZ.
       Z.  EXIT.
      ************************************ Anzeigen Lieferscheininhalt *
       LFSCHANZ SECTION.
       A.  MOVE 6 TO VDU-L.
           CALL "CAUP" USING "1307012480" WH-CREG.
           MOVE WM-LFNUM TO LF-NUM.
           MOVE DE-KTONR TO LF-KTONR.
           MOVE 0 TO LF-ARNUM WI.
           START LFSCHEIN KEY NOT < LF-KEY INVALID KEY GO Z.
       C.  READ LFSCHEIN NEXT NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE LF-GRM TO WF-GRM.
       D.  IF LF-KTONR NOT = DE-KTONR OR LF-NUM NOT = WM-LFNUM GO Z.
           IF WI < 13 ADD 1 TO WI GO H.
       E.  DISPLAY "<esc>= Abbruch, <tab/ret>= weiterlisten < >"
               AT 2301.
           MOVE VDU-LP TO VDU-AB.
           CALL "CAUP" USING "0023420000" WH-CREG.
           MOVE VDU-AB TO VDU-LP.
           IF ESC GO Z.
           IF NOT TABL and not RET GO E.
           MOVE 1 TO WI.
       H.  PERFORM BS-ZEIL.
           MOVE LF-ARNUM TO WD-ART.
           MOVE 1 TO VDU-P.
           DISPLAY WD-ART with highlight AT VDU-LP.
           MOVE 8 TO VDU-P.
           DISPLAY LF-BEZ with highlight AT VDU-LP.
           ADD 1 WF-MEH GIVING WH-MEH.
           MOVE 46 TO VDU-P.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           MOVE 1 TO WX.
       G.  COMPUTE VDU-P = WX * 15 + 20.
           IF WX = 3 MOVE 61 TO VDU-P.
           IF LF-MENGE(WX) NOT = 0 MOVE LF-MENGE(WX) TO WH-ANZ
               PERFORM MG
               DISPLAY WD-MENGE with highlight AT VDU-LP.
           IF WX < 3 ADD 1 TO WX GO G.
           GO C.
       Z.  EXIT.
      ********************************** Lieferscheinabfrage je Kunden *
       LFABFRAG SECTION.
       A.  MOVE 3 TO VDU-L.
       C.  START LFSCHEIN KEY > LF-KEY INVALID SET FINE TO TRUE GO F.
       D.  READ LFSCHEIN NEXT WITH NO LOCK AT END GO F.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF LF-KTONR NOT = DE-KTONR SET FINE TO TRUE GO F.
           MOVE LF-KTONR TO WP-KTONR.
           MOVE LF-NUM TO WP-NUM.
           MOVE LF-ARNUM TO WP-ARNUM.
      *    MOVE LF-KEY TO WH-LFKEY.
           MOVE LF-NUM TO WM-LFNUM.
           ADD 1 TO VDU-L.
           IF VDU-L < 21 GO H.
       F.  IF VDU-L = 3 MOVE 0 TO LF-NUM GO Z.
      *    IF VDU-L = 4 MOVE WH-LFKEY TO LF-KEY GO Z.
           IF VDU-L = 4 MOVE WP-KTONR TO LF-KTONR
                        MOVE WP-NUM TO LF-NUM
                        MOVE WP-ARNUM TO LF-ARNUM
                        GO Z.
           IF FINE DISPLAY "keine Daten mehr!" with highlight AT 2461.
           DISPLAY "<#>= naechste Lfsch.-Nr." AT 2401.
           DISPLAY "<ret>= Lfsch.Nr., <ret-leer>= Ende " AT 2301.
           MOVE 36 TO VDU-P.
           IF NOT FINE DISPLAY ", <tab>= weiterlisten " AT 2335
               MOVE 57 TO VDU-P.
       G.  CALL "CAUP" USING "0023005005" WH-CREG.
           IF TABL AND FINE GO G.
           CALL "CAUP" USING "1304102480" WH-CREG.
           IF RET; IF WH-NUM = 0 SET ESC TO TRUE.
           IF ESC OR KIST OR RET GO Z.
           IF NOT TABL GO F.
           MOVE 4 TO VDU-L.
       H.  MOVE LF-NUM TO WD-NUM.
           MOVE 10 TO VDU-P.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           MOVE LF-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE 18 TO VDU-P.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE 9999 TO LF-ARNUM.
           GO C. 
       Z.  EXIT.
      ************************************************** Kundeninkassi *
       AUFTEIL SECTION.
       A.  CALL "CAUP" USING "0703111969010" WH-CREG.
           OPEN I-O INKASSO.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Kundeninkassi " with highlight AT VDU-LP
               " max. 50 Belege " AT 0000.
           MOVE WK-D TO WH-IKEY WH-BUKEY.
       B.  READ INKASSO INVALID INITIALIZE IN-SATZ
               MOVE WH-IKEY TO IN-NUM
               WRITE IN-SATZ GO B.
           IF ZUGRIF PERFORM BESETZT GO B.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "  Kd.Nr.  Bezeichnung
      -        "    Betrag Pos " with reverse-video AT VDU-LP.
       C.  MOVE 0 TO WZ.
           PERFORM VARYING WX FROM 50 BY -1 UNTIL WX = 0
               OR IN-KTONR(WX) NOT = 0 CONTINUE.
           IF WX = 50 ADD -15 TO WX
               else IF WX > 15 ADD -14 TO WX
                    else MOVE 1 TO WX.
       D.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 1 TO IZ.
           PERFORM VARYING WE FROM WX BY 1 UNTIL IZ > 16
               OR IN-KTONR(WE) = 0
                    MOVE IN-KTONR(WE) TO DE-KTONR
                    PERFORM KUN-DIS
                    ADD 1 TO IZ.
           IF WE > 50 MOVE 50 TO WE
                      MOVE 16 TO IZ.
           IF WZ NOT = 0 MOVE WZ TO IZ
               IF IZ = 17 ADD -1 TO IZ.
       F.  DISPLAY "<Einf>= einfÅgen, <ret>= Kunden.Nr., <Entf>= lîschen
      -        ", <#>= alles lîschen" AT 2301.
           DISPLAY "<esc>= zurÅck, < >= auf, < >= ab, alpha+<ret>= suche
      -        "n, <Ende>= Ende" AT 2401.
           SET RET TO TRUE.
           MOVE 0 TO WS-BET.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 50
               OR IN-BET(WX) = 0
                   ADD IN-BET(WX) TO WS-BET
               end-perform ADD 152 VDU-ECK GIVING VDU-LP
                   MOVE WS-BET TO WD-SALDO
                   DISPLAY WD-SALDO with highlight AT VDU-LP.
           IF IZ > 16 CALL "CAUP" USING "17SRCFEN" WH-CREG
                      ADD -1 TO IZ GO F.
           IF WE = 0 MOVE 1 TO WE.
           IF IN-KTONR(WE) NOT = 0 MOVE IN-KTONR(WE) TO DE-KTONR
                PERFORM KUN-DIS
           else COMPUTE VDU-LP = IZ * 100 - 99
                CALL "CAUP" USING "1800010000" WH-CREG.
           COMPUTE VDU-LP = IZ * 100 + 202.
           COMPUTE WH-WERT = IN-KTONR(WE) / 10 - 0,2.
           MOVE 0 TO DE-KTONR WH-MOD.
           CALL "CAUP" USING "1000036010" WH-CREG.
           IF HELP CALL "CAUP" USING "190127012801" WH-CREG GO F.
       G.  IF ENTF PERFORM LOE-KUN GO D.
           IF ENDE OR ESC REWRITE IN-SATZ GO X.
           IF KIST PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 50
                       MOVE 0 TO IN-KTONR(IX)
                   end-perform GO C.
           IF BAUF PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 15
               IF WE > 1 MOVE IN-KTONR(WE) TO DE-KTONR WK-NUM
                   PERFORM KUN-DIS
                   ADD -1 TO WE IY IZ
               else MOVE 1 TO WE IY GO F.
           IF BAB PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 15
               OR IN-KTONR(WE) = 0
                   IF WE < 50 MOVE IN-KTONR(WE) TO DE-KTONR WK-NUM
                       PERFORM KUN-DIS
                       ADD 1 TO WE IY IZ
                   else MOVE 50 TO WE IY GO F.
           IF AUF; IF WE > 1 ADD -1 TO WE IY;
               IF IZ = 1 CALL "CAUP" USING "22SCRAB" WH-CREG
               else ADD -1 TO IZ
               end-if GO F.
           IF AB; IF IN-KTONR(WE) NOT = 0 GO L.
           IF NOT RET AND NOT EINF GO F.
           IF WE = 50 AND IN-KTONR(50) NOT = 0
               ADD 1928 VDU-ECK GIVING VDU-LP
               DISPLAY " nur 50 mîglich " with BLINK AT VDU-LP
               GO F.
           IF EINF; IF WH-NUM = 0 AND WH-MCODE = SPACE GO F
                    else PERFORM EINF-KUN.
           MOVE WH-WERT TO WH-NUM WK-NUM.
           IF IN-KTONR(WE) NOT = 0
               COMPUTE WH-WERT = IN-KTONR(WE) / 10 - 0,2
               IF WH-WERT NOT = WH-NUM GO F.
           IF WH-MCODE = SPACE MOVE WK-NUM TO WH-MCODE.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PANANZ" USING "10DEBSUCH" WH-CREG
               CANCEL "PANANZ".
           IF WH-NUM = 0;
               IF WH-MOD = 1 SET ENTF TO TRUE GO G else GO F.
           MOVE 1 TO DE-FNR.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           PERFORM KUN-DIS.
           IF ESC GO F.
       I.  DISPLAY "<esc>= Abbruch, <ret>= Betrag" AT 2301.
           COMPUTE VDU-LP = IZ * 100 + 252.
           MOVE IN-BET(WE) TO WH-WERT.
           CALL "CAUP" USING "1000525208" WH-CREG.
           IF NOT RET GO I.
           MOVE WH-WERT TO IN-BET(WE) WD-SALDO.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           MOVE DE-KTONR TO IN-KTONR(WE).
       L.  IF WE < 50 ADD 1 TO WE IY IZ.
           IF WH-MOD = 1 MOVE IZ TO WZ
               COMPUTE WX = WE - IZ + 1 GO D.
           GO F.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE INKASSO.
       Z.  EXIT.
      ******************************************************************
       KUN-DIS SECTION.
       A.  IF IZ > 16 CALL "CAUP" USING "17SRCFEN" WH-CREG
                      ADD -1 TO IZ.
           IF IZ <  1 CALL "CAUP" USING "22SCRAB" WH-CREG
                      ADD 1 TO IZ.
           COMPUTE VDU-LP = IZ * 100 + 203 + VDU-ECK.
           IF DE-KTONR = 0 SET ESC TO TRUE GO Z.
           MOVE 1 TO DE-FNR.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       E.  READ DEBITOR INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT PERFORM WEITER GO E.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           ADD 9 TO VDU-P.
           DISPLAY WT-TX with SIZE 40 AT VDU-LP.
           ADD 40 TO VDU-P.
           IF IN-BET(WE) NOT = 0 MOVE IN-BET(WE) TO WD-SALDO
               DISPLAY WD-SALDO with highlight AT VDU-LP.
           MOVE WE TO WD-TG.
           ADD 13 TO VDU-LP.
           DISPLAY WD-TG with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************** lîschen *
       LOE-KUN SECTION.
       A.  MOVE 0 TO IN-KTONR(WE).
           MOVE WE TO WZ.
           PERFORM VARYING WE FROM WE BY 1 UNTIL WE > 49
               MOVE IN-KTONR(WE + 1) TO IN-KTONR(WE)
               MOVE IN-BET(WE + 1) TO IN-BET(WE).
           MOVE 0 TO IN-KTONR(WE) IN-BET(WE).
           MOVE WZ TO WE.
           MOVE IZ TO WZ.
           COMPUTE WX = WE - IZ + 1.
       Z.  EXIT.
      ******************************************************************
       EINF-KUN SECTION.
       A.  IF WH-NUM = 0 AND WH-MCODE NOT = SPACE MOVE 0 TO WH-WERT.
           IF IN-KTONR(WE) = 0 OR WH-WERT = IN-KTONR(WE) GO X.
           MOVE 1 TO WH-MOD.
           IF IN-KTONR(WE) NOT = 0
               DIVIDE 10 INTO IN-KTONR(WE) GIVING WD-KTO
               DISPLAY WD-KTO AT VDU-LP
           else DISPLAY "       " AT VDU-LP.
           CALL "CAUP" USING "220000SCRAB" WH-CREG.
           PERFORM VARYING IX FROM 30 BY -1 UNTIL IX < WE
               MOVE IN-KTONR(IX) TO IN-KTONR(IX + 1).
           MOVE 0 TO IN-KTONR(WE).
       X.  SET RET TO TRUE.
           MOVE WH-NUM TO WH-WERT IN-KTONR(WE).
       Z.  EXIT.
      ************************************* falls ORDERS v. SPAR kommt *
       HOLDATEN SECTION.
       A.  IF WM-VER not = 3 GO Z.                              *> Lohr
           CALL "CAUP" USING "0707050670000" WH-CREG.
           MOVE WM-DATUM TO WV-DATUM.
           ADD 103 VDU-ECK GIVING VDU-LP.
           IF WH-PG = 13
               DISPLAY " Hofer-Listung " with highlight AT VDU-LP GO C
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
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "fÅr Lieferung am: " AT VDU-LP.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WM-DATUM).
           ADD 1 TO WA-DAT.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT).
           MOVE WV-DATUM TO WZ-DATUM.
           ADD 0321 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
               WR-TN(WO-TGN + 1) with highlight foreground-color 3.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
       E.  DISPLAY "<ret>= Datum, <esc>= Abbruch, <ret-leer>= wiederhole
      -        "n" AT 2301.
           CALL "CAUP" USING "1103216006" WH-CREG.
           IF ESC GO Z.
           IF WX-DATUM = 0 GO E.
           MOVE WX-DATUM TO WV-DATUM LF-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
               WR-TN(WO-TGN + 1) with highlight foreground-color 3.
           IF WV-DATUM not > WM-DATUM;
               DISPLAY "Datum muss grîsser sein!" AT 2401
               PERFORM WEITER GO C.
           IF WV-DATUM > WE-MAXDAT DISPLAY "Datum zu gro·" AT 2401
               PERFORM WEITER GO C.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF not RET GO G.
           IF WH-PG = 13 GO X.
           PERFORM INIT-HOFART.
           OPEN INPUT MODEMDAT.
           OPEN I-O HOFERART.
           IF WF-STATUS = "35" GO W.
       I.  READ MODEMDAT AT END GO Q.
           EVALUATE MD-SA
               WHEN 16 IF MD-LFDAT not = WV-DATUM GO W
                       else GO I
               WHEN 00 PERFORM LF-KOPF GO I
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
           WRITE LF-SATZ INVALID REWRITE LF-SATZ.
           MOVE DE-KTONR TO HA-KTONR.
       O.  READ HOFERART INVALID MOVE 0 TO HA-DATUM.
           IF ZUGRIF PERFORM BESETZT GO O.
           IF HA-DATUM not = WV-DATUM INITIALIZE HA-SATZ
               MOVE DE-KTONR TO HA-KTONR
               PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
                   MOVE WT-ARNUM(WX) TO HA-ARNUM(WX).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
                   or HA-ARNUM(WX) = FA-ARNUM CONTINUE.
           ADD LF-MENGE(1) TO HA-MENGE(WX).
           REWRITE HA-SATZ INVALID WRITE HA-SATZ.
           GO I.
       Q.  CLOSE MODEMDAT.
           CLOSE HOFERART.
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
           MOVE WV-DATUM TO LF-DATUM WZ-DATUM.
           COMPUTE LF-NUM = WZ-TAG * 100.
           MOVE 1 TO DE-FNR.
       C.  READ DEBITOR IGNORE LOCK INVALID
              DISPLAY "Kunde nicht angelegt!" with highlight at 2401
              PERFORM WEITER
              GO C.
           MOVE DE-TOUR TO LF-TOUR.
           MOVE DE-SUB TO LF-SUB.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO
           DISPLAY WD-KTO with highlight AT VDU-LP " " DE-BEZ(1:50).
       Z.  EXIT.
      ******************************************************************
       INIT-HOFART SECTION.
       A.  DISPLAY " Bitte warten " with highlight foreground-color 5
               AT 2333.
           MOVE LOW-VALUE TO FA-SATZ WT-HART.
           MOVE 20000 TO FA-KTONR.
           MOVE 1 TO FA-KUARNUM.
           START FAKTDAT KEY > FA-BKEY INVALID STOP RUN.
       C.  READ FAKTDAT NEXT IGNORE LOCK AT END GO Z.
           IF FA-KUARNUM = 0 GO C.
           IF FA-KTONR > 29992 GO Z.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               or FA-ARNUM = WT-ARNUM(WX) or WT-ARNUM(WX) = 0
               CONTINUE.
           MOVE FA-ARNUM TO WT-ARNUM(WX).
           GO C.
       Z.  EXIT.
