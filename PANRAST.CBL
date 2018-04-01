      $SET LINKCOUNT"384" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANRAST.
      ******************************************************************
      *         copyright (c) 1986 by Christa ROBNIK, Graz             *
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEART.CPY.
           COPY PANSEKON.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEDAU.CPY.
      *    SELECT BUCHFAKT   ASSIGN TO "PANISFIB.DAT"
      *                      ORGANIZATION INDEXED, ACCESS DYNAMIC
      *                      RECORD KEY BF-KEY
      *                      ALTERNATE KEY BF-RKEY DUPLICATES
      *                      FILE STATUS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
       COPY PANLIEF.CPY.
      *COPY PANEBUCH.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(242).
       01  DRQ-SATZ.
           03  DRQ-NO                  PIC X(8).
           03  DRQ-NUM                 PIC ZZ.ZZZ,9-.
           03  DRQ-NAME                PIC X(139).
       01  DRR-SATZ.
           03  FILLER                  PIC X(8).
           03  DRR-STR.
               05 DRR-ARNUM            PIC ZZ9-.   
               05 DRR-BEZ              PIC X(26).
               05 DRR-MG                          OCCURS 31 INDEXED DY.
                  07 DRR-MA            PIC ZZZZ9-.
                  07 DRR-MB REDEFINES
                     DRR-MA            PIC ZZZ,9-.
                  07 DRR-R REDEFINES DRR-MA.
                     09 DRR-TG         PIC ZZZ9.
                     09 DRR-P          PIC XX.
               05 DRR-SU               PIC X(7).
       01  DRY-SATZ.
           03  DRY-X                   PIC X   OCCURS 242  INDEXED XX.
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
           03  WH-PX                   PIC XX         OCCURS 2.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99      COMP-X.
           03  WX-PRSTAT               PIC 99      COMP-X.
           03  WZ-SEITE                PIC 99      COMP.
           03  WZ-SCHALT               PIC 99      COMP.
           03  WZ-ZEILEN               PIC 99      COMP.
           03  WZ-SIZE                 PIC 99      COMP.
           03  WI                      PIC 9(6)    COMP.
           03  WS                      PIC 99      COMP.
           03  WK-DS.
               05 FILLER               PIC X(5)       VALUE "per:".
               05 WK-DAT               PIC X(10).
               05 FILLER               PIC X(7)       VALUE "Seite: ".
               05 WK-SEITE             PIC ZZ9-.
           03  WX-DS REDEFINES WK-DS   PIC X     OCCURS 26 INDEXED DX.
           03  WH-VON                  PIC 9999.
           03  WK-VON                  PIC 9999      COMP.
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WH-MOD                  PIC 99        COMP.
               88  MANUEL   VALUE 0.    88  SOFORT    VALUE 6.
               88  MON      VALUE 1 2.  88  HALB      VALUE 5.
               88  WOCH     VALUE 3 4.  88  RETOUR    VALUE 9.
           03  WX-VER                  PIC 9.
           03  WX-RMON                 PIC 9999.
      *    03  WX-VON                  PIC 9999.
           03  WK-D                    PIC 9999      COMP.
           03  WX-M                    PIC 99        COMP.
           03  WX-J                    PIC 99        COMP.
           03  WV-ARNUM                PIC 9(4)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-ULT                  PIC 99        COMP.
           03  WH-ABDAT                PIC 9(8)      COMP.
           03  WH-BISDAT               PIC 9(8)      COMP.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WA-ANZ                  PIC 9(5)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(5).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9999V9.
           03  WH-ANZ2                 PIC S999V99.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WD-TG                   PIC ZZ9.
           03  WT-MENGE                PIC S9(7)      COMP-3.
           03  WT-RETM                 PIC S9(7)      COMP-3.
           03  WH-PZ                   PIC S9999V99   COMP-3.
           03  WD-PZ                   PIC ZZZ9,99-.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WD-X                    PIC X.
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
      *DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON BUCHFAKT.
      *A.  CALL "CADECL" USING "FIBUEBER.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON DAUER.
       A.  CALL "CADECL" USING "PANEBEST.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE WH-KEY TO WK-D.
      *    MOVE 1 TO WH-KEY.
      *    READ KONSTANT IGNORE LOCK INVALID KEY STOP RUN.
      *    MOVE KO-MERK TO WK-M.
           CLOSE DAUER LFSCHEIN.
           MOVE WE-DRU(3) TO WM-DRU.
           IF WL-CA = 10 PERFORM RASTWAHL GO X.
           GO Z.
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
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *********************************************** Listladeroutine *
       DASEIT SECTION.
       A.  MOVE VDU-DATUM TO WK-DAT.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
       B.  MOVE WX-DS(DX) TO DRY-X(XX).
           IF DX < 26 SET DX XX UP BY 1 GO B.
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
           IF WM-DRU = 1 MOVE DRA-SATZ(3:) TO DRA-SATZ(1:).    *> Laser
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU = 0 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
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
           IF WF-STATUS = 99 GO D.
           IF WM-DRU = 1 GO X.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
       X.  MOVE SPACE TO DRA-SATZ.
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
       RASTWAHL SECTION.
       A.  MOVE " Rasterdruck" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM PERPRUEF.
           IF ESC GO T.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Kunden-Nr.:" AT 0301.
           UNLOCK DEBITOR.
           UNLOCK ARTIKEL.
           MOVE 0 TO DE-KTONR FA-KTONR FA-ARNUM WV-ARNUM.
           DISPLAY "<esc>= Ende, alpha+<ret>= Kunden suchen, <ret>= Kund
      -        "en-Nr. " AT 2301.
           IF WH-PG > 0 DISPLAY ", <ret-leer>= alles " AT 2359.
           DISPLAY "<#> statt <ret>= alle Raster drucken" AT 2401.
           CALL "CAUP" USING "0003135010" WH-CREG.
           IF ESC GO T.
           move 0 to ws.
           if kist set ret to true
                   move 1 to ws.
           IF NOT RET GO B.
       C.  IF MANUEL; IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY "Kunden-Nr. muss sein" with BEEP AT 2401
               PERFORM WEITER GO B.
           IF WH-NUM = 0; IF WH-MCODE = SPACE 
               DISPLAY " alles " AT 0270 GO D
           ELSE CALL "PANANZ" USING "10DEB-SUCH" WH-CREG
               IF DE-KTONR = 0 GO B ELSE GO C.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           ADD WH-NUM 0,2 GIVING WD-KTO.
           MOVE 1 TO DE-FNR.
           IF DE-KTONR < 3 GO B.
           READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO B.
           IF DE-KTONR = 0 GO B.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE DE-BEZ TO WT-BEZ WH-X.
           DISPLAY "Kunden-Nr.:" AT 0301.
           DISPLAY WD-KTO with highlight AT 0313.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with SIZE 78 AT 0401.
           DIVIDE 100 INTO WK-MON GIVING WH-VON.
           MOVE WH-VON TO WK-VON.
           IF MANUEL GO N.
       D.  MOVE DE-KTONR TO FA-KTONR WV-KTONR.
           DIVIDE 100 INTO WK-MON GIVING WH-VON.
           MOVE WH-VON TO WK-VON.
           MOVE WK-VON TO FA-MON.
           MOVE WH-MOD TO FA-ART.
           IF WH-MOD = 9 MOVE DE-FAKART TO FA-ART.
           START FAKTDAT KEY NOT < FA-AKEY INVALID PERFORM NO-REC GO P.
       F.  READ FAKTDAT NEXT IGNORE LOCK AT END GO P.
           IF WV-KTONR NOT = 0; IF FA-KTONR NOT = WV-KTONR GO P.
           IF FA-MON > WK-VON GO P.
           IF WH-MOD not = 9 and FA-ART not = WH-MOD GO P.

           IF WX-VER = 1;
              EVALUATE TRUE
                  WHEN WX-RMON = FA-RMON(3) MOVE FA-RMR(3) TO FA-MR
                  WHEN WX-RMON = FA-RMON(2) MOVE FA-RMR(2) TO FA-MR
                  WHEN WX-RMON = FA-RMON(1) MOVE FA-RMR(1) TO FA-MR
                  WHEN OTHER GO P.

           IF FA-KTONR = DE-KTONR GO H.
           MOVE 1 TO DE-FNR.
           MOVE FA-KTONR TO DE-KTONR.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0313.
       G.  READ DEBITOR IGNORE LOCK INVALID DISPLAY WD-KTO AT 2301
               DISPLAY "Kunde gel”scht" with highlight AT 2410
                   " - bitte anlegen"
               PERFORM WEITER GO W.
       H.  MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with SIZE 78 AT 0401.
           if ws = 1 and de-rast = 0 move 1 to de-rast.
           IF WH-PG = 5; IF DE-RAST = 0 ADD 10 TO FA-KTONR
               MOVE 0 TO FA-ARNUM GO K.
           MOVE WH-ABDAT TO WZ-DATUM.
           MOVE 1 TO WX.
           MOVE 0 TO WM-OPEN WZ-ZEILEN WZ-SCHALT.
           SET OX TO WZ-TAG.
           IF SOFORT MOVE WM-DATUM TO WH-ABDAT WH-BISDAT WZ-DATUM
               SET OX TO WZ-TAG
               IF WM-DATUM > WK-MON SET OX UP BY 31.
      *--------------------> WI: Tageanzahl, WV-ULT: Monatsende <-
       I.  IF OX > WV-ULT AND OX < 32 SET OX UP BY 1 GO I.
           IF FA-MENGE(OX) NOT = 0 OR FA-RETOUR(OX) NOT = 0 GO J.
           IF WX < WI ADD 1 TO WX
               SET OX UP BY 1 GO I.
           GO F.
       J.  IF WH-PG = 5 PERFORM RASTER.
       K.  IF WV-KTONR > 2 GO P.
           START FAKTDAT KEY NOT < FA-AKEY INVALID
               MOVE 9999 TO WV-KTONR GO K.
           GO F.
       P.  PERFORM END-DRU.
           MOVE 0 TO WZ-SCHALT.
      *    CLOSE BUCHFAKT.
           GO B.
      *------------------------------------------> Ende Fakturierung <-
       T.  CALL "CAUP" USING "06KOPF" WH-CREG.
       W.
      *    CLOSE BUCHFAKT.
           OPEN I-O DAUER LFSCHEIN.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************** Pruefung der FAKT-Periode mit LIST-Periode *
       PERPRUEF SECTION.
       A.  MOVE 0 TO WX-TASTE.
      *    OPEN I-O BUCHFAKT.
       C.  MOVE 0 TO WH-ABDAT WH-BISDAT.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "<esc>= Abbruch, < >= Hilfe, <ret>= Fakt.Periode"
               AT 2301.
           DISPLAY "Raster f." AT 0301.
           DISPLAY "1 - Monatsrechnungen    " AT 0810
                   "2 - Monatsrechnungen    " AT 0010
                   "3 - Wochenrechnungen    " AT 0010
                   "4 - Wochenrechnungen    " AT 0010
                   "5 - Halbmonatsrechnungen" AT 0010
                   "6 - Halbmonatsretouren" with highlight AT 0010.
           DISPLAY "bitte w„hlen Sie:  " AT 1530.
           CALL "CAUP" USING "0015481001" WH-CREG.
           IF HELP CALL "CAUP" USING "190023002401" WH-CREG GO C.
           MOVE WH-NUM TO WH-MOD.
           IF ESC GO Z.
           IF EINF PERFORM PERIODE.
           IF NOT RET GO C.
           EVALUATE WH-MOD
               WHEN 1 MOVE " monatl. Rechnung 1" TO WK-GEB
               WHEN 2 MOVE " monatl. Rechnung 2" TO WK-GEB
               WHEN 3 MOVE " w”chentl. Rechnung 3" TO WK-GEB
               WHEN 4 MOVE " w”chentl. Rechnung 4" TO WK-GEB
               WHEN 5 MOVE " halbmonatl. Rechng." TO WK-GEB
               WHEN 6 MOVE "    Retourenraster" TO WK-GEB
                      MOVE 9 TO WH-MOD
               WHEN OTHER GO C.
           DISPLAY WK-GEB AT 0324.
           MOVE 0 TO WX-VER.                     *> Merker Perioden„ndg.
       D.  DISPLAY "Rechnungsdatum:" AT 0350.
           DISPLAY "<esc>= Abbruch, <ret-leer>= Tagesdatum, " AT 2301
               "Datum + <Einfg>= Perioden„nderung" with highlight
                   foreground-color 6.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0366.
           MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0103666006" WH-CREG.
           IF ESC GO Z.
           IF EINF MOVE 1 TO WX-VER
               SET RET TO TRUE.
           IF WX-DATUM = 0 GO D.
           DISPLAY VDU-DATUM with highlight AT 0366.
           MOVE WX-DATUM TO WM-DATUM.
           MOVE WK-MON TO WZ-DATUM WH-BISDAT.
           IF WX-VER = 1 MOVE WM-DATUM TO WZ-DATUM WH-BISDAT
               COMPUTE WX-RMON = WM-DATUM / 100.
           MOVE 1 TO WZ-TAG.
           MOVE WZ-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           MOVE WX-DATUM TO WH-ABDAT.
       E.  DISPLAY "Lieferdatum von:          bis:" AT 0410.
           MOVE WH-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM AT 0427.
           MOVE WH-ABDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0104276006" WH-CREG.
           IF WOLI GO C.
           IF ESC GO Z.
           IF WX-DATUM = 0 GO E.
           DISPLAY VDU-DATUM with highlight AT 0427.
           IF WX-DATUM < WH-ABDAT DISPLAY "Datumsfehler" AT 2401
               PERFORM WEITER GO E.
           MOVE WX-DATUM TO WH-ABDAT.
           MOVE WT-KON(WZ-MONAT) TO WI WV-ULT.
           if wz-monat = 2 compute wh-num = wh-abdat / 10000
               divide 4 into wh-num giving wh-num remainder wi
               if wi not = 0 move 28 to wi wv-ult
                        else move 29 to wi wv-ult.
       F.  MOVE WH-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0441.
           MOVE WH-BISDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0104416006" WH-CREG.
           IF WOLI GO D.
           IF ESC GO Z.
           IF WX-DATUM = 0 GO F.
           IF WX-DATUM < WH-ABDAT DISPLAY "Datum zu klein" AT 2401
               PERFORM WEITER GO F. 
           DISPLAY VDU-DATUM with highlight AT 0441.
           IF MON OR HALB; IF WX-DATUM > WH-BISDAT
               DISPLAY "Datum zu gross" AT 2401
               PERFORM WEITER GO F.
           MOVE WX-DATUM TO WH-BISDAT.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WH-BISDAT).
           COMPUTE WA-DAT = WA-DAT - FUNCTION INTEGER-OF-DATE(WH-ABDAT).
           ADD 1 TO WA-DAT GIVING WI WD-TG.
           IF MON GO K.
           DISPLAY WD-TG with highlight AT 0375.
           IF WOCH; IF WI > 7;
               IF WI > 10 DISPLAY "Datumsfehler" AT 2401
                   PERFORM WEITER GO F ELSE
               DISPLAY "ACHTUNG " with highlight AT 2401
                   "zuviele Tage" AT 2400 PERFORM WEITER.
           IF RETOUR and WI > 16
               DISPLAY "ACHTUNG " with highlight AT 2401
                   "zuviele Tage" AT 2400
               PERFORM WEITER
               GO F.
       K.  DISPLAY "<ret>= Start, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO K.
       Z.  EXIT.
      ****************************************** Eingabe Rasterperiode *
       PERIODE SECTION.
       A.
      *    IF WH-NUM not = 0 GO G.

       Z.  EXIT.
      ********************************************* autom. Rasterdruck *
       RASTER SECTION.
       A.  MOVE 0 TO WZ-SEITE.
           MOVE 25 TO VDU-P.
           SET WY TO XX.
       B.  MOVE 0 TO WT-MENGE WT-RETM.
           MOVE 7 TO VDU-L.
           CALL "CAUP" USING "1307010880000" WH-CREG.
           MOVE 0 TO WH-ANZ WK-ANZ.
           MOVE FA-ARNUM TO AR-NUM.
       I.  READ ARTIKEL IGNORE LOCK INVALID
                 MOVE "Artikel fehlt" TO AR-BEZ.
           MOVE WH-ABDAT TO WZ-DATUM.
           MOVE AR-GRM TO WF-GRM.
           SET DY TO 1.
           SET OX TO WZ-TAG.
       K.  IF OX > WV-ULT AND OX < 32 SET OX UP BY 1 GO K.
           IF FA-MENGE(OX) = 0 and FA-RETOUR(OX) = 0 GO L.
           PERFORM RASTKOPF.
           IF WK-ANZ = 0 MOVE AR-BEZ TO DRR-BEZ
               MOVE AR-NUM TO DRR-ARNUM.
           IF WF-NK = 0 MOVE FA-MENGE(OX) TO DRR-MA(DY).
           IF WF-NK = 1 DIVIDE 10 INTO FA-MENGE(OX) GIVING DRR-MB(DY).
           IF WF-NK = 2 DIVIDE 100 INTO FA-MENGE(OX) GIVING DRR-MB(DY).
           ADD FA-MENGE(OX) TO WT-MENGE.
           MOVE 1 TO WK-ANZ.
       L.  IF DY < WI SET DY UP BY 1
               SET OX UP BY 1 GO K.
           MOVE WT-MENGE TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MG TO WV-MCODE.
           SET MX TO 1.
           SET XX TO WY.
       M.  MOVE WV-MC(MX) TO DRY-X(XX + 2).
           IF MX < 7 SET MX XX UP BY 1 GO M.
           MOVE DRR-STR TO WT-TX.
           DISPLAY WT-TX with SIZE 74 AT 2005.
           IF WK-ANZ > 0; IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:)
                          end-if
                          PERFORM DRUCK.
           SET DY TO 1.
           SET OX TO WZ-TAG.
       N.  IF OX > WV-ULT AND OX < 32 SET OX UP BY 1 GO N.
           IF FA-RETOUR(OX) = 0 GO O.
           IF WZ-ZEILEN > (WZ-SIZE - 2) PERFORM RASTKOPF.
           IF WK-ANZ = 0 MOVE AR-BEZ TO DRR-BEZ      *> nur Retourmenge
               MOVE AR-NUM TO DRR-ARNUM
               IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:)
               end-if
               PERFORM DRUCK
               ADD 1 TO WK-ANZ.
           IF WF-NK = 0 MOVE FA-RETOUR(OX) TO DRR-MA(DY).
           IF WF-NK = 1 DIVIDE 10 INTO FA-RETOUR(OX) GIVING DRR-MB(DY).
           IF WF-NK = 2 DIVIDE 100 INTO FA-RETOUR(OX) GIVING DRR-MB(DY).
           ADD FA-RETOUR(OX) TO WT-RETM.
           ADD 2 TO WK-ANZ.
       O.  IF DY < WI SET DY UP BY 1
               SET OX UP BY 1 GO N.
           IF WK-ANZ = 0 GO S.
           IF WK-ANZ = 1 GO Q.
           MOVE WT-RETM TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MG TO WV-MCODE.
           MOVE "      Retourmenge" TO DRR-BEZ.
           SET XX TO WY.
           SET MX TO 1.
       P.  MOVE WV-MC(MX) TO DRY-X(XX + 2).
           IF MX < 7 SET MX XX UP BY 1 GO P.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           IF WH-MOD = 9 SET WX TO XX
               COMPUTE WT-RETM = WT-RETM * -1
               COMPUTE WH-PZ WD-PZ rounded = WT-RETM / WT-MENGE * 100
               ADD 3 TO WX
               MOVE "=" TO DRY-SATZ(WX:2)
               ADD 1 TO WX
               MOVE WD-PZ TO DRY-SATZ(WX:7)
               ADD 6 TO WX
               MOVE "%" TO DRY-SATZ(WX:1)
               IF WH-PZ > 15,00 ADD 3 TO WX
                  MOVE "zuviel um:" TO DRY-SATZ(WX:)
                  ADD 10 TO WX
                  COMPUTE WD-PZ = WH-PZ - 15,00
                  MOVE WD-PZ TO DRY-SATZ(WX:7)
                  ADD 7 TO WX
                  MOVE "%" TO DRY-SATZ(WX:1).
           PERFORM DRUCK.
       Q.  MOVE WT-ADR TO DRA-SATZ.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
       S.  READ FAKTDAT NEXT IGNORE LOCK AT END
                MOVE 99999 TO WV-KTONR GO X.
           IF ZUGRIF PERFORM BESETZT GO S.
           IF FA-KTONR NOT = DE-KTONR GO X.
           SET OX TO 1.

           IF WX-VER = 1;
              EVALUATE TRUE
                  WHEN WX-RMON = FA-RMON(3) MOVE FA-RMR(3) TO FA-MR
                  WHEN WX-RMON = FA-RMON(2) MOVE FA-RMR(2) TO FA-MR
                  WHEN WX-RMON = FA-RMON(1) MOVE FA-RMR(1) TO FA-MR
                  WHEN OTHER GO P.

           GO B.
       X.  MOVE SPACE TO DRA-SATZ.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************************************* Kopf der Rasterliste *
       RASTKOPF SECTION.
       A.  IF WZ-ZEILEN > WZ-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           IF WM-OPEN = 1 GO C.
           MOVE 40 TO WZ-SIZE.
           MOVE X"0008" TO WH-PX(2).
              IF MON OR HALB MOVE X"0500" TO WH-PX(1)
                        else MOVE X"0100" TO WH-PX(1).
           IF WM-KO = 1; IF WOCH AND HALB MOVE X"000C" TO WH-PX(2).
           IF WM-DRU = 1 MOVE "RASTER.LST" TO WH-DRUNAM
               IF MON or HALB MOVE 9 TO WH-P
                         else MOVE 5 TO WH-P
                              MOVE 60 TO WZ-SIZE.
           IF WM-DRU = 1 and WH-MOD = 9 MOVE 10 TO WH-P
               MOVE "&l26a4h6d1O(s0p15.5h1b0s6T" TO WE-STG(10).
           PERFORM BEG-DRU.
       C.  DIVIDE 10 INTO DE-KTONR GIVING DRQ-NUM.
           MOVE DE-BEZ TO DRQ-NAME.
           INSPECT DRQ-NAME REPLACING ALL "#" BY ",".
           MOVE 0 TO WZ-SCHALT.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE "Art Bezeichnung" TO DRR-STR.
           MOVE 1 TO WX.
           MOVE WH-ABDAT TO WZ-DATUM.
       E.  MOVE WZ-TAG TO DRR-TG(WX).
           MOVE "." TO DRR-P(WX).
           ADD 1 TO WZ-TAG.
           IF WZ-TAG > WV-ULT MOVE 1 TO WZ-TAG.
           IF WX < WI ADD 1 TO WX GO E.
           COMPUTE WY = WX * 6 + 38.
           ADD 3 TO WY.
           MOVE "Summe#" TO DRY-SATZ(WY:).
           MOVE DRA-SATZ TO WT-ADR.
           MOVE SPACE TO DRA-SATZ.
           MOVE "Lieferzeitraum vom:" TO DRA-SATZ(19:20).
           MOVE WH-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-SATZ(39:10).
           MOVE "bis:" TO DRA-SATZ(49:5).
           MOVE WH-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-SATZ(54:8).
           COMPUTE WY = WX * 6 + 38.
           SET XX TO WY.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX DOWN BY 21.
           IF XX < 65 SET XX TO 65.
           SET DX TO 6 PERFORM DASEIT.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           COMPUTE WY = WX * 6 + 38.
           SET XX TO WY.
           MOVE WT-ADR TO DRA-SATZ.
           INSPECT DRA-SATZ REPLACING CHARACTERS BY "Ä" BEFORE "#".
           MOVE "Ä" TO DRY-X(XX + 8).
           MOVE SPACE TO DRQ-NO.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE WT-ADR TO DRA-SATZ.
           MOVE SPACE TO DRY-X(XX + 8).
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           IF WH-MOD = 9 SET WZ TO XX
               ADD 11 TO WZ
               MOVE "Z I E L = 15% maximal"TO DRY-SATZ(WZ:).
           PERFORM DRUCK.
           MOVE WT-ADR TO DRA-SATZ.
           INSPECT DRA-SATZ REPLACING CHARACTERS BY "Ä" BEFORE "#".
           MOVE "Ä" TO DRY-X(XX + 8).
           MOVE SPACE TO DRQ-NO.
           MOVE DRA-SATZ TO WT-ADR.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE WH-ABDAT TO WZ-DATUM.
       Z.  EXIT.
