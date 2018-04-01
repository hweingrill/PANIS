      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PARDRU.
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  PC.
       OBJECT-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEART.CPY.
           COPY PARSEART.CPY.
           COPY PANSEKON.CPY.
           COPY PARSEREZ.CPY.
           SELECT MODULE     ASSIGN TO "PARTEXT.DAT",
                             ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                             RECORD KEY IS MO-KEY,
                             FILE STATUS IS WF-STATUS.
           SELECT SUMMEN     ASSIGN TO "PANESUMM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SU-KEY
                             ALTERNATE RECORD KEY SU-AKEY
                             FILE STATUS IS WF-STATUS.
           SELECT PRODSUMME  ASSIGN TO "PANPRODU.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY PS-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT KOMPOSUM   ASSIGN TO "PANKOSUM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY KS-NUM
                             FILE STATUS IS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
       COPY PANFDART.CPY.
       COPY PARFDART.CPY.
       COPY PARREZEP.CPY.
      *********************************************** Karteibewegungen *
       FD  MODULE      external        RECORD IS VARYING IN SIZE
                                       FROM 2 TO 602 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  MO-SATZ.
           03  MO-KEY.
               05 MO-NUM               PIC 9(4)      COMP.
           03  MO-TXT.
               05 MO-ZL                PIC X(40)     OCCURS 15.
      ********************************* akkumulierte Komponentensummen *
       FD  KOMPOSUM                    LABEL RECORD STANDARD.
       01  KS-SATZ.
           03  KS-NUM                  PIC 9(4)     COMP.
           03  KS-SUM                  PIC 9(9)V99  COMP.
      *********************************** monatliche Produktionssummen *
       FD  PRODSUMME                   LABEL RECORD STANDARD.
       01  PS-SATZ.
           03  PS-KEY.
               05 PS-PER               PIC 9(4)     COMP.
               05 PS-ARNUM             PIC 9(4)     COMP.
           03  PS-MENGE                PIC 9(7)V99  COMP OCCURS 31.
      *********************************** Touren- u. Backzettelsummen *
       FD  SUMMEN       external       LABEL RECORD STANDARD
                                       RECORD 128 CHARACTERS.
       01  SU-SATZ.
           03  SU-AKEY.
               05 SU-GRP               PIC 9999    COMP.
               05 SU-ANUM              PIC 9(6)    COMP.
           03  SU-KEY.
               05 SU-TB                PIC 99      COMP.
               05 SU-NUM               PIC 9(6)    COMP.
           03  SU-BEZ                  PIC X(25).
           03  SU-MEH                  PIC 99      COMP.
           03  SU-NK                   PIC 99      COMP.
           03  SU-GVZ                  PIC 99      COMP.
           03  SU-MG                   PIC S9(7)   COMP-3 OCCURS 23.
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  DRA-NO                  PIC XX.
           03  DRA-TEXT.
               05 DRA-KTONR            PIC ZZ.ZZ9,9.
               05 DRA-ARNUM            PIC ZZZZ.ZZ9-.
               05 DRA-ART              PIC ZZZ9-.
               05 DRA-MON              PIC ZZZ9/99-.
               05 FILLER               PIC X(210).
       01  DRB-SATZ.
           03  FILLER                  PIC XX.
           03  DRB-STR.
               05 DRB-NUM              PIC Z.ZZ9-.
               05 DRB-BEZ              PIC X(26).
               05 DRB-PREIS            PIC ZZ.ZZ9,99-   OCCURS 3.
               05 DRB-PROZ             PIC ZZZZ9,99-.
       01  DRH-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRH-STR.
               05 DRH-ARNUM            PIC Z.ZZ9-.
               05 DRH-ARBEZ            PIC X(27).
               05 DRH-MEH              PIC XXX.
               05 DRH-NK               PIC Z9-.
               05 DRH-ART              PIC 9-.
               05 DRH-PR               PIC X(10)          OCCURS 2.
       01  DRM-SATZ.
           03  FILLER                  PIC XX.
           03  DRM-STR.
               05 DRM-KONUM            PIC ZZ.ZZ9-.
               05 DRM-KOBEZ            PIC X(27).
               05 DRM-T                PIC X.
               05 DRM-PROZ             PIC ZZ.ZZ9,99-.
               05 DRM-MENGE REDEFINES
                  DRM-PROZ             PIC Z.ZZZ.ZZ9-.
               05 DRM-MEH              PIC XXXX.
               05 FILLER               PIC X(15).
       01  DRN-SATZ.
           03  FILLER                  PIC X(4).
           03  DRN-STR.
               05 DRN-NUM              PIC Z.ZZ9-.
               05 DRN-BEZ              PIC X(27).
               05 DRN-TX               PIC X(8).
               05 DRN-TEIG             PIC ZZ.ZZ9-.
               05 DRN-X                PIC X.
               05 DRN-STK              PIC X(10).
       01  DRR-SATZ.
           03  FILLER                  PIC X(6).
           03  DRR-STR.
               05 DRR-POS              PIC Z9-.
               05 DRR-KONUM            PIC ZZ.ZZ9-.
               05 DRR-KOBEZ            PIC X(23).
               05 DRR-A                PIC Z.
               05 DRR-T                PIC Z.
               05 DRR-PROZ             PIC ZZ.ZZ9,99-.
               05 DRR-MENGE REDEFINES
                  DRR-PROZ             PIC Z.ZZZ.ZZ9-.
               05 DRR-MEH              PIC XXXX.
               05 DRR-PREIS            PIC ZZ.ZZ9,99-.
               05 DRR-BET              PIC ZZZ.ZZ9,99-.
       01  DRS-SATZ.
           03  FILLER                  PIC X(6).
           03  DRS-STR.
               05 DRS-NUM              PIC Z.ZZ9-.
               05 DRS-BEZ              PIC X(30).
               05 DRS-TX               PIC X(8).
               05 DRS-TEIG             PIC ZZ.ZZ9-.
               05 DRS-X                PIC X.
               05 DRS-STK              PIC ZZ.ZZ9-.
       01  DRT-SATZ.
           03  FILLER                  PIC X(6).
           03  DRT-STR.
               05 DRT-NUM              PIC Z.ZZ9-.
               05 FILLER               PIC X.
               05 DRT-BEZ              PIC X(28).
               05 DRT-TEIG             PIC ZZ.ZZ9-.
               05 DRT-STK              PIC ZZ.ZZ9-.
               05 DRT-PREIS            PIC ZZZ.ZZ9,99-.
       01  DRX-SATZ.
           03  DRX-X                   PIC 99   COMP   OCCURS 242.
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
       01  WH-KART      external       PIC 9999      COMP.
       COPY "WHCREG.CPY".
       01  WH-REG.
           03  WH-PX                   PIC XX      OCCURS 2.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99     COMP-X.
           03  WX-PRSTAT               PIC 99     COMP-X.
           03  WH-DRUNAM               PIC X(12)  VALUE "LPT1:".
           03  WZ-SEITE                PIC 99     COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99     COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99     COMP-3 VALUE ZERO.
           03  WV-ARNUM                PIC 9(4)      COMP.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WA-ANZ                  PIC 9(5)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(5).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9999V9.
           03  WH-ANZ2                 PIC S999V99.
           03  WK-NUM                  PIC 9(8)V99   COMP.
           03  WH-MENGE                PIC S9(5)     COMP-3.
           03  WC-MENGE                PIC S9(7)     COMP.
           03  WK-S                    PIC 9         COMP.
           03  WH-MOD                  PIC 9         COMP.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WK-D                    PIC 9999      COMP.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WD-ART                  PIC ZZZ9.
           03  WT-ZEIT                 PIC 9(6)V9999  COMP  OCCURS 5.
           03  WM-BS                   PIC 99.
           03  WK-PROZ                 PIC S999V99    COMP-3 OCCURS 8.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WK-DS.
               05 WK-PX                PIC X(5)       VALUE "per:".
               05 WK-DAT               PIC X(10).
               05 WK-SEITX             PIC X(7)       VALUE "Seite: ".
               05 WK-SEITE             PIC ZZ9-.
           03  WX-DS REDEFINES WK-DS   PIC X     OCCURS 26 INDEXED DX.
           03  WH-STK                  PIC 9(7)       COMP-3.
           03  WH-EHP                  PIC S9(7)V99   COMP-3.
           03  WH-ENVP                 PIC S9(5)V9999 COMP-3.
           03  WH-GHP                  PIC S9(7)V99   COMP-3.
           03  WH-GNVP                 PIC S9(5)V9999 COMP-3.
           03  WH-FAKT                 PIC S9(3)V9999 COMP-3.
           03  WH-MIN                  PIC 9(7)V99    COMP-3.
           03  WH-HERSTEL              PIC 9(7)V99    COMP-3.
           03  WD-TEIG                 PIC ZZ.ZZ9-.
           03  WD-RPM                  PIC ZZZ.ZZ9-.
           03  WD-KZ                   PIC 9.
           03  WD-PROZ                 PIC Z9,9.
           03  WD-TG                   PIC ZZ9.
           03  WD-BET                  PIC ZZZ.ZZ9,99-.
           03  WD-PREIS                PIC Z.ZZ9,99-.
           03  WD-APREIS               PIC Z.ZZ9,999-.
           03  WH-PREIS                PIC 9(5)V999   COMP-3.
           03  WR-PR REDEFINES WH-PREIS.
               05 FILLER               PIC X(4).
               05 WR-GR                PIC 9           COMP-3.
           03  WS-MENGE                PIC S9(9)       COMP-3.
           03  WH-BET                  PIC S9(9)V99    COMP-3.
           03  WS-BET                  PIC S9(9)V99    COMP-3.
           03  WH-ATG                  PIC S9(7)V9(4)  COMP-3.
           03  WH-MEH                  PIC 99          COMP.
           03  WI-M                    PIC 99          COMP.
           03  WM-LP                   PIC 9999        COMP.
           03  WT-KONUM                PIC 9(4)        COMP OCCURS 20.
           03  WT-KOSTK                PIC 9(4)        COMP OCCURS 20.
           03  WH-TNAM                 PIC 9(6)             VALUE ZERO.
           03  RED REDEFINES WH-TNAM.
               05 FILLER               PIC 9.
               05 WH-T                 PIC 9.
               05 WH-N                 PIC 9.
               05 WH-A                 PIC 9.
               05 WH-M                 PIC 99.
           03  WH-PARM                 PIC X(45).
           03  WH-TART                 PIC 99               VALUE ZERO.
           03  REDA REDEFINES WH-TART.
               05 WH-BK                PIC 9.
               05 WH-RART              PIC 9.
           03  WD-SUM                  PIC Z.ZZZ.ZZ9-.
           03  WD-MIN                  PIC ZZ9,99-.
           03  WD-SATZ                 PIC Z9,99-.
           03  WD-PZ                   PIC Z9,99.
           03  WD-POS                  PIC ZZ9.
           03  WH-MZUS                 PIC 9(3)V99   COMP-3.
           03  WH-DUMIN                PIC 9(5)V99   COMP-3.
           03  WV-ABDAT                PIC 9(8)      COMP.
           03  WV-BISDAT               PIC 9(8)      COMP.
           03  WT-AB                   PIC 99        COMP.
           03  WT-BIS                  PIC 99        COMP.
      *------------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
           03  WD-X                    PIC X.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON SUMMEN.
       A.  CALL "CADECL" USING "PANESUMM.DAT" WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON PRODSUMME.
       A.  CALL "CADECL" USING "PANPRODU.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON KOMPOSUM.
       A.  CALL "CADECL" USING "PANKOSUM.DAT" WH-CREG.
       DECL-M SECTION.         USE AFTER ERROR PROCEDURE ON MODULE.
       A.  CALL "CADECL" USING "PARTEXT.DAT "  WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           CLOSE ARTIKEL.
           OPEN I-O ARTIKEL.
           IF WL-CA = 10 PERFORM MASSEN GO X.
           IF WL-CA = 20 PERFORM RP-LI  GO Y.
           PERFORM LIWAHL.
           EVALUATE WH-PG
               WHEN 0 GO X
               WHEN 1
               WHEN 2
               WHEN 3 PERFORM RPUPDATE
               WHEN 4 PERFORM RP-LI
               WHEN 5 PERFORM RPKULI
               WHEN 6 PERFORM KO-LI
               WHEN 7 PERFORM GESMAS
               WHEN 8 PERFORM KOMPSUCH.
       X.  MOVE WH-CREG TO WL-CREG.
       Y.  CLOSE ARTIKEL.
           OPEN INPUT ARTIKEL.
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
           OPEN OUTPUT DRUCKER.
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
      *****************************************************************
       DISP-MENGE SECTION.
       A.  MOVE WH-ANZ TO WA-ANZ.
           IF WF-NK = 0 GO C.
           IF WF-NK = 1 GO B.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ2 ON SIZE ERROR GO X.
           MOVE WH-ANZ2 TO WD-ANZ2 WD-MGC.
           MOVE WD-ANZ2 TO WD-MENGE.
           MOVE WD-MGC  TO WD-MG.
           GO E.
       B.  IF WR-A2 NOT = 0 GO X.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ1 ON SIZE ERROR GO X.
           MOVE WH-ANZ1 TO WD-ANZ1 WD-MGB.
           MOVE WD-ANZ1 TO WD-MENGE.
           MOVE WD-MGB  TO WD-MG.
           GO E.
       C.  IF WH-ANZ > 999999 OR WH-ANZ < -999999 GO X.
           IF WR-A2 NOT = 0 AND WR-A1 NOT = 0 GO X.
           MOVE WH-ANZ TO WD-ANZ WD-MGA.
           MOVE WD-ANZ TO WD-MENGE.
           MOVE WD-MGA TO WD-MG.
       E.  DISPLAY WD-MG WITH HIGHLIGHT AT VDU-LP.
           GO Z.
       X.  DISPLAY "Menge zu gross" AT 2401
               PERFORM WEITER
               MOVE 99 TO WX-TASTE.
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
      *********************************************** Listladeroutine *
       DASEIT SECTION.
       A.  MOVE VDU-DATUM TO WK-DAT.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
       B.  MOVE WX-DS(DX) TO DRY-X(XX).
           IF DX < 26 SET DX XX UP BY 1 GO B.
       Z.  EXIT.
      ********************************************** Mengenermittlung *
       MASSEN SECTION.
       A.  MOVE "  Mengenermittlung" TO WK-GEB.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
           OPEN INPUT SUMMEN.
           IF WF-STATUS > "10" PERFORM NO-REC GO Z.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
       C.  DISPLAY "Rpnr:          " AT 0301.
           DISPLAY "<ret>= Rezepturennr., <ret+alpha> = suchen, <esc>= E
      -        "nde" AT 2301.
           CALL "CAUP" USING "0003074009" WH-CREG.
           IF ESC OR WOLI GO Y.
           IF TABL SET RET TO TRUE.
           IF NOT RET GO C.
           IF WH-NUM = 0 CALL "PANANZ" USING "40RPSUCH" WH-CREG
               IF RE-NUM = 0 GO B
               ELSE GO D.
           MOVE WH-NUM TO RE-NUM WV-ARNUM.
           IF RE-NUM = 0 GO C.
       D.  MOVE RE-NUM TO WD-NUM WV-ARNUM.
           MOVE 0 TO WM-MERKER.
           MOVE 7 TO VDU-L.
       E.  READ REZEPT IGNORE LOCK INVALID PERFORM NO-REC GO B.
           MOVE RE-ART TO WH-TART.
           PERFORM RPTKO.
       F.  MOVE RE-NUM TO SU-NUM AR-NUM.
           READ SUMMEN IGNORE LOCK INVALID MOVE 0 TO SU-MG(1) GO H.
           MOVE 2 TO WX.
       G.  ADD SU-MG(WX) TO SU-MG(1).
           IF WX < 24 ADD 1 TO WX GO G.
       H.  READ ARTIKEL IGNORE LOCK INVALID PERFORM NO-REC GO B.
           MOVE AR-GRM TO WF-GRM.
           MOVE SU-MG(1) TO WH-ANZ WH-WERT.
           PERFORM MG.
           ADD 1 WF-MEH GIVING WH-MEH.
           DISPLAY WT-MEH(WH-MEH) with highlight AT 0736.
       K.  DISPLAY "Bedarf: " AT 0720.
           DISPLAY WD-MENGE with highlight AT 0728.
           DISPLAY "<ret>= erforderl. Menge, <esc>= Abbruch" AT 2301.
           IF WF-NK = 0 CALL "CAUP" USING "0007286007" WH-CREG.
           IF WF-NK = 1 CALL "CAUP" USING "0007285107" WH-CREG.
           IF WF-NK = 2 CALL "CAUP" USING "0007284207" WH-CREG.
           IF ESC GO B.
           IF NOT RET GO K.
           MOVE 0 TO WK-S.
           IF NOT RET GO K.
           MOVE WH-WERT TO WH-ANZ.
           IF WH-ANZ = 0 GO K.
           MOVE 28 TO VDU-P.
           PERFORM DISP-MENGE.
           IF ESC GO K.
      *---- 0 ----> Menge = WH-ANZ * RE-TEIL * RE-MENGE(XR) / RE-GEW <-
      *---- 1 ----> Menge = WH-ANZ / RE-TEIL <-------------------------
           IF WH-RART = 1 COMPUTE WH-ATG ROUNDED = WH-ANZ / RE-TEIL
              else COMPUTE WH-ATG ROUNDED = (WH-ANZ * RE-TEIL) / RE-GEW.
           MOVE 0 TO WH-WERT WS-MENGE.
           PERFORM RP-MEKO.
           MOVE RE-NUM TO DRN-NUM
           MOVE RE-BEZ TO DRN-BEZ.
           MOVE "Teiler:" TO DRN-TX.
           MOVE RE-TEIL TO DRN-TEIG.
           MOVE "/" TO DRN-X.
           MOVE WD-MENGE TO DRN-STK.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRN-STR PERFORM DRUCK.
           SET XR TO 1.
       L.  IF RE-KONUM(XR) = 0 OR RE-MENGE(XR) = 0 GO P.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           IF WH-A > 0 GO P.
           COMPUTE WH-STK ROUNDED = WH-ATG * RE-MENGE(XR).
           IF WH-T < 5 ADD WH-STK TO WH-WERT.
           IF WK-S = 0 MOVE WH-T TO WK-S
               MOVE 0 TO WS-MENGE.
           IF WK-S = WH-T GO O.
       M.  IF WK-S = 0 GO N.
           IF WK-S = 1 MOVE "** BrÅhstÅck ****" TO DRM-KOBEZ.
           IF WK-S = 2 MOVE "** QuellstÅck ****" TO DRM-KOBEZ.
           IF WK-S = 3 MOVE "** Vorteig ****" TO DRM-KOBEZ.
           IF WK-S = 4 MOVE "** FÅllung ****" TO DRM-KOBEZ.
           IF WK-S = 5 MOVE "** Zutaten o.Gew. **" TO DRM-KOBEZ.
           IF WK-S = 6 MOVE "** Zeiten **" TO DRM-KOBEZ.
           MOVE WS-MENGE TO DRM-MENGE.
           MOVE "gr" TO DRM-MEH.
           MOVE WK-S TO DRM-T.
           PERFORM DRUCK.
           MOVE WH-T TO WK-S.
           MOVE 0 TO WS-MENGE.
       N.  IF XR = 20 GO Q.
       O.  IF WH-T > 0 AND WH-T < 5 ADD WH-STK TO WS-MENGE.
           IF WZ-ZEILEN > 61 PERFORM RP-KOPF.
           MOVE RE-KONUM(XR) TO DRM-KONUM.
           MOVE RE-KOBEZ(XR) TO DRM-KOBEZ.
           MOVE WH-STK TO DRM-MENGE.
           MOVE "gr" TO DRM-MEH
           MOVE RE-TNAM(XR) TO WH-TNAM.
           IF WH-T NOT = 0 MOVE WH-T TO DRM-T.
           PERFORM DRUCK.
       P.  IF XR < 20 SET XR UP BY 1 GO L.
           IF WK-S NOT = 0 GO M.
       Q.  MOVE WH-WERT TO DRM-MENGE.
           MOVE "gr" TO DRM-MEH
           PERFORM DRUCK.
           IF WZ-ZEILEN > 33 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN GO R.
           SUBTRACT WZ-ZEILEN FROM 38 GIVING WZ-SCHALT.
           PERFORM DRUCK.
       R.  IF WV-ARNUM NOT = 0 GO B.
           GO G.
       Y.  IF WZ-ZEILEN NOT = 0 PERFORM END-DRU.
           CLOSE SUMMEN.
       Z.  EXIT.
      *****************************************************************
       PRODDAT SECTION.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "vom:           bis inkl.:" AT 0510.
           DISPLAY "<ret>= Datum, <esc>=Ende" AT 2301.
           MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0105156006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO B.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WZ-DATUM = 0 OR > WH-DATUM GO B.
           MOVE WX-DATUM TO WV-ABDAT.
           MOVE WZ-DATUM(1:4) TO PS-PER WK-D.
           MOVE WZ-TAG TO WT-AB.
           MOVE 0 TO PS-ARNUM.
           START PRODSUMME KEY NOT < PS-KEY INVALID
               SET ESC TO TRUE GO Z.
       D.  READ PRODSUMME NEXT AT END PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF PS-PER NOT = WZ-DATUM(1:4) PERFORM NO-REC GO B.
       E.  DISPLAY "<ret>= Datum, <esc>=Ende, <>= zurÅck" AT 2301.
           MOVE WV-ABDAT TO WC-DATUM WH-WERT.
           CALL "CAUP" USING "0105366006" WH-CREG.
           IF WOLI GO B.
           IF ESC GO Z.
           IF NOT RET GO E.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WZ-DATUM TO WV-BISDAT.
           MOVE WZ-TAG TO WT-BIS.
           COMPUTE PS-PER = WV-BISDAT / 100.
           IF WZ-DATUM = 0 OR > WH-DATUM OR WZ-DATUM < WV-ABDAT
               OR WK-D NOT = PS-PER GO E.
           MOVE 0 TO PS-ARNUM.
           START PRODSUMME KEY NOT < PS-KEY.
       Z.  EXIT.
      ********************************************** Mengenermittlung *
       GESMAS SECTION.
       A.  MOVE " Gesamtmengenrechnung " TO WK-GEB.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
           OPEN INPUT PRODSUMME.
           IF WF-STATUS = 35 GO Z.
           OPEN OUTPUT KOMPOSUM.
           CLOSE KOMPOSUM.
           OPEN I-O KOMPOSUM.
           PERFORM PRODDAT.
           IF ESC GO Y.
       C.  READ PRODSUMME NEXT AT END GO T.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 0 TO WH-WERT.
           PERFORM VARYING WX FROM WT-AB BY 1 UNTIL WX > WT-BIS
               ADD PS-MENGE(WX) TO WH-WERT.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 20
               MOVE 0 TO WT-KONUM(WY) WT-KOSTK(WY).
           MOVE PS-ARNUM TO RE-NUM WV-ARNUM WD-NUM.
       E.  READ REZEPT IGNORE LOCK INVALID GO C.
           MOVE RE-ART TO WH-TART.
       F.  MOVE RE-NUM TO AR-NUM.
       H.  READ ARTIKEL IGNORE LOCK INVALID GO C.
           MOVE AR-GRM TO WF-GRM.
           MOVE WH-WERT TO WH-ANZ.
           PERFORM MG.
      *---- 0 ----> Menge = WH-ANZ * RE-TEIL * RE-MENGE(XR) / RE-GEW <-
      *---- 1 ----> Menge = WH-ANZ / RE-TEIL <-------------------------
           IF WH-RART = 1 COMPUTE WH-ATG ROUNDED = WH-ANZ / RE-TEIL
              else COMPUTE WH-ATG ROUNDED = (WH-ANZ * RE-TEIL) / RE-GEW.
           MOVE 0 TO WH-WERT WS-MENGE WY.
           SET XR TO 1.
       L.  IF RE-KONUM(XR) = 0 OR RE-MENGE(XR) = 0 GO P.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           IF WH-A > 0 GO P.
           COMPUTE WH-STK ROUNDED = WH-ATG * RE-MENGE(XR).
           IF WH-T > 0;
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 20
                   OR WT-KONUM(WY) = RE-KONUM(XR)
                   OR WT-KONUM(WY) = 0
               END-PERFORM
               MOVE RE-KONUM(XR) TO WT-KONUM(WY)
               ADD WH-STK TO WT-KOSTK(WY) GO N.
      *    IF WH-T < 5 ADD WH-STK TO WH-WERT.
           IF WK-S = 0 MOVE WH-T TO WK-S
               MOVE 0 TO WS-MENGE.
           IF WK-S = WH-T GO O.
       M.  IF WK-S = 0 GO N.
           IF WK-S = 1 MOVE "** BrÅhstÅck ****" TO DRM-KOBEZ.
           IF WK-S = 2 MOVE "** QuellstÅck ****" TO DRM-KOBEZ.
           IF WK-S = 3 MOVE "** Vorteig ****" TO DRM-KOBEZ.
           IF WK-S = 4 MOVE "** FÅllung ****" TO DRM-KOBEZ.
           IF WK-S = 5 MOVE "** Zutaten o.Gew. **" TO DRM-KOBEZ.
           IF WK-S > 5 MOVE "** Zeiten **" TO DRM-KOBEZ.
           MOVE WS-MENGE TO DRM-MENGE.
           MOVE WH-T TO WK-S.
           MOVE 0 TO WS-MENGE.
       N.  IF XR = 20 GO Q.
       O.  IF WH-T > 0 AND WH-T < 5 ADD WH-STK TO WS-MENGE.
           MOVE RE-KONUM(XR) TO KS-NUM.
           DISPLAY KS-NUM AT 2020.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           IF WH-T NOT = 0 MOVE WH-T TO DRM-T.
           IF WH-T > 0 GO P.
       O1. READ KOMPOSUM INVALID INITIALIZE KS-SATZ
               MOVE RE-KONUM(XR) TO KS-NUM.
           ADD WH-STK TO KS-SUM.
           REWRITE KS-SATZ INVALID WRITE KS-SATZ.
       P.  IF XR < 20 SET XR UP BY 1 GO L.
           IF WK-S NOT = 0 GO M.
           IF WY = 0 GO R.
           MOVE WT-KONUM(1) TO RE-NUM.
           MOVE WT-KOSTK(1) TO WH-WERT.
           MOVE 0 TO WT-KONUM(1) WT-KOSTK(1).
           PERFORM VARYING WY FROM 2 BY 1 UNTIL WY > 20
               MOVE WT-KONUM(WY - 1) TO WT-KONUM(WY)
               MOVE WT-KOSTK(WY - 1) TO WT-KOSTK(WY).
           GO E.
       Q.
       R.  IF WV-ARNUM NOT = 0 GO C.
      *    GO G.
       T.  PERFORM SUMDRU.
       Y.  IF WZ-ZEILEN NOT = 0 PERFORM END-DRU.
           CLOSE PRODSUMME.
       Z.  EXIT.
      *****************************************************************
       SUMDRU SECTION.
       A.  CLOSE KOMPOSUM.
           OPEN INPUT KOMPOSUM.
           INITIALIZE KS-SATZ.
       C.  READ KOMPOSUM NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM RP-MEKO.
           MOVE KS-NUM TO DRN-NUM LG-NUM.
       E.  READ LAGARTIK INVALID MOVE " -- fehlt -- " TO LG-BEZ.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE LG-BEZ TO DRN-BEZ.
           MOVE KS-SUM TO DRM-MENGE.
           MOVE "gr" TO DRM-MEH.
           DISPLAY DRA-SATZ(1:50) AT 1910.
           PERFORM DRUCK.
           GO C.
       W.  MOVE ALL "ƒ" TO DRA-SATZ(1:40).
           PERFORM DRUCK.
           CLOSE KOMPOSUM.
       Z.  EXIT.
      *****************************************************************
       RPTKO SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Rpnr:" AT 0301.
           MOVE RE-NUM TO WD-ART.
           DISPLAY WD-ART with highlight AT 0307.
           DISPLAY RE-BEZ with highlight AT 0312.
           DISPLAY "Teiler:" AT 0339.
           MOVE RE-TEIL TO WD-TEIG.
           DISPLAY WD-TEIG with highlight AT 0347.
           IF WH-RART = 0 DISPLAY "gr  =" AT 0300
                     else DISPLAY "Stk =" AT 0300.
           IF RE-GEW NOT = 0 COMPUTE WK-NUM = RE-GEW / RE-TEIL
               ADD 0,03 WK-NUM GIVING WH-MENGE
               MOVE WH-MENGE TO WD-MGA
               DISPLAY WD-MGA with highlight AT 0360
               IF WH-RART = 0 DISPLAY "Stk" AT 0300
                         else DISPLAY "gr " AT 0300.
           DISPLAY "Pos Komp.  Bezeichnung            Menge Meh     Prei
      -        "s     Betrag" AT 0401.
           DISPLAY ALL "ƒ" with SIZE 67 AT 0501.
       Z.  EXIT.
      *****************************************************************
       RP-MEKO SECTION.
       A.  IF WZ-ZEILEN > 60 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0000" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE WE-DRU(5) TO WM-DRU.
           IF WM-DRU = 1 MOVE 4 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Mengenermittlung fÅr:" TO DRM-STR.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX TO 41.
           SET DX TO 1 PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRM-STR.
           PERFORM DRUCK.
           MOVE " Nr.  B e z e i c h n u n g            Menge Meh"
               TO DRM-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRM-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ********************************************** Auswahl Listungen *
       LIWAHL SECTION.
       A.  MOVE "  Listungen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
       B.  DISPLAY "<ret>= Programmwahl, <ret-leer>= Ende" AT 2301
               "1 - Update Rezepturen  "  AT 0724
               "2 - Update Artikel/Komp." AT 0024
               "3 - Update Rezepturen und Artikel" AT 0024
               "4 - Rezepturenlistung  "  AT 0024
               "5 - Kurzliste Rezepturen" AT 0024
               "6 - Komponentenlistung "  AT 0024
               "7 - Komponentenauflîsung" AT 0024
               "8 - Komponentensuche"     AT 0024
               "leer = Programmende"      AT 0024
               "bitte wÑhlen Sie: "       AT 0039
           CALL "CAUP" USING "0016572002" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 1 MOVE "Update Rezepturen"     TO WK-GEB
               WHEN 2 MOVE "Update Artikel/Komp."  TO WK-GEB
               WHEN 3 MOVE "Update Rezpt./Artikel" TO WK-GEB
               WHEN 4 MOVE "Rezepturenliste"       TO WK-GEB
               WHEN 5 MOVE "Kurzliste Rezepturen"  TO WK-GEB
               WHEN 6 MOVE "Komponentenliste"      TO WK-GEB
               WHEN 8 MOVE "Komponentensuche"      TO WK-GEB.
       Z.  EXIT.
      ********************************************* Update Rezepturen *
       RPUPDATE SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE 6 TO WH-KEY.
       B.  READ KONSTANT IGNORE LOCK INVALID GO Z.
           IF WH-PG = 1 OR WH-PG = 3 DISPLAY "Alle Rezepturen werden mit
      -        " den Preisen" AT 0521
               "der Komponentenstammdaten neu durchgerechnet." AT 0621.
           IF WH-PG = 2 OR WH-PG = 3 DISPLAY "Alle Artikelstammdaten wer
      -        "den mit neuen" AT 0721
      -        "Einstandspreis aus der Kalkulation versehen." AT 0821.
           DISPLAY "<esc>= Abbruch, <ret>= Start ohne Druck, <tab>= Star
      -        "t mit Druck" AT 2301.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
           CALL "CAUP" USING "0023650000" WH-CREG.
           MOVE WX-TASTE TO WS-TASTE.
           IF ESC GO Z.
           IF NOT RETA GO A.
           MOVE 0 TO RE-NUM.
           START REZEPT KEY NOT < RE-KEY INVALID PERFORM NO-REC GO Z.
       C.  READ REZEPT NEXT AT END GO Y.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE RE-NUM TO WD-TEIG.
           MOVE RE-ART TO WH-TART.
           IF WH-BK = 0 PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
                            MOVE KO-PROZ(WZ) TO WK-PROZ(WZ)
                   else PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
                            MOVE KO-PROZ(WZ + 8) TO WK-PROZ(WZ).
           DISPLAY WD-TEIG AT 1010 RE-BEZ with highlight.
           IF WH-PG = 2 GO L.
           MOVE RE-PREIS TO WH-PREIS.
           MOVE 0 TO RE-GEW RE-BET RE-MAT.
           SET XR TO 1.
       E.  MOVE RE-TNAM(XR) TO WH-TNAM.
           IF RE-KONUM(XR) = 0 OR RE-MENGE(XR) = 0 GO K.
           MOVE RE-KONUM(XR) TO LG-NUM.
       F.  READ LAGARTIK INVALID MOVE LG-BEZ TO WT-TX
               MOVE "*" TO WR-TX(19) WR-TX(20)
               MOVE WT-TX TO LG-BEZ GO G.
           IF ZUGRIF PERFORM BESETZT GO F.
           MOVE LG-MEH TO WF-MEH WH-M.
           MOVE LG-NK TO WF-NK WH-N.
           MOVE LG-ART TO WH-A.
           MOVE WH-TNAM TO RE-TNAM(XR).
       G.  MOVE LG-BEZ TO RE-KOBEZ(XR).
           MOVE LG-NUM TO WD-TEIG.
           DISPLAY WD-TEIG AT 1210 LG-BEZ with highlight.
           MOVE LG-PREIS TO RE-EKP(XR).
       I.  IF WH-A = 0
              COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-MENGE(XR) / 1000
              ADD WH-BET TO RE-MAT
              IF WH-T < 5 MOVE RE-MENGE(XR) TO RE-NGEW(XR);
                 IF LG-NETPZ not = 0
                     COMPUTE RE-NGEW(XR) = RE-NGEW(XR) * LG-NETPZ / 100
                 end-if
                 ADD RE-NGEW(XR) TO RE-GEW.
           IF WH-A = 1
                COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-PROZ(XR).
           IF WH-A = 2
                COMPUTE WH-BET ROUNDED = RE-MAT * RE-PROZ(XR) / 100.
           ADD WH-BET TO RE-BET.
       K.  IF XR < 20 SET XR UP BY 1 GO E.
           IF WH-RART = 1 MOVE RE-TEIL TO WH-STK
                else COMPUTE WH-STK = RE-GEW / RE-TEIL.
           COMPUTE RE-PREIS ROUNDED = RE-BET / WH-STK.
           COMPUTE WK-NUM = RE-GEW / RE-TEIL.
           ADD 0,03 WK-NUM GIVING WH-MENGE.
           PERFORM ERMITTELN.
           IF RE-NUM < 1000 MOVE WH-GHP TO RE-SKP       *> Selbstkosten
           else MOVE WH-HERSTEL TO RE-SKP.            *> Herstellkosten
           REWRITE RE-SATZ.
           IF WH-PG = 1 GO P.
       L.  PERFORM ERMITTELN.
           MOVE RE-NUM TO AR-NUM LG-NUM.
           IF RE-NUM > 999 GO O.
      *---------------------------------------> Verkaufsartikelupdate <-
       M.  READ ARTIKEL INVALID KEY GO C.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE AR-GRM TO WF-GRM.
           IF AR-NUM < 1000 MOVE AR-EKP TO WH-PREIS
               MOVE WH-GHP TO AR-EKP                    *> Selbstkosten
           ELSE MOVE AR-PREIS TO WH-PREIS
               MOVE WH-HERSTEL TO AR-PREIS.           *> Herstellkosten
           MOVE AR-NUM TO WD-TEIG.
           REWRITE AR-SATZ.
           GO P.
      *----------------------------------------> Update Halbfabrikate <-
       O.  READ LAGARTIK INVALID KEY GO C.
           IF ZUGRIF PERFORM BESETZT GO O.
           MOVE WH-HERSTEL TO LG-PREIS WH-PREIS.      *> Herstellkosten
           MOVE WH-GHP TO LG-EKP.                     *> Selbstkosten
           MOVE LG-NUM TO WD-TEIG.
970912*    MOVE 10 TO LG-ART.
           IF LG-ART not = 10 UNLOCK LAGARTIK GO P.   *> nut Halbfabr.
           REWRITE LG-SATZ.
       P.  PERFORM REZART.
           GO C.
       Y.  IF WZ-ZEILEN NOT = 0 PERFORM END-DRU.
           UNLOCK ARTIKEL.
           UNLOCK REZEPT.
       Z.  EXIT.
      ******************************************************************
       ERMITTELN SECTION.
       A.  SET XR TO 1.
           MOVE 0 TO RE-BET RE-GEW RE-MAT WH-BET WH-MIN WH-MZUS
                     WT-ZEIT(1) WT-ZEIT(2) WT-ZEIT(3) WT-ZEIT(4)
                     WT-ZEIT(5) RE-SKP.
       B.  IF RE-MENGE(XR) = 0 GO E.
           MOVE RE-TNAM(XR) TO WH-TNAM.
      *----------------> WH-A: 0 = Material, 1 = Zeit, 2 = Aufschlag <-
           IF WH-A = 2
               COMPUTE WH-BET ROUNDED = RE-MAT * RE-PROZ(XR) / 100.
           IF WH-A = 0
             COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-MENGE(XR) / 1000
             ADD WH-BET TO RE-MAT
             IF WH-T < 5 ADD RE-MENGE(XR) TO RE-GEW.
           IF WH-A = 1
               COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-PROZ(XR)
               evaluate wh-t
                  when 5
                  when 6 ADD RE-PROZ(XR) TO WH-MIN
                         ADD RE-PROZ(XR) TO WT-ZEIT(WH-T - 4).
           IF WH-A = 3 MOVE 0 TO WH-BET
               MOVE RE-PROZ(XR) TO WH-MZUS.
           ADD WH-BET TO RE-BET.
       E.  IF XR < 20 SET XR UP BY 1 GO B.
           IF RE-BET = 0 AND RE-GEW = 0 GO Z.
           IF WH-RART = 1 MOVE RE-TEIL TO WH-STK
                ELSE COMPUTE WH-STK = RE-GEW / RE-TEIL.
           COMPUTE RE-PREIS ROUNDED = RE-BET / WH-STK.
      *--------------------------------------------------> Kalkulation *
           MOVE 0 TO WH-BET WS-BET.
      *---------------------------------------------> Materialeinsatz <-
           MOVE RE-PREIS TO WS-BET.
      *--------------------------------------------------> Lohnkosten <-
           IF WH-MZUS NOT = 0 DIVIDE 100 INTO WH-MZUS GIVING WH-FAKT
               ADD 1 TO WH-FAKT
               COMPUTE WH-MIN = WH-MIN * WH-FAKT
               COMPUTE RE-ZEIT(1) = WT-ZEIT(1) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(2) = WT-ZEIT(2) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(3) = WT-ZEIT(3) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(4) = WT-ZEIT(4) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(5) = WT-ZEIT(5) * WH-FAKT / WH-STK.
           DIVIDE WH-STK INTO WH-MIN GIVING WH-DUMIN ROUNDED.
           IF WH-BK = 0 MOVE KO-FAKT(1) TO WH-WERT
                   ELSE MOVE KO-FAKT(2) TO WH-WERT.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-MIN / WH-STK.
           ADD WH-BET TO WS-BET.
      *--------------------------------------> Produktionsgmeinkosten <-
           IF WH-BK = 0 MOVE KO-FAKT(3) TO WH-WERT
                   ELSE MOVE KO-FAKT(4) TO WH-WERT.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-MIN / WH-STK.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WS-BET.
      *----------------------------------------------> Herstellkosten <-
           MOVE WS-BET TO WH-HERSTEL.
      *---------------------------------> Retourwarenanteil GHP / EHP <-
           MOVE WK-PROZ(1) TO WH-WERT.
           COMPUTE WH-BET = WS-BET * WH-WERT / 100.
           ADD WS-BET WH-BET GIVING WH-GHP.
           MOVE WK-PROZ(2) TO WH-WERT.
           COMPUTE WH-BET = WS-BET * WH-WERT / 100.
           ADD WS-BET WH-BET GIVING WH-EHP.
      *-----------------------------------> Vertriebskosten GHP / EHP <-
           ADD WK-PROZ(3) WK-PROZ(5) WK-PROZ(7) GIVING WH-BET.
           COMPUTE WH-GNVP = WH-GHP / (100 - WH-BET).
           ADD WK-PROZ(4) WK-PROZ(6) WK-PROZ(8) GIVING WH-BET.
           COMPUTE WH-ENVP = WH-EHP / (100 - WH-BET).
           MOVE WK-PROZ(3) TO WH-WERT.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP.
           ADD WH-BET TO WH-GHP.
           MOVE WK-PROZ(4) TO WH-WERT.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP.
           ADD WH-BET TO WH-EHP.
      *-------------------------------------------> Verwaltungskosten <-
           MOVE WK-PROZ(5) TO WD-PZ WH-WERT.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP.
           ADD WH-BET TO WH-GHP.                         *> Selbstkosten
           MOVE WK-PROZ(6) TO WH-WERT.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP.
           ADD WH-BET TO WH-EHP.
      *------------------------------------------------> Selbstkosten <-
           IF AR-NUM > 999 MOVE WH-HERSTEL TO WD-PREIS.
           MOVE WH-GHP TO WD-PREIS.
           DISPLAY "Selbstkosten: " AT 1045 WD-PREIS with highlight.
       Z.  EXIT.
      *************************** Rezepturen- ArtikelverÑnderungsliste *
       REZART SECTION.
       A.  IF WS-TASTE = 0 GO Z.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO C.
           MOVE x"0000" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE WE-DRU(5) TO WM-DRU.
           IF WM-DRU = 1 MOVE 4 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           IF WH-PG = 1 MOVE "RezepturenverÑnderungsliste" TO DRB-STR
               ELSE MOVE "ArtikelverÑnderungsliste" TO DRB-STR.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX TO 42.
           SET DX TO 1 PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRB-STR.
           PERFORM DRUCK.
           MOVE "Nr.   B e z e i c h n u n g         Wvkp.   Ekp-alt   E
      -        "kp-neu   +/- %" TO DRB-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRB-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       C.  IF AR-NUM < 1000;
               IF AR-APREIS NOT = 0 MOVE AR-APREIS TO DRB-PREIS(1)
               else MOVE AR-PREIS TO DRB-PREIS(1).
           MOVE WH-PREIS TO DRB-PREIS(2).
           IF WH-PG = 1 MOVE RE-NUM TO DRB-NUM
               MOVE RE-BEZ TO DRB-BEZ
               MOVE WH-GHP TO DRB-PREIS(3) WH-WERT
           else IF AR-NUM < 1000
                    MOVE AR-NUM TO DRB-NUM
                    MOVE AR-BEZ TO DRB-BEZ
                    MOVE AR-EKP TO DRB-PREIS(3) WH-WERT
               else MOVE LG-NUM TO DRB-NUM
                    MOVE LG-BEZ TO DRB-BEZ
                    MOVE LG-PREIS TO DRB-PREIS(3) WH-WERT
           IF WH-PREIS = 0 MOVE WH-WERT TO WH-PREIS.
           IF WH-PREIS NOT = 0 AND WH-WERT NOT = 0
               COMPUTE WH-PREIS ROUNDED = WH-WERT * 100 / WH-PREIS - 100
               IF WH-PREIS NOT = 0 MOVE WH-PREIS TO DRB-PROZ.
           PERFORM DRUCK.
       Z.  EXIT.
      ************************************* Rezepturenstammdatenliste *
       RP-LI SECTION.
       A.  CALL "CAUP" USING "0710050670000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druck Rezepturstammdaten " AT VDU-LP.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN WV-ARNUM.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Rpnr:          " AT VDU-LP.
           IF WL-CA = 20 GO H.
           DISPLAY "<ret-leer>= alle, <ret>= Rpnr., <ret+alpha> = suchen
      -        ", <esc>= Ende" AT 2301.
           DISPLAY "< >= alle ab Nr." AT 2401.
           CALL "CAUP" USING "1003094008" WH-CREG.
           IF ESC GO Y.
           IF TABL OR WOLI SET RET TO TRUE.
           IF AB MOVE WH-NUM TO RE-NUM GO D.
           IF NOT RET GO C.
           IF WH-NUM = 0 AND WV-MCODE NOT = SPACE
               CALL "PANANZ" USING "40RPSUCH" WH-CREG
               IF RE-NUM = 0 GO C
               ELSE GO D.
           MOVE WH-NUM TO RE-NUM WV-ARNUM.
           IF RE-NUM = 0 DISPLAY "alles" with highlight AT VDU-LP GO F.
       D.  MOVE RE-NUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
       E.  READ REZEPT IGNORE LOCK INVALID PERFORM NO-REC GO A.
           GO H.
       F.  START REZEPT KEY NOT < RE-KEY INVALID PERFORM NO-REC GO A.
       G.  READ REZEPT NEXT IGNORE LOCK AT END GO Y.
       H.  MOVE RE-ART TO WH-TART.
           PERFORM RP-KOPF.
           MOVE 0 TO RE-BET RE-MAT WH-BET WH-MIN WH-MZUS.
           MOVE 0 TO WS-MENGE WH-WERT WK-S.
           MOVE RE-NUM TO DRS-NUM
           MOVE RE-BEZ TO DRS-BEZ.
           MOVE "Teiler:" TO DRS-TX.
           MOVE RE-TEIL TO DRS-TEIG.
           MOVE "/" TO DRS-X.
           COMPUTE WK-NUM = RE-GEW / RE-TEIL.
           ADD 0,03 WK-NUM GIVING WH-MENGE.
           COMPUTE WH-MENGE = RE-GEW / RE-TEIL.
           MOVE WH-MENGE TO DRS-STK.
           ADD 309 VDU-ECK GIVING VDU-LP.
           DISPLAY DRS-STR AT VDU-LP.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
      *----------------------------------------------> Teigtemperatur <-
           IF RE-TEMP not = 0 or RE-GMIN(1) not = 0
                  or RE-GSEK(1) not = 0
               MOVE "Temperatur:" to DRS-STR
               MOVE RE-TEMP TO WD-PROZ
               MOVE WD-PROZ TO DRS-STR(13:)
               MOVE "Kneter: 1. Gang     Min     Sek" TO DRS-STR(21:)
               MOVE RE-GMIN(1) TO WD-TG
               MOVE WD-TG TO DRS-STR(37:3)
               MOVE RE-GSEK(1) TO WD-TG
               MOVE WD-TG TO DRS-STR(45:3)
               PERFORM DRUCK
               MOVE "        2. Gang     Min     Sek" TO DRS-STR(21:)
               MOVE RE-GMIN(2) TO WD-TG
               MOVE WD-TG TO DRS-STR(37:3)
               MOVE RE-GSEK(2) TO WD-TG
               MOVE WD-TG TO DRS-STR(45:3)
               PERFORM DRUCK.
           IF RE-GMIN(3) not = 0 or RE-GSEK(3) not = 0
               MOVE "Nachkneten: 1. Gang     Min     Sek"
                   TO DRS-STR(21:)
               MOVE RE-GNR TO WD-KZ
               MOVE WD-KZ TO DRS-STR(33:3)
               MOVE RE-GMIN(3) TO WD-TG
               MOVE WD-TG TO DRS-STR(41:3)
               MOVE RE-GSEK(3) TO WD-TG
               MOVE WD-TG TO DRS-STR(49:3)
               PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           SET XR TO 1.
           MOVE 1 TO WX.
       I.  IF RE-KONUM(XR) = 0 OR RE-MENGE(XR) = 0 GO W.
           IF WZ-ZEILEN > 61 PERFORM RP-KOPF.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           IF WK-S = 0 MOVE WH-T TO WK-S
               MOVE 0 TO WS-MENGE.
           IF WK-S = WH-T GO L.
       J.  IF WK-S = 0 GO K.
           IF WS-MENGE = 0 MOVE WH-T TO WK-S
               GO K.
           IF WK-S = 1 MOVE "** BrÅhstÅck **" TO DRR-KOBEZ.
           IF WK-S = 2 MOVE "** QuellstÅck **" TO DRR-KOBEZ.
           IF WK-S = 3 MOVE "** Vorteig **" TO DRR-KOBEZ.
           IF WK-S = 4 MOVE "** FÅllung **" TO DRR-KOBEZ.
      *    IF WK-S = 5 MOVE "** Zutaten o.Gew. **" TO DRR-KOBEZ.
           IF WK-S > 5 MOVE "** Zeiten **" TO DRR-KOBEZ.
           MOVE WS-MENGE TO DRR-MENGE.
           IF WK-S > 5 MOVE "min" TO DRR-MEH
                  else MOVE "gr" TO DRR-MEH.
           PERFORM DRUCK.
           MOVE WH-T TO WK-S.
           MOVE 0 TO WS-MENGE.
       K.  IF XR = 20 GO X.
       L.  MOVE WX TO DRR-POS.
           MOVE RE-KONUM(XR) TO DRR-KONUM.
           MOVE RE-KOBEZ(XR) TO DRR-KOBEZ.
           MOVE RE-MENGE(XR) TO WC-MENGE.
           IF RE-NGEW(XR) not = WC-MENGE MOVE "%" TO DRR-KONUM(7:1)
               COMPUTE WC-MENGE = WC-MENGE * RE-NGEW(XR) / 100.
           MOVE WH-A TO DRR-A.
           MOVE WH-T TO DRR-T.
           ADD 1 WH-M GIVING WH-MEH.
      *----------------> WH-A: 0 = Material, 1 = Zeit, 2 = Aufschlag <-
           IF WH-A = 0 MOVE "gr" TO DRR-MEH
                    MOVE WC-MENGE TO DRR-MENGE
               ELSE MOVE WT-MEH(WH-MEH) TO DRR-MEH
                    MOVE RE-PROZ(XR) TO DRR-PROZ.
           IF WH-A = 2
               COMPUTE WH-BET ROUNDED = RE-MAT * RE-PROZ(XR) / 100.
           IF WH-A = 0 MOVE RE-EKP(XR) TO DRR-PREIS
               COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-MENGE(XR) / 1000
               ADD WH-BET TO RE-MAT
               ADD WC-MENGE TO WS-MENGE
               IF WH-T < 5 ADD WC-MENGE TO WH-WERT.
           IF WH-A = 1
               COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-PROZ(XR)
               evaluate wh-t
                  when 5
                  when 6 ADD RE-PROZ(XR) TO WH-MIN
                         MOVE RE-EKP(XR) TO DRR-PREIS
                         ADD RE-PROZ(XR) TO WT-ZEIT(WH-T - 4).
           IF WH-A = 3 MOVE 0 TO WH-BET
               MOVE RE-PROZ(XR) TO WH-MZUS.
           MOVE WH-BET TO DRR-BET.
           ADD WH-BET TO RE-BET.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY DRR-STR(1:65) AT VDU-LP.
           PERFORM DRUCK.
           ADD 1 TO WX.
       W.  IF XR < 20 SET XR UP BY 1 GO I.
           IF WK-S NOT = 0 GO J.
       X.  MOVE ALL "ƒ" TO DRR-STR(21:) PERFORM DRUCK.
           MOVE WH-MIN TO WD-MIN.
           MOVE WD-MIN TO DRR-STR(25:7).
           MOVE "min." TO DRR-STR(32:4).
           MOVE WH-WERT TO DRR-MENGE.
           MOVE "gr" TO DRR-MEH
           MOVE RE-BET TO DRR-BET.
           IF WH-RART = 1 MOVE RE-TEIL TO WH-STK
                else COMPUTE WH-STK = RE-GEW / RE-TEIL.
           COMPUTE RE-PREIS ROUNDED = RE-BET / WH-STK.
           MOVE RE-PREIS TO DRR-PREIS.
           PERFORM DRUCK.
      *-------------------------------------> nur bei Artikel bis 999 <-
           IF RE-NUM < 1000 PERFORM KALKDRU.
           MOVE 2 TO WZ-SCHALT.
           PERFORM MODULDRU.
           IF WV-ARNUM NOT = 0 WRITE DRA-SATZ AFTER PAGE GO A.
           MOVE 3 TO WZ-SCHALT.
           IF WL-CA NOT = 20 MOVE 70 TO WZ-ZEILEN GO G.
       Y.  IF WZ-ZEILEN > 0 PERFORM END-DRU.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************************** Moduldruck *
       MODULDRU SECTION.
       A.  MOVE RE-NUM TO MO-NUM.
           MOVE SPACE TO MO-TXT.
           READ MODULE INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO A.
           PERFORM VARYING WY FROM 15 BY -1 UNTIL WY = 0
               or MO-ZL(WY) not = SPACE CONTINUE.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WY
               PERFORM RP-KOPF
               MOVE MO-ZL(WX) TO DRR-STR
               PERFORM DRUCK.
       Z.  EXIT.
      **************************************** Druck Kalkulationsdaten *
       KALKDRU SECTION.
       A.  MOVE 6 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO Z.
           MOVE RE-ART TO WH-TART.
           IF WH-BK = 0 PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
                            MOVE KO-PROZ(WZ) TO WK-PROZ(WZ)
                   else PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
                            MOVE KO-PROZ(WZ + 8) TO WK-PROZ(WZ).
           MOVE RE-NUM TO AR-NUM.
       C.  READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Kein Verkaufsartikel vorhanden" TO DRR-STR
               PERFORM DRUCK
               GO Z.
           MOVE RE-PREIS TO WS-BET.
           MOVE "Lohnkosten" TO DRR-STR(4:).
      *    MOVE RE-ART TO WH-TART.                 *> nach oben verlegt
           MOVE WH-BK TO DRR-STR(20:1).
           MOVE "/" TO DRR-STR(21:1).
           IF WH-BK = 0 MOVE "B" TO DRR-STR(22:1)
                   ELSE MOVE "K" TO DRR-STR(22:1).
           IF WH-MZUS NOT = 0
               COMPUTE WH-MIN = WH-MIN + (WH-MIN * WH-MZUS / 100).
           DIVIDE WH-STK INTO WH-MIN GIVING WH-DUMIN ROUNDED.
           MOVE WH-DUMIN TO WD-MIN.
           MOVE WD-MIN TO DRR-STR(34:7).
           MOVE "min" to DRR-STR(41:3).
           IF WH-BK = 0 MOVE KO-FAKT(1) TO WD-SATZ WH-WERT
                   ELSE MOVE KO-FAKT(2) TO WD-SATZ WH-WERT.
           MOVE WD-SATZ TO DRR-STR(45:6).
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-MIN / WH-STK.
           MOVE WH-BET TO DRR-PREIS.
           ADD WH-BET TO WS-BET.
           MOVE 2 TO WZ-SCHALT PERFORM DRUCK.
      *--------------------------------------> Produktionsgmeinkosten <-
           MOVE "Produktionsgemeinkosten" TO DRR-STR(4:).
           MOVE WD-MIN TO DRR-STR(34:7).
           MOVE "min" to DRR-STR(41:3).
           IF WH-BK = 0 MOVE KO-FAKT(3) TO WD-SATZ WH-WERT
                   ELSE MOVE KO-FAKT(4) TO WD-SATZ WH-WERT.
           MOVE WD-SATZ TO DRR-STR(45:6).
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-MIN / WH-STK.
           MOVE WH-BET TO DRR-PREIS.
           ADD WH-BET TO WS-BET.
           PERFORM DRUCK.
      *----------------------------------------------> Herstellkosten <-
           MOVE "= Herstellkosten  engros / detail" TO DRR-STR(4:)
           MOVE WS-BET TO DRR-PREIS WD-PREIS
           MOVE WD-PREIS TO DRR-STR(42:9)
           PERFORM DRUCK.
      *---------------------------------> Retourwarenanteil GHP / EHP <-
           MOVE "Retourwarenanteil" TO DRR-STR(4:)
           MOVE WK-PROZ(1) TO WD-PZ WH-WERT
           MOVE WD-PZ TO DRR-STR(23:4)
           MOVE "%" TO DRR-STR(27:1)
           COMPUTE WH-BET = WS-BET * WH-WERT / 100
           ADD WS-BET WH-BET GIVING WH-GHP
           MOVE WH-BET TO WD-PREIS
           MOVE WD-PREIS TO DRR-STR(42:9)
           MOVE WK-PROZ(2) TO WD-PZ WH-WERT
           MOVE WD-PZ TO DRR-STR(32:4)
           MOVE "%" TO DRR-STR(36:1)
           COMPUTE WH-BET = WS-BET * WH-WERT / 100
           ADD WS-BET WH-BET GIVING WH-EHP
           MOVE WH-BET TO WD-PREIS DRR-PREIS
           MOVE 2 TO WZ-SCHALT PERFORM DRUCK.
      *-----------------------------------> Vertriebskosten GHP / EHP <-
           MOVE "Vertriebskosten" TO DRR-STR(4:)
           ADD WK-PROZ(3) WK-PROZ(5) WK-PROZ(7) GIVING WH-BET
           COMPUTE WH-GNVP = WH-GHP / (100 - WH-BET)
           ADD WK-PROZ(4) WK-PROZ(6) WK-PROZ(8) GIVING WH-BET
           COMPUTE WH-ENVP = WH-EHP / (100 - WH-BET)
           MOVE WK-PROZ(3) TO WD-PZ WH-WERT
           MOVE WD-PZ TO DRR-STR(23:4)
           MOVE "%" TO DRR-STR(27:1)
           DISPLAY WD-PZ with highlight AT VDU-LP.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP
           MOVE WH-BET TO WD-PREIS
           ADD WH-BET TO WH-GHP
           MOVE WD-PREIS TO DRR-STR(42:9)
           MOVE WK-PROZ(4) TO WD-PZ WH-WERT.
           MOVE WD-PZ TO DRR-STR(32:4)
           MOVE "%" TO DRR-STR(36:1)
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP
           MOVE WH-BET TO DRR-PREIS
           ADD WH-BET TO WH-EHP
           PERFORM DRUCK.
      *-------------------------------------------> Verwaltungskosten <-
           MOVE "Verwaltungskosten" TO DRR-STR(4:)
           MOVE WK-PROZ(5) TO WD-PZ WH-WERT.
           MOVE WD-PZ TO DRR-STR(23:4)
           MOVE "%" TO DRR-STR(27:1)
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP
           MOVE WH-BET TO WD-PREIS
           ADD WH-BET TO WH-GHP
           MOVE WD-PREIS TO DRR-STR(42:9)
           MOVE WK-PROZ(6) TO WD-PZ WH-WERT
           MOVE WD-PZ TO DRR-STR(32:4)
           MOVE "%" TO DRR-STR(36:1)
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP
           MOVE WH-BET TO DRR-PREIS
           ADD WH-BET TO WH-EHP
           PERFORM DRUCK.
      *------------------------------------------------> Selbstkosten <-
           MOVE "= Selbstkosten" TO DRR-STR(4:)
           MOVE WH-GHP TO WD-PREIS
           MOVE WD-PREIS TO DRR-STR(42:9)
           MOVE WH-EHP TO DRR-PREIS
           PERFORM DRUCK.
      *--------------------------------------------> Risiko u. Gewinn <-
           MOVE "Risiko u. Gewinn" TO DRR-STR(4:)
           MOVE WK-PROZ(7) TO WD-PZ WH-WERT
           MOVE WD-PZ TO DRR-STR(23:4)
           MOVE "%" TO DRR-STR(27:1)
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP
           MOVE WH-BET TO WD-PREIS
           ADD WH-BET TO WH-GHP
           MOVE WD-PREIS TO DRR-STR(42:9)
           MOVE WK-PROZ(8) TO WD-PZ WH-WERT
           MOVE WD-PZ TO DRR-STR(32:4)
           MOVE "%" TO DRR-STR(36:1)
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP
           MOVE WH-BET TO DRR-PREIS
           ADD WH-BET TO WH-EHP
           MOVE 2 TO WZ-SCHALT PERFORM DRUCK.
      *------------------------------------------> Nettoverkaufspreis <-
           MOVE "= Nettoverkaufspreis" TO DRR-STR(4:)
           MOVE WH-GHP TO WD-PREIS.
           MOVE WD-PREIS TO DRR-STR(42:)
           MOVE WH-EHP TO DRR-PREIS
           PERFORM DRUCK.
      *------------------------------------------------> Umsatzsteuer <-
           MOVE WT-UST(AR-UST) TO WD-PZ WH-WERT
           MOVE WD-PZ TO DRR-STR(4:)
           MOVE "Umsatzsteuer" TO DRR-STR(10:)
           COMPUTE WH-WERT = WH-WERT / 100
           COMPUTE WH-BET ROUNDED = WH-EHP * WH-WERT
           MOVE WH-BET TO DRR-PREIS
           ADD WH-BET TO WH-EHP
           PERFORM DRUCK.
      *-----------------------------------------------> Verkaufspreis <-
           MOVE "= Verkaufspreis" TO DRR-STR(4:)
           MOVE WH-GHP TO WD-PREIS.
           MOVE WD-PREIS TO DRR-STR(42:9)
           MOVE WH-EHP TO DRR-PREIS
           PERFORM DRUCK.
      *------------------------------------> effektiver Verkaufspreis <-
           MOVE "effektiver Verkaufspreis" TO DRR-STR(4:)
           IF AR-ADAT = 0 OR AR-ADAT > WM-DATUM
               MOVE AR-PREIS TO WD-PREIS WH-WERT
               MOVE AR-VKP TO WH-PREIS
           ELSE MOVE AR-APREIS TO WD-PREIS WH-WERT
               MOVE AR-AVKP TO WH-PREIS.
           MOVE WD-PREIS TO DRR-STR(42:9)
           MOVE WH-PREIS TO DRR-PREIS
           PERFORM DRUCK.
      *-----------------------------------------------------> Deckung <-
           MOVE "= Deckung" TO DRR-STR(4:)
           COMPUTE WH-BET = (WH-WERT / WH-GHP * 100) - 100
           MOVE WH-BET TO WD-PREIS
           MOVE WD-PREIS TO DRR-STR(42:9)
           COMPUTE WH-BET = (WH-PREIS / WH-EHP * 100) - 100
           MOVE WH-BET TO DRR-PREIS
           MOVE "%" TO DRR-STR(52:1)
           MOVE "%" TO DRR-STR(60:1)
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       RP-KOPF SECTION.
       A.  IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0000" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE WE-DRU(5) TO WM-DRU.
           IF WM-DRU = 1 MOVE 4 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Rezepturenaufstellung" TO DRR-STR.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX TO 47.
           SET DX TO 1 PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRR-STR.
           PERFORM DRUCK.
           MOVE "Pos Nr.   B e z e i c h n u n g  AT   Menge Meh      Pr
      -        "eis     Betrag" TO DRR-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRR-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      *********************************************** Komponentensuche *
       KOMPSUCH SECTION.
       A.  CALL "CAUP" USING "0707050560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Komponentensuche " with highlight AT VDU-LP.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Komp.-Nr.:" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Komp.Bez.:" AT VDU-LP.
       C.  DISPLAY "<ret>= Komponentennr., <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "1002144004"WH-CREG.
           IF ESC GO X.
           IF NOT RET or WH-WERT = 0 GO C.
           MOVE WH-WERT TO LG-NUM WD-NUM WV-ARNUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
       E.  READ LAGARTIK INVALID MOVE "nicht angelegt!" TO LG-BEZ.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 314 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-BEZ with highlight AT VDU-LP.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WM-OPEN.
       K.  DISPLAY "<ret/#>= Start, <esc>= zurÅck < >" AT 2301.
           DISPLAY "<#> statt <ret>= Bildschirmausgabe" AT 2401.
           CALL "CAUP" USING "0023320000" WH-CREG.
           MOVE 0 TO WH-MOD.
           IF ESC GO B.
           IF KIST MOVE 1 TO WH-MOD
               SET RET TO TRUE.
           IF NOT RET GO K.
           MOVE LOW-VALUE TO RE-SATZ.
           START REZEPT KEY NOT < RE-KEY INVALID GO C.
       O.  READ REZEPT NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO O.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE RE-NUM TO WD-NUM
           DISPLAY "Rezept-Nr.: " AT VDU-LP WD-NUM with highlight " "
               RE-BEZ with highlight foreground-color 6.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
              or RE-KONUM(WX) = WV-ARNUM CONTINUE.
           IF WX = 21 GO O.
           IF WH-MOD = 1 GO Q.
           IF WZ-ZEILEN > 60 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Q.
           MOVE x"0000" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE WE-DRU(5) TO WM-DRU.
           IF WM-DRU = 1 MOVE 4 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Komp.Suche:" TO DRT-STR.
           MOVE WV-ARNUM TO WD-NUM.
           MOVE WD-NUM(3:) TO DRT-STR(14:5).
      *    IF LG-NUM not = 0 MOVE LG-NUM TO WD-NUM
      *        MOVE "Komp.Tausch:" TO DRT-STR(1:12)
      *        MOVE "/" TO DRT-STR(20:1)
      *        MOVE WD-NUM TO DRT-STR(22:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX TO 42.
           SET DX TO 5 PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-STR.
           PERFORM DRUCK.
           MOVE "Rp.Nr  B e z e i c h n u n g           " TO DRT-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Q.  MOVE RE-NUM TO DRT-NUM.
           MOVE RE-BEZ TO DRT-BEZ.
           IF WH-MOD not = 1 PERFORM DRUCK GO O.
      *-------------------------------------------> Bildschirmausgabe <-
      *    DISPLAY "Rezept-Nr.: " AT 2401 DRT-STR(1:35).
       S.  DISPLAY "<esc>= Abbruch, <ret>= weiter < >" AT 2301.
           CALL "CAUP" USING "0023320000" WH-CREG.
           IF ESC GO B.
           IF not RET GO S.
           MOVE SPACE TO DRA-SATZ.
           GO O.
       W.  IF WZ-ZEILEN NOT = 0 PERFORM END-DRU.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************** Rezepturenkurzliste *
       RPKULI SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
       B.  DISPLAY "<esc>= Abbruch, <ret>= Start(num), <tab>= Start(alph
      -        "a) " AT 2301.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN RE-NUM.
           MOVE SPACE TO RE-AKEY.
           CALL "CAUP" USING "0023560000" WH-CREG.
           IF ESC GO Z.
           IF RET GO C.
           IF NOT TABL GO B.
           START REZEPT KEY NOT < RE-AKEY INVALID PERFORM NO-REC GO Z.
           GO D.
       C.  START REZEPT KEY NOT < RE-KEY INVALID PERFORM NO-REC GO Z.
       D.  READ REZEPT NEXT IGNORE LOCK AT END GO Y.
           MOVE RE-ART TO WH-TART.
           PERFORM RPKU-KOPF.
           MOVE RE-NUM TO DRT-NUM
           MOVE RE-BEZ TO DRT-BEZ.
           MOVE RE-TEIL TO DRT-TEIG.
           COMPUTE WK-NUM = RE-GEW / RE-TEIL.
           ADD 0,03 WK-NUM GIVING WH-MENGE.
           COMPUTE WH-MENGE = RE-GEW / RE-TEIL.
           MOVE WH-MENGE TO DRT-STK.
           MOVE RE-PREIS TO DRT-PREIS.
           DISPLAY DRT-STR AT 1001.
           PERFORM DRUCK.
           GO D.
       Y.  IF WZ-ZEILEN > 0 PERFORM END-DRU.
       Z.  EXIT.
      *****************************************************************
       RPKU-KOPF SECTION.
       A.  IF WZ-ZEILEN > 60 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE x"0000" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE WE-DRU(5) TO WM-DRU.
           IF WM-DRU = 1 MOVE 4 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE WK-GEB TO DRT-STR.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX TO 37.
           SET DX TO 1 PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-STR.
           PERFORM DRUCK.
           MOVE "Rp.Nr  B e z e i c h n u n g           Teiler         P
      -        "reis" TO DRT-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ********************************************  Artikelstammliste *
       KO-LI SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
       B.  DISPLAY "<esc>= Abbruch, <ret>= Start(num), <tab>= Start(alph
      -        "a) " AT 2301.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN.
           MOVE SPACE TO LG-AKEY.
           MOVE 1000 TO LG-NUM.
           CALL "CAUP" USING "0023550000" WH-CREG.
           IF ESC GO Z.
           IF RET GO C.
           IF NOT TABL GO B.
           START LAGARTIK KEY NOT < LG-AKEY INVALID PERFORM NO-REC GO Z.
           GO D.
       C.  START LAGARTIK KEY NOT < LG-KEY INVALID PERFORM NO-REC GO Z.
       D.  READ LAGARTIK NEXT IGNORE LOCK AT END GO W.
           IF LG-NUM < 1000 GO D.
           IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE x"0100" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           MOVE WE-DRU(5) TO WM-DRU.
           IF WM-DRU = 1 MOVE 4 TO WH-P.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Komponentenstammdatenliste" TO DRH-STR.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX TO 40.
           SET DX TO 1 PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRH-STR.
           PERFORM DRUCK.
           MOVE "Nr.   B e z e i c h n u n g      Meh N A    Preis
      -        "Ekp." TO DRH-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRH-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  MOVE LG-NUM TO DRH-ARNUM.
           MOVE LG-BEZ TO DRH-ARBEZ.
           ADD 1 LG-MEH GIVING WH-MEH.
           MOVE WT-MEH(WH-MEH) TO DRH-MEH.
           MOVE LG-NK TO DRH-NK.
           MOVE LG-PREIS TO WH-PREIS WD-PREIS WD-APREIS.
           IF WR-GR = 0 MOVE WD-PREIS TO DRH-PR(1)
                  ELSE MOVE WD-APREIS TO DRH-PR(1).
           IF LG-PREIS NOT = 0 MOVE LG-PREIS TO WD-PREIS
                MOVE WD-PREIS TO DRH-PR(2).
      *    MOVE LG-UST TO DRH-ART.
           MOVE DRH-STR TO WT-TX.
           DISPLAY WT-TX with SIZE 80 AT 2001.
           PERFORM DRUCK.
           GO D.
       W.  IF WZ-ZEILEN > 0 PERFORM END-DRU.
       Z.  EXIT.
