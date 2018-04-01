      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PANLAG.
      ******************************************************************
      *            Lagerbuchhaltung fuer Gaerverzoegerer               *
      *     Statistik f. 30 besondere Tage u. Erzeugungskontrolle      *
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       OBJECT-COMPUTER.  P3800.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROTOK     ASSIGN TO "PANEPROT.DAT"
                             ORGANIZATION LINE SEQUENTIAL
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
           SELECT LAGER      ASSIGN TO "PANELAG.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY LA-KEY
                             FILE STATUS IS WF-STATUS.
           COPY PANSEART.CPY.
           COPY PANSEKON.CPY.
           SELECT PRDLIST    ASSIGN TO "PANEXCEL.PDL"
                             FILE STATUS WF-STATUS.
           SELECT GVLLIST    ASSIGN TO "PANEXCEL.GVL"
                             FILE STATUS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANFDART.CPY.
       COPY PANKONS.CPY.
       COPY PANSUMM.CPY.
      *********************** alle erzeugten und ausgebackenen Artikel *
      *----------------------> Produktionsauswertung: nur 5-er Zeiten <-
       FD  PRDLIST                     LABEL RECORD STANDARD.
       01  PD-SATZ.
           03  PD-NUM                  PIC 999.
           03  PD-BEZ                  PIC X(25).
           03  PD-ANZ                  PIC S9(5)V99.
      ************************************ nur ausgebackene GV-Artikel *
      *----------------------> Produktionsauswertung: nur 6-er Zeiten <-
       FD  GVLLIST                     LABEL RECORD STANDARD.
       01  GV-SATZ.
           03  GV-NUM                  PIC 999.
           03  GV-BEZ                  PIC X(25).
           03  GV-ANZ                  PIC S9(5)V99.
      ********************************************** Artikelstatistik *
       FD  LAGER     external         LABEL RECORD STANDARD
                                      RECORD 192 CHARACTERS.
       01  LA-SATZ.
           03  LA-KEY.
               05 LA-KZ                PIC 99        COMP.
               05 LA-ARNUM             PIC 9(4)      COMP.
           03  LA-PERIOD               PIC 9(6)      COMP.
           03  LA-DATUM                PIC 9(6)      COMP.
           03  LA-PDAT                 PIC 9(6)      COMP.
           03  LA-MENGEN               PIC S9(7)V99  COMP-3  OCCURS 36.
      *-----> LA-MENGEN: 34= Produktion *** 35= Verkauf *** 36= Stand <-
      *----------------------------------------> Record '0' Tageliste <-
       01  LA-BSATZ.
           03  FILLER                  PIC X(70).
           03  LA-JAHR                 PIC 9999      COMP.
           03  LA-TN.
               05 LA-TGTGN                 OCCURS 30.
                  07 LA-TAGE           PIC 9(6)     COMP.
                  07 LA-TGN            PIC 99       COMP.
      *********************************** monatliche Produktionssummen *
       FD  PRODSUMME                    LABEL RECORD STANDARD.
       01  PS-SATZ.
           03  PS-KEY.
               05 PS-PER               PIC 9(4)     COMP.
               05 PS-ARNUM             PIC 9(4)     COMP.
           03  PS-MENGE                PIC 9(7)V99  COMP OCCURS 31.
           03  PS-RETMG                PIC 9(7)V99  COMP OCCURS 31.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC XX.
           03  DRC-STR.
               05 DRC-ARNUM            PIC ZZZ9-.
               05 DRC-BEZ.
                  07 FILLER            PIC X(7).
                  07 DRC-MENGE         PIC X(9)    OCCURS 14.
       01  DRD-SATZ.
           03  FILLER                  PIC X(14).
           03  DRD-DAT                             OCCURS 14.
               05 DRD-DATUM            PIC XXXXX.
               05 DRD-ST               PIC X.
               05 DRD-TAG              PIC XX.
               05 DRD-BAR              PIC X.
       01  DRE-SATZ.
           03  FILLER                  PIC XX.
           03  DRE-STR.
               05 DRE-ARNUM            PIC ZZZ9-.
               05 DRE-BEZ              PIC X(27).
               05 DRE-PMG              PIC X(12).
               05 DRE-LMG              PIC X(12).
               05 DRE-DIF              PIC X(15).
       01  DRX-SATZ.
           03  DRX-X                   PIC 9999  COMP-X  OCCURS 121.
       01  DRY-SATZ.
           03  DRY-X                   PIC X   OCCURS 242  INDEXED XX.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-AZ               PIC 99.
               05  WL-VL               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-VP               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WH-DRUNAM               PIC X(12)   VALUE "LPT1:".
           03  WM-LST                  PIC X(12).
           03  WH-PX                   PIC XX      OCCURS 2.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99      COMP-X.
           03  WX-PRSTAT               PIC 99      COMP-X.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WK-DS.
               05 FILLER               PIC X(5)       VALUE "per:".
               05 WK-DAT               PIC X(10).
               05 FILLER               PIC X(7)       VALUE "Seite: ".
               05 WK-SEITE             PIC ZZ9-.
           03  WX-DS REDEFINES WK-DS   PIC X     OCCURS 26 INDEXED DX.
           03  WH-MOD                  PIC 99        COMP.
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
           03  WH-L                    PIC 99        COMP.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WD-ART                  PIC ZZZ9.
           03  WH-KB                   PIC 99       COMP.
       01  WT-DK    external.
           03  WR-TN                   PIC X(10)  OCCURS 7.
           03  WT-KON                  PIC 99     OCCURS 12.
       01  WF-REG.
           03  WF-FEIER.
               05 FILLER         PIC X(19) VALUE "vor  3 Kînige".
               05 FILLER         PIC X(19) VALUE "nach 3 Kînige".
               05 FILLER         PIC X(19) VALUE "Faschingsamstag".
               05 FILLER         PIC X(19) VALUE "Faschingsonntag".
               05 FILLER         PIC X(19) VALUE "Rosenmontag".
               05 FILLER         PIC X(19) VALUE "Faschingdienstag".
               05 FILLER         PIC X(19) VALUE "Aschermittwoch".
               05 FILLER         PIC X(19) VALUE "GrÅndonnerstag".
               05 FILLER         PIC X(19) VALUE "Karfreitag".
               05 FILLER         PIC X(19) VALUE "Karsamstag".
               05 FILLER         PIC X(19) VALUE "vor 1. Mai".
               05 FILLER         PIC X(19) VALUE "vor Muttertag".
               05 FILLER         PIC X(19) VALUE "Pfingstsamstag".
               05 FILLER         PIC X(19) VALUE "vor 26. Oktober".
               05 FILLER         PIC X(19) VALUE "2 Tg. v. Allerhlg.".
               05 FILLER         PIC X(19) VALUE "1 Tg. v. Allerhlg.".
               05 FILLER         PIC X(19) VALUE "Allerheiligen".
               05 FILLER         PIC X(19) VALUE "3 Tg. v. Krampus".
               05 FILLER         PIC X(19) VALUE "2 Tg. v. Krampus".
               05 FILLER         PIC X(19) VALUE "1 Tg. v. Krampus".
               05 FILLER         PIC X(19) VALUE "Krampus".
               05 FILLER         PIC X(19) VALUE "Nikolaus".
               05 FILLER         PIC X(19) VALUE "3 Tg. v. H.Abend".
               05 FILLER         PIC X(19) VALUE "2 Tg. v. H.Abend".
               05 FILLER         PIC X(19) VALUE "1 Tg. v. H.Abend".
               05 FILLER         PIC X(19) VALUE "Heiliger Abend".
               05 FILLER         PIC X(19) VALUE "1 Tg. v. Silvester".
               05 FILLER         PIC X(19) VALUE "Silvester".
           03  WF-FNAME REDEFINES
               WF-FEIER          PIC X(19) OCCURS 28.
           03  WK-FTAG.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0105071.
               05 FILLER         PIC S9(7)  COMP-3  VALUE +0107071.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000510.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000500.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000490.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000480.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000470.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000040.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000030.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0000020.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -0430071.
               05 FILLER         PIC S9(7)  COMP-3  VALUE +0507991.
               05 FILLER         PIC S9(7)  COMP-3  VALUE +0000470.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1025071.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1030071.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1031071.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1101000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1202000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1203000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1204000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1205000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1206000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1221000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1222000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1223000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1224000.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1230071.
               05 FILLER         PIC S9(7)  COMP-3  VALUE -1231000.
           03  WT-FTAG                 PIC X(112).
           03  WF-FTAG REDEFINES
               WT-FTAG                 PIC S9(7)    COMP-3  OCCURS 28.
           03  WI                      PIC 99       COMP.
           03  WM-TAG                  PIC S9(7).
           03  WR-TG REDEFINES WM-TAG.
               05 WR-TAG               PIC 9(4).
               05 WR-WOTG              PIC 99.
               05 WR-DIF               PIC S9.
           03  WD-JAHR                 PIC 9999.
           03  WK-JAHR                 PIC 9999     COMP.
           03  WV-JAHR                 PIC 9999     COMP.
           03  WD-X                    PIC X.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON LAGER.    
       A.  CALL "CADECL" USING "PANELAG.DAT"  WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON PROTOK.
       A.  CALL "CADECL" USING "PANEPROT.DAT" WH-CREG.
       DECL-S SECTION.         USE AFTER ERROR PROCEDURE ON SUMMEN.
       A.  CALL "CADECL" USING "PANESUMM.DAT" WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON PRODSUMME.
       A.  CALL "CADECL" USING "PANPRODU.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON GVLLIST.
       A.  CALL "CADECL" USING "PANEXCEL.GVL" WH-CREG.
       DECL-L SECTION.         USE AFTER ERROR PROCEDURE ON PRDLIST.
       A.  CALL "CADECL" USING "PANEXCEL.GVL" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE "Tage/Lager Verwaltung" TO WK-GEB.
           OPEN I-O LAGER.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "1 - Tagesverwaltung"           AT 0524
                   "2 - Druck Tagesstatistik (50)" AT 0024
                   "3 - Produktionsmengen"     AT 0024
                   "4 - öbernahme Verkauf"     AT 0024
                   "5 - Produktionsvergleich"  AT 0024
                   "6 - GVL-Produktionsmengen" AT 0024
                   " "                         AT 0024
                   "leer - Programmende"       AT 0024
                   " "                         AT 0024
                   "bitte wÑhlen Sie:  " with highlight AT 0039
           CALL "CAUP" USING "0014581001" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 GO X
               WHEN 1 PERFORM TAGE
               WHEN 2 PERFORM TAGDRU
               WHEN 3 PERFORM PRODME
               WHEN 4 PERFORM UEBERN
               WHEN 5 PERFORM VERGL
               WHEN 6 PERFORM PRODME.
           GO B.
       X.  CLOSE LAGER.
           PERFORM END-DRU.
           MOVE WL-CREG TO WH-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  CALL "CAUP" USING "04DATDREH" WH-CREG.
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
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO VDU-L.
           IF VDU-L < 20 GO Z.
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
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) not = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
       C.  DISPLAY "Drucker nicht bereit: " with highlight AT 2301
               "<ret>= Fehler beheben <Einfg>= Druckdatei < >".
           DISPLAY "Dateiname: " AT 2401 WM-LST with highlight.
           CALL "CAUP" USING "0023660000" WH-CREG.
           IF RET GO A.
           IF EINF MOVE WM-LST TO WH-DRUNAM GO A.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
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
       KB-STAT SECTION.
       A.  CALL "CBL_GET_KBD_STATUS" using WH-KB.
           IF WH-KB = 0 GO Z.
       C.  DISPLAY "Programm unterbrochen!" with highlight AT 2401
              "<esc>= Abbruch, <ret>= Fortsetzung " AT 2424.
           CALL "CAUP" USING "0024591000" WH-CREG.
           PERFORM UNTER.
           IF NOT RET AND NOT ESC GO C.
       Z.  EXIT.
      ***************************************** Text 'Unterbrechnung' *
       UNTER SECTION.
       A.  DISPLAY "Unterbrechung mîglich" AT 2301.
       Z.  EXIT.
      *****************************************************************
       DISP-MENGE SECTION.
       A.  MOVE WH-ANZ TO WA-ANZ.
           IF AR-NK = 0 GO C.
           IF AR-NK = 1 GO B.
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
       E.  DISPLAY WD-MG with highlight AT VDU-LP.
           GO Z.
       X.  DISPLAY "Menge zu gross" AT 2401
               PERFORM WEITER
               MOVE 99 TO WX-TASTE.
       Z.  EXIT.
      ***************************************** Mengenkommaumwandlung *
       MG SECTION.
       A.  IF AR-NK = 2 DIVIDE 100 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ2 WD-MGC
               MOVE WD-MGC TO WD-MG
               MOVE WD-ANZ2 TO WD-MENGE.
           IF AR-NK = 1 DIVIDE 10 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ1 WD-MGB
               MOVE WD-MGB TO WD-MG
               MOVE WD-ANZ1 TO WD-MENGE.
           IF AR-NK = 0 MOVE WH-ANZ TO WD-ANZ WD-MGA
               MOVE WD-MGA TO WD-MG
               MOVE WD-ANZ TO WD-MENGE.
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      *********************************************** Listladeroutine *
       DASEIT SECTION.
       A.  SET DX TO 1.
           MOVE VDU-DATUM TO WK-DAT.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
       B.  MOVE WX-DS(DX) TO DRY-X(XX).
           IF DX < 26 SET DX XX UP BY 1 GO B.
       Z.  EXIT.
      ******************************************************************
       TAGE SECTION.
       A.  MOVE " Verwaltung - 30 Tage" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE WM-DATUM TO WZ-DATUM.
           DISPLAY "Jahr: " AT 0310.
           MOVE LOW-VALUE TO LA-KEY.
       B.  READ LAGER INVALID KEY MOVE LOW-VALUE TO LA-SATZ.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF LA-JAHR = 0 GO F.
           PERFORM TAGANZ.
       D.  MOVE LA-JAHR TO WD-JAHR.
           DISPLAY WD-JAHR with highlight AT 0317.
           DISPLAY "<esc>= Abbruch, <ret-leer>= Ende, <ret>= Jahr "
               AT 2301.
           DISPLAY "Jahreszahl bitte 4-stellig!" AT 2401.
           CALL "CAUP" USING "0003174004" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO D.
           IF WH-NUM = 0 GO Z.
           IF WH-NUM < 1909 OR WH-NUM > 2099 GO D.
           COMPUTE WK-JAHR = WM-DATUM / 10000 + 1900.
           IF WK-JAHR < 1909 ADD 100 TO WK-JAHR.
           IF WH-NUM < WK-JAHR GO D.
           ADD 1 TO WK-JAHR.
           IF WH-NUM > WK-JAHR GO D.
       F.  PERFORM JAHRDURCH.
           PERFORM TAGANZ.
           GO D.
       Z.  EXIT.
      ******************************************************************
       JAHRDURCH SECTION.
       A.  IF LA-JAHR = 0
               IF WZ-JAHR < 09 ADD 2000 WZ-JAHR GIVING WH-NUM
                          else ADD 1900 WZ-JAHR GIVING WH-NUM.
           MOVE WH-NUM TO WK-JAHR WD-JAHR LA-JAHR.
      *------------------------------------> neues Jahr durchrechnen <-
           DISPLAY WD-JAHR AT 0317.
           COMPUTE WZ-DATUM = WK-JAHR * 10000 + 0229.
           CALL "CAUP" USING "05DATPRUEF" WH-CREG.
           IF WZ-DATUM = 0 MOVE 28 TO WT-KON(2)
               else MOVE 29 TO WT-KON(2).
           MOVE WK-FTAG TO WT-FTAG.
           MOVE 1 TO WI.
       G.  MOVE WF-FTAG(WI) TO WM-TAG.
           MOVE WR-TAG TO WZ-DATUM.
           MOVE WK-JAHR TO WZ-JAHR.
           IF WR-TAG = 0 PERFORM FEIER.
       H.  CALL "CAUP" USING "03DATUM" WH-CREG.
           PERFORM VERSCHIEB.
           IF WR-WOTG = 99; IF WO-TGN NOT = 1 ADD 1 TO WZ-TAG GO H.
           IF WO-TGN = WR-WOTG ADD WR-DIF TO WZ-TAG
               CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WX-DATUM TO LA-TAGE(WI).
           MOVE WO-TGN TO LA-TGN(WI).
           IF WI < 28 ADD 1 TO WI GO G.
           WRITE LA-SATZ INVALID REWRITE LA-SATZ.
       Z.  EXIT.
      ******************************************************************
       TAGANZ SECTION.
       A.  MOVE 1 TO WI.
           MOVE LA-JAHR TO WD-JAHR.
           DISPLAY WD-JAHR with highlight AT 0317.
           CALL "CAUP" USING "1303450380000" WH-CREG.
       D.  IF WI > 15 COMPUTE VDU-L = WI - 11
                      MOVE 45 TO VDU-P
                 else COMPUTE VDU-L = WI + 4
                      MOVE 10 TO VDU-P.
           MOVE LA-TAGE(WI) TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO WT-TX.
           DISPLAY WT-TX WITH SIZE 6 AT VDU-LP.
           ADD 8 TO VDU-P.
           MOVE LA-TGN(WI) TO WO-TGN.
           DISPLAY WR-TN(WO-TGN) WITH SIZE 3 AT VDU-LP.
           ADD 5 TO VDU-P.
           DISPLAY WF-FNAME(WI) AT VDU-LP.
           IF WI < 28 ADD 1 TO WI GO D.
       Z.  EXIT.
      ******************************************************************
       VERSCHIEB SECTION.
       A.  IF WI = 18; IF WO-TGN = 2
               ADD 1000 TO WF-FTAG(18)
               ADD 1000 TO WF-FTAG(19)
               ADD 1000 TO WF-FTAG(20) GO X.
           IF WI = 18; IF WO-TGN = 1
               ADD 1000 TO WF-FTAG(18)
               ADD 1000 TO WF-FTAG(19) GO X.
           IF WI = 18; IF WO-TGN = 7 ADD 1000 TO WF-FTAG(18) GO X.
           IF WI = 23; IF WO-TGN = 2
               ADD 1000 TO WF-FTAG(23)
               ADD 1000 TO WF-FTAG(24)
               ADD 1000 TO WF-FTAG(25) GO X.
           IF WI = 23; IF WO-TGN = 1
               ADD 1000 TO WF-FTAG(23)
               ADD 1000 TO WF-FTAG(24) GO X.
           IF WI = 18; IF WO-TGN = 7 ADD 1000 TO WF-FTAG(23) GO X.
           GO Z.
       X.  MOVE WF-FTAG(WI) TO WM-TAG.
           MOVE WR-TAG TO WZ-DATUM.
           MOVE WK-JAHR TO WZ-JAHR.
           CALL "CAUP" USING "03DATUM" WH-CREG.
       Z.  EXIT.
      ******************************************* Feiertagsermittlung *
       FEIER SECTION.
       A.  COMPUTE WZ-DATUM = WK-JAHR * 10000 + 0315.
           MOVE 0 TO WZ.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           ADD 29 WX TO WZ.
           MOVE 3 TO WF.
           IF WF-FTAG(WI) > 0 ADD WR-WOTG TO WZ.
       C.  IF WZ > WT-KON(WF) SUBTRACT WT-KON(WF) FROM WZ
               ADD 1 TO WF GO C.
       D.  IF WF-FTAG(WI) NOT < 0 GO G.
           IF WZ < WR-WOTG SUBTRACT WZ FROM WR-WOTG
                           SUBTRACT 1 FROM WF
                           MOVE WT-KON(WF) TO WZ GO D
                      else SUBTRACT WR-WOTG FROM WZ
                           IF WZ = 0  SUBTRACT 1 FROM WF
                               MOVE WT-KON(WF) TO WZ.
       G.  MOVE WF TO WZ-MONAT.
           MOVE WZ TO WZ-TAG.
       Z.  EXIT.
      ******************************************************************
       TAGDRU SECTION.
       A.  MOVE " Druck Tagestatistik" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Bitte 8-Zoll Formular ( Din-A4 quer) einlegen"
               AT 1010.
           DISPLAY "<ret>= Start, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           CALL "CAUP" USING "1310012480" WH-CREG.
           DISPLAY "Unterbrechnung mîglich!" AT 2301.
           MOVE LOW-VALUE TO LA-SATZ.
       C.  READ LAGER INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN.
           MOVE LA-TN TO WT-BEZ.
       D.  READ LAGER NEXT WITH NO LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO D.
           PERFORM KB-STAT
               IF ESC GO W.
           PERFORM TAKO.
           MOVE LA-ARNUM TO AR-NUM.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 28
               IF LA-MENGEN(WI) not = 0 MOVE 39 TO WI.
           IF WI = 29 GO D.
       F.  READ ARTIKEL WITH NO LOCK INVALID
               MOVE "Artikel fehlt" TO AR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO F.
           MOVE AR-NUM TO DRC-ARNUM.
           MOVE AR-BEZ TO DRC-BEZ.
           DISPLAY DRC-ARNUM AT 1010 " " DRC-BEZ.
           PERFORM DRUCK.
           MOVE 1 TO WI WX.
       H.  MOVE LA-MENGEN(WI) TO WH-ANZ.
           IF WH-ANZ NOT = 0
               PERFORM MG
               MOVE WD-MENGE TO DRC-MENGE(WX).
           IF WI = 14 PERFORM DRUCK
               MOVE 0 TO WX.
           IF WI < 28 ADD 1 TO WI
                      ADD 1 TO WX GO H.
           PERFORM DRUCK.
           GO D.
       W.  PERFORM END-DRU.
           MOVE "LPT1" TO WH-DRUNAM.
       Z.  EXIT.
      ******************************************************************
       TAKO SECTION.
       A.  IF WZ-ZEILEN > 40 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE LA-SATZ TO WT-ADR.
           MOVE WT-BEZ TO LA-TN.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"0008" TO WH-PX(2).
           MOVE "&l26a1h6d1O(s0p12.8h0b0s6T" TO WE-STG(10).
           MOVE WE-DRU(4) TO WM-DRU.
           IF WM-DRU = 1 MOVE 10 TO WH-P.
           MOVE "TAGSTAT.LST" TO WM-LST.
           PERFORM BEG-DRU.
           MOVE ALL "ƒ" TO DRC-STR PERFORM DRUCK.
           MOVE "Art. B e z e i c h n u n g                       T a g
      -        "e s s t a t i s t i k" TO DRC-STR.
           SET XX TO 115.
           PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE 1 TO WI WX.
       D.  MOVE LA-TAGE(WI) TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRD-DATUM(WX).
           MOVE LA-TGN(WI) TO WO-TGN.
           MOVE "/" TO DRD-ST(WX).
           MOVE WR-TN(WO-TGN) TO DRD-TAG(WX).
           MOVE "≥" TO DRD-BAR(WX).
           IF WI = 14 PERFORM DRUCK
               MOVE 0 TO WX.
           IF WI < 28 ADD 1 TO WI
                      ADD 1 TO WX GO D.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRC-STR PERFORM DRUCK.
           MOVE WT-ADR TO LA-SATZ.
       Z.  EXIT.
      ********************************* Erfassung d. Produktionsmengen *
       PRODME SECTION.
       A.  MOVE " Produktionserfassung" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WH-PG = 3 DISPLAY "Erfassung fÅr Vergleich Backproduktion
      -        "zu Expedit" AT 0301.
           IF WH-PG = 6 DISPLAY "Erfassung der GÑrverzîgererproduktion"
               AT 0301.
           PERFORM DATEIN.
           MOVE WX-DATUM TO WS-DATUM.
           IF ESC GO Z.
           MOVE 604 TO VDU-LP.
           DISPLAY "   Art.:    Menge     Stand    Bezeichnung" AT 0501.
           DISPLAY ALL "ƒ" WITH SIZE 52 AT 0604.
       B.  PERFORM BS-ZEIL.
       C.  CALL "CAUP" USING "1300012480000" WH-CREG.
           DISPLAY "<esc>= Ende, <ret>= Artikel" AT 2301.
           CALL "CAUP" USING "0000043003" WH-CREG.
           IF ESC GO Z.
           IF WH-NUM = 0 GO Z.
           IF FEHLER GO C.
           MOVE WH-NUM TO LA-ARNUM AR-NUM.
       D.  READ ARTIKEL INVALID PERFORM NO-REC GO C.
           IF ZUGRIF PERFORM BESETZT GO D.
           MOVE AR-NUM TO WD-ART.
           MOVE 3 TO VDU-P.
           DISPLAY WD-ART AT VDU-LP.
           MOVE 32 TO VDU-P.
           DISPLAY AR-BEZ AT VDU-LP.
           EVALUATE WH-PG also AR-GVZ
               WHEN 6 also 0 DISPLAY "kein GVZ-Produkt im Pkt. 3 erfasse
      -                         "n" AT 2401
                             PERFORM WEITER GO C
               WHEN 3 also 1 DISPLAY "GVZ-Produkt im Pkt. 6 erfassen"
                                AT 2401
                             PERFORM WEITER GO C.
           PERFORM READLAG.
           IF WH-PG = 6 MOVE LA-MENGEN(33) TO WH-ANZ
                   else MOVE LA-MENGEN(34) TO WH-ANZ.
           PERFORM MG.
           MOVE 21 TO VDU-P.
           DISPLAY WD-MENGE AT VDU-LP.
       F.  DISPLAY "<ret>= Prod. Menge, <esc>= zurÅck" AT 2301.
           MOVE 0 TO WH-WERT.
           IF AR-NK = 0 CALL "CAUP" USING "0000126007" WH-CREG.
           IF AR-NK = 1 CALL "CAUP" USING "0000125107" WH-CREG.
           IF AR-NK = 2 CALL "CAUP" USING "0000124207" WH-CREG.
           IF WOLI OR ESC GO C.
           IF FEHLER GO F.
           MOVE WH-WERT TO WH-ANZ.
           PERFORM DISP-MENGE.
           IF ESC GO F.
           IF AR-NK = 1 MULTIPLY 10 BY WH-ANZ.
           IF AR-NK = 2 MULTIPLY 100 BY WH-ANZ.
           IF WH-PG = 6 ADD WH-ANZ TO LA-MENGEN(33)
                        MOVE LA-MENGEN(33) TO WH-ANZ
                        MOVE WS-DATUM TO LA-PDAT
                   else ADD WH-ANZ TO LA-MENGEN(34)
                        MOVE LA-MENGEN(34) TO WH-ANZ.
           PERFORM MG.
           MOVE 21 TO VDU-P.
           DISPLAY WD-MENGE AT VDU-LP.
           REWRITE LA-SATZ.
           GO B.
       Z.  EXIT.
      ******************************************************************
       READLAG SECTION.
       A.  MOVE 0 TO LA-KZ WX-TASTE.
           READ LAGER INVALID INITIALIZE LA-SATZ
               MOVE AR-NUM TO LA-ARNUM
               WRITE LA-SATZ.
           IF ZUGRIF PERFORM BESETZT GO A.
       Z.  EXIT.
      ******************************************************************
       DATEIN SECTION.
       A.  DISPLAY "Datum:" AT 0360.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0368.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0103686006" WH-CREG.
           IF ESC GO Z.
           IF WZ-DATUM = 0 GO A.
           DISPLAY VDU-DATUM with highlight AT 0368.
       Z.  EXIT.
      ******************************************************************
       VERGL SECTION.
       A.  MOVE "Liefer-/Prod.Vergleich" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM DATEIN.
           IF ESC GO Z.
           DISPLAY "ACHTUNG! " with highlight foreground-color 4 AT 1010
               "Nur zulÑssig wenn "  with highlight foreground-color 3
               "BACKZETTEL " with highlight "fÅr den"  with highlight
                  foreground-color 3
               "nÑchsten Arbeitstag erstellt worden ist!"
                   with highlight foreground-color 3 AT 1110.
           DISPLAY "<ret>= Start, <esc>= Abbruch, <tab>= ohne lîschen "
               AT 2301.
           CALL "CAUP" USING "0023510000" WH-CREG.
           IF ESC GO Z.
           IF TABL MOVE 0 TO WH-MOD
               DISPLAY "ohne lîschen" AT 0460
           else MOVE 1 TO WH-MOD
               DISPLAY "mit lîschen" AT 0460.
           IF NOT RETA GO A.
           PERFORM UEBERN.                    *> öbernahme Auslieferung
           CALL "CAUP" USING "1310012480000" WH-CREG.
           DISPLAY "Unterbrechnung mîglich!" AT 2301.
           MOVE LOW-VALUE TO LA-SATZ.
       C.  READ LAGER INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 0 TO WZ-SEITE WZ-ZEILEN.
           MOVE 1 TO WI.
           OPEN I-O PRODSUMME.
      *---------------------------> Produktionsliste f. ExcelÅbernahme *
           IF WM-KO = 3 OPEN OUTPUT PRDLIST
                        OPEN OUTPUT GVLLIST.
       D.  READ LAGER NEXT WITH NO LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO D.
           PERFORM KB-STAT
               IF ESC GO W.
           IF LA-MENGEN(34) = 0 AND LA-MENGEN(35) = 0
               AND LA-MENGEN(33) = 0 GO D.
           PERFORM VERKO.
           MOVE LA-ARNUM TO AR-NUM.
       E.  READ ARTIKEL WITH NO LOCK INVALID
               MOVE "Artikel fehlt" TO AR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE AR-NUM TO DRE-ARNUM PD-NUM GV-NUM.
           MOVE AR-BEZ TO DRE-BEZ PD-BEZ GV-BEZ.
           MOVE LA-MENGEN(34) TO WH-ANZ.
           IF AR-GVZ = 1 MOVE LA-MENGEN(33) TO LA-MENGEN(34)
                         MOVE WH-ANZ TO LA-MENGEN(33).
           MOVE LA-MENGEN(34) TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MENGE TO DRE-PMG.
           MOVE WH-ANZ TO PD-ANZ.
           MOVE LA-MENGEN(35) TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MENGE TO DRE-LMG.
           DISPLAY DRE-ARNUM AT 1010 " " DRE-BEZ.
           SUBTRACT LA-MENGEN(34) FROM LA-MENGEN(35) GIVING WH-ANZ.
           PERFORM MG.
           IF WH-ANZ NOT = 0 MOVE WD-MENGE TO DRE-DIF.
           MOVE LA-MENGEN(34) TO WH-ANZ.
           PERFORM MG.
           IF WH-MOD = 0 GO F.
           PERFORM PRODSUM.
           MOVE 0 TO LA-MENGEN(34) LA-MENGEN(35)
                     LA-MENGEN(33) LA-PDAT.
           REWRITE LA-SATZ.
      *----------------------------------------------> GVL-Produktion <-
       F.  IF WM-KO = 3 AND WH-ANZ NOT = 0 MOVE WH-ANZ TO GV-ANZ
                             WRITE GV-SATZ.
           IF WM-KO = 3 WRITE PD-SATZ.
           PERFORM DRUCK.
           GO D.
       G.  PERFORM DRUCK.
           MOVE 1 TO WI.
           GO D.
       W.  IF WI = 2 PERFORM DRUCK.
           PERFORM END-DRU.
           MOVE "LPT1" TO WH-DRUNAM.
           CLOSE PRODSUMME.
           IF WM-KO = 3 CLOSE PRDLIST
                        CLOSE GVLLIST.
       Z.  EXIT.
      ************************************ Produktionssummen monatlich *
       PRODSUM SECTION.
       A.  MOVE AR-NUM TO PS-ARNUM.
           MOVE WZ-DATUM(1:4) TO PS-PER.
       C.  READ PRODSUMME INVALID INITIALIZE PS-SATZ
               MOVE AR-NUM TO PS-ARNUM
               MOVE WZ-DATUM(1:4) TO PS-PER.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD LA-MENGEN(34) TO PS-MENGE(WZ-TAG).
           REWRITE PS-SATZ INVALID WRITE PS-SATZ.
       Z.  EXIT.
      ******************************************************************
       VERKO SECTION.
       A.  IF WZ-ZEILEN > 41 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE LA-SATZ TO WT-ADR.
           MOVE WT-BEZ TO LA-TN.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "PRODVER.LST" TO WH-DRUNAM.
           MOVE WE-DRU(4) TO WM-DRU.
           IF WM-DRU = 1 MOVE 1 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "Produktions- und Auslieferungsvergleich" TO DRE-STR.
           SET XX TO 48.
           PERFORM DASEIT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRE-STR.
           PERFORM DRUCK.
           MOVE "Art. Bezeichnung               Produktion   Lieferung
      -        "Differenz" TO DRE-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRE-STR.
           PERFORM DRUCK.
           MOVE WT-ADR TO LA-SATZ.
       Z.  EXIT.
      ********************************** öbernahme d. Tageslieferungen *
       UEBERN SECTION.
       A.  MOVE "  LieferÅbernahmen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           OPEN I-O PROTOK.
           IF WF-STATUS = 94 PERFORM NO-REC GO Z.
       C.  READ PROTOK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF PR-ARNUM = 0 PERFORM OB-VERGLTAG GO C.
           MOVE PR-ARNUM TO LA-ARNUM AR-NUM.
           DISPLAY PR-ARNUM AT 1020.
           PERFORM READLAG.
           ADD PR-MENGE TO LA-MENGEN(35).
           ADD PR-RETMG TO LA-MENGEN(32).
      *-----> LA-DATUM = letzte Addition - autom. Rundumlauflîschung <-
           IF WI < 29;
               IF LA-DATUM = 0 OR LA-DATUM < WV-DATUM
                   MOVE PR-MENGE TO LA-MENGEN(WI)
               else ADD PR-MENGE TO LA-MENGEN(WI)
               END-IF MOVE WV-DATUM TO LA-DATUM.
           MOVE 0 TO PR-MENGE PR-RETMG.
           REWRITE PR-SATZ.
           REWRITE LA-SATZ.
           GO C.
       W.  CLOSE PROTOK.
           OPEN OUTPUT PROTOK.
           CLOSE PROTOK.
       Z.  EXIT.
      ******************************************************************
       OB-VERGLTAG SECTION.
       A.  MOVE PR-MENGE TO WV-DATUM WZ-DATUM.
           MOVE WZ-DATUM TO WV-DATUM.
           IF WZ-JAHR < 09 ADD 2000 WZ-JAHR GIVING WV-JAHR
                      else ADD 1900 WZ-JAHR GIVING WV-JAHR.
           MOVE LOW-VALUE TO LA-KEY.
       B.  READ LAGER INVALID KEY MOVE LOW-VALUE TO LA-SATZ.
           IF ZUGRIF PERFORM BESETZT GO B.
      *-------------------------------------> ob Tagesdatum < LA-JAHR <-
           IF LA-JAHR = 0 OR WV-JAHR > LA-JAHR MOVE WV-JAHR TO WH-NUM
                PERFORM JAHRDURCH.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 28
               MOVE LA-TAGE(WI) TO WF-FTAG(WI).
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 28
               OR WV-DATUM = WF-FTAG(WI) END-PERFORM.
       Z.  EXIT.
