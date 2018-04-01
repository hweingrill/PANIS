      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PAKOSTAM.
      ******************************************* Stammdatenverwaltung *
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PAKOSEC.CPY.
           COPY PARSEART.CPY.
           SELECT DRUCKER    ASSIGN TO PRINTER
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PAKOFD.CPY.
       COPY PARFDART.CPY.
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(134).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WX-PRNO                 PIC 99      COMP-X VALUE ZERO.
           03  WX-PRSTAT               PIC 99      COMP-X.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WH-DRUNAM               PIC X(12).
           03  WD-UST                  PIC Z9,99-.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-BANK                 PIC ZZ9.
           03  WD-BLZ                  PIC 99.999.
           03  WD-PZ                   PIC Z9.
           03  WD-TOL                  PIC 9,9-.
           03  WD-POS                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-KTO                  PIC ZZ999,9-.
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WX-ART.
               05 WH-ART               PIC 99       COMP-X.
           03  WD-ART                  PIC Z99.
           03  WH-PX                   PIC XX        OCCURS 2.
           03  IX                      PIC 99       COMP.
           03  WI                      PIC 99       COMP.
           03  WK-VERDAT               PIC 9(8)     COMP.
           03  WK-VPREIS               PIC S9(7)V99 COMP.
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WD-TG                   PIC ZZ9.
           03  WD-PROZ                 PIC Z9,9.
           03  WX-KZ                   PIC X.
      *------------------------------------------> Bildschirmausgaben <-
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
       COPY PAKOEXT.CPY.
       COPY PAKODECL.CPY.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 12 PERFORM KONDIT
               WHEN 60 MOVE " Stammdatenverwaltung" TO WK-GEB
                       CALL "CAUP" USING "06KOPF" WH-CREG
                       PERFORM STAMVERW.
           MOVE WH-CREG TO WL-CREG.
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
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
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
       A.  IF WM-OPEN > 0 GO C.
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
      ****************************************** Stammdatenverwaltung *
       STAMVERW SECTION.
       A.  CALL "CAUP" USING "0704081746000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Programmwahl " with highlight AT VDU-LP.
       B.  ADD 307 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Komponenten"         AT VDU-LP
                   "2 - Lieferanten"         AT 0014
                   "3 - Mengeneinheiten"     AT 0014
                   "4 - Wirtschaftsjahr"     AT 0014
                   "5 - Kopf / Code"         AT 0014
                   "6 - sonst. Konstanten"   AT 0014
                   "7 - Gruppentexte"        AT 0014
                   "8 - Druckerzuordnung"    AT 0014
                   "9 - Firmenanlage"        AT 0014
                  "10 - Firmenkopf"          AT 0013
                  "11 - Bestelltext"         AT 0013
                   " "                       AT 0043
                   "bitte wÑhlen Sie:   " with highlight AT 0026.
       C.  UNLOCK KONSLAG.
           DISPLAY "<ret-leer/esc/Ende> = Programmende" AT 2301.
           CALL "CAUP" USING "1015372002" WH-CREG.
           IF ESC OR ENDE SET RET TO TRUE
               MOVE 0 TO WH-NUM.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 MOVE 9 TO WH-PG SET ENDE TO TRUE GO Z
               WHEN 1 MOVE 49 TO WH-PG
                      MOVE "PARKOMKA55KOMPO" TO WT-TX GO Z
               WHEN 2 MOVE 49 TO WH-PG
                      MOVE "PAKOKRED20LFTAN" TO WT-TX GO Z
               WHEN 3 PERFORM MENGEN  GO C
               WHEN 4 PERFORM WIJAHR  GO C
               WHEN 7 PERFORM GRUANL  GO C
               WHEN 8 PERFORM DRUANL  GO C
               WHEN 9 PERFORM FI-ANL  GO C
               WHEN 10 PERFORM REKOP  GO C
               WHEN 11 PERFORM BETEXT GO C
               WHEN 5
               WHEN 6 NEXT SENTENCE
               WHEN OTHER GO C.
           MOVE 1 TO WH-KEY.
       D.  READ KONSLAG INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF WH-PG = 13 MOVE WM-DRU TO WK
                         CALL "CADRUCK" USING "00DRUWA" WH-CREG
                         CANCEL "CADRUCK"
                         MOVE WK TO WM-DRU KL-DRU
                         REWRITE KL-SATZ GO C.
           IF WH-PG = 6 GO J.
      *------------------------------> énderg. Firmenkopf u. Codewort <-
       F.  MOVE KL-FIRMA TO WT-TX.
           CALL "CAUP" USING "0201270122" WH-CREG.
           MOVE WT-TX TO KL-FIRMA.
           IF KL-FIRMA = SPACE GO F.
           DISPLAY KL-FIRMA AT VDU-LP.
           CALL "CAUP" USING "0705430528000" WH-CREG.
           DISPLAY " Codewort " with highlight AT 0545.
       G.  DISPLAY "Altes Codewort:" AT 0645.
           DISPLAY KL-SPERRE AT 0663.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           DISPLAY "Neues Codewort:" AT 0745.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1203190106" WH-CREG.
           IF NOT RET GO G.
           MOVE WT-TX TO WX-CODE(1) KL-SPERRE.
       H.  DISPLAY "Kontrolleingabe:" AT 0845.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1204200106" WH-CREG.
           MOVE WT-TX TO WX-CODE(2).
           IF NOT RET GO H.
           IF WX-CODE(2) NOT = WX-CODE(1)
               DISPLAY "Eingaben nicht gleich" AT 0445 GO G.
       I.  REWRITE KL-SATZ.
           UNLOCK KONSLAG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           GO C.
      *-----------------------------------------> sonstige Konstanten <-
       J.  CALL "CAUP" USING "0704010944000" WH-CREG.
           DISPLAY " sonst. Konstanten " with highlight AT 0403.
           DISPLAY "letztes Journal.......:" AT 0503.
           DISPLAY "Verwiegungstoleranz...:" AT 0603.
           DISPLAY "Firmensitz............:" AT 0703.
           DISPLAY "Drucker...............:" AT 0803.
           DISPLAY "COM1 / COM2 ..........:" AT 0903.
           DISPLAY "Journaldruck..........:" AT 1003.
           DISPLAY "Variante..............:" AT 1103.
           MOVE KL-MERK TO WK-M.
           MOVE KL-JONUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT 0527.
           MOVE KL-WTOL TO WD-TOL.
           DISPLAY WD-TOL with highlight AT 0627 "%".
           DISPLAY KL-ORT with highlight AT 0727.
           MOVE KL-DRU TO WD-KZ.
           DISPLAY WD-KZ with highlight AT 0827.
           IF KL-DRU = 0 DISPLAY " Nadel" AT 0829
                   ELSE  DISPLAY " Laser" AT 0829.
           MOVE KL-CHAN TO WD-KZ.
           DISPLAY WD-KZ with highlight AT 0927 "  ".
           IF KL-CHAN = 0 DISPLAY "COM1" AT 0000
                     else DISPLAY "COM2" AT 0000.
           ADD 1029 VDU-ECK GIVING VDU-LP.
           PERFORM KOART.
           ADD 1129 VDU-ECK GIVING VDU-LP.
           MOVE WM-MA TO WD-KZ.
           DISPLAY WD-KZ with highlight AT 1127.
           IF WM-MA = 0 DISPLAY " Netz" AT 1129
                   else DISPLAY " Pc  " AT 1129.
           MOVE 1 TO WX.
       K.  MOVE KL-JONUM TO WH-WERT WD-NUM.
           DISPLAY WD-NUM with highlight AT 0528.
           DISPLAY "<ret-leer>= keine énderung" AT 2301.
           CALL "CAUP" USING "0005285005" WH-CREG.
           IF ESC OR ENDE GO R.
           IF NOT RET GO K.
           MOVE WH-WERT TO KL-JONUM WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
       L.  MOVE KL-WTOL TO WH-WERT.
           DISPLAY "Verwiegungstoleranz (Fehlerprotokoll)" AT 2401.
           CALL "CAUP" USING "0006271103" WH-CREG.
           IF HELP CALL "CAUP" USING "190045004601" WH-CREG GO P.
           IF WOLI GO K.
           IF ESC OR ENDE GO R.
           IF NOT RET GO L.
           MOVE WH-WERT TO KL-WTOL WD-TOL.
           DISPLAY WD-TOL with highlight AT VDU-LP "%".
           ADD 2 TO VDU-P.
       M.  MOVE KL-ORT TO WT-TX.
           CALL "CAUP" USING "0207270115" WH-CREG.
           IF WOLI GO L.
           IF ESC OR ENDE GO R.
           IF NOT RET GO M.
           MOVE WT-TX TO KL-ORT.
           IF KL-ORT = SPACE GO M.
           DISPLAY KL-ORT with highlight AT VDU-LP.
       N.  MOVE KL-DRU TO WH-WERT.
           DISPLAY "0 = Nadel-, 1 = Laserdrucker" AT 2401.
           CALL "CAUP" USING "0008271001" WH-CREG.
           IF HELP CALL "CAUP" USING "190047004801" WH-CREG GO N.
           IF WOLI GO M.
           IF ESC OR ENDE GO R.
           IF NOT RET GO N.
           IF WH-WERT > 1 GO N.
           MOVE WH-WERT TO KL-DRU WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 2 TO VDU-P.
           IF KL-DRU = 0 DISPLAY " Nadel" AT VDU-LP
                    else DISPLAY " Laser" AT VDU-LP.
       O.  MOVE KL-CHAN TO WH-WERT.
           DISPLAY "0 = COM1, 1 = COM2" AT 2401.
           CALL "CAUP" USING "0009271001" WH-CREG.
           IF HELP CALL "CAUP" USING "190000000001" WH-CREG GO O.
           IF WOLI GO N.
           IF ESC OR ENDE GO R.
           IF NOT RET GO O.
           IF WH-WERT > 1 GO O.
           MOVE WH-WERT TO KL-CHAN WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP "  ".
           IF KL-CHAN = 0 DISPLAY "COM1" AT 0000
                     else DISPLAY "COM2" AT 0000.
       P.  MOVE WM-KO TO WH-WERT.
           DISPLAY "Journalpapier: 0 = 8-Zoll, 1 = 12-Zoll" AT 2401.
           CALL "CAUP" USING "0010271001" WH-CREG.
           IF HELP CALL "CAUP" USING "190000000001" WH-CREG GO P.
           IF WOLI GO P.
           IF ESC OR ENDE GO R.
           IF NOT RET OR WH-WERT > 1 GO P.
           MOVE WH-WERT TO WM-KO.
           PERFORM KOART.
       Q.  MOVE WM-MA TO WH-WERT.
           DISPLAY "0 = Netz, 1 = Pc" AT 2401.
           CALL "CAUP" USING "0011271001" WH-CREG.
           IF HELP CALL "CAUP" USING "190047004801" WH-CREG GO N.
           IF WOLI GO M.
           IF ESC OR ENDE GO R.
           IF NOT RET GO N.
           IF WH-WERT > 1 GO N.
           MOVE WH-WERT TO WM-MA WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 2 TO VDU-P.
           IF WM-MA = 0 DISPLAY " Netz" AT VDU-LP
                   else DISPLAY " Pc  " AT VDU-LP.
       R.  DISPLAY "<ret>= speichern, <>= zurÅck, <esc>= Abbruch "
               AT 2301.
           CALL "CAUP" USING "0023471001" WH-CREG.
           IF ESC UNLOCK KONSLAG GO R1.
           IF WOLI GO P.
           IF NOT RET GO R.
           MOVE WK-M TO KL-MERK.
           REWRITE KL-SATZ.
       R1. CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
           GO C.
       Z.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           EXIT.
      ****************************************** Kopf bei Nadeldrucker *
       NADEL-KOPF SECTION.
       A.  MOVE X"1B21" TO DRA-SATZ(1:).
           EVALUATE WH-KEY
               WHEN 2 MOVE X"3B00" TO DRA-SATZ(3:2)
               WHEN 3 MOVE X"3100" TO DRA-SATZ(3:2)
               WHEN 4 MOVE X"3100" TO DRA-SATZ(3:2)
               WHEN 5 MOVE X"0100" TO DRA-SATZ(3:2).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           IF WH-KEY = 5 MOVE KL-KOPF TO DRA-SATZ(6:)
                    else MOVE KL-KOPF TO DRA-SATZ(4:).
       Z.  EXIT.
      ************************************************ ob Druckteilung *
       OB-TEILG SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 80
              IF DRA-SATZ(WX:1) = "*" MOVE DRA-SATZ(WX:) TO WT-TX
                  IF WM-DRU = 1 MOVE "(s25v0s3b4101T" TO DRA-SATZ(WX:)
                                ADD 15 TO WX
                  end-if MOVE WT-TX(2:) TO DRA-SATZ(WX:)
                         MOVE 84 TO WX.
       Z.  EXIT.
      ****************************************** Kopf bei Laserdrucker *
       LASER-KOPF SECTION.
       A.  MOVE 0 TO WZ-SCHALT.
           EVALUATE WH-KEY
               WHEN 2 MOVE "  (s1p18v4s3b4101T" TO DRA-SATZ
                      PERFORM DRUCK
                      MOVE KL-KOPF TO DRA-SATZ(9:)
                      PERFORM OB-TEILG
                      MOVE 0 TO WZ-SCHALT
               WHEN 3 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KL-KOPF TO DRA-SATZ(9:)
               WHEN 4 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KL-KOPF TO DRA-SATZ(9:)
               WHEN 5 MOVE "  (s0p0s0b0T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KL-KOPF TO DRA-SATZ(14:).
       Z.  EXIT.
      ************************************************** Rechnungskopf *
       REKOP SECTION.
       S.  CALL "CAUP" USING "0707010680000" WH-CREG.
           DISPLAY " Rechnungskopf * 4 Zeilen " with highlight
               foreground-color 7 reverse-video AT 0703.
           DISPLAY "<esc>= Abbruch" AT 2301.
           MOVE 2 TO WH-KEY.
       T.  READ KONSLAG IGNORE LOCK INVALID MOVE SPACE TO KL-SATZ
               MOVE WH-KEY TO KL-NUM
               WRITE KL-SATZ.
           COMPUTE VDU-REL = WH-KEY * 100 + 2 + VDU-ECK.
           DISPLAY KL-KOPF WITH SIZE 78 HIGHLIGHT AT VDU-REL.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO T.
           MOVE 2 TO WH-KEY.
       U.  READ KONSLAG.
           IF ZUGRIF PERFORM BESETZT GO U.
           COMPUTE VDU-L = WH-KEY + 6.
           MOVE KL-KOPF TO WT-TX.
           CALL "CAUP" USING "0200020178" WH-CREG.
           IF ESC GO Y.
           MOVE WT-TX TO KL-KOPF.
           REWRITE KL-SATZ.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO U.
       V.  DISPLAY "<ret>= weiter, <tab>= Testdruck " AT 2301.
           CALL "CAUP" USING "0023331001" WH-CREG.
           IF RET GO Y.
           IF NOT TABL GO V.
           MOVE WE-DRU(1) TO WM-DRU.
           MOVE "LPT1" TO WH-DRUNAM.
           IF WM-OPEN = 0 MOVE X"0100" TO WH-PX(1)
               MOVE X"000C" TO WH-PX(2)
               IF WM-DRU = 1 MOVE 5 TO WH-P
               end-if
               PERFORM BEG-DRU.
           MOVE 2 TO WH-KEY.
       W.  READ KONSLAG IGNORE LOCK.
           EVALUATE WM-DRU
               WHEN 0 PERFORM NADEL-KOPF
               WHEN 1 PERFORM LASER-KOPF.
           PERFORM DRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO W.
           PERFORM END-DRU.
           MOVE 2 TO WH-KEY.
           GO T.
       Y.  PERFORM END-DRU.
           REWRITE KL-SATZ.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************************** Bestelltext *
       BETEXT SECTION.
       A.  CALL "CAUP" USING "0707050564000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bestelltext * 3 Zeilen " with highlight
               foreground-color 7 reverse-video AT VDU-LP.
           DISPLAY "<esc>= Abbruch" AT 2301.
           MOVE 20 TO WH-KEY.
       C.  READ KONSLAG INVALID MOVE SPACE TO KL-SATZ
               MOVE WH-KEY TO KL-NUM
               WRITE KL-SATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 3
               COMPUTE VDU-REL = WX * 100 + 103 + VDU-ECK
               DISPLAY KL-BEST(WX) with highlight AT VDU-REL.
           MOVE 1 TO WX.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Text" AT 2301.
           COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK.
           DISPLAY KL-BEST(WX) with highlight AT VDU-LP.
           MOVE KL-BEST(WX) TO WT-TX.
           COMPUTE VDU-LP = WX * 100 + 103.
           CALL "CAUP" USING "1200000160" WH-CREG.
           IF ESC GO X.
           MOVE WT-TX TO KL-BEST(WX).
           IF WX < 3 ADD 1 TO WX GO E.
       Q.  DISPLAY "<ret>= weiter, < />= zurÅck < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF WOLI or AUF MOVE 1 TO WX GO E.
           IF not RET GO Q.
           REWRITE KL-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
       Z.  EXIT.
      ******************************************************************
       KOART SECTION.
       A.  MOVE WM-KO TO WD-KZ.
           ADD 0727 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 2 TO VDU-LP.
           EVALUATE WM-KO
               WHEN 0 DISPLAY " 8-Zoll " AT VDU-LP
               WHEN 1 DISPLAY " 12-Zoll" AT VDU-LP.
       Z.  EXIT.
      ************************************************ Wirtschaftsjahr *
       WIJAHR SECTION.
       A.  MOVE 9 TO WH-KEY.
           MOVE 0 TO WH-NUM.
           READ KONSLAG INVALID KEY INITIALIZE KL-TSATZ
               WRITE KL-SATZ.
           IF ZUGRIF PERFORM BESETZT GO Z.
           CALL "CAUP" USING "0706200729010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Wirtschaftsjahre " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " WKz.    Beginn      Ende  " with reverse-video
               AT VDU-LP.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 4
               OR KL-WKZ(WI) = 0
                   MOVE KL-WKZ(WI) TO WD-PZ
                   COMPUTE VDU-LP = WI * 100 + 204 + VDU-ECK
                   DISPLAY WD-PZ with highlight AT VDU-LP
                   ADD 5 TO VDU-LP
                   MOVE KL-WBEG(WI) TO WC-DATUM
                   CALL "CAUP" USING "04DATDREH" WH-CREG
                   DISPLAY VDU-DATUM with highlight AT VDU-LP
                   ADD 10 TO VDU-LP
                   MOVE KL-WEND(WI) TO WC-DATUM
                   CALL "CAUP" USING "04DATDREH" WH-CREG
                   DISPLAY VDU-DATUM with highlight AT VDU-LP.
       C.  DISPLAY "<Entf>= Eintrag lîschen" AT 2401.
           IF WI = 5 MOVE 4 TO WI.
           IF WI = 4 AND KL-WKZ(4) NOT = 0
                DISPLAY ", <Einfg>= neues Jahr einfÅgen" AT 0000.
           DISPLAY "<ret>= Jahr, <esc>= Abbruch, <>= zurÅck, <Ende>= En
      -        "de mit speichern" AT 2301.
           MOVE KL-WKZ(WI) TO WH-WERT.
           COMPUTE VDU-LP = WI * 100 + 204.
           IF WH-NUM NOT = 0 MOVE WH-NUM TO WH-WERT
               ADD VDU-ECK TO VDU-LP GO D.
           CALL "CAUP" USING "1000042002" WH-CREG.
           IF ENTF MOVE 0 TO KL-WKZ(WI) KL-WBEG(WI) KL-WEND(WI)
               IF WI > 1 ADD -1 TO WI GO B.
           IF WOLI AND WI > 1 ADD -1 TO WI GO C.
           IF ESC GO X.
           IF ENDE GO W.
           IF EINF AND WI = 4
               PERFORM VARYING WI FROM 2 BY 1 UNTIL WI = 5
                   MOVE KL-WKZ(WI) TO KL-WKZ(WI - 1)
                   MOVE KL-WBEG(WI) TO KL-WBEG(WI - 1)
                   MOVE KL-WEND(WI) TO KL-WEND(WI - 1)
                   MOVE 0 TO KL-WKZ(WI) KL-WBEG(WI) KL-WEND(WI)
               END-PERFORM GO B.
           IF NOT RET GO C.
       D.  MOVE WH-WERT TO KL-WKZ(WI) WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
           MOVE KL-WBEG(WI) TO WK-VERDAT.
       F.  MOVE KL-WBEG(WI) TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE KL-WBEG(WI) TO WZ-DATUM WH-WERT.
           DISPLAY "<ret>= Beginndatum, <esc>= Abbruch" AT 2301.
           DISPLAY "Achtung!" with highlight AT 2401 " énderung kann fal
      -        "sche Auswertungen bewirken!".
           COMPUTE VDU-LP = WI * 100 + 209.
           CALL "CAUP" USING "1100096006" WH-CREG.
           IF ESC GO X.
           IF WOLI GO C.
           IF NOT RET GO F.
           IF WZ-DATUM = 0 GO F.
           MOVE WX-DATUM TO KL-WBEG(WI).
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF KL-WBEG(WI) NOT = WK-VERDAT MOVE 0 TO KL-WEND(WI).
       H.  PERFORM BILDJAHR.
           DISPLAY "<ret>= Endedatum, <>= zurÅck, <esc>= Abbruch"
               AT 2301.
           DISPLAY "Achtung!" with highlight AT 2401 " énderung kann fal
      -        "sche Auswertungen bewirken!".
           COMPUTE VDU-LP = WI * 100 + 219.
           CALL "CAUP" USING "1100196006" WH-CREG.
           IF WOLI GO F.
           IF ESC GO X.
           IF NOT RET GO H.
           IF WZ-DATUM = 0 GO H.
           MOVE WX-DATUM TO KL-WEND(WI).
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF KL-WEND(WI) NOT > KL-WBEG(WI) DISPLAY "Daten fehlerhaft"
               AT 2401 PERFORM WEITER GO F.
           IF WI < 4 ADD 1 TO WI.
           GO C.
       W.  REWRITE KL-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
       Z.  EXIT.
      ************************************* Bilde Ende Wirtschaftsjahr *
       BILDJAHR SECTION.
       A.  IF KL-WEND(WI) NOT = 0 GO X.
           MOVE KL-WBEG(WI) TO WZ-DATUM.
           ADD -1 TO WZ-MONAT.
           IF WZ-MONAT = 0 MOVE 12 TO WZ-MONAT
           ELSE ADD 1 TO WZ-JAHR.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE 32 TO WZ-TAG.
       C.  ADD -1 TO WZ-TAG.
           MOVE WZ-DATUM TO WK-VERDAT.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE WK-VERDAT TO WZ-DATUM GO C.
           MOVE WZ-DATUM TO WK-VERDAT WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           COMPUTE VDU-LP = WI * 100 + 219 + VDU-ECK.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WM-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WK-VERDAT TO WZ-DATUM WH-WERT.
           GO Z.
       X.  MOVE KL-WEND(WI) TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE KL-WEND(WI) TO WZ-DATUM WH-WERT.
       Z.  EXIT.
      ********************************************************* Anrede *
       ANR SECTION.
       A.  MOVE 1 TO WH-KEY.
           READ KONSLAG INVALID GO Y.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0706601117020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Anredetexte " with highlight AT VDU-REL.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr. T e x t e" AT VDU-REL.
           ADD 99 TO VDU-REL.
           DISPLAY "---------------" AT VDU-REL.
           MOVE 1 TO WX.
       F.  COMPUTE VDU-REL = 304 + WX * 100 + VDU-ECK.
           SUBTRACT 1 FROM WX GIVING WD-KZ.
           DISPLAY WD-KZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY WT-TXT(WX) with highlight AT VDU-REL.
           IF WX < 7 ADD 1 TO WX GO F.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Eingabe, <esc>= Abbruch, <Ende>= Ende, <>= z
      -        "urÅck" AT 2301.
           COMPUTE VDU-LP = WX * 100 + 307.
           MOVE WT-TXT(WX) TO WT-TX.
           CALL "CAUP" USING "1200000109" WH-CREG.
           IF ENDE GO L.
           IF ESC GO Y.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF NOT RET GO H.
           MOVE WT-TX TO WT-TXT(WX).
           COMPUTE VDU-REL = 307 + WX * 100 + VDU-ECK.
           DISPLAY WT-TXT(WX) with highlight AT VDU-REL.
           IF WX < 7 ADD 1 TO WX.
           GO H.
       L.  MOVE 1 TO WX.
           MOVE WH-TABTX TO KL-ANR.
           REWRITE KL-SATZ.
       X.  MOVE KL-AN(WX) TO WT-TXT(WX).
           IF WX < 7 ADD 1 TO WX GO X.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
       Z.  EXIT.
      ************************************* Druckereinstellung im Netz *
       DRUPAR SECTION.
       A.  CALL "CAUP" USING "0710100550000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druckerauswahl " with highlight AT VDU-LP.
           MOVE 11 TO WH-KEY.
       C.  READ KONSLAG INVALID MOVE SPACE TO KLD-APAR.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 11 TO KL-NUM.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY KLD-APAR with highlight AT VDU-LP.
       E.  MOVE KLD-APAR TO WT-TX.
           CALL "CAUP" USING "1322012480" WH-CREG.
           DISPLAY "<ret>= Druckernummer oder Befehl" AT 2301.
           DISPLAY "1: 1. Netzdrucker, 2: 2. Netzdrucker, 0: kein Netzdr
      -        "ucker" AT 2301.
           CALL "CAUP" USING "1203030145" WH-CREG.
           IF WT-TX(2:44) = SPACE; IF WR-TX(1) = 0 MOVE SPACE TO WT-TX
               ELSE IF WR-TX(1) = 1 OR 2
               MOVE "CAPTURE P=0 Q=PRINTQ_0 NB TI=0 L=1 NFF" TO KLD-APAR
               IF WR-TX(1) = 2 MOVE 1 TO KLD-APAR(11:1) KLD-APAR(22:1)
               END-IF MOVE KLD-APAR TO WT-TX.
           IF NOT RET AND NOT KIST GO E.
           MOVE WT-TX TO KLD-APAR.
           IF KLD-APAR = SPACE DISPLAY "Druck auf Arbeitsplatzdrucker"
               with highlight AT VDU-LP
           ELSE DISPLAY KLD-APAR with highlight AT VDU-LP.
       G.  DISPLAY "<ret>= speichern, <>= zurÅck, <esc>= Abbruch"
               AT 2301.
           CALL "CAUP" USING "0023470000" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RET GO G.
           REWRITE KL-SATZ INVALID WRITE KL-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
       Z.  EXIT.
      ************************************************* Rechnungstexte *
       DRUANL SECTION.
       A.  CALL "CAUP" USING "0709081234020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druckerverzeichnis " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Nr. Druckerbezeichnung         " with reverse-video
                AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY ALL "ƒ" with SIZE 32 AT VDU-LP.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
      *---------------------------------------------------> auflisten <-
           MOVE 1 TO WX WR WF.
       C.  ADD 11 WX GIVING WH-KEY.
           MOVE 1 TO WF.
           READ KONSLAG WITH NO LOCK INVALID INITIALIZE KL-PRSATZ
               MOVE 0 TO WF.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF ENTF SET RET TO TRUE
                   DELETE KONSLAG INVALID GO B
                   END-DELETE GO B.
           COMPUTE VDU-LP = 303 + WX * 100 + VDU-ECK.
           MOVE WX TO WD-KZ.
           DISPLAY WD-KZ AT VDU-LP.
           ADD 3 TO VDU-LP.
           DISPLAY KOP-BEZ with highlight AT VDU-LP.
           IF WR = 1; IF WX < 8 ADD 1 TO WX GO C.
           IF WR = 2 GO E
                     ELSE MOVE 2 TO WR.
       D.  DISPLAY "<esc/Ende>= Ende, Nr.+<Entf>= lîschen, <ret>= Nr.:"
               AT 2301.
           CALL "CAUP" USING "0023521001" WH-CREG.
           IF ESC OR ENDE GO X.
           IF NOT RET AND NOT ENTF OR WH-NUM = 0 GO D.
           IF WH-NUM > 8 GO D.
           MOVE WH-NUM TO WX.
           GO C.
       E.  PERFORM DRUSTG.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
       Z.  EXIT.
      *********************************************** Druckerparameter *
       DRUSTG SECTION.
       A.  CALL "CAUP" USING "0706071566000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druckerparameter " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:     Bezeichnung: " AT VDU-LP.
           MOVE WX TO WD-KZ.
           ADD 5 TO VDU-P.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       C.  ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY KOP-BEZ with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK
               EVALUATE WX
                   WHEN 1 DISPLAY "10-Zeichen/Zoll....:" AT VDU-LP
                   WHEN 2 DISPLAY "12-Zeichen/Zoll....:" AT VDU-LP
                   WHEN 3 DISPLAY "15-Zeichen/Zoll....:" AT VDU-LP
                   WHEN 4 DISPLAY "12-Zoll Formular...:" AT VDU-LP
                   WHEN 5 DISPLAY " 8-Zoll Formular...:" AT VDU-LP
                   WHEN 6 DISPLAY "Breitdruck ein.....:" AT VDU-LP
                   WHEN 7 DISPLAY "Breitdruck aus.....:" AT VDU-LP
                   WHEN 8 DISPLAY "Fettdruck ein......:" AT VDU-LP
                   WHEN 9 DISPLAY "Fettdruck aus......:" AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 6
                   MOVE KOP-STG(WX)(WY:1) TO WX-ART
                   COMPUTE VDU-LP = WX * 100 + 220 + VDU-ECK + WY * 4
                   IF WH-ART NOT = 32 MOVE WH-ART TO WD-ART
                   DISPLAY WD-ART with highlight AT VDU-LP.
      *---------------------------------> erstmalig anlegen f. EPSON <-
           IF WF = 0 MOVE SPACE TO KOP-TX
               MOVE "Epson LQ1050+" TO KOP-BEZ
               MOVE x"1B50" TO KOP-STG(1)
               MOVE x"1B4D" TO KOP-STG(2)
               MOVE x"1B57" TO KOP-STG(3)
               MOVE x"1B43000C" TO KOP-STG(4)
               MOVE x"1B430008" TO KOP-STG(5)
               MOVE x"1B5701" TO KOP-STG(6)
               MOVE x"1B5700" TO KOP-STG(7)
               MOVE x"1B45" TO KOP-STG(8)
               MOVE x"1B46" TO KOP-STG(9)
               MOVE 1 TO WF
               WRITE KL-SATZ GO C.
           ADD 1303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Geben Sie die einzelnen Steuerzeichen lt. Druckerhan
      -        "dbuch ein." AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Verwenden Sie jedoch nur die Dezimalzahlen der Steue
      -        "rzeichen!" AT VDU-LP.
       D.  DISPLAY "<esc>= Abbruch, <ret>= Bezeichnung, <ende> = Ende"
               AT 2301.
           MOVE KOP-BEZ TO WT-TX.
           ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY KOP-BEZ with highlight AT VDU-LP.
           CALL "CAUP" USING "1202250126" WH-CREG.
           IF ESC GO M.
           IF ENDE GO K.
           IF NOT RET GO D.
           MOVE WT-TX TO KOP-BEZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 6
                   COMPUTE VDU-LP = WX * 100 + 220 + VDU-ECK + WY * 4
                   MOVE KOP-STG(WX)(WY:1) TO WX-ART
                   MOVE WH-ART TO WD-ART WH-WERT
                   IF WH-ART = 32 DISPLAY "    " AT VDU-LP
                   ELSE DISPLAY WD-ART with highlight AT VDU-LP
                   END-IF
                   DISPLAY "<ende>= Ende, <tab>= lîschen, <>= zurÅck, <
      -                "esc>= Abbruch" AT 2301
                   CALL "CAUP" USING "0000003003" WH-CREG
                   EVALUATE TRUE
                   WHEN TABL DISPLAY "    " AT VDU-LP
                       MOVE SPACE TO WX-ART
                       MOVE WX-ART TO KOP-STG(WX)(WY:1)
                   WHEN RET MOVE WH-WERT TO WH-ART WD-ART
                       MOVE WX-ART TO KOP-STG(WX)(WY:1)
                       IF WH-ART = 32 DISPLAY " " AT VDU-LP
                       ELSE DISPLAY WD-ART with highlight AT VDU-LP
                   WHEN WOLI; IF WY > 1 SUBTRACT 2 FROM WY
                           ELSE MOVE 11 TO WY
                           IF WX > 1 SUBTRACT 2 FROM WX
                           ELSE SUBTRACT 1 FROM WX
                   WHEN ENDE GO K
                   WHEN ESC  GO M
                   WHEN HELP CALL "CAUP" USING "190061006201" WH-CREG
                        SUBTRACT 1 FROM WY
                   WHEN OTHER SUBTRACT 1 FROM WY
                 END-PERFORM
               END-PERFORM.
       K.  REWRITE KL-SATZ.
       M.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSLAG.
       Z.  EXIT.
     ********************************************* Standardkonditionen *
       KONDIT SECTION.
       A.  CALL "CAUP" USING "0702021332000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Standardkonditionen " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr. Tage   %  Tage   %  Netto" AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL "ƒ" with SIZE 30 AT VDU-LP.
           MOVE 1 TO WH-KEY.
       C.  READ KONSLAG INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
               COMPUTE VDU-LP = IX * 100 + 303 + VDU-ECK
               MOVE IX TO WD-KZ
               DISPLAY WD-KZ with highlight AT VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
               MOVE KL-KONDIT(IX) TO WH-KK
               PERFORM KONDIS.
       E.  DISPLAY "<esc>= Abbruch, <ret-leer>= speichern, Nr+<ret>= Zei
      -        "e fÅr énderg. < >" AT 2301.
           CALL "CAUP" USING "0023681001" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           IF WH-NUM = 0 GO W.
           MOVE WH-NUM TO IX.
           MOVE KL-KONDIT(IX) TO WH-KK.
           PERFORM KONDIS.
       G.  COMPUTE VDU-LP = IX * 100 + 307.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "1000073002" WH-CREG.
           IF ESC OR WOLI GO E.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-TG1.
           PERFORM KONDIS.
       I.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "1000112103" WH-CREG.
           IF WOLI GO G.
           IF ESC GO E.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-SK1.
           PERFORM KONDIS.
       K.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-TG2 TO WH-WERT.
           CALL "CAUP" USING "1000173002" WH-CREG.
           IF WOLI GO I.
           IF ESC GO E.
           IF NOT RET GO K.
           MOVE WH-WERT TO WH-TG2.
           PERFORM KONDIS.
       M.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-SK2 TO WH-WERT.
           CALL "CAUP" USING "1000212103" WH-CREG.
           IF WOLI GO K.
           IF ESC GO E.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-SK2.
           PERFORM KONDIS.
       O.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "1000283002" WH-CREG.
           IF WOLI GO M.
           IF ESC GO E.
           IF NOT RET GO O.
           MOVE WH-WERT TO WH-TGN.
           PERFORM KONDIS.
           MOVE WH-KK TO KL-KONDIT(IX).
           IF IX < 9 ADD 1 TO IX.
           GO E.
       W.  REWRITE KL-SATZ.
           MOVE KL-TABK TO WH-TABK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KONDIS SECTION.
       A.  COMPUTE VDU-LP = IX * 100 + 307 + VDU-ECK.
           MOVE WH-TG1 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 4 TO VDU-LP.
           MOVE WH-SK1 TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT VDU-LP.
           ADD 6 TO VDU-LP.
           MOVE WH-TG2 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 4 TO VDU-LP.
           MOVE WH-SK2 TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT VDU-LP.
           ADD 7 TO VDU-LP.
           MOVE WH-TGN TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       Z.  EXIT.
      *****************************************************************
       MENGEN SECTION.
       A.  IF WL-CA = 22 CALL "CAUP" USING "0702441434020" WH-CREG
                    else CALL "CAUP" USING "0705401434020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Mengeneinheiten " with highlight AT VDU-REL.
           MOVE 10 TO WH-KEY.
       B.  READ KONSLAG INVALID KEY GO X.
           IF ZUGRIF PERFORM BESETZT GO B.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.  Text        Nr.  Text   " AT VDU-REL.
           ADD 0303 VDU-ECK GIVING VDU-REL.
           DISPLAY "-----------------------------" AT VDU-REL.
           MOVE 0 TO WH-ART.
       C.  ADD 1 WH-ART GIVING WX.
           ADD 03 VDU-EL WX GIVING VDU-L.
           IF WX < 11 ADD 02 VDU-EP GIVING VDU-P
                 ELSE ADD 19 VDU-EP GIVING VDU-P
                      SUBTRACT 10 FROM VDU-L.
           MOVE WH-ART TO WD-POS.
           DISPLAY WD-POS AT VDU-LP.
           ADD 6 TO VDU-P.
           DISPLAY KL-MEH(WX) with highlight AT VDU-LP.
           ADD 1 TO WH-ART.
           IF WX < 20 ADD 1 TO WX GO C.
           MOVE 1 TO WX.
       D.  IF WL-CA = 22 GO Z.
           DISPLAY "  = auf,   = ab, <esc>= Ende, <ret>= Meh" AT 2301.
           ADD 03 WX GIVING VDU-RL.
           IF WX < 11 MOVE 08 TO VDU-RP
                 ELSE MOVE 25 TO VDU-RP
                      SUBTRACT 10 FROM VDU-RL.
           MOVE VDU-REL TO VDU-LP.
           MOVE KL-MEH(WX) TO WT-TX.
           CALL "CAUP" USING "1200000103" WH-CREG.
           MOVE WT-TX TO KL-MEH(WX).
           IF ESC GO K.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO D.
           IF AB  GO I.
           IF NOT RET GO D.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY KL-MEH(WX) with highlight AT VDU-LP.
       I.  IF WX < 20 ADD 1 TO WX GO D.
       K.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO K.
           REWRITE KL-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************************** Gruppentexte *
       GRUANL SECTION.
       A.  CALL "CAUP" USING "0703131235020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Artikelgruppen " with highlight AT VDU-REL.
           ADD 203 TO VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.Bezeichnung" AT VDU-REL.
           ADD 302 TO VDU-ECK GIVING VDU-REL.
           DISPLAY ALL "ƒƒ" with SIZE 33 AT VDU-REL.
           MOVE 1 TO WX.
       C.  DISPLAY "<esc>= Ende, < >= Hilfe, < >= Druck, <tab>= anzeigen
      -        ", <ret>= Art-Grp." AT 2301.
           DISPLAY "1 - 33 mîglich" with highlight AT 2401.
           COMPUTE VDU-LP = 303 + WX * 100.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF HELP CALL "CAUP" USING "190093009401" WH-CREG GO C.
           IF ESC GO X.
           IF RET; IF WH-NUM = 0 GO C.
           IF WH-NUM > 30 GO C.
           IF NOT RETA GO C.
           IF NOT TABL GO J.
       D.  MOVE 0 TO WR.
           SUBTRACT 1 FROM WX.
           CALL "CAUP" USING "1500002002" WH-CREG.
           IF WH-NUM = 0 MOVE 1 TO WH-NUM.
           ADD WH-NUM 20 GIVING WH-KEY.
      *----------------------------------------------------> anzeigen <-
           START KONSLAG KEY NOT < WH-KEY INVALID KEY GO H.
       F.  ADD 1 TO WX.
           READ KONSLAG NEXT WITH NO LOCK AT END GO C.
           IF ZUGRIF PERFORM BESETZT GO F.
           IF WH-KEY > 59 GO C.
           ADD 1 TO WR.
           IF WR NOT = 8 GO H.
       G.  IF FINE GO C.
           DISPLAY "<esc>= Ende , <tab>= weiterlisten, <ret>= Nummer"
               AT 2301.
           COMPUTE VDU-LP = 303 + WX * 100.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF NOT RETA GO G.
           MOVE 1 TO WR.
           IF RET AND WH-NUM NOT = 0 GO J.
           CALL "CAUP" USING "1500002002" WH-CREG.
           IF WH-NUM NOT = 0 GO D.
       H.  IF WX = 8 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 7 TO WX.
           MOVE KL-NUM TO WD-PZ.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KL-GRPTX with highlight AT VDU-REL.
           GO F.
       J.  MOVE WH-NUM TO WD-PZ KL-NUM.
           ADD 20 WH-NUM GIVING WH-KEY.
           READ KONSLAG INVALID INITIALIZE KL-GSATZ.
           IF ZUGRIF PERFORM BESETZT GO J.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KL-GRPTX with highlight AT VDU-REL.
           DISPLAY "<ret-leer>= wiederholen, kein Text = lîschen"
               AT 2301.
           MOVE KL-GRPTX TO WT-TX.
           COMPUTE VDU-LP = 306 + WX * 100.
           CALL "CAUP" USING "1200000125" WH-CREG.
           IF HELP CALL "CAUP" USING "190095009601" WH-CREG GO F.
           IF ESC GO C.
           MOVE WT-TX TO KL-GRPTX.
           IF KL-GRPTX NOT = SPACE GO P.
           DELETE KONSLAG INVALID KEY GO C.
           GO C.
       P.  SUBTRACT 20 FROM WH-KEY GIVING KL-NUM.
           ADD 1 TO WX.
           IF WX > 8 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 8 TO WX.
           WRITE KL-SATZ INVALID REWRITE KL-SATZ.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************************** Firmentexte *
       FI-ANL SECTION.
       A.  CALL "CAUP" USING "0703131235020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Firmentexte " with highlight AT VDU-REL.
           ADD 203 TO VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.Bezeichnung" AT VDU-REL.
           ADD 302 TO VDU-ECK GIVING VDU-REL.
           DISPLAY ALL "ƒƒ" with SIZE 33 AT VDU-REL.
           MOVE 1 TO WX.
       C.  DISPLAY "<esc>= Ende, <tab>= anzeigen, <ret>= Firma"
               AT 2301.
           DISPLAY "1 - 9 mîglich" with highlight AT 2401.
           COMPUTE VDU-LP = 303 + WX * 100.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF RET; IF WH-NUM = 0 GO C.
           IF WH-NUM > 10 GO C.
           IF NOT RETA GO C.
           IF NOT TABL GO J.
       D.  MOVE 0 TO WR.
           SUBTRACT 1 FROM WX.
           CALL "CAUP" USING "1500002002" WH-CREG.
           ADD WH-NUM 120 GIVING WH-KEY.
      *----------------------------------------------------> anzeigen <-
           START KONSLAG KEY NOT < WH-KEY INVALID KEY GO H.
       F.  ADD 1 TO WX.
           READ KONSLAG NEXT WITH NO LOCK AT END GO C.
           IF ZUGRIF PERFORM BESETZT GO F.
           IF WH-KEY > 131 GO C.
           ADD 1 TO WR.
           IF WR NOT = 8 GO H.
       G.  IF FINE GO C.
           DISPLAY "<esc>= Ende , <tab>= weiterlisten, <ret>= Nummer"
               AT 2301.
           COMPUTE VDU-LP = 303 + WX * 100.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF NOT RETA GO G.
           MOVE 1 TO WR.
           IF RET AND WH-NUM NOT = 0 GO J.
           CALL "CAUP" USING "1500002002" WH-CREG.
           IF WH-NUM NOT = 0 GO D.
       H.  IF WX = 8 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 7 TO WX.
           MOVE KL-NUM TO WD-PZ.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KL-FIBEZ with highlight AT VDU-REL.
           GO F.
       J.  MOVE WH-NUM TO WD-PZ KL-NUM.
           ADD 120 WH-NUM GIVING WH-KEY.
           READ KONSLAG INVALID INITIALIZE KL-GSATZ.
           IF ZUGRIF PERFORM BESETZT GO J.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KL-FIBEZ with highlight AT VDU-REL.
           DISPLAY "<ret-leer>= wiederholen, kein Text = lîschen"
               AT 2301.
           MOVE KL-FIBEZ TO WT-TX.
           COMPUTE VDU-LP = 306 + WX * 100.
           CALL "CAUP" USING "1200000122" WH-CREG.
           IF HELP CALL "CAUP" USING "190095009601" WH-CREG GO F.
           IF ESC GO C.
           MOVE WT-TX TO KL-FIBEZ.
           IF KL-FIBEZ NOT = SPACE GO P.
           DELETE KONSLAG INVALID KEY GO C.
           GO C.
       P.  SUBTRACT 120 FROM WH-KEY GIVING KL-NUM.
           ADD 1 TO WX.
           IF WX > 8 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 8 TO WX.
           WRITE KL-SATZ INVALID REWRITE KL-SATZ.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
