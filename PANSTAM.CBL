      $SET LINKCOUNT"384" ANS85 BOUND AUTOLOCK NOALTER
      $SET SEGSIZE "50000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANSTAM.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       OBJECT-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEKON.CPY.
           COPY PANSEART.CPY.
           COPY PARSEREZ.CPY.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
       COPY PANFDART.CPY.
       COPY PARREZEP.CPY.
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(134).
       01  DRB-SATZ.
           03  FILLER                  PIC X(6).
           03  DRB-TX.
               05 DRB-FIL              PIC X(116).
       01  DRC-SATZ.
           03  FILLER                  PIC X(6).
           03  DRC-STR.
               05 DRC-NR               PIC ZZZ9-.
               05 DRC-BEZ              PIC X(30).
               05 DRC-LFD              PIC ZZZ9-.
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
       01  WF-REG.
           03  WX-PRNO                 PIC 99      COMP-X VALUE ZERO.
           03  WX-PRSTAT               PIC 99      COMP-X.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WH-DRUNAM               PIC X(12).
           03  IP                      PIC 999     COMP.
           03  IZ                      PIC 999     COMP.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-APZ                  PIC Z99,9-.
           03  WD-BKZ                  PIC 9(7).
           03  WD-UST                  PIC Z9,99-.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WD-PZ                   PIC Z9.
           03  WD-TN                   PIC ZZ.
           03  WD-RZ                   PIC Z99,9.
           03  WD-POS                  PIC ZZ9.
           03  WD-TG                   PIC ZZZ.
           03  WD-RP                   PIC ZZZ9.
           03  WD-BPZ                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-VER                  PIC Z9.
           03  WD-KTO                  PIC ZZ.ZZ9,9-.
           03  WD-ZEIT                 PIC 99/99.
           03  WH-PX                   PIC XX       OCCURS 2.
           03  WK-MAX                  PIC 999      COMP.
           03  WK-MIN                  PIC 999      COMP.
           03  WK-PRF                  PIC 99       COMP-X  OCCURS 255.
           03  WK-UID.
               05 WK-ATU               PIC XXX.
               05 WK-ATUNR             PIC 9(8).
      *-----------------------------------------> Bildschirmausgaben <-
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WH-ART                  PIC 99.
           03  WV-CODE                 PIC XXXX.
           03  WD-X                    PIC X.
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WD-PROZ                 PIC Z9,9.
           03  WX-KZ                   PIC X.
           03  IX                      PIC 999     COMP.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 22 PERFORM MENGEN GO X.
           MOVE " Stammdatenverwaltung" TO WK-GEB.
           PERFORM STAMVERW.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
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
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WM-KO NOT = 3 or WE-PANIS = 1 GO C.
           GO C.
           IF WV-CODE = "4711" GO C.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           MOVE 6 TO WH-PG.
           IF ENDE GO C.
           IF ESC MOVE 0 TO WH-PG GO Z.
           MOVE WT-TX TO WV-CODE.
           IF WV-CODE NOT = "4711" GO A.
       C.  CALL "CAUP" USING "06KOPF" WH-CREG.
           UNLOCK KONSTANT.
           DISPLAY "<esc/ret-leer>= Programmende" AT 2301.
           DISPLAY "1 - Kunden"            AT 0714
                   "2 - Touren"            AT 0014
                   "3 - Artikel"           AT 0014
                   "4 - Mengeneinheiten"   AT 0014
                   "5 - Ust-SÑtze"         AT 0014
                   "6 - Rechnungstext"     AT 0014
                   "7 - Kopf / Code"       AT 0014
                   "8 - Rechnungsnummer"   AT 0014
                   "9 - Rechnungskopf"     AT 0014
                  "10 - Retourprozentsatz" AT 0013
                  "11 - Kundenetiketten"   AT 0013
                  "12 - Druckerangaben"    AT 0743
                  "13 - Dampflartikel"     AT 0043
                  "14 - Fibuverzeichnis"   AT 0043
                  "15 - Erlîskonten"       AT 0043
                  "16 - Tourenplan"        AT 0043
                  "17 - Teignummern"       AT 0043
                  "18 - Prod. Statistik"   AT 0043
                  "19 - Wirtschaftsjahr"   AT 0043
                  "20 - Backz.-Artikel"    AT 0043
                  "21 - Konditionen/Symb." AT 0043
                  "22 - Filialliste Komm." AT 0043
                  "bitte wÑhlen Sie:" with highlight AT 1929.
           CALL "CAUP" USING "0019472002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC OR ENDE MOVE 0 TO WH-PG GO Z.
           IF NOT RET GO C.
           EVALUATE WH-PG
               WHEN 0 GO Z
               WHEN 1 CALL "PANDEBAN" USING "60DEB" WH-CREG
                      CANCEL "PANDEBAN"
               WHEN 2 PERFORM TOUREN
               WHEN 3 CALL "PANARTAN" USING "50ART" WH-CREG
                      CANCEL "PANARTAN"
               WHEN 4 PERFORM MENGEN
               WHEN 5
               WHEN 6
               WHEN 7
               WHEN 8 PERFORM KOUSTTXNR
               WHEN 9 PERFORM REKOP
               WHEN 10 PERFORM RETPROZ
               WHEN 11 CALL "PANLIST" USING "50ETKDRU" WH-CREG
                         CANCEL "PANLIST"
               WHEN 12 PERFORM DRUPAR
               WHEN 13 PERFORM DAMPFL
               WHEN 14 PERFORM VERZEICH
               WHEN 15 PERFORM ERLOESE
               WHEN 16 PERFORM TOURPLAN
               WHEN 17 PERFORM TEIGGRP
               WHEN 18 PERFORM OEPZSTAT
               WHEN 19 PERFORM WIJAHR
               WHEN 20 PERFORM AUTBACK
               WHEN 21 PERFORM KONDIT
               WHEN 22 PERFORM FILLIST.
           GO C.
       Z.  EXIT.
      ***************************** Einstellung W-Jahr f. Archivierung *
       WIJAHR SECTION.
       A.  CALL "CAUP" USING "0710100555000" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "laufendes Wirtschaftsjahr:" AT VDU-LP.
           MOVE 10 TO WH-KEY.
       C.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF KO-WJAHR(1:2) = SPACE MOVE 0701 TO KO-WJAHR.
           PERFORM BILDJAHR.
       E.  DISPLAY "Beginn des lfd. Wirtschaftsjahres" AT 2401.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1103306006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO X.
           IF WZ-DATUM = 0 GO E.
           MOVE WZ-DATUM TO KO-WJAHR.
           PERFORM BILDJAHR.
       G.  DISPLAY "<ret>= speichern, </ >= zurÅck, <esc>= Abbruch < >"
               AT 2301.
           CALL "CAUP" USING "0023500000" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO C.
           IF NOT RET GO G.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************* Bilde Ende Wirtschaftsjahr *
       BILDJAHR SECTION.
       A.  MOVE WM-DATUM TO WZ-DATUM.
           COMPUTE WV-DATUM = WZ-JAHR * 10000 + KO-WJAHR.
           MOVE WV-DATUM TO WC-DATUM.
           IF WZ-DATUM < WC-DATUM;
               IF WC-TAG not = 0 SUBTRACT 1 FROM WC-TAG
                            else MOVE 99 TO WC-TAG.
           MOVE WC-DATUM TO WV-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           ADD 330 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " - ".
           MOVE WV-DATUM  TO WZ-DATUM.
           ADD -1 TO WZ-MONAT.
           IF WZ-MONAT = 0 MOVE 12 TO WZ-MONAT
                      else ADD 1 TO WZ-JAHR.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE 32 TO WZ-TAG.
       C.  ADD -1 TO WZ-TAG.
           MOVE WZ-DATUM TO WS-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE WS-DATUM TO WZ-DATUM GO C.
           MOVE WZ-DATUM TO WS-DATUM WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT 0000.
           ADD 17 TO VDU-LP.
           DISPLAY VDU-DATUM(7:2) with highlight foreground-color 4
              AT VDU-LP.
           MOVE WM-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
       Z.  EXIT.
      ********************************************** restl. Stammdaten *
       KOUSTTXNR SECTION.
       A.  MOVE 1 TO WH-KEY.
       D.  READ KONSTANT INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF WH-PG = 5 PERFORM USTSATZ GO Z.
           IF WH-PG = 6 PERFORM USTSATZ GO Z.
           IF WH-PG = 8 GO J.
      *-------------------------------> éndern Firmenkopf u. Codewort <-
       F.  MOVE KO-FIRMA TO WT-TX.
           CALL "CAUP" USING "0201270122" WH-CREG.
           MOVE WT-TX TO KO-FIRMA.
           IF NOT RET OR KO-FIRMA = SPACE GO F.
           DISPLAY KO-FIRMA AT 0127.
           CALL "CAUP" USING "0705430528000" WH-CREG.
           DISPLAY " Codewort " with highlight AT 0545.
       G.  DISPLAY "Altes Codewort:" AT 0645.
           DISPLAY KO-SPERRE AT 0663.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           DISPLAY "Neues Codewort:" AT 0745.
           ACCEPT WX-CODE(1) AT 0763.
           IF NOT RET GO G.
           MOVE WX-CODE(1) TO KO-SPERRE.
       H.  DISPLAY "Kontrolleingabe:" AT 0845.
           ACCEPT WX-CODE(2) AT 0863.
           IF NOT RET GO H.
           IF WX-CODE(2) NOT = WX-CODE(1)
               DISPLAY "Eingaben nicht gleich" AT 0445 GO G.
       I.  REWRITE KO-SATZ.
           GO X.
      *----------------------------------------> sonstige Konstanten <-
       J.  CALL "CAUP" USING "0704011644000" WH-CREG.
           DISPLAY " sonst. Konstanten " with highlight AT 0403.
           DISPLAY "letzte Rechnung.......:" AT 0603
                   "Ende Rechnungsperiode.:" AT 0703
                   "Rechnungsort..........:" AT 0903
                   "EAN-Betriebskennzahl..:" AT 1003
                   "Etikettendrucker......:" AT 1103
                   "Preis auf Lieferschein:" AT 1203
                   "Variante..............:" AT 1303
                   "Kopfdruck.............:" AT 1403
                   "Stammrabatt...........:" AT 1603
                   "Uid-Nummer............:" AT 1703.
           MOVE KO-RENUM TO WD-NUM.
           ADD 328 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           MOVE KO-MON TO WC-DATUM.
           CALL "CAUP" USING "04DATDR" WH-CREG.
           ADD 99 TO VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 200 TO VDU-LP.
           DISPLAY KO-ORT with highlight AT VDU-LP.
           MOVE KO-EANNR TO WD-BKZ.
           ADD 100 TO VDU-LP.
           DISPLAY WD-BKZ with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           MOVE KO-ETKDRU TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           EVALUATE WD-KZ
               WHEN 0 DISPLAY " - kein Drucker" AT 0000
               WHEN 1 DISPLAY " - T 402       " AT 0000
               WHEN 2 DISPLAY " - TLP 2844    " AT 0000.
           ADD 100 TO VDU-LP.
           MOVE KO-MERK TO WK-M.
           MOVE WM-PR TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           IF WM-PR = 1 DISPLAY "ja  " with highlight AT VDU-LP
                   ELSE DISPLAY "nein" with highlight AT VDU-LP.
           MOVE WM-VER TO WD-VER.
           ADD 97 TO VDU-LP.
           DISPLAY WD-VER with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           ADD 97 TO VDU-LP.
           PERFORM KOART.
           ADD 197 TO VDU-LP.
           MOVE WM-RB TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           EVALUATE WM-RB
               WHEN 0 DISPLAY "ja  " with highlight AT VDU-LP
               WHEN 1 DISPLAY "nein" with highlight AT VDU-LP.
           ADD 97 TO VDU-LP.
           IF KO-ATU not = SPACE MOVE KO-ATU TO WK-ATU
               MOVE KO-ATUNR TO WK-ATUNR
               DISPLAY WK-UID with highlight AT VDU-LP
           else DISPLAY "vorgedruckt" AT VDU-LP
                MOVE SPACE TO WK-ATU
                MOVE 0 TO WK-ATUNR.
      *------------------------------------------> Anlage / énderung <-
       K.  MOVE KO-RENUM TO WH-WERT.
           DISPLAY "<ret-leer>= keine énderung, <esc>= Abbruch" AT 2401.
           CALL "CAUP" USING "1003285005" WH-CREG.
           IF ESC GO X.
           IF FEHLER GO K.
           MOVE WH-WERT TO KO-RENUM WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
       L.  MOVE KO-MON TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1104276006" WH-CREG.
           IF WOLI GO K.
           IF WX-DATUM = 0 GO L.
           MOVE 31 TO WZ-TAG.
           MOVE WZ-DATUM TO WV-DATUM.
       M.  CALL "CAUP" USING "05DATPRUEF" WH-CREG.
           IF WX-DATUM = 0 SUBTRACT 1 FROM WV-DATUM
               MOVE WV-DATUM TO WZ-DATUM GO M.
           MOVE WX-DATUM TO KO-MON.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       N.  MOVE KO-ORT TO WT-TX.
           CALL "CAUP" USING "1206270115" WH-CREG.
           IF WOLI GO L.
           MOVE WT-TX TO KO-ORT.
           IF KO-ORT = SPACE GO N.
           DISPLAY KO-ORT with highlight AT 0927.
       O.  MOVE KO-EANNR TO WH-WERT.
           CALL "CAUP" USING "1007277007" WH-CREG.
           IF WOLI GO N.
           IF FEHLER GO O.
           MOVE WH-WERT TO WD-BKZ KO-EANNR.
           DISPLAY WD-BKZ with highlight AT 1027.
       OQ. MOVE KO-ETKDRU TO WH-WERT.
           DISPLAY "0= kein Drucker, 1= GK420t/ T402, 2= TLP2844"
                        AT 2401.
           CALL "CAUP" USING "1008271001" WH-CREG.
           IF WOLI GO O.
           IF WH-WERT > 2 GO OQ.
           IF FEHLER GO OQ.
           MOVE WH-WERT TO WD-KZ KO-ETKDRU.
           DISPLAY WD-KZ with highlight AT 1127.
           EVALUATE WD-KZ
               WHEN 0 DISPLAY " - kein Drucker" AT 0000
               WHEN 1 DISPLAY " - GK420t/T 402" AT 0000
               WHEN 2 DISPLAY " - TLP 2844    " AT 0000.
       P.  MOVE WM-PR TO WH-WERT.
           DISPLAY "empf. VKP am Lfs.: 0 = nein, 1 = ja" AT 2301.
           CALL "CAUP" USING "1009271001" WH-CREG.
           IF WOLI GO OQ.
           IF FEHLER OR WH-WERT > 2 GO P.
           MOVE WH-WERT TO WM-PR WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           IF WM-PR = 1 DISPLAY "ja  " with highlight AT VDU-LP
                   ELSE DISPLAY "nein" with highlight AT VDU-LP.
       Q.  MOVE WM-VER TO WH-WERT.
           DISPLAY "Version: 0 - 19 (+10 Lfs.Version)" AT 2301.
           CALL "CAUP" USING "1010272002" WH-CREG.
           IF WOLI GO P.
           IF FEHLER GO Q.
           MOVE WH-WERT TO WM-VER WD-VER.
           DISPLAY WD-VER with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
       R.  MOVE WM-KO TO WH-WERT.
           DISPLAY "Kopfdruck: 0 = nein, 1 = mit Druck, 2 = kl. Lfsch.,
      -        "3 = 2 zeilig" AT 2301.
           CALL "CAUP" USING "1011271001" WH-CREG.
           IF WOLI GO Q.
           IF FEHLER OR WH-WERT > 3 GO R.
           MOVE WH-WERT TO WM-KO.
           PERFORM KOART.
       S.  MOVE WM-RB TO WH-WERT.
           DISPLAY "Stammrabatt: " AT 2301
                   "0= ja, 1= nein" with highlight AT 0000.
           CALL "CAUP" USING "1013271001" WH-CREG.
           IF WOLI GO R.
           IF FEHLER GO S.
           ADD 3 TO VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           MOVE WH-WERT TO WD-KZ.
           EVALUATE WD-KZ
               WHEN 0 DISPLAY "ja  " with highlight AT VDU-LP
               WHEN 1 DISPLAY "nein" with highlight AT VDU-LP
               WHEN OTHER GO S.
           MOVE WH-WERT TO WM-RB.
       T.  MOVE WM-RB TO WH-WERT.
           DISPLAY "Uid-Nr.: (ATUnnnnnnnn)" AT 2301
           MOVE WK-UID TO WT-TX.
           CALL "CAUP" USING "1214270111" WH-CREG.
           IF WOLI GO S.
           IF FEHLER GO T.
           MOVE WT-TX TO WK-UID.
           IF WK-ATU = SPACE MOVE 0 TO WK-ATUNR
           else IF WK-ATU not = "ATU" GO T
                else INSPECT WK-ATUNR REPLACING ALL " " BY ZERO
                     IF WK-ATUNR not numeric GO T.
           IF WK-ATU not = SPACE
               DISPLAY WK-UID with highlight AT VDU-LP
           else DISPLAY "vorgedruckt" AT VDU-LP.
           MOVE WK-ATU TO KO-ATU.
           MOVE WK-ATUNR TO KO-ATUNR.
       W.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023340000" WH-CREG.
           IF ESC UNLOCK KONSTANT GO X.
           IF NOT RET GO W.
           MOVE WK-M TO KO-MERK.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KOART SECTION.
       A.  MOVE WM-KO TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           EVALUATE WM-KO
               WHEN 0 DISPLAY "kein Kopf" with highlight AT VDU-LP
               WHEN 1 DISPLAY "mit Kopf " with highlight AT VDU-LP
               WHEN 2 DISPLAY "kl. Lfsch" with highlight AT VDU-LP
               WHEN 3 DISPLAY "2 zeilig " with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       PARMDRU SECTION.
       A.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " "with SIZE 40 AT VDU-LP.
           EVALUATE KO-PARM
               WHEN 2 DISPLAY "CAPTURE P=0 Q=PRINTQ_1 NB TI=0 L=1 NFF"
                          with highlight AT VDU-LP
               WHEN 1 DISPLAY "CAPTURE P=0 Q=PRINTQ_0 NB TI=0 L=1 NFF"
                          with highlight AT VDU-LP
               WHEN OTHER DISPLAY "Druck auf lokalem Drucker"
                          with highlight AT VDU-LP.
       Z.  EXIT.
      ************************************* Druckereinstellung im Netz *
       DRUPAR SECTION.
       A.  CALL "CAUP" USING "0710101050000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Was wo drucken? " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 35 TO WH-KEY.
           READ KONSTANT INVALID MOVE 0 TO KO-PARM.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 35 TO KO-NUM.
           PERFORM PARMDRU.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 5
               COMPUTE VDU-LP = WX * 100 + 403 + VDU-ECK
               EVALUATE WX
                   WHEN 1 DISPLAY "Rechnung....:" AT VDU-LP
                   WHEN 2 DISPLAY "Lf-Schein...:" AT VDU-LP
                   WHEN 3 DISPLAY "Backpapiere.:" AT VDU-LP
                   WHEN 4 DISPLAY "Auswertungen:" AT VDU-LP
                   WHEN 5 DISPLAY "Rez-Auswertg:" AT VDU-LP
               end-evaluate
               MOVE KOP-DRU(WX) TO WD-KZ
               ADD 17 TO VDU-LP
               DISPLAY WD-KZ with highlight foreground-color 2 AT VDU-LP
               ADD 5 TO VDU-LP
               MOVE KOP-KOPF(WX) TO WD-KZ
               DISPLAY WD-KZ with highlight foreground-color 3 AT VDU-LP
               ADD 5 TO VDU-LP
               MOVE KOP-WO(WX) TO WD-KZ
               DISPLAY WD-KZ with highlight foreground-color 6
                   AT VDU-LP
               ADD 5 TO VDU-LP
               MOVE KOP-TYP(WX) TO WD-KZ
               DISPLAY WD-KZ with highlight foreground-color 6
                   AT VDU-LP.
           MOVE 1 TO WX.
       E.  MOVE KO-PARM TO WT-TX.
           DISPLAY "<ret>= Druckernummer" AT 2301.
           DISPLAY "1: 1. Netzdrucker, 2: 2. Netzdrucker, 0: kein Netzdr
      -        "ucker" AT 2301.
           MOVE KO-PARM TO WH-WERT.
           CALL "CAUP" USING "1003031001" WH-CREG.
           IF not RET GO E.
           IF WH-WERT > 2 GO E.
           MOVE WH-WERT TO KO-PARM.
           PERFORM PARMDRU.
      *-------------------------------------------> Anlage / énderung <-
       G.  DISPLAY "<esc>= Abbruch, < />= zurÅck" AT 2301.
           DISPLAY "auf: 0= Nadel- 1= Laserdrucker" with highlight
               AT 2401.
           MOVE KOP-DRU(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 420.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF; IF WX > 1 ADD -1 TO WX GO G.
           IF ENDE GO W.
           IF ESC GO X.
           IF not RET GO G.
           IF WH-WERT > 1 GO G.
           MOVE WH-WERT TO KOP-DRU(WX) WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 2 AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, < />= zurÅck" AT 2301.
           DISPLAY "0= ohne 1= mit Kopf" with highlight AT 2401.
           MOVE KOP-KOPF(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 425.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF GO G.
           IF ENDE GO W.
           IF ESC GO X.
           IF not RET GO I.
           IF WH-WERT > 1 GO G.
           MOVE WH-WERT TO KOP-KOPF(WX) WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 3 AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, < />= zurÅck" AT 2301.
           DISPLAY "0= Druckdatei, 9= Direktdruck" with highlight
               AT 2401.
           MOVE KOP-WO(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 430.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF GO G.
           IF ENDE GO W.
           IF ESC GO X.
           IF WH-WERT > 9 GO K.
           MOVE WH-WERT TO KOP-WO(WX) WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 6 AT VDU-LP.
       M.  DISPLAY "<esc>= Abbruch, < />= zurÅck" AT 2301.
           EVALUATE WX
               WHEN 1 DISPLAY "0= normal, 1= Kopie getrennt (Laser)"
                           with highlight AT 2401
               WHEN 2 DISPLAY "0= keine, 1= kleiner Lfsch., 3= 2-zeilig"
                           with highlight AT 2401
               WHEN OTHER DISPLAY "ohne Bedeutung" AT 2401.
           MOVE KOP-TYP(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 435.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF GO K.
           IF ENDE GO W.
           IF ESC GO X.
           IF WH-WERT > 3 GO M.
           MOVE WH-WERT TO KOP-TYP(WX) WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 6 AT VDU-LP.
           IF WX < 5 ADD 1 TO WX.
           GO G.
       W.  REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       RETPROZ SECTION.
       A.  CALL "CAUP" USING "0710300530000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Retourwarenprozentsatz " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "max.        ab:         " AT VDU-LP.
           MOVE 6 TO WH-KEY.
       B.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE KO-RET TO WD-RZ.
           ADD 5 TO VDU-P.
           DISPLAY WD-RZ with highlight AT VDU-LP "%".
           ADD 11 TO VDU-P.
           MOVE KO-RETDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDR" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       C.  DISPLAY "max. Retourenprozentsatz" AT 2301.
           MOVE KO-RET TO WH-WERT WD-RZ.
           CALL "CAUP" USING "1003083104" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WH-WERT > 100 GO C.
           MOVE WH-WERT TO KO-RET WD-RZ.
           DISPLAY WD-RZ with highlight AT VDU-LP "%".
       E.  DISPLAY "gÅltig ab" AT 2301.
           IF KO-RETDAT = 0 MOVE WM-DATUM TO KO-RETDAT.
           MOVE KO-RETDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1103196006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           IF WZ-DATUM = 0 GO E.
           MOVE WX-DATUM TO KO-RETDAT.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
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
           IF WH-KEY = 5 MOVE KO-KOPF TO DRB-SATZ(6:)
                    else MOVE KO-KOPF TO DRB-SATZ(4:).
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
                      MOVE KO-KOPF TO DRB-SATZ(9:)
                      PERFORM OB-TEILG
                      MOVE 0 TO WZ-SCHALT
               WHEN 3 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(9:)
               WHEN 4 MOVE "  (s0p0s3b6T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(9:)
               WHEN 5 MOVE "  (s0p0s0b0T" TO DRA-SATZ PERFORM DRUCK
                      MOVE KO-KOPF TO DRB-SATZ(14:).
       Z.  EXIT.
      ************************************************** Rechnungskopf *
       REKOP SECTION.
       S.  CALL "CAUP" USING "0707010680000" WH-CREG.
           DISPLAY " Rechnungskopf * 4 Zeilen " with highlight
               foreground-color 7 reverse-video AT 0703.
           DISPLAY "<esc>= Abbruch" AT 2301.
           MOVE 2 TO WH-KEY.
       T.  READ KONSTANT IGNORE LOCK INVALID MOVE SPACE TO KO-SATZ
               MOVE WH-KEY TO KO-NUM
               WRITE KO-SATZ.
           COMPUTE VDU-REL = WH-KEY * 100 + 2 + VDU-ECK.
           DISPLAY KO-KOPF WITH SIZE 78 HIGHLIGHT AT VDU-REL.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO T.
           MOVE 2 TO WH-KEY.
       U.  READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO U.
           COMPUTE VDU-L = WH-KEY + 6.
           MOVE KO-KOPF TO WT-TX.
           CALL "CAUP" USING "0200020178" WH-CREG.
           IF ESC GO Y.
           MOVE WT-TX TO KO-KOPF.
           REWRITE KO-SATZ.
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
       W.  READ KONSTANT IGNORE LOCK.
           EVALUATE WM-DRU
               WHEN 0 PERFORM NADEL-KOPF
               WHEN 1 PERFORM LASER-KOPF.
           PERFORM DRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO W.
           MOVE ALL "ƒ" TO DRB-TX.
           PERFORM DRUCK.
           PERFORM END-DRU.
           MOVE 2 TO WH-KEY.
           GO T.
       Y.  PERFORM END-DRU.
           REWRITE KO-SATZ.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       TOUREN SECTION.
       A.  CALL "CAUP" USING "0703231250020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Touren " with highlight AT VDU-REL.
           ADD 203 TO VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.Bezeichnung" AT VDU-REL.
           ADD 302 TO VDU-ECK GIVING VDU-REL.
           DISPLAY ALL "ƒ" WITH SIZE 48 AT VDU-REL.
           MOVE 1 TO WX.
       C.  DISPLAY "<ret>= Nummer, <tab>= suchen, <esc>= Ende" AT 2301.
           DISPLAY "1 - 20 mîglich" with highlight AT 2401.
           COMPUTE VDU-LP = 303 + WX * 100.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF RET; IF WH-NUM = 0 OR WH-NUM > 20 GO C.
           IF NOT RETA GO C.
           IF NOT TABL GO J.
       D.  MOVE 0 TO WR.
           SUBTRACT 1 FROM WX.
           CALL "CAUP" USING "1500002002" WH-CREG.
           IF WH-NUM = 0 MOVE 1 TO WH-NUM.
           ADD WH-NUM 10 GIVING WH-KEY.
      *----------------------------------------------------> anzeigen <-
           START KONSTANT KEY NOT < WH-KEY INVALID KEY GO H.
       F.  ADD 1 TO WX.
           READ KONSTANT NEXT IGNORE LOCK AT END GO C.
           IF KO-TOUR = SPACE ADD -1 TO WX GO F.
           IF WH-KEY > 30 GO C.
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
           MOVE KO-NUM TO WD-PZ.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KO-TOUR with highlight AT VDU-REL.
           ADD 26 TO VDU-REL.
           MOVE KO-REST TO WD-BET.
           DISPLAY WD-BET AT VDU-REL.
           GO F.
       J.  MOVE WH-NUM TO WD-PZ KO-NUM.
           ADD 10 WH-NUM GIVING WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-TSATZ.
           IF ZUGRIF PERFORM BESETZT GO J.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KO-TOUR with highlight AT VDU-REL.
           ADD 26 TO VDU-REL.
           MOVE KO-REST TO WD-BET.
           DISPLAY WD-BET AT VDU-REL.
           DISPLAY "<ret-leer>= wiederholen, kein Text = lîschen"
               AT 2301.
           MOVE KO-TOUR TO WT-TX.
           COMPUTE VDU-LP = 306 + WX * 100.
           CALL "CAUP" USING "1200000130" WH-CREG.
           IF ESC GO C.
           MOVE WT-TX TO KO-TOUR.
           IF KO-TOUR not = SPACE or KO-SBEZ not = SPACE GO P.
           DELETE KONSTANT INVALID KEY GO C.
           GO C.
       P.  SUBTRACT 10 FROM WH-KEY GIVING KO-NUM.
           ADD 1 TO WX.
           IF WX > 7 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 8 TO WX.
           WRITE KO-SATZ INVALID REWRITE KO-SATZ.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       OEPZSTAT SECTION.
       A.  CALL "CAUP" USING "0703231246020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Prod. Statistik " with highlight AT VDU-REL.
           ADD 203 TO VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.Bezeichnung              Statistikcode"
               AT VDU-REL.
           ADD 302 TO VDU-ECK GIVING VDU-REL.
           DISPLAY ALL "ƒ" WITH SIZE 44 AT VDU-REL.
           MOVE 1 TO WX.
       C.  DISPLAY "<ret>= Nummer, <tab>= suchen, <esc>= Ende" AT 2301.
           DISPLAY "1 - 20 mîglich" with highlight AT 2401.
           COMPUTE VDU-LP = 303 + WX * 100.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF RET; IF WH-NUM = 0 OR WH-NUM > 20 GO C.
           IF NOT RETA GO C.
           IF NOT TABL GO J.
       D.  MOVE 0 TO WR.
           SUBTRACT 1 FROM WX.
           CALL "CAUP" USING "1500002002" WH-CREG.
           IF WH-NUM = 0 MOVE 1 TO WH-NUM.
           ADD WH-NUM 10 GIVING WH-KEY.
      *----------------------------------------------------> anzeigen <-
           START KONSTANT KEY NOT < WH-KEY INVALID KEY GO H.
       F.  ADD 1 TO WX.
           READ KONSTANT NEXT IGNORE LOCK AT END GO C.
           IF KO-SBEZ = SPACE ADD -1 TO WX GO F.
           IF WH-KEY > 30 GO C.
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
           MOVE KO-NUM TO WD-PZ.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KO-SBEZ with highlight AT VDU-REL.
           ADD 26 TO VDU-REL.
           DISPLAY KO-STATNR with highlight AT VDU-REL.
           GO F.
       J.  MOVE WH-NUM TO WD-PZ KO-NUM.
           ADD 10 WH-NUM GIVING WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-TSATZ.
           IF ZUGRIF PERFORM BESETZT GO J.
           COMPUTE VDU-REL = VDU-ECK + 303 + WX * 100.
           DISPLAY WD-PZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY KO-SBEZ with highlight AT VDU-REL.
           ADD 26 TO VDU-REL.
           DISPLAY KO-STATNR with highlight AT VDU-REL.
       L.  DISPLAY "<ret-leer>= wiederholen, kein Text = lîschen"
               AT 2301.
           MOVE KO-SBEZ TO WT-TX.
           COMPUTE VDU-LP = 306 + WX * 100.
           CALL "CAUP" USING "1200000124" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO L.
           MOVE WT-TX TO KO-SBEZ.
           IF KO-TOUR not = SPACE or KO-SBEZ not = SPACE GO N.
           DELETE KONSTANT INVALID KEY GO C.
           GO C.
       N.  DISPLAY "<esc/ >= zurÅck, <ret>= Stat. Code-Nr." AT 2301.
           MOVE KO-STATNR TO WT-TX.
           COMPUTE VDU-LP = 332 + WX * 100 + VDU-ECK.
           DISPLAY KO-STATNR with highlight AT VDU-LP.
           SUBTRACT VDU-ECK FROM VDU-LP.
           CALL "CAUP" USING "1200320112" WH-CREG.
           IF WOLI or AUF or ESC GO L.
           IF NOT RET GO N.
           MOVE WT-TX TO KO-STATNR.
           DISPLAY KO-STATNR with highlight AT VDU-LP.
       P.  SUBTRACT 10 FROM WH-KEY GIVING KO-NUM.
           ADD 1 TO WX.
           IF WX > 7 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 8 TO WX.
           WRITE KO-SATZ INVALID REWRITE KO-SATZ.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       USTSATZ SECTION.
       A.  IF WH-PG = 5 GO D.
           CALL "CAUP" USING "0709080355000" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Rechnungstext " with highlight AT VDU-REL.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY KO-TX with highlight AT VDU-REL.
       B.  DISPLAY "<ret>= Aenderung / Anlage, <esc>= Abbruch" AT 2301.
           MOVE KO-TX TO WT-TX.
           CALL "CAUP" USING "1202030151" WH-CREG.
           MOVE WT-TX TO KO-TX.
           IF ESC GO X.
           IF NOT RET GO B.
           REWRITE KO-SATZ.
           GO X.
       D.  CALL "CAUP" USING "0704081017020" WH-CREG.
           DISPLAY " Ust-Saetze " with highlight AT 0411.
           DISPLAY "Nr.    %-Satz" AT 0510.
           DISPLAY "-------------" AT 0610.
           MOVE 1 TO WX.
       F.  ADD 6 WX GIVING VDU-L.
           MOVE WX TO WD-KZ.
           MOVE 11 TO VDU-P.
           DISPLAY WD-KZ AT VDU-LP.
           MOVE KO-UST(WX) TO WD-UST.
           MOVE 17 TO VDU-P.
           DISPLAY WD-UST with highlight AT VDU-LP.
           IF WX < 6 ADD 1 TO WX GO F.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Eingabe, <esc>= Ende, <>= zurueck" AT 2301.
           ADD 6 WX GIVING VDU-L.
           MOVE KO-UST(WX) TO WH-WERT.
           CALL "CAUP" USING "0000002205" WH-CREG.
           IF ESC GO L.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF AB GO I.
           IF FEHLER GO H.
           MOVE 17 TO VDU-P.
           MOVE WH-WERT TO KO-UST(WX) WD-UST.
           DISPLAY WD-UST with highlight AT VDU-LP.
       I.  IF WX < 6 ADD 1 TO WX GO H.
       L.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO X.
           IF NOT RET GO L.
           REWRITE KO-SATZ.
           MOVE 1 TO WX.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       MENGEN SECTION.
       A.  IF WL-CA = 22 CALL "CAUP" USING "0702441434020" WH-CREG
                    ELSE CALL "CAUP" USING "0705401434020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Mengeneinheiten " with highlight AT VDU-REL.
           MOVE 10 TO WH-KEY.
       B.  READ KONSTANT INVALID KEY GO X.
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
           DISPLAY KO-MEH(WX) with highlight AT VDU-LP.
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
           MOVE KO-MEH(WX) TO WT-TX.
           CALL "CAUP" USING "1200000103" WH-CREG.
           MOVE WT-TX TO KO-MEH(WX).
           IF ESC GO K.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO D.
           IF AB  GO I.
           IF NOT RET GO D.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY KO-MEH(WX) with highlight AT VDU-LP.
       I.  IF WX < 20 ADD 1 TO WX GO D.
       K.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO K.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************ Artikel fÅr die Dampflliste *
       DAMPFL SECTION.
       A.  CALL "CAUP" USING "0705051470010" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Dampflartikel " with highlight AT VDU-REL.
           MOVE 10 TO WH-KEY.
       B.  READ KONSTANT INVALID KEY GO X.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF KO-MART(1) = 8224 PERFORM VARYING WX FROM 1 BY 1
               UNTIL WX > 12 MOVE 0 TO KO-MART(WX).
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Art. Bezeichnung" AT VDU-REL.
           ADD 0238 VDU-ECK GIVING VDU-REL.
           DISPLAY "Art. Bezeichnung" AT VDU-REL.
           ADD 0303 VDU-ECK GIVING VDU-REL.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 1 TO WX.
       D.  COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK.
           IF WX > 11 ADD -1065 TO VDU-LP.
           MOVE KO-MART(WX) TO WD-POS AR-NUM.
           IF AR-NUM = 0 GO G.
           DISPLAY WD-POS with highlight AT VDU-LP.
           ADD 5 TO VDU-P.
       E.  READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel fehlt" TO AR-BEZ.
           DISPLAY AR-BEZ with highlight AT VDU-LP.
       F.  IF WX < 22 ADD 1 TO WX GO D.
           MOVE 1 TO WX.
       G.  DISPLAY "<ret>= Art.Nr." AT 2301.
           DISPLAY "< >= auf, < >= ab, <esc>= Ende" AT 2401.
           MOVE KO-MART(WX) TO WH-WERT WD-POS.
           COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK.
           IF WX > 11 ADD -1065 TO VDU-LP.
           IF WH-WERT NOT = 0 DISPLAY WD-POS with highlight AT VDU-LP.
           COMPUTE VDU-LP = VDU-LP - VDU-ECK.
           CALL "CAUP" USING "1000003003" WH-CREG.
           IF ESC GO Q.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO G.
           IF AB  GO I.
           IF NOT RET GO G.
           MOVE WH-WERT TO AR-NUM.
           IF AR-NUM = 0 DISPLAY ALL " " with SIZE 30 AT VDU-LP
               IF WX < 22 PERFORM VARYING WY FROM WX BY 1 UNTIL WY > 11
                   MOVE KO-MART(WY + 1) TO KO-MART(WY)
                   MOVE 0 TO KO-MART(WY + 1)
               ELSE MOVE 0 TO KO-MART(WX)
               END-IF GO C.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 22
               IF KO-MART(WY) = AR-NUM AND WY NOT = WX
                   DISPLAY "bereits vorhanden" AT 2401
                   PERFORM WEITER GO G.
           MOVE WH-WERT TO KO-MART(WX) WD-POS.
           DISPLAY WD-POS with highlight AT VDU-LP.
           READ ARTIKEL IGNORE LOCK INVALID PERFORM NO-REC GO G.
           ADD 5 TO VDU-LP.
           DISPLAY AR-BEZ with highlight AT VDU-LP.
       I.  IF WX < 22 ADD 1 TO WX GO G.
       Q.  CALL "CAUP" USING "1323012440" WH-CREG.
           DISPLAY "<ret>= speichern, <>= zurÅck" AT 2301.
           DISPLAY "<esc>= Abbruch" AT 2401.
           CALL "CAUP" USING "0023310000" WH-CREG.
           CALL "CAUP" USING "1323012440" WH-CREG.
           IF ESC GO X.
           IF WOLI GO C.
           IF NOT RET GO Q.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       VERZEICH SECTION.
       A.  IF WM-VER = 3 CALL "CAUP" USING "0710100453000" WH-CREG
                    else CALL "CAUP" USING "0710100353000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " FibudateiverknÅpfung " with highlight AT VDU-LP.
       C.  MOVE 36 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-ESATZ
               MOVE WH-KEY TO KO-NUM.
           IF ZUGRIF PERFORM BESETZT GO X.
           IF KO-VERZ = SPACE MOVE "F:\FIBU\FIBUKONS.DAT" TO KO-VERZ.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Verzeichnis:" AT VDU-LP.
           ADD 216 VDU-ECK GIVING VDU-LP.
           DISPLAY KO-VERZ with highlight AT VDU-LP.
           IF WM-VER = 3 ADD 303 VDU-ECK GIVING VDU-LP
               DISPLAY "Modemdatei :" AT VDU-LP
               ADD 316 VDU-ECK GIVING VDU-LP
               DISPLAY KO-MODEM with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Verzeichnis und Bezeichnung d
      -        ". FIBU-Konstantendatei" AT 2301.
           DISPLAY "BMD" with highlight AT 2401 " fÅr BMD-, ".
           DISPLAY "KWF  xxxxxx" with highlight AT 0000 " fÅr KWF-Fibu
      -        "Mandaten-Nr.".
           DISPLAY "Achtung bei KWF zwei Leerschritte und dan Mandanten-
      -        "Nr." AT 2501.
           MOVE KO-VERZ TO WT-TX.
           CALL "CAUP" USING "1202160135" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           MOVE WT-TX TO KO-VERZ.
           DISPLAY KO-VERZ with highlight AT VDU-LP.
           IF WM-VER not = 3 GO W.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Verzeichnis und Bezeichnung d
      -        ". Hofer-MODEM-Datei" AT 2301.
           MOVE KO-MODEM TO WT-TX.
           CALL "CAUP" USING "1203160127" WH-CREG.
           IF ESC GO X.
           IF WOLI GO E.
           IF NOT RET GO G.
           MOVE WT-TX TO KO-MODEM.
           DISPLAY KO-MODEM with highlight AT VDU-LP.
       W.  REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************* Erlîskontenzuordnung *
       ERLOESE SECTION.
       A.  MOVE 36 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-ESATZ
               WRITE KO-SATZ GO A.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0705201432010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Erlîskonten " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "  Nr. Konto-Nr. Nr. Konto-Nr. "
                with reverse-video AT VDU-LP.
       F.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 21
               COMPUTE VDU-LP = 204 + WX * 100 + VDU-ECK
               IF WX > 10 ADD -986 TO VDU-LP
               end-if
               MOVE WX TO WD-PZ
               DISPLAY WD-PZ AT VDU-LP
               ADD 4 TO VDU-LP
               IF KO-KTONR(WX) NOT = 0
                   DIVIDE 10 INTO KO-KTONR(WX) GIVING WD-KTO
                   DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 1303 VDU-ECK GIVING VDU-LP.
           DISPLAY "   Erlîse 20%" AT VDU-LP.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Eingabe, <esc>= Ende, <>= zurÅck" AT 2301.
           COMPUTE VDU-LP = WX * 100 + 208.
           IF WX > 10 ADD -986 TO VDU-LP.
           MOVE KO-KTONR(WX) TO WH-WERT.
           CALL "CAUP" USING "1000005107" WH-CREG.
           IF ESC GO L.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF FEHLER GO H.
           MOVE WH-WERT TO KO-KTONR(WX).
           IF WX = 1;
              PERFORM VARYING WX FROM 2 BY 1 UNTIL WX > 21
                  IF KO-KTONR(WX) not = 0 MOVE 29 TO WX
              end-perform
              IF WX = 22;
                 PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 21
                    MOVE KO-KTONR(1) TO KO-KTONR(WX)
                    COMPUTE VDU-LP = 204 + WX * 100 + VDU-ECK
                    IF WX > 10 ADD -986 TO VDU-LP
                    end-if
                    MOVE WX TO WD-PZ
                    DISPLAY WD-PZ AT VDU-LP
                    ADD 4 TO VDU-LP
                    DIVIDE 10 INTO KO-KTONR(WX) GIVING WD-KTO
                    DISPLAY WD-KTO with highlight AT VDU-LP
              end-if MOVE 1 TO WX.
           DIVIDE 10 INTO WH-WERT GIVING WD-KTO.
           IF WH-WERT NOT = 0 DISPLAY WD-KTO with highlight AT VDU-LP
                         else DISPLAY "         " AT VDU-LP.
           IF WX < 21 ADD 1 TO WX GO H.
       L.  DISPLAY "<ret>= speichern, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023341001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO Y.
           IF NOT RET GO L.
           REWRITE KO-SATZ.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************** Plan anzeigen *
       PLANDIS SECTION.
       A.  ADD 303 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE WX TO WD-TN
               DISPLAY WD-TN AT VDU-LP
               ADD 1 TO VDU-L.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
                   IF KO-TZEIT(WX) NOT = 0
                       MOVE KO-TZEIT(WX) TO WD-ZEIT
                       MOVE ":" TO WD-ZEIT(3:1)
                       COMPUTE VDU-LP = WX * 100 + 206 + VDU-ECK
                       DISPLAY WD-ZEIT with highlight AT VDU-LP
                   end-if
                   PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 10
                       IF KO-TNR(WX WY) NOT = 0
                           MOVE KO-TNR(WX WY) TO WD-TN
                           COMPUTE VDU-LP = WX * 100 + 209 + VDU-ECK
                           COMPUTE VDU-LP = VDU-LP + (WY * 4)
                           DISPLAY WD-TN with highlight AT VDU-LP.
       Z.  EXIT.
      ***************************************************** Tourenplan *
       TOURPLAN SECTION.
       A.  CALL "CAUP" USING "0707131352010" WH-CREG.
           MOVE 9 TO WH-KEY.
           READ KONSTANT INVALID MOVE LOW-VALUE TO KO-ZSATZ.
           IF ZUGRIF PERFORM BESETZT GO A.
           IF KO-TKO NOT = SPACE MOVE LOW-VALUE TO KO-ZSATZ.
           MOVE SPACE TO KO-TKO.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Tourenplanung " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Nr. Zeit   1   2   3   4   5   6   7   8   9  10 "
      *              1  12:12  12  12  12  12  12  12  12  12  12  12
               with reverse-video AT VDU-LP.
           PERFORM PLANDIS.
      *-----------------------------------------------------> Planung <-
           MOVE 1 TO WX.
       E.  DISPLAY "Tourabfahrt" AT 2401.
           DISPLAY "<esc>= Abbruch, <ret>= Zeit, <>= zurÅck, <Ende>= En
      -        "de" AT 2301.
           MOVE KO-TZEIT(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 206.
           CALL "CAUP" USING "1000064004" WH-CREG.
           IF ESC GO X.
           IF Ende GO W.
           IF WOLI AND WX > 1 ADD -1 TO WX GO E.
           IF NOT RET GO E.
           MOVE WH-WERT TO KO-TZEIT(WX) WD-ZEIT.
           MOVE ":" TO WD-ZEIT(3:1).
           DISPLAY WD-ZEIT with highlight AT VDU-LP.
           MOVE 1 TO WY.
       G.  DISPLAY "Tourenzuordnung" AT 2301.
           DISPLAY "<esc>= Abbruch, <ret>= Tour, <>= zurÅck, 0+<ret>= k
      -       "eine weiter Tour" AT 2301.
           MOVE KO-TNR(WX WY) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 209.
           COMPUTE VDU-LP = VDU-LP + (WY * 4).
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF WOLI; IF WY = 1 GO E else ADD -1 TO WY GO G.
           IF NOT RET GO G.
           IF WV-MCODE(1:1) = "0"
               PERFORM VARYING WY FROM WY BY 1 UNTIL WY > 10
                       MOVE 0 TO KO-TNR(WX WY) GO H.
           MOVE WH-WERT TO KO-TNR(WX WY) WD-TN.
           DISPLAY WD-TN with highlight AT VDU-LP.
           IF WY < 10 ADD 1 TO WY GO G.
       H.  MOVE WX TO WZ.
           PERFORM PLANDIS.
           MOVE WZ TO WX.
           IF WX < 10 ADD 1 TO WX GO E.
       K.  DISPLAY "<esc>= Abbruch, <ret>= speichern < >" AT 2301.
           CALL "CAUP" USING "0023350000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO K.
       W.  REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       WECHSEL SECTION.
       A.  REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           IF BAB and WH-KEY < 39 ADD 1 TO WH-KEY GO Z.
           IF BAUF and WH-KEY > 37 ADD -1 TO WH-KEY GO Z.
       Z.  EXIT.
      ****************************************** Teiggruppenverwaltung *
       TEIGGRP SECTION.
       A.  CALL "CAUP" USING "0704071766000" WH-CREG.
           OPEN INPUT REZEPT.
           MOVE 0 TO WD-KZ.
           IF WF-STATUS not = 00 MOVE 1 TO WD-KZ.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Teiggruppenzuordnung " AT VDU-LP.
           MOVE 37 TO WH-KEY.
       B.  READ KONSTANT INVALID MOVE SPACE TO KO-VKZ.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF KO-VKZ not = "*" MOVE LOW-VALUES TO KO-VSATZ.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 86 TO WK-MIN.
           IF WH-KEY = 37 MOVE 100 TO WK-MAX
                          MOVE 1 TO WY.
           IF WH-KEY = 38 MOVE 100 TO WK-MAX
                          MOVE 101 TO WY.
           IF WH-KEY = 39 MOVE  50 TO WK-MAX
                          MOVE 201 TO WY
                          MOVE 36 TO WK-MIN.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WK-MAX
               MOVE WY TO WD-POS
               ADD 1 TO WY
               DIVIDE 15 INTO WX GIVING VDU-P REMAINDER VDU-L
               MULTIPLY 9 BY VDU-P GIVING VDU-P
               ADD 203 VDU-ECK TO VDU-LP
               DISPLAY WD-POS AT VDU-LP "= "
               IF KO-TGRP(WX) not = 0 MOVE KO-TGRP(WX) TO WD-TG
                   DISPLAY WD-TG with highlight AT 0000.
           MOVE 1 TO WX.
       E.  DISPLAY "< / // >= auf/ab/vor/zurÅck, <Ende>= Ende" AT 2301.
           DISPLAY "<Bild- /Bild- >= vorherige/nÑchste Gruppe" AT 2401.
           DIVIDE 15 INTO WX GIVING VDU-P REMAINDER VDU-L
           MULTIPLY 9 BY VDU-P GIVING VDU-P
           ADD 208 TO VDU-LP.
           MOVE KO-TGRP(WX) TO WH-WERT.
           IF WD-KZ = 1 GO G.
           ADD 3000 WX GIVING RE-NUM.
           IF WH-KEY = 38 ADD 100 TO RE-NUM.
           IF WH-KEY = 39 ADD 200 TO RE-NUM.
           MOVE SPACE TO RE-BEZ.
           CALL "CAUP" USING "1321012180" WH-CREG.
       F.  READ REZEPT INVALID NEXT SENTENCE.
           IF ZUGRIF PERFORM BESETZT GO F.
           DISPLAY RE-BEZ  with highlight AT 2110.
       G.  CALL "CAUP" USING "1000003003" WH-CREG.
           IF BAB or BAUF PERFORM WECHSEL GO B.
           IF AUF and WX > 1 ADD -1 TO WX.
       K.  IF AB and WX < WK-MAX ADD 1 TO WX.
           IF WOLI; IF WX > 14 ADD -15 TO WX;
                       IF WX = 0 MOVE 1 TO WX.
           IF WORE; IF WX < WK-MIN ADD 15 TO WX.
           IF ESC GO X.
           IF ENDE GO W.
           IF NOT RET GO E.
           IF WH-WERT > 0
              PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 100
                  IF KO-TGRP(WY) = WH-WERT AND WY not = WX
                     DISPLAY "Doppelt" with highlight AT 2401
                     PERFORM WEITER GO E.
           MOVE WH-WERT TO WD-TG KO-TGRP(WX).
           DISPLAY WD-TG with highlight AT VDU-LP.
           SET AB TO TRUE.
           GO K.
       W.  DISPLAY "<ret>= speichern, <esc>= Abbruch, < >= drucken, <>=
      -        " zurÅck < >" AT 2301.
           CALL "CAUP" USING "0023620000" WH-CREG.
           IF WOLI GO E.
           IF NOT RET and NOT WORE GO W.
           MOVE "*" TO KO-VKZ.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           PERFORM FOLGPRUEF.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE REZEPT.
       Z.  EXIT.
      ******************************************* PrÅfung Sortierfolge *
       FOLGPRUEF SECTION.
       A.  PERFORM VARYING IP FROM 1 BY 1 UNTIL IP > 250
               MOVE 0 TO WK-PRF(IP).
           MOVE 36 TO WH-KEY.
       C.  ADD 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO Q.
           EVALUATE WH-KEY
               WHEN 37 MOVE 0 TO WK-MIN
                       MOVE 100 TO WK-MAX
               WHEN 38 MOVE 100 TO WK-MIN
               WHEN 39 MOVE 100 TO WK-MIN
                       MOVE  50 TO WK-MAX.
           PERFORM VARYING IP FROM 1 BY 1 UNTIL IP > WK-MAX
               IF KO-TGRP(IP) not = 0 MOVE KO-TGRP(IP) TO WX
                   ADD WK-MIN IP GIVING IZ
                   IF WK-PRF(WX) = 0 MOVE IZ TO WK-PRF(WX)
                   else MOVE WX TO WD-POS
                       DISPLAY "Gruppe: " AT 2401 WD-POS with highlight
                           " mehrmals vergeben! "
                       PERFORM WEITER.
       Q.  IF WH-KEY < 39 GO C.
           MOVE 0 TO WZ-ZEILEN WM-OPEN.
           MOVE 36 TO WH-KEY.
           IF WORE PERFORM VARYING IP FROM 1 BY 1 UNTIL IP > 250
                      PERFORM FOLDRU.
           PERFORM END-DRU.
       Z.  EXIT.
      ************************************************ öbersichtsdruck *
       FOLDRU SECTION.
       A.  IF WZ-ZEILEN > 63 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "TGFOLGE.LST" TO WH-DRUNAM.
           MOVE WE-DRU(4) TO WM-DRU.
           IF WM-DRU = 1 MOVE 5 TO WH-P.
           PERFORM BEG-DRU.
           MOVE "Teigfolgeliste" TO DRC-STR.
           MOVE WM-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM TO DRC-STR(17:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRC-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  IF IP not = 1 and not = 101 and not = 201 GO K.
           ADD 1 TO WH-KEY.
       H.  READ KONSTANT IGNORE LOCK.
       K.  MOVE IP TO DRC-NR.
           ADD IP 3000 GIVING RE-NUM.
           MOVE SPACE TO RE-BEZ.
       M.  READ REZEPT INVALID NEXT SENTENCE.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE RE-BEZ TO DRC-BEZ.
           MOVE IP TO WX.
       N.  IF WX > 100 ADD -100 TO WX GO N.
           IF KO-TGRP(WX) not = 0 MOVE KO-TGRP(WX) TO DRC-LFD.
       P.  PERFORM DRUCK.
       Z.  EXIT.
      ********************************** autom. Artikel fÅr Backzettel *
       AUTBACK SECTION.
       A.  CALL "CAUP" USING "0705031475010" WH-CREG.
           OPEN INPUT REZEPT.
           IF WF-STATUS = "35" OPEN I-O REZEPT.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " autom.Backzettelartikel " with highlight
                AT VDU-REL.
           MOVE 40 TO WH-KEY.
       B.  READ KONSTANT INVALID KEY INITIALIZE KO-QSATZ
               MOVE 40 TO KO-NUM.
           IF ZUGRIF PERFORM BESETZT GO B.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Art. Bezeichnung                %" AT VDU-REL.
           ADD 0240 VDU-ECK GIVING VDU-REL.
           DISPLAY "Art. Bezeichnung                %" AT VDU-REL.
           ADD 0303 VDU-ECK GIVING VDU-REL.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 1 TO WX.
       D.  COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK.
           IF WX > 11 ADD -1063 TO VDU-LP.
           MOVE KO-BART(WX) TO WD-RP RE-NUM.
           IF RE-NUM = 0 GO G.
           DISPLAY WD-RP with highlight AT VDU-LP.
           ADD 5 TO VDU-P.
       E.  READ REZEPT IGNORE LOCK INVALID
               MOVE "Rezeptur fehlt" TO RE-BEZ.
           DISPLAY RE-BEZ with highlight AT VDU-LP.
           ADD 26 TO VDU-LP.
           MOVE KO-BPRZ(WX) TO WD-BPZ.
           DISPLAY WD-BPZ with highlight AT VDU-LP.
       F.  IF WX < 22 ADD 1 TO WX GO D.
           MOVE 1 TO WX.
       G.  DISPLAY "<ret>= Art.Nr." AT 2301.
           DISPLAY "< >= auf, < >= ab, <esc/Ende>= Ende" AT 2401.
           MOVE KO-BART(WX) TO WH-WERT WD-RP.
           COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK.
           IF WX > 11 ADD -1063 TO VDU-LP.
           IF WH-WERT NOT = 0 DISPLAY WD-RP with highlight AT VDU-LP.
           COMPUTE VDU-LP = VDU-LP - VDU-ECK.
           CALL "CAUP" USING "1000004004" WH-CREG.
           IF ESC or ENDE GO Q.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO G.
           IF AB  GO I.
           IF NOT RET GO G.
           MOVE WH-WERT TO RE-NUM.
           IF RE-NUM = 0 DISPLAY ALL " " with SIZE 30 AT VDU-LP
               IF WX < 22 PERFORM VARYING WY FROM WX BY 1 UNTIL WY > 21
                   MOVE KO-TB(WY + 1) TO KO-TB(WY)
                   MOVE 0 TO KO-BART(WY + 1) KO-BPRZ(WY + 1)
               else MOVE 0 TO KO-BART(WX) KO-BPRZ(WX)
               END-IF GO C.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 22
               IF KO-BART(WY) = RE-NUM AND WY NOT = WX
                   DISPLAY "bereits vorhanden" AT 2401
                   PERFORM WEITER GO G.
           MOVE WH-WERT TO WD-RP.
           DISPLAY WD-RP with highlight AT VDU-LP.
           READ REZEPT IGNORE LOCK INVALID PERFORM NO-REC GO G.
           ADD 5 TO VDU-LP.
           MOVE WH-WERT TO KO-BART(WX).
           DISPLAY RE-BEZ with highlight AT VDU-LP.
       H.  DISPLAY "<ret>= Prozentsatz f. HB-Backzettel" AT 2301.
           MOVE KO-BPRZ(WX) TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 234.
           IF WX > 11 ADD -1063 TO VDU-LP.
           CALL "CAUP" USING "1000003003" WH-CREG.
           IF WOLI or AUF GO G.
           IF not RET GO H.
           MOVE WH-WERT TO KO-BPRZ(WX) WD-BPZ.
           DISPLAY WD-BPZ with highlight AT VDU-LP.
       I.  IF WX < 22 ADD 1 TO WX GO G.
       Q.  CALL "CAUP" USING "1323012440" WH-CREG.
           DISPLAY "<ret>= speichern, <>= zurÅck" AT 2301.
           DISPLAY "<esc>= Abbruch" AT 2401.
           CALL "CAUP" USING "0023310000" WH-CREG.
           CALL "CAUP" USING "1323012440" WH-CREG.
           IF ESC GO X.
           IF WOLI GO C.
           IF NOT RET GO Q.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE REZEPT.
       Z.  EXIT.
     ********************************************* Standardkonditionen *
       KONDIT SECTION.
       A.  IF WN-BUEB not = SPACE GO C.
           MOVE 7 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-FSATZ
               MOVE 7 TO KO-NUM
               WRITE KO-FSATZ.
           MOVE KO-TABK TO WH-TABK.
       C.  CALL "CAUP" USING "0702021332000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Standardkonditionen " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr. Tage   %  Tage   %  Netto" AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL "ƒ" with SIZE 30 AT VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
               COMPUTE VDU-LP = IX * 100 + 303 + VDU-ECK
               MOVE IX TO WD-KZ
               DISPLAY WD-KZ with highlight AT VDU-LP.
       D.  PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
               MOVE WT-KONDIT(IX) TO WH-KK
               PERFORM KONDIS.
           IF WL-CA = 12
               DISPLAY "Kz. +<ret>= öbernahme Kondition < >" AT 2301
               CALL "CAUP" USING "0023341001" WH-CREG
               GO X.
       E.  DISPLAY "<esc>= Abbruch, <ret-leer>= speichern, Nr+<ret>= Zei
      -        "e fÅr énderg. < >" AT 2301.
           CALL "CAUP" USING "0023681001" WH-CREG.
           IF ESC GO Q.
           IF NOT RET GO E.
           IF WN-BUEB not = SPACE DISPLAY "Bitte nur in FIBU Ñndern"
               with highlight foreground-color 4 AT 2501 GO E.
           IF WH-NUM = 0 GO Q.
           MOVE WH-NUM TO IX.
           MOVE WT-KONDIT(IX) TO WH-KK.
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
           MOVE WH-KK TO WT-KONDIT(IX).
           IF IX < 9 ADD 1 TO IX.
           GO E.
      *-----------------------------------------------------> Austieg <-
       Q.  MOVE 7 TO WH-KEY.
           READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Q.
           IF ESC MOVE KO-TABK TO WH-TABK GO W.
           MOVE WH-TABK TO KO-TABK.
           REWRITE KO-SATZ.
       W.  MOVE KO-TABK TO WH-TABK.
           UNLOCK KONSTANT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KONDIS SECTION.
       A.  COMPUTE VDU-LP = IX * 100 + 307 + VDU-ECK.
           MOVE WH-TG1 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 4 TO VDU-LP.
           MOVE WH-SK1 TO WD-PROZ.
           DISPLAY WD-PROZ WITH HIGHLIGHT AT VDU-LP.
           ADD 6 TO VDU-LP.
           MOVE WH-TG2 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 4 TO VDU-LP.
           MOVE WH-SK2 TO WD-PROZ.
           DISPLAY WD-PROZ WITH HIGHLIGHT AT VDU-LP.
           ADD 7 TO VDU-LP.
           MOVE WH-TGN TO WD-TG.
           DISPLAY WD-TG WITH HIGHLIGHT AT VDU-LP.
       Z.  EXIT.
      ************************************ Filialliste f. Auslieferung *
       FILLIST SECTION.
           CALL "CAUP" USING "270310031000014" WH-CREG.
           CALL "CAUP" USING "1303012580" WH-CREG.
           CALL "CAUP" USING "270310182600014" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " EinzÑhlliste " AT VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Kto. Bezeichnung Seite " with highlight
                background-color 7 foreground-color 8 AT VDU-LP.
           MOVE 0 TO WY.
           MOVE 60 TO WH-KEY.
       C.  READ KONSTANT INVALID INITIALIZE KF-SATZ.
           IF ZUGRIF PERFORM BESETZT GO C.
       E.  MOVE 0 TO WZ.
      *--------------------------------------------> Filiale anzeigen <-
           PERFORM VARYING WX FROM 1 by 1 UNTIL WX > 13
               COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK
               IF KF-FBEZ(WX) not = SPACE MOVE KF-FNR(WX) TO WD-APZ
                   DISPLAY WD-APZ(1:3) with highlight AT VDU-LP
                   ADD 5 TO VDU-LP
                   MOVE WX TO WZ
                   DISPLAY KF-FBEZ(WX) with highlight AT VDU-LP
                   MOVE KF-SEITE(WZ) TO WD-POS
                   COMPUTE VDU-LP = WZ * 100 + 220 + VDU-ECK
                   DISPLAY WD-POS with highlight AT VDU-LP.
       G.  IF WY = 0 and WZ < 13 ADD 1 TO WZ
           else MOVE WY TO WZ.
           IF WZ = 0 MOVE 1 TO WZ.
           MOVE 0 TO WY.
           DISPLAY "<esc>= Abbruch, < >= zurÅck, <Ende>= speichern, <ret
      -        ">= Fil.ktonr." AT 2301.
           DISPLAY "alpha+<ret>= suchen, <Einfg>= einfÅgen, <Entf>= lîsc
      -        "hen" AT 2401.
           MOVE KF-FNR(WZ) TO WH-WERT WD-APZ.
           COMPUTE VDU-LP = WZ * 100 + 203 + VDU-ECK.
           IF WH-WERT not = 0
               DISPLAY WD-APZ(1:3) with highlight AT VDU-LP.
           COMPUTE VDU-LP = WZ * 100 + 203.
           CALL "CAUP" USING "1000033003" WH-CREG.
           IF ENDE GO W.
           IF ESC GO X.
           IF AUF and WZ > 1 ADD -2 TO WZ
                  GO G.
           IF EINF PERFORM VARYING WR FROM 13 BY -1 UNTIL WR = WZ
                       MOVE KF-FNR(WR - 1) TO KF-FNR(WR)
                       MOVE KF-FBEZ(WR - 1) TO KF-FBEZ(WR)
                       MOVE KF-SEITE(WR - 1) TO KF-SEITE(WR)
                   end-perform
                   MOVE 0 TO KF-FNR(WR)
                   MOVE SPACE TO KF-FBEZ(WR)
                   MOVE 0 TO KF-SEITE(WR)
                   MOVE WZ TO WY
                   GO E.
           IF ENTF PERFORM VARYING WR FROM WZ BY 1 UNTIL WR = 13
                       MOVE KF-FNR(WR + 1) TO KF-FNR(WR)
                       MOVE KF-FBEZ(WR + 1) TO KF-FBEZ(WR)
                       MOVE 0 TO KF-FNR(WR + 1)
                       MOVE SPACE TO KF-FBEZ(WR + 1)
                   end-perform
                   MOVE WZ TO WY
                   GO E.
           IF not RET GO G.
           IF WH-NUM = 0 and WV-MCODE not = space
               CALL "PANANZ" USING "10DEB-ANZ" WH-CREG
               CANCEL "PANANZ"
               IF ESC or WH-NUM = 0 GO G
               else MOVE WH-NUM TO WH-WERT.
           MOVE WH-WERT TO KF-FNR(WZ) WD-APZ.
           DISPLAY WD-APZ(1:3) with highlight AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, < >= zurÅck, <Ende>= speichern, <ret
      -        ">= Bezeichnung" AT 2301.
           DISPLAY "max. 7 Zeichen" with highlight AT 2401.
           MOVE KF-FBEZ(WZ) TO WT-TX.
           COMPUTE VDU-LP = WZ * 100 + 208 + VDU-ECK.
           DISPLAY KF-FBEZ(WZ) with highlight AT VDU-LP.
           COMPUTE VDU-LP = WZ * 100 + 208.
           CALL "CAUP" USING "1200000107" WH-CREG.
           IF AUF and WZ > 1 ADD -2 TO WZ
                  GO G.
           IF ESC GO X.
           IF ENDE GO W.
           IF not RET GO I.
           MOVE WT-TX TO KF-FBEZ(WZ).
           DISPLAY KF-FBEZ(WZ) with highlight AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, < >= zurÅck, <ret>= Seite" AT 2301.
           DISPLAY "Seite = Nr. EinzÑhlliste (1 - 4)" AT 2401.
           MOVE KF-SEITE(WZ) TO WH-WERT WD-POS.
           COMPUTE VDU-LP = WZ * 100 + 220 + VDU-ECK.
           DISPLAY WD-POS with highlight AT VDU-LP.
           COMPUTE VDU-LP = WZ * 100 + 220.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO I.
           IF not RET GO K.
           IF WH-WERT = 0 or WH-WERT > 4 GO K.
           MOVE WH-WERT TO KF-SEITE(WZ) WD-POS.
           DISPLAY WD-POS with highlight AT VDU-LP.
           GO G.
       W.  PERFORM LEERZL.
           IF BAB GO E.
           REWRITE KF-SATZ INVALID WRITE KF-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** Leerzeilen suchen *
       LEERZL SECTION.
       A.  PERFORM VARYING WX FROM 13 BY -1 UNTIL WX =  0
               or KF-FNR(WX) not = 0 CONTINUE.
           IF WX = 0 GO Z.                       *> noch nichts belegt
           PERFORM VARYING IP FROM WX BY -1 UNTIL IP = 0
               IF KF-FNR(IP) = 0
                   DISPLAY "Leezeile nicht erlaubt!" AT 2401
                   PERFORM WEITER
                   MOVE IP TO WY
                   SET BAB TO TRUE.
       Z.  EXIT.
