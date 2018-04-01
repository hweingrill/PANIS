      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PARVORL.
      ******************************************************************
      *    keine DruckerÑnderung da nur Etikettendruck                 *
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
           COPY PARSEREZ.CPY.
           COPY PARSELFT.CPY.
           COPY PANSEKON.CPY.
           SELECT KONSLAG  ASSIGN "PAKOKONS.DAT"
                           ORGANIZATION RELATIVE, ACCESS DYNAMIC
                           RELATIVE KEY WH-KEY
                           FILE STATUS WF-STATUS.
           SELECT MODULE   ASSIGN TO "PARTEXT.DAT",
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                           RECORD KEY IS MO-KEY,
                           FILE STATUS IS WF-STATUS.
           SELECT ETIKETT  ASSIGN TO "PANETIK.DAT"
                           ORGANIZATION RELATIVE ACCESS DYNAMIC
                           RELATIVE KEY WH-KEY
                           FILE STATUS IS WF-STATUS.
           SELECT SECDAT   ASSIGN TO DATEI
                           ORGANIZATION SEQUENTIAL.
           SELECT DRUCKER  ASSIGN TO PRINTER
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
       COPY PANFDART.CPY.
       COPY PARFDART.CPY.
       COPY PARREZEP.CPY.
       COPY PARFDLFT.CPY.
      ******************************************************************
       FD  KONSLAG      external       LABEL RECORD STANDARD.
      *------------------------------------------------> WH-Key = 1 <-
       01  KL-SATZ.
           03  KL-NUM                  PIC 999     COMP-3.
           03  KL-FIRMA                PIC X(22).
           03  KL-UST                  PIC 99V99   COMP    OCCURS 6.
           03  KL-JONUM                PIC 9(6)    COMP.
           03  KL-PER                  PIC 9(6)    COMP.
           03  FILLER                  PIC X(3).
           03  KL-CHAN                 PIC 99      COMP.
           03  KL-WTOL                 PIC 9V9     COMP.
           03  KL-SPERRE               PIC X(6).
           03  KL-ORT                  PIC X(15).
           03  KL-MERK                 PIC XXXX.
           03  KL-DRU                  PIC 99      COMP.
           03  KL-ANR.
               05 KL-AN                PIC X(9)            OCCURS 7.
           03  KL-TABK.
               05 KL-KONDIT            PIC 9(13)   COMP-3  OCCURS 9.
      *------------> Schleppsummenspeicher WH-KEY = 2  user + 1 (1-5) <-
       01  KL-BSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KL-BUDAT                PIC 9(6)     COMP.
           03  KL-JOKEY                PIC X(5).
           03  KL-SASUM.
               05 KL-JSUM              PIC S9(9)V99 COMP.
               05 KL-GRUPPE            PIC S9(9)V99 COMP  OCCURS 40.
      *---------------------------------> zwingende Konten WH-KEY = 9 <-
       01  KL-TSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KL-KTONR                PIC 9(6)     COMP   OCCURS 20.
           03  KL-WIJ                            OCCURS 4.
               05 KL-WKZ               PIC 99       COMP.
               05 KL-WBEG              PIC 9(6)     COMP.
               05 KL-WEND              PIC 9(6)     COMP.
      *---------------------------------> Mengeneinheiten WH-KEY = 10 <-
       01  KL-MSATZ.
           03  FILLER                  PIC X(24).
           03  KL-MART                 PIC 9999     COMP  OCCURS 22.
           03  KL-TM.
               05 KL-MEH               PIC XXX      OCCURS 20.
      *-----------------------------> Druckerseinstellung WH-KEY = 11 <-
       01  KLD-SATZ.
           03  FILLER                  PIC X(2).
           03  KLD-VERZ                PIC X(36).
           03  KLD-APAR                PIC X(45).
           03  KLD-BPAR                PIC X(45).
      *---------------------------------------------> Druckerparamter <-
       01  KL-PRSATZ.
           03  FILLER                  PIC XX.
           03  KOP-BEZ                 PIC X(26).
           03  KOP-TX.
               05 KOP-STG              PIC X(10)     OCCURS 10.
      *----------------------------------------> Preise Key 101 - 199 <-
       01  KL-PSATZ.
           03  FILLER                  PIC XXX.
           03  KL-STELLE               PIC X(10).
           03  KL-KOART                PIC X(15).
           03  KL-ARTBEZ               PIC X(15).
           03  KL-PTAB         OCCURS 6.
               05 KL-PREIS             PIC 9(6)V99   COMP.
               05 KL-PDAT              PIC 9(6)      COMP.
      ************************************************ Security-Datei *
       FD  SECDAT.
       01  SEC-SATZ                    PIC X(128).
      *****************************************************************
       FD  ETIKETT                     LABEL RECORD STANDARD
                                       RECORD 224 CHARACTERS.
       01  ET-SATZ.
           03  ET-NUM                  PIC 9(4)        COMP.
           03  ET-ZEIL                            OCCURS 7.
               05 ET-BEZ               PIC X(30).
               05 ET-MERK              PIC 99          COMP.
           03  FILLER                  PIC X(5).
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(272).
      ******************************************* Rezepturbeschreibung *
       FD  MODULE       external       RECORD IS VARYING IN SIZE
                                       FROM 2 TO 602 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  MO-SATZ.
           03  MO-KEY.
               05 MO-NUM               PIC 9(4)      COMP.
           03  MO-TXT.
               05 MO-ZL                PIC X(40)     OCCURS 15.
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
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99      COMP-X.
           03  WX-PRSTAT               PIC 99      COMP-X.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WH-ART                  PIC 99      COMP.
           03  WD-POS                  PIC ZZ9.
           03  WD-SATZ                 PIC ZZ9,99.
           03  WD-BAR                  PIC 9(8).
           03  WH-SEC                  PIC 9(14)    COMP-X.
           03  WI                      PIC 99       COMP-X.
           03  WL                      PIC 99       COMP-X.
           03  WC-ANZ                  PIC S99      COMP.
           03  WC-FOLGE                PIC X(16)
                  VALUE x"0102090A03040B0C05060D0E07080F10".
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WH-PARM                 PIC X(45).
      *------------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
       01  WD-ETK.
      *---------------------------------> Etiketten-Druckersteuerung <-
           03  WH-ESC.
               05  FILLER              PIC X             VALUE "".
               05  WH-ST1              PIC 9999   COMP-X VALUE  0.
               05  WR-ST REDEFINES WH-ST1.
                   07  WH-ST2          PIC X.
                   07  WK-CR           PIC X.
           03  WK-LNG.
               05  FILLER              PIC X.
               05  WH-LNG1             PIC 99   COMP-X.
           03  WR-LEN REDEFINES WK-LNG.
               05  WH-LNG2             PIC 99   COMP-X.
               05  FILLER              PIC X.
           03  WR-LNG REDEFINES WK-LNG.
               05  FILLER              PIC X.
               05  WH-LNG              PIC XX.
           03  WB-BC.
               05  WC-LSL              PIC XXX   OCCURS 3.
               05  WC-CR               PIC X.
               05  WC-BIT11            PIC XXX.
               05  WC-BIT12            PIC XX.
               05  WC-STR.
                   07  WT-ST-X         PIC X OCCURS 120 INDEXED X2.
           03  WS-MUSTER.
               05  WT-MU               PIC X(108)        OCCURS 2.
           03  WH-STRI REDEFINES
               WS-MUSTER               PIC XX OCCURS 108 INDEXED X1.
      *-----------------------------------------> Etikettenparameter <-
           03  ET-HOE                  PIC 9       COMP.
           03  ET-LIN                  PIC 999     COMP.
       01  WS.
      *-----------------------------------------------> Barcode Werte <-
           03  WH-BL           VALUE "0110010010011011110101000110110001
      -         "0101111011101101101110001011000110101100110110110010000
      -         "101011100100111010100001000100100100011101001110010".
               05  WH-LIWERT           OCCURS 10.
                   07  WH-LI           PIC 9 OCCURS 7 INDEXED X3.
               05  WH-REWERT           OCCURS 10.
                   07  WH-RE           PIC 9 OCCURS 7.
           03  WH-ANFANG               VALUE "001010".
               05  WH-ANF              PIC 9 OCCURS 6.
           03  WH-ENDE                 VALUE "0101".
               05  WH-END              PIC 9 OCCURS 4.
           03  WH-MITTE                VALUE "01010".
               05  WH-MIT              PIC 9 OCCURS 5.
           03  WX-FF              VALUE "??".
               05  WR-FF               PIC X.
               05  FILLER              PIC X.
           03  WS-LR                   PIC 99     VALUE ZERO COMP-3.
           03  WM-EAN                  PIC 9(8)   VALUE ZERO.
           03  WT-EAN REDEFINES WM-EAN.
               05  WT-7                PIC 9      OCCURS 8 INDEXED IX.
       01  WH-KART     external        PIC 9999      COMP.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON LAGARTIK.
       A.  CALL "CADECL" USING "PARARTIK.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON KONSLAG.
       A.  CALL "CADECL" USING "PAKOKONS.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       DECL-M SECTION.         USE AFTER ERROR PROCEDURE ON MODULE.
       A.  CALL "CADECL" USING "PARHINW.DAT "  WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON ETIKETT.
       A.  CALL "CADECL" USING "PANETIK.DAT "  WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM VORLAUF.
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
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
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
       A.  MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              PERFORM WEITER GO A.
       Z.  EXIT.
      **************************** KO-REZPM = SPACE - kein Netzbetrieb *
       OBNETZ SECTION.
       A.  MOVE 35 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE SPACE TO WH-PARM.
           IF ZUGRIF PERFORM BESETZT GO A.
           MOVE KO-REZPM TO WH-PARM.
           MOVE 1 TO WH-KEY.
       C.  READ KONSTANT IGNORE LOCK INVALID KEY STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
       Z.  EXIT.
      ******************************************* MS-DOS/NETZ-Commands *
       COMM SECTION.
       A.  CALL X"91" USING RESULT FUNKT PARAM.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               IF WH-PARM NOT = SPACE
                   DISPLAY WH-PARM UPON COMMAND-LINE
                   PERFORM COMM
               END-IF OPEN OUTPUT DRUCKER
               PERFORM DRU-OK.
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO D.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
           MOVE SPACE TO DRA-SATZ.
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
      ******************************* Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           IF WH-PARM NOT = SPACE
               DISPLAY " " AT 1780
               DISPLAY "ENDCAP" UPON COMMAND-LINE
               PERFORM COMM
               CALL "CAUP" USING "1318012480" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       VORLAUF SECTION.
       A.  MOVE " Kalkul. & Rezepturen" TO WK-GEB.
           PERFORM LADE-DRU.
           IF WH-PG = 9 or WH-PG = 49 GO H.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           OPEN I-O REZEPT.
           OPEN INPUT ARTIKEL.
           OPEN I-O LAGARTIK.
           OPEN INPUT MODULE.
           OPEN I-O KREDIT
           MOVE "PANREZEP.DAT" TO DATEI.
           PERFORM SEC.
           MOVE "PANARTIK.DAT" TO DATEI.
           PERFORM SEC.
           OPEN I-O KONSTANT.
           OPEN INPUT KONSLAG.
           IF WF-STATUS not = "00" PERFORM HOLMENG.
       H.  MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID
               DISPLAY "Kopf im Hauptprogramm anlegen! " AT 2401
               PERFORM WEITER
               STOP RUN. 
       J.  COMMIT.
           MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           MOVE 1 TO WK-BS.
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE KO-UST(WX) TO WT-UST(WX).
       K.  MOVE 35 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID INITIALIZE KO-PSATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE KO-PARAM(WX) TO WE-PARAM(WX).
           MOVE 10 TO WH-KEY.
       N.  READ KONSLAG IGNORE LOCK INVALID CLOSE KONSLAG
               PERFORM HOLMENG.
           MOVE KL-TM TO WT-TM.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WH-PG = 49 MOVE 6 TO WH-PG GO R.
       O.  IF WX-CODE(2) = WX-CODE(1) GO P.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           MOVE 9 TO WH-PG.
           IF ESC MOVE 0 TO WH-PG GO Z.
           MOVE WT-TX TO WX-CODE(1).
           GO O.
       P.  CALL "CAUP" USING "0704201742000" WH-CREG.
           DISPLAY "Programmwahl" AT 0624.
           DISPLAY ALL "ƒ" with SIZE 12        AT 0724.
       Q.  DISPLAY "1 - Mengenermittlung"      AT 0824
                   "2 - Rezepturen"            AT 0024
                   "3 - Komponentenverwaltung" AT 0024
                   "4 - Preiswartung"          AT 0024
                   "5 - Druckprogramm"         AT 0024
                   "6 - Stammdatenverwaltung"  AT 0024
                   "7 - Listungen"             AT 0024
                   "8 - Druckereinstellung"    AT 0024
                   "9 - DatumsÑnderung"        AT 0024
                   "leer = Programmende"       AT 0024
                   "bitte wÑhlen Sie:   " with highlight AT 0039.
           DISPLAY "<F1>= Hilfe, <esc>= Abbruch" AT 2301.
           MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "0018572002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF HELP CALL "CAUP" USING "190011001201" WH-CREG GO Q.
           IF ESC MOVE 0 TO WH-PG
               SET RET TO TRUE.
           IF NOT RET GO Q.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       R.  EVALUATE WH-PG
               WHEN 0 CLOSE REZEPT ARTIKEL MODULE KONSTANT KREDIT
                            KONSLAG
                      CALL "CAUP" USING "1301012480" WH-CREG
                      DISPLAY " Programm beendet " AT 2030
                      DISPLAY " " AT 2380
                      STOP RUN
               WHEN 1 MOVE "PARDRU  10MASSEN" TO WT-TX
               WHEN 2 MOVE "PAREZANL20RPT"    TO WT-TX
               WHEN 3 MOVE "PARKOMKA50KOMPO"  TO WT-TX
               WHEN 4 MOVE "PARKOMKA50KOMPO"  TO WT-TX
               WHEN 5 CALL "CADRUCK" USING "40DRUCK" WH-CREG
                      CANCEL "CADRUCK" GO P
               WHEN 6 PERFORM STAM-MENU
               WHEN 7 MOVE "PARDRU  50LIWAHL" TO WT-TX
               WHEN 8 PERFORM DRUPAR
               WHEN 9 MOVE 0 TO WZ-DATUM WH-PG
                      CALL "CAUP" USING "03DATUM" WH-CREG
      *        WHEN 11 CALL "PARCONV" USING "20CONV" WH-CREG
      *        WHEN 10 CALL "PARCONV" USING "10CONV" WH-CREG
               WHEN OTHER GO P.
       Z.  EXIT.
      ************************************** Holen der Mengeneinheiten *
       HOLMENG SECTION.
       A.  OPEN I-O KONSLAG.
           MOVE 10 TO WH-KEY.
       C.  READ KONSTANT IGNORE LOCK INVALID KEY
               MOVE WH-KEY TO KO-NUM
               MOVE "StkKg daggr KtnSckNtzPckPkg" TO KO-TM
               WRITE KO-SATZ GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE KO-TM TO WT-TM.
           INITIALIZE KL-MSATZ
           MOVE WT-TM TO KL-TM.
           MOVE WH-KEY TO KL-NUM.
           WRITE KL-SATZ.
           CLOSE KONSLAG.
           OPEN INPUT KONSLAG.
       Z.  EXIT.
      ******************************************************************
       sec section.
       a.  open i-o secdat.
           read secdat at end go x.
           compute wh-sec = 92837465 * 970328 + 28473703.
           if sec-satz(23:14) = wh-sec go x.
           move sec-satz(23:6) to wz-datum.
           add 4 to wz-monat.
           if wz-monat > 12 add 1 to wz-jahr
               subtract 12 from wz-monat.
           if wz-datum > wh-datum go x.
           display "Security-Error (007) - " at 2401.
           display "weiter mit <ret>" at 2424.
           call "CAUP" using "0124416006" wh-creg.
           compute wv-datum = wh-datum + 4711 - 470000.
           if wv-datum not = wh-num stop run.
           move wh-sec to sec-satz(23:14).
           rewrite sec-satz.
       x.  close secdat.
       z.  exit.
      ****************************************************** UntermenÅ *
       STAM-MENU SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
       C.  CALL "CAUP" USING "0704201542000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Stammdatenverwaltung " AT VDU-LP.
           DISPLAY "Auswahl" AT 0624.
           DISPLAY ALL "ƒ" with SIZE 7        AT 0724.
           DISPLAY "1 - Lieferanten"           AT 0824
                   "2 - Mengeneinheiten"       AT 0024
                   "3 - Komponentenverwaltung" AT 0024
                   " "                         AT 0024
                   "4 - Faktorenverwaltung"    AT 0024
                   "5 - Etikettendruck"        AT 0024
                   "6 - Druckereinstellung"    AT 0024
                   "leer = Programmende"       AT 0024
                   "bitte wÑhlen Sie:   " with highlight AT 0039.
           DISPLAY "<esc>= Abbruch" AT 2301.
           MOVE 0 TO WH-NUM.
       E.  CALL "CAUP" USING "0016572002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC MOVE 0 TO WH-PG
               SET RET TO TRUE.
           IF NOT RET GO E.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           EVALUATE WH-PG
               WHEN 0 GO Z
               WHEN 1 MOVE 49 TO WH-PG
                      MOVE "PAKOKRED22LFTAN" TO WT-TX GO Z
               WHEN 2 MOVE 0 TO WL-CA
                      PERFORM MENGEN GO A
               WHEN 3 MOVE 49 TO WH-PG
                      MOVE "PARKOMKA56KOMPO" TO WT-TX GO Z
               WHEN 4 PERFORM FAKTO GO A
               WHEN 5 PERFORM ETIKWAHL
               WHEN 6 PERFORM DRUPAR
               WHEN OTHER GO C.
       Z.  EXIT.
      ***************************** Druckerparameter Sofortliefrschein *
       DRUPAR SECTION.
       A.  CALL "CAUP" USING "0710100550000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druckersteuerung Rezepturprogramm "
               with highlight AT VDU-LP.
           MOVE 35 TO WH-KEY.
       C.  READ KONSTANT INVALID MOVE SPACE TO KO-REZPM.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 35 TO KO-NUM.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY KO-REZPM with highlight AT VDU-LP.
       E.  MOVE KO-REZPM TO WT-TX.
           DISPLAY "<ret>= Druckernummer oder Befehl" AT 2301.
           DISPLAY "1: 1. Netzdrucker, 2: 2. Netzdrucker, 0: kein Netzdr
      -        "ucker" AT 2301.
           CALL "CAUP" USING "1203030145" WH-CREG.
           IF WT-TX(2:44) = SPACE; IF WR-TX(1) = 0 MOVE SPACE TO WT-TX
               ELSE IF WR-TX(1) = 1 OR 2
               MOVE "CAPTURE P=0 Q=PRINTQ_0 NB TI=0 L=1 NFF" TO KO-REZPM
               IF WR-TX(1) = 2 MOVE 1 TO KO-REZPM(11:1) KO-REZPM(22:1)
               END-IF MOVE KO-REZPM TO WT-TX.
           IF NOT RET AND NOT KIST GO E.
           MOVE WT-TX TO KO-REZPM.
           IF KO-REZPM = SPACE DISPLAY "Druck auf Workstationdrucker"
               with highlight AT VDU-LP
           ELSE DISPLAY KO-REZPM with highlight AT VDU-LP.
       G.  DISPLAY "<ret>= speichern, <>= zurÅck, <esc>= Abbruch"
               AT 2301.
           CALL "CAUP" USING "0023470000" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RET GO G.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE SPACE TO WT-TX.
       Z.  EXIT.
      ************************************** Etikettendruckverwaltung *
       ETIK SECTION.
       A.  OPEN I-O ETIKETT.
           IF WF-STATUS = 94 OPEN OUTPUT ETIKETT
                             CLOSE ETIKETT GO A.
           PERFORM OBNETZ.
       B.  MOVE "  Textetiketten" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
       C.  DISPLAY "Etikett:" with highlight AT 0510.
           DISPLAY "<esc>= Ende, <ret>= Etikett-Nr., <tab>= nÑchste Nr."
                AT 2301.
           CALL "CAUP" USING "0005193003" WH-CREG.
           IF ESC GO X.
           IF TABL GO W.
           IF NOT RET OR WH-NUM = 0 GO B.
           MOVE WH-NUM TO WH-KEY WD-POS.
           DISPLAY WD-POS with highlight AT 0519.
       E.  READ ETIKETT INVALID MOVE SPACE TO ET-SATZ.
           IF ZUGRIF PERFORM BESETZT GO E.
       F.  DISPLAY "7-Zeilen zu 30 Zeichen" AT 0926
               "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø" AT 0021
               "≥                                ≥" AT 0021
               "≥                                ≥" AT 0021
               "≥                                ≥" AT 0021
               "≥                                ≥" AT 0021
               "≥                                ≥" AT 0021
               "≥                                ≥" AT 0021
               "≥                                ≥" AT 0021
               "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ" AT 0021.
           MOVE 1 TO WX.
       G.  COMPUTE VDU-L = WX + 10.
           MOVE 23 TO VDU-P.
           DISPLAY ET-BEZ(WX) AT VDU-LP.
           IF WX < 7 ADD 1 TO WX GO G.
       J.  DISPLAY "<tab>= naechste Nr." AT 2401.
           DISPLAY "<ret-leer>= énderung, <ret-Anzahl>= Druck, <esc>= En
      -        "de " AT 2301.
           CALL "CAUP" USING "0023563003" WH-CREG.
           IF ESC GO B.
           IF TABL GO W.
           IF NOT RET GO J.
           IF WV-MC(1) = "#" GO B.
           IF WH-NUM = 0 GO M.
      *---------------------------------------------> Etikettendruck <-
           MOVE x"0000" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           PERFORM BEG-DRU.
           MOVE 1 TO WX.
       K.  MOVE WH-NUM TO WD-POS.
           DISPLAY WD-POS with highlight AT 1459.
       L.  MOVE ET-BEZ(WX) TO DRA-SATZ(7:).
           PERFORM DRUCK.
           IF WX < 7 ADD 1 TO WX GO L.
           MOVE 3 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           IF WH-NUM > 1 SUBTRACT 1 FROM WH-NUM
               MOVE 1 TO WX GO K.
           GO B.
      *--------------------------------------> Anlage oder Aenderung <-
       M.  MOVE 1 TO WX.
       N.  MOVE ET-BEZ(WX) TO WT-TX.
           COMPUTE VDU-L = WX + 10.
           CALL "CAUP" USING "0200230131" WH-CREG.
           IF WOLI; IF WX > 1 SUBTRACT 1 FROM WX GO N
               ELSE GO N.
           IF ESC GO J.
           MOVE WT-TX TO ET-BEZ(WX).
           IF WX < 7 ADD 1 TO WX GO N.
           MOVE WH-KEY TO ET-NUM.
           WRITE ET-SATZ INVALID REWRITE ET-SATZ.
           GO J.
       U.  IF WH-KEY > 0 GO W.
           START ETIKETT KEY > WH-KEY INVALID KEY GO B.
       W.  READ ETIKETT NEXT AT END MOVE 0 TO WH-KEY GO U.
           IF ZUGRIF PERFORM BESETZT GO W.
           MOVE ET-NUM TO WD-POS.
           DISPLAY WD-POS with highlight AT 0519.
           GO F.
       X.  CLOSE ETIKETT.
           IF WM-OPEN > 0 PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       FAKTO SECTION.
       A.  MOVE 6 TO WH-KEY.
           READ KONSTANT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO A.
           IF KO-FIRMA = SPACE MOVE 0 TO KO-FAKT(1)
               KO-FAKT(2) KO-FAKT(3) KO-FAKT(4)
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 16
                   MOVE 0 TO KO-PROZ(WZ)
               end-perform REWRITE KO-SATZ GO A.
           IF KO-PROZ(9)(1:) = SPACE
               PERFORM VARYING WZ FROM 9 BY 1 UNTIL WZ > 16
                   MOVE 0 TO KO-PROZ(WZ).
           CALL "CAUP" USING "0705101262000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Faktoren " with highlight AT VDU-REL.
       C.  ADD 227 VDU-ECK GIVING VDU-REL.
           DISPLAY "     BÑckerei        Konditorei" AT VDU-REL.
           ADD 627 VDU-ECK GIVING VDU-REL.
           DISPLAY " engros   detail   engros   detail" AT VDU-REL.
           ADD 303 VDU-ECK GIVING VDU-REL.
           DISPLAY "Minutensatz" AT VDU-REL.
           ADD 403 VDU-ECK GIVING VDU-REL.
           DISPLAY "Prod.-Gmeinkosten" AT VDU-REL.
           ADD 703 VDU-ECK GIVING VDU-REL.
           DISPLAY "Retourwarenanteil" AT VDU-REL.
           ADD 803 VDU-ECK GIVING VDU-REL.
           DISPLAY "Vertriebskosten" AT VDU-REL.
           ADD 903 VDU-ECK GIVING VDU-REL.
           DISPLAY "Verwaltungskosten" AT VDU-REL.
           ADD 1003 VDU-ECK GIVING VDU-REL.
           DISPLAY "Risiko u. Gewinn" AT VDU-REL.
           ADD 332 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WX FROM 1 BY 2 UNTIL WX > 4
               MOVE KO-FAKT(WX) TO WD-SATZ
               DISPLAY WD-SATZ with highlight AT VDU-REL "%"
               MOVE KO-FAKT(WX + 1) TO WD-SATZ
               ADD 18 TO VDU-REL
               DISPLAY WD-SATZ with highlight AT VDU-REL "%"
               ADD 82 TO VDU-REL.
           ADD 727 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WX FROM 1 BY 2 UNTIL WX > 8
               MOVE KO-PROZ(WX) TO WD-SATZ
               DISPLAY WD-SATZ with highlight AT VDU-REL "%"
               MOVE KO-PROZ(WX + 1) TO WD-SATZ
               ADD 9 TO VDU-REL
               DISPLAY WD-SATZ with highlight AT VDU-REL "%"
               MOVE KO-PROZ(WX + 8) TO WD-SATZ
               ADD 9 TO VDU-REL
               DISPLAY WD-SATZ with highlight AT VDU-REL "%"
               MOVE KO-PROZ(WX + 9) TO WD-SATZ
               ADD 9 TO VDU-REL
               DISPLAY WD-SATZ with highlight AT VDU-REL "%"
               ADD 73 TO VDU-REL.
           MOVE 1 TO WX.
       E.  EVALUATE WX
               WHEN 1 MOVE 332 TO VDU-LP
               WHEN 2 MOVE 350 TO VDU-LP
               WHEN 3 MOVE 432 TO VDU-LP
               WHEN 4 MOVE 450 TO VDU-LP.
           MOVE KO-FAKT(WX) TO WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= énderung" AT 2401.
           CALL "CAUP" USING "1000003206" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           IF WX < 5 MOVE WH-WERT TO KO-FAKT(WX).
           MOVE WH-WERT TO WD-SATZ.
           DISPLAY WD-SATZ with highlight AT VDU-LP "%".
           IF WX < 4 ADD 1 TO WX GO E.
      *----------------------------------------------------> Prozente <-
           MOVE 1 TO WX.
       G.  SUBTRACT 1 FROM WX GIVING WY.
           DIVIDE 4 INTO WY GIVING WZ REMAINDER WY.
           COMPUTE VDU-LP = (WZ * 100) + (WY * 9) + 727.
           MOVE WC-FOLGE(WX:1) TO WI(1:).
           MOVE KO-PROZ(WI) TO WH-WERT.
       H.  DISPLAY "<esc>= Abbruch, <ret>= énderung" AT 2401.
           CALL "CAUP" USING "1000003206" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO H.
           MOVE WH-WERT TO KO-PROZ(WI) WD-SATZ.
           DISPLAY WD-SATZ with highlight AT VDU-LP "%".
           IF WX < 16 ADD 1 TO WX GO G.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ******************************************************************
       ETIKWAHL SECTION.
       A.  CALL "CAUP" USING "0710100639000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Textetiketten" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "2 - Barcodeetiketten" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "leer = Programmende" AT VDU-LP.
           ADD 516 VDU-ECK GIVING VDU-LP.
           DISPLAY "bitte wÑhlen Sie:   " with highlight AT VDU-LP.
       C.  DISPLAY "<F1>= Hilfe, <esc>= Abbruch" AT 2301.
           MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "1005341001" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF HELP CALL "CAUP" USING "190000000001" WH-CREG GO C.
           IF WH-PG = 1 PERFORM ETIK GO X.
           IF ESC GO X.
           IF NOT RET GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WH-PG = 2 PERFORM BARCODE.
       Z.  EXIT.
      *********************************************** Barcodeetiketten *
       BARCODE SECTION.
       A.  CALL "CAUP" USING "0710150545000" WH-CREG.
           MOVE 0 TO WM-OPEN WZ-SCHALT WZ-ZEILEN.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Barcodeetiketten " with highlight AT VDU-LP.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.: " AT VDU-LP.
           MOVE "28360018" TO WD-BAR WH-WERT.
           MOVE "2836001" TO WH-WERT.
           DISPLAY WD-BAR with highlight AT 0000.
       C.  DISPLAY "<ret-leer>= widerholen, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "1003087007" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           MULTIPLY WH-WERT BY 10 GIVING WH-EAN.
           PERFORM BAR.
           MOVE WH-EAN TO WD-BAR.
           DISPLAY WD-BAR with highlight AT VDU-LP.
       D.  DISPLAY "<ret>= Anzahl, <esc>= zurÅck" AT 2301.
           ADD 318 VDU-ECK GIVING VDU-LP.
           DISPLAY "Anzahl:  " AT VDU-LP.
           CALL "CAUP" USING "1003262002" WH-CREG.
           IF ESC GO B.
           IF NOT RET GO D.
           MOVE WH-WERT TO WC-ANZ WD-POS.
           IF WC-ANZ = 0 GO D.
           DISPLAY WD-POS with highlight AT VDU-LP.
       F.  DISPLAY "<ret>= Bezeichnung, <esc>= zurÅck" AT 2301.
           DISPLAY "max. LÑnge 11 Zeichen; bei weniger links und rechts
      -        "auf gleichen Abstand achten" AT 2401.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE "Vitalisbrot" TO WT-TX.
           DISPLAY WT-TX(1:12) with highlight AT VDU-LP.
           CALL "CAUP" USING "1204030111" WH-CREG.
           IF ESC GO D.
           IF NOT RET GO F.
           DISPLAY WT-TX(1:12) with highlight AT VDU-LP.
           MOVE SPACE TO WT-ADR.
           MOVE x"0000" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           PERFORM BEG-DRU.
       Q.  PERFORM BAR.
           ADD -2 TO WC-ANZ.
           MOVE 0 TO WL.
           MOVE 6 TO ET-HOE.
           MOVE 9 TO ET-LIN.
           MOVE 1 TO WZ-SCHALT.
           MOVE X"1B40" TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE x"1B33001B21001B6C050D1B2A010000" TO DRA-SATZ(1:).
           MOVE WT-TX TO DRA-SATZ(16:11).
           MOVE DRA-SATZ(11:16) TO DRA-SATZ(35:).
           WRITE DRA-SATZ AFTER 0.
           MOVE SPACE TO DRA-SATZ.
           PERFORM EANDRU.
           MOVE SPACE TO DRA-SATZ.
           MOVE 3 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE X"1B4A17" TO DRA-SATZ.
           WRITE DRA-SATZ AFTER 0.
           IF WC-ANZ > 0 GO Q.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WM-OPEN > 0 MOVE X"1B40" TO DRA-SATZ(1:)
              PERFORM DRUCK
              CLOSE DRUCKER
              MOVE 0 TO WM-OPEN.
           MOVE SPACE TO WT-TX.
       Z.  EXIT.
      ******************************************************************
       BAR SECTION.
       A.  MOVE WH-EAN TO WM-EAN.
           ADD WT-7(1) WT-7(3) WT-7(5) WT-7(7) GIVING WI.
           MULTIPLY 3 BY WI.
           ADD WT-7(2) WT-7(4) WT-7(6) TO WI.
           DIVIDE 10 INTO WI GIVING WI REMAINDER WI.
           SUBTRACT WI FROM 10 GIVING WT-7(8).
           MOVE WM-EAN TO WH-EAN.
           MOVE 3 TO ET-HOE.
           MOVE 9 TO ET-LIN.
           SET X3 TO 1.
           MOVE LOW-VALUE TO WS-MUSTER.
           SET X1 IX TO 1.
           MOVE 1 TO WI.
       B.  IF WH-ANF(WI) = 1 MOVE WX-FF TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF WI < 6 ADD 1 TO WI GO B.
       E.  IF WT-7(IX) = 0 MOVE 10 TO WS-LR
                ELSE MOVE WT-7(IX) TO WS-LR.
       F.  IF IX > 4; IF WH-RE(WS-LR X3) = 1 MOVE WX-FF TO WH-STRI(X1)
                      ELSE MOVE  LOW-VALUE TO WH-STRI(X1)
                 ELSE IF WH-LI(WS-LR X3) = 1 MOVE WX-FF TO WH-STRI(X1)
                      ELSE MOVE LOW-VALUE TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF X3 < 7 SET X3 UP BY 1 GO F.
           SET X3 TO 1.
           IF IX NOT = 4 GO I.
           MOVE 2 TO WI.
       H.  IF WH-MIT(WI) = 1 MOVE WX-FF TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF WI < 5 ADD 1 TO WI GO H.
       I.  SET IX UP BY 1.
           IF IX NOT > 8 GO E.
           MOVE 2 TO WI.
       K.  IF WH-END(WI) = 1 MOVE WX-FF TO WH-STRI(X1).
           SET X1 UP BY 1.
           IF WI < 4 ADD 1 TO WI GO K.
       Z.  EXIT.
      ***********************************************************************
       ZAHLEN SECTION.
       A.  MOVE LOW-VALUE TO WC-STR.
           MOVE 7 TO WH-ST1.
           MOVE "l" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(3).
           SET X2 TO 1.
           SET EX TO 6.
       C.  MOVE WR-EAN(EX) TO WT-ST-X(X2).
           IF X2 < 8 SET EX X2 UP BY 1 GO C.
           MOVE 0 TO WH-LNG1.
           MOVE 0 TO WH-LNG2.
           MOVE WK-LNG TO WC-BIT12.
           move space to dra-satz.
           move wb-bc(1:23) to dra-satz(1:23).
           move wb-bc(11:13) to dra-satz(34:).
           write dra-satz after 0.
           MOVE 0 TO WH-ST1.
           MOVE "l" TO WH-ST2.
           WRITE DRA-SATZ FROM WH-ESC AFTER 0.
       Z.  EXIT.
      ******************************************************************
       ANZAHL SECTION.
       A.  MOVE "l" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(3).
           MOVE 0 TO WH-LNG1.
           MOVE 120 TO WH-LNG2.
           SET X2 TO 120.
       C.  IF WT-ST-X(X2) NOT = "?" AND
              WT-ST-X(X2) NOT = LOW-VALUE SUBTRACT 1 FROM WH-LNG2
              IF X2 > 1 SET X2 DOWN BY 1 GO C.
           MOVE WK-LNG TO WC-BIT12.
           move wb-bc to dra-satz(1:).
           move wb-bc(11:) to dra-satz(133:).
           write dra-satz after 0.
      *    WRITE DRA-SATZ FROM WB-BC AFTER 0.
       Z.  EXIT.
      ************************************** Lieferschein mit EAN-Code *
       EANDRU SECTION.
       A.  MOVE SPACE TO WB-BC.
           SUBTRACT WL FROM ET-LIN GIVING WZ-SCHALT.
           ADD WZ-SCHALT TO WL.
           MULTIPLY 3 BY WZ-SCHALT GIVING WH-ST1.
           MOVE 0 TO WZ-SCHALT.
           MOVE "3" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(1).
           IF ET-HOE = 1 ADD 09 TO WH-ST1
               MOVE WH-ESC TO WC-LSL(1)
               MOVE X"1B210100" TO DRA-SATZ(1:)
               PERFORM DRUCK
               MOVE 0 TO WH-ST1
               MOVE "l" TO WH-ST2
               WRITE DRA-SATZ FROM WH-ESC AFTER 0
               MOVE WT-ADR TO DRA-SATZ
               WRITE DRA-SATZ.
           WRITE DRA-SATZ FROM WB-BC BEFORE 1.
           MOVE 0 TO WH-ST1.
           MOVE "3" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(1).
           MOVE 1 TO WH-ST1.
           MOVE "*" TO WH-ST2.
           MOVE WH-ESC TO WC-BIT11.
           MOVE 0 TO WH-LNG1.
           MOVE 120 TO WH-LNG2.
           MOVE WK-LNG TO WC-BIT12.
           MOVE 0 TO WH-ST1.
           MOVE "!" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(2).
           MOVE 13 TO WH-ST1.
           MOVE WK-CR TO WC-CR.
           IF ET-HOE = 1 MOVE LOW-VALUE TO WS-MUSTER.
           MOVE 5 TO WH-ST1.
           MOVE WT-MU(1) TO WC-STR.
           PERFORM ANZAHL.
           MOVE 14 TO WH-ST1.
           MOVE WT-MU(2) TO WC-STR.
           PERFORM ANZAHL.
           IF ET-HOE < 2 PERFORM ZAHLEN.
           SUBTRACT 1 FROM ET-HOE.
           ADD 6 TO ET-LIN.
           IF ET-HOE > 0 GO A.
           MOVE 0 TO WH-ST1.
           MOVE "2" TO WH-ST2.
           WRITE DRA-SATZ FROM WH-ESC AFTER 0.
           ADD 3 TO WZ-ZEILEN.
           MOVE X"1B210100" TO DRA-SATZ(1:).
           WRITE DRA-SATZ AFTER 0.
           MOVE 1 TO WZ-SCHALT.
           MOVE SPACE TO DRA-SATZ.
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
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************* Steuerzeichen LASER laden **
       LADE-DRU SECTION.
      *                                   1  *> quer A4 /10" 6 Zeilen <-
           MOVE "&l26a1h6d1O(s0p10h0b0s3T" TO WE-STG(1).
      *                                   2  *> quer A4 /12" 6 Zeilen <-
           MOVE "&l26a1h6d1O(s0p12h0b0s3T" TO WE-STG(2).
      *                                   3  *> quer A4 /16" 6 Zeilen <-
           MOVE "&l26a1h6d1O(s0p14.6h0b0s3T" TO WE-STG(3).
      *                                   4  *> hoch A4 /10" 6 Zeilen <-
           MOVE "&l26a1h6d0O(s0p10h0b0s3T" TO WE-STG(4).
      *                                   5  *> hoch A4 /12" 6 Zeilen <-
           MOVE "&l26a1h6d0O(s0p12h0b0s3T" TO WE-STG(5).
      *                                   6  *> hoch A4 /16" 6 Zeilen <-
           MOVE "&l26a1h6d0O(s0p14.6h0b0s3T" TO WE-STG(6).
      *                                   7  *> wie 5 nur fetter      <-
           MOVE "&l26a1h6d0O(s0p10h3b0s6T" TO WE-STG(7).
      *                                   8  *> wie 2 etwas fetter    <-
           MOVE "&l26a1h6d1O(s0p12h1b0s6T" TO WE-STG(8).
      *                                   9  *> quer A4 /16" 6 Zeilen <-
           MOVE "&l26a1h6d1O(s0p20.0h0b0s6T" TO WE-STG(9).
      *    &la: Format, c: 6 Zeilen, 1/0O: quer/hoch
      *    (sp: Abstand, h: Zeich./Zoll b: StÑrke s: Schrift T: Schrift
       Z.  EXIT.
