      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PAKOVORL.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PAKOSEC.CPY.
           COPY PARSEART.CPY.
           COPY PARSELFT.CPY.
           COPY PARSEREZ.CPY.
           SELECT SECDAT   ASSIGN TO DATEI
                           ORGANIZATION SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       COPY PAKOFD.CPY.
       COPY PARFDART.CPY.
       COPY PARREZEP.CPY.
       COPY PARFDLFT.CPY.
      ************************************************ Security-Datei *
       FD  SECDAT.
       01  SEC-SATZ                    PIC X(128).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WH-K                    PIC 99.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99       COMP.
           03  WH-P2                   PIC 99       COMP.
           03  WZ-SCHALT               PIC 99       COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99       COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 99       COMP-3 VALUE ZERO.
           03  WM-OPEN                 PIC 9        COMP-3 VALUE ZERO.
           03  WD-FNR                  PIC Z9.
           03  WE-EXTN                 PIC X(5).
           03  WE-EXTV.
               05 WR-VAL               PIC 9.
       COPY PAKOEXT.CPY.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       COPY PAKODECL.CPY.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM VORLAUF.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       VORLAUF SECTION.
       A.  MOVE "  Lagerbuchhaltung" TO WK-GEB.
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           IF WE-USE > 7 MOVE 0 TO WH-PG GO R.
           IF WH-PG = 9 GO B.
           INITIALIZE WE-EXT.
           PERFORM LADE-DRU.
           MOVE "PAKNR" TO WE-EXTN.
           CALL "_EXTNAME" USING WE-EXTN WE-EXTV.
           IF WR-VAL(1:) = SPACE MOVE 1 TO WR-VAL.
           MOVE WR-VAL TO WE-USE.
           MOVE WK-BS(2:1) TO WX.
           COMPUTE WK-BS = WX * 10 + WE-USE.

      *    call "pak2000" using "10" wh-creg
      *                  on overflow NEXT SENTENCE.       *> konv. 2000
      *    CANCEL "PAk2000".

           MOVE "PAKWAAG.DAT" TO WN-BUEB.
           MOVE "PAKOKONS.DAT" TO WN-KONS.
           MOVE "PARARTIK.DAT" TO WN-KOST.
           MOVE "PAKLAG01.DAT" TO WN-BZEI.
           OPEN I-O LAGARTIK.
           OPEN I-O KONSLAG.
           OPEN I-O LAGERBZ.
           OPEN I-O KREDIT.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
       B.  MOVE 1 TO WH-KEY.
           READ KONSLAG NOT INVALID GO J.
           IF ZUGRIF PERFORM BESETZT GO B.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "bitte Kopf anlegen -" AT 2401.
           INITIALIZE KL-SATZ.
           MOVE 1 TO KL-NUM.
       C.  ACCEPT KL-FIRMA WITH HIGHLIGHT AT 0127.
           IF KL-FIRMA = SPACE GO C.
           DISPLAY KL-FIRMA WITH HIGHLIGHT AT 0127.
           MOVE "jowe" TO KL-SPERRE.
           MOVE 20 TO KL-UST(1).
           MOVE 10 TO KL-UST(2).
           MOVE 30 TO KL-UST(3).
           MOVE 0 TO KL-UST(4) KL-UST(5) KL-UST(6).
           MOVE "Firma    Familie  Herrn    Frau     An das   An die
      -        "An" TO KL-ANR.
           INITIALIZE WH-TABK.
           MOVE WH-TABK TO KL-TABK.
           WRITE KL-SATZ.
           MOVE 9 TO WH-KEY.
           INITIALIZE KL-TSATZ.
           MOVE WH-KEY TO KL-NUM.
           WRITE KL-SATZ.
           GO B.
       J.  UNLOCK KONSLAG.
      *    MOVE KL-FIRMA TO WK-FIRMA.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE KL-SPERRE TO WX-CODE(2).
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE KL-UST(WX) TO WT-UST(WX).
           MOVE KL-ANR TO WH-TABTX.
           MOVE KL-TABK TO WH-TABK.
           MOVE KL-MERK TO WK-M.
           MOVE KL-DRU TO WM-DRU.
           MOVE KL-WTOL TO WE-WTOL.
           MOVE KL-CHAN TO WE-CHAN.
       K.  MOVE 9 TO WH-KEY.
           READ KONSLAG INVALID MOVE 0 TO KL-WBEG(1).
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE 0 TO WY WE-WKZ WE-WJAHR(1) WE-WJAHR(2).
           PERFORM VARYING WX FROM 4 BY -1 UNTIL WX = 0
               if KL-wbeg(wx) not = 0 move kl-wbeg(wx) to wv-datum
      *           perform dat2000
                  if wm-datum >= KL-wbeg(wx) and wm-datum <= KL-wend(wx)
                      move wx to wy.
           if wy not = 0
               ADD KL-WKZ(WY) 50 GIVING WH-K
               MOVE WH-K TO WE-WKZ
               MOVE KL-WBEG(WY) TO WE-WJAHR(1)
               MOVE KL-WEND(WY) TO WE-WJAHR(2).
           IF WE-WJAHR(1) = 0 OR WE-WJAHR(2) = 0
               DISPLAY "Wirtschaftjahr nicht angelegt" AT 2401
               PERFORM WEITER.
       L.  MOVE 10 TO WH-KEY.
           READ KONSLAG WITH NO LOCK INVALID KEY
               INITIALIZE KL-MSATZ
               MOVE 10 TO WH-KEY KL-NUM
               MOVE "StkKg daggr KtnSckNtzPckPkg" TO KL-TM
               WRITE KL-SATZ
               GO L.
           IF ZUGRIF PERFORM BESETZT GO L.
           MOVE KL-TM TO WT-TM.
           IF WE-USE = 1 MOVE 1 TO WH-PG;
                IF ESC MOVE 0 TO WH-PG GO R else GO Q.
       M.  ADD WN-FNR 120 GIVING WH-KEY.
           READ KONSLAG WITH NO LOCK INVALID KEY
               INITIALIZE KL-FSATZ
               MOVE 1 TO KL-NUM
               MOVE WK-FIRMA TO KL-FIBEZ
               WRITE KL-SATZ
               GO M.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE KL-FIBEZ TO WK-FIRMA.
           CALL "CAUP" USING "06KOPF" WH-CREG.
       P.  IF WX-CODE(2) = WX-CODE(1) GO Q.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           IF ESC MOVE 0 TO WH-PG GO R.
           MOVE WT-TX TO WX-CODE(1).
           GO P.
       Q.  IF WH-PG = 1 MOVE "PAKOERF 10VERW" TO WT-TX GO Z.
           DISPLAY "<esc>= Abbruch, < >= Firmenwechsel" AT 2301.
           DISPLAY "Programmwahl"           AT 0524
                   "컴컴컴컴컴컴"           AT 0024
                   "1 - Verwiegen"          AT 0024
                   "2 - Buchen"             AT 0024
                   "3 - Eingangsrechnung"   AT 0024
                   "4 - Rezeptbilanz"       AT 0024
                   "5 - Inventurerfassung"  AT 0024
                   "6 - Druckprogramm"      AT 0024
                   "7 - Stammdaten"         AT 0024
                   "8 - Auswertungen"       AT 0024
                   "9 - Datums꼗derung"     AT 0024
                   "leer = Programmende"    AT 0024
                   " bitte w꼑len Sie:   " with highlight AT 1939.
           CALL "CAUP" USING "0019582002" WH-CREG.
       R.  IF ESC MOVE 0 TO WH-NUM
               SET RET TO TRUE.
           IF AUF PERFORM FIRMA
                  CALL "CAUP" USING "06KOPF" WH-CREG
                  GO Q.
           IF NOT RET GO Q.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 CALL "CAUP" USING "1301012480000" WH-CREG
                  CLOSE LAGARTIK KONSLAG LAGERBZ KREDIT
                  DISPLAY " Programm beendet " AT 1231
                  STOP RUN
               WHEN 1 MOVE "PAKOERF 10VERW" TO WT-TX
               WHEN 2 MOVE "PAKOERF 20BUCH" TO WT-TX
               WHEN 3 MOVE "PAKOERF 30EINR" TO WT-TX
               WHEN 4 MOVE "PAKOBIL 30BIL"  TO WT-TX
               WHEN 5 MOVE "PAKOERF 21BUCH" TO WT-TX
               WHEN 6 MOVE "CADRUCK 40DRUK" TO WT-TX
               WHEN 7 MOVE "PAKOSTAM60STAM" TO WT-TX
               WHEN 8 MOVE "PAKOLIS 70LIST" TO WT-TX
               when 69 perform mach-null go b
               WHEN 9 MOVE 0 TO WZ-DATUM WH-PG
                      CALL "CAUP" USING "03DATUM" WH-CREG GO B.
       Z.  EXIT.
      ************************************************** Firmenwechsel *
       FIRMA SECTION.
       A.  CALL "CAUP" USING "0710100340000" WH-CREG.
           CLOSE LAGERBZ.
       C.  CALL "CAUP" USING "16CL5FEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           MOVE WN-FNR TO WD-FNR.
           DISPLAY "Firma: " AT VDU-LP WD-FNR with highlight " ".
           ADD WN-FNR 120 GIVING WH-KEY.
       E.  READ KONSLAG NO LOCK INVALID MOVE "- fehlt -" TO KL-FIBEZ.
           IF ZUGRIF PERFORM BESETZT GO E.
           DISPLAY KL-FIBEZ with highlight AT 0000.
           DISPLAY "<ret>= Firma, <ret-leer>= Ende" AT 2301.
           MOVE WN-FNR TO WH-WERT WD-FNR.
           CALL "CAUP" USING "1002102002" WH-CREG.
           IF not RET GO C.
           IF WH-MCODE = SPACE;
              IF WF-STATUS = "00" MOVE KL-FIBEZ TO WK-FIRMA GO X
              else DISPLAY "Firma fehlt"  with highlight AT 2401 GO C.
           MOVE WH-WERT TO WD-FNR WN-FNR.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           OPEN I-O LAGERBZ.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      ********************************************* alle Werte l봲chen *
       MACH-NULL SECTION.
       A.  CALL "CAUP" USING "0707100340000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Alles l봲chen? <    > <ret>= Code" AT VDU-LP.
       C.  CALL "CAUP" USING "1002194004" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WH-NUM NOT = 4711 GO C.
           CLOSE LAGERBZ.
           DELETE FILE LAGERBZ.
           OPEN I-O LAGERBZ.
           MOVE 0 TO LG-NUM.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           START LAGARTIK KEY NOT < LG-KEY INVALID GO X.
       E.  READ LAGARTIK NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           DISPLAY LG-BEZ with highlight AT VDU-LP.
           MOVE 0 TO LG-STAND LG-MIND LG-BMENGE.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE SPACE TO LG-LIEF(WX)
               MOVE 0 TO  LG-DATUM(WX) LG-BTTO(WX) LG-RAB(WX)
                    LG-ZRAB(WX) LG-AUF(WX) LG-SYM(WX) LG-NTTO(WX).
           REWRITE LG-SATZ.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
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
      *    (sp: Abstand, h: Zeich./Zoll b: St꼛ke s: Schrift T: Schrift
       Z.  EXIT.
