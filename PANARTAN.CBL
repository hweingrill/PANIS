      $SET LINKCOUNT"384" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PANARTAN.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEART.CPY.
           COPY PARSEREZ.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSEKON.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEDAU.CPY.
           COPY PANSESTA.CPY.
           SELECT ETIKETT   ASSIGN TO "PANPRETI.DAT"
                            ORGANIZATION INDEXED ACCESS DYNAMIC
                            RECORD KEY ET-KEY
                            FILE STATUS IS WF-STATUS.
           SELECT DRUCKER   ASSIGN TO PRINTER "LPT1"
                            FILE STATUS WF-STATUS.
           SELECT EXCELIS   ASSIGN TO WN-EXCEL
                            ORGANIZATION LINE SEQUENTIAL
                            FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANFETIK.CPY.
       COPY PANFDART.CPY.
       COPY PARREZEP.CPY.
       COPY PANFAKT.CPY.
       COPY PANSTAT.CPY.
       COPY PANDAU.CPY.
       COPY PANLIEF.CPY.
       COPY PANKONS.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(64).
      ******************************************************************
       FD  EXCELIS                     LABEL RECORD OMITTED.
       01  EXC-SATZ                    PIC X(128).
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
           03  WT-FOLGE.
               05 WH-FOLGE             PIC 999      OCCURS  250.
           03  WN-EXCEL                PIC X(20).
           03  WH-GRP                  PIC 9(4)        COMP.
           03  WH-GEW                  PIC 9(7)        COMP.
           03  WD-GEW                  PIC Z.ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WI                      PIC 99        COMP.
           03  WH-ART                  PIC 99.
           03  WF-GRM.
               05 WF-GRP               PIC 99        COMP-X.
               05 WF-SON               PIC 99        COMP.
               05 WF-MEH               PIC 99        COMP.
               05 WF-NK                PIC 99        COMP.
               05 WF-TB                PIC 99        COMP.
           03  WD-TG                   PIC ZZ9.
           03  WD-PROZ                 PIC Z9,9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-UST                  PIC Z9,99-.
           03  WH-MEH                  PIC 99         COMP.
           03  WR-P                    PIC 99         COMP.
           03  WI-M                    PIC 99         COMP.
           03  WQ                      PIC 99         COMP.
           03  WE                      PIC 99         COMP.
           03  IX                      PIC 99         COMP.
           03  WM-LP                   PIC 9999       COMP.
           03  WM-ETIK                 PIC 99         COMP  VALUE 0.
           03  WM-OPEN                 PIC 99         COMP  VALUE 0.
           03  WX-PRNO                 PIC 99     COMP-X.
           03  WX-PRSTAT               PIC 99     COMP-X.
           03  WZ-SCHALT               PIC 99     COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99     COMP   VALUE ZERO.
           03  WM-TAST                 PIC 99     COMP.
           03  WT-EXTAB.
               05 WT-EX                PIC X(7)     OCCURS 30.
      *-----------------------------------------> Bildschirmausgaben <-
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WM-MERKER               PIC 9       COMP   VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WV-ARNUM                PIC 9(4)      COMP.
           03  WV-NEU                  PIC 9(4)      COMP.
           03  WD-POS                  PIC ZZ9.
           03  WD-GRP                  PIC Z9.
           03  WD-ART                  PIC ZZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-PREIS                PIC Z.ZZ9,99-.
           03  WD-APREIS               PIC Z.ZZ9,999-.
           03  WH-PREIS                PIC S9(5)V999  COMP-3.
           03  WR-PR REDEFINES WH-PREIS.
               05 FILLER               PIC X(4).
               05 WR-GR                PIC S9         COMP-3.
           03  WD-X                    PIC X.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON DAUER.
       A.  CALL "CADECL" USING "PANBEST.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING  "PANELFS.DAT"  WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING "PANEFAKT.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON ETIKETT.
       A.  CALL "CADECL" USING  "PANPRETI.DAT" WH-CREG.
       DECL-S SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  CALL "CADECL" USING  "PANESTAT.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING  "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM ARTIK.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG TO WZ-TAG VDU-JAHR.
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
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
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
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ********** 50: Anl./éndg. * 51: aus and.Prog. * 52: nur anzeigen *
       ARTIK SECTION.
       A.  MOVE 0 TO WM-MERKER WI.
           OPEN I-O REZEPT.
           IF WL-CA = 50 CALL "CAUP" USING "0715050765000" WH-CREG
                    ELSE CALL "CAUP" USING "0701040765000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Artikelverwaltung " WITH HIGHLIGHT AT VDU-REL.
           IF WL-CA = 80 GO P.
           IF WL-CA = 53 PERFORM ART-BILD GO Z.
           IF WL-CA = 52 PERFORM ART-BILD
                         PERFORM WEITER GO X.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:            " AT VDU-LP.
           IF WL-CA = 51 GO M.
       C.  DISPLAY "<ret>= Anl./énd., alpha+<ret>= suchen, <esc>= Ende"
               AT 2301.
           DISPLAY "<tab>= Artikelnummernwechsel, <>= PreisÑnderung, <S
      -       "trg.Einf>= ExcelÅbern." AT 2401.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1002084010" WH-CREG.
           IF SEIN PERFORM CSV-UEB GO B.
           IF TABL PERFORM UMTAUSCH GO B.
           IF ESC MOVE 0 TO AR-NUM GO X.
           MOVE 0 TO WM-TAST.
           IF WOLI SET RET TO TRUE
               MOVE 1 TO WM-TAST
               CALL "CAUP" USING "1500003003" WH-CREG GO M.
           IF NOT RET GO C.
       E.  IF WH-NUM = 0 MOVE WV-MCODE TO AR-ABEZ
               MOVE SPACE TO AR-SA
               CALL "PANANZ" USING "20ARTSUCH" WH-CREG
               CANCEL "PANANZ".
       M.  MOVE 0 TO WM-MERKER.
           MOVE WH-NUM TO AR-NUM.
           IF WH-NUM = 0 GO C.
       P.  READ ARTIKEL INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO P.
       Q.  MOVE AR-NUM TO RE-NUM.
           READ REZEPT INVALID INITIALIZE RE-SATZ
               MOVE AR-NUM TO RE-NUM.
           IF ZUGRIF PERFORM BESETZT GO P.
           PERFORM AR-ANL.
           UNLOCK ARTIKEL.
           IF WL-CA = 50 CALL "CAUP" USING "16LOEFENST" WH-CREG
               GO B.
           IF ESC MOVE 0 TO AR-NUM.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK ARTIKEL.
           CLOSE REZEPT.
           IF WM-ETIK = 1 CLOSE ETIKETT.
       Z.  EXIT.
      ***************************************** ArtikelnummernÑnderung *
       UMTAUSCH SECTION.
       A.  CALL "CAUP" USING "0705100640000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Artikelnummerumwandlung " AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           UNLOCK ARTIKEL.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Artikel alt:      Artikel neu:" AT VDU-LP.
           DISPLAY "<esc/ret-leer>= Ende" AT 2301.
           CALL "CAUP" USING "1002164004" WH-CREG.
           IF ESC OR WH-NUM = 0 GO X.
           IF NOT RET GO C.
           MOVE WH-NUM TO AR-NUM WV-ARNUM WD-ART.
           DISPLAY WD-ART with highlight AT VDU-LP.
       E.  READ ARTIKEL INVALID PERFORM NO-REC GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 305 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-BEZ with highlight AT VDU-LP.
       G.  CALL "CAUP" USING "1002344004" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO G.
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO WD-ART AR-NUM WV-NEU.
           DISPLAY WD-ART with highlight AT VDU-LP.
       I.  READ ARTIKEL INVALID GO K.
           IF ZUGRIF PERFORM BESETZT GO I.
           ADD 405 VDU-ECK GIVING VDU-LP.
           DISPLAY AR-BEZ with highlight AT VDU-LP.
           DISPLAY "Artikel nicht frei" with highlight AT 2401.
           PERFORM WEITER.
           CALL "CAUP" USING "1323012480" WH-CREG.
           GO C.
       K.  DISPLAY "<esc>= Abbruch, <ret>= durchfÅhren < >" AT 2301.
           CALL "CAUP" USING "0123370000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO K.
           MOVE WV-ARNUM TO AR-NUM.
           READ ARTIKEL INVALID PERFORM NO-REC GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           DELETE ARTIKEL.
           MOVE WV-NEU TO AR-NUM.
           WRITE AR-SATZ INVALID KEY REWRITE AR-SATZ.
           DISPLAY " -- bitte warten --" with blink AT 2330.
      *------------------------------> Sonderpreise u. Fakturierdatei <-
           PERFORM NEU-SOND.
      *----------------------------------------------> Statistikdatei <-
           PERFORM NEU-STAT.
      *-------------------------------------------> Dauerbestellungen <-
           PERFORM NEU-DAUER.
      *-------------------------------------------> Lieferscheindatei <-
           PERFORM NEU-LFS.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************************** énderung Sonderpreise *
       NEU-SOND SECTION.
       A.  ADD 505 VDU-ECK GIVING VDU-LP.
           DISPLAY "Sonderpreise" with highlight AT VDU-LP.
           MOVE 0 TO FA-KTONR.
           MOVE WV-ARNUM TO FA-ARNUM.
       C.  START FAKTDAT KEY NOT < FA-KEY INVALID KEY GO Z.
       E.  READ FAKTDAT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO FA-KTONR GIVING WD-KTO.
           MOVE FA-ARNUM TO WD-ART.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ART.
           IF FA-ARNUM < WV-ARNUM MOVE WV-ARNUM TO FA-ARNUM GO C.
           IF FA-ARNUM > WV-ARNUM MOVE WV-ARNUM TO FA-ARNUM
               ADD 1 TO FA-KTONR GO C.
           DELETE FAKTDAT INVALID NEXT SENTENCE.
           MOVE WV-NEU TO FA-ARNUM.
           WRITE FA-SATZ.
           GO C.
       Z.  EXIT.
      ************************************ énderung in Dauerbestellung *
       NEU-DAUER SECTION.
       A.  ADD 505 VDU-ECK GIVING VDU-LP.
           DISPLAY "Dauerbestellung" with highlight AT VDU-LP.
           MOVE 0 TO DA-KTONR.
           MOVE WV-ARNUM TO DA-ARNUM.
       C.  START DAUER KEY NOT < DA-KEY INVALID GO Z.
       E.  READ DAUER NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO DA-KTONR GIVING WD-KTO.
           MOVE DA-ARNUM TO WD-ART.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ART.
           IF DA-ARNUM < WV-ARNUM MOVE WV-ARNUM TO DA-ARNUM GO C.
           IF DA-ARNUM > WV-ARNUM MOVE WV-ARNUM TO DA-ARNUM
               ADD 1 TO DA-KTONR GO C.
           MOVE DA-KEY TO WT-TX.
           DELETE DAUER INVALID NEXT SENTENCE.
           MOVE WV-NEU TO DA-ARNUM.
           WRITE DA-SATZ INVALID REWRITE DA-SATZ.
           MOVE WT-TX TO DA-KEY.
           GO C.
       Z.  EXIT.
      ********************************** énderung in Lieferscheindatei *
       NEU-LFS SECTION.
       A.  ADD 505 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lieferschein   " with highlight AT VDU-LP.
       C.  MOVE WV-ARNUM TO LF-ARNUM.
           START LFSCHEIN KEY not < LF-ARNUM INVALID GO Z.
       E.  READ LFSCHEIN NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO LF-KTONR GIVING WD-KTO.
           MOVE LF-ARNUM TO WD-ART.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ART.
           IF LF-ARNUM not = WV-ARNUM GO Z.
           DELETE LFSCHEIN.
           MOVE WV-NEU TO LF-ARNUM.
           WRITE LF-SATZ INVALID REWRITE LF-SATZ.
           GO C.
       Z.  EXIT.
      ************************************* énderung in Statistikdatei *
       NEU-STAT SECTION.
       A.  OPEN I-O STATISTIK.
           ADD 505 VDU-ECK GIVING VDU-LP.
           DISPLAY "Statistik   " with highlight AT VDU-LP.
           MOVE 0 TO ST-KTONR ST-JAHR.
           MOVE WV-ARNUM TO ST-ARNUM.
       C.  START STATISTIK KEY NOT < ST-KEY INVALID GO X.
       E.  READ STATISTIK NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO ST-KTONR GIVING WD-KTO.
           MOVE ST-ARNUM TO WD-ART.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ART.
           IF ST-ARNUM < WV-ARNUM MOVE WV-ARNUM TO ST-ARNUM
               MOVE 0 TO ST-JAHR GO C.
           IF ST-ARNUM > WV-ARNUM MOVE WV-ARNUM TO ST-ARNUM
               MOVE 0 TO ST-JAHR
               ADD 1 TO ST-KTONR GO C.
           MOVE ST-KEY TO WT-TX.
           DELETE STATISTIK.
           MOVE WV-NEU TO ST-ARNUM.
           WRITE ST-SATZ INVALID REWRITE ST-SATZ.
           MOVE WT-TX TO ST-KEY.
           GO C.
       X.  CLOSE STATISTIK.
       Z.  EXIT.
      ******************************************************************
       ART-MASKE SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:      Bez.:" AT VDU-LP.
           ADD 43 TO VDU-LP.
           DISPLAY "Meh:      Rab:" AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nk:    Gew:         Ust:  /  %  Pl:    Tz:    Tg:
      -        "  Tk: " AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY " Wvkp:             Ekp:            Lvkp:
      -        "S-Nr:  " AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY " Wvkp:           Datum:            Lvkp:
      -        "Sort:  " AT VDU-LP.
           ADD 603 VDU-ECK GIVING VDU-LP
           DISPLAY "Teiggew.:       gr  Blechgrî·e:    Stk" AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       ART-BILD SECTION.
       A.  PERFORM ART-MASKE.
           IF AEND GO G.
           MOVE AR-NUM TO WH-NUM.
           INITIALIZE AR-SATZ.
           MOVE SPACE TO AR-AKEY AR-BEZ.
           MOVE 1 TO AR-UST WX.
           MOVE WH-NUM TO AR-NUM.
       G.  MOVE AR-GRM TO WF-GRM.
           MOVE AR-NUM TO WD-ART.
           IF RE-GEW not = 0 MOVE RE-GEW TO WD-GEW
               ADD 613 VDU-ECK GIVING VDU-LP
               DISPLAY WD-GEW with highlight AT VDU-LP.
           IF RE-BLECH(1) not = 0 MOVE RE-BLECH(1) TO WD-GRP
               ADD 635 VDU-ECK GIVING VDU-LP
               DISPLAY WD-GRP with highlight AT VDU-LP.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ART with highlight AT VDU-LP.
           IF ANLAGE GO Z.
           ADD 11 TO VDU-LP.
           DISPLAY AR-BEZ with highlight AT VDU-LP.
           ADD 1 WF-MEH GIVING WH-MEH.
           ADD 251 VDU-ECK GIVING VDU-LP.
           IF WH-MEH > 20 DISPLAY "-f-" with highlight BLINK AT VDU-LP
               MOVE 19 TO WF-MEH
           else DISPLAY WT-MEH(WH-MEH) with highlight AT VDU-LP.
           ADD 261 VDU-ECK GIVING VDU-LP.
           MOVE AR-RAB TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP "/".
           EVALUATE AR-RAB
               WHEN 0 DISPLAY "j" AT 0000
               WHEN 1 DISPLAY "n" AT 0000.
           MOVE WF-NK TO WD-KZ.
           ADD 307 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           MOVE AR-ANZ TO WD-ANZ.
           ADD 7 TO VDU-LP.
           DISPLAY WD-ANZ with highlight AT VDU-LP.
           MOVE AR-UST TO WD-KZ.
           ADD 14 TO VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 2 TO VDU-LP.
           IF AR-UST > 0 AND < 7
               MOVE WT-UST(AR-UST) TO WD-UST
               DISPLAY WD-UST with highlight SIZE 2 AT VDU-LP
           else MOVE 1 TO AR-UST
               DISPLAY "-f-" with highlight BLINK AT VDU-LP.
           ADD 9 TO VDU-LP.
           DISPLAY AR-PL with highlight AT VDU-LP.
           MOVE WF-TB TO WD-KZ.
           ADD 7 TO VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           MOVE WF-GRP TO WD-POS.
           ADD 7 TO VDU-LP.
           DISPLAY WD-POS with highlight AT VDU-LP.
           IF AR-GVZ = 32 MOVE 0 TO AR-GVZ.
           ADD 8 TO VDU-LP.
           MOVE AR-GVZ TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           IF AR-OEPZ not = 0 and < 21 ADD 461 VDU-ECK GIVING VDU-LP
               MOVE AR-OEPZ TO WD-GRP
               DISPLAY WD-GRP with highlight AT VDU-LP.
           MOVE AR-PREIS TO WD-PREIS WD-APREIS WH-PREIS.
           ADD 410 VDU-ECK GIVING VDU-LP.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                  ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
           ADD 17 TO VDU-LP.
           MOVE AR-EKP TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           ADD 17 TO VDU-LP.
           MOVE AR-VKP TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
      *    IF AR-ADAT = 0 GO Z.
           MOVE AR-APREIS TO WD-PREIS WD-APREIS WH-PREIS.
           ADD 510 VDU-ECK GIVING VDU-LP.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                  ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
           ADD 17 TO VDU-LP.
           MOVE AR-ADAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 17 TO VDU-LP.
           MOVE AR-AVKP TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           ADD 17 TO VDU-LP.
           MOVE AR-SGRP TO WD-GRP.
           DISPLAY WD-GRP with highlight foreground-color 6 AT VDU-LP.
       Z.  EXIT.
      ******************************************** Strichcodeetiketten *
       EAN-ETIK SECTION.
       A.  PERFORM DIS-ETIK.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Text" AT 2301.
           DISPLAY "Produktbezeichnung" with highlight AT 2401.
           MOVE ET-BEZ-A TO WT-TX.
           CALL "CAUP" USING "1202090121" WH-CREG.
           IF ESC GO X.
           IF ENDE GO Q.
           IF not RET GO C.
           MOVE WT-TX TO ET-BEZ-A.
           DISPLAY ET-BEZ-A with highlight AT VDU-LP.
       E.  GO G.
           DISPLAY "<esc>= Abbruch, <ret>= Text" AT 2301.
           DISPLAY "2. Produktbezeichnung" with highlight AT 2401.
           MOVE ET-BEZ-B TO WT-TX.
           CALL "CAUP" USING "1203090121" WH-CREG.
           IF WOLI or AUF GO C.
           IF ESC GO X.
           IF ENDE GO Q.
           IF not RET GO E.
           MOVE WT-TX TO ET-BEZ-B.
           DISPLAY ET-BEZ-B with highlight AT VDU-LP.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Text" AT 2301.
           DISPLAY "Zutaten 1. Zeile" with highlight AT 2401.
           MOVE ET-ZU-A TO WT-TX.
           CALL "CAUP" USING "1204090139" WH-CREG.
           IF WOLI or AUF GO C.
           IF ESC GO X.
           IF ENDE GO Q.
           IF not RET GO G.
           MOVE WT-TX TO ET-ZU-A.
           DISPLAY ET-ZU-A with highlight AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, <ret>= Text" AT 2301.
           DISPLAY "Zutaten 2. Zeile" with highlight AT 2401.
           MOVE ET-ZU-B TO WT-TX.
           CALL "CAUP" USING "1205090139" WH-CREG.
           IF WOLI or AUF GO G.
           IF ENDE GO Q.
           IF ESC GO X.
           IF not RET GO I.
           MOVE WT-TX TO ET-ZU-B.
           DISPLAY ET-ZU-B with highlight AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Text" AT 2301.
           DISPLAY "Zutaten 3. Zeile" with highlight AT 2401.
           MOVE ET-ZU-C TO WT-TX.
           CALL "CAUP" USING "1206090139" WH-CREG.
           IF WOLI or AUF GO I.
           IF ENDE GO Q.
           IF ESC GO X.
           IF not RET GO K.
           MOVE WT-TX TO ET-ZU-C.
           DISPLAY ET-ZU-C with highlight AT VDU-LP.
       M.  DISPLAY "<esc>= Abbruch, <ret>= Tage" AT 2301.
           DISPLAY "Mindethaltbarkeitstage" with highlight AT 2401.
           MOVE ET-MHD TO WH-WERT.
           CALL "CAUP" USING "1007093003" WH-CREG.
           IF WOLI or AUF GO K.
           IF ESC GO X.
           IF not RET GO M.
           MOVE WH-WERT TO WD-TG ET-MHD.
           DISPLAY WD-TG with highlight AT VDU-LP.
       O.  DISPLAY "<esc>= Abbruch, <ret>= Ean-Code, < >= Ean-Code vorsc
      -        "hlagen" AT 2301.
           DISPLAY "EAN-Strichcode" with highlight AT 2401.
           DISPLAY "letzte Stelle immer 0 eingeben - autom. Errechnung"
               AT 2501.
           MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "100909D013" WH-CREG.
           IF WOLI or AUF GO M.
           IF AB MOVE 0 TO ET-EAN
                 PERFORM MAKE-EAN
                 DISPLAY ET-EAN with highlight AT VDU-LP
                 GO O.
           IF ESC GO X.
           IF not RET GO O.
           IF WH-NUM not = 0 MOVE WH-NUM TO ET-EAN.
           PERFORM MAKE-EAN.
           DISPLAY ET-EAN with highlight AT VDU-LP.
       P.  DISPLAY "<esc>= Abbruch, <ret>= Kennzeichen" AT 2301.
           DISPLAY "S= Sonderetikett, leer= Normaletikett"
               with highlight AT 2401.
           MOVE ET-SONDER TO WT-TX.
           CALL "CAUP" USING "1209390101" WH-CREG.
           IF WOLI or AUF GO K.
           IF ESC GO X.
           IF not RET GO M.
           MOVE WT-TX TO ET-SONDER.
           IF ET-SONDER = "S" or "s" MOVE "S" TO ET-SONDER
           else MOVE SPACE TO ET-SONDER.
           DISPLAY ET-SONDER with highlight AT VDU-LP.
       Q.  DISPLAY "<esc>= Abbruch, <ret>= speichern, <Entf>= lîschen <
      -        ">" AT 2301.
           DISPLAY "<Ende>= Testdruck" AT 2401.
           CALL "CAUP" USING "0023520000" WH-CREG.
           IF ENDE CALL "PANLFDRU" USING "95TEST" WH-CREG
                   CANCEL "PANLFDRU"
                   GO Q.
           IF ESC GO X.
           IF ENTF DELETE ETIKETT.
           IF not RET GO Q.
           WRITE ET-SATZ INVALID REWRITE ET-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       DIS-ETIK SECTION.
       A.  CALL "CAUP" USING "270613105100011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Ean-Etiketten " AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Bez.:                      Æ" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "                           Æ" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Zut.:                                        Æ"
               AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "                                             Æ"
               AT VDU-LP
           ADD 100 TO VDU-LP.
           DISPLAY "                                             Æ"
               AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Mhd.: " AT VDU-LP.
           ADD 200 TO VDU-LP.
           DISPLAY "Ean.: " AT VDU-LP.
           ADD 24 TO VDU-LP.
           DISPLAY "S.-Etikett: " AT VDU-LP.
           IF WM-ETIK = 0 OPEN I-O ETIKETT
               MOVE 1 TO WM-ETIK.
           MOVE AR-NUM TO ET-NUM.
           MOVE "E" TO ET-SA.
       D.  READ ETIKETT INVALID INITIALIZE ET-SATZ
               MOVE AR-NUM TO ET-NUM
               MOVE "E" TO ET-SA
               MOVE AR-BEZ TO ET-BEZ-A
               MOVE "Zutaten:" TO ET-ZU-A
               PERFORM MAKE-EAN.
           IF ZUGRIF PERFORM BESETZT GO D.
           ADD 209 VDU-ECK GIVING VDU-LP.
           DISPLAY ET-BEZ-A with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY ET-BEZ-B with highlight AT VDU-LP.
           ADD 409 VDU-ECK GIVING VDU-LP.
           DISPLAY ET-ZU-A with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY ET-ZU-B with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY ET-ZU-C with highlight AT VDU-LP.
           IF ET-MHD = 0 MOVE 3 TO ET-MHD.
           MOVE ET-MHD TO WD-TG.
           ADD 100 TO VDU-LP.
           DISPLAY WD-TG with highlight AT VDU-LP " Tage".
           ADD 200 TO VDU-LP.
           DISPLAY "Ean.: " AT VDU-LP.
           IF ET-EAN = 0 PERFORM MAKE-EAN.
           DISPLAY ET-EAN with highlight AT VDU-LP.
           ADD 30 TO VDU-LP.
           DISPLAY ET-SONDER with highlight AT VDU-LP.
       Z.  EXIT.
      ************************************************* Bilde Ean-Code *
       MAKE-EAN SECTION.
       A.  IF ET-EAN = 0
               COMPUTE ET-EAN = AR-NUM * 10 + (WK-EAN * 1000000).
           MOVE ET-EAN TO WH-EAN.
           ADD WR-EAN(2) WR-EAN(4) WR-EAN(6) WR-EAN(8) WR-EAN(10)
               WR-EAN(12) GIVING WK-ANZ.
           MULTIPLY 3 BY WK-ANZ.
           ADD WR-EAN(1) WR-EAN(3) WR-EAN(5) WR-EAN(7) WR-EAN(9)
               WR-EAN(11) TO WK-ANZ.
           DIVIDE 10 INTO WK-ANZ GIVING WK-ANZ REMAINDER WK-ANZ.
           SUBTRACT WK-ANZ FROM 10 GIVING WR-EAN(13).
           MOVE WH-EAN TO ET-EAN.
       Z.  EXIT.
      *************************************** ErgÑnzung Preisetiketten *
       PRETIK SECTION.
       A.  CALL "CAUP" USING "0719150344000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Etikettentext " AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Bez.:" AT VDU-LP.
           ADD 27 TO VDU-LP.
           DISPLAY "Gew.:" AT VDU-LP.
           IF WM-ETIK = 0 OPEN I-O ETIKETT
               MOVE 1 TO WM-ETIK.
           MOVE AR-NUM TO ET-NUM.
       D.  READ ETIKETT INVALID INITIALIZE ET-SATZ
               MOVE AR-NUM TO ET-NUM
               MOVE AR-BEZ TO ET-BEZ-P.
           IF ZUGRIF PERFORM BESETZT GO D.
       E.  MOVE ET-BEZ-P TO WT-TX.
           ADD 209 VDU-ECK GIVING VDU-LP.
           DISPLAY ET-BEZ-P with highlight AT VDU-LP.
           IF ET-GEW-P NOT = 0 MOVE ET-GEW-P TO WD-ANZ
               ADD 236 VDU-ECK GIVING VDU-LP
               DISPLAY WD-ANZ with highlight AT VDU-LP.
           DISPLAY "kein Text = Etikett wird gelîscht" AT 2401.
           CALL "CAUP" USING "1202090117" WH-CREG.
           IF ESC GO X.
           MOVE WT-TX TO ET-BEZ-P.
           IF ET-BEZ-P NOT = SPACE GO F.
           DELETE ETIKETT INVALID GO X.
           GO X.
       F.  DISPLAY "<ret>= Gewicht, <>= zurÅck" AT 2301.
           MOVE ET-GEW-P TO WH-WERT.
           CALL "CAUP" USING "1002365005" WH-CREG.
           IF WOLI GO E.
           IF NOT RET GO F.
           MOVE WH-WERT TO ET-GEW-P WD-ANZ.
           DISPLAY WD-ANZ with highlight AT VDU-LP.
           WRITE ET-SATZ INVALID REWRITE ET-SATZ.
       H.  DISPLAY "<ret>= ok, <>= zurÅck, < >= alle 1 x drucken"
               AT 2301.
           DISPLAY "< >= Einzeletikett drucken mit StÅckeingabe"
               AT 2401.
           CALL "CAUP" USING "0023472002" WH-CREG.
           IF WOLI GO F.
           IF RET GO X.
           IF NOT AB AND NOT WORE OR WH-NUM > 10 GO H.
           MOVE WX-TASTE TO WS-TASTE.
      *------------------------------------------> Etikett ausdrucken <-
           IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               OPEN OUTPUT DRUCKER.
           IF WS-TASTE = 4 GO M.
           MOVE 0 TO ET-NUM.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           START ETIKETT KEY NOT < ET-KEY INVALID GO X.
       J.  READ ETIKETT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO J.
           MOVE ET-NUM TO AR-NUM.
       K.  READ ARTIKEL NO LOCK INVALID GO J.
           IF ZUGRIF PERFORM BESETZT GO J.
           PERFORM ART-BILD.
      *-------------------------------------------------> Bezeichnung <-
       M.  MOVE x"1b21731b7701" TO DRA-SATZ.
           MOVE ET-BEZ-P TO DRA-SATZ(7:).
           MOVE 1 TO WZ-SCHALT PERFORM DRUCK.
      *-----------------------------------------------------> Gewicht <-
           MOVE ET-GEW-P TO WD-ANZ.
           MOVE x"1b21111b7700" TO DRA-SATZ(1:).
           MOVE WD-ANZ TO DRA-SATZ(8:).
           MOVE "gr" TO DRA-SATZ(16:).
      *    MOVE 2 TO WZ-SCHALT PERFORM DRUCK.
      *-------------------------------------------------------> Preis <-
           MOVE x"1b21731b7701" TO DRA-SATZ(19:)
           MOVE AR-AVKP TO WD-PREIS.
           MOVE WD-PREIS TO DRA-SATZ(29:).
           IF DRA-SATZ(35:2) = "00" MOVE "oo" TO DRA-SATZ(35:2).
           PERFORM VARYING WX FROM 30 BY -1 UNTIL WX < 14
               OR DRA-SATZ(WX:1) = SPACE CONTINUE.
           SUBTRACT 2 FROM WX.
           MOVE "S" TO DRA-SATZ(WX:1).
           MOVE 4 TO WZ-SCHALT PERFORM DRUCK.
      *-----------------------------------> Vorschub nÑchstes Etikett <-
           MOVE 4 TO WZ-SCHALT PERFORM DRUCK.
           IF WS-TASTE = 2 GO J.
           IF WH-NUM > 1 SUBTRACT 1 FROM WH-NUM GO M.
       X.  IF WS-TASTE = 2 SET ESC TO TRUE GO Z.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       AR-ANL SECTION.
       A.  PERFORM ART-BILD.
           IF WM-TAST = 01 AND AEND GO Q.
           SET RET TO TRUE.
       B.  DISPLAY "<esc>= Abbruch, <#>= Preisetikett, < >= Etiketten, <
      -        " />= Ñndern < >" AT 2301.
           DISPLAY "<Ende>= Ende énderung bei allen Feldern!"
                with highlight foreground-color 6 AT 2401.
           CALL "CAUP" USING "0023670000" WH-CREG.
           IF KIST PERFORM PRETIK
                   GO B.
           IF AB PERFORM EAN-ETIK
                 GO B.
           IF RET AND WH-NUM = 0 SET ESC TO TRUE.
           IF ESC GO Z.
           IF ENDE GO V.
           IF not WOLI and not AUF GO B.
       C.  DISPLAY "<esc> = Abbruch, <ret>= Bezeichnung" AT 2301.
           IF ANLAGE DISPLAY "Artikel fehlt!" AT 2401.
           MOVE AR-BEZ TO WT-TX.
           CALL "CAUP" USING "1202190125" WH-CREG.
           IF WOLI OR ESC GO Z.
           IF WT-TX = SPACE DISPLAY "Bezeichnung muss sein!" LINE 24
               PERFORM WEITER GO B.
           MOVE WT-TX TO AR-BEZ.
           IF ENDE GO V.
           MOVE 1 TO WX WI-M.
           MOVE VDU-ECK TO WM-LP.
       D.  DISPLAY "<tab>= anzeigen Mengeneinheiten" AT 2301.
           MOVE WF-MEH TO WH-WERT.
           MOVE WM-LP TO VDU-ECK.
           CALL "CAUP" USING "1002512002" WH-CREG.
           IF TABL MOVE 22 TO WL-CA
                PERFORM MENGEN
                MOVE 0 TO WI-M GO D.
           IF WI-M = 0 MOVE 1 TO WI-M
               CALL "CAUP" USING "08CLOFEN" WH-CREG
               MOVE WS-TASTE TO WX-TASTE.
           IF ESC GO V.
           IF WOLI GO B.
           IF ENDE GO V.
           IF NOT RET GO D.
           MOVE WH-WERT TO WH-MEH.
           IF WH-MEH > 19 GO D.
           MOVE WH-MEH TO WF-MEH.
           ADD 1 TO WH-MEH.
           DISPLAY WT-MEH(WH-MEH) with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Kz. rabattfÑhig 0/1" AT 2301.
           DISPLAY "0= ja, 1= nein" AT 2401.
           MOVE AR-RAB TO WH-WERT.
           CALL "CAUP" USING "1002611001" WH-CREG.
           IF ESC GO V.
           IF WOLI MOVE 1 TO WX GO D.
           IF ENDE GO V.
           IF NOT RET GO E.
           IF WH-WERT > 1 GO E.
           MOVE WH-WERT TO AR-RAB WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP "/".
           EVALUATE AR-RAB
               WHEN 0 DISPLAY "j" AT 0000
               WHEN 1 DISPLAY "n" AT 0000.
       F.  DISPLAY "Nachkommastellen: " AT 2301.
           DISPLAY "Achtung falsche Rechnungsmengen mîglich!"
                 with highlight BLINK AT 2440.
           DISPLAY "0 = ohne, 1 = 1 NK, 2 = 2 NK" AT 2401.
           MOVE WF-NK TO WH-WERT.
           CALL "CAUP" USING "1003071001" WH-CREG.
           IF ESC GO V.
           IF WOLI GO E.
           IF ENDE GO V.
           IF NOT RET GO F.
           MOVE WH-WERT TO WF-NK WD-KZ.
           IF WF-NK > 2 GO F.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       G.  DISPLAY "Ausbackgewicht in Gramm" AT 2301.
           MOVE AR-ANZ TO WH-WERT.
           CALL "CAUP" USING "1003145005" WH-CREG.
           IF ESC GO V.
           IF WOLI GO F.
           IF ENDE GO V.
           IF NOT RET GO G.
           IF WH-WERT = 0 MOVE 1 TO WH-WERT.
           MOVE WH-WERT TO WD-ANZ AR-ANZ.
           DISPLAY WD-ANZ with highlight AT VDU-LP.
       H.  DISPLAY "Ust-Kennzeichen 1-6" AT 2301.
           MOVE AR-UST TO WH-WERT.
           CALL "CAUP" USING "1003281001" WH-CREG.
           IF ESC GO V.
           IF WOLI GO G.
           IF NOT RET GO H.
           MOVE WH-WERT TO AR-UST WD-KZ.
           IF AR-UST < 1 OR AR-UST > 6 GO H.
           DISPLAY WD-KZ AT VDU-LP.
           MOVE WT-UST(AR-UST) TO WD-UST.
           ADD 2 TO VDU-LP.
           DISPLAY WD-UST with highlight SIZE 2 AT VDU-LP.
       I.  DISPLAY "Preislistenkz.:" with highlight AT 2301.
           DISPLAY "0= allg.Preisl., 1= Kundenpreisl., 2= Inkassoartikel
      -        "" AT 2401.
           MOVE AR-PL TO WH-WERT.
           CALL "CAUP" USING "1003391001" WH-CREG.
           IF ESC GO V.
           IF WOLI GO H.
           IF ENDE GO V.
           IF NOT RET GO I.
           MOVE WH-WERT TO AR-PL.
           IF AR-PL > 2 GO I.
           DISPLAY AR-PL with highlight AT VDU-LP.
       J.  DISPLAY "Tourenzettl:" with highlight AT 2301
               " 0= nein, 1 - 5 = ja (Grp)".
           DISPLAY "1= Brot, 2= GebÑck, 3= Konditorei, 4= GebÑck (in Kon
      -        "ditorei eingezÑhlt)" AT 2401.
           MOVE WF-TB TO WH-WERT.
           CALL "CAUP" USING "1003461001" WH-CREG.
           IF ESC GO V.
           IF WOLI GO I.
           IF ENDE GO V.
           IF NOT RET GO J.
           MOVE WH-WERT TO WF-TB WD-KZ.
           IF WF-TB NOT = 9; IF WF-TB > 4 GO J.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       K.  DISPLAY "Teiggruppe:" with highlight AT 2301.
           MOVE WF-GRP TO WH-WERT.
           CALL "CAUP" USING "1003533003" WH-CREG.
           IF ESC GO V.
           IF WOLI GO J.
           IF ENDE GO V.
           IF NOT RET GO K.
           IF WH-WERT > 250 GO K.
           MOVE WH-WERT TO WF-GRP WD-POS.
           DISPLAY WD-POS with highlight AT VDU-LP.
       KA. DISPLAY "GÑrverzîgererware: 0= nein, 1= ja" AT 2301.
           MOVE AR-GVZ TO WH-WERT.
           CALL "CAUP" USING "1003612002" WH-CREG.
           IF ESC GO V.
           IF WOLI GO K.
           IF ENDE GO V.
           IF NOT RET GO KA.
           MOVE WH-WERT TO AR-GVZ WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       KB. DISPLAY "Prod. Statistik-Nr." AT 2301.
           MOVE AR-OEPZ TO WH-WERT.
           CALL "CAUP" USING "1004612002" WH-CREG.
           IF ESC GO V.
           IF WOLI GO KA.
           IF ENDE GO V.
           IF NOT RET GO KB.
           MOVE WH-WERT TO AR-OEPZ WD-GRP.
           DISPLAY WD-GRP with highlight AT VDU-LP.
       L.  IF WF-TB = 9 DISPLAY "Gesamtverkaufspreis" AT 2301
               ELSE DISPLAY "Wiederverkaufspreis" AT 2301.
           MOVE AR-PREIS TO WH-WERT.
           CALL "CAUP" USING "1004104308" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO KB.
           IF ENDE GO V.
           IF NOT RET GO L.
           MOVE WH-WERT TO AR-PREIS WD-PREIS WD-APREIS WH-PREIS.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                  ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
       M.  DISPLAY "EK-Preis" AT 2301.
           MOVE AR-EKP TO WH-WERT.
           CALL "CAUP" USING "1004274207" WH-CREG.
           IF ESC GO V.
           IF WOLI GO L.
           IF ENDE GO V.
           IF NOT RET GO M.
           MOVE WH-WERT TO AR-EKP WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
       N.  IF WF-TB = 9 DISPLAY "Einzelpreis z.B. 1/8 etc" AT 2301
               ELSE DISPLAY "Ladenpreis" AT 2301.
           IF AR-VKP = 0 AND WF-TB = 9
               COMPUTE AR-VKP ROUNDED = AR-PREIS / AR-ANZ * WF-GRP.
           MOVE AR-VKP TO WH-WERT WD-PREIS.
           ADD 444 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           CALL "CAUP" USING "1004444207" WH-CREG.
           IF ESC GO V.
           IF WOLI GO M.
           IF ENDE GO V.
           IF NOT RET GO N.
           IF WH-WERT = 0 AND WF-TB = 9
               COMPUTE WH-WERT ROUNDED = AR-PREIS / AR-ANZ * WF-GRP.
           MOVE WH-WERT TO AR-VKP WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           GO T.
       Q.  DISPLAY "GÅltigkeitsdatum des neuen Preises:" AT 2207.
           IF WV-DATUM = 0 MOVE WM-DATUM TO WC-DATUM WH-WERT
                      ELSE MOVE WV-DATUM TO WC-DATUM WH-WERT.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 2243.
           MOVE WH-WERT TO WZ-DATUM.
           CALL "CAUP" USING "0122436006" WH-CREG.
           IF WOLI OR ESC GO Z.
           IF WX-DATUM = 0 GO Q.
           MOVE WX-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 527 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WV-DATUM NOT = AR-ADAT; IF AR-APREIS NOT = 0
               MOVE AR-APREIS TO AR-PREIS
               MOVE AR-AVKP TO AR-VKP.
           MOVE WV-DATUM TO AR-ADAT.
           IF ENDE GO V.
       R.  IF WF-TB = 9 DISPLAY "neuer Gesamtverkaufspreis" AT 2301
               ELSE DISPLAY "neuer Wiederverkaufspreis" AT 2301.
           MOVE AR-APREIS TO WH-WERT.
           CALL "CAUP" USING "1005104308" WH-CREG.
           IF ESC GO V.
           IF WOLI GO Q.
           IF ENDE GO V.
           IF NOT RET GO R.
           MOVE WH-WERT TO AR-APREIS WD-PREIS WD-APREIS WH-PREIS.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP
                  ELSE DISPLAY WD-APREIS with highlight AT VDU-LP.
       S.  IF WF-TB = 9 DISPLAY "neuer Einzelpreis z.B. 1/8 etc" AT 2301
               ELSE DISPLAY "neuer Ladenpreis" AT 2301.
           IF AR-AVKP = 0 AND WF-TB = 9
               COMPUTE AR-AVKP ROUNDED = AR-APREIS / AR-ANZ * WF-GRP.
           MOVE AR-AVKP TO WH-WERT WD-PREIS.
           ADD 544 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           CALL "CAUP" USING "1005444207" WH-CREG.
           IF ESC GO R.
           IF WOLI GO R.
           IF ENDE GO V.
           IF NOT RET GO S.
           MOVE WH-WERT TO AR-AVKP WD-PREIS.
           IF WH-WERT = 0 AND WF-TB = 9
               COMPUTE WH-WERT ROUNDED = AR-APREIS / AR-ANZ * WF-GRP.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           IF AR-APREIS = 0 AND AR-AVKP = 0 MOVE 0 TO AR-ADAT.
      *----------------------------------------> Gruppe fÅr LFS-Druck <-
       T.  IF WE-LFS = 1 PERFORM SO-TGW-BL.
           IF WOLI or AUF GO S.
       V.  IF ENDE and ANLAGE UNLOCK ARTIKEL GO Z.
           DISPLAY "<Entf>= lîschen, <ret>= speichern, <esc>= Abbruch"
              AT 2301.
           DISPLAY "<>= zum Wvkp-Preis" AT 2301.
           CALL "CAUP" USING "0023510000" WH-CREG.
           IF ESC UNLOCK ARTIKEL GO Z.
           IF WOLI; IF WM-TAST = 01 GO Q ELSE GO L.
           IF ENTF GO Y.
           IF NOT RET GO V.
           MOVE " " TO AR-SA.
           MOVE WF-GRM TO AR-GRM.
           MOVE AR-BEZ TO AR-ABEZ.
       W.  WRITE AR-SATZ INVALID REWRITE AR-SATZ.
           IF RE-NUM = 0 GO Z.
           MOVE AR-BEZ TO RE-BEZ.
           REWRITE RE-SATZ INVALID WRITE RE-SATZ.
           GO Z.
       Y.  DISPLAY "Wirklich lîschen? < >, <esc>= Abbruch, <Entf>= ja"
               AT 2301.
           CALL "CAUP" USING "0023200000" WH-CREG.
           IF ESC GO Z.
           IF NOT ENTF GO Y.
           MOVE AR-NUM TO WV-ARNUM.
           CALL "CAUP" USING "0707200540000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Artikellîschung " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           PERFORM LOE-LFS.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG GO Z.
           PERFORM LOE-SOND.
           PERFORM LOE-DAUER.
           PERFORM LOE-STAT.
           ADD 0219 VDU-ECK GIVING VDU-LP.
           DISPLAY "** gelîscht **" AT VDU-LP.
           DELETE ARTIKEL INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO Y.
       Z.  EXIT.
      ******************************************* énderung Teiggewicht *
       SO-TGW-BL SECTION.
       A.  IF WOLI or AUF GO G.
           DISPLAY "Artikelsortg.am Lfschein" AT 2401.
           MOVE AR-SGRP TO WH-WERT.
           CALL "CAUP" USING "1005612002" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO Z.
           IF ENDE GO Z.
           IF NOT RET GO A.
           MOVE WH-WERT TO AR-SGRP WD-GRP.
           DISPLAY WD-GRP with highlight foreground-color 6 AT VDU-LP.
       C.  MOVE AR-NUM TO RE-NUM.
           READ REZEPT INVALID INITIALIZE RE-SATZ
               MOVE AR-NUM TO RE-NUM.
           MOVE RE-GEW TO WD-GEW.
           ADD 613 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-GEW with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Gewicht in gr" AT 2301.
           MOVE RE-GEW TO WH-WERT.
           CALL "CAUP" USING "1006134004" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO C.
           IF ENDE GO Z.
           IF not RET GO E.
           MOVE WH-WERT TO RE-GEW WD-GRP.
           DISPLAY WD-GEW with highlight AT VDU-LP.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Blechgrî·e in Stk" AT 2301.
           ADD 635 VDU-ECK GIVING VDU-LP.
           MOVE RE-BLECH(1) TO WH-WERT WD-GRP.
           DISPLAY WD-GRP with highlight AT VDU-LP.
           CALL "CAUP" USING "1006352002" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO E.
           IF not RET GO G.
           MOVE WH-WERT TO RE-BLECH(1) WD-GRP.
           DISPLAY WD-GRP with highlight AT VDU-LP.
       Z.  EXIT.
      ********************************** énderung in Lieferscheindatei *
       LOE-LFS SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lieferschein   " with highlight AT VDU-LP.
           MOVE WV-ARNUM TO LF-ARNUM.
       C.  START LFSCHEIN KEY not < LF-ARNUM INVALID GO Z.
       E.  READ LFSCHEIN NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO LF-KTONR GIVING WD-KTO.
           MOVE LF-NUM TO WD-ANZ.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ANZ.
           IF LF-ARNUM not = WV-ARNUM GO Z.
           DISPLAY "Lieferschein vorhanden - daher nicht lîschbar"
              AT 2401 PERFORM WEITER
              SET ESC TO TRUE GO Z.
           GO E.
       Z.  EXIT.
      ****************************************** énderung Sonderpreise *
       LOE-SOND SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Sonderpreise   " with highlight AT VDU-LP.
           MOVE 0 TO FA-KTONR.
       C.  MOVE WV-ARNUM TO FA-ARNUM.
           START FAKTDAT KEY NOT < FA-KEY INVALID KEY GO Z.
       E.  READ FAKTDAT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO FA-KTONR GIVING WD-KTO.
           MOVE FA-ARNUM TO WD-ANZ.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ANZ.
           IF FA-ARNUM < WV-ARNUM GO C.
           IF FA-ARNUM > WV-ARNUM ADD 1 TO FA-KTONR GO C.
           DELETE FAKTDAT INVALID NEXT SENTENCE.
           GO E.
       Z.  EXIT.
      ************************************ énderung in Dauerbestellung *
       LOE-DAUER SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Dauerbestellung" with highlight AT VDU-LP.
           MOVE 0 TO DA-KTONR.
       C.  MOVE WV-ARNUM TO DA-ARNUM.
           START DAUER KEY NOT < DA-KEY INVALID GO Z.
       E.  READ DAUER NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO DA-KTONR GIVING WD-KTO.
           MOVE DA-ARNUM TO WD-ANZ.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ANZ.
           IF DA-ARNUM < WV-ARNUM GO C.
           IF DA-ARNUM > WV-ARNUM ADD 1 TO DA-KTONR GO C.
           DELETE DAUER INVALID NEXT SENTENCE.
           GO E.
       Z.  EXIT.
      ************************************* énderung in Statistikdatei *
       LOE-STAT SECTION.
       A.  OPEN I-O STATISTIK.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Statistik      " with highlight AT VDU-LP.
           MOVE 0 TO ST-KTONR ST-JAHR.
       C.  MOVE 0 TO ST-JAHR.
           MOVE WV-ARNUM TO ST-ARNUM.
           START STATISTIK KEY NOT < ST-KEY INVALID GO X.
       E.  READ STATISTIK NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO ST-KTONR GIVING WD-KTO.
           MOVE ST-ARNUM TO WD-ANZ.
           DISPLAY WD-KTO with highlight AT VDU-LP " / " WD-ANZ.
           IF ST-ARNUM < WV-ARNUM GO C.
           IF ST-ARNUM > WV-ARNUM ADD 1 TO ST-KTONR GO C.
           DELETE STATISTIK.
           GO E.
       X.  CLOSE STATISTIK.
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
      ******************************************** öbernahme CSV-Datei *
       CSV-UEB SECTION.
       A.  CALL "CAUP" USING "270505055000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " CSV-öbernahme " with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "CSV-Datei:           " with highlight AT VDU-LP.
           DISPLAY "Dateinamen mit '.CSV' eingeben" AT 2401.
           DISPLAY "<esc>= Abbruch, <ret>= Name" AT 2301.
           CALL "CAUP" USING "1203140112" WH-CREG.
           IF ESC GO X.
           IF not RET OR WT-TX = SPACE GO C.
           MOVE WT-TX TO WN-EXCEL.
           OPEN INPUT EXCELIS.
           IF WF-STATUS not = "00"
               DISPLAY "Datei nicht vorhanden!" AT 2401
               PERFORM WEITER
               GO C.
       E.  DISPLAY " öbernahme Exceldatei " with highlight AT 2401.
           DISPLAY "<esc>= Abbruch, <Einfg>= Start < >" AT 2301.
           CALL "CAUP" USING "0023330000" WH-CREG.
           IF not EINF and not ESC GO C.
           IF ESC GO X.
           EVALUATE WN-EXCEL(1:2)
               WHEN "1-" PERFORM CSV-1
               WHEN "2-" PERFORM CSV-SGRP.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* CSV-Sortiergruppen *
       CSV-SGRP SECTION.
       A.  READ EXCELIS AT END CLOSE EXCELIS GO W.
           IF EXC-SATZ = SPACE GO A.
           UNSTRING EXC-SATZ DELIMITED BY ";" INTO WT-EX(1) WT-EX(2)
                WT-EX(3) WT-EX(4) WT-EX(5) WT-EX(6) WT-EX(7).
           IF WT-EX(2) = SPACE GO A.
       G.  READ EXCELIS AT END CLOSE EXCELIS GO W.
           IF EXC-SATZ = SPACE GO G.
           IF EXC-SATZ(1:1) not NUMERIC GO G.
           MOVE 1 TO WI IX.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 7
               PERFORM EXCELTAB
               IF ESC MOVE 8 TO WX
                      SET RET TO TRUE
                      GO G.
           REWRITE AR-SATZ.
           GO G.
       W.  CLOSE EXCELIS.
       Z.  EXIT.
      ******************************************************************
       CSV-1 SECTION.
       A.  INITIALIZE WT-FOLGE.
           PERFORM VARYING WH-KEY FROM 37 BY 1 UNTIL WH-KEY > 39
               READ KONSTANT INVALID NEXT SENTENCE
               end-read
               MOVE LOW-VALUES TO KO-VSATZ
               REWRITE KO-VSATZ INVALID WRITE KO-VSATZ.
       E.  READ EXCELIS AT END CLOSE EXCELIS GO W.
           IF EXC-SATZ = SPACE GO E.
           UNSTRING EXC-SATZ DELIMITED BY ";" INTO WT-EX(1) WT-EX(2)
                WT-EX(3) WT-EX(4) WT-EX(5) WT-EX(6) WT-EX(7).
           IF WT-EX(2) = SPACE GO E.
       G.  READ EXCELIS AT END CLOSE EXCELIS GO W.
           IF EXC-SATZ = SPACE GO E.
           IF EXC-SATZ(1:1) not NUMERIC GO G.
           MOVE 1 TO WI IX.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 7
               PERFORM EXCELTAB
               IF ESC MOVE 8 TO WX
                      SET RET TO TRUE
                      GO G.
           MOVE WH-GEW TO RE-GEW.
           IF AR-GRP = 0 GO G.
           COMPUTE WH-KEY = (AR-GRP - 1) / 100 + 37.
           READ KONSTANT.
           MOVE AR-GRP TO WH-ART.
           MOVE WH-GRP TO KO-TGRP(WH-ART).
           MOVE "*" TO KO-VKZ.
           REWRITE KO-VSATZ.
           IF WH-GRP > 255 DISPLAY "Teig-Nr. falsch" AT 2301
                           PERFORM WEITER
                           GO G.
           REWRITE RE-SATZ INVALID REWRITE RE-SATZ.
           REWRITE AR-SATZ.
           GO G.
       W.  Display "öbernahme beendet! < > weiter mit <ret>" AT 2301.
           CALL "CAUP" USING "0023210000" WH-CREG.
           MOVE 37 TO WH-KEY.
           MOVE 0 TO WZ.
           READ KONSTANT INVALID INITIALIZE KO-VSATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 100
               ADD 1 TO WZ
               MOVE WH-FOLGE(WZ) TO KO-TGRP(WX).
           WRITE KO-SATZ INVALID REWRITE KO-SATZ.
           MOVE 38 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-VSATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 100
               ADD 1 TO WZ
               MOVE WH-FOLGE(WZ) TO KO-TGRP(WX).
           WRITE KO-SATZ INVALID REWRITE KO-SATZ.
           MOVE 39 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-VSATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 50
               ADD 1 TO WZ
               MOVE WH-FOLGE(WZ) TO KO-TGRP(WX).
           WRITE KO-SATZ INVALID REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* numerisch auslesen *
       HOL-NUM SECTION.
       A.  MOVE 0 TO WH-NUM WK WH-WERT WH-NEG.
           MOVE WI TO WQ.
           PERFORM VARYING WI FROM WI BY 1 UNTIL WI > (WQ + 15)
               EVALUATE EXC-SATZ(WI:1)
                   WHEN "," MOVE 1 TO WK
                            ADD 1 WI GIVING WE
                            MOVE EXC-SATZ(WE:) TO EXC-SATZ(WI:)
                   WHEN "-" MOVE 1 TO WH-NEG
                            ADD 1 WI GIVING WE
                            MOVE EXC-SATZ(WE:) TO EXC-SATZ(WI:)
                   WHEN ";" exit perform
                   WHEN " " exit perform
                   WHEN OTHER IF WK > 0 ADD 1 TO WK.
           COMPUTE WX = 14 - WI + WQ.
           MOVE EXC-SATZ(WQ:13) TO WH-NUM(WX:).
           MOVE WH-NUM TO WH-WERT.
           IF WK not = 0
               PERFORM VARYING WZ FROM WK BY -1 UNTIL WZ = 0
               COMPUTE WH-WERT = WH-WERT / 10.
           IF WH-NEG = 1 COMPUTE WH-WERT = WH-WERT * -1.
           ADD 1 TO WI.
       Z.  EXIT.
      ************************************************* alpha auslesen *
       HOL-ALPHA SECTION.
       A.  MOVE WI TO WQ.
           MOVE SPACE TO WT-TX.
           PERFORM VARYING WI FROM WI BY 1 UNTIL
               EXC-SATZ(WI:1) = ";" CONTINUE.
           SUBTRACT WQ FROM WI GIVING WX.
           MOVE EXC-SATZ(WQ:WX) TO WT-TX.
           ADD 1 TO WI.
       Z.  EXIT.
      *************************************************** Read Artikel *
       READ-ARTIKEL SECTION.
       A.  MOVE WH-NUM TO AR-NUM.
           READ ARTIKEL INVALID DISPLAY "Artikel: " AT 2401
               AR-NUM " NICHT öBERNOMMEN!"
               PERFORM WEITER
               SET ESC TO TRUE.
           IF ZUGRIF PERFORM BESETZT GO A.
       Z.  EXIT.
      **************************************************** Read Rezept *
       READ-REZEPT SECTION.
       A.  INITIALIZE RE-SATZ.
           MOVE AR-NUM TO RE-NUM.
           READ REZEPT not INVALID go z.
           IF ZUGRIF PERFORM BESETZT GO A.
           WRITE RE-SATZ.
           GO A.
       Z.  EXIT.
      ****************************************** Kopf fÅr Excelausgabe *
       EXCELTAB SECTION.
       A.  EVALUATE WT-EX(IX)
               WHEN "Art.Nr." PERFORM HOL-NUM
                              PERFORM READ-ARTIKEL
                              ADD 308 VDU-ECK GIVING VDU-LP
                              MOVE AR-NUM TO WD-ART
                              DISPLAY WD-ART AT VDU-LP " " AR-BEZ
               WHEN "Artikel" PERFORM HOL-ALPHA
                              MOVE WT-TX TO AR-BEZ
               WHEN "Tg.Gew." PERFORM HOL-NUM
                              COMPUTE WH-NUM = WH-WERT * 1000
                              MOVE WH-NUM TO WH-GEW            *> in gr
               WHEN "Tg.Nr."  PERFORM HOL-NUM
                              PERFORM READ-REZEPT
                              MOVE WH-NUM TO WH-GRP
                              PERFORM FOLGE
               WHEN "RNr"     PERFORM HOL-NUM
                              MOVE WH-NUM TO WH-WERT AR-GRP
               WHEN "Tg.Bez." PERFORM HOL-ALPHA
                              MOVE WT-TX TO RE-BEZ
               WHEN "SGruppe" PERFORM HOL-NUM
                              ADD 308 VDU-ECK GIVING VDU-LP
                              MOVE WH-NUM TO AR-SGRP.
       Z.  EXIT.
      ************************************************** Teigfolge-Nr. *
       FOLGE SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 255
               IF WH-FOLGE(WX) = 0 or WH-FOLGE(WX) = WH-NUM
                   MOVE WH-NUM TO WH-FOLGE(WX)
                   MOVE 299 TO WX.
       Z.  EXIT.
