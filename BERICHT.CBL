      $SET LINKCOUNT"320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BERICHT.
      ******************************************************************
      * Einstellungen in VECTRON fÅr Export:
      *
      * - Konfiguration Kassen
      * - Kassa auswÑhlen
      * - bearbeiten
      * - Einstellungen - Optionen
      * - Lesungen exportieren
      *
      * Die Kassadaten werden automatisch nach c:\bersi exportiert und
      * in disem Programm (Bericht) von K:\ gelesen und als BERKA.00x
      * pro Tag umgespeichert und ins Verzeicnis C:\vectron\vcom6\imp\
      * als Datei VCOM.IMP kopiert.
      * Der Vectron-Commander mu· aktiv sein.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KERMODEL ASSIGN "FIBUBER.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KM-KEY
                           FILE STATUS WF-STATUS.
           SELECT EXCELIS  ASSIGN TO WH-FIBNAM
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT EXPODAT  ASSIGN TO WN-EXPO
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  KERMODEL                    LABEL RECORD STANDARD.
       01  KM-SATZ.
           03  KM-KEY.
               05 KM-KNR               PIC 99         COMP.
               05 KM-ZEIL              PIC 999V9      COMP.
           03  KM-ART                  PIC 99         COMP.
           03  KM-BET                  PIC S9(7)V99   COMP.
           03  KM-TEXT                 PIC X(42).
           03  KM-KZ                   PIC 99         COMP.
           03  KM-DATUM                PIC 9(8)       COMP.
           03  KM-ZEIT                 PIC 9(6)       COMP.
           03  KM-POS1                 PIC 99         COMP.
           03  KM-POS2                 PIC 99         COMP.
           03  KM-ZAHL                 PIC 9(6)       COMP.
           03  KM-EXTEXT               PIC X(30).
           03  KM-EXPO                 PIC 99         COMP.
      ******************************************************************
       FD  EXCELIS                     LABEL RECORD OMITTED.
       01  EXC-ASATZ                   PIC X(164).
       01  EXC-BSATZ.
           03  EXC-KTONR               PIC ZZZZ.
           03  EXC-TA                  PIC X.
           03  EXC-BEZ                 PIC X(50).
           03  EXC-TB                  PIC X.
           03  EXC-MBET                PIC ZZZZZZZZ9,99-.
           03  EXC-TC                  PIC X.
           03  EXC-GBET                PIC ZZZZZZZZ9,99-.
       01  EXC-SATZ                    PIC X(180).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ                    PIC X(42).
      ******************************************************************
       FD  EXPODAT                     LABEL RECORD OMITTED.
       01  EXP-SATZ                    PIC X(144).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REGI.
           03  WK-DRU                  PIC X(4).
           03  WL-REG                  PIC 9(15).
           03  WD-ZEIL                 PIC ZZ9-.
           03  WD-KER                  PIC Z9-.
           03  WD-ZL                   PIC Z9-.
           03  WD-KZ                   PIC 9.
           03  WD-JAHR                 PIC 9999.
           03  WD-ZEIT                 PIC 99B99B99.
           03  WD-DATUM.
               05 WD-TT                PIC Z9-.
               05 WD-MM                PIC 99-.
               05 WD-JJ                PIC 9999.
           03  WD-HILF                 PIC X(4).
           03  WD-POS                  PIC Z9.
           03  WD-ZAHL                 PIC ZZZZZ9.
           03  WD-SZAHL REDEFINES
               WD-ZAHL                 PIC ZZBZZ9.
           03  WD-BETP                 PIC -ZZ.ZZ9,99.
           03  WD-BET REDEFINES
               WD-BETP                 PIC -ZZZZZ9,99.
           03  WD-DZ                   PIC X(19).
           03  WM-KNR                  PIC 99         COMP.
           03  WM-ZEIL                 PIC 999        COMP.
           03  WH-MOD                  PIC 99         COMP.
           03  WX-TAG                  PIC 9(8)       COMP.
           03  WA-TAG                  PIC 9(8)       COMP.
           03  WV-TAG                  PIC XXX.
           03  WS-TAG                  PIC XXX.
           03  WM-EXBET                PIC S99999V99.
           03  WM-EXZAHL               PIC S99999V999.
           03  WM-EXZEIT               PIC 9(6).
           03  WM-KUNDEN               PIC 9(6)       COMP.
           03  WM-STRONO               PIC 9(6)       COMP.
           03  WD-XNUM                 PIC -Z(5)9.
           03  WD-XBER                 PIC -Z(6),ZZ.
           03  WD-XUST                 PIC -Z(6),Z(12).
           03  WM-UMSN                 PIC 9(3).
           03  WM-UMSB                 PIC 9(3).
           03  WM-UST                  PIC 9(3).
           03  WX-A                    PIC 9(6).
           03  WX-B                    PIC 9(6).
           03  WX-C                    PIC 9(6).
           03  WX-D                    PIC X(20).
           03  WX-E                    PIC 9(6).
      *--------------------------------------------> Bildschirmfelder <-
           03  WI-ATTR                 PIC X(2000).
           03  WI-ZEICH                PIC X(2000).
           03  WI-ERFTAB               PIC X(204).
           03  WX-ATTRIB               PIC X(80).
           03  WM-ATTRIB               PIC X(80).
           03  WX-LNG                  PIC 9(4)      COMP-X.
           03  WX-PAR.
               05 WX-L                 PIC 99        COMP-X.
               05 WX-P                 PIC 99        COMP-X.
           03  WX-PS REDEFINES WX-PAR  PIC 9999      COMP.
           03  WX-LG                   PIC 9999      COMP.
       01  WF-REG.
           03  WH-MIN                  PIC 99.
           03  WH-STD                  PIC 99.
           03  WX-ZER                  PIC 9(6)V9(12).
           03  WM-ANZ                  PIC 9(5).
           03  WD-ANZ                  PIC ZZ.ZZ9.
           03  WH-SIZE                 PIC 99        COMP   VALUE 62.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WH-FIBNAM               PIC X(24).
           03  WN-EXPO.
               05 WN-EXNA              PIC X(6).
               05 WN-EXNR              PIC 999.
               05 WN-REST              PIC X(15).
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 22.
           03  WT-DRUTAB.
               05 WM-DRUBUF            PIC X(42)    OCCURS 10.
               05 WM-DATA              PIC 9        OCCURS 10.
           03  WE                      PIC 999      COMP.
           03  WI                      PIC 999      COMP.
           03  WQ                      PIC S99      COMP.
           03  WP                      PIC 99       COMP.
           03  WL                      PIC 999      COMP.
           03  WS                      PIC 99       COMP.
           03  IX                      PIC 99       COMP.
           03  IY                      PIC 99       COMP.
           03  XS                      PIC 99       COMP.
           03  WM-BET                  PIC S9(9)V99 COMP.
           03  WX-BET                  PIC S9(9)V99 COMP.
           03  WX-NUM                  PIC S9(6)    COMP.
           03  WX-ZEIT                 PIC 9(6).
           03  WS-SUM                  PIC S9(9)V99 COMP.
           03  WH-UST                  PIC 9V99     COMP.
           03  WT-ANZ.
               05 WR-ANZ               OCCURS 10.
                  07 WT-KNR            PIC 99       COMP.
                  07 WT-TEXT           PIC X(25).
           03  WK-KZ                   PIC 99       COMP.
           03  WK-ART                  PIC 99       COMP.
           03  WK-BON                  PIC 9(6)     COMP.
           03  WK-KADAT                PIC 9(8)     COMP.
           03  WT-BERTAB.
               05 WT-BER                             OCCURS 100.
                  07 WKM-KEY.
                     09 WKM-KNR        PIC 99         COMP.
                     09 WKM-ZEIL       PIC 999V9      COMP.
                  07 WKM-ART           PIC 99         COMP.
                  07 WKM-BET           PIC S9(7)V99   COMP.
                  07 WKM-TEXT          PIC X(42).
                  07 WKM-KZ            PIC 99         COMP.
                  07 WKM-DATUM         PIC 9(8)       COMP.
                  07 WKM-ZEIT          PIC 9(6)       COMP.
                  07 WKM-POS1          PIC 99         COMP.
                  07 WKM-POS2          PIC 99         COMP.
                  07 WKM-ZAHL          PIC 9(6)       COMP.
                  07 WKM-EXTEXT        PIC X(30).
                  07 WKM-EXPO          PIC 99         COMP.
           03  WT-WKDAT                           OCCURS 10.
               05 WK-CSV               PIC X(35)     OCCURS 24.
           03  WT-CSDAT.
               05 WT-CSV               PIC X(35)     OCCURS 24.
           03  WT-DTN.
               05 WT-DATNAM            PIC X(24)     OCCURS 10.
           03  HI-SATZ.
               05 HI-ATTR              PIC X(2000).
               05 HI-ZEICH             PIC X(2000).
               05 HI-ERFTAB            PIC X(120).
           03  WV-EXTEXT               PIC X(30).
      *---------------------------------------------> Directory lesen <-
           03  WL-RESULT.
               05 WL-ERR               PIC X       COMP-X.
               05 WL-HANDLE            PIC XX      COMP-X.
               05 WL-ATTROUT           PIC X       COMP-X.
               05 WL-TIME              PIC XX      COMP-X.
               05 WL-DATE              PIC XX      COMP-X.
               05 WL-SIZE              PIC X(4)    COMP-X.
               05 WL-FOUT.
                  07 FILLER            PIC X(9).
                  07 WL-FIL            PIC 999.
                  07 WL-REST           PIC X(5).
           03  WL-FUNKT                PIC X       COMP-X   VALUE 69.
           03  WL-PARM.
               07 WL-ACT               PIC X       COMP-X.
               07 WL-ATTRIB            PIC X       COMP-X.
               07 WL-FILEIN            PIC X(24).
      *--------------------------------------------> check_file_exist <-
           03  WH-FNAME                PIC X(56).
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)   COMP-X.
               05 WF-TT                PIC 99     COMP-X.
               05 WF-MO                PIC 99     COMP-X.
               05 WF-YY                PIC 9999   COMP-X.
               05 WF-HH                PIC 99     COMP-X.
               05 WF-MM                PIC 99     COMP-X.
               05 WF-SS                PIC 99     COMP-X.
               05 WF-HS                PIC 99     COMP-X.
      *------------------------------------------------> Command-Line <-
           03  WD-UPON                 PIC X(64).
           03  RESULT                  PIC 99      COMP-X.
           03  FUNKT                   PIC 99      COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99      COMP-X VALUE 0.
               05 PAR                  PIC X(40)   VALUE SPACE.
       PROCEDURE DIVISION.
       DECLARATIVES.
       DECL-T SECTION.         USE AFTER ERROR PROCEDURE ON KERMODEL.
       A.  CALL "CADECL" USING "FIBUKER.DAT " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           ACCEPT WK-DRU FROM COMMAND-LINE.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           OPEN I-O KERMODEL.
           MOVE "     Bericht" TO WK-GEB.
           PERFORM VORTAG.
       E.  MOVE " Grp. auswÑhlen" TO WK-FIRMA.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY WK-DRU with background-color 4 AT 0175.
           PERFORM GRP-WAHL.
           IF EINF PERFORM KOPFDATEN GO E.
           IF ESC GO E.
           MOVE KM-TEXT TO WK-FIRMA.
           MOVE KM-KZ TO WK-KZ.
           MOVE KM-ART TO WK-ART.
           MOVE KM-ZAHL TO WK-BON.
           MOVE KM-DATUM TO WK-KADAT.
       G.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY WK-DRU with background-color 4 AT 0175.
           CALL "CAUP" USING "270511123800011" WH-CREG.
           IF WH-MCODE = "ok" MOVE 6 TO WH-PG
                              GO J.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Erfolgsbericht " with highlight
              AT VDU-LP.
           DISPLAY "1- Erfassung " AT 0717
                   "2- Zuordnung " AT 0017
                   "3- drucken   " AT 0017
                   "4- Datum     " AT 0017
                   "5- Wechsel   " AT 0017
                   "6- Åbernehmen" AT 0017.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT 1422.
       I.  DISPLAY "<ret-leer/esc>= Programmende" AT 2301.
           CALL "CAUP" USING "1010301001" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM.
                  SET RET TO TRUE.
           IF not RET or WH-NUM > 6 GO I.
           MOVE WH-NUM TO WH-PG.
       J.  EVALUATE WH-PG
               WHEN 0 GO X
               WHEN 1 PERFORM KER-ZUORD
               WHEN 2 PERFORM KER-ANL
               WHEN 3 PERFORM BONDRUCK
               WHEN 4 MOVE 0 TO WZ-DATUM WH-PG
                      CALL "CAUP" USING "03DATUM" WH-CREG
                      CALL "CAUP" USING "08CLOFEN" WH-CREG
                      GO G
               WHEN 5 CALL "CAUP" USING "08CLOFEN" WH-CREG
                      GO E
               WHEN 6 CALL "CAUP" USING "08CLOFEN" WH-CREG
                      PERFORM EXP-SUCH
                      GO Y
               WHEN 7 CALL "CAUP" USING "08CLOFEN" WH-CREG
                      PERFORM EXIMPORT
                      GO G.
           GO I.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Y.  CLOSE KERMODEL.
       Z.  GOBACK.
      ******************************************************************
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
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401
           PERFORM WEITER.
       Z.  EXIT.
      ************************************************ serieller Druck *
      *DRUCK SECTION.
      *A.  MOVE "COPY ""123456789012345678901234567890123456789012""
      *        "TO LPT2: " TO WD-UPON.
      *    MOVE DRA-SATZ(1:) TO WD-UPON(7:42).
      *    IF WK-DRU not = SPACE MOVE WK-DRU TO WD-UPON(15:4).
      *    DISPLAY WD-UPON UPON COMMAND-LINE.
      *    DISPLAY LOW-VALUES AT 2080.
      *    CALL X"91" USING RESULT FUNKT PARAM.
      *Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO A.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       KER-ANL SECTION.
       A.  CALL "CAUP" USING "270610146901012" WH-CREG.
           PERFORM CSV-SUCH.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Gliederung " with highlight foreground-color 6
                AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "   Zl Art Bezeichnung
      -        " P-1 P-2 KZ EX " with highlight foreground-color 3
                    reverse-video AT VDU-LP.
       B.  MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WR.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM LAST-KER.
           IF KM-KEY not = LOW-VALUE MOVE 1 TO IX
               PERFORM KER-ANZ
               PERFORM AUF-ZEIL.
       C.  PERFORM VARYING IX FROM 12 BY -1 UNTIL IX = 0
               OR WT-KEY(IX) not = LOW-VALUE
               END-PERFORM.
       E.  IF not XPOS PERFORM BS-ZEIL.
           MOVE 0 TO WS.
       F.  DISPLAY "<ret>= Pos.-Nr., <esc>= Ende < / >= auf/ab" AT 2301.
           DISPLAY "<Bild  / >= Seite auf/ab, <Entf>= lîschen, <Einfg>=
      -        "Zeile einfÅgen" AT 2401.
           PERFORM ANZEIG.
           MOVE KM-ZEIL TO WM-ZEIL WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 204.
           PERFORM OLDPIC.
           PERFORM ZEIHEL.
           CALL "CAUP" USING "1000003009" WH-CREG.
           PERFORM ZEIDUN.
           MOVE 0 TO WS.
       G.  EVALUATE TRUE
               WHEN ESC  GO X
               WHEN ENDE GO X
               WHEN SAPO PERFORM BER-REORG
                         GO A
               WHEN APOS MOVE 1 TO IX GO F
               WHEN BAUF PERFORM AUF-ZEIL GO F
               WHEN AUF  IF IX > 1 SUBTRACT 1 FROM IX GO F
                         else PERFORM AUF-ZEIL GO F
               WHEN BAB  IF WH-NUM not = 0 MULTIPLY 1000 BY WH-NUM
                             SET SAPO TO TRUE
                         else PERFORM ABZEIL
                              IF FINE GO C
                              else SUBTRACT 1 FROM IX GO F
               WHEN AB   IF WT-KEY(IX + 1) not = LOW-VALUE GO E
                         else PERFORM ABZEIL
                              IF FINE GO C
                              else GO F
               WHEN ENTF PERFORM LOEBEW
                         PERFORM AUSGAB
                         GO F
               WHEN EINF PERFORM EINFUEG
                         IF ESC PERFORM ALTBILD
                                GO F
               WHEN RET NEXT SENTENCE
               WHEN WORE; IF KM-ART = 0 MOVE KM-ZEIL TO WY
                          end-if GO F
               WHEN OTHER GO F.
           MOVE 0 TO WL WI WE.
           IF WH-WERT = 0 GO F.
           IF KM-ZEIL not = 0 and WH-WERT not = KM-ZEIL GO F.
           MOVE WH-WERT TO KM-ZEIL WD-ZEIL WM-ZEIL.
           MOVE WM-KNR TO KM-KNR.
           READ KERMODEL not INVALID MOVE 1 TO WS GO H.
           INITIALIZE KM-SATZ.
           MOVE WH-WERT TO KM-ZEIL.
       H.  DISPLAY WD-ZEIL with highlight foreground-color 3 AT VDU-LP.
           EVALUATE WS
               WHEN 0 GO I                          *> neue Zeile
               WHEN 1 PERFORM KER-ANZ               *> vorhandene Zeile
               WHEN 2 GO I.                         *> eingef. Zeile
       I.  DISPLAY "<esc>= Abbruch, <ret>= Art, < />= zurÅck, <#>= Hilf
      -        "e" AT 2301.
      *--> +10= T.MM.JJ SS:MM, +20= T.M.JJJJ SS:MM + 30= T.MM.JJ   ss:mm
      *------------------------------> LEERZEICHEN VON SS:MM BEACHTEN <-
           MOVE KM-ART TO WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 209.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC or WOLI or AUF PERFORM ALTBILD GO F.
           IF KIST PERFORM HI-ZEIG GO I.
           IF not RET GO I.
           MOVE WH-WERT TO WD-KER.
           DISPLAY WD-KER with highlight AT VDU-LP.
           MOVE WH-WERT TO KM-ART.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Text, < >= zurÅck" AT 2301.
           MOVE KM-TEXT TO WT-TX.
           COMPUTE VDU-LP = IX * 100 + 212.
           CALL "CAUP" USING "1200000142" WH-CREG.
           IF AUF GO I.
           IF ESC GO F.
           IF not RET GO K.
           MOVE WT-TX TO KM-TEXT.
           DISPLAY KM-TEXT with highlight AT VDU-LP.
       L.  DISPLAY "<esc>= Abbruch, <ret>= Pos, < />= zurÅck" AT 2301.
           DISPLAY "Druckpostion 1" AT 2401.
           MOVE KM-POS1 TO WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 256.
           CALL "CAUP" USING "1000003003" WH-CREG.
           IF ESC or WOLI or AUF GO K.
           IF not RET GO L.
           IF WH-WERT > 42 GO L.
           MOVE WH-WERT TO KM-POS1 WD-POS.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
       M.  DISPLAY "<esc>= Abbruch, <ret>= Pos, < />= zurÅck" AT 2301.
           DISPLAY "Druckpostion 2" AT 2401.
           MOVE KM-POS2 TO WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 260.
           CALL "CAUP" USING "1000003003" WH-CREG.
           IF ESC or WOLI or AUF GO L.
           IF not RET GO M.
           IF WH-WERT > 42 GO M.
           MOVE WH-WERT TO KM-POS2 WD-POS.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
       N.  DISPLAY "<esc>= Abbruch, <ret>= Kz., < />= zurÅck" AT 2301.
           DISPLAY "nicht drucken: 1- wenn 0, 2- wenn Grp. 0" AT 2401.
           MOVE KM-KZ TO WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 263.
           CALL "CAUP" USING "1000001001" WH-CREG.
      *    IF KIST PERFORM HIKZ-ZEIG GO N.
           IF ESC or WOLI or AUF GO M.
           IF not RET GO N.
           IF WH-WERT > 5 GO N.
           MOVE WH-WERT TO KM-KZ WD-POS.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
           CALL "CAUP" USING "271930034000014" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bezeichung im Excel " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bez.: " AT VDU-LP.
           ADD 6 TO VDU-P.
           DISPLAY KM-EXTEXT with highlight AT VDU-LP.
           PERFORM AUSWAHL.
           DISPLAY KM-EXTEXT with highlight AT VDU-LP.
       O.  DISPLAY "<esc>= Abbruch, <ret>= Text, < />= zurÅck" AT 2301.
           MOVE KM-EXTEXT TO WT-TX.
           CALL "CAUP" USING "1202090130" WH-CREG.
           IF ESC or WOLI or AUF
               CALL "CAUP" USING "08CLOFEN" WH-CREG
               GO N.
           IF not RET GO O.
           MOVE WT-TX TO KM-EXTEXT.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       P.  DISPLAY "<esc>= Abbruch, <ret>= Kz., < />= zurÅck" AT 2301.
           DISPLAY "Export-Kz" AT 2401.
           MOVE KM-EXPO TO WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 266.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC or WOLI or AUF GO N.
           IF not RET GO P.
           MOVE WH-WERT TO KM-EXPO WD-POS.
           DISPLAY WD-POS with highlight foreground-color 5 AT VDU-LP.
           IF WS = 1 REWRITE KM-SATZ INVALID GO F
                     end-rewrite GO U.
           MOVE WM-KNR TO KM-KNR.
       S.  WRITE KM-SATZ INVALID ADD 1 TO KM-ZEIL GO S.
       U.  IF WS = 2 PERFORM BER-REORG.
           COMPUTE WD-KER = KM-ZEIL * 10.
           MOVE KM-KEY TO WT-KEY(IX).
           MOVE WT-KEY(1) TO KM-KEY.
           PERFORM AUSGAB.
           GO E.
       W.  PERFORM ALTBILD.
           GO F.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       AUSWAHL SECTION.
       A.  CALL "CAUP" USING "0701011540000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Textauswahl " with highlight AT VDU-LP.
           MOVE 1 TO WE WL WX.
           SET AB TO TRUE.
       C.  IF WE = 0 GO E.
           IF WT-CSV(WE) = LOW-VALUES or WE > 24 GO E.
           MOVE WE TO WD-ZL.
           IF WL = 14 MOVE 13 TO WL
               CALL "CAUP" USING "17SCROLAUF" WH-CREG.
           IF WL = 0 MOVE 1 TO WL
               CALL "CAUP" USING "22SCROLAB" WH-CREG.
           COMPUTE VDU-LP = WL * 100 + 103 + VDU-ECK.
           DISPLAY WD-ZL AT VDU-LP.
           ADD 3 TO VDU-LP.
           DISPLAY WT-CSV(WE)(1:30) with highlight AT VDU-LP.
           EVALUATE TRUE
               WHEN AB ADD 1 TO WX WL
                       ADD 1 TO WE
               WHEN AUF ADD -1 TO WL
                        ADD 1 TO WX
                        ADD -1 TO WE.
           IF WX < 14 GO C.
       E.  DISPLAY "<esc>= Abbruch, < / >= blÑttern, <ret>= Text-Nr. <
      -        ">" AT 2301.
           CALL "CAUP" USING "0023512002" WH-CREG.
           IF ESC GO X.
           IF AB and WE < 25 MOVE -13 TO WX
                 IF WE = 0 MOVE 1 TO WE WL
                 end-if
                 GO C.
           IF AUF and WE > 1
                  MOVE -13 TO WX
                  MOVE 13 TO WL
      *           CALL "CAUP" USING "16CLRFEN" WH-CREG
                  IF WT-CSV(WE) = LOW-VALUES ADD -1 TO WE
                  end-if
                  GO C.
           IF not RET GO E.
           IF WH-NUM = 0 GO X.
           IF WH-NUM > 24 GO E.
      *    COMPUTE WE = WE - WL + WH-NUM - 1.
           MOVE WH-NUM TO WE.
           IF WT-CSV(WE) = LOW-VALUES GO E.
           MOVE WT-CSV(WE) TO WT-TX.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 30
               or WT-TX(WX:1) = ":" or ";" end-perform.
           MOVE WT-TX(1:WX) TO KM-EXTEXT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************** Art ************* Hilfetext anzeigen *
       HI-ZEIG SECTION.
       A.  CALL "CAUP" USING "270440123900015" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zeilenart " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "0 = nur Text" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "1 = + Datum u. Zeit" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "2 = + Zahl" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "3 = + Zahl u. Wert" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "4 = + Wert" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "5 = + 10% v. vorzl." with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "6 = + 20% v. vorzl." with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "7 = + Summe beid. Vorzl." with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "8 = + Zahl u. -Wert" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "9 = Datum + Zeit" with highlight AT VDU-LP.
       C.  DISPLAY "Weiter mit beliebiger Taste oder autom. nach 9 Sekun
      -        "den < >" AT 2301.
           ACCEPT WD-KZ AT 2358 TIMEOUT AFTER +9 with NO-ECHO
              SIZE 1 AUTO-SKIP NOT ON EXCEPTION CONTINUE.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************* KZ **************** Hilfetext anzeigen *
       HIKZ-ZEIG SECTION.
       A.  CALL "CAUP" USING "270440123900013" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zeilenart " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "0 = ohne Funktion" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "1 = nicht drucken wenn 0" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "2 = nicht drucken wenn GRp = 0" with highlight
                AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "3 =              " with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "4 = Summe = G-Ums." with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "5 = BonzÑhler" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "6 =                " with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "7 =                " with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "8 =                " with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "9 =             " with highlight AT VDU-LP.
       C.  DISPLAY "Weiter mit beliebiger Taste oder autom. nach 9 Sekun
      -        "den < >" AT 2301.
           ACCEPT WD-KZ AT 2358 TIMEOUT AFTER +9 with NO-ECHO
              SIZE 1 AUTO-SKIP NOT ON EXCEPTION CONTINUE.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************************** nÑchsten freien lesen *
       READ-NEXT SECTION.
       A.  INITIALIZE KM-SATZ.
           MOVE 999,9 TO KM-ZEIL.
           START KERMODEL KEY < KM-KEY INVALID GO Q.
           READ KERMODEL PREVIOUS IGNORE LOCK AT END GO Q.
       Q.  INITIALIZE KM-SATZ.
           MOVE WM-ZEIL TO KM-ZEIL.
       Z.  EXIT.
      ******************************************************************
       LAST-KER SECTION.
       A.  MOVE WM-KNR TO KM-KNR.
           MOVE 999,9 TO KM-ZEIL
           START KERMODEL KEY < KM-KEY INVALID MOVE 0 TO KM-ZEIL GO Z.
       C.  READ KERMODEL PREVIOUS IGNORE LOCK AT END
               MOVE 0 TO KM-ZEIL
               GO Z.
           IF WM-KNR not = KM-KNR MOVE 0 TO KM-ZEIL.
       Z.  EXIT.
      **************************************** Leistungszeile anzeigen *
       KER-ANZ SECTION.
       A.  COMPUTE VDU-LP = IX * 100 + 204 + VDU-ECK.
           MOVE KM-ZEIL TO WD-ZEIL.
           DISPLAY WD-ZEIL with highlight foreground-color 3 AT VDU-LP.
           ADD 5 TO VDU-LP.
           MOVE KM-ART TO WD-ZL.
           DISPLAY WD-ZL with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           DISPLAY KM-TEXT with highlight AT VDU-LP.
           ADD 44 TO VDU-LP.
           MOVE KM-POS1 TO WD-POS.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
           ADD 4 TO VDU-LP.
           MOVE KM-POS2 TO WD-POS.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
           ADD 3 TO VDU-LP.
           MOVE KM-KZ TO WD-POS.
           DISPLAY WD-POS with highlight foreground-color 6 AT VDU-LP.
           ADD 3 TO VDU-LP.
      *------------------------------> NéCHSTEN bEFEHLER WIEDER RAUS <-
           IF KM-EXPO = 32 MOVE 0 TO KM-EXPO.
           MOVE KM-EXPO TO WD-POS.
           DISPLAY WD-POS with highlight foreground-color 5 AT VDU-LP.
           ADD 1 TO WR.
           MOVE KM-KEY TO WT-KEY(IX).
           DISPLAY KM-EXTEXT with highlight foreground-color 6 at 2540.
       Z.  EXIT.
      ******************************************************************
       AUSGAB SECTION.
       A.  CALL "CAUP" USING "16CLREFN" WH-CREG.
           MOVE IX TO WE.
           MOVE 1 TO IX.
           MOVE WT-KEY(IX) TO KM-KEY.
           MOVE LOW-VALUE TO WT-ERFTAB.
           START KERMODEL KEY not < KM-KEY INVALID SET FINE TO TRUE
               GO X.
           SET BAB TO TRUE.
           MOVE 0 TO WI WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 11.
           IF WI not = 8 MOVE 9 TO WI.
           MOVE WE TO IX.
       H.  IF IX = 0 GO X.
           IF WT-KEY(IX) = LOW-VALUE ADD -1 TO IX GO H.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      **************************************************** weiterlesen *
       ABZEIL SECTION.
       A.  PERFORM VARYING IX FROM 12 BY -1 UNTIL IX = 0
               OR WT-KEY(IX) not = LOW-VALUE CONTINUE.
           IF IX = 0 MOVE 1 TO IX SET FINE TO TRUE GO X.
       C.  MOVE WT-KEY(IX) TO KM-KEY.
           IF AB OR (BAB and WH-NUM = 0) PERFORM READBEW
               ADD 1 TO IX
               MOVE 0 TO WI WR
               IF AB MOVE 10 TO WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 11.
           IF WI not = 8 MOVE 9 TO WI.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAB SECTION.
       E.  READ KERMODEL NEXT IGNORE LOCK AT END MOVE 8 TO WI GO Z.
           IF WM-KNR not = KM-KNR MOVE 8 TO WI
                                  SET FINE TO TRUE GO Z.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 11
               IF KM-KEY = WT-KEY(WF) GO E.
           IF IX = 12 CALL "CAUP" USING "17SCROLAUF" WH-CREG
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
                   MOVE WT-KEY(IX + 1) TO WT-KEY(IX)
                   END-PERFORM.
           PERFORM KER-ANZ.
           IF BAB ADD 1 TO IX.
       Z.  EXIT.
      ******************************************************************
       AUF-ZEIL SECTION.
       A.  IF WT-KEY(1) = LOW-VALUE SET FINE TO TRUE GO X.
           MOVE WT-KEY(1) TO KM-KEY.
           MOVE 0 TO WR.
           MOVE IX TO WX.
           IF AUF MOVE 09 TO WR.
           START KERMODEL KEY < KM-KEY INVALID SET FINE TO TRUE GO X.
           MOVE 1 TO IX WI.
           PERFORM NEXTAUF WITH TEST AFTER UNTIL WI = 2 OR WR = 10.
           MOVE 1 TO IX.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAUF SECTION.
       N.  READ KERMODEL PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO Z.
           IF WM-KNR not = KM-KNR or KM-ZEIL = 0 MOVE 2 TO WI GO Z.
           IF WI > 8 PERFORM VARYING WF FROM 12 BY -1 UNTIL WF < 1
               IF KM-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLLAB" WH-CREG.
           PERFORM VARYING IX FROM 11 BY -1 UNTIL IX < 2
               MOVE WT-KEY(IX - 1) TO WT-KEY(IX).
           COMPUTE VDU-LP = IX * 100 + 100.
           PERFORM KER-ANZ.
       Z.  EXIT.
      ******************************************************************
       NODATA SECTION.
       A.  DISPLAY "** keine Daten **" with highlight AT 2562.
           SET FINE TO TRUE.
       Z.  EXIT.
      ******************************************************************
       READBEW SECTION.
       C.  READ KERMODEL INVALID MOVE LOW-VALUE TO WT-ERFTAB.
           IF ZUGRIF PERFORM BESETZT GO C.
       Z.  EXIT.
      ******************************************* Einzelzeile anzeigen *
       ANZEIG SECTION.
       A.  SET RET TO TRUE.
           IF IX = 0 GO Z.
           COMPUTE VDU-LP = IX * 100 + 100.
           IF WT-KEY(IX) = LOW-VALUE INITIALIZE KM-SATZ
                IF WS not = 2 MOVE 0 TO WS
                end-if
                GO Z.
           MOVE WT-KEY(IX) TO KM-KEY.
       C.  READ KERMODEL KEY IS KM-KEY INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 1 TO WS.
           PERFORM KER-ANZ.
       Z.  EXIT.
      ************************************************** Zeile lîschen *
       LOEBEW SECTION.
       A.  DISPLAY "Wirklich lîschen? " with highlight at 2301
               " < >, <esc>= Abbruch, <Strg+Entf>= lîschen".
           CALL "CAUP" USING "0023210000" WH-CREG.
           IF ESC GO Z.
           IF not SENT GO A.
           DELETE KERMODEL INVALID NEXT SENTENCE.
           PERFORM BER-REORG.
       Z.  EXIT.
      ********************************************** Position einfÅgen *
       EINFUEG SECTION.
       A.  IF WH-WERT = 0 GO X.
           PERFORM ALTBILD.
           CALL "CAUP" USING "1323012580" WH-CREG.
           MOVE WT-KEY(IX) TO KM-KEY.
           ADD -,50 TO KM-ZEIL GIVING WH-WERT.
           IF WH-WERT not < KM-ZEIL GO X.
           IF IX = 1 MOVE WT-KEY(IX) TO KM-KEY
                     START KERMODEL KEY < KM-KEY INVALID GO C
                     end-start
                     READ KERMODEL PREVIOUS AT END GO C
                     end-read
                     IF WH-WERT not > KM-ZEIL GO X
                     else GO C.
           IF KM-ZEIL = 0 MOVE WT-KEY(IX - 1) TO KM-KEY
                          IF WH-NUM < KM-ZEIL GO X.
           MOVE WT-KEY(IX - 1) TO KM-KEY.
           IF WH-WERT not > KM-ZEIL GO X.
           IF IX = 11 MOVE WT-KEY(IX) TO KM-KEY
                      START KERMODEL KEY NOT < KM-KEY INVALID GO C
                      end-start
                      READ KERMODEL NEXT AT END GO C
                      end-read
                      IF WH-WERT not < KM-ZEIL GO X.
       C.  MOVE WH-WERT TO KM-ZEIL.
           READ KERMODEL NOT INVALID
               DISPLAY "bereits vorhanden" AT 2401
               PERFORM WEITER
               SET ESC TO TRUE
               GO Z.
           IF WT-KEY(IX) = LOW-VALUE GO X.
           MOVE 2 TO WS.
           PERFORM OLDPIC.
           PERFORM VARYING XS FROM 11 BY -1 UNTIL XS < IX
               MOVE WT-KEY(XS) TO WT-KEY(XS + 1).
           MOVE LOW-VALUE TO WT-KEY(IX).
           SET RET TO TRUE.
           GO Z.
       X.  DISPLAY "Nicht mîglich!" with highlight foreground-color 4
               AT 2401.
           PERFORM WEITER.
           SET ESC TO TRUE.
       Z.  EXIT.
      ******************************************* altes Bild abstellen *
       OLDPIC SECTION.
       A.  MOVE WT-ERFTAB TO WI-ERFTAB.
           MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_READ_SCR_CHATTRS" USING
                WX-PS WI-ZEICH WI-ATTR WX-LG.
           IF not EINF GO Z.
           CALL "CAUP" USING "220000SCRAB" WH-CREG.
       Z.  EXIT.
      ************************************* altes Bild wieder ausgeben *
       ALTBILD SECTION.
       A.  MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_WRITE_SCR_CHATTRS"
               USING WX-PS WI-ZEICH WI-ATTR WX-LG.
           MOVE WI-ERFTAB TO WT-ERFTAB.
       Z.  EXIT.
      ******************************************************************
       ZEIHEL SECTION.
       A.  COMPUTE WX-L = VDU-EL + 1 + IX.
           COMPUTE WX-P = VDU-EP + 1.
           MOVE 64 TO WX-LNG.
           CALL "CBL_READ_SCR_ATTRS" USING WX-PAR WM-ATTRIB WX-LNG.
           MOVE ALL x"71" TO WX-ATTRIB.
           CALL "CBL_WRITE_SCR_ATTRS" USING WX-PAR WX-ATTRIB WX-LNG.
       Z.  EXIT.
      ******************************************************************
       ZEIDUN SECTION.
       A.  COMPUTE WX-L = VDU-EL + 1 + IX.
           COMPUTE WX-P = VDU-EP + 1.
           MOVE 64 TO WX-LNG.
           CALL "CBL_WRITE_SCR_ATTRS" USING WX-PAR WM-ATTRIB WX-LNG.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO IX.
           IF IX < 12 GO Z.
       B.  CALL "CAUP" USING "17SCROLAUF" WH-CREG.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
               MOVE WT-KEY(IX + 1) TO WT-KEY(IX).
           MOVE LOW-VALUE TO WT-KEY(IX).
           SET AB TO TRUE.
           PERFORM ABZEIL.
           MOVE 11 TO IX.
       Z.  EXIT.
      ************************************************** Kassa lîschen *
       LOEKAS SECTION.
       A.  INITIALIZE KM-SATZ.
           MOVE WH-WERT TO KM-KNR WM-KNR.
           READ KERMODEL INVALID GO X.
       C.  DISPLAY "wirklich lîschen? " with highlight AT 2301
               "< > <esc>= Abbruch, <Einfg>= lîschen".
           CALL "CAUP" USING "0023210000" WH-CREG.
           IF ESC GO X.
           IF not EINF GO S.
           START KERMODEL KEY not < KM-KEY INVALID GO X.
       G.  READ KERMODEL NEXT AT END GO X.
           IF WM-KNR not = KM-KNR GO X.
           DELETE KERMODEL INVALID NEXT SENTENCE.
           GO G.
       x.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *********************************************** Vortag ermitteln *
       VORTAG SECTION.
       A.  ACCEPT WA-TAG FROM DATE.
           ADD 20000000 TO WA-TAG.
           COMPUTE WH-NUM = FUNCTION INTEGER-OF-DATE(WA-TAG).
       C.  ADD -1 TO WH-NUM.
           COMPUTE WA-TAG = FUNCTION DATE-OF-INTEGER(WH-NUM).
           MOVE WA-TAG TO WS-DATUM WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WO-TAG TO WV-TAG.
           IF WO-TGN = 0 GO C.                     *> Sonntag Åberlesen
       Z.  EXIT.
      ************************************************* Grp. auswÑhlen *
       GRP-WAHL SECTION.
       A.  INITIALIZE KM-SATZ WT-ANZ.
           MOVE 1 TO KM-KNR.
           MOVE 0 TO WY.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               MOVE 0 TO KM-ZEIL
               READ KERMODEL not INVALID
                   ADD 1 TO WY
                   MOVE KM-KNR TO WT-KNR(WY)
                   MOVE KM-TEXT TO WT-TEXT(WY)
               end-read
               ADD 1 TO KM-KNR.
           MOVE 270510124000014 TO WL-REG.
           ADD 3 WY GIVING WD-ZL.
           MOVE WD-ZL TO WL-REG(7:2).
           CALL "CAUP" USING WL-REG WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Grp " AT VDU-LP.
           ADD 7 TO VDU-LP.
           DISPLAY " Bezeichnung " AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
              IF WT-KNR(WX) not = 0 MOVE WT-KNR(WX) TO WD-POS
                  COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK
                  DISPLAY WD-POS with highlight AT VDU-LP
                  ADD 8 TO VDU-LP
                  DISPLAY WT-TEXT(WX) with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Auswahl, <Entf>= lîschen, <Ei
      -        "nfg>= Kopfdaten" AT 2301.
           COMPUTE VDU-LP = WY * 100 + 204.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF ESC GOBACK.
      *    IF ENTF PERFORM LOEKAS
      *            GO A.
           IF ENDE MOVE 1 TO WH-WERT
                   SET RET TO TRUE
                   MOVE "ok" TO WH-MCODE.
           IF not RET and not EINF GO E.
           INITIALIZE KM-SATZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > WY
               or WH-WERT = WT-KNR(WZ) end-perform.
           MOVE WT-KNR(WZ) TO KM-KNR WM-KNR.
           MOVE WH-WERT TO KM-KNR WM-KNR.
           IF WM-KNR = 0 GO E.
           READ KERMODEL not INVALID GO X.
           MOVE 10 TO WX.
           ADD -1 TO VDU-P.
       F.  MOVE WH-WERT TO KM-KNR WD-POS.
           MOVE 0 TO KM-ZEIL.
           PERFORM KOPFDATEN.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** éndrung Kopfdaten *
       KOPFDATEN SECTION.
       A.  CALL "CAUP" USING "271010084000010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " énderung Kopfdaten " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bez..: " AT VDU-LP.
           DISPLAY KM-TEXT(1:25) with highlight AT 0000.
           ADD 100 TO VDU-LP.
           DISPLAY "Zeit.: " AT VDU-LP.
           EVALUATE KM-KZ
               WHEN 0 DISPLAY " 0:00" with highlight AT 0000
               WHEN 1 DISPLAY "00:00" with highlight AT 0000
               WHEN 3 DISPLAY "00:00:00" with highlight AT 0000.
           ADD 100 TO VDU-LP.
           DISPLAY "Punkt: " AT VDU-LP.
           EVALUATE KM-ART
               WHEN 0 DISPLAY "1234,56" with highlight AT 0000
               WHEN 1 DISPLAY "1 234,56" with highlight AT 0000
               WHEN 2 DISPLAY "1.234,56" with highlight AT 0000.
           ADD 100 TO VDU-LP.
           DISPLAY "Bonz.: " AT VDU-LP.
           MOVE KM-ZAHL TO WD-ZAHL.
           ADD 100 TO VDU-LP.
           DISPLAY WD-ZAHL with highlight AT 0000.
           DISPLAY "B-Dat: " AT VDU-LP.
           MOVE KM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0000.
           ADD 100 TO VDU-LP.
           DISPLAY "-Ums-: " AT VDU-LP.
           MOVE KM-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT 0000.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Bezeichnung" AT 2301.
           MOVE KM-TEXT TO WT-TX.
           CALL "CAUP" USING "1202100125" WH-CREG.
           IF ESC GO X.
           IF not RET GO G.
           MOVE WT-TX TO KM-TEXT.
           IF KM-TEXT = SPACE GO G.
           DISPLAY KM-TEXT(1:25) with highlight AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, <ret>= Zeitdarstellung" AT 2301.
           DISPLAY "0= 0:00, 1= 00:00, 2= 00:00:00" AT 2401.
           DISPLAY "Druckbild am Bon" AT 2501.
           MOVE KM-KZ TO WH-WERT.
           CALL "CAUP" USING "1003101001" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO G.
           IF not RET GO I.
           IF WH-WERT > 3 GO I.
           MOVE WH-WERT TO KM-KZ.
           DISPLAY "         " AT VDU-LP.
           EVALUATE KM-KZ
               WHEN 0 DISPLAY " 0:00" with highlight AT VDU-LP
               WHEN 1 DISPLAY "00:00" with highlight AT VDU-LP
               WHEN 2 DISPLAY "00:00:00" with highlight AT VDU-LP.
           WRITE KM-SATZ INVALID REWRITE KM-SATZ.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Tausenderpunkt" AT 2301.
           DISPLAY "0= 1234,56 1= 1 234,56, 2= 1.234,56" AT 2401.
           DISPLAY "Druckbild am Bon" AT 2501.
           MOVE KM-ART TO WH-WERT.
           CALL "CAUP" USING "1004101001" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO I.
           IF not RET GO K.
           IF WH-WERT > 3 GO K.
           MOVE WH-WERT TO KM-ART.
           DISPLAY "         " AT VDU-LP.
           EVALUATE KM-ART
               WHEN 0 DISPLAY "1234,56" with highlight AT VDU-LP
               WHEN 1 DISPLAY "1 234,56" with highlight AT VDU-LP
               WHEN 2 DISPLAY "1.234,56" with highlight AT VDU-LP.
       M.  DISPLAY "<esc>= Abbruch, <ret>= BonzÑhler " AT 2301.
           DISPLAY "BonzÑhlerstand des Vortages" AT 2401.
           MOVE KM-ZAHL TO WH-WERT.
           CALL "CAUP" USING "1005106006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO K.
           IF not RET GO M.
           MOVE WH-WERT TO KM-ZAHL WD-ZAHL.
           DISPLAY WD-ZAHL with highlight AT VDU-LP.
       O.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           DISPLAY "Datum zum BonzÑhler lt. Vorzeile" AT 2401.
           MOVE KM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE KM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1106106006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO M.
           IF not RET GO O.
           MOVE WX-DATUM TO KM-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       P.  DISPLAY "<esc>= Abbruch, <ret>= -Umsatz (NETTO) " AT 2301.
           DISPLAY "nur ganze 10,oo Euro" AT 2401.
           MOVE KM-BET TO WH-WERT.
           CALL "CAUP" USING "1007103003" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO O.
           IF not RET GO P.
           COMPUTE KM-BET = WH-WERT / 1000.
           COMPUTE KM-BET = KM-BET * 1000.
           MOVE KM-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
       S.  DISPLAY "<esc>= Abbruch, <ret>= speichern < >" AT 2301.
           CALL "CAUP" USING "0023350000" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO O.
           IF not RET GO S.
           WRITE KM-SATZ INVALID REWRITE KM-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************************** Anlage KER-Auswertung *
       KER-LIST SECTION.
       A.  CALL "CAUP" USING "270612075500003" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Auswertung " with highlight AT VDU-LP.

       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************ KERTAB einlesen *
       EINLESEN SECTION.
       A.  MOVE WM-KNR TO KM-KNR.
           MOVE 1 TO KM-ZEIL WI.
           MOVE LOW-VALUES TO WT-BERTAB.
           START KERMODEL KEY not < KM-KEY INVALID STOP RUN.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 100
               READ KERMODEL NEXT AT END NEXT SENTENCE
               end-read
               IF KM-ZEIL = 0 or KM-KNR not = WM-KNR NEXT SENTENCE
               else MOVE KM-TEXT TO WKM-TEXT(WE)
                    MOVE KM-ART TO WKM-ART(WE)
                    MOVE KM-KZ TO WKM-KZ(WE)
                    MOVE KM-BET TO WKM-BET(WE)
                    MOVE KM-DATUM TO WKM-DATUM(WE)
                    MOVE KM-ZEIT TO WKM-ZEIT(WE)
                    MOVE KM-POS1 TO WKM-POS1(WE)
                    MOVE KM-POS2 TO WKM-POS2(WE)
                    MOVE KM-ZAHL TO WKM-ZAHL(WE)
                    MOVE KM-EXTEXT TO WKM-EXTEXT(WE)
                    IF KM-EXTEXT(1:10) = LOW-VALUES
                        MOVE SPACE TO WKM-EXTEXT(WE)
                    end-if
                    MOVE KM-EXPO TO WKM-EXPO(WE)
                    IF WH-PG = 6
                        IF KM-KZ = 1 MOVE 0 TO WKM-ZEIT(WE) WKM-BET(WE)
                                               WKM-ZAHL(WE)
                    end-if.
       Z.  EXIT.
      ***************************************** serielle KER-Zuordnung *
       KER-ZUORD SECTION.
       A.  CALL "CAUP" USING "270210214800014" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Erfassung " with highlight AT VDU-LP.
           PERFORM EINLESEN.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 19
               PERFORM DIS-ZEIL
               ADD 1 TO WI.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WKM-ART(WE) not = 0
               CONTINUE.
           MOVE WE TO WI.
       G.  EVALUATE WKM-ART(WE)
               WHEN 1 PERFORM DATEIN
               WHEN 2 PERFORM ZAHLEIN
               WHEN 3 PERFORM ZAHLEIN
               WHEN 4 PERFORM BETEIN
               WHEN 5 PERFORM UST
               WHEN 6 PERFORM UST
               WHEN 7 PERFORM SUMVORZL
               WHEN 8 PERFORM ZAHLEIN
               WHEN 9 PERFORM DATEIN.
           IF ESC or ENDE GO Q.
           IF not AUF and not WOLI GO M.
       I.  IF WE = 1 GO Q.
           ADD -1 TO WE.
           IF WI = 1 CALL "CAUP" USING "22SCROLAB" WH-CREG
           else ADD -1 TO WI.
           PERFORM DIS-ZEIL.
           IF WKM-ART(WE) = 0 GO I.
           GO G.
       M.  ADD 1 TO WI.
           IF WE < 19 ADD 1 TO WE GO G.
           IF WI > 19 CALL "CAUP" USING "17SCROLAUF" WH-CREG
               MOVE 19 TO WI.
       P.  ADD 1 TO WE.
           IF WT-BER(WE) = LOW-VALUES GO Q.
           IF WI > 19 CALL "CAUP" USING "17SCROLAUF" WH-CREG
               MOVE 19 TO WI.
           PERFORM DIS-ZEIL.
           IF WKM-ART(WE) = 0 ADD 1 TO WI
               GO P.
           GO G.
       Q.  DISPLAY "<esc>= Abbruch, <Ende>= speichern, <ret>= drucken un
      -        "d speichern < >" AT 2301.
           CALL "CAUP" USING "0023660000" WH-CREG.
           EVALUATE TRUE
               WHEN RET PERFORM ABSTELL
                        PERFORM BONDRUCK
               WHEN ENDE PERFORM ABSTELL
               WHEN ESC GO X
               WHEN OTHER GO Q.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       DIS-ZEIL SECTION.
       A.  COMPUTE VDU-LP = WI * 100 + 104 + VDU-ECK
           DISPLAY WKM-TEXT(WE) with highlight AT VDU-LP.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE) - 1.
           EVALUATE WKM-ART(WE)
               WHEN 0 MOVE 0 TO WH-NUM
      *        WHEN 1 MOVE WM-DATUM TO WX-DATUM
               WHEN 11
               WHEN 21
               WHEN 1 MOVE WKM-DATUM(WE) TO WX-DATUM
                      PERFORM GETDATE
                      IF WK-KZ = 2
                           DISPLAY WD-DZ with highlight AT VDU-LP
                      else DISPLAY WD-DZ(1:16) with highlight AT VDU-LP
               WHEN 2 MOVE WKM-ZAHL(WE) TO WD-ZAHL
                      DISPLAY WD-ZAHL with highlight AT VDU-LP
               WHEN 3 MOVE WKM-ZAHL(WE) TO WD-ZAHL
                      DISPLAY WD-ZAHL with highlight AT VDU-LP
                      MOVE WKM-BET(WE) TO KM-BET
                      IF WKM-POS2(WE) not = 0 PERFORM BET-AUF
                          COMPUTE VDU-LP =
                              VDU-LP - WKM-POS1(WE) + WKM-POS2(WE)
                          DISPLAY WD-BET with highlight AT VDU-LP
               WHEN 4
               WHEN 5
               WHEN 6
               WHEN 7 MOVE WKM-BET(WE) TO KM-BET
                      PERFORM BET-AUF
                      DISPLAY WD-BET with highlight AT VDU-LP
               WHEN 8 MOVE WKM-ZAHL(WE) TO WD-ZAHL
                      DISPLAY WD-ZAHL with highlight AT VDU-LP
                      COMPUTE WH-WERT = WKM-BET(WE) * -1
                      MOVE WKM-BET(WE) TO KM-BET
                      PERFORM BET-AUF
                      IF WH-WERT < 0
                        PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
                          IF WD-BET(WZ:1) not = " " ADD -1 WZ GIVING WX
                             MOVE "-" TO WD-BET(WX:1)
                             MOVE 7 TO WZ
                          end-if
                      end-if
                      COMPUTE VDU-LP =
                          VDU-LP - WKM-POS1(WE) + WKM-POS2(WE)
                      DISPLAY WD-BET with highlight AT VDU-LP
      *        WHEN 9 MOVE WM-DATUM TO WX-DATUM
               WHEN 39
               WHEN 9 MOVE WKM-DATUM(WE) TO WX-DATUM
                      PERFORM GETDATE
                      IF WKM-TEXT(WE)(1:3) = "(6)"
                          DISPLAY VDU-DATUM with highlight AT VDU-LP
                      else DISPLAY WD-DATUM with highlight AT VDU-LP
                      COMPUTE VDU-LP =
                          VDU-LP - WKM-POS1(WE) + WKM-POS2(WE)
                      IF WK-KZ = 2
                           DISPLAY WD-ZEIT with highlight AT VDU-LP
                      else DISPLAY WD-ZEIT(1:5) with highlight
                                 AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       DATEIN SECTION.
       A.  PERFORM GETDATE.
           COMPUTE VDU-LP = WI * 100 + 103 + VDU-ECK.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE).
           IF WKM-ART(WE) = 9 and WKM-TEXT(WE)(1:3) = "(6)"
                DISPLAY VDU-DATUM with highlight AT VDU-LP " "
           else DISPLAY WD-DATUM with highlight AT VDU-LP " ".
           PERFORM DATDREH.
           MOVE WKM-DATUM(WE) TO WZ-DATUM WH-WERT.
           COMPUTE VDU-LP = WI * 100  + 103.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE).
       C.  DISPLAY "<esc/Ende>= Ende, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1100006006" WH-CREG.
           IF AUF or WOLI GO Z.
           IF ESC or ENDE GO Z.
           IF not RET GO A.
           MOVE WX-DATUM TO WKM-DATUM(WE).
           PERFORM GETDATE.
           IF WKM-ART(WE) = 9 and WKM-TEXT(WE)(1:3) = "(6)"
                DISPLAY VDU-DATUM with highlight AT VDU-LP " "
           else DISPLAY WD-DATUM with highlight AT VDU-LP " ".
       E.  COMPUTE VDU-LP = WI * 100 + 103.
           COMPUTE VDU-P = VDU-P + WKM-POS2(WE).
           DISPLAY "<esc/Ende>= Ende, <ret>= Zeit " AT 2301.
           EVALUATE WK-KZ
               WHEN 0
               WHEN 1 DISPLAY "hhmm" with highlight AT 0000
                      COMPUTE WH-WERT = WKM-ZEIT(WE)  / 100
                      CALL "CAUP" USING "1000004004" WH-CREG
               WHEN 2 DISPLAY "hhmmss" with highlight AT 0000
                      MOVE WKM-ZEIT(WE) TO WH-WERT
                      CALL "CAUP" USING "1000006006" WH-CREG.
           IF ESC or ENDE GO Z.
           IF WOLI or AUF GO A.
           IF WK-KZ < 2 COMPUTE WKM-ZEIT(WE) = WH-WERT * 100
                   else MOVE WH-WERT TO WKM-ZEIT(WE).
           IF WH-WERT > 2359 GO E.
           IF WH-WERT < 400 GO E.
           PERFORM GETDATE.
           COMPUTE VDU-LP = WI * 100 + 103 + VDU-ECK.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE).
           IF WKM-ART(WE) = 9 COMPUTE VDU-LP = WI * 100 + 103 + VDU-ECK
                COMPUTE VDU-P = VDU-P + WKM-POS1(WE)
                DISPLAY VDU-DATUM with highlight AT VDU-LP
                COMPUTE VDU-LP = VDU-LP - WKM-POS1(WE) + WKM-POS2(WE)
                IF WK-KZ = 2 DISPLAY WD-ZEIT with highlight AT VDU-LP
                else DISPLAY WD-ZEIT(1:5) with highlight AT VDU-LP
                end-if
           else IF WK-KZ = 2 DISPLAY WD-DZ with highlight AT VDU-LP
                else DISPLAY WD-DZ(1:16) with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       ZAHLEIN SECTION.
       A.  MOVE WKM-ZAHL(WE) TO WH-WERT.
       C.  COMPUTE VDU-LP = WI * 100 + 103.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE).
           DISPLAY "<esc/Ende>= Ende, <ret>= Zahl, < >= plus 1" AT 2301.
           CALL "CAUP" USING "1000006006" WH-CREG.
           IF AUF or WOLI GO Z.
           IF ESC or ENDE GO Z.
           IF AB ADD 1 TO WH-WERT.
           IF not RET and not AB GO C.
           MOVE WH-WERT TO WKM-ZAHL(WE) WD-ZAHL.
           DISPLAY WD-ZAHL with highlight AT VDU-LP.
           IF WKM-ART(WE) = 2 GO Z.
           IF WKM-POS2(WE) = 0 GO Z.
       G.  MOVE WKM-BET(WE) TO WH-WERT.
           MOVE WI TO WI.
           IF WI > 19 MOVE 19 TO WI.
           COMPUTE VDU-LP = WI * 100 + 103.
           COMPUTE VDU-P = VDU-P + WKM-POS2(WE).
           CALL "CAUP" USING "1000006209" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           MOVE WH-WERT TO WKM-BET(WE) WD-BET.
           IF WKM-ART(WE) = 8 COMPUTE WH-WERT = WH-WERT * -1
               IF WH-WERT < 0
                  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
                      IF WD-BET(WZ:1) not = " " ADD -1 WZ GIVING WX
                          MOVE "-" TO WD-BET(WX:1)
                          MOVE 7 TO WZ.
           DISPLAY WD-BET with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       BETEIN SECTION.
       A.  MOVE WKM-BET(WE) TO WH-WERT.
           COMPUTE VDU-LP = WI * 100 + 103.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE).
           DISPLAY "<esc/Ende>= Ende, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1000006209" WH-CREG.
           IF AUF or WOLI GO Z.
           IF ESC or ENDE GO Z.
           IF not RET GO A.
           MOVE WH-WERT TO WKM-BET(WE) KM-BET.
           PERFORM BET-AUF.
           DISPLAY WD-BET with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************** Druckdatum erzeugen *
       GETDATE SECTION.
       A.  IF WKM-KZ(WE) = 1 MOVE WX-DATUM TO WKM-DATUM(WE).
           IF WKM-DATUM(WE) = 0 MOVE WM-DATUM TO WKM-DATUM(WE).
           MOVE WKM-DATUM(WE) TO WZ-DATUM WC-DATUM.
           PERFORM DATDREH.
           MOVE WKM-DATUM(WE) TO WZ-DATUM WC-DATUM.
           MOVE WZ-TAG TO WD-TT.
           MOVE WZ-MONAT TO WD-MM.
           COMPUTE WD-JJ = WZ-JAHR + 2000.
           MOVE SPACE TO WD-DZ.
           MOVE "." TO WD-DATUM(3:1) WD-DATUM(6:1).
           IF WD-DATUM(4:1) = "0" MOVE SPACE TO WD-HILF
               MOVE WD-DATUM(1:3) TO WD-HILF(2:3)
               MOVE WD-HILF TO WD-DATUM(1:4).
           MOVE WKM-ZEIT(WE) TO WD-ZEIT.
           MOVE ":" TO WD-ZEIT(3:1) WD-ZEIT(6:1).
           MOVE WD-ZEIT TO WD-DZ(12:).
           IF WK-KZ = 0 and WD-ZEIT(1:1) = 0
               MOVE WD-DATUM TO WD-DZ(2:11)
           else MOVE WD-DATUM TO WD-DZ(1:10).
       Z.  EXIT.
      ********************************* 5 / 6 ******************** Ust *
       UST SECTION.
       A.  MOVE WKM-BET(WE - 1) TO WM-BET.
           MOVE ,10 TO WH-UST.
           IF WKM-ART(WE) = 6 MOVE ,20 TO WH-UST.
           COMPUTE WKM-BET(WE) rounded = WM-BET * WH-UST.
           COMPUTE VDU-LP = WI * 100 + 103 + VDU-ECK.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE).
           MOVE WKM-BET(WE) TO KM-BET.
           PERFORM BET-AUF.
           DISPLAY WD-BET with highlight AT VDU-LP.
       Z.  EXIT.
      *********************************** 7 ********** Summenvorzeilen *
       SUMVORZL SECTION.
       A.  COMPUTE WKM-BET(WE) =  WKM-BET(WE - 2) + WKM-BET(WE - 1).
           COMPUTE VDU-LP = WI * 100 + 103 + VDU-ECK.
           COMPUTE VDU-P = VDU-P + WKM-POS1(WE).
           MOVE WKM-BET(WE) TO KM-BET.
           PERFORM BET-AUF.
           DISPLAY WD-BET with highlight AT VDU-LP.
       Z.  EXIT.
      ********************************************** Bericht abstellen *
       ABSTELL SECTION.
       A.  PERFORM BONZAHL.
           MOVE WM-KNR TO KM-KNR.
           MOVE 0 TO KM-ZEIL.
           READ KERMODEL.
           MOVE WK-KADAT TO KM-DATUM.
           MOVE WK-BON TO KM-ZAHL.
           REWRITE KM-SATZ.
           START KERMODEL KEY > KM-KEY INVALID GO Z.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 100
               READ KERMODEL NEXT AT END NEXT SENTENCE
               end-read
               IF KM-ZEIL = 0 or KM-KNR not = WM-KNR NEXT SENTENCE
               else MOVE WKM-TEXT(WE) TO KM-TEXT
                    MOVE WKM-ART(WE) TO KM-ART
                    MOVE WKM-KZ(WE) TO KM-KZ
                    MOVE WKM-BET(WE) TO KM-BET
                    MOVE WKM-DATUM(WE) TO KM-DATUM
                    MOVE WKM-ZEIT(WE) TO KM-ZEIT
                    MOVE WKM-POS1(WE) TO KM-POS1
                    MOVE WKM-POS2(WE) TO KM-POS2
                    MOVE WKM-ZAHL(WE) TO KM-ZAHL
                    MOVE WKM-EXTEXT(WE) TO KM-EXTEXT
                    MOVE WKM-EXPO(WE) TO KM-EXPO
                    REWRITE KM-SATZ.
       Z.  EXIT.
      ******************************************** BonzÑhler ermitteln *
       BONZAHL SECTION.
       A.  MOVE 0 TO WM-KUNDEN WM-STRONO.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 99
               or WKM-KZ(WZ) = 5 end-perform.
           IF WZ = 100 GO Z.
           IF WK-KADAT = WM-DATUM GO Z.                *> Tag erledigt!
           MOVE WKM-EXTEXT(WZ) TO WT-TX.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 30 or
               WT-TX(WX:1) = ":" end-perform.
           MOVE WT-TX(1:WX) TO WV-EXTEXT.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 99
               or WKM-EXTEXT(WE) = WV-EXTEXT end-perform
           IF WKM-EXTEXT(WE) = SPACE GO G.
           IF WE = 100 GO G.
           MOVE WKM-ZAHL(WE) TO WM-KUNDEN.                    *> Kunden
           ADD 1 TO WX.
       G.  PERFORM VARYING WX FROM WX BY 1 UNTIL WX > 30 or
               WT-TX(WX:1) not = "+" and not = " " end-perform.
           MOVE WT-TX(WX:) TO WV-EXTEXT.
           PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 99
               or WKM-EXTEXT(WE) = WV-EXTEXT end-perform
           IF WKM-EXTEXT(WE) = SPACE GO K.
           IF WE = 100 GO K.
           MOVE WKM-ZAHL(WE) TO WM-STRONO.                    *> Storno
       K.  COMPUTE WKM-ZAHL(WZ) = WK-BON + WM-KUNDEN + WM-STRONO.
           MOVE WM-DATUM TO WKM-DATUM(WZ).
       Z.  EXIT.
      *************************************** Reorganisation KER-Daten *
       BER-REORG SECTION.
       A.  INITIALIZE KM-SATZ WT-BERTAB.
           MOVE WM-KNR TO KM-KNR.
           MOVE 1 TO WE.
           START KERMODEL KEY not < KM-KEY INVALID STOP RUN.
       C.  READ KERMODEL NEXT AT END GO G.
           IF WM-KNR not = KM-KNR GO G.
           DELETE KERMODEL.
           COMPUTE KM-ZEIL = WE - 1.
           MOVE KM-SATZ TO WT-BER(WE).
           ADD 1 TO WE.
           GO C.
       G.  PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 99
               IF WT-BER(WE) not = LOW-VALUES
                   MOVE WT-BER(WE) TO KM-SATZ
                   WRITE KM-SATZ INVALID NEXT SENTENCE.
       Z.  EXIT.
      ******************************************************* Bondruck *
       BONDRUCK SECTION.
       A.  MOVE "BON.LST" TO WH-DRUNAM.
           OPEN OUTPUT DRUCKER.
           MOVE "    Test " TO DRA-SATZ.
           PERFORM DRUCK.
           MOVE "    Test " TO DRA-SATZ.
           PERFORM DRUCK.
           PERFORM DRUCK.
           MOVE x"1d564200" TO DRA-SATZ.                *> schneiden
           PERFORM DRUCK.
           MOVE 0 TO WK-BON WK-DATUM WP.
           INITIALIZE WT-DRUTAB.
           INITIALIZE KM-SATZ.
           MOVE WM-KNR TO KM-KNR.
           START KERMODEL KEY > KM-KEY INVALID CLOSE DRUCKER GO Z.
       C.  READ KERMODEL NEXT AT END GO Q.
           IF KM-KNR not = WM-KNR GO Q.
           IF KM-KZ = 5 MOVE KM-ZAHL TO WK-BON
                        MOVE KM-DATUM TO WK-DATUM.
           PERFORM DRU-ZEIL.
           GO C.
       Q.  MOVE x"1d564200" TO DRA-SATZ.                *> schneiden
           PERFORM DRUCK.
           CLOSE DRUCKER.
      *-------------------> MODE  com1:961,n,8,1 und MODE lpt2:=com1: <-
           MOVE "COPY BON.LST LPT2: >NUL" TO WD-UPON.
           IF WK-DRU not = SPACE MOVE WK-DRU TO WD-UPON(14:4).
           DISPLAY WD-UPON UPON COMMAND-LINE.
           DISPLAY LOW-VALUES AT 2080.
           CALL X"91" USING RESULT FUNKT PARAM.
           MOVE WM-KNR TO KM-KNR.
           MOVE 0 TO KM-ZEIL.
           READ KERMODEL.
           MOVE WK-KADAT TO KM-DATUM.
           MOVE WK-BON TO KM-ZAHL.
           REWRITE KM-SATZ.
       Z.  EXIT.
      *        move x"1b6101" to dra-satz               *> zentrieren
      *        move x"1b2110" to dra-satz               *> ?
      *        move x"1b6100" to dra-satz               *> linksbÅndig
      *        move x"1b6102" to dra-satz               *> rechtsbÅndig
      *        move x"1d564200" to dra-satz             *> schneiden
      ******************************************************************
       DRU-ZEIL SECTION.
       A.  IF KM-KZ = 0 and WP > 0 PERFORM DRU-BUF.
      *    IF KM-ZEIL = 2,0
      *        MOVE x"1b6101" TO DRA-SATZ               *> zentrieren
      *        MOVE KM-TEXT TO DRA-SATZ(4:).
           MOVE KM-TEXT TO DRA-SATZ.
           EVALUATE KM-ART
               WHEN 0 MOVE 0 TO WH-NUM
               WHEN 11
               WHEN 21
               WHEN 31
               WHEN 01 PERFORM DRUDATE
                       MOVE WD-DZ TO DRA-SATZ(KM-POS1:)
               WHEN 2 IF KM-ZAHL = 0 GO Z
                      end-if
                      MOVE KM-ZAHL TO WD-ZAHL
                      IF KM-ZEIL > 24 MOVE KM-ZAHL TO WD-SZAHL
                      end-if
                      MOVE WD-ZAHL TO DRA-SATZ(KM-POS1:)
               WHEN 3 IF KM-ZAHL = 0 GO Z
                      end-if
                      MOVE KM-ZAHL TO WD-ZAHL
                      IF KM-ZEIL > 24 MOVE KM-ZAHL TO WD-SZAHL
                      end-if
                      MOVE WD-ZAHL TO DRA-SATZ(KM-POS1:)
                          PERFORM BET-AUF
                      MOVE WD-BET TO DRA-SATZ(KM-POS2:)
                      IF KM-KZ = 1 and KM-ZAHL = 0 and KM-BET = 0
                          MOVE SPACE TO DRA-SATZ
                          GO Z
               WHEN 4
               WHEN 5
               WHEN 6
               WHEN 7 PERFORM BET-AUF
                      MOVE WD-BET TO DRA-SATZ(KM-POS1:)
               WHEN 8 IF KM-ZAHL = 0 and KM-BET = 0 GO Z
                      end-if
                      MOVE KM-ZAHL TO WD-ZAHL
                      IF KM-ZEIL > 24 MOVE KM-ZAHL TO WD-SZAHL
                      end-if
                      MOVE WD-ZAHL TO DRA-SATZ(KM-POS1:)
                      COMPUTE WH-WERT = KM-BET * -1
                      PERFORM BET-AUF
                      IF WH-WERT < 0
                        PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
                          IF WD-BET(WZ:1) not = " " ADD -1 WZ GIVING WX
                             MOVE "-" TO WD-BET(WX:1)
                             MOVE 7 TO WZ
                          end-if
                      end-if
                      MOVE WD-BET TO DRA-SATZ(KM-POS2:)
               WHEN 39
               WHEN 9 PERFORM DRUDATE
                      MOVE WD-DZ TO DRA-SATZ.
       Q.  IF KM-KZ not = 0 ADD 1 TO WP
               MOVE DRA-SATZ TO WM-DRUBUF(WP)
               IF KM-ART = 0 MOVE 1 TO WM-DATA(WP)
               else MOVE 2 TO WM-DATA(WP)
               end-if
               MOVE SPACE TO DRA-SATZ
               GO Z.
           IF KM-KZ > 0 and WP > 0 MOVE 2 TO WM-DATA(WP).
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       DRU-BUF SECTION.
       A.  PERFORM VARYING WP FROM 1 BY 1 UNTIL WP > 10
               or WM-DATA(WP) = 2 CONTINUE.
           IF WP > 10 GO Q.
           PERFORM VARYING WP FROM 1 BY 1 UNTIL WP > 10
               IF WM-DATA(WP) not = 0 MOVE WM-DRUBUF(WP) TO DRA-SATZ
                   PERFORM DRUCK.
       Q.  INITIALIZE WT-DRUTAB.
           MOVE 0 TO WP.
       Z.  EXIT.
      ******************************************** Betragsaufbereitung *
       BET-AUF SECTION.
       A.  EVALUATE WK-ART
               WHEN 0 MOVE KM-BET TO WD-BET
               WHEN 1 MOVE KM-BET TO WD-BETP
                      MOVE " " TO WD-BETP(4:1)
                      MOVE WD-BETP(1:) TO WD-BET(1:)
               WHEN 2 MOVE KM-BET TO WD-BETP
                      MOVE WD-BETP(1:) TO WD-BET(1:).
           IF WD-BETP(1:1) not = "-" GO Z.
           IF WD-BETP(1:2) = "- " MOVE " -" TO WD-BETP(1:2).
           IF WD-BETP(2:2) = "- " MOVE " -" TO WD-BETP(2:2).
           IF WD-BETP(3:2) = "- " MOVE " -" TO WD-BETP(3:2).
           IF WD-BETP(4:2) = "- " MOVE " -" TO WD-BETP(4:2).
           IF WD-BETP(5:2) = "- " MOVE " -" TO WD-BETP(5:2).
       Z.  EXIT.
      *********************************************** Druckdatum /Zeit *
       DRUDATE SECTION.
       A.  IF KM-DATUM = 0 MOVE WM-DATUM TO KM-DATUM.
           MOVE KM-DATUM TO WZ-DATUM WC-DATUM.
           MOVE WZ-TAG TO WD-TT.
           MOVE WZ-MONAT TO WD-MM.
           COMPUTE WD-JJ = WZ-JAHR + 2000.
           MOVE "." TO WD-DATUM(3:1) WD-DATUM(6:1).
           MOVE KM-ZEIT TO WD-ZEIT.
           MOVE ":" TO WD-ZEIT(3:1) WD-ZEIT(6:1).
           MOVE SPACE TO WD-DZ.
           MOVE WD-ZEIT TO WD-DZ(12:5).
           IF WD-ZEIT(1:1) = " " MOVE WD-DATUM TO WD-DZ(2:10)
           else MOVE WD-DATUM TO WD-DZ(1:11).
           EVALUATE KM-ART
               WHEN > 10 and < 20 PERFORM VARIANTE-1
               WHEN > 20 and < 30 PERFORM VARIANTE-2
               WHEN > 38 and < 40 PERFORM VARIANTE-3 .
       Z.  EXIT.
      *********************************************** Datumsvariante 2 *
       VARIANTE-1 SECTION.
       A.  MOVE SPACE TO WH-MCODE.
           MOVE WD-DZ(1:6) TO WH-MCODE(3:6).
           MOVE WH-MCODE(1:8) TO WD-DZ(1:8).
       Z.  EXIT.
      *********************************************** Datumsvariante 1 *
       VARIANTE-2 SECTION.
       A.  MOVE SPACE TO WH-MCODE.
           IF WD-DZ(4:1) = "0" MOVE WD-DZ(1:3) TO WH-MCODE(2:3)
               MOVE WH-MCODE(1:4) TO WD-DZ(1:4).
       Z.  EXIT.
      *********************************************** Datumsvariante 3 *
       VARIANTE-3 SECTION.
       A.  PERFORM VARIANTE-1.
           IF WD-DZ(3:1) = SPACE MOVE WD-DZ(4:) TO WD-DZ(3:).
           MOVE WD-DZ(3:8) TO WD-DZ(1:10).
       Z.  EXIT.
      ************************************************* Zahl u. Betrag *
       ZER-ZABET SECTION.
       A.  MOVE 7 TO WF.
           MOVE 0 TO WH-NEG WD-KZ.
           PERFORM VARYING WZ FROM WZ BY -1 UNTIL WZ = 1
               EVALUATE EXC-ASATZ(WZ:1)
                   WHEN ";" MOVE WZ TO WY
                            MOVE 2 TO WZ
                   WHEN ":" MOVE 2 TO WZ
                   WHEN "-" MOVE 2 TO WH-NEG
                   WHEN "."
                   WHEN "," MOVE 1 TO WD-KZ
                   WHEN OTHER
                        MOVE EXC-ASATZ(WZ:1) TO WM-EXBET(WF:1)
                        ADD -1 TO WF.
           IF WH-NEG = 1 COMPUTE WM-EXBET = WM-EXBET * -1.
           MOVE 8 TO WF.
           ADD -1 TO WY.
           MOVE 0 TO WH-NEG WD-KZ.
           PERFORM VARYING WZ FROM WY BY -1 UNTIL WZ = 1
               EVALUATE EXC-ASATZ(WZ:1)
                   WHEN ";"
                   WHEN ":" MOVE 2 TO WZ
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN "."
                   WHEN "," MOVE 1 TO WD-KZ
                   WHEN OTHER
                        MOVE EXC-ASATZ(WZ:1) TO WM-EXZAHL(WF:1)
                        ADD -1 TO WF.
           IF WH-NEG = 1 COMPUTE WM-EXZAHL = WM-EXZAHL * -1.
           IF WD-KZ = 0 COMPUTE WM-EXZAHL = WM-EXZAHL * 1000.
       Z.  EXIT.
      ************************************************** Zeit zerlegen *
       ZER-ZEIT SECTION.
       A.  MOVE 4 TO WF.
           IF EXC-ASATZ(WZ:1) = ";" ADD -1 TO WZ.
           PERFORM VARYING WZ FROM WZ BY -1 UNTIL WZ = 1
               EVALUATE EXC-ASATZ(WZ:1)
                   WHEN ";" MOVE 2 TO WZ
                   WHEN ":" MOVE 0 TO WH-NUM
                   WHEN OTHER
                        MOVE EXC-ASATZ(WZ:1) TO WM-EXZEIT(WF:1)
                        ADD -1 TO WF.
       Z.  EXIT.
      ************************************************** Zahl zerlegen *
       ZER-ZAHL SECTION.
       A.  MOVE 8 TO WF.
           MOVE 0 TO WH-NEG WD-KZ.
           IF EXC-ASATZ(WZ:1) = ";" ADD -1 TO WZ.
           PERFORM VARYING WZ FROM WZ BY -1 UNTIL WZ = 1
               EVALUATE EXC-ASATZ(WZ:1)
                   WHEN ";"
                   WHEN ":" MOVE 2 TO WZ
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN "."
                   WHEN "," MOVE 1 TO WD-KZ
                   WHEN OTHER
                        MOVE EXC-ASATZ(WZ:1) TO WM-EXZAHL(WF:1)
                        ADD -1 TO WF.
           IF WH-NEG = 1 COMPUTE WM-EXZAHL = WM-EXZAHL * -1.
           IF WD-KZ = 0 COMPUTE WM-EXZAHL = WM-EXZAHL * 1000.
       Z.  EXIT.
      ************************************************ Betrag zerlegen *
       ZER-BET SECTION.
       A.  MOVE 7 TO WF.
           MOVE 0 TO WH-NEG.
           PERFORM VARYING WZ FROM WZ BY -1 UNTIL WZ = 1
               EVALUATE EXC-ASATZ(WZ:1)
                   WHEN ";"
                   WHEN ":" MOVE 2 TO WZ
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN "."
                   WHEN "," MOVE 1 TO WD-KZ
                   WHEN OTHER
                        MOVE EXC-ASATZ(WZ:1) TO WM-EXBET(WF:1)
                        ADD -1 TO WF.
           IF WH-NEG = 1 COMPUTE WM-EXBET = WM-EXBET * -1.
       Z.  EXIT.
      ******************************************** Import Exceldateien *
       EX-IMPORT SECTION.
       A.  PERFORM CSV-SUCH.
           MOVE 270510124000014 TO WL-REG.
           ADD 2 WY GIVING WD-ZL.
           MOVE WD-ZL TO WL-REG(7:2).
           IF WY = 0 MOVE "03" TO WL-REG(7:2).
           CALL "CAUP" USING WL-REG WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Import aus Excel " with highlight AT VDU-LP.
           IF WY = 0 ADD 203 VDU-ECK GIVING VDU-LP
               DISPLAY "keine Exceldaten vorhanden" AT VDU-LP
               GO E.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
              IF WT-DATNAM(WX) not = SPACE
                  COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK
                  DISPLAY WT-DATNAM(WX) with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF WY = 0 GO X.
           IF ESC GO X.
           IF not RET GO E.
           PERFORM EINLESEN.
           MOVE 0 TO WI.
       G.  IF WI = 24 GO Q.
           ADD 1 TO WI.
           MOVE 1 TO WE.
           MOVE WT-CSV(WI) TO EXC-ASATZ.
           IF EXC-ASATZ = SPACE or LOW-VALUES GO G.
           DISPLAY EXC-ASATZ AT 2520.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 30
                or EXC-ASATZ(WX:1) = ":" or ";" end-perform.
           MOVE EXC-ASATZ(1:WX) TO WV-EXTEXT.
       I.  PERFORM VARYING WE FROM WE BY 1 UNTIL WE > 99
               or WKM-EXTEXT(WE) = WV-EXTEXT end-perform
           IF WKM-EXTEXT(WE) = SPACE GO G.
           IF WE = 100 GO G.
           IF EXC-ASATZ(1:WX) = ";" MOVE " " TO WV-EXTEXT(WX:1).
           MOVE 0 TO WM-EXBET WM-EXZAHL WM-EXZEIT.
           PERFORM VARYING WZ FROM 64 BY -1 UNTIL WZ = 1
               or EXC-ASATZ(WZ:1) not = " " end-perform.
           EVALUATE WKM-ART(WE)
               WHEN 0 NEXT SENTENCE
               WHEN 1 PERFORM ZER-ZEIT               *> Zeit
                      MOVE WM-EXZEIT TO WKM-ZEIT(WE)
               WHEN 2 PERFORM ZER-ZAHL               *> Zahl
                      MOVE WM-EXZAHL TO WKM-ZAHL(WE)
               WHEN 3 PERFORM ZER-ZABET              *> Zahl u. Betrag
                      MOVE WM-EXZAHL TO WKM-ZAHL(WE)
                      MOVE WM-EXBET TO WKM-BET(WE)
               WHEN 4 PERFORM ZER-BET                *> Betrag
                      MOVE WM-EXBET TO WKM-BET(WE)
               WHEN 5                                *> Ust-Betrag
               WHEN 6 PERFORM ZER-BET                *> Ust-Betrag
                      MOVE WM-EXBET TO WKM-BET(WE)
                          COMPUTE WKM-BET(WE - 1) =
                                  WKM-BET(WE + 1) - WKM-BET(WE)
               WHEN 7 PERFORM ZER-BET                *> Summe
                      MOVE WM-EXBET TO WKM-BET(WE)
               WHEN 8 PERFORM ZER-ZABET              *> Zahl u. -Betrag
                      MOVE WM-EXZAHL TO WKM-ZAHL(WE)
                      MOVE WM-EXBET TO WKM-BET(WE).
           ADD 1 TO WE.
           GO I.
       Q.  PERFORM ABSTELL.

       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* CSV-Dateien suchen *
       CSV-SUCH SECTION.
       A.  MOVE 0 TO WY WE.
           MOVE SPACE TO WT-DTN.
           MOVE LOW-VALUES TO WT-CSDAT.
           MOVE WS-DATUM TO WC-DATUM.             *> wg. KassaÅbernahme
           PERFORM DATDREH.
           INSPECT VDU-DATUM REPLACING ALL " " BY "0".
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE LOW-VALUES TO WT-WKDAT(WX).
           MOVE WS-DATUM TO WZ-DATUM.
           MOVE "BERI\JJMMTT.CSV" TO WL-FILEIN.
           MOVE WZ-DATUM TO WL-FILEIN(6:6).
           MOVE 0 TO WL-ACT WL-ATTRIB.
       C.  CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0 GO X.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           ADD 1 TO WY.
           MOVE WL-FILEIN TO WH-FIBNAM.
      *    MOVE WL-FOUT TO WH-FIBNAM(10:).
           MOVE WH-FIBNAM TO WT-DATNAM(WY).
           OPEN INPUT EXCELIS.
       E.  READ EXCELIS AT END CLOSE EXCELIS GO C.
           ADD 1 TO WE.
           IF EXC-ASATZ = SPACE GO E.
           INSPECT EXC-ASATZ REPLACING ALL "ƒ" by "é" ALL "‹" by "ö"
                       ALL "÷" by "ô" ALL "‰" by "Ñ"
                       ALL "¸" by "Å" ALL "ˆ" by "î" ALL "ﬂ" by "·".
           MOVE EXC-ASATZ TO WT-CSV(WE).
           IF EXC-ASATZ(1:6) = "Kassa:" MOVE EXC-ASATZ(8:1) TO WH-NUM
               MOVE WH-NUM TO WK.
      *        IF WK not = WM-KNR MOVE 0 TO WY
      *            GO Z.
           IF EXC-ASATZ(1:6) = "Datum:"
               IF EXC-ASATZ(9:) not = VDU-DATUM(1:) MOVE 0 TO WY
                   GO Z
               else MOVE WT-CSDAT TO WT-WKDAT(WK)
                    INITIALIZE WT-CSDAT
                    MOVE 0 TO WE.
           GO E.
       X.  MOVE WT-WKDAT(WM-KNR) TO WT-CSDAT.
       Z.  EXIT.
      ******************************************** Import Exceldateien *
       EXIMPORT SECTION.
       A.
      *    PERFORM EXP-SUCH.
           MOVE 270510124000014 TO WL-REG.
           ADD 2 WY GIVING WD-ZL.
           MOVE WD-ZL TO WL-REG(7:2).
           IF WY = 0 MOVE "03" TO WL-REG(7:2).
           CALL "CAUP" USING WL-REG WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Import aus Excel " with highlight AT VDU-LP.
           IF WY = 0 ADD 203 VDU-ECK GIVING VDU-LP
               DISPLAY "keine Exceldaten vorhanden" AT VDU-LP
               GO E.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
              IF WT-DATNAM(WX) not = SPACE
                  COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK
                  DISPLAY WT-DATNAM(WX) with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF WY = 0 GO X.
           IF ESC GO X.
           IF not RET GO E.
           PERFORM EINLESEN.
           MOVE 0 TO WI.
       G.  IF WI = 24 GO Q.
           ADD 1 TO WI.
           MOVE 1 TO WE.
           MOVE WT-CSV(WI) TO EXC-ASATZ.
           IF EXC-ASATZ = SPACE or LOW-VALUES GO G.
           DISPLAY EXC-ASATZ AT 2520.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 30
                or EXC-ASATZ(WX:1) = ":" or ";" end-perform.
           MOVE EXC-ASATZ(1:WX) TO WV-EXTEXT.
       I.  PERFORM VARYING WE FROM WE BY 1 UNTIL WE > 99
               or WKM-EXTEXT(WE) = WV-EXTEXT end-perform
           IF WKM-EXTEXT(WE) = SPACE GO G.
           IF WE = 100 GO G.
           IF EXC-ASATZ(1:WX) = ";" MOVE " " TO WV-EXTEXT(WX:1).
           MOVE 0 TO WM-EXBET WM-EXZAHL WM-EXZEIT.
           PERFORM VARYING WZ FROM 64 BY -1 UNTIL WZ = 1
               or EXC-ASATZ(WZ:1) not = " " end-perform.
           EVALUATE WKM-ART(WE)
               WHEN 0 NEXT SENTENCE
               WHEN 1 PERFORM ZER-ZEIT               *> Zeit
                      MOVE WM-EXZEIT TO WKM-ZEIT(WE)
               WHEN 2 PERFORM ZER-ZAHL               *> Zahl
                      MOVE WM-EXZAHL TO WKM-ZAHL(WE)
               WHEN 3 PERFORM ZER-ZABET              *> Zahl u. Betrag
                      MOVE WM-EXZAHL TO WKM-ZAHL(WE)
                      MOVE WM-EXBET TO WKM-BET(WE)
               WHEN 4 PERFORM ZER-BET                *> Betrag
                      MOVE WM-EXBET TO WKM-BET(WE)
               WHEN 5                                *> Ust-Betrag
               WHEN 6 PERFORM ZER-BET                *> Ust-Betrag
                      MOVE WM-EXBET TO WKM-BET(WE)
                          COMPUTE WKM-BET(WE - 1) =
                                  WKM-BET(WE + 1) - WKM-BET(WE)
               WHEN 7 PERFORM ZER-BET                *> Summe
                      MOVE WM-EXBET TO WKM-BET(WE)
               WHEN 8 PERFORM ZER-ZABET              *> Zahl u. -Betrag
                      MOVE WM-EXZAHL TO WKM-ZAHL(WE)
                      MOVE WM-EXBET TO WKM-BET(WE).
           ADD 1 TO WE.
           GO I.
       Q.  PERFORM ABSTELL.

       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************************** Export-Dateien suchen *
       EXP-SUCH SECTION.
       A.  MOVE 0 TO WY WE.
           MOVE "net use k: \\SERVER\CSRV >NULL" TO WD-UPON.
           DISPLAY WD-UPON UPON COMMAND-LINE.
           DISPLAY LOW-VALUES AT 1801.
           CALL X"91" USING RESULT FUNKT PARAM.
           CALL "CAUP" USING "1318012580" WH-CREG.
           PERFORM AUFTEIL.
           MOVE "BERKA.*" TO WL-FILEIN.
           MOVE 0 TO WL-ACT WL-ATTRIB.
       C.  INITIALIZE WT-BERTAB.
           CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0 DISPLAY "Kassa NICHT vorhanden!" AT 2401
               GO X.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           ADD 1 TO WY.
           MOVE WL-FOUT TO WH-FIBNAM.
           OPEN INPUT EXCELIS.
           MOVE "TEST.DAT" TO WN-EXPO.
           OPEN OUTPUT EXPODAT.
           MOVE KM-BET TO WM-UMSN.
           COMPUTE WM-UST = KM-BET * ,10.
           ADD WM-UMSN WM-UST GIVING WM-UMSB.
       E.  READ EXCELIS AT END GO K.
           EVALUATE EXC-SATZ(1:9)
               WHEN "0,0,1,1;1" INSPECT EXC-SATZ REPLACING ALL ";24,A;"
                                    BY ";24,R;"
               WHEN "6,1,823,C" MOVE 07 TO WQ
                                PERFORM ZERLEG           *> Storno 2
               WHEN "6,1,206,C" MOVE 08 TO WQ
                                PERFORM ZERLEG           *> Korrekturen
               WHEN "6,1,150,C" MOVE 09 TO WQ
                                PERFORM ZERLEG           *> man.Rechnung
               WHEN "6,1,582,C" MOVE 10 TO WQ
                                PERFORM ZERLEG           *> Z-ZÑhler
               WHEN "6,1,1104," MOVE 11 TO WQ
                                PERFORM ZERLEG           *> GT-BonzÑhl.
               WHEN "6,1,1105," MOVE 12 TO WQ
                                PERFORM ZERLEG           *> GT-Re.-ZÑhl.
               WHEN "6,1,944,D" MOVE 13 TO WQ
                                PERFORM DA-ZEI           *> erste Buchg.
               WHEN "6,1,945,D" MOVE 14 TO WQ
                                PERFORM DA-ZEI           *> letzte Buch.
               WHEN "6,1,4,CA:" MOVE 15 TO WQ
                                PERFORM ZERLEG           *> GU gesamt
                                PERFORM NUOVO
               WHEN "6,1,1039," MOVE 16 TO WQ
                                PERFORM ZERLEG           *> verworf. Bon
               WHEN "6,1,138,C" MOVE 17 TO WQ
                                PERFORM ZERLEG           *> Kunden
               WHEN "6,1,144,C" MOVE 18 TO WQ
                                PERFORM ZERLEG           *> kein Verkf.
               WHEN "6,1,321,C" MOVE 19 TO WQ
                                PERFORM ZERLEG           *> Kund/Umsatz
                                PERFORM NUOVO
               WHEN "6,1,445,C" MOVE 20 TO WQ
                                PERFORM ZERLEG           *> Umsatz Soll
                                PERFORM NUOVO
               WHEN "6,1,828,C" MOVE 21 TO WQ
                                PERFORM ZERLEG           *> Kd/Halbpr.1
               WHEN "6,1,829,C" MOVE 22 TO WQ
                                PERFORM ZERLEG           *> Kd/Halbpr.2
               WHEN "6,1,830,C" MOVE 23 TO WQ
                                PERFORM ZERLEG           *> Kd/Persrab.
               WHEN "6,1,301,C" MOVE 24 TO WQ
                                PERFORM ZERLEG           *> Ums. Zahlung
                                PERFORM NUOVO
               WHEN "6,1,80,CA" MOVE 25 TO WQ
                                PERFORM ZERLEG           *> Brutto 10%
                                PERFORM NUOVO
               WHEN "6,1,89,CA" MOVE 26 TO WQ
                                PERFORM ZERLEG           *> Ust 10%
                                PERFORM U-NUOVO
               WHEN "6,1,81,CA" MOVE 27 TO WQ
                                PERFORM ZERLEG           *> Brutto 20%
               WHEN "6,1,90,CA" MOVE 28 TO WQ
                                PERFORM ZERLEG           *> Ust 20%
               WHEN "2,11,1,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> BÑckerei WG
                                PERFORM NUOVO
               WHEN "2,12,1,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Konditor WG
               WHEN "2,13,1,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Milchpro WG
               WHEN "2,14,1,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Handel   WG
               WHEN "2,15,1,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> KÅche    WG
      *----------------------------------------------------> Bediener <-
               WHEN "4,1,4,CA:" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> GU gesamt
                                PERFORM NUOVO
               WHEN "4,1,1039," MOVE 50 TO WQ
                                PERFORM ZERLEG           *> verworf. Bon
               WHEN "4,1,138,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Kunden
               WHEN "4,1,144,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> kein Verkf.
               WHEN "4,1,321,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Kund/Umsatz
                                PERFORM NUOVO
               WHEN "4,1,445,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Umsatz Soll
                                PERFORM NUOVO
               WHEN "4,1,828,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Kd/Halbpr.1
               WHEN "4,1,829,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Kd/Halbpr.2
               WHEN "4,1,830,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Kd/Persrab.
               WHEN "4,1,301,C" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Ums. Zahlung
                                PERFORM NUOVO
               WHEN "4,1,80,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Brutto 10%
                                PERFORM NUOVO
               WHEN "4,1,89,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Ust 10%
                                PERFORM U-NUOVO
               WHEN "4,1,81,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Brutto 20%
               WHEN "4,1,90,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> Ust 20%
      *----------------------------------------------> Artikelbericht <-
               WHEN "1,1,1,CA:" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> WGR 1 - 11
                                PERFORM NUOVO
               WHEN "1,2,1,CA:" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> WGR 1 - 11
               WHEN "1,3,1,CA:" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> WGR 1 - 11
               WHEN "1,4,1,CA:" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> WGR 1 - 11
               WHEN "1,5,1,CA:" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> WGR 1 - 11
               WHEN "1,10,1,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> WGR 1 - 11
               WHEN "1,11,1,CA" MOVE 50 TO WQ
                                PERFORM ZERLEG           *> WGR 1 - 11
               WHEN OTHER NEXT SENTENCE.
           MOVE EXC-SATZ TO EXP-SATZ.
           WRITE EXP-SATZ.
           IF EXC-SATZ(1:3) = "10," UNSTRING EXC-SATZ DELIMITED BY ","
               INTO WX-A WX-B WX-C WX-D WX-E
               IF WX-C = 11003 MOVE EXC-SATZ TO WT-TX.
           GO E.
       K.  MOVE 99 TO WQ.
           MOVE WT-TX TO EXC-SATZ.
           PERFORM DA-ZEI.
           CLOSE EXCELIS.
           CLOSE EXPODAT.
       Q.  PERFORM LADEN.
           GO C.
       X.  MOVE "net use K: \d >NULL" TO WD-UPON.
           DISPLAY WD-UPON UPON COMMAND-LINE.
           DISPLAY LOW-VALUES AT 2201.
           CALL X"91" USING RESULT FUNKT PARAM.
           CALL "CAUP" USING "1318012580" WH-CREG.
       Z.  EXIT.
      *********************************** Zerlegen von Zahl und Betrag *
       ZERLEG SECTION.
       A.  MOVE 0 TO WH-NEG WD-KZ WX-ZER.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL EXC-SATZ(WX:1) = ":"
                 CONTINUE.
           ADD 1 TO WX.
           PERFORM VARYING WX FROM WX BY 1 UNTIL WX > 30
             IF EXC-SATZ(WX:1) not = SPACE
               EVALUATE EXC-SATZ(WX:1)
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN "."
                   WHEN "," MOVE 1 TO WD-KZ
                   WHEN "/" MOVE WX-ZER TO WX-NUM
                            IF WH-NEG = 1 COMPUTE WX-NUM = WX-NUM * -1
                            end-if
                            MOVE 0 TO WX-ZER WH-NEG
                            MOVE 7 TO WZ
                   WHEN OTHER
                      IF WD-KZ = 0 COMPUTE WX-ZER = WX-ZER * 10
                          MOVE EXC-SATZ(WX:1) TO WX-ZER(6:1)
                      else MOVE EXC-SATZ(WX:1) TO WX-ZER(WZ:1)
                              ADD 1 TO WZ.
           COMPUTE WX-BET ROUNDED = WX-ZER.
           IF WH-NEG = 1 COMPUTE WX-BET = WX-BET * -1.
           IF WQ > 49 GO Z.
           MOVE WX-NUM TO WKM-ZAHL(WQ).
           MOVE WX-BET TO WKM-BET(WQ).
       Z.  EXIT.
      ******************************************************************
       NUOVO SECTION.
       A.  IF WD-KZ = 0 COMPUTE WX-ZER = WX-ZER - WM-UMSB
                        COMPUTE WX-BET = WX-BET - WM-UMSB
                   else COMPUTE WX-ZER = WX-ZER - WM-UMSN
                        COMPUTE WX-BET = WX-BET - WM-UMSN.   *> BMG-UST
           PERFORM VARYING WX FROM 1 BY 1 UNTIL EXC-SATZ(WX:1) = ":"
                 CONTINUE.
           MOVE 1 TO WY.
           MOVE WX-NUM TO WD-XNUM.
           IF WH-NEG = 1 COMPUTE WD-XBER = WX-ZER * -1
                    else COMPUTE WD-XBER = WX-ZER.
           MOVE "." TO WD-XBER(8:1).
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 7
               IF WD-XNUM(WY:1) not = SPACE
                   ADD 1 TO WX
                   MOVE WD-XNUM(WY:1) TO EXC-SATZ(WX:1).
           ADD 1 TO WX.
           MOVE "/" TO EXC-SATZ(WX:1).
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 10
               IF WD-XBER(WY:1) not = SPACE
                   ADD 1 TO WX
                   MOVE WD-XBER(WY:1) TO EXC-SATZ(WX:1).
           MOVE 0 TO WD-KZ.
           MOVE WX-NUM TO WKM-ZAHL(WQ).
           MOVE WX-BET TO WKM-BET(WQ).
       Z.  EXIT.
      ******************************************************************
       U-NUOVO SECTION.
       A.  COMPUTE WX-ZER = WX-ZER - WM-UST.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL EXC-SATZ(WX:1) = ":"
                 CONTINUE.
           MOVE 1 TO WY.
           MOVE WX-NUM TO WD-XNUM.
           IF WH-NEG = 1 COMPUTE WD-XUST = WX-ZER * -1
                    else COMPUTE WD-XUST = WX-ZER.
           MOVE "." TO WD-XUST(8:1).
           PERFORM VARYING WZ FROM 20 BY -1 UNTIL WZ < 12
               IF WD-XUST(WZ:1) = "0" MOVE SPACE TO WD-XUST(WZ:1)
               else MOVE 10 TO WZ.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 7
               IF WD-XNUM(WY:1) not = SPACE
                   ADD 1 TO WX
                   MOVE WD-XNUM(WY:1) TO EXC-SATZ(WX:1).
           ADD 1 TO WX.
           MOVE "/" TO EXC-SATZ(WX:1).
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 15
               IF WD-XUST(WY:1) not = SPACE
                   ADD 1 TO WX
                   MOVE WD-XUST(WY:1) TO EXC-SATZ(WX:1).
           MOVE 0 TO WD-KZ.
           MOVE WX-NUM TO WKM-ZAHL(WQ).
           MOVE WX-BET TO WKM-BET(WQ).
       Z.  EXIT.
      ******************************************************************
       DA-ZEI SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL EXC-SATZ(WX:1) = ":"
                 CONTINUE.
           ADD 1 TO WX.
           MOVE EXC-SATZ(WX:2) TO WZ-TAG.
           ADD 3 TO WX.
           MOVE EXC-SATZ(WX:2) TO WZ-MONAT.
           ADD 3 TO WX.
           MOVE EXC-SATZ(WX:2) TO WZ-JAHR.
           ADD 3 TO WX.
           MOVE EXC-SATZ(WX:2) TO WX-ZEIT(1:2).
           ADD 3 TO WX.
           MOVE EXC-SATZ(WX:2) TO WX-ZEIT(3:2).
           ADD 3 TO WX.
           MOVE EXC-SATZ(WX:2) TO WX-ZEIT(5:2).
           IF WQ = 99 MOVE WX-ZEIT(1:2) TO WH-STD
               MOVE WX-ZEIT(3:2) TO WH-MIN
               ADD 11 TO WH-MIN
               IF WH-MIN > 59 ADD -60 TO WH-MIN
                   ADD 1 TO WH-STD
               end-if
               MOVE WH-STD TO WX-ZEIT(1:2)
               MOVE WH-MIN TO WX-ZEIT(3:2).
           MOVE WX-ZEIT TO WKM-ZEIT(WQ).
           MOVE WZ-DATUM TO WKM-DATUM(WQ).
       Z.  EXIT.
      ****************************************** Werte fÅr Druck laden *
       LADEN SECTION.
       A.  INITIALIZE KM-SATZ.
           MOVE 1 TO KM-KNR.
           START KERMODEL KEY > KM-KEY INVALID CLOSE DRUCKER GO Z.
       C.  READ KERMODEL NEXT AT END GO G.
           MOVE 0 TO KM-ZAHL KM-BET KM-ZEIT KM-DATUM.
           IF KM-KNR not = 1 GO G.
           EVALUATE KM-EXPO
               WHEN 80 COMPUTE KM-BET = WKM-BET(25) - WKM-BET(26)
               WHEN 81 COMPUTE KM-BET = WKM-BET(27) - WKM-BET(28)
               WHEN 98 PERFORM VOR-TAG                  *> letzte Nul
               WHEN 99 MOVE WKM-DATUM(KM-EXPO) TO KM-DATUM
                       MOVE WKM-ZEIT(KM-EXPO) TO KM-ZEIT
               WHEN > 0 MOVE WKM-ZAHL(KM-EXPO) TO KM-ZAHL
                        MOVE WKM-BET(KM-EXPO) TO KM-BET
                        MOVE WKM-ZEIT(KM-EXPO) TO KM-ZEIT
                        MOVE WKM-DATUM(KM-EXPO) TO KM-DATUM.
           REWRITE KM-SATZ.
           GO C.
       G.  PERFORM BONDRUCK.
       M.  DISPLAY "<esc>= Abbruch, <Ende>= alles ok! < >" AT 2301.
           CALL "CAUP" USING "0023360000" WH-CREG.
           IF ESC GOBACK.
           IF not ENDE GO M.
           CLOSE DRUCKER.
           DELETE FILE DRUCKER.
       Q.  MOVE 0 TO WF-SIZE.
           MOVE "K:\VECTRON\VCOM6\IMP\VCOM.IMP " TO WH-FNAME.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           IF WF-SIZE not = 0 DISPLAY "bitte warten! - alte Datei vorhan
      -        "den - geht von selbst weiter! " AT 2301
               ACCEPT WD-KZ AT 2440 TIMEOUT AFTER +30 with NO-ECHO
                   SIZE 1 AUTO-SKIP NOT ON EXCEPTION CONTINUE
               end-accept
               CALL "CAUP" USING "1320012580000" WH-CREG
               GO Q.
           CALL "CBL_COPY_FILE" USING
               "TEST.DAT " "K:\VECTRON\VCOM6\IMP\VCOM.IMP ".
           CALL "CBL_COPY_FILE" USING "REBUILD.EXE " WH-FIBNAM.
           CALL "CBL_DELETE_FILE" USING WH-FIBNAM.
           DELETE FILE EXCELIS.
           DELETE FILE EXPODAT.
       Z.  EXIT.
      ******************************************************************
       VOR-TAG SECTION.
       A.  MOVE WKM-DATUM(14) TO WC-DATUM WA-TAG.
           ADD 20000000 TO WA-TAG.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WO-TGN = 1
               COMPUTE WA-TAG = FUNCTION INTEGER-OF-DATE(WA-TAG)
               ADD -2 TO WA-TAG
               COMPUTE WA-TAG = FUNCTION DATE-OF-INTEGER(WA-TAG).
           MOVE WA-TAG TO KM-DATUM.
           MOVE 193100 TO KM-ZEIT.
       Z.  EXIT.
      ************************************* aufteilen auf mehrere Tage *
       AUFTEIL SECTION.
       A.  CALL "CBL_COPY_FILE" USING "K:\BERKA " "C:\BERSI ".
           MOVE "BERKA." TO WN-EXPO.
           MOVE "C:\BERSI" TO WH-FIBNAM.
           MOVE "000" TO WN-EXNR.
           OPEN INPUT EXCELIS.
       E.  ADD 1 TO WN-EXNR.
           OPEN INPUT EXPODAT.
           IF WF-STATUS not = "35" CLOSE EXPODAT GO E.
           OPEN OUTPUT EXPODAT.
           IF WX = 1 GO H.
           MOVE 0 TO WX.
       G.  READ EXCELIS AT END GO X.
           IF EXC-SATZ(1:12) = "100,1,1,1;10" GO I.
       H.  MOVE EXC-SATZ TO EXP-SATZ.
           WRITE EXP-SATZ.
           GO G.
       I.  IF WX = 0 ADD 1 TO WX GO H.
           CLOSE EXPODAT.
           GO E.
       X.  CLOSE EXPODAT.
           CLOSE EXCELIS.
           CALL "CBL_DELETE_FILE" USING "K:\BERKA ".
       Z.  EXIT.
