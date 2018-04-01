      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PAKOERF.
      *********************** Buchungsprogramm und WaagedatenÅbernahme *
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY PAKOSEC.CPY.
       COPY PARSEART.CPY.
       COPY PAKSEWAG.CPY.
       COPY PARSEREZ.CPY.
       COPY PAKSESRE.CPY.
       COPY PARSELFT.CPY.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PAKOFD.CPY.
       COPY PARFDART.CPY.
       COPY PARREZEP.CPY.
       COPY PAKFDWAG.CPY.
       COPY PAKFDSRE.CPY.
       COPY PARFDLFT.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(5).
           03  DRA-STR.
               05 DRA-BEZ              PIC X(15).
               05 DRA-STERN            PIC X.
               05 DRA-SGEW             PIC ZZZ9,999.
               03 DRA-IGEW             PIC ZZZ9,999.
           03  FILLER                  PIC X(80).
       01  DRS-SATZ.
           03  FILLER                  PIC X(5).
           03  DRS-STR.
               05 DRS-ARTNR            PIC ZZ.ZZ9-.
               05 DRS-BEZ              PIC X(26).
               05 DRS-MENGE            PIC X(11).
               05 FILLER               PIC X.
               05 DRS-MEH              PIC XXXX.
               05 DRS-PREIS            PIC ZZZ.ZZ9,99-.
               05 DRS-RAB              PIC ZZZ,ZZ-      OCCURS 2.
               05 DRS-BETRAG           PIC ZZZZ.ZZ9,99-.
       01  PRA-SATZ.
           03  PRA-STR.
               05 PRA-BEZ              PIC X(25).
               05 PRA-STERN            PIC X.
               05 PRA-GEW              PIC ZZZ9.999.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WH-PX                   PIC XX       OCCURS 2.
           03  WM-OPEN                 PIC 9        COMP   VALUE ZERO.
           03  WX-PRNO                 PIC 99       COMP-X.
           03  WX-PRSTAT               PIC 99       COMP-X.
           03  WH-DRUNAM               PIC X(12)  VALUE "LPT1:".
           03  WZ-SEITE                PIC 99       COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99       COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99       COMP   VALUE ZERO.
           03  VDU-AB                  PIC 9999.
           03  WB                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  WU                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  IY                      PIC 99        COMP.
           03  IZ                      PIC 99        COMP.
           03  RZ                      PIC 99        COMP.
           03  WM-G                    PIC 99        COMP.
           03  WM-L                    PIC 99        COMP.
           03  WM-M                    PIC 99        COMP.
           03  WM-EIN                  PIC 99        COMP.
           03  WM-WAAG                 PIC 99        COMP.
           03  WM-GB                   PIC 99        COMP.
             88 OHNE      VALUE 0.   88 EINZEL     VALUE 1.
             88 SAMMEL    VALUE 2.   88 AUFTEIL    VALUE 4.
           03  WX-J                    PIC 9999      COMP.
           03  WT-JOTAB.                        *> Journalzeilentabelle
               05 WT-JKEY              PIC X(12)     OCCURS 17.
           03  WT-ERFTAB.                       *> Buchungszeilentabelle
               05 WT-KEY               PIC X(7)      OCCURS 17.
           03  WH-BZSATZ               PIC X(64).
           03  WH-SRSATZ               PIC X(32).
           03  WH-TEIL                 PIC S999V9(7) COMP.
           03  WK-UST                  PIC 99        COMP.
           03  WH-UST                  PIC S99V99    COMP.
           03  WC-UST                  PIC S99V9999  COMP.
           03  WK-MWST                 PIC S9(7)V99  COMP.
           03  WK-BASIS                PIC S9(9)V99  COMP.
           03  WK-BRUTTO               PIC S9(9)V99  COMP.
           03  WK-NETTO                PIC S9(9)V99  COMP.
           03  WK-BUBET                PIC S9(9)V99  COMP.
           03  WK-UMS                  PIC S9(9)V99  COMP.
           03  WK-KSBET                PIC S9(9)V99  COMP.
           03  WK-KSMG                 PIC S9(9)V99  COMP.
           03  WK-STAND                PIC S9(7)V99  COMP.
           03  WK-BET                  PIC S9(9)V99  COMP.
           03  WD-MENGA                PIC ZZ.ZZ9,99-.
           03  WD-MENGB                PIC ZZ.ZZ9,9-.
           03  WD-MENGC                PIC ZZZ.ZZ9-.
           03  WD-MENGE                PIC X(10).
           03  WD-DIF                  PIC ZZ9,99-.
           03  WH-DIF                  PIC 9(4)V99   COMP.
           03  WH-MENGE                PIC S9(7)V99  COMP.
           03  WC-GRAMM                PIC S9(7)     COMP.
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(6)      COMP.
               05 WH-LFD               PIC 99        COMP-X.
           03  WH-JOKEY                PIC X(12).
           03  WH-KAT                  PIC 9         COMP.
           03  WH-BN                   PIC 99        COMP.
           03  WH-AKAT                 PIC 9         COMP.
           03  WH-ABN                  PIC 99        COMP.
           03  WH-GBN                  PIC 99        COMP.
           03  WH-JS                   PIC 9(7)      COMP.
           03  WM-KTONR                PIC 9(6)      COMP.
           03  WM-UST                  PIC S9(8)V99  COMP.
           03  WH-PREIS                PIC S9(7)V99  COMP.
           03  WD-KOMPO                PIC Z.ZZ9.
           03  WH-ANZ                  PIC S9(8)V99.
           03  WH-STK                  PIC S9(7)      COMP.
           03  WH-MEH                  PIC 99         COMP.
           03  WS-MENGE                PIC S9(9)      COMP.
           03  WH-ATG                  PIC S9(5)V9(7) COMP.
           03  WH-BET                  PIC S9(9)V99   COMP.
           03  WD-BET                  PIC ZZZ.ZZ9,99-.
           03  WD-GEW                  PIC ZZZ,999-.
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-JONUM                PIC Z.ZZ9.
           03  WD-NUM                  PIC Z.ZZ9.
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-UST                  PIC ZZZZZ.ZZ9,99-.
           03  WD-BELNR                PIC ZZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9.
           03  WD-PREIS                PIC ZZZ.ZZ9,99-.
           03  WD-MENG                 PIC ZZ.ZZ9,99-.
           03  WD-STAND                PIC ZZZ.ZZ9,9-.
           03  WD-KZ                   PIC 9.
           03  WD-RAB                  PIC Z9,99-.
           03  WD-PROZ                 PIC Z9.
           03  WD-KOART                PIC Z9.
           03  WD-PZ                   PIC 9,9.
           03  WD-TG                   PIC ZZ9.
           03  WH-TNAM                 PIC 9(6)         VALUE ZERO.
           03  RED REDEFINES WH-TNAM.
               05 FILLER               PIC 9.
               05 WH-T                 PIC 9.
               05 WH-N                 PIC 9.
               05 WH-A                 PIC 9.
               05 WH-M                 PIC 99.
           03  WH-TART                 PIC 99           VALUE ZERO.
           03  REDA REDEFINES WH-TART.
               05 WH-BK                PIC 9.
               05 WH-RART              PIC 9.
           03  WH-ZEIT                 PIC 9(8).
      *--------------------------------------> Datenfelder fuer DFUE <-
       01  WH-DFU.
           03  WK-RTS                  PIC X       VALUE HIGH-VALUE.
           03  WK-DTR                  PIC X       VALUE HIGH-VALUE.
           03  WH-RETC                 PIC 9999   COMP-5 VALUE 11.
           03  WH-MODSTAT              PIC 9999   COMP-5 VALUE 0.
           03  WK-CHAN                 PIC 9999   COMP-5 VALUE 1.
           03  WK-BAUD                 PIC X      VALUE X"9B".
      *-----> "9F" = 1001 1011 * 100 = 1200 * 11 = E * 0 = 1 * 11 = 8 <-
      *--------------------->  Baudrate / parity / stopbit / bit/word <-
           03  WK-MASK                 PIC 9999   COMP-5 VALUE 64.
           03  WH-END                  PIC X      VALUE X"0D".
           03  WH-CR                   PIC X      VALUE X"0D".
           03  WH-LF                   PIC X      VALUE X"0A".
           03  WH-BCC                  PIC X.
           03  E-FELD.
               05 E-F1                 PIC X.
               05 E-REST               PIC X(44).
           03  E-TIME                  PIC 9(5)   COMP-5 VALUE 32700.
           03  E-LENG                  PIC 9(5)   COMP-5 VALUE ZERO.
           03  R-LENG                  PIC 9(5)   COMP-5 VALUE ZERO.
           03  WH-KEYB                 PIC 9(5)   COMP-5 VALUE 1.
           03  WH-WAAGE.
               05 WA-ID                PIC XX.
               05 WA-FILA              PIC X.
               05 WA-GEW               PIC 9(5).
               05 WA-FILB              PIC X.
               05 WA-GRAM              PIC 999.
               05 WA-SPACE             PIC X.
               05 WA-MEH               PIC XX.
               05 WA-END               PIC XX.
      *------------------------------------------> MFC-x"??"-Routinen <-
           03  WK-KBD                  PIC X       COMP-X.
      *    03  WK-3F8                  PIC 99999      VALUE 1016.
      *    03  WK-3F9                  PIC 99999      VALUE 1017.
      *    03  WK-3FA                  PIC 99999      VALUE 1018.
      *    03  WK-3FB                  PIC 99999      VALUE 1019.
      *    03  WK-3FC                  PIC 99999      VALUE 1020.
           03  WK-3F8                  PIC 99999      VALUE  760.
           03  WK-3F9                  PIC 99999      VALUE  761.
           03  WK-3FA                  PIC 99999      VALUE  762.
           03  WK-3FB                  PIC 99999      VALUE  763.
           03  WK-3FC                  PIC 99999      VALUE  764.
           03  WK-X                    PIC X.
           03  WT-DAT.
               05 WT-DS                PIC X(23)  OCCURS 10.
               05 WT-KO                PIC X      OCCURS 70.
           03  WD-WS                   PIC ZZZ9-.
       COPY PAKOEXT.CPY.
       COPY PAKODECL.CPY.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON PROTWAAG.
       A.  CALL "CADECL" USING "PAKWAAG.DAT " WH-CREG.
       DECL-R SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       DECL-M SECTION.         USE AFTER ERROR PROCEDURE ON SREKOPF.
       A.  CALL "CADECL" USING "PAKSRKOP.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 10 PERFORM VERWIEG
               WHEN 20                         *> normales buchen
               WHEN 30 PERFORM BUCHEN.         *> Scheinrechnungserf.
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
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 07 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       NODATA SECTION.
       A.  DISPLAY "** Datenende **" with highlight blink AT 2466.
           SET FINE TO TRUE.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) NOT = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              GO A.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN = 0 MOVE 1 TO WM-OPEN
               OPEN OUTPUT DRUCKER
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
       Z.  EXIT.
      ************************************** fÅr Journalbuchungszeilen *
       BS-JZEIL SECTION.
       A.  ADD 1 TO IX.
       B.  IF IX > 16 CALL "CAUP" USING "17SCROLLF" WH-CREG
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX = 16
                   MOVE WT-JKEY(IX + 1) TO WT-JKEY(IX)
               end-perform MOVE LOW-VALUE TO WT-JKEY(IX).
       Z.  EXIT.
      **************************************** fÅr Kontobuchungszeilen *
       BS-BZEIL SECTION.
       A.  ADD 1 TO IY.
           IF IY < 9 GO Z.
       B.  CALL "CAUP" USING "17SCROLLF" WH-CREG.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 7
               MOVE WT-KEY(IY + 1) TO WT-KEY(IY).
           MOVE LOW-VALUE TO WT-KEY(IY).
       Z.  EXIT.
      ***************************************** LP-SATZ initialisieren *
       INIT-BZ SECTION.
       A.  INITIALIZE LB-SATZ.
       Z.  EXIT.
      *****************************************************************
       WD-MG SECTION.
       A.  EVALUATE LB-NK
               WHEN 2 MOVE WH-MENGE TO WD-MENGA
                      MOVE WD-MENGA(1:) TO WD-MENGE
               WHEN 1 MOVE WH-MENGE TO WD-MENGB
                      MOVE WD-MENGB(1:) TO WD-MENGE
               WHEN 0 MOVE WH-MENGE TO WD-MENGC
                      MOVE WD-MENGC(1:) TO WD-MENGE.
           DISPLAY WD-MENGE with highlight AT VDU-LP.
       Z.  EXIT.
      ************************************** eigentl. Buchungsprogramm *
       BUCHEN SECTION.
       A.  IF WL-CA = 30 MOVE "  Eingangsrechnungen" TO WK-GEB
                    else MOVE "    allg. Buchungen " TO WK-GEB.
           MOVE 0 TO WM-KTONR.
           MOVE WH-DATUM TO WM-DATUM WZ-DATUM.
           ADD 1900 WZ-JAHR GIVING WX-J.
           IF WX-J < 1990 ADD 100 TO WX-J.
           PERFORM INIT-BZ.
           DISPLAY ALL SPACES with size 50 at 2501.
           OPEN I-O SREKOPF.
       C.  CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM P-PRUEF.
           IF ESC GO W.
           IF WL-CA = 30 MOVE 1 TO WE-BSR
                    else MOVE 0 TO WE-BSR.
      *-----------------------------------------------------------------
           IF SR PERFORM SR-KOPF
               IF ESC GO W.
       E.  PERFORM VARYING IX FROM 17 BY -1 UNTIL IX = 0
               OR WT-JKEY(IX) NOT = LOW-VALUE
               END-PERFORM.
       F.  IF NOT XPOS PERFORM BS-JZEIL.
      *--------------------------------------------> Komponentenummer <-
       G.  DISPLAY "alpha+<ret>= suchen, <#>= Art. anzeigen, <ret-leer>=
      -        " letzte Art.-Nr." AT 2301.
           DISPLAY "<esc>= Re-Ende, <Ende>= Ende mit SR-Druck" AT 2401.
           UNLOCK KONSLAG.
           UNLOCK LAGARTIK.
           UNLOCK LAGERBZ.
       I.  IF WM-G NOT = 1 PERFORM ANZEIG
               IF WM-M = 2 PERFORM BUTEX.
           COMPUTE VDU-L = IX + 4.
           IF WT-JKEY(IX) = LOW-VALUE PERFORM DIS-BET.
           CALL "CAUP" USING "1000026108" WH-CREG.
           MOVE LOW-VALUE TO WE-RETAB.
           MOVE 0 TO WH-KAT WH-AKAT.
           IF WT-JKEY(IX) = LOW-VALUE
               CALL "CAUP" USING "1300012580" WH-CREG.
       K.  IF HELP CALL "CAUP" USING "190075007601" WH-CREG GO I.
           IF WM-G = 1 MOVE 0 TO WM-G
               CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WORE AND WM-M = 1 PERFORM TX-KORR GO G.
           IF WM-M = 1 ADD 1 TO VDU-L
                       CALL "CAUP" USING "1400002280" WH-CREG
                       SUBTRACT 1 FROM VDU-L
                       MOVE 0 TO WM-M.
           IF EINF CALL "PAKOANZ" USING "10JOUR" WH-CREG
                   CANCEL "PAKOANZ" GO G.
           IF KIST; IF WH-NUM = 0 MOVE LB-KTONR TO WH-NUM
                    end-if
                   IF WH-NUM NOT = 0 MOVE 1 TO WH-AKAT
                       PERFORM LIESKTO
                       IF NOT FEHLER
                           CALL "PARKOMKA" USING "52ARTIK" WH-CREG
                           CANCEL "PARKOMKA"
                       end-if
                   end-if GO G.
           IF NOT SR; IF ESC OR ENDE GO W.
           IF ENDE PERFORM SR-DRUCK GO C.
           IF ESC  MOVE 0 TO WE-LIEFER WE-BELNR WE-BELDAT GO C.
           IF APOS; IF WH-NUM NOT = 0 PERFORM FIRST-JOUR
                                      SET BAB TO TRUE GO K
                    else MOVE 1 TO IX GO G.
           IF XPOS GO E.
           IF BAUF; IF WH-NUM = 0 PERFORM AUFZEIL GO G
                    else PERFORM FIRST-JOUR
                         SET BAB TO TRUE GO K.
           IF AUF; IF IX > 1 SUBTRACT 1 FROM IX
                   ELSE PERFORM AUFZEIL
                   end-if MOVE 2 TO WM-M GO G.
           IF BAB PERFORM ABZEIL; IF FINE GO E
                  ELSE SUBTRACT 1 FROM IX GO G.
           IF AB; IF WT-JKEY(IX + 1) NOT = LOW-VALUE GO F
                  ELSE PERFORM ABZEIL
                       IF FINE GO E ELSE GO G.
           MOVE 0 TO WI WM-GB.
           IF SAPO PERFORM FIRST-JOUR
                   SET BAB TO TRUE GO K.
           IF SEPO PERFORM AUSGAB GO F.
           IF RET AND WH-NUM = 0 AND WH-MCODE = SPACE
               MOVE WM-KTONR TO WH-WERT WH-NUM.
           IF ENTF PERFORM LOEBU GO G.
           IF NOT RET GO G.
           IF WT-JKEY(IX) NOT = LOW-VALUE PERFORM LAST-JOUR GO E.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PANANZ" USING "30SUCH" WH-CREG
               CANCEL "PANANZ".
           IF WH-NUM = 0 GO G.
           PERFORM LIESKTO-NOLOCK.
           IF FEHLER GO G.
           MOVE WH-KAT TO WH-AKAT.                  *> Kateg. Erstkonto
           MOVE WH-BN TO WH-ABN.                    *> brutto/netto - KZ
           MOVE WH-NUM TO WK-KTONR LB-KTONR WM-KTONR.
           ADD WE-USE 1 GIVING WH-KEY.
       M.  READ KONSLAG INVALID INITIALIZE KL-BSATZ
               MOVE WH-KEY TO KL-NUM WRITE KL-SATZ GO M.
           IF ZUGRIF STOP RUN.
           MOVE WE-PER TO WC-DATUM.
           IF KL-BUDAT = 0 MOVE WE-PER TO KL-BUDAT.
           MOVE KL-BUDAT TO WZ-DATUM WE-PER
           IF WC-DATUM(1:4) NOT = WZ-DATUM(1:4) MOVE WE-PER TO KL-BUDAT.
           MOVE LOW-VALUE TO KL-JOKEY.
           PERFORM BUCH-ZEIL.
           IF LB-SYM = 3 GO E.                  *> keine Preisbuchungen
           IF ESC GO E.
           PERFORM LB-WRITE.
           GO E.
       W.  CLOSE SREKOPF.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
      **************************************************** weiterlesen *
       ABZEIL SECTION.
       A.  PERFORM VARYING IX FROM 16 BY -1 UNTIL IX = 0
               OR WT-JKEY(IX) NOT = LOW-VALUE CONTINUE.
           IF IX = 0 MOVE 1 TO IX SET FINE TO TRUE GO X.
       C.  MOVE WT-JKEY(IX) TO WH-JOKEY.
           IF AB OR (BAB AND WH-NUM = 0) PERFORM READBEW
               ADD 1 TO IX
               MOVE 0 TO WI WR
               IF AB MOVE 14 TO WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 15.
           IF WI NOT = 8 MOVE 9 TO WI.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAB SECTION.
       E.  READ LAGERBZ NEXT AT END MOVE 8 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF SR; IF WE-LIEFER NOT = LB-LIEFER
               OR WE-BELNR NOT = LB-BELNR OR WE-BELDAT NOT = LB-BELDAT
               SET FINE TO TRUE
               MOVE 8 TO WI GO Z.
           IF WE-USE NOT = LB-USE MOVE 8 TO WI SET FINE TO TRUE GO Z.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 16
               IF LB-JKEY = WT-JKEY(WF) GO E.
           IF IX = 17 CALL "CAUP" USING "17SCRAUF" WH-CREG
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX = 16
                   MOVE WT-JKEY(IX + 1) TO WT-JKEY(IX)
                   END-PERFORM.
           PERFORM JOUR-DIS.
           IF BAB ADD 1 TO IX.
       Z.  EXIT.
      ******************************************************************
       AUFZEIL SECTION.
       A.  IF WT-JKEY(1) = LOW-VALUE SET FINE TO TRUE GO X.
           MOVE WT-JKEY(1) TO WH-JOKEY LB-JKEY.
           IF WR = -999 MOVE LOW-VALUE TO WT-KEY(1) WT-JKEY(1).
           MOVE 0 TO WR.
           IF AUF MOVE 14 TO WR.
           START LAGERBZ KEY < LB-JKEY INVALID SET FINE TO TRUE GO X.
           MOVE 1 TO IX WI.
           PERFORM NEXTAUF WITH TEST AFTER UNTIL WI = 2 OR WR = 15.
           MOVE 1 TO IX.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAUF SECTION.
       N.  READ LAGERBZ PREVIOUS AT END MOVE 2 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF SR; IF WE-LIEFER NOT = LB-LIEFER
               OR WE-BELNR NOT = LB-BELNR OR WE-BELDAT NOT = LB-BELDAT
               SET FINE TO TRUE
               MOVE 2 TO WI GO Z.
           IF WE-USE NOT = LB-USE SET FINE TO TRUE MOVE 2 TO WI GO Z.
           IF WI > 8 PERFORM VARYING WF FROM 16 BY -1 UNTIL WF < 1
               IF LB-JKEY = WT-JKEY(WF) GO N.
           MOVE VDU-LP TO VDU-AB.
           MOVE 1500 TO VDU-LP.
           CALL "CAUP" USING "18CLRWIN" WH-CREG.
           MOVE VDU-AB TO VDU-LP.
           CALL "CAUP" USING "22SCRAB" WH-CREG.
           PERFORM VARYING IX FROM 16 BY -1 UNTIL IX < 2
              MOVE WT-JKEY(IX - 1) TO WT-JKEY(IX).
           COMPUTE VDU-LP = IX * 200 + 300.
           PERFORM JOUR-DIS.
       Z.  EXIT.
      ******************************************************************
       READBEW SECTION.
       A.  MOVE WH-JOKEY TO LB-JKEY.
       C.  READ LAGERBZ KEY LB-JKEY INVALID MOVE LOW-VALUE TO WT-JOTAB.
           IF ZUGRIF PERFORM BESETZT GO C.
       Z.  EXIT.
      ******************************************* Einzelzeile anzeigen *
       ANZEIG SECTION.
       A.  SET RET TO TRUE.
           IF IX = 0 OR WT-JKEY(IX) = LOW-VALUE PERFORM INIT-BZ
               COMPUTE VDU-L = IX + 4
               CALL "CAUP" USING "1300012280" WH-CREG GO Z.
           MOVE WT-JKEY(IX) TO WH-JOKEY LB-JKEY.
       C.  READ LAGERBZ KEY IS LB-JKEY INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM JOUR-DIS.
       Z.  EXIT.
      ******************************************* Einzelzeile anzeigen *
       BUZEIG SECTION.
       A.  SET RET TO TRUE.
           IF IY = 0 OR WT-KEY(IY) = LOW-VALUE PERFORM INIT-BZ GO Z.
           MOVE WT-KEY(IY) TO WH-BKEY LB-KEY.
       C.  READ LAGERBZ KEY IS LB-KEY INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM BUCH-DIS.
       Z.  EXIT.
      ************************************************* SRE-KOPF lesen *
       SR-KOPF SECTION.
       A.  IF WE-LIEFER NOT = 0 GO I.
           MOVE 0 TO SK-SA.
       B.  CALL "CAUP" USING "1303010380" WH-CREG.
           DISPLAY "Nr.:        /          Lft.: " AT 0303.
       C.  DISPLAY "<esc>= Abbruch, alpha+<ret>= suchen Lft., <ret>= Lie
      -        "ferant" AT 2301.
           DISPLAY "< >= Rechng. aus l. Buchung, < >= Rechng. ab Lft.Nr.
      -        "" AT 2401.
           CALL "CAUP" USING "0003325005" WH-CREG.
           IF ESC GO Z.
           IF AUF; IF WT-KEY(1) NOT = LOW-VALUE
                      MOVE LB-LIEFER TO WE-LIEFER
                      MOVE LB-BELNR TO WE-BELNR
                      MOVE LB-BELDAT TO WE-BELDAT GO I.
           IF AB CALL "PAKOANZ" USING "20SRSUCH" WH-CREG
                 CANCEL "PAKOANZ"
                 IF SK-LIEFER NOT = 0 MOVE SK-LIEFER TO WE-LIEFER
                                      MOVE SK-BELNR TO WE-BELNR
                                      MOVE SK-BELDAT TO WE-BELDAT GO I.
           IF NOT RET GO C.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PAKOANZ" USING "40KRSUCH" WH-CREG
               CANCEL "PAKOANZ".
           IF WH-NUM = 0 GO C.
           PERFORM READ-LFT.
           IF ESC GO C.
           MOVE KR-KTONR TO WE-LIEFER.
           MOVE KR-KTONR TO WD-KTO.
           MOVE KR-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO with highlight AT VDU-LP " ".
           MOVE WT-TX TO LB-TX.
           DISPLAY LB-TX(1:5) AT 0000 " " WT-BEZ(1:30) with highlight.
       E.  DISPLAY "Eingangsrechnung-Nr. oder sonstg. Referenz-Nr."
                with highlight AT 2401.
           DISPLAY "<esc>= abbruch, <>= zurÅck, <ret>= Nummer" AT 2301.
           CALL "CAUP" USING "0003087007" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO B.
           IF AB CALL "PAKOANZ" USING "20SRSUCH" WH-CREG
                 CANCEL "PAKOANZ"
                 IF SK-LIEFER NOT = 0 MOVE SK-LIEFER TO WE-LIEFER
                                      MOVE SK-BELNR TO WE-BELNR
                                      MOVE SK-BELDAT TO WE-BELDAT GO I.
           IF NOT RET GO E.
           MOVE WH-WERT TO WD-BELNR WE-BELNR SK-BELNR.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
       G.  DISPLAY "Belegdatum" with highlight AT 2401.
           DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret>= Belegdatum"
               AT 2301.
           IF WE-BELDAT = 0 MOVE WM-DATUM TO WE-BELDAT.
           MOVE WE-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0316.
           MOVE WE-BELDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0103166006" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO E.
           IF NOT RET GO G.
           MOVE WZ-DATUM TO WE-BELDAT SK-BELDAT.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           GO Z.
      *----------------------------------------> wenn Daten vorhanden <-
       I.  MOVE WE-LIEFER TO SK-LIEFER KR-KTONR WD-KTO.
           MOVE WE-BELNR TO SK-BELNR.
           MOVE WE-BELDAT TO SK-BELDAT.
           MOVE 0 TO SK-SA.
       K.  READ SREKOPF INVALID INITIALIZE SK-SATZ GO B.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE KR-KTONR TO WH-NUM WD-KTO.
           PERFORM READ-LFT.
           MOVE KR-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO with highlight AT 0332 " ".
           MOVE WT-TX TO LB-TX.
           DISPLAY LB-TX(1:5) AT 0000 " " WT-BEZ(1:30) with highlight.
           ADD 203 VDU-ECK GIVING VDU-LP.
           MOVE SK-BELNR TO WD-BELNR.
           MOVE SK-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Nr.: " AT 0303 WD-BELNR with highlight "/"
               VDU-DATUM with highlight.
           PERFORM LAST-SRBU.
       Z.  EXIT.
      *************************************** anzeigen Rechnungsbetrag *
       DIS-BET SECTION.
       A.  IF NOT SR OR WT-JKEY(IX) NOT = LOW-VALUES GO Z.
           MOVE SK-BET TO WD-BET.
           MOVE 17 TO VDU-P.
           DISPLAY WD-BET with highlight foreground-color 6 AT VDU-LP
                " Netto-Rechnungsbetrag".
       Z.  EXIT.
      ****************************************** lîschen Journalzeilen *
       LOEBU SECTION.
       A.  DISPLAY "Code: <    >, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "002308404" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           IF WH-NUM NOT =  LB-SEITE GO A.
      *-------------------------------------------------> Kontosummen <-
           MOVE LB-KTONR TO LG-NUM.
       C.  READ LAGARTIK INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           SUBTRACT LB-MENGE FROM LG-STAND.
           REWRITE LG-SATZ.
      *-----------------------------------------------> Journalsummen <-
           ADD WE-USE 1 GIVING WH-KEY.
       E.  READ KONSLAG INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           SUBTRACT LB-MENGE FROM KL-JSUM.
           REWRITE KL-SATZ.
           DELETE LAGERBZ INVALID GO Z.
           IF IX = 1; IF WT-JKEY(2) = LOW-VALUE SET AUF TO TRUE
                           CALL "CAUP" USING "16CLRFEN" WH-CREG
                           MOVE -999 TO WR
                           PERFORM AUFZEIL GO Z
                      else MOVE WT-JKEY(2) TO WH-JOKEY
           else MOVE WT-JKEY(1) TO WH-JOKEY.
           MOVE LOW-VALUE TO WT-JOTAB.
           MOVE WH-JOKEY TO WT-JKEY(1).
           SET BAB TO TRUE.
           MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 1 TO IX.
           PERFORM ANZEIG.
           SET BAB TO TRUE.
           PERFORM ABZEIL.
       Z.  EXIT.
      ******************************************************************
       BUTEX SECTION.
       A.  IF WT-JKEY(IX) = LOW-VALUE GO Z.
           MOVE 1 TO WM-M.
           COMPUTE VDU-LP = IX * 100 + 502.
           CALL "CAUP" USING "220000SCRAB" WH-CREG.
           CALL "CAUP" USING "1300010080" WH-CREG.
           MOVE LB-SEITE TO WD-JONUM.
           DISPLAY WD-JONUM AT VDU-LP " "
               " " with reverse-video SIZE 16.
           ADD 7 TO VDU-LP.
           DISPLAY LB-TX AT VDU-LP.
           MOVE 25 TO VDU-P.
           MOVE LB-BET TO WD-BET.
           DISPLAY WD-BET with highlight foreground-color 6 AT VDU-LP.
           SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ******************************************************************
       AUSGAB SECTION.
       A.  MOVE 1 TO IX.
           MOVE WT-JKEY(IX) TO LB-JKEY.
           MOVE LOW-VALUE TO WT-JOTAB.
           START LAGERBZ KEY NOT < LB-JKEY INVALID
               SET FINE TO TRUE GO X.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           SET BAB TO TRUE.
           MOVE 0 TO WI WR.
           PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8.
           IF WI NOT = 8 MOVE 9 TO WI.
       H.  IF IX = 0 GO X.
           IF WT-JKEY(IX) = LOW-VALUE ADD -1 TO IX GO H.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ************************************************** Buchungszeile *
       SYMDIS SECTION.
       A.  EVALUATE LB-SYM
               WHEN 0 DISPLAY "zu" AT VDU-LP
               WHEN 1 DISPLAY "ab" AT VDU-LP
               WHEN 2 DISPLAY "ib" AT VDU-LP
               WHEN 3 DISPLAY "pr" AT VDU-LP
               WHEN 4 DISPLAY "rp" AT VDU-LP.
       Z.  EXIT.
      ************************************************** Buchungszeile *
       BUCH-ZEIL SECTION.
       A.  CALL "CAUP" USING "0703012080020" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " K-Nr.:         " AT VDU-LP.
           ADD 8 TO VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 8 TO VDU-LP.
           DISPLAY ALL " " with SIZE 52 reverse-video AT VDU-LP.
           ADD 1 TO VDU-LP.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           DISPLAY WT-TX with SIZE 30 AT VDU-LP.
           ADD 30 TO VDU-LP.
           DISPLAY WT-MEH(LG-MEH + 1) AT VDU-LP.
           ADD 301 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bu-Datum Sy  Bel-Nr.- Datum      Menge  Meh     Pre
      *              12.12.12 zu 1234567 12.12.88 12.345,67- kg 123.456,
      -        "is  Rab.% Rab.% Lieferant   " with background-color 2
      *        "78- 12,50 12,50 12345/ABCED
                    highlight AT VDU-LP.
           MOVE LB-SATZ TO WH-BZSATZ.
           PERFORM LAST-BUCH.
           ADD 1 TO IY.
           MOVE 0 TO WU.
           MOVE LG-STAND TO WK-UMS.
           PERFORM SALDO.
           MOVE WH-BZSATZ TO LB-SATZ.
       B.  PERFORM VARYING IY FROM 10 BY -1 UNTIL IY = 0
               OR WT-KEY(IY) NOT = LOW-VALUE
               END-PERFORM.
           IF NOT XPOS PERFORM BS-BZEIL.
      *-----------------------------------------------> Buchungsdatum <-
       C.  DISPLAY "<ret>= Buchungsdatum, <esc>= Ende" at 2301.
           PERFORM BUZEIG.
           MOVE WK-KTONR TO LB-KTONR.
           MOVE WE-PER TO WC-DATUM.
           PERFORM DATDREH.
           COMPUTE VDU-LP = IY * 200 + 202 + VDU-ECK.
           IF WT-KEY(IY) = LOW-VALUE AND WC-DATUM NOT = 0
                DISPLAY VDU-DATUM AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 200.
           MOVE WC-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1100026006" WH-CREG.
           IF HELP CALL "CAUP" USING "190077007801" WH-CREG GO E.
           IF ESC GO X.
           IF APOS MOVE 1 TO IY GO C.
           IF XPOS GO B.
           if sent delete LAGERBZ
              set sf3 to true.
           IF SF3 CALL "PAKOANZ" USING "90KTOSUM" WH-CREG
                  CANCEL "PAKOANZ"
                  SET ESC TO TRUE GO X.
           IF BAUF PERFORM AUFBUZEI GO C.
           IF AUF; IF IY > 1 SUBTRACT 1 FROM IY GO C
                   ELSE PERFORM AUFBUZEI GO C.
           IF BAB PERFORM ABBUZEI; IF FINE GO B
                  ELSE SUBTRACT 1 FROM IY GO C.
           IF AB; IF WT-KEY(IY + 1) NOT = LOW-VALUE
                     PERFORM BS-BZEIL GO C
                  ELSE PERFORM ABBUZEI
                       IF FINE GO B ELSE GO C.
           MOVE 0 TO WI.
           IF NOT RET AND NOT SEIN GO C.
           IF SEIN CALL "PAKOANZ" USING "80KORR" WH-CREG
                   CANCEL "PAKOANZ"
                   SET ESC TO TRUE GO X.
           IF WT-KEY(IY) NOT = LOW-VALUE PERFORM LAST-BUCH GO B.
           IF WZ-DATUM = 0 GO C.
           MOVE WZ-DATUM TO LB-BUDAT.
           MOVE WE-PER TO WC-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WZ-JAHR NOT = WC-TAG OR WZ-MONAT NOT = WC-MONAT
               CALL "PAKOANZ" USING "50PERWE" WH-CREG
               CANCEL "PAKOANZ"
               IF ESC GO C.
           MOVE LB-BUDAT TO KL-BUDAT.
           IF SR COMPUTE VDU-LP = IY * 200 + 211 + VDU-ECK
               MOVE 0 TO LB-SYM
               PERFORM SYMDIS
               ADD 3 TO VDU-LP
               MOVE WE-BELNR TO WD-BELNR LB-BELNR
               DISPLAY WD-BELNR with highlight AT VDU-LP
               ADD 8 TO VDU-LP
               MOVE WE-BELDAT TO LB-BELDAT  WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               COMPUTE VDU-LP = IY * 200 + 242 + VDU-ECK
               ADD 1 LG-MEH GIVING LB-MEH
               MOVE LG-NK TO LB-NK
               DISPLAY WT-MEH(LB-MEH) AT VDU-LP
               COMPUTE VDU-AB = IY * 2 + 2
               GO J.
      *------------------------------------------------------> Symbol <-
       E.  DISPLAY "<esc>= Abbruch, <ret>= Symbolkz." AT 2301.
           DISPLAY "0= Zugang, 1= Abgang, 2= Invent., 3= Preise, 4= Rez.
      -        " Abgang" AT 2401.
           MOVE LB-SYM TO WH-WERT.
           COMPUTE VDU-LP = IY * 200 + 211.
           MOVE VDU-L TO VDU-AB.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF ESC GO X.
           IF WOLI GO C.
           IF NOT RET GO E.
           IF WH-WERT > 4 GO E.
           MOVE WH-WERT TO LB-SYM.
           PERFORM SYMDIS.
      *-------------------------------------------------> Belegnummer <-
       G.  IF LB-SYM NOT = 0 GO H.
           DISPLAY "<esc>= Abbruch, <ret>= Beleg-Nr." AT 2301.
           COMPUTE VDU-LP = IY * 200 + 214.
           MOVE WE-BELNR TO WH-WERT.
           CALL "CAUP" USING "1000007007" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RET GO E.
           MOVE WH-WERT TO WD-BELNR LB-BELNR.
           MOVE LB-BELNR TO WE-BELNR.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
           GO I.
      *------------------------------> Buchungstext bei LB-SYM= 1 / 2 <-
       H.  COMPUTE VDU-LP = IY * 200 + 214 + VDU-ECK.
           DISPLAY LB-TX AT VDU-LP.
           MOVE LB-TX TO WT-TX.
           DISPLAY "<ret>= Texthinweis, <esc>= Abbruch, <>= zurÅck"
               AT 2301.
           CALL "CAUP" USING "0200140115" WH-CREG.
           IF ESC GO X.
           IF WOLI GO E.
           IF NOT RET GO H.
           MOVE WT-TX TO LB-TX.
           DISPLAY LB-TX AT VDU-LP.
           GO J.
      *--------------------------------------------------> Belegdatum <-
       I.  DISPLAY "<esc>= Abbruch, <ret>= Belegdatum" AT 2301.
           IF LB-BELDAT = 0 MOVE WM-DATUM TO LB-BELDAT.
           MOVE LB-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           COMPUTE VDU-LP = IY * 200 + 222 + VDU-ECK.
           DISPLAY VDU-DATUM AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 222.
           MOVE LB-BELDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1100006006" WH-CREG.
           IF WOLI GO G.
           IF ESC GO X.
           IF NOT RET GO I.
           IF WZ-DATUM = 0 GO I.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WZ-DATUM TO LB-BELDAT.
       J.  COMPUTE VDU-LP = IY * 200 + 242 + VDU-ECK.
           ADD 1 LG-MEH GIVING LB-MEH.
           MOVE LG-NK TO LB-NK.
           DISPLAY WT-MEH(LB-MEH) AT VDU-LP.
           IF LB-SYM = 3 CALL "CAUP" USING "1300310079" WH-CREG GO L.
      *-------------------------------------------------------> Menge <-
       K.  IF LB-SYM = 3 GO G.
           DISPLAY "<esc>= Abbruch, <ret>= Betrag" AT 2301.
           MOVE 0 TO WH-WERT WH-NUM.
           MOVE VDU-AB TO VDU-L.
           CALL "CAUP" USING "1000318212" WH-CREG.
           IF WM-G = 1 MOVE 0 TO WM-G
               CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WOLI CALL "CAUP" USING "1300310079" WH-CREG;
                   IF SR GO C else GO G.
           IF ESC GO X.
           IF NOT RET GO K.
           IF LB-SYM = 1 MULTIPLY -1 BY WH-WERT.
           MOVE WH-WERT TO LB-MENGE WH-MENGE WK-BUBET.
           IF WH-WERT NOT = 0 PERFORM WD-MG
           else CALL "CAUP" USING "1300310079" WH-CREG.
           CALL "CAUP" USING "1300440079" WH-CREG.
           PERFORM UPSALDO.
      *------------------------------------------------> Preis/Betrag <-
       L.  DISPLAY "<esc>= Abbruch, <ret>= Preis, < >= Betrag" AT 2301.
           COMPUTE VDU-LP = IY * 200 + 245 + VDU-ECK.
           IF LB-PREIS = 0 MOVE LG-PREIS TO LB-PREIS.
           MOVE LB-PREIS TO WD-PREIS WH-WERT.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 245.
           CALL "CAUP" USING "1000006209" WH-CREG.
           EVALUATE TRUE
               WHEN AB MOVE WH-WERT TO LB-BET WD-PREIS
                   MOVE 0 TO LB-PREIS
                   DISPLAY WD-PREIS with highlight reverse-video
                       AT VDU-LP
               WHEN RET MOVE WH-WERT TO WD-PREIS LB-PREIS WE-PREIS
                   MOVE 0 TO LB-BET
                   DISPLAY WD-PREIS with highlight AT VDU-LP
               WHEN WOLI CALL "CAUP" USING "1300550079" WH-CREG
                   MOVE LG-STAND TO WK-UMS
                   PERFORM SALDO GO K
               WHEN ESC GO X
               WHEN OTHER GO L.
      *--------------------------------------------------> Rabatt -1- <-
       M.  IF LB-SYM = 1 or LB-SYM = 2 MOVE SPACE TO LB-SRECH GO T.
           DISPLAY "<esc>= Abbruch, <ret>= Rabatt" AT 2301.
           MOVE LB-RAB(1) TO WH-WERT.
           MOVE VDU-AB TO VDU-L.
           CALL "CAUP" USING "1000572205" WH-CREG.
           IF WOLI CALL "CAUP" USING "1300570079" WH-CREG GO K.
           IF ESC GO X.
           IF WOLI GO L.
           IF NOT RET GO M.
           MOVE WH-WERT TO WD-RAB LB-RAB(1) WE-RAB(1).
           IF LB-RAB(1) = 0 CALL "CAUP" USING "1300570079" WH-CREG
                       else DISPLAY WD-RAB with highlight AT VDU-LP.
      *--------------------------------------------------> Rabatt -2- <-
       O.  DISPLAY "<esc>= Abbruch, <ret>= Rabatt" AT 2301.
           MOVE LB-RAB(2) TO WH-WERT.
           MOVE VDU-AB TO VDU-L.
           CALL "CAUP" USING "1000632205" WH-CREG.
           IF WOLI CALL "CAUP" USING "1300630079" WH-CREG GO K.
           IF ESC GO X.
           IF WOLI GO M.
           IF NOT RET GO O.
           MOVE WH-WERT TO WD-RAB LB-RAB(2) WE-RAB(2).
           IF LB-RAB(2) = 0 CALL "CAUP" USING "1300630079" WH-CREG
                       else DISPLAY WD-RAB with highlight AT VDU-LP.
      *---------------------------------------------------> Lieferant <-
       R.  IF SR MOVE WE-LIEFER TO WH-NUM
               COMPUTE VDU-LP = IY * 200 + 269 + VDU-ECK
               GO S.
           DISPLAY "<esc>= Abbruch, <ret>= Lieferant, alpha+<ret>= suche
      -        "n" AT 2301.
           MOVE LB-TX TO WT-TX.
           MOVE VDU-AB TO VDU-L.
           CALL "CAUP" USING "1000695005" WH-CREG.
           IF ESC GO X.
           IF WOLI GO O.
           IF NOT RET GO R.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PAKOANZ" USING "40KRSUCH" WH-CREG
               CANCEL "PAKOANZ".
           IF WH-NUM = 0 GO R.
       S.  PERFORM READ-LFT.
           IF ESC GO R.
           MOVE KR-KTONR TO LB-LIEFER.
           MOVE KR-KTONR TO WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP"/".
           MOVE WT-TX TO LB-TX.
           DISPLAY LB-TX(1:5) AT 0000.
       T.  DISPLAY "<ret>= Buchung ok!, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023376208" WH-CREG.
           IF ESC GO X.
           IF WOLI GO R.
           IF NOT RET GO T.
           PERFORM RESTZEIL.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** Lieferanten lesen *
       READ-LFT SECTION.
       A.  MOVE WH-NUM TO KR-KTONR.
           READ KREDIT NO LOCK NOT INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "PAKOKRED" USING "21KREDAN" WH-CREG.
           CANCEL "PAKOKRED".
           IF ESC GO Z.
       X.  MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV5" WH-CREG.
       Z.  EXIT.
      ******************************************** Scheinrechnungskopf *
       SCHEINRECH SECTION.
       A.  MOVE 0 TO SK-SA LB-LFD.
           MOVE LB-LIEFER TO SK-LIEFER.
           MOVE LB-BELDAT TO SK-BELDAT.
           MOVE LB-BELNR TO SK-BELNR.
       C.  READ SREKOPF INVALID MOVE 0 TO SK-LFD.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE KR-MCODE TO SK-MCODE.
           MOVE KR-BEZ TO SK-BEZ.
           ADD 1 TO SK-LFD.
           ADD LB-BET TO SK-BET.
           REWRITE SK-SATZ INVALID WRITE SK-SATZ.
           MOVE SK-LFD TO LB-SKLFD.
       Z.  EXIT.
      ****************************** Kontensalden updaten und anzeigen *
       UPSALDO SECTION.
       A.  MOVE LG-STAND TO WK-UMS.
           ADD LB-MENGE TO WK-UMS WK-KSBET.
           PERFORM SALDO.
       Z.  EXIT.
      ******************************************** Ausgabe Kontensaldo *
       SALDO SECTION.
       A.  MOVE WK-UMS TO WD-SALDO.
           DISPLAY "Stand: " AT 0410.
           DISPLAY WD-SALDO with highlight AT 0417.
           IF WU = 0 GO Z.
           DISPLAY "KOA: " AT 0435.
           MOVE WK-KSBET TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0442.
           MOVE WK-KSMG TO WD-MENG.
           DISPLAY WD-MENGE with highlight AT 0460.
       Z.  EXIT.
      **************************************** Buchungszeile speichern *
       LB-WRITE SECTION.
       A.  IF IX > 1 MOVE WT-JKEY(IX - 1) TO LB-JKEY.
       B.  MOVE WE-USE TO LB-USE.
           ADD 1 TO LB-SEITE.
           MOVE LB-SATZ TO WH-BZSATZ.
           MOVE LB-KEY TO WH-BKEY.
           MOVE 253 TO LB-LFD.
           START LAGERBZ KEY < LB-KEY INVALID MOVE 1 TO WH-LFD GO E.
       C.  READ LAGERBZ PREVIOUS WITH NO LOCK AT END
               MOVE 1 TO WH-LFD GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 1 LB-LFD TO WH-LFD.
           IF LB-KTONR NOT = WH-KTONR OR LB-BUDAT NOT = WH-BUDAT
               MOVE 1 TO WH-LFD.
       E.  MOVE WH-BZSATZ TO LB-SATZ.
           IF SR PERFORM SCHEINRECH.
           MOVE WH-BKEY TO LB-KEY.
           WRITE LB-SATZ INVALID GO B.
           IF KL-JOKEY = LOW-VALUE MOVE LB-JKEY TO KL-JOKEY.
           PERFORM JOUR-DIS.
       Z.  EXIT.
      ************************************************** Ust-errechnen *
       RECH-UST SECTION.
       A.  MOVE 0 TO WH-NUM WH-WERT.
           DIVIDE 100 INTO WH-UST GIVING WC-UST.
           ADD 1 TO WC-UST.
           EVALUATE WU
               WHEN 0 DIVIDE WC-UST INTO WK-BRUTTO GIVING WK-BASIS
                      COMPUTE WK-MWST = (WH-UST / 100) * WK-BASIS
               WHEN 1 MOVE WK-BRUTTO TO WK-BASIS
                      COMPUTE WK-MWST = (WH-UST / 100) * WK-BASIS
               WHEN 2 DISPLAY "<esc>=Abbruch, <ret>= Steuer" AT 2301
                      MOVE 0 TO WH-WERT
                      COMPUTE VDU-LP = IY * 200 + 267
                      CALL "CAUP" USING "1000007211" WH-CREG
                      MOVE WH-WERT TO WK-MWST.
           MOVE 67 TO VDU-P.
           MOVE WK-MWST TO WD-UST.
           DISPLAY WD-UST with highlight AT VDU-LP.
           SUBTRACT WK-MWST FROM WK-BRUTTO GIVING WK-NETTO.
       Z.  EXIT.
      ******************************* Summenbildung Sachkonten Kat.= 0 *
       RESTZEIL SECTION.
       A.  MOVE WK-KTONR TO LG-NUM.
           IF WM-L = 0 GO C.
           READ LAGARTIK INVALID PERFORM KTO-FEHL.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO A.
       C.  MOVE WK-UMS TO LG-STAND.
           IF LB-BET = 0
               MULTIPLY LB-MENGE BY LB-PREIS GIVING LB-BET ROUNDED
               IF LB-RAB(1) NOT = 0
                   DIVIDE 100 INTO LB-BET GIVING WH-WERT
                   MULTIPLY WH-WERT BY LB-RAB(1) GIVING WH-WERT ROUNDED
                   SUBTRACT WH-WERT FROM LB-BET
               end-if
               IF LB-RAB(2) NOT = 0
                   DIVIDE 100 INTO LB-BET GIVING WH-WERT
                   MULTIPLY WH-WERT BY LB-RAB(2) GIVING WH-WERT ROUNDED
                   SUBTRACT WH-WERT FROM LB-BET.
           IF LB-MENGE < 0 OR LB-SYM = 1 GO H.
           IF LB-PREIS NOT = LG-BTTO(1) OR LB-RAB(1) NOT = LG-RAB(1)
               OR LB-RAB(2) NOT = LG-ZRAB(1)
               PERFORM VARYING WX FROM 6 BY -1 UNTIL WX = 1
                   MOVE LG-TAB(WX - 1) TO LG-TAB(WX)
               end-perform
           else GO H.
      *-----------------------------------------------------------------
           MOVE LB-PREIS TO LG-BTTO(1) LG-NTTO(1).
           MOVE LB-RAB(1) TO LG-RAB(1).
           MOVE LB-RAB(2) TO LG-ZRAB(1).
           MOVE LB-TX TO LG-LIEF(1).
           MOVE LB-SYM TO LG-SYM(1).
           MOVE LB-BUDAT TO LG-DATUM(1).
           SUBTRACT LB-RAB(1) FROM 100 GIVING WH-TEIL.
           IF LB-RAB(1) NOT = 0 DIVIDE 100 INTO WH-TEIL
               MULTIPLY LG-NTTO(1) BY WH-TEIL GIVING LG-NTTO(1) ROUNDED.
           SUBTRACT LB-RAB(2) FROM 100 GIVING WH-TEIL.
           IF LB-RAB(2) NOT = 0 DIVIDE 100 INTO WH-TEIL
               MULTIPLY LG-NTTO(1) BY WH-TEIL GIVING LG-NTTO(1) ROUNDED.
           DIVIDE LG-PREIS INTO LG-NTTO(1) GIVING WH-TEIL.
           COMPUTE WH-DIF = (WH-TEIL - 1) * 100
           IF WH-DIF < 5,00 GO H.
           CALL "CAUP" USING "0710060368000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Achtung        % Preisabweichung! <Einf>= ok, <esc>=
      -        " Abbruch < >" AT VDU-LP.
           MOVE WH-DIF TO WD-DIF.
           ADD 211 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-DIF with highlight BLINK AT VDU-LP.
           MOVE LG-NTTO(1) TO LG-PREIS.
       G.  CALL "CAUP" USING "1002650000" WH-CREG.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG GO Z.
           IF ESC GO Z.
           IF NOT EINF GO G.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       H.  IF LB-SYM = 3 MOVE LG-NTTO(1) TO LG-PREIS
               MOVE LG-LIEF(1) TO LG-LFT.
           REWRITE LG-SATZ.
      *-------------------------------------> Journalsummen speichern <-
           ADD WE-USE 1 GIVING WH-KEY.
       Q.  READ KONSLAG INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Q.
           ADD LB-MENGE TO KL-JSUM.
           MOVE LB-BUDAT TO KL-BUDAT.
           REWRITE KL-SATZ.
       Z.  EXIT.
      ******************************************************************
       JOUR-DIS SECTION.
       A.  IF IX = 0 GO Z.
       C.  IF IX > 16 PERFORM B IN BS-JZEIL GO C.
           MOVE LB-KTONR TO WD-KTO.
           COMPUTE VDU-LP = IX * 100 + 402 + VDU-ECK.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           MOVE LB-BUDAT TO WC-DATUM.
           ADD 8 TO VDU-P.
           PERFORM BU-TEIL.
           MOVE LB-MENGE TO WD-SALDO.
           COMPUTE VDU-LP = IX * 100 + 453 + VDU-ECK.
           MOVE LB-PREIS TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           ADD 12 TO VDU-LP.
           IF LB-RAB(1) NOT = 0 MOVE LB-RAB(1) TO WD-RAB
               DISPLAY WD-RAB AT VDU-LP.
           ADD 6 TO VDU-LP.
           IF LB-RAB(2) NOT = 0 MOVE LB-RAB(2) TO WD-RAB
               DISPLAY WD-RAB AT VDU-LP.
           MOVE LB-JKEY TO WH-JOKEY WT-JKEY(IX).
           ADD 1 TO WR.
       Z.  EXIT.
      **************************************** Datum - Symbol - Ggkto. *
       BU-TEIL SECTION.
       A.  PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-P.
           PERFORM SYMDIS.
           ADD 3 TO VDU-P.
           IF LB-SRECH NOT = SPACE MOVE LB-BELNR TO WD-BELNR
               DISPLAY WD-BELNR with highlight AT VDU-LP
               ADD 8 TO VDU-P
               MOVE LB-BELDAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP
           else IF LB-SYM = 4 DISPLAY "  -- Waage --" AT VDU-LP
                else DISPLAY LB-TX AT VDU-LP
                end-if
                ADD 8 TO VDU-P.
           ADD 9 TO VDU-P.
           MOVE LB-MENGE TO WH-MENGE.
           PERFORM WD-MG.
           ADD 11 TO VDU-LP.
           DISPLAY WT-MEH(LB-MEH) AT VDU-LP.
           ADD 3 TO VDU-P.
       Z.  EXIT.
      ********************************* letzte Buchung je Konto suchen *
       LAST-BUCH SECTION.
       A.  MOVE 1 TO IY.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE WK-KTONR TO LB-KTONR.
           MOVE 999999 TO LB-BUDAT.
           START LAGERBZ KEY < LB-KEY INVALID GO C.
       B.  READ LAGERBZ PREVIOUS NO LOCK AT END GO C.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF LB-KTONR = WK-KTONR CALL "CAUP" USING "16CLRFEN" WH-CREG
               PERFORM BUCH-DIS
               GO Z.
       C.  PERFORM INIT-BZ.
           MOVE WK-KTONR TO LB-KTONR.
           MOVE 0 TO IY.
       Z.  EXIT.
      ************************************** Buchung je Konto anzeigen *
       BUCH-DIS SECTION.
       A.  IF IY = 0 GO Z.
           COMPUTE VDU-LP = IY * 200 + 202 + VDU-ECK.
           IF LB-SYM = 99 MOVE LB-BELDAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               MOVE LB-SOLL TO WD-SALDO
               ADD 20 TO VDU-LP
               DISPLAY "S: " AT VDU-LP WD-SALDO with highlight
               MOVE LB-HABEN TO WD-SALDO
               ADD 25 TO VDU-LP
               DISPLAY " H: " AT VDU-LP WD-SALDO with highlight
                    " Saldo: "
               SUBTRACT LB-HABEN FROM LB-SOLL GIVING WD-SALDO
               DISPLAY WD-SALDO with highlight at 0000
               ADD 100 TO VDU-LP
               MOVE 2 TO VDU-LP
               DISPLAY "Eb-VJ: " AT VDU-LP
               MOVE LB-EBIL TO WD-SALDO
               DISPLAY WD-SALDO with highlight AT 0000 "  "
                   "** Wirtschaftsjahrende **" AT 0000
               GO X.
           IF LB-SYM = 98 MOVE LB-BELDAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               ADD 9 TO VDU-LP
               DISPLAY "Saldovortrag" AT VDU-LP
               ADD 27 TO VDU-LP
               MOVE LB-MENGE TO WD-SALDO
               DISPLAY WD-SALDO with highlight at VDU-LP
               ADD 100 TO VDU-LP
               MOVE 2 TO VDU-LP
      *        DISPLAY "** autom. Saldovortrag **" AT VDU-LP
               GO X.
           MOVE LB-BUDAT TO WC-DATUM.
           PERFORM BU-TEIL.
           MOVE LB-PREIS TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           ADD 12 TO VDU-LP.
           IF LB-RAB(1) NOT = 0 MOVE LB-RAB(1) TO WD-RAB
               DISPLAY WD-RAB AT VDU-LP.
           ADD 6 TO VDU-LP.
           IF LB-RAB(2) NOT = 0 MOVE LB-RAB(2) TO WD-RAB
               DISPLAY WD-RAB AT VDU-LP.
           ADD 6 TO VDU-LP.
           IF LB-LIEFER NOT = 0 and LB-LIEFER(1:) NOT = SPACE
               MOVE LB-LIEFER TO WD-KTO
               DISPLAY WD-KTO with highlight AT VDU-LP "/"
               DISPLAY LB-TX(1:5) AT 0000.
           ADD 100 TO VDU-LP.
           MOVE 3 TO VDU-P.
           IF LB-USE > 10 OR LB-USE = 0
               DIVIDE 100 INTO LB-SEITE GIVING WD-JONUM
               DISPLAY WD-JONUM with foreground-color 2 AT VDU-LP " Stan
      -            "d: " with foreground-color 2
                    WD-STAND with foreground-color 2
           else MOVE LB-SEITE TO WD-JONUM
               DISPLAY WD-JONUM with foreground-color 2 AT VDU-LP
                  "-" with foreground-color 2
                  LB-USE with foreground-color 2
                  " Stand: " with foreground-color 2
                  WD-STAND with foreground-color 2.
      *--------------------------------------> letzten Preis anzeigen <-
       X.  MOVE LB-KEY TO WH-BKEY WT-KEY(IY).
           ADD 1 TO WB.
       Z.  EXIT.
      ****************************************** Lesen ohne Satzsperre *
       LIESKTO-NOLOCK SECTION.
       A.  PERFORM LIESKTO.
           UNLOCK LAGARTIK.
           MOVE 1 TO WM-L.                 *> Flag f. freigegebens Konto
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  SET RET TO TRUE.
           MOVE 0 TO WM-L.                   *> Flag f. gesperrtes Konto
           IF WH-NUM = 0 GO X.
           MOVE WH-NUM TO WD-KZ.
           MOVE WD-KZ TO WH-KAT.
           MOVE WH-NUM TO LG-NUM WD-KTO.
      *---------------------------------------------> lesen Kostkonto <-
       C.  READ LAGARTIK NOT INVALID MOVE LG-BEZ TO WT-TX
               MOVE LG-STAND TO WE-UMS
               MOVE LG-NUM TO WH-NUM
               MOVE LG-STAND TO WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO X.
           IF WH-AKAT = 1 SET ESC TO TRUE GO X.
           CALL "PARKOMKA" USING "51KOMP" WH-CREG.
           CANCEL "PARKOMKA".
           IF LG-NUM = 0 GO X ELSE GO C.
       X.  SET FEHLER TO TRUE.
       Z.  EXIT.
      ***************************************** Druck Schattenrechnung *
       SR-DRUCK SECTION 70.
       A.  CALL "CAUP" USING "0718250330000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Druck lÑuft - bitte warten" with highlight
               AT VDU-LP.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WE-LIEFER WE-BELNR WE-BELDAT.
           MOVE 0 TO LB-SKLFD WH-BET.
           MOVE SK-LIEFER TO LB-LIEFER.
           MOVE SK-BELNR  TO LB-BELNR.
           MOVE SK-BELDAT TO LB-BELDAT.
           START LAGERBZ KEY NOT < LB-SRECH INVALID PERFORM NO-REC GO X.
       C.  READ LAGERBZ NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LB-LIEFER NOT = SK-LIEFER OR LB-BELNR NOT = SK-BELNR
               OR LB-BELDAT NOT = SK-BELDAT GO W.
           MOVE LB-KTONR TO LG-NUM.
       E.  READ LAGARTIK NO LOCK INVALID MOVE "Artikel fehlt" TO LG-BEZ.
           IF ZUGRIF PERFORM BESETZT GO E.
           PERFORM DRUKO-SR.
           MOVE LG-NUM TO DRS-ARTNR.
           MOVE LG-BEZ TO DRS-BEZ.
           MOVE LB-MENGE TO WH-MENGE.
           EVALUATE LB-NK
               WHEN 2 MOVE WH-MENGE TO WD-MENGA
                      MOVE WD-MENGA(1:) TO WD-MENGE
               WHEN 1 MOVE WH-MENGE TO WD-MENGB
                      MOVE WD-MENGB(1:) TO WD-MENGE
               WHEN 0 MOVE WH-MENGE TO WD-MENGC
                      MOVE WD-MENGC(1:) TO WD-MENGE.
           MOVE WD-MENGE TO DRS-MENGE.
           MOVE WT-MEH(LB-MEH) TO DRS-MEH.
           IF LB-PREIS NOT = 0 MOVE LB-PREIS TO DRS-PREIS
               IF LB-RAB(1) NOT = 0 MOVE LB-RAB(1) TO DRS-RAB(1) end-if
               IF LB-RAB(2) NOT = 0 MOVE LB-RAB(1) TO DRS-RAB(2) end-if
           MOVE LB-BET TO DRS-BETRAG.
           ADD LB-BET TO WH-BET.
           PERFORM DRUCK.
           GO C.
       W.  MOVE ALL "ƒ" TO DRS-BETRAG(1:) PERFORM DRUCK.
           MOVE "S u m m e  ohne UST" TO DRS-BEZ.
           MOVE WH-BET TO DRS-BETRAG.
           PERFORM DRUCK.
           DELETE SREKOPF.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           PERFORM END-DRU.
       Z.  EXIT.
      ************************************* Kopfdruck Schattenrechnung *
       DRUKO-SR SECTION 70.
       A.  IF WZ-ZEILEN > 63;
               IF WZ-SEITE > 1 MOVE "öbertrag" TO DRS-BEZ
                   MOVE WH-BET TO DRS-BETRAG
                   PERFORM DRUCK
               end-if
               WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "LPT1:" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE 2 TO WZ-SCHALT.
           MOVE "Schattenrechnung:         vom:
      -        "         Seite:     / " TO DRS-STR.
           MOVE SK-BELNR TO WD-BELNR.
           MOVE WD-BELNR TO DRS-STR(19:7).
           MOVE LB-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-STR(32:8).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-TG.
           MOVE WD-TG TO DRA-STR(72:3).
           MOVE LB-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-STR(78:8).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE KR-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           MOVE WT-BEZ TO DRS-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRS-STR PERFORM DRUCK.
           IF WZ-SEITE > 1 MOVE "öbertrag" TO DRS-BEZ
               MOVE WH-BET TO DRS-BETRAG
               PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       READBZ SECTION 70.
       A.  MOVE WH-BKEY TO LB-KEY.
       C.  START LAGERBZ KEY NOT < LB-KEY INVALID GO X.
           READ LAGERBZ NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LB-KTONR = WK-KTONR PERFORM BUCH-DIS GO Z.
       X.  MOVE LOW-VALUE TO WT-ERFTAB.
       Z.  EXIT.
      **************************************************** weiterlesen *
       ABBUZEI SECTION 70.
       A.  PERFORM VARYING IY FROM 8 BY -1 UNTIL IY = 0
               OR WT-KEY(IY) NOT = LOW-VALUE CONTINUE.
           IF IY = 0 MOVE 1 TO IY SET FINE TO TRUE GO X.
       C.  MOVE WT-KEY(IY) TO WH-BKEY.
           IF AB OR (BAB AND WH-NUM = 0) PERFORM READBZ
               ADD 1 TO IY
               MOVE 0 TO WI WB
               IF AB MOVE 6 TO WB.
       E.  PERFORM NEXTABZ WITH TEST AFTER UNTIL WI = 8 OR WB = 7.
           IF WI NOT = 8 MOVE 9 TO WI.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTABZ SECTION 70.
       E.  READ LAGERBZ NEXT AT END MOVE 8 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WK-KTONR NOT = LB-KTONR MOVE 8 TO WI
               SET FINE TO TRUE GO Z.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 7
               IF LB-KEY = WT-KEY(WF) GO E.
           IF IY = 9 CALL "CAUP" USING "17SCRAUF" WH-CREG
                     CALL "CAUP" USING "17SCRAUF" WH-CREG
               PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 7
                   MOVE WT-KEY(IY + 1) TO WT-KEY(IY)
                   END-PERFORM.
           PERFORM BUCH-DIS.
           IF BAB ADD 1 TO IY.
       Z.  EXIT.
      **************************************************** Kontozeilen *
       AUFBUZEI SECTION 70.
       A.  IF WT-KEY(1) = LOW-VALUE SET FINE TO TRUE GO X.
           MOVE WT-KEY(1) TO WH-JOKEY LB-KEY.
           MOVE 0 TO WB.
           MOVE IY TO WX.
           IF AUF MOVE 6 TO WB.
           START LAGERBZ KEY < LB-KEY INVALID SET FINE TO TRUE GO X.
           MOVE 1 TO IY WI.
           PERFORM NEXTAUBZ WITH TEST AFTER UNTIL WI = 2 OR WB = 7.
           MOVE 1 TO IY.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAUBZ SECTION 70.
       N.  READ LAGERBZ PREVIOUS AT END MOVE 2 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF WK-KTONR NOT = LB-KTONR SET FINE TO TRUE
               MOVE 2 TO WI GO Z.
           IF WI > 8 PERFORM VARYING WF FROM 16 BY -1 UNTIL WF < 1
               IF LB-KEY = WT-KEY(WF) GO N.
           MOVE VDU-LP TO VDU-AB.
           MOVE 1400 TO VDU-LP.
           CALL "CAUP" USING "18CLRWIN" WH-CREG.
           MOVE VDU-AB TO VDU-LP.
           CALL "CAUP" USING "22SCRAB" WH-CREG.
           CALL "CAUP" USING "22SCRAB" WH-CREG.
           PERFORM VARYING IY FROM 8 BY -1 UNTIL IY < 2
              MOVE WT-KEY(IY - 1) TO WT-KEY(IY).
           COMPUTE VDU-LP = IY * 100 + 500.
           PERFORM BUCH-DIS.
       Z.  EXIT.
      ******************************************************************
       LAST-SRBU SECTION 80.
       A.  MOVE 1 TO IX.
           MOVE LOW-VALUE TO WT-JOTAB.
           MOVE WE-LIEFER TO LB-LIEFER.
           MOVE WE-BELNR TO LB-BELNR.
           MOVE WE-BELDAT TO LB-BELDAT.
           MOVE 9999 TO LB-SKLFD.
           START LAGERBZ KEY < LB-SRECH INVALID MOVE 99 TO LB-USE GO C.
       B.  READ LAGERBZ PREVIOUS NO LOCK AT END MOVE 99 TO LB-USE GO C.
           IF ZUGRIF PERFORM BESETZT GO B.
       C.  IF WE-USE = LB-USE AND WE-LIEFER = LB-LIEFER
               AND WE-BELNR = LB-BELNR AND WE-BELDAT = LB-BELDAT
               CALL "CAUP" USING "16CLREFN" WH-CREG
               PERFORM JOUR-DIS
               GO Z.
           PERFORM INIT-BZ.
           MOVE 0 TO IX.
       Z.  EXIT.
      ******************************************************************
       LAST-JOUR SECTION 80.
       A.  IF SR PERFORM LAST-SRBU GO Z.
           MOVE 1 TO IX.
           MOVE LOW-VALUE TO WT-JOTAB.
           MOVE 999999 TO LB-SEITE.
           MOVE WE-USE TO LB-USE.
           START LAGERBZ KEY < LB-JKEY INVALID MOVE 99 TO LB-USE GO C.
       B.  READ LAGERBZ PREVIOUS NO LOCK AT END MOVE 99 TO LB-USE GO C.
           IF ZUGRIF PERFORM BESETZT GO B.
       C.  IF WE-USE = LB-USE CALL "CAUP" USING "16CLREFN" WH-CREG
               PERFORM JOUR-DIS
               GO Z.
           PERFORM INIT-BZ.
           MOVE 0 TO IX.
       Z.  EXIT.
      ******************************************************************
       FIRST-JOUR SECTION 80.
       A.  MOVE WE-USE TO LB-USE.
           MOVE 0 TO LB-SEITE.
           IF BAUF MOVE WH-NUM TO LB-SEITE GO B.
           IF WH-NUM NOT = 0 MOVE WH-NUM TO WD-BELNR
               DISPLAY "Belegnr.: " AT 2120 WD-BELNR with highlight " "
                   "wird gesucht. - " "Bitte warten" with BLINK.
       B.  START LAGERBZ KEY NOT < LB-JKEY INVALID
               MOVE 0 TO LB-SEITE WH-NUM
               IF BAUF SET RET TO TRUE GO A
               else GO E.
       C.  READ LAGERBZ NEXT NO LOCK AT END MOVE 0 TO LB-SEITE
               IF WH-NUM NOT = 0 MOVE 0 TO WH-NUM GO A.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BAUF MOVE LB-BELNR TO WH-NUM.
       E.  IF LB-USE NOT = WE-USE SET ESC TO TRUE GO Z.
           IF WH-NUM NOT = 0; IF LB-BELNR NOT = WH-NUM GO C
                              else MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE LOW-VALUE TO WT-JOTAB.
           MOVE 1 TO IX.
           MOVE LB-JKEY TO WT-JKEY(IX).
           PERFORM JOUR-DIS.
       Z.  EXIT.
      ******************************************************************
       TX-KORR SECTION 80.
       A.  ADD 1 TO VDU-L.
       B.  MOVE LB-TX TO WT-TX.
           CALL "CAUP" USING "1200090110" WH-CREG.
           IF ESC MOVE LB-TX TO WT-TX
               SET RET TO TRUE.
           IF NOT RET GO B.
           MOVE WT-TX TO LB-TX.
           REWRITE LB-SATZ.
           DISPLAY LB-TX with reverse-video SIZE 10 AT VDU-LP.
           SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ******************************************************************
       KTO-FEHL SECTION 80.
       A.  DISPLAY "Konto fehlt oder fehlerhaft" AT 2401.
           PERFORM WEITER.
           STOP RUN.
       Z.  EXIT.
      ************************************************ PeriodenprÅfung *
       P-PRUEF SECTION 80.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           ADD WE-USE 1 GIVING WH-KEY.
       F.  READ KONSLAG INVALID INITIALIZE KL-BSATZ
               MOVE WH-KEY TO KL-NUM WRITE KL-SATZ GO F.
           IF ZUGRIF STOP RUN.
           ADD 401 VDU-ECK GIVING VDU-LP.
           SET RET TO TRUE.
           IF WL-CA = 30 MOVE 1 TO WE-BSR else MOVE 0 TO WE-BSR.
      *-----------------------------------------------------------------
           IF SR DISPLAY " Art.Nr. Bu-Datum Sy  Bel-Nr.- Datum      Meng
      -              "e Meh      Preis  Rab.% Rab.% "
                 with background-color 1 highlight AT VDU-LP
            else DISPLAY " Art.Nr. Bu-Datum Sy  Bel-Nr.- Datum      Meng
      -              "e Meh      Preis  Rab.% Rab.% "
                 with background-color 6 highlight AT VDU-LP.
           MOVE 0 TO WE-BSR.
           PERFORM LAST-JOUR.
           MOVE WM-DATUM TO WZ-DATUM.
           MOVE LB-BUDAT TO WC-DATUM WE-PER.
           IF LB-BUDAT = 0 MOVE WM-DATUM TO WE-PER KL-BUDAT GO W.
           IF WC-DATUM(1:4) = WZ-DATUM(1:4) GO W.
           CALL "CAUP" USING "0706070565000" WH-CREG.
           ADD 205 VDU-ECK GIVING VDU-LP.
           PERFORM DATDREH.
           DISPLAY "Buchungsdatum: " AT VDU-LP VDU-DATUM with highlight
               " stimmt mit " "      (= Bu-Datum)" with highlight.
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Buchungsperiode: " AT VDU-LP VDU-DATUM
                with highlight " nicht Åberein!" "   (= Tagesdatum)"
                 with highlight.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bitte Tagesdatum Pkt. 8 Ñndern oder Bu-Datum ÅberprÅ
      -        "fen! < >" AT VDU-LP.
           CALL "CAUP" USING "1004610000" WH-CREG.
           SET ESC TO TRUE.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       W.  IF WE-BELNR = 0 MOVE LB-BELNR TO WE-BELNR.
           MOVE 0 TO WM-M WM-G.
       Z.  EXIT.
      ***************************************************** Verwiegung *
       VERWIEG SECTION 90.
       A.  PERFORM WAAG-SYNC.
           IF ESC GO Z.
           OPEN INPUT REZEPT.
           OPEN I-O PROTWAAG.
           MOVE 0 TO WZ-ZEILEN WM-OPEN.
           IF WM-WAAG = 1 MOVE " Verwiegung OHNE Waage" TO WK-GEB
                     else MOVE " Verwiegung MIT Waage" TO WK-GEB.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Rezept-Nr.: " AT 0301.
           DISPLAY "<esc>= Abbruch, alpha+<ret>= suchen, <#>= letzte Rez
      -        "eptur wiederholen" AT 2301.
           DISPLAY "0+<ret>= man. Entnahme (ohne Rezeptur)"AT 2401.
           CALL "CAUP" USING "0003135005" WH-CREG.
           IF ESC GO X.
           IF KIST GO C.
           IF NOT RET GO B.
           MOVE 0 TO PWA-RENUM PWA-SA PWA-ZEIT.
      *--------------------------------------> 0+<ret>= man. Entnahme <-
           IF WH-NUM = 0 AND WH-MCODE(1:1) = "0"
               MOVE WM-DATUM TO PWA-DAT
               MOVE 99999 TO PWA-RENUM
               MOVE "Entnahme o. Rz" TO PWA-RPBEZ
               MOVE 0 TO PWA-ANZ
               DISPLAY "Entnahme ohne Rezeptur" with highlight AT VDU-LP
               INITIALIZE RE-SATZ GO H.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PANANZ" USING "40RPSUCH" WH-CREG
               CANCEL "PANANZ".
           MOVE WH-NUM TO RE-NUM.
       C.  IF RE-NUM = 0 GO B.
           MOVE RE-NUM TO WD-NUM PWA-RENUM.
           MOVE WM-DATUM TO PWA-DAT.
       E.  READ REZEPT INVALID PERFORM NO-REC GO B.
           IF ZUGRIF PERFORM BESETZT GO E.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           ADD 7 TO VDU-LP.
           DISPLAY RE-BEZ with highlight AT VDU-LP.
           MOVE RE-BEZ TO PWA-RPBEZ.
       G.  DISPLAY "Anzahl:" AT 0345.
           DISPLAY "<esc>= Abbruch, <ret>= Anzahl" AT 2301.
           CALL "CAUP" USING "0003535005" WH-CREG.
           IF ESC OR WOLI GO B.
           IF NOT RET GO G.
           MOVE WH-WERT TO WD-MENGC WH-ANZ PWA-ANZ.
           DISPLAY WD-MENGC with highlight AT VDU-LP.
           MOVE 0 TO IY.
           MOVE RE-ART TO WH-TART.
      *---- 0 ----> Menge = WH-ANZ * RE-TEIL * RE-MENGE(XR) / RE-GEW <-
      *---- 1 ----> Menge = WH-ANZ / RE-TEIL <-------------------------
      *    IF WH-RART = 1 COMPUTE WH-ATG ROUNDED = WH-ANZ / RE-TEIL
      *       else COMPUTE WH-ATG ROUNDED = (WH-ANZ * RE-TEIL) / RE-GEW.
           IF WH-RART = 1 COMPUTE WH-ATG ROUNDED = WH-ANZ / RE-TEIL
           else COMPUTE WH-ATG ROUNDED =
               WH-ANZ * RE-TEIL / RE-GEW + 0,0002127.
       H.  PERFORM VARYING RZ FROM 1 BY 1 UNTIL RZ > 20
               MOVE 0 TO PWA-LGNUM(RZ) PWA-SOLL(RZ) PWA-IST(RZ)
               MOVE SPACE TO PWA-KOBEZ(RZ).
           MOVE 0 TO RZ.
       I.  PERFORM VARYING XR FROM 1 BY 1 UNTIL XR > 20
               MOVE RE-TNAM(XR) TO WH-TNAM
               IF RE-KONUM(XR) NOT = 0 and WH-A = 0 and WH-T NOT = 5
                   MOVE RE-KONUM(XR) TO WD-KOMPO
                   ADD 1 TO IY
                   COMPUTE VDU-LP = IY * 100 + 302
                   DISPLAY WD-KOMPO AT VDU-LP
                   ADD 7 TO VDU-P
                   DISPLAY RE-KOBEZ(XR) with highlight AT VDU-LP
                   ADD 1 WH-M GIVING WH-MEH
                   ADD 26 TO VDU-P
                   COMPUTE WH-STK ROUNDED = WH-ATG * RE-MENGE(XR)
      *                WH-ATG * RE-MENGE(XR) * WH-ANZ
                   DIVIDE 1000 INTO WH-STK GIVING WD-GEW
                   DISPLAY WD-GEW with highlight AT VDU-LP
                   ADD 8 TO VDU-P
                   DISPLAY "kg" AT VDU-LP
                   ADD 1 TO RZ
                   MOVE RE-KONUM(XR) TO PWA-LGNUM(RZ)
                   MOVE RE-KOBEZ(XR) TO PWA-KOBEZ(RZ)
                   MOVE WH-STK TO PWA-SOLL(RZ).
           MOVE 1 TO RZ.
           MOVE 0 TO WY.
       K.  DISPLAY "<Bild- >= Tara, <ret>= Gewicht ok, < / >= auf/ab, <E
      -        "nde>= beenden" AT 2301.
           IF WM-WAAG = 1 AND PWA-RENUM = 0
               DISPLAY "erst Komponente mit <Einfg> anwÑhlen"
                    with highlight AT 2401.
           COMPUTE VDU-LP = RZ * 100 + 349.
           MOVE PWA-IST(RZ) TO WH-WERT.
           DIVIDE 1000 INTO WH-WERT GIVING WD-GEW.
           IF WH-WERT = 0 CALL "CAUP" USING "1300000080" WH-CREG
                     else DISPLAY WD-GEW with highlight AT VDU-LP.
           CALL "CAUP" USING "1000006008" WH-CREG.
           IF ESC GO B.
           IF WOLI AND WY = 0 GO G.
           IF BAB PERFORM TARA GO K.
           IF ENDE PERFORM DRU-PROT GO B.
           IF AUF; IF RZ > 1 ADD -1 TO RZ GO K.
           IF AB; IF PWA-LGNUM(RZ) NOT = 0 ADD 1 TO RZ GO K.
           IF EINF PERFORM MAN-KOMP GO K.
           IF NOT RET GO K.
           IF PWA-LGNUM(RZ) = 0 MOVE 10 TO VDU-P
               DISPLAY "Keine Komponente angewÑhlt!" with BLINK
                   highlight AT VDU-LP
               PERFORM WEITER
               CALL "CAUP" USING "1300010080" WH-CREG
               GO K.
      *------------------------------------------> Gewicht Åbernehmen <-
           IF RET AND WH-NUM NOT = 0 MOVE 1 TO WM-EIN
               MOVE WH-WERT TO WC-GRAMM GO L.
           MOVE 0 TO WM-EIN.
           IF RET PERFORM HOL-WAAG
               IF ESC GO K.
       L.  IF WC-GRAMM = 0 GO K.
           MOVE WC-GRAMM TO PWA-IST(RZ).
           IF PWA-RENUM = 0 MOVE WC-GRAMM TO PWA-SOLL(RZ).
           DIVIDE 1000 INTO WC-GRAMM GIVING WD-GEW.
           COMPUTE VDU-LP = RZ * 100 + 349.
           DISPLAY WD-GEW with highlight AT VDU-LP "kg".
           IF WM-EIN = 1 DISPLAY " - Eingabe" AT 0000.
           ACCEPT WH-ZEIT FROM TIME.
           DIVIDE 10000 INTO WH-ZEIT GIVING PWA-ZEIT.
           MOVE 1 TO WY.
           ADD 1 TO RZ.
           GO K.
       X.  CLOSE REZEPT.
           CLOSE PROTWAAG.
       Z.  EXIT.
      ******************************************* man. Komponentenwahl *
       MAN-KOMP SECTION 90.
       A.  IF PWA-LGNUM(RZ) NOT = 0 GO Z.
           IF WH-NUM = 0 AND WH-MCODE NOT = SPACE
               CALL "PANANZ" USING "30SUCH" WH-CREG.
           IF WH-NUM = 0 SET ESC TO TRUE GO Z.
           PERFORM LIESKTO-NOLOCK.
           MOVE LG-NUM TO WD-KOMPO PWA-LGNUM(RZ).
           COMPUTE VDU-LP = RZ * 100 + 302.
           CALL "CAUP" USING "1300010080" WH-CREG.
           DISPLAY WD-KOMPO AT VDU-LP.
           ADD 7 TO VDU-P.
           DISPLAY LG-BEZ with highlight AT VDU-LP.
           MOVE LG-BEZ TO PWA-KOBEZ(RZ).
           MOVE 0 TO PWA-SOLL(RZ) PWA-IST(RZ).
       Z.  EXIT.
      ****************************************************** Tarierung *
       TARA SECTION 90.
       A.  CALL "mfclink" USING "INIT" WH-RETC WK-CHAN WK-BAUD.
           MOVE LOW-VALUES TO WK-RTS WK-DTR.
           CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR
           MOVE x"540D0A" TO E-FELD.
           MOVE 3 TO E-LENG.
           CALL "mfclink" USING "SEND" WH-RETC WK-CHAN E-FELD E-LENG.
       Z.  EXIT.
      ***************************************************** Verwiegung *
       HOL-WAAG SECTION 90.
       A.  IF WM-WAAG = 1 DISPLAY "OHNE" with highlight AT 2401
               " Waage angewÑhlt!"
               PERFORM WEITER
               SET ESC TO TRUE GO Z.
           MOVE LOW-VALUE TO WK-RTS WK-DTR.
           CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR.
           GO E.
       C.  DISPLAY "<ret>= wenn Waage bereit, <esc>= Abbruch < >"
               AT 2301.
           CALL "CAUP" USING "0023430000" WH-CREG.
           CALL "CAUP" USING "1323012580" WH-CREG.
           IF ENDE SET ESC TO TRUE.
           IF ESC GO Z.
           IF AB GO G.
           IF AUF MOVE LOW-VALUES TO WK-RTS
              MOVE HIGH-VALUES TO WK-DTR
              CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR
              GO C.
           IF WORE MOVE LOW-VALUES TO WK-DTR
              MOVE HIGH-VALUES TO WK-RTS
              CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR
              GO C.
           IF WOLI MOVE LOW-VALUE TO WK-RTS WK-DTR
              CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR
              GO C.
           IF NOT RET GO C.
       E.  MOVE LOW-VALUES TO WK-RTS WK-DTR.
           CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR
           CALL "mfclink" USING "INIT" WH-RETC WK-CHAN WK-BAUD.
           IF WH-RETC = 10 DISPLAY "GerÑt nicht betriebsbereit" AT 2401
               PERFORM WEITER GO C.
      *----------------------------> "SI, cr, lf" = Sendeaufforderung <-
       F.  MOVE x"53490D0A" TO E-FELD.
           MOVE 4 TO E-LENG.
           CALL "mfclink" USING "SEND" WH-RETC WK-CHAN E-FELD E-LENG.
           IF WH-RETC = 10 DISPLAY "Sendetimeout " AT 2401
                           PERFORM WEITER GO E.
           DISPLAY "-- bitte warten --" with highlight AT 2453.
      *-----------------------------------------> Waagedaten auslesen <-
       G.  MOVE 17 TO R-LENG.
           CALL "mfclink" USING "RECV" WH-RETC WK-CHAN E-FELD R-LENG
               E-TIME WH-LF.
           IF WH-RETC = 10 GO C.
           IF E-FELD(2:1) = "D"
               DISPLAY E-FELD(1:20) AT 2220 "R: " WH-RETC "-" R-LENG
               DISPLAY "kein Ruhezustand" with highlight AT 2501
               GO C.
           MOVE E-FELD TO WH-WAAGE.
           IF WA-END NOT = WH-CR
               DISPLAY E-FELD(1:20) AT 2220 "R: " WH-RETC "-" R-LENG
               DISPLAY "Wiegevorgang nicht korrekt!"
                   with highlight AT 2501
               GO C.
      *-----------------------------------------> Wiegedaten ok in gr <-
       I.  INSPECT WA-GEW REPLACING ALL SPACES BY ZERO.
           MOVE 0 TO IZ.
           INSPECT WA-GEW TALLYING IZ FOR ALL "-".
           INSPECT WA-GEW REPLACING ALL "-" BY ZERO.
           COMPUTE WC-GRAMM = WA-GRAM + (WA-GEW * 1000).
           IF IZ = 1 MULTIPLY -1 BY WC-GRAMM.
           CALL "CAUP" USING "1323012580" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       DIREKT SECTION 90.
       A.  MOVE 1 TO WY WX WS.
           MOVE LOW-VALUE TO WT-DAT.
           MOVE SPACE TO WT-TX.
           MOVE x"DA" TO WK-X.                    *> DLAB +BREAK +7,1,E
           CALL X"88" USING WK-3FB WK-X.
           MOVE x"60" TO WK-X.                    *> 1200 baud
           CALL X"88" USING WK-3F8 WK-X.
           MOVE x"00" TO WK-X.                    *> 1200 baud
           CALL X"88" USING WK-3F9 WK-X.
           MOVE x"1A" TO WK-X.                    *> 7,1,E
           CALL X"88" USING WK-3FB WK-X.
           MOVE x"03" TO WK-X.                    *> RTS + DTR setzen
           CALL X"88" USING WK-3FC WK-X.
           MOVE x"49440D0A" TO E-FELD.
           PERFORM VARYING wx FROM 1 by 1 until wx > 4
               move e-feld(wx:1) to wk-x
               CALL X"88" USING WK-3F8 WK-X.
           move space to e-feld.
       D.  CALL "CBL_GET_KBD_STATUS" USING WK-KBD.
           IF WK-KBD NOT = 0 CALL "CBL_READ_KBD_CHAR" USING WK-KBD
               IF WK-KBD = 27 MOVE x"00" TO WK-X
                   CALL X"88" USING WK-3FC WK-X
                   SET ESC TO TRUE GO Z.
           MOVE x"01" TO WK-X.
           CALL X"88" USING WK-3F9 WK-X.
           CALL X"87" USING WK-3FA WK-X.
           IF WK-X NOT = X"04" GO D.              *> ob ein Zeichen da
       C.  CALL X"87" USING WK-3F8 WK-X.          *> COM1 auslesen
           MOVE WK-X TO E-FELD(WX:1).
           ADD 1 TO WX.
           DISPLAY wk-x at 0000.
           IF WK-X = x"0A" CALL "CAUP" USING "17SCROL" WH-CREG
               ADD 403 VDU-ECK GIVING VDU-LP
               DISPLAY E-FELD with highlight AT VDU-LP WD-WS
               MOVE SPACE TO E-FELD
               MOVE 1 TO WX.
           if wx < 80 GO D.
       Z.  EXIT.
      ****************************************** Waage synchronisieren *
       WAAG-SYNC SECTION 90.
       A.  CALL "CAUP" USING "1303012580" WH-CREG.
           PERFORM direkt.
           MOVE LOW-VALUES TO WK-RTS WK-DTR.
           MOVE WE-CHAN TO WK-CHAN.
           CALL "mfclink" USING "INIT" WH-RETC WK-CHAN WK-BAUD.
       C.  CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR.
           DISPLAY "Waageinitialiserung:" AT 1021.
           DISPLAY " " AT 0021.
           MOVE x"49440D0A" TO E-FELD.
           MOVE 4 TO E-LENG.
           CALL "mfclink" USING "SEND" WH-RETC WK-CHAN E-FELD E-LENG.
           MOVE 0 TO WY.
       G.  ADD 1 TO WY.
           MOVE 45 TO R-LENG.
           CALL "mfclink" USING "RECV" WH-RETC WK-CHAN E-FELD R-LENG
               E-TIME WH-LF.
           ADD -1 TO R-LENG GIVING WX.
           MOVE E-FELD(1:WX) TO WR-ADR(WY).
           DISPLAY E-FELD(1:WX) with highlight AT 0021.
           IF WY NOT = 3 AND WH-RETC NOT = 10 GO G.
           IF WH-RETC = 1 AND WY = 3 AND WR-ADR(1) =
               "STANDARD  V10.42.00" GO Z.
       K.  DISPLAY "Waage nicht bereit!" with highlight AT 1221.
           DISPLAY "<ret>= Wiederholung wenn Waage bereit, <esc>= Abbruc
      -       "h < >" AT 2301.
           DISPLAY "<Entf>= Entnahme ohne Waage" AT 2401.
           MOVE 0 TO WM-WAAG.
           CALL "CAUP" USING "0023560000" WH-CREG.
           IF BAB MOVE LOW-VALUES TO WK-RTS WK-DTR GO C.
           IF BAUF MOVE HIGH-VALUES TO WK-RTS WK-DTR GO C.
           IF RET GO A.
           IF EINF DISPLAY WY AT 2010 " - R: " WH-RETC " " WR-ADR(1).
           IF ENTF MOVE 1 TO WM-WAAG GO Z.
           IF NOT ESC GO K.
       Z.  EXIT.
      ****************************************** Waage synchronisieren *
       WAAG-SYNCALT SECTION 90.
       A.  CALL "CAUP" USING "1303012580" WH-CREG.
           MOVE LOW-VALUES TO WK-RTS WK-DTR.
           MOVE WE-CHAN TO WK-CHAN.
           CALL "mfclink" USING "INIT" WH-RETC WK-CHAN WK-BAUD.
       C.  CALL "mfclink" USING "SMCL" WH-RETC WK-CHAN WK-RTS WK-DTR.
           DISPLAY "Waageinitialiserung:" AT 1021.
           DISPLAY " " AT 0021.
           MOVE x"49440D0A" TO E-FELD.
           MOVE 4 TO E-LENG.
           CALL "mfclink" USING "SEND" WH-RETC WK-CHAN E-FELD E-LENG.
           MOVE 0 TO WY.
       G.  ADD 1 TO WY.
           MOVE 45 TO R-LENG.
           CALL "mfclink" USING "RECV" WH-RETC WK-CHAN E-FELD R-LENG
               E-TIME WH-LF.
           ADD -1 TO R-LENG GIVING WX.
           MOVE E-FELD(1:WX) TO WR-ADR(WY).
           DISPLAY E-FELD(1:WX) with highlight AT 0021.
           IF WY NOT = 3 AND WH-RETC NOT = 10 GO G.
           IF WH-RETC = 1 AND WY = 3 AND WR-ADR(1) =
               "STANDARD  V10.42.00" GO Z.
       K.  DISPLAY "Waage nicht bereit!" with highlight AT 1221.
           DISPLAY "<ret>= Wiederholung wenn Waage bereit, <esc>= Abbruc
      -       "h < >" AT 2301.
           DISPLAY "<Entf>= Entnahme ohne Waage" AT 2401.
           MOVE 0 TO WM-WAAG.
           CALL "CAUP" USING "0023560000" WH-CREG.
           IF BAB MOVE LOW-VALUES TO WK-RTS WK-DTR GO C.
           IF BAUF MOVE HIGH-VALUES TO WK-RTS WK-DTR GO C.
           IF RET GO A.
           IF EINF DISPLAY WY AT 2010 " - R: " WH-RETC " " WR-ADR(1).
           IF ENTF MOVE 1 TO WM-WAAG GO Z.
           IF NOT ESC GO K.
       Z.  EXIT.
      *************************************** Druck u. Fehlerprotokoll *
       DRU-PROT SECTION 90.
       A.  WRITE PWA-SATZ.
           MOVE x"0000" TO WH-PX(1).
           MOVE x"000C" TO WH-PX(2).
           PERFORM BEG-DRU.
           MOVE ALL "=" TO PRA-STR.
           PERFORM DRUCK.
           MOVE "Rez.:" TO PRA-STR.
           MOVE RE-NUM TO WD-NUM.
           MOVE WD-NUM TO PRA-STR(6:5).
           MOVE WD-MENGC TO PRA-STR(11:).
           PERFORM DRUCK.
           MOVE RE-BEZ TO PRA-STR.
           PERFORM DRUCK.
           MOVE ALL "-" TO PRA-STR.
           PERFORM DRUCK.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               IF PWA-LGNUM(WX) NOT = 0 AND PWA-IST(WX) NOT = 0
                   MOVE PWA-KOBEZ(WX) TO PRA-BEZ
                   MOVE PWA-IST(WX) TO PRA-GEW
                   PERFORM DRUCK.
           MOVE ALL "=" TO PRA-STR.
           MOVE 3 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE x"1B69" TO PRA-SATZ.
           PERFORM DRUCK.
       X.  CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       PROTKOPF SECTION 90.
       A.  IF WZ-ZEILEN > 63 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE X"0000" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE "WAAGPROT.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE "Verwiegungsprotokoll vom:                       Seite:"
               TO DRA-SATZ.
       Z.  EXIT.
