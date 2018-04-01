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
               05 PRA-BEZ              PIC X(23).
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
           03  WK-CHAN                 PIC 9999   COMP-5 VALUE 0.
           03  WK-BAUD                 PIC X      VALUE X"9E".
      *-----> "9E" = 1001 1011 * 100 = 1200 * 11 = E * 0 = 1 * 10 = 7 <-
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
