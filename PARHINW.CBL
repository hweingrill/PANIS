      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PARHINW.
      ******************************************************************
      *               Anlage und Žnderung Hinweisdatei                 *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       OBJECT-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PARSEREZ.CPY.
           COPY PANSEKON.CPY.
           SELECT MODULE     ASSIGN TO "PARTEXT.DAT",
                             ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC,
                             RECORD KEY IS MO-KEY,
                             FILE STATUS IS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY "PANKONS.CPY".
      *********************************************** Karteibewegungen *
       FD  MODULE      external        RECORD IS VARYING IN SIZE
                                       FROM 2 TO 602 CHARACTERS
                                       DEPENDING ON WH-KART.
       01  MO-SATZ.
           03  MO-KEY.
               05 MO-NUM               PIC 9(4)      COMP.
           03  MO-TXT.
               05 MO-ZL                PIC X(40)     OCCURS 15.
       COPY PARREZEP.CPY.
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  DRA-NO                  PIC XX.
           03  DRA-TEXT                PIC X(67).
       01  DRR-SATZ.
           03  FILLER                  PIC XX.
           03  DRR-STR.
               05 DRR-POS              PIC Z9.
               05 DRR-KONUM            PIC ZZ.ZZ9-.
               05 DRR-KOBEZ            PIC X(23).
      *        05 DRR-A                PIC Z.
      *        05 DRR-T                PIC Z.
               05 DRR-PROZ             PIC ZZ.ZZ9,99-.
               05 DRR-MENGE REDEFINES
                  DRR-PROZ             PIC Z.ZZZ.ZZ9-.
               05 DRR-MEH              PIC XXXX.
               05 DRR-PREIS            PIC ZZ.ZZ9,99-.
               05 DRR-BET              PIC ZZZ.ZZ9,99-.
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
       01  WH-KART      external       PIC 9999      COMP.
       01  WF-REG.
           03  WH-PX                   PIC XX       OCCURS 2.
           03  WM-OPEN                 PIC 9       COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99      COMP-X.
           03  WX-PRSTAT               PIC 99      COMP-X.
           03  WZ-SEITE                PIC 99      COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99      COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99      COMP-3 VALUE ZERO.
           03  IX                      PIC 999     COMP-X.
           03  WK-S                    PIC 9       COMP.
           03  WH-MEH                  PIC 99          COMP.
           03  WH-TNAM                 PIC 9(6)           VALUE ZERO.
           03  RED REDEFINES WH-TNAM.
               05 FILLER               PIC 9.
               05 WH-T                 PIC 9.
               05 WH-N                 PIC 9.
               05 WH-A                 PIC 9.
               05 WH-M                 PIC 99.
           03  WH-MENGE                PIC S9(5)   COMP-3.
           03  WS-MENGE                PIC S9(5)   COMP-3.
           03  WH-KB.
               05 WH-KBA               PIC X       VALUE "N".
               05 WH-KBC               PIC X.
               05 WH-KBZUS             PIC X.
           03  WD-MENGE                PIC Z.ZZ9.
      *------------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
           03  WH-PARM                 PIC X(45).
           03  WD-X                    PIC X.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON MODULE.
       A.  CALL "CADECL" USING "PARTEXT.DAT "  WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 42 or
              WL-CA = 45 PERFORM MODUL GO X.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1323012480000" WH-CREG.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              GO A.
       Z.  EXIT.
      **************************** KO-REZPM = SPACE - kein Netzbetrieb *
       OBNETZ SECTION.
       A.  MOVE 35 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE SPACE TO WH-PARM.
           MOVE KO-REZPM TO WH-PARM.
           MOVE 1 TO WH-KEY.
       C.  READ KONSTANT IGNORE LOCK INVALID KEY STOP RUN.
       Z.  EXIT.
      ******************************************* MS-DOS/NETZ-Commands *
       COMM SECTION.
       A.  CALL X"91" USING RESULT FUNKT PARAM.
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
      ****************************** Druckerrckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU not = 1 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularh”he *
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
      ******************************************************************
       MODUL SECTION.
       A.  MOVE RE-NUM TO MO-NUM.
           MOVE SPACE TO MO-TXT.
       B.  READ MODULE IGNORE LOCK INVALID KEY MOVE MO-NUM TO WH-NUM
               INITIALIZE MO-SATZ
               MOVE WH-NUM TO MO-NUM.
           IF WL-CA = 45 and MO-TXT = SPACE GO Z.
           CALL "CAUP" USING "0704011848000" WH-CREG.
           CLOSE MODULE.
           OPEN I-O MODULE.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Produktionsbeschreibung " with highlight
               AT VDU-LP.
       C.  ADD 305 VDU-ECK GIVING VDU-LP.
       D.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 15
               DISPLAY MO-ZL(WX) with highlight AT VDU-LP
               ADD 1 TO VDU-L.
           MOVE 1 TO WX.
           IF WL-CA = 45
               DISPLAY "Weiter mit beliebiger Taste < >" AT 2301
               CALL "CAUP" USING "0023300000" WH-CREG GO X.
       F.  DISPLAY "<esc/#>= Textende, <ret>= Zeilenende, < >= Zeile auf
      -        ", < >= Zeile ab" AT 2301.
           DISPLAY "kein Text = l”schen" AT 2401.
           MOVE MO-ZL(WX) TO WT-TX.
           COMPUTE VDU-LP = 205 + WX * 100.
           CALL "CAUP" USING "1200000140" WH-CREG.
           IF AUF; IF WX > 1 PERFORM ZDRU
                SUBTRACT 1 FROM WX GO F.
           IF AB; IF WX < 13 PERFORM ZDRU
                ADD 1 TO WX GO F.
           MOVE WT-TX TO MO-ZL(WX).
           PERFORM ZDRU.
           IF ESC OR KIST GO H.
           IF NOT RET GO F.
           IF WX < 15 ADD 1 TO WX GO F.
       H.  IF MO-TXT = SPACE GO Q.
       I.  DISPLAY "<ret>= speichern, <esc>= Abbruch, <tab>= l”schen, < 
      -        ">= drucken" AT 2301.
       K.  CALL "CAUP" USING "0023640000" WH-CREG.
           IF WORE PERFORM MODRU GO I.
           IF WOLI GO C.
           IF TABL GO Q.
           IF ESC GO W.
           IF NOT RET GO K.
           PERFORM VARYING IX FROM 15 BY -1 UNTIL MO-ZL(IX)
               NOT = SPACE CONTINUE.
           COMPUTE WH-KART = IX * 40 + 2.
           REWRITE MO-SATZ INVALID WRITE MO-SATZ.
           GO X.
       Q.  DELETE MODULE INVALID GO W.
       W.  INITIALIZE MO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE MODULE.
           OPEN INPUT MODULE.
       Z.  EXIT.
      ******************************************************************
       ZDRU SECTION.
       A.  MOVE MO-ZL(WX) TO WT-TX.
           DISPLAY WT-TX with highlight SIZE 40 AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       MODRU SECTION.
       A.  IF MO-TXT = SPACE GO Z.
           MOVE 0 TO WS-MENGE.
           MOVE X"0100" TO WH-PX(1).
           MOVE X"000C" TO WH-PX(2).
           MOVE WE-DRU(4) TO WM-DRU.
           IF WM-DRU = 1 MOVE 7 TO WH-P.
           PERFORM BEG-DRU.
           IF WM-DRU = 0 MOVE X"1B213100" TO DRA-SATZ(1:4).
           PERFORM DRUCK.
           MOVE RE-BEZ TO DRA-TEXT.
           COMPUTE WD-MENGE = RE-GEW / RE-TEIL.
           MOVE WD-MENGE TO DRA-TEXT(28:).
           MOVE "Stk" TO DRA-TEXT(34:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
      *----------------------------------------------------------------
           SET XR TO 1.
           MOVE 1 TO WX.
       I.  IF RE-KONUM(XR) = 0 OR RE-MENGE(XR) = 0 GO W.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           IF WK-S = 0 MOVE WH-T TO WK-S
               MOVE 0 TO WS-MENGE.
           IF WK-S = WH-T GO L.
       J.  IF WK-S = 0 GO K.
           IF WK-S = 1 MOVE "** Bruehstueck ****" TO DRR-KOBEZ.
           IF WK-S = 2 MOVE "** Quellstueck ****" TO DRR-KOBEZ.
           IF WK-S = 3 MOVE "** Vorteig ****" TO DRR-KOBEZ.
           IF WK-S = 4 MOVE "** Fuellung ****" TO DRR-KOBEZ.
           IF WK-S = 5 MOVE "** Zutaten o.Gew. **" TO DRR-KOBEZ.
           MOVE WS-MENGE TO DRR-MENGE.
           MOVE "gr" TO DRR-MEH
           PERFORM DRUCK.
           MOVE WH-T TO WK-S.
           MOVE 0 TO WS-MENGE.
       K.  IF XR = 20 GO X.
       L.  MOVE WX TO DRR-POS.
           MOVE RE-KONUM(XR) TO DRR-KONUM.
           MOVE RE-KOBEZ(XR) TO DRR-KOBEZ.
      *    MOVE WH-A TO DRR-A.
      *    MOVE WH-T TO DRR-T.
           ADD 1 WH-M GIVING WH-MEH.
           IF WH-A = 0 MOVE "gr" TO DRR-MEH
                    MOVE RE-MENGE(XR) TO DRR-MENGE
               ELSE MOVE WT-MEH(WH-MEH) TO DRR-MEH
                    MOVE RE-PROZ(XR) TO DRR-PROZ.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           ADD 1 TO WX.
       W.  IF XR < 20 SET XR UP BY 1 GO I.
           IF WK-S NOT = 0 GO J.
      *-----------------------------------------------------------------
       X.  MOVE 2 TO WZ-SCHALT.
           PERFORM VARYING IX FROM 15 BY -1 UNTIL MO-ZL(IX)
               NOT = SPACE CONTINUE.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > IX
               MOVE MO-ZL(WX) TO DRA-TEXT
               MOVE 2 TO WZ-SCHALT
               PERFORM DRUCK.
           MOVE VDU-DATUM TO DRA-TEXT.
           PERFORM END-DRU.
       Z.  EXIT.
