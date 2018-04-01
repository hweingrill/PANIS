      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER CALLFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANARCH.
      ************************* Archivierung Rechnung u. Lieferscheine *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEKON.CPY.
           SELECT HILFPOS    ASSIGN TO "DRUHILF.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
           SELECT ARCHIV     ASSIGN TO WH-ARCHIV
                             ORGANIZATION BINARY SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT DRUDAT     ASSIGN TO "PANDRUCK.LST"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
      $SET DATACOMPRESS "1"
           SELECT RE-ARCHIV  ASSIGN TO WN-REARC
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY REA-KEY
                             ALTERNATE KEY REA-AKEY
                             FILE STATUS WF-STATUS.
           SELECT LF-ARCHIV  ASSIGN TO WN-LFARC
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY LFA-KEY
                             FILE STATUS WF-STATUS.
           SELECT ORE-ARCHIV ASSIGN TO WN-OREARC
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY OREA-KEY
                             ALTERNATE KEY OREA-AKEY
                             FILE STATUS WF-STATUS.
           SELECT OLF-ARCHIV ASSIGN TO WN-OLFARC
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY OLFA-KEY
                             FILE STATUS WF-STATUS.
      $SET NO DATACOMPRESS
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
      ******************************************************************
       FD  ARCHIV      external.
       01  AC-SATZ.
           03  AC-RENUM                PIC 9(5).
           03  AC-KTONR                PIC 9(5).
           03  AC-MCODE.
               05 AC-DATUM             PIC 999999.
               05 AC-FIL               PIC 99.
           03  AC-REST                 PIC X(1006).
      ************************************************ Druckhilfsdatei *
       FD  DRUDAT                      LABEL RECORD OMITTED.
       01  PA-SATZ.
           03 PA-ZEIL                  PIC X(132).
      ************************************************ Rechnungsarchiv *
       FD  RE-ARCHIV   external        LABEL RECORD STANDARD
                                       RECORDING MODE IS VARIABLE.
       01  REA-SATZ.
           03  REA-AKEY.
               05 REA-KTONR            PIC 9(5)     COMP.
               05 REA-KEY.
                  07 REA-NUM           PIC 9(5)     COMP.
                  07 REA-LFD           PIC 99       COMP.
           03 REA-REST                 PIC X(1024).
      ********************************************* Lieferscheinarchiv *
       FD  LF-ARCHIV   external        LABEL RECORD STANDARD
                                       RECORDING MODE IS VARIABLE.
       01  LFA-SATZ.
           03  LFA-KEY.
               05 LFA-KTONR            PIC 9(5)     COMP.
               05 LFA-DATUM            PIC 9(6)     COMP.
               05 LFA-NUM              PIC 9(5)     COMP.
               05 LFA-LFD              PIC 99       COMP.
           03  LFA-REST                PIC X(1024).
      ************************************************ Rechnungsarchiv *
       FD  ORE-ARCHIV   external        LABEL RECORD STANDARD
                                       RECORDING MODE IS VARIABLE.
       01  OREA-SATZ.
           03  OREA-AKEY.
               05 OREA-KTONR            PIC 9(5)     COMP.
               05 OREA-KEY.
                  07 OREA-NUM           PIC 9(5)     COMP.
                  07 OREA-LFD           PIC 99       COMP.
           03 OREA-REST                 PIC X(1024).
      ********************************************* Lieferscheinarchiv *
       FD  OLF-ARCHIV   external        LABEL RECORD STANDARD
                                       RECORDING MODE IS VARIABLE.
       01  OLFA-SATZ.
           03  OLFA-KEY.
               05 OLFA-KTONR            PIC 9(5)     COMP.
               05 OLFA-DATUM            PIC 9(6)     COMP.
               05 OLFA-NUM              PIC 9(5)     COMP.
               05 OLFA-LFD              PIC 99       COMP.
           03  OLFA-REST                PIC X(1024).
      ******************************************************************
       FD  HILFPOS             LABEL RECORD STANDARD
                               RECORD IS VARYING IN SIZE FROM 1 TO 80
                               DEPENDING ON WS-HILF.
       01  HI-SATZ                     PIC X(80).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(132).
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
       01  WH-ACSIZE                   PIC 9(5)      COMP.
       01  WH-SIZE                     PIC 99.
       01  WH-ARCHIV.
           03  FILLER                  PIC XXX       VALUE "PAC".
           03  WH-ARCNR                PIC 9(5).
           03  FILLER                  PIC XXXX      VALUE ".DAT".
       01  WN-REARC.
           03  FILLER                  PIC X(6)  VALUE "PANREA".
           03  WN-RJ                   PIC 99    VALUE 95.
           03  FILLER                  PIC X(4)  VALUE ".DAT".
       01  WN-LFARC.
           03  FILLER                  PIC X(6)  VALUE "PANLFA".
           03  WN-LJ                   PIC 99    VALUE 95.
           03  FILLER                  PIC X(4)  VALUE ".DAT".
       01  WN-OREARC.
           03  FILLER                  PIC X(6)  VALUE "YANREA".
           03  WN-ORJ                   PIC 99    VALUE 98.
           03  FILLER                  PIC X(4)  VALUE ".DAT".
       01  WN-OLFARC.
           03  FILLER                  PIC X(6)  VALUE "YANLFA".
           03  WN-OLJ                   PIC 99    VALUE 98.
           03  FILLER                  PIC X(4)  VALUE ".DAT".
       01  WH-REG.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 10.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WH-PX                   PIC XX         OCCURS 2.
           03  WM-MERKER               PIC 9         COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WM-LFOPEN               PIC 9         COMP   VALUE ZERO.
           03  WM-REOPEN               PIC 9         COMP   VALUE ZERO.
           03  WV-LJ                   PIC 99.
           03  WV-RJ                   PIC 99.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP-3 VALUE ZERO.
           03  WH-L                    PIC 99        COMP.
           03  WK-F                    PIC 99      COMP OCCURS 10.
           03  WD-ZL                   PIC Z9.
           03  WD-SIZE                 PIC ZZ.ZZZ.ZZ9.
           03  WI                      PIC 9999      COMP VALUE 0.
           03  WE                      PIC 99      COMP VALUE 0.
           03  IX                      PIC 99      COMP VALUE 0.
           03  WL                      PIC 99      COMP VALUE 0.
           03  WH-LR                   PIC XXXX.
           03  VDU-ZEIT.
               05 VDU-STD              PIC Z9.
               05 VDU-DD               PIC X.
               05 VDU-MIN              PIC 99.
           03  WH-TX                   PIC X(51).
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WH-MOD                  PIC 99        COMP.
           03  WK-D                    PIC 9999      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-LFNUM                PIC 9(6)      COMP.
           03  WV-NUM                  PIC 9(5)      COMP.
           03  WA-DATUM                PIC 9(6)      COMP.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZZ9,9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MG                   PIC X(7).
           03  WD-POS                  PIC ZZ9.
           03  WD-ART                  PIC ZZZ9.
           03  WD-PROZ                 PIC Z9,9.
           03  WD-TG                   PIC ZZ9.
           03  WD-BET                  PIC Z.ZZZ.ZZ9,99-.
           03  WD-PREIS                PIC Z.ZZ9,99-.
           03  WD-APREIS               PIC Z.ZZ9,999-.
           03  WD-X                    PIC X.
           03  WD-UPON                 PIC X(60).
           03  WS-HILF                 PIC 99      COMP  VALUE ZERO.
           03  WK-J                    PIC 99.
           03  WM-J                    PIC 99.
           03  WD-J                    PIC 9999.
      *----------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
           03  WH-FNAME                PIC X(12).
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)    COMP-X.
               05 WF-TT                PIC 99      COMP-X.
               05 WF-MO                PIC 99      COMP-X.
               05 WF-YY                PIC 9999    COMP-X.
               05 WF-HH                PIC 99      COMP-X.
               05 WF-MM                PIC 99      COMP-X.
               05 WF-SS                PIC 99      COMP-X.
               05 WF-HS                PIC 99      COMP-X.
           03  WR-DNAM                 PIC X(12)  OCCURS 10.
           03  wn-verdicht             pic x(16).
       01  WT-DK     external.
           03  WR-TN                PIC X(10)  OCCURS 7.
           03  WT-KON               PIC 99     OCCURS 12.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON HILFPOS.
       A.  CALL "CADECL" USING "DRUHILF.DAT " WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARCHIV.
       A.  CALL "CADECL" USING WH-ARCHIV      WH-CREG.
       DECL-Q SECTION.         USE AFTER ERROR PROCEDURE ON DRUDAT.
       A.  CALL "CADECL" USING "PANDRUCK.LST" WH-CREG.
       DECL-S SECTION.         USE AFTER ERROR PROCEDURE ON RE-ARCHIV.
       A.  CALL "CADECL" USING WN-REARC       WH-CREG.
       DECL-T SECTION.         USE AFTER ERROR PROCEDURE ON LF-ARCHIV.
       A.  CALL "CADECL" USING WN-LFARC       WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE WH-KEY TO WK-D.
           MOVE 1 TO WH-KEY.
           READ KONSTANT WITH NO LOCK INVALID KEY STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO A.
           MOVE KO-MERK TO WK-M.
           MOVE KO-TX TO WH-TX.
           MOVE 10 TO WH-KEY.
       C.  READ KONSTANT WITH NO LOCK INVALID KEY STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF KO-WJAHR(1:2) = SPACE MOVE 0701 TO KO-WJAHR.
           MOVE WM-DATUM TO WZ-DATUM.
           PERFORM BILDJAHR.
           PERFORM ARCH-WAHL.
       Z.  EXIT PROGRAM.
      ************************************* Bilde Ende Wirtschaftsjahr *
       BILDJAHR SECTION.
       A.  MOVE WZ-JAHR TO WK-J.
           COMPUTE WA-DATUM = WK-J * 10000 + KO-WJAHR.
           MOVE WA-DATUM TO WC-DATUM.
           IF WZ-DATUM < WC-DATUM;
               IF WC-TAG not = 0 SUBTRACT 1 FROM WC-TAG
                            else MOVE 99 TO WC-TAG.
           MOVE WC-DATUM TO WA-DATUM WZ-DATUM.
           ADD -1 TO WZ-MONAT.
           IF WZ-MONAT = 0 MOVE 12 TO WZ-MONAT
                      else ADD 1 TO WZ-JAHR.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE 32 TO WZ-TAG.
       C.  ADD -1 TO WZ-TAG.
           MOVE WZ-DATUM TO WS-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE WS-DATUM TO WZ-DATUM GO C.
           MOVE WZ-DATUM TO WS-DATUM.
           MOVE WZ-JAHR TO WK-J WM-J.
           IF WK-J < 50 ADD 2000 WK-J GIVING WD-J
                   else ADD 1900 WK-J GIVING WD-J.
       Z.  EXIT.
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
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
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
       A.  IF WH-DRUNAM(1:3) NOT = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              GO A.
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
       B.  IF WH-PG NOT = 3 WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ***** (1B21)+WH-PX(1) = Schrift. (1B43)+WH-PX(2) = Formularhoehe *
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO C.
           MOVE 1 TO WM-OPEN.
           OPEN OUTPUT DRUCKER.
           PERFORM DRU-OK.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
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
      ******************************************************************
       ARCH-WAHL SECTION.
       A.  MOVE "  Reorganisation" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Reorg durchfÅhren? < >" with highlight AT 1020.
           DISPLAY "<Einf>= durchfÅhren, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0010400000" WH-CREG.
           IF ESC GO Z.
           IF NOT EINF GO Z.
           MOVE 98 TO WN-ORJ WN-OLJ.
           DISPLAY "Rechnungen" with highlight AT 1520.
       C.  OPEN INPUT ORE-ARCHIV.
           IF WF-STATUS not = "00" GO L.
           MOVE LOW-VALUES TO OREA-SATZ.
           START ORE-ARCHIV KEY not < OREA-KEY INVALID GO K.
       E.  READ ORE-ARCHIV NEXT AT END GO K.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF OREA-LFD = 0 MOVE OREA-REST(11:2) TO WK-J
               PERFORM OPEN-REARC.
           MOVE OREA-SATZ TO REA-SATZ.
       G.  WRITE REA-SATZ INVALID ADD 1 TO REA-LFD GO G.
           DISPLAY REA-REST(1:20) AT 2020.
           GO E.
       K.  CLOSE ORE-ARCHIV.
       L.  ADD -1 TO WN-ORJ.
           IF WN-ORJ > 94 GO C.
      *-------------------------------------------> Lieferscheindatei <-
           DISPLAY "Lieferscheine" with highlight AT 1520.
       O.  OPEN INPUT OLF-ARCHIV.
           IF WF-STATUS not = "00" GO S.
           MOVE LOW-VALUES TO OLFA-SATZ.
           START OLF-ARCHIV KEY not < OLFA-KEY INVALID GO R.
       P.  READ OLF-ARCHIV NEXT AT END GO R.
           IF ZUGRIF PERFORM BESETZT GO P.
           IF OLFA-LFD = 0 MOVE OLFA-REST(11:2) TO WK-J
               MOVE OLFA-DATUM TO AC-DATUM
               PERFORM OPEN-LFARC.
           MOVE OLFA-SATZ TO LFA-SATZ.
       Q.  WRITE LFA-SATZ INVALID ADD 1 TO LFA-LFD GO Q.
           DISPLAY LFA-REST(1:20) AT 2020.
           GO P.
       R.  CLOSE OLF-ARCHIV.
       S.  ADD -1 TO WN-OLJ.
           IF WN-OLJ > 94 GO O.
       Z.  EXIT PROGRAM.
      ******************************************************************
       COMLINE SECTION.
       A.  DISPLAY WD-UPON UPON COMMAND-LINE.
           DISPLAY LOW-VALUE AT 2301.
           CALL X"91" USING RESULT FUNKT PARAM.
           CALL "CAUP" USING "1323012480" WH-CREG.
       Z.  EXIT.
      *************************** Jahreseinstellung Lieferscheinarchiv *
       OPEN-LFARC SECTION.
       A.  MOVE WK-J TO WV-LJ.
           MOVE AC-DATUM TO LFA-DATUM.
           IF WV-LJ NOT = WN-LJ;
               IF WM-LFOPEN = 1 CLOSE LF-ARCHIV
                   MOVE 0 TO WM-LFOPEN.
           MOVE WV-LJ TO WN-LJ.
           IF WM-LFOPEN = 0 OPEN I-O LF-ARCHIV
               MOVE 1 TO WM-LFOPEN.
       Z.  EXIT.
      ****************************** Jahreseinstellung Rechnungsarchiv *
       OPEN-REARC SECTION.
       A.  MOVE WK-J TO WV-RJ.
           IF WV-RJ NOT = WN-RJ;
               IF WM-REOPEN = 1 CLOSE RE-ARCHIV
                   MOVE 0 TO WM-REOPEN.
           MOVE WV-RJ TO WN-RJ.
           IF WM-REOPEN not = 0 GO Z.
           OPEN I-O RE-ARCHIV.
           MOVE 1 TO WM-REOPEN.
       Z.  EXIT.
      **************************************** Jahresumwandlung Archiv *
       JAHRREG SECTION.
       A.  MOVE "REN PAN??A9?.* Y*.*" TO WD-UPON.
           PERFORM COMLINE.
       Z.  EXIT.
