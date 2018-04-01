      $SET LINKCOUNT"384" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANVORL.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT SECDAT     ASSIGN TO DATEI
      *                      ORGANIZATION SEQUENTIAL.
           COPY PANSEDEB.CPY.
           COPY PANSEKON.CPY.
           COPY PANSEART.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEDAU.CPY.
           COPY PANSESTA.CPY.
           COPY PARSEREZ.CPY.
           SELECT BUCHFAKT   ASSIGN TO "PANISFIB.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BF-KEY
                             ALTERNATE KEY BF-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT KONSFIBU   ASSIGN TO WN-FKON
                             ORGANIZATION RELATIVE, ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS WF-STATUS.
           SELECT BUCHUNG    ASSIGN TO "FIBUEBER.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BU-KEY
                             ALTERNATE KEY BU-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT BBNDAT     ASSIGN TO WBN-NAME
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
      *    SELECT BMDFIB     ASSIGN TO "BMDFIBU.DAT"
      *                      ORGANIZATION LINE SEQUENTIAL
      *                      FILE STATUS WF-STATUS.
      *    SELECT KWFIBU     ASSIGN TO "KWFIBU.DAT"
      *                      ORGANIZATION LINE SEQUENTIAL
      *                      FILE STATUS WF-STATUS.
           SELECT SUMMEN     ASSIGN TO DISK "PANESUMM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SU-KEY
                             ALTERNATE RECORD KEY SU-AKEY
                             FILE STATUS IS WF-STATUS.
           SELECT PROTOK     ASSIGN TO DISK "PANEPROT.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
           SELECT EDIFACT    ASSIGN TO WN-EDIFAC
                             ORGANIZATION IS LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANEBUCH.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
       COPY PANSTAT.CPY.
       COPY PANLIEF.CPY.
       COPY PANSUMM.CPY.
       COPY PANFDBUC.CPY.
       COPY PARREZEP.CPY.
      ******************************************************************
       FD  EDIFACT                     LABEL RECORD STANDARD.
       01  EDI-SATZ.
           03  EDI-Z                   PIC X    OCCURS 128.

      ************************************************* Security-Datei *
       FD  BBNDAT                      LABEL RECORD OMITTED.
       01  BBN-SATZ                    PIC X(312).
      ******************************************** KW-Fibu-öberleitung *
      *FD  KWFIBU                      LABEL RECORD OMITTED.
      *01  KW-KSATZ.
      *    03  KW-UEBKZ                PIC 9.
      *    03  KW-TR01                 PIC X.
      *    03  KW-MANDANT              PIC 9(6).
      *    03  KW-TR02                 PIC X.
      *    03  KW-JAHR                 PIC 9999.
      *    03  KW-TR03                 PIC X.
      *    03  KW-MONAT                PIC 99.
      *    03  KW-TR04                 PIC X.
      *01  KW-FSATZ.
      *    03  KW-BSA                  PIC XXX.               *> "AR ".
      *    03  KW-AR01                 PIC X.
      *    03  KW-SKTONR               PIC 9(7).
      *    03  KW-AR02                 PIC X.
      *    03  KW-HKTONR               PIC 9(7).
      *    03  KW-AR03                 PIC X.
      *    03  KW-BUTEXT               PIC X(30).
      *    03  KW-AR04                 PIC X.
      *    03  KW-BELDAT               PIC 99/99/9999.
      *    03  KW-AR05                 PIC X.
      *    03  KW-BELNR                PIC 9(8).
      *    03  KW-AR06                 PIC X.
      *    03  KW-BUBET                PIC 9(9)V99.
      *    03  KW-AR07                 PIC X.
      *    03  KW-BUCODE               PIC X(4).             *> "420 "
      *    03  KW-AR08                 PIC X.
      *    03  KW-UST                  PIC 9(9)V99.
      *    03  KW-AR09                 PIC X.
      *    03  KW-OPNR                 PIC 9(8).
      *    03  kw-AR10                 PIC X.
      *    03  KW-OPDAT                PIC 99/99/9999.
      *    03  KW-AR11                 PIC X.
      *    03  KW-ASKPZ                PIC 9V9.
      *    03  KW-AR12                 PIC X.
      *    03  KW-ASKTAG               PIC 999.
      *    03  KW-AR13                 PIC X.
      *    03  KW-BSKPZ                PIC 9V9.
      *    03  KW-AR14                 PIC X.
      *    03  KW-BSKTAG               PIC 999.
      *    03  KW-AR15                 PIC X.
      *    03  KW-CSKPZ                PIC 9V9.
      *    03  KW-AR16                 PIC X.
      *    03  KW-CSKTAG               PIC 999.
      *    03  KW-AR17                 PIC X.
      *    03  KW-KSTS                 PIC X(6).
      *    03  KW-AR18                 PIC X.
      *    03  KW-KSTH                 PIC X(6).
      *    03  KW-AR19                 PIC X.
      *    03  KW-UID                  PIC 9(14).
      *    03  KW-AR20                 PIC X.
      *    03  KW-FW                   PIC XXX.
      *    03  KW-AR21                 PIC X.
      *    03  KW-FWBET                PIC 9(9)V99.
      *    03  KW-AR22                 PIC X.
      *    03  KW-EURO                 PIC 9.
      *    03  KW-AR23                 PIC X.
      *01  KW-BSATZ.
      *    03  KW-KSA                  PIC X(7).           *> "ADRESSE"
      *    03  KW-BR01                 PIC X.
      *    03  KW-KTO                  PIC 9(7).           *> 2......
      *    03  KW-BR02                 PIC X.
      *    03  KW-ANR                  PIC X(30).
      *    03  KW-BR03                 PIC X.
      *    03  KW-ANAME                PIC X(30).
      *    03  KW-BR04                 PIC X.
      *    03  KW-BNAME                PIC X(30).
      *    03  KW-BR05                 PIC X.
      *    03  KW-CNAME                PIC X(30).
      *    03  KW-BR06                 PIC X.
      *    03  KW-STRASSE              PIC X(30).
      *    03  KW-BR07                 PIC X.
      *    03  KW-PLZL                 PIC X(5).
      *    03  KW-BR08                 PIC X.
      *    03  KW-ORT                  PIC X(30).
      *    03  KW-BR09                 PIC X.
      *    03  KW-LAND                 PIC XXX.
      *    03  KW-BR10                 PIC X.
      *    03  KW-MATCH                PIC X(16).
      *    03  KW-BR11                 PIC X.
      *    03  KW-SELCD                PIC X(10).
      *    03  KW-BR12                 PIC X.
      *    03  KW-SPRACHE              PIC X.
      *    03  KW-BR13                 PIC X.
      *    03  KW-SKPZ                 PIC 9V9.
      *    03  KW-BR14                 PIC X.
      *   03  KW-SKTG                 PIC 999.
      *    03  KW-BR15                 PIC X.
      *    03  KW-ZIEL                 PIC 999.
      *    03  KW-BR16                 PIC X.
      *    03  KW-BKLZL                PIC X(16).
      *    03  KW-BR17                 PIC X.
      *    03  KW-BKNAM                PIC X(30).
      *    03  KW-BR18                 PIC X.
      *    03  KW-BKKTO                PIC X(16).
      *    03  KW-BR19                 PIC X.
      *    03  KW-KUID                 PIC X(14).
      *    03  KW-BR20                 PIC X.
      ******************************************************************
      *FD  BMDFIB.
      *01  BM-KSATZ.
      *    03  BM-KTONR                PIC 9(9).
      *    03  BM-BEZ                  PIC X(35).
      *    03  BM-MCODE                PIC X(20).
      *    03  BM-ZUS                  PIC X(35).
      *    03  BM-STR                  PIC X(30).
      *    03  BM-PLZ                  PIC X(30).
      *    03  BM-ORT                  PIC X(30).
      *    03  BM-STKZ                 PIC 9.       *> Steuerkz immer 1
      *    03  BM-SKZ                  PIC 9.       *> Sammelkto -"-
      *    03  BM-KEND                 PIC X.       *> Endekz. immer "*"
      *01  BM-BSATZ.
      *    03  BM-SA                   PIC 9.       *> immer 0
      *    03  BM-KTO                  PIC 9(9).
      *    03  BM-BUDAT                PIC 9(8).
      *    03  BM-GGKTO                PIC 9(9).
      *    03  BM-BELNR                PIC 9(9).
      *    03  BM-BELDAT               PIC 9(8).
      *    03  BM-MWST                 PIC 99999.   *> 2000%
      *    03  BM-STCD                 PIC 99.      *> 03
      *    03  BM-SH                   PIC 9.       *> 1= S, 2= H
      *    03  BM-BET                  PIC S9(17)   SIGN TRAILING
      *                                             SEPARATE CHARACTER.
      *    03  BM-UST                  PIC S9(17)   SIGN TRAILING
      *                                             SEPARATE CHARACTER.
      *    03  BM-OPBET                PIC S9(17)   SIGN TRAILING
      *                                             SEPARATE CHARACTER.
      *    03  BM-PER                  PIC 99.      *>
      *    03  BM-TEXT                 PIC X(18).   *>
      *    03  BM-SYM                  PIC XX.      *> immer AR
      *    03  BM-USER                 PIC 99.      *> immer 77
      *    03  BM-BUKZ                 PIC 99.      *> immer 00
      *    03  BM-GGBKZ                PIC X.       *> immer S
      *    03  BM-VBKZ                 PIC X.       *> immer A
      *    03  BM-END                  PIC XX.      *> immer 0*
      *    03  BM-REST                 PIC X(36).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(134).
       01  DRL-SATZ.
           03  FILLER                  PIC X(9).
           03  DRL-STR.
               05 DRL-REN              PIC ZZ.ZZ9-.
               05 DRL-KTO              PIC ZZ.ZZ9,9-.
               05 DRL-BEZ              PIC X(36).
               05 DRL-BET              PIC ZZ.ZZZ.ZZ9,99-.
               05 DRL-EBET             PIC ZZZ.ZZ9,99-.
       01  DRS-SATZ.
           03  FILLER                  PIC X(12).
           03  DRS-TEXT.
               05  DRS-KTONR           PIC ZZZ.ZZ9,9-.
               05  DRS-KZ              PIC ZZZ9/.
               05  DRS-UST             PIC 99.
               05  DRS-PZ              PIC X.
           03  FILLER                  PIC X(4).
           03  DRS-BETRAG              PIC ZZ.ZZZ.ZZ9,99-.
       01  DRT-SATZ.
           03  FILLER                  PIC X(12).
           03  DRT-TX.
               05 DRT-TEXT             PIC X(17).
               05 DRT-DATUM            PIC X(10).
               05 DRT-VON              PIC X(8).
               05 DRT-NR               PIC ZZZZ9-.
               05 DRT-BIS              PIC X(5).
               05 DRT-NR1              PIC ZZZZ9.
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
           03  WBN-NAME                PIC X(30).
           03  WBN-VORG                PIC X(25).
           03  WBN-BELNR               PIC 9(9).
           03  WBN-DATUM               PIC X(10).
           03  WBN-LFZEIT              PIC X(30).
           03  WBN-KDNR                PIC 9(6).
           03  WBN-KUBEZ               PIC X(50).
           03  WBN-BET                 PIC X(12)   OCCURS 8.
           03  WBN-BETRAG              PIC 9(8)V99  COMP-3 OCCURS 8.

      *    03  WBN-BTTO20              PIC 9(8)V99    COMP-3.
      *    03  WBN-NTTO20              PIC 9(8)V99    COMP-3.
      *    03  WBN-UST20               PIC 9(8)V99    COMP-3.
      *    03  WBN-BTTO10              PIC 9(8)V99    COMP-3.
      *    03  WBN-NTTO10              PIC 9(8)V99    COMP-3.
      *    03  WBN-UST10               PIC 9(8)V99    COMP-3.
      *    03  WBN-BRUTTO              PIC 9(8)V99    COMP-3.
      *    03  WBN-NETTO               PIC 9(8)V99    COMP-3.
      *----------------------------------------------> Ende BBN-Daten <-
           03  WI                      PIC 999      COMP.
           03  WE                      PIC 999      COMP.
           03  WH-VD                   PIC X(330).
           03  WM-VD                   PIC X(190).
           03  WH-DRUNAM               PIC X(12)    VALUE "LPT1:".
           03  WM-OPEN                 PIC 99       COMP   VALUE ZERO.
           03  WK-OPEN                 PIC 99       COMP   VALUE ZERO.
           03  WX-PRNO                 PIC 99       COMP-X VALUE ZERO.
           03  WX-PRSTAT               PIC 99       COMP-X.
           03  WZ-SCHALT               PIC 99       COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99       COMP   VALUE ZERO.
           03  WZ-SEITE                PIC 99       COMP   VALUE ZERO.
           03  WS-BET                  PIC S9(9)V99 COMP   VALUE ZERO.
           03  WM-UST                  PIC S9(9)V99 COMP.
           03  WD-POS                  PIC ZZ9.
           03  WD-KTO                  PIC ZZZZZ9.
           03  WD-BET                  PIC +Z(8)9,99.
           03  WD-KZ                   PIC 9.
           03  WH-PX                   PIC XX           OCCURS 2.
           03  WK-END                  PIC X(5)       VALUE "KIST".
           03  WF-END                  PIC X(5)       VALUE "ENDE".
           03  WV-MDAT                 PIC 9(6)      COMP.
           03  WV-NUM                  PIC 9(6)      COMP.
           03  WH-RENUM                PIC 9(6)      COMP.
           03  WH-ERST                 PIC 9(6)      COMP.
           03  WK-BUKEY                PIC 9(6)      COMP.
           03  WD-X                    PIC X.
           03  WH-PLZL                 PIC 9999.
           03  WH-SYMT                 PIC 9999.
           03  WR-Y REDEFINES WH-SYMT.
               05 WH-SH                PIC 9.
               05 WH-UST               PIC 9.
               05 WH-OP                PIC 9.
               05 WH-DIV               PIC 9.
           03  WT-KTONR                PIC 9(6)      COMP   OCCURS 21.
           03  WT-ERLOES               PIC S9(9)V99  COMP   OCCURS 24.
           03  WE-EXTN                 PIC X(5).
           03  WE-EXTV.
               05 WR-VAL               PIC 9.
               05 WR-AUF               PIC 9.
           03  WK-EURO                 PIC 99V9(4)   COMP.
           03  WX-MON                  PIC 9999.
           03  WD-MON                  PIC 99/99.
           03  WN-EDIFAC               PIC X(50).
           03  WH-DTM                  PIC 9(8).
           03  WH-LFDAT                PIC 9(8).
           03  WU-Z                    PIC X.
           03  WU-SEG                  PIC XXX.
           03  WU-QUAL                 PIC XXX.
           03  WU-ANZ                  PIC 9(4).
           03  WU-MEH                  PIC XXX.
           03  WU-TEXT                 PIC X(30).
           03  WK-ARNUM                PIC 9(4).
           03  WH-DP                   PIC 9(13).
           03  WH-BY                   PIC 9(13).
           03  WH-OB                   PIC 9(13).
           03  WH-IV                   PIC 9(13).
           03  WH-UC                   PIC 9(13).
           03  WH-CN                   PIC 9(13).
           03  WH-SU                   PIC 9(13).
           03  WH-EMPF                 PIC 9(13).
           03  WH-EAN                  PIC 9(13).
           03  IC                      PIC 9999    COMP.
           03  IQ                      PIC 9999    COMP.
           03  IP                      PIC 9999    COMP.
           03  WT-GRP.
               05 WT-EDI               PIC X      OCCURS 128.
           03  WH-MG                   PIC 99999V999.
      *--------------------------------------------> Dateien auslesen <-
           03  WL-FUNKT                PIC X       COMP-X   VALUE 69.
           03  WL-PARM.
               07 WL-ACT               PIC X       COMP-X.
               07 WL-ATTRIB            PIC X       COMP-X.
               07 WL-FILEIN            PIC X(50).
      *---------------------------------------------> Directory lesen <-
           03  WL-RESULT.
               05 WL-ERR               PIC X       COMP-X.
               05 WL-HANDLE            PIC XX      COMP-X.
               05 WL-ATTROUT           PIC X       COMP-X.
               05 WL-TIME              PIC XX      COMP-X.
               05 WL-DATE              PIC XX      COMP-X.
               05 WL-SIZE              PIC X(4)    COMP-X.
               05 WL-FOUT.
                  07 WL-FNAM           PIC X(9).
                  07 WL-FIL            PIC 999.
                  07 WL-REST           PIC X(5).
      *-------------------------------------------- Byte-Stream-Files <-
           03  WN-EDICUS               PIC X(30).
           03  WB-SATZ                 PIC X(160).
           03  WB-AC                   PIC X     COMP-X.
           03  WB-DE                   PIC X     COMP-X.
           03  WB-DV                   PIC X     COMP-X.
           03  WB-FH                   PIC X(4).
           03  WB-OFS                  PIC X(8)  COMP-x.
           03  WB-CNT                  PIC X(4)  COMP-x.
           03  WB-FLG                  PIC X     COMP-x.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "PANDEBIT.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON BUCHUNG.
       A.  CALL "CADECL" USING "FIBUEBER.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 88 PERFORM TAGESSUMMEN GO X.
           PERFORM VORLAUF.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       VORLAUF SECTION.
       A.  MOVE " Brot - GebÑck 17.2  " TO WK-GEB.
           PERFORM LADE-DRU.
           MOVE 0 TO WH-BUKEY.
           IF WH-PG = 9 GO B.
           MOVE "PANIS" TO WE-EXTN.
           CALL "_EXTNAME" USING WE-EXTN WE-EXTV.
           IF WE-EXTV(1:1) = SPACE MOVE 0 TO WR-VAL.
           IF WE-EXTV(2:1) = SPACE MOVE 0 TO WR-AUF.         *> Reserve
           MOVE WR-VAL TO WE-PANIS.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           INITIALIZE LF-SATZ.
           INITIALIZE AR-SATZ.
           INITIALIZE DE-SATZ.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           MOVE "Firma    An das   An die   Herrn    Frau     An
      -        "                                   MonMonWo Wo hMoSof"
               TO WH-TABTX.
       B.  MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK not INVALID GO J.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Bitte Kopf anlegen!" AT 2401.
           INITIALIZE KO-SATZ.
       C.  ACCEPT KO-FIRMA with highlight AT 0127.
           IF KO-FIRMA = SPACE GO C.
           DISPLAY KO-FIRMA with highlight AT 0127.
           MOVE "pane" TO KO-SPERRE.
           MOVE 10 TO KO-UST(1).
           MOVE 20 TO KO-UST(2).
           MOVE 0 TO KO-UST(3) KO-UST(4) KO-UST(5) KO-UST(6).
           MOVE SPACE TO KO-ORT KO-TX.
           MOVE WH-DATUM TO WZ-DATUM.
           MOVE 31 TO WZ-TAG.
           MOVE WZ-DATUM TO WV-DATUM.
       D.  CALL "CAUP" USING "05DATPRUEF" WH-CREG
           IF WZ-DATUM = 0 SUBTRACT 1 FROM WV-DATUM
               MOVE WV-DATUM TO WZ-DATUM GO D.
           MOVE WX-DATUM TO KO-MON.
           MOVE 1 TO KO-RENUM KO-NUM WM-VER WM-KO WM-PR WM-RB.
           MOVE WK-M TO KO-MERK.
           WRITE KO-SATZ.
           MOVE SPACE TO KO-SATZ.
           MOVE 10 TO WH-KEY KO-NUM.
           MOVE "StkKg daggr KtnSckNtzPckPkg" TO KO-TM.
           WRITE KO-SATZ.
           MOVE SPACE TO KO-SATZ.
           MOVE 6 TO WH-KEY KO-NUM.
           MOVE 0 TO KO-ERST KO-DATUM KO-ERLOES(1) KO-ERLOES(2)
               KO-ERLOES(3) KO-ERLOES(4) KO-ERLOES(5) KO-ERLOES(6)
               KO-ERLOES(7) KO-ERLOES(8) KO-ERLOES(9) KO-ERLOES(10).
           WRITE KO-SATZ.
           GO B.
       J.  MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           MOVE KO-MERK TO WK-M.
           MOVE KO-EANNR TO WK-EAN.
           MOVE KO-ORT TO WT-TX.
           MOVE KO-MON TO WK-MON.
           MOVE 0 TO WE-LFS.
           IF WM-VER > 10 ADD -10 TO WM-VER
                          MOVE 1 TO WE-LFS.              *> Lfsch. Kern
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           MOVE WX-DATUM TO WS-DATUM WM-DATUM.
      *-----------------------------------------> max. und min. Datum <-
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WK-MON)
           ADD 6 TO WA-DAT
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT)
           ADD 3 TO WA-DAT
           COMPUTE WE-MAXDAT = FUNCTION DATE-OF-INTEGER(WA-DAT)
           MOVE WK-MON TO WZ-DATUM.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WK-MON).
           SUBTRACT WZ-TAG -1 FROM WA-DAT.
           COMPUTE WE-MINDAT = FUNCTION DATE-OF-INTEGER(WA-DAT).
           SET TY FY TO 15.
       K.  IF WR-TX(TY) not = SPACE GO L.
           IF TY > 1 SET TY DOWN BY 1 GO K.
       L.  MOVE WR-TX(TY) TO WR-TX(FY).
           IF TY not = FY MOVE SPACE TO WR-TX(TY).
           IF TY > 1 SET TY FY DOWN BY 1 GO L.
           MOVE WT-TX TO VDU-ORT.

           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE KO-UST(WX) TO WT-UST(WX).
           PERFORM KONDIT.
           MOVE 10 TO WH-KEY.
       N.  READ KONSTANT IGNORE LOCK.
           MOVE KO-TM TO WT-TM.
           MOVE KO-DRU TO WM-DRU.
           CALL "CAUP" USING "06KOPF" WH-CREG.
       O.  IF WX-CODE(2) = WX-CODE(1) GO P.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           MOVE 9 TO WH-PG.
           IF ESC CALL "CAUP" USING "0703030303000" WH-CREG
                  CALL "CAUP" USING "1303010880" WH-CREG GO R.
           MOVE WT-TX TO WX-CODE(1).
           GO O.
       P.  CALL "CAUP" USING "0704201838000" WH-CREG.
       Q.  DISPLAY "<esc/ret-leer>= Programmende" AT 2301.
           IF KO-ETKDRU > 0 DISPLAY "<Strg-Ende>= BBN-Ausgabe" AT 2401.
           PERFORM OB-EDIFACT.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Programmwahl " with highlight AT VDU-LP
                   " "                    AT 0524
                   "1 - Lieferscheine"    AT 0024
                   "2 - Rechnungen"       AT 0024
                   "3 - Preise/Bestellg." AT 0024
                   " "                    AT 0001
                   "4 - Druckprogramm"    AT 0024
                   "5 - Rasterdruck"      AT 0024
                   "6 - Stammdaten"       AT 0024
                   " "                    AT 0001
                   "7 - Listungen"        AT 0024
                   "8 - Archivierung"     AT 0024
                   "9 - DatumsÑnderung"   AT 0024
                   " "                    AT 0024
                   "bitte wÑhlen Sie: " with highlight AT 0032.
           CALL "CAUP" USING "0018502002" WH-CREG.
           IF SAPO PERFORM HOGAST GO B.
           IF SEPO PERFORM BBN-SUMMEN GO Z.
           MOVE WH-NUM TO WH-PG.
           IF KIST and WM-KO = 3
               CALL "CBL_COPY_FILE" USING WK-END WF-END
               MOVE 0 TO WH-PG.
           IF EINF CALL "CALOG" USING "20ANZ" WH-CREG GO Q.
       R.  IF ESC OR WH-PG = 0 MOVE 0 TO WH-PG
               CALL "CAUP" USING "08CLOFEN" WH-CREG
               CALL "CALOG" USING "10AUS" WH-CREG
               CLOSE ARTIKEL DEBITOR FAKTDAT LFSCHEIN DAUER KONSTANT
               CALL "CAUP" USING "1301012480" WH-CREG
               DISPLAY "Programm beendet" AT 1228
               STOP RUN.
           IF not RET GO Q.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WH-PG = 9 MOVE 0 TO WZ-DATUM WH-PG
                        CALL "CAUP" USING "03DATUM" WH-CREG GO P.
           MOVE WH-NUM TO WH-PG.
           IF WH-PG = 8 GO Z.
      *------------------------------------> prÅfen min. & max. Datum <-
           IF WM-DATUM < WE-MINDAT
               DISPLAY "Datum zu klein" AT 2401
               PERFORM WEITER GO P.
           IF WM-DATUM > WE-MAXDAT
               DISPLAY "Verarbeitung nur nach durchgefÅhrter PrÅfliste m
      -            "îglich!" AT 2401
               PERFORM WEITER GO P.
           IF WM-DATUM > WV-DATUM
               DISPLAY "Bitte PrÅfliste durchfÅhren!" AT 2401
               PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       OB-EDIFACT SECTION.
       A.  OPEN INPUT HILFDESA.
           IF WF-STATUS = "35" GO Z.
           INITIALIZE HD-SATZ.
           START HILFDESA KEY > HD-KEY INVALID GO X.
       C.  READ HILFDESA NEXT AT END GO X.
           DISPLAY "Edifact-Daten" with highlight foreground-color 6
              AT 2023 " vorhanden!" with highlight foreground-color 4.
       X.  CLOSE HILFDESA.
       Z.  EXIT.
      ******************************************************************
       KONDIT SECTION.
       A.  INITIALIZE WH-TABK.
           MOVE 35 TO WH-KEY.                         *> Druckerangaben
           READ KONSTANT IGNORE LOCK INVALID INITIALIZE KO-PSATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE KO-PARAM(WX) TO WE-PARAM(WX).
           MOVE 36 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID INITIALIZE KO-ESATZ
               MOVE 36 TO KO-NUM
               MOVE SPACE TO KO-VERZ
               WRITE KO-SATZ GO A.
           MOVE KO-VERZ TO WN-FKON WN-BUEB.
           IF WN-FKON(1:3) = "KWF" and WN-FKON(6:6) = SPACE
               DISPLAY "Mandatennummer fehlt" AT 2401
               PERFORM WEITER.
           IF KO-VERZ(2:1) not = ":" GO R.
           PERFORM VARYING WX FROM 30 BY -1 UNTIL WX = 0
               OR WN-BUEB(WX:1) = "\" CONTINUE.
           ADD 1 TO WX.
           MOVE "FIBUEBER.DAT" TO WN-BUEB(WX:).
           OPEN INPUT KONSFIBU.
           IF WF-STATUS not = "00" MOVE SPACE TO WN-BUEB GO R.
           MOVE 1 TO WH-KEY.
       C.  READ KONSFIBU IGNORE LOCK INVALID GO Q.
           MOVE KF-TABK TO WH-TABK.
      *-----------------------------------------> Symboltabelle lesen <-
           MOVE 10 TO WH-KEY.
       E.  READ KONSFIBU IGNORE LOCK INVALID GO Q.
           MOVE 99 TO WE-ARSYM WE-GSSYM.
           PERFORM VARYING WX FROM 0 BY 1 UNTIL WX > 19
               MOVE KF-SKZ(WX + 1) TO WH-SYMT
               EVALUATE WH-DIV
                   WHEN 1 MOVE WX TO WE-ARSYM
                   WHEN 2 MOVE WX TO WE-GSSYM.
           IF WE-ARSYM = 99 OR WE-GSSYM = 99 DISPLAY "Symbolkz. f. AR un
      -        "d/oder GU nicht angelegt" AT 2401 PERFORM WEITER
               MOVE 0 TO WE-ARSYM WE-GSSYM.
       Q.  CLOSE KONSFIBU.
           GO Z.
      *---------------------------------------> aus PANEKONS auslesen <-
       R.  MOVE 7 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO Z.
           MOVE KO-TABK TO WH-TABK.
      *-----------------------------------------> Symboltabelle lesen <-
           MOVE 8 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO Z.
           MOVE 99 TO WE-ARSYM WE-GSSYM.
           PERFORM VARYING WX FROM 0 BY 1 UNTIL WX > 19
               MOVE KO-SKZ(WX + 1) TO WH-SYMT
               EVALUATE WH-DIV
                   WHEN 1 MOVE WX TO WE-ARSYM
                   WHEN 2 MOVE WX TO WE-GSSYM.
           IF WE-ARSYM = 99 OR WE-GSSYM = 99 DISPLAY "Symbolkz. f. AR un
      -        "d/oder GU nicht angelegt" AT 2401 PERFORM WEITER
               MOVE 0 TO WE-ARSYM WE-GSSYM.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" USING "1323012480000" WH-CREG.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) not = "LST" GO Z.
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
           WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO A.
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
           IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ GO E.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO D.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 99 GO E.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       TAGESSUMMEN SECTION.
       A.  CALL "CAUP" USING "0710050560000" WH-CREG.
           MOVE WE-DRU(1) TO WM-DRU.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Summendruck " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           DISPLAY "Druck Tagessummen bis:          " AT VDU-LP.
           ADD 326 VDU-ECK GIVING VDU-LP.
           MOVE WM-DATUM TO WC-DATUM WS-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       B.  DISPLAY "<ret>= Datum, <esc>= Abbruch" AT 2301.
           MOVE WS-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1103266006" WH-CREG.
           IF ESC GO X.
           IF not RET GO B.
           IF WZ-DATUM = 0 GO Z.
           MOVE WZ-DATUM TO WS-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       C.  DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT 2301.
           DISPLAY "tab-.lst" AT 2401.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF TABL MOVE "TAGSUM.LST" TO WH-DRUNAM
               SET RET TO TRUE.
           IF WM-DRU = 1 MOVE "TAGSUM.LST" TO WH-DRUNAM.
           IF not RET GO C.
           PERFORM READ-KONS.
           IF ESC GO X.
           MOVE 0 TO WZ-SEITE WM-OPEN WZ-ZEILEN WK-OPEN.
           PERFORM ARLIST.
           PERFORM END-DRU.
       K.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE 0 TO KO-ERLOES(WX).
           MOVE 0 TO KO-DATUM KO-ERST.
           REWRITE KO-SATZ.
       X.  MOVE 9 TO WH-PG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************* öberleitung neuer Adressen *
       NEUE-ADR SECTION.
       A.  MOVE 0 TO WZ-SEITE WS-BET BF-REN BF-KEY.
           MOVE "K" TO BF-SA.
           START BUCHFAKT KEY > BF-RKEY INVALID GO Z.
       B.  READ BUCHFAKT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF BF-SA not = "K" GO B.
      *---------------------------------------> Abfrage andere FIBU's <-
      *    IF WN-FKON(1:3) = "BMD" GO F.
      *    IF WN-FKON(1:3) = "KWF" GO K.
           MOVE BF-SATZ TO BU-SATZ.
       C.  ADD 1 TO WK-BUKEY.
           MOVE WK-BUKEY TO BU-KEY.
           WRITE BU-SATZ INVALID GO C.
           DELETE BUCHFAKT.
           GO B.
      *---------------------------------------------> neue BMD-Kunden <-
      *F.  MOVE BF-KTONR TO BM-KTONR.
      *    MOVE BF-MCODE TO WT-TX.
      *    CALL "CAUP" USING "21CONV10" WH-CREG.
      *    MOVE WT-TX TO BM-MCODE.
      *    MOVE 1 TO BM-STKZ BM-SKZ.
      *    MOVE "*" TO BM-KEND.
      *    MOVE BF-BEZ TO WT-BEZ.
      *    MOVE SPACE TO WT-ADR.
      *    MOVE "#" TO WR-BEZ(131).
      *    UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
      *        WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
      *    MOVE 5 TO WZ.
      *    IF WR-ADR(5) = SPACE MOVE 4 TO WZ.
      *    MOVE WR-ADR(WZ)(1:4) TO WH-PLZL(1:).
      *    INSPECT WH-PLZL REPLACING ALL " " BY "0".
      *    IF WH-PLZL not NUMERIC MOVE 0 TO WH-PLZL.
      *    MOVE WR-ADR(1) TO BM-BEZ.
      *    MOVE WR-ADR(2) TO BM-ZUS.
      *    MOVE WR-ADR(WZ)(6:) TO BM-ORT.
      *    MOVE WR-ADR(WZ - 1) TO BM-STR.
      *    MOVE WH-PLZL TO BM-PLZ.
      *    WRITE BM-KSATZ.
      *    DELETE BUCHFAKT.
      *    GO B.
      *------------------------------------------> neue Kunde KW-Fibu <-
      *K.  MOVE ALL X"7f" TO KW-BSATZ.
      *    MOVE "ADRESSE" TO KW-KSA.
      *    COMPUTE KW-KTO = BF-KTONR / 10 + 2000000.
      *    MOVE BF-BEZ TO WT-BEZ.
      *    MOVE SPACE TO WT-ADR.
      *    MOVE "#" TO WR-BEZ(131).
      *    UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
      *        WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
      *    PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 5
      *        IF WR-ADR(WZ)(1:4) numeric and
      *           WR-ADR(WZ)(1:4) not = SPACE
      *           MOVE WR-ADR(WZ)(1:4) TO KW-PLZL(1:)
      *           MOVE WR-ADR(WZ)(6:) TO KW-ORT
      *           exit perform.
      *    MOVE 5 TO WZ.
      *    IF WR-ADR(5) = SPACE MOVE 4 TO WZ.
      *    IF KW-PLZL(1:4) not NUMERIC MOVE 8010 TO KW-PLZL.
      *    MOVE WR-ADR(1) TO KW-ANAME.
      *    MOVE WR-ADR(2) TO KW-BNAME.
      *    MOVE WR-ADR(WZ - 1) TO KW-STRASSE.
      *    MOVE SPACE TO KW-BKLZL KW-ANR KW-BKNAM KW-BKKTO KW-KUID
      *        KW-MATCH KW-SELCD KW-LAND KW-CNAME.
      *    MOVE 8 TO KW-ZIEL.
      *    MOVE 0 TO KW-SKPZ KW-SKTG.
      *    MOVE KW-BSATZ TO WH-VD.
      *    PERFORM VERDICHT.
      *    MOVE WH-VD TO KW-BSATZ.
      *    WRITE KW-BSATZ.
      *    DELETE BUCHFAKT.
      *    GO B.
       Z.  EXIT.
      ********************************************* Fibusummen aus BBN *
       BBN-SUMMEN SECTION.
       A.  CALL "CAUP" USING "271010076000014" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " BBN - Fibusummen " with highlight AT VDU-LP.
           ADD 313 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - RechnungsÅbernahme " AT VDU-LP.
           ADD 435 VDU-ECK GIVING VDU-LP.
           DISPLAY "< >" AT VDU-LP.
           ADD 513 VDU-ECK GIVING VDU-LP.
           DISPLAY "2 - KassaÅbernahme " AT VDU-LP.
       C.  DISPLAY "<ret>= Auswahl, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "1004361001" WH-CREG.
           IF ESC GO X.
           EVALUATE WH-NUM
               WHEN 1 PERFORM BBN-RECH
               WHEN 2 PERFORM WEITER
               WHEN OTHER GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************* BBN-Rechnungen *
       BBN-RECH SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY "RechnungsÅbernahme" AT VDU-LP.
           MOVE "BBNRECH.CSV" TO WBN-NAME.
           OPEN INPUT BBNDAT.
           IF WF-STATUS = "00" READ BBNDAT AT END GO Z
                               end-read
                               GO C.
           DISPLAY " Keine Rechnungen vorhanden" AT 2401.
           PERFORM WEITER.
           GO Z.
       C.  READ BBNDAT AT END GO Z.
           DISPLAY BBN-SATZ AT 0501.
           UNSTRING BBN-SATZ DELIMITED BY ";" INTO WBN-VORG WBN-BELNR
               WBN-DATUM WBN-LFZEIT WBN-KDNR WBN-KUBEZ WBN-BET(1)
               WBN-BET(2) WBN-BET(3) WBN-BET(4) WBN-BET(5)
               WBN-BET(6) WBN-BET(7) WBN-BET(8).
      *        WBN-NTTO20 WBN-UST20
      *                   WBN-BTTO10 WBN-NTTO10 WBN-UST10
      *        WBN-BRUTTO WBN-NETTO.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 8
               MOVE WBN-BET(WY) TO WH-MCODE
               PERFORM ZERLEG
               MOVE WH-WERT TO WBN-BETRAG(WY).
           MOVE WBN-DATUM(1:2) TO WZ-TAG.
           MOVE WBN-DATUM(4:2) TO WZ-MONAT.
           MOVE WBN-DATUM(9:2) TO WZ-JAHR.
           GO C.
       Z.  EXIT.
      *************************************** numeriche Werte zerlegen *
       ZERLEG SECTION.
       A.  MOVE 0 TO WH-NUM WH-NEG.
           MOVE 13 TO WZ.
           PERFORM VARYING WX FROM 13 BY -1 UNTIL WX = 0
               EVALUATE WH-MCODE(WX:1)
                   WHEN 0 THRU 9 MOVE WH-MCODE(WX:1) TO WH-NUM(WZ:1)
                                 ADD -1 TO WZ
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN OTHER CONTINUE.
           COMPUTE WH-WERT = WH-NUM / 100.
           IF WH-NEG = 1 COMPUTE WH-WERT = WH-WERT * -1.
       Z.  EXIT.
      *********************************************** Datei verdichten *
      *VERDICHT SECTION.
      *A.  MOVE 1 TO WI.
      *    PERFORM VARYING WI FROM 330 BY -1 UNTIL WI = 1
      *        IF WH-VD(WI:1) = "" exit perform.
      *C.  ADD -1 TO WI.
      *    COMPUTE WE = WI + 1.
      *    IF WH-VD(WI:2) = " "
      *        MOVE WH-VD(WE:) TO WH-VD(WI:).
      *    IF WI not = 1 GO C.
      *Z.  EXIT.
      ****************************************************** KW-Haeder *
      *KW-HAEDER SECTION.
      *A.  IF WK-OPEN = 1 GO Z.
      *    MOVE 1 TO WK-OPEN.
      *    MOVE SPACE TO KW-KSATZ.
      *    MOVE 0 TO KW-UEBKZ.
      *    MOVE WN-FKON(6:6) TO KW-MANDANT(1:).
      *    COMPUTE KW-JAHR = WS-DATUM / 10000 + 1900.
      *    IF KW-JAHR < 50 ADD 100 TO KW-JAHR.
      *    COMPUTE KW-MONAT = WS-DATUM / 100.
      *    MOVE X"7f" TO KW-TR01
      *    MOVE X"7f" TO KW-TR02
      *    MOVE X"7f" TO KW-TR03
      *    MOVE X"7f" TO KW-TR04.
      *    WRITE KW-KSATZ.
      *Z.  EXIT.
      ****************************** nÑchster Key Fibudatei (fibueber) *
       NEXTKEY SECTION.
       A.  OPEN I-O BUCHUNG.
           MOVE 999999 TO BU-KEY.
           MOVE 0 TO WK-BUKEY.
           START BUCHUNG KEY < BU-KEY INVALID GO Z.
       C.  READ BUCHUNG PREVIOUS NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE BU-KEY TO WK-BUKEY.
       Z.  EXIT.
      ***************************************** Rechnungsausgangsliste *
      *  ohne Fibu: Altenburger, Kînig, Edegger, Viertler              *
      ******************************************************************
       ARLIST SECTION.
       A.  MOVE 13,7603 TO WK-EURO.
           OPEN I-O BUCHFAKT.
      *    EVALUATE WN-FKON(1:3)
      *        WHEN "BMD" OPEN EXTEND BMDFIB
      *        WHEN "KWF" OPEN EXTEND KWFIBU
      *                   PERFORM KW-HAEDER
      *        WHEN OTHER PERFORM NEXTKEY.
           PERFORM NEXTKEY.
           PERFORM NEUE-ADR.                     *> neue Kundenadressen
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 24
               MOVE 0 TO WT-ERLOES(WX).
           MOVE 0 TO WZ-SEITE WS-BET BF-REN BF-KEY WH-RENUM WV-NUM.
           MOVE 999999 TO WH-ERST.
           MOVE SPACE TO BF-SA.
           START BUCHFAKT KEY > BF-RKEY INVALID GO Z.
       B.  READ BUCHFAKT NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE BF-KTONR TO WD-KZ.
           IF WD-KZ = 0 GO B.
           IF BF-SA not = " " GO B.
           IF BF-DAT > WS-DATUM GO B.
       bu. IF WZ-ZEILEN > 61 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WM-OPEN = 0 MOVE X"0100" TO WH-PX(1)
                          MOVE X"000C" TO WH-PX(2)
                          IF WE-DRU(1) = 1 MOVE 5 TO WH-P
                          end-if
                          PERFORM BEG-DRU GO C.
           IF WZ-ZEILEN > 0 GO G.
           IF WM-KO = 0 MOVE 12 TO WZ-SCHALT
                   else MOVE 3 TO WZ-SCHALT.
       C.  MOVE 3 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE 1 TO WZ-SCHALT.
           MOVE "  Rechnungsliste per: " TO DRL-STR.
           MOVE VDU-DATUM TO DRL-STR(23:8).
           ADD 1 TO WZ-SEITE.
           MOVE "Seite: " TO DRL-STR(68:7).
           MOVE WZ-SEITE TO WD-POS.
           MOVE WD-POS TO DRL-STR(75:3).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRL-STR PERFORM DRUCK.
           MOVE "  Re-Nr.   Kd-Nr. Name
      -       "               Betrag" TO DRL-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRL-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  IF BF-REN > WH-RENUM MOVE BF-REN TO WH-RENUM.
           IF BF-REN < WH-ERST MOVE BF-REN TO WH-ERST.
           if wv-num = 0 move bf-ren to wv-num
           else add 1 to wv-num
                if wv-num not = bf-ren
                    display wv-num at 2401 " "
                    move wv-num to drl-ren
                    perform druck
                    go bu.
      *    go b.                               *> nur fehlende drucken
           MOVE BF-REN TO DRL-REN.
           DIVIDE 10 INTO BF-KTONR GIVING DRL-KTO.
           MOVE BF-KTONR TO DE-KTONR.
           MOVE 1 TO DE-FNR.
       H.  READ DEBITOR IGNORE LOCK INVALID KEY MOVE SPACES TO DE-BEZ.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           MOVE WT-BEZ TO DRL-BEZ.
           MOVE BF-SATZ TO BU-SATZ.
      *--------------------------------------> énderungen wg. FIB2000 <-
           MOVE "Ats" TO BU-FSYM.
           MOVE 100 TO BU-KURS.
           MOVE BU-BET TO BU-FWBET.
      *------------------------------------------> Umrechnung auf EUR <-
           MOVE "Eur" TO BU-FSYM.
           IF BU-FSYM = "Ats" MOVE BU-BET TO DRL-BET
               COMPUTE BU-BET rounded = BU-BET / WK-EURO
               COMPUTE BU-B1 rounded = BU-B1 / WK-EURO
               COMPUTE BU-B2 rounded = BU-B2 / WK-EURO
               COMPUTE BU-MW = BU-BET - BU-B1 - BU-B2.
           MOVE BU-BET TO DRL-EBET.
           ADD BU-BET TO WS-BET.
      *----------------------------------------> neue Erlîsuafteilung <-
           IF WM-KO = 3 MOVE BU-GK TO WZ
               IF WZ = 0 MOVE DE-TOUR TO WZ
               end-if
               ADD BU-B2 TO WT-ERLOES(21)
               ADD BU-B1 TO WT-ERLOES(WZ)
           else ADD BU-B2 TO WT-ERLOES(21)
               ADD BU-B1 TO WT-ERLOES(1).
           ADD BU-MW TO WT-ERLOES(22).
           ADD BU-BET TO WT-ERLOES(23).
           MOVE 0 TO BU-GK.
           IF BU-SKTOBAS(1:) = SPACE MOVE 0 TO BU-SKTOBAS.
           MOVE BU-DAT TO BU-VALDAT.
      *    IF WN-FKON(1:3) = "BMD" PERFORM BMD-BUCH GO K.
      *    IF WN-FKON(1:3) = "KWF" PERFORM KWF-BUCH GO K.
      *-----------------------------------------> BU-DAT = Belegdatum <-
       J.  ADD 1 TO WK-BUKEY.
           MOVE WK-BUKEY TO BU-KEY.
           WRITE BU-SATZ INVALID GO J.
       K.  ADD 402 VDU-ECK GIVING VDU-LP.
           DISPLAY DRL-STR(1:57) AT VDU-LP.
           PERFORM DRUCK.
           DELETE BUCHFAKT.
           GO B.
      *----------------------------------------------------> Listende <-
       Q.  IF WZ-SEITE = 0 GO X.
           MOVE ALL "ƒ" TO DRL-STR.
           PERFORM DRUCK.
           MOVE "S u m m e" TO DRL-BEZ.
           MOVE WS-BET TO DRL-EBET.
           PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
           IF WZ-ZEILEN > 50 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN
               IF WM-KO = 0 MOVE 12 TO WZ-SCHALT
                   else MOVE 4 TO WZ-SCHALT
               end-if PERFORM DRUCK.
           MOVE "Tagessummen per:" TO DRT-TEXT.
           MOVE WH-RENUM TO DRT-NR1.
           MOVE WH-ERST  TO DRT-NR.
           MOVE WS-DATUM TO WC-DATUM.
           MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
           MOVE VDU-DATUM TO DRT-SATZ(30:8).
           MOVE "von Re:" TO DRT-SATZ(40:8).
           MOVE "bis:" TO DRT-BIS.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRT-TX.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
      *----------------------------------------> Ausdruck Erlîskonten <-
           MOVE "Erlîskonto:" TO DRS-TEXT PERFORM DRUCK.
           MOVE 0 TO WH-WERT.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 21
              IF WT-ERLOES(WX) not = 0 PERFORM TAGKOPF
                  DIVIDE 10 INTO WT-KTONR(WX) GIVING DRS-KTONR
                  MOVE WT-ERLOES(WX) TO DRS-BETRAG
                  ADD WT-ERLOES(WX) TO WH-WERT
                  PERFORM DRUCK.
           MOVE "Nettosumme" TO DRS-TEXT.
           MOVE WH-WERT TO DRS-BETRAG.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE "Umsatzsteuer" TO DRS-TEXT.
           MOVE WT-ERLOES(22) TO DRS-BETRAG.
           PERFORM DRUCK.
           MOVE "Gesamtsumme " TO DRS-TEXT.
           MOVE WT-ERLOES(23) TO DRS-BETRAG.
           PERFORM DRUCK.
      *------------------------------------------> Ust-Basis ausgeben <-
           MOVE 2 TO WZ-SCHALT
           MOVE "UST-Gruppe:" TO DRS-TEXT.
           MOVE 1 TO DRS-KZ.
           MOVE "%" TO DRS-PZ.
           MOVE WT-UST(1) TO DRS-UST.
           MOVE 0 TO WS-BET.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               ADD WT-ERLOES(WX) TO WS-BET.
           MOVE WS-BET TO DRS-BETRAG.
           PERFORM DRUCK.
           IF WT-ERLOES(21) not = 0 MOVE 2 TO DRS-KZ
               MOVE "%" TO DRS-PZ
               MOVE WT-UST(2) TO DRS-UST
               MOVE WT-ERLOES(21) TO DRS-BETRAG
               PERFORM DRUCK.
      *    EVALUATE WN-FKON(1:3)
      *        WHEN "BMD" NEXT SENTENCE
      *        WHEN "KWF" NEXT SENTENCE
      *        WHEN OTHER PERFORM SABU.
           PERFORM SABU.
       X.  CLOSE BUCHFAKT.
      *    EVALUATE WN-FKON(1:3)
      *        WHEN "BMD" CLOSE BMDFIB
      *        WHEN "KWF" CLOSE KWFIBU
      *        WHEN OTHER CLOSE BUCHUNG.
           CLOSE BUCHUNG.
       Z.  EXIT.
      ************************************************** BMD-Buchungen *
      *BMD-BUCH SECTION.
      *A.  INITIALIZE BM-BSATZ.
      *    MOVE 0 TO BM-SA.
      *    COMPUTE BM-KTO = BF-KTONR / 10.
      *    MOVE BF-DAT TO BM-BELDAT.
      *    IF BF-DAT < 1000000 ADD 19000000 TO BM-BELDAT.
      *    COMPUTE BM-GGKTO = WT-KTONR(1) / 10.
      *    MOVE BF-REN TO BM-BELNR.
      *    MOVE WS-DATUM TO BM-BUDAT.
      *    IF WS-DATUM < 1000000 ADD 19000000 TO BM-BUDAT.
      *    COMPUTE BM-MWST = WT-UST(BF-U1) * 100.
      *    MOVE 03 TO BM-STCD.
      *    MOVE 1 TO BM-SH.
      *    COMPUTE BM-BET = BF-BET * 100.
      *    COMPUTE BM-UST = BF-MW * 100.
      *    MOVE BM-BET TO BM-OPBET.
      *    MOVE BM-BUDAT(5:2) TO BM-PER(1:).
      *    MOVE SPACE TO BM-TEXT.
      *    MOVE "AR" TO BM-SYM.
      *    MOVE 77 TO BM-USER.
      *    MOVE 0 TO BM-BUKZ.
      *    MOVE "S" TO BM-GGBKZ.
      *    MOVE "A" TO BM-VBKZ.
      *    MOVE "*" TO BM-END.
      *    WRITE BM-BSATZ.
      *Z.  EXIT.
      ************************************************** KWF-Buchungen *
      *KWF-BUCH SECTION.
      *A.  MOVE SPACE TO KW-FSATZ.
      *    MOVE "" TO KW-AR01 KW-AR01 KW-AR02 KW-AR03 KW-AR04 KW-AR05
      *                KW-AR06 KW-AR07 KW-AR08 KW-AR09 KW-AR10 KW-AR11
      *                KW-AR12 KW-AR13 KW-AR14 KW-AR15 KW-AR16 KW-AR17
      *                KW-AR18 KW-AR19 KW-AR20 KW-AR21 KW-AR22 KW-AR23.
      *    MOVE "AR" TO KW-BSA.
      *    COMPUTE KW-SKTONR = BF-KTONR / 10 + 2000000.
      *    MOVE DE-TOUR TO WZ.
      *    COMPUTE WD-KTO = WT-KTONR(WZ) / 10.
      *    PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
      *        IF WD-KTO(WZ:1) not = SPACE
      *            MOVE WD-KTO(WZ:) TO KW-HKTONR(1:)
      *            exit perform.
      *    MOVE BF-TX TO KW-BUTEXT.
      *    MOVE BF-BET TO WD-BET.
      *    MOVE WD-BET(1:1) TO KW-BUBET(1:1).
      *    PERFORM VARYING WZ FROM 2 BY 1 UNTIL WZ > 8
      *        IF WD-BET(WZ:1) not = SPACE
      *            MOVE WD-BET(WZ:) TO KW-BUBET(2:)
      *            exit perform.
      *    MOVE BF-MW TO WD-BET.
      *    MOVE WD-BET(1:1) TO KW-UST(1:1).
      *    PERFORM VARYING WZ FROM 2 BY 1 UNTIL WZ > 8
      *        IF WD-BET(WZ:1) not = SPACE
      *            MOVE WD-BET(WZ:) TO KW-UST(2:)
      *            exit perform.
      *    MOVE BF-DAT TO WC-DATUM.
      *    PERFORM DATDREH.
      *    IF WZ-TAG < 50 COMPUTE WH-NUM = WZ-TAG + 2000
      *              else COMPUTE WH-NUM = WZ-TAG + 1900.
      *    DIVIDE 100 INTO WZ-DATUM.
      *    COMPUTE KW-BELDAT = WZ-DATUM * 10000 + WH-NUM.
      *    MOVE "-" TO KW-BELDAT(3:1) KW-BELDAT(6:1).
      *    MOVE BF-REN TO WD-KTO.
      *    PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 5
      *        IF WD-KTO(WZ:1) not = SPACE
      *            MOVE WD-KTO(WZ:) TO KW-BELNR(1:)
      *            exit perform.
      *    MOVE "410" TO KW-BUCODE.
      *    MOVE 0 TO KW-EURO.
      *    IF BF-B2 not = 0 GO G.
      *    MOVE KW-FSATZ TO WH-VD.
      *    PERFORM VERDICHT.
      *    MOVE WH-VD TO KW-FSATZ.
      *    WRITE KW-FSATZ.
      *    GO Z.
      *-----------------------------> Splittbuchung bei UST 10% / 20% <-
      *G.  MOVE KW-FSATZ TO WM-VD.
      *    MOVE "DIV" TO KW-HKTONR(1:).
      *    MOVE "4" TO KW-BUCODE.
      *    MOVE KW-FSATZ TO WH-VD.
      *    PERFORM VERDICHT.
      *    MOVE WH-VD TO KW-FSATZ.
      *    WRITE KW-FSATZ.                             *> Kundenbuchung
      *    MOVE WM-VD TO KW-FSATZ.
      *    MOVE BF-U1 TO WY.
      *    COMPUTE WM-UST = BF-B1 * WT-UST(WY) / 100.
      *    SUBTRACT WM-UST FROM BF-MW.
      *    COMPUTE WD-BET = BF-B1 + WM-UST.                *> 1. Brutto
      *    MOVE WD-BET(1:1) TO KW-BUBET(1:1).
      *    PERFORM VARYING WZ FROM 2 BY 1 UNTIL WZ > 8
      *        IF WD-BET(WZ:1) not = SPACE
      *            MOVE WD-BET(WZ:) TO KW-BUBET(2:)
      *            exit perform.
      *    MOVE WM-UST TO WD-BET.
      *    MOVE WD-BET(1:1) TO KW-UST(1:1).
      *    PERFORM VARYING WZ FROM 2 BY 1 UNTIL WZ > 8
      *        IF WD-BET(WZ:1) not = SPACE
      *            MOVE WD-BET(WZ:) TO KW-UST(2:)
      *            exit perform.
      *    MOVE "410" TO KW-BUCODE.
      *    MOVE KW-FSATZ TO WH-VD.
      *    PERFORM VERDICHT.
      *    MOVE WH-VD TO KW-FSATZ.
      *    WRITE KW-FSATZ.                              *> 1. Erlîkonto
      *    MOVE WM-VD TO KW-FSATZ.
      *    COMPUTE WD-KTO = WT-KTONR(21).              *> Erlîskonto 20
      *    PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
      *        IF WD-KTO(WZ:1) not = SPACE
      *            MOVE WD-KTO(WZ:) TO KW-HKTONR(1:)
      *            exit perform.
      *    MOVE WD-BET(1:1) TO KW-BUBET(1:1).
      *    COMPUTE WD-BET = BF-B2 + BF-MW.              *> 2. Brutto
      *    PERFORM VARYING WZ FROM 2 BY 1 UNTIL WZ > 8
      *        IF WD-BET(WZ:1) not = SPACE
      *            MOVE WD-BET(WZ:) TO KW-BUBET(2:)
      *            exit perform.
      *    MOVE BF-MW TO WD-BET.
      *    MOVE WD-BET(1:1) TO KW-UST(1:1).
      *    PERFORM VARYING WZ FROM 2 BY 1 UNTIL WZ > 8
      *        IF WD-BET(WZ:1) not = SPACE
      *            MOVE WD-BET(WZ:) TO KW-UST(2:)
      *            exit perform.
      *    MOVE "420" TO KW-BUCODE.
      *    MOVE KW-FSATZ TO WH-VD.
      *    PERFORM VERDICHT.
      *    MOVE WH-VD TO KW-FSATZ.
      *    WRITE KW-FSATZ.                              *> 2. Erlîkonto
      *Z.  EXIT.
      ******************************************************************
       SABU SECTION.
       A.  MOVE 1 TO WZ.
           MOVE WH-RENUM TO DRT-NR1.
           MOVE "bis:" TO DRT-BIS.
           MOVE WH-ERST TO DRT-NR.
      *-----------------------------------------------> 3 Erlîskonten <-
       E.  IF WT-ERLOES(WZ) = 0 GO K.
           INITIALIZE BU-SATZ.
           MOVE WT-KTONR(WZ) TO BU-KTONR BU-REN.
           MOVE WS-DATUM TO BU-DAT.
           MOVE DRT-TX(36:) TO BU-TX.
           MOVE 19 TO BU-SY.
           MOVE 1 TO BU-SH.
           MOVE WT-ERLOES(WZ) TO BU-BET.
       G.  ADD 1 TO WK-BUKEY.
           MOVE WK-BUKEY TO BU-KEY.
           WRITE BU-SATZ INVALID GO G.
       K.  IF WZ < 21 ADD 1 TO WZ GO E.
           MOVE SPACE TO DRT-SATZ.
       Z.  EXIT.
      ************************************************ Read Konstanten *
       READ-KONS SECTION.
       A.  MOVE 36 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO X.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 21
               IF KO-KTONR(WX)(1:3) = SPACE
                   MOVE 0 TO KO-KTONR(WX) end-if
               MOVE KO-KTONR(WX) TO WT-KTONR(WX).
           MOVE 1 TO WX WH-KEY.
       C.  READ KONSTANT IGNORE LOCK.
           MOVE KO-RENUM TO WH-RENUM.
           MOVE 6 TO WH-KEY.
       D.  READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO D.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 2
               IF KO-ERLOES(WX + 6) not = 0 AND WT-KTONR(WX) = 0 GO X.
           MOVE KO-ERLOES(7) TO WT-ERLOES(1).
           MOVE KO-ERLOES(8) TO WT-ERLOES(2).
           GO Z.
       X.  DISPLAY "Erlîskonten nicht angelegt" with highlight
               AT 2401 PERFORM WEITER.
           SET ESC TO TRUE.
       Z.  EXIT.
      ******************************************************************
       TAGKOPF SECTION.
       A.  IF WZ-ZEILEN < 62 GO Z.
           MOVE "öbertrag" TO DRS-TEXT.
           MOVE WH-WERT TO DRS-BETRAG.
           PERFORM DRUCK.
           WRITE DRA-SATZ AFTER PAGE.
           MOVE 0 TO WZ-ZEILEN.
           MOVE 3 TO WZ-SCHALT.
           MOVE "öbertrag per:" TO DRS-TEXT.
           MOVE VDU-DATUM TO DRS-TEXT(15:9).
           MOVE WH-WERT TO DRS-BETRAG.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ************************************* Steuerzeichen LASER laden **
       LADE-DRU SECTION.
      *--------> Lasenzuordnung sollte Åber KONSTANT erledigt werden! <-
      *                                   1  *> quer A4 /10" 6 Zeilen <-
           MOVE "&l26a4h6d1O(s0p10h0b0s3T" TO WE-STG(1).
      *                                   2  *> quer A4 /12" 6 Zeilen <-
           MOVE "&l26a4h6d1O(s0p12h0b0s3T" TO WE-STG(2).
      *                                   3  *> quer A4 /16" 6 Zeilen <-
           MOVE "&l26a4h6d1O(s0p14.6h0b0s3T" TO WE-STG(3).
      *                                   4  *> hoch A4 /10" 6 Zeilen <-
           MOVE "&l26a4h6d0O(s0p10h0b0s3T" TO WE-STG(4).
      *                                   5  *> hoch A4 /12" 6 Zeilen <-
           MOVE "&l26a4h6d0O(s0p12h0b0s3T" TO WE-STG(5).
      *                                   6  *> hoch A4 /16" 6 Zeilen <-
           MOVE "&l26a4h6d0O(s0p14.6h0b0s3T" TO WE-STG(6).
      *                                   7  *> wie 5 nur fetter      <-
           MOVE "&l26a4h6d0O(s0p10h1b0s6T" TO WE-STG(7).
      *                                   8  *> wie 2 etwas fetter    <-
           MOVE "&l26a4h6d1O(s0p12h1b0s6T" TO WE-STG(8).
      *                                   9  *> quer A4 /16" 6 Zeilen <-
           MOVE "&l26a4h6d1O(s0p20.0h0b0s6T" TO WE-STG(9).
      *    &la: Format, c: 6 Zeilen, 1/0 O: quer/hoch
      *    (sp: Abstand, h: Zeich./Zoll b: StÑrke s: Schrift T: Schrift
       Z.  EXIT.
      ******************************************************************
       fakorr section.
       a.  if wm-datum > 20081021 go z.
           move low-values to fa-satz.
           move 0 to de-ktonr.
           start faktdat key not < fa-key invalid go z.
       c.  read faktdat next at end go z.
           if fa-rmon(1) = 0809 move 1 to wx go g.
           if fa-rmon(2) = 0809 move 2 to wx go g.
           if fa-rmon(3) = 0809 move 3 to wx go g.
           go c.
       g.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 31
      *        MOVE FA-RMENGE(WX, WZ) TO FA-MENGE(WZ)
               MOVE 0 TO FA-MENGE(WZ)
               MOVE FA-RRETOUR(WX, WZ) TO FA-RETOUR(WZ).
           move 0809 to fa-mon.
           rewrite fa-satz.
           go c.
       z.  exit.
      ***************************************** Auslesen DESADV hogast *
       HOGAST SECTION.
       A.  CALL "CAUP" USING "271010056000014" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Hogast - Desadv " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
       C.  DISPLAY "Monat: jjmm  " AT VDU-LP.
           CALL "CAUP" USING "1002104004" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           MOVE WH-NUM TO WX-MON WD-MON
           DISPLAY WD-MON with highlight AT VDU-LP.
       E.  DISPLAY "<Einfg>= Start, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023330000" WH-CREG.
           IF ESC GO X.
           IF NOT EINF GO E.
      *--------------------------------------> aktuelle Daten lîschen <-

           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunde: " AT VDU-LP.
           ADD 310 VDU-ECK GIVING VDU-LP.
           PERFORM FAKORR.
           INITIALIZE FA-SATZ.
           GO K.
           START FAKTDAT KEY not < FA-KEY INVALID STOP RUN.
       G.  READ FAKTDAT NEXT IGNORE LOCK AT END GO K.
           MOVE WX-MON TO FA-MON.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE 0 TO FA-MENGE(WX).
           DISPLAY FA-KTONR AT VDU-LP.
           REWRITE FA-SATZ.
           GO G.
      *---------------------------------> Verzeichnis DESADV auslesen <-
       K.  MOVE 0 TO WL-ACT WL-ATTRIB.
           MOVE "desadv\*.*" TO WL-FILEIN WN-EDIFAC.
       M.  CALL x"91" USING WL-RESULT WL-FUNKT WL-PARM.
           IF WL-ERR not = 0 GO W.
           IF WL-ACT = 0 MOVE 1 TO WL-ACT.
           IF WL-FOUT(1:4) not = "KEDE" GO M.
           MOVE WL-FOUT TO WN-EDIFAC(8:).
           PERFORM VERARBEITEN.
           IF not ESC GO M.
       W.  CLOSE EDIFACT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       VERARBEITEN SECTION.
       A.  MOVE 1 TO WB-AC.                               *> read only
           MOVE 0 TO WB-DE WB-DV WB-FLG WB-OFS IC.
           MOVE SPACE TO WB-FH.
           MOVE 1 TO WB-CNT IQ.
           MOVE 10 TO WX.
           MOVE SPACE TO EDI-SATZ.
      *    MOVE 0 TO WH-DTM WH-LFDAT WH-DP WH-BY
      *              WH-UC WH-CN WH-IV WH-UC WH-OB WH-EMPF.
           CALL "CBL_OPEN_FILE" USING WN-EDIFAC WB-AC WB-DE WB-DV WB-FH.
       E.  MOVE SPACE TO WU-Z.
           CALL "CBL_READ_FILE" USING WB-FH WB-OFS WB-CNT WB-FLG WU-Z.
           IF RETURN-CODE = 10
               CALL "CBL_CLOE_FILE" USING WB-FH GO Z.
           IF RETURN-CODE not = 0 GO K.
           ADD WB-CNT TO WB-OFS.
           EVALUATE WU-Z
               WHEN x"0D"
               WHEN x"0A" NEXT SENTENCE
               WHEN  "'"  GO G
               WHEN OTHER MOVE WU-Z TO EDI-SATZ(IQ:1)
                          ADD 1 TO IQ.
           GO E.
      *-----------------------------------------> Segmente abarbeiten <-
       G.  MOVE 1 TO IQ IC.
           IF WB-OFS < 100
               EVALUATE EDI-SATZ(1:5)
                   WHEN "UNB+U"
                   WHEN "UNA:+" MOVE SPACE TO EDI-SATZ
                                GO E
                   WHEN OTHER GO X.                     *> kein Edifact
           MOVE EDI-SATZ TO WT-GRP.
           MOVE SPACE TO WU-SEG WU-QUAL.
           SET RET TO TRUE.
           EVALUATE WT-GRP(1:4)
               WHEN "UNB+" NEXT SENTENCE
               WHEN "UNH+" INITIALIZE LF-SATZ
               WHEN "BGM+" PERFORM BGM-NR                *> Lfs.-Nr.
               WHEN "DTM+" PERFORM DTM-DAT               *> Datum
               WHEN "NAD+" PERFORM NAD-DES
               WHEN "LIN+" PERFORM BEST-AR
               WHEN "QTY+" PERFORM QTY-MG
                           PERFORM FAKT-ZEIL
               WHEN "UNS+"                           *> Kunde abgeschl.
               WHEN "UNT+" GO E
               WHEN "UNZ+" MOVE 0 TO WT-TX.           *> is fia nix
           MOVE SPACE TO EDI-SATZ.
           IF not ESC GO E.
       K.  CALL "CBL_CLOSE_FILE" USING WB-FH.
           IF ESC GO X.
      *    CALL "CBL_DELETE_FILE" USING WN-EDIFAC.          *> lîschen
       X.
       Z.  EXIT.
      ******************************************************************
       FAKT-ZEIL SECTION.
       A.  MOVE WX-MON TO FA-MON.
           MOVE DE-KTONR TO FA-KTONR.
           MOVE AR-NUM TO FA-ARNUM.
           MOVE 1 TO FA-ART.
           READ FAKTDAT INVALID NEXT SENTENCE.
           MOVE WH-DTM TO WZ-DATUM.
           SET OX TO WZ-TAG.
           MOVE WH-MG TO FA-MENGE(OX).
           REWRITE FA-SATZ INVALID WRITE FA-SATZ.
       Z.  EXIT.
      ******************************************************************
       BGM-NR SECTION.
       A.  MOVE 1 TO WX.
           MOVE 9 TO WZ.
           EVALUATE WT-GRP(5:3)
               WHEN 220
               WHEN 351 UNSTRING WT-GRP DELIMITED BY "+"
                                 INTO WU-SEG WU-QUAL WU-TEXT WT-TX
               WHEN OTHER GO Z.
       Z.  EXIT.
      ******************************************************************
       DTM-DAT SECTION.
       A.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = ":"
                CONTINUE.
           ADD 1 TO WZ.
           MOVE 0 TO WX.
           PERFORM VARYING WZ FROM WZ BY 1 UNTIL WT-GRP(WZ:1) = ":"
               or WZ > 19 ADD 1 TO WX
               EVALUATE WT-GRP(5:1)
                   WHEN 1 MOVE WT-GRP(WZ:1) TO WH-DTM(WX:1)
                   WHEN 2 MOVE WT-GRP(WZ:1) TO WH-LFDAT(WX:1).
       Z.  EXIT.
      ******************************************************************
       NAD-DES SECTION.
       A.  EVALUATE WT-GRP(5:3)
               WHEN "BY+" MOVE WT-GRP(8:13) TO WH-BY
               WHEN "OB+" MOVE WT-GRP(8:13) TO WH-OB
               WHEN "IV+" MOVE WT-GRP(8:13) TO WH-IV
               WHEN "UC+" MOVE WT-GRP(8:13) TO WH-UC WH-EMPF
                          GO C
               WHEN "CN+" MOVE WT-GRP(8:13) TO WH-CN WH-EMPF GO C
               WHEN "SU+" MOVE WT-GRP(8:13) TO WH-SU
               WHEN "DP+" MOVE WT-GRP(8:13) TO WH-DP WH-EMPF
                          GO C.
           GO Z.
      *-----------------------------------------> Lieferadresse lesen <-
       C.  MOVE WH-EMPF TO DE-GLN.
           MOVE 0 TO DE-KTONR.
           START DEBITOR KEY > DE-EANKEY INVALID GO X.
           READ DEBITOR NEXT IGNORE LOCK AT END GO X.
           IF DE-GLN not = WH-EMPF GO X.
           GO Z.
       X.  DISPLAY "Kunde nicht gefunden" AT 1301.
           PERFORM WEITER.
       Z.  EXIT.
      ************************************************** Bestellzeilen *
       BEST-AR SECTION.
       A.  IF WH-EMPF not = DE-GLN SET ESC TO TRUE GO Z.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WT-GRP(WZ:1) = ":"
               continue.
           MOVE 13 TO WF.
           MOVE 0 TO WH-NUM.
           PERFORM VARYING WX FROM WZ BY -1 UNTIL WT-GRP(WX:1) = "+"
               EVALUATE WT-GRP(WX:1)
                   WHEN 0 THRU 9 MOVE WT-GRP(WX:1) TO WH-NUM(WF:1)
                                 ADD -1 TO WF
                   WHEN OTHER MOVE 0 TO WH-HILF.
           MOVE WH-NUM TO AR-NUM WK-ARNUM.
       G.  MOVE "A" TO AR-SA.
           READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel fehlt" TO AR-BEZ.
           MOVE AR-NUM TO WK-ARNUM.
       Z.  EXIT.
      ****************************************** Mengen aus DESADV-DHL *
       QTY-MG SECTION.
       A.  MOVE 0 TO WH-MG WR WH-NEG.
           UNSTRING WT-GRP DELIMITED BY ":" or "+"
                           INTO WU-SEG WU-QUAL WH-MCODE WU-MEH.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WH-MCODE(WZ:1) = " "
               EVALUATE WH-MCODE(WZ:1)
                   WHEN "." MOVE 6 TO WR
                   WHEN "-" MOVE 1 TO WH-NEG
                   WHEN ":" exit perform
                   WHEN 0 THRU 9
                          IF WR = 0 COMPUTE WH-MG = WH-MG * 10
                               MOVE WH-MCODE(WZ:1) TO WH-MG(5:1)
                          else MOVE WH-MCODE(WZ:1) TO WH-MG(WR:1)
                               ADD 1 TO WR.
           IF WH-NEG = 0 MOVE WH-MG TO WH-WERT
           else COMPUTE WH-WERT = WH-MG * -1.
       Z.  EXIT.
