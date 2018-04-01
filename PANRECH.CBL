      $SET LINKCOUNT "512" ANS85"SYNTAX" BOUND AUTOLOCK NOALTER
      $SET SEGSIZE "50000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANRECH.
      ************************************************* EDIFACT-Aufbau *
      *   UNA+  UNB+ (gln-Absender + gln-EmpfÑnger)                    *
      *              UNH+   je Rechnungskopf                           *
      *                   LIN+ LIN+ .. Artikelzeilen         WH-LIN    *
      *              UNT+                                    WH-SEG    *
      *         UNZ+                                         WH-UNH    *
      *----------------------------------------------------------------*
      *     WH-LIN = LIN+ ZÑhler gesamt          steht in LIN+WH-LIN   *
      *     WH-UNH = UNH+ ZÑhler gesamt          steht in UNZ+WH-UNH   *
      *     WH-SEG = Anz. Seqmente im LIN+       steht in UNT+WH-SEG   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEART.CPY.
           COPY PANSEKONS.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEDAU.CPY.
           COPY PANSESTA.CPY.
           COPY PANSEINK.CPY.
           SELECT BUCHFAKT   ASSIGN TO "PANISFIB.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BF-KEY
                             ALTERNATE KEY BF-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT PROTOK     ASSIGN TO "PANEPROT.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT RECHNUNG   ASSIGN TO "PANERECH.DAT"
                             FILE STATUS IS WF-STATUS.
           SELECT ARCHIV     ASSIGN TO WH-ARCHIV         *> Druckdatei
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT ABLAGE     ASSIGN TO WN-FORM
                             ORGANIZATION RECORD SEQUENTIAL
                             FILE STATUS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
       COPY PANSTAT.CPY.
       COPY PANLIEF.CPY.
       COPY PANEBUCH.CPY.
       COPY PANFDINK.CPY.
      ************************************** Rechnungsarchivdruckdatei *
       FD  ARCHIV                      LABEL RECORD OMITTED.
       01  AC-ASATZ.
           03  AC-RENUM                PIC 9(5).
           03  AC-KTONR                PIC 9(5).
           03  AC-MCODE.
               05 AC-REDAT             PIC 9(6).
               05 FILLER               PIC XX.
       01  AC-SATZ                     PIC X(132).
      ************************************************* Rechnungsdatei *
       FD  RECHNUNG                   LABEL RECORD STANDARD.
       01  RE-SATZ.
           03  RE-ARNUM                PIC 9(14)      COMP.
           03  RE-BEZ                  PIC X(25).
           03  RE-MEH                  PIC 99         COMP.
           03  RE-ARPL                 PIC 99         COMP.
           03  RE-MENGE                PIC S9(5)V99   COMP.
           03  RE-PREIS                PIC S9(4)V999  COMP.
           03  RE-RETBEZ               PIC X(25).
           03  RE-RETMG                PIC S9(5)V99   COMP.
           03  RE-RETPR                PIC S9(4)V999  COMP.
           03  RE-RAB                  PIC S99V9      COMP.
           03  RE-UST                  PIC 99         COMP.
           03  RE-WDMG                 PIC X(11).
           03  RE-RWDMG                PIC X(11).
           03  RE-BET                  PIC S9(7)V99   COMP.
           03  RE-GRM                  PIC X(5).
           03  RE-EKP                  PIC S9(4)V99.
      **************************************************** Lieferdaten *
       FD  PROTOK       external       LABEL RECORD STANDARD.
       01  PR-SATZ.
           03  PR-ARNUM                PIC 999.
           03  PR-MENGE                PIC S9(7)V99.
           03  PR-RETMG                PIC S9(7)V99.
      ******************************************************************
       FD  ABLAGE                      LABEL RECORD OMITTED.
       01  ABA-SATZ.
           03 ABA-SCHALT               PIC 99.
           03 ABA-REST                 PIC X(134).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(242).
       01  DRB-SATZ.
           03  FILLER                  PIC X(6).
           03  DRB-TX.
               05 FILLER               PIC X(10).
               05 DRB-ADR.
                  07 FILLER            PIC X(25).
                  07 DRB-RTX           PIC X(5).
                  07 DRB-RENUM         PIC ZZ.ZZ9-.
                  07 DRB-STR           PIC X.
                  07 DRB-KTONR         PIC ZZ.ZZ9,9.
                  07 DRB-ORT           PIC X(28).
                  07 DRB-TOUR          PIC ZZZ9-.
               05 FILLER               PIC X(20).
       01  DRC-SATZ.
           03  FILLER                  PIC X(7).
           03  DRC-ARNUM               PIC Z(12)9-.
           03  DRC-BEZ                 PIC X(27).
           03  DRC-MENGE               PIC X(12).
           03  DRC-MEH                 PIC XXX.
           03  DRC-USTR.
               05 DRC-APREIS           PIC ZZ.ZZ9,999-.
               05 DRC-AP REDEFINES DRC-APREIS.
                  07 DRC-PREIS         PIC ZZ.ZZ9,99-.
                  07 FILLER            PIC X.
               05 DRC-PROZ             PIC Z9,9-.
               05 DRC-STR.
                  07 DRC-BETRAG        PIC ZZZZ.ZZ9,99-.
           03  DRC-UST                 PIC ZZ9.
       01  DRD-SATZ.
           03  FILLER                  PIC X(10).
           03  DRD-MWST.
               05  FILLER              PIC X(18).
               05  DRD-PROZ            PIC Z9,99.
               05  DRD-PZ              PIC XX.
               05  DRD-BEZ             PIC X(19).
               05  DRD-BASIS           PIC ZZZ.ZZ9,99-.
               05  FILLER              PIC X(18).
               05  DRD-STR             PIC X(13).
       01  DRP-SATZ.
           03  FILLER                  PIC X(27).
           03  DRP-RETX.
               05 FILLER               PIC X(35).
               05 DRP-RENR             PIC X(5).
               05 DRP-RENUM            PIC ZZ.ZZ9-.
               05 DRP-STR              PIC X.
               05 DRP-KTO              PIC X(13).
       01  DRU-SATZ.
           03  FILLER                  PIC X(18).
           03  DRU-TX                  PIC X(20).
           03  DRU-AB                  PIC X(11).
           03  DRU-BX                  PIC X(5).
           03  DRU-BIS                 PIC X(10).
       01  DRL-SATZ.
           03  FILLER                  PIC XX.
           03  DRL-STR.
               05 DRL-KTONR            PIC ZZ.ZZ9,9-.
               05 DRL-BEZ              PIC X(34).
               05 DRL-RENUM            PIC ZZZZ.ZZ9-.
               05 DRL-BET              PIC ZZZ.ZZ9,99-.
      *-------------------------------------------------> Rasterdruck <-
       01  DRM-SATZ.
           03  DRM-NO                  PIC X(8).
           03  DRM-NUM                 PIC ZZ.ZZZ,9-.
           03  DRM-NAME                PIC X(139).
       01  DRQ-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRQ-STR.
               05 DRQ-NAME             PIC X(45).
               05 FILLER               PIC XX.
               05 DRQ-ILN              PIC 9(13)-.
               05 DRQ-RENR             PIC ZZZ.ZZ9-.
               05 DRQ-DATUM            PIC X(10).
               05 DRQ-WERT             PIC ZZZ.ZZ9,99-.
               05 DRQ-SKONTO           PIC ZZZ.ZZ9,99-.
               05 DRQ-EMBALL           PIC ZZZ.ZZ9,99-.
               05 DRQ-UST              PIC ZZZ.ZZ9,99-.
               05 DRQ-BETRAG           PIC ZZZ.ZZ9,99-.
       01  DRR-SATZ.
           03  FILLER                  PIC X(8).
           03  DRR-STR.
               05 DRR-ARNUM            PIC ZZ9-.
               05 DRR-BEZ              PIC X(26).
               05 DRR-MG                          OCCURS 31 INDEXED DY.
                  07 DRR-MA            PIC ZZZZ9-.
                  07 DRR-MB REDEFINES
                     DRR-MA            PIC ZZZ,9-.
                  07 DRR-R REDEFINES DRR-MA.
                     09 DRR-TG         PIC ZZZ9.
                     09 DRR-P          PIC XX.
               05 DRR-SU               PIC X(7).
       01  DRW-SATZ.
           03  FILLER                  PIC X(16).
           03  DRW-STR.
               05 DRW-ATSEUR           PIC X(4).
               05 DRW-NTTO             PIC Z.ZZZ.ZZ9,99-.
               05 DRW-UST              PIC Z.ZZZ.ZZ9,99-.
               05 DRW-BTTO             PIC Z.ZZZ.ZZ9,99-.
       01  DRY-SATZ.
           03  DRY-X                   PIC X   OCCURS 242  INDEXED XX.
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
       01  WH-ARCHIV.
           03  FILLER                  PIC XXX      VALUE "PAC".
           03  WH-ARCNR                PIC 9(5).
           03  FILLER                  PIC XXXX     VALUE ".DAT".
       01  WH-REG.
           03  WE-EXT                  PIC XXX.
           03  WK-EANNR                PIC 9(13).
           03  WK-GLN                  PIC 9(13).
           03  WV-GLN                  PIC 9(13).
           03  WV-ILN                  PIC 9(13).
           03  WH-GRENZ                PIC S999V99   COMP-3 VALUE ZERO.
           03  WK-D                    PIC 9999      COMP.
           03  WH-PX                   PIC XX         OCCURS 2.
           03  WM-MERKER               PIC 9         COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WF-OPEN                 PIC 99        COMP.
           03  WZ-SIZE                 PIC 99        COMP.
           03  WM-OPEN                 PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP-3 VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP-3 VALUE ZERO.
           03  WI                      PIC 9(6)      COMP.
           03  IX                      PIC 9(6)      COMP.
           03  IY                      PIC 99        COMP.
           03  WM-RETVER               PIC 99        COMP.
           03  WI-TG                   PIC 99        COMP.
           03  WM-RAST                 PIC 99        COMP.
           03  WK-DS.
               05 FILLER               PIC X(5)       VALUE "per:".
               05 WK-DAT               PIC X(10).
               05 FILLER               PIC X(7)       VALUE "Seite: ".
               05 WK-SEITE             PIC ZZ9-.
           03  WX-DS REDEFINES WK-DS   PIC X     OCCURS 26 INDEXED DX.
           03  WH-VON                  PIC 9999.
           03  WK-VON                  PIC 9999      COMP.
           03  WX-FIL                  PIC X(37).
           03  WH-TX                   PIC X(51).
           03  WK-RM                   PIC 99        COMP.
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WH-MOD                  PIC 99        COMP.
               88  MANUEL   VALUE 0.    88  SOFORT    VALUE 6.
               88  MON      VALUE 1 2.  88  HALB      VALUE 5.
               88  WOCH     VALUE 3 4.
           03  WX-M                    PIC 99        COMP.
           03  WX-J                    PIC 99        COMP.
           03  WH-RAB                  PIC S99V9     COMP-3.
           03  WV-ARNUM                PIC 9(4)      COMP.
           03  WH-RENUM                PIC 9(6)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WT-KTONR                PIC 9(6)      COMP.
           03  WV-LFNUM                PIC 9(5)      COMP.
           03  WV-ULT                  PIC 99        COMP.
           03  WV-TOUR                 PIC 99        COMP.
           03  WH-ABDAT                PIC 9(8)      COMP.
           03  WH-BISDAT               PIC 9(8)      COMP.
           03  WH-RET                  PIC 999V9     COMP.
           03  WV-RETDAT               PIC 9(8)      COMP.
           03  WH-SAKEY                PIC 9(5).                 
           03  WH-IKEY                 PIC 9(5).
           03  WH-ANZ                  PIC S9(8)V99.
           03  WM-ANZ                  PIC S9(8)V99.     *> wg. Edifact
           03  WA-ANZ                  PIC 9(5)V99.
           03  WR-ANZ REDEFINES WA-ANZ.
               05  FILLER              PIC 9(5).
               05  WR-A1               PIC 9.
               05  WR-A2               PIC 9.
           03  WH-ANZ1                 PIC S9999V9.
           03  WH-ANZ2                 PIC S999V99.
           03  WH-L                    PIC 99        COMP.
           03  WD-TOUR                 PIC Z9.
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
           03  WW-NTTO                 PIC S9(7)V99   COMP-3.
           03  WW-BTTO                 PIC S9(7)V99   COMP-3.
           03  WH-PREIS                PIC S9(5)V999  COMP-3.
           03  WR-PR REDEFINES WH-PREIS.
               05 FILLER               PIC X(4).
               05 WR-GR                PIC S9         COMP-3.
           03  WH-NADSU                PIC 9(13).
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  WT-BASIS                PIC S9(7)V99   COMP-3 OCCURS 6.
           03  WH-USTKZ                PIC 9.
           03  WH-MEH                  PIC 99         COMP.
           03  WM-RE                   PIC 99         COMP.
           03  WP-RET                  PIC S9(6)V99   COMP-3.
           03  WV-BON                  PIC S999V99    COMP-3.
           03  WV-RET                  PIC S999V99    COMP-3.
           03  WH-STAT                 PIC S9(9)V99   COMP-3.
           03  WH-BET                  PIC S9(9)V99   COMP-3.
           03  WK-RET                  PIC S9(9)V99   COMP-3.
           03  WS-RET                  PIC S9(9)V99   COMP-3.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
           03  WH-MWST                 PIC S9(9)V99   COMP-3.
           03  WH-UST                  PIC S9(9)V99   COMP-3.
           03  WS-UST                  PIC S9(9)V99   COMP-3.
           03  WS-BONUS                PIC S9(9)V99   COMP-3.
           03  WS-SUM                  PIC S9(9)V99   COMP-3 OCCURS 9.
           03  WT-MENGE                PIC S9(7)      COMP-3 OCCURS 10.
           03  WT-RETM                 PIC S9(7)      COMP-3 OCCURS 10.
           03  WT-PREIS                PIC S9(4)V999  COMP-3 OCCURS 10.
           03  WT-RETP                 PIC S9(4)V999  COMP-3 OCCURS 10.
           03  WT-RAB                  PIC S99V9      COMP-3 OCCURS 10.
           03  WS-MENGE                PIC S9(7)V99   COMP.
           03  WS-RETM                 PIC S9(7)V99   COMP.
           03  WM-BEZ                  PIC X(110).
           03  WH-DEBEZ                PIC X(110).
           03  WH-EFKTONR              PIC 9(5)       COMP.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WD-X                    PIC X.
           03  WM-ARC                  PIC 99         COMP.
           03  WO-TAGE                 PIC 99         COMP  OCCURS 31.
           03  WT-FTAG.
               05  FILLER              PIC 9(4)    COMP   OCCURS 4.
               05  FILLER              PIC 9(4)    COMP   VALUE 0101.
               05  FILLER              PIC 9(4)    COMP   VALUE 0106.
               05  FILLER              PIC 9(4)    COMP   VALUE 0501.
               05  FILLER              PIC 9(4)    COMP   VALUE 0815.
               05  FILLER              PIC 9(4)    COMP   VALUE 1026.
               05  FILLER              PIC 9(4)    COMP   VALUE 1101.
               05  FILLER              PIC 9(4)    COMP   VALUE 1208.
               05  FILLER              PIC 9(4)    COMP   VALUE 1225.
               05  FILLER              PIC 9(4)    COMP   VALUE 1226.
           03  WF-FTAG REDEFINES
               WT-FTAG                 PIC 9(4)    COMP  OCCURS 13.
           03  WK-FTAG                 PIC 9(6)    COMP  OCCURS 20.
           03  WK-JAHR                 PIC 9(4)    COMP.
           03  WK-UID.
               05 WK-ATU               PIC XXX.
               05 WK-ATUNR             PIC 9(8).
           03  WK-ILN                  PIC 9(14)    COMP.
           03  WK-ADR                  PIC X(110).
           03  WK-RETX                 PIC X(60).
           03  WM-UST                  PIC 99.
           03  VD-GZDAT.
               05 VD-ORT               PIC X(17).
               05 FILLER               PIC X.
               05 VD-DATUM.
                  07 VD-TAG            PIC 99.
                  07 VD-A              PIC X       VALUE ".".
                  07 VD-MONAT          PIC 99.
                  07 VD-B              PIC X       VALUE ".".
                  07 VD-JAHR           PIC 9999.
      *-----------------------------------------------------> edifact <-
           03  WH-TIME.
               05  WH-HHMM             PIC 9(4)      VALUE 0.
               05  WH-SSHH             PIC 9(4)      VALUE 0.
           03  WN-EDIFACT              PIC X(20).
           03  WN-EDICUS               PIC X(20).
           03  WN-SICHB                PIC X(20).
           03  WH-KONTROL              PIC X(15).
           03  WK-UNZ                  PIC X(15).
           03  WM-CD                   PIC X(35).
           03  WH-LIN                  PIC 9999.      *> LIN+ ZÑhler
           03  WH-UNH                  PIC 9999.      *> UNH+ ZÑhler
           03  WH-SEG                  PIC 9999.      *> Pos. im LIN+
           03  WE-ANZ                  PIC ZZZ9.
           03  WE-NUM                  PIC ZZZZZ9.
           03  WE-PREIS                PIC ZZZ9,999.
           03  WE-BET                  PIC ZZZZZ9,99.
           03  WM-REDAS                PIC 9       COMP.
           03  WE-NTG                  PIC 999.
           03  WE-GLN                  PIC 9(13).
      *-------------------------------------------> Byte-Stream-DATEI <-
           03  WB-SATZ                 PIC X(160).
           03  WB-AC                   PIC X     COMP-X.
           03  WB-DE                   PIC X     COMP-X.
           03  WB-DV                   PIC X     COMP-X.
           03  WB-FH                   PIC X(4).
           03  WB-OFS                  PIC X(8)  COMP-x.
           03  WB-CNT                  PIC X(4)  COMP-x.
           03  WB-FLG                  PIC X     COMP-x.
      *------------------------------------------------> Command-Line <-
           03  WD-UPON                 PIC X(60).
           03  RESULT                  PIC 99      COMP-X.
           03  FUNKT                   PIC 99      COMP-X  VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99      COMP-X  VALUE 0.
               05 PAR                  PIC X(40)   VALUE SPACE.
      *--------------------------------------------> check_file_exist <-
           03  WH-FNAME                PIC X(20).
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)   COMP-X.
               05 WF-TT                PIC 99     COMP-X.
               05 WF-MO                PIC 99     COMP-X.
               05 WF-YY                PIC 9999   COMP-X.
               05 WF-HH                PIC 99     COMP-X.
               05 WF-MM                PIC 99     COMP-X.
               05 WF-SS                PIC 99     COMP-X.
               05 WF-HS                PIC 99     COMP-X.
           03  WV-SIZE                PIC X(8)   COMP-X.
       01  WT-DK     external.
           03  WR-TN                PIC X(10)  OCCURS 7.
           03  WT-KON               PIC 99     OCCURS 12.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  CALL "CADECL" USING "PANEFAKT.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING "PANELFS.DAT "  WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "PANDEBIT.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  CALL "CADECL" USING "PANESTAT.DAT" WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON BUCHFAKT.
       A.  CALL "CADECL" USING "PANISFIB.DAT" WH-CREG.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON DAUER.
       A.  CALL "CADECL" USING "PANEBEST.DAT" WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON PROTOK.
       A.  CALL "CADECL" USING "PANEPROT.DAT" WH-CREG.
       DECL-R SECTION.         USE AFTER ERROR PROCEDURE ON RECHNUNG.
       A.  CALL "CADECL" USING "PANERECH.DAT" WH-CREG.
       DECL-Q SECTION.         USE AFTER ERROR PROCEDURE ON ARCHIV.
       A.  CALL "CADECL" USING WH-ARCHIV      WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON INKASSO.
       A.  CALL "CADECL" USING "PANINKAS.DAT" WH-CREG.
       DECL-L SECTION.         USE AFTER ERROR PROCEDURE ON ABLAGE.
       A.  CALL "CADECL" USING WN-FORM  WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE WH-KEY TO WK-D.
           MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID KEY STOP RUN.
           MOVE KO-ATU TO WK-ATU.
           MOVE KO-ATUNR TO WK-ATUNR.
           MOVE KO-TX TO WH-TX.
           MOVE "ABLAGE.LST" TO WN-FORM.
           MOVE 0 TO WF-OPEN WM-OPEN WM-RAST.
           IF WL-CA = 10 CLOSE DAUER LFSCHEIN
                         PERFORM FAKTURA GO X.
           IF WL-CA = 30 PERFORM LFRECH  GO X.
           GO Z.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR VD-JAHR
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT VD-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG VD-TAG.
           ADD 2000 TO VD-JAHR.
           MOVE VDU-ORT TO VD-ORT.
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
       A.  IF WH-DRUNAM(1:3) not = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              GO A.
       Z.  EXIT.
      ******************************************************************
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
           IF WM-DRU = 1 MOVE DRA-SATZ(3:) TO DRA-SATZ(1:)
           else IF WM-KO = 2 MOVE DRA-SATZ(10:) TO DRA-SATZ(5:).
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           IF WM-DRU = 1 MOVE WZ-SCHALT TO ABA-SCHALT
                         MOVE DRA-SATZ TO ABA-REST
                         WRITE ABA-SATZ.
       D.  IF DE-RAST > 2 or WH-MOD > 7 or WM-DRU = 3 GO E.
           WRITE AC-SATZ FROM DRA-SATZ AFTER WZ-SCHALT.
       E.  MOVE SPACE TO DRA-SATZ AC-SATZ ABA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       PAGEDRU SECTION.
       A.  WRITE DRA-SATZ AFTER PAGE.
           IF WF-STATUS = 27 GO A.
           IF WM-DRU = 1 MOVE 99 TO ABA-SCHALT
                         MOVE SPACE TO ABA-REST
                         WRITE ABA-SATZ.
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
       Z.  EXIT.
      ******************************* Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU not = 1 MOVE x"1B210000" TO DRA-SATZ(1:).
           PERFORM PAGEDRU.
           MOVE SPACE TO DRA-SATZ ABA-SATZ.
           MOVE 0 TO WM-OPEN WF-OPEN.
           CLOSE DRUCKER
           IF WM-DRU = 1 CLOSE ABLAGE.
       Z.  EXIT.
      ****************************************** Archivdatei schlie·en *
       END-ARC SECTION.
       A.  IF DE-RAST = 3 or WH-MOD > 7 GO Z.
           MOVE SPACE TO AC-SATZ.
           IF WM-ARC = 0 GO Z.
           CLOSE ARCHIV.
           MOVE 0 TO WM-ARC.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  PERFORM DRU-OK.
           IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           IF WH-DRUNAM(1:3) = "LPT" OPEN OUTPUT DRUCKER.
           IF WM-DRU = 0 OPEN EXTEND DRUCKER
                    else OPEN EXTEND DRUCKER ABLAGE
                         MOVE 1 TO WF-OPEN.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE X"1B21" TO DRA-SATZ(1:).
           MOVE WH-PX(1) TO DRA-SATZ(3:2).
      *    IF WM-DRU = 3 MOVE WE-STG(WH-P) TO DRA-SATZ.
      *    IF WM-DRU = 1 MOVE WE-STG(WH-P) TO DRA-SATZ
260512     IF WM-DRU = 1 or WM-DRU = 3
               MOVE WE-STG(WH-P) TO DRA-SATZ
               IF WF-OPEN = 1 MOVE 0 TO ABA-SCHALT
                              MOVE DRA-SATZ TO ABA-REST
                              WRITE ABA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           IF WM-DRU not = 0 GO G.
           MOVE X"1B43" TO DRA-SATZ(1:).
           MOVE WH-PX(2) TO DRA-SATZ(3:2).
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
       G.  MOVE SPACE TO DRA-SATZ ABA-SATZ.
       Z.  EXIT.
      *****************************************************************
       DISP-MENGE SECTION.
       A.  MOVE WH-ANZ TO WA-ANZ.
           IF WF-NK = 0 GO C.
           IF WF-NK = 1 GO B.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ2 ON SIZE ERROR GO X.
           MOVE WH-ANZ2 TO WD-ANZ2 WD-MGC.
           MOVE WD-ANZ2 TO WD-MENGE.
           MOVE WD-MGC  TO WD-MG.
           GO E.
       B.  IF WR-A2 NOT = 0 GO X.
           MULTIPLY WH-ANZ BY 1 GIVING WH-ANZ1 ON SIZE ERROR GO X.
           MOVE WH-ANZ1 TO WD-ANZ1 WD-MGB.
           MOVE WD-ANZ1 TO WD-MENGE.
           MOVE WD-MGB  TO WD-MG.
           GO E.
       C.  IF WH-ANZ > 9999999 OR WH-ANZ < -9999999 GO X.
           IF WR-A2 NOT = 0 AND WR-A1 NOT = 0 GO X.
           MOVE WH-ANZ TO WD-ANZ WD-MGA.
           MOVE WD-ANZ TO WD-MENGE.
           MOVE WD-MGA TO WD-MG.
       E.  DISPLAY WD-MENGE with highlight AT VDU-LP.
           GO Z.
       X.  DISPLAY "Menge zu gross" AT 2401
               PERFORM WEITER
               MOVE 7 TO WX-TASTE.
       Z.  EXIT.
      ***************************************** Mengenkommaumwandlung *
       MG SECTION.
       A.  IF WF-NK = 2 DIVIDE 100 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ2 WD-MGC
               MOVE WD-MGC TO WD-MG
               MOVE WD-ANZ2 TO WD-MENGE.
           IF WF-NK = 1 DIVIDE 10 INTO WH-ANZ
               MOVE WH-ANZ TO WD-ANZ1 WD-MGB
               MOVE WD-MGB TO WD-MG
               MOVE WD-ANZ1 TO WD-MENGE.
           IF WF-NK = 0 MOVE WH-ANZ TO WD-ANZ WD-MGA
               MOVE WD-MGA TO WD-MG
               MOVE WD-ANZ TO WD-MENGE.
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      **************************** autom. Rechnung statt Lieferschein *
       LFRECH SECTION.
       A.  CLOSE DAUER.
           MOVE 100 TO WP-RET.
           MOVE WH-BUKEY TO WH-IKEY.           *> KeyÅbergabe aus Lfsch.
           MOVE 50 TO WS-TASTE.
           MOVE 0 TO WM-RE WM-ARC.
           MOVE WE-DRU(1) TO WM-DRU.
           MOVE "SOFRECH.LST" TO WH-DRUNAM.
           MOVE DE-FAKART TO WH-MOD.
           PERFORM PERPRUEF.
           MOVE 9 TO WS-TASTE.
           MOVE 6 TO WH-MOD.
           IF ESC GO Z.
           DISPLAY " Sofortrechnung" AT 0103.
           DISPLAY "Kunden-Nr.:" AT 0301.
           MOVE DE-KTONR TO FA-KTONR WV-KTONR LF-KTONR.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0313.
           MOVE 1 TO WX.
           MOVE WM-DATUM TO WH-ABDAT WH-BISDAT WZ-DATUM.
           MOVE 0 TO WZ-ZEILEN WV-LFNUM LF-ARNUM LF-NUM.
           IF WH-PG = 9 COMPUTE LF-NUM = WZ-TAG * 100.
           OPEN EXTEND PROTOK.
           MOVE 0 TO PR-ARNUM.
           MOVE WM-DATUM TO PR-MENGE.
           IF WM-KO = 3 WRITE PR-SATZ.
           START LFSCHEIN KEY > LF-KEY INVALID GO X.
       C.  READ LFSCHEIN NEXT AT END GO X.
           IF WV-KTONR NOT = LF-KTONR OR LF-DATUM NOT = WV-DATUM GO X.
           IF LF-STAT = 1 GO C.
           IF LF-STAT = 95 or LF-STAT = 94 GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WZ-ZEILEN = 0 MOVE LF-NUM TO WV-LFNUM
               PERFORM FAKTKOPF
               MOVE DE-BEZ TO WT-BEZ
               INSPECT WT-BEZ REPLACING ALL "#" BY ","
               DISPLAY WT-BEZ with SIZE 78 AT 0401.
           IF LF-NUM NOT = WV-LFNUM PERFORM DRUCK
               MOVE LF-NUM TO WV-LFNUM.
           MOVE 7 TO VDU-L.
           CALL "CAUP" USING "1307010780000" WH-CREG.
           PERFORM UEBERTRAG.
           MOVE 0 TO WH-ANZ WH-BET WH-STAT.
           MOVE LF-ARNUM TO AR-NUM.
       I.  READ ARTIKEL IGNORE LOCK INVALID
               MOVE 1 TO AR-UST
               MOVE "Artikel gelîscht" TO AR-BEZ.
           MOVE AR-GRM TO WF-GRM.
           MOVE AR-PL TO RE-ARPL.
           MOVE 0 TO WH-RAB.
           IF LF-MENGE(1) = 0 AND LF-MENGE(2) = 0 AND LF-MENGE (3) = 0
               MOVE LF-PREIS TO WH-BET
               MOVE 0 TO WH-PREIS WH-ANZ WK-RM
               MOVE AR-BEZ TO WT-ADR GO O.
           MOVE DE-RAB TO WH-RAB.
           IF AR-RAB not = 0 MOVE 0 TO WH-RAB.
           IF AR-PL = 2
               ADD LF-MENGE(1) LF-MENGE(2) LF-MENGE(3) TO WH-BET
               IF WF-NK > 0 DIVIDE 10 INTO WH-BET
                   IF WF-NK > 1 DIVIDE 10 INTO WH-BET
               END-IF
               ADD WH-BET TO WS-SUM(8) GO S.
           PERFORM SOPREI.
       K.  ADD LF-MENGE(1) LF-MENGE(3) GIVING WH-ANZ.
           MOVE 0 TO WK-RM.
           IF WH-ANZ NOT = 0 GO M.
       L.  MOVE LF-MENGE(2) TO WH-ANZ.
           IF WH-ANZ = 0 GO Q.
           MOVE 1 TO WK-RM.
       M.  MOVE LF-ARNUM TO AR-NUM WD-ART.
           PERFORM EAN.
           DISPLAY WD-ART AT 0701.
           MOVE LF-BEZ TO DRC-BEZ WT-ADR.
           IF LF-MENGE(1) NOT = 0 AND WK-RM = 1
               MOVE "Retourmenge" TO WT-ADR DRC-SATZ(28:11).
           DISPLAY DRC-BEZ with highlight AT 0706.
           PERFORM MG.
           DISPLAY WD-MENGE with highlight AT 0732.
           ADD 1 WF-MEH GIVING WH-MEH.
           DISPLAY WT-MEH(WH-MEH) with highlight AT 0744.
           DISPLAY WD-PREIS with highlight AT 0748.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO WD-PROZ DRC-PROZ
               DISPLAY WD-PROZ with highlight AT 0760.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * WH-RAB / 100).
       O.  MOVE WH-BET TO WH-STAT.
           PERFORM ERLOEKTO.
       Q.  IF WK-RM = 0; IF LF-MENGE(2) NOT = 0 GO L.
           MOVE LF-ARNUM TO PR-ARNUM.
           MOVE LF-MENGE(1) TO PR-MENGE.
           MOVE LF-MENGE(2) TO PR-RETMG.
           IF WM-KO = 3 WRITE PR-SATZ.
       S.  DELETE LFSCHEIN.
           GO C.
       X.  IF WZ-ZEILEN NOT = 0 PERFORM FAKTENDE.
           PERFORM DUPLO.
           CLOSE STATISTIK.
           CLOSE PROTOK.
           OPEN I-O DAUER.
       Z.  EXIT.
      ********************************************* Sonntage ermitteln *
       WOTAGE SECTION.
       A.  MOVE WM-DATUM TO WV-DATUM.
           PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 20
               MOVE 0 TO WK-FTAG(IY).
           IF WH-MOD = 0 or 6 MOVE WV-DATUM TO WH-ABDAT.
           MOVE WH-ABDAT TO WZ-DATUM.
           MOVE WT-KON(WZ-MONAT) TO WI-TG.
           MOVE WZ-JAHR TO WK-JAHR.
           ADD 1900 TO WK-JAHR.
           IF WZ-JAHR < 30 ADD 100 TO WK-JAHR.
           if wz-monat = 2
               divide 4 into wk-jahr giving wh-num remainder wi-tg
               if wi-tg = 0 move 29 to wi-tg
                       else move 28 to wi-tg.
      *-----------------------------------------> Feiertagsermittlung <-
           CALL "CAUP" USING "03DATUM" WH-CREG.
      *---> Ostermon. * Chr. Himmelfahrt * Pfingstmon. * Frohnleichnam *
           MOVE 1 TO WY.
           ADD 29 WX GIVING WZ PERFORM TG.
           ADD 67 WX GIVING WZ PERFORM TG.
           ADD 78 WX GIVING WZ PERFORM TG.
           ADD 88 WX GIVING WZ PERFORM TG.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 13
               MOVE WF-FTAG(WY) TO WZ-DATUM WK-FTAG(WY)
               MOVE WK-JAHR TO WZ-JAHR
               MOVE WZ-DATUM TO WK-FTAG(WY).
           PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > WI-TG
               MOVE WH-ABDAT TO WZ-DATUM
               MOVE IY TO WZ-TAG
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 13
                   or WZ-DATUM = WK-FTAG(WY) CONTINUE
               end-perform
               IF WY > 13 CALL "CAUP" USING "03DATUM" WH-CREG
                          IF WO-TGN = 7 MOVE 1 TO WO-TAGE(IY)
                                   else MOVE 0 TO WO-TAGE(IY)
                          end-if
               else MOVE 1 TO WO-TAGE(IY).
           MOVE WV-DATUM TO WM-DATUM.        *> RÅckstellung s. 1. Zeile
       Z.  EXIT.
      ********************************************* Tageermittlung ab *
       TG SECTION.
       A.  MOVE 3 TO WF.
       B.  IF WZ > WT-KON(WF) SUBTRACT WT-KON(WF) FROM WZ
               ADD 1 TO WF GO B.
           MULTIPLY WF BY 100 GIVING WF-FTAG(WY).
           ADD WZ TO WF-FTAG(WY).
           ADD 1 TO WY.
       Z.  EXIT.
      ******************************************************************
       FAKTURA SECTION.
           MOVE 0 TO WZ-ZEILEN WH-UNH WH-LIN WH-SEG WM-REDAS.
       A.  MOVE "  Rechnungen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE WE-DRU(1) TO WM-DRU.
           MOVE 0 TO WZ-ZEILEN.
           PERFORM PERPRUEF.
           IF WH-PG = 7 GO W.
           IF ESC GO T.
           PERFORM WOTAGE.
           IF WH-MOD = 8 PERFORM DESAZEIL GO W.
      *    IF WH-MOD = 8 PERFORM INK-LIST GO W.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Kunden-Nr.:" AT 0301.
           UNLOCK DEBITOR.
           UNLOCK ARTIKEL.
           MOVE 0 TO DE-KTONR FA-KTONR FA-ARNUM WV-ARNUM WM-ARC.
           DISPLAY "<esc>= Ende, alpha+<ret>= Kunden suchen, <ret>= Kund
      -        "en-Nr. " AT 2301.
           IF WH-PG > 0 DISPLAY ", <ret-leer>= alles " AT 2359.
           CALL "CAUP" USING "0003135010" WH-CREG.
           IF ESC GO T.
           IF SEIN PERFORM KOREK GO B.
           IF NOT RET GO B.
       C.  IF MANUEL; IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY "Kunden-Nr. muss sein" with BEEP AT 2401
               PERFORM WEITER GO B.
           IF WH-NUM = 0; IF WH-MCODE = SPACE 
               DISPLAY " alles " AT 0270 GO D
           ELSE CALL "PANANZ" USING "10DEB-SUCH" WH-CREG
               IF DE-KTONR = 0 GO B ELSE GO C.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           ADD WH-NUM 0,2 GIVING WD-KTO.
           MOVE 1 TO DE-FNR.
           MOVE 0 TO WM-MERKER.
           IF DE-KTONR < 3 GO B.
           READ DEBITOR INVALID MOVE 1 TO WM-MERKER 
               CALL "PANDEBAN" USING "61DEB-ANL" WH-CREG.
           IF DE-KTONR = 0 GO B.
           IF ZUGRIF PERFORM BESETZT GO B.
           DISPLAY "EUR" with highlight foreground-color 6 AT 0565.
           MOVE DE-BEZ TO WT-BEZ WH-X.
           IF WH-X = "*" CALL "PANDEBAN" USING "61DEB-ANL" WH-CREG.
           IF ESC GO B.
           DISPLAY "Kunden-Nr.:" AT 0301.
           DISPLAY WD-KTO with highlight AT 0313.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with SIZE 78 AT 0401.
           DIVIDE 100 INTO WK-MON GIVING WH-VON.
           MOVE WH-VON TO WK-VON.
           IF DE-REKTO not = DE-KTONR PERFORM RE-ADR.
           IF MANUEL; IF DE-BEZ(1:1) = "*" GO B else GO N.
      *----------------------------------------> autom. Fakturierung <-
       D.  DIVIDE 100 INTO WK-MON GIVING WH-VON.
           MOVE WH-VON TO WK-VON.
      *    display wk-von at 1920.
      *    compute wk-von = wk-mon / 100.
      *    display wk-von at 1930.
      *    perform weiter.
           MOVE WH-MOD TO FA-ART DE-FAKART.
           MOVE 1 TO DE-FNR.
           MOVE DE-KTONR TO WV-KTONR.
           IF WV-KTONR not = 0 GO EB.
           MOVE 0 TO DE-TOUR DE-SUB.
           START DEBITOR KEY NOT < DE-RKEY INVALID PERFORM NO-REC GO P.
       E.  READ DEBITOR NEXT IGNORE LOCK AT END GO P.
           IF DE-FAKART not = WH-MOD GO P.
       EB. DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0313.
           MOVE 0 TO FA-SGRP.
           MOVE SPACE TO FA-ARBEZ.
           MOVE DE-KTONR TO FA-KTONR.
           MOVE DE-FAKART TO FA-ART.
           MOVE DE-GLN TO WE-GLN.
           MOVE WK-VON TO FA-MON.
           MOVE 0 TO FA-ARNUM.
           MOVE SPACE TO WM-BEZ.
           START FAKTDAT KEY NOT < FA-AKEY INVALID GO E.
       F.  READ FAKTDAT NEXT AT END GO E.
           IF ZUGRIF PERFORM BESETZT GO F.
           IF WV-KTONR NOT = 0; IF FA-KTONR NOT = WV-KTONR GO E.
           IF FA-MON > WK-VON OR FA-ART NOT = WH-MOD GO E.
           IF FA-KTONR not = DE-KTONR GO E.             *> keine Zeilen
           MOVE FA-AKEY TO WX-FIL.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0313.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with SIZE 78 AT 0401.
           MOVE WH-ABDAT TO WZ-DATUM.
      *    perform retsich.
           MOVE 1 TO WX.
           SET OX TO WZ-TAG.
           IF SOFORT MOVE WM-DATUM TO WH-ABDAT WH-BISDAT WZ-DATUM
               SET OX TO WZ-TAG
               IF WM-DATUM > WK-MON SET OX UP BY 31.
      *--------------------> WI: Tageanzahl, WV-ULT: Monatsende <-
       I.  IF OX > WV-ULT AND OX < 32 SET OX UP BY 1 GO I.
           IF FA-MENGE(OX) NOT = 0 OR FA-RETOUR(OX) NOT = 0 GO J.
           IF WX < WI ADD 1 TO WX
               SET OX UP BY 1 GO I.
           GO F.
       J.  IF WM-VER = 3; IF WM-BEZ = SPACE and DE-REKTO not = DE-KTONR
                PERFORM RE-ADR.
           IF WM-DRU = 1 and DE-RAST not = 0 and WM-RAST = 1
               PERFORM RASTER.
           PERFORM AUTOZEIL.
       K.  IF WV-KTONR > 2 GO P.
           GO E.
       N.  DISPLAY "<ret>= Rechnung, <esc>= Abbruch " AT 2301.
           CALL "CAUP" USING "0023330000" WH-CREG.
           IF ESC GO B.
           IF NOT RET GO N.
           PERFORM FAKTKOPF.
           OPEN EXTEND PROTOK.
           MOVE 0 TO PR-ARNUM.
           MOVE WM-DATUM TO PR-MENGE.
           IF WM-KO = 3 WRITE PR-SATZ.
           PERFORM FAKTZEIL.
           PERFORM FAKTENDE.
           CLOSE PROTOK.
           PERFORM INKASS.
           GO B.
       P.
021205*    CLOSE STATISTIK.
021205     GO B.
      *------------------------------------------> Ende Fakturierung <-
       T.  CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM SARE.
       W.  PERFORM DUPLO.
           CLOSE STATISTIK.
           OPEN I-O DAUER LFSCHEIN.
           PERFORM END-DRU.
           IF WM-REDAS not = 0 PERFORM EDIFUS.
       Z.  EXIT.
      ******************************************************************
       retsich section.
       a.  IF WM-DATUM = 20060823
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 31
                   MOVE FA-RMENGE(1, WZ) TO FA-MENGE(WZ)
                   MOVE FA-RRETOUR(1, WZ) TO FA-RETOUR(WZ)
               end-perform
               rewrite fa-satz.
       z.  exit.
      ******************************************************************
       fakorr section.
       a.  if wm-datum > 20081021 go z.
           move low-values to fa-satz.
           move 0 to de-ktonr.
           start faktdat key not < fa-key invalid go z.
       c.  read faktdat next at end go z.
           perform varying wz from 1 by 1 until wz > 31
               move 0 to fa-menge(wz) fa-retour(wz).
           if fa-rmon(1) = 0809 move 1 to wx go g.
           if fa-rmon(2) = 0809 move 2 to wx go g.
           if fa-rmon(3) = 0809 move 3 to wx go g.
           go c.
       g.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 31
               MOVE FA-RMENGE(WX, WZ) TO FA-MENGE(WZ)
               MOVE FA-RRETOUR(WX, WZ) TO FA-RETOUR(WZ).
           move 0809 to fa-mon.
           rewrite fa-satz.
           go c.
       z.  exit.
      ***************************************** Rechnungsadresse lesen *
       RE-ADR SECTION.
       A.  MOVE DE-RKEY TO WH-MCODE.
           MOVE DE-KTONR TO WT-KTONR.
           MOVE DE-REKTO TO DE-KTONR.
           MOVE SPACE TO WM-BEZ.
           READ DEBITOR IGNORE LOCK INVALID GO C.
           MOVE DE-BEZ TO WM-BEZ.
           MOVE DE-GLN TO WE-GLN.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO WE-EXT.
       C.  MOVE WH-MCODE TO DE-RKEY.
           START DEBITOR KEY NOT < DE-RKEY INVALID PERFORM NO-REC GO Z.
       E.  READ DEBITOR NEXT AT END STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF DE-KTONR not = WT-KTONR GO E.
           MOVE 0 TO WT-KTONR.
       Z.  EXIT.
      ****************************************************** Korrektur *
       korek section.
       a.  move low-value to fa-satz.
           START FAKTDAT KEY NOT < FA-AKEY INVALID GO x.
       F.  READ FAKTDAT NEXT AT END GO x.
           IF ZUGRIF PERFORM BESETZT GO F.
           if fa-ktonr = 0 delete faktdat
                           end-delete go f.
           if fa-ktonr = de-ktonr go k.
           MOVE FA-KTONR TO DE-KTONR.
           move 1 to de-fnr.
       G.  READ DEBITOR INVALID DISPLAY WD-KTO AT 2301
           IF ZUGRIF PERFORM BESETZT GO G.
       k.  if de-fakart not = fa-art move de-fakart to fa-art
               rewrite fa-satz.
           go f.
       x.  exit.
      ********************************************** Inkasso abstellen *
       INKASS SECTION.
       A.  IF WH-X = "*" MOVE "*" TO DE-BEZ.
           MOVE WM-DATUM TO DE-REDAT.
           MOVE WH-RENUM TO DE-RENR.
           MOVE WS-BET TO DE-REBET.
           REWRITE DE-SATZ.
       Z.  EXIT.
      ******************************************************************
       FAKTKOPF SECTION.
       A.  MOVE 1 TO WZ-SCHALT.
           MOVE 0 TO WT-BASIS(1) WT-BASIS(2) WT-BASIS(3) WT-BASIS(4)
                WT-BASIS(5) WT-BASIS(6) WH-NUM WH-MWST WS-BET WS-RET
                WS-SUM(1) WS-SUM(2) WS-SUM(3) WS-SUM(4) WS-SUM(8)
                WS-BONUS WH-STAT WH-UST WS-UST WE-NTG.
           MOVE 1 TO WH-KEY.
       C.  READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF DE-RAST < 2 ADD 1 TO KO-RENUM.
           MOVE KO-RENUM TO WH-RENUM.
           REWRITE KO-SATZ.
           MOVE 6 TO WH-KEY.
       D.  READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF KO-ERST = 0 MOVE WH-RENUM TO KO-ERST
               MOVE WM-DATUM TO KO-DATUM.
           IF DE-RAST < 2 REWRITE KO-SATZ.
           MOVE KO-RET TO WH-RET.
           MOVE KO-RETDAT TO WV-RETDAT.
           UNLOCK KONSTANT.
      *----------------------------------------> abstellen Archivkopf <-
           IF DE-RAST > 2 GO Q.
           MOVE WH-RENUM TO AC-RENUM WH-ARCNR.
           OPEN OUTPUT ARCHIV.
           MOVE 1 TO WM-ARC.
           MOVE DE-KTONR TO AC-KTONR.
           MOVE DE-MCODE TO AC-MCODE.                    *> alte Version
           MOVE WM-DATUM TO AC-REDAT.
           WRITE AC-ASATZ.
       Q.  MOVE SPACE TO AC-SATZ.
           IF WM-OPEN = 0 MOVE X"0100" TO WH-PX(1)
               MOVE X"000C" TO WH-PX(2)
               IF WM-DRU = 1 MOVE 5 TO WH-P
               end-if
               PERFORM BEG-DRU.
           PERFORM FAKADR.
           PERFORM BSKOPF.
           MOVE WH-RENUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT 0330.
       Z.  EXIT.
      *****************************************************************
       BSKOPF SECTION.
       A.  DISPLAY " Re-Nr.: " AT 0321.
           DISPLAY "   Re-Datum: " AT 0337.
           DISPLAY VD-DATUM with highlight AT 0300.
           DISPLAY "Art. B e z e i c h n u n g          Menge  Meh
      -        "Preis             Betrag  U" AT 0501.
           DISPLAY "EUR" with highlight foreground-color 6 AT 0565.
           MOVE 6 TO VDU-L.
           DISPLAY ALL "ƒ" with SIZE 80 AT 0601.
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
                    else MOVE KO-KOPF TO DRB-SATZ(4:)
                    IF WH-KEY = 2 PERFORM OB-TEILG.
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
      ******************************************************************
       FAKADR SECTION.
       A.  IF WM-DRU = 1 MOVE 0 TO WZ-SCHALT
               MOVE "&l26a4H" TO ABA-REST              *> Kopie Fach 2
               MOVE WZ-SCHALT TO ABA-SCHALT
               WRITE ABA-SATZ
               ADD 5 TO WM-DRU                       *> nich bei Kopie
               MOVE "&l26a1H" TO DRB-TX             *> Original Fach 1
               PERFORM DRUCK
               ADD -5 TO WM-DRU.
           IF WM-KO = 2 MOVE 6 TO WZ-SCHALT GO C.
           IF WE-KOPF(1) = 0 MOVE 7 TO WZ-SCHALT GO C.
           MOVE 2 TO WH-KEY.
       B.  READ KONSTANT IGNORE LOCK.
           EVALUATE WM-DRU
               WHEN 0 PERFORM NADEL-KOPF
               WHEN 1 PERFORM LASER-KOPF.
           PERFORM DRUCK.
           IF WH-KEY < 5 ADD 1 TO WH-KEY GO B.
           MOVE ALL "ƒ" TO DRB-TX(1:95).
       C.  PERFORM DRUCK.
           MOVE DE-BEZ TO WT-BEZ.
           IF WM-VER = 3 and WM-BEZ not = SPACE MOVE WM-BEZ TO WT-BEZ.
           SET FX TO 5.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(2) TO WX-FIL.
           IF WM-KO NOT = 3 MOVE 3 TO WZ-SCHALT.
           IF WE-KOPF(1) = 1 MOVE 6 TO WZ-SCHALT.
           PERFORM DRUCK.
           SET TX TO 1.
           MOVE WT-TXT(DE-ANR + 1) TO DRB-ADR.
           PERFORM DRUCK.
       E.  MOVE WR-ADR(TX) TO DRB-ADR.
           IF WM-VER not = 3 IF TX = 3 MOVE SPACE TO DRB-ADR.
           PERFORM DRUCK.
           IF TX < 5 SET TX UP BY 1 GO E.
           MOVE WZ-SCHALT TO ABA-SCHALT.
           IF WM-DRU = 1 MOVE "(s1p18v0s0b4101TK o p i e(s0p0s0b4101T"
                             TO ABA-REST(68:)
                         WRITE ABA-SATZ
                         IF DE-RAST NOT > 2
                            WRITE AC-SATZ FROM ABA-REST AFTER WZ-SCHALT
                         END-IF
                         MOVE SPACE TO ABA-SATZ DRA-SATZ
                         WRITE DRA-SATZ AFTER WZ-SCHALT
           else PERFORM DRUCK.
      *----------------------------------------------> Rechnungsdaten <-
           MOVE "Nr:" TO DRB-RTX.
           IF DE-RAST NOT = 2 MOVE WH-RENUM TO DRB-RENUM
               MOVE "/" TO DRB-STR.
           DIVIDE 10 INTO DE-KTONR GIVING DRB-KTONR.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VD-GZDAT TO DRB-ORT.
           MOVE DE-TOUR TO DRB-TOUR.
           MOVE DRB-SATZ(42:) TO WT-TX.
           IF WM-DRU = 0 MOVE X"1B213100" TO DRA-SATZ(1:4).
           MOVE 3 TO WZ-SCHALT.
           IF WM-KO = 2 MOVE 1 TO WZ-SCHALT
                        MOVE SPACE TO DRA-SATZ
                        MOVE X"1B213100" TO DRP-RETX(1:4)
                        MOVE "Rechnung" TO DRP-RETX(15:)
                        MOVE X"1B210100" TO DRP-RETX(24:4)
                        DIVIDE 10 INTO DE-KTONR GIVING WD-KTO
                        MOVE "Nr.:" TO DRP-RENR
                        MOVE WH-RENUM TO DRP-RENUM
                        MOVE "/" TO DRP-STR
                        MOVE WD-KTO TO DRP-KTO
                        PERFORM DRUCK
                        MOVE 2 TO WZ-SCHALT
                        MOVE WM-DATUM TO WC-DATUM
                        PERFORM DATDREH
                        MOVE VD-GZDAT TO DRB-ORT
                        MOVE DE-TOUR TO DRB-TOUR GO F.
           IF WM-DRU = 0 MOVE "Rechnung" TO DRB-SATZ(14:)
                        MOVE X"1B210100" TO DRB-SATZ(22:)
                        MOVE WT-TX TO DRB-SATZ(38:)
           else MOVE "(s1p18v0s2b4101TRechnung(s0p0s0b4101T"
                     TO DRB-SATZ(13:)
                MOVE WT-TX TO DRB-SATZ(53:).
       F.  PERFORM DRUCK.
           IF NOT MANUEL; IF WH-ABDAT NOT = 0
               MOVE "Lieferzeitraum vom:" TO DRU-TX
               MOVE WH-ABDAT TO WC-DATUM
               PERFORM DATDREH
               MOVE VD-DATUM TO DRU-AB
               MOVE "bis:" TO DRU-BX
               MOVE WH-BISDAT TO WC-DATUM
               PERFORM DATDREH
               MOVE VD-DATUM TO DRU-BIS
               PERFORM DRUCK.
           IF DE-EAN = 2 MOVE 0 TO DE-EAN.
           IF WM-DRU = 1 MOVE "&l6.857C" TO DRB-TX
                         MOVE 0 TO WZ-SCHALT
                         PERFORM DRUCK.
           MOVE "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒ¬ƒ
      -        "ƒƒ¬ƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒø" TO DRB-TX.
           PERFORM DRUCK.
           MOVE "≥Artikelnummer≥ B e z e i c h n u n g    ≥     Menge ≥M
      -        "eh≥    Preis ≥  Eur    Betrag ≥U%≥" TO DRB-TX.
           PERFORM DRUCK.
           MOVE "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒ¡ƒ
      -        "ƒƒ¡ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒŸ" TO DRB-TX.
           PERFORM DRUCK.
           IF WM-DRU = 1 MOVE "&l8C" TO DRB-TX.
           PERFORM DRUCK.
           IF WM-VER = 3 and DE-REKTO not = DE-KTONR
               MOVE "Lfg. an:" TO DRB-TX
               MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE WT-TX TO DRB-TX(10:)
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      *****************************************************************
       UEBERTRAG SECTION.
       A.  IF WZ-ZEILEN < 59 GO Z.
           MOVE ALL "ƒ" TO DRC-STR.
           PERFORM DRUCK.
           MOVE "ö b e r t r a g  " TO DRC-SATZ(32:).
           MOVE WS-BET TO DRC-BETRAG.
           PERFORM DRUCK.
           WRITE DRA-SATZ AFTER PAGE.
           IF WM-ARC = 1 WRITE AC-SATZ FROM DRA-SATZ AFTER PAGE.
           MOVE 0 TO WZ-ZEILEN.
           PERFORM FAKADR.
           MOVE "ö b e r t r a g " TO DRC-SATZ(32:).
           MOVE WS-BET TO DRC-BETRAG.
           PERFORM DRUCK.
       Z.  EXIT.
      **************************************************** EAN-Nummer *
       EAN SECTION.
       A.  IF DE-EAN = 0 MOVE AR-NUM TO DRC-ARNUM PR-ARNUM GO Z.
           IF DE-EAN = 2 OR DE-EAN = 3 GO D.
           COMPUTE WH-EAN = WK-EAN * 1000000 + (AR-NUM * 10).
       B.  ADD WR-EAN(2) WR-EAN(4) WR-EAN(6) WR-EAN(8) WR-EAN(10)
               WR-EAN(12) GIVING WK-ANZ.
           MULTIPLY 3 BY WK-ANZ.
           ADD WR-EAN(1) WR-EAN(3) WR-EAN(5) WR-EAN(7) WR-EAN(9)
               WR-EAN(11) TO WK-ANZ.
           DIVIDE 10 INTO WK-ANZ GIVING WK-ANZ REMAINDER WK-ANZ.
           SUBTRACT WK-ANZ FROM 10 GIVING WR-EAN(13).
           MOVE WH-EAN TO DRC-ARNUM PR-ARNUM.
           GO Z.
      *----------------------> lesen v. kundeneigenen Artikelnummern <-
       D.  IF AR-NUM = FA-ARNUM AND DE-KTONR = FA-KTONR GO F.
           MOVE AR-NUM TO FA-ARNUM.
           MOVE DE-KTONR TO FA-KTONR.
       E.  READ FAKTDAT INVALID KEY MOVE 0 TO FA-KUARNUM GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
       F.  IF DE-EAN = 3; IF FA-KUARNUM = 0
               MOVE AR-NUM TO DRC-ARNUM PR-ARNUM GO Z
           ELSE MOVE FA-KUARNUM TO DRC-ARNUM PR-ARNUM GO Z.
           MOVE FA-KUARNUM TO WH-EAN.
      *    IF WM-RE = 1 CALL "PANBAR.COB" USING "01BAR" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       AUTOZEIL SECTION.
       A.  MOVE 1 TO WM-RE.
           PERFORM BSKOPF.
           MOVE 0 TO WT-BASIS(1) WT-BASIS(2) WT-BASIS(3) WT-BASIS(4)
                WT-BASIS(5) WT-BASIS(6) WH-NUM WH-MWST WS-BET WS-RET
                WS-SUM(1) WS-SUM(2) WS-SUM(3) WS-SUM(4) WS-SUM(8)
                WS-BONUS WH-STAT.
           OPEN OUTPUT RECHNUNG.
       B.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE 0 TO WT-MENGE(WX) WT-RETM(WX) WT-PREIS(WX)
                   WT-RETP(WX) WT-RAB(WX).
           INITIALIZE RE-SATZ.
           MOVE FA-ARNUM TO AR-NUM RE-ARNUM.
       C.  READ ARTIKEL IGNORE LOCK INVALID
               MOVE "Artikel gelîscht" TO AR-BEZ GO Q.
           MOVE AR-GRM TO WF-GRM RE-GRM.
           MOVE AR-PL TO RE-ARPL.
           MOVE WH-ABDAT TO WZ-DATUM WC-DATUM.
           MOVE AR-UST TO RE-UST.
           MOVE AR-EKP TO RE-EKP.
           SET OX TO WZ-TAG.
           IF SOFORT AND WM-DATUM > WK-MON SET OX UP BY 31.
      *--------------------------> WI: Tageanzahl, WV-ULT: Monatsende <-
           MOVE WI TO WX.
       D.  IF OX > WV-ULT AND OX < 32 SET OX UP BY 1 GO D.
           IF WC-JAHR > WV-ULT MOVE 1 TO WC-JAHR
               ADD 1 TO WC-MONAT
               IF WC-MONAT > 12 MOVE 1 TO WC-MONAT
                   ADD 1 TO WC-TAG.
           MOVE WC-DATUM TO WS-DATUM.
           IF Ws-datum < 500000 add 20000000 to ws-datum
           else add 19000000 to ws-datum.
           SET PX TO 4.
      *----------------> keine Sonderkondition an Sonn- u. Feiertagen <-
           SET IY TO OX.
       E.  IF FA-AB(PX) = 0 OR FA-AB(PX) > WS-DATUM GO F.
           IF FA-BIS(PX) = 0 OR FA-BIS(PX) NOT < WS-DATUM
               MOVE FA-PREIS(PX) TO WH-PREIS
               MOVE FA-RAB(PX) TO WH-RAB
               MOVE FA-RET(PX) TO WH-WERT GO G.
       F.  IF PX > 1 SET PX DOWN BY 1 GO E.
      *-------------------------------------------> ohne Preistabelle <-
           SET PX TO 5.
           MOVE 0 TO WH-PREIS WH-RAB.
      *--------------------------------------------> mit Preistabelle <-
       G.  IF DE-PRDAT > AR-ADAT MOVE DE-PRDAT TO AR-ADAT.
       H.  IF WH-PREIS = 0;
               IF (AR-ADAT = 0 OR AR-ADAT > WS-DATUM)
                   MOVE AR-PREIS TO WH-PREIS WH-WERT
                       IF DE-RAST = 9 MOVE AR-VKP TO WH-PREIS WH-WERT
                       end-if
                   else SET PX UP BY 5
                       MOVE AR-APREIS TO WH-PREIS WH-WERT
                       IF DE-RAST = 9 MOVE AR-AVKP TO WH-PREIS WH-WERT.
           SET IX TO PX.
           ADD FA-MENGE(OX) TO WT-MENGE(IX).
           ADD FA-RETOUR(OX) TO WT-RETM(IX).
           MOVE WH-WERT TO WT-RETP(IX).
           MOVE WH-PREIS TO WT-PREIS(IX).
           MOVE WH-RAB TO WT-RAB(IX).
      *---------------------------------> öbernahme in Periodenraster <-
           IF FA-MON > FA-RMON(1) MOVE FA-RAST(2) TO FA-RAST(3)
                                  MOVE FA-Rast(1) TO FA-RAST(2)
                                  INITIALIZE FA-RAST(1)
                                  MOVE FA-MON TO FA-RMON(1).
           ADD FA-MENGE(OX) TO FA-RMENGE(1, OX).
           ADD FA-RETOUR(OX) TO FA-RRETOUR(1, OX).
      *--------------------------------> Lîschung fakturierter Mengen <-
           MOVE 0 TO FA-MENGE(OX) FA-RETOUR(OX).
           IF WX > 1 SUBTRACT 1 FROM WX SET OX UP BY 1
               ADD 1 TO WC-JAHR GO D.
      *------------------------------------> Druck der Artikelzeilen <-
           MOVE 7 TO VDU-L.
           CALL "CAUP" USING "1307010780000" WH-CREG.
           MOVE 1 TO IX.
           MOVE 0 TO WH-ANZ WH-BET.
           MOVE AR-BEZ TO RE-BEZ.
       K.  MOVE DE-RAB TO WH-RAB.
           IF AR-PL = 2 MOVE WT-MENGE(IX) TO WH-BET
               SUBTRACT WT-RETM(IX) FROM WH-BET
               IF WF-NK > 0 DIVIDE 10 INTO WH-BET
                   IF WF-NK > 1 DIVIDE 10 INTO WH-BET
                   END-IF
               END-IF MOVE WH-BET TO WH-STAT
                   PERFORM ERLOEKTO
                   MOVE WH-BET TO RE-BET GO P.
           MOVE 0 TO WH-PREIS.
           MOVE WT-MENGE(IX) TO WH-ANZ.
           MOVE WT-PREIS(IX) TO WH-PREIS RE-PREIS.
           IF IX NOT = 5 AND IX NOT = 10;
               IF WM-RB = 1 MOVE WT-RAB(IX) TO WH-RAB
                  ELSE IF WT-RAB(IX) NOT = 0 MOVE WT-RAB(IX) TO WH-RAB.
           MOVE 0 TO WK-RM.
           MOVE WH-RAB TO RE-RAB.
           IF WH-ANZ NOT = 0 GO M.
      *---------------------------------------> Retourwarenbehandlung <-
       L.  MOVE WT-RETM(IX) TO WH-ANZ.
           MOVE WT-RETP(IX) TO WH-PREIS RE-RETPR.
           IF IX NOT = 5 AND IX NOT = 10;
               IF WM-RB = 1 MOVE WT-RAB(IX) TO WH-RAB
                  ELSE IF WT-RAB(IX) NOT = 0 MOVE WT-RAB(IX) TO WH-RAB.
           IF WH-ANZ = 0 GO P.
           MOVE 1 TO WK-RM.
       M.  MOVE AR-NUM TO WD-ART.
           DISPLAY WD-ART AT 0701.
           MOVE AR-BEZ TO DRC-BEZ RE-RETBEZ.
           IF WT-MENGE(IX) NOT = 0 AND WK-RM = 1
               MOVE "      Retourmenge" TO DRC-BEZ RE-RETBEZ.
           DISPLAY DRC-BEZ with highlight AT 0706.
           PERFORM MG.
           IF WK-RM = 0 MOVE WH-ANZ TO RE-MENGE
                        MOVE WD-MENGE TO RE-WDMG
                   ELSE MOVE WH-ANZ TO RE-RETMG
                        MOVE WD-MENGE TO RE-RWDMG.
           ADD 1 WF-MEH GIVING RE-MEH WH-MEH.
           DISPLAY WD-MENGE with highlight AT 0732.
           DISPLAY WT-MEH(RE-MEH) AT 0744.
           IF WR-GR = 0 MOVE WH-PREIS TO WD-PREIS
               DISPLAY WD-PREIS with highlight AT 0748
           ELSE MOVE WH-PREIS TO WD-APREIS
               DISPLAY WD-APREIS with highlight AT 0748.
           IF WH-RAB NOT = 0 and AR-RAB = 0
               MOVE WH-RAB TO WD-PROZ
               DISPLAY WD-PROZ with highlight AT 0760
           else MOVE 0 TO RE-RAB.
           IF WH-PREIS = 0 and WH-ANZ not = 0 PERFORM HINWEIS.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * WH-RAB / 100).
           PERFORM ERLOEKTO.
           IF WK-RM = 0 GO L.
       P.  IF RE-MENGE NOT = 0 OR RE-RETMG NOT = 0 OR RE-BET NOT = 0
               WRITE RE-SATZ
               END-WRITE MOVE 0 TO RE-MENGE RE-RETMG RE-BET.
           IF IX < 10 ADD 1 TO IX GO K.
           REWRITE FA-SATZ.
990820*Q.  READ FAKTDAT NEXT AT END MOVE 9 TO WH-MOD GO R.
       Q.  READ FAKTDAT NEXT AT END GO R.
           IF FA-KTONR NOT = DE-KTONR GO R.
           IF ZUGRIF PERFORM BESETZT GO Q.
           SET PX TO 1.
           GO B.
       R.  PERFORM REDRUCK.
           CLOSE RECHNUNG.
       Z.  EXIT.
      ******************************************************************
       HINWEIS SECTION.
       A.  DISPLAY "Preis fehlt!" with highlight foreground-color 4
               AT 2401.
           DISPLAY "Preis: " AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "0023085206" WH-CREG.
           MOVE WH-WERT TO WH-PREIS RE-PREIS.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * WH-RAB / 100).
           IF WR-GR = 0 MOVE WH-PREIS TO WD-PREIS
               DISPLAY WD-PREIS with highlight AT 0748
           ELSE MOVE WH-PREIS TO WD-APREIS
               DISPLAY WD-APREIS with highlight AT 0748.
           PERFORM ERLOEKTO.
       Z.  EXIT.
      ******************************************************************
       REDRUCK SECTION.
       A.  UNLOCK FAKTDAT.
           MOVE 2 TO WM-RE.
311205     IF DE-RAST = 5 GO Z.
           CLOSE RECHNUNG.
           OPEN INPUT RECHNUNG.
           MOVE 100 TO WP-RET.
           MOVE WS-RET TO WK-RET.
           IF WS-RET < 0 MOVE 0 TO WS-RET.
           IF WM-KO = 3 ADD WS-SUM(3) WS-RET GIVING WS-BET
                   else ADD WS-SUM(3) WS-SUM(4) WS-RET GIVING WS-BET.
           COMPUTE WV-BON = WS-RET / WS-BET * 100.
      *-----------------------------------------------> ob Returbonus <-
           IF DE-RBASIS not = 0 and WV-BON < DE-RBASIS
               MOVE DE-BONUS TO WV-BON
           else MOVE 0 TO WV-BON.
           IF (DE-RET >= 0 AND < 100) AND WS-RET NOT = 0
               COMPUTE WV-RET = WS-RET / WS-BET * 100
               COMPUTE WP-RET ROUNDED = WS-BET * DE-RET / WS-RET.
           IF WK-EAN = 09005793 and DE-RET not = 0
               MOVE DE-RET TO WP-RET.
           PERFORM FAKTKOPF.
           IF WP-RET = 99,9 MOVE 1 TO WM-RETVER
           else MOVE 0 TO WM-RETVER.
           IF WK-EAN = 09005793 and DE-RET not = 0          *> nur Kern
               MOVE "maximale Retourware 10% lt. Vereinbarung"
                   TO DRC-SATZ(18:)
               MOVE DE-RET TO WD-PROZ
               MOVE WD-PROZ(1:2) TO DRC-SATZ(38:2)
               IF DE-RET not = 100 PERFORM DRUCK
                   PERFORM DRUCK
               end-if
               GO C.
           IF WP-RET < 99,9   MOVE "Retourware:     % vereinbart:     %
      -        "daher Preisreduktion auf     %" TO DRC-SATZ(18:)
               MOVE WV-RET TO WD-PROZ
               MOVE WD-PROZ TO DRC-SATZ(30:4)
               MOVE DE-RET TO WD-PROZ
               MOVE WD-PROZ TO DRC-SATZ(48:4)
               MOVE WP-RET TO WD-PROZ
               MOVE WD-PROZ TO DRC-SATZ(79:4)
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
           else MOVE 100,0 TO WP-RET.
       C.  READ RECHNUNG AT END GO X.
           PERFORM UEBERTRAG.
           IF RE-ARPL = 2 ADD RE-BET TO WS-SUM(8) GO C.
           MOVE RE-ARNUM TO DRC-ARNUM WH-EAN AR-NUM.
           MOVE RE-BEZ TO DRC-BEZ.
           MOVE RE-UST TO AR-UST.
           MOVE RE-EKP TO AR-EKP.
           MOVE RE-GRM TO WF-GRM AR-GRM.
           MOVE 0 TO WK-RM.
       G.  IF RE-MENGE = 0 GO K.
           MOVE RE-PREIS TO WH-PREIS.
           MOVE RE-MENGE TO WH-ANZ.
           MOVE RE-WDMG TO WD-MENGE.
           MOVE WD-MENGE TO DRC-MENGE.
           MOVE WT-MEH(RE-MEH) TO DRC-MEH.
           IF WR-GR = 0 MOVE WH-PREIS TO DRC-PREIS
                   ELSE MOVE WH-PREIS TO DRC-APREIS.
           IF RE-RAB NOT = 0 MOVE RE-RAB TO DRC-PROZ.
           MOVE WT-UST(RE-UST) TO DRC-UST WX WH-UST.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * RE-RAB / 100).
           IF WK-EAN = 09005793 and DE-RET not = 0          *> nur Kern
               COMPUTE WH-GRENZ = WH-BET * DE-RET / 100 * -1.
           COMPUTE WH-UST ROUNDED = WH-BET * WH-UST / 100.
           ADD WH-UST TO WS-UST.                      *> EDIFACT-Kunden
           IF WV-BON = 0 MOVE WH-BET TO WH-STAT
           else COMPUTE WH-STAT = WH-BET - (WH-BET * WV-BON / 100).
           COMPUTE WS-BONUS = WS-BONUS - WH-BET + WH-STAT.
           MOVE 7 TO VDU-L.
           PERFORM ERLOEKTO.
           MOVE WS-SUM(9) TO DRC-BETRAG.
           PERFORM DRUCK.
           PERFORM DESADATEN.
      *    PERFORM EDIZEIL.
       K.  IF RE-RETMG = 0 GO C.
           MOVE 1 TO WK-RM.
           IF RE-MENGE NOT = 0 MOVE RE-RETBEZ TO DRC-BEZ.
           COMPUTE WH-PREIS ROUNDED = RE-RETPR * WP-RET / 100.
           IF WK-EAN = 09005793 and DE-RET not = 0          *> nur Kern
               MOVE RE-RETPR TO WH-PREIS.
           MOVE RE-RETMG TO WH-ANZ.
           MOVE RE-RWDMG TO WD-MENGE.
           MOVE WD-MENGE TO DRC-MENGE.
           MOVE WT-MEH(RE-MEH) TO DRC-MEH.
           IF WR-GR = 0 MOVE WH-PREIS TO DRC-PREIS
                   ELSE MOVE WH-PREIS TO DRC-APREIS.
           IF WM-RETVER = 2 MOVE SPACE TO DRC-APREIS(1:).
           IF RE-RAB NOT = 0 MOVE RE-RAB TO DRC-PROZ.
           MOVE WT-UST(RE-UST) TO DRC-UST WX WH-UST.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * RE-RAB / 100).
      *----------------------------------------> Begrenzung je Position
           IF WK-EAN = 09005793 and DE-RET not = 0;         *> nur Kern
250512         IF WH-BET < WH-GRENZ MOVE WH-GRENZ TO WH-BET
                   COMPUTE WH-PREIS = WH-BET / WH-ANZ
                   IF WR-GR = 0 MOVE WH-PREIS TO DRC-PREIS
                           else MOVE WH-PREIS TO DRC-APREIS.
      *-----------------------------------------------------------------
           COMPUTE WH-UST ROUNDED = WH-BET * WH-UST / 100.
           ADD WH-UST TO WS-UST.                      *> EDIFACT-Kunden
           IF WV-BON = 0 MOVE WH-BET TO WH-STAT
           else COMPUTE WH-STAT = WH-BET - (WH-BET * WV-BON / 100).
           COMPUTE WS-BONUS = WS-BONUS - WH-BET + WH-STAT.
           MOVE 7 TO VDU-L.
           PERFORM ERLOEKTO.
           MOVE WS-SUM(9) TO DRC-BETRAG.
           PERFORM DRUCK.
           PERFORM DESADATEN.
      *    PERFORM EDIZEIL.
           GO C.
       X.  PERFORM FAKTENDE.
           PERFORM INKASS.
       Z.  EXIT.
      ************************************* Daten fÅr DESADV erstellen *
       DESADATEN SECTION.
       A.  IF DE-GLN = 0 GO Z.
           OPEN I-O HILFDESA.
           INITIALIZE HD-SATZ.
           MOVE WE-EXT TO HD-EXT.
           MOVE WE-GLN TO HD-GLN.
           MOVE DE-GLN TO HD-ILN.
           MOVE RE-ARNUM TO HD-ARNUM.
           MOVE 0 TO HD-LFNUM.
           MOVE RE-BEZ TO HD-BEZ.
           MOVE DE-REKTO TO HD-KTONR.
           MOVE WM-DATUM TO HD-DATUM.
           MOVE WH-RENUM TO HD-NUM.
           MOVE WH-ANZ TO HD-ANZ.
           EVALUATE RE-MEH
               WHEN 1 MOVE "STK" TO HD-MEH
               WHEN 2 MOVE "KGM" TO HD-MEH
               WHEN OTHER MOVE "STK" TO HD-MEH.
           MOVE WH-BISDAT TO HD-BISDAT.
           MOVE WH-PREIS TO HD-PREIS.
           MOVE WH-BET TO HD-BET.
           MOVE WT-UST(RE-UST) TO HD-USTPZ.
           MOVE WH-UST TO HD-UST.
           MOVE WE-NTG TO HD-NTG.
           WRITE HD-SATZ INVALID REWRITE HD-SATZ.
           CLOSE HILFDESA.
       Z.  EXIT.
      ******************************************************************
       PRFZ SECTION.
       B.  COMPUTE WH-EAN = WK-EAN * 1000000.
           ADD WR-EAN(2) WR-EAN(4) WR-EAN(6) WR-EAN(8) WR-EAN(10)
               WR-EAN(12) GIVING WK-ANZ.
           MULTIPLY 3 BY WK-ANZ.
           ADD WR-EAN(1) WR-EAN(3) WR-EAN(5) WR-EAN(7) WR-EAN(9)
               WR-EAN(11) TO WK-ANZ.
           DIVIDE 10 INTO WK-ANZ GIVING WK-ANZ REMAINDER WK-ANZ.
           SUBTRACT WK-ANZ FROM 10 GIVING WR-EAN(13).
           COMPUTE WS-DATUM = FUNCTION INTEGER-OF-DATE(WM-DATUM).
       Z.  EXIT.
      ******************************************* Liefermonat ausgeben *
       LIEFMON SECTION.
       A.  MOVE WH-BISDAT TO WC-DATUM.
           EVALUATE WC-MONAT
               WHEN 1 MOVE " JÑnner" TO WM-CD
               WHEN 2 MOVE " Feburar" TO WM-CD
               WHEN 3 MOVE " MÑrz" TO WM-CD
               WHEN 4 MOVE " April" TO WM-CD
               WHEN 5 MOVE " Mai" TO WM-CD
               WHEN 6 MOVE " Juni" TO WM-CD
               WHEN 7 MOVE " Juli" TO WM-CD
               WHEN 8 MOVE " August" TO WM-CD
               WHEN 9 MOVE " September" TO WM-CD
               WHEN 10 MOVE " Oktober" TO WM-CD
               WHEN 11 MOVE " November" TO WM-CD
               WHEN 12 MOVE " Dezember" TO WM-CD.
           PERFORM ANHANG.
           MOVE HD-BISDAT TO WM-CD(2:).
           MOVE SPACE TO WM-CD(6:).
           PERFORM ANHANG.
       Z.  EXIT.
      ******************************************* DESADV-Lieferzeilen *
       DESAZEIL SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           OPEN INPUT HILFDESA.
           IF WF-STATUS not = "00" GO W.
           MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           COMPUTE WH-EAN = KO-EANNR * 1000000.
           PERFORM PRFZ.
           MOVE WH-EAN TO WK-EANNR.
           MOVE 0 TO WM-REDAS WV-ILN WV-GLN.
           CALL "CAUP" USING "0705150750000" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Es werden alle EDIFACT-Dateien erstellt!"
                with highlight foreground-color 6 AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Vorgang kann nicht wiederholt werden!"
                 with highlight foreground-color 6 AT VDU-LP.
           ADD 603 VDU-ECK GIVING VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <Einfg>= Start < >" AT VDU-LP.
           CALL "CAUP" USING "1006350000" WH-CREG.
           IF ESC GO W.
           IF not EINF GO C.
           INITIALIZE HD-SATZ.
           MOVE WM-DATUM TO HD-DATUM.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           START HILFDESA KEY > HD-KEY INVALID GO W.
       E.  READ HILFDESA NEXT AT END GO Q.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY HD-EXT AT VDU-LP "  " HD-GLN "  " HD-ILN.
           IF WM-DATUM not = HD-DATUM GO E.
           IF HD-GLN not = WV-GLN and WM-REDAS > 1 PERFORM DESA-UNT
                                                   PERFORM EDIFUS.
           IF HD-ILN not = WV-ILN and WM-REDAS > 1 PERFORM DESA-UNT.
           IF WM-REDAS = 0 PERFORM OPEN-DESADV
               MOVE HD-GLN TO WV-GLN
               MOVE 1 TO WM-REDAS.
           IF WM-REDAS not = 2 PERFORM DESAKOPF
               MOVE HD-ILN TO WV-ILN
               MOVE 2 TO WM-REDAS.
      *    ADD 1 TO WC-POS.
      *----------------------------------> Beginn der Rechnungszeilen <-
       K.  MOVE "LIN+" TO WB-SATZ.
           ADD 1 TO WH-LIN.                      *> Anz. LIN+ Segemente
           MOVE WH-LIN TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "PIA+1+" TO WB-SATZ.
           MOVE HD-ARNUM TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE ":SA" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "IMD+F++:::" TO WB-SATZ.
           INSPECT HD-BEZ REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹".
           MOVE HD-BEZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "QTY+47:" TO WB-SATZ.
           MOVE HD-ANZ TO WE-PREIS.
           IF HD-ANZ < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-PREIS(1:1) = " " MOVE WE-PREIS(2:) TO WE-PREIS(1:).
           MOVE WE-PREIS TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           IF HD-MEH = 1 MOVE "STK" TO WM-CD
                    else MOVE "KGM" TO WM-CD.
           MOVE HD-MEH TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "MOA+66:" TO WB-SATZ.
           MOVE HD-BET TO WE-BET.
           IF HD-BET < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
               IF WE-BET(1:1) = " " MOVE WE-BET(2:) TO WE-BET(1:).
           MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "PRI+AAA:" TO WB-SATZ.
           MOVE HD-PREIS TO WE-PREIS.
           IF HD-PREIS < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-PREIS(1:1) = " " MOVE WE-PREIS(2:) TO WE-PREIS(1:).
           MOVE WE-PREIS TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "TAX+7+VAT+++:::" TO WB-SATZ.
           MOVE HD-USTPZ TO WE-BET WM-UST.              *> fÅr UNT-Satz
           MOVE WE-BET TO WM-CD.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
               IF WE-BET(1:1) = SPACE MOVE WE-BET(2:) TO WE-BET(1:).
           MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE "+S" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "MOA+154:" TO WB-SATZ.
           MOVE HD-UST TO WE-PREIS.
           IF HD-UST < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-PREIS(1:1) = " " MOVE WE-PREIS(2:) TO WE-PREIS(1:).
           MOVE WE-PREIS TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           ADD HD-BET TO WW-NTTO.
           ADD HD-UST TO WS-UST.
           GO E.
       Q.  IF WM-REDAS > 1 PERFORM DESA-UNT
                           PERFORM EDIFUS.
           CLOSE HILFDESA.
           DELETE FILE HILFDESA.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           DISPLAY "EDIFACT-Erstellung beendet!" AT 2401.
           PERFORM WEITER.
           GO Z.
       W.  DISPLAY "Keine Edifact-Daten vorhanden!" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************* DESADV-Rechnungskopf *
       DESAKOPF SECTION.
       A.  MOVE 0 TO WE-NTG WW-NTTO WS-UST.
           ADD 1 TO WH-UNH.
           MOVE "UNH+" TO WB-SATZ.
           MOVE HD-NUM TO WH-KONTROL(5:) WH-RENUM.
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE "INVOIC" TO WM-CD.
           PERFORM ANHANG.
           MOVE ":D:" TO WM-CD.
           PERFORM ANHANG.
           MOVE "96A:" TO WM-CD.
           PERFORM ANHANG.
           MOVE "UN" TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EAN008" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "BGM+385+" TO WB-SATZ.
           MOVE HD-NUM TO WE-NUM.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
               IF WE-NUM(1:1) = " " MOVE WE-NUM(2:) TO WE-NUM(1:).
           MOVE WE-NUM TO WM-CD.
           PERFORM ANHANG.
           MOVE "+9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "DTM+137:" TO WB-SATZ.
           MOVE HD-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":102" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "RFF+DQ:Lieferung" TO WB-SATZ.
           PERFORM LIEFMON.
           PERFORM ZEILEND.
      *--------------------------------------> Beginn des B-Segmentes <-
           MOVE "NAD+BY+" TO WB-SATZ.
           MOVE HD-GLN TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+SU+" TO WB-SATZ.
           MOVE WK-EANNR TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+DP+" TO WB-SATZ.
      *--------------------------------> GLN des Waren-/Rechnugsempf. <-
           MOVE HD-ILN TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "CUX+2:EUR:4" TO WB-SATZ.
           PERFORM ZEILEND.
      *------------------------------------------------> Zahlungsziel <-
           MOVE "PAT+1++5:3:D:" TO WB-SATZ.
           MOVE HD-NTG TO WE-NTG.
           IF WE-NTG = 0 MOVE 7 TO WE-NTG.
           MOVE WE-NTG TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE 0 TO WH-SEG.
           MOVE 0 TO WH-LIN.
       Z.  EXIT.
      ******************************************************************
       OPEN-DESADV SECTION.
       A.  MOVE "INVOICE.EDF" TO WN-EDICUS WN-SICHB.
           MOVE 0 TO WH-UNH.
           MOVE WN-EDICUS TO WN-EDIFACT.
           MOVE HD-EXT TO WE-EXT.
      *-------------------------------------> lîschen und create file <-
           CALL "CBL_DELETE_FILE" USING WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS WH-SEG.
           MOVE "UNA:+,? '" TO WB-SATZ.
           MOVE 9 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE "UNB+UNOA:3+" TO WB-SATZ.
           MOVE WK-EANNR TO WM-CD.                     *> GLN-Versender
           PERFORM ANHANG.
           MOVE ":14+" TO WM-CD.
           PERFORM ANHANG.
           MOVE HD-GLN TO WM-CD.                       *> GLN-EmpfÑnger
           PERFORM ANHANG.
           MOVE ":14+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-DATUM TO WC-DATUM.
           MOVE WC-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           ACCEPT WH-TIME FROM TIME.
           MOVE WH-HHMM TO WM-CD(1:4).
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE "KERN" TO WH-KONTROL.
           MOVE HD-NUM TO WH-KONTROL(5:).
           MOVE WH-KONTROL TO WM-CD WK-UNZ.         *> Kennung wie UNZ+
           PERFORM ANHANG.
           MOVE "+++++EANCOM" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
       Z.  EXIT.
      **************************************** Ust-Aufstellung EDIFACT *
       FAKT-AUF SECTION.
       A.  IF WK-EAN = 0 GO Z.
           IF DE-GLN = 0 GO Z.
           OPEN I-O FAKTEDI.
           MOVE WH-RENUM TO EF-RENUM.
           MOVE FA-KTONR TO EF-KTONR.
           MOVE WM-DATUM TO EF-REDAT.
           MOVE WT-BASIS(1) TO EF-NETTO.
           MOVE WH-MWST TO EF-UST.
           ADD EF-NETTO EF-UST GIVING EF-GESAMT.
           MOVE DE-REKTO TO EF-KUNDE.
           MOVE DE-GLN TO EF-GLN.
           MOVE DE-BEZ TO EF-TEXT.
           INSPECT EF-TEXT REPLACING ALL "##" BY ", " "#" BY ",".
           WRITE EF-SATZ.
           CLOSE FAKTEDI.
       Z.  EXIT.
      ******************************************************************
       DESA-UNT SECTION.
      *-------------------------------------------> Schlu· je Rechung <-
       A.  MOVE "UNS+S" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "MOA+79:" TO WB-SATZ.
           MOVE WW-NTTO TO WE-BET.
           IF WW-NTTO < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
               IF WE-BET(1:1) = SPACE MOVE WE-BET(2:) TO WE-BET(1:).
           MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "MOA+176:" TO WB-SATZ.
           MOVE WS-UST TO WE-BET.
           IF WS-UST < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           MOVE WE-BET TO WM-CD.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
               IF WE-BET(1:1) = SPACE MOVE WE-BET(2:) TO WE-BET(1:).
           MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "MOA+39:" TO WB-SATZ.
           ADD WW-NTTO WS-UST GIVING WS-BET.
           MOVE WS-BET TO WE-BET.
           IF WS-BET < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           MOVE WE-BET TO WM-CD.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
               IF WE-BET(1:1) = SPACE MOVE WE-BET(2:) TO WE-BET(1:).
           MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "TAX+7+VAT+++:::" TO WB-SATZ.
           MOVE WM-UST TO WM-CD.
      *    PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
      *        IF WE-BET(1:1) = SPACE MOVE WE-BET(2:) TO WE-BET(1:).
      *    MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "MOA+125:" TO WB-SATZ.
           MOVE WW-NTTO TO WE-BET.
           IF WW-NTTO < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
               IF WE-BET(1:1) = SPACE MOVE WE-BET(2:) TO WE-BET(1:).
           MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "MOA+124:" TO WB-SATZ.
           MOVE WS-UST TO WE-BET.
           IF WS-UST < 0 MOVE "-" TO WM-CD
               PERFORM ANHANG.
           MOVE WE-BET TO WM-CD.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
               IF WE-BET(1:1) = SPACE MOVE WE-BET(2:) TO WE-BET(1:).
           MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "UNT+" TO WB-SATZ.
           MOVE WH-SEG TO WE-ANZ.             *> Anz. Pos. im LIN+ Segm.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE 1 TO WM-REDAS.
           MOVE 0 TO WV-ILN.
       Z.  EXIT.
      ******************************************************************
       EDIFUS SECTION.
           MOVE "UNZ+" TO WB-SATZ.
           MOVE WH-UNH TO WE-ANZ.                 *> Anz. UNH+ Segmente
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 4
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WK-UNZ TO WM-CD.                  *> Kennung aus UNB+
           PERFORM ANHANG.
           PERFORM ZEILEND.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
           MOVE "EDIDAT\INVOICEX.EDF " TO WN-EDIFACT.
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE WZ-DATUM(1:4) TO WN-EDIFACT(12:4).
           INSPECT WN-EDIFACT REPLACING ALL "EDF" BY WE-EXT.
           INSPECT WN-SICHB REPLACING ALL "EDF" BY WE-EXT.
           CALL "CBL_COPY_FILE" USING WN-EDICUS WN-EDIFACT.
           CALL "CBL_COPY_FILE" USING WN-EDICUS WN-SICHB.
           CALL "CBL_DELETE_FILE" USING WN-EDICUS.
           EVALUATE WE-EXT
               WHEN "HOG" PERFORM INV-BLAT
               WHEN "DUS" PERFORM INV-FTP.
           MOVE 0 TO WV-GLN WV-ILN WM-REDAS.
       Z.  EXIT.
      ************************************************ FTP-Versand DUS *
       INV-FTP SECTION.
       A.  GO Z.
           MOVE "DUSI.BAT" TO WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
      *-------------------------------------> PSFtp-Dateien erstellen <-
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           Move "ftp -s:DUSI.FTP" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
      *---------------------------------> erst BAT-File dann SCR-File <-
           MOVE "DUSI.FTP" TO WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           MOVE "open ftp.necta.at" TO WB-SATZ.
           MOVE "open 93.83.12.171" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "1050_15" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "s4PpLEDA" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "asc" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "cd /invoice" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "put INVOICE.DUS." TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "lcd \FTP" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "get INVOICE.DUS" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "lcd \BUG" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "quit" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
      *-------------------------------------------> BAT-File erledigt <-
           CALL "CAUP" USING "0703011980000" WH-CREG.
           CALL "CAUP" USING "1301012580" WH-CREG.
      *-----------------------------------------------> FTP-ausfÅhren <-
           MOVE "DUSR.BAT" TO WD-UPON.
           DISPLAY LOW-VALUE AT 1801.
           DISPLAY WD-UPON UPON COMMAND-LINE.
      *------------------------> öbertragung kontrollieren u. lîschen <-
           MOVE "INVOICE.DUS" TO WH-FNAME WN-EDICUS.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           MOVE WF-SIZE TO WV-SIZE.
           MOVE "FTP\INVOICE.DUS" TO WH-FNAME.
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WH-FNAME WH-FDET.
           IF WF-SIZE = WV-SIZE
      *-----------> WH-FNAME = Retourdatei WN-EDICAUS = *> Org.-Datei <-
               CALL "CBL_DELETE_FILE" USING WH-FNAME
               CALL "CBL_DELETE_FILE" USING WN-EDICUS
               GO X.
           DISPLAY "öbertragung an DUSSMANN fehlgeschlagen!" AT 2003
           DISPLAY "Bitte Datei 'DUSS.BAT' in F:\BUG durchfÅhren!"
                       AT 2103.
           PERFORM WEITER.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************* BAT-Datei fÅr mailsend.bat *
       INV-BLAT SECTION.
       A.  MOVE "BLAT\MAILSEND.BAT" TO WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           MOVE "blat -t edifact@hogast.at -c office@baeckerei-kern.at -
      -       "s INVOICE" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE "-body ""INVOICE zu Rechnungsdatum: 12.12.06"""
                TO WB-SATZ.
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VD-DATUM TO WB-SATZ(35:8).
           PERFORM COUNT-WRITE.
           MOVE "-attach f:\bug\INVOICE.HOG -server bsmtp.telekom.at -f
      -        "office@baeckerei-kern.at -log mailog.dat" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
       Z.  EXIT.
      ******************************************************************
       COUNT-WRITE SECTION.
       A.  PERFORM VARYING WB-CNT FROM 99 BY -1 UNTIL
               WB-SATZ(WB-CNT:1) not = SPACE CONTINUE.
           ADD 1 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
       Z.  EXIT.
      ***************************************************** Zeilenende *
       ZEILEND SECTION.
       A.  MOVE "'" TO WM-CD(1:).
      *    move x"0d0a" TO WM-CD(2:).              *> raus
           PERFORM ANHANG.
      *    add 2 to wx.                            *> raus
           MOVE WX TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE SPACE TO WB-SATZ.
           ADD 1 TO WH-SEG.
       Z.  EXIT.
      ******************************************************* anhÑngen *
       ANHANG SECTION.
       A.  PERFORM VARYING WX FROM 160 BY -1
               UNTIL WB-SATZ(WX:1) not = SPACE continue.
           ADD 1 TO WX.
           MOVE WM-CD TO WB-SATZ(WX:).
       Z.  EXIT.
      ******************************************************************
       UMLAUTE SECTION.
       A.  INSPECT WM-CD REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹".
           PERFORM ANHANG.
       Z.  EXIT.
      ******************************************************************
       FAKTZEIL SECTION.
       A.  PERFORM BS-ZEIL.
           MOVE VDU-L TO WH-L.
           PERFORM UEBERTRAG.
       B.  MOVE WH-L TO VDU-L.
           MOVE SPACE TO DRC-SATZ WT-BEZ.
           UNLOCK ARTIKEL.
           MOVE 0 TO WK-RM.
           CALL "CAUP" USING "1300012480000" WH-CREG.
           DISPLAY "<ret>= Artikelnummer, <tab>= Text, <esc>= Ende" 
               AT 2301.
           DISPLAY "alpha+<ret>= suchen, '#'= Retourmenge" AT 2401.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0200060125" WH-CREG.
           MOVE WT-TX TO DRC-BEZ WV-MCODE.
           IF TABL PERFORM DRUCK GO A.
           IF ESC GO Z.
           IF KIST; IF WV-ARNUM = AR-NUM
               MOVE AR-NUM TO WH-NUM GO D.
           CALL "CAUP" USING "1500004004" WH-CREG.
           IF WH-NUM NOT = 0 GO D.
           CALL "PANANZ" USING "20AR-SUCH" WH-CREG.
           IF ESC GO B.
       D.  MOVE WH-NUM TO AR-NUM WD-ART.
           MOVE 1 TO VDU-P.
           CALL "CAUP" USING "1300010080000" WH-CREG.
           DISPLAY WD-ART AT VDU-LP.
       E.  READ ARTIKEL IGNORE LOCK INVALID PERFORM NO-REC GO B.
           MOVE AR-GRM TO WF-GRM.
           MOVE AR-PL TO RE-ARPL.
           MOVE AR-BEZ TO WT-ADR.
           IF KIST MOVE "      Retourmenge" TO WT-BEZ WR-ADR(1)
               MOVE 1 TO WK-RM.
           MOVE 6 TO VDU-P.
           DISPLAY WR-ADR(1) with SIZE 25 AT VDU-LP.
           IF WR-ADR(1) NOT = SPACE GO G.
       F.  DISPLAY "Artikelbez.: <ret>= Zeilenende, <#>= Textende"
                AT 2301.
           CALL "CAUP" USING "0200060225" WH-CREG.
           IF WOLI OR ESC MOVE WH-L TO VDU-L GO B. 
       G.  MOVE WH-L TO VDU-L.
           IF WR-ADR(2) NOT = SPACE ADD 1 TO VDU-L
               DISPLAY WR-ADR(2) witH size 25 AT VDU-LP.
       H.  CALL "CAUP" USING "1300320080000" WH-CREG.
           ADD 1 WF-MEH GIVING WH-MEH.
           MOVE 44 TO VDU-P.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           DISPLAY "<ret>= Menge, <ret-leer>= zum Betrag" AT 2301.
           MOVE 0 TO WH-WERT PR-MENGE PR-RETMG.
           IF WF-NK = 0 CALL "CAUP" USING "0000326007" WH-CREG.
           IF WF-NK = 1 CALL "CAUP" USING "0000325107" WH-CREG.
           IF WF-NK = 2 CALL "CAUP" USING "0000324207" WH-CREG.
           IF WOLI OR ESC MOVE WH-L TO VDU-L GO F.
           IF NOT RET GO H.
           IF WK-RM = 1 MULTIPLY WH-WERT BY -1 GIVING WH-WERT
                        MOVE WH-WERT TO PR-RETMG
                   else MOVE WH-WERT TO PR-MENGE.
           MOVE WH-WERT TO WH-ANZ.
           IF WH-ANZ = 0 MOVE SPACE TO DRC-MEH
               CALL "CAUP" USING "1300320080000" WH-CREG GO K.
           MOVE 32 TO VDU-P.
           PERFORM DISP-MENGE.
           IF ESC GO H.
           IF WT-MEH(WH-MEH) NOT = SPACE GO L.
           SUBTRACT 1 FROM WH-MEH GIVING WH-WERT.
       J.  DISPLAY "Mengeneinheiten-Kz. lt. Vorlaufdatei" AT 2301.
           CALL "CAUP" USING "0000441001" WH-CREG.
           IF WOLI OR ESC GO H.
           IF NOT RET GO J.
           ADD 1 WH-NUM GIVING WH-MEH.
           IF WH-MEH > 20 GO J.
           DISPLAY WT-MEH(WH-MEH) AT VDU-LP.
           IF WH-ANZ NOT = 0 GO L.
       K.  DISPLAY "<ret>= Betrag, <F1>= zur Menge" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "0000656209" WH-CREG.
           IF WOLI OR ESC GO H.
           IF NOT RET GO K.
           MOVE WH-WERT TO WH-BET.
           MOVE 0 TO WH-RAB.
           GO N.
       L.  MOVE DE-RAB TO WH-RAB.
           PERFORM SOPREI.
       M.  DISPLAY "Preis: <ret-leer>= Wiederholung, <ret>= Eingabe" 
               AT 2301.
           MOVE 49 TO VDU-P.
           IF WR-GR = 0 MOVE WH-PREIS TO WD-PREIS
               DISPLAY WD-PREIS with highlight AT VDU-LP
           ELSE MOVE WH-PREIS TO WD-APREIS
               DISPLAY WD-APREIS with highlight AT VDU-LP.
           CALL "CAUP" USING "0000494308" WH-CREG.
           IF WOLI OR ESC CALL "CAUP" USING "130064008000" WH-CREG GO H.
           IF NOT RET GO M.
           MOVE WH-WERT TO WH-PREIS WD-PREIS.
           IF WR-GR = 0 MOVE WH-PREIS TO WD-PREIS
               DISPLAY WD-PREIS with highlight AT VDU-LP
           ELSE MOVE WH-PREIS TO WD-APREIS
               DISPLAY WD-APREIS with highlight AT VDU-LP.
           CALL "CAUP" USING "1300640080000" WH-CREG.
           MOVE 60 TO VDU-P.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO WD-PROZ
               DISPLAY WD-PROZ AT VDU-LP.
           MULTIPLY WH-ANZ BY WH-PREIS GIVING WH-BET ROUNDED.
           COMPUTE WH-BET = WH-BET - (WH-BET * WH-RAB / 100).
       N.  MOVE AR-NUM TO LF-ARNUM.
           MOVE WH-BET TO WH-STAT.
           PERFORM ERLOEKTO.
           IF ESC MOVE WH-L TO VDU-L GO B.
           IF WOLI; IF WH-ANZ = 0 GO K ELSE GO M.
           MOVE AR-NUM TO WV-ARNUM.
           IF WM-KO = 3 WRITE PR-SATZ.
           GO A.
       Z.  EXIT.
      ******************************************** autom. Sonderpreis *
       SOPREI SECTION.
       A.  CALL "CAUP" USING "1300600080000" WH-CREG.
           MOVE WM-DATUM TO WC-DATUM WS-DATUM.
           IF DE-RAST = 9; IF AR-ADAT = 0 OR AR-ADAT > WS-DATUM
                   MOVE AR-VKP TO WH-PREIS WH-WERT GO Z
               ELSE MOVE AR-AVKP TO WH-PREIS WH-WERT GO Z.
           MOVE 0 TO WH-PREIS.
           MOVE DE-KTONR TO FA-KTONR.
           MOVE AR-NUM TO FA-ARNUM.
       B.  READ FAKTDAT INVALID GO F.
           IF ZUGRIF PERFORM BESETZT GO B.
           SET PX TO 4.
       D.  IF FA-AB(PX) = 0 OR FA-AB(PX) > WS-DATUM GO E.
           IF FA-BIS(PX) = 0 OR FA-BIS(PX) NOT < WS-DATUM
               MOVE FA-PREIS(PX) TO WH-WERT WH-PREIS 
               IF WM-RB = 1 MOVE FA-RAB(PX) TO WH-RAB GO F
                   ELSE IF FA-RAB(PX) NOT = 0
                        MOVE FA-RAB(PX) TO WH-RAB GO F ELSE GO F.
       E.  IF PX > 1 SET PX DOWN BY 1 GO D.
       F.  IF WH-PREIS NOT = 0 MOVE 64 TO VDU-P
               DISPLAY "Sonderpreis" with highlight AT VDU-LP GO Z.
           IF DE-PRDAT > AR-ADAT MOVE DE-PRDAT TO AR-ADAT.
           IF AR-ADAT = 0 OR AR-ADAT > WS-DATUM
               MOVE AR-PREIS TO WH-PREIS WH-WERT
           ELSE MOVE AR-APREIS TO WH-PREIS WH-WERT.
       Z.  EXIT.
      ****************************************** Ausdruck einer Zeile **
       ZEIDRUCK SECTION.
       A.  MOVE AR-NUM TO WH-NUM.
           PERFORM EAN.
           MOVE WR-ADR(1) TO DRC-BEZ.
           IF WR-ADR(2) NOT = SPACE PERFORM DRUCK
               MOVE WR-ADR(2) TO DRC-BEZ.
           IF WH-ANZ NOT = 0 MOVE WD-MENGE TO DRC-MENGE
               MOVE WT-MEH(WH-MEH) TO DRC-MEH
               IF WR-GR = 0 MOVE WH-PREIS TO DRC-PREIS
                      ELSE MOVE WH-PREIS TO DRC-APREIS.
           IF WH-RAB NOT = 0 MOVE WH-RAB TO DRC-PROZ.
           MOVE WS-SUM(9) TO DRC-BETRAG.
           MOVE WT-UST(WH-USTKZ) TO DRC-UST.
       X.  PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       FAKTENDE SECTION.
       A.  MOVE 0 TO WX-TASTE WW-BTTO.
           IF WZ-ZEILEN > 53 MOVE 70 TO WZ-ZEILEN.
           PERFORM UEBERTRAG.
           IF WS-BONUS not = 0 MOVE WV-BON TO DRD-PROZ
               MOVE "%" TO DRD-PZ
               MOVE "Retourwarenbonus" TO DRD-BEZ
               MOVE WS-BONUS TO DRC-BETRAG
               ADD WS-BONUS TO WS-BET
               PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRC-STR.
           PERFORM DRUCK.
           IF DE-KTOART > 1 MOVE "Ohne Umsatzsteuer!" TO DRD-BEZ.
           MOVE WS-BET TO DRC-BETRAG.
           MOVE WS-BET TO WW-NTTO.
           PERFORM DRUCK.
       B.  IF VDU-L > 20  PERFORM B IN BS-ZEIL GO B.
           IF VDU-L = 7 ADD 1 TO VDU-L.
           MOVE VDU-L TO WH-L.
           MOVE 65 TO VDU-P.
           DISPLAY ALL "ƒ" with SIZE 13 AT VDU-LP.
           PERFORM BS-ZEIL.
           MOVE WS-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           PERFORM BS-ZEIL.
           MOVE WS-RET TO WK-RET.
               IF WM-KO = 3 ADD WS-SUM(3) TO WK-RET
                       ELSE ADD WS-BET TO WK-RET
               END-IF COMPUTE WS-RET ROUNDED = (WS-RET / WK-RET) * 100.
       C.  MOVE 1 TO WX.
           MOVE WZ-ZEILEN TO WZ.
       F.  IF WT-BASIS(WX) = 0 GO G.
           IF DE-RAST = 2 OR DE-RAST = 9 GO G.
           IF DE-KTOART > 1 GO G.
           MOVE WT-BASIS(WX) TO WH-BET.
           COMPUTE WH-BET ROUNDED = WT-BASIS(WX) * WT-UST(WX) / 100.
           IF WH-BET = 0 MOVE SPACE TO DRD-SATZ GO G.
           MOVE WT-UST(WX) TO DRD-PROZ.
           MOVE "%" TO DRD-PZ.
           MOVE "Mehrwertsteuer von" TO DRD-BEZ.
           MOVE WT-BASIS(WX) TO DRD-BASIS.
           MOVE WH-BET TO DRC-BETRAG WD-BET.
           ADD WH-BET TO WH-MWST.
           ADD WH-BET TO WS-BET.
           MOVE 1 TO VDU-P.
           MOVE DRD-MWST TO WT-TX.
           DISPLAY WT-TX with highlight SIZE 55 AT VDU-LP.
           MOVE 65 TO VDU-P.
           DISPLAY WD-BET with highlight AT VDU-LP.
           PERFORM BS-ZEIL.
           PERFORM DRUCK.
       G.  IF WX < 6 ADD 1 TO WX GO F.
           MOVE WS-BET TO WH-BET.
      *--------------------------------------------> Akontozahlungen <-
           IF WH-PG = 9 GO K.
           IF WS-SUM(8) NOT = 0 MOVE "Akontozahlung" TO DRD-BEZ
               MULTIPLY -1 BY WS-SUM(8)
               MOVE WS-SUM(8) TO DRC-BETRAG WD-BET
               MOVE 65 TO VDU-P
               DISPLAY WD-BET with highlight AT VDU-LP
               MOVE 1 TO VDU-P
               MOVE DRD-MWST TO WT-TX
               DISPLAY WT-TX with highlight SIZE 55 AT VDU-LP
               PERFORM BS-ZEIL
               PERFORM DRUCK
               ADD WS-SUM(8) TO WH-BET WS-BET.
      *-> Rechnungss. f. Ueberleitung - Tagessumme USt: 8 * Summe: 9 <-
       K.  IF WZ-ZEILEN = WZ GO L.
           MOVE ALL "ƒ" TO DRC-STR.
           MOVE 65 TO VDU-P.
           IF WS-RET NOT = 0 AND WP-RET = 100;
               MOVE "Rwa." TO DRD-BEZ
               MOVE WS-RET TO DRD-PROZ
               MOVE "%" TO DRD-PZ.
           DISPLAY ALL "ƒ" with SIZE 13 AT VDU-LP.
           PERFORM BS-ZEIL.
           PERFORM DRUCK.
           IF WH-PG NOT = 9 MOVE WH-TX TO DRB-ADR.
           MOVE WS-BET TO DRC-BETRAG WD-BET WW-BTTO.
           MOVE "Eur" TO DRC-PROZ(2:).
           MOVE WH-BET TO WS-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           PERFORM DRUCK.
           PERFORM BS-ZEIL.
       L.  MOVE WH-BET TO WS-BET.
      *    IF WS-RET NOT = 0 AND WP-RET = 100;
      *        MOVE "Rwa." TO DRD-BEZ
      *        MOVE WS-RET TO DRD-PROZ
      *        MOVE "%" TO DRD-PZ.
           IF DE-RAST < 3 MOVE ALL "=" TO DRC-STR.
           PERFORM DRUCK.
           IF WH-PG = 9 GO Q.
           IF MANUEL PERFORM ZUS-TEXT.
           IF DE-RAST = 9 GO S.
           IF DE-RAST = 2 ADD 1 TO WH-SAKEY
               MOVE HIGH-VALUE TO KO-SATZ
               MOVE WH-SAKEY TO WH-KEY KO-NUM
               MOVE WX-FIL TO KO-FIL
               MOVE DE-REKTO TO KO-REKTO
               MOVE DE-KTONR TO KO-KONTO
               MOVE 0 TO KO-ERLOES(1) KO-ERLOES(2) KO-ERLOES(3)
                   KO-ERLOES(4) KO-ERLOES(5) KO-ERLOES(6) KO-ERLOES(7)
                   KO-ERLOES(8) KO-ERLOES(9) KO-ERLOES(10)
               MOVE WM-DATUM TO KO-DAT(1)
               MOVE WH-ABDAT TO KO-DAT(2)
               MOVE WH-BISDAT TO KO-DAT(3)
               MOVE 1 TO WX GO R.
           IF WK-ATUNR not = 0 MOVE "Uid-Nr.: " TO DRB-TX(1:)
               MOVE WK-UID TO DRB-TX(10:).
           MOVE "Zahlbar bei Erhalt der Rechnung ohne Abzug"
               TO DRB-TX(23:).
           PERFORM DRUCK.
      *--------------------------------------> Addition Erloessummen <-
       Q.  MOVE 6 TO WH-KEY.
           READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO Q.
           MOVE 1 TO WX.
       R.  ADD WT-BASIS(WX) TO KO-ERLOES(WX).
           IF WX < 6 ADD 1 TO WX GO R.
           ADD WH-MWST   TO KO-ERLOES(9).
           ADD WS-BET    TO KO-ERLOES(10).
           ADD WS-SUM(3) TO KO-ERLOES(7).
           ADD WS-SUM(4) TO KO-ERLOES(8).
           IF DE-RAST > 2 GO S.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           IF DE-RAST < 2 PERFORM VERBUCHE.
       S.  IF WH-PG = 9 PERFORM ZUS-TEXT.
           PERFORM END-ARC.
           PERFORM FAKT-AUF.
           IF WM-DRU = 0 PERFORM END-DRU
                    else PERFORM PAGEDRU.
           IF MANUEL GO Z.
       T.  IF VDU-L > 7 PERFORM B IN BS-ZEIL GO T.
       Z.  EXIT.
      ******************************************************************
       ERLOEKTO SECTION.
       A.  MOVE WH-BET TO WD-BET WS-SUM(9).
           MOVE 65 TO VDU-P.
           CALL "CAUP" USING "1300650080000" WH-CREG.
           DISPLAY WD-BET with highlight AT VDU-LP.
           IF RE-ARPL = 2 ADD WH-BET TO WS-SUM(8) GO Z.
           IF WK-RM = 1 SUBTRACT WH-BET FROM WS-RET.
           MOVE AR-UST TO WX.
       B.  MOVE WX TO WH-USTKZ.
           MOVE 79 TO VDU-P.
           DISPLAY WH-USTKZ AT VDU-LP.
           IF NOT MANUEL GO E.
           DISPLAY "<ret>= Zeile ok., <>= zurueck, <esc>= Abbruch, <tab
      -       ">= Ust-Kz (1-6)" AT 2301.
           CALL "CAUP" USING "0000791001" WH-CREG.
           MOVE WH-NUM TO WH-USTKZ.
           IF ESC OR WOLI GO Z.
           IF RET; IF WH-USTKZ > 0
               DISPLAY "keine Eingabe erlaubt" AT 2401
               PERFORM WEITER GO B
           ELSE MOVE WX TO WH-USTKZ GO E.
           IF NOT TABL GO B.
           IF WH-USTKZ > 0 AND WH-USTKZ < 7 MOVE WH-USTKZ TO WX GO B.
       D.  DISPLAY "Ust-Kz falsch" AT 2401.
           PERFORM WEITER.
           GO B.
       E.  IF WX = 0 GO D.
           MOVE 79 TO VDU-P.
           DISPLAY WH-USTKZ AT VDU-LP.
           ADD WH-BET TO WS-BET.
           MOVE WS-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT 0365.
      *----------------------> Addition Ust-Basen & Rechnungssummen <-
980430     IF WF-TB > 3 MOVE 2 TO WF-TB.             *> BÑckereiartikel
           IF WF-TB < 3 ADD WH-BET TO WS-SUM(3)
                   ELSE ADD WH-BET TO WS-SUM(4).
           ADD WH-STAT TO WT-BASIS(WX).
      *------------------------------> Addition in Artikelstatistik <-
           MOVE 0 TO WT-KTONR.
           IF WM-RE = 1 PERFORM EAN GO Z.
           IF WM-RE NOT = 2 PERFORM ZEIDRUCK.
           IF DE-RAST = 9 COMPUTE WH-BET ROUNDED =
                    WH-BET / ((WT-UST(WX) + 100) / 100).
           MULTIPLY AR-EKP BY WH-ANZ GIVING WH-WERT.
           SUBTRACT WH-WERT FROM WH-BET GIVING WH-WERT.
           MOVE WH-ANZ TO WM-ANZ.
           IF WF-NK = 2 MULTIPLY 100 BY WH-ANZ.
           IF WF-NK = 1 MULTIPLY  10 BY WH-ANZ.
      *------------------------------------------> Addition Statistik <-
           PERFORM ADDSTAT.                          *> Artikelstatistik
           IF DE-STATIS = 1 OR DE-STATIS = 3
               MOVE DE-KTONR TO WT-KTONR
               PERFORM ADDSTAT.                      *> Kundenstatistik
           IF WM-KO = 3 COMPUTE WT-KTONR = (DE-TOUR * 10) + 2
               PERFORM ADDSTAT.                      *> Fahrerstatistik
           MOVE WM-ANZ TO WH-ANZ.
       Z.  EXIT.
      ****************************************** Addition in Statistik *
       ADDSTAT SECTION.
       A.  MOVE WX-J TO ST-JAHR.
           MOVE AR-NUM TO ST-ARNUM.
           MOVE WT-KTONR TO ST-KTONR.
           READ STATISTIK INVALID KEY INITIALIZE ST-SATZ
               MOVE WX-J TO ST-JAHR
               MOVE AR-NUM TO ST-ARNUM
               MOVE WT-KTONR TO ST-KTONR.
           IF ZUGRIF PERFORM BESETZT GO A.
           IF WK-RM = 0 ADD WH-ANZ TO ST-MENGEN(WX-M)
                   ELSE ADD WH-ANZ TO ST-RET(WX-M).
           ADD WH-WERT TO ST-GEWINN(WX-M).
           ADD WH-STAT TO ST-UMSATZ(WX-M).
           REWRITE ST-SATZ INVALID KEY WRITE ST-SATZ.
       Z.  EXIT.
      ******************************************************************
       ZUS-TEXT SECTION.
       A.  IF WH-PG = 9 GO F.
           PERFORM BS-ZEIL.
           PERFORM DRUCK.
           PERFORM BS-ZEIL.
       B.  SUBTRACT WZ-ZEILEN FROM 58 GIVING WD-POS.
           IF WZ-ZEILEN NOT < 58 DISPLAY "kein weiterer Text moeglich"
               AT 2401 PERFORM WEITER GO Z.
           DISPLAY "<tab>= Textmoeglichkeit, <ret-leer>= Rechnungsende  
      -        "*  noch freie Zeilen: " AT 2301 WD-POS.
       C.  MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0200110125" WH-CREG.
           MOVE WT-TX TO DRC-BEZ.
           IF RET AND DRC-BEZ = SPACE GO Z.
           IF WOLI GO Z.
           IF NOT TABL GO B.
           PERFORM DRUCK.
           PERFORM BS-ZEIL.
           GO B.
      *--------------------------------------------> Abrechnungsrest <-
       F.  MOVE 0 TO WX-TASTE.
           PERFORM UEBERTRAG.
           MOVE WZ-ZEILEN TO WZ.
           MOVE WK-D TO WH-KEY.
       H.  READ KONSTANT INVALID KEY PERFORM NO-REC GO A.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF WM-KO = 2 MOVE 0 TO KO-REST.
           IF KO-REST NOT = 0 MOVE "Diff. Vortag" TO DRC-BEZ
               MOVE KO-REST TO DRC-BETRAG
               PERFORM DRUCK.
           IF KO-SUM(2) NOT = 0 MOVE "Ablieferung" TO DRC-BEZ
               MULTIPLY -1 BY KO-SUM(2) GIVING DRC-BETRAG
               PERFORM DRUCK.
           IF KO-SUM(3) NOT = 0; IF WM-KO = 3 PERFORM INKASOLIST
               end-if MOVE "Kundeninkassi" TO DRC-BEZ
                      MOVE KO-SUM(3) TO DRC-BETRAG
                      ADD KO-SUM(3) TO KO-REST
                      PERFORM DRUCK.
           ADD WS-BET TO KO-REST.
           SUBTRACT KO-SUM(2) FROM KO-REST.
           IF WS-SUM(8) NOT = 0 MOVE "Akontozahlungen" TO DRC-BEZ
               MULTIPLY -1 BY WS-SUM(8)
               MOVE WS-SUM(8) TO DRC-BETRAG
               ADD WS-SUM(8) TO KO-REST
               PERFORM DRUCK.
           IF WZ-ZEILEN NOT = WZ MOVE ALL "ƒ" TO DRC-STR
               PERFORM DRUCK.
           MOVE "Abrechnungsdiff." TO DRC-BEZ
           MOVE KO-REST TO DRC-BETRAG WD-BET
           PERFORM DRUCK.
           DISPLAY DRC-BEZ AT 1030
                   WD-BET  with highlight AT 1065.
           PERFORM DRUCK.
           MOVE WM-DATUM TO KO-ABRDAT.
           REWRITE KO-SATZ.
       Z.  EXIT.
      ******************************************** autom. Rasterdruck *
       RASTER SECTION.
311205 A.  IF DE-RAST = 5 GO Z.                   *> keie Rechng/Raster
           ADD 2 TO WM-DRU.
           MOVE 0 TO WZ-SEITE WZ-SCHALT.
           MOVE 25 TO VDU-P.
           SET WY TO XX.
           MOVE WX-FIL TO FA-AKEY.
           START FAKTDAT KEY = FA-AKEY INVALID GO X.
       C.  READ FAKTDAT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
      *    perform retsich.
           IF FA-KTONR not = DE-KTONR GO X.
           IF FA-MON > WK-VON OR FA-ART NOT = WH-MOD GO X.
       E.  MOVE 0 TO WS-MENGE WS-RETM WH-ANZ WK-ANZ.
           MOVE 7 TO VDU-L.
           CALL "CAUP" USING "1307010880000" WH-CREG.
           MOVE FA-ARNUM TO AR-NUM.
       I.  READ ARTIKEL IGNORE LOCK INVALID
                 MOVE "Artikel fehlt" TO AR-BEZ.
           MOVE WH-ABDAT TO WZ-DATUM.
           MOVE AR-GRM TO WF-GRM.
           SET DY TO 1.
           SET OX TO WZ-TAG.
       K.  IF OX > WV-ULT AND OX < 32 SET OX UP BY 1 GO K.
           IF FA-MENGE(OX) = 0 and FA-RETOUR(OX) = 0 GO L.
           PERFORM RASTKOPF.
           IF WK-ANZ = 0 MOVE AR-BEZ TO DRR-BEZ
               MOVE AR-NUM TO DRR-ARNUM.
           IF WF-NK = 0 MOVE FA-MENGE(OX) TO DRR-MA(DY).
           IF WF-NK = 1 DIVIDE 10 INTO FA-MENGE(OX) GIVING DRR-MB(DY).
           IF WF-NK = 2 DIVIDE 100 INTO FA-MENGE(OX) GIVING DRR-MB(DY).
           ADD FA-MENGE(OX) TO WS-MENGE.
           MOVE 1 TO WK-ANZ.
       L.  IF DY < WI SET DY UP BY 1
               SET OX UP BY 1 GO K.
           MOVE WS-MENGE TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MG TO WV-MCODE.
           SET MX TO 1.
           SET XX TO WY.
       M.  MOVE WV-MC(MX) TO DRY-X(XX + 2).
           IF MX < 7 SET MX XX UP BY 1 GO M.
           MOVE DRR-STR TO WT-TX.
           DISPLAY WT-TX with SIZE 74 AT 2005.
           IF WK-ANZ > 0; IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:)
                          end-if
                          PERFORM DRUCK.
           SET DY TO 1.
           SET OX TO WZ-TAG.
       N.  IF OX > WV-ULT AND OX < 32 SET OX UP BY 1 GO N.
           IF FA-RETOUR(OX) = 0 GO O.
           IF WZ-ZEILEN > (WZ-SIZE - 2) PERFORM RASTKOPF.
           IF WK-ANZ = 0 MOVE AR-BEZ TO DRR-BEZ
               MOVE AR-NUM TO DRR-ARNUM              *> nur Retourmenge
               IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:)
               end-if
               PERFORM DRUCK
               ADD 1 TO WK-ANZ.
           IF WF-NK = 0 MOVE FA-RETOUR(OX) TO DRR-MA(DY).
           IF WF-NK = 1 DIVIDE 10 INTO FA-RETOUR(OX) GIVING DRR-MB(DY).
           IF WF-NK = 2 DIVIDE 100 INTO FA-RETOUR(OX) GIVING DRR-MB(DY).
           ADD FA-RETOUR(OX) TO WS-RETM.
           ADD 2 TO WK-ANZ.
       O.  IF DY < WI SET DY UP BY 1
               SET OX UP BY 1 GO N.
           IF WK-ANZ = 0 GO S.
           IF WK-ANZ = 1 GO Q.
           MOVE WS-RETM TO WH-ANZ.
           PERFORM MG.
           MOVE WD-MG TO WV-MCODE.
           MOVE "      Retourmenge" TO DRR-BEZ.
           SET XX TO WY.
           SET MX TO 1.
       P.  MOVE WV-MC(MX) TO DRY-X(XX + 2).
           IF MX < 7 SET MX XX UP BY 1 GO P.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
       Q.  MOVE WT-ADR TO DRA-SATZ.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
       S.  READ FAKTDAT NEXT AT END MOVE 99999 TO WV-KTONR GO X.
           IF ZUGRIF PERFORM BESETZT GO S.
           IF FA-KTONR NOT = DE-KTONR GO X.

           perform retsich.

           SET OX TO 1.
           GO E.
       X.  IF WM-DRU = 3 MOVE 0 TO WZ-SCHALT
                MOVE WE-STG(5) TO DRA-SATZ(3:)
                PERFORM DRUCK
           else MOVE SPACE TO DRA-SATZ
                PERFORM PAGEDRU.
           ADD -2 TO WM-DRU.
           MOVE WX-FIL TO FA-AKEY.
           START FAKTDAT KEY = FA-AKEY.             *> mu· noch da sein
       Y.  READ FAKTDAT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO Y.
           CALL "CAUP" USING "1320012480000" WH-CREG.
           MOVE 0 TO WZ-SEITE WZ-SCHALT WZ-ZEILEN.
       Z.  EXIT.
      ******************************************* Kopf der Rasterliste *
       RASTKOPF SECTION.
       A.  IF WZ-ZEILEN > WZ-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           IF WM-OPEN = 0 MOVE 5 TO WH-P
                          PERFORM BEG-DRU.
      *                   OPEN EXTEND ABLAGE.
           IF MON or HALB MOVE WE-STG(9) TO DRA-SATZ(3:)
                          MOVE 41 TO WZ-SIZE
                     else MOVE WE-STG(5) TO DRA-SATZ(3:)
                          MOVE 61 TO WZ-SIZE.
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT
           MOVE "&l26a4H" TO DRA-SATZ.                       *> Fach 2
           PERFORM DRUCK.
      *-----------------------------> Drucker geîffnet u. eingestellt <-
       C.  DIVIDE 10 INTO DE-KTONR GIVING DRM-NUM.
           MOVE DE-BEZ TO DRM-NAME.
           INSPECT DRM-NAME REPLACING ALL "#" BY ",".
           MOVE 0 TO WZ-SCHALT.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE "Art Bezeichnung" TO DRR-STR.
           MOVE 1 TO WX.
           MOVE WH-ABDAT TO WZ-DATUM.
       E.  MOVE WZ-TAG TO DRR-TG(WX).
           MOVE "." TO DRR-P(WX).
           ADD 1 TO WZ-TAG.
           IF WZ-TAG > WV-ULT MOVE 1 TO WZ-TAG.
           IF WX < WI ADD 1 TO WX GO E.
           COMPUTE WY = WX * 6 + 38.
           ADD 3 TO WY.
           MOVE "Summe#" TO DRY-SATZ(WY:).
           MOVE DRA-SATZ TO WT-ADR.
           MOVE SPACE TO DRA-SATZ.
           MOVE "Lieferzeitraum vom:" TO DRA-SATZ(19:20).
           MOVE WH-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VD-DATUM TO DRA-SATZ(39:10).
           MOVE "bis:" TO DRA-SATZ(50:5).
           MOVE WH-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VD-DATUM TO DRA-SATZ(55:10).
           COMPUTE WY = WX * 6 + 38.
           SET XX TO WY.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           SET XX DOWN BY 21.
           IF XX < 65 SET XX TO 65.
           SET DX TO 6 PERFORM DASEIT.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           COMPUTE WY = WX * 6 + 38.
           SET XX TO WY.
           MOVE WT-ADR TO DRA-SATZ.
           INSPECT DRA-SATZ REPLACING CHARACTERS BY "ƒ" BEFORE "#".
           MOVE "ƒ" TO DRY-X(XX + 8).
           MOVE SPACE TO DRM-NO.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE WT-ADR TO DRA-SATZ.
           MOVE SPACE TO DRY-X(XX + 8).
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE WT-ADR TO DRA-SATZ.
           INSPECT DRA-SATZ REPLACING CHARACTERS BY "ƒ" BEFORE "#".
           MOVE "ƒ" TO DRY-X(XX + 8).
           MOVE SPACE TO DRM-NO.
           MOVE DRA-SATZ TO WT-ADR.
           IF MON MOVE DRA-SATZ(6:) TO DRA-SATZ(1:).
           PERFORM DRUCK.
           MOVE WH-ABDAT TO WZ-DATUM.
       Z.  EXIT.
      *********************************************** Listladeroutine *
       DASEIT SECTION.
       A.  MOVE VD-DATUM TO WK-DAT.
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WK-SEITE.
       B.  MOVE WX-DS(DX) TO DRY-X(XX).
           IF DX < 26 SET DX XX UP BY 1 GO B.
       Z.  EXIT.
      *************************************************** Inkassoliste *
       INKASOLIST SECTION.
       A.  OPEN I-O INKASSO.
       C.  READ INKASSO INVALID GO F.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 50
               IF IN-BET(WX) NOT = 0 PERFORM INSUM.
           DELETE INKASSO.
           CLOSE INKASSO.
       F.  MOVE WK-D TO WH-KEY.
           READ KONSTANT INVALID KEY STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO F.
       Z.  EXIT.
      ******************************************************************
       INSUM SECTION.
       A.  PERFORM UEBERTRAG.
           MOVE DE-SATZ TO FA-SATZ.
           MOVE IN-KTONR(WX) TO DE-KTONR.
           MOVE 1 TO DE-FNR.
       C.  READ DEBITOR IGNORE LOCK INVALID GO Z.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           MOVE DE-KTONR TO WD-KTO.
           MOVE DE-KTONR TO DRA-SATZ(8:8).
           MOVE WT-BEZ(1:40) TO DRC-BEZ.
           MOVE IN-BET(WX) TO DRC-BETRAG.
           PERFORM DRUCK.
           MOVE FA-SATZ TO DE-SATZ.
       Z.  EXIT.
      ***************************************** Rechnungs-Inkassoliste *
       INK-LIST SECTION.
       A.  CALL "CAUP" USING "1304012580" WH-CREG.
           MOVE "INKASSO.LST" TO WH-DRUNAM.
           DISPLAY WH-DRUNAM with highlight AT 0333.
           MOVE 99 TO WV-TOUR.
           IF WM-DRU = 1 ADD 2 TO WM-DRU.
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT WM-OPEN WS-BET.
           MOVE 0 TO DE-FAKART DE-TOUR DE-SUB.
           START DEBITOR KEY NOT < DE-IKEY INVALID GO Z.
       C.  READ DEBITOR NEXT IGNORE LOCK AT END MOVE 99 TO WV-TOUR GO K.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 1020 " " DE-BEZ(1:50).
           MOVE WM-DATUM TO WZ-DATUM.
           IF DE-REDAT not = WZ-DATUM or DE-INKASSO = 0 GO C.
           IF WV-TOUR = 99 MOVE DE-TOUR TO WV-TOUR.
           IF WV-TOUR not = DE-TOUR GO K.
       E.  IF WZ-ZEILEN > 62 WRITE DRA-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           IF WM-OPEN = 0 MOVE X"3400" TO WH-PX(1)
               MOVE X"000C" TO WH-PX(2)
               IF WM-DRU = 1 or 3 MOVE 5 TO WH-P
               end-if
               PERFORM BEG-DRU.
           IF WM-DRU = 1 MOVE "&l26a4H" TO DRA-SATZ(3:)      *> Fach 2
               MOVE 0 TO WZ-SCHALT
               PERFORM DRUCK.
           ADD 10 WV-TOUR GIVING WH-KEY.
       F.  READ KONSTANT IGNORE LOCK INVALID
                 MOVE "- fehlt -" TO KO-TOUR.
           MOVE WV-TOUR TO WD-TOUR.
           MOVE "Inkassoliste fÅr Tour:" TO DRL-STR.
           MOVE WD-TOUR TO DRL-STR(24:2).
           MOVE KO-TOUR TO DRL-STR(27:).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VD-DATUM TO DRL-STR(55:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRL-STR.
           PERFORM DRUCK.
       G.  DIVIDE 10 INTO DE-KTONR GIVING DRL-KTONR.
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(1) TO DRL-BEZ.
           MOVE DE-RENR TO DRL-RENUM.
           MOVE DE-REBET TO DRL-BET.
           ADD DE-REBET TO WS-BET.
           MOVE 2 TO WZ-SCHALT.
           PERFORM DRUCK.
           GO C.
      *--------------------------------------------------> Tourenende <-
       K.  IF WM-OPEN not = 0 MOVE ALL "ƒ" TO DRL-STR
               PERFORM DRUCK
               MOVE "Inkassosumme:" TO DRL-BEZ
               MOVE WS-BET TO DRL-BET
               MOVE 0 TO WS-BET
               PERFORM DRUCK.
           IF WV-TOUR = 99 GO W.
           MOVE DE-TOUR TO WV-TOUR.
           MOVE 80 TO WZ-ZEILEN.
           GO E.
       W.  PERFORM END-DRU.
           IF WM-DRU = 3 MOVE 1 TO WM-DRU.
       Z.  EXIT.
      ******************************************************************
       VERBUCHE SECTION.
       A.  OPEN I-O BUCHFAKT
           MOVE 99999 TO BF-KEY.
           MOVE 1 TO WH-BUKEY.
           START BUCHFAKT KEY < BF-KEY INVALID GO E.
       C.  READ BUCHFAKT PREVIOUS IGNORE LOCK AT END GO E.
           ADD 1 BF-KEY GIVING WH-BUKEY.
       E.  INITIALIZE BF-SATZ.
           MOVE DE-KTONR TO BF-KTONR.
           IF WM-VER = 3 MOVE DE-REKTO TO BF-KTONR.
           MOVE DE-TOUR TO BF-GK.
           MOVE WM-DATUM TO BF-DAT.
           MOVE SPACE TO BF-TX.
           MOVE WH-MWST TO BF-MW.
           MOVE WT-BASIS(1) TO BF-B1.
           MOVE WT-BASIS(2) TO BF-B2.
           MOVE WS-BET TO BF-BET.
           MOVE WE-ARSYM TO BF-SY.
           MOVE 1 TO BF-VM.
           MOVE 0 TO BF-Z.
           MOVE 0 TO BF-SH.
           IF WS-BET < 0 MOVE WE-GSSYM TO BF-SY.
           MOVE WH-RENUM TO BF-REN.
           IF DE-KOND = 0 MOVE 1 TO DE-KOND.
           MOVE WT-KONDIT(DE-KOND) TO BF-KOND WE-NTG.
           IF WT-BASIS(1) NOT = 0 MOVE 1 TO BF-U1.
           IF WT-BASIS(2) NOT = 0 MOVE 2 TO BF-U2.
           IF BF-MW = 0 MOVE 3 TO BF-U1
               ADD BF-U2 TO BF-U1
               MOVE 0 TO BF-U2.
       F.  MOVE WH-BUKEY TO BF-KEY.
           WRITE BF-SATZ INVALID ADD 1 TO WH-BUKEY GO F.
      *-------------------> KundenÅberleitung nur wenn NEU oder éNDG. <-
           IF DE-ANLMERK = 0 GO X.
           MOVE 0 TO DE-ANLMERK.
           REWRITE DE-SATZ.
           MOVE "K" TO BF-SA.
           MOVE DE-MCODE TO BF-MCODE.
           MOVE WT-KONDIT(DE-KOND) TO BF-KOND.
           MOVE DE-ANR TO BF-ANREDE.
           MOVE DE-BEZ TO BF-BEZ.
           MOVE SPACE TO BF-TEL.
       G.  ADD 1 TO WH-BUKEY.
           MOVE WH-BUKEY TO BF-KEY.
           WRITE BF-SATZ INVALID GO G.
       X.  CLOSE BUCHFAKT.
       Z.  EXIT.
      ***************************************** autom. Sammelrechnung *
       SARE SECTION.
       A.  MOVE 4 TO WH-MOD.
           MOVE 40 TO WH-KEY.
           MOVE 0 TO WV-KTONR.
           START KONSTANT KEY > WH-KEY INVALID KEY GO X.
       B.  READ KONSTANT NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF WH-KEY > 49 GO X.
           IF WV-KTONR NOT = 0 GO E.
           MOVE KO-DAT(1) TO WM-DATUM.
           MOVE KO-DAT(2) TO WH-ABDAT.
           MOVE KO-DAT(3) TO WH-BISDAT.
           MOVE KO-REKTO TO WV-KTONR DE-KTONR.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY "Kunden-Nr.:" AT 0301.
           DISPLAY WD-KTO with highlight AT 0313.
           MOVE 1 TO DE-FNR.
       C.  READ DEBITOR IGNORE LOCK INVALID
               DISPLAY "Kunde fehlt!" AT 2401
               PERFORM WEITER GO X.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ with SIZE 78 AT 0401.
           IF DE-RAST = 2 MOVE 1 TO DE-RAST.
           MOVE WH-KEY TO WH-SAKEY.
           PERFORM FAKTKOPF.
           MOVE WH-SAKEY TO WH-KEY.
       D.  READ KONSTANT INVALID KEY GO B.
           IF ZUGRIF PERFORM BESETZT GO D.
       E.  IF WV-KTONR NOT = KO-REKTO GO B.
           PERFORM UEBERTRAG.
           MOVE KO-FIL TO DRC-BEZ.
           MOVE 1 TO WX.
       F.  IF KO-ERLOES(WX) NOT = 0 MOVE KO-ERLOES(WX) TO WH-BET
               MOVE WT-UST(WX) TO DRC-UST
               ADD WH-BET TO WT-BASIS(WX)
               MOVE WH-BET TO DRC-BETRAG
               ADD WH-BET TO WS-BET
               PERFORM DRUCK.
           IF WX < 6 ADD 1 TO WX GO F.
           MOVE KO-ERLOES(7) TO WS-SUM(3).
           MOVE KO-ERLOES(8) TO WS-SUM(4).
           DELETE KONSTANT.
           GO B.
       G.  IF WV-KTONR NOT = 0 PERFORM FAKTENDE GO A.
       X.  CALL "CAUP" USING "06KOPF" WH-CREG.
       Z.  EXIT.
      ********************** PrÅfung der FAKT-Periode mit LIST-Periode *
       PERPRUEF SECTION.
       A.  MOVE 0 TO WX-TASTE.
           OPEN I-O STATISTIK.
       C.  MOVE 0 TO WH-ABDAT WH-BISDAT.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WS-TASTE = 50 MOVE WX-DATUM TO WM-DATUM
               MOVE WZ-MONAT TO WX-M
               MOVE WZ-JAHR TO WX-J
               MOVE 1 TO WI GO Z.
           EVALUATE WE-WO(1)
               WHEN 0 MOVE "FAKT.LST" TO WH-DRUNAM
                      DISPLAY WH-DRUNAM with highlight AT 0365
               WHEN 9 MOVE "LPT1" TO WH-DRUNAM
                      DISPLAY "Direktdruck" with highlight AT 0365.
           DISPLAY "Fakt.-Art:" AT 0311.
           if wm-datum < 19991219
               display "Bitte unbedingt vor Rechnungslauf 'PRöFUNG RECHN
      -         "UNGSPERIODE'" with highlight foreground-color 6 at 1910
                "Progr.: Lieferschein / PrÅflauf Pkt. 3 - durchfÅhren!"
                    with highlight foreground-color 6 at 2010.
           DISPLAY "<esc>= Abbruch, <ret>= Fakt.-Grp." AT 2301.
           DISPLAY "< >= Wechsel Druckausgabe" AT 2401.
           DISPLAY "0 - manuelle Rechnung"    AT 0710
                   "1 - Monatsrechnungen"     AT 0810
                   "2 - Monatsrechnungen"     AT 0010
                   "3 - Wochenrechnungen"     AT 0010
                   "4 - Wochenrechnungen"     AT 0010
                   "5 - Halbmonatsrechnungen" AT 0010
                   "6 - Tagesrechnung"        AT 0010
                   "7 - Tagessummen"          AT 0010
                   "8 - EDIFACT-Erstellung"   AT 0010
                   "9 - Rechnungsaufstellung" AT 0010
           DISPLAY "bitte wÑhlen Sie:  " with highlight AT 1830.
           CALL "CAUP" USING "0018481001" WH-CREG.
           IF AB; IF WE-WO(1) = 0 MOVE 9 TO WE-WO(1)
                             else MOVE 0 TO WE-WO(1).
           MOVE WH-NUM TO WH-MOD.
           if sapo perform fakorr.
           IF ESC GO Z.
           IF EINF PERFORM RE-TEXT.
           IF NOT RET GO C.
           IF WH-MOD = 7 MOVE 7 TO WH-PG GO Z.
           MOVE 40 TO WH-SAKEY.
           MOVE 0 TO WM-RE.
           IF WH-MOD > 9 GO C.
           DISPLAY "Rechnungsdatum muss Lieferdatum sein" AT 2401.
           EVALUATE WH-MOD
               WHEN 0 MOVE " manuelle Rechnung" TO WK-GEB
               WHEN 1 MOVE " monatl. Rechnung 1" TO WK-GEB
               WHEN 2 MOVE " monatl. Rechnung 2" TO WK-GEB
               WHEN 3 MOVE " wîchentl. Rechnung 3" TO WK-GEB
               WHEN 4 MOVE " wîchentl. Rechnung 4" TO WK-GEB
               WHEN 5 MOVE " halbmonatl. Rechng." TO WK-GEB
               WHEN 6 MOVE " Tagesrechnung" TO WK-GEB
               WHEN 8 MOVE " EDIFACT-Erstellung" TO WK-GEB
               WHEN 9 MOVE " Rechnungsaufstellung" TO WK-GEB.
           DISPLAY WK-GEB AT 0324.
           MOVE SPACE TO WK-RETX.
       D.  CALL "CAUP" USING "130350380000" WH-CREG.
           DISPLAY "Rechnungsdatum:" AT 0350.
           DISPLAY "<ret-leer>= Tagesdatum" AT 2301.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0366.
           MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0103666006" WH-CREG.
           IF ESC GO Z.
           IF WX-DATUM = 0 GO D.
           DISPLAY VDU-DATUM with highlight AT 0366.
           MOVE WX-DATUM TO WM-DATUM.
           IF MANUEL OR SOFORT MOVE WZ-MONAT TO WX-M
               MOVE WZ-JAHR TO WX-J
               MOVE WT-KON(WZ-MONAT) TO WV-ULT
               MOVE 1 TO WI GO Z.
           MOVE WK-MON TO WZ-DATUM WH-BISDAT.
           MOVE WZ-MONAT TO WX-M.
           MOVE WZ-JAHR TO WX-J
           MOVE 1 TO WZ-TAG.
           MOVE WZ-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           MOVE WX-DATUM TO WH-ABDAT.
      *    IF WH-MOD = 8 PERFORM DUPLO GO K.
           IF WH-MOD = 8 GO Z.                    *> Edifact-Erstellung
           IF WH-MOD = 9 PERFORM RE-AUFST
                         GO Z.
       E.  DISPLAY "Lieferdatum von:          bis:" AT 0410.
           MOVE WH-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM AT 0427.
           MOVE WH-ABDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0104276006" WH-CREG.
           IF WOLI GO C.
           IF ESC GO Z.
           IF WZ-DATUM = 0 GO E.
           DISPLAY VDU-DATUM with highlight AT 0427.
           IF WX-DATUM < WH-ABDAT DISPLAY "Datumsfehler" AT 2401
               PERFORM WEITER GO E.
           MOVE WX-DATUM TO WH-ABDAT.
           MOVE WT-KON(WZ-MONAT) TO WI WV-ULT.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WH-ABDAT).
           EVALUATE WH-MOD
               WHEN 1
               WHEN 2 MOVE WT-KON(WZ-MONAT) TO WI WV-ULT
               WHEN 3
               WHEN 4 ADD 6 TO WA-DAT
                      COMPUTE WH-BISDAT =
                              FUNCTION DATE-OF-INTEGER(WA-DAT)
               WHEN 5 IF WZ-TAG < 10 ADD 14 TO WA-DAT
                          COMPUTE WH-BISDAT =
                                  FUNCTION DATE-OF-INTEGER(WA-DAT)
                      else MOVE WT-KON(WZ-MONAT) TO WI WV-ULT
               WHEN 6 MOVE WH-ABDAT TO WH-BISDAT.
           if wz-monat = 2 compute wh-num = wh-abdat / 10000
               divide 4 into wh-num giving wh-num remainder wi
               if wi not = 0 move 28 to wi wv-ult
                        else move 29 to wi wv-ult.
       F.  MOVE WH-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0441.
           MOVE WH-BISDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0104416006" WH-CREG.
           IF WOLI GO D.
           IF ESC GO Z.
           IF WX-DATUM = 0 GO F.
           IF WX-DATUM < WH-ABDAT DISPLAY "Datum zu klein" AT 2401
               PERFORM WEITER GO F.
           DISPLAY VDU-DATUM with highlight AT 0441.
           IF MON OR HALB; IF WX-DATUM > WH-BISDAT
               DISPLAY "Datum zu gross" AT 2401
               PERFORM WEITER GO F.
           MOVE WX-DATUM TO WH-BISDAT.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WH-BISDAT).
           COMPUTE WA-DAT = WA-DAT - FUNCTION INTEGER-OF-DATE(WH-ABDAT).
           ADD 1 TO WA-DAT GIVING WI WD-TG.
           IF MON GO J.
           DISPLAY WD-TG with highlight AT 0375.
           IF WOCH; IF WI > 7;
               IF WI > 10 DISPLAY "Datumsfehler" AT 2401
                    PERFORM WEITER GO F
               else DISPLAY "ACHTUNG " with highlight AT 2401
                    "mehr als 7 Tage" AT 2400 PERFORM WEITER.
       J.  MOVE WX-DATUM TO WH-BISDAT.
       K.  DISPLAY "<ret/#>= Start, <esc>= Abbruch " AT 2301.
           IF WH-MOD not = 8;
               IF WM-DRU = 1 DISPLAY "<#> statt <ret>= ohne Rasterdruck"
                             with highlight AT 2401.
           CALL "CAUP" USING "0023320000" WH-CREG.
           IF ESC GO C.
           IF KIST MOVE 0 TO WM-RAST
                   SET RET TO TRUE
              else MOVE 1 TO WM-RAST.
           IF NOT RET GO K.
       Z.  EXIT.
      ************************************************** Rechnungstext *
       RE-TEXT SECTION.
       A.  MOVE WK-RETX TO WT-TX.
           CALL "CAUP" USING "0220100160" WH-CREG.
           MOVE WK-RETX TO WT-TX.
       Z.  EXIT.
      ******************************************************************
       DUPLO SECTION.
       A.  IF WM-DRU = 0 GO Z.
           IF WF-OPEN = 0 GO Z.
           CLOSE ABLAGE.
           CLOSE DRUCKER.
           MOVE "KOPIE.LST" TO WH-DRUNAM.
           OPEN EXTEND DRUCKER.
           OPEN INPUT ABLAGE WITH LOCK.
           IF WF-STATUS = "35" GO Z.
           CALL "CAUP" USING "0707100342000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bitte warten! " with blink AT VDU-LP
               "Kopien werden erstellt!" with highlight.
           ADD 2 TO WM-DRU.
      *    MOVE 0 TO WZ-SCHALT.
      *    MOVE "&l26a4H" TO DRA-SATZ.                       *> Fach 2
      *    PERFORM DRUCK.
       C.  READ ABLAGE AT END GO Q.
           MOVE ABA-REST TO DRA-SATZ.
           IF ABA-SCHALT = 99 WRITE DRA-SATZ AFTER PAGE GO C.
           MOVE ABA-SCHALT TO WZ-SCHALT.
           PERFORM DRUCK.
           GO C.
       Q.  CLOSE ABLAGE.
           MOVE 0 TO WF-OPEN.
           DELETE FILE ABLAGE.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       fakorr-n section.
       a.  move low-values to fa-satz.
           move 0 to de-ktonr.
           start faktdat key not < fa-key invalid go z.
       c.  read faktdat next at end go z.
           if de-ktonr = fa-ktonr go g.
           move fa-ktonr to de-ktonr.
           read debitor ignore lock invalid go c.
       g.  move de-rast to fa-art.
           rewrite fa-satz.
           go c.
       z.  exit.
      ******************************************* Rechnungsaufstellung *
       RE-AUFST SECTION.
       A.  CALL "CAUP" USING "1304012580" WH-CREG.
           OPEN I-O FAKTEDI.
           CALL "CAUP" USING "271010066000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Rechnungsaufstellung " with highlight AT VDU-LP.
           MOVE 9 TO WH-P.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kunden-Nr.: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Kto.-Nr., alpha+<ret>= suchen
      -        " " AT 2301.
           CALL "CAUP" USING "1003155010" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WH-NUM = 0 and WH-MCODE not = SPACE
               CALL "PANANZ" USING "10DEB-SUCH" WH-CREG
               IF DE-KTONR = 0 GO C.
           IF WH-NUM = 0 GO C.
           COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           ADD WH-NUM 0,2 GIVING WD-KTO.
           MOVE 1 TO DE-FNR.
           IF DE-KTONR < 3 GO C.
           READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO C.
           DISPLAY WD-KTO AT VDU-LP
           ADD 0403 VDU-ECK GIVING VDU-LP.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WT-TX(1:56) with highlight AT VDU-LP.
           INITIALIZE EF-SATZ.
           MOVE DE-KTONR TO EF-KUNDE.
           START FAKTEDI KEY > EF-KEY INVALID PERFORM NO-REC GO C.
       E.  READ FAKTEDI NEXT AT END PERFORM NO-REC GO C.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF EF-KUNDE not = DE-KTONR PERFORM NO-REC GO C.
           PERFORM ZAHL-AUF.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE FAKTEDI.
       Z.  EXIT.
      ********************************************** Sammelaufstellung *
       ZAHL-AUF SECTION.
       A.  MOVE DE-KTONR TO WV-KTONR.
           MOVE 0 TO WT-BASIS(1) WT-BASIS(2) WT-BASIS(3) WT-BASIS(4)
                WT-BASIS(5) WT-BASIS(6) WH-NUM WH-MWST WS-BET WS-RET
                WS-SUM(1) WS-SUM(2) WS-SUM(3) WS-SUM(4) WS-SUM(8)
                WS-BONUS WH-STAT WH-UST WS-UST WE-NTG WH-BET.
           MOVE "ZAHLAUF.LST" TO WH-DRUNAM.
           MOVE "Julius Kern GesmbH#BÑckerei-Kontidorei-Cafe##Mariatrost
      -        "erstra·e 93#8043 Graz" TO WK-ADR.
           MOVE "ATU28821406" TO WK-UID.                  *> eigene UID
           MOVE 9005793000009 TO WK-ILN.                  *> eigene GLN
           MOVE WV-KTONR TO DE-KTONR.
           READ DEBITOR INVALID STOP RUN.
           MOVE 0 TO WH-RENUM.
           GO G.                               *> 1. Satz schon gelesen
       E.  READ FAKTEDI NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WV-KTONR not = EF-KUNDE GO Q.
       G.  IF EF-REDAT > WH-BISDAT or EF-REDAT < WH-ABDAT GO E.
           PERFORM ZAHL-KOPF.
           IF WZ-ZEILEN > 38 MOVE ALL "ƒ" TO DRQ-STR(63:)
               PERFORM DRUCK
               MOVE "öbertrag" TO DRQ-STR(63:)
               MOVE WH-BET TO DRQ-WERT
               MOVE WS-UST TO DRQ-UST
               MOVE WS-BET TO DRQ-BETRAG
               PERFORM DRUCK
               WRITE DRA-SATZ BEFORE PAGE
               MOVE 0 TO WZ-ZEILEN
               PERFORM ZAHL-KOPF
               MOVE "öbertrag" TO DRQ-STR(63:)
               MOVE WH-BET TO DRQ-WERT
               MOVE WS-UST TO DRQ-UST
               MOVE WS-BET TO DRQ-BETRAG
               PERFORM DRUCK.
           MOVE EF-TEXT TO DRQ-NAME.
           MOVE EF-GLN TO DRQ-ILN.
           MOVE EF-RENUM TO DRQ-RENR.
           MOVE EF-UST TO DRQ-UST.
           MOVE EF-NETTO TO DRQ-WERT.
           MOVE EF-GESAMT TO DRQ-BETRAG.
           IF EF-EMBALL not = 0 MOVE EF-EMBALL TO DRQ-EMBALL.
           MOVE EF-REDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VD-DATUM TO DRQ-DATUM.
           PERFORM DRUCK.
           ADD EF-NETTO TO WH-BET.
           ADD EF-EMBALL TO WS-RET.
           ADD EF-UST TO WS-UST.
           ADD EF-GESAMT TO WS-BET.
           ADD EF-GESAMT TO WS-BONUS.
           GO E.
       Q.  IF WM-OPEN = 0 GO X.
           MOVE ALL "ƒ" TO DRQ-STR.
           PERFORM DRUCK.
           MOVE "Summen" TO DRQ-STR(63:).
           MOVE WH-BET TO DRQ-WERT.
           MOVE WS-UST TO DRQ-UST.
           MOVE WS-BET TO DRQ-BETRAG.
           IF WS-RET not = 0 MOVE WS-RET TO DRQ-EMBALL.
           PERFORM DRUCK.
           MOVE ALL "=" TO DRQ-STR(63:).
       S.  PERFORM DRUCK.
           PERFORM END-DRU.
           CALL "CBL_COPY_FILE" USING "ZAHLAUF.LST " "ABLAGE.LST ".
       X.  DELETE FILE ABLAGE.
           MOVE "ABLAGE.LST" TO WN-FORM.
       Z.  EXIT.
      ****************************************************** Kopfdruck *
       ZAHL-KOPF SECTION.
       A.  IF WM-OPEN = 0 MOVE WE-STG(2) TO WE-STG(9)
      *        MOVE "&l5H" TO WE-STG(9)(35:).
               MOVE "ZAHLAUF.XXX" TO WN-FORM
               PERFORM BEG-DRU.
           IF WZ-ZEILEN > 0 GO Z.
           ADD 1 TO WZ-SEITE.
           MOVE 0 TO WZ-SCHALT.
           MOVE "ILN: xxxxxxxxxxxxx      Uid-Nr.: xxxxxxxxxxx"
                TO DRA-SATZ(60:).
           MOVE WK-ILN TO DRA-SATZ(65:14).
           MOVE WK-UID TO DRA-SATZ(93:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           MOVE WK-ADR TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 5
               MOVE "(s1p13v1s3b4102T" TO DRA-SATZ(10:)
               MOVE WR-ADR(WZ) TO DRA-SATZ(27:)
               MOVE "(s0p0s0b4101T" TO DRA-SATZ(62:)
               PERFORM DRUCK.
           PERFORM DRUCK.
           MOVE "          Datum      Seite" TO DRA-SATZ(90:).
           PERFORM DRUCK.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VD-DATUM TO DRA-SATZ(100:).
           MOVE WZ-SEITE TO WD-KTO.
           MOVE WD-KTO(1:6) TO DRA-SATZ(110:).
           MOVE "      " TO DRA-SATZ(10:7).
           PERFORM DRUCK.
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 5
               IF WR-ADR(WZ) not = SPACE
                   MOVE "(s1p11v0s3b4101T" TO DRA-SATZ(10:)
                   MOVE "(s0p0s0b4101T" TO DRA-SATZ(60:)
                   MOVE WR-ADR(WZ) TO DRA-SATZ(27:30)
                   PERFORM DRUCK
                   IF WZ = 5 MOVE "ILN:                    Uid-Nr.:"
                           TO DRA-SATZ(60:)
                       MOVE DE-GLN TO DRA-SATZ(65:15)
                       MOVE DE-UID TO DRA-SATZ(93:)
                       MOVE 0 TO WZ-SCHALT
                       PERFORM DRUCK
                   end-if
                   IF WZ = 1 MOVE 0 TO WZ-SCHALT
                       MOVE "(s1p13v0s3b4101TSAMMELAUFSTELLUNG(s0p0s0b
      -                    "4101T" TO DRA-SATZ(60:)
                       PERFORM DRUCK.
           PERFORM DRUCK.
           MOVE"(s1p10v1s3b4102T  Rechnungsdatum = Ausstellungsdatum
      -      "(s0p0s0b4101T" TO DRA-SATZ(59:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRQ-STR.
           PERFORM DRUCK.
           MOVE "WarenempfÑnger Name u. Anschrift
      -      "  ILN  Re.-Nr   -Datum   Warenwert     Skonto     Leergut
      -      "Ums.Steuer Endbetrag" TO DRQ-STR.
           MOVE " " TO DRQ-STR(94:7).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRQ-STR.
           PERFORM DRUCK.
       Z.  EXIT.
