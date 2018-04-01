      $SET LINKCOUNT "384" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PANDES.
      ************************************ Elektronischer Lieferschein *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER.    PC.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEART.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSEKON.CPY.
           COPY PANSELFS.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANLIEF.CPY.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WN-SEDAS                    PIC X(12)   VALUE "TIDESADV.XX".
       01  WN-REDAS                    PIC X(12)   VALUE "TIREDAS.XX".
       01  WN-SICH                     PIC X(25).
       01  WS-REG.
      *-------------------------------------------- Byte-Stream-Files <-
           03  WB-SATZ                 PIC X(160).
           03  WB-AC                   PIC X     COMP-X.
           03  WB-DE                   PIC X     COMP-X.
           03  WB-DV                   PIC X     COMP-X.
           03  WB-FH                   PIC X(4).
           03  WB-OFS                  PIC X(8)  COMP-x.
           03  WB-CNT                  PIC X(4)  COMP-x.
           03  WB-FLG                  PIC X     COMP-x.
      *-----------------------------------------------------> edifact <-
           03  WT-EDIS.
               05 WR-ED                         OCCURS 1000.
                  07 WE-ARTNR          PIC 9(6)         COMP.
                  07 WE-MENGE          PIC 9(5)V99      COMP.
                  07 WE-BETRAG         PIC 9(6)         COMP.
                  07 WE-UID            PIC XX.
           03  WN-EDICUS               PIC X(24).
           03  WH-TIME.
               05  WH-HHMM             PIC 9(4)      VALUE 0.
               05  WH-SSHH             PIC 9(4)      VALUE 0.
           03  WK-NOMCODE              PIC 9(8).
           03  WM-CD                   PIC X(35).
           03  WH-CST                  PIC 99999.     *> CST+ ZÑhler
           03  WH-UNH                  PIC 9999.      *> UNH+ ZÑhler
           03  WH-SEG                  PIC 9999.      *> Pos. im LIN+
           03  WH-LIN                  PIC 9999.      *> LIN+ ZÑhler
           03  WH-KONTROL              PIC X(15).
           03  WK-UNZ                  PIC X(15).
           03  WH-KAT                  PIC 99        COMP.
           03  WM-UID                  PIC X(15).
           03  WE-ANZ                  PIC Z(6)9.
           03  WE-BET                  PIC Z(6)9,99.
           03  WS-MOA                  PIC 9(7).
           03  WA-TAGE                 PIC 9(9)      COMP.
           03  WD-CHARGE               PIC 9(6).
           03  WH-PARA.
               05 WH-PX                PIC XX      OCCURS 3.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WM-MANADR               PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WM-OPEN                 PIC 99        COMP.
           03  WA-OPEN                 PIC 99        COMP.
           03  WE-OPEN                 PIC 99        COMP.
           03  WM-KOP                  PIC 99        COMP.
           03  WM-REDAS                PIC 99        COMP.
           03  WC-POS                  PIC 9(4)      COMP.
           03  WC-PAC                  PIC 9(4)      COMP.
           03  WM-BAN                  PIC 9(9)      COMP.
           03  WL-KUSATZ               PIC X(251).
           03  WM-EMB                  PIC S9999     COMP.
           03  WM-RET                  PIC S9999     COMP.
           03  WM-FIL                  PIC 999.
           03  WH-DRUNAM.
               05  WH-FAKT1            PIC X(4).
               05  WH-FAKT2            PIC 99.
               05  WH-FAKT3            PIC 99.              *> Sare-Nr.
               05  WH-FAKT4            PIC X(4).
           03  WH-ABLNAM               PIC X(12).
           03  WH-RAB                  PIC S9(9)V99 COMP-3 VALUE 0.
           03  WH-NETTO                PIC S9(9)V99 COMP-3 VALUE 0.
           03  WV-FK                   PIC X(4).
           03  WV-KUNDE                PIC 9(6)     COMP.
           03  WM-KUNDE                PIC 9(6)     COMP.
           03  IX                      PIC 999      COMP.
           03  WI                      PIC 999      COMP   VALUE 0.
           03  WL                      PIC 999      COMP   VALUE 0.
           03  IQ                      PIC 99.
           03  WV-ART                  PIC 99       COMP.
           03  WD-ART                  PIC Z9-.
           03  WD-KTO                  PIC ZZ.ZZ9-.
           03  WD-RNR                  PIC ZZZ.ZZ9-.
           03  WH-DRUCK-KZ             PIC 99              VALUE 0.
           03  WH-DRUCK-KZ-R REDEFINES WH-DRUCK-KZ.
               05  FILLER              PIC 9.
               05  WH-DRUCK-FAK        PIC 9.
                   88  DRUCK-EAN-NR    VALUE 1 2.
                   88  DRUCK-INT-NR    VALUE 0 3 THRU 9.
           03  WH-EAN.
               05 WR-EAN               PIC 9        OCCURS 13.
           03  WK-EAN                  PIC 9(13).
           03  WK-BAN                  PIC 9(7).
           03  WT-EAN                  PIC 9(7).          *> Titznummer
           03  WH-REP                  PIC X(150).
           03  WH-REL                  PIC X(150).
           03  WH-REAN                 PIC 9(6).
           03  WM-EAN                  PIC 9(13).
           03  WH-BEDAT                PIC X(8).
           03  WH-BENR                 PIC X(16).
           03  WV-BESTNR               PIC X(16).
      *----------------------------------------> Nachkommadarstellung <-
           03  WH-ANZ                  PIC S9(8)V99.
           03  WH-ANZ1                 PIC S9(5)V9.
           03  WH-ANZ2                 PIC S9(4)V99.
           03  WD-SKTO                 PIC 9,9-.
           03  WD-ANZG                 PIC ZZZ.ZZ9-.
           03  WD-ANZ1                 PIC ZZZ.ZZ9,9-.
           03  WD-ANZ2                 PIC ZZZ.ZZ9,99-.
           03  WD-MENGE                PIC X(11).
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGB                  PIC ZZ.ZZ9,9-.
           03  WD-MGC                  PIC ZZ.ZZ9,99-.
           03  WD-MG                   PIC X(10).
           03  WD-REART                PIC 99       VALUE 0.
           03  WV-REART                PIC 99       VALUE 0 COMP.
                      88 EDIFA     VALUE 10 20 40 41.
           03  WM-REART                PIC 99       VALUE 0 COMP.
               88  EINZEL   VALUE  0 THRU  9.
               88  SAMMLER  VALUE 10 THRU 95.
               88  BARVKF   VALUE 97.       88  KORREKT  VALUE 96.
               88  EMBRE    VALUE 98.       88  ZENTRE   VALUE 99.
               88  BILLA    VALUE 20.       88  MONDO    VALUE 10.
               88  SPAR     VALUE 50 52 53 54 55 56 57 58 59
                                  70 72 73 74 75 76 77 78 79.
               88  LFSRE    VALUE 3.
           03  WM-ZENTRALE             PIC S9(6)     COMP.
           03  WD-LFSNR                PIC ZZ.ZZ9.
           03  WH-SAMNR                PIC 9(7)      COMP.
           03  WH-RENR                 PIC 9(7)      COMP.
           03  WV-RENR                 PIC 9(7)      COMP.
           03  WZ-SCHALT               PIC 99        COMP.
           03  WZ-SEITE                PIC 99        COMP.
           03  WZ-ZEILEN               PIC 99        COMP.
               88  UEBERTRAG       VALUE 57 THRU 99.
               88  E-UEBERTRAG     VALUE 55 THRU 99.
           03  WM-ALLE                 PIC S9        COMP.
           03  WS-BASIS                PIC S9(9)V99  COMP.
           03  WS-RAB                  PIC S9(9)V99  COMP.
           03  WS-UST                  PIC S9(9)V99  COMP.
           03  WH-UST                  PIC S9(9)V99  COMP.
           03  WS-LFS                  PIC S9(9)V99  COMP.
           03  WS-MENGE                PIC S9(9)V99  COMP.
           03  WS-BET                  PIC S9(9)V99  COMP.
           03  WS-NETTO                PIC S9(9)V99  COMP.
           03  WS-BRUTTO               PIC S9(9)V99  COMP.
           03  WS-ABZUG                PIC S9(7)V99  COMP.
           03  WN-PREIS                PIC S9(7)V99  COMP.
           03  WH-EMB                  PIC S9999     COMP.
           03  WS-EMB                  PIC S9999     COMP.
           03  WS-RET                  PIC S9999     COMP.
           03  WM-EMB-ALT              PIC S9999     COMP.
           03  WM-EMB-NEU              PIC S9999     COMP.
           03  WM-LIEFDAT              PIC 9(8)      COMP.
           03  WM-VERS                 PIC X(10)   OCCURS 10.
           03  WD-NUM                  PIC 9(7).
           03  WH-LKNUM                PIC 9(7)     COMP.
      *------------------------------------------------> Command-Line <-
           03  WD-UPON                 PIC X(60).
           03  RESULT                  PIC 99      COMP-X.
           03  FUNKT                   PIC 99      COMP-X  VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99      COMP-X  VALUE 0.
               05 PAR                  PIC X(40)   VALUE SPACE.
      *--------------------------------------------------> file exist <-
           03  WH-FDET.
               05 WF-SIZE              PIC X(8)    COMP-X.
               05 WF-TT                PIC 99      COMP-X.
               05 WF-MO                PIC 99      COMP-X.
               05 WF-YY                PIC 9999    COMP-X.
               05 WF-HH                PIC 99      COMP-X.
               05 WF-MM                PIC 99      COMP-X.
               05 WF-SS                PIC 99      COMP-X.
               05 WF-HS                PIC 99      COMP-X.
           03  WN-BEST                 PIC X(45).
           03  WN-ACOP                 PIC X(8).
           03  WN-BCOP                 PIC X(8).
           03  WD-Z                    PIC 99.
           03  ZA                      PIC 9999    COMP.
           03  DT-DATE                 PIC 9(8)    COMP.
           03  VDU-ZEIT.
               05 VDU-STD              PIC Z9.
               05 VDU-DD               PIC X.
               05 VDU-MIN              PIC 99.
           03  WD-SIZE                 PIC ZZ.ZZZ.ZZ9.
           03  WV-SIZE                 PIC X(8)    COMP-X.
           03  WT-TAB.
               05 WT-RED               PIC X(15)   OCCURS 20.
      *-----------------------------------------> Konditionentabelle <-
      *    03  WH-KK                   PIC 9(13).
      *    03  WR-KK REDEFINES WH-KK.
      *        05  WH-SK1              PIC 9V9.
      *        05  WH-TG1              PIC 999.
      *        05  WH-SK2              PIC 9V9.
      *        05  WH-TG2              PIC 999.
      *        05  WH-TGN              PIC 999.
      *---------------------------------------------> Dateien lîschen <-
           03  WM-PATH                 PIC X(50).
           03  WF-RESU.
               05 WF-ERR               PIC X       COMP-X.
               05 WF-HANDL             PIC XX      COMP-X.
               05 WF-ATT               PIC X       COMP-X.
               05 WF-TIME              PIC XX      COMP-X.
               05 WF-DATE              PIC XX      COMP-X.
               05 WF-SIZES             PIC 9(8)    COMP.
               05 WF-OUT               PIC X(14).
           03  WF-CODE                 PIC X       COMP-X VALUE 69.
           03  WF-PARA.
               05 WF-ACT               PIC X       COMP-X.
               05 WF-ATTR              PIC X       COMP-X VALUE 39.
               05 WF-EIN               PIC X(30).
       COPY PANEXT.CPY.
      ******************************************************************
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
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE " DESADV - Lieferschein" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE 9 TO WH-PG.
           MOVE "    DESADV     " TO WK-GEB.
           MOVE 67 TO WL-CA.
           PERFORM DES-INV.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *********************************************** DESADV / INVOICE *
       DES-INV SECTION.
       A.  CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE 0 TO WM-ALLE WM-OPEN WA-OPEN WM-REDAS WC-POS.
      *    MOVE WK-BS TO WN-SPAR(7:2) WN-SAM(7:2).
           CLOSE DEBITOR.
           OPEN I-O FAKTDAT DEBITOR.
           CALL "CAUP" USING "07101005600000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Desadv-Kunden " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto-Nr.: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Kto.Nr., alpha+<ret>= suchen"
               AT 2301.
           CALL "CAUP" USING "1003126006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO C.
           IF WH-NUM = 0 CALL "PANANZ" USING "10DEBSUCH" WH-CREG
               CANCEL "PANANZ".
           IF WH-NUM = 0 GO C.
           MULTIPLY WH-WERT BY 10 GIVING WH-NUM.
           ADD 2 WH-NUM GIVING DE-KTONR.
       E.  READ DEBITOR IGNORE LOCK INVALID PERFORM NO-REC GO C.
           MOVE DE-KTONR TO WD-KTO.
           DISPLAY WD-KTO AT VDU-LP " " DE-BEZ(1:45).
           IF DE-KTONR not = DE-REKTO or DE-GLN = 0
               DISPLAY "kein DESADV-Kunde!" AT 2401
               PERFORM WEITER
               GO C.
           MOVE DE-REKTO TO WV-KUNDE.                   *> Zentralkonto
           MOVE DE-KTONR TO WM-KUNDE.
       G.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lieferdatum:" AT VDU-LP.

           ACCEPT WS-DATUM FROM DATE.
           ADD 20000000 TO WS-DATUM.
           MOVE WM-DATUM TO WC-DATUM WM-LIEFDAT.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight foreground-color 2 AT 0370.
           MOVE WM-LIEFDAT TO WZ-DATUM WH-WERT.
           DISPLAY "bis Lieferdatum" AT 2301.
           CALL "CAUP" USING "0103706006" WH-CREG.
           IF WOLI GO E.
           IF ESC GO Z.
           IF not RET and not EINF GO G.
           IF WX-DATUM = 0
               DISPLAY "ungÅltiges Datum!" AT 2401 GO G.
           MOVE 0 TO WH-MOD.
           IF EINF MOVE 1 TO WH-MOD GO W.                *> wiederholen
      *    IF WV-REART not = 0 and WX-DATUM not > WS-DATUM
      *         DISPLAY "Datum zu klein" AT 2401 GO G.
       W.  MOVE WX-DATUM TO WM-LIEFDAT.
           DISPLAY VDU-DATUM with highlight foreground-color 2 AT 0370.


           MOVE 0 TO DE-KTONR.
           START DEBITOR KEY = DE-EANKEY INVALID GO C.
       G.  READ DEBITOR NEXT AT END GO X.
           IF DE-KTONR = WM-KUNDE GO G.
           IF DE-REKTO not = WV-KUNDE GO X.

           MOVE 1 TO WM-KOP.
       G.  PERFORM VORLAUF.
           IF ESC OR ENDE GO Y.
           DISPLAY "Kd.Nr.:" AT 0301.
           IF WL-CA = 49 MOVE LK-KUNDE TO KU-KTONR WV-KUNDE GO J.
           MOVE 0 TO WH-WERT WH-UNH.
      *---------------------------------------------------------------
           IF SAMMLER PERFORM ZENTRALKTO
           else MOVE 0 TO WH-WERT WH-NUM.
           PERFORM ALTDESAD.
           IF ESC GO Y.
       I.  DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret-leer>= alle, alpha
      -        "+<ret>= Kunden suchen" AT 2301.
           IF WM-REART > 9 DISPLAY "Zentralkunden-Nr." with highlight
                AT 2401.
           CALL "CAUP" USING "0003095009" WH-CREG.
           IF HELP CALL "CAUP" USING "190351035201" WH-CREG GO I.
           IF ENDE OR ESC GO Y.
           IF WOLI GO G.
           IF not RET GO I.
           IF WH-NUM = 0 and WV-MCODE not = SPACE
               CALL "TIANZEIG" USING "01KDSUCH" WH-CREG
               CANCEL "TIANZEIG"
               IF WH-NUM = 0 GO I.
           MOVE WH-WERT TO KU-KTONR WV-KUNDE WM-KUNDE.
           IF SAMMLER MOVE KU-KTONR TO WM-ZENTRALE
               DISPLAY " Sammler " with highlight background-color 7
                   foreground-color 2 AT 0321.
       J.  MOVE "K" TO KU-SA.
           MOVE 0 TO WM-ALLE.
           IF KU-KTONR = 0 MOVE 2 TO WM-ALLE
               DISPLAY " alle " with highlight background-color 7
                   foreground-color 2 AT 0323 GO M.
       K.  READ DEBITOR INVALID PERFORM NO-REC GO I.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE KU-KTONR TO WD-KTO.
           DISPLAY WD-KTO with highlight AT 0309.
           MOVE KU-BEZ TO WT-BEZ.
           MOVE SPACE TO ZA-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ(1:80) with highlight AT 0401.
           IF KU-KTONR not = 0 and not SAMMLER
               CALL "CAUP" USING "1303250330" WH-CREG.
           MOVE WV-REART TO WM-REART.
           IF WM-REART = 0 MOVE KU-REART TO WM-REART.
           IF WL-CA = 49; IF LK-NUM > 99000 SET KORREKT TO TRUE
               DISPLAY " Korrekt. " with reverse-video AT 0321 GO M
           else DISPLAY " S-Rechg. " with reverse-video AT 0321 GO M.
           IF SAMMLER MOVE 2 TO WM-ALLE GO M.
      *-----------------> nur bei normalen Rechnungen nicht bei 10/20 <-
           IF WM-REART not = KU-REART
               DISPLAY "falsche Rechnungsasrt" AT 2401
               PERFORM WEITER GO I.
       M.  DISPLAY "<esc>= Abbruch, <ret>= Start, <>= zurÅck < >"
               AT 2301.
           CALL "CAUP" USING "0023440000" WH-CREG.
           IF HELP CALL "CAUP" USING "190353035401" WH-CREG GO M.
           IF WOLI; IF WL-CA = 49 SET ESC TO TRUE
                    else GO I.
           IF ESC GO Y.
           IF not RET GO M.
           CALL "CAUP" USING "1305012480" WH-CREG.
           DISPLAY "Artnr Bezeichnung               Anz Vp      Menge
      -        "    Preis           Betrag " with reverse-video at 0501.
           IF WM-REART > 0 PERFORM EINZ-RE.

       Y.  CLOSE LBEWEG LKOPF DEBITOR.
           OPEN I-O DEBITOR.
           IF WL-CA = 49 MOVE "TILFS   20LFS"  TO WT-TX.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT.
      ***************************************** abfragen ob alte DESAD *
       ALTDESAD SECTION.
       A.  IF EDIFA OPEN INPUT DESADV
                    IF WF-STATUS = "35" GO Z
                    else CLOSE DESADV
                         GO B.
           OPEN INPUT DESADV.
           IF WF-STATUS = "35" GO Z.
           CLOSE DESADV.
       B.  CALL "CAUP" USING "0707100660000" WH-CREG.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Alte Datei vorhanden!" with highlight AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<Entf>= lîschen, <esc>= Abbruch < >" AT VDU-LP.
           CALL "CAUP" USING "1004360000" WH-CREG.
           IF ESC GO X.
           IF not ENTF GO C.
           DELETE FILE DESADV.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  CALL "CAUP" USING "04DATDREH" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       MG SECTION.
       A.  EVALUATE AR-NK
               WHEN  0
               WHEN 10 MOVE WH-ANZ TO WD-ANZG WD-MGA
                       MOVE WD-ANZG TO WD-MENGE
                       MOVE WD-MGA TO WD-MG
               WHEN  1
               WHEN 11 MOVE WH-ANZ TO WD-ANZ1 WD-MGB
                       MOVE WD-ANZ1 TO WD-MENGE
                       MOVE WD-MGB  TO WD-MG
               WHEN  2
               WHEN 12 MOVE WH-ANZ TO WD-ANZ2 WD-MGC
                       MOVE WD-ANZ2 TO WD-MENGE
                       MOVE WD-MGC  TO WD-MG.
           MOVE WH-ANZ TO WH-WERT.
       Z.  EXIT.
      ******************************************************************
       VORLAUF SECTION.
       A.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Art" AT 2301.
           DISPLAY "0 = Einzelkunden" AT 2401.
           MOVE 0 TO WH-WERT WD-ART.
           DISPLAY WD-ART with highlight AT 0350.
           CALL "CAUP" USING "0003502002" WH-CREG.
           IF HELP CALL "CAUP" USING "190355035601" WH-CREG GO E.
           IF ESC GO Z.
           IF not RET GO E.
           EVALUATE WH-WERT
               WHEN 25 NEXT SENTENCE
               WHEN 26 NEXT SENTENCE
               WHEN 70 THRU 80 NEXT SENTENCE
               WHEN OTHER GO E.
      *    IF WH-WERT < 70 GO E.
           MOVE WH-WERT TO WD-ART WV-REART WM-REART WD-REART.
           IF WV-REART = 0
               DISPLAY "E" with highlight foreground-color 5 AT VDU-LP
           else
             DISPLAY WD-ART with highlight foreground-color 3 AT VDU-LP.
       G.  DISPLAY "  Lieferdatum:" AT 0355.
           ACCEPT WS-DATUM FROM DATE.
           ADD 20000000 TO WS-DATUM.
           MOVE WM-DATUM TO WC-DATUM WM-LIEFDAT.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight foreground-color 2 AT 0370.
           MOVE WM-LIEFDAT TO WZ-DATUM WH-WERT.
           DISPLAY "bis Lieferdatum" AT 2301.
           CALL "CAUP" USING "0103706006" WH-CREG.
           IF WOLI GO E.
           IF ESC GO Z.
           IF not RET and not EINF GO G.
           IF WX-DATUM = 0
               DISPLAY "ungÅltiges Datum!" AT 2401 GO G.
           MOVE 0 TO WH-MOD.
           IF EINF MOVE 1 TO WH-MOD GO W.                *> wiederholen
      *    IF WV-REART not = 0 and WX-DATUM not > WS-DATUM
      *         DISPLAY "Datum zu klein" AT 2401 GO G.
       W.  MOVE WX-DATUM TO WM-LIEFDAT.
           DISPLAY VDU-DATUM with highlight foreground-color 2 AT 0370.
           MOVE -1 TO WM-ZENTRALE.
       Z.  EXIT.
      ********************************************* Zentralkonto lesen *
       ZENTRALKTO SECTION.
       A.  MOVE WM-REART TO KU-REART.
           MOVE 0 TO KU-FOLGE WV-KUNDE LK-KUNDE KU-KTONR.
      *---------------------------------> Kunden je Fakturenart lesen <-
           START DEBITOR KEY not < KU-FKEY INVALID GO Z.
       C.  READ DEBITOR NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF KU-REART not = WM-REART GO Z.
           IF KU-FIBU = 0 GO C.
           IF KU-BAN > 9999999 MOVE KU-BAN TO WK-EAN
                               MOVE KU-RAB TO WH-RAB
                               GO D.
       D.  MOVE KU-KTONR TO WD-KTO WV-KUNDE LK-KUNDE WH-WERT.
           MOVE KU-FIBU TO KU-KTONR.
           DISPLAY WD-KTO with highlight AT 0309.
           MOVE KU-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ(1:80) with highlight AT 0401.
       E.  READ DEBITOR INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE WD-REART TO WN-SEDAS(10:2) WN-REDAS(9:2).
           MOVE "SARE" TO WH-DRUNAM.
           MOVE WK-BS TO WH-FAKT3.
           MOVE WD-REART TO WH-FAKT2.
           MOVE ".LST" TO WH-FAKT4.
           MOVE 2 TO WM-KOP.
           IF WM-REART = 24 or 25 or 26 or 41
               MOVE "FAKTxx.LST" TO WH-DRUNAM
               MOVE WK-BS TO WH-FAKT2
               MOVE 1 TO WM-KOP.
       Z.  EXIT.
      ********************************************** PrÅfziffer rechen *
       PRFZ SECTION.
       A.  COMPUTE WR-EAN(13) = (WR-EAN(2) + WR-EAN(4) + WR-EAN(6)
                      + WR-EAN(8) + WR-EAN(10) + WR-EAN(12)) * 3
                      + (WR-EAN(1) + WR-EAN(3) + WR-EAN(5)
                      + WR-EAN(7) + WR-EAN(9) + WR-EAN(11)).
           COMPUTE WR-EAN(13) = 10 - WR-EAN(13).
       Z.  EXIT.
      ************************************************* Einzelrechnung *
       EINZ-RE SECTION.
       A.  IF WV-REART = 0 GO I.
           MOVE 0 TO KU-FOLGE WV-RENR WM-MANADR.
           MOVE WV-REART TO KU-REART.
           MOVE WV-KUNDE TO KU-KTONR.
       C.  START DEBITOR KEY not < KU-FKEY INVALID GO Z.
           GO G.
      *------------------------> nÑchsten Kunden je Fakturenart lesen <-
       E.  MOVE WV-FK TO KU-FK.
           MOVE WV-KUNDE TO KU-KTONR.
           START DEBITOR KEY > KU-FKEY INVALID GO Z.
       G.  READ DEBITOR NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
       I.  IF not KORREKT IF KU-REART not = WM-REART GO X.
           IF SAMMLER and KU-KTONR = WM-ZENTRALE GO G.
      *    MOVE LOW-VALUE TO WH-EMBSATZ.
           MOVE KU-KTONR TO WD-KTO WV-KUNDE LK-KUNDE.
           MOVE KU-FK TO WV-FK.
           DISPLAY WD-KTO with highlight AT 0309.
           MOVE KU-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           DISPLAY WT-BEZ(1:80) with highlight AT 0401.
      *    MOVE WT-BEZ TO WM-LIEFADR WM-FIBTX.         *> Lieferadresse
           MOVE 0 TO WH-RENR WS-RET WS-EMB WS-ABZUG WS-BET WM-EMB
                     WS-LFS WM-RET WS-BRUTTO WS-NETTO WS-RAB WS-BASIS
                     WZ-ZEILEN WZ-SCHALT WZ-SEITE.
      *    INITIALIZE WF-SATZ.
      *-------------------------------------------> ob Sofortrechnung <-
           MOVE "O" TO LK-STATUS.
           MOVE WM-LIEFDAT TO LK-LIEFDAT.
      *    MOVE 0 TO LK-LIEFDAT.
      *    START LKOPF KEY not < LK-RKEY INVALID GO G.
           START LKOPF KEY = LK-RKEY INVALID GO G.
       M.  READ LKOPF NEXT  AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO M.
           IF LK-STATUS not = "O" GO M.
           IF LK-KUNDE not = KU-KTONR GO E.
210306     IF WV-REART = 26; IF WM-LIEFDAT = LK-LIEFDAT GO O
                             else GO M.
230106     IF WV-REART = 25 or WV-REART = 26;
               IF WM-LIEFDAT = LK-LIEFDAT GO O.
      *--------------------------------------> nur dieses Lieferdatum <-
           IF WH-MOD = 1                                 *> wiederholen
              IF LK-LIEFDAT = WM-LIEFDAT and LK-RENR  = 1234568 GO O.
           IF LK-LIEFDAT > WM-LIEFDAT or LK-RENR not = 1234567 GO M.
       O.  IF WV-KUNDE = 0 MOVE LK-KUNDE TO WV-KUNDE.
           IF WV-KUNDE = LK-KUNDE PERFORM FAKTZEIL.
           GO M.
       Q.  IF WH-RENR = 0 GO S.
           IF KU-SAMKZ = 1 PERFORM NEXT-KD
               IF KU-SAMKZ = 1 GO O
               else MOVE WL-KUSATZ TO KU-SATZ.
      *--------------------------------------------------> Kundenende <-
       S.  IF WM-ALLE = 2;
              IF SAMMLER and WM-REART > 89
      *           PERFORM SAMM-RE
                  MOVE 0 TO WV-RENR
                  MOVE WV-REART TO WM-REART GO E
              else GO E.
      *-------------------------------> je Lieferschein eine Rechnung <-
           IF WM-ALLE not = 3 GO Z.
           MOVE WV-KUNDE TO KU-KTONR.
       T.  READ DEBITOR INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO T.
           IF WV-KUNDE = LK-KUNDE GO I.
           IF WM-KUNDE = 0 GO E.
           GO Z.
       X.  IF WA-OPEN = 1 CLOSE DESADV.
           IF WV-REART = 26 PERFORM EDIFUS.
       Z.  EXIT.
      ******************************************************************
       NEXT-KD SECTION.
       A.  MOVE LK-KUNDE TO KU-KTONR.
       C.  READ DEBITOR IGNORE LOCK INVALID MOVE 0 TO WV-KUNDE GO Z.
           IF KU-SAMKZ = 1 MOVE LK-KUNDE TO WV-KUNDE.
       Z.  EXIT.
      ************************************* automatische Fakturenzeile *
       FAKTZEIL SECTION.
       A.  ADD LK-RETOUR TO WS-RET.
           ADD LK-EMB-SUMME TO WS-EMB.
           MOVE LK-NUM TO LP-NUM.
           IF not SPAR MOVE LK-NUM TO WH-LKNUM
           else COMPUTE WH-LKNUM = LK-NUM * 10.
           MOVE ALL "9" TO WV-BESTNR.
           MOVE 0 TO LP-POS.
           MOVE SPACE TO LP-BESTNR.
           START LBEWEG KEY > LP-BKEY INVALID GO Z.
       C.  READ LBEWEG NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF LP-NUM not = LK-NUM GO Z.
           IF LP-KZ = 0 and LP-MENGE = 0 GO C.
       G.  IF WV-BESTNR(1:5) = 99999 MOVE LP-BESTNR TO WV-BESTNR.
           IF WV-BESTNR not = LP-BESTNR MOVE LP-BESTNR TO WV-BESTNR
                IF WV-REART not = 26 MOVE 1 TO WM-REDAS.
      *------------------------------------------------> Artikelzeile <-
           INITIALIZE AR-SATZ.
           MOVE "A" TO AR-ART.
           MOVE LP-ARTNR TO AR-NUM.
           READ ARTIKEL IGNORE LOCK INVALID UNLOCK ARTIKEL
               DISPLAY "nicht angelegt!" AT VDU-LP
               GO Z.
           MOVE 0 TO IQ.
           IF LP-ARTNR not = AR-NUM PERFORM READ-KALIB.
      *--------------------------------------------> IF DRUCK-EAN-NR: <-
           IF LP-FEAN not NUMERIC MOVE 0 TO LP-FEAN.
           IF LP-FEAN not = 0
               DIVIDE 10 INTO LP-FEAN GIVING WH-REAN
           else MOVE AR-EAN TO WH-REAN.
      *--------------------------------------------> IF DRUCK-INT-NR: <-
           IF WH-REAN(1:) = SPACE OR WH-REAN(1:1) = 0
               MOVE AR-NUM TO WH-REAN.
           MOVE LP-MENGE TO WH-ANZ.
           PERFORM MG.
      *    PERFORM RECH-UST.
       H.  PERFORM DESAZEIL.
           IF LP-NUM = 0 GO P.
       I.  READ LBEWEG NEXT AT END GO P
           IF ZUGRIF PERFORM BESETZT GO I.
           INITIALIZE AR-SATZ.
           IF LP-NUM not = LK-NUM GO P.
           IF SPAR; IF LP-BESTNR not = WV-BESTNR MOVE 1 TO WM-REDAS
                       ADD 1 TO WH-LKNUM.
           IF LP-KZ = 1 GO G.
           IF LP-KZ = 0 and LP-MENGE not = 0 GO G.
           GO I.
      *-----------------------------------> Ende eines Lieferscheines <-
       P.  IF WV-REART = 26 PERFORM DESA-UNT.
           MOVE WM-LIEFDAT TO LK-REDAT.
           MOVE 1234568 TO LK-RENR.                  *> DESADV erledigt
           REWRITE LK-SATZ.
           MOVE 1 TO WM-REDAS.
       Z.  EXIT.
      ******************************************************************
       READ-KALIB SECTION.
       A.  MOVE AR-NUM TO KA-ARNUM.
           COMPUTE WH-WERT = LP-ARTNR - AR-NUM.
           COMPUTE IQ = WH-WERT * 100.
           READ KALIBER IGNORE LOCK INVALID MOVE 0 TO IQ.
           MOVE KA-BNUM(IQ) TO AR-NUM.
           MOVE KA-ZUSBEZ(IQ) TO AR-ZUSBEZ.
       Z.  EXIT.
      ***************************** OPEN DESADV und UNB-Satz schreiben *
       OPEN-DESADV SECTION.
       A.  DIVIDE 10 INTO KO-EANNR GIVING WT-EAN.
      *------------------------------------------> edifact- UNB -Satz <-
           OPEN EXTEND DESADV.
           MOVE 1 TO WA-OPEN.
           INITIALIZE DV-UNB.
           MOVE "UNB" TO DV-SA.
           MOVE "9001456000003" TO DV-MBSEND.
           MOVE WK-EAN TO DV-MBEMPF(1:).
           MOVE SPACE TO DV-RADR.
           MOVE "TILFS" TO DV-REFF.
           MOVE WM-LIEFDAT TO DV-REFF(6:).
           MOVE "0" TO DV-INDIC.
           WRITE DV-UNB.
           MOVE 0 TO WC-POS.
           MOVE 2 TO WC-PAC.
           IF WV-REART not = 26 GO Z.
      *------------------------> Ausgabe der 'fertigen' EDIFACT-Datei <-
       C.  MOVE 0 TO WH-UNH.
           MOVE WN-SEDAS TO WN-EDICUS.
           MOVE "1" TO WN-EDICUS(12:1).
      *-------------------------------------> lîschen und create file <-
           CALL "CBL_DELETE_FILE" USING WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS WH-SEG.
           MOVE 1 TO WE-OPEN.                         *> Datei geîffnet
           MOVE "UNA:+,? '" TO WB-SATZ.
           MOVE 9 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE "UNB+UNOA:3+" TO WB-SATZ.
           MOVE "9001456000003:14+" TO WM-CD.              *> Versender
           PERFORM ANHANG.
           MOVE WK-EAN TO WM-CD.
           PERFORM ANHANG.
           MOVE ":14+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WM-LIEFDAT TO WC-DATUM.
           MOVE WC-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           ACCEPT WH-TIME FROM TIME.
           MOVE WH-HHMM TO WM-CD(1:4).
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE "TILFS" TO WH-KONTROL
           MOVE WM-LIEFDAT TO WH-KONTROL(6:).
           MOVE WH-KONTROL TO WM-CD.
           PERFORM ANHANG
           MOVE "++DESADV++++0" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
       Z.  EXIT.
      ******************************************* DESADV-Rechnungskopf *
       DESAKOPF SECTION.
       A.  INITIALIZE DV-HDR.
           MOVE "HDR" TO DV-HSA.
      *-------------------------------> HDR-Kopfsatz nur 1 x ausgeben <-
           MOVE WH-LKNUM TO WD-NUM.
           MOVE WD-NUM TO DV-MESREF DV-LFSNR DV-ILFSNR.
           MOVE WM-LIEFDAT TO DV-LFSDAT DV-LIEFDAT.
           MOVE LP-BESTNR TO DV-BESTNR.
           MOVE LP-BESDAT TO WC-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE WX-DATUM TO DV-BESTDAT.
           MOVE WK-EAN TO DV-BBN-BY.
           MOVE KU-NAD-DP TO DV-BBN-DP.
           IF KU-NAD-DP not = KU-BAN MOVE KU-BAN TO DV-BBN-UC.
           MOVE "9001456000003" TO DV-BBN-SU.
           WRITE DV-HDR.
           MOVE SPACE TO DV-HDR.
      *----------------------------------------------------> CPS-Satz <-
           INITIALIZE DV-CPS.
           MOVE "CPS1" TO DV-CSA.
           WRITE DV-CPS.
      *---------------------------------------> PAC - Ladehilfsmittel <-
           INITIALIZE DV-PCA.
           MOVE "PAC" TO DV-PSA.
           IF WV-REART not = 26 GO Z.
      *------------------------> Ausgabe der 'fertigen' EDIFACT-Datei <-
       G.  MOVE "UNH+" TO WB-SATZ.
           MOVE WH-LKNUM TO WD-NUM.
           MOVE WD-NUM TO WM-CD.
           PERFORM ANHANG.
           MOVE "+DESADV:D:96A:UN:EAN005" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "BGM+351+" TO WB-SATZ.
           MOVE WH-LKNUM TO WD-NUM.
           MOVE WD-NUM TO WM-CD.
           PERFORM ANHANG.
           MOVE "+9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "DTM+137:" TO WB-SATZ.                 *> Lfs.-Datum
           MOVE WM-LIEFDAT TO WM-CD.
           PERFORM ANHANG.
           MOVE ":102" TO WM-CD.                       *> fÅr JJJJMMTT
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "DTM+17:" TO WB-SATZ.                  *> Lieferdatum
           MOVE WM-LIEFDAT TO WM-CD.
           PERFORM ANHANG.
           MOVE ":102" TO WM-CD.                       *> fÅr JJJJMMTT
           PERFORM ANHANG.
           PERFORM ZEILEND.
           IF LP-BESTNR not = SPACE
               MOVE "RFF+ON:" TO WB-SATZ               *> Bestell-Nr.
               MOVE LP-BESTNR TO WM-CD
               PERFORM ANHANG
               PERFORM ZEILEND
               MOVE "DTM+171:" TO WB-SATZ              *> Bestelldatum
               MOVE LP-BESDAT TO WC-DATUM
               CALL "CAUP" USING "05DATPRF" WH-CREG
               MOVE WX-DATUM TO WM-CD
               PERFORM ANHANG
               MOVE ":102" TO WM-CD                    *> fÅr JJJJMMTT
               PERFORM ANHANG
               PERFORM ZEILEND.
           MOVE "RFF+DQ:" TO WB-SATZ.                  *> Lfschein-Nr.
           MOVE WH-LKNUM TO WD-NUM.
           MOVE WD-NUM TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+BY+" TO WB-SATZ.
           MOVE WK-EAN TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+SU+9001456000003::9" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "NAD+DP+" TO WB-SATZ.
           MOVE KU-NAD-DP TO WM-CD.
           PERFORM ANHANG.
           MOVE "::9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           IF KU-NAD-DP not = KU-BAN
               MOVE "NAD+UC+" TO WB-SATZ
               MOVE KU-NAD-DP TO WM-CD
               PERFORM ANHANG
               MOVE "::9" TO WM-CD
               PERFORM ANHANG
               PERFORM ZEILEND.
           MOVE "CPS+1" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE 0 TO WH-LIN.
       Z.  EXIT.
      **************************************** DESADV-Rechnungszeilen *
       DESAZEIL SECTION.
       A.  IF WM-REDAS = 0 PERFORM OPEN-DESADV
               MOVE 1 TO WM-REDAS.
           IF WM-REDAS not = 2 PERFORM DESAKOPF
               MOVE 2 TO WM-REDAS.
           INITIALIZE DV-LIN.
           MOVE "LIN" TO DV-LSA.
           ADD 1 TO WC-POS.
           MOVE WC-POS TO DV-LPOS.
           COMPUTE WH-NUM = 9001456000000 + AR-NUM * 10.
           MOVE WH-NUM TO WH-EAN(1:).
           PERFORM PRFZ.
           MOVE WH-EAN TO DV-LARTNR.
270106*--------------------------------> geÑndert wg. StÅckverrechung <-
           EVALUATE LP-STKVER
               WHEN 0 MOVE LP-MENGE TO DV-LMENGE
                      MOVE "KGM" TO DV-LMEH
                      MOVE LP-ANZEFF TO DV-LBMENGE
                      MOVE "PCE" TO DV-LBMEH
               WHEN 1 MOVE LP-MENGE TO DV-LBMENGE
                      MOVE "KGM" TO DV-LBMEH
                      MOVE LP-ANZEFF TO DV-LMENGE
                      MOVE "PCE" TO DV-LMEH.
           MOVE LP-BESTNR TO DV-LBESTNR.
      *------------------------------------------> Mindesthaltbarkeit <-
           MOVE WM-LIEFDAT TO WA-TAGE.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WA-TAGE).
           MOVE WA-TAGE TO WD-CHARGE.
           MOVE WD-CHARGE TO DV-LCHARGE.
           ADD 5 TO WA-TAGE.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           MOVE WV-DATUM TO DV-LHALTB.
           WRITE DV-LIN.
           IF WV-REART not = 26 GO Z.
      *------------------------> Ausgabe der 'fertigen' EDIFACT-Datei <-
           ADD 1 TO WH-LIN.
           MOVE "LIN+" TO WB-SATZ.
           MOVE WH-LIN TO WE-ANZ.                     *> Anz. LIN+ Pos.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "++" TO WM-CD.
           PERFORM ANHANG.
           COMPUTE WH-NUM = 9001456000000 + AR-NUM * 10.
           MOVE WH-NUM TO WH-EAN(1:).
           PERFORM PRFZ.
           MOVE WH-EAN TO WM-CD.
           PERFORM ANHANG.
           MOVE ":EN" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           EVALUATE LP-STKVER
               WHEN 0 MOVE LP-MENGE TO DV-LMENGE
                      MOVE "KGM" TO DV-LMEH
                      MOVE LP-ANZEFF TO DV-LBMENGE
                      MOVE "PCE" TO DV-LBMEH
               WHEN 1 MOVE LP-MENGE TO DV-LBMENGE
                      MOVE "KGM" TO DV-LBMEH
                      MOVE LP-ANZEFF TO DV-LMENGE
                      MOVE "PCE" TO DV-LMEH.
      *--------------------------------> gelieferte/verrechenb. Menge <-
           MOVE "QTY+12:" TO WB-SATZ.
           IF DV-LMEH = "PCE" MOVE DV-LMENGE TO WE-ANZ
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
                   IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:)
               end-perform
               MOVE WE-ANZ TO WM-CD
           else MOVE DV-LMENGE TO WE-BET
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
                   IF WE-BET(1:1) = " " MOVE WE-BET(2:) TO WE-BET(1:)
               end-perform
               MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE DV-LMEH TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "QTY+21:" TO WB-SATZ.
           IF DV-LBMEH = "PCE" MOVE DV-LBMENGE TO WE-ANZ
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
                   IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:)
               end-perform
               MOVE WE-ANZ TO WM-CD
           else MOVE DV-LBMENGE TO WE-BET
               PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
                   IF WE-BET(1:1) = " " MOVE WE-BET(2:) TO WE-BET(1:)
               end-perform
               MOVE WE-BET TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE DV-LBMEH TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           IF LP-BESTNR not = SPACE
               MOVE "RFF+ON:"TO WB-SATZ
               MOVE LP-BESTNR TO WM-CD
               PERFORM ANHANG
               PERFORM ZEILEND.
           MOVE "PCI+17" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "DTM+361:"TO WB-SATZ.                 *> Haltbarkeitsd.
           MOVE WM-LIEFDAT TO WA-TAGE.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WA-TAGE).
           MOVE WA-TAGE TO WD-CHARGE.
           ADD 5 TO WA-TAGE.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           MOVE WV-DATUM TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "GIN+BX+" TO WB-SATZ.                  *> Chargennummer
           MOVE WD-CHARGE TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
       Z.  EXIT.
      ****************************************** 07 Hogast = per Email *
       KOPIERE SECTION.
       A.  MOVE "COPY" TO WD-UPON.
           MOVE 6 TO IX.
           MOVE WT-RED(WX) TO WD-UPON(IX:).
           PERFORM VARYING IX FROM 60 BY -1 UNTIL
               WD-UPON(IX:1) not = SPACE CONTINUE.
      *    IF WT-RED(WX)(11:1) = "1" or WT-RED(WX)(9:2) = "07"
           IF WT-RED(WX)(11:1) = "1" GO Z.
           ADD 1 TO IX.
           IF WX > 1 GO C.
           ADD 1 TO IX.
           MOVE WX TO WD-Z.
           EVALUATE WH-PG
               WHEN 3 MOVE "DESADV.XX" TO WN-ACOP WN-BCOP
                      MOVE WD-Z TO WN-ACOP(8:2) WN-BCOP(8:2)
               WHEN 5 MOVE "REDAS.XX" TO WN-ACOP WN-BCOP
                      MOVE WD-Z TO WN-ACOP(7:2) WN-BCOP(7:2).
           MOVE WN-ACOP(1:) TO WD-UPON(IX:).
           GO G.
       C.  MOVE WX TO WD-Z.
           EVALUATE WH-PG
               WHEN 3 MOVE WD-Z TO WN-BCOP(8:2)
               WHEN 5 MOVE WD-Z TO WN-BCOP(7:2).
           MOVE "+" TO WD-UPON(IX:).
           ADD 1 TO IX.
           MOVE WN-ACOP(1:) TO WD-UPON(IX:).
           ADD 9 TO IX.
           MOVE WN-BCOP(1:) TO WD-UPON(IX:).
       G.  CALL "CAUP" USING "1318012580" WH-CREG.
           DISPLAY "Dateien kopieren - bitte warten!"
                with highlight foreground-color 6 BLINK AT 2421.
           DISPLAY LOW-VALUE AT 1801.
           DISPLAY WD-UPON UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.         *> command-line
           CALL "CAUP" USING "1318012580" WH-CREG.
           MOVE WN-ACOP TO WN-SEDAS.
           IF WX > 1 CALL "CBL_DELETE_FILE" USING WN-SEDAS.
           MOVE WN-BCOP TO WN-SEDAS WN-ACOP.
       Z.  EXIT.
      ************************************* DESADV und INVOICE lîschen *
       LOE-DESINV SECTION.
       A.  CALL "CAUP" USING "1303012580" WH-CREG.
           CALL "CAUP" USING "0707090365000" WH-CREG.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Alle DESADV und INVOICE Ñlter als 60 Tage werden gel
      -        "îscht!" with highlight AT VDU-LP.
           MOVE WM-DATUM TO WA-TAGE.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WA-TAGE).
           ADD -65 TO WA-TAGE.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           MOVE "DESADV\*.*" TO WM-PATH PERFORM LOE-DIR.
           MOVE "INVOICE\*.*" TO WM-PATH PERFORM LOE-DIR.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************* Lîschung Verzeichnis *
       LOE-DIR SECTION.
       A.  PERFORM VARYING IX FROM 20 BY -1 UNTIL
               WM-PATH(IX:1) = "\" CONTINUE.
           ADD 1 TO IX.
           MOVE WM-PATH TO WF-EIN.
           MOVE 0 TO WF-HANDL WF-ACT.
       G.  MOVE SPACES TO WF-OUT.
           CALL X"91" USING WF-RESU WF-CODE WF-PARA.              *> 69
           IF WF-ERR = 0 MOVE 1 TO WF-ACT
               ADD 318 VDU-ECK GIVING VDU-LP
               PERFORM RAUS
               MOVE WF-OUT TO WM-PATH(IX:)
               IF DT-DATE < WV-DATUM
                   CALL "CBL_DELETE_FILE" USING WM-PATH
               end-if
               GO G.
       Z.  EXIT.
      ******************************************************************
       RAUS SECTION.
       A.  COMPUTE ZA = WF-DATE / 512 + 1980.
           MOVE 0 TO WZ-JAHR.
           COMPUTE WZ-MONAT = (WF-DATE - ((ZA - 1980) * 512)) / 32.
           COMPUTE WZ-TAG =
                   WF-DATE - (WZ-MONAT * 32 +  (ZA - 1980) * 512).
           COMPUTE WC-TAG = WF-TIME / 2048.
           COMPUTE WC-MONAT = (WF-TIME - (WC-TAG * 2048)) / 32.
           COMPUTE WC-JAHR =
                      (WF-TIME - (WC-TAG * 2048 + WC-MONAT * 32)) * 2.
           MOVE WF-SIZES TO WD-SIZE.
           COMPUTE DT-DATE = ZA * 10000 + WZ-DATUM.
           IF DT-DATE not < WV-DATUM GO Z.
           MOVE DT-DATE TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           DISPLAY WF-OUT AT VDU-LP " " VDU-DATUM " " WD-SIZE.
       Z.  EXIT.
      ******************************************************************
       EDIEND SECTION.
       A.  MOVE "UNS+S" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "CNT+2:" TO WB-SATZ.
           MOVE WH-CST TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "CNT+18:" TO WB-SATZ.
           MOVE WS-MENGE TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "CNT+20:" TO WB-SATZ.
           MOVE WS-BET TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "CNT+22:" TO WB-SATZ.
           MOVE WS-NETTO TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "AUT+BV245988" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "DTM+187:" TO WB-SATZ.
           MOVE WM-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":102" TO WM-CD.                    *> fÅr JJJJMMTT
           PERFORM ANHANG.
           PERFORM ZEILEND.
      *----------------------------------------------> Schlu·segement <-
           MOVE "UNT+" TO WB-SATZ.
           MOVE WH-SEG TO WE-ANZ.             *> Anz. Pos. im CST+ Segm.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-UNH TO WE-ANZ.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE 1 TO WE-OPEN.
       Z.  EXIT.
      ******************************************************************
       DESA-UNT SECTION.
       A.  MOVE "UNT+" TO WB-SATZ.
           MOVE WH-SEG TO WE-ANZ.            *> Anz. Pos. im UNB+ Segm.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-LKNUM TO WD-NUM.
           MOVE WD-NUM TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE 1 TO WH-SEG.
           ADD 1 TO WH-UNH.
       Z.  EXIT.
      ******************************************************************
       EDIFUS SECTION.
       A.  IF WH-UNH = 0 GO Z.
           MOVE "UNZ+" TO WB-SATZ.
           MOVE WH-UNH TO WE-ANZ.                 *> Anz. UNH+ Segmente
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WE-ANZ(1:1) = " " MOVE WE-ANZ(2:) TO WE-ANZ(1:).
           MOVE WE-ANZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WK-UNZ TO WM-CD.                  *> Kennung aus UNB+
           PERFORM ANHANG.
           IF WV-REART = 26 MOVE WH-KONTROL TO WM-CD
               PERFORM ANHANG
               PERFORM ZEILEND
               CALL "CBL_CLOSE_FILE" USING WB-FH
               MOVE "DESADV\TIDExxxx.26 " TO WN-EDICUS
               MOVE WM-LIEFDAT TO WC-DATUM
               PERFORM DATDREH
               MOVE WZ-DATUM(1:4) TO WN-EDICUS(12:4)
               MOVE WN-SEDAS(9:) TO WN-BEST(16:)
               CALL "CBL_COPY_FILE" USING "TIDESADV.261 " WN-EDICUS
               CALL "CBL_COPY_FILE" USING "TIDESADV.261 " "DESADV.UNI "
               CALL "CBL_DELETE_FILE" USING "TIDESADV.26 "
               CALL "CBL_DELETE_FILE" USING "TIDESADV.261 "
               PERFORM DES-BLAT
               GO Z.                              *> Ende DESADV
           PERFORM ZEILEND.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
           MOVE "INTRASTA\INTRxxxx.EDI" TO WN-EDICUS.
           MOVE WS-DATUM TO WZ-DATUM.
           MOVE WZ-DATUM(1:4) TO WN-EDICUS(14:4).
           CALL "CBL_COPY_FILE" USING "27387100.EDI " WN-EDICUS.
       Z.  EXIT.
      ************************************** BAT-Datei fÅr unisend.bat *
       DES-BLAT SECTION.
       A.  MOVE "BLAT\UNISEND.BAT" TO WN-EDICUS.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDICUS WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           MOVE "blat -t daniel.paumgartner@hogast.at -c thomas.oliva@ho
      -       "gast.at -s DESADV" TO WB-SATZ.
           MOVE 73 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE "-body ""DESADV fÅr Lieferung 12.12.12""" TO WB-SATZ.
           MOVE WM-LIEFDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO WB-SATZ(29:8).
           MOVE 38 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE "-attach N:\TIFAKT\DESADV.UNI -server mail.titz.at -f of
      -        "fice@titz.at -log unilog.dat" TO WB-SATZ.
           MOVE 83 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
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
