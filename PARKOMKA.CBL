      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PARKOMKA.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY PARSEART.CPY.
       COPY PANSEART.CPY.
       COPY PARSELFT.CPY.
       COPY PANSEKON.CPY.
       COPY PARSEREZ.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
       COPY PANFDART.CPY.
       COPY PARFDART.CPY.
       COPY PARFDLFT.CPY.
       COPY PARREZEP.CPY.
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
       01  WH-REG.
           03  WV-ARNUM                PIC 9(4)      COMP.
           03  WK-S                    PIC 9         COMP.
           03  WH-MOD                  PIC 9         COMP.
           03  WI                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WD-KOMPO                PIC Z.ZZ9.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-ANZ                  PIC ZZZ.ZZ9-.
           03  WD-MIN                  PIC ZZ9,99-.
           03  WD-IN                   PIC ZZZ9,99-.
           03  WD-SATZ                 PIC Z9,99-.
           03  WD-PZ                   PIC Z9,99.
           03  WD-PRZ                  PIC Z9,9.
           03  WD-ART                  PIC ZZZ9.
           03  WD-POS                  PIC ZZ9.
           03  VDU-REL                 PIC 9999.
           03  WD-SORT                 PIC ZZ9,99.
           03  WD-TEIG                 PIC ZZ.ZZ9-.
           03  WD-KZ                   PIC 9.
           03  WD-BPR                  PIC ZZ.ZZ9,99-.
           03  WD-ABPR                 PIC ZZ.ZZ9,999.
           03  WD-GRP                  PIC Z9.
           03  WD-PREIS                PIC Z.ZZ9,99-.
           03  WD-APREIS               PIC Z.ZZ9,999-.
           03  WD-MENGA                PIC ZZ.ZZ9,99-.
           03  WD-MENGB                PIC ZZ.ZZ9,9-.
           03  WD-MENGC                PIC ZZZ.ZZ9-.
           03  WD-MENGE                PIC X(10).
           03  WH-TEIL                 PIC S999V9(7)  COMP.
           03  WH-MENGE                PIC S9(7)V99   COMP.
           03  WH-PREIS                PIC 9(5)V999   COMP-3.
           03  WR-PR REDEFINES WH-PREIS.
               05 FILLER               PIC X(4).
               05 WR-GR                PIC 9          COMP-3.               
           03  WH-EHP                  PIC S9(7)V99   COMP-3.
           03  WH-ENVP                 PIC S9(5)V9999 COMP-3.
           03  WH-GHP                  PIC S9(7)V99   COMP-3.
           03  WH-GNVP                 PIC S9(5)V9999 COMP-3.
           03  WM-SKST                 PIC S9(5)V9999 COMP-3.
           03  WM-HKST                 PIC S9(5)V9999 COMP-3.
           03  WH-MEH                  PIC 99         COMP.
           03  WH-NK                   PIC 99         COMP.
           03  WH-ART                  PIC 99.
           03  WI-M                    PIC 99         COMP.
           03  WK-LP                   PIC 9999       COMP.
           03  WM-LP                   PIC 9999       COMP.
           03  WV-LAGER                PIC X(5).
           03  WV-SORT                 PIC 999V99     COMP.
           03  WK-PROZ                 PIC S999V99    COMP-3 OCCURS 8.
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
      *---------------------------------------------> 7. Preiselement <-
           03  WG-TAB.
               05 WG-LIEF              PIC X(5).
               05 WG-DATUM             PIC 9(6)       COMP.
               05 WG-BTTO              PIC S9(5)V99   COMP.
               05 WG-RAB               PIC S99V99     COMP.
               05 WG-ZRAB              PIC S99V99     COMP.
               05 WG-AUF               PIC S999V99    COMP.
               05 WG-SYM               PIC 99         COMP.
               05 WG-NTTO              PIC S9(5)V99   COMP.
               05 WG-TX                PIC X(15).
      *-----------------------------------------------------------------
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
           03  WT-INHALT               PIC X(300).
       COPY PAREXT.CPY.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON LAGARTIK.
       A.  CALL "CADECL" USING "PARARTIK.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON LAGMATCH.
       A.  CALL "CADECL" USING "PARMATCH.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 20 PERFORM KALKULAT
               WHEN 40
               WHEN 50
               WHEN 51
               WHEN 52
               WHEN 56 PERFORM KOMPO
               WHEN 55 OPEN I-O REZEPT
                       OPEN I-O ARTIKEL
                       OPEN I-O KONSTANT
                       PERFORM MAKE-MATCH
                       PERFORM KOMPO
                       CLOSE ARTIKEL KONSTANT
                       CLOSE REZEPT.                     *> aus PAKOLAG
       X.  MOVE WH-CREG TO WL-CREG.
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
       A.  DISPLAY " weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO VDU-L.
           IF VDU-L < 20 GO Z.
       B.  CALL "CAUP" USING "1407022070000" WH-CREG.
           SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ********************************************* Lagmatch erstellen *
       MAKE-MATCH SECTION.
       A.  OPEN INPUT LAGMATCH.
           IF WF-STATUS = "00" CLOSE LAGMATCH GO Z.
           CALL "CAUP" USING "0710100340000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " bitte warten " with highlight AT VDU-LP.
           OPEN OUTPUT LAGMATCH.
           MOVE LOW-VALUE TO LG-SATZ.
           START LAGARTIK KEY not < LG-NUM INVALID GO X.
       E.  READ LAGARTIK NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE LG-NUM TO LM-LGNUM.
           MOVE LG-BEZ TO WT-TX.
           CALL "CAUP" USING "20CONV15" WH-CREG.
           MOVE WT-TX TO LM-MCODE.
           MOVE 0 TO LM-LFD.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-NUM AT VDU-LP " " LG-BEZ(1:15).
       G.  WRITE LM-SATZ INVALID ADD 1 TO LM-LFD GO G.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE LAGMATCH.
       Z.  EXIT.
      ********** 50: Anl./éndg. * 51: aus and.Prog. * 52: nur anzeigen *
       KOMPO SECTION.
       A.  MOVE 0 TO WM-MERKER WI.
      *-------------------------------> wl-ca = 40: Bestellverwaltung <-
           IF WL-CA = 40 CALL "CAUP" USING "0709041472000" WH-CREG
               PERFORM KO-BILD
               DISPLAY "Weiter mit beliebiger Taste! < >" AT 2301
               CALL "CAUP" USING "0023310000" WH-CREG
               CALL "CAUP" USING "08CLOFEN" WH-CREG
               GO Z.
           CALL "CAUP" USING "0706041472000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           IF WH-PG = 4
               DISPLAY " Preiswartung " with highlight AT VDU-REL GO B.
           DISPLAY " Komponentenverwaltung " with highlight AT VDU-REL.
           IF WL-CA = 52 PERFORM KO-BILD
               DISPLAY "Weiter mit beliebiger Taste! < >" AT 2301
               CALL "CAUP" USING "0023310000" WH-CREG
               GO X.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:           " AT VDU-LP.
           IF WL-CA = 51 GO M.
       C.  DISPLAY "<ret>= Anl./Aend., alpha+<ret>= suchen, <esc>= Ende,
      -        " < >= Hilfe" AT 2301.
           DISPLAY "<tab>= Art.Nrnwechsel, < >= Reorg Lagerorte, < >= le
      -        "tzter Artikel" AT 2401.
           DISPLAY "<Einf>= nÑchste freie Komponente, <#>= mit BUG-Verbi
      -        "ndung" AT 2501.
           MOVE SPACE TO WT-TX.
           MOVE 0 TO WM-MERKER.
           CALL "CAUP" USING "1002084013" WH-CREG.
           CALL "CAUP" USING "1325012580" WH-CREG.
           MOVE WX-TASTE TO WS-TASTE.
           IF WORE AND LG-NUM NOT = 0 GO P.
           IF AB PERFORM REORG GO C.
           MOVE 0 TO WH-MOD.
           IF HELP CALL "CAUP" USING "190017001801" WH-CREG GO C.
           IF TABL OPEN I-O LAGMATCH
                   PERFORM UMTAUSCH
                   GO B.
           IF ESC MOVE 0 TO LG-NUM GO X.
           IF WOLI SET RET TO TRUE GO M.
           IF EINF PERFORM SUCH-NEXT.
           IF KIST MOVE 1 TO WH-MOD
              SET RET TO TRUE.
           IF NOT RET GO C.
       E.  IF WH-NUM = 0 CALL "PANANZ" USING "30KOSUCH" WH-CREG
                         CANCEL "PANANZ"
               IF LG-NUM = 0 GO C ELSE MOVE LG-NUM TO WH-NUM.
       M.  MOVE 0 TO WM-MERKER.
           MOVE WH-NUM TO LG-NUM.
           IF LG-NUM = 0 GO C.
       P.  READ LAGARTIK INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO P.
           PERFORM KO-ANL.
           UNLOCK LAGARTIK.
           IF WL-CA = 50 OR WL-CA = 55
               CALL "CAUP" USING "16CLRFEN" WH-CREG GO B.
           IF ESC MOVE 0 TO LG-NUM.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK LAGARTIK.
           IF WL-CA = 56 MOVE "PARVORL 60VORL" TO WT-TX
               MOVE 49 TO WH-PG GO Z.
           IF WL-CA = 55 MOVE "PAKOSTAM60STAM" TO WT-TX
               MOVE 49 TO WH-PG GO Z.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
      ******************************************** ob PANART vorhanden *
       OB-PANART SECTION.
       A.  IF WH-MOD = 0 GO Z.
           MOVE LG-NUM TO AR-NUM.
       C.  READ ARTIKEL IGNORE LOCK INVALID GO Z.
           CALL "PANARTAN" USING "53ARTIK" WH-CREG.
           CANCEL "PANARTAN".
       E.  DISPLAY "<Einfg>= Artikel Åbernehmen, <ret>= weiter < >"
               AT 2301.
           CALL "CAUP" USING "0023450000" WH-CREG.
           IF RET GO X.
           IF NOT EINF GO E.
           IF ANLAGE INITIALIZE LG-SATZ
               MOVE AR-BEZ TO LG-BEZ LG-ABEZ
               MOVE AR-EKP TO LG-PREIS
               MOVE 0 TO LG-EKP
               MOVE AR-VKP TO LG-VKP
               MOVE AR-GRM TO WF-GRM
               MOVE WF-NK TO LG-NK
               MOVE WF-MEH TO LG-MEH.
      *--------------------------------> Werte in LAGARTIK Åbernehmen <-
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************** suche nÑchste Komp-Nr. ab *
       SUCH-NEXT SECTION.
       A.  DISPLAY "- bitte warten -" with highlight BLINK AT 2440.
           ADD 208 VDU-ECK GIVING VDU-LP.
           SET ESC TO TRUE.
       C.  MOVE WH-NUM TO LG-NUM.
           IF WH-NUM > 999999 GO X.
       E.  READ LAGARTIK IGNORE LOCK INVALID GO W.
           MOVE LG-NUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           ADD 1 TO WH-NUM.
           GO C.
       W.  SET RET TO TRUE.
       X.  CALL "CAUP" USING "1323012580" WH-CREG.
       Z.  EXIT.
      ***************************************** ArtikelnummernÑnderung *
       UMTAUSCH SECTION.
       A.  CALL "CAUP" USING "0705100543000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Korrektur Komponenten - Rezepturen " AT VDU-LP.
       G.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kompon. alt:       Kompon. neu:" AT VDU-LP.
           DISPLAY "<esc/ret-leer>= Ende" AT 2301.
           CALL "CAUP" USING "1002164004" WH-CREG.
           IF ESC OR WH-NUM = 0 GO X.
           IF NOT RET GO G.
           MOVE WH-NUM TO LG-NUM WV-ARNUM WD-KOMPO.
           DISPLAY WD-KOMPO with highlight AT VDU-LP.
       H.  READ LAGARTIK INVALID PERFORM NO-REC GO G.
           IF ZUGRIF PERFORM BESETZT GO G.
           ADD 305 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-BEZ with highlight AT VDU-LP.
       J.  CALL "CAUP" USING "1002354004" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO G.
           IF NOT RET GO J.
           IF WH-WERT = 0 GO G.
           MOVE WH-WERT TO WD-KOMPO.
           DISPLAY WD-KOMPO with highlight AT VDU-LP.
           DELETE LAGARTIK.
           MOVE WH-NUM TO LG-NUM.
           WRITE LG-SATZ INVALID KEY GO Q.
           MOVE 0 TO RE-NUM.
           START REZEPT KEY NOT < RE-KEY INVALID GO N.
       L.  READ REZEPT NEXT AT END GO N.
           IF ZUGRIF PERFORM BESETZT GO L.
           MOVE RE-NUM TO WD-ART.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ART with highlight AT VDU-LP.
           ADD 5 TO VDU-P.
           DISPLAY RE-BEZ AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
              IF RE-KONUM(WX) = WV-ARNUM MOVE LG-NUM TO RE-KONUM(WX).
           REWRITE RE-SATZ.
           GO L.
       N.  MOVE WV-ARNUM TO LM-LGNUM.
           MOVE 0 TO LM-LFD.
           START LAGMATCH KEY NOT < LM-KEY INVALID GO G.
       O.  READ LAGMATCH NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO O.
           IF LM-LGNUM not = WV-ARNUM GO G.
           DELETE LAGMATCH INVALID GO O.
           MOVE LG-NUM TO LM-LGNUM.
           WRITE LM-SATZ INVALID NEXT SENTENCE.
           GO O.
       Q.  MOVE WV-ARNUM TO LG-NUM.
           WRITE LG-SATZ.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Komponente 'neu' bereits vorhanden" with highlight
               BLINK AT 2401.
           PERFORM WEITER.
           GO G.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KO-BILD SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:      Bez.:" AT VDU-LP.
           ADD 42 TO VDU-LP.
           DISPLAY "Meh:     Nk:   N-%: " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
      *    DISPLAY "Art:   Inhalt:           Lagerort:           Folge:"
           DISPLAY "Inh.:          Meh:     Nk:    Art:     Lager:
      -        "  Folge:" AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Ekpr:                Lgpr:                Lft.:
      -        "    Grp:   " AT VDU-LP.
           PERFORM HALBFAB.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "L-Stand:            M-Stand:            B-Menge: "
               AT VDU-LP.
           IF AEND GO G.
           MOVE LG-NUM TO WH-NUM.
           INITIALIZE LG-SATZ.
           MOVE 1 TO WX.
           MOVE WH-NUM TO LG-NUM.
       G.  ADD 602 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lief. Art     Datum  Bt.Preis  Rab.% Rab.%  Nt.Prei
      -         "s Anmerkung       "  with reverse-video AT VDU-LP.
           MOVE LG-NUM TO WD-ART.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ART with highlight AT VDU-LP.
           IF ANLAGE GO Z.
           ADD 11 TO VDU-LP.
           DISPLAY LG-BEZ with highlight AT VDU-LP.
           ADD 250 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-MEH(LG-BMEH + 1) with highlight
                foreground-color 5 AT VDU-LP.
           MOVE LG-BNK TO WD-KZ.
           ADD 258 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           IF LG-NETPZ not = 0 ADD 265 VDU-ECK GIVING VDU-LP
               MOVE LG-NETPZ TO WD-PRZ
               DISPLAY WD-PRZ with highlight foreground-color 6
                   AT VDU-LP.
      *--------------------------------------> Inhalt Lager-MEH u. NK <-
           MOVE LG-ANZ TO WD-IN.
           ADD 309 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-IN with highlight AT VDU-LP.
           ADD 323 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-MEH(LG-MEH + 1) with highlight
                 foreground-color 3 AT VDU-LP.
           ADD 331 VDU-ECK GIVING VDU-LP.
           MOVE LG-NK TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 339 VDU-ECK GIVING VDU-LP.
           MOVE LG-ART TO WD-GRP.
           DISPLAY WD-GRP with highlight AT VDU-LP.
           ADD 350 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-LAGER with highlight AT VDU-LP.
           ADD 364 VDU-ECK GIVING VDU-LP.
           MOVE LG-SORT TO WD-SORT.
           MOVE "-" TO WD-SORT(4:1).
           DISPLAY WD-SORT with highlight AT VDU-LP.
           ADD 409 VDU-ECK GIVING VDU-LP.
           MOVE LG-EKP TO WD-BPR WD-ABPR WH-PREIS.
           IF WR-GR = 0 DISPLAY WD-BPR with highlight AT VDU-LP "/"
                  else DISPLAY WD-ABPR with highlight AT VDU-LP "/".
           DISPLAY WT-MEH(LG-BMEH + 1) with highlight
                foreground-color 5 AT 0000.
           ADD 430 VDU-ECK GIVING VDU-LP.
           IF LG-ART not = 10
               DIVIDE LG-ANZ INTO LG-EKP GIVING LG-PREIS.
           MOVE LG-PREIS TO WD-PREIS WD-APREIS WH-PREIS.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP "/"
                  else DISPLAY WD-APREIS with highlight AT VDU-LP "/".
           DISPLAY WT-MEH(LG-MEH + 1) with highlight
                foreground-color 3 AT 0000.
           ADD 451 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-LFT with highlight AT VDU-LP.
           ADD 464 VDU-ECK GIVING VDU-LP.
           MOVE LG-GRP TO WD-GRP.
           DISPLAY WD-GRP with highlight AT VDU-LP.
           ADD 512 VDU-ECK GIVING VDU-LP.
           MOVE LG-STAND TO WH-MENGE.
           MOVE LG-NK TO WH-NK.
           PERFORM WD-MG.
           ADD 532 VDU-ECK GIVING VDU-LP.
           MOVE LG-MIND TO WH-MENGE.
           PERFORM WD-MG.
           ADD 552 VDU-ECK GIVING VDU-LP.
           MOVE LG-BMENGE TO WH-MENGE.
           MOVE LG-BNK TO WH-NK.
           PERFORM WD-MG.
           DISPLAY WT-MEH(LG-BMEH + 1) with highlight
               foreground-color 5 AT 0000.
           MOVE 0 TO WY.
           PERFORM VARYING WX FROM 6 BY -1 UNTIL WX = 0
               IF LG-LIEF(WX) NOT = SPACE OR LG-BTTO(WX) NOT = 0
                   ADD 1 TO WY
                   COMPUTE VDU-LP = WY * 100 + 603 + VDU-ECK
                   DISPLAY LG-LIEF(WX) with highlight AT VDU-LP
                   ADD 7 TO VDU-LP
                   EVALUATE LG-SYM(WX)
                       WHEN 0 DISPLAY "zu" AT VDU-LP
                       WHEN 3 DISPLAY "pr" AT VDU-LP
                       WHEN 5 DISPLAY "of" AT VDU-LP
                   end-evaluate
                   MOVE LG-DATUM(WX) TO WC-DATUM
                   PERFORM DATDREH
                   ADD 4 TO VDU-LP
                   DISPLAY VDU-DATUM with highlight AT VDU-LP
                   ADD 9 TO VDU-LP
                   MOVE LG-BTTO(WX) TO WD-BPR
                   DISPLAY WD-BPR with highlight AT VDU-LP
                   ADD 10 TO VDU-LP
                   IF LG-RAB(WX) NOT = 0 MOVE LG-RAB(WX) TO WD-PZ
                       DISPLAY WD-PZ with highlight AT VDU-LP
                   end-if
                   ADD 6 TO VDU-LP
                   IF LG-ZRAB(WX) NOT = 0 MOVE LG-ZRAB(WX) TO WD-PZ
                       DISPLAY WD-PZ with highlight AT VDU-LP
                   end-if
                   ADD 7 TO VDU-LP
                   MOVE LG-NTTO(WX) TO WD-BPR
                   DISPLAY WD-BPR with highlight AT VDU-LP
                   ADD 10 TO VDU-LP
                   DISPLAY LG-TX(WX) with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************** Maske Halbfabrikate *
       HALBFAB SECTION.
       A.  ADD 403 VDU-ECK GIVING VDU-LP.
           IF LG-ART > 9 DISPLAY "Hkst" AT VDU-LP
                    else DISPLAY "Ekpr" AT VDU-LP.
           ADD 424 VDU-ECK GIVING VDU-LP.
           IF LG-ART > 9 DISPLAY "SKst" AT VDU-LP
                    else DISPLAY "Lgpr" AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       WD-MG SECTION.
       A.  EVALUATE WH-NK
               WHEN 2 MOVE WH-MENGE TO WD-MENGA
                      MOVE WD-MENGA(1:) TO WD-MENGE
               WHEN 1 MOVE WH-MENGE TO WD-MENGB
                      MOVE WD-MENGB(1:) TO WD-MENGE
               WHEN 0 MOVE WH-MENGE TO WD-MENGC
                      MOVE WD-MENGC(1:) TO WD-MENGE.
           DISPLAY WD-MENGE with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************** Mengeneinheit u. NK *
       MEH-NK SECTION.
       A.  DISPLAY "<tab>= anzeigen Mengeneinheiten" AT 2301.
           IF WK-S = 1 DISPLAY "Einkaufs" with highlight AT 2401
                  else DISPLAY "Lager" with highlight AT 2401.
           DISPLAY "mengeneinheit" with highlight AT 0000.
       E.  MOVE WH-MEH TO WH-WERT.
           MOVE WM-LP TO VDU-ECK.
           MOVE WK-LP TO VDU-LP.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF WI-M = 0 MOVE 1 TO WI-M
               CALL "CAUP" USING "08CLOFEN" WH-CREG
               MOVE WS-TASTE TO WX-TASTE.
           IF TABL PERFORM MENGEN
                MOVE 0 TO WI-M GO A.
           IF ESC or WOLI or AUF GO Z.
           IF NOT RET GO A.
           MOVE WH-WERT TO WH-MEH.
           IF WH-MEH > 19 GO A.
           IF WK-S = 1 DISPLAY WT-MEH(WH-MEH + 1) with highlight
                           foreground-color 5 AT VDU-LP
           else DISPLAY WT-MEH(WH-MEH + 1) with highlight
                           foreground-color 3 AT VDU-LP.
       F.  DISPLAY "Nachkommastellen:" AT 2301.
           DISPLAY "0 = ohne, 1= 1 NK, 2= 2 NK, 3= 3 NK" AT 2401.
           MOVE WH-NK TO WH-WERT.
           ADD WK-LP 8 GIVING VDU-LP.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO A.
           IF NOT RET GO F.
           MOVE WH-WERT TO WH-NK.
           IF WH-NK > 3 GO F.
           MOVE WH-NK TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       Z.  EXIT.
      *****************************************************************
       KO-ANL SECTION.
       A.  PERFORM KO-BILD.
           IF LG-NUM > 2000 PERFORM OB-PANART.
           IF WM-MERKER NOT = 1 GO V.
       B.  DISPLAY "<esc>= Abbruch, < >= zus. Matchcode" AT 2301.
           IF ANLAGE DISPLAY "Komponente fehlt!" AT 2401.
           MOVE LG-BEZ TO WT-TX.
           CALL "CAUP" USING "1202190121" WH-CREG.
           IF WOLI or AUF or ESC GO Z.
           IF WT-TX = SPACE DISPLAY "Bezeichnung mu· sein!" AT 2401
               PERFORM WEITER GO B.
           MOVE WT-TX TO LG-BEZ LG-ABEZ.
           IF AB OPEN I-O LAGMATCH
                 CALL "PANANZ" USING "35MATCH" WH-CREG
                 CANCEL "PANANZ".
       C.  MOVE 1 TO WI-M WK-S.
           MOVE VDU-ECK TO WM-LP.
           MOVE 250 TO WK-LP.
           MOVE LG-BMEH TO WH-MEH.
           MOVE LG-BNK TO WH-NK.
           PERFORM MEH-NK.                    *> Einkaufsmengeneinheit
           IF ESC GO C.
           IF WOLI or AUF GO B.
           MOVE WH-MEH TO LG-BMEH.
           MOVE WH-NK TO LG-BNK.
      *------------------------------------------> Faktor Nettoanteil <-
       D.  DISPLAY "Nettogewichtsminderung in Kalkulation"
                with highlight AT 2401.
           DISPLAY "Verbleibende Gewichtsmenge!" with
                foreground-color 14 AT 2501.
           MOVE LG-NETPZ TO WH-WERT.
           CALL "CAUP" USING "1002652104" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO C.
           IF NOT RET GO D.
           MOVE WH-WERT TO LG-NETPZ WD-PRZ.
           IF LG-NETPZ not = 0
               DISPLAY WD-PRZ with highlight foreground-color 6
                   AT VDU-LP.
      *-------------------------> Faktor f. Entnahme- bzw. Lagermenge <-
       E.  DISPLAY "Packungsinhalt" with highlight AT 2301 " (Lager- bzw
      -        ". Entnahmemenge)".
           MOVE LG-ANZ TO WH-WERT.
           CALL "CAUP" USING "1003094307" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO D.
           IF NOT RET GO E.
           IF WH-WERT = 0 MOVE 1 TO WH-WERT.
           MOVE WH-WERT TO WD-IN LG-ANZ.
           DISPLAY WD-IN with highlight AT VDU-LP.
           IF LG-ART not = 10
               DIVIDE LG-ANZ INTO LG-EKP GIVING LG-PREIS.
           IF NOT ANLAGE PERFORM KO-BILD.
       F.  MOVE 2 TO WI-M WK-S.
           MOVE VDU-ECK TO WM-LP.
           MOVE 323 TO WK-LP.
           MOVE LG-MEH TO WH-MEH.
           MOVE LG-NK TO WH-NK.
           PERFORM MEH-NK.                    *> Einkaufsmengeneinheit
           IF ESC GO V.
           IF WOLI or AUF GO E.
           MOVE WH-MEH TO LG-MEH.
           MOVE WH-NK TO LG-NK.
       G.  DISPLAY "Komponentenart:" AT 2301.
           DISPLAY "0: Material, 1: Zeit, 2: %-Aufschlg., 3: %-Zeitzusch
      -        "lag, 10: Halbfabrikat" AT 2401.
           MOVE LG-ART TO WH-WERT.
           CALL "CAUP" USING "1003392002" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO F.
           IF NOT RET GO G.
           MOVE WH-WERT TO LG-ART WD-GRP.
           IF LG-ART not = 10 and LG-ART > 3 GO G.
           DISPLAY WD-GRP with highlight AT VDU-LP.
           PERFORM HALBFAB.
      *----------------------------------------------------> Lagerort <-
       J.  DISPLAY "Lagerort" AT 2301.
           MOVE LG-LAGER TO WT-TX.
           CALL "CAUP" USING "1203500105" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO G.
           IF NOT RET GO J.
           MOVE WT-TX TO LG-LAGER.
           DISPLAY LG-LAGER with highlight AT VDU-LP.
      *--------------------------------------------------> Sortierung <-
       K.  DISPLAY "Sortiergfole in Aufnamheliste xxx,xx" AT 2301.
           MOVE LG-SORT TO WH-WERT.
           CALL "CAUP" USING "1003643206" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO J.
           IF NOT RET GO K.
           MOVE WH-WERT TO LG-SORT WD-SORT.
           MOVE "-" TO WD-SORT(4:1).
           DISPLAY WD-SORT with highlight AT VDU-LP.
      *-------------------------------------------------> Kalk. Preis <-
       L.  DISPLAY "Einkaufspreis" with highlight AT 2301.
           MOVE LG-EKP TO WH-WERT.
           CALL "CAUP" USING "1004095309" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO K.
           IF NOT RET GO L.
           MOVE WH-WERT TO LG-EKP WD-BPR WD-ABPR WH-PREIS.
           IF WR-GR = 0 DISPLAY WD-BPR with highlight AT VDU-LP "/"
                  ELSE DISPLAY WD-ABPR with highlight AT VDU-LP "/".
           DISPLAY WT-MEH(LG-BMEH + 1) with highlight
                foreground-color 5 AT 0000.
           ADD 430 VDU-ECK GIVING VDU-LP.
           IF LG-ART not = 10
               DIVIDE LG-ANZ INTO LG-EKP GIVING LG-PREIS.
           MOVE LG-PREIS TO WD-PREIS WD-APREIS WH-PREIS.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP "/"
                  else DISPLAY WD-APREIS with highlight AT VDU-LP "/".
           DISPLAY WT-MEH(LG-MEH + 1) with highlight
                foreground-color 3 AT 0000.
       N.  DISPLAY "Kalkulationspreis" with highlight AT 2301.
           MOVE LG-PREIS TO WH-WERT.
           CALL "CAUP" USING "1004304208" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO L.
           IF NOT RET GO N.
           MOVE WH-WERT TO LG-PREIS WD-PREIS WD-APREIS WH-PREIS.
           IF WR-GR = 0 DISPLAY WD-PREIS with highlight AT VDU-LP "/"
                  else DISPLAY WD-APREIS with highlight AT VDU-LP "/".
           DISPLAY WT-MEH(LG-MEH + 1) with highlight
                foreground-color 3 AT 0000.
       O.  DISPLAY "alpha+<ret>= Lieferant" AT 2301.
           DISPLAY "Nr.+<ret>= Anlage falls nicht vorhanden" AT 2401.
           MOVE LG-LFT TO WT-TX.
           ADD 451 VDU-ECK GIVING VDU-LP.
           DISPLAY LG-LFT with highlight AT VDU-LP.
           CALL "CAUP" USING "1204510105" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO N.
           IF NOT RET AND NOT AB GO O.
           PERFORM READ-KRED.
           IF WH-NUM = 0 GO O.
           MOVE WT-TX TO LG-LFT.
           DISPLAY WT-TX(1:5) with highlight AT VDU-LP.
       P.  DISPLAY "Inv.Gruppe: 1-30" AT 2301.
           MOVE LG-GRP TO WH-WERT.
           CALL "CAUP" USING "1004642002" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO O.
           IF NOT RET GO P.
           MOVE WH-WERT TO LG-GRP WD-GRP.
           IF LG-GRP > 30 GO P.
           DISPLAY WD-GRP with highlight AT VDU-LP.
           GO R.
       Q.  DISPLAY "Lagerstand" AT 2301.
           MOVE LG-STAND TO WH-WERT.
           CALL "CAUP" USING "1005125208" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO P.
           IF RET MOVE LG-STAND TO WH-WERT
                  SET SEIN TO TRUE.
           IF NOT SEIN GO Q.
           MOVE WH-WERT TO LG-STAND WH-MENGE.
           MOVE LG-NK TO WH-NK.
           PERFORM WD-MG.
       R.  DISPLAY "Mindeststand" AT 2301.
           MOVE LG-MIND TO WH-WERT.
           CALL "CAUP" USING "1005325208" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO Q.
           IF NOT RET GO R.
           MOVE WH-WERT TO LG-MIND WH-MENGE.
           MOVE LG-NK TO WH-NK.
           PERFORM WD-MG.
       S.  DISPLAY "Bestellmenge" AT 2301.
           MOVE LG-BMENGE TO WH-WERT.
           CALL "CAUP" USING "1005525208" WH-CREG.
           IF ESC GO V.
           IF WOLI or AUF GO R.
           IF NOT RET GO S.
           MOVE WH-WERT TO LG-BMENGE WH-MENGE.
           MOVE LG-BNK TO WH-NK.
           PERFORM WD-MG.
           DISPLAY WT-MEH(LG-BMEH + 1) with highlight
                 foreground-color 5 AT 0000.
       V.  IF ANLAGE AND ESC GO Z.
           DISPLAY "<Entf>= lîschen, <ret/Ende>= speichern, <esc>= Abbru
      -        "ch, <Bild- >= Zutaten < >" AT 2301.
           DISPLAY "<F1>= Hilfe, < >= Ñndern, <>= zurÅck, < >= PreisÑnd
      -        "erung, <Einf>= Matchcode" AT 2401.
           IF LG-INHALT not = SPACE
               DISPLAY "Zutaten" with highlight AT 2367.
           DISPLAY "<#>= mit Anl./éndg. im BUG" AT 2501.
           IF WH-MOD = 1 DISPLAY " Achtung! Mit énderung im BUG"
                with highlight AT 2500.
           CALL "CAUP" USING "0023760000" WH-CREG.
           IF HELP CALL "CAUP" USING "190015001601" WH-CREG GO V.
           IF ESC UNLOCK LAGARTIK GO Z.
           IF AUF GO B.
           IF AB PERFORM PREISE
                 GO V.
           IF WOLI or AUF GO S.
           IF ENTF GO Y.
           IF BAB PERFORM ZUTATEN GO V.
           IF EINF OPEN I-O LAGMATCH
                   CALL "PANANZ" USING "33MATCH" WH-CREG
                   CANCEL "PANANZ".
           IF NOT RET and not KIST GO V.
       X.  MOVE LG-BEZ TO WT-TX.
           CALL "CAUP" USING "20CONV10" WH-CREG.
           MOVE WT-TX TO LG-ABEZ.
           WRITE LG-SATZ INVALID REWRITE LG-SATZ.          *> Artikel
           PERFORM ZUSMATCH.
           IF WH-MOD = 1 or KIST PERFORM MACH-PANART.
           GO Z.
       Y.  CALL "CAUP" USING "1325012580" WH-CREG.
           DISPLAY "Artikel: " AT 2401 WD-ART with highlight " "
               LG-BEZ with highlight " gelîscht! "
               PERFORM WEITER.
           DELETE LAGARTIK INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO Y.
           PERFORM DELMATCH.
       Z.  EXIT.
      ***************************************** zus. Matchcode lîschen *
       DELMATCH SECTION.
       A.  MOVE 0 TO LM-LFD.
           MOVE LG-NUM TO LM-LGNUM.
           OPEN I-O LAGMATCH.
       C.  READ LAGMATCH INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
       D.  IF LG-NUM not = LM-LGNUM GO X.
           DELETE LAGMATCH INVALID NEXT SENTENCE.
       E.  READ LAGMATCH NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           GO D.
       X.  CLOSE LAGMATCH.
       Z.  EXIT.
      *************************************** zus. Matchcode erstellen *
       ZUSMATCH SECTION.
       A.  MOVE LG-BEZ TO WT-TX.
           CALL "CAUP" USING "20CONV15" WH-CREG.
           OPEN I-O LAGMATCH.
           MOVE 0 TO LM-LFD.
           MOVE LG-NUM TO LM-LGNUM.
       B.  READ LAGMATCH INVALID GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE WT-TX TO LM-MCODE.
           REWRITE LM-SATZ.
           GO X.
       C.  MOVE WT-TX TO LM-MCODE.
       G.  WRITE LM-SATZ INVALID ADD 1 TO LM-LFD GO G.
       X.  CLOSE LAGMATCH.
       Z.  EXIT.
      ********************************************** Zutatenverwaltung *
       ZUTATEN SECTION.
       A.  MOVE LG-INHALT TO WT-INHALT.
           CALL "CAUP" USING "0705501429000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zutaten " with highlight foreground-color 2
               AT VDU-LP.
           PERFORM BILD.
           IF WX > 12 MOVE 12 TO WX.
       C.  DISPLAY "<ret>= énderung, <esc>= Abbruch, < / >= ab/auf, <End
      -        "e>= Ende" AT 2301.
           DISPLAY "<Einf>= Zeile einfÅgen" AT 2401.
           COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK.
           MOVE LG-INH(WX) TO WT-TX.
           DISPLAY LG-INH(WX) with highlight AT VDU-LP.
           COMPUTE VDU-LP = WX * 100 + 103.
           CALL "CAUP" USING "1200030125" WH-CREG.
           IF ESC GO X.
           IF EINF; IF LG-INH(12) not = SPACE GO C
               else PERFORM VARYING WY FROM 11 BY -1 UNTIL WY = (WX - 1)
                        MOVE LG-INH(WY) TO LG-INH(WY + 1)
                    end-perform
                    MOVE SPACE TO LG-INH(WX)
                    MOVE WX TO WZ
                    PERFORM BILD
                    MOVE WZ TO WX
                    GO C.
           IF NOT RET and not ENDE and not AUF and not AB GO C.
           MOVE WT-TX TO LG-INH(WX).
           IF LG-INH(WX) = SPACE MOVE WX TO WZ
                                 PERFORM AUSREORG
                                 PERFORM BILD
                                 MOVE WZ TO WX
                                 IF RET GO C.
           DISPLAY LG-INH(WX) with highlight AT VDU-LP.
           IF ENDE PERFORM AUSREORG GO X.
           IF AUF and WX > 1 ADD -2 TO WX.
           IF WX < 12 ADD 1 TO WX.
           GO C.
       X.  IF ESC MOVE WT-INHALT TO LG-INHALT.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BILD SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 12
               COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK
               DISPLAY LG-INH(WX) with highlight AT VDU-LP.
       Z.  EXIT.
      **************************************** Reorg und Wiederausgabe *
       AUSREORG SECTION.
       A.  PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 11
              IF LG-INH(WY) = SPACE
                  MOVE LG-INH(WY + 1) TO LG-INH(WY)
                  MOVE SPACE TO LG-INH(WY + 1).
       Z.  EXIT.
      ********************************************** Panartik erzeugen *
       MACH-PANART SECTION.
       A.  INITIALIZE AR-SATZ.
           MOVE LG-NUM TO AR-NUM.
           MOVE LG-ABEZ TO AR-ABEZ.
           MOVE " " TO AR-SA.
           MOVE LG-BEZ TO AR-BEZ.
           MOVE 0 TO AR-KTO.
           MOVE 1 TO AR-UST.
           MOVE LG-PREIS TO AR-EKP.
           MOVE 0 TO AR-PREIS.
           MOVE LG-VKP TO AR-VKP.
           MOVE LG-NK TO WF-NK.
           MOVE LG-MEH TO WF-MEH.
           MOVE 98 TO WF-GRP.
           MOVE 0 TO WF-SON.
           MOVE 1 TO WF-TB.
           MOVE WF-GRM TO AR-GRM.
       C.  READ ARTIKEL not INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           WRITE AR-SATZ.
       E.  CALL "PANARTAN" USING "80UEBART" WH-CREG.
           CANCEL "PANARTAN".
       Z.  EXIT.
      ********************************************** Lieferanten lesen *
       READ-KRED SECTION.
       A.  MOVE WT-TX TO WH-MCODE WV-MCODE.
           CALL "CAUP" USING "15NUMC5005" WH-CREG.
           CALL "CAUP" USING "20CONV5" WH-CREG.
           MOVE WT-TX TO KR-MCODE.
       E.  READ KREDIT KEY IS KR-MCODE NOT INVALID GO G.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WH-NUM NOT = 0 MOVE WH-NUM TO KR-KTONR GO F.
           CALL "PAKOANZ" USING "40KRANZ" WH-CREG.
           CANCEL "PAKOANZ"
           IF WH-NUM = 0 GO Z.
       F.  READ KREDIT NOT INVALID GO G.
           IF ZUGRIF PERFORM BESETZT GO F.
           CALL "PAKOKRED" USING "21ANL" WH-CREG.
           CANCEL "PAKOKRED".
           IF KR-KTONR = 0 MOVE 0 TO WH-NUM.
           GO Z.
       G.  MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV5" WH-CREG.
           MOVE KR-KTONR TO WH-NUM.
       Z.  EXIT.
      ********************************************* man. PreisÑnderung *
       PREISE SECTION.
       A.  MOVE 0 TO WY.
           PERFORM VARYING WX FROM 6 BY -1 UNTIL WX = 0
               IF LG-LIEF(WX) NOT = SPACE OR LG-BTTO(WX) NOT = 0
                   ADD 1 TO WY.
      *    IF WY = 0 MOVE 1 TO WY.
           MOVE WY TO WI.
           MOVE 0 TO WS.
       C.  IF WS NOT = 0 MOVE LG-TAB(WS) TO WG-TAB
                   else MOVE SPACE TO WG-LIEF
                        MOVE 0 TO WG-DATUM WG-BTTO WG-RAB WG-AUF
                                  WG-ZRAB WG-SYM WG-NTTO.
           DISPLAY "<esc>= Abbruch, alpha+<ret>= Lieferant, < / >= auf/a
      -        "b, <Ende>= Ende" AT 2301.
           DISPLAY "Nr.+<ret>= Anlage falls nicht vorhanden" AT 2401.
           COMPUTE VDU-LP = WY * 100 + 703.
           MOVE WG-LIEF TO WT-TX.
           CALL "CAUP" USING "1200030105" WH-CREG.
           IF ENDE GO X.
           IF AUF; IF WY > 0 ADD 1 TO WS
                       ADD -1 TO WY
                   end-if GO C.
           IF AB; IF WY NOT = WI ADD -1 TO WS
                                 ADD 1 TO WY
                                 end-if GO C.
           IF ESC GO X.
           IF NOT RET GO C.
           PERFORM READ-KRED.
           IF WH-NUM = 0 GO C.
           DISPLAY WT-TX(1:5) with highlight AT VDU-LP.
           MOVE WT-TX TO WG-LIEF.
       I.  DISPLAY "<esc>= Abbruch, <ret>= Symbol, <>= zurÅck" AT 2301.
           DISPLAY "3= neuer Preis " with highlight AT 2401
               "(bewirkt PreisÑnderung)," " 5= Offert" with highlight.
           COMPUTE VDU-LP = WY * 100 + 710.
           MOVE WG-SYM TO WH-WERT.
           CALL "CAUP" USING "1000101001" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO C.
           IF NOT RET GO I.
           IF WH-WERT NOT = 3 AND NOT = 5 GO I.
           MOVE WH-WERT TO WG-SYM.
           EVALUATE WG-SYM
               WHEN 0 DISPLAY "zu" AT VDU-LP
               WHEN 3 DISPLAY "pr" AT VDU-LP
               WHEN 5 DISPLAY "of" AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Datum, <>= zurÅck" AT 2301.
           COMPUTE VDU-LP = WY * 100 + 714 + VDU-ECK.
           IF WG-DATUM = 0 MOVE WM-DATUM TO WG-DATUM.
           MOVE WG-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           COMPUTE VDU-LP = WY * 100 + 714.
           MOVE WG-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1100006006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO I.
           IF NOT RET OR WZ-DATUM = 0 GO K.
           MOVE WZ-DATUM TO WG-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       M.  DISPLAY "<esc>= Abbruch, <ret>= Preis, <>= zurÅck" AT 2301.
           MOVE WG-BTTO TO WH-WERT.
           COMPUTE VDU-LP = WY * 100 + 723.
           CALL "CAUP" USING "1000005309" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO K.
           IF NOT RET GO M.
           MOVE WH-WERT TO WD-BPR WG-BTTO.
           DISPLAY WD-BPR with highlight AT VDU-LP.
       O.  DISPLAY "<esc>= Abbruch, <ret>= Rabatt, <>= zurÅck" AT 2301.
           MOVE WG-RAB TO WH-WERT.
           COMPUTE VDU-LP = WY * 100 + 733.
           CALL "CAUP" USING "1000002205" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO K.
           IF NOT RET GO O.
           MOVE WH-WERT TO WD-PZ WG-RAB.
           IF WG-RAB = 0 DISPLAY "     " AT VDU-LP
                    else DISPLAY WD-PZ with highlight AT VDU-LP.
       Q.  DISPLAY "<esc>= Abbruch, <ret>= Rabatt, <>= zurÅck" AT 2301.
           MOVE WG-ZRAB TO WH-WERT.
           COMPUTE VDU-LP = WY * 100 + 739.
           CALL "CAUP" USING "1000002205" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO O.
           IF NOT RET GO Q.
           MOVE WH-WERT TO WD-PZ WG-ZRAB.
           IF WG-ZRAB = 0 DISPLAY "     " AT VDU-LP
                     else DISPLAY WD-PZ with highlight AT VDU-LP.
           MOVE WG-BTTO TO WG-NTTO.
           SUBTRACT WG-RAB FROM 100 GIVING WH-TEIL.
           IF WG-RAB NOT = 0 DIVIDE 100 INTO WH-TEIL
               MULTIPLY WG-NTTO BY WH-TEIL GIVING WG-NTTO ROUNDED.
           SUBTRACT WG-ZRAB FROM 100 GIVING WH-TEIL.
           IF WG-ZRAB NOT = 0 DIVIDE 100 INTO WH-TEIL
               MULTIPLY WG-NTTO BY WH-TEIL GIVING WG-NTTO ROUNDED.
           MOVE WG-NTTO TO WD-BPR.
           ADD 7 TO VDU-LP.
           DISPLAY WD-BPR with highlight AT VDU-LP.
       R.  DISPLAY "Hinweistext 15 Zeichen" with highlight AT 2301.
           MOVE WG-TX TO WT-TX.
           COMPUTE VDU-LP = WY * 100 + 756.
           CALL "CAUP" USING "1200000115" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO Q.
           IF NOT RET GO R.
           MOVE WT-TX TO WG-TX.
           DISPLAY WG-TX with highlight AT VDU-LP.
       S.  DISPLAY "<esc>= Abbruch, <ret>= speichern, <>= zurÅck < >"
               AT 2301.
           CALL "CAUP" USING "0023480000" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO R.
           IF NOT RET GO S.
           IF WS NOT = 0 MOVE WG-TAB TO LG-TAB(WS)
               PERFORM KO-BILD GO A.
      *-------------------------------------> nachschieben bei WY = 6 <-
           PERFORM VARYING WX FROM 6 BY -1 UNTIL WX = 1
               MOVE LG-TAB(WX - 1) TO LG-TAB(WX).
           MOVE WG-TAB TO LG-TAB(1).
           IF LG-SYM(1) = 3 MOVE LG-NTTO(1) TO LG-EKP
               MOVE LG-LIEF(1) TO LG-LFT.
       W.  REWRITE LG-SATZ.
       X.  PERFORM KO-BILD.
       Z.  EXIT.
      **************************************************** Kalkulation *
       KALKULAT SECTION.
       A.  CALL "CAUP" USING "0706091660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           MOVE RE-ART TO WH-TART.
           DISPLAY " Kalkulation " with highlight AT VDU-REL.
           MOVE RE-NUM TO AR-NUM.
       C.  READ ARTIKEL IGNORE LOCK INVALID KEY INITIALIZE AR-SATZ.
           MOVE 6 TO WH-KEY.
       D.  READ KONSTANT IGNORE LOCK INVALID GO X.
           IF WH-BK = 0 PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
                            MOVE KO-PROZ(WZ) TO WK-PROZ(WZ)
                   else PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
                            MOVE KO-PROZ(WZ + 8) TO WK-PROZ(WZ).
           PERFORM KALKBILD.
           MOVE 0 TO WH-BET WS-BET.
      *---------------------------------------------> Materialeinsatz <-
           ADD 250 VDU-ECK GIVING VDU-REL.
           MOVE RE-PREIS TO WD-PREIS WS-BET.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *--------------------------------------------------> Lohnkosten <-
           ADD 323 VDU-ECK GIVING VDU-REL.
           DISPLAY WH-BK with highlight AT VDU-REL "/"
           IF WH-BK = 0 DISPLAY "B" AT 0000
                  ELSE DISPLAY "K" AT 0000.
           IF WH-MZUS NOT = 0 DIVIDE 100 INTO WH-MZUS GIVING WH-FAKT
               ADD 1 TO WH-FAKT
               COMPUTE WH-MIN = WH-MIN * WH-FAKT
               COMPUTE RE-ZEIT(1) = WT-ZEIT(1) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(2) = WT-ZEIT(2) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(3) = WT-ZEIT(3) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(4) = WT-ZEIT(4) * WH-FAKT / WH-STK
               COMPUTE RE-ZEIT(5) = WT-ZEIT(5) * WH-FAKT / WH-STK.
           DIVIDE WH-STK INTO WH-MIN GIVING WH-DUMIN ROUNDED.
           MOVE WH-DUMIN TO WD-MIN.
           ADD 331 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-MIN with highlight AT VDU-REL.
           IF WH-BK = 0 MOVE KO-FAKT(1) TO WD-SATZ WH-WERT
                   ELSE MOVE KO-FAKT(2) TO WD-SATZ WH-WERT.
           ADD 342 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-SATZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-MIN / WH-STK.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WS-BET.
           ADD 8 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *--------------------------------------> Produktionsgmeinkosten <-
           ADD 431 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-MIN with highlight AT VDU-REL.
           IF WH-BK = 0 MOVE KO-FAKT(3) TO WD-SATZ WH-WERT
                   ELSE MOVE KO-FAKT(4) TO WD-SATZ WH-WERT.
           ADD 442 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-SATZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-MIN / WH-STK.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WS-BET.
           ADD 8 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *----------------------------------------------> Herstellkosten <-
           ADD 539 VDU-ECK GIVING VDU-REL.
           MOVE WS-BET TO WD-PREIS WM-HKST.
           DISPLAY WD-PREIS AT VDU-REL.
           ADD 11 TO VDU-REL.
           DISPLAY WD-PREIS AT VDU-REL.
      *---------------------------------> Retourwarenanteil GHP / EHP <-
           MOVE WK-PROZ(1) TO WD-PZ WH-WERT.
           ADD 621 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET = WS-BET * WH-WERT / 100.
           ADD WS-BET WH-BET GIVING WH-GHP.
           MOVE WH-BET TO WD-PREIS.
           ADD 18 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
           MOVE WK-PROZ(2) TO WD-PZ WH-WERT.
           SUBTRACT 9 FROM VDU-REL.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET = WS-BET * WH-WERT / 100.
           ADD WS-BET WH-BET GIVING WH-EHP.
           MOVE WH-BET TO WD-PREIS.
           ADD 20 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *-----------------------------------> Vertriebskosten GHP / EHP <-
           ADD WK-PROZ(3) WK-PROZ(5) WK-PROZ(7) GIVING WH-BET.
           COMPUTE WH-GNVP = WH-GHP / (100 - WH-BET).
           ADD WK-PROZ(4) WK-PROZ(6) WK-PROZ(8) GIVING WH-BET.
           COMPUTE WH-ENVP = WH-EHP / (100 - WH-BET).
           MOVE WK-PROZ(3) TO WD-PZ WH-WERT.
           ADD 721 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WH-GHP.
           ADD 18 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
           SUBTRACT 9 FROM VDU-REL.
           MOVE WK-PROZ(4) TO WD-PZ WH-WERT.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WH-EHP.
           ADD 20 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *-------------------------------------------> Verwaltungskosten <-
           MOVE WK-PROZ(5) TO WD-PZ WH-WERT.
           ADD 821 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WH-GHP.
           ADD 18 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
           SUBTRACT 9 FROM VDU-REL.
           MOVE WK-PROZ(6) TO WD-PZ WH-WERT.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WH-EHP.
           ADD 20 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *------------------------------------------------> Selbstkosten <-
           ADD 939 VDU-ECK GIVING VDU-REL.
           MOVE WH-GHP TO WD-PREIS WM-SKST.
           DISPLAY WD-PREIS AT VDU-REL.
           ADD 11 TO VDU-REL.
           MOVE WH-EHP TO WD-PREIS.
           DISPLAY WD-PREIS AT VDU-REL.
           IF RE-NUM < 1000 MOVE WH-GHP TO RE-SKP       *> Selbstkosten
           else MOVE WS-BET TO RE-SKP.                *> Herstellkosten
      *--------------------------------------------> Risiko u. Gewinn <-
           MOVE WK-PROZ(7) TO WD-PZ WH-WERT.
           ADD 1021 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-GNVP.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WH-GHP.
           ADD 18 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
           SUBTRACT 9 FROM VDU-REL.
           MOVE WK-PROZ(8) TO WD-PZ WH-WERT.
           DISPLAY WD-PZ with highlight AT VDU-REL.
           COMPUTE WH-BET ROUNDED = WH-WERT * WH-ENVP.
           MOVE WH-BET TO WD-PREIS.
           ADD WH-BET TO WH-EHP.
           ADD 20 TO VDU-REL.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *------------------------------------------> Nettoverkaufspreis <-
           ADD 1139 VDU-ECK GIVING VDU-REL.
           MOVE WH-GHP TO WD-PREIS.
           DISPLAY WD-PREIS AT VDU-REL.
           ADD 11 TO VDU-REL.
           MOVE WH-EHP TO WD-PREIS.
           DISPLAY WD-PREIS AT VDU-REL.
           IF AR-NUM = 0 ADD 1635 VDU-ECK GIVING VDU-LP
               DISPLAY " Artikel nicht angelegt " AT VDU-LP GO Q.
      *------------------------------------------------> Umsatzsteuer <-
           ADD 1203 VDU-ECK GIVING VDU-REL.
           IF AR-UST > 0 MOVE WT-UST(AR-UST) TO WD-PZ WH-WERT
               DISPLAY WD-PZ with highlight AT VDU-REL
               COMPUTE WH-WERT = WH-WERT / 100
               COMPUTE WH-BET ROUNDED = WH-EHP * WH-WERT
               MOVE WH-BET TO WD-PREIS
               ADD WH-BET TO WH-EHP
               ADD 47 TO VDU-REL
               DISPLAY WD-PREIS AT VDU-REL.
      *-----------------------------------------------> Verkaufspreis <-
           ADD 1339 VDU-ECK GIVING VDU-REL.
           MOVE WH-GHP TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
           ADD 11 TO VDU-REL.
           MOVE WH-EHP TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-REL.
      *------------------------------------> effektiver Verkaufspreis <-
           IF AR-ADAT = 0 OR AR-ADAT > WM-DATUM
               MOVE AR-PREIS TO WD-PREIS WH-WERT
               MOVE AR-VKP TO WH-PREIS
           ELSE MOVE AR-APREIS TO WD-PREIS WH-WERT
               MOVE AR-AVKP TO WH-PREIS.
           ADD 1439 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-PREIS AT VDU-REL.
           ADD 11 TO VDU-REL.
           MOVE WH-PREIS TO WD-PREIS.
           DISPLAY WD-PREIS AT VDU-REL.
      *-----------------------------------------------------> Deckung <-
           COMPUTE WH-BET = (WH-WERT / WH-GHP * 100) - 100.
           MOVE WH-BET TO WD-MIN.
           ADD 1541 VDU-ECK GIVING VDU-REL.
           IF WH-BET < 0 DISPLAY WD-MIN with foreground-colour 4
                highlight AT VDU-REL
           ELSE DISPLAY WD-MIN with foreground-colour 2 highlight
                AT VDU-REL.
           COMPUTE WH-BET = (WH-PREIS / WH-EHP * 100) - 100.
           MOVE WH-BET TO WD-MIN.
           ADD 11 TO VDU-REL.
           IF WH-BET < 0 DISPLAY WD-MIN with foreground-colour 4
                highlight AT VDU-REL
           ELSE DISPLAY WD-MIN with foreground-colour 2 highlight
                AT VDU-REL.
       Q.  DISPLAY "beliebige Taste = zurÅck, < > <Ende>= mit " AT 2301.
           IF RE-NUM < 999 DISPLAY "ArtikelÑnderung" with highlight
                               foreground-color 2 AT 0000
                      else DISPLAY "KomponentenÑnderung" with highlight
                               foreground-color 3 AT 0000.
           CALL "CAUP" USING "0023280000" WH-CREG.
           IF ENDE PERFORM ABSTELL.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *********************************************** Preise abstellen *
       ABSTELL SECTION.
       A.  CLOSE ARTIKEL.
           OPEN I-O ARTIKEL.
           MOVE RE-NUM TO AR-NUM LG-NUM.
           IF RE-NUM > 999 GO G.
       C.  READ ARTIKEL INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE WM-SKST TO AR-EKP.
           REWRITE AR-SATZ.
           GO Z.
       G.  READ LAGARTIK INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE WM-HKST TO LG-PREIS.
           MOVE WM-SKST TO LG-EKP.
           REWRITE LG-SATZ.
       X.  CLOSE ARTIKEL.
           OPEN INPUT ARTIKEL.
       Z.  EXIT.
      *********************************************** Kalkulationsbild *
       KALKBILD SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Materialeinsatz                               ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Lohnkosten                         min        ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Produktionsgemeinkosten            min        ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "= Herstellkosten" with highlight AT VDU-REL.
           ADD 17 TO VDU-REL.
           DISPLAY " engros / detail ≥           ≥" AT VDU-REL.
           ADD 83 TO VDU-REL.
           DISPLAY "Retourwarenanteil      % /      % ≥           ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Vertriebskosten        % /      % ≥           ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "Verwaltungkosten       % /      % ≥           ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "= Selbstkosten" with highlight AT VDU-REL.
           ADD 34 TO VDU-REL.
           DISPLAY "≥           ≥" AT VDU-REL.
           ADD 66 TO VDU-REL.
           DISPLAY "Risiko u. Gewinn       % /      % ≥           ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "= Netto-Verkaufspreis" with highlight AT VDU-REL.
           ADD 34 TO VDU-REL.
           DISPLAY "≥           ≥" AT VDU-REL.
           ADD 66 TO VDU-REL.
           DISPLAY "     % Umsatzsteuer               ≥           ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "= Verkaufspreis                   ≥           ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "effekt. Verkaufspreis             ≥           ≥"
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "= Deckung" with highlight AT VDU-REL.
           ADD 34 TO VDU-REL.
           DISPLAY "≥          %≥         %" AT VDU-REL.
       Z.  EXIT.
      *****************************************************************
       MENGEN SECTION.
       A.  CALL "CAUP" USING "0702041434020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Mengeneinheiten " with highlight AT VDU-REL.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.  Text        Nr.  Text   " AT VDU-REL.
           ADD 0303 VDU-ECK GIVING VDU-REL.
           DISPLAY "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ-" AT VDU-REL.
           MOVE 0 TO WH-ART.
       C.  ADD 1 WH-ART GIVING WX.
           ADD 03 VDU-EL WX GIVING VDU-L.
           IF WX < 11 ADD 02 VDU-EP GIVING VDU-P
                 ELSE ADD 19 VDU-EP GIVING VDU-P
                      SUBTRACT 10 FROM VDU-L.
           MOVE WH-ART TO WD-POS.
           DISPLAY WD-POS AT VDU-LP.
           ADD 6 TO VDU-P.
           DISPLAY WT-MEH(WX) with highlight AT VDU-LP.
           ADD 1 TO WH-ART.
           IF WX < 20 ADD 1 TO WX GO C.
           MOVE 1 TO WX.
       Z.  EXIT.
      ************************************************ Reorg Lagerorte *
       REORG SECTION.
       A.  CALL "CAUP" USING "0707100360000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Reorg Lagerorte " with highlight AT VDU-LP.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
           CALL "CAUP" USING "1002330000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "- bitte warten -" with blink AT VDU-LP.
           ADD 229 VDU-ECK GIVING VDU-LP.
           MOVE HIGH-VALUE TO LG-LAGER.
       E.  MOVE 999,99 TO LG-SORT WV-SORT.
           START LAGARTIK KEY < LG-BKEY INVALID GO X.
       G.  READ LAGARTIK PREVIOUS AT END GO M.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE LG-NUM TO WD-ART.
           DISPLAY WD-ART with highlight AT VDU-LP.
           IF WV-LAGER = SPACE MOVE LG-LAGER TO WV-LAGER
               MOVE LG-LAGER TO WV-LAGER.
           IF WV-LAGER NOT = LG-LAGER MOVE 999,99 TO WV-SORT.
           MOVE WV-SORT TO LG-SORT.
           REWRITE LG-SATZ.
           ADD -,01 TO WV-SORT.
           GO G.
      *---------------------------------------------> neue Sortierung <-
       M.  MOVE LOW-VALUE TO LG-LAGER WV-LAGER.
           MOVE 0 TO LG-SORT.
           MOVE 1 TO WV-SORT.
           START LAGARTIK KEY NOT < LG-BKEY INVALID GO X.
       Q.  READ LAGARTIK NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE LG-NUM TO WD-ART.
           DISPLAY WD-ART with highlight AT VDU-LP.
           IF WV-LAGER = LOW-VALUE MOVE LG-LAGER TO WV-LAGER.
           IF WV-LAGER NOT = LG-LAGER MOVE 1 TO WV-SORT
               MOVE LG-LAGER TO WV-LAGER.
           MOVE WV-SORT TO LG-SORT.
           REWRITE LG-SATZ.
           ADD 1 TO WV-SORT.
           GO Q.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
