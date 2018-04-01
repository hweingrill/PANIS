      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PANDEBAN.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEDAU.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANDAU.CPY.
       COPY PANFAKT.CPY.
       COPY PANLIEF.CPY.
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
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WK-ANR                  PIC 9         VALUE ZERO.
           03  WH-DEKEY                PIC X(6).
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
      *----------------------------------------> Konditionentabelle <--
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WD-TG                   PIC ZZ9.
           03  WD-FNR                  PIC ZZ9.
           03  WD-PROZ                 PIC Z9,9.
           03  WD-PZ                   PIC ZZ9,9.
           03  WD-ART                  PIC 9999.
           03  WD-FAK                  PIC 9.
           03  WD-STAT                 PIC 9.
           03  WD-FILNR                PIC Z(5).
           03  WD-GLN                  PIC Z(13).
           03  WL-ANZ                  PIC 9.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13.
      *-----------------------------------------> Bildschirmausgaben <-
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WM-MERKER               PIC 9       COMP   VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WD-X                    PIC X.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "PANDEBIT.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 60 PERFORM DEBIT GO Z.
           IF WL-CA = 61 PERFORM DEBIT GO Z.
           IF WL-CA = 99 PERFORM SERIEN GO Z.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
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
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION.
       A.  MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
       Z.  EXIT.
      ******************************************************************
       DEBIT SECTION.
       A.  CALL "CAUP" USING "0710011380000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Verwaltung Kunden " with highlight AT VDU-REL.
       C.  ADD 202 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr.:" AT VDU-REL.
           IF WL-CA = 61 GO P.
           DISPLAY "<ret>= Anl./éndg., alpha+<ret>= suchen, <esc>= Ende"
               AT 2301.
           DISPLAY "Nr.+<tab>=nÑchste freie Kd.Nr." AT 2401
               " 1-20 nur fÅr Touren verwenden" with highlight AT 0000.
           CALL "CAUP" USING "1002085005" WH-CREG.
           ADD 208 VDU-ECK GIVING VDU-REL.
           DISPLAY "     " AT VDU-REL.
           IF ESC GO X.
           IF FEHLER GO C.
           IF RET GO M.
           IF WH-NUM = 0 MOVE HIGH-VALUE TO DE-KEY
           else COMPUTE DE-KTONR = WH-NUM * 10 + 2.
           START DEBITOR KEY < DE-KEY INVALID MOVE 2 TO DE-KTONR GO G.
           READ DEBITOR PREVIOUS IGNORE LOCK AT END MOVE 2 TO DE-KTONR.
       G.  COMPUTE WH-WERT = (DE-KTONR - 2) / 10.
           IF WH-WERT = 99999
               DISPLAY "hîchste mîgliche Nr. schon vergeben" AT 2401
               GO C.
           ADD 1 WH-WERT GIVING WH-NUM WH-WERT.
       M.  MOVE 0 TO WM-MERKER.
           IF WH-NUM = 0 CALL "PANANZ" USING "10DEBSUCH" WH-CREG
               CANCEL "PANANZ".
           IF WH-NUM = 0 GO C.
           MULTIPLY WH-WERT BY 10 GIVING WH-NUM.
           ADD 2 WH-NUM GIVING DE-KTONR.
       P.  READ DEBITOR INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO P.
           PERFORM DEB-ANL.
           UNLOCK DEBITOR.
           IF WL-CA = 61 GO X.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK DEBITOR.
       Z.  EXIT.
      *****************************************************************
       BEDTX SECTION.
       A.  DISPLAY "<esc>= Abbruch, <>= zurÅck" AT 2301.
       Z.  EXIT.
      *****************************************************************
       DEB-ANL SECTION.
       A.  ADD 202 VDU-ECK GIVING VDU-REL.
           DISPLAY "Konto-Nr.: " AT VDU-REL.
           COMPUTE WH-WERT = DE-KTONR / 10.
           MOVE WH-WERT TO WD-KTO.
           ADD 214 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-KTO with highlight AT VDU-REL.
           ADD 302 VDU-ECK GIVING VDU-REL.
           DISPLAY "Anrede:" AT VDU-REL.
           ADD 402 VDU-ECK GIVING VDU-REL.
           DISPLAY "Bezeichnung:" AT VDU-REL.
           ADD 225 VDU-ECK GIVING VDU-REL.
           DISPLAY "Kurzname:" AT VDU-REL.
           ADD 335 VDU-ECK GIVING VDU-REL.
           DISPLAY "Kdgrp:" AT VDU-REL.
           ADD 250 VDU-ECK GIVING VDU-REL.
           DISPLAY "EAN-Etiketten....:" AT VDU-REL.
           ADD 350 VDU-ECK GIVING VDU-REL.
           DISPLAY "Kontogruppe......:" AT VDU-REL.
           ADD 450 VDU-ECK GIVING VDU-REL.
           DISPLAY "Inkassoliste.....:" AT VDU-REL.
           ADD 550 VDU-ECK GIVING VDU-REL.
           DISPLAY "Rabatt:     % EAN:" AT VDU-REL.
           ADD 650 VDU-ECK GIVING VDU-REL.
           DISPLAY "Zahlungskondition:" AT VDU-REL.
           ADD 902 VDU-ECK GIVING VDU-REL.
           DISPLAY "Preisdatum: " AT VDU-REL.
           ADD 925 VDU-ECK GIVING VDU-REL.
           DISPLAY "Retour:      " AT VDU-REL.
           ADD 941 VDU-ECK GIVING VDU-REL.
           DISPLAY "Basis:         Bonus:" AT VDU-REL.
           ADD 1002 VDU-ECK GIVING VDU-REL.
           DISPLAY "Re.-Ktonr:           Tour:         Stat.:    Fakt.-A
      -            "rt:         Raster:" AT VDU-REL.
           ADD 1102 VDU-ECK GIVING VDU-REL.
           DISPLAY "Fil.Nr.:        GLN-Nr.:" AT VDU-REL.
           ADD 1142 VDU-ECK GIVING VDU-REL.
           DISPLAY "Uid.Nr.:            " AT VDU-REL.
           ADD 1202 VDU-ECK GIVING VDU-REL.
           DISPLAY "Anmerkung:" AT VDU-REL.
           IF ANLAGE MOVE DE-KEY TO WH-DEKEY
               INITIALIZE DE-SATZ
               MOVE WH-DEKEY TO DE-KEY
               MOVE 100 TO DE-RET
               MOVE 1 TO WY GO C.
           ADD 311 VDU-ECK GIVING VDU-REL.
           MOVE DE-ANR TO WK-ANR.
           DISPLAY WK-ANR AT VDU-REL.
           ADD 1 DE-ANR GIVING WK-ANR.
           ADD 315 VDU-ECK GIVING VDU-REL.
           DISPLAY WT-TXT(WK-ANR) with highlight AT VDU-REL.
           MOVE 5 TO WL-AZ.
           ADD 415 VDU-ECK GIVING VDU-REL.
           MOVE VDU-REL TO WL-VP.
           MOVE VDU-RL TO VDU-L.
           MOVE DE-BEZ TO WT-BEZ.
           PERFORM HOLADR.
           MOVE 1 TO WR.
       B.  DISPLAY WR-ADR(WR) WITH SIZE 34 HIGHLIGHT AT VDU-REL.
           IF WR < 5 ADD 1 TO WR ADD 100 TO VDU-REL GO B.
           ADD 235 VDU-ECK GIVING VDU-REL.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-REL.
           ADD 342 VDU-ECK GIVING VDU-REL.
           MOVE DE-FNR TO WD-FNR.
           DISPLAY WD-FNR with highlight AT VDU-REL.
           ADD 269 VDU-ECK GIVING VDU-REL.
           MOVE DE-ETIK TO WD-FAK.
           DISPLAY WD-FAK with highlight AT VDU-REL "/".
           IF DE-ETIK = 0 DISPLAY "ohne  " with highlight AT 0000
                     else DISPLAY "ean-E." with highlight AT 0000.
           ADD 369 VDU-ECK GIVING VDU-REL.
           MOVE DE-KTOART TO WD-FAK.
           DISPLAY WD-FAK with highlight AT VDU-REL.
           IF DE-KTOART > 1 DISPLAY "/" AT 0000
                               "ohne UST" with highlight BLINK.
           ADD 469 VDU-ECK GIVING VDU-REL.
           MOVE DE-INKASSO TO WD-FAK.
           DISPLAY WD-FAK with highlight AT VDU-REL.
           ADD 558 VDU-ECK GIVING VDU-REL.
           MOVE DE-RAB TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT VDU-REL.
           ADD 569 VDU-ECK GIVING VDU-REL.
           DISPLAY DE-EAN with highlight AT VDU-REL.
           ADD 669 VDU-ECK GIVING VDU-REL.
           DISPLAY DE-KOND with highlight AT VDU-REL.
           PERFORM SKTO-DIS.
           ADD 915 VDU-ECK GIVING VDU-REL.
           MOVE DE-PRDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           IF DE-PRDAT = 0 DISPLAY "         " AT VDU-REL
                      else DISPLAY VDU-DATUM with highlight AT VDU-REL.
           ADD 933 VDU-ECK GIVING VDU-REL.
           MOVE DE-RET TO WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-REL "%".
           ADD 948 VDU-ECK GIVING VDU-REL.
           MOVE DE-RBASIS TO WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-REL "%".
           ADD 963 VDU-ECK GIVING VDU-REL.
           MOVE DE-BONUS TO WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-REL "%".
           ADD 1013 VDU-ECK GIVING VDU-REL.
           DIVIDE 10 INTO DE-REKTO GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-REL.
           MOVE DE-TOUR TO WD-TG.
           ADD 1028 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-TG with highlight AT VDU-REL "/".
           MOVE DE-SUB TO WD-ART.
           ADD 1032 VDU-ECK GIVING VDU-REL.
           DISPLAY WD-ART with highlight AT VDU-REL.
           ADD 1044 VDU-ECK GIVING VDU-REL.
           MOVE DE-STATIS TO WD-STAT.
           DISPLAY WD-STAT with highlight AT VDU-REL.
           ADD 1058 VDU-ECK GIVING VDU-REL.
           MOVE DE-FAKART TO WD-FAK.
           DISPLAY WD-FAK with highlight AT VDU-REL.
           ADD 1061 VDU-ECK GIVING VDU-REL.
           IF DE-FAKART < 10
               DISPLAY WT-FA(DE-FAKART) AT VDU-REL
           else DISPLAY "Sond" AT VDU-REL.
           ADD 1074 VDU-ECK GIVING VDU-REL.
           MOVE DE-RAST TO WD-FAK.
           DISPLAY WD-FAK with highlight AT VDU-REL.
           ADD 1111 VDU-ECK GIVING VDU-REL.
           IF DE-FILNR not = 0 MOVE DE-FILNR TO WD-FILNR
               DISPLAY WD-FILNR with highlight AT VDU-REL.
           ADD 1127 VDU-ECK GIVING VDU-REL.
           IF DE-GLN not = 0 MOVE DE-GLN TO WD-GLN
               DISPLAY WD-GLN with highlight AT VDU-REL.
           ADD 1151 VDU-ECK GIVING VDU-LP.
           DISPLAY DE-UID with highlight AT VDU-LP.
           ADD 1213 VDU-ECK GIVING VDU-REL.
           IF DE-ANMERK(5:2) = X"000C" MOVE SPACE TO DE-ANMERK.
           DISPLAY DE-ANMERK with highlight AT VDU-REL.
           GO X.
       C.  MOVE DE-ANR TO WH-WERT.
       D.  DISPLAY "<esc>= Abbruch, <ret-leer>= keine énderung" AT 2301.
           MOVE 2401 TO VDU-REL.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 7
               SUBTRACT 1 FROM WY GIVING WK-ANR
               DISPLAY WK-ANR AT VDU-REL ":" WT-TXT(WY)
               ADD 9 TO VDU-REL.
           CALL "CAUP" USING "1003111001" WH-CREG.
           IF ESC OR WOLI MOVE 0 TO DE-KTONR GO Z.
           IF FEHLER OR WH-WERT > 6 DISPLAY " " AT VDU-LP GO D.
           MOVE WH-WERT TO WK-ANR DE-ANR.
           DISPLAY WK-ANR AT VDU-LP.
           ADD 4 TO VDU-LP.
           ADD 1 TO WK-ANR.
           DISPLAY WT-TXT(WK-ANR) AT VDU-LP.
       E.  ADD 415 VDU-ECK GIVING VDU-LP.
           MOVE DE-BEZ TO WT-BEZ.
           PERFORM HOLADR.
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 5
               DISPLAY WR-ADR(WR) WITH SIZE 34 HIGHLIGHT AT VDU-LP
               ADD 100 TO VDU-LP.
       F.  DISPLAY "<esc>= zurÅck, <ret-leer>= keine énderung" AT 2301.
           CALL "CAUP" USING "1204150534" WH-CREG.
           MOVE WT-BEZ TO DE-BEZ.
           IF ESC GO D.
           IF WR-ADR(1) = SPACE
               DISPLAY "Bezeichnung mu· sein!" AT 2401
               PERFORM WEITER GO F.
           IF WH-X = "*" GO Z.
       G.  ADD 235 VDU-ECK GIVING VDU-REL.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-REL.
           CALL "CAUP" USING "1202350108" WH-CREG.
           IF ESC GO E.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO DE-MCODE.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
           IF WH-X = "$" GO Z.
       GA. PERFORM BEDTX.
           DISPLAY "Kundengruppe" with highlight AT 2401.
           MOVE DE-FNR TO WH-WERT.
           CALL "CAUP" USING "1003423003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO G.
           IF NOT RET GO A.
           MOVE WH-WERT TO DE-FNR.
           MOVE DE-FNR TO WD-FNR.
           DISPLAY WD-FNR with highlight AT VDU-LP.
       H.  PERFORM BEDTX.
           DISPLAY "Abweichendes GÅltigkeitsdatum der ArtikelpreisÑnderu
      -        "ng" AT 2401.
           MOVE DE-PRDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1109156006" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO GA.
           IF NOT RET GO H.
           MOVE WZ-DATUM TO DE-PRDAT.
           IF DE-PRDAT = 0 DISPLAY "         " AT VDU-LP
               ELSE DISPLAY VDU-DATUM with highlight AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, <>= zurÅck" AT 2301.
           DISPLAY "max. Retourwarenprozentsatz" AT 2401.
           MOVE DE-RET TO WH-WERT.
           CALL "CAUP" USING "1109333105" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO H.
           MOVE WH-WERT TO DE-RET WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP "%".
       J.  DISPLAY "<esc>= Abbruch, <>= zurÅck" AT 2301.
           DISPLAY "Retourwarenbonusbasis" AT 2401.
           MOVE DE-RBASIS TO WH-WERT.
           CALL "CAUP" USING "1109482105" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO I.
           MOVE WH-WERT TO DE-RBASIS WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP "%".
       K.  DISPLAY "<esc>= Abbruch, <>= zurÅck" AT 2301.
           DISPLAY "Retourwarenbonussatz" AT 2401.
           MOVE DE-BONUS TO WH-WERT.
           CALL "CAUP" USING "1109632105" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO J.
           MOVE WH-WERT TO DE-BONUS WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP "%".
       LA. MOVE DE-ETIK TO WH-WERT.
           DISPLAY "EAN-Etikett" with highlight AT 2301.
           DISPLAY "0= ohne, 1= mit EAN-Etikett" AT 2401.
           CALL "CAUP" USING "1002691001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO K.
           IF NOT RET GO LA.
           IF WH-WERT > 4 GO LA.
           MOVE WH-WERT TO DE-ETIK WD-FAK
           DISPLAY WD-FAK with highlight AT VDU-LP "/".
           IF DE-ETIK = 0 DISPLAY "ohne  " with highlight AT 0000
                     else DISPLAY "ean-E." with highlight AT 0000.
       L.  MOVE DE-KTOART TO WH-WERT.
           DISPLAY "Kontoart: 0/1 + 5= ohne UST" AT 2301.
           CALL "CAUP" USING "1003691001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO LA.
           IF NOT RET GO L.
           EVALUATE WH-WERT
               WHEN 0
               WHEN 1
               WHEN 5
               WHEN 6 MOVE WH-WERT TO DE-KTOART WD-FAK
                      DISPLAY WD-FAK with highlight AT VDU-LP
                      IF WH-WERT > 1 DISPLAY "/" AT 0000
                                       "ohne UST" with highlight BLINK
                                else DISPLAY "         " AT 0000
               WHEN OTHER GO L.
       M.  MOVE DE-INKASSO TO WH-WERT.
           DISPLAY "Inkassoliste 0: nein, 1 = ja" AT 2301.
           CALL "CAUP" USING "1004691001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO L.
           IF not RET or WH-WERT > 1 GO M.
           MOVE WH-WERT TO DE-INKASSO.
       O.  MOVE DE-RAB TO WH-WERT.
           CALL "CAUP" USING "1005582104" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO M.
           IF NOT RET GO O.
           MOVE WH-WERT TO DE-RAB WD-PROZ.
           DISPLAY WD-PROZ with highlight AT VDU-LP.
       P.  MOVE DE-EAN TO WH-WERT.
           PERFORM BEDTX.
           DISPLAY "0= keine, 1= Betriebs-EAN, 2= Barcode, 3= nur Fremda
      -        "rtikelnr." AT 2401.
           CALL "CAUP" USING "1005691001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO O.
           IF NOT RET GO P.
           MOVE WH-WERT TO DE-EAN.
           DISPLAY DE-EAN with highlight AT VDU-LP.
       Q.  MOVE DE-KOND TO WH-WERT.
           CALL "CAUP" USING "1006691001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO P.
           IF NOT RET GO Q.
           MOVE WH-WERT TO DE-KOND.
           DISPLAY DE-KOND with highlight AT VDU-LP.
           PERFORM SKTO-DIS.
       R.  CALL "CAUP" USING "1010135005" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO Q.
           IF NOT RET GO R.
           IF WH-NUM = 0;
               IF DE-REKTO = 0 COMPUTE WH-WERT = (DE-KTONR - 2) / 10
           ELSE COMPUTE WH-WERT = (DE-REKTO - 2) / 10.
           COMPUTE WH-WERT = WH-WERT * 10 + 2.
           MOVE WH-WERT TO DE-REKTO.
           COMPUTE WH-WERT = WH-WERT / 10.
           MOVE WH-WERT TO WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       S.  MOVE DE-TOUR TO WH-WERT.
           CALL "CAUP" USING "1010282002" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO R.
           IF NOT RET GO S.
           IF WH-WERT = 0 OR WH-WERT > 20 GO S.
           MOVE WH-WERT TO DE-TOUR WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP "/".
       T.  DISPLAY "Tourfolge" AT 2401.
           PERFORM BEDTX.
           MOVE DE-SUB TO WH-WERT.
           CALL "CAUP" USING "1010324004" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO S.
           IF NOT RET GO T.
           MOVE WH-WERT TO DE-SUB WD-ART.
           DISPLAY WD-ART with highlight AT VDU-LP.
       U.  MOVE DE-STATIS TO WH-WERT.
           PERFORM BEDTX.
           DISPLAY "Stat.: 0 = nein, 1 = ja, (+2 nur Bestellg.)"
               AT 2401.
           CALL "CAUP" USING "1010441001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO T.
           IF NOT RET OR WH-WERT > 3 GO U.
           MOVE WH-WERT TO WD-STAT DE-STATIS.
           DISPLAY WD-STAT with highlight AT VDU-LP.
       V.  MOVE DE-FAKART TO WH-WERT.
           DISPLAY "1= Mon, 2= Mon, 3= Wo, 4= Wo, 5= hMo, 6= Sof"
               AT 2401.
           PERFORM BEDTX.
           CALL "CAUP" USING "1010582002" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO U.
           IF NOT RET GO V.
           MOVE WH-WERT TO DE-FAKART WD-FAK.
           IF DE-FAKART < 11 and > 6 GO V.
           IF DE-FAKART = 0 GO V.
           DISPLAY WD-FAK with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           IF DE-FAKART < 10
               DISPLAY WT-FA(DE-FAKART) AT VDU-LP
           else DISPLAY "Sond" with highlight foreground-color 6
                   AT VDU-LP.
       W.  DISPLAY "0= ohne Raster, 1= mit Raster, 5= Fil.-Lfsch."
               AT 2401.
           PERFORM BEDTX.
           MOVE DE-RAST TO WH-WERT.
           CALL "CAUP" USING "1010741001" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO V.
           IF NOT RET GO W.
           MOVE WH-WERT TO DE-RAST.
           DISPLAY DE-RAST AT VDU-LP.
           IF DE-RAST = 2; IF DE-KTONR = DE-REKTO
               DISPLAY "Raster oder Re-Ktonr. falsch" AT 2401
               PERFORM WEITER GO W.
       WQ. PERFORM ANMERK.
           IF AUF OR WOLI GO W.
           IF ESC GO Z.
       X.  DISPLAY "<ret>= ok, <esc>= Abbruch, <Entf>= lîschen, < >= Ñnd
      -        "ern, < />= zurÅck < >" AT 2301.
           CALL "CAUP" USING "0023730000" WH-CREG.
           IF AUF or WOLI GO WQ.
           IF AB GO C.
           IF ESC  GO Z.
           IF ENTF GO Y.
           IF NOT RET GO X.
           MOVE 99 TO DE-ANLMERK.
           IF ANLAGE WRITE DE-SATZ GO Z.
           IF AEND REWRITE DE-SATZ.
           PERFORM UPDAT.
           GO Z.
       Y.  DISPLAY "Wirklich lîschen? <ret>= Abbruch, <Entf>= lîschen"
               AT 2301.
           CALL "CAUP" USING "0023501001" WH-CREG.
           IF RET or ESC GO X.
           IF NOT ENTF GO Y.
           DELETE DEBITOR.
           DISPLAY "gelîscht!" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       PRFZ SECTION.
       B.  ADD WR-EAN(2) WR-EAN(4) WR-EAN(6) WR-EAN(8) WR-EAN(10)
               WR-EAN(12) GIVING WL-ANZ.
           MULTIPLY 3 BY WL-ANZ.
           ADD WR-EAN(1) WR-EAN(3) WR-EAN(5) WR-EAN(7) WR-EAN(9)
               WR-EAN(11) TO WL-ANZ.
           DIVIDE 10 INTO WL-ANZ GIVING WL-ANZ REMAINDER WL-ANZ.
           SUBTRACT WL-ANZ FROM 10 GIVING WR-EAN(13).
       Z.  EXIT.
      ******************************************************************
       ANMERK SECTION.
       A.  IF WOLI OR AUF GO E.
       B.  MOVE DE-FILNR TO WH-WERT WD-FILNR.
           ADD 1111 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-FILNR with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, < >= zurÅck" AT 2301.
           DISPLAY "Filial- oder Mitgliedsnummer" with highlight
               AT 2401.
           CALL "CAUP" USING "1011115005" WH-CREG.
           EVALUATE TRUE
               WHEN ESC
               WHEN WOLI
               WHEN AUF GO Z
               WHEN RET MOVE WH-WERT TO DE-FILNR WD-FILNR
                        DISPLAY WD-FILNR with highlight AT VDU-LP
               WHEN OTHER GO B.
       C.  MOVE DE-GLN TO WD-GLN WH-EAN.
           ADD 1127 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-GLN with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, < >= zurÅck, <Entf>= lîschen"
               AT 2301.
           DISPLAY "ean-Betriebsnummer (13-stellig, letzte Stelle immer
      -        "0 oder Prfz." with highlight AT 2401.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "101127D013" WH-CREG.
           EVALUATE TRUE
               WHEN ESC
               WHEN WOLI
               WHEN ENTF MOVE 0 TO WH-EAN DE-GLN
                         MOVE DE-GLN TO WD-GLN
                         DISPLAY WD-GLN with highlight AT VDU-LP
               WHEN AUF GO Z
               WHEN RET IF WH-WERT not = 0 MOVE WH-NUM TO WH-EAN
                        end-if
                        PERFORM PRFZ
                        MOVE WH-EAN TO DE-GLN WD-GLN
                        DISPLAY WD-GLN with highlight AT VDU-LP
               WHEN OTHER GO A.
       D.  ADD 1151 VDU-ECK GIVING VDU-LP.
           DISPLAY DE-UID with highlight AT VDU-LP.
           MOVE DE-UID TO WT-TX.
           DISPLAY "<esc>= Abbruch, < >= zurÅck" AT 2301.
           DISPLAY "UID-Nr." with highlight AT 2401.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1211510112" WH-CREG.
           EVALUATE TRUE
               WHEN ESC GO Z
               WHEN WOLI
               WHEN AUF GO C
               WHEN RET NEXT SENTENCE
               WHEN OTHER GO D.
           MOVE WT-TX TO DE-UID.
           DISPLAY DE-UID with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, < >= zurÅck" AT 2301.
           DISPLAY "Telefon-Nr. / Ansprechspartner etc" AT 2401.
           MOVE DE-ANMERK TO WT-TX.
           ADD 1213 VDU-ECK GIVING VDU-LP.
           DISPLAY DE-ANMERK with highlight AT VDU-LP.
           CALL "CAUP" USING "1212130160" WH-CREG.
           EVALUATE TRUE
               WHEN ESC GO Z
               WHEN WOLI
               WHEN AUF GO D
               WHEN RET MOVE WT-TX TO DE-ANMERK
                   DISPLAY DE-ANMERK with highlight AT VDU-LP
                   GO Z
               WHEN OTHER GO E.
       Z.  EXIT.
      ********************************************************* update *
       UPDAT SECTION.
       A.  MOVE DE-KTONR TO FA-KTONR DA-KTONR LF-KTONR
           MOVE 0 TO FA-ARNUM DA-ARNUM LF-NUM LF-ARNUM.
           DISPLAY "Update Rasterdatei" AT 2301.
           DISPLAY "- bitte warten -" AT 2330.
           START FAKTDAT KEY NOT < FA-KEY INVALID GO V.
       S.  READ FAKTDAT NEXT AT END GO T.
           IF ZUGRIF PERFORM BESETZT GO S.
           IF FA-KTONR NOT = DE-KTONR GO T.
           IF DE-FAKART = FA-ART GO S.
           MOVE DE-FAKART TO FA-ART.
           REWRITE FA-SATZ.
           GO S.
       T.  START DAUER KEY NOT < DA-KEY INVALID KEY GO V.
           DISPLAY "Update Dauerstellungen" AT 2301.
       U.  READ DAUER NEXT AT END GO V.
           IF ZUGRIF PERFORM BESETZT GO U.
           IF DA-KTONR NOT = DE-KTONR GO V.
           MOVE DE-FAKART TO DA-FART.
           MOVE DE-TOUR TO DA-TOUR.
           MOVE DE-SUB TO DA-SUB.
           MOVE DE-EAN TO DA-EAN.
           MOVE DA-GRM TO WF-GRM.
           MOVE DE-STATIS TO WF-SON.
           MOVE WF-GRM TO DA-GRM.
           REWRITE DA-SATZ.
           GO U.
       V.  START LFSCHEIN KEY NOT < LF-KEY INVALID KEY GO Z.
           DISPLAY "Update Lieferscheine  " AT 2301.
       W.  READ LFSCHEIN NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO W.
           IF LF-KTONR NOT = DE-KTONR GO Z.
           MOVE DE-FAKART TO LF-FART.
           MOVE DE-TOUR TO LF-TOUR.
           MOVE DE-EAN TO LF-EAN.
           MOVE DE-SUB TO LF-SUB.
           MOVE LF-GRM TO WF-GRM.
           MOVE DE-STATIS TO WF-SON.
           MOVE WF-GRM TO LF-GRM.
           REWRITE LF-SATZ.
           GO W.
       Z.  EXIT.
      ******************************************************************
       SKTO-DIS SECTION.
       A.  ADD 750 VDU-ECK GIVING VDU-REL.
           IF DE-KOND = 0
               DISPLAY "keine Skontierung    " AT VDU-REL
               ADD 100 TO VDU-REL
               DISPLAY "                     " AT VDU-REL
               GO Z
           else MOVE WT-KONDIT(DE-KOND) TO WH-KK.
           DISPLAY "                             " AT VDU-REL.
           ADD 100 TO VDU-REL.
           DISPLAY "                      " AT VDU-REL.
           SUBTRACT 100 FROM VDU-REL.
           IF DE-KOND = 9999999 MOVE 0 TO DE-KOND WH-KK.
           DISPLAY " %-1 Tg. %-2 Tg. TgN. " with reverse-video
      *              2,0  30 2,0  30 120
               AT VDU-REL.
           ADD 100 TO VDU-REL.
           IF WH-SK1 NOT = 0 MOVE WH-SK1 TO WD-PZ
               DISPLAY WD-PZ(2:) with highlight AT VDU-REL
               MOVE WH-TG1 TO WD-TG
               ADD 5 TO VDU-REL
               DISPLAY WD-TG with highlight AT VDU-REL
               ADD 3 TO VDU-REL
           else ADD 8 TO VDU-REL.
           IF WH-SK2 NOT = 0 MOVE WH-SK2 TO WD-PZ
               DISPLAY WD-PZ(2:) with highlight AT VDU-REL
               MOVE WH-TG2 TO WD-TG
               ADD 5 TO VDU-REL
               DISPLAY WD-TG with highlight AT VDU-REL
               ADD 4 TO VDU-REL
           else ADD 9 TO VDU-REL.
           IF WH-TGN NOT = 0 MOVE WH-TGN TO WD-TG
               DISPLAY WD-TG with highlight AT VDU-REL.
       Z.  EXIT.
      *****************************************************************
       SKTO-DIS-ALT SECTION.
       A.  IF DE-KOND = 0
               DISPLAY "keine Skontierung            " AT VDU-REL
               ADD 109 TO VDU-REL
               DISPLAY "             " AT VDU-REL GO Z.
           DISPLAY "                             " AT VDU-REL.
           ADD 109 TO VDU-REL.
           DISPLAY "             " AT VDU-REL.
           SUBTRACT 109 FROM VDU-REL.
           MOVE WT-KONDIT(DE-KOND) TO WH-KK.
           MOVE WH-TG1 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-REL.
           ADD 4 TO VDU-REL.
           DISPLAY "Tg./" AT VDU-REL.
           ADD 4 TO VDU-REL.
           MOVE WH-SK1 TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT VDU-REL.
           ADD 4 TO VDU-REL.
           DISPLAY "% " AT VDU-REL.
           ADD 4 TO VDU-REL.
           MOVE WH-TG2 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-REL.
           ADD 4 TO VDU-REL.
           DISPLAY "Tg./" AT VDU-REL.
           ADD 4 TO VDU-REL.
           MOVE WH-SK2 TO WD-PROZ.
           DISPLAY WD-PROZ with highlight AT VDU-REL.
           ADD 4 TO VDU-REL.
           DISPLAY "% " AT VDU-REL.
           ADD 100 TO VDU-REL.
           SUBTRACT 16 FROM VDU-REL.
           MOVE WH-TGN TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-REL.
           ADD 4 TO VDU-REL.
           DISPLAY "Tage netto" AT VDU-REL.
       Z.  EXIT.
      ************************************************* SerienÑnderung *
       serien section.
       A.  CALL "CAUP" USING "0710100662000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " KundenserienÑnderung " AT VDU-LP.
           ADD 403  VDU-ECK GIVING VDU-LP.
           DISPLAY "Kd-Nr.:" AT VDU-LP.
           MOVE 0 TO DE-KTONR.
       B.  START DEBITOR KEY NOT < DE-KEY INVALID GO C.
           GO G.
       C.  COMPUTE WH-WERT = DE-KTONR / 10.
           CALL "CAUP" USING "1004115005" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WH-WERT = 0 GO C.
           COMPUTE DE-KTONR = WH-WERT * 10 + 2.
           GO B.
       G.  READ DEBITOR NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO G.
       H.  ADD 403  VDU-ECK GIVING VDU-LP.
           DISPLAY "Kd-Nr.:" AT VDU-LP.
           COMPUTE WH-WERT = DE-KTONR / 10.
           MOVE WH-WERT TO WD-KTO.
           ADD 411 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO AT VDU-LP.
           ADD 9 TO VDU-LP.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY WT-BEZ(1:40) with highlight AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Retour:      " AT VDU-LP.
           ADD 8 TO VDU-LP.
           IF DE-RET = 0 MOVE 100 TO DE-RET.
           MOVE DE-RET TO WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP "%".
       K.  DISPLAY "<>= zur Kd-Nr., <ret>= Ret. %-Satz, <esc>= Ende, <#
      -        ">= 15%, < >= zurÅck" AT 2301.
           MOVE DE-RET TO WH-WERT.
           CALL "CAUP" USING "1005113104" WH-CREG.
           IF KIST MOVE 15 TO WH-WERT
               SET RET TO TRUE.
           IF ESC GO X.
           IF WOLI GO C.
           IF AUF GO Q.
           MOVE WH-WERT TO DE-RET.
           MOVE DE-RET TO WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP "%".
           REWRITE DE-SATZ.
           CALL "CAUP" USING "17SCROL" WH-CREG.
           CALL "CAUP" USING "17SCROL" WH-CREG.
           GO G.
       Q.  CALL "CAUP" USING "22SCROL" WH-CREG.
           CALL "CAUP" USING "22SCROL" WH-CREG.
       R.  READ DEBITOR PREVIOUS AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO R.
           GO H.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       z.  exit.
