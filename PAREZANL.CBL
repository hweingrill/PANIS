      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PAREZANL.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  PC.
       OBJECT-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEART.CPY.
           COPY PARSEART.CPY.
           COPY PARSEREZ.CPY.
           SELECT SUMMEN     ASSIGN TO "PANESUMM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SU-KEY
                             ALTERNATE RECORD KEY SU-AKEY
                             FILE STATUS IS WF-STATUS.
           SELECT ETIKETT    ASSIGN TO "PANETIK.DAT"
                             ORGANIZATION RELATIVE ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANFDART.CPY.
       COPY PARFDART.CPY.
       COPY PARREZEP.CPY.
      *********************************** Touren- u. Backzettelsummen *
       FD  SUMMEN       external       LABEL RECORD STANDARD
                                       RECORD 128 CHARACTERS.
       01  SU-SATZ.
           03  SU-AKEY.
               05 SU-GRP               PIC 9999    COMP.
               05 SU-ANUM              PIC 9(6)    COMP.
           03  SU-KEY.
               05 SU-TB                PIC 99      COMP.
               05 SU-NUM               PIC 9(6)    COMP.
           03  SU-BEZ                  PIC X(25).
           03  SU-MEH                  PIC 99      COMP.
           03  SU-NK                   PIC 99      COMP.
           03  SU-GVZ                  PIC 99      COMP.
           03  SU-MG                   PIC S9(7)   COMP-3 OCCURS 23.
      *****************************************************************
       FD  ETIKETT                     LABEL RECORD STANDARD
                                       RECORD 224 CHARACTERS.
       01  ET-SATZ.
           03  ET-NUM                  PIC 9(4)        COMP.
           03  ET-ZEIL                            OCCURS 7.
               05 ET-BEZ               PIC X(30).
               05 ET-MERK              PIC 99          COMP.
           03  FILLER                  PIC X(5).
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
       01  WI-SATZ.
           03  WI-ATTR                 PIC X(2000).
           03  WI-ZEICH                PIC X(2000).
           03  WI-ERFTAB               PIC X(120).
           03  WT-ERFTAB.
               05 WT-WI                PIC 99        COMP  OCCURS 16.
               05 XX                   PIC 99        COMP.
               05 XS                   PIC 99        COMP.
               05 WI                   PIC 99        COMP.
               05 WS                   PIC 99        COMP.
           03  WX-PARM.
               05 WX-L                 PIC 99        COMP-X.
               05 WX-P                 PIC 99        COMP-X.
           03  WX-PS REDEFINES WX-PARM PIC 9999      COMP.
           03  WX-LG                   PIC 9999      COMP.
           03  WX-ATTRIB               PIC X(80).
           03  WX-LNG                  PIC 9(4)      COMP-X.
       01  WH-REG.
           03  WH-RESATZ               PIC X(800).
           03  VDU-AB                  PIC 9999.
           03  IX                      PIC 99        COMP.
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WV-ARNUM                PIC 9(4)      COMP.
           03  WH-KOMP                 PIC X(34).
           03  WK-NUM                  PIC 9(8)V99   COMP.
           03  WH-MENGE                PIC S9(5)     COMP-3.
           03  WM-MERKER               PIC 9         COMP-3.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WD-KOMPO                PIC Z.ZZ9.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MIN                  PIC ZZ9,99-.
           03  WD-GEW                  PIC ZZ9,99-.
           03  WD-POS                  PIC ZZ9.
           03  WD-ART                  PIC ZZZ9.
           03  WM-BS                   PIC 99.
           03  WD-TEMP                 PIC Z9,9.
           03  WD-ZT                   PIC ZZ.
      *----------------> Maschine, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WD-TEIG                 PIC ZZ.ZZ9-.
           03  WD-RPM                  PIC ZZZ.ZZ9-.
           03  WD-GES                  PIC ZZZZ.ZZ9-.
           03  WD-KZ                   PIC 9.
           03  WD-TG                   PIC ZZ9.
           03  WD-BLECH                PIC Z9-.
           03  WD-BET                  PIC ZZZ.ZZ9,99-.
           03  WD-PREIS                PIC Z.ZZ9,99-.
           03  WH-PREIS                PIC 9(5)V999   COMP-3.
           03  WH-ATG                  PIC S9(7)V9(4) COMP-3.
           03  WH-MEH                  PIC 99         COMP.
      *--------------------------------------> Adapt-feld aus FIBU <-- 
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
       COPY PAREXT.CPY.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON LAGARTIK.
       A.  CALL "CADECL" USING "PARARTIK.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON SUMMEN.
       A.  CALL "CADECL" USING "PANESUMM.DAT" WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON ETIKETT.
       A.  CALL "CADECL" USING "PANETIK.DAT "  WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 20 PERFORM RPT.
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
           IF VDU-L < 21 GO Z.
       B.  CALL "CAUP" USING "1408022170000" WH-CREG.
           SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ****************************************** Rezepturenverwaltung *
       RPT SECTION.
       A.  MOVE "   Rezepturen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "0705011871020" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Material u. Zeiten " with foreground-color 2
               AT VDU-LP.
           DISPLAY "Pos Komp.  Bezeichnung            Menge Meh     Prei
      -        "s     Betrag TA" AT 0603.
           DISPLAY ALL "ƒ" with foreground-color 3 SIZE 69 AT 0702.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
       C.  CALL "CAUP" USING "1303010380" WH-CREG.
           CALL "CAUP" USING "1304010480" WH-CREG.
           CALL "CAUP" USING "1306722480" WH-CREG.
           DISPLAY "Rpnr:" AT 0301.
           DISPLAY "<ret>= anlegen/Ñndern, <ret+alpha> = suchen, <esc>=
      -        "Ende" AT 2301.
           DISPLAY "< >= Beschreibung, < >= Hilfe" AT 2401.
           IF RE-NUM NOT = 0 DISPLAY ", <#>= zuletzt bearb. Rpnr."
               AT 2400.
           CALL "CAUP" USING "0003074009" WH-CREG.
           IF KIST GO D.
           IF HELP CALL "CAUP" USING "190013001401" WH-CREG GO C.
           IF ENTF SET RET TO TRUE.
           IF AB SET RET TO TRUE.
           IF ESC OR WOLI GO Z.
           IF NOT RET GO C.
           IF WH-NUM = 0 CALL "PANANZ" USING "40RPSUCH" WH-CREG
               CANCEL "PANANZ" GO D.
           MOVE WS-TASTE TO WX-TASTE.
           MOVE WH-NUM TO RE-NUM.
       D.  IF RE-NUM = 0 GO C.
           MOVE RE-NUM TO WD-NUM.
           MOVE 0 TO WM-MERKER.
           MOVE 7 TO VDU-L.
       E.  READ REZEPT INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WM-MERKER = 1 PERFORM RP-ANL; IF ESC GO B ELSE GO F.
           MOVE RE-ART TO WH-TART.
           PERFORM RPTKO.
           IF ENTF GO G.
           IF AB MOVE RE-BEZ TO WT-TX
               CALL "PARHINW" USING "42MOD" WH-CREG
               CANCEL "PARHINW" GO B.
       F.  PERFORM RPANZ.
       G.  MOVE RE-BEZ TO WT-TX.
           CALL "PARHINW" USING "45MOD" WH-CREG.      *> Modul anzeigen
           CANCEL "PARHINW".
           PERFORM RPZEIL.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           GO A.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************************** duplizieren *
       RPDUPLO SECTION.
       A.  SET RET TO TRUE.
           MOVE RE-NUM TO WV-ARNUM.
           CALL "CAUP" USING "0710100330000" WH-CREG.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "kopieren von Rpnr.:" AT VDU-LP.
           DISPLAY "<ret>= alte Rezepturennummer, <esc/#>= Abbruch"
              AT 2301.
           CALL "CAUP" USING "1002234004" WH-CREG.
           IF ESC OR WOLI OR KIST SET ESC TO TRUE GO F.
           IF NOT RET GO B.
       C.  MOVE WH-NUM TO RE-NUM.
           IF RE-NUM = 0 GO B.
           MOVE RE-BEZ TO WT-TX.
           MOVE RE-TEIL TO WH-MENGE.
       E.  READ REZEPT IGNORE LOCK INVALID PERFORM NO-REC GO B.
       F.  MOVE WV-ARNUM TO RE-NUM.
           MOVE WT-TX TO RE-BEZ.
           MOVE WH-MENGE TO RE-TEIL.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************* anzeigen Temperaturfenster *
       ANZ-TEMP SECTION.
       A.  CALL "CAUP" USING "0709351339000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Wasser-/Teigtemperatur " with
               reverse-video foreground-color 2 AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Temperatur:     ¯C" AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kneter:" AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "1. Gang       Min    Sek." AT VDU-LP.
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY "2. Gang       Min    Sek." AT VDU-LP.
           ADD 703 VDU-ECK GIVING VDU-LP.
           DISPLAY "Knetpause     Min    Sek." AT VDU-LP.
           ADD 803 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nachkneten   . Gang     Min    Sek." AT VDU-LP.
           ADD 1003 VDU-ECK GIVING VDU-LP.
           DISPLAY "max. Chargengew...:        kg M:   " AT VDU-LP.
           ADD 1103 VDU-ECK GIVING VDU-LP.
           DISPLAY "Chargenteiler kg/%:" AT VDU-LP.
           ADD 315 VDU-ECK GIVING VDU-LP.
           MOVE RE-TEMP TO WD-TEMP.
           DISPLAY WD-TEMP with highlight AT VDU-LP.
           ADD 411 VDU-ECK GIVING VDU-LP.
           MOVE RE-KNET TO WD-ZT.
           DISPLAY WD-ZT with highlight AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 4
               COMPUTE VDU-LP = WX * 100 + 414 + VDU-ECK
               IF WX = 4 ADD 10 TO VDU-P
               end-if
               IF RE-GMIN(WX) not = 0 or RE-GSEK(WX) not = 0
                   MOVE RE-GMIN(WX) TO WD-ZT
                   DISPLAY WD-ZT with highlight AT VDU-LP
                   ADD 7 TO VDU-LP
                   MOVE RE-GSEK(WX) TO WD-ZT
                   DISPLAY WD-ZT with highlight AT VDU-LP.
           IF RE-GNR not = 0 ADD 815 VDU-ECK GIVING VDU-LP
               MOVE RE-GNR TO WD-KZ
               DISPLAY WD-KZ with highlight AT VDU-LP.
           IF RE-CHKZ = 0 GO C.
           COMPUTE WD-GEW = RE-CHMAX / 1000.
           ADD 1023 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-GEW with highlight AT VDU-LP.
           MOVE RE-CHKZ TO WD-KZ.
           ADD 1036 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 1123 VDU-ECK GIVING VDU-LP.
           COMPUTE WD-GEW = RE-CHGEW / 1000.
           DISPLAY WD-GEW with highlight AT VDU-LP
           EVALUATE WD-KZ
               WHEN 1 DISPLAY "kg" with highlight AT 0000
               WHEN 2
               WHEN 3 MOVE RE-CHGEW TO WD-GEW
                      DISPLAY "%" AT 0000.
           GO E.
       C.  DISPLAY "<#/ret-leer>= Ende, < >= énderung < >" AT 2301.
           CALL "CAUP" USING "0023360000" WH-CREG.
           IF KIST or RET or ESC GO X.
           IF NOT AUF GO C.
       E.  DISPLAY "<esc/#/Ende>= Ende, <ret>= Temperatur" AT 2301.
           MOVE RE-TEMP TO WH-WERT.
           CALL "CAUP" USING "1003152104" WH-CREG.
           IF ESC or ENDE GO W.
           IF KIST GO X.
           IF NOT RET GO E.
           MOVE WH-WERT TO RE-TEMP WD-TEMP.
           DISPLAY WD-TEMP with highlight AT VDU-LP.
       F.  DISPLAY "<esc>= Ende, <ret>= Kneter" AT 2301.
           MOVE RE-KNET TO WH-WERT.
           CALL "CAUP" USING "1004111001" WH-CREG.
           IF ESC GO W.
           IF WOLI or AUF GO E.
           IF NOT RET GO F.
           MOVE WH-WERT TO RE-KNET WD-ZT.
           DISPLAY WD-ZT with highlight AT VDU-LP.
       G.  PERFORM EINGAB VARYING WX FROM 1 BY 1 UNTIL WX > 4.
       I.  DISPLAY "<esc>= Ende, <ret>= Gewicht in Kg" AT 2301.
           DISPLAY "max. Chargengewicht" with highlight AT 2401.
           COMPUTE WH-WERT = RE-CHMAX / 1000.
           CALL "CAUP" USING "1010233206" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO G.
           IF NOT RET GO I.
           COMPUTE RE-CHMAX =  WH-WERT * 1000
           MOVE WH-WERT TO WD-GEW.
           DISPLAY WD-GEW with highlight AT VDU-LP.
       K.  DISPLAY "<esc>= Ende, <ret>= Methode" AT 2301.
           DISPLAY "Teilung: 1= Kg, 2= %, 3= volle Chargen"
               with highlight AT 2401.
           MOVE RE-CHKZ TO WH-WERT.
           CALL "CAUP" USING "1010361001" WH-CREG.
           IF ESC GO W.
           IF WOLI or AUF GO I.
           IF NOT RET GO K.
           IF WH-WERT = 0 or WH-WERT > 3 GO K.
           MOVE WH-WERT TO RE-CHKZ WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       M.  DISPLAY "<esc>= Ende, <ret>= kg/%-Satz" AT 2301.
           COMPUTE WH-WERT = RE-CHGEW / 1000.
           CALL "CAUP" USING "1011233206" WH-CREG.
           IF ESC GO W.
           IF WOLI or AUF GO K.
           IF NOT RET GO M.
           COMPUTE RE-CHGEW =  WH-WERT * 1000
           MOVE WH-WERT TO WD-GEW.
           DISPLAY WD-GEW with highlight AT VDU-LP.
           EVALUATE WD-KZ
               WHEN 1 DISPLAY "kg" with highlight AT 0000
               WHEN 2
               WHEN 3 MOVE RE-CHGEW TO WD-GEW
                      DISPLAY "%" AT 0000.
           GO C.
       W.  REWRITE RE-SATZ INVALID WRITE RE-SATZ.
           READ REZEPT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       EINGAB SECTION.
       A.  IF WX not = 4 GO E.
       C.  DISPLAY "<esc>= Ende, <ret>= Gang-Nr." AT 2301.
           MOVE RE-GNR TO WH-WERT.
           CALL "CAUP" USING "1008151001" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF; IF WX > 1 ADD -1 TO WX GO A
                           else GO X.
           IF NOT RET GO C.
           MOVE WH-WERT TO RE-GNR WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       E.  COMPUTE VDU-LP = WX * 100 + 414.
           IF WX = 4 ADD 10 TO VDU-LP.
           DISPLAY "<esc>= Ende, <ret>= Minuten" AT 2301.
           MOVE RE-GMIN(WX) TO WH-WERT.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO Z.
           IF NOT RET GO E.
           MOVE WH-WERT TO RE-GMIN(WX) WD-ZT.
           DISPLAY WD-ZT with highlight AT VDU-LP.
       G.  COMPUTE VDU-LP = WX * 100 + 421.
           IF WX = 4 ADD 10 TO VDU-LP.
           DISPLAY "<esc>= Ende, <ret>= Minuten" AT 2301.
           MOVE RE-GSEK(WX) TO WH-WERT.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO E.
           IF NOT RET GO G.
           MOVE WH-WERT TO RE-GSEK(WX) WD-ZT.
           DISPLAY WD-ZT with highlight AT VDU-LP.
           GO Z.
       X.  MOVE 5 TO WX.
       Z.  EXIT.
      ******************************************** Subanzeige Rezeptur *
       ZWEITES SECTION.
       A.  CALL "CAUP" USING "0705011871020" WH-CREG.
           CALL "CAUP" USING "1303010380" WH-CREG.
           CALL "CAUP" USING "1304010480" WH-CREG.
           CALL "CAUP" USING "1306722480" WH-CREG.
           MOVE RE-SATZ TO WH-RESATZ.
           PERFORM OLDPIC.
           DISPLAY "Rpnr:" AT 0301.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Material u. Zeiten "  with foreground-color 3
                AT VDU-LP.
           ADD 140 VDU-ECK GIVING VDU-LP.
           DISPLAY " zusÑtl. Komponente " with highlight
               foreground-color 4 AT VDU-LP.
           DISPLAY "Pos Komp.  Bezeichnung            Menge Meh     Prei
      -        "s     Betrag TA" AT 0603.
           DISPLAY ALL "ƒ" with SIZE 69 AT 0702.
           MOVE WH-WERT TO RE-NUM.
       C.  READ REZEPT INVALID GO W.
           IF ZUGRIF PERFORM BESETZT GO W.
           PERFORM RPTKO.
           MOVE 0 TO WX WR WF XX.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 1 TO WI.
           PERFORM AB-ZEIG.
           PERFORM RPMEANZ.
       E.  DISPLAY "< / >= auf/ab, zurÅck mit beliebiger Taste" AT 2301.
           COMPUTE VDU-LP = XX * 100 + 307.
           CALL "CAUP" USING "1000070000" WH-CREG.
           IF AB; IF XX = 14 MOVE 13 TO WR
                             PERFORM AB-ZEIG GO E
                  else IF WT-WI(XX) not = 0 ADD 1 TO XX
                       end-if GO E.
           IF AUF; IF XX not = 1 ADD -1 TO XX
                   else MOVE 13 TO WR
                        PERFORM AUF-ZEIG
                   end-if GO E.
       W.  MOVE WH-RESATZ TO RE-SATZ.
           PERFORM ALTBILD.
           READ REZEPT INVALID NEXT SENTENCE.
           IF ZUGRIF PERFORM BESETZT GO W.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************ Inhalt anzeigen *
       AB-ZEIG SECTION.
       A.  CALL "CAUP" USING "1321022103" WH-CREG.
           PERFORM VARYING XX FROM 14 BY -1 UNTIL XX = 0
               or WT-WI(XX) not = 0 CONTINUE.
           IF XX not = 0 MOVE WT-WI(XX) TO WI
               IF WI not = 20 ADD 1 TO WI
                         else GO Z.
           IF (AB or BAB) and WT-WI(XX) = 0 GO Z.
           IF WI = 0 GO Z.
       C.  IF BAB and RE-KONUM(WI) = 0 GO Z.
           IF RE-KONUM(1) = 0 MOVE 1 TO XX GO Z.
           IF WR > 13 GO Z.
           IF XX = 14
               CALL "CAUP" USING "17SCROLL" WH-CREG
               PERFORM VARYING XX FROM 1 BY 1 UNTIL XX > 14
                   MOVE WT-WI(XX + 1) TO WT-WI(XX)
               end-perform MOVE 0 TO WT-WI(XX)
                   MOVE 13 TO XX.
           SET XR TO WI.
           PERFORM RPDIS.
           IF WI not = 21;
               IF RE-KONUM(WI) = 0 and XX NOT = 14
                   IF NOT RET ADD 1 TO XX
                   end-if GO Z
              else ADD 1 TO WI
                   GO C.
       Z.  EXIT.
      ************************************************ Inhalt anzeigen *
       AUF-ZEIG SECTION.
       A.  CALL "CAUP" USING "1308020803" WH-CREG.
           MOVE WT-WI(1) TO WI.
           ADD -1 TO WI.
           MOVE 0601 TO VDU-LP.
           IF WI = 0 MOVE 1 TO WI GO Z.
       C.  IF RE-KONUM(WI) = 0 GO Z.
           IF WR > 13 GO Z.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING XX FROM 13 BY -1 UNTIL XX < 1
               MOVE WT-WI(XX) TO WT-WI(XX + 1).
           SET XR TO WI.
           PERFORM RPDIS.
           IF WI not = 1 ADD -1 TO WI GO C.
       Z.  EXIT.
      *********************************************** ob voll anzeigen *
       VOLL SECTION.
       A.  IF WT-WI(1) = 0 GO Z.
           IF WT-WI(1) not = 1 ADD 402 VDU-ECK GIVING VDU-LP
               DISPLAY " " with highlight foreground-color 5 AT VDU-LP.
           ADD WT-WI(14) 1 GIVING WY.
           IF WY > 20 or WY = 1 GO Z.
           IF RE-KONUM(WY) = 0 GO Z.
           ADD 1702 VDU-ECK GIVING VDU-LP.
           DISPLAY " " with highlight foreground-color 5 AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       MEHR SECTION.
       A.  CALL "CAUP" USING "0715050660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " weiter Bedienungshinweise " AT VDU-LP.
           ADD 304 VDU-ECK GIVING VDU-LP.
           DISPLAY "<Entf>= Zeile lîschen" with highlight AT VDU-LP.
           ADD 404 VDU-ECK GIVING VDU-LP.
           DISPLAY "'*'+<ret>= Anmerkungstext" with highlight
              AT VDU-LP.
           DISPLAY "Weiter mit beliebiger Taste < >" AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       RPZEIL SECTION.
       A.  IF ENTF SET TABL TO TRUE GO Y.
           DISPLAY "< / >= auf/ab, <esc>= Ende, <tab>= anzeigen, <>= Ko
      -        "pfÑndg., < >= Kalkulation" AT 2301
               "<#>= W.-/Teigtemperatur, '+'= mitteln, '-'= drucken, <Po
      -        "s 1>= kopieren" AT 2401
               " ?+<ret>" with highlight foreground-color 6 AT 0000
               "<Einf>= einfÅgen, <Ende>= speichern, <Bild- >= Komp.éndg
      -        ". <Bild- >= Rp. anzeigen" AT 2501.
           PERFORM RPMEANZ.
           SET XR TO XX.
           PERFORM VOLL.
           PERFORM OLDPIC.
           MOVE RE-SATZ TO WH-RESATZ.
           PERFORM DUNHEL.
           COMPUTE VDU-LP = XX * 100 + 307.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1000004010" WH-CREG.
           PERFORM DUNHEL.
           COMPUTE VDU-LP = XX * 100 + 307 + VDU-ECK.
           MOVE WT-WI(XX) TO WI.
           IF WI not = 0 MOVE RE-KONUM(WI) TO WD-KOMPO
               DISPLAY WD-KOMPO with highlight AT VDU-LP
           else DISPLAY "       " AT VDU-LP
                PERFORM VARYING WI FROM 20 BY -1 UNTIL WI = 0
                    or RE-KOMP(WI) not = LOW-VALUE CONTINUE
                end-perform ADD 1 TO WI.
           IF RET and WH-MCODE(1:2) = "? " PERFORM MEHR GO A.
           IF RET and WH-MCODE(1:2) = "* "
               MOVE RE-BEZ TO WT-TX
               CALL "PARHINW" USING "42MOD" WH-CREG
               CANCEL "PARHINW" GO A.
           IF ESC GO Z.
           IF ENDE GO W.
           IF BAUF SET WI TO XR
                   IF WH-WERT = 0 and WH-NUM = 0
                       MOVE RE-KONUM(XR) TO WH-WERT
                   end-if
                   PERFORM ZWEITES
                   GO A.
           IF BAB  SET WI TO XR
                   IF WH-WERT = 0 and WH-NUM = 0
                       MOVE RE-KONUM(XR) TO WH-WERT
                   end-if
                   MOVE WH-WERT TO WH-NUM
                   CALL "PARKOMKA" USING "51ANZ" WH-CREG
                   CANCEL "PARKOMKA"
                   GO A.
       C.  IF AB; IF XX = 14 MOVE 13 TO WR
                             PERFORM AB-ZEIG GO A
                  else IF WT-WI(XX) not = 0 ADD 1 TO XX
                       end-if GO A.
           IF AUF; IF XX not = 1 ADD -1 TO XX
                   else MOVE 13 TO WR
                        PERFORM AUF-ZEIG
                   end-if GO A.
           IF ENTF MOVE XX TO IX
                   PERFORM LOESCH
                   MOVE IX TO XX
                   SET ESC TO TRUE
                   IF WT-WI(XX) = 0 and XX not = 1 ADD -1 TO XX.
           MOVE 0 TO WS.
           IF EINF PERFORM EINFUEG
                   IF ESC GO A
                   else GO E.
           IF WORE; IF RE-KONUM(1) NOT = 0
                        CALL "PARKOMKA" USING "20KALK" WH-CREG
                        CANCEL "PARKOMKA"
                        GO A.
           IF APOS; IF RE-KONUM(1) = 0 PERFORM RPDUPLO
                        IF ESC GO A ELSE PERFORM RPANZ GO A.
           IF WOLI MOVE 0 TO WM-MERKER
                   PERFORM RP-ANL
                   PERFORM RPTKO
                   PERFORM RPANZ GO A.
           IF WV-MC(1) = "+" PERFORM DURCHRECH GO A.
           IF WV-MC(1) = "-" CALL "PARDRU" USING "20RPLI" WH-CREG
                             CANCEL "PARDRU" GO A.
           IF KIST PERFORM ANZ-TEMP GO A.
           IF TABL CALL "CAUP" USING "16CLRFEN" WH-CREG
               PERFORM RPREORG
               SET RET TO TRUE
               PERFORM RPANZ GO A.
           IF not RET GO A.
           SET XR TO WI.
           IF XR < 21 MOVE RE-KOMP(XR) TO WH-KOMP.
           IF WH-WERT = 0 and WH-NUM = 0
               MOVE RE-KONUM(XR) TO WH-WERT.
       E.  IF WV-MCODE not = SPACE AND WH-NUM = 0
               CALL "PANANZ" USING "30KOSUCH" WH-CREG
               CANCEL "PANANZ"
               IF LG-NUM = 0 GO A
               ELSE MOVE LG-NUM TO WH-WERT.
           IF WH-WERT = 0 GO A.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           MOVE WH-WERT TO LG-NUM WD-KOMPO.
           COMPUTE VDU-LP = XX * 100 + 307 + VDU-ECK.
           DISPLAY WD-KOMPO with highlight AT VDU-LP.
           IF LG-NUM < 1000 GO A.
           MOVE 0 TO WM-MERKER.
       F.  READ LAGARTIK IGNORE LOCK INVALID MOVE 1 TO WM-MERKER.
           IF WM-MERKER = 1
               CALL "PARKOMKA" USING "51KOMPO" WH-CREG
               CANCEL "PARKOMKA"
               IF ESC OR LG-NUM = 0 GO A.
           IF LG-NUM not = RE-KONUM(XR) INITIALIZE RE-KOMP(XR)
                        CALL "CAUP" USING "1300070070" WH-CREG.
           DISPLAY WD-KOMPO with highlight AT VDU-LP.
           MOVE LG-NUM TO RE-KONUM(XR).
           MOVE LG-BEZ TO RE-KOBEZ(XR).
           IF LG-NETPZ not = 0 DISPLAY "%" AT 0000.
           MOVE 14 TO VDU-P.
           DISPLAY RE-KOBEZ(XR) with highlight AT VDU-LP.
           MOVE LG-ART TO WH-A.
           MOVE LG-NK TO WH-N.
           IF LG-ART = 0 or 10 MOVE 0 TO WH-N.
           MOVE LG-MEH TO WH-M.
           ADD 1 WH-M GIVING WH-MEH.
       G.  IF WH-A > 1; IF RE-PROZ(XR) = 0 MOVE LG-PREIS TO RE-PROZ(XR).
           MOVE 35 TO VDU-P.
           IF WH-A = 0 MOVE "gr" TO WT-FA(10)
                    MOVE RE-MENGE(XR) TO WD-RPM WH-WERT
                    DISPLAY WD-RPM with highlight AT VDU-LP
               ELSE MOVE WT-MEH(WH-MEH) TO WT-FA(10)
                    MOVE RE-PROZ(XR) TO WD-MGC WH-WERT
                    DISPLAY WD-MGC with highlight AT VDU-LP.
           MOVE 43 TO VDU-P.
           DISPLAY WT-FA(10) AT VDU-LP.
           MOVE SPACE TO WT-FA(10).
       H.  DISPLAY "<ret>= Menge, <esc/>= zurÅck" AT 2301.
           DISPLAY "Materialeingaben nur in gr" AT 2401.
           IF LG-NETPZ not = 0 MOVE LG-NETPZ TO WD-TEMP
               DISPLAY "Netto-%: " with highlight AT 2430 WD-TEMP
                    with highlight foreground-color 5.
           IF WH-N = 0 CALL "CAUP" USING "0000356007" WH-CREG.
           IF WH-N = 1 CALL "CAUP" USING "0000355107" WH-CREG.
           IF WH-N > 1 CALL "CAUP" USING "0000354207" WH-CREG.
           IF WOLI OR ESC; PERFORM ALTBILD
                           MOVE WH-RESATZ TO RE-SATZ GO A.
           IF NOT RET GO H.
           IF WH-WERT = 0 GO H.
           MOVE WH-WERT TO WD-RPM WD-MGC.
           IF WH-A = 0 DISPLAY WD-RPM with highlight AT VDU-LP
                       MOVE WH-WERT TO RE-MENGE(XR) RE-NGEW(XR)
                       IF LG-NETPZ not = 0
                          COMPUTE RE-NGEW(XR) = WH-WERT * LG-NETPZ / 100
                       end-if
                  ELSE DISPLAY WD-MGC with highlight AT VDU-LP
                       MOVE WH-WERT TO RE-PROZ(XR)
                       MOVE RE-MENGE(XR) TO RE-NGEW(XR).
           IF WH-A = 2 
               COMPUTE WH-BET ROUNDED = RE-MAT * RE-PROZ(XR) / 100
               GO K.
           IF WH-A = 3 MOVE 0 TO WH-BET GO K.
       I.  IF RE-EKP(XR) = 0 MOVE LG-PREIS TO RE-EKP(XR).
           MOVE RE-EKP(XR) TO WD-PREIS WH-WERT.
           MOVE 48 TO VDU-P.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           DISPLAY "EK-Preis" AT 2301.
           CALL "CAUP" USING "0000484208" WH-CREG.
           IF ESC OR WOLI GO G.
           IF NOT RET GO I.
           MOVE WH-WERT TO RE-EKP(XR) WD-PREIS. 
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           IF WH-A = 0 
             COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-MENGE(XR) / 1000
           ELSE COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-PROZ(XR).
       K.  MOVE WH-BET TO WD-BET.
           MOVE 57 TO VDU-P.
           DISPLAY WD-BET with highlight AT VDU-LP.
       L.  DISPLAY "K-art: 0= Teig, 1= BrÅhst., 2= Quellst. 3= Vorteig,
      -        "4= FÅlllg." AT 2301.
           DISPLAY "5= TÑtigkt. heute, 6= TÑtigkt. morgen, 7= Reinung, 8
      -        "= frei, 9= Zeit ohne TÑtigkt." AT 2401.
           MOVE WH-T TO WH-WERT WD-KZ.
           MOVE 69 TO VDU-P.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           CALL "CAUP" USING "0000691001" WH-CREG.
           IF ESC OR WOLI; IF WH-A = 3 GO G ELSE GO I.
           IF NOT RET GO L.
           MOVE WH-WERT TO WH-T WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           IF WH-A > 0; IF WH-T < 5 DISPLAY "nicht mîglich" AT 2401
                PERFORM  WEITER GO L.
           ADD 1 TO VDU-L.
           MOVE WH-TNAM TO RE-TNAM(XR).
           MOVE WI TO WT-WI(XX).
           SET AB TO TRUE.
           GO C.
       W.  DISPLAY "<tab>= lîschen, <ret/Ende>= speichern, <esc>= Abbruc
      -        "h < >" AT 2301.
           CALL "CAUP" USING "0023560000" WH-CREG.
           IF ESC UNLOCK REZEPT GO Z.
           IF TABL GO Y.
           IF NOT RET and not ENDE GO W.
       X.  PERFORM RPREORG.
           REWRITE RE-SATZ.
           GO Z.
       Y.  DISPLAY "wirklich lîschen? <ret>= ok, <esc>= Abbruch "
               AT 2301.
           CALL "CAUP" USING "0023450000" WH-CREG.
           IF ESC GO W.
           IF NOT RET GO Y.
           DELETE REZEPT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO Y.
       Z.  EXIT.
      ******************************************* altes Bild abstellen *
       OLDPIC SECTION.
       A.  MOVE WT-ERFTAB TO WI-ERFTAB.
           MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_READ_SCR_CHATTRS" USING
                WX-PS WI-ZEICH WI-ATTR WX-LG.
       Z.  EXIT.
      ************************************* altes Bild wieder ausgeben *
       ALTBILD SECTION.
       A.  MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_WRITE_SCR_CHATTRS"
               USING WX-PS WI-ZEICH WI-ATTR WX-LG.
           MOVE WI-ERFTAB TO WT-ERFTAB.
       Z.  EXIT.
      *********************************************** Position lîschen *
       LOESCH SECTION.
       A.  IF WT-WI(XX) = 0 GO Z.
           MOVE WT-WI(XX) TO WI WZ.
           PERFORM VARYING XX FROM WI BY 1 UNTIL XX > 19
               MOVE RE-KOMP(XX + 1) TO RE-KOMP(XX)
           end-perform
               INITIALIZE RE-KOMP(XX).
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE WT-WI(1) TO WY.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF.
           MOVE WY TO WI.
           PERFORM AB-ZEIG.
       Z.  EXIT.
      ********************************************** Position einfÅgen *
       EINFUEG SECTION.
       A.  IF RE-KONUM(20) not = 0
               DISPLAY "Bereits 20 Komponenten verwendet" AT 2401
               PERFORM WEITER GO Z.
           IF WT-WI(XX) = 00 GO Z.
           PERFORM OLDPIC.
           PERFORM VARYING WR FROM 14 BY -1 UNTIL WR < XX
               MOVE WT-WI(WR) TO WT-WI(WR + 1)
               IF WT-WI(WR + 1) not = 0 ADD 1 TO WT-WI(WR + 1).
           MOVE WT-WI(XX) TO WI.
           CALL "CAUP" USING "220000SCRAB" WH-CREG.
           MOVE RE-SATZ TO WH-RESATZ.
           PERFORM VARYING XR FROM 19 BY -1 UNTIL XR < XX
               MOVE RE-KOMP(XR) TO RE-KOMP(XR + 1).
           SET XR UP BY 1.
           MOVE LOW-VALUE TO RE-KOMP(XR).
           MOVE RE-TNAM(XR) TO WH-TNAM.
           MOVE 2 TO WS.
       C.  IF WH-WERT not = 0 GO Z
           else IF WH-MCODE not = SPACE GO Z.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Art.Nr., alpha+<ret>= suchen"
               AT 2301.
           COMPUTE VDU-LP = XX * 100 + 307.
           CALL "CAUP" USING "1000004010" WH-CREG.
           IF RET GO C.
           IF not ESC GO E.
           PERFORM ALTBILD.
           MOVE WH-RESATZ TO RE-SATZ.
       Z.  EXIT.
      ******************************************************************
       DUNHEL SECTION.
       A.  COMPUTE VDU-LP = XX * 100 + 302 + VDU-ECK.
           COMPUTE WX-L = VDU-L - 1.
           MOVE VDU-P TO WX-P.
           MOVE 68 TO WX-LNG.
           IF WX-ATTRIB(1:1) = X"31"
              MOVE ALL x"0F" TO WX-ATTRIB(1:69)
              MOVE ALL x"07" TO WX-ATTRIB(33:14)
              CALL "CBL_WRITE_SCR_ATTRS" USING WX-PARM WX-ATTRIB WX-LNG
           else MOVE ALL x"31" TO WX-ATTRIB
              CALL "CBL_WRITE_SCR_ATTRS" USING WX-PARM WX-ATTRIB WX-LNG.
       Z.  EXIT.
      *********************************** durchrechnen der Planmengen *
       DURCHRECH SECTION.
       A.  IF WH-RART = 1 GO Z.
           CALL "CAUP" USING "1300030070" WH-CREG.
           PERFORM RPREORG.
           COMPUTE WH-PREIS ROUNDED = RE-GEW / RE-TEIL.
           ADD 0,029 TO WH-PREIS.
           MOVE WH-PREIS TO WH-STK.
           COMPUTE WH-ATG ROUNDED = (WH-STK * RE-TEIL) / RE-GEW.
           SET XR TO 1.
       C.  IF RE-KONUM(XR) = 0 OR RE-MENGE(XR) = 0 GO E.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           IF WH-A = 0
               COMPUTE RE-MENGE(XR) ROUNDED = WH-ATG * RE-MENGE(XR).
       E.  IF XR < 20 SET XR UP BY 1 GO C.
           PERFORM RPANZ.
       Z.  EXIT.
      *****************************************************************
       RPTKO SECTION.
       A.  CALL "CAUP" USING "1303010380" WH-CREG.
           DISPLAY "Rpnr:" AT 0301.
           MOVE RE-NUM TO WD-ART.
           DISPLAY WD-ART with highlight AT 0307.
           DISPLAY RE-BEZ with highlight AT 0312.
           DISPLAY "Teiler:" AT 0339.
           MOVE RE-TEIL TO WD-TEIG.
           DISPLAY WD-TEIG with highlight AT 0347.
           IF WH-RART = 0 DISPLAY "gr  =" AT 0300
                    ELSE DISPLAY "Stk =" AT 0300.
           IF RE-GEW NOT = 0 COMPUTE WK-NUM = RE-GEW / RE-TEIL
               ADD 0,03 WK-NUM GIVING WH-MENGE
               MOVE WH-MENGE TO WD-MGA
               DISPLAY WD-MGA with highlight AT 0360
               IF WH-RART = 0 DISPLAY "Stk" AT 0300
                        ELSE DISPLAY "gr " AT 0300.
           DISPLAY WH-BK with highlight AT 0632 "/"
           IF WH-BK = 0 DISPLAY "B" AT 0000
                  ELSE DISPLAY "K" AT 0000.
           CALL "CAUP" USING "1304010460" WH-CREG.
           PERFORM BLECHE.
       Z.  EXIT.
      *********************************++++++***** anzeigen Blechgrî·e *
       BLECHE SECTION.
       A.  CALL "CAUP" USING "1304010450" WH-CREG.
           DISPLAY "Blechgrî·e: " AT 0401.
           IF RE-BLECH(1) = 0 GO Z.
           IF RE-BLECH(2) = 0 MOVE RE-BLECH(1) TO WD-POS GO X.
           IF RE-BLECH(3) = 0 MOVE RE-BLECH(1) TO WD-BLECH
                  DISPLAY WD-BLECH with highlight AT 0413 "x "
                  MOVE RE-BLECH(2) TO WD-BLECH
                  DISPLAY WD-BLECH with highlight AT 0000 "= "
                  MULTIPLY RE-BLECH(1) BY RE-BLECH(2) GIVING WD-POS
                  GO X.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 3
               MOVE RE-BLECH(WX) TO WD-BLECH
               COMPUTE VDU-LP = WX * 6 + 407
               DISPLAY WD-BLECH with highlight AT VDU-LP
               EVALUATE WX WHEN 1 DISPLAY "+ " AT 0000
                           WHEN 2 DISPLAY "+ " AT 0000
                           WHEN 3 DISPLAY "= " AT 0000
                                 ADD RE-BLECH(1) RE-BLECH(2) RE-BLECH(3)
                                     GIVING WD-POS.
       X.  DISPLAY WD-POS with highlight AT 0000 " Stk".
       Z.  EXIT.
      *****************************************************************
       RPDIS SECTION.
       A.  IF RE-KONUM(XR) = 0 GO Z.
           MOVE RE-TNAM(XR) TO WH-TNAM.
           MOVE RE-KONUM(XR) TO WD-KOMPO.
           COMPUTE VDU-LP = XX * 100 + 407 + VDU-ECK.
           DISPLAY WD-KOMPO with highlight AT VDU-LP.
           IF RE-MENGE(XR) not = RE-NGEW(XR) DISPLAY "%" AT 0000.
           ADD 7 TO VDU-P.
           DISPLAY RE-KOBEZ(XR) with highlight AT VDU-LP.
           ADD 1 WH-M GIVING WH-MEH.
           ADD 21 TO VDU-P.
           IF WH-A = 0 MOVE "gr" TO WT-FA(10)
                    MOVE RE-MENGE(XR) TO WD-RPM
                    DISPLAY WD-RPM AT VDU-LP
               ELSE MOVE WT-MEH(WH-MEH) TO WT-FA(10)
                    MOVE RE-PROZ(XR) TO WD-MGC
                    DISPLAY WD-MGC AT VDU-LP.
           ADD 8 TO VDU-P.
           DISPLAY WT-FA(10) AT VDU-LP.
           MOVE SPACE TO WT-FA(10).
           IF WH-A = 2
               COMPUTE WH-BET ROUNDED = RE-MAT * RE-PROZ(XR) / 100
               GO C.
           IF WH-A = 3 MOVE 0 TO WH-BET GO C.
           ADD 5 TO VDU-P.
           MOVE RE-EKP(XR) TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT VDU-LP.
           IF WH-A = 0 
             COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-MENGE(XR) / 1000
           ELSE COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-PROZ(XR).
       C.  MOVE WH-BET TO WD-BET.
           MOVE 57 TO VDU-P.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE WH-T TO WD-KZ.
           ADD 12 TO VDU-P.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 1 TO WR XX.
           MOVE WI TO WT-WI(XX).
       Z.  EXIT.
      *****************************************************************
       RPMEANZ SECTION.
       A.  SET XR TO 1.
           MOVE 0 TO RE-BET RE-GEW RE-MAT WH-BET WH-MIN WH-MZUS
                     WT-ZEIT(1) WT-ZEIT(2) WT-ZEIT(3) WT-ZEIT(4)
                     WT-ZEIT(5).
       B.  IF RE-MENGE(XR) = 0 GO E.
           MOVE RE-TNAM(XR) TO WH-TNAM.
      *----------------> WH-A: 0 = Material, 1 = Zeit, 2 = Aufschlag <-
           IF WH-A = 2
               COMPUTE WH-BET ROUNDED = RE-MAT * RE-PROZ(XR) / 100.
           IF WH-A = 0 
             COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-MENGE(XR) / 1000
             ADD WH-BET TO RE-MAT
             IF WH-T < 5 ADD RE-NGEW(XR) TO RE-GEW.
           IF WH-A = 1
               COMPUTE WH-BET ROUNDED = RE-EKP(XR) * RE-PROZ(XR)
               IF WH-T = 0 MOVE 5 TO WH-T end-if
               evaluate wh-t
                  when 5
                  when 6 ADD RE-PROZ(XR) TO WH-MIN
                         ADD RE-PROZ(XR) TO WT-ZEIT(WH-T - 4).
           IF WH-A = 3 MOVE 0 TO WH-BET
               MOVE RE-PROZ(XR) TO WH-MZUS.
           ADD WH-BET TO RE-BET.
       E.  IF XR < 20 SET XR UP BY 1 GO B.
           IF RE-BET = 0 AND RE-GEW = 0 GO Z.
           IF WH-RART = 1 MOVE RE-TEIL TO WH-STK
                ELSE COMPUTE WH-STK = RE-GEW / RE-TEIL.
           COMPUTE RE-PREIS ROUNDED = RE-BET / WH-STK.
           MOVE RE-PREIS TO WD-PREIS.
           DISPLAY WD-PREIS with highlight AT 0372.
           COMPUTE WD-PREIS = RE-BET * 1000 / RE-GEW.
           DISPLAY WD-PREIS with highlight AT 0472.
           MOVE WH-MIN TO WD-MIN.
           DISPLAY "Ges.Min." AT 0572.
           DISPLAY WD-MIN with highlight AT 0674.
           IF RE-SKP NOT = 0 DISPLAY "Selbstk." AT 0872
               MOVE RE-SKP TO WD-PREIS
               DISPLAY WD-PREIS with highlight AT 0972.
           MOVE RE-TEIL TO WD-TEIG.
           DISPLAY WD-TEIG with highlight AT 0347.
           IF WH-RART = 0 DISPLAY "gr  =" AT 0300
                    ELSE DISPLAY "Stk =" AT 0300.
           IF RE-GEW NOT = 0 COMPUTE WK-NUM = RE-GEW / RE-TEIL
               ADD 0,03 WK-NUM GIVING WH-MENGE
               MOVE WH-MENGE TO WD-MGA
               DISPLAY WD-MGA with highlight AT 0360
               IF WH-RART = 0 DISPLAY "Stk" AT 0300
                        ELSE DISPLAY "gr " AT 0300.
           MOVE RE-GEW TO WD-GES.
           MOVE VDU-LP TO VDU-RES.
           ADD 1834 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-GES with foreground-color 11 AT VDU-LP.
           MOVE 43 TO VDU-P.
           DISPLAY "gr " with foreground-color 11 AT VDU-LP.
           MOVE RE-BET TO WD-BET.
           MOVE 57 TO VDU-P.
           DISPLAY WD-BET with foreground-color 11 AT VDU-LP.
           MOVE VDU-RES TO VDU-LP.
       Z.  EXIT.
      *****************************************************************
       RPANZ SECTION.
       A.  MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 1 TO WI.
           MOVE 0 TO WX WR WF.
           PERFORM AB-ZEIG.
       Z.  EXIT.
      *****************************************************************
       RPREORG SECTION.
       A.  SET XR TO 1.
       B.  IF RE-MENGE(XR) NOT = 0; IF XR < 19 SET XR UP BY 1
               GO B ELSE GO Z.
           SET WX TO XR.
           MOVE LOW-VALUE TO RE-KOMP(XR).
       C.  SET XR UP BY 1.
           IF RE-MENGE(XR) = 0 MOVE LOW-VALUE TO RE-KOMP(XR);
               IF XR < 20 GO C ELSE GO Z.
       D.  MOVE RE-KOMP(XR) TO RE-KOMP(WX).
           MOVE LOW-VALUE TO RE-KOMP(XR).
           GO A.
       Z.  EXIT.
      *****************************************************************
       READ-ARTK SECTION.
       A.  IF AR-NUM > 999 GO C.
           READ ARTIKEL IGNORE LOCK INVALID MOVE SPACE TO AR-BEZ.
           IF ANLAGE MOVE AR-BEZ TO RE-BEZ
               DISPLAY RE-BEZ with highlight AT 0312.
           GO Z.
       C.  MOVE AR-NUM TO LG-NUM.
           READ LAGARTIK IGNORE LOCK INVALID MOVE SPACE TO RE-BEZ GO Z.
           IF ANLAGE MOVE LG-BEZ TO RE-BEZ
               DISPLAY RE-BEZ with highlight BLINK AT 0312.
           DISPLAY "Lagerartikel vorhanden!" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      *****************************************************************
       RP-ANL SECTION.
       A.  MOVE 0 TO AR-NUM.
           IF ANLAGE MOVE LOW-VALUE TO RE-SATZ
               MOVE WH-NUM TO RE-NUM AR-NUM
               MOVE SPACE TO RE-BEZ.
           PERFORM RPTKO.
           PERFORM READ-ARTK.
       B.  MOVE RE-BEZ TO WT-TX.
           DISPLAY RE-BEZ with highlight AT 0312.
           IF RE-NUM > 3000
              DISPLAY "Keine Umlaute mîglich!" with highlight AT 2401.
           CALL "CAUP" USING "0203120125" WH-CREG.
           IF WOLI OR ESC GO Z.
           IF WT-TX = SPACE DISPLAY "Bezeichnung muss sein!" AT 2401
               PERFORM WEITER GO B.
           IF RE-NUM > 3000 PERFORM TEXTPRF
               IF ESC GO B.
           MOVE WT-TX TO RE-BEZ.
           DISPLAY RE-BEZ with highlight AT 0312.
       C.  MOVE RE-TEIL TO WH-WERT.
           DISPLAY "<esc>= Ende" AT 2301.
           CALL "CAUP" USING "0003475006" WH-CREG.
           IF ESC GO V.
           IF WOLI GO B.
           IF TABL MOVE 1 TO WH-RART
               MOVE 13 TO WX-TASTE
           ELSE MOVE 0 TO WH-RART.
           IF NOT RET GO C.
           MOVE WH-WERT TO RE-TEIL WD-TEIG.
           DISPLAY WD-TEIG with highlight AT 0347.
           MOVE WH-RART TO WH-WERT.
       D.  DISPLAY "Mengeneinheit z. Teiler:" AT 2301.
           DISPLAY "0: gr, 1: Stk" AT 2401.
           CALL "CAUP" USING "0003541001" WH-CREG.
           IF ESC GO V.
           IF WOLI GO C.
           IF NOT RET GO D.
           IF WH-WERT > 1 GO D.
           MOVE WH-NUM TO WH-RART.
           IF WH-RART = 0 DISPLAY "gr  =" AT 0354
                    ELSE DISPLAY "Stk =" AT 0354.
           IF RE-GEW NOT = 0 COMPUTE WK-NUM = RE-GEW / RE-TEIL
               ADD 0,03 WK-NUM GIVING WH-MENGE
               MOVE WH-MENGE TO WD-MGA
               DISPLAY WD-MGA AT 0360
               IF WH-RART = 0 DISPLAY "Stk" AT 0300
                        ELSE DISPLAY "gr " AT 0300.
           MOVE 1 TO WX.
      *--------------------------------------------------> Blechgrî·e <-
       F.  DISPLAY "<ret>= Anzahl, 0 = keine StÅck" AT 2301.
           MOVE RE-BLECH(WX) TO WH-WERT.
           EVALUATE WX WHEN 1 MOVE 13 TO VDU-P
                       WHEN 2 MOVE 18 TO VDU-P
                       WHEN 3 MOVE 24 TO VDU-P.
           CALL "CAUP" USING "0004002002" WH-CREG.
           IF ESC GO V.
           IF WOLI; IF WX = 1 PERFORM BLECHE GO D
                    ELSE ADD -1 TO WX GO F.
           IF NOT RET GO F.
           MOVE WH-WERT TO WD-BLECH RE-BLECH(WX).
           IF WH-WERT = 0;
               EVALUATE WX WHEN 3 GO G
                           WHEN 2 MOVE 0 TO RE-BLECH(3) GO G
                           WHEN 1 MOVE 0 TO RE-BLECH(2) RE-BLECH(3)
                                  GO G.
           DISPLAY WD-BLECH with highlight AT VDU-LP.
           IF WX < 3 ADD 1 TO WX GO F.
       G.  PERFORM BLECHE.
       K.  DISPLAY "<ret>= Kalk.Satz: 0 = BÑckerei, 1 = Konditorei"
                AT 2401.
           MOVE WH-BK TO WH-WERT.
           DISPLAY WH-BK with highlight AT 0632 "/"
           IF WH-BK = 0 DISPLAY "B" AT 0000
                  ELSE DISPLAY "K" AT 0000.
           CALL "CAUP" USING "0006321001" WH-CREG.
           IF WOLI MOVE 1 TO WX GO F.
           IF NOT RET GO K.
           MOVE WH-WERT TO WH-BK.
           DISPLAY WH-BK with highlight AT 0632 "/"
           IF WH-BK = 0 DISPLAY "B" AT 0000
                  ELSE DISPLAY "K" AT 0000.
       V.  DISPLAY "<ret>= speichern, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0023340000" WH-CREG.
           IF ESC UNLOCK REZEPT GO Z.
           IF WOLI GO D.
           IF NOT RET GO V.
           MOVE WH-TART TO RE-ART.
       X.  WRITE RE-SATZ INVALID REWRITE RE-SATZ.
           IF ZUGRIF PERFORM BESETZT GO X.
           GO Z.
       Z.  EXIT.
      ***************************** ab 3000 nur Gro·schreibung ohne SZ *
       TEXTPRF SECTION.
       A.  MOVE WT-TX TO WT-BEZ.
           CALL "CAUP" USING "20CONV25" WH-CREG.
           CALL "CAUP" USING "21CONV25" WH-CREG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 25
               IF WT-TX(WZ:1) = "é" or WT-TX(WZ:1) = "ô"
               or WT-TX(WZ:1) = "ö" MOVE 30 TO WZ.
           IF WZ > 29 DISPLAY "Keine Umlaute erlaubt!" AT 2401
               PERFORM WEITER
               SET ESC TO TRUE.
           MOVE WT-BEZ TO WT-TX.
       Z.  EXIT.
