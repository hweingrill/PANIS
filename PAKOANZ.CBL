      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PAKOANZ.
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       OBJECT-COMPUTER.     PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
           COPY PAKOSEC.CPY.
           COPY PARSEART.CPY.
           COPY PARSELFT.CPY.
           COPY PAKSEWAG.CPY.
           COPY PAKSESRE.CPY.
       DATA DIVISION.
       FILE SECTION.
           COPY PAKOFD.CPY.
           COPY PARFDART.CPY.
           COPY PARFDLFT.CPY.
           COPY PAKFDWAG.CPY.
           COPY PAKFDSRE.CPY.
      *****************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(14)    OCCURS 12.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WI                      PIC 99       COMP.
           03  WL                      PIC 99       COMP.
           03  IX                      PIC 99       COMP.
           03  WD-POS                  PIC Z9.
           03  WD-KZ                   PIC 9.
           03  WD-KTO                  PIC ZZ.ZZ9.
           03  WD-BELNR                PIC ZZZZZZ9.
           03  WD-PREIS                PIC ZZ.ZZ9,99-.
           03  WD-RAB                  PIC Z9,9-.
           03  WD-RENUM                PIC Z.Z99.
           03  WD-ZEIT                 PIC 99.99-.
           03  WD-BET                  PIC ZZZZ.ZZ9,99-.
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WH-SEITE                PIC 9(5)      COMP.
      *---------------------------------------------> Journalrechnung <-
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(6)      COMP.
               05 WH-LFD               PIC 99        COMP-X.
           03  WH-JOKEY                PIC X(4).
       COPY PAKOEXT.CPY.
       COPY PAKODECL.CPY.
       DECL-M SECTION.         USE AFTER ERROR PROCEDURE ON SREKOPF.
       A.  CALL "CADECL" USING "PAKSRKOP.DAT" WH-CREG.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON PROTWAAG.
       A.  CALL "CADECL" USING "PAKWAAG.DAT " WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON KREDIT.
       A.  CALL "CADECL" USING "PAKOLIEF.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 10 PERFORM JOURSUM GO X
               WHEN 20 PERFORM SCHEINR GO X
               WHEN 30 PERFORM WAAGANZ GO X
               WHEN 40 PERFORM KREDANZ GO X
               WHEN 50 PERFORM P-WECHSEL GO X
               WHEN 80 PERFORM KORREKT GO Y
               WHEN 90 PERFORM KTOSUM GO Y.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Y.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401
           PERFORM WEITER.
       Z.  EXIT.
      *****************************************************************
       WASNUN SECTION.
       A.  IF WX = 0 PERFORM NO-REC
               SET ESC TO TRUE GO Z.
       B.  DISPLAY "Zeile+<ret>= Auswahl, <ret-leer>/< >= weiterlisten,
      -        "< >= zur�cklisten" AT 2301.
           IF FINE DISPLAY "***** keine Daten mehr *****"
               with highlight AT 2450.
           DISPLAY "<Esc>= Ende" AT 2401.
           CALL "CAUP" USING "0023711001" WH-CREG.
           MOVE 1 TO WR.
           IF ESC OR ENDE SET ESC TO TRUE MOVE 0 TO WH-NUM GO Z.
           IF WH-NUM > 0; IF WT-KEY(WH-NUM) = LOW-VALUE GO B ELSE GO Z.
           IF RET SET AB TO TRUE.
           IF FINE; IF AB  AND WI = 8 GO B
               ELSE IF AUF AND WI = 2 GO B.
           IF NOT AB AND NOT AUF GO A.
       Z.  EXIT.
      ******************************************************************
       ALLANZ SECTION.
       A.  PERFORM VARYING WX FROM 13 BY -1 UNTIL WX = 1
               OR WV-MCODE(WX:1) NOT = SPACE CONTINUE.
           IF WV-MCODE(WX:1) NUMERIC MOVE SPACE TO WV-MCODE(WX:1).
           ADD 202 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 9
               MOVE WI TO WD-POS
               DISPLAY WD-POS with highlight AT VDU-REL
               ADD 100 TO VDU-REL.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF.
           MOVE 3 TO WI.
       Z.  EXIT.
      *****************************************************************
       KREDANZ SECTION.
       A.  CALL "CAUP" USING "0703011180001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Kreditoren " with highlight AT VDU-REL.
           PERFORM ALLANZ.
           MOVE WV-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO KR-MCODE.
           START KREDIT KEY NOT < KR-MCODE INVALID PERFORM NO-REC
                GO Z.
       E.  READ KREDIT NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF KR-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM KRED-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO KR-KTONR GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO KR-KEY
               MOVE KR-KTONR TO WH-WERT WH-NUM GO Z.
           IF AB GO E.
       N.  READ KREDIT PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF KR-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM KRED-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       KRED-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE KR-KTONR TO WD-KTO.
           DISPLAY WD-KTO AT VDU-REL.
           ADD 9 TO VDU-REL.
           MOVE KR-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           DISPLAY WT-TX with highlight SIZE 52 AT VDU-REL.
           ADD 54 TO VDU-REL.
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV05" WH-CREG.
           DISPLAY WT-TX WITH SIZE 5 AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE KR-KEY TO WT-KEY(WX).
       Z.  EXIT.
      *****************************************************************
       WAAGANZ SECTION.
       A.  CALL "CAUP" USING "0703011140001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Waageprotokolle " with highlight
               AT VDU-REL.
           PERFORM ALLANZ.
           START PROTWAAG KEY NOT < PWA-AKEY INVALID PERFORM NO-REC
                GO Z.
       E.  READ PROTWAAG NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF PWA-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM WAAG-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO WH-NUM PWA-KEY GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO PWA-KEY
               MOVE PWA-KEY TO WH-WERT WH-NUM GO Z.
           IF AB GO E.
       N.  READ PROTWAAG PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF PWA-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM WAAG-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       WAAG-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE PWA-RENUM TO WD-RENUM.
           DISPLAY WD-RENUM with highlight AT VDU-REL.
           ADD 6 TO VDU-REL.
           MOVE PWA-ZEIT TO WD-ZEIT.
           MOVE ":" TO WD-ZEIT(3:1).
           DISPLAY WD-ZEIT AT VDU-REL.
           ADD 6 TO VDU-REL.
           DISPLAY PWA-RPBEZ with highlight AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE KR-KEY TO WT-KEY(WX).
       Z.  EXIT.
      ***************************************** Journalsummen anzeigen *
       JOURSUM SECTION.
       A.  CALL "CAUP" USING "0701102256000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " noch nicht verbuchte Sammelbuchungen "
               with highlight AT VDU-LP.
           ADD WE-USE 1 GIVING WH-KEY.
       C.  READ KONSLAG IGNORE LOCK INVALID GO Z.
           DISPLAY "Soll         Haben" AT 0245.
           DISPLAY "Vorsteuer.............:" AT 0312.
           DISPLAY "Mehrwertsteuer........:" AT 0412.
           DISPLAY "Skontoerl�s.(w).......:" AT 0512.
           DISPLAY "Skontoerl�s.(s).......:" AT 0612.
           DISPLAY "Skto.-Aufwand Ust-Kz.1:" AT 0712.
           DISPLAY "Skto.-Aufwand Ust-Kz.2:" AT 0812.
           DISPLAY "Skto.-Aufwand Ust-Kz.3:" AT 0912.
           DISPLAY "Skto.-Aufwand Ust-Kz.4:" AT 1012.
           DISPLAY "Skto.-Aufwand Ust-Kz.5:" AT 1112.
           DISPLAY "Skto.-Aufwand Ust-Kz.6:" AT 1212.
           DISPLAY "Sammelkonto Deb.- 0...:" AT 1312.
           DISPLAY "    ''      Deb.- 1...:" AT 1412.
           DISPLAY "    ''      Deb.- 2...:" AT 1512.
           DISPLAY "Sammelkonto Kred.- 0..:" AT 1612.
           DISPLAY "    ''      Kred.- 1..:" AT 1712.
           DISPLAY "    ''      Kred.- 2..:" AT 1812.
           DISPLAY "Journalsummen.........:" AT 1912.
           DISPLAY ALL "�" with SIZE 28 AT 2036.
           DISPLAY "Totalsummen...........:" AT 2112.
      *    MOVE KL-VST TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0336.
      *    MOVE KL-MWST TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0450.
      *    MOVE KL-SKTOERL(1) TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0550.
      *    MOVE KL-SKTOERL(2) TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0650.
           MOVE 0636 TO VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
      *        MOVE KL-SKTOAUF(IX) TO WD-SALDO
               ADD 100 TO VDU-LP
               DISPLAY WD-SALDO with highlight AT VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
      *        MOVE KL-DEBSUM(IX) TO WD-SALDO
               COMPUTE VDU-L = IX + 12
               IF IX > 3 MOVE 50 TO VDU-P
                         SUBTRACT 3 FROM VDU-L
                    else MOVE 36 TO VDU-P
               end-if DISPLAY WD-SALDO AT VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
      *        MOVE KL-KREDSUM(IX) TO WD-SALDO
               COMPUTE VDU-L = IX + 15
               IF IX > 3 MOVE 50 TO VDU-P
                         SUBTRACT 3 FROM VDU-L
                    else MOVE 36 TO VDU-P
               end-if DISPLAY WD-SALDO AT VDU-LP.
      *    MOVE KL-JSOLL TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 1936.
      *    MOVE KL-JHABEN TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 1950.
      *    ADD KL-VST KL-SKTOAUF(1) KL-SKTOAUF(2) KL-SKTOAUF(3)
      *               KL-SKTOAUF(4) KL-SKTOAUF(5) KL-SKTOAUF(6)
      *                     KL-JSOLL GIVING WD-SALDO WE-SOLL.
           DISPLAY WD-SALDO with highlight AT 2136.
      *    ADD KL-MWST KL-SKTOERL(1) KL-SKTOERL(2)
      *                     KL-JHABEN GIVING WD-SALDO WE-HABEN.
           DISPLAY WD-SALDO with highlight AT 2150.
           SUBTRACT WE-HABEN FROM WE-SOLL GIVING WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 2243.
           DISPLAY "weiter mit beliebiger Taste < >" AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           IF SF1 PERFORM JOURECH GO C.
       Z.  EXIT.
      ******************************** Durchrechnung der Journalsummen *
       JOURECH SECTION.
       A.  CALL "CAUP" USING "0712200550000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY " Journalkontrolle " with highlight AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO IX.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Start <    >" AT VDU-LP.
           DISPLAY "<#>= J-Zeile f. 0-Vergleich" AT 2401.
           CALL "CAUP" USING "1004334004" WH-CREG.
           IF ESC GO X.
           IF KIST MOVE 1 TO IX
               SET RET TO TRUE MOVE 0 TO LB-USE LB-SEITE.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-SEITE.
           MOVE 0 TO WV-DATUM.
           INITIALIZE KL-SASUM.
           IF IX = 0 PERFORM FIRST-JOUR
                     IF ESC GO W.
           MOVE LB-JKEY TO WH-JOKEY.
       E.  MOVE WH-JOKEY TO LB-JKEY.
           START LAGERBZ KEY NOT < LB-JKEY INVALID GO W.
       G.  MOVE SPACE TO LB-TX.
           READ LAGERBZ NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
      *    if LB-use = 43 and LB-seite < 57000 go i.   *> raus!!!
           IF LB-USE NOT = WE-USE GO W.
           if ix = 1; if LB-use > 0 go w
                      ELSE move 1 to LB-use rewrite LB-satz go e.
       i.  ADD 230 VDU-ECK GIVING VDU-LP.
           DISPLAY LB-USE AT VDU-LP "/" LB-SEITE.
           MOVE LB-JKEY TO WH-JOKEY.
           MOVE WE-SYM(LB-SYM + 1) TO WT-TX.
           MOVE LB-KTONR TO WH-NUM.
           PERFORM LIESKTO.
      *    MOVE LB-VM TO WE-UST.
           PERFORM KOSTSUM.
           PERFORM ADD-UST.
      *--------------------------------------> Journalsummenvergleich <-
           DISPLAY WD-SALDO with highlight AT 2150.
           SUBTRACT WE-HABEN FROM WE-SOLL GIVING WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 2243.
           IF WE-SOLL NOT = WE-HABEN GO G.
           IF WH-SEITE NOT = 0; IF LB-SEITE < WH-SEITE GO G.
      *-----------------------------------------> ob Journalsaldo = 0 <-
           DISPLAY "weiter mit beliebiger Taste <    > "
               AT 2301 WH-SEITE.
           CALL "CAUP" USING "0023304004" WH-CREG.
           MOVE WH-NUM TO WH-SEITE.
           GO G.
       W.  REWRITE KL-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************************** Addition UST *
       ADD-UST SECTION.
       Z.  EXIT.
      ******************************* Summenbildung Sachkonten Kat.= 0 *
       KOSTSUM SECTION.
       Z.  EXIT.
      ******************************************************************
       FIRST-JOUR SECTION.
       A.  MOVE WE-USE TO LB-USE.
           MOVE 0 TO LB-SEITE.
           START LAGERBZ KEY NOT < LB-JKEY INVALID
               MOVE 0 TO LB-SEITE LB-USE GO E.
           MOVE SPACE TO LB-SATZ(12:).
       C.  READ LAGERBZ NEXT IGNORE LOCK AT END MOVE 0 TO LB-SEITE.
       E.  IF LB-USE NOT = WE-USE SET ESC TO TRUE GO Z.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  SET RET TO TRUE.
           IF WH-NUM = 0 GO X.
           MOVE WH-NUM TO WD-KZ.
           MOVE WH-NUM TO WD-KTO.
           MOVE WH-NUM TO LG-NUM.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ LAGARTIK NOT INVALID
               MOVE LG-STAND TO WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
       X.  SET FEHLER TO TRUE.
           DISPLAY "Konto fehlt: " at 2401 WH-NUM.
           PERFORM WEITER.
       Z.  EXIT.
      ************************************* Ermittlung von Kontosummen *
       KTOSUM SECTION.
       X.  SET ESC TO TRUE.
       Z.  EXIT.
      *************************************** Korrektur Buchungszeilen *
       KORREKT SECTION.
       A.  CALL "CAUP" USING "0701010365000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Korrektur BZ " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Betrag:                +/-:                Neu:"
               AT VDU-LP.
           MOVE LB-MENGE TO WD-SALDO.
           ADD 211 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Betrag" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1002318210" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           MOVE WH-WERT TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           ADD WH-WERT TO LB-MENGE.
           MOVE LB-MENGE TO WD-SALDO.
           ADD 251 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= ok!" AT 2301.
           CALL "CAUP" USING "0023280000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           REWRITE LB-SATZ.
           UNLOCK LAGERBZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       P-WECHSEL SECTION.
       A.  CALL "CAUP" USING "0715070665000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " ACHTUNG - Periodenwechsel! " with highlight
               AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           IF WX NOT = 37 DISPLAY "Journalabschlu� erforderlich!"
               with highlight AT VDU-LP
               ADD 403 VDU-ECK GIVING VDU-LP GO K.
           MOVE WM-DATUM TO WZ-DATUM.
           MOVE LB-BUDAT TO WC-DATUM.
           IF LB-BUDAT = 0 MOVE WM-DATUM TO WE-PER GO K.
           IF WC-DATUM(1:4) = WZ-DATUM(1:4) GO K.
           ADD 205 VDU-ECK GIVING VDU-LP.
           PERFORM DATDREH.
           DISPLAY "Buchungsdatum: " AT VDU-LP VDU-DATUM with highlight
               " stimmt mit " "      (= Bu-Datum)" with highlight.
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Buchungsperiode: " AT VDU-LP VDU-DATUM
                with highlight " nicht �berein!" "   (= Tagesdatum)"
                 with highlight.
           ADD 403 VDU-ECK GIVING VDU-LP.
       C.  DISPLAY "Bitte Tagesdatum Pkt. 8 �ndern oder Bu-Datum �berpr�
      -        "fen! < >" AT VDU-LP.
           CALL "CAUP" USING "1004610000" WH-CREG.
           SET ESC TO TRUE.
           GO Z.
       K.  DISPLAY "Periodenwechsel daher unzul�ssig! < >" AT VDU-LP.
           CALL "CAUP" USING "1004380000" WH-CREG.
           SET ESC TO TRUE.
       Z.  EXIT.
      *************************************** anzeigen Schreinrechnung *
       SCHEINR SECTION.
       A.  CALL "CAUP" USING "0708011280011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Schattenrechnung " with highlight AT VDU-REL.
           ADD 202 VDU-ECK GIVING VDU-REL.
           DISPLAY " Z.  KtoNr. Re-Nr.   Datum   Mcode Bezeichnung
      -     "                   Betrag "  with reverse-video AT VDU-REL.
           ADD 100 TO VDU-ECK.
           PERFORM ALLANZ.
           ADD -100 TO VDU-ECK.
           MOVE 0 TO SK-SA SK-BELNR SK-BELDAT.
           MOVE WH-NUM TO WE-LIEFER.
           MOVE WE-LIEFER TO SK-LIEFER.
           START SREKOPF KEY NOT < SK-KEY INVALID PERFORM NO-REC GO Z.
       E.  READ SREKOPF NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF SK-SA > 0 MOVE 8 TO WI SET FINE TO TRUE GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF SK-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM SRE-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO SK-LIEFER SK-BELNR GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO SK-KEY
               MOVE SK-LIEFER TO WH-WERT WH-NUM GO Z.
           IF AB GO E.
       N.  READ SREKOPF PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF SK-SA > 0 MOVE 2 TO WI SET FINE TO TRUE GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF SK-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM SRE-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       SRE-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 306 + VDU-ECK.
           MOVE SK-LIEFER TO WD-KTO.
           DISPLAY WD-KTO AT VDU-REL.
           MOVE SK-BELNR TO WD-BELNR.
           ADD 6 TO VDU-REL.
           MOVE SK-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV5" WH-CREG.
           MOVE SK-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY ",".
           MOVE SK-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY WD-BELNR AT VDU-REL with highlight "/"
               VDU-DATUM with highlight
               "   " WT-TX(1:5) with highlight " " WT-BEZ(1:30).
           COMPUTE VDU-REL = WX * 100 + 368 + VDU-ECK.
           MOVE SK-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE SK-KEY TO WT-KEY(WX).
       Z.  EXIT.
