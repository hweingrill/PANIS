      $SET LINKCOUNT"256" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PANANZ.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEDEB.CPY.
           COPY PANSEART.CPY.
           COPY PARSEART.CPY.
           COPY PARSEREZ.CPY.
           COPY PANSELFS.CPY.
      *--------------------------------------*> nur Euro-Konvtg.
           COPY PANSEFAK.CPY.
           COPY PANSESTA.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANFDART.CPY.
       COPY PARFDART.CPY.
       COPY PARREZEP.CPY.
       COPY PANLIEF.CPY.
       COPY PANSTAT.CPY.
       COPY PANFAKT.CPY.
      *****************************************************************
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
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(15)         OCCURS 15.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WH-L                    PIC 99.
           03  WI                      PIC 99       COMP.
           03  WS                      PIC 99       COMP.
           03  WD-POS                  PIC Z9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-ART                  PIC ZZZ9.
           03  WD-LFSNR                PIC ZZ.ZZ9-.
           03  WD-X                    PIC X.
           03  WV-LGNUM                PIC 9(4)     COMP.
           03  WH-BET                  PIC 9(6)V99  COMP.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING  "PANELFS.DAT"  WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON LAGARTIK.
       A.  CALL "CADECL" USING "PARARTIK.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON LAGMATCH.
       A.  CALL "CADECL" USING "PARMATCH.DAT" WH-CREG.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  CALL "CADECL" USING "PANDEBIT.DAT" WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE 0 TO WV-LGNUM.
           EVALUATE WL-CA
               WHEN 10 PERFORM DEB-ANZ
               WHEN 20 PERFORM ART-ANZ
               WHEN 30 OPEN INPUT LAGMATCH
                       PERFORM KO-MATCH
               WHEN 33
               WHEN 35 PERFORM KO-MATCH
               WHEN 40 PERFORM RP-SUCH
               WHEN 50 PERFORM LFS-ANZ
               WHEN OTHER GO Z.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " WITH HIGHLIGHT AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           SET ESC TO TRUE.
       Z.  EXIT.
      *****************************************************************
       WASNUN SECTION.
      *A.  IF WX = 0 PERFORM NO-REC GO Z.
       A.  IF WL-CA = 33 MOVE 35 TO WL-CA
                         SET EINF TO TRUE GO Z.
       B.  DISPLAY "Zeile+<ret>= Auswahl, <ret-leer>/< >= weiterlisten,
      -        "< >= zur�cklisten" AT 2301.
           IF FINE DISPLAY "** keine Daten **" with highlight AT 2461.
           DISPLAY "<esc/#>= Ende" AT 2401.
           IF WL-CA = 35 DISPLAY ", <Einfg>= einf�gen Matchcodes"
               AT 0000.
           CALL "CAUP" USING "0023711001" WH-CREG.
           IF EINF GO Z.
           IF WX = 0 PERFORM NO-REC GO Z.
           MOVE 1 TO WR.
           IF ESC OR KIST MOVE 0 TO WH-NUM GO Z.
           IF WH-NUM > 0; IF WT-KEY(WH-NUM) = LOW-VALUE GO B ELSE GO Z.
           IF RET SET AB TO TRUE.
           IF FINE; IF AB  AND WI = 8 GO B
               ELSE IF AUF AND WI = 2 GO B.
           IF NOT AB AND NOT AUF GO B.
       Z.  EXIT.
      ******************************************************************
       ALLANZ SECTION.
       A.  ADD 202 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 9
               MOVE WI TO WD-POS
               DISPLAY WD-POS WITH HIGHLIGHT AT VDU-REL
               ADD 100 TO VDU-REL.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF.
           MOVE 3 TO WI.
       Z.  EXIT.
      *****************************************************************
       ART-ANZ SECTION.
       A.  CALL "CAUP" USING "0702351137001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Artikel " WITH HIGHLIGHT AT VDU-REL.
           PERFORM ALLANZ.
           START ARTIKEL KEY NOT < AR-AKEY INVALID PERFORM NO-REC GO Z.
       E.  READ ARTIKEL NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF AR-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM AR-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO AR-NUM GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO AR-KEY
               MOVE AR-NUM TO WH-NUM GO Z.
           IF AB GO E.
       N.  READ ARTIKEL PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF AR-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM AR-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       AR-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE AR-NUM TO WD-ART.
           DISPLAY WD-ART AT VDU-REL.
           ADD 6 TO VDU-REL.
           DISPLAY AR-BEZ WITH HIGHLIGHT AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE AR-KEY TO WT-KEY(WX).
       Z.  EXIT.
      *****************************************************************
       DEB-ANZ SECTION.
       A.  CALL "CAUP" USING "0703011180001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Kunden " WITH HIGHLIGHT AT VDU-REL.
           PERFORM ALLANZ.
           MOVE WV-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO DE-MCODE.
           START DEBITOR KEY NOT < DE-MCODE INVALID PERFORM NO-REC GO Z.
       E.  READ DEBITOR NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF DE-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM DEB-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
       M.  IF ESC OR KIST MOVE 0 TO DE-KTONR GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO DE-KEY
               COMPUTE WH-WERT WH-NUM = (DE-KTONR - 2) / 10 GO Z.
           IF AB GO E.
       N.  READ DEBITOR PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF DE-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM DEB-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       DEB-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT VDU-REL.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           ADD 10 TO VDU-REL.
           DISPLAY WT-TX WITH SIZE 53 HIGHLIGHT AT VDU-REL.
           ADD 55 TO VDU-REL.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE DE-KEY TO WT-KEY(WX).
       Z.  EXIT.
      *****************************************************************
       KO-SUCH SECTION.
       A.  CALL "CAUP" USING "0704351137001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Komponenten " with highlight AT VDU-REL.
           PERFORM ALLANZ.
           MOVE WV-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV10" WH-CREG.
           MOVE WT-TX TO LG-ABEZ.
           START LAGARTIK KEY NOT < LG-AKEY INVALID PERFORM NO-REC
               MOVE 0 TO LG-NUM GO Z.
       E.  READ LAGARTIK NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF LG-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM KO-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO LG-NUM GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO LG-KEY
               MOVE LG-NUM TO WH-NUM GO Z.
           IF AB GO E.
       N.  READ LAGARTIK PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF LG-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM KO-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       KO-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE LG-NUM TO WD-ART.
           DISPLAY WD-ART AT VDU-REL.
           ADD 5 TO VDU-REL.
           DISPLAY LG-BEZ WITH HIGHLIGHT AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE LG-KEY TO WT-KEY(WX).
       Z.  EXIT.
      *********************************************** Zusatzmatchcodes *
       KO-MATCH SECTION.
       A.  CALL "CAUP" USING "0708101160001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Matchcodes " with highlight AT VDU-LP.
           PERFORM ALLANZ.
           MOVE WV-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV13" WH-CREG.
           MOVE WT-TX TO LM-MCODE.
           MOVE 0 TO LM-LFD LM-LGNUM.
           IF WL-CA = 30
               START LAGMATCH KEY NOT < LM-AKEY INVALID PERFORM NO-REC
                   MOVE 0 TO LM-LGNUM GO X
               end-start GO E.
           MOVE LG-NUM TO LM-LGNUM.
           MOVE SPACE TO LM-MCODE.
           START LAGMATCH KEY NOT < LM-KEY INVALID PERFORM NO-REC
                   MOVE 0 TO LG-NUM GO L.
       E.  READ LAGMATCH NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WL-CA not = 30;
               IF WV-LGNUM = 0 MOVE LM-LGNUM TO WV-LGNUM
               end-if
               IF WV-LGNUM not = LM-LGNUM SET FINE TO TRUE
                   MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF LM-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM LM-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF EINF GO O.
           IF ESC OR KIST MOVE 0 TO LM-LGNUM LG-NUM GO X.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO LM-KEY
               MOVE LM-LGNUM TO LG-NUM WH-NUM GO X.
           IF AB GO E.
       N.  READ LAGMATCH PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WL-CA not = 30;
               IF WV-LGNUM not = LM-LGNUM SET FINE TO TRUE
                   MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF LM-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM LM-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
      *-----------------------------------------> Matchcodes einf�gen <-
       O.  ADD 1 TO WX.
       Q.  DISPLAY "<ret>= zus�tzlicher Matchcode, <esc>= Abbruch"
               AT 2301.
           COMPUTE VDU-LP = WX * 100 + 106 + VDU-ECK.
           MOVE LG-NUM TO WD-ART.
           DISPLAY WD-ART AT VDU-LP.
           MOVE LOW-VALUES TO LM-KEY.
           MOVE 1 TO WS.
           IF WT-KEY(WX) not = LOW-VALUE MOVE WT-KEY(WX) TO LM-KEY.
       R.  READ LAGMATCH INVALID MOVE LOW-VALUE TO WT-KEY(WX)
               MOVE LOW-VALUE TO LM-SATZ
               MOVE SPACE TO LM-MCODE
               MOVE 0 TO WS.
           IF LM-LGNUM = 0 and WS = 1
               DELETE LAGMATCH INVALID NEXT SENTENCE.
           MOVE LM-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV13" WH-CREG.
           COMPUTE VDU-LP = WX * 100 + 111.
           CALL "CAUP" USING "1200000113" WH-CREG.
           IF ESC GO X.
           IF AUF; IF WX not = 1 ADD -1 TO WX.
           IF AB; IF WX not = 9 and WT-KEY(WX) not = LOW-VALUE GO O.
           IF not RET GO Q.
           IF WT-TX = SPACE; IF LM-LGNUM not = 0 GO W
                             else SET ESC TO TRUE GO X.
           MOVE LG-NUM TO LM-LGNUM.
           CALL "CAUP" USING "20CONV13" WH-CREG.
           MOVE WT-TX TO LM-MCODE.
           IF WS = 1 REWRITE LM-SATZ not INVALID GO T.
       S.  WRITE LM-SATZ INVALID ADD 1 TO LM-LFD GO S.
       T.  CALL "CAUP" USING "21CONV13" WH-CREG.
           DISPLAY WT-TX(1:13) with highlight foreground-color 6
              AT VDU-LP.
           MOVE LM-KEY TO WT-KEY(WX).
           IF WX < 9 GO O.
           GO Q.
       W.  DELETE LAGMATCH INVALID NEXT SENTENCE.
           GO Q.
       X.  CLOSE LAGMATCH.
       Z.  EXIT.
      ******************************************************************
       LM-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE LM-LGNUM TO WD-ART.
           IF LG-NUM = LM-LGNUM GO E.
           MOVE LM-LGNUM TO LG-NUM.
       C.  READ LAGARTIK INVALID MOVE "- fehlt -" TO LG-BEZ.
           IF ZUGRIF PERFORM BESETZT GO C.
       E.  DISPLAY WD-ART AT VDU-REL.
           ADD 5 TO VDU-REL.
           MOVE LM-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV13" WH-CREG.
           DISPLAY WT-TX(1:13) with highlight foreground-color 6
                AT VDU-REL.
           ADD 14 TO VDU-REL.
           DISPLAY LG-BEZ with highlight foreground-color 3 AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE LM-KEY TO WT-KEY(WX).
       Z.  EXIT.
      ******************************************************************
       RP-SUCH SECTION.
       A.  CALL "CAUP" USING "0702351137001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Listung Rezepturen " with highlight AT VDU-REL.
           PERFORM ALLANZ.
           MOVE WH-MCODE TO RE-BEZ.
           START REZEPT KEY NOT < RE-AKEY INVALID PERFORM NO-REC
               MOVE 0 TO RE-NUM GO Z.
       E.  READ REZEPT NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF RE-KEY = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM RP-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO RE-NUM GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO RE-KEY
               MOVE RE-NUM TO WH-NUM GO Z.
           IF AB GO E.
       N.  READ REZEPT PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF RE-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM RP-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       RP-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 206 + VDU-ECK.
           MOVE RE-NUM TO WD-ART.
           DISPLAY WD-ART AT VDU-REL.
           ADD 5 TO VDU-REL.
           DISPLAY RE-BEZ with highlight AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE RE-KEY TO WT-KEY(WX).
       Z.  EXIT.
      *********************************** offene Lieferschein anzeigen *
       LFS-ANZ SECTION.
       A.  CALL "CAUP" USING "0710101140001" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lieferscheine " with highlight AT VDU-LP.
           PERFORM ALLANZ.
           START LFSCHEIN KEY NOT < LF-KEY INVALID GO Z.
       E.  READ LFSCHEIN NEXT IGNORE LOCK AT END MOVE 8 TO WI GO L.
           IF LF-KTONR NOT = DE-KTONR MOVE 8 TO WI
               SET FINE TO TRUE GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        PERFORM K-ZERLEG
                        IF WP-PRIME = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM LFS-DIS.
           IF WR NOT = 9 MOVE 9999 TO LF-ARNUM
               START LFSCHEIN KEY NOT < LF-KEY INVALID MOVE 8 TO WI
                     SET FINE TO TRUE GO L
               end-start GO E.
           MOVE 9 TO WI.
       L.  IF WT-ERFTAB = LOW-VALUE GO Z
           else PERFORM WASNUN.
       M.  IF ESC OR KIST MOVE 0 TO LF-KTONR WH-NUM GO Z.
           IF RET AND WH-NUM > 0 MOVE WT-KEY(WH-NUM) TO WP-PRIME
               MOVE WP-KTONR TO LF-KTONR
               MOVE WP-NUM TO LF-NUM
               MOVE WP-ARNUM TO LF-ARNUM
      *        MOVE WT-KEY(WH-NUM) TO LF-KEY
               MOVE LF-NUM TO WH-NUM GO Z.
           IF AB GO E.
       N.  READ LFSCHEIN PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO L.
           IF LF-KTONR NOT = DE-KTONR MOVE 2 TO WI
               SET FINE TO TRUE GO N.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        PERFORM K-ZERLEG
                        IF WP-PRIME = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM LFS-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************** LF-KEY Key zerlegen *
       K-ZERLEG SECTION.
       A.  MOVE LF-KTONR TO WP-KTONR.
           MOVE LF-NUM TO WP-NUM.
           MOVE LF-ARNUM TO WP-ARNUM.
       Z.  EXIT.
      ******************************************************************
       LFS-DIS SECTION.
       A.  COMPUTE VDU-LP = WX * 100 + 206 + VDU-ECK.
           MOVE LF-NUM TO WD-LFSNR.
           DISPLAY WD-LFSNR with highlight AT VDU-LP.
           MOVE LF-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           ADD 10 TO VDU-P.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 1 TO WR WX.
           PERFORM K-ZERLEG.
           MOVE WP-PRIME TO WT-KEY(WX).
       Z.  EXIT.
