      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PARCONV.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  PC.
       OBJECT-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARTIKEL    ASSIGN TO "PANARTIK.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY AR-KEY
                             ALTERNATE RECORD KEY AR-AKEY DUPLICATES
                             ALTERNATE RECORD KEY AR-BKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT KONSTANT   ASSIGN TO "PANEKONS.DAT"
                             ORGANIZATION RELATIVE ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT REZEPT     ASSIGN TO "PANREZEP.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY RE-KEY
                             ALTERNATE RECORD KEY RE-AKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT OREZEPT    ASSIGN TO "OLDREZEP.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ORE-KEY
                             ALTERNATE RECORD KEY ORE-AKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY "PANKONS.CPY".
       COPY "PANFDART.CPY".
       COPY "PARREZEP.CPY".
      *****************************************************************
       FD  OREZEPT       external       LABEL RECORD STANDARD
                                        RECORD 672 CHARACTERS.
       01  ORE-SATZ.
           03  ORE-KEY.
               05 ORE-NUM               PIC 9(4)        COMP.
           03  ORE-BEZ.
               05 ORE-AKEY              PIC X(10).
               05 ORE-TX                PIC X(15).
           03  ORE-DATUM                PIC 9(6)        COMP.
           03  ORE-KOMP                          OCCURS 20 INDEXED XR.
               05 ORE-KONUM             PIC 9(4)        COMP.
               05 ORE-KOBEZ             PIC X(20).
               05 ORE-MENGE             PIC 9(6)        COMP.
               05 ORE-PROZ REDEFINES
                  ORE-MENGE             PIC 999V999     COMP.
               05 ORE-TNAM              PIC 9(6)        COMP.
               05 ORE-EKP               PIC 9(4)V99     COMP.
           03  ORE-TEIL                 PIC 9(5)        COMP.
           03  ORE-GEW                  PIC 9(7)        COMP.
           03  ORE-BET                  PIC S9(7)V99    COMP-3.
           03  ORE-MAT                  PIC S9(7)V99    COMP-3.
           03  ORE-PREIS                PIC S9(5)V99    COMP-3.
           03  ORE-ART                  PIC 99          COMP.
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
           03  VDU-AB                  PIC 9999.
           03  WH-DRUNAM               PIC X(12)     VALUE "LPT1".
           03  WV-ARNUM                PIC 9(4)      COMP.
           03  WH-KOMP                 PIC X(32).
           03  WK-NUM                  PIC 9(8)V99   COMP.
           03  WH-MENGE                PIC S9(5)     COMP-3.
           03  WM-MERKER               PIC 9       COMP-3 VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
           03  WD-KOMPO                PIC Z.ZZ9.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-MGA                  PIC ZZ.ZZ9-.
           03  WD-MGC                  PIC ZZ9,99-.
           03  WD-MIN                  PIC ZZ9,99-.
           03  WD-POS                  PIC ZZ9.
           03  WD-ART                  PIC ZZZ9.
           03  WM-BS                   PIC 99.
           03  WT-KOTAB       OCCURS 9.
               05 WT-KOM               PIC 9999       COMP.
               05 WT-ART               PIC 99         COMP.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
      *----------------> Maschine, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WD-TEIG                 PIC ZZ.ZZ9-.
           03  WD-RPM                  PIC ZZZ.ZZ9-.
           03  WD-KZ                   PIC 9.
           03  WD-TG                   PIC ZZ9.
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
           03  WD-X                    PIC X.
       01  WH-PAR     external.
           03  WH-STK                  PIC 9(7)      COMP-3.
           03  WH-MIN                  PIC 9(7)V99   COMP-3.
           03  WH-MZUS                 PIC 9(3)V99   COMP-3.
           03  WH-DUMIN                PIC 9(5)V99   COMP-3.
           03  WH-BET                  PIC S9(9)V99  COMP-3.
           03  WS-BET                  PIC S9(9)V99   COMP-3.
       01  WH-KART    external         PIC 9999      COMP.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "PANEKONS.DAT" WH-CREG.
       DECL-H SECTION.         USE AFTER ERROR PROCEDURE ON OREZEPT.
       A.  CALL "CADECL" USING "OLDREZEP.DAT" WH-CREG.
       DECL-I SECTION.         USE AFTER ERROR PROCEDURE ON REZEPT.
       A.  CALL "CADECL" USING "PANREZEP.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 10 PERFORM KOMPCONV
               WHEN 20 PERFORM RPTCONV.
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
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
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
      ****************************************** Rezepturenverwaltung *
       RPTCONV SECTION.
       A.  MOVE "  Rezept.Konvtg." TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           OPEN INPUT OREZEPT.
           MOVE 0 TO ORE-NUM.
           START OREZEPT KEY NOT < ORE-KEY INVALID STOP RUN.
       E.  READ OREZEPT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE ORE-SATZ TO RE-SATZ.
           MOVE 0 TO RE-ZEIT(1) RE-ZEIT(2) RE-ZEIT(3) RE-ZEIT(4).
           MOVE 0 TO RE-SKP.
           MOVE ORE-BET TO RE-BET.
           MOVE ORE-MAT TO RE-MAT.
           MOVE ORE-PREIS TO RE-PREIS.
           MOVE ORE-ART TO RE-ART.
           DISPLAY RE-NUM AT 1010 " " RE-BEZ.
           WRITE RE-SATZ.
           GO E.
       X.  CLOSE OREZEPT.
       Z.  EXIT.
      *************************************** Komponentenkonvertierung *
       KOMPCONV SECTION.
       A.  CALL "CAUP" USING "0705101222010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Komp.Artumwandlg. " AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Komp.-Nr. auf Art " AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               MOVE LOW-VALUE TO WT-KOTAB(WX).
           MOVE 1 TO WX.
       D.  DISPLAY "<ret>= Komp. Nr., <esc>= Abbruch, < >= ab, < >= auf,
      -        " <Entf>= lîschen" AT 2301.
           DISPLAY "<Ende>= Umwandlung durchfÅhren" AT 2401.
           PERFORM ANZEIG.
           COMPUTE VDU-LP = WX * 100 + 200.
           MOVE WT-KOM(WX) TO WH-WERT.
           CALL "CAUP" USING "1000034004" WH-CREG.
           IF ENDE PERFORM UMWAND GO X.
           IF ESC GO X.
           IF AUF; IF WX > 1 ADD -1 TO WX GO D.
           IF AB; IF WX < 9 AND WT-KOM(WX + 1) NOT = 0
               ADD 1 TO WX GO D.
           IF ENTF PERFORM LOESCH GO D.
           IF NOT RET GO D.
           MOVE WH-WERT TO WT-KOM(WX) WD-KOMPO.
           DISPLAY WD-KOMPO with highlight AT VDU-LP.
      *----------------------------------------------> Komponentenart <-
       G.  DISPLAY "énderung auf Komponentenart x!" AT 2401.
           DISPLAY "<>= zurÅck, <ret>= Art" AT 2301.
           COMPUTE VDU-LP = WX * 100 + 200.
           CALL "CAUP" USING "1000151001" WH-CREG.
           IF WOLI GO D.
           IF NOT RET GO G.
           MOVE WH-WERT TO WT-ART(WX) WD-ART.
           DISPLAY WD-ART with highlight AT VDU-LP.
           IF WX < 9 ADD 1 TO WX.
           GO D.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       LOESCH SECTION.
       A.  MOVE 0 TO WT-KOM(WX) WT-ART(WX).
           PERFORM VARYING WZ FROM WX BY 1 UNTIL WZ > 8
               MOVE WT-KOM(WZ + 1) TO WT-KOM(WZ)
               MOVE WT-ART(WZ + 1) TO WT-ART(WZ)
               MOVE 0 TO WT-KOM(WZ + 1) WT-ART(WZ + 1).
           IF WT-KOM(WX) = 0 AND WX > 1 ADD -1 TO WX.
       Z.  EXIT.
      ******************************************************************
       ANZEIG SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 9
               IF WT-KOM(WZ) NOT = 0 MOVE WT-KOM(WZ) TO WD-KOMPO
                   COMPUTE VDU-LP = WZ * 100 + 203 + VDU-ECK
                   MOVE WT-ART(WZ) TO WD-ART
                   DISPLAY WD-KOMPO with highlight AT VDU-LP
                      "       " WD-ART with highlight.
       Z.  EXIT.
      ******************************************************************
       UMWAND SECTION.
       A.  CALL "CAUP" USING "0713200337000" WH-CREG.
           MOVE 0 TO RE-NUM.
           START REZEPT KEY NOT < RE-KEY INVALID STOP RUN.
       E.  READ REZEPT NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE RE-NUM TO WD-KOMPO.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KOMPO AT VDU-LP " " RE-BEZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               IF RE-KONUM(WX) NOT = 0
                  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 9
                      IF RE-KONUM(WX) = WT-KOM(WZ)
                          MOVE RE-TNAM(WX) TO WH-TNAM
                          MOVE WT-ART(WZ) TO WH-T
                          MOVE WH-TNAM TO RE-TNAM(WX).
           REWRITE RE-SATZ.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
