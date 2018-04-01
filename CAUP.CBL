      $SET LINKCOUNT "192" ANS85"SYNTAX" BOUND NOALTER NESTCALL
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        CAUP.
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       OBJECT-COMPUTER.     PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA
                        ENVIRONMENT-NAME IS ENV-SET
                        ENVIRONMENT-VALUE IS ENV-WERT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASKEN     ASSIGN TO WH-MASK
                             ORGANIZATION RELATIVE ACCESS DYNAMIC
                             RELATIVE KEY WH-MAKEY
      *                      LOCK EXCLUSIVE
                             FILE STATUS IS WF-STATUS.
           SELECT BEDIEN     ASSIGN TO "BEDIEN.DAT"
                             ORGANISATION RELATIVE ACCESS DYNAMIC
                             RELATIVE KEY WH-BEKEY
                             LOCK AUTOMATIC
                             FILE STATUS IS WF-STATUS.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  MASKEN.
       01  MA-SATZ.
           03  MA-ATTR                 PIC X(2000).
           03  FILLER                  PIC X(48).
           03  MA-ZEICH                PIC X(2000).
           03  MA-CALL.
               05 MA-CA                PIC 99.
               05 MA-ECK               PIC 9999        VALUE ZERO.
               05 FILLER REDEFINES MA-ECK.
                  07 MA-VL             PIC 99.
                  07 MA-VP             PIC 99.
               05 MA-GROSS             PIC 9999        VALUE ZERO.
               05 FILLER REDEFINES MA-GROSS.
                  07 MA-AZ             PIC 99.
                  07 MA-SZ             PIC 99.
               05 MA-KO                PIC 99.
               05 MA-MA                PIC 9.
               05 MA-ATT               PIC XX.
           03  MA-CURSOR               PIC 9999.
           03  FILLER                  PIC X(27).
      ******************************************************************
       FD  BEDIEN.
       01  BE-SATZ.
           03  BE-TX                   PIC X(62).
      ******************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
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
           03  WL-ATTR                 PIC 99.
       01  WF-REG.
           03  VDU-AB.
               05 VDU-LA               PIC 99.
               05 VDU-PA               PIC 99.
           03  VDU-ZP                  PIC 9999.
           03  VDU-DI.
               05 VDU-DL               PIC 99.
               05 VDU-DP               PIC 99.
           03  WX-X                    PIC X.
           03  WK-RM                   PIC 99        COMP.
           03  WH-MAKEY                PIC 9(5)      VALUE 0.
           03  WM-CALL                 PIC X(20).
           03  WH-MASK.
               05 FILLER               PIC X(9)    VALUE "C:\MASKE.".
               05 WH-USE               PIC 999     VALUE 001.
           03  WM-ATTR                 PIC X    COMP-5.
           03  WX-ATTR                 PIC X    COMP-5.
           03  WE-EXTV.
               05 WR-VAL               PIC 99.
      *-----------------------------------------> Bildschirmausgaben <-
           03  WX-ABUF                 PIC X(2000).
           03  WX-ZBUF.
               05  WX-ZEIL                    OCCURS 25 INDEXED BZ BW.
                   07 WX-POS     PIC X COMP-5 OCCURS 80 INDEXED BQ BV.
           03  WX-PARM.
               05 WX-L                 PIC 99   COMP-X.
               05 WX-P                 PIC 99   COMP-X.
           03  WX-PS REDEFINES WX-PARM PIC 9999 COMP.
           03  WX-LG                   PIC 9999 COMP.
           03  WX-ZEI                  PIC X(80).
           03  WX-ATT                  PIC X(80).
           03  WB-MASK                 PIC 999       COMP.
           03  WI                      PIC 99        COMP-X.
           03  WQ REDEFINES WI         PIC X.
           03  WB                      PIC 99        COMP-X.
           03  WH-BEKEY                PIC 9(5)      VALUE 0.
           03  WT-Z.
               05 WR-Z                 PIC 99     COMP  OCCURS 12.
           03  WC-WERT                 PIC S9(12)V999 COMP.
           03  WM-N                    PIC 9      COMP.
           03  WA-TAG                  PIC 9(8)   COMP.
      *--------------------------> fuer Datumsaufbereitung im DATUM <--
       01  WT-DK    external.
           03  WR-TN                   PIC X(10)  OCCURS 7.
           03  WT-KON                  PIC 99     OCCURS 12.
       01  WT-DAT.
           03  WT-CONV                 PIC X(256)    VALUE "
      -    "                     !""#$%&'()*+,-./0123456789:;<=>?@AéBCDE
      -    "FGHIJKLMNOôPQRSTUöVWXYZ^_`aÑbcdefghijklmnoîpqrstuÅvwXYZ~ÄÅÇ
      -    "ÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü
      -    "
      -    "     ".
           03  wr-cv redefines wt-conv pic x      occurs 256.
           03  WT-RECO                 PIC X(256)    VALUE "
      -    "                     !""#$%&'()*+,-./0123456789:;<=>?@ACDEFG
      -    "HIJKLMNOPRSTUVWYZ[\]^_`***ACDEFGHIJKLMNOPRSTUVWYZ[\]~Äx**XÇ
      -    "ÉBÖÜáàâäãåçBèêëíìQïñóòQXõúùûü
      -    "                                  U
      -    "     ".
           03  wr-rv redefines wt-reco pic x      occurs 256.
       01  WX-FEHL.
           03  WD-FA                   PIC X(8)     VALUE "Fehler: ".
           03  WD-SA                   PIC 9.
           03  WD-B                    PIC XXX      VALUE " / ".
           03  WD-SB                   PIC ZZ9.
           03  WD-C                    PIC X.
           03  DATEI                   PIC X(12)    VALUE SPACE.
           03  WD-D                    PIC X(39)
                    VALUE " - Programmabbruch - weiter mit <ret>: ".
           03  WD-X                    PIC X.
      *---------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON BEDIEN.
       A.  MOVE "BEDIEN.DAT" TO DATEI PERFORM DECL-X.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON MASKEN.
       A.  MOVE WH-MASK      TO DATEI PERFORM DECL-X.
       DECL-X SECTION.
       A.  IF ZUGRIF GO Z.
           IF WF-STATUS = "35" GO Z.
           IF WF-STATUS = "41" SET ZUGRIF TO TRUE GO Z.       *>Windows
           IF WF-SA > 8  MOVE WF-SA TO WD-SA
               MOVE WF-SB TO WD-SB
               MOVE " /" TO WD-B
           else MOVE WF-STATUS TO WD-SB
                MOVE SPACE TO WD-B.
           DISPLAY WX-FEHL AT 2401.
           ACCEPT WD-X AT 2468.
           STOP RUN.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CREG TO WH-CREG.
           MOVE WL-CALL TO WH-CALL.
           MOVE WL-CA TO WK-RM.
           MOVE VDU-LP TO VDU-RES.
           EVALUATE WL-CA
               WHEN 00 PERFORM EINGABE
               WHEN 10 PERFORM EINGABE
               WHEN 01 PERFORM EINGABE
               WHEN 11 PERFORM EINGABE
               WHEN 02 PERFORM PLUSTEXT
               WHEN 12 PERFORM PLUSTEXT
               WHEN 03 PERFORM DATUM
               WHEN 04 PERFORM DATDREH
               WHEN 05 PERFORM DATPRUEF
               WHEN 06 PERFORM KOPF
               WHEN 07 PERFORM FENSTER
               WHEN 08 PERFORM FENSTER
               WHEN 09 PERFORM DISPL
               WHEN 13 PERFORM SCROLL   GO Z
               WHEN 14 PERFORM SCROLL   GO Z
               WHEN 15 PERFORM NUMCONV
               WHEN 16 PERFORM SCROLL
               WHEN 17 PERFORM SCROLL
               WHEN 18 PERFORM SCROLL   GO Z
               WHEN 19 PERFORM BEDIENEN
               WHEN 20 PERFORM ALPHACONV
               WHEN 21 PERFORM ALPHACONV
               WHEN 22 PERFORM SCROLL
               WHEN 27 PERFORM FENSTER
               WHEN 97
               WHEN 98
               WHEN 99 PERFORM USERS.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ************************************************** 06 ************
       KOPF SECTION.
       A.  IF WM-ATTR(1:) = SPACE MOVE 78 TO WM-ATTR.
           IF WH-CALL(3:4) not = "KOPF" MOVE WH-CALL(3:1) TO WQ
               MOVE WI TO WM-ATTR.
           MOVE "1301012480000" TO WH-CALL PERFORM SCROLL.
           MOVE "9901010280000" TO WH-CALL PERFORM SCROLL.
           DISPLAY WK-KOPF AT 0101.
           MOVE ALL "ƒ" TO WX-ZEI.
           MOVE "¡" TO WX-ZEI(24:1) WX-ZEI(50:1).
           DISPLAY WX-ZEI AT 0201.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           MOVE "1324012480000" TO WH-CALL.
           PERFORM SCROLL.
       Z.  EXIT.
      ******************************************************************
       LOE34 SECTION.
       A.  MOVE WH-CALL TO WM-CALL.
           MOVE 1323012580000 TO WH-CALL.
           PERFORM SCROLL.
           MOVE WM-CALL TO WH-CALL.
       Z.  EXIT.
      *************************************************** 01/11 ********
       NUMCONV SECTION.
       A.  MOVE 0 TO WH-NEG WH-NUM.
           IF WL-AZ(1:1) NOT NUMERIC MOVE WL-AZ(1:1) TO WK-ANZ(1:1)
               ADD -55 TO WK-ANZ
               MOVE "0" TO WL-AZ(1:1)
               COMPUTE WK-ANZ = WK-ANZ * 10 + WL-AZ
           else MOVE WL-AZ TO WK-ANZ.
           SET KX RX MX TO 13.
       B.  IF WV-MC(MX) = SPACE GO D.
           IF WV-MC(MX) = "-" MOVE 1 TO WH-NEG GO D.
           IF WV-MC(MX) = "," OR WV-MC(MX) NUMERIC GO F.
       D.  IF MX NOT = 1 SET MX DOWN BY 1 GO B.
           GO Z.
       F.  IF WV-MC(MX) NUMERIC MOVE WV-MC(MX) TO WR-NUM(RX)
           else IF WV-MC(MX) = "," SET KX TO RX
                    SET RX UP BY 1
               else IF WV-MC(MX) = "-" MOVE 1 TO WH-NEG
                    SET RX UP BY 1 else SET RX UP BY 1.
           IF MX NOT = 1 SET MX RX DOWN BY 1 GO F.
           DIVIDE 10 INTO WK-ANZ GIVING WK-ANZ REMAINDER WK.
           SUBTRACT WK FROM 13 GIVING WK.
           IF KX < WK DISPLAY "zuviele Komma, " AT 2401 GO X.
           MOVE WH-NUM TO WC-WERT.
           SET WK TO KX.
           IF KX = 13 GO H.
       G.  DIVIDE 10 INTO WC-WERT.
           IF KX < 12 SET KX UP BY 1 GO G.
       H.  MOVE WC-WERT TO WH-WERT.
           IF WH-NEG = 1 MULTIPLY WH-WERT BY -1 GIVING WH-WERT.
           SUBTRACT WK FROM 13 GIVING WK.
           SET RX UP BY WK.
           SET WK TO RX.
           SUBTRACT WK FROM 14 GIVING WK.
           IF WK NOT > WK-ANZ GO Z.
           DISPLAY "zuviele Stellen, " AT 2401.
       X.  PERFORM WEITER.
           MOVE 0 TO WH-NUM.
           MOVE 99 TO WX-TASTE.
       Z.  EXIT.
      *************************************************** 00/01/10/11 **
       EINGABE SECTION.
       A.  IF WL-VL = 0; IF WL-CA < 10 MOVE VDU-L TO WL-VL
               else ADD VDU-EL TO VDU-L
                    MOVE VDU-L TO WL-VL
           else IF WL-CA > 9 ADD VDU-EL TO WL-VL.
           IF WL-VP = 0; IF WL-CA < 10 MOVE VDU-P TO WL-VP
               else ADD VDU-EP TO VDU-P
                    MOVE VDU-P TO WL-VP
           else IF WL-CA > 9 ADD VDU-EP TO WL-VP.
           MOVE WL-VL TO VDU-LA.
           MOVE WL-VP TO VDU-P.
           MOVE WL-VL TO VDU-L.
           SET MX TO 1.
           MOVE SPACE TO WV-MCODE.
       B.  SET VDU-PA TO MX.
           ADD WL-VP -1 TO VDU-PA.
           DISPLAY LOW-VALUE AT VDU-AB.
       C.  CALL "CBL_GET_KBD_STATUS" using WK-RM.
           IF WK-RM = 0 GO C.
           CALL "CBL_READ_KBD_CHAR" USING WX-X.
           IF WX-X = X"00" CALL "CBL_READ_KBD_CHAR" USING WX-X
               DISPLAY "falsche Taste" AT 2467 GO B.
           IF WX-X = X"0D" GO L.
           IF WX-X = X"08"; IF MX = 1 CALL X"E5"
                            else SET MX DOWN BY 1
                                 ADD -1 TO VDU-PA
                                 MOVE SPACE TO WV-MC(MX)
               END-IF MOVE WV-MC(MX) TO WX-X
                      DISPLAY WX-X AT VDU-AB GO B.
           IF MX > WL-SZ CALL X"E5" GO B
           else MOVE WX-X TO WV-MC(MX)
                 DISPLAY WX-X AT VDU-AB
                 SET MX UP BY 1
                 ADD 1 TO VDU-PA
                 GO B.
       L.  MOVE WX-TASTE TO WS-TASTE.
           PERFORM LOE34.
           IF WOLI OR ESC GO Z.
           PERFORM NUMCONV.
           MOVE WV-MCODE TO WH-MCODE.
           IF WL-CA = 01 OR = 11 PERFORM TEILDAT
               MOVE WS-TASTE TO WX-TASTE.
       Z.  EXIT.
      ************************************************** 02/12 *********
       PLUSTEXT SECTION.
       A.  IF WL-VL = 0; IF WL-CA < 10 MOVE VDU-L TO WL-VL
               else ADD VDU-EL VDU-L GIVING WL-VL
           else IF WL-CA = 12 ADD VDU-EL TO WL-VL.
           MOVE WL-VL TO VDU-L.
           IF WL-VP = 0; IF WL-CA < 10 MOVE VDU-P TO WL-VP
               else ADD VDU-EP VDU-P GIVING WL-VP
           else IF WL-CA = 12 ADD VDU-EP TO WL-VP.
           MOVE WL-VP TO VDU-P VDU-PA.
       B.  SET FX TX TO WL-AZ.
           SET FY TY TO WL-SZ.
           COMPUTE VDU-LA = WL-AZ - 1 + VDU-L.
           IF TX = 1 GO D.
       C.  MOVE WR-ADR(TX) TO WT-TX.
           IF TX > 1; IF WT-TX = SPACE SET TX DOWN BY 1
               SUBTRACT 1 FROM VDU-LA GO C.
       D.  IF FY >= 1; IF WR-TX(FY) = SPACE SET FY DOWN BY 1 GO D.
           SET VDU-PA TO FY.
           SET FY UP BY 1.
           ADD WL-VP TO VDU-PA.
           GO F.
       E.  CALL X"E5".
       F.  DISPLAY LOW-VALUE AT VDU-AB.
           CALL "CBL_GET_KBD_STATUS" using WK-RM.
           IF WK-RM = 0 GO F.
           CALL "CBL_READ_KBD_CHAR" USING WX-X.
           IF WX-X = X"00" CALL "CBL_READ_KBD_CHAR" USING WX-X GO E.
           IF WH-PG = 99 DISPLAY " " AT VDU-AB.
           IF WX-X = X"0D";
               EVALUATE TRUE
                  WHEN AUF
                  WHEN AB
                  WHEN WOLI
                  WHEN SAPO
                  WHEN SEPO
                  WHEN SENT GO I
                  WHEN WORE GO G
                  WHEN ENTF IF WM-N = 0 GO W
                            else GO H
                  WHEN OTHER GO H.
           IF WX-X = X"08" GO J.
           IF FY > TY GO E.
           SET WR TO FY.
           MOVE WT-TX(WR:) TO WX-ZEI(WR + 1:).
           MOVE WX-ZEI(WR:) TO WT-TX(WR:).
           MOVE WX-X TO WR-TX(FY).
           IF WH-PG NOT = 99 PERFORM DISPL.
       G.  IF FY <= TY SET FY UP BY 1
               ADD 1 TO VDU-PA
               else GO E.
           GO F.
       H.  MOVE WT-TX TO WR-ADR(TX).
           IF NOT RET AND NOT TABL AND NOT SF1 GO X.
           IF TX < FX ADD 1 TO VDU-LA VDU-L SET TX UP BY 1
               MOVE WR-ADR(TX) TO WT-TX
                   IF AB GO F
                   else SET FY TO 1 MOVE WL-VP TO VDU-PA GO F.
           GO X.
       I.  IF SENT MOVE SPACE TO WT-TX
                   PERFORM DISPL
                   SET SAPO TO TRUE.
           IF SAPO MOVE WL-VP TO VDU-PA
               SET FY TO 1 GO F.
           IF SEPO SET FY TO WL-SZ GO D.
           IF AUF; IF WL-AZ > 1;
                       IF TX NOT = 1 MOVE WT-TX TO WR-ADR(TX)
                           SUBTRACT 1 FROM VDU-LA
                           SET TX DOWN BY 1
                           MOVE WR-ADR(TX) TO WT-TX GO F
                       else GO E
                   else GO X.
           IF AB; IF WL-AZ > 1;
                      IF TX < FX MOVE WT-TX TO WR-ADR(TX)
                          ADD 1 TO VDU-LA
                          SET TX UP BY 1
                          MOVE WR-ADR(TX) TO WT-TX GO F
                      else GO E
                  else GO X.
           IF NOT WOLI GO F.
       J.  IF FY > 1 SET FY DOWN BY 1
               SUBTRACT 1 FROM VDU-PA
           else IF WOLI MOVE WT-TX TO WR-ADR(TX)
                   IF TX > 1 SUBTRACT 1 FROM VDU-LA
                       SET TX DOWN BY 1
                       MOVE WR-ADR(TX) TO WT-TX
                   else PERFORM LOE34 GO Z
               else GO E.
       W.  IF NOT WOLI SET WR TO FY
               MOVE WT-TX(WR + 1:) TO WT-TX(WR:)
               IF WH-PG NOT = 99 PERFORM DISPL.
           GO F.
       X.  PERFORM LOE34.
           IF WL-AZ > 1 PERFORM ABSTELLEN.
       Z.  EXIT.
      ***************************************** Textuebergabe in WT-TX *
       DISPL SECTION.
      *--> WH-CALL :    WL-CA  /  WL-VL  /  L-VP  /  WL-AZ  /  WL-SZ
      *                  "09" at  Line       Pos.           mit LÑnge
      *------------------- wenn WL-VL bzw. WL-VP = 0 -> aus VDU-LP ----*
       A.  IF WL-CA not = 9 MOVE VDU-LA TO VDU-DL
                            MOVE VDU-P TO VDU-DP GO C.
           IF WL-VL = 0 MOVE VDU-L TO WL-VL.
           IF WL-VP = 0 MOVE VDU-P TO WL-VP.
           COMPUTE WX-L = WL-VL - 1.
           MOVE WL-VP TO VDU-DP.
       C.  MOVE WL-SZ TO WX-LG.
           DISPLAY WT-TX WITH SIZE WX-LG HIGHLIGHT AT VDU-DI.
       Z.  EXIT.
      ******************************************************************
       ABSTELLEN SECTION.
       A.  SET BX TX TO 1.
           MOVE SPACE TO WT-BEZ.
       B.  MOVE WR-ADR(TX) TO WT-TX.
           SET TY TO WL-SZ.
           MOVE 1 TO WX.
       C.  IF WR-TX(TY) NOT = SPACE SET WX TO TY
               SET BX UP BY WX
               MOVE "#" TO WR-BEZ(BX)
               SET BX DOWN BY 1
               ADD 1 TO WX
               GO D.
           IF TY NOT = 1 SET TY DOWN BY 1 GO C.
           MOVE "#" TO WR-BEZ(BX).
           GO E.
       D.  MOVE WR-TX(TY) TO WR-BEZ(BX).
           IF TY NOT = 1 SET TY BX DOWN BY 1 GO D.
       E.  SET BX UP BY WX.
       F.  IF BX > 240 DISPLAY "Zuviele Zeichen eingegeben! " AT 2401
                PERFORM WEITER GO Z.
           IF TX < FX SET TX UP BY 1 GO B.
       Z.  EXIT.
      ************************************************** 07 ************
       FENSTER SECTION.
      *--------> aus CALL "00 / WL-VL / WL-VP / WL-AZ / WL-SZ" WH-CREG *
      *                   Ecke:   LINE/POS   Groesse: LINE/POS
      *--------> daher ergibt sich folgendes Fenster:
      *                   obere/untere Zeile:  WL-VL (=BZ) / WL-AZ (=BW)
      *                   linke/rechte Spalte: WL-VP (=BQ) / WL-SZ (=BV)
      *-----------------------------------------------------------------
       A.  MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           IF WL-CA = 07 or 27 GO D.
      *---------------------------> altes Fenster lesen und ausgeben <-
           READ MASKEN IGNORE LOCK.
           SUBTRACT 1 FROM WH-MAKEY.
           MOVE MA-CALL TO WH-CALL.
           MOVE WL-ECK TO VDU-ECK.
           SUBTRACT 202 FROM VDU-ECK.
           MOVE MA-CURSOR TO VDU-LP.
           MOVE MA-ZEICH TO WX-ZBUF.
           MOVE MA-ATTR TO WX-ABUF.
           CALL "CBL_WRITE_SCR_CHATTRS" USING
                WX-PS WX-ZBUF WX-ABUF WX-LG.
           GO Z.
      *------------------------> altes Fenster auslesen u. speichern <-
       D.  ADD 1 TO WH-MAKEY.
           MOVE VDU-LP TO MA-CURSOR.
           CALL "CBL_READ_SCR_CHATTRS"
               USING WX-PS MA-ZEICH MA-ATTR WX-LG.
           REWRITE MA-SATZ INVALID WRITE MA-SATZ.
           UNLOCK MASKEN.
           MOVE MA-ATTR TO WX-ZBUF.
           SUBTRACT 101 FROM WL-ECK GIVING VDU-ECK.
           ADD VDU-ECK TO WL-GROSS.
      *---------------------------------> Rahmen zeichnen und lîschen <-
           SET BZ TO WL-VL.
           SET BW TO WL-AZ.
           SET BQ TO WL-VP.
           SET BV TO WL-SZ.
           IF WL-CA = 27 MOVE WL-ATTR TO WX-ATTR
                    else MOVE 03 TO WX-ATTR.
       F.  MOVE WX-ATTR TO WX-POS(BZ, BQ).
           IF BQ < BV SET BQ UP BY 1 GO F.
       G.  SET BZ UP BY 1.
           SET BQ TO WL-VP.
           IF BZ NOT = BW
               MOVE WX-ATTR TO WX-POS(BZ, BQ) WX-POS(BZ, BV) GO G.
       H.  MOVE WX-ATTR TO WX-POS(BZ, BQ).
           IF BQ < BV SET BQ UP BY 1 GO H.
           MOVE WX-ZBUF TO WX-ABUF.
           SET BZ TO WL-VL.
           SET BW TO WL-AZ.
           SET BQ TO WL-VP.
           SET BV TO WL-SZ.
       I.  MOVE MA-ZEICH TO WX-ZBUF.
           MOVE "…" TO WX-POS(BZ, BQ)(1:).
           MOVE "º" TO WX-POS(BW, BV)(1:).
           MOVE "ª" TO WX-POS(BZ, BV)(1:).
           MOVE "»" TO WX-POS(BW, BQ)(1:).
           SET BZ UP BY 1.
           SET BQ TO WL-VP.
           SET BW DOWN BY 1.
       L.  MOVE "∫" TO WX-POS(BZ, BQ)(1:) WX-POS(BZ, BV)(1:).
           IF BZ < BW SET BZ UP BY 1 GO L.
           SET BQ UP BY 1.
           SET BV DOWN BY 1.
           SET BZ TO WL-VL.
           SET BW TO WL-AZ.
       K.  MOVE "Õ" TO WX-POS(BZ, BQ)(1:) WX-POS(BW, BQ)(1:).
           IF BQ < BV SET BQ UP BY 1 GO K.
           ADD 101 TO WL-ECK.
           SUBTRACT 101 FROM WL-GROSS.
           MOVE WH-CALL TO MA-CALL.
      *------------------------------> gezeichnetes Fenster ausgeben <-
       P.  CALL "CBL_WRITE_SCR_CHATTRS"
               USING WX-PS WX-ZBUF WX-ABUF WX-LG.
           PERFORM SCROLL.
       Z.  EXIT.
      ************************************************** 13/14/16/17 ***
       SCROLL SECTION.
      *------------------> 13/14 = BS-LOE/SCROL * 16/17 = FS-LOE/SCROL *
      *------> WH-CALL: WL-CA / WL-VL / WL-VP / WL-AZ / WL-SZ / WL-KO  *
      *------>          "xx"  ab Zeile/Posit. Anz.Zeilen/Pos. 00 = auf *
      *-----> 18: VDU-LP = relative Festeradresse ab der gelîscht wird *
      *-----------------------------------------------------> 01 = ab  *
       A.  IF WL-CA = 22; IF WH-CALL(3:8) NUMERIC
                              MOVE 0 TO WL-KO WL-MA GO C
                          end-if IF WH-CALL(3:4) NUMERIC
                                     MOVE MA-CALL TO WH-CALL
                                     MOVE VDU-L TO WL-VL
                                     MOVE 0 TO WL-KO
                                     MOVE WK-RM TO WL-CA GO C.
           IF WL-CA = 16 OR = 17 OR 22 MOVE MA-CALL TO WH-CALL
               MOVE WK-RM TO WL-CA GO C.
      *    IF WL-CA = 18 MOVE MA-GROSS TO WL-GROSS
      *                  ADD MA-VP VDU-P GIVING WL-VP
      *                  ADD MA-VL VDU-L GIVING WL-VL GO C.
           IF WL-CA = 18 MOVE MA-CALL(3:) TO WH-CALL(3:)
                         ADD VDU-P TO WL-VP
                         ADD VDU-L TO WL-VL GO C.
           IF WL-VL = 0 MOVE VDU-L TO WL-VL.
           IF WL-VP = 0 MOVE VDU-P TO WL-VP.
       C.  IF WL-AZ < WL-VL COMPUTE WX-L = WL-VL - 1
               else COMPUTE WX-L = WL-AZ - 1.
           IF WL-CA = 7 OR 13 OR 14 OR 27 GO D.
           IF WL-MA = 1 ADD 4 TO WL-VP.
           IF WL-MA = 2 SUBTRACT 5 FROM WL-SZ.
           ADD WL-KO TO WL-VL.
       D.  COMPUTE WX-P = WL-VP - 1.
           COMPUTE WX-LG = WL-SZ - WL-VP + 1.
           MOVE ALL SPACES TO WX-ZEI.
      *---------------------------------------------------> testweise <-
      *    move wl-attr(1:1) to wx-x.
      *    if wx-x = 0 move all x"07" to wx-att
      *        else inspect wx-att replacing characters by wx-x.
971001*    IF WL-CA = 99 MOVE ALL X"4E" TO WX-ATT
971001     IF WL-CA = 99 MOVE WM-ATTR(1:) TO WX-X
                    INSPECT WX-ATT REPLACING CHARACTERS BY WX-X
                    else MOVE ALL X"07" TO WX-ATT.
           IF WL-CA = 07 OR 13 OR 16 OR 18 OR 27 MOVE 0 TO WL-CA.
       G.  if wl-ca = 22 subtract 1 from wx-l
              call "cbl_read_scr_chattrs" using wx-parm wx-zei wx-att
              wx-lg
              add 1 to wx-l.
           CALL "CBL_SWAP_SCR_CHATTRS"
               USING WX-PARM WX-ZEI WX-ATT WX-LG.
971001*    IF WL-CA = 00 OR 99 MOVE SPACE TO WX-ZEI
971001     IF WL-CA = 00 or 99 MOVE SPACE TO WX-ZEI
971001         IF WL-CA = 99 MOVE WM-ATTR(1:) TO WX-X
                   INSPECT WX-ATT REPLACING CHARACTERS BY WX-X
      *             MOVE ALL X"4E" TO WX-ATT
               else MOVE ALL X"07" TO WX-ATT.
      *        if wx-x = 0 move all x"07" to wx-att
      *    inspect wx-att replacing characters by wx-x.
           if wl-ca = 22; IF WX-L = WL-VL
                     move space to wx-zei
                     MOVE ALL X"07" TO WX-ATT
                     subtract 1 from wx-l
                     call "cbl_write_scr_chattrs"
                     using wx-parm wx-zei wx-att wx-lg go z.
           IF WX-L NOT < WL-VL SUBTRACT 1 FROM WX-L GO G.
       Z.  EXIT.
      *************************** SET MASKE = STATION ** 99 ************
       USERS SECTION.
       A.  IF WL-CA = 97 MOVE "1301012580" TO WH-CALL
               PERFORM SCROLL
               CLOSE MASKEN
               DELETE FILE MASKEN
               GO Z.
           DISPLAY "MASKE" UPON ENV-SET.
           ACCEPT WE-EXTV FROM ENV-WERT.
           IF WE-EXTV = SPACE DISPLAY "SET MASKE=nn fehlt!" AT 2401
           else MOVE 1 TO WR-VAL.
                IF WR-VAL(2:1) = SPACE MOVE 0 TO WR-VAL(2:1)
                   DIVIDE 10 INTO WR-VAL GIVING WR-VAL.
           MOVE WR-VAL TO WH-USE.
       C.  OPEN I-O MASKEN.
           IF ZUGRIF ADD 1 TO WH-USE GO C.
           MOVE 0 TO WM-N.
           IF WL-CA = 98 MOVE 1 TO WM-N
               MOVE 99 TO WL-CA WK-RM.
           MOVE WH-CALL TO MA-CALL.
           MOVE WH-USE TO WK-BS.
           MOVE 13 TO WL-CA.
           PERFORM SCROLL.
           MOVE X"02004B02004D02004802005001090123011B02003B02003C02003D
      -          "02003E02005402005502005602005202004702004902005302004F
      -          "02005102009202009302007702007500" TO WX-FTAB.
           MOVE "Sonntag   Montag    Dienstag  Mittwoch  DonnerstagFreit
      -         "ag   Samstag   312931303130313130313031" TO WT-DK.
           CALL X"B0" USING WU-FUNC WU-KEYTAB.
       Z.  EXIT.
      *************************************************** 03 ***********
       DATUM SECTION.
       A.  IF WZ-DATUM NOT = 0 PERFORM DATPRUEF GO D.
       B.  DISPLAY "Datum: (leer = keine énderung)" AT 2301.
           MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           MOVE "0101536008" TO WH-CALL PERFORM EINGABE.
           MOVE "1323012480000" TO WH-CALL PERFORM SCROLL.
       D.  IF WZ-DATUM = 0 GO B.
           IF WH-PG = 0 MOVE WZ-DATUM TO WH-DATUM
                        MOVE WX-DATUM TO WM-DATUM.
           COMPUTE WA-TAG = FUNCTION INTEGER-OF-DATE(WX-DATUM).
           DIVIDE 7 INTO WA-TAG GIVING WA-TAG REMAINDER WO-TGN.
260705*    MOVE WR-TN(WO-TGN + 1) TO WK-TAG WO-TAG.
           MOVE WR-TN(WO-TGN + 1) TO WO-TAG.
           IF WH-PG = 0 DISPLAY VDU-DATUM AT 0153
               MOVE WO-TAG TO WK-TAG
               DISPLAY WK-TAG AT 0164
               MOVE VDU-DATUM TO WK-DATUM.
       Z.  EXIT.
      ********************************************** Teildatumseingabe *
       TEILDAT SECTION.
       A.  IF FEHLER MOVE 0 TO WZ-DATUM WX-DATUM GO Z.
           MOVE WH-WERT TO WC-DATUM.
           IF WH-WERT = 0 MOVE 0 TO WZ-DATUM WX-DATUM GO Z.
           IF WH-NUM = 0 GO X.
           IF WC-TAG > 0 PERFORM DATDREH GO X.
           IF WC-MONAT > 0 MOVE WC-MONAT TO WZ-TAG
               MOVE WC-JAHR TO WZ-MONAT
           else MOVE WC-JAHR TO WZ-TAG.
       X.  PERFORM DATPRUEF.
       Z.  EXIT.
      ************************************************** 04 ************
       DATPRUEF SECTION.
       A.  COMPUTE WX-DATUM = WZ-DATUM + 19000000.
           IF WZ-JAHR < 19 ADD 1000000 TO WX-DATUM.
           IF WZ-DATUM = 0 MOVE 0 TO WX-DATUM.
           COMPUTE WA-TAG = FUNCTION INTEGER-OF-DATE(WX-DATUM).
           COMPUTE WA-TAG = FUNCTION DATE-OF-INTEGER(WA-TAG).
           IF WA-TAG not = WX-DATUM MOVE 0 TO WZ-DATUM WX-DATUM GO Z.
           MOVE WZ-JAHR TO VDU-JAHR.
           MOVE WZ-TAG TO VDU-TAG.
           MOVE WZ-MONAT TO VDU-MONAT.
       Z.  EXIT.
      *************************************************** 05 ***********
       DATDREH SECTION.
       A.  MOVE WC-TAG   TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR  TO WZ-JAHR VDU-TAG.
           COMPUTE WX-DATUM = WC-DATUM + 19000000.
           IF WZ-TAG < 19 ADD 1000000 TO WX-DATUM.
           IF WZ-DATUM = 0 MOVE 0 TO WX-DATUM.
       Z.  EXIT.
      ***************************************** anlegen / Ñndern Texte *
       BEDIENEN SECTION.
       A.  OPEN I-O BEDIEN.
           MOVE "0701101566010" TO WH-CALL PERFORM FENSTER.
       B.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-ECK TO WB-MASK.
           PERFORM BEDI.
           IF AB MOVE "0702131566010" TO WH-CALL PERFORM FENSTER
               MOVE WL-CALL TO WH-CALL
               MOVE WL-GROSS TO WB-MASK
               PERFORM BEDI
               MOVE "08CLOFE" TO WH-CALL PERFORM FENSTER GO B.
           CLOSE BEDIEN.
           MOVE "08CLOFE" TO WH-CALL PERFORM FENSTER.
           MOVE WL-CALL TO WH-CALL.
       Z.  EXIT.
      ******************************************** Bedienungsanleitung *
       BEDI SECTION.
      *------------> Aufbau: WL-ECK = 1. Fenster, WL-GROSS = 2. Fenster,
      *---------------------> WL-KO = ab Zeile f. Anwender
       A.  ADD 103 VDU-ECK GIVING VDU-ZP.
           DISPLAY " Handbuch " with highlight AT VDU-ZP.
           ADD 55 TO VDU-ZP.
           DISPLAY " (" AT VDU-ZP WB-MASK ") ".
           ADD 207 VDU-ECK GIVING VDU-ZP.
           DISPLAY "<esc/#>= Ende, < >= auf, < >= blÑttern, <ret>= Einga
      -        "be" AT VDU-ZP.
           MOVE 1 TO WB.
      *----------------------------------------------------> anzeigen <-
       C.  ADD 1 TO WB.
       D.  COMPUTE WH-BEKEY = WB-MASK * 12 - 13 + WB.
           READ BEDIEN INVALID INITIALIZE BE-SATZ.
           IF ZUGRIF GO D.
           PERFORM BEDAUS.
           IF VDU-ZP NOT = 0; IF WB NOT = 13 GO C else GO F.
           GO G.
       E.  MOVE 13 TO WB.
       F.  IF WR-Z(WB - 1) = 0; IF WB > 1 SUBTRACT 1 FROM WB
              IF WB not = WL-KO GO F.
           MOVE 0 TO VDU-ZP.
           IF WB NOT = 13 GO C.
       G.  MOVE BE-TX TO WT-TX.
           COMPUTE VDU-LP = 103 + WB * 100.
           MOVE "1200000162" TO WH-CALL PERFORM PLUSTEXT.
           MOVE WL-CALL TO WH-CALL.
           IF ESC OR KIST GO Z.
           IF SF1 MOVE 1 TO WL-KO
               SET AUF TO TRUE.
           IF AUF SUBTRACT WL-KO FROM WB GIVING WI; IF WI > 1 GO H.
           IF AB GO H.
           IF NOT RET GO G.
       H.  MOVE WT-TX TO BE-TX.
       K.  PERFORM BEDAUS.
       L.  WRITE BE-SATZ INVALID REWRITE BE-SATZ.
           IF AUF SUBTRACT 2 FROM WH-BEKEY WB.
           IF AB; IF WL-GROSS = 0 GO G else GO Z.
           IF WB = 13 GO E else GO C.
       Z.  EXIT.
      ******************************************************************
       BEDAUS SECTION.
       K.  COMPUTE VDU-LP = 103 + WB * 100 + VDU-ECK.
           DISPLAY BE-TX with highlight AT VDU-LP.
           IF BE-TX = SPACE MOVE 0 TO WR-Z(WB - 1)
               else MOVE 1 TO WR-Z(WB - 1).
       Z.  EXIT.
      **************** Convertg. Gro·- /Kleinbuchstaben u. Umlautsortg.*
       ALPHACONV SECTION.
       A.  set ty to 1.
           set fy to wl-az.
           if wl-ca = 21 go E.
       C.  move wr-tx(ty) to wq.
           move wr-rv(wi + 1) to wr-tx(ty).
           if ty < fy set ty up by 1 go C.
           go z.
      *----------------------------------------------> Reconvertierung *
       E.  move wr-tx(ty) to wq.
           move wr-cv(wi + 1) to wr-tx(ty).
           if ty < fy set ty up by 1 go E.
       Z.  exit.
       END PROGRAM CAUP.
      *SET LINKCOUNT "192" ANS85 BOUND NOALTER NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CADECL.
      ******************************************************************
      *         DECLARATIVES als CALL fÅr alle Programmteile           *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WX-FEHL.
           03  WD-FA                   PIC X(8)     VALUE "Fehler: ".
           03  WD-SB                   PIC ZZ9.
           03  WD-B                    PIC X      VALUE "/".
           03  WD-SA                   PIC 9.
           03  WD-C                    PIC X.
           03  DATEI                   PIC X(12)    VALUE SPACE.
           03  WD-D                    PIC X(39)
                    VALUE " - Programmabbruch - weiter mit <ret>: ".
           03  WD-X                    PIC X.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC X.
           03  WL-DRUCK                PIC X(14).
       COPY WHCREG.CPY.
       PROCEDURE DIVISION using WH-CALL WH-CREG.
      *****************************************************************
       STEUER SECTION.
       A.  IF WL-CA = "9" GO Z.
           IF WL-CA = "1" PERFORM DECLDRU
           else PERFORM DECL-X.
       Z.  EXIT PROGRAM.
      ******************************************************************
       DECL-X SECTION.
       A.  IF ZUGRIF GO Z.
           MOVE WH-CALL TO DATEI.
           IF WF-STATUS = "35" GO Z.
           IF WF-STATUS = "41" SET ZUGRIF TO TRUE GO Z.       *>Windows
           IF WF-SB = 13 GO Z.
           IF WF-SA > 8  MOVE WF-SA TO WD-SA
               MOVE WF-SB TO WD-SB
               MOVE "/" TO WD-B
           else MOVE WF-STATUS TO WD-SB
                MOVE SPACE TO WD-B.
           DISPLAY WX-FEHL with highlight AT 2401.
           ACCEPT WD-X with auto-skip AT 2467.
           STOP RUN.
       Z.  EXIT.
      ******************************************************************
       DECLDRU SECTION.
       A.  IF WF-STATUS = "94" GO Z.
       C.  MOVE WF-SB TO WD-SB.
           DISPLAY "DRUCKER Fehler: " at 2401 WD-SB
                   " A = Abbruch, W = Wiederholen".
           ACCEPT WD-X with auto-skip AT 2451.
           CALL "CAUP" USING "1324012480" WH-CREG.
           IF WD-X = "A" OR WD-X = "a" STOP RUN.
           IF WD-X = "W" OR WD-X = "w" MOVE "27" TO WF-STATUS GO Z.
           GO C.
       Z.  EXIT.
       END PROGRAM CADECL.
      *SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CADRUCK.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DRUDAT     ASSIGN TO WN-DRUDAT
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY DT-KEY
                             FILE STATUS WF-STATUS.
           SELECT HILFPOS    ASSIGN TO WN-HIDAT
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ********************************************* interne Hilfsdatei *
       FD  DRUDAT                      LABEL RECORD STANDARD.
       01  DT-SATZ.
           03  DT-KEY.
               05 DT-DATE              PIC 9(8)    COMP.
               05 DT-TIME              PIC 9(6)    COMP.
               05 DT-MERK              PIC 9       COMP.
           03  DT-NAME                 PIC X(14).
           03  DT-SIZE                 PIC 9(8)    COMP.
      *************************************************** Ausgabedatei *
       FD  HILFPOS                     LABEL RECORD STANDARD.
       01  HI-SATZ.
           03  HI-NAME                 PIC X(12).
           03  FILLER                  PIC X(1).
           03  HI-SIZE                 PIC ZZ.ZZZ.ZZ9.
           03  FILLER                  PIC X(2).
           03  HI-DATE                 PIC X(10).
           03  FILLER                  PIC X(2).
           03  HI-TIME                 PIC XXBXXBXX.
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
       COPY $COBDIR\WHCREG.CPY.
       01  WH-SONS.
           03  WS-HILF                 PIC 99      COMP  VALUE ZERO.
           03  VDU-AB                  PIC 9999.
           03  WN-DRUDAT               PIC X(13)   VALUE "DRU-xx.DAT".
           03  WN-HIDAT                PIC X(13)   VALUE "HIL-xx.DAT".
           03  WN-CFILE                PIC X(13).
           03  WN-DFILE                PIC X(13).
      *------------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99      COMP-X.
           03  FUNKT                   PIC 99      COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99      COMP-X VALUE 0.
      *        05 PAR                  PIC X(40)   VALUE SPACE.
      *--------------------------------------------> check_file_exist <-
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
           03  WD-UPON                 PIC X(40).
           03  WK-F                    PIC 99      COMP OCCURS 10.
           03  WD-ZL                   PIC Z9.
           03  WD-SIZE                 PIC ZZ.ZZZ.ZZ9.
           03  WI                      PIC 99      COMP VALUE 0.
           03  WE                      PIC 99      COMP VALUE 0.
           03  IX                      PIC 99      COMP VALUE 0.
           03  WL                      PIC 99      COMP VALUE 0.
           03  WH-LR                   PIC XXXX.
           03  VDU-ZEIT.
               05 VDU-STD              PIC Z9.
               05 VDU-DD               PIC X.
               05 VDU-MIN              PIC 99.
           03  WX-PRNO                 PIC 99      COMP-X.
           03  WX-PRSTAT               PIC 99      COMP-X.
           03  WM-DRU                  PIC 99      COMP.
           03  WM-D                    PIC 9            VALUE 0.
           03  WM-MA                   PIC 99      COMP.
           03  WZ-JAR                  PIC 9999.
       01  WM-REG.
           03  WM-DRIVE                PIC X.
           03  FILLER                  PIC XX      VALUE ":\".
           03  WM-PATH                 PIC X(40).
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
               05 WF-ACTION            PIC X       COMP-X.
               05 WF-ATTR              PIC X       COMP-X VALUE 39.
               05 WF-EIN               PIC X(80).
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       DECLARATIVES.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON HILFPOS.
       A.  CALL "CADECL" USING "DRUHILF.DAT " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           CALL "PC_READ_DRIVE" USING WM-DRIVE.
           CALL "CBL_READ_DIR" USING WM-PATH WX-PRNO.
           IF WL-CA = 99 MOVE WM-REG TO WT-TX GO X.
           IF WK-BS(1:1) = SPACE MOVE "0" TO WK-BS(1:1).
           IF WK-BS(1:2) = SPACE MOVE "0" TO WK-BS(1:2).
      *---------------------------------------------> Userkennzeichen <-
           IF WK-BS(1:1) not = SPACE MOVE WK-BS(1:2) TO WN-DRUDAT(5:2)
                                     MOVE WK-BS(1:2) TO WN-HIDAT(5:2).
           EVALUATE WL-CA
               WHEN 10 MOVE WT-TX TO WF-EIN
                       PERFORM DIRSUCH
                       MOVE WN-HIDAT TO WT-TX GO X
               WHEN 11 MOVE "*.*" TO WF-EIN
                       PERFORM DIRSUCH
                       MOVE WN-HIDAT TO WT-TX GO X
               WHEN 40 CALL "CAUP" USING "0705121553021" WH-CREG
                       PERFORM DRUCKEN
                       CALL "CAUP" USING "08CLOFEN" WH-CREG
               WHEN OTHER PERFORM DRUCAP.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       COMLINE SECTION.
       A.  DISPLAY WD-UPON UPON COMMAND-LINE.
           DISPLAY LOW-VALUE AT 2301.
           CALL X"91" USING RESULT FUNKT PARAM.
           CALL "CAUP" USING "1323012480" WH-CREG.
       Z.  EXIT.
      ****************************************** Druck von LST-Dateien *
       DRUCKEN SECTION.
       A.  IF WL-CA = 40 MOVE "*.LST " TO WH-LR WF-EIN
                    else MOVE "*.RES " TO WH-LR WF-EIN.
           PERFORM DIRSUCH.
      *------------------------------------------> ob-Datei vorhanden <-
           MOVE 0 TO WF-SIZE.
           CALL "CBL_CHECK_FILE_EXIST" USING WN-HIDAT WH-FDET.
           IF WF-SIZE = 0 OPEN OUTPUT HILFPOS
               CLOSE HILFPOS.
           OPEN INPUT HILFPOS.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE SPACE TO WR-DNAM(WX).
           MOVE 0 TO WX.
       E.  PERFORM DRUCKDAT.
           CLOSE HILFPOS.
           DELETE FILE HILFPOS.
           IF WL-CA NOT = 00 GO A.
       Z.  EXIT.
      ******************************************************************
       DRUCKDAT SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 11
               COMPUTE VDU-LP = WX * 100 + 302 + VDU-ECK
               CALL "CAUP" USING "1300000030" WH-CREG.
           ADD 103  VDU-ECK GIVING VDU-LP.
           DISPLAY " vorhandene Druckdateien " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Nr. Bezeichnung       Grî·e    Erstellungsdaten  "
               AT VDU-LP
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL "ƒ" with SIZE 51 AT VDU-LP.
           IF WI = 0 MOVE 00 TO WL-CA
               PERFORM DRUCAP
               MOVE 1 TO WI
               MOVE 40 TO WL-CA.
           CALL "CAUP" USING "1320012480" WH-CREG.
           MOVE 0 TO IX WE.
           PERFORM OB-FILE.
       B.  ADD 1503 VDU-ECK GIVING VDU-LP.
           EVALUATE WM-D
               WHEN 0 DISPLAY " lokaler Drucker " AT VDU-LP
               WHEN OTHER DISPLAY "  . Netzdrucker  " AT VDU-LP
                          ADD 1 TO VDU-LP
                          DISPLAY WM-D AT VDU-LP.
       C.  ADD 1403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Welche Datei wollen Sie drucken? < >" AT VDU-LP.
           ADD 1026 VDU-ECK GIVING VDU-LP.
           IF WL-CA = 40 DISPLAY "<ret-leer/esc>= Abbruch, <>= Wiederho
      -        "lungsdruck, < >= Druckerwahl" AT 2301
                    else DISPLAY "<ret-leer/esc>= Abbruch, <>= Erstdruc
      -        "k, < >= Druckerwahl" AT 2301.
           DISPLAY "< >= weiterlisten" AT 2401.
           IF WL-CA = 40 DISPLAY ", <Entf>= lîschen" AT 0000.
           CALL "CAUP" USING "1014371001" WH-CREG.
           IF WOLI; IF WL-CA = 40 MOVE 41 TO WL-CA GO Z
                           else MOVE 40 TO WL-CA GO Z.
           IF WORE MOVE WL-CA TO WL
                   MOVE 00 TO WL-CA
                   PERFORM DRUCAP
                   MOVE WL TO WL-CA
                   GO B.
           IF AB or AUF ADD 1403 VDU-ECK GIVING VDU-LP
               CALL "CAUP" USING "1300000050" WH-CREG
               PERFORM OB-FILE GO B.
           IF WH-NUM = 0 GO Y.                *> testweise
           IF ESC GO Y.
           IF NOT RET AND NOT ENTF GO C.
           IF WH-NUM > IX GO C.
           IF WH-NUM = 0 GO Y.
           MOVE WH-NUM TO WX.
           IF ENTF; IF WL-CA = 40 GO E ELSE GO C.
           IF WM-DRU = 0 MOVE WL-CA TO WL
                         MOVE 01 TO WL-CA
                         PERFORM DRUCAP
                         MOVE WL TO WL-CA
                         IF WL-CA = 01 GO B.
           MOVE WR-DNAM(WX) TO WN-CFILE.
      *-----------------------------------> ausgewÑhlte Datei sichern <-
       E.  IF WL-CA = 40 MOVE WR-DNAM(WX) TO WN-CFILE WN-DFILE
               PERFORM VARYING WY FROM 9 BY -1 UNTIL WY = 0 OR
                   WN-CFILE(WY:4) = ".LST" CONTINUE
               end-perform
               IF WY > 0 MOVE ".RES" TO WN-CFILE(WY:4)
               end-if
               CALL "CBL_COPY_FILE" USING WN-DFILE WN-CFILE
               CALL "CBL_DELETE_FILE" USING WN-DFILE
               MOVE WN-CFILE TO WR-DNAM(WX)
               IF ENTF GO Z.
           IF WM-MA = 0 AND WM-DRU = 0 MOVE 90 TO WL-CA
               PERFORM DRUCAP.                         *> endcap setzen
           IF WM-MA = 1 OR WM-DRU = 0           *> kein Netz oder lokal
               CALL "CBL_COPY_FILE" USING WN-CFILE "LPT1 "
               GO Y.
       G.  MOVE "NPRINT              Q=PRINTQ_0 " TO WD-UPON.
           MOVE WR-DNAM(WX) TO WD-UPON(8:12).
      *    IF WM-DRU < 4 SUBTRACT 1 FROM WM-DRU GIVING WM-D
      *        else MOVE WM-DRU TO WM-D.
      *    SUBTRACT 1 FROM WM-DRU GIVING WM-D.
           MOVE WM-D TO WD-UPON(30:1).
           PERFORM COMLINE.
           IF WM-MA = 0 MOVE 90 TO WL-CA
               PERFORM DRUCAP.
       Y.  MOVE 0 TO WL-CA.
       Z.  EXIT.
      ******************************************************************
       OB-FILE SECTION.
       A.  MOVE 1 TO WY.
           IF WE = 0 CALL "CAUP" USING "16CLREN" WH-CREG
                     MOVE 0 TO IX.
       B.  IF WY = 10 GO Z.
       C.  READ HILFPOS NEXT AT END CLOSE HILFPOS
               MOVE 0 TO WE
               OPEN INPUT HILFPOS GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF HI-SATZ(1:1) = SPACE GO C.
           MOVE HI-NAME TO WN-DFILE.
      *------------------------------------------> .LST /.RES anfÅgen <-
           MOVE 0 TO WF-SIZE WK-F(WY).
           CALL "CBL_CHECK_FILE_EXIST" USING WN-DFILE WH-FDET.
           IF WF-SIZE NOT = 0 ADD 1 TO IX
               IF IX > 9 COMPUTE VDU-LP = 1203 + VDU-ECK
                         CALL "CAUP" USING "17SCROL" WH-CREG
                         PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 8
                             MOVE WR-DNAM(IX + 1) TO WR-DNAM(IX)
                             MOVE SPACE TO WR-DNAM(IX + 1)
                         end-perform
                else COMPUTE VDU-LP = IX * 100 + VDU-ECK + 303
                end-if
                MOVE WN-DFILE TO WR-DNAM(IX)
                MOVE IX TO WD-ZL
                DISPLAY WD-ZL AT VDU-LP
                ADD 4 TO VDU-P
                DISPLAY HI-SATZ with highlight AT VDU-LP
                ADD 1 TO WY WE.
           GO B.
       Z. EXIT.
      ******************************************************************
       DRUCAP SECTION.
       A.  DISPLAY LOW-VALUE AT 2310.
           MOVE "CAPTURE Q=PRINTQ_0 NB TI=1 NFF" TO WD-UPON.
           EVALUATE WL-CA
               WHEN 00
               WHEN 01 IF WM-DRIVE < "F" PERFORM DRU-PC
                                    else PERFORM DRU-NET
                       end-if GO Z
               WHEN 90 MOVE "ENDCAP" TO WD-UPON
               WHEN OTHER MOVE WL-CA(2:1) TO WD-UPON(18:1).
      *    DISPLAY WD-UPON UPON COMMAND-LINE.
      *    CALL X"91" USING RESULT FUNKT PARAM.
       Z.  EXIT.
      ************************************************ Netzwerkdrucker *
       DRU-NET SECTION.
       A.  MOVE 0 TO WM-MA.
           CALL "CAUP" USING "0710100760000" WH-CREG.
           MOVE 9 TO WH-NUM.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druckerwahl " with reverse-video AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "  0 +<ret>= lokaler Drucker" AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY " 1-9+<ret>= 1-9. Netzdrucker     < >" AT VDU-LP.
           ADD 331 VDU-ECK GIVING VDU-AB.
           PERFORM DRU-OK.
       Z.  EXIT.
      *********************************************** Einzelplatz - PC *
       DRU-PC SECTION.
       A.  MOVE 1 TO WM-MA.
           MOVE 9 TO WH-NUM.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           EVALUATE WX-PRSTAT
               WHEN  48
               WHEN 176
               WHEN 136
               WHEN   8
               WHEN 200
               WHEN 192
               WHEN  24
               WHEN  56 NEXT SENTENCE
               WHEN OTHER GO Z.
      *---------------------------------------> nur bei Druckerfehler <-
           CALL "CAUP" USING "0710100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Drucker " with reverse-video AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret-leer>= lokaler Drucker < >" AT VDU-LP.
           ADD 335 VDU-ECK GIVING VDU-AB.
           PERFORM DRU-OK.
           MOVE 2 TO WH-WERT.
       Z.  EXIT.
      ************************************************** ob Drucker ok *
       DRU-OK SECTION.
       C.  MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           MOVE VDU-AB TO VDU-LP.
           CALL "CAUP" USING "1300000065" WH-CREG.
           EVALUATE WX-PRSTAT
               WHEN  48 DISPLAY " nicht vorhanden " with reverse-video
                           AT VDU-LP
               WHEN 176 WHEN 136 WHEN   8 WHEN 200
               WHEN 192 DISPLAY " nicht eingeschaltet " with
                           reverse-video AT VDU-LP
               WHEN  24 DISPLAY " nicht ON LINE " with reverse-video
                           AT VDU-LP
               WHEN  56 DISPLAY " kein Papier " with reverse-video
                           AT VDU-LP
               WHEN OTHER; IF WL-CA = 01 MOVE 00 TO WL-CA GO X
                           else IF WH-NUM NOT = 9 GO X.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Druckerwahl" AT 2301.
           IF WM-MA = 0 CALL "CAUP" USING "1005371001" WH-CREG  *> Netz
                   else CALL "CAUP" USING "1003320000" WH-CREG. *> PC
           IF ESC; IF WM-MA = 1 MOVE 0 TO WM-DRU
                           else MOVE 1 TO WM-DRU
                   end-if MOVE WL-CA TO WL
                          GO X.
           IF NOT RET GO E.
           MOVE WH-NUM TO WM-DRU.
           IF WM-MA = 1 MOVE 0 TO WM-DRU GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE WM-DRU TO WM-D.
       Z.  EXIT.
      ******************************************* Verzeichnis auslesen *
       DIRSUCH SECTION.
       A.  MOVE 0 TO WF-HANDL WF-ACTION DT-MERK.
           DELETE FILE DRUDAT HILFPOS.
           OPEN I-O DRUDAT.
           OPEN OUTPUT HILFPOS.
       C.  MOVE SPACES TO WF-OUT.
           CALL X"91" USING WF-RESU WF-CODE WF-PARA.
           IF WF-ERR = 0 PERFORM RAUS
                         MOVE 1 TO WF-ACTION GO C.
           IF WF-ERR = 2 CLOSE HILFPOS DRUDAT
                         DELETE FILE HILFPOS
                         DELETE FILE DRUDAT
                         MOVE SPACE TO WT-TX
                         GO Z.
           MOVE 99999999 TO DT-DATE DT-TIME DT-MERK.
           START DRUDAT KEY < DT-KEY INVALID GO X.
       E.  READ DRUDAT PREVIOUS AT END GO X.
           MOVE DT-DATE TO WC-DATUM.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           MOVE VDU-DATUM TO HI-DATE.
           MOVE DT-TIME TO HI-TIME.
           MOVE ":" TO HI-TIME(3:1) HI-TIME(6:1).
           MOVE DT-NAME TO HI-NAME.
           MOVE DT-SIZE TO HI-SIZE.
           WRITE HI-SATZ.
           GO E.
       X.  CLOSE DRUDAT HILFPOS.
           DELETE FILE DRUDAT.
       Z.  EXIT.
      ******************************************************************
       RAUS SECTION.
       A.  COMPUTE WZ-JAR = WF-DATE / 512 + 1980.
           COMPUTE WZ-MONAT = (WF-DATE - ((WZ-JAR - 1980) * 512)) / 32.
           COMPUTE WZ-TAG =
                   WF-DATE - (WZ-MONAT * 32 +  (WZ-JAR - 1980) * 512).
           COMPUTE WC-TAG = WF-TIME / 2048.
           COMPUTE WC-MONAT = (WF-TIME - (WC-TAG * 2048)) / 32.
           COMPUTE WC-JAHR =
                      (WF-TIME - (WC-TAG * 2048 + WC-MONAT * 32)) * 2.
           MOVE WF-OUT TO DT-NAME.
           MOVE WF-SIZES TO DT-SIZE.
           COMPUTE DT-DATE = WZ-TAG + WZ-MONAT * 100 + WZ-JAR * 10000.
           MOVE WC-DATUM TO DT-TIME.
           ADD 1 TO DT-MERK.
           WRITE DT-SATZ.
       Z.  EXIT.
           END PROGRAM CADRUCK.
      *SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CALOG.
      ******************************************* allgemeine Log-DAtei *
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                         ENVIRONMENT-NAME IS ENV-SET
                         ENVIRONMENT-VALUE IS ENV-WERT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EINAUS     ASSIGN TO "EINAUS.LOG"
                             ORGANIZATION IS INDEXED, ACCESS DYNAMIC
                             RECORD KEY EA-KEY.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  EINAUS                     LABEL RECORD STANDARD.
       01  EA-SATZ.
           03  EA-KEY.
               05 EA-BFNR              PIC 99.
               05 EA-STR               PIC X.
               05 EA-LFD               PIC 99.
           03  FILLER                  PIC X.
           03  EA-WOTAG                PIC XXX.
           03  FILLER                  PIC X(2).
           03  EA-DATUM1               PIC XX.
           03  EA-S1                   PIC X.
           03  EA-DATUM2               PIC XX.
           03  EA-S2                   PIC X.
           03  EA-DATUM3               PIC XX.
           03  FILLER                  PIC XXX.
           03  EA-ZEITST               PIC XX.
           03  EA-S11                  PIC X.
           03  EA-ZEITMI               PIC XX.
           03  EA-S12                  PIC X.
           03  EA-ZEITSEC              PIC XX.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WS-REG.
           03  WO-TG                   PIC 9.
           03  WO-ZT                   PIC 9(8).
           03  WO-DAT                  PIC 9(6).
           03  WN-LOGN.
               05 FILLER               PIC X(8)  VALUE "EINAUS.0".
               05 WC-EXTV.
                  07 WC-VAL            PIC 99.
      *------------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99      COMP-X.
           03  FUNKT                   PIC 99      COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99      COMP-X VALUE 0.
               05 PAR                  PIC X(40)   VALUE SPACE.
           03  WI                      PIC 99      COMP.
           03  IR                      PIC 99      COMP.
           03  WP                      PIC 9       COMP.
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 50 PERFORM SWI-SET GO Z.
           DISPLAY "CALOG" UPON ENV-SET.
           ACCEPT WC-EXTV FROM ENV-WERT.
           IF WC-EXTV(1:1) = SPACE or WC-EXTV(2:1) = SPACE
               MOVE WK-BS TO WC-VAL.
           OPEN I-O EINAUS.
           EVALUATE WL-CA
               WHEN 10 GO Q
               WHEN 20 PERFORM ANZ GO W.
           ACCEPT WO-DAT FROM DATE.
           ACCEPT WO-ZT FROM TIME.
           ACCEPT WO-TG FROM DAY-OF-WEEK.
           MOVE ALL SPACES TO EA-SATZ.
           EVALUATE WO-TG
               WHEN 1 MOVE "Mo," TO EA-WOTAG
               WHEN 2 MOVE "Di," TO EA-WOTAG
               WHEN 3 MOVE "Mi," TO EA-WOTAG
               WHEN 4 MOVE "Do," TO EA-WOTAG
               WHEN 5 MOVE "Fr," TO EA-WOTAG
               WHEN 6 MOVE "Sa," TO EA-WOTAG
               WHEN 7 MOVE "So," TO EA-WOTAG.
           MOVE WO-DAT(1:2) TO EA-DATUM1.
           MOVE WO-DAT(3:2) TO EA-DATUM2.
           MOVE WO-DAT(5:2) TO EA-DATUM3.
           MOVE "-" TO EA-S1 EA-S2.
           MOVE WO-ZT(1:2) TO EA-ZEITST.
           MOVE WO-ZT(3:2) TO EA-ZEITMI.
           MOVE WO-ZT(5:2) TO EA-ZEITSEC.
           MOVE ":" TO EA-S11 EA-S12.
           MOVE "/" TO EA-STR.
           MOVE WC-VAL TO EA-BFNR.
           MOVE 0 TO EA-LFD.
       G.  WRITE EA-SATZ not INVALID GO W.
           IF EA-LFD not = 99 ADD 1 TO EA-LFD GO G.
           GO W.
      *----------------------------------------------------> Ausstieg <-
       Q.  MOVE "/" TO EA-STR.
           MOVE WC-VAL TO EA-BFNR.
           MOVE 99 TO EA-LFD.
           START EINAUS KEY < EA-KEY INVALID GO W.
       R.  READ EINAUS PREVIOUS AT END GO W.
           IF ZUGRIF GO R.
           IF WC-VAL not = EA-BFNR GO R.
           DELETE EINAUS.
       W.  CLOSE EINAUS.
       Z.  EXIT PROGRAM.
      ********************************************* Log-Datei anzeigen *
       ANZ SECTION.
       A.  CALL "CAUP" USING "0705081638000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Log-Datei-öbersicht " with highlight AT VDU-LP.
           MOVE 0 TO EA-BFNR EA-LFD.
           START EINAUS KEY > EA-KEY INVALID GO W.
       B.  MOVE 1 TO WX.
       C.  IF WX > 14 GO W.
           READ EINAUS NEXT AT END MOVE 99 TO WX GO W.
           IF ZUGRIF GO C.
           COMPUTE VDU-LP = WX * 100 + 104 + VDU-ECK.
           IF WX = 1 CALL "CAUP" USING "16CLRFEN" WH-CREG.
           DISPLAY EA-SATZ AT VDU-LP.
           ADD 1 TO WX.
           GO C.
       W.  DISPLAY "<esc>= Abbruch, <ret>= weiter, <Entf>= lîschen <
      -       ">" AT 2301.
           CALL "CAUP" USING "0023494004" WH-CREG.
           IF ESC GO X.
           IF ENTF AND WH-NUM = 4711 CLOSE EINAUS
               OPEN OUTPUT EINAUS
               GO X.
           IF NOT RET GO W.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** Run-Time Switch setzen *
       SWI-SET SECTION.
       A.  MOVE 14 TO FUNKT.
           CALL X"91" USING RESULT FUNKT PARAM.
           MOVE WL-REST(5:1) TO RESULT(1:).
           SUBTRACT 64 FROM RESULT GIVING WI.
           MOVE WL-REST(6:1) TO RESULT(1:).
           SUBTRACT 47 FROM RESULT GIVING WP.
           MOVE PARAM(WI:1) TO IR(1:1).
           IF WL-REST(4:1) = "+" and IR = 0 or IR = 2
               ADD WP TO IR GIVING WP.
           IF WL-REST(4:1) = "-" and IR = 1 or IR = 3
               SUBTRACT WP FROM IR GIVING WP.
           MOVE WP(1:1) TO PARAM(WI:1).
           MOVE 13 TO FUNKT.
           CALL X"91" USING RESULT FUNKT PARAM.
       Z.  EXIT.
           END PROGRAM CALOG.
      *SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CABARCOD.
      ***************************************** Barcodeerstellung TITZ *
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                         ENVIRONMENT-NAME IS ENV-SET
                         ENVIRONMENT-VALUE IS ENV-WERT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT EINAUS     ASSIGN TO "EINAUS.LOG"
      *                      ORGANIZATION IS INDEXED, ACCESS DYNAMIC
      *                      RECORD KEY EA-KEY.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *FD  EINAUS                     LABEL RECORD STANDARD.
      *01  EA-SATZ.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WS-REG.
           03  WI                      PIC 9999      COMP.
           03  IY                      PIC 99        COMP.
           03  IZ                      PIC 99        COMP.
           03  X4                      PIC 999       COMP.
           03  WS                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WC-EAN                  PIC 99        COMP.
           03  WD-BAR                  PIC 99/99BB999.999.
      *----------------------------------> Etiketten-Druckersteuerung <-
           03  WH-EAN                  PIC 9(10).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 10.
           03  WL-MUSTER.                               *> Laserdrucker
               05 WL-STRI              PIC XX   OCCURS  90.
           03  WS-BAR                  PIC X(34).
      *----------> WL-MUSTER = digit * 14 (5 schmale + 2 breite) + 20 <-
      *--------------> WS-BAR = 13 + (WL-MUSTER / 16 * 2) = 33 <--------
       01  WS-BCREG.
      *---------------------------------------------> 2/5 interleaved <-
           03  WT-BL
             VALUE "00110100010100111000001011010001100000111001001010".
               05 WI-BL                          OCCURS 10.
                  07 WX-BL             PIC X     OCCURS 5.
           03  WI-GRP                  PIC X(10).
           03  WH-ANFANG                         VALUE "01010".
               05 WH-ANF               PIC 9     OCCURS 5.
           03  WH-ENDE                           VALUE "11010".
               05 WH-END               PIC 9     OCCURS 5.
           03  WM-EAN                  PIC 9(10).
           03  WT-EAN REDEFINES WM-EAN.
               05 WT-E                 PIC 9      OCCURS 10 INDEXED IX.
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 10 PERFORM L-BAR-10.
           MOVE WS-BAR TO WT-TX.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      **************************************** Barcodemuster ermitteln *
       L-BAR-10 SECTION.
       A.  MOVE WM-DATUM TO WZ-DATUM.
           IF WZ-DATUM(3:6) > "0400" ADD 1 TO WZ-JAHR.
           COMPUTE WH-EAN = (0200 + WZ-JAHR) * 1000000 + WH-NUM.
           SET X4 IX TO 1.
           MOVE 0 TO X4.
           MOVE SPACE TO WL-MUSTER.
      *----------------------------------------------> Anfangszeichen <-
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 5
               ADD 1 TO X4
               IF WH-ANF(WI) = 1 MOVE "11" TO WL-STRI(X4)
                            else MOVE "00" TO WL-STRI(X4).
       E.  COMPUTE WS = WR-EAN(IX) + 1.              *> Tab. beg. mit 0
           MOVE 1 TO IY.
           PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 5
               EVALUATE WX-BL(WS IZ)
                   WHEN 0 MOVE "1" TO WI-GRP(IY:1)   *> schmaler Strich
                   WHEN 1 MOVE "2" TO WI-GRP(IY:1)   *> breiter Strich
               end-evaluate
               ADD 2 TO IY.
           SET IX UP BY 1.
           COMPUTE WS = WR-EAN(IX) + 1.             *> Tab. beg. mit 0
           MOVE 2 TO IY.
           PERFORM VARYING IZ FROM 1 BY 1 UNTIL IZ > 5
               EVALUATE WX-BL(WS IZ)
                   WHEN 0 MOVE "3" TO WI-GRP(IY:1)     *> schmale LÅcke
                   WHEN 1 MOVE "4" TO WI-GRP(IY:1)     *> breite LÅcke
               end-evaluate
               ADD 2 TO IY.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 10
               ADD 1 TO X4
               EVALUATE WI-GRP(WI:1)
                   WHEN 1 MOVE "11" TO WL-STRI(X4)
                   WHEN 2 MOVE "11" TO WL-STRI(X4)
                          SET X4 UP BY 1
                          MOVE "11" TO WL-STRI(X4)
                   WHEN 3 MOVE "00" TO WL-STRI(X4)
                   WHEN 4 MOVE "0000" TO WL-STRI(X4)
                          SET X4 UP BY 1
                          MOVE "00" TO WL-STRI(X4).
           IF IX < 10 SET IX UP BY 1
               GO E.
       K.  PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 5
               ADD 1 TO X4
               IF WH-END(WI) = 1 MOVE "11" TO WL-STRI(X4)
                            else MOVE "00" TO WL-STRI(X4).
           MOVE 13 TO WI.
           MOVE "*b000Y*b21W" TO WS-BAR.    *> Anz. Zeichen ohne Code
      *--------------------------------------------------> Umwandlung <-
           PERFORM VARYING WX FROM 1 BY 8 UNTIL WX > 168
               MOVE 0 TO WC-EAN
               ADD 1 TO WI
               MOVE WL-MUSTER(WX:8) TO WV-MCODE(1:)
               IF WV-MC(1) = "1"   ADD 128 TO WC-EAN
                   end-if
               IF WV-MC(2) = "1" ADD  64 TO WC-EAN
                   end-if
               IF WV-MC(3) = "1" ADD  32 TO WC-EAN
                   end-if
               IF WV-MC(4) = "1" ADD  16 TO WC-EAN
                   end-if
               IF WV-MC(5) = "1" ADD   8 TO WC-EAN
                   end-if
               IF WV-MC(6) = "1" ADD   4 TO WC-EAN
                   end-if
               IF WV-MC(7) = "1" ADD   2 TO WC-EAN
                   end-if
               IF WV-MC(8) = "1" ADD   1 TO WC-EAN
                   end-if
               MOVE WC-EAN(1:) TO WS-BAR(WI:1).
       Z.  EXIT.
           END PROGRAM CABARCOD.
