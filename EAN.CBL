      $SET LINKCOUNT "448" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EANDRU.
      ***************************** Anlage/Žnderung/L”schen Stammdaten *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    PC.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORDS OMITTED.
       01  DRA-SATZ                    PIC X(480).
       01  DRX-SATZ                    PIC X(480).
       01  DRY-SATZ.
           03  DRY-EAN                 PIC X(31)      OCCURS 62.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WS-REG.
           03  WZ-BREIT                PIC 999       COMP.
           03  WZ-GROSS                PIC 9(4).
           03  WH-DRUNAM               PIC X(20).
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WX-DR                   PIC 99        COMP.
           03  WM-OPEN                 PIC 9         COMP  VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP  VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP  VALUE ZERO.
           03  WH-ZEI                  PIC S999V9    COMP.
           03  WE-STG                  PIC X(40)     OCCURS 10.
           03  WM-START                PIC 9(6)      COMP.
           03  WL                      PIC 99        COMP.
           03  WI                      PIC 9999      COMP.
           03  WC-EAN                  PIC 99        COMP.
           03  WH-2OF5                 PIC 9(8)   VALUE ZERO.
           03  WD-EAN                  PIC 999BB9999BB9.
           03  WD-BAR                  PIC 999B9999.
      *----------------------------------> Etiketten-Druckersteuerung <-
       01  WD-ETK.
           03  WH-EAN                  PIC 9(13).
           03  WR-EAN REDEFINES WH-EAN PIC 9    OCCURS 13 INDEXED EX.
           03  WH-MEH                  PIC 99         COMP.
           03  WH-ESC.
               05  FILLER              PIC X               VALUE "".
               05  WH-ST1              PIC 9999   COMP-X   VALUE  0.
               05  WR-ST REDEFINES WH-ST1.
                   07 WH-ST2           PIC X.
                   07 FILLER           PIC X.
           03  WB-BC.
               05  WC-LSL              PIC XXX       OCCURS 3.
               05  WC-CR               PIC X.
               05  WC-BIT11            PIC XXX.
               05  WC-LNG              PIC 9999    COMP-5.
               05  WC-STR.
                   07 WC-X             PIC X    OCCURS 450 INDEXED X2.
           03  WN-MUSTER.                               *> Nadeldrucker
               05 WN-STRI              PIC X(6) OCCURS  69 INDEXED X1.
           03  WL-MUSTER.                               *> Laserdrucker
               05 WL-STRI              PIC XX   OCCURS  72 INDEXED X4.
           03  WS-BAR                  PIC X(31).
      *-----------------------------------------> Etikettenparameter <-
           03  ET-HOE                  PIC 9       COMP.
           03  ET-LIN                  PIC 999     COMP.
       01  WS-BCREG.
      *-----------------------------------------------> Barcode Werte <-
           03  WH-BL           VALUE "0110010010011011110101000110110001
      -         "0101111011101101101110001011000110101100110110110010000
      -         "101011100100111010100001000100100100011101001110010".
               05  WH-LIWERT           OCCURS 10.
                   07  WH-LI           PIC 9 OCCURS 7 INDEXED X3.
               05  WH-REWERT           OCCURS 10.
                   07  WH-RE           PIC 9 OCCURS 7.
           03  WH-ANFANG                   VALUE "001010".
               05 WH-ANF               PIC 9 OCCURS 6.
           03  WH-ENDE                     VALUE "0101".
               05 WH-END               PIC 9 OCCURS 4.
           03  WH-MITTE                    VALUE "01010".
               05 WH-MIT               PIC 9 OCCURS 5.
           03  WX-FF                   PIC X(6)  VALUE X"FFFFFFFFFFFF".
           03  WS-LR                   PIC 99    VALUE 0 COMP-3.
           03  WM-EAN                  PIC 9(8)  VALUE 0.
           03  WT-EAN REDEFINES WM-EAN.
               05  WT-7                PIC 9      OCCURS 8 INDEXED IX.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
      ******************************************************************
       DECLARATIVES.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACE UPON CRT.
           CALL "CAUP" USING "99010124800000" WH-CREG.
       F.  PERFORM ETIKDRU.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       DATDREH SECTION.
       A.  CALL "CAUP" USING "04DATDREH" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO A.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ***************************************** Drucker Seitenvorschub *
       PAGE-DRU SECTION.
           WRITE DRA-SATZ BEFORE PAGE.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WZ-ZEILEN.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE "E" TO DRA-SATZ(1:)
           WRITE DRA-SATZ AFTER 0.
           PERFORM PAGE-DRU.
       C.  MOVE 46 TO WX-PRNO.
           CALL X"91" USING WX-PRSTAT WX-PRNO DRUCKER.
           MOVE 0 TO WM-OPEN WZ-ZEILEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 0 TO WZ-ZEILEN.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
           MOVE 47 TO WX-PRNO.
           CALL X"91" USING WX-PRSTAT WX-PRNO DRUCKER.
       C.  IF WZ-ZEILEN > 0 PERFORM PAGE-DRU.
           MOVE 0 TO WZ-SCHALT.
           MOVE WE-STG(WX-DR) TO DRA-SATZ.
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       DAT-DREH SECTION.                                                       DAT-DREH SECTION 60.
       A.  MOVE WC-TAG TO WZ-JAHR VDU-JAHR.
           MOVE WC-JAHR  TO WZ-TAG VDU-TAG.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
       Z.  EXIT.
      *********************************************** Barcodeetiketten *
       ETIKDRU SECTION.
       A.  CALL "CAUP" USING "071010055000011" WH-CREG.
           MOVE "EAN2.LST" TO WH-DRUNAM.
           MOVE "e&l1s26a7d0O(s0p12h0b20v0s4102T&a6L" TO WE-STG(9).
           MOVE 9 TO WX-DR.
           PERFORM BEG-DRU.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Start-Nr.: " with highlight AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Start-Nr." AT 2301.
           CALL "CAUP" USING "1003146006" WH-CREG.
           IF ESC GO X.
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO WM-START.
      *--------------------------------------------> Barcode ausgeben <-
           MOVE 0 TO WL.
           MOVE WM-START TO WH-EAN.
           PERFORM L-BAR.
           PERFORM L-EANDRU.
           GO C.
       X.  WRITE DRA-SATZ AFTER 0.
           PERFORM END-DRU.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       L-BAR SECTION.
       A.  MULTIPLY 10 BY WH-EAN GIVING WM-EAN.
           ADD WT-7(1) WT-7(3) WT-7(5) WT-7(7) GIVING WI.
           MULTIPLY 3 BY WI.
           ADD WT-7(2) WT-7(4) WT-7(6) TO WI.
           ADD 1 TO WI.                                      *> MOD10GP
           DIVIDE 10 INTO WI GIVING WI REMAINDER WI.
           SUBTRACT WI FROM 10 GIVING WT-7(8).
           MOVE WM-EAN TO WH-EAN.
           SET X4 X3 X1 IX TO 1.
           MOVE ZEROS TO WL-MUSTER.
           MOVE 1 TO WI.
       B.  IF WH-ANF(WI) = 1 MOVE "11" TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF WI < 6 ADD 1 TO WI GO B.
       E.  IF WT-7(IX) = 0 MOVE 10 TO WS-LR
                else MOVE WT-7(IX) TO WS-LR.
       F.  IF IX > 4; IF WH-RE(WS-LR X3) = 1 MOVE "11" TO WL-STRI(X4)
                      else MOVE ZEROS TO WL-STRI(X4)
                 else IF WH-LI(WS-LR X3) = 1 MOVE "11" TO WL-STRI(X4)
                      else MOVE ZEROS TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF X3 < 7 SET X3 UP BY 1 GO F.
           SET X3 TO 1.
           IF IX NOT = 4 GO I.
           MOVE 2 TO WI.
       H.  IF WH-MIT(WI) = 1 MOVE "11" TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF WI < 5 ADD 1 TO WI GO H.
       I.  SET IX UP BY 1.
           IF IX NOT > 8 GO E.
           MOVE 2 TO WI.
       K.  IF WH-END(WI) = 1 MOVE "11" TO WL-STRI(X4).
           SET X4 UP BY 1.
           IF WI < 4 ADD 1 TO WI GO K.
           MOVE 13 TO WI.
           MOVE "*b000Y*b18W" TO WS-BAR.
      *--------------------------------------------------> Umwandlung <-
           PERFORM VARYING WX FROM 1 BY 8 UNTIL WX > 144
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
      ******************************************************************
       L-EANDRU SECTION.
       A.  MOVE WZ-ZEILEN TO WH-ZEI.
           MOVE "*t150R" TO DRA-SATZ(1:).            *> 150 Dots/inch
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE "*r1A" TO DRA-SATZ(1:).              *> current Cursor
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 52
               MOVE WS-BAR TO DRY-EAN(WX).
           MOVE 0 TO WZ-SCHALT.
           MOVE "*rC" TO DRY-EAN(WX).                *> end graphics
           MOVE ALL " " TO DRA-SATZ(2:80).
           PERFORM DRUCK.
           MOVE SPACE TO DRY-SATZ.
           ADD 2,0 WH-ZEI GIVING WZ-ZEILEN.
           COMPUTE WD-BAR = WH-EAN / 10.
           MOVE WH-EAN TO WD-BAR.
           MOVE WD-BAR TO DRX-SATZ(2:).
           PERFORM DRUCK.
           PERFORM DRUCK.
       Z.  EXIT.
      ***********************************************************************
       ZAHLEN SECTION.
       A.  MOVE LOW-VALUE TO WC-STR.
           MOVE 3 TO WH-ST1.
           MOVE "l" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(3).
      *    MOVE x"1B6C03" TO WC-LSL(3).
           SET X2 TO 1.
           SET EX TO 6.
       C.  MOVE WR-EAN(EX) TO WC-X(X2).
           IF X2 < 8 SET EX X2 UP BY 1 GO C.
           MOVE 0 TO WC-LNG.
           WRITE DRX-SATZ FROM WB-BC AFTER 0.
           MOVE 0 TO WH-ST1.
           MOVE "l" TO WH-ST2.
           WRITE DRX-SATZ FROM WH-ESC AFTER 0.
       Z.  EXIT.
      ******************************************************************
       ANZAHL SECTION.
       A.  MOVE "l" TO WH-ST2.
           MOVE WH-ESC TO WC-LSL(3).
       C.  PERFORM VARYING WC-LNG FROM 430 BY -1 UNTIL WC-LNG < 50
               or WC-X(WC-LNG) NOT = SPACE CONTINUE.
           IF WH-ST1 > 31 DIVIDE 3 INTO WC-LNG.
           WRITE DRX-SATZ FROM WB-BC AFTER 0.
       Z.  EXIT.
      ***************************************** Druckerparameter laden *
       LAD-DRUCK SECTION.
      *                                  *> 11= quer A4 /10" 6 Zeilen <-
       A.  MOVE "(10U&l26a8c1O(s0p10h15v0b0s3T" TO WE-STG(1).
      *                                  *> 12= quer A4 /12" 6 Zeilen <-
           MOVE "(10U&l26a8c1O(s0p12h15v0b0s3T" TO WE-STG(2).
      *                                  *> 13= quer A4 /16" 6 Zeilen <-
           MOVE "(10U&l26a8c1O(s0p16.7h15v0b0s3T" TO WE-STG(3).
      *                                  *> 14= hoch A4 /10" 6 Zeilen <-
           MOVE "(10U&l26a8c0O(s0p10h15v0b0s3T" TO WE-STG(4).
      *                                  *> 15= hoch A4 /12" 6 Zeilen <-
           MOVE "(10U&l26a8c0O(s0p12h15v0b0s3T" TO WE-STG(5).
      *                                  *> 16= hoch A4 /16" 6 Zeilen <-
           MOVE "(10U&l26a8c0O(s0p16.7h15v0b0s3T" TO WE-STG(6).
      *                                  *> 17= hoch A4 /12" 9 Zeilen <-
           MOVE "(10U&l26a6c0O(s0p12h15v0b0s3T" TO WE-STG(7).
      *    &la: Format, c: 6 Zeilen, 1/0O: quer/hoch
      *    (sp: Abstand, h: Zeich./Zoll b: St„rke s: Schrift T: Schrift
       Z.  EXIT.
