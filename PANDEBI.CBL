      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PARLGCV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEBITOR    ASSIGN TO "DEBITORE.FII"
                             ORGANIZATION INDEXED  ACCESS DYNAMIC
                             RECORD KEY DE-KEY
                             ALTERNATE RECORD KEY DE-MCODE DUPLICATES
                             ALTERNATE RECORD KEY DE-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.

      *********************************** Rohstoffe und Betriebsstoffe *
       FD  OLDARTIK     external       LABEL RECORD STANDARD.
       01  ol-SATZ.
           03  ol-KEY.
               05 ol-NUM               PIC 9(4)       COMP.
           03  ol-AKEY.
               05 ol-ABEZ              PIC X(10).
           03  ol-BKEY.
               05 ol-LAGER             PIC X(5).
               05 ol-SORT              PIC 999V99     COMP.
           03  ol-CKEY.
               05 ol-LFT               PIC X(5).
           03  ol-BEZ                  PIC X(25).
           03  ol-STAND                PIC 9(5)V99    COMP.
           03  ol-MIND                 PIC 9(5)V99    COMP.
           03  ol-BMENGE               PIC 9(5)V99    COMP.
           03  ol-ART                  PIC 99         COMP.
           03  ol-EKP                  PIC S9(4)V999  COMP.
           03  ol-VKP                  PIC S9(5)V99   COMP.
           03  ol-PREIS                PIC S9(4)V999  COMP.
           03  ol-ANZ                  PIC S9(5)V999  COMP.
           03  ol-MEH                  PIC 99         COMP.
           03  ol-NK                   PIC 99         COMP.
           03  ol-BMEH                 PIC 99         COMP.
           03  ol-BNK                  PIC 99         COMP.
      *------------------> Preise & Rabatt des aktuellen Lieferanten <-
           03  ol-TAB         OCCURS 6.
               05 ol-LIEF              PIC X(5).
               05 ol-DATUM             PIC 9(6)       COMP.
               05 ol-BTTO              PIC S9(5)V99   COMP.
               05 ol-RAB               PIC S99V99     COMP.
               05 ol-ZRAB              PIC S99V99     COMP.
               05 ol-AUF               PIC S999V99    COMP.
               05 ol-SYM               PIC 99         COMP.
               05 ol-NTTO              PIC S9(5)V99   COMP.
               05 ol-TX                PIC X(15).
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
       COPY "WHCREG.CPY".
       01  WH-REG.
           03  WN-LAG                  PIC X(20)  VALUE "PARARTIK.IDX".
           03  WN-OLD                  PIC X(20)  VALUE "OLDARTIK.IDX".
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  CALL "CADECL" USING "PANARTIK.DAT" WH-CREG.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           IF WL-CA = 20 PERFORM NEULAGART GO Z.
           OPEN I-O ARTIKEL.
           OPEN I-O LAGARTIK.
           IF WF-STATUS NOT = "05" GO X.
           MOVE 999 TO AR-NUM.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO X.
       C.  READ ARTIKEL NEXT AT END GO X.
           IF ZUGRIF GO C.
           DISPLAY AR-NUM AT 2020 " " AR-BEZ.
           INITIALIZE LG-SATZ.
           MOVE AR-NUM TO LG-NUM.
           MOVE AR-ABEZ TO WT-TX.
           CALL "CAUP" USING "20CONV10" WH-CREG.
           MOVE WT-TX TO LG-ABEZ.
           MOVE AR-BEZ TO LG-BEZ.
           MOVE AR-UST TO LG-ART.
           IF AR-PREIS(1:) = SPACE
               MOVE 0 TO AR-APREIS AR-PREIS AR-ADAT.
           MOVE AR-PREIS TO LG-PREIS.
           MOVE AR-VKP TO LG-VKP.
           MOVE AR-EKP TO LG-EKP.
           MOVE 950101 TO LG-DATUM(1).
           MOVE AR-ANZ TO LG-ANZ.
           MOVE AR-GRM TO WF-GRM.
           MOVE WF-MEH TO LG-MEH LG-BMEH.
           MOVE WF-NK TO LG-NK LG-BNK.
           MOVE "KONDLAGER " TO LG-LAGER.
           WRITE LG-SATZ INVALID GO C.
           DELETE ARTIKEL INVALID NEXT SENTENCE.
           GO C.
       X.  CLOSE ARTIKEL LAGARTIK.
       Z.  EXIT PROGRAM.
      ********************************************* neuer Lagerartikel *
       NEULAGART SECTION.
       A.  OPEN INPUT LAGARTIK.
           IF WF-STATUS = "00" GO X.
           CLOSE LAGARTIK.
           CALL "CBL_COPY_FILE" USING WN-LAG WN-OLD RETURN-CODE.
           MOVE "DAT" TO WN-OLD (10:3) WN-LAG (10:3).
           CALL "CBL_COPY_FILE" USING WN-LAG WN-OLD RETURN-CODE.
           OPEN INPUT OLDARTIK.
           OPEN OUTPUT LAGARTIK.
           CLOSE LAGARTIK.
           OPEN I-O LAGARTIK.
           MOVE 0 TO OL-NUM.
           START OLDARTIK KEY NOT < OL-KEY INVALID GO X.
       C.  READ OLDARTIK NEXT AT END GO X.
           IF ZUGRIF GO C.
           DISPLAY OL-NUM AT 2020 " " OL-BEZ.
           INITIALIZE LG-SATZ.
           MOVE OL-SATZ TO LG-SATZ.
           MOVE 0 TO LG-GRP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE OL-TAB(WX) TO LG-TAB(WX).
           WRITE LG-SATZ INVALID GO C.
           GO C.
       X.  CLOSE OLDARTIK LAGARTIK.
           DELETE FILE OLDARTIK.
       Z.  EXIT.
