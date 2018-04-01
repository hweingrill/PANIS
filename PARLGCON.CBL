      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PARLGCON.
      *---------------------------------------------> Erweiterung EKP <-
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REZEPT     ASSIGN TO WN-REZ
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY RE-KEY
                             ALTERNATE RECORD KEY RE-AKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT OREZEPT    ASSIGN TO WN-OLD
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ORE-KEY
                             ALTERNATE RECORD KEY ORE-AKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           COPY PARSEART.CPY.
           SELECT OLDARTIK ASSIGN TO WN-OLD
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY OL-KEY
                           ALTERNATE RECORD KEY OL-AKEY DUPLICATES
                           ALTERNATE RECORD KEY OL-BKEY DUPLICATES
                           ALTERNATE RECORD KEY OL-CKEY DUPLICATES
                           FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY "PARFDART.CPY".
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
           03  ol-GRP                  PIC 99         COMP.
           03  ol-NETPZ                PIC 99V9       COMP.
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
           03  ol-INHALT.
               05 ol-INH               PIC X(25)   OCCURS 12.
       COPY PARREZEP.CPY.
      *****************************************************************
       FD  OREZEPT      external       LABEL RECORD STANDARD.
       01  ORE-SATZ.
           03  ORE-KEY.
               05 ORE-NUM               PIC 9(4)        COMP.
           03  ORE-BEZ.
               05 ORE-AKEY              PIC X(10).
               05 ORE-TX                PIC X(15).
           03  ORE-KNET                 PIC 99          COMP.
           03  ORE-FILL                 PIC 9(4)        COMP.
           03  ORE-KOMP                          OCCURS 20 INDEXED XR.
               05 ORE-KONUM             PIC 9(4)        COMP.
               05 ORE-KOBEZ             PIC X(20).
               05 ORE-MENGE             PIC 9(6)        COMP.
               05 ORE-PROZ REDEFINES
                  ORE-MENGE             PIC 999V999     COMP.
               05 ORE-TNAM              PIC 9(6)        COMP.
               05 ORE-EKP               PIC 9(4)V99     COMP.
               05 ORE-NGEW              PIC 9(6)        COMP.
           03  ORE-TEIL                 PIC 9(5)        COMP.
           03  ORE-GEW                  PIC 9(7)        COMP.
           03  ORE-BET                  PIC S9(7)V99    COMP.
           03  ORE-MAT                  PIC S9(7)V99    COMP.
           03  ORE-PREIS                PIC S9(5)V99    COMP.
           03  ORE-ART                  PIC 99          COMP.
           03  ORE-SKP                  PIC S9(5)V99    COMP.
           03  ORE-ZEIT                 PIC S99V9(4)    OCCURS 5 COMP.
           03  ORE-BLECH                PIC 99          COMP  OCCURS 3.
           03  ORE-TEMP                 PIC 99V9        COMP.
           03  ORE-GNR                  PIC 99          COMP.
           03  ORE-GANG              OCCURS 3.              *> 4 = Pause
               05 ORE-GMIN              PIC 99          COMP.
               05 ORE-GSEK              PIC 99          COMP.
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
           03  WN-REZ                  PIC X(20)  VALUE "PANREZEP.DAT".
           03  WN-LAG                  PIC X(20)  VALUE "PARARTIK.IDX".
           03  WN-OLD                  PIC X(20)  VALUE "OLSDATEI.IDX".
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
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           PERFORM NEULAGART.
      *    PERFORM NEUREZEP.
       Z.  EXIT PROGRAM.
      ********************************************* neuer Lagerartikel *
       NEULAGART SECTION.
       A.  OPEN INPUT LAGARTIK.
           IF WF-STATUS = "00" CLOSE LAGARTIK GO Z.
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
      *    MOVE 0 TO LG-GRP LG-NETPZ.           *> zweite Konvertierung
      *    PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
      *        MOVE OL-TAB(WX) TO LG-TAB(WX).
           WRITE LG-SATZ INVALID GO C.
           GO C.
       X.  CLOSE OLDARTIK LAGARTIK.
           DELETE FILE OLDARTIK.
       Z.  EXIT.
      *********************************************** neue Rezeptdatei *
       NEUREZEP SECTION.
       A.  OPEN INPUT REZEPT.
           IF WF-STATUS = "00" CLOSE REZEPT GO Z.
           MOVE "IDX" TO WN-OLD (10:3) WN-REZ (10:3).
           CALL "CBL_COPY_FILE" USING WN-REZ WN-OLD RETURN-CODE.
           MOVE "DAT" TO WN-OLD (10:3) WN-REZ (10:3).
           CALL "CBL_COPY_FILE" USING WN-REZ WN-OLD RETURN-CODE.
           OPEN INPUT OREZEPT.
           OPEN OUTPUT REZEPT.
           CLOSE REZEPT.
           OPEN I-O REZEPT.
           MOVE 0 TO ORE-NUM.
           START OREZEPT KEY NOT < ORE-KEY INVALID GO X.
       C.  READ OREZEPT NEXT AT END GO X.
           IF ZUGRIF GO C.
           DISPLAY ORE-NUM AT 2020 " " ORE-BEZ.
           INITIALIZE RE-SATZ.
           MOVE ORE-SATZ TO RE-SATZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               MOVE ORE-KOMP(WX) TO RE-KOMP(WX)
               MOVE RE-MENGE(WX) TO RE-NGEW(WX).
           MOVE RE-GANG(3) TO RE-GANG(4).
           MOVE 0 TO RE-GMIN(3) RE-GSEK(3) RE-CHMAX RE-CHKZ RE-CHGEW.
           MOVE ORE-TEIL TO RE-TEIL.
           MOVE ORE-GEW TO RE-GEW.
           MOVE ORE-BET TO RE-BET.
           MOVE ORE-MAT TO RE-MAT.
           MOVE ORE-PREIS TO RE-PREIS.
           MOVE ORE-ART TO RE-ART.
           MOVE ORE-SKP TO RE-SKP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 5
               MOVE ORE-ZEIT(WX) TO RE-ZEIT(WX).
           MOVE ORE-BLECH(1) TO RE-BLECH(1).
           MOVE ORE-BLECH(2) TO RE-BLECH(2).
           MOVE ORE-BLECH(3) TO RE-BLECH(3).
           WRITE RE-SATZ INVALID GO C.
           GO C.
       X.  CLOSE OREZEPT REZEPT.
           DELETE FILE OREZEPT.
       Z.  EXIT.
