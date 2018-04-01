      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANDECV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRUEFDAT   ASSIGN TO "PANDEBIT.IDX"
                             ORGANIZATION RECORD SEQUENTIAL.
           COPY PANSEDEB.CPY.
           SELECT ODEBITOR   ASSIGN TO "OLDDEBIT.DAT"
                             ORGANIZATION RECORD SEQUENTIAL
      *                      ORGANIZATION INDEXED  ACCESS DYNAMIC
      *                      RECORD KEY ODE-KEY
      *                      ALTERNATE RECORD KEY ODE-MCODE DUPLICATES
      *                      ALTERNATE RECORD KEY ODE-RKEY DUPLICATES
      *                      ALTERNATE RECORD KEY ODE-IKEY =
      *                                ODE-TOUR, ODE-SUB DUPLICATES
      *                      ALTERNATE RECORD KEY ODE-EKEY = ODE-ETIK
      *                                ODE-KEY DUPLICATES
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
      ******************************************************************
       FD  PRUEFDAT                    LABEL RECORD STANDARD.
       01  PRF-SATZ                    PIC X(256).
      *****************************************************************
       FD  ODEBITOR     external       LABEL RECORDS STANDARD
                                       RECORD IS VARYING IN
                                       SIZE FROM 227 TO 246
                                       DEPENDING ON WH-SIZE.
       01  ODE-SATZ.
           03 ODE-GKEY.
               05 ODE-FNR              PIC 999       COMP.
               05 ODE-KEY.
                  07 ODE-KTONR         PIC 9(6)      COMP.
           03 ODE-MCODE                PIC X(8).
           03 ODE-KTOART               PIC 9.
           03 ODE-ETIK                 PIC 99        COMP.
           03 ODE-ANLMERK              PIC 99        COMP.
           03 ODE-ANMERK               PIC X(60).
           03 ODE-KOND                 PIC 9.
           03 ODE-BEZ                  PIC X(110).
           03 ODE-RET                  PIC 999V9     COMP.
           03 ODE-PRDAT                PIC 9(8)      COMP.
           03 ODE-SPERR                PIC 9(8)      COMP.
           03 ODE-REKTO                PIC 9(6)      COMP.
           03 ODE-RAST                 PIC 9.
           03 ODE-RAB                  PIC S99V9     COMP.
           03 ODE-EAN                  PIC 9.
           03 ODE-RKEY.
               05 ODE-FAKART           PIC 99.
               05 ODE-TOUR             PIC 99.
               05 ODE-SUB              PIC 9999      COMP.
      *---------------------> neue Eintragung fr Fakturenliste <-
           03 ODE-INKASSO              PIC 99        COMP.
           03 ODE-RENR                 PIC 9(6)      COMP.
           03 ODE-REDAT                PIC 9(6)      COMP.
           03 ODE-REBET                PIC S9(6)V99  COMP.
           03 ODE-ANR                  PIC 99        COMP.
           03 ODE-STATIS               PIC 99        COMP.
           03 ODE-BONUS                PIC 99V99     COMP.
           03 ODE-RBASIS               PIC 99V99     COMP.
           03 ODE-FILNR                PIC 9(5).
           03 ODE-GLN                  PIC 9(14).
           03 ODE-UID                  PIC X(14).
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
           03  WH-SIZE                 PIC 9(5).
           03  wi                      pic 99      COMP.
           03  WH-KETTD                PIC 9(5)    VALUE ZERO.
           03  RED REDEFINES WH-KETTD.
               05  WH-ANREDE           PIC 9.
               05  WH-STATIS           PIC 9.
               05  WH-FAKART           PIC 9.
               05  WH-TOUR             PIC 99.
           03  WH-GRM.
               05 WH-GRP               PIC 99       COMP-X.
               05 WH-SON               PIC 99       COMP.
               05 WH-MEH               PIC 99       COMP.
               05 WH-NK                PIC 99       COMP.
               05 WH-TB                PIC 99       COMP.
       COPY PANEXT.CPY.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON ODEBITOR.
       A.  CALL "CADECL" USING "olddebit.dat" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           OPEN INPUT PRUEFDAT.
           READ PRUEFDAT NEXT AT END GO Z.
           CLOSE PRUEFDAT.
           IF PRF-SATZ(57:2) = x"0104" GO Q.
           CALL "CBL_COPY_FILE" USING "PANDEBIT.DAT " "OLDDEBIT.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING "PANDEBIT.IDX " "OLDDEBIT.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
       I.  OPEN INPUT ODEBITOR.
           IF WF-STATUS = "35" GO Z.
           DELETE FILE DEBITOR.
           OPEN I-O DEBITOR.
           MOVE LOW-VALUES TO ODE-SATZ.
      *    START ODEBITOR KEY NOT < ODE-KEY INVALID GO M.
       K.  READ ODEBITOR NEXT IGNORE LOCK AT END GO M.
           DISPLAY ODE-KTONR AT 2020.
           MOVE ODE-SATZ TO DE-SATZ.
           MOVE SPACE TO DE-UID.
           IF WH-SIZE not = 246 MOVE 0 TO DE-FILNR DE-GLN.
           WRITE DE-SATZ INVALID GO K.
           DISPLAY DE-KTONR AT 0543 " " DE-BEZ(1:30).
           GO K.
       M.  CLOSE ODEBITOR.
           DELETE FILE ODEBITOR.
           CLOSE DEBITOR.
           GO X.
       Q.  OPEN INPUT DEBITOR.
           IF WF-STATUS not = "39" CLOSE DEBITOR GO Z.
           CALL "CBL_COPY_FILE" USING "PANDEBIT.DAT " "OLDDEBIT.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING "PANDEBIT.IDX " "OLDDEBIT.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
           OPEN INPUT ODEBITOR.
           IF WF-STATUS = "35" GO Z.
           DELETE FILE DEBITOR.
           OPEN I-O DEBITOR.
           MOVE LOW-VALUES TO ODE-SATZ.
       S.  READ ODEBITOR NEXT IGNORE LOCK AT END GO M.
           DISPLAY ODE-KTONR AT 2020.
           MOVE ODE-SATZ TO DE-SATZ.
           WRITE DE-SATZ INVALID GO S.
           DISPLAY DE-KTONR AT 0543 " " DE-BEZ(1:30).
           GO S.
       X.  DISPLAY "Konvertierung beendet!" AT 2060.
       Z.  EXIT PROGRAM.
      ******************************************************************
       OLD SECTION.
       A.  MOVE ODE-FNR TO DE-FNR.
           MOVE ODE-KTONR TO DE-KTONR.
           MOVE ODE-MCODE TO DE-MCODE.
           MOVE ODE-KTOART TO DE-KTOART.
           MOVE 0 TO DE-ETIK.
           MOVE ODE-ANLMERK TO DE-ANLMERK.
           MOVE ODE-ANMERK TO DE-ANMERK.
           MOVE ODE-KOND TO DE-KOND.
           MOVE ODE-BEZ TO DE-BEZ.
           MOVE ODE-RET TO DE-RET.
           MOVE ODE-PRDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO DE-PRDAT.
           MOVE ODE-SPERR TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO DE-SPERR.
           MOVE ODE-REKTO TO DE-REKTO.
           MOVE ODE-RAST TO DE-RAST.
           MOVE ODE-RAB TO DE-RAB.
           MOVE ODE-EAN TO DE-EAN.
      *    MOVE ODE-KETTD TO WH-KETTD.
           MOVE 1 TO DE-INKASSO.
           IF ODE-RAB(1:2) = X"000F" MOVE 0 TO ODE-RAB.
           MOVE ODE-RAB TO DE-RAB.
           MOVE ODE-EAN TO DE-EAN.
           MOVE WH-FAKART TO DE-FAKART.
           MOVE WH-STATIS TO DE-STATIS.
           MOVE WH-ANREDE TO DE-ANR.
           MOVE WH-TOUR TO DE-TOUR.
           MOVE ODE-SUB TO DE-SUB.
           MOVE 0 TO DE-INKASSO DE-RENR DE-REDAT DE-REBET.
           MOVE 0 TO DE-BONUS DE-RBASIS.
       Z.  EXIT.
