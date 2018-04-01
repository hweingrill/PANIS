      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PANFACV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSEFAK.CPY.
           SELECT AFAKTDAT   ASSIGN TO "\BUGSI\PANEFAKT.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY AFA-KEY
                             ALTERNATE RECORD KEY AFA-AKEY
                             FILE STATUS IS WF-STATUS.
           COPY PANSEDEB.CPY.
           SELECT ADEBITOR   ASSIGN TO "\BUGSI\DEBITORE.FII"
                             ORGANIZATION INDEXED  ACCESS DYNAMIC
                             RECORD KEY ADE-KEY
                             ALTERNATE RECORD KEY ADE-MCODE DUPLICATES
                             FILE STATUS WF-STATUS.
      *    SELECT ODEBITOR   ASSIGN TO "ODEBI.DAT"
      *                      ORGANIZATION INDEXED  ACCESS DYNAMIC
      *                      RECORD KEY ODE-KEY
      *                      ALTERNATE RECORD KEY ODE-MCODE DUPLICATES
      *                      ALTERNATE RECORD KEY ODE-RKEY DUPLICATES
      *                      ALTERNATE RECORD KEY ODE-IKEY =
      *                                ODE-TOUR, ODE-SUB DUPLICATES
      *                      FILE STATUS WF-STATUS.
           COPY PANSEART.CPY.
           SELECT ALTARTIK   ASSIGN TO "\BUGSI\PANARTIK.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY AAR-KEY
                             ALTERNATE RECORD KEY AAR-AKEY DUPLICATES
                             ALTERNATE RECORD KEY AAR-BKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       copy panfakt.cpy.
      ********************************************* Dauerbestellungen *
       FD  AFAKTDAT      external       LABEL RECORD STANDARD
                                        RECORD 320 CHARACTERS.
       01  AFA-SATZ.
           03  AFA-AKEY.
               05 AFA-MON               PIC 9999       COMP.
               05 AFA-ART               PIC 99         COMP.
               05 AFA-KEY.
                  07 AFA-KTONR          PIC 9(6)       COMP.
                  07 AFA-ARNUM          PIC 9(4)       COMP.
           03  AFA-AR                        OCCURS 40 INDEXED OX.
               05 AFA-MENGE             PIC S9(5)      COMP-3.
               05 AFA-RETOUR            PIC S9(5)      COMP-3.
           03  AFA-XTAB.
               05 AFA-TAB                       OCCURS 4  INDEXED PX.
                  07 AFA-PREIS          PIC S9(4)V999  COMP-3.
                  07 AFA-AB             PIC 9(6)       COMP.
                  07 AFA-BIS            PIC 9(6)       COMP.
                  07 AFA-RET            PIC S9(4)V999  COMP-3.
                  07 AFA-RAB            PIC S99V9      COMP-3.
           03  AFA-KUARNUM              PIC 9(14)      COMP.
           03  FILLER                   PIC X.
       COPY PANDEBI.CPY.
      *****************************************************************
       FD  ADEBITOR     external       RECORD 256 CHARACTERS
                                       LABEL RECORDS STANDARD.
       01  ADE-SATZ.
           03  ADE-KEY.
               05 ADE-FNR               PIC 999       COMP-3.
               05 ADE-KTONR             PIC 9(6)      COMP-3.
           03  ADE-MCODE                PIC X(8).
           03  ADE-KTOART               PIC 9.
           03  ADE-ANLMERK              PIC 99.
           03  FILLER                  PIC X(30).
           03  ADE-FWKZ                 PIC 9.
           03  ADE-OPKZ                 PIC 9.
           03  ADE-ANMERK               PIC X(60).
           03  ADE-KOND                 PIC 9.
           03  ADE-MAHNT                PIC 999       COMP-3.
           03  ADE-MAHND                PIC 9(6)      COMP-3.
           03  ADE-BEZ                  PIC X(110).
           03  FILLER                  PIC 9(9).
           03  ADE-RET                  PIC 999V9     COMP.
           03  ADE-PRDAT                PIC 9(6)      COMP.
           03  ADE-SPERR                PIC 9(6)      COMP.
           03  ADE-KETTD                PIC 9(5)      COMP-3.
           03  ADE-REKTO                PIC 9(6)      COMP-3.
           03  ADE-RAST                 PIC 9.
           03  ADE-RAB                  PIC S99V9     COMP-3.
           03  ADE-EAN                  PIC 9.
           03  ADE-SUB                  PIC 9999      COMP.
      *****************************************************************
      *FD  ODEBITOR      external       LABEL RECORDS STANDARD.
      *01  ODE-SATZ.
      *    03  ODE-KEY.
      *        05 ODE-FNR               PIC 999       COMP-3.
      *        05 ODE-KTONR             PIC 9(6)      COMP-3.
      *    03  ODE-MCODE                PIC X(8).
      *    03  ODE-KTOART               PIC 9.
      *    03  ODE-SFTKOND              PIC 99        COMP.
      *    03  ODE-ANLMERK              PIC 99        COMP.
      *    03  ODE-ANMERK               PIC X(60).
      *    03  ODE-KOND                 PIC 9.
      *    03  ODE-BEZ                  PIC X(110).
      *    03  ODE-RET                  PIC 999V9     COMP.
      *    03  ODE-PRDAT                PIC 9(6)      COMP.
      *    03  ODE-SPERR                PIC 9(6)      COMP.
      *    03  ODE-REKTO                PIC 9(6)      COMP-3.
      *    03  ODE-RAST                 PIC 9.
      *    03  ODE-RAB                  PIC S99V9     COMP-3.
      *    03  ODE-EAN                  PIC 9.
      *    03  ODE-RKEY.
      *        05 ODE-FAKART            PIC 99.
      *        05 ODE-TOUR              PIC 99.
      *        05 ODE-SUB               PIC 9999      COMP.
      *---------------------------> neue Eintragung fÅr Fakturenliste <-
      *    03  ODE-INKASSO              PIC 99        COMP.
      *    03  ODE-RENR                 PIC 9(6)      COMP.
      *    03  ODE-REDAT                PIC 9(6)      COMP.
      *    03  ODE-REBET                PIC S9(6)V99  COMP.
      *    03  ODE-ANR                  PIC 99        COMP.
      *    03  ODE-STATIS               PIC 99        COMP.
      *    03  ODE-BONUS                PIC 99V99     COMP.
      *    03  ODE-RBASIS               PIC 99V99     COMP.
       COPY PANFDART.CPY.
      *****************************************************************
       FD  ALTARTIK     external       LABEL RECORD STANDARD
                                       RECORD 80 CHARACTERS.
       01  AAR-SATZ.
           03  AAR-KEY.
               05 AAR-NUM               PIC 9(4)       COMP.
           03  AAR-AKEY.
               05 AAR-SA                PIC X.
               05 AAR-ABEZ              PIC X(10).
           03  AAR-BEZ                  PIC X(25).
           03  AAR-KTO                  PIC 9(8)       COMP.
           03  AAR-UST                  PIC 99         COMP.
           03  AAR-PREIS                PIC S9(4)V999  COMP-3.
           03  AAR-VKP                  PIC S9(5)V99   COMP-3.
           03  AAR-EKP                  PIC S9(4)V999  COMP-3.
           03  AAR-ANZ                  PIC S9(5)      COMP-3.
           03  AAR-GRM                  PIC X(5).
           03  AAR-PL                   PIC 9.
           03  AAR-APREIS               PIC S9(4)V999  COMP-3.
           03  AAR-ADAT                 PIC 9(6)       COMP.
           03  AAR-AVKP                 PIC S9(5)V99   COMP-3.
           03  AAR-BKEY.
               05 AAR-GVZ               PIC 99         COMP.
           03  FILLER                  PIC X(4).
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
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON AFAKTDAT.
       A.  CALL "CADECL" USING "panefakt.dat" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           OPEN INPUT FAKTDAT.
           IF WF-STATUS < "10" CLOSE FAKTDAT GO I.
           IF WF-STATUS = "35" GO I.
           OPEN I-O AFAKTDAT.
           OPEN OUTPUT FAKTDAT.
           MOVE LOW-VALUES TO AFA-SATZ.
           START AFAKTDAT KEY NOT < AFA-KEY INVALID GO G.
       C.  READ AFAKTDAT NEXT AT END GO G.
           IF ZUGRIF GO C.
           INITIALIZE FA-SATZ.
           MOVE AFA-MON TO FA-MON.
           MOVE AFA-ART TO FA-ART.
           MOVE AFA-KTONR TO FA-KTONR.
           MOVE AFA-ARNUM TO FA-ARNUM.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 40
               MOVE AFA-MENGE(WI) TO FA-MENGE(WI)
               MOVE AFA-RETOUR(WI) TO FA-RETOUR(WI).
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 4
               MOVE AFA-PREIS(WI) TO FA-PREIS(WI)
               MOVE AFA-AB(WI) TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE WX-DATUM TO FA-AB(WI)
               MOVE AFA-BIS(WI) TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE WX-DATUM TO FA-BIS(WI)
               MOVE AFA-RET(WI) TO FA-RET(WI)
               MOVE AFA-RAB(WI) TO FA-RAB(WI).
           MOVE AFA-KUARNUM TO FA-KUARNUM.
           WRITE FA-SATZ INVALID STOP RUN.
           DISPLAY FA-KTONR AT 0743.
           WRITE FA-SATZ INVALID GO C.
           GO C.
       G.  CLOSE AFAKTDAT.
           CLOSE FAKTDAT.
           CALL "CBL_COPY_FILE" USING
                "PANEFAKT.DAT " "\BUGSI\PANDEFAKT.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING
                "PANEFAKT.IDX " "\BUGSI\PANDEFAKT.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
       I.  OPEN INPUT DEBITOR.
           IF WF-STATUS < "10" CLOSE DEBITOR GO O.
           IF WF-STATUS = "35" GO Z.
           OPEN I-O ADEBITOR.
           OPEN OUTPUT DEBITOR.
           MOVE LOW-VALUES TO ADE-SATZ.
           START ADEBITOR KEY NOT < ADE-KEY INVALID GO M.
       K.  READ ADEBITOR NEXT AT END GO M.
           IF ZUGRIF GO K.
           DISPLAY ADE-KTONR AT 2020.
           INITIALIZE DE-SATZ.
           MOVE ADE-FNR TO DE-FNR.
           MOVE ADE-KTONR TO DE-KTONR.
           MOVE ADE-MCODE TO DE-MCODE.
           MOVE ADE-KTOART TO DE-KTOART.
           MOVE 0 TO DE-ETIK.
           MOVE ADE-ANLMERK TO DE-ANLMERK.
           MOVE ADE-ANMERK TO DE-ANMERK.
           MOVE ADE-KOND TO DE-KOND.
           MOVE ADE-BEZ TO DE-BEZ.
           MOVE ADE-RET TO DE-RET.
           MOVE ADE-PRDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO DE-PRDAT.
           MOVE ADE-SPERR TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO DE-SPERR.
           MOVE ADE-REKTO TO DE-REKTO.
           MOVE ADE-RAST TO DE-RAST.
           MOVE ADE-RAB TO DE-RAB.
           MOVE ADE-EAN TO DE-EAN.
           MOVE ADE-KETTD TO WH-KETTD.
           MOVE 1 TO DE-INKASSO.
           IF ADE-RAB(1:2) = X"000F" MOVE 0 TO ADE-RAB.
           MOVE ADE-RAB TO DE-RAB.
           MOVE ADE-EAN TO DE-EAN.
           MOVE WH-FAKART TO DE-FAKART.
           MOVE WH-STATIS TO DE-STATIS.
           MOVE WH-ANREDE TO DE-ANR.
           MOVE WH-TOUR TO DE-TOUR.
           MOVE ADE-SUB TO DE-SUB.
           MOVE 0 TO DE-INKASSO DE-RENR DE-REDAT DE-REBET.
           MOVE 0 TO DE-BONUS DE-RBASIS.
           WRITE DE-SATZ INVALID GO K.
           DISPLAY DE-KTONR AT 0543 " " DE-BEZ(1:30).
           GO K.
       M.  CLOSE ADEBITOR.
           CLOSE DEBITOR.
           CALL "CBL_COPY_FILE" USING
                "PANDEBIT.DAT " "\BUGSI\PANDEBIT.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING
                "PANDEBIT.IDX " "\BUGSI\PANDEBIT.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
      *--------------------------------------------> Artikelkonvertg. <-
       O.  OPEN INPUT ARTIKEL.
           IF WF-STATUS < "10" CLOSE ARTIKEL GO Z.
           IF WF-STATUS = "35" GO Z.
           OPEN I-O ALTARTIK.
           OPEN OUTPUT ARTIKEL.
           MOVE LOW-VALUES TO AAR-SATZ.
           START ALTARTIK KEY NOT < AAR-KEY INVALID GO Q.
       P.  READ ALTARTIK NEXT AT END GO Q.
           IF ZUGRIF GO P.
           INITIALIZE AR-SATZ.
           MOVE AAR-NUM TO AR-NUM.
           MOVE AAR-SA TO AR-SA.
           MOVE AAR-ABEZ TO AR-ABEZ.
           MOVE AAR-BEZ TO AR-BEZ
           MOVE AAR-KTO TO AR-KTO
           MOVE AAR-UST TO AR-UST
           MOVE AAR-PREIS TO AR-PREIS
           MOVE AAR-VKP TO AR-VKP
           MOVE AAR-EKP TO AR-EKP
           MOVE AAR-ANZ TO AR-ANZ
           MOVE AAR-GRM TO WH-GRM.
           MOVE WH-GRP TO AR-GRP
           MOVE WH-SON TO AR-SON
           MOVE WH-MEH TO AR-MEH
           MOVE WH-NK TO AR-NK
           MOVE WH-TB TO AR-TB
           MOVE AAR-PL TO AR-PL
           MOVE AAR-APREIS TO AR-APREIS
           MOVE AAR-AVKP TO AR-AVKP
           MOVE AAR-GVZ TO AR-GVZ
           MOVE 0 TO AR-RAB AR-OEPZ
           MOVE AAR-ADAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO AR-ADAT.

           WRITE AR-SATZ INVALID GO P.
           DISPLAY AR-NUM AT 0543 " " AR-ABEZ.
           GO P.
       Q.  CLOSE ALTARTIK.
           CLOSE ARTIKEL.
           CALL "CBL_COPY_FILE" USING
                "PANARTIK.DAT " "\BUGSI\PANARTIK.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING
                "PANARTIK.IDX " "\BUGSI\PANARTIK.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
           DISPLAY "Konvertierung beendet!" AT 2060.
           STOP RUN.
       Z.  EXIT PROGRAM.
