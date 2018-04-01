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
           SELECT AFAKTDAT   ASSIGN TO "OLDEFAKT.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY AFA-KEY
                             ALTERNATE RECORD KEY AFA-AKEY
                             ALTERNATE RECORD AFA-BKEY =
                                     AFA-KTONR AFA-KUARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT PRUEFDAT   ASSIGN TO "PANEFAKT.IDX"
                             ORGANIZATION RECORD SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       copy panfakt.cpy.
      ******************************************************************
       FD  PRUEFDAT                    LABEL RECORD STANDARD.
       01  PRF-SATZ                    PIC X(256).
      ********************************************* Dauerbestellungen *
       FD  AFAKTDAT      external       LABEL RECORD STANDARD.
       01  AFA-SATZ.
           03  AFA-AKEY.
               05 AFA-MON               PIC 9999       COMP.
               05 AFA-ART               PIC 99         COMP.
               05 AFA-KEY.
                  07 AFA-KTONR          PIC 9(6)       COMP.
                  07 AFA-ARNUM          PIC 9(4)       COMP.
           03  AFA-AR                           OCCURS 40.
               05 AFA-MENGE             PIC S9(5)      COMP.
               05 AFA-RETOUR            PIC S9(5)      COMP.
           03  AFA-XTAB.
               05 AFA-TAB                       OCCURS 4.
                  07 AFA-PREIS          PIC S9(4)V999  COMP.
                  07 AFA-AB             PIC 9(8)       COMP.
                  07 AFA-BIS            PIC 9(8)       COMP.
                  07 AFA-RET            PIC S9(4)V999  COMP.
                  07 AFA-RAB            PIC S99V9      COMP.
           03  AFA-KUARNUM              PIC 9(14)      COMP.
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
       A.  CALL "CADECL" USING "PANEFAKT.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           OPEN INPUT PRUEFDAT.
           READ PRUEFDAT NEXT AT END GO Z.
           CLOSE PRUEFDAT.
           IF PRF-SATZ(57:2) = x"037A" GO Z.
           CALL "CBL_COPY_FILE" USING "PANEFAKT.DAT " "OLDEFAKT.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING "PANEFAKT.IDX " "OLDEFAKT.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.

           OPEN I-O AFAKTDAT.
           OPEN OUTPUT FAKTDAT.
           MOVE LOW-VALUES TO AFA-SATZ.
           START AFAKTDAT KEY NOT < AFA-KEY INVALID GO G.
       C.  READ AFAKTDAT NEXT IGNORE LOCK AT END GO G.

           MOVE AFA-SATZ TO FA-SATZ.
           INITIALIZE FA-RAST(1) FA-RAST(2) FA-RAST(3).
           MOVE AFA-MON TO FA-RMON(1).

           WRITE FA-SATZ INVALID STOP RUN.
           DISPLAY FA-KTONR AT 0743.
           GO C.
       G.  CLOSE AFAKTDAT.
           DELETE FILE AFAKTDAT.
           CLOSE FAKTDAT.
           DISPLAY "Konvertierung beendet!" AT 2060.
       Z.  GOBACK.
