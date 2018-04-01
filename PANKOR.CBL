      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PARLGCV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARTIKEL    ASSIGN TO "PANARTIK.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY AR-KEY
                             ALTERNATE RECORD KEY AR-AKEY DUPLICATES
                             ALTERNATE RECORD KEY AR-BKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT LAGARTIK   ASSIGN TO "PARARTIK.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY LG-KEY
                             ALTERNATE RECORD KEY LG-AKEY DUPLICATES
                             ALTERNATE RECORD KEY LG-BKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY "PANARTIK.CPY".
       COPY "PARARTIK.CPY".
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
           03  WH-MG.
               05 WR-MG                PIC X    OCCURS 3.
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
       A.  OPEN I-O ARTIKEL.
           OPEN I-O LAGARTIK.
           IF WF-STATUS NOT = "05" GO X.
           MOVE 999 TO AR-NUM.
           START ARTIKEL KEY NOT < AR-KEY INVALID GO X.
       C.  READ ARTIKEL NEXT AT END GO X.
           IF ZUGRIF GO C.
           DISPLAY AR-NUM AT 2020 " " AR-BEZ.
           MOVE AR-SATZ TO LG-SATZ.
           MOVE LG-NUM TO LG-NUM.
           MOVE LG-ABEZ TO LG-ABEZ.
           MOVE LG-BEZ TO LG-BEZ.
           MOVE LG-UST TO LG-UST.
           MOVE LG-PREIS TO LG-PREIS.
           MOVE LG-VKP TO LG-VKP.
           MOVE LG-EKP TO LG-EKP.
           MOVE LG-ANZ TO LG-ANZ.
           MOVE LG-GRM TO LG-GRM.
           MOVE LG-PL TO LG-PL.
           MOVE LG-APREIS TO LG-APREIS.
           MOVE LG-ADAT TO LG-ADAT.
           MOVE LG-AVKP TO LG-AVKP.
           MOVE LG-LAGER TO LG-LAGER.
           MOVE "KONDLAGER " TO LG-LAGER.
           WRITE LG-SATZ INVALID GO C.
           DELETE ARTIKEL INVALID NEXT SENTENCE.
           GO C.
       X.  CLOSE ARTIKEL LAGARTIK.
       Z.  STOP RUN.
