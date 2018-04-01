      $SET LINKCOUNT"320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     KASREAD.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OLDKER   ASSIGN "q:\vectron\vcom6\data\REP.DAT"
      *    SELECT OLDKER   ASSIGN "ECR.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT KERMODEL ASSIGN "FIBUBER.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KM-KEY
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  KERMODEL                    LABEL RECORD STANDARD.
       01  KM-SATZ.
           03  KM-KEY.
               05 KM-GRP               PIC 99         COMP.
               05 KM-ZEIL              PIC 999V9      COMP.
           03  KM-ART                  PIC 99         COMP.
           03  KM-BET                  PIC S9(7)V99   COMP.
           03  KM-TEXT                 PIC X(42).
           03  KM-KZ                   PIC 99         COMP.
           03  KM-DATUM                PIC 9(8)       COMP.
           03  KM-ZEIT                 PIC 9(6)       COMP.
           03  KM-POS1                 PIC 99         COMP.
           03  KM-POS2                 PIC 99         COMP.
           03  KM-ZAHL                 PIC 9(6)       COMP.
           03  KM-EXTEXT               PIC X(30).
      ******************************************************************
       FD  OLDKER                      LABEL RECORD STANDARD.
      *                                record is varying in size from
      *                                1 to 1024 depending on wh-rec.
       01  OM-SATZ                     PIC X(1024).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       01  WH-REC                      PIC 9(5)      COMP-x.
       COPY WHCREG.CPY.
       PROCEDURE DIVISION.
      ******************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           DISPLAY " Konvertierung " AT 1010.
           OPEN INPUT OLDKER reversed.
       C.  READ OLDKER NEXT AT END GO X.
           if om-satz(26:1) = x"01"
              CALL "CAUP" USING "0024010000" WH-CREG.
           GO C.
       X.  CLOSE KERMODEL.
           CLOSE OLDKER.
       Z.  GOBACK.
