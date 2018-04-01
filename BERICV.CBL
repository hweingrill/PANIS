      $SET LINKCOUNT"320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BERICHT.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OLDKER   ASSIGN "OLDBERI.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OM-KEY
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
       01  OM-SATZ.
           03  OM-KEY.
               05 OM-GRP               PIC 99         COMP.
               05 OM-ZEIL              PIC 999V9      COMP.
           03  OM-ART                  PIC 99         COMP.
           03  OM-BET                  PIC S9(7)V99   COMP.
           03  OM-TEXT                 PIC X(42).
           03  OM-KZ                   PIC 99         COMP.
           03  OM-DATUM                PIC 9(8)       COMP.
           03  OM-ZEIT                 PIC 9(4)       COMP.
           03  OM-POS1                 PIC 99         COMP.
           03  OM-POS2                 PIC 99         COMP.
           03  OM-ZAHL                 PIC 9(6)       COMP.
           03  OM-EXTEXT               PIC X(30).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       PROCEDURE DIVISION.
      ******************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           DISPLAY " Konvertierung " AT 1010.
           OPEN INPUT KERMODEL.
           IF WF-STATUS = "00" CLOSE KERMODEL GO Z.
           CALL "CBL_COPY_FILE" USING "FIBUBER.DAT " "OLDBERI.DAT ".
           CALL "CBL_COPY_FILE" USING "FIBUBER.IDX " "OLDBERI.IDX ".
           DELETE FILE KERMODEL.
           OPEN I-O KERMODEL.
           OPEN INPUT OLDKER.
       C.  READ OLDKER NEXT AT END GO X.
           MOVE OM-SATZ TO KM-SATZ.
           IF OM-GRP = 0 GO C.
           COMPUTE KM-ZEIT = OM-ZEIT * 100.
           MOVE OM-POS1 TO KM-POS1.
           MOVE OM-POS2 TO KM-POS2.
           MOVE OM-ZAHL TO KM-ZAHL.
           MOVE OM-EXTEXT TO KM-EXTEXT.
           WRITE KM-SATZ.
           GO C.
       X.  CLOSE KERMODEL.
           CLOSE OLDKER.
       Z.  GOBACK.
