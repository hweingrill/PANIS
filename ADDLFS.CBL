      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ADDLFS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY PANSELFS.CPY.
           SELECT ALFSCHEIN  ASSIGN TO DISK "\BUGSI\PANELFS.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ALF-KEY
                             ALTERNATE RECORD KEY ALF-AKEY DUPLICATES
                             ALTERNATE RECORD KEY ALF-ARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANLIEF.CPY.
      *****************************************************************
       FD  ALFSCHEIN     external       LABEL RECORD STANDARD.
       01  ALF-SATZ.
           03  ALF-AKEY.
               05 ALF-DATUM             PIC 9(8)       COMP.
               05 ALF-TOUR              PIC 99         COMP.
               05 ALF-SUB               PIC 9999       COMP.
               05 ALF-KEY.
                  07 ALF-KTONR          PIC 9(6)       COMP.
                  07 ALF-NUM            PIC 9(6)       COMP.
                  07 ALF-ARNUM.
                     09 ALF-ARNUM       PIC 9(4)       COMP.
           03  ALF-MENGE                PIC S9(5)      COMP   OCCURS 3.
           03  ALF-PREIS                PIC S9(4)V999  COMP.
           03  ALF-BEZ                  PIC X(25).
           03  ALF-GRM.
               05 ALF-GRP               PIC 99         COMP-X.
               05 ALF-SON               PIC 99         COMP.
               05 ALF-MEH               PIC 99         COMP.
               05 ALF-NK                PIC 99         COMP.
               05 ALF-TB                PIC 99         COMP.
           03  ALF-FART                 PIC 99         COMP.
           03  ALF-STAT                 PIC 99         COMP.
           03  ALF-EAN                  PIC 99         COMP.
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
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  CALL "CADECL" USING "panefakt.dat" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           CALL "CADECL" USING "99AUFRUF" WH-CREG.
           OPEN I-O LFSCHEIN.
           OPEN INPUT ALFSCHEIN.
           MOVE LOW-VALUES TO ALF-SATZ.
           START ALFSCHEIN KEY NOT < ALF-KEY INVALID GO G.
       C.  READ ALFSCHEIN NEXT AT END GO G.
           IF ZUGRIF GO C.
           MOVE ALF-SATZ TO LF-SATZ.
           DISPLAY LF-KTONR AT 0743.
           WRITE LF-SATZ INVALID GO C.
           DISPLAY LF-KTONR AT 0743.
           GO C.
       G.  CLOSE ALFSCHEIN.
           CLOSE LFSCHEIN.
       Z.  EXIT PROGRAM.
