      $SET LINKCOUNT"450" ANS85 BOUND NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PAK2000.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APROTWAAG  ASSIGN TO "\BUGSI\PAKWAAG.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY APWA-KEY DUPLICATES
                             ALTERNATE KEY APWA-AKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT ASREKOPF   ASSIGN TO "\BUGSI\PAKSRKOP.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ASK-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT ALAGERBZ   ASSIGN to "\BUGSI\PAKLAG01.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY ALB-KEY
                             ALTERNATE KEY IS ALB-JKEY
                             ALTERNATE KEY IS ALB-SRECH DUPLICATES
                                 SUPPRESS WHEN SPACES
                             FILE STATUS WF-STATUS.
      *-----------------------------------------------> neuen Dateien <-
       COPY PAKSEWAG.CPY.
       COPY PAKSESRE.CPY.
       COPY PAKOSEC.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY PAKFDWAG.CPY.
       COPY PAKFDSRE.CPY.
       COPY PAKOFD.CPY.
      ************************************************** Waagprotokoll *
       FD  APROTWAAG     external       LABEL RECORD STANDARD.
       01  APWA-SATZ.
           03  APWA-AKEY.
               05 APWA-SA               PIC 99        COMP.
               05 APWA-DAT              PIC 9(6)      COMP.
               05 APWA-KEY.
                  07 APWA-RENUM         PIC 9(4)      COMP.
           03  APWA-RPBEZ               PIC X(15).
           03  APWA-ZEIT                PIC 9(4)      COMP.
           03  APWA-ANZ                 PIC 9(5)V99   COMP.
           03  APWA-TAB.
               05 APWA-LGNUM            PIC 9(5)      COMP   OCCURS 20.
               05 APWA-SOLL             PIC 9(7)      COMP   OCCURS 20.
               05 APWA-IST              PIC 9(7)      COMP   OCCURS 20.
               05 APWA-KOBEZ            PIC X(25)            OCCURS 20.
      ******************************************** Scheinrechnungskopf *
       FD  ASREKOPF      external       LABEL RECORD STANDARD.
       01  ASK-SATZ.
           03  ASK-KEY.
               05 ASK-SA                PIC 99       COMP.
               05 ASK-LIEFER            PIC 9(5)     COMP.
               05 ASK-BELDAT            PIC 9(6)     COMP.
               05 ASK-BELNR             PIC 9(7)     COMP.
           03  ASK-LFD                  PIC 999      COMP.
           03  ASK-MCODE                PIC X(5).
           03  ASK-BEZ                  PIC X(30).
           03  ASK-BET                  PIC 9(7)V99  COMP.
           03  ASK-RAB                  PIC 99V9     COMP  OCCURS 2.
      ************************************* Journal- u. Buchungszeilen *
       FD  ALAGERBZ      external       LABEL RECORD STANDARD.
       01  ALB-SATZ.
           03  ALB-KEY.
               05 ALB-KTONR             PIC 9(6)      COMP.
               05 ALB-BUDAT             PIC 9(6)      COMP.
               05 ALB-LFD               PIC 99        COMP-X.
           03  ALB-JKEY.
               05 ALB-USE               PIC 99        COMP.
               05 ALB-SEITE             PIC 9(7)      COMP.
           03  ALB-SRECH.
               05 ALB-SA                PIC 99        COMP.
               05 ALB-LIEFER            PIC 9(6)      COMP.
               05 ALB-BELNR             PIC 9(7)      COMP.
               05 ALB-BELDAT            PIC 9(6)      COMP.
               05 ALB-SKLFD             PIC 9999      COMP.
           03  ALB-SYM                  PIC 99        COMP.
           03  ALB-BMEH                 PIC 99        COMP.
           03  ALB-BNK                  PIC 99        COMP.
           03  ALB-ANZ                  PIC S9(7)V99  COMP.
           03  ALB-MENGE                PIC S9(7)V99  COMP.
           03  ALB-PREIS                PIC S9(7)V99  COMP.
           03  ALB-BET                  PIC S9(7)V99  COMP.
           03  ALB-MEH                  PIC 99        COMP.
           03  ALB-NK                   PIC 99        COMP.
           03  ALB-KSTNR                PIC 99        COMP.
           03  ALB-RAB                  PIC S99V99    COMP   OCCURS 2.
           03  ALB-TX                   PIC X(15).
      *----------------------------------------> Jahresabschluásummen <-
       01  ALB-ASATZ.
           03  FILLER                   PIC X(30).
           03  ALB-SOLL                 PIC S9(9)V99  COMP-3.
           03  ALB-HABEN                PIC S9(9)V99  COMP-3.
           03  ALB-EBIL                 PIC S9(9)V99  COMP-3.
      **************************************************************-***
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  REGISTER.
           03  COP-STA                 PIC X(2)    COMP-5.
           03  WH-ANZ                  PIC 9(6).
           03  WH-HW                   PIC 99.
           03  WI                      PIC 999     COMP.
       COPY PANEXT.CPY.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CREG TO WH-CREG.
           MOVE WL-CALL TO WH-CALL.
           MOVE "PAKLAG01.DAT" TO WN-BZEI.
           MOVE "PAKWAAG.DAT" TO WN-BUEB.
           MOVE 9 TO WH-HW.
           CALL "CAUP" USING "1301012580" WH-CREG.
           perform CV-SRE.
           perform CV-WAG.
           perform CV-LB.
           MOVE WH-CREG TO WL-CREG.
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       CV-SRE SECTION.
           open input SREKOPF.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Scheinrechnung:" with FOREGROUND-COLOR 10 AT 0515.
           OPEN OUTPUT SREKOPF.
           OPEN INPUT ASREKOPF.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO ASK-SATZ.
           READ ASREKOPF NEXT AT END GO X.
           INITIALIZE SK-SATZ.
           MOVE ASK-SA TO SK-SA
           MOVE ASK-LIEFER TO SK-LIEFER
           MOVE ASK-BELDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG
           MOVE WX-DATUM TO SK-BELDAT
           MOVE ASK-BELNR TO SK-BELNR
           MOVE ASK-LFD TO SK-LFD
           MOVE ASK-MCODE TO SK-MCODE
           MOVE ASK-BEZ TO SK-BEZ
           MOVE ASK-BET TO SK-BET
           MOVE ASK-RAB(1) TO SK-RAB(1)
           MOVE ASK-RAB(2) TO SK-RAB(2).
           WRITE SK-SATZ INVALID STOP RUN.
           DISPLAY SK-LIEFER AT 0543 " " SK-BELDAT.
           GO C.
       X.  CLOSE ASREKOPF.
       Y.  CLOSE SREKOPF.
       Z.  EXIT.
      ******************************************************************
       CV-WAG SECTION.
           open input PROTWAAG.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Waage.........:" with FOREGROUND-COLOR 10 AT 0615.
           OPEN OUTPUT PROTWAAG.
           OPEN INPUT APROTWAAG.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO APWA-SATZ.
           READ APROTWAAG NEXT AT END GO X.
           INITIALIZE PWA-SATZ.
           MOVE APWA-SA TO PWA-SA
           MOVE APWA-DAT TO WC-DATUM
           CALL "CAUP" USING "04DATDREH" WH-CREG
           MOVE WX-DATUM TO PWA-DAT
           MOVE APWA-RENUM TO PWA-RENUM
           MOVE APWA-RPBEZ TO PWA-RPBEZ
           MOVE APWA-ZEIT TO PWA-ZEIT
           MOVE APWA-ANZ TO PWA-ANZ
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               MOVE APWA-LGNUM(WX) TO PWA-LGNUM(WX)
               MOVE APWA-SOLL(WX) TO PWA-SOLL(WX)
               MOVE APWA-IST(WX) TO PWA-IST(WX)
               MOVE APWA-KOBEZ(WX) TO PWA-KOBEZ(WX).
           WRITE PWA-SATZ INVALID STOP RUN.
           DISPLAY PWA-RENUM AT 0643.
           GO C.
       X.  CLOSE APROTWAAG.
       Y.  CLOSE PROTWAAG.
       Z.  EXIT.
      ******************************************************************
       CV-LB SECTION.
           open input LAGERBZ.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Lagerbuchung..:" with FOREGROUND-COLOR 10 AT 0715.
           OPEN OUTPUT LAGERBZ.
           OPEN INPUT ALAGERBZ.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO ALB-SATZ.
           READ ALAGERBZ NEXT AT END GO X.
           INITIALIZE LB-SATZ.
           MOVE ALB-KTONR TO LB-KTONR.
           MOVE ALB-BUDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG
           MOVE WX-DATUM TO LB-BUDAT.
           MOVE ALB-LFD TO LB-LFD.
           MOVE ALB-JKEY TO LB-JKEY.
           IF ALB-SRECH not = SPACE
               MOVE ALB-SA TO LB-SA
               MOVE ALB-LIEFER TO LB-LIEFER
               MOVE ALB-BELNR TO LB-BELNR
               MOVE ALB-BELDAT TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE WX-DATUM TO LB-BELDAT
               MOVE ALB-SKLFD TO LB-SKLFD
           else MOVE SPACE TO LB-SRECH.
           MOVE ALB-SYM TO LB-SYM.
           IF LB-SYM > 98 MOVE ALB-SOLL TO LB-SOLL
                          MOVE ALB-HABEN TO LB-HABEN
                          MOVE ALB-EBIL TO LB-EBIL
                          GO G.
           MOVE ALB-BMEH TO LB-BMEH.
           MOVE ALB-BNK TO LB-BNK.
           MOVE ALB-ANZ TO LB-ANZ.
           MOVE ALB-MENGE TO LB-MENGE.
           MOVE ALB-PREIS TO LB-PREIS.
           MOVE ALB-BET TO LB-BET.
           MOVE ALB-MEH TO LB-MEH.
           MOVE ALB-NK TO LB-NK.
           MOVE ALB-KSTNR TO LB-KSTNR.
           MOVE ALB-RAB(1) TO LB-RAB(1).
           MOVE ALB-RAB(2) TO LB-RAB(2).
           MOVE ALB-TX TO LB-TX.
       G.  WRITE LB-SATZ INVALID STOP RUN.
           DISPLAY LB-KTONR AT 0743.
           GO C.
       X.  CLOSE ALAGERBZ.
       Y.  CLOSE LAGERBZ.
       Z.  EXIT.
