      $SET LINKCOUNT"384" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PANCVART.
230712*------------------------> Konvertierung Artikel wg. Sortg. <-----
230712*------------------------> Konvertierung Lfschein wg. Sortg. <----
150812*------------------------> Konvertierung Lfschein wg. KEY-Umbau <-
150812*------------------------> Konvertierung Panefakt wg. Sortg. <----
      *----------> Lfschein-Druck Sortierung nach Gruppen u. Alphabet <-
      *----------> Rechnungs- u. Rastdruck ebenfalls sortieren <--------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRUEFDAT   ASSIGN TO WN-PRFDAT
                             ORGANIZATION RECORD SEQUENTIAL.
           COPY PANSEART.CPY.
           SELECT OARTIKEL   ASSIGN TO "OLDART.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OAR-KEY
                             ALTERNATE RECORD OAR-AKEY DUPLICATES
                             ALTERNATE RECORD OAR-BKEY DUPLICATES
                             ALTERNATE RECORD OAR-CKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           COPY PANSETIK.CPY.
           SELECT OETIKETT   ASSIGN TO "OLDETI.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OET-KEY
                             FILE STATUS IS WF-STATUS.
           COPY PANSELFS.CPY.
           SELECT OLFSCHEIN  ASSIGN TO DISK "OLDLFS.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OLF-KEY
                             ALTERNATE RECORD KEY OLF-AKEY DUPLICATES
                             ALTERNATE RECORD KEY OLF-ARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           COPY PANSEFAK.CPY.
           SELECT OFAKTDAT    ASSIGN TO "OLDFAKT.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OFA-KEY
                             ALTERNATE RECORD KEY OFA-AKEY
                             ALTERNATE RECORD OFA-BKEY =
                                     OFA-KTONR OFA-KUARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  PRUEFDAT                    LABEL RECORD STANDARD.
       01  PRF-SATZ                    PIC X(256).
       COPY PANFDART.CPY.
      ************************************************ Verkaufsartikel *
       FD OARTIKEL      external       LABEL RECORD STANDARD.
       01 OAR-SATZ.
           03 OAR-KEY.
               05 OAR-NUM               PIC 9(4)       COMP.
           03 OAR-AKEY.
               05 OAR-SA               PIC X.
               05 OAR-ABEZ             PIC X(10).
           03 OAR-BEZ                  PIC X(25).
           03 OAR-KTO                  PIC 9(8)       COMP.
           03 OAR-UST                  PIC 99         COMP.
           03 OAR-PREIS                PIC S9(4)V999  COMP.
           03 OAR-VKP                  PIC S9(5)V99   COMP.
           03 OAR-EKP                  PIC S9(4)V999  COMP.
           03 OAR-ANZ                  PIC S9(5)      COMP.
           03 OAR-GRM.
               05 OAR-GRP              PIC 99         COMP-X.
               05 OAR-SON              PIC 99         COMP.
               05 OAR-MEH              PIC 99         COMP.
               05 OAR-NK               PIC 99         COMP.
               05 OAR-TB               PIC 99         COMP.
           03 OAR-PL                   PIC 9.
           03 OAR-APREIS               PIC S9(4)V999  COMP.
           03 OAR-ADAT                 PIC 9(8)       COMP.
           03 OAR-AVKP                 PIC S9(5)V99   COMP.
           03 OAR-BKEY.
               05 OAR-GVZ               PIC 99         COMP.
           03 OAR-RAB                  PIC 99         COMP.
           03 OAR-CKEY.
               05 OAR-OEPZ              PIC 99         COMP.
           03 OAR-INH                  PIC 99V99      COMP.
           03 OAR-INH                  PIC X(8).
       COPY PANFETIK.CPY.
      *****************************************************************
       FD  OETIKETT      external       LABEL RECORD STANDARD.
       01  OET-SATZ.
           03  OET-KEY.
               05 OET-SA                PIC X.
               05 OET-NUM               PIC 9(4)       COMP.
           03  OET-BEZ-A                PIC X(20).
           03  OET-BEZ-B                PIC X(20).
           03  OET-ZU-A                 PIC X(39).
           03  OET-RES-A                PIC X.
           03  OET-ZU-B                 PIC X(39).
           03  OET-RES-B                PIC X.
           03  OET-ZU-C                 PIC X(39).
           03  OET-SONDER               PIC X.
           03  OET-MHD                  PIC 999.
           03  OET-EAN                  PIC 9(13).
       COPY PANLIEF.CPY.
      *****************************************************************
       FD  OLFSCHEIN     external       LABEL RECORD STANDARD.
       01  OLF-SATZ.
           03  OLF-AKEY.
               05 OLF-DATUM             PIC 9(8)       COMP.
               05 OLF-TOUR              PIC 99         COMP.
               05 OLF-SUB               PIC 9999       COMP.
               05 OLF-KEY.
                  07 OLF-KTONR          PIC 9(6)       COMP.
                  07 OLF-NUM            PIC 9(6)       COMP.
      *           07 OLF-ARNUM.
                  07 OLF-ARNUM          PIC 9(4)       COMP.
           03  OLF-MENGE                PIC S9(5)      COMP   OCCURS 3.
           03  OLF-PREIS                PIC S9(4)V999  COMP.
           03  OLF-BEZ                  PIC X(25).
           03  OLF-GRM.
               05 OLF-GRP               PIC 99         COMP-X.
               05 OLF-SON               PIC 99         COMP.
               05 OLF-MEH               PIC 99         COMP.
               05 OLF-NK                PIC 99         COMP.
               05 OLF-TB                PIC 99         COMP.
           03  OLF-FART                 PIC 99         COMP.
           03  OLF-STAT                 PIC 99         COMP.
           03  OLF-EAN                  PIC 99         COMP.
       COPY PANFAKT.CPY.
      **************************************** Fakturen- u. Preisdatei *
       FD  OFAKTDAT      external       LABEL RECORD STANDARD.
       01  OFA-SATZ.
           03  OFA-AKEY.
               05 OFA-MON               PIC 9999       COMP.
               05 OFA-ART               PIC 99         COMP.
               05 OFA-KEY.
                  07 OFA-KTONR          PIC 9(6)       COMP.
                  07 OFA-ARNUM          PIC 9(4)       COMP.
           03  OFA-MR.
               05 OFA-AR                       OCCURS 40 INDEXED OX.
                  07 OFA-MENGE          PIC S9(5)      COMP.
                  07 OFA-RETOUR         PIC S9(5)      COMP.
           03  OFA-XTAB.
               05 OFA-TAB                       OCCURS 4  INDEXED PX.
                  07 OFA-PREIS          PIC S9(4)V999  COMP.
                  07 OFA-AB             PIC 9(8)       COMP.
                  07 OFA-BIS            PIC 9(8)       COMP.
                  07 OFA-RET            PIC S9(4)V999  COMP.
                  07 OFA-RAB            PIC S99V9      COMP.
           03  OFA-KUARNUM              PIC 9(14)      COMP.
      *---------------------------> Rastermengen der letzten 3 Monate <-
           03  OFA-RAST                         OCCURS 3.
               05 OFA-RMON              PIC 9999       COMP.
               05 OFA-RMR.
                  07 OFA-RAR                       OCCURS 31.
                     09 OFA-RMENGE      PIC S9(5)      COMP.
                     09 OFA-RRETOUR     PIC S9(5)      COMP.
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
           03  WN-PRFDAT               PIC X(20).
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
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON OARTIKEL.
       A.  CALL "CADECL" USING "oldart.dat  " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           EVALUATE WL-CA
               WHEN 10 PERFORM ART-CV
                       PERFORM ETI-CV
               WHEN 20 PERFORM LFS-CV
               WHEN 30 PERFORM FAK-CV.
       Z.  EXIT PROGRAM.
      ****************************************** Konvertierung Artikel *
       ART-CV SECTION.
       A.  MOVE "PANARTIK.IDX" TO WN-PRFDAT.
           OPEN INPUT PRUEFDAT.
           READ PRUEFDAT NEXT AT END GO Z.
           CLOSE PRUEFDAT.
           IF PRF-SATZ(57:2) = x"0063" GO Z.
           CALL "CBL_COPY_FILE" USING "PANARTIK.DAT " "OLDART.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING "PANARTIK.IDX " "OLDART.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
       I.  OPEN INPUT OARTIKEL.
           IF WF-STATUS = "35" GO  Z.
           DELETE FILE ARTIKEL.
           OPEN I-O ARTIKEL.
           MOVE LOW-VALUES TO OAR-SATZ.
           START OARTIKEL KEY NOT < OAR-KEY INVALID GO M.
       K.  READ OARTIKEL NEXT IGNORE LOCK AT END GO M.
           MOVE OAR-SATZ TO AR-SATZ.
           MOVE SPACE TO AR-FILLER.
           MOVE 0 TO AR-SGRP AR-FILLER.
           WRITE AR-SATZ INVALID GO K.
           DISPLAY AR-NUM AT 0543 " " AR-BEZ(1:30).
           GO K.
       M.  CLOSE OARTIKEL.
           DELETE FILE OARTIKEL.
           CLOSE ARTIKEL.
           DISPLAY "Art.-Konvertierung beendet!" AT 2001.
       Z.  EXIT.
      **************************************** Konvertierung Etiketten *
       ETI-CV SECTION.
       A.  MOVE "PANPRETI.IDX" TO WN-PRFDAT.
           OPEN INPUT PRUEFDAT.
           READ PRUEFDAT NEXT AT END GO Z.
           CLOSE PRUEFDAT.
           IF PRF-SATZ(57:2) = x"00B3" GO Z.
           CALL "CBL_COPY_FILE" USING "PANPRETI.DAT " "OLDETI.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING "PANPRETI.IDX " "OLDETI.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
       I.  OPEN INPUT OETIKETT.
           IF WF-STATUS = "35" GO  Z.
           DELETE FILE ETIKETT.
           OPEN I-O ETIKETT.
           MOVE LOW-VALUES TO OET-SATZ.
           START OETIKETT KEY NOT < OET-KEY INVALID GO M.
       K.  READ OETIKETT NEXT IGNORE LOCK AT END GO M.
           MOVE OET-SATZ TO ET-SATZ.

      *    MOVE ET-ZU-B TO ET-ZUSATZ(2).
      *    MOVE ET-ZU-C TO ET-ZUSATZ(3).
      *    MOVE SPACE TO ET-ZUSATZ(4) ET-ZUSATZ(5) ET-ZUSATZ(6).
           MOVE OET-SONDER TO ET-SONDER.
           MOVE OET-MHD TO ET-MHD.
           MOVE OET-EAN TO ET-EAN.
           WRITE ET-SATZ INVALID GO K.
           DISPLAY ET-NUM AT 0543 " " ET-BEZ-A(1:30).
           GO K.
       M.  CLOSE OETIKETT.
           DELETE FILE OETIKETT.
           CLOSE ETIKETT.
           DISPLAY "Etiketten-Konvertierung beendet!" AT 2001.
       Z.  EXIT.
      ************************************ Konvertierung Lieferscheine *
       LFS-CV SECTION.
       A.  MOVE "PANELFS.IDX" TO WN-PRFDAT.
           OPEN INPUT PRUEFDAT.
           READ PRUEFDAT NEXT AT END GO Z.
           CLOSE PRUEFDAT.
           IF PRF-SATZ(57:2) = x"0047" GO Z.
           CALL "CBL_COPY_FILE" USING "PANELFS.DAT " "OLDLFS.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING "PANELFS.IDX " "OLDLFS.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
       I.  OPEN INPUT OLFSCHEIN.
           IF WF-STATUS = "35" GO Z.
           OPEN INPUT ARTIKEL.
           DELETE FILE LFSCHEIN.
           OPEN I-O LFSCHEIN.
           MOVE LOW-VALUES TO OLF-SATZ.
           START OLFSCHEIN KEY NOT < OLF-KEY INVALID GO M.
       K.  READ OLFSCHEIN NEXT IGNORE LOCK AT END GO M.
           MOVE OLF-SATZ TO LF-SATZ.
           MOVE LF-ARNUM TO AR-NUM.
           READ ARTIKEL IGNORE LOCK.
           MOVE AR-SGRP TO LF-SGRP.
           MOVE SPACE TO LF-ARBEZ.
      *    MOVE AR-BEZ TO LF-ARBEZ.      *> nicht bei K”nig / Altenburg.
           WRITE LF-SATZ INVALID GO K.
           DISPLAY LF-NUM AT 1901 " " AR-BEZ(1:30).
           GO K.
       M.  CLOSE OLFSCHEIN.
           DELETE FILE OLFSCHEIN.
           CLOSE LFSCHEIN.
           GO X.
       X.  DISPLAY "Konvertierung beendet!" AT 2001.
       Z.  EXIT.
      ************************************ Konvertierung Fakturendatei *
       FAK-CV SECTION.
       A.  MOVE "PANEFAKT.IDX" TO WN-PRFDAT.
           OPEN INPUT PRUEFDAT.
           READ PRUEFDAT NEXT AT END GO Z.
           CLOSE PRUEFDAT.
           IF PRF-SATZ(57:2) = x"0384" GO Z.
           CALL "CBL_COPY_FILE" USING "PANEFAKT.DAT " "OLDFAKT.DAT ".
           IF RETURN-CODE not = 0 STOP RUN.
           CALL "CBL_COPY_FILE" USING "PANEFAKT.IDX " "OLDFAKT.IDX ".
           IF RETURN-CODE not = 0 STOP RUN.
       I.  OPEN INPUT OFAKTDAT.
           IF WF-STATUS = "35" GO Z.
           OPEN INPUT ARTIKEL.
           DELETE FILE FAKTDAT.
           OPEN I-O FAKTDAT.
           MOVE LOW-VALUES TO OFA-SATZ.
           START OFAKTDAT KEY NOT < OFA-KEY INVALID GO M.
       K.  READ OFAKTDAT NEXT IGNORE LOCK AT END GO M.
           INITIALIZE FA-SATZ.
           MOVE OFA-ARNUM TO AR-NUM.
           READ ARTIKEL IGNORE LOCK.
           MOVE AR-SGRP TO FA-SGRP.
           MOVE AR-BEZ TO FA-ARBEZ.
           MOVE OFA-MON TO FA-MON.
           MOVE OFA-ART TO FA-ART.
           MOVE OFA-KTONR TO FA-KTONR.
           MOVE OFA-ARNUM TO FA-ARNUM.
           MOVE OFA-MR TO FA-MR.
           MOVE OFA-XTAB TO FA-XTAB.
           MOVE OFA-KUARNUM TO FA-KUARNUM.
           MOVE OFA-RAST(1) TO FA-RAST(1).
           MOVE OFA-RAST(2) TO FA-RAST(2).
           MOVE OFA-RAST(3) TO FA-RAST(3).
           WRITE FA-SATZ INVALID GO K.
           DISPLAY FA-KTONR AT 1901 " " FA-ARBEZ(1:30).
           GO K.
       M.  CLOSE OFAKTDAT.
           DELETE FILE OFAKTDAT.
           CLOSE FAKTDAT.
           GO X.
       X.  DISPLAY "Konvertierung beendet!" AT 2001.
       Z.  EXIT.
