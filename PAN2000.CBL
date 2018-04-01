      $SET LINKCOUNT"450" ANS85 BOUND NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PAN2000.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ODEBITOR   ASSIGN TO "\BUGSI\PANDEBIT.DAT"
                             ORGANIZATION INDEXED  ACCESS DYNAMIC
                             RECORD KEY ODE-KEY
                             ALTERNATE RECORD KEY ODE-MCODE DUPLICATES
                             ALTERNATE RECORD KEY ODE-RKEY DUPLICATES
                             ALTERNATE RECORD KEY ODE-IKEY =
                                       ODE-TOUR, ODE-SUB DUPLICATES
                             ALTERNATE RECORD KEY ODE-GKEY
                             FILE STATUS WF-STATUS.
           SELECT OKONSTANT  ASSIGN TO "\BUGSI\PANEKONS.DAT"
                             ORGANIZATION RELATIVE, ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS WF-STATUS.
           SELECT OARTIKEL   ASSIGN TO "\BUGSI\PANARTIK.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OAR-KEY
                             ALTERNATE RECORD KEY OAR-AKEY DUPLICATES
                             ALTERNATE RECORD KEY OAR-BKEY DUPLICATES
                             ALTERNATE RECORD KEY OAR-CKEY DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT OFAKTDAT   ASSIGN TO "\BUGSI\PANEFAKT.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OFA-KEY
                             ALTERNATE RECORD KEY OFA-AKEY
                             ALTERNATE RECORD OFA-BKEY =
                                     OFA-KTONR OFA-KUARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT OLFSCHEIN  ASSIGN TO "\BUGSI\PANELFS.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OLF-KEY
                             ALTERNATE RECORD KEY OLF-AKEY DUPLICATES
                             ALTERNATE RECORD KEY OLF-ARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT ODAUER     ASSIGN TO "\BUGSI\PANEBEST.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ODA-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT OSTATISTIK ASSIGN TO "\BUGSI\PANESTAT.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY OST-KEY
                             FILE STATUS IS WF-STATUS.
      *-----------------------------------------------> neuen Dateien <-
       COPY PANSEDEB.CPY.
       COPY PANSEKON.CPY.
       COPY PANSEART.CPY.
       COPY PANSEFAK.CPY.
       COPY PANSELFS.CPY.
       COPY PANSEDAU.CPY.
       COPY PANSESTA.CPY.
       DATA DIVISION.
       FILE SECTION.
      *****************************************************************
       FD  ODEBITOR      external       LABEL RECORDS STANDARD.
       01  ODE-SATZ.
           03 ODE-GKEY.
               05 ODE-FNR               PIC 999       COMP-3.
               05 ODE-KEY.
                  07 ODE-KTONR          PIC 9(6)      COMP-3.
           03 ODE-MCODE                PIC X(8).
           03 ODE-KTOART               PIC 9.
           03 ODE-SFTKOND              PIC 99        COMP.
           03 ODE-ANLMERK              PIC 99        COMP.
           03 ODE-ANMERK               PIC X(60).
           03 ODE-KOND                 PIC 9.
           03 ODE-BEZ                  PIC X(110).
           03 ODE-RET                  PIC 999V9     COMP.
           03 ODE-PRDAT                PIC 9(6)      COMP.
           03 ODE-SPERR                PIC 9(6)      COMP.
           03 ODE-REKTO                PIC 9(6)      COMP-3.
           03 ODE-RAST                 PIC 9.
           03 ODE-RAB                  PIC S99V9     COMP-3.
           03 ODE-EAN                  PIC 9.
           03 ODE-RKEY.
               05 ODE-FAKART           PIC 99.
               05 ODE-TOUR             PIC 99.
               05 ODE-SUB              PIC 9999      COMP.
      *---------------------------> neue Eintragung fÅr Fakturenliste <-
           03 ODE-INKASSO              PIC 99        COMP.
           03 ODE-RENR                 PIC 9(6)      COMP.
           03 ODE-REDAT                PIC 9(6)      COMP.
           03 ODE-REBET                PIC S9(6)V99  COMP.
           03 ODE-ANR                  PIC 99        COMP.
           03 ODE-STATIS               PIC 99        COMP.
           03 ODE-BONUS                PIC 99V99     COMP.
           03 ODE-RBASIS               PIC 99V99     COMP.
      *******************************************  Konstanten - Datei *
       FD  OKONSTANT      external     LABEL RECORDS STANDARD
                                       RECORD 128 CHARACTERS.
      *--------------------------------------------------> WH-Key = 1 <-
       01  OKO-SATZ.
           03  OKO-NUM                  PIC 999     COMP-3.
           03  OKO-MON                  PIC 9(6)    COMP.
           03  OKO-FIRMA                PIC X(22).
           03  OKO-UST                  PIC 99V99   COMP-3  OCCURS 6.
           03  OKO-RENUM                PIC 9(6)    COMP.
           03  OKO-SPERRE               PIC X(6).
           03  OKO-ORT                  PIC X(15).
           03  OKO-EANNR                PIC 9(8)    COMP.
           03  OKO-MERK                 PIC XXXX.
           03  OKO-TX                   PIC X(51).
      *---------------------------------------------> Meh WH-KEY = 10 <-
       01  OKO-MSATZ.
           03  FILLER                  PIC X(21).
           03  OKO-DRU                  PIC 99       COMP.
           03  OKO-WJAHR                PIC 9(4)     COMP.
           03  OKO-MART                 PIC 9999     COMP  OCCURS 22.
           03  OKO-TM.
               05 OKO-MEH               PIC XXX      OCCURS 20.
      *-----------------------------> Firmenkopf am Briefpapier 2 - 5 <-
       01  OKO-KSATZ.
           03  FILLER                  PIC X(26).
           03  OKO-KOPF                 PIC X(102).
      *-------------------------------------------> Summen WH-Key = 6 <-
       01  OKO-SSATZ.
           03  FILLER                  PIC XX.
           03  OKO-FAKT                 PIC S999V99  COMP-3  OCCURS 4.
           03  OKO-PROZ                 PIC S999V99  COMP-3  OCCURS 16.
           03  FILLER                  PIC X(5).
           03  OKO-RETDAT               PIC 9(6)     COMP.
           03  OKO-RET                  PIC 999V9    COMP.
           03  OKO-ERST                 PIC 9(6)     COMP.
           03  OKO-DATUM                PIC 9(6)     COMP.
           03  OKO-ERLOES               PIC S9(7)V99 COMP-3 OCCURS 10.
      *-----------------------------------> Tourenzeitplan WH-KEY = 9 <-
       01  OKO-ZSATZ.
           03  OKO-TKO                 PIC X(8).
           03  OKO-TOURTAB                     OCCURS 10.
               05 OKO-TZEIT             PIC 9(4)      COMP.
               05 OKO-TNR               PIC 99        COMP OCCURS 10.
      *-----------------------------> Tourenbezeichnungen WH-KEY + 10 <-
       01  OKO-TSATZ.
           03  FILLER                  PIC X(45).
           03  OKO-ABRDAT               PIC 9(6)      COMP.
           03  OKO-SBEZ                 PIC X(24).
           03  OKO-STATNR               PIC X(12).
           03  OKO-SUM                  PIC S9(5)V99 COMP-3 OCCURS 3.
           03  OKO-KTO                  PIC 9(6)     COMP.
           03  OKO-REST                 PIC S9(5)V99 COMP-3.
           03  OKO-TOUR                 PIC X(25).
      *----------------------------> Netzdruckersteuerung WH-Key = 35 <-
       01  OKO-PSATZ.
           03  FILLER                  PIC X(38).
           03  OKO-REZPM                PIC X(45).
           03  OKO-PARM                 PIC 99      COMP.
           03  OKO-EURO                 PIC 9999V99999 COMP.
           03  OKO-PARAM   OCCURS 10.
               05 KOP-DRU              PIC 99      COMP.
               05 KOP-KOPF             PIC 99      COMP.
               05 KOP-WO               PIC 99      COMP.
               05 KOP-TYP              PIC 99      COMP.
      *----------------------------> Erlîskontenzuordnung WH-KEY = 36 <-
       01  OKO-ESATZ.
           03  FILLER                  PIC XX.
           03  OKO-VERZ                 PIC X(36).
           03  OKO-KTONR                PIC 9(6)     COMP    OCCURS 21.
           03  OKO-MODEM                PIC X(27).
      *--------------------------------> Teiggruppen WH-KEY = 37 - 39 <-
       01  OKO-VSATZ.
           03  OKO-VKZ                  PIC X.
           03  FILLER                  PIC X(27).
           03  OKO-TGRP                 PIC 99        COMP-X OCCURS 100.
      *--------------------------------> Artikel f. autom. Backzettel <-
       01  OKO-QSATZ.
           03  FILLER                  PIC X(22).
           03  OKO-TB                                OCCURS 22.
               05 OKO-BART              PIC 9999     COMP.
               05 OKO-BPRZ              PIC 9999     COMP.
      *---------------------> Sammelrechnungssaetze BILLA WH-KEY + 45 <-
       01  OKO-RSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  OKO-REKTO                PIC 9(6)     COMP.
           03  OKO-KONTO                PIC 9(6)     COMP.
           03  OKO-FIL                  PIC X(37).
           03  OKO-DAT                  PIC 9(6)     COMP   OCCURS 3.
           03  FILLER                  PIC X(74).
      ********************************************* Dauerbestellungen *
       FD  OFAKTDAT      external       LABEL RECORD STANDARD
                                        RECORD 320 CHARACTERS.
       01  OFA-SATZ.
           03  OFA-AKEY.
               05 OFA-MON               PIC 9999       COMP.
               05 OFA-ART               PIC 99         COMP.
               05 OFA-KEY.
                  07 OFA-KTONR          PIC 9(6)       COMP.
                  07 OFA-ARNUM          PIC 9(4)       COMP.
           03  OFA-AR                        OCCURS 40 INDEXED OX.
               05 OFA-MENGE             PIC S9(5)      COMP-3.
               05 OFA-RETOUR            PIC S9(5)      COMP-3.
           03  OFA-XTAB.
               05 OFA-TAB                       OCCURS 4  INDEXED PX.
                  07 OFA-PREIS          PIC S9(4)V999  COMP-3.
                  07 OFA-AB             PIC 9(6)       COMP.
                  07 OFA-BIS            PIC 9(6)       COMP.
                  07 OFA-RET            PIC S9(4)V999  COMP-3.
                  07 OFA-RAB            PIC S99V9      COMP-3.
           03  OFA-KUARNUM              PIC 9(14)      COMP.
           03  FILLER                  PIC X.
      ************************************************ Verkaufsartikel *
       FD  OARTIKEL      external       LABEL RECORD STANDARD.
       01  OAR-SATZ.
           03  OAR-KEY.
               05 OAR-NUM               PIC 9(4)       COMP.
           03  OAR-AKEY.
               05 OAR-SA                PIC X.
               05 OAR-ABEZ              PIC X(10).
           03  OAR-BEZ                  PIC X(25).
           03  OAR-KTO                  PIC 9(8)       COMP.
           03  OAR-UST                  PIC 99         COMP.
           03  OAR-PREIS                PIC S9(4)V999  COMP-3.
           03  OAR-VKP                  PIC S9(5)V99   COMP-3.
           03  OAR-EKP                  PIC S9(4)V999  COMP-3.
           03  OAR-ANZ                  PIC S9(5)      COMP-3.
           03  OAR-GRM.
               05 OAR-GRP               PIC 99         COMP-X.
               05 OAR-SON               PIC 99         COMP.
               05 OAR-MEH               PIC 99         COMP.
               05 OAR-NK                PIC 99         COMP.
               05 OAR-TB                PIC 99         COMP.
           03  OAR-PL                   PIC 9.
           03  OAR-APREIS               PIC S9(4)V999  COMP-3.
           03  OAR-ADAT                 PIC 9(6)       COMP.
           03  OAR-AVKP                 PIC S9(5)V99   COMP-3.
           03  OAR-BKEY.
               05 OAR-GVZ               PIC 99         COMP.
           03  OAR-RAB                  PIC 99         COMP.
           03  OAR-CKEY.
               05 OAR-OEPZ              PIC 99         COMP.
           03  FILLER                  PIC XX.
      ********************************************* Dauerbestellungen *
       FD  ODAUER        external       LABEL RECORD STANDARD.
       01  ODA-SATZ.
           03  ODA-KEY.
               05 ODA-KTONR             PIC 9(6)       COMP.
               05 ODA-ARNUM             PIC 9(4)       COMP.
           03  ODA-BEZ                  PIC X(25).
           03  ODA-ANZ                  PIC S9(5)      COMP-3 OCCURS 27.
           03  ODA-DAT                  PIC 9(6)       COMP   OCCURS 3.
           03  ODA-GRM.
               05 ODA-GRP               PIC 99         COMP-X.
               05 ODA-SON               PIC 99         COMP.
               05 ODA-MEH               PIC 99         COMP.
               05 ODA-NK                PIC 99         COMP.
               05 ODA-TB                PIC 99         COMP.
           03  ODA-FART                 PIC 99         COMP.
           03  ODA-TOUR                 PIC 99         COMP.
           03  ODA-SUB                  PIC 9999       COMP.
           03  ODA-EAN                  PIC 99         COMP.
      ********************************************** Artikelstatistik *
       FD  OSTATISTIK   external      LABEL RECORD STANDARD
                                      RECORD 224 CHARACTERS.
       01  OST-SATZ.
           03  OST-KEY.
               05 OST-KTONR             PIC 9(8)       COMP.
               05 OST-ARNUM             PIC 9(4)       COMP.
               05 OST-JAHR              PIC 99         COMP.
           03  OST-SUM                          OCCURS 12.
               05 OST-MENGEN            PIC S9(7)      COMP-3.
               05 OST-RET               PIC S9(7)      COMP-3.
               05 OST-UMSATZ            PIC S9(8)V9    COMP-3.
               05 OST-GEWINN            PIC S9(8)V9    COMP-3.
           03  FILLER                  PIC X.
      *****************************************************************
       FD  OLFSCHEIN     external       LABEL RECORD STANDARD
                                       RECORD 64 CHARACTERS.
       01  OLF-SATZ.
           03  OLF-AKEY.
               05 OLF-DATUM             PIC 9(6)       COMP.
               05 OLF-TOUR              PIC 99         COMP.
               05 OLF-SUB               PIC 9999       COMP.
               05 OLF-KEY.
                  07 OLF-KTONR          PIC 9(6)       COMP.
                  07 OLF-NUM            PIC 9(6)       COMP.
                  07 OLF-ARNUM.
                     09 OLF-ARNUM       PIC 9(4)       COMP.
           03  OLF-MENGE                PIC S9(5)      COMP-3 OCCURS 3.
           03  OLF-PREIS                PIC S9(4)V999  COMP-3.
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
           03  FILLER                  PIC X(4).
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
       COPY PANSTAT.CPY.
       COPY PANLIEF.CPY.
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
           MOVE 9 TO WH-HW.
           CALL "CAUP" USING "1301012580" WH-CREG.
           perform CV-DEB
           perform CV-KON.
           perform CV-FAK.
           perform CV-ART.
           perform CV-DAU.
           perform CV-STA.
           perform CV-LFS.
           MOVE WH-CREG TO WL-CREG.
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       CV-DEB SECTION.
           open input DEBITOR.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Kunden........:" with FOREGROUND-COLOR 10 AT 0515.
           OPEN OUTPUT debitor.
           OPEN INPUT odebitor.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO ODE-SATZ.
           READ ODEBITOR NEXT AT END GO X.
           INITIALIZE DE-SATZ.
           MOVE ODE-FNR TO DE-FNR.
           MOVE ODE-KTONR TO DE-KTONR.
           MOVE ODE-MCODE TO DE-MCODE.
           MOVE ODE-KTOART TO DE-KTOART.
           MOVE ODE-SFTKOND TO DE-SFTKOND.
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
           MOVE ODE-FAKART TO DE-FAKART.
           MOVE ODE-TOUR TO DE-TOUR.
           MOVE ODE-SUB TO DE-SUB.
           MOVE ODE-INKASSO TO DE-INKASSO.
           MOVE ODE-RENR TO DE-RENR.
           MOVE ODE-REDAT TO DE-REDAT.
           MOVE ODE-REBET TO DE-REBET.
           MOVE ODE-ANR TO DE-ANR.
           MOVE ODE-STATIS TO DE-STATIS.
           MOVE ODE-BONUS TO DE-BONUS.
           MOVE ODE-RBASIS TO DE-RBASIS.
           WRITE DE-SATZ INVALID STOP RUN.
           DISPLAY DE-KTONR AT 0543 " " DE-BEZ(1:30).
           GO C.
       X.  CLOSE ODEBITOR.
       Y.  CLOSE DEBITOR.
       Z.  EXIT.
      ************************************************ Konstantendatei *
       CV-KON SECTION.
       A.  open input KONSTANT.
           MOVE 1 TO WH-KEY.
           READ KONSTANT.
           IF KO-MON > 999999 and ko-num not = 31 GO Y.
           CLOSE KONSTANT.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Konstantendatei: " with FOREGROUND-COLOR 14 AT 0615.
           OPEN OUTPUT KONSTANT.
           OPEN INPUT OKONSTANT.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUE TO KO-SATZ.
           READ OKONSTANT NEXT AT END GO X.
           MOVE OKO-SATZ TO KO-SATZ.
           EVALUATE WH-KEY
               WHEN 1 MOVE OKO-NUM TO KO-NUM
                      MOVE OKO-MON TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-MON
                      MOVE OKO-FIRMA TO KO-FIRMA
                      PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 6
                          MOVE OKO-UST(WI) TO KO-UST(WI)
                      end-perform
                      MOVE OKO-RENUM TO KO-RENUM
                      MOVE OKO-SPERRE TO KO-SPERRE
                      MOVE OKO-ORT TO KO-ORT
                      MOVE OKO-EANNR TO KO-EANNR
                      MOVE OKO-MERK TO KO-MERK
                      MOVE OKO-TX TO KO-TX
               WHEN 2
               WHEN 3
               WHEN 4
               WHEN 5 INITIALIZE KO-KSATZ
                      MOVE OKO-KOPF TO KO-KOPF
               WHEN 6 INITIALIZE KO-SSATZ
                      PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 16
                          IF WI < 5; IF OKO-FAKT(WI)(1:) = SPACE
                                        MOVE 0 TO OKO-FAKT(WI)
                                     end-if
                              MOVE OKO-FAKT(WI) TO KO-FAKT(WI)
                          end-if
                          IF OKO-PROZ(WI)(1:) = SPACE
                              MOVE 0 TO OKO-PROZ(WI)
                          end-if
                          MOVE OKO-PROZ(WI) TO KO-PROZ(WI)
                      end-perform
                      MOVE OKO-RETDAT TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-RETDAT
                      MOVE OKO-RET TO KO-RET
                      MOVE OKO-ERST TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-ERST
                      MOVE OKO-DATUM TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-DATUM
                      PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 10
                          MOVE OKO-ERLOES(WI) TO KO-ERLOES(WI)
                      end-perform
               WHEN 7
               WHEN 8 GO C
               WHEN 9 INITIALIZE KO-ZSATZ
                      MOVE OKO-TKO TO KO-TKO
                      PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 10
                          MOVE OKO-TZEIT(WI) TO KO-TZEIT(WI)
                      PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
                          MOVE OKO-TNR(WI, WX) TO KO-TNR(WI, WX)
                      end-perform
               WHEN 10 INITIALIZE KO-MSATZ
                       MOVE OKO-DRU TO KO-DRU
                       MOVE OKO-WJAHR TO KO-WJAHR
                       PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 20
                           MOVE OKO-MART(WI) TO KO-MART(WI)
                       end-perform
                       MOVE OKO-TM TO KO-TM
               WHEN 11 THRU 31 INITIALIZE KO-TSATZ
                       MOVE OKO-ABRDAT TO WC-DATUM
                       CALL "CAUP" USING "04DATDREH" WH-CREG
                       MOVE WX-DATUM TO KO-ABRDAT
                       MOVE OKO-SBEZ TO KO-SBEZ
                       MOVE OKO-STATNR TO KO-STATNR
                       MOVE OKO-SUM(1) TO KO-SUM(1)
                       MOVE OKO-SUM(2) TO KO-SUM(2)
                       MOVE OKO-SUM(3) TO KO-SUM(3)
                       MOVE OKO-KTO TO KO-KTO
                       MOVE OKO-REST TO KO-REST
                       MOVE OKO-TOUR TO KO-TOUR
               WHEN 35 INITIALIZE KO-PSATZ
                       MOVE OKO-REZPM TO KO-REZPM
                       MOVE OKO-PARM TO KO-PARM
                       MOVE OKO-EURO TO KO-EURO
                       PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 10
                           MOVE OKO-PARAM(WI) TO KO-PARAM(WI)
                       end-perform
               WHEN 36 INITIALIZE KO-ESATZ
                       MOVE OKO-VERZ TO KO-VERZ
                       PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 21
                           MOVE OKO-KTONR(WI) TO KO-KTONR(WI)
                       end-perform
                       MOVE OKO-MODEM TO KO-MODEM
               WHEN 37 THRU 39 INITIALIZE KO-VSATZ
                       MOVE OKO-VKZ TO KO-VKZ
                       PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 100
                           MOVE OKO-TGRP(WI) TO KO-TGRP(WI)
                       end-perform
               WHEN 40 INITIALIZE KO-QSATZ
                       PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 22
                           MOVE OKO-BART(WI) TO KO-BART(WI)
                           MOVE OKO-BPRZ(WI) TO KO-BPRZ(WI)
                       end-perform
               WHEN 45 THRU 60 INITIALIZE KO-RSATZ.
           WRITE KO-SATZ.
           ADD 1 TO WH-ANZ.
           DISPLAY WH-ANZ AT 0943.
           GO C.
       X.  CLOSE OKONSTANT.
       Y.  CLOSE KONSTANT.
       Z.  EXIT.
      ******************************************************************
       CV-FAK SECTION.
           open input FAKTDAT.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Preise........:" with FOREGROUND-COLOR 10 AT 0715.
           OPEN OUTPUT faktdat.
           OPEN INPUT ofaktdat.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OFA-SATZ.
           READ OFAKTDAT NEXT AT END GO X.
           INITIALIZE FA-SATZ.
           MOVE OFA-MON TO FA-MON.
           MOVE OFA-ART TO FA-ART.
           MOVE OFA-KTONR TO FA-KTONR.
           MOVE OFA-ARNUM TO FA-ARNUM.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 40
               MOVE OFA-MENGE(WI) TO FA-MENGE(WI)
               MOVE OFA-RETOUR(WI) TO FA-RETOUR(WI).
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 4
               MOVE OFA-PREIS(WI) TO FA-PREIS(WI)
               MOVE OFA-AB(WI) TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE WX-DATUM TO FA-AB(WI)
               MOVE OFA-BIS(WI) TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE WX-DATUM TO FA-BIS(WI)
               MOVE OFA-RET(WI) TO FA-RET(WI)
               MOVE OFA-RAB(WI) TO FA-RAB(WI).
           MOVE OFA-KUARNUM TO FA-KUARNUM.
           WRITE FA-SATZ INVALID STOP RUN.
           DISPLAY FA-KTONR AT 0743.
           GO C.
       X.  CLOSE OFAKTDAT.
       Y.  CLOSE FAKTDAT.
       Z.  EXIT.
      ******************************************************************
       CV-ART SECTION.
           open input ARTIKEL.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Artikel.......:" with FOREGROUND-COLOR 10 AT 0815.
           OPEN OUTPUT ARTIKEL.
           OPEN INPUT OARTIKEL.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OAR-SATZ.
           READ OARTIKEL NEXT AT END GO X.
           INITIALIZE AR-SATZ.
           MOVE OAR-NUM TO AR-NUM.
           MOVE OAR-AKEY TO AR-AKEY.
           MOVE OAR-BEZ TO AR-BEZ.
           MOVE OAR-KTO TO AR-KTO.
           MOVE OAR-UST TO AR-UST.
           MOVE OAR-PREIS TO AR-PREIS.
           MOVE OAR-VKP TO AR-VKP.
           MOVE OAR-EKP TO AR-EKP.
           MOVE OAR-ANZ TO AR-ANZ.
           MOVE OAR-GRP TO AR-GRP.
           MOVE OAR-SON TO AR-SON.
           MOVE OAR-MEH TO AR-MEH.
           MOVE OAR-NK TO AR-NK.
           MOVE OAR-TB TO AR-TB.
           MOVE OAR-PL TO AR-PL.
           MOVE OAR-APREIS TO AR-APREIS.
           MOVE OAR-ADAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE WX-DATUM TO AR-ADAT.
           MOVE OAR-AVKP TO AR-AVKP.
           MOVE OAR-GVZ TO AR-GVZ.
           MOVE OAR-RAB TO AR-RAB.
           MOVE OAR-OEPZ TO AR-OEPZ.
           WRITE AR-SATZ INVALID STOP RUN.
           DISPLAY AR-NUM AT 0843.
           GO C.
       X.  CLOSE OARTIKEL.
       Y.  CLOSE ARTIKEL.
       Z.  EXIT.
      ******************************************************************
       CV-DAU SECTION.
           open input DAUER.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Dauerbestellg.:" with FOREGROUND-COLOR 10 AT 0915.
           OPEN OUTPUT DAUER.
           OPEN INPUT ODAUER.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OAR-SATZ.
           READ ODAUER NEXT AT END GO X.
           INITIALIZE DA-SATZ.
           MOVE ODA-KTONR TO DA-KTONR.
           MOVE ODA-ARNUM TO DA-ARNUM.
           MOVE ODA-BEZ TO DA-BEZ.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 27
               MOVE ODA-ANZ(WI) TO DA-ANZ(WI).
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 3
               MOVE ODA-DAT(WI) TO WC-DATUM
               CALL "CAUP" USING "04DATDREH" WH-CREG
               MOVE WX-DATUM TO DA-DAT(WI).
           MOVE ODA-GRP TO DA-GRP.
           MOVE ODA-SON TO DA-SON.
           MOVE ODA-MEH TO DA-MEH.
           MOVE ODA-NK TO DA-NK.
           MOVE ODA-TB TO DA-TB.
           MOVE ODA-FART TO DA-FART.
           MOVE ODA-TOUR TO DA-TOUR.
           MOVE ODA-SUB TO DA-SUB.
           MOVE ODA-EAN TO ODA-EAN.
           WRITE DA-SATZ INVALID STOP RUN.
           DISPLAY DA-KTONR AT 0943.
           GO C.
       X.  CLOSE ODAUER.
       Y.  CLOSE DAUER.
       Z.  EXIT.
      ******************************************************************
       CV-STA SECTION.
           open input STATISTIK.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Statistik.....:" with FOREGROUND-COLOR 10 AT 1015.
           OPEN OUTPUT STATISTIK.
           OPEN INPUT OSTATISTIK.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OAR-SATZ.
           READ OSTATISTIK NEXT AT END GO X.
           INITIALIZE ST-SATZ.
           MOVE OST-KTONR TO ST-KTONR.
           MOVE OST-ARNUM TO ST-ARNUM.
           MOVE OST-JAHR TO ST-JAHR.
      *    ADD 1900 TO ST-JAHR.
           IF ST-JAHR < 98 GO C.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 12
               MOVE OST-MENGEN(WI) TO ST-MENGEN(WI)
               MOVE OST-RET(WI) TO ST-RET(WI)
               MOVE OST-UMSATZ(WI) TO ST-UMSATZ(WI)
               MOVE OST-GEWINN(WI) TO ST-GEWINN(WI).
           WRITE ST-SATZ INVALID STOP RUN.
           DISPLAY ST-JAHR AT 1043 " " ST-KTONR.
           GO C.
       X.  CLOSE OSTATISTIK.
       Y.  CLOSE STATISTIK.
       Z.  EXIT.
      ******************************************************************
       CV-LFS SECTION.
           open input LFSCHEIN.
           if wf-status = "00" go Y.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Lieferscheine.:" with FOREGROUND-COLOR 10 AT 1115.
           OPEN OUTPUT LFSCHEIN.
           OPEN INPUT OLFSCHEIN.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OAR-SATZ.
           READ OLFSCHEIN NEXT AT END GO X.
           INITIALIZE LF-SATZ.
           MOVE OLF-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE WX-DATUM TO LF-DATUM.
           MOVE OLF-TOUR TO LF-TOUR.
           MOVE OLF-SUB TO LF-SUB.
           MOVE OLF-KTONR TO LF-KTONR.
           MOVE OLF-NUM TO LF-NUM.
           MOVE OLF-ARNUM TO LF-ARNUM.
           MOVE OLF-MENGE(1) TO LF-MENGE(1).
           MOVE OLF-MENGE(2) TO LF-MENGE(2).
           MOVE OLF-MENGE(3) TO LF-MENGE(3).
           MOVE OLF-PREIS TO LF-PREIS.
           MOVE OLF-BEZ TO LF-BEZ.
           MOVE OLF-GRP TO LF-GRP.
           MOVE OLF-SON TO LF-SON.
           MOVE OLF-MEH TO LF-MEH.
           MOVE OLF-NK TO LF-NK.
           MOVE OLF-TB TO LF-TB.
           MOVE OLF-FART TO LF-FART.
           MOVE OLF-STAT TO LF-STAT.
           MOVE OLF-EAN TO LF-EAN.
           WRITE LF-SATZ INVALID STOP RUN.
           DISPLAY LF-KTONR AT 1143 " " LF-DATUM.
           GO C.
       X.  CLOSE OLFSCHEIN.
       Y.  CLOSE LFSCHEIN.
       Z.  EXIT.
