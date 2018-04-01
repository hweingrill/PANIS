      $SET LINKCOUNT"192" BOUND TRUNC NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        PACO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy pansedeb.cpy.
           SELECT OLDDEB     ASSIGN TO DISK "C:\BUGSI\DEBITORE.fii"
                             ORGANIZATION SEQUENTIAL,
                             FILE STATUS IS WF-STATUS.
           copy panseart.cpy.
           SELECT OLDART     ASSIGN TO DISK "C:\BUGSI\PANARTIK.DAT"
                             ORGANIZATION SEQUENTIAL,
                             FILE STATUS IS WF-STATUS.
           copy pansefak.cpy.
           SELECT OLDFAKT    ASSIGN TO DISK "C:\BUGSI\PANEFAKT.DAT"
                             ORGANIZATION SEQUENTIAL,
                             FILE STATUS IS WF-STATUS.
           SELECT LFSCHEIN   ASSIGN TO DISK "PANELFS.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY LF-KEY
                             ALTERNATE RECORD KEY LF-AKEY DUPLICATES
                             ALTERNATE RECORD KEY LF-ARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT DAUER      ASSIGN TO DISK "PANEBEST.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY DA-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT STATISTIK  ASSIGN TO DISK "PANESTAT.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY ST-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT OLDLFS     ASSIGN TO DISK "C:\BUGSI\PANELFS.DAT"
                             ORGANIZATION SEQUENTIAL,
                             FILE STATUS IS WF-STATUS.
           SELECT OLDDAU     ASSIGN TO DISK "C:\BUGSI\PANEBEST.DAT"
                             ORGANIZATION SEQUENTIAL,
                             FILE STATUS IS WF-STATUS.
           SELECT OLDSTAT    ASSIGN TO DISK "C:\BUGSI\PANESTAT.DAT"
                             ORGANIZATION SEQUENTIAL,
                             FILE STATUS IS WF-STATUS.
           SELECT KONSTANT   ASSIGN TO DISK "PANEKONS.DAT"
                             ORGANIZATION RELATIVE ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT OLDKONS    ASSIGN TO DISK "C:\BUGSI\PANEKONS.DAT"
                             FILE STATUS IS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANKONS.CPY.
      *******************************************  Konstanten - Datei *
       FD  OLDKONS                     LABEL RECORDS STANDARD
                                       RECORD 128 CHARACTERS.
      *------------------------------------------------> WH-Key = 1 <-
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
      *--------------------------------> Mengeneinheiten WH-KEY = 10 <-
       01  OKO-MSATZ.
           03  FILLER                   PIC X(68).
           03  OKO-TM.
               05 OKO-MEH               PIC XXX      OCCURS 20.
      *----------------------------> Firmenkopf am Briefpapier 2 - 5 <-
       01  OKO-KSATZ.
           03  FILLER                   PIC X(26).
           03  OKO-KOPF                 PIC X(102).
      *--------------------> Sammelrechnungssaetze BILLA WH-KEY + 30 <-
       01  OKO-RSATZ.
           03  FILLER                   PIC 999     COMP-3.
           03  OKO-REKTO                PIC 9(6)    COMP.
           03  OKO-KTONR                PIC 9(6)    COMP.
           03  OKO-FIL                  PIC X(37).
           03  OKO-DAT                  PIC 9(6)    COMP OCCURS 3.
           03  FILLER                   PIC X(74).
      *------------------------------------------> Summen WH-Key = 6 <-
       01  OKO-SSATZ.
           03  FILLER                   PIC X(72).
           03  OKO-ERST                 PIC 9(6)    COMP.
           03  OKO-DATUM                PIC 9(6)    COMP.
           03  OKO-ERLOES               PIC 9(7)V99 COMP-3  OCCURS 10.
      *----------------------------> Tourenbezeichnungen WH-KEY + 10 <-
       01  OKO-TSATZ.
           03  FILLER                   PIC X(96).
           03  OKO-KTO                  PIC 9(6)    COMP.
           03  OKO-REST                 PIC 9(5)V99 COMP-3.
           03  OKO-TOUR                 PIC X(25).
       COPY PANFDART.CPY.
      *****************************************************************
       FD  OLDART       external      LABEL RECORD STANDARD
                                      RECORD  80 CHARACTERS.
       01  OR-SATZ.
           03  OR-KEY.
               05 OR-NUM               PIC 9(4)       COMP.
           03  OR-AKEY.
               05 OR-SA                PIC X.
               05 OR-ABEZ              PIC X(10).
           03  OR-BEZ                  PIC X(25).
           03  OR-KTO                  PIC 9(8)       COMP.
           03  OR-UST                  PIC 99         COMP.
           03  OR-PREIS                PIC S9(4)V999  COMP-3.
           03  OR-VKP                  PIC S9(5)V99   COMP-3.
           03  OR-EKP                  PIC S9(4)V999  COMP-3.
           03  OR-ANZ                  PIC S9(5)      COMP-3.
           03  OR-GRM                  PIC X(5).
           03  OR-PL                   PIC 9.
           03  OR-APREIS               PIC S9(4)V999  COMP-3.
           03  OR-ADAT                 PIC 9(6)       COMP.
           03  OR-AVKP                 PIC S9(5)V99   COMP-3.
           03  FILLER                  PIC X(5).
       COPY PANDEBI.CPY.
      *****************************************************************
       FD  OLDDEB                      RECORD 256 CHARACTERS
                                       LABEL RECORDS STANDARD.
       01  OB-SATZ.
           03  OB-KEY.
               05 OB-FNR               PIC 999      COMP-3.
               05 OB-KTONR             PIC 9(6)     COMP-3.
           03  OB-MCODE                PIC X(8).
           03  OB-KTOART               PIC 9.
           03  ZEILE                   PIC 99.
           03  ZEILE-VJ                PIC 99.
           03  SEITE                   PIC 999      COMP-3.
           03  SEITE-VJ                PIC 999      COMP-3.
           03  T-BEW                OCCURS 3.
               05  BEW1                PIC 9(5)     COMP-3.
               05  BEWL                PIC 9(5)     COMP-3.
           03  OP1                     PIC 9(5)     COMP-3.
           03  OPL                     PIC 9(5)     COMP-3.
           03  OB-FWKZ                 PIC 9.
           03  OB-OPKZ                 PIC 9.
           03  UMTAB                   PIC 9(9)V99 OCCURS 10 COMP-3.
           03  OB-KOND                 PIC 9(1).
           03  OB-MAHNT                PIC 999      COMP-3.
           03  OB-MAHND                PIC 9(6)     COMP-3.
           03  OB-BEZ                  PIC X(110).
           03  FILLER                  PIC 9(11).
           03  OB-PRDAT                PIC 9(6)     COMP.
           03  OB-SPERR                PIC 9(6)     COMP.
           03  OB-KETTD                PIC 9(5)     COMP-3.
           03  OB-REKTO                PIC 9(6)     COMP-3.
           03  OB-RAST                 PIC 9.
           03  OB-RAB                  PIC 99V9     COMP-3.
           03  OB-EAN                  PIC 9.
           03  OB-SUB                  PIC 9999     COMP.
       COPY PANFAKT.CPY.
      ********************************************* Dauerbestellungen *
       FD  OLDFAKT      external       LABEL RECORD STANDARD
                                       RECORD 320 CHARACTERS.
       01  OF-SATZ.
           03  OF-AKEY.
               05 OF-MON               PIC 9999       COMP.
               05 OF-ART               PIC 99         COMP.
               05 OF-KEY.
                  07 OF-KTONR          PIC 9(6)       COMP.
                  07 OF-ARNUM          PIC 9(4)       COMP.
           03  OF-AR                        OCCURS 40 INDEXED OX.
               05 OF-MENGE             PIC S9(5)      COMP-3.
               05 OF-RETOUR            PIC S9(5)      COMP-3.
           03  OF-XTAB.
               05 OF-TAB                       OCCURS 4  INDEXED PX.
                  07 OF-PREIS          PIC S9(4)V999  COMP-3.
                  07 OF-AB             PIC 9(6)       COMP.
                  07 OF-BIS            PIC 9(6)       COMP.
                  07 OF-RET            PIC S9(4)V999  COMP-3.
                  07 OF-RAB            PIC S99V9      COMP-3.
           03  OF-KUARNUM              PIC x(7).
           03  FILLER                  PIC X.
       COPY PANLIEF.CPY.
      *****************************************************************
       FD  OLDLFS     external       LABEL RECORD STANDARD
                                       RECORD 64 CHARACTERS.
       01  OL-SATZ.
           03  OL-AKEY.
               05 OL-DATUM             PIC 9(6)       COMP.
               05 OL-TOUR              PIC 99         COMP.
               05 OL-SUB               PIC 9999       COMP.
               05 OL-KEY.
                  07 OL-KTONR          PIC 9(6)       COMP.
                  07 OL-NUM            PIC 9(6)       COMP.
                  07 OL-BKEY.
                     09 OL-ARNUM       PIC 9(4)       COMP.
           03  OL-MENGE                PIC S9(5)      COMP-3 OCCURS 3.
           03  OL-PREIS                PIC S9(4)V999  COMP-3.
           03  OL-BEZ                  PIC X(25).
           03  OL-GRM                  PIC X(5).
           03  OL-FART                 PIC 99         COMP.
           03  OL-STAT                 PIC 99         COMP.
           03  OL-EAN                  PIC 99         COMP.
           03  FILLER                  PIC X(4).
       COPY PANDAU.CPY.
      ********************************************* Dauerbestellungen *
       FD  OLDDAU       external       LABEL RECORD STANDARD
                                       RECORD 128 CHARACTERS.
       01  OD-SATZ.
           03  OD-KEY.
               05 OD-KTONR             PIC 9(6)       COMP.
               05 OD-ARNUM             PIC 9(4)       COMP.
           03  OD-BEZ                  PIC X(25).
           03  OD-ANZ                  PIC S9(5)      COMP-3 OCCURS 18.
           03  OD-DAT                  PIC 9(6)       COMP   OCCURS 3.
           03  OD-GRM                  PIC X(5).
           03  OD-FART                 PIC 99         COMP.
           03  OD-TOUR                 PIC 99         COMP.
           03  OD-SUB                  PIC 9999       COMP.
           03  OD-EAN                  PIC 99         COMP.
           03  FILLER                  PIC X(25).
       COPY "PANSTAT.CPY".
      ********************************************** Artikelstatistik *
       FD  OLDSTAT      external      LABEL RECORD STANDARD
                                      RECORD 224 CHARACTERS.
       01  OS-SATZ.
           03  OS-KEY.
               05 OS-KTONR             PIC 9(8)       COMP.
               05 OS-ARNUM             PIC 9(4)       COMP.
               05 OS-JAHR              PIC 99         COMP.
           03  OS-SUM                          OCCURS 12.
               05 OS-MENGEN            PIC S9(7)      COMP-3.
               05 OS-RET               PIC S9(7)      COMP-3.
               05 OS-UMSATZ            PIC S9(8)V9    COMP-3.
               05 OS-GEWINN            PIC S9(8)V9    COMP-3.
           03  FILLER                  PIC X.
      *****************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WH-F                    PIC X.
           03  wn-num                  pic 9(14)   comp.
           03  ws                      PIC 99      COMP.
           03  WI                      PIC 99      COMP.
           03  wc                      PIC 999     COMP.
           03  wh-key                  pic 9(5).
           03  wh-cv.
               05 wh-dk                pic 99   comp-x  occurs 7.
      *---------------------------------> -GRP, -RAB, -MEH, -NK, -TB <-
           03  WF-GRM.
               05 WF-GRP               PIC 99         COMP-X.
               05 WF-SON               PIC 99         COMP.
               05 WF-MEH               PIC 99         COMP.
               05 WF-NK                PIC 99         COMP.
               05 WF-TB                PIC 99         COMP.
           03  WK-GRM.
               05 WK-GRP               PIC 99         COMP.
               05 WK-SON               PIC 99         COMP.
               05 WK-MEH               PIC 99         COMP.
               05 WK-NK                PIC 99         COMP.
               05 WK-TB                PIC 99         COMP.
           03  WH-KETTD                PIC 9(5)    VALUE ZERO.
           03  RED REDEFINES WH-KETTD.
               05  WH-ANREDE           PIC 9.
               05  WH-STATIS           PIC 9.
               05  WH-FAKART           PIC 9.
               05  WH-TOUR             PIC 99.
           03  WT-X.
               05 wx-x                 pic 99      comp-x
                                               occurs 130 indexed ax ay.
           03  WT-ACON                 PIC X(128)     VALUE "
      -    "                     !""#$%&'()*+,-./0123456789:;<=>?@ABCDEF
      -    "GHIJKLMNOPQRSTUVWXYZéôö^_`abcdefghijklmnopqrstuvwxyzÑîÅ·".
           03  wr-cv redefines wt-acon pic 99        comp-x  occurs 128.
           03  iy                      pic 999       comp-x.
       01  WX-FEHL.
           03  WD-FA                   PIC X(8)     VALUE "Fehler: ".
           03  WD-SB                   PIC ZZ9.
           03  WD-C                    PIC XXX      VALUE " / ".
           03  DATEI                   PIC X(12)    VALUE SPACE.
           03  WD-D                    PIC X(39)
                    VALUE " - Programmabbruch - weiter mit <ret>: ".
           03  WD-X                    PIC X.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       DECLARATIVES.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON FAKTDAT.
       A.  MOVE "PANEFAKT.DAT" TO DATEI PERFORM DECL-X.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON LFSCHEIN.
       A.  MOVE "PANELFS.DAT"  TO DATEI PERFORM DECL-X.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON ARTIKEL.
       A.  MOVE "PANARTIK.DAT" TO DATEI PERFORM DECL-X.
       DECL-D SECTION.         USE AFTER ERROR PROCEDURE ON DEBITOR.
       A.  MOVE "DEBITORE.FII" TO DATEI PERFORM DECL-X.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  MOVE "PANEKONS.DAT" TO DATEI PERFORM DECL-X.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON STATISTIK.
       A.  MOVE "PANESTAT.DAT" TO DATEI PERFORM DECL-X.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON DAUER.
       A.  MOVE "PANEBEST.DAT" TO DATEI PERFORM DECL-X.
       DECL-X SECTION.
       A.  IF WF-SA < 9; IF WF-STATUS = "35" GO Z.
           IF WF-SA > 8 COMPUTE WD-SB = WF-SB + 900
               ELSE MOVE WF-STATUS TO WD-SB.
           DISPLAY WX-FEHL AT 2401.
           ACCEPT WD-X AT 2465.
           STOP RUN.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACE UPON CRT.
           CALL "CAUP" USING "990101248000000" WH-CREG.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE "  Dateikonvertierung" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           perform konsreg.
           perform artreg.
           perform debreg.
           perform faktreg.
           perform daureg.
           perform lfsreg.
           perform stareg.
       Y.  DISPLAY "Programm beendet" WITH BLANK SCREEN AT 2428.
       Z.  STOP RUN.
      ******************************************************************
       conv2 section.
       a.  move 0 to wn-num.
           divide 16 into wh-dk(1) giving wh-dk(1) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(1) * 10) * 100.
           divide 16 into wh-dk(2) giving wh-dk(2) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(2) * 10).
       z.  exit.
      ******************************************************************
       conv3 section.
       a.  move 0 to wn-num.
           divide 16 into wh-dk(1) giving wh-dk(1) remainder ws.
           compute wn-num = (ws + wh-dk(1) * 10) * 10000.
           divide 16 into wh-dk(2) giving wh-dk(2) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(2) * 10) * 100.
           divide 16 into wh-dk(3) giving wh-dk(3) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(3) * 10).
       z.  exit.
      ******************************************************************
       conv4 section.
       a.  move 0 to wn-num.
           divide 16 into wh-dk(1) giving wh-dk(1) remainder ws.
           compute wn-num = (ws + wh-dk(1) * 10) * 1000000.
           divide 16 into wh-dk(2) giving wh-dk(2) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(2) * 10) * 10000.
           divide 16 into wh-dk(3) giving wh-dk(3) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(3) * 10) * 100.
           divide 16 into wh-dk(4) giving wh-dk(4) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(4) * 10).
       z.  exit.
      ****************************************************************
       conv7 section.
       a.  move 0 to wn-num.
           divide 16 into wh-dk(1) giving wh-dk(1) remainder ws.
           compute wn-num = (ws + wh-dk(1) * 10) * 1000000000000.
           divide 16 into wh-dk(2) giving wh-dk(2) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(2) * 10) * 10000000000.
           divide 16 into wh-dk(3) giving wh-dk(3) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(3) * 10) * 100000000
           divide 16 into wh-dk(4) giving wh-dk(4) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(4) * 10) * 1000000.
           divide 16 into wh-dk(5) giving wh-dk(5) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(5) * 10) * 10000
           divide 16 into wh-dk(6) giving wh-dk(6) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(6) * 10) * 100
           divide 16 into wh-dk(7) giving wh-dk(7) remainder ws.
           compute wn-num = wn-num + (ws + wh-dk(7) * 10).
       z.  exit.
      ******************************************************************
       ALCON SECTION.
       A.  perform  varying ax from 1 by 1 until ax > ay
               move wx-x(ax) to iy
               if iy = 0 or iy > 127 move iy to wx-x(ax)
               else move wr-cv(iy) to wx-x(ax).
       Z.  EXIT.
      *****************************************************************
       grm section.
           divide 16 into wk-grp giving wc remainder ws.
           compute wf-grp = ws + wc * 10.
           divide 16 into wk-son giving wc remainder ws.
           compute wf-son = ws + wc * 10.
           divide 16 into wk-meh giving wc remainder ws.
           compute wf-meh = ws + wc * 10.
           divide 16 into wk-nk giving wc remainder ws.
           compute wf-nk = ws + wc * 10.
           divide 16 into wk-tb giving wc remainder ws.
           compute wf-tb = ws + wc * 10.
       z.  exit.
      *****************************************************************
       konsreg section.
       A.  open input OLDKONS.
           open output konstant.
           close konstant.
           OPEN I-O KONSTANT.
           DISPLAY "Konstanten....:" AT 0711.
           move 0 to wh-key.
       C.  READ OLDKONS next AT END close konstant GO Z.
           MOVE OKO-SATZ TO KO-SATZ.
           IF KO-SATZ = LOW-VALUE GO C.
           move oko-num to wh-hilf.
           move wh-hilf to ko-num.
           if ko-num < wh-key add 10 to ko-num.
           EVALUATE KO-NUM
               WHEN 01 move oko-mon to wh-cv
                       perform conv3
                       move wn-num to wc-datum
                       CALL "CAUP" USING "04DATPRF" WH-CREG
                       move wx-datum to ko-mon
                       move oko-renum to wh-cv
                       perform conv3
                       move wn-num to ko-renum
                       move oko-firma to ko-firma
                       move oko-sperre to ko-sperre
                       move oko-ort to ko-ort
                       move oko-tx to ko-tx
                       move 0 to ko-eannr
                       move oko-merk to ko-merk
                       perform varying wi from 1 by 1 until wi > 6
                           move oko-ust(wi) to ko-ust(wi)
                       end-perform
               WHEN 02
               WHEN 03
               WHEN 04
               WHEN 05 move oko-kopf to ko-kopf
               WHEN 06 move oko-erst to wh-cv
                       perform conv3
                       move wn-num to ko-erst
                       move oko-datum to wh-cv
                       perform conv3
                       move wn-num to ko-datum
                       perform varying wi from 1 by 1 until wi > 10
                           move oko-erloes(wi) to ko-erloes(wi)
                       end-perform
               WHEN 10 move oko-tm to ko-tm
               WHEN 11 THRU 30
                       initialize ko-tsatz
                       if oko-kto(1:) not = space
                           move oko-kto to wh-cv
                           perform conv3
                           move wn-num to ko-kto
                       else compute ko-kto = ko-num * 10 + 2 - 100
                       end-if
                       if oko-rest(1:) = space move 0 to oko-rest
                       end-if
                       move oko-rest to ko-rest
                       move oko-tour to ko-tour.
           move ko-num to wh-key.
           if ko-num > 10 and < 31 add -10 to ko-num.
           write ko-satz.
           DISPLAY ko-num at 0727.
           go c.
       z.  exit.
      *****************************************************************
       artreg section.
       A.  open input OLDART.
           open output artikel.
           close artikel.
           OPEN I-O ARTIKEL.
           DISPLAY "Artikel.......:" AT 0811.
       C.  READ OLDART AT END close artikel GO Z.
           IF OR-SATZ = LOW-VALUE GO C.
           initialize ar-satz.
           move or-key to wh-cv.
           perform conv2.
           move wn-num to ar-num.
           move or-kto to wh-cv.
           perform conv4.
           move wn-num to ar-kto.
           move or-adat to wh-cv.
           perform conv3.
           move wn-num to wc-datum
           CALL "CAUP" USING "04DATPRF" WH-CREG
           move wx-datum to ar-adat.
           move or-preis(4:1) to wh-f.
           perform vz.
           move wh-f to or-preis(4:1).
           move or-preis to ar-preis.
           move or-vkp(4:1) to wh-f.
           perform vz.
           move wh-f to or-vkp(4:1).
           move or-vkp to ar-vkp.
           move or-ekp(4:1) to wh-f.
           perform vz.
           move wh-f to or-ekp(4:1).
           move or-ekp to ar-ekp.
           move or-anz(3:1) to wh-f.
           perform vz.
           move wh-f to or-anz(3:1).
           move or-anz to ar-anz.
           move or-apreis(4:1) to wh-f.
           perform vz.
           move wh-f to or-apreis(4:1).
           move or-apreis to ar-apreis.
           move or-avkp(4:1) to wh-f.
           perform vz.
           move wh-f to or-avkp(4:1).
           move or-avkp to ar-avkp.
           move or-grm to wk-grm.
           perform grm.
           move wf-grm to ar-grm.
           move or-bez to wt-x.
           set ay to 25.
           perform alcon move wt-x to ar-bez.
           display ar-bez at 0827.
           move ar-bez to ar-abez.
           move or-ust to ar-ust.
           move or-pl to ar-pl.
           write ar-satz invalid go c.
           go c.
       z.  exit.
      ******************************************************************
       vz section.
       a.  evaluate wh-f
               when x"0f" move x"0c" to wh-f
               when x"1f" move x"1c" to wh-f
               when x"2f" move x"2c" to wh-f
               when x"3f" move x"3c" to wh-f
               when x"4f" move x"4c" to wh-f
               when x"5f" move x"5c" to wh-f
               when x"6f" move x"6c" to wh-f
               when x"7f" move x"7c" to wh-f
               when x"8f" move x"8c" to wh-f
               when x"9f" move x"9c" to wh-f.
       z.  exit.
      ******************************************************************
       debreg section.
       A.  open input olddeb.
           open output debitor.
           close debitor.
           OPEN I-O DEBITOR.
           DISPLAY "Kunden........:" at 0911.
       C.  READ OLDDEB AT END close debitor GO Z.
           IF OB-SATZ = LOW-VALUE GO C.
           MOVE OB-KETTD TO WH-KETTD.
           INITIALIZE DE-SATZ.
           MOVE 1 TO DE-INKASSO.
           move ob-fnr TO DE-FNR.
           if ob-ktonr(1:) = low-values go c.
           move ob-ktonr to DE-KTONR.
           if ob-satz(6:1) not = x"2f" go c.
           if de-ktonr = 0 go c.
           MOVE OB-MCODE TO DE-MCODE.
           MOVE OB-KTOART TO DE-KTOART.
           MOVE 0 TO DE-ANLMERK.
           MOVE OB-KOND TO DE-KOND.
           MOVE OB-BEZ TO DE-BEZ.
           MOVE 100 TO DE-RET.
           move ob-prdat to wh-cv.
           perform conv3.
           move wn-num to wc-datum
           CALL "CAUP" USING "04DATPRF" WH-CREG
           move wx-datum to DE-PRDAT.
           move ob-sperr to wh-cv.
           perform conv3.
           move wn-num to wc-datum
           CALL "CAUP" USING "04DATPRF" WH-CREG
           move wx-datum to DE-SPERR.
           MOVE OB-REKTO TO DE-REKTO.
           MOVE OB-RAST TO DE-RAST.
           IF OB-RAB(1:2) = X"000F" MOVE 0 TO OB-RAB.
           MOVE OB-RAB TO DE-RAB.
           MOVE OB-EAN TO DE-EAN.
           MOVE WH-FAKART TO DE-FAKART.
           MOVE WH-STATIS TO DE-STATIS.
           MOVE WH-ANREDE TO DE-ANR.
           MOVE WH-TOUR TO DE-TOUR.
           move de-bez to wt-x.
           set ay to 130.
           perform alcon move wt-x to de-bez.
           display de-bez with size 50 at 0927.
           inspect de-mcode replacing all x"e1" by "s".
           move de-mcode to wt-tx.
           call "CAUP" USING "20CONV08" WH-CREG.
           move wt-tx to de-mcode.
           move ob-sub to wh-cv.
           perform conv2.
           move wn-num to de-sub.
           move 0 to de-bonus de-rbasis.
           write de-satz invalid go c.
           go c.
       z.  exit.
      ******************************************************************
       faktreg section.
       a.  open input oldfakt.
           open output faktdat.
           close faktdat.
           open i-o faktdat.
           DISPLAY "Sonderpreise..:" at 1011.
       c.  read oldfakt at end close faktdat go z.
           if of-satz = low-value go c.
           move of-satz to fa-satz.
           IF FA-ART > 6 GO C.
           move of-mon to wh-cv.
           perform conv2.
           move wn-num to fa-mon.
           divide 16 into of-art giving wc remainder ws.
           compute of-art = ws + wc * 10.
           move of-art to fa-art.
           move of-ktonr to wh-cv.
           perform conv3.
           move wn-num to fa-ktonr.
           move of-arnum to wh-cv.
           perform conv2.
           move wn-num to fa-arnum.
           display fa-ktonr at 1027 "/" fa-arnum.
           move 1 to wi.
       e.  perform varying wi from 1 by 1 until wi > 40
               move of-menge(wi)(3:1) to wh-f
               perform vz
               move wh-f to of-menge(wi)(3:1)
               move of-menge(wi) to fa-menge(wi)
               move of-retour(wi)(3:1) to wh-f
               perform vz
               move wh-f to of-retour(wi)(3:1)
               move of-retour(wi) to fa-retour(wi).

       g.  perform varying wi from 1 by 1 until wi > 4
               move of-preis(wi)(4:1) to wh-f
               perform vz
               move wh-f to of-preis(wi)(4:1)
               move of-preis(wi) to fa-preis(wi)
               move of-ab(wi) to wh-cv
               perform conv3
               move wn-num to wc-datum
               call "caup" using "04datprf" wh-creg
               move wx-datum to fa-ab(wi)
               move of-bis(wi) to wh-cv
               perform conv3
               move wn-num to wc-datum
               call "caup" using "04datprf" wh-creg
               move wx-datum to fa-bis(wi)
               move of-ret(wi)(4:1) to wh-f
               perform vz
               move wh-f to of-ret(wi)(4:1)
               move of-ret(wi) to fa-ret(wi)
               move of-rab(wi)(2:1) to wh-f
               perform vz
               move wh-f to of-rab(wi)(2:1)
               move of-rab(wi) to fa-rab(wi).
           move of-kuarnum to wh-cv.
           perform conv7.
           move wn-num to fa-kuarnum.
      *-------------------------------------- ku-arn fehlt noch <-
           write fa-satz invalid go c.
           go c.
       z.  exit.
      ******************************************************************
       zwiread section.
       a.  read olddau at end go x.
           read olddau at end go x.
           read olddau at end go x.
           go z.
       x.  move HIGH-VALUES to od-satz.
       z.  exit.
      ******************************************************************
       daureg section.
       a.  open input olddau.
           open output dauer.
           close dauer.
           open i-o dauer.
           DISPLAY "Dauerbestellg.:" AT 1111.
       c.  read olddau at end close dauer go z.
           if od-satz(1:2) = x"fdff" perform zwiread
               if od-satz = HIGH-VALUES go z else go c.
           initialize da-satz.
           if od-satz = low-value go c.
           move od-ktonr to wh-cv.
           perform conv3.
           move wn-num to da-ktonr.
           move od-arnum to wh-cv.
           perform conv2.
           move wn-num to da-arnum.
           display da-ktonr at 1127 "/" da-arnum.
           move 1 to wi.
       e.  move od-anz(wi)(3:1) to wh-f.
           perform vz.
           move wh-f to od-anz(wi)(3:1).
           move od-anz(wi) to da-anz(wi).
           if wi < 18 add 1 to wi go e.
           move od-dat(1) to wh-cv.
           perform conv3.
           move wn-num to wc-datum.
           call "caup" using "04datprf" wh-creg
           move wx-datum to da-dat(1).
           move od-dat(2) to wh-cv.
           perform conv3.
           move wn-num to wc-datum
           call "caup" using "04datprf" wh-creg
           move wx-datum to da-dat(2).
           move od-dat(3) to wh-cv.
           perform conv3.
           move wn-num to wc-datum.
           call "caup" using "04datprf" wh-creg
           move wx-datum to da-dat(3).
           move od-sub to wh-cv.
           perform conv2.
           move wn-num to da-sub.
           move od-grm to wk-grm.
           perform grm.
           move wf-grm to da-grm.
           move od-bez to wt-x.
           set ay to 25.
           perform alcon move wt-x to da-bez.
           divide 16 into od-tour giving wc remainder ws.
           compute od-tour = ws + wc * 10.
           move od-tour to da-tour.
           divide 16 into od-fart giving wc remainder ws.
           compute od-fart = ws + wc * 10.
           move od-fart to da-fart.
           divide 16 into od-ean giving wc remainder ws.
           compute od-ean = ws + wc * 10.
           move od-ean to da-ean.
           write da-satz invalid go c.
           go c.
       z.  exit.
      ******************************************************************
       zwilfs section.
       a.  read oldlfs at end go x.
           read oldlfs at end go x.
           read oldlfs at end go x.
           read oldlfs at end go x.
           read oldlfs at end go x.
           read oldlfs at end go x.
           read oldlfs at end go x.
           go z.
       x.  move HIGH-VALUES to ol-satz.
       z.  exit.
      ******************************************************************
       lfsreg section.
       a.  open input oldlfs.
           open output lfschein.
           close lfschein.
           open i-o lfschein.
           DISPLAY "Lieferscheine.:" AT 1211.
       c.  read oldlfs at end close lfschein go z.
           if ol-satz(1:2) = x"fdff" perform zwilfs
               if ol-satz = HIGH-VALUES go z else go c.
           if ol-satz = low-value go c.
           move ol-satz to lf-satz.
           move ol-datum to wh-cv.
           perform conv3.
           move wn-num to wc-datum.
           call "caup" using "04datprf" wh-creg
           move wx-datum to lf-datum.
           move ol-sub to wh-cv.
           perform conv2.
           move wn-num to lf-sub.
           move ol-ktonr to wh-cv.
           perform conv3.
           move wn-num to lf-ktonr.
           move ol-num to wh-cv.
           perform conv3.
           move wn-num to lf-num.
           move ol-bez to lf-bez.
           display lf-ktonr at 1227 "/" lf-num " " lf-bez.
           divide 16 into ol-tour giving wc remainder ws.
           compute ol-tour = ws + wc * 10.
           move ol-tour to lf-tour.
           move ol-fart to lf-fart.
           divide 16 into ol-stat giving wc remainder ws.
           compute ol-stat = ws + wc * 10.
           move ol-stat to lf-stat.
           divide 16 into ol-ean giving wc remainder ws.
           compute ol-ean = ws + wc * 10.
           move ol-ean  to lf-ean.
           move ol-arnum to wh-cv.
           perform conv2.
           move wn-num to lf-arnum.
           perform VARYING wi FROM 1 by 1 until wi > 3
               move ol-menge(wi)(3:1) to wh-f
               perform vz
               move wh-f to ol-menge(wi)(3:1)
               move ol-menge(wi) to lf-menge(wi).
           move ol-preis(4:1) to wh-f.
           perform vz.
           move wh-f to ol-preis(4:1).
           move ol-preis to lf-preis.
           move ol-grm to wk-grm.
           perform grm.
           move wf-grm to lf-grm.
           move ol-bez to wt-x.
           set ay to 25.
           perform alcon move wt-x to lf-bez.
           write lf-satz invalid key go c.
           go c.
       z.  exit.
      ******************************************************************
       stareg section.
       a.  open input oldstat.
           open output statistik.
           close statistik.
           open i-o statistik.
           DISPLAY "Statistik.....:" AT 1311.
       c.  read oldstat at end close statistik go z.
           if os-satz = low-value go c.
           move os-satz to st-satz.
           move os-ktonr to wh-cv.
           perform conv4.
           move wn-num to st-ktonr.
           if st-ktonr > 99999920 go c.
           move os-arnum to wh-cv.
           perform conv2.
           move wn-num to st-arnum.
           display st-ktonr at 1327 "/" st-arnum.
           divide 16 into os-jahr giving wc remainder ws.
           compute st-jahr = ws + wc * 10.
           if st-jahr < 98 go c.
           move 1 to wi.
       e.  move os-mengen(wi)(4:1) to wh-f.
           perform vz.
           move wh-f to os-mengen(wi)(4:1).
           move os-mengen(wi) to st-mengen(wi).
           move os-ret(wi)(4:1) to wh-f.
           perform vz.
           move wh-f to os-ret(wi)(4:1).
           move os-ret(wi) to st-ret(wi).
           move os-umsatz(wi)(5:1) to wh-f.
           perform vz.
           move wh-f to os-umsatz(wi)(5:1).
           move os-umsatz(wi) to st-umsatz(wi).
           move os-gewinn(wi)(5:1) to wh-f.
           perform vz.
           move wh-f to os-gewinn(wi)(5:1).
           move os-gewinn(wi) to st-gewinn(wi).
           if wi < 12 add 1 to wi go e.
           write st-satz invalid go c.
           go c.
       z.  exit.
