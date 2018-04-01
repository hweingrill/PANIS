      *******************************************  Konstanten - Datei *
       FD  KONSTANT      external      LABEL RECORDS STANDARD.
      *--------------------------------------------------> WH-Key = 1 <-
       01  KO-SATZ.
           03  KO-NUM                  PIC 999     COMP.
           03  KO-MON                  PIC 9(8)    COMP.
           03  KO-FIRMA                PIC X(22).
           03  KO-UST                  PIC 99V99   COMP  OCCURS 6.
           03  KO-RENUM                PIC 9(6)    COMP.
           03  KO-SPERRE               PIC X(5).
           03  KO-ETKDRU               PIC 99      COMP.
           03  KO-ORT                  PIC X(15).
           03  KO-EANNR                PIC 9(8)    COMP.
           03  KO-MERK                 PIC XXXX.
           03  KO-TX                   PIC X(51).
           03  KO-ATU                  PIC XXX.
           03  KO-ATUNR                PIC 9(8)    COMP.
      *-----------------------------> Firmenkopf am Briefpapier 2 - 5 <-
       01  KO-KSATZ.
           03  FILLER                  PIC X(26).
           03  KO-KOPF                 PIC X(102).
      *-------------------------------------------> Summen WH-Key = 6 <-
       01  KO-SSATZ.
           03  FILLER                  PIC XX.
           03  KO-FAKT                 PIC S999V99  COMP  OCCURS 4.
           03  KO-PROZ                 PIC S999V99  COMP  OCCURS 16.
           03  FILLER                  PIC X(5).
           03  KO-RETDAT               PIC 9(8)     COMP.
           03  KO-RET                  PIC 999V9    COMP.
           03  KO-ERST                 PIC 9(8)     COMP.
           03  KO-DATUM                PIC 9(8)     COMP.
           03  KO-ERLOES               PIC S9(7)V99 COMP  OCCURS 10.
      *---------------------------------------------> Konditionen = 7 <-
       01  KO-FSATZ.
           03  FILLER                  PIC XX.
           03  KO-TABK.
               05 KO-KONDIT            PIC 9(13)    COMP-3 OCCURS 9.
      *-------------------------------------------------> Symbole = 8 <-
       01  KO-FSSATZ.
           03  FILLER                  PIC XX.
           03  KO-TS.
               05 KO-SYM               PIC XX              OCCURS 30.
               05 KO-SKZ               PIC 9(4)    COMP-X  OCCURS 30.
      *-----------------------------------> Tourenzeitplan WH-KEY = 9 <-
       01  KO-ZSATZ.
           03  KO-TKO                 PIC X(8).
           03  KO-TOURTAB                     OCCURS 10.
               05 KO-TZEIT             PIC 9(4)      COMP.
               05 KO-TNR               PIC 99        COMP OCCURS 10.
      *---------------------------------------------> Meh WH-KEY = 10 <-
       01  KO-MSATZ.
           03  FILLER                  PIC X(21).
           03  KO-DRU                  PIC 99       COMP.
           03  KO-WJAHR                PIC 9(4)     COMP.
           03  KO-MART                 PIC 9999     COMP  OCCURS 22.
           03  KO-TM.
               05 KO-MEH               PIC XXX      OCCURS 20.
      *-----------------------------> Tourenbezeichnungen WH-KEY + 10 <-
       01  KO-TSATZ.
           03  FILLER                  PIC X(45).
           03  KO-ABRDAT               PIC 9(8)      COMP.
           03  KO-SBEZ                 PIC X(24).
           03  KO-STATNR               PIC X(12).
           03  KO-SUM                  PIC S9(5)V99 COMP   OCCURS 3.
           03  KO-KTO                  PIC 9(6)     COMP.
           03  KO-REST                 PIC S9(5)V99 COMP.
           03  KO-TOUR                 PIC X(25).
      *----------------------------> Netzdruckersteuerung WH-Key = 35 <-
       01  KO-PSATZ.
           03  FILLER                  PIC X(38).
           03  KO-REZPM                PIC X(45).
           03  KO-PARM                 PIC 99      COMP.
           03  KO-EURO                 PIC 9999V99999 COMP.
           03  KO-PARAM   OCCURS 10.
               05 KOP-DRU              PIC 99      COMP.
               05 KOP-KOPF             PIC 99      COMP.
               05 KOP-WO               PIC 99      COMP.
               05 KOP-TYP              PIC 99      COMP.
      *----------------------------> Erl”skontenzuordnung WH-KEY = 36 <-
       01  KO-ESATZ.
           03  FILLER                  PIC XX.
           03  KO-VERZ                 PIC X(36).
           03  KO-KTONR                PIC 9(6)    COMP    OCCURS 21.
           03  KO-MODEM                PIC X(27).
      *--------------------------------> Teiggruppen WH-KEY = 37 - 39 <-
       01  KO-VSATZ.
           03  KO-VKZ                  PIC X.
           03  FILLER                  PIC X(27).
           03  KO-TGRP                 PIC 99       COMP-X OCCURS 100.
      *--------------------------------> Artikel f. autom. Backzettel <-
       01  KO-QSATZ.
           03  FILLER                  PIC X(22).
           03  KO-TB                                OCCURS 22.
               05 KO-BART              PIC 9999     COMP.
               05 KO-BPRZ              PIC 9999     COMP.
      *---------------------> Sammelrechnungssaetze BILLA WH-KEY + 45 <-
       01  KO-RSATZ.
           03  FILLER                  PIC 999      COMP.
           03  KO-REKTO                PIC 9(6)     COMP.
           03  KO-KONTO                PIC 9(6)     COMP.
           03  KO-FIL                  PIC X(37).
           03  KO-DAT                  PIC 9(8)     COMP   OCCURS 3.
           03  FILLER                  PIC X(74).
      *---------------> Filialeliste f. Auslieferung Kern WH-KEY = 60 <-
       01  KF-SATZ.
           03  KF-FILTAB                                  OCCURS 13.
               05 KF-FNR               PIC 9(4)     COMP.
               05 KF-FBEZ              PIC X(7).
               05 KF-SEITE             PIC 9.
