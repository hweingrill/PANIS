      ******************************************************************
       FD  KONSLAG      external       LABEL RECORD STANDARD.
      *------------------------------------------------> WH-Key = 1 <-
       01  KL-SATZ.
           03  KL-NUM                  PIC 999     COMP-3.
           03  KL-FIRMA                PIC X(22).
           03  KL-UST                  PIC 99V99   COMP    OCCURS 6.
           03  KL-JONUM                PIC 9(6)    COMP.
           03  KL-PER                  PIC 9(6)    COMP.
           03  FILLER                  PIC X(3).
           03  KL-CHAN                 PIC 99      COMP.
           03  KL-WTOL                 PIC 9V9     COMP.
           03  KL-SPERRE               PIC X(6).
           03  KL-ORT                  PIC X(15).
           03  KL-MERK                 PIC XXXX.
           03  KL-DRU                  PIC 99      COMP.
           03  KL-ANR.
               05 KL-AN                PIC X(9)            OCCURS 7.
           03  KL-TABK.
               05 KL-KONDIT            PIC 9(13)   COMP-3  OCCURS 9.
      *-----------------------------> Firmenkopf am Briefpapier 2 - 5 <-
       01  KL-KSATZ.
           03  FILLER                  PIC X(26).
           03  KL-KOPF                 PIC X(102).
      *------------> Schleppsummenspeicher WH-KEY = 2  user + 1 (1-5) <-
       01  KL-BSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KL-BUDAT                PIC 9(6)     COMP.
           03  KL-JOKEY                PIC X(5).
           03  KL-SASUM.
               05 KL-JSUM              PIC S9(9)V99 COMP.
               05 KL-GRUPPE            PIC S9(9)V99 COMP  OCCURS 40.
      *---------------------------------> zwingende Konten WH-KEY = 9 <-
       01  KL-TSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KL-KTONR                PIC 9(6)     COMP   OCCURS 20.
           03  KL-WIJ                            OCCURS 4.
               05 KL-WKZ               PIC 99       COMP.
               05 KL-WBEG              PIC 9(8)     COMP.
               05 KL-WEND              PIC 9(8)     COMP.
      *---------------------------------> Mengeneinheiten WH-KEY = 10 <-
       01  KL-MSATZ.
           03  FILLER                  PIC X(24).
           03  KL-MART                 PIC 9999     COMP  OCCURS 22.
           03  KL-TM.
               05 KL-MEH               PIC XXX      OCCURS 20.
      *-----------------------------> Druckerseinstellung WH-KEY = 11 <-
       01  KLD-SATZ.
           03  FILLER                  PIC X(2).
           03  KLD-VERZ                PIC X(36).
           03  KLD-APAR                PIC X(45).
           03  KLD-BPAR                PIC X(45).
      *---------------------------------------------> Druckerparamter <-
       01  KL-PRSATZ.
           03  FILLER                  PIC XX.
           03  KOP-BEZ                 PIC X(26).
           03  KOP-TX.
               05 KOP-STG              PIC X(10)     OCCURS 10.
      *-------------------------------------> Bestelltext WH-KEY = 20 <-
       01  KL-KSATZ.
           03  FILLER                  PIC XXX.
           03  KL-BEST                 PIC X(60)     OCCURS 3.
      *-----------------------------------------> Preise Key 21 - 120 <-
       01  KL-GSATZ.
           03  FILLER                  PIC XXX.
           03  KL-GRPTX                PIC X(25).
      *----------------------------------------> Firmen Key 121 - 220 <-
       01  KL-FSATZ.
           03  FILLER                  PIC XXX.
           03  KL-FIBEZ                PIC X(22).
      ************************************* Journal- u. Buchungszeilen *
       FD  LAGERBZ      external       LABEL RECORD STANDARD.
       01  LB-SATZ.
           03  LB-KEY.
               05 LB-KTONR             PIC 9(6)      COMP.
               05 LB-BUDAT             PIC 9(8)      COMP.
               05 LB-LFD               PIC 99        COMP-X.
           03  LB-JKEY.
               05 LB-USE               PIC 99        COMP.
               05 LB-SEITE             PIC 9(7)      COMP.
           03  LB-SRECH.
               05 LB-SA                PIC 99        COMP.
               05 LB-LIEFER            PIC 9(6)      COMP.
               05 LB-BELNR             PIC 9(7)      COMP.
               05 LB-BELDAT            PIC 9(8)      COMP.
               05 LB-SKLFD             PIC 9999      COMP.
           03  LB-SYM                  PIC 99        COMP.
           03  LB-BMEH                 PIC 99        COMP.
           03  LB-BNK                  PIC 99        COMP.
           03  LB-ANZ                  PIC S9(7)V99  COMP.
           03  LB-MENGE                PIC S9(7)V99  COMP.
           03  LB-PREIS                PIC S9(7)V99  COMP.
           03  LB-BET                  PIC S9(7)V99  COMP.
           03  LB-MEH                  PIC 99        COMP.
           03  LB-NK                   PIC 99        COMP.
           03  LB-KSTNR                PIC 99        COMP.
           03  LB-RAB                  PIC S99V99    COMP   OCCURS 2.
           03  LB-TX                   PIC X(15).
      *----------------------------------------> Jahresabschluásummen <-
       01  LB-ASATZ.
           03  FILLER                  PIC X(30).
           03  LB-SOLL                 PIC S9(9)V99  COMP-3.
           03  LB-HABEN                PIC S9(9)V99  COMP-3.
           03  LB-EBIL                 PIC S9(9)V99  COMP-3.
