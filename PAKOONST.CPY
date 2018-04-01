      *******************************************  Konstanten - Datei *
       FD  KONSTANT    EXTERNAL        LABEL RECORDS STANDARD
                                       RECORD 128 CHARACTERS.
      *------------------------------------------------> WH-Key = 1 <-
       01  KO-SATZ.
           03  KO-NUM                  PIC 999     COMP-3.
           03  KO-ABNUM                PIC 9(6)    COMP.
           03  KO-FIRMA                PIC X(22).
           03  KO-UST                  PIC 99V99   COMP    OCCURS 6.
           03  KO-RENUM                PIC 9(6)    COMP    OCCURS 4.
           03  KO-SPERRE               PIC X(6).
           03  KO-ORT                  PIC X(15).
           03  KO-EANNR                PIC 9(8)    COMP.
           03  KO-MERK                 PIC XXXX.
           03  KO-LFNUM                PIC 9(6)    COMP.
           03  KO-DRU                  PIC 99      COMP.
           03  FILLER                  PIC X(41).
      *--------------------------------> Mengeneinheiten WH-KEY = 10 <-
       01  KO-MSATZ.
           03  FILLER                  PIC X(5).
           03  KO-ANR.
               05 KO-AN                PIC X(9)          OCCURS 7.
           03  KO-TM.
               05 KO-MEH               PIC XXX           OCCURS 20.
               05 KO-NK                PIC 99      COMP  OCCURS 20.
      *-----------------------------> Druckerseinstellung WH-KEY = 11 <-
       01  KOD-SATZ.
           03  FILLER                  PIC X(38).
           03  KOD-APAR                PIC X(45).
           03  KOD-BPAR                PIC X(45).
      *---------------------------------------------> Druckerparamter <-
       01  KO-PSATZ.
           03  FILLER                  PIC XX.
           03  KOP-BEZ                 PIC X(26).
           03  KOP-TX.
               05 KOP-STG              PIC X(10)     OCCURS 10.
      *----------> Firmenkopf am Briefpapier 2 - 5 / Rasterkopf 7 - 9 <-
       01  KO-KSATZ.
           03  FILLER                  PIC X(26).
           03  KO-KOPF                 PIC X(102).
      *-----------------------------------> Summen WH-Key = 90 + User <-
       01  KO-SSATZ.
           03  FILLER                  PIC XX.
           03  KO-ERST                 PIC 9(6)     COMP.
           03  KO-DATUM                PIC 9(6)     COMP.
           03  KO-ERLOES               PIC S9(7)V99 COMP-3  OCCURS 24.
      *------------------------------> Verk„ufer-Summen WH-KEY = + 21 <-
       01  KO-VSATZ.
           03  FILLER                  PIC X(6).
           03  KO-NAME                 PIC X(20).
           03  KO-JAHR                 PIC 9999     COMP.
           03  KO-UMSATZ               PIC S9(7)V99 COMP-3  OCCURS 20.
      *-------------------------------> Rechnungstexte WH-KEY = + 101 <-
       01  KO-RSATZ.
           03  FILLER                  PIC X(68).
           03  KO-TX                   PIC X(60).
      *--------------------------> Artikelgruppentexte WH-KEY = + 120 <-
       01  KO-GSATZ.
           03  FILLER                  PIC X(6).
           03  KO-GRPTX                PIC X(40).
