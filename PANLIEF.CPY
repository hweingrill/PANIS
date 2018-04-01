      *****************************************************************
       FD  LFSCHEIN     external       LABEL RECORD STANDARD.
       01  LF-SATZ.
           03  LF-DTKEY.
               05 LF-DATUM             PIC 9(8)       COMP.
               05 LF-TOUR              PIC 99         COMP.
               05 LF-SUB               PIC 9999       COMP.
           03  LF-PRIME.
               05 LF-KTONR             PIC 9(6)       COMP.
               05 LF-NUM               PIC 9(6)       COMP.
           03  LF-ARNUM                PIC 9(4)       COMP.
           03  LF-MENGE                PIC S9(5)      COMP   OCCURS 3.
           03  LF-PREIS                PIC S9(4)V999  COMP.
           03  LF-BEZ                  PIC X(25).
           03  LF-GRM.
               05 LF-GRP               PIC 99         COMP-X.
               05 LF-SON               PIC 99         COMP.
               05 LF-MEH               PIC 99         COMP.
               05 LF-NK                PIC 99         COMP.
               05 LF-TB                PIC 99         COMP.
           03  LF-FART                 PIC 99         COMP.
           03  LF-STAT                 PIC 99         COMP.
           03  LF-EAN                  PIC 99         COMP.
           03  LF-SORT.
               05 LF-SGRP              PIC 99         COMP-X.
               05 LF-ARBEZ             PIC X(9).
      ****************************************** Texte je Lieferschein *
       FD  TX-LFTEXT      external     LABEL RECORD STANDARD.
       01  TX-SATZ.
           03  TX-KEY.
               05 TX-KTONR             PIC 9(5)      COMP.
               05 TX-LFDAT             PIC 9(8)      COMP.
               05 TX-LFNUM             PIC 9(5)      COMP.
           03  TX-TEXT                 PIC X(60)  OCCURS 2.
      ********************************** Datensammlung DESADV + INVOIC *
       FD  HILFDESA     external       LABEL RECORD STANDARD.
       01  HD-SATZ.
           03  HD-KEY.
               05 HD-DATUM             PIC 9(8)       COMP.
               05 HD-GLN               PIC 9(13)      COMP.
               05 HD-ILN               PIC 9(13)      COMP.
               05 HD-LFNUM             PIC 9(7)       COMP.
               05 HD-ARNUM             PIC 9(5)       COMP.
           03  HD-BEZ                  PIC X(26).
           03  HD-ANZ                  PIC S9(5)V99   COMP.
           03  HD-MEH                  PIC XXX.
           03  HD-KTONR                PIC 9(5)       COMP.
           03  HD-NUM                  PIC 9(5)       COMP.
           03  HD-EXT                  PIC XXX.
           03  HD-PREIS                PIC S9(3)V999  COMP.
           03  HD-BET                  PIC S9(5)V99   COMP.
           03  HD-USTPZ                PIC 99         COMP.
           03  HD-UST                  PIC S9(5)V99   COMP.
           03  HD-BISDAT               PIC 9(8)       COMP.
           03  HD-NTG                  PIC 999        COMP.
