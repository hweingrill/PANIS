      *********************************** Rohstoffe und Betriebsstoffe *
       FD  LAGARTIK     external       LABEL RECORD STANDARD.
       01  LG-SATZ.
           03  LG-KEY.
               05 LG-NUM               PIC 9(4)       COMP.
           03  LG-AKEY.
               05 LG-ABEZ              PIC X(10).
           03  LG-BKEY.
               05 LG-LAGER             PIC X(5).
               05 LG-SORT              PIC 999V99     COMP.
           03  LG-LKEY.
               05 LG-LFT               PIC X(5).
           03  LG-BEZ                  PIC X(25).
           03  LG-STAND                PIC 9(5)V99    COMP.
           03  LG-MIND                 PIC 9(5)V99    COMP.
           03  LG-BMENGE               PIC 9(5)V99    COMP.
           03  LG-ART                  PIC 99         COMP.
           03  LG-EKP                  PIC S9(5)V999  COMP.
           03  LG-VKP                  PIC S9(5)V99   COMP.
           03  LG-PREIS                PIC S9(5)V999  COMP.
           03  LG-ANZ                  PIC S9(5)V999  COMP.
           03  LG-MEH                  PIC 99         COMP.
           03  LG-NK                   PIC 99         COMP.
           03  LG-BMEH                 PIC 99         COMP.
           03  LG-BNK                  PIC 99         COMP.
           03  LG-GRP                  PIC 99         COMP.
           03  LG-NETPZ                PIC 99V9       COMP.
      *------------------> Preise & Rabatt des aktuellen Lieferanten <-
           03  LG-TAB         OCCURS 6.
               05 LG-LIEF              PIC X(5).
               05 LG-DATUM             PIC 9(6)       COMP.
               05 LG-BTTO              PIC S9(5)V99   COMP.
               05 LG-RAB               PIC S99V99     COMP.
               05 LG-ZRAB              PIC S99V99     COMP.
               05 LG-AUF               PIC S999V99    COMP.
               05 LG-SYM               PIC 99         COMP.
               05 LG-NTTO              PIC S9(5)V99   COMP.
               05 LG-TX                PIC X(15).
           03  LG-INHALT.
               05 LG-INH               PIC X(25)   OCCURS 12.
      ******************** Matchcodes fr Rohstoffe und Betriebsstoffe *
       FD  LAGMATCH     external       LABEL RECORD STANDARD.
       01  LM-SATZ.
           03  LM-AKEY.
               05 LM-MCODE             PIC X(13).
               05 LM-KEY.
                  07 LM-LGNUM          PIC 9(4)       COMP.
                  07 LM-LFD            PIC 99         COMP-X.
