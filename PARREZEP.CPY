      *****************************************************************
       FD  REZEPT       external       LABEL RECORD STANDARD.
       01  RE-SATZ.
           03  RE-KEY.
               05 RE-NUM               PIC 9(4)        COMP.
           03  RE-BEZ.
               05 RE-AKEY              PIC X(10).
               05 RE-TX                PIC X(15).
           03  RE-KNET                 PIC 99          COMP.
           03  RE-FILL                 PIC 9(4)        COMP.
           03  RE-KOMP                          OCCURS 20 INDEXED XR.
               05 RE-KONUM             PIC 9(4)        COMP.
               05 RE-KOBEZ             PIC X(20).
               05 RE-MENGE             PIC 9(6)        COMP.
               05 RE-PROZ REDEFINES
                  RE-MENGE             PIC 999V999     COMP.
               05 RE-TNAM              PIC 9(6)        COMP.
               05 RE-EKP               PIC 9(4)V99     COMP.
               05 RE-NGEW              PIC 9(6)        COMP.
           03  RE-TEIL                 PIC 9(5)        COMP.
           03  RE-GEW                  PIC 9(7)        COMP.
           03  RE-BET                  PIC S9(7)V99    COMP.
           03  RE-MAT                  PIC S9(7)V99    COMP.
           03  RE-PREIS                PIC S9(5)V99    COMP.
           03  RE-ART                  PIC 99          COMP.
           03  RE-SKP                  PIC S9(5)V99    COMP.
           03  RE-ZEIT                 PIC S99V9(4)    OCCURS 5 COMP.
           03  RE-BLECH                PIC 99          COMP  OCCURS 3.
           03  RE-TEMP                 PIC 99V9        COMP.
           03  RE-GNR                  PIC 99          COMP.
           03  RE-GANG              OCCURS 4.              *> 3 = Pause
               05 RE-GMIN              PIC 99          COMP.
               05 RE-GSEK              PIC 99          COMP.
           03  RE-CHMAX                PIC 9(6)        COMP.
           03  RE-CHKZ                 PIC 99          COMP.
           03  RE-CHGEW                PIC 9(6)        COMP.
