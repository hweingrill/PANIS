      *****************************************************************
       FD  ARTIKEL      external       LABEL RECORD STANDARD
                                       RECORD 80 CHARACTERS.
       01  AR-SATZ.
           03  AR-KEY.
               05 AR-NUM               PIC 9(4)       COMP.
           03  AR-AKEY.
               05 AR-SA                PIC X.
               05 AR-ABEZ              PIC X(10).
           03  AR-BEZ                  PIC X(25).
           03  AR-KTO                  PIC 9(8)       COMP.
           03  AR-UST                  PIC 99         COMP.
           03  AR-PREIS                PIC S9(4)V999  COMP-3.
           03  AR-VKP                  PIC S9(5)V99   COMP-3.
           03  AR-EKP                  PIC S9(4)V999  COMP-3.
           03  AR-ANZ                  PIC S9(5)      COMP-3.
           03  AR-GRM                  PIC X(5).
           03  AR-PL                   PIC 9.
           03  AR-APREIS               PIC S9(4)V999  COMP-3.
           03  AR-ADAT                 PIC 9(6)       COMP.
           03  AR-AVKP                 PIC S9(5)V99   COMP-3.
           03  AR-BKEY.
               05 AR-GVZ               PIC 99         COMP.
           03  FILLER                  PIC X(4).
