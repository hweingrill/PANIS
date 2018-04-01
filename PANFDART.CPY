      ************************************************ Verkaufsartikel *
       FD  ARTIKEL      external       LABEL RECORD STANDARD.
       01  AR-SATZ.
           03  AR-KEY.
               05 AR-NUM               PIC 9(4)       COMP.
           03  AR-AKEY.
               05 AR-SA                PIC X.
               05 AR-ABEZ              PIC X(10).
           03  AR-BEZ                  PIC X(25).
           03  AR-KTO                  PIC 9(8)       COMP.
           03  AR-UST                  PIC 99         COMP.
           03  AR-PREIS                PIC S9(4)V999  COMP.
           03  AR-VKP                  PIC S9(5)V99   COMP.
           03  AR-EKP                  PIC S9(4)V999  COMP.
           03  AR-ANZ                  PIC S9(5)      COMP.
           03  AR-GRM.
               05 AR-GRP               PIC 99         COMP-X.
               05 AR-SON               PIC 99         COMP.
               05 AR-MEH               PIC 99         COMP.
               05 AR-NK                PIC 99         COMP.
               05 AR-TB                PIC 99         COMP.
           03  AR-PL                   PIC 9.
           03  AR-APREIS               PIC S9(4)V999  COMP.
           03  AR-ADAT                 PIC 9(8)       COMP.
           03  AR-AVKP                 PIC S9(5)V99   COMP.
           03  AR-BKEY.
               05 AR-GVZ               PIC 99         COMP.
           03  AR-RAB                  PIC 99         COMP.
           03  AR-CKEY.
               05 AR-OEPZ              PIC 99         COMP.
           03  AR-INH                  PIC 99V99      COMP.
           03  AR-INH                  PIC X(8).
           03  AR-SGRP                 PIC 9(4)       COMP.
           03  AR-FILLER               PIC X(8).
