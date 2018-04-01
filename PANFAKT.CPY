      **************************************** Fakturen- u. Preisdatei *
       FD  FAKTDAT      external       LABEL RECORD STANDARD.
       01  FA-SATZ.
           03  FA-AKEY.
               05 FA-MON               PIC 9999       COMP.
               05 FA-ART               PIC 99         COMP.
               05 FA-KTONR             PIC 9(6)       COMP.
               05 FA-SGRP              PIC 99         COMP-X.
               05 FA-ARBEZ             PIC X(9).               *> P-Key
               05 FA-ARNUM             PIC 9(4)       COMP.    *> P-key
           03  FA-MR.                                   *> Monatsraster
               05 FA-AR                       OCCURS 40 INDEXED OX.
                  07 FA-MENGE          PIC S9(5)      COMP.
                  07 FA-RETOUR         PIC S9(5)      COMP.
           03  FA-XTAB.
               05 FA-TAB                       OCCURS 4  INDEXED PX.
                  07 FA-PREIS          PIC S9(4)V999  COMP.
                  07 FA-AB             PIC 9(8)       COMP.
                  07 FA-BIS            PIC 9(8)       COMP.
                  07 FA-RET            PIC S9(4)V999  COMP.
                  07 FA-RAB            PIC S99V9      COMP.
           03  FA-KUARNUM              PIC 9(14)      COMP.
      *---------------------------> Rastermengen der letzten 3 Monate <-
           03  FA-RAST                         OCCURS 3.
               05 FA-RMON              PIC 9999       COMP.
               05 FA-RMR.
                  07 FA-RAR                       OCCURS 31.
                     09 FA-RMENGE      PIC S9(5)      COMP.
                     09 FA-RRETOUR     PIC S9(5)      COMP.
      ******************************************** Edifakt-Aufstellung *
       FD  FAKTEDI      external       LABEL RECORD STANDARD.
       01  EF-SATZ.
           03  EF-KEY.
               05 EF-KUNDE             PIC 9(6)       COMP.
               05 EF-RENUM             PIC 9(6)       COMP.
           03  EF-KTONR                PIC 9(6)       COMP.
           03  EF-REDAT                PIC 9(8)       COMP.
           03  EF-NETTO                PIC 9(6)V99    COMP.
           03  EF-UST                  PIC 9(6)V99    COMP.
           03  EF-GESAMT               PIC 9(6)V99    COMP.
           03  EF-EMBALL               PIC 9(6)V99    COMP.
           03  EF-GLN                  PIC 9(13)      COMP.
           03  EF-TEXT                 PIC X(110).
