       01  WE-EXT       external.
      *    03  WN-PLAN                 PIC X(25).
           03  WN-ARTIK                PIC X(25).
           03  WN-ARKAL                PIC X(25).
           03  WN-KONST                PIC X(25).
           03  WN-LSKOPF               PIC X(25).
           03  WN-LSBEWEG              PIC X(25).
           03  WN-SIKOPF               PIC X(25).
           03  WN-SIBEWEG              PIC X(25).
           03  WE-STG                  PIC X(40)    OCCURS 10.
           03  WH-KEY                  PIC 9(5).
           03  WH-BUKEY                PIC 9(5).
           03  WE-FNR                  PIC 99       COMP.
           03  WM-NEU                  PIC 99       COMP.
           03  WM-DRU                  PIC 99       COMP.
           03  WX-DR                   PIC 99       COMP.
           03  WE-LFTX                 PIC X(60)       OCCURS 4.
           03  WH-PROTNAM.
               05 WH-PN                PIC X(12).
               05 WH-PER               PIC 999.
               05 FILLER               PIC X(4).
           03  WH-RENUM                PIC 9(6)     COMP.
      *    03  WS-LF                   PIC 9999     COMP.
           03  WE-REKTO                PIC 9(6)     COMP.
           03  WE-LIEFDAT              PIC 9(8)     COMP.
           03  WE-ARKIST               PIC 9(6)     COMP.
           03  WE-KTONR                PIC 9(6)     COMP.
           03  WE-VKF                  PIC 99       COMP.
           03  WE-RAB                  PIC S99V99   COMP-3   OCCURS 3.
           03  WE-DEL                  PIC 99       COMP.
           03  WN-STAT.
               05 FILLER               PIC X(6).
               05 WN-JAHR              PIC 99.
               05 FILLER               PIC XXXX.
           03  WN-FKON                 PIC X(35).
           03  WN-BUEB                 PIC X(35).
           03  WN-DATUEB               PIC X(15).
           03  WN-SPAR                 PIC X(15).
           03  WN-INVOICE              PIC X(15).
           03  WN-SAM                  PIC X(15).
           03  WE-ARSYM                PIC 99        COMP.
           03  WE-GSSYM                PIC 99        COMP.
           03  WE-ERSYM                PIC 99        COMP.
