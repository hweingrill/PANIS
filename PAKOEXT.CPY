       01  WE-EXT       external.
           03  WH-KEY                  PIC 9(5).
           03  WK-M.
               05 WM-MA                PIC 99        COMP.
               05 WM-KO                PIC 99        COMP.
               05 WM-JO                PIC 99        COMP.
               05 WM-SK                PIC 99        COMP.
           03  WE-BSR                  PIC 9         COMP.
                                               88 SR      VALUE  1.
           03  WE-USE                  PIC 9         COMP.
           03  WE-PER                  PIC 9(8)      COMP.
           03  WE-WKZ                  PIC 99        COMP.
           03  WE-WJAHR                PIC 9(8)      COMP   OCCURS 2.
           03  WE-WTOL                 PIC 9V9       COMP.
           03  WM-DRU                  PIC 99        COMP.
           03  WE-UMS                  PIC S9(8)V99  COMP.
           03  WE-PREIS                PIC S9(7)V99  COMP.
           03  WE-RAB                  PIC S99V99    COMP   OCCURS 2.
           03  WE-SOLL                 PIC S9(8)V99  COMP.
           03  WE-HABEN                PIC S9(8)V99  COMP.
           03  WE-SALDO                PIC S9(8)V99  COMP.
           03  WE-BELNR                PIC 9(7)      COMP.
           03  WE-BELDAT               PIC 9(8)      COMP.
           03  WE-LIEFER               PIC 9(6)      COMP.
           03  WE-GB                   PIC 99        COMP.
           03  WK-KTONR                PIC 9(6)      COMP.
           03  WZ-JOUR                 PIC 9999      COMP.
           03  WE-SY                   PIC 99        COMP.
           03  WE-CHAN                 PIC 99        COMP.
           03  WE-TS.
               05 WE-SYM               PIC XX                OCCURS 20.
               05 WE-SKZ               PIC 9999      COMP    OCCURS 20.
           03  WE-SYMT                 PIC 9999.
           03  WR-Y REDEFINES WE-SYMT.
               05 WE-SH                PIC 9.
               05 WE-UST               PIC 9.
               05 WE-OP                PIC 9.
               05 WE-DIV               PIC 9.
           03  WE-RETAB.
               05 WH-RT                        OCCURS 30.
                  07 WE-OPKEY          PIC X(8).
                  07 WE-REF            PIC 9(7)       COMP.
                  07 WE-REBET          PIC S9(9)V99   COMP.
                  07 WE-SKONTO         PIC S9(9)V99   COMP.
                  07 WE-UKZ            PIC 99         COMP.
           03  WZ-BUCH                 PIC 9999.
           03  WS-BUCH                 PIC 9999.
           03  WN-BUEB                 PIC X(30).
           03  WN-KONS                 PIC X(12).
           03  WN-KOST                 PIC X(12).
           03  WN-BZEI.
               05 WN-BZ                PIC X(6).
               05 WN-FNR               PIC 99.
               05 WN-RS                PIC X(4).
           03  WE-PARAM       OCCURS 10.
               05 WE-DRU               PIC 99        COMP.
               05 WE-KOPF              PIC 99        COMP.
               05 WE-WO                PIC 99        COMP.
               05 WE-TYP               PIC 99        COMP.
           03  WE-STG                  PIC X(40)    OCCURS  10.
           03  WH-P                    PIC 99        COMP.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       DECLARATIVES.
