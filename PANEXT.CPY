       01  WH-PANEXT      external.
           03  WK-EAN                  PIC 9(8)      COMP.
           03  WK-MON                  PIC 9(8)      COMP.
           03  WA-DAT                  PIC 9(8)      COMP.
           03  WE-MAXDAT               PIC 9(8)      COMP.
           03  WE-MINDAT               PIC 9(8)      COMP.
           03  WH-KEY                  PIC 9(5).
           03  WH-BUKEY                PIC 9(5).
      *------------------> Version, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-VER               PIC 99        COMP.
               05 WM-KO                PIC 99        COMP.
               05 WM-PR                PIC 99        COMP.
               05 WM-RB                PIC 99        COMP.
           03  WE-LFS                  PIC 99        COMP.
           03  WN-FKON                 PIC X(35).
           03  WN-BUEB                 PIC X(35).
           03  WE-ARSYM                PIC 99        COMP.
           03  WE-GSSYM                PIC 99        COMP.
           03  WE-PANIS                PIC 99        COMP.
           03  WM-DRU                  PIC 99        COMP.
           03  WE-PARAM       OCCURS 10.
               05 WE-DRU               PIC 99        COMP.
               05 WE-KOPF              PIC 99        COMP.
               05 WE-WO                PIC 99        COMP.
               05 WE-TYP               PIC 99        COMP.
           03  WE-STG                  PIC X(40)    OCCURS  10.
           03  WH-P                    PIC 99        COMP.
      *---------------------------> Key Zerlegung wg. concatinted Key <-
           03  WQ-AKEY.                                      *> LF-AKEY
               05 WQ-DTKEY.
                  07 WQ-DATUM          PIC 9(8)       COMP.
                  07 WQ-TOUR           PIC 99         COMP.
                  07 WQ-SUB            PIC 9999       COMP.
               05 WQ-PRIME.
                  07 WQ-KTONR          PIC 9(6)       COMP.
                  07 WQ-NUM            PIC 9(6)       COMP.
               05 WQ-SORT              PIC X(10).
               05 WQ-ARNUM             PIC 9(4)       COMP.
           03  WP-KEY.                                       *> LF-KEY
               05 WP-PRIME.
                  07 WP-KTONR          PIC 9(6)       COMP.
                  07 WP-NUM            PIC 9(6)       COMP.
               05 WP-ARNUM             PIC 9(4)       COMP.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
