      ********************************************** Artikelstatistik *
       FD  STATISTIK    external      LABEL RECORD STANDARD.
       01  ST-SATZ.
           03  ST-KEY.
               05 ST-KTONR             PIC 9(8)       COMP.
               05 ST-ARNUM             PIC 9(4)       COMP.
               05 ST-JAHR              PIC 99         COMP.
           03  ST-SUM                          OCCURS 12.
               05 ST-MENGEN            PIC S9(7)      COMP.
               05 ST-RET               PIC S9(7)      COMP.
               05 ST-UMSATZ            PIC S9(8)V9    COMP.
               05 ST-GEWINN            PIC S9(8)V9    COMP.
