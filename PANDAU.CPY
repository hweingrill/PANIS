      ********************************************* Dauerbestellungen *
       FD  DAUER        external       LABEL RECORD STANDARD.
       01  DA-SATZ.
           03  DA-KEY.
               05 DA-KTONR             PIC 9(6)       COMP.
               05 DA-ARNUM             PIC 9(4)       COMP.
           03  DA-BEZ                  PIC X(25).
           03  DA-ANZ                  PIC S9(5)      COMP   OCCURS 27.
           03  DA-DAT                  PIC 9(8)       COMP   OCCURS 3.
           03  DA-GRM.
               05 DA-GRP               PIC 99         COMP-X.
               05 DA-SON               PIC 99         COMP.
               05 DA-MEH               PIC 99         COMP.
               05 DA-NK                PIC 99         COMP.
               05 DA-TB                PIC 99         COMP.
           03  DA-FART                 PIC 99         COMP.
           03  DA-TOUR                 PIC 99         COMP.
           03  DA-SUB                  PIC 9999       COMP.
           03  DA-EAN                  PIC 99         COMP.
