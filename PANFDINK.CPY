      ************************************************** Kundeninkassi *
       FD  INKASSO                     LABEL RECORD STANDARD.
       01  IN-SATZ.
           03  IN-NUM                  PIC 999        COMP.
           03  IN-TAB                              OCCURS 50.
               05 IN-KTONR             PIC 9(6)       COMP.
               05 IN-BET               PIC S9(7)V99   COMP.
