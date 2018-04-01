      ******************************************** Scheinrechnungskopf *
       FD  SREKOPF      external       LABEL RECORD STANDARD.
       01  SK-SATZ.
           03  SK-KEY.
               05 SK-SA                PIC 99       COMP.
               05 SK-LIEFER            PIC 9(5)     COMP.
               05 SK-BELDAT            PIC 9(8)     COMP.
               05 SK-BELNR             PIC 9(7)     COMP.
           03  SK-LFD                  PIC 999      COMP.
           03  SK-MCODE                PIC X(5).
           03  SK-BEZ                  PIC X(30).
           03  SK-BET                  PIC 9(7)V99  COMP.
           03  SK-RAB                  PIC 99V9     COMP  OCCURS 2.
