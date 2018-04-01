      ************************************************** Waagprotokoll *
       FD  PROTWAAG     external       LABEL RECORD STANDARD.
       01  PWA-SATZ.
           03  PWA-AKEY.
               05 PWA-SA               PIC 99        COMP.
               05 PWA-DAT              PIC 9(8)      COMP.
               05 PWA-KEY.
                  07 PWA-RENUM         PIC 9(4)      COMP.
           03  PWA-RPBEZ               PIC X(15).
           03  PWA-ZEIT                PIC 9(4)      COMP.
           03  PWA-ANZ                 PIC 9(5)V99   COMP.
           03  PWA-TAB.
               05 PWA-LGNUM            PIC 9(5)      COMP   OCCURS 20.
               05 PWA-SOLL             PIC 9(7)      COMP   OCCURS 20.
               05 PWA-IST              PIC 9(7)      COMP   OCCURS 20.
               05 PWA-KOBEZ            PIC X(25)            OCCURS 20.
