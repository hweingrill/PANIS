      *****************************************************************
       FD  DEBITOR      external       LABEL RECORDS STANDARD.
       01  DE-SATZ.
           03  DE-GKEY.
               05 DE-FNR               PIC 999       COMP.
               05 DE-KEY.
                  07 DE-KTONR          PIC 9(6)      COMP.
           03  DE-MCODE                PIC X(8).
           03  DE-KTOART               PIC 9.
           03  DE-ETIK                 PIC 99        COMP.
           03  DE-ANLMERK              PIC 99        COMP.
           03  DE-ANMERK               PIC X(60).
           03  DE-KOND                 PIC 9.
           03  DE-BEZ                  PIC X(110).
           03  DE-RET                  PIC 999V9     COMP.
           03  DE-PRDAT                PIC 9(8)      COMP.
           03  DE-SPERR                PIC 9(8)      COMP.
           03  DE-REKTO                PIC 9(6)      COMP.
           03  DE-RAST                 PIC 9.
           03  DE-RAB                  PIC S99V9     COMP.
           03  DE-EAN                  PIC 9.
           03  DE-RKEY.
               05 DE-FAKART            PIC 99.
               05 DE-TOUR              PIC 99.
               05 DE-SUB               PIC 9999      COMP.
      *---------------------------> neue Eintragung fr Fakturenliste <-
           03  DE-INKASSO              PIC 99        COMP.
           03  DE-RENR                 PIC 9(6)      COMP.
           03  DE-REDAT                PIC 9(6)      COMP.
           03  DE-REBET                PIC S9(6)V99  COMP.
           03  DE-ANR                  PIC 99        COMP.
           03  DE-STATIS               PIC 99        COMP.
           03  DE-BONUS                PIC 99V99     COMP.
           03  DE-RBASIS               PIC 99V99     COMP.
           03  DE-FILNR                PIC 9(5).
           03  DE-GLN                  PIC 9(14).
           03  DE-UID                  PIC X(14).
