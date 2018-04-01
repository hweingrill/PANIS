      *****************************************************************
       FD  KREDIT       external       LABEL RECORD STANDARD.
       01  KR-SATZ.
           03  KR-KEY.
               05  KR-KTONR            PIC 9(7)      COMP.
           03  KR-MCODE                PIC X(5).
           03  KR-BEZ                  PIC X(110).
           03  KR-TELE                 PIC X(25).
           03  KR-FAX                  PIC X(25).
           03  KR-ANMERK               PIC X(45).
      ********************************************* ArtikelverknÅpfung *
       FD  KNOTEN       external       LABEL RECORD STANDARD.
       01  KN-SATZ.
           03  KN-KEY.
               05 KN-NUM               PIC 9(5)      COMP.
           03  KN-AKEY.
               05 KN-LFTNUM            PIC 9(5)      COMP.
               05 KN-LFTART            PIC X(15).
