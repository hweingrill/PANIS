      *****************************************************************
       FD  ETIKETT      external       LABEL RECORD STANDARD.
       01  ET-SATZ.
           03  ET-KEY.
               05 ET-SA                PIC X.
               05 ET-NUM               PIC 9(4)       COMP.
           03  ET-BEZ-A                PIC X(20).
           03  ET-BEZ-B                PIC X(20).
           03  ET-ZU-A                 PIC X(39).
           03  ET-RES-A                PIC X.
           03  ET-ZU-B                 PIC X(39).
           03  ET-RES-B                PIC X.
           03  ET-ZU-C                 PIC X(39).
           03  ET-SONDER               PIC X.
           03  ET-MHD                  PIC 999.
           03  ET-EAN                  PIC 9(13).
      *-----------------------------------------> alte Preisetiketten <-
       01  ET-PSATZ.
           03  FILLER                  PIC XXX.
           03  ET-BEZ-P                PIC X(17).
           03  ET-GEW-P                PIC 9(5)       COMP.
           03  FILLER                  PIC X(4).
