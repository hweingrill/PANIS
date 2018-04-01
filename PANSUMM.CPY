      *************************************************** Lieferdaten *
       FD  PROTOK       external      LABEL RECORD STANDARD.
       01  PR-SATZ.
           03  PR-ARNUM               PIC 999.
           03  PR-MENGE               PIC S9(7)V99.
           03  PR-RETMG               PIC S9(7)V99.
      *********************************** Touren- u. Backzettelsummen *
       FD  SUMMEN       external       LABEL RECORD STANDARD
                                       RECORD 128 CHARACTERS.
       01  SU-SATZ.
           03  SU-AKEY.                                   *> f. Backz.
               05 SU-KONDIT            PIC X(15).
               05 SU-SRT               PIC 9999    COMP.
               05 SU-ANUM              PIC 9(6)    COMP.
           03  SU-KEY.                                    *> f. Touren
               05 SU-TB                PIC 99      COMP.
               05 SU-NUM               PIC 9(6)    COMP.
           03  SU-BEZ                  PIC X(25).
           03  SU-MEH                  PIC 99      COMP.
           03  SU-NK                   PIC 99      COMP.
           03  SU-GVZ                  PIC 99      COMP.
           03  SU-GRP                  PIC 9999    COMP.
           03  SU-MG                   PIC S9(7)   COMP   OCCURS 26.
           03  SU-TGGEW                PIC 9(5)V99 COMP.
