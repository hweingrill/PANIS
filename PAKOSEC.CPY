           SELECT KONSLAG  ASSIGN WN-KONS
                           ORGANIZATION RELATIVE, ACCESS DYNAMIC
                           RELATIVE KEY WH-KEY
                           FILE STATUS WF-STATUS.
           SELECT LAGERBZ  ASSIGN WN-BZEI
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY LB-KEY
                           ALTERNATE KEY IS LB-JKEY
                           ALTERNATE KEY IS LB-SRECH DUPLICATES
                               SUPPRESS WHEN SPACES
                           FILE STATUS WF-STATUS.
