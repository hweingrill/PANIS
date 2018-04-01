           SELECT FAKTDAT    ASSIGN TO "PANEFAKT.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY FA-KEY = FA-KTONR FA-ARNUM
                             ALTERNATE RECORD KEY FA-AKEY
                             ALTERNATE RECORD FA-BKEY =
                                     FA-KTONR FA-KUARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT FAKTEDI    ASSIGN TO "PANEDIFA.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY EF-KEY
                             FILE STATUS IS WF-STATUS.
