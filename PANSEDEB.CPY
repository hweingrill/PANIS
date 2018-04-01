           SELECT DEBITOR    ASSIGN TO "PANDEBIT.DAT"
                             ORGANIZATION INDEXED  ACCESS DYNAMIC
                             RECORD KEY DE-KEY
                             ALTERNATE KEY DE-MCODE DUPLICATES
                             ALTERNATE KEY DE-RKEY DUPLICATES
                             ALTERNATE KEY DE-IKEY =
                                           DE-TOUR, DE-SUB DUPLICATES
                             ALTERNATE KEY DE-EKEY = DE-ETIK
                                           DE-KEY DUPLICATES
                             ALTERNATE KEY DE-EANKEY = DE-GLN DE-KTONR
                             FILE STATUS WF-STATUS.
