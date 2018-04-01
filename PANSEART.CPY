           SELECT ARTIKEL    ASSIGN TO "PANARTIK.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY AR-KEY
                             ALTERNATE RECORD AR-AKEY DUPLICATES
                             ALTERNATE RECORD AR-BKEY DUPLICATES
                             ALTERNATE RECORD AR-CKEY DUPLICATES
                             ALTERNATE RECORD AR-SORT =
                                       AR-SGRP AR-NUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
