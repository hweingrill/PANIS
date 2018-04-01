           SELECT LAGARTIK ASSIGN TO "PARARTIK.DAT"
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY LG-KEY
                           ALTERNATE RECORD LG-AKEY DUPLICATES
                           ALTERNATE RECORD LG-BKEY DUPLICATES
                           ALTERNATE RECORD LG-CKEY = LG-LKEY LG-KEY
                           FILE STATUS IS WF-STATUS.
           SELECT LAGMATCH ASSIGN TO "PARMATCH.DAT"
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY LM-KEY
                           ALTERNATE RECORD LM-AKEY DUPLICATES
                           FILE STATUS IS WF-STATUS.
