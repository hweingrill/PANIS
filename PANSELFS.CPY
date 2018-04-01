           SELECT LFSCHEIN   ASSIGN TO DISK "PANELFS.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY LF-KEY = LF-PRIME LF-ARNUM
                             ALTERNATE RECORD LF-AKEY =
                                 LF-DTKEY LF-PRIME LF-SORT LF-ARNUM
                                 DUPLICATES
                             ALTERNATE RECORD LF-ARNUM DUPLICATES
                             FILE STATUS IS WF-STATUS.
           SELECT TX-LFTEXT  ASSIGN TO "PANLFTX.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY TX-KEY
                             FILE STATUS IS WF-STATUS.
           SELECT HILFDESA   ASSIGN TO "PANHIDES.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY HD-KEY
                             FILE STATUS IS WF-STATUS.
