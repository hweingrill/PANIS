           SELECT SREKOPF    ASSIGN TO "PAKSREKP.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SK-KEY DUPLICATES
                             ALTERNATE KEY SK-AKEY
                             FILE STATUS IS WF-STATUS.
           SELECT SREZEIL    ASSIGN TO "PAKSREZL.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SR-KEY
                             FILE STATUS IS WF-STATUS.
