           SELECT KREDIT   ASSIGN "PAKOLIEF.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KR-KEY
                           ALTERNATE KEY KR-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT KNOTEN   ASSIGN "PAKNOTEN.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KN-KEY
                           ALTERNATE KEY KN-AKEY DUPLICATES
                           FILE STATUS WF-STATUS.
