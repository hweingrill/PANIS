       01  WH-CREG.
           03  WS-TASTE                PIC 99        COMP.
                  88  HELP    VALUE 8.
           03  WU-FUNC                 PIC 99  COMP    VALUE 0.
           03  WU-KEYTAB.
               05 WX-TASTE             PIC 99  COMP.
                  88  TABL    VALUE 5.    88  ESC    VALUE 7.
                  88  FEHLER  VALUE 99.   88  WOLI   VALUE 1 8.
                  88  RET     VALUE 0.    88  WORE   VALUE 2 9.
                  88  AUF     VALUE 3 10. 88  KIST   VALUE 6.
                  88  AB      VALUE 4 11. 88  RETA   VALUE 0 5.
                  88  SF1     VALUE 12.   88  SF2    VALUE 13.
                  88  SF3     VALUE 14.   88  EINF   VALUE 15.
                  88  APOS    VALUE 16.   88  BAUF   VALUE 17.
                  88  ENTF    VALUE 18.   88  XPOS   VALUE 19.
                  88  BAB     VALUE 20.   88  ENDE   VALUE 19.
                  88  SEIN    VALUE 21.   88  SENT   VALUE 22.
                  88  SAPO    VALUE 23.   88  SEPO   VALUE 24.
               05 WX-FTAB              PIC X(70).
           03  WX-CODE                 PIC X(6)               OCCURS 2.
           03  WV-MCODE.
               05  WV-MC               PIC X      OCCURS 13 INDEXED MX.
           03  WH-MCODE                PIC X(13).
           03  WH-WERT                 PIC S9(9)V999 COMP-3 VALUE ZERO.
           03  WH-NUM                  PIC 9(13)            VALUE ZERO.
           03  WR-NUM REDEFINES WH-NUM PIC 9    OCCURS 13 INDEXED RX KX.
           03  WK-ANZ                  PIC 99      COMP-X   VALUE ZERO.
           03  WK                      PIC 99      COMP     VALUE ZERO.
           03  WH-NEG                  PIC 9       COMP.
           03  WH-ACCDAT               PIC 9(6)    COMP     VALUE ZERO.
           03  WH-PG                   PIC 99      VALUE ZERO.
           03  WF-STATUS.          88  FINE     VALUE "10".
               05 WF-SA                PIC 9.
               05 WF-SB                PIC 99      COMP-X.
                                   88  ZUGRIF   VALUE 65 68 209 224.
           03  WF                      PIC S99     COMP-3.
           03  WM                      PIC S999    COMP-3.
           03  WX                      PIC S999    COMP-3.
           03  WY                      PIC S999    COMP-3.
           03  WZ                      PIC S999    COMP-3.
           03  WR                      PIC S999    COMP-3.
           03  WH-HILF                 PIC S999    COMP-3.
           03  WO-TAG                  PIC XXX.
           03  WO-TGN                  PIC 99      COMP.
           03  WX-DATUM                PIC 9(8)    COMP   VALUE 0.
           03  WV-DATUM                PIC 9(8)    COMP   VALUE 0.
           03  WS-DATUM                PIC 9(8)    COMP   VALUE 0.
           03  WM-DATUM                PIC 9(8)    COMP   VALUE 0.
           03  WH-DATUM                PIC 9(6)    COMP   VALUE 0.
           03  WC-DATUM                PIC 9(6)    VALUE 0.
           03  WC-RD REDEFINES WC-DATUM.
               05  WC-TAG              PIC 99.
               05  WC-MONAT            PIC 99.
               05  WC-JAHR             PIC 99.
           03  WZ-DATUM                PIC 9(6)    VALUE 0.
           03  WZ-D REDEFINES WZ-DATUM.
               05  WZ-JAHR             PIC 99.
               05  WZ-MONAT            PIC 99.
               05  WZ-TAG              PIC 99.
           03  VDU-GZDAT.
               05  VDU-ORT             PIC X(17).
               05  FILLER              PIC X.
               05  VDU-DATUM.
                   07 VDU-TAG          PIC Z9.
                   07 VDU-A            PIC X       VALUE ".".
                   07 VDU-MONAT        PIC Z9.
                   07 VDU-B            PIC X       VALUE ".".
                   07 VDU-JAHR         PIC 99.
           03  WK-KOPF.
               05  WK-GEB              PIC X(22)   VALUE SPACE.
               05  WK-A                PIC XXXX    VALUE " ³ ".
               05  WK-FIRMA            PIC X(22)   VALUE SPACE.
               05  WK-B                PIC X(4)    VALUE " ³  ".
               05  WK-DATUM            PIC X(8)    VALUE SPACE.
               05  WK-C                PIC XXX     VALUE " / ".
               05  WK-TAG              PIC XXXB    VALUE SPACE.
               05  WK-BS               PIC Z9-     VALUE SPACE.
               05  WK-FREI             PIC X(10)   VALUE SPACE.
           03  WT-UST                  PIC 99V99      COMP-3 OCCURS 6.
           03  WH-TABK.
               05 WT-KONDIT            PIC 9(13)      COMP-3 OCCURS 9.
           03  WH-TABTX.
               05 WT-TXT               PIC X(9)              OCCURS 10.
               05 WT-FA                PIC XXX               OCCURS 10.
               05 WT-TM.
                  07 WT-MEH            PIC XXX               OCCURS 20.
                  07 WT-NK             PIC 99       COMP     OCCURS 20.
      *---------------------------------------------> Textbehandlung <-
           03  WT-ADR.
               05 WR-ADR               PIC X(40) OCCURS 6 INDEXED TX FX.
           03  WT-TX                   PIC X(80).
           03  WR-TX REDEFINES WT-TX   PIC X    OCCURS 80 INDEXED TY FY.
           03  WT-BEZ                  PIC X(241).
           03  WR-BEZ REDEFINES WT-BEZ PIC X   OCCURS 241 INDEXED BX.
           03  WH-X                    PIC X.
           03  VDU-ECK                 PIC 9999.
           03  VDR-ECK REDEFINES VDU-ECK.
               05  VDU-EL              PIC 99.
               05  VDU-EP              PIC 99.
           03  VDU-LP                  PIC 9999      VALUE ZERO.
           03  VDR-LP REDEFINES VDU-LP.
               05 VDU-L                PIC 99.
               05 VDU-P                PIC 99.
           03  VDU-RES                 PIC 9999      COMP.
           03  WN-FREI                 PIC 9(6)      COMP.
      *-------------------------------------> Ende der šbergabefelder <-
