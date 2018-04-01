      ************************************* Unternehmerstammdaten FIBU *
       FD  KONSFIBU     external       LABEL RECORD STANDARD.
      *------------------------------------------------> WH-Key = 1 <-
       01  KF-SATZ.
           03  KF-NUM                  PIC 999     COMP-3.
           03  KF-FIRMA                PIC X(22).
           03  KF-UST                  PIC 99V99   COMP    OCCURS 6.
           03  KF-JONUM                PIC 9(6)    COMP.
           03  KF-PER                  PIC 9(8)    COMP.
           03  KF-ZTAG                 PIC 9(8)    COMP.
           03  KF-ZPZ                  PIC 999V99  COMP.
           03  KF-SPERRE               PIC X(6).
           03  KF-ORT                  PIC X(15).
           03  KF-MERK                 PIC XXXX.
           03  KF-KST                  PIC 99      COMP.
           03  KF-ANR.
               05 KF-AN                PIC X(9)            OCCURS 7.
           03  KF-TABK.
               05 KF-KONDIT            PIC 9(13)   COMP-3  OCCURS 9.
      *-----------------------------------------> Symbole WH-KEY = 10 <-
       01  KF-MSATZ.
           03  FILLER                  PIC X(5).
           03  KF-TS.
               05 KF-SYM               PIC XX              OCCURS 30.
               05 KF-SKZ               PIC 9(4)    COMP-X  OCCURS 30.
           03  KF-TR-SR                PIC 9(6)    COMP    OCCURS  3.
           03  KF-TSR REDEFINES
               KF-TR-SR.
               05 KF-NFUST             PIC 9(6)    COMP.
               05 KF-ERLSR             PIC 9(6)    COMP.
               05 KF-ERLTR             PIC 9(6)    COMP.
           03  KOF-NEU                 PIC XXX.
      ************* Buchs„tze f. Fibuueberleitung aus FAKT = Fibudatei *
       FD  BUCHUNG      external       LABEL RECORD STANDARD.
       01  BU-SATZ.
           03  BU-KEY                  PIC 9(6)     COMP.
           03  BU-RKEY.
               05 BU-SA                PIC X.
               05 BU-REN               PIC 9(7)      COMP.
           03  BU-KTONR                PIC 9(6)      COMP.
           03  BU-KOND                 PIC 9(13)     COMP.
           03  BU-DAT                  PIC 9(6)      COMP.
           03  BU-SY                   PIC 99        COMP.
           03  BU-GK                   PIC 9(6)      COMP.
           03  BU-Z                    PIC 9         COMP.
           03  BU-VM                   PIC 9         COMP.
           03  BU-RF                   PIC 9(7)      COMP.
           03  BU-U1                   PIC 9         COMP.
           03  BU-U2                   PIC 9         COMP.
           03  BU-MW                   PIC S9(7)V99  COMP-3.
           03  BU-B1                   PIC S9(9)V99  COMP-3.
           03  BU-B2                   PIC S9(9)V99  COMP-3.
           03  BU-SH                   PIC S9        COMP-3.
           03  BU-BET                  PIC S9(9)V99  COMP-3.
           03  BU-KST                  PIC S9(5)     COMP-3.
           03  BU-TX                   PIC X(40).
           03  BU-SKTOBAS              PIC S9(9)V99  COMP-3.
           03  BU-VALDAT               PIC 9(6)      COMP.
      *--------------------------------------------> fr Fremdw„hrung <-
           03  BU-FWBET                PIC S9(9)V99  COMP.
           03  BU-KURS                 PIC 9(4)V9(7) COMP.
           03  BU-FSYM                 PIC XXX.
      *------------------ BU-SA= "K" ---------------> Kontenbernahme <-
       01  BU-KSATZ.
           03  FILLER                  PIC X(19).
           03  BU-MCODE                PIC X(8).
           03  BU-ANREDE               PIC 99       COMP.
           03  BU-BEZ                  PIC X(110).
           03  BU-TEL                  PIC X(35).
