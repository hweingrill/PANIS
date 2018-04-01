      ***************************** Buchs„tze f. Fibuueberleitung FAKT *
       FD  BUCHFAKT     external       LABEL RECORD STANDARD.
       01  BF-SATZ.
           03  BF-KEY                  PIC 9(6)     COMP.
           03  BF-RKEY.
               05 BF-SA                PIC X.
               05 BF-REN               PIC 9(7)     COMP.
           03  BF-KTONR                PIC 9(6)     COMP.
           03  BF-KOND                 PIC 9(13)    COMP.
           03  BF-DAT                  PIC 9(6)     COMP.
           03  BF-SY                   PIC 99       COMP.
           03  BF-GK                   PIC 9(6)     COMP.
           03  BF-Z                    PIC 9        COMP.
           03  BF-VM                   PIC 9        COMP.
           03  BF-RF                   PIC 9(7)     COMP.
           03  BF-U1                   PIC 9        COMP.
           03  BF-U2                   PIC 9        COMP.
           03  BF-MW                   PIC S9(7)V99 COMP-3.
           03  BF-B1                   PIC S9(9)V99 COMP-3.
           03  BF-B2                   PIC S9(9)V99 COMP-3.
           03  BF-SH                   PIC S9       COMP-3.
           03  BF-BET                  PIC S9(9)V99 COMP-3.
           03  BF-KST                  PIC S9(5)    COMP-3.
           03  BF-TX                   PIC X(40).
           03  BF-SKTOBAS              PIC S9(9)V99 COMP-3.
      *------------------ BF-SA= "K" ---------------> Kontenbernahme <-
       01  BF-KSATZ.
           03  FILLER                  PIC X(19).
           03  BF-MCODE                PIC X(8).
           03  BF-ANREDE               PIC 99       COMP.
           03  BF-BEZ                  PIC X(110).
           03  BF-TEL                  PIC X(35).
