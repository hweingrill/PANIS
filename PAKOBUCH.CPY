      ************************************************* 392 Characters *
       FD  BUCHSATZ     external       LABEL RECORD STANDARD.
       01  BU-SATZ.
           03  BU-HEAD                PIC XXXX.           *> wegen RM85
           03  BU-INHALT.
      *----------------------------------> Sperrkenzeichen fr Satz 1 <-
      *            0 = Schnittstellendatei kann bearbeitet werden
      *            1 = in Schnittstellendatei wird soeben gearbeitet;
      *                solange Satz 1 lesen, bis Sperre aufgehoben ist
               05  BU-SPERRE          PIC 9.
               05  BU-LREC            PIC 9(5).
               05  BU-VONDAT          PIC 9(8).
               05  BU-BISDAT          PIC 9(8).
               05  FILLER             PIC X(368).
      *-----------------------------------------------------------------
      *                      Inhalt ab Satz 2 - n
           03  BU-INH2 REDEFINES BU-INHALT.
               05  BU-RELKEY          PIC 9(5).
               05  BU-FNR             PIC 9(3).
               05  BU-KTONR           PIC 9(6).
               05  BU-BUDAT           PIC 9(5).
               05  BU-BELDAT          PIC 9(8).
               05  BU-VALDAT          PIC 9(8).
               05  BU-SYM             PIC XX.
               05  BU-GEGKTO          PIC 9(6).
               05  BU-BELNR           PIC 9(5).
               05  BU-EVINR           PIC 9(5).
               05  BU-FRBEL           PIC X(12).
               05  BU-KOND.
                   07  BU-KOND1P      PIC 9V9.
                   07  BU-KOND1T      PIC 9(3).
                   07  BU-KOND2P      PIC 9V9.
                   07  BU-KOND2T      PIC 9(3).
                   07  BU-KONDN       PIC 9(3).
               05  BU-ZUSTAND         PIC 9.
               05  BU-REFNR           PIC 9(5).
               05  BU-USTCODE         PIC S9
                                      SIGN IS TRAILING SEPARATE.
               05  BU-USTCODE2        PIC S9
                                      SIGN IS TRAILING SEPARATE.
               05  BU-USTBTR          PIC S9(8)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-SKOBETR         PIC S9(8)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-BMG-U1          PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-BMG-2           PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-SHKZ            PIC S9
                                      SIGN IS TRAILING SEPARATE.
               05  BU-BUBETRAG        PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-KST             PIC 9(5).
               05  BU-BUTEXT          PIC X(20).
               05  BU-FW-KZ           PIC 9.
               05  BU-FW-KURS         PIC 9(5)V9(4).
               05  BU-FW-BETRAG       PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-FW-SKONTO       PIC S9(8)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-ZESS            PIC 9.
               05  BU-ART             PIC X.
               05  BU-ZAHL            PIC 9.
               05  BU-STEU            PIC 9.
               05  BU-EB-BUCH         PIC 9.
               05  BU-MAHNST          PIC 9.
               05  BU-SEL1            PIC 9(3).
               05  BU-RATINT          PIC 9(3).
               05  BU-RATRBETR        PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-RATANZ          PIC 9(3).
               05  BU-RATTEIL         PIC S9(7)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-RATANZDAT       PIC 9(8).
               05  BU-RAT1DAT         PIC 9(8).
               05  BU-SKOF-BETR       PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BU-BEM-T-KOB       PIC 9.
               05  BU-BEM-T-LFD       PIC 9.
               05  BU-BEM-TEXT        PIC X(80).
               05  FILLER             PIC X(35).
      *                      HEX '0D' '0A' ALS SATZENDE-KENNUNG (SEQU.D.)
               05  HEX-0D0A           PIC XX.
