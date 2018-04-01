      ************************************************* 392 Characters *
       FD  BUCHPROT    external       LABEL RECORD STANDARD.
       01  BP-SATZ.
           03  BP-HEAD                PIC XXXX.           *> wegen RM85
           03  BP-INHALT.
      *----------------------------------> Sperrkenzeichen fr Satz 1 <-
      *            0 = Schnittstellendatei kann bearbeitet werden
      *            1 = in Schnittstellendatei wird soeben gearbeitet;
      *                solange Satz 1 lesen, bis Sperre aufgehoben ist
               05  BP-SPERRE          PIC 9.
               05  BP-LREC            PIC 9(5).
               05  BP-VONDAT          PIC 9(8).
               05  BP-BISDAT          PIC 9(8).
               05  FILLER             PIC X(368).
      *-----------------------------------------------------------------
      *                      Inhalt ab Satz 2 - n
           03  BP-INH2 REDEFINES BP-INHALT.
               05  BP-RELKEY          PIC 9(5).
               05  BP-FNR             PIC 9(3).
               05  BP-KTONR           PIC 9(6).
               05  BP-BUDAT           PIC 9(5).
               05  BP-BELDAT          PIC 9(8).
               05  BP-VALDAT          PIC 9(8).
               05  BP-SYM             PIC XX.
               05  BP-GEGKTO          PIC 9(6).
               05  BP-BELNR           PIC 9(5).
               05  BP-EVINR           PIC 9(5).
               05  BP-FRBEL           PIC X(12).
               05  BP-KOND.
                   07  BP-KOND1P      PIC 9V9.
                   07  BP-KOND1T      PIC 9(3).
                   07  BP-KOND2P      PIC 9V9.
                   07  BP-KOND2T      PIC 9(3).
                   07  BP-KONDN       PIC 9(3).
               05  BP-ZUSTAND         PIC 9.
               05  BP-REFNR           PIC 9(5).
               05  BP-USTCODE         PIC S9
                                      SIGN IS TRAILING SEPARATE.
               05  BP-USTCODE2        PIC S9
                                      SIGN IS TRAILING SEPARATE.
               05  BP-USTBTR          PIC S9(8)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-SKOBETR         PIC S9(8)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-BMG-U1          PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-BMG-2           PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-SHKZ            PIC S9
                                      SIGN IS TRAILING SEPARATE.
               05  BP-BUBETRAG        PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-KST             PIC 9(5).
               05  BP-BUTEXT          PIC X(20).
               05  BP-FW-KZ           PIC 9.
               05  BP-FW-KURS         PIC 9(5)V9(4).
               05  BP-FW-BETRAG       PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-FW-SKONTO       PIC S9(8)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-ZESS            PIC 9.
               05  BP-ART             PIC X.
               05  BP-ZAHL            PIC 9.
               05  BP-STEU            PIC 9.
               05  BP-EB-BUCH         PIC 9.
               05  BP-MAHNST          PIC 9.
               05  BP-SEL1            PIC 9(3).
               05  BP-RATINT          PIC 9(3).
               05  BP-RATRBETR        PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-RATANZ          PIC 9(3).
               05  BP-RATTEIL         PIC S9(7)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-RATANZDAT       PIC 9(8).
               05  BP-RAT1DAT         PIC 9(8).
               05  BP-SKOF-BETR       PIC S9(9)V99
                                      SIGN IS TRAILING SEPARATE.
               05  BP-BEM-T-KOB       PIC 9.
               05  BP-BEM-T-LFD       PIC 9.
               05  BP-BEM-TEXT        PIC X(80).
               05  FILLER             PIC X(35).
      *                      HEX '0D' '0A' ALS SATZENDE-KENNUNG (SEQU.D.)
               05  HEX-0D0A           PIC XX.
