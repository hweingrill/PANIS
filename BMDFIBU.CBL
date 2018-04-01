           SELECT BMDFIB     ASSIGN TO "BMDFIBU.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS WF-STATUS.
      ******************************************************************
       FD  BMDFIB.
       01  BM-KSATZ.
           03  BM-KTONR                PIC 9(9).
           03  BM-BEZ                  PIC X(35).
           03  BM-MCODE                PIC X(20).
           03  BM-ZUS                  PIC X(35).
           03  BM-STR                  PIC X(30).
           03  BM-PLZ                  PIC X(30).
           03  BM-ORT                  PIC X(30).
           03  BM-STKZ                 PIC 9.       *> Steuerkz immer 1
           03  BM-SKZ                  PIC 9.       *> Sammelkto -"-
           03  BM-KEND                 PIC X.       *> Endekz. immer "*"
       01  BM-BSATZ.
           03  BM-SA                   PIC 9.       *> immer 0
           03  BM-KTO                  PIC 9(9).
           03  BM-BUDAT                PIC 9(8).
           03  BM-GGKTO                PIC 9(9).
           03  BM-BELNR                PIC 9(9).
           03  BM-BELDAT               PIC 9(8).
           03  BM-MWST                 PIC 99999.   *> 2000%
           03  BM-STCD                 PIC 99.      *> 03
           03  BM-SH                   PIC 9.       *> 1= S, 2= H
           03  BM-BET                  PIC S9(17)   SIGN TRAILING
                                                    SEPARATE CHARACTER.
           03  BM-UST                  PIC S9(17)   SIGN TRAILING
                                                    SEPARATE CHARACTER.
           03  BM-OPBET                PIC S9(17)   SIGN TRAILING
                                                    SEPARATE CHARACTER.
           03  BM-PER                  PIC 99.      *>
           03  BM-TEXT                 PIC X(18).   *>
           03  BM-SYM                  PIC XX.      *> immer AR
           03  BM-USER                 PIC 99.      *> immer 77
           03  BM-BUKZ                 PIC 99.      *> immer 00
           03  BM-GGBKZ                PIC X.       *> immer S
           03  BM-VBKZ                 PIC X.       *> immer A
           03  BM-END                  PIC XX.      *> immer 0*
           03  BM-REST                 PIC X(36).
      *---------------------------------------------> neue BMD-Kunden <-
       F.  MOVE BF-KTONR TO BM-KTONR.
           MOVE BF-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV10" WH-CREG.
           MOVE WT-TX TO BM-MCODE.
           MOVE 1 TO BM-STKZ BM-SKZ.
           MOVE "*" TO BM-KEND.
           MOVE BF-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE 5 TO WZ.
           IF WR-ADR(5) = SPACE MOVE 4 TO WZ.
           MOVE WR-ADR(WZ)(1:4) TO WH-PLZL(1:).
           INSPECT WH-PLZL REPLACING ALL " " BY "0".
           IF WH-PLZL not NUMERIC MOVE 0 TO WH-PLZL.
           MOVE WR-ADR(1) TO BM-BEZ.
           MOVE WR-ADR(2) TO BM-ZUS.
           MOVE WR-ADR(WZ)(6:) TO BM-ORT.
           MOVE WR-ADR(WZ - 1) TO BM-STR.
           MOVE WH-PLZL TO BM-PLZ.
           WRITE BM-KSATZ.
      ************************************************** BMD-Buchungen *
       BMD-BUCH SECTION.
       A.  INITIALIZE BM-BSATZ.
           MOVE 0 TO BM-SA.
           COMPUTE BM-KTO = BF-KTONR / 10.
           MOVE BF-DAT TO BM-BELDAT.
           IF BF-DAT < 1000000 ADD 19000000 TO BM-BELDAT.
           COMPUTE BM-GGKTO = WT-KTONR(1) / 10.
           MOVE BF-REN TO BM-BELNR.
           MOVE WS-DATUM TO BM-BUDAT.
           IF WS-DATUM < 1000000 ADD 19000000 TO BM-BUDAT.
           COMPUTE BM-MWST = WT-UST(BF-U1) * 100.
           MOVE 03 TO BM-STCD.
           MOVE 1 TO BM-SH.
           COMPUTE BM-BET = BF-BET * 100.
           COMPUTE BM-UST = BF-MW * 100.
           MOVE BM-BET TO BM-OPBET.
           MOVE BM-BUDAT(5:2) TO BM-PER(1:).
           MOVE SPACE TO BM-TEXT.
           MOVE "AR" TO BM-SYM.
           MOVE 77 TO BM-USER.
           MOVE 0 TO BM-BUKZ.
           MOVE "S" TO BM-GGBKZ.
           MOVE "A" TO BM-VBKZ.
           MOVE "*" TO BM-END.
           WRITE BM-BSATZ.
       Z.  EXIT.
