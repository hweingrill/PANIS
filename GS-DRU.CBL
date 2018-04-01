
      ******************************************************************
       GS-DRU SECTION.
       A.  PERFORM ETIK-START.
           MOVE "^LH30,330" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F025,10^CF0,51,35^FD" TO DRE-SATZ.
           MOVE "ET-BEZ-A TO DRE-SATZ(22:).
           PERFORM FS-ZEI.
           MOVE "^LH30,400" TO DRE-SATZ.
           PERFORM DRUCK
           MOVE "^F020,10^A0,21^FD" TO DRE-SATZ.
           MOVE ET-ZU-A TO DRE-SATZ(18:).
           IF DRE-SATZ = SPACE PERFORM DRUCK
           else PERFORM FS-ZEI.
           MOVE "^LH30,420" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,50^A0,21^FD" TO DRE-SATZ.
           MOVE ET-ZU-B TO DRE-SATZ(18:).
           IF DRE-SATZ = SPACE PERFORM DRUCK
           else PERFORM FS-ZEI.
           MOVE "^LH30,440" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,50^A0,21^FD" TO DRE-SATZ.
           MOVE ET-ZU-C TO DRE-SATZ(18:).
           IF DRE-SATZ = SPACE PERFORM DRUCK
           else PERFORM FS-ZEI.
      *-----------------------------------------------> abgepackt am: <-
           MOVE "^LH30,490" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,199^A0,21^FDabgep. am:" TO DRE-SATZ.
           MOVE WS-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM(1:6) TO DRE-SATZ(30:).
           MOVE "20" TO DRE-SATZ(36:).
           MOVE VDU-DATUM(7:2) TO DRE-SATZ(38:).
           PERFORM FS-ZEI.
      *------------------------------------> mindestends haltbar bis: <-
           MOVE "^LH30,510" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F020,199^A0,21^FDmindestens haltbar bis:" TO DRE-SATZ.
           COMPUTE WA-DAT = FUNCTION INTEGER-OF-DATE(WS-DATUM).
           ADD ET-MHD TO WA-DAT.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-DAT).
           MOVE WV-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE VDU-DATUM(1:6) TO DRE-SATZ(43:).
           MOVE "20" TO DRE-SATZ(49:).
           MOVE VDU-DATUM(7:2) TO DRE-SATZ(51:).
           PERFORM FS-ZEI.
      *----------------------------------------------> EAN-Strichcode <-
           MOVE "^LH50,555" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^F025,450^BY3" TO DRE-SATZ.                *> Ean-Code
           PERFORM DRUCK.
           MOVE "^BEN,65,Y,N" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^FD9005793000030^FS" TO DRE-SATZ.
           MOVE ET-EAN TO DRE-SATZ(4:13).
           PERFORM DRUCK.
           MOVE "^PQ" TO DRE-SATZ.
           MOVE WD-ET(3:1) TO DRE-SATZ(4:).
           MOVE ",1,,Y" TO DRE-SATZ(5:).
           IF WD-ET > 9 MOVE WD-ET(2:2) TO DRE-SATZ(4:)
               MOVE ",1,,Y" TO DRE-SATZ(6:).
           IF WD-ET > 99 MOVE WD-ET(1:3) TO DRE-SATZ(4:)
               MOVE ",1,,Y" TO DRE-SATZ(7:).
           PERFORM DRUCK.
           MOVE "^MCN^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
           MOVE "^XA^MCY^XZ" TO DRE-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
