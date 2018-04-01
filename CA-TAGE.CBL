      $SET LINKCOUNT"192" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CA-DIVER.
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                         ENVIRONMENT-NAME IS ENV-SET
                         ENVIRONMENT-VALUE IS ENV-WERT.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
      *--------------------------> fuer Datumsaufbereitung im DATUM <--
       01  WT-DK    external.
           03  WR-TN                   PIC X(10)  OCCURS 7.
           03  WT-KON                  PIC 99     OCCURS 12.
      *--------------------------> fuer Datumsaufbereitung im DATUM <--
       01  WT-DAT.
           03  WH-J4                   PIC S9999   COMP-3.
           03  WK-D                    PIC S9999   COMP-3.
           03  WK-R                    PIC S9999   COMP-3.
           03  WK-S                    PIC S9      COMP-3.
           03  WI                      PIC 99      COMP.
           03  WX-PG                   PIC 99      COMP.
           03  IQ                      PIC 99      COMP.
           03  IP                      PIC 99      COMP.
           03  IX                      PIC 999     COMP.
           03  WM-CD                   PIC X(128).
           03  WE-DATUM                PIC 9999.99.99.
       01  WS-REG.
           03  WV-TGN                  PIC 9.
           03  WF-DATUM                PIC 9(8)    COMP.
           03  WX-JAHR                 PIC 99.
           03  WA-TAGE                 PIC 9(8)    COMP.
           03  WT-FTAG.
               05  FILLER              PIC 9(4)    COMP   OCCURS 4.
               05  FILLER              PIC 9(4)    COMP   VALUE 0101.
               05  FILLER              PIC 9(4)    COMP   VALUE 0106.
               05  FILLER              PIC 9(4)    COMP   VALUE 0501.
               05  FILLER              PIC 9(4)    COMP   VALUE 0815.
               05  FILLER              PIC 9(4)    COMP   VALUE 1026.
               05  FILLER              PIC 9(4)    COMP   VALUE 1101.
               05  FILLER              PIC 9(4)    COMP   VALUE 1208.
               05  FILLER              PIC 9(4)    COMP   VALUE 1224.
               05  FILLER              PIC 9(4)    COMP   VALUE 1225.
               05  FILLER              PIC 9(4)    COMP   VALUE 1226.
               05  FILLER              PIC 9(4)    COMP   VALUE 1231.
           03  WF-FTAG REDEFINES
               WT-FTAG                 PIC 9(4)    COMP  OCCURS 15.
           03  WT-FNAM.
               05 FILLER               PIC X(13)  VALUE "Ostermontag".
               05 FILLER               PIC X(13)  VALUE "Christi Hmf.".
               05 FILLER               PIC X(13)  VALUE "Pfingstmontag".
               05 FILLER               PIC X(13)  VALUE "Frohnleichnam".
               05 FILLER               PIC X(13)  VALUE "Neujahrstag".
               05 FILLER               PIC X(13)  VALUE "Hl. 3 Kînige".
               05 FILLER               PIC X(13)  VALUE "Staatsfeiertg".
               05 FILLER               PIC X(13)  VALUE "MariÑ Hmf.".
               05 FILLER               PIC X(13)  VALUE "Nationalf.Tag".
               05 FILLER               PIC X(13)  VALUE "Allerheiligen".
               05 FILLER               PIC X(13)  VALUE "Maria Empfg.".
               05 FILLER               PIC X(13)  VALUE "Hl. Abend".
               05 FILLER               PIC X(13)  VALUE "Christfest".
               05 FILLER               PIC X(13)  VALUE "Stephanitag".
               05 FILLER               PIC X(13)  VALUE "Silvester".
           03  WF-FNAM REDEFINES
               WT-FNAM                 PIC X(13)      OCCURS 15.
      *-------------------------------------------- Byte-Stream-Files <-
           03  WB-SATZ                 PIC X(400).
           03  WB-AC                   PIC X     COMP-X.
           03  WB-DE                   PIC X     COMP-X.
           03  WB-DV                   PIC X     COMP-X.
           03  WB-FH                   PIC X(4).
           03  WB-OFS                  PIC X(8)  COMP-x.
           03  WB-CNT                  PIC X(4)  COMP-x.
           03  WB-FLG                  PIC X     COMP-x.
           03  WB-STRNAM               PIC X(20).
           03  WB-NUM                  PIC 9(7),99-.
           03  WB-EAN                  PIC 9(14).
       COPY TIEXT.CPY.
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 99 GO Z                               *> nur Aufruf
               WHEN 90 PERFORM OPEN-STREAM
                       GO X
               WHEN 91
               WHEN 92
               WHEN 93
               WHEN 94
               WHEN 96                                  *> mit Umlauten
               WHEN 98
               WHEN 95 PERFORM EXC-TAB
                       GO X.
           IF WL-CA = 20 MOVE WO-TGN TO WV-TGN.             *> TIEDIFAC
           MOVE 1 TO WX.
           MOVE WH-PG TO WX-PG.
           MOVE 1 TO WH-PG.
           PERFORM FEIERTAGE.
           EVALUATE WL-CA
               WHEN 10                            *> mit  Silv. /Weihn.
               WHEN 12 PERFORM NEXT-DAY           *> ohne Silv. /Weihn.
               WHEN 20 PERFORM OB-FTG
               WHEN 50 PERFORM LIEF-DAT.
           MOVE 0 TO WH-PG.
      *    MOVE WV-DATUM TO WZ-DATUM.
      *    CALL "CAUP" USING "03DATUM" WH-CREG.
      *    IF WO-TGN = 0 ADD 2 TO WH-HILF.
           MOVE WX-PG TO WH-PG.
           IF WL-CA = 80 MOVE WT-FTAG TO WT-TX.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ************************************ autom. Feiertagsermittlung *
       FEIERTAGE SECTION.
       A.
      *    MOVE WM-DATUM TO WZ-DATUM.
           MOVE WZ-JAHR TO WH-NUM.
           MOVE WZ-JAHR TO WX-JAHR.
           PERFORM OSTERN.
      *---> Ostermon. * Chr. Himmelfahrt * Pfingstmon. * Frohnleichnam *
           MOVE 1 TO WY.
           ADD 29 WX GIVING WZ PERFORM TG.
           ADD 67 WX GIVING WZ PERFORM TG.
           ADD 78 WX GIVING WZ PERFORM TG.
           ADD 88 WX GIVING WZ PERFORM TG.
       Z.  EXIT.
      ********************************************* Tageermittlung ab *
       TG SECTION.
       A.  MOVE 3 TO WF.
       B.  IF WZ > WT-KON(WF) SUBTRACT WT-KON(WF) FROM WZ
               ADD 1 TO WF GO B.
           MULTIPLY WF BY 100 GIVING WF-FTAG(WY).
           ADD WZ TO WF-FTAG(WY).
           ADD 1 TO WY.
       Z.  EXIT.
      *********************************************** Ostern ermitteln *
       OSTERN SECTION.
       A.  IF WZ-DATUM = 0 GO Z.
           ADD 1900 WZ-JAHR GIVING WK-R.
           IF WZ-JAHR < 50 ADD 100 TO WK-R.
           DIVIDE WK-R BY 4 GIVING WH-J4 REMAINDER WR.
           MOVE 1 TO WK-S.
           IF WR NOT = 0 MOVE 0 TO WK-S.
           DIVIDE WK-R BY 19 GIVING WK-D REMAINDER WK-D.
           COMPUTE WK-D = 204 - (WK-D * 11).
           DIVIDE 30 INTO WK-D GIVING WK-D REMAINDER WK-D.
           IF WK-D = 29 OR WK-D = 28 SUBTRACT 1 FROM WK-D.
           COMPUTE WK-R = WK-R + WH-J4 + WK-D - 13.
           DIVIDE 7 INTO WK-R GIVING WK-R REMAINDER WK-R.
           COMPUTE WX = WK-D - WK-R.
           COMPUTE WK-D = WK-D + 87 + WK-S - WK-R.
           SUBTRACT WK-D FROM WZ-TAG GIVING WK-D.
           SUBTRACT 1 FROM WZ-MONAT GIVING WR.
       E.  IF WR NOT = 0 ADD WT-KON(WR) TO WK-D
               SUBTRACT 1 FROM WR GO E.
           DIVIDE 7 INTO WK-D GIVING WK-R REMAINDER WK-R.
           MULTIPLY -1 BY WK-R.
           IF WK-R < 1 ADD 7 TO WK-R.
           MOVE WR-TN(WK-R) TO WO-TAG.
           MOVE WK-R TO WO-TGN.
       Z.  EXIT.
      ************************************************ nÑchter Werktag *
       NEXT-DAY SECTION.
       A.  COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WV-DATUM).
           MOVE 15 TO WI.
           IF WL-CA = 12 MOVE 13 TO WI.
       C.  ADD 1 TO WA-TAGE.
           COMPUTE WS-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           MOVE WS-DATUM TO WZ-DATUM WV-DATUM.
           MOVE WZ-JAHR TO WX-JAHR.
           CALL "CAUP" USING "03DATPRF" WH-CREG.
           IF WO-TGN = 0 GO C.                          *> weil Sonntag
           MOVE 1 TO WY.
      *------------------------------------------> prÅfen ob Feiertag <-
       G.  MOVE WF-FTAG(WY) TO WZ-DATUM.
           MOVE WX-JAHR TO WZ-JAHR.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           IF WX-DATUM = WV-DATUM GO C.            *> Feiertag gefunden
           IF WY < WI ADD 1 TO WY GO G.
           MOVE 0 TO WY.
       Z.  EXIT.
      ******************************************** ob Feiertag gewÑhlt *
       OB-FTG SECTION.
       A.  MOVE 0 TO WH-HILF.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 15
               COMPUTE WF-DATUM =
                    20000000 + WF-FTAG(WY) + (WX-JAHR * 10000)
               IF WF-DATUM = WV-DATUM MOVE 1 TO WH-HILF   *> = Feiertag
                   MOVE WF-FNAM(WY) TO WV-MCODE
                   EVALUATE WF-FTAG(WY)
                       WHEN 1226 MOVE 3 TO WH-HILF
                       WHEN 1225 MOVE 2 TO WH-HILF
                   end-evaluate
               IF WV-TGN = 0 MOVE 9 TO WH-HILF.

       Z.  EXIT.
      ******************************************************************
       LIEF-DAT SECTION.
       A.  CALL "CAUP" USING "270410033000014" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lieferdatum:" AT VDU-LP.
       B.  IF WE-LIEFDAT = 0 MOVE WM-DATUM TO WE-LIEFDAT.
           MOVE WE-LIEFDAT TO WZ-DATUM WV-DATUM.
           CALL "CAUP" USING "03DATPRF" WH-CREG.
           ADD 216 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
               WO-TAG with highlight foreground-color 2.
           PERFORM OB-FTG.
           MOVE WE-LIEFDAT TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Lieferdatum" AT 2301.
           CALL "CAUP" USING "1102166006" WH-CREG.
           IF ESC or WOLI GO X.
           IF NOT RET GO B.
           IF WZ-DATUM = 0 GO B.
           MOVE WX-DATUM TO WV-DATUM WZ-DATUM.
           CALL "CAUP" USING "03DATPRF" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT VDU-LP " "
              WO-TAG with highlight foreground-color 2.
           PERFORM OB-FTG.
      *-------------------------------------> Datum nicht > + 29 Tage <-
           ACCEPT WA-TAGE FROM DATE.
           ADD 20000000 TO WA-TAGE.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WA-TAGE).
           ADD 29 TO WA-TAGE.
           COMPUTE WV-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE)
           IF WX-DATUM > WV-DATUM
               CALL "CAUP" USING "1303012580" WH-CREG
               DISPLAY "ACHTUNG Datumsfehler! < >" with highlight
                   foreground-color 4 AT 2401
               DISPLAY " <Einfg>= annehmen" AT 0000
               CALL "CAUP" USING "0024240000" WH-CREG
               IF EINF NEXT SENTENCE
               else STOP RUN.
           MOVE WX-DATUM TO WE-LIEFDAT.
           IF WH-HILF = 0 GO X.
           ADD 12 TO VDU-LP.
           EVALUATE WH-HILF
               WHEN 1 DISPLAY " Feiertag! " with highlight
                         foreground-color 4 AT VDU-LP "  < >"
               WHEN 2 ADD -4 TO VDU-LP
                      DISPLAY " Sonntag!      " with highlight
                         foreground-color 4 AT VDU-LP "  < >"
               WHEN 3 ADD -4 TO VDU-LP
                      DISPLAY " Sonn-/Feiertag!" with highlight
                      foreground-color 4 AT VDU-LP " < >".
           DISPLAY "< />= zurÅck, <ret>= ok " AT 2301.
           CALL "CAUP" USING "1002420000" WH-CREG.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WOLI or AUF GO A.
           DISPLAY "Lieferdatum: " AT 0310
              VDU-DATUM with highlight foreground-color 6.
       Z.  EXIT.
      ***************************************** Open Excel-Streamdatei *
       OPEN-STREAM SECTION.
       A.  MOVE WT-TX TO WB-STRNAM.
           CALL "CBL_DELETE_FILE" USING WB-STRNAM.
           MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           CALL "CBL_CREATE_FILE" USING
                                  WB-STRNAM WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS.
           MOVE WT-ADR TO WB-SATZ.                       *> öberschrift
           IF WB-SATZ(240:1) not = space MOVE WT-BEZ TO WB-SATZ(241:).
           PERFORM COUNT-WRITE.
           MOVE x"0D0A" TO WB-SATZ.
           PERFORM COUNT-WRITE.
           MOVE SPACE TO WB-SATZ.
       Z.  EXIT.
      ********************************************* Artikel-Exceldatei *
       EXC-TAB SECTION.
       A.  MOVE SPACE TO WM-CD.
           IF WX = 1 MOVE 0 TO IX.
           EVALUATE WL-CA
               WHEN 92 MOVE WT-TX TO WM-CD
                       GO Q
               WHEN 96 MOVE WT-TX TO WM-CD
                       PERFORM UMLAUTE
                       GO Q
               WHEN 93 PERFORM ZEILEND
                       GO Z
               WHEN 95 CALL "CBL_CLOSE_FILE" USING WB-FH
                       GO Z
      *---------------------------------------------> ean-14 ausgeben <-
               WHEN 94 MOVE WT-TX TO WB-EAN(1:)
                       PERFORM VARYING IQ FROM 1 BY 1 UNTIL IQ > 14
                             or WB-EAN(IQ:1) not = 0 CONTINUE
                       end-perform
                       MOVE 1 TO IP
                       PERFORM VARYING IQ FROM IQ BY 1 UNTIL IQ > 14
                           MOVE WB-EAN(IQ:1) TO WM-CD(IP:1)
                           ADD 1 TO IP
                       end-perform
                       GO Q
      *------------------------------------------> numerische Ausgabe <-
               WHEN 91 MOVE WH-WERT TO WB-NUM
                       IF WH-NEG not = 1 and WB-NUM(8:3) = ",00"
                           MOVE SPACE TO WB-NUM(8:3)
                       end-if
                       PERFORM VARYING IQ FROM 1 BY 1 UNTIL IQ > 10 or
                           WB-NUM(IQ:1) not = 0 CONTINUE
                       end-perform
                       IF WB-NUM(IQ:1) = "," ADD -1 TO IQ
                       end-if
                       MOVE 0 TO IP
                       PERFORM VARYING IQ FROM IQ BY 1 UNTIL IQ > 10
                           IF WB-NUM(IQ:1) not = SPACE ADD 1 TO IP
                               MOVE WB-NUM(IQ:1) TO WM-CD(IP:1)
                       end-perform
                       IF WB-NUM(11:1) = "-" MOVE WM-CD(1:) TO WT-TX
                                             MOVE "-" TO WM-CD(1:)
                                             MOVE WT-TX TO WM-CD(2:)
                                             ADD 1 TO IP
                       end-if
                       ADD 1 TO IX
                       MOVE WM-CD TO WB-SATZ(IX:)
                       ADD IP TO IX
                       MOVE ";" TO WB-SATZ(IX:)
                       GO Z
               WHEN 98 IF WS-DATUM not = 0
                           MOVE WS-DATUM TO WE-DATUM
                           MOVE WE-DATUM(9:2) TO WM-CD
                           MOVE "." TO WM-CD(3:)
                           MOVE WE-DATUM(6:3) TO WM-CD(4:)
                           MOVE WE-DATUM(1:4) TO WM-CD(7:)
                           ADD 1 TO IX
                           MOVE WM-CD TO WB-SATZ(IX:)
                           ADD 9 TO IX
                       end-if
                       ADD 1 TO IX
                       MOVE ";" TO WB-SATZ(IX:)
                       GO Z.
       Q.  PERFORM ANHANG.
      *-----------------------------------------> immer ";" anhÑngen <-
       W.  MOVE ";" TO WM-CD.
           PERFORM ANHANG.
       Z.  EXIT.
      ******************************************************************
       UMLAUTE SECTION.
       A.  INSPECT WM-CD REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹"
                    ALL "+" by "&".
       Z.  EXIT.
      ***************************************************** Zeilenende *
       ZEILEND SECTION.
       A.  PERFORM VARYING IX FROM 399 BY -1 UNTIL
               WB-SATZ(IX:1) not = SPACE CONTINUE.
           ADD 1 TO IX.
           MOVE x"0D0A" TO WM-CD(1:).
           MOVE WM-CD TO WB-SATZ(IX:).
           ADD 1 TO IX GIVING WB-CNT.
           INSPECT WB-SATZ REPLACING ALL ";" BY "|".
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
      *    PERFORM COUNT-WRITE.
           MOVE SPACE TO WB-SATZ.
       Z.  EXIT.
      ******************************************************* anhÑngen *
       ANHANG SECTION.
       A.  PERFORM VARYING IX FROM 399 BY -1 UNTIL IX < 1
               or WB-SATZ(IX:1) not = SPACE continue.
           ADD 1 TO IX.
       G.  MOVE WM-CD TO WB-SATZ(IX:).
       Z.  EXIT.
      ******************************************************************
       COUNT-WRITE SECTION.
       A.  PERFORM VARYING WB-CNT FROM 399 BY -1 UNTIL
               WB-SATZ(WB-CNT:1) not = SPACE CONTINUE.
           INSPECT WB-SATZ REPLACING ALL ";" BY "|".
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
       Z.  EXIT.
