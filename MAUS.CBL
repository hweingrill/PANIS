      $SET LINKCOUNT"192" ANS85 BOUND
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        MAUS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-VL               PIC 99.
               05  WL-VP               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-AZ               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY "WHCREG.CPY".
       01  WF-REG.
           03  WH-ZEIT.
               05  WH-STD              PIC 99.
               05  WH-A1               PIC X   VALUE ":".
               05  WH-MIN              PIC 99.
               05  WH-A2               PIC X   VALUE ":".
               05  WH-SEC              PIC 99.
               05  WH-A2               PIC X   VALUE ":".
               05  WH-HS               PIC 999.
           03  MO-HD                   PIC 9(8)   COMP-X.
           03  MO-BUT                  PIC 99     COMP-X.
           03  MO-LP.
               05 MO-L                 PIC 9(4)   COMP-X.
               05 MO-P                 PIC 9(4)   COMP-X.
           03  MO-RT                   PIC 99     COMP-X.
           03  MO-ED.
               05 MO-ETY               PIC 9(4)   COMP-X.
               05 MO-ETIM              PIC 9(8)   COMP-X.
               05 MO-EL                PIC 9(4)   COMP-X.
               05 MO-EP                PIC 9(4)   COMP-X.
           03  MO-FUN                  PIC 99     COMP-X.
           03  MO-PAR                  PIC 99     COMP-X.
       01  MO-DET.
           03  MO-X                    PIC 9999   COMP-X.
           03  MO-Y                    PIC 9999   COMP-X.
           03  MO-PS                   PIC 9999   COMP-X.
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACES UPON CRT.
           CALL "CAUP" USING "9901012480000" WH-CREG.
      *    MOVE 64 TO MO-FUN.
      *    MOVE 1 TO MO-PAR.
      *    CALL X"AF" USING MO-FUN MO-PAR.
      *    MOVE 67 TO MO-FUN.
      *    CALL X"AF" USING MO-FUN MO-DET.
      *    DISPLAY MO-X AT 1010 " " MO-Y " " MO-PS.
      *----------------------> dieses Programm l„uft nur im RUN-Modus <-
           MOVE 2 TO MO-BUT.
           MOVE 0 TO MO-HD.
           CALL "CBL_INIT_MOUSE" USING MO-HD MO-BUT.
           CALL "CBL_SHOW_MOUSE" USING MO-HD.
       B.  CALL "CBL_GET_MOUSE_POSITION" USING MO-HD MO-LP.
           DISPLAY MO-L AT 0101 MO-P AT 0204.
           IF MO-L NOT = 0 AND MO-P NOT = 0 GO B.
       C.  CALL "CBL_READ_MOUSE_EVENT" USING MO-HD MO-ED MO-RT.
           EVALUATE MO-ETY
               WHEN 1 DISPLAY "mouse moved             " at 1010
               WHEN 2 DISPLAY "button 1                " at 1010
               WHEN 3 DISPLAY "mouse moved + button 1  " at 1010
               WHEN 4 DISPLAY "button 2                " at 1010
               WHEN 5 DISPLAY "mouse moved + button 2  " at 1010
               WHEN 6 DISPLAY "button 1+2              " at 1010
               WHEN 7 DISPLAY "mouse moved + button 1+2" at 1010.
           IF MO-ETIM NOT = 0
               DISPLAY MO-ETIM AT 0140 "-" MO-ETY "-" MO-EL " " MO-EP
               DIVIDE 1000 INTO MO-ETIM GIVING MO-ETIM REMAINDER WH-HS
               DIVIDE 3600 INTO MO-ETIM GIVING WH-STD
               COMPUTE MO-ETIM = MO-ETIM - (WH-STD * 3600)
               DIVIDE 60 INTO MO-ETIM GIVING WH-MIN
               COMPUTE WH-SEC = MO-ETIM - WH-MIN * 60
               DISPLAY WH-ZEIT(1:8) AT 0350.
           IF MO-EL = 24 AND MO-EP = 79 STOP RUN.
           GO C.
           CALL "CBL_HIDE_MOUSE" USING MO-HD.
       Z.  EXIT.
