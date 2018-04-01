      $SET LINKCOUNT"512" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PANIS.
      ************************************************* Record-Locking *
      *  set COBSW=+B = CALL "CALOG" USING "50SET+B0" WH-CREG
      *  "B0" bewirkt bei seq. Lesen die Freigabe des Records
      *       und kann daher weiter gelesen werden.
      *  Drucker Kyocera: 1H = Lade oben, 4H = Lade unten siehe Word"
      *-----------------------------------------------------------------
      *  WM-VER: 3 = Loh, we-lfs: 1 = Kern
      *  Datenbergabe: HOGAST-Dateien via BLAT
      *                 DUSMANN-Dateien via FTP (dusi.bat + dusd.bat)
      ******************************************************************
      *  PANIS    CBL    Hauptprogramm
      *  PANDEBAN CBL    Wartung Debitoren
      *  PANARTAN CBL    Wartung Artikel
      *  PANANZ   CBL    Anzeigen Debitoren / Artikel
      *  PANLIST  CBL    diverse Listungen
      *  PANLAG   CBL    Lagerbuchhaltung - Auslieferung / Produktion
      *  PANVORL  CBL    Vorlaufprogramm
      *  PANDAU   CBL    Wartung Dauerbestellungen / Sonderkonditionen
      *  PANSTAM  CBL    Wartung Stammdaten
      *  PACO     CBL    Konvertierung PANIS-RM auf MFC
      *  PARCO    CBL    Konvertierung PAREZ-RM auf MFC
      *  PANLFDRU CBL    Lieferscheindruck
      *  PANLIEF  CBL    Lieferscheinprogramm
      *  PANTUED  CBL    Pruef-, Tourback-, Uebfaktprogramm
      *  PANRECH  CBL    Rechnungsprogramm
      *  PANRAST  CBL    Rasterprogramm
      *  PANBSKOM CBL    Bildschirmkommissionierung
      *  CAUP     CBL    allgem. Unterprogramme
      ******************************************************************  
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SECDAT     ASSIGN TO DATEI
                             ORGANIZATION SEQUENTIAL.
           COPY PANSEDEB.CPY.
           COPY PANSEKON.CPY.
           COPY PANSEART.CPY.
           COPY PANSEFAK.CPY.
           COPY PANSELFS.CPY.
           COPY PANSEDAU.CPY.
           COPY PANSESTA.CPY.
           SELECT BUCHFAKT   ASSIGN TO "PANISFIB.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BF-KEY
                             ALTERNATE KEY BF-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT KONSFIBU   ASSIGN TO WN-FKON
                             ORGANIZATION RELATIVE, ACCESS DYNAMIC
                             RELATIVE KEY WH-KEY
                             FILE STATUS WF-STATUS.
           SELECT BUCHUNG    ASSIGN TO "FIBUEBER.DAT"
                             ORGANIZATION INDEXED, ACCESS DYNAMIC
                             RECORD KEY BU-KEY
                             ALTERNATE KEY BU-RKEY DUPLICATES
                             FILE STATUS WF-STATUS.
           SELECT SUMMEN     ASSIGN TO DISK "PANESUMM.DAT"
                             ORGANIZATION INDEXED ACCESS DYNAMIC
                             RECORD KEY SU-KEY
                             ALTERNATE RECORD KEY SU-AKEY
                             FILE STATUS IS WF-STATUS.
           SELECT PROTOK     ASSIGN TO DISK "PANEPROT.DAT"
                             ORGANIZATION LINE SEQUENTIAL
                             FILE STATUS IS WF-STATUS.
           SELECT DRUCKER    ASSIGN TO PRINTER WH-DRUNAM
                             FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY PANDEBI.CPY.
       COPY PANKONS.CPY.
       COPY PANEBUCH.CPY.
       COPY PANFAKT.CPY.
       COPY PANFDART.CPY.
       COPY PANDAU.CPY.
       COPY PANSTAT.CPY.
       COPY PANLIEF.CPY.
       COPY PANSUMM.CPY.
       COPY PANFDBUC.CPY.
      ************************************************ Security-Datei *
       FD  SECDAT.
       01  SEC-SATZ                    PIC X(128).
      *****************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ                    PIC X(134).
      ******************************************************************
       WORKING-STORAGE SECTION.
       COPY "WHCREG.CPY".
       01  WT-CALL                     PIC X(8).
       01  WH-PANEXT      external.
           03  WK-EAN                  PIC 9(8)      COMP.
           03  WK-MON                  PIC 9(8)      COMP.
           03  WA-DAT                  PIC 9(8)      COMP.
           03  WE-MAXDAT               PIC 9(8)      COMP.
           03  WE-MINDAT               PIC 9(8)      COMP.
           03  WH-KEY                  PIC 9(5).
           03  WH-BUKEY                PIC 9(5).
      *------------------> Version, Kopfdruck, Preis/Lfs., Rabattvar. <-
           03  WK-M.
               05 WM-VER               PIC 99        COMP.
               05 WM-KO                PIC 99        COMP.
               05 WM-PR                PIC 99        COMP.
               05 WM-RB                PIC 99        COMP.
           03  WE-LFS                  PIC 99        COMP.
           03  WN-FKON                 PIC X(35).
           03  WN-BUEB                 PIC X(35).
           03  WE-ARSYM                PIC 99        COMP.
           03  WE-GSSYM                PIC 99        COMP.
           03  WE-PANIS                PIC 99        COMP.
           03  WM-DRU                  PIC 99        COMP.
           03  WE-PARAM       OCCURS 10.
               05 WE-DRU               PIC 99        COMP.
               05 WE-KOPF              PIC 99        COMP.
               05 WE-WO                PIC 99        COMP.
               05 WE-TYP               PIC 99        COMP.
           03  WE-STG                  PIC X(40)    OCCURS  10.
           03  WH-P                    PIC 99        COMP.
      *---------------------------> Key Zerlegung wg. concatinted Key <-
           03  WQ-AKEY.                                      *> LF-AKEY
               05 WQ-DTKEY.
                  07 WQ-DATUM          PIC 9(8)       COMP.
                  07 WQ-TOUR           PIC 99         COMP.
                  07 WQ-SUB            PIC 9999       COMP.
               05 WQ-PRIME.
                  07 WQ-KTONR          PIC 9(6)       COMP.
                  07 WQ-NUM            PIC 9(6)       COMP.
               05 WQ-SORT              PIC X(10).
               05 WQ-ARNUM             PIC 9(4)       COMP.
           03  WP-KEY.                                       *> LF-KEY
               05 WP-PRIME.
                  07 WP-KTONR          PIC 9(6)       COMP.
                  07 WP-NUM            PIC 9(6)       COMP.
               05 WP-ARNUM             PIC 9(4)       COMP.
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012580000" WH-CREG.
           CALL "CADECL" USING "99AUFRUF" WH-CREG.
      *----------------------------------------> autom. Konvertierung <-
      *    CALL "PANDECV" USING "99" WH-CREG        *> konv. Debitoren
      *        ON EXCEPTION NEXT SENTENCE.
      *    CANCEL "PANDECV".
      *    CALL "PANFACV" USING "10" WH-CREG        *> konv. Faktdat
      *        ON OVERFLOW NEXT SENTENCE.
      *    CANCEL "PANFACV".
      *------ 15.08.2012 -----> lt. Prfg. bei K”nig u. Altenburger ok <-
           CALL "PANCVART" USING "10ART" WH-CREG
               ON OVERFLOW NEXT SENTENCE.
           CALL "PANCVART" USING "20LFS" WH-CREG
               ON OVERFLOW NEXT SENTENCE.
           CALL "PANCVART" USING "30LFS" WH-CREG
               ON OVERFLOW NEXT SENTENCE.
           CANCEL "PANCVART".
           OPEN I-O FAKTDAT.
           OPEN I-O LFSCHEIN.
           OPEN I-O ARTIKEL.
           OPEN I-O DEBITOR.
           OPEN I-O DAUER.
           OPEN I-O KONSTANT.
       B.  CALL "PANVORL" USING "99VORLAUF" WH-CREG.
           CANCEL "PANVORL".
           IF KIST CALL "PANBSKOM" USING "10BSKOM" WH-CREG
                   CANCEL "PANBSKOM" GO C.
           IF WH-PG = 8 CALL "PANARCH" USING "10ARCHIV" WH-CREG
                        CANCEL "PANARCH" GO C.
           IF WH-PG = 4 CALL "CADRUCK" USING "40DRUCK" WH-CREG
                        CANCEL "CADRUCK" GO C.
           IF WH-PG = 99 CALL "PANDEBAN" USING "99SERIE" WH-CREG
               CANCEL "PANDEBAN" GO C.
           IF WH-PG = 1 GO G.
           IF WH-PG = 2 CALL "PANRECH" USING "10FAKT" WH-CREG
                CANCEL "PANRECH"
                IF WH-PG not = 7 and WH-PG not = 8
                     CALL "PANARCH" USING "11REARC" WH-CREG
                     CANCEL "PANARCH" GO C
                else CALL "PANVORL" USING "88TAGSUM" WH-CREG
                     CANCEL "PANVORL" GO C.
           IF WH-PG = 5 CALL "PANRAST" USING "10RAST" WH-CREG
                        CANCEL "PANRAST" GO C.
           IF WH-PG = 3 CALL "PANDAU" USING "10ANLPR" WH-CREG
                        CANCEL "PANDAU" GO C.
           IF WH-PG = 6 CALL "PANSTAM" USING "01STAM" WH-CREG
                        CANCEL "PANSTAM" GO C.
           IF WH-PG = 7 CALL "PANLIST" USING "01ALL" WH-CREG
                        CANCEL "PANLIST" GO C.
           IF WH-PG = 10 CALL "PANLIEF" USING "20FAHR" WH-CREG
                CANCEL "PANLIEF"
                IF NOT ESC CALL "PANRECH" USING "30LFRECH" WH-CREG
                           CANCEL "PANRECH"
                           CALL "PANARCH" USING "11REARC" WH-CREG
                           CANCEL "PANARCH"
                end-if GO C.
           IF WH-PG = 19 CALL "PANLOE" USING "19LOE" WH-CREG GO C.
       C.  MOVE 9 TO WH-PG GO B.
      *---------------------> Verzweigung aus Lieferscheinbearbeitung <-
       G.  CALL "PANLIEF" USING "30LFWAHL" WH-CREG.
           CANCEL "PANLIEF".
           IF WH-PG = 11 CALL "PANLFDRU" USING "50TAGTOUR" WH-CREG
                         CANCEL "PANLFDRU" GO C.
           IF WH-PG = 12 CALL "PANLFDRU" USING "60HOFMODE" WH-CREG
                         CANCEL "PANLFDRU" GO C.
           IF WH-PG = 13 CALL "PANLFDRU" USING "70HOFLIST" WH-CREG
                         CANCEL "PANLFDRU" GO C.
           IF WH-PG = 0 GO C.
           IF WH-PG = 10 CALL "PANLAG" USING "10LAGER" WH-CREG
               CANCEL "PANLAG" GO G.
           CALL "PANTUED" USING "30UEBFAK" WH-CREG.
           CANCEL "PANTUED".
           CALL "PANARCH" USING "11REARC" WH-CREG.
           CANCEL "PANARCH".
           IF WH-PG = 4; IF WY = 10
               CALL "PANLFDRU" USING "01LFDRU" WH-CREG
               CANCEL "PANLFDRU" GO G.
           IF WH-PG = 6 OR WH-PG = 7;
               IF NOT ESC CALL "PANLFDRU" USING "01LFDRU" WH-CREG
                   CANCEL "PANLFDRU" GO G.
           GO C.
       Z.  EXIT.
