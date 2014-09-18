*----------------------------------------------------------------------*
*   RSSTAT20: Anzeige der Statistik Einzelsaetze                       *
*----------------------------------------------------------------------*
*   Autor: Stefan Klimt, Guido Derwand, Daniela Hören                  *
*----------------------------------------------------------------------*
*
REPORT RSSTAT20 MESSAGE-ID S1 NO STANDARD PAGE HEADING LINE-SIZE 180.
*
TABLES: STATS_CUML,                    "Ergebnisstruktur Einzelsatz
        STA1,                          "Statistikanzeige Summensatz
        STA2,                          "Statistikanzeige Einzelsatz
        STATL,                         "Selection data single record
*       statr,                         "Rohdaten Statistikeinzelsatz
        STATB,                         "Rohdaten Tab.Statistikeinzelsatz
        TSTCT,                         "Text für Transaktionen
        D020T,                         "Text für Screens
        TADIR,                         "Objekttabelle für Textbeschaf.
        TDEVCT,                        "Text für Entw.klassen
        TRCLT,                         "Text für Reportklassen
        TRDIR,                         "Reportinformationen
        EUDB,                          "Enthält Text der Funktionskeys
        MONI,                          "Performance Datenbank MONI
        MONI_V01,                      "Streichview zu MONI
*       pfnorm,
        SAPWLPFNRM.                    "Version 2: Normal statistic rec.


data : begin of it_tab occurs 0,
      name like trdir-name,
      include like D010INC-include,
     end of it_tab.

constants : repname(2) value 'Z%'.
*
INCLUDE ZRSSTATDA.
*INCLUDE RSSTATDA .
INCLUDE ZRSSTATCS.
*INCLUDE RSSTATCS.                      "Daten für CUA Profil
INCLUDE ZRSSTATSU.
*INCLUDE RSSTATSU.                      "Summentabellen Tag
INCLUDE ZRSSTATD2.
*INCLUDE RSSTATD2.                      "Data especially for this report
INCLUDE ZRSSTATPD.
*INCLUDE RSSTATPD.                      "Data for Reorg parameters
INCLUDE ZRSSTATX2.
*INCLUDE RSSTATX2.                      "some decl. for version 2 stat.

*--------Selektionsparameter fuer Anzeige der Einzelsaetze--------------
*
PARAMETERS:      RUSER      LIKE SAPWLPFNRM-ACCOUNT,      "User
                 RTCODE     LIKE SAPWLPFNRM-TCODE,        "Tcode
                 RPROGRAM   LIKE SAPWLPFNRM-REPORT default 'Z*',
"Report
                 TASKTYPE,                                "Tasktyp
                 RSCREEN    LIKE SAPWLPFNRM-DYNPRONR,     "Dynpro
                 RWPID(2).                                "Work Process
SELECTION-SCREEN ULINE.
PARAMETERS:      RRSPTI        LIKE SAPWLPFIDX-MAX_RESPTI,"Resp. time
                 RDBTI         LIKE SAPWLPFIDX-MAX_DBTI,  "DB time
                 RCPUTI        LIKE SAPWLPFIDX-MAX_CPUTI, "CPU time
                 RKBYTE        TYPE I,                    "Bytes trans
                 RCHG          TYPE I.                    "Phys.Aenderg.
SELECTION-SCREEN ULINE.
PARAMETERS:      RSTARTTI      LIKE SY-UZEIT,             "ab Uhrzeit
                 RDAY          LIKE SY-DATUM,             "ab Tag
                 RENDTI        LIKE SY-UZEIT,
                 RENDDAY       LIKE SY-DATUM.
SELECTION-SCREEN ULINE.


PARAMETERS:      RPATH         LIKE SAPWLPSTRC-FILENAME, "Stat.Dateinam
                 RMAXCNT       TYPE I DEFAULT 50000.       "Anzahl
  SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-BL1.
  SELECTION-SCREEN COMMENT /1(30) TEXT-COM.
     PARAMETERS: P_DIST    as checkbox,
                 P_INCL    as checkbox,
                 p_p1 as checkbox ,
                 p_p2 as checkbox.
  SELECTION-SCREEN END OF BLOCK BL1.

"anzuz. Objekt
*
*---Initialwerte--------------------------------------------------------
INITIALIZATION.
  PERFORM FILL-PFAD.                   "Stat-Datei-Pfad, Systemid laden
  IMPORT STATL FROM MEMORY             "Externe Selektionsvorgaben holen
               ID   'RSSTAT20_STATL'.
  IF SY-SUBRC NE 0.                    "Falls vorhanden:
    CLEAR STATL.
  ENDIF.
  PERFORM TRANSFER_SELECTION_DATA.     "Selektionsdaten uebernehmen
  SET TITLEBAR '001'.
  BI_FLAG      = ' '.                  "Keine BI-Selektion
  HEADLINESIZE = 4.                    "Vier Kopfzeilen bei Hitlisten
  STA2-SORTNAME = 'time'.              "Sortierkriterium anfangs
  SAVECUCOL    = 15.                   "Cursorposition fuer Columnsort
  STATUS       = 'SING'.               "Status for single record displ.
  INDEXAPPEND  = 'N'.                  "Indexaufbau noch nicht noetig
*
*-Hole Profilparameter stat/tabrec--------------------------------------
*
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'stat/tabrec'
                     ID 'VALUE' FIELD PARVALUE.
  IF SY-SUBRC > 0.
    TABREC = 0.
  ELSE.
    TABREC = PARVALUE.
  ENDIF.
*
data : l_cnt type i,
       l_cnt1 type i.
*
START-OF-SELECTION.
*----Import Selektionsparameter-----------------------------------------
  if p_p1 eq '' and p_p2 eq ''.
  PERFORM RETRANS_SELECTION_DATA.      "STATL mit Selektionsdaten fuelle
    PERFORM SELECT-COMMAND USING 'INIT'. "Initialisieren
  else.
    if p_p1 eq 'X'.
      select a~PROG b~include  into table it_tab from
        D010SINF as a inner join  D010INC as b
         on a~PROG = b~MASTER
        where  a~prog like  repname and
              a~SUBC eq '1' and
              b~include in ( select prog from D010SINF
                             where  prog = b~include and
                                    SUBC = 'I' and
                                    prog like 'Z%' ).

    elseif p_p2 eq 'X'.
      select distinct include  into table it_tab
             from D010INC where INCLUDE like repname.
    endif.
  endif.
*
if p_p1 eq 'X'.
  sort it_tab by name.
  format color 1.
  write :/(30) 'Program Name',30(30) 'Includes'.
  skip 1.
  format reset.
  loop at it_tab.
    at new name.
      write :/(30) it_tab-name.
      l_cnt = l_cnt + 1.
    endat.
    write :/(30) '',30(30) it_tab-include.
    l_cnt1 = l_cnt1 + 1.
    at end of name.
     format color 2.
*    uline at 1(60).
    write :/
 '__________________________________________________________'.
    format reset.
    endat.
  endloop.
         write :/ 'No of Programs',l_cnt color 3,
                'No of Includes', l_cnt1 COLOR 3.

endif.
if p_p2 eq 'X'.
sort it_tab by name.

format color 1.
  write :/(30) 'Include Name'.
  skip 1.
  format reset.
  loop at it_tab.
      write :/(30) it_tab-name.
 l_cnt1 = l_cnt1 + 1.
  endloop.
         write :/ 'No of Programs',l_cnt color 3,
                'No of Includes', l_cnt1 COLOR 3.
endif.
*
*--Line Selection-------------------------------------------------------
AT LINE-SELECTION.
* First check if a transaction ID was selected via double click
  PERFORM SET_GET_TID USING TID 'G'.
  IF TID IS INITIAL.
* Now check if something else can be done (skip to another line)
    IF NOT ( SCOPEFLAG IS INITIAL ).
      CLEAR SY-TABIX.
      READ CURRENT LINE.
    ENDIF.
    CHECK SY-TABIX > 0.
    IF INDEX     > 0.                    "Groesse der Tabelle STATS
      IF STEUER = 'DB5F' OR              "Tabellenanzeige
         STEUER = 'ALRC'.
        PERFORM FILL-TABS-FROM-TABSLIST. "Füllen Minitabelle TABS
      ENDIF.
*   perform display-hitlist.           "Pickup-Stufe ausgeben
      PERFORM SELECT-COMMAND USING STEUER.
    ELSE.
      MESSAGE S006 WITH MYNAME.      "cpuid.     "Kein Eintrag vorhanden
    ENDIF.
  ELSE.
* Jump into the ALE display report
    SELECT SINGLE * FROM TRDIR WHERE NAME = 'RBDCCMS1'.
    "The report belongs to the application part and is therefore not
    "available in all systems.
    IF SY-SUBRC = 0.
      SUBMIT RBDCCMS1 WITH ARFCTID = TID AND RETURN.
    ENDIF.
  ENDIF.
*
*
*
*--Anwahl von PF-Tasten-------------------------------------------------
AT USER-COMMAND.
  PERFORM SELECT-COMMAND USING SY-UCOMM.
*


*---Zeitpunkt TOP-OF-PAGE-----------------------------------------------
TOP-OF-PAGE.
  PERFORM HEAD-HIT1.
*
*
*
*---Zeitpunkt TOP-OF-PAGE DURING LINE-SELECTION-------------------------
TOP-OF-PAGE DURING LINE-SELECTION.

  IF SY-LSIND = 0.
    PERFORM HEAD-HIT1.
  ENDIF.
*
*
*--------------FORMS----------------------------------------------------
*
*
FORM SELECT-COMMAND USING VALUE(CODE).
  DATA REC_LENGTH TYPE I.
  DATA POSITION   TYPE I.
  DATA ROW        LIKE SY-STARO."for saving the list position
  DATA COLUMN     LIKE SY-STACO.
  DATA PAGE       LIKE SY-CPAGE.
  DATA LSIND     LIKE SY-LSIND.

* save list position

  ROW    = SY-STARO.
  COLUMN = SY-STACO.
  PAGE   = SY-CPAGE.
  LSIND  = SY-LSIND.

* execute selected command

  CASE CODE.
*
* --- Namespace extension
    WHEN 'NAME'.
      NAMELENGTH_FLAG = 1 - NAMELENGTH_FLAG.
      CALL FUNCTION 'SAPWL_WORKLOAD_NAME_LENGTHS'
           EXPORTING
                NAMELENGTH    = NAMELENGTH_FLAG
           IMPORTING
                REPORT_LENGTH = NAMELENGTH_REPORT
                TCODE_LENGTH  = NAMELENGTH_TCODE
                TABLE_LENGTH  = NAMELENGTH_TABLE
                DEVC_LENGTH   = NAMELENGTH_DEVC.
*               ENTRY_ID_LENGTH =
      SY-LSIND = 0.
      PERFORM WRITE-DISP.
      SCROLL LIST INDEX LSIND TO: PAGE   PAGE
                                  LINE   ROW,
                                  COLUMN COLUMN.
*
*---Neue Anzahl anzuzeigender Sätze auswählen---------------------------
    WHEN 'SIZE'.
      PERFORM GET_NUMBER_OF_RECS_DISPLAYED USING RMAXCNT2.
      IF RMAXCNT2 NE RMAXCNT.
        RMAXCNT = RMAXCNT2.
        PERFORM STATUS_0010_SET.       "Basisstatus setzen
        SY-LSIND = 0.
        PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
        PERFORM FILL-STATS USING 'F'.    "oder auch 'B' ???
      ENDIF.
*
*---Statistik über Lesevorgänge in der Statistik anzeigen---------------
    WHEN 'MISS'.
      PERFORM SHOW_RECORD_STATISTIC.
*---Initiale Vorgaenge beim erstmaligen Anzeigen------------------------
    WHEN 'INIT'.
      OUTPUT = 'N'.
      STEUER = 'DISP'.                 "Standardanzeige bei Pickup
      SET TITLEBAR '001'.
      PERFORM STATUS_0010_SET.         "Basisstatus setzen
      PERFORM PREPARE_SELECTION.       "Seletionsdatenen aufbereiten
      PERFORM CHECK-PARAMETERS.        "Parameter ueberpruefen
      PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
      PERFORM FILL-STATS USING 'F'.    "Laden interne Statistik-Tabelle
*
*---Neue Selektion durch veraenderung der Auswahlzeile------------------
    WHEN 'NEWT'.                       "Neue Zeit gesetzt (explizit)
      IF SY-CUROW <> MODLINE.          "Nicht auf richtiger Zeile pos.
        READ LINE MODLINE.
        IF SY-SUBRC NE 0.
          MESSAGE E026.
        ENDIF.
      ENDIF.
*-----Endzeit
      SEL = SEL2 = STATL.
      STRING = SY-LISEL+1(8).
      IF STRING(1) = '!'.
        STRING = ' '.
      ENDIF.
      IF STRING(8) = '*       '.
        STRING = '000000'.
        SEL-SDAT = BASISDAY.
      ENDIF.
      IF STRING CA ':'.
        TRANSLATE STRING USING ': '.
        CONDENSE STRING NO-GAPS.
      ENDIF.
      IF STRING <> SPACE.
        IF STRING CO ' 0123456789'.    "avoid convert_no_numbers
          IF STRING <> SEL2-STIME.
            SEL-STIME = STRING(6).
          ENDIF.
        ELSE.
          MESSAGE E058.
*   Please check the time format
        ENDIF.
      ELSE.
        STRING = '000000'.
        SEL-SDAT = BASISDAY.
      ENDIF.
*-----Tcode
      STRING = SY-LISEL+10(NAMELENGTH_TCODE).
      IF STRING(1) = '!'.
        CLEAR STRING.
      ENDIF.
      TRANSLATE STRING TO UPPER CASE.
      IF STRING <> SEL2-STCOD.
        CONDENSE STRING NO-GAPS.
        SEL-STCOD = STRING(NAMELENGTH_TCODE).
        IF NAMELENGTH_FLAG < 1.
          CONCATENATE SEL-STCOD '*' INTO SEL-STCOD.
        ENDIF.
      ENDIF.
*-----Reportname
      POSITION = 13 + NAMELENGTH_TCODE.
      STRING = SY-LISEL+POSITION(NAMELENGTH_REPORT).
      IF STRING(1) = '!'.
        CLEAR STRING.
      ENDIF.
      TRANSLATE STRING TO UPPER CASE.
      IF STRING <> SEL2-SPROGRAM.
        SEL-SPROGRAM = STRING(NAMELENGTH_REPORT).
        IF NAMELENGTH_FLAG < 1.
          CONCATENATE SEL-SPROGRAM '*' INTO SEL-SPROGRAM.
        ENDIF.
      ENDIF.
*-----Tasktyp (Kurzbezeichnung)
      POSITION = 14 + NAMELENGTH_REPORT + NAMELENGTH_TCODE.
      STRING = SY-LISEL+POSITION(1).
      IF STRING(1) = '!'.
        STRING = '*'.
      ENDIF.
      TRANSLATE STRING TO UPPER CASE.
      IF STRING <> SPACE AND STRING <> SEL2-STASK.
        SEL-STASK = STRING(1).
      ENDIF.
*-----Screen
      POSITION = 16 + NAMELENGTH_REPORT + NAMELENGTH_TCODE.
      STRING = SY-LISEL+POSITION(4).
      IF STRING(1) = '!' OR STRING IS INITIAL.
        STRING = '*'.
      ELSEIF STRING CO ' 0123456789'.
        SY-TFILL = STRING.
        WRITE SY-TFILL TO STRING USING EDIT MASK 'RR____'.
        TRANSLATE STRING USING ' 0'.
      ENDIF.
      IF STRING <> SEL2-SSCREEN.
        SEL-SSCREEN = STRING(4).
      ENDIF.
*-----Workprozess
      POSITION = 21 + NAMELENGTH_REPORT + NAMELENGTH_TCODE.
      STRING = SY-LISEL+POSITION(2).
      CONDENSE STRING NO-GAPS.         " Abfangen der Eingabe ' *', ' !'
      IF STRING(1) = '!' OR STRING = '*' OR STRING IS INITIAL.
        STRING = '*'.
      ENDIF.
      IF STRING <> SEL2-SWPID.
        IF STRING CO ' 0123456789'.
          SY-TFILL = STRING.
          WRITE SY-TFILL TO SEL-SWPID USING EDIT MASK 'RR__'.
        ELSE.
          SEL-SWPID = STRING.
        ENDIF.
      ENDIF.
*-----Benutzer
      POSITION = 24 + NAMELENGTH_REPORT + NAMELENGTH_TCODE.
      STRING = SY-LISEL+POSITION(12).
      IF STRING(1) = '!'.
        CLEAR STRING.
      ENDIF.
      IF STATL-SSELTIME1 <> 'X'.       "Bei Terminalid statt Account
        TRANSLATE STRING TO UPPER CASE."Nur bei Account Grossschreib.
      ENDIF.
      IF STRING <> SEL2-SBENU.
        SEL-SBENU = STRING(12).
      ENDIF.
*-----Wurde neu selektiert ?
      IF SEL <> SEL2.
        STATL    = SEL.
        RSTARTTI = STATL-STIME.
        IF RSTARTTI < '210000'.
          RENDTI  = RSTARTTI + 10800.
          RENDDAY = RDAY.
        ELSE.
          RENDTI  = RSTARTTI - 75600.
          RENDDAY = RDAY + 1.
        ENDIF.
        STATL-SDAT     = RDAY.
        STATL-STIME    = RSTARTTI.
        STATL-SENDDAT  = RENDDAY.
        STATL-SENDTIME = RENDTI.
        PERFORM STATUS_0010_SET.        "Basisstatus setzen
        PERFORM TRANSFER_SELECTION_DATA."Selektiondaten uebernehmen
        PERFORM PREPARE_SELECTION.      "Selektionsdaten aufbereiten
        PERFORM CHECK-PARAMETERS.       "Parameter ueberpruefen
      ENDIF.
      PERFORM STATUS_0010_SET.
      SY-LSIND = 0.
      READ_CONTINUE_OFFSET = -1.       "don't start at last position.. ?
      PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
      PERFORM FILL-STATS USING 'F'.
*
*---Neue Selektion der anzuzeigenden Einzelsaetze-----------------------
    WHEN 'NEWR'.                       "Neue Record-Selektion
      CALL SCREEN '0020' STARTING AT 10 01
                         ENDING   AT 40 20 .

      IF FCODE = 'STRT'.
        PERFORM STATUS_0010_SET.       "Basisstatus setzen
        SY-LSIND = 0.
*       read_continue_offset = -1.     "don't start at last position
        PERFORM PREPARE_SELECTION.     "Seletionsdaten aufbereiten
        PERFORM CHECK-PARAMETERS.      "Parameter ueberpruefen
        PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
        PERFORM FILL-STATS USING 'F'.  "Fuellen Statistik-Tabelle
      ENDIF.
*
*---Zusaztext Tcode/Screen/Report an/ausschalten
    WHEN 'TEXT'.
      IF SY-LSIND > 1.
        PERFORM TEXT_DISPLAY.
      ELSE.
        READ TABLE STATS INDEX SY-TABIX.
        IF SY-SUBRC NE 0.
          MESSAGE S010.
        ELSE.
          PERFORM TEXT_DISPLAY.
        ENDIF.
      ENDIF.
      CLEAR: FCODE, ACODE.
      EXIT.
*
*---Verlassen der Anzeige-----------------------------------------------
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'MONT'.
      LEAVE.   EXIT.
*
* --- Display of RFC subrecords -------------------------------------- *
    WHEN 'RFC'.
      STEUER = 'RFC'.  PERFORM DISPLAY-HITLIST.
    WHEN 'RFCI'.                 "RFC Time Interval Records
      STEUER = 'RFCI'.  PERFORM DISPLAY-HITLIST.
*
*---Anzeige der DB-Zugriffe zu einem Record in Subliste-----------------
    WHEN 'DBCL'.
      STEUER = 'DBCL'. PERFORM DISPLAY-HITLIST.
*
*---Anzeige der MC-Zugriffe zu einem Record in Subliste-----------------
    WHEN 'MATC'.
      STEUER = 'MATC'. PERFORM DISPLAY-HITLIST.
*
*---Anzeige der Zeitwerte zu einem Record in Subliste-------------------
    WHEN 'DISP'.
      STEUER = 'DISP'. PERFORM DISPLAY-HITLIST.
*
*---Anzeige der Taskinformation zu einem Record in Subliste-------------
    WHEN 'STOR'.
      STEUER = 'STOR'. PERFORM DISPLAY-HITLIST.
*
*---Anzeige der zugegriffenen Tabellen zu einem Record in Subliste------
    WHEN 'DB5F'.
      STEUER = 'DB5F'.
      PERFORM FILL-TABS-FROM-TABSLIST.
      PERFORM DISPLAY-HITLIST.
*
* --- Display of spool subrecords ------------------------------------ *
    WHEN 'SPOO'.
      STEUER = 'SPOO'.  PERFORM DISPLAY-HITLIST.
*
*---Anzeige aller Details auf Pickup Level------------------------------
    WHEN 'ALRC'.
      STEUER = 'ALRC'.
      PERFORM FILL-TABS-FROM-TABSLIST.
      PERFORM DISPLAY-HITLIST.
*
*---Anzeige  der  Details auf Main Level--------------------------------
    WHEN 'AUFR'.
      CALL SCREEN 0035 STARTING AT 10 5 ENDING AT 37 13.
      CASE FCODE.
        WHEN 'STRT'.
          IF STAD-DB5F NE SPACE AND SCOPEFLAG NE SPACE.
            PERFORM FILL-TABS-FROM-TABSLIST.
          ENDIF.
          SY-LSIND = 0.
*          PERFORM STATUS_0010_SET.     "Basisstatus setzen
          PERFORM WRITE-DISP.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
      PERFORM STATUS_0010_SET.
*
*---Sortieren der Tabellen in Subliste der benutzten Tabellen-----------
    WHEN 'SORT'.
      IF STEUER = 'DB5F'.
        PERFORM RESORT-TABS.
        PERFORM DISPLAY-HITLIST.
      ENDIF.
*
*---Anzeige der transferierten Bytes in Subliste------------------------
    WHEN 'NETH'.
      STEUER = 'NETH'. PERFORM DISPLAY-HITLIST.
*
*---Anzeige des folgenden Records --------------------------------------
    WHEN 'NEXT'.
      IF SY-TABIX NE INDEX  AND INDEX > 0.
        ADD 1 TO SY-TABIX.
      ENDIF.
      IF STEUER = 'DB5F'.              "Tabellenanzeige
        PERFORM FILL-TABS-FROM-TABSLIST. "Füllen Minitabelle TABS
      ENDIF.
      PERFORM DISPLAY-HITLIST.
*
*---Anzeige der vorhergehenden Records ---------------------------------
    WHEN 'VORH'.
      IF SY-TABIX > 1 AND INDEX > 0.
        SUBTRACT 1 FROM SY-TABIX.
      ENDIF.
      IF STEUER = 'DB5F'.              "Tabellenanzeige
        PERFORM FILL-TABS-FROM-TABSLIST. "Füllen Minitabelle TABS
      ENDIF.
      PERFORM DISPLAY-HITLIST.
*
*---Anzeige Records eine Stunde spaeter---------------------------------
    WHEN 'MINS'.
      SY-LSIND = 0.
      IF RSTARTTI < '230000'.
        ADD 3600 TO RSTARTTI.
      ELSE.
        SUBTRACT 82800 FROM RSTARTTI.
        ADD 1 TO RDAY.
      ENDIF.
*     read_continue_offset = -1. "don't start at last position...
      IF RSTARTTI < '210000'.
        RENDTI  = RSTARTTI + 10800.
        RENDDAY = RDAY.
      ELSE.
        RENDTI  = RSTARTTI - 75600.
        RENDDAY = RDAY + 1.
      ENDIF.
      PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
      PERFORM FILL-STATS USING 'F'.
*
*---Anzeige Records eine Stunde frueher---------------------------------
    WHEN 'MINM'.
      SY-LSIND = 0.
      IF RSTARTTI >= '010000'.
        SUBTRACT 3600 FROM RSTARTTI.
      ELSE.
        ADD 82800 TO RSTARTTI.
        SUBTRACT 1 FROM RDAY.
      ENDIF.
      IF RSTARTTI < '210000'.
        RENDTI  = RSTARTTI + 10800.
        RENDDAY = RDAY.
      ELSE.
        RENDTI  = RSTARTTI - 75600.
        RENDDAY = RDAY + 1.
      ENDIF.
*     read_continue_offset = -1. "don't start at last position...   ?
      PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
      PERFORM FILL-STATS USING 'F'.
*
*---Anzeige n naechsten Records---------------------------------------
    WHEN 'RECN' OR 'REC+'.
      SY-LSIND = 0.
*     if not ( index_available is initial ).
*       rstartti = rendti.
*       rday     = rendday.
*       if rstartti < '235959'.
*         add 1 to rstartti.
*       else.
*         clear rstartti.
*         add 1 to rday.
*       endif.
*     endif.
*     if rstartti >= '210000'.
*       rendti  = rstartti - 75600.
*       rendday = rday + 1.
*     else.
*       rendti  = rstartti + 10800.
*       rendday = rday.
*     endif.
      PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
      PERFORM FILL-STATS USING 'F'.
*
*---Anzeige n vorherigen Records--------------------------------------
    WHEN 'RECM' OR 'REC-'.             "Lese die vorhergehenden Records
      SY-LSIND = 0.

*     if index_available is initial.
*       if read_continue_offset = 0.
*         message e002.
*       endif.
*       case statistic_version.
*         when 1.
*           describe field statr length rec_length.
*         when 2.
*           describe field pfnorm length rec_length.
*       endcase.
*       "Position rmaxcnt records backwards. With version 1 records this
*       "works fine, but with version 2 - since there are subrecords -
*       "this works only approximately
*
*       read_continue_offset = last_read_continue_offset -
*                              rec_length * rmaxcnt.
*
*       if read_continue_offset < 0.
*         read_continue_offset = 0.
*         message s002.
*   Begin of statistical records
*       endif.
*
*       clear: statl-stime, rstartti, seltime.
*     else.
*       if rstartti = '000000'.
*         rendti    = '235959'.
*         rendday   = rday - 1.
*       else.
*         rendti  = rstartti - 1.
*         rendday = rday.
*       endif.
*       if rendti < '030000'.
*         rday     = rendday - 1.
*         rstartti = rendti + 75600.
*       else.
*         rday     = rendday.
*         rstartti = rendti - 10800.
*       endif.
*     endif.
      PERFORM PREPARE_FILL_STATS USING CODE. "Übergabeparameter setzen
      PERFORM FILL-STATS USING 'B'.
*
*---Verzweigen zur Performance Analyse----------------------------------
    WHEN 'RS10'.
      SUBMIT RSSTAT10 WITH SDATE = RDATE.
*
*---Verzweigen zum Alert Monitor----------------------------------------
    WHEN 'RS25'.
      SUBMIT RSSTAT25 VIA SELECTION-SCREEN.
*
*---Verzweigen zur Historie fuer diesen Server--------------------------
    WHEN 'RS70'.
      SUBMIT RSSTAT70.
  ENDCASE.
ENDFORM.
*
*-----------------------------------------------------------------------
* FORM WITE-DISP: Ausgabe der STATS-internen Tabelle
*-----------------------------------------------------------------------
*
FORM WRITE-DISP.
  DATA FLAG.
  DATA LAST_DATE  LIKE SY-DATUM.
  DATA SAVE_TABIX LIKE SY-TABIX.


  data : begin of it_prog occurs 0,
        name like trdir-name,
       end of it_prog.

  ranges : r_name for  trdir-name.
NAMELENGTH_TCODE = 20.  " Manju

  NAMELENGTH_ULINE = 109 + NAMELENGTH_TCODE + NAMELENGTH_REPORT.
  NEW-PAGE LINE-SIZE NAMELENGTH_ULINE.

  if P_DIST eq 'X'.
    sort stats by DATE REPORT.
    delete adjacent duplicates from  STATS comparing date REPORT.
  endif.
  if  P_INCL  eq 'X'.

    loop at stats.
      r_name-SIGN = 'I'.
      r_name-OPTION = 'EQ'.
      r_name-low = stats-report.
      collect  r_name.
    endloop.

*  select a~PROG b~include  into table it_tab from
*      D010SINF as a inner join  D010INC as b
*        on a~PROG = b~MASTER
**        for all entries in it_prog
*      where  a~prog in  r_name  and
*             a~SUBC eq '1' and
*             b~include in ( select prog from D010SINF
*                           where prog = b~include and
*                                SUBC = 'I' and
*                                prog like 'Z%' ).

    select master include  into table it_tab from
        D010INC   where  master in r_name and
                  include like 'Z%' .

  endif.

  IF EOF_REACHED = 'X' AND READ_DIRECTION = ' '.
    IF INDEX > 0.
      WRITE: / SY-VLINE,
              20 '*** Beginning of statistic file reached ***',
              AT SY-LINSZ SY-VLINE.
    ELSE.
      RSTARTTI = SELTIME = STATL-STIME = '000000'.
    ENDIF.
  ENDIF.

  IF INDEX   < 1.                      "Groesse Tabelle STATS
    SKIP.
*    write: /20 '*** END OF LIST ***'.
    MESSAGE S006 WITH MYNAME.                      "cpuid.
  ELSE.
    LAST_DATE = RDAY.
    LOOP AT STATS.
      PERFORM FLIP_FLOP USING FLAG.
      IF LAST_DATE NE STATS-DATE.
        FORMAT COLOR COL_GROUP.
        STA2-DATE = LAST_DATE = STATS-DATE.
        WRITE: / SY-VLINE NO-GAP, 'Start of', STA2-DATE,
                 AT SY-LINSZ SY-VLINE.
        SAVE_TABIX = SY-TABIX.
        CLEAR SY-TABIX.
        HIDE SY-TABIX.
        SY-TABIX = SAVE_TABIX.
        FORMAT COLOR COL_NORMAL.
      ENDIF.
      PERFORM WRITE-HITL1-RECORD.
      IF SCOPEFLAG NE SPACE AND DETAILS NE SPACE.
        PERFORM FILL-TABS-FROM-TABSLIST.
        PERFORM DETAILS_ON_MAINLIST_WRITE.
        PERFORM FLIP_FLOP USING FLAG.
      ENDIF.
    ENDLOOP.
    IF SCOPEFLAG IS INITIAL OR DETAILS IS INITIAL.

      IF EOF_REACHED = 'X' AND READ_DIRECTION = 'X'.
        FORMAT RESET.
        WRITE: / SY-VLINE, 20 '*** End of statistic file reached ***'
                 COLOR 1 INVERSE ON,
                 AT SY-LINSZ SY-VLINE.
      ENDIF.

      ULINE.
    ENDIF.
  ENDIF.
       WRITE: /20 '*** Summary  ***'
                 COLOR 1 INVERSE ON,
                 AT SY-LINSZ SY-VLINE.
       write :/ 'No of Programs',l_cnt color 3,
                'No of Includes', l_cnt1 COLOR 3.


  SY-TABIX = 1.                        "Position auf Anfang
ENDFORM.
*---------------------------------------------------------------------*
*       FORM DETAILS_ON_MAINLIST_WRITE                                *
*---------------------------------------------------------------------*
FORM DETAILS_ON_MAINLIST_WRITE.
  FORMAT INTENSIFIED OFF COLOR 4.
  IF DETAILS NE SPACE.
    ULINE.
  ENDIF.
  IF DETAILS+0(1) = 'X'.               "DISPatched Zeit
    PERFORM DISP-DETAIL.
  ENDIF.
  IF DETAILS+1(1) = 'X'.               "DB Calls
    PERFORM DBCL-DETAIL.
  ENDIF.
  IF DETAILS+2(1) = 'X'.               "Task details
    PERFORM STOR-DETAIL.
  ENDIF.
  IF DETAILS+3(1) = 'X'.               "Bytes requested
    PERFORM DB5F-DETAIL.
  ENDIF.
  IF DETAILS+4(1) = 'X'.               "Tabellenanzeige
    PERFORM NETZ-DETAIL.
  ENDIF.
  IF DETAILS+5(1) = 'X'.               "Matchcode details
    PERFORM MATC-DETAIL.
  ENDIF.
  IF NOT ( DETAILS+6(1) IS INITIAL ).  "RFC details
    PERFORM RFC_DETAIL.
*    PERFORM rfc_int_detail.
  ENDIF.
  IF NOT ( DETAILS+7(1) IS INITIAL ).  "Spool information
    PERFORM SPOOL_DETAIL.
  ENDIF.
  ULINE AT 1.
ENDFORM.
*----------------------------------------------------------------------*
*                    Macro ADD_REC_TO_STATS
*----------------------------------------------------------------------*
*                 Add a record to the stats table                      *
*----------------------------------------------------------------------*
*  --> &1   statistic record                                           *
*  --> &2   record number                                              *
*  --> &2   record number                                              *
*  --> &3   program/report name                                        *
*  --> &4   entry ID                                                   *
*  --> &5   subrecord index table (sorted by the record number)        *
*----------------------------------------------------------------------*
DEFINE ADD_REC_TO_STATS.
  DIVIDE: &1-RESPTI     BY 1000,                            "B30K012402
          &1-CPUTI      BY 1000,
          &1-QUEUETI    BY 1000,
          &1-ROLLINTI   BY 1000,
          &1-ROLLOUTTI  BY 1000,
          &1-LOCKTI     BY 1000,
          &1-TXXXTI     BY 1000,
          &1-READDIRTI  BY 1000,
          &1-READSEQTI  BY 1000,
          &1-INSTI      BY 1000,
          &1-UPDTI      BY 1000,
          &1-DELTI      BY 1000,
          &1-COMMITTI   BY 1000,
          &1-GENERATETI BY 1000,
          &1-REPLOADTI  BY 1000,
          &1-CUALOADTI  BY 1000,
          &1-DYNPLOADTI BY 1000,
          &1-QUETI      BY 1000,
          &1-DDICTI     BY 1000,
          &1-MCTI       BY 1000.
  MOVE-CORRESPONDING &1 TO STATS.
  STATS-ORIGIN = &2.
  STATS-REPORT = &3.
  IF &4+64(1) = 'T'.
    STATS-TCODE = &4(40).
  ENDIF.
  STATS-ENDTI      = &1-ENDTIME.
  STATS-DATE       = &1-ENDDATE.
  STATS-DBCALLS    = STATS-READDIRCNT + STATS-READSEQCNT +
                     STATS-INSCNT     + STATS-UPDCNT     + STATS-DELCNT.
  STATS-TABLEPOINT = 0.
  IF &2 > 0.
    READ TABLE &5 WITH KEY RECORDNO = &2 BINARY SEARCH.
    IF SY-SUBRC = 0 AND &5-TAB_B > 0.
      STATS-TABLEPOINT = 1.
    ENDIF.
  ENDIF.
  APPEND STATS.
END-OF-DEFINITION.
*-----------------------------------------------------------------------
* FORM FILL-STATS:   Fuellen der internen Tabelle STATS mit Records
* ----------------------------------------------------------------------
* --> DIRECTION  read direction F)orward or B)ackwards
*-----------------------------------------------------------------------
*
FORM FILL-STATS USING DIRECTION TYPE C.
  DATA  RECORDS_READ          LIKE SAPWLSFIDX-RECORDNO.
  DATA  RECORDS               LIKE SAPWLSFIDX-RECORDNO.
  DATA  FILE_ERROR            LIKE SAPWLPSTRC-FILE_ERROR.
  DATA  USER                  LIKE SAPWLPFNRM-ACCOUNT.
  DATA  TCODE                 LIKE SAPWLPFNRM-TCODE.
  DATA  ENTRY_ID              LIKE SAPWLUENTI-ENTRY_ID.
  DATA  PROGRAM               LIKE SAPWLPFNRM-REPORT.
  DATA  SCREEN                LIKE SAPWLPFNRM-DYNPRONR.
  DATA  TASKTYPE              TYPE C.
  DATA  WPID(2)               TYPE C.
  DATA  HEX_WPID              LIKE SAPWLPSTRC-WP_NUMBER.
  DATA  RSPTI                 TYPE P.
  DATA  CPUTI                 TYPE P.
  DATA  DBTI                  TYPE P.
  DATA  KBYTE                 TYPE P.
  DATA  CHG                   TYPE P.
  DATA  V2_NORMAL_START       TYPE I.
  DATA  V2_BTC_STEP_START     TYPE I.
  DATA  EXIT_LOOP.
  DATA  STATS_WA              LIKE STATS.
  DATA  CUTINDEX              TYPE I.
  DATA  WHILE_COUNTER         TYPE I                    VALUE '0'.

  STATICS NO_BUFFER_FLUSH.

  FREE: V2_NORMAL_RECORDS,
        V2_BTC_STEP_RECORDS,
        V2_TABLE_RECORDS,
        V2_RFC_CLIENT_RECORDS,
        V2_RFC_SERVER_RECORDS,
        V2_RFC_CLIENT_DEST_RECORDS,
        V2_RFC_SERVER_DEST_RECORDS,
        V2_SPOOL_PRINT_RECORDS,
        V2_SPOOL_ACTIVITY_RECORDS,
        V2_RFC_TIME_INT_RECORDS,
        NORM_SUBRECORD_INDEX,
        STATS.

  CLEAR INDEX.

  IF RWPID CO ' 0123456789'.
    SY-TFILL = RWPID.     "Conversion to type I
    HEX_WPID = SY-TFILL.  "Conversion to type X
  ELSE.
    HEX_WPID = 'FFFF'.
  ENDIF.

*--- New version of SAPWL_STATREC_READ (_FILE) - June 1997 - DH
* - check read dirction
  IF DIRECTION <> 'B'.
    READ_DIRECTION = 'X'.
  ELSE.
    READ_DIRECTION = ' '.
  ENDIF.
* - check wether start_read_recordno is given
  IF START_READ_RECORDNO IS INITIAL.
    START_READ_RECORDNO = -1.
  ENDIF.

* - read all data from statistic file
  WHILE RECORDS_READ < RMAXCNT AND
        EXIT_LOOP    IS INITIAL.
* - set counter to get first/last record read
    ADD 1 TO WHILE_COUNTER.
* - who many records to read are left?
    RECORDS = RMAXCNT - RECORDS_READ.
* - get start position for now read records
    DESCRIBE: TABLE V2_NORMAL_RECORDS   LINES V2_NORMAL_START,
              TABLE V2_BTC_STEP_RECORDS LINES V2_BTC_STEP_START.
* - read from statistic file 'records' records
    CALL FUNCTION 'SAPWL_STATREC_READ_FILE'
        EXPORTING
             NO_OF_RECORDS               = RECORDS
*            READ_CLIENT                 =
             READ_END_DATE               = RENDDAY
             READ_END_TIME               = RENDTI
*            READ_EXCLUDE_USERNAME       =
             READ_CONTINUE_RECORDNO      = START_READ_RECORDNO
             READ_START_DATE             = RDAY
             READ_START_TIME             = RSTARTTI
             READ_USERNAME               = RUSER
             READ_WORKPROCESS            = HEX_WPID
             READ_FORWARD                = READ_DIRECTION
             STATISTIC_FILE              = RPATH
             NO_BUFFER_FLUSH             = NO_BUFFER_FLUSH
        IMPORTING
             FILE_ERROR                  = FILE_ERROR
             RECORDS_READ                = RECORDS
             EOF_REACHED                 = EOF_REACHED
             CONTINUE_RECORDNO_FORWARD   = CONTINUE_RECORDNO_FORWARD
             CONTINUE_RECORDNO_BACKWARD  = CONTINUE_RECORDNO_BACKWARD
*            PROBLEMS                    =
        TABLES
             V2_NORMAL_RECORDS           = V2_NORMAL_RECORDS
             V2_BTC_STEP_RECORDS         = V2_BTC_STEP_RECORDS
             V2_TABLE_RECORDS            = V2_TABLE_RECORDS
             V2_RFC_CLIENT_RECORDS       = V2_RFC_CLIENT_RECORDS
             V2_RFC_SERVER_RECORDS       = V2_RFC_SERVER_RECORDS
             V2_RFC_CLIENT_DEST_RECORDS  = V2_RFC_CLIENT_DEST_RECORDS
             V2_RFC_SERVER_DEST_RECORDS  = V2_RFC_SERVER_DEST_RECORDS
             V2_SPOOL_PRINT_RECORDS      = V2_SPOOL_PRINT_RECORDS
             V2_SPOOL_ACTIVITY_RECORDS   = V2_SPOOL_ACTIVITY_RECORDS
             V2_RFC_TIME_INT_RECORDS     = V2_RFC_TIME_INT_RECORDS
             NORM_SUBRECORD_INDEX        = NORM_SUBRECORD_INDEX
         EXCEPTIONS
              WRONG_PARAMETER_COMBINATION = 1
              OTHERS                      = 2.
* - records found?
    IF RECORDS = 0.
      EXIT.
    ENDIF.
* - get first / last record read
    IF WHILE_COUNTER = 1.
      IF DIRECTION = 'B'.
        SAVE_CONTINUE_RECORDNO_FORW  = CONTINUE_RECORDNO_FORWARD.
      ELSE.
        SAVE_CONTINUE_RECORDNO_BACKW = CONTINUE_RECORDNO_BACKWARD.
      ENDIF.
    ELSE.
      IF DIRECTION = 'B'.
        SAVE_CONTINUE_RECORDNO_BACKW = CONTINUE_RECORDNO_BACKWARD.
      ELSE.
        SAVE_CONTINUE_RECORDNO_FORW  = CONTINUE_RECORDNO_FORWARD.
      ENDIF.
    ENDIF.
* - don't flush the buffer anymore
    NO_BUFFER_FLUSH = 'X'.
* - sort subrecord index by recordno - no more necessary!?!
    SORT NORM_SUBRECORD_INDEX BY RECORDNO.
* - get / overwork a number of parameters in v2_normal_records
    ADD 1 TO V2_NORMAL_START.
    LOOP AT V2_NORMAL_RECORDS FROM V2_NORMAL_START.
      CALL FUNCTION 'SAPWL_STATREC_GET_ENTRY_ID'
           EXPORTING
                V2_RECORD             = V2_NORMAL_RECORDS
           IMPORTING
                ENTRY_ID              = ENTRY_ID
                CONVERTED_REPORT_NAME = PROGRAM.
      TCODE    = V2_NORMAL_RECORDS-TCODE.
      PERFORM TT_CONVERT_NUMBER_TO_LETTER
              USING V2_NORMAL_RECORDS-TASKTYPE TASKTYPE.
      USER     = V2_NORMAL_RECORDS-ACCOUNT.
      SCREEN   = V2_NORMAL_RECORDS-DYNPRONR.
      SY-TFILL = V2_NORMAL_RECORDS-WPID.
      WRITE SY-TFILL TO WPID USING EDIT MASK 'RR__'.
      RSPTI    = V2_NORMAL_RECORDS-RESPTI / 1000.
      CPUTI    = V2_NORMAL_RECORDS-CPUTI  / 1000.
      DBTI     = ( V2_NORMAL_RECORDS-DELTI +
                   V2_NORMAL_RECORDS-INSTI +
                   V2_NORMAL_RECORDS-UPDTI +
                   V2_NORMAL_RECORDS-READSEQTI +
                   V2_NORMAL_RECORDS-READDIRTI ) / 1000.
      KBYTE    = ( V2_NORMAL_RECORDS-NTABCNT +
                   V2_NORMAL_RECORDS-QUECNT +
                   V2_NORMAL_RECORDS-RFCCNT +
                   V2_NORMAL_RECORDS-DDICCNT +
                   V2_NORMAL_RECORDS-DYNPSCNT +
                   V2_NORMAL_RECORDS-DYNPLCNT +
                   V2_NORMAL_RECORDS-ABAPSCNT +
                   V2_NORMAL_RECORDS-ABAPLCNT +
                   V2_NORMAL_RECORDS-CUALCNT +
                   V2_NORMAL_RECORDS-DSQLCNT ) / 1024.
      CHG      = V2_NORMAL_RECORDS-PHYUPDCNT +
                 V2_NORMAL_RECORDS-PHYINSCNT +
                 V2_NORMAL_RECORDS-PHYDELCNT.
* - don't put record into stats if selection criteria don't match
      CHECK USER     CP RUSER
        AND TCODE    CP RTCODE
        AND PROGRAM  CP RPROGRAM
        AND SCREEN   CP RSCREEN
        AND ( V2_NORMAL_RECORDS-TASKTYPE = RTASKTYPE
                                    OR RTASKTYPE = '00' )
        AND WPID     CP RWPID
        AND RSPTI    >= RRSPTI
        AND CPUTI    >= RCPUTI
        AND DBTI     >= RDBTI
        AND KBYTE    >= RKBYTE
        AND CHG      >= RCHG.
      CLEAR STATS.
      STATS-CPICTI = V2_NORMAL_RECORDS-RFCTI.
* - put normal records into stats
      ADD_REC_TO_STATS V2_NORMAL_RECORDS
                       SY-TABIX
                       PROGRAM
                       ENTRY_ID
                       NORM_SUBRECORD_INDEX.
    ENDLOOP.
* - get / overwork a number of parameters of batches
    ADD 1 TO V2_BTC_STEP_START.
    LOOP AT V2_BTC_STEP_RECORDS FROM V2_BTC_STEP_START.
      PERFORM TT_CONVERT_NUMBER_TO_LETTER
              USING V2_BTC_STEP_RECORDS-TASKTYPE TASKTYPE.
      CALL FUNCTION 'SAPWL_STATREC_GET_ENTRY_ID'
           EXPORTING
                V2_BTC_RECORD           = V2_BTC_STEP_RECORDS
           IMPORTING
                ENTRY_ID                = ENTRY_ID
                CONVERTED_REPORT_NAME   = PROGRAM
           EXCEPTIONS
                WRONG_STATISTIC_VERSION = 1
                OTHERS                  = 2.
      TCODE       = SPACE.           " v2_btc_step_records-tcode.
      USER        = V2_BTC_STEP_RECORDS-ACCOUNT.
      SCREEN      = V2_BTC_STEP_RECORDS-DYNPRONR.
      SY-TFILL    = V2_BTC_STEP_RECORDS-WPID.
      WRITE SY-TFILL TO WPID USING EDIT MASK 'RR__'.
      RSPTI       = V2_BTC_STEP_RECORDS-RESPTI / 1000.
      CPUTI       = V2_BTC_STEP_RECORDS-CPUTI  / 1000.
      DBTI        = ( V2_BTC_STEP_RECORDS-DELTI +
                     V2_BTC_STEP_RECORDS-INSTI +
                     V2_BTC_STEP_RECORDS-UPDTI +
                     V2_BTC_STEP_RECORDS-READSEQTI +
                     V2_BTC_STEP_RECORDS-READDIRTI ) / 1000.
      KBYTE        = ( V2_BTC_STEP_RECORDS-NTABCNT +
                     V2_BTC_STEP_RECORDS-QUECNT +
                     V2_BTC_STEP_RECORDS-RFCCNT +
                     V2_BTC_STEP_RECORDS-DDICCNT +
                     V2_BTC_STEP_RECORDS-DYNPSCNT +
                     V2_BTC_STEP_RECORDS-DYNPLCNT +
                     V2_BTC_STEP_RECORDS-ABAPSCNT +
                     V2_BTC_STEP_RECORDS-ABAPLCNT +
                     V2_BTC_STEP_RECORDS-CUALCNT +
                     V2_BTC_STEP_RECORDS-DSQLCNT ) / 1024.
      CHG          = V2_BTC_STEP_RECORDS-PHYUPDCNT +
                     V2_BTC_STEP_RECORDS-PHYINSCNT +
                     V2_BTC_STEP_RECORDS-PHYDELCNT.
* - don't out record into stats if selection criteria don't match
      CHECK USER     CP RUSER
        AND TCODE    CP RTCODE
        AND PROGRAM  CP RPROGRAM
        AND SCREEN   CP RSCREEN
        AND WPID     CP RWPID
        AND RSPTI    >= RRSPTI
        AND CPUTI    >= RCPUTI
        AND DBTI     >= RDBTI
        AND KBYTE    >= RKBYTE
        AND CHG      >= RCHG
        AND ( V2_BTC_STEP_RECORDS-TASKTYPE = RTASKTYPE
                                    OR RTASKTYPE = '00' ).
      CLEAR STATS.
      STATS-CPICTI = V2_BTC_STEP_RECORDS-RFCTI.
* - put batch record into stats
      ADD_REC_TO_STATS V2_BTC_STEP_RECORDS
                           -1
                           PROGRAM
                           ENTRY_ID
                           NORM_SUBRECORD_INDEX.
    ENDLOOP.
* - beginning/end of statistic file reached? or read problems?
    IF EOF_REACHED = 'X' OR NOT ( FILE_ERROR IS INITIAL ) .
      EXIT.
    ENDIF.
* - get number of records in stat
    DESCRIBE TABLE STATS LINES RECORDS_READ.
* - check wether rmaxcnt was reached and set new start_read_recordno
    IF RECORDS_READ < RMAXCNT.
      IF DIRECTION = 'B'.
        START_READ_RECORDNO = CONTINUE_RECORDNO_BACKWARD.
      ELSE.
        START_READ_RECORDNO = CONTINUE_RECORDNO_FORWARD.
      ENDIF.
    ENDIF.
  ENDWHILE.
* - end of read data from file
* - Satznummer vom zuerst bzw. zuletzt gelesenen Satz wiederherstellen
  IF DIRECTION = 'B'.
    CONTINUE_RECORDNO_FORWARD = SAVE_CONTINUE_RECORDNO_FORW.
  ELSE.
    CONTINUE_RECORDNO_BACKWARD = SAVE_CONTINUE_RECORDNO_BACKW.
  ENDIF.
* - get start time and day if it was reset before
  IF RSTARTTI = SPACE AND RDAY = SPACE.
    SORT STATS BY DATE ASCENDING ENDTI ASCENDING.
    READ TABLE STATS INDEX 1.
    RDAY = STATS-DATE.
    RSTARTTI = STATS-ENDTI.
  ENDIF.

  RDATE   = STATL-SDAT  = RDAY.
  SELTIME = STATL-STIME = RSTARTTI.

* - get number of records in stat - nochmals nötig, falls EOF erreicht
  DESCRIBE TABLE STATS LINES RECORDS_READ.
  IF RECORDS_READ = 0.
    STATL-SENDDAT  = RENDDAY.
    STATL-SENDTIME = RENDTI.
    IF FILE_ERROR IS INITIAL.
      MESSAGE S006 WITH MYNAME.                          "rsystem.
*    No statistical record for $ found with given criteria
    ELSE.
      MESSAGE S076 WITH FILE_ERROR.
*    Read problems with statistic file (OS error: &)
    ENDIF.
  ELSE.
* - get end time and day
    SORT STATS BY DATE ASCENDING ENDTI ASCENDING.
    READ TABLE STATS INDEX RECORDS_READ.
    RENDDAY = STATL-SENDDAT  = STATS-DATE.
    RENDTI  = STATL-SENDTIME = STATS-ENDTI.
  ENDIF.

  INDEX = RECORDS_READ.            "for write-disp
  PERFORM WRITE-DISP.
ENDFORM. "FILL-STATS
*---------------------------------------------------------------------*
*       FORM FILL-TABS-FROM-TABSLIST                                  *
*---------------------------------------------------------------------*
* Tabelle TABS wird mit aktuellen Tabellenstatistikwerten gefüllt     *
*---------------------------------------------------------------------*
FORM FILL-TABS-FROM-TABSLIST.
*  check steuer = 'DB5F'.

  SAVETABIX = SY-TABIX.
  FREE TABS.

  READ TABLE STATS INDEX SAVETABIX.
  IF SY-SUBRC = 0 AND STATS-TABLEPOINT > 0.
    READ TABLE NORM_SUBRECORD_INDEX WITH KEY RECORDNO = STATS-ORIGIN
         BINARY SEARCH.
    IF SY-SUBRC NE 0 OR NORM_SUBRECORD_INDEX-TAB_B <= 0.
      SY-TABIX = SAVETABIX.
      EXIT.
    ENDIF.
    CLEAR TABS.
    LOOP AT V2_TABLE_RECORDS FROM NORM_SUBRECORD_INDEX-TAB_B
                             TO   NORM_SUBRECORD_INDEX-TAB_E.
      TABS-TNAME   = V2_TABLE_RECORDS-TABNAME.
      TABS-MODCNT  = V2_TABLE_RECORDS-MODCNT.
      TABS-DIRCNT  = V2_TABLE_RECORDS-DIRCNT.
      TABS-SEQCNT  = V2_TABLE_RECORDS-SEQCNT.
      TABS-TABTIME = V2_TABLE_RECORDS-TABTIME / 1000. "usec -> msec
      TABS-TOTCNT  = TABS-DIRCNT + TABS-SEQCNT + TABS-MODCNT.
      APPEND TABS.
    ENDLOOP.
  ENDIF.
*
*-TOTAL-Zeile erzeugen--------------------------------------------------

  DESCRIBE TABLE TABS LINES LINES.
  IF LINES > 1.
    LOOP AT TABS.
      IF TABS-TNAME <> 'TOTAL'.
        TABS-CROSS = 'X'.
        TABS-TNAME = 'TOTAL'.
        IF TABS-MODCNT > 0 OR TABS-DIRCNT > 0 OR TABS-SEQCNT > 0.
          COLLECT TABS.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SORT TABS BY CROSS DESCENDING TABTIME DESCENDING.
    STA2-SORTNAME = 'time'.
  ENDIF.

  SY-TABIX = SAVETABIX.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM RESORT-TABS                                              *
*---------------------------------------------------------------------*
*       Umsortieren der Tabellenzugriffsanzeige                       *
*---------------------------------------------------------------------*
FORM RESORT-TABS.

  DATA FIELD(32).
*  SY-CUCOL = SAVECUCOL.

  GET CURSOR FIELD FIELD.
  IF SY-SUBRC NE 0                     OR
     FIELD    IS INITIAL               OR
    ( FIELD NE 'STA1-TNAME'       AND
      FIELD NE 'STA1-DIRCNT'      AND
      FIELD NE 'STA1-MODCNT'      AND
      FIELD NE 'STA1-SEQCNT'      AND
      FIELD NE 'STA1-TOTALREC'    AND
      FIELD NE 'STA1-TABTIME'     AND
      FIELD NE 'STA1-TDIRCNT'     AND
      FIELD NE 'STA1-TMODCNT'     AND
      FIELD NE 'STA1-TSEQCNT'     AND
      FIELD NE 'STA1-TTOTALREC'   AND
      FIELD NE 'STA1-TTABTIME'        ).
    FIELD = 'STA1-TABTIME'.
  ENDIF.
  CASE FIELD.
    WHEN 'STA1-TNAME'.
      STA2-SORTNAME = 'table name'.
      SORT TABS BY CROSS DESCENDING TNAME TOTCNT DESCENDING.
    WHEN 'STA1-TOTALREC' OR 'STA1-TTOTALREC'.
      STA2-SORTNAME = 'total rows'.
      SORT TABS BY CROSS DESCENDING TOTCNT DESCENDING TNAME DESCENDING.
    WHEN 'STA1-DIRCNT' OR 'STA1-TDIRCNT'.
      STA2-SORTNAME = 'dir. read rows'.
      SORT TABS BY CROSS DESCENDING DIRCNT DESCENDING.
    WHEN 'STA1-SEQCNT' OR 'STA1-TSEQCNT'.
      STA2-SORTNAME = 'seq. read rows'.
      SORT TABS BY CROSS DESCENDING SEQCNT DESCENDING.
    WHEN 'STA1-MODCNT' OR 'STA1-TMODCNT'.
      STA2-SORTNAME = 'changed rows'.
      SORT TABS BY CROSS DESCENDING MODCNT DESCENDING.
    WHEN 'STA1-TABTIME' OR 'STA1-TTABTIME'.
      STA2-SORTNAME = 'time'.
      SORT TABS BY CROSS DESCENDING TABTIME DESCENDING.
  ENDCASE.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM TRANSFER_SELECTION_DATA                                  *
*---------------------------------------------------------------------*
*       Transfer der Selektionsdaten                                  *
*---------------------------------------------------------------------*
FORM TRANSFER_SELECTION_DATA.
  RMAXCNT = STATL-SMAXCNT.
  IF RMAXCNT <= 0.
    RMAXCNT = 50000.
  ENDIF.

  RDAY = STATL-SDAT.
  IF RDAY IS INITIAL.
    RDAY = SY-DATUM.
    IF STATL-STIME < SY-UZEIT.
      RSTARTTI = STATL-STIME.
    ELSE.
      CLEAR RSTARTTI.
    ENDIF.
  ELSE.
    RSTARTTI = STATL-STIME.
  ENDIF.

  RENDDAY = STATL-SENDDAT.
  IF RENDDAY IS INITIAL.
    RENDDAY = SY-DATUM.
    RENDTI  = SY-UZEIT.
  ELSE.
    RENDTI = STATL-SENDTIME.
  ENDIF.

  IF RDAY > SY-DATUM                             OR
     ( RDAY = SY-DATUM AND RSTARTTI > SY-UZEIT ).
    RDAY = SY-DATUM.
    CLEAR RSTARTTI.
  ENDIF.
  IF RENDDAY < RDAY                              OR
     ( RENDDAY = RDAY AND RENDTI < RSTARTTI ).
    RENDDAY = RDAY.
    RENDTI = '235959'.
  ENDIF.

  RUSER = STATL-SBENU.
  IF RUSER IS INITIAL.
    RUSER = '*'.
  ENDIF.

  RTCODE = STATL-STCOD.
  IF RTCODE IS INITIAL.
    RTCODE = '*'.
  ENDIF.

  RPROGRAM = STATL-SPROGRAM.
  IF RPROGRAM IS INITIAL.
    RPROGRAM = 'Z*'.
  ENDIF.

  TASKTYPE = STATL-STASK.
  IF TASKTYPE IS INITIAL.
    TASKTYPE = '*'.
  ENDIF.

  RWPID = STATL-SWPID.
  IF RWPID IS INITIAL.
    RWPID = '*'.
  ENDIF.

  RRSPTI = STATL-SRESPTIME.
  RCPUTI = STATL-SCPUTIME.
  RDBTI  = STATL-SDBTIME.
  RCHG   = STATL-SCHG.
  RKBYTE = STATL-SBYTES.

  RSCREEN = STATL-SSCREEN.
  IF RSCREEN IS INITIAL.
    RSCREEN = '*'.
  ENDIF.

  RPATH = STATL-SPATH.
  IF RPATH IS INITIAL OR RPATH = '*'.
    RPATH     = PFAD.
    RSYSTEMID = SYSTEMID.
  ELSE.
    RSYSTEMID = STATL-SSYSTEMID.
    IF RSYSTEMID IS INITIAL OR RSYSTEMID = '*'.
      RSYSTEMID = SYSTEMID.
    ENDIF.
  ENDIF.

  IF STATL-SSELTIME1 <> 'X'.           "Terminal ID statt Account nehmen
    TRANSLATE RUSER TO UPPER CASE.     "Accountname in Grossbuchstaben
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM RETRANS_SELECTION_DATA                                   *
*---------------------------------------------------------------------*
*       STATL mit Selektionsdaten fuellen                             *
*---------------------------------------------------------------------*
FORM RETRANS_SELECTION_DATA.
  TRANSLATE RUSER TO UPPER CASE.
  IF RWPID IS INITIAL OR RWPID(1) = '!'.
    RWPID = '*'.
  ELSEIF RWPID CO ' 0123456789'.
    SY-TFILL = RWPID.
    WRITE SY-TFILL TO RWPID USING EDIT MASK  'RR__'.
  ENDIF.
  STATL-SWPID = RWPID.
  IF RMAXCNT <= 0.
    RMAXCNT = 500.
  ENDIF.
  STATL-SMAXCNT = RMAXCNT.
  STATL-SBENU = RUSER.
  STATL-STCOD = RTCODE.
  STATL-SPROGRAM = RPROGRAM.
  STATL-STASK = TASKTYPE.
  STATL-SRESPTIME = RRSPTI.
  STATL-SCPUTIME = RCPUTI.
  STATL-SDBTIME = RDBTI.
  STATL-SCHG = RCHG.
  STATL-SBYTES = RKBYTE.
  STATL-SDAT = RDAY.
  STATL-STIME = RSTARTTI.
  STATL-SENDDAT = RENDDAY.
  STATL-SENDTIME = RENDTI.
  IF RSCREEN IS INITIAL.
    RSCREEN = '*'.
  ELSEIF RSCREEN CO ' 0123456789'.
    SY-TFILL = RSCREEN.
    WRITE SY-TFILL TO RSCREEN USING EDIT MASK 'RR____'.
    TRANSLATE RSCREEN USING ' 0'.
  ENDIF.
  STATL-SSCREEN = RSCREEN.
  STATL-SPATH = RPATH.
  STATL-SSYSTEMID = RSYSTEMID.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM PREPARE_SELECTION                                        *
*---------------------------------------------------------------------*
*       Aufbereitung der Selektionskriterien                          *
*---------------------------------------------------------------------*
FORM PREPARE_SELECTION.
  DATA: SAVEPATH LIKE PFAD.
*---Sonderfall bei Zeitslot: Terminalid statt Accountname
  IF STATL-SSELTIME1 =  'X'.
    IF STATL-SBENU = '_'.
      RUSER = SPACE.
    ENDIF.
  ELSE.
    TRANSLATE RUSER TO UPPER CASE.
  ENDIF.

  IF STATL-SBENU = '*TOTAL*'.
    STATL-SBENU = '*'.
    RUSER = '*'.
  ENDIF.

  IF STATL-STCOD = '*TOTAL*'.
    STATL-STCOD = '*'.
    RTCODE = '*'.
  ENDIF.

  IF STATL-SPROGRAM = '*TOTAL*'.
    STATL-SPROGRAM = '*'.
    RPROGRAM = '*'.
  ENDIF.
*
*-Eigener Server aber andere Instanz  ( andere SYSTEMID )
  IF STATL-SSYSTEMID BETWEEN '00' AND '99' AND
    STATL-SSYSTEMID <> SYSTEMID.

    NEWINSTANCEFLAG = 'X'.             "Nachbarinstanz wird behandelt
  ELSE.
    NEWINSTANCEFLAG = ' '.             "eigene Instanz
  ENDIF.
*
  PFAD     = RPATH.                    "Pfad setzen
  IF PFAD = SPACE OR PFAD = '*'.       "Default Pfad
    NEWINSTANCEFLAG = ' '.
    STATL-SSYSTEMID = '*'.
    PERFORM FILL-PFAD.
  ENDIF.

  BI_FLAG = ' '.
  PERFORM TT_CONVERT_LETTER_TO_NUMBER USING TASKTYPE RTASKTYPE.
  IF TASKTYPE = 'I'.
    BI_FLAG = 'X'.
  ENDIF.
*
  SELTIME = RSTARTTI.                  "Min.zeit bei Selektion festlegen
  PERFORM GET-CPU-ID.                  "Server-Id laden
  RSYSTEM    = CPUID.                  "Grundparameter setzen

*-Server extern---------------------------------------------------------
  IF STATL-SSYSTEMID <> '**' AND STATL-SSYSTEMID <> '  ' AND
     STATL-SSYSTEMID <> ' *' AND STATL-SSYSTEMID <> '* ' AND
     STATL-SSYSTEMID <> '--' AND STATL-SSYSTEMID <> SYSTEMID.
    IF STATL-SSYSTEMID BETWEEN '00' AND '99'.
    ELSE.
      RSYSTEM    = 'Extern'.           "bei externen Systemen
    ENDIF.
  ENDIF.

*--Pfadnamen verproben / Kann Index ueberhaupt benutzt, aufgebaut werden
  IF PFAD <> SPACE AND PFAD <> '*'.    "Default Pfad
    SAVEPATH = PFAD.
    PERFORM FILL-PFAD.
    IF PFAD <> SAVEPATH                "Nicht Default Pfadname
      AND RSYSTEM <> 'Extern'          "und keine Externen Daten
      AND NEWINSTANCEFLAG <> 'X'.      "und keine Nachbarinstanzdaten
      NOINDEXFLAG = 'X'.               "Kein Indexaufbau gewuenscht !
    ENDIF.
    PFAD = SAVEPATH.
  ENDIF.
*
*-Authority-Check bei Userfremden Daten---------------------------------
  IF RUSER <> SY-UNAME.                "Fremde User anzeigen !
*    AUTHORITY-CHECK OBJECT 'S_TOOLS_EX'
*    ID 'AUTH' FIELD 'S_TOOLS_EX_A'.
*    IF SY-SUBRC <> 0.
*      NOAUTHFLAG = 'X'.                "Nicht authorisiert
*      IF RUSER <> '*'.                 "Alle sind erlaubt
*        STATL-SBENU = SY-UNAME.        "oder nur der Benutzer selbs
*        STATL-SSELTIME1 =  ' '.        "Terminalid nicht erlaubt
*        RUSER = SY-UNAME.
*      ENDIF.
*      MESSAGE I090 WITH 'Monitortools'."B20K000662 QQI
*    ENDIF.
  ENDIF.
  IF RUSER <> '*'.                 "Alle sind erlaubt
    STATL-SBENU = SY-UNAME.        "oder nur der Benutzer selbs
    STATL-SSELTIME1 =  ' '.        "Terminalid nicht erlaubt
    RUSER = SY-UNAME.
  ENDIF.

*
*-Pruefen Profileparameter rsdb/level bei eigenem Server----------------
  IF RSYSTEM = SY-HOST.
    CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'stat/level'
                       ID 'VALUE' FIELD PARVALUE.
    IF SY-SUBRC > 0 OR PARVALUE = '0'.
      MESSAGE E097 WITH 'stat/level' PARVALUE.
      EXIT.
    ENDIF.
  ENDIF.
*
  RPERIOD    = 'DAY'.                                       "
  RDATE      = RDAY.                                        "
ENDFORM.
*---------------------------------------------------------------------*
*       FORM CHECK-SPECIAL-TCODE-2                                    *
*---------------------------------------------------------------------*
*       Sonderfaelle fuer TCODE (RSSTAT20) BLANK ODER HEX-NULLEN      *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM CHECK-SPECIAL-TCODE-2 USING TCODE DYNPRONR TASKTYPE REP USER TERM.
  DATA: REPORT(8).
  REPORT = REP.
*
*--Tcode = BLANK Fallbehandlung-----------------------------------------
  IF TCODE CA SRTF2.
    TCODE = SPACE.
  ENDIF.


  IF REPORT = 'SAPMSYST'.

    CASE DYNPRONR.
      WHEN '0011'.
        REP = 'LOGOFF'.                "Logoff Sicherheitspopup
      WHEN '0020'.
        REP = 'LOGIN_PW'.              "Login Screen
      WHEN '0040'.
        REP = 'MAINMENU'.              "Mainmenu
      WHEN '0010'.
        REP = REPORT.                  "R/2 Schnittstelle Abmeldebild
        TCODE = SPACE.
      WHEN '0050'.
        REP = REPORT.   .              "R/2 Schnittstelle Anmeldebild
        TCODE = SPACE.
      WHEN '0200'.
        REP = 'NEW PSWD'.              "Neues Passwort
      WHEN OTHERS.
        REP = 'SAPMSYST'.              "Andere
    ENDCASE.

  ELSEIF REPORT = 'SAPMSSY0'.
    CASE DYNPRONR.
      WHEN '0120'.
        REP  = 'LOGIN_OK'.             "Copyright
      WHEN OTHERS.
    ENDCASE.

  ELSEIF REPORT = 'SAPMSSY1'.          "CPIC/Update Verarbeitung
    CASE DYNPRONR.
      WHEN '3003'.
        REP   = 'CPIC/RFC'.
      WHEN '3004'.
        REP = 'CPIC/RFC'.
      WHEN OTHERS.
        REP = 'CPIC/RFC'.
    ENDCASE.

  ELSEIF REPORT = 'SAPMSSY2'.          "Batch/Background
    CASE DYNPRONR.
      WHEN '4004'.
        REP = '(B)SCHDL'.
      WHEN '4005'.
        REP = '(B)STRTR'.
      WHEN '4006'.
        REP = '(B)ZOMBI'.
      WHEN '4007'.
        REP = '(B)EVDRI'.
      WHEN '4008'.
        REP = '(B)EXTSC'.
      WHEN '4009'.
        REP = '(B)AUTOD'.
      WHEN '4010'.
        REP = '(B)SWTCH'.
      WHEN OTHERS.
        REP = '(B)?????'.
    ENDCASE.

  ELSEIF REPORT = 'SAPMSSY6'.          "AutoABAP    BINK073862
    IF STATS-TASKTYPE = '07'.
      REP = 'AUTOABAP'.
    ELSE.
      REP = REPORT.
    ENDIF.

  ELSEIF REPORT(5) = 'SAPMS'.          "Andere Systemprogramme

  ELSEIF REPORT CA SRTF2.              "Reportname enthaelt Hex Nullen
    REP = '?'.
    IF DYNPRONR CA SRTF2 OR            "Dynpronummer auch mit Hex Nullen
       DYNPRONR EQ SPACE.
      DYNPRONR = ' '.
      IF (  USER = 'SAPSYS' AND TERM = SPACE    ) OR
         (  USER = SPACE    AND TERM = 'SAPSYS' ).
        REP   = 'SYSTEM'.
        TCODE = '    '.
      ELSEIF USER <> SPACE.
        TCODE = '       '.
        IF USER EQ 'SAPSYS' AND  STATS-TASKTYPE EQ '06'.
          REP = 'BUF.SYNC'.            "Buffer synchronisation
        ELSE.
          REP = 'ABORTED'.             "Dialog wurde hart abgebrochen
        ENDIF.
      ELSE.
        REP   = '       '.             "Dialog unbekannt
        TCODE = '       '.             "Dialog unbekannt
      ENDIF.
    ELSE.
      REP = '    '.                    "Nicht zu klaerende Zuweisung
    ENDIF.

  ELSEIF TASKTYPE = '02'               "Bei Update ohne TCOD Bezeichn.
      OR TASKTYPE = '04'.              "Bei Batch  ohne TCOD Bezeichn.
  ELSE.
    IF REPORT = SPACE.
      REP = '?   '.                    "Nicht zu klaerende Zuweisung
    ELSE.
      TCODE = '    '.
      IF TASKTYPE NE '03'.
        TRANSLATE REP TO LOWER CASE.     "In Kleinschrift
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM STATUS_0010_SET                                          *
*---------------------------------------------------------------------*
*       Status 0010 setzen / Drucktasten ausschließen                 *
*---------------------------------------------------------------------*
FORM STATUS_0010_SET.
  REFRESH EX.
  CLEAR   EX.

  IF SCOPEFLAG EQ SPACE.
*    EX-EX = 'NOAU'.
*    APPEND EX.
  ELSE.
*    ex-ex = 'PICK'.
*    APPEND ex.
  ENDIF.
*  APPEND ex.
*  IF RMAXCNT = 200.
*    EX-EX = 'REC-'.
*    APPEND EX.
*    EX-EX = 'REC+'.
*  ELSE.
*    EX-EX = 'RECM'.
*    APPEND EX.
*    EX-EX = 'RECN'.
*  ENDIF.
*  APPEND EX.
  SET PF-STATUS '0010' EXCLUDING EX.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM CHECK-SPECIAL-TCODE-3                                    *
*---------------------------------------------------------------------*
*       Sonderfaelle fuer TCODE (RSSTAT20) SE38/SE31/SA38             *
*       -Editorfunktion (SAPMSEDT)                                    *
*       -Listbild Grundliste                                          *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM CHECK-SPECIAL-TCODE-3 USING TCODE DYNPRONR REP.
  IF REP = 'SAPMSEDT'.
    REP = 'REP_EDIT'.                  "Report Editor Funktion
    EXIT.
  ELSEIF REP = 'SAPMSSY3'.
    REP = 'DEBUGGER'.                  "Debugging Modus
    EXIT.
  ELSEIF REP = 'SAPMSSY0' AND DYNPRONR = '0120'.
    REP = 'REP_LIST'.                  "Report Grundliste
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       MODULE D0020_ANWAHL                                           *
*---------------------------------------------------------------------*
* Setzt Datum und Status                                              *
*---------------------------------------------------------------------*
MODULE D0020_ANWAHL OUTPUT.
  SET TITLEBAR '002'.
  SET PF-STATUS '0030' EXCLUDING 'NOAU'.
  PERFORM RETRANS_SELECTION_DATA.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE D0020_SUBMIT                                           *
*---------------------------------------------------------------------*
* Ansteuern des entsprechenden Reports                                *
*                                                                     *
*---------------------------------------------------------------------*
MODULE D0020_SUBMIT.
  DATA: TEXT(50).
  CASE FCODE.
    WHEN 'CANC'.
      SET SCREEN  0. LEAVE SCREEN.
    WHEN 'STRT'.
      IF STATL-SENDTIME = SPACE.
        STATL-SENDTIME = '235959'.
      ENDIF.
      IF STATL-SENDDAT IS INITIAL.
        STATL-SENDTIME = '99991231'.
      ENDIF.
      IF STATL-SENDDAT < STATL-SDAT OR
       ( STATL-SENDDAT = STATL-SDAT AND STATL-SENDTIME <= STATL-STIME ).
        TEXT = 'The end time must be later than the start time!'.
        MESSAGE E333 WITH TEXT.
* - The end time must be later than the start time.
      ENDIF.
      IF STATL-STIME = SPACE OR STATL-SDAT IS INITIAL.
        MESSAGE E333 WITH 'Please select a start time and date!'.
* - Please select a start time and date.
      ENDIF.
      PERFORM TRANSFER_SELECTION_DATA.
      ACODE = FCODE.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.
      MESSAGE E004.
  ENDCASE.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE AUTHORITY_CHECK                                        *
*---------------------------------------------------------------------*
*       Ausblenden Userauswahl bei Autorisierungsmangel               *
*---------------------------------------------------------------------*
MODULE AUTHORITY_CHECK OUTPUT.
  AUTHORITY-CHECK OBJECT 'S_TOOLS_EX'
  ID 'AUTH' FIELD 'S_TOOLS_EX_A'.
  IF SY-SUBRC <> 0.
    STATL-SBENU = SY-UNAME.

    LOOP AT SCREEN.
      CHECK SCREEN-GROUP4 = 'USR'.
      MOVE '0' TO SCREEN-INPUT.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE D0035_ANWAHL                                           *
*---------------------------------------------------------------------*
* Setzt Datum und Status                                              *
*---------------------------------------------------------------------*
MODULE D0035_ANWAHL OUTPUT.
  SET TITLEBAR '035'.
  REFRESH: EX.
  IF SCOPEFLAG EQ SPACE.
    SET PF-STATUS '0030' EXCLUDING 'NOAU'.
  ELSE.
    SET PF-STATUS '0030'.
  ENDIF.
  CLEAR: STAD, FCODE.
*
  IF DETAILS+0(1) = 'X'.
    STAD-DISP = 'X'.
  ENDIF.
  IF DETAILS+1(1) = 'X'.
    STAD-DBCL = 'X'.
  ENDIF.
  IF DETAILS+2(1) = 'X'.
    STAD-STOR = 'X'.
  ENDIF.
  IF DETAILS+3(1) = 'X'.
    STAD-DB5F = 'X'.
  ENDIF.
  IF DETAILS+4(1) = 'X'.
    STAD-NETH = 'X'.
  ENDIF.
  IF DETAILS+5(1) = 'X'.
    STAD-MATC = 'X'.
  ENDIF.
  IF NOT ( DETAILS+6(1) IS INITIAL ).
    STAD-RFC = 'X'.
  ENDIF.
  IF NOT ( DETAILS+7(1) IS INITIAL ).
    STAD-SPOOL = 'X'.
  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE D0035_AUSWAHL                                          *
*---------------------------------------------------------------------*
* Ansteuern des entsprechenden Reports                                *
*                                                                     *
*---------------------------------------------------------------------*
MODULE D0035_AUSWAHL.
  DATA: SAVEDET LIKE DETAILS.
  SAVEDET = DETAILS.
*
  CASE FCODE.
    WHEN 'CANC'.
      SET SCREEN  0. LEAVE SCREEN.
    WHEN 'STRT'.
      CLEAR DETAILS.
      ACODE = FCODE.
      IF STAD-DISP = 'X'.
        DETAILS+0(1) = 'X'.
      ENDIF.
      IF STAD-DBCL = 'X'.
        DETAILS+1(1) = 'X'.
      ENDIF.
      IF STAD-STOR = 'X'.
        DETAILS+2(1) = 'X'.
      ENDIF.
      IF STAD-DB5F = 'X'.
        DETAILS+3(1) = 'X'.
      ENDIF.
      IF STAD-NETH = 'X'.
        DETAILS+4(1) = 'X'.
      ENDIF.
      IF STAD-MATC = 'X'.
        DETAILS+5(1) = 'X'.
      ENDIF.
      IF NOT ( STAD-RFC IS INITIAL ).
        DETAILS+6(1) = 'X'.
      ENDIF.
      IF NOT ( STAD-SPOOL IS INITIAL ).
        DETAILS+7(1) = 'X'.
      ENDIF.
      IF DETAILS = SAVEDET AND SCOPEFLAG NE SPACE.
        CLEAR FCODE.
      ENDIF.
      IF DETAILS NE SPACE.
        SCOPEFLAG = 'X'.
      ELSE.
        SCOPEFLAG = SPACE.
      ENDIF.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'NOAU'.
      SCOPEFLAG = SPACE.
      FCODE = 'STRT'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN OTHERS.
      MESSAGE E004.
  ENDCASE.
ENDMODULE.
*
INCLUDE ZRSSTATMO1.
*INCLUDE RSSTATMO.           "Kommunikation mit MONI FORM-Anweisungen
INCLUDE ZRSSTATHL.
*INCLUDE RSSTATHL.           "Anzeige der Records, FORM-Anweisungen
INCLUDE ZRSSTATSL.
*INCLUDE RSSTATSL.                      "Forms for record selection
INCLUDE ZRSSTATPA.
*INCLUDE RSSTATPA.                      "Forms for Reorg Parameters
INCLUDE ZRSSTATCK.
*INCLUDE RSSTATCK.                      "Checkroutine ob zentrales Syste
INCLUDE ZRSSTATCO.
*INCLUDE RSSTATCO.
*&---------------------------------------------------------------------*
*&      Module  DYNP0040_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       Show record statistic                                          *
*----------------------------------------------------------------------*
MODULE DYNP0040_PBO OUTPUT.
  SET PF-STATUS 'DYNP0040'.
  SET TITLEBAR 'D40'.

  CLEAR REC_STATISTIC.

  REC_STATISTIC-FILE = RPATH.
  LOOP AT STATS.

    IF STATS-TASKTYPE = TT_BTC.
      ADD 1 TO REC_STATISTIC-BTC.
    ELSE.
      ADD 1 TO REC_STATISTIC-NORMAL.
    ENDIF.
    READ TABLE NORM_SUBRECORD_INDEX WITH KEY RECORDNO = STATS-ORIGIN
         BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF NORM_SUBRECORD_INDEX-TAB_B > 0.
        REC_STATISTIC-TABLE = REC_STATISTIC-TABLE + 1 +
                              NORM_SUBRECORD_INDEX-TAB_E -
                              NORM_SUBRECORD_INDEX-TAB_B.
      ENDIF.
      IF NORM_SUBRECORD_INDEX-RFC_CLI_B > 0.
        REC_STATISTIC-RFC_CLI = REC_STATISTIC-RFC_CLI + 1 +
                                NORM_SUBRECORD_INDEX-RFC_CLI_E -
                                NORM_SUBRECORD_INDEX-RFC_CLI_B.
      ENDIF.
      IF NORM_SUBRECORD_INDEX-RFC_SERV_B > 0.
        REC_STATISTIC-RFC_SERV = REC_STATISTIC-RFC_SERV + 1 +
                                 NORM_SUBRECORD_INDEX-RFC_SERV_E -
                                 NORM_SUBRECORD_INDEX-RFC_SERV_B.
      ENDIF.
      IF NORM_SUBRECORD_INDEX-RFC_CLID_B > 0.
        REC_STATISTIC-RFC_CLI_DEST = REC_STATISTIC-RFC_CLI_DEST + 1 +
                                     NORM_SUBRECORD_INDEX-RFC_CLID_E -
                                     NORM_SUBRECORD_INDEX-RFC_CLID_B.
      ENDIF.
      IF NORM_SUBRECORD_INDEX-RFC_SERVDB > 0.
        REC_STATISTIC-RFC_SERV_DEST = REC_STATISTIC-RFC_SERV_DEST + 1 +
                                      NORM_SUBRECORD_INDEX-RFC_SERVDE -
                                      NORM_SUBRECORD_INDEX-RFC_SERVDB.
      ENDIF.
      IF NORM_SUBRECORD_INDEX-SPO_PRI_B > 0.
        REC_STATISTIC-SPOOL_PRINT = REC_STATISTIC-SPOOL_PRINT + 1 +
                                    NORM_SUBRECORD_INDEX-SPO_PRI_E -
                                    NORM_SUBRECORD_INDEX-SPO_PRI_B.
      ENDIF.
      IF NORM_SUBRECORD_INDEX-SPO_ACT_B > 0.
        REC_STATISTIC-SPOOL_ACTIVITY = REC_STATISTIC-SPOOL_ACTIVITY + 1
                                     + NORM_SUBRECORD_INDEX-SPO_ACT_E
                                     - NORM_SUBRECORD_INDEX-SPO_ACT_B.
      ENDIF.
    ENDIF.
  ENDLOOP.
  REC_STATISTIC-READ_POS_START = CONTINUE_RECORDNO_BACKWARD.
  REC_STATISTIC-READ_POS_END   = CONTINUE_RECORDNO_FORWARD - 1.
ENDMODULE.                 " DYNP0040_PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SHOW_RECORD_STATISTIC
*&---------------------------------------------------------------------*
*       Initialize and show the record statistic                       *
*----------------------------------------------------------------------*
*       no parameters
*----------------------------------------------------------------------*
FORM SHOW_RECORD_STATISTIC.
  DATA SAVE_TABIX LIKE SY-TABIX. "never change sy-tabix :-(

  SAVE_TABIX = SY-TABIX.

  CALL SCREEN 40 STARTING AT 12 1 ENDING AT 52 21.

  SY-TABIX = SAVE_TABIX.
ENDFORM.                    " SHOW_RECORD_STATISTIC
*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER_OF_RECS_DISPLAYED
*&---------------------------------------------------------------------*
*       Return the number of records displayed                         *
*----------------------------------------------------------------------*
*  <--  RECS      number of records
*----------------------------------------------------------------------*
FORM GET_NUMBER_OF_RECS_DISPLAYED USING RECS TYPE I.
  CALL SCREEN 50 STARTING AT 6 9 ENDING AT 50 11.
  IF DYNP0050_OKCODE = 'CONT'.
    RECS = DYNP0050_RECS_DISPLAYED.
  ELSE.
    RECS = RMAXCNT.
  ENDIF.
ENDFORM.                    " GET_NUMBER_OF_RECS_DISPLAYED
*&---------------------------------------------------------------------*
*&      Module  DYNP0050_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       Init dynpro 50 (dialog for changing number of records displd)  *
*----------------------------------------------------------------------*
MODULE DYNP0050_PBO OUTPUT.
  DYNP0050_RECS_DISPLAYED = RMAXCNT.
  CLEAR DYNP0050_OKCODE.
  SET PF-STATUS 'DYNP0050'.
  SET TITLEBAR 'D50'.
ENDMODULE.                 " DYNP0050_PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DYNP0050_PAI  INPUT
*&---------------------------------------------------------------------*
*       PAI for dynpro 50                                              *
*----------------------------------------------------------------------*
MODULE DYNP0050_PAI INPUT.
  IF DYNP0050_OKCODE = 'CONT'.
    IF DYNP0050_RECS_DISPLAYED <= 0.
      MESSAGE W077.
*    Please enter values greater than 0
      SET SCREEN 50.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDMODULE.                 " DYNP0050_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FILL_STATS
*&---------------------------------------------------------------------*
*       Übergabeparameter für Form Fill-Stats bzw. Funktionsbaustein
*       SAPWL_STATREC_READ_FILE setzen.
*----------------------------------------------------------------------*
*  -->  code      Bearbeitungsmodus (-> Leserichtung, Leseergebnis)
*  <--  start_read_recordno  Startsatz, falls 'Weiter-'lesen
*       rday, rstartti       ggf. Lesestartzeitpunkt
*       rendday, rendti      ggf. Leseendzeitpunkt
*----------------------------------------------------------------------*
FORM PREPARE_FILL_STATS USING VALUE(FALL).

  CASE FALL.
    WHEN 'INIT' OR 'MINS' OR 'MINM' OR 'NEWR' OR 'NEWT'.
      START_READ_RECORDNO = '1-'.
    WHEN 'RECM' OR 'SIZE'.
      START_READ_RECORDNO = CONTINUE_RECORDNO_BACKWARD.
      RDAY = SPACE. RSTARTTI = SPACE.
      RENDDAY = '99991231'. RENDTI = '235959'.
    WHEN 'RECN'.
      START_READ_RECORDNO = CONTINUE_RECORDNO_FORWARD.
      RDAY = SPACE. RSTARTTI = SPACE.
      RENDDAY = '99991231'. RENDTI = '235959'.
    WHEN OTHERS.
      START_READ_RECORDNO = '1-'.
  ENDCASE.

ENDFORM.                    " PREPARE_FILL_STATS
