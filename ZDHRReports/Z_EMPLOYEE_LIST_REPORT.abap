*----------------------------------------------------------------------
* Program ID        : Z_EMPLOYEE_LIST_REPORT
* Title             : [HR] Employee list report
* Created on        : 07/14/2008
* Created by        : I.G.MOON
* Specifications By : Ahmad,Imtiaz
* Description       : Employee list report
* This program was copied from RPLMIT00 and
* was modified for adding status column.
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 04/05/2010 VALERIAN   UD1K948769  Add the following new columns to
*                                   the report: Emp.Group and
*                                   description, Emp.Subgroup and
*                                   description, Last Name, First Name
*                                   and Middle name.
* 07/14/2011 VALERIAN   UD1K952410  Add columns: Work schedule rule
*                                   Code and Text
*----------------------------------------------------------------------


REPORT z_employee_list_report USING DATABASE pnp MESSAGE-ID pn.
TABLES: pernr.
TABLES: t001p,
        prelq.                                              "AHRK052167
tables: t503.                                               "UD1K952410

INFOTYPES: 0000, "Massnahmen (wg. STAT2 fuer HIRE+FIRE)
           0001,                       "org. Zuordnung
           0002, "Daten zur Person (insbesondere Name)
** Furong on 05/07/14 (
*           0003,                       "Abrechnungsstatus
** End )
           0007.                                            "UD1K952410

*---  Daten fuer Funktionsbaustein RP_HIRE+FIRE
DATA: hiredate LIKE rptxxxxx-datum1,   "statt type d.      "XIRK036103
      firedate LIKE rptxxxxx-datum1.   "statt type d.      "XIRK036103
DATA: BEGIN OF phifi OCCURS 5.
        INCLUDE STRUCTURE phifi.
DATA: END OF phifi.

DATA: nachn LIKE p0002-nachn,          "XBSK11K107382
      name2 LIKE p0002-name2,          "XBSK11K107382
      vorna LIKE p0002-vorna.          "XBSK11K107382

*----  Selektionsbildschirm definieren
SELECTION-SCREEN BEGIN OF BLOCK frm1 WITH FRAME TITLE frametxt.
"XWIP30K013937
SELECT-OPTIONS:
  name     FOR nachn NO INTERVALS,                          "AHRK052167
  gbname   FOR name2 NO INTERVALS,                          "AHRK052167
  vorname  FOR vorna NO INTERVALS,                          "AHRK052167
  nation   FOR p0002-natio NO INTERVALS,                    "AHRK052167
  nur_neu  FOR hiredate.

* Radiobuttonblock fuer Geschlechter-Auswahl             "XWIP30K013937
SELECTION-SCREEN BEGIN OF BLOCK frm2 WITH FRAME TITLE text-gsl.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
PARAMETERS:
  list1 LIKE rplxxxxx-list RADIOBUTTON GROUP list DEFAULT 'X'.
SELECTION-SCREEN COMMENT 03(20) text-s11.
SELECTION-SCREEN POSITION 33.
PARAMETERS:
  list2 LIKE rplxxxxx-list RADIOBUTTON GROUP list.
SELECTION-SCREEN COMMENT 35(15) text-s12.
SELECTION-SCREEN POSITION 58.
PARAMETERS:
  list3 LIKE rplxxxxx-list RADIOBUTTON GROUP list.
SELECTION-SCREEN COMMENT 60(15) text-s13.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK frm2.
SELECTION-SCREEN END OF BLOCK frm1.    "XWIP30K013937

* Rahmen fuer Listenaufbereitung                         "XIRK036103
SELECTION-SCREEN BEGIN OF BLOCK frm3 WITH FRAME TITLE text-lst.
* checkbox fuer Kostenstellentext
PARAMETERS: kostltxt LIKE rplxxxxx-kostltxt DEFAULT space.
SELECTION-SCREEN END OF BLOCK frm3.

*---  Konstanten
rp-lowdate-highdate.
*---  Daten fuer RMAC-Baustein RP_EDIT_NAME
DATA: $edit-name(40),
      $ret-code LIKE sy-subrc,
      $length(2) TYPE n.
*---  Daten fuer Stellentext
* DATA: stell_text LIKE t513s-stltx,                        "PH4K013329
DATA: stell_text LIKE p1000-stext,                          "PH4K013329
      initial_p0001_stell LIKE p0001-stell.
*---  Daten fuer Gruppenwechsel BUKRS/WERKS/KOSTL
DATA: BEGIN OF neu,
        bukrs LIKE p0001-bukrs,
        werks LIKE p0001-werks,                             "XIRK036103
*       gsber like p0001-gsber,        "del                 "XIRK036103
        kostl LIKE p0001-kostl,
      END OF neu,
      alt LIKE neu.

DATA:
* COSTCENTERTEXT LIKE HRCA_COSTC-SHORTTNAME,                  AHRK038889
      costcentertext LIKE hrca_costc-name,                  "AHRK038889
      begda          LIKE prel-begda,
      endda          LIKE prel-endda.

*---  Statistikfelder
DATA: BEGIN OF stat,
        pernr TYPE p,
        kostl TYPE p,
        err   TYPE p,                                       "XIRK036103
      END OF stat.

*---- Daten fuer den FuBaustein HR_DISPLAY_BASIC_LIST       "XIRK036103
*---- (--> Listausgabe ueber TableControl)
DATA: header2(132),                    "Ueberschrift
      footnote1(132), footnote2(132),  "Fussnoten
      footnote3(132),                  "Fussnoten
      myreport LIKE sy-repid,          "Reportname
      return_code LIKE sy-subrc,       "Returncode
      datum(10),                       "Hilfsfeld
      show_perid TYPE i,               "PERID anzeigen?     "XIRK100480
      show_name2 TYPE i VALUE 0,       "Geburtsname anzeigen?
      column_type,                     "Typ der Spalte
      list_level(2).                   "Listenlevel
* Mitarbeiterliste mit Gruppenwechsel BUKRS/WERKS/KOSTL
DATA: BEGIN OF display_all OCCURS 100,
        bukrs LIKE p0001-bukrs,
        werks LIKE p0001-werks,
        kostl LIKE p0001-kostl,
        kosttext(30),
        pernr LIKE pernr-pernr,
        perid LIKE p0002-perid,
        name LIKE p0001-ename,
        name2 LIKE p0002-name2,
*       stell LIKE t513s-stltx,                             "PH4K013329
        stell LIKE p1000-stext,                             "PH4K013329
        hiredate LIKE prelq-hiredate,                       "AHRK041517
        firedate LIKE prelq-firedate,                       "AHRK041517
* by ig.moon 7/14/2008 {
        s_text(30),
* Begin of HIS20094
        persg  type p0001-persg,
        persgt type t501t-ptext,
        persk  type p0001-persk,
        perskt type t503t-ptext,
        nachn  type p0002-nachn,
        vorna  type p0002-vorna,
        midnm  type p0002-midnm,
        schkz  type p0007-schkz,                            "UD1K952410
        rtext  type t508s-rtext,                            "UD1K952410
* End of HIS20094
      END OF display_all.
* }
* Fehlerliste
DATA: BEGIN OF display_err OCCURS 30.
        INCLUDE STRUCTURE hrerror.
DATA: END OF display_err.
* Ausgabe-Steuerungstabelle fuer display_all.
DATA: BEGIN OF fieldnames OCCURS 10,
        text(60), tabname(10), fieldname(10), typ,
      END OF fieldnames.

INITIALIZATION.
  frametxt = text-wse. "statt pnp_spec_standard_frametxt. "XWIP30K013937
* RP-INIT-WITH-SYDATUM.                               "YFGK11K084002
  pnptimed = 'D'.                      "YFGK11K084002
  rp-sel-ein-aus-init.

START-OF-SELECTION.
  rp_set_data_interval 'P0002' pn-begda pn-endda.           "XJFK138520

  myreport = sy-repid.                                      "XIRK036103
  PERFORM check-input.
  rp-set-name-format.   "Read T522F / $$FORMAT = '01' fuer RP_EDIT_NAME
* rp-fetch-alter-perid.  "Call Function RP_FETCH_ALTER_PERNR"XIRK100480
* stattdessen direkter Aufruf des FBSt.                     "XIRK100480
  CALL FUNCTION 'RP_FETCH_ALTERNATE_PERNR'                  "XIRK100480
       IMPORTING                                            "XIRK100480
            alter_pernr = show_perid                        "XIRK100480
            retcode     = return_code                       "XIRK100480
       EXCEPTIONS                                           "XIRK100480
            OTHERS      = 1.                                "XIRK100480

  IF sy-subrc <> 0.
    CLEAR show_perid.
  ENDIF.

  LOOP AT name.                        "XBSK11K107382
    TRANSLATE name-low  TO UPPER CASE. "XBSK11K107382
    TRANSLATE name-high TO UPPER CASE. "XBSK11K107382
    SHIFT name-low LEFT DELETING LEADING space.             "AHRK025257
    SHIFT name-high LEFT DELETING LEADING space.            "AHRK025257
    MODIFY name.                       "XBSK11K107382
  ENDLOOP.                             "XBSK11K107382

  LOOP AT vorname.                     "XBSK11K107382
    TRANSLATE vorname-low  TO UPPER CASE.                "XBSK11K107382
    TRANSLATE vorname-high TO UPPER CASE.                "XBSK11K107382
    SHIFT vorname-low LEFT DELETING LEADING space.          "AHRK025257
    SHIFT vorname-high LEFT DELETING LEADING space.         "AHRK025257
    MODIFY vorname.                    "XBSK11K107382
  ENDLOOP.                             "XBSK11K107382

  LOOP AT gbname.                      "XBSK11K107382
    TRANSLATE gbname-low  TO UPPER CASE.                 "XBSK11K107382
    TRANSLATE gbname-high TO UPPER CASE.                 "XBSK11K107382
    SHIFT gbname-low LEFT DELETING LEADING space.           "AHRK025257
    SHIFT gbname-high LEFT DELETING LEADING space.          "AHRK025257
    MODIFY gbname.                     "XBSK11K107382
  ENDLOOP.                             "XBSK11K107382

GET pernr.
* 1. check
  " RP-SEL-EIN-AUS. "jetzt durch RP-SEL-EIN-AUS-INIT ersetzt
* 2. check
  rp_provide_from_last p0002 space pn-begda pn-endda.
  IF pnp-sw-found NE '1'.
*   MESSAGE I101 WITH PERNR-PERNR.
    PERFORM error_handling USING pernr-pernr '103'
                                 pernr-pernr '0002' space space.
    REJECT.
  ENDIF.
  MOVE p0002-nachn TO nachn.           "XBSK11K107382
  TRANSLATE nachn TO UPPER CASE.                         "#EC TRANSLANG
  MOVE p0002-name2 TO name2.
  TRANSLATE name2 TO UPPER CASE.                         "#EC TRANSLANG
  MOVE p0002-vorna TO vorna.           "XBSK11K107382
  TRANSLATE vorna TO UPPER CASE.                         "#EC TRANSLANG

  CHECK: name,
         vorname,
         gbname,
         nation.
* neuer check wegen Radiobuttongroup                    "XWIP30K013937
  IF list2 EQ 'X'.
    CHECK p0002-gesch = '1'.
  ELSEIF list3 EQ 'X'.
    CHECK p0002-gesch = '2'.
  ENDIF.
* 3. check
  begda = pn-begda.
  endda = pn-endda.

  CALL FUNCTION 'RP_HIRE_FIRE'
       EXPORTING
            beg       = begda
            end       = endda
       IMPORTING
            hire_date = hiredate
            fire_date = firedate
       TABLES
            pp0000    = p0000  "input
            pp0001    = p0001  "input
            pphifi    = phifi. "output
  CHECK nur_neu.

* person selected
  ADD 1 TO stat-pernr.                 "statistics
  rp_provide_from_last p0001 space pn-begda pn-endda.

  PERFORM kst-hdr.

* Aufbereitung des Namens gemaess nationalen Vorschriften
  PERFORM re001p.                      "set MOLGA
  rp-edit-name p0001 p0002 t001p-molga space.  "using $$FORMAT
  IF $ret-code NE 0.
*   MESSAGE I110 WITH PERNR-PERNR.
    PERFORM error_handling USING pernr-pernr '109'
                                 pernr-pernr space space space.
  ENDIF.

* begin of PH4K013329
* Stellentext besorgen
  IF p0001-stell EQ initial_p0001_stell.
    stell_text = space.
  ELSE.
    CALL FUNCTION 'HR_READ_FOREIGN_OBJECT_TEXT'          "#EC DOM_EQUAL
         EXPORTING
              otype                   = 'C'
              objid                   = p0001-stell
              begda                   = p0001-begda
              endda                   = p0001-endda
              langu                   = sy-langu
         IMPORTING
              object_text             = stell_text
         EXCEPTIONS
              nothing_found           = 1
              wrong_objecttype        = 2
              missing_costcenter_data = 3
              missing_object_id       = 4
              OTHERS                  = 5.
    IF sy-subrc <> 0.
      CLEAR stell_text.
    ENDIF.
  ENDIF.
* end of PH4K013329

* Ausgabe: fuer TableControl display_all gefuellt.          "XIRK036103
  display_all-pernr = pernr-pernr.
  IF show_perid  = 1.                                       "XIRK100480
*    display_all-perid = p0002-perid.
  ENDIF.                                                    "XIRK100480
  display_all-name  = $edit-name.
  IF t001p-molga NE '04' AND t001p-molga NE '05'.
    display_all-name2 = p0002-name2.
    show_name2 = 1.
  ENDIF.
  display_all-stell = stell_text.
  display_all-hiredate = hiredate.                          "AHRK041517
  IF firedate NE high-date.
    display_all-firedate = firedate.                        "AHRK041517
  ELSE.
    CLEAR display_all-firedate.
  ENDIF.

* by ig.moon 7/14/2008 {
  CASE pernr-stat2.
    WHEN '0'.
      display_all-s_text = 'Withdrawn'.
    WHEN '1'.
      display_all-s_text = 'Inactive'.
    WHEN '2'.
      display_all-s_text = 'Retiree'.
    WHEN '3'.
      display_all-s_text = 'Active'.
  ENDCASE.
*  }

* Begin of HIS20094
  display_all-persg = pernr-persg.
  display_all-persk = pernr-persk.
  display_all-nachn = p0002-nachn.
  display_all-vorna = p0002-vorna.
  display_all-midnm = p0002-midnm.

  select single ptext
           into display_all-persgt
           from t501t
          where sprsl = sy-langu
            and persg = pernr-persg.

  select single ptext
           into display_all-perskt
           from t503t
          where sprsl = sy-langu
            and persk = pernr-persk.
* End of HIS20094

* BEGIN OF UD1K952410
  rp_provide_from_last p0007 space pn-begda pn-endda.
  display_all-schkz = p0007-schkz.

  select single *
    from t503
  where persg = p0001-persg
    and persk = p0001-persk.

  if sy-subrc = 0.
    select single rtext into display_all-rtext
      from t508s
     where zeity = t503-zeity
       and mofid = t001p-mofid
       and mosid = t001p-mosid
       and schkz = p0007-schkz
       and sprsl = sy-langu.
  endif.
* END OF UD1K952410
  APPEND display_all.

END-OF-SELECTION.
*--- Vorbereitungen fuer Ausgabe mit HR_DISPLAY_BASIC_LIST
*--- komplett neu                                           "XIRK036103
* Ueberschriften
  PERFORM def_header2.
* Fuellen der Ausgabe-Strukturtabelle fieldnames
  IF kostltxt NE space.                "Gruppenwechsel aktiv
    column_type = 'F'. list_level = '01'.
  ELSE.
    column_type = 'I'. list_level = '02'.
  ENDIF.
  PERFORM init_fieldnames USING space 'P0001' 'BUKRS' column_type.
  PERFORM init_fieldnames USING space 'P0001' 'WERKS' column_type.
  PERFORM init_fieldnames USING space 'P0001' 'KOSTL' column_type.
  IF kostltxt NE space.
    PERFORM init_fieldnames USING 'Text'(ktx) 'CSKT' 'KTEXT' space.
    PERFORM init_fieldnames USING 'PersNr.'(per) 'PERNR' 'PERNR' space.
  ELSE.
    PERFORM init_fieldnames USING 'Text'(ktx) 'CSKT' 'KTEXT' 'I'.
    PERFORM init_fieldnames USING 'PersNr.'(per) 'PERNR' 'PERNR' 'F'.
  ENDIF.

* perform init_fieldnames using 'PersIdNr.'(pid)            "XIRK100480
*                               'P0002' 'PERID' space.      "XIRK100480
*  CLEAR column_type.                                        "XIRK100480
*  IF show_perid NE 1. column_type = 'I'. ENDIF.             "XIRK100480
*  PERFORM init_fieldnames USING 'PersIdNr.'(pid)            "XIRK100480
*                                'P0002' 'PERID' column_type."XIRK100480
  PERFORM init_fieldnames USING 'Name'(nam) 'P0001' 'ENAME' space.
* if show_name2 eq 1.                                       "XIRK100480
*   perform init_fieldnames using                           "XIRK100480
*                'Geburtsname'(gnm) 'P0002' 'NAME2' space.  "XIRK100480
* else.                                                     "XIRK100480
*   perform init_fieldnames using                           "XIRK100480
*                'Geburtsname'(gnm) 'P0002' 'NAME2' 'I'.    "XIRK100480
* endif.                                                    "XIRK100480
  CLEAR column_type.                                        "XIRK100480
  IF show_name2 NE 1. column_type = 'I'. ENDIF.             "XIRK100480
  PERFORM init_fieldnames USING                             "XIRK100480
             'Geburtsname'(gnm) 'P0002' 'NAME2' column_type."XIRK100480
  PERFORM init_fieldnames USING 'Stellenbezeichnung'(stl)   "note 981249
                                'P1000' 'STEXT' space.      "note 981249
  PERFORM init_fieldnames USING space 'PRELQ' 'HIREDATE' space.
  PERFORM init_fieldnames USING space 'PRELQ' 'FIREDATE' space.


* by ig.moon 7/14/2008 {
  PERFORM init_fieldnames USING 'Status' 'S_TEXT' 'S_TEXT' space.
* }

* Begin of HIS20094
  PERFORM init_fieldnames USING space 'PERNR' 'PERSG' space.
  PERFORM init_fieldnames USING space 'T501T' 'PTEXT' space.
  PERFORM init_fieldnames USING space 'PERNR' 'PERSK' space.
  PERFORM init_fieldnames USING space 'T503T' 'PTEXT' space.
  PERFORM init_fieldnames USING space 'P0002' 'NACHN' space.
  PERFORM init_fieldnames USING space 'P0002' 'VORNA' space.
  PERFORM init_fieldnames USING space 'P0002' 'MIDNM' space.
  PERFORM init_fieldnames USING space 'P0007' 'SCHKZ' space.
  PERFORM init_fieldnames USING space 'T508S' 'RTEXT' space.
* End of HIS20094

*----- Ende der Vorbereitungen

  PERFORM write-statistic.
  DESCRIBE TABLE display_err LINES stat-err.
  SORT display_err.

* Sonderbehandlung, wenn keine MA selektiert wurden        "XIRK049740
  IF stat-err EQ 0 AND stat-pernr EQ 0.                     "XIRK049740
    MESSAGE i050.                                           "XIRK049740
  ENDIF.                                                    "XIRK049740
*
* Ausgabeschleife                                          "XIRK036103
  CLEAR return_code.
  WHILE return_code <> 12.
*    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'                " note 0442504
    CALL FUNCTION 'DISPLAY_BASIC_LIST'                    " note 0442504
         EXPORTING
              lay_out          = 0
              head_line1       = header2                    "AHRK041517
              head_line2       = footnote1                "note 0503800
              head_line3       = footnote2                "note 0503800
              head_line4       = footnote3                "note 0503800
              basic_list_title = sy-title
              file_name        = myreport
              current_report   = myreport
              list_level       = list_level
         IMPORTING
              return_code      = return_code
         TABLES
              data_tab         = display_all
              fieldname_tab    = fieldnames
              error_tab        = display_err
         EXCEPTIONS
              print_problems   = 5
              OTHERS           = 1.
    IF sy-subrc EQ 5. PERFORM write_warning. ENDIF.
    IF return_code EQ 0. return_code = 12. ENDIF.
    IF NOT ( sy-batch IS INITIAL ).    "fuer Batchaufruf
      return_code = 12.
    ENDIF.
  ENDWHILE.

*---------------------------------------------------------------------*
*       FORM KST-HDR                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
* Umstellung der Ausgabe auf TableControl                   "XIRK036103
FORM kst-hdr.
  display_all-bukrs = p0001-bukrs.
  display_all-werks = p0001-werks.
  display_all-kostl = p0001-kostl.

* CHECK kostltxt NE space.
  MOVE-CORRESPONDING p0001 TO neu.
  IF alt NE neu.
    ADD 1 TO stat-kostl.               "statistics
* Ausgabe von BUKRS/WERKS/KOSTL als Spalten im TableControl

    PERFORM get_cost_center_text IN PROGRAM rplico90
            USING costcentertext
                  p0001-bukrs
                  p0001-gsber
                  p0001-begda
                  p0001-endda                               "PH4K007936
                  p0001-kostl.
*    display_all-bukrs = p0001-bukrs.
*    display_all-werks = p0001-werks.
*    display_all-kostl = p0001-kostl.
    display_all-kosttext = costcentertext.
    alt = neu.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM RE001P                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM re001p.
* QNUR3 BUKRS entfernt
  CHECK t001p-werks NE p0001-werks OR t001p-btrtl NE p0001-btrtl.
  SELECT SINGLE * FROM t001p WHERE werks EQ p0001-werks
                               AND btrtl EQ p0001-btrtl.
  IF sy-subrc NE 0.
    t001p-werks = p0001-werks.
    t001p-btrtl = p0001-btrtl.
*   MESSAGE E001 WITH 'T001P' T001P-ARG.
    PERFORM error_handling USING pernr-pernr '001'
                                 t001p+3(8) space space space.
    CLEAR t001p.                       "XIRP30K07943
  ENDIF.
ENDFORM.                                                    "RE001P.

*---------------------------------------------------------------------*
*       FORM CHECK-INPUT                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
* Ausgabeanweisungen an TC angepasst                        "XIRK036103
FORM check-input.
  CLEAR: footnote2, footnote3.
  IF kostltxt NE space.
    IF NOT ( pnpsortf CS 'KOSTL' ).
* Warnung wird als Fussnote ausgegeben.
*      footnote2 = text-er1.                                "XIRK091036
*      footnote3 = text-er2.                                "XIRK091036
*      CONCATENATE text-er1 text-er2                        "XIRK091036
*                  INTO footnote3 SEPARATED BY space.       "XIRK091036
      footnote3 = text-er1.                                 "PH4K018719
    ENDIF.
  ENDIF.
ENDFORM.                               "CHECK-INPUT.

*---------------------------------------------------------------------*
*       FORM WRITE-STATISTIC                                           *
*---------------------------------------------------------------------*
*       Statistikaufbereitung fuer die Fussnoten                       *
*---------------------------------------------------------------------*
FORM write-statistic.
* Statt write-Statements, fuellen der Fussnoten             "XIRK036103
  DATA: anzahl(8).
  CLEAR footnote1.
  CLEAR footnote2.                                          "XIRK091036
  WRITE stat-pernr TO anzahl LEFT-JUSTIFIED NO-SIGN.
* concatenate text-003 text-004 anzahl text-000             "XIRK091036
*             into footnote1 separated by space.            "XIRK091036
  CONCATENATE 'Anzahl selektierter Mitarbeiter:'(fn1)       "XIRK091036
              anzahl INTO footnote1 SEPARATED BY space.     "XIRK091036
  IF kostltxt NE space.
    WRITE stat-kostl TO anzahl LEFT-JUSTIFIED NO-SIGN.      "XIRK091036
*   concatenate footnote1 text-005 anzahl text-001          "XIRK091036
*               into footnote1 separated by space.          "XIRK091036
    CONCATENATE 'Anzahl selektierter Kostenstelle(n):'(fn2) "XIRK091036
               anzahl INTO footnote2 SEPARATED BY space.    "XIRK091036
  ENDIF.
ENDFORM.                               "WRITE-STATISTIC.

*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDNAMES                     "neu     "XIRK036103
*&---------------------------------------------------------------------*
*       Fuellen der Strukturtabelle fieldnames.                        *
*----------------------------------------------------------------------*
*  -->  field1, field2, field3, field4.
*----------------------------------------------------------------------*
FORM init_fieldnames USING field1 field2 field3 field4.
  fieldnames-text = field1.
  fieldnames-tabname = field2.
  fieldnames-fieldname = field3.
  fieldnames-typ = field4.
  APPEND fieldnames. CLEAR fieldnames.
ENDFORM.                               " INIT_FIELDNAMES
*&---------------------------------------------------------------------*
*&      Form  DEF_HEADER2                           "neu   "XIRK036103
*&---------------------------------------------------------------------*
*       Ueberschriftszeile fuer Zeitraum/Stichtag definieren.          *
*----------------------------------------------------------------------*
FORM def_header2.
  IF pn-begda EQ pn-endda.
    header2 = 'Stichtag:'(stg).
    WRITE pn-begda TO datum DD/MM/YYYY.
    CONCATENATE header2 datum INTO header2
                                   SEPARATED BY space.
  ELSE.
    header2 = text-zrm.
    WRITE pn-begda TO datum DD/MM/YYYY.
    REPLACE '$1' WITH datum INTO header2.
    WRITE pn-endda TO datum DD/MM/YYYY.
    REPLACE '$2' WITH datum INTO header2.
  ENDIF.
ENDFORM.                               " DEF_HEADER2
*&---------------------------------------------------------------------*
*&      Form  WRITE_WARNING                 "neu       "XIRK036103
*&---------------------------------------------------------------------*
*       Warnung, falls Liste breiter als Druckeraufloesung             *
*----------------------------------------------------------------------*
FORM write_warning.
  WRITE: / 'Keine Listausgabe, da Listbreite nicht zum'(ex1),
         / 'Druckerformat passt.'(ex2),
         / 'Bitte passen Sie die Listbreite im TableControl'(ex3),
         / 'dem Druckerformat an.'(ex4).
ENDFORM.                               " WRITE_WARNING
*&---------------------------------------------------------------------*
*&      Form  ERROR_HANDLING             "neu           "XIRK049740
*&---------------------------------------------------------------------*
*       Fuellen der Fehlertabelle                                      *
*----------------------------------------------------------------------*
FORM error_handling USING pernr msgno msgv1 msgv2 msgv3 msgv4.
  CLEAR display_err.
  display_err-pernr = pernr.
  display_err-arbgb = '72'.
  display_err-msgty = 'E'.
  display_err-msgno = msgno.
  display_err-msgv1 = msgv1.
  display_err-msgv2 = msgv2.
  display_err-msgv3 = msgv3.
  display_err-msgv4 = msgv4.
  APPEND display_err.
ENDFORM.                               " ERROR_HANDLING
