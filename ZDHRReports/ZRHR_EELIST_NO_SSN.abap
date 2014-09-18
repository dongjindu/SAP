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


report zrhr_eelist_no_ssn using database pnp message-id pn.
tables: pernr.
tables: t001p,
        prelq.                                              "AHRK052167
tables: t503.                                               "UD1K952410

infotypes: 0000, "Massnahmen (wg. STAT2 fuer HIRE+FIRE)
           0001,                       "org. Zuordnung
           0002, "Daten zur Person (insbesondere Name)
           0003,                       "Abrechnungsstatus
           0007.                                            "UD1K952410

*---  Daten fuer Funktionsbaustein RP_HIRE+FIRE
data: hiredate like rptxxxxx-datum1,   "statt type d.      "XIRK036103
      firedate like rptxxxxx-datum1.   "statt type d.      "XIRK036103
data: begin of phifi occurs 5.
        include structure phifi.
data: end of phifi.

data: nachn like p0002-nachn,          "XBSK11K107382
      name2 like p0002-name2,          "XBSK11K107382
      vorna like p0002-vorna.          "XBSK11K107382

*----  Selektionsbildschirm definieren
selection-screen begin of block frm1 with frame title frametxt.
"XWIP30K013937
select-options:
  name     for nachn no intervals,                          "AHRK052167
  gbname   for name2 no intervals,                          "AHRK052167
  vorname  for vorna no intervals,                          "AHRK052167
  nation   for p0002-natio no intervals,                    "AHRK052167
  nur_neu  for hiredate.

* Radiobuttonblock fuer Geschlechter-Auswahl             "XWIP30K013937
selection-screen begin of block frm2 with frame title text-gsl.
selection-screen begin of line.
selection-screen position 01.
parameters:
  list1 like rplxxxxx-list radiobutton group list default 'X'.
selection-screen comment 03(20) text-s11.
selection-screen position 33.
parameters:
  list2 like rplxxxxx-list radiobutton group list.
selection-screen comment 35(15) text-s12.
selection-screen position 58.
parameters:
  list3 like rplxxxxx-list radiobutton group list.
selection-screen comment 60(15) text-s13.
selection-screen end of line.
selection-screen end of block frm2.
selection-screen end of block frm1.    "XWIP30K013937

* Rahmen fuer Listenaufbereitung                         "XIRK036103
selection-screen begin of block frm3 with frame title text-lst.
* checkbox fuer Kostenstellentext
parameters: kostltxt like rplxxxxx-kostltxt default space.
selection-screen end of block frm3.

*---  Konstanten
rp-lowdate-highdate.
*---  Daten fuer RMAC-Baustein RP_EDIT_NAME
data: $edit-name(40),
      $ret-code like sy-subrc,
      $length(2) type n.
*---  Daten fuer Stellentext
* DATA: stell_text LIKE t513s-stltx,                        "PH4K013329
data: stell_text like p1000-stext,                          "PH4K013329
      initial_p0001_stell like p0001-stell.
*---  Daten fuer Gruppenwechsel BUKRS/WERKS/KOSTL
data: begin of neu,
        bukrs like p0001-bukrs,
        werks like p0001-werks,                             "XIRK036103
*       gsber like p0001-gsber,        "del                 "XIRK036103
        kostl like p0001-kostl,
      end of neu,
      alt like neu.

data:
* COSTCENTERTEXT LIKE HRCA_COSTC-SHORTTNAME,                  AHRK038889
      costcentertext like hrca_costc-name,                  "AHRK038889
      begda          like prel-begda,
      endda          like prel-endda.

*---  Statistikfelder
data: begin of stat,
        pernr type p,
        kostl type p,
        err   type p,                                       "XIRK036103
      end of stat.

*---- Daten fuer den FuBaustein HR_DISPLAY_BASIC_LIST       "XIRK036103
*---- (--> Listausgabe ueber TableControl)
data: header2(132),                    "Ueberschrift
      footnote1(132), footnote2(132),  "Fussnoten
      footnote3(132),                  "Fussnoten
      myreport like sy-repid,          "Reportname
      return_code like sy-subrc,       "Returncode
      datum(10),                       "Hilfsfeld
      show_perid type i,               "PERID anzeigen?     "XIRK100480
      show_name2 type i value 0,       "Geburtsname anzeigen?
      column_type,                     "Typ der Spalte
      list_level(2).                   "Listenlevel
* Mitarbeiterliste mit Gruppenwechsel BUKRS/WERKS/KOSTL
data: begin of display_all occurs 100,
        bukrs like p0001-bukrs,
        werks like p0001-werks,
        kostl like p0001-kostl,
        kosttext(30),
        pernr like pernr-pernr,
*        perid LIKE p0002-perid,
        name like p0001-ename,
        name2 like p0002-name2,
*       stell LIKE t513s-stltx,                             "PH4K013329
        stell like p1000-stext,                             "PH4K013329
        hiredate like prelq-hiredate,                       "AHRK041517
        firedate like prelq-firedate,                       "AHRK041517
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
      end of display_all.
* }
* Fehlerliste
data: begin of display_err occurs 30.
        include structure hrerror.
data: end of display_err.
* Ausgabe-Steuerungstabelle fuer display_all.
data: begin of fieldnames occurs 10,
        text(60), tabname(10), fieldname(10), typ,
      end of fieldnames.

initialization.
  frametxt = text-wse. "statt pnp_spec_standard_frametxt. "XWIP30K013937
* RP-INIT-WITH-SYDATUM.                               "YFGK11K084002
  pnptimed = 'D'.                      "YFGK11K084002
  rp-sel-ein-aus-init.

start-of-selection.
  rp_set_data_interval 'P0002' pn-begda pn-endda.           "XJFK138520

  myreport = sy-repid.                                      "XIRK036103
  perform check-input.
  rp-set-name-format.   "Read T522F / $$FORMAT = '01' fuer RP_EDIT_NAME
* rp-fetch-alter-perid.  "Call Function RP_FETCH_ALTER_PERNR"XIRK100480
* stattdessen direkter Aufruf des FBSt.                     "XIRK100480
  call function 'RP_FETCH_ALTERNATE_PERNR'                  "XIRK100480
       importing                                            "XIRK100480
            alter_pernr = show_perid                        "XIRK100480
            retcode     = return_code                       "XIRK100480
       exceptions                                           "XIRK100480
            others      = 1.                                "XIRK100480

  if sy-subrc <> 0.
    clear show_perid.
  endif.

  loop at name.                        "XBSK11K107382
    translate name-low  to upper case. "XBSK11K107382
    translate name-high to upper case. "XBSK11K107382
    shift name-low left deleting leading space.             "AHRK025257
    shift name-high left deleting leading space.            "AHRK025257
    modify name.                       "XBSK11K107382
  endloop.                             "XBSK11K107382

  loop at vorname.                     "XBSK11K107382
    translate vorname-low  to upper case.                "XBSK11K107382
    translate vorname-high to upper case.                "XBSK11K107382
    shift vorname-low left deleting leading space.          "AHRK025257
    shift vorname-high left deleting leading space.         "AHRK025257
    modify vorname.                    "XBSK11K107382
  endloop.                             "XBSK11K107382

  loop at gbname.                      "XBSK11K107382
    translate gbname-low  to upper case.                 "XBSK11K107382
    translate gbname-high to upper case.                 "XBSK11K107382
    shift gbname-low left deleting leading space.           "AHRK025257
    shift gbname-high left deleting leading space.          "AHRK025257
    modify gbname.                     "XBSK11K107382
  endloop.                             "XBSK11K107382

get pernr.
* 1. check
  " RP-SEL-EIN-AUS. "jetzt durch RP-SEL-EIN-AUS-INIT ersetzt
* 2. check
  rp_provide_from_last p0002 space pn-begda pn-endda.
  if pnp-sw-found ne '1'.
*   MESSAGE I101 WITH PERNR-PERNR.
    perform error_handling using pernr-pernr '103'
                                 pernr-pernr '0002' space space.
    reject.
  endif.
  move p0002-nachn to nachn.           "XBSK11K107382
  translate nachn to upper case.                         "#EC TRANSLANG
  move p0002-name2 to name2.
  translate name2 to upper case.                         "#EC TRANSLANG
  move p0002-vorna to vorna.           "XBSK11K107382
  translate vorna to upper case.                         "#EC TRANSLANG

  check: name,
         vorname,
         gbname,
         nation.
* neuer check wegen Radiobuttongroup                    "XWIP30K013937
  if list2 eq 'X'.
    check p0002-gesch = '1'.
  elseif list3 eq 'X'.
    check p0002-gesch = '2'.
  endif.
* 3. check
  begda = pn-begda.
  endda = pn-endda.

  call function 'RP_HIRE_FIRE'
    exporting
      beg       = begda
      end       = endda
    importing
      hire_date = hiredate
      fire_date = firedate
    tables
      pp0000    = p0000  "input
      pp0001    = p0001  "input
      pphifi    = phifi. "output
  check nur_neu.

* person selected
  add 1 to stat-pernr.                 "statistics
  rp_provide_from_last p0001 space pn-begda pn-endda.

  perform kst-hdr.

* Aufbereitung des Namens gemaess nationalen Vorschriften
  perform re001p.                      "set MOLGA
  rp-edit-name p0001 p0002 t001p-molga space.  "using $$FORMAT
  if $ret-code ne 0.
*   MESSAGE I110 WITH PERNR-PERNR.
    perform error_handling using pernr-pernr '109'
                                 pernr-pernr space space space.
  endif.

* begin of PH4K013329
* Stellentext besorgen
  if p0001-stell eq initial_p0001_stell.
    stell_text = space.
  else.
    call function 'HR_READ_FOREIGN_OBJECT_TEXT'          "#EC DOM_EQUAL
         exporting
              otype                   = 'C'
              objid                   = p0001-stell
              begda                   = p0001-begda
              endda                   = p0001-endda
              langu                   = sy-langu
         importing
              object_text             = stell_text
         exceptions
              nothing_found           = 1
              wrong_objecttype        = 2
              missing_costcenter_data = 3
              missing_object_id       = 4
              others                  = 5.
    if sy-subrc <> 0.
      clear stell_text.
    endif.
  endif.
* end of PH4K013329

* Ausgabe: fuer TableControl display_all gefuellt.          "XIRK036103
  display_all-pernr = pernr-pernr.
*  IF show_perid  = 1.                                       "XIRK100480
*    display_all-perid = p0002-perid.
*  ENDIF.                                                    "XIRK100480
  display_all-name  = $edit-name.
  if t001p-molga ne '04' and t001p-molga ne '05'.
    display_all-name2 = p0002-name2.
    show_name2 = 1.
  endif.
  display_all-stell = stell_text.
  display_all-hiredate = hiredate.                          "AHRK041517
  if firedate ne high-date.
    display_all-firedate = firedate.                        "AHRK041517
  else.
    clear display_all-firedate.
  endif.

* by ig.moon 7/14/2008 {
  case pernr-stat2.
    when '0'.
      display_all-s_text = 'Withdrawn'.
    when '1'.
      display_all-s_text = 'Inactive'.
    when '2'.
      display_all-s_text = 'Retiree'.
    when '3'.
      display_all-s_text = 'Active'.
  endcase.
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
  append display_all.

end-of-selection.
*--- Vorbereitungen fuer Ausgabe mit HR_DISPLAY_BASIC_LIST
*--- komplett neu                                           "XIRK036103
* Ueberschriften
  perform def_header2.
* Fuellen der Ausgabe-Strukturtabelle fieldnames
  if kostltxt ne space.                "Gruppenwechsel aktiv
    column_type = 'F'. list_level = '01'.
  else.
    column_type = 'I'. list_level = '02'.
  endif.
  perform init_fieldnames using space 'P0001' 'BUKRS' column_type.
  perform init_fieldnames using space 'P0001' 'WERKS' column_type.
  perform init_fieldnames using space 'P0001' 'KOSTL' column_type.
  if kostltxt ne space.
    perform init_fieldnames using 'Text'(ktx) 'CSKT' 'KTEXT' space.
    perform init_fieldnames using 'PersNr.'(per) 'PERNR' 'PERNR' space.
  else.
    perform init_fieldnames using 'Text'(ktx) 'CSKT' 'KTEXT' 'I'.
    perform init_fieldnames using 'PersNr.'(per) 'PERNR' 'PERNR' 'F'.
  endif.
*
* perform init_fieldnames using 'PersIdNr.'(pid)            "XIRK100480
*                               'P0002' 'PERID' space.      "XIRK100480
*  CLEAR column_type.                                        "XIRK100480
*  IF show_perid NE 1. column_type = 'I'. ENDIF.             "XIRK100480
*  PERFORM init_fieldnames USING 'PersIdNr.'(pid)            "XIRK100480
*                                'P0002' 'PERID' column_type."XIRK100480
  perform init_fieldnames using 'Name'(nam) 'P0001' 'ENAME' space.
* if show_name2 eq 1.                                       "XIRK100480
*   perform init_fieldnames using                           "XIRK100480
*                'Geburtsname'(gnm) 'P0002' 'NAME2' space.  "XIRK100480
* else.                                                     "XIRK100480
*   perform init_fieldnames using                           "XIRK100480
*                'Geburtsname'(gnm) 'P0002' 'NAME2' 'I'.    "XIRK100480
* endif.                                                    "XIRK100480
  clear column_type.                                        "XIRK100480
  if show_name2 ne 1. column_type = 'I'. endif.             "XIRK100480
  perform init_fieldnames using                             "XIRK100480
             'Geburtsname'(gnm) 'P0002' 'NAME2' column_type. "XIRK100480
  perform init_fieldnames using 'Stellenbezeichnung'(stl)   "note 981249
                                'P1000' 'STEXT' space.      "note 981249
  perform init_fieldnames using space 'PRELQ' 'HIREDATE' space.
  perform init_fieldnames using space 'PRELQ' 'FIREDATE' space.


* by ig.moon 7/14/2008 {
  perform init_fieldnames using 'Status' 'S_TEXT' 'S_TEXT' space.
* }

* Begin of HIS20094
  perform init_fieldnames using space 'PERNR' 'PERSG' space.
  perform init_fieldnames using space 'T501T' 'PTEXT' space.
  perform init_fieldnames using space 'PERNR' 'PERSK' space.
  perform init_fieldnames using space 'T503T' 'PTEXT' space.
  perform init_fieldnames using space 'P0002' 'NACHN' space.
  perform init_fieldnames using space 'P0002' 'VORNA' space.
  perform init_fieldnames using space 'P0002' 'MIDNM' space.
  perform init_fieldnames using space 'P0007' 'SCHKZ' space.
  perform init_fieldnames using space 'T508S' 'RTEXT' space.
* End of HIS20094

*----- Ende der Vorbereitungen

  perform write-statistic.
  describe table display_err lines stat-err.
  sort display_err.

* Sonderbehandlung, wenn keine MA selektiert wurden        "XIRK049740
  if stat-err eq 0 and stat-pernr eq 0.                     "XIRK049740
    message i050.                                           "XIRK049740
  endif.                                                    "XIRK049740
*
* Ausgabeschleife                                          "XIRK036103
  clear return_code.
  while return_code <> 12.
*    CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'                " note 0442504
    call function 'DISPLAY_BASIC_LIST'                    " note 0442504
         exporting
              lay_out          = 0
              head_line1       = header2                    "AHRK041517
              head_line2       = footnote1                "note 0503800
              head_line3       = footnote2                "note 0503800
              head_line4       = footnote3                "note 0503800
              basic_list_title = sy-title
              file_name        = myreport
              current_report   = myreport
              list_level       = list_level
         importing
              return_code      = return_code
         tables
              data_tab         = display_all
              fieldname_tab    = fieldnames
              error_tab        = display_err
         exceptions
              print_problems   = 5
              others           = 1.
    if sy-subrc eq 5. perform write_warning. endif.
    if return_code eq 0. return_code = 12. endif.
    if not ( sy-batch is initial ).    "fuer Batchaufruf
      return_code = 12.
    endif.
  endwhile.

*---------------------------------------------------------------------*
*       FORM KST-HDR                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
* Umstellung der Ausgabe auf TableControl                   "XIRK036103
form kst-hdr.
  display_all-bukrs = p0001-bukrs.
  display_all-werks = p0001-werks.
  display_all-kostl = p0001-kostl.

* CHECK kostltxt NE space.
  move-corresponding p0001 to neu.
  if alt ne neu.
    add 1 to stat-kostl.               "statistics
* Ausgabe von BUKRS/WERKS/KOSTL als Spalten im TableControl

    perform get_cost_center_text in program rplico90
            using costcentertext
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
  endif.
endform.                    "kst-hdr


*---------------------------------------------------------------------*
*       FORM RE001P                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form re001p.
* QNUR3 BUKRS entfernt
  check t001p-werks ne p0001-werks or t001p-btrtl ne p0001-btrtl.
  select single * from t001p where werks eq p0001-werks
                               and btrtl eq p0001-btrtl.
  if sy-subrc ne 0.
    t001p-werks = p0001-werks.
    t001p-btrtl = p0001-btrtl.
*   MESSAGE E001 WITH 'T001P' T001P-ARG.
    perform error_handling using pernr-pernr '001'
                                 t001p+3(8) space space space.
    clear t001p.                       "XIRP30K07943
  endif.
endform.                                                    "RE001P.

*---------------------------------------------------------------------*
*       FORM CHECK-INPUT                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
* Ausgabeanweisungen an TC angepasst                        "XIRK036103
form check-input.
  clear: footnote2, footnote3.
  if kostltxt ne space.
    if not ( pnpsortf cs 'KOSTL' ).
* Warnung wird als Fussnote ausgegeben.
*      footnote2 = text-er1.                                "XIRK091036
*      footnote3 = text-er2.                                "XIRK091036
*      CONCATENATE text-er1 text-er2                        "XIRK091036
*                  INTO footnote3 SEPARATED BY space.       "XIRK091036
      footnote3 = text-er1.                                 "PH4K018719
    endif.
  endif.
endform.                               "CHECK-INPUT.

*---------------------------------------------------------------------*
*       FORM WRITE-STATISTIC                                           *
*---------------------------------------------------------------------*
*       Statistikaufbereitung fuer die Fussnoten                       *
*---------------------------------------------------------------------*
form write-statistic.
* Statt write-Statements, fuellen der Fussnoten             "XIRK036103
  data: anzahl(8).
  clear footnote1.
  clear footnote2.                                          "XIRK091036
  write stat-pernr to anzahl left-justified no-sign.
* concatenate text-003 text-004 anzahl text-000             "XIRK091036
*             into footnote1 separated by space.            "XIRK091036
  concatenate 'Anzahl selektierter Mitarbeiter:'(fn1)       "XIRK091036
              anzahl into footnote1 separated by space.     "XIRK091036
  if kostltxt ne space.
    write stat-kostl to anzahl left-justified no-sign.      "XIRK091036
*   concatenate footnote1 text-005 anzahl text-001          "XIRK091036
*               into footnote1 separated by space.          "XIRK091036
    concatenate 'Anzahl selektierter Kostenstelle(n):'(fn2) "XIRK091036
               anzahl into footnote2 separated by space.    "XIRK091036
  endif.
endform.                               "WRITE-STATISTIC.

*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDNAMES                     "neu     "XIRK036103
*&---------------------------------------------------------------------*
*       Fuellen der Strukturtabelle fieldnames.                        *
*----------------------------------------------------------------------*
*  -->  field1, field2, field3, field4.
*----------------------------------------------------------------------*
form init_fieldnames using field1 field2 field3 field4.
  fieldnames-text = field1.
  fieldnames-tabname = field2.
  fieldnames-fieldname = field3.
  fieldnames-typ = field4.
  append fieldnames. clear fieldnames.
endform.                               " INIT_FIELDNAMES
*&---------------------------------------------------------------------*
*&      Form  DEF_HEADER2                           "neu   "XIRK036103
*&---------------------------------------------------------------------*
*       Ueberschriftszeile fuer Zeitraum/Stichtag definieren.          *
*----------------------------------------------------------------------*
form def_header2.
  if pn-begda eq pn-endda.
    header2 = 'Stichtag:'(stg).
    write pn-begda to datum dd/mm/yyyy.
    concatenate header2 datum into header2
                                   separated by space.
  else.
    header2 = text-zrm.
    write pn-begda to datum dd/mm/yyyy.
    replace '$1' with datum into header2.
    write pn-endda to datum dd/mm/yyyy.
    replace '$2' with datum into header2.
  endif.
endform.                               " DEF_HEADER2
*&---------------------------------------------------------------------*
*&      Form  WRITE_WARNING                 "neu       "XIRK036103
*&---------------------------------------------------------------------*
*       Warnung, falls Liste breiter als Druckeraufloesung             *
*----------------------------------------------------------------------*
form write_warning.
  write: / 'Keine Listausgabe, da Listbreite nicht zum'(ex1),
         / 'Druckerformat passt.'(ex2),
         / 'Bitte passen Sie die Listbreite im TableControl'(ex3),
         / 'dem Druckerformat an.'(ex4).
endform.                               " WRITE_WARNING
*&---------------------------------------------------------------------*
*&      Form  ERROR_HANDLING             "neu           "XIRK049740
*&---------------------------------------------------------------------*
*       Fuellen der Fehlertabelle                                      *
*----------------------------------------------------------------------*
form error_handling using pernr msgno msgv1 msgv2 msgv3 msgv4.
  clear display_err.
  display_err-pernr = pernr.
  display_err-arbgb = '72'.
  display_err-msgty = 'E'.
  display_err-msgno = msgno.
  display_err-msgv1 = msgv1.
  display_err-msgv2 = msgv2.
  display_err-msgv3 = msgv3.
  display_err-msgv4 = msgv4.
  append display_err.
endform.                               " ERROR_HANDLING
