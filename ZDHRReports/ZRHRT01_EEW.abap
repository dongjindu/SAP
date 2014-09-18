*&---------------------------------------------------------------------*
* Program Name      : ZRHRT01_EEW                                      *
* Author            : Ho-Joong, Hwang                                  *
* Creation Date     : 2003.10.30                                       *
* Specifications By : Ho-Joong, Hwang                                  *
* Pattern           : Report 1-1                                       *
* Development Request No : 4.6C UD1K903483                             *
* Addl Documentation:                                                  *
* Description       : EE Working Report after 1/2hr Shift Starting     *
*                                                                      *
* Modification Logs                                                    *
* Date       Developer    RequestNo    Description                     *
*                                                                      *
*&---------------------------------------------------------------------*
REPORT ZRHRT01_EEW MESSAGE-ID ZMHR
                   NO STANDARD PAGE HEADING
                   LINE-SIZE 200
                   LINE-COUNT 65.
************************************************************************
*                          DATA SOURCES                                *
************************************************************************

tables: pernr,           " Standard Selections for HR Master Data Report
        pa0001,          " Infotype 0001 (Org. Assignment)
        pa0007,          " Infotype 0007 (Planned Working Time)
        pa2001,          " HR Time Record: Infotype 2001 (Absences)
        pa2002,          " HR Time Record: Infotype 2002 (Attendances)
        t552a,           " Monthly Work Schedule
        t001p,           " Personnel Area/Subarea
        asshr,           " Assignment of HR to AFRU/ASSOB
        assob,           " Assignment Objects
        t503,            " Employee Group/Subgroup
        catsdb,          " CATS: Database Table for Time Sheet
        hrp1000,         " Infotype 1000 DB Table
        hrp1001,         " Infotype 1001 DB Table
        zthr_1001,       " EE working report - Org. unit maintenance
        zthr_shift.      " shift schedule type maintenance

type-pools vrm.

************************************************************************
*                           VARIABLES                                  *
************************************************************************

*... internal tables
data: begin of it_hirar occurs 0,     " use for get sub-division
      orgeh    like hrp1000-objid,
      grad1    type i,
      parnt    like hrp1000-objid,
      grad2.
data: end of it_hirar.

data: begin of it_p0001 occurs 0,
      orgeh    like pa0001-orgeh,
      pernr    like pa0001-pernr,
      begzt    like pdpsp-begzt.
data: end of it_p0001.

data: it_avail like pdpsp occurs 0 with header line.

data: begin of it_colec occurs 0,
      orgeh    like pa0001-orgeh,
      short    like hrp1000-short,
      stext    like hrp1000-stext,
      total    type i,
      atten    type i,
      absen    type i,
      fild1    type i,
      fild2    type i,
      fild3    type i,
      fild4    type i,
      fild5    type i,
      fild6    type i,
      fild7    type i,
      fild8    type i,
      fild9    type i,
      fild0    type i,
      filda    type i.
data: end of it_colec.

data: tmp_cole like it_colec occurs 0 with header line.

data: DD07V_TAB LIKE DD07V OCCURS 0 WITH HEADER LINE,
      dy_field(72)         occurs 0 with header line.

*... possibel entry
data: it_units   like rhldapo  occurs 0 with header line,
      it_persn   like rhldapp  occurs 0 with header line,
      it_ogper   like rhldapop occurs 0 with header line.

data: begin of it_value occurs 0,
      short    like hrp1000-short,
      stext    like hrp1000-stext,
      objid    like hrp1000-objid.
data: end of it_value.

data: it_field   like help_value occurs 1 with header line,
      dynpfields like standard table of dynpread with header line.

*... variants
data: w_otype    like objec-otype,    " object type
      w_wegid    like gdstr-wegid,    " evaluation path
      w_plvar    like objec-plvar,    " plan version
      w_orgeh    like pa0001-orgeh,
      w_tprxx    like t552a-tpr01,
      w_title(50),
      w_tdate(10).

data: name  type vrm_id,
      list  type vrm_values,
      value like line of list.

data: w_fname    like help_info-fieldname,
      w_tabix    like sy-tabix,
      w_fldvl    like help_info-fldvalue,
      w_answr,
      w_count    type i,
      w_width    type i,
      w_objid    like pa0001-orgeh.

*...range
ranges: r_orgeh  for hrp1000-objid.

************************************************************************
*                           PARAMETERS                                 *
************************************************************************

*... frame : Reporting date
selection-screen begin of block frm1 with frame title text-f01.
  parameters: p_rptda    like sy-datum obligatory.
selection-screen end of block frm1.

*... frame : Org. unit & name
selection-screen begin of block frm2 with frame title text-f02.
selection-screen begin of line.
selection-screen comment 1(31) text-001 for field p_short.
  parameters: p_short    like hrp1000-short obligatory.
selection-screen position 47.
  parameters: p_stext    like hrp1000-stext modif id pe1.
selection-screen end of line.
selection-screen end of block frm2.

*... frame : Shift
selection-screen begin of block frm3 with frame title text-f03.
  parameters: p_schkz(15) as listbox visible length 15 obligatory.
selection-screen end of block frm3.

************************************************************************
*                           INITIALIZATION                             *
************************************************************************

initialization.
  w_otype = 'O'.
  w_wegid = 'ORGEH'.
  w_plvar = '01'.

************************************************************************
*                        AT SELECTION-SCREEN                           *
************************************************************************

at selection-screen.
*... get sub division
  perform get_sub_division.

at selection-screen output.
*... shift schedule list box
  name = 'P_SCHKZ'.
  DD07V_TAB-DOMNAME = 'ZSHIFT'.
  call function 'GET_DOMAIN_VALUES'
       exporting  domname    = dd07v_tab-domname
       tables     values_tab = dd07v_tab
       exceptions others     = 1.

  loop at dd07v_tab.
    value-key  = dd07v_tab-domvalue_l.
    value-text = dd07v_tab-ddtext.
    append value to list.
  endloop.

  call function 'VRM_SET_VALUES'
       exporting id     = name
                 values = list.

*... screen modify
  loop at screen.
    if screen-group1 = 'PE1'.
      screen-input = 0.
      screen-display_3d = 0.
      screen-intensified = 1.
      modify screen.
    endif.
  endloop.

at selection-screen on value-request for p_short.
  perform check_maintenance_view.
* perform pop_up_org_unit.

************************************************************************
*                         TOP-OF-PAGE                                  *
************************************************************************

top-of-page.
  perform heading.

************************************************************************
*                         START-OF-SELECTION                           *
************************************************************************

start-of-selection.
*... 1. select data
  perform get_pernr_by_select_option.
*... 2. collect data
  perform sum_of_selected_date.

************************************************************************
*                         END-OF-SELECTION                             *
************************************************************************

end-of-selection.
  describe table it_colec lines sy-tfill.
  if sy-tfill = 0.
    message s001 with 'No data selected'.
  else.
*   perform write_body.
    perform write_body_new_form.
  endif.

************************************************************************
*                            SUBROUTINES                               *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  pop_up_org_unit
*&---------------------------------------------------------------------*
FORM pop_up_org_unit.
  clear it_value. refresh it_value.
*
  clear zthr_1001.
  select objid short stext
    into (zthr_1001-objid, zthr_1001-short, zthr_1001-stext)
    from zthr_1001 where zmark = space.
    it_value-short = zthr_1001-short.
    it_value-stext = zthr_1001-stext.
    it_value-objid = zthr_1001-objid.
    append it_value. clear it_value.
  endselect.
*
  clear it_field. refresh it_field.
*
  it_field-tabname   = 'ZTHR_1001'.
  it_field-fieldname = 'SHORT'.
  it_FIELD-selectflag = 'X'.
  append it_field. clear it_field.

  it_field-tabname   = 'ZTHR_1001'.
  it_field-fieldname = 'STEXT'.
  it_FIELD-selectflag = ' '.
  append it_field. clear it_field.

  it_field-tabname   = 'ZTHR_1001'.
  it_field-fieldname = 'OBJID'.
  it_FIELD-selectflag = ' '.
  append it_field. clear it_field.
*
  call function 'HELP_VALUES_GET_NO_DD_NAME'
       exporting
            selectfield      = w_fname
       importing
            ind              = w_tabix
            select_value     = w_fldvl
       tables
            fields           = it_field
            full_table       = it_value.
*
  read table it_value index w_tabix.
  p_short = w_fldvl.
  p_stext = it_value-stext.
  w_orgeh = it_value-objid.
*
  clear dynpfields. refresh dynpfields.
  dynpfields-fieldname = 'P_STEXT'.
  dynpfields-fieldvalue = P_STEXT.
  append dynpfields. clear dynpfields.
*
  call function 'DYNP_VALUES_UPDATE'
       exporting
            dyname                     = sy-cprog
            dynumb                     = sy-dynnr
       tables
            dynpfields                 = dynpfields
       EXCEPTIONS
            INVALID_ABAPWORKAREA       = 1
            INVALID_DYNPROFIELD        = 2
            INVALID_DYNPRONAME         = 3
            INVALID_DYNPRONUMMER       = 4
            INVALID_REQUEST            = 5
            NO_FIELDDESCRIPTION        = 6
            UNDEFIND_ERROR             = 7
            OTHERS                     = 8.
ENDFORM.                    " pop_up_org_unit
*&---------------------------------------------------------------------*
*&      Form  get_sub_division
*&---------------------------------------------------------------------*
FORM get_sub_division.
  clear: it_units, it_persn, it_ogper.
  refresh: it_units, it_persn, it_ogper.
*
  if w_orgeh = '0000000000'.
    clear hrp1000.
    select single objid into w_orgeh
      from hrp1000 where plvar = w_plvar
                     and otype = w_otype
                     and istat = '1'
                     and endda = '99991231'
                     and langu = sy-langu
                     and short = p_short
                     and stext = p_stext.
  endif.
*
  call function 'RH_DIR_ORG_STRUC_GET'
       exporting
            act_orgunit           = w_orgeh
            act_plvar             = w_plvar
            act_date              = p_rptda
            srot_flag             = 'X'
            add_flag_pdata        = 'X'
       tables
            org_units             = it_units
            person_tab            = it_persn
            org_pers_rel          = it_ogper
       exceptions
            no_active_plvar       = 1
            others                = 2.
*
  clear r_orgeh. refresh r_orgeh.
  loop at it_units.
    r_orgeh-sign = 'I'. r_orgeh-option = 'EQ'.
    r_orgeh-low = it_units-orgid.
    append r_orgeh. clear r_orgeh.
  endloop.
*
  clear it_hirar. refresh it_hirar.
* loop at it_units where parentid = w_orgeh.
*   it_hirar-orgeh = it_units-orgid.
*   append it_hirar.
* endloop.
  loop at it_units.
    it_hirar-orgeh = it_units-orgid.
    it_hirar-parnt = it_units-parentid.
    append it_hirar. clear it_hirar.
  endloop.
*
  loop at it_hirar.
    clear zthr_1001.
    select single ztype into zthr_1001-ztype
      from zthr_1001 where objid = it_hirar-orgeh.
    if sy-subrc = 0.
      case zthr_1001-ztype.
        when 'H'. it_hirar-grad1 = 4.
        when 'D'. it_hirar-grad1 = 3.
        when 'S'. it_hirar-grad1 = 2.
        when 'T'. it_hirar-grad1 = 1.
      endcase.
    endif.

    clear zthr_1001.
    select single ztype into zthr_1001-ztype
      from zthr_1001 where objid = it_hirar-parnt.
    if sy-subrc = 0.
      case zthr_1001-ztype.
        when 'H'. it_hirar-grad2 = 4.
        when 'D'. it_hirar-grad2 = 3.
        when 'S'. it_hirar-grad2 = 2.
        when 'T'. it_hirar-grad2 = 1.
      endcase.
    endif.

    if it_hirar-parnt = '00000000' or it_hirar-parnt = space.
      it_hirar-parnt = it_hirar-orgeh.
    endif.

    modify it_hirar. clear it_hirar.
  endloop.
ENDFORM.                    " get_sub_division
*&---------------------------------------------------------------------*
*&      Form  get_pernr_by_select_option
*&---------------------------------------------------------------------*
FORM get_pernr_by_select_option.
  clear it_p0001. refresh it_p0001.
*
*  clear pa0001.
*  select pernr persg orgeh
*    into (pa0001-pernr, pa0001-persg, pa0001-orgeh)
*    from pa0001 where endda = '99991231'
*                  and orgeh in r_orgeh.
*    if pa0001-persg <> '2'.
*      it_p0001-orgeh = pa0001-orgeh.
*      it_p0001-pernr = pa0001-pernr.
*      append it_p0001. clear it_p0001.
*    endif.
*  endselect.
*
* sort it_p0001 by orgeh pernr.
  loop at it_ogper.
    clear pa0001.
    select single werks persg persk btrtl
      into (pa0001-werks, pa0001-persg, pa0001-persk, pa0001-btrtl)
      from pa0001 where pernr = it_ogper-objid
                    and endda = '99991231'.
    clear t001p.
    select single mofid mosid into (t001p-mofid, t001p-mosid)
      from t001p where werks = pa0001-werks
                   and btrtl = pa0001-btrtl.
    clear t503.
    select single zeity into t503-zeity
      from t503 where persg = pa0001-persg
                  and persk = pa0001-persk.
    clear pa0007.
    select single schkz into pa0007-schkz
      from pa0007 where pernr = it_ogper-objid
                    and endda = '99991231'.
    clear dy_field. refresh dy_field.
    concatenate 'TPR' p_rptda+6(2) into dy_field.
    append dy_field.
    clear t552a.
    select single (dy_field) into w_tprxx
      from t552a where zeity = t503-zeity
                   and mofid = t001p-mofid
                   and mosid = t001p-mosid
                   and schkz = pa0007-schkz
                   and kjahr = p_rptda+(4)
                   and monat = p_rptda+4(2).
    clear zthr_shift.
    select single erdat into zthr_shift-erdat
      from zthr_shift where ztype = p_schkz
                        and tprog = w_tprxx.
    if sy-subrc = 0.
      it_p0001-orgeh = it_ogper-orgid.
      it_p0001-pernr = it_ogper-objid.
      append it_p0001. clear it_p0001.
    endif.
  endloop.
*
*... modified by hhj (2004.02.04) - start
*  loop at it_p0001.
*    clear it_avail. refresh it_avail.
*
*    call function 'HR_READ_TIMEDATA_PSP'
*         exporting
*              begin_date                  = p_rptda
*              end_date                    = p_rptda
*              person_id                   = it_p0001-pernr
*              person_typ                  = 'P'
*         tables
*              pers_avail                  = it_avail
*         exceptions
*              no_capacity_available       = 1
*              no_integration_activ        = 2
*              timeinfo_error              = 3
*              others                      = 4.
*
*    if sy-subrc = 0.
*      read table it_avail index 1.
*      it_p0001-begzt = it_avail-begzt.
*      modify it_p0001. clear it_p0001.
*    endif.
*  endloop.
*... modified end
*
*  case p_schkz.
*    when '1'.                                 " day shift
*      loop at it_p0001.
*        if it_p0001-begzt >= '06:30:00' and
*           it_p0001-begzt <= '12:00:00'.
*        else.
*          delete it_p0001.
*        endif.
*      endloop.
*    when '2'.                                 " night shift
*      loop at it_p0001.
*        if it_p0001-begzt >= '06:30:00' and
*           it_p0001-begzt <= '12:00:00'.
*          delete it_p0001.
*        else.
*        endif.
*      endloop.
*  endcase.
ENDFORM.                    " get_pernr_by_select_option
*&---------------------------------------------------------------------*
*&      Form  sum_of_selected_date
*&---------------------------------------------------------------------*
FORM sum_of_selected_date.
  sort it_p0001 by orgeh pernr.
*
  loop at it_p0001.
    clear it_hirar.
    read table it_hirar with key orgeh = it_p0001-orgeh.
    if it_hirar-grad1 = space.
      it_colec-orgeh = it_hirar-parnt.
    else.
      it_colec-orgeh = it_p0001-orgeh.
    endif.
    it_colec-total = 1.

*   clear catsdb.
*   select single skostl awart into (catsdb-skostl, catsdb-awart)
*     from catsdb where pernr = it_p0001-pernr
*                   and workdate = p_rptda.

    clear pa2001.
    select single awart into pa2001-awart
      from pa2001 where pernr = it_p0001-pernr
                    and endda >= p_rptda
                    and begda <= p_rptda.

    case pa2001-awart.
      when '1023' or '1038'.
        it_colec-fild1 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1024' or '1037'.
        it_colec-fild2 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1025'.
        it_colec-fild3 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1030' or '1029' or '1031'.
        it_colec-fild4 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1032'.
        it_colec-fild5 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1020' or '1034' or '1035'.
        it_colec-fild6 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1027'.
        it_colec-fild7 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1042' or '1047'.
        it_colec-fild8 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1041' or '1043' or '1046'.
        it_colec-fild9 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when '1048'.
        it_colec-fild0 = 1.
        it_colec-atten = 0.
        it_colec-absen = 1.
      when others.
        clear pa2002.
        select single pdsnr into asshr-pdsnr
          from asshr as a join pa2002 as b
            on a~pernr = b~pernr and
               a~subty = b~subty and
               a~objps = b~objps and
               a~sprps = b~sprps and
               a~endda = b~endda and
               a~begda = b~begda and
               a~seqnr = b~seqnr
         where b~pernr = it_p0001-pernr
           and b~endda >= p_rptda
           and b~begda <= p_rptda
           and b~refex = 'X'
           and a~infty = '2002'.
        if sy-subrc = 0.
          it_colec-filda = 1.
          it_colec-atten = 0.
          it_colec-absen = 1.
        else.
          it_colec-filda = 0.
          it_colec-atten = 1.
          it_colec-absen = 0.
        endif.
    endcase.

    collect it_colec. clear it_colec.
  endloop.
*
  loop at it_colec.
    clear it_units.
    read table it_units with key orgid = it_colec-orgeh.
    it_colec-short = it_units-short.
    it_colec-stext = it_units-stext.
    modify it_colec. clear it_colec.
  endloop.
  sort it_colec by orgeh.
*
  loop at it_hirar.
    w_count = 0.
    if it_hirar-grad1 = 0 and it_hirar-orgeh <> it_hirar-parnt.
      delete it_hirar. clear it_hirar. continue.
    endif.

    w_count = it_hirar-grad1 + 1.
    if it_hirar-grad2 <> w_count.
      it_hirar-parnt = '99999999'.
      modify it_hirar. clear it_hirar.
    endif.
  endloop.
  sort it_hirar by grad1 parnt.
ENDFORM.                    " sum_of_selected_date
*&---------------------------------------------------------------------*
*&      Form  check_maintenance_view
*&---------------------------------------------------------------------*
FORM check_maintenance_view.
  clear zthr_1001.
  select single objid into zthr_1001-objid
    from zthr_1001 where zmark = space.
*
  if sy-subrc = 0.
    perform pop_up_org_unit.
  else.
    call function 'POPUP_TO_CONFIRM_STEP'
         exporting
              defaultoption = 'Y'
              textline1     = 'No values selected'
              textline2     = 'Maintain entries?'
              titel         = 'Prompt entry maintenance'
         importing
              answer        = w_answr
         exceptions
              others        = 1.

    if w_answr = 'J'.
      call transaction 'ZAHR0004'.
    else.
      set screen 1000.
    endif.
  endif.
ENDFORM.                    " check_maintenance_view
*&---------------------------------------------------------------------*
*&      Form  heading
*&---------------------------------------------------------------------*
FORM heading.
  w_width = 199.
*
  write p_rptda to w_tdate using edit mask '__/__/____'.
  concatenate '***' 'Daily Attendance Report(' w_tdate ')' '***'
         into w_title separated by space.
  write  at /(w_width) w_title centered no-gap.
  skip 1.
* write  at /(w_width) '' centered no-gap.
  format color col_heading intensified off.
  uline at (w_width).
  write: / sy-vline no-gap.
  write: (12) ' ' no-gap, sy-vline no-gap.
  write: (30) ' ' no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) 'Personal' centered no-gap, sy-vline no-gap.
  write: (10) 'Unpaid' centered no-gap, sy-vline no-gap.
  write: (10) 'Restrict' centered no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) 'Sick/STD' centered no-gap, sy-vline no-gap.
  write: (10) 'Injury/' centered no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
  write: (10) ' ' no-gap, sy-vline no-gap.
*
  write: / sy-vline no-gap.
  write: (12) 'Org. Unit' centered no-gap, sy-vline no-gap.
  write: (30) 'Org. Unit Name' centered no-gap, sy-vline no-gap.
  write: (10) 'Total' centered no-gap, sy-vline no-gap.
  write: (10) 'Attendance' no-gap, sy-vline no-gap.
  write: (10) 'Absence' centered no-gap, sy-vline no-gap.
  write: (10) 'Vacation' centered no-gap, sy-vline no-gap.
  write: (10) 'Time off' centered no-gap, sy-vline no-gap.
  write: (10) 'Leave' centered no-gap, sy-vline no-gap.
  write: (10) 'Leave' centered no-gap, sy-vline no-gap.
  write: (10) 'FMLA' centered no-gap, sy-vline no-gap.
  write: (10) '/LTD' centered no-gap, sy-vline no-gap.
  write: (10) 'Works Comp' no-gap, sy-vline no-gap.
  write: (10) 'Travel' centered no-gap, sy-vline no-gap.
  write: (10) 'Training' centered no-gap, sy-vline no-gap.
  write: (10) 'Meeting' centered no-gap, sy-vline no-gap.
  write: (10) 'Dispatch' centered no-gap, sy-vline no-gap.
*
  uline at (w_width).
ENDFORM.                    " heading
*&---------------------------------------------------------------------*
*&      Form  write_body
*&---------------------------------------------------------------------*
FORM write_body.
  sort it_colec by orgeh.
  sort it_units by parentid.
*
  loop at it_hirar.
    clear tmp_cole. refresh tmp_cole.
    loop at it_units where parentid = it_hirar-orgeh.
      format color col_normal intensified off.
      clear it_colec.
      read table it_colec with key orgeh = it_units-orgid.
      if sy-subrc = 0.
        move-corresponding it_colec to tmp_cole.
        append tmp_cole. clear tmp_cole.
        perform write_index_key.
      endif.
    endloop.
    format color col_normal intensified.
    clear it_colec.
    read table it_colec with key orgeh = it_hirar-orgeh.
    if sy-subrc = 0.
      move-corresponding it_colec to tmp_cole.
      append tmp_cole. clear tmp_cole.
      perform write_index_key.
    endif.

    loop at tmp_cole.
      at last.
        sum.
        format color col_group intensified off.
        uline at (w_width).
        write: / sy-vline no-gap.
        write: (43) 'Team total' centered no-gap, sy-vline no-gap.
        write: (10) tmp_cole-total no-gap, sy-vline no-gap.
        write: (10) tmp_cole-atten no-gap, sy-vline no-gap.
        write: (10) tmp_cole-absen no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild1 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild2 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild3 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild4 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild5 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild6 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild7 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild8 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild9 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-fild0 no-gap, sy-vline no-gap.
        write: (10) tmp_cole-filda no-gap, sy-vline no-gap.
        uline at (w_width).
      endat.
    endloop.
  endloop.
*
  clear it_colec.
  read table it_colec with key orgeh = w_orgeh.
  if sy-subrc = 0.
    format color col_normal intensified off.
    perform write_index_key.
  endif.
  uline at (w_width).
*
  loop at it_colec.
    at last.
      sum.
      format color col_group intensified.
      write: / sy-vline no-gap.
      write: (43) 'Team total' centered no-gap, sy-vline no-gap.
      write: (10) it_colec-total no-gap, sy-vline no-gap.
      write: (10) it_colec-atten no-gap, sy-vline no-gap.
      write: (10) it_colec-absen no-gap, sy-vline no-gap.
      write: (10) it_colec-fild1 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild2 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild3 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild4 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild5 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild6 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild7 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild8 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild9 no-gap, sy-vline no-gap.
      write: (10) it_colec-fild0 no-gap, sy-vline no-gap.
      write: (10) it_colec-filda no-gap, sy-vline no-gap.
    endat.
  endloop.
  uline at (w_width).
ENDFORM.                    " write_body
*&---------------------------------------------------------------------*
*&      Form  write_index_key
*&---------------------------------------------------------------------*
FORM write_index_key.
  write: / sy-vline no-gap.
  write: (12) it_colec-short no-gap, sy-vline no-gap.
  write: (30) it_colec-stext no-gap, sy-vline no-gap.
  write: (10) it_colec-total no-gap, sy-vline no-gap.
  write: (10) it_colec-atten no-gap, sy-vline no-gap.
  write: (10) it_colec-absen no-gap, sy-vline no-gap.
  write: (10) it_colec-fild1 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild2 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild3 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild4 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild5 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild6 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild7 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild8 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild9 no-gap, sy-vline no-gap.
  write: (10) it_colec-fild0 no-gap, sy-vline no-gap.
  write: (10) it_colec-filda no-gap, sy-vline no-gap.
ENDFORM.                    " write_index_key
*&---------------------------------------------------------------------*
*&      Form  write_body_new_form
*&---------------------------------------------------------------------*
FORM write_body_new_form.
  loop at it_hirar.
    if sy-tabix = 1.
      w_objid = it_hirar-parnt.
    endif.

    if it_hirar-orgeh = w_orgeh.
      continue.
    endif.

*   at end of parnt.
    if w_objid <> it_hirar-parnt.
      clear it_colec.
      read table it_colec with key orgeh = w_objid.
      if sy-subrc = 0.
        format color 2 intensified off.
        perform write_index_key.
        move-corresponding it_colec to tmp_cole.
        append tmp_cole. clear tmp_cole.
      endif.
      if w_objid <> '99999999'.
        loop at tmp_cole.
          at last. sum.
            perform write_tmp_cole.
          endat.
        endloop.
      endif.
      clear tmp_cole. refresh tmp_cole.
      w_objid = it_hirar-parnt.
    endif.
*   endat.

*   at new parnt.
*     clear tmp_cole. clear tmp_cole.
*   endat.

    if it_hirar-grad1 > 1.
      continue.
    endif.

    case it_hirar-grad1.
      when 1. format color 3 intensified off.
      when 2. format color 7 intensified off.
      when 3. format color 7 intensified.
      when 4. format color 6 intensified off.
      when others. format color 2 intensified off.
    endcase.

    clear it_colec.
    read table it_colec with key orgeh = it_hirar-orgeh.
    if sy-subrc = 0.
      perform write_index_key.
      move-corresponding it_colec to tmp_cole.
      append tmp_cole. clear tmp_cole.
    endif.
  endloop.
*
  read table it_hirar with key orgeh = w_orgeh.
  if it_hirar-grad1 > 1.
    clear it_colec.
    read table it_colec with key orgeh = w_orgeh.
    format color 2 intensified off.
*   uline at (w_width).
    perform write_index_key.
  endif.
*
  case it_hirar-grad1.
    when 1. format color 3 intensified off.
    when 2. format color 7 intensified off.
    when 3. format color 7 intensified.
    when 4. format color 6 intensified off.
    when others. format color 2 intensified off.
  endcase.
*
  clear tmp_cole. refresh tmp_cole.
  tmp_cole[] = it_colec[].
  w_objid = w_orgeh.
*
  loop at tmp_cole.
    at last. sum.
      perform write_tmp_cole.
    endat.
  endloop.
ENDFORM.                    " write_body_new_form
*&---------------------------------------------------------------------*
*&      Form  write_tmp_cole
*&---------------------------------------------------------------------*
FORM write_tmp_cole.
  uline at (w_width).
  write: / sy-vline no-gap.
*
  clear zthr_1001.
  select single ztype into zthr_1001-ztype
    from zthr_1001 where objid = w_objid.
  case zthr_1001-ztype.
    when 'H'. format color 6 intensified off.
      write: (43) 'HMMA Total' centered no-gap, sy-vline no-gap.
    when 'D'. format color 7 intensified.
      write: (43) 'Division Total' centered no-gap, sy-vline no-gap.
    when 'S'. format color 7 intensified off.
      write: (43) 'Sub-Division Total' centered no-gap, sy-vline no-gap.
    when 'T'. format color 3 intensified off.
      write: (43) 'Team Total' centered no-gap, sy-vline no-gap.
    when others. format color 2 intensified off.
      write: (43) 'Total' centered no-gap, sy-vline no-gap.
  endcase.
*
  write: (10) tmp_cole-total no-gap, sy-vline no-gap.
  write: (10) tmp_cole-atten no-gap, sy-vline no-gap.
  write: (10) tmp_cole-absen no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild1 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild2 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild3 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild4 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild5 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild6 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild7 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild8 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild9 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-fild0 no-gap, sy-vline no-gap.
  write: (10) tmp_cole-filda no-gap, sy-vline no-gap.
  uline at (w_width).
ENDFORM.                    " write_tmp_cole
