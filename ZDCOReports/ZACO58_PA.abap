************************************************************************
* Program Name      :
* Author            : Andy Choi
* Creation Date     : 08/07/2005
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
report zaco58_pa message-id zmco.

include: <icon>.
*ype-pools: vrm,rkea1.     "//Value Request Manager: Types & Constants
tables: ce1h201, t001.
tables: dd03l.

data: begin of it_value_field occurs 0,
        field       like   dd03l-fieldname,
        value_field like   dd03l-fieldname,
        fieldname   like   dd03l-fieldname,
        ddtext      like    dd04t-ddtext,
      end   of it_value_field.

data: begin of i_flds occurs 0,
        field   like   dd03l-fieldname,
      end   of i_flds.

*----- If value field count is 50 over,
*----- increase structure ZSCO_COGS_CO's fields
data: begin of itab occurs 0,
       vrgar    type rke_vrgar,
       matnr	type matnr ,
       kunnr	type kunnr ,
       kmland	type land1_gp,
       vkorg	type vkorg ,
       vtweg	type vtweg ,
       fd001	type zfd001,                                   "revenue1
       fd002	type zfd001,                                   "revenue2
       fd003	type zfd001,
       fd004	type zfd001,
       fd005	type zfd001,
       fd006	type zfd001,
       fd007	type zfd001,
       fd008	type zfd001,
       fd009	type zfd001,
       fd010	type zfd001,
       fd011	type zfd001,
       fd012	type zfd001,
       fd013	type zfd001,
       fd014	type zfd001,
       fd015	type zfd001,
      end of itab.

data: it_tab like itab occurs 0 with header line.

data: begin of it_pa occurs 0,
       fieldname type fieldname,
       matnr	type matnr ,
       kunnr	type kunnr ,
       kmland	type land1_gp,
       vkorg	type vkorg ,
       vtweg	type vtweg ,
       coamt    type zpost ,
       post	type zpost ,
      end of it_pa.

data: it_pa_tar like it_pa occurs 0 with header line.

* posting PA
data: it_inputdata like bapi_copa_data  occurs 0 with header line,
      it_fieldlist like bapi_copa_field occurs 0 with header line,
      it_return    like bapiret2        occurs 0 with header line.

*----- Global variables & Structures
data: w_kokrs     like   tka01-kokrs,
      w_perbl     like   ce3h201-perbl,    "Period
      w_cnt       type   i,                "Field count
      w_fisum_new like   zsco_cogs-fiamt,  "For calclulation
      w_fisum_old like   zsco_cogs-fiamt,  "For calclulation
      w_cosum     like   zsco_cogs-fiamt,  "For calclulation
      w_diff      like   zsco_cogs-fiamt,  "For calclulation
      w_op_concern like  bapi0017-op_concern,
      w_top_line  type   i,                "ALV top line
      w_record    like   bapi_copa_data-record_id,   "Record ID
      w_select    type   i,                "Selected items
      w_success   type   i,                "Success items
      w_fail      type   i,                "Failed items
      w_msg(50).                           "Message
data: wa_error,
      wa_message(80).
*----- Constants
constants: c_bukrs   like   t001-bukrs   value   'H201',
           c_werks   like   t001w-werks  value   'P001',
           c_error   type   i            value   1,
           c_bdcerr  type   i            value   2,
           c_ready   type   i            value   3,
           c_finish  type   i            value   4.

*--- ALV
type-pools: slis.
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line1 type slis_listheader.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv,
      g_repid     like sy-repid.

*---- ALV


*&---------------------------------------------------------------------*
*----- Selection screens
selection-screen begin of block bl1 with frame title text-t01.
parameters: p_bukrs  like t001-bukrs  default c_bukrs obligatory,
            p_month  like s001-spmon default sy-datum(6) obligatory.

select-options:
            s_vfld   for dd03l-fieldname no intervals no-display,
            s_vrgar  for ce1h201-vrgar   no intervals.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' .
selection-screen end   of block bl1.

selection-screen begin of block bl2 with frame title text-t02.
select-options:
            s_artnr  for ce1h201-artnr,
            s_kndnr  for ce1h201-kndnr,
            s_vkorg  for ce1h201-vkorg,
            s_vtweg  for ce1h201-vtweg.
selection-screen end   of block bl2.
*&---------------------------------------------------------------------*

*----- Default Value
initialization.
  perform set_initial_value.

*----- Check Input value
at selection-screen.
  perform check_rtn.

*&---------------------------------------------------------------------*
start-of-selection.
  perform read_value_field.
  perform read_pa_data.
  perform derive_chars.
  perform calculate_it_pa.


  perform display_it_pa.

*&---------------------------------------------------------------------*
*&      Form  set_initial_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_initial_value.
*----- Set Controlling area
  call function 'K_KOKRS_SET'
       importing
            e_kokrs   = w_kokrs
       exceptions
            not_found = 1
            others    = 2.
  if sy-subrc <> 0.
    if sy-msgty = 'E' or sy-msgty = 'A' or sy-msgty = 'X'.
      message id sy-msgid type 'S' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      leave program.
    else.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

* value fields
  refresh s_vfld.
  s_vfld-option = 'EQ'. s_vfld-sign = 'I'.
  s_vfld-low = 'ERLOS'. append s_vfld.
  s_vfld-low = 'VV021'. append s_vfld.

  s_vfld-low = 'VV370'. append s_vfld.
  s_vfld-low = 'VV372'. append s_vfld.
  s_vfld-low = 'VV600'. append s_vfld.
  s_vfld-low = 'VV480'. append s_vfld.
  s_vfld-low = 'VV500'. append s_vfld.
  s_vfld-low = 'VV620'. append s_vfld.
  s_vfld-low = 'VV610'. append s_vfld.
  s_vfld-low = 'VV495'. append s_vfld.

  refresh s_vrgar.
  s_vrgar-option = 'EQ'. s_vrgar-sign = 'I'.
  s_vrgar-low = 'F'.  append s_vrgar.
  s_vrgar-low = 'Y'.  append s_vrgar.
  s_vrgar-low = 'B'.  append s_vrgar.

endform.                    " set_initial_value
*&---------------------------------------------------------------------*
*&      Form  CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_rtn.
  perform check_bukrs.
  perform check_month.
endform.                    " CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_bukrs.
  select single * from t001 where bukrs = p_bukrs.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.
endform.                    " CHECK_BUKRS

*&---------------------------------------------------------------------*
*&      Form  CHECK_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_month.
  data: w_budat_f   like   sy-datum,
        w_budat_t   like   sy-datum.


  concatenate p_month '01' into w_budat_f.

  call function 'RP_LAST_DAY_OF_MONTHS'
       exporting
            day_in            = w_budat_f
       importing
            last_day_of_month = w_budat_t
       exceptions
            day_in_no_date    = 1
            others            = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  concatenate p_month(4) '0' p_month+4(2)
         into w_perbl.
endform.                    " CHECK_MONTH
*&---------------------------------------------------------------------*
*&      Form  read_value_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_value_field.
  data: lw_index(3) type n.

  ranges: lr_field for dd03l-fieldname.

* SET THE FIELD NAME NEED TO READ
  i_flds-field = 'VRGAR'.   append i_flds.
  i_flds-field = 'ARTNR'.   append i_flds.
  i_flds-field = 'KNDNR'.   append i_flds.
  i_flds-field = 'KMLAND'.  append i_flds.
  i_flds-field = 'VKORG'.   append i_flds.
  i_flds-field = 'VTWEG'.   append i_flds.


* GET THE VALUE FIELDS
  refresh lr_field.
  move: 'I'  to lr_field-sign,
        'CP' to lr_field-option.

  loop at s_vfld.
    concatenate s_vfld-low '*' into lr_field-low.
    collect lr_field.
  endloop.

* READ THE VALUE FIELDS
  select fieldname ddtext
    into corresponding fields of table it_value_field
    from dd03l as a inner join dd04t as b
      on a~rollname = b~rollname
     and b~ddlanguage = sy-langu
     and b~as4local   = 'A'
*     AND b~as4vers    = 0
   where a~tabname   =  'CE3H201'
     and a~fieldname in lr_field
     and a~as4local  =  'A'.
*     AND a~as4vers   =  0.

  sort it_value_field by fieldname.

  data: lw_field like dd03l-fieldname.

* APPEND THE VALUE FIELDS
  loop at it_value_field.
*   move: sy-tabix to lw_index.
    concatenate it_value_field-fieldname(5) '*' into lw_field.
    loop at s_vfld  where low cp lw_field.
      move: sy-tabix to lw_index.
      exit.
    endloop.

    move: it_value_field-fieldname to i_flds-field.
    append i_flds.

    concatenate 'FD' lw_index into it_value_field-field.
    modify it_value_field.
  endloop.

* value fields to be checked.
  loop at s_vfld.
    concatenate s_vfld-low '*' into lw_field.

    loop at it_value_field where fieldname cp lw_field.
      it_value_field-value_field = s_vfld-low.
      modify it_value_field.
    endloop.
  endloop.


endform.                    " read_value_field
*&---------------------------------------------------------------------*
*&      Form  derive_chars
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form derive_chars.
  data: lw_field(100),
        lw_index(3) type n.
  field-symbols: <field>.
  data: ls_item   like ce0h201,
        ls_item_e like ce0h201,
        ls_idx    like sy-index,
        ls_rev    type rke2_erlos.

* derive the co characteristics
  loop at itab.
* check if revenue exist
*    ls_rev = itab-fd001 + itab-fd002.
*    if ls_rev <> 0.
*      continue.
*    endif.

    ls_idx = sy-index.

*  set the value of input structure
*   ls_item-hzdat       = sy-datum.
*   ls_item-budat       = w_budat_t.
    ls_item-bukrs       = c_bukrs.
    ls_item-kokrs       = w_kokrs.
    ls_item-perde       = p_month+4(2).
    ls_item-gjahr       = p_month(4).
    ls_item-artnr       = itab-matnr.
    ls_item-kndnr       = itab-kunnr.
    ls_item-kmland      = itab-kmland.
*   LS_ITEM-SPART       = '10'.

    call function 'KEDR_COPA_DERIVE'
      exporting
        i_erkrs                        = w_kokrs
        i_item                         = ls_item
*       I_TAB_EXCEPTIONS               =
        i_derivation_date              = sy-datum
*       I_DERIVE_ANYWAY                =
*       I_TRACE_MODE                   = ' '
        i_tabname                      = 'CE0H201'
*       I_GLOBAL_FIELDS                =
        i_mass_processing              = 'X'
*       I_CHECK_CHIER_ONLY             =
      importing
        e_item                         = ls_item_e
      exceptions
        derivation_failed              = 1
        check_chier_failed             = 2
        check_values_failed            = 3
        others                         = 4.

    if sy-subrc eq 0.
*    Read the characteritics
      itab-matnr     = ls_item_e-artnr.
      itab-kunnr     = ls_item_e-kndnr.
      itab-kmland    = ls_item_e-kmland.
    endif.

    it_tab = itab.
    it_tab-fd001 = it_tab-fd001 + it_tab-fd002.
    clear it_tab-fd002.

    collect it_tab.
  endloop.

endform.                    " derive_chars
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_it_pa
*&---------------------------------------------------------------------*
form calculate_it_pa.
* summarize
  data: lt_co like itab occurs 0 with header line,
        ls_rev     type rke2_erlos.

  loop at it_tab.
    if  it_tab-fd001 = 0 and it_tab-fd002 = 0 and it_tab-fd003 = 0
    and it_tab-fd004 = 0 and it_tab-fd005 = 0 and it_tab-fd006 = 0
    and it_tab-fd007 = 0 and it_tab-fd008 = 0 and it_tab-fd009 = 0
    and it_tab-fd010 = 0 and it_tab-fd011 = 0 and it_tab-fd012 = 0
    and it_tab-fd013 = 0 and it_tab-fd014 = 0 and it_tab-fd015 = 0.
      continue.
    endif.

    collect it_tab into lt_co.
  endloop.

  refresh it_tab.
  it_tab[] = lt_co[].



  field-symbols: <vfield>.
  data: l_text(30).
*
* SEPARATE VALUE FILEDS TO DIFFRENT RECORDS
  clear: it_pa[], it_pa.

  loop at itab.
    it_pa-kunnr  = itab-kunnr.
    it_pa-kmland = itab-kmland.
    it_pa-matnr  = itab-matnr.
    loop at it_value_field.
      it_pa-fieldname = it_value_field-value_field.
*      it_pa-ddtext    = it_value_field-ddtext.
      concatenate 'itab-' it_value_field-field into l_text.
      assign (l_text) to <vfield>.
      it_pa-coamt = <vfield>.
      append it_pa.
    endloop.
  endloop.

  data: it_pa_tmp like it_pa occurs 0 with header line.

* Collect for the same value field value
  clear: it_pa_tmp[], it_pa_tmp.
  loop at it_pa.
    collect it_pa into it_pa_tmp.
  endloop.

  refresh it_pa.
  clear:  it_pa.
  it_pa[] = it_pa_tmp[].

endform.                    " CALCULATE_it_pa
*&---------------------------------------------------------------------*
*&      Form  display_it_pa
*&---------------------------------------------------------------------*
form display_it_pa.

  sort it_tab by matnr vrgar.

  perform field_setting tables gt_fieldcat using :
 'MATNR'      'Product'     '18' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'VRGAR'      'RT'          '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'VKORG'      'Sales Org'   '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'VTWEG'      'DCh'         '02' 'X' 'R'  ' '  ' '  ' '  ' '  'X',
 'KMLAND'     'Cty'         '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'KUNNR'      'Customer'    '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'FD001'      'Sales'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
*'FD002'      'Scrap Sales' '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD003'      'VV370'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD004'      'VV372'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD005'      'VV600'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD006'      'VV480'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD007'      'VV500'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD008'      'VV620'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD009'      'VV610'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD010'      'VV495'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD011'      'VVxxx'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD012'      'VVxxx'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD013'      'VVxxx'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD014'      'VVxxx'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'FD015'      'VVxxx'       '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X'.

  g_repid = sy-repid.
    call function 'REUSE_ALV_GRID_DISPLAY'
         exporting
              i_callback_program = g_repid
              it_fieldcat        = gt_fieldcat
              i_save             = 'A'
         tables
              t_outtab           = it_tab
         exceptions
              program_error      = 1
              others             = 2.

endform.                    " display_it_pa
*&---------------------------------------------------------------------*
*&      Form  POSTING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form posting_rtn.
  "/Indexes of Selected Rows
  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "/Numeric IDs of Selected Rows
  data: l_line type i.

  clear: w_select, w_success, w_fail.

  read table lt_rows index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m12.
  endif.

  perform call_bapi_rtn.

  clear: w_msg.
  write: text-m13  to w_msg(11),
         w_select  to w_msg+11(4),
         text-m14  to w_msg+16(10),
         w_success to w_msg+26(4),
         text-m15  to w_msg+31(09),
         w_fail    to w_msg+41(4).
  message s000(zz) with w_msg.
endform.                    " POSTING_RTN

*&---------------------------------------------------------------------*
*&      Form  SUCCESSFULLY_UPDATED_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form successfully_updated_rtn.
  clear: it_return, it_return[].

  call function 'BAPI_TRANSACTION_COMMIT'
       exporting
            wait   = 'X'
       importing
            return = it_return.
  loop at it_return where type ca 'AEX'.
    write:/ 'ERROR BDC'.
    exit.
  endloop.
  if sy-subrc ne 0.
  endif.
endform.                    " SUCCESSFULLY_UPDATED_RTN
*&---------------------------------------------------------------------*
*&      Form  call_bapi_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_bapi_rtn.
  clear: it_return, it_return[].

  move: p_bukrs to w_op_concern.

  call function 'BAPI_COPAACTUALS_POSTCOSTDATA'
       exporting
            operatingconcern = w_op_concern
            testrun          = ' '
       tables
            inputdata        = it_inputdata
            fieldlist        = it_fieldlist
            return           = it_return.
  loop at it_return where type ca 'AEX'.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    write:/ 'BDC ERROR'.
    exit.
  endloop.
  if sy-subrc ne 0.
    perform successfully_updated_rtn.
  endif.

endform.                    " call_bapi_rtn

*&---------------------------------------------------------------------*
*&      Form  set_field_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_list.
  clear: it_fieldlist, it_fieldlist[],
         it_inputdata, it_inputdata[].

  move: 'VRGAR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'BUDAT'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'PERDE'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'GJAHR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'BUKRS'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'KOKRS'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'FRWAE'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'KNDNR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'ARTNR'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'PRODH'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'PAPH2'  to it_fieldlist-fieldname.
  append it_fieldlist.

  move: 'RBELN'  to it_fieldlist-fieldname.
  append it_fieldlist.
  move: 'BELNR'  to it_fieldlist-fieldname.
  append it_fieldlist.

*----- Calculate separated diffrence amount
  loop at it_value_field.
    read table s_vfld with key low = it_value_field-value_field
      .
    if sy-subrc ne 0.
      message e000(zz) with text-m01.
    endif.
    move: s_vfld-low  to it_fieldlist-fieldname.
    collect it_fieldlist.
  endloop.
endform.                    " set_field_list
*&---------------------------------------------------------------------*
*&      Form  read_vf_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_VALUE_FIELD_VALE_FIELD  text
*      -->P_it_pa_DDTEXT  text
*----------------------------------------------------------------------*
form read_vf_desc using    p_vfield
                           p_ddtext.

  read table it_value_field with key value_field = p_vfield.
  if sy-subrc eq 0.
    p_ddtext = it_value_field-ddtext.
  else.
    clear: p_ddtext.
  endif.


endform.                    " read_vf_desc
*&---------------------------------------------------------------------*
*&      Form  read_pa_data
*&---------------------------------------------------------------------*
form read_pa_data.
* Actual = CE1xxxx
* Segment Table
  select (i_flds)
    into  table itab
    from ce4h201 as a inner join ce3h201 as b
                         on a~paobjnr = b~paobjnr
                        and b~perbl   =  w_perbl
*                 left outer join kna1 as f
*                         on a~kndnr = f~kunnr
   where a~aktbo    =  'X'
     and a~bukrs    =  p_bukrs
     and a~kokrs    =  w_kokrs
     and a~artnr    in s_artnr
     and a~kndnr    in s_kndnr
     and a~vkorg    in s_vkorg
     and a~vtweg    in s_vtweg
     and b~vrgar    in s_vrgar
     and b~paledger =  '01'
     and b~plikz    =  '0'.
*     AND a~werks    =  p_werks
*     AND b~versi    =  '0'.

endform.                    " read_pa_data
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat using
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  append ls_fieldcat to gt_fieldcat.

endform.                    " fill_field_category
*ALV
