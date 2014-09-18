************************************************************************
* Program Name      : ZCOABP_DEL_BOM
* Description       : Delete ABP BOM for 1 level
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
report  ZCOABP_DEL_BOM.
tables: mast, mara, stpo, t001w, t416t.

data: begin of it_9000 occurs 0,
       MATNR	type MATNR,
       MAKTX	type MAKTX,
       WERKS	type WERKS_D,
       STLTY	type STLTY,
       STLAN	type STLAN,
       STLAL	type STLAL,
       MSG	type ZMSG100,
       STLNR	type STNUM,
      end of it_9000.

* like zspp_zapp702 occurs 0 with header line,
*data: it_log  like ztpp_bom_del_log occurs 0 with header line.
data: begin of it_log occurs 0,
       STLTY	type STLTY,
       MATNR	type MATNR,
       WERKS	type WERKS_D,
       STLAN	type STLAN,
       STLAL	type STLAL,
       ERDAT	type ERDAT,
       ERZET	type ERZET,
       ERNAM	type ERNAM,
       MSG	type ZMSG100,
     end of it_log.

data: begin of bdc_tab occurs 0.
        include structure bdcdata.
data: end of bdc_tab.

*---// Global variables and structures
data: w_total(4)   type n,
      w_success(4) type n,
      w_fail(4)    type n.

*---// Constants
constants: c_check                    value 'X'.



*---// Selection screens
selection-screen begin of block bl1 with frame title text-t01.
parameters:
   p_stlan like mast-stlan  memory id csv default '6'
                            no-display,
   p_werks like t001w-werks obligatory memory id wrk.

select-options: s_matnr for mara-matnr obligatory.
selection-screen end   of block bl1.

parameters:
   p_DATUV type DATUV obligatory.

*---// Check input fields & Read data
at selection-screen.
  check sy-ucomm eq 'ONLI'.
  perform check_input_value.
  perform get_bom_data.

start-of-selection.
  perform delete_bom.
  perform create_log.
  perform display_data.

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_input_value.
  perform check_werks.
  perform check_stlan.
  perform check_matnr.
endform.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  get_bom_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_bom_data.
  select * into corresponding fields of table it_9000
    from mast as a inner join stko as b
                      on a~stlnr = b~stlnr
                     and a~stlal = b~stlal
                     and b~stlty = 'M'
                   inner join makt as c
                      on a~matnr = c~matnr
                     and c~spras = sy-langu
   where a~werks eq p_werks
     and a~stlan eq p_stlan
     and a~matnr in s_matnr.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.

  data: l_idx like sy-tabix.
  loop at it_9000.
    l_idx = sy-tabix.

    select single * from stpo where stlty = it_9000-stlty
                              and stlnr = it_9000-stlnr.

    if stpo-datuv < p_datuv.
      delete it_9000 index l_idx.
    endif.
endloop.


  describe table it_9000 lines w_total.
endform.                    " get_bom_data
*&---------------------------------------------------------------------*
*&      Form  DELETE_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_bom.
  loop at it_9000.

    perform generate_bdc_data.

    call transaction 'CS02' using  bdc_tab
                             mode  'E'
                           update  'S'.
    if sy-subrc ne 0 or not ( sy-msgno eq '032' or sy-msgno eq '034' ).
      perform get_error_msg.
      w_fail = w_fail + 1.
    else.
      move: text-m03 to it_9000-msg.
      w_success = w_success + 1.
    endif.

    modify it_9000.
  endloop.
endform.                    " DELETE_BOM
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form generate_bdc_data.
  data: lw_screen(4),
        l_bdcdt(10) type c.

  refresh bdc_tab.

  select single * from stpo where stlty = it_9000-stlty
                              and stlnr = it_9000-stlnr.

  if sy-subrc eq 0.
    lw_screen = '0150'.
  else.
    lw_screen = '0150'.
  endif.

  perform get_bdc_date using stpo-datuv l_bdcdt.

  perform dynpro using:
        'X' 'SAPLCSDI'              '0100',
        ' ' 'RC29N-MATNR'           it_9000-matnr,
        ' ' 'RC29N-WERKS'           it_9000-werks,
        ' ' 'RC29N-STLAN'           it_9000-stlan,
        ' ' 'RC29N-STLAL'           it_9000-stlal,

        ' ' 'RC29N-DATUV'           l_bdcdt,

        ' ' 'BDC_OKCODE'            '=FCPU',

        'X' 'SAPLCSDI'              lw_screen,
        ' ' 'BDC_OKCODE'            '=FCLO',

        'X' 'SAPLSPO1'              '0100',
        ' ' 'BDC_OKCODE'            '=YES'.
endform.                    " GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0148   text
*      -->P_0149   text
*      -->P_0150   text
*----------------------------------------------------------------------*
form dynpro using dynbegin name value.
  if dynbegin = 'X'.
    clear:  bdc_tab.
    move: name  to bdc_tab-program,
          value to bdc_tab-dynpro,
          'X'   to bdc_tab-dynbegin.
    append bdc_tab.
  else.
    clear:  bdc_tab.
    move: name  to bdc_tab-fnam,
          value to bdc_tab-fval.
    append bdc_tab.
  endif.
endform.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  GET_ERROR_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_error_msg.
  call function 'RKC_MSG_STRING'
       exporting
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       importing
            msg_lin = it_9000-msg.
endform.                    " GET_ERROR_MSG
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data.
  sort it_9000 by msg matnr stlan stlal.

  loop at it_9000.
    write:/ it_9000-MATNR, '..deleted.'.
  endloop.
endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_log.
  data: lw_msg01(50),
        lw_msg02(50).

  concatenate text-m04 w_total text-m11 text-m05 w_success
              text-m06 w_fail
         into lw_msg01
    separated by space.

  loop at it_9000.
    move-corresponding it_9000 to it_log.
    move: sy-datum to it_log-erdat,
          sy-uzeit to it_log-erzet,
          sy-uname to it_log-ernam.
    append it_log.
  endloop.

*  insert ztpp_bom_del_log from table it_log accepting duplicate keys.
  if sy-subrc ne 0.
    message s000(zz) with lw_msg01 lw_msg02.
  else.
    message s000(zz) with lw_msg01.
  endif.
endform.                    " create_log
*&---------------------------------------------------------------------*
*&      Form  check_werks
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_werks.
  select single * from t001w where werks = p_werks.
  if sy-subrc ne 0.
    message e000(zz) with text-m08.
  endif.
endform.                    " check_werks
*&---------------------------------------------------------------------*
*&      Form  check_stlan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_stlan.
  select single * from t416t where stlan = p_stlan
                               and spras = sy-langu.
  if sy-subrc ne 0.
    message e000(zz) with text-m09.
  endif.
endform.                    " check_stlan
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_matnr.
  select single * from mara where matnr in s_matnr.
  if sy-subrc ne 0.
    message e000(zz) with text-m10.
  endif.
endform.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  GET_BDC_DATE
*&---------------------------------------------------------------------*
form get_bdc_date using l_fkdat
                        l_bdcdt.
  tables: usr01.
  select single *
         from usr01
        where bname = sy-uname.
  case usr01-datfm.
    when '1'. "DD.MM.YYYY
      l_bdcdt+4(4) = l_fkdat+0(4).
      l_bdcdt+2(2) = l_fkdat+4(2).
      l_bdcdt+0(2) = l_fkdat+6(2).
    when '2' or '3'. "MM/DD/YYYY "MM-DD-YYYY
      l_bdcdt+4(4) = l_fkdat+0(4).
      l_bdcdt+0(2) = l_fkdat+4(2).
      l_bdcdt+2(2) = l_fkdat+6(2).
  endcase.
endform.
