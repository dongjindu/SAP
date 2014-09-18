*----------------------------------------------------------------------*
***INCLUDE ZAPP221M_FOB .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CHANGE_TOGGLE
*&---------------------------------------------------------------------*
*       Changing Toggle's Mode
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_change_toggle_1203.
  if g_toggle_1203 ne 'C'.
    g_toggle_1203 = 'C'.
  else.
    clear g_toggle_1203.
  endif.
endform.                    " DISPLAY_CHANGE_TOGGLE
*&---------------------------------------------------------------------*
*&      Form  NATION_DATA_SAVE
*&---------------------------------------------------------------------*
*       Updating Data with toggle( C: Change, A: Insert)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form nation_data_save_1203.

  if  g_editdata_1203  ne  'X'.
    clear  g_toggle_1203.
    message s001  with 'No Save data'.
    exit.
  endif.
  if g_toggle_1203 eq 'C'.
    update  ztpp_nation_def  set: name      =  IS_APP221-name
                                  dealer    =  IS_APP221-dealer
                                  address1  =  IS_APP221-address1
                                  address2  =  IS_APP221-address2
                                  address3  =  IS_APP221-address3
                                  drive     =  IS_APP221-drive
                                  weather   =  IS_APP221-weather
                                  region    =  IS_APP221-region
                                  spec      =  IS_APP221-spec
                                  port      =  IS_APP221-port
                                  portname  =  IS_APP221-portname
                                  langu     =  IS_APP221-langu
                                  warranty  =  IS_APP221-warranty
                                  anti_rust =  IS_APP221-anti_rust
                                  n_code    =  IS_APP221-n_code
                                  aedat     =  sy-datum
                                  aezet     =  sy-uzeit
                                  aenam     =  sy-uname
            where   nation eq  is_app221-nation.

    if sy-subrc  eq  0.
      message s000  with  'Data Change Succesful !!'.
      clear: g_toggle_1203,  g_editdata_1203.
      perform  it_app221_modyfy.
    else.
      message e003  with  is_app221-nation 'Data Change Fail'.
    endif.
  endif.

  if g_toggle_1203  eq  'A'.
    clear  ztpp_nation_def.
    move-corresponding  is_app221  to  ztpp_nation_def.
    move   sy-datum                to  ztpp_nation_def-erdat.
    move   sy-uzeit                to  ztpp_nation_def-erzet.
    move   sy-uname                to  ztpp_nation_def-ernam.

    insert ztpp_nation_def.
    case  sy-subrc.
      when   0.
        perform  it_app221_append.
        message s000  with  'Data Insert Succesful !!'.
        clear: g_toggle_1203,  g_EDITDATA_1203.
      when   4.
        message e000  with  'Dupplicate Nation code!'.
      when   others.
        message e003  with  is_app221-nation 'Data Change Fail'.
    endcase.
  endif.
endform.                    " NATION_DATA_SAVE
*&---------------------------------------------------------------------*
*&      Form  nation_display
*&---------------------------------------------------------------------*
*       Searching Data & Handling Error
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form nation_display_1203.
  data: l_nation  like  ztpp_nation_def-nation.

  check  g_toggle_1203 ne 'C' and g_toggle_1203 ne 'A'.

  select  single * from  ztpp_nation_def
    into corresponding fields of is_app221
    where nation = is_app221-nation.

  if  sy-subrc ne  0.
    l_nation = is_app221-nation.
    clear is_app221.
    is_app221-nation = l_nation.
    message s000 with 'It is country code that is not registered.'.
  endif.
endform.                    " nation_display
*&---------------------------------------------------------------------*
*&      Form  nation_data_insert
*&---------------------------------------------------------------------*
*       Setting Toggle's Mode For New Data Creation
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form nation_data_insert_1203.
  clear  is_app221.
  g_toggle_1203 = 'A'.       "insert mode

endform.                    " nation_data_insert
*&---------------------------------------------------------------------*
*&      Form  IT_APP221_MODYFY
*&---------------------------------------------------------------------*
*       Modification of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form it_app221_modyfy.

  read table it_app221  with key  nation = is_app221-nation.
  check sy-subrc eq 0.
  move-corresponding is_app221  to  it_app221.
  modify  it_app221  index  sy-tabix.

endform.                    " IT_APP221_MODYFY
*&---------------------------------------------------------------------*
*&      Form  IT_APP221_APPEND
*&---------------------------------------------------------------------*
*       Setting Screen's Data with Internal Table.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form it_app221_append.

  clear  it_app221.
  move-corresponding  is_app221  to  it_app221.
  append  it_app221.

  sort  it_app221  by  nation.
  loop at  it_app221.
    move  sy-tabix   to  it_app221-seq.
    modify it_app221.
  endloop.

  describe table it_app221  lines  g_seq_1203.

  read table it_app221  with key nation = is_app221.
  if sy-subrc eq 0.
    move-corresponding   it_app221   to   is_app221.
  endif.

endform.                    " IT_APP221_APPEND
*&---------------------------------------------------------------------*
*&      Form  transport_to_alc
*&---------------------------------------------------------------------*
form transport_to_alc_1203.
  data: l_msgtxt(100)        .

  refresh it_zsppvn1. clear it_zsppvn1.

  move-corresponding is_app221   to  it_zsppvn1.
  it_zsppvn1-zsdat     = sy-datum.
  it_zsppvn1-MODI_DATE = sy-datum.
  it_zsppvn1-zstim     = sy-uzeit.
  append it_zsppvn1. clear it_zsppvn1.

  call function 'Z_FPP_SET_ZTPPVN1'
                destination c_dest
    tables
      i_zsppvn1       =  it_zsppvn1
    exceptions
      communication_failure = 1  message l_msgtxt
      system_failure        = 2  message l_msgtxt.

  if sy-subrc = 0.
    message s001 with 'Completed Update Successfully'.
    modify ztppvn1 from table it_zsppvn1.
    if sy-subrc = 0.
      commit work.
    else.
      rollback work.
    endif.
  else.
    message e001 with l_msgtxt.
  endif.
endform.                    " transport_to_alc
*&---------------------------------------------------------------------*
*&      Form  exit_process_1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exit_process_1203.
  if g_editdata_1203  ne 'X'.
    leave  to  screen  0.
  endif.

  call function 'POPUP_TO_CONFIRM_STEP'
       exporting
            textline1 = 'Nation Data has been changed.'
            textline2 = 'Save data?'
            titel     = 'Save Confirm'
       importing
            answer    = G_ANSWER_1203.

  case  G_ANSWER_1203.
    when  'J'.
      perform  nation_data_save_1203.
      leave to screen 0.
    when  'N'.
      leave to screen 0.
    when  'A'.
  endcase.

endform.                    " exit_process_1203
