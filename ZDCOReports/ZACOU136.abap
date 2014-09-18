*----------------------------------------------------------------------
* Program ID        : ZACOU136
* Title             : [CO] Simple version of reverse doc.
* Created on        : 12/04/2008
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Reverse Plan Doc.
*----------------------------------------------------------------------
report zacou136 message-id zmco.

include <icon>.                        " icon

constants:  false value ' ',
            true  value 'X'.

data  g_error.

tables : cobk, coej, sscrfields.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
* controlling area selection
selection-screen begin of block co_area.
selection-screen skip 2.

parameters: p_kokrs like tka01-kokrs  memory id cac.
selection-screen end of block co_area.
selection-screen skip 2.

selection-screen begin of block documents.
* document selection
parameters p_belnr like cobk-belnr memory id bln obligatory.
parameters p_tst as checkbox default 'X'.
selection-screen skip 2.
selection-screen end of block documents.

selection-screen begin of block view-result.
selection-screen skip.
selection-screen pushbutton  1(30) vslt user-command vslt.
selection-screen end of block view-result.

parameters p_delta default 'X' no-display.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
initialization.
  sy-title = '[CO] Reverse Plan Document'.
  perform default_.

*----------------------------------------------------------------------*
start-of-selection.
*----------------------------------------------------------------------*
  clear g_error.
  perform really?.
  check g_error eq false.

  data: ls_headerinfo like bapiplnhdr,
        lt_indexstructure like bapiacpstru occurs 0,
        lt_coobject like bapipcpobj occurs 0,
        lt_pervalue like bapipcpval occurs 0,
        lt_contrl   like bapipcpctrl occurs 0,
        lt_return   like bapiret2 occurs 0,
        ls_return like line of lt_return.

  perform fill_tables tables   lt_indexstructure
                               lt_coobject
                               lt_pervalue
                      changing ls_headerinfo.

  check g_error eq false.

  if p_tst eq true.

    call function 'BAPI_COSTACTPLN_CHECKPRIMCOST'
         exporting
              headerinfo     = ls_headerinfo
              delta          = p_delta
         tables
              indexstructure = lt_indexstructure
              coobject       = lt_coobject
              pervalue       = lt_pervalue
              contrl         = lt_contrl
              return         = lt_return.

    describe table lt_return lines sy-tabix.
    if sy-tabix eq 0.
      message s000 with p_belnr ' has been tested with no error!'.
    endif.


  else.

    call function 'BAPI_COSTACTPLN_POSTPRIMCOST'
         exporting
              headerinfo     = ls_headerinfo
              delta          = p_delta
         tables
              indexstructure = lt_indexstructure
              coobject       = lt_coobject
              pervalue       = lt_pervalue
              contrl         = lt_contrl
              return         = lt_return.

    describe table lt_return lines sy-tabix.
    if sy-tabix eq 0.
      message s000 with p_belnr ' has been revsered successfully!'.
    endif.


    commit work.

  endif.

  loop at lt_return into ls_return.
    write: ls_return.
  endloop.


end-of-selection.

*----------------------------------------------------------------------*

at selection-screen.
  case sscrfields-ucomm.
    when 'VSLT'.
      clear g_error.
      set parameter id 'BLN' field p_belnr.
      call transaction 'KABP' and skip first screen.
  endcase.

*&---------------------------------------------------------------------*
at selection-screen on value-request for p_belnr.
*&---------------------------------------------------------------------*
  call function 'K_DOCUMENT_SELECT'
       exporting
            imp_kokrs = p_kokrs
            imp_eptab = 'COEJ'
       importing
            exp_belnr = p_belnr.


*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).

  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = pf_val
            text       = pf_text.

endform.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form default_.

  write:
          icon_biw_report_view as icon to vslt,
         'View Document Data' to vslt+4(24).

endform.                    " default_
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form really?.

  data l_answer(1).

  perform pop_up using
      'Please confirm!'
      'Do you really want to reverse?' ' '
                 changing l_answer.

  if l_answer ne 'J'.
    g_error = true.
    message s000 with 'Processing was canceled by user.'.
  endif.

endform.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  pop_up
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0120   text
*      -->P_0121   text
*      -->P_0122   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
form pop_up using    p_text p_text2 p_canc
            changing p_answer.

  call function 'POPUP_TO_CONFIRM_STEP'
       exporting
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Confirm!'
            cancel_display = p_canc
       importing
            answer         = p_answer.


endform.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  fill_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_INDEXSTRUCTURE  text
*      -->P_LT_COOBJECT  text
*      -->P_LT_TOTVALUE  text
*      <--P_LS_HEADERINFO  text
*----------------------------------------------------------------------*
form fill_tables tables et_indexstructure structure bapiacpstru
                        et_coobject       structure bapipcpobj
                        et_pervalue       structure bapipcpval
                 changing es_headerinfo   structure bapiplnhdr.

  data : fix_val_perxx type bapicurr_d,
         var_val_perxx type bapicurr_d,
         wtgxxx        type wtgxxx,
         wkfxxx        type wkfxxx.


  clear es_headerinfo.
  clear et_indexstructure.

  select single * from cobk where kokrs eq p_kokrs
                       and belnr eq p_belnr.

  if sy-subrc ne 0.
    g_error = true.
    message s000 with 'Could not find document.'.
    exit.
  endif.

  es_headerinfo-co_area       =       cobk-kokrs.
  es_headerinfo-fisc_year     =       cobk-gjahr.
  es_headerinfo-period_from   =       cobk-perab.
  es_headerinfo-period_to     =       cobk-perbi.
  es_headerinfo-version       =       cobk-versn.
  es_headerinfo-doc_hdr_tx    =       cobk-bltxt.
  es_headerinfo-plan_currtype =       'C'.

  select * from coej where kokrs eq p_kokrs
                        and belnr eq p_belnr.

    et_indexstructure-object_index = coej-buzei.
    et_indexstructure-value_index  = coej-buzei.
    append et_indexstructure to et_indexstructure.

    et_coobject-object_index = coej-buzei.
    case coej-objnr(2).
      when 'KS'.
        et_coobject-costcenter = coej-objnr+6(10).
        et_coobject-acttype = coej-objnr+16.
      when 'OR'.
        et_coobject-orderid = coej-objnr+3(12).

      when others.

    endcase.

    append et_coobject to et_coobject.

    et_pervalue-value_index = coej-buzei.
    et_pervalue-cost_elem   = coej-kstar.

    do 16 times varying wtgxxx        from coej-wtg001
                                      next coej-wtg002
                varying wkfxxx        from coej-wkf001
                                      next coej-wkf002
                varying fix_val_perxx from et_pervalue-fix_val_per01
                                      next et_pervalue-fix_val_per02
                varying var_val_perxx from et_pervalue-var_val_per01
                                      next et_pervalue-var_val_per02.

      fix_val_perxx = wkfxxx * -1.
      var_val_perxx = ( wtgxxx - wkfxxx ) * - 1.

    enddo.

    append et_pervalue.
    clear  et_pervalue.

  endselect.

endform.                    " FILL_TABLES
