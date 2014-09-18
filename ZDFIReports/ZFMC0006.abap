*&----------------------------------------------------------------------
*& Program ID     : ZFMC0006
*& Program Name   : Budget transfer parking
*& Created by     : YN.Kim
*& Created on     : 18/08/2011
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*&
*&----------------------------------------------------------------------
REPORT  zfmc0006                                .

INCLUDE <icon>.

TABLES: bppe.
type-pools zfmcm.
CONTROLS tc1 TYPE TABLEVIEW USING SCREEN 9000.

*-----/// Constants
DATA: c_euro   LIKE   bkpf-waers   VALUE   zfmcm_euro,
      c_versn  LIKE   bppe-versn   VALUE   zfmcm_versn_0,
      c_fikrs  LIKE   fmdy-fikrs   VALUE   zfmcm_fm_area,
      c_yes                        VALUE   'J',
      c_true                       VALUE   'X'.

*-----/// Internal tables
DATA: BEGIN OF it_list OCCURS 0,
        flag,
        position   LIKE bpdy-blpos,
        xsender    LIKE bpdy-xsender,
        xempfgr    LIKE bpdy-xempfgr,
        fictr      LIKE fmdy-fictr,
        fipex      LIKE fmdy-fipex,
        valu0      LIKE fmbpdy-val0,
      END   OF it_list.

DATA: BEGIN OF st_detl,
        position   LIKE bpdy-blpos,
        fictr      LIKE fmdy-fictr,
        fipex      LIKE fmdy-fipex,
        waers      LIKE bkpf-waers,
        mon01      LIKE fmbpdy-val0,
        mon02      LIKE fmbpdy-val0,
        mon03      LIKE fmbpdy-val0,
        mon04      LIKE fmbpdy-val0,
        mon05      LIKE fmbpdy-val0,
        mon06      LIKE fmbpdy-val0,
        mon07      LIKE fmbpdy-val0,
        mon08      LIKE fmbpdy-val0,
        mon09      LIKE fmbpdy-val0,
        mon10      LIKE fmbpdy-val0,
        mon11      LIKE fmbpdy-val0,
        mon12      LIKE fmbpdy-val0,
        total      LIKE fmbpdy-val0,
      END   OF st_detl.

DATA: it_detl LIKE st_detl OCCURS 0 WITH HEADER LINE.

*> for BDC
DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

*---// Structures
DATA: BEGIN OF st_scr,
        fikrs   LIKE fmdy-fikrs,
        versn   LIKE bpdy-versn,
        waers   LIKE fmdy-fwaer,
        s_geber LIKE bppe-geber,
        r_geber LIKE bppe-geber,
        s_gjahr LIKE bppe-gjahr,
        r_gjahr LIKE bppe-gjahr,
        s_amt   LIKE bppe-wtp01,
        r_amt   LIKE bppe-wtp01,
      END   OF st_scr.

DATA: BEGIN OF st_doc,
        send    LIKE bpbk-belnr,
        msg_s(100),
        tran    LIKE bpbk-belnr,
        msg_t(100),
        reci    LIKE bpbk-belnr,
        msg_r(100),
      END   OF st_doc.

DATA: BEGIN OF st_opt.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF st_opt.

*-----/// Global variable
DATA: ok_code    LIKE sy-ucomm,
      sv_code    LIKE sy-ucomm.

DATA: v_ans,
      sw_err.

*-----///   Start of selection   ///-----*
START-OF-SELECTION.
  SET PARAMETER ID 'FIK' FIELD zfmcm_fm_area.
  PERFORM initial_value.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Form  initial_value
*&---------------------------------------------------------------------*
FORM initial_value .
  CLEAR st_scr.
  MOVE: c_fikrs     TO  st_scr-fikrs,
        c_versn     TO  st_scr-versn,
        c_euro      TO  st_scr-waers.

  DO 999 TIMES.
    it_list-position = sy-index.
    APPEND it_list.  CLEAR it_list.
  ENDDO.

*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  st_opt-defsize = 'X'.
  st_opt-updmode = 'S'.
  st_opt-dismode = 'N'.
*  st_opt-dismode = 'A'.

*  SET PARAMETER ID 'FIK' FIELD st_scr-fikrs.

ENDFORM.                    " initial_value
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.

  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN 9100.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9100'.
    WHEN 9200.
      SET PF-STATUS '9200'.
      SET TITLEBAR  '9200'.
  ENDCASE.

ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  va_funds_f4_fipos  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE va_funds_f4_fipos INPUT.

  PERFORM va_funds_f4_fipos.

ENDMODULE.                 " va_funds_f4_fipos  INPUT
*&---------------------------------------------------------------------*
*&      Form  va_funds_f4_fipos
*&---------------------------------------------------------------------*
FORM va_funds_f4_fipos .

  DATA: l_fipex   LIKE it_list-fipex.
  DATA: l_gjahr   LIKE st_scr-s_gjahr.
  DATA: l_se_line.
  DATA: l_off     VALUE ' '.

  PERFORM vutool_get_lineinfo_is_se CHANGING l_se_line.
  IF l_se_line = c_true.
    l_gjahr = st_scr-s_gjahr.
  ELSE.
    l_gjahr = st_scr-r_gjahr.
  ENDIF.

  CALL FUNCTION 'FM_F4_FOR_COMMITMENT_ITEM'
    EXPORTING
      i_fikrs             = st_scr-fikrs
      i_gjahr             = l_gjahr
      i_flg_only_postable = l_off
    IMPORTING
      e_fipex             = l_fipex
    EXCEPTIONS
      no_select           = 1
      OTHERS              = 2.

  IF sy-subrc IS INITIAL.
    it_list-fipex = l_fipex.
  ENDIF.

ENDFORM.                    " va_funds_f4_fipos
*&---------------------------------------------------------------------*
*&      Form  vutool_get_lineinfo_is_se
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SE_LINE  text
*----------------------------------------------------------------------*
FORM vutool_get_lineinfo_is_se  CHANGING p_se_line.

  DATA: BEGIN OF lt_dynp OCCURS 0.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF lt_dynp.

  DATA: l_stepl LIKE sy-stepl.



*-----Cursor-Zeile-Ermitteln
  CALL FUNCTION 'DYNP_GET_STEPL'
    IMPORTING
      povstepl        = l_stepl
    EXCEPTIONS
      stepl_not_found = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    lt_dynp-fieldname = 'IT_LIST-XSENDER'.
**lt_dynp-stepl     = sy-curow - g_con_header_lines. "***old_solution
    lt_dynp-stepl     = l_stepl.
    APPEND lt_dynp.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynp
      EXCEPTIONS
        OTHERS     = 01.
  ENDIF.

  IF sy-subrc = 0.
    READ TABLE lt_dynp INDEX 1.
  ENDIF.

  IF sy-subrc = 0.
    p_se_line = lt_dynp-fieldvalue.
  ELSE.
    p_se_line = c_true.
  ENDIF.

ENDFORM.                    " vutool_get_lineinfo_is_se
*&---------------------------------------------------------------------*
*&      Module  modify_itab  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_itab INPUT.

  MODIFY it_list INDEX tc1-current_line.

ENDMODULE.                 " modify_itab  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  sv_code = ok_code.
  CLEAR ok_code.
  CASE sv_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'CLSS'.
      PERFORM modify_sum_field.

    WHEN 'PERI'.
      PERFORM goto_period_screen.

    WHEN 'EXECUTE'.
      PERFORM check_data.
      CHECK sw_err <> c_true.
      PERFORM execute_rtn.
      PERFORM shwo_result.
      IF sw_err <> c_true.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  GOTO_PERIOD_SCREEN
*&---------------------------------------------------------------------*
FORM goto_period_screen .

  READ TABLE it_list WITH KEY flag = c_true.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  IF it_list-fictr IS INITIAL.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  IF it_list-fipex IS INITIAL.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.

  CLEAR: it_detl, st_detl.
  READ TABLE it_detl WITH KEY position = it_list-position.
  IF sy-subrc EQ 0.
    st_detl = it_detl.
  ELSE.
    MOVE: it_list-position    TO st_detl-position,
          it_list-fictr       TO st_detl-fictr,
          it_list-fipex       TO st_detl-fipex,
          st_scr-waers        TO st_detl-waers.
  ENDIF.

  CALL SCREEN 9100 STARTING AT 45   3
                   ENDING   AT 78  22.

ENDFORM.                    " GOTO_PERIOD_SCREEN
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.

  sv_code = ok_code.
  CLEAR ok_code.
  CASE sv_code.
    WHEN 'CONT'.
      PERFORM modify_total.

    WHEN 'CLOS'.
      PERFORM modify_total.
      PERFORM transfer_detail.
      LEAVE TO SCREEN 0.

    WHEN 'RSTR'.
      PERFORM clear_detail.

    WHEN 'ESC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_TOTAL
*&---------------------------------------------------------------------*
FORM modify_total .

  st_detl-total = st_detl-mon01 + st_detl-mon02 + st_detl-mon03
                + st_detl-mon04 + st_detl-mon05 + st_detl-mon06
                + st_detl-mon07 + st_detl-mon08 + st_detl-mon09
                + st_detl-mon10 + st_detl-mon11 + st_detl-mon12.

ENDFORM.                    " MODIFY_TOTAL
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DETAIL
*&---------------------------------------------------------------------*
FORM clear_detail .

  CLEAR: st_detl-mon01, st_detl-mon02, st_detl-mon03,
         st_detl-mon04, st_detl-mon05, st_detl-mon06,
         st_detl-mon07, st_detl-mon08, st_detl-mon09,
         st_detl-mon10, st_detl-mon11, st_detl-mon12,
         st_detl-total.

ENDFORM.                    " CLEAR_DETAIL
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DETAIL
*&---------------------------------------------------------------------*
FORM transfer_detail .

  CLEAR it_detl.
  READ TABLE it_detl WITH KEY position = st_detl-position.
  IF sy-subrc EQ 0.
    MODIFY it_detl FROM st_detl INDEX sy-tabix.
  ELSE.
    APPEND st_detl TO it_detl.
  ENDIF.

  CLEAR it_list.
  READ TABLE it_list WITH KEY position = st_detl-position.
  IF sy-subrc EQ 0.
    it_list-valu0 = st_detl-total.
    MODIFY it_list INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " TRANSFER_DETAIL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SUM_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_sum_field OUTPUT.

  PERFORM modify_sum_field.

ENDMODULE.                 " MODIFY_SUM_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  tablecon_pai_loop  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tablecon_pai_loop INPUT.

  PERFORM table_check_field.

ENDMODULE.                 " tablecon_pai_loop  INPUT
*&---------------------------------------------------------------------*
*&      Form  table_check_field
*&---------------------------------------------------------------------*
FORM table_check_field .

  DATA: l_object_entered.

  CLEAR l_object_entered.
  IF NOT ( it_list-fictr IS INITIAL AND
           it_list-fipex IS INITIAL ).
    l_object_entered = c_true.
  ENDIF.

  IF l_object_entered = c_true.
    IF it_list-fictr IS INITIAL.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    IF it_list-fipex IS INITIAL.
      MESSAGE e000(zz) WITH text-m03.
    ENDIF.
  ENDIF.

ENDFORM.                    " table_check_field
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SUM_FIELD
*&---------------------------------------------------------------------*
FORM modify_sum_field .

  CLEAR: st_scr-s_amt, st_scr-r_amt.
  LOOP AT it_list.
    CASE c_true.
      WHEN it_list-xsender.
        ADD it_list-valu0    TO st_scr-s_amt.
      WHEN it_list-xempfgr.
        ADD it_list-valu0    TO st_scr-r_amt.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " MODIFY_SUM_FIELD
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
FORM check_data .

  DATA: text1(40),
        text2(40),
        titl(40).

  CLEAR sw_err.
  LOOP AT it_list.
    IF NOT it_list-fictr IS INITIAL AND
       NOT it_list-fipex IS INITIAL.
      CLEAR it_detl.
      READ TABLE it_detl WITH KEY position = it_list-position.
      IF sy-subrc <> 0.
        sw_err = c_true.
        MESSAGE e000(zz) WITH it_list-position text-m05.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF st_scr-s_amt <> st_scr-r_amt.
    sw_err = c_true.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.

  text1 = text-m06.
*  text2 = text-m07.
  titl  = text-m08.
  PERFORM popup_confirm   USING text1 text2 titl
                                'X'   'A'   v_ans.
  IF v_ans <> c_yes.
    sw_err = c_true.
  ENDIF.

ENDFORM.                    " check_data
*&---------------------------------------------------------------------*
*&      Form  execute_rtn
*&---------------------------------------------------------------------*
FORM execute_rtn .

  CLEAR: st_doc.
  PERFORM transaction_sender.

ENDFORM.                    " execute_rtn
*&---------------------------------------------------------------------*
*&      Form  transaction_sender
*&---------------------------------------------------------------------*
FORM transaction_sender .

  PERFORM generate_bdc_sender.
  PERFORM generate_bdc_transfer.
  PERFORM generate_bdc_receiver.

ENDFORM.                    " transaction_sender
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_sender
*&---------------------------------------------------------------------*
FORM generate_bdc_sender .

  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21).

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'     '0200',
     ' ' 'FMDY-FIKRS'   st_scr-fikrs,         "FM Area
     ' ' 'BPDY-GJAHR'   st_scr-s_gjahr,       "Fiscal year
     ' ' 'BPDY-VERSN'   st_scr-versn,         "Version
     ' ' 'FMDY-FINCODE' st_scr-s_geber,       "Fund
     ' ' 'BDC_OKCODE'   '/00'.

*     'X' 'SAPLKBPB'     '0400',               "Detail screen
*     ' ' 'BDC_OKCODE'   '=DOCH',              "Header
*
*     'X' 'SAPLKBPB'     '0150',               "Header
*     ' ' 'BPDY-SGTXT'   c_sgtxt,              "Text
*     ' ' 'BDC_OKCODE'   '/00'.


  LOOP AT it_list WHERE xsender = c_true
                    AND fictr   <> space
                    AND fipex   <> space.
    CLEAR it_detl.
    READ TABLE it_detl WITH KEY position = it_list-position.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    it_detl-mon01   =  it_detl-mon01 * -1.
    it_detl-mon02   =  it_detl-mon02 * -1.
    it_detl-mon03   =  it_detl-mon03 * -1.
    it_detl-mon04   =  it_detl-mon04 * -1.
    it_detl-mon05   =  it_detl-mon05 * -1.
    it_detl-mon06   =  it_detl-mon06 * -1.
    it_detl-mon07   =  it_detl-mon07 * -1.
    it_detl-mon08   =  it_detl-mon08 * -1.
    it_detl-mon09   =  it_detl-mon09 * -1.
    it_detl-mon10   =  it_detl-mon10 * -1.
    it_detl-mon11   =  it_detl-mon11 * -1.
    it_detl-mon12   =  it_detl-mon12 * -1.

    WRITE: it_detl-mon01     CURRENCY it_detl-waers
                             TO lv_peri1_01(21),
           it_detl-mon02     CURRENCY it_detl-waers
                             TO lv_peri1_02(21),
           it_detl-mon03     CURRENCY it_detl-waers
                             TO lv_peri1_03(21),
           it_detl-mon04     CURRENCY it_detl-waers
                             TO lv_peri1_04(21),
           it_detl-mon05     CURRENCY it_detl-waers
                             TO lv_peri1_05(21),
           it_detl-mon06     CURRENCY it_detl-waers
                             TO lv_peri1_06(21),
           it_detl-mon07     CURRENCY it_detl-waers
                             TO lv_peri1_07(21),
           it_detl-mon08     CURRENCY it_detl-waers
                             TO lv_peri1_08(21),
           it_detl-mon09     CURRENCY it_detl-waers
                             TO lv_peri1_09(21),
           it_detl-mon10     CURRENCY it_detl-waers
                             TO lv_peri1_10(21),
           it_detl-mon11     CURRENCY it_detl-waers
                             TO lv_peri1_11(21),
           it_detl-mon12     CURRENCY it_detl-waers
                             TO lv_peri1_12(21).
    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=INSL',              "Insert

       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'FMDY-FICTR(01)'   it_list-fictr,        "Fund Center
       ' ' 'FMDY-FIPEX(01)'   it_list-fipex,        "Commitment Item
       ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=PERI',

       'X' 'SAPLKBPP'         '0600',               "Period Screen
       ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
       ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
       ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
       ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
       ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
       ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
       ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
       ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
       ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
       ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
       ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
       ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
       ' ' 'G_SCREEN_0600-DK' '0',                  "DK
       ' ' 'BDC_OKCODE'  '=CLOS'.
  ENDLOOP.

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',                 "Detail Screen
     ' ' 'BDC_OKCODE'       '=PARK'.                "Save

  PERFORM posting_rtn_fr62 USING 'S'.
*  PERFORM posting_rtn_fr51 USING 'S'.

ENDFORM.                    " generate_bdc_sender

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.

  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.

ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  posting_rtn_fr62
*&---------------------------------------------------------------------*
FORM posting_rtn_fr62  USING    value(p_0900).

  CALL TRANSACTION 'FR62'  USING it_bdc
                           OPTIONS FROM st_opt.
*  CALL TRANSACTION 'FR51'  USING it_bdc
*                           OPTIONS FROM st_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '044'.
    IF p_0900 = 'S'.
      PERFORM get_err_msg USING st_doc-msg_s.
    ELSE.
      PERFORM get_err_msg USING st_doc-msg_r.
    ENDIF.
    sw_err = c_true.
  ELSE.
    IF p_0900 = 'S'.
      MOVE: sy-msgv1       TO st_doc-send.
    ELSE.
      MOVE: sy-msgv1       TO st_doc-reci.
    ENDIF.
  ENDIF.

ENDFORM.                    " posting_rtn_fr62

*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
FORM get_err_msg USING pw_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
    EXPORTING
      id      = sy-msgid
      mtype   = sy-msgty
      number  = sy-msgno
      par1    = sy-msgv1
      par2    = sy-msgv2
      par3    = sy-msgv3
      par4    = sy-msgv4
    IMPORTING
      msg_lin = lw_msg.

  MOVE: lw_msg TO pw_msg.

ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_transfer
*&---------------------------------------------------------------------*
FORM generate_bdc_transfer .

  CHECK NOT st_doc-send IS INITIAL.

  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21).

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'       '0210',
     ' ' 'FMDY-FIKRS'     st_scr-fikrs,       "FM Area
     ' ' 'BPDY-VERSN'     st_scr-versn,       "Version
     ' ' 'BPDY-U_S_GEBER' st_scr-s_geber,     "Sender fund
     ' ' 'BPDY-U_E_GEBER' st_scr-r_geber,     "Receiver year
     ' ' 'BPDY-U_SJAHR'   st_scr-s_gjahr,     "Sender year
     ' ' 'BPDY-U_EJAHR'   st_scr-r_gjahr,     "Receiver year
     ' ' 'BDC_OKCODE'     '/00',

     'X' 'SAPLKBPB'       '0400',             "Detail screen
     ' ' 'BDC_OKCODE'     '=DOCH',            "Header

     'X' 'SAPLKBPB'     '0150',               "Header
     ' ' 'BPDY-RPUBLAW' st_doc-send,          "sender minus Doc No.
     ' ' 'BDC_OKCODE'   '/00'.

  LOOP AT it_list WHERE fictr   <> space
                    AND fipex   <> space.

    IF it_list-xsender = c_true.
      CONTINUE.
    ENDIF.

    CLEAR it_detl.
    READ TABLE it_detl WITH KEY position = it_list-position.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    WRITE: it_detl-mon01     CURRENCY it_detl-waers
                             TO lv_peri1_01(21),
           it_detl-mon02     CURRENCY it_detl-waers
                             TO lv_peri1_02(21),
           it_detl-mon03     CURRENCY it_detl-waers
                             TO lv_peri1_03(21),
           it_detl-mon04     CURRENCY it_detl-waers
                             TO lv_peri1_04(21),
           it_detl-mon05     CURRENCY it_detl-waers
                             TO lv_peri1_05(21),
           it_detl-mon06     CURRENCY it_detl-waers
                             TO lv_peri1_06(21),
           it_detl-mon07     CURRENCY it_detl-waers
                             TO lv_peri1_07(21),
           it_detl-mon08     CURRENCY it_detl-waers
                             TO lv_peri1_08(21),
           it_detl-mon09     CURRENCY it_detl-waers
                             TO lv_peri1_09(21),
           it_detl-mon10     CURRENCY it_detl-waers
                             TO lv_peri1_10(21),
           it_detl-mon11     CURRENCY it_detl-waers
                             TO lv_peri1_11(21),
           it_detl-mon12     CURRENCY it_detl-waers
                             TO lv_peri1_12(21).
    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=INSL'.              "Insert

    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'FMDY-FICTR(01)'   it_list-fictr,        "Fund Center
       ' ' 'FMDY-FIPEX(01)'   it_list-fipex,        "Commitment Item
       ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X'.          "Check

    IF it_list-xsender = c_true.
      PERFORM dynpro USING:
         ' ' 'BPDY-XSENDER(01)'   'X'.        "Sender
    ELSE.
      PERFORM dynpro USING:
         ' ' 'BPDY-XEMPFGR(01)'   'X'.        "Receiver
    ENDIF.

    PERFORM dynpro USING:
       ' ' 'BDC_OKCODE'       '=PERI',

       'X' 'SAPLKBPP'         '0600',               "Period Screen
       ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
       ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
       ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
       ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
       ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
       ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
       ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
       ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
       ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
       ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
       ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
       ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
       ' ' 'G_SCREEN_0600-DK' '0',                  "DK
       ' ' 'BDC_OKCODE'  '=CLOS'.

  ENDLOOP.

  LOOP AT it_list WHERE fictr   <> space
                    AND fipex   <> space.

    IF it_list-xsender <> c_true.
      CONTINUE.
    ENDIF.

    CLEAR it_detl.
    READ TABLE it_detl WITH KEY position = it_list-position.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    WRITE: it_detl-mon01     CURRENCY it_detl-waers
                             TO lv_peri1_01(21),
           it_detl-mon02     CURRENCY it_detl-waers
                             TO lv_peri1_02(21),
           it_detl-mon03     CURRENCY it_detl-waers
                             TO lv_peri1_03(21),
           it_detl-mon04     CURRENCY it_detl-waers
                             TO lv_peri1_04(21),
           it_detl-mon05     CURRENCY it_detl-waers
                             TO lv_peri1_05(21),
           it_detl-mon06     CURRENCY it_detl-waers
                             TO lv_peri1_06(21),
           it_detl-mon07     CURRENCY it_detl-waers
                             TO lv_peri1_07(21),
           it_detl-mon08     CURRENCY it_detl-waers
                             TO lv_peri1_08(21),
           it_detl-mon09     CURRENCY it_detl-waers
                             TO lv_peri1_09(21),
           it_detl-mon10     CURRENCY it_detl-waers
                             TO lv_peri1_10(21),
           it_detl-mon11     CURRENCY it_detl-waers
                             TO lv_peri1_11(21),
           it_detl-mon12     CURRENCY it_detl-waers
                             TO lv_peri1_12(21).
    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=INSL'.              "Insert

    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'FMDY-FICTR(01)'   it_detl-fictr,        "Fund Center
       ' ' 'FMDY-FIPEX(01)'   it_detl-fipex,        "Commitment Item
       ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X'.          "Check

    IF it_list-xsender = c_true.
      PERFORM dynpro USING:
         ' ' 'BPDY-XSENDER(01)'   'X'.        "Sender
    ELSE.
      PERFORM dynpro USING:
         ' ' 'BPDY-XEMPFGR(01)'   'X'.        "Receiver
    ENDIF.

    PERFORM dynpro USING:
       ' ' 'BDC_OKCODE'       '=PERI',

       'X' 'SAPLKBPP'         '0600',               "Period Screen
       ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
       ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
       ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
       ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
       ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
       ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
       ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
       ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
       ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
       ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
       ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
       ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
       ' ' 'G_SCREEN_0600-DK' '0',                  "DK
       ' ' 'BDC_OKCODE'  '=CLOS'.

  ENDLOOP.

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',                 "Detail Screen
     ' ' 'BDC_OKCODE'       '=PARK'.                "Save

  PERFORM posting_rtn_fr69.

ENDFORM.                    " generate_bdc_transfer
*&---------------------------------------------------------------------*
*&      Form  posting_rtn_fr69
*&---------------------------------------------------------------------*
FORM posting_rtn_fr69 .

  CALL TRANSACTION 'FR69'  USING it_bdc
                           OPTIONS FROM st_opt.
*  CALL TRANSACTION 'FR58'  USING it_bdc
*                           OPTIONS FROM st_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '044'.
    PERFORM get_err_msg USING st_doc-msg_t.
    sw_err = c_true.
  ELSE.
    MOVE: sy-msgv1       TO st_doc-tran.
  ENDIF.

ENDFORM.                    " posting_rtn_fr69
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_receiver
*&---------------------------------------------------------------------*
FORM generate_bdc_receiver .

  CHECK NOT st_doc-send IS INITIAL AND
        NOT st_doc-tran IS INITIAL.

  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21).

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'     '0200',
     ' ' 'FMDY-FIKRS'   st_scr-fikrs,         "FM Area
     ' ' 'BPDY-GJAHR'   st_scr-r_gjahr,       "Fiscal year
     ' ' 'BPDY-VERSN'   st_scr-versn,         "Version
     ' ' 'FMDY-FINCODE' st_scr-r_geber,       "Fund
     ' ' 'BDC_OKCODE'   '/00',

     'X' 'SAPLKBPB'       '0400',             "Detail screen
     ' ' 'BDC_OKCODE'     '=DOCH',            "Header

     'X' 'SAPLKBPB'     '0150',               "Header
     ' ' 'BPDY-RPUBLAW' st_doc-send,          "sender minus Doc No.
     ' ' 'BDC_OKCODE'   '/00'.

  LOOP AT it_list WHERE xempfgr = c_true
                    AND fictr   <> space
                    AND fipex   <> space.
    CLEAR it_detl.
    READ TABLE it_detl WITH KEY position = it_list-position.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    WRITE: it_detl-mon01     CURRENCY it_detl-waers
                             TO lv_peri1_01(21),
           it_detl-mon02     CURRENCY it_detl-waers
                             TO lv_peri1_02(21),
           it_detl-mon03     CURRENCY it_detl-waers
                             TO lv_peri1_03(21),
           it_detl-mon04     CURRENCY it_detl-waers
                             TO lv_peri1_04(21),
           it_detl-mon05     CURRENCY it_detl-waers
                             TO lv_peri1_05(21),
           it_detl-mon06     CURRENCY it_detl-waers
                             TO lv_peri1_06(21),
           it_detl-mon07     CURRENCY it_detl-waers
                             TO lv_peri1_07(21),
           it_detl-mon08     CURRENCY it_detl-waers
                             TO lv_peri1_08(21),
           it_detl-mon09     CURRENCY it_detl-waers
                             TO lv_peri1_09(21),
           it_detl-mon10     CURRENCY it_detl-waers
                             TO lv_peri1_10(21),
           it_detl-mon11     CURRENCY it_detl-waers
                             TO lv_peri1_11(21),
           it_detl-mon12     CURRENCY it_detl-waers
                             TO lv_peri1_12(21).
    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=INSL',              "Insert

       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'FMDY-FICTR(01)'   it_list-fictr,        "Fund Center
       ' ' 'FMDY-FIPEX(01)'   it_list-fipex,        "Commitment Item
       ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=PERI',

       'X' 'SAPLKBPP'         '0600',               "Period Screen
       ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
       ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
       ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
       ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
       ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
       ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
       ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
       ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
       ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
       ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
       ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
       ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
       ' ' 'G_SCREEN_0600-DK' '0',                  "DK
       ' ' 'BDC_OKCODE'  '=CLOS'.
  ENDLOOP.

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',                 "Detail Screen
     ' ' 'BDC_OKCODE'       '=PARK'.                "Save

  PERFORM posting_rtn_fr62 USING 'R'.
*  PERFORM posting_rtn_fr51 USING 'R'.

ENDFORM.                    " generate_bdc_receiver
*&---------------------------------------------------------------------*
*&      Form  shwo_result
*&---------------------------------------------------------------------*
FORM shwo_result .

  CALL SCREEN 9200 STARTING AT 15   3
                   ENDING   AT 78   9.

ENDFORM.                    " shwo_result
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.

  sv_code = ok_code.

  CASE sv_code.
    WHEN 'CONT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9200  INPUT

*&---------------------------------------------------------------------*
*&      Form  popup_confirm
*&---------------------------------------------------------------------*
FORM popup_confirm USING   value(p_txt1)
                           value(p_txt2)
                           value(p_titl)
                           value(p_cancel_flg)
                           value(p_def_val)
                           p_ans.

  CLEAR p_ans.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = p_def_val
      textline1      = p_txt1
      textline2      = p_txt2
      titel          = p_titl
      cancel_display = p_cancel_flg
    IMPORTING
      answer         = p_ans.

ENDFORM.                    " POPUP_CONFIRM
*&---------------------------------------------------------------------*
*&      Module  fictr_FIPOS_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fictr_fipos_check INPUT.
  IF NOT it_list-fictr IS INITIAL.
    CALL FUNCTION 'FM_FICTR_READ_SINGLE'
      EXPORTING
        i_fikrs                  = zfmcm_fm_area
        i_fictr                  = it_list-fictr
        i_date                   = sy-datum
        i_gjahr                  = sy-datum(4)
      EXCEPTIONS
        input_error              = 1
        master_data_not_found    = 2
        hierarchy_data_not_found = 3
        OTHERS                   = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CHECK NOT it_list-fipex IS INITIAL.
      CALL FUNCTION 'FM_FIPEX_READ_SINGLE_DATA'
        EXPORTING
          i_fikrs                  = zfmcm_fm_area
          i_gjahr                  = '0000'
          i_fipex                  = it_list-fipex
        EXCEPTIONS
          master_data_not_found    = 1
          hierarchy_data_not_found = 2
          input_error              = 3
          OTHERS                   = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " fictr_FIPOS_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  POSTING_RTN_FR51
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2591   text
*----------------------------------------------------------------------*
form POSTING_RTN_FR51  using    value(p_0900).
*  CALL TRANSACTION 'FR62'  USING it_bdc
*                           OPTIONS FROM st_opt.
  CALL TRANSACTION 'FR51'  USING it_bdc
                           OPTIONS FROM st_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '044'.
    IF p_0900 = 'S'.
      PERFORM get_err_msg USING st_doc-msg_s.
    ELSE.
      PERFORM get_err_msg USING st_doc-msg_r.
    ENDIF.
    sw_err = c_true.
  ELSE.
    IF p_0900 = 'S'.
      MOVE: sy-msgv1       TO st_doc-send.
    ELSE.
      MOVE: sy-msgv1       TO st_doc-reci.
    ENDIF.
  ENDIF.

endform.                    " POSTING_RTN_FR51
