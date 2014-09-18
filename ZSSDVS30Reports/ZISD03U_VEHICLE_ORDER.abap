************************************************************************
* Author                 : jun ho choi
* Creation Date          : 2003-09-01
* Specifications By      :
* Development Request No : UD1K901594
* Pattern                : 3-1
* Addl documentation     :
* Description            : Vehicle Order Interface.
*
*
*
* Modification Log
* Date       Developer    Request ID Description
* 08.06.2014 Victor       add Vin Spec Check logic with HMMA and HMC
************************************************************************
REPORT zisd03u_vehicle_order NO STANDARD PAGE HEADING
                             MESSAGE-ID zmsd
                             LINE-SIZE 168.


*
TABLES : ztpp_wosum,
         usr01,
         mara,
         vbap,
         knvv,
         ztpp_common_vals.


*
DATA : BEGIN OF it_wosum OCCURS 0.
        INCLUDE STRUCTURE ztpp_wosum.
DATA : END OF it_wosum.

DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

DATA : BEGIN OF mess_tab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF mess_tab.

DATA : BEGIN OF bdc_list OCCURS 0,
       gubun(1),
       gubun_s(1),
       gubun_w(1),
       gubun_m(1),
       message(75),

       wo_ser LIKE ztpp_wosum-wo_ser,
       nation LIKE ztpp_wosum-nation,
       dealer LIKE ztpp_wosum-dealer,
       extc   LIKE ztpp_wosum-extc,
       intc   LIKE ztpp_wosum-intc,
       modqty LIKE ztpp_wosum-modqty,
       fsc    LIKE ztpp_wosum-fsc,
       sales  LIKE ztpp_wosum-sales,
       END OF bdc_list.

* BAPI
DATA : BEGIN OF order_header_inx.
        INCLUDE STRUCTURE bapisdh1x.
DATA : END OF order_header_inx.

DATA : BEGIN OF return OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA : END OF return.

DATA : BEGIN OF order_item_in OCCURS 0.
        INCLUDE STRUCTURE bapisditm.
DATA : END OF order_item_in.

DATA : BEGIN OF order_item_inx OCCURS 0.
        INCLUDE STRUCTURE bapisditmx.
DATA : END OF order_item_inx.

DATA : BEGIN OF schedule_lines OCCURS 0.
        INCLUDE STRUCTURE bapischdl.
DATA : END OF schedule_lines.

DATA : BEGIN OF schedule_linesx OCCURS 0.
        INCLUDE STRUCTURE bapischdlx.
DATA : END OF schedule_linesx.
* BAPI

DATA : w_cnt TYPE i,
       w_cnt_s TYPE i,
       w_cnt_e TYPE i,
       w_date(6) TYPE n,
       w_char_5(5),
       w_char_mod(10),
       w_char_seq(10),
       w_char_20(20),
       w_vbeln LIKE vbak-vbeln,
       w_index LIKE sy-tabix.

DATA : variant LIKE indx-srtfd VALUE 'ZISD03_01'.
DATA: p_date LIKE sy-datum.

*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS : P_DATE LIKE SY-DATUM.
SELECT-OPTIONS: s_date FOR sy-datum NO-EXTENSION OBLIGATORY.
SELECT-OPTIONS: s_wo_ser FOR ztpp_wosum-wo_ser,
                s_nation FOR ztpp_wosum-nation,
                s_dealer FOR ztpp_wosum-dealer.
PARAMETERS : p_vinchk AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN SKIP 1.
PARAMETERS : p_mode(1) DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK b1.

*
INITIALIZATION.
  SELECT SINGLE dates
                INTO p_date
                FROM ztpp_common_vals
               WHERE jobs = 'ZAPP703C_WORKORDER_MAINT'.
  IF sy-subrc = 0.
    p_date = p_date - 1.
  ELSE.
    p_date = sy-datum.
  ENDIF.

  MOVE: 'I'          TO s_date-sign,
        'EQ'         TO s_date-option,
        p_date       TO s_date-low,
        p_date       TO s_date-high.
  APPEND s_date.
*
AT SELECTION-SCREEN ON s_date.
  IF s_date-high IS INITIAL.
    MESSAGE e000 WITH text-002.
  ENDIF.

TOP-OF-PAGE.
  PERFORM top_of_page.

*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.


*
START-OF-SELECTION.
  SET PF-STATUS 'ISD03U'.
  PERFORM get_data.
  PERFORM bdc_process.
  PERFORM display_result.
  PERFORM submit_zisd15.


*
END-OF-SELECTION.


*
AT USER-COMMAND.
  PERFORM user_command.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  IF sy-batch EQ 'X'.
    IMPORT p_date FROM DATABASE indx(zz) ID variant.
    MOVE: 'I'          TO s_date-sign,
          'EQ'         TO s_date-option,
          p_date       TO s_date-low,
          p_date       TO s_date-high.
    APPEND s_date.
  ENDIF.
  REFRESH : it_wosum.
  CLEAR   : it_wosum.

** CHANGED BY FURONG ON 09/09/2005
*  SELECT *
*         INTO TABLE IT_WOSUM
*         FROM ZTPP_WOSUM
*        WHERE WOMODDATE GT P_DATE.
  SELECT *
         INTO TABLE it_wosum
         FROM ztpp_wosum
        WHERE wocredate IN s_date
          AND wo_ser IN s_wo_ser
          AND nation IN s_nation
          AND dealer IN s_dealer.
** END OF CHANGE
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM bdc_process.
  DATA : l_vinspec_check(1).

  DESCRIBE TABLE it_wosum LINES w_cnt.
  IF w_cnt = 0.
    SKIP 5.
    WRITE:/ 'No Entry'.
    STOP.
  ENDIF.

  REFRESH : bdc_list.
  CLEAR   : bdc_list.

  LOOP AT it_wosum.
    CLEAR : l_vinspec_check.

    IF sy-batch NE 'X'.
      PERFORM sapgui_progress_indicator USING 1.
    ENDIF.

    PERFORM move_it_wosum_2_bdc_list.

    IF it_wosum-sales IS INITIAL.
      bdc_list-gubun = '1'. "CREATE

*-    Vin Spec Check 08.06.2014 Victor
      IF p_vinchk = 'X'.
        PERFORM check_vin_spec USING it_wosum CHANGING l_vinspec_check.
        IF l_vinspec_check = 'X'.
          CONTINUE.
        ENDIF.
      ENDIF.

      PERFORM bdc_va01.

      CALL TRANSACTION 'VA01' USING bdc_tab MODE p_mode
                                    UPDATE 'S'
                                    MESSAGES INTO mess_tab.
      PERFORM message_adjust_1 USING 1 bdc_list-gubun_s.
    ELSE.
      bdc_list-gubun = '2'. "CHANGE
      PERFORM check_change.
      IF sy-subrc = 0.

*-    Vin Spec Check 08.06.2014 Victor
        IF p_vinchk = 'X'.
          PERFORM check_vin_spec USING it_wosum CHANGING l_vinspec_check.
          IF l_vinspec_check = 'X'.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF it_wosum-modqty = it_wosum-seqqty OR
           it_wosum-seqqty = 0.
          PERFORM bdc_va02.

          CALL TRANSACTION 'VA02' USING bdc_tab MODE p_mode
                                        UPDATE 'S'
                                        MESSAGES INTO mess_tab.
          PERFORM message_adjust_1 USING 1 bdc_list-gubun_s.
        ELSE.
          PERFORM bapi_va02.
        ENDIF.

        PERFORM message_adjust_2 USING bdc_list-gubun_s.
      ELSEIF sy-subrc = 4. "NOT CHANGE
        CONTINUE.
      ELSEIF sy-subrc = 1. "Quantity decrease is not allowed
        bdc_list-gubun_s = 'E'.
        bdc_list-message = 'Quantity decrease is not allowed'.
      ENDIF.
    ENDIF.

    IF bdc_list-gubun_s = 'S' AND bdc_list-gubun = '1'.
      bdc_list-sales = sy-msgv2.
      PERFORM update_wosum USING bdc_list-gubun_w.
    ENDIF.

    APPEND bdc_list. CLEAR bdc_list.
  ENDLOOP.

  w_cnt = 0.
  LOOP AT bdc_list WHERE gubun_s = 'S' AND gubun = '1'.
    w_cnt = w_cnt + 1.
  ENDLOOP.
  LOOP AT bdc_list WHERE gubun_s = 'S' AND gubun = '1'.
    IF sy-batch NE 'X'.
      PERFORM sapgui_progress_indicator USING 2.
    ENDIF.

    w_index = sy-tabix.
*    REFRESH : BDC_TAB, MESS_TAB.
*    CLEAR   : BDC_TAB, MESS_TAB.
*
*    PERFORM BDC_MM02.
*
*    CALL TRANSACTION 'MM02' USING BDC_TAB MODE P_MODE
*                                  UPDATE 'S'
*                                  MESSAGES INTO MESS_TAB.
*
*    PERFORM MESSAGE_ADJUST_1 USING 3 BDC_LIST-GUBUN_M.

    PERFORM func_mm02.

    MODIFY bdc_list INDEX w_index.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM sapgui_progress_indicator USING gubun.
  DATA : w_perc TYPE p DECIMALS 2,
         w_text(50).

  w_perc = sy-tabix / w_cnt * 100.
  WRITE w_perc TO w_text+0(7).
  CASE gubun.
    WHEN '1'.
      CONCATENATE w_text 'Creating sales order'
                  INTO w_text SEPARATED BY space.
    WHEN '2'.
      CONCATENATE w_text 'Updating work order'
                  INTO w_text SEPARATED BY space.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = w_perc
      text       = w_text.
ENDFORM.                    " SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  MOVE_IT_WOSUM_2_BDC_LIST
*&---------------------------------------------------------------------*
FORM move_it_wosum_2_bdc_list.
  bdc_list-wo_ser = it_wosum-wo_ser.
  bdc_list-nation = it_wosum-nation.
  bdc_list-dealer = it_wosum-dealer.
  bdc_list-extc   = it_wosum-extc.
  bdc_list-intc   = it_wosum-intc.
  bdc_list-modqty = it_wosum-modqty.
  bdc_list-fsc    = it_wosum-fsc.
  bdc_list-sales  = it_wosum-sales.
ENDFORM.                    " MOVE_IT_WOSUM_2_BDC_LIST
*&---------------------------------------------------------------------*
*&      Form  BDC_VA01
*&---------------------------------------------------------------------*
FORM bdc_va01.
  REFRESH : bdc_tab, mess_tab.
  CLEAR   : bdc_tab, mess_tab.

  DATA w_auart LIKE vbak-auart.

  SELECT SINGLE *
         FROM usr01
        WHERE bname = sy-uname.
  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      w_date+4(2) = it_wosum-wo_ser+1(2).
      w_date+2(2) = it_wosum-wo_ser+3(2).
      w_date+0(2) = '01'.
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      w_date+4(2) = it_wosum-wo_ser+1(2).
      w_date+0(2) = it_wosum-wo_ser+3(2).
      w_date+2(2) = '01'.
  ENDCASE.

  SELECT SINGLE *
         FROM mara
        WHERE matnr = it_wosum-fsc.

* will be customer code 3 digit later !!
* so we don't need the delaer code
  CONCATENATE it_wosum-nation it_wosum-dealer
              INTO w_char_5.

  SELECT SINGLE *
         FROM knvv
        WHERE kunnr = w_char_5
        AND   vtweg = '10'
        AND   loevm = ''
        AND   aufsd = ''.

  CONCATENATE it_wosum-wo_ser it_wosum-nation it_wosum-dealer
              it_wosum-extc+0(2) it_wosum-intc+0(2)
              INTO w_char_20.

  WRITE it_wosum-modqty TO w_char_mod.

  PERFORM get_vbeln USING w_vbeln.

  IF it_wosum-dealer+0(1) = 'X'.
    w_auart = 'ZVTO'.
  ELSE.
    w_auart = 'ZVSO'.
  ENDIF.

  PERFORM bdc_fill USING :
          'X' 'SAPMV45A'             '0101',
          ' ' 'VBAK-AUART'           w_auart,
          ' ' 'VBAK-VKORG'           knvv-vkorg,
          ' ' 'VBAK-VTWEG'           knvv-vtweg,
          ' ' 'VBAK-SPART'           mara-spart,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
          ' ' 'VBAK-VBELN'           w_vbeln,
          ' ' 'KUAGV-KUNNR'          w_char_5,
          ' ' 'VBKD-BSTKD'           w_char_20,
          ' ' 'RV45A-KPRGBZ'         'D',
          ' ' 'RV45A-KETDAT'         w_date,
          ' ' 'VBKD-PRSDT'           w_date,
          ' ' 'VBAP-POSNR(01)'       '10',
          ' ' 'VBAP-POSNR(02)'       '20',
          ' ' 'RV45A-MABNR(01)'      it_wosum-fsc,
          ' ' 'RV45A-MABNR(02)'      it_wosum-fsc,
          ' ' 'RV45A-KWMENG(01)'     '0',
          ' ' 'RV45A-KWMENG(02)'     w_char_mod,
          ' ' 'VBAP-UEPOS(02)'       '10',
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPLCEI0'             '0109'.                " ITEM 10
** Changed by Furong on 10/09/07 For EBOM
  IF it_wosum-fsc+13(1) = space.
    PERFORM bdc_fill USING :
            ' ' 'RCTMS-MNAME(01)'      'COLOREXT',
            ' ' 'RCTMS-MNAME(02)'      'COLORINT'.
  ELSE.
    PERFORM bdc_fill USING :
          ' ' 'RCTMS-MNAME(01)'      'COL_EXT',
          ' ' 'RCTMS-MNAME(02)'      'COL_INT'.
  ENDIF.
  PERFORM bdc_fill USING :
          ' ' 'RCTMS-MWERT(01)'      it_wosum-extc,
          ' ' 'RCTMS-MWERT(02)'      it_wosum-intc,
          ' ' 'BDC_OKCODE'           '=BACK',
          'X' 'SAPLCEI0'             '0109'.                " ITEM 20
  IF it_wosum-fsc+13(1) = space.
    PERFORM bdc_fill USING :
          ' ' 'RCTMS-MNAME(01)'      'COLOREXT',
          ' ' 'RCTMS-MNAME(02)'      'COLORINT'.
  ELSE.
    PERFORM bdc_fill USING :
          ' ' 'RCTMS-MNAME(01)'      'COL_EXT',
          ' ' 'RCTMS-MNAME(02)'      'COL_INT'.
  ENDIF.
** End of change
  PERFORM bdc_fill USING :
         ' ' 'RCTMS-MWERT(01)'      it_wosum-extc,
         ' ' 'RCTMS-MWERT(02)'      it_wosum-intc,
         ' ' 'BDC_OKCODE'           '=BACK',
         'X' 'SAPMV45A'             '4001',
         ' ' 'BDC_OKCODE'           '=SICH'.
ENDFORM.                                                    " BDC_VA01
*&---------------------------------------------------------------------*
*&      Form  CHECK_CHANGE
*&---------------------------------------------------------------------*
FORM check_change.
  DATA : w_10 LIKE it_wosum-seqqty,
         w_20 LIKE it_wosum-modqty.

  CLEAR : w_10, w_20.

  SELECT *
         FROM vbap
        WHERE vbeln EQ it_wosum-sales.
    CASE vbap-posnr.
      WHEN '000010'.
        w_10 = vbap-kwmeng.
      WHEN '000020'.
        w_20 = vbap-kwmeng.
    ENDCASE.
  ENDSELECT.

  w_20 = w_20 + w_10.

  IF w_10 EQ it_wosum-seqqty AND
     w_20 EQ it_wosum-modqty.
    sy-subrc = 4.
    EXIT.
  ENDIF.

* Quantity decrease is not allowed
  IF it_wosum-wo_ser+1(4) NE sy-datum+2(4) AND
     w_20 < it_wosum-modqty.
    sy-subrc = 1.
    EXIT.
  ENDIF.
ENDFORM.                    " CHECK_CHANGE
*&---------------------------------------------------------------------*
*&      Form  BDC_VA02
*&---------------------------------------------------------------------*
FORM bdc_va02.
  REFRESH : bdc_tab, mess_tab.
  CLEAR   : bdc_tab, mess_tab.

  it_wosum-modqty = it_wosum-modqty - it_wosum-seqqty.
  WRITE it_wosum-modqty TO w_char_mod.
  WRITE it_wosum-seqqty TO w_char_seq.

  PERFORM bdc_fill USING :
          'X' 'SAPMV45A'             '0102',
          ' ' 'VBAK-VBELN'           it_wosum-sales,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
          ' ' 'RV45A-KWMENG(01)'     w_char_seq,
          ' ' 'RV45A-KWMENG(02)'     w_char_mod,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
          ' ' 'BDC_OKCODE'           '=SICH'.
ENDFORM.                                                    " BDC_VA02
*&---------------------------------------------------------------------*
*&      Form  BAPI_VA02
*&---------------------------------------------------------------------*
FORM bapi_va02.
  it_wosum-modqty = it_wosum-modqty - it_wosum-seqqty.
  WRITE it_wosum-modqty TO w_char_mod.
  WRITE it_wosum-seqqty TO w_char_seq.

  CLEAR   : order_header_inx.
  REFRESH : return, order_item_in, order_item_inx,
            schedule_lines, schedule_linesx.
  CLEAR   : return, order_item_in, order_item_inx,
            schedule_lines, schedule_linesx.

  order_header_inx-updateflag = 'U'.

  order_item_in-itm_number = '000010'. APPEND order_item_in.
  order_item_in-itm_number = '000020'. APPEND order_item_in.

  order_item_inx-updateflag = 'I'.
  order_item_inx-itm_number = '000010'. APPEND order_item_inx.
  order_item_inx-updateflag = 'I'.
  order_item_inx-itm_number = '000020'. APPEND order_item_inx.

  schedule_lines-itm_number = '000010'.
  schedule_lines-sched_line = '0001'.
  schedule_lines-req_qty = w_char_seq. APPEND schedule_lines.
  schedule_lines-itm_number = '000020'.
  schedule_lines-sched_line = '0001'.
  schedule_lines-req_qty = w_char_mod. APPEND schedule_lines.

  schedule_linesx-updateflag = 'U'.
  schedule_linesx-itm_number = '000010'.
  schedule_linesx-sched_line = '0001'.
  schedule_linesx-req_qty = 'X'. APPEND schedule_linesx.
  schedule_linesx-updateflag = 'U'.
  schedule_linesx-itm_number = '000020'.
  schedule_linesx-sched_line = '0001'.
  schedule_linesx-req_qty = 'X'. APPEND schedule_linesx.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument         = it_wosum-sales
*     ORDER_HEADER_IN       =
      order_header_inx      = order_header_inx
*     SIMULATION            = ' '
*     BEHAVE_WHEN_ERROR     = ' '
*     INT_NUMBER_ASSIGNMENT = ' '
*     LOGIC_SWITCH          =
    TABLES
      return                = return
      order_item_in         = order_item_in
      order_item_inx        = order_item_inx
*     PARTNERS              =
*     PARTNERCHANGES        =
*     PARTNERADDRESSES      =
*     ORDER_CFGS_REF        =
*     ORDER_CFGS_INST       =
*     ORDER_CFGS_PART_OF    =
*     ORDER_CFGS_VALUE      =
*     ORDER_CFGS_BLOB       =
*     ORDER_CFGS_VK         =
*     ORDER_CFGS_REFINST    =
      schedule_lines        = schedule_lines
      schedule_linesx       = schedule_linesx
*     ORDER_TEXT            =
*     ORDER_KEYS            =
*     CONDITIONS_IN         =
*     CONDITIONS_INX        =
*     EXTENSIONIN           =
    .
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait   = 'X'
    IMPORTING
      return = return.
ENDFORM.                                                    " BAPI_VA02
*&---------------------------------------------------------------------*
*&      Form  BDC_MM02
*&---------------------------------------------------------------------*
FORM bdc_mm02.
  CONCATENATE bdc_list-wo_ser bdc_list-nation bdc_list-dealer
              bdc_list-extc+0(2) bdc_list-intc+0(2)
              INTO w_char_20.

  PERFORM bdc_fill USING :
          'X' 'SAPLMGMM'             '0060',
          ' ' 'RMMG1-MATNR'          w_char_20+0(18),
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPLMGMM'             '0070',
          ' ' 'BDC_OKCODE'           '=SELA',
          'X' 'SAPLMGMM'             '0070',
          ' ' 'BDC_OKCODE'           '=ENTR',
          'X' 'SAPLMGMM'             '5004',
          ' ' 'BDC_OKCODE'           '=SP03',
          'X' 'SAPLCLCA'             '0602',
          ' ' 'RMCLF-KLART'          '001',
          ' ' 'BDC_OKCODE'           '=ENTE',
          'X' 'SAPLCLFM'             '0500',
          ' ' 'RMCLF-KREUZ(01)'      'X',
          ' ' 'BDC_OKCODE'           '=AUSW',
          'X' 'SAPLCTMS'             '0109',
          ' ' 'RCTMS-MNAME(01)'      'P_SALES_ORDER',
          ' ' 'RCTMS-MWERT(01)'      bdc_list-sales,
          ' ' 'BDC_OKCODE'           '=BACK',
          'X' 'SAPLCLFM'             '0500',
          ' ' 'BDC_OKCODE'           '=SAVE'.
ENDFORM.                                                    " BDC_MM02
*&---------------------------------------------------------------------*
*&      Form  FUNC_MM02
*&---------------------------------------------------------------------*
FORM func_mm02.
  DATA : BEGIN OF val_table OCCURS 0.
          INCLUDE STRUCTURE zspp_vin_value.
  DATA : END OF val_table.

  CONCATENATE bdc_list-wo_ser bdc_list-nation bdc_list-dealer
              bdc_list-extc+0(2) bdc_list-intc+0(2)
              INTO w_char_20.

  REFRESH val_table. CLEAR val_table.

  val_table-atnam = 'P_SALES_ORDER'.
  val_table-atwrt = bdc_list-sales.
  APPEND val_table. CLEAR val_table.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = w_char_20+0(18)
      mode         = 'W'
      ctype        = '001'
*     DISPLAY      = 'D'
    TABLES
      val_table    = val_table
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      OTHERS       = 4.

  IF sy-subrc <> 0.
    bdc_list-gubun_m = 'E'.
    bdc_list-message = 'Work Order does not updated'.
  ELSE.
    READ TABLE val_table INDEX 1.
    IF val_table-zflag = ''. "SUCCESS
      bdc_list-gubun_m = 'S'.
      bdc_list-message = ''.
    ELSE.
      bdc_list-gubun_m = 'E'.
      bdc_list-message = 'Work Order does not updated'.
    ENDIF.
  ENDIF.
ENDFORM.                                                    " FUNC_MM02
*&---------------------------------------------------------------------*
*&      Form  GET_VBELN
*&---------------------------------------------------------------------*
FORM get_vbeln USING vbeln.
  CLEAR vbeln.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = 'ZY'
      object                  = 'RV_BELEG'
    IMPORTING
      number                  = vbeln
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  CONCATENATE '2' it_wosum-wo_ser+1(4) vbeln+5(5) INTO vbeln.
ENDFORM.                    " GET_VBELN
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM bdc_fill USING    p1 p2 p3.
  CLEAR bdc_tab.
  IF p1 = 'X'.
    bdc_tab-dynbegin = p1.
    bdc_tab-program  = p2.
    bdc_tab-dynpro   = p3.
  ELSE.
    bdc_tab-dynbegin = p1.
    bdc_tab-fnam     = p2.
    bdc_tab-fval     = p3.
  ENDIF.
  APPEND bdc_tab.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_1
*&---------------------------------------------------------------------*
FORM message_adjust_1 USING type gubun.
  IF type EQ '1'.
    READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                 msgid  = 'V1'
                                 msgnr  = '311'.
  ELSE. "3
    READ TABLE mess_tab WITH KEY msgtyp = 'S'.
  ENDIF.
  IF sy-subrc = 0.
    gubun = 'S'.
    bdc_list-message = ''.
  ELSE.
    READ TABLE mess_tab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = mess_tab-msgid
          msgnr               = mess_tab-msgnr
          msgv1               = mess_tab-msgv1
          msgv2               = mess_tab-msgv2
          msgv3               = mess_tab-msgv3
          msgv4               = mess_tab-msgv4
        IMPORTING
          message_text_output = bdc_list-message.
      gubun = 'E'.
    ELSE.
      READ TABLE mess_tab INDEX 1.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = mess_tab-msgid
          msgnr               = mess_tab-msgnr
          msgv1               = mess_tab-msgv1
          msgv2               = mess_tab-msgv2
          msgv3               = mess_tab-msgv3
          msgv4               = mess_tab-msgv4
        IMPORTING
          message_text_output = bdc_list-message.
      gubun = 'W'.
    ENDIF.
  ENDIF.
ENDFORM.                    " MESSAGE_ADJUST_1
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_2
*&---------------------------------------------------------------------*
FORM message_adjust_2 USING gubun.
  READ TABLE return WITH KEY type    = 'S'
                             id      = 'V1'
                             number  = '311'.
  IF sy-subrc = 0.
    gubun = 'S'.
  ELSE.
    READ TABLE return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      bdc_list-message = return-message.
      gubun = 'E'.
    ELSE.
      READ TABLE return INDEX 1.
      bdc_list-message = return-message.
      gubun = 'W'.
    ENDIF.
  ENDIF.
ENDFORM.                    " MESSAGE_ADJUST_2
*&---------------------------------------------------------------------*
*&      Form  UPDATE_WOSUM
*&---------------------------------------------------------------------*
FORM update_wosum USING gubun.
  UPDATE ztpp_wosum SET : sales = sy-msgv2
                    WHERE wo_ser = it_wosum-wo_ser
                    AND   nation = it_wosum-nation
                    AND   dealer = it_wosum-dealer
                    AND   extc   = it_wosum-extc
                    AND   intc   = it_wosum-intc.
  IF sy-subrc = 0.
    COMMIT WORK.
    gubun = 'S'.
  ELSE.
    gubun = 'E'.
  ENDIF.
ENDFORM.                    " UPDATE_WOSUM
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM display_result.
  WRITE:/ ''.
  LOOP AT bdc_list.
    WRITE:/ sy-vline.
    CASE bdc_list-gubun_s.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) bdc_list-gubun_s.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) bdc_list-gubun_s.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) bdc_list-gubun_s.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  sy-vline.
    CASE bdc_list-gubun_w.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) bdc_list-gubun_w.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) bdc_list-gubun_w.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) bdc_list-gubun_w.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  sy-vline.
    CASE bdc_list-gubun_m.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) bdc_list-gubun_m.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) bdc_list-gubun_m.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) bdc_list-gubun_m.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    CONCATENATE bdc_list-wo_ser bdc_list-nation bdc_list-dealer
                INTO w_char_20.

    WRITE:  sy-vline, (14) w_char_20+0(14),
            sy-vline, (03) bdc_list-extc,
            sy-vline, (03) bdc_list-intc,
            sy-vline, (07) bdc_list-modqty,
            sy-vline, (18) bdc_list-fsc,
            sy-vline, (10) bdc_list-sales,
            sy-vline, (75) bdc_list-message,
            sy-vline.

    w_index = sy-tabix.
    HIDE : w_index.

    WRITE:/(167) sy-uline.
  ENDLOOP.

ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DESCRIBE TABLE bdc_list LINES w_cnt.
  WRITE:/ 'Total   records :', w_cnt.

  w_cnt_s = 0. w_cnt_e = 0.
  LOOP AT bdc_list.
    IF bdc_list-gubun_s = 'S'.
      w_cnt_s = w_cnt_s + 1.
    ENDIF.
    IF bdc_list-gubun_s <> 'S'.
      w_cnt_e = w_cnt_e + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', w_cnt_s.
  WRITE:/ 'Error   records :', w_cnt_e.

  FORMAT COLOR COL_HEADING.
  WRITE:/(167) sy-uline.
  WRITE:/ sy-vline, (02) 'SO',
          sy-vline, (02) 'WS',
          sy-vline, (02) 'WC',
          sy-vline, (14) 'W/O Number',
          sy-vline, (03) 'Ext',
          sy-vline, (03) 'Int',
          sy-vline, (07) 'Mod Qty',
          sy-vline, (18) 'FSC',
          sy-vline, (10) 'SalesOrder',
          sy-vline, (75) 'Message',
          sy-vline.
  WRITE:/(167) sy-uline.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.
  DATA : ok_code(4).
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REST'.
      PERFORM restarting.
      sy-lsind = sy-lsind - 1.
      PERFORM display_result.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  RESTARTING
*&---------------------------------------------------------------------*
FORM restarting.
  IF sy-lisel+3(1) EQ ' '.
    READ TABLE bdc_list INDEX w_index.
    IF sy-subrc = 0.
      IF bdc_list-gubun_s EQ 'S' AND
         bdc_list-gubun_w EQ 'S' AND
         bdc_list-gubun_m NE 'S'.
*        REFRESH : BDC_TAB, MESS_TAB.
*        CLEAR   : BDC_TAB, MESS_TAB.
*
*        PERFORM BDC_MM02.
*
*        CALL TRANSACTION 'MM02' USING BDC_TAB MODE P_MODE
*                                      UPDATE 'S'
*                                      MESSAGES INTO MESS_TAB.
*
*        PERFORM MESSAGE_ADJUST_1 USING 3 BDC_LIST-GUBUN_M.

        PERFORM func_mm02.

        MODIFY bdc_list INDEX w_index.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE i000 WITH text-m02.
  ENDIF.
ENDFORM.                    " RESTARTING
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_ZISD15
*&---------------------------------------------------------------------*
FORM submit_zisd15.
  DATA : eventid LIKE tbtcjob-eventid,
         variant LIKE indx-srtfd VALUE 'ZISD15_01'.

  EXPORT p_date TO DATABASE indx(zz) ID variant.

  eventid = 'ZISD15_01'.

  CALL FUNCTION 'BP_EVENT_RAISE'
    EXPORTING
      eventid                = eventid
*     EVENTPARM              = ' '
*     TARGET_INSTANCE        = ' '
    EXCEPTIONS
      bad_eventid            = 1
      eventid_does_not_exist = 2
      eventid_missing        = 3
      raise_failed           = 4
      OTHERS                 = 5.
ENDFORM.                    " SUBMIT_ZISD15
*&---------------------------------------------------------------------*
*&      Form  CHECK_VIN_SPEC
*&---------------------------------------------------------------------*
FORM check_vin_spec  USING    ps_wosum STRUCTURE ztpp_wosum
                     CHANGING p_check.
  DATA : l_woups LIKE ztsd_sodata-woups,
         l_atinn LIKE ausp-atinn,
         l_hmc_vinspec LIKE ztpp_vin_if-vin_spec,
         l_hmma_vinspec LIKE ausp-atwrt,
         l_wo    LIKE ausp-objek,
         l_dealer_2(2),
         l_dealer_1(1).

*-Not for Test Work Order
  CHECK ps_wosum-dealer <> 'XA' AND ps_wosum-dealer <> 'XX'
                                and ps_wosum-dealer <> 'XY'.

*-hmma vin spec
  CONCATENATE ps_wosum-wo_ser ps_wosum-nation ps_wosum-dealer
                                                  INTO l_wo.
  SELECT SINGLE atinn INTO l_atinn
    FROM cabn WHERE atnam = 'P_VIN_SPEC'.
  SELECT SINGLE atwrt INTO l_hmma_vinspec
  FROM ausp
  WHERE objek = l_wo
    AND atinn = l_atinn.

*-hmc vin spec
  l_dealer_2 = ps_wosum-dealer.

  CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
    EXPORTING
      old_dealer = l_dealer_2
    IMPORTING
      new_dealer = l_dealer_1.
  SELECT woups INTO l_woups
  FROM ztsd_sodata
    UP TO 1 ROWS
  WHERE wo_ser  = ps_wosum-wo_ser
    AND nation  = ps_wosum-nation
    AND dealer  = ps_wosum-dealer
  ORDER BY wo_ser nation dealer zsdat DESCENDING
                                zstim DESCENDING.
  ENDSELECT.

  IF l_woups IS INITIAL.
    SELECT SINGLE vin_spec
      INTO l_hmc_vinspec
      FROM ztpp_vin_if
      WHERE wo_serial = ps_wosum-wo_ser
        AND wo_nation = ps_wosum-nation
        AND wo_dealer = l_dealer_1.
    IF sy-subrc <> 0.
      p_check = 'X'.
      bdc_list-gubun_s = 'E'.
      bdc_list-message = 'There is No HMC Vin Spec'.
    ELSE.
      IF l_hmc_vinspec+0(8) <> l_hmma_vinspec+0(8).
        p_check = 'X'.
        bdc_list-gubun_s = 'E'.
        CONCATENATE 'Vin Spec does Not match. ' 'HMMA:'
                    l_hmma_vinspec+0(8) '/ HMC:' l_hmc_vinspec+0(8)
                          INTO bdc_list-message SEPARATED BY space.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE vin_spec
      INTO l_hmc_vinspec
      FROM ztpp_vin_if
      WHERE wo_serial = l_woups
        AND wo_nation = ps_wosum-nation
        AND wo_dealer = l_dealer_1.
    IF sy-subrc <> 0.
      p_check = 'X'.
      bdc_list-gubun_s = 'E'.
      bdc_list-message = 'There is No HMC Vin Spec'.
    ELSE.
      IF l_hmc_vinspec+0(8) <> l_hmma_vinspec+0(8).
        p_check = 'X'.
        bdc_list-gubun_s = 'E'.
        CONCATENATE 'Vin Spec does Not match. ' 'HMMA:'
                    l_hmma_vinspec+0(8) '/HMC:' l_hmc_vinspec+0(8)
                          INTO bdc_list-message SEPARATED BY space.
      ENDIF.
    ENDIF.

  ENDIF.

  IF p_check = 'X'.
    APPEND bdc_list. CLEAR bdc_list.
  ENDIF.
ENDFORM.                    " CHECK_VIN_SPEC
