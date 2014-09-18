*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM p2000_get_data.
  DATA : lt_zwosum LIKE TABLE OF ztpp_wosum WITH HEADER LINE,
         lt_cabn   LIKE TABLE OF zv_cabn WITH HEADER LINE,
         lt_s219   LIKE TABLE OF zv_cabn WITH HEADER LINE.

  DATA : lv_objek  LIKE ausp-objek,
         lv_objekc LIKE ausp-objek.

  DATA : BEGIN OF lt_objek OCCURS 0,
            objek LIKE ausp-objek,
         END OF lt_objek.

  DATA : lt_edidd LIKE TABLE OF edidd WITH HEADER LINE,
         lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
         l_idoc_ctrl LIKE edidc.

  RANGES : r_atnam  FOR cabn-atnam,
           r_atnamc FOR cabn-atnam.

  CLEAR : gt_data[], gt_data.    "Victor 06.24.2011

**#0. Set Range For 'P_219%' .
  r_atnam-low = 'P_219*'.
  r_atnam-option = 'CP'.
  r_atnam-sign = 'I'.
  APPEND r_atnam.

**#1 GET ZTPP_WOSUM DATA .
  PERFORM show_progress     USING 'Data gathering...' '5'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zwosum
  FROM ztpp_wosum
  WHERE wo_ser IN p_woser
*      AND nation IN s_nation
    AND nation = l_nation         "02.27.2014 Victor
    AND dealer IN s_dealer
    AND erdat  IN p_datum.

  CHECK NOT lt_zwosum[] IS INITIAL.

  PERFORM show_progress     USING 'Data gathering...' '10'.


  SORT lt_zwosum BY wo_ser.

  LOOP AT lt_zwosum.

    MOVE-CORRESPONDING lt_zwosum TO gt_data.

    gt_data-initqty = '1'.
    gt_data-_ioqty  = 1.
    gt_data-modqty  = '1'.
    gt_data-_moqty  = 1.

    gt_data-crt_date  = lt_zwosum-wocredate.
    gt_data-_crt_date = lt_zwosum-wocredate.

    gt_data-chg_date  = lt_zwosum-erdat.  "Victor 06.30.2011
    gt_data-_chg_date = lt_zwosum-erdat.

    CONCATENATE lt_zwosum-wo_ser lt_zwosum-nation  lt_zwosum-dealer
                                                    INTO lv_objek.

    CONCATENATE lt_zwosum-wo_ser lt_zwosum-nation  lt_zwosum-dealer
                lt_zwosum-extc   lt_zwosum-intc    INTO lv_objekc.

**  #2 GET AUSP-ATWRT DATA
    PERFORM p2210_getsingle_atwrt USING :
        lv_objek gt_data-moye  'P_MODEL_YEAR',
        lv_objek gt_data-bmdl  'P_MI',
        lv_objek gt_data-ocnn  'P_OCN',
        lv_objek gt_data-vers  'P_VERSION',
        lv_objek gt_data-dest  'P_DESTINATION_CODE',
        lv_objekc gt_data-clsr  'P_COLOR_SER',
        lv_objekc gt_data-flet  'P_FLEET',
                        lv_objek gt_data-lcnt 'P_LC_COUNT',
                        lv_objek gt_data-lcno 'P_LC_NO'.

**  #3 GET AUSP-ATINN - S219 DATA
    SELECT  au~objek
            au~atwrt
            ct~atbez
       INTO CORRESPONDING FIELDS OF TABLE lt_s219
       FROM ausp AS au
       JOIN cabn AS cb
         ON au~atinn EQ cb~atinn
*        AND AU~ADZHL EQ CB~ADZHL
        JOIN cabnt AS ct
         ON au~atinn EQ ct~atinn
*        AND AU~ADZHL EQ CT~ADZHL
        WHERE au~klart EQ '001'
          AND au~objek EQ lv_objek
          AND au~mafid EQ 'O'
          AND cb~atnam IN r_atnam  .


    SORT lt_s219 BY objek atbez.
    READ TABLE lt_s219 WITH KEY objek = lv_objek
                                atbez = 'YEAR'
                                BINARY SEARCH.
    IF sy-subrc <> 0 . CLEAR : lt_s219 . ENDIF.
    gt_data-s219 = lt_s219-atwrt .

    LOOP AT lt_s219 WHERE objek = lv_objek
                      AND atbez NE 'YEAR'.
      CONCATENATE gt_data-s219 lt_s219-atwrt INTO gt_data-s219 .
    ENDLOOP.

*    IF lt_zwosum-wo_ser  CP 'E++++Z*'.  "Victor 06.24.2011
    gt_data-desc = wa_ale_dest-comp.
*      IF lt_zwosum-nation = 'B28'.
*        gt_data-desc   =  'HMA Spec'.
*      ELSE.
*        gt_data-desc   =  'HAC Spec'.
*      ENDIF.

    IF gt_data-bmdl IS INITIAL OR gt_data-s219 IS INITIAL.
      gt_data-check    = 'X'.
      gt_data-select   = ''.
      gt_data-status   = '@0A@'.
    ELSE.
      gt_data-check    = 'X'.
      gt_data-select   = 'X'.
    ENDIF.
*    ELSE.
*      gt_data-desc   =  'Not Spec'.
*      gt_data-check    = ' '.
*      gt_data-select   = ' '.
*      gt_data-status   = '@0A@'.
*    ENDIF.

    APPEND gt_data.

* by Daniel on 06/21/11 {
    CLEAR gt_data.
* }
  ENDLOOP.

  PERFORM show_progress     USING 'Data gathering...' '70'.

ENDFORM.                    "p2000_get_data

*&---------------------------------------------------------------------*
*&      Form  P2210_GETSINGLE_ATWRT
*&---------------------------------------------------------------------*
FORM p2210_getsingle_atwrt USING p_objek p_atwrt p_atnam.

  DATA : lv_atinn LIKE ausp-atinn.
  PERFORM p4000_conversion_atinn USING p_atnam lv_atinn.

  SELECT SINGLE atwrt INTO p_atwrt
  FROM ausp
  WHERE klart = '001'
    AND atinn = lv_atinn
    AND objek = p_objek
    AND mafid EQ 'O'
* %_HINTS ORACLE 'index("AUSP","AUSP~N1")'
 .

ENDFORM.                    " P2210_GETSINGLE_ATWRT

*---------------------------------------------------------------------*
*       FORM P3100_GENERATE_CONTROL_RECORD                            *
*---------------------------------------------------------------------*
FORM p3100_generate_control_record TABLES p_edidc STRUCTURE edidc .
  DATA : wa_edp13 TYPE edp13,
         wa_edidc LIKE p_edidc.

  SELECT SINGLE * INTO  wa_edp13
           FROM edp13 WHERE mestyp = c_mestyp.


  wa_edidc-mestyp =  wa_edp13-mestyp . "Message type
  wa_edidc-idoctp =  wa_edp13-idoctyp ."Basic IDOC type
  wa_edidc-rcvprt =  wa_edp13-rcvprt ."Partner type of receiver 'LS'

  wa_edidc-rcvprn =  wa_ale_dest-rcvprn . "Partner number of recei
  wa_edidc-rcvpor =  wa_ale_dest-rcvpor . "Receiver Port
*    wa_edidc-sndprn =  wa_ale_dest-sndprn. "Sender Partner Number
  APPEND wa_edidc TO p_edidc.

**<Below is Old coding
*  SELECT * INTO TABLE lt_edp13
*           FROM edp13 WHERE mestyp = c_mestyp.
*
*  LOOP AT lt_edp13.
*    ll_edidc-rcvprn =  lt_edp13-rcvprn . "Partner number of receiver
*    ll_edidc-rcvpor =  lt_edp13-rcvpor . "Receiver Port
*    ll_edidc-mestyp =  lt_edp13-mestyp . "Message type
*    ll_edidc-idoctp =  lt_edp13-idoctyp ."Basic IDOC type
*    ll_edidc-rcvprt =  lt_edp13-rcvprt ."Partner type of receiver 'LS'
*    APPEND ll_edidc TO p_edidc.
*  ENDLOOP.
ENDFORM.                    " generate_control_record

*---------------------------------------------------------------------*
*       FORM P3000_SEND_IDOC                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_EDIDD                                                       *
*  -->  P_EDIDC                                                       *
*---------------------------------------------------------------------*
FORM p3000_send_idoc TABLES p_edidd STRUCTURE edidd
                            p_edidc STRUCTURE edidc.

  DATA : l_idoc_ctrl LIKE TABLE OF edidc WITH HEADER LINE.
  DATA : message(1000) .


  PERFORM  p3100_generate_control_record TABLES l_idoc_ctrl.

  LOOP AT l_idoc_ctrl.
    CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
      EXPORTING
        master_idoc_control            = l_idoc_ctrl
      TABLES
        communication_idoc_control     = p_edidc
        master_idoc_data               = p_edidd
      EXCEPTIONS
        error_in_idoc_control          = 1
        error_writing_idoc_status      = 2
        error_in_idoc_data             = 3
        sending_logical_system_unknown = 4
        OTHERS                         = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gt_data-status = '@02@'.
    ELSE.
      gt_data-status = '@01@'.
      DATA : lv_datum(100).
      COMMIT WORK.
* by Daniel on 01/03/12 {
      CALL FUNCTION 'DEQUEUE_ALL'. "Empty Queue -> send Idoc
* }
      WRITE p_datum-low  TO lv_datum.
      CONCATENATE p_datum-low '' INTO lv_datum.
      IF NOT p_datum-high IS  INITIAL.
        CONCATENATE lv_datum ' ' '~ ' p_datum-high INTO lv_datum.
      ENDIF.
      CLEAR : gv_docnum.

      LOOP AT p_edidc.
        PERFORM conversion_output USING : p_edidc-docnum,
                                          p_edidc-maxsegnum .
        gv_docnum = p_edidc-docnum.
        CONCATENATE
          message
          'Create '
          p_edidc-docnum 'IDoc number'
          ' ' 'and '
          'Generated' p_edidc-maxsegnum 'count.'
        INTO message  SEPARATED BY ' '.
      ENDLOOP.

      CONCATENATE 'Date Period : ' lv_datum
      INTO lv_datum.


      MESSAGE s001 WITH lv_datum message .

    ENDIF.
  ENDLOOP.

  gt_data-select = ''.
  MODIFY gt_data TRANSPORTING status select WHERE select = 'X' .

ENDFORM.                    "p3000_send_idoc


*---------------------------------------------------------------------*
*       FORM P4000_CONVERSION_ATINN                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*  -->  P_ATINN                                                       *
*---------------------------------------------------------------------*
FORM p4000_conversion_atinn USING p_value p_atinn .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_atinn.


ENDFORM.                    "p4000_conversion_atinn

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_1
*&---------------------------------------------------------------------*
FORM popup_to_confirm USING p_default
                            p_title
                            p_text1
                            p_text2
                            p_display
                      CHANGING p_answer.

  DATA: lv_company(5).
  DATA: lv_text2(70).

*  IF p_ra1 = 'X'.
*    lv_company = text-t03.
*  ELSEIF p_ra2 = 'X'.
*    lv_company = text-t04.
*  ENDIF.

  CONCATENATE p_text2 wa_ale_dest-comp '?' INTO lv_text2 SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = p_default
      textline1      = p_text1
      textline2      = lv_text2
      titel          = p_title
      cancel_display = p_display
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found.

ENDFORM.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_KUNNR_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_INPUT_KUNNR  text
*----------------------------------------------------------------------*
FORM conversion_output  USING    p_value.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_value.


ENDFORM.                    " CONVERSION_KUNNR_INPUT
*&---------------------------------------------------------------------*
*&      Form  CALL_LOG_PRG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_log_prg.

  DATA: ls_seltab TYPE STANDARD TABLE OF rsparams WITH HEADER LINE.

  CLEAR ls_seltab .
  ls_seltab-selname  = 'S_WOSER'.
  ls_seltab-sign     = 'I'.
  ls_seltab-option   = 'EQ'.

  LOOP AT gt_data.
    ls_seltab-low      = gt_data-wo_ser.
    APPEND  ls_seltab.
  ENDLOOP.

  SORT ls_seltab BY low.
  DELETE ADJACENT DUPLICATES FROM ls_seltab.

  CLEAR  ls_seltab.
  ls_seltab-selname = 'S_DATUM'.
  ls_seltab-sign = 'I'.
  ls_seltab-option = 'EQ'.
  ls_seltab-low = sy-datum.

  APPEND ls_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'S_DOCNUM'.
  ls_seltab-sign = 'I'.
  ls_seltab-option = 'EQ'.
  ls_seltab-low = gv_docnum.

  APPEND ls_seltab.


  DATA : l_idoc_ctrl LIKE TABLE OF edidc WITH HEADER LINE,
         lv_sndprn LIKE edidc-sndprn.
  PERFORM  p3100_generate_control_record TABLES l_idoc_ctrl.

  READ TABLE l_idoc_ctrl INDEX 1.
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = lv_sndprn.


  SUBMIT zrpp_hma_idoc_report
       WITH p_mstyp EQ c_mestyp "'ZSPEC_ORD_MST'
       WITH p_direct EQ '1'
       WITH p_rcvprn EQ l_idoc_ctrl-rcvprn "'NDECLNT850'
       WITH p_sndprn EQ lv_sndprn                           "'UQ1300'
       WITH SELECTION-TABLE ls_seltab
       AND RETURN.
*       VIA SELECTION-SCREEN AND RETURN.


ENDFORM.                    " CALL_LOG_PRG

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  PRO_CONVERT_COLOR
*&---------------------------------------------------------------------*
FORM pro_convert_color .
  DATA : lv_model(3).
  lv_model  =  gt_data-bmdl+0(2).
  CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
    EXPORTING
      i_model = lv_model
      i_year  = gt_data-moye
      i_gubn  = ''            "HMMA -> HAC/HMM
      i_extc  = gt_data-extc
      i_intc  = gt_data-intc
    IMPORTING
      e_extc  = gt_data-extc
      e_intc  = gt_data-intc.
ENDFORM.                    " PRO_CONVERT_COLOR

*&---------------------------------------------------------------------*
*&      Form  GET_NATION
*&---------------------------------------------------------------------*
FORM get_nation .

  IF p_ra1 = 'X'.
    l_nation  = 'B28'.
  ELSEIF p_ra2 = 'X'.
    l_nation  = 'B06'.
  ELSEIF p_ra3 = 'X'.
    l_nation  = 'B20'.
  ENDIF.

  SELECT SINGLE * INTO wa_ale_dest
  FROM ztsd_ale_dest
  WHERE wo_nation = l_nation.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'No Definition for Nation:' l_nation.
  ENDIF.

ENDFORM.                    " GET_NATION
