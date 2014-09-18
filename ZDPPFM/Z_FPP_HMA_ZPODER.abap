*&---------------------------------------------------------------------*
*& Program:                                                            *
*& Type   :  Fuction                                                   *
*& Author :                                                            *
*& Title  :  [PP] HMA->HMMA Production Order (D-Order) Inbound         *
*&---------------------------------------------------------------------*
*   Requested by:        Daniel Kim                                    *
*&---------------------------------------------------------------------*
*MODIFICATION LOG
************************************************************************
*DATE      Developer      RequestNo.      Description
*29/10/10  Sujin Lee      UD1K949859      Init.
************************************************************************

FUNCTION z_fpp_hma_zpoder .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT_METHOD) TYPE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) TYPE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) TYPE  BDWF_PARAM-RESULT
*"     VALUE(APPLICATION_VARIABLE) TYPE  BDWF_PARAM-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) TYPE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) TYPE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"----------------------------------------------------------------------
* 02.25.2014 Victor Modified for HMM
*"----------------------------------------------------------------------

  DATA : l_idoc_contrl LIKE idoc_contrl ,
         l_idoc_data   LIKE idoc_data
.
  DATA : l_podoc LIKE ztpp_po_idoc .
  DATA : lv_item_cnt TYPE i .
  DATA : lt_zposeg1 LIKE TABLE OF zposeg1 WITH HEADER LINE ,
         lt_zposeg2 LIKE TABLE OF zposeg2 WITH HEADER LINE .

  DATA : l_new(1), l_new_t(1), new, prod, mqty.
  DATA : lv_model(3).
  DATA : return LIKE TABLE OF bapireturn WITH HEADER LINE.

  CLEAR : new, prod, lv_item_cnt.

  PERFORM inbound_initialize CHANGING workflow_result.

  DATA :  lt_idoc LIKE TABLE OF ztpp_po_idoc WITH HEADER LINE.

  LOOP AT idoc_data INTO l_idoc_data
                   WHERE docnum EQ idoc_contrl-docnum.

    CASE l_idoc_data-segnam.
      WHEN 'ZPOSEG1'. " Header
        lt_zposeg1 = l_idoc_data-sdata.

        IF lt_zposeg1-natn  <> 'B28'.  "Victor
          lv_model  =  lt_zposeg1-mdinx+0(2).

          CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
            EXPORTING
              i_model = lv_model
              i_year  = lt_zposeg1-mdyr
              i_gubn  = 'X'            "HAC -> HMMA
              i_extc  = lt_zposeg1-wkexc
              i_intc  = lt_zposeg1-wkinc
            IMPORTING
              e_extc  = lt_zposeg1-wkexc
              e_intc  = lt_zposeg1-wkinc.
        ENDIF.

        l_podoc-mandt = sy-mandt.
        l_podoc-docnum = l_idoc_data-docnum.

        l_podoc-wo_ser = lt_zposeg1-prdod.
        l_podoc-natn   = lt_zposeg1-natn.
        l_podoc-dist   = lt_zposeg1-dist.
        l_podoc-wkexc  = lt_zposeg1-wkexc.
        l_podoc-wkinc  = lt_zposeg1-wkinc.
        l_podoc-crdat  = sy-datum.
        l_podoc-crtim  = sy-uzeit.
        l_podoc-uname  = sy-uname.

        IF NOT lt_zposeg1-prdod IS INITIAL.
          APPEND lt_zposeg1.
        ELSE .
          prod = 'X'.
        ENDIF.
        "LT_KSBO
      WHEN 'ZPOSEG2'. " Item
        lt_zposeg2 = l_idoc_data-sdata.

        IF lt_zposeg2-natn  <> 'B28'.  "Victor
          lv_model  =  lt_zposeg2-mdinx+0(2).

          CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
            EXPORTING
              i_model = lv_model
              i_year  = lt_zposeg2-mdyr
              i_gubn  = 'X'            "HAC -> HMMA
              i_extc  = lt_zposeg2-wkexc
              i_intc  = lt_zposeg2-wkinc
            IMPORTING
              e_extc  = lt_zposeg2-wkexc
              e_intc  = lt_zposeg2-wkinc.
        ENDIF.

        APPEND lt_zposeg2.
    ENDCASE.

    CLEAR : l_new.
    AT FIRST .
      PERFORM check_wosum USING lt_zposeg1 l_new .
      l_new_t = l_new.
    ENDAT.

    PERFORM check_wosum USING lt_zposeg1 l_new .
    l_podoc-type = l_new.

    IF l_new NE l_new_t.
      new = 'X'.
    ELSE.
      APPEND l_podoc TO lt_idoc.
    ENDIF.
    l_new_t = l_new.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_zposeg1
    COMPARING prdod natn dist wkexc wkinc destn.

  LOOP AT lt_zposeg1 .
    CLEAR : lv_item_cnt.
    LOOP AT lt_zposeg2 WHERE prdod = lt_zposeg1-prdod
                         AND natn   = lt_zposeg1-natn
                         AND dist   = lt_zposeg1-dist
                         AND wkexc  = lt_zposeg1-wkexc
                         AND wkinc  = lt_zposeg1-wkinc
                         AND destn  = lt_zposeg1-destn.

      lv_item_cnt = lt_zposeg2-moqty + lv_item_cnt .

    ENDLOOP.
    IF lt_zposeg1-moqty NE lv_item_cnt .
      mqty = 'X'.
    ELSE.
      CLEAR : mqty.
    ENDIF.
  ENDLOOP.

  SORT lt_idoc BY docnum wo_ser natn dist wkexc wkinc.
  DELETE ADJACENT DUPLICATES FROM lt_idoc.

  IF new EQ 'X'.
    idoc_status-docnum = idoc_contrl-docnum.
    idoc_status-status = '51'.
    idoc_status-msgty = 'E'.
    idoc_status-msgid = 'YM'.
    idoc_status-msgno = '005'.
    idoc_status-msgv1 = 'Data is not uniform' . "Message Added
    APPEND idoc_status.
    CLEAR idoc_status.

    workflow_result = c_wf_result_error.
    return_variables-wf_param = 'Error_Idocs : '.
    return_variables-doc_number = idoc_contrl-docnum.
    APPEND return_variables.
    CLEAR return_variables.
  ELSEIF prod EQ 'X'.
    idoc_status-docnum = idoc_contrl-docnum.
    idoc_status-status = '51'.
    idoc_status-msgty = 'E'.
    idoc_status-msgid = 'YM'.
    idoc_status-msgno = '005'.
    idoc_status-msgv1 = 'Error_Idocs : Order is initial' .
    "Message Added
    APPEND idoc_status.
    CLEAR idoc_status.

    workflow_result = c_wf_result_error.
    return_variables-wf_param = 'Error_Idocs : Order is initial'.
    return_variables-doc_number = idoc_contrl-docnum.
    APPEND return_variables.
    CLEAR return_variables.
  ELSEIF mqty EQ 'X'.
    idoc_status-docnum = idoc_contrl-docnum.
    idoc_status-status = '51'.
    idoc_status-msgty = 'E'.
    idoc_status-msgid = 'YM'.
    idoc_status-msgno = '005'.
    idoc_status-msgv1 =
      'Error_Idocs : Item Quantity is different than Header Quantity.' .
    "Message Added
    APPEND idoc_status.
    CLEAR idoc_status.

    workflow_result = c_wf_result_error.
    return_variables-wf_param = 'Error_Idocs : Order is initial'.
    return_variables-doc_number = idoc_contrl-docnum.
    APPEND return_variables.
    CLEAR return_variables.
  ELSE.

    idoc_status-docnum = idoc_contrl-docnum.
    idoc_status-status = '53'.
    idoc_status-msgty = 'I'.
    idoc_status-msgid = 'YM'.
    idoc_status-msgno = '004'.
    idoc_status-msgv1 =''
    .
    APPEND idoc_status.
    CLEAR idoc_status.


    INSERT ztpp_po_idoc FROM TABLE lt_idoc
      ACCEPTING DUPLICATE KEYS  .
  ENDIF.

*  CALL FUNCTION 'Z_FPP_HMA_IF_PO'
*       EXPORTING
*            NEW    = L_NEW
*            IDOCNUM = IDOC_CONTRL-DOCNUM
*       TABLES
*            RETURN = RETURN
*            HEADER = LT_ZPOSEG1.


ENDFUNCTION.
