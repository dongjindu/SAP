
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
FUNCTION z_fpp_hma_zsoder .
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
*"----------------------------------------------------------------------



  DATA : l_idoc_contrl LIKE idoc_contrl ,
         l_idoc_data   LIKE idoc_data
.
  DATA : lt_so LIKE TABLE OF ztpp_so_idoc WITH HEADER LINE .
  DATA : lt_zsoseg LIKE TABLE OF zsoseg WITH HEADER LINE  .
  DATA : return LIKE TABLE OF bapireturn WITH HEADER LINE.
*  DATA : lv_model  LIKE ztpp_vm-model_code.
  DATA : lv_model(3).


  PERFORM inbound_initialize CHANGING workflow_result.

  LOOP AT idoc_data INTO l_idoc_data
                              WHERE docnum EQ idoc_contrl-docnum.

    CASE l_idoc_data-segnam.
      WHEN 'ZSOSEG'. " Header

        lt_zsoseg = l_idoc_data-sdata.


        IF lt_zsoseg-destn+0(3)  <> 'B28'.   "Victor:Color conversion 3-> 2
          lv_model  =  lt_zsoseg-mdinx+0(2).

          CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
            EXPORTING
              i_model = lv_model
              i_year  = lt_zsoseg-mdyr
              i_gubn  = 'X'            "HAC -> HMMA
              i_extc  = lt_zsoseg-wkexc
              i_intc  = lt_zsoseg-wkinc
            IMPORTING
              e_extc  = lt_zsoseg-wkexc
              e_intc  = lt_zsoseg-wkinc.
        ENDIF.

        MOVE-CORRESPONDING lt_zsoseg TO  lt_so .

        lt_so-nation = lt_zsoseg-natn.
        lt_so-vin    = lt_zsoseg-zvin.
        lt_so-audat  = sy-datum.
        lt_so-auzet  = sy-uzeit.
        lt_so-aunam  = sy-uname.

        APPEND lt_so .
    ENDCASE.

  ENDLOOP.

  MODIFY ztpp_so_idoc FROM TABLE lt_so .
*  COMMIT WORK.

** Changed by Furong on 05/31/11
*  PERFORM SET_UM_DEALER TABLES LT_SO.
  PERFORM set_um_dealer_new TABLES lt_so.
** End on 10/18/11

  IF sy-subrc EQ 0 .
    idoc_status-docnum = idoc_contrl-docnum.
    idoc_status-status = '53'.
    idoc_status-msgty = 'S'.
    idoc_status-msgid = 'YM'.
    idoc_status-msgno = '005'.
    idoc_status-msgv1 = 'Success' . "Message Added
    APPEND idoc_status.
    CLEAR idoc_status.

    workflow_result = c_wf_result_error.
    return_variables-doc_number = idoc_contrl-docnum.
    APPEND return_variables.
    CLEAR return_variables.
  ELSE.
    idoc_status-docnum = idoc_contrl-docnum.
    idoc_status-status = '51'.
    idoc_status-msgty = 'E'.
    idoc_status-msgid = 'YM'.
    idoc_status-msgno = '005'.
    "Message Added
    APPEND idoc_status.
    CLEAR idoc_status.

    workflow_result = c_wf_result_error.
    return_variables-wf_param = 'Error_Idocs : Order is initial'.
    return_variables-doc_number = idoc_contrl-docnum.
    APPEND return_variables.
    CLEAR return_variables.
  ENDIF.

** Added by furong on 05/31/11
  PERFORM send_dealer_to_alc.
** End
ENDFUNCTION.

*
*  DATA : L_IDOC_CONTRL LIKE IDOC_CONTRL ,
*         L_IDOC_DATA   LIKE IDOC_DATA
*.
*  DATA : LT_SO LIKE TABLE OF ZTPP_SO_IDOC WITH HEADER LINE .
*
*  DATA : LT_ZSOSEG LIKE TABLE OF ZSOSEG WITH HEADER LINE  .
*
*
*  DATA : RETURN LIKE TABLE OF BAPIRETURN WITH HEADER LINE.
*  PERFORM INBOUND_INITIALIZE CHANGING WORKFLOW_RESULT.
*
*  LOOP AT IDOC_DATA INTO L_IDOC_DATA
*    WHERE DOCNUM EQ IDOC_CONTRL-DOCNUM.
*
*    CASE L_IDOC_DATA-SEGNAM.
*      WHEN 'ZSOSEG'. " Header
*
*        LT_ZSOSEG = L_IDOC_DATA-SDATA.
*        MOVE-CORRESPONDING LT_ZSOSEG TO  LT_SO .
*
*        LT_SO-VIN = LT_ZSOSEG-ZVIN.
*        LT_SO-AUDAT  = SY-DATUM.
*        LT_SO-AUZET  = SY-UZEIT.
*        LT_SO-AUNAM  = SY-UNAME.
**        L_PODOC-MANDT = SY-MANDT.
**        L_PODOC-DOCNUM = L_IDOC_DATA-DOCNUM.
**        L_PODOC-WO_SER = LT_ZPOSEG1-PRDOD.
**        L_PODOC-NATN   = LT_ZPOSEG1-NATN.
**        L_PODOC-DIST   = LT_ZPOSEG1-DIST.
**        L_PODOC-WKEXC  = LT_ZPOSEG1-WKEXC.
**        L_PODOC-WKINC  = LT_ZPOSEG1-WKINC.
*
**
**        IF NOT LT_ZPOSEG1-PRDOD IS INITIAL.
**          INSERT INTO ZTPP_PO_IDOC VALUES  L_PODOC .
**
**          APPEND LT_ZPOSEG1.
**        ELSE .
**          PROD = 'X'.
**        ENDIF.
**        "LT_KSBO
*
*        APPEND LT_SO .
*    ENDCASE.
*  ENDLOOP.
*  MODIFY ZTPP_SO_IDOC FROM TABLE LT_SO .
*  "ACCEPTING DUPLICATE KEYS.
*
*  PERFORM SET_UM_DEALER.
*
*  IF SY-SUBRC EQ 0 .
*    IDOC_STATUS-DOCNUM = IDOC_CONTRL-DOCNUM.
*    IDOC_STATUS-STATUS = '53'.
*    IDOC_STATUS-MSGTY = 'S'.
*    IDOC_STATUS-MSGID = 'YM'.
*    IDOC_STATUS-MSGNO = '005'.
*    IDOC_STATUS-MSGV1 = 'Success' . "Message Added
*    APPEND IDOC_STATUS.
*    CLEAR IDOC_STATUS.
*
*    WORKFLOW_RESULT = C_WF_RESULT_ERROR.
*    RETURN_VARIABLES-DOC_NUMBER = IDOC_CONTRL-DOCNUM.
*    APPEND RETURN_VARIABLES.
*    CLEAR RETURN_VARIABLES.
*  ELSE.
*    IDOC_STATUS-DOCNUM = IDOC_CONTRL-DOCNUM.
*    IDOC_STATUS-STATUS = '51'.
*    IDOC_STATUS-MSGTY = 'E'.
*    IDOC_STATUS-MSGID = 'YM'.
*    IDOC_STATUS-MSGNO = '005'.
*    IDOC_STATUS-MSGV1 = 'Error_Idocs : Order is initial' .
*    "Message Added
*    APPEND IDOC_STATUS.
*    CLEAR IDOC_STATUS.
*
*    WORKFLOW_RESULT = C_WF_RESULT_ERROR.
*    RETURN_VARIABLES-WF_PARAM = 'Error_Idocs : Order is initial'.
*    RETURN_VARIABLES-DOC_NUMBER = IDOC_CONTRL-DOCNUM.
*    APPEND RETURN_VARIABLES.
*    CLEAR RETURN_VARIABLES.
*  ENDIF.
*
*
*
*ENDFUNCTION.
