*&---------------------------------------------------------------------*
*& Report  ZTEST_SD_DEL                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  ztest_sd_update NO STANDARD PAGE HEADING
                     LINE-SIZE  1023
                     LINE-COUNT 65
                     MESSAGE-ID zmsd.


INCLUDE ztest_sd_up_top.
*INCLUDE ztest_sd_del_top.
INCLUDE ztest_sd_up_f01.
*INCLUDE ztest_sd_del_f01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

START-OF-SELECTION.

  REFRESH it_excl. CLEAR it_excl.
  PERFORM upload_process.
  PERFORM bdc_process.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  REFRESH: it_bdc, it_mess.
  CLEAR:   it_bdc, it_mess.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
    IF p_rdo1 EQ 'X'.
      CASE screen-name.
        WHEN 'P_FILETY' OR 'P_TCODE' OR 'P_FILE' OR
        '%B002004_BLOCK_1000' OR '%_P_FILE_%_APP_%-TEXT' OR
        '%_P_FILETY_%_APP_%-TEXT' OR '%_P_TCODE_%_APP_%-TEXT'.
          screen-active = 0.
      ENDCASE.
*    ELSEIF SCREEN-NAME  EQ 'P_RDO2'.  "EXCEL DATA
    ELSEIF p_rdo2 EQ 'X'.  "EXCEL DATA
      CASE screen-name.
        WHEN 'P_FILETY' OR 'P_TCODE'.
          screen-input = 0.
      ENDCASE.
    ENDIF.
    MODIFY SCREEN.
    CLEAR screen.
  ENDLOOP.

ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0035   text
*----------------------------------------------------------------------*
FORM at_sel_screen_on_value_request  USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

* Build Filter for Fileselektor

  tmp_mask = ',*.*,*.*.'.
  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       IMPORTING
            filename         = tmp_filename
*         RC               =
       EXCEPTIONS
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_process.
  DATA : it_temp LIKE stpo,
         cov(8),
         w_int TYPE i,num TYPE i,
         z_matnr(18).

  DESCRIBE TABLE it_excl LINES w_int.
  IF w_int <> 0.
    IF p_a1 = 'X'.
*      LOOP AT it_excl.
*        REFRESH : it_bdc.
*        UPDATE ztpp_wosum SET sales = ' '
*                   WHERE wo_ser = it_excl-wo_ser
*                     AND nation = it_excl-nation
*                     AND dealer = it_excl-dealer
*                     AND extc   = it_excl-extc
*                     AND intc   = it_excl-intc.
*        IF it_excl-vbeln <> space.
*          PERFORM bdc_fill USING :
*                 'X' 'SAPMV45A'             '0102',
*                 ' ' 'BDC_CURSOR'           vbak-vbeln,
*                 ' ' 'BDC_OKCODE'           '/00',
*                 ' ' 'VBAK-VBELN'           it_excl-vbeln,
*
*                 'X' 'SAPMV45A'             '4001',
*                 ' ' 'BDC_OKCODE'          '/ELOES'.
*
*          CALL TRANSACTION 'VA02' USING it_bdc MODE p_mode
*                                        UPDATE 'S'.
*        ENDIF.
*      ENDLOOP.
    ELSEIF p_a2 = 'X'.
      CLEAR : num.
      num = 1.
      LOOP AT it_excl.
        CLEAR : z_matnr,val_table[],val_table.
*        CONCATENATE it_excl-wo_ser it_excl-nation it_excl-dealer
*          it_excl-extc it_excl-intc INTO z_matnr.
        MOVE it_excl-bstkd TO z_matnr.
        PERFORM get_data_value USING it_excl.

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = z_matnr
           mode               = 'W'
           ctype              = '001'
*           DISPLAY            = 'D'
          TABLES
            val_table          = val_table
*         EXCEPTIONS
*           NO_DATA            = 1
*           ERROR_MODE         = 2
*           ERROR_OBJECT       = 3
*           OTHERS             = 4
                  .
*        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ELSE.
        num = num + 1.
*        ENDIF.
      ENDLOOP.
      WRITE : w_int, num.
    ENDIF.
  ENDIF.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0230   text
*      -->P_0231   text
*      -->P_0232   text
*----------------------------------------------------------------------*
FORM bdc_fill USING   p1 p2 p3.
  CLEAR it_bdc.
  IF p1 = 'X'.
    it_bdc-dynbegin = p1.
    it_bdc-program  = p2.
    it_bdc-dynpro   = p3.
  ELSE.
    it_bdc-dynbegin = p1.
    it_bdc-fnam     = p2.
    it_bdc-fval     = p3.
  ENDIF.
  APPEND it_bdc.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCL  text
*      -->P_TBALES  text
*      -->P_VAL_TABLE  text
*----------------------------------------------------------------------*
FORM get_data_value USING    pt_excl.

  SELECT SINGLE atinn INTO  val_table-atinn
   FROM cabn
    WHERE atnam EQ 'P_SALES_ORDER'.

  val_table-atnam = 'P_WOCL_001'.
  val_table-atwrt = it_excl-vbeln.
  APPEND val_table.

ENDFORM.                    " GET_DATA_VALUE
