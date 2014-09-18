*&---------------------------------------------------------------------*
*&  Include           ZSDF00200TF01
*&---------------------------------------------------------------------*

FORM modify_data USING rc
                       us_screen.

  PERFORM make_head.
  PERFORM make_item.
  PERFORM call_sm USING rc.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEAD
*&---------------------------------------------------------------------*
FORM make_head.
  REFRESH : it_dn_h.
  CLEAR   : it_dn_h.

  SELECT SINGLE *
         FROM likp
        WHERE vbeln EQ nast-objky.

* 1
  SELECT SINGLE * FROM lips WHERE vbeln = likp-vbeln.
  it_dn_h-z011 = lips-werks.

  SELECT SINGLE * FROM t001w WHERE werks EQ lips-werks.
  it_dn_h-z012 = t001w-name1. " Change supplier to ship to party
*  IT_DN_H-Z012 = 'KMS'.
  it_dn_h-z013 = likp-ablad.

* 2
  it_dn_h-z021 = 'AID0'.
  it_dn_h-z022 = 'Hyundai Motor Manufacturing AL'.
  it_dn_h-z023 = 'Alabama-America'.

* 3

  CONCATENATE 'AIDO' 'V' sy-datum(6) likp-vbeln+5(5)
              INTO it_dn_h-z031.

* WRITE LIKP-LFDAT TO IT_DN_H-Z032 DD/MM/YYYY.
  CONCATENATE likp-lfdat+4(2) likp-lfdat+6(2) likp-lfdat+0(4)
              INTO it_dn_h-z032 SEPARATED BY '/'.
  WRITE likp-lfuhr+0(4) TO it_dn_h-z033 USING EDIT MASK '__:__'.

  it_dn_h-z024 = likp-lifex.

  it_dn_h-z025 = likp-vbeln.


ENDFORM.                    " MAKE_HEAD
*&---------------------------------------------------------------------*
*&      Form  MAKE_ITEM
*&---------------------------------------------------------------------*
FORM make_item.

  DATA $volum TYPE p DECIMALS 2.

  REFRESH : it_dn_i, it_list.
  CLEAR   : it_dn_i, it_list.

  SELECT *
         FROM lips
        WHERE vbeln EQ nast-objky.

    it_list-posnr = lips-posnr.
    it_list-pstyv = lips-pstyv.
    it_list-kdmat = lips-kdmat.
    it_list-arktx = lips-arktx.
    it_list-lfimg = lips-lfimg.
    it_list-vrkme = '6'. "LIPS-VRKME.
    it_list-matnr = lips-matnr.
    $volum = lips-lfimg / 6.

    CALL FUNCTION 'ROUND'
         EXPORTING
              decimals     = 0
              input        = $volum
              sign         = '+'
         IMPORTING
              output       = $volum
         EXCEPTIONS
              input_invald = 01
              overflow     = 02
              type_invalid = 03.
    it_list-volum = $volum.
    APPEND it_list. CLEAR it_list.
  ENDSELECT.

  SORT : it_list.

  LOOP AT it_list.
    PERFORM move_line.
  ENDLOOP.
ENDFORM.                    " MAKE_ITEM
*&---------------------------------------------------------------------*
*&      Form  MOVE_LINE
*&---------------------------------------------------------------------*
FORM move_line.
  IF it_list-pstyv = 'ZTAL'.
    it_dn_i-kdmat = it_list-kdmat.
  ELSE.
    CONCATENATE it_list-kdmat+0(5) '-' it_list-kdmat+5(5)
                INTO it_dn_i-kdmat SEPARATED BY space.
  ENDIF.
  it_dn_i-text  = it_list-arktx.

  WRITE it_list-lfimg TO it_dn_i-lfimg UNIT it_list-vrkme.
  it_dn_i-vrkme = it_list-vrkme.

  it_dn_i-matnr = it_list-matnr+0(10).

  it_dn_i-volum = it_list-volum.

  APPEND it_dn_i. CLEAR it_dn_i.
ENDFORM.                    " MOVE_LINE
*&---------------------------------------------------------------------*
*&      Form  CALL_SM
*&---------------------------------------------------------------------*
FORM call_sm USING rc.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            formname           = 'ZSDF00202T'
       IMPORTING
            fm_name            = func_mod_name
       EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.
  rc = sy-subrc.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


* CONTROL_PARAMETERS-GETOTF = 'X'.
* CONTROL_PARAMETERS-NO_DIALOG = 'X'.

  output_options-tdnewid = 'X'.
* OUTPUT_OPTIONS-TDNOPREV = 'X'.
* OUTPUT_OPTIONS-TDIMMED = 'X'.
* OUTPUT_OPTIONS-TDDELETE = 'X'.

  CLEAR job_output_info.

  CALL FUNCTION func_mod_name
    EXPORTING
      control_parameters = control_parameters
      output_options     = output_options
*      USER_SETTINGS      = ''
      it_dn_h           = it_dn_h
    IMPORTING
      job_output_info    = job_output_info
    TABLES
      it_dn_i           = it_dn_i
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  rc = sy-subrc.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  control_parameters-getotf = 'X'.
* CONTROL_PARAMETERS-NO_DIALOG = 'X'.

  output_options-tdnewid = 'X'.
* OUTPUT_OPTIONS-TDNOPREV = 'X'.
* OUTPUT_OPTIONS-TDIMMED = 'X'.
* OUTPUT_OPTIONS-TDDELETE = 'X'.

  CLEAR job_output_info.

  CALL FUNCTION func_mod_name
    EXPORTING
      control_parameters = control_parameters
      output_options     = output_options
*      USER_SETTINGS      = ''
      it_dn_h           = it_dn_h
    IMPORTING
      job_output_info    = job_output_info
    TABLES
      it_dn_i           = it_dn_i
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  rc = sy-subrc.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

*
*  DATA: LV_INDEX TYPE I,
*        LV_FLE1 TYPE I,
*        LV_FLE2 TYPE I,
*        LV_LINE TYPE I,
*        LV_OFF1 TYPE I,
*        LV_HFLD(1000).
*
*  DATA: LT_PDF LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE.
*
*  V_OTF[] = JOB_OUTPUT_INFO-OTFDATA[].
*  CLEAR: V_PDF, V_PDF[].
*  CALL FUNCTION 'CONVERT_OTF'
*    EXPORTING
*      FORMAT       = 'PDF'
*    IMPORTING
*      BIN_FILESIZE = V_SIZE
*    TABLES
*      OTF          = V_OTF
*      LINES        = V_PDF
*    EXCEPTIONS
*      OTHERS       = 1.
*  IF SY-SUBRC = 0.
*    IT_PDF[] = V_PDF[].
*  ENDIF.
*
*  CLEAR: OBJPACK, EMAIL_SEND.
*  REFRESH: OBJPACK, EMAIL_SEND.
*  CLEAR: IT_TEXT.
*  REFRESH: IT_TEXT.
*  IT_TEXT-LINE =
*    'It is a delivery note that contains the loading infomation'.
*  APPEND IT_TEXT.
*  IT_TEXT-LINE =
*    'regarding the volumn and quanties of MIP parts that are sold.'.
*  APPEND IT_TEXT.
*  CLEAR: IT_TEXT.
*
*  EMAIL_DATA-OBJ_NAME = 'MESSAGE'.
**   EMAIL_DATA-OBJ_DESCR = 'Delivery Note'.
*  EMAIL_DATA-OBJ_DESCR = 'Delivery Note'. "mail title.
*  EMAIL_DATA-OBJ_LANGU = SY-LANGU.
*  EMAIL_DATA-SENSITIVTY = 'P'.
*
*  EMAIL_DATA-OBJ_PRIO =  '1'.
*  EMAIL_DATA-NO_CHANGE = 'X'.
*  EMAIL_DATA-PRIORITY = '1'.
*
**   TRANSLATE P_MAILID TO LOWER CASE.
*  EMAIL_SEND-REC_TYPE = 'U'.
*  EMAIL_SEND-EXPRESS = 'X'.
*  EMAIL_SEND-RECEIVER = 'patrik.placek@mobis.co.kr'.
*
*  APPEND EMAIL_SEND.
*
*  EMAIL_SEND-RECEIVER = 'kiaphy@kia.co.kr'.
*
*  APPEND EMAIL_SEND.
*
*  DESCRIBE TABLE IT_TEXT LINES LV_INDEX.
*  CLEAR: OBJPACK.
*  OBJPACK-HEAD_START = 1.
*  OBJPACK-HEAD_NUM = 0.
*  OBJPACK-BODY_START = 1.
*  OBJPACK-BODY_NUM = LV_INDEX.
*  OBJPACK-DOC_TYPE = 'RAW'.
*  APPEND OBJPACK.
*
*  CLEAR: OBJPACK.
*
*  DESCRIBE TABLE IT_PDF LINES LV_LINE.
*  DESCRIBE FIELD IT_PDF LENGTH LV_FLE1 IN CHARACTER MODE.
*  DESCRIBE FIELD LT_PDF LENGTH LV_FLE2 IN CHARACTER MODE.
*  CLEAR: LT_PDF. REFRESH: LT_PDF.
*  LOOP AT IT_PDF.
*    LV_INDEX = SY-TABIX.
*    MOVE IT_PDF TO LV_HFLD+LV_OFF1.
*    IF LV_INDEX = LV_LINE.
*      LV_FLE1 = STRLEN( IT_PDF ).
*    ENDIF.
*    LV_OFF1 = LV_OFF1 + LV_FLE1.
*    IF LV_OFF1 GE LV_FLE2.
*      CLEAR LT_PDF.
*      LT_PDF = LV_HFLD(LV_FLE2).
*      APPEND LT_PDF.
*      SHIFT LV_HFLD BY LV_FLE2 PLACES.
*      LV_OFF1 = LV_OFF1 - LV_FLE2.
*    ENDIF.
*    IF LV_INDEX = LV_LINE.
*      IF LV_OFF1 GT 0.
*        CLEAR LT_PDF.
*        LT_PDF = LV_HFLD(LV_OFF1).
*        APPEND LT_PDF.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE LT_PDF LINES LV_INDEX.
*
*  OBJPACK-TRANSF_BIN = 'X'.
*  OBJPACK-HEAD_START = 1.
*  OBJPACK-HEAD_NUM = 0.
*  OBJPACK-BODY_START = 1.
*  OBJPACK-BODY_NUM = LV_INDEX.
*  OBJPACK-DOC_TYPE = 'PDF'.
*  OBJPACK-OBJ_NAME = 'Delivery Note'.
*  OBJPACK-OBJ_DESCR = NAST-OBJKY. "att.pdf
*  APPEND OBJPACK.
*
*  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*    EXPORTING
*      DOCUMENT_DATA              = EMAIL_DATA
*      PUT_IN_OUTBOX              = 'X'
*      COMMIT_WORK                = 'X'
*    TABLES
*      PACKING_LIST               = OBJPACK
*      CONTENTS_BIN               = LT_PDF
*      CONTENTS_TXT               = IT_TEXT
*      RECEIVERS                  = EMAIL_SEND
*    EXCEPTIONS
*      TOO_MANY_RECEIVERS         = 1
*      DOCUMENT_NOT_SENT          = 2
*      DOCUMENT_TYPE_NOT_EXIST    = 3
*      OPERATION_NO_AUTHORIZATION = 4
*      PARAMETER_ERROR            = 5
*      X_ERROR                    = 6
*      ENQUEUE_ERROR              = 7
*      OTHERS                     = 8.
*  IF SY-SUBRC = 0.
*  ELSE.
*  ENDIF.
ENDFORM.                    " CALL_SM
