************************************************************************
* Program Name      : ZSD_SOFF_REPROCESS
* Author            :
* Creation Date     : 09/25/212
* Specifications By :
* Description       : re-process outboud delivery
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 08.31.2014  Victor      modified fucntion:Z_FSD_VEHICLE_DELIVERY
************************************************************************
REPORT zsd_soff_reprocess NO STANDARD PAGE HEADING
                                 LINE-SIZE 200
                                 MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztppvr, likp, nast.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF it_ztppvr OCCURS 0.
        INCLUDE STRUCTURE ztppvr.
DATA: vbeln LIKE likp-vbeln,
      END OF it_ztppvr.

DATA: it_del LIKE TABLE OF it_ztppvr WITH HEADER LINE,
      it_output LIKE TABLE OF it_ztppvr WITH HEADER LINE,
      it_log LIKE TABLE OF ztsd_soff_rlog WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA: messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: ctumode LIKE ctu_params-dismode VALUE 'N'.
*                                      "A: show all dynpros
*                                      "E: show dynpro on error only
*                                      "N: do not display dynpro,
DATA: cupdate LIKE ctu_params-updmode VALUE  'L'.
*                                      "S: synchronously
*                                      "A: asynchronously
*                                      "L: local.
DATA: w_rver LIKE somlreci1-receiver VALUE ' '.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_dest FOR ztppvr-p_dest_code,
                s_vbeln FOR likp-vbeln,
                s_date FOR ztppvr-zbdat OBLIGATORY,
                s_time FOR ztppvr-zbtim.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM save_send_email.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM list_process.

*&---------------------------------------------------------------------*
*&      Form  CALL_SD_FUNCTION
*&---------------------------------------------------------------------*
FORM create_delivery.
  DATA: l_vbeln LIKE likp-vbeln,
        msg(255).

  it_log-modl = it_del-p_model.
  it_log-body_ser = it_del-p_body_serial.
  it_log-rdate = sy-datum.
  it_log-err_ty = 'D'.
  it_log-vbeln = it_del-vbeln.
  it_log-zuser = sy-uname.
  it_log-cre_date = sy-datum.
  it_log-cre_time = sy-uzeit.

  CALL FUNCTION 'Z_FSD_VEHICLE_DELIVERY'
    EXPORTING
*     equno               = it_ztppvr-vbeln
      equno               = it_del-vbeln      "08.28.2014 Victor
    IMPORTING
      delivery_no         = l_vbeln
    EXCEPTIONS
      not_found_vin       = 1
      not_found_sales     = 2
      not_found_vstel     = 3
      not_create_delivery = 4
      not_update_delivery = 5
      OTHERS              = 6.
  IF sy-subrc = 0 AND l_vbeln <> ' '.
    it_log-bdc_flag = 'S'.
    it_log-bdc_msg = l_vbeln.
  ELSE.
    it_log-bdc_flag = 'E'.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = sy-msgid
        msgnr               = sy-msgno
        msgv1               = sy-msgv1
        msgv2               = sy-msgv2
        msgv3               = sy-msgv3
        msgv4               = sy-msgv4
      IMPORTING
        message_text_output = msg.
    it_log-bdc_msg = msg.
  ENDIF.
  APPEND it_log.
  CLEAR: it_log.
ENDFORM.                    " CALL_SD_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.

ENDFORM.                    " LIST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM at_selection-screen.

ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: w_soffseg LIKE zsoffseg,
        l_vbeln LIKE likp-vbeln,
        l_index LIKE sy-tabix.

  DATA: lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
        lt_edidd LIKE TABLE OF edidd WITH HEADER LINE.

  DATA: BEGIN OF lt_likp OCCURS 0,
        vbeln LIKE likp-vbeln,
        END OF lt_likp.

  RANGES: r_status FOR ztppvr-p_status,
          r_vin FOR ztppvr-p_vin..

  r_status-option = 'EQ'.
  r_status-sign = 'I'.
  r_status-low = 'T24'.
  APPEND r_status.
  r_status-low = 'T25'.
  APPEND r_status.
  r_status-low = 'T26'.
  APPEND r_status.

  r_vin-option = 'EQ'.
  r_vin-sign = 'I'.

** On 03/18/13 for performance tuning
  IF s_date[] IS NOT INITIAL.                             "Addition
    SELECT * INTO TABLE it_ztppvr
        FROM ztppvr
       WHERE p_status IN r_status
         AND p_dest_code IN s_dest
         AND zbdat IN s_date
         AND zbtim IN s_time
 %_HINTS ORACLE 'INDEX (ZTPPVR "ZTPPVR~Z02")'.
  ELSE.
** End on 03/18/13
    SELECT * INTO TABLE it_ztppvr
      FROM ztppvr
      WHERE p_status IN r_status
        AND p_dest_code IN s_dest
        AND zbdat IN s_date
        AND zbtim IN s_time.
  ENDIF.
  IF sy-subrc = 0.
    LOOP AT it_ztppvr.
      CONCATENATE it_ztppvr-p_model it_ztppvr-p_body_serial
             INTO it_ztppvr-vbeln.
      MODIFY it_ztppvr.
    ENDLOOP.

    IF s_vbeln[] IS INITIAL.
    ELSE.
      LOOP AT it_ztppvr.
        IF NOT it_ztppvr-vbeln IN s_vbeln.
          DELETE it_ztppvr.
        ENDIF.
      ENDLOOP.
    ENDIF.

** get IDOC
    SELECT docnum
     INTO CORRESPONDING FIELDS OF TABLE lt_edidc
     FROM edidc
     WHERE status = '03'
       AND mestyp EQ 'ZSIGNOFF_MST'
       AND credat IN s_date
       AND rcvprt EQ 'LS'.
    DELETE lt_edidc WHERE docnum IS INITIAL.

** get Vin number
    LOOP AT lt_edidc.
      REFRESH: lt_edidd.
      CALL FUNCTION 'IDOC_READ_COMPLETELY'
        EXPORTING
          document_number         = lt_edidc-docnum
        TABLES
          int_edidd               = lt_edidd
        EXCEPTIONS
          document_not_exist      = 1
          document_number_invalid = 2
          OTHERS                  = 3.
      IF sy-subrc = 0.
        LOOP AT lt_edidd WHERE segnam = 'ZSOFFSEG'.
          w_soffseg  = lt_edidd-sdata.
          r_vin-low = w_soffseg-vin.
          COLLECT r_vin.
          CLEAR: r_vin-low.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    IF NOT r_vin[] IS INITIAL.
      LOOP AT it_ztppvr.
        IF it_ztppvr-p_vin IN r_vin.
          DELETE it_ztppvr.
        ENDIF.
      ENDLOOP.
    ENDIF.

** CHECK DELIVERY MISSING
    CHECK NOT it_ztppvr[] IS INITIAL.
    SELECT vbeln INTO TABLE lt_likp
      FROM likp
      FOR ALL ENTRIES IN it_ztppvr
      WHERE vbeln = it_ztppvr-vbeln.

    LOOP AT it_ztppvr.
      l_index = sy-tabix.
      READ TABLE lt_likp WITH KEY vbeln = it_ztppvr-vbeln.
      IF sy-subrc = 0.
      ELSE.
        it_del = it_ztppvr.
        APPEND it_del.
        CLEAR: it_del.
        DELETE it_ztppvr INDEX l_index.
      ENDIF.
    ENDLOOP.

** CHECK MISSUNG OUTPUT
    LOOP AT it_ztppvr.
      l_index = sy-tabix.
      SELECT SINGLE *
        FROM nast
        WHERE objky = it_ztppvr-vbeln
          AND kschl = 'ZV01'
          AND vstat = '1'.
      IF sy-subrc = 0.
      ELSE.
        it_output = it_ztppvr.
        APPEND it_output.
        CLEAR: it_output.
        DELETE it_ztppvr INDEX l_index.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  REFRESH: it_log.
  LOOP AT it_del.
    PERFORM create_delivery.
  ENDLOOP.

  LOOP AT it_output.
    PERFORM create_output.
  ENDLOOP.

  LOOP AT it_ztppvr.
    PERFORM save_no_ale.
  ENDLOOP.
ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_output.

  it_log-modl = it_output-p_model.
  it_log-body_ser = it_output-p_body_serial.
  it_log-rdate = sy-datum.
  it_log-err_ty = 'O'.
  it_log-vbeln = it_output-vbeln.
  it_log-zuser = sy-uname.
  it_log-cre_date = sy-datum.
  it_log-cre_time = sy-uzeit.

  PERFORM bdc_dynpro      USING 'SAPMV50A' '4004'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LIKP-VBELN'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'LIKP-VBELN'
                                it_output-vbeln.
  PERFORM bdc_dynpro      USING 'SAPMV50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=KPRT_T'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LIPS-MATNR(02)'.

  PERFORM bdc_dynpro      USING 'SAPDV70A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DNAST-KSCHL(08)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'DNAST-KSCHL(08)'
                                'ZV01'.
  PERFORM bdc_dynpro      USING 'SAPDV70A' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DNAST-KSCHL(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=V70S'.
  PERFORM bdc_transaction USING 'VL02N' it_log-bdc_flag
                                  it_log-bdc_msg.
  APPEND it_log.
  CLEAR it_log.

ENDFORM.                    " CREATE_OUTPUT

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  SAVE_NO_ALE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_no_ale .
  it_log-modl = it_ztppvr-p_model.
  it_log-body_ser = it_ztppvr-p_body_serial.
  it_log-rdate = sy-datum.
  it_log-err_ty = 'A'.
  it_log-vbeln = it_ztppvr-vbeln.
  it_log-zuser = sy-uname.
  it_log-cre_date = sy-datum.
  it_log-cre_time = sy-uzeit.
  APPEND it_log.
  CLEAR it_log.
ENDFORM.                    " SAVE_NO_ALE
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0598   text
*----------------------------------------------------------------------*
FORM bdc_transaction USING p_tcode p_flag p_msg.
  DATA: l_subrc LIKE sy-subrc,
        msg(255).

  REFRESH: messtab.

  CALL TRANSACTION p_tcode USING bdcdata
                   MODE   ctumode
                   UPDATE cupdate
                   MESSAGES INTO messtab.
  l_subrc = sy-subrc.

  READ TABLE messtab WITH KEY msgtyp = 'E'.

  IF sy-subrc = 0.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = sy-msgid
        msgnr               = sy-msgno
        msgv1               = sy-msgv1
        msgv2               = sy-msgv2
        msgv3               = sy-msgv3
        msgv4               = sy-msgv4
      IMPORTING
        message_text_output = msg.

    REFRESH bdcdata.
    p_flag = 'E'.
    p_msg = msg.

  ELSE.
    READ TABLE messtab WITH KEY msgtyp = 'A'.
    IF sy-subrc = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = msg.

      REFRESH bdcdata.
      p_flag = 'E'.
      p_msg = msg.
    ELSE.
      p_flag = 'S'.
      p_msg =  'Successfully updated'.
    ENDIF.
  ENDIF.
  REFRESH: messtab.

ENDFORM.                    " BDC_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_send_email.
  CHECK NOT it_log[] IS INITIAL.
  MODIFY ztsd_soff_rlog FROM TABLE it_log.
*  INSERT ztsd_soff_rlog FROM TABLE it_log
*   ACCEPTING DUPLICATE KEYS.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    MESSAGE i001 WITH 'Log table update error'.
    ROLLBACK WORK.
  ENDIF.
  PERFORM send_email.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email .
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.


  MOVE 'Sign-off Reprocess' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '==================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Model' TO lt_body+0(5),
        'Body Number' TO lt_body+5(12),
        'Error Type' TO lt_body+20(15),
       'BDC Flag' TO  lt_body+35(10),
       'BDC Message' TO lt_body+45(80).

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '-------' TO  lt_body+0(5),
        '--------------' TO  lt_body+5(12),
        '------------------' TO  lt_body+20(15),
        '----------' TO  lt_body+35(10),
        '----------------------------' TO  lt_body+45(30),
        '----------------------------' TO lt_body+75(30),
        '-----------------------' TO lt_body+105(20).
  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_log.
    MOVE: it_log-modl TO  lt_body+0(5),
          it_log-body_ser TO  lt_body+5(12),
          it_log-err_ty TO lt_body+20(15),
          it_log-bdc_flag  TO  lt_body+35(10),
          it_log-bdc_msg TO lt_body+45(80).

    APPEND lt_body.
  ENDLOOP.

  CLEAR: lt_body.
  APPEND lt_body.
  APPEND lt_body.
  APPEND lt_body.


  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'Sign-off Reprocess'
      p_rec_type = 'C'
      p_receiver = w_rver
    TABLES
      pt_body    = lt_body.
ENDFORM.                    " SEND_EMAIL
