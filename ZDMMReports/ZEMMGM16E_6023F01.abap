*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM16E_6023F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*/ Upload Characteristic Data from OS file
  PERFORM ws_upload
       TABLES   it_charact_data
       USING    p_file
                'DAT'    "File type
       CHANGING w_filelength.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ZTMM_6023_01_LOGNO_H  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
*/ App. Doc. No.
  PERFORM number_get_next USING    c_nro_nr_09
                                   'ZMMNRO0002'
                          CHANGING w_zdocno.
  COMMIT WORK.
************************************************************************
  LOOP AT it_charact_data ASSIGNING <fs_charact_data>.
    PERFORM get_it_charactdescr.
    PERFORM get_wa_charactdetail.
* Existence Check
    PERFORM bapi_charact_existencecheck
              TABLES it_bapiret2
              USING  <fs_charact_data>-charact_name.
    READ TABLE it_bapiret2 INTO wa_bapiret2
           WITH KEY type = 'S'.
    IF sy-subrc = 0.
      <fs_charact_data>-zzret = 'E'.  "Failure
      wa_bapiret2-type = 'E'.
      MODIFY it_bapiret2 FROM wa_bapiret2 INDEX sy-tabix.
    ELSE.
* Create Characteristic
      PERFORM bapi_charact_create
                 TABLES   it_charactdescr
                          it_bapiret2
                 USING    wa_charactdetail.
    ENDIF.
*/ BAPI Log to the table ZTLOG
    PERFORM bapilog_to_ztlog USING w_zdocno.
  ENDLOOP.

*/ Success/Failure Log to ZTMM_6023_01
  CLEAR: it_ztmm_6023_01, wa_ztmm_6023_01.
  LOOP AT it_charact_data ASSIGNING <fs_charact_data>.
    MOVE-CORRESPONDING <fs_charact_data> TO wa_ztmm_6023_01.
* App.Doc. Number
    wa_ztmm_6023_01-zdocno = w_zdocno.
* Log number header
    wa_ztmm_6023_01-logno_h = wa_ztmm_6023_01-logno_h + 1.
    APPEND wa_ztmm_6023_01 TO it_ztmm_6023_01.
  ENDLOOP.

  INSERT ztmm_6023_01 FROM TABLE it_ztmm_6023_01.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM mask_columns TABLES   p_it_fieldcat STRUCTURE it_fieldcat.
* Build the fieldcat according to DDIC structure ZTMM_6023_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZTMM_6023_01'
       CHANGING
            ct_fieldcat      = p_it_fieldcat[].

* Make Column header
  LOOP AT p_it_fieldcat.
    IF p_it_fieldcat-fieldname = 'ZDOCNO'.
      p_it_fieldcat-coltext = 'App.DocNo.'.
    ELSEIF p_it_fieldcat-fieldname = 'LOGNO_H'.
      p_it_fieldcat-coltext = 'Log No.'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSDAT'.
      p_it_fieldcat-no_out = 'X'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSTIM'.
      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_IT_fieldcat-fieldname = 'MATNR'.
*      p_IT_fieldcat-outputlen = 18.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY p_it_fieldcat.
  ENDLOOP.

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  bapi_charact_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM bapi_charact_create
           TABLES   imt_charactdescr
                      STRUCTURE bapicharactdescr
                    ext_bapiret2
                      STRUCTURE bapiret2
           USING    value(im_charactdetail) TYPE bapicharactdetail.
  CLEAR: ext_bapiret2, ext_bapiret2[].
  CALL FUNCTION 'BAPI_CHARACT_CREATE'
    EXPORTING
      charactdetail             = im_charactdetail
*   CHANGENUMBER              =
*   KEYDATE                   = SY-DATUM
    TABLES
      charactdescr              = imt_charactdescr
*   CHARACTVALUESNUM          =
*   CHARACTVALUESCHAR         =
*   CHARACTVALUESCURR         =
*   CHARACTVALUESDESCR        =
*   CHARACTREFERENCES         =
*   CHARACTRESTRICTIONS       =
      return                    = ext_bapiret2.


  CLEAR: ext_bapiret2.
  READ TABLE ext_bapiret2 WITH KEY type = 'E'.
  IF sy-subrc = 0.  "Error Occurred !
    <fs_charact_data>-zzret = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
              .
  ELSE.
    <fs_charact_data>-zzret = 'S'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait          = 'X'
*     IMPORTING
*       RETURN        =    .
         .
  ENDIF.

ENDFORM.                    "BAPI_CHARACT_CREATE
*&---------------------------------------------------------------------*
*&      Form  get_it_charactdescr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_charactdescr.
  CLEAR: wa_charactdescr, it_charactdescr.
  wa_charactdescr-language_int = 'E'.
  wa_charactdescr-language_iso = 'EN'.
  wa_charactdescr-description = <fs_charact_data>-description.
*wa_charactdescr-HEADER1 = .
*wa_charactdescr-HEADER2 = .
  APPEND wa_charactdescr TO it_charactdescr.
ENDFORM.                    " get_it_charactdescr
*&---------------------------------------------------------------------*
*&      Form  get_wa_charactdetail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wa_charactdetail.
  CLEAR: wa_charactdetail.
  wa_charactdetail-charact_name = <fs_charact_data>-charact_name.
  "  'CHARACTERISTIC003'.
  wa_charactdetail-data_type = 'CHAR'.
  wa_charactdetail-length = '30'.
  wa_charactdetail-decimals = '00'.
*wa_charactdetail-CASE_SENSITIV =
  wa_charactdetail-exponent_type = '0'.
  wa_charactdetail-exponent = '0'.
*wa_charactdetail-TEMPLATE =
*wa_charactdetail-WITH_SIGN =
*wa_charactdetail-UNIT_OF_MEASUREMENT =
*wa_charactdetail-UNIT_OF_MEASUREMENT_ISO =
*wa_charactdetail-CURRENCY =
*wa_charactdetail-CURRENCY_ISO =
  wa_charactdetail-status = 1.  "Released
  wa_charactdetail-charact_group = 'SPEC'.
*wa_charactdetail-VALUE_ASSIGNMENT = 0
*wa_charactdetail-NO_ENTRY =
*wa_charactdetail-NO_DISPLAY =
*wa_charactdetail-ENTRY_REQUIRED =
*wa_charactdetail-INTERVAL_ALLOWED =
*wa_charactdetail-SHOW_TEMPLATE =
*wa_charactdetail-DISPLAY_VALUES =
*wa_charactdetail-ADDITIONAL_VALUES =
*wa_charactdetail-DOCUMENT_NO =
*wa_charactdetail-DOCUMENT_TYPE =
*wa_charactdetail-DOCUMENT_PART =
*wa_charactdetail-DOCUMENT_VERSION =
*wa_charactdetail-CHECK_TABLE =
*wa_charactdetail-CHECK_FUNCTION =
*wa_charactdetail-PLANT =
*wa_charactdetail-SELECTED_SET =
*wa_charactdetail-ADT_CLASS =
*wa_charactdetail-ADT_CLASS_TYPE =
*wa_charactdetail-AGGREGATING =
*wa_charactdetail-BALANCING =
*wa_charactdetail-INPUT_REQUESTED_CONF =
*wa_charactdetail-AUTHORITY_GROUP =
*wa_charactdetail-UNFORMATED =
ENDFORM.                    " get_wa_charactdetail
*&---------------------------------------------------------------------*
*&      Form  bapilog_to_ztlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM bapilog_to_ztlog
               USING value(im_w_zdocno) TYPE num10.

**** (Begin)BAPI Log to the table ZTLOG
  IF it_bapiret2 IS INITIAL.  "SUCCESS
    CLEAR: wa_bapiret2.
    wa_bapiret2-type       = 'S'.  "SUCCESS
    wa_bapiret2-id         = 'ZMMM'.
    wa_bapiret2-number     = '999'.
    wa_bapiret2-message_v1 = 'Characteristic is Created'.
*    wa_bapiret2-message_v2 = .
*    wa_bapiret2-message_v3 = .
*    wa_bapiret2-message_v4 = .
    APPEND wa_bapiret2 TO it_bapiret2.
  ENDIF.

  DATA: lv_ztcode     TYPE tcode.
  DATA: lv_zprogramm  TYPE programm.
  DATA: lv_tcode      TYPE tcode.
  DATA: lv_fm_name    TYPE rs38l_fnam.

  lv_ztcode    = sy-tcode.
  lv_zprogramm = sy-cprog.
  lv_tcode     = 'CT04'.
  lv_fm_name   = 'BAPI_CHARACT_CREATE'.

  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_w_zdocno
      im_ztcode            = lv_ztcode
      im_zprogramm         = lv_zprogramm
      im_tcode             = lv_tcode
      im_fm_name           = lv_fm_name
   TABLES
*     imt_bdcmsgcoll       = it_bdcmsgcoll
     imt_bapiret2         = it_bapiret2.
  COMMIT WORK.
**** (End)BAPI Log to the table ZTLOG

ENDFORM.                    " bapilog_to_ztlog
*&---------------------------------------------------------------------*
*&      Form  ps_tb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ps_tb.
* Instanciate PF-STATUS & TITLEBAR.
  w_title = 'Create Characteristic from file'.
  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'                "PF-STATUS
              im_it_func = it_func             "Excluding func
              im_tb      = 'TB'                "TITLEBAR
              im_title   = w_title.            "TITLE
  CLEAR it_func.
ENDFORM.                    " ps_tb
*&---------------------------------------------------------------------*
*&      Form  ws_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTMM_6023_01  text
*      -->P_P_FILE  text
*      -->P_0009   text
*      <--P_W_FILELENGTH  text
*----------------------------------------------------------------------*
FORM ws_upload
  TABLES   ext_data_tab
  USING    value(im_filename) TYPE rlgrap-filename
           value(im_filetype) TYPE rlgrap-filetype
  CHANGING value(ex_filelength).

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
*   CODEPAGE                      = ' '
      filename                      = im_filename
      "'C:\data\polist_rel.txt'
      filetype                      = im_filetype  "'ASC'
*   HEADLEN                       = ' '
*   LINE_EXIT                     = ' '
*   TRUNCLEN                      = ' '
*   USER_FORM                     = ' '
*   USER_PROG                     = ' '
*   DAT_D_FORMAT                  = ' '
   IMPORTING
     filelength                    = ex_filelength
    TABLES
      data_tab                      = ext_data_tab
   EXCEPTIONS
     conversion_error              = 1
     file_open_error               = 2
     file_read_error               = 3
     invalid_type                  = 4
     no_batch                      = 5
     unknown_error                 = 6
     invalid_table_width           = 7
     gui_refuse_filetransfer       = 8
     customer_error                = 9
     OTHERS                        = 10.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    "ws_upload
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.

  LOOP AT it_charact_data ASSIGNING <fs_charact_data>.
    WRITE:/ <fs_charact_data>-charact_name,
            <fs_charact_data>-description.
  ENDLOOP.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  f4_ws_filename_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FILE  text
*----------------------------------------------------------------------*
FORM f4_ws_filename_get
             CHANGING value(ch_filename).
  DATA: lv_cfield(20),
        lv_cvalue  LIKE rlgrap-filename.
  GET CURSOR FIELD lv_cfield VALUE lv_cvalue.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = lv_cvalue
            mask             = ',*.*,*.*.'
            title            = 'Select Upload Filename'
       IMPORTING
            filename         = ch_filename
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.
  IF sy-subrc <> 0.
  ENDIF.
ENDFORM.                    "f4_ws_filename_get
*&---------------------------------------------------------------------*
*&      Form  bapi_charact_existencecheck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BAPIRET2  text
*      -->P_<FS_CHARACT_DATA>_CHARACT_NAME  text
*----------------------------------------------------------------------*
FORM bapi_charact_existencecheck
          TABLES ext_bapiret2
                   STRUCTURE bapiret2
          USING  value(im_charactname) TYPE bapicharactkey-charactname.
  CLEAR: ext_bapiret2, ext_bapiret2[].
  CALL FUNCTION 'BAPI_CHARACT_EXISTENCECHECK'
    EXPORTING
      charactname       = im_charactname
*   KEYDATE           = SY-DATUM
    TABLES
      return            = ext_bapiret2.
ENDFORM.                    "BAPI_CHARACT_EXISTENCECHECK
*&---------------------------------------------------------------------*
*&      Form  make_it_func
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_func.
  wa_func = 'CREA'. APPEND wa_func TO it_func.
ENDFORM.                    " make_it_func
