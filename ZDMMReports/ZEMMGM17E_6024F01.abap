*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM17E_6024F01                                          *
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
*/ Upload Class Data from OS file
  PERFORM ws_upload
       TABLES   it_class_data
       USING    p_file
                'DAT'    "File type
       CHANGING w_filelength.
* To Upper Case "To avoid selection wrong working
  LOOP AT it_class_data ASSIGNING <fs_class_data>.
    TRANSLATE <fs_class_data>-classnumnew TO UPPER CASE.
    TRANSLATE <fs_class_data>-classgroup  TO UPPER CASE.
    TRANSLATE <fs_class_data>-name_char   TO UPPER CASE.
  ENDLOOP.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ZTMM_6024_01_LOGNO_H  text
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

*  SORT it_class_data BY classnumnew classgroup name_char.
* I think that at least it is better to sort it_class_data
* by classnumnew. But by need of MM Biz user, so I do not
* sort it_class_data. MM Biz user said characteristic original
* order was important.

  DATA: lv_zzret TYPE zzret.
**S> 08/05/11 Paul : Double COMMENT
  LOOP AT it_class_data ASSIGNING <fs_class_data>.
**    AT NEW <fs_class_data>-classnumnew.
    AT NEW classnumnew.
      PERFORM get_it_classdescriptions.
      PERFORM get_wa_classbasicdata.
      PERFORM bapi_class_existencecheck
                TABLES it_bapiret2
                USING  '001' "Class type
                       <fs_class_data>-classnumnew.
      READ TABLE it_bapiret2 INTO wa_bapiret2
             WITH KEY type = 'S'.
      IF sy-subrc = 0.
        lv_zzret = 'E'.
      ELSE.
        lv_zzret = 'S'.
      ENDIF.
    ENDAT.
    <fs_class_data>-zzret = lv_zzret.

    PERFORM get_it_classcharacteristics.

**    AT END OF <fs_class_data>-classnumnew.
    AT END OF classnumnew.
      IF lv_zzret = 'E'.
        wa_bapiret2-type = 'E'.
        MODIFY it_bapiret2 FROM wa_bapiret2
                           TRANSPORTING type
                           WHERE type = 'S'.
      ELSE.
        PERFORM bapi_class_create
           TABLES it_bapiret2
                  it_classdescriptions
                  it_classcharacteristics
           USING  <fs_class_data>-classnumnew
                  '001' "Class type
                  wa_classbasicdata.
      ENDIF.
*/ BAPI Log to the table ZTLOG
      PERFORM bapilog_to_ztlog USING w_zdocno.

      CLEAR: it_classcharacteristics.
    ENDAT.

  ENDLOOP.

*/ Success/Failure Log to ZTMM_6024_01
  SORT it_class_data BY classnumnew classgroup name_char.

  CLEAR: it_ztmm_6024_01, wa_ztmm_6024_01.
  LOOP AT it_class_data ASSIGNING <fs_class_data>.
    MOVE-CORRESPONDING <fs_class_data> TO wa_ztmm_6024_01.

* App.Doc. Number
    wa_ztmm_6024_01-zdocno = w_zdocno.
**    AT NEW <fs_class_data>-classnumnew.
    AT NEW classnumnew.
* Log number header
      wa_ztmm_6024_01-logno_h = wa_ztmm_6024_01-logno_h + 1.
    ENDAT.
    wa_ztmm_6024_01-logno_d = wa_ztmm_6024_01-logno_d + 1.
    APPEND wa_ztmm_6024_01 TO it_ztmm_6024_01.
  ENDLOOP.
**E<
  INSERT ztmm_6024_01 FROM TABLE it_ztmm_6024_01.

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
* Build the fieldcat according to DDIC structure ZTMM_6024_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZTMM_6024_01'
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
*&      Form  BAPI_CLASS_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM bapi_class_create
          TABLES ext_bapiret2
                   STRUCTURE bapiret2
                 imt_classdescriptions
                   STRUCTURE bapi1003_catch
                 imt_classcharacteristics
                   STRUCTURE bapi1003_charact
          USING  value(im_classnumnew) TYPE bapi_class_key-classnum
                 value(im_classtypenew) TYPE bapi_class_key-classtype
                 value(im_classbasicdata) TYPE bapi1003_basic.
  CLEAR: ext_bapiret2, ext_bapiret2[].
  CALL FUNCTION 'BAPI_CLASS_CREATE'
    EXPORTING
      classnumnew                 = im_classnumnew
      classtypenew                = im_classtypenew
*   CHANGENUMBER                =
      classbasicdata              = im_classbasicdata
*   CLASSDOCUMENT               =
*   CLASSADDITIONAL             =
*   CLASSSTANDARD               =
    TABLES
      return                      = ext_bapiret2
      classdescriptions           = imt_classdescriptions
*   CLASSLONGTEXTS              =
      classcharacteristics        = imt_classcharacteristics
*   CHARACTOVERWRITE            =
*   CHARACTVALUEOVERWRITE       =
*   CHARACTVALUETEXTOVR         =
            .


  CLEAR: ext_bapiret2.
  READ TABLE ext_bapiret2 WITH KEY type = 'E'.
  IF sy-subrc = 0.  "Error Occurred !
    <fs_class_data>-zzret = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
              .
  ELSE.
    <fs_class_data>-zzret = 'S'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait          = 'X'
*     IMPORTING
*       RETURN        =    .
         .
  ENDIF.

ENDFORM.                    "BAPI_CLASS_CREATE
*&---------------------------------------------------------------------*
*&      Form  get_it_classdescriptions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_classdescriptions.
  CLEAR: wa_classdescriptions, it_classdescriptions.
  wa_classdescriptions-langu = 'E'.
  wa_classdescriptions-langu_iso = 'EN'.
  wa_classdescriptions-catchword = <fs_class_data>-catchword.
*wa_classdescriptions-DELETE_FLAG = .
*wa_classdescriptions-INSERT_BEFORE = .

  APPEND wa_classdescriptions TO it_classdescriptions.
ENDFORM.                    " get_it_classdescriptions
*&---------------------------------------------------------------------*
*&      Form  get_wa_classbasicdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wa_classbasicdata.
  CLEAR: wa_classbasicdata.
  wa_classbasicdata-status = 1. "Released
  wa_classbasicdata-classgroup = <fs_class_data>-classgroup.
  " 'SB'. "Sub Part
*wa_classbasicdata-DEPARTMENT_VIEW
*wa_classbasicdata-VALID_FROM
*wa_classbasicdata-VALID_TO
*wa_classbasicdata-AUTHMAINTAIN
*wa_classbasicdata-AUTHCLASSIFY
*wa_classbasicdata-AUTHSEARCH
*wa_classbasicdata-SAME_VALUE_NO
*wa_classbasicdata-SAME_VALUE_W
*wa_classbasicdata-SAME_VALUE_E
*wa_classbasicdata-LOCAL_CLASS
*wa_classbasicdata-KATALOG
ENDFORM.                "get_wa_classbasicdata

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
  lv_tcode     = 'CL02'.
  lv_fm_name   = 'BAPI_CLASS_CREATE'.

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
  w_title = 'Create Class from file'.
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
*      -->P_IT_ZTMM_6024_01  text
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

  LOOP AT it_class_data ASSIGNING <fs_class_data>.
    WRITE:/ <fs_class_data>-classnumnew,
            <fs_class_data>-catchword,
            <fs_class_data>-classgroup,
            <fs_class_data>-name_char.
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
*&      Form  bapi_class_existencecheck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BAPIRET2  text
*      -->P_<FS_CHARACT_DATA>_CHARACT_NAME  text
*----------------------------------------------------------------------*
FORM bapi_class_existencecheck
              TABLES ext_bapiret2
                       STRUCTURE bapiret2
              USING  value(im_classtype) TYPE bapi_class_key-classtype
                     value(im_classnum)  TYPE bapi_class_key-classnum.
  CLEAR: ext_bapiret2, ext_bapiret2[].
  CALL FUNCTION 'BAPI_CLASS_EXISTENCECHECK'
       EXPORTING
            classtype = im_classtype
            classnum  = im_classnum
       TABLES
            return    = ext_bapiret2.
ENDFORM.                    "BAPI_CLASS_EXISTENCECHECK
*&---------------------------------------------------------------------*
*&      Form  get_it_classcharacteristics
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_classcharacteristics.
  CLEAR: wa_classcharacteristics.
  wa_classcharacteristics-name_char = <fs_class_data>-name_char.
*WA_classcharacteristics-CODE_LETTER
*WA_classcharacteristics-CHARACT_ORIGIN
*WA_classcharacteristics-DEPARTMENT_VIEW
*WA_classcharacteristics-INSERT_BEFORE
*WA_classcharacteristics-DELETEVALUE
*WA_classcharacteristics-PRINT_RELEV
*WA_classcharacteristics-SELECT_RELEV
*WA_classcharacteristics-DISPLAY_RELEV
*WA_classcharacteristics-INDEX_RELEV
  APPEND wa_classcharacteristics TO it_classcharacteristics.
ENDFORM.                    " get_it_classcharacteristics
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
