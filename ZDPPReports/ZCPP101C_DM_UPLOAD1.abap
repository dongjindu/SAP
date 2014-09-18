************************************************************************
* Program Name      : ZCPP101C_DM_UPLOAD1
* Author            : CHOI WOON-MOOK
* Creation Date     : 2003.08.14.
* Specifications By : CHOI WOON-MOOK
* Pattern           : 1.1
* Development Request No :
* Addl Documentation:
* Description       : P.I.R. UPLOAD
*
* Modification Logs
* Date       Developer    RequestNo    Description
*&---------------------------------------------------------------------*
REPORT  zcpp101C_dm_upload1  MESSAGE-ID zmpp.
TABLES: marc, mast, mapl, plko, pbim, pgmi, mara.
* Create internal table from pcdata
DATA: BEGIN OF it_excel       OCCURS 0 ,
        werks        LIKE marc-werks  ,
        matnr        LIKE mara-matnr  ,
        m01(12)      TYPE c ,
        m02(12)      TYPE c ,
        m03(12)      TYPE c ,
        m04(12)      TYPE c ,
        m05(12)      TYPE c ,
        m06(12)      TYPE c ,
        m07(12)      TYPE c ,
        m08(12)      TYPE c ,
        m09(12)      TYPE c ,
        m10(12)      TYPE c ,
        m11(12)      TYPE c ,
        m12(12)      TYPE c ,
      END OF it_excel.
DATA: BEGIN OF st_num,
        werks        LIKE marc-werks  ,
        matnr        LIKE mara-matnr  ,
        m01(12)      TYPE p DECIMALS 0,
        m02(12)      TYPE p DECIMALS 0,
        m03(12)      TYPE p DECIMALS 0,
        m04(12)      TYPE p DECIMALS 0,
        m05(12)      TYPE p DECIMALS 0,
        m06(12)      TYPE p DECIMALS 0,
        m07(12)      TYPE p DECIMALS 0,
        m08(12)      TYPE p DECIMALS 0,
        m09(12)      TYPE p DECIMALS 0,
        m10(12)      TYPE p DECIMALS 0,
        m11(12)      TYPE p DECIMALS 0,
        m12(12)      TYPE p DECIMALS 0,
      END OF st_num.

* ERROR LOGGING
DATA: BEGIN OF it_error   OCCURS 0 ,
        werks        LIKE marc-werks,
        matnr        LIKE mara-matnr,
        marc         TYPE c ,   "Material master not found
        mast         TYPE c ,   "BOM not found
        mapl         TYPE c ,   "Routing not found
        relt(20),               "BAPI RUN RESULT
      END OF it_error.
* Material Header
DATA: st_item   LIKE  bapisitemr.   " MATERIAL HEADER
* MATERIAL MONTHLY PLAN 01 ~ 12
DATA: BEGIN OF  it_month    OCCURS 0.
INCLUDE  STRUCTURE  bapisshdin.
DATA  END OF it_month.

DATA: BEGIN OF  it_werks    OCCURS 0,
         werks  LIKE marc-werks,
      END OF it_werks.

*
DATA: it_message             LIKE TABLE OF bdcmsgcoll WITH HEADER LINE,
      bapiret1               TYPE TABLE OF bapireturn1 WITH HEADER LINE,
      fbdcdata               LIKE TABLE OF bdcdata    WITH HEADER LINE,
      record                 LIKE TABLE OF it_excel   WITH HEADER LINE.
*-----------------------------------------------------------------------
DATA: it_lines   TYPE  i.
DATA: data_ok(3) TYPE  c  VALUE 'YES'.
DATA  g_bdzei    LIKE  pbim-bdzei.   "Independent requirements pointer
DATA  g_bedae    LIKE  pbim-bedae.   "REQUIREMENTS TYPE
DATA  g_answer(1).
DATA  g_pbim.
* BDC DATA -----------------------------------------------------------
DATA : bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE,
       messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : p_mode TYPE c.
DATA : p_versb      LIKE  t459v-versb.   " VERSION
DATA : version_error  TYPE c.
DATA : char_date(10)  TYPE c.
DATA : last_date      LIKE sy-datum.
DATA : g_routing.
DATA : g_prgrp  LIKE  rmcp3-prgrp,
       g_pgktx  LIKE  rmcp3-pgktx,
       g_werks  LIKE  rmcp3-werks,
       g_meins  LIKE  rmcp3-meins.

*=======================================================================
SELECTION-SCREEN  BEGIN OF BLOCK bl1 WITH FRAME.
PARAMETERS: p_year        LIKE  bkpf-gjahr     OBLIGATORY
                                               DEFAULT '2004'.
*PARAMETERS: p_werks       LIKE  t001w-werks    OBLIGATORY
*                                               DEFAULT 'P001'.
*PARAMETERS: p_versb       LIKE  t459v-versb,   " VERSION
PARAMETERS: p_bedae       LIKE  rm60x-bedae     OBLIGATORY
                                               DEFAULT 'VSF'.
SELECTION-SCREEN  END  OF BLOCK bl1.
SELECTION-SCREEN  BEGIN OF BLOCK bl2 WITH FRAME.
PARAMETERS: filename(128) TYPE c               OBLIGATORY
                                               DEFAULT 'c:\'.
SELECTION-SCREEN  END  OF BLOCK bl2.
*-----------------------------------------------------------------------
*AT SELECTION-SCREEN.
*  PERFORM  version_check.

AT SELECTION-SCREEN  ON  p_year.
  PERFORM  version_create.

AT SELECTION-SCREEN  ON  VALUE-REQUEST FOR filename.
  PERFORM filename_get.
*=======================================================================

START-OF-SELECTION.
  PERFORM  version_check.
  CHECK  data_ok  EQ  'YES'.

  PERFORM  bdc_upload_data.

  CHECK  data_ok  EQ  'YES'.

  PERFORM  check_condition.
*------------------------------------------------------------------
END-OF-SELECTION.
  CHECK  data_ok  EQ  'YES'.

  SORT  it_excel  BY  werks  matnr.

  PERFORM  bapi_run.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_condition.

  REFRESH: it_error.


  LOOP AT it_excel WHERE NOT matnr IS initial.
** for e002
*    IF it_excel-werks EQ 'P001' OR it_excel-werks EQ 'E001'.
*    ELSE.
*      DELETE it_excel.
*      CONTINUE.
*    ENDIF.
    IF it_excel-werks EQ 'P001' OR it_excel-werks EQ 'E001'
       or it_excel-werks EQ 'E002'.
    ELSE.
      DELETE it_excel.
      CONTINUE.
    ENDIF.
** end e002

    CLEAR it_error.
    MOVE  it_excel-werks   TO  it_error-werks.
    MOVE  it_excel-matnr   TO  it_error-matnr.
*   MATRIAL MASTER CHECK
    SELECT SINGLE * FROM  marc
      WHERE matnr  EQ  it_excel-matnr
        AND werks  EQ  it_excel-werks.
    IF sy-subrc NE 0.
      CLEAR data_ok.            "No material
      MOVE  'X'              TO  it_error-marc.
    ENDIF.
*   MATRIAL BOM  CHECK
    SELECT SINGLE * FROM  mast
      WHERE matnr  EQ  it_excel-matnr
        AND werks  EQ  it_excel-werks
        AND stlan  EQ  '6'.
    IF sy-subrc NE 0.
      CLEAR data_ok.            "No material
      MOVE  'X'              TO  it_error-mast.
    ENDIF.

*   ROUTNG CHECK
    CLEAR g_routing.
    SELECT * FROM  mapl
      WHERE matnr  EQ  it_excel-matnr
        AND werks  EQ  it_excel-werks.

      SELECT SINGLE * FROM plko
         WHERE  plnnr  EQ  mapl-plnnr
           AND  plnal  EQ  mapl-plnal
           AND  verwe  EQ  '10'.  "costing routing
      IF sy-subrc EQ 0.
        g_routing = 'X'.  EXIT.
      ENDIF.
    ENDSELECT.
    IF  g_routing NE 'X'.
      CLEAR   data_ok.         "No material
      MOVE    'CX'       TO  it_error-mapl.
    ENDIF.
*
    APPEND it_error.

  ENDLOOP.

  IF data_ok IS INITIAL.
    SORT it_error BY werks matnr.
    PERFORM  error_log_write  USING 'E'.
  ENDIF.


ENDFORM.                    " CHECK_CONDITION

*&---------------------------------------------------------------------*
*&      Form  filename_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filename_get.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_path         = 'C:\'
            mask             = ',*.*,*.*.'
            mode             = 'O'
       IMPORTING
            filename         = filename
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.

ENDFORM.                    " filename_get
*&---------------------------------------------------------------------*
*&      Form  BDC_UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_upload_data.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            filename                = filename
            filetype                = 'DAT'
       TABLES
            data_tab                = it_excel
       EXCEPTIONS
            conversion_error        = 1
            file_open_error         = 2
            file_read_error         = 3
            invalid_table_width     = 4
            invalid_type            = 5
            no_batch                = 6
            unknown_error           = 7
            gui_refuse_filetransfer = 8
            OTHERS                  = 9.

  DESCRIBE TABLE it_excel  LINES  it_lines.
  IF it_lines IS  INITIAL.
    CLEAR data_ok.
    MESSAGE i000  WITH  'Upload data 0 rows'.
  ENDIF.

ENDFORM.                    " BDC_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  BAPI_RUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_run.
  FIELD-SYMBOLS: <fs_mon>.
  DATA: l_mon(2)  TYPE  n.
  DATA: l_month(30).
  DATA: l_reqdat  LIKE   sy-datum.
  DATA: l_text(80).     "Message text
  DATA: l_werks   LIKE   marc-werks.

  CLEAR st_item.

* MOVE  p_bedae    TO  st_item-requ_type.  "REQUIREMENTS TYPE
  MOVE  p_versb    TO  st_item-version.    "INPUT VERSION
*  MOVE  'X'        TO  st_item-vers_activ.  "inactive

  REFRESH: it_werks.

  LOOP AT it_excel  WHERE NOT matnr IS initial.

    AT NEW werks.
      PERFORM it_werks_create  USING  it_excel-werks.
      PERFORM check_data_md61  USING  it_excel-werks.
    ENDAT.

    perform set_requirement_type  using st_item-requ_type.
    if st_item-requ_type = 'ERRR' .
       write at: /001(022) 'Error of the Data ==> ',
                  024(018) IT_EXCEL-MATNR          ,
                  042(080) ' : the Strategy in the Material Master.'.
       continue.
    endif.
    MOVE-CORRESPONDING  it_excel  TO  st_num.
    REFRESH: it_month.  CLEAR  it_month.
    DO 12 TIMES.
      MOVE  sy-index    TO  l_mon.
      CONCATENATE 'ST_NUM-M' l_mon  INTO l_month.  " ex) IT_EXCEL-M01
      CONCATENATE p_year l_mon '01' INTO l_reqdat. " ex) 20040101

      ASSIGN (l_month)  TO  <fs_mon>.

      MOVE  '3'        TO  it_month-date_type.
      MOVE  l_reqdat   TO  it_month-req_date.
      MOVE  <fs_mon>   TO  it_month-req_qty.
      MOVE  'EA'       TO  it_month-unit.

      APPEND it_month.

    ENDDO.

    MOVE  it_excel-matnr   TO  st_item-material.
    MOVE  it_excel-werks   TO  st_item-plant.

    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
         EXPORTING
              requirements_item        = st_item
         TABLES
              requirements_schedule_in = it_month
              return                   = bapiret1.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    DESCRIBE TABLE bapiret1 LINES it_lines.

    IF it_lines = 0.
*      WRITE:/ it_excel-matnr, 'P.I.R. Upload succesful' .
      READ  TABLE  it_error  WITH KEY werks = it_excel-werks
                                      matnr = it_excel-matnr.
      IF sy-subrc EQ 0.
        MOVE 'Upload succesful'   TO  it_error-relt.
        MODIFY it_error  INDEX  sy-tabix.
      ENDIF.
*      modify it_error
    ELSE.
      FORMAT COLOR 6   .
      LOOP AT bapiret1 .
        CLEAR: l_text .
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  msgid               = bapiret1-id
                  msgnr               = bapiret1-number
                  msgv1               = bapiret1-message_v1
                  msgv2               = bapiret1-message_v2
                  msgv3               = bapiret1-message_v3
                  msgv4               = bapiret1-message_v4
             IMPORTING
                  message_text_output = l_text.

*        WRITE:/ it_excel-matnr, 'ERROR DESC =',  l_text.
      ENDLOOP.
      FORMAT COLOR OFF .
      CLEAR data_ok.
      READ  TABLE  it_error  WITH KEY werks = it_excel-werks
                                      matnr = it_excel-matnr.
      IF sy-subrc EQ 0.
        MOVE    l_text     TO     it_error-relt.
        MODIFY  it_error   INDEX  sy-tabix.
      ENDIF.
      PERFORM  uploaded_plan_delete  USING it_excel-werks.
      "Version data delete
      EXIT.
    ENDIF.
  ENDLOOP.

  PERFORM  product_group_check.

  PERFORM  product_group_material.

  PERFORM error_log_write USING  'R'.

ENDFORM.                    " BAPI_RUN
*&---------------------------------------------------------------------*
*&      Form  VERSION_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM version_create.

  IF NOT p_year   IS INITIAL  AND p_versb  IS INITIAL.
    p_versb = 'Y1'.          " p_year+2(2).
  ENDIF.
*  IF p_versb EQ '00' OR p_versb EQ '99'.
*    version_error = 'Y'.
*  ENDIF.

  CHECK NOT p_versb IS INITIAL.

  CONCATENATE 'ANNUALPLAN'  p_versb  INTO  g_prgrp.
  CONCATENATE 'Annual Plan' p_year   INTO  g_pgktx SEPARATED BY space.
  MOVE        'P001'                   TO  g_werks.
  MOVE        'EA'                     TO  g_meins.
ENDFORM.                    " VERSION_CREATE
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_log_write  USING  p_title.
  IF p_title = 'E'.
    WRITE:/ 'Upload Data Condition Check Results'.
  ELSE.
    WRITE:/ 'P.I.R. Uploading Results'.
  ENDIF.
  WRITE:/ sy-uline(112).
  FORMAT COLOR COL_HEADING.
  WRITE:/ sy-vline,(05) 'PLANT'    CENTERED,
          sy-vline,(18) 'MATERIAL' CENTERED,
          sy-vline,(10) 'MASTER'   CENTERED,
          sy-vline,(10) 'BOM'      CENTERED,
          sy-vline,(10) 'ROUTING'  CENTERED,
          sy-vline,(40) 'Results',
          sy-vline.
  FORMAT COLOR OFF.
  WRITE:/ sy-uline(112).
  LOOP AT it_error.
    IF p_title EQ 'E'.
      MOVE 'PIR Not load !!!'  TO  it_error-relt.
    ENDIF.
    WRITE:/ sy-vline,(05) it_error-werks  CENTERED,
            sy-vline,(18) it_error-matnr  CENTERED,
            sy-vline,(10) it_error-marc   CENTERED,
            sy-vline,(10) it_error-mast   CENTERED,
            sy-vline,(10) it_error-mapl   CENTERED,
            sy-vline,(40) it_error-relt,
            sy-vline.
  ENDLOOP.
  WRITE:/ sy-uline(112).

ENDFORM.                    " ERROR_LOG_WRITE
*&---------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_MD61
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_MATNR  text
*      -->P_P_WERKS  text
*      -->P_PVERSB_  text
*----------------------------------------------------------------------*
FORM check_data_md61 USING    p_werks.
  DATA: l_text1(100).
* Plant + Version data Delete
  CONCATENATE p_year '1231' INTO   last_date.
  WRITE last_date             TO   char_date.

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.
  CONCATENATE  'Requirement already exists. Plant' p_werks INTO l_text1
                                    SEPARATED BY space.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1 = l_text1
            textline2 = 'It eliminates and it controls again?'
            titel     = 'PIR Upload'
       IMPORTING
            answer    = g_answer.

  CHECK g_answer EQ 'J'.

  p_mode = 'E'.

  CLEAR bdcdata. REFRESH  bdcdata.
  PERFORM generate_bdc_data.
  PERFORM call_transaction  USING  'MD74'.  " DELETE INACTIVE VERSION

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.

  CLEAR bdcdata. REFRESH  bdcdata.
  PERFORM generate_bdc_data_md75.
  PERFORM call_transaction  USING  'MD75'.  " DELETE INACTIVE VERSION

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.

  CLEAR bdcdata. REFRESH  bdcdata.
  PERFORM generate_bdc_data_md76.
  PERFORM call_transaction  USING  'MD76'.  " DELETE INACTIVE VERSION

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.
  MESSAGE i000 WITH 'Processing Error for Deletion...! ' .
  STOP.
ENDFORM.                    " CHECK_DATA_MD61
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_data.

  PERFORM bdc_dynpro USING 'RM60RR20'   '1000'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ONLI'.
  PERFORM bdc_field  USING 'WERKS-LOW'  it_excel-werks.
  PERFORM bdc_field  USING 'BEDAE-LOW'  ' '.    "p_bedae.
  PERFORM bdc_field  USING 'VERSB-LOW'  p_versb.
  PERFORM bdc_field  USING 'HISTFLAG'   'X'.
  PERFORM bdc_field  USING 'INACFLAG'   'X'.
  PERFORM bdc_field  USING 'DATE1'      char_date.
  PERFORM bdc_field  USING 'TESTFLAG'   ' '.

  PERFORM bdc_dynpro USING 'SAPMSSY0'   '0120'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=BACK'.
  PERFORM bdc_dynpro USING 'RM60RR20'   '1000'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/EE'.

ENDFORM.                    " generate_bdc_data
*&---------------------------------------------------------------------*
*&      Form  call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_transaction  USING p_tcode.

  CALL TRANSACTION p_tcode
         USING     bdcdata
         MODE      p_mode
         UPDATE    'S'
         MESSAGES  INTO   messtab.        "error

  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

ENDFORM.                    " call_transaction

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING    program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_field USING    fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  PBIM_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PBIM  text
*----------------------------------------------------------------------*
FORM pbim_data_check  USING      p_werks
                      CHANGING   p_pbim.
  CLEAR p_pbim.

  SELECT SINGLE *  FROM pbim
    WHERE werks = p_werks
      AND versb = p_versb.

  CHECK sy-subrc EQ 0.

  p_pbim = 'X'.

ENDFORM.                    " PBIM_DATA_CHECK
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_data_md75
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_data_md75.

  PERFORM bdc_dynpro USING 'RM60RR30'   '1000'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ONLI'.
  PERFORM bdc_field  USING 'WERKS-LOW'  it_excel-werks.
  PERFORM bdc_field  USING 'BEDAE-LOW'  p_bedae.
  PERFORM bdc_field  USING 'VERSB-LOW'  p_versb.
  PERFORM bdc_field  USING 'DATE1'      char_date.
  PERFORM bdc_field  USING 'TESTFLAG'   ' '.

  PERFORM bdc_dynpro USING 'SAPMSSY0'   '0120'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=BACK'.
  PERFORM bdc_dynpro USING 'RM60RR30'   '1000'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/EE'.

ENDFORM.                    " generate_bdc_data_md75
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_data_md76
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_data_md76.

  PERFORM bdc_dynpro USING 'RM60RR40'   '1000'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ONLI'.
  PERFORM bdc_field  USING 'WERKS-LOW'  it_excel-werks.
  PERFORM bdc_field  USING 'BEDAE-LOW'  p_bedae.
  PERFORM bdc_field  USING 'VERSB-LOW'  p_versb.
  PERFORM bdc_field  USING 'HDATE'      char_date.
  PERFORM bdc_field  USING 'TESTFLAG'   ' '.

  PERFORM bdc_dynpro USING 'SAPMSSY0'   '0120'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=BACK'.
  PERFORM bdc_dynpro USING 'RM60RR40'   '1000'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/EE'.

ENDFORM.                    " generate_bdc_data_md76
*&---------------------------------------------------------------------*
*&      Form  version_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM version_check.

  IF p_versb = '00' OR p_versb = '99'.
    MESSAGE i003 WITH 'Version ' p_versb 'upload being not right'.
    CLEAR  data_ok.
    STOP.
  ENDIF.

ENDFORM.                    " version_check
*&---------------------------------------------------------------------*
*&      Form  UPLOADED_PLAN_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM uploaded_plan_delete  USING p_werks.
* Plant + Version data Delete
  CONCATENATE p_year '1231' INTO   last_date.
  WRITE last_date             TO   char_date.

  CLEAR g_pbim.

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.
  p_mode = 'E'.

  CLEAR bdcdata. REFRESH  bdcdata.
  PERFORM generate_bdc_data.
  PERFORM call_transaction  USING  'MD74'.  " DELETE INACTIVE VERSION

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.

  CLEAR bdcdata. REFRESH  bdcdata.
  PERFORM generate_bdc_data_md75.
  PERFORM call_transaction  USING  'MD75'.  " DELETE INACTIVE VERSION

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.

  CLEAR bdcdata. REFRESH  bdcdata.
  PERFORM generate_bdc_data_md76.
  PERFORM call_transaction  USING  'MD76'.  " DELETE INACTIVE VERSION

  PERFORM  pbim_data_check  USING     p_werks
                            CHANGING  g_pbim.
  CHECK  g_pbim EQ 'X'.
  MESSAGE i000 WITH 'Not erase plan data.'.
  STOP.

ENDFORM.                    " UPLOADED_PLAN_DELETE
*&---------------------------------------------------------------------*
*&      Form  PRODUCT_GROUP_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM product_group_check.
  SELECT SINGLE  *   FROM  mara
   WHERE matnr =  g_prgrp
     AND mtart = 'PROD'  .
  IF sy-subrc  NE 0.       "NOT FOUND GROUP
    PERFORM  create_product_group.
    EXIT.
  ENDIF.

  CHECK  sy-subrc  =  0.

  LOOP AT it_werks.
    SELECT SINGLE  *   FROM  pgmi
       WHERE prgrp = g_prgrp
         AND werks = it_werks-werks.

    CHECK sy-subrc EQ 0.

    CLEAR bdcdata. REFRESH  bdcdata.
    PERFORM bdc_dynpro USING 'SAPMMCP3'   '2000'.
    PERFORM bdc_field  USING 'RMCP3-PRGRP' g_prgrp.
    PERFORM bdc_field  USING 'RMCP3-WERKS' it_werks-werks.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '/00'.

    PERFORM bdc_dynpro USING 'SAPMMCP3'   '0200'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=PLOE'.

    PERFORM bdc_dynpro USING 'SAPMMCP3'   '0200'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=VERB'.

    PERFORM call_transaction  USING  'MC86'.  " DELETE MEMBER

  ENDLOOP.

ENDFORM.                    " PRODUCT_GROUP_CHECK
*&---------------------------------------------------------------------*
*&      Form  product_group_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM product_group_material.
  DATA  last_data.
*  P_MODE = 'A'.

  LOOP AT it_werks.
    CLEAR bdcdata. REFRESH  bdcdata.
    PERFORM bdc_dynpro USING 'SAPMMCP3'   '2000'.
    PERFORM bdc_field  USING 'RMCP3-PRGRP' g_prgrp.
    PERFORM bdc_field  USING 'RMCP3-WERKS' it_WERKS-werks.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '/00'.

    PERFORM bdc_dynpro USING 'SAPMMCP3'   '2100'.
    PERFORM bdc_field  USING 'RMCP3-OMIMA' 'X'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=ENTE'.

    PERFORM bdc_dynpro USING 'SAPMMCP3'   '0200'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=EINZ'.

    LOOP AT it_excel  WHERE  werks EQ it_werks-werks.
      PERFORM bdc_dynpro USING 'SAPMMCP3'   '0200'.
      PERFORM bdc_field  USING 'RMCP3-NRMIT(01)' it_excel-matnr.
      PERFORM bdc_field  USING 'RMCP3-WEMIT(01)' it_excel-werks.
      PERFORM bdc_field  USING 'BDC_OKCODE'  '/00'.
      AT END OF WERKS.
       EXIT.
      ENDAT.
      PERFORM bdc_dynpro USING 'SAPMMCP3'    '0200'.
      PERFORM bdc_field  USING 'BDC_OKCODE'  '=EINZ'.
    ENDLOOP.
    PERFORM bdc_dynpro USING 'SAPMMCP3'    '0200'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=VERB'.
    PERFORM bdc_dynpro USING 'SAPMMCP3'    '0250'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=JAAA'.
    PERFORM bdc_dynpro USING 'SAPMMCP3'    '0200'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=BACK'.
    PERFORM call_transaction  USING  'MC86'.  " CREATE GROUP
  ENDLOOP.

ENDFORM.                    " product_group_material
*&---------------------------------------------------------------------*
*&      Form  create_product_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_product_group.
  LOOP AT it_werks.
    CLEAR bdcdata. REFRESH  bdcdata.
    PERFORM bdc_dynpro USING 'SAPMMCP3'   '0100'.
    PERFORM bdc_field  USING 'RMCP3-PRGRP' g_prgrp.
    PERFORM bdc_field  USING 'RMCP3-PGKTX' g_pgktx.
    PERFORM bdc_field  USING 'RMCP3-WERKS' it_werks-werks.
    PERFORM bdc_field  USING 'RMCP3-MEINS' g_meins.
    PERFORM bdc_field  USING 'RMCP3-OMIMA' 'X'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '/00'.

    PERFORM bdc_dynpro USING 'SAPMMCP3'   '0200'.
    PERFORM bdc_field  USING 'BDC_OKCODE'  '=VERB'.

    PERFORM call_transaction  USING  'MC84'.  " CREATE GROUP
  ENDLOOP.
ENDFORM.                    " create_product_group
*&---------------------------------------------------------------------*
*&      Form  IT_WERKS_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WERKS  text
*----------------------------------------------------------------------*
FORM it_werks_create USING    p_it_werks.

  CLEAR  it_werks.
  MOVE   p_it_werks   TO  it_werks-werks.
  APPEND it_werks.

ENDFORM.                    " IT_WERKS_CREATE

*&---------------------------------------------------------------------*
*&      Form  set_requirement_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_ITEM_REQU_TYPE  text
*----------------------------------------------------------------------*
FORM set_requirement_type USING    pa_type.
  data: l_marc                like marc.

  clear: pa_type.
  select single *  into l_marc
    from marc
   where matnr = it_excel-matnr
     and werks = it_excel-werks .

  if sy-subrc = 0.
     if l_marc-strgr is initial.
        pa_type = 'ERRR'       .
        exit.
     endif.
     select single bedvp  into  pa_type
       from T461S
      where stra1 = l_marc-strgr .
  else.
     pa_type = 'ERRR'          .
  endif.
ENDFORM.                    " set_requirement_type
