
************************************************************************
* Program Name      : ZMMR_BOM_COMPARE
* Description       : Module BOM Compare
* Modification Logs
*
************************************************************************
REPORT zmmr_bom_compare NO STANDARD PAGE HEADING
                                     LINE-SIZE  182
                                     LINE-COUNT  58.
INCLUDE: <icon>.

*----- Type
TYPE-POOLS : slis.
FIELD-SYMBOLS: <module>.

DATA: w_mode(1) VALUE 'N',
      w_update(1) VALUE 'S'.

DATA: BEGIN OF it_data OCCURS 0,
      upgvc LIKE mara-matnr,
      comp  LIKE mara-matnr,
      maktx1 LIKE  makt-maktx ,
      maktx2 LIKE  makt-maktx ,
      qty_old LIKE stpo-menge,
      qty_new LIKE stpo-menge,
      qty_diff LIKE stpo-menge,
      datuv_old LIKE ztpp_mod_bom_his-datuv,
      datub_old LIKE ztpp_mod_bom_his-datub,
      aennr_old LIKE stpo-aennr,
      aennr_to_old LIKE stpob-aenra,
      datuv_new LIKE ztpp_mod_bom_his-datuv,
      datub_new LIKE ztpp_mod_bom_his-datub,
      aennr_new LIKE stpo-aennr,
      aennr_to_new LIKE stpob-aenra,
      date_diff(1),
      ct TYPE lvc_t_scol,
      END OF it_data.

DATA: c_capid   LIKE rc29l-capid VALUE 'PP01'.
DATA: c_ready             VALUE   1,
      c_warning             VALUE   2,
      c_success             VALUE   3,
      c_error             VALUE   4,
      c_incorrect            VALUE   5.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.


DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: g_docking_container TYPE REF TO cl_gui_docking_container.

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE   i.

DATA: it_color TYPE lvc_t_scol,
       wa_color LIKE LINE OF it_color.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

PARAMETERS: p_matnr LIKE mast-matnr,
            p_werks LIKE marc-werks DEFAULT 'P001',
            p_datum LIKE sy-datum DEFAULT sy-datum.
PARAMETERS: p_stlst LIKE stko-stlst DEFAULT '1'.
SELECTION-SCREEN END OF BLOCK block1.

*----- Input value check & read data
AT SELECTION-SCREEN.

  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE e000(zz) WITH text-m01.
  ELSE.
    PERFORM display_data.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*  DATA: LT_STB_U2 TYPE STPOX OCCURS 0 WITH HEADER LINE,

*        LT_STB_U3 TYPE STPOX OCCURS 0 WITH HEADER LINE,
  DATA:   lt_stb_u3 TYPE ztpp_mod_bom_his OCCURS 0 WITH HEADER LINE,
          l_stlal LIKE mast-stlal,
          l_verson LIKE ztpp_mod_bom_his-verson,
          l_stlnr LIKE mast-stlnr.

  DATA : BEGIN OF lt_stb_u2 OCCURS 0.
          INCLUDE STRUCTURE stpob.
  DATA : END OF lt_stb_u2.

  DATA : BEGIN OF add_wa OCCURS 0.
          INCLUDE STRUCTURE cszalt.
  DATA : END OF add_wa.

  DATA: BEGIN OF lt_temp OCCURS 0,
        upgn LIKE lt_stb_u2-upgn,
        idnrk  LIKE lt_stb_u2-idnrk,
        END OF lt_temp.
  DATA: lt_mast LIKE TABLE OF mast WITH HEADER LINE.
  DATA: lt_stko LIKE TABLE OF stko WITH HEADER LINE.
  DATA: lt_stas LIKE TABLE OF stas WITH HEADER LINE.
  CLEAR: l_stlal.

  SELECT SINGLE a~stlnr INTO l_stlnr
          FROM mast AS a
          INNER JOIN stko AS b
          ON a~stlnr = b~stlnr
          WHERE a~matnr = p_matnr
          AND a~werks = p_werks
          AND a~stlan = '2'
          AND b~stlty = 'M'
          AND b~stlst = p_stlst.

  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH 'No BOM exist'.
  ENDIF.

*  SELECT single stlnr into l_stlnr
*            FROM mast
*           WHERE MATNR = P_MATNR
*            AND WERKS = P_WERKS
*            AND STLAN = '2'.

  lt_stb_u2-stlty = 'M'.
  lt_stb_u2-stlnr = l_stlnr.
  APPEND lt_stb_u2.

  CALL FUNCTION 'GET_STPO'
   EXPORTING
     all                    = 'X'
     alter                  = '01'
     datub                  = p_datum
     datuv                  = p_datum
*     NO_BUFFER              = ' '
     set                    = 'X'
*     VALID                  = 'X'
*     VIEWNAME               =
    TABLES
      add_wa                 = add_wa
      wa                     = lt_stb_u2
   EXCEPTIONS
     call_invalid           = 1
     end_of_table           = 2
     get_without_set        = 3
     key_incomplete         = 4
     key_invalid            = 5
     no_record_found        = 6
     viewname_invalid       = 7
     OTHERS                 = 8
            .


* SELECT UPGN IDNRK MENGE INTO CORRESPONDING FIELDS OF table LT_STB_U2
*            FROM ZVPP_BOM
*           WHERE MATNR = P_MATNR
*            AND WERKS = P_WERKS
*            AND STLAN = '2'
*            AND STLTY = 'M'
*            AND STLST = '2'
*            AND DATUV <= P_DATUM
*            AND LOEKZ = ' '.



*  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*     EXPORTING
*      CAPID                       = C_CAPID
*      DATUV                       = P_DATUM
**    EMENG                       = p_emeng
**    MEHRS                       = p_mehrs
**    MMORY                       = p_mmory
*      MTNRV                       = P_MATNR
*      MKTLS                       = 'X'
*      STLAL                       = L_STLAL
*      STLAN                       = '2'
**   STPST                       = 0
**   SVWVO                       = 'X'
*      WERKS                       = P_WERKS
** IMPORTING
**    TOPMAT                     =
**   DSTST                       =
*      TABLES
*        STB                       = LT_STB_U2
**   MATCAT                      =
*   EXCEPTIONS
*     ALT_NOT_FOUND               = 1
*     CALL_INVALID                = 2
*     MATERIAL_NOT_FOUND          = 3
*     MISSING_AUTHORIZATION       = 4
*     NO_BOM_FOUND                = 5
*     NO_PLANT_DATA               = 6
*     NO_SUITABLE_BOM_FOUND       = 7
*     CONVERSION_ERROR            = 8
*     OTHERS                      = 9 .
*  IF SY-SUBRC <> 0.
*    MESSAGE E000(ZZ) WITH 'No Active BOM for usegae 2'.
*  ENDIF.
*
  SELECT SINGLE MAX( verson ) INTO l_verson
     FROM ztpp_mod_bom_his
     WHERE matnr = p_matnr.
  IF l_verson > 0.
    SELECT * INTO TABLE lt_stb_u3
      FROM ztpp_mod_bom_his
      WHERE matnr = p_matnr
       AND verson = l_verson.
  ENDIF.
*  CLEAR: L_STLAL.
*  SELECT SINGLE MAX( A~STLAL ) INTO L_STLAL
*          FROM MAST AS A
*          INNER JOIN STKO AS B
*          ON A~STLNR = B~STLNR
*          WHERE A~MATNR = P_MATNR
**       AND A~WERKS = IT_LIST-WERKS
*          AND A~STLAN = '2'
*          AND B~STLTY = 'M'
*          AND B~STLST = '1'.
*  IF SY-SUBRC <> 0.
*    MESSAGE E000(ZZ) WITH 'No Active BOM for usegae 2'.
*  ENDIF.
*
*  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*       EXPORTING
*        CAPID                       = C_CAPID
*        DATUV                       = P_DATUM
**    EMENG                       = p_emeng
**    MEHRS                       = p_mehrs
**    MMORY                       = p_mmory
*        MTNRV                       = P_MATNR
*        MKTLS                       = 'X'
*        STLAL                       = L_STLAL
*        STLAN                       = '2'
**   STPST                       = 0
**   SVWVO                       = 'X'
*        WERKS                       = P_WERKS
** IMPORTING
**    TOPMAT                     =
**   DSTST                       =
*        TABLES
*          STB                       = LT_STB_U3
**   MATCAT                      =
*     EXCEPTIONS
*       ALT_NOT_FOUND               = 1
*       CALL_INVALID                = 2
*       MATERIAL_NOT_FOUND          = 3
*       MISSING_AUTHORIZATION       = 4
*       NO_BOM_FOUND                = 5
*       NO_PLANT_DATA               = 6
*       NO_SUITABLE_BOM_FOUND       = 7
*       CONVERSION_ERROR            = 8
*       OTHERS                      = 9 .

  SORT lt_stb_u3 BY upgn idnrk.
  SORT lt_stb_u2 BY upgn idnrk.
  LOOP AT lt_stb_u2.
    lt_temp-upgn = lt_stb_u2-upgn.
    lt_temp-idnrk = lt_stb_u2-idnrk.
    COLLECT lt_temp.
  ENDLOOP.
  LOOP AT lt_stb_u3.
    lt_temp-upgn = lt_stb_u3-upgn.
    lt_temp-idnrk = lt_stb_u3-idnrk.
    COLLECT lt_temp.
  ENDLOOP.
** Changed on 02/10/11
  SORT lt_temp BY upgn idnrk.
** end of change
  LOOP AT lt_temp.
    CLEAR: it_data, lt_stb_u2, lt_stb_u3.
    READ TABLE lt_stb_u2 WITH KEY upgn = lt_temp-upgn
                                  idnrk = lt_temp-idnrk
                                  BINARY SEARCH.

    it_data-upgvc = lt_temp-upgn.
    it_data-comp  = lt_temp-idnrk.
    it_data-qty_new =  lt_stb_u2-menge.

    it_data-datuv_new = lt_stb_u2-datuv.
    it_data-datub_new = lt_stb_u2-datub.

    it_data-aennr_new = lt_stb_u2-aennr.
    it_data-aennr_to_new = lt_stb_u2-aenra.

    READ TABLE lt_stb_u3 WITH KEY upgn = lt_temp-upgn
                                  idnrk = lt_temp-idnrk
                                  BINARY SEARCH.

    it_data-qty_old = lt_stb_u3-menge.
    it_data-qty_diff = it_data-qty_new - it_data-qty_old.
** Changed on 02/10/11
    IF it_data-datuv_new <> lt_stb_u3-datuv.
      it_data-date_diff = 'X'.
      PERFORM build_color USING 'DATE_DIFF' '3'.
    ENDIF.
    IF it_data-datub_new <> lt_stb_u3-datub.
      it_data-date_diff = 'X'.
      PERFORM build_color USING 'DATE_DIFF' '3'.
    ENDIF.
** End of change

    IF it_data-qty_diff <> 0.
      PERFORM build_color USING 'QTY_DIFF' '6'.
    ENDIF.

    it_data-ct = it_color.
    SELECT SINGLE maktx INTO it_data-maktx1
      FROM makt
      WHERE matnr = it_data-upgvc.

    SELECT SINGLE maktx INTO it_data-maktx2
      FROM makt
      WHERE matnr = it_data-comp.

*??? old/new valid from/to

    it_data-datuv_old = lt_stb_u3-datuv.
    it_data-datub_old = lt_stb_u3-datub.

    it_data-aennr_old = lt_stb_u3-aennr.
    it_data-aennr_to_old = lt_stb_u3-aennr_to.


    APPEND it_data.
    REFRESH it_color.
    CLEAR: it_color.
  ENDLOOP.

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.


  IF g_docking_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_DATA'.
*    PERFORM EVENT_HANDLER_REGISTER.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
*     WHEN 'MM03'.
*      PERFORM CALL_MM03.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_data[]
               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv

*---------------------------------------------------------------------*
*       FORM CREATE_CONTAINER_N_OBJECT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM create_container_n_object.
*  CLEAR: W_REPID.
*  CREATE OBJECT GRID_CONTAINER
*          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
*          EXCEPTIONS
*           CNTL_ERROR = 1
*           CNTL_SYSTEM_ERROR = 2
*           CREATE_ERROR = 3
*           LIFETIME_ERROR = 4
*           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*  W_REPID = SY-REPID.
*  IF SY-SUBRC NE 0.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*         EXPORTING
*              TITEL = W_REPID
*              TXT2  = SY-SUBRC
*              TXT1  = 'The control can not be created'.
*  ENDIF.
*  CREATE OBJECT ALV_GRID
*         EXPORTING I_PARENT = GRID_CONTAINER
*                   I_APPL_EVENTS = 'X'.

  w_repid = sy-repid.

  CREATE OBJECT g_docking_container
    EXPORTING
      repid     = w_repid
      dynnr     = '0200'
      side      = cl_gui_docking_container=>dock_at_bottom
*        RATIO     = 90
      extension = 2000.

  CREATE OBJECT alv_grid
     EXPORTING
       i_parent = g_docking_container.


ENDFORM.                    " create_container_n_object

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sortcat_display.

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'UPGVC'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = ' '.
  APPEND IT_SORT.

  IT_SORT-SPOS           = 2.
  IT_SORT-FIELDNAME      = 'COMP'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = ' '.
  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname.
*        lw_waers LIKE t001-waers,

*  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
  CLEAR: it_fieldcat, it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'UPGVC'       ' ',
                                  ' ' 'COLTEXT'     'UPGVC',
*                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '18',
*
*                                  'S' 'MAKTX1'        ' ',
*                                  ' ' 'COLTEXT'     'Description',
**                                  ' ' 'KEY'         'X',
*                                  'E' 'OUTPUTLEN'   '20',
*
                                  'S' 'COMP'       ' ',
                                 ' ' 'COLTEXT'     'Parts',
*                                  ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MAKTX2'        ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'QTY_OLD'       ' ',
                                  ' ' 'COLTEXT'     'Old Qty',
                                   ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'DATUV_OLD'       ' ',
                                 ' ' 'COLTEXT'     'Valid from',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'DATUB_OLD'       ' ',
                                 ' ' 'COLTEXT'     'Valid to',
                                  'E' 'OUTPUTLEN'   '10',

                                    'S' 'AENNR_OLD'       ' ',
                                 ' ' 'COLTEXT'     'Change No.',
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'AENNR_TO_OLD'       ' ',
                                 ' ' 'COLTEXT'     'Change No. to',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'QTY_NEW'       ' ',
                                  ' ' 'COLTEXT'     'New Qty',
                                   ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '18',


                                 'S' 'DATUV_NEW'       ' ',
                                 ' ' 'COLTEXT'     'Valid from',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'DATUB_NEW'       ' ',
                                 ' ' 'COLTEXT'     'Valid to',
                                  'E' 'OUTPUTLEN'   '10',

                                    'S' 'AENNR_NEW'       ' ',
                                 ' ' 'COLTEXT'     'Change No.',
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'AENNR_TO_NEW'       ' ',
                                 ' ' 'COLTEXT'     'Change No. to',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'QTY_DIFF'       ' ',
                                  ' ' 'COLTEXT'     'Qty Diff',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'DATE_DIFF'       ' ',
                                  ' ' 'COLTEXT'     'Date Diff',
                                  'E' 'OUTPUTLEN'   '1'.
  .


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*&---------------------------------------------------------------------*
*&      Form  event_handler_register
*&---------------------------------------------------------------------*
*       text
**----------------------------------------------------------------------
**
*FORM EVENT_HANDLER_REGISTER .
*
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*   EXPORTING
*     I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*  CREATE OBJECT EVENT_RECEIVER.
*  SET HANDLER event_receiver->handle_hotspot_click FOR ALV_grid.
*  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
*ENDFORM.                    " event_handler_register
*
*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM hotspot_click  USING    e_row_id
      e_column_id.

  READ TABLE it_data INDEX e_row_id.

  CASE e_column_id.
    WHEN 'MATNR'.
      PERFORM call_mm03.
*    WHEN 'RBLNR'.
*      SET PARAMETER ID 'MBN'  FIELD gt_display-rblnr.
*      SET PARAMETER ID 'MJA'  FIELD gt_display-rjahr.
*      CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

  ENDCASE.


ENDFORM.                    " hotspot_click
*&---------------------------------------------------------------------*
*&      Form  CALL_mm03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mm03.

ENDFORM.                                                    " CALL_mm03
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 0200.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0569   text
*----------------------------------------------------------------------*
FORM build_color USING p_fname p_color.
  wa_color-color-col = p_color.
  wa_color-color-int = 1.
  wa_color-fname = p_fname.
  APPEND wa_color TO it_color.
  CLEAR wa_color.
ENDFORM.                    " BUILD_COLOR
