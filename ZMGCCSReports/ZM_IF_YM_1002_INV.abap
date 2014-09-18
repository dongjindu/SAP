*----------------------------------------------------------------------
* Program ID        : ZM_IF_YM_1002_INV
* Title             : [MM] GCCS Interface - Inventory w/ location &
*                     Storage unit information
* Created on        : 10/24/2007
* Created by        : Rakesh Gandhi
* Specifications By : Crossley, Ron
* Description       : [MM] GCCS Interface - Inventory w/ location &
*                     Storage unit information
*----------------------------------------------------------------------
REPORT zm_if_ym_1002_inv MESSAGE-ID zmco.

TYPE-POOLS : slis,
             icon.

TABLES: mara,
        lqua.

*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_inv   .
INCLUDE TYPE ztmm_eai_inv.
TYPES: END OF ty_inv     .

TYPES: BEGIN OF ty_alv.
INCLUDE  TYPE ty_inv    .
TYPES : chkbox(1)       ,
        icon TYPE icon_d,
        tabcolor  TYPE slis_t_specialcol_alv.
TYPES: END OF ty_alv.

DATA: BEGIN OF it_inventory OCCURS 0,
        lgtyp LIKE lqua-lgtyp      ,  " Storage Type
        lgpla LIKE lqua-lgpla      ,  " Storage BIN
        matnr LIKE lqua-matnr      ,  " Material number
        lenum LIKE lqua-lenum      ,  " Storage unit number
        lgnum LIKE lqua-lgnum      ,  " Warehouse No/Warehouse Complex
        gesme LIKE lqua-gesme      ,  " ON Hand Qty
      END OF it_inventory           .

DATA: it_inv      TYPE TABLE OF ty_inv WITH HEADER LINE,
      it_inv_tmp  TYPE TABLE OF ty_inv WITH HEADER LINE,
      gt_alv      TYPE TABLE OF ty_alv WITH HEADER LINE,
      gt_fieldcat TYPE slis_t_fieldcat_alv             .

DATA : gs_layout   TYPE slis_layout_alv  ,
       gs_variant  TYPE disvariant       ,
       gs_fieldcat TYPE slis_fieldcat_alv.

DATA: gv_text(20) TYPE c         ,
      gv_repid    LIKE sy-repid  ,
      gv_lines TYPE i            ,
      gv_cnt   TYPE i            .

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_lgtyp FOR lqua-lgtyp,
                s_lgpla FOR lqua-lgpla,
                s_lgnum FOR lqua-lgnum,
                s_lenum FOR lqua-lenum,  " Storage unit number
                s_mtart FOR mara-mtart DEFAULT 'ROH'.
PARAMETERS: p_datum LIKE sy-datum DEFAULT sy-datum OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER: p_opt1 RADIOBUTTON GROUP g1 DEFAULT 'X',
           p_opt2 RADIOBUTTON GROUP g1,
           p_opt3 RADIOBUTTON GROUP g1,
           p_opt4 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME.
PARAMETERS p_dest LIKE rfcdes-rfcdest
                       DEFAULT 'WMRM01'.
SELECTION-SCREEN END OF BLOCK b4.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
sy-title = '[MM] GCCS Interface - Inventory w/ loc & Storage unit info'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_opt1 = 'X'.      " Display current data.
    PERFORM get_data_from_ztable.
  ELSEIF p_opt2 = 'X'.  " Refresh Data
    PERFORM save_data.
  ELSEIF p_opt3 = 'X'.  " Refresh and send data
    PERFORM save_data.
    PERFORM send_data_to_eai.
  ELSEIF p_opt4 = 'X'.  " Send existing data
    PERFORM send_data_to_eai.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  alv_variant_f4
*&---------------------------------------------------------------------*
*       F4 help for ALV Variant
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING p_vari.
  DATA: rs_variant LIKE disvariant.

  CLEAR rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant = rs_variant
            i_save     = 'A'
       IMPORTING
            es_variant = rs_variant
       EXCEPTIONS
            OTHERS     = 1.

  IF sy-subrc = 0.
    p_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                    " alv_variant_f4
*&---------------------------------------------------------------------*
*&      Form  get_data_from_ztable
*&---------------------------------------------------------------------*
*       Subroutine to get existing data from Z table
*----------------------------------------------------------------------*
FORM get_data_from_ztable.
  REFRESH: it_inv.
  CLEAR  : it_inv.
  SELECT * FROM ztmm_eai_inv
           INTO TABLE it_inv
           WHERE epart_no    IN s_matnr AND
                 eloc_tp     IN s_lgtyp AND
                 ebin_no     IN s_lgpla AND
                 ewhn        IN s_lgnum AND
                 estrg_ut    IN s_lenum AND
                 tait_targ_d EQ p_datum.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH text-005.
    EXIT.
  ENDIF.

  gv_text = 'Preparing output...'.
  PERFORM show_progress USING gv_text.

  REFRESH gt_alv.
  CLEAR   gt_alv.

  LOOP AT it_inv.
    MOVE-CORRESPONDING it_inv TO gt_alv.

    CASE it_inv-tait_targ_rslt.
      WHEN space.
      WHEN 'S'.
        gt_alv-icon = icon_led_green.
      WHEN 'F'.
        gt_alv-icon = icon_led_red.
    ENDCASE.

    APPEND gt_alv.
    CLEAR  gt_alv.
  ENDLOOP.

  CLEAR gv_lines.
  DESCRIBE TABLE  gt_alv LINES gv_lines.
  IF gv_lines = 1.
    MESSAGE s000 WITH gv_lines text-011.
  ELSEIF gv_lines > 1.
    MESSAGE s000 WITH gv_lines text-010.
  ENDIF.
  PERFORM display_alv_data.

ENDFORM.                    " get_data_from_ztable
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       Subroutine to delete existing data and Save new data
*----------------------------------------------------------------------*
FORM save_data.

  CLEAR gv_text.
  gv_text = 'Refreshing data...'.
  PERFORM show_progress USING gv_text.

  DELETE FROM ztmm_eai_inv
           WHERE epart_no IN s_matnr AND
                 eloc_tp  IN s_lgtyp AND
                 ebin_no  IN s_lgpla AND
                 ewhn     IN s_lgnum AND
                 estrg_ut IN s_lenum AND
                 tait_targ_d EQ p_datum.

  gv_text = 'Saving data...'.
  PERFORM show_progress USING gv_text.

  SELECT a~lgtyp
         a~lgpla
         a~matnr
         a~lenum
         a~lgnum
         a~gesme
         INTO TABLE it_inventory
         FROM lqua AS a
         INNER JOIN mara AS b
         ON a~matnr EQ b~matnr
         WHERE a~lgtyp IN s_lgtyp AND
               a~lgpla IN s_lgpla AND
               a~matnr IN s_matnr AND
               a~lenum IN s_lenum AND
               a~lgnum IN s_lgnum AND
               b~mtart IN s_mtart AND
               b~lvorm EQ space   AND
               b~profl IN ('V','K','M') AND
               b~tempb NE '11'.

  SORT it_inventory BY lgtyp lgpla matnr lenum.
  DELETE ADJACENT DUPLICATES FROM it_inventory COMPARING
                             lgtyp lgpla matnr lenum.
  IF NOT it_inventory[] IS INITIAL.
    LOOP AT it_inventory.
      it_inv-tait_targ_d    = sy-datum.
*      IF it_inv-eloc_tp IS INITIAL.
*        MOVE space TO it_inv-eloc_tp.
*      ELSE.
      it_inv-eloc_tp        = it_inventory-lgtyp.
*      ENDIF.
*      IF it_inv-ebin_no IS INITIAL.
*        MOVE space TO it_inv-ebin_no.
*      ELSE.
      it_inv-ebin_no        = it_inventory-lgpla.
*      ENDIF.
*      IF it_inv-epart_no IS INITIAL.
*        MOVE space TO it_inv-epart_no.
*      ELSE.
      it_inv-epart_no       = it_inventory-matnr.
*      ENDIF.
*      IF it_inventory-lenum IS INITIAL.
*        MOVE space TO it_inv-estrg_ut.
*      ELSE.
      it_inv-estrg_ut       = it_inventory-lenum.
*      ENDIF.
*      IF it_inv-ewhn IS INITIAL.
*        MOVE space TO it_inv-ewhn.
*      ELSE.
      it_inv-ewhn           = it_inventory-lgnum.
*      ENDIF.
*      IF it_inv-eonhand IS INITIAL.
*        it_inv-eonhand = '0'.
*      ELSE.
      it_inv-eonhand        = it_inventory-gesme.
*      ENDIF.
      it_inv-tait_targ_t    = sy-uzeit.
      it_inv-tait_targ_rslt = ' '.
      it_inv-tait_targ_desc = ' '.
      it_inv-tait_event_c   = 'I'.
      APPEND it_inv.
      CLEAR it_inv.
    ENDLOOP.

    CLEAR gv_lines.
    DESCRIBE TABLE it_inv LINES gv_lines.

*    INSERT ztmm_eai_inv FROM TABLE it_inv ACCEPTING DUPLICATE KEYS.
    INSERT ztmm_eai_inv FROM TABLE it_inv.


    IF sy-subrc = 0.
      MESSAGE s000 WITH gv_lines text-003.
      IF p_opt2 = 'X'.
        PERFORM display_alv_data.
      ENDIF.
    ELSE.
      MESSAGE s000 WITH text-004.
    ENDIF.

  ELSE.
    MESSAGE s000 WITH text-005.
    EXIT.
  ENDIF.

ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  send_data_to_eai
*&---------------------------------------------------------------------*
*       Subroutine to call RFC to pass data to WebMethods
*----------------------------------------------------------------------*
FORM send_data_to_eai.
  DATA: lv_cnt TYPE i.

  IF p_opt4 = 'X'.
    REFRESH: it_inv.
    CLEAR  : it_inv.
    SELECT * FROM ztmm_eai_inv
             INTO TABLE it_inv
             WHERE epart_no IN s_matnr AND
                   eloc_tp  IN s_lgtyp AND
                   ebin_no  IN s_lgpla AND
                   ewhn     IN s_lgnum AND
                   estrg_ut IN s_lenum AND
                   tait_targ_d EQ p_datum.
    IF sy-subrc NE 0.
      MESSAGE s000 WITH text-005.
      EXIT.
    ENDIF.
  ENDIF.        " IF p_opt4 = 'X'.

  IF NOT it_inv[] IS INITIAL.
    DATA: l_msgtxt(100) TYPE c,
          l_size      TYPE num9.
    SORT it_inv BY eloc_tp.
    CLEAR: gv_cnt,
           lv_cnt.

**    LOOP AT it_inv.
**      gv_cnt = gv_cnt + 1.
***      lv_cnt = lv_cnt + 1.
**      MOVE-CORRESPONDING it_inv TO it_inv_tmp.
**      APPEND it_inv_tmp.
**      CLEAR  it_inv_tmp.
**      IF gv_cnt >= 5000.
**
**        CALL FUNCTION 'Z_GCS_EAI_INVENTORY'
**            DESTINATION p_dest
**         TABLES
**           eai_inventory       = it_inv_tmp
**         EXCEPTIONS
**           no_data_found       = 1
**           OTHERS              = 2.
**        IF sy-subrc = 0.
**          lv_cnt = lv_cnt + gv_cnt.
**          LOOP AT it_inv_tmp.
**            it_inv_tmp-tait_targ_rslt = 'S'.
**           MODIFY it_inv_tmp INDEX sy-tabix TRANSPORTING
*tait_targ_rslt
**.
**          ENDLOOP.
**        ELSE.
**          LOOP AT it_inv_tmp.
**            it_inv_tmp-tait_targ_rslt = 'F'.
**           MODIFY it_inv_tmp INDEX sy-tabix TRANSPORTING
*tait_targ_rslt
**.
**          ENDLOOP.
**        ENDIF.  " IF sy-subrc = 0.
**        REFRESH it_inv_tmp.
**        CLEAR:  it_inv_tmp,
**                gv_cnt.
**      ENDIF.    " IF gv_cnt >= 5000.
**    ENDLOOP.    " LOOP AT it_inv
**
**    CALL FUNCTION 'Z_GCS_EAI_INVENTORY'
**            DESTINATION p_dest
**     TABLES
**       eai_inventory       = it_inv_tmp
**     EXCEPTIONS
**       no_data_found       = 1
**       OTHERS              = 2.
**    IF sy-subrc = 0.
**      lv_cnt = lv_cnt + gv_cnt.
**      LOOP AT it_inv_tmp.
**        it_inv_tmp-tait_targ_rslt = 'S'.
**        MODIFY it_inv_tmp INDEX sy-tabix TRANSPORTING tait_targ_rslt.
**      ENDLOOP.
**    ELSE.
**      LOOP AT it_inv_tmp.
**        it_inv_tmp-tait_targ_rslt = 'F'.
**        MODIFY it_inv_tmp INDEX sy-tabix TRANSPORTING tait_targ_rslt.
**      ENDLOOP.
**    ENDIF.  " IF sy-subrc = 0.
**
**    MESSAGE s000 WITH lv_cnt text-009.
**    UPDATE ztmm_eai_inv FROM TABLE it_inv_tmp.


    CALL FUNCTION 'Z_GCS_EAI_INVENTORY'
        DESTINATION p_dest
     TABLES
       eai_inventory       = it_inv
     EXCEPTIONS
       no_data_found       = 1
       OTHERS              = 2.

    IF sy-subrc <> 0.
      LOOP AT it_inv.
        it_inv-tait_targ_rslt = 'F'.
        MODIFY it_inv INDEX sy-tabix TRANSPORTING tait_targ_rslt.
      ENDLOOP.
      MESSAGE s000 WITH text-008.
    ELSE.
      LOOP AT it_inv.
        it_inv-tait_targ_rslt = 'S'.
        MODIFY it_inv INDEX sy-tabix TRANSPORTING tait_targ_rslt.
      ENDLOOP.
      CLEAR gv_lines.
      DESCRIBE TABLE it_inv LINES gv_lines.

      MESSAGE s000 WITH gv_lines text-009.
    ENDIF.
    UPDATE ztmm_eai_inv FROM TABLE it_inv.

  ENDIF.    " IF NOT it_inv[] IS INITIAL

ENDFORM.                    " send_data_to_eai
*&---------------------------------------------------------------------*
*&      Form  display_alv_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv_data.
  IF p_opt2 = 'X'.
    REFRESH gt_alv.
    CLEAR   gt_alv.

    LOOP AT it_inv.
      MOVE-CORRESPONDING it_inv TO gt_alv.
      CASE it_inv-tait_targ_rslt.
        WHEN space.
        WHEN 'S'.
          gt_alv-icon = icon_led_green.
        WHEN 'F'.
          gt_alv-icon = icon_led_red.
      ENDCASE.
      APPEND gt_alv.
      CLEAR  gt_alv.
    ENDLOOP.

  ENDIF.
  PERFORM set_layout.
  CLEAR gs_variant.

  gv_repid = sy-repid.
  gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

  PERFORM alv_fieldcat.

  PERFORM alv_grid_display.

ENDFORM.                    " display_alv_data
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       Set Layout
*----------------------------------------------------------------------*
FORM set_layout.
  CLEAR gs_layout.
  gs_layout-colwidth_optimize      = 'X'.
  gs_layout-box_fieldname          = 'CHKBOX'.
  gs_layout-coltab_fieldname       = 'TABCOLOR'.
ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       Prepare ALV fieldcatalogue
*----------------------------------------------------------------------*
FORM alv_fieldcat.
  DATA: l_pos TYPE i.
  CLEAR:  gs_fieldcat,
          gt_fieldcat.
  REFRESH gt_fieldcat.

  DEFINE append_fieldcat.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-qfieldname    = &6.
    append gs_fieldcat to gt_fieldcat.
  END-OF-DEFINITION.

  append_fieldcat :

          'X'  'ELOC_TP'        'Location Type'      3 'CHAR' ' ',
          'X'  'EBIN_NO'        'Storage BIN'       10 'CHAR' ' ',
          'X'  'TAIT_TARG_D'    'EAI TARGET DATE'    8 'CHAR' ' ',
          'X'  'EPART_NO'       'PART NO'           18 'CHAR' ' ',
          'X'  'ESTRG_UT'       'Storage Unit'      20 'CHAR' ' ',
          ' '  'EWHN'           'Warehouse No'       3 'CHAR' ' ',
          ' '  'EONHAND'        'ON Hand Qty'       13 'NUMC' ' ',
          ' '  'ICON'           'flg'                3 'ICON' ' '.


  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.
      WHEN 'EONHAND'.
        gs_fieldcat-just = 'R'.
    ENDCASE.
    gs_fieldcat-ref_tabname = 'ZTMM_EAI_INV'.
    gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    MODIFY gt_fieldcat FROM gs_fieldcat.
  ENDLOOP.


ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_grid_display
*&---------------------------------------------------------------------*
*       Subroutine to display data in ALV Grid form
*----------------------------------------------------------------------*
FORM alv_grid_display.
  DATA: lv_save VALUE 'A'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = gv_repid
*            i_callback_pf_status_set = ' '
*            i_callback_user_command  = ' '
            is_layout                = gs_layout
*            it_excluding             =
            it_fieldcat              = gt_fieldcat
*            it_special_groups        =
*            it_sort                  =
            i_save                   = lv_save
            is_variant               = gs_variant
*            it_events                =
       TABLES
            t_outtab                 = gt_alv
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

ENDFORM.                    " alv_grid_display
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       Subroutine to show progress
*----------------------------------------------------------------------*
*      -->P_GV_TEXT  text
*----------------------------------------------------------------------*
FORM show_progress USING    p_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
*            PERCENTAGE =
            text       = p_text.

ENDFORM.                    " show_progress
