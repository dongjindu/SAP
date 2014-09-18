*----------------------------------------------------------------------
* Program ID        : ZM_IF_YM_100B_BIN_MASTER
* Title             : [MM] GCCS Interface - BIN Master
* Created on        : 10/24/2007
* Created by        : Rakesh Gandhi
* Specifications By : Crossley, Ron
* Description       : [MM] GCCS Interface - BIN Master
*----------------------------------------------------------------------
REPORT zm_if_ym_100b_bin_master MESSAGE-ID zmco.
TYPE-POOLS : slis,
             icon.

TABLES: mara,
        mlgt,
        mlgn.

*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_bin   .
INCLUDE TYPE ztmm_eai_bin_mst.
TYPES: END OF ty_bin     .

TYPES: BEGIN OF ty_alv.
INCLUDE  TYPE ty_bin    .
TYPES : chkbox(1)       ,
        icon TYPE icon_d,
        tabcolor  TYPE slis_t_specialcol_alv.
TYPES: END OF ty_alv.

DATA: BEGIN OF it_binmst OCCURS 0   ,
        lgtyp  LIKE mlgt-lgtyp      ,
        lgpla  LIKE mlgt-lgpla      ,
        matnr  LIKE mara-matnr      ,
        lgnum  LIKE mlgn-lgnum      ,
        lvorm  LIKE mlgn-lvorm      ,
        lvorm1 LIKE mlgt-lvorm      ,
        rdmng  LIKE mlgt-rdmng      ,
      END OF it_binmst              .

DATA: it_bin      TYPE TABLE OF ty_bin WITH HEADER LINE,
      gt_alv      TYPE TABLE OF ty_alv WITH HEADER LINE,
      gt_fieldcat TYPE slis_t_fieldcat_alv             .

DATA : gs_layout   TYPE slis_layout_alv  ,
       gs_variant  TYPE disvariant       ,
       gs_fieldcat TYPE slis_fieldcat_alv.

DATA: gv_text(20) TYPE c         ,
      gv_repid    LIKE sy-repid  ,
      gv_lines TYPE i            .

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_lgtyp FOR mlgt-lgtyp,      " Fixed BIN Type
                s_lgpla FOR mlgt-lgpla,      " BIN Location
                s_lgnum FOR mlgn-lgnum,      " Whs Number / Wrs Complex
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
  sy-title = '[MM] GCCS Interface - BIN Master'.

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
  REFRESH: it_bin.
  CLEAR  : it_bin.
  SELECT * FROM ztmm_eai_bin_mst
           INTO TABLE it_bin
           WHERE epart_no    IN s_matnr AND
                 ebin_type   IN s_lgtyp AND
                 ebin_loc    IN s_lgpla AND
                 ewhn        IN s_lgnum AND
                 tait_targ_d EQ p_datum.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH text-005.
    EXIT.
  ENDIF.

  gv_text = 'Preparing output...'.
  PERFORM show_progress USING gv_text.

  REFRESH gt_alv.
  CLEAR   gt_alv.

  LOOP AT it_bin.
    MOVE-CORRESPONDING it_bin TO gt_alv.

    CASE it_bin-tait_targ_rslt.
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

  DELETE FROM ztmm_eai_bin_mst
           WHERE epart_no IN s_matnr AND
                 ebin_type   IN s_lgtyp AND
                 ebin_loc    IN s_lgpla AND
                 ewhn        IN s_lgnum AND
                 tait_targ_d EQ p_datum.

  gv_text = 'Saving data...'.
  PERFORM show_progress USING gv_text.

  SELECT a~lgtyp
         a~lgpla
         a~matnr
         b~lgnum
         b~lvorm
         a~lvorm
         a~rdmng
         INTO TABLE it_binmst
         FROM mlgt AS a
         INNER JOIN mara AS c
         ON a~matnr EQ c~matnr
         INNER JOIN mlgn AS b
         ON a~matnr EQ b~matnr AND
            a~lgnum EQ b~lgnum
         WHERE a~lgtyp IN s_lgtyp AND
               a~lgpla IN s_lgpla AND
               a~matnr IN s_matnr AND
               b~lgnum IN s_lgnum AND
               c~mtart IN s_mtart AND
               c~lvorm EQ space   AND
               c~profl IN ('V','K','M') AND
               c~tempb NE '11'.
  SORT it_binmst BY lgtyp lgpla.
  DELETE ADJACENT DUPLICATES FROM it_binmst
                        COMPARING lgtyp lgpla.
  IF NOT it_binmst[] IS INITIAL.
    LOOP AT it_binmst.
      it_bin-tait_targ_d    = sy-datum.
*      IF it_bin-ebin_type IS INITIAL.
*        it_bin-ebin_type = ' '.
*      ELSE.
        it_bin-ebin_type      = it_binmst-lgtyp.
*      ENDIF.
*      IF it_bin-ebin_loc IS INITIAL.
*        it_bin-ebin_loc = ' '.
*      ELSE.
        it_bin-ebin_loc       = it_binmst-lgpla.
*      ENDIF.
      it_bin-epart_no       = it_binmst-matnr.
      it_bin-ewhn           = it_binmst-lgnum.
      it_bin-edel_whse      = it_binmst-lvorm.
      it_bin-edel_styp      = it_binmst-lvorm1.
      it_bin-ernd_qty        = it_binmst-rdmng.
      it_bin-tait_targ_t    = sy-uzeit.
      it_bin-tait_targ_rslt = ' '.
      it_bin-tait_targ_desc = ' '.
      it_bin-tait_event_c   = 'I'.
      APPEND it_bin.
      CLEAR it_bin.
    ENDLOOP.

    CLEAR gv_lines.
    DESCRIBE TABLE it_bin LINES gv_lines.

    INSERT ztmm_eai_bin_mst FROM TABLE it_bin ACCEPTING DUPLICATE KEYS.

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
  IF p_opt4 = 'X'.
    REFRESH: it_bin.
    CLEAR  : it_bin.
    SELECT * FROM ztmm_eai_bin_mst
             INTO TABLE it_bin
             WHERE epart_no    IN s_matnr AND
                   ebin_type   IN s_lgtyp AND
                   ebin_loc    IN s_lgpla AND
                   ewhn        IN s_lgnum AND
                   tait_targ_d EQ p_datum.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH text-005.
      EXIT.
    ENDIF.
  ENDIF.        " IF p_opt4 = 'X'.

  IF NOT it_bin[] IS INITIAL.
    DATA: l_msgtxt(100) TYPE c,
          l_size      TYPE num9.

    CALL FUNCTION 'Z_GCS_EAI_BIN_MASTER'
        DESTINATION p_dest
         TABLES
              eai_bin_mast  = it_bin
         EXCEPTIONS
              no_data_found = 1
              OTHERS        = 2.

    IF sy-subrc <> 0.
      LOOP AT it_bin.
        it_bin-tait_targ_rslt = 'F'.
        MODIFY it_bin INDEX sy-tabix TRANSPORTING tait_targ_rslt.
      ENDLOOP.
      MESSAGE s000 WITH text-008.
    ELSE.
      LOOP AT it_bin.
        it_bin-tait_targ_rslt = 'S'.
        MODIFY it_bin INDEX sy-tabix TRANSPORTING tait_targ_rslt.
      ENDLOOP.
      CLEAR gv_lines.
      DESCRIBE TABLE it_bin LINES gv_lines.

      MESSAGE s000 WITH gv_lines text-009.
    ENDIF.
    UPDATE ztmm_eai_bin_mst FROM TABLE it_bin.

  ENDIF.    " IF NOT it_bin[] IS INITIAL

ENDFORM.                    " send_data_to_eai
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
*&---------------------------------------------------------------------*
*&      Form  display_alv_data
*&---------------------------------------------------------------------*
*       Subroutine to display ALV data
*----------------------------------------------------------------------*
FORM display_alv_data.
  IF p_opt2 = 'X'.
    REFRESH gt_alv.
    CLEAR   gt_alv.

    LOOP AT it_bin.
      MOVE-CORRESPONDING it_bin TO gt_alv.
      CASE it_bin-tait_targ_rslt.
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

          'X'  'EBIN_TYPE'      'Storage Type'       3 'CHAR' ' ',
          'X'  'EBIN_LOC'       'Storage BIN'       10 'CHAR' ' ',
          'X'  'TAIT_TARG_D'    'EAI TARGET DATE'    8 'CHAR' ' ',
          ' '  'EPART_NO'       'PART NO'           15 'CHAR' ' ',
          ' '  'EWHN'           'Warehouse No'       4 'CHAR' ' ',
          ' '  'EDEL_WHSE'      'Del whse'           3 'CHAR' ' ',
          ' '  'EDEL_STYP'      'Del Stype'          3 'CHAR' ' ',
          ' '  'ERND_QTY'       'Rounding Qty'      10 'NUMC' ' ',
          ' '  'ICON'           'flg'                3 'ICON' ' '.


  LOOP AT gt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.
      WHEN 'ERND_QTY'.
        gs_fieldcat-just = 'R'.
    ENDCASE.
    gs_fieldcat-ref_tabname = 'ZTMM_EAI_BIN_MST'.
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
