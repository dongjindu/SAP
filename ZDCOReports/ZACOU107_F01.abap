*----------------------------------------------------------------------*
*   INCLUDE ZACOU107_F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get data of Calculate Variances from table ZTCOU106
*----------------------------------------------------------------------*
FORM get_data.
  DATA lt_106 TYPE TABLE OF ty_ztcou106 WITH HEADER LINE.

  REFRESH: gt_ztcou106, gt_out.
  CLEAR  : gt_ztcou106, gt_out, gv_cnt.

* Controling area description
  SELECT SINGLE bezei INTO bezei
    FROM tka01
   WHERE kokrs = p_kokrs.

  PERFORM convert_reason_for_selection.

  IF r_kzust[] IS INITIAL.
    SELECT a~kokrs a~bdatj a~kalka a~id a~poper a~zrclss
           a~upgvc a~compn a~kstar a~lifnr
           a~kzust1 a~pmenge a~pwertn a~kpein a~menge a~wertn
           a~meeht dmenge dwertn
           a~wertn1 a~kzust2 a~wertn2 a~kzust3 a~wertn3 maktx c~waers
* UD1K941594 by IG.MOON 9/18/07
* {
           d~kzust1 AS infrsn
           a~ekgrp
* }
      INTO CORRESPONDING FIELDS OF TABLE gt_ztcou106
      FROM ztcou106 AS a
      JOIN makt AS b
        ON b~matnr = a~compn
       AND spras = sy-langu
      JOIN tka01 AS c
        ON c~kokrs  = a~kokrs
* UD1K941594 by IG.MOON 9/18/07
* {
      LEFT OUTER JOIN ztcou102 AS d
        ON  d~kokrs EQ a~kokrs
        AND d~bdatj EQ a~bdatj
        AND d~poper EQ a~poper
        AND d~kalka EQ a~kalka
        AND d~ver   EQ space
        AND d~matnr EQ a~compn
* }
     WHERE a~kokrs = p_kokrs
       AND a~bdatj = p_year
       AND a~kalka = p_kalka
       AND a~poper = p_poper
       AND a~id  IN s_id
       AND a~upgvc IN s_upgvc
       AND a~compn IN s_compn
       AND a~lifnr IN s_lifnr
       AND a~kzust1 IN s_rsn
* UD1K941594 by IG.MOON 9/18/07
* {
       AND zrclss IN s_zrclss
       AND a~ekgrp  IN s_ekgrp.
* }
  ELSE.

    SELECT a~kokrs a~bdatj a~kalka a~id a~poper a~zrclss
           a~upgvc a~compn a~kstar a~lifnr
           a~kzust1 a~pmenge a~pwertn a~kpein a~menge a~wertn
           a~meeht dmenge dwertn
           a~wertn1 a~kzust2 a~wertn2 a~kzust3 a~wertn3 maktx c~waers
* UD1K941594 by IG.MOON 9/18/07
* {
           d~kzust1 AS infrsn
* }
      INTO CORRESPONDING FIELDS OF TABLE gt_ztcou106
      FROM ztcou106 AS a
      JOIN makt AS b
        ON b~matnr = a~compn
       AND spras = sy-langu
      JOIN tka01 AS c
        ON c~kokrs  = a~kokrs
* UD1K941594 by IG.MOON 9/18/07
* {
      LEFT OUTER JOIN ztcou102 AS d
        ON  d~kokrs EQ a~kokrs
        AND d~bdatj EQ a~bdatj
        AND d~poper EQ a~poper
        AND d~kalka EQ a~kalka
        AND d~ver   EQ space
        AND d~matnr EQ a~compn
* }
     WHERE a~kokrs = p_kokrs
       AND a~bdatj = p_year
       AND a~kalka = p_kalka
       AND a~poper = p_poper
       AND a~id  IN s_id
       AND a~upgvc IN s_upgvc
       AND a~compn IN s_compn
       AND a~lifnr IN s_lifnr
       AND a~kzust1 IN r_kzust
* UD1K941594 by IG.MOON 9/18/07
* {
       AND zrclss IN s_zrclss.
* }

  ENDIF.

  DESCRIBE TABLE gt_ztcou106 LINES gv_cnt.

  CHECK gv_cnt > 0.

  SORT gt_ztcou106 BY id upgvc compn.

* UD1K941594 - by IG.MOON 9/18/2007 {
  DATA : BEGIN OF $upgvc OCCURS 0,
           upgvc     LIKE makt-matnr,
         END   OF  $upgvc.
  DATA : BEGIN OF $upgvc_t OCCURS 0,
           upgvc     LIKE makt-matnr,
           maktx     LIKE makt-maktx,
         END   OF  $upgvc_t.

  LOOP AT gt_ztcou106.
    $upgvc-upgvc = gt_ztcou106-upgvc.
    COLLECT $upgvc.
  ENDLOOP.

  SELECT matnr maktx INTO TABLE $upgvc_t
  FROM makt
   FOR ALL ENTRIES IN $upgvc
   WHERE matnr = $upgvc-upgvc.

  SORT $upgvc_t BY upgvc.

* }

  SORT gt_ztcou106 BY kokrs bdatj kalka id poper.

  DATA $flag.
  DATA locked.

  LOOP AT gt_ztcou106.

    AT NEW poper.
      $flag = true.
    ENDAT.

    MOVE-CORRESPONDING gt_ztcou106 TO  gt_out.

    READ TABLE $upgvc_t WITH KEY upgvc = gt_out-upgvc BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-upgvc_t = $upgvc_t-maktx.
    ENDIF.

*   Convert reason : 3digit -> 2digit for display

    IF gt_out-infrsn+0(1) <> 'X'.
      CLEAR gt_out-infrsn.
      CONCATENATE gt_ztcou106-infrsn+0(1)
                  gt_ztcou106-infrsn+2(1) INTO  gt_out-infrsn.
    ENDIF.

    IF gt_out-kzust1+0(1) <> 'X'.
      CLEAR gt_out-kzust1.
      CONCATENATE gt_ztcou106-kzust1+0(1)
                  gt_ztcou106-kzust1+2(1) INTO  gt_out-kzust1.
    ENDIF.

    IF gt_out-kzust2+0(1) <> 'X'.
      CLEAR gt_out-kzust2.
      CONCATENATE gt_ztcou106-kzust2+0(1)
                  gt_ztcou106-kzust2+2(1) INTO  gt_out-kzust2.
    ENDIF.

    IF gt_out-kzust3+0(1) <> 'X'.
      CLEAR gt_out-kzust3.
      CONCATENATE gt_ztcou106-kzust3+0(1)
                  gt_ztcou106-kzust3+2(1) INTO  gt_out-kzust3.
    ENDIF.

    IF $flag EQ true.
      CALL FUNCTION 'Z_CHK_LOCK_STATUS_FSC'
           EXPORTING
                kokrs         = gt_out-kokrs
                bdatj         = gt_out-bdatj
                kalka         = gt_out-kalka
                id            = gt_out-id
                poper         = gt_out-poper
           IMPORTING
                locked        = locked
           EXCEPTIONS
                invalid_poper = 1
                OTHERS        = 2.
      IF sy-subrc <> 0.
      ENDIF.
      CLEAR $flag.
    ENDIF.

*// 2011.09.06    Change by YN.KIM   for (ECC6)
    IF locked EQ true.
*      gt_out-lock_ico = icon_locked.
      gt_out-lock_ico = gc_yellow_icon.
*    else.
*      gt_out-lock_ico = gc_red_icon.

    ENDIF.
    gt_out-lock = locked.

    APPEND  gt_out.
    CLEAR  gt_out.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Exclude function code
*----------------------------------------------------------------------*
FORM exclude_functions USING p_tabname.
  PERFORM append_exclude_functions
          TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_loc_delete_row,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_refresh.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Create ALV control: Field catalog
*----------------------------------------------------------------------*
FORM create_field_category.
  CLEAR gt_fcat.
  DATA: l_pos       TYPE i.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    gs_fcat-edit     = &7.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
          'X'  'LOCK_ICO'   'LK'               4  'CHAR' '' '',
          'X'  'ID'         'ID'              10  'CHAR' '' '',
          'X'  'UPGVC'      'UPG'             18  'CHAR' '' '',
          'X'  'UPGVC_T'    'UPGVC.Desc.'     30  'CHAR' '' '',
          'X'  'COMPN'      'Part #'          18  'CHAR' '' '',
          'X'  'MAKTX'      'Part.Desc. '     30  'CHAR' '' '',
          ' '  'KSTAR'      'CstEmt'          10  'CHAR' '' '',
          ' '  'LIFNR'      'Vendor'          10  'CHAR' '' '',
          ' '  'EKGRP'      'PurG'             3  'CHAR' '' '',
          ' '  'INFRSN'     'Info.'            3  'CHAR' '' '',
          ' '  'ZRCLSS'     'C'               1   'CHAR' '' '',
          ' '  'DWERTN'     'Diff'            15  'DEC' '' '',
          ' '  'KZUST1'     'Rsn1'            3   'CHAR' '' '',
          ' '  'WERTN1'     'Rs1 $'           15  'DEC' '' '',
          ' '  'KZUST2'     'Rsn2'            3   'CHAR' '' '',
          ' '  'WERTN2'     'Rs2 $'           15  'DEC' '' '',
          ' '  'KZUST3'     'Rsn3'            3   'CHAR' '' '',
          ' '  'WERTN3'     'Rs3 $'           15  'DEC' '' '',
          ' '  'DMENGE'     'Diff.Qty'        15  'QUAN' '' '',
          ' '  'PWERTN'     'Prv.Price'       15  'CURR' '' '',
          ' '  'WERTN'      'Curr.Price'      15  'CURR' '' '',
          ' '  'KPEIN'      'Per'             5   'DEC' '' '',
          ' '  'MEEHT'      'UoM'             3   'UNIT' '' '',
          ' '  'PMENGE'     'Prv.Qty'         15  'QUAN' '' '',
          ' '  'MENGE'      'Curr.Qty'        15  'QUAN' '' ''.


  LOOP AT gt_fcat INTO gs_fcat WHERE ( fieldname+0(5) = 'KZUST'
                                OR fieldname = 'INFRSN' )
                                  OR fieldname = 'KSTAR'
                                  OR fieldname = 'ICON'
* UD1K941594 by IG.MOON 9/18/07
* {
                                  OR fieldname = 'ZRCLSS'.
* }
    gs_fcat-just = 'C'.

    IF gs_fcat-fieldname+0(5) = 'KZUST' OR
       gs_fcat-fieldname = 'INFRSN'.

      gs_fcat-f4availabl = 'X'.
      MODIFY gt_fcat FROM gs_fcat
             TRANSPORTING f4availabl just
             WHERE fieldname = gs_fcat-fieldname.
    ELSE.
* UD1K941594 by IG.MOON 9/18/07
* {
      IF gs_fcat-fieldname EQ 'ZRCLSS' OR gs_fcat-fieldname EQ 'EKGRP'.
        gs_fcat-ref_table   = 'ZTCOU106'.
        gs_fcat-ref_field   = gs_fcat-fieldname.
        MODIFY gt_fcat FROM gs_fcat TRANSPORTING ref_table
                                                 ref_field
                       WHERE fieldname = gs_fcat-fieldname.
      ELSE.
* }
        MODIFY gt_fcat FROM gs_fcat TRANSPORTING just
                       WHERE fieldname = gs_fcat-fieldname.
      ENDIF.

    ENDIF.

  ENDLOOP.

  gs_fcat-just = 'R'.
  gs_fcat-currency = 'WAERS'.
  MODIFY gt_fcat FROM gs_fcat
         TRANSPORTING just currency
         WHERE datatype = 'CURR'.

  gs_fcat-just = 'R'.
  gs_fcat-decimals = 3.
  MODIFY gt_fcat FROM gs_fcat
         TRANSPORTING just decimals
         WHERE fieldname = 'DWERTN'
            OR fieldname = 'WERTN1'
            OR fieldname = 'WERTN2'
            OR fieldname = 'WERTN3'.

  gs_fcat-qfieldname = 'MEEHT'.
  gs_fcat-ref_field = gs_fcat-fieldname.
  gs_fcat-ref_table = 'ZTCOU106'.
  gs_fcat-just = 'R'.
  MODIFY gt_fcat FROM gs_fcat
         TRANSPORTING just qfieldname ref_field ref_table
         WHERE datatype = 'QUAN'.

  gs_fcat-just = 'C'.
  gs_fcat-convexit = 'CUNIT'.
  gs_fcat-ref_field = gs_fcat-fieldname.
  gs_fcat-ref_table = 'ZTCOU106'.
  MODIFY gt_fcat FROM gs_fcat
         TRANSPORTING just convexit ref_field ref_table
         WHERE datatype = 'UNIT'.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_EVENT
*&---------------------------------------------------------------------*
*       Setting for event
*----------------------------------------------------------------------*
FORM set_event.
  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_toolbar       FOR g_grid.
  SET HANDLER g_event_receiver->handle_user_command  FOR g_grid.
  SET HANDLER g_event_receiver->handle_data_changed FOR g_grid.
  SET HANDLER g_event_receiver->on_f4 FOR g_grid.

ENDFORM.                    " SET_EVENT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       Create attributes of cell
*----------------------------------------------------------------------*
FORM build_cell_attr.

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.
  DATA $ix TYPE i.

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  LOOP AT gt_out.
    $ix = sy-tabix.

    CLEAR gs_fcat.
    LOOP AT gt_fcat INTO gs_fcat.
      ls_celltab-fieldname = gs_fcat-fieldname.
      IF ( gs_fcat-fieldname = 'KZUST1' OR
         gs_fcat-fieldname = 'WERTN1' OR
         gs_fcat-fieldname = 'KZUST2' OR
         gs_fcat-fieldname = 'WERTN2' OR
         gs_fcat-fieldname = 'KZUST3' OR
         gs_fcat-fieldname = 'WERTN3' ) AND gt_out-lock EQ false.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      ELSE.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      ENDIF.
      INSERT ls_celltab INTO TABLE lt_celltab.
    ENDLOOP.

    __cls gt_out-celltab.
    INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
    MODIFY gt_out INDEX $ix TRANSPORTING celltab.

  ENDLOOP.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU100
*----------------------------------------------------------------------*
FORM save_data.
  DATA: l_kzust1    TYPE kzust,
        l_kzust2    TYPE kzust,
        l_kzust3    TYPE kzust,
        lt_row      TYPE lvc_t_row,
        ls_row      TYPE lvc_s_row,
        lt_roid     TYPE lvc_t_roid,
        l_chk,
        l_cnt(5),
        l_cnt1(5),
        l_acnt TYPE i,
        l_ecnt TYPE i,
        l_tot TYPE zdwertn,
        lv_msg(200),                 " Message
        lt_ztcou106 TYPE TABLE OF ztcou106 WITH HEADER LINE.

* Save seleted data to table ZTCOU106
  CLEAR: l_cnt, lt_ztcou106, lt_row[], lt_roid[].
  REFRESH lt_ztcou106.

  DESCRIBE TABLE gt_out LINES l_acnt.

  CALL METHOD g_grid->get_selected_rows
              IMPORTING et_index_rows = lt_row
                        et_row_no = lt_roid.

  LOOP AT lt_row INTO ls_row.
    READ TABLE gt_out INDEX ls_row-index.

    IF sy-subrc = 0.
      CLEAR l_tot.
      l_tot = gt_out-wertn1 + gt_out-wertn2 + gt_out-wertn3.

      IF l_tot <> gt_out-dwertn OR
        ( gt_out-kzust1 IS INITIAL AND NOT gt_out-wertn1 IS INITIAL ) OR
        ( gt_out-kzust2 IS INITIAL AND NOT gt_out-wertn2 IS INITIAL ) OR
        ( gt_out-kzust3 IS INITIAL AND NOT gt_out-wertn3 IS INITIAL ) OR
        ( NOT gt_out-kzust1 IS INITIAL AND gt_out-wertn1 IS INITIAL ) OR
        ( NOT gt_out-kzust2 IS INITIAL AND gt_out-wertn2 IS INITIAL ) OR
        ( NOT gt_out-kzust3 IS INITIAL AND gt_out-wertn3 IS INITIAL ).

        l_ecnt = l_ecnt + 1.

      ELSE.
        CLEAR: l_kzust1, l_kzust2, l_kzust3, l_cnt1, l_chk.

        IF gt_out-kzust1+0(1) = 'X'.
          l_kzust1 = gt_out-kzust1.
        ELSE.
          PERFORM convert_reason_for_db USING    gt_out-kzust1
                                                 gt_out-wertn1
                                        CHANGING l_kzust1.
        ENDIF.

        IF gt_out-kzust2+0(1) = 'X'.
          l_kzust1 = gt_out-kzust2.
        ELSE.
          PERFORM convert_reason_for_db USING    gt_out-kzust2
                                                 gt_out-wertn2
                                        CHANGING l_kzust2.
        ENDIF.

        IF gt_out-kzust3+0(1) = 'X'.
          l_kzust1 = gt_out-kzust3.
        ELSE.
          PERFORM convert_reason_for_db USING    gt_out-kzust3
                                                 gt_out-wertn3
                                        CHANGING l_kzust3.
        ENDIF.

        SELECT COUNT(*) INTO l_cnt1
          FROM ztcou106
         WHERE kokrs = p_kokrs
           AND bdatj = p_year
           AND kalka = p_kalka
           AND id    = gt_out-id
           AND poper = p_poper
           AND compn = gt_out-compn.

        IF l_cnt1 > 0.
          IF NOT l_kzust1 IS INITIAL.
            UPDATE ztcou106 SET kzust1 = l_kzust1
                                wertn1 = gt_out-wertn1
                                aedat  = sy-datum
                                aenam  = sy-uname
                       WHERE kokrs = p_kokrs
                         AND bdatj = p_year
                         AND kalka = p_kalka
                         AND id = gt_out-id
                         AND poper = p_poper
                         AND compn = gt_out-compn.
            IF sy-subrc <> 0.
              l_chk = 'X'.
            ENDIF.
          ENDIF.


          IF NOT l_kzust2 IS INITIAL.
            UPDATE ztcou106 SET kzust2 = l_kzust2
                                wertn2 = gt_out-wertn2
                                aedat  = sy-datum
                                aenam  = sy-uname
                       WHERE kokrs = p_kokrs
                         AND bdatj = p_year
                         AND kalka = p_kalka
                         AND id = gt_out-id
                         AND poper = p_poper
                         AND compn = gt_out-compn.
            IF sy-subrc <> 0.
              l_chk = 'X'.
            ENDIF.
          ENDIF.

          IF NOT l_kzust3 IS INITIAL.
            UPDATE ztcou106 SET kzust3 = l_kzust3
                                wertn3 = gt_out-wertn3
                                aedat  = sy-datum
                                aenam  = sy-uname
                       WHERE kokrs = p_kokrs
                         AND bdatj = p_year
                         AND kalka = p_kalka
                         AND id = gt_out-id
                         AND poper = p_poper
                         AND compn = gt_out-compn.
            IF sy-subrc <> 0.
              l_chk = 'X'.
            ENDIF.
          ENDIF.

          IF l_chk = space.
            l_cnt = l_cnt + l_cnt1.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDLOOP.

  IF l_ecnt = 0.
    MESSAGE s000 WITH 'Data has been saved successfully (' l_cnt ').'.
  ELSE.
    IF l_cnt = 0.
      MESSAGE s000 WITH 'Saving was failed:' l_ecnt 'records.'.
    ELSE.
      MESSAGE s000 WITH 'Saved:' l_cnt 'Failed:' l_ecnt.
    ENDIF.
  ENDIF.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  flag_data_changed = true.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE,
        l_name(20).

  FIELD-SYMBOLS <fs> TYPE ANY.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    READ TABLE gt_out INDEX ls_mod_cells-row_id.

    IF sy-subrc = 0.
      IF ls_mod_cells-fieldname+0(5) = 'KZUST' OR
         ls_mod_cells-fieldname+0(5) = 'WERTN'.

        CALL METHOD rr_data_changed->modify_cell
                EXPORTING i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
        CLEAR l_name.
        CONCATENATE 'GT_OUT-' ls_mod_cells-fieldname INTO l_name.
        ASSIGN (l_name) TO <fs>.
        <fs> = ls_mod_cells-value.

        MODIFY gt_out INDEX ls_mod_cells-row_id.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       Setting for layout
*----------------------------------------------------------------------*
FORM set_lvc_layout.
  gs_layo-edit       = 'X'.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-sel_mode   = 'A'.           " Column and row selection
  gs_layo-stylefname = 'CELLTAB'.
  gs_layo-ctab_fname = 'TABCOLOR'.    " CELL COLOR
ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
FORM create_alv_control.
  IF g_custom_container IS INITIAL.
*   Create object
    PERFORM create_object.

*   Exclude toolbar
    PERFORM exclude_functions USING 'GT_EXCLUDE'.

*   Create field category
    PERFORM create_field_category.

* by IG.MOON 9/18/2007 {
    PERFORM sort_build USING gt_sort[].
*}
*   Setting for layout
    PERFORM set_lvc_layout.

*   Define possible entry fields
    PERFORM create_f4_fields.

*   Define editable field
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

    CALL METHOD g_grid->register_edit_event
         EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*   Setting for event
    PERFORM set_event.

*   Define cell attribute
    PERFORM build_cell_attr.

*   Define variant
    gs_variant-report = sy-repid.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].

  ENDIF.

ENDFORM.                    " CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
*&      Form  CONVERT_REASON_FOR_SELECTION
*&---------------------------------------------------------------------*
*       Convert Reason for select from Table ZTCOU106
*       (ex. P1 -> PD1 : Current Price < Prv.Price
*                  PE1 : Current Price = Prv.Price
*                  PU1 : Current Price > Prv.Price)
*----------------------------------------------------------------------*
FORM convert_reason_for_selection.
  CLEAR r_kzust.
  REFRESH r_kzust.

  LOOP AT s_rsn.
    IF s_rsn-low+0(1) <> 'X'.
      IF s_rsn-high IS INITIAL.
        r_kzust-sign = 'I'.
        r_kzust-option = 'EQ'.
        CONCATENATE s_rsn-low+0(1) 'U' s_rsn-low+1(1) INTO r_kzust-low.
        APPEND r_kzust.

        CONCATENATE s_rsn-low+0(1) 'D' s_rsn-low+1(1) INTO r_kzust-low.
        APPEND r_kzust.

        CONCATENATE s_rsn-low+0(1) 'E' s_rsn-low+1(1) INTO r_kzust-low.
        APPEND r_kzust.

      ELSE.
        r_kzust-sign = 'I'.
        r_kzust-option = 'BT'.
        CONCATENATE s_rsn-low+0(1) 'U' s_rsn-low+1(1) INTO r_kzust-low.
        CONCATENATE s_rsn-high+0(1) 'U' s_rsn-high+1(1) INTO s_rsn-high.
        APPEND r_kzust.

        CONCATENATE s_rsn-low+0(1) 'D' s_rsn-low+1(1) INTO r_kzust-low.
        CONCATENATE s_rsn-high+0(1) 'D' s_rsn-high+1(1) INTO s_rsn-high.
        APPEND r_kzust.

        CONCATENATE s_rsn-low+0(1) 'E' s_rsn-low+1(1) INTO r_kzust-low.
        CONCATENATE s_rsn-high+0(1) 'E' s_rsn-high+1(1) INTO s_rsn-high.
        APPEND r_kzust.

      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CONVERT_REASON_FOR_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CONVERT_REASON_FOR_DB
*&---------------------------------------------------------------------*
*       Convert Reason for Table ZTCOU106
*       (ex. P1 -> PD1 : Current Price < Prv.Price
*                  PE1 : Current Price = Prv.Price
*                  PU1 : Current Price > Prv.Price)
*----------------------------------------------------------------------*
*  <--  P_OKZUST  Reason
*  <--  P_WERTN   Price
*  <--  P_NKZUST  Reason for DB
*----------------------------------------------------------------------*
FORM convert_reason_for_db USING    p_okzust TYPE zkzust1
                                    p_wertn  TYPE zwertn1_1
                           CHANGING p_nkzust TYPE zkzust1.

  DATA lv_op.

  CHECK NOT p_okzust IS INITIAL.
  CLEAR lv_op.

  CLEAR lv_op.
  IF p_wertn = 0.
    lv_op = 'E'.
  ELSEIF p_wertn > 0.
    lv_op = 'U'.
  ELSE.
    lv_op = 'D'.
  ENDIF.

  CONCATENATE p_okzust+0(1) lv_op p_okzust+1(1) INTO p_nkzust.

ENDFORM.                    " CONVERT_REASON_FOR_DB
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       Define possible entries
*----------------------------------------------------------------------*
FORM on_f4 USING sender         TYPE REF TO cl_gui_alv_grid
                 e_fieldname    TYPE lvc_fname
                 e_fieldvalue   TYPE lvc_value
                 es_row_no      TYPE lvc_s_roid
                 er_event_data  TYPE REF TO cl_alv_event_data
                 et_bad_cells   TYPE lvc_t_modi
                 e_display      TYPE char01.

  DATA lt_f4 TYPE TABLE OF ddshretval.

* Call my personal f4-help
  CALL METHOD g_event_receiver->my_f4
    EXPORTING
      sender        = sender
      es_row_no     = es_row_no
      er_event_data = er_event_data
      et_bad_cells  = et_bad_cells
      e_display     = e_display
      e_fieldname   = e_fieldname
    IMPORTING
      lt_f4         = lt_f4.

* Assign the cell table fieldsymbol to the dereferenced data table and
* fill the table.
  ASSIGN er_event_data->m_data->* TO <f4tab>.

  READ TABLE lt_f4 INTO ls_f4 INDEX 1.

  CHECK NOT ls_f4 IS INITIAL.

  PERFORM f4_aply USING es_row_no-row_id
                        ls_f4-fieldname.

* To avoid standard f4-help.
  er_event_data->m_event_handled = 'X'.

ENDFORM.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  MY_F4
*&---------------------------------------------------------------------*
FORM my_f4 TABLES et_f4         STRUCTURE ddshretval
           USING  sender        TYPE REF TO cl_gui_alv_grid
                  et_bad_cells  TYPE lvc_t_modi
                  es_row_no     TYPE lvc_s_roid
                  er_event_data TYPE REF TO cl_alv_event_data
                  e_display     TYPE c
                  e_fieldname   TYPE lvc_fname
                  p_tab.

  DATA : ls_out        LIKE LINE OF gt_out,
         lt_fcat       TYPE lvc_t_fcat,
         ls_fieldcat   TYPE lvc_s_fcat,
         lv_tabname    TYPE dd03v-tabname,
         lv_fieldname  TYPE dd03v-fieldname,
         lv_help_value TYPE help_info-fldvalue,
         lt_bad_cell   TYPE lvc_t_modi,
         l_wa          TYPE REF TO data.

  FIELD-SYMBOLS : <l_field_value> TYPE ANY,
                  <ls_wa>         TYPE ANY.

  CALL METHOD sender->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = lt_fcat.

  READ TABLE gt_out INDEX es_row_no-row_id INTO ls_out.

  IF sy-subrc = 0.
    CREATE DATA l_wa LIKE LINE OF gt_out.
    ASSIGN l_wa->* TO <ls_wa>.
    <ls_wa> = ls_out.
  ENDIF.

  READ TABLE lt_fcat WITH KEY fieldname = e_fieldname
                     INTO ls_fieldcat.
  IF sy-subrc = 0.
    lv_tabname = ls_fieldcat-ref_table.
    lv_fieldname = ls_fieldcat-fieldname.

    ASSIGN COMPONENT ls_fieldcat-fieldname
                  OF STRUCTURE ls_out TO <l_field_value>.

    WRITE <l_field_value> TO lv_help_value.
  ENDIF.

  PERFORM f4_set IN PROGRAM bcalv_f4
                 USING sender
                       lt_fcat
                       lt_bad_cell
                       es_row_no-row_id
                       <ls_wa>.

  IF e_fieldname = 'INFRSN' OR
     e_fieldname = 'KZUST1' OR e_fieldname = 'KZUST2' OR
     e_fieldname = 'KZUST3'.
    PERFORM f4_reason USING e_fieldname.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = e_fieldname
       TABLES
            field_tab       = gt_fields
            value_tab       = gt_values
            return_tab      = et_f4[]
       EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                                                    " MY_F4
*&---------------------------------------------------------------------*
*&      Form  F4_APLY
*&---------------------------------------------------------------------*
FORM f4_aply USING  es_row_no_row_id
                    e_fieldname TYPE fieldname.
  ls_modi-row_id    = es_row_no_row_id.
  ls_modi-fieldname = e_fieldname.
  ls_modi-value     = ls_f4-fieldval.
  APPEND ls_modi TO <f4tab>.

  CASE e_fieldname.
    WHEN 'INFRSN'.
      gt_out-infrsn = ls_f4-fieldval.

    WHEN 'KZUST1'.
      gt_out-kzust1 = ls_f4-fieldval.

    WHEN 'KZUST2'.
      gt_out-kzust2 = ls_f4-fieldval.

    WHEN 'KZUST3'.
      gt_out-kzust3 = ls_f4-fieldval.
  ENDCASE.

  READ TABLE gt_out INDEX es_row_no_row_id.

ENDFORM.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*       Define possible entry fields
*----------------------------------------------------------------------*
FORM create_f4_fields.
  CLEAR: gs_f4, gt_f4, gt_f4[].

* F4 FIELD

  gs_f4-fieldname  = 'INFRSN'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  gs_f4-fieldname  = 'KZUST1'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  gs_f4-fieldname  = 'KZUST2'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.

  gs_f4-fieldname  = 'KZUST3'.
  gs_f4-register   = 'X'.
  APPEND gs_f4 TO gt_f4.


  CALL METHOD g_grid->register_f4_for_fields
         EXPORTING it_f4 = gt_f4.

ENDFORM.                    " CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       Setting Color
*----------------------------------------------------------------------*
FORM set_color.
  DATA: l_ecnt TYPE i,
        l_tot  TYPE zdwertn,
        l_idx  TYPE sytabix.

  CLEAR l_ecnt.

  LOOP AT gt_out.
    CLEAR: l_tot, l_idx.

    l_idx = sy-tabix.
    l_tot = gt_out-wertn1 + gt_out-wertn2 + gt_out-wertn3.

    IF gt_out-dwertn <> l_tot OR
      ( gt_out-kzust1 IS INITIAL AND NOT gt_out-wertn1 IS INITIAL ) OR
      ( gt_out-kzust2 IS INITIAL AND NOT gt_out-wertn2 IS INITIAL ) OR
      ( gt_out-kzust3 IS INITIAL AND NOT gt_out-wertn3 IS INITIAL ) OR
      ( NOT gt_out-kzust1 IS INITIAL AND gt_out-wertn1 IS INITIAL ) OR
      ( NOT gt_out-kzust2 IS INITIAL AND gt_out-wertn2 IS INITIAL ) OR
      ( NOT gt_out-kzust3 IS INITIAL AND gt_out-wertn3 IS INITIAL ).

      l_ecnt = l_ecnt + 1.
      gt_out-wchk = 'X'.

*     Setting Color
      CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

      gs_specialcol-color-col = cl_gui_resources=>list_col_negative.
      gs_specialcol-color-int = 0.

      APPEND gs_specialcol TO gt_specialcol.

      gt_out-tabcolor[] = gt_specialcol[].

    ELSE.
      CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].
    ENDIF.

    MODIFY gt_out INDEX l_idx TRANSPORTING wchk tabcolor.
  ENDLOOP.

  IF l_ecnt = 0.
    MESSAGE s000 WITH 'Rs$ is correct.'.
  ELSE.
    MESSAGE s000 WITH 'Error:' l_ecnt 'records. Check Rsn & Rs$.'.
  ENDIF.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  toolbar
*&---------------------------------------------------------------------*
FORM toolbar USING r_object TYPE REF TO cl_alv_event_toolbar_set
                            r_interactive.

  DATA ls_toolbar  TYPE stb_button.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = 3.
  APPEND ls_toolbar TO r_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-icon = icon_check.
  ls_toolbar-function = 'CHK'.
  APPEND ls_toolbar TO r_object->mt_toolbar.

ENDFORM.                    " toolbar
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm.
  CASE r_ucomm.
*   Check amount: if diff.price is defferent price sum(WERTN1+2+3)
*                 change color as pink of the line
    WHEN 'CHK'.
      PERFORM set_color.
      PERFORM refresh_field.
  ENDCASE.

ENDFORM.                    " 5000_user_command_part
