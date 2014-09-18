*&---------------------------------------------------------------------*
*& Program ID     : ZFMC0007                                           *
*& Program Name   : Enter The Release                                  *
*& Created by     : YN.Kim                                             *
*& Created on     : 08/18/2011                                         *
*& Reference Pgm  :                                                    *
*&                                                                     *
*& Modification Log                                                    *
*----------------------------------------------------------------------*
* DATE      |  NAME          |Transport | Issue #  |      DESC         *
*----------------------------------------------------------------------*
*                                                                      *
*&=====================================================================*
REPORT  ZFMC0007 NO STANDARD PAGE HEADING
                             MESSAGE-ID  zmfi
                             LINE-SIZE 255.

CLASS lcl_event_receiver DEFINITION DEFERRED.  "for event handling

type-pools zfmcm.
DATA: ok_code LIKE sy-ucomm,
      save_ok LIKE sy-ucomm,
      g_container TYPE scrfname VALUE 'CONT1',
      g_grid  TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_event_receiver TYPE REF TO lcl_event_receiver,
      gt_fieldcat TYPE lvc_t_fcat,
      gs_layout TYPE lvc_s_layo,
      g_max TYPE i VALUE 100.

TYPES: BEGIN OF gs_excel_p.
TYPES: checkbox TYPE c.                "field for checkbox
TYPES: celltab TYPE lvc_t_styl.        "field to switch editability
        INCLUDE STRUCTURE zsfm0012.
TYPES: END OF gs_excel_p.


DATA: excel_p TYPE gs_excel_p OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF it_bppe OCCURS 0,
       objnr LIKE bppe-objnr,
       posit LIKE bppe-posit,
       wtp01 LIKE bppe-wtp01,
       wtp02 LIKE bppe-wtp01,
       wtp03 LIKE bppe-wtp01,
       wtp04 LIKE bppe-wtp01,
       wtp05 LIKE bppe-wtp01,
       wtp06 LIKE bppe-wtp01,
       wtp07 LIKE bppe-wtp01,
       wtp08 LIKE bppe-wtp01,
       wtp09 LIKE bppe-wtp01,
       wtp10 LIKE bppe-wtp01,
       wtp11 LIKE bppe-wtp01,
       wtp12 LIKE bppe-wtp01,
       wtpxx LIKE bppe-wtp01,
       profil LIKE tbpfc-profil,
      END OF it_bppe.
DATA: it_bppe_a LIKE it_bppe OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_bppe_6 OCCURS 0,
       objnr LIKE bppe-objnr,
       posit LIKE bppe-posit,
       wtp01 LIKE bppe-wtp01,
       wtp02 LIKE bppe-wtp01,
       wtp03 LIKE bppe-wtp01,
       wtp04 LIKE bppe-wtp01,
       wtp05 LIKE bppe-wtp01,
       wtp06 LIKE bppe-wtp01,
       wtp07 LIKE bppe-wtp01,
       wtp08 LIKE bppe-wtp01,
       wtp09 LIKE bppe-wtp01,
       wtp10 LIKE bppe-wtp01,
       wtp11 LIKE bppe-wtp01,
       wtp12 LIKE bppe-wtp01,
       wtpxx LIKE bppe-wtp01,
       profil LIKE tbpfc-profil,
      END OF it_bppe_6.
DATA: it_bppe_6_a LIKE it_bppe_6 OCCURS 0 WITH HEADER LINE.

DATA:  f_fictr LIKE fmdy-fictr,
       f_fipex LIKE fmdy-fipex,
       f_val0  LIKE fmbpdy-val0,
       t(2)    TYPE c,
       c_excel_cnt TYPE i.
DATA: BEGIN OF gt_fld OCCURS 0,
        fieldname TYPE fieldname,
      END OF gt_fld,
       c_fictr(20),
       c_val0(20),
       c_fipex(20),
       p_budget_check(1),
       answer(1).

DATA: BEGIN OF it_bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdcdata.

DATA: it_bdcmsg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1  WITH FRAME TITLE  text-001.
PARAMETERS: p_fikrs LIKE ifmcidy-fikrs
            MEMORY ID fik OBLIGATORY,
            p_gjahr LIKE bpdy-gjahr
            MEMORY ID gjr OBLIGATORY,
            p_perio LIKE bpdy-perio,
            p_profil TYPE zbp_profil,
            p_mode  TYPE bdcmode DEFAULT 'N' NO-DISPLAY .
SELECTION-SCREEN END   OF BLOCK blk1.

***********************************************************************
* LOCAL CLASSES
***********************************************************************
*
* This local class only handles event DOUBLE_CLICK.
* Wenn the user double clicks on a checkbox cell the status of
* this cell is switched from editable to not editable and vice versa.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS: catch_doubleclick
             FOR EVENT double_click OF cl_gui_alv_grid
             IMPORTING
                e_column
                es_row_no
                sender.
ENDCLASS.                    "lcl_event_receiver DEFINITION


*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD catch_doubleclick.
    DATA: ls_outtab TYPE gs_excel_p.
    READ TABLE excel_p INTO ls_outtab INDEX es_row_no-row_id.
    CASE e_column.
      WHEN 'BELNR'.
        CHECK ls_outtab-belnr NE space.
        SET PARAMETER ID 'BPB' FIELD ls_outtab-belnr.
        CALL TRANSACTION 'FR60' AND SKIP FIRST SCREEN.
      WHEN 'FICTR'.
        SET PARAMETER ID 'FIK' FIELD p_fikrs.
        SET PARAMETER ID 'FIS' FIELD ls_outtab-fictr.
        CALL TRANSACTION 'FMSC' AND SKIP FIRST SCREEN.
      WHEN 'FIPEX'.
        SET PARAMETER ID 'FIK' FIELD p_fikrs.
        SET PARAMETER ID 'FPS' FIELD ls_outtab-fipex.
        SET PARAMETER ID 'GJR' FIELD p_gjahr.
        CALL TRANSACTION 'FMCIC' AND SKIP FIRST SCREEN.
      WHEN OTHERS.
        SET PARAMETER ID 'FIK' FIELD p_fikrs.
        SET PARAMETER ID 'FIS' FIELD ls_outtab-fictr.
        SET PARAMETER ID 'FIP' FIELD ls_outtab-fipex.
        SET PARAMETER ID 'GJR' FIELD p_gjahr.
        CALL TRANSACTION 'FR11' AND SKIP FIRST SCREEN.
    ENDCASE.

    CALL METHOD sender->refresh_table_display.
  ENDMETHOD.                    "catch_doubleclick
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*
*---------------------------------------------------------------------*
* INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.
  p_perio = sy-datum+4(2).
  SET PARAMETER ID 'FIK' FIELD zfmcm_fm_area.
*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*

END-OF-SELECTION.
  CALL SCREEN 100.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAIN100' WITH '/'.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.
  ENDIF.

ENDMODULE.                    "pbo OUTPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE pai INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT' or 'BACK' or 'CANC'.
      PERFORM exit_program.
    WHEN 'SELECT'.
      PERFORM select_all_entries CHANGING excel_p[].
      PERFORM enable_disable TABLES excel_p[].
    WHEN 'DESELECT'.
      PERFORM deselect_all_entries CHANGING excel_p[].
      PERFORM enable_disable TABLES excel_p[].
    WHEN 'POST'.
      PERFORM bdc_posting_processing_rel CHANGING excel_p[].
      PERFORM enable_disable TABLES excel_p[].


*    WHEN 'RESET'.
*      PERFORM reset_selected_entries CHANGING excel_p[].
*    WHEN 'SWITCH'.
*      PERFORM switch_activation CHANGING excel_p[].
  ENDCASE.
ENDMODULE.                    "pai INPUT
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM exit_program.
  LEAVE TO SCREEN 0. "PROGRAM.
ENDFORM.                    "exit_program
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA ls_fcat TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSFM0012'                         "'ZFIT0027'
    CHANGING
      ct_fieldcat      = pt_fieldcat.

*§A2.Add an entry for the checkbox in the fieldcatalog
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'CHECKBOX'.
* Essential: declare field as checkbox and
*            mark it as editable field:
  ls_fcat-checkbox = 'X'.
  ls_fcat-edit = 'X'.

* do not forget to provide texts for this extra field
  ls_fcat-coltext = text-f01.
  ls_fcat-tooltip = text-f02.
  ls_fcat-seltext = text-f03.

* optional: set column width
  ls_fcat-outputlen = 02.
*
  APPEND ls_fcat TO pt_fieldcat.

  LOOP AT pt_fieldcat INTO ls_fcat WHERE fieldname = 'VAL0'.
    ls_fcat-edit = 'X'.
*   modify pt_fieldcat.
    MODIFY pt_fieldcat INDEX sy-tabix FROM ls_fcat
           TRANSPORTING edit.
  ENDLOOP.

ENDFORM.                    "build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*      <--P_GT_FIELDCAT  text
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM create_and_init_alv.

  DATA: lt_exclude TYPE ui_functions.

  CREATE OBJECT g_custom_container
    EXPORTING
      container_name = g_container.
  CREATE OBJECT g_grid
    EXPORTING
      i_parent = g_custom_container.

  PERFORM build_fieldcat CHANGING gt_fieldcat.

* Exclude all edit functions in this example since we do not need them:
  PERFORM exclude_tb_functions CHANGING lt_exclude.

  PERFORM build_data.

*§ B3.Use the layout structure to aquaint additional field to ALV.

  gs_layout-stylefname = 'CELLTAB'.
  gs_layout-no_rowmark = 'X'.
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layout
      it_toolbar_excluding = lt_exclude
      i_save               = 'A'
    CHANGING
      it_fieldcatalog      = gt_fieldcat
      it_outtab            = excel_p[].

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->catch_doubleclick FOR g_grid.

* Set editable cells to ready for input initially
  CALL METHOD g_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.


ENDFORM.                               "CREATE_AND_INIT_ALV

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCLUDE  text
*----------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.

  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO pt_exclude.


ENDFORM.                               " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  build_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_data.

  DATA: ls_celltab TYPE lvc_s_styl,
        lt_celltab TYPE lvc_t_styl,
        l_index TYPE i.
  IF excel_p[] IS INITIAL.
    PERFORM budget_existence_confirmation.

  ENDIF.
**  ls_celltab-fieldname = 'CHECKBOX'.
**  ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.

  DATA : lt_fmfctrt LIKE fmfctrt OCCURS 0 WITH HEADER LINE.
  DATA : lt_fmcit LIKE fmcit OCCURS 0 WITH HEADER LINE.
  SELECT * FROM fmfctrt
           INTO TABLE lt_fmfctrt
           WHERE spras = sy-langu
             AND fikrs = p_fikrs.
  SELECT * FROM fmcit
           INTO TABLE lt_fmcit
           WHERE spras = sy-langu
             AND fikrs = p_fikrs.
  LOOP AT excel_p.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    IF excel_p-belnr IS INITIAL.
      ls_celltab-fieldname = 'CHECKBOX'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

      ls_celltab-fieldname = 'VAL0'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_celltab INTO TABLE lt_celltab.
    ELSE.
      excel_p-checkbox = ''.
      ls_celltab-fieldname = 'CHECKBOX'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

      ls_celltab-fieldname = 'VAL0'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

    ENDIF.
    INSERT lines of lt_celltab INTO TABLE excel_p-celltab.
    READ TABLE lt_fmfctrt WITH KEY fictr = excel_p-fictr.
    IF sy-subrc EQ 0.
      excel_p-bezeich = lt_fmfctrt-bezeich.
    ENDIF.
    READ TABLE lt_fmcit WITH KEY fipex = excel_p-fipex.
    IF sy-subrc EQ 0.
      excel_p-bezei = lt_fmcit-bezei.
    ENDIF.
    MODIFY excel_p INDEX l_index.
  ENDLOOP.

ENDFORM.                               " build_data
*&---------------------------------------------------------------------*
*&      Form  select_all_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*----------------------------------------------------------------------*
FORM select_all_entries CHANGING pt_outtab TYPE STANDARD TABLE.
  DATA: ls_outtab TYPE gs_excel_p.
  DATA: l_valid TYPE c,
        l_locked TYPE c,
        l_tcnt TYPE i.

*§A4ad. Before you (a)set, reset or (d)evaluate checkboxes,
*       you must check the input cells.
*
* If all entries are ok, ALV transferes new values to the output
* table which you then can modify.

  CALL METHOD g_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.

  IF l_valid EQ 'X'.

    LOOP AT pt_outtab INTO ls_outtab.
      IF ls_outtab-belnr IS INITIAL.
        PERFORM check_lock USING    ls_outtab
                           CHANGING l_locked.
        IF l_locked IS INITIAL
           AND NOT ls_outtab-checkbox EQ '-'.
          ls_outtab-checkbox = 'X'.
        ENDIF.
      ENDIF.
      MODIFY pt_outtab FROM ls_outtab.
    ENDLOOP.
*    CALL METHOD g_grid->refresh_table_display.
  ENDIF.

ENDFORM.                               " select_all_entries
*&---------------------------------------------------------------------*
*&      Form  deselect_all_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB[]  text
*----------------------------------------------------------------------*
FORM deselect_all_entries CHANGING pt_outtab TYPE STANDARD TABLE.
  DATA: ls_outtab TYPE gs_excel_p.
  DATA: l_valid TYPE c,
        l_locked TYPE c.


  CALL METHOD g_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.

  IF l_valid EQ 'X'.

    LOOP AT pt_outtab INTO ls_outtab.
      PERFORM check_lock USING    ls_outtab
                       CHANGING l_locked.
      IF l_locked IS INITIAL
         AND NOT ls_outtab-checkbox EQ '-'.
        ls_outtab-checkbox = ' '.
      ENDIF.

      MODIFY pt_outtab FROM ls_outtab.
    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display.

  ENDIF.



ENDFORM.                               " deselect_all_entries
*&---------------------------------------------------------------------*
*&      Form  reset_selected_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB[]  text
*----------------------------------------------------------------------*
FORM reset_selected_entries CHANGING pt_outtab TYPE STANDARD TABLE.
  DATA: ls_outtab TYPE gs_excel_p.
  DATA: l_valid TYPE c.


  CALL METHOD g_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.

  IF l_valid EQ 'X'.

    LOOP AT pt_outtab INTO ls_outtab.
      IF     NOT ls_outtab-checkbox IS INITIAL
         AND NOT ls_outtab-checkbox EQ '-'.
        CLEAR ls_outtab.
        MODIFY pt_outtab FROM ls_outtab.
      ENDIF.
    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display.

  ENDIF.

ENDFORM.                               " reset_selected_entries

*&---------------------------------------------------------------------*
*&      Form  switch_activation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB[]  text
*----------------------------------------------------------------------*

FORM switch_activation CHANGING pt_outtab TYPE STANDARD TABLE.
  DATA: ls_outtab TYPE gs_excel_p.
  DATA: l_valid TYPE c,
        lt_row_no TYPE lvc_t_roid WITH HEADER LINE.


  CALL METHOD g_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.

  IF l_valid EQ 'X'.
    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_row_no = lt_row_no[].

    LOOP AT lt_row_no.
      READ TABLE pt_outtab INTO ls_outtab INDEX lt_row_no-row_id.
      IF ls_outtab-checkbox NE '-'.
        ls_outtab-checkbox = '-'.
      ELSE.
        ls_outtab-checkbox = ' '.
      ENDIF.
      MODIFY pt_outtab FROM ls_outtab INDEX lt_row_no-row_id.

    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display.

  ENDIF.

ENDFORM.                               " switch_activation

*&---------------------------------------------------------------------*
*&      Form  check_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUTTAB  text
*      <--P_L_LOCKED  text
*----------------------------------------------------------------------*
FORM check_lock USING    ps_outtab TYPE gs_excel_p
                CHANGING p_locked.
  DATA ls_celltab TYPE lvc_s_styl.

  LOOP AT ps_outtab-celltab INTO ls_celltab.
    IF ls_celltab-fieldname = 'CHECKBOX'.
      IF ls_celltab-style EQ cl_gui_alv_grid=>mc_style_disabled.
        p_locked = 'X'.
      ELSE.
        p_locked = space.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " check_lock

*&---------------------------------------------------------------------*
*&      Form  budget_existence_confirmation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM budget_existence_confirmation .
  CLEAR: excel_p, excel_p[].
  PERFORM field_define.

  SELECT (gt_fld) FROM bppe
                  INTO TABLE it_bppe_a
                  WHERE wrttp =  '43'
                    AND gjahr = p_gjahr
                    AND versn = zfmcm_versn_0
                    AND trgkz = 'N'
                    AND geber EQ space.

  SELECT *  FROM bppe
            INTO CORRESPONDING FIELDS OF TABLE it_bppe_6_a
            WHERE wrttp =  '46'
              AND gjahr = p_gjahr
              AND versn = zfmcm_versn_0
              AND trgkz = 'N'
              AND geber EQ space.

  LOOP AT it_bppe_a.
    CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
      EXPORTING
        i_posit         = it_bppe_a-posit
        i_objnr         = it_bppe_a-objnr
        i_gjahr         = p_gjahr
        i_geber         = ' '
        i_farea         = ' '
      IMPORTING
        e_profil        = it_bppe_a-profil
      EXCEPTIONS
        no_profil_found = 1
        OTHERS          = 2.

    CHECK p_profil = it_bppe_a-profil.
    it_bppe = it_bppe_a.

    COLLECT it_bppe.
  ENDLOOP.

  LOOP AT it_bppe_6_a.
    CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
      EXPORTING
        i_posit         = it_bppe_6_a-posit
        i_objnr         = it_bppe_6_a-objnr
        i_gjahr         = p_gjahr
        i_geber         = ' '
        i_farea         = ' '
      IMPORTING
        e_profil        = it_bppe_6_a-profil
      EXCEPTIONS
        no_profil_found = 1
        OTHERS          = 2.

    CHECK p_profil = it_bppe_6_a-profil.
    it_bppe_6_a-wtpxx = it_bppe_6_a-wtp01 + it_bppe_6_a-wtp02 +
                      it_bppe_6_a-wtp03 + it_bppe_6_a-wtp04 +
                      it_bppe_6_a-wtp05 + it_bppe_6_a-wtp06 +
                      it_bppe_6_a-wtp07 + it_bppe_6_a-wtp08 +
                      it_bppe_6_a-wtp09 + it_bppe_6_a-wtp10 +
                      it_bppe_6_a-wtp11 + it_bppe_6_a-wtp12.
    it_bppe_6 = it_bppe_6_a.
    COLLECT it_bppe_6.
  ENDLOOP.

  SORT it_bppe BY objnr posit profil.
  SORT it_bppe_6 BY objnr posit profil.
  CLEAR: it_bppe, it_bppe_6.
  LOOP AT it_bppe.
    excel_p-profil = it_bppe-profil.
    READ TABLE it_bppe_6 WITH KEY objnr = it_bppe-objnr
                                  posit = it_bppe-posit
                                  profil = it_bppe-profil
                                  BINARY SEARCH.
    MOVE: it_bppe-objnr+6(16) TO excel_p-fictr.
    CALL FUNCTION 'FM_FIPEX_GET_FROM_POSIT'
      EXPORTING
        i_fikrs = p_fikrs
        i_posit = it_bppe-posit
      IMPORTING
        e_fipex = excel_p-fipex.

    CLEAR : t.
    CASE p_profil.
      WHEN 'Y'.
        t = 0.
        DO 12 TIMES.
          PERFORM perio_data.
        ENDDO.
      WHEN 'H'.
        IF p_perio <= 6.
          t = 0.
        ELSE.
          t = 6.
        ENDIF.
        DO 6 TIMES.
          PERFORM perio_data.
        ENDDO.
      WHEN 'Q'.
        IF p_perio <= 3.
          t = 0.
        ELSEIF p_perio <= 6.
          t = 3.
        ELSEIF p_perio <= 9.
          t = 6.
        ELSEIF p_perio <= 12.
          t = 9.
        ENDIF.
        DO 3 TIMES.
          PERFORM perio_data.
        ENDDO.
      WHEN 'M'.
        t = p_perio - 1.
        DO 1 TIMES.
          PERFORM perio_data.
        ENDDO.
      WHEN OTHERS.
        t = p_perio - 1.
        DO 1 TIMES.
          PERFORM perio_data.
        ENDDO.
    ENDCASE.
    CLEAR: excel_p, it_bppe, it_bppe_6.
  ENDLOOP.

  DESCRIBE TABLE excel_p LINES c_excel_cnt.

ENDFORM.                    " budget_existence_confirmation
*&---------------------------------------------------------------------*
*&      Form  field_define
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_define .
  REFRESH : gt_fld. CLEAR: gt_fld.
  gt_fld-fieldname = 'OBJNR'.
  APPEND gt_fld.
  gt_fld-fieldname = 'POSIT'.
  APPEND gt_fld.

  CLEAR: t.
  DO 12 TIMES.
    t = t + 1.
    IF t < 10.
      CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
    ELSE.
      CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
    ENDIF.
  ENDDO.

ENDFORM.                    " field_define
*&---------------------------------------------------------------------*
*&      Form  perio_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM perio_data .
  DATA: p_val0 LIKE excel_p-val0.
  t = t + 1.
*  excel_p-fictr = excel-fictr.
*  excel_p-fipex = excel-fipex.
*  excel_p-profil = it_bppe-profil.
  excel_p-perio = t.
  CASE t.
    WHEN 1.
      it_bppe-wtpxx = it_bppe-wtp01.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp01.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp01.
      ELSEIF p_val0 > 0.
*        IF p_val0 >= it_bppe-wtp01.
        IF it_bppe-wtp01 = it_bppe_6-wtp01.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp01 > it_bppe_6-wtp01.
          excel_p-val0 = it_bppe-wtp01 - it_bppe_6-wtp01.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
*        ELSE.
*
**          excel_p-val0 = p_val0.
*        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp01 - it_bppe_6-wtp01.
      ENDIF.
    WHEN 2.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp02.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp02.
      ELSEIF p_val0 > 0.
*        IF p_val0 >= it_bppe-wtp02.
        IF it_bppe-wtp02 = it_bppe_6-wtp02.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp02 > it_bppe_6-wtp02.
          excel_p-val0 = it_bppe-wtp02 - it_bppe_6-wtp02.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
*        ELSE.
*          excel_p-val0 = p_val0.
*        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp02 - it_bppe_6-wtp02.
      ENDIF.
    WHEN 3.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp03.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp03.
      ELSEIF p_val0 > 0.
*        IF p_val0 >= it_bppe-wtp03.
        IF it_bppe-wtp03 = it_bppe_6-wtp03.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp03 > it_bppe_6-wtp03.
          excel_p-val0 = it_bppe-wtp03 - it_bppe_6-wtp03.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
*        ELSE.
*          excel_p-val0 = p_val0.
*        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp03 - it_bppe_6-wtp03.
      ENDIF.
    WHEN 4.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 .
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp04.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp04.
      ELSEIF p_val0 > 0.
*        IF p_val0 >= it_bppe-wtp04.
        IF it_bppe-wtp04 = it_bppe_6-wtp04.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp04 > it_bppe_6-wtp04.
          excel_p-val0 = it_bppe-wtp04 - it_bppe_6-wtp04.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
*        ELSE.
*          excel_p-val0 = p_val0.
*        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp04 - it_bppe_6-wtp04.
      ENDIF.
    WHEN 5.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp05.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp05.
      ELSEIF p_val0 > 0.
*        IF p_val0 >= it_bppe-wtp05.
        IF it_bppe-wtp05 = it_bppe_6-wtp05.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp05 > it_bppe_6-wtp05.
          excel_p-val0 = it_bppe-wtp05 - it_bppe_6-wtp05.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
*        ELSE.
*          excel_p-val0 = p_val0.
*        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp05 - it_bppe_6-wtp05.
      ENDIF.
    WHEN 6.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05 + it_bppe-wtp06.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp06.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp06.
      ELSEIF p_val0 > 0.
        IF it_bppe-wtp06 = it_bppe_6-wtp06.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp06 > it_bppe_6-wtp06.
          excel_p-val0 = it_bppe-wtp06 - it_bppe_6-wtp06.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp06 - it_bppe_6-wtp06.
      ENDIF.
    WHEN 7.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05 + it_bppe-wtp06 +
                      it_bppe-wtp07.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp07.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp07.
      ELSEIF p_val0 > 0.
        IF it_bppe-wtp07 = it_bppe_6-wtp07.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp07 > it_bppe_6-wtp07.
          excel_p-val0 = it_bppe-wtp07 - it_bppe_6-wtp07.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp07 - it_bppe_6-wtp07.
      ENDIF.
    WHEN 8.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05 + it_bppe-wtp06 +
                      it_bppe-wtp07 + it_bppe-wtp08.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp08.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp08.
      ELSEIF p_val0 > 0.
        IF it_bppe-wtp08 = it_bppe_6-wtp08.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp08 > it_bppe_6-wtp08.
          excel_p-val0 = it_bppe-wtp08 - it_bppe_6-wtp08.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp08 - it_bppe_6-wtp08.
      ENDIF.
    WHEN 9.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05 + it_bppe-wtp06 +
                      it_bppe-wtp07 + it_bppe-wtp08 +
                      it_bppe-wtp09.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp03.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp09.
      ELSEIF p_val0 > 0.
        IF it_bppe-wtp09 = it_bppe_6-wtp09.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp09 > it_bppe_6-wtp09.
          excel_p-val0 = it_bppe-wtp09 - it_bppe_6-wtp09.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp09 - it_bppe_6-wtp09.
      ENDIF.
    WHEN 10.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05 + it_bppe-wtp06 +
                      it_bppe-wtp07 + it_bppe-wtp08 +
                      it_bppe-wtp09 + it_bppe-wtp10.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp10.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp10.
      ELSEIF p_val0 > 0.
        IF it_bppe-wtp10 = it_bppe_6-wtp10.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp10 > it_bppe_6-wtp10.
          excel_p-val0 = it_bppe-wtp10 - it_bppe_6-wtp10.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp10 - it_bppe_6-wtp10.
      ENDIF.
    WHEN 11.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05 + it_bppe-wtp06 +
                      it_bppe-wtp07 + it_bppe-wtp08 +
                      it_bppe-wtp09 + it_bppe-wtp10 +
                      it_bppe-wtp11.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp11.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp11.
      ELSEIF p_val0 > 0.
        IF it_bppe-wtp11 = it_bppe_6-wtp11.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp11 > it_bppe_6-wtp11.
          excel_p-val0 = it_bppe-wtp11 - it_bppe_6-wtp11.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp11 - it_bppe_6-wtp11.
      ENDIF.
    WHEN 12.
      it_bppe-wtpxx = it_bppe-wtp01 + it_bppe-wtp02 +
                      it_bppe-wtp03 + it_bppe-wtp04 +
                      it_bppe-wtp05 + it_bppe-wtp06 +
                      it_bppe-wtp07 + it_bppe-wtp08 +
                      it_bppe-wtp09 + it_bppe-wtp10 +
                      it_bppe-wtp11 + it_bppe-wtp12.
      p_val0  = it_bppe-wtpxx - it_bppe_6-wtpxx.
      p_val0 = p_val0 - it_bppe-wtp12.
      IF p_val0 = 0.
        excel_p-val0 = it_bppe-wtp12.
      ELSEIF p_val0 > 0.
*        IF p_val0 >= it_bppe-wtp12.
        IF it_bppe-wtp12 = it_bppe_6-wtp12.
          excel_p-val0 = 0.
        ELSEIF it_bppe-wtp12 > it_bppe_6-wtp12.
          excel_p-val0 = it_bppe-wtp12 - it_bppe_6-wtp12.
        ELSE.
          excel_p-val0 = 0.
        ENDIF.
*        ELSE.
*          excel_p-val0 = p_val0.
*        ENDIF.
      ELSE.
        excel_p-val0 = it_bppe-wtp12 - it_bppe_6-wtp12.
      ENDIF.
  ENDCASE.
  IF excel_p-val0 > 0.
    it_bppe_6-wtpxx = it_bppe_6-wtpxx + excel_p-val0.
    APPEND excel_p.
  ENDIF.

ENDFORM.                    " perio_data
*&---------------------------------------------------------------------*
*&      Form  enable_disable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_EXCEL_P[]  text
*----------------------------------------------------------------------*
FORM enable_disable  TABLES pt_outtab .
  DATA: ls_outtab TYPE gs_excel_p.
  DATA: ls_celltab TYPE lvc_s_styl,
        lt_celltab TYPE lvc_t_styl,
        l_index TYPE i.

  LOOP AT pt_outtab INTO ls_outtab.
    l_index = sy-tabix.
    CHECK ls_outtab-belnr NE space.
    ls_outtab-checkbox = ' '.
    LOOP AT ls_outtab-celltab INTO ls_celltab.
      IF ls_celltab-fieldname EQ 'CHECKBOX' OR
         ls_celltab-fieldname EQ 'VAL0'.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        MODIFY ls_outtab-celltab FROM ls_celltab.
      ENDIF.
    ENDLOOP.

    MODIFY pt_outtab FROM ls_outtab INDEX l_index.
  ENDLOOP.
  CALL METHOD g_grid->refresh_table_display.

ENDFORM.                    " enable_disable
*&---------------------------------------------------------------------*
*&      Form  bdc_posting_processing_rel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXCEL_P[]  text
*----------------------------------------------------------------------*
FORM bdc_posting_processing_rel  CHANGING pt_outtab TYPE STANDARD TABLE.
  DATA: ls_outtab TYPE gs_excel_p.
  DATA: l_valid TYPE c,
        l_locked TYPE c.
  CALL METHOD g_grid->check_changed_data
    IMPORTING
      e_valid = l_valid.

  IF l_valid EQ 'X'.

    LOOP AT pt_outtab INTO ls_outtab.
      CHECK ls_outtab-checkbox = 'X'.
      CHECK ls_outtab-belnr EQ space.
      CHECK ls_outtab-val0 > 0.
      IF it_bdcdata[] IS INITIAL.
        PERFORM butget_bdc_h USING ls_outtab-perio.
      ENDIF.

      PERFORM butget_bdc USING ls_outtab-fictr
                               ls_outtab-fipex
                               ls_outtab-val0.

      PERFORM butget_bdc_e.

      CALL TRANSACTION  'FR51'     USING          it_bdcdata
                                   MODE           p_mode
                                   UPDATE         'S'
                                   MESSAGES INTO  it_bdcmsg.
      READ TABLE it_bdcmsg WITH KEY msgtyp  = 'S'
                                    msgnr   = '043'.
      IF sy-subrc EQ 0.
        ls_outtab-belnr = it_bdcmsg-msgv1.
      ENDIF.
      REFRESH: it_bdcdata,it_bdcmsg.
      CLEAR:  it_bdcdata,it_bdcmsg.
      MODIFY pt_outtab FROM ls_outtab.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " bdc_posting_processing_rel
*&---------------------------------------------------------------------*
*&      Form  butget_bdc_h
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_OUTTAB  text
*----------------------------------------------------------------------*
FORM butget_bdc_h  USING    p_perio.
  PERFORM dynpro USING:
       'X' 'SAPLKBPB'        '0200',
       ' ' 'BDC_OKCODE'      '/ESFIK'.
*  macro_bdc_dynpro   :  'SAPLKBPB'        '0200'.
*  macro_bdc_field    :  'BDC_OKCODE'      '/ESFIK'.
  PERFORM dynpro USING:
       'X' 'SAPLFMCI1'        '0100',
       ' ' 'BDC_OKCODE'      '=OK',
       ' ' 'IFMCIDY-FIKRS'   p_fikrs.
*  macro_bdc_dynpro   :  'SAPLFMCI1'       '0100'.
*  macro_bdc_field    :  'BDC_OKCODE'      '=OK',
*                        'IFMCIDY-FIKRS'   p_fikrs.
*

  PERFORM dynpro USING:
       'X' 'SAPLKBPB'        '0200',
       ' ' 'BDC_OKCODE'      '/00',
       ' ' 'FMDY-FIKRS'      p_fikrs,
       ' ' 'BPDY-GJAHR'      p_gjahr,
       ' ' 'BPDY-PERIO'      p_perio,
       ' ' 'BPDY-WGES'       'X'.

*  macro_bdc_dynpro   :  'SAPLKBPB'        '0200'.
*  macro_bdc_field    :  'BDC_OKCODE'      '/00',
*                        'FMDY-FIKRS'      p_fikrs,
*                        'BPDY-GJAHR'      p_gjahr,
*                        'BPDY-PERIO'      p_perio,
*                        'BPDY-WGES'       'X'.

ENDFORM.                    " butget_bdc_h
*&---------------------------------------------------------------------*
*&      Form  butget_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUTTAB  text
*----------------------------------------------------------------------*
FORM butget_bdc  USING    p_fictr
                          p_fipex
                          p_val0.
  DATA: bdc_wrbtr(16).
  CONCATENATE 'FMDY-FICTR(' '01' ')'  INTO c_fictr.
  CONCATENATE 'FMBPDY-VAL0(' '01' ')' INTO c_val0.
  CONCATENATE 'FMDY-FIPEX(' '01' ')'  INTO c_fipex.

  WRITE:  p_val0 CURRENCY  zfmcm_euro
                           TO  bdc_wrbtr.
  PERFORM dynpro USING:
   'X' 'SAPLKBPB'        '0400',
   ' ' 'BDC_OKCODE'      '/00',
   ' ' c_fictr           p_fictr,
   ' ' c_val0            bdc_wrbtr,
   ' ' c_fipex           p_fipex.



*  macro_bdc_dynpro   :  'SAPLKBPB'        '0400'.
*  macro_bdc_field    :  'BDC_OKCODE'      '/00',
*                         c_fictr           p_fictr,
*                         c_val0            bdc_wrbtr,
*                         c_fipex           p_fipex.

ENDFORM.                    " butget_bdc
*&---------------------------------------------------------------------*
*&      Form  butget_bdc_e
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUTTAB  text
*----------------------------------------------------------------------*
FORM butget_bdc_e.
  PERFORM dynpro USING:
    'X' 'SAPLKBPB' '0400',
    ' ' 'BDC_OKCODE'      '=POST'.
*  macro_bdc_dynpro   :  'SAPLKBPB'        '0400'.
*  macro_bdc_field    :  'BDC_OKCODE'      '=POST'.

ENDFORM.                    " butget_bdc_e
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.

  IF dynbegin = 'X'.
    CLEAR it_bdcdata.
    MOVE: name TO it_bdcdata-program,
          value TO it_bdcdata-dynpro,
          dynbegin TO it_bdcdata-dynbegin.
    APPEND it_bdcdata.
  ELSE.
    CLEAR it_bdcdata.
    MOVE: name TO it_bdcdata-fnam,
          value TO it_bdcdata-fval.
    APPEND it_bdcdata.
  ENDIF.

ENDFORM.                    " dynpro
