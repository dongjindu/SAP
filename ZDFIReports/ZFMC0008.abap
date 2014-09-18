*&---------------------------------------------------------------------*
*& Program ID     : ZFMC0008                                           *
*& Program Name   : Enter The Release ( - ) : Perio                    *
*& Created by     : YN.Kim                                             *
*& Created on     : 08/18/2011                                         *
*& Reference Pgm  :                                                    *
*&                                                                     *
*& Modification Log                                                    *
*----------------------------------------------------------------------*
*   DATE     |  Developer   |     Description(Reason)                  *
*----------------------------------------------------------------------*
* 2011.08.11 |                                                         *
*&=====================================================================*

**********************************************************************
report  ZFMC0008     no standard page heading
*                     message-id  zmfi
                     line-size 76.

type-pools zfmcm.

CLASS lcl_event_receiver DEFINITION DEFERRED.  "for event handling

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
        include structure ZSFM0013.
TYPES: END OF gs_excel_p.

DATA: excel_p TYPE gs_excel_p OCCURS 0 WITH HEADER LINE.
DATA: excel_t LIKE ZSFM0013 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_bppe OCCURS 0,
       fictr LIKE ZSFM0013-fictr,
       fipex LIKE ZSFM0013-fipex,
       profil LIKE tbpfc-profil,
       objnr LIKE bppe-objnr,
       posit LIKE bppe-posit,
       wlp01 LIKE bppe-wlp01,
       wlp02 LIKE bppe-wlp01,
       wlp03 LIKE bppe-wlp01,
       wlp04 LIKE bppe-wlp01,
       wlp05 LIKE bppe-wlp01,
       wlp06 LIKE bppe-wlp01,
       wlp07 LIKE bppe-wlp01,
       wlp08 LIKE bppe-wlp01,
       wlp09 LIKE bppe-wlp01,
       wlp10 LIKE bppe-wlp01,
       wlp11 LIKE bppe-wlp01,
       wlp12 LIKE bppe-wlp01,
       wlpxx LIKE bppe-wlp01,
      END OF it_bppe.
DATA: it_bppe_a LIKE it_bppe OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF it_fmit OCCURS 0,
       rfistl LIKE fmit-rfistl,
       rfipex LIKE fmit-rfipex,
*       rwrttp LIKE fmit-rwrttp,
*       hslvt LIKE fmit-hsl01,
       hsl01 LIKE fmit-hsl01,
       hsl02 LIKE fmit-hsl01,
       hsl03 LIKE fmit-hsl01,
       hsl04 LIKE fmit-hsl01,
       hsl05 LIKE fmit-hsl01,
       hsl06 LIKE fmit-hsl01,
       hsl07 LIKE fmit-hsl01,
       hsl08 LIKE fmit-hsl01,
       hsl09 LIKE fmit-hsl01,
       hsl10 LIKE fmit-hsl01,
       hsl11 LIKE fmit-hsl01,
       hsl12 LIKE fmit-hsl01,
       hslxx LIKE fmit-hsl01,
      END OF it_fmit.
DATA: it_fmit_a LIKE it_fmit OCCURS 0 WITH HEADER LINE.

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
DATA: rspar TYPE TABLE OF rsparams,
      wa_rspar LIKE LINE OF rspar.

data: begin of it_bdcdata occurs 0.
        include structure bdcdata.
data: end of it_bdcdata.

data: it_bdcmsg like bdcmsgcoll occurs 0 with header line.

********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1  WITH FRAME TITLE  text-001.
PARAMETERS: p_fikrs LIKE ifmcidy-fikrs DEFAULT zfmcm_fm_area
            MEMORY ID fik OBLIGATORY,
            p_gjahr LIKE bpdy-gjahr DEFAULT sy-datum(4)
            MEMORY ID gjr OBLIGATORY,
            p_perio LIKE bpdy-perio OBLIGATORY,
            p_profil LIKE tbp1c-profil OBLIGATORY,
            p_mode  TYPE bdcmode DEFAULT 'N' NO-DISPLAY.
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
        CHECK ls_outtab-BELNR NE SPACE.
        SET PARAMETER ID 'BPB' FIELD ls_outtab-BELNR.
        CALL TRANSACTION 'FR60' AND SKIP FIRST SCREEN.
      WHEN 'FICTR'.
        SET PARAMETER ID 'FIK' FIELD p_fikrs.
        SET PARAMETER ID 'FIS' FIELD ls_outtab-fictr.
        CALL TRANSACTION 'FMSC' AND SKIP FIRST SCREEN.
      WHEN 'FIPEX'.
        SET PARAMETER ID 'FIK' FIELD p_fikrs.
        SET PARAMETER ID 'FPS' FIELD ls_outtab-fipex.
        SET PARAMETER ID 'GJR' FIELD ls_outtab-gjahr.
        CALL TRANSACTION 'FMCIC' AND SKIP FIRST SCREEN.
      when 'VAL_P'.
          refresh : rspar.
          wa_rspar-selname = 'S_FICTR'. wa_rspar-kind = 'S'.
          wa_rspar-sign = 'I'. wa_rspar-option = 'BT'.
          wa_rspar-low  = ls_outtab-fictr.   wa_rspar-high = ls_outtab-fictr.
          APPEND wa_rspar TO rspar.
          wa_rspar-selname = 'S_FIPEX'. wa_rspar-kind = 'S'.
          wa_rspar-sign = 'I'. wa_rspar-option = 'BT'.
          wa_rspar-low  = ls_outtab-fipex.   wa_rspar-high = ls_outtab-fipex.
          APPEND wa_rspar TO rspar.
          wa_rspar-selname = 'P_FIKRS'. wa_rspar-kind = 'P'.
          wa_rspar-low  = p_fikrs.
          APPEND wa_rspar TO rspar.

          wa_rspar-selname = 'P_GJAHR'. wa_rspar-kind = 'P'.
          wa_rspar-low  = ls_outtab-gjahr.
          APPEND wa_rspar TO rspar.
          wa_rspar-selname = 'P_VARNT'. wa_rspar-kind = 'P'.
          wa_rspar-low  = '000'.
          APPEND wa_rspar TO rspar.

          wa_rspar-selname = 'S_GJAHR'. wa_rspar-kind = 'S'.
          wa_rspar-sign = 'I'. wa_rspar-option = 'BT'.
          wa_rspar-low  = ls_outtab-gjahr.   wa_rspar-high = ls_outtab-gjahr.
          APPEND wa_rspar TO rspar.
          wa_rspar-selname = 'P_BUDGT'. wa_rspar-kind = 'P'.
          wa_rspar-low  = ' '.
          APPEND wa_rspar TO rspar.
          wa_rspar-selname = 'P_RELES'. wa_rspar-kind = 'P'.
          wa_rspar-low  = 'X'.
          APPEND wa_rspar TO rspar.
          wa_rspar-selname = 'P_NOWARN'. wa_rspar-kind = 'P'.
          wa_rspar-low  = 'X'.
          APPEND wa_rspar TO rspar.

          SUBMIT rffmep30x WITH SELECTION-TABLE rspar
                           AND RETURN.
      WHEN OTHERS.
        SET PARAMETER ID 'FIK' FIELD P_FIKRS.
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
  SET TITLEBAR 'MAIN100'.
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
  DATA: answer(1),
        lv_subrc1 LIKE sy-subrc.
  LOOP AT excel_p WHERE belnr NE space.
    MOVE-CORRESPONDING excel_p TO excel_t.
    APPEND excel_t.
  ENDLOOP.
  IF NOT excel_t[] IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption  = 'Y'
        textline1      = text-p02
        textline2      = text-p01
        titel          = text-p01
        cancel_display = ' '
      IMPORTING
        answer         = answer
      EXCEPTIONS
        OTHERS         = 01.
    IF answer = 'J'.
      SET UPDATE TASK LOCAL.
*      MODIFY  ZtFM0013   FROM TABLE excel_t.
      lv_subrc1  =  sy-subrc.
      IF  lv_subrc1  =  0 .
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
  ENDIF.
  LEAVE TO SCREEN 0.
*  LEAVE PROGRAM.

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
      i_structure_name = 'ZSFM0013'
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
  ls_fcat-tooltip = text-f01.
  ls_fcat-seltext = text-f01.

* optional: set column width
  ls_fcat-outputlen = 02.
*
  APPEND ls_fcat TO pt_fieldcat.
  CLEAR : ls_fcat.

  ls_fcat-no_out = 'X'.
  MODIFY pt_fieldcat FROM ls_fcat
         TRANSPORTING no_out WHERE fieldname = 'GJAHR'.

  CLEAR : ls_fcat.
  ls_fcat-coltext = text-f04.
  ls_fcat-tooltip = text-f04.
  ls_fcat-seltext = text-f04.
  ls_fcat-no_out = 'X'.

  MODIFY pt_fieldcat FROM ls_fcat
         TRANSPORTING coltext tooltip seltext no_out
         WHERE fieldname = 'ZNUM'.

*  LOOP AT pt_fieldcat INTO ls_fcat WHERE fieldname = 'VAL0'.
*    ls_fcat-edit = 'X'.
*    MODIFY pt_fieldcat INDEX sy-tabix FROM ls_fcat
*           TRANSPORTING edit.
*  ENDLOOP.

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
         EXPORTING container_name = g_container.
  CREATE OBJECT g_grid
         EXPORTING i_parent = g_custom_container.

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

  LOOP AT excel_p.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    IF excel_p-val_r <= 0.
      excel_p-checkbox = ''.
      ls_celltab-fieldname = 'CHECKBOX'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

      ls_celltab-fieldname = 'VAL0'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.
    ELSEIF excel_p-val0 >= 0.
      excel_p-checkbox = ''.
      ls_celltab-fieldname = 'CHECKBOX'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

      ls_celltab-fieldname = 'VAL0'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.
    ELSEIF excel_p-belnr IS INITIAL.
      ls_celltab-fieldname = 'CHECKBOX'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

      ls_celltab-fieldname = 'VAL0'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT ls_celltab INTO TABLE lt_celltab.
    ELSEIF NOT excel_p-belnr IS INITIAL.
      excel_p-checkbox = ''.
      ls_celltab-fieldname = 'CHECKBOX'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

      ls_celltab-fieldname = 'VAL0'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE lt_celltab.

    ENDIF.
    INSERT LINES OF lt_celltab INTO TABLE excel_p-celltab.
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
  DATA: p_num TYPE int4,
        p_val0 type p decimals 2.
  CLEAR: excel_p, excel_p[].

***?? ??? ??
  PERFORM field_define USING 'B'.
  SELECT (gt_fld) FROM bppe
                  INTO CORRESPONDING FIELDS OF TABLE it_bppe_a
                  WHERE wrttp =  '46'
                    AND gjahr = p_gjahr
                    AND versn = zfmcm_versn_0
                    AND trgkz = 'N'
                    and geber eq space.


  CHECK NOT it_bppe_a[] IS INITIAL.

  LOOP AT it_bppe_a.
    it_bppe_a-fictr = it_bppe_a-objnr+6(16).
    CALL FUNCTION 'FM_FIPEX_GET_FROM_POSIT'
      EXPORTING
        i_fikrs = p_fikrs
        i_posit = it_bppe_a-posit
      IMPORTING
        e_fipex = it_bppe_a-fipex.

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
    it_bppe_a-wlpxx = it_bppe_a-wlp01 + it_bppe_a-wlp02 +
                      it_bppe_a-wlp03 + it_bppe_a-wlp04 +
                      it_bppe_a-wlp05 + it_bppe_a-wlp06 +
                      it_bppe_a-wlp06 + it_bppe_a-wlp08 +
                      it_bppe_a-wlp09 + it_bppe_a-wlp10 +
                      it_bppe_a-wlp11 + it_bppe_a-wlp12.
    it_bppe = it_bppe_a.
    COLLECT it_bppe.
  ENDLOOP.


***???? ??.
  PERFORM field_define USING 'F'.
  SELECT (gt_fld)  FROM fmit
           INTO CORRESPONDING FIELDS OF TABLE it_fmit_a
           FOR ALL ENTRIES IN it_bppe
           WHERE rldnr = '9A'
             AND rvers = zfmcm_versn_0
             AND ryear = p_gjahr
             AND rbukrs = p_fikrs
             AND rfistl = it_bppe-fictr
             AND rfipex = it_bppe-fipex
             and RFONDS eq space
             AND rstats NE 'X'.
  LOOP AT it_fmit_a.
    it_fmit_a-hslxx = it_fmit_a-hsl01 + it_fmit_a-hsl02 +
                      it_fmit_a-hsl03 + it_fmit_a-hsl04 +
                      it_fmit_a-hsl05 + it_fmit_a-hsl06 +
                      it_fmit_a-hsl06 + it_fmit_a-hsl08 +
                      it_fmit_a-hsl09 + it_fmit_a-hsl10 +
                      it_fmit_a-hsl11 + it_fmit_a-hsl12.
    it_fmit = it_fmit_a.
    COLLECT it_fmit.
  ENDLOOP.


  SORT it_bppe BY fictr fipex.
  SORT it_fmit BY rfistl rfipex.
  CLEAR: it_bppe, it_fmit.
*  SELECT MAX( znum ) FROM ZtSFM0013
*                     INTO p_num
*                     WHERE gjahr = p_gjahr.
*  IF p_num IS INITIAL.
*    p_num = 1.
*  ELSE.
*    p_num = p_num + 1.
*  ENDIF.

  LOOP AT it_bppe.
    excel_p-gjahr = p_gjahr.
    excel_p-znum  = p_num.
    excel_p-fictr = it_bppe-fictr.
    excel_p-fipex = it_bppe-fipex.
    excel_p-profil = it_bppe-profil.
    CASE p_profil.
      WHEN 'Y'.
        excel_p-perio = 12.
      WHEN 'H'.
        IF p_perio <= 6.
          excel_p-perio = 6.
        ELSE.
          excel_p-perio = 12.
        ENDIF.
      WHEN 'Q'.
        IF p_perio <= 3.
          excel_p-perio = 3.
        ELSEIF p_perio <= 6.
          excel_p-perio = 6.
        ELSEIF p_perio <= 9.
          excel_p-perio = 9.
        ELSE.
          excel_p-perio = 12.
        ENDIF.
      WHEN OTHERS.
        excel_p-perio = p_perio.
    ENDCASE.

    excel_p-val_r = it_bppe-wlpxx.
    READ TABLE it_fmit WITH KEY rfistl = it_bppe-fictr
                                rfipex = it_bppe-fipex
                                BINARY SEARCH.

    excel_p-val_p = it_fmit-hslxx * -1.
*    p_val0 = excel_p-val_r.
*    IF p_val0 > 0.
    IF excel_p-val_r > 0.
      excel_p-val0 = excel_p-val_p - excel_p-val_r.
    ELSE.
      excel_p-val0 = 0.
    ENDIF.
    APPEND excel_p.
    CLEAR: it_bppe, it_fmit, excel_p.
  ENDLOOP.

ENDFORM.                    " budget_existence_confirmation
*&---------------------------------------------------------------------*
*&      Form  field_define
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_define USING p_field.
  REFRESH : gt_fld. CLEAR: gt_fld.
  IF p_field = 'B'.
    gt_fld-fieldname = 'OBJNR'.
    APPEND gt_fld.
    gt_fld-fieldname = 'POSIT'.
    APPEND gt_fld.

    CASE p_profil.
      WHEN 'Y'.
        CLEAR: t.
        DO 12 TIMES.
          t = t + 1.
          IF t < 10.
            CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
          ELSE.
            CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
          ENDIF.
        ENDDO.
      WHEN 'H'.
        CLEAR: t.
        IF p_perio <= 6.
          t = 0.
          DO 6 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSE.
          t = 6.
          DO 6 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.

        ENDIF.
      WHEN 'Q'.
        CLEAR: t.
        IF p_perio <= 3.
          t = 0.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSEIF p_perio <= 6.
          t = 3.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSEIF p_perio <= 9.
          t = 6.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSE.
          t = 9.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'wlp' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ENDIF.
      WHEN OTHERS.
        CLEAR: t.
        t = p_perio+1(2).
        CONCATENATE 'wlp' t INTO gt_fld. APPEND gt_fld.
    ENDCASE.

  ELSE.
    gt_fld-fieldname = 'RFISTL'.
    APPEND gt_fld.
    gt_fld-fieldname = 'RFIPEX'.
    APPEND gt_fld.
    CASE p_profil.
      WHEN 'Y'.
        CLEAR: t.
        DO 12 TIMES.
          t = t + 1.
          IF t < 10.
            CONCATENATE 'hsl' '0' t INTO gt_fld. APPEND gt_fld.
          ELSE.
            CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
          ENDIF.
        ENDDO.
      WHEN 'H'.
        CLEAR: t.
        IF p_perio <= 6.
          t = 0.
          DO 6 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'hsl' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSE.
          t = 6.
          DO 6 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'hsl' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.

        ENDIF.
      WHEN 'Q'.
        CLEAR: t.
        IF p_perio <= 3.
          t = 0.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'hsl' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSEIF p_perio <= 6.
          t = 3.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'hsl' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSEIF p_perio <= 9.
          t = 6.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'hsl' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ELSE.
          t = 9.
          DO 3 TIMES.
            t = t + 1.
            IF t < 10.
              CONCATENATE 'hsl' '0' t INTO gt_fld. APPEND gt_fld.
            ELSE.
              CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
            ENDIF.
          ENDDO.
        ENDIF.
      WHEN OTHERS.
        CLEAR: t.
        t = p_perio+1(2).
        CONCATENATE 'hsl' t INTO gt_fld. APPEND gt_fld.
    ENDCASE.

  ENDIF.
ENDFORM.                    " field_define

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
      CHECK ls_outtab-val0 < 0.
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

  perform dynpro using:
       'X' 'SAPLKBPB'        '0200',
       ' ' 'BDC_OKCODE'      '/ESFIK'.

*  macro_bdc_dynpro   :  'SAPLKBPB'        '0200'.
*  macro_bdc_field    :  'BDC_OKCODE'      '/ESFIK'.

  perform dynpro using:
       'X' 'SAPLFMCI1'        '0100',
       ' ' 'BDC_OKCODE'      '=OK',
       ' ' 'IFMCIDY-FIKRS'   p_fikrs.

*  macro_bdc_dynpro   :  'SAPLFMCI1'       '0100'.
*  macro_bdc_field    :  'BDC_OKCODE'      '=OK',
*                        'IFMCIDY-FIKRS'   p_fikrs.


  perform dynpro using:
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

  perform dynpro using:
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
  perform dynpro using:
    'X' 'SAPLKBPB' '0400',
    ' ' 'BDC_OKCODE'      '=POST'.

ENDFORM.                    " butget_bdc_e
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
form dynpro using dynbegin name value.

  if dynbegin = 'X'.
    clear it_bdcdata.
    move: name to it_bdcdata-program,
          value to it_bdcdata-dynpro,
          dynbegin to it_bdcdata-dynbegin.
    append it_bdcdata.
  else.
    clear it_bdcdata.
    move: name to it_bdcdata-fnam,
          value to it_bdcdata-fval.
    append it_bdcdata.
  endif.

endform.                    " dynpro
