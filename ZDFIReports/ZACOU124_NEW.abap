*----------------------------------------------------------------------
* Program ID        : ZACOU124_NEW
* Title             : [CO] Duty ME Clearing(New)
* Created on        : 05/10/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Duty ME Reconciliation
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request    Description
* 03/03/2012 T00266     UD1K950699 Modify new Process
*----------------------------------------------------------------------
*
* [input]  => [ Calc.&ALV display ] => [Output]
*                                       Call Invoice Posting BAPI
*                                      'BAPI_INCOMINGINVOICE_CREATE'
*        +----------------+
*       /     BSIS       /
*      +----------------+
*
*       #FTZ I/F Table Header         # Log Table(C)
*        +----------------+           +--------------+
*       /  ZTMM_DUTY_HD  /           /   ZTCOU124_NEW   /
*      +----------------+           +--------------+
*
*       #FTZ I/F Table Item       #FTZ I/F Table Header(U)
*        +----------------+           +----------------+
*       /  ZTMM_DUTY_IT  /        /  ZTMM_DUTY_HD  /<-Cleared?
*      +----------------+      +----------------+
*
*       #PO                #FTZ I/F Table Item(U)

*        +----------------+        +----------------+
*       /      EKBZ      /        /  ZTMM_DUTY_IT  /<-CL_DOC_NO
*      +----------------+      +----------------+
*        +----------------+
*       /      EKPO      /
*      +----------------+
*
* Entry Type - 06/01
*   Read DDLC account line item (table: BSIS)
*      - Read duty entry summary(06), Entry# ( Total duty$ )
*      - Read duty summary(01), BOL, Entry# ( Total duty$ )
*
* Entry Type - 06/08
*   Read FTZ I/F table ( ZTMM_DUTY_HD , ZTMM_DUTY_IT )
*      - Posting date (parameter)
*      - Entry summary data (06/08), Entry#
*
*----------------------------------------------------------------------
REPORT zacou124_new MESSAGE-ID zmco.

INCLUDE : zacou124_new_top.
INCLUDE : zacoui00.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:   p_bukrs  TYPE bukrs OBLIGATORY MEMORY ID buk,
              p_kschl  TYPE kschl OBLIGATORY
              MEMORY ID vks DEFAULT 'ZOA1',
              p_hkont LIKE bsis-hkont DEFAULT '215011',
              p_entry LIKE ztmm_duty_hd-fentp OBLIGATORY DEFAULT '01'.
SELECT-OPTIONS : s_entno  FOR ztmm_duty_it-entno MODIF ID mat,
                 s_matnr  FOR ekpo-matnr MODIF ID mat MEMORY ID mat,
                 s_ebeln  FOR ekpo-ebeln MEMORY ID bes,
                 s_entrd  FOR ztmm_duty_hd-entdate .

PARAMETERS : p_expo AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

* Load saved data
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-011.
PARAMETERS p_load AS CHECKBOX USER-COMMAND ucom MODIF ID lod.
SELECT-OPTIONS s_date FOR sy-datum MODIF ID lod DEFAULT sy-datum.
PARAMETERS p_incr AS CHECKBOX MODIF ID lod DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS : p_date LIKE sy-datum OBLIGATORY DEFAULT sy-datum
                                  MODIF ID pst,
             p_max(3)  TYPE n DEFAULT 10 MODIF ID pst,
             p_w_off LIKE bsis-hkont OBLIGATORY DEFAULT '532160'
             MODIF ID pst .

SELECTION-SCREEN END OF BLOCK b2..

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-022.
PARAMETER p_mx_r TYPE i DEFAULT '50' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b5.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
  CASE sscrfields-ucomm.
    WHEN 'UCOM'.
      PERFORM modify_screen.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kschl.
  PERFORM kschl_input_help CHANGING p_kschl.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

** Temp blocking the program run for further investigate
** on 03/20/13
  MESSAGE e000 with 'Program is blockig for use, please contact IT'.
** End
* Gather row data
  PERFORM get_data.
  CHECK g_error EQ space .
  CALL SCREEN 100.

END-OF-SELECTION.
* nothing

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'POST'.
      PERFORM post_data.
      PERFORM save_data.
    WHEN 'FIXLOG'.
      gt_out-chk = true.
      MODIFY gt_out TRANSPORTING chk WHERE chk EQ space.
      PERFORM save_data.
      gt_out-chk = false.
      MODIFY gt_out TRANSPORTING chk WHERE chk EQ true .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.

  CLEAR g_error.

  IF p_load EQ true .
    PERFORM load_saved_data.           " Load saved data
    EXIT.
  ENDIF.
* Duty entry type
  CASE p_entry.
    WHEN '06'.
* Get bsis, ztmm_duty_it, ZTCOU124_NEW, rbkp
      PERFORM get_row_data_06_new.
      CHECK g_error EQ space.
* Get ekpo, ekbz --> data from PO table
      PERFORM : get_row_data_ekbz_06_08_new,
                refine_itab_06_08.
    WHEN '08'.
* Get ztmm_duty_it, ZTCOU124_NEW, rbkp
      PERFORM get_row_data_08.
      CHECK g_error EQ space.
* Get ekpo, ekbz --> data from PO table
      PERFORM : get_row_data_ekbz_06_08_new,
                refine_itab_06_08.
    WHEN '01' .
* Get bsis
      PERFORM get_row_data_01.
      CHECK g_error EQ space.
* Get ekpo, ekbz --> data from PO table
      PERFORM : get_row_data_ekbz_01,
                refine_itab_01.
  ENDCASE.

  PERFORM : calc_data,
            adjust_and_dist_amount.

  PERFORM   get_gt_out.

  __process 'Prepare Screen...' '98'.                      "98%

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.        " Column heading
    gs_fcat-outputlen     = &4.        " Column width
    gs_fcat-datatype      = &5.        " Data type
    gs_fcat-emphasize     = &6.
*    GS_FCAT-DO_SUM        = &7.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
          'X'  'ENTNO'   'Entry #'          20  'CHAR' '',
          'X'  'MATNR'   'Material'         18  'CHAR' ''.

  IF p_load NE space.
    __catalog :
            'X'  'BELUM'   'Acct.doc.#'       10  'CHAR' ''.
  ENDIF.

  __catalog :
          'X'  'XMENGES' 'Qty'              13  'CHAR' '',
          'X'  'DTAMTS'  'Duty'             13  'CHAR' ''.
  IF p_entry NE '01'.
    __catalog :
          'X'  'IVAMTS'  'Invoice'          13  'CHAR' ''.
  ENDIF.
  __catalog :
          'X'  'EBELN'   'P/O No.'          10  'CHAR' '',
          'X'  'EBELP'   'Item'              5  'CHAR' '',
          ' '  'PMENGE'  'P/O Qty.   '      13  'QUAN' '',
          ' '  'GMENGE'  'G/R Qty.   '      13  'QUAN' '',
          ' '  'BMENGE'  'Balance    '      13  'QUAN' '',
          ' '  'RMENGE'  'Clr.Qty.'         13  'QUAN' '',
          ' '  'AMENGE'  'Qty.Aftr'         13  'QUAN' '',
          ' '  'PERCENTS' 'Duty%'            10  'CHAR' '',
*          ' '  'DTYUPS'  'Dty.U/P'          10  'CHAR' '',
          ' '  'DMBTR'   'Clr.Amt.'         13  'CURR' '',
          ' '  'DSTAMT'  'Dst.Amt.'         13  'CURR' '',
          ' '  'ACCUP'   'Acc.U/P'          13  'CHAR' '',
          ' '  'ACCAMT'  'Acc.Amt.'         13  'CURR' '', " new
          ' '  'VARAMT'  'Var.Amt.'         13  'CURR' '', " new
          ' '  'HKONT'   'GL.Acct.'         10  'CHAR' '',
          ' '  'ICON'    'Clr'               3  'ICON' ''.
  IF p_load EQ space.
    __catalog :
            ' '  'BELUM'   'Acct.doc.#'       10  'CHAR' ''.
  ENDIF.
  __catalog :
          ' '  'BAL_TR'  ''                1   'CHAR' '',
          ' '  'BAL_RV'  ''                10  'CHAR' '',
          ' '  'MSG'  'Remarks                          .'
          30  'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'BAL_TR' OR 'BAL_RV' .
        gs_fcat-no_out = 'X'.
        MODIFY gt_fcat FROM gs_fcat.
      WHEN 'XMENGES' OR 'XMENGE2S' OR  'DTAMTS' OR 'IVAMTS' .
        gs_fcat-just = 'R'.
        MODIFY gt_fcat FROM gs_fcat.
      WHEN 'BMENGE' OR 'RMENGE' OR 'PMENGE' OR 'AMENGE'
                    OR 'GMENGE' .
        gs_fcat-qfieldname = 'MEINS'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'EKPO'.
        gs_fcat-just = 'R'.
        MODIFY gt_fcat FROM gs_fcat.
      WHEN 'ICON'.
        gs_fcat-just = 'C'.
        MODIFY gt_fcat FROM gs_fcat.
      WHEN 'DMBTR' OR 'ACCUP' OR 'DSTAMT' OR 'ACCAMT' OR 'VARAMT'.
        gs_fcat-qfieldname = 'WAERS'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        gs_fcat-ref_table = 'EKBZ'.
        gs_fcat-just = 'R'.
        MODIFY gt_fcat FROM gs_fcat.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY

*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
             'ENTNO'    ' ' 'X' 'X' 'X',
             'MATNR'    ' ' 'X' 'X' 'X',
             'XMENGES'  ' ' 'X' 'X' 'X',
             'DTAMTS'   ' ' 'X' 'X' 'X'.
  IF p_entry NE '01'.
    sort_tab 'IVAMTS'   ' ' 'X' 'X' 'X'.
  ENDIF.
  sort_tab :
             'EBELN'    ' ' 'X' 'X' 'X',
             'BAL_TR'  ' ' ' ' 'X' 'X',
             'BAL_RV'  ' ' 'X' 'X' 'X',
             'PERCENTS'  ' ' 'X' 'X' 'X',
*             'DTYUPS'   ' ' 'X' 'X' 'X',
*             'XMENGE2S' ' ' 'X' 'X' 'X',
             'HKONT'    ' ' 'X' 'X' 'X'.


  IF p_load EQ true.
    sort_tab 'BELUM'   ' ' 'X' 'X' 'X'.
  ENDIF.

ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       Save data to table ZTCOU105
*----------------------------------------------------------------------*
FORM save_data.

  DATA : flag1(1),flag2(1),
        $dmbtr LIKE gt_out-dmbtr.

  LOOP AT gt_out WHERE chk EQ true.

    AT END OF matnr.
      flag1 = true.
    ENDAT.
    ADD gt_out-dmbtr TO $dmbtr.

    AT END OF entno.
      flag2 = true.
    ENDAT.

    MOVE-CORRESPONDING gt_out TO *ztcou124_new.
     *ztcou124_new-fentp = p_entry.
     *ztcou124_new-kschl = p_kschl.
     *ztcou124_new-pdate = p_date.
     *ztcou124_new-erdat = sy-datum.
     *ztcou124_new-ernam = sy-uname.

    IF flag1 EQ true AND gt_out-belum NE space.
      PERFORM update_ftz_if_item.
      COMMIT WORK .
      CLEAR flag1.
    ENDIF.

    IF flag2 EQ true.
      IF gt_out-duty_amt EQ $dmbtr.
        PERFORM update_ftz_if_header USING 'C'.
      ELSEIF $dmbtr <> 0.
        PERFORM update_ftz_if_header USING 'B'.
      ENDIF.
      COMMIT WORK .
      CLEAR : flag2,$dmbtr.
    ENDIF.

  ENDLOOP.

  PERFORM refresh_alv.
  __focus g_grid.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
FORM set_lvc_layout.
  CLEAR gs_layo.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
FORM exclude_functions.
  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.
*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      CHANGING
        it_outtab            = gt_out[]
        it_fieldcatalog      = gt_fcat[]
        it_sort              = gt_sort[].
  ENDIF.
  __focus g_grid.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.
*  CLEAR: GS_SPECIALCOL, GT_SPECIALCOL[], GT_OUT-TABCOLOR[].
*
*  DEFINE __COLOR.
*    GS_SPECIALCOL-FIELDNAME = &1 .
*    GS_SPECIALCOL-COLOR-COL = &2 .
*    GS_SPECIALCOL-COLOR-INT = &3 .
*    APPEND GS_SPECIALCOL TO GT_SPECIALCOL .
*  END-OF-DEFINITION.
*
** COLOR
*  __COLOR : 'RMENGE' '6' 0,
*            'DTYUPS' '5' 0,
*            'DMBTR'  '7' 0.
*  GT_OUT-TABCOLOR[] = GT_SPECIALCOL[].
*  MODIFY GT_OUT TRANSPORTING TABCOLOR WHERE TABCOLOR IS INITIAL.
ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv.
  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = stable.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.
*  CLEAR GV_INDEX.
*  GV_INDEX = E_ROW-INDEX.
*
*  READ TABLE GT_OUT INDEX GV_INDEX.
*  IF SY-SUBRC = 0.
*    IF E_COLUMN = 'BELUM'.
*      CHECK GT_OUT-BELUM NE SPACE.
*      SET PARAMETER ID : 'RBN'  FIELD GT_OUT-BELUM,
*                         'GJR'  FIELD P_DATE(4).
*      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
*    ENDIF.
*    IF E_COLUMN = 'MATNR'.
*      CHECK GT_OUT-MATNR NE SPACE.
*      SET PARAMETER ID 'MAT'  FIELD GT_OUT-MATNR.
*      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
*    ENDIF.
*    IF E_COLUMN = 'EBELN'.
*      CHECK GT_OUT-EBELN NE SPACE.
*      SET PARAMETER ID 'BES'  FIELD GT_OUT-EBELN.
*      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*    ENDIF.
*  ENDIF.
*
*  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS EXPORTING CONTROL = G_GRID.
*
ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.
*   Create object
  PERFORM create_object.

*   Exclude toolbar
  PERFORM exclude_functions.

  IF p_load EQ true.
    SET PF-STATUS '100'.
  ELSE.
    SET PF-STATUS '100' EXCLUDING 'FIXLOG'.
  ENDIF.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET :
    HANDLER g_event_receiver->handle_data_changed FOR g_grid,
    HANDLER g_event_receiver->handle_double_click FOR g_grid.

*   Create field category
  PERFORM :
      create_field_category USING false,
      sort_build USING gt_sort[],
      set_lvc_layout,
      set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.
  LOOP AT SCREEN.
    IF screen-group1 = 'PST'.
      screen-input = 1.
      screen-invisible = 0.
    ENDIF.
    CASE 'X'.
      WHEN p_load.
        IF screen-group1 = 'PST'.
          screen-input = 0.
          screen-invisible  = 1.
        ENDIF.
*      WHEN P_O_MAT.
*        IF SCREEN-GROUP1 = 'PST'.
*          SCREEN-INPUT = 0.
*          SCREEN-INVISIBLE  = 1.
*        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  get_data_from_dkbz_dkpo_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data_06_new.

  __process 'Read BSIS...' '30'.

  __cls : sum_i_bsis, i_bsis.

  DATA $sum_i_bsis LIKE sum_i_bsis OCCURS 0 WITH HEADER LINE.

* bsis
  SELECT bukrs hkont sgtxt AS fentp gjahr belnr
         dmbtr shkzg zuonr
     INTO  CORRESPONDING FIELDS OF TABLE sum_i_bsis
     FROM  bsis
     WHERE bukrs EQ p_bukrs
      AND  hkont EQ p_hkont
      AND  sgtxt EQ p_entry
      AND  zuonr IN s_entno.

  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    EXIT.
  ENDIF.

  DATA $ix TYPE i.

  LOOP AT sum_i_bsis.
    $ix = sy-tabix.
    IF sum_i_bsis-shkzg EQ 'H'.
      sum_i_bsis-dmbtr = -1 * sum_i_bsis-dmbtr.
      CLEAR sum_i_bsis-shkzg.
      MODIFY sum_i_bsis INDEX $ix TRANSPORTING shkzg dmbtr.
    ENDIF.
  ENDLOOP.

  LOOP AT sum_i_bsis.
    $sum_i_bsis = sum_i_bsis.
    COLLECT $sum_i_bsis.
  ENDLOOP.

  __cls sum_i_bsis.
  sum_i_bsis[] = $sum_i_bsis[].

  LOOP AT sum_i_bsis.
    SELECT entno ebeln matnr
           SUM( menge )     AS menge
           SUM( entamount ) AS entamount
           SUM( duty_amt )  AS duty_amt
           SUM( mpf_amt )   AS mpf_amt
           entcurr AS curr
           uom
           cl_doc_no
       APPENDING  CORRESPONDING FIELDS OF TABLE i_bsis
       FROM ztmm_duty_it
       WHERE entno EQ sum_i_bsis-zuonr
       AND  ebeln IN s_ebeln
       AND  entno IN ( SELECT entno FROM ztmm_duty_hd
                                WHERE entdate IN s_entrd )
       GROUP BY entno ebeln matnr  entcurr uom cl_doc_no.
  ENDLOOP.

* Country Key 'KR' process
  SORT i_bsis BY ebeln entno matnr entcurr uom cl_doc_no.
  LOOP AT i_bsis.
    $ix = sy-tabix.
* Vendor Account Number
    CLEAR: ekko.
    SELECT SINGLE lifnr
      INTO ekko-lifnr
      FROM ekko
     WHERE ebeln = i_bsis-ebeln.
* Country Key
    CLEAR: lfa1.
    SELECT SINGLE land1
      INTO lfa1-land1
      FROM lfa1
     WHERE lifnr = ekko-lifnr.
    IF lfa1-land1 = 'KR'
    or lfa1-land1 = ''.
      DELETE i_bsis INDEX $ix.
    ENDIF.
  ENDLOOP.
* 02.29.2012(insert end), mjc

  LOOP AT sum_i_bsis.

    IF sum_i_bsis-dmbtr EQ 0.
      g_error = true.
      MESSAGE s000 WITH 'The Amount in BSIS is Zero.'.
      CONTINUE.
    ENDIF.

    LOOP AT i_bsis WHERE entno = sum_i_bsis-zuonr.
      $ix = sy-tabix.
      i_bsis-bukrs = sum_i_bsis-bukrs.
      i_bsis-hkont = sum_i_bsis-hkont.
      i_bsis-fentp = sum_i_bsis-fentp.
      i_bsis-gjahr = sum_i_bsis-gjahr.
      i_bsis-belnr = sum_i_bsis-belnr.
      i_bsis-dmbtr = sum_i_bsis-dmbtr.
      MODIFY i_bsis INDEX $ix.
    ENDLOOP.
  ENDLOOP.

* }

* UD1K941354 - by IG.MOON 8/17/2007 {
  PERFORM get_dst_amt.
* }

  __process 'Wait...' '40'.
  __cls gr_matnr.

  SORT i_bsis BY matnr entno.

  LOOP AT i_bsis.
    $ix = sy-tabix.

    IF NOT i_bsis-cl_doc_no IS INITIAL.
      CLEAR *ztcou124_new.
      SELECT belum
             SUM( rmenge )  " Partial Posting Qty
             SUM( dmbtr )   " Partial Posting Amt
             SUM( dstamt )
            INTO (*ztcou124_new-belum,*ztcou124_new-rmenge,
                  *ztcou124_new-dmbtr,*ztcou124_new-dstamt)
            FROM ztcou124_new
            WHERE entno EQ i_bsis-entno
                  AND  matnr EQ i_bsis-matnr
                  AND  belum NE space
                  GROUP BY entno matnr belum.
        IF *ztcou124_new-rmenge <> 0.
          SELECT SINGLE belnr FROM rbkp
                          INTO *rbkp-belnr
                         WHERE  belnr EQ *ztcou124_new-belum
                           AND  gjahr EQ i_bsis-gjahr
                           AND  stjah EQ space
                           AND  stblg EQ space.
          IF sy-subrc EQ 0.
            i_bsis-menge    = i_bsis-menge    + *ztcou124_new-rmenge.
            i_bsis-duty_amt = i_bsis-duty_amt + *ztcou124_new-dmbtr.
            i_bsis-$dst_amt = i_bsis-$dst_amt + *ztcou124_new-dstamt.

            MODIFY i_bsis INDEX $ix TRANSPORTING menge duty_amt $dst_amt.

            CLEAR *ztcou124_new.
          ENDIF.
        ENDIF.
      ENDSELECT.
    ENDIF.

    IF i_bsis-menge EQ 0.
      DELETE i_bsis INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    IF i_bsis-matnr IN s_matnr.
    ELSE.
      DELETE i_bsis INDEX sy-tabix.
      CONTINUE.
    ENDIF.

* Creating the Mat'l ranges for performance improvement of searching PO.
    CLEAR gr_matnr.
    gr_matnr-sign   = 'I'.
    gr_matnr-low    = i_bsis-matnr.
    gr_matnr-option = 'EQ'.
    APPEND gr_matnr.

    CLEAR gr_ebeln.
    gr_ebeln-sign   = 'I'.
    gr_ebeln-low    = i_bsis-ebeln.
    gr_ebeln-option = 'EQ'.
    APPEND gr_ebeln.
  ENDLOOP.

  READ TABLE gr_matnr INDEX 1.
  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

  SORT gr_matnr BY low .
  DELETE ADJACENT DUPLICATES FROM gr_matnr COMPARING low.

ENDFORM.                    "get_row_data_06_new
*&---------------------------------------------------------------------*
*&      Form  KSCHL_INPUT_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_KSCHL  text
*----------------------------------------------------------------------*
FORM kschl_input_help CHANGING p_p_kschl.
  DATA j LIKE sy-index.
  __cls con_list.

  SELECT kschl vtext
  INTO TABLE con_list
  FROM t685t
  WHERE spras = 'EN'
    AND kvewe = 'A'
    AND kappl = 'M'.

  SORT con_list BY kschl .

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'KSCHL'.
  help_field-selectflag = 'X'.
  APPEND help_field.

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'VTEXT'.
  help_field-selectflag = ' '.
  APPEND help_field.

  LOOP AT con_list.
    help_value-value = con_list-kschl.
    APPEND help_value.
    help_value-value = con_list-vtext.
    APPEND help_value.
  ENDLOOP.

  PERFORM value_help CHANGING j.

  IF j > 0.
    READ TABLE con_list INDEX j.
    p_p_kschl = con_list-kschl.
  ENDIF.

  dynpfields-fieldname  = 'KSCHL'.
  dynpfields-fieldvalue = con_list-kschl.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpfields.

  CLEAR: dynpfields.
  REFRESH: con_list, help_field, help_vtab, help_value, dynpfields.
ENDFORM.                    " KSCHL_INPUT_HELP
*&---------------------------------------------------------------------*
*&      Form  VALUE_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_J  text
*----------------------------------------------------------------------*
FORM value_help CHANGING p_j.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      display       = ' '
    IMPORTING
      index         = p_j
    TABLES
      fields        = help_field
      select_values = help_vtab
      valuetab      = help_value.
ENDFORM.                               " VALUE_HELP
*&---------------------------------------------------------------------*
*&      Form  CALC_DATA_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_data.

  DATA : flag(1),
         $target LIKE i_bsis-menge.
  __cls gt_calc.

* The balance data
  SORT itab_for_matnr_sum BY $key.

* The P/O Data
  SORT it_row_tab BY $key ebeln ebelp bal_rv bal_tr.

* The FTZ data
  SORT : i_bsis BY $key.

  DESCRIBE TABLE i_bsis LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.
  CLEAR current_doc_cnt.

  LOOP AT i_bsis.
    ADD 1 TO current_doc_cnt.

    AT NEW $key.
      flag = true.
    ENDAT.
*** proceed ?
    CHECK flag EQ true.
***
    IF i_bsis-$menge <> 0.
      $target = i_bsis-$menge.
    ELSE.
      $target = i_bsis-menge.
    ENDIF.

    CLEAR *ztcou124_new.
    MOVE-CORRESPONDING i_bsis TO *ztcou124_new.
    MOVE : i_bsis-menge TO *ztcou124_new-xmenge,
           i_bsis-$menge TO *ztcou124_new-xmenge2,
           i_bsis-entcurr TO *ztcou124_new-waers,
           i_bsis-uom TO *ztcou124_new-meins,
           i_bsis-pstqty TO *ztcou124_new-rmenge.

    READ TABLE it_row_tab WITH KEY $key = i_bsis-$key
                          BINARY SEARCH .

    IF sy-subrc EQ 0.
      PERFORM create_calc_tab TABLES gt_calc
                               USING i_bsis-$key
                                     $target
                                     i_bsis-duty_amt .
    ELSE.
      PERFORM create_none_po TABLES gt_calc  "  No Item to Calculate
                               USING i_bsis-$key
                                     i_bsis-menge
                                     i_bsis-duty_amt .
    ENDIF.

    $mod = current_doc_cnt MOD 100.
    IF $mod EQ 0.
      $current_cnt = current_doc_cnt.
      CONCATENATE $current_cnt '/' $total_cnt
      INTO $text.
      CONDENSE $text.
      CONCATENATE 'Check Data...' $text INTO $prog_text.
      percentage = current_doc_cnt / total_doc_cnt * 100.
      PERFORM show_progress USING $prog_text percentage.
    ENDIF.

  ENDLOOP.

  IF p_entry EQ '01'.
    PERFORM check_partial_post_01 TABLES gt_calc.
  ENDIF.

ENDFORM.                    " CALC_DATA_06
*&---------------------------------------------------------------------*
*&      Form  get_row_data_ekbz_06_08_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data_ekbz_06_08_new.

  DATA $gr_ebeln LIKE gr_ebeln OCCURS 0  WITH HEADER LINE.

  __process 'Read PO...' '30'.

  __cls it_row_tab.
  IF NOT s_ebeln[] IS INITIAL.
    $gr_ebeln[] = s_ebeln[] .
*    PERFORM get_po_one_time TABLES $gr_matnr.
    PERFORM get_po_one_time_new TABLES $gr_ebeln.       "new
  ELSE.

    DATA max_range TYPE i.
    DATA line1 LIKE sy-tabix.
    DESCRIBE TABLE gr_ebeln LINES line1.
    IF p_mx_r IS INITIAL.
      max_range = 50.
    ELSE.
      max_range = p_mx_r.
    ENDIF.

    IF line1 < max_range.
      $gr_ebeln[] = gr_ebeln[].
*      PERFORM get_po_one_time TABLES $gr_ebeln.       "new
      PERFORM get_po_one_time_new TABLES $gr_ebeln.
    ELSE.

      DATA : $times TYPE i,
             $ix LIKE sy-tabix,
             $line LIKE sy-tabix.

      $times = line1 / max_range.
      ADD 1 TO $times.
      $ix = 1.

      DO $times TIMES.

        __cls $gr_ebeln.
        $line = 1.

        LOOP AT gr_ebeln FROM $ix.
          $ix = sy-tabix.
          $gr_ebeln = gr_ebeln.
          APPEND $gr_ebeln.
          CLEAR $gr_ebeln.
          ADD 1 TO $line.
          IF $line > max_range.
            EXIT.
          ENDIF.
        ENDLOOP.

        ADD 1 TO $ix.

        IF NOT $gr_ebeln[] IS INITIAL.
          PERFORM get_po_several_times_new     TABLES $gr_ebeln.    "new
        ENDIF.

        IF $ix > line1.
          EXIT.
        ENDIF.

      ENDDO.

    ENDIF.

  ENDIF.

  DELETE it_row_tab WHERE ebeln = '4200007416' AND ebelp = '00019'.
  DELETE it_row_tab WHERE ebeln = '4200007436' AND ebelp = '00074'.

  IF p_expo EQ true.
    DELETE it_row_tab WHERE ebeln CP '43*'.
    DELETE it_row_tab WHERE ebeln CP '46*'.
  ENDIF.


  SORT it_row_tab BY bukrs kschl matnr ebeln ebelp.

*  LOOP AT it_row_tab.
*    AT NEW matnr.
*      READ TABLE gr_matnr WITH KEY low = it_row_tab-matnr
*      BINARY SEARCH.
*      IF sy-subrc NE 0.
*        DELETE it_row_tab WHERE matnr EQ it_row_tab-matnr.
*      ENDIF.
*    ENDAT.
*  ENDLOOP.

  it_row_tab_bal[] = it_row_tab[].

  SORT i_bsis BY entno ebeln matnr.
  LOOP AT it_row_tab.
    $ix = sy-tabix.
    READ TABLE i_bsis WITH KEY ebeln = it_row_tab-ebeln
                               matnr = it_row_tab-matnr
                               BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_row_tab INDEX $ix.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_row_data_ekbz_06_08_new
*&---------------------------------------------------------------------*
*&      Form  REFINE_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_itab_06_08.

  DATA : lt_itab LIKE it_row_tab OCCURS 0 WITH HEADER LINE,
         lt_itab_for_unitprice LIKE it_row_tab OCCURS 0
                                           WITH HEADER LINE.

  __process 'Calc...U/P' '55'.

**   Calculate the unit price using by 'DCGR'
  PERFORM get_unit_price TABLES lt_itab_for_unitprice.

**   Collect EKBZ : Debit('S') --> * -1
  PERFORM collect_itab TABLES lt_itab.

**   Distribute PO by Entry# MATNR
  PERFORM distribute_po_data TABLES lt_itab.

  __process 'Wait...' '65'.

**   Fill U/P,P/O Qty,Balance to IT_ROW_TAB
  PERFORM recalc_balance TABLES lt_itab
                                lt_itab_for_unitprice .

  __process 'Wait...' '75'.

ENDFORM.                    " REFINE_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gt_out.

  DATA: xcolor TYPE slis_specialcol_alv.

  __cls gt_out .

  LOOP AT gt_calc.
    $ix = sy-tabix.

    PERFORM end_of_compute.
    gt_calc-accamt = gt_calc-accup * gt_calc-rmenge.
    gt_calc-varamt = gt_calc-accamt - gt_calc-dstamt.

    PERFORM write_num_to_string TABLES gt_calc.
    MOVE-CORRESPONDING gt_calc TO gt_out.

* UD1K941354 - by IG.MOON 7/17/2007 {
    IF gt_out-entamount <> 0.
      gt_out-percent = gt_out-duty_amt / gt_out-entamount * 100.
      WRITE gt_out-percent TO gt_out-percents
            RIGHT-JUSTIFIED NO-ZERO.
    ENDIF.
* }

    CLEAR gt_out-$key.
    IF gt_out-rmenge <> 0 AND gt_out-matnr IN s_matnr.
      APPEND gt_out.
    ENDIF.
  ENDLOOP.
* Please don't try to analysis followings,
* It's only for changing of cell-color, not important at all.

  IF p_load EQ space.
    SORT gt_calc BY matnr bal_rv bal_tr.

    LOOP AT gt_out.
      IF gt_out-bal_tr NE space.
        $ix = sy-tabix.
        READ TABLE gt_calc WITH KEY matnr = gt_out-matnr
                                    bal_rv = gt_out-ebeln
                                    bal_tr = space
                                    BINARY SEARCH.
        IF sy-subrc NE 0.
          gt_out-bal_tr = space.
          MODIFY gt_out INDEX $ix TRANSPORTING bal_tr.
        ELSE.
          IF gt_calc-rmenge EQ 0.
            gt_out-bal_tr = space.
            MODIFY gt_out INDEX $ix TRANSPORTING bal_tr.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDLOOP.

    LOOP AT gt_out.
      IF gt_out-bal_tr EQ space AND gt_out-amenge > 0.
        $ix = sy-tabix.
        READ TABLE gt_calc WITH KEY matnr = gt_out-matnr
                                    bal_rv = gt_out-ebeln
                                    BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF gt_calc-rmenge > 0.
            gt_out-bal_tr = true.
            MODIFY gt_out INDEX $ix TRANSPORTING bal_tr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_out.
      IF gt_out-bal_tr NE space.
        $ix = sy-tabix.
        IF gt_out-amenge = 0.
          gt_out-bal_tr = space.
          MODIFY gt_out INDEX $ix TRANSPORTING bal_tr.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT gt_out BY entno matnr xmenges xmenge2s
                 dtamts ivamts ebeln dtyups.

  LOOP AT gt_out.

    IF gt_out-bal_tr EQ true.          "color CELL
      PERFORM set_filed_color TABLES gt_out
                               USING
                                    'AMENGE'  " Field name
                                    '5'      ." Color
    ENDIF.
    IF NOT gt_out-bal_rv IS INITIAL.
      PERFORM set_filed_color TABLES gt_out
                               USING
                                    'BMENGE'  " Field name
                                    '5'      ." Color
    ENDIF.
    IF NOT gt_out-pstqty IS INITIAL.
      PERFORM set_filed_color TABLES gt_out
                               USING
                                    'XMENGES'  " Field name
                                    '6'      ." Color
      PERFORM set_filed_color TABLES gt_out
                               USING
                                    'DTAMTS'  " Field name
                                    '6'      ." Color
*   CONCATENATE GT_OUT-PSTQTYS ' was posted already.' INTO GT_OUT-MSG.
    ENDIF.
    IF gt_out-rev EQ true.
      PERFORM set_filed_color TABLES gt_out
                               USING
                                    'BELUM'  " Field name
                                    '6'      ." Color
      gt_out-msg = 'doc. is reversed'.
    ENDIF.

    IF p_entry EQ '08'.
      IF gt_out-ebeln EQ space.
        gt_out-icon = icon_led_inactive.
      ENDIF.
    ENDIF.

    IF gt_out-bmenge EQ '0.000' AND  gt_out-duty_amt EQ '0.00'.
      gt_out-icon = icon_led_inactive.
    ENDIF.

    MODIFY gt_out.
    CLEAR gt_out.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Form  create_calc_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CALC  text
*----------------------------------------------------------------------*
FORM create_calc_tab TABLES p_gt_calc STRUCTURE gt_calc
                      USING  p_$key
                             tempq
                             dtyamt .

  DATA : quantity LIKE ekbz-menge,  " Temp variable
         balance  LIKE ekbz-menge,  " Sum by Material
         abs_qty  LIKE ekbz-menge,  " ABS Value for Quantity
         $index   LIKE sy-tabix,
         m_idx    LIKE sy-tabix.

  DATA : BEGIN OF $itab OCCURS 0,
           index     LIKE sy-tabix,
           dtyamt    LIKE ztcou124_new-duty_amt, "Amount
         END   OF  $itab.

  quantity =  tempq.
  m_idx = 0.

  CLEAR *ztcou124_new-dtyup. " Clear garbage

  READ TABLE itab_for_matnr_sum WITH KEY
                      $key = p_$key
                      BINARY SEARCH.

  IF sy-subrc EQ 0.
    m_idx = sy-tabix.
    balance = itab_for_matnr_sum-bmenge.
    IF dtyamt GT 0.
      IF balance GE quantity.
         *ztcou124_new-dtyup = dtyamt / quantity .
      ELSE.
        IF balance <> '0'.
           *ztcou124_new-dtyup = dtyamt / balance .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  quantity =  -1 * quantity.

  READ TABLE it_row_tab WITH KEY
                      $key = p_$key
*                      land1 <> 'KR'
                      BINARY SEARCH.



  IF sy-subrc EQ 0.
    m_idx = sy-tabix.
  ELSE.
    EXIT.
  ENDIF.

  LOOP AT it_row_tab FROM m_idx.
    IF it_row_tab-$key NE p_$key.
      EXIT.
    ENDIF.

    $index = sy-tabix.

    IF it_row_tab-bmenge > 0 AND quantity LT 0.
      abs_qty = abs( quantity ) .
***   Decrease quantity from old PO#
      IF  abs_qty <= it_row_tab-bmenge.
        it_row_tab-rmenge =  abs_qty .
        quantity = 0.               " Calc. end for this material
      ELSE.
        it_row_tab-rmenge = it_row_tab-bmenge .
        quantity  = it_row_tab-bmenge + quantity.
      ENDIF .
      it_row_tab-rmenge = -1 * it_row_tab-rmenge.
    ENDIF.

    MOVE-CORRESPONDING it_row_tab  TO p_gt_calc.
    MOVE : *ztcou124_new-entno         TO p_gt_calc-entno,
           *ztcou124_new-xmenge        TO p_gt_calc-xmenge,
           *ztcou124_new-xmenge2       TO p_gt_calc-xmenge2,
           *ztcou124_new-entamount     TO p_gt_calc-entamount,
           *ztcou124_new-duty_amt      TO p_gt_calc-duty_amt,
           *ztcou124_new-mpf_amt       TO p_gt_calc-mpf_amt,
           *ztcou124_new-dtyup         TO p_gt_calc-dtyup,
           it_row_tab-rmenge       TO p_gt_calc-rmenge,
           it_row_tab-bmenge       TO p_gt_calc-bmenge,
           *ztcou124_new-rmenge        TO p_gt_calc-pstqty,
           *ztcou124_new-gjahr         TO p_gt_calc-gjahr.

    p_gt_calc-amenge = p_gt_calc-bmenge + p_gt_calc-rmenge.

* Duty Clearing Amount
    IF p_entry EQ '01'.
      p_gt_calc-dmbtr  = p_gt_calc-accup * p_gt_calc-rmenge.
    ELSE.
      p_gt_calc-dmbtr  = p_gt_calc-dtyup * p_gt_calc-rmenge.
    ENDIF.

    p_gt_calc-hkont  = p_hkont.
    APPEND p_gt_calc.

    $itab-index  = sy-tabix.
    IF p_gt_calc-dmbtr <> 0.
      $itab-dtyamt = p_gt_calc-dmbtr.
      APPEND $itab .
    ENDIF.
  ENDLOOP.

  CHECK p_entry NE '01'.

** Adjust total amount with Duty amount

  DATA : $dtyamt LIKE ztcou124_new-duty_amt,
         $adjust LIKE ztcou124_new-duty_amt.

  LOOP AT $itab.
    AT LAST.
      SUM.
      $dtyamt = $itab-dtyamt .
    ENDAT.
  ENDLOOP.

  IF $dtyamt <> dtyamt .
    $adjust = abs( $dtyamt ) - abs( dtyamt ) .
    SORT $itab BY dtyamt.
    READ TABLE $itab INDEX 1. " Adjust diff.Amt into the biggest.
    IF sy-subrc EQ 0.
      READ TABLE p_gt_calc INDEX $itab-index.
      IF sy-subrc EQ 0.
        IF $dtyamt < dtyamt .
          p_gt_calc-dmbtr = p_gt_calc-dmbtr + $adjust.
        ELSE.
          p_gt_calc-dmbtr = p_gt_calc-dmbtr - $adjust.
        ENDIF.
        MODIFY p_gt_calc INDEX $itab-index.
      ENDIF.
    ENDIF.
    EXIT.
  ENDIF.

ENDFORM.                    " create_calc_tab
*&---------------------------------------------------------------------*
*&      Form  GET_MATNR_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM distribute_po_data TABLES p_lt_itab STRUCTURE it_row_tab.

  DATA : flag(1).

*  SORT : P_LT_ITAB BY BUKRS KSCHL MATNR EBELN EBELP,
*         I_BSIS BY ENTNO MATNR .

  SORT : p_lt_itab BY matnr ebeln ebelp,
         i_bsis BY entno matnr .

  DESCRIBE TABLE i_bsis LINES total_doc_cnt.

*  for Unit convert, Just in case - not enabled yet {
*  PERFORM UNIT_CONVERT_BATCH TABLES I_BSIS
*                                    P_LT_ITAB.
* }

* Simulate first .
  PERFORM prepare_assign_by_quantity TABLES i_bsis
                                            p_lt_itab
                                      USING 'X'.

* refresh the $key
  p_lt_itab-$key = space.
  MODIFY p_lt_itab TRANSPORTING $key WHERE $key NE space.

*   Assign the P/Os for each entry# + material#
  PERFORM prepare_assign_by_quantity TABLES i_bsis
                                            p_lt_itab
                                      USING ' '.
*   Fill Articles's $Key
  LOOP AT i_bsis.
*    CONCATENATE i_bsis-entno i_bsis-matnr INTO i_bsis-$key.
    CONCATENATE i_bsis-entno i_bsis-ebeln i_bsis-matnr INTO i_bsis-$key.
    MODIFY i_bsis INDEX sy-tabix.
  ENDLOOP.

  SORT : i_bsis BY matnr entno,
         p_lt_itab BY matnr ebeln ebelp ASCENDING $key DESCENDING.

  DATA modi_start(1).

  $total_cnt = total_doc_cnt.
  CLEAR current_doc_cnt.

  DATA : $key LIKE p_lt_itab-$key,
         $str(20).
  DESCRIBE TABLE p_lt_itab LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.
  CLEAR current_doc_cnt.

  LOOP AT p_lt_itab.
    ADD 1 TO current_doc_cnt.
    IF p_lt_itab-$key NE space.
      $key = p_lt_itab-$key.
    ELSE.
      CONCATENATE '*' p_lt_itab-matnr INTO $str.
      IF $key CP $str.
        p_lt_itab-$key = $key.
        MODIFY p_lt_itab INDEX sy-tabix TRANSPORTING $key.
      ENDIF.
    ENDIF.
    $mod = current_doc_cnt MOD 100.
    IF $mod EQ 0.
      $current_cnt = current_doc_cnt.
      CONCATENATE $current_cnt '/' $total_cnt
      INTO $text.
      CONDENSE $text.
      CONCATENATE 'Assign ENT#...' $text INTO $prog_text.
      percentage = current_doc_cnt / total_doc_cnt * 100.
      PERFORM show_progress USING $prog_text percentage.
    ENDIF.
  ENDLOOP.

  SORT : i_bsis BY entno matnr,
         p_lt_itab BY $key.

*   GET SUM by MATNR
  __cls itab_for_matnr_sum.

  LOOP AT p_lt_itab.
    MOVE p_lt_itab TO itab_for_matnr_sum .
    AT END OF $key.
      SUM.
      itab_for_matnr_sum-bmenge = p_lt_itab-bmenge.
      APPEND itab_for_matnr_sum.
    ENDAT .
  ENDLOOP.

ENDFORM.                    " GET_MATNR_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_UNIT_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ROW_TAB  text
*      -->P_LT_ITAB_FOR_UNITPRICE  text
*----------------------------------------------------------------------*
FORM get_unit_price TABLES   p_lt_itab_for_unitprice
                             STRUCTURE it_row_tab .

* Calculate the unit price using by 'DCGR'
  LOOP AT it_row_tab WHERE bewtp EQ 'F'.
    MOVE it_row_tab TO p_lt_itab_for_unitprice .

    IF p_lt_itab_for_unitprice-shkzg EQ 'S'.
      p_lt_itab_for_unitprice-bmenge =
               -1 * p_lt_itab_for_unitprice-bmenge.
      p_lt_itab_for_unitprice-dmbtr =
               -1 * p_lt_itab_for_unitprice-dmbtr.
    ENDIF.

    CLEAR :  p_lt_itab_for_unitprice-pmenge,
             p_lt_itab_for_unitprice-accup,
             p_lt_itab_for_unitprice-shkzg,
             p_lt_itab_for_unitprice-bewtp,
             p_lt_itab_for_unitprice-meins,
             p_lt_itab_for_unitprice-waers.

    COLLECT p_lt_itab_for_unitprice.

  ENDLOOP.

  LOOP AT p_lt_itab_for_unitprice.
    IF p_lt_itab_for_unitprice-bmenge > 0.
      p_lt_itab_for_unitprice-accup =
      p_lt_itab_for_unitprice-dmbtr / p_lt_itab_for_unitprice-bmenge.
      MODIFY p_lt_itab_for_unitprice INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  SORT p_lt_itab_for_unitprice BY matnr ebeln ebelp.


ENDFORM.                    " GET_UNIT_PRICE
*&---------------------------------------------------------------------*
*&      Form  RECALC_BALANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ITAB  text
*----------------------------------------------------------------------*
FORM recalc_balance TABLES p_lt_itab STRUCTURE it_row_tab
                           p_lt_itab_for_unitprice STRUCTURE it_row_tab.

  LOOP AT p_lt_itab.
    $ix = sy-tabix.
*   reset PO quantity as original quantity cause it was summed.
    READ TABLE it_row_tab WITH KEY
                          bukrs = p_lt_itab-bukrs
                          kschl = p_lt_itab-kschl
                          matnr = p_lt_itab-matnr
                          ebeln = p_lt_itab-ebeln
                          ebelp = p_lt_itab-ebelp
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      p_lt_itab-pmenge = it_row_tab-pmenge .
    ENDIF.

    READ TABLE p_lt_itab_for_unitprice WITH KEY
                          bukrs = p_lt_itab-bukrs
                          kschl = p_lt_itab-kschl
                          matnr = p_lt_itab-matnr
                          ebeln = p_lt_itab-ebeln
                          ebelp = p_lt_itab-ebelp
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      p_lt_itab-accup = p_lt_itab_for_unitprice-accup .
*      At this point LT_ITAB_FOR_UNITPRICE-BMENGE contains the
*      G/R quantity
      p_lt_itab-gmenge = p_lt_itab_for_unitprice-bmenge .
    ENDIF.

    MODIFY p_lt_itab INDEX $ix.

  ENDLOOP.

  __cls it_row_tab.
  it_row_tab[] = p_lt_itab[] .

ENDFORM.                    " RECALC_BALANCE
*&---------------------------------------------------------------------*
*&      Form  CREATE_CALC_TAB_NONE_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CALC  text
*      -->P_ENDIF  text
*----------------------------------------------------------------------*
FORM create_none_po   TABLES p_gt_calc STRUCTURE gt_calc
                      USING  p_$key
                             tempq
                             dtyamt .

   *ztcou124_new-dtyup = 0. " Clear garbage
  CLEAR p_gt_calc.

  IF tempq EQ 0.
     *ztcou124_new-dtyup = 0..
  ELSE.
     *ztcou124_new-dtyup = dtyamt / tempq .
  ENDIF.

  tempq = -1 * tempq .

  MOVE-CORRESPONDING *ztcou124_new TO p_gt_calc.

  MOVE :
         p_$key TO p_gt_calc-$key,
         tempq TO p_gt_calc-rmenge,
         0 TO p_gt_calc-bmenge.

  CLEAR p_gt_calc-ebelp.

  p_gt_calc-amenge = p_gt_calc-bmenge + p_gt_calc-rmenge.
  p_gt_calc-dmbtr =  -1 * dtyamt.
  p_gt_calc-hkont = p_hkont.
  APPEND p_gt_calc.

ENDFORM.                    " CREATE_CALC_TAB_NONE_PO
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_data.
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

****************************> No Important
  DATA : total_lines TYPE i,
         count TYPE i.
****************************> end

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'Please select a data.'.
    EXIT.
  ENDIF.

  PERFORM get_posting_data TABLES lt_rows
                                  lt_row_no .

* read total lines
  DESCRIBE TABLE it_inv LINES total_lines.

* Start BAPI logic.
  READ TABLE it_inv INDEX 1.
  IF sy-subrc EQ 0.

* delete saved data
    DELETE FROM ztcou124_new
      WHERE     pdate        IN    s_date
        AND     matnr        IN    s_matnr
        AND     entno        IN    s_entno
        AND     belum        EQ    space.

    COMMIT WORK.

    PERFORM call_bapi_inv TABLES it_inv
                          USING text-p01
                                'X' 'RI'
                                total_lines
                       CHANGING count.
  ENDIF.

ENDFORM.                    " POST_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_POSTING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*      -->P_LT_ROW_NO  text
*----------------------------------------------------------------------*
FORM get_posting_data TABLES pt_rows STRUCTURE lvc_s_row
                             pt_row_no STRUCTURE lvc_s_roid.

  __cls it_inv .

  CLEAR gt_out-chk.
  MODIFY gt_out TRANSPORTING chk WHERE chk = 'X'.

* Selected Row by row selection
  LOOP AT pt_rows WHERE rowtype IS INITIAL.
    READ TABLE gt_out INDEX pt_rows-index.
    CHECK gt_out-rmenge NE 0 .
    gt_out-chk = true .
    MODIFY gt_out INDEX pt_rows-index .
  ENDLOOP.

  PERFORM select_row_by_subtotal TABLES pt_rows .

  LOOP AT gt_out WHERE chk EQ true.

    CHECK : gt_out-rmenge NE 0 ,
            gt_out-belum IS INITIAL .

    IF p_entry EQ '08'.
      CHECK gt_out-ebeln NE space.
    ENDIF.

    IF gt_out-bmenge EQ '0.000' AND  gt_out-duty_amt EQ '0.00'.
    ELSE.
      MOVE-CORRESPONDING gt_out TO it_inv.
      MOVE gt_out-rmenge TO it_inv-menge.
      MOVE sy-tabix TO it_inv-line_no.
      APPEND it_inv. CLEAR it_inv.
    ENDIF.
  ENDLOOP.

* grouping by max. posting item per entry# .
  total_doc_cnt = 0.
  current_doc_cnt = 0.
  PERFORM grouping_by_ent TABLES it_inv .
ENDFORM.                    " GET_POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_ROWS  text
*----------------------------------------------------------------------*
FORM select_row_by_subtotal TABLES p_pt_rows
                                   STRUCTURE lvc_s_row.

  DATA: tmpgrp TYPE lvc_t_grpl, " For subtotal Selection .
       $tmpgrp TYPE lvc_s_grpl.

  CALL METHOD g_grid->get_subtotals
    IMPORTING
      et_grouplevels = tmpgrp.

* Selected Row by row selection ( Sub total )
  LOOP AT p_pt_rows WHERE NOT rowtype IS INITIAL.
    READ TABLE tmpgrp INDEX p_pt_rows-index INTO $tmpgrp.
    CHECK sy-subrc EQ 0 .

    LOOP AT gt_out FROM $tmpgrp-index_from
                     TO $tmpgrp-index_to.
      gt_out-chk = true .
      MODIFY gt_out.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " SELECT_ROW_BY_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  GROUPING_BY_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INV  text
*----------------------------------------------------------------------*
FORM grouping_by_ent TABLES p_it_inv STRUCTURE it_inv.

  DATA :  group_key TYPE i,
          max_line  TYPE i,
          line_cnt  TYPE i.

  max_line = p_max .
  group_key = 0.

  LOOP AT p_it_inv.
    AT NEW entno.
      line_cnt = 0 .
      ADD 1 TO : group_key, total_doc_cnt.
    ENDAT.
    IF  line_cnt GE  max_line.
      line_cnt = 0 .
      ADD 1 TO : group_key, total_doc_cnt.
    ENDIF.
    p_it_inv-$group =  group_key .
    MODIFY p_it_inv INDEX sy-tabix.
    ADD 1 TO  line_cnt .
  ENDLOOP.

  LOOP AT p_it_inv.
    p_it_inv-group = p_it_inv-$group .
    MODIFY p_it_inv INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " GROUPING_BY_MAT
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_INV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INV  text
*      -->P_TEXT_P01  text
*      -->P_2477   text
*      -->P_2478   text
*      -->P_TOTAL_LINES  text
*      <--P_COUNT  text
*----------------------------------------------------------------------*
FORM call_bapi_inv TABLES p_it_inv STRUCTURE it_inv
                   USING prog_text
                         ind
                         iv_type
                         p_total_lines
                CHANGING p_count.

  DATA : end_flag(1) ,
         item_cnt(4) TYPE n,
         gross_amount LIKE bapi_incinv_create_header-gross_amount,
         grs_dist_amt LIKE bapi_incinv_create_header-gross_amount,
         $meins LIKE p_it_inv-meins,            " for G/L posting
         $matnr LIKE p_it_inv-matnr,            " for G/L posting
         $gl_doc_cnt  TYPE i.

  __cls : itemdata , gl_data, it_index.

  MOVE  0  TO : item_cnt, $gl_doc_cnt.

  LOOP AT p_it_inv.
    ADD 1 TO p_count.

    it_index-index = p_it_inv-line_no. " Keep for status update
    APPEND it_index.CLEAR it_index.

    AT END OF group.
      end_flag = true.
      SUM.
      gross_amount = p_it_inv-dmbtr.
      grs_dist_amt = p_it_inv-dstamt.
    ENDAT.

    PERFORM fill_bapi_structure_item TABLES   p_it_inv
                                     CHANGING item_cnt .

** Accrual G/L Account
    $meins = p_it_inv-meins.
    $matnr = p_it_inv-matnr.
    AT END OF hkont.
      ADD 1 TO $gl_doc_cnt.
      SUM.
      gl_data-invoice_doc_item = $gl_doc_cnt.

*ANDY
*     GL_DATA-GL_ACCOUNT = P_IT_INV-HKONT.
      gl_data-gl_account = p_hkont.

      gl_data-alloc_nmbr = p_it_inv-entno.
*     GL_DATA-ITEM_TEXT  = P_ENTRY.
      gl_data-db_cr_ind  = 'H'.
      gl_data-comp_code  = p_bukrs.
      gl_data-item_amount = abs( p_it_inv-dstamt ) .
      APPEND : gl_data.
*ANDY
      IF p_it_inv-hkont <> p_hkont.
        ADD 1 TO $gl_doc_cnt.
        gl_data-invoice_doc_item = $gl_doc_cnt.
        gl_data-gl_account = p_it_inv-hkont.
        gl_data-alloc_nmbr = p_it_inv-entno.
        gl_data-item_text  = $matnr.
        gl_data-base_uom   = $meins.
        gl_data-quantity   = abs( p_it_inv-menge ) .
        gl_data-db_cr_ind  = 'S'.
        gl_data-comp_code  = p_bukrs.
        gl_data-item_amount = abs( p_it_inv-dstamt ) .
        APPEND : gl_data.
      ENDIF.

      CLEAR  : gl_data.
    ENDAT.
** End of G/L

    IF end_flag EQ true.
**-----ANDY
*      IF P_IT_INV-HKONT = P_HKONT.
*        PERFORM WRITE_OFF_ITEM TABLES   P_IT_INV
*                               USING    GROSS_AMOUNT
*                                        GRS_DIST_AMT
*                               CHANGING $GL_DOC_CNT .
*      ENDIF.

*------------------------------------------------------------*
*     Not important
*------------------------------------------------------------*
      ADD 1 TO current_doc_cnt.
      $current_cnt = current_doc_cnt.
      $total_cnt = total_doc_cnt.
      CONCATENATE $current_cnt '/' $total_cnt
      INTO $text.
      CONDENSE $text .
      CONCATENATE prog_text $text INTO $prog_text.
      percentage = p_count / p_total_lines * 100.
      PERFORM show_progress USING $prog_text percentage.
*------------------------------------------------------------*
      item_cnt = 0.
      PERFORM fill_bapi_structure_header TABLES p_it_inv
                                          USING gross_amount
                                                ind
                                                iv_type  .

* UD1K941350 - by IG.MOON 8/17/2007 {
      PERFORM collect_gl_data.
* }

      PERFORM post_bapi_invoice   USING prog_text
                                        ind.
      CLEAR end_flag .

    ENDIF.

  ENDLOOP.


ENDFORM.                    " CALL_BAPI_INV
*&---------------------------------------------------------------------*
*&      Form  FILL_BAPI_STRUCTURE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_INV  text
*      <--P_ITEM_CNT  text
*----------------------------------------------------------------------*
FORM fill_bapi_structure_item TABLES p_it_inv STRUCTURE it_inv
                            CHANGING p_cnt .

  ADD 1 TO p_cnt.

* FIll Item data
  SELECT SINGLE * FROM ekpo
                    WHERE  ebeln = p_it_inv-ebeln
                       AND ebelp = p_it_inv-ebelp.

  IF sy-subrc EQ 0 AND ekpo-webre EQ 'X'.
    SELECT SINGLE * FROM ekbe
            WHERE ebeln = p_it_inv-ebeln
             AND  ebelp = p_it_inv-ebelp.
    IF sy-subrc EQ 0.
      MOVE :  ekbe-belnr  TO    itemdata-ref_doc,
              ekbe-gjahr  TO    itemdata-ref_doc_year,
              ekbe-buzei  TO    itemdata-ref_doc_it.
    ENDIF.
  ENDIF.

  itemdata-invoice_doc_item = p_cnt.

* use most recent PO for processing purpose
  IF p_it_inv-ebeln IS INITIAL.
  ELSE.
    itemdata-po_number = p_it_inv-ebeln.
    itemdata-po_item = p_it_inv-ebelp.

    itemdata-cond_type = p_kschl.
    itemdata-tax_code = 'U0'.
    itemdata-item_amount = abs( p_it_inv-dstamt ) .
    itemdata-quantity    = abs( p_it_inv-menge ) .
    itemdata-po_unit     = p_it_inv-meins.
    itemdata-po_unit_iso = itemdata-po_unit.

    APPEND : itemdata.

  ENDIF.

  CLEAR  : itemdata.

ENDFORM.                    " fill_BAPI_STRUCTURE_item
*&---------------------------------------------------------------------*
*&      Form  FILL_BAPI_STRUCTURE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_INV  text
*      -->P_GROSS_AMOUNT  text
*      -->P_IND  text
*      -->P_IV_TYPE  text
*----------------------------------------------------------------------*
FORM fill_bapi_structure_header TABLES p_it_inv STRUCTURE it_inv
                                 USING p_gross_amount
                                       ind
                                       iv_type.

*&---------------------------------------------------------- *
*& Type 06
*&  - G/L Account: DDLC, amount = BSIS, h.text = Entry#
*&  - Line item :  PO Item, quantity, amount
*&
*& Type 08
*&  - Vendor = ZERO, line item=ZERO
*&
*& Type 01
*&  - G/L Account: DDLC, amount = BSIS, h.text = Entry#
*&  - Line item :  PO Item(BOL binding),
*&    GR quantity(BOL), GR amount(BOL)
*&---------------------------------------------------------- *

  CLEAR headerdata .

  MOVE : ind              TO  headerdata-invoice_ind,
         iv_type          TO  headerdata-doc_type,
         p_bukrs          TO  headerdata-comp_code,
         p_it_inv-waers   TO  headerdata-currency,
         p_date           TO  headerdata-doc_date,
         p_date           TO  headerdata-pstng_date,
         sy-uname         TO  headerdata-person_ext,
         ''               TO  headerdata-calc_tax_ind,
         p_it_inv-entno(18) TO  headerdata-header_txt,
         p_entry          TO  headerdata-ref_doc_no,
         'N999'           TO  headerdata-diff_inv.   "FIXME
*       |..................XX| ??

ENDFORM.                    " fill_BAPI_STRUCTURE_header
*&---------------------------------------------------------------------*
*&      Form  POST_BAPI_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_INV_INDEX  text
*      -->P_PROG_TEXT  text
*      -->P_IND  text
*----------------------------------------------------------------------*
FORM post_bapi_invoice  USING     p_prog_text
                                  p_ind.
  DATA : $flag(1), $str(20) ,$p_w_off(6).

  IF p_entry EQ '08'. " no G/L when 08 posting
    __cls gl_data.
  ENDIF.

* Call Invoice Posting BAPI
*BAPI_INCOMINGINVOICE_SAVE & RMBABG00 ; refer function module document
*         REFDOCCATEGORY   = '5'  "Invoice with planned delivery costs

*BAPI_INCOMINGINVOICE_CREATE
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
    EXPORTING
      headerdata       = headerdata
    IMPORTING
      invoicedocnumber = invoicedocnumber
      fiscalyear       = fiscalyear
    TABLES
      itemdata         = itemdata
      glaccountdata    = gl_data
      return           = xreturn.

  IF NOT invoicedocnumber IS INITIAL.
    LOOP AT it_index.
      gt_out-icon = icon_led_green.
      gt_out-belum = invoicedocnumber .
      gt_out-msg = 'ok'.
      MODIFY gt_out INDEX it_index-index
      TRANSPORTING icon belum msg .

      AT LAST.
        $flag = true.
      ENDAT.

      CHECK $flag EQ true.

      READ TABLE gl_data WITH KEY
                  alloc_nmbr = headerdata-header_txt
                  gl_account = p_w_off.

      IF sy-subrc EQ 0.
        WRITE : gl_data-item_amount TO $str
                UNIT headerdata-currency
                LEFT-JUSTIFIED,
                p_w_off TO $p_w_off NO-ZERO.
        CONDENSE $str.

*        CONCATENATE 'ok $' $STR ' Write-off(G/L#' $P_W_OFF ')'
*                    INTO GT_OUT-MSG.

        MOVE 'ok' TO gt_out-msg.

        MODIFY gt_out INDEX it_index-index
        TRANSPORTING icon belum msg.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    PERFORM save_log_table TABLES it_index.

  ELSE.
    LOOP AT xreturn WHERE type = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = xreturn-id
          msgnr               = xreturn-number
          msgv1               = xreturn-message_v1
          msgv2               = xreturn-message_v2
          msgv3               = xreturn-message_v3
          msgv4               = xreturn-message_v4
        IMPORTING
          message_text_output = xreturn-message.
      EXIT.
    ENDLOOP.
    LOOP AT it_index.
      gt_out-msg =   xreturn-message.
      gt_out-icon = icon_led_yellow.
      CLEAR gt_out-belum.
      MODIFY gt_out INDEX it_index-index
      TRANSPORTING icon belum msg.
    ENDLOOP.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM save_log_table TABLES it_index.

  ENDIF.

  CLEAR :headerdata.
  __cls : itemdata,gl_data, xreturn, it_index .

ENDFORM.                    " post_bapi_invoice
*&---------------------------------------------------------------------*
*&      Form  pre_assign_entry_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_ITAB  text
*      -->P_$TARGET  text
*----------------------------------------------------------------------*
FORM pre_assign_entry_no TABLES   p_lt_itab STRUCTURE it_row_tab
                         USING    $matnr
                                  $key
                                  is_simulation
                      CHANGING    $target
                                  $converted_qty
                                  p_uom.
  DATA : $balance LIKE i_bsis-menge,
         $tabix LIKE sy-tabix,
         $tline LIKE sy-tabix.
  $balance = 0.

  READ TABLE p_lt_itab WITH KEY matnr = $matnr BINARY SEARCH.

  CHECK sy-subrc EQ 0.

* UD1K940995 BY IG.MOON
* {
  IF is_simulation EQ true AND p_uom NE space.
    IF p_uom NE p_lt_itab-meins AND $converted_qty <> 0.

      PERFORM unit_convert USING p_lt_itab-meins
                        CHANGING $converted_qty
                                 p_uom.

      IF $converted_qty <> 0.
        $target = $converted_qty .
      ENDIF.

    ENDIF.
  ENDIF.
* }

  LOOP AT p_lt_itab FROM sy-tabix WHERE $key EQ space.

    IF p_lt_itab-matnr NE $matnr.
      EXIT.
    ENDIF.

    MOVE $key TO p_lt_itab-$key.
    MODIFY p_lt_itab INDEX sy-tabix TRANSPORTING $key .

    IF p_lt_itab-bmenge <= $target.
      $target = $target - p_lt_itab-bmenge.
    ELSE.

      IF p_lt_itab-bal_rv IS INITIAL.
        $balance = p_lt_itab-bmenge - $target.
      ELSE.
        $balance = p_lt_itab-bmenge.
      ENDIF.
      $target = 0.

      IF is_simulation EQ space.

*       Check flag of balance is transfered.
        p_lt_itab-bal_tr = true.

        MODIFY p_lt_itab INDEX sy-tabix.
        CLEAR : p_lt_itab-$key,p_lt_itab-bal_tr .

*       Check flag if balance is reveiced.
        p_lt_itab-bal_rv = p_lt_itab-ebeln.
        $tabix = sy-tabix + 1.
        DESCRIBE TABLE p_lt_itab LINES $tline.

*******************************************
        p_lt_itab-bmenge = $balance. " Balance Transfer

        IF $tline > $tabix.
          INSERT p_lt_itab INDEX $tabix.
        ELSE.
          APPEND p_lt_itab.
        ENDIF.

      ENDIF.

    ENDIF.

    IF $target = 0.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " pre_assign_entry_no
*&---------------------------------------------------------------------*
*&      Form  GET_NEW_TARGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_BSIS  text
*      -->P_I_BSIS_MATNR  text
*      <--P_$TARGET  text
*----------------------------------------------------------------------*
FORM get_new_target TABLES   p_i_bsis  STRUCTURE i_bsis
                             p_lt_itab STRUCTURE it_row_tab
                     USING   $entno
                             $matnr
                             p_uom
                    CHANGING $target.

  DATA : BEGIN OF i_target OCCURS 0,
           entno  LIKE ztmm_duty_it-entno,
           menge  LIKE ztmm_duty_it-menge,
           trate  TYPE p DECIMALS 5,
         END   OF  i_target.

  DATA : BEGIN OF i_matnr OCCURS 0,
           matnr  LIKE ekpo-matnr,
           menge  LIKE ztmm_duty_it-menge,
         END   OF  i_matnr.

  DATA $sum LIKE ztmm_duty_it-menge.
  DATA flag(1).

  LOOP AT p_i_bsis WHERE  entno EQ $entno
                     AND  matnr EQ $matnr.

* Unit Converting {
    IF p_i_bsis-uom NE space AND p_i_bsis-uom NE p_uom.
      PERFORM unit_convert USING p_uom
                        CHANGING p_i_bsis-menge
                                 p_i_bsis-uom.
    ENDIF.
* }

    i_target-entno = p_i_bsis-entno.
    i_target-menge = p_i_bsis-menge.
    APPEND i_target.
  ENDLOOP.

  LOOP AT i_target.
    AT LAST.
      SUM.
      $sum = i_target-menge .
    ENDAT.
  ENDLOOP.

  LOOP AT i_target.
    IF $sum > 0 AND i_target-menge > 0.
      i_target-trate = i_target-menge / $sum.
      MODIFY i_target INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  SORT i_target BY trate.


  DATA : sum_menge LIKE p_i_bsis-$menge,
         sum_bal  LIKE p_i_bsis-$menge,
         $line TYPE i.

  LOOP AT p_lt_itab WHERE matnr EQ $matnr.
    i_matnr-matnr = p_lt_itab-matnr.
    i_matnr-menge = p_lt_itab-bmenge.
    COLLECT i_matnr.
  ENDLOOP.

  READ TABLE i_matnr INDEX 1.
  IF sy-subrc EQ 0.
    sum_bal = i_matnr-menge.
  ELSE.
    EXIT.
  ENDIF.

* Set the new target with available balance .
  DESCRIBE TABLE i_target LINES $line.
  IF $line > 1.
    LOOP AT i_target.
      AT LAST.
        flag = true.
      ENDAT.
      READ TABLE p_i_bsis WITH KEY entno = i_target-entno
                                   matnr = $matnr
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF flag EQ true.
          p_i_bsis-$menge = trunc( i_matnr-menge - sum_menge ).
        ELSE.
          p_i_bsis-$menge = trunc( i_matnr-menge * i_target-trate ).
        ENDIF.

* Unit Converting {
        IF p_i_bsis-uom NE space AND p_i_bsis-uom NE p_uom.
          PERFORM unit_convert USING p_uom
                            CHANGING p_i_bsis-menge
                                     p_i_bsis-uom.
        ENDIF.
* }
        MODIFY p_i_bsis INDEX sy-tabix.
        sum_menge = sum_menge + p_i_bsis-$menge .
      ENDIF.
    ENDLOOP.
  ELSE.
    READ TABLE i_target INDEX 1.
    IF sy-subrc EQ 0.
      READ TABLE p_i_bsis WITH KEY entno = i_target-entno
                                   matnr = $matnr
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        p_i_bsis-$menge = trunc( i_matnr-menge * i_target-trate ).
* Unit Converting {
        IF p_i_bsis-uom NE space AND p_i_bsis-uom NE p_uom.
          PERFORM unit_convert USING p_uom
                            CHANGING p_i_bsis-menge
                                     p_i_bsis-uom.
        ENDIF.
* }
      ENDIF.
      MODIFY p_i_bsis INDEX sy-tabix.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_NEW_TARGET
*&---------------------------------------------------------------------*
*&      Form  PREPARE_ASSIGN_BY_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_BSIS  text
*      -->P_P_LT_ITAB  text
*      -->P_I_BSIS_MATNR  text
*      -->P_$KEY  text
*----------------------------------------------------------------------*
FORM prepare_assign_by_quantity TABLES   p_i_bsis STRUCTURE i_bsis
                                         p_lt_itab STRUCTURE it_row_tab
                                USING    is_simulation.

  DATA : $target LIKE i_bsis-menge,
         flag(1),
         $key(60),
         $ix LIKE sy-tabix.
* UD1K940995 BY IG.MOON
* {
  DATA : $converted_qty LIKE i_bsis-menge,
         $uom LIKE i_bsis-uom.
* }

  $total_cnt = total_doc_cnt.
  CLEAR current_doc_cnt.
  SORT p_lt_itab BY matnr ebeln ebelp.

  LOOP AT p_i_bsis .

    ADD 1 TO current_doc_cnt.

    AT NEW matnr.
      flag = true .
    ENDAT.
    CHECK flag EQ true.
    $ix = sy-tabix.
* 28.02.2012(1)
*    CONCATENATE p_i_bsis-entno p_i_bsis-matnr INTO $key.
* 28.02.2012(1)
    CONCATENATE p_i_bsis-entno p_i_bsis-ebeln p_i_bsis-matnr INTO $key.

* UD1K940995 BY IG.MOON for Unit Conversion
* {
    $converted_qty = 0.
* }

    IF p_i_bsis-$menge <> 0.
      $target = p_i_bsis-$menge.
    ELSE.
      $target = p_i_bsis-menge.
      $converted_qty = p_i_bsis-menge.
      $uom = p_i_bsis-uom.
    ENDIF.

*   Pre-assign the P/Os for each entry# + material#

    PERFORM pre_assign_entry_no TABLES p_lt_itab
                                USING  p_i_bsis-matnr
                                       $key
                                       is_simulation
                             CHANGING  $target
                                       $converted_qty
                                       $uom.

    IF is_simulation EQ true AND p_i_bsis-$menge EQ 0.
      IF $converted_qty NE 0 AND $uom NE p_i_bsis-uom.
        p_i_bsis-menge = $converted_qty.
        p_i_bsis-uom = $uom.
        MODIFY p_i_bsis INDEX $ix TRANSPORTING uom menge.
      ENDIF.
    ENDIF.

    IF $target > 0.
      IF is_simulation EQ true.
        PERFORM get_new_target TABLES p_i_bsis
                                      p_lt_itab
                                USING p_i_bsis-entno
                                      p_i_bsis-matnr
                                      $uom
                             CHANGING $target.
      ENDIF.
    ENDIF.

    CLEAR flag.

    $mod = current_doc_cnt MOD 100.
    IF $mod EQ 0.
      $current_cnt = current_doc_cnt.
      CONCATENATE $current_cnt '/' $total_cnt
      INTO $text.
      CONDENSE $text.
      IF is_simulation EQ space.
        CONCATENATE 'Adjusting...' $text INTO $prog_text.
      ELSE.
        CONCATENATE 'Simulating...' $text INTO $prog_text.
      ENDIF.

      percentage = current_doc_cnt / total_doc_cnt * 100.
      PERFORM show_progress USING $prog_text percentage.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " PREPARE_ASSIGN_BY_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  set_filed_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XCOLOR  text
*      -->P_GT_OUT_TABCOLOR  text
*      -->P_1564   text
*      -->P_1565   text
*----------------------------------------------------------------------*
FORM set_filed_color TABLES   p_gt_out STRUCTURE gt_out
                      USING   p_fieldname
                              p_color.
  DATA: xcolor TYPE slis_specialcol_alv.
  CLEAR xcolor.
  xcolor-fieldname = p_fieldname.
  xcolor-color-col = p_color.
  xcolor-color-int = '0'. "Intensified on/off
  xcolor-color-inv = '0'.
  APPEND xcolor TO p_gt_out-tabcolor.
ENDFORM.                    " set_filed_color
*&---------------------------------------------------------------------*
*&      Form  DIST_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM adjust_and_dist_amount.
*     Calculate the amount to be distributed accroding to BSIS

  __cls i_write_off.

  DESCRIBE TABLE i_bsis LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.
  CLEAR current_doc_cnt.
*^^*

  DATA : flag(1).

  SORT : i_bsis BY entno,
         gt_calc BY $key.

*  CLEAR I_BSIS-MATNR.
*
*  MODIFY I_BSIS TRANSPORTING MATNR WHERE MATNR NE SPACE.

  LOOP AT i_bsis.
    ADD 1 TO current_doc_cnt.

    $ix = sy-tabix.

    AT NEW $key.
      flag = true.
    ENDAT.

    IF flag EQ true.
      CLEAR flag.

* 02.27.2012(delete start), mjc
* UD1K941354 - by IG.MOON 8/17/2007 {
      IF NOT i_bsis-$dst_amt IS INITIAL.
        i_bsis-dmbtr = i_bsis-$dst_amt.
      ENDIF.
* }
* 02.27.2012(delete end), mjc

* 02.27.2012(insert start), mjc
*        i_bsis-dmbtr = i_bsis-$dst_amt.
* 02.27.2012(insert end), mjc

      PERFORM dist_amount_by_bsis USING i_bsis-$key
                                        i_bsis-entno
                                        i_bsis-matnr
                                        i_bsis-dmbtr.
    ENDIF.

    $mod = current_doc_cnt MOD 100.
    IF $mod EQ 0.
      $current_cnt = current_doc_cnt.
      CONCATENATE $current_cnt '/' $total_cnt
      INTO $text.
      CONDENSE $text.
      CONCATENATE 'Dist.Amt...' $text INTO $prog_text.
      percentage = current_doc_cnt / total_doc_cnt * 100.
      PERFORM show_progress USING $prog_text percentage.
    ENDIF.

  ENDLOOP.

  IF p_entry EQ '08'.
    LOOP AT gt_calc.
      gt_calc-dmbtr = gt_calc-dstamt.
      MODIFY gt_calc.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DIST_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  DIST_AMOUNT_BY_BSIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CALC  text
*      -->P_I_BSIS_DMBTR  text
*----------------------------------------------------------------------*
FORM dist_amount_by_bsis USING    p_$key
                                  p_entno
                                  p_matnr
                                  p_amount.

  CHECK : p_amount <> 0,
          p_entry NE '08'.

  DATA : BEGIN OF $itab OCCURS 0,
           index     LIKE sy-tabix,
           dtyamt    LIKE ztcou124_new-duty_amt, "Amount
           dstamt    LIKE ztcou124_new-dstamt, "Amount
         END   OF  $itab.
  DATA $ix LIKE sy-tabix.

  READ TABLE gt_calc WITH KEY $key = p_$key BINARY SEARCH.
  IF sy-subrc EQ 0.
    $ix = sy-tabix.
  ELSE.
    EXIT.
  ENDIF.

  LOOP AT gt_calc FROM $ix.
    IF gt_calc-matnr EQ p_matnr
     AND gt_calc-entno EQ p_entno.
      $itab-index  = sy-tabix.
      IF gt_calc-dmbtr <> 0.
        $itab-dtyamt = gt_calc-dmbtr.
        APPEND $itab .
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

  DATA : $dtyamt LIKE ztcou124_new-duty_amt,
         $adjust LIKE ztcou124_new-duty_amt,
         $dstamt LIKE ztcou124_new-duty_amt.

  LOOP AT $itab.
    AT LAST.
      SUM.
      $dtyamt = abs( $itab-dtyamt ).
    ENDAT.
  ENDLOOP.

  IF $dtyamt <> p_amount .
    i_write_off-entno = p_entno.
    i_write_off-dmbtr = p_amount - $dtyamt.
    APPEND i_write_off.                             " Keep for debug

    LOOP AT $itab.
      $itab-dstamt = ( $itab-dtyamt * p_amount ) / $dtyamt .
      MODIFY $itab.
    ENDLOOP.

    LOOP AT $itab.
      AT LAST.
        SUM.
        $dstamt = abs( $itab-dstamt ).
      ENDAT.
    ENDLOOP.

    $adjust = abs( $dstamt ) - abs( p_amount ) .
    SORT $itab BY dstamt DESCENDING.

    READ TABLE $itab INDEX 1.        " Adjust diff.Amt into the biggest.
    IF sy-subrc EQ 0.
      IF $dstamt < p_amount .
        $itab-dstamt = $itab-dstamt + $adjust.
      ELSE.
        $itab-dstamt = $itab-dstamt - $adjust.
      ENDIF.
      MODIFY $itab INDEX 1.
    ENDIF.

    LOOP AT $itab.
      READ TABLE gt_calc INDEX $itab-index.
      IF sy-subrc EQ 0.
        gt_calc-dstamt = $itab-dstamt.
        MODIFY gt_calc INDEX $itab-index TRANSPORTING dstamt.
      ENDIF.
    ENDLOOP.

  ELSE.
    LOOP AT gt_calc WHERE matnr EQ p_matnr
                      AND entno EQ p_entno.
      MOVE gt_calc-dmbtr TO gt_calc-dstamt.
      MODIFY gt_calc INDEX sy-tabix.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DIST_AMOUNT_BY_BSIS
*&---------------------------------------------------------------------*
*&      Form  WRITE_OFF_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_IT_INV  text
*      <--P_ITEM_CNT  text
*----------------------------------------------------------------------*
FORM write_off_item TABLES   p_it_inv STRUCTURE it_inv
                    USING    $gross_amount
                             $grs_dist_amt
                    CHANGING $gl_doc_cnt .

  CHECK p_entry NE '08'.

  DATA $adjust LIKE it_inv-dmbtr.

  IF $grs_dist_amt EQ $gross_amount .
    EXIT.
  ENDIF.


* $GRS_DIST_AMT > $GROSS_AMOUNT ? debit posting (S) : credit posting (H)
  $adjust = abs( $grs_dist_amt ) - abs( $gross_amount ).

  ADD 1 TO $gl_doc_cnt.

  gl_data-invoice_doc_item = $gl_doc_cnt.
  gl_data-gl_account = p_w_off.
  gl_data-alloc_nmbr = p_it_inv-entno.
  gl_data-comp_code  = p_bukrs.
  gl_data-base_uom   = p_it_inv-meins.
  gl_data-gl_account = p_w_off.

  IF $adjust > 0.
    gl_data-db_cr_ind = 'S'.
  ELSE.
    gl_data-db_cr_ind = 'H'.
  ENDIF.

  gl_data-item_amount = abs( $adjust ) .
  gl_data-quantity    = 0 .

  APPEND : gl_data.
  CLEAR  : gl_data.

ENDFORM.                    " WRITE_OFF_ITEM
*&---------------------------------------------------------------------*
*&      Form  LOAD_SAVED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_saved_data.

  __cls : gt_calc, gt_out .

* Get data from table : ZTCOU124_NEW
  SELECT  *
    INTO CORRESPONDING FIELDS OF TABLE gt_calc
    FROM    ztcou124_new
    WHERE     pdate        IN    s_date
      AND     matnr        IN    s_matnr
      AND     entno        IN    s_entno
      AND     bukrs        EQ    p_bukrs
      AND     kschl        EQ    p_kschl
      AND     fentp        EQ    p_entry.

  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    EXIT.
  ENDIF.

  LOOP AT gt_calc.
    IF NOT gt_calc-belum  IS INITIAL.    " Check Reverse
      SELECT SINGLE * FROM  rbkp
                      INTO *rbkp
                     WHERE  belnr EQ gt_calc-belum
                       AND  gjahr EQ gt_calc-gjahr
                       AND  stjah NE space
                       AND  stblg NE space.
      IF sy-subrc EQ 0.
        IF p_incr EQ true.
          gt_calc-rev = true.
          MODIFY gt_calc INDEX sy-tabix.
        ELSE.
          DELETE gt_calc INDEX sy-tabix.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
    PERFORM write_num_to_string TABLES gt_calc.
    MODIFY gt_calc INDEX sy-tabix.
  ENDLOOP.

  READ TABLE gt_calc INDEX 1.
  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    EXIT.
  ENDIF.

*///////////////////// temp
  DATA $gt_calc LIKE gt_calc OCCURS 0 WITH HEADER LINE.
  DATA $ix LIKE sy-tabix.
  DATA $flag(1).

  LOOP AT gt_calc.
    $ix = sy-tabix.
    CHECK gt_calc-icon EQ icon_led_yellow.
    $gt_calc = gt_calc.
    APPEND $gt_calc .
    DELETE gt_calc INDEX $ix.
  ENDLOOP.

  SORT $gt_calc BY entno matnr bukrs kschl fentp ebeln ebelp.

  LOOP AT $gt_calc.
    AT NEW ebelp.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    gt_calc = $gt_calc.
    CLEAR gt_calc-belum.
    APPEND gt_calc.
    CLEAR $flag.
  ENDLOOP.

*///////////////////// temp

  PERFORM get_gt_out.

  __process 'Prepare Screen...' '98'. "98%

ENDFORM.                    " LOAD_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_NUM_TO_STRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GT_CALC  text
*----------------------------------------------------------------------*
FORM write_num_to_string TABLES p_gt_calc STRUCTURE gt_calc.

  IF p_gt_calc-waers IS INITIAL.
    p_gt_calc-waers = 'USD'.
  ENDIF.

  WRITE:
      p_gt_calc-xmenge     TO p_gt_calc-xmenges  UNIT p_gt_calc-meins,
      p_gt_calc-xmenge2    TO p_gt_calc-xmenge2s UNIT p_gt_calc-meins,
      p_gt_calc-entamount  TO p_gt_calc-ivamts   UNIT p_gt_calc-waers,
      p_gt_calc-duty_amt   TO p_gt_calc-dtamts   UNIT p_gt_calc-waers,
      p_gt_calc-dtyup      TO p_gt_calc-dtyups NO-ZERO.

  IF p_gt_calc-fentp IS INITIAL.
    p_gt_calc-fentp = p_entry.
  ENDIF.

ENDFORM.                    " WRITE_NUM_TO_STRING
*&---------------------------------------------------------------------*
*&      Form  UPDATE_FTZ_IF_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ftz_if_item.

  UPDATE ztmm_duty_it
     SET cl_doc_no = *ztcou124_new-belum
   WHERE entno = *ztcou124_new-entno
     AND matnr = *ztcou124_new-matnr.

ENDFORM.                    " UPDATE_FTZ_IF_TABLE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_FTZ_IF_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ftz_if_header USING p_flag.
  UPDATE ztmm_duty_hd
     SET clear_doc = p_flag
   WHERE entno = *ztcou124_new-entno.
ENDFORM.                    " UPDATE_FTZ_IF_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data_01.

  __process 'Read BSIS...' '30'.
  __cls : i_bsis.

  SELECT zuonr AS entno
         bukrs hkont sgtxt AS fentp
         gjahr
*        XBLNR AS BELNR                " reference doc.#
         xref3 AS belnr                " reference doc.#
         dmbtr
         waers AS curr
     INTO  CORRESPONDING FIELDS OF TABLE i_bsis
     FROM  bsis
     WHERE bukrs EQ p_bukrs
      AND  hkont EQ p_hkont
      AND  sgtxt EQ p_entry
      AND  zuonr IN s_entno.
*      AND  BUDAT IN s_entrd.
  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

  LOOP AT i_bsis.
    CLEAR gr_xblnr.
    gr_xblnr-sign   = 'I'.
    gr_xblnr-low    = i_bsis-belnr.
    gr_xblnr-option = 'EQ'.
    APPEND gr_xblnr.
  ENDLOOP.

  DELETE  gr_xblnr WHERE low IS INITIAL.

  READ TABLE gr_xblnr INDEX 1.
  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

  SORT gr_xblnr BY low .
  DELETE ADJACENT DUPLICATES FROM gr_xblnr COMPARING low.

ENDFORM.                    " GET_ROW_DATA_01
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA_08
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data_08.

  DATA gjahr TYPE  gjahr.

  __process 'Read FTZ I/F...' '30'.
  __cls : i_bsis.      " Share internal table with 06

  SELECT a~entno b~ebeln b~matnr
         a~fentp a~duty_amt AS dmbtr
         SUM( b~menge )  AS menge
         SUM( b~entamount ) AS entamount
         SUM( b~duty_amt ) AS duty_amt
         SUM( b~mpf_amt ) AS mpf_amt
         b~entcurr AS curr
         b~uom
         b~cl_doc_no
     INTO  CORRESPONDING FIELDS OF TABLE i_bsis
     FROM  ztmm_duty_hd AS a
     INNER JOIN ztmm_duty_it AS b
       ON  b~entno EQ a~entno
     WHERE a~fentp EQ p_entry
      AND  a~entno IN s_entno
      AND  b~matnr IN s_matnr
      AND  a~entdate IN s_entrd
     GROUP BY a~fentp a~entno a~duty_amt
              b~ebeln b~matnr b~entcurr b~uom b~cl_doc_no.

  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

* Country Key 'KR' process
  SORT i_bsis BY ebeln entno matnr entcurr uom cl_doc_no.
  LOOP AT i_bsis.
    $ix = sy-tabix.
* Vendor Account Number
    CLEAR: ekko.
    SELECT SINGLE lifnr
      INTO ekko-lifnr
      FROM ekko
     WHERE ebeln = i_bsis-ebeln.
* Country Key
    CLEAR: lfa1.
    SELECT SINGLE land1
      INTO lfa1-land1
      FROM lfa1
     WHERE lifnr = ekko-lifnr.
    IF lfa1-land1 = 'KR'.
      DELETE i_bsis INDEX $ix.
    ENDIF.
  ENDLOOP.

* UD1K941354 - by IG.MOON 8/17/2007 {
  PERFORM get_dst_amt.
* }

  __process 'Wait...' '40'.
  __cls gr_matnr.

  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = p_bukrs
      date  = p_date
    IMPORTING
      curry = gjahr.

  SORT i_bsis BY matnr entno.
  DATA $ix LIKE sy-tabix.

  LOOP AT i_bsis.
    $ix = sy-tabix.
    IF NOT i_bsis-cl_doc_no IS INITIAL.
      CLEAR *ztcou124_new.
      SELECT belum
             SUM( rmenge )  " Partial Posting Qty
             SUM( dmbtr )   " Partial Posting Amt
             SUM( dstamt )
            INTO (*ztcou124_new-belum,*ztcou124_new-rmenge,
                  *ztcou124_new-dmbtr,*ztcou124_new-dstamt)
            FROM ztcou124_new
            WHERE entno EQ i_bsis-entno
                  AND  matnr EQ i_bsis-matnr
                  AND  belum NE space
                  GROUP BY entno matnr belum.
        IF *ztcou124_new-rmenge <> 0.
          SELECT SINGLE belnr FROM rbkp
                          INTO *rbkp-belnr
                         WHERE  belnr EQ *ztcou124_new-belum
                           AND  gjahr EQ gjahr
                           AND  stjah EQ space
                           AND  stblg EQ space.
          IF sy-subrc EQ 0.
            i_bsis-menge = i_bsis-menge + *ztcou124_new-rmenge.
            i_bsis-duty_amt = i_bsis-duty_amt + *ztcou124_new-dmbtr.
            i_bsis-$dst_amt = i_bsis-$dst_amt + *ztcou124_new-dstamt.

            MODIFY i_bsis INDEX $ix TRANSPORTING menge duty_amt $dst_amt.

            CLEAR *ztcou124_new.
          ENDIF.
        ENDIF.
      ENDSELECT.
    ENDIF.

    IF i_bsis-menge EQ 0.
      DELETE i_bsis INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    IF i_bsis-matnr IN s_matnr.
    ELSE.
      DELETE i_bsis INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    MOVE : p_bukrs TO i_bsis-bukrs,
           p_hkont TO i_bsis-hkont,
           gjahr TO i_bsis-gjahr,
           i_bsis-duty_amt TO i_bsis-dmbtr.

    MODIFY i_bsis INDEX sy-tabix.

* Creating the Mat'l ranges for performance improvement of searching PO.
    CLEAR gr_matnr.
    gr_matnr-sign   = 'I'.
    gr_matnr-low    = i_bsis-matnr.
    gr_matnr-option = 'EQ'.
    APPEND gr_matnr.

    CLEAR gr_ebeln.
    gr_ebeln-sign   = 'I'.
    gr_ebeln-low    = i_bsis-ebeln.
    gr_ebeln-option = 'EQ'.
    APPEND gr_ebeln.

  ENDLOOP.

  READ TABLE gr_matnr INDEX 1.
  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

  SORT gr_matnr BY low .
  DELETE ADJACENT DUPLICATES FROM gr_matnr COMPARING low.
ENDFORM.                    " GET_ROW_DATA_08
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA_EKBZ_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data_ekbz_01.
  __process 'Read PO...' '45'.

  __cls it_row_tab.

* Get data from PO table
  SELECT  b~bukrs a~kschl b~matnr a~frbnr a~ebeln a~ebelp
          b~menge AS pmenge
          SUM( a~menge ) AS bmenge
          SUM( a~dmbtr ) AS dmbtr
          a~shkzg a~bewtp b~meins a~waers b~werks
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM    ekbz AS  a
  INNER JOIN ekpo AS b
  ON      b~ebeln      EQ    a~ebeln
  AND     b~ebelp      EQ    a~ebelp
  WHERE   a~frbnr      IN    gr_xblnr
  AND     b~bukrs      EQ    p_bukrs
*  AND     a~kschl      EQ    p_kschl
  AND     ( a~bewtp    EQ    'M' OR a~bewtp  EQ 'F' )
  AND     b~matnr      IN    s_matnr
  AND     a~ebeln      IN    s_ebeln
  GROUP BY b~matnr a~frbnr a~ebeln a~ebelp a~shkzg
           b~bukrs a~kschl a~bewtp b~menge
           b~brtwr b~meins a~waers b~werks.

  it_row_tab_bal[] = it_row_tab[].

ENDFORM.                    " GET_ROW_DATA_EKBZ_01
*&---------------------------------------------------------------------*
*&      Form  COLLECT_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ITAB  text
*----------------------------------------------------------------------*
FORM collect_itab TABLES p_lt_itab STRUCTURE it_row_tab.

  DATA : flag(1).

  LOOP AT it_row_tab.
    MOVE it_row_tab TO p_lt_itab .
    IF p_lt_itab-shkzg EQ 'S'. " is Debit ?
      p_lt_itab-bmenge = -1 * p_lt_itab-bmenge.
      p_lt_itab-dmbtr  = -1 * p_lt_itab-dmbtr.
    ENDIF.
    CLEAR : p_lt_itab-shkzg,p_lt_itab-bewtp,p_lt_itab-accup.
    COLLECT p_lt_itab.
  ENDLOOP.

  DELETE p_lt_itab WHERE bmenge <= 0.

ENDFORM.                    " COLLECT_ITAB
*&---------------------------------------------------------------------*
*&      Form  REFINE_ITAB_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_itab_01.
  DATA : lt_itab LIKE it_row_tab OCCURS 0 WITH HEADER LINE,
         lt_itab_for_unitprice LIKE it_row_tab OCCURS 0
                                           WITH HEADER LINE.
  __process 'Calc U/P...' '55'.

**   Calculate the unit price using by 'DCGR'
  PERFORM get_unit_price TABLES lt_itab_for_unitprice.


**   Collect EKBZ
  PERFORM collect_itab TABLES lt_itab.

**
  DATA $i_bsis LIKE i_bsis OCCURS 0 WITH HEADER LINE.
  DATA flag(1).
* create the matnr line for bsis

  SORT it_row_tab BY bukrs kschl frbnr matnr ebeln ebelp.
  SORT i_bsis BY entno belnr.

  LOOP AT i_bsis.
    AT NEW belnr.
      flag = true.
    ENDAT.
    CHECK flag EQ true.
    $i_bsis = i_bsis.
    LOOP AT it_row_tab WHERE frbnr = i_bsis-belnr.
      AT NEW matnr.
        SUM.
        $i_bsis-matnr    = it_row_tab-matnr.
        $i_bsis-menge    = it_row_tab-bmenge.

* 10/22/2--76 by IG.MOON {
*        $I_BSIS-DUTY_AMT = $I_BSIS-DMBTR.
        $i_bsis-duty_amt = it_row_tab-dmbtr. "$I_BSIS-DMBTR.
*}
        APPEND $i_bsis.
      ENDAT.
    ENDLOOP.
  ENDLOOP.
  __cls i_bsis.
  i_bsis[] = $i_bsis[].

* UD1K941354 - by IG.MOON 10/19/2007 {
  PERFORM get_dst_amt_01.
* }

**   Distribute PO by Entry# MATNR
  PERFORM distribute_po_data TABLES lt_itab.

***   Fill U/P,P/O Qty,Balance to IT_ROW_TAB
  PERFORM recalc_balance TABLES lt_itab
                                lt_itab_for_unitprice .

ENDFORM.                    " REFINE_ITAB_01
*&---------------------------------------------------------------------*
*&      Form  CHECK_PARTIAL_POST_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_partial_post_01 TABLES p_gt_calc STRUCTURE gt_calc .

  LOOP AT p_gt_calc.
    SELECT belum rmenge icon msg
            FROM  ztcou124_new
            INTO (p_gt_calc-belum,
                  p_gt_calc-pstqty,
                  p_gt_calc-icon,
                  p_gt_calc-msg)
            WHERE entno EQ p_gt_calc-entno
              AND matnr EQ p_gt_calc-matnr
              AND bukrs EQ p_gt_calc-bukrs
              AND kschl EQ p_gt_calc-kschl
              AND fentp EQ p_entry
              AND ebeln EQ p_gt_calc-ebeln
              AND ebelp EQ p_gt_calc-ebelp .
      IF p_gt_calc-belum NE space.
        SELECT SINGLE belnr FROM  rbkp
                        INTO *rbkp-belnr
                       WHERE  belnr EQ p_gt_calc-belum
                         AND  gjahr EQ p_gt_calc-gjahr
                         AND  stblg EQ space
                         AND  stjah EQ space.
        IF sy-subrc EQ 0.
          MODIFY p_gt_calc INDEX sy-tabix
                           TRANSPORTING belum pstqty icon msg.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

ENDFORM.                    " CHECK_PARTIAL_POST_01
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plant.
  __cls  : gt_plant, gr_bwkey.

* Get plant
  SELECT bwkey INTO TABLE gt_plant
    FROM t001k
   WHERE bukrs = p_bukrs.

  LOOP AT gt_plant.
    gr_bwkey-sign = 'I'.
    gr_bwkey-option = 'EQ'.
    gr_bwkey-low = gt_plant-bwkey.

    APPEND gr_bwkey.
    CLEAR gr_bwkey.
  ENDLOOP.


ENDFORM.                    " GET_PLANT
*&---------------------------------------------------------------------*
*&      Form  get_po_one_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
FORM get_po_one_time TABLES $gr_matnr STRUCTURE gr_matnr.

* Get data from PO table
  SELECT  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
          a~menge AS pmenge
          SUM( b~menge ) AS bmenge
          SUM( b~dmbtr ) AS dmbtr
          b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM    ekpo AS  a  INNER JOIN ekbz AS b
  ON      a~ebeln      EQ    b~ebeln
  AND     a~ebelp      EQ    b~ebelp
  WHERE   a~matnr      IN    $gr_matnr
  AND     a~werks      IN    gr_bwkey
  AND     a~bukrs      EQ    p_bukrs
  AND     b~kschl      EQ    p_kschl
  AND     ( b~bewtp    EQ    'M' OR b~bewtp  EQ 'F' )
  AND     a~ebeln      IN    s_ebeln

  GROUP BY a~matnr a~ebeln a~ebelp b~shkzg
           a~bukrs b~kschl b~bewtp a~menge
           a~brtwr a~meins b~waers b~lifnr a~werks
           b~zaehk.                                         "UD1K950699

* BEGIN OF UD1K950699
  SORT it_row_tab BY
  matnr ebeln ebelp shkzg bukrs kschl bewtp meins waers werks.

  DELETE ADJACENT DUPLICATES FROM it_row_tab COMPARING
  matnr ebeln ebelp shkzg bukrs kschl bewtp meins waers werks.
* END OF UD1K950699

*  IF SY-SUBRC <> 0.
**    G_ERROR = TRUE.
**    MESSAGE S000 WITH 'Could not find data.'.
*    EXIT.
*  ENDIF.


ENDFORM.                    " get_po_one_time
*&---------------------------------------------------------------------*
*&      Form  get_po_one_time_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
FORM get_po_one_time_new TABLES $gr_ebeln STRUCTURE gr_ebeln.

* Get data from PO table
  SELECT  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
          a~menge AS pmenge
          SUM( b~menge ) AS bmenge
          SUM( b~dmbtr ) AS dmbtr
          b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
  INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM    ekpo AS  a  INNER JOIN ekbz AS b
  ON      a~ebeln      EQ    b~ebeln
  AND     a~ebelp      EQ    b~ebelp
  WHERE   a~ebeln      IN    $gr_ebeln
  AND     a~werks      IN    gr_bwkey
  AND     a~bukrs      EQ    p_bukrs
***  AND     b~kschl      EQ    p_kschl
  AND     ( b~bewtp    EQ    'M' OR b~bewtp  EQ 'F' )
  AND     a~matnr      IN    s_matnr
  AND     a~ebeln      IN    s_ebeln

  GROUP BY a~matnr a~ebeln a~ebelp b~shkzg
           a~bukrs b~kschl b~bewtp a~menge
           a~brtwr a~meins b~waers b~lifnr a~werks
           b~zaehk.                                         "UD1K950699

* BEGIN OF UD1K950699
  SORT it_row_tab BY
  ebeln matnr ebelp shkzg bukrs kschl bewtp meins waers werks.

  DELETE ADJACENT DUPLICATES FROM it_row_tab COMPARING
  ebeln matnr ebelp shkzg bukrs kschl bewtp meins waers werks.
* END OF UD1K950699

*  IF SY-SUBRC <> 0.
**    G_ERROR = TRUE.
**    MESSAGE S000 WITH 'Could not find data.'.
*    EXIT.
*  ENDIF.


ENDFORM.                    " get_po_one_time_new
*&---------------------------------------------------------------------*
*&      Form  get_po_several_times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
FORM get_po_several_times TABLES $gr_matnr STRUCTURE gr_matnr.

* Get data from PO table
  SELECT  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
          a~menge AS pmenge
          SUM( b~menge ) AS bmenge
          SUM( b~dmbtr ) AS dmbtr
          b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
  APPENDING CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM    ekpo AS  a  INNER JOIN ekbz AS b
  ON      a~ebeln      EQ    b~ebeln
  AND     a~ebelp      EQ    b~ebelp
  WHERE   a~matnr      IN    $gr_matnr
  AND     a~werks      IN    gr_bwkey
  AND     a~bukrs      EQ    p_bukrs
****  AND     b~kschl      EQ    p_kschl
  AND     ( b~bewtp    EQ    'M' OR b~bewtp  EQ 'F' )
  AND     a~ebeln      IN    s_ebeln
  GROUP BY a~matnr a~ebeln a~ebelp b~shkzg
           a~bukrs b~kschl b~bewtp a~menge
           a~brtwr a~meins b~waers b~lifnr a~werks
           b~zaehk.                                         "UD1K950705

* BEGIN OF UD1K950705
  SORT it_row_tab BY
  matnr ebeln ebelp shkzg bukrs kschl bewtp meins waers werks.

  DELETE ADJACENT DUPLICATES FROM it_row_tab COMPARING
  matnr ebeln ebelp shkzg bukrs kschl bewtp meins waers werks.
* END OF UD1K950705

*  IF SY-SUBRC <> 0.
*    G_ERROR = TRUE.
*    MESSAGE S000 WITH 'Could not find data.'.
*    EXIT.
*  ENDIF.


ENDFORM.                    " get_po_several_times
*&---------------------------------------------------------------------*
*&      Form  get_po_several_times_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GR_MATNR  text
*----------------------------------------------------------------------*
FORM get_po_several_times_new TABLES $gr_ebeln STRUCTURE gr_ebeln.

* Get data from PO table
  SELECT  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
          a~menge AS pmenge
          SUM( b~menge ) AS bmenge
          SUM( b~dmbtr ) AS dmbtr
          b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
  APPENDING CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM    ekpo AS  a  INNER JOIN ekbz AS b
  ON      a~ebeln      EQ    b~ebeln
  AND     a~ebelp      EQ    b~ebelp
  WHERE   a~ebeln      IN    $gr_ebeln
  AND     a~werks      IN    gr_bwkey
  AND     a~bukrs      EQ    p_bukrs
***  AND     b~kschl      EQ    p_kschl
  AND     ( b~bewtp    EQ    'M' OR b~bewtp  EQ 'F' )
  AND     a~matnr      IN    s_matnr
  AND     a~ebeln      IN    s_ebeln

  GROUP BY a~matnr a~ebeln a~ebelp b~shkzg
           a~bukrs b~kschl b~bewtp a~menge
           a~brtwr a~meins b~waers b~lifnr a~werks
           b~zaehk.                                         "UD1K950705

* BEGIN OF UD1K950705
  SORT it_row_tab BY
  ebeln matnr ebelp shkzg bukrs kschl bewtp meins waers werks.

  DELETE ADJACENT DUPLICATES FROM it_row_tab COMPARING
  ebeln matnr ebelp shkzg bukrs kschl bewtp meins waers werks.
* END OF UD1K950705

*  IF SY-SUBRC <> 0.
*    G_ERROR = TRUE.
*    MESSAGE S000 WITH 'Could not find data.'.
*    EXIT.
*  ENDIF.


ENDFORM.                    " get_po_several_times_new
*&---------------------------------------------------------------------*
*&      Form  UNIT_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FROM_U  text
*      -->P_TO_U  text
*      -->P_QTY1  text
*      <--P_RESULT_QTY  text
*----------------------------------------------------------------------*
FORM unit_convert USING    p_to_u
                  CHANGING p_result_qty
                           p_from_u.

  DATA $p_result_qty LIKE i_bsis-menge.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input                = p_result_qty
      unit_in              = p_from_u
      unit_out             = p_to_u
    IMPORTING
      output               = $p_result_qty
    EXCEPTIONS
      conversion_not_found = 1
      division_by_zero     = 2
      input_invalid        = 3
      output_invalid       = 4
      overflow             = 5
      type_invalid         = 6
      units_missing        = 7
      unit_in_not_found    = 8
      unit_out_not_found   = 9.

  IF sy-subrc <> 0.
*           ALT UoM
    DATA : l_umrez_f TYPE umrez,
           l_umrez_t TYPE umrez.

    CLEAR : l_umrez_f,l_umrez_t.

*    SELECT SINGLE UMREZ INTO :
*              L_UMREZ_F FROM MARM
*             WHERE MATNR = I_BSIS-MATNR
*             AND MEINH = P_FROM_U,
*
*              L_UMREZ_T FROM MARM
*             WHERE MATNR = I_BSIS-MATNR
*             AND MEINH = P_TO_U.
*
    SELECT SINGLE umrez umren INTO :
              (l_umrez_f, l_umrez_t) FROM marm
             WHERE matnr = i_bsis-matnr
             AND meinh = p_from_u.

    IF l_umrez_f <> 0 AND  l_umrez_t <> 0.
      $p_result_qty = p_result_qty * ( l_umrez_f / l_umrez_t ).
      p_result_qty = $p_result_qty.
      p_from_u = p_to_u.
    ELSE.
* error
      p_result_qty = 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " UNIT_CONVERT
*&---------------------------------------------------------------------*
*&      Form  UNIT_CONVERT_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_BSIS  text
*      -->P_P_LT_ITAB  text
*----------------------------------------------------------------------*
FORM unit_convert_batch TABLES p_i_bsis STRUCTURE i_bsis
                               p_lt_itab STRUCTURE it_row_tab .

  SORT p_lt_itab BY matnr ebeln ebelp.

  LOOP AT p_i_bsis .
    $ix = sy-tabix.
    READ TABLE p_lt_itab WITH KEY matnr = p_i_bsis-matnr BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    IF i_bsis-uom NE space AND i_bsis-uom NE p_lt_itab-meins.
      PERFORM unit_convert USING p_lt_itab-meins
                        CHANGING p_i_bsis-menge
                                 i_bsis-uom.
      IF p_i_bsis-menge NE 0 AND i_bsis-uom NE space.
        MODIFY i_bsis INDEX $ix TRANSPORTING uom menge.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " UNIT_CONVERT_BATCH
*&---------------------------------------------------------------------*
*&      Form  COLLECT_GL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_gl_data.
  DATA $gl_data LIKE gl_data OCCURS 0 WITH HEADER LINE.

  LOOP AT gl_data.
    $gl_data = gl_data.
    CLEAR $gl_data-invoice_doc_item.
    COLLECT $gl_data.
    CLEAR $gl_data.
  ENDLOOP.

  LOOP AT $gl_data.
    $gl_data-invoice_doc_item = sy-tabix.
    MODIFY $gl_data.
  ENDLOOP.

  __cls gl_data.
  gl_data[] = $gl_data[].

ENDFORM.                    " COLLECT_GL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DST_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dst_amt.

  DATA : $dstamt LIKE i_bsis-duty_amt,
         $total_amt LIKE i_bsis-duty_amt,
         $i_bsis LIKE i_bsis OCCURS 0 WITH HEADER LINE,
         $flag(1),$flag_end(1),
         $ix LIKE sy-tabix,
         $check_amt LIKE i_bsis-duty_amt,
         $adj_amt LIKE i_bsis-duty_amt.


  DATA : BEGIN OF $entno OCCURS 0,
          entno  LIKE ztmm_duty_it-entno,
          total  LIKE bsis-dmbtr,
         END   OF  $entno.

  LOOP AT i_bsis.
    $i_bsis = i_bsis.
    $i_bsis-$key = i_bsis-matnr.
    CLEAR $i_bsis-matnr.
    APPEND $i_bsis.
  ENDLOOP.

  SORT $i_bsis BY entno duty_amt.

  LOOP AT $i_bsis.
    AT END OF entno.
      SUM.
      $total_amt = $i_bsis-duty_amt.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    $entno-entno = $i_bsis-entno.
    $entno-total = $total_amt.
    APPEND $entno.
    CLEAR : $entno,$flag.
  ENDLOOP.

  LOOP AT $i_bsis.
    $ix = sy-tabix.
    AT NEW entno.
      $flag = true.
    ENDAT.

    IF $flag EQ true.
      READ TABLE $entno WITH KEY entno = $i_bsis-entno.
      $total_amt = $entno-total.
      CLEAR : $flag, $check_amt.
    ENDIF.

    CHECK $total_amt <> 0.

    $i_bsis-$dst_amt =
      ( $i_bsis-duty_amt * $i_bsis-dmbtr ) / $total_amt.

    MODIFY  $i_bsis INDEX $ix TRANSPORTING $dst_amt.

    $check_amt = $check_amt + $i_bsis-$dst_amt.

    AT END OF entno.
      $flag_end = true.
    ENDAT.

    CHECK $flag_end EQ true.
    CLEAR $flag_end.

    IF $check_amt <> $i_bsis-dmbtr.
      $adj_amt = $check_amt - $i_bsis-dmbtr.

      IF $i_bsis-dmbtr > $check_amt.
        $i_bsis-$dst_amt = $i_bsis-$dst_amt + abs( $adj_amt ).
      ELSE.
        $i_bsis-$dst_amt = $i_bsis-$dst_amt - abs( $adj_amt ).
      ENDIF.

    ENDIF.

    MODIFY  $i_bsis INDEX $ix TRANSPORTING $dst_amt.

  ENDLOOP.

  LOOP AT $i_bsis.
    $ix = sy-tabix.
    $i_bsis-matnr = $i_bsis-$key.
    $i_bsis-$key = space.
    MODIFY $i_bsis INDEX $ix TRANSPORTING matnr $key.
  ENDLOOP.

  __cls i_bsis.

  LOOP AT $i_bsis.
    IF $i_bsis-matnr IN s_matnr.
      i_bsis = $i_bsis.
      APPEND i_bsis.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_DST_AMT
*&---------------------------------------------------------------------*
*&      Form  save_log_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INDEX  text
*----------------------------------------------------------------------*
FORM save_log_table TABLES   p_it_index STRUCTURE it_index.

  DATA : flag1(1),flag2(1),
        $dmbtr LIKE gt_out-dmbtr.

  __cls i_ztcou124_new.

  LOOP AT p_it_index.
    CLEAR gt_out.
    CLEAR *ztcou124_new.
    READ TABLE gt_out INDEX p_it_index-index.
    MOVE-CORRESPONDING gt_out TO *ztcou124_new.

     *ztcou124_new-fentp = p_entry.
     *ztcou124_new-kschl = p_kschl.
     *ztcou124_new-pdate = p_date.
     *ztcou124_new-erdat = sy-datum.
     *ztcou124_new-ernam = sy-uname.

    APPEND *ztcou124_new TO i_ztcou124_new.

*    SELECT SINGLE * FROM ZTCOU124_NEW
*     WHERE entno EQ *ZTCOU124_NEW-entno
*    	AND matnr EQ *ZTCOU124_NEW-matnr
*    	AND bukrs EQ *ZTCOU124_NEW-bukrs
*    	AND kschl EQ *ZTCOU124_NEW-kschl
*    	AND fentp EQ *ZTCOU124_NEW-fentp
*    	AND ebeln EQ *ZTCOU124_NEW-ebeln
*    	AND ebelp EQ *ZTCOU124_NEW-ebelp
*    	AND belum EQ *ZTCOU124_NEW-belum .
*
*    IF sy-subrc EQ 0.
*      ZTCOU124_NEW = *ZTCOU124_NEW.
*      UPDATE ZTCOU124_NEW.
*    ELSE.
*      ZTCOU124_NEW = *ZTCOU124_NEW.
*      INSERT ZTCOU124_NEW.
*    ENDIF.
*    IF sy-subrc NE 0.
*      ROLLBACK WORK.
*    ELSE.
*      COMMIT WORK.
*    ENDIF.

  ENDLOOP.

  MODIFY ztcou124_new FROM TABLE i_ztcou124_new.
  COMMIT WORK.

ENDFORM.                    " save_log_table
*&---------------------------------------------------------------------*
*&      Form  GET_DST_AMT_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dst_amt_01.

  DATA : $dstamt LIKE i_bsis-duty_amt,
         $total_amt LIKE i_bsis-duty_amt,
         $i_bsis LIKE i_bsis OCCURS 0 WITH HEADER LINE,
         $flag(1),$flag_end(1),
         $ix LIKE sy-tabix,
         $check_amt LIKE i_bsis-duty_amt,
         $adj_amt LIKE i_bsis-duty_amt.


  DATA : BEGIN OF $entno OCCURS 0,
          entno  LIKE ztmm_duty_it-entno,
          total  LIKE bsis-dmbtr,
         END   OF  $entno.

  LOOP AT i_bsis.
    $i_bsis = i_bsis.
    $i_bsis-$key = i_bsis-matnr.
    CLEAR $i_bsis-matnr.
    APPEND $i_bsis.
  ENDLOOP.

  SORT $i_bsis BY entno duty_amt.

  LOOP AT $i_bsis.
    AT END OF entno.
      SUM.
      $total_amt = $i_bsis-duty_amt.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.

    $entno-entno = $i_bsis-entno.
    $entno-total = $total_amt.
    APPEND $entno.
    CLEAR : $entno,$flag.
  ENDLOOP.

  LOOP AT $i_bsis.
    $ix = sy-tabix.
    AT NEW entno.
      $flag = true.
    ENDAT.

    IF $flag EQ true.
      READ TABLE $entno WITH KEY entno = $i_bsis-entno.
      $total_amt = $entno-total.
      CLEAR : $flag, $check_amt.
    ENDIF.

    CHECK $total_amt <> 0.

    $i_bsis-$dst_amt =
    ( $i_bsis-duty_amt * $i_bsis-dmbtr ) / $total_amt.

* 10/22/2007 by IG.MOON {
    $i_bsis-duty_amt = $i_bsis-dmbtr.
* }

    MODIFY  $i_bsis INDEX $ix TRANSPORTING $dst_amt duty_amt.

    $check_amt = $check_amt + $i_bsis-$dst_amt.

    AT END OF entno.
      $flag_end = true.
    ENDAT.

    CHECK $flag_end EQ true.
    CLEAR $flag_end.

    IF $check_amt <> $i_bsis-dmbtr.
      $adj_amt = $check_amt - $i_bsis-dmbtr.

      IF $i_bsis-dmbtr > $check_amt.
        $i_bsis-$dst_amt = $i_bsis-$dst_amt + abs( $adj_amt ).
      ELSE.
        $i_bsis-$dst_amt = $i_bsis-$dst_amt - abs( $adj_amt ).
      ENDIF.

    ENDIF.

    MODIFY  $i_bsis INDEX $ix TRANSPORTING $dst_amt.

  ENDLOOP.

  LOOP AT $i_bsis.
    $ix = sy-tabix.
    $i_bsis-matnr = $i_bsis-$key.
    $i_bsis-$key = space.
    MODIFY $i_bsis INDEX $ix TRANSPORTING matnr $key.
  ENDLOOP.

  __cls i_bsis.

  LOOP AT $i_bsis.
    IF $i_bsis-matnr IN s_matnr.
      i_bsis = $i_bsis.
      APPEND i_bsis.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_DST_AMT_01
*&---------------------------------------------------------------------*
*&      Form  DELETE_LAND1_KR
*&---------------------------------------------------------------------*
FORM delete_land1_kr .

  DATA : BEGIN OF lt_lifnr OCCURS 0,
           lifnr           LIKE   ekko-lifnr,
         END   OF  lt_lifnr.
  DATA : BEGIN OF lt_land1 OCCURS 0,
           lifnr           LIKE   lfa1-lifnr,
           land1           LIKE   lfa1-land1,
         END   OF  lt_land1.
* lifnr Key
  DATA l_tabix TYPE i.
  LOOP AT it_row_tab.
    lt_lifnr-lifnr = it_row_tab-lifnr.
    APPEND lt_lifnr.
  ENDLOOP.
  SORT lt_lifnr BY lifnr.
  DELETE ADJACENT  DUPLICATES FROM lt_lifnr.

* 'KR' Data select
  SELECT lifnr land1
    INTO CORRESPONDING FIELDS OF TABLE lt_land1
    FROM lfa1
     FOR ALL ENTRIES  IN  lt_lifnr
   WHERE lifnr        =   lt_lifnr-lifnr.

* 'KR' move
  SORT lt_land1 BY lifnr.
  LOOP AT it_row_tab.
    l_tabix  =  sy-tabix.
    CLEAR: lt_land1.
    READ TABLE lt_land1 WITH KEY lifnr = it_row_tab-lifnr
                                 BINARY SEARCH.
    it_row_tab-land1  = lt_land1-land1.
    MODIFY it_row_tab TRANSPORTING land1.
  ENDLOOP.

ENDFORM.                    " DELETE_LAND1_KR
*&---------------------------------------------------------------------*
*&      Form  END_OF_COMPUTE
*&---------------------------------------------------------------------*
FORM end_of_compute .

* 7. P/O qty.
  CLEAR: it_row_tab_bal.
  READ TABLE  it_row_tab_bal WITH KEY ebeln = gt_calc-ebeln
                                      matnr = gt_calc-matnr.
  gt_calc-ebelp  = it_row_tab_bal-ebelp.
  gt_calc-pmenge = it_row_tab_bal-pmenge.

* 8. GR/ Qty.
  CLEAR: it_row_tab_bal, gt_calc-gmenge.
  LOOP AT it_row_tab_bal WHERE ebeln = gt_calc-ebeln
                           AND matnr = gt_calc-matnr
                           AND kschl = p_kschl.
    gt_calc-gmenge = gt_calc-gmenge + it_row_tab_bal-bmenge.
  ENDLOOP.
* 9. Balance.
  gt_calc-bmenge = gt_calc-pmenge - gt_calc-gmenge.
* 10. Clr.Qty.

* 11. Qty.Aftr.

* 12. Acc.U/P
  IF gt_calc-accup = 0.
    IF gt_calc-gmenge <> 0.
      gt_calc-accup  = gt_calc-dmbtr  /  gt_calc-gmenge.
    ENDIF.
  ENDIF.
* 13. Acc.Amt.
  IF gt_calc-accamt = 0.
    gt_calc-accamt = gt_calc-accup    *  gt_calc-rmenge.
  ENDIF.
* 14. Var.Amt.
  IF gt_calc-varamt = 0.
    gt_calc-varamt = gt_calc-accamt   -  gt_calc-dstamt.
  ENDIF.

ENDFORM.                    " END_OF_COMPUTE
