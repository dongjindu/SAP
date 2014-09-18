*----------------------------------------------------------------------
* Program ID        : ZACOU155
* Title             : [CO] Duty ME Clearing(New 2)
* Created on        : 04/2/2013
* Created by        : I.G Moon
* Specifications By : Ravi
* Description       : Duty ME Reconciliation
*----------------------------------------------------------------------

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
* On 08/27/13 : The line items cannot be cleared
* if the amount is zero when clearing custom duty
*----------------------------------------------------------------------
REPORT zacou155 MESSAGE-ID zmco.

INCLUDE zacou155_top.
INCLUDE zacoui00.

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
                 s_matnr  FOR ekpo-matnr MODIF ID mat,
                 s_ebeln  FOR ekpo-ebeln MEMORY ID bes,
                 s_entrd  FOR ztmm_duty_hd-entdate .
PARAMETERS : p_expo AS CHECKBOX.
PARAMETERS : p_krpo no-display.
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
             p_w_off LIKE bsis-hkont OBLIGATORY DEFAULT '532160' MODIF ID pst,
             p_open_q LIKE bsis-hkont NO-DISPLAY DEFAULT '215010'.

SELECTION-SCREEN END OF BLOCK b2..

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

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

  CLEAR g_error.

  IF p_load EQ true .
    PERFORM load_saved_data.           " Load saved data
  ELSE.

    CASE p_entry.
      WHEN '06'.
        PERFORM get_row_data_06_new.   CHECK g_error EQ space.
        PERFORM get_row_data_ekbz_06_08_new.
        PERFORM refine_itab_06_08.
      WHEN '08'.
        PERFORM get_row_data_08.       CHECK g_error EQ space.
        PERFORM get_row_data_ekbz_06_08_new.
        PERFORM refine_itab_06_08.
      WHEN '01' .
        PERFORM get_row_data_01.       CHECK g_error EQ space.
        PERFORM get_row_data_ekbz_01.
        PERFORM refine_itab_06_08.
    ENDCASE.

    PERFORM calc_data.
    PERFORM get_gt_out.

  ENDIF.

  __process 'Prepare Screen...' '98'.                      "98%

  READ TABLE gt_out INDEX 1.
  IF sy-subrc <> 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
  ENDIF.

  CHECK g_error EQ space .
  CALL SCREEN 100.

END-OF-SELECTION.

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

    MOVE-CORRESPONDING gt_out TO *ztcou124.
     *ztcou124-fentp = p_entry.
     *ztcou124-kschl = p_kschl.
     *ztcou124-pdate = p_date.
     *ztcou124-erdat = sy-datum.
     *ztcou124-ernam = sy-uname.

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

  __cls : sum_i_bsis, i_bsis, it_row_tab_bal.

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
** Furong on 07/15/14   (
       and duty_amt <> 0
** )
       GROUP BY entno ebeln matnr  entcurr uom cl_doc_no.
  ENDLOOP.

  SORT i_bsis BY ebeln entno matnr entcurr uom cl_doc_no.


  IF p_krpo EQ true.
* Country Key 'KR' process
    SORT i_bsis BY ebeln entno matnr entcurr uom cl_doc_no.
    LOOP AT i_bsis.
      $ix = sy-tabix.
* Vendor Account Number
      CLEAR: ekko.
      SELECT SINGLE lifnr INTO ekko-lifnr FROM ekko
       WHERE ebeln = i_bsis-ebeln.
* Country Key
      CLEAR: lfa1.
      SELECT SINGLE land1 INTO lfa1-land1 FROM lfa1
       WHERE lifnr = ekko-lifnr.
      IF lfa1-land1 = 'KR' OR lfa1-land1 = ''.
        DELETE i_bsis INDEX $ix.
      ENDIF.
    ENDLOOP.
  ENDIF.

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

  PERFORM get_dst_amt.

  __process 'Wait...' '40'.

  SORT i_bsis BY matnr entno.

  LOOP AT i_bsis.
    $ix = sy-tabix.

    IF NOT i_bsis-cl_doc_no IS INITIAL.
      CLEAR *ztcou124.
      SELECT belum
             SUM( rmenge )  " Partial Posting Qty
             SUM( dmbtr )   " Partial Posting Amt
             SUM( dstamt )
            INTO (*ztcou124-belum,*ztcou124-rmenge,
                  *ztcou124-dmbtr,*ztcou124-dstamt)
            FROM ztcou124
            WHERE entno EQ i_bsis-entno
                  AND  matnr EQ i_bsis-matnr
                  AND  belum NE space
                  GROUP BY entno matnr belum.

        IF *ztcou124-rmenge <> 0.
          SELECT SINGLE belnr FROM rbkp
                          INTO *rbkp-belnr
                         WHERE  belnr EQ *ztcou124-belum
                           AND  gjahr EQ i_bsis-gjahr
                           AND  stjah EQ space
                           AND  stblg EQ space.

          IF sy-subrc EQ 0.
            i_bsis-already = true.
            MODIFY i_bsis INDEX $ix TRANSPORTING already.
*            i_bsis-menge    = i_bsis-menge    + *ztcou124-rmenge.
*            i_bsis-duty_amt = i_bsis-duty_amt + *ztcou124-dmbtr.
*            i_bsis-$dst_amt = i_bsis-$dst_amt + *ztcou124-dstamt.
*            MODIFY i_bsis INDEX $ix TRANSPORTING menge duty_amt $dst_amt.
            CLEAR *ztcou124.
          ENDIF.
        ENDIF.
      ENDSELECT.
    ENDIF.

*    IF i_bsis-duty_amt EQ 0.
*      DELETE i_bsis INDEX sy-tabix.
*      CONTINUE.
*    ENDIF.

    IF i_bsis-menge EQ 0.
      DELETE i_bsis INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    IF i_bsis-matnr IN s_matnr.
    ELSE.
      DELETE i_bsis INDEX sy-tabix.
      CONTINUE.
    ENDIF.
  ENDLOOP.

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

  __cls gt_calc.

  LOOP AT i_bsis.

    CLEAR *ztcou124.
    MOVE-CORRESPONDING i_bsis TO *ztcou124.
    MOVE : i_bsis-menge TO *ztcou124-xmenge,
           i_bsis-$menge TO *ztcou124-xmenge2,
           i_bsis-entcurr TO *ztcou124-waers,
           i_bsis-uom TO *ztcou124-meins.

    MOVE-CORRESPONDING i_bsis  TO gt_calc.
    MOVE-CORRESPONDING *ztcou124 TO gt_calc.

    gt_calc-rmenge = i_bsis-pstqty.

    IF i_bsis-menge <> 0.
       *ztcou124-dtyup = i_bsis-duty_amt / i_bsis-menge .
    ENDIF.

    MOVE : *ztcou124-entno         TO gt_calc-entno,
           *ztcou124-xmenge        TO gt_calc-xmenge,
           *ztcou124-xmenge2       TO gt_calc-xmenge2,
           *ztcou124-entamount     TO gt_calc-entamount,
           *ztcou124-duty_amt      TO gt_calc-duty_amt,
           *ztcou124-mpf_amt       TO gt_calc-mpf_amt,
           *ztcou124-dtyup         TO gt_calc-dtyup,
            i_bsis-pstqty              TO gt_calc-pstqty,
           *ztcou124-gjahr         TO gt_calc-gjahr,
           i_bsis-duty_amt             TO gt_calc-dmbtr,
           i_bsis-$dst_amt             TO gt_calc-dstamt,
           'USD'                       TO gt_calc-waers,
           i_bsis-menge TO gt_calc-rmenge.

    gt_calc-amenge = gt_calc-bmenge + gt_calc-rmenge.
    gt_calc-accup = gt_calc-dtyup.

    APPEND gt_calc.

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

  DATA : $times TYPE i,
         $ix LIKE sy-tabix,
         $line LIKE sy-tabix.

  __process 'Read PO...' '30'.

  __cls : it_row_tab, it_row_sum.

  IF NOT i_bsis[] IS INITIAL.

    SELECT  a~bukrs b~kschl a~matnr a~ebeln a~ebelp
            a~menge AS pmenge

            b~menge AS bmenge
            b~dmbtr AS dmbtr

            b~shkzg b~bewtp a~meins b~waers b~lifnr a~werks
    INTO CORRESPONDING FIELDS OF TABLE it_row_tab
    FROM    ekpo AS  a  INNER JOIN ekbz AS b
    ON      a~ebeln      EQ    b~ebeln
    AND     a~ebelp      EQ    b~ebelp

    FOR ALL ENTRIES      IN    i_bsis

    WHERE   a~ebeln      EQ    i_bsis-ebeln
    AND     a~werks      IN    gr_bwkey
    AND     a~bukrs      EQ    p_bukrs
    AND     ( b~bewtp    EQ    'M' OR b~bewtp  EQ 'F' )
    AND     a~matnr      IN    s_matnr
    AND     a~ebeln      IN    s_ebeln.

    LOOP AT it_row_tab.
      MOVE-CORRESPONDING it_row_tab TO it_row_sum.
      COLLECT it_row_sum.
    ENDLOOP.

    SORT it_row_tab BY matnr ebeln ebelp  shkzg kschl.
    DELETE ADJACENT DUPLICATES FROM it_row_tab COMPARING matnr ebeln ebelp shkzg kschl.
    SORT it_row_sum BY matnr ebeln ebelp  shkzg kschl.

    LOOP AT it_row_tab.
      $ix = sy-tabix.
      READ TABLE it_row_sum WITH KEY matnr = it_row_tab-matnr
                                     ebeln = it_row_tab-ebeln
                                     ebelp = it_row_tab-ebelp
                                     shkzg = it_row_tab-shkzg
                                     kschl = it_row_tab-kschl
            BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-bmenge = it_row_sum-bmenge.
        it_row_tab-dmbtr = it_row_sum-dmbtr.
      ENDIF.
    ENDLOOP.

  ENDIF.


  IF p_expo EQ true.
    DELETE it_row_tab WHERE ebeln CP '43*'.
    DELETE it_row_tab WHERE ebeln CP '46*'.
  ENDIF.

  SORT it_row_tab BY bukrs kschl matnr ebeln ebelp.
  it_row_tab_bal[] = it_row_tab[].

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

  DATA lt_itab LIKE it_row_tab OCCURS 0 WITH HEADER LINE.

  PERFORM collect_itab TABLES lt_itab.

  SORT : lt_itab BY matnr ebeln ebelp,
         i_bsis BY entno matnr ebeln.

  LOOP AT i_bsis.
    $ix = sy-tabix.
    READ TABLE lt_itab WITH KEY matnr = i_bsis-matnr
                                ebeln = i_bsis-ebeln
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      i_bsis-ebelp = lt_itab-ebelp.
** On 08/27/13
    else.
      if i_bsis-duty_amt = 0.
         delete i_bsis INDEX $ix.
         continue.
       endif.
** End on 08/27/13
    ENDIF.
    MODIFY i_bsis INDEX $ix.
  ENDLOOP.

  __cls it_row_tab.
  it_row_tab[] = lt_itab[] .

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

  DATA : $zuonr(18),$dmbtr LIKE bsis-dmbtr,$p_open_q LIKE bsis-hkont.
  DESCRIBE TABLE gt_calc LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.
  CLEAR current_doc_cnt.

  __cls gt_out .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_open_q
    IMPORTING
      output = $p_open_q.

  LOOP AT gt_calc.
    $ix = sy-tabix.
    ADD 1 TO current_doc_cnt.

    IF p_load <> true.

      PERFORM end_of_compute.

      CONCATENATE gt_calc-ebeln gt_calc-ebelp INTO $zuonr.
      SELECT SINGLE dmbtr INTO $dmbtr FROM bsis
                            WHERE bukrs = 'H201'
                              AND hkont = $p_open_q
                              AND zuonr = $zuonr.
      IF sy-subrc NE 0.
        gt_calc-hkont = p_w_off.
      ELSE.
        gt_calc-hkont = p_hkont.
      ENDIF.
    ENDIF.

    gt_calc-accamt = gt_calc-accup * gt_calc-rmenge.
    gt_calc-varamt = gt_calc-accamt - gt_calc-dstamt.

    PERFORM write_num_to_string TABLES gt_calc.
    MOVE-CORRESPONDING gt_calc TO gt_out.

    IF gt_out-entamount <> 0.
      gt_out-percent = gt_out-duty_amt / gt_out-entamount * 100.
      WRITE gt_out-percent TO gt_out-percents
            RIGHT-JUSTIFIED NO-ZERO.
    ENDIF.

    IF gt_out-matnr IN s_matnr.
      APPEND gt_out.
    ENDIF.

    $mod = current_doc_cnt MOD 10.
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

  SORT gt_out BY entno matnr xmenges xmenge2s
                 dtamts ivamts ebeln dtyups.

  LOOP AT gt_out.

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
    DELETE FROM ztcou124
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

  Data: l_ifdmbtr type c,
        l_lines type i.

  max_line = p_max .
  group_key = 0.

** On 08/27/13
  Describe table p_it_inv lines l_lines.
  While l_lines >= 0.
   Read table p_it_inv index l_lines.
   If p_it_inv-dstamt ne 0.
      Exit.
   Endif.
   L_lines = l_lines - 1.
   Endwhile.
** end on 08/27/13

  LOOP AT p_it_inv.
    AT NEW entno.
      line_cnt = 0 .
      ADD 1 TO : group_key, total_doc_cnt.
    ENDAT.
** On 08/27/13
* if p_it_inv-dmbtr ne 0.
    if p_it_inv-dstamt ne 0.
       L_ifdmbtr = 'X'.
    endif.
** End
    IF  line_cnt GE  max_line
** On 08/27/13
         and L_ifdmbtr = 'X'
         and sy-tabix <= l_lines.
** End
      line_cnt = 0 .
      ADD 1 TO : group_key, total_doc_cnt.
      clear: l_ifdmbtr.
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


***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************

*break-point.

*      IF P_IT_INV-HKONT = P_HKONT.
*        PERFORM WRITE_OFF_ITEM TABLES   P_IT_INV
*                               USING    GROSS_AMOUNT
*                                        GRS_DIST_AMT
*                               CHANGING $GL_DOC_CNT .
*      ENDIF.


***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************


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
      PERFORM collect_gl_data.


***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
      PERFORM post_bapi_invoice   USING prog_text ind.
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************
***************************************************************************

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

    IF p_it_inv-hkont EQ p_w_off.
    ELSE.
      APPEND itemdata.
    ENDIF.
  ENDIF.

  CLEAR  itemdata.

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

* Get data from table : ztcou124
  SELECT  *
    INTO CORRESPONDING FIELDS OF TABLE gt_calc
    FROM    ztcou124
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
     SET cl_doc_no = *ztcou124-belum
   WHERE entno = *ztcou124-entno
     AND matnr = *ztcou124-matnr.

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
   WHERE entno = *ztcou124-entno.
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
  __cls : sum_i_bsis, i_bsis, it_row_tab_bal.

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
  __cls : sum_i_bsis, i_bsis, it_row_tab_bal.

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

* This line has been disabled during the bug tracking on April / 2013.
*  LOOP AT i_bsis.
*    $ix = sy-tabix.
** Vendor Account Number
*    CLEAR: ekko.
*    SELECT SINGLE lifnr INTO ekko-lifnr FROM ekko WHERE ebeln = i_bsis-ebeln.
** Country Key
*    CLEAR: lfa1.
*    SELECT SINGLE land1 INTO lfa1-land1 FROM lfa1 WHERE lifnr = ekko-lifnr.
*    IF lfa1-land1 = 'KR'.
**      DELETE i_bsis INDEX $ix.
*    ENDIF.
*  ENDLOOP.

  PERFORM get_dst_amt.

  __process 'Wait...' '40'.

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
      CLEAR *ztcou124.
      SELECT belum
             SUM( rmenge )  " Partial Posting Qty
             SUM( dmbtr )   " Partial Posting Amt
             SUM( dstamt )
            INTO (*ztcou124-belum,*ztcou124-rmenge,
                  *ztcou124-dmbtr,*ztcou124-dstamt)
            FROM ztcou124
            WHERE entno EQ i_bsis-entno
                  AND  matnr EQ i_bsis-matnr
                  AND  belum NE space
                  GROUP BY entno matnr belum.
        IF *ztcou124-rmenge <> 0.
          SELECT SINGLE belnr FROM rbkp
                          INTO *rbkp-belnr
                         WHERE  belnr EQ *ztcou124-belum
                           AND  gjahr EQ gjahr
                           AND  stjah EQ space
                           AND  stblg EQ space.
          IF sy-subrc EQ 0.
            i_bsis-menge = i_bsis-menge + *ztcou124-rmenge.
            i_bsis-duty_amt = i_bsis-duty_amt + *ztcou124-dmbtr.
            i_bsis-$dst_amt = i_bsis-$dst_amt + *ztcou124-dstamt.

            MODIFY i_bsis INDEX $ix TRANSPORTING menge duty_amt $dst_amt.

            CLEAR *ztcou124.
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

  ENDLOOP.

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

**   Collect EKBZ : Debit('S') --> * -1

  LOOP AT it_row_tab.
    MOVE it_row_tab TO p_lt_itab .
    IF p_lt_itab-shkzg EQ 'S'. " is Debit ?
      p_lt_itab-bmenge = -1 * p_lt_itab-bmenge.
      p_lt_itab-dmbtr  = -1 * p_lt_itab-dmbtr.
    ENDIF.
    CLEAR : p_lt_itab-shkzg,p_lt_itab-bewtp,p_lt_itab-accup.
    COLLECT p_lt_itab.
  ENDLOOP.

  DELETE p_lt_itab WHERE kschl NE p_kschl.

ENDFORM.                    " COLLECT_ITAB
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
            FROM  ztcou124
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
    $i_bsis-zuonr = $i_bsis-matnr.
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
    $i_bsis-matnr = $i_bsis-zuonr.
    $i_bsis-zuonr = space.
    MODIFY $i_bsis INDEX $ix TRANSPORTING matnr zuonr.
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

  __cls i_ztcou124.

  LOOP AT p_it_index.
    CLEAR gt_out.
    CLEAR *ztcou124.
    READ TABLE gt_out INDEX p_it_index-index.
    MOVE-CORRESPONDING gt_out TO *ztcou124.

     *ztcou124-fentp = p_entry.
     *ztcou124-kschl = p_kschl.
     *ztcou124-pdate = p_date.
     *ztcou124-erdat = sy-datum.
     *ztcou124-ernam = sy-uname.

    APPEND *ztcou124 TO i_ztcou124.

*    SELECT SINGLE * FROM ztcou124
*     WHERE entno EQ *ztcou124-entno
*    	AND matnr EQ *ztcou124-matnr
*    	AND bukrs EQ *ztcou124-bukrs
*    	AND kschl EQ *ztcou124-kschl
*    	AND fentp EQ *ztcou124-fentp
*    	AND ebeln EQ *ztcou124-ebeln
*    	AND ebelp EQ *ztcou124-ebelp
*    	AND belum EQ *ztcou124-belum .
*
*    IF sy-subrc EQ 0.
*      ztcou124 = *ztcou124.
*      UPDATE ztcou124.
*    ELSE.
*      ztcou124 = *ztcou124.
*      INSERT ztcou124.
*    ENDIF.
*    IF sy-subrc NE 0.
*      ROLLBACK WORK.
*    ELSE.
*      COMMIT WORK.
*    ENDIF.

  ENDLOOP.

  MODIFY ztcou124 FROM TABLE i_ztcou124.
  COMMIT WORK.

ENDFORM.                    " save_log_table
*&---------------------------------------------------------------------*
*&      Form  END_OF_COMPUTE
*&---------------------------------------------------------------------*
FORM end_of_compute .
  DATA $ix TYPE i.

* 7. P/O qty.
  SORT it_row_tab_bal BY ebeln matnr." kschl.
  READ TABLE  it_row_tab_bal WITH KEY ebeln = gt_calc-ebeln
                                      matnr = gt_calc-matnr
                                      BINARY SEARCH.
  IF sy-subrc EQ 0.
    gt_calc-ebelp  = it_row_tab_bal-ebelp.
    gt_calc-pmenge = it_row_tab_bal-pmenge.
  ENDIF.

* 8. GR/ Qty.
  CLEAR: it_row_tab_bal, gt_calc-gmenge.
  READ TABLE  it_row_tab_bal WITH KEY ebeln = gt_calc-ebeln
                                      matnr = gt_calc-matnr
*                                      kschl = p_kschl
                                      BINARY SEARCH.
  IF sy-subrc EQ 0.
    LOOP AT it_row_tab_bal FROM sy-tabix.
      IF it_row_tab_bal-ebeln = gt_calc-ebeln AND
         it_row_tab_bal-matnr = gt_calc-matnr." AND
*         it_row_tab_bal-kschl = p_kschl.
        gt_calc-gmenge = gt_calc-gmenge + it_row_tab_bal-bmenge.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

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
