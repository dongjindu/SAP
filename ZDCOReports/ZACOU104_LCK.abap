*----------------------------------------------------------------------
* Program ID        : ZACOU104_LCK
* Title             : [CO] Lock management for Register Analysis Codes
* Created on        : 05/20/2008
* Created by        : ig.moon
* Specifications By : Andy Choi
* Description       : [CO] Lock management for Register Analysis Codes
*----------------------------------------------------------------------
REPORT zacou104 MESSAGE-ID zmco.

INCLUDE zacoui00.
INCLUDE zacou104_top_lck.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
PARAMETERS: p_kokrs LIKE ztcou104-kokrs OBLIGATORY MEMORY ID cac
                                        MATCHCODE OBJECT fc_kokrs,
            p_year  LIKE ztcou104-bdatj OBLIGATORY MEMORY ID bdtj,
            p_kalka LIKE ztcou104-kalka OBLIGATORY MEMORY ID kka
                                        DEFAULT 'U1'.

" In case,be called from zacou104
* {
PARAMETERS: p_poper LIKE keko-poper NO-DISPLAY.
* }

SELECT-OPTIONS s_id FOR ztcou104-id MATCHCODE OBJECT zid.
SELECTION-SCREEN END OF BLOCK b0.

PARAMETERS p_lock NO-DISPLAY.

INCLUDE zacou104_f01_lck.
*INCLUDE zacou104_f01_old.
*include zacou104_f01.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kalka.
  PERFORM popup_kalka USING p_kalka 'P_KALKA'.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_data.
  PERFORM get_lock_infor.
  CALL SCREEN 300.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

  PERFORM create_alv_control.

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

    WHEN 'LKMAT'.
      PERFORM get_selected_rows TABLES $gt_out.
      PERFORM chk_all_row TABLES $gt_out
                          USING true.

      PERFORM refresh_field.

    WHEN 'LKMON'.
      PERFORM chk_selected_cells USING true.
      PERFORM refresh_field.

    WHEN 'ULKM'.
      PERFORM get_selected_rows TABLES $gt_out.
      PERFORM chk_all_row TABLES $gt_out
                          USING false.

      PERFORM refresh_field.

    WHEN 'ULKMO'.
      PERFORM chk_selected_cells USING false.
      PERFORM refresh_field.

    WHEN 'SAVE'.
      PERFORM save_data.
      PERFORM refresh_field.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.

  DATA $text(10).
  DATA $ix(2) TYPE n.

  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
        'ID'         '1' 0,
        'IDTEXT'     '1' 0,
        'LOCK00'     '3' 0.

  DO 12 TIMES.
    $ix = sy-index.
    CONCATENATE 'LOCK' $ix INTO $text.
    __color :
      $text     '3' 0.
  ENDDO.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.


ENDFORM.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

  PERFORM clear_chk.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  __cls $gt_out.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'Please select at least 1 row.'.
    EXIT.
*    $gt_out[] = gt_out[].
*    gt_out-chk = true .
*    MODIFY gt_out TRANSPORTING chk WHERE chk EQ false.
*    LOOP AT $gt_out.
*      $gt_out-indx = sy-tabix.
*      MODIFY $gt_out INDEX sy-tabix TRANSPORTING indx.
*    ENDLOOP.

  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-chk = true .
      MODIFY gt_out INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT gt_out.
      CHECK gt_out-chk EQ true.
      $gt_out = gt_out.
      $gt_out-indx = sy-tabix.
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  clear_chk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_chk.
  CLEAR gt_out-chk.
  MODIFY gt_out TRANSPORTING chk WHERE chk EQ true.

ENDFORM.                    " clear_chk
*&---------------------------------------------------------------------*
*&      Form  get_most_prd_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_most_prd_fsc USING p_mi
                            p_dealer
                            p_gr5
                           $p_year
                            p_ix
                            p_tp
                            CHANGING p_mat_pr.

  IF p_tp EQ '1'.
    CLEAR p_gr5.
  ENDIF.

  __cls it_ckmlmv003 .
  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

** read GR data
  SELECT  a~bwkey a~matnr a~verid_nd
          c~aufnr
          b~out_menge
          b~meinh
          d~mvgr5
          e~fevor
          e~werks
          f~mtart
          f~matkl
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003_temp
    FROM ckmlmv001 AS a
    INNER JOIN ckmlmv003 AS b
       ON a~kalnr    =  b~kalnr_bal
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = b~kalnr_in
    INNER JOIN mvke AS d
       ON d~matnr = a~matnr
    INNER JOIN marc AS e
   ON e~matnr = d~matnr
    INNER JOIN mara AS f
   ON f~matnr = d~matnr

   WHERE a~werks    IN gr_bwkey
     AND a~matnr    LIKE p_mi
     AND a~btyp     =  'BF'
     AND a~bwkey    IN  gr_bwkey
     AND b~gjahr    =  $p_year
     AND b~perio    =  p_ix
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05'.

  LOOP AT it_ckmlmv003_temp.
    MOVE-CORRESPONDING it_ckmlmv003_temp TO it_ckmlmv003.
    CLEAR: it_ckmlmv003-verid_nd,
           it_ckmlmv003-aufnr.

    IF p_tp EQ '1'.
      CLEAR it_ckmlmv003-mvgr5.
    ENDIF.

    IF it_ckmlmv003-fevor NE 'SEA'.
      PERFORM get_code USING it_ckmlmv003-matnr
                    CHANGING it_ckmlmv003-model
                             it_ckmlmv003-kunnr .
    ENDIF.


    IF it_ckmlmv003-mtart EQ 'HALB'.
      it_ckmlmv003-model = it_ckmlmv003-werks.
    ENDIF.
    IF it_ckmlmv003-matkl EQ 'A/S'.
      it_ckmlmv003-kunnr = 'MOBIS'.
    ELSE.

      IF it_ckmlmv003-kunnr IS INITIAL.
        SELECT SINGLE kunnr INTO it_ckmlmv003-kunnr
        FROM a005
        WHERE matnr EQ it_ckmlmv003-matnr.
      ENDIF.
    ENDIF.

    IF it_ckmlmv003-kunnr NE p_dealer OR p_gr5 NE it_ckmlmv003-mvgr5 .
      CONTINUE.
    ENDIF.

    COLLECT it_ckmlmv003. CLEAR it_ckmlmv003.
  ENDLOOP.

  SORT it_ckmlmv003 BY out_menge DESCENDING.

  READ TABLE it_ckmlmv003 INDEX 1.
  IF sy-subrc EQ 0.
    p_mat_pr = it_ckmlmv003-matnr.
  ENDIF.

ENDFORM.                    " get_most_prd_fsc
*&---------------------------------------------------------------------*
*&      Form  get_mi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_mi USING    p_$gt_out STRUCTURE gt_out
            CHANGING p_mi.

  DATA $strlen TYPE i.

  $strlen = strlen( p_$gt_out-id ).

  IF $strlen NE 18.
    p_mi = p_$gt_out-id.
    EXIT.
  ENDIF.

  IF p_$gt_out-id+13(1) EQ space.
    p_mi = p_$gt_out-id+6(7).
  ELSE.
    p_mi = p_$gt_out-id+5(9).
  ENDIF.



ENDFORM.                    " get_mi
*&---------------------------------------------------------------------*
*&      Form  get_plant
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
   WHERE bukrs = g_kokrs.

  LOOP AT gt_plant.
    gr_bwkey-sign = 'I'.
    gr_bwkey-option = 'EQ'.
    gr_bwkey-low = gt_plant-bwkey.

    APPEND gr_bwkey.
    CLEAR gr_bwkey.
  ENDLOOP.


ENDFORM.                    " get_plant
*&---------------------------------------------------------------------*
*&      Form  get_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ROW_TAB_FSC  text
*      <--P_IT_ROW_TAB_MODEL  text
*      <--P_IT_ROW_TAB_KUNNR  text
*----------------------------------------------------------------------*
FORM get_code USING    p_matnr
              CHANGING p_model
                       p_kunnr.

  IF p_matnr+13(1) EQ space.    " old
    p_model = p_matnr+6(2).
    p_kunnr = p_matnr+1(3).
  ELSE.
    p_model = p_matnr+5(2).     " eBOM
    p_kunnr = p_matnr+1(3).
  ENDIF.

ENDFORM.                    " get_code
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$TEXT  text
*      -->P_PERCENTAGE  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  get_most_pln_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_WORK_MI  text
*      -->P_$GT_WORK_DEALER  text
*      -->P_$GT_WORK_GR5  text
*      -->P_P_YEAR  text
*      -->P_$IX  text
*      -->P_P_TP  text
*      <--P_MI_PLN_TAB_FSC  text
*----------------------------------------------------------------------*
FORM get_most_pln_fsc USING p_mi
                            p_dealer
                            p_gr5
                           $p_year
                            p_ix
                            p_tp
                            CHANGING p_mat_pl.

  DATA BEGIN OF copa_field OCCURS 0.
          INCLUDE STRUCTURE bapi_copa_field.
  DATA END   OF copa_field.

  DATA BEGIN OF copa_selection OCCURS 0.
          INCLUDE STRUCTURE bapi_copa_selection.
  DATA END   OF copa_selection.

  DATA BEGIN OF copa_data OCCURS 0.
          INCLUDE STRUCTURE bapi_copa_data.
  DATA END   OF copa_data.

  DATA BEGIN OF  ireturn OCCURS 0.
          INCLUDE STRUCTURE bapiret2.
  DATA END   OF ireturn.

  DATA : BEGIN OF  plan_data OCCURS 0,
            paledger TYPE ledbo,
            vrgar TYPE rke_vrgar,
            versi TYPE coversi,
            perio(7),
            spart TYPE spart,
            artnr TYPE artnr,
            kndnr TYPE kunde_pa,
            absmg_me TYPE meins,
            erlos TYPE rke2_erlos,
            absmg TYPE rke2_absmg,
         END   OF plan_data.

  DATA : v_index TYPE i.

  copa_field = 'PALEDGER'.  APPEND copa_field.
  copa_field = 'VRGAR'.     APPEND copa_field.
  copa_field = 'VERSI'.     APPEND copa_field.
  copa_field = 'PERIO'.     APPEND copa_field.
  copa_field = 'SPART'.     APPEND copa_field.
  copa_field = 'ARTNR'.     APPEND copa_field.
  copa_field = 'KNDNR'.     APPEND copa_field.
  copa_field = 'ABSMG_ME'.  APPEND copa_field.
  copa_field = 'ERLOS'.     APPEND copa_field.
  copa_field = 'ABSMG'.     APPEND copa_field.

  copa_selection-fieldname = 'PALEDGER'.
  copa_selection-sign = 'I'.
  copa_selection-option = 'EQ'.
  copa_selection-low = '01'.
  APPEND copa_selection.

  copa_selection-fieldname = 'VRGAR'.
  copa_selection-sign = 'I'.
  copa_selection-option = 'EQ'.
  copa_selection-low = 'F'.
  APPEND copa_selection.

  copa_selection-fieldname = 'VERSI'.
  copa_selection-sign = 'I'.
  copa_selection-option = 'EQ'.
  copa_selection-low = '311'.
  APPEND copa_selection.

  copa_selection-fieldname = 'PERIO'.
  copa_selection-sign = 'I'.
  copa_selection-option = 'EQ'.
  CONCATENATE $p_year '0' p_ix INTO copa_selection-low.
  APPEND copa_selection.

  FIELD-SYMBOLS <fs>.

  CALL FUNCTION 'BAPI_COPAPLANNING_GETDATA'
       EXPORTING
            operatingconcern     = p_kokrs
            typeofprofitanalysis = '1'
       TABLES
            selectedfields       = copa_field
            selection            = copa_selection
            data                 = copa_data
            return               = ireturn.

  IF sy-subrc EQ 0.

    READ TABLE ireturn WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
    ELSE.

      LOOP AT copa_data.
        ADD 1 TO v_index.
        ASSIGN COMPONENT v_index OF STRUCTURE plan_data TO <fs>.
        MOVE copa_data-value TO <fs>.
        AT END OF record_id.
          IF plan_data-kndnr EQ p_dealer.
            APPEND plan_data.
          ENDIF.
          v_index = 0.
        ENDAT.
      ENDLOOP.

      SORT plan_data BY absmg DESCENDING.

      IF p_tp EQ '1'.
        READ TABLE plan_data INDEX 1.
        p_mat_pl = plan_data-artnr.
      ELSE.
        LOOP AT plan_data.
          SELECT SINGLE mvgr5 INTO mvke-mvgr5
              FROM mvke WHERE matnr EQ plan_data-artnr.
          IF sy-subrc EQ 0 AND p_gr5 EQ mvke-mvgr5.
            p_mat_pl = plan_data-artnr.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_most_prd_fsc
*&---------------------------------------------------------------------*
*&      Form  get_lock_infor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_lock_infor.

  __cls gt_out.

  LOOP AT gt_ztcou104.

    MOVE-CORRESPONDING gt_ztcou104 TO gt_out.

    SELECT SINGLE lock00
              lock01 lock02 lock03 lock04 lock05
              lock06 lock07 lock08 lock09 lock10
              lock11 lock12

    INTO (gt_out-lock00,gt_out-lock01,gt_out-lock02,
          gt_out-lock03,gt_out-lock04,gt_out-lock05,gt_out-lock06,
           gt_out-lock07,gt_out-lock08,gt_out-lock09,
           gt_out-lock10,gt_out-lock11,gt_out-lock12)

    FROM ztcou104lock
    WHERE  kokrs EQ gt_ztcou104-kokrs
       AND bdatj EQ gt_ztcou104-bdatj
       AND kalka EQ gt_ztcou104-kalka
       AND id    EQ gt_ztcou104-id.

    APPEND gt_out.
    CLEAR GT_OUT.
  ENDLOOP.

ENDFORM.                    " get_lock_infor
*&---------------------------------------------------------------------*
*&      Form  chk_all_row
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM chk_all_row TABLES   p_gt_out STRUCTURE gt_out
                 USING    p_true.

  DATA $ix(2) TYPE n.
  FIELD-SYMBOLS : <to> .
  DATA fname(30).

  LOOP AT p_gt_out.

    p_gt_out-lock00 = p_true.

    DO 12 TIMES.
      $ix = sy-index.
      CONCATENATE 'P_GT_OUT-LOCK' $ix INTO fname.
      ASSIGN (fname) TO <to>.
      <to> = p_true.
    ENDDO.
    MODIFY p_gt_out INDEX sy-tabix.
  ENDLOOP.

  LOOP AT $gt_out.
    READ TABLE gt_out INDEX $gt_out-indx.
    IF sy-subrc EQ 0.
      gt_out = $gt_out.
      MODIFY gt_out INDEX $gt_out-indx.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " chk_all_row
*&---------------------------------------------------------------------*
*&      Form  chk_selected_cells
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM chk_selected_cells USING    p_true.

  DATA : lt_cols       TYPE lvc_t_col,
         ls_col        TYPE lvc_s_col.

  FIELD-SYMBOLS : <to> .
  DATA fname(30).

  CALL METHOD g_grid->get_selected_columns
    IMPORTING
      et_index_columns = lt_cols.

  LOOP AT lt_cols INTO ls_col.
    IF NOT ls_col-fieldname IS INITIAL.
      IF ls_col-fieldname CP 'LOCK*'.
        CONCATENATE 'GT_OUT-' ls_col-fieldname INTO fname.
        ASSIGN (fname) TO <to>.
        LOOP AT gt_out.
          <to> = p_true.
          MODIFY gt_out INDEX sy-tabix.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " chk_selected_cells
