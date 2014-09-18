*----------------------------------------------------------------------
* Program ID        : ZACOU104_FSC
* Title             : [CO] Register Analysis Codes
* Created on        : 08/18/2006, and 5/20/2008 recreated by ig.moon
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Register analysis codes for variance analysis.
*----------------------------------------------------------------------
REPORT zacou104 MESSAGE-ID zmco.

INCLUDE zacoui00.
INCLUDE zacou104_top_fsc.

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

INCLUDE zacou104_f01_fsc.

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
  CALL SCREEN 100.

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

    WHEN 'FILL'.
      PERFORM fill_fsc USING '1'.
      PERFORM refresh_field.

    WHEN 'FILM'.
      PERFORM fill_fsc USING '2'.
      PERFORM refresh_field.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      PERFORM save_data.
      PERFORM refresh_field.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '200'.
  SET TITLEBAR '100'.
  PERFORM modi_screen.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN 'ENTR'.
      MOVE-CORRESPONDING *ztcou104 TO gt_out.
      IF gt_out-zbase_fsc IS INITIAL.
        MESSAGE s000 WITH 'Input Base FSC!'.
        EXIT.
      ENDIF.

      IF gt_out-base_year IS INITIAL.
        MESSAGE s000 WITH 'Input Base year!'.
        EXIT.
      ENDIF.

      IF gt_out-base_poper IS INITIAL.
        MESSAGE s000 WITH 'Input Base period!'.
        EXIT.
      ENDIF.

      MODIFY gt_out INDEX gv_index.
      PERFORM refresh_field.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_data INPUT.
  MOVE-CORRESPONDING *ztcou104 TO gt_out.
ENDMODULE.                 " GET_DATA  INPUT
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
        'ZABP_FSC'   '1' 0,
        'ZBASE_FSC'  '1' 0,
        'BASE_YEAR'  '1' 0,
        'BASE_POPER' '1' 0,
*        'FSC01'      '4' 0,
*        'FSC02'      '4' 0,
*        'FSC03'      '4' 0,
*        'FSC04'      '4' 0,
*        'FSC05'      '4' 0,
*        'FSC06'      '4' 0,
*        'FSC07'      '4' 0,
*        'FSC08'      '4' 0,
*        'FSC09'      '4' 0,
*        'FSC10'      '4' 0,
*        'FSC11'      '4' 0,
*        'FSC12'      '4' 0,
        'BDATJ'      '5' 0,
        'KALKA'      '6' 0.

  IF p_poper NE space.
    CONCATENATE 'FSC' p_poper+1(2) INTO $text.
    __color :
      $text     '3' 1.

  ENDIF.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.


ENDFORM.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  fill_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_fsc USING value(p_tp).

  DATA $mi(20).
  DATA $flag.
  DATA ls_celltab TYPE lvc_s_styl.

  PERFORM get_selected_rows TABLES $gt_out.

  READ TABLE $gt_out INDEX 1.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  __cls $gt_work.

  LOOP AT $gt_out.
    MOVE-CORRESPONDING $gt_out TO $gt_work.
    PERFORM get_mi USING $gt_out-id CHANGING $mi.
    CONCATENATE '%' $mi '%' INTO $mi.
    CONDENSE $mi.
    $gt_work-mi = $mi.
    $gt_work-idx = sy-tabix.
    MOVE $gt_out-kunnr TO $gt_work-dealer.
    MOVE $gt_out-mvgr5 TO $gt_work-gr5.
    IF p_tp EQ '1'.
      CLEAR $gt_work-gr5.
    ENDIF.
    APPEND $gt_work.
  ENDLOOP.

  SORT $gt_work BY dealer gr5 mi.

  DATA f_name(20).
  DATA $idx(2) TYPE n.
  DATA $idx2(2) TYPE n.
  DATA $year(4) TYPE n.

  DATA wfscxx TYPE zjan_fsc.
  DATA wfscx2 TYPE zjan_fsc.
  DATA gfscxx TYPE zjan_fsc.
  DATA : idx TYPE i,
         $matnr TYPE matnr,
         $perio TYPE poper.

  __cls : mi_prd_tab, mi_pln_tab.

* {
  __define_not_important.

  LOOP AT $gt_work.
    AT NEW mi.
      $flag = true.
    ENDAT.
    CHECK $flag = true.
    CLEAR $flag.
    ADD 1 TO total_doc_cnt.
  ENDLOOP.

  $total_cnt = total_doc_cnt.
* }

  LOOP AT $gt_work.
    idx = sy-tabix.

    DO 12 TIMES VARYING wfscxx FROM $gt_work-fsc01 NEXT $gt_work-fsc02.
      $idx = sy-index.
      CHECK $idx < p_poper.
      CONCATENATE '$GT_WORK-FSC' $idx INTO fname.
      CONCATENATE 'FSC' $idx INTO fname2.
      IF wfscxx IS INITIAL.

        SELECT SINGLE a~matnr b~perio INTO ($matnr,$perio)
          FROM ckmlmv001 AS a
          INNER JOIN ckmlmv003 AS b
             ON a~kalnr    =  b~kalnr_bal
          INNER JOIN ckmlmv013 AS c
             ON c~kalnr_proc = b~kalnr_in
         WHERE a~werks    IN gr_bwkey
           AND a~matnr    EQ $gt_work-id
           AND a~btyp     =  'BF'
           AND a~bwkey    IN  gr_bwkey
*           AND b~gjahr    <=  p_year
           AND b~gjahr    =  p_year      "within year
           AND b~perio    <=  $idx
           AND c~flg_wbwg = 'X'
           AND c~autyp = '05'
           AND b~out_menge <> 0.
        IF sy-subrc EQ 0.         "Previous Production exist
*--andy
          SELECT SINGLE artnr INTO ztcou103-artnr
                          FROM ztcou103
                          WHERE kokrs EQ p_kokrs
                            AND bdatj EQ p_year
                            AND kalka EQ p_kalka
                            AND poper EQ $idx
                            AND artnr EQ $gt_work-id.
          IF sy-subrc EQ 0.
            ASSIGN (fname) TO <to>.
            <to> = $gt_work-id.
            MODIFY $gt_work INDEX idx TRANSPORTING (fname2).
          ENDIF.

        ELSE.                   "No production

*andy---start
*          DO 12 TIMES VARYING wfscx2 FROM
*                $gt_work-fsc01 NEXT $gt_work-fsc02.
*            $idx2 = sy-index.
*            IF $idx2 > $idx. " AND $idx2 <= p_poper .
*              IF wfscx2 IS INITIAL.
*                CONCATENATE '$GT_WORK-FSC' $idx2 INTO fname.
*                CONCATENATE 'FSC' $idx2 INTO fname2.
*                ASSIGN (fname) TO <to>.
*                <to> = $gt_work-id.
*                SELECT SINGLE artnr INTO ztcou103-artnr
*                                FROM ztcou103
*                                WHERE kokrs EQ p_kokrs
*                                  AND bdatj EQ p_year
*                                  AND kalka EQ p_kalka
*                                  AND poper EQ $idx2
*                                  AND artnr EQ $gt_work-id.
*                IF sy-subrc EQ 0.
*                  MODIFY $gt_work INDEX idx TRANSPORTING (fname2).
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDDO.
*          EXIT.

        ENDIF.
      ENDIF.
    ENDDO.

    AT NEW mi.
      $flag = true.
    ENDAT.

    CHECK $flag = true.
    CLEAR $flag.

* {
    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
    CONCATENATE $gt_work-mi ':' $current_cnt '/' $total_cnt
    INTO $text.
    CONDENSE $text.
    percentage = current_doc_cnt / total_doc_cnt * 100.
    PERFORM show_progress USING $text percentage.
* }

    DO 12 TIMES.
      $idx = sy-index.
      CHECK $idx < p_poper.
      PERFORM get_most_prd_fsc USING $gt_work-mi
                                     $gt_work-dealer
                                     $gt_work-gr5
                                     p_year
                                     $idx
                                     p_tp
                            CHANGING mi_prd_tab-fsc.
      IF mi_prd_tab-fsc NE space.
*check 103 unit cost again.
        SELECT SINGLE artnr INTO ztcou103-artnr
                        FROM ztcou103
                        WHERE kokrs EQ p_kokrs
                          AND bdatj EQ p_year
                          AND kalka EQ p_kalka
                          AND poper EQ $idx
                          AND artnr EQ mi_prd_tab-fsc.
        IF sy-subrc EQ 0.
          mi_prd_tab-dealer = $gt_work-dealer.
          mi_prd_tab-mi     = $gt_work-mi.
          mi_prd_tab-year   = p_year.
          mi_prd_tab-poper  = $idx.
          APPEND mi_prd_tab.
        ENDIF.
      ENDIF.
    ENDDO.

* base
    $year = p_year - 1.
    PERFORM get_most_prd_fsc USING $gt_work-mi
                                   $gt_work-dealer
                                   $gt_work-gr5
                                   $year
                                   '12'
                                   p_tp
                          CHANGING mi_prd_tab-fsc.

    IF mi_prd_tab-fsc NE space.
      mi_prd_tab-dealer = $gt_work-dealer.
      mi_prd_tab-mi     = $gt_work-mi.
      mi_prd_tab-year   = $year.
      mi_prd_tab-poper  = $idx.
      APPEND mi_prd_tab.
    ENDIF.
* abp
    PERFORM get_most_pln_fsc USING $gt_work-model
                                   $gt_work-mi
                                   $gt_work-dealer
                                   $gt_work-gr5
                                   p_year
                                   $idx
                                   p_tp
                          CHANGING mi_pln_tab-fsc.
    IF mi_pln_tab-fsc NE space.
      mi_pln_tab-dealer = $gt_work-dealer.
      mi_pln_tab-mi     = $gt_work-mi.
      mi_pln_tab-year   = '9999'.
      mi_pln_tab-poper  = $idx.
      APPEND mi_pln_tab.
    ENDIF.
  ENDLOOP.

  SORT mi_prd_tab BY dealer mi year poper.

  LOOP AT $gt_work.

    idx = sy-tabix.

    DO 12 TIMES VARYING wfscxx FROM $gt_work-fsc01 NEXT $gt_work-fsc02.
      $idx = sy-index.
      CHECK $idx < p_poper.
      CONCATENATE '$GT_WORK-FSC' $idx INTO fname.
      CONCATENATE 'FSC' $idx INTO fname2.
      IF wfscxx IS INITIAL.
        ASSIGN (fname) TO <to>.
        READ TABLE mi_prd_tab WITH KEY dealer = $gt_work-dealer
                                       mi     = $gt_work-mi
                                       year  = p_year
                                       poper  = $idx
                                       BINARY SEARCH.
*FIXME
        IF sy-subrc EQ 0.
          <to> = mi_prd_tab-fsc.
        ENDIF.
      ENDIF.
      MODIFY $gt_work INDEX idx TRANSPORTING (fname2).
    ENDDO.

* base FSC
    IF $gt_work-zbase_fsc IS INITIAL.
      $year = p_year - 1.
      READ TABLE mi_prd_tab WITH KEY dealer = $gt_work-dealer
                                     mi     = $gt_work-mi
                                     year   = $year
                                     poper  = $idx
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        $gt_work-zbase_fsc = mi_prd_tab-fsc.
        $gt_work-base_year = p_year - 1.
        $gt_work-base_poper = '012'.
      ENDIF.
      MODIFY $gt_work INDEX idx TRANSPORTING zbase_fsc
                                             base_year
                                             base_poper.
    ENDIF.

* ABP FSC
    IF $gt_work-zabp_fsc IS INITIAL.
      READ TABLE mi_pln_tab WITH KEY dealer = $gt_work-dealer
                                     mi     = $gt_work-mi
                                     year   = '9999'
                                     poper  = $idx
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        $gt_work-zabp_fsc = mi_pln_tab-fsc.
      ENDIF.
      MODIFY $gt_work INDEX idx TRANSPORTING zabp_fsc .
    ENDIF.
  ENDLOOP.

  LOOP AT $gt_work.
    READ TABLE $gt_out INDEX $gt_work-idx.
    CHECK sy-subrc EQ 0.
    DO 12 TIMES VARYING wfscxx FROM $gt_work-fsc01 NEXT $gt_work-fsc02.
      $idx = sy-index.
      CHECK $idx < p_poper.
      CONCATENATE '$GT_OUT-FSC' $idx INTO fname.
      CONCATENATE 'FSC' $idx INTO fname2.
      READ TABLE $gt_work-celltab INTO ls_celltab
                  WITH KEY fieldname = fname2.
      IF sy-subrc EQ 0.
        MESSAGE s000 WITH 'Can not change the locked field!'.
      ELSE.
        ASSIGN (fname) TO <to>.
        IF <to> IS INITIAL.
          <to> = wfscxx.
        ENDIF.
      ENDIF.
    ENDDO.
    $gt_out-zbase_fsc = $gt_work-zbase_fsc.
    $gt_out-zabp_fsc = $gt_work-zabp_fsc.
    IF $gt_out-zbase_fsc NE space.
      $gt_out-base_year = p_year - 1.
      $gt_out-base_poper = '012'.
    ENDIF.
    MODIFY $gt_out INDEX $gt_work-idx.
  ENDLOOP.

  LOOP AT $gt_out.
    READ TABLE gt_out INDEX $gt_out-indx.
    IF sy-subrc EQ 0.
      gt_out = $gt_out.
      MODIFY gt_out INDEX $gt_out-indx.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_fsc
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  PERFORM clear_chk.

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
    $gt_out[] = gt_out[].
    gt_out-chk = true .
    MODIFY gt_out TRANSPORTING chk WHERE chk EQ false.
    LOOP AT $gt_out.
      $gt_out-indx = sy-tabix.
      MODIFY $gt_out INDEX sy-tabix TRANSPORTING indx.
    ENDLOOP.
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

  DELETE it_ckmlmv003 WHERE out_menge EQ 0.

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
FORM get_mi USING    p_id
            CHANGING p_mi.

  DATA $strlen TYPE i.

  $strlen = strlen( p_id ).

  IF $strlen NE 18.
    p_mi = p_id.
    EXIT.
  ENDIF.

  IF p_id+13(1) EQ space.
    p_mi = p_id+6(7).
  ELSE.
    p_mi = p_id+5(9).
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
FORM get_most_pln_fsc USING p_model
                            p_mi
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

  DATA v_index TYPE i.
  DATA $kunnr(3).
  DATA $model(2).
  DATA $mi(20).
  DATA $mi2(20).

  $mi2 = p_mi.

  REPLACE '%' WITH '' INTO : $mi2,$mi2.
  CONDENSE $mi2.

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

*  break-point.

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

        IF v_index EQ 10.

          IF plan_data-artnr+13(1) EQ space.    " old
            $model = plan_data-artnr+6(2).
            $kunnr = plan_data-artnr+1(3).
          ELSE.
            $model = plan_data-artnr+5(2).
            $kunnr = plan_data-artnr+1(3).
          ENDIF.

          PERFORM get_mi USING plan_data-artnr CHANGING $mi.

          IF $mi EQ $mi2.
            SELECT SINGLE matkl INTO mara-matkl FROM mara WHERE
                    matnr EQ plan_data-artnr.
            IF mara-matkl NE 'A/S'.
              APPEND plan_data.
            ENDIF.
          ENDIF.
          v_index = 0.
        ENDIF.
      ENDLOOP.

      CLEAR plan_data.
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
*&      Form  build_cell_attr1_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr1_lock.

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  __cls gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE lock EQ 'X'.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE lock EQ 'X'.

ENDFORM.                    " build_cell_attr1_lock
*&---------------------------------------------------------------------*
*&      Form  modi_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modi_screen.
  DATA ls_celltab TYPE lvc_s_styl.
  LOOP AT gt_out-celltab INTO ls_celltab.
    CHECK ls_celltab-style EQ cl_gui_alv_grid=>mc_style_disabled.
    LOOP AT SCREEN.
      IF screen-name+10 EQ ls_celltab-fieldname.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name+10 EQ 'BASE_YEAR' OR
                    screen-name+10 EQ 'BASE_POPER'.
        IF ls_celltab-fieldname EQ 'ZBASE_FSC'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " modi_screen
