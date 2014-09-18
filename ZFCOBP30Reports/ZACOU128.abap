*----------------------------------------------------------------------
* Program ID        : ZACOU128
* Title             : [CO] ABP Revision
* Created on        : 09/13/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : ABP Revision
*----------------------------------------------------------------------
REPORT zacou128 MESSAGE-ID zmco.

INCLUDE zacou128_top.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
* Planing Year
PARAMETERS : p_kokrs LIKE keko-kokrs MEMORY ID cac OBLIGATORY,
             p_bdatj LIKE keko-bdatj MEMORY ID zpyr OBLIGATORY,
             p_versn LIKE cosl-versn OBLIGATORY DEFAULT '311'.
SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME TITLE text-t03.
SELECT-OPTIONS :
             p_eff   FOR tc31a-zgkal NO INTERVALS OBLIGATORY .
*SELECTION-SCREEN SKIP.
*SELECTION-SCREEN PUSHBUTTON  1(30) CRAT USER-COMMAND CRAT.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON  1(30) cslt USER-COMMAND cslt.
SELECTION-SCREEN END OF BLOCK view-result.

SELECTION-SCREEN BEGIN OF BLOCK bdc-req WITH FRAME TITLE text-t04.
SELECT-OPTIONS s_matnr FOR mara-matnr .
PARAMETERS :
             p_plscn LIKE plaf-plscn DEFAULT '901' OBLIGATORY.
PARAMETERS : p_mode(1) DEFAULT 'E'.
*ABP routing usg
PARAMETERS: p_verwe TYPE pln_verwe DEFAULT '10',
            p_usg LIKE plkod-verwe DEFAULT '10' NO-DISPLAY.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON  1(30) rout USER-COMMAND rout.
SELECTION-SCREEN END OF BLOCK bdc-req.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.


*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
*  sy-title = '[CO] ABP Revision'.
  PERFORM default_.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    IF sy-tcode = 'ZCOA128'.
*      IF screen-name = 'bdc-req'.
*        screen-active = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*
*    ENDIF.
*  ENDLOOP.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
      CLEAR g_proc_type.
      PERFORM view_result.

END-OF-SELECTION.

*----------------------------------------------------------------------*

AT SELECTION-SCREEN.
  CLEAR g_error.

  CASE sscrfields-ucomm.
    WHEN 'CRAT'.
      CLEAR g_proc_type.
      PERFORM new_record .
*    WHEN 'VSLT'.
*      CLEAR g_proc_type.
*      PERFORM view_result.
    WHEN 'CSLT'.
      CLEAR g_proc_type.
      PERFORM new_record .

    WHEN 'ROUT'.
      g_proc_type = 'BDC'.
      __cls : it_rout,it_display .
      PERFORM make_bdc_rout.
      PERFORM get_routing_info.
      CHECK : g_error IS INITIAL.
      __cls gt_display.
      SORT it_display BY werks matnr kostl.
      gt_display[] = it_display[].

      CALL SCREEN 200.

  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.

  CHECK : g_error IS INITIAL.
  CLEAR flag_data_changed.
  PERFORM apply_icon.
  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.
  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.
  sort_tab :
             'KOKRS'    '1'  'X' '' 'X' '',
             'BDATJ'    '2'  'X' '' 'X' '',
*             'VERSN'    '3'  'X' '' 'X' '',
             'KOSTL'    '4'  'X' '' 'X' '',
             'ABTEI'    '5'  'X' '' 'X' '',
             '$PERIO'   '6'  'X' '' 'X' '',
             'DATUB'    '7'  'X' '' 'X' ''.

ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.
  CLEAR : g_error.
  __cls : it_row_tab.

ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate.
*  CLEAR TKA01.
*
*  SELECT SINGLE * FROM TKA01
*                 WHERE KOKRS = P_KOKRS.
*  IF SY-SUBRC <> 0.
*    MESSAGE S038 WITH P_KOKRS.
*    G_ERROR = TRUE.
*  ENDIF.
ENDFORM.                    " VALIDATE_
*&---------------------------------------------------------------------*
*&      Form  default_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_.

  WRITE:
*          ICON_KEYFIGURE_ACT AS ICON TO CRAT,
*         'Calculate Cap.R Step-2' TO CRAT+4(24),
*          ICON_BIW_REPORT_VIEW AS ICON TO VSLT,
*         'Rel. Capa.Rate  Step-3' TO VSLT+4(24).
          icon_biw_report_view AS ICON TO cslt,
         'Maintain Efficiency' TO cslt+4(24).

  WRITE:
          icon_routing AS ICON TO rout,
         'Create Routing' TO rout+4(21).

*monthly labor performance rate
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.
  p_eff-low = '100'. APPEND p_eff.

ENDFORM.                    " default_
*&---------------------------------------------------------------------*
*&      Form  refine_row_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_row_itab.
  CHECK g_error EQ space.
  __process 'Refining data' '70'.

ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.
*  CHECK P_DSP EQ TRUE.
  __process 'Preparing output...' '95'.

  __cls gt_out.

* get department {
  DATA : BEGIN OF $kostl OCCURS 0,
           kostl     LIKE csks-kostl,
         END   OF  $kostl.

  DATA : BEGIN OF $kostl_abtei OCCURS 0,
           kostl     LIKE csks-kostl,
           abtei     LIKE csks-abtei,
         END   OF  $kostl_abtei.

  LOOP AT it_row_tab.
    $kostl-kostl = it_row_tab-kostl.
    COLLECT $kostl.
  ENDLOOP.

  SELECT kostl abtei INTO TABLE $kostl_abtei
  FROM csks
   FOR ALL ENTRIES IN $kostl
   WHERE kokrs EQ p_kokrs
    AND  kostl EQ $kostl-kostl.

  SORT $kostl_abtei BY kostl.
* }

  DATA : $year(4) TYPE n,
         $period(3) TYPE n.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    PERFORM convert_period USING it_row_tab-perio
                        CHANGING gt_out-$perio.

    gt_out-kokrs = p_kokrs.
    gt_out-bdatj = p_bdatj.
*    GT_OUT-VERSN = P_VERSN.
    READ TABLE $kostl_abtei WITH KEY kostl = it_row_tab-kostl
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-abtei = $kostl_abtei-abtei.
    ENDIF.

    $year = it_row_tab-perio DIV 100.
    $period = it_row_tab-perio - ( $year * 100 ).

    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
         EXPORTING
              i_gjahr = $year
              i_periv = 'K0'
              i_poper = $period
         IMPORTING
              e_date  = gt_out-datub.

    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  SAVE_z_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_table.

* by ig.moon {
*  IF P_BDATJ <= SY-DATUM(4).
*    EXIT.
*  ENDIF.
* }

  DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message
* Save seleted data to table ZTCOU128
  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.

  DATA  : i_ztcou128 LIKE ztcou128 OCCURS 0 WITH HEADER LINE,
          ls_ztcou128 LIKE ztcou128,
          lt_del_rows TYPE TABLE OF ztcou128.

* Delete Lines
  CALL METHOD g_event_receiver->get_deleted_rows
            IMPORTING deleted_rows = lt_del_rows.

  READ TABLE lt_del_rows INTO *ztcou128 WITH KEY released = true.
  IF sy-subrc EQ 0.
    MESSAGE i000 WITH 'You can not delete the released line(s).'
                      'Deleting the recored(s) will be ignored!'.
    DELETE lt_del_rows WHERE released EQ true.
  ENDIF.

  DELETE ztcou128 FROM TABLE lt_del_rows.

  CALL METHOD g_event_receiver->refresh_delta_tables.

  READ TABLE $gt_out WITH KEY released = true.
  IF sy-subrc EQ 0.
    MESSAGE i000 WITH 'You can not apply with the released line(s).'
                      'Applying the recored(s) will be ignored!'.
  ENDIF.

  LOOP AT $gt_out.
    CHECK $gt_out-released NE true.
    MOVE-CORRESPONDING $gt_out TO *ztcou128.
     *ztcou128-aedat = sy-datum.
     *ztcou128-aenam = sy-uname.
    i_ztcou128 = *ztcou128.
    i_ztcou128-confirmed = 'X'.
    APPEND i_ztcou128.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.

  READ TABLE i_ztcou128 INDEX 1.
  IF sy-subrc EQ 0.
    MODIFY ztcou128 FROM TABLE i_ztcou128.
  ELSE.
    EXIT.
  ENDIF.

  DATA $subrc(1).

  PERFORM update_tc31a TABLES lt_del_rows
                              i_ztcou128
                       CHANGING $subrc.
  IF $subrc EQ 'E'.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.
  DESCRIBE TABLE lt_del_rows LINES lv_dcnt.
  IF lv_dcnt > 0.
    IF lv_cnt > 0.
      CONCATENATE 'Data has been deleted' lv_dcnt  'record(s),'
                  'saved' lv_cnt 'record(s).'
             INTO lv_msg SEPARATED BY space.
    ELSE.
      CONCATENATE 'Data has been deleted' lv_dcnt  'record(s).'
             INTO lv_msg SEPARATED BY space.
    ENDIF.
  ELSE.
    CONCATENATE 'Data has been saved;'
                 lv_cnt  'record(s).'
            INTO lv_msg SEPARATED BY space.
  ENDIF.
  IF lv_dcnt > 0 OR lv_cnt > 0.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  PERFORM apply_icon.
  CLEAR flag_data_changed.

ENDFORM.                    " SAVE_z_TABLE
*&---------------------------------------------------------------------*
*&      Form  VIEW_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_.
  CLEAR g_error.
  __cls : it_row_tab,gt_out.

  SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM ztcou128 WHERE kokrs EQ p_kokrs
                  AND bdatj EQ p_bdatj.
*                  AND VERSN EQ P_VERSN.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'Could not find data.'.
    g_error = true.
    EXIT.
  ENDIF.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    PERFORM convert_period USING it_row_tab-perio
                        CHANGING gt_out-$perio.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " VIEW_
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  DATA_DELETE_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM really?.
  DATA $exists(1).
  DATA l_answer(1).

  PERFORM pop_up USING
      'The existing data will be refreshed!'
      'Do you really want to save?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.


ENDFORM.                    " DATA_DELETE_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  select_cosl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_cosl.
  CHECK g_error IS INITIAL.

  __process 'Read available M/H' '10'.

  __cls : it_cosl_temp, it_cosl .

  DATA : l_cnt(3) TYPE n,
         l_field(20),
         l_field2(20).

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cosl_temp
     FROM cosl
     WHERE gjahr = p_bdatj
       AND ( wrttp = '01' AND versn = p_versn )
       AND objnr LIKE '%MAN_HR%'.

  LOOP AT it_cosl_temp.
    MOVE-CORRESPONDING it_cosl_temp TO it_cosl.
    it_cosl-kostl = it_cosl_temp-objnr+6(6).
    CLEAR : l_cnt.
    DO 12 TIMES.
      l_cnt = l_cnt + 1.
*-------plan qty
      CONCATENATE 'IT_COSL_TEMP-LST' l_cnt INTO l_field.
      ASSIGN  (l_field)    TO   <f_field> .

      CONCATENATE 'IT_COSL-MH' l_cnt INTO l_field2.
      ASSIGN  (l_field2)  TO   <f_field2> .

      READ TABLE p_eff INDEX l_cnt.
      IF sy-subrc <> 0.
        READ TABLE p_eff INDEX 1.
      ENDIF.

      <f_field2> = <f_field> * p_eff-low / 100.

    ENDDO.
    COLLECT  it_cosl.
    CLEAR it_cosl.
  ENDLOOP.
  SORT it_cosl BY kostl.

ENDFORM.                    " select_cosl
*&---------------------------------------------------------------------*
*&      Form  select_plaf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_plaf.
  CHECK g_error IS INITIAL.

  DATA : $psttr TYPE psttr,
         $pedtr TYPE pedtr.

  __process 'Read Plan...' '20'.

  __cls : it_plaf_temp, it_plaf.

  CONCATENATE p_bdatj : '0101' INTO $psttr,
                        '1231' INTO $pedtr.

  SELECT a~plnum a~plwrk a~matnr
         a~gsmng a~pedtr
         b~sauft c~mtart c~bismt b~sfepr
   INTO CORRESPONDING FIELDS OF TABLE it_plaf_temp
     FROM plaf AS a
     INNER JOIN marc AS b
     ON b~matnr EQ a~matnr
     AND b~werks EQ a~pwwrk
     AND b~fevor NE space
     INNER JOIN mara AS c
     ON c~matnr EQ b~matnr
     WHERE a~plscn = p_plscn              "Scenario
       AND a~sobes = 'E'                  "Special procurement
       AND a~matnr IN s_matnr
       AND a~psttr >= $psttr
       AND a~pedtr <= $pedtr.

  LOOP AT it_plaf_temp.
    MOVE-CORRESPONDING it_plaf_temp TO it_plaf.
    it_plaf-perio(6) = it_plaf_temp-pedtr.
    COLLECT it_plaf. CLEAR it_plaf.
  ENDLOOP.

  SORT it_plaf BY plwrk matnr.

ENDFORM.                    " select_plaf
*&---------------------------------------------------------------------*
*&      Form  select_routing_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_routing_info.
  CHECK g_error IS INITIAL.
  __process 'Read Routing...' '30'.

  __cls : it_mara,it_mip,it_mi,it_mip,it_plpo_temp,it_plpo, it_crhd.
  __cls : it_rate,it_product.

  CLEAR g_startdt.
  CONCATENATE p_bdatj '0101' INTO g_startdt.
  CHECK NOT it_plaf[] IS INITIAL.


  DATA $mi TYPE matnr.
  LOOP AT it_plaf .
    IF it_plaf-sauft = 'X'.
      IF g_proc_type = 'BDC' AND it_plaf-sfepr = 'VEHI'.
        CLEAR $mi.
        CALL FUNCTION 'Z_CO_GET_MI_CODE_SINGLE'
             EXPORTING
                  p_fsc     = it_plaf-matnr
             IMPORTING
                  p_mi      = $mi
             EXCEPTIONS
                  not_found = 1
                  OTHERS    = 2.
        IF sy-subrc <> 0.
        ELSE.
*-------- reference rate routing
          it_mi-plnnr = $mi.
          it_mi-matnr = it_plaf-matnr.
          APPEND it_mi. CLEAR it_mi.
        ENDIF.
*        IT_RATE-MATNR = IT_PLAF-MATNR.
*        IT_RATE-BISMT = IT_PLAF-BISMT.
*        APPEND IT_RATE. CLEAR IT_RATE.
      ENDIF.

*---- rate routing
      it_rate-matnr = it_plaf-matnr.
      it_rate-bismt = it_plaf-bismt.
      APPEND it_rate. CLEAR it_rate.
    ELSE.
*---- production routing
      it_product-matnr = it_plaf-matnr.
      it_product-bismt = it_plaf-bismt.   "old material number
      APPEND it_product. CLEAR it_product.
    ENDIF.
  ENDLOOP.

  SORT: it_product, it_rate, it_mi.
  DELETE ADJACENT DUPLICATES FROM : it_product, it_rate, it_mi.

  IF NOT it_mi[]  IS INITIAL.
    PERFORM make_mi_routing.
  ENDIF.

  IF NOT it_rate[] IS INITIAL.
    PERFORM make_mip_routing TABLES it_rate
                             USING  'R'.
  ENDIF.
  IF NOT it_product[] IS INITIAL.
    PERFORM make_mip_routing TABLES it_product
                             USING  'N'.
  ENDIF.

* For get work center mapping
  SELECT objid arbpl INTO CORRESPONDING FIELDS OF TABLE it_crhd
     FROM crhd
     FOR ALL ENTRIES IN it_plpo
     WHERE objty = 'A'
       AND objid = it_plpo-arbid.

  SORT : it_crhd BY objid,
         it_mip BY matnr,
         it_mi BY matnr,
         it_rate BY matnr,
         it_product BY matnr.

ENDFORM.                    " select_routing_info
*&---------------------------------------------------------------------*
*&      Form  make_mip_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PRODUCT  text
*      -->P_1020   text
*----------------------------------------------------------------------*
FORM make_mip_routing TABLES   p_tab  STRUCTURE it_rate
                       USING   p_plnty.

  DATA : it_plpo_temp2 LIKE db_plpo OCCURS 0 WITH HEADER LINE.
  DATA : it_plpo_temp3 LIKE db_plpo OCCURS 0 WITH HEADER LINE.
  DATA : $verwe TYPE pln_verwe.

  __cls : it_plpo_temp2, it_plpo_temp3.

*  IF g_proc_type IS INITIAL.
*    $verwe = '10'.
*  ELSE.
*    $verwe = p_verwe.
*  ENDIF.

*  DO 2 TIMES.
*
**    perform select_routing_data using P_PLNTY
**                                    L_VERWE
**                            tables  p_tab
**                                    it_plpo_temp2.
*    SELECT
*          d~werks
*          a~matnr
*          d~plnty
*          d~plnnr
*          d~plnkn
*          d~arbid
*          d~zaehl
*          d~datuv
*
*          d~vgw01
*          d~vgw02
*          d~vgw03
*
*          d~vge01
*          d~vge02
*          d~vge03
*          d~aennr
*          c~plnal
*          d~aufak
*          b~loekz
*     INTO CORRESPONDING FIELDS OF TABLE it_plpo_temp2
*       FROM mapl AS a
*      INNER JOIN plko AS b
*        ON  a~plnty = b~plnty
*       AND  a~plnnr = b~plnnr
*       AND  a~plnal = b~plnal
*      INNER JOIN plas AS c
*         ON b~plnty = c~plnty
*        AND b~plnnr = c~plnnr
*       AND  b~plnal = c~plnal
*      INNER JOIN plpo AS d
*         ON c~plnty = d~plnty
*        AND c~plnnr = d~plnnr
*        AND c~plnkn = d~plnkn
*      FOR ALL ENTRIES IN p_tab
*      WHERE a~plnty = p_plnty         "R:Rate routing N:Product
*        AND a~matnr = p_tab-matnr
*        AND a~loekz = ''
*        AND b~verwe = $verwe          "Usage
*        AND b~datuv <= g_startdt      "Valid from
*
**        AND B~LOEKZ = ''              "Delete indicator
*        AND c~loekz = ''              "Delete indicator
*        AND d~loekz = ''.             "Delete indicator
*
*    IF sy-subrc EQ 0 OR g_proc_type IS INITIAL.
*      EXIT.
*    ENDIF.
*
*    $verwe = '1'.
*  ENDDO.

  LOOP AT p_tab.

    IF g_proc_type IS INITIAL.   "if not routing creation, use '10'
      $verwe = '10'.
    ELSE.
      $verwe = p_verwe.
    ENDIF.

    DO 2 TIMES.

      SELECT
            d~werks
            a~matnr
            d~plnty
            d~plnnr
            d~plnkn
            d~arbid
            d~zaehl
            d~datuv

            d~vgw01
            d~vgw02
            d~vgw03

            d~vge01
            d~vge02
            d~vge03
            d~aennr
            c~plnal
            d~aufak
            b~loekz
       APPENDING CORRESPONDING FIELDS OF TABLE it_plpo_temp2
         FROM mapl AS a
        INNER JOIN plko AS b
          ON  a~plnty = b~plnty
         AND  a~plnnr = b~plnnr
         AND  a~plnal = b~plnal
        INNER JOIN plas AS c
           ON b~plnty = c~plnty
          AND b~plnnr = c~plnnr
         AND  b~plnal = c~plnal
        INNER JOIN plpo AS d
           ON c~plnty = d~plnty
          AND c~plnnr = d~plnnr
          AND c~plnkn = d~plnkn
        WHERE a~plnty = p_plnty         "R:Rate routing N:Product
          AND a~matnr = p_tab-matnr
          AND a~loekz = ''
          AND b~verwe = $verwe          "Usage
          AND b~datuv <= g_startdt      "Valid from

          AND c~loekz = ''              "Delete indicator
          AND d~loekz = ''.             "Delete indicator

      IF sy-subrc EQ 0 OR g_proc_type IS INITIAL.
        EXIT.
      ENDIF.

      $verwe = '1'.  "Try usage 1 if failed...
    ENDDO.

  ENDLOOP.

* routing creation; refer old material number
  IF NOT g_proc_type IS INITIAL.
    SORT it_plpo_temp2 BY matnr.
    LOOP AT p_tab.
      READ TABLE it_plpo_temp2 WITH KEY matnr = p_tab-matnr
           BINARY SEARCH.
      IF sy-subrc EQ 0.

      ELSE.

        $verwe = '1'.
        __cls it_plpo_temp3.
        SELECT
              d~werks
              a~matnr
              d~plnty
              d~plnnr
              d~plnkn
              d~arbid
              d~zaehl
              d~datuv

              d~vgw01
              d~vgw02
              d~vgw03

              d~vge01
              d~vge02
              d~vge03
              d~aennr
              c~plnal
              d~aufak
              b~loekz

         INTO CORRESPONDING FIELDS OF TABLE it_plpo_temp3
           FROM mapl AS a
          INNER JOIN plko AS b
            ON  a~plnty = b~plnty
           AND  a~plnnr = b~plnnr
           AND  a~plnal = b~plnal
          INNER JOIN plas AS c
             ON b~plnty = c~plnty
            AND b~plnnr = c~plnnr
           AND  b~plnal = c~plnal
          INNER JOIN plpo AS d
             ON c~plnty = d~plnty
            AND c~plnnr = d~plnnr
            AND c~plnkn = d~plnkn
          WHERE a~plnty = p_plnty         "R:Rate routing N:Product
            AND a~matnr = p_tab-bismt
            AND a~loekz = ''
            AND b~verwe = $verwe          "Usage
            AND b~datuv <= g_startdt      "Valid from

*            AND B~LOEKZ = ''              "Delete indicator
            AND c~loekz = ''              "Delete indicator
            AND d~loekz = ''.             "Delete indicator

        IF sy-subrc EQ 0.
          it_plpo_temp3-matnr = p_tab-matnr.
          MODIFY it_plpo_temp3 TRANSPORTING matnr
                                WHERE matnr NE space.
          APPEND LINES OF it_plpo_temp3 TO it_plpo_temp2.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM make_itemp TABLES it_plpo_temp2.

  DELETE : it_plpo_temp2 WHERE datub < g_startdt.

  SORT it_plpo_temp2 BY plnnr arbid matnr ASCENDING datuv DESCENDING.

  DELETE ADJACENT DUPLICATES FROM it_plpo_temp2
      COMPARING plnnr arbid matnr.

  LOOP AT it_plpo_temp2.

    MOVE-CORRESPONDING it_plpo_temp2 TO it_plpo.
    IF p_plnty = 'R'.
      it_plpo-type = 'R'.
    ELSE.
      it_plpo-type = 'N'.
    ENDIF.

    COLLECT it_plpo. CLEAR it_plpo.

  ENDLOOP.

ENDFORM.                    " make_mip_routing
*&---------------------------------------------------------------------*
*&      Form  make_mi_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_mi_routing.

* MI Rounting (FERT)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_plpo_temp
     FROM plko AS a
    INNER JOIN plas AS b
       ON a~plnty = b~plnty
      AND a~plnnr = b~plnnr
      AND a~plnal = b~plnal
    INNER JOIN plpo AS c
       ON b~plnty = c~plnty
      AND b~plnnr = c~plnnr
      AND b~plnkn = c~plnkn
      FOR ALL ENTRIES IN it_mi
     WHERE a~plnty = 'M'
       AND a~plnnr = it_mi-plnnr
       AND a~verwe = '1'             "Usage
       AND a~statu IN ('3', '4')     "Status
       AND a~datuv <= g_startdt      "Valid from
       AND a~delkz = ''              "Delete indicator
       AND b~loekz = ''              "Delete indicator
       AND c~loekz = '' .            "Delete indicator

* delete old data; change number
  SORT it_plpo_temp BY plnnr arbid ASCENDING
                       datuv       DESCENDING.

*  DATA: W_PLPO_TEMP LIKE IT_PLPO_TEMP.
*  LOOP AT IT_PLPO_TEMP.
*    IF  IT_PLPO_TEMP-PLNNR = W_PLPO_TEMP-PLNNR
*    AND IT_PLPO_TEMP-ARBID = W_PLPO_TEMP-ARBID.
*      DELETE IT_PLPO_TEMP.
*    ENDIF.
*    W_PLPO_TEMP = IT_PLPO_TEMP.
*  ENDLOOP.

  SORT it_mi.
  DELETE ADJACENT DUPLICATES FROM it_mi.

  DELETE ADJACENT DUPLICATES FROM it_plpo_temp
      COMPARING plnnr arbid.

  LOOP AT it_mi.
    LOOP AT it_plpo_temp WHERE plnnr = it_mi-plnnr.
      MOVE-CORRESPONDING it_plpo_temp TO it_plpo_ref.
      it_plpo_ref-matnr = it_mi-matnr.
      it_plpo_ref-type = 'M'.
      COLLECT it_plpo_ref.  CLEAR it_plpo_ref.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " make_mi_routing
*&---------------------------------------------------------------------*
*&      Form  calculate_abp_mh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_abp_mh.
  CHECK g_error IS INITIAL.
  __process 'Calc...' '50'.

  DATA : l_cnt(3) TYPE n,
         l_field(40).

  LOOP AT itab.
    it_row_tab-perio = itab-perio.
    it_row_tab-kostl = itab-kostl.
*    IT_ROW_TAB-VGE03 = ITAB-VGE03.
    COLLECT it_row_tab. CLEAR it_row_tab.
  ENDLOOP.

  SORT tmpt BY kostl perio.

  LOOP AT it_row_tab.

    CLEAR it_cosl.
    READ TABLE it_cosl WITH KEY kostl = it_row_tab-kostl
                                          BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    l_cnt = it_row_tab-perio+4(2).

*    CONCATENATE 'IT_COSL-MH' L_CNT INTO L_FIELD.
    CONCATENATE 'IT_COSL-KAP' l_cnt INTO l_field.
    ASSIGN  (l_field)    TO   <f_field> .
    it_row_tab-cc_capa = <f_field>.

*    READ TABLE tmpt WITH KEY kostl = it_row_tab-kostl
*                             perio = it_row_tab-perio
*                             BINARY SEARCH.
*
*    CHECK sy-subrc EQ 0.
*    it_row_tab-eng_mh = tmpt-eng_mh.

    CONCATENATE 'IT_COSL-MH' l_cnt INTO l_field.
    ASSIGN  (l_field)    TO   <f_field> .
    it_row_tab-eng_mh = <f_field>.

    CLEAR it_row_tab-cc_rate.
    IF it_row_tab-cc_capa <> 0.
     it_row_tab-cc_rate =  it_row_tab-eng_mh / it_row_tab-cc_capa * 100.
    ENDIF.
     it_row_tab-cc_ratd  = it_row_tab-cc_rate.  "% dicimal


    MODIFY it_row_tab.

  ENDLOOP.

ENDFORM.                    " calculate_abp_mh
*&---------------------------------------------------------------------*
*&      Form  convert_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PLAF_PERIO  text
*      <--P_IT_ROW_TAB_PERIO  text
*----------------------------------------------------------------------*
FORM convert_period USING    p_perio
                    CHANGING p_jahrper.

  DATA : $year(4) TYPE n,
         $period(3) TYPE n.

  $year = p_perio DIV 100.
  $period = p_perio - ( $year * 100 ).

  CONCATENATE $year $period INTO p_jahrper.

ENDFORM.                    " convert_period
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.
* by ig.moon {
*  IF P_BDATJ <= SY-DATUM(4).
*    EXIT.
*  ENDIF.
* }
  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 1.
    SET PF-STATUS '100'.
    PERFORM info_text_set USING true.
  ELSE.
    IF flag_data_changed EQ true.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
           EXPORTING
                textline1     = 'Data has not been saved yet.'
                textline2     = 'Do you want to continue anyway? '
                titel         = 'Confirmation'
                defaultoption = 'N'
           IMPORTING
                answer        = answer.
      CHECK answer EQ 'J'.
    ENDIF.
    CLEAR flag_data_changed.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
    PERFORM info_text_set USING false.
  ENDIF.

  PERFORM build_cell_attr.

ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.

*   Exclude toolbar
  PERFORM exclude_functions.
  __cls ftab.

  IF sscrfields-ucomm NE 'VSLT'.
*    FTAB-FCODE = 'RELS'.
*    APPEND FTAB. CLEAR FTAB.
*    FTAB-FCODE = 'CREL'.
*    APPEND FTAB. CLEAR FTAB.
  ENDIF.

* by ig.moon {
*  IF P_BDATJ <= SY-DATUM(4).
*    FTAB-FCODE = 'CRER'.
*    APPEND FTAB. CLEAR FTAB.
*    FTAB-FCODE = 'SWITCH'.
*    APPEND FTAB. CLEAR FTAB.
*    FTAB-FCODE = 'SAVE'.
*    APPEND FTAB. CLEAR FTAB.
*    SET PF-STATUS '100' EXCLUDING FTAB.
*  ELSE.

  ftab-fcode = 'SAV2'.
  APPEND ftab. CLEAR ftab.
  ftab-fcode = 'CRER'.
  APPEND ftab. CLEAR ftab.
  ftab-fcode = 'DELS'.
  APPEND ftab. CLEAR ftab.
  SET PF-STATUS '100' EXCLUDING ftab.
*  ENDIF.

  icon_not_ready = icon_led_inactive.
  icon_ready     = icon_rating_positive.
  icon_confirmed = icon_checked.
  icon_released  = icon_release.

ENDMODULE.                 " STATUS_0100  OUTPUT
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
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  __focus g_grid.
ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR : g_error.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      PERFORM free_container.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.

      IF sy-dynnr EQ '0100'.
        PERFORM really?.
        CHECK g_error NE true.

        PERFORM : save_table,
                  refresh_alv.
        __focus g_grid.
      ENDIF.

    WHEN 'SAV2'.

      IF sy-dynnr EQ '0200'.
        PERFORM only_save_to_ztable.
      ENDIF.


    WHEN 'SWITCH'.
      IF sy-dynnr EQ '0100'.
        PERFORM switch_edit_mode.
      ENDIF.
      IF sy-dynnr EQ '0200'.
        PERFORM switch_edit_mode_200.
      ENDIF.
      __focus g_grid.

    WHEN 'RELS'.
      CHECK sy-dynnr EQ '0100'.
      PERFORM :release_rate,
               refresh_alv .
    WHEN 'CREL'.
      CHECK sy-dynnr EQ '0100'.
      PERFORM :un_release_rate,
               refresh_alv .

    WHEN 'CRER'.
      CHECK sy-dynnr EQ '0200'.
      PERFORM : create_prd_route,
                refresh_alv .
    WHEN 'DELS'.
      CHECK sy-dynnr EQ '0200'.
      PERFORM : dele_prd_route,
                refresh_alv .
    WHEN 'REST'.
      CHECK sy-dynnr EQ '0100'.
      PERFORM : reset_cc,
                refresh_alv .

    WHEN 'LOGV'.
      CALL SCREEN '300'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.

*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
  PERFORM set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

*   Define cell attribute
  PERFORM build_cell_attr.

ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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

*---------------------------------------------------------------------*
*       FORM exclude_functions_200                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM exclude_functions_200.
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
                  cl_gui_alv_grid=>mc_fc_loc_delete_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
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
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'KOKRS'    'C.Area'            4  'CHAR' '',
    'X'  'BDATJ'    'Year'              4  'NUMC' '',
*    'X'  'VERSN'    'Ver.'              3  'CHAR' '',
    'X'  'KOSTL'    'Cost.C'           10  'CHAR' '',
    'X'  'ABTEI'    'ABTEI'            12  'CHAR' '',
    'X'  '$PERIO'   'PERIOD'            7  'NUMC' '',
    'X'  'DATUB'    'Valid-To'          8  'DATS' '',
    ' '  'CC_CAPA'  'Capa.'            15  'QUAN' '',
    ' '  'ENG_MH'   'Plan'             15  'QUAN' '',
    ' '  'CC_RATD'  'KSPP(%)'          15  'DEC'  '',
    ' '  'ICON_C'   'flg'               3  'ICON' '',
    ' '  'NOR_R'    'ABP backup%'      15  'DEC'  '',
    ' '  'ICON'     'flg'               3  'ICON' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'CC_CAPA' OR 'ENG_MH' OR 'CC_RATD' OR 'NOR_R'.
        gs_fcat-just = 'R'.
        gs_fcat-no_zero = 'X'.
        MODIFY gt_fcat FROM gs_fcat.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout.
  CLEAR gs_layo.

  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.

ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.
  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
            'KOKRS'     '1' 0,
            'BDATJ'     '1' 0,
            'VERSN'     '1' 0,
            'KOSTL'     '2' 0,
            'ABTEI'     '3' 0,
            '$PERIO'    '2' 0,
            'CC_CAPA'   '1' 0,
            'DATUB'     '2' 0,
            'ENG_MH'    '1' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  flag_data_changed = true.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.
  DATA  $kostl TYPE kostl.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    READ TABLE gt_out INDEX ls_mod_cells-row_id.
    IF sy-subrc = 0.
      $kostl = gt_out-kostl.
      CALL METHOD rr_data_changed->modify_cell
                EXPORTING i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
      IF ls_mod_cells-fieldname EQ 'CC_RATD'.
        gt_out-cc_ratd = ls_mod_cells-value.
      ELSE.
        gt_out-nor_r = ls_mod_cells-value.
      ENDIF.

      IF gt_out-nor_r EQ 0.
        gt_out-icon = icon_led_inactive.
      ELSE.
        gt_out-icon = icon_rating_positive.
      ENDIF.

      MODIFY gt_out TRANSPORTING cc_ratd nor_r
                                  icon WHERE kostl EQ $kostl.

      MODIFY gt_out INDEX ls_mod_cells-row_id TRANSPORTING icon.

    ENDIF.
  ENDLOOP.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM info_text_set USING p_true.

* by ig.moon {
*  IF P_BDATJ <= SY-DATUM(4).
*    INFO = TEXT-017.
*  ELSE.
  IF p_true EQ true.
    info = text-015.
  ELSE.
    info = text-015.
  ENDIF.
*  ENDIF.
* }

ENDFORM.                    " info_text_set
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
       EXPORTING is_stable = stable.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TC31A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DEL_ROWS  text
*      -->P_I_ZTCOU128  text
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
FORM update_tc31a TABLES   p_del_rows STRUCTURE ztcou128
                           p_ztcou128 STRUCTURE ztcou128
                  CHANGING p_$subrc.


* Table tc31a ( Key for performance efficiency rate ) MUST BE cusomized
* Delivery Class to 'A'
* V_TC31A also

  LOOP AT p_del_rows.
    CLEAR *tc31a.
    SELECT SINGLE * INTO *tc31a
      FROM tc31a WHERE zgrad EQ p_del_rows-abtei
                  AND datub EQ p_del_rows-datub.
    IF sy-subrc EQ 0.
      DELETE tc31a FROM *tc31a.
    ENDIF.

  ENDLOOP.

  LOOP AT p_ztcou128.
    CLEAR *tc31a.
    SELECT SINGLE * INTO *tc31a
      FROM tc31a WHERE zgrad EQ p_ztcou128-abtei
                  AND datub EQ p_ztcou128-datub.
    IF sy-subrc EQ 0.
       *tc31a-zgkal = p_ztcou128-cc_ratd.
       *tc31a-zgter = p_ztcou128-nor_r.
      UPDATE tc31a FROM *tc31a.
    ELSE.
       *tc31a-zgrad = p_ztcou128-abtei.
       *tc31a-datub = p_ztcou128-datub.
       *tc31a-zgkal = p_ztcou128-cc_ratd.
       *tc31a-zgter = p_ztcou128-nor_r.
      INSERT tc31a FROM *tc31a.
    ENDIF.

    IF sy-subrc NE 0.
      p_$subrc = 'E'.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " UPDATE_TC31A
*&---------------------------------------------------------------------*
*&      Form  make_actual_display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_actual_display_data.
  DATA : l_cnt(3) TYPE n,
         l_field(40),
         l_total LIKE it_display-abptot.

  __cls : it_rout, it_display .

  SORT itab.
  LOOP AT itab.
    AT NEW kostl .
      CLEAR : g_val_cnt, l_total.
    ENDAT.

    CLEAR l_field.
    MOVE-CORRESPONDING itab TO it_rout.
    it_rout-type = 'A'.

    IF itab-mtart = 'FERT'.
      it_rout-bdc_type = 'R'.
    ELSE.
      CLEAR it_rate.
      READ TABLE it_rate WITH KEY matnr = itab-matnr BINARY SEARCH.
      IF sy-subrc = 0 .
        it_rout-bdc_type = 'R'.
      ELSE.
        CLEAR it_product.
        READ TABLE it_product WITH KEY matnr = itab-matnr BINARY SEARCH.
        IF sy-subrc = 0 .
          it_rout-bdc_type = 'N'.
        ENDIF.
      ENDIF.
    ENDIF.
    l_cnt = itab-perio+4(2).
    CONCATENATE 'IT_ROUT-ABP' l_cnt INTO l_field.
    ASSIGN (l_field)    TO   <f_field> .
    <f_field> = itab-abp_mh .
    IF  <f_field> <>  0 .
      g_val_cnt = g_val_cnt + 1.
    ENDIF.

*   MH Average
*   FIXME (count if not ZERO)
    IF g_val_cnt <>  0 .
      l_total  = <f_field> + l_total.
    ENDIF.

    CONCATENATE 'IT_ROUT-SET' l_cnt INTO l_field.
    ASSIGN (l_field)    TO   <f_field> .
    <f_field> = itab-vgw01 .

    CONCATENATE 'IT_ROUT-MCH' l_cnt INTO l_field.
    ASSIGN (l_field)    TO   <f_field> .
    <f_field> = itab-vgw02 .
    AT END OF kostl.
      IF g_val_cnt <> 0 .
        it_rout-abptot = l_total / g_val_cnt .
      ENDIF.
    ENDAT.
    COLLECT  it_rout.
    MOVE-CORRESPONDING it_rout TO it_display.
    COLLECT it_display. CLEAR it_display.
    CLEAR it_rout.
  ENDLOOP.

ENDFORM.                    " make_actual_display_data
*&---------------------------------------------------------------------*
*&      Form  make_product_display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_product_display_data.
  DATA : l_cnt(3) TYPE n,
         l_field(40).

  SORT it_plaf.
  LOOP AT it_plaf WHERE matnr IN s_matnr.
    CLEAR l_field.
    it_rout-werks = it_plaf-plwrk.
    it_rout-matnr = it_plaf-matnr.
    it_rout-type  = 'P'.
    IF it_plaf-mtart = 'FERT'.
      it_rout-bdc_type = 'R'.
    ELSE.
      CLEAR it_rate.
      READ TABLE it_rate WITH KEY matnr = it_rout-matnr.
      IF sy-subrc = 0 .
        it_rout-bdc_type = 'R'.
      ELSE.
        CLEAR it_product.
        READ TABLE it_product WITH KEY matnr = it_rout-matnr.
        IF sy-subrc = 0 .
          it_rout-bdc_type = 'N'.
        ENDIF.
      ENDIF.
    ENDIF.

    l_cnt = it_plaf-perio+4(2).
    CONCATENATE 'IT_ROUT-ABP' l_cnt INTO l_field.
    ASSIGN  (l_field)  TO   <f_field> .
    <f_field> = it_plaf-gsmng .
    it_rout-abptot = it_plaf-gsmng .
    COLLECT  it_rout.
    MOVE-CORRESPONDING it_rout TO it_display.
    COLLECT it_display. CLEAR it_display.
    CLEAR it_rout.
  ENDLOOP.

ENDFORM.                    " make_product_display_data
*&---------------------------------------------------------------------*
*&      Form  GET_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_itab.

* Avaialable M/H : by Cost center
  PERFORM select_cosl.
* Product Manufacturing : by Material
  PERFORM select_plaf.
* Rounting M/H(Labor) : by Cost center
  PERFORM select_routing_info.

  CHECK g_error IS INITIAL.
  __process 'Get Plan...' '40'.

  DATA : l_cnt(3) TYPE n,
         l_field(40).

  __cls tmpt.


  DATA $ix LIKE sy-tabix.
  DATA $plpo_ix LIKE sy-tabix.
  DATA $date_from LIKE sy-datum.
  DATA $plnty(1).

  SORT it_plaf.
  LOOP AT it_plaf.
    $ix = sy-tabix.

    READ TABLE it_plpo WITH KEY matnr = it_plaf-matnr .
    IF sy-subrc EQ 0.
      $plpo_ix = sy-tabix.
    ELSE.
      CONTINUE.
    ENDIF.

* {
    IF it_plaf-sauft EQ 'X'.
      $plnty = 'R'.
    ELSE.
      $plnty = 'N'.
    ENDIF.
    CONCATENATE it_plaf-perio '01' INTO $date_from.

    IF  g_proc_type IS INITIAL.    " Not for BDC
      CALL FUNCTION 'CP_CC_S_TSK_EXISTENCE_CHECK'
           EXPORTING
                i_date_from    = $date_from
                i_plnty        = $plnty
                i_plnnr        = it_plpo-plnnr
                i_plnal        = it_plpo-plnal
           EXCEPTIONS
                task_not_found = 1
                OTHERS         = 2.
      IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Error(s) was(were) occured when data gathering.'
                                          'Please check the error log!'
                                                                       .
        CONTINUE.
      ENDIF.
    ENDIF.
* }

    LOOP AT it_plpo FROM $plpo_ix.
      IF it_plpo-matnr NE it_plaf-matnr .
        EXIT.
      ENDIF.
      tmpt-perio   = it_plaf-perio.
      CLEAR it_crhd .
      READ TABLE it_crhd WITH KEY objid = it_plpo-arbid BINARY SEARCH.
      tmpt-kostl   = it_crhd-arbpl.
      tmpt-arbpl   = it_plpo-arbid.
      tmpt-eng_mh  = it_plaf-gsmng * it_plpo-vgw03.
      CHECK tmpt-eng_mh <>  0 .
      COLLECT tmpt. CLEAR tmpt.
      it_plaf-$flag = true.
      MODIFY it_plaf INDEX $ix TRANSPORTING $flag.
    ENDLOOP.

  ENDLOOP.

  SORT tmpt        BY arbpl perio .
  SORT it_plpo     BY matnr.
  SORT it_plpo_ref BY matnr.

  __cls itab.
  DATA: lt_plpo LIKE it_plpo OCCURS 0 WITH HEADER LINE.

  LOOP AT it_plaf WHERE matnr IN s_matnr.

    REFRESH lt_plpo.
    READ TABLE it_plpo WITH KEY matnr = it_plaf-matnr .
    IF sy-subrc EQ 0.
      $plpo_ix = sy-tabix.
      LOOP AT it_plpo FROM $plpo_ix.
        IF it_plpo-matnr NE it_plaf-matnr .
          EXIT.
        ENDIF.
        APPEND it_plpo TO lt_plpo.
      ENDLOOP.
    ELSE.
      IF g_proc_type = 'BDC' AND it_plaf-sfepr = 'VEHI'.
        READ TABLE it_plpo_ref WITH KEY matnr = it_plaf-matnr .
        IF sy-subrc EQ 0.
          $plpo_ix = sy-tabix.
          LOOP AT it_plpo_ref FROM $plpo_ix.
            IF it_plpo_ref-matnr NE it_plaf-matnr .
              EXIT.
            ENDIF.
            APPEND it_plpo_ref TO lt_plpo.
          ENDLOOP.
        ENDIF.

      ENDIF.
    ENDIF.

    DESCRIBE TABLE lt_plpo LINES sy-index.
    IF sy-index > 0.
      PERFORM fill_to_itab TABLES lt_plpo.
    ENDIF.
*    LOOP AT IT_PLPO FROM $PLPO_IX.
*      IF IT_PLPO-MATNR NE IT_PLAF-MATNR .
*        EXIT.
*      ENDIF.
*      ITAB-WERKS   = IT_PLAF-PLWRK.
*      ITAB-MATNR   = IT_PLPO-MATNR.
*      ITAB-PERIO   = IT_PLAF-PERIO.
*      ITAB-TYPE    = IT_PLPO-TYPE.
*      ITAB-VGW01   = IT_PLPO-VGW01.    "Set
*      ITAB-VGW02   = IT_PLPO-VGW02.    "Machine
*
*      ITAB-VGE01   = IT_PLPO-VGE01.    "unit Set
*      ITAB-VGE02   = IT_PLPO-VGE02.    "unit Machine
*      ITAB-VGE03   = IT_PLPO-VGE03.    "unit MH
*      ITAB-AUFAK   = IT_PLPO-AUFAK.
*
*      CLEAR TMPT.
*      READ TABLE TMPT WITH KEY ARBPL = IT_PLPO-ARBID
*                               PERIO = IT_PLAF-PERIO
*                               BINARY SEARCH.
*      CHECK SY-SUBRC = 0 .
*      ITAB-KOSTL = TMPT-KOSTL.
*      ITAB-ABP_MH = IT_PLPO-VGW03.
*      ITAB-MTART = IT_PLAF-MTART.
*      CHECK NOT ITAB-ABP_MH IS INITIAL.
*      COLLECT ITAB. CLEAR ITAB.
*    ENDLOOP.
  ENDLOOP.

* following logic was copied from old program
* {
* keep for BDC logic
* 'A' -> is used for BDC, 'P' -> ignore.
  PERFORM make_actual_display_data.
  PERFORM make_product_display_data.
* }

  SORT : it_rout, it_display.

ENDFORM.                    " GET_ITAB
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_bdc_rout.

  IF it_rout[] IS INITIAL.
    PERFORM get_itab.
  ENDIF.

  CLEAR : it_bdc_rout[], it_bdc_rout.
  CLEAR it_rout.

  LOOP AT it_rout WHERE type = 'A'.
    MOVE-CORRESPONDING it_rout TO it_bdc_rout.
    APPEND it_bdc_rout. CLEAR it_bdc_rout.
  ENDLOOP.

ENDFORM.                    " make_bdc_rout
*&---------------------------------------------------------------------*
*&      Form  get_routing_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_routing_info.
  DATA : BEGIN OF it_del_rout OCCURS 0 ,
          plnty LIKE plko-plnty,
          plnnr LIKE plko-plnnr,
          plnal LIKE plko-plnal,
         END OF it_del_rout.

  CHECK NOT it_bdc_rout[] IS INITIAL.
  CLEAR : it_chg_rout, it_chg_rout[],
          it_chg_rout_del, it_chg_rout_del[].
  DATA : $aennr1(5),$aennr2(5).
  DATA   $bdatj LIKE p_bdatj.

  $bdatj = p_bdatj + 1.

  CONCATENATE : p_bdatj '%' INTO $aennr1,
                $bdatj '%' INTO $aennr2.

  SELECT matnr a~werks b~plnty b~plnnr b~plnal b~aennr b~datuv
    INTO CORRESPONDING FIELDS OF TABLE it_chg_rout
    FROM mapl AS a
   INNER JOIN plko AS b
      ON a~plnty = b~plnty
     AND a~plnnr = b~plnnr
     AND a~plnal = b~plnal
     FOR ALL ENTRIES IN it_bdc_rout
   WHERE a~plnty = it_bdc_rout-bdc_type   "R:Rate routing N:Product
     AND a~matnr = it_bdc_rout-matnr
     AND a~loekz = ''
     AND b~verwe = '10'            "Usage
     AND b~loekz = ''             "Delete indicator
     AND b~aennr LIKE $aennr1.

  SELECT matnr a~werks b~plnty b~plnnr b~plnal b~aennr b~datuv
    INTO CORRESPONDING FIELDS OF TABLE it_chg_rout_del
    FROM mapl AS a
   INNER JOIN plko AS b
      ON a~plnty = b~plnty
     AND a~plnnr = b~plnnr
     AND a~plnal = b~plnal
     FOR ALL ENTRIES IN it_chg_rout
   WHERE b~plnty = it_chg_rout-plnty
     AND b~plnnr = it_chg_rout-plnnr
     AND b~plnal = it_chg_rout-plnal
     AND a~loekz = 'X'
     AND b~verwe = '10'            "Usage
     AND b~loekz = 'X'              "Delete indicator
     AND b~aennr LIKE $aennr2.

  SORT : it_chg_rout BY matnr aennr,
         it_chg_rout_del BY matnr.

ENDFORM.                    " get_routing_info
*&---------------------------------------------------------------------*
*&      Form  call_bdc_for_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_for_routing  TABLES $it_display STRUCTURE it_display.
  DATA : l_month_cnt(3) TYPE n,
         l_cnt(5) TYPE n.
  DATA : l_type(10),
         l_new(1).

  DATA $flag.

  DATA $aennr(15).
  DATA : $bdatj LIKE keko-bdatj.
  $bdatj = p_bdatj + 1.
  CONCATENATE $bdatj '0101' '-001' INTO  $aennr.
  DATA : l_aennr(15).
  CONCATENATE p_bdatj '0101' '-001' INTO  l_aennr.

  l_month_cnt = '001'.

  LOOP AT it_bdc_rout .
    READ TABLE $it_display WITH KEY werks = it_bdc_rout-werks
                                    matnr = it_bdc_rout-matnr
                                    kostl = it_bdc_rout-kostl.
    CHECK sy-subrc EQ 0.

    AT NEW matnr.
      l_new = 'X'.
      CLEAR l_cnt.
    ENDAT.
    l_cnt = l_cnt + 1.

    IF l_new = 'X'.
      CLEAR l_new.
      PERFORM make_bdc_header  USING     l_month_cnt
                               CHANGING  l_type
                                         l_aennr
                                         it_bdc_rout-aufak.
    ENDIF.

*---FIX ME ; if man hour is zero, don't need to create it.
    IF $it_display-abp001 = 0.
      CONTINUE.
    ENDIF.

    PERFORM make_bdc_detail USING $it_display
                                  l_cnt
                                  l_month_cnt
                                  l_type
                                  it_bdc_rout-aufak.

    AT END OF matnr.
      $flag = true.
    ENDAT.

    CHECK $flag EQ true.
    CLEAR $flag.

    IF l_type = 'CREATE'.
      PERFORM bdc_dynpro  USING   'SAPLCPDI' g_screen.
    ELSE.
      PERFORM bdc_dynpro  USING   'SAPLCPDO' '1200'.
    ENDIF.
    PERFORM bdc_field USING 'BDC_OKCODE' '=BU'.

    CLEAR l_new.
    wa_opt-dismode = p_mode.
    wa_opt-updmode = 'S'.
    wa_opt-defsize = 'X'.
    CALL TRANSACTION g_code USING it_bdcdata
                            OPTIONS FROM wa_opt
                            MESSAGES INTO it_msg.

    PERFORM get_detail_msg.
    REFRESH: it_bdcdata, it_msg.
    CLEAR : g_code, l_type.

*    IF NOT IT_BDC_ROUT-AUFAK IS INITIAL.
*      PERFORM MAKE_BDC_SCRAP   USING     L_MONTH_CNT
*                               CHANGING  L_TYPE
*                                         L_AENNR
*                                         IT_BDC_ROUT-AUFAK.
*
*      CALL TRANSACTION G_CODE USING IT_BDCDATA
*                              OPTIONS FROM WA_OPT
*                              MESSAGES INTO IT_MSG.
*
*      PERFORM GET_DETAIL_MSG.
*      REFRESH: IT_BDCDATA, IT_MSG.
*      CLEAR : G_CODE, L_TYPE.
*    ENDIF.
*
*
  ENDLOOP.

  PERFORM get_routing_info.

  LOOP AT it_bdc_rout .
    AT NEW matnr.
      l_new = 'X'.
      CLEAR l_cnt.
    ENDAT.

    CHECK l_new EQ 'X'.
    CLEAR l_new.
    READ TABLE it_chg_rout_del WITH KEY matnr = it_bdc_rout-matnr
                               BINARY SEARCH.
    CHECK sy-subrc NE 0.
    READ TABLE it_chg_rout     WITH KEY matnr = it_bdc_rout-matnr
                               BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    PERFORM make_bdc_header_del  USING l_month_cnt $aennr.

    IF it_bdc_rout-bdc_type  = 'R'.
      g_code = 'CA22'.
    ELSE.
      g_code = 'CA02'.
    ENDIF.

    wa_opt-dismode = p_mode.
    wa_opt-updmode = 'S'.
    wa_opt-defsize = 'X'.
    CALL TRANSACTION g_code USING it_bdcdata
                            OPTIONS FROM wa_opt
                            MESSAGES INTO it_msg.
    PERFORM get_detail_msg.
    REFRESH: it_bdcdata, it_msg.
    CLEAR : g_code.
  ENDLOOP.

ENDFORM.                    " call_bdc_for_routing
*&---------------------------------------------------------------------*
*&      Form  make_bdc_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MONTH_CNT  text
*      <--P_L_TYPE  text
*----------------------------------------------------------------------*
FORM make_bdc_header  USING p_month_cnt
                   CHANGING
                            p_type
                            p_aennr
                            p_aufak.

  DATA : l_datuv(8).

  CLEAR it_chg_rout.
  READ TABLE it_chg_rout WITH KEY matnr = it_bdc_rout-matnr
                                  aennr = p_aennr
                                  BINARY SEARCH.
  IF sy-subrc <> 0 .
    p_type = 'CREATE'.
  ELSE.
    p_type = 'CHANGE'.
  ENDIF.

  CLEAR g_code.

  g_screen = '5400'.
  IF p_type = 'CREATE'.
    g_code = 'CA21'.
  ELSE.
    g_code = 'CA22'.
  ENDIF.

  IF it_bdc_rout-bdc_type  = 'R'.
    g_screen = '5400'.
    IF p_type = 'CREATE'.
      g_code = 'CA21'.
    ELSE.
      g_code = 'CA22'.
    ENDIF.
  ELSE.
    g_screen = '1400'.
    IF p_type = 'CREATE'.
      g_code = 'CA01'.
    ELSE.
      g_code = 'CA02'.
    ENDIF.

  ENDIF.

  IF p_type = 'CREATE'.
    PERFORM bdc_dynpro  USING   'SAPLCPDI' '1010'.
    PERFORM bdc_field   USING : 'RC27M-MATNR' it_bdc_rout-matnr ,
                                'RC27M-WERKS' it_bdc_rout-werks,
                                'RC271-AENNR' p_aennr,
                                'BDC_OKCODE' '=ANLG'.

    PERFORM bdc_dynpro  USING   'SAPLCPDA' '1010'.
    PERFORM bdc_field   USING : 'PLKOD-VERWE' p_usg,
                                'PLKOD-STATU' '4',
                                'BDC_OKCODE' '=VOUE'.
  ELSE.

    CONCATENATE it_chg_rout-datuv+4(2) it_chg_rout-datuv+6(2)
                it_chg_rout-datuv(4) INTO l_datuv.

    PERFORM bdc_dynpro  USING   'SAPLCPDI' '1010'.
    PERFORM bdc_field   USING : 'RC27M-MATNR' it_bdc_rout-matnr ,
                                'RC27M-WERKS' it_bdc_rout-werks,
                                'RC271-AENNR' p_aennr,
                                'RC271-PLNNR' ' ',
                                'RC271-STTAG' l_datuv,
                                'RC271-PLNAL' it_chg_rout-plnal,
                                'BDC_OKCODE' '=VOUE'.

    PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
    PERFORM bdc_field  USING 'BDC_OKCODE'   '=MAAL'.

    PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
    PERFORM bdc_field  USING : 'BDC_CURSOR'   'PLPOD-VORNR(01)'.
    PERFORM bdc_field  USING 'BDC_OKCODE'   '=PICK'.

  ENDIF.
ENDFORM.                    " MAKE_BDC_ITAB
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3631   text
*      -->P_3632   text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING p_program p_dynpro.

  CLEAR wa_bdcdata.

  wa_bdcdata-program = p_program.
  wa_bdcdata-dynpro = p_dynpro.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO it_bdcdata.
ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3636   text
*      -->P_IT_BDC_ROUT_MATNR  text
*----------------------------------------------------------------------*
FORM bdc_field USING    p_fnam
                        p_fval.
  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = p_fnam.
  wa_bdcdata-fval = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " bdc_field
*&---------------------------------------------------------------------*
*&      Form  make_bdc_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_CNT  text
*      -->P_L_MONTH_CNT  text
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM make_bdc_detail USING    $it_display STRUCTURE it_display
                              p_cnt
                              p_month_cnt
                              p_type
                              p_aufak.

  DATA : l_field1(20),
         l_field2(20),
         l_field3(20).

  DATA : l_vgw01(15), " like plpo-vgw01,
         l_vgw02(15), " like plpo-vgw02,
         l_vgw03(15). " like plpo-vgw03

  FIELD-SYMBOLS: <f_field1>,
                 <f_field2>,
                 <f_field3>.

* Rate routing
  IF it_bdc_rout-bdc_type  = 'R'.
    g_screen = '5400'.
* Production routing
  ELSE.
    g_screen = '1400'.
  ENDIF.

  it_bdc_rout-vge03 = $it_display-vge03.
  it_bdc_rout-abp001 = $it_display-abp001.

  CONCATENATE 'IT_BDC_ROUT-SET' p_month_cnt INTO l_field1.
  ASSIGN  (l_field1)    TO   <f_field1> .
  CONCATENATE 'IT_BDC_ROUT-MCH' p_month_cnt INTO l_field2.
  ASSIGN  (l_field2)    TO   <f_field2> .
  CONCATENATE 'IT_BDC_ROUT-ABP' p_month_cnt INTO l_field3.
  ASSIGN  (l_field3)    TO   <f_field3> .

  l_vgw01 =   <f_field1> .
  l_vgw02 =   <f_field2> .
  l_vgw03 =   <f_field3> .

  CONDENSE : l_vgw01,l_vgw02,l_vgw03.

* Unit conversion
  IF it_bdc_rout-vge01 = 'STD'.
    it_bdc_rout-vge01 = 'HR'.
  ENDIF.

  IF it_bdc_rout-vge02 = 'STD'.
    it_bdc_rout-vge02 = 'HR'.
  ENDIF.

  IF it_bdc_rout-vge03 = 'STD'.
    it_bdc_rout-vge03 = 'HR'.
  ENDIF.


* Create routing
  IF p_type = 'CREATE'.

    IF l_vgw02 EQ 0 AND l_vgw03 EQ 0.
      EXIT.
    ENDIF.

    IF p_cnt = 1.
      PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
      PERFORM bdc_field   USING : 'PLPOD-ARBPL(01)' it_bdc_rout-kostl,
                                  'BDC_OKCODE'      '/00'.
      PERFORM bdc_dynpro USING    'SAPLCPDI' g_screen.
      PERFORM bdc_field  USING :  'RC27X-FLG_SEL(01)'  'X',
                                  'BDC_OKCODE'         '=VOD1'.
    ELSE.
      PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
      PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
      PERFORM bdc_field   USING : 'PLPOD-ARBPL(02)' it_bdc_rout-kostl,
                                  'BDC_OKCODE'      '/00'.
      PERFORM bdc_dynpro USING    'SAPLCPDI' g_screen.
      PERFORM bdc_field  USING :  'RC27X-FLG_SEL(01)'  ' ',
                                  'RC27X-FLG_SEL(02)'  'X',
                                  'BDC_OKCODE'         '=VOD1'.
    ENDIF.

*   Routing Operation detail
    PERFORM bdc_dynpro USING 'SAPLCPDO' '1200'.
    PERFORM bdc_field  USING : 'PLPOD-VGW01'   l_vgw01,
                               'PLPOD-VGW02'   l_vgw02,
                               'PLPOD-VGW03'   l_vgw03,
                               'PLPOD-VGE01'   it_bdc_rout-vge01,
                               'PLPOD-VGE02'   it_bdc_rout-vge02,
                               'PLPOD-VGE03'   it_bdc_rout-vge03,
*{
                               'PLPOD-AUFAK'   p_aufak,
*}
                               'BDC_OKCODE'    '=BACK'.

    PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
    PERFORM bdc_field  USING : 'BDC_OKCODE'         '=P+'.

* change routing
  ELSE.
*   Routing Operation detail
    PERFORM bdc_dynpro USING 'SAPLCPDO' '1200'.
    PERFORM bdc_field  USING : 'PLPOD-VGW01'   l_vgw01,
                               'PLPOD-VGW02'   l_vgw02,
                               'PLPOD-VGW03'   l_vgw03,
                               'PLPOD-VGE01'   it_bdc_rout-vge01,
                               'PLPOD-VGE02'   it_bdc_rout-vge02,
                               'PLPOD-VGE03'   it_bdc_rout-vge03,
*{
                               'PLPOD-AUFAK'   p_aufak,
*}
                               'BDC_OKCODE'    '=OD+'.
  ENDIF.



ENDFORM.                    " make_bdc_detail
*&---------------------------------------------------------------------*
*&      Form  get_detail_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_detail_msg.
  DATA: w_numb LIKE bapiret2-number,
        w_msgv1 LIKE bapiret2-message_v1,
        w_msgv2 LIKE bapiret2-message_v2,
        w_msgv3 LIKE bapiret2-message_v3,
        w_msgv4 LIKE bapiret2-message_v4,
        w_messa LIKE bapiret2-message.

  LOOP AT it_msg INTO wa_msg.
    w_numb = wa_msg-msgnr.
    w_msgv1 = wa_msg-msgv1.
    w_msgv2 = wa_msg-msgv2.
    w_msgv3 = wa_msg-msgv3.
    w_msgv4 = wa_msg-msgv4.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = wa_msg-msgid
        number     = w_numb
*       LANGUAGE   = SY-LANGU
        textformat = 'ASC'
*   LINKPATTERN       =
        message_v1 = w_msgv1
        message_v2 = w_msgv2
   message_v3      = w_msgv3
   message_v4      = w_msgv4
     IMPORTING
       message     = w_messa
*   RETURN            =
* TABLES
*   TEXT              =
              .
    wa_result-matnr = it_rout-matnr.
    wa_result-messa = w_messa.
    APPEND wa_result TO it_result.
    CLEAR wa_result.
  ENDLOOP.

ENDFORM.                    " get_detail_msg
*&---------------------------------------------------------------------*
*&      Form  make_bdc_header_del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MONTH_CNT  text
*----------------------------------------------------------------------*
FORM make_bdc_header_del USING    p_month_cnt p_aennr.
  DATA : l_pre_month_cnt(3) TYPE n.

  IF it_bdc_rout-bdc_type  = 'R'.
    g_screen = '5200'.
  ELSE.
    g_screen = '1200'.
  ENDIF.

  PERFORM bdc_dynpro  USING   'SAPLCPDI' '1010'.
  PERFORM bdc_field   USING : 'RC27M-MATNR' it_chg_rout-matnr ,
                              'RC27M-WERKS' it_chg_rout-werks,
                              'RC271-AENNR' p_aennr,
*                             'RC271-PLNNR' IT_CHG_ROUT-PLNNR,
                              'RC271-PLNAL' it_chg_rout-plnal,
                              'BDC_OKCODE' '=ALUE'.

  PERFORM bdc_dynpro  USING   'SAPLCPDI' g_screen .
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=LOE'.

  PERFORM bdc_dynpro  USING   'SAPLSPO1' '0100'.
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=YES'.

  PERFORM bdc_dynpro  USING   'SAPLCPDI' g_screen .
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=BACK'.

  PERFORM bdc_dynpro  USING   'SAPLSPO1' '0100'.
  PERFORM bdc_field   USING :
                              'BDC_OKCODE' '=YES'.

ENDFORM.                    " make_bdc_header_del
*&---------------------------------------------------------------------*
*&      Form  create_prd_route
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_prd_route.
  DATA  $it_display LIKE it_display OCCURS 0 WITH HEADER LINE.

  PERFORM get_selected_row_200 TABLES $it_display.

  PERFORM call_bdc_for_routing TABLES $it_display.
  PERFORM save_ztco_routing.

ENDFORM.                    " create_prd_route
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET TITLEBAR '200'.

*   Exclude toolbar
  PERFORM exclude_functions_200.
  __cls ftab.
*  FTAB-FCODE = 'SWITCH'.
*  APPEND FTAB. CLEAR FTAB.
  ftab-fcode = 'SAVE'.
  APPEND ftab. CLEAR ftab.
  ftab-fcode = 'RELS'.
  APPEND ftab. CLEAR ftab.
  ftab-fcode = 'CREL'.
  APPEND ftab. CLEAR ftab.
  SET PF-STATUS '100' EXCLUDING ftab.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv_200.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = it_display[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  __focus g_grid.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_OUTTAB  text
*      <--P_=  text
*      <--P_GT_OUT[]  text
*      <--P_IT_FIELDCATALOG  text
*      <--P_=  text
*      <--P_GT_FCAT[]  text
*      <--P_IT_SORT  text
*      <--P_=  text
*      <--P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM create_and_init_alv_200.

*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_route.
  SET HANDLER : g_event_route->handle_data_changed FOR g_grid.

*   Create field category
  PERFORM create_field_category_200 USING false.

  PERFORM sort_build_200 USING gt_sort[].

  CLEAR gs_layo.

  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
                   EXPORTING i_ready_for_input = 0.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_field_category_200 USING mode_edit.
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
    'X'  'WERKS'    'Plant'             4  'CHAR' '' '',
    'X'  'MATNR'    'Product'          18  'CHAR' '' '',
    'X'  'KOSTL'    'Cost.C'           10  'CHAR' '' '',
    ' '  'ABPTOT'   'Total   '         15  'QUAN' '' '',
    ' '  'ABP001'   '01      '         15  'QUAN' '' 'X',
    ' '  'ABP002'   '02      '         15  'QUAN' '' '',
    ' '  'ABP003'   '03      '         15  'QUAN' '' '',
    ' '  'ABP004'   '04      '         15  'QUAN' '' '',
    ' '  'ABP005'   '05      '         15  'QUAN' '' '',
    ' '  'ABP006'   '06      '         15  'QUAN' '' '',
    ' '  'ABP007'   '07      '         15  'QUAN' '' '',
    ' '  'ABP008'   '08      '         15  'QUAN' '' '',
    ' '  'ABP009'   '09      '         15  'QUAN' '' '',
    ' '  'ABP010'   '10      '         15  'QUAN' '' '',
    ' '  'ABP011'   '11      '         15  'QUAN' '' '',
    ' '  'ABP012'   '12      '         15  'QUAN' '' '',
    ' '  'AUFAK'    'Scrap   '          7  'DEC'  '' 'X'.
  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'ABPTOT' OR 'ABP001' OR 'ABP002' OR' ABP003' OR 'ABP004'
        OR 'ABP005' OR 'ABP006' OR 'ABP007' OR 'ABP008' OR 'ABP009'
        OR 'ABP010' OR 'ABP011' OR 'ABP012'.
        gs_fcat-just = 'R'.
*        GS_FCAT-NO_ZERO = 'X'.
*        GS_FCAT-QFIELDNAME = 'VGE03'.
        gs_fcat-ref_table = 'ZTCO_ROUTING'.
        gs_fcat-ref_field = gs_fcat-fieldname.
        MODIFY gt_fcat FROM gs_fcat.
      WHEN 'AUFAK'.
        gs_fcat-just = 'R'.
*        GS_FCAT-REF_TABLE = 'ZTCO_ROUTING'.
*        GS_FCAT-REF_FIELD = GS_FCAT-FIELDNAME.
        MODIFY gt_fcat FROM gs_fcat.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY_200
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build_200 USING ft_sort TYPE lvc_t_sort.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
             'WERKS'    '1'  'X' '' 'X' '',
             'MATNR'    '2'  'X' '' 'X' '',
             'KOSTL'    '3'  'X' '' 'X' ''.

ENDFORM.                    " SORT_BUILD_200
*&---------------------------------------------------------------------*
*&      Form  save_ztco_routing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_ztco_routing.
  DATA : BEGIN OF it_routing OCCURS 0,
         matnr  LIKE ztco_routing-matnr,
         END OF it_routing.
  DATA : it_ztco_rout LIKE ztco_routing OCCURS 0 WITH HEADER LINE.

  DATA : l_title(50),
         l_text1(50),
         l_text2(50),
         l_answer .

  SELECT matnr
   INTO CORRESPONDING FIELDS OF TABLE it_routing
   FROM ztco_routing
    FOR ALL ENTRIES IN it_display
  WHERE bdatj   = p_bdatj
    AND versn   = p_versn
    AND werks   = it_display-werks
    AND matnr   = it_display-matnr
    AND kostl   = it_display-kostl .


  IF NOT it_routing[] IS INITIAL.
    l_title  = 'Warning TABLE : ZTCO_ROUTING' .
    l_text1  = 'The key data will be overwritten in Table' .
    l_text2  = 'Do you want to save? '.
  ELSE.
    l_title  = 'TABLE : ZTCO_ROUTING' .
    l_text1  = 'Do you want to save?' .
    l_text2  = 'The new routing data'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = l_text1
            textline2      = l_text2
            titel          = l_title
            cancel_display = ''
       IMPORTING
            answer         = l_answer.

  CHECK l_answer = 'J'.

  DATA abpxxx LIKE cosl-kap001.
  DATA $ix LIKE sy-tabix.

  LOOP AT it_display.
    $ix = sy-tabix.
    IF it_display-type = 'P'.
    ELSE.
      DO 12 TIMES VARYING abpxxx FROM it_display-abp001 NEXT
                                      it_display-abp002.
        abpxxx = it_display-abp001.
      ENDDO.
      it_display-abptot  = it_display-abp001.
    ENDIF.
    MODIFY it_display INDEX $ix.
  ENDLOOP.

  LOOP AT it_display.
    MOVE-CORRESPONDING it_display TO it_ztco_rout.

    IF it_ztco_rout-type = 'P'.
    ELSE.
      DO 12 TIMES VARYING abpxxx FROM it_ztco_rout-abp001 NEXT
                                      it_ztco_rout-abp002.
        abpxxx = it_display-abp001.
      ENDDO.
      it_ztco_rout-abptot  = it_display-abp001.
    ENDIF.

    it_ztco_rout-kokrs   = p_kokrs.
    it_ztco_rout-bdatj   = p_bdatj.
    it_ztco_rout-versn   = p_versn.
    it_ztco_rout-erdat   = sy-datum.
    it_ztco_rout-erzet   = sy-uzeit.
    it_ztco_rout-ernam   = sy-uname.
    APPEND it_ztco_rout. CLEAR it_ztco_rout.
  ENDLOOP.

  MODIFY ztco_routing FROM TABLE it_ztco_rout.

  IF sy-subrc = 0 .
    COMMIT WORK.
    MESSAGE s000 WITH 'Data has been saved sucessfully'.
  ENDIF.

ENDFORM.                    " save_ztco_routing
*&---------------------------------------------------------------------*
*&      Form  CREATE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_rate.
  PERFORM :
            initialize            ,
            validate              .

  PERFORM :  get_itab,
             calculate_abp_mh.

  PERFORM   move_out.

ENDFORM.                    " CREATE_RATE
*&---------------------------------------------------------------------*
*&      Form  apply_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_icon.
  DATA $ix LIKE sy-tabix.

  LOOP AT gt_out.
    $ix = sy-tabix.
    IF gt_out-released EQ true.
      gt_out-icon = icon_release.
    ELSE.
      gt_out-icon = icon_led_inactive.
      IF NOT gt_out-nor_r IS INITIAL.
        gt_out-icon = icon_rating_positive.
      ENDIF.
    ENDIF.
    IF gt_out-confirmed EQ 'X'.
      gt_out-icon_c = icon_checked.
    ENDIF.
    MODIFY gt_out INDEX $ix TRANSPORTING icon icon_c.
  ENDLOOP.

ENDFORM.                    " apply_icon
*&---------------------------------------------------------------------*
*&      Form  RELEASE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM release_rate.

* by ig.moon {
*  IF P_BDATJ <= SY-DATUM(4).
*    EXIT.
*  ENDIF.
* }

  DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

* Save seleted data to table ZTCOU128
  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.

  DATA  : i_ztcou128 LIKE ztcou128 OCCURS 0 WITH HEADER LINE,
          ls_ztcou128 LIKE ztcou128.

  LOOP AT $gt_out.
* by ig.moon 10/18/2012 {
*    CHECK NOT $gt_out-nor_r IS INITIAL.
* }
    CHECK $gt_out-released NE true.
    MOVE-CORRESPONDING $gt_out TO *ztcou128.
     *ztcou128-aedat = sy-datum.
     *ztcou128-aenam = sy-uname.
    i_ztcou128 = *ztcou128.
    APPEND i_ztcou128.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.

  READ TABLE i_ztcou128 INDEX 1.
  CHECK sy-subrc EQ 0.

  DATA $subrc(1).

  PERFORM relese_tc31a TABLES i_ztcou128
                       CHANGING $subrc.

  IF $subrc EQ 'E'.
    ROLLBACK WORK.
  ELSE.
    i_ztcou128-released = true.
    MODIFY i_ztcou128 TRANSPORTING released WHERE released NE true .
    MODIFY ztcou128 FROM TABLE i_ztcou128.
    COMMIT WORK.
  ENDIF.

  CONCATENATE 'Data has been released;'
               lv_cnt  'record(s).'
          INTO lv_msg SEPARATED BY space.

  IF lv_cnt > 0.
    PERFORM update_selected_rows USING true.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  CLEAR flag_data_changed.
  PERFORM apply_icon.
ENDFORM.                    " RELEASE_RATE
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows_200 TABLES $it_display STRUCTURE it_display.

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

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    $it_display[] = it_display[].
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE it_display INDEX lt_rows-index.
      it_display-chk = true .
      MODIFY it_display INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT it_display.
      CHECK it_display-chk EQ true.
      $it_display = it_display.
      APPEND $it_display.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " get_selected_rows

*---------------------------------------------------------------------*
*       FORM GET_SELECTED_ROWS                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  $GT_OUT                                                       *
*---------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.
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

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    $gt_out[] = gt_out[].
    gt_out-chk = true .
    MODIFY gt_out TRANSPORTING chk WHERE chk EQ false.
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-chk = true .
      MODIFY gt_out INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT gt_out.
      CHECK gt_out-chk EQ true.
      $gt_out = gt_out.
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " get_selected_rows

*&---------------------------------------------------------------------*
*&      Form  RELESE_TC31A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_ZTCOU128  text
*      <--P_$SUBRC  text
*----------------------------------------------------------------------*
FORM relese_tc31a TABLES   p_ztcou128 STRUCTURE ztcou128
                  CHANGING p_$subrc.

  DATA : $zgkal TYPE dzeitgradk,
         $zgter TYPE dzeitgrad.

  LOOP AT p_ztcou128.
    CLEAR *tc31a.
    SELECT SINGLE * INTO *tc31a
      FROM tc31a WHERE zgrad EQ p_ztcou128-abtei
                  AND datub EQ p_ztcou128-datub.

    IF sy-subrc EQ 0.
      $zgkal = *tc31a-zgkal.
      $zgter = *tc31a-zgter.
       *tc31a-zgkal = $zgter.
       *tc31a-zgter = $zgkal.
      UPDATE tc31a FROM *tc31a.
    ENDIF.

*    IF SY-SUBRC NE 0.
*      P_$SUBRC = 'E'.
*      EXIT.
*    ENDIF.

  ENDLOOP.


ENDFORM.                    " RELESE_TC31A

*---------------------------------------------------------------------*
*       FORM UNRELESE_TC31A                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ZTCOU128                                                    *
*  -->  P_$SUBRC                                                      *
*---------------------------------------------------------------------*
FORM unrelese_tc31a TABLES   p_ztcou128 STRUCTURE gt_out
                  CHANGING p_$subrc.

  DATA $zgter TYPE dzeitgrad.

  LOOP AT p_ztcou128.
    CLEAR *tc31a.
    SELECT SINGLE * INTO *tc31a
      FROM tc31a WHERE zgrad EQ p_ztcou128-abtei
                  AND datub EQ p_ztcou128-datub.

    IF sy-subrc EQ 0.
      $zgter = *tc31a-zgter.
       *tc31a-zgkal = $zgter.
       *tc31a-zgter = p_ztcou128-nor_r.
      UPDATE tc31a FROM *tc31a.
    ENDIF.

*    IF SY-SUBRC NE 0.
*      P_$SUBRC = 'E'.
*      EXIT.
*    ENDIF.

  ENDLOOP.


ENDFORM.                    " RELESE_TC31A

*&---------------------------------------------------------------------*
*&      Form  UN_RELEASE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM un_release_rate.

* by ig.moon {
*  IF P_BDATJ <= SY-DATUM(4).
*    EXIT.
*  ENDIF.
* }

  DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

* Save seleted data to table ZTCOU128
  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.

  DATA  i_ztcou128 LIKE ztcou128 OCCURS 0 WITH HEADER LINE.

  LOOP AT $gt_out.
* by ig.moon 10/18/2012 {
*    CHECK NOT $gt_out-nor_r IS INITIAL.
* }
    CHECK NOT $gt_out-released NE true.
    MOVE-CORRESPONDING $gt_out TO *ztcou128.
     *ztcou128-aedat = sy-datum.
     *ztcou128-aenam = sy-uname.
    i_ztcou128 = *ztcou128.
    APPEND i_ztcou128.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.

  READ TABLE i_ztcou128 INDEX 1.
  CHECK sy-subrc EQ 0.

  DATA $subrc(1).

  PERFORM unrelese_tc31a TABLES $gt_out
                       CHANGING $subrc.

  IF $subrc EQ 'E'.
    ROLLBACK WORK.
  ELSE.
    CLEAR i_ztcou128-released.
    MODIFY i_ztcou128 TRANSPORTING released WHERE released EQ true.
    MODIFY ztcou128 FROM TABLE i_ztcou128.
    COMMIT WORK.
  ENDIF.

  CONCATENATE 'Data has been unreleased;'
               lv_cnt  'record(s).'
          INTO lv_msg SEPARATED BY space.

  IF lv_cnt > 0.
    PERFORM update_selected_rows USING false.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  CLEAR flag_data_changed.
  PERFORM apply_icon.

ENDFORM.                    " UN_RELEASE_RATE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_selected_rows USING p_flag.

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

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    gt_out-released = p_flag.
    MODIFY gt_out TRANSPORTING released WHERE NOT nor_r IS initial .
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-released = p_flag.
      MODIFY gt_out INDEX lt_rows-index.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPDATE_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr.
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF ls_celltab-fieldname = 'CC_RATD'
                        OR ls_celltab-fieldname =  'NOR_R'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  CLEAR gt_out-celltab.
  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE celltab IS initial.
  PERFORM build_cell_attr1_lock.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
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
  MODIFY gt_out TRANSPORTING celltab WHERE released EQ true.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat1-fieldname.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE released EQ true.


ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_container.

  IF NOT g_event_receiver IS INITIAL.
    FREE g_event_receiver.
  ENDIF.
  IF NOT g_event_route IS INITIAL.
    FREE g_event_route.
  ENDIF.

  IF NOT g_grid IS INITIAL.
    CALL METHOD g_grid->free.
  ENDIF.

  IF NOT g_custom_container IS INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.

  FREE : g_grid,g_custom_container.

  CLEAR :  gs_layo,gt_exclude,gt_out[],gt_fcat[],gt_sort[].

ENDFORM.                    " FREE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  view_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_result.

  PERFORM view_.
  CHECK g_error EQ space .
  PERFORM info_text_set USING false.

*  DATA $FLAG(1).
*  LOOP AT GT_OUT.
*    AT NEW KOSTL.
*      $FLAG = TRUE.
*    ENDAT.
*    CHECK $FLAG EQ TRUE.
*    CLEAR $FLAG.
*    PERFORM CHK_NOR_R.
*    MODIFY GT_OUT TRANSPORTING NOR_R
*              WHERE KOSTL EQ GT_OUT-KOSTL.
*  ENDLOOP.

  PERFORM set_output .

ENDFORM.                    " view_result
*&---------------------------------------------------------------------*
*&      Form  new_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_record.

  DATA l_answer(1).

  l_answer = 'J'.

  SELECT SINGLE *
    FROM ztcou128 WHERE kokrs EQ p_kokrs
                    AND bdatj EQ p_bdatj.

  IF sy-subrc EQ 0.
    PERFORM pop_up USING
        'The data already exists! '
        '(Yes) Gather new records / (No) will load the saved data.'
        'X'
                   CHANGING l_answer.
  ENDIF.
  IF l_answer EQ 'J'.
    PERFORM create_rate.
    CHECK g_error EQ space .
    PERFORM info_text_set USING false.
    PERFORM chk_existing.
    PERFORM set_output .
  ELSEIF l_answer EQ 'A'.
    g_error = true.
    EXIT.
  ELSE.
    PERFORM view_result.
  ENDIF.

ENDFORM.                    " new_record
*&---------------------------------------------------------------------*
*&      Form  chk_existing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_existing.
  DATA $flag(1).
  LOOP AT gt_out.
    AT NEW kostl.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    PERFORM chk_released.
    PERFORM chk_nor_r.
    MODIFY gt_out TRANSPORTING released confirmed nor_r
              WHERE kostl EQ gt_out-kostl.
  ENDLOOP.

ENDFORM.                    " chk_existing
*&---------------------------------------------------------------------*
*&      Form  CHK_RELEASED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_released.

  CLEAR *ztcou128.
  SELECT SINGLE * INTO *ztcou128
    FROM ztcou128 WHERE kokrs EQ p_kokrs
                    AND bdatj EQ p_bdatj
                    AND kostl EQ gt_out-kostl
                    AND abtei EQ gt_out-abtei
                    AND perio EQ gt_out-perio.
*                    AND RELEASED EQ TRUE.
  IF sy-subrc EQ 0.
    IF *ztcou128-released EQ true.
      gt_out-released = true.
    ENDIF.
    IF *ztcou128-confirmed EQ true.
      gt_out-confirmed = true.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHK_RELEASED
*&---------------------------------------------------------------------*
*&      Form  CHK_NOR_R
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_nor_r.
  SELECT SINGLE * INTO *ztcou129
    FROM ztcou129 WHERE kokrs EQ p_kokrs
                    AND plnyr EQ p_bdatj
                    AND kostl EQ gt_out-kostl.
  IF sy-subrc EQ 0.
    gt_out-nor_r = *ztcou129-nor_r.
  ENDIF.
ENDFORM.                    " CHK_NOR_R
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SET PF-STATUS 'ZLOG'.
  sy-title = 'Error log...'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM error_list.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  error_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_list.

  READ TABLE it_plaf WITH KEY $flag = space.
  IF sy-subrc EQ 0.
    WRITE:/ 'Could not find the routing information!' COLOR = 3 .
  ELSE.
    WRITE:/ 'No error log was found!' COLOR = 4 .
  ENDIF.

  DATA $type(20).

  LOOP AT it_plaf WHERE $flag IS initial.
    IF it_plaf-sauft EQ 'X'.
      $type = 'Rate routing'.
    ELSE.
      $type = 'Product routing'.
    ENDIF.
    WRITE:/
            it_plaf-perio,
            it_plaf-plwrk,
            it_plaf-matnr,
            $type .
  ENDLOOP.

ENDFORM.                    " error_list
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode_200.

  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 1.
  ELSE.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
  ENDIF.

  SET PF-STATUS '100'.

ENDFORM.                    " SWITCH_EDIT_MODE_200
*&---------------------------------------------------------------------*
*&      Form  get_selected_row_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_row_200 TABLES $it_display STRUCTURE it_display.

  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

* Save seleted data to table ZTCOU128
  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows_200 TABLES $it_display.

ENDFORM.                    " get_selected_row_200
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM data_changed_200 USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  flag_data_changed = true.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.
  DATA  $werks TYPE werks_d.
  DATA  $matnr TYPE matnr.

*  DATA: l_del_row TYPE lvc_s_moce.
**          ls_tab TYPE ztcou128,
**          ls_outtab LIKE LINE OF gt_out.
*
*  LOOP AT rr_data_changed->mt_deleted_rows INTO l_del_row.
*
**    READ TABLE gt_display INDEX l_del_row-row_id.
**    break-point.
*    READ TABLE it_display INDEX l_del_row-row_id.
*
*    IF sy-subrc EQ 0.
*      it_display-del = true.
*      MODIFY it_display INDEX l_del_row-row_id  TRANSPORTING del.
*    ENDIF.
*  ENDLOOP.
*
**  DELETE  it_display  WHERE del EQ true.
*
  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    READ TABLE it_display INDEX ls_mod_cells-row_id.
    IF sy-subrc = 0.
      $werks = it_display-werks.
      $matnr = it_display-matnr.

      CALL METHOD rr_data_changed->modify_cell
                EXPORTING i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
      IF ls_mod_cells-fieldname EQ 'ABP001'.
        it_bdc_rout-abp001 = ls_mod_cells-value.
        MODIFY it_bdc_rout TRANSPORTING abp001
                                         WHERE werks EQ $werks
                                           AND matnr EQ $matnr.
      ELSE.
        it_bdc_rout-aufak = ls_mod_cells-value.
        MODIFY it_bdc_rout TRANSPORTING aufak
                                         WHERE werks EQ $werks
                                           AND matnr EQ $matnr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  MAKE_BDC_SCRAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MONTH_CNT  text
*      <--P_L_TYPE  text
*      <--P_L_AENNR  text
*      <--P_IT_BDC_ROUT_AUFAK  text
*----------------------------------------------------------------------*
FORM make_bdc_scrap  USING p_month_cnt
                     CHANGING
                            p_type
                            p_aennr
                            p_aufak.

  DATA : l_datuv(8).

  p_type = 'CHANGE'.

  IF it_bdc_rout-bdc_type  = 'R'.
    g_screen = '5400'.
    g_code = 'CA22'.
  ELSE.
    g_screen = '1400'.
    g_code = 'CA02'.
  ENDIF.

  CONCATENATE it_chg_rout-datuv+4(2) it_chg_rout-datuv+6(2)
              it_chg_rout-datuv(4) INTO l_datuv.

  PERFORM bdc_dynpro  USING   'SAPLCPDI' '1010'.
  PERFORM bdc_field   USING : 'RC27M-MATNR' it_bdc_rout-matnr ,
                              'RC27M-WERKS' it_bdc_rout-werks,
                              'RC271-AENNR' p_aennr,
                              'RC271-STTAG' l_datuv,
                              'RC271-PLNAL' it_chg_rout-plnal,
                              'BDC_OKCODE' '=VOUE'.

  PERFORM bdc_dynpro USING 'SAPLCPDI' g_screen.
  PERFORM bdc_field  USING 'PLPOD-AUFAK(01)' p_aufak.
  PERFORM bdc_field  USING 'BDC_OKCODE'   '=BU'.

ENDFORM.                    " MAKE_BDC_ITAB
*&---------------------------------------------------------------------*
*&      Form  RESET_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_cc.
* by ig.moon {
*  IF P_BDATJ <= SY-DATUM(4).
*    EXIT.
*  ENDIF.
* }

  DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message
* Save seleted data to table ZTCOU128
  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.

  gt_out-cc_ratd = '100'.
  MODIFY gt_out TRANSPORTING cc_ratd WHERE chk EQ true
                                       AND released EQ false.

  READ TABLE $gt_out WITH KEY released = true.
  IF sy-subrc EQ 0.
    MESSAGE i000 WITH 'You can not apply with the released line(s).'
                      'Applying the recored(s) will be ignored!'.
  ENDIF.

ENDFORM.                    " RESET_CC
*&---------------------------------------------------------------------*
*&      Form  FILL_TO_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PLPO  text
*----------------------------------------------------------------------*
FORM fill_to_itab TABLES   lt_plpo STRUCTURE it_plpo.

  LOOP AT lt_plpo.
*      IF LT_PLPO-MATNR NE IT_PLAF-MATNR .
*        EXIT.
*      ENDIF.
    itab-werks   = it_plaf-plwrk.
    itab-matnr   = lt_plpo-matnr.
    itab-perio   = it_plaf-perio.
    itab-type    = lt_plpo-type.
    itab-vgw01   = lt_plpo-vgw01.    "Set
    itab-vgw02   = lt_plpo-vgw02.    "Machine

    itab-vge01   = lt_plpo-vge01.    "unit Set
    itab-vge02   = lt_plpo-vge02.    "unit Machine
    itab-vge03   = lt_plpo-vge03.    "unit MH
    itab-aufak   = lt_plpo-aufak.


    CLEAR it_crhd .
    READ TABLE it_crhd WITH KEY objid = lt_plpo-arbid BINARY SEARCH.
    itab-kostl   = it_crhd-arbpl.
*    CLEAR TMPT.
*    READ TABLE TMPT WITH KEY ARBPL = LT_PLPO-ARBID
*                             PERIO = IT_PLAF-PERIO
*                             BINARY SEARCH.
*    CHECK SY-SUBRC = 0 .
*    ITAB-KOSTL = TMPT-KOSTL.

    itab-abp_mh = lt_plpo-vgw03.
    itab-mtart = it_plaf-mtart.
    CHECK NOT itab-abp_mh IS INITIAL.
    COLLECT itab. CLEAR itab.
  ENDLOOP.

ENDFORM.                    " FILL_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  select_routing_data
*&---------------------------------------------------------------------*
*FORM select_routing_data USING    F_PLNTY
*                                  F_VERWE
*                         TABLES   FT_MAT  STRUCTURE IT_RATE
*                                  FT_PLPO STRUCTURE DB_PLPO.
*
*    SELECT
*          D~WERKS
*          A~MATNR
*          D~PLNTY
*          D~PLNNR
*          D~PLNKN
*          D~ARBID
*          D~ZAEHL
*          D~DATUV
*
*          D~VGW01
*          D~VGW02
*          D~VGW03
*
*          D~VGE01
*          D~VGE02
*          D~VGE03
*          D~AENNR
*          C~PLNAL
*          D~AUFAK
*
*     INTO CORRESPONDING FIELDS OF TABLE FT_PLPO
*       FROM MAPL AS A
*      INNER JOIN PLKO AS B
*        ON  A~PLNTY = B~PLNTY
*       AND  A~PLNNR = B~PLNNR
*       AND  A~PLNAL = B~PLNAL
*      INNER JOIN PLAS AS C
*         ON B~PLNTY = C~PLNTY
*        AND B~PLNNR = C~PLNNR
*       AND  B~PLNAL = C~PLNAL
*      INNER JOIN PLPO AS D
*         ON C~PLNTY = D~PLNTY
*        AND C~PLNNR = D~PLNNR
*        AND C~PLNKN = D~PLNKN
*      FOR ALL ENTRIES IN FT_MAT
*      WHERE A~PLNTY = F_PLNTY         "R:Rate routing N:Product
*        AND A~MATNR = FT_MAT-MATNR
*        AND A~LOEKZ = ''
*        AND B~VERWE = F_VERWE          "Usage
*        AND B~DATUV <= G_STARTDT      "Valid from
*
*        AND B~LOEKZ = ''              "Delete indicator
*        AND C~LOEKZ = ''              "Delete indicator
*        AND D~LOEKZ = ''.             "Delete indicator
*
*ENDFORM.                    " select_routing_data
*&---------------------------------------------------------------------*
*&      Form  make_itemp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PLPO_TEMP2  text
*----------------------------------------------------------------------*
FORM make_itemp TABLES   p_it_plpo STRUCTURE db_plpo.
  DATA $ix LIKE sy-tabix.
  DATA $itab LIKE db_plpo OCCURS 0 WITH HEADER LINE.

  LOOP AT p_it_plpo.
    $ix = sy-tabix.
    IF p_it_plpo-loekz EQ 'X'.
      $itab = p_it_plpo.
      APPEND $itab.CLEAR $itab.
      DELETE p_it_plpo INDEX $ix.
    ENDIF.
  ENDLOOP.

  SORT $itab  BY werks matnr plnty  plnnr plnkn .

  LOOP AT p_it_plpo.
    $ix = sy-tabix.
    READ TABLE $itab WITH KEY werks = p_it_plpo-werks
                              matnr = p_it_plpo-matnr
                              plnty = p_it_plpo-plnty
                              plnnr = p_it_plpo-plnnr
                              plnkn = p_it_plpo-plnkn
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      p_it_plpo-datub = $itab-datuv - 1.
    ELSE.
      p_it_plpo-datub = '99991231'.
    ENDIF.
    MODIFY p_it_plpo INDEX $ix TRANSPORTING datub.
  ENDLOOP.

ENDFORM.                    " make_itemp
*&---------------------------------------------------------------------*
*&      Form  dele_prd_route
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dele_prd_route.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  IF lt_rows[] IS INITIAL.
    MESSAGE s000 WITH 'Please select at least one line'.
    EXIT.
  ENDIF.

  DATA  $it_display LIKE it_display OCCURS 0 WITH HEADER LINE.

  PERFORM get_selected_row_200 TABLES $it_display.

  LOOP AT $it_display.
    DELETE it_display WHERE werks = $it_display-werks
                        AND type = $it_display-type
                        AND matnr = $it_display-matnr
                        AND kostl =  $it_display-kostl.
  ENDLOOP.

ENDFORM.                    " dele_prd_route
*&---------------------------------------------------------------------*
*&      Form  only_save_to_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM only_save_to_ztable.

  DATA  $it_display LIKE it_display OCCURS 0 WITH HEADER LINE.

  PERFORM get_selected_row_200 TABLES $it_display.

  PERFORM save_ztco_routing.

ENDFORM.                    " only_save_to_ztable
