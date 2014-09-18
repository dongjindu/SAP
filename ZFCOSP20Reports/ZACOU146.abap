*----------------------------------------------------------------------
* Program ID        : ZACOU146
* Title             : [CO] Fill Shop Cost Data for NAFTA
* Created on        : 01/04/2010
* Created by        : I.G.MOON
* Specifications By : Michael Yoon
* Description       : Fill Shop Cost Data to Cost Estimate Table
*----------------------------------------------------------------------
REPORT zacou146 MESSAGE-ID zmco.
TABLES : ztco_shop_sum, ztco_ck11, *ztco_ck11, mbewh.

TYPES: BEGIN OF ty_ztco_shop_sum.
TYPES: fsc TYPE matnr.
        INCLUDE STRUCTURE ztco_shop_sum.
TYPES: indx TYPE zcoindx,
       reqqt TYPE menge_pos,
       upgvc TYPE matnr,
       pp_gr_qty TYPE ckml_outmenge,
       gpreis TYPE zgpreis,
       wertn TYPE ck_kwt,
       total TYPE zetotam,
       lifnr TYPE lifnr.
TYPES: END   OF  ty_ztco_shop_sum.

TYPES: BEGIN OF ty_ztco_ck11.
        INCLUDE STRUCTURE ztco_ck11.
TYPES: END   OF  ty_ztco_ck11.

DATA : it_ztco_shop_sum TYPE TABLE OF ty_ztco_shop_sum WITH HEADER LINE.
DATA : it_ztco_ck11 TYPE TABLE OF ty_ztco_ck11 WITH HEADER LINE.
DATA : itab TYPE TABLE OF ty_ztco_ck11 WITH HEADER LINE.


DATA : BEGIN OF it_ckmlmv003 OCCURS 0,
         bwkey      LIKE ckmlmv001-bwkey,
         matnr      LIKE ckmlmv001-matnr,
         aufnr      LIKE ckmlmv013-aufnr,
         verid_nd   LIKE ckmlmv001-verid_nd,
         meinh      LIKE ckmlmv003-meinh,
         out_menge  LIKE ckmlmv003-out_menge,
       END OF  it_ckmlmv003.

DATA : BEGIN OF it_engine OCCURS 0,
         matnr      LIKE ckmlmv001-matnr,
       END OF  it_engine.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.

PARAMETERS     : p_kokrs LIKE ztco_shop_sum-kokrs OBLIGATORY
                 MEMORY ID cac,
                 p_bdatj LIKE ztco_shop_sum-bdatj OBLIGATORY
                 MEMORY ID bdtj,
                 p_poper LIKE ztco_shop_sum-poper OBLIGATORY
                 MEMORY ID popr.

SELECT-OPTIONS   s_artnr FOR ztco_shop_sum-artnr NO INTERVALS.
SELECT-OPTIONS   s_matnr FOR ztco_shop_sum-llv_matnr NO INTERVALS.

PARAMETERS p_dele AS CHECKBOX.
PARAMETERS p_crea AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK bl1.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

****************************** global data *****************************
DATA $ix TYPE i.
DATA: gv_date_f TYPE sydatum,             " from date
      gv_info_f TYPE sydatum,             " from date (info)
      gv_date_t TYPE sydatum,             " to date
      gv_date3  TYPE sydatum.             " next end
DATA g_bukrs TYPE bukrs.

INITIALIZATION.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*
  IF p_dele IS INITIAL AND p_crea IS INITIAL.
    MESSAGE i000 WITH 'Please select the job option'.
    STOP.
  ENDIF.

  PERFORM get_date.

  __process 'Read Row Data...' '10'.

  IF p_dele EQ true.
    WRITE:/ 'Delete entries...' COLOR COL_HEADING.
    DELETE FROM ztco_ck11
    WHERE kokrs EQ p_kokrs
     AND klvar EQ 'ZUNF'
     AND bdatj EQ p_bdatj
     AND poper EQ p_poper
     AND artnr IN s_artnr
     AND compn IN s_matnr
     AND btyp EQ 'ZUNF'.

    IF sy-subrc EQ 0.
      WRITE:/ sy-dbcnt,
              ' entries was(were) deleted.'.
    ELSE.
      WRITE:/ ' No entry has been deleted.'.
    ENDIF.

    COMMIT WORK.
  ENDIF.

  CHECK p_crea EQ true.
  WRITE:/ 'Create entries...' COLOR COL_HEADING.

  PERFORM get_row_data.

  READ TABLE it_ztco_shop_sum INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No Shop Cost Data has been found.'.
    STOP.
  ENDIF.

  SORT it_ztco_shop_sum BY fsc llv_matnr.
  SORT it_ztco_ck11 BY kokrs klvar bdatj poper artnr verid werks compn.

  __process 'Read Prd. Qty...' '20'.
  PERFORM get_product_gr.

  __process 'Fill Price info...' '50'.

  READ TABLE it_ztco_shop_sum INDEX 1.
  clear: it_engine,it_engine[].
  IF sy-subrc EQ 0.
    SELECT matnr INTO TABLE it_engine
    FROM marc
    FOR ALL ENTRIES IN it_ztco_shop_sum
    WHERE matnr = it_ztco_shop_sum-llv_matnr
      AND FEVOR NE space.
  ENDIF.

  SORT it_engine.

  LOOP AT it_ztco_shop_sum.
    $ix = sy-tabix.

    PERFORM get_pp_grqty USING it_ztco_shop_sum-artnr
                               it_ztco_shop_sum-verid
                      CHANGING it_ztco_shop_sum-pp_gr_qty.
    IF it_ztco_shop_sum-pp_gr_qty IS INITIAL.
      DELETE it_ztco_shop_sum INDEX $ix.
      CONTINUE.
    ENDIF.

    IF NOT it_ztco_shop_sum-pp_gr_qty IS INITIAL.
      it_ztco_shop_sum-reqqt =
      it_ztco_shop_sum-manu_qty / it_ztco_shop_sum-pp_gr_qty.
    ENDIF.

    IF it_ztco_shop_sum-verpr IS INITIAL.
      PERFORM get_verpr CHANGING it_ztco_shop_sum-verpr.
    ENDIF.

    IF NOT it_ztco_shop_sum-manu_qty IS INITIAL.
      it_ztco_shop_sum-gpreis =
      it_ztco_shop_sum-manu_amt / it_ztco_shop_sum-manu_qty.
    ENDIF.

    it_ztco_shop_sum-wertn =
    it_ztco_shop_sum-gpreis * it_ztco_shop_sum-reqqt.
    it_ztco_shop_sum-total = it_ztco_shop_sum-wertn.

    READ TABLE it_engine WITH KEY matnr = it_ztco_shop_sum-llv_matnr
                         BINARY SEARCH.
    IF sy-subrc EQ 0.
    ELSE.
      PERFORM get_lifnr USING  it_ztco_shop_sum-llv_matnr
                        CHANGING it_ztco_shop_sum-lifnr.
    ENDIF.

    MODIFY it_ztco_shop_sum INDEX $ix
    TRANSPORTING pp_gr_qty reqqt verpr gpreis wertn total lifnr.

  ENDLOOP.

  __process 'Update Cost Estimate Table...' '80'.
  __cls itab.

  DATA $flag.
  DATA $idx(10) TYPE n.

  LOOP AT it_ztco_shop_sum.

    AT NEW fsc.
      $flag = true.
    ENDAT.

    IF $flag EQ true.
      CLEAR $flag.
      CLEAR $idx.
      SELECT MAX( indx ) INTO $idx FROM ztco_ck11
          WHERE kokrs EQ it_ztco_shop_sum-kokrs
            AND klvar EQ 'ZUNF'
            AND bdatj EQ it_ztco_shop_sum-bdatj
            AND poper EQ it_ztco_shop_sum-poper
            AND artnr EQ it_ztco_shop_sum-artnr
            GROUP by kokrs klvar bdatj poper artnr.
      ENDSELECT.
    ENDIF.

    READ TABLE it_ztco_ck11 WITH KEY kokrs = it_ztco_shop_sum-kokrs
                                     klvar = 'ZUNF'
                                     bdatj = it_ztco_shop_sum-bdatj
                                     poper = it_ztco_shop_sum-poper
                                     artnr = it_ztco_shop_sum-artnr
                                     verid = it_ztco_shop_sum-verid
                                     werks = it_ztco_shop_sum-bwkey
                                     compn = it_ztco_shop_sum-llv_matnr
                                     BINARY SEARCH.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    ADD 1 TO $idx.
    MOVE-CORRESPONDING it_ztco_shop_sum TO itab.
    itab-klvar = itab-btyp = 'ZUNF'.
    itab-compn = it_ztco_shop_sum-llv_matnr.
    itab-werks = it_ztco_shop_sum-par_werks.
    itab-hwaer = 'USD'.
    itab-meins = it_ztco_shop_sum-meeht.
    itab-indx  = $idx.
    SELECT SINGLE mtart matkl INTO (itab-mtart,itab-matkl) FROM mara
    WHERE matnr EQ itab-compn.
    IF sy-subrc EQ 0
    AND itab-matkl EQ 'MP'.
    ELSE.

      SELECT SINGLE land1 INTO itab-land1
      FROM lfa1 WHERE lifnr EQ itab-lifnr.

      SELECT SINGLE bklas INTO itab-bklas FROM mbew
      WHERE matnr EQ itab-compn
      AND bwkey EQ itab-werks.
      IF sy-subrc NE 0.

        SELECT * FROM mbewh
        WHERE matnr EQ itab-compn
        AND bwkey EQ itab-werks
        ORDER BY lfgja lfmon DESCENDING.
          itab-bklas = mbewh-bklas.
          EXIT.
        ENDSELECT.

      ENDIF.

      APPEND itab.
    ENDIF.
  ENDLOOP.

  DATA $cnt TYPE i.
  DATA $mod TYPE i.
  DATA $matcnt TYPE i.
  DATA g_matcnt TYPE i.

  SORT itab BY kokrs klvar bdatj poper artnr.

  LOOP AT itab.

    AT NEW artnr.
      WRITE:/ itab-artnr COLOR COL_POSITIVE.
      CLEAR $matcnt.
    ENDAT.

    CLEAR *ztco_ck11.
     *ztco_ck11 = itab.
    IF *ztco_ck11-verid IS INITIAL.
       *ztco_ck11-verid = '00'.
    ENDIF.

     *ztco_ck11-aedat = sy-datum.
     *ztco_ck11-aenam = sy-uname.
     *ztco_ck11-cputm = sy-uzeit.
     *ztco_ck11-losgr = 1.
    read table it_engine with key matnr = itab-COMPN binary search.
    if sy-subrc eq 0.
      *ztco_ck11-stkkz = 'X'.
    endif.
    ADD 1 TO $cnt.
    ADD 1 TO $matcnt.
    INSERT ztco_ck11 FROM *ztco_ck11.
    $mod = $cnt MOD 100.
    IF $mod EQ 0.
      COMMIT WORK.
    ENDIF.

    AT END OF artnr.
      WRITE:/ $matcnt COLOR 2,
              'record(s)'.
      g_matcnt = g_matcnt + $matcnt.
    ENDAT.

  ENDLOOP.


*--------------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------------*
  IF g_matcnt > 0.
    WRITE:/ 'Total : ',
             g_matcnt COLOR 2,
            'record(s)'.
  ENDIF.

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.

  __cls : it_ztco_shop_sum,it_ztco_ck11.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_shop_sum
  FROM ztco_shop_sum
  WHERE kokrs EQ p_kokrs
   AND bdatj EQ p_bdatj
   AND poper EQ p_poper
   AND artnr IN s_artnr
   AND llv_matnr IN s_matnr
   AND typps EQ 'M'.

  LOOP AT it_ztco_shop_sum.
    $ix = sy-tabix.
    it_ztco_shop_sum-fsc = it_ztco_shop_sum-artnr.
    MODIFY it_ztco_shop_sum INDEX $ix TRANSPORTING fsc.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_ck11
  FROM ztco_ck11
  WHERE kokrs EQ p_kokrs
   AND klvar EQ 'ZUNF'
   AND bdatj EQ p_bdatj
   AND poper EQ p_poper
   AND artnr IN s_artnr
   AND compn IN s_matnr.

ENDFORM.                    " get_row_data

*---------------------------------------------------------------------*
*       FORM GET_PRODUCT_GR                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_product_gr.

  DATA : BEGIN OF it_proc_gr OCCURS 0,
          par_werks LIKE ztco_shop_sum-bwkey,
          artnr     LIKE ztco_shop_sum-artnr,
          aufnr     LIKE ztco_shop_sum-aufnr,
          verid     LIKE ztco_shop_sum-verid,
          bdatj     LIKE ztco_shop_sum-bdatj,
          poper     LIKE ztco_shop_sum-poper,
        END OF it_proc_gr.

  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_ztco_shop_sum.
    MOVE-CORRESPONDING it_ztco_shop_sum TO it_proc_gr.
    APPEND it_proc_gr. CLEAR it_proc_gr.
  ENDLOOP.

  SORT it_proc_gr.
  DELETE ADJACENT DUPLICATES FROM it_proc_gr.

  CHECK NOT it_proc_gr[] IS INITIAL.

** read GR data
  SELECT  a~bwkey a~matnr a~verid_nd
          c~aufnr
          b~out_menge
          b~meinh
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003_temp
    FROM ckmlmv001 AS a
    INNER JOIN ckmlmv003 AS b
       ON a~kalnr    =  b~kalnr_bal
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = b~kalnr_in
     FOR ALL ENTRIES IN it_proc_gr
   WHERE a~werks    =  it_proc_gr-par_werks
     AND a~matnr    =  it_proc_gr-artnr
     AND a~btyp     =  'BF'
     AND a~bwkey    =  it_proc_gr-par_werks
     AND b~gjahr    =  p_bdatj
     AND b~perio    =  p_poper
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05'.
*    and c~loekz    = space. "Not deleted.

  LOOP AT it_ckmlmv003_temp.
    MOVE-CORRESPONDING it_ckmlmv003_temp TO it_ckmlmv003.
    CLEAR: "it_ckmlmv003-verid_nd,
           it_ckmlmv003-aufnr,
           it_ckmlmv003-meinh.
    COLLECT it_ckmlmv003. CLEAR it_ckmlmv003.
  ENDLOOP.

  DELETE it_ckmlmv003 WHERE out_menge EQ 0.

  SORT it_ckmlmv003 BY matnr verid_nd.

ENDFORM.                    " get_product_gr

*---------------------------------------------------------------------*
*       FORM GET_PP_GRQTY                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_PP_GRQTY                                                    *
*---------------------------------------------------------------------*
FORM get_pp_grqty USING p_pp_artnr
                        p_pp_verid
                  CHANGING p_pp_grqty TYPE ckml_outmenge.

  CLEAR it_ckmlmv003.

  READ TABLE it_ckmlmv003 WITH KEY matnr = p_pp_artnr
                                  verid_nd = p_pp_verid
      BINARY SEARCH.

  CHECK : sy-subrc EQ 0,
          it_ckmlmv003-out_menge > 0.

  p_pp_grqty = it_ckmlmv003-out_menge.

*  CLEAR : P_PP_GRQTY.
*
*  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*       EXPORTING
*            INPUT                = IT_CKMLMV003-OUT_MENGE
*            UNIT_IN              = IT_CKMLMV003-MEINH
*            UNIT_OUT             = IT_CKMLMV003-MEINH
*       IMPORTING
*            OUTPUT               = P_PP_GRQTY
*       EXCEPTIONS
*            CONVERSION_NOT_FOUND = 1
*            DIVISION_BY_ZERO     = 2
*            INPUT_INVALID        = 3
*            OUTPUT_INVALID       = 4
*            OVERFLOW             = 5
*            TYPE_INVALID         = 6
*            UNITS_MISSING        = 7
*            UNIT_IN_NOT_FOUND    = 8
*            UNIT_OUT_NOT_FOUND   = 9.
*
*  IF P_PP_GRQTY = 0.
*    BREAK-POINT.
*  ENDIF.

ENDFORM.                    " pp_grqty
*&---------------------------------------------------------------------*
*&      Form  get_verpr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ZTCO_SHOP_SUM_VERPR  text
*----------------------------------------------------------------------*
FORM get_verpr CHANGING p_verpr.

  DATA $year(4) TYPE n.
  DATA $mon(2) TYPE n.

  $mon = it_ztco_shop_sum-poper+1(2).
  $year = it_ztco_shop_sum-bdatj.

  DO 100 TIMES.
    SELECT SINGLE verpr INTO p_verpr
    FROM mbewh
     WHERE matnr EQ it_ztco_shop_sum-llv_matnr
       AND bwkey EQ it_ztco_shop_sum-bwkey
       AND lfgja EQ $year
       AND lfmon EQ $mon.
    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
    IF $mon EQ '01'.
      $mon = '12'.
      $year = $year - 1.
    ELSE.
      $mon = $mon - 1.
    ENDIF.
  ENDDO.

ENDFORM.                    " get_verpr
*&---------------------------------------------------------------------*
*&      Form  get_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ZTCO_SHOP_SUM_LIFNR  text
*----------------------------------------------------------------------*
FORM get_lifnr USING p_compn CHANGING p_lifnr.


  DATA: BEGIN OF it_lifnr OCCURS 21,
          lifnr TYPE lifnr,
        END   OF it_lifnr.
  DATA $matnr TYPE matnr.
  DATA l_lifnr TYPE lifnr.
  DATA : $used_source TYPE  tabname16,
         $ekorg TYPE  ekorg,
         $infnr TYPE  infnr.


  SELECT DISTINCT lifnr INTO TABLE it_lifnr
               FROM ztcou137
              WHERE bukrs EQ g_bukrs
                AND matnr EQ p_compn
                AND ( ( zdtfr <= gv_date_f AND zdtto >= gv_date_t )
                  OR  ( zdtfr <= gv_date_t AND zdtto >= gv_date_t ) ).

  IF sy-subrc NE 0.

    PERFORM get_simple_matnr USING p_compn
                          CHANGING $matnr.
    REPLACE '%' WITH '' INTO $matnr.
    CONCATENATE $matnr '%' INTO $matnr.

    SELECT DISTINCT lifnr INTO TABLE it_lifnr
                 FROM ztcou137
                WHERE bukrs EQ g_bukrs
                  AND matnr LIKE $matnr
                AND ( ( zdtfr <= gv_date_f AND zdtto <= gv_date_t )
                OR  ( zdtfr <= gv_date_t AND zdtto >= gv_date_t ) ).
  ENDIF.

  READ TABLE it_lifnr INDEX 2.
*---multiple vendor - take KD vendor
  IF sy-subrc EQ 0.
    LOOP AT it_lifnr.
      SELECT SINGLE lifnr INTO l_lifnr
                  FROM lfa1
                WHERE lifnr EQ it_lifnr-lifnr
                  AND land1 <> 'US'.
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF l_lifnr EQ space.
    READ TABLE it_lifnr INDEX 1.
    IF sy-subrc EQ 0.
      l_lifnr = it_lifnr-lifnr.
    ENDIF.
  ENDIF.

  IF l_lifnr EQ space .

    CALL FUNCTION 'Z_CO_GET_VENDOR_SOURCE_AUTO'
         EXPORTING
              bukrs           = g_bukrs
              matnr           = p_compn
              available_date  = gv_date_f
         IMPORTING
              lifnr           = l_lifnr
              used_source     = $used_source
              ekorg           = $ekorg
              infnr           = $infnr
         EXCEPTIONS
              no_source_found = 1
              invalid_werks   = 2
              OTHERS          = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  p_lifnr = l_lifnr.

ENDFORM.                    " get_lifnr

*---------------------------------------------------------------------*
*       FORM get_simple_matnr                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_COMPN                                                       *
*  -->  P_$MATNR                                                      *
*---------------------------------------------------------------------*
FORM get_simple_matnr USING    p_compn
                      CHANGING p_$matnr.

* EM4CPM100210QZ
* 012345678901

  DATA $strlen TYPE i.

  $strlen = strlen( p_compn ).

  IF p_compn+5(2) EQ 'M1'.
    p_$matnr = p_compn(12).
  ELSE.
    IF $strlen > 11.
      CASE $strlen.
        WHEN 12 OR 13.
          p_$matnr = p_compn(10).
*        WHEN 14 OR 15.
*          p_$matnr = p_compn+(12).
      ENDCASE.
    ELSE.
      p_$matnr = p_compn.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_simple_matnr
*&---------------------------------------------------------------------*
*&      Form  get_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date.
  DATA: l_date(8).

  CLEAR: gv_info_f, gv_date_f, gv_date_t, gv_date3.

  CLEAR l_date.
  CONCATENATE '0101' p_bdatj INTO l_date.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            input  = l_date
       IMPORTING
            output = gv_info_f.

  CLEAR l_date.
  CONCATENATE p_poper+1(2) '01' p_bdatj  INTO l_date.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            input  = l_date
       IMPORTING
            output = gv_date_f.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = p_bdatj
            i_periv = 'K0'
            i_poper = p_poper
       IMPORTING
            e_date  = gv_date_t.

  g_bukrs = 'H201'.

ENDFORM.                    " get_date
