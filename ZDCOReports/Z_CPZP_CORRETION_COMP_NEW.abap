*&---------------------------------------------------------------------*
*& Recalculation program for Hyundai for the period 2007006
*&---------------------------------------------------------------------*
*& This works according to following assumptions:
*&    - lotsize = 1 and no GR reversals. Thus, an order which has one
*&      GR confirmation will be considered 'closed' for WIP.
*&    - no scrap, no variances - these quantities won't be computed
*&      for the new WIP
*&    - no special components are used
*&    - no activities will be considered
*&    - for the prev per 2007005 WIP calculation is correct
*&    - Updates the GMSUM and ISTMN for the period 2007006
*&    - GMPER for the period 2007006 is correct
*&---------------------------------------------------------------------*

REPORT  z_cpzp_corretion_comp_new LINE-SIZE 250
NO STANDARD PAGE HEADING.
TYPE-POOLS icon.
TABLES : mara, *cpzp.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-sl1.
PARAMETERS:
  p_matnr  TYPE matnr,
  p_werks  TYPE werks_d,
  p_aufnr  TYPE aufnr,
  p_gjper  TYPE co_gjper, " DEFAULT '2007006'.
  p_ver    TYPE verid.
SELECT-OPTIONS s_matnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-sl2.
PARAMETERS:
  p_debug  TYPE c AS CHECKBOX,
  p_atrep  TYPE c AS CHECKBOX,
  p_lock  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

PARAMETERS     : P_FORC AS CHECKBOX.

*data: p_gjper  type co_gjper.
*p_gjper = '2007006'.

DEFINE u_break.
  if not p_debug is initial.
    break-point.
  endif.
END-OF-DEFINITION.
* --
DATA:
  gf_accassobj TYPE ppc_accassobj_int,
  gf_objnr     TYPE j_objnr,
  gf_datum     TYPE budat,
  gf_frdatum  TYPE budat,
  gf_icon      LIKE icon_red_light,

  BEGIN OF gs_stats,
      kkey     TYPE c,
      text(30) TYPE c,
      val1     TYPE i,
      val2     TYPE i,
  END OF gs_stats,
  gt_stats LIKE TABLE OF gs_stats,

  BEGIN OF gs_order,
      orderid  TYPE ppc_orderid,
      kdauf    TYPE kdauf,
      ordernr  TYPE ppc_ordernr,
      accassobj TYPE ppc_accassobj_int,
  END OF gs_order,
*  gt_openorder like table of gs_order,
  gt_closedorder LIKE TABLE OF gs_order,
  gt_varorder  LIKE TABLE OF gs_order,

  BEGIN OF gs_comp,
      matnr    TYPE matnr,
      werks    TYPE werks_d,
      sobkz    TYPE sobkz,
      kzbws    TYPE kzbws,
      kdauf    TYPE kdauf,
      kdpos    TYPE kdpos,
      confunit TYPE ppc_confunit,
      quant    TYPE menge_pos,
  END OF gs_comp,
  gt_comp LIKE HASHED TABLE OF gs_comp
        WITH UNIQUE KEY matnr werks sobkz kzbws kdauf kdpos confunit,

  BEGIN OF gs_finaltb,
      matnr    TYPE matnr,
      kdauf    TYPE kdauf,
      kdpos    TYPE kdpos,
      objnr    TYPE objnr,
      f_objnr  TYPE f_objnr,
      gjper    TYPE co_gjper,
      quant    TYPE menge_pos,
      istmn    TYPE ist_menge,
      gmper    TYPE g_men_per,
      gmsum    TYPE g_men_sum,
      status   TYPE c,
      delta    TYPE menge_pos,
  END OF gs_finaltb,
  gs_hidtab  LIKE gs_finaltb,
  gt_finaltb LIKE TABLE OF gs_finaltb.


*---------------
LOAD-OF-PROGRAM.
*---------------
  SET RUN TIME CLOCK RESOLUTION LOW.


*-------------------
AT SELECTION-SCREEN.
*-------------------
*  p_gjper  = '2007006'.
  PERFORM check_input_val  USING p_matnr
                                 p_werks
                                 p_gjper
                        CHANGING gf_datum
                                 gf_frdatum
                                 gf_accassobj.

*------------------
START-OF-SELECTION.
*------------------
*CCC
  PERFORM enqueue.
*CCC
  PERFORM find_closed_orders USING p_matnr
                                 p_werks
                                 gf_datum
                                 gf_frdatum
                        CHANGING gt_closedorder
                                 gt_stats.

  PERFORM compute_bfl_qty  USING gf_datum
                                 gt_closedorder
                        CHANGING gt_comp
                                 gt_stats
                                 gf_accassobj.

  PERFORM update_bfl_cpzp USING gf_accassobj
                                 p_gjper
                                 gt_comp
                        CHANGING gf_objnr
                                 gt_finaltb
                                 gt_stats.
  PERFORM write_log USING gt_stats '1'.


*  perform write_output     using p_matnr
*                                 p_werks
*                                 gf_accassobj
*                                 gf_objnr
*                                 p_eonly
*                                 gt_openorder
*                                 gt_finaltb
*                                 gt_stats.
*

*-----------------
AT LINE-SELECTION.
*-----------------

  CASE sy-lsind.

    WHEN 1.
      IF NOT gs_hidtab IS INITIAL.
*        perform popup_detail changing gs_hidtab.
      ELSEIF NOT gs_stats IS INITIAL.
        PERFORM write_log USING gt_stats '2'.
      ENDIF.

    WHEN 2.
      "do nothing

  ENDCASE.



*&---------------------------------------------------------------------*
*&      Form  check_input_val
*&---------------------------------------------------------------------*
FORM check_input_val USING    pf_matnr
                              pf_werks
                              pf_gjper
                     CHANGING pf_datum
                              pf_frdatum
                              pf_accassobj.

  DATA:
    lf_year(4) TYPE n,
    lf_peri(3) TYPE n,
    lf_datum   TYPE budat,
    ls_head    TYPE ppc_head,
    lp_kokrs   LIKE aufk-kokrs,   "Kostenrechungskreis
    lp_variant LIKE tka01-lmona.  "GJahr-Variante


  u_break.

* get accounting object
  SELECT SINGLE accassobj
      FROM ppc_ord_inf
      INTO pf_accassobj
      WHERE materialnr = pf_matnr
        AND plant      = pf_werks
        AND version    = p_ver.

* get limit date
  lf_year = pf_gjper DIV 1000.
  lf_peri = pf_gjper - ( lf_year * 1000 ).


* Zuerst Kostenrechnungskreis aus Werk bestimmen
  CALL FUNCTION 'RM_KOKRS_TO_PLANT_FIND'
       EXPORTING
            werks   = pf_werks
       IMPORTING
            e_kokrs = lp_kokrs
       EXCEPTIONS
            OTHERS  = 1.
  IF sy-subrc NE 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO       "P45K072474
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.       "P45K072474
*            raising wrong_input.                            "P45K072474

    WRITE:/ sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.         "P45K072474
    LEAVE PROGRAM.


  ENDIF.

* GJahr-Variante bestimmen
  SELECT SINGLE lmona FROM tka01 INTO lp_variant
                                 WHERE kokrs = lp_kokrs.

* To get the posting period date
  CALL FUNCTION 'G_POSTING_DATE_OF_PERIOD_GET'
       EXPORTING
            period              = lf_peri
            variant             = lp_variant
            year                = lf_year
       IMPORTING
            from_date           = pf_frdatum
            to_date             = pf_datum
       EXCEPTIONS
            period_not_defined  = 1
            variant_not_defined = 2
            OTHERS              = 3.





* check to see if there are any open backflushes
  SELECT SINGLE * FROM ppc_head INTO ls_head
      WHERE postdate LE gf_datum
        AND accassobj EQ pf_accassobj
        AND ( ( flg_synch NE 'X' AND flg_synch NE 'V' )
              OR ( flg_asynch NE 'X' AND flg_asynch NE 'V' ) ) .

  IF sy-dbcnt GT 0 and p_forc eq space.

    "ups! unprocessed PPCGO
    MESSAGE I999(PP) WITH 'Before running the report, run PPCGO'(M01)
                          PF_MATNR 'period'(M02) PF_GJPER.

*    WRITE:/ 'Before running the report, run PPCGO',
*                          pf_matnr, 'period', pf_gjper.

    LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " check_input_val


*&---------------------------------------------------------------------*
*&      Form  find_open_orders
*&---------------------------------------------------------------------*
FORM find_closed_orders  USING    pf_matnr     TYPE matnr
                                pf_werks     TYPE werks_d
                                pf_datum     TYPE budat
                                pf_frdatum     TYPE budat
                       CHANGING pt_closedorder LIKE gt_closedorder
                                pt_stats     LIKE gt_stats.

  DATA:
    ls_stats   LIKE gs_stats.
  RANGES:
    lt_selopt  FOR ppc_ord_inf-orderid.

  u_break.
  GET RUN TIME FIELD ls_stats-val1.

* get all open orders by a single select statement
  SELECT orderid ordernr accassobj
      FROM ppc_ord_inf AS o
      INTO CORRESPONDING FIELDS OF TABLE pt_closedorder
      WHERE materialnr = pf_matnr
        AND plant      = pf_werks
        AND version    = p_ver
        AND exists ( SELECT * FROM ppc_head
                     WHERE orderid EQ o~orderid
                       AND postdate LE pf_datum
                       AND postdate GE pf_frdatum
                       AND flg_gr_head EQ 'X'
                    ).

* stats and log
  GET RUN TIME FIELD ls_stats-val2.
  ls_stats-kkey = 'R'.
  ls_stats-text = 'Order selection took:'.
  APPEND ls_stats TO pt_stats.
  CLEAR ls_stats.
  ls_stats-kkey = 'A'.
  DESCRIBE TABLE pt_closedorder LINES ls_stats-val1.
  APPEND ls_stats TO pt_stats.
  CLEAR ls_stats.

ENDFORM.                    " find_open_orders



*&---------------------------------------------------------------------*
*&      Form  compute_bfl_qty
*&---------------------------------------------------------------------*
FORM compute_bfl_qty  USING    pf_datum     TYPE budat
                               pt_closedorder LIKE gt_closedorder
                      CHANGING pt_comp      LIKE gt_comp
                               pt_stats     LIKE gt_stats
                               pf_accassobj.

  DATA:
    ls_order   LIKE gs_order,
    ls_comp    LIKE gs_comp,
    ls_stats   LIKE gs_stats,
    ls_mats    TYPE ppc_material_components,
    lt_repts   TYPE ppc_t_reppoint,
    lt_mats    TYPE ppc_t_material_components,
    lt_matsvar TYPE ppc_t_material_components,
    lt_matsrev TYPE ppc_t_material_components,
    lt_matreva TYPE ppc_t_material_components,
    lf_year(4) TYPE n,
    lf_peri(3) TYPE n,
    lf_datum   TYPE budat,
    v_accassobj TYPE ppc_accassobj_int.

  u_break.
  GET RUN TIME FIELD ls_stats-val1.

* Process orders one by one
  LOOP AT pt_closedorder INTO ls_order.
    IF v_accassobj IS INITIAL.
      v_accassobj = ls_order-accassobj.
      pf_accassobj = ls_order-accassobj.
    ELSEIF v_accassobj <> ls_order-accassobj.
      "ups! Different accass obj
*      MESSAGE S999(PP) WITH 'Diff acc objects for '(M01)
*                          P_MATNR 'period'(M02) P_GJPER.
      WRITE:/ 'Diff acc objects for ',
              p_matnr, 'period', p_gjper.

    ENDIF.


    " get backflushed components
    PERFORM get_comp_per_order USING ls_order-orderid
                                     pf_datum
                            CHANGING lt_mats
                                     lt_matsvar
                                     lt_matsrev
                                     lt_matreva.

    " collect the issued components
    LOOP AT lt_mats INTO ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ls_mats-quantity.

      ls_comp-confunit = ls_mats-unit_of_measure.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.
    " subtract the variance values,
    LOOP AT lt_matsvar INTO ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ( -1 ) * ls_mats-quantity.
      ls_comp-confunit = ls_mats-unit_of_measure.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.
    " subtract the reversed GI's
    LOOP AT lt_matsrev INTO ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ( -1 ) * ls_mats-quantity.
      ls_comp-confunit = ls_mats-unit_of_measure.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.
    " add the variance reversed values,
    LOOP AT lt_matreva INTO ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ls_mats-quantity.
      ls_comp-confunit = ls_mats-unit_of_measure.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.

  ENDLOOP.

  SORT pt_comp BY matnr ASCENDING.

* stats and log
  GET RUN TIME FIELD ls_stats-val2.
  ls_stats-kkey = 'R'.
  ls_stats-text = 'Component selection took:'.
  APPEND ls_stats TO pt_stats.
  CLEAR ls_stats.
  ls_stats-kkey = 'B'.
  DESCRIBE TABLE pt_comp LINES ls_stats-val1.
  APPEND ls_stats TO pt_stats.
  CLEAR ls_stats.


ENDFORM.                    " compute_bfl_qty


*&---------------------------------------------------------------------*
*&      Form  compare_bfl_cpzp
*&---------------------------------------------------------------------*
FORM update_bfl_cpzp  USING    pf_accassobj TYPE ppc_accassobj_int
                                pf_gjper     TYPE co_gjper
                                pt_comp      LIKE gt_comp
                       CHANGING pf_objnr     TYPE j_objnr
                                pt_finaltb   LIKE gt_finaltb
                                pt_stats     LIKE gt_stats.

  DATA:
    lf_aufnr   TYPE aufnr,
    lf_kaln1   TYPE ck_kalnr1,
    lf_fobjnr  TYPE f_objnr,
    lf_quant   TYPE ist_menge,
    ls_comp    LIKE gs_comp,
    ls_finaltb LIKE gs_finaltb,
    ls_stats   LIKE gs_stats,
    ls_cpzp    TYPE cpzp,
    lt_cpzp    TYPE TABLE OF cpzp,
    ls_cpzp_prv    TYPE cpzp,
    lt_cpzp_prv TYPE TABLE OF cpzp.
  DATA :
      nistmn    TYPE ist_menge,
      ngmsum    TYPE g_men_sum,
      iv_err    TYPE sy-subrc,
      iv_upd    TYPE sy-subrc,
      iv_prvper TYPE co_gjper.

* by ig.moon 7/7/2008 {
  DATA $pf_gjper LIKE pf_gjper.
  DATA $nistmn LIKE nistmn.
* }

* Assuming the prev period as 2007005
*  if pf_gjper = '2007006'.
*    iv_prvper = '2007005'.
*  endif.


* get prevoius {
  DATA : $year(4) TYPE n,
         $peri(3) TYPE n.

  iv_prvper = pf_gjper - 1.

  $year = iv_prvper DIV 1000.
  $peri = iv_prvper - ( $year * 1000 ).

  IF $peri EQ '000'.
    $year = $year - 1 .
    CONCATENATE $year '012' INTO iv_prvper.
  ENDIF.
* }

  u_break.
  GET RUN TIME FIELD ls_stats-val1.
  iv_err  = 0.
* Get accounting order number anf cpzp object number
  CALL FUNCTION 'QRP_QRP002_READ'
       EXPORTING
            if_cc_guid = pf_accassobj
       IMPORTING
            ef_aufnr   = lf_aufnr
       EXCEPTIONS
            OTHERS     = 1.
  IF NOT p_aufnr IS INITIAL.
    IF p_aufnr <> lf_aufnr.
      "ups! Different accass obj
*      MESSAGE S999(PP) WITH 'Diff acc object exists for the period '
*                            P_GJPER.

      WRITE:/ 'Diff acc object exists for the period ', p_gjper.

    ENDIF.
  ENDIF.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'QRP_APO_PKOSA_AUFNR_TO_OBJNR'
         EXPORTING
              if_pkosa_aufnr = lf_aufnr
         IMPORTING
              ef_pkosa_objnr = pf_objnr.
  ENDIF.

* ... and buffer cpzp table
  SELECT * FROM cpzp INTO TABLE lt_cpzp
      WHERE objnr = pf_objnr
        AND gjper = pf_gjper.
*.....get prev period cpzp value for the calculation of istmn
* ... and buffer cpzp table
  SELECT * FROM cpzp INTO TABLE lt_cpzp_prv
      WHERE objnr = pf_objnr
        AND gjper = iv_prvper.


* Build final table
  LOOP AT pt_comp INTO ls_comp.

*   ... write already determined values
    ls_finaltb-matnr = ls_comp-matnr.
    ls_finaltb-quant = ls_comp-quant.

*   ... find objnr from comat
    CALL FUNCTION 'QRP_APO_COMP_OBJNR_ENCODE'
         EXPORTING
              if_matnr   = ls_comp-matnr
              if_werks   = ls_comp-werks
              if_vbeln   = ls_comp-kdauf
              if_posnr   = ls_comp-kdpos
              if_kzbws   = ls_comp-kzbws
              if_sobkz   = ls_comp-sobkz
         IMPORTING
              ef_f_objnr = lf_fobjnr
              ef_kaln1   = lf_kaln1
         EXCEPTIONS
              OTHERS     = 1.

    IF sy-subrc NE 0.

      ls_finaltb-kdauf   = ls_comp-kdauf.
      ls_finaltb-kdpos   = ls_comp-kdpos.
      ls_finaltb-f_objnr = 'Not found!'.
      ls_finaltb-status  = 'P'.

    ELSE.

*     ... find cpzp data
      READ TABLE lt_cpzp INTO ls_cpzp
              WITH KEY objnr   = pf_objnr
                       f_objnr = lf_fobjnr.
      IF sy-subrc EQ 0.
        IF ls_cpzp-meinh NE ls_comp-confunit.
*          MESSAGE S001(00) WITH 'Uom problem'
*                       LS_COMP-MATNR.

          WRITE:/ 'Uom problem', ls_comp-matnr.

        ENDIF.
        ls_finaltb-objnr   = ls_cpzp-objnr.
        ls_finaltb-f_objnr = ls_cpzp-f_objnr.
        ls_finaltb-gjper   = ls_cpzp-gjper.
        ls_finaltb-gmper   = ls_cpzp-gmper.
        IF ls_cpzp-gmsum <> ls_comp-quant.
          ngmsum   = ls_comp-quant.
        ELSE.
          ngmsum  = ls_cpzp-gmsum.
        ENDIF.
        READ TABLE lt_cpzp_prv INTO ls_cpzp_prv
              WITH KEY objnr   = pf_objnr
                       f_objnr = lf_fobjnr.
        IF sy-subrc = 0.
          nistmn = ( ls_cpzp_prv-istmn - ls_cpzp_prv-gmsum ) +
                     ls_cpzp-gmper.
        ELSE.
          nistmn = ls_cpzp-gmper.
        ENDIF.

        IF ls_cpzp-istmn <> nistmn OR ls_cpzp-gmsum <> ls_comp-quant.

          WRITE:
           / 'Assembly:', p_matnr COLOR 5 INVERSE INTENSIFIED,
*           / 'Plant:   ', p_werks COLOR 5 INVERSE INTENSIFIED,
*           / 'Acc. obj:', pf_accassobj COLOR 5 INVERSE INTENSIFIED,
            'P.Ver:   ', p_ver COLOR 5 INVERSE INTENSIFIED,
            'Objnr:   ', pf_objnr COLOR 5 INVERSE INTENSIFIED,
            'F_Objnr:   ', lf_fobjnr COLOR 5 INVERSE INTENSIFIED,
            'GMSUM:   ', ngmsum COLOR 5 INVERSE INTENSIFIED,
            'ISTMN:   ', nistmn COLOR 5 INVERSE INTENSIFIED.
          IF p_atrep EQ 'X'.

            UPDATE cpzp
             SET gmsum   = ngmsum
                 istmn = nistmn
             WHERE objnr = pf_objnr
               AND f_objnr = lf_fobjnr
              AND gjper = pf_gjper.
            IF sy-subrc <> 0.
              iv_err = 1.
            ENDIF.

* by ig.moon 07/07/2008 {

            $pf_gjper = pf_gjper + 1.
            SELECT SINGLE * INTO *cpzp
            FROM cpzp
             WHERE objnr = pf_objnr
               AND f_objnr = lf_fobjnr
              AND gjper = $pf_gjper.
            IF sy-subrc EQ 0.
              $nistmn = *cpzp-istmn + ( nistmn - ls_cpzp-istmn ).
              UPDATE cpzp
               SET istmn = $nistmn
               WHERE objnr = pf_objnr
                 AND f_objnr = lf_fobjnr
                AND gjper = $pf_gjper.
              IF sy-subrc <> 0.
                iv_err = 1.
              ENDIF.
            ENDIF.
* }
          ENDIF.
          ls_finaltb-gmsum   = ngmsum.
          ls_finaltb-istmn   = nistmn.
          ls_finaltb-status   = 'X'.
          iv_upd = 1.
        ELSE.

*     This line is ok...
          ls_finaltb-gmsum   = ls_cpzp-gmsum.
          ls_finaltb-istmn   = ls_cpzp-istmn.
          ls_finaltb-status   = 'Y'.
          WRITE:/ 'The component is consistent:', ls_comp-matnr COLOR 6
                 INVERSE INTENSIFIED.

        ENDIF.
        IF NOT ls_finaltb IS INITIAL.
          APPEND ls_finaltb TO pt_finaltb.
          CLEAR ls_finaltb.
        ENDIF.
      ENDIF.
    ENDIF.
*   append line to the final table
    CLEAR: nistmn, ngmsum.
  ENDLOOP.
* Do commit if everything is right.
  IF p_atrep EQ 'X'.
* if no error happened during updation
    IF iv_err = 0.
* Atleast one record modified.
      IF iv_upd = 1.
        COMMIT WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

* stats and log
  GET RUN TIME FIELD ls_stats-val2.
  ls_stats-kkey = 'R'.
  ls_stats-text = 'CPZP comparison took:'.
  APPEND ls_stats TO pt_stats.
  CLEAR ls_stats.

ENDFORM.                    " compare_bfl_cpzp


*---------------------------------------------------------------------*
*       FORM get_comp_per_order                                       *
*---------------------------------------------------------------------*
FORM get_comp_per_order
         USING  if_orderid  TYPE ppc_orderid
                if_postdate TYPE budat
       CHANGING et_material_components TYPE ppc_t_material_components
                et_rev_mat_components  TYPE ppc_t_material_components
                et_mat_comp_var        TYPE ppc_t_material_components
                et_rev_mat_comp_var    TYPE ppc_t_material_components.


  CONSTANTS c_gmove_ind_0 TYPE ppc_gmove_ind VALUE '0'.
  DATA:
    BEGIN OF ls_ext_mat_comp,
      mat_comp LIKE ppc_material_components,
      head_sales_doc TYPE kdauf,
      head_sales_doc_item TYPE kdpos,
      head_wbs_elem TYPE ps_psp_pnr,
      flg_reversal TYPE xflag,
    END OF ls_ext_mat_comp.


* ... Initialize output
  REFRESH et_material_components.
  REFRESH et_rev_mat_components.
  REFRESH et_mat_comp_var.
  REFRESH et_rev_mat_comp_var.


  IF p_lock EQ 'X'.

* ... Lock the orderid
    CALL FUNCTION 'ENQUEUE_E_PPC_ORDERID'
         EXPORTING
              mode_ppc_ord_inf = 'E'
              mandt            = sy-mandt
              orderid          = if_orderid
              _scope           = '1'
              _wait            = 'X'
         EXCEPTIONS
              foreign_lock     = 1
              system_failure   = 2
              OTHERS           = 3.

    IF sy-subrc EQ 1.
*      MESSAGE S550(PPC1DM). " raising lock_error.
      WRITE:/
      'No components can be determined as simultaneous backflush'.
      LEAVE PROGRAM.

    ELSEIF sy-subrc GT 2.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*            raising lock_error.

      WRITE:/ sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.

      LEAVE PROGRAM.
    ENDIF.

  ENDIF.

* ... Select components
  SELECT
    t1~reppoint
    t2~matnr
    t2~werks
    t2~lgort
    t2s~charg
    t2~prvbe
    t2~gmove_ind
    t2~sobkz
    t2~kzbws
    t2~kzvbr
    t2s~kdauf
    t2s~kdpos
    t2s~pspnr
    t2s~calcnr
    SUM( t4~confquant ) AS confquant
    SUM( t5~delta_confquant )  AS delta_confquant
    t4~confunit
    t1~kdauf
    t1~kdpos
    t1~pspnr
    t1~flg_reversal
  INTO
    (ls_ext_mat_comp-mat_comp-reppoint,
     ls_ext_mat_comp-mat_comp-mat_number,
     ls_ext_mat_comp-mat_comp-plant,
     ls_ext_mat_comp-mat_comp-storage_loc,
     ls_ext_mat_comp-mat_comp-batch,
     ls_ext_mat_comp-mat_comp-supply_area,
     ls_ext_mat_comp-mat_comp-gmove_ind,
     ls_ext_mat_comp-mat_comp-special_stock,
     ls_ext_mat_comp-mat_comp-special_stock_val,
     ls_ext_mat_comp-mat_comp-consumpt_posting,
     ls_ext_mat_comp-mat_comp-sales_doc,
     ls_ext_mat_comp-mat_comp-sales_doc_item,
     ls_ext_mat_comp-mat_comp-wbs_elem,
     ls_ext_mat_comp-mat_comp-costing_num,
     ls_ext_mat_comp-mat_comp-quantity,
     ls_ext_mat_comp-mat_comp-delta_quantity,
     ls_ext_mat_comp-mat_comp-unit_of_measure,
     ls_ext_mat_comp-head_sales_doc,
     ls_ext_mat_comp-head_sales_doc_item,
     ls_ext_mat_comp-head_wbs_elem,
     ls_ext_mat_comp-flg_reversal)
  FROM
      ( ( ( ppc_head AS t1 INNER JOIN ppc_conf_mat AS t4
          ON t1~headid = t4~headid )
        LEFT OUTER JOIN ppc_conf_mat_var AS t5
          ON t4~headid = t5~headid AND
             t4~accid = t5~accid )
        INNER JOIN ppc_mat_det AS t2s
          ON t4~accid = t2s~accid )
        INNER JOIN ppc_mat AS t2
          ON t2s~matid = t2~matid
  WHERE
        t1~orderid EQ if_orderid AND
        t2~gmove_ind EQ c_gmove_ind_0 AND
        t1~postdate LE if_postdate AND
        t2~matnr IN s_matnr
  GROUP BY
      T1~REPPOINT
      t2~matnr
      T2~WERKS
      t2~lgort
      t2s~charg
      t2~prvbe
      t2~gmove_ind
      t2~sobkz
      t2~kzbws
      t2~kzvbr
      t2s~kdauf
      t2s~kdpos
      t2s~pspnr
      t2s~calcnr
      t4~confunit
      t1~kdauf
      t1~kdpos
      t1~pspnr
      t1~flg_reversal
      %_HINTS ORACLE '&SUBSTITUTE LITERALS&'
              MSSQLNT '&SUBSTITUTE LITERALS&'
              DB2     '&SUBSTITUTE LITERALS&'.


*   AMF_CONVERT
    IF NOT ls_ext_mat_comp-mat_comp-special_stock IS INITIAL AND
       NOT ls_ext_mat_comp-head_sales_doc IS INITIAL AND
           ls_ext_mat_comp-mat_comp-sales_doc IS INITIAL.
      MOVE ls_ext_mat_comp-head_sales_doc TO
           ls_ext_mat_comp-mat_comp-sales_doc.
    ENDIF.
    IF NOT ls_ext_mat_comp-mat_comp-special_stock IS INITIAL AND
       NOT ls_ext_mat_comp-head_sales_doc_item IS INITIAL AND
           ls_ext_mat_comp-mat_comp-sales_doc_item IS INITIAL.
      MOVE ls_ext_mat_comp-head_sales_doc_item TO
           ls_ext_mat_comp-mat_comp-sales_doc_item.
    ENDIF.
    IF NOT ls_ext_mat_comp-mat_comp-special_stock IS INITIAL AND
       NOT ls_ext_mat_comp-head_wbs_elem IS INITIAL AND
           ls_ext_mat_comp-mat_comp-wbs_elem IS INITIAL.
      MOVE ls_ext_mat_comp-head_wbs_elem TO
             ls_ext_mat_comp-mat_comp-wbs_elem.
    ENDIF.


*   Build output tables: first check reversal flag
    IF ls_ext_mat_comp-flg_reversal IS INITIAL.

*     Direct posting
      APPEND ls_ext_mat_comp-mat_comp TO et_material_components.
*     When variances found, fill the export table accordingly
      IF NOT ls_ext_mat_comp-mat_comp-delta_quantity IS INITIAL.
        MOVE ls_ext_mat_comp-mat_comp-delta_quantity TO
             ls_ext_mat_comp-mat_comp-quantity.
        CLEAR ls_ext_mat_comp-mat_comp-delta_quantity.
        APPEND ls_ext_mat_comp-mat_comp TO et_mat_comp_var.
      ENDIF.

    ELSE.

*     Reversal
      APPEND ls_ext_mat_comp-mat_comp TO et_rev_mat_components.
*     When variances found, fill the export table accordingly
      IF NOT ls_ext_mat_comp-mat_comp-delta_quantity IS INITIAL.
        MOVE ls_ext_mat_comp-mat_comp-delta_quantity TO
             ls_ext_mat_comp-mat_comp-quantity.
        CLEAR ls_ext_mat_comp-mat_comp-delta_quantity.
        APPEND ls_ext_mat_comp-mat_comp TO et_rev_mat_comp_var.
      ENDIF.

    ENDIF.

    CLEAR ls_ext_mat_comp.

  ENDSELECT.

  IF p_lock EQ 'X'.
* ... Unlock the order
    CALL FUNCTION 'DEQUEUE_E_PPC_ORDERID'
         EXPORTING
              mode_ppc_ord_inf = 'E'
              mandt            = sy-mandt
              orderid          = if_orderid
              _scope           = '1'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  write_output
*&---------------------------------------------------------------------*
FORM write_output  USING    pf_matnr     TYPE matnr
                            pf_werks     TYPE werks_d
                            pf_accassobj TYPE ppc_accassobj_int
                            pf_objnr     TYPE j_objnr
                            pf_eonly     TYPE c
                            pt_openorder LIKE gt_closedorder
                            pt_finaltb   LIKE gt_finaltb
                            pt_stats     LIKE gt_stats.

  DATA:
    ls_finaltb LIKE gs_finaltb.

  SKIP.
  WRITE: / 'Assembly:', pf_matnr COLOR 5 INVERSE INTENSIFIED,
         / 'Plant:   ', pf_werks COLOR 5 INVERSE INTENSIFIED,
         / 'Acc. obj:', pf_accassobj COLOR 5 INVERSE INTENSIFIED,
         / 'Objnr:   ', pf_objnr COLOR 5 INVERSE INTENSIFIED.
  SKIP.

  PERFORM write_log USING pt_stats '1'.

  SKIP.
  ULINE AT 1(126). FORMAT INTENSIFIED COLOR 4.
  WRITE: /1 '|', 3 'matnr', 20 'f_objnr',
         50 'quant', 68 'istmn', 88 'gmper',
         107 'gmsum', 113 '|', 120 'delta', 126 '|'.
  NEW-LINE.
  ULINE AT 1(126). FORMAT INTENSIFIED COLOR OFF.

  FORMAT INTENSIFIED OFF.
  LOOP AT pt_finaltb INTO ls_finaltb.
    IF NOT pf_eonly IS INITIAL AND ls_finaltb-status IS INITIAL.
      CONTINUE.
    ENDIF.
    CASE ls_finaltb-status.
      WHEN 'P' OR 'Y'.
        FORMAT COLOR 3.
      WHEN 'C'.
        FORMAT COLOR 7.
      WHEN 'X' OR 'F'.
        FORMAT COLOR 6.
      WHEN 'S'.
        FORMAT COLOR 5.
      WHEN OTHERS.
        "nothing
    ENDCASE.
    WRITE: /3 ls_finaltb-matnr, 20 ls_finaltb-f_objnr,
           36 ls_finaltb-quant, 56 ls_finaltb-istmn,
           76 ls_finaltb-gmper, 95 ls_finaltb-gmsum.
    FORMAT COLOR OFF.
    CASE ls_finaltb-status.
      WHEN 'F'.
        gf_icon = icon_red_light.
        FORMAT HOTSPOT ON.
        WRITE: 115 gf_icon AS ICON.
        FORMAT HOTSPOT OFF.
      WHEN 'X' OR 'C'.
        gf_icon = icon_yellow_light.
        FORMAT HOTSPOT ON.
        WRITE: 115 gf_icon AS ICON.
        FORMAT HOTSPOT OFF.
      WHEN 'S'.
        gf_icon = icon_green_light.
        WRITE: 115 gf_icon AS ICON.
    ENDCASE.
    WRITE: 120 ls_finaltb-delta LEFT-JUSTIFIED.
    MOVE-CORRESPONDING ls_finaltb TO gs_hidtab.
    HIDE gs_hidtab.
  ENDLOOP.

  CLEAR gs_hidtab.

  SKIP.
  ULINE AT 1(70). NEW-LINE.
  FORMAT INTENSIFIED COLOR 4.
  WRITE:  '|', 'Legend', 70 '|'.
  FORMAT INTENSIFIED COLOR OFF.
  NEW-LINE. ULINE AT 1(70).
  FORMAT INTENSIFIED OFF.
  WRITE: / '|', 5 'F_OBJNR error  ' COLOR 3,
      'Could not find OBJNR for the component'(l01), 70 '|'.
  WRITE: / '|', 5 'CPZP error     ' COLOR 7,
      'A WIP entry without reference to an open order'(l02), 70 '|'.
  WRITE: / '|', 5 'WIP value error' COLOR 6,
      'The WIP is not consistent with backflushes'(l03), 70 '|'.
  WRITE: / '|', 5 'Error corrected' COLOR 5,
      'WIP inconsistency was corrected - CPZP adjusted', 70 '|'.
  NEW-LINE. ULINE AT 1(70).

ENDFORM.                    " write_output


*&---------------------------------------------------------------------*
*&      Form  write_log
*&---------------------------------------------------------------------*
FORM write_log  USING  pt_stats LIKE gt_stats
                       pf_level TYPE c.


  IF pf_level NE '1'.
    SKIP 1.
    ULINE (25).
    WRITE: / '| DETAILED INFO:', 25 '|'.
    ULINE /(25).
    SKIP 1.
  ENDIF.

* write orders
  READ TABLE pt_stats WITH KEY kkey = 'A' INTO gs_stats.
  WRITE: / 'Open orders found: ',
           gs_stats-val1 LEFT-JUSTIFIED.
  HIDE gs_stats.
* write orders
  READ TABLE pt_stats WITH KEY kkey = 'B' INTO gs_stats.
  WRITE:  'Number of aggregated components checked: ',
           gs_stats-val1 LEFT-JUSTIFIED.
  HIDE gs_stats.

* write detailed timestamps
  CHECK pf_level NE '1'.
  SKIP 2.
  LOOP AT pt_stats INTO gs_stats WHERE kkey = 'R'.
    SUBTRACT gs_stats-val1 FROM gs_stats-val2.
    WRITE: / gs_stats-text, gs_stats-val2, 'milisecs.'.
  ENDLOOP.

ENDFORM.                    " write_log



*CCC
*&---------------------------------------------------------------------*
*&      Form  enqueue
*&---------------------------------------------------------------------*
FORM enqueue .

  CHECK p_lock EQ 'X'.

* LOCK SYNC. PPC PROCESS
  CALL FUNCTION 'ENQUEUE_E_PPC_ORDERID'
       EXPORTING
            mode_ppc_ord_inf = 'E'
            mandt            = sy-mandt
            _scope           = '2'
            _wait            = 'X'
       EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
  IF sy-subrc NE 0.
*    MESSAGE S507(PPC1DM) WITH SY-MSGV1 .
    WRITE:/ 'Backflush currently locked by user',sy-msgv1.
    LEAVE PROGRAM.

  ENDIF.
* LOCK PPCGO RUN
  CALL FUNCTION 'ENQUEUE_E_PPC_CONF_MAT'
       EXPORTING
            mode_ppc_conf_mat = 'E'
            mandt             = sy-mandt
            _scope            = '2'
            _wait             = 'X'
       EXCEPTIONS
            foreign_lock      = 1
            system_failure    = 2
            OTHERS            = 3.
  IF sy-subrc NE 0.
*    MESSAGE S020(PPC1PR) WITH SY-MSGV1 .
    WRITE:/ 'Backflush processing is currently locked by',sy-msgv1.
    LEAVE PROGRAM.

  ENDIF.

ENDFORM.                    " enqueue
*CCC
