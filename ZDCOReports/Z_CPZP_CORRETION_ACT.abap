*---------------------------------------------------------------------*
*
* This program had been copied from Z_CPZP_CORRETION and changed
* for activity correction.
* by IG.MOON 8/20/2007
*
*---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
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

REPORT  z_cpzp_corretion_581661 LINE-SIZE 250
NO STANDARD PAGE HEADING.

TYPE-POOLS icon.
TABLES: ppc_act,ppc_ord_inf, *cpzp.

TYPES BEGIN OF $t_activity_components.
TYPES pname TYPE pvs_pnode.
        INCLUDE STRUCTURE ppc_activity_components.
TYPES END OF $t_activity_components.

DATA $t_act    TYPE TABLE OF $t_activity_components   WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-sl1.
PARAMETERS:
  p_matnr  TYPE matnr,
  p_werks  TYPE werks_d,
  p_aufnr  TYPE aufnr,
  p_ver    TYPE verid.
							
			
*  p_gjper  type co_gjper.

SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-sl2.
PARAMETERS:
  p_debug  TYPE c AS CHECKBOX,
  p_atrep  TYPE c AS CHECKBOX,
  p_lock   TYPE c AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b2.
SELECT-OPTIONS: s_compn FOR ppc_ord_inf-materialnr NO-DISPLAY.
PARAMETERS:
  p_gjper  TYPE co_gjper. " default '2005003'.

PARAMETERS     : P_FORC AS CHECKBOX.

*DATA: p_gjper  type co_gjper.
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
      accassobj TYPE ppc_accassobj_int, " <- new
  END OF gs_order,
*  gt_openorder like table of gs_order,
  gt_closedorder LIKE TABLE OF gs_order,
  gt_varorder  LIKE TABLE OF gs_order.

TYPES :
  BEGIN OF gs_comp,
      pname TYPE pvs_pnode,
      guid TYPE pvs_pnguid,
      unit TYPE ppc_durunit,
      quant TYPE ppc_duration_var,
  END OF gs_comp.
*  gt_comp like hashed table of gs_comp
*        with unique key pname guid unit,
DATA  gt_comp TYPE TABLE OF gs_comp WITH HEADER LINE.

DATA :
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

DATA BEGIN OF gt_act_tb OCCURS 0.
DATA :
    orderid TYPE ppc_orderid,
    headid TYPE ppc_headid_int,
    actid TYPE ppc_actid.

        INCLUDE STRUCTURE ppc_activity_components.

DATA :
    head_sales_doc TYPE kdauf,
    head_sales_doc_item TYPE kdpos,
    head_wbs_elem TYPE ps_psp_pnr,
    pname TYPE pvs_pnode,
    flg_reversal TYPE xflag.
DATA END OF gt_act_tb.

*---------------
LOAD-OF-PROGRAM.
*---------------
  SET RUN TIME CLOCK RESOLUTION LOW.


*-------------------
AT SELECTION-SCREEN.
*-------------------
*  p_gjper  = '2005003'.
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

  PERFORM compute_bfl_qty
                        TABLES   gt_comp
                                 gt_stats
                          USING gf_datum
                                 gt_closedorder
                                 gf_accassobj.. " <- new

  PERFORM update_bfl_cpzp
                         TABLES   gt_comp
                            USING gf_accassobj
                                 p_gjper
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


*  u_break.

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
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*"P45K072474
*            RAISING WRONG_INPUT.                            "P45K072474
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
*    "ups! unprocessed PPCGO
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

*  u_break.
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
*
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
FORM compute_bfl_qty  TABLES   pt_comp      STRUCTURE gt_comp
                               pt_stats     STRUCTURE gs_stats
                      USING    pf_datum     TYPE budat
                               pt_closedorder LIKE gt_closedorder
                   CHANGING    pf_accassobj. " <- new
  .

  DATA:
    ls_order   LIKE gs_order,
    ls_comp    TYPE gs_comp,
    ls_stats   LIKE gs_stats,
    ls_acts    TYPE $t_activity_components,
    lt_repts   TYPE ppc_t_reppoint,
    lt_acts    LIKE ls_acts OCCURS 0,
    lt_actsvar LIKE ls_acts OCCURS 0,
    lt_actsrev LIKE ls_acts OCCURS 0,
    lt_actreva LIKE ls_acts OCCURS 0,
    lf_year(4) TYPE n,
    lf_peri(3) TYPE n,
    lf_datum   TYPE budat,
    v_accassobj TYPE ppc_accassobj_int. " <- new

*  u_break. " <--
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
    PERFORM get_comp_per_order
                            TABLES   lt_acts
                                     lt_actsrev
                                     lt_actsvar
                                     lt_actreva
                               USING ls_order-orderid
                                     pf_datum .

    SORT : lt_acts, lt_actsvar,  lt_actsrev, lt_actreva.


    " collect the issued components
    LOOP AT lt_acts INTO ls_acts.
      ls_comp-pname = ls_acts-pname.
      ls_comp-guid = ls_acts-ressource_guid.
      ls_comp-unit = ls_acts-durunit.
      ls_comp-quant = ls_acts-duration_var.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.
    " subtract the variance values,
    LOOP AT lt_actsvar INTO ls_acts.
      ls_comp-pname = ls_acts-pname.
      ls_comp-guid = ls_acts-ressource_guid.
      ls_comp-unit = ls_acts-durunit.
      ls_comp-quant = ( -1 ) * ls_acts-duration_var.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.
    " subtract the reversed GI's
    LOOP AT lt_actsrev INTO ls_acts.
      ls_comp-pname = ls_acts-pname.
      ls_comp-guid = ls_acts-ressource_guid.
      ls_comp-unit = ls_acts-durunit.
      ls_comp-quant = ( -1 ) * ls_acts-duration_var.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.
    " add the variance reversed values,
    LOOP AT lt_actreva INTO ls_acts.
      ls_comp-pname = ls_acts-pname.
      ls_comp-guid = ls_acts-ressource_guid.
      ls_comp-unit = ls_acts-durunit.
      ls_comp-quant = ls_acts-duration_var.
      COLLECT ls_comp INTO pt_comp.
    ENDLOOP.

  ENDLOOP.

  SORT pt_comp BY pname guid.

*  u_break.
*  loop at gt_act_tb.
*  endloop.
*

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
FORM update_bfl_cpzp  TABLES    pt_comp STRUCTURE gt_comp
                      USING     pf_accassobj TYPE ppc_accassobj_int
                                pf_gjper     TYPE co_gjper
                       CHANGING pf_objnr     TYPE j_objnr
                                pt_finaltb   LIKE gt_finaltb
                                pt_stats     LIKE gt_stats.

  DATA:
    lf_aufnr   TYPE aufnr,
    lf_kaln1   TYPE ck_kalnr1,
    lf_fobjnr  TYPE f_objnr,
    lf_quant   TYPE ist_menge,
    ls_comp    TYPE gs_comp,
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

* Assuming the prev period

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

*  u_break.
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
        AND gjper = pf_gjper
        AND f_objnr LIKE 'KL%'.

*.....get prev period cpzp value for the calculation of istmn
* ... and buffer cpzp table
  SELECT * FROM cpzp INTO TABLE lt_cpzp_prv
      WHERE objnr = pf_objnr
        AND gjper = iv_prvper
        AND f_objnr LIKE 'KL%'.

* Build final table
  LOOP AT pt_comp INTO ls_comp.

*   ... write already determined values
    ls_finaltb-matnr = ls_comp-guid.
    ls_finaltb-quant = ls_comp-quant.

    PERFORM get_lf_fobjnr USING ls_comp-pname
                          CHANGING lf_fobjnr.

*     ... find cpzp data
    READ TABLE lt_cpzp INTO ls_cpzp
            WITH KEY objnr   = pf_objnr
                     f_objnr = lf_fobjnr.
    IF sy-subrc EQ 0.
      IF ls_cpzp-meinh NE ls_comp-unit.
*        MESSAGE S001(00) WITH 'Uom problem'.
        WRITE:/ 'Uom problem',ls_comp-guid.

*                       ls_comp-matnr.
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
        nistmn =  ls_cpzp-gmper.
      ENDIF.

      IF ls_cpzp-istmn <> nistmn OR ls_cpzp-gmsum <> ls_comp-quant.

        	WRITE: / 'Assembly:', p_matnr COLOR 5 INVERSE INTENSIFIED,
*                 / 'Plant:   ', p_werks COLOR 5 INVERSE INTENSIFIED,
*                / 'Acc. obj:', pf_accassobj COLOR 5 INVERSE INTENSIFIED
*,
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
        WRITE:/ 'The component is consistent:',
        ls_comp-pname COLOR 6 INVERSE INTENSIFIED.

      ENDIF.
      IF NOT ls_finaltb IS INITIAL.
        APPEND ls_finaltb TO pt_finaltb.
        CLEAR ls_finaltb.
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
        TABLES  et_activity_components STRUCTURE $t_act
                et_rev_act_components  STRUCTURE $t_act
                et_act_comp_var        STRUCTURE $t_act
                et_rev_act_comp_var    STRUCTURE $t_act
         USING  if_orderid  TYPE ppc_orderid
                if_postdate TYPE budat.

  CONSTANTS c_gmove_ind_0 TYPE ppc_gmove_ind VALUE '0'.
  DATA:
    BEGIN OF ls_ext_act_comp,
      act_comp LIKE $t_act,
      head_sales_doc TYPE kdauf,
      head_sales_doc_item TYPE kdpos,
      head_wbs_elem TYPE ps_psp_pnr,
      flg_reversal TYPE xflag,
    END OF ls_ext_act_comp.


* ... Initialize output
  REFRESH et_activity_components.
  REFRESH et_rev_act_components.
  REFRESH et_act_comp_var.
  REFRESH et_rev_act_comp_var.

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
*      MESSAGE S550(PPC1DM)." RAISING LOCK_ERROR.
      WRITE:/
      'No components can be determined as simultaneous backflush'.

      LEAVE PROGRAM.

    ELSEIF sy-subrc GT 2.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*            RAISING LOCK_ERROR.
      WRITE:/ sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.

      LEAVE PROGRAM.

    ENDIF.

  ENDIF.

  DATA : lt_act_tb LIKE gt_act_tb OCCURS 0 WITH HEADER LINE,
         lt_act_tb_var LIKE gt_act_tb OCCURS 0 WITH HEADER LINE.

* ... Select components
  SELECT
    t1~orderid
    t1~headid
    t4~actid
    t1~reppoint AS reppoint
    t2s~resource_guid AS ressource_guid
    SUM( t4~duration_var ) AS duration_var
    t4~durunit AS durunit
    t1~kdauf AS head_sales_doc
    t1~kdpos AS head_sales_doc_item
    t1~pspnr AS head_wbs_elem
    t6~pname AS pname
    t1~flg_reversal AS flg_reversal
    INTO CORRESPONDING FIELDS OF TABLE lt_act_tb
  FROM
     ( ( ppc_ord_inf AS t2
        INNER JOIN ppc_head AS t1
          ON t1~orderid = t2~orderid )
      INNER JOIN ppc_conf_act AS t4
          ON t4~headid = t1~headid )
      INNER JOIN ppc_act AS t2s
          ON t2s~actid = t4~actid
      INNER JOIN pnodid AS t6
          ON t6~pnguid = t2s~resource_guid

  WHERE
        t1~orderid EQ if_orderid
    AND t1~postdate LE if_postdate
    AND t2~materialnr IN s_compn
    AND t2~ordernr NE space
  GROUP by
      t1~orderid
      t1~headid
      t4~actid
      t1~reppoint
      t2s~resource_guid
      t4~durunit
      t1~kdauf
      t1~kdpos
      t1~pspnr
      t6~pname
      t1~flg_reversal
      %_HINTS ORACLE '&SUBSTITUTE LITERALS&'.

*  u_break.

  LOOP AT lt_act_tb.
    MOVE-CORRESPONDING lt_act_tb TO ls_ext_act_comp-act_comp.
*   Build output tables: first check reversal flag
    IF lt_act_tb-flg_reversal IS INITIAL.
*     Direct posting
      APPEND ls_ext_act_comp-act_comp TO et_activity_components.
    ELSE.
*     Reversal
      APPEND ls_ext_act_comp-act_comp TO et_rev_act_components.
    ENDIF.
    CLEAR ls_ext_act_comp .
  ENDLOOP.

  APPEND LINES OF lt_act_tb TO gt_act_tb.

***  select
***    t1~orderid
***    t1~headid
***    t4~actid
***    t1~reppoint as reppoint
***    t2s~resource_guid as ressource_guid
***    sum( t4~duration_var ) as duration_var
***    t4~durunit as durunit
***    t1~kdauf as head_sales_doc
***    t1~kdpos as head_sales_doc_item
***    t1~pspnr as head_wbs_elem
***    t6~pname as pname
***    t1~flg_reversal as flg_reversal
***    into corresponding fields of table lt_act_tb_var
***  from
***     ( ( ppc_ord_inf as t2
***        inner join ppc_head as t1
***          on t1~orderid = t2~orderid )
***      inner join ppc_conf_act as t4
***          on t4~headid = t1~headid )
***      inner join ppc_act as t2s
***          on t2s~actid = t4~actid
***      inner join pnodid as t6
***          on t6~pnguid = t2s~resource_guid
***
***  where
***        t1~orderid eq if_orderid
***    and t1~postdate le if_postdate
***    and t2~materialnr in s_compn
***    and t2~ordernr eq space
***  group by
***      t1~orderid
***      t1~headid
***      t4~actid
***      t1~reppoint
***      t2s~resource_guid
***      t4~durunit
***      t1~kdauf
***      t1~kdpos
***      t1~pspnr
***      t6~pname
***      t1~flg_reversal
***      %_hints oracle '&SUBSTITUTE LITERALS&'.
***
***  loop at lt_act_tb_var.
***    move-corresponding lt_act_tb_var to ls_ext_act_comp-act_comp.
****   Build output tables: first check reversal flag
***    if lt_act_tb_var-flg_reversal is initial.
****     Direct posting
***      append ls_ext_act_comp-act_comp to :
****                          et_activity_components,
***                          et_act_comp_var.
***
***    else.
****     Reversal
***      append ls_ext_act_comp-act_comp to :
****                          et_rev_act_components,
***                          et_rev_act_comp_var.
***    endif.
***    clear ls_ext_act_comp .
***  endloop.
***
***  append lines of lt_act_tb_var to gt_act_tb.

* ... Unlock the order

  IF p_lock EQ 'X'. " lock when only on update mode

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
    WRITE:/ 'Backflush currently locked by user', sy-msgv1 .

    LEAVE PROGRAM.

  ENDIF.

* ////////////////////////////////////// *

* LOCK PPCGO RUN
  CALL FUNCTION 'ENQUEUE_EZ_PPC_CONF_ACT'
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
*&---------------------------------------------------------------------*
*&      Form  get_lf_fobjnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_COMP_PNAME  text
*      <--P_LF_FOBJNR  text
*----------------------------------------------------------------------*
FORM get_lf_fobjnr USING    p_ls_comp_pname
                   CHANGING p_lf_fobjnr.

  DATA $pname TYPE pvs_pnode.
  DATA $fobjnr TYPE f_objnr.

  CONCATENATE
  'KLH201'
  p_ls_comp_pname(6)
  '^^^^'
  p_ls_comp_pname+7(3) '_HR'
  INTO $fobjnr.

  REPLACE '^^^^' WITH '    ' INTO $fobjnr.

  p_lf_fobjnr = $fobjnr.
ENDFORM.                    " get_lf_fobjnr
