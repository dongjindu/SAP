************************************************************************
* Program Name      : ZAPP_DELIVERY_HMC
* Creation Date     : 01/30/2008
* Development Request No :
* Addl Documentation:
* Description       : Send Delivery Sum to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zapp_delivery_hmc NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.

TYPE-POOLS: slis, vrm.
DATA: it_data LIKE TABLE OF ztpp_deliver_hmc WITH HEADER LINE.
DATA: it_data_inf LIKE TABLE OF zspp_deliver_hmc WITH HEADER LINE.
CONSTANTS: c_dest(10) VALUE 'WMHR01',
** Changed by Furong on 07/23/10
*           C_CG_DATE TYPE D VALUE '20090531'.
           c_cg_date TYPE d VALUE '20100731'.
** End of change on 07/23/10
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_datum LIKE sy-datum.
*PARAMETERS: P_REC AS CHECKBOX.
PARAMETERS: p_snt AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM send_write_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA:  lt_actual LIKE TABLE OF ztpp_prod_actual WITH HEADER LINE,
         lt_bf_stock LIKE TABLE OF ztpp_deliver_hmc WITH HEADER LINE,
*         LT_DAY LIKE TABLE OF ZTPP_DELIVER_HMC WITH HEADER LINE,
*         LT_YEAR LIKE TABLE OF ZTPP_DELIVER_HMC WITH HEADER LINE,
*         LT_TEMP1 LIKE ZTPP_DELIVER_HMC,
*         LT_SUM LIKE TABLE OF ZTPP_DELIVER_HMC WITH HEADER LINE,
         lt_plan  LIKE TABLE OF ztpp_month_plan WITH HEADER LINE.

  DATA: BEGIN OF lt_temp OCCURS 0.
          INCLUDE STRUCTURE ztpp_deliver_hmc.
  DATA: qty_vpcout LIKE  ztpp_prod_actual-qty_vpcout,
        qty_signoff LIKE  ztpp_prod_actual-qty_signoff,
                qty_shipin LIKE  ztpp_prod_actual-qty_shipin,
        qty_shipout LIKE  ztpp_prod_actual-qty_shipout.
  DATA: END OF lt_temp.

  DATA:  lt_day LIKE TABLE OF lt_temp WITH HEADER LINE,
         lt_year LIKE TABLE OF lt_temp WITH HEADER LINE,
         lt_sum LIKE TABLE OF lt_temp WITH HEADER LINE.

  DATA: BEGIN OF lt_natn_model_temp OCCURS 0,
        natn LIKE ztpp_deliver_hmc-dist,
        model LIKE ztpp_deliver_hmc-model,
        END OF lt_natn_model_temp.
  DATA: lt_natn_model LIKE TABLE OF lt_natn_model_temp WITH HEADER LINE.

  DATA: BEGIN OF lt_yr_stock OCCURS 0,
        natn LIKE ztpp_prod_actual-natn,
        model LIKE ztpp_prod_actual-model,
        qty_cgate LIKE ztpp_deliver_hmc-qty_dd,
        qty_shipin LIKE ztpp_deliver_hmc-qty_dd,
        qty_shipout LIKE ztpp_deliver_hmc-qty_dd,
        qty_signoff LIKE ztpp_deliver_hmc-qty_dd,
        qty_vpcout LIKE ztpp_deliver_hmc-qty_dd,
        qty_mgate LIKE ztpp_deliver_hmc-qty_dd,
        s_disposal LIKE ztpp_deliver_hmc-qty_dd,
        v_disposal LIKE ztpp_deliver_hmc-qty_dd,
* by Daniel on 11/04/10 {
        h_disposal LIKE ztpp_deliver_hmc-qty_dd,
* }
        END OF lt_yr_stock.

  DATA: BEGIN OF lt_shipin_temp OCCURS 0,
        model LIKE ztpp_prod_actual-model,
        dest LIKE ztpp_prod_actual-dest,
        natn LIKE ztpp_prod_actual-natn,
        bmdl LIKE ztpp_prod_actual-bmdl,
        ocn LIKE ztpp_prod_actual-ocn,
        extc LIKE ztpp_prod_actual-extc,
        intc LIKE ztpp_prod_actual-intc,
        qty_shipin LIKE ztpp_deliver_hmc-qty_dd,
        qty_shipout LIKE ztpp_deliver_hmc-qty_dd,
        END OF lt_shipin_temp.
  DATA : lt_shipin LIKE TABLE OF lt_yr_stock WITH HEADER LINE.

  DATA : lt_yr_stock_tmp LIKE TABLE OF lt_yr_stock WITH HEADER LINE.


* DATA : LT_YR_STOCK_05312009 LIKE TABLE OF LT_YR_STOCK WITH HEADER LINE
*       ,
*        LT_YR_STOCK_06012009 LIKE TABLE OF LT_YR_STOCK WITH HEADER LINE
*       .
  DATA : lt_yr_stock_07312010 LIKE TABLE OF lt_yr_stock WITH HEADER LINE,
         lt_yr_stock_08012010 LIKE TABLE OF lt_yr_stock WITH HEADER LINE.

  DATA: l_day_bf LIKE sy-datum,
        l_qty_mm LIKE ztpp_deliver_hmc-qty_dd,
        l_qty_yy LIKE ztpp_deliver_hmc-qty_dd,
        l_day_year LIKE sy-datum,
        l_day_month LIKE sy-datum,
        l_month(2),
        l_year(4),
*        l_emf_qty LIKE ztpp_deliver_hmc-qty_dd,
*        l_cra_qty LIKE ztpp_deliver_hmc-qty_dd,
*        l_tcf_qty LIKE ztpp_deliver_hmc-qty_dd,
** by ig.moon 8/3/2009 {
*        l_inf_qty LIKE ztpp_deliver_hmc-qty_dd,
* }
        l_model(3).
  DATA: l_dist LIKE lt_natn_model-natn.

** 11/04/13
  DATA: BEGIN OF lt_model_qty OCCURS 0,
       model LIKE ztpp_prod_actual-model,
       zmpqty LIKE ztpp_deliver_hmc-qty_dd,
       END OF lt_model_qty.
** End

  CONCATENATE p_datum+0(4) '0101' INTO l_day_year.
  CONCATENATE p_datum+0(6) '01' INTO l_day_month.
  l_day_bf = p_datum - 1.
  l_month = p_datum+4(2).
  l_year = p_datum+0(4).

*  SELECT * INTO TABLE LT_NATN_MODEL
*  FROM ZTPP_NATN_MODEL.
  SELECT natn model INTO TABLE lt_natn_model_temp
    FROM ztpp_prod_actual
    GROUP BY natn model.
  LOOP AT lt_natn_model_temp.
    lt_natn_model = lt_natn_model_temp.
    lt_natn_model-natn = lt_natn_model_temp+0(3).
    COLLECT lt_natn_model.
  ENDLOOP.

  SELECT * INTO TABLE lt_actual
           FROM ztpp_prod_actual
          WHERE prdt_date BETWEEN l_day_year AND p_datum.

  SELECT * INTO TABLE lt_bf_stock
           FROM ztpp_deliver_hmc
           WHERE prdt_date = l_day_bf.

  SELECT gjahr model SUM( zmpqty ) AS zmpqty
     INTO CORRESPONDING FIELDS OF TABLE lt_plan
           FROM ztpp_month_plan
          WHERE gjahr = l_year
            AND ptype = '0'
          GROUP BY gjahr model.

** By Fuorng on 11/04/13
*  SELECT SINGLE zmpqty INTO l_emf_qty
*           FROM ztpp_month_plan
*          WHERE gjahr = l_year
*            AND model = 'EMF'
*            AND ptype = '0'
*            AND zmonth = l_month.
*
*  SELECT SINGLE zmpqty INTO l_cra_qty
*          FROM ztpp_month_plan
*         WHERE gjahr = l_year
*           AND model = 'CRA'
*           AND ptype = '0'
*           AND zmonth = l_month.
*
*  SELECT SINGLE zmpqty INTO l_tcf_qty
*           FROM ztpp_month_plan
*          WHERE gjahr = l_year
*            AND model = 'TCF'
*            AND ptype = '0'
*            AND zmonth = l_month.
*
*
** by ig.moon 8/3/2009 {
*  SELECT SINGLE zmpqty INTO l_inf_qty
*          FROM ztpp_month_plan
*         WHERE gjahr = l_year
*           AND model = 'INF'
*           AND ptype = '0'
*           AND zmonth = l_month.
* }
  SELECT model SUM( zmpqty )
   INTO CORRESPONDING FIELDS OF TABLE lt_model_qty
   FROM ztpp_month_plan
  WHERE gjahr = l_year
    AND ptype = '0'
    AND zmonth = l_month
  GROUP BY model.
** End on 11/04/13

*** Changed by Furong on 07/29/09
*** LT_YR_STOCK_05312009
*  SELECT NATN MODEL SUM( QTY_CGATE ) SUM( QTY_SHIPOUT )
*          SUM( QTY_SIGNOFF ) SUM( QTY_VPCOUT ) "AS QTY
*      INTO TABLE LT_YR_STOCK_TMP
*          FROM ZTPP_PROD_ACTUAL
*          WHERE PRDT_DATE <= C_CG_DATE
*          GROUP BY NATN MODEL.
*
*  LOOP AT LT_YR_STOCK_TMP.
*    IF LT_YR_STOCK_TMP-NATN+3(2) = 'AA' OR
*       LT_YR_STOCK_TMP-NATN+3(2) = 'AB'.
*      LT_YR_STOCK_05312009 = LT_YR_STOCK_TMP.
*      LT_YR_STOCK_05312009-NATN = LT_YR_STOCK_TMP-NATN+0(3).
*      COLLECT LT_YR_STOCK_05312009.
*    ENDIF.
*    CLEAR: LT_YR_STOCK_05312009, LT_YR_STOCK_TMP.
*  ENDLOOP.
*  REFRESH LT_YR_STOCK_TMP.

*** LT_YR_STOCK_06012009
*  SELECT NATN MODEL SUM( QTY_CGATE ) SUM( QTY_SHIPOUT )
*          SUM( QTY_SIGNOFF ) SUM( QTY_VPCOUT ) "AS QTY
*      INTO TABLE LT_YR_STOCK_TMP
*          FROM ZTPP_PROD_ACTUAL
*          WHERE PRDT_DATE BETWEEN C_CG_DATE AND P_DATUM
*          GROUP BY NATN MODEL.
*
*  LOOP AT LT_YR_STOCK_TMP.
*    IF LT_YR_STOCK_TMP-NATN+3(2) = 'AA' OR
*       LT_YR_STOCK_TMP-NATN+3(2) = 'AB'.
*      LT_YR_STOCK_06012009 = LT_YR_STOCK_TMP.
*      LT_YR_STOCK_06012009-NATN = LT_YR_STOCK_TMP-NATN+0(3).
*      COLLECT LT_YR_STOCK_06012009.
*    ENDIF.
*    CLEAR: LT_YR_STOCK_06012009, LT_YR_STOCK_TMP.
*  ENDLOOP.
*  REFRESH LT_YR_STOCK_TMP.
*** End of change


** Changed by Furong on 07/27/10
** LT_YR_STOCK_07312010
  SELECT natn model SUM( qty_cgate ) AS qty_cgate
          SUM( qty_shipout ) AS qty_shipout
          SUM( qty_signoff ) AS  qty_signoff
          SUM( qty_vpcout ) AS qty_vpcout
          SUM( qty_mgate ) AS qty_mgate
      INTO CORRESPONDING FIELDS OF TABLE lt_yr_stock_tmp
          FROM ztpp_prod_actual
          WHERE prdt_date <= c_cg_date
          GROUP BY natn model.

  LOOP AT lt_yr_stock_tmp.
*    IF LT_YR_STOCK_TMP-NATN+3(2) = 'AA' OR
*       LT_YR_STOCK_TMP-NATN+3(2) = 'AB'.
    IF lt_yr_stock_tmp-natn+3(1) = 'A'.
      lt_yr_stock_07312010 = lt_yr_stock_tmp.
      lt_yr_stock_07312010-natn = lt_yr_stock_tmp-natn+0(3).
      COLLECT lt_yr_stock_07312010.
    ENDIF.
    CLEAR: lt_yr_stock_07312010, lt_yr_stock_tmp.
  ENDLOOP.
  REFRESH lt_yr_stock_tmp.

** LT_YR_STOCK_08012010
*  SELECT NATN MODEL SUM( QTY_CGATE ) SUM( QTY_SHIPOUT )
*          SUM( QTY_SIGNOFF ) SUM( QTY_VPCOUT )
*                    SUM( QTY_MGATE ) as QTY_MGATE    "AS QTY
*      INTO TABLE LT_YR_STOCK_TMP
*          FROM ZTPP_PROD_ACTUAL
*          WHERE PRDT_DATE BETWEEN C_CG_DATE AND P_DATUM
*          GROUP BY NATN MODEL.

*QTY_SIGNOFF
*QTY_MM
*QTY_YY
*STOCK
*QTY_BODY
*QTY_PAINT
*QTY_TRIM
*QTY_CGATE
*QTY_VPCOUT
*QTY_SHIPIN
*QTY_SHIPOUT
*QTY_MGATE
  SELECT natn model SUM( qty_signoff )  AS qty_signoff
                    SUM( qty_cgate )  AS qty_cgate
                    SUM( qty_vpcout ) AS qty_vpcout
                    SUM( qty_shipin ) AS qty_shipin
                    SUM( qty_shipout ) AS qty_shipout
                    SUM( qty_mgate ) AS qty_mgate    "AS QTY
       INTO CORRESPONDING FIELDS OF TABLE lt_yr_stock_tmp
           FROM ztpp_prod_actual
           WHERE ( prdt_date > c_cg_date AND prdt_date <= p_datum )
           GROUP BY natn model.


  LOOP AT lt_yr_stock_tmp.
*    IF LT_YR_STOCK_TMP-NATN+3(2) = 'AA' OR
*       LT_YR_STOCK_TMP-NATN+3(2) = 'AB'.
    IF lt_yr_stock_tmp-natn+3(1) = 'A'.
      lt_yr_stock_08012010 = lt_yr_stock_tmp.
      lt_yr_stock_08012010-natn = lt_yr_stock_tmp-natn+0(3).
      COLLECT lt_yr_stock_08012010.
    ENDIF.
    CLEAR: lt_yr_stock_08012010, lt_yr_stock_tmp.
  ENDLOOP.
  REFRESH lt_yr_stock_tmp.
** End of change


*  IF NOT P_REC IS INITIAL.

** Changed by Furong on 05/29/09
*    SELECT NATN MODEL SUM( QTY_CGATE ) SUM( QTY_SHIPOUT ) AS QTY
*       INTO TABLE LT_YR_STOCK_TMP
*           FROM ZTPP_PROD_ACTUAL
*           WHERE PRDT_DATE <= P_DATUM
*           GROUP BY NATN MODEL.


  SELECT natn model SUM( qty_shipin ) AS qty_shipin
                    SUM( qty_shipout ) AS qty_shipout
                    SUM( qty_signoff ) AS qty_signoff
                    SUM( qty_vpcout ) AS qty_vpcout
                    SUM( qty_mgate ) AS qty_mgate
* by ig.moon   {
                    SUM( qty_cgate ) AS qty_cgate
* }
                    SUM( s_disposal ) AS s_disposal
                    SUM( v_disposal ) AS v_disposal
* by Daniel on 11/04 {
                    SUM( h_disposal ) AS h_disposal
* }
    INTO CORRESPONDING FIELDS OF TABLE lt_yr_stock_tmp
        FROM ztpp_prod_actual
        WHERE prdt_date <= p_datum
        GROUP BY natn model.

*  SELECT NATN MODEL SUM( QTY_CGATE ) SUM( QTY_SHIPIN )
*         SUM( QTY_SHIPOUT )
*         SUM( QTY_SIGNOFF ) SUM( QTY_VPCOUT ) "AS QTY
*                  SUM( S_DISPOSAL )
*                   SUM( V_DISPOSAL )
*     INTO TABLE LT_YR_STOCK_TMP
*         FROM ZTPP_PROD_ACTUAL
*         WHERE PRDT_DATE <= P_DATUM
*         GROUP BY NATN MODEL.
** End of change on 05/29/09
  LOOP AT lt_yr_stock_tmp.
** Changed by Furong on 11/29/09
*    IF LT_YR_STOCK_TMP-NATN+3(2) = 'AA' OR
*       LT_YR_STOCK_TMP-NATN+3(2) = 'AB'.
    IF lt_yr_stock_tmp-natn+3(1) <> 'X'.
** End of change
      lt_yr_stock = lt_yr_stock_tmp.
      lt_yr_stock-natn = lt_yr_stock_tmp-natn+0(3).
      COLLECT lt_yr_stock.
    ENDIF.
    CLEAR: lt_yr_stock, lt_yr_stock_tmp.
  ENDLOOP.

** Changed by Furong on 08/11/09 to get shipin data
  SELECT model dest natn bmdl ocn extc intc SUM( qty_shipin )
         SUM( qty_shipout )
     INTO TABLE lt_shipin_temp
         FROM ztpp_prod_actual
         WHERE prdt_date <= p_datum
         GROUP BY model dest natn bmdl ocn extc intc.

  LOOP AT lt_shipin_temp.
    IF lt_shipin_temp-qty_shipin < lt_shipin_temp-qty_shipout.
      lt_shipin_temp-qty_shipin = lt_shipin_temp-qty_shipout.
      MODIFY lt_shipin_temp.
    ENDIF.
    CLEAR: lt_shipin_temp.
  ENDLOOP.

  LOOP AT lt_shipin_temp.
** Changed by Furong on 11/29/09
*    IF LT_SHIPIN_TEMP-NATN+3(2) = 'AA' OR
*       LT_SHIPIN_TEMP-NATN+3(2) = 'AB'.
    IF lt_shipin_temp-natn+3(1) <> 'X'.
** End of change
      MOVE-CORRESPONDING lt_shipin_temp TO lt_shipin.
      lt_shipin-natn = lt_shipin_temp-natn+0(3).
      COLLECT lt_shipin.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_yr_stock.
    READ TABLE lt_shipin WITH KEY natn = lt_yr_stock-natn
                                  model = lt_yr_stock-model.
    IF sy-subrc = 0.
      lt_yr_stock-qty_shipin = lt_shipin-qty_shipin.
    ELSE.
*      clear: LT_YR_STOCK-qty_shipin
    ENDIF.
    MODIFY lt_yr_stock.
  ENDLOOP.
** End of change

  LOOP AT lt_actual.
** Changed by Furong on 11/20/09
*    IF LT_ACTUAL-NATN+3(2) = 'AA' OR
*       LT_ACTUAL-NATN+3(2) = 'AB'.
    IF lt_actual-natn+3(1) <> 'X'.
** End of change
      lt_temp-plant = '5N'.
      lt_temp-prdt_date = lt_actual-prdt_date.
      IF lt_actual-natn+0(3) = 'B28'.
        lt_temp-mkt_class = 'D'.
      ELSE.
        lt_temp-mkt_class = 'E'.
      ENDIF.
** Changed by Furong on 02/12/08
*     LT_TEMP-DIST = LT_ACTUAL-NATN.
      lt_temp-dist = lt_actual-natn+0(3).

** End of change
      IF lt_actual-bmdl+2(1) = 'S'.
        lt_temp-veh_class = 'P'.
      ELSE.
        lt_temp-veh_class = 'R'.
      ENDIF.

*      IF LT_ACTUAL-MODEL = 'EMF' OR LT_ACTUAL-MODEL = 'EM'.
*        LT_TEMP-VEH_CLASS = 'P'.
** by ig.moon 8/3/2009 {
*      ELSEIF LT_ACTUAL-MODEL = 'INF' OR LT_ACTUAL-MODEL = 'IN'.
*        LT_TEMP-VEH_CLASS = 'P'.
** }
*      ELSE.
*        LT_TEMP-VEH_CLASS = 'R'.
*      ENDIF.

      lt_temp-model = lt_actual-model.
** Changed by Furong on 02/21/08
*      LT_TEMP-QTY_DD = LT_ACTUAL-QTY_SHIPOUT.
      IF lt_actual-natn+0(3) = 'B28'.
*** Changed by Furong on 07/29/09

*        IF  P_DATUM <= C_CG_DATE.
*          LT_TEMP-QTY_DD = LT_ACTUAL-QTY_CGATE.
*        ELSE.
*          LT_TEMP-QTY_DD = LT_ACTUAL-QTY_VPCOUT.
*        ENDIF.

*** Changed by Furong on 07/23/10

        IF  p_datum <= c_cg_date.
          lt_temp-qty_dd = lt_actual-qty_cgate.
        ELSE.
*          LT_TEMP-QTY_DD = LT_ACTUAL-QTY_VPCOUT.
          lt_temp-qty_dd = lt_actual-qty_mgate.
        ENDIF.

*        LT_TEMP-QTY_DD = LT_ACTUAL-QTY_CGATE.
*** End of change on 07/23/10

*** End of change
*        IF LT_ACTUAL-PRDT_DATE <= C_CG_DATE.
*           LT_TEMP-QTY_YY = LT_TEMP-QTY_YY + LT_TEMP-CGATE.
*        ELSE.
*           LT_TEMP-QTY_YY = LT_TEMP-QTY_YY + LT_TEMP-VPCOUT.
*        ENDIF.
      ELSE.
        lt_temp-qty_dd = lt_actual-qty_shipout.
      ENDIF.
** End of change on 02/21/08
      lt_temp-qty_dd_cancel = lt_actual-qty_cgate.
      lt_temp-qty_vpcout = lt_actual-qty_vpcout.
      lt_temp-qty_signoff = lt_actual-qty_signoff.
      lt_temp-qty_shipin = lt_actual-qty_shipin.
      lt_temp-qty_shipout = lt_actual-qty_shipout.

      IF lt_actual-prdt_date = p_datum.
        COLLECT lt_temp INTO lt_day.
      ELSE.
        COLLECT lt_temp INTO lt_year.
      ENDIF.
      CLEAR: lt_temp.
    ENDIF.
  ENDLOOP.

  SORT lt_day BY mkt_class dist veh_class model.
  SORT lt_year BY mkt_class dist veh_class model.

  LOOP AT lt_natn_model.
*  LOOP AT LT_DAY.
    READ TABLE lt_day WITH KEY dist = lt_natn_model-natn
                               model = lt_natn_model-model.
    IF sy-subrc = 0.
      lt_sum = lt_day.
    ELSE.
      lt_day-plant = '5N'.
      lt_day-prdt_date = p_datum.
      IF lt_natn_model-natn+0(3) = 'B28'.
        lt_day-mkt_class = 'D'.
      ELSE.
        lt_day-mkt_class = 'E'.
      ENDIF.
      lt_day-dist = lt_natn_model-natn.

      IF lt_natn_model-model = 'EMF' OR lt_natn_model-model = 'EM'.
        lt_day-veh_class = 'P'.
* by ig.moon 8/3/2009 {
      ELSEIF lt_natn_model-model = 'INF' OR lt_natn_model-model = 'IN'.
        lt_day-veh_class = 'P'.
* }
      ELSEIF lt_natn_model-model = 'TCF' OR lt_natn_model-model = 'TC'.
        lt_day-veh_class = 'P'.
** By Furong on 11/04/13
      ELSEIF lt_natn_model-model = 'C2F' OR lt_natn_model-model = 'C2'.
        lt_day-veh_class = 'P'.
** End
      ELSE.
        lt_day-veh_class = 'R'.
      ENDIF.
      lt_day-model = lt_natn_model-model.

      lt_sum = lt_day.

    ENDIF.
*    LOOP AT LT_YEAR WHERE MKT_CLASS = LT_DAY-MKT_CLASS
*                      AND DIST = LT_DAY-DIST
*                      AND VEH_CLASS = LT_DAY-VEH_CLASS
*                      AND MODEL = LT_DAY-MODEL.
*      IF LT_YEAR-PRDT_DATE+4(2) = L_MONTH.
*        L_QTY_MM = L_QTY_MM + LT_YEAR-QTY_DD.
*      ENDIF.
*      L_QTY_YY = L_QTY_YY + LT_YEAR-QTY_DD.
*    ENDLOOP.
    LOOP AT lt_actual.
** Changed by Furong on 11/20/09
*     IF LT_ACTUAL-NATN+3(2) = 'AA' OR
*        LT_ACTUAL-NATN+3(2) = 'AB'.
      IF lt_actual-natn+3(1) <> 'X'.
** End of change
        l_dist = lt_actual-natn+0(3).

        l_model = lt_actual-model+0(2).
        IF l_dist = lt_natn_model-natn
           AND l_model = lt_natn_model-model.
          IF l_dist = 'B28'.

*** Changed by Furong on 07/29/09
*            IF LT_ACTUAL-PRDT_DATE <= C_CG_DATE.
*              L_QTY_YY = L_QTY_YY + LT_ACTUAL-QTY_CGATE.
*            ELSE.
*              L_QTY_YY = L_QTY_YY + LT_ACTUAL-QTY_VPCOUT.
*            ENDIF.
*            IF LT_ACTUAL-PRDT_DATE+4(2) = L_MONTH.
*              IF LT_ACTUAL-PRDT_DATE <= C_CG_DATE.
*                L_QTY_MM = L_QTY_MM + LT_ACTUAL-QTY_CGATE.
*              ELSE.
*                L_QTY_MM = L_QTY_MM + LT_ACTUAL-QTY_VPCOUT.
*              ENDIF.
*            ENDIF.
** Changed by Furong on 07/23/10
            IF lt_actual-prdt_date <= c_cg_date.
              l_qty_yy = l_qty_yy + lt_actual-qty_cgate.
            ELSE.
*              L_QTY_YY = L_QTY_YY + LT_ACTUAL-QTY_VPCOUT.
              l_qty_yy = l_qty_yy + lt_actual-qty_mgate.
            ENDIF.
            IF lt_actual-prdt_date+4(2) = l_month.
              IF lt_actual-prdt_date <= c_cg_date.
                l_qty_mm = l_qty_mm + lt_actual-qty_cgate.
              ELSE.
*                L_QTY_MM = L_QTY_MM + LT_ACTUAL-QTY_VPCOUT.
                l_qty_mm = l_qty_mm + lt_actual-qty_mgate.
              ENDIF.
            ENDIF.
*
*            L_QTY_YY = L_QTY_YY + LT_ACTUAL-QTY_CGATE.
*            IF LT_ACTUAL-PRDT_DATE+4(2) = L_MONTH.
*              L_QTY_MM = L_QTY_MM + LT_ACTUAL-QTY_CGATE.
*            ENDIF.
*** End of change on 07/23/10
*** End of change on 07/29/09
          ELSE.    "B06
            l_qty_yy = l_qty_yy + lt_actual-qty_shipout.
            IF lt_actual-prdt_date+4(2) = l_month.
              l_qty_mm = l_qty_mm + lt_actual-qty_shipout.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    lt_sum-qty_yy = l_qty_yy.
    lt_sum-qty_mm = l_qty_mm.
*    LT_SUM-QTY_MM = L_QTY_MM + LT_DAY-QTY_DD.
*    LT_SUM-QTY_YY = L_QTY_YY + LT_DAY-QTY_DD.

    SELECT SINGLE model INTO l_model
      FROM ztpp_model_conv
      WHERE bmdl = lt_day-model.

*    IF LT_DAY-MODEL = 'EM'.
*      L_MODEL ='EMF'.
** by ig.moon 8/3/2009 {
*    ELSEIF LT_DAY-MODEL = 'IN'.
*      L_MODEL ='INF'.
** }
*    ELSE.
*      L_MODEL ='CRA'.
*    ENDIF.


*    IF P_REC IS INITIAL.
*      READ TABLE LT_BF_STOCK WITH KEY MKT_CLASS = LT_DAY-MKT_CLASS
*                                      DIST = LT_DAY-DIST
*                                      VEH_CLASS = LT_DAY-VEH_CLASS
*                                      MODEL = LT_DAY-MODEL.
*** Changed by Furong on 05/29/09
**      IF SY-SUBRC = 0.
**        LT_SUM-STOCK = LT_BF_STOCK-STOCK + LT_SUM-QTY_DD_CANCEL
**                       - LT_SUM-QTY_DD.
*
*      IF SY-SUBRC = 0.
*        LT_SUM-STOCK = LT_BF_STOCK-STOCK.
*        LT_SUM-PLANT_STOCK = LT_BF_STOCK-PLANT_STOCK.
*        LT_SUM-SALE_STOCK = LT_BF_STOCK-SALE_STOCK.
*        LT_SUM-SIGNOFF_STOCK = LT_BF_STOCK-SIGNOFF_STOCK.
*      ENDIF.
*      LT_SUM-STOCK = LT_SUM-STOCK  + LT_SUM-QTY_DD_CANCEL
*                     - LT_SUM-QTY_DD.
*
*** Plant stock
*      IF LT_SUM-MKT_CLASS = 'D'.
*        IF P_DATUM <= C_CG_DATE.
**          LT_SUM-PLANT_STOCK = LT_BF_STOCK-PLANT_STOCK.
*        ELSE.
*          LT_SUM-PLANT_STOCK = LT_SUM-PLANT_STOCK +
*                      LT_SUM-QTY_DD_CANCEL - LT_SUM-QTY_VPCOUT.
*        ENDIF.
*      ELSE.
*        LT_SUM-PLANT_STOCK = LT_SUM-PLANT_STOCK +
*                    LT_SUM-QTY_DD_CANCEL - LT_SUM-QTY_SHIPOUT.
*      ENDIF.
** sale stock
*      IF LT_SUM-MKT_CLASS = 'D'.
*        IF P_DATUM <= C_CG_DATE.
*          LT_SUM-SALE_STOCK = LT_SUM-SALE_STOCK +
*                   LT_SUM-QTY_DD_CANCEL - LT_SUM-QTY_SHIPOUT.
*        ELSE.
*          LT_SUM-SALE_STOCK = LT_SUM-SALE_STOCK +
*                 LT_SUM-QTY_VPCOUT - LT_SUM-QTY_SHIPOUT.
*        ENDIF.
*      ELSE.
**        LT_SUM-SALE_STOCK = LT_BF_STOCK-SALE_STOCK.
*      ENDIF.
** Sign off stock
*      LT_SUM-SIGNOFF_STOCK =  LT_SUM-SIGNOFF_STOCK +
*                     LT_SUM-QTY_SIGNOFF - LT_SUM-QTY_DD_CANCEL.
** Stock ( signoff - mg)
*      LT_SUM-MG_STOCK = LT_SUM-MG_STOCK + LT_SUM-PLANT_STOCK +
*                LT_SUM-SIGNOFF_STOCK.
*** End of change on 05/29/09

*    ELSE.
    READ TABLE lt_yr_stock WITH KEY natn = lt_day-dist
                                    model = l_model.
    IF sy-subrc = 0.
** Changed by Furong on 08/06/10
** Changed by Furong on 08/09/10

* by Daniel on 11/01/10 {
*     LT_SUM-STOCK = LT_YR_STOCK-QTY_CGATE - LT_YR_STOCK-QTY_SHIPOUT.

* by Daniel on 11/04/10 {
*      LT_SUM-STOCK = LT_YR_STOCK-QTY_CGATE
*                   - LT_YR_STOCK-QTY_SHIPOUT
*                   - LT_YR_STOCK-V_DISPOSAL.

      lt_sum-stock = lt_yr_stock-qty_cgate
                   - lt_yr_stock-qty_shipout
                   - lt_yr_stock-v_disposal
                   - lt_yr_stock-h_disposal.
* }
* }

** End of change  on 08/09/10
** End of change  on 08/06/10

** Changed by Furong on 05/29/09

*** Changed by Furong on 07/29/09
*      READ TABLE LT_YR_STOCK_05312009 WITH KEY NATN = LT_DAY-DIST
*                                       MODEL = L_MODEL.
*
*      READ TABLE LT_YR_STOCK_06012009 WITH KEY NATN = LT_DAY-DIST
*                                       MODEL = L_MODEL.
*** End of change

**************** Plant stock
** Change by Furong on 01/12/10
*      IF LT_SUM-MKT_CLASS = 'D'.
**** Changed by Furong on 07/29/09
**        IF P_DATUM <= C_CG_DATE.
**          CLEAR: LT_SUM-PLANT_STOCK.
**        ELSE.
**          LT_SUM-PLANT_STOCK = LT_YR_STOCK_06012009-QTY_CGATE
**                               - LT_YR_STOCK-QTY_VPCOUT.
**        ENDIF.
**** Changed by Furong on 07/31/09
**        IF LT_YR_STOCK-NATN = 'B28'.
**          LT_SUM-PLANT_STOCK =  LT_YR_STOCK-QTY_CGATE -
**                                LT_YR_STOCK-QTY_VPCOUT.
**        ELSE.
**          LT_SUM-PLANT_STOCK = LT_YR_STOCK-QTY_CGATE  -
**                                LT_YR_STOCK-QTY_SHIPOUT.
**        ENDIF.

** Changed by Furong on 08/05/09 - uncomment the ship in qty
* by ig.moon 8/3/2009 {


*        LT_SUM-PLANT_STOCK =  LT_YR_STOCK-QTY_CGATE -
*                                LT_YR_STOCK-QTY_SHIPIN.
**        LT_SUM-PLANT_STOCK = 0.
** }
*
**** End of change on 07/31/09
**** End of change on 07/29/09
*** End of change on 08/05/09
*      ELSE.
*
*** Changed by Furong on 08/05/09 - BOTH D and E
*        LT_SUM-PLANT_STOCK =  LT_YR_STOCK-QTY_CGATE -
*                                 LT_YR_STOCK-QTY_SHIPIN.
*
** by ig.moon 8/3/2009 {
**        LT_SUM-PLANT_STOCK = LT_YR_STOCK-QTY_CGATE  -
**                               LT_YR_STOCK-QTY_SHIPOUT.
**        LT_SUM-PLANT_STOCK = 0.
** }
*** End of change on 08/05/09
*      ENDIF.

*** Changed by Furong on 08/09/10

* by Daniel on 11/01/10 {
*     LT_SUM-PLANT_STOCK = LT_YR_STOCK-QTY_CGATE  -
** Changed by Furong on 08/09/10
*                               LT_YR_STOCK-QTY_SHIPOUT.
*                            LT_YR_STOCK-QTY_SHIPIN.
** End of change on 08/09/10
      lt_sum-plant_stock = lt_yr_stock-qty_cgate
                         - lt_yr_stock-qty_shipin
                         - lt_yr_stock-v_disposal.
* }

*** End of change on 08/09/10
**  end of change on 01/12/10

******************* sale stock
*      IF LT_SUM-MKT_CLASS = 'D'.
*** Changed by Furong on 07/29/09
*        IF P_DATUM <= C_CG_DATE.
*          LT_SUM-SALE_STOCK = LT_YR_STOCK-QTY_CGATE
*                              - LT_YR_STOCK-QTY_SHIPOUT.
*        ELSE.
*          LT_SUM-SALE_STOCK = LT_YR_STOCK_05312009-QTY_CGATE
*                              + LT_YR_STOCK-QTY_VPCOUT
*                              - LT_YR_STOCK-QTY_SHIPOUT.
*        ENDIF.


*        LT_SUM-SALE_STOCK = LT_YR_STOCK-QTY_VPCOUT
*                               - LT_YR_STOCK-QTY_SHIPOUT.
*** Changed by Furong on 07/31/09

** Changed by Furong on 08/05/09 - uncomment the ship in qty
* by ig.moon 8/3/2009 {

* by Daniel on 08/03/10 {
*      LT_SUM-SALE_STOCK = LT_YR_STOCK-QTY_SHIPIN
*                             - LT_YR_STOCK-QTY_SHIPOUT.

      lt_sum-sale_stock = lt_yr_stock-qty_shipin
                        - lt_yr_stock-qty_shipout
                        - lt_yr_stock-h_disposal.
* }

*        CLEAR: LT_SUM-SALE_STOCK.
* }
** End of change  on 08/05/09
*** Changed by Furong on 07/31/09

*** Changed by Furong on 07/29/09
** Changed by Furong on 08/05/09 - ship in count for DOM and EXP
*      ELSE.
*        CLEAR: LT_SUM-SALE_STOCK.
*      ENDIF.
** End of change  on 08/05/09

* Sign off stock

*** Changed by Furong on 01/12/10
*      LT_SUM-SIGNOFF_STOCK =  LT_YR_STOCK-QTY_SIGNOFF
*                              - LT_YR_STOCK-QTY_CGATE

* by Daniel on 11/01/10 {
*      LT_SUM-SIGNOFF_STOCK =  LT_YR_STOCK-QTY_SIGNOFF
*                                - LT_YR_STOCK-QTY_CGATE.

      lt_sum-signoff_stock = lt_yr_stock-qty_signoff
                           - lt_yr_stock-qty_cgate
                           - lt_yr_stock-s_disposal.
* }


** Changed by Furong on 08/09/10
*                                - LT_YR_STOCK-S_DISPOSAL.
** End of change  on 08/09/10

*** Changed by Furong  on 01/12/10

* Stock ( signoff - mg)

** Changed by Furong on 07/26/10
      CLEAR: lt_yr_stock_07312010, lt_yr_stock_08012010.

      READ TABLE lt_yr_stock_07312010 WITH KEY natn = lt_day-dist
                                       model = l_model.

      READ TABLE lt_yr_stock_08012010 WITH KEY natn = lt_day-dist
                                       model = l_model.
*** End of change

* by Daniel on 1/11/2012 { 'remove sign-stock addition
*      LT_SUM-MG_STOCK = LT_SUM-SIGNOFF_STOCK +
*                LT_SUM-STOCK.
      lt_sum-mg_stock = lt_sum-stock.
* }

      IF lt_yr_stock-natn = 'B28'.

**** Changed by Furong  on 08/03/10
*** Changed by Furong  on 07/21/10
*        IF P_DATUM <= C_CG_DATE.
*          LT_SUM-MG_STOCK = LT_SUM-MG_STOCK -
*                                       ( LT_YR_STOCK_07312010-QTY_CGATE
*                                            - LT_YR_STOCK-QTY_SHIPOUT )
*.
*        ELSE.
**          LT_SUM-MG_STOCK = LT_SUM-MG_STOCK -
**                                       (
* LT_YR_STOCK_07312010-QTY_CGATE
**                                       -
**LT_YR_STOCK_08012010-QTY_VPCOUT
**                                            - LT_YR_STOCK-QTY_SHIPOUT
*)
**.
*
*          LT_SUM-MG_STOCK = LT_SUM-MG_STOCK -
*                                       ( LT_YR_STOCK_07312010-QTY_CGATE
*                                       - LT_YR_STOCK_08012010-QTY_MGATE
*                                            - LT_YR_STOCK-QTY_SHIPOUT )
*.
*
*
*        ENDIF.

** Changed by Furong on 08/05/10
** Changed by Furong on 08/09/10

*        LT_SUM-MG_STOCK = LT_YR_STOCK-QTY_CGATE
*                                - LT_YR_STOCK_07312010-QTY_CGATE
*                                - LT_YR_STOCK_08012010-QTY_MGATE
*                                + LT_SUM-SIGNOFF_STOCK
*                                - LT_YR_STOCK-V_DISPOSAL.

* by Daniel on 1/11/2012 { 'remove sign-stock addition
*        lt_sum-mg_stock = lt_yr_stock-qty_cgate
*                                      - lt_yr_stock_07312010-qty_cgate
*                                      - lt_yr_stock_08012010-qty_mgate
*                                      + lt_sum-signoff_stock.
        lt_sum-mg_stock = lt_yr_stock-qty_cgate
                        - lt_yr_stock_07312010-qty_cgate
                        - lt_yr_stock_08012010-qty_mgate.
* }

** End of change on 08/09/10
** End of change on 08/05/10

** End of change on 08/03/10

*          LT_SUM-MG_STOCK = LT_SUM-MG_STOCK - ( LT_YR_STOCK-QTY_CGATE
*                                            - LT_YR_STOCK-QTY_SHIPOUT )
*.
** End of change on 07/21/10
      ENDIF.
** End of change on 05/29/09

    ENDIF.
*    ENDIF.
    CLEAR: lt_sum-qty_dd_cancel.

    lt_sum-userid = sy-uname.
    lt_sum-chdate = sy-datum.
    lt_sum-chtime = sy-uzeit.

    READ TABLE lt_plan WITH KEY  model = l_model.
    IF sy-subrc = 0.
      lt_sum-annual_plan = lt_plan-zmpqty.
    ENDIF.

** on 11/04/13
*    IF lt_day-model = 'EMF' OR  lt_day-model = 'EM'.
*      lt_sum-month_plan = l_emf_qty.
** by ig.moon 8/3/2009 {
*    ELSEIF lt_day-model = 'INF' OR  lt_day-model = 'IN'.
*      lt_sum-month_plan = l_inf_qty.
** }
*    ELSEIF lt_day-model = 'TCF' OR  lt_day-model = 'TC'.
*      lt_sum-month_plan = l_tcf_qty.
*    ELSE.
*      lt_sum-month_plan = l_cra_qty.
*    ENDIF.

    READ TABLE lt_model_qty WITH KEY  model = l_model.
    IF sy-subrc = 0.
      lt_sum-month_plan = lt_model_qty-zmpqty.
    ENDIF.


** End 11/04/13

    APPEND lt_sum.

    CLEAR lt_sum.

    CLEAR: lt_day, l_qty_mm, l_qty_yy.

  ENDLOOP.
  it_data[] = lt_sum[].
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_write_data.
  DATA: l_result(1),
        l_msgtxt(100).
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_data_inf.
    APPEND it_data_inf.
    CLEAR: it_data_inf.
  ENDLOOP.
  IF p_snt IS INITIAL.
  ELSE.

    CALL FUNCTION 'Z_FPP_DELIVERY_HMC'
      DESTINATION c_dest
      IMPORTING
        flag                  = l_result
      TABLES
*       I_PROD_ACT            = IT_DATA
        i_del_hmc             = it_data_inf
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.

*  IF SY-SUBRC = 0.
    IF l_result = 'S'.
      MESSAGE i001 WITH 'Data successfully sent out'.
    ELSE.
      l_result = 'E'.
      CONCATENATE '*' l_result '*' l_msgtxt INTO l_msgtxt.
      MESSAGE i001 WITH 'Interface Error:' l_msgtxt.
    ENDIF.
  ENDIF.

** Changed by Furong on 05/29/09
  LOOP AT it_data.
    it_data-int_flag = l_result.
    MODIFY it_data TRANSPORTING int_flag.
  ENDLOOP.

** End of change

* by ig.moon 8/3/2009 {
  DELETE FROM ztpp_deliver_hmc WHERE prdt_date EQ p_datum.
* }
  MODIFY ztpp_deliver_hmc FROM TABLE it_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " write_data
