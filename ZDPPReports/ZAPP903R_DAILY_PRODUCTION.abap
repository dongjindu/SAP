************************************************************************
* Program Name      : ZAPP903R_DAILY_PRODUCTION
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : Daily Production Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
* 02.18.2014 Victor   Modified program to input multi shop date
************************************************************************
REPORT  zapp903r_daily_production  MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: zvpp_vehicle,
        ztpp_input_plan,
        ausp ,
        crhd .

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: it_worder          LIKE TABLE OF zspp_day_worder WITH HEADER LINE,
      it_worder_tmp      LIKE TABLE OF zspp_day_worder WITH HEADER LINE,
      it_product         LIKE TABLE OF ztpp_day_sum    WITH HEADER LINE.


*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
SELECT-OPTIONS : p_wdate     FOR sy-datum OBLIGATORY NO-EXTENSION .
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  IF p_wdate[] IS INITIAL.
    p_wdate-low = sy-datum .
    p_wdate-sign = 'I'.
    p_wdate-option = 'EQ'.
    APPEND p_wdate.
  ENDIF.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM function_call                .
  PERFORM save_table                   .

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  FUNCTION_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM function_call .
  DATA : l_date_gap(3) type n,
         l_date LIKE sy-datum.

  CLEAR : it_worder[].

*-Modified program to input multi shope date: 02.18.2014 Victor
  READ TABLE p_wdate INDEX 1.
  IF p_wdate-high IS INITIAL.
    p_wdate-high = p_wdate-low.
    MODIFY p_wdate INDEX 1.
  ENDIF.

  IF  p_wdate-high+0(6)  <> p_wdate-low+0(6).
    MESSAGE s000 WITH 'Month of Shop Date period should be Same'.
    STOP.
  ENDIF.

  l_date_gap  = p_wdate-high - p_wdate-low + 1.

  DO l_date_gap TIMES.
    l_date  = p_wdate-low + sy-index - 1.
    CALL FUNCTION 'Z_FPP_RP_PRODUCTION'
      EXPORTING
        wdate             = l_date
** Furong on 04/24/13  for D-1
        i_input_plan_chk  = 'X'
** End on 04/24/13
      TABLES
        t_worder          = it_worder_tmp
      EXCEPTIONS
        input_error       = 1
        OTHERS            = 2 .

    IF sy-subrc = 0 AND it_worder_tmp[] IS NOT INITIAL.
      APPEND LINES OF it_worder_tmp TO it_worder.
    ENDIF.

  ENDDO.

  IF it_worder[] IS INITIAL.
    MESSAGE s000 WITH 'There is No data'.
    STOP.
  ENDIF.
ENDFORM.                    " FUNCTION_CALL

*&---------------------------------------------------------------------*
*&      Form  SAVE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_table    .
  FIELD-SYMBOLS: <fields>   TYPE any.
  DATA: lt_product          LIKE TABLE OF ztpp_day_sum WITH HEADER LINE,
        l_names(30)         TYPE c         ,
        l_wohead            LIKE mara-matnr,
        l_count             TYPE i         ,
        l_pqty              TYPE i         ,
        l_mqty              TYPE i         ,
        l_status            TYPE zpp_status       ,
        l_wdate             LIKE ztpp_day_sum-wdate,  "02.19.2014
        l_wo_ser            LIKE ztpp_wosum-wo_ser,
        l_nation            LIKE ztpp_wosum-nation,
        l_dealer            LIKE ztpp_wosum-dealer,
        l_extc              LIKE ztpp_wosum-extc  ,
        l_intc              LIKE ztpp_wosum-intc  .

  SORT it_worder BY wdate worder status   . "added wdate 02.18.2014
  READ TABLE it_worder INDEX 1      .
  l_wdate  = it_worder-wdate        .
  l_wohead = it_worder-worder       .
  l_wo_ser = it_worder-worder(09)   .
  l_nation = it_worder-worder+09(03).
  l_dealer = it_worder-worder+12(02).
  l_extc   = it_worder-worder+14(02).
  l_intc   = it_worder-worder+16(02).
  LOOP AT it_worder.
    l_status = it_worder-status    .
    IF l_wdate = it_worder-wdate AND  l_wohead = it_worder-worder.
      CONCATENATE 'LT_PRODUCT-RP' l_status(2) 'Q' INTO  l_names.
      ASSIGN (l_names)  TO    <fields> .
      <fields> = it_worder-wo_qty      .
      CONTINUE .
    ELSE.
      lt_product-wdate  = l_wdate .
      lt_product-wo_ser = l_wo_ser.
      lt_product-nation = l_nation.
      lt_product-dealer = l_dealer.
      lt_product-extc   = l_extc  .
      lt_product-intc   = l_intc  .
      APPEND lt_product.  CLEAR: lt_product.
      CONCATENATE 'LT_PRODUCT-RP' l_status(2) 'Q' INTO  l_names.
      ASSIGN (l_names)  TO    <fields> .
      <fields> = it_worder-wo_qty      .
      l_wdate  = it_worder-wdate        .
      l_wohead = it_worder-worder       .
      l_wo_ser = it_worder-worder(09)   .
      l_nation = it_worder-worder+09(03).
      l_dealer = it_worder-worder+12(02).
      l_extc   = it_worder-worder+14(02).
      l_intc   = it_worder-worder+16(02).
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_worder LINES l_count.
  IF l_count > 0.
    lt_product-wdate  = l_wdate .
    lt_product-wo_ser = l_wo_ser.
    lt_product-nation = l_nation.
    lt_product-dealer = l_dealer.
    lt_product-extc   = l_extc  .
    lt_product-intc   = l_intc  .
    APPEND lt_product.  CLEAR: lt_product.
  ENDIF.

  SORT lt_product BY wdate wo_ser nation dealer extc intc   .
  READ TABLE lt_product INDEX 1     .
  CLEAR: l_pqty, l_mqty.
  l_wdate    = lt_product-wdate     .
  l_wo_ser   = lt_product-wo_ser    .
  l_nation   = lt_product-nation    .
  l_dealer   = lt_product-dealer    .
  l_extc     = lt_product-extc      .
  l_intc     = lt_product-intc      .

  LOOP AT lt_product.
    IF   l_wdate  = lt_product-wdate AND l_wo_ser = lt_product-wo_ser
     AND l_nation = lt_product-nation AND l_dealer = lt_product-dealer
     AND l_extc   = lt_product-extc   AND l_intc   = lt_product-intc     .
      it_product-rp01q = it_product-rp01q + lt_product-rp01q .
      it_product-rp02q = it_product-rp02q + lt_product-rp02q .
      it_product-rp03q = it_product-rp03q + lt_product-rp03q .
      it_product-rp04q = it_product-rp04q + lt_product-rp04q .
      it_product-rp05q = it_product-rp05q + lt_product-rp05q .
      it_product-rp06q = it_product-rp06q + lt_product-rp06q .
      it_product-rp07q = it_product-rp07q + lt_product-rp07q .
      it_product-rp08q = it_product-rp08q + lt_product-rp08q .
      it_product-rp09q = it_product-rp09q + lt_product-rp09q .
      it_product-rp10q = it_product-rp10q + lt_product-rp10q .
      it_product-rp11q = it_product-rp11q + lt_product-rp11q .
      it_product-rp12q = it_product-rp12q + lt_product-rp12q .
      it_product-rp13q = it_product-rp13q + lt_product-rp13q .
      it_product-rp14q = it_product-rp14q + lt_product-rp14q .
      it_product-rp15q = it_product-rp15q + lt_product-rp15q .
      it_product-rp16q = it_product-rp16q + lt_product-rp16q .
      it_product-rp17q = it_product-rp17q + lt_product-rp17q .
      it_product-rp18q = it_product-rp18q + lt_product-rp18q .
      it_product-rp19q = it_product-rp19q + lt_product-rp19q .
      it_product-rp20q = it_product-rp20q + lt_product-rp20q .
      it_product-rp21q = it_product-rp21q + lt_product-rp21q .
      it_product-rp22q = it_product-rp22q + lt_product-rp22q .
      it_product-rp23q = it_product-rp23q + lt_product-rp23q .
      it_product-rp24q = it_product-rp24q + lt_product-rp24q .
      it_product-rp25q = it_product-rp25q + lt_product-rp25q .
      it_product-rp26q = it_product-rp26q + lt_product-rp26q .
      it_product-rp27q = it_product-rp27q + lt_product-rp27q .
*       IT_PRODUCT-RP28Q = IT_PRODUCT-RP28Q + LT_PRODUCT-RP28Q .
    ELSE.
      it_product-wdate  = l_wdate  .
      it_product-wo_ser = l_wo_ser .
      it_product-dealer = l_dealer .
      it_product-nation = l_nation .
      it_product-extc   = l_extc   .
      it_product-intc   = l_intc   .
      APPEND it_product. CLEAR: it_product.
      it_product-rp01q = it_product-rp01q + lt_product-rp01q .
      it_product-rp02q = it_product-rp02q + lt_product-rp02q .
      it_product-rp03q = it_product-rp03q + lt_product-rp03q .
      it_product-rp04q = it_product-rp04q + lt_product-rp04q .
      it_product-rp05q = it_product-rp05q + lt_product-rp05q .
      it_product-rp06q = it_product-rp06q + lt_product-rp06q .
      it_product-rp07q = it_product-rp07q + lt_product-rp07q .
      it_product-rp08q = it_product-rp08q + lt_product-rp08q .
      it_product-rp09q = it_product-rp09q + lt_product-rp09q .
      it_product-rp10q = it_product-rp10q + lt_product-rp10q .
      it_product-rp11q = it_product-rp11q + lt_product-rp11q .
      it_product-rp12q = it_product-rp12q + lt_product-rp12q .
      it_product-rp13q = it_product-rp13q + lt_product-rp13q .
      it_product-rp14q = it_product-rp14q + lt_product-rp14q .
      it_product-rp15q = it_product-rp15q + lt_product-rp15q .
      it_product-rp16q = it_product-rp16q + lt_product-rp16q .
      it_product-rp17q = it_product-rp17q + lt_product-rp17q .
      it_product-rp18q = it_product-rp18q + lt_product-rp18q .
      it_product-rp19q = it_product-rp19q + lt_product-rp19q .
      it_product-rp20q = it_product-rp20q + lt_product-rp20q .
      it_product-rp21q = it_product-rp21q + lt_product-rp21q .
      it_product-rp22q = it_product-rp22q + lt_product-rp22q .
      it_product-rp23q = it_product-rp23q + lt_product-rp23q .
      it_product-rp24q = it_product-rp24q + lt_product-rp24q .
      it_product-rp25q = it_product-rp25q + lt_product-rp25q .
      it_product-rp26q = it_product-rp26q + lt_product-rp26q .
      it_product-rp27q = it_product-rp27q + lt_product-rp27q .
*       IT_PRODUCT-RP28Q = IT_PRODUCT-RP28Q + LT_PRODUCT-RP28Q .
      l_wdate    = lt_product-wdate     .
      l_wo_ser   = lt_product-wo_ser    .
      l_nation   = lt_product-nation    .
      l_dealer   = lt_product-dealer    .
      l_extc     = lt_product-extc      .
      l_intc     = lt_product-intc      .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_product LINES l_count.
  IF l_count > 0.
    it_product-wdate  = l_wdate  .
    it_product-wo_ser = l_wo_ser .
    it_product-dealer = l_dealer .
    it_product-nation = l_nation .
    it_product-extc   = l_extc   .
    it_product-intc   = l_intc   .
    APPEND it_product.  CLEAR: it_product.
  ENDIF.

  LOOP AT it_product.  "TimeStamp on 02.19.2014 Victor
    it_product-erdat  = sy-datum.
    it_product-erzet  = sy-uzeit.
    it_product-ernam  = sy-uname.
    MODIFY it_product.
  ENDLOOP.

  DELETE FROM ztpp_day_sum WHERE wdate IN p_wdate.

  MODIFY ztpp_day_sum FROM TABLE it_product.
  IF sy-subrc = 0.
    MESSAGE s000 WITH 'Data was updated successfully'.
  ELSE.
    MESSAGE e000 WITH 'Data update failed'.
  ENDIF.
ENDFORM.                    " SAVE_TABLE
