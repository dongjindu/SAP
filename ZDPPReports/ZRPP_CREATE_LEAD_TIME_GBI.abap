************************************************************************
* Program Name      : ZRPP_CREATE_LEAD_TIME_GBI
* Creation Date     : 06/2013
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT  zrpp_create_lead_time_gbi MESSAGE-ID zmpp.
*-----// Internal tables
DATA: BEGIN OF it_wip OCCURS 0,
        objek      LIKE ausp-objek,
        shop,                         "B, P, T
        zsdate_in  LIKE ausp-atflv,
        zadate_in  LIKE ausp-atwrt,
        zsdate_out LIKE ausp-atflv,
        zadate_out LIKE ausp-atwrt,
        worder     LIKE ausp-atwrt,
      END   OF it_wip.

DATA: BEGIN OF it_work_time OCCURS 0,
        shop   TYPE   arbpl.
        INCLUDE STRUCTURE zsmm_working_time.
DATA: END   OF it_work_time.

DATA: it_lead_time LIKE ztpp_lead_time OCCURS 0 WITH HEADER LINE.

*-----// Global variable
DATA: v_sdate_bin  LIKE cabn-atinn,
      v_sdate_bout LIKE cabn-atinn,
      v_sdate_pin  LIKE cabn-atinn,
      v_sdate_pout LIKE cabn-atinn,
      v_sdate_tin  LIKE cabn-atinn,
      v_sdate_tout LIKE cabn-atinn,
      v_adate_bin  LIKE cabn-atinn,
      v_adate_bout LIKE cabn-atinn,
      v_adate_pin  LIKE cabn-atinn,
      v_adate_pout LIKE cabn-atinn,
      v_adate_tin  LIKE cabn-atinn,
      v_adate_tout LIKE cabn-atinn,
      v_worder     LIKE cabn-atinn.

RANGES: r_shop_out FOR ausp-atflv.

CONSTANTS: c_werks LIKE t001w-werks VALUE 'P001'.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_sout FOR sy-datum OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  PERFORM read_data.
  PERFORM make_table_data.

  MODIFY ztpp_lead_time FROM TABLE it_lead_time.

  message s000 with text-m05.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
  s_sout-sign   = 'I'.
  s_sout-option = 'BT'.

  s_sout-low  = sy-datum - 3.
  s_sout-high = sy-datum - 1.

  APPEND s_sout.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data .
  PERFORM read_master.

  PERFORM read_wip USING 'B' v_sdate_bin  v_adate_bin
                             v_sdate_bout v_adate_bout.
  PERFORM read_wip USING 'P' v_sdate_pin  v_adate_pin
                             v_sdate_pout v_adate_pout.
  PERFORM read_wip USING 'T' v_sdate_tin  v_adate_tin
                             v_sdate_tout v_adate_tout.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_master .
  DATA: l_date_tmp(8) TYPE n.

  SELECT SINGLE atinn INTO v_sdate_bin
    FROM cabn WHERE atnam = 'P_RP01_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_sdate_bout
    FROM cabn WHERE atnam = 'P_RP02_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_sdate_pin
    FROM cabn WHERE atnam = 'P_RP02_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_sdate_pout
    FROM cabn WHERE atnam = 'P_RP07_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_sdate_tin
    FROM cabn WHERE atnam = 'P_RP07_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_sdate_tout
    FROM cabn WHERE atnam = 'P_RP17_SHOP_DATE'.

  SELECT SINGLE atinn INTO v_adate_bin
    FROM cabn WHERE atnam = 'P_RP01_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_adate_bout
    FROM cabn WHERE atnam = 'P_RP02_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_adate_pin
    FROM cabn WHERE atnam = 'P_RP02_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_adate_pout
    FROM cabn WHERE atnam = 'P_RP07_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_adate_tin
    FROM cabn WHERE atnam = 'P_RP07_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_adate_tout
    FROM cabn WHERE atnam = 'P_RP17_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_worder
    FROM cabn WHERE atnam = 'P_WORK_ORDER'.

  LOOP AT s_sout.
    r_shop_out-sign   = s_sout-sign.
    r_shop_out-option = s_sout-option.
    r_shop_out-low    = l_date_tmp = s_sout-low.
    r_shop_out-high   = l_date_tmp = s_sout-high.

    APPEND r_shop_out.
  ENDLOOP.
ENDFORM.                    " READ_MASTER
*&---------------------------------------------------------------------*
*&      Form  READ_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM read_wip USING pv_shop pv_sdate_in  pv_adate_in
                            pv_sdate_out pv_adate_out.
  DATA: lt_wip LIKE it_wip OCCURS 0 WITH HEADER LINE.

  SELECT a~objek
         b~atflv AS zsdate_in
         c~atwrt AS zadate_in
         d~atflv AS zsdate_out
         e~atwrt AS zadate_out
         f~atwrt AS worder
    INTO CORRESPONDING FIELDS OF TABLE lt_wip
    FROM ausp AS a LEFT OUTER JOIN ausp AS b
                     ON b~objek = a~objek
                    AND b~atinn = pv_sdate_in
                    AND b~mafid = a~mafid
                    AND b~klart = a~klart
                   LEFT OUTER JOIN ausp AS c
                     ON c~objek = a~objek
                    AND c~atinn = pv_adate_in
                    AND c~mafid = a~mafid
                    AND c~klart = a~klart
                   LEFT OUTER JOIN ausp AS d
                     ON d~objek = a~objek
                    AND d~atinn = pv_sdate_out
                    AND d~mafid = a~mafid
                    AND d~klart = a~klart
                   LEFT OUTER JOIN ausp AS e
                     ON e~objek = a~objek
                    AND e~atinn = pv_adate_out
                    AND e~mafid = a~mafid
                    AND e~klart = a~klart
                   LEFT OUTER JOIN ausp AS f
                     ON f~objek = a~objek
                    AND f~atinn = v_worder
                    AND f~mafid = a~mafid
                    AND f~klart = a~klart
   WHERE a~klart EQ '002'
     AND a~atinn EQ pv_sdate_out
     AND a~atflv IN r_shop_out.

  MOVE: pv_shop TO lt_wip-shop.
  MODIFY lt_wip TRANSPORTING shop WHERE objek >= space.

  DELETE lt_wip WHERE worder+12(2) EQ 'XX'
                   OR worder+12(2) EQ 'XY'
                   OR worder+12(2) EQ 'XA'.

  APPEND LINES OF lt_wip TO it_wip.
ENDFORM.                    " READ_WIP
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_LEAD_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_lead_time USING pv_shop pv_from_time
                               pv_to_time pv_time_gap.
  DATA: lv_tabix LIKE sy-tabix.

  PERFORM get_first_time_zone USING pv_shop pv_from_time lv_tabix.

  LOOP AT it_work_time  FROM lv_tabix
                       WHERE shop  =   pv_shop.
    IF it_work_time-wosec EQ 0 OR it_work_time-opsec EQ 0.
      IF it_work_time-wofrm <= pv_to_time AND
         it_work_time-woend >= pv_to_time.
        EXIT.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF it_work_time-wofrm > pv_to_time.
      EXIT.
    ENDIF.

    IF it_work_time-wofrm <= pv_from_time AND
       it_work_time-woend >= pv_to_time.
      PERFORM get_time_gap USING pv_from_time pv_to_time pv_time_gap.
      EXIT.
    ENDIF.

    IF it_work_time-wofrm <= pv_from_time AND
       it_work_time-woend >= pv_from_time.
      PERFORM get_time_gap USING pv_from_time it_work_time-woend
                                 pv_time_gap.
      CONTINUE.
    ENDIF.

    IF it_work_time-wofrm <= pv_to_time AND
       it_work_time-woend >= pv_to_time.
      PERFORM get_time_gap USING it_work_time-wofrm pv_to_time
                                 pv_time_gap.
      EXIT.
    ENDIF.

    pv_time_gap = pv_time_gap + it_work_time-opsec.
  ENDLOOP.

  pv_time_gap = pv_time_gap / 60.
ENDFORM.                    " CALCULATE_LEAD_TIME
*&---------------------------------------------------------------------*
*&      Form  MAKE_TABLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_table_data .

  DATA: l_date_tmp(8) TYPE n.

  PERFORM read_working_time.

  LOOP AT it_wip.
    CLEAR: it_lead_time.

    it_lead_time-bwerk       = c_werks.
    it_lead_time-model_code  = it_wip-objek(3).
    it_lead_time-body_no     = it_wip-objek+3(6).
    it_lead_time-zshop       = it_wip-shop.
    it_lead_time-zsdate_in   = l_date_tmp = it_wip-zsdate_in.
    it_lead_time-zadate_in   = it_wip-zadate_in.
    it_lead_time-zsdate_out  = l_date_tmp = it_wip-zsdate_out.
    it_lead_time-zadate_out  = it_wip-zadate_out.
    it_lead_time-ernam       = sy-uname.
    it_lead_time-erdat       = sy-datum.
    it_lead_time-erzet       = sy-uzeit.
    it_lead_time-aenam       = sy-uname.
    it_lead_time-aedat       = sy-datum.
    it_lead_time-aezet       = sy-uzeit.

    PERFORM get_lead_time.

    APPEND it_lead_time.
  ENDLOOP.

  PERFORM set_time_stamp.

ENDFORM.                    " MAKE_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_working_time .
  DATA: lv_min_bin  LIKE sy-datum,
        lv_min_pin  LIKE sy-datum,
        lv_min_tin  LIKE sy-datum.

** Chnaged on 12/02/13
*  LOOP AT it_wip.
*    CASE it_wip-shop.
*      WHEN 'B'.
*        IF lv_min_bin > it_wip-zadate_in(8) OR lv_min_bin IS INITIAL.
*          lv_min_bin = it_wip-zadate_in(8).
*        ENDIF.
*      WHEN 'P'.
*        IF lv_min_pin > it_wip-zadate_in(8) OR lv_min_pin IS INITIAL.
*          lv_min_pin = it_wip-zadate_in(8).
*        ENDIF.
*      WHEN 'T'.
*        IF lv_min_tin > it_wip-zadate_in(8) OR lv_min_tin IS INITIAL.
*          lv_min_tin = it_wip-zadate_in(8).
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.
*
*  lv_min_bin = lv_min_bin - 1.
*  lv_min_pin = lv_min_pin - 1.
*  lv_min_tin = lv_min_tin - 1.
*
*  CLEAR: it_work_time. REFRESH: it_work_time.
*
*  PERFORM get_working_time USING 'B' lv_min_bin.
*  PERFORM get_working_time USING 'P' lv_min_pin.
*  PERFORM get_working_time USING 'T' lv_min_tin.

  LOOP AT it_wip.
    CASE it_wip-shop.
      WHEN 'B' or 'P'.
        IF lv_min_bin > it_wip-zadate_in(8) OR lv_min_bin IS INITIAL.
          lv_min_bin = it_wip-zadate_in(8).
        ENDIF.
      WHEN 'T'.
        IF lv_min_tin > it_wip-zadate_in(8) OR lv_min_tin IS INITIAL.
          lv_min_tin = it_wip-zadate_in(8).
        ENDIF.
    ENDCASE.
  ENDLOOP.

  lv_min_bin = lv_min_bin - 1.
  lv_min_tin = lv_min_tin - 1.

  CLEAR: it_work_time. REFRESH: it_work_time.

  PERFORM get_working_time USING 'B' lv_min_bin.
  PERFORM get_working_time USING 'T' lv_min_tin.

** End on 12/02/13
  SORT it_work_time BY shop wofrm woend.
ENDFORM.                    " READ_WORKING_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0686   text
*      -->P_LV_MIN_BIN  text
*----------------------------------------------------------------------*
FORM get_working_time USING pv_shop pv_bdate.
  DATA: lv_gap TYPE i.

  DATA: lt_work_time  LIKE zsmm_working_time OCCURS 0 WITH HEADER LINE.

  CHECK pv_bdate IS NOT INITIAL.

  lv_gap = sy-datum - pv_bdate + 1.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
    EXPORTING
      i_datum              = pv_bdate
      i_day                = lv_gap
      i_arbpl              = pv_shop
    TABLES
      t_working_time       = lt_work_time
    EXCEPTIONS
      cannot_read_dayname  = 1
      incorrect_shift_info = 2
      incorrect_capa_info  = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m02 pv_shop.
  ENDIF.

  LOOP AT lt_work_time.
    CLEAR: it_work_time.
    MOVE: pv_shop TO it_work_time-shop.
    MOVE-CORRESPONDING lt_work_time TO it_work_time.

    APPEND it_work_time.
  ENDLOOP.
ENDFORM.                    " GET_WORKING_TIME

*&---------------------------------------------------------------------*
*&      Form  GET_LEAD_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_lead_time .
  DATA: l_gap  TYPE p DECIMALS 2,
        l_shop.

  IF it_wip-shop EQ 'P'.
    l_shop = 'B'.
  ELSE.
    l_shop = it_wip-shop.
  ENDIF.

  PERFORM calculate_lead_time USING l_shop            it_wip-zadate_in
                                    it_wip-zadate_out l_gap.

  it_lead_time-zleadtime = l_gap.
ENDFORM.                    " GET_LEAD_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_FIRST_TIME_ZONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_SHOP  text
*      -->P_PV_FROM_TIME  text
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM get_first_time_zone USING pv_shop pv_from_time pv_tabix.
  DATA: lv_from_time(14).

  READ TABLE it_work_time WITH KEY shop = pv_shop
                          BINARY SEARCH.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH text-m06.
  ENDIF.

  CONCATENATE pv_from_time(10) it_work_time-wofrm+10(4)
         INTO lv_from_time.

  READ TABLE it_work_time WITH KEY shop  = pv_shop
                                   wofrm = lv_from_time
                          BINARY SEARCH.
  IF sy-subrc NE 0.
    LOOP AT it_work_time WHERE shop  = pv_shop
                           AND wofrm >= pv_from_time.
      MOVE: sy-tabix TO pv_tabix.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      MESSAGE e000 WITH text-m06.
    ENDIF.
  ENDIF.

  IF pv_from_time >= lv_from_time.
    pv_tabix = sy-tabix.
  ELSE.
    pv_tabix = sy-tabix - 1.
  ENDIF.
ENDFORM.                    " GET_FIRST_TIME_ZONE
*&---------------------------------------------------------------------*
*&      Form  GET_TIME_GAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_FROM_TIME  text
*      -->P_PV_TO_TIME  text
*      -->P_PV_TIME_GAP  text
*----------------------------------------------------------------------*
FORM get_time_gap USING pv_time_f pv_time_t pv_time_gap.
  DATA: lv_time_f   LIKE   sy-uzeit,
        lv_time_t   LIKE   sy-uzeit.

  MOVE: pv_time_f+8(6) TO lv_time_f,
        pv_time_t+8(6) TO lv_time_t.

  IF lv_time_f < lv_time_t.
    pv_time_gap = pv_time_gap +
                  ( lv_time_t - lv_time_f ) *
                  ( it_work_time-opsec / it_work_time-wosec ).
  ELSEIF lv_time_f > lv_time_t.
    pv_time_gap = pv_time_gap +
                  ( 86400 - ( lv_time_f - lv_time_t ) ) *
                  ( it_work_time-opsec / it_work_time-wosec ).
  ELSEIF lv_time_f = lv_time_t.
    " Time Gap is 0
  ENDIF.
ENDFORM.                    " GET_TIME_GAP

*&---------------------------------------------------------------------*
*&      Form  SET_TIME_STAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_time_stamp .
  DATA: lt_old LIKE ztpp_lead_time OCCURS 0 WITH HEADER LINE.

  CHECK it_lead_time[] IS NOT INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_old
    FROM ztpp_lead_time
     FOR ALL ENTRIES IN it_lead_time
   WHERE bwerk      = it_lead_time-bwerk
     AND model_code = it_lead_time-model_code
     AND body_no    = it_lead_time-body_no
     AND zshop      = it_lead_time-zshop.

  SORT it_lead_time BY bwerk model_code body_no zshop.

  LOOP AT lt_old.
    READ TABLE it_lead_time WITH KEY bwerk      = lt_old-bwerk
                                     model_code = lt_old-model_code
                                     body_no    = lt_old-body_no
                                     zshop      = lt_old-zshop
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE: lt_old-ernam TO it_lead_time-ernam,
            lt_old-erdat TO it_lead_time-erdat,
            lt_old-erzet TO it_lead_time-erzet.

      MODIFY it_lead_time INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SET_TIME_STAMP
