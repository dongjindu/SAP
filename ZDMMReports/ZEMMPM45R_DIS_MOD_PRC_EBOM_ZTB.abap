************************************************************************
* Program Name      : ZEMMPM45R_DIS_MOD_PRC_EBOM_ZTB
* Author            : Byung-sung, Bae
* Creation Date     : 2004.06.28.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Display Module Price
*
* Modification Logs
* Date       Developer    RequestNo    Description
* copy from ZEMMPM45R_DIS_MOD_PRC_EBOM to get all the data from Z-table
************************************************************************
REPORT zemmpm45r_dis_mod_prc_ebom NO STANDARD PAGE HEADING
                                      LINE-SIZE  180
                                      LINE-COUNT  58.

INCLUDE: <icon>.

TABLES: ztmm_assy_cost1,
        ztmm_assy_cost2,
*        ztmm_cp_color,
        lfa1,
        a018,
        mara,
        t024,
        eina,
        eine,
        t001,
        v_t686a,
        v_t685a.

*----- Internal tables
DATA: BEGIN OF it_module OCCURS 0,
        indicator,                              "Sub Material Status
        vtype     LIKE   ztmm_assy_cost1-vtype, "Vehicle type
        name1     LIKE   lfa1-name1,            "Vendor name
        lifnr     LIKE   lfa1-lifnr,            "Vendor
        maktx     LIKE   makt-maktx,            "Description
        matnr     LIKE   mara-matnr,            "Material
        datab     LIKE   ztmm_assy_cost1-datab, "Valid on
        datbi     LIKE   ztmm_assy_cost1-datbi, "Valid to
        moamt     LIKE   mseg-dmbtr,            "Module Price
        asytr     LIKE   mseg-dmbtr,            "Module Assy Cost
        dmbtr     LIKE   mseg-dmbtr,            "Material Cost
        dmamt     TYPE   f,                     "Material Cost(Floating)
        waers     LIKE   t001-waers,            "Currency
        ekgrp     LIKE   ekko-ekgrp,            "Purchasing Group
        netpr     LIKE   ekpo-netpr,            "Component Amount
        peinh     LIKE   ekpo-peinh,            "Component Price Unit
        meins     LIKE   ekpo-meins,            "UoM
        mcode     LIKE   ztmm_assy_cost1-mcode, "Module code
        kzust     LIKE   konh-kzust.            "Reason code
        INCLUDE STRUCTURE zsmm_custom_condition.
DATA:   ztir     LIKE   ekpo-netpr,
        sts       TYPE   i,                     "Module Info Status
        msg(100),
        chbox,
      END   OF it_module.

DATA : BEGIN OF it_tab OCCURS 0,
       vtype  LIKE   ztmm_assy_cost1-vtype,
       lifnr  LIKE   lfa1-lifnr,
       matnr  LIKE   mara-matnr,
       datab LIKE   ztmm_assy_cost1-datab,
       datbi LIKE   ztmm_assy_cost1-datbi,
              asytr LIKE mseg-dmbtr,
       maktx  LIKE   makt-maktx,
       name1 LIKE   lfa1-name1,
       upgvc LIKE mara-matnr,
       pref LIKE stpo-posnr, "ztbm_abxduldt-pref,
       comp  LIKE mara-matnr,
       maktx1 LIKE  makt-maktx ,
       qnty LIKE stpo-menge,
       unit LIKE   mara-meins,
       meins LIKE   mara-meins,
       ekgrp LIKE   ekko-ekgrp,
       mcode LIKE ztmm_assy_cost1-mcode,
       datab1 LIKE   ztmm_assy_cost1-datab ,
       datbi1 LIKE   ztmm_assy_cost1-datbi ,
       stgb LIKE stpo-stgb,
        netpr LIKE stpo-preis,
       peinh LIKE stpo-peinh,
      END OF it_tab.

DATA : BEGIN OF it_comp OCCURS 0,
        matnr LIKE mara-matnr,
        upgvc LIKE mara-matnr,
       END OF it_comp .

DATA: BEGIN OF it_sub OCCURS 0,
        vtype     LIKE   ztmm_assy_cost1-vtype,   "Vehicle type
        matnr     LIKE   ztmm_assy_cost1-vtype,   "Material
        upgvc     LIKE   mara-matnr,              "UPG-VC
        pref      LIKE   stpo-posnr,
        comp      LIKE   mara-matnr,
        maktx     LIKE   makt-maktx,              "Description
        lifnr     LIKE   lfa1-lifnr,              "Vendor
        amount    TYPE   f,                       "Component Amount
        qnty      LIKE   stpo-menge,
        stgb      LIKE   stpo-stgb,
        unit      LIKE   mara-meins,
        meins     LIKE   mara-meins,              "UoM(sub)
        kmein     LIKE   konp-kmein,              "UoM(Info)
        datab     LIKE   sy-datum,                "Valid on(sub)
        datbi     LIKE   sy-datum,                "Valid to(Sub)
        netpr     LIKE   ekpo-netpr,              "Component Amount
        peinh     LIKE   ekpo-peinh,              "Component Price Unit
        waers     LIKE   t001-waers,              "Currency
        kzust     LIKE   konh-kzust.              "Reason code
        INCLUDE STRUCTURE zsmm_custom_condition.
DATA:   ztir      LIKE   ekpo-netpr,
        sts,                                      "Status
        msg(100),                                  "Message
      END   OF it_sub.

DATA: BEGIN OF it_compare OCCURS 0,
        upgvc     LIKE   mara-matnr,              "UPG-VC
        pref      LIKE   stpo-posnr,    "ztbm_abxduldt-pref,
        comp      LIKE   mara-matnr,
        lifnr     LIKE   lfa1-lifnr,              "Vendor
        msg(100),                                 "Message
        qnty_s    LIKE   stpo-menge,
        unit_s    LIKE   mara-meins,
        datab_s   LIKE   ztmm_assy_cost1-datab,   "Valid on(sub)
        datbi_s   LIKE   ztmm_assy_cost1-datbi,   "Valid to(Sub)
        netpr_s   LIKE   ekpo-netpr,              "Component Amount
        peinh_s   LIKE   ekpo-peinh,              "Component Price Unit
        kzust_s   LIKE   konh-kzust,              "Reason code
        netwr_s   LIKE   ekpo-netwr,
        qnty_t    LIKE   stpo-menge,
        unit_t    LIKE   mara-meins,
        datab_t   LIKE   ztmm_assy_cost1-datab,   "Valid on(sub)
        datbi_t   LIKE   ztmm_assy_cost1-datbi,   "Valid to(Sub)
        netpr_t   LIKE   ekpo-netpr,              "Component Amount
        peinh_t   LIKE   ekpo-peinh,              "Component Price Unit
        kzust_t   LIKE   konh-kzust,              "Reason code
        netwr_t   LIKE   ekpo-netwr,
        waers     LIKE   t001-waers,              "Currency
      END   OF it_compare.

DATA : BEGIN OF it_output OCCURS 0,
            vtype LIKE ztmm_assy_cost1-vtype,
            lifnr LIKE lfa1-lifnr,
            matnr LIKE mara-matnr,
            datab LIKE ztmm_assy_cost1-datab,
            datbi LIKE ztmm_assy_cost1-datbi,
            maktx LIKE makt-maktx,
            name1 LIKE lfa1-name1,
            upgvc LIKE mara-matnr,
            pref  LIKE stpo-posnr,  "ZTBM_ABXDULDT-pref
            comp  LIKE mara-matnr,
            cmaktx LIKE makt-maktx,
            qnty LIKE stpo-menge,
            unit LIKE mara-meins,
            meins     LIKE   mara-meins,
            ekgrp LIKE ztmm_assy_cost1-ekgrp,
            mcode LIKE ztmm_assy_cost1-mcode,
            cdatab LIKE ztmm_assy_cost1-datab,
            cdatbi LIKE ztmm_assy_cost1-datbi,
            stgb   LIKE stpo-stgb,     "******
     netpr LIKE stpo-preis,
       peinh LIKE stpo-peinh,
       zp12 LIKE zsmm_custom_condition-zp12,
       zp13 LIKE zsmm_custom_condition-zp13,
       kzust LIKE konh-kzust,
       dmbtr LIKE mseg-dmbtr,
       waers LIKE stpo-waers,
       asytr LIKE mseg-dmbtr,
        END OF it_output.

DATA : BEGIN OF it_detail OCCURS 0,
       upgvc LIKE stpo-upgn,
       sposn LIKE stpo-posnr,
       idnrk LIKE stpo-idnrk,
       maktx LIKE makt-maktx,
       kmpmg LIKE stpo-menge,
       kmpme LIKE stpo-meins,
       datab LIKE stpo-datuv,
       datbi LIKE stpo-datuv,
       netpr LIKE stpo-preis,
       peinh LIKE stpo-peinh,
       zp12 LIKE zsmm_custom_condition-zp12,
       zp13 LIKE zsmm_custom_condition-zp13,
       kzust LIKE konh-kzust,
       dmbtr LIKE mseg-dmbtr,
       waers LIKE stpo-waers,
       lifnr LIKE stpo-lifnr,
       msg(100),
       END OF it_detail.

DATA : BEGIN OF it_subsum OCCURS 0,
       upgvc LIKE stpo-upgn,
       sposn LIKE stpo-posnr,
       idnrk LIKE stpo-idnrk,
       maktx LIKE makt-maktx,
       msg(100),
       END OF it_subsum.

DATA: it_condition LIKE ztmm_assy_cost2 OCCURS 0 WITH HEADER LINE.

DATA: it_assy LIKE zsmm_assy_cost OCCURS 0 WITH HEADER LINE.

*----- Global variables & structures
DATA: wa_module LIKE it_module.
DATA: wa_sub LIKE it_sub.

DATA: BEGIN OF wa_compare,
        vtype_s     LIKE   ztmm_assy_cost1-vtype, "Vehicle type
        name1_s     LIKE   lfa1-name1,            "Vendor name
        lifnr_s     LIKE   lfa1-lifnr,            "Vendor
        maktx_s     LIKE   makt-maktx,            "Description
        matnr_s     LIKE   mara-matnr,            "Material
        datab_s     LIKE   ztmm_assy_cost1-datab, "Valid on
        datbi_s     LIKE   ztmm_assy_cost1-datbi, "Valid to
        moamt_s     LIKE   mseg-dmbtr,            "Module Price
        asytr_s     LIKE   ztmm_assy_cost1-asytr, "Module Assy Cost
        dmbtr_s     LIKE   mseg-dmbtr,            "Material Cost
        indicator_s,                              "Sub Material Status
        vtype_t     LIKE   ztmm_assy_cost1-vtype, "Vehicle type
        name1_t     LIKE   lfa1-name1,            "Vendor name
        lifnr_t     LIKE   lfa1-lifnr,            "Vendor
        maktx_t     LIKE   makt-maktx,            "Description
        matnr_t     LIKE   mara-matnr,            "Material
        datab_t     LIKE   ztmm_assy_cost1-datab, "Valid on
        datbi_t     LIKE   ztmm_assy_cost1-datbi, "Valid to
        moamt_t     LIKE   mseg-dmbtr,            "Module Price
        asytr_t     LIKE   ztmm_assy_cost1-asytr, "Module Assy Cost
        dmbtr_t     LIKE   mseg-dmbtr,            "Material Cost
        indicator_t,                              "Sub Material Status
        waers       LIKE   t001-waers,            "Currency
      END   OF wa_compare.

DATA: wa_vtype_f   LIKE   ztmm_assy_cost1-vtype,
      wa_vtype_t   LIKE   ztmm_assy_cost1-vtype,
      wa_mcode_f   LIKE   ztmm_assy_cost1-mcode,
      wa_mcode_t   LIKE   ztmm_assy_cost1-mcode,
      wa_lifnr_f   LIKE   ztmm_assy_cost1-lifnr,
      wa_lifnr_t   LIKE   ztmm_assy_cost1-lifnr,
      wa_matnr_f   LIKE   mara-matnr,
      wa_matnr_t   LIKE   mara-matnr,
      wa_ekgrp_f   LIKE   t024-ekgrp,
      wa_ekgrp_t   LIKE   t024-ekgrp,
      wa_tot_page  TYPE   i,
      wa_format_flg,
      w_module(50),
      w_sub(50),
      w_index(2)   TYPE   n,
      l_mcode(4) TYPE c.

DATA :  l_dyname LIKE sy-repid.

DATA : t_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE.

FIELD-SYMBOLS: <module>, <sub>.

*----- Constants
" Module BOM is same all of plants
DATA: c_werks   LIKE   t001w-werks   VALUE   'P001',
      c_ekorg   LIKE   ekko-ekorg    VALUE   'PU01',
      c_kschl   LIKE   konp-kschl    VALUE   'PB00',
      c_ztir    LIKE   konp-kschl    VALUE   'ZTIR',
      c_bukrs   LIKE   t001-bukrs    VALUE   'H201',
      c_ready   TYPE   i             VALUE   1,
      c_warning TYPE   i             VALUE   2,
      c_success TYPE   i             VALUE   3,
      c_error   TYPE   i             VALUE   4,
      c_incorrect TYPE i             VALUE   5,
      c_new     TYPE   i             VALUE   1,"Module Status
      c_deleted TYPE   i             VALUE   2,"Module Status
      c_no_cond TYPE   i             VALUE   3,"Condition does not exist
      c_del_con TYPE   i             VALUE   4,"Condition was deleted
      c_no_info TYPE   i             VALUE   5,"InfoRecord dosn't exist
      c_uom_err TYPE   i             VALUE   6,"Incorrect UoM
      c_no_matl TYPE   i             VALUE   7,"M/M does not exist.
      c_exist   TYPE   i             VALUE   8,"Info Record is exist.
      c_sub_err TYPE   i             VALUE   9,
      c_diff    TYPE   i             VALUE  10.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_vtype FOR ztmm_assy_cost1-vtype
                NO-EXTENSION NO INTERVALS.
PARAMETERS:     p_mcode LIKE ztmm_assy_cost1-mcode OBLIGATORY,
                p_lifnr LIKE lfa1-lifnr,
                p_matnr LIKE mara-matnr,
                p_ekgrp LIKE ztmm_assy_cost1-ekgrp.

PARAMETERS:     p_datum LIKE sy-datum OBLIGATORY.

SELECT-OPTIONS: s_mstae FOR mara-mstae.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Initialization
INITIALIZATION.
  PERFORM initialization_rtn.

*----- Input value check & read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM read_data.

* F4 Values for MCODE
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_MCODE-LOW.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  p_mcode.
  DATA : BEGIN OF value_tab OCCURS 0,
            mcode  LIKE ztmm_assy_cost1-mcode,
           END OF value_tab.
* Select
  SELECT DISTINCT mcode  FROM ztmm_assy_cost1
             INTO TABLE value_tab.
  l_dyname = sy-repid.

* Set F4 values for Module Code
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'P_MCODE'
            dynpprog        = l_dyname
            dynpnr          = '1000'
            dynprofield     = 'MCODE'
            window_title    = 'Module Codes'
            value_org       = 'S'
       TABLES
            value_tab       = value_tab
       EXCEPTIONS
            parameter_error = 1.

* F4 Values for Vechicle type
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  s_vtype-low.

  DATA : BEGIN OF value_tab OCCURS 0,
            vtype  LIKE ztmm_assy_cost1-vtype,
           END OF value_tab.
* Select
  SELECT DISTINCT vtype  FROM ztmm_assy_cost1
             INTO TABLE value_tab.
  l_dyname = sy-repid.


* Set F4 values for Vehicle type
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'S_VTYPE'
            dynpprog        = l_dyname
            dynpnr          = '1000'
            dynprofield     = 'VTYPE'
            window_title    = 'Vehicle Type'
            value_org       = 'S'
       TABLES
            value_tab       = value_tab
       EXCEPTIONS
            parameter_error = 1.

* F4 Values for Purchasing group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  p_ekgrp.

  DATA : BEGIN OF value_tab OCCURS 0,
            ekgrp LIKE t024-ekgrp,
            eknam LIKE t024-eknam,
           END OF value_tab.
* Select
  SELECT  ekgrp eknam  FROM t024
             INTO TABLE value_tab.
  l_dyname = sy-repid.

  t_return-retfield = 'EKGRP'.
  t_return-recordpos = '1'.
  t_return-fieldname = 'S_EKGRP'.
  APPEND t_return.

* Set  Purchasing Group
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'EKGRP'
            dynpprog        = l_dyname
            dynpnr          = '1000'
            dynprofield     = 'P_EKGRP'
            window_title    = 'Purchasing Group'
            value_org       = 'S'
       TABLES
            value_tab       = value_tab
*              RETURN_TAB      = T_RETURN
       EXCEPTIONS
            parameter_error = 1.



*----- Display Header
TOP-OF-PAGE.
  PERFORM display_base_header.

TOP-OF-PAGE DURING LINE-SELECTION.
  CASE sy-pfkey.
    WHEN 'BASE'.
      PERFORM display_base_header.
    WHEN 'DETAIL'.
      PERFORM display_detail_header.
    WHEN 'COMPARE'.
      PERFORM display_detail_compare.
    WHEN 'SUB'.
      PERFORM display_sub_part.
  ENDCASE.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  SET TITLEBAR  'BASE'.
  IF it_module[] IS INITIAL.
    MESSAGE s000(zz) WITH 'No available data'.
  ELSE.
    SORT it_module BY indicator lifnr matnr.
    PERFORM display_data.
  ENDIF.
*----- Double click
AT LINE-SELECTION.
  PERFORM double_click_rtn.

*----- Function Key
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'COMPARE'.
      PERFORM compare_rtn.
    WHEN 'DETAIL'.
      PERFORM detail_rtn.
    WHEN 'HISTORY'.
      PERFORM history_rtn.
    WHEN 'MSG'.
      MESSAGE s000(zz) WITH it_module-msg(50) it_module-msg+50(50).
      CLEAR: it_module.
    WHEN 'SUB_INFO'.
      PERFORM display_sub_info_record.
    WHEN 'SUB_MSG'.
      MESSAGE s000(zz) WITH it_detail-msg(50) it_detail-msg+50(50).
      CLEAR: it_detail.
    WHEN 'ASCENDING'.
      PERFORM ascending_rtn.
    WHEN 'DESCENDING'.
      PERFORM descending_rtn.
    WHEN 'EXCEL'.
      PERFORM excel_download_rtn.
    WHEN 'PAGE'.
      PERFORM display_total_page.
** Changed by Furong on 02/10/10
    WHEN 'SSUM'.
      PERFORM sub_part_sum.
** End of change
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  initialization_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization_rtn.
  p_datum = sy-datum - 1.
ENDFORM.                    " initialization_rtn
*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
  PERFORM check_company.
  PERFORM check_vtype.
  PERFORM check_mcode.
  PERFORM check_lifnr.
  PERFORM check_matnr.
  PERFORM check_ekgrp.
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_company
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_company.
  SELECT SINGLE * FROM t001 WHERE bukrs = c_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.
ENDFORM.                    " check_company
*&---------------------------------------------------------------------*
*&      Form  check_vtype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vtype.
  LOOP AT s_vtype.
    TRANSLATE s_vtype-low  TO UPPER CASE.
    TRANSLATE s_vtype-high TO UPPER CASE.
    MODIFY s_vtype.
  ENDLOOP.

  IF s_vtype-low EQ ' '.
    wa_vtype_t = 'ZZ'.
  ELSE.
    wa_vtype_f = wa_vtype_t = s_vtype-low.
  ENDIF.
ENDFORM.                    " check_vtype
*&---------------------------------------------------------------------*
*&      Form  check_mcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mcode.
*  IF S_MCODE EQ ' '.
*    WA_MCODE_T = 'ZZ'.
*    MOVE '%' TO L_MCODE.
*  ELSE.
  wa_mcode_f = wa_mcode_t = p_mcode.
  CONCATENATE '%' p_mcode '%' INTO l_mcode.
*  ENDIF.
ENDFORM.                    " check_mcode
*&---------------------------------------------------------------------*
*&      Form  check_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lifnr.
  IF p_lifnr EQ ' '.
    wa_lifnr_t = 'ZZZZZZZZZZ'.
  ELSE.
    SELECT SINGLE * FROM lfa1 WHERE lifnr = p_lifnr.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m03.
    ENDIF.
    wa_lifnr_f = wa_lifnr_t = p_lifnr.
  ENDIF.
*  IF S_LIFNR-LOW EQ ' '.
*    WA_LIFNR_T = 'ZZZZZZZZZZ'.
*  ELSE.
*  WA_LIFNR_F = WA_LIFNR_T = P_LIFNR.
*  ENDIF.
ENDFORM.                    " check_lifnr
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.

  IF p_matnr EQ ' '.
    wa_matnr_t = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSE.
    SELECT SINGLE * FROM mara WHERE matnr = p_matnr.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m04.
    ENDIF.
    wa_matnr_t = wa_matnr_f = p_matnr.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_ekgrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ekgrp.
  IF p_ekgrp EQ ' '.
    wa_ekgrp_t = 'ZZZ'.
  ELSE.
    SELECT SINGLE * FROM t024 WHERE ekgrp = p_ekgrp.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.
    wa_ekgrp_t = wa_ekgrp_f = p_ekgrp.
  ENDIF.
ENDFORM.                    " check_ekgrp
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM get_sub_price.
  PERFORM calculate_module_price.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  get_sub_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_price.
  DATA: c_capid   LIKE rc29l-capid VALUE 'PP01',
        l_matnr LIKE mara-matnr,
        l_name1 LIKE lfa1-name1,
        l_max_date LIKE sy-datum,
        l_max_time LIKE sy-uzeit,
        l_asytr LIKE it_output-asytr,
        l_count TYPE i,
        l_curr_lines TYPE i,
*        LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE.
     lt_stb TYPE ztmm_mod_pri_bk OCCURS 0 WITH HEADER LINE.

  DATA: lt_cost1 LIKE TABLE OF ztmm_assy_cost1 WITH HEADER LINE,
        lt_tab LIKE TABLE OF it_tab WITH HEADER LINE.

  SELECT * INTO TABLE lt_cost1
     FROM ztmm_assy_cost1
     WHERE  vtype BETWEEN wa_vtype_f AND wa_vtype_t
        AND mcode = p_mcode
        AND lifnr    BETWEEN  wa_lifnr_f AND wa_lifnr_t
        AND ekgrp    BETWEEN  wa_ekgrp_f AND wa_ekgrp_t
        AND datab <= p_datum
        AND datbi >= p_datum .

  LOOP AT lt_cost1.
    IF p_matnr IS INITIAL.
      CONCATENATE '%' lt_cost1-vtype p_mcode '%M1%' INTO l_matnr.
      SELECT g~matnr AS matnr
             d~maktx
             m~meins AS unit m~meins AS meins
             INTO CORRESPONDING FIELDS OF TABLE lt_tab
                FROM ztmm_mod_pri_bk AS g
                INNER JOIN mara AS m ON
                      g~matnr = m~matnr
                INNER JOIN makt AS d ON
                       m~matnr = d~matnr
                   WHERE g~matnr LIKE l_matnr
                      AND m~lvorm = ''
                      AND m~mstae IN s_mstae
                      AND d~spras = sy-langu.
    ELSE.
      SELECT g~matnr AS matnr
             d~maktx
             m~meins AS unit m~meins AS meins
             INTO CORRESPONDING FIELDS OF TABLE lt_tab
                FROM ztmm_mod_pri_bk AS g
                INNER JOIN mara AS m ON
                      g~matnr = m~matnr
                INNER JOIN makt AS d ON
                       m~matnr = d~matnr
                   WHERE g~matnr = p_matnr
                      AND m~lvorm = ''
                     AND m~mstae IN s_mstae
                      AND d~spras = sy-langu.
      LOOP AT lt_tab.
        IF ( lt_cost1-vtype = lt_tab-matnr+0(3) )
           AND ( p_mcode = lt_tab-matnr+3(2) ).
        ELSE.
          DELETE lt_tab.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT SINGLE name1 INTO l_name1
      FROM lfa1
      WHERE lifnr = lt_cost1-lifnr.
    LOOP AT lt_tab.
      it_tab = lt_tab.
      it_tab-name1 = l_name1.
      it_tab-lifnr = lt_cost1-lifnr.
      it_tab-vtype = lt_cost1-vtype.
      it_tab-ekgrp = lt_cost1-ekgrp.
      it_tab-asytr = lt_cost1-asytr.
      it_tab-datab = lt_cost1-datab.
      it_tab-datbi = lt_cost1-datbi.
      COLLECT it_tab.
    ENDLOOP.
    REFRESH lt_tab.
  ENDLOOP.



  LOOP AT it_tab.

    MOVE-CORRESPONDING it_tab TO it_output.
    CLEAR: l_max_date, l_max_time, l_asytr.
    REFRESH lt_stb.
    SELECT MAX( run_date ) "MAX( run_time )
        INTO (l_max_date)  "l_max_time)
        FROM ztmm_mod_pri_bk
         WHERE matnr = it_tab-matnr
          AND input_date <= p_datum.

    IF sy-subrc = 0.
      clear: l_curr_lines.
      DESCRIBE TABLE it_output LINES l_curr_lines.

      SELECT * INTO TABLE lt_stb
      FROM ztmm_mod_pri_bk
      WHERE matnr = it_tab-matnr
        AND run_date = l_max_date.
*        AND run_time = l_max_time.
      SORT lt_stb BY run_time DESCENDING.
      READ TABLE lt_stb INDEX 1.
      l_max_time = lt_stb-run_time.
      LOOP AT lt_stb.
        IF lt_stb-run_time = l_max_time.

          IF lt_stb-asytr > 0.
            l_asytr = lt_stb-asytr.
          ENDIF.
          it_output-comp = lt_stb-comp.
          it_output-qnty = lt_stb-qnty.
          it_output-datab = lt_stb-datab.
          it_output-datbi = lt_stb-datbi.
          it_output-cdatab = lt_stb-datab.
          it_output-cdatbi = lt_stb-datbi.
          it_output-upgvc = lt_stb-upgvc.
          it_output-netpr = lt_stb-netpr.
          it_output-peinh  = lt_stb-peinh.
          it_output-zp12 = lt_stb-zp12.
          it_output-zp13  = lt_stb-zp13.
          it_output-kzust  = lt_stb-kzust.
          it_output-dmbtr = lt_stb-dmbtr.
          it_output-waers = lt_stb-waers.
          SELECT SINGLE maktx INTO it_output-cmaktx
            FROM makt
            WHERE matnr = lt_stb-comp.
          APPEND it_output.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
      l_curr_lines = l_curr_lines + 1.
      READ TABLE it_output INDEX l_curr_lines.
      it_output-asytr  = l_asytr.
      MODIFY it_output INDEX l_curr_lines.
*  MODIFY TABLE it_output TRANSPORTING asytr.
    ENDIF.

  ENDLOOP.


  PERFORM append_itab.
ENDFORM.                    " get_sub_price
*&---------------------------------------------------------------------*
*&      Form  APPEND_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_itab.
  DATA: lw_continue VALUE 'X'.

  LOOP AT it_output.
    ON CHANGE OF it_output-matnr OR it_output-lifnr.
      PERFORM read_module_info_record.
    ENDON.

    PERFORM read_sub_info_record.
  ENDLOOP.
ENDFORM.                    " APPEND_ITAB
*&---------------------------------------------------------------------*
*&      Form  calculate_module_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_module_price.
  DATA: lw_amount TYPE f,
        lw_mtot LIKE it_module-moamt.

  DATA: lw_detail LIKE it_detail.

  SORT it_module BY vtype matnr lifnr.
  SORT it_sub BY vtype matnr upgvc pref.

  LOOP AT it_module.
    CLEAR: it_module-dmamt , it_module-zp12,
           it_module-zp13, it_module-ztir.
    LOOP AT it_sub WHERE vtype = it_module-vtype
                     AND matnr = it_module-matnr.
      it_module-dmamt = it_module-dmamt + it_sub-amount.
      it_module-zp12 = it_module-zp12 + it_sub-zp12.
      it_module-zp13 = it_module-zp13 + it_sub-zp13.
      it_module-ztir = it_module-ztir + it_sub-ztir
                       / it_sub-peinh * it_sub-qnty.

      IF it_sub-sts    NE 0 AND
         it_module-sts EQ c_exist.
        it_module-sts = c_sub_err.
      ENDIF.
    ENDLOOP.

    MOVE: it_module-dmamt TO it_module-dmbtr.
    it_module-moamt = it_module-asytr + it_module-dmbtr.

    lw_mtot = it_module-netpr + it_module-ztir.
    IF   it_module-moamt NE  lw_mtot AND
       ( it_module-sts   EQ c_exist OR
         it_module-sts   EQ c_sub_err  ).
      MOVE: c_diff    TO it_module-sts,
            text-b12  TO it_module-msg.
    ENDIF.

    CASE it_module-sts.
      WHEN c_new OR c_no_cond.
        MOVE: text-b04 TO it_module-msg.
        MOVE: c_error  TO it_module-indicator.
      WHEN c_deleted.
        MOVE: text-b05 TO it_module-msg.
        MOVE: c_error  TO it_module-indicator.
      WHEN c_del_con.
        MOVE: text-b06 TO it_module-msg.
        MOVE: c_error  TO it_module-indicator.
      WHEN c_no_matl.
        MOVE: text-b07 TO it_module-msg.
        MOVE: c_error  TO it_module-indicator.
      WHEN c_exist.
        MOVE: text-b09 TO it_module-msg.
        MOVE: c_success   TO it_module-indicator.
      WHEN c_sub_err.
        MOVE: text-b10  TO it_module-msg.
        MOVE: c_warning TO it_module-indicator.
      WHEN c_diff.
        MOVE: text-b12  TO it_module-msg.
        MOVE: c_error  TO it_module-indicator.
    ENDCASE.

    MODIFY it_module.
  ENDLOOP.
ENDFORM.                    " calculate_module_price
*&---------------------------------------------------------------------*
*&      Form  read_module_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_module_info_record.
  PERFORM check_module.
  PERFORM append_it_module.
ENDFORM.                    " read_module_info_record
*&---------------------------------------------------------------------*
*&      Form  check_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_module.
*----- Check Info Record Deletion
  DATA: BEGIN OF lt_condition OCCURS 0,
          kbetr LIKE zvmm_info_condi-kbetr,
          kpein LIKE zvmm_info_condi-kpein,
          kmein LIKE zvmm_info_condi-kmein,
          kschl LIKE zvmm_info_condi-kschl,
          kzust LIKE zvmm_info_condi-kzust,
        END   OF lt_condition.
  DATA: lw_mtot LIKE wa_module-netpr.

  CLEAR: eina, a018.

  MOVE-CORRESPONDING  it_output TO wa_module.
  CLEAR: wa_module-netpr, wa_module-peinh, wa_module-meins,
         wa_module-msg,  wa_module-sts.

*----- Check Material Master
  IF it_output-maktx IS INITIAL.
    MOVE: c_no_matl TO wa_module-sts.
    EXIT.
  ENDIF.

  SELECT SINGLE matnr a~loekz wglif
    INTO (eina-matnr,eina-loekz,eina-wglif)
    FROM eina AS a INNER JOIN eine AS b
      ON a~infnr = b~infnr
   WHERE a~matnr = it_output-matnr
     AND a~lifnr = it_output-lifnr
     AND a~loekz = ' '
     AND b~werks = ' '
     AND b~ekorg = c_ekorg
     AND b~loekz = ' '.
  IF sy-subrc NE 0.
    MOVE: c_new    TO wa_module-sts.
    EXIT.
  ENDIF.

  IF eina-loekz EQ 'X'.
    MOVE: c_deleted TO wa_module-sts.
    EXIT.
  ENDIF.

*----- Read Module price
  SELECT SINGLE *
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_output-matnr
     AND lifnr =  it_output-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab <= p_datum
     AND datbi >= p_datum.
  IF sy-subrc NE 0.
    MOVE: c_no_cond TO wa_module-sts.
    EXIT.
  ELSE.
    MOVE: c_exist   TO wa_module-sts.
  ENDIF.

  SELECT kbetr kpein kmein kzust kschl
   INTO CORRESPONDING FIELDS OF TABLE lt_condition
   FROM zvmm_info_condi
  WHERE knumh = a018-knumh
    AND ( kschl = c_kschl   OR
          kschl = c_ztir    OR
          kschl LIKE 'ZP%' )
    AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    MOVE: c_del_con TO wa_module-sts.
    EXIT.
  ENDIF.

  SORT lt_condition BY kschl.
  LOOP AT lt_condition.
    CASE lt_condition-kschl.
      WHEN c_kschl.
        MOVE: lt_condition-kbetr TO wa_module-netpr,
              lt_condition-kpein TO wa_module-peinh,
              lt_condition-kmein TO wa_module-meins,
              lt_condition-kzust TO wa_module-kzust.
      WHEN c_ztir.
        lw_mtot = wa_module-netpr + lt_condition-kbetr.
        MOVE: lw_mtot TO wa_module-netpr.
      WHEN OTHERS.
        MOVE: lt_condition-kschl+2(2) TO w_index.

        CONCATENATE: 'WA_MODULE-ZP' w_index INTO w_module.

        ASSIGN: (w_module) TO <module>.
        IF sy-subrc NE 0. CONTINUE. ENDIF.

        <module> = lt_condition-kbetr.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " check_module
*&---------------------------------------------------------------------*
*&      Form  append_it_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_it_module.
  CLEAR: it_module.

  IF it_module-peinh EQ 0.
    it_module-peinh = 1.
  ENDIF.

  MOVE: t001-waers TO wa_module-waers.

  MOVE: wa_module TO it_module.

  APPEND it_module.
  CLEAR: it_module.
ENDFORM.                    " append_it_module
*&---------------------------------------------------------------------*
*&      Form  read_sub_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_sub_info_record.
  DATA: lw_sub(50).
  FIELD-SYMBOLS: <lw_sub>.
  CLEAR: it_sub.

*  READ TABLE it_sub WITH KEY comp = it_output-comp.
*  IF sy-subrc NE 0.
  MOVE-CORRESPONDING  it_output TO it_sub.
  MOVE-CORRESPONDING  it_sub TO wa_sub.
*    PERFORM CHECK_RTN.
  PERFORM append_sub_price.
*  ELSE.
*      MOVE-CORRESPONDING  it_sub TO wa_sub.
**    MOVE: it_sub-lifnr  TO wa_sub-lifnr,
**          it_sub-amount TO wa_sub-amount,
**          it_sub-kmein  TO wa_sub-kmein,
**          it_sub-netpr  TO wa_sub-netpr,
**          it_sub-peinh  TO wa_sub-peinh,
**          it_sub-waers  TO wa_sub-waers,
**          it_sub-kzust  TO wa_sub-kzust,
**          it_sub-sts    TO wa_sub-sts,
**          it_sub-msg    TO wa_sub-msg,
**          it_sub-ztir   TO wa_sub-ztir,
**          it_sub-datab  TO wa_sub-datab,
**          it_sub-datbi  TO wa_sub-datbi.
*    DO.
*      MOVE: sy-index TO w_index.
*
*      CONCATENATE: 'WA_SUB-ZP' w_index INTO lw_sub,
*                   'IT_SUB-ZP' w_index INTO w_sub.
*
*      ASSIGN: (w_sub)  TO <sub>,
*              (lw_sub) TO <lw_sub>.
*      IF sy-subrc NE 0. EXIT. ENDIF.
*
*      MOVE: <sub> TO <lw_sub>.
*    ENDDO.
*
*    IF NOT ( it_sub-sts EQ c_no_matl OR
*             it_sub-sts EQ c_no_cond OR
*             it_sub-sts EQ c_no_info    ).
*      IF NOT ( ( it_sub-meins EQ wa_sub-kmein AND
*                 it_sub-meins EQ it_sub-unit  AND
*                 wa_sub-kmein EQ it_sub-unit )   ).
*        MOVE: c_uom_err TO wa_sub-sts.
*      ENDIF.
*    ENDIF.
*
*    PERFORM append_sub_price.
*  ENDIF.
ENDFORM.                    " read_sub_info_record
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
*----- Check Material Master
*  DATA: BEGIN OF LT_CONDITION OCCURS 0,
*          KBETR LIKE ZVMM_INFO_CONDI-KBETR,
*          KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
*          KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
*          KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
*          KZUST LIKE ZVMM_INFO_CONDI-KZUST,
*        END   OF LT_CONDITION.
*
*  DATA: BEGIN OF IT_EINA_EINE_TEMP OCCURS 0,
*        MATNR LIKE EINA-MATNR,
*        LIFNR LIKE EINA-LIFNR,
*        WGLIF LIKE EINA-WGLIF,
*        END OF IT_EINA_EINE_TEMP.

  IF it_output-cmaktx IS INITIAL.
    MOVE: c_no_matl TO wa_sub-sts.
    EXIT.
  ENDIF.

*  SELECT MATNR A~LIFNR WGLIF INTO TABLE IT_EINA_EINE_TEMP
*   FROM EINA AS A INNER JOIN EINE AS B
*     ON A~INFNR = B~INFNR
*  WHERE A~MATNR = IT_OUTPUT-COMP
*    AND A~URZZT = 'SUB'
*    AND A~LOEKZ = ' '
*    AND B~WERKS = ' '
*    AND B~EKORG = C_EKORG
*    AND B~LOEKZ = ' '.
*  IF SY-SUBRC NE 0.
*    MOVE: C_NO_INFO TO WA_SUB-STS.
*    EXIT.
*  ENDIF.
*
**----- Read submaterial price
*  LOOP AT IT_EINA_EINE_TEMP.
*    CLEAR: EINA, A018.
*    WA_SUB-LIFNR = IT_EINA_EINE_TEMP-LIFNR.
*    SELECT SINGLE KNUMH DATAB DATBI
*                    INTO (A018-KNUMH, WA_SUB-DATAB, WA_SUB-DATBI)
*                    FROM A018
*                    WHERE KAPPL =  'M'
*                      AND KSCHL =  'PB00'
*                      AND MATNR =  IT_OUTPUT-COMP
*                      AND LIFNR =  WA_SUB-LIFNR
*                      AND EKORG =  C_EKORG
*                      AND ESOKZ =  '0'
*                      AND DATAB <= P_DATUM
*                      AND DATBI >= P_DATUM.
*    IF SY-SUBRC EQ 0.
*      EXIT.
*    ENDIF.
*  ENDLOOP.

*  IF A018-KNUMH IS INITIAL.
*    MOVE: C_NO_COND TO WA_SUB-STS.
*    EXIT.
*  ENDIF.

*  SELECT SINGLE KBETR KPEIN KMEIN KZUST
*    INTO (WA_SUB-NETPR,WA_SUB-PEINH,
*          WA_SUB-KMEIN,WA_SUB-KZUST)
*    FROM ZVMM_INFO_CONDI
*   WHERE KNUMH = A018-KNUMH
*     AND KSCHL = C_KSCHL
*     AND LOEVM_KO = ' '.
*  IF SY-SUBRC NE 0.
*    MOVE: C_NO_COND TO WA_SUB-STS.
*    EXIT.
*  ENDIF.
*
*  SELECT KBETR KPEIN KMEIN KZUST KSCHL
*    INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
*    FROM ZVMM_INFO_CONDI
*   WHERE KNUMH = A018-KNUMH
*     AND ( KSCHL = C_KSCHL   OR
*           KSCHL = 'ZTIR'    OR
*           KSCHL LIKE 'ZP%' )
*     AND LOEVM_KO = ' '.
*  IF SY-SUBRC NE 0.
*    MOVE: C_NO_COND TO WA_SUB-STS.
*    EXIT.
*  ENDIF.

*  SORT LT_CONDITION BY KSCHL.
*  LOOP AT LT_CONDITION.
*    CASE LT_CONDITION-KSCHL.
*      WHEN C_KSCHL.
*        MOVE: LT_CONDITION-KBETR TO WA_SUB-NETPR,
*              LT_CONDITION-KPEIN TO WA_SUB-PEINH,
*              LT_CONDITION-KMEIN TO WA_SUB-KMEIN,
*              LT_CONDITION-KZUST TO WA_SUB-KZUST.
*        IF IT_EINA_EINE_TEMP-WGLIF = 'ZTIR'.
*          WA_SUB-ZTIR = LT_CONDITION-KBETR.
*        ENDIF.
*
*      WHEN OTHERS.
*        MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.
*
*        CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO W_SUB.
*
*        ASSIGN: (W_SUB) TO <SUB>.
*        IF SY-SUBRC NE 0. CONTINUE. ENDIF.
*
*        <SUB> = LT_CONDITION-KBETR.
*    ENDCASE.
*  ENDLOOP.

**----- A sub material's UoM must be 'EA'.
**----- If UoM is not 'EA', display error message.
*  IF NOT ( ( IT_SUB-MEINS EQ WA_SUB-KMEIN AND
*             IT_SUB-MEINS EQ IT_SUB-UNIT  AND
*             WA_SUB-KMEIN EQ IT_SUB-UNIT )   ).
*    MOVE: C_UOM_ERR TO WA_SUB-STS.
*  ENDIF.

ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  append_sub_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_sub_price.
*  CLEAR: it_sub.

  CASE wa_sub-sts.
    WHEN c_no_info.
      MOVE: text-b02 TO wa_sub-msg.
    WHEN c_no_cond.
      MOVE: text-b03 TO wa_sub-msg.
    WHEN c_uom_err.
      MOVE: text-b01 TO wa_sub-msg.
    WHEN c_no_matl.
      MOVE: text-b07 TO wa_sub-msg.
  ENDCASE.

  MOVE: wa_module-vtype TO wa_sub-vtype,
        wa_module-matnr TO wa_sub-matnr.

*  MOVE: wa_sub TO it_sub.
*  MOVE-CORRESPONDING it_output TO it_sub.
  it_sub-maktx = it_output-cmaktx.
*  it_sub-lifnr  = wa_sub-lifnr.
*  it_sub-datab = wa_sub-datab.
*  it_sub-datbi = wa_sub-datbi.
*
*  IF it_sub-datab IS INITIAL.
*    it_sub-datab = it_output-cdatab.
*  ENDIF.
*
*  IF it_sub-datbi IS INITIAL.
*    it_sub-datbi = it_output-cdatbi.
*  ENDIF.

  IF it_sub-peinh EQ 0.
    it_sub-peinh = 1.
  ENDIF.

  it_sub-amount = it_sub-qnty * it_sub-netpr / it_sub-peinh.

  APPEND it_sub.
  CLEAR: it_sub, wa_sub.
ENDFORM.                    " append_sub_price
*&---------------------------------------------------------------------*
*&      Form  display_base_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_base_header.
  SET LEFT SCROLL-BOUNDARY COLUMN 84.

*  READ TABLE S_LIFNR INDEX 1.
*  IF SY-SUBRC NE 0.
*    CLEAR: LFA1.
*  ENDIF.
*
*  READ TABLE S_EKGRP INDEX 1.
*  IF SY-SUBRC NE 0.
*    CLEAR: T024.
*  ENDIF.

  WRITE: AT /1(sy-linsz) text-h01 CENTERED.

  SKIP.
  WRITE:/2   text-h02, (3) s_vtype-low, p_mcode.
  WRITE:/2   text-h04, p_lifnr, lfa1-name1.
  WRITE:/2   text-h05, (18) p_matnr,
         153 text-h08, sy-datum.
  WRITE:/2   text-h06, (18) p_ekgrp,
         153 text-h09, sy-uzeit.
  WRITE:/2   text-h07, (18) p_datum,
         153 text-h10, (4) sy-pagno NO-GAP,'/', (4) wa_tot_page NO-ZERO.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/    text-h11 NO-GAP, text-h12.
  ULINE.

  MOVE: sy-pagno TO wa_tot_page.
ENDFORM.                    " display_base_header
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  NEW-PAGE LINE-SIZE 180 LINE-COUNT 58.

  LOOP AT it_module.
    NEW-LINE.

    SET LEFT SCROLL-BOUNDARY COLUMN 84.

    PERFORM set_format.

    IF sy-linno EQ 90. ULINE. ENDIF.

    PERFORM display_line.

    AT NEW matnr.
      PERFORM display_matnr.
    ENDAT.

    AT NEW lifnr.
      PERFORM display_lifnr.
    ENDAT.

    IF sy-linno EQ 11.
      PERFORM display_matnr.
      PERFORM display_lifnr.
    ENDIF.

    AT LAST. ULINE. ENDAT.
  ENDLOOP.

  CLEAR: it_module.
  SET USER-COMMAND 'PAGE'.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  set_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_format.
  DATA: lw_color_flg TYPE i.

  lw_color_flg = sy-tabix MOD 4.

  IF lw_color_flg EQ 1 OR
     lw_color_flg EQ 2.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

*  CASE it_module-indicator.
*    WHEN c_error.
*      FORMAT COLOR COL_NEGATIVE   INTENSIFIED ON.
*    WHEN c_success.
*      FORMAT COLOR COL_POSITIVE   INTENSIFIED OFF.
*    WHEN c_incorrect.
*      FORMAT COLOR COL_BACKGROUND INTENSIFIED ON.
*  ENDCASE.
ENDFORM.                    " set_format
*&---------------------------------------------------------------------*
*&      Form  display_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_line.
  WRITE:       '|' NO-GAP,
               it_module-chbox AS CHECKBOX,
          (03) space NO-GAP, '|' NO-GAP,
          (10) space NO-GAP, '|' NO-GAP,
          (20) space NO-GAP, '|' NO-GAP,
          (18) space NO-GAP, '|' NO-GAP,
          (20) space NO-GAP, '|' NO-GAP.

* SELECT SINGLE b~effpr INTO eine-effpr
*              FROM eina AS a INNER JOIN eine AS b
*              ON a~infnr = b~infnr
*              WHERE a~matnr = it_module-matnr
*                AND a~lifnr = it_module-lifnr
*                AND a~loekz = ' '
*                AND b~werks = ' '
*                AND b~ekorg = c_ekorg
*                AND b~loekz = ' '.
  IF it_module-netpr = it_module-moamt.
    WRITE: (4) icon_green_light  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
  ELSE.
    DATA: lw_red(1).
    PERFORM set_it_detail.
    PERFORM check_it_detail_light USING lw_red.
    IF lw_red IS INITIAL.
      WRITE: (4) icon_yellow_light AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
    ELSE.
      WRITE: (4) icon_red_light  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
    ENDIF.
  ENDIF.

*  CASE it_module-indicator.
*    WHEN c_ready.
*      WRITE: (4) icon_light_out    AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
*    WHEN c_warning.
*      WRITE: (4) icon_yellow_light AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
*    WHEN c_success.
*      WRITE: (4) icon_green_light  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
*    WHEN c_error OR c_incorrect.
*      WRITE: (4) icon_red_light    AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
*  ENDCASE.

  WRITE: (03) it_module-waers NO-GAP, '|' NO-GAP,
         (09) it_module-netpr CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) it_module-moamt CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) it_module-asytr CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) it_module-dmbtr CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) it_module-ztir CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) it_module-zp12  CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) it_module-zp13  CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
              it_module-datab NO-GAP, '|' NO-GAP,
              it_module-datbi NO-GAP,
              '|'.

  HIDE: it_module.
ENDFORM.                    " display_line
*&---------------------------------------------------------------------*
*&      Form  display_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_matnr.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 04(03) space,
         08(10) space,
         19(20) space,
         40(18) it_module-matnr HOTSPOT,
         59(20) it_module-maktx.
ENDFORM.                    " display_matnr
*&---------------------------------------------------------------------*
*&      Form  display_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_lifnr.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 04(03) it_module-vtype,
         08(10) it_module-lifnr,
         19(20) it_module-name1.
ENDFORM.                    " display_lifnr
*&---------------------------------------------------------------------*
*&      Form  double_click_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click_rtn.
  CASE sy-pfkey.
    WHEN 'BASE'.
      PERFORM double_click_base.
    WHEN 'DETAIL'.
      PERFORM double_click_detail.
    WHEN 'COMPARE'.
      PERFORM double_click_compare.
  ENDCASE.
ENDFORM.                    " double_click_rtn
*&---------------------------------------------------------------------*
*&      Form  double_click_base
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click_base.
  DATA: lw_field(40).

  CHECK NOT it_module IS INITIAL.

  GET CURSOR FIELD lw_field.

  CASE lw_field.
    WHEN 'IT_MODULE-MATNR'.
      PERFORM display_module_info_record.
    WHEN 'ICON_RED_LIGHT' OR 'ICON_YELLOW_LIGHT'.
*      MESSAGE s000(zz) WITH it_module-msg(50) it_module-msg+50(50).
      SET PF-STATUS 'DETAIL'.
      PERFORM display_sub_material.

    WHEN 'ICON_GREEN_LIGHT' OR 'ICON_LIGHT_OUT'.
      SET PF-STATUS 'DETAIL'.
      PERFORM display_sub_material.
    WHEN OTHERS.
      SET PF-STATUS 'DETAIL'.
      PERFORM display_sub_material.
  ENDCASE.

  CLEAR: it_module.
ENDFORM.                    " double_click_base
*&---------------------------------------------------------------------*
*&      Form  double_click_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click_detail.
  DATA: lw_field(40).

  CHECK NOT it_detail IS INITIAL.

  GET CURSOR FIELD lw_field.

  CASE lw_field.
    WHEN 'IT_DETAIL-IDNRK'.
      PERFORM display_sub_info_record.
    WHEN 'ICON_RED_LIGHT'.
      MESSAGE s000(zz) WITH it_detail-msg(50) it_detail-msg+50(50).
  ENDCASE.

  CLEAR: it_detail.
ENDFORM.                    " double_click_detail
*&---------------------------------------------------------------------*
*&      Form  compare_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compare_rtn.
  SET PF-STATUS 'COMPARE'.
  PERFORM checked_items.
  PERFORM set_it_compare_from_source.
  PERFORM display_compare.
  CLEAR: it_compare.
ENDFORM.                    " compare_rtn
*&---------------------------------------------------------------------*
*&      Form  detail_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detail_rtn.
  SET PF-STATUS 'DETAIL'.
  PERFORM read_selected_item.
  PERFORM display_sub_material.
ENDFORM.                    " detail_rtn
*&---------------------------------------------------------------------*
*&      Form  display_sub_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_sub_info_record.
  CASE sy-pfkey.
    WHEN 'DETAIL'.
      CHECK NOT it_detail IS INITIAL.

      SET PARAMETER ID 'LIF' FIELD it_detail-lifnr.
      SET PARAMETER ID 'MAT' FIELD it_detail-idnrk.
      SET PARAMETER ID 'EKO' FIELD c_ekorg.
      SET PARAMETER ID 'WRK' FIELD ''.

      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

      CLEAR: it_detail.
    WHEN 'COMPARE'.
      CHECK NOT it_compare IS INITIAL.

      SET PARAMETER ID 'LIF' FIELD it_compare-lifnr.
      SET PARAMETER ID 'MAT' FIELD it_compare-comp.
      SET PARAMETER ID 'EKO' FIELD c_ekorg.
      SET PARAMETER ID 'WRK' FIELD ''.

      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

      CLEAR: it_compare.
  ENDCASE.
ENDFORM.                    " display_sub_info_record
*&---------------------------------------------------------------------*
*&      Form  ascending_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ascending_rtn.
  CASE sy-pfkey.
    WHEN 'BASE'.
      PERFORM ascending_module.
    WHEN 'DETAIL'.
      PERFORM ascending_sub.
  ENDCASE.
ENDFORM.                    " ascending_rtn
*&---------------------------------------------------------------------*
*&      Form  ascending_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ascending_module.
  DATA: lw_field(40).

  CHECK NOT it_module IS INITIAL.

  GET CURSOR FIELD lw_field.

  lw_field = lw_field+10.

  SORT it_module BY (lw_field).

  sy-lsind = sy-lsind - 1.

  PERFORM display_data.
ENDFORM.                    " ascending_module
*&---------------------------------------------------------------------*
*&      Form  ascending_sub
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ascending_sub.
  DATA: lw_field(40).

  CHECK NOT it_detail IS INITIAL.

  GET CURSOR FIELD lw_field.

  lw_field = lw_field+10.

  SORT it_detail BY (lw_field).

  sy-lsind = sy-lsind - 1.

  PERFORM display_it_detail.
ENDFORM.                    " ascending_sub
*&---------------------------------------------------------------------*
*&      Form  display_total_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_page.
  DO wa_tot_page TIMES.
    READ LINE 7 OF PAGE sy-index.
    MODIFY LINE 7 OF PAGE sy-index
                     FIELD VALUE wa_tot_page FROM wa_tot_page.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " display_total_page
*&---------------------------------------------------------------------*
*&      Form  descending_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descending_rtn.
  CASE sy-pfkey.
    WHEN 'BASE'.
      PERFORM descending_module.
    WHEN 'DETAIL'.
      PERFORM descending_sub.
  ENDCASE.
ENDFORM.                    " descending_rtn
*&---------------------------------------------------------------------*
*&      Form  descending_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descending_module.
  DATA: lw_field(40).

  CHECK NOT it_module IS INITIAL.

  GET CURSOR FIELD lw_field.

  lw_field = lw_field+10.

  SORT it_module BY (lw_field) DESCENDING.

  sy-lsind = sy-lsind - 1.

  PERFORM display_data.
ENDFORM.                    " descending_module
*&---------------------------------------------------------------------*
*&      Form  descending_sub
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descending_sub.
  DATA: lw_field(40).

  CHECK NOT it_detail IS INITIAL.

  GET CURSOR FIELD lw_field.

  lw_field = lw_field+10.

  SORT it_detail BY (lw_field) DESCENDING.

  sy-lsind = sy-lsind - 1.

  PERFORM display_it_detail.
ENDFORM.                    " descending_sub
*&---------------------------------------------------------------------*
*&      Form  read_selected_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_selected_item.
  DO.
    CLEAR: it_module.

    READ LINE sy-index FIELD VALUE it_module-chbox.
    IF sy-subrc NE 0. EXIT. ENDIF.

    CHECK it_module-chbox EQ 'X'.

    READ TABLE it_module WITH KEY vtype = it_module-vtype
                                  lifnr = it_module-lifnr
                                  matnr = it_module-matnr
                                  datab = it_module-datab
                                  datbi = it_module-datbi.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    EXIT.
  ENDDO.

  IF it_module IS INITIAL.
    MESSAGE e000(zz) WITH text-m07.
  ENDIF.
ENDFORM.                    " read_selected_item
*&---------------------------------------------------------------------*
*&      Form  display_sub_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_sub_material.
  PERFORM set_it_detail.
  PERFORM display_it_detail.
ENDFORM.                    " display_sub_material
*&---------------------------------------------------------------------*
*&      Form  set_format_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_format_detail.
  IF wa_format_flg EQ 'X'.
    CLEAR: wa_format_flg.
  ELSE.
    wa_format_flg = 'X'.
  ENDIF.
ENDFORM.                    " set_format_detail
*&---------------------------------------------------------------------*
*&      Form  display_line_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_line_detail.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 1      '|' NO-GAP,
          (18) space NO-GAP, '|' NO-GAP,
               it_detail-sposn NO-GAP RIGHT-JUSTIFIED, '|' NO-GAP.

  IF it_detail-msg IS INITIAL.
    WRITE: (4) icon_green_light  AS ICON NO-GAP,'|' NO-GAP.
  ELSE.
    WRITE: (4) icon_red_light    AS ICON HOTSPOT NO-GAP,'|' NO-GAP.
  ENDIF.

  IF wa_format_flg EQ 'X'.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE: (18) it_detail-idnrk NO-GAP, '|' NO-GAP,
         (25) it_detail-maktx NO-GAP, '|' NO-GAP,
              it_detail-datab NO-ZERO NO-GAP, '|' NO-GAP,
              it_detail-datbi NO-ZERO NO-GAP, '|' NO-GAP,
         (04) it_detail-kmpmg UNIT it_detail-kmpme NO-ZERO NO-GAP,
              '|' NO-GAP,
              it_detail-kmpme NO-GAP, '|' NO-GAP,
         (09) it_detail-netpr CURRENCY it_detail-waers ,
         (09) it_detail-zp12  CURRENCY it_detail-waers ,
         (09) it_detail-zp13  CURRENCY it_detail-waers NO-GAP,
              '|' NO-GAP,
              it_detail-peinh NO-GAP, '|' NO-GAP,
              it_detail-kzust NO-GAP, '|' NO-GAP,
         (10) it_detail-dmbtr CURRENCY it_detail-waers NO-GAP, '|'.

  HIDE: it_detail.
ENDFORM.                    " display_line_detail
*&---------------------------------------------------------------------*
*&      Form  display_upgvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_upgvc.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 2(18) it_detail-upgvc.
ENDFORM.                    " display_upgvc
*&---------------------------------------------------------------------*
*&      Form  display_it_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_it_detail.
  NEW-PAGE LINE-SIZE 158 LINE-COUNT 58.

  LOOP AT it_detail.
    NEW-LINE.

    IF sy-linno EQ 90. ULINE. ENDIF.

    AT NEW upgvc.
      PERFORM set_format_detail.
    ENDAT.

    PERFORM display_line_detail.

    AT NEW upgvc.
      PERFORM display_upgvc.
    ENDAT.

    AT LAST. ULINE. ENDAT.
  ENDLOOP.

  CLEAR: it_detail.
ENDFORM.                    " display_it_detail
*&---------------------------------------------------------------------*
*&      Form  display_module_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_module_info_record.
  SET PARAMETER ID 'LIF' FIELD it_module-lifnr.
  SET PARAMETER ID 'MAT' FIELD it_module-matnr.
  SET PARAMETER ID 'EKO' FIELD c_ekorg.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

  CLEAR: it_module.
ENDFORM.                    " display_module_info_record
*&---------------------------------------------------------------------*
*&      Form  set_it_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_detail.
  CLEAR: it_detail, it_detail[].

  LOOP AT it_sub WHERE vtype = it_module-vtype
                   AND matnr = it_module-matnr.
    MOVE: it_sub-upgvc   TO it_detail-upgvc,
          it_sub-pref    TO it_detail-sposn,
          it_sub-comp    TO it_detail-idnrk,
          it_sub-maktx   TO it_detail-maktx,
          it_sub-qnty    TO it_detail-kmpmg,
          it_sub-unit    TO it_detail-kmpme,
          it_sub-lifnr   TO it_detail-lifnr,
          it_sub-datab   TO it_detail-datab,
          it_sub-datbi   TO it_detail-datbi,
          it_sub-netpr   TO it_detail-netpr,
          it_sub-zp12    TO it_detail-zp12,
          it_sub-zp13    TO it_detail-zp13,
          it_sub-peinh   TO it_detail-peinh,
          it_sub-kzust   TO it_detail-kzust,
          it_sub-amount  TO it_detail-dmbtr,
          it_sub-waers   TO it_detail-waers,
          it_sub-msg     TO it_detail-msg.
    APPEND it_detail.
  ENDLOOP.

  SORT it_detail BY upgvc sposn.
ENDFORM.                    " set_it_detail
*&---------------------------------------------------------------------*
*&      Form  display_detail_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_header.
  WRITE: AT /1(sy-linsz) text-h13 CENTERED.

  SKIP.
  WRITE:/2   text-h02, (03) it_module-vtype.
  WRITE:/2   text-h03,      it_module-lifnr,      it_module-name1,
         65  text-h14,      it_module-moamt CURRENCY it_module-waers.
  WRITE:/2   text-h05, (18) it_module-matnr, (25) it_module-maktx,
         65  text-h15,      it_module-asytr CURRENCY it_module-waers,
         141 text-h08,      sy-datum.
  WRITE:/2   text-h06, (18) it_module-ekgrp,
         65  text-h16,      it_module-dmbtr CURRENCY it_module-waers,
         141 text-h09,      sy-uzeit.
  WRITE:/2   text-h07, (18) p_datum,
         65  text-h17,      it_module-waers,
         141 text-h10, (04) sy-pagno NO-GAP,'/',(4) wa_tot_page NO-ZERO.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/    text-h18 NO-GAP, text-h19.
  ULINE.

  MOVE: sy-pagno TO wa_tot_page.
ENDFORM.                    " display_detail_header
*&---------------------------------------------------------------------*
*&      Form  history_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM history_rtn.
  PERFORM read_selected_item.

  SUBMIT zemmpm48r_module_assy_cost
    WITH s_vtype = it_module-vtype
    WITH s_ekgrp = it_module-ekgrp
    WITH s_mcode = it_module-mcode
     AND RETURN.

  CLEAR: it_module.
ENDFORM.                    " history_rtn
*&---------------------------------------------------------------------*
*&      Form  CHECKED_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM checked_items.
  DATA: lw_count TYPE i.                   "Item Count

  CLEAR: wa_compare.

  DO.
    CLEAR: it_module.

    READ LINE sy-index FIELD VALUE it_module-chbox.
    IF sy-subrc NE 0. EXIT. ENDIF.

    CHECK it_module-chbox EQ 'X'.

    READ TABLE it_module WITH KEY vtype = it_module-vtype
                                  lifnr = it_module-lifnr
                                  matnr = it_module-matnr
                                  datab = it_module-datab
                                  datbi = it_module-datbi.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    CASE lw_count.
      WHEN 0.
        MOVE: it_module-vtype     TO wa_compare-vtype_s,
              it_module-name1     TO wa_compare-name1_s,
              it_module-lifnr     TO wa_compare-lifnr_s,
              it_module-maktx     TO wa_compare-maktx_s,
              it_module-matnr     TO wa_compare-matnr_s,
              it_module-datab     TO wa_compare-datab_s,
              it_module-datbi     TO wa_compare-datbi_s,
              it_module-moamt     TO wa_compare-moamt_s,
              it_module-asytr     TO wa_compare-asytr_s,
              it_module-dmbtr     TO wa_compare-dmbtr_s,
              it_module-indicator TO wa_compare-indicator_s,
              it_module-waers     TO wa_compare-waers.
        lw_count = lw_count + 1.
      WHEN 1.
        MOVE: it_module-vtype     TO wa_compare-vtype_t,
              it_module-name1     TO wa_compare-name1_t,
              it_module-lifnr     TO wa_compare-lifnr_t,
              it_module-maktx     TO wa_compare-maktx_t,
              it_module-matnr     TO wa_compare-matnr_t,
              it_module-datab     TO wa_compare-datab_t,
              it_module-datbi     TO wa_compare-datbi_t,
              it_module-moamt     TO wa_compare-moamt_t,
              it_module-asytr     TO wa_compare-asytr_t,
              it_module-dmbtr     TO wa_compare-dmbtr_t,
              it_module-indicator TO wa_compare-indicator_t.
        lw_count = lw_count + 1.
        EXIT.
      WHEN OTHERS.
        MESSAGE e000(zz) WITH text-m08.
    ENDCASE.
  ENDDO.

  IF lw_count < 2.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.
ENDFORM.                    " CHECKED_ITEMS
*&---------------------------------------------------------------------*
*&      Form  set_it_compare_from_source
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_compare_from_source.
*----- Fill IT_COMPARE for Source Module
  CLEAR: it_compare, it_compare[].

  LOOP AT it_sub WHERE vtype = wa_compare-vtype_s
                   AND matnr = wa_compare-matnr_s.
    CLEAR: it_compare.
    MOVE: it_sub-upgvc   TO it_compare-upgvc,
          it_sub-comp    TO it_compare-comp,
          it_sub-lifnr   TO it_compare-lifnr,
          it_sub-msg     TO it_compare-msg,
          it_sub-pref    TO it_compare-pref,
          it_sub-qnty    TO it_compare-qnty_s,
          it_sub-unit    TO it_compare-unit_s,
          it_sub-datab   TO it_compare-datab_s,
          it_sub-datbi   TO it_compare-datbi_s,
          it_sub-netpr   TO it_compare-netpr_s,
          it_sub-peinh   TO it_compare-peinh_s,
          it_sub-kzust   TO it_compare-kzust_s,
          it_sub-amount  TO it_compare-netwr_s.
    APPEND it_compare.
  ENDLOOP.

*----- Fill IT_COMPARE for Target Module
  LOOP AT it_sub WHERE vtype = wa_compare-vtype_t
                   AND matnr = wa_compare-matnr_t.
    CLEAR: it_compare.

    READ TABLE it_compare WITH KEY upgvc = it_sub-upgvc
                                   comp  = it_sub-comp
                                   pref  = it_sub-pref.
    IF sy-subrc EQ 0.
      MOVE: it_sub-qnty    TO it_compare-qnty_t,
            it_sub-unit    TO it_compare-unit_t,
            it_sub-datab   TO it_compare-datab_t,
            it_sub-datbi   TO it_compare-datbi_t,
            it_sub-netpr   TO it_compare-netpr_t,
            it_sub-peinh   TO it_compare-peinh_t,
            it_sub-kzust   TO it_compare-kzust_t,
            it_sub-amount  TO it_compare-netwr_t.
      MODIFY it_compare INDEX sy-tabix.
    ELSE.
      MOVE: it_sub-upgvc   TO it_compare-upgvc,
            it_sub-comp    TO it_compare-comp,
            it_sub-lifnr   TO it_compare-lifnr,
            it_sub-msg     TO it_compare-msg,
            it_sub-pref    TO it_compare-pref,
            it_sub-qnty    TO it_compare-qnty_t,
            it_sub-unit    TO it_compare-unit_t,
            it_sub-datab   TO it_compare-datab_t,
            it_sub-datbi   TO it_compare-datbi_t,
            it_sub-netpr   TO it_compare-netpr_t,
            it_sub-peinh   TO it_compare-peinh_t,
            it_sub-kzust   TO it_compare-kzust_t,
            it_sub-amount  TO it_compare-netwr_t.
      APPEND it_compare.
    ENDIF.
  ENDLOOP.

  SORT it_compare BY upgvc comp.
ENDFORM.                    " set_it_compare_from_source
*&---------------------------------------------------------------------*
*&      Form  display_compare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_compare.
  NEW-PAGE LINE-SIZE 126.

  LOOP AT it_compare.
    NEW-LINE.

    PERFORM display_compare_line.

    AT NEW upgvc.
      PERFORM display_compare_upgvc.
    ENDAT.

    AT END OF upgvc.
      SUM.
      PERFORM display_compare_upgvc_sum.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " display_compare
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DETAIL_COMPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_compare.
  WRITE: AT /1(sy-linsz) text-h20 CENTERED.

  SKIP.

  FORMAT COLOR COL_POSITIVE INTENSIFIED ON.

  ULINE.
  WRITE:/01 '|' NO-GAP,
            text-h22, p_datum,
         30 '|',
         31 text-h23,  (18) wa_compare-matnr_s,
         78 '|' NO-GAP,
            text-h23,  (18) wa_compare-matnr_t,
        126 '|'.
  WRITE:/01 '|',
         30 '|',
         46(31) wa_compare-maktx_s, '|' NO-GAP,
         94(31) wa_compare-maktx_t, '|'.
  WRITE:/01 '|',
         30 '|',
         31 text-h24,   wa_compare-lifnr_s, (20) wa_compare-name1_s,
         78 '|' NO-GAP,
            text-h24,   wa_compare-lifnr_t, (20) wa_compare-name1_t,
         '|'.
  WRITE:/01 '|',
         30 '|',
         31 text-h14,      wa_compare-moamt_s CURRENCY wa_compare-waers,
         78 '|' NO-GAP,
            text-h14,      wa_compare-moamt_t CURRENCY wa_compare-waers,
        126 '|'.
  WRITE:/01 '|',
         30 '|',
         31 text-h15,      wa_compare-asytr_s CURRENCY wa_compare-waers,
         78 '|' NO-GAP,
            text-h15,      wa_compare-asytr_t CURRENCY wa_compare-waers,
        126 '|'.
  WRITE:/01 '|',
         30 '|',
         31 text-h14,      wa_compare-dmbtr_s CURRENCY wa_compare-waers,
         78 '|' NO-GAP,
            text-h14,      wa_compare-dmbtr_t CURRENCY wa_compare-waers,
        126 '|'.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/    text-h21.
  ULINE.
ENDFORM.                    " DISPLAY_DETAIL_COMPARE
*&---------------------------------------------------------------------*
*&      Form  display_compare_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_compare_line.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: '|' NO-GAP,
         (09) space,
         (18) it_compare-comp NO-GAP,'|' NO-GAP.

  IF it_compare-qnty_s EQ 0 OR
     it_compare-qnty_t EQ 0.
    FORMAT COLOR COL_GROUP INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE: (04) it_compare-qnty_s UNIT it_compare-unit_s NO-ZERO,
              it_compare-unit_s,
              it_compare-datbi_s NO-ZERO,
         (09) it_compare-netpr_s NO-ZERO,
         (03) it_compare-peinh_s NO-ZERO,
              it_compare-kzust_s,
         (09) it_compare-netwr_s NO-GAP NO-ZERO, '|' NO-GAP.

  IF it_compare-qnty_s EQ 0 OR
     it_compare-qnty_t EQ 0.
    FORMAT COLOR COL_GROUP INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE: (04) it_compare-qnty_t UNIT it_compare-unit_t NO-ZERO,
              it_compare-unit_t,
              it_compare-datbi_t NO-ZERO,
         (09) it_compare-netpr_t NO-ZERO,
         (03) it_compare-peinh_t NO-ZERO,
              it_compare-kzust_t,
         (09) it_compare-netwr_t NO-GAP NO-ZERO, '|'.

  HIDE it_compare.
ENDFORM.                    " display_compare_line
*&---------------------------------------------------------------------*
*&      Form  display_compare_upgvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_compare_upgvc.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 2(9) it_compare-upgvc.
ENDFORM.                    " display_compare_upgvc
*&---------------------------------------------------------------------*
*&      Form  display_compare_upgvc_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_compare_upgvc_sum.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.

  ULINE.

  WRITE:         '|' NO-GAP,
            (09) it_compare-upgvc,
            (18) text-b11 NO-GAP,
                 '|' NO-GAP,
          69(09) it_compare-netwr_s NO-GAP,
                 '|' NO-GAP,
         117(09) it_compare-netwr_t NO-GAP,
                 '|'.

  ULINE.
ENDFORM.                    " display_compare_upgvc_sum
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_COMPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click_compare.
  CHECK NOT it_compare IS INITIAL.

  SET PARAMETER ID 'LIF' FIELD it_compare-lifnr.
  SET PARAMETER ID 'MAT' FIELD it_compare-comp.
  SET PARAMETER ID 'EKO' FIELD c_ekorg.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

  CLEAR: it_compare.
ENDFORM.                    " DOUBLE_CLICK_COMPARE
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWNLOAD_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_download_rtn.
  CASE sy-pfkey.
    WHEN 'BASE'.
      PERFORM download_module_price.
    WHEN 'DETAIL'.
      PERFORM download_detail_price.
    WHEN 'SUB'.
      PERFORM download_sub_part.
    WHEN 'COMPARE'.
      PERFORM download_compare.
  ENDCASE.
ENDFORM.                    " EXCEL_DOWNLOAD_RTN
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_MODULE_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_module_price.
  DATA: BEGIN OF lt_download OCCURS 0,
          vtype     LIKE   ztmm_assy_cost1-vtype, "Vehicle type
          lifnr     LIKE   lfa1-lifnr,            "Vendor
          name1     LIKE   lfa1-name1,            "Vendor name
          matnr     LIKE   mara-matnr,            "Material
          maktx     LIKE   makt-maktx,            "Description
          ekgrp     LIKE   ekko-ekgrp,            "Purchasing Group
          waers     LIKE   t001-waers,            "Currency
          moamt(16),                              "Module Price
          asytr(16),                              "Module Assy Cost
          dmbtr(16),                              "Material Cost
          datab(10),                              "Valid on
          datbi(10),                              "Valid to
          msg(100),
  END   OF lt_download.

  LOOP AT it_module.
    MOVE-CORRESPONDING it_module TO lt_download.

    WRITE: it_module-moamt CURRENCY it_module-waers
                           TO lt_download-moamt,
           it_module-asytr CURRENCY it_module-waers
                           TO lt_download-asytr,
           it_module-dmbtr CURRENCY it_module-waers
                           TO lt_download-dmbtr,
           it_module-datab TO lt_download-datab,
           it_module-datbi TO lt_download-datbi.

    APPEND lt_download.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'C:\TEMP\MODULE_PRICE.XLS'
            filetype                = 'DAT'
       TABLES
            data_tab                = lt_download
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: it_module.
ENDFORM.                    " DOWNLOAD_MODULE_PRICE
*&---------------------------------------------------------------------*
*&      Form  download_detail_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_detail_price.
  DATA: BEGIN OF lt_download OCCURS 0,
          upgvc LIKE zsmm_sub_detail-upgvc,
          sposn LIKE zsmm_sub_detail-sposn,
          idnrk LIKE zsmm_sub_detail-idnrk,
          maktx LIKE zsmm_sub_detail-maktx,
          kmpmg(16),
          kmpme LIKE zsmm_sub_detail-kmpme,
          datab(10),
          datbi(10),
          netpr(14),
          peinh LIKE zsmm_sub_detail-peinh,
          kzust LIKE zsmm_sub_detail-kzust,
          dmbtr(16),
          waers LIKE zsmm_sub_detail-waers,
          lifnr LIKE zsmm_sub_detail-lifnr,
          msg LIKE zsmm_sub_detail-msg,
        END   OF lt_download.

  LOOP AT it_detail.
    MOVE-CORRESPONDING it_detail TO lt_download.

    WRITE: it_detail-netpr CURRENCY it_detail-waers
                           TO lt_download-netpr,
           it_detail-dmbtr CURRENCY it_detail-waers
                           TO lt_download-dmbtr,
           it_detail-kmpmg UNIT it_detail-kmpme
                           TO lt_download-kmpmg,
           it_detail-datab TO lt_download-datab,
           it_detail-datbi TO lt_download-datbi.

    APPEND lt_download.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'C:\TEMP\SUB_PRICE.XLS'
            filetype                = 'DAT'
       TABLES
            data_tab                = lt_download
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: it_detail.
ENDFORM.                    " download_detail_price
*&---------------------------------------------------------------------*
*&      Form  download_compare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_compare.
  DATA: BEGIN OF lt_download OCCURS 0,
          upgvc     LIKE   mara-matnr,              "UPG-VC
          pref      LIKE   stpo-posnr,              "ztbm_abxduldt-pref
          comp      LIKE   mara-matnr,              "BOM Component
          qnty_s(16),                               "Quantity
          unit_s    LIKE   mara-meins,              "Unit of measure(BOM
          datab_s(10),                              "Valid on(sub)
          datbi_s(10),                              "Valid to(Sub)
          netpr_s(14),                              "Component Amount
          peinh_s   LIKE   ekpo-peinh,              "Component PriceUnit
          kzust_s   LIKE   konh-kzust,              "Reason code
          netwr_s(16),
          qnty_t(16),                               "Quantity
          unit_t    LIKE   mara-meins,              "Unit of measure(BOM
          datab_t(10),                              "Valid on(sub)
          datbi_t(10),                              "Valid to(Sub)
          netpr_t(14),                              "Component Amount
          peinh_t   LIKE   ekpo-peinh,              "Component PriceUnit
          kzust_t   LIKE   konh-kzust,              "Reason code
          netwr_t(16),
          waers     LIKE   t001-waers,              "Currency
          lifnr     LIKE   lfa1-lifnr,              "Vendor
          msg(100),                                 "Message
  END   OF lt_download.

  LOOP AT it_compare.
    MOVE-CORRESPONDING it_compare TO lt_download.

    WRITE: it_compare-qnty_s UNIT it_compare-unit_s
                             TO lt_download-qnty_s,
           it_compare-datab_s TO lt_download-datab_s,
           it_compare-datbi_s TO lt_download-datbi_s,
           it_compare-netpr_s CURRENCY it_compare-waers
                              TO lt_download-netpr_s,
           it_compare-netwr_s CURRENCY it_compare-waers
                              TO lt_download-netwr_s,
           it_compare-qnty_t UNIT it_compare-unit_t
                             TO lt_download-qnty_t,
           it_compare-datab_t TO lt_download-datab_t,
           it_compare-datbi_t TO lt_download-datbi_t,
           it_compare-netpr_t CURRENCY it_compare-waers
                              TO lt_download-netpr_t,
           it_compare-netwr_t CURRENCY it_compare-waers
                              TO lt_download-netwr_t.

    APPEND lt_download.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'C:\TEMP\COMPARISON.XLS'
            filetype                = 'DAT'
       TABLES
            data_tab                = lt_download
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: it_compare.
ENDFORM.                    " download_compare
*&---------------------------------------------------------------------*
*&      Form  check_cockpit_module_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_CONTINUE  text
*----------------------------------------------------------------------*
*FORM check_cockpit_module_color USING p_continue.
*  DATA: lw_int_key(3).         " Internal Key Color
*
*  CHECK it_output-matnr+3(2) EQ 'CP' AND
*        it_output-stgb          EQ 'U'.
*
*  MOVE: it_output-matnr+10(3) TO lw_int_key.
*
*  SELECT SINGLE * FROM ztmm_cp_color WHERE COPIT EQ it_output-matnr
*                                       AND inkey EQ lw_int_key
*                                       AND submt EQ it_output-comp
*                                       AND datab <  p_datum
*                                       AND datbi >= p_datum.
*  IF sy-subrc NE 0.
*    CLEAR: p_continue.
*  ENDIF.
*ENDFORM.                    " check_cockpit_module_color
*&---------------------------------------------------------------------*
*&      Form  check_it_detail_light
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_RED  text
*----------------------------------------------------------------------*
FORM check_it_detail_light USING    p_lw_red TYPE c.
  DATA: w_red(1).
  LOOP AT it_detail.
    IF NOT it_detail-msg IS INITIAL.
      w_red = '1'.
    ENDIF.
  ENDLOOP.
  IF NOT w_red IS INITIAL.
    p_lw_red = w_red.
  ELSE.
    CLEAR: p_lw_red.
  ENDIF.
ENDFORM.                    " check_it_detail_light
*&---------------------------------------------------------------------*
*&      Form  SUB_PART_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_part_sum.
  CLEAR: it_subsum, it_subsum[].

  SET PF-STATUS 'SUB'.

  DO.
    CLEAR: it_module.

    READ LINE sy-index FIELD VALUE it_module-chbox.
    IF sy-subrc NE 0. EXIT. ENDIF.

    CHECK it_module-chbox EQ 'X'.

    READ TABLE it_module WITH KEY vtype = it_module-vtype
                                  lifnr = it_module-lifnr
                                  matnr = it_module-matnr
                                  datab = it_module-datab
                                  datbi = it_module-datbi.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    LOOP AT it_sub WHERE vtype = it_module-vtype
                     AND matnr = it_module-matnr.
      IF it_sub-msg IS INITIAL.
      ELSE.
        MOVE:    it_sub-comp    TO it_subsum-idnrk,
                 it_sub-maktx   TO it_subsum-maktx.
        COLLECT it_subsum.
      ENDIF.
    ENDLOOP.

  ENDDO.

  SORT it_subsum BY idnrk.

  IF it_subsum[] IS INITIAL.
    MESSAGE e000(zz) WITH 'No data selected'.
  ENDIF.

  NEW-PAGE LINE-SIZE 158 LINE-COUNT 58.

  LOOP AT it_subsum.
    NEW-LINE.

    IF sy-linno EQ 90.
      WRITE:
/1(54)'------------------------------------------------------' NO-GAP.

    ENDIF.

*    AT NEW UPGVC.
*      PERFORM SET_FORMAT_DETAIL.
*    ENDAT.
*
    PERFORM display_line_sub_sum.

*    AT NEW UPGVC.
*      PERFORM DISPLAY_UPGVC.
*    ENDAT.

    AT LAST.
      WRITE:
 /1(54)'------------------------------------------------------' NO-GAP.
    ENDAT.
  ENDLOOP.

  CLEAR: it_subsum.


ENDFORM.                    " SUB_PART_SUM
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LINE_SUB_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_line_sub_sum.
*  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 1      '|' NO-GAP.
*          (4) SPACE NO-GAP, '|' NO-GAP.
*               IT_DETAIL-SPOSN NO-GAP RIGHT-JUSTIFIED, '|' NO-GAP.

*  IF IT_DETAIL-MSG IS INITIAL.
*    WRITE: (4) ICON_GREEN_LIGHT  AS ICON NO-GAP,'|' NO-GAP.
*  ELSE.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: (8) icon_red_light    AS ICON HOTSPOT NO-GAP,'|' NO-GAP.
*  ENDIF.

*  IF WA_FORMAT_FLG EQ 'X'.
*    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*  ELSE.
*    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*  ENDIF.

  WRITE: (18) it_subsum-idnrk NO-GAP, '|' NO-GAP,
         (24) it_subsum-maktx NO-GAP, '|' NO-GAP.

  HIDE: it_detail.

ENDFORM.                    " DISPLAY_LINE_SUB_SUM
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SUB_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_sub_part.
  DATA: BEGIN OF lt_download OCCURS 0,
        idnrk LIKE zsmm_sub_detail-idnrk,
        maktx LIKE zsmm_sub_detail-maktx,
        END   OF lt_download.

  LOOP AT it_subsum.
    MOVE-CORRESPONDING it_subsum TO lt_download.

*    WRITE: IT_DETAIL-NETPR CURRENCY IT_DETAIL-WAERS
*                           TO LT_DOWNLOAD-NETPR,
*           IT_DETAIL-DMBTR CURRENCY IT_DETAIL-WAERS
*                           TO LT_DOWNLOAD-DMBTR,
*           IT_DETAIL-KMPMG UNIT IT_DETAIL-KMPME
*                           TO LT_DOWNLOAD-KMPMG,
*           IT_DETAIL-DATAB TO LT_DOWNLOAD-DATAB,
*           IT_DETAIL-DATBI TO LT_DOWNLOAD-DATBI.

    APPEND lt_download.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'C:\TEMP\SUB_PART.XLS'
            filetype                = 'DAT'
       TABLES
            data_tab                = lt_download
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: it_subsum.

ENDFORM.                    " DOWNLOAD_SUB_PART
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SUB_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_sub_part.
  WRITE: AT /1(sy-linsz) text-h25. "CENTERED.
  WRITE: AT /1(sy-linsz) '================'.
  SKIP.
*  WRITE:/2   TEXT-H02, (03) IT_MODULE-VTYPE.
*  WRITE:/2   TEXT-H03,      IT_MODULE-LIFNR,      IT_MODULE-NAME1,
*         65  TEXT-H14,      IT_MODULE-MOAMT CURRENCY IT_MODULE-WAERS.
*  WRITE:/2   TEXT-H05, (18) IT_MODULE-MATNR, (25) IT_MODULE-MAKTX,
*         65  TEXT-H15,      IT_MODULE-ASYTR CURRENCY IT_MODULE-WAERS,
*         141 TEXT-H08,      SY-DATUM.
*  WRITE:/2   TEXT-H06, (18) IT_MODULE-EKGRP,
*         65  TEXT-H16,      IT_MODULE-DMBTR CURRENCY IT_MODULE-WAERS,
*         141 TEXT-H09,      SY-UZEIT.
*  WRITE:/2   TEXT-H07, (18) P_DATUM,
*         65  TEXT-H17,      IT_MODULE-WAERS,
*         141 TEXT-H10, (04) SY-PAGNO NO-GAP,'/',(4) WA_TOT_PAGE NO-ZERO
*.
*
  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  WRITE: /1(54)'------------------------------------------------------'
 NO-GAP.
  WRITE:/1(54) '|Status  |Part Number       |Description             |'
  NO-GAP.
  WRITE: /1(54)'------------------------------------------------------'
 NO-GAP.
  .

  MOVE: sy-pagno TO wa_tot_page.

ENDFORM.                    " DISPLAY_SUB_PART
