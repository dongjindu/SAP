************************************************************************
* Program Name      : ZEMMPM45R_DISPLAY_MODULE_PRICE
* Author            : Byung-sung, Bae
* Creation Date     : 2004.06.28.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : D1K911262
* Addl Documentation:
* Description       : Display Module Price
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 08/08/2005 Furong Wang               Info reco = PB00+ZTIR
*                                      Indicator(green/yellow/red light)
*
************************************************************************
REPORT zemmpm45r_display_module_price NO STANDARD PAGE HEADING
                                      LINE-SIZE  180
                                      LINE-COUNT  58.
INCLUDE: <icon>.

TABLES: ztmm_assy_cost1,
        ztmm_assy_cost2,
        ztmm_cp_color,
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
DATA:   ztir     like   ekpo-netpr,
        sts       TYPE   i,                     "Module Info Status
        msg(100),
        chbox,
      END   OF it_module.

DATA: BEGIN OF it_sub OCCURS 0,
        vtype     LIKE   ztmm_assy_cost1-vtype,   "Vehicle type
        matnr     LIKE   ztmm_assy_cost1-vtype,   "Material
        upgvc     LIKE   mara-matnr,              "UPG-VC
        pref      LIKE   ztbm_abxduldt-pref,      "BOM Item Number
        comp      LIKE   ztbm_abxduldt-comp,      "BOM Component
        maktx     LIKE   makt-maktx,              "Description
        lifnr     LIKE   lfa1-lifnr,              "Vendor
        amount    TYPE   f,                       "Component Amount
        qnty      LIKE   ztbm_abxduldt-qnty,      "Quantity
        stgb      LIKE   ztbm_abxduldt-stgb,      "End item type
        unit      LIKE   ztbm_abxduldt-unit,      "Unit of measure(BOM)
        meins     LIKE   mara-meins,              "UoM(sub)
        kmein     LIKE   konp-kmein,              "UoM(Info)
        datab     LIKE   sy-datum,                "Valid on(sub)
        datbi     LIKE   sy-datum,                "Valid to(Sub)
        netpr     LIKE   ekpo-netpr,              "Component Amount
        peinh     LIKE   ekpo-peinh,              "Component Price Unit
        waers     LIKE   t001-waers,              "Currency
        kzust     LIKE   konh-kzust.              "Reason code
        INCLUDE STRUCTURE zsmm_custom_condition.
DATA:   ztir      like   ekpo-netpr,
        sts,                                      "Status
        msg(100),                                  "Message
      END   OF it_sub.

DATA: BEGIN OF it_compare OCCURS 0,
        upgvc     LIKE   mara-matnr,              "UPG-VC
        pref      LIKE   ztbm_abxduldt-pref,      "BOM item No
        comp      LIKE   ztbm_abxduldt-comp,      "BOM Component
        lifnr     LIKE   lfa1-lifnr,              "Vendor
        msg(100),                                 "Message
        qnty_s    LIKE   ztbm_abxduldt-qnty,      "Quantity
        unit_s    LIKE   ztbm_abxduldt-unit,      "Unit of measure(BOM)
        datab_s   LIKE   ztmm_assy_cost1-datab,   "Valid on(sub)
        datbi_s   LIKE   ztmm_assy_cost1-datbi,   "Valid to(Sub)
        netpr_s   LIKE   ekpo-netpr,              "Component Amount
        peinh_s   LIKE   ekpo-peinh,              "Component Price Unit
        kzust_s   LIKE   konh-kzust,              "Reason code
        netwr_s   LIKE   ekpo-netwr,
        qnty_t    LIKE   ztbm_abxduldt-qnty,      "Quantity
        unit_t    LIKE   ztbm_abxduldt-unit,      "Unit of measure(BOM)
        datab_t   LIKE   ztmm_assy_cost1-datab,   "Valid on(sub)
        datbi_t   LIKE   ztmm_assy_cost1-datbi,   "Valid to(Sub)
        netpr_t   LIKE   ekpo-netpr,              "Component Amount
        peinh_t   LIKE   ekpo-peinh,              "Component Price Unit
        kzust_t   LIKE   konh-kzust,              "Reason code
        netwr_t   LIKE   ekpo-netwr,
        waers     LIKE   t001-waers,              "Currency
      END   OF it_compare.

DATA: it_detail LIKE zsmm_sub_detail OCCURS 0 WITH HEADER LINE.

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
      w_index(2)   TYPE   n.

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
                            NO-EXTENSION NO INTERVALS,
                s_mcode FOR ztmm_assy_cost1-mcode
                            NO-EXTENSION NO INTERVALS,
                s_lifnr FOR lfa1-lifnr
                            NO-EXTENSION NO INTERVALS,
                s_matnr FOR mara-matnr NO-EXTENSION NO INTERVALS,
                s_ekgrp FOR ztmm_assy_cost1-ekgrp
                            NO-EXTENSION NO INTERVALS.
PARAMETERS:     p_datum LIKE sy-datum OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Initialization
INITIALIZATION.
  PERFORM initialization_rtn.

*----- Input value check & read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM read_data.

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
  ENDCASE.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  SET TITLEBAR  'BASE'.
  SORT it_module BY indicator lifnr matnr.
  PERFORM display_data.

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
  IF s_mcode EQ ' '.
    wa_mcode_t = 'ZZ'.
  ELSE.
    wa_mcode_f = wa_mcode_t = s_mcode-low.
  ENDIF.
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
  SELECT SINGLE * FROM lfa1 WHERE lifnr IN s_lifnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.

  IF s_lifnr-low EQ ' '.
    wa_lifnr_t = 'ZZZZZZZZZZ'.
  ELSE.
    wa_lifnr_f = wa_lifnr_t = s_lifnr-low.
  ENDIF.
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
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.

  IF s_matnr-low EQ ' '.
    wa_matnr_t = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSE.
    wa_matnr_t = wa_matnr_f = s_matnr-low.
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
  SELECT SINGLE * FROM t024 WHERE ekgrp IN s_ekgrp.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  IF s_ekgrp-low EQ ' '.
    wa_ekgrp_t = 'ZZZ'.
  ELSE.
    wa_ekgrp_t = wa_ekgrp_f = s_ekgrp-low.
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
  EXEC SQL PERFORMING APPEND_ITAB.
    SELECT /*+ ORDERED*/
           A.VTYPE,          A.LIFNR,          G.MTNO,
           A.ASYTR,          A.DATAB,          A.DATBI,
           D.MAKTX,          F.NAME1,          G.COMP,
           C.PREF,           C.COMP,           E.MAKTX,
           C.QNTY,           C.UNIT,           B.MEINS,
           A.EKGRP,          A.MCODE,          C.DATUV,
           C.DATUB,          C.STGB
      INTO :WA_MODULE-VTYPE, :WA_MODULE-LIFNR, :WA_MODULE-MATNR,
           :WA_MODULE-ASYTR, :WA_MODULE-DATAB, :WA_MODULE-DATBI,
           :WA_MODULE-MAKTX, :WA_MODULE-NAME1, :WA_SUB-UPGVC,
           :WA_SUB-PREF,     :WA_SUB-COMP,     :WA_SUB-MAKTX,
           :WA_SUB-QNTY,     :WA_SUB-UNIT,     :WA_SUB-MEINS,
           :WA_MODULE-EKGRP, :WA_MODULE-MCODE, :WA_SUB-DATAB,
           :WA_SUB-DATBI,    :WA_SUB-STGB
      FROM ZTMM_ASSY_COST1 A, ZTBM_ABXDULDT G,
           ZTBM_ABXDULDT   C, MARA B,  MAKT D,  MAKT E, LFA1 F
     WHERE A.MANDT    =       :SY-MANDT
       AND A.VTYPE    BETWEEN :WA_VTYPE_F AND :WA_VTYPE_T
       AND A.MCODE    BETWEEN :WA_MCODE_F AND :WA_MCODE_T
       AND A.LIFNR    BETWEEN :WA_LIFNR_F AND :WA_LIFNR_T
       AND A.EKGRP    BETWEEN :WA_EKGRP_F AND :WA_EKGRP_T
       AND A.DATAB    <=      :P_DATUM
       AND A.DATBI    >=      :P_DATUM
       AND F.MANDT    =       A.MANDT
       AND F.LIFNR    =       A.LIFNR
       AND G.MANDT    =       A.MANDT
       AND G.MTNO     LIKE    CONCAT(CONCAT(A.VTYPE,A.MCODE),'%')
       AND G.MTNO     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND G.PLNT     =       :C_WERKS
       AND G.USAG     =       '2'
       AND G.ALTN     =       '01'
       AND G.DATUV    <       :P_DATUM
       AND G.DATUB    >=      :P_DATUM
       AND B.MANDT(+) =       G.MANDT
       AND B.MATNR(+) =       G.MTNO
       AND B.LVORM(+) =       ' '
       AND C.MANDT(+) =       G.MANDT
       AND C.MTNO(+)  =       G.COMP
       AND C.PLNT(+)  =       G.PLNT
       AND C.USAG(+)  =       '2'
       AND C.ALTN(+)  =       '01'
       AND C.DATUV(+) <       :P_DATUM
       AND C.DATUB(+) >=      :P_DATUM
       AND D.MANDT(+) =       B.MANDT
       AND D.MATNR(+) =       B.MATNR
       AND D.SPRAS(+) =       :SY-LANGU
       AND E.MANDT(+) =       C.MANDT
       AND E.MATNR(+) =       C.COMP
       AND E.SPRAS(+) =       :SY-LANGU
     ORDER BY A.VTYPE, A.LIFNR, G.MTNO
  ENDEXEC.
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

  ON CHANGE OF wa_module-matnr OR wa_module-lifnr.
    PERFORM read_module_info_record.
  ENDON.

  PERFORM check_cockpit_module_color USING lw_continue.

  CHECK lw_continue EQ 'X'.

  PERFORM read_sub_info_record.
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
        lw_mtot like it_module-moamt.

  DATA: lw_detail LIKE it_detail.

  SORT it_module BY vtype matnr lifnr.
  SORT it_sub BY vtype matnr upgvc pref.

  LOOP AT it_module.
    LOOP AT it_sub WHERE vtype = it_module-vtype
                     AND matnr = it_module-matnr.
      it_module-dmamt = it_module-dmamt + it_sub-amount.
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
  data: lw_mtot like wa_module-netpr.

  CLEAR: eina, a018.
  CLEAR: wa_module-netpr, wa_module-peinh, wa_module-meins,
         wa_module-msg,  wa_module-sts.

*----- Check Material Master
  IF wa_module-maktx IS INITIAL.
    MOVE: c_no_matl TO wa_module-sts.
    EXIT.
  ENDIF.

  SELECT SINGLE matnr a~loekz wglif
    INTO (eina-matnr,eina-loekz,eina-wglif)
    FROM eina AS a INNER JOIN eine AS b
      ON a~infnr = b~infnr
   WHERE a~matnr = wa_module-matnr
     AND a~lifnr = wa_module-lifnr
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
     AND matnr =  wa_module-matnr
     AND lifnr =  wa_module-lifnr
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

*  CASE wa_module-sts.
*    WHEN c_new.
*      MOVE: text-b04 TO wa_module-msg.
*    WHEN c_deleted.
*      MOVE: text-b05 TO wa_module-msg.
*    WHEN c_no_cond.
*    WHEN c_del_con.
*      MOVE: text-b06 TO wa_module-msg.
*    WHEN c_no_matl.
*      MOVE: text-b07 TO wa_module-msg.
*    WHEN c_exist.
*      MOVE: text-b09 TO wa_module-msg.
*  ENDCASE.

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

  READ TABLE it_sub WITH KEY comp = wa_sub-comp.
  IF sy-subrc NE 0.
    PERFORM check_rtn.
    PERFORM append_sub_price.
  ELSE.
    MOVE: it_sub-lifnr  TO wa_sub-lifnr,
          it_sub-amount TO wa_sub-amount,
          it_sub-kmein  TO wa_sub-kmein,
          it_sub-netpr  TO wa_sub-netpr,
          it_sub-peinh  TO wa_sub-peinh,
          it_sub-waers  TO wa_sub-waers,
          it_sub-kzust  TO wa_sub-kzust,
          it_sub-sts    TO wa_sub-sts,
          it_sub-msg    TO wa_sub-msg,
          it_sub-ztir   TO wa_sub-ztir.

    DO.
      MOVE: sy-index TO w_index.

      CONCATENATE: 'WA_SUB-ZP' w_index INTO lw_sub,
                   'IT_SUB-ZP' w_index INTO w_sub.

      ASSIGN: (w_sub)  TO <sub>,
              (lw_sub) TO <lw_sub>.
      IF sy-subrc NE 0. EXIT. ENDIF.

      MOVE: <sub> TO <lw_sub>.
    ENDDO.

    IF NOT ( it_sub-sts EQ c_no_matl OR
             it_sub-sts EQ c_no_cond OR
             it_sub-sts EQ c_no_info    ).
      IF NOT ( ( wa_sub-meins EQ wa_sub-kmein AND
                 wa_sub-meins EQ wa_sub-unit  AND
                 wa_sub-kmein EQ wa_sub-unit )   ).
        MOVE: c_uom_err TO wa_sub-sts.
      ENDIF.
    ENDIF.

    PERFORM append_sub_price.
  ENDIF.
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
  DATA: BEGIN OF lt_condition OCCURS 0,
          kbetr LIKE zvmm_info_condi-kbetr,
          kpein LIKE zvmm_info_condi-kpein,
          kmein LIKE zvmm_info_condi-kmein,
          kschl LIKE zvmm_info_condi-kschl,
          kzust LIKE zvmm_info_condi-kzust,
        END   OF lt_condition.

  data: begin of it_eina_eine_temp occurs 0,
        matnr like eina-matnr,
        lifnr like eina-lifnr,
        wglif like eina-wglif,
        end of it_eina_eine_temp.

  IF wa_sub-maktx IS INITIAL.
    MOVE: c_no_matl TO wa_sub-sts.
    EXIT.
  ENDIF.

*----- Check Info Record Deletion
** commented by Furong on 27/07/2005

*  CLEAR: eina, a018.
*  SELECT SINGLE lifnr
*    INTO wa_sub-lifnr
*    FROM eina AS a INNER JOIN eine AS b
*      ON a~infnr = b~infnr
*   WHERE a~matnr = wa_sub-comp
*     AND a~urzzt = 'SUB'
*     AND a~loekz = ' '
*     AND b~werks = ' '
*     AND b~ekorg = c_ekorg
*     AND b~loekz = ' '.
*  IF sy-subrc NE 0.
*    MOVE: c_no_info TO wa_sub-sts.
*    EXIT.
*  ENDIF.

*SELECT SINGLE knumh
*    INTO (a018-knumh)
*    FROM a018
*   WHERE kappl =  'M'
*     AND kschl =  'PB00'
*     AND matnr =  wa_sub-comp
*     AND lifnr =  wa_sub-lifnr
*     AND ekorg =  c_ekorg
*     AND esokz =  '0'
*     AND datab <= p_datum
*     AND datbi >= p_datum.
*  IF sy-subrc NE 0.
*    MOVE: c_no_cond TO wa_sub-sts.
*    EXIT.
*  ENDIF.

*** INSERTED BY FURONG ON 27/07/2005
   SELECT matnr a~lifnr wglif into table it_eina_eine_temp
    FROM eina AS a INNER JOIN eine AS b
      ON a~infnr = b~infnr
   WHERE a~matnr = wa_sub-comp
     AND a~urzzt = 'SUB'
     AND a~loekz = ' '
     AND b~werks = ' '
     AND b~ekorg = c_ekorg
     AND b~loekz = ' '.
  IF sy-subrc NE 0.
    MOVE: c_no_info TO wa_sub-sts.
    EXIT.
  ENDIF.

*----- Read submaterial price
  loop at it_eina_eine_temp.
     CLEAR: eina, a018.
     wa_sub-lifnr = it_eina_eine_temp-lifnr.
     SELECT SINGLE knumh datab datbi
                     INTO (a018-knumh, wa_sub-datab, wa_sub-datbi)
                     FROM a018
                     WHERE kappl =  'M'
                       AND kschl =  'PB00'
                       AND matnr =  wa_sub-comp
                       AND lifnr =  wa_sub-lifnr
                       AND ekorg =  c_ekorg
                       AND esokz =  '0'
                       AND datab <= p_datum
                       AND datbi >= p_datum.
      if sy-subrc eq 0.
         exit.
      endif.
  endloop.
*  if sy-subrc ne 0.
  if a018-knumh is initial.
     MOVE: c_no_cond TO wa_sub-sts.
     EXIT.
  ENDIF.
*** END OF INSERTION

  SELECT SINGLE kbetr kpein kmein kzust
    INTO (wa_sub-netpr,wa_sub-peinh,
          wa_sub-kmein,wa_sub-kzust)
    FROM zvmm_info_condi
   WHERE knumh = a018-knumh
     AND kschl = c_kschl
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    MOVE: c_no_cond TO wa_sub-sts.
    EXIT.
  ENDIF.

  SELECT kbetr kpein kmein kzust kschl
    INTO CORRESPONDING FIELDS OF TABLE lt_condition
    FROM zvmm_info_condi
   WHERE knumh = a018-knumh
     AND ( kschl = c_kschl   OR
           kschl = 'ZTIR'    OR
           kschl LIKE 'ZP%' )
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    MOVE: c_no_cond TO wa_sub-sts.
    EXIT.
  ENDIF.

  SORT lt_condition BY kschl.
  LOOP AT lt_condition.
    CASE lt_condition-kschl.
      WHEN c_kschl.
        MOVE: lt_condition-kbetr TO wa_sub-netpr,
              lt_condition-kpein TO wa_sub-peinh,
              lt_condition-kmein TO wa_sub-kmein,
              lt_condition-kzust TO wa_sub-kzust.
        if it_eina_eine_temp-wglif = 'ZTIR'.
           wa_sub-ztir = lt_condition-kbetr.
        endif.

      WHEN OTHERS.
        MOVE: lt_condition-kschl+2(2) TO w_index.

        CONCATENATE: 'WA_SUB-ZP' w_index INTO w_sub.

        ASSIGN: (w_sub) TO <sub>.
        IF sy-subrc NE 0. CONTINUE. ENDIF.

        <sub> = lt_condition-kbetr.
    ENDCASE.
  ENDLOOP.

*----- A sub material's UoM must be 'EA'.
*----- If UoM is not 'EA', display error message.
  IF NOT ( ( wa_sub-meins EQ wa_sub-kmein AND
             wa_sub-meins EQ wa_sub-unit  AND
             wa_sub-kmein EQ wa_sub-unit )   ).
    MOVE: c_uom_err TO wa_sub-sts.
  ENDIF.
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
  CLEAR: it_sub.

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

  MOVE: wa_sub TO it_sub.

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

  READ TABLE s_lifnr INDEX 1.
  IF sy-subrc NE 0.
    CLEAR: lfa1.
  ENDIF.

  READ TABLE s_ekgrp INDEX 1.
  IF sy-subrc NE 0.
    CLEAR: t024.
  ENDIF.

  WRITE: AT /1(sy-linsz) text-h01 CENTERED.

  SKIP.
  WRITE:/2   text-h02, (3) s_vtype-low, s_mcode-low.
  WRITE:/2   text-h04, s_lifnr-low, lfa1-name1.
  WRITE:/2   text-h05, (18) s_matnr-low,
         153 text-h08, sy-datum.
  WRITE:/2   text-h06, (18) s_ekgrp-low,
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
  if it_module-netpr = it_module-moamt.
      WRITE: (4) icon_green_light  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
  else.
      data: lw_red(1).
      PERFORM set_it_detail.
      PERFORM check_it_detail_light using lw_red.
      if lw_red is initial.
         WRITE: (4) icon_yellow_light AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
      else.
         WRITE: (4) icon_red_light  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
      endif.
  endif.

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
      MESSAGE s000(zz) WITH it_module-msg(50) it_module-msg+50(50).
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
          pref      LIKE   ztbm_abxduldt-pref,      "BOM item No
          comp      LIKE   ztbm_abxduldt-comp,      "BOM Component
          qnty_s(16),                               "Quantity
          unit_s    LIKE   ztbm_abxduldt-unit,      "Unit of measure(BOM
          datab_s(10),                              "Valid on(sub)
          datbi_s(10),                              "Valid to(Sub)
          netpr_s(14),                              "Component Amount
          peinh_s   LIKE   ekpo-peinh,              "Component PriceUnit
          kzust_s   LIKE   konh-kzust,              "Reason code
          netwr_s(16),
          qnty_t(16),                               "Quantity
          unit_t    LIKE   ztbm_abxduldt-unit,      "Unit of measure(BOM
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
FORM check_cockpit_module_color USING p_continue.
  DATA: lw_int_key(3).         " Internal Key Color

  CHECK wa_module-matnr+3(2) EQ 'CP' AND
        wa_sub-stgb          EQ 'U'.

  MOVE: wa_module-matnr+10(3) TO lw_int_key.

  SELECT SINGLE * FROM ztmm_cp_color WHERE COPIT EQ wa_module-matnr
                                       AND inkey EQ lw_int_key
                                       AND submt EQ wa_sub-comp
                                       AND datab <  p_datum
                                       AND datbi >= p_datum.
  IF sy-subrc NE 0.
    CLEAR: p_continue.
  ENDIF.
ENDFORM.                    " check_cockpit_module_color
*&---------------------------------------------------------------------*
*&      Form  check_it_detail_light
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_RED  text
*----------------------------------------------------------------------*
FORM check_it_detail_light USING    P_LW_RED type c.
  data: w_red(1).
  loop at it_detail.
    if not it_detail-msg is initial.
       w_red = '1'.
    endif.
  endloop.
  if not w_red is initial.
     P_LW_RED = w_red.
  else.
     clear: P_LW_RED.
  endif.
ENDFORM.                    " check_it_detail_light
