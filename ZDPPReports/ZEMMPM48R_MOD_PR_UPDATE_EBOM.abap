************************************************************************
* Program Name      : ZEMMPM48R_MOD_PR_UPDATE_EBOM
* Author            : Byung-sung, Bae
* Creation Date     : 2004.07.22.
* Specifications By : Byung-sung, Bae
* Pattern           :
* Development Request No : UD1K911264
* Addl Documentation:
* Description       : Module Cost Update (copy original)
*                     for EBOM
* Modification Logs
* Date       Developer    RequestNo    Description
*01/06/2005  Manjunath    UD1K919255   Bug fixing
*                                      to display all non-colored parts
*01/23/2007  Manjunath    UD1K930385   Convert Native SQL to OPEN SQL
*11/02/2007  Furong Wang               EBOM
*10/2010     Furong                    copy to module price history
*                                      table & send email to report
*                                      part,module error
* 07.17.2014 Victor      Check Info Record with Valid date
************************************************************************
REPORT zemmpm48r_mod_pr_update_ebom NO STANDARD PAGE HEADING
                                     LINE-SIZE  182
                                     LINE-COUNT  58.
*TABLES: zsmm_sub_material,
*        ztmm_cp_color.
TABLES: ztpp_mod_bom_his.
INCLUDE: <icon>.

*----- Type
TYPE-POOLS : slis, sp01r.

TABLES: ztmm_assy_cost1,
        ztmm_assy_cost2,
        zvmm_info_condi,
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
        seqno     LIKE   ztmm_assy_cost1-seqno, "Serial No
        datab     LIKE   ztmm_assy_cost1-datab, "Valid on
        datbi     LIKE   ztmm_assy_cost1-datbi, "Valid to
        moamt     LIKE   mseg-dmbtr,            "Module Price
        asytr     LIKE   ztmm_assy_cost1-asytr, "Module Assy Cost
        dmbtr     LIKE   mseg-dmbtr,            "Material Cost
        dmamt     TYPE   f,                     "Material Cost(Floating)
        waers     LIKE   t001-waers,            "Currency
        ekgrp     LIKE   ekko-ekgrp,            "Purchasing Group
        netpr     LIKE   ekpo-netpr,            "Component Amount
        peinh     LIKE   ekpo-peinh,            "Component Price Unit
        meins     LIKE   ekpo-meins,            "UoM
        kzust     LIKE   konh-kzust,            "Reason code
        netpr_y   LIKE   ekpo-netpr,            "Yeaterday amount
        kzust_y   LIKE   konh-kzust.            "Yesterday reason code
        INCLUDE STRUCTURE zsmm_custom_condition_floating.
DATA:   ztir     LIKE   ekpo-netpr,
        sts,                                    "Module Info Status
        same_date_flg,                          "Info valid from same?
        msg(100),
        chbox,
      END   OF it_module.

DATA: BEGIN OF it_sub OCCURS 0,
        vtype     LIKE   ztmm_assy_cost1-vtype, "Vehicle type
        matnr     LIKE   ztmm_assy_cost1-vtype, "Material
        upgvc     LIKE   mara-matnr,              "UPG-VC
        pref      LIKE   stpo-posnr,              "BOM Item Number
        comp      LIKE   mara-matnr,              "BOM Component
        maktx     LIKE   makt-maktx,              "Description
        lifnr     LIKE   lfa1-lifnr,              "Vendor
        amount    TYPE   f,                       "Component Amount
        qnty      LIKE   stpo-menge,      "Quantity
        stgb      LIKE   stpo-stgb,      "End item type
        unit      LIKE   mara-meins,      "Unit of measure(BOM)
        meins     LIKE   mara-meins,              "UoM(sub)
        kmein     LIKE   konp-kmein,              "UoM(Info)
        datab     LIKE   ztmm_assy_cost1-datab,   "Valid on(sub)
        datbi     LIKE   ztmm_assy_cost1-datbi,   "Valid to(Sub)
        netpr     LIKE   ekpo-netpr,              "Component Amount
        peinh     LIKE   ekpo-peinh,              "Component Price Unit
        waers     LIKE   t001-waers,              "Currency
        kzust     LIKE   konh-kzust,              "Reason code
        amount_y  TYPE   f.                       "Yesterday amount
        INCLUDE STRUCTURE zsmm_custom_condition_floating.
DATA:   ztir      LIKE  ekpo-netpr,
        sts,                                      "Status
        msg(100),                                  "Message
      END   OF it_sub.

DATA : BEGIN OF it_tab OCCURS 0,
       vtype  LIKE   ztmm_assy_cost1-vtype,
       lifnr  LIKE   lfa1-lifnr,
       matnr  LIKE   mara-matnr,
       asytr  LIKE mseg-dmbtr,
       datab LIKE   ztmm_assy_cost1-datab,
       datbi LIKE   ztmm_assy_cost1-datbi,
       maktx  LIKE   makt-maktx,
       name1 LIKE   lfa1-name1,
       upgvc LIKE mara-matnr,
       pref LIKE stpo-posnr,
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
       seqno LIKE ztmm_assy_cost1-seqno,
       END OF it_tab.

*data : begin of it_comp occurs 0,
*        matnr like mara-matnr,
*        UPGVC like mara-matnr,
*       end of it_comp .

*data : begin of it_scomp occurs 0.
*        include STRUCTURE ZTBM_ABXDULDT.
*data:   maktx     LIKE   makt-maktx,
*       end of it_scomp.
*
DATA : BEGIN OF it_output OCCURS 0,
            vtype LIKE ztmm_assy_cost1-vtype,
            lifnr LIKE lfa1-lifnr,
            matnr LIKE mara-matnr,
            asytr LIKE mseg-dmbtr,
            datab LIKE ztmm_assy_cost1-datab,
            datbi LIKE ztmm_assy_cost1-datbi,
            maktx LIKE makt-maktx,
            name1 LIKE lfa1-name1,
            upgvc LIKE mara-matnr,
            pref  LIKE stpo-posnr,
            seqno LIKE ztmm_assy_cost2-seqno,
            comp  LIKE mara-matnr,
            cmaktx LIKE makt-maktx,
            qnty LIKE stpo-menge,
            unit LIKE mara-meins,
            meins LIKE   mara-meins,
            ekgrp LIKE ztmm_assy_cost1-ekgrp,
            mcode LIKE ztmm_assy_cost1-mcode,
            cdatab LIKE ztmm_assy_cost1-datab,
            cdatbi LIKE ztmm_assy_cost1-datbi,
            stgb   LIKE stpo-stgb,
         END OF it_output.

DATA:   BEGIN OF it_error_part OCCURS 0,
            lifnr LIKE lfa1-lifnr,
            matnr LIKE mara-matnr,
            comp  LIKE mara-matnr,
            cmaktx LIKE makt-maktx,
            sts(1),
            msg(100),
         END OF it_error_part.

DATA:   BEGIN OF it_error_info OCCURS 0,
            lifnr LIKE lfa1-lifnr,
            comp  LIKE mara-matnr,
            sts(1),
            msg(100),
         END OF it_error_info.


DATA : BEGIN OF it_error_module OCCURS 0,
            lifnr LIKE lfa1-lifnr,
            matnr LIKE mara-matnr,
*            COMP  LIKE MARA-MATNR,
            sts(1),
            msg(100),
         END OF it_error_module.

DATA : BEGIN OF it_updated_module OCCURS 0,
       matnr LIKE mara-matnr,
       asytr LIKE mseg-dmbtr,
                   msg(100),
       END OF it_updated_module.

*DATA: it_detail LIKE zsmm_sub_detail OCCURS 0 WITH HEADER LINE.
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

DATA: it_condition LIKE ztmm_assy_cost2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdc_tab.

*----- Global variables & structures
DATA: wa_module LIKE it_module.
DATA: wa_sub LIKE it_sub,
      l_mcode(4) TYPE c,
      l_check(6) TYPE c.

DATA: w_mode(1) VALUE 'N',
      w_update(1) VALUE 'S'.

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
      l_dyname LIKE sy-repid.

FIELD-SYMBOLS: <module>, <sub>.

*----- Constants
" Module BOM is same all of plants
DATA: c_werks   LIKE   t001w-werks   VALUE   'P001',
      c_ekorg   LIKE   ekko-ekorg    VALUE   'PU01',
      c_kschl   LIKE   konp-kschl    VALUE   'PB00',
      c_ztir    LIKE   konp-kschl    VALUE   'ZTIR',
      c_bukrs   LIKE   t001-bukrs    VALUE   'H201',
*      C_READY   TYPE   I             VALUE   1,
*      C_WARNING TYPE   I             VALUE   2,
*      C_SUCCESS TYPE   I             VALUE   3,
*      C_ERROR   TYPE   I             VALUE   4,
*      C_INCORRECT TYPE I             VALUE   5,
*      C_NEW     TYPE   I             VALUE   1,"Module Status
*      C_DELETED TYPE   I             VALUE   2,"Module Status
*      C_NO_COND TYPE   I             VALUE   3,"Condition does not
*exist
*      C_DEL_CON TYPE   I             VALUE   4,"Condition was deleted
*      C_NO_INFO TYPE   I             VALUE   5,"InfoRecord dosn't exist
*      C_UOM_ERR TYPE   I             VALUE   6,"Incorrect UoM
*      C_NO_MATL TYPE   I             VALUE   7,"M/M does not exist.
*      C_EXIST   TYPE   I             VALUE   8,"Info Record is exist.
*      C_NO_PRICE TYPE  I             VALUE   9,"No Price.
      c_ready             VALUE   1,
      c_warning             VALUE   2,
      c_success             VALUE   3,
      c_error             VALUE   4,
      c_incorrect            VALUE   5,
      c_new             VALUE   1,"Module Status
      c_deleted           VALUE   2,"Module Status
      c_no_cond             VALUE   3,"Condition does not exist
      c_del_con             VALUE   4,"Condition was deleted
      c_no_info             VALUE   5,"InfoRecord dosn't exist
      c_uom_err             VALUE   6,"Incorrect UoM
      c_no_matl             VALUE   7,"M/M does not exist.
      c_exist             VALUE   8,"Info Record is exist.
      c_no_price             VALUE   9,"No Price.
      c_overlap_info(1)              VALUE   'A', "DUPLICATE INFO RECORD
      c_no_bom(1)               VALUE   'B', "No Module BOM
      c_bom_exp(1)              VALUE   'C', "BOM EXPLOSION
** Chnaged by Furong on 05/19/09
*      C_RSN01(3)                     VALUE  'A0X',"Reason code
*      C_RSN02(3)                     VALUE  'A01',"Reason code
*      C_RSN03(3)                     VALUE  'ADX',"Reason code
*      C_RSN04(3)                     VALUE  'AD1',"Reason code
*      C_RSN05(3)                     VALUE  'AUX',"Reason code
*      C_RSN06(3)                     VALUE  'AU1'."Reason code

      c_rsn01(3)                     VALUE  'XM2',"Reason code
      c_rsn02(3)                     VALUE  'ME2',"Reason code
      c_rsn03(3)                     VALUE  'XM2',"Reason code
      c_rsn04(3)                     VALUE  'MD2',"Reason code
      c_rsn05(3)                     VALUE  'XM2',"Reason code
      c_rsn06(3)                     VALUE  'MU2'."Reason code
** End of change

*----- Table controls

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_vtype FOR ztmm_assy_cost1-vtype
                            NO-EXTENSION NO INTERVALS.
*                s_mcode FOR ztmm_assy_cost1-mcode
*                            NO-EXTENSION NO INTERVALS,
*                s_lifnr FOR lfa1-lifnr
*                            NO-EXTENSION NO INTERVALS,
*                s_matnr FOR mara-matnr NO-EXTENSION NO INTERVALS,
*                s_ekgrp FOR ztmm_assy_cost1-ekgrp
*                            NO-EXTENSION NO INTERVALS.
PARAMETERS:     p_mcode LIKE ztmm_assy_cost1-mcode OBLIGATORY,
                p_lifnr LIKE lfa1-lifnr,
                p_matnr LIKE mara-matnr,
                p_ekgrp LIKE ztmm_assy_cost1-ekgrp.

PARAMETERS:     p_datum LIKE sy-datum OBLIGATORY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) text-t02.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-t03 FOR FIELD p_back.
SELECTION-SCREEN POSITION 33.
PARAMETERS:     p_back  AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-t04 FOR FIELD p_back.
SELECTION-SCREEN POSITION 33.
PARAMETERS:     p_email  AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.

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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  p_mcode.

  DATA : BEGIN OF value_tab OCCURS 0,
            mcode  LIKE ztmm_assy_cost1-mcode,
           END OF value_tab.
* Select
  SELECT DISTINCT mcode  FROM ztmm_assy_cost1
             INTO TABLE value_tab.

  l_dyname = sy-repid.

* Set F4 values for Module code
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

* Select Vehicle TYpe
  SELECT DISTINCT vtype  FROM ztmm_assy_cost1
             INTO TABLE value_tab.
  l_dyname = sy-repid.


*
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
*     RETURN_TAB      = T_RETURN
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
  ENDCASE.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  SET TITLEBAR  'BASE'.
  SORT it_module BY indicator lifnr matnr.
  IF p_back IS INITIAL.
    PERFORM display_data.
  ELSE.
    PERFORM processing_rtn.
    IF p_email = 'X'.
      PERFORM send_email.
    ENDIF.
  ENDIF.

*----- Double click
AT LINE-SELECTION.
  PERFORM double_click_rtn.

*----- Function Key
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'EXCUTE'.
      PERFORM excute_rtn.
      IF p_email = 'X'.
        PERFORM send_email.
      ENDIF.

    WHEN 'DETAIL'.
      PERFORM detail_rtn.
    WHEN 'MSG'.
      MESSAGE s000(zz) WITH it_module-msg(50) it_module-msg+50(50).
      CLEAR: it_module.
    WHEN 'SUB_INFO'.
      PERFORM display_sub_info_record.
    WHEN 'SUB_MSG'.
      MESSAGE s000(zz) WITH it_detail-msg(50) it_detail-msg+50(50).
      CLEAR: it_detail.
    WHEN 'S_ALL'.
      PERFORM select_all.
    WHEN 'D_ALL'.
      PERFORM deselect_all.
    WHEN 'ASCENDING'.
      PERFORM ascending_rtn.
    WHEN 'DESCENDING'.
      PERFORM descending_rtn.
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
  p_datum = sy-datum.
ENDFORM.                    " initialization_rtn
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM get_sub_price.
  PERFORM calculate_module_price.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
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
  PERFORM check_locking.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_EKGRP
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
ENDFORM.                    " CHECK_EKGRP
*&---------------------------------------------------------------------*
*&      Form  CHECK_LIFNR
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
ENDFORM.                    " CHECK_LIFNR
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
    wa_vtype_t = 'ZZZ'.
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
*  IF s_mcode EQ ' '.
*    wa_mcode_t = 'ZZ'.
*    move '%' to l_mcode.
*  ELSE.
  wa_mcode_f = wa_mcode_t = p_mcode.
  CONCATENATE '%' p_mcode '%' INTO l_mcode.
*  ENDIF.
ENDFORM.                    " check_mcode
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

*  lw_continue = 'X'.
*  PERFORM check_cockpit_module_color USING lw_continue.
*
*  CHECK lw_continue EQ 'X'.

    PERFORM read_sub_info_record.
  ENDLOOP.
ENDFORM.                    " APPEND_ITAB
*&---------------------------------------------------------------------*
*&      Form  APPEND_SUB_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0      text
*      -->P_1      text
*----------------------------------------------------------------------*
FORM append_sub_price.
  CASE wa_sub-sts.
    WHEN c_no_info.
      MOVE: text-b02 TO wa_sub-msg.
    WHEN c_no_cond.
      MOVE: text-b03 TO wa_sub-msg.
    WHEN c_uom_err.
      MOVE: text-b01 TO wa_sub-msg.
    WHEN c_no_matl.
      MOVE: text-b07 TO wa_sub-msg.
    WHEN c_no_price.
      MOVE: text-b12 TO wa_sub-msg.
    WHEN c_overlap_info.
      MOVE: text-b13 TO wa_sub-msg.
  ENDCASE.

  MOVE: it_output-vtype TO wa_sub-vtype,
        it_output-matnr TO wa_sub-matnr.

  IF wa_sub-peinh EQ 0.
    wa_sub-peinh = 1.
  ENDIF.



  MOVE: wa_sub TO it_sub.
  MOVE-CORRESPONDING it_output TO it_sub.
  it_sub-amount = it_sub-qnty * wa_sub-netpr / wa_sub-peinh.
  it_sub-maktx = it_output-cmaktx.
  it_sub-lifnr = wa_sub-lifnr.
*  it_sub-datab =  it_output-cDATAB.
*  it_sub-datbi =  it_output-cDATBI.
  it_sub-datab =  wa_sub-datab.
  it_sub-datbi =  wa_sub-datbi.

  IF it_sub-datab IS INITIAL.
    it_sub-datab =  it_output-cdatab.
  ENDIF.

  IF it_sub-datbi IS INITIAL.
    it_sub-datbi =  it_output-cdatbi.
  ENDIF.


  APPEND it_sub.
  CLEAR: it_sub, wa_sub.
ENDFORM.                    " APPEND_SUB_PRICE
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
** Changed by Furong on 11/02/10
*        LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE,
        lt_stb TYPE ztpp_mod_bom_his OCCURS 0 WITH HEADER LINE,
        l_verson LIKE ztpp_mod_bom_his-verson,
** End of change
        l_stlal LIKE mast-stlal.

  DATA: lt_cost1 LIKE TABLE OF ztmm_assy_cost1 WITH HEADER LINE,
        lt_tab LIKE TABLE OF it_tab WITH HEADER LINE.
  REFRESH: it_tab, it_output, it_module.
* Get all the Modules
*  SELECT a~VTYPE          A~LIFNR a~MCODE a~seqno A~EKGRP
*         a~ASYTR          A~DATAB A~DATBI
*         g~MTNO as matnr  g~comp as UPGVC g~PREF
*         d~maktx v~name1 as NAME1 g~UNIT as unit m~meins as MEINS
*         into corresponding fields of table it_tab
*          FROM ZTMM_ASSY_COST1 as A inner join ZTBM_ABXDULDT as G
*            on a~mandt = g~Mandt
*            inner join mara as m on
*                  g~mtno = m~matnr
*            inner join makt as d on
*                   g~mtno = d~matnr
*            inner join lfa1 as v on
*                  v~lifnr = a~lifnr
*                   where  A~VTYPE    BETWEEN WA_VTYPE_F AND WA_VTYPE_T
*               AND A~MCODE    BETWEEN  WA_MCODE_F AND WA_MCODE_T
*               AND A~LIFNR    BETWEEN  WA_LIFNR_F AND WA_LIFNR_T
*               AND A~EKGRP    BETWEEN  WA_EKGRP_F AND WA_EKGRP_T
*               AND A~DATAB    <=      P_DATUM
*               AND A~DATBI    >=      P_DATUM
*                  AND G~PLNT     =       C_WERKS
*                  AND G~USAG     =       '2'
*                  AND G~ALTN     =       '01'
*                  AND G~DATUV    <       P_DATUM
*                  AND G~DATUB    >=      P_DATUM
*                  and G~MTNO    BETWEEN WA_MATNR_F AND WA_MATNR_T
*                  and g~mtno  like l_mcode
*                  AND m~LVORM = ''
*                  and d~spras = sy-langu.
*
*                  loop at it_tab.
**    it_comp-matnr = it_tab-MATNR.
*    it_comp-UPGVC = it_tab-UPGVC.
*    collect it_comp.
*  endloop.
*
** Get All the sub components
*  select
*  a~MTNO
*  a~PLNT
*  a~USAG
*  a~ALTN
*  a~PREF
*  a~COMP
*  a~SUFF
*  a~SEQC
*  a~SEQU
*  a~QNTY
*  a~UNIT
*  a~DATUV
*  a~DATUB
*  a~STGB
*  d~MAKTX
*       from  ZTBM_ABXDULDT as a
*            inner join makt as d on
*                    d~matnr = a~COMP
*             into corresponding fields of table it_scomp
*             for all entries in it_comp
*           where mtno = it_comp-UPGVC
*            AND PLNT     =       C_WERKS
*                    AND USAG     =       '2'
*                    AND ALTN     =       '01'
*                    AND DATUV    <       P_DATUM
*                    AND DATUB    >=      P_DATUM
*                    and d~SPRAS = sy-langu.
*
*
*  sort it_scomp by MTNO comp.
*
*
*  loop at it_tab.
*    concatenate   it_tab-VTYPE it_tab-MCODE into l_check.
*    check it_tab-MATNR+0(5) eq l_check.
*    move-corresponding it_tab to it_output.
*    loop at it_scomp where MTNO = it_tab-UPGVC.
*      move-corresponding it_scomp to it_output.
*      it_output-maktx  = it_tab-maktx.
*      it_output-cMAKTX = it_scomp-MAKTX.
*      it_output-cDATAB = it_scomp-DATUV.
*      it_output-cDATBI = it_scomp-DATUB.
*      append it_output.
*    endloop.
*    clear it_output.
*  endloop.
************* New


*  SELECT * INTO TABLE LT_COST1
*   FROM ZTMM_ASSY_COST1
*   WHERE  VTYPE BETWEEN WA_VTYPE_F AND WA_VTYPE_T
*      AND MCODE = P_MCODE
*      AND LIFNR    BETWEEN  WA_LIFNR_F AND WA_LIFNR_T
*      AND EKGRP    BETWEEN  WA_EKGRP_F AND WA_EKGRP_T
*      AND DATAB <= P_DATUM
*      AND DATBI >= P_DATUM .
*  LOOP AT LT_COST1.
*    IF P_MATNR IS INITIAL.
*** Changed on 01/09/08 only get the module no. including 'M1'
*** for limit only new modules are selecte.
**    CONCATENATE '%' LT_COST1-VTYPE P_MCODE '%' INTO L_MATNR.
*    CONCATENATE '%' LT_COST1-VTYPE P_MCODE '%M1%' INTO L_MATNR.
*** End of change
*
*    SELECT V~LIFNR G~MATNR AS MATNR
*           D~MAKTX V~NAME1 AS NAME1
*           M~MEINS AS UNIT M~MEINS AS MEINS
*           INTO CORRESPONDING FIELDS OF TABLE LT_TAB
*            FROM LFA1 AS V INNER JOIN EINA AS V1 ON
*              V~LIFNR = V1~LIFNR
*              INNER JOIN MAST AS G ON
*                   G~MATNR = V1~MATNR
*              INNER JOIN MARA AS M ON
*                    G~MATNR = M~MATNR
*              INNER JOIN MAKT AS D ON
*                     M~MATNR = D~MATNR
*                 WHERE V~LIFNR = lt_cost1-lifnr
*                    AND G~WERKS     =       C_WERKS
*                    AND G~STLAN     =       '2'
*                    AND G~STLAL     =       '01'
**                  AND G~DATUV    <       P_DATUM
**                  AND G~DATUB    >=      P_DATUM
*                    AND G~MATNR LIKE L_MATNR
*                    AND M~LVORM = ''
*                    AND D~SPRAS = SY-LANGU
*                    AND V1~LOEKZ = ' '.
*    ELSE.
*     SELECT V~LIFNR G~MATNR AS MATNR
*           D~MAKTX V~NAME1 AS NAME1
*           M~MEINS AS UNIT M~MEINS AS MEINS
*           INTO CORRESPONDING FIELDS OF TABLE LT_TAB
*            FROM LFA1 AS V INNER JOIN EINA AS V1 ON
*              V~LIFNR = V1~LIFNR
*              INNER JOIN MAST AS G ON
*                   G~MATNR = V1~MATNR
*              INNER JOIN MARA AS M ON
*                    G~MATNR = M~MATNR
*              INNER JOIN MAKT AS D ON
*                     M~MATNR = D~MATNR
*                 WHERE V~LIFNR = lt_cost1-lifnr
*                    AND G~WERKS     =       C_WERKS
*                    AND G~STLAN     =       '2'
*                    AND G~STLAL     =       '01'
**                  AND G~DATUV    <       P_DATUM
**                  AND G~DATUB    >=      P_DATUM
*                    AND G~MATNR = P_MATNR
*                    AND M~LVORM = ''
*                    AND D~SPRAS = SY-LANGU
*                    AND V1~LOEKZ = ' '.
*
*    ENDIF.

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
** Changed on 01/09/08 only get the module no. including 'M1'
** for limit only new modules are selecte.
*    CONCATENATE '%' LT_COST1-VTYPE P_MCODE '%' INTO L_MATNR.
      CONCATENATE '%' lt_cost1-vtype p_mcode '%M1%' INTO l_matnr.
** End of change
      SELECT g~matnr AS matnr
             d~maktx
             m~meins AS unit m~meins AS meins
             INTO CORRESPONDING FIELDS OF TABLE lt_tab
                FROM mast AS g
                INNER JOIN mara AS m ON
                      g~matnr = m~matnr
                INNER JOIN makt AS d ON
                       m~matnr = d~matnr
                   WHERE g~werks     =       c_werks
                      AND g~stlan     =       '2'
                      AND g~stlal     =       '01'
*                  AND G~DATUV    <       P_DATUM
*                  AND G~DATUB    >=      P_DATUM
                      AND g~matnr LIKE l_matnr
                      AND m~lvorm = ''
                      AND m~mstae <> '14'
                      AND d~spras = sy-langu.
    ELSE.
      SELECT g~matnr AS matnr
              d~maktx
              m~meins AS unit m~meins AS meins
              INTO CORRESPONDING FIELDS OF TABLE lt_tab
                 FROM mast AS g
                 INNER JOIN mara AS m ON
                       g~matnr = m~matnr
                 INNER JOIN makt AS d ON
                        m~matnr = d~matnr
                    WHERE g~werks     =       c_werks
                       AND g~stlan     =       '2'
                       AND g~stlal     =       '01'
*                  AND G~DATUV    <       P_DATUM
*                  AND G~DATUB    >=      P_DATUM
                       AND g~matnr = p_matnr
                       AND m~lvorm = ''
                       AND m~mstae <> '14'
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
      it_tab-seqno = lt_cost1-seqno.
      it_tab-datab = lt_cost1-datab.
      it_tab-datbi = lt_cost1-datbi.
      APPEND it_tab.
    ENDLOOP.
    REFRESH lt_tab.
  ENDLOOP.

*  DELETE ADJACENT DUPLICATES FROM IT_TAB COMPARING MATNR LIFNR.
  LOOP AT it_tab.
    CLEAR: l_stlal.

** Changed by Furong on 11/02/10
    CLEAR: l_verson.

    SELECT SINGLE MAX( verson ) INTO l_verson
       FROM ztpp_mod_bom_his
       WHERE matnr = it_tab-matnr.
*         AND CONF_DATE = P_DATUM.
    IF l_verson > 0.
      SELECT * INTO TABLE lt_stb
        FROM ztpp_mod_bom_his
        WHERE matnr = it_tab-matnr
          AND verson = l_verson.
*          AND CONF_DATE = P_DATUM.

*    SELECT SINGLE MAX( A~STLAL ) INTO L_STLAL
*            FROM MAST AS A
*            INNER JOIN STKO AS B
*            ON A~STLNR = B~STLNR
*            WHERE A~MATNR = IT_TAB-MATNR
**       AND A~WERKS = IT_LIST-WERKS
*            AND A~STLAN = '2'
*            AND B~STLTY = 'M'
*            AND B~STLST = '1'.
*    IF SY-SUBRC <> 0.
*      IT_ERROR_MODULE-MATNR = IT_TAB-MATNR.
*      IT_ERROR_MODULE-STS = C_NO_BOM.
*      IT_ERROR_MODULE-MSG = 'No Active BOM for usegae 3'.
*      APPEND IT_ERROR_MODULE.
*      CLEAR: IT_ERROR_MODULE.
*      CONTINUE.
*    ENDIF.
*
*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*         EXPORTING
*          CAPID                       = C_CAPID
*          DATUV                       = P_DATUM
**    EMENG                       = p_emeng
**    MEHRS                       = p_mehrs
**    MMORY                       = p_mmory
*          MTNRV                       = IT_TAB-MATNR
*          MKTLS                       = 'X'
*          STLAL                       = L_STLAL
*          STLAN                       = '2'
**   STPST                       = 0
**   SVWVO                       = 'X'
*          WERKS                       = 'P001'
** IMPORTING
**    TOPMAT                     =
**   DSTST                       =
*          TABLES
*            STB                       = LT_STB
**   MATCAT                      =
*       EXCEPTIONS
*         ALT_NOT_FOUND               = 1
*         CALL_INVALID                = 2
*         MATERIAL_NOT_FOUND          = 3
*         MISSING_AUTHORIZATION       = 4
*         NO_BOM_FOUND                = 5
*         NO_PLANT_DATA               = 6
*         NO_SUITABLE_BOM_FOUND       = 7
*         CONVERSION_ERROR            = 8
*         OTHERS                      = 9 .
** end of cahnge on 11/02/10

*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*       EXPORTING
*        CAPID                       = C_CAPID
*        DATUV                       = P_DATUM
**    EMENG                       = p_emeng
**    MEHRS                       = p_mehrs
**    MMORY                       = p_mmory
*        MTNRV                       = IT_TAB-MATNR
*        MKTLS                       = 'X'
*        STLAL                       = '01'
*        STLAN                       = '2'
**   STPST                       = 0
**   SVWVO                       = 'X'
*        WERKS                       = 'P001'
** IMPORTING
**    TOPMAT                     =
**   DSTST                       =
*        TABLES
*          STB                       = LT_STB
**   MATCAT                      =
*     EXCEPTIONS
*       ALT_NOT_FOUND               = 1
*       CALL_INVALID                = 2
*       MATERIAL_NOT_FOUND          = 3
*       MISSING_AUTHORIZATION       = 4
*       NO_BOM_FOUND                = 5
*       NO_PLANT_DATA               = 6
*       NO_SUITABLE_BOM_FOUND       = 7
*       CONVERSION_ERROR            = 8
*       OTHERS                      = 9 .

      IF sy-subrc <> 0.
        it_error_module-matnr = it_tab-matnr.
        it_error_module-sts = c_bom_exp.
        it_error_module-msg = 'No Active BOM for usegae 2'.
        APPEND it_error_module.
        CLEAR it_error_module.
      ENDIF.

      MOVE-CORRESPONDING it_tab TO it_output.
      LOOP AT lt_stb WHERE datuv <= p_datum AND datub > p_datum.
        it_output-comp = lt_stb-idnrk.
        it_output-qnty = lt_stb-menge.
        it_output-datab = lt_stb-datuv.
        it_output-datbi = lt_stb-datub.
        it_output-cdatab = lt_stb-datuv.
        it_output-cdatbi = lt_stb-datub.
        it_output-upgvc = lt_stb-upgn.
        SELECT SINGLE maktx INTO it_output-cmaktx
          FROM makt
          WHERE matnr = lt_stb-idnrk.
        APPEND it_output.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

** End of change
  PERFORM append_itab.

* END of changes - UD1K930385

  READ TABLE it_module INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.
ENDFORM.                    " get_sub_price
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
        lw_price_flg,       "undetermined price or not
        lw_new_condition.   "New condition or not
  DATA: lw_detail LIKE it_detail.
  DATA: l_amount TYPE f.

  SORT it_module BY vtype matnr lifnr.
  SORT it_sub BY vtype matnr upgvc pref.

  LOOP AT it_module.
    CLEAR: lw_price_flg, lw_new_condition.
    CLEAR: l_amount.
    LOOP AT it_sub WHERE vtype = it_module-vtype
                     AND matnr = it_module-matnr.
      l_amount = l_amount + + it_sub-amount.
*      IT_MODULE-DMAMT = IT_MODULE-DMAMT + IT_SUB-AMOUNT.
      it_module-ztir = it_module-ztir + it_sub-ztir
                       / it_sub-peinh * it_sub-qnty.

      DO.
        MOVE: sy-index TO w_index.

        CONCATENATE: 'IT_MODULE-ZP' w_index INTO w_module,
                     'IT_SUB-ZP'    w_index INTO w_sub.

        ASSIGN: (w_sub)    TO <sub>,
                (w_module) TO <module>.
        IF sy-subrc NE 0. EXIT. ENDIF.

        <module> = <module> + <sub> / it_sub-peinh * it_sub-qnty.
      ENDDO.

*      IF IT_SUB-STS NE 0.
      IF NOT it_sub-sts IS INITIAL.
        it_module-indicator = c_warning.
      ENDIF.

      IF NOT ( it_sub-sts EQ c_no_matl OR
               it_sub-sts EQ c_no_cond OR
               it_sub-sts EQ c_no_info    ).
        IF NOT ( ( wa_sub-meins EQ wa_sub-kmein AND
                   wa_sub-meins EQ wa_sub-unit  AND
                   wa_sub-kmein EQ wa_sub-unit )   ).
          MOVE: c_uom_err TO wa_sub-sts.
        ENDIF.
      ENDIF.

      IF it_sub-sts IS INITIAL.
        IF it_sub-kzust(1) EQ 'X'.
          lw_price_flg = 'X'.
        ENDIF.
      ELSE.
        lw_price_flg = 'X'.
      ENDIF.

      IF it_sub-amount NE it_sub-amount_y.
        lw_new_condition = 'X'.
      ENDIF.
    ENDLOOP.

    it_module-dmamt = l_amount.
    MOVE: it_module-dmamt TO it_module-dmbtr.

    it_module-moamt = it_module-asytr + it_module-dmbtr.

    PERFORM set_reason_code USING lw_price_flg lw_new_condition.

    CASE it_module-sts.
      WHEN c_new OR c_no_cond.
        IF it_module-indicator NE c_warning.
          MOVE: c_ready TO it_module-indicator.
        ENDIF.
      WHEN c_deleted OR c_del_con OR c_no_matl.
        MOVE: c_incorrect TO it_module-indicator.
      WHEN c_exist.
        IF lw_new_condition    EQ 'X'.
          IF it_module-indicator NE c_warning.
            MOVE: c_ready     TO it_module-indicator.
          ENDIF.
        ELSE.
          MOVE: c_success   TO it_module-indicator.
        ENDIF.
    ENDCASE.

    IF it_module-indicator = c_warning.
      MOVE: text-b10 TO it_module-msg.
    ENDIF.


    IF NOT ( it_module-indicator EQ c_incorrect OR
             it_module-indicator EQ c_success      ).
    ENDIF.
    MODIFY it_module.
  ENDLOOP.

*----- If Material cost is 0, can't excute creation Info Record
  LOOP AT it_module WHERE dmbtr     = 0
                      AND NOT ( indicator = c_incorrect OR
                                indicator = c_success   ).
    MOVE: text-b11    TO it_module-msg,
          c_incorrect TO it_module-indicator,
          c_no_matl   TO it_module-sts.
    MODIFY it_module.
  ENDLOOP.
ENDFORM.                    " calculate_module_price
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  NEW-PAGE LINE-SIZE 182 LINE-COUNT 58.

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
*&      Form  display_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_line.
  DATA: lw_zp12 LIKE it_module-dmbtr,
        lw_zp13 LIKE it_module-dmbtr.

  WRITE:       '|' NO-GAP.

  IF it_module-indicator EQ c_incorrect OR
     it_module-indicator EQ c_success.
    WRITE: it_module-chbox AS CHECKBOX INPUT OFF.
  ELSE.
    WRITE: it_module-chbox AS CHECKBOX.
  ENDIF.

  WRITE:  (03) space NO-GAP, '|' NO-GAP,
          (10) space NO-GAP, '|' NO-GAP,
          (20) space NO-GAP, '|' NO-GAP,
          (18) space NO-GAP, '|' NO-GAP,
          (20) space NO-GAP, '|' NO-GAP.

  SELECT SINGLE b~effpr INTO eine-effpr
              FROM eina AS a INNER JOIN eine AS b
              ON a~infnr = b~infnr
              WHERE a~matnr = it_module-matnr
                AND a~lifnr = it_module-lifnr
                AND a~loekz = ' '
                AND b~werks = ' '
                AND b~ekorg = c_ekorg
                AND b~loekz = ' '.
  IF eine-effpr = it_module-moamt.
    WRITE: (4) icon_green_light  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
  ELSE.
    WRITE: (4) icon_light_out    AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
  ENDIF.

*** marked by furong
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

  MOVE: it_module-zp12 TO lw_zp12,
        it_module-zp13 TO lw_zp13.

  WRITE:      it_module-kzust NO-GAP, '|' NO-GAP,
              it_module-waers NO-GAP, '|' NO-GAP,
         (11) it_module-moamt CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (10) it_module-asytr CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (10) it_module-dmbtr CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (10) it_module-ztir CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) lw_zp12         CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
         (09) lw_zp13         CURRENCY it_module-waers NO-GAP,
              '|' NO-GAP,
              it_module-datab NO-GAP, '|' NO-GAP,
              it_module-datbi NO-GAP,
              '|'.

  HIDE: it_module.
ENDFORM.                    " display_line
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
*&      Form  display_base_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_base_header.
  SET LEFT SCROLL-BOUNDARY COLUMN 84.

*  READ TABLE p_lifnr INDEX 1.
*  IF sy-subrc NE 0.
*    CLEAR: lfa1.
*  ENDIF.
*
*  READ TABLE p_ekgrp INDEX 1.
*  IF sy-subrc NE 0.
*    CLEAR: t024.
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

  CASE it_module-indicator.
    WHEN c_error.
      FORMAT COLOR COL_NEGATIVE   INTENSIFIED ON.
    WHEN c_success.
      FORMAT COLOR COL_POSITIVE   INTENSIFIED OFF.
    WHEN c_incorrect.
      FORMAT COLOR COL_BACKGROUND INTENSIFIED ON.
  ENDCASE.
ENDFORM.                    " set_format
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
*&      Form  DETAIL_RTN
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
ENDFORM.                    " DETAIL_RTN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SUB_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_sub_material.
  PERFORM set_it_detail.
  PERFORM display_it_detail.
ENDFORM.                    " DISPLAY_SUB_MATERIAL
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
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
  ENDCASE.
*    SUPPRESS DIALOG.
*  *  leave screen.
  LEAVE LIST-PROCESSING.
  WRITE: 'aaa'.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
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
  WRITE:/2   text-h04,      it_module-lifnr,      it_module-name1,
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
  ENDCASE.
ENDFORM.                    " double_click_rtn
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_DETAIL
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
ENDFORM.                    " DOUBLE_CLICK_DETAIL
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_BASE
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
ENDFORM.                    " DOUBLE_CLICK_BASE
*&---------------------------------------------------------------------*
*&      Form  READ_MODULE_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_module_info_record.
  PERFORM check_module.
  PERFORM append_it_module.
ENDFORM.                    " READ_MODULE_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  READ_SUB_INFO_RECORD
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

  READ TABLE it_sub WITH KEY comp  = it_output-comp
                             upgvc = it_output-upgvc
                             pref  = it_output-pref.
  IF sy-subrc NE 0.
    MOVE-CORRESPONDING  it_output TO it_sub.
    PERFORM check_rtn.
    PERFORM append_sub_price.
  ELSE.
** Furong on 04/12/12
    PERFORM check_part_info_for_module.
** end on 04/12/12
    MOVE: it_sub-lifnr    TO wa_sub-lifnr,
          it_sub-amount   TO wa_sub-amount,
          it_sub-amount_y TO wa_sub-amount_y,
          it_sub-kmein    TO wa_sub-kmein,
          it_sub-datab    TO wa_sub-datab,
          it_sub-datbi    TO wa_sub-datbi,
          it_sub-netpr    TO wa_sub-netpr,
          it_sub-peinh    TO wa_sub-peinh,
          it_sub-waers    TO wa_sub-waers,
          it_sub-kzust    TO wa_sub-kzust,
          it_sub-sts      TO wa_sub-sts,
          it_sub-msg      TO wa_sub-msg,
          it_sub-ztir     TO wa_sub-ztir,
          it_sub-datab    TO wa_sub-datab,
          it_sub-datbi    TO wa_sub-datbi.
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
      IF NOT ( ( it_sub-meins EQ wa_sub-kmein AND
                 it_sub-meins EQ it_sub-unit  AND
                 wa_sub-kmein EQ it_sub-unit )   ).
        MOVE: c_uom_err TO wa_sub-sts.
      ENDIF.
    ENDIF.

    PERFORM append_sub_price.
  ENDIF.
ENDFORM.                    " READ_SUB_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM input_day_info_record.
  PERFORM yesterday_info_record.
ENDFORM.                    " check_rtn
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
** changed by furong
  IF wa_module-sts = c_deleted.
    CLEAR: wa_module.
    EXIT.
  ENDIF.
** end of change
  CASE wa_module-sts.
    WHEN c_new.
      MOVE: text-b04 TO wa_module-msg.
*    WHEN c_deleted.
*      MOVE: text-b05 TO wa_module-msg.
    WHEN c_no_cond.
    WHEN c_del_con.
      MOVE: text-b06 TO wa_module-msg.
    WHEN c_no_matl.
      MOVE: text-b07 TO wa_module-msg.
    WHEN c_exist.
      MOVE: text-b09 TO wa_module-msg.
  ENDCASE.

  IF it_module-peinh EQ 0.
    it_module-peinh = 1.
  ENDIF.

  MOVE: t001-waers TO wa_module-waers.

  MOVE: wa_module TO it_module.
  MOVE-CORRESPONDING it_output TO it_module.

  APPEND it_module.
  CLEAR: it_module.
ENDFORM.                    " append_it_module
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
  CLEAR: eina, a018.
  CLEAR: wa_module-netpr, wa_module-peinh, wa_module-meins,
         wa_module-msg,   wa_module-sts.

*----- Check Material Master
  IF it_output-maktx IS INITIAL.
    MOVE: c_no_matl TO wa_module-sts.
    EXIT.
  ENDIF.

  SELECT SINGLE matnr a~loekz
    INTO (eina-matnr,eina-loekz)
    FROM eina AS a INNER JOIN eine AS b
      ON a~infnr = b~infnr
   WHERE a~matnr = it_output-matnr
     AND a~lifnr = it_output-lifnr
** changed by furong
*     AND a~loekz = ' '
** end of change
     AND b~werks = ' '
     AND b~ekorg = c_ekorg.
** changed by furong
*     AND b~loekz = ' '.
** end of change
  IF sy-subrc NE 0.
    MOVE: c_new    TO wa_module-sts.
    EXIT.
  ENDIF.

  IF eina-loekz EQ 'X' OR eine-loekz = 'X'.
    MOVE: c_deleted TO wa_module-sts.
    EXIT.
  ENDIF.

*----- read info record
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

  SELECT SINGLE kbetr kpein kmein
    INTO (wa_module-netpr, wa_module-peinh, wa_module-meins)
    FROM zvmm_info_condi
   WHERE knumh = a018-knumh
     AND kschl = c_kschl
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    MOVE: c_del_con TO wa_module-sts.
    EXIT.
  ENDIF.

  CLEAR: it_condition, it_condition[].

  SELECT *
    INTO TABLE it_condition
    FROM ztmm_assy_cost2
   WHERE vtype = it_output-vtype
     AND mcode = it_output-matnr+3(2)
     AND lifnr = it_output-lifnr
     AND seqno = it_output-seqno.
  IF sy-subrc NE 0.
    MOVE c_error  TO wa_module-indicator.
    MOVE text-b08 TO wa_module-msg.
  ENDIF.

  LOOP AT it_condition.
    MOVE: it_condition-kschl+2(2) TO w_index.

    CONCATENATE: 'WA_MODULE-ZP' w_index INTO w_module.

    ASSIGN: (w_module) TO <module>.
    IF sy-subrc NE 0. CONTINUE. ENDIF.

    MOVE: it_condition-kbetr TO <module>.
  ENDLOOP.


  IF a018-datab < p_datum.
    SELECT SINGLE *
      FROM a018
     WHERE kappl =  'M'
       AND kschl =  'PB00'
       AND matnr =  it_output-matnr
       AND lifnr =  it_output-lifnr
       AND ekorg =  c_ekorg
       AND esokz =  '0'
       AND datab >= p_datum.
    IF sy-subrc EQ 0.
      MOVE: 'X' TO wa_module-same_date_flg.
    ENDIF.
  ELSE.
    MOVE: 'X' TO wa_module-same_date_flg.
  ENDIF.
ENDFORM.                    " check_module
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SUB_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_sub_info_record.
  SET PARAMETER ID 'LIF' FIELD it_detail-lifnr.
  SET PARAMETER ID 'MAT' FIELD it_detail-idnrk.
  SET PARAMETER ID 'EKO' FIELD c_ekorg.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

  CLEAR: it_detail.
ENDFORM.                    " DISPLAY_SUB_INFO_RECORD
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
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_all.
  DO.
    CLEAR: it_module.

    READ LINE sy-index FIELD VALUE it_module-chbox.
    IF sy-subrc NE 0. EXIT. ENDIF.

    CHECK NOT ( it_module-indicator EQ c_incorrect OR
                it_module-indicator EQ c_success ).

    it_module-chbox = 'X'.

    MODIFY LINE sy-index FIELD VALUE it_module-chbox.
  ENDDO.

  CLEAR: it_module.
ENDFORM.                    " SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deselect_all.
  DO.
    CLEAR: it_module.

    READ LINE sy-index FIELD VALUE it_module-chbox.
    IF sy-subrc NE 0. EXIT. ENDIF.

    CHECK NOT ( it_module-indicator EQ c_incorrect OR
                it_module-indicator EQ c_success ).

    it_module-chbox = ' '.

    MODIFY LINE sy-index FIELD VALUE it_module-chbox.
  ENDDO.

  CLEAR: it_module.
ENDFORM.                    " DESELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  ASCENDING_RTN
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
ENDFORM.                    " ASCENDING_RTN
*&---------------------------------------------------------------------*
*&      Form  DESCENDING_RTN
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
ENDFORM.                    " DESCENDING_RTN
*&---------------------------------------------------------------------*
*&      Form  ASCENDING_MODULE
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
ENDFORM.                    " ASCENDING_MODULE
*&---------------------------------------------------------------------*
*&      Form  ascending_SUB
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
ENDFORM.                    " ascending_SUB
*&---------------------------------------------------------------------*
*&      Form  SET_IT_DETAIL
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
          it_sub-peinh   TO it_detail-peinh,
          it_sub-kzust   TO it_detail-kzust,
          it_sub-amount  TO it_detail-dmbtr,
          it_sub-zp12    TO it_detail-zp12,
          it_sub-zp13    TO it_detail-zp13,
          it_sub-waers   TO it_detail-waers,
          it_sub-msg     TO it_detail-msg.
    APPEND it_detail.
  ENDLOOP.

  SORT it_detail BY upgvc sposn.
ENDFORM.                    " SET_IT_DETAIL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_IT_DETAIL
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
ENDFORM.                    " DISPLAY_IT_DETAIL
*&---------------------------------------------------------------------*
*&      Form  DEscending_module
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
ENDFORM.                    " DEscending_module
*&---------------------------------------------------------------------*
*&      Form  DEscending_SUB
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
ENDFORM.                    " DEscending_SUB
*&---------------------------------------------------------------------*
*&      Form  excute_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_rtn.
  DATA: lw_tabix LIKE sy-tabix.

  DATA: lt_module LIKE it_module OCCURS 0 WITH HEADER LINE.

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


** Added by Furong on 10/08/10
    MOVE: sy-tabix TO lw_tabix.

    READ TABLE it_error_module WITH KEY matnr = it_module-matnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_error_part WITH KEY matnr = it_module-matnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
** End of addition

** Added by Furong on 10/08/10
*    SELECT SINGLE *
*        FROM ZTPP_MOD_BOM_HIS
*        WHERE MATNR = IT_MODULE-MATNR.
**          AND CONF_DATE <> P_DATUM.
*    IF SY-SUBRC = 0.
*      CONTINUE.
*    ENDIF.
** End of addition

*    MOVE: SY-TABIX TO LW_TABIX.

    CASE it_module-sts.
      WHEN c_new.
        PERFORM create_info_record.
      WHEN c_no_cond OR c_exist.
        PERFORM create_info_condition.
    ENDCASE.

    MODIFY it_module INDEX lw_tabix.
  ENDDO.

** Addition by Furong on 10/11/10
  PERFORM save_price_history.
** End of addition

  sy-lsind = sy-lsind - 1.
  SORT it_module BY indicator lifnr matnr.
  PERFORM display_data.
ENDFORM.                    " excute_rtn
*&---------------------------------------------------------------------*
*&      Form  create_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_info_record.
  CLEAR: it_module-indicator.

  PERFORM set_it_condition.
  PERFORM generate_bdc_me11.
ENDFORM.                    " create_info_record
*&---------------------------------------------------------------------*
*&      Form  create_info_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_info_condition.
  CLEAR: it_module-indicator.

  PERFORM set_it_condition.
  PERFORM generate_bdc_me12.
ENDFORM.                    " create_info_condition

*------------------------------------------------*
*       FORM DYNPRO
*------------------------------------------------*
*  -->  DYNBEGIN
*  -->  NAME
*  -->  VALUE
*------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-program,
          value TO bdc_tab-dynpro,
          'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-fnam,
          value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.
ENDFORM.                    "dynpro
*&---------------------------------------------------------------------*
*&      Form  SET_IT_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_condition.
  CLEAR: it_condition, it_condition[].

  SELECT *
    INTO TABLE it_condition
    FROM ztmm_assy_cost2
   WHERE vtype = it_module-vtype
     AND mcode = it_module-matnr+3(2)
     AND lifnr = it_module-lifnr
     AND seqno = it_module-seqno.
  IF sy-subrc NE 0.
    MOVE c_error  TO it_module-indicator.
    MOVE text-b08 TO it_module-msg.
  ENDIF.
ENDFORM.                    " SET_IT_CONDITION
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_me11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_me11.
  DATA: lw_kbetr(15),
        lw_datab(10),
        lw_ztir(15),
        wa_kbetr LIKE it_module-moamt.

  CHECK it_module-indicator NE c_error.

  REFRESH bdc_tab.

  IF it_module-ztir > 0.
    WRITE it_module-ztir CURRENCY it_module-waers TO lw_ztir.
    wa_kbetr = it_module-moamt - it_module-ztir.
    WRITE wa_kbetr CURRENCY it_module-waers TO lw_kbetr.
  ELSE.
    WRITE it_module-moamt CURRENCY it_module-waers TO lw_kbetr.
  ENDIF.
  WRITE p_datum TO lw_datab.

  PERFORM dynpro USING:
        'X' 'SAPMM06I'              '0100',
        ' ' 'EINA-LIFNR'            it_module-lifnr,
        ' ' 'EINA-MATNR'            it_module-matnr,
        ' ' 'EINE-EKORG'            c_ekorg,
        ' ' 'EINE-WERKS'            ' ',
        ' ' 'EINA-INFNR'            ' ',
        ' ' 'RM06I-NORMB'           'X',
        ' ' 'BDC_OKCODE'            '/00',

        'X' 'SAPMM06I'              '0101',
        ' ' 'EINA-MEINS'            'EA',
        ' ' 'BDC_OKCODE'            '/00',

        'X' 'SAPMM06I'              '0102',
        ' ' 'EINE-EKGRP'            it_module-ekgrp,
        ' ' 'EINE-NORBM'            '1',
        ' ' 'EINE-UEBTK'            'X',
        ' ' 'EINE-WEBRE'            'X',
        ' ' 'EINE-MWSKZ'            'U0',
        ' ' 'EINE-NETPR'            lw_kbetr,
        ' ' 'EINE-WAERS'            it_module-waers,
        ' ' 'EINE-PEINH'            '1',
        ' ' 'EINE-BPRME'            'EA',
        ' ' 'BDC_OKCODE'            '=KO',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV13A-DATAB'           lw_datab,
        ' ' 'KONP-KBETR(01)'        lw_kbetr,
        ' ' 'BDC_OKCODE'            '=KDAT',

        'X' 'SAPMV13A'              '0200',
        ' ' 'KONH-KZUST'            it_module-kzust,
        ' ' 'BDC_OKCODE'            '=KPOS',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV130-SELKZ(1)'        'X',
        ' ' 'BDC_CURSOR'            'KONP-KBETR(01)',
        ' ' 'BDC_OKCODE'            '=EINF'.

  IF it_module-ztir > 0.
    PERFORM dynpro USING:
         'X' 'SAPMV13A'              '0201',
         ' ' 'KONP-KSCHL(02)'        c_ztir,
         ' ' 'KONP-KBETR(02)'        lw_ztir,
         ' ' 'BDC_OKCODE'            '/00',

         'X' 'SAPMV13A'              '0201',
         ' ' 'RV130-SELKZ(2)'        'X',
         ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
         ' ' 'BDC_OKCODE'            '=EINF'.
  ENDIF.


  SORT it_condition BY kschl DESCENDING.

  LOOP AT it_condition.
    WRITE: it_condition-kbetr CURRENCY it_module-waers TO lw_kbetr.

    PERFORM dynpro USING:
          'X' 'SAPMV13A'              '0201',
          ' ' 'KONP-KSCHL(02)'        it_condition-kschl,
          ' ' 'KONP-KBETR(02)'        lw_kbetr,
          ' ' 'BDC_OKCODE'            '/00',

          'X' 'SAPMV13A'              '0201',
          ' ' 'RV130-SELKZ(2)'        'X',
          ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
          ' ' 'BDC_OKCODE'            '=EINF'.

    AT LAST.
      PERFORM dynpro USING:
            'X' 'SAPMV13A'              '0201',
            ' ' 'BDC_OKCODE'            '=SICH'.
    ENDAT.
  ENDLOOP.

  CALL TRANSACTION 'ME11' USING  bdc_tab
*                          MODE   'N'
*                          UPDATE 'S'.
                          MODE   w_mode
                          UPDATE w_update.

  IF sy-subrc NE 0 OR sy-msgno NE '331'.
    CALL FUNCTION 'RKC_MSG_STRING'
      EXPORTING
        id      = sy-msgid
        mtype   = sy-msgty
        number  = sy-msgno
        par1    = sy-msgv1
        par2    = sy-msgv2
        par3    = sy-msgv3
        par4    = sy-msgv4
      IMPORTING
        msg_lin = it_module-msg.

    it_module-indicator  = c_error.

    it_error_module-matnr = it_module-matnr.
    it_error_module-msg = it_module-msg.
    it_error_module-sts = c_error.
    APPEND it_error_module.
  ELSE.
    it_module-indicator  = c_success.
    it_updated_module-matnr = it_module-matnr.
    it_updated_module-asytr = it_module-asytr.
    it_updated_module-msg = 'Successfuly Created'.
    APPEND it_updated_module.
  ENDIF.
ENDFORM.                    " generate_bdc_me11
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_ME12
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_me12.
  DATA: lw_kbetr(15),
        lw_datab(10),
        lw_dmbtr LIKE mseg-dmbtr,
        lw_ztir(15),
        wa_kbetr LIKE it_module-moamt.
  .

  CHECK it_module-indicator NE c_error.

  REFRESH bdc_tab.

  IF it_module-ztir > 0.
    WRITE it_module-ztir CURRENCY it_module-waers TO lw_ztir.
    wa_kbetr = it_module-moamt - it_module-ztir.
    WRITE wa_kbetr CURRENCY it_module-waers TO lw_kbetr.
  ELSE.
    WRITE it_module-moamt CURRENCY it_module-waers TO lw_kbetr.
  ENDIF.

*  WRITE it_module-moamt CURRENCY it_module-waers TO lw_kbetr.
  WRITE p_datum TO lw_datab.

  PERFORM dynpro USING:
        'X' 'SAPMM06I'              '0100',
        ' ' 'EINA-LIFNR'            it_module-lifnr,
        ' ' 'EINA-MATNR'            it_module-matnr,
        ' ' 'EINE-EKORG'            c_ekorg,
        ' ' 'EINE-WERKS'            ' ',
        ' ' 'EINA-INFNR'            ' ',
        ' ' 'RM06I-NORMB'           'X',
        ' ' 'BDC_OKCODE'            '/00',

        'X' 'SAPMM06I'              '0101',
        ' ' 'BDC_OKCODE'            '=KO',

        'X' 'SAPLV14A'              '0102',
        ' ' 'BDC_OKCODE'            '=NEWD',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV13A-DATAB'           lw_datab,
        ' ' 'KONP-KBETR(01)'        lw_kbetr,
        ' ' 'BDC_OKCODE'            '=KDAT',

        'X' 'SAPMV13A'              '0200',
        ' ' 'KONH-KZUST'            it_module-kzust,
        ' ' 'BDC_OKCODE'            '=KPOS',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV130-SELKZ(1)'        'X',
        ' ' 'BDC_CURSOR'            'KONP-KBETR(01)',
        ' ' 'BDC_OKCODE'            '=EINF'.

  IF it_module-ztir > 0.
    PERFORM dynpro USING:
         'X' 'SAPMV13A'              '0201',
         ' ' 'KONP-KSCHL(02)'        c_ztir,
         ' ' 'KONP-KBETR(02)'        lw_ztir,
         ' ' 'BDC_OKCODE'            '/00',

         'X' 'SAPMV13A'              '0201',
         ' ' 'RV130-SELKZ(2)'        'X',
         ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
         ' ' 'BDC_OKCODE'            '=EINF'.
  ENDIF.



  SORT it_condition BY kschl DESCENDING.

*  LOOP AT it_condition.
*    WRITE: it_condition-kbetr CURRENCY it_module-waers TO lw_kbetr.
*
*    PERFORM dynpro USING:
*          'X' 'SAPMV13A'              '0201',
*          ' ' 'KONP-KSCHL(02)'        it_condition-kschl,
*          ' ' 'KONP-KBETR(02)'        lw_kbetr,
*          ' ' 'BDC_OKCODE'            '/00',
*
*          'X' 'SAPMV13A'              '0201',
*          ' ' 'RV130-SELKZ(2)'        'X',
*          ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
*          ' ' 'BDC_OKCODE'            '=EINF'.
*
*    AT LAST.
*      PERFORM dynpro USING:
*            'X' 'SAPMV13A'              '0201',
*            ' ' 'BDC_OKCODE'            '=SICH'.
*
*      IF it_module-same_date_flg EQ ''.
*        PERFORM dynpro USING:
*              'X' 'SAPMV13A'              '0121',
*              ' ' 'BDC_OKCODE'            '=BSTA'.
*
*      ENDIF.
*    ENDAT.
*  ENDLOOP.


  DO 100 TIMES.
    w_index = 100 - sy-index.

    CONCATENATE: 'IT_MODULE-ZP' w_index INTO w_module.

    ASSIGN: (w_module)  TO <module>.

    CHECK sy-subrc EQ 0.

    MOVE: <module> TO lw_dmbtr.
    WRITE: lw_dmbtr CURRENCY it_module-waers TO lw_kbetr.

    PERFORM dynpro USING:
          'X' 'SAPMV13A'              '0201',
          ' ' 'KONP-KSCHL(02)'        w_module+10(4),
          ' ' 'KONP-KBETR(02)'        lw_kbetr,
          ' ' 'BDC_OKCODE'            '/00',

          'X' 'SAPMV13A'              '0201',
          ' ' 'RV130-SELKZ(2)'        'X',
          ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
          ' ' 'BDC_OKCODE'            '=EINF'.

    CHECK w_index EQ '01'.

    PERFORM dynpro USING:
          'X' 'SAPMV13A'              '0201',
          ' ' 'BDC_OKCODE'            '=SICH'.

    IF it_module-same_date_flg EQ ''.
      PERFORM dynpro USING:
            'X' 'SAPMV13A'              '0121',
            ' ' 'BDC_OKCODE'            '=BSTA'.

    ENDIF.

    EXIT.
  ENDDO.

  CALL TRANSACTION 'ME12' USING  bdc_tab
*                          MODE   'N'
*                          UPDATE 'S'.
                          MODE   w_mode
                          UPDATE w_update.

  IF sy-subrc NE 0 OR sy-msgno NE '335'.
    CALL FUNCTION 'RKC_MSG_STRING'
      EXPORTING
        id      = sy-msgid
        mtype   = sy-msgty
        number  = sy-msgno
        par1    = sy-msgv1
        par2    = sy-msgv2
        par3    = sy-msgv3
        par4    = sy-msgv4
      IMPORTING
        msg_lin = it_module-msg.

    it_module-indicator  = c_error.
    it_error_module-matnr = it_module-matnr.
    it_error_module-msg = it_module-msg.
    it_error_module-sts = c_error.
    APPEND it_error_module.

  ELSE.
    it_module-indicator  = c_success.
    it_updated_module-matnr = it_module-matnr.
    it_updated_module-asytr = it_module-asytr.
    it_updated_module-msg = 'Successfuly Changed'.
    APPEND it_updated_module.
  ENDIF.
ENDFORM.                    " GENERATE_BDC_ME12
*&---------------------------------------------------------------------*
*&      Form  processing_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processing_rtn.

  LOOP AT it_module.

    CHECK it_module-indicator EQ c_ready.
    " OR it_module-indicator EQ c_warning.

** Added by Furong on 10/08/10

    READ TABLE it_error_module WITH KEY matnr = it_module-matnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_error_part WITH KEY matnr = it_module-matnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

*    SELECT SINGLE *
*        FROM ZTPP_MOD_BOM_HIS
*        WHERE MATNR = IT_MODULE-MATNR.
**          AND CONF_DATE <> P_DATUM.
*    IF SY-SUBRC = 0.
*      CONTINUE.
*    ENDIF.
** End of addition

    SELECT SINGLE b~effpr INTO eine-effpr
                FROM eina AS a INNER JOIN eine AS b
                ON a~infnr = b~infnr
                WHERE a~matnr = it_module-matnr
                  AND a~lifnr = it_module-lifnr
                  AND a~loekz = ' '
                  AND b~werks = ' '
                  AND b~ekorg = c_ekorg
                  AND b~loekz = ' '.

    IF eine-effpr <> it_module-moamt.

      CASE it_module-sts.
        WHEN c_new.
          PERFORM create_info_record.
        WHEN c_no_cond OR c_exist.
          PERFORM create_info_condition.
      ENDCASE.
      MODIFY it_module.
    ENDIF.

  ENDLOOP.
** Addition by Furong on 10/11/10
  PERFORM save_price_history.
** End of addition

  SORT it_module BY indicator lifnr matnr.
  PERFORM display_data.
ENDFORM.                    " processing_rtn
*&---------------------------------------------------------------------*
*&      Form  check_locking
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_locking.
  CALL FUNCTION 'ENQUEUE_EZ_ZSMM_MODULE'
    EXPORTING
      mode_zsmm_module = 'E'
      mandt            = sy-mandt
      vtype            = s_vtype-low
      mcode            = p_mcode
      lifnr            = p_lifnr
      matnr            = p_matnr
      ekgrp            = p_ekgrp
      datum            = p_datum
    EXCEPTIONS
      foreign_lock     = 1
      system_failure   = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " check_locking
*&---------------------------------------------------------------------*
*&      Form  SET_REASON_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_reason_code USING pw_price_flg pw_new_condition.
  DATA: lw_datum LIKE sy-datum,
        lw_peinh LIKE ekpo-peinh,
        lw_netpr LIKE ekpo-netpr,
        lw_info(50).
  FIELD-SYMBOLS: <lw_info>.
*        lw_moamt LIKE it_module-moamt.

  DATA: BEGIN OF lt_condition OCCURS 0,
          kbetr LIKE zvmm_info_condi-kbetr,
          kpein LIKE zvmm_info_condi-kpein,
          kmein LIKE zvmm_info_condi-kmein,
          kschl LIKE zvmm_info_condi-kschl,
          kzust LIKE zvmm_info_condi-kzust,
        END   OF lt_condition.

  DATA: lw_last_info LIKE zsmm_custom_condition_floating.

  CLEAR: eina, a018, it_module-netpr_y, it_module-kzust_y.
  IF it_module-sts EQ c_new     OR
     it_module-sts EQ c_deleted.
    IF pw_price_flg EQ 'X'.
      it_module-kzust = c_rsn01.
    ELSE.
      it_module-kzust = c_rsn02.
    ENDIF.
    EXIT.
  ENDIF.

*----- read yesterday info record
  lw_datum = p_datum - 1.
  SELECT SINGLE *
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_module-matnr
     AND lifnr =  it_module-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab <= lw_datum
     AND datbi >= lw_datum.
  IF sy-subrc NE 0.
    IF pw_price_flg EQ 'X'.
      it_module-kzust = c_rsn01.
    ELSE.
      it_module-kzust = c_rsn02.
    ENDIF.
    MOVE: 'X' TO pw_new_condition.
    EXIT.
  ENDIF.

*  SELECT SINGLE kbetr kpein kzust
*    INTO (lw_netpr, lw_peinh, it_module-kzust_y)
*    FROM zvmm_info_condi
*   WHERE knumh = a018-knumh
*     AND kschl = c_kschl
*     AND loevm_ko = ' '.
*  IF sy-subrc NE 0.
*    IF pw_price_flg EQ 'X'.
*      it_module-kzust = c_rsn01.
*    ELSE.
*      it_module-kzust = c_rsn02.
*    ENDIF.
*    EXIT.
*  ENDIF.
*
*  it_module-netpr_y = lw_netpr / lw_peinh.


  SELECT kbetr kpein kzust kschl
    INTO CORRESPONDING FIELDS OF TABLE lt_condition
    FROM zvmm_info_condi
   WHERE knumh = a018-knumh
     AND ( kschl =    c_kschl OR
           kschl LIKE 'ZP%' )
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    IF pw_price_flg EQ 'X'.
      it_module-kzust = c_rsn01.
    ELSE.
      it_module-kzust = c_rsn02.
    ENDIF.
    MOVE: 'X' TO pw_new_condition.
    EXIT.
  ENDIF.

  LOOP AT lt_condition.
    CASE lt_condition-kschl.
      WHEN c_kschl.
        MOVE: lt_condition-kzust TO it_module-kzust_y.
        it_module-netpr_y = lt_condition-kbetr / lt_condition-kpein.
      WHEN OTHERS.
        MOVE: lt_condition-kschl+2(2) TO w_index.

        CONCATENATE: 'LW_LAST_INFO-ZP' w_index INTO w_module.

        ASSIGN: (w_module) TO <module>.
        IF sy-subrc NE 0. CONTINUE. ENDIF.

        IF lt_condition-kpein EQ 0.
          <module> = 0.
        ELSE.
          <module> = lt_condition-kbetr / lt_condition-kpein.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  IF     it_module-netpr_y EQ it_module-moamt.
    IF pw_price_flg EQ 'X'.
      it_module-kzust = c_rsn01.
    ELSE.
      it_module-kzust = c_rsn02.
    ENDIF.

    IF it_module-kzust NE it_module-kzust_y.
      MOVE: 'X' TO pw_new_condition.
    ENDIF.
  ELSEIF it_module-netpr_y > it_module-moamt.
    IF pw_price_flg EQ 'X'.
      it_module-kzust = c_rsn03.
    ELSE.
      it_module-kzust = c_rsn04.
    ENDIF.
    MOVE: 'X' TO pw_new_condition.
  ELSEIF it_module-netpr_y < it_module-moamt.
    IF pw_price_flg EQ 'X'.
      it_module-kzust = c_rsn05.
    ELSE.
      it_module-kzust = c_rsn06.
    ENDIF.
    MOVE: 'X' TO pw_new_condition.
  ENDIF.

  IF pw_new_condition EQ space.
    DO.
      MOVE: sy-index TO w_index.

      CONCATENATE: 'LW_LAST_INFO-ZP' w_index INTO lw_info,
                   'IT_MODULE-ZP'    w_index INTO w_module.

      ASSIGN: (lw_info)  TO <lw_info>,
              (w_module) TO <module>.
      IF sy-subrc NE 0. EXIT. ENDIF.

      IF <lw_info> NE <module>.
        MOVE: 'X' TO pw_new_condition.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    " SET_REASON_CODE
*&---------------------------------------------------------------------*
*&      Form  input_day_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_day_info_record.
  DATA: BEGIN OF lt_condition OCCURS 0,
          kbetr LIKE zvmm_info_condi-kbetr,
          kpein LIKE zvmm_info_condi-kpein,
          kmein LIKE zvmm_info_condi-kmein,
          kschl LIKE zvmm_info_condi-kschl,
          kzust LIKE zvmm_info_condi-kzust,
        END   OF lt_condition.

  DATA: BEGIN OF it_eina_eine_temp OCCURS 0,
        matnr LIKE eina-matnr,
        lifnr LIKE eina-lifnr,
        wglif LIKE eina-wglif,
        prdat LIKE eine-prdat,
        END OF it_eina_eine_temp.

  DATA: l_line TYPE i.

*----- Check Material Master
  IF it_output-cmaktx IS INITIAL.
    MOVE: c_no_matl TO wa_sub-sts.
    EXIT.
  ENDIF.

*** INSERTED BY FURONG ON 27/07/2005
  SELECT matnr a~lifnr wglif prdat INTO TABLE it_eina_eine_temp
   FROM eina AS a INNER JOIN eine AS b
     ON a~infnr = b~infnr
  WHERE a~matnr = it_output-comp
    AND a~urzzt = 'SUB'
    AND a~loekz = ' '
    AND b~werks = ' '
    AND b~ekorg = c_ekorg
    AND b~loekz = ' '.

*    AND b~prdat >= p_datum.
  IF sy-subrc NE 0.
    MOVE: c_no_info TO wa_sub-sts.
    CLEAR: it_error_part.
    it_error_part-lifnr = it_output-lifnr.
    it_error_part-matnr = it_output-matnr.
    it_error_part-comp = it_output-comp.
    it_error_part-cmaktx = it_output-cmaktx.
    it_error_part-sts = c_no_info.
    it_error_part-msg = text-b02.
    MOVE: c_no_price TO wa_sub-sts.
    APPEND it_error_part.
** on 04/12/12
    it_error_info-comp = it_output-comp.
    it_error_info-sts = c_no_info.
    it_error_info-msg = text-b02.
    APPEND it_error_info.
** End 04/12/12
    EXIT.
  ENDIF.

  DESCRIBE TABLE it_eina_eine_temp LINES l_line.
  IF l_line > 1.

    LOOP AT it_eina_eine_temp.
      SELECT SINGLE * INTO ztmm_assy_cost1
      FROM ztmm_assy_cost1
      WHERE  vtype = it_output-matnr+0(3)
         AND mcode = it_output-matnr+3(2)
         AND lifnr = it_eina_eine_temp-lifnr
         AND datab <= p_datum
         AND datbi >= p_datum .
      IF sy-subrc = 0.
      ELSE.
        DELETE it_eina_eine_temp.
      ENDIF.
    ENDLOOP.

    CLEAR: l_line.
    DESCRIBE TABLE it_eina_eine_temp LINES l_line.
    IF l_line <> 1.
** added check logic for duplicated records on 01/31/11
      LOOP AT it_eina_eine_temp.
        CLEAR: eina, a018.
        wa_sub-lifnr = it_eina_eine_temp-lifnr.
        SELECT SINGLE knumh datab datbi
                      INTO (a018-knumh, wa_sub-datab, wa_sub-datbi)
                        FROM a018
                        WHERE kappl =  'M'
                          AND kschl =  'PB00'
                          AND matnr =  it_output-comp
                          AND lifnr =  wa_sub-lifnr
                          AND ekorg =  c_ekorg
                          AND esokz =  '0'
                          AND datab <= p_datum
                          AND datbi >= p_datum.
        IF sy-subrc EQ 0.
        ELSE.
          DELETE it_eina_eine_temp.
        ENDIF.
      ENDLOOP.
      CLEAR: l_line.
      DESCRIBE TABLE it_eina_eine_temp LINES l_line.
      IF l_line <> 1.
        CLEAR: it_error_part.
        it_error_part-lifnr = it_output-lifnr.
        it_error_part-matnr = it_output-matnr.
        it_error_part-comp = it_output-comp.
        it_error_part-cmaktx = it_output-cmaktx.
        it_error_part-sts = c_overlap_info.
        it_error_part-msg = text-b13.
        APPEND it_error_part.
        MOVE: c_overlap_info TO wa_sub-sts.
** on 04/12/12
        it_error_info-comp = it_output-comp.
        it_error_info-sts = c_overlap_info.
        it_error_info-msg = text-b13.
        APPEND it_error_info.
** End 04/12/12
        EXIT.
      ENDIF.
    ENDIF.

  ELSE.

*-< Victor 04.16.2014  Check Valid date
    LOOP AT it_eina_eine_temp.
      wa_sub-lifnr = it_eina_eine_temp-lifnr.
      SELECT SINGLE knumh datab datbi
                    INTO (a018-knumh, wa_sub-datab, wa_sub-datbi)
                      FROM a018
                      WHERE kappl =  'M'
                        AND kschl =  'PB00'
                        AND matnr =  it_output-comp
                        AND lifnr =  wa_sub-lifnr
                        AND ekorg =  c_ekorg
                        AND esokz =  '0'
                        AND datab <= p_datum
                        AND datbi >= p_datum.
      IF sy-subrc <> 0.
        CLEAR: it_error_part.
        it_error_part-lifnr = it_output-lifnr.
        it_error_part-matnr = it_output-matnr.
        it_error_part-comp  = it_output-comp.
        it_error_part-cmaktx = it_output-cmaktx.
        it_error_part-sts = c_overlap_info.
        it_error_part-msg = text-b02.
        APPEND it_error_part.
        MOVE: c_overlap_info TO wa_sub-sts.
        it_error_info-comp = it_output-comp.
        it_error_info-sts = c_overlap_info.
        it_error_info-msg = text-b02.
        APPEND it_error_info.
        EXIT.

      ENDIF.
    ENDLOOP.
  ENDIF.
** End of check


*----- Read submaterial price
  LOOP AT it_eina_eine_temp.
    CLEAR: eina, a018.
    wa_sub-lifnr = it_eina_eine_temp-lifnr.
    SELECT SINGLE knumh datab datbi
                  INTO (a018-knumh, wa_sub-datab, wa_sub-datbi)
                    FROM a018
                    WHERE kappl =  'M'
                      AND kschl =  'PB00'
                      AND matnr =  it_output-comp
                      AND lifnr =  wa_sub-lifnr
                      AND ekorg =  c_ekorg
                      AND esokz =  '0'
                      AND datab <= p_datum
                      AND datbi >= p_datum.
    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDLOOP.
*  if sy-subrc ne 0.
  IF a018-knumh IS INITIAL.
    MOVE: c_no_cond TO wa_sub-sts.
    EXIT.
  ENDIF.
*** END OF INSERTION

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
** Added by Furong on 10/08/10
        IF lt_condition-kbetr = 0.
          MOVE c_no_price TO wa_sub-sts.
          CLEAR: it_error_part.
          it_error_part-lifnr = it_output-lifnr.
          it_error_part-matnr = it_output-matnr.
          it_error_part-comp = it_output-comp.
          it_error_part-cmaktx = it_output-cmaktx.
          it_error_part-sts = c_no_price.
          it_error_part-msg = text-b12.
          APPEND it_error_part.
** ON 04/12/12
          it_error_info-comp = it_output-comp.
          it_error_info-sts = c_no_price.
          it_error_info-msg = text-b12.
          APPEND it_error_info.
** End 04/12/12

        ENDIF.
** End of addition
        IF it_eina_eine_temp-wglif = 'ZTIR'.
          wa_sub-ztir = lt_condition-kbetr.
        ENDIF.
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
  IF NOT ( ( it_sub-meins EQ wa_sub-kmein AND
             it_sub-meins EQ it_sub-unit  AND
             wa_sub-kmein EQ it_sub-unit )   ).
    MOVE: c_uom_err TO wa_sub-sts.
  ENDIF.
ENDFORM.                    " input_day_info_record
*&---------------------------------------------------------------------*
*&      Form  yesterday_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yesterday_info_record.
  DATA: lw_datum LIKE sy-datum.

  lw_datum = p_datum - 1.

*----- Read submaterial price
  SELECT SINGLE *
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_output-comp
     AND lifnr =  wa_sub-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab <= lw_datum
     AND datbi >= lw_datum.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zvmm_info_condi
   WHERE knumh = a018-knumh
     AND kschl = c_kschl
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  wa_sub-amount_y = wa_sub-qnty * zvmm_info_condi-kbetr /
                                  zvmm_info_condi-kpein.
ENDFORM.                    " yesterday_info_record
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
*  CHECK it_output-matnr+3(2) EQ 'CP'
*** Requested by Mr. Choi, changed by Furong on 01/06/2006
** Begin of changes - UD1K919255 Backed out earlier changes
*  AND  it_output-stgb          EQ 'U'.
*** end of change  - UD1K919255 Backed out earlier changes
*
*  MOVE: it_output-matnr+10(3) TO lw_int_key.
*
*  SELECT SINGLE * FROM ztmm_cp_color WHERE copit EQ it_output-matnr
*                                       AND inkey EQ lw_int_key
*                                       AND submt EQ it_output-comp
*                                       AND datab <  p_datum
*                                       AND datbi >= p_datum.
*  IF sy-subrc NE 0.
*    CLEAR: p_continue.
*  ENDIF.
*ENDFORM.                    " check_cockpit_module_color
*&---------------------------------------------------------------------*
*&      Module  move_itab_to_screen_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_itab_to_screen_9000 OUTPUT.

ENDMODULE.                 " move_itab_to_screen_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_indicator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.
  DATA: l_subject(40) TYPE c VALUE 'Module Price Change List'.

  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
          it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE,
          gd_cnt TYPE i,
          gd_sent_all(1) TYPE c,
          gd_doc_data LIKE sodocchgi1.
*          GD_ERROR TYPE SY-SUBRC.

  DATA: l_lines_p TYPE i,
        l_lines_m TYPE i,
        l_lines_upm TYPE i.

** Checking error data

  DESCRIBE TABLE it_error_part LINES l_lines_p.
  DESCRIBE TABLE it_error_module LINES l_lines_m.
  DESCRIBE TABLE it_updated_module LINES l_lines_upm.

  IF l_lines_p = 0 AND l_lines_m = 0 AND  l_lines_upm = 0.
    EXIT.
  ENDIF.
**

  CLEAR: it_mail,it_mail[].

** Make body part of email
  MOVE 'RE: Module Price Update' TO it_mail.

  APPEND it_mail.
  CLEAR: it_mail.
*  MOVE '---------------------------------------------' TO IT_MAIL.
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.

  APPEND it_mail.
  CLEAR: it_mail.

  MOVE 'Error with parts:' TO it_mail.
  APPEND it_mail.
  CLEAR: it_mail.
  MOVE '===============' TO it_mail.
  APPEND it_mail.
  CLEAR: it_mail.

*  MOVE: 'Suppier' TO IT_MAIL+0(10),
  MOVE: 'Module' TO  it_mail+0(20),
       'Part' TO  it_mail+20(20),
       ' Description' TO it_mail+40(30),
       'Message' TO  it_mail+70(50).
  APPEND it_mail.
  CLEAR: it_mail.

*  MOVE: '----------' TO IT_MAIL+0(10),
  MOVE: '--------------------' TO  it_mail+0(20),
        '--------------------' TO  it_mail+20(20),
        '------------------------------' TO  it_mail+40(30),
        '------------------------------' TO  it_mail+70(30),
        '--------------------' TO  it_mail+100(20).
  APPEND it_mail.
  CLEAR: it_mail.

  LOOP AT it_error_part.
*    MOVE: IT_ERROR_PART-LIFNR TO IT_MAIL+0(10),
    MOVE:it_error_part-matnr TO  it_mail+0(20),
         it_error_part-comp TO  it_mail+20(20),
         it_error_part-cmaktx TO  it_mail+40(30),
         it_error_part-msg TO  it_mail+70(50).
    APPEND it_mail.
    CLEAR: it_mail.
  ENDLOOP.


  APPEND it_mail.
  APPEND it_mail.
  APPEND it_mail.

  MOVE 'Error with Module(s):' TO it_mail.
  APPEND it_mail.
  CLEAR: it_mail.
  MOVE '===================' TO it_mail.
  APPEND it_mail.
  CLEAR: it_mail.

*  MOVE: 'Suppier' TO IT_MAIL+0(10),
  MOVE:  'Module' TO  it_mail+0(20),
        'Message' TO  it_mail+20(60).
  APPEND it_mail.
  CLEAR: it_mail.

*  MOVE: '----------' TO IT_MAIL+0(10),
  MOVE: '--------------------' TO  it_mail+0(20),
        '----------------------------------------' TO  it_mail+20(40),
*        '----------------------------------------' TO  it_mail+60(40),
        '--------------------' TO  it_mail+60(20).
  APPEND it_mail.
  CLEAR: it_mail.

  LOOP AT it_error_module.
*    MOVE: IT_ERROR_MODULE-LIFNR TO IT_MAIL+0(10),
    MOVE: it_error_module-matnr TO  it_mail+0(20),
          it_error_module-msg TO  it_mail+20(60).
    APPEND it_mail.
  ENDLOOP.


  APPEND it_mail.
  APPEND it_mail.
  APPEND it_mail.

  MOVE 'Successfully Updated Module(s):' TO it_mail.
  APPEND it_mail.
  CLEAR: it_mail.
  MOVE '================================' TO it_mail.
  APPEND it_mail.
  CLEAR: it_mail.

*  MOVE: 'Suppier' TO IT_MAIL+0(10),
  MOVE:  'Module' TO  it_mail+0(20),
        'Message' TO  it_mail+20(100).
  APPEND it_mail.
  CLEAR: it_mail.

*  MOVE: '----------' TO IT_MAIL+0(10),
  MOVE: '--------------------' TO  it_mail+0(20),
        '----------------------------------------' TO  it_mail+20(40),
*        '----------------------------------------' TO  it_mail+60(40),
        '--------------------' TO  it_mail+60(20).
  APPEND it_mail.
  CLEAR: it_mail.

  LOOP AT it_updated_module.
    MOVE: it_updated_module-matnr TO  it_mail+0(20),
          it_updated_module-msg TO  it_mail+20(60).
    APPEND it_mail.
  ENDLOOP.

  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr = l_subject.
  gd_doc_data-sensitivty = 'F'.

* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_mail LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

* Add the recipients email address
  CLEAR it_receivers.
  REFRESH it_receivers.
*  IT_RECEIVERS-RECEIVER = 'SAIFVAL'.
  it_receivers-receiver = 'PARTS_DEV'.
*  it_receivers-rec_type = 'U'.  " internet email
  it_receivers-rec_type = 'C'.
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = 'X'.
  it_receivers-notif_ndel = 'X'.
  APPEND it_receivers.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
*     PUT_IN_OUTBOX              = 'X'
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_txt               = it_mail
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

ENDFORM.                    " SEND_EMAIL
*
*&---------------------------------------------------------------------*
*&      Form  backup_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_price_history.
  DATA: it_mod_pri_bk LIKE TABLE OF ztmm_mod_pri_bk WITH HEADER LINE.
  DATA: it_pri_bk_tmp LIKE TABLE OF ztmm_mod_pri_bk WITH HEADER LINE.
  DATA: l_date LIKE sy-datum,
        l_time LIKE sy-uzeit,
        lv_date LIKE sy-datum,
        lv_time LIKE sy-uzeit.

*-< Victor 03.14.2014
  SORT it_sub BY matnr upgvc comp.
  LOOP AT it_module.
    CLEAR : it_pri_bk_tmp[], it_pri_bk_tmp.

    READ TABLE it_error_module WITH KEY matnr = it_module-matnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_error_part WITH KEY matnr = it_module-matnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_updated_module WITH KEY matnr = it_module-matnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    SELECT run_date run_time INTO (lv_date, lv_time)
    FROM ztmm_mod_pri_bk
      UP TO 1 ROWS
    WHERE matnr = it_module-matnr
      ORDER BY run_date DESCENDING
               run_time DESCENDING.
    ENDSELECT.

    IF lv_date IS NOT INITIAL AND  lv_time IS NOT INITIAL.
      SELECT * INTO TABLE it_pri_bk_tmp
      FROM ztmm_mod_pri_bk
       WHERE matnr = it_module-matnr
         AND run_date = lv_date
         AND run_time = lv_time.

      LOOP AT it_pri_bk_tmp.
        READ TABLE it_sub WITH KEY matnr = it_pri_bk_tmp-matnr
                                   upgvc = it_pri_bk_tmp-upgvc
                                   comp  = it_pri_bk_tmp-comp
                                            BINARY SEARCH.
        IF sy-subrc <> 0.
          it_updated_module-matnr = it_pri_bk_tmp-matnr.
          APPEND it_updated_module. CLEAR it_updated_module.
          EXIT.
        ENDIF.
      ENDLOOP.

    ELSE.
    ENDIF.
    CONTINUE.
  ENDLOOP.
*->

  l_date = sy-datum.
  l_time = sy-uzeit.
  SORT it_updated_module BY matnr.
  LOOP AT it_updated_module.
    it_mod_pri_bk-matnr  = it_updated_module-matnr.
    it_mod_pri_bk-run_date = l_date.
    it_mod_pri_bk-run_time = l_time.
    it_mod_pri_bk-run_user = sy-uname.
    it_mod_pri_bk-asytr = it_updated_module-asytr.
    LOOP AT it_sub WHERE matnr = it_updated_module-matnr.
*          IT_SUB-PREF    TO IT_DETAIL-SPOSN,
*          IT_SUB-MAKTX   TO IT_DETAIL-MAKTX,
*          IT_SUB-MSG     TO IT_DETAIL-MSG.
      it_mod_pri_bk-comp = it_sub-comp.
      it_mod_pri_bk-lifnr = it_sub-lifnr.
      it_mod_pri_bk-qnty = it_sub-qnty.
      it_mod_pri_bk-unit = it_sub-unit.
      it_mod_pri_bk-upgvc = it_sub-upgvc.
      it_mod_pri_bk-datab = it_sub-datab.
      it_mod_pri_bk-datbi = it_sub-datbi.
      it_mod_pri_bk-netpr = it_sub-netpr.
      it_mod_pri_bk-peinh = it_sub-peinh.
      it_mod_pri_bk-zp12 = it_sub-zp12.
      it_mod_pri_bk-zp13 = it_sub-zp13.
      it_mod_pri_bk-kzust = it_sub-kzust.
      it_mod_pri_bk-dmbtr = it_sub-amount.
      it_mod_pri_bk-waers = it_sub-waers.

      it_mod_pri_bk-input_date = p_datum.
      APPEND it_mod_pri_bk.
      CLEAR: it_mod_pri_bk-asytr.
    ENDLOOP.
  ENDLOOP.
  IF it_mod_pri_bk[] IS NOT INITIAL.
    INSERT ztmm_mod_pri_bk FROM TABLE it_mod_pri_bk.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " backup_data
*&---------------------------------------------------------------------*
*&      Form  CHECK_PART_INFO_FOR_MODULE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_part_info_for_module.
  DATA: l_index LIKE sy-tabix.

  READ TABLE it_error_info WITH KEY comp = it_output-comp.
  IF sy-subrc = 0.
    MOVE: it_error_info-sts TO wa_sub-sts.
    CLEAR: it_error_part.
    it_error_part-lifnr = it_output-lifnr.
    it_error_part-matnr = it_output-matnr.
    it_error_part-comp = it_output-comp.
    it_error_part-cmaktx = it_output-cmaktx.
    it_error_part-sts = it_error_info-sts.
    it_error_part-msg = it_error_info-msg.
    APPEND it_error_part.
  ENDIF.

ENDFORM.                    " CHECK_PART_INFO_FOR_MODULE
