************************************************************************
* Program Name      : ZEMMPM45R_DIS_MOD_PRC_EBOM_V
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
* 01/22/2007 Manju        UD1K930385   Convert Native SQL to  OPEN SQL
* 10/26/2007 Furong Wang               Rename and using standard BOM to
*                                      module component for EBOM
* 03/13/09   Furong Wang               copy ZEMMPM45R_DIS_MOD_PRC_EBOM
*                                      to change getting bom from
*                                      ztbm_ebom_ecm & ztbm_ebom_ecm_bk
************************************************************************
REPORT ZEMMPM45R_DIS_MOD_PRC_EBOM_V2 NO STANDARD PAGE HEADING
                                      LINE-SIZE  180
                                      LINE-COUNT  58.
INCLUDE: <ICON>.

TABLES: ZTMM_ASSY_COST1,
        ZTMM_ASSY_COST2,
*        ztmm_cp_color,
        LFA1,
        A018,
        MARA,
        T024,
        EINA,
        EINE,
        T001,
        V_T686A,
        V_T685A.

*----- Internal tables
DATA: BEGIN OF IT_MODULE OCCURS 0,
        INDICATOR,                              "Sub Material Status
        VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE, "Vehicle type
        NAME1     LIKE   LFA1-NAME1,            "Vendor name
        LIFNR     LIKE   LFA1-LIFNR,            "Vendor
        MAKTX     LIKE   MAKT-MAKTX,            "Description
        MATNR     LIKE   MARA-MATNR,            "Material
        DATAB     LIKE   ZTMM_ASSY_COST1-DATAB, "Valid on
        DATBI     LIKE   ZTMM_ASSY_COST1-DATBI, "Valid to
        MOAMT     LIKE   MSEG-DMBTR,            "Module Price
        ASYTR     LIKE   MSEG-DMBTR,            "Module Assy Cost
        DMBTR     LIKE   MSEG-DMBTR,            "Material Cost
        DMAMT     TYPE   F,                     "Material Cost(Floating)
        WAERS     LIKE   T001-WAERS,            "Currency
        EKGRP     LIKE   EKKO-EKGRP,            "Purchasing Group
        NETPR     LIKE   EKPO-NETPR,            "Component Amount
        PEINH     LIKE   EKPO-PEINH,            "Component Price Unit
        MEINS     LIKE   EKPO-MEINS,            "UoM
        MCODE     LIKE   ZTMM_ASSY_COST1-MCODE, "Module code
        KZUST     LIKE   KONH-KZUST.            "Reason code
        INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION.
DATA:   ZTIR     LIKE   EKPO-NETPR,
        STS       TYPE   I,                     "Module Info Status
        MSG(100),
        CHBOX,
      END   OF IT_MODULE.

DATA : BEGIN OF IT_TAB OCCURS 0,
       VTYPE  LIKE   ZTMM_ASSY_COST1-VTYPE,
       LIFNR  LIKE   LFA1-LIFNR,
       MATNR  LIKE   MARA-MATNR,
       ASYTR  LIKE MSEG-DMBTR,
       DATAB LIKE   ZTMM_ASSY_COST1-DATAB,
       DATBI LIKE   ZTMM_ASSY_COST1-DATBI,
       MAKTX  LIKE   MAKT-MAKTX,
       NAME1 LIKE   LFA1-NAME1,
       UPGVC LIKE MARA-MATNR,
       PREF LIKE STPO-POSNR, "ztbm_abxduldt-pref,
       COMP  LIKE MARA-MATNR,
       MAKTX1 LIKE  MAKT-MAKTX ,
       QNTY LIKE STPO-MENGE,
       UNIT LIKE   MARA-MEINS,
       MEINS LIKE   MARA-MEINS,
       EKGRP LIKE   EKKO-EKGRP,
       MCODE LIKE ZTMM_ASSY_COST1-MCODE,
       DATAB1 LIKE   ZTMM_ASSY_COST1-DATAB ,
       DATBI1 LIKE   ZTMM_ASSY_COST1-DATBI ,
       STGB LIKE STPO-STGB,
      END OF IT_TAB.

DATA : BEGIN OF IT_COMP OCCURS 0,
        MATNR LIKE MARA-MATNR,
        UPGVC LIKE MARA-MATNR,
       END OF IT_COMP .

*data : begin of it_scomp occurs 0.
*        include STRUCTURE ZTBM_ABXDULDT.
*data:   maktx     LIKE   makt-maktx,
*       end of it_scomp.

DATA: BEGIN OF IT_SUB OCCURS 0,
        VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE,   "Vehicle type
        MATNR     LIKE   ZTMM_ASSY_COST1-VTYPE,   "Material
        UPGVC     LIKE   MARA-MATNR,              "UPG-VC
        PREF      LIKE   STPO-POSNR,
        COMP      LIKE   MARA-MATNR,
        MAKTX     LIKE   MAKT-MAKTX,              "Description
        LIFNR     LIKE   LFA1-LIFNR,              "Vendor
        AMOUNT    TYPE   F,                       "Component Amount
        QNTY      LIKE   STPO-MENGE,
        STGB      LIKE   STPO-STGB,
        UNIT      LIKE   MARA-MEINS,
        MEINS     LIKE   MARA-MEINS,              "UoM(sub)
        KMEIN     LIKE   KONP-KMEIN,              "UoM(Info)
        DATAB     LIKE   SY-DATUM,                "Valid on(sub)
        DATBI     LIKE   SY-DATUM,                "Valid to(Sub)
        NETPR     LIKE   EKPO-NETPR,              "Component Amount
        PEINH     LIKE   EKPO-PEINH,              "Component Price Unit
        WAERS     LIKE   T001-WAERS,              "Currency
        KZUST     LIKE   KONH-KZUST.              "Reason code
        INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION.
DATA:   ZTIR      LIKE   EKPO-NETPR,
        STS,                                      "Status
        MSG(100),                                  "Message
      END   OF IT_SUB.

DATA: BEGIN OF IT_COMPARE OCCURS 0,
        UPGVC     LIKE   MARA-MATNR,              "UPG-VC
        PREF      LIKE   STPO-POSNR,    "ztbm_abxduldt-pref,
        COMP      LIKE   MARA-MATNR,
        LIFNR     LIKE   LFA1-LIFNR,              "Vendor
        MSG(100),                                 "Message
        QNTY_S    LIKE   STPO-MENGE,
        UNIT_S    LIKE   MARA-MEINS,
        DATAB_S   LIKE   ZTMM_ASSY_COST1-DATAB,   "Valid on(sub)
        DATBI_S   LIKE   ZTMM_ASSY_COST1-DATBI,   "Valid to(Sub)
        NETPR_S   LIKE   EKPO-NETPR,              "Component Amount
        PEINH_S   LIKE   EKPO-PEINH,              "Component Price Unit
        KZUST_S   LIKE   KONH-KZUST,              "Reason code
        NETWR_S   LIKE   EKPO-NETWR,
        QNTY_T    LIKE   STPO-MENGE,
        UNIT_T    LIKE   MARA-MEINS,
        DATAB_T   LIKE   ZTMM_ASSY_COST1-DATAB,   "Valid on(sub)
        DATBI_T   LIKE   ZTMM_ASSY_COST1-DATBI,   "Valid to(Sub)
        NETPR_T   LIKE   EKPO-NETPR,              "Component Amount
        PEINH_T   LIKE   EKPO-PEINH,              "Component Price Unit
        KZUST_T   LIKE   KONH-KZUST,              "Reason code
        NETWR_T   LIKE   EKPO-NETWR,
        WAERS     LIKE   T001-WAERS,              "Currency
      END   OF IT_COMPARE.


DATA : BEGIN OF IT_OUTPUT OCCURS 0,
            VTYPE LIKE ZTMM_ASSY_COST1-VTYPE,
            LIFNR LIKE LFA1-LIFNR,
            MATNR LIKE MARA-MATNR,
            ASYTR LIKE MSEG-DMBTR,
            DATAB LIKE ZTMM_ASSY_COST1-DATAB,
            DATBI LIKE ZTMM_ASSY_COST1-DATBI,
            MAKTX LIKE MAKT-MAKTX,
            NAME1 LIKE LFA1-NAME1,
            UPGVC LIKE MARA-MATNR,
            PREF  LIKE STPO-POSNR,  "ZTBM_ABXDULDT-pref
            COMP  LIKE MARA-MATNR,
            CMAKTX LIKE MAKT-MAKTX,
            QNTY LIKE STPO-MENGE,
            UNIT LIKE MARA-MEINS,
            MEINS     LIKE   MARA-MEINS,
            EKGRP LIKE ZTMM_ASSY_COST1-EKGRP,
            MCODE LIKE ZTMM_ASSY_COST1-MCODE,
            CDATAB LIKE ZTMM_ASSY_COST1-DATAB,
            CDATBI LIKE ZTMM_ASSY_COST1-DATBI,
            STGB   LIKE STPO-STGB,     "******
        END OF IT_OUTPUT.

*DATA: IT_DETAIL LIKE ZSMM_SUB_DETAIL OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF IT_DETAIL OCCURS 0,
       UPGVC LIKE STPO-UPGN,
       SPOSN LIKE STPO-POSNR,
       IDNRK LIKE STPO-IDNRK,
       MAKTX LIKE MAKT-MAKTX,
       KMPMG LIKE STPO-MENGE,
       KMPME LIKE STPO-MEINS,
       DATAB LIKE STPO-DATUV,
       DATBI LIKE STPO-DATUV,
       NETPR LIKE STPO-PREIS,
       PEINH LIKE STPO-PEINH,
       ZP12 LIKE ZSMM_CUSTOM_CONDITION-ZP12,
       ZP13 LIKE ZSMM_CUSTOM_CONDITION-ZP13,
       KZUST LIKE KONH-KZUST,
       DMBTR LIKE MSEG-DMBTR,
       WAERS LIKE STPO-WAERS,
       LIFNR LIKE STPO-LIFNR,
       MSG(100),
       END OF IT_DETAIL.

DATA: IT_CONDITION LIKE ZTMM_ASSY_COST2 OCCURS 0 WITH HEADER LINE.

DATA: IT_ASSY LIKE ZSMM_ASSY_COST OCCURS 0 WITH HEADER LINE.

*----- Global variables & structures
DATA: WA_MODULE LIKE IT_MODULE.
DATA: WA_SUB LIKE IT_SUB.

DATA: BEGIN OF WA_COMPARE,
        VTYPE_S     LIKE   ZTMM_ASSY_COST1-VTYPE, "Vehicle type
        NAME1_S     LIKE   LFA1-NAME1,            "Vendor name
        LIFNR_S     LIKE   LFA1-LIFNR,            "Vendor
        MAKTX_S     LIKE   MAKT-MAKTX,            "Description
        MATNR_S     LIKE   MARA-MATNR,            "Material
        DATAB_S     LIKE   ZTMM_ASSY_COST1-DATAB, "Valid on
        DATBI_S     LIKE   ZTMM_ASSY_COST1-DATBI, "Valid to
        MOAMT_S     LIKE   MSEG-DMBTR,            "Module Price
        ASYTR_S     LIKE   ZTMM_ASSY_COST1-ASYTR, "Module Assy Cost
        DMBTR_S     LIKE   MSEG-DMBTR,            "Material Cost
        INDICATOR_S,                              "Sub Material Status
        VTYPE_T     LIKE   ZTMM_ASSY_COST1-VTYPE, "Vehicle type
        NAME1_T     LIKE   LFA1-NAME1,            "Vendor name
        LIFNR_T     LIKE   LFA1-LIFNR,            "Vendor
        MAKTX_T     LIKE   MAKT-MAKTX,            "Description
        MATNR_T     LIKE   MARA-MATNR,            "Material
        DATAB_T     LIKE   ZTMM_ASSY_COST1-DATAB, "Valid on
        DATBI_T     LIKE   ZTMM_ASSY_COST1-DATBI, "Valid to
        MOAMT_T     LIKE   MSEG-DMBTR,            "Module Price
        ASYTR_T     LIKE   ZTMM_ASSY_COST1-ASYTR, "Module Assy Cost
        DMBTR_T     LIKE   MSEG-DMBTR,            "Material Cost
        INDICATOR_T,                              "Sub Material Status
        WAERS       LIKE   T001-WAERS,            "Currency
      END   OF WA_COMPARE.

DATA: WA_VTYPE_F   LIKE   ZTMM_ASSY_COST1-VTYPE,
      WA_VTYPE_T   LIKE   ZTMM_ASSY_COST1-VTYPE,
      WA_MCODE_F   LIKE   ZTMM_ASSY_COST1-MCODE,
      WA_MCODE_T   LIKE   ZTMM_ASSY_COST1-MCODE,
      WA_LIFNR_F   LIKE   ZTMM_ASSY_COST1-LIFNR,
      WA_LIFNR_T   LIKE   ZTMM_ASSY_COST1-LIFNR,
      WA_MATNR_F   LIKE   MARA-MATNR,
      WA_MATNR_T   LIKE   MARA-MATNR,
      WA_EKGRP_F   LIKE   T024-EKGRP,
      WA_EKGRP_T   LIKE   T024-EKGRP,
      WA_TOT_PAGE  TYPE   I,
      WA_FORMAT_FLG,
      W_MODULE(50),
      W_SUB(50),
      W_INDEX(2)   TYPE   N,
      L_MCODE(4) TYPE C.

DATA :  L_DYNAME LIKE SY-REPID.

DATA : T_RETURN TYPE STANDARD TABLE OF DDSHRETVAL WITH HEADER LINE.

FIELD-SYMBOLS: <MODULE>, <SUB>.

*----- Constants
" Module BOM is same all of plants
DATA: C_WERKS   LIKE   T001W-WERKS   VALUE   'P001',
      C_EKORG   LIKE   EKKO-EKORG    VALUE   'PU01',
      C_KSCHL   LIKE   KONP-KSCHL    VALUE   'PB00',
      C_ZTIR    LIKE   KONP-KSCHL    VALUE   'ZTIR',
      C_BUKRS   LIKE   T001-BUKRS    VALUE   'H201',
      C_READY   TYPE   I             VALUE   1,
      C_WARNING TYPE   I             VALUE   2,
      C_SUCCESS TYPE   I             VALUE   3,
      C_ERROR   TYPE   I             VALUE   4,
      C_INCORRECT TYPE I             VALUE   5,
      C_NEW     TYPE   I             VALUE   1,"Module Status
      C_DELETED TYPE   I             VALUE   2,"Module Status
      C_NO_COND TYPE   I             VALUE   3,"Condition does not exist
      C_DEL_CON TYPE   I             VALUE   4,"Condition was deleted
      C_NO_INFO TYPE   I             VALUE   5,"InfoRecord dosn't exist
      C_UOM_ERR TYPE   I             VALUE   6,"Incorrect UoM
      C_NO_MATL TYPE   I             VALUE   7,"M/M does not exist.
      C_EXIST   TYPE   I             VALUE   8,"Info Record is exist.
      C_SUB_ERR TYPE   I             VALUE   9,
      C_DIFF    TYPE   I             VALUE  10.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_VTYPE FOR ZTMM_ASSY_COST1-VTYPE
                NO-EXTENSION NO INTERVALS.
*
*                S_MCODE FOR ZTMM_ASSY_COST1-MCODE
*                            NO-EXTENSION NO INTERVALS,
*                S_LIFNR FOR LFA1-LIFNR
*                            NO-EXTENSION NO INTERVALS,
*                S_MATNR FOR MARA-MATNR NO-EXTENSION NO INTERVALS,
*                S_EKGRP FOR ZTMM_ASSY_COST1-EKGRP
*                            NO-EXTENSION NO INTERVALS.
PARAMETERS:     P_MCODE LIKE ZTMM_ASSY_COST1-MCODE OBLIGATORY,
                P_LIFNR LIKE LFA1-LIFNR,
                P_MATNR LIKE MARA-MATNR,
                P_EKGRP LIKE ZTMM_ASSY_COST1-EKGRP.

PARAMETERS:     P_DATUM LIKE SY-DATUM OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK BL1.

*----- Initialization
INITIALIZATION.
  PERFORM INITIALIZATION_RTN.

*----- Input value check & read data
AT SELECTION-SCREEN.
  CHECK SY-UCOMM EQ 'ONLI'.
  PERFORM CHECK_INPUT_VALUE.
  PERFORM READ_DATA.

* F4 Values for MCODE
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_MCODE-LOW.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  P_MCODE.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            MCODE  LIKE ZTMM_ASSY_COST1-MCODE,
           END OF VALUE_TAB.
* Select
  SELECT DISTINCT MCODE  FROM ZTMM_ASSY_COST1
             INTO TABLE VALUE_TAB.
  L_DYNAME = SY-REPID.

* Set F4 values for Module Code
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'P_MCODE'
            DYNPPROG        = L_DYNAME
            DYNPNR          = '1000'
            DYNPROFIELD     = 'MCODE'
            WINDOW_TITLE    = 'Module Codes'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

* F4 Values for Vechicle type
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_VTYPE-LOW.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            VTYPE  LIKE ZTMM_ASSY_COST1-VTYPE,
           END OF VALUE_TAB.
* Select
  SELECT DISTINCT VTYPE  FROM ZTMM_ASSY_COST1
             INTO TABLE VALUE_TAB.
  L_DYNAME = SY-REPID.


* Set F4 values for Vehicle type
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'S_VTYPE'
            DYNPPROG        = L_DYNAME
            DYNPNR          = '1000'
            DYNPROFIELD     = 'VTYPE'
            WINDOW_TITLE    = 'Vehicle Type'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

* F4 Values for Purchasing group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  P_EKGRP.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            EKGRP LIKE T024-EKGRP,
            EKNAM LIKE T024-EKNAM,
           END OF VALUE_TAB.
* Select
  SELECT  EKGRP EKNAM  FROM T024
             INTO TABLE VALUE_TAB.
  L_DYNAME = SY-REPID.

  T_RETURN-RETFIELD = 'EKGRP'.
  T_RETURN-RECORDPOS = '1'.
  T_RETURN-FIELDNAME = 'S_EKGRP'.
  APPEND T_RETURN.

* Set  Purchasing Group
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'EKGRP'
            DYNPPROG        = L_DYNAME
            DYNPNR          = '1000'
            DYNPROFIELD     = 'P_EKGRP'
            WINDOW_TITLE    = 'Purchasing Group'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
*              RETURN_TAB      = T_RETURN
       EXCEPTIONS
            PARAMETER_ERROR = 1.



*----- Display Header
TOP-OF-PAGE.
  PERFORM DISPLAY_BASE_HEADER.

TOP-OF-PAGE DURING LINE-SELECTION.
  CASE SY-PFKEY.
    WHEN 'BASE'.
      PERFORM DISPLAY_BASE_HEADER.
    WHEN 'DETAIL'.
      PERFORM DISPLAY_DETAIL_HEADER.
    WHEN 'COMPARE'.
      PERFORM DISPLAY_DETAIL_COMPARE.
  ENDCASE.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  SET TITLEBAR  'BASE'.
  SORT IT_MODULE BY INDICATOR LIFNR MATNR.
  PERFORM DISPLAY_DATA.

*----- Double click
AT LINE-SELECTION.
  PERFORM DOUBLE_CLICK_RTN.

*----- Function Key
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'COMPARE'.
      PERFORM COMPARE_RTN.
    WHEN 'DETAIL'.
      PERFORM DETAIL_RTN.
    WHEN 'HISTORY'.
      PERFORM HISTORY_RTN.
    WHEN 'MSG'.
      MESSAGE S000(ZZ) WITH IT_MODULE-MSG(50) IT_MODULE-MSG+50(50).
      CLEAR: IT_MODULE.
    WHEN 'SUB_INFO'.
      PERFORM DISPLAY_SUB_INFO_RECORD.
    WHEN 'SUB_MSG'.
      MESSAGE S000(ZZ) WITH IT_DETAIL-MSG(50) IT_DETAIL-MSG+50(50).
      CLEAR: IT_DETAIL.
    WHEN 'ASCENDING'.
      PERFORM ASCENDING_RTN.
    WHEN 'DESCENDING'.
      PERFORM DESCENDING_RTN.
    WHEN 'EXCEL'.
      PERFORM EXCEL_DOWNLOAD_RTN.
    WHEN 'PAGE'.
      PERFORM DISPLAY_TOTAL_PAGE.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  initialization_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION_RTN.
  P_DATUM = SY-DATUM - 1.
ENDFORM.                    " initialization_rtn
*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE.
  PERFORM CHECK_COMPANY.
  PERFORM CHECK_VTYPE.
  PERFORM CHECK_MCODE.
  PERFORM CHECK_LIFNR.
  PERFORM CHECK_MATNR.
  PERFORM CHECK_EKGRP.
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_company
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_COMPANY.
  SELECT SINGLE * FROM T001 WHERE BUKRS = C_BUKRS.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M06.
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
FORM CHECK_VTYPE.
  LOOP AT S_VTYPE.
    TRANSLATE S_VTYPE-LOW  TO UPPER CASE.
    TRANSLATE S_VTYPE-HIGH TO UPPER CASE.
    MODIFY S_VTYPE.
  ENDLOOP.

  IF S_VTYPE-LOW EQ ' '.
    WA_VTYPE_T = 'ZZ'.
  ELSE.
    WA_VTYPE_F = WA_VTYPE_T = S_VTYPE-LOW.
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
FORM CHECK_MCODE.
*  IF S_MCODE EQ ' '.
*    WA_MCODE_T = 'ZZ'.
*    MOVE '%' TO L_MCODE.
*  ELSE.
  WA_MCODE_F = WA_MCODE_T = P_MCODE.
  CONCATENATE '%' P_MCODE '%' INTO L_MCODE.
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
FORM CHECK_LIFNR.
  IF P_LIFNR EQ ' '.
    WA_LIFNR_T = 'ZZZZZZZZZZ'.
  ELSE.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR = P_LIFNR.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M03.
    ENDIF.
    WA_LIFNR_F = WA_LIFNR_T = P_LIFNR.
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
FORM CHECK_MATNR.

  IF P_MATNR EQ ' '.
    WA_MATNR_T = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSE.
    SELECT SINGLE * FROM MARA WHERE MATNR = P_MATNR.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M04.
    ENDIF.
    WA_MATNR_T = WA_MATNR_F = P_MATNR.
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
FORM CHECK_EKGRP.
  IF P_EKGRP EQ ' '.
    WA_EKGRP_T = 'ZZZ'.
  ELSE.
    SELECT SINGLE * FROM T024 WHERE EKGRP = P_EKGRP.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M02.
    ENDIF.
    WA_EKGRP_T = WA_EKGRP_F = P_EKGRP.
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
FORM READ_DATA.
  PERFORM GET_SUB_PRICE.
  PERFORM CALCULATE_MODULE_PRICE.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  get_sub_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SUB_PRICE.
  DATA: C_CAPID   LIKE RC29L-CAPID VALUE 'PP01',
        L_MATNR LIKE MARA-MATNR,
        L_NAME1 LIKE LFA1-NAME1,
        LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE.
  DATA: LT_COST1 LIKE TABLE OF ZTMM_ASSY_COST1 WITH HEADER LINE,
        LT_TAB LIKE TABLE OF IT_TAB WITH HEADER LINE.

  DATA: LT_ZBOM LIKE TABLE OF ZTBM_EBOM_ECM WITH HEADER LINE,
        L_LINE TYPE I,
        L_COMP LIKE ZTBM_EBOM_ECM-COMP,
        L_UPGN  LIKE ZTBM_EBOM_ECM-UPGN,
        L_FLAG(1).

*  DATA: LT_ZBDAT LIKE TABLE OF ZTBM_EBOM_ECM WITH HEADER LINE,
*        LT_DATT  LIKE TABLE OF ZTBM_EBOM_ECM WITH HEADER LINE.
*
*  DATA: BEGIN OF LT_PARTS OCCURS 0,
*         UPGN  LIKE ZTBM_EBOM_ECM-UPGN,
*         COMP LIKE ZTBM_EBOM_ECM-COMP,
*        END OF LT_PARTS.

  SELECT * INTO TABLE LT_COST1
    FROM ZTMM_ASSY_COST1
    WHERE  VTYPE BETWEEN WA_VTYPE_F AND WA_VTYPE_T
       AND MCODE = P_MCODE
       AND LIFNR    BETWEEN  WA_LIFNR_F AND WA_LIFNR_T
       AND EKGRP    BETWEEN  WA_EKGRP_F AND WA_EKGRP_T
       AND DATAB <= P_DATUM
       AND DATBI >= P_DATUM .
  LOOP AT LT_COST1.
    IF P_MATNR IS INITIAL.
** Changed on 01/09/08 only get the module no. including 'M1'
** for limit only new modules are selecte.
*    CONCATENATE '%' LT_COST1-VTYPE P_MCODE '%' INTO L_MATNR.
      CONCATENATE '%' LT_COST1-VTYPE P_MCODE '%M1%' INTO L_MATNR.
** End of change
      SELECT G~MATNR AS MATNR
             D~MAKTX
             M~MEINS AS UNIT M~MEINS AS MEINS
             INTO CORRESPONDING FIELDS OF TABLE LT_TAB
                FROM MAST AS G
                INNER JOIN MARA AS M ON
                      G~MATNR = M~MATNR
                INNER JOIN MAKT AS D ON
                       M~MATNR = D~MATNR
                   WHERE G~WERKS     =       C_WERKS
                      AND G~STLAN     =       '2'
                      AND G~STLAL     =       '01'
*                  AND G~DATUV    <       P_DATUM
*                  AND G~DATUB    >=      P_DATUM
                      AND G~MATNR LIKE L_MATNR
                      AND M~LVORM = ''
                      AND D~SPRAS = SY-LANGU.
    ELSE.
      SELECT G~MATNR AS MATNR
              D~MAKTX
              M~MEINS AS UNIT M~MEINS AS MEINS
              INTO CORRESPONDING FIELDS OF TABLE LT_TAB
                 FROM MAST AS G
                 INNER JOIN MARA AS M ON
                       G~MATNR = M~MATNR
                 INNER JOIN MAKT AS D ON
                        M~MATNR = D~MATNR
                    WHERE G~WERKS     =       C_WERKS
                       AND G~STLAN     =       '2'
                       AND G~STLAL     =       '01'
*                  AND G~DATUV    <       P_DATUM
*                  AND G~DATUB    >=      P_DATUM
                       AND G~MATNR = P_MATNR
                       AND M~LVORM = ''
                       AND D~SPRAS = SY-LANGU.
      LOOP AT LT_TAB.

        IF ( LT_COST1-VTYPE = LT_TAB-MATNR+0(3) )
           AND ( P_MCODE = LT_TAB-MATNR+3(2) ).
        ELSE.
          DELETE LT_TAB.
        ENDIF.
      ENDLOOP.

    ENDIF.
    SELECT SINGLE NAME1 INTO L_NAME1
      FROM LFA1
      WHERE LIFNR = LT_COST1-LIFNR.
    LOOP AT LT_TAB.
      IT_TAB = LT_TAB.
      IT_TAB-NAME1 = L_NAME1.
      IT_TAB-LIFNR = LT_COST1-LIFNR.
      IT_TAB-VTYPE = LT_COST1-VTYPE.
      IT_TAB-EKGRP = LT_COST1-EKGRP.
      IT_TAB-ASYTR = LT_COST1-ASYTR.
      IT_TAB-DATAB = LT_COST1-DATAB.
      IT_TAB-DATBI = LT_COST1-DATBI.
      APPEND IT_TAB.
    ENDLOOP.
    REFRESH LT_TAB.
  ENDLOOP.

  LOOP AT IT_TAB.
** changed on 03/013/09 by Furong

    MOVE-CORRESPONDING IT_TAB TO IT_OUTPUT.

    SELECT * INTO TABLE LT_ZBOM
     FROM ZTBM_EBOM_ECM_BK
     WHERE MTNO = IT_TAB-MATNR
       AND ZBDAT <= P_DATUM.

    SELECT * APPENDING TABLE LT_ZBOM
      FROM ZTBM_EBOM_ECM
      WHERE MTNO = IT_TAB-MATNR
       AND ZBDAT <= P_DATUM.

    DESCRIBE TABLE LT_ZBOM LINES L_LINE.

    IF L_LINE > 0.
      SORT LT_ZBOM BY UPGN COMP ZBDAT DESCENDING.
      L_COMP = '**'.
      L_UPGN = '**'.
      LOOP AT LT_ZBOM.
        IF L_COMP <> LT_ZBOM-COMP OR L_UPGN <> LT_ZBOM-UPGN.
          IF LT_ZBOM-ZMODE = 'D'.
            L_COMP = LT_ZBOM-COMP.
            L_UPGN = LT_ZBOM-UPGN.
            L_FLAG = 'X'.
            CONTINUE.
          ENDIF.
          IF LT_ZBOM-ZMODE = 'U'.
            L_COMP = LT_ZBOM-COMP.
            L_UPGN = LT_ZBOM-UPGN.
            IF  LT_ZBOM-DATT >= P_DATUM AND LT_ZBOM-DATF <= P_DATUM.
              L_FLAG = 'X'.
              IT_OUTPUT-COMP = LT_ZBOM-COMP.
              IT_OUTPUT-QNTY = LT_ZBOM-QNTY.
              IT_OUTPUT-DATAB = LT_ZBOM-DATF.
              IT_OUTPUT-DATBI = LT_ZBOM-DATT.
              IT_OUTPUT-CDATAB = LT_ZBOM-DATF.
              IT_OUTPUT-CDATBI = LT_ZBOM-DATT.
              IT_OUTPUT-UPGVC = LT_ZBOM-UPGN.
              SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
                FROM MAKT
                WHERE MATNR = LT_ZBOM-COMP.
              APPEND IT_OUTPUT.
            ENDIF.
          ELSE.
            CLEAR: L_FLAG.
          ENDIF.
        ELSE.
          IF L_FLAG IS INITIAL AND LT_ZBOM-ZMODE = 'U' AND
             LT_ZBOM-DATT >= P_DATUM AND LT_ZBOM-DATF <= P_DATUM.
            L_COMP = LT_ZBOM-COMP.
            L_UPGN = LT_ZBOM-UPGN.
            L_FLAG = 'X'.
            IT_OUTPUT-COMP = LT_ZBOM-COMP.
            IT_OUTPUT-QNTY = LT_ZBOM-QNTY.
            IT_OUTPUT-DATAB = LT_ZBOM-DATF.
            IT_OUTPUT-DATBI = LT_ZBOM-DATT.
            IT_OUTPUT-CDATAB = LT_ZBOM-DATF.
            IT_OUTPUT-CDATBI = LT_ZBOM-DATT.
            IT_OUTPUT-UPGVC = LT_ZBOM-UPGN.
            SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
              FROM MAKT
              WHERE MATNR = LT_ZBOM-COMP.
            APPEND IT_OUTPUT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      APPEND IT_OUTPUT.
    ENDIF.

*    REFRESH: LT_ZBDAT, LT_DATT, LT_PARTS.
*
*    MOVE-CORRESPONDING IT_TAB TO IT_OUTPUT.
*
*    SELECT * INTO TABLE LT_ZBOM
*      FROM ZTBM_EBOM_ECM_BK
*      WHERE MTNO = IT_TAB-MATNR.
*
*    SELECT * APPENDING TABLE LT_ZBOM
*      FROM ZTBM_EBOM_ECM
*      WHERE MTNO = IT_TAB-MATNR.
*
*    DESCRIBE TABLE LT_ZBOM LINES L_LINE.
*
*    IF L_LINE > 0.
*      LOOP AT LT_ZBOM.
*        IF LT_ZBOM-ZBDAT >= P_DATUM.
*          LT_ZBDAT = LT_ZBOM.
*        ELSE.
*          LT_DATT = LT_ZBOM.
*        ENDIF.
*        LT_PARTS-UPGN = LT_ZBOM-UPGN.
*        LT_PARTS-COMP = LT_ZBOM-COMP.
*        COLLECT LT_PARTS.
*        CLEAR: LT_ZBOM, LT_ZBDAT, LT_DATT.
*      ENDLOOP.
*
*      SORT LT_ZBDAT BY UPGN COMP ZBDAT.
*      SORT LT_DATT BY UPGN COMP ZBDAT DESCENDING.
*
*      LOOP AT LT_PARTS.
*        READ TABLE LT_ZBDAT WITH KEY UPGN = LT_PARTS-UPGN
*                                     COMP = LT_PARTS-COMP.
*        IF SY-SUBRC = 0.
*          IF LT_ZBDAT-ZMODE = 'U'.
*            IT_OUTPUT-COMP = LT_ZBDAT-COMP.
*            IT_OUTPUT-QNTY = LT_ZBDAT-QNTY.
*            IT_OUTPUT-DATAB = LT_ZBDAT-DATF.
*            IT_OUTPUT-DATBI = LT_ZBDAT-DATT.
*            IT_OUTPUT-CDATAB = LT_ZBDAT-DATF.
*            IT_OUTPUT-CDATBI = LT_ZBDAT-DATT.
*            IT_OUTPUT-UPGVC = LT_ZBDAT-UPGN.
*            SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
*               FROM MAKT
*              WHERE MATNR = LT_ZBDAT-COMP.
*            APPEND IT_OUTPUT.
*          ENDIF.
*        ELSE.
*          LOOP AT LT_DATT WHERE UPGN = LT_PARTS-UPGN
*                            AND COMP = LT_PARTS-COMP.
*            IF LT_ZBDAT-ZMODE = 'U'.
*              IT_OUTPUT-COMP = LT_ZBDAT-COMP.
*              IT_OUTPUT-QNTY = LT_ZBDAT-QNTY.
*              IT_OUTPUT-DATAB = LT_ZBDAT-DATF.
*              IT_OUTPUT-DATBI = LT_ZBDAT-DATT.
*              IT_OUTPUT-CDATAB = LT_ZBDAT-DATF.
*              IT_OUTPUT-CDATBI = LT_ZBDAT-DATT.
*              IT_OUTPUT-UPGVC = LT_ZBDAT-UPGN.
*              SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
*                 FROM MAKT
*                WHERE MATNR = LT_ZBDAT-COMP.
*              APPEND IT_OUTPUT.
*              EXIT.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDLOOP.
*    ELSE.
*      APPEND IT_OUTPUT.
*    ENDIF.


*     SELECT * INTO TABLE LT_ZBOM
*      FROM ZTBM_EBOM_ECM_BK
*      WHERE MTNO = IT_TAB-MATNR
**        AND ZMODE = 'U'
*        AND DATT >= P_DATUM
*        AND DATF <= P_DATUM.
*
*    SELECT * APPENDING TABLE LT_ZBOM
*      FROM ZTBM_EBOM_ECM
*      WHERE MTNO = IT_TAB-MATNR
**        AND ZMODE = 'U'
*        AND DATT >= P_DATUM
*        AND DATF <= P_DATUM.
*
*    DESCRIBE TABLE LT_ZBOM LINES L_LINE.
*
*    IF L_LINE > 0.
*      SORT LT_ZBOM BY UPGN COMP ZBDAT DESCENDING.
*      L_COMP = '**'.
*      L_UPGN = '**'.
*      LOOP AT LT_ZBOM.
*        IF L_COMP <> LT_ZBOM-COMP OR L_UPGN <> LT_ZBOM-UPGN.
*          IF LT_ZBOM-ZMODE = 'D'.
*            L_COMP = LT_ZBOM-COMP.
*            L_UPGN = LT_ZBOM-UPGN.
*            L_FLAG = 'X'.
*            CONTINUE.
*          ENDIF.
*          IF LT_ZBOM-ZMODE = 'U'.
*            L_COMP = LT_ZBOM-COMP.
*            L_UPGN = LT_ZBOM-UPGN.
*            L_FLAG = 'X'.
*            IT_OUTPUT-COMP = LT_ZBOM-COMP.
*            IT_OUTPUT-QNTY = LT_ZBOM-QNTY.
*            IT_OUTPUT-DATAB = LT_ZBOM-DATF.
*            IT_OUTPUT-DATBI = LT_ZBOM-DATT.
*            IT_OUTPUT-CDATAB = LT_ZBOM-DATF.
*            IT_OUTPUT-CDATBI = LT_ZBOM-DATT.
*            IT_OUTPUT-UPGVC = LT_ZBOM-UPGN.
*            SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
*              FROM MAKT
*              WHERE MATNR = LT_ZBOM-COMP.
*            APPEND IT_OUTPUT.
*          ELSE.
*            CLEAR: L_FLAG.
*          ENDIF.
*        ELSE.
*          IF L_FLAG IS INITIAL AND LT_ZBOM-ZMODE = 'U'.
*            L_COMP = LT_ZBOM-COMP.
*            L_UPGN = LT_ZBOM-UPGN.
*            L_FLAG = 'X'.
*            IT_OUTPUT-COMP = LT_ZBOM-COMP.
*            IT_OUTPUT-QNTY = LT_ZBOM-QNTY.
*            IT_OUTPUT-DATAB = LT_ZBOM-DATF.
*            IT_OUTPUT-DATBI = LT_ZBOM-DATT.
*            IT_OUTPUT-CDATAB = LT_ZBOM-DATF.
*            IT_OUTPUT-CDATBI = LT_ZBOM-DATT.
*            IT_OUTPUT-UPGVC = LT_ZBOM-UPGN.
*            SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
*              FROM MAKT
*              WHERE MATNR = LT_ZBOM-COMP.
*            APPEND IT_OUTPUT.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ELSE.
*      APPEND IT_OUTPUT.
*    ENDIF.

    CLEAR: LT_ZBOM[], IT_OUTPUT.

*
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
*
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    MOVE-CORRESPONDING IT_TAB TO IT_OUTPUT.
*    LOOP AT LT_STB WHERE DATUV <= P_DATUM AND DATUB > P_DATUM.
*      IT_OUTPUT-COMP = LT_STB-IDNRK.
*      IT_OUTPUT-QNTY = LT_STB-MENGE.
*      IT_OUTPUT-DATAB = LT_STB-DATUV.
*      IT_OUTPUT-DATBI = LT_STB-DATUB.
*      IT_OUTPUT-CDATAB = LT_STB-DATUV.
*      IT_OUTPUT-CDATBI = LT_STB-DATUB.
*      IT_OUTPUT-UPGVC = LT_STB-UPGN.
*      SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
*        FROM MAKT
*        WHERE MATNR = LT_STB-IDNRK.
*      APPEND IT_OUTPUT.
*    ENDLOOP.

** End of change on 03/013/09
  ENDLOOP.

  PERFORM APPEND_ITAB.
ENDFORM.                    " get_sub_price
*&---------------------------------------------------------------------*
*&      Form  APPEND_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_ITAB.
  DATA: LW_CONTINUE VALUE 'X'.

  LOOP AT IT_OUTPUT.
*  ON CHANGE OF wa_module-matnr OR wa_module-lifnr.
    ON CHANGE OF IT_OUTPUT-MATNR OR IT_OUTPUT-LIFNR.
      PERFORM READ_MODULE_INFO_RECORD.
    ENDON.
*    lw_continue = 'X'.
*    PERFORM check_cockpit_module_color USING lw_continue.
*
*    CHECK lw_continue EQ 'X'.

    PERFORM READ_SUB_INFO_RECORD.
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
FORM CALCULATE_MODULE_PRICE.
  DATA: LW_AMOUNT TYPE F,
        LW_MTOT LIKE IT_MODULE-MOAMT.

  DATA: LW_DETAIL LIKE IT_DETAIL.

  SORT IT_MODULE BY VTYPE MATNR LIFNR.
  SORT IT_SUB BY VTYPE MATNR UPGVC PREF.

  LOOP AT IT_MODULE.
    LOOP AT IT_SUB WHERE VTYPE = IT_MODULE-VTYPE
                     AND MATNR = IT_MODULE-MATNR.
      IT_MODULE-DMAMT = IT_MODULE-DMAMT + IT_SUB-AMOUNT.
      IT_MODULE-ZTIR = IT_MODULE-ZTIR + IT_SUB-ZTIR
                       / IT_SUB-PEINH * IT_SUB-QNTY.

      IF IT_SUB-STS    NE 0 AND
         IT_MODULE-STS EQ C_EXIST.
        IT_MODULE-STS = C_SUB_ERR.
      ENDIF.
    ENDLOOP.

    MOVE: IT_MODULE-DMAMT TO IT_MODULE-DMBTR.
    IT_MODULE-MOAMT = IT_MODULE-ASYTR + IT_MODULE-DMBTR.

    LW_MTOT = IT_MODULE-NETPR + IT_MODULE-ZTIR.
    IF   IT_MODULE-MOAMT NE  LW_MTOT AND
       ( IT_MODULE-STS   EQ C_EXIST OR
         IT_MODULE-STS   EQ C_SUB_ERR  ).
      MOVE: C_DIFF    TO IT_MODULE-STS,
            TEXT-B12  TO IT_MODULE-MSG.
    ENDIF.

    CASE IT_MODULE-STS.
      WHEN C_NEW OR C_NO_COND.
        MOVE: TEXT-B04 TO IT_MODULE-MSG.
        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
      WHEN C_DELETED.
        MOVE: TEXT-B05 TO IT_MODULE-MSG.
        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
      WHEN C_DEL_CON.
        MOVE: TEXT-B06 TO IT_MODULE-MSG.
        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
      WHEN C_NO_MATL.
        MOVE: TEXT-B07 TO IT_MODULE-MSG.
        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
      WHEN C_EXIST.
        MOVE: TEXT-B09 TO IT_MODULE-MSG.
        MOVE: C_SUCCESS   TO IT_MODULE-INDICATOR.
      WHEN C_SUB_ERR.
        MOVE: TEXT-B10  TO IT_MODULE-MSG.
        MOVE: C_WARNING TO IT_MODULE-INDICATOR.
      WHEN C_DIFF.
        MOVE: TEXT-B12  TO IT_MODULE-MSG.
        MOVE: C_ERROR  TO IT_MODULE-INDICATOR.
    ENDCASE.

    MODIFY IT_MODULE.
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
FORM READ_MODULE_INFO_RECORD.
  PERFORM CHECK_MODULE.
  PERFORM APPEND_IT_MODULE.
ENDFORM.                    " read_module_info_record
*&---------------------------------------------------------------------*
*&      Form  check_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_MODULE.
*----- Check Info Record Deletion
  DATA: BEGIN OF LT_CONDITION OCCURS 0,
          KBETR LIKE ZVMM_INFO_CONDI-KBETR,
          KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
          KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
          KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
          KZUST LIKE ZVMM_INFO_CONDI-KZUST,
        END   OF LT_CONDITION.
  DATA: LW_MTOT LIKE WA_MODULE-NETPR.

  CLEAR: EINA, A018.
  CLEAR: WA_MODULE-NETPR, WA_MODULE-PEINH, WA_MODULE-MEINS,
         WA_MODULE-MSG,  WA_MODULE-STS.

*----- Check Material Master
  IF IT_OUTPUT-MAKTX IS INITIAL.
    MOVE: C_NO_MATL TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  SELECT SINGLE MATNR A~LOEKZ WGLIF
    INTO (EINA-MATNR,EINA-LOEKZ,EINA-WGLIF)
    FROM EINA AS A INNER JOIN EINE AS B
      ON A~INFNR = B~INFNR
   WHERE A~MATNR = IT_OUTPUT-MATNR
     AND A~LIFNR = IT_OUTPUT-LIFNR
     AND A~LOEKZ = ' '
     AND B~WERKS = ' '
     AND B~EKORG = C_EKORG
     AND B~LOEKZ = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NEW    TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  IF EINA-LOEKZ EQ 'X'.
    MOVE: C_DELETED TO WA_MODULE-STS.
    EXIT.
  ENDIF.

*----- Read Module price
  SELECT SINGLE *
    FROM A018
   WHERE KAPPL =  'M'
     AND KSCHL =  'PB00'
     AND MATNR =  IT_OUTPUT-MATNR
     AND LIFNR =  IT_OUTPUT-LIFNR
     AND EKORG =  C_EKORG
     AND ESOKZ =  '0'
     AND DATAB <= P_DATUM
     AND DATBI >= P_DATUM.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_COND TO WA_MODULE-STS.
    EXIT.
  ELSE.
    MOVE: C_EXIST   TO WA_MODULE-STS.
  ENDIF.

  SELECT KBETR KPEIN KMEIN KZUST KSCHL
   INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
   FROM ZVMM_INFO_CONDI
  WHERE KNUMH = A018-KNUMH
    AND ( KSCHL = C_KSCHL   OR
          KSCHL = C_ZTIR    OR
          KSCHL LIKE 'ZP%' )
    AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_DEL_CON TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  SORT LT_CONDITION BY KSCHL.
  LOOP AT LT_CONDITION.
    CASE LT_CONDITION-KSCHL.
      WHEN C_KSCHL.
        MOVE: LT_CONDITION-KBETR TO WA_MODULE-NETPR,
              LT_CONDITION-KPEIN TO WA_MODULE-PEINH,
              LT_CONDITION-KMEIN TO WA_MODULE-MEINS,
              LT_CONDITION-KZUST TO WA_MODULE-KZUST.
      WHEN C_ZTIR.
        LW_MTOT = WA_MODULE-NETPR + LT_CONDITION-KBETR.
        MOVE: LW_MTOT TO WA_MODULE-NETPR.
      WHEN OTHERS.
        MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.

        CONCATENATE: 'WA_MODULE-ZP' W_INDEX INTO W_MODULE.

        ASSIGN: (W_MODULE) TO <MODULE>.
        IF SY-SUBRC NE 0. CONTINUE. ENDIF.

        <MODULE> = LT_CONDITION-KBETR.
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
FORM APPEND_IT_MODULE.
  CLEAR: IT_MODULE.

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

  IF IT_MODULE-PEINH EQ 0.
    IT_MODULE-PEINH = 1.
  ENDIF.

  MOVE: T001-WAERS TO WA_MODULE-WAERS.

  MOVE: WA_MODULE TO IT_MODULE.
  MOVE-CORRESPONDING  IT_OUTPUT TO IT_MODULE.

  APPEND IT_MODULE.
  CLEAR: IT_MODULE.
ENDFORM.                    " append_it_module
*&---------------------------------------------------------------------*
*&      Form  read_sub_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_SUB_INFO_RECORD.
  DATA: LW_SUB(50).
  FIELD-SYMBOLS: <LW_SUB>.
  CLEAR: IT_SUB.

  READ TABLE IT_SUB WITH KEY COMP = IT_OUTPUT-COMP.
  IF SY-SUBRC NE 0.
    MOVE-CORRESPONDING  IT_OUTPUT TO IT_SUB.
    PERFORM CHECK_RTN.
    PERFORM APPEND_SUB_PRICE.
  ELSE.
    MOVE: IT_SUB-LIFNR  TO WA_SUB-LIFNR,
          IT_SUB-AMOUNT TO WA_SUB-AMOUNT,
          IT_SUB-KMEIN  TO WA_SUB-KMEIN,
          IT_SUB-NETPR  TO WA_SUB-NETPR,
          IT_SUB-PEINH  TO WA_SUB-PEINH,
          IT_SUB-WAERS  TO WA_SUB-WAERS,
          IT_SUB-KZUST  TO WA_SUB-KZUST,
          IT_SUB-STS    TO WA_SUB-STS,
          IT_SUB-MSG    TO WA_SUB-MSG,
          IT_SUB-ZTIR   TO WA_SUB-ZTIR,
          IT_SUB-DATAB  TO WA_SUB-DATAB,
          IT_SUB-DATBI  TO WA_SUB-DATBI.


    DO.
      MOVE: SY-INDEX TO W_INDEX.

      CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO LW_SUB,
                   'IT_SUB-ZP' W_INDEX INTO W_SUB.

      ASSIGN: (W_SUB)  TO <SUB>,
              (LW_SUB) TO <LW_SUB>.
      IF SY-SUBRC NE 0. EXIT. ENDIF.

      MOVE: <SUB> TO <LW_SUB>.
    ENDDO.

    IF NOT ( IT_SUB-STS EQ C_NO_MATL OR
             IT_SUB-STS EQ C_NO_COND OR
             IT_SUB-STS EQ C_NO_INFO    ).
      IF NOT ( ( IT_SUB-MEINS EQ WA_SUB-KMEIN AND
                 IT_SUB-MEINS EQ IT_SUB-UNIT  AND
                 WA_SUB-KMEIN EQ IT_SUB-UNIT )   ).
        MOVE: C_UOM_ERR TO WA_SUB-STS.
      ENDIF.
    ENDIF.

    PERFORM APPEND_SUB_PRICE.
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
FORM CHECK_RTN.
*----- Check Material Master
  DATA: BEGIN OF LT_CONDITION OCCURS 0,
          KBETR LIKE ZVMM_INFO_CONDI-KBETR,
          KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
          KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
          KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
          KZUST LIKE ZVMM_INFO_CONDI-KZUST,
        END   OF LT_CONDITION.

  DATA: BEGIN OF IT_EINA_EINE_TEMP OCCURS 0,
        MATNR LIKE EINA-MATNR,
        LIFNR LIKE EINA-LIFNR,
        WGLIF LIKE EINA-WGLIF,
        END OF IT_EINA_EINE_TEMP.

  IF    IT_OUTPUT-CMAKTX IS INITIAL.
*  IF wa_sub-maktx IS INITIAL.
    MOVE: C_NO_MATL TO WA_SUB-STS.
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
  SELECT MATNR A~LIFNR WGLIF INTO TABLE IT_EINA_EINE_TEMP
   FROM EINA AS A INNER JOIN EINE AS B
     ON A~INFNR = B~INFNR
  WHERE A~MATNR = IT_OUTPUT-COMP
    AND A~URZZT = 'SUB'
    AND A~LOEKZ = ' '
    AND B~WERKS = ' '
    AND B~EKORG = C_EKORG
    AND B~LOEKZ = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_INFO TO WA_SUB-STS.
    EXIT.
  ENDIF.

*----- Read submaterial price
  LOOP AT IT_EINA_EINE_TEMP.
    CLEAR: EINA, A018.
    WA_SUB-LIFNR = IT_EINA_EINE_TEMP-LIFNR.
    SELECT SINGLE KNUMH DATAB DATBI
                    INTO (A018-KNUMH, WA_SUB-DATAB, WA_SUB-DATBI)
                    FROM A018
                    WHERE KAPPL =  'M'
                      AND KSCHL =  'PB00'
                      AND MATNR =  IT_OUTPUT-COMP
                      AND LIFNR =  WA_SUB-LIFNR
                      AND EKORG =  C_EKORG
                      AND ESOKZ =  '0'
                      AND DATAB <= P_DATUM
                      AND DATBI >= P_DATUM.
    IF SY-SUBRC EQ 0.
      EXIT.
    ENDIF.
  ENDLOOP.
*  if sy-subrc ne 0.
  IF A018-KNUMH IS INITIAL.
    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.
*** END OF INSERTION

  SELECT SINGLE KBETR KPEIN KMEIN KZUST
    INTO (WA_SUB-NETPR,WA_SUB-PEINH,
          WA_SUB-KMEIN,WA_SUB-KZUST)
    FROM ZVMM_INFO_CONDI
   WHERE KNUMH = A018-KNUMH
     AND KSCHL = C_KSCHL
     AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.

  SELECT KBETR KPEIN KMEIN KZUST KSCHL
    INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
    FROM ZVMM_INFO_CONDI
   WHERE KNUMH = A018-KNUMH
     AND ( KSCHL = C_KSCHL   OR
           KSCHL = 'ZTIR'    OR
           KSCHL LIKE 'ZP%' )
     AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_COND TO WA_SUB-STS.
    EXIT.
  ENDIF.

  SORT LT_CONDITION BY KSCHL.
  LOOP AT LT_CONDITION.
    CASE LT_CONDITION-KSCHL.
      WHEN C_KSCHL.
        MOVE: LT_CONDITION-KBETR TO WA_SUB-NETPR,
              LT_CONDITION-KPEIN TO WA_SUB-PEINH,
              LT_CONDITION-KMEIN TO WA_SUB-KMEIN,
              LT_CONDITION-KZUST TO WA_SUB-KZUST.
        IF IT_EINA_EINE_TEMP-WGLIF = 'ZTIR'.
          WA_SUB-ZTIR = LT_CONDITION-KBETR.
        ENDIF.

      WHEN OTHERS.
        MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.

        CONCATENATE: 'WA_SUB-ZP' W_INDEX INTO W_SUB.

        ASSIGN: (W_SUB) TO <SUB>.
        IF SY-SUBRC NE 0. CONTINUE. ENDIF.

        <SUB> = LT_CONDITION-KBETR.
    ENDCASE.
  ENDLOOP.

*----- A sub material's UoM must be 'EA'.
*----- If UoM is not 'EA', display error message.
  IF NOT ( ( IT_SUB-MEINS EQ WA_SUB-KMEIN AND
             IT_SUB-MEINS EQ IT_SUB-UNIT  AND
             WA_SUB-KMEIN EQ IT_SUB-UNIT )   ).
    MOVE: C_UOM_ERR TO WA_SUB-STS.
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
FORM APPEND_SUB_PRICE.
  CLEAR: IT_SUB.

  CASE WA_SUB-STS.
    WHEN C_NO_INFO.
      MOVE: TEXT-B02 TO WA_SUB-MSG.
    WHEN C_NO_COND.
      MOVE: TEXT-B03 TO WA_SUB-MSG.
    WHEN C_UOM_ERR.
      MOVE: TEXT-B01 TO WA_SUB-MSG.
    WHEN C_NO_MATL.
      MOVE: TEXT-B07 TO WA_SUB-MSG.
  ENDCASE.

  MOVE: WA_MODULE-VTYPE TO WA_SUB-VTYPE,
        WA_MODULE-MATNR TO WA_SUB-MATNR.

  MOVE: WA_SUB TO IT_SUB.
  MOVE-CORRESPONDING IT_OUTPUT TO IT_SUB.
  IT_SUB-MAKTX = IT_OUTPUT-CMAKTX.
*  it_sub-datab = it_output-cdatab.
*  it_sub-datbi = it_output-cdatbi.
  IT_SUB-LIFNR  = WA_SUB-LIFNR.
  IT_SUB-DATAB = WA_SUB-DATAB.
  IT_SUB-DATBI = WA_SUB-DATBI.

  IF IT_SUB-DATAB IS INITIAL.
    IT_SUB-DATAB = IT_OUTPUT-CDATAB.
  ENDIF.

  IF IT_SUB-DATBI IS INITIAL.
    IT_SUB-DATBI = IT_OUTPUT-CDATBI.
  ENDIF.


  IF IT_SUB-PEINH EQ 0.
    IT_SUB-PEINH = 1.
  ENDIF.

  IT_SUB-AMOUNT = IT_SUB-QNTY * IT_SUB-NETPR / IT_SUB-PEINH.

  APPEND IT_SUB.
  CLEAR: IT_SUB, WA_SUB.
ENDFORM.                    " append_sub_price
*&---------------------------------------------------------------------*
*&      Form  display_base_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_BASE_HEADER.
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

  WRITE: AT /1(SY-LINSZ) TEXT-H01 CENTERED.

  SKIP.
  WRITE:/2   TEXT-H02, (3) S_VTYPE-LOW, P_MCODE.
  WRITE:/2   TEXT-H04, P_LIFNR, LFA1-NAME1.
  WRITE:/2   TEXT-H05, (18) P_MATNR,
         153 TEXT-H08, SY-DATUM.
  WRITE:/2   TEXT-H06, (18) P_EKGRP,
         153 TEXT-H09, SY-UZEIT.
  WRITE:/2   TEXT-H07, (18) P_DATUM,
         153 TEXT-H10, (4) SY-PAGNO NO-GAP,'/', (4) WA_TOT_PAGE NO-ZERO.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/    TEXT-H11 NO-GAP, TEXT-H12.
  ULINE.

  MOVE: SY-PAGNO TO WA_TOT_PAGE.
ENDFORM.                    " display_base_header
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  NEW-PAGE LINE-SIZE 180 LINE-COUNT 58.

  LOOP AT IT_MODULE.
    NEW-LINE.

    SET LEFT SCROLL-BOUNDARY COLUMN 84.

    PERFORM SET_FORMAT.

    IF SY-LINNO EQ 90. ULINE. ENDIF.

    PERFORM DISPLAY_LINE.

    AT NEW MATNR.
      PERFORM DISPLAY_MATNR.
    ENDAT.

    AT NEW LIFNR.
      PERFORM DISPLAY_LIFNR.
    ENDAT.

    IF SY-LINNO EQ 11.
      PERFORM DISPLAY_MATNR.
      PERFORM DISPLAY_LIFNR.
    ENDIF.

    AT LAST. ULINE. ENDAT.
  ENDLOOP.

  CLEAR: IT_MODULE.
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
FORM SET_FORMAT.
  DATA: LW_COLOR_FLG TYPE I.

  LW_COLOR_FLG = SY-TABIX MOD 4.

  IF LW_COLOR_FLG EQ 1 OR
     LW_COLOR_FLG EQ 2.
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
FORM DISPLAY_LINE.
  WRITE:       '|' NO-GAP,
               IT_MODULE-CHBOX AS CHECKBOX,
          (03) SPACE NO-GAP, '|' NO-GAP,
          (10) SPACE NO-GAP, '|' NO-GAP,
          (20) SPACE NO-GAP, '|' NO-GAP,
          (18) SPACE NO-GAP, '|' NO-GAP,
          (20) SPACE NO-GAP, '|' NO-GAP.

* SELECT SINGLE b~effpr INTO eine-effpr
*              FROM eina AS a INNER JOIN eine AS b
*              ON a~infnr = b~infnr
*              WHERE a~matnr = it_module-matnr
*                AND a~lifnr = it_module-lifnr
*                AND a~loekz = ' '
*                AND b~werks = ' '
*                AND b~ekorg = c_ekorg
*                AND b~loekz = ' '.
  IF IT_MODULE-NETPR = IT_MODULE-MOAMT.
    WRITE: (4) ICON_GREEN_LIGHT  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
  ELSE.
    DATA: LW_RED(1).
    PERFORM SET_IT_DETAIL.
    PERFORM CHECK_IT_DETAIL_LIGHT USING LW_RED.
    IF LW_RED IS INITIAL.
      WRITE: (4) ICON_YELLOW_LIGHT AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
    ELSE.
      WRITE: (4) ICON_RED_LIGHT  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
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

  WRITE: (03) IT_MODULE-WAERS NO-GAP, '|' NO-GAP,
         (09) IT_MODULE-NETPR CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) IT_MODULE-MOAMT CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) IT_MODULE-ASYTR CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) IT_MODULE-DMBTR CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) IT_MODULE-ZTIR CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) IT_MODULE-ZP12  CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) IT_MODULE-ZP13  CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
              IT_MODULE-DATAB NO-GAP, '|' NO-GAP,
              IT_MODULE-DATBI NO-GAP,
              '|'.

  HIDE: IT_MODULE.
ENDFORM.                    " display_line
*&---------------------------------------------------------------------*
*&      Form  display_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MATNR.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 04(03) SPACE,
         08(10) SPACE,
         19(20) SPACE,
         40(18) IT_MODULE-MATNR HOTSPOT,
         59(20) IT_MODULE-MAKTX.
ENDFORM.                    " display_matnr
*&---------------------------------------------------------------------*
*&      Form  display_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LIFNR.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 04(03) IT_MODULE-VTYPE,
         08(10) IT_MODULE-LIFNR,
         19(20) IT_MODULE-NAME1.
ENDFORM.                    " display_lifnr
*&---------------------------------------------------------------------*
*&      Form  double_click_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOUBLE_CLICK_RTN.
  CASE SY-PFKEY.
    WHEN 'BASE'.
      PERFORM DOUBLE_CLICK_BASE.
    WHEN 'DETAIL'.
      PERFORM DOUBLE_CLICK_DETAIL.
    WHEN 'COMPARE'.
      PERFORM DOUBLE_CLICK_COMPARE.
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
FORM DOUBLE_CLICK_BASE.
  DATA: LW_FIELD(40).

  CHECK NOT IT_MODULE IS INITIAL.

  GET CURSOR FIELD LW_FIELD.

  CASE LW_FIELD.
    WHEN 'IT_MODULE-MATNR'.
      PERFORM DISPLAY_MODULE_INFO_RECORD.
    WHEN 'ICON_RED_LIGHT' OR 'ICON_YELLOW_LIGHT'.
      MESSAGE S000(ZZ) WITH IT_MODULE-MSG(50) IT_MODULE-MSG+50(50).
    WHEN 'ICON_GREEN_LIGHT' OR 'ICON_LIGHT_OUT'.
      SET PF-STATUS 'DETAIL'.
      PERFORM DISPLAY_SUB_MATERIAL.
    WHEN OTHERS.
      SET PF-STATUS 'DETAIL'.
      PERFORM DISPLAY_SUB_MATERIAL.
  ENDCASE.

  CLEAR: IT_MODULE.
ENDFORM.                    " double_click_base
*&---------------------------------------------------------------------*
*&      Form  double_click_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOUBLE_CLICK_DETAIL.
  DATA: LW_FIELD(40).

  CHECK NOT IT_DETAIL IS INITIAL.

  GET CURSOR FIELD LW_FIELD.

  CASE LW_FIELD.
    WHEN 'IT_DETAIL-IDNRK'.
      PERFORM DISPLAY_SUB_INFO_RECORD.
    WHEN 'ICON_RED_LIGHT'.
      MESSAGE S000(ZZ) WITH IT_DETAIL-MSG(50) IT_DETAIL-MSG+50(50).
  ENDCASE.

  CLEAR: IT_DETAIL.
ENDFORM.                    " double_click_detail
*&---------------------------------------------------------------------*
*&      Form  compare_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPARE_RTN.
  SET PF-STATUS 'COMPARE'.
  PERFORM CHECKED_ITEMS.
  PERFORM SET_IT_COMPARE_FROM_SOURCE.
  PERFORM DISPLAY_COMPARE.
  CLEAR: IT_COMPARE.
ENDFORM.                    " compare_rtn
*&---------------------------------------------------------------------*
*&      Form  detail_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETAIL_RTN.
  SET PF-STATUS 'DETAIL'.
  PERFORM READ_SELECTED_ITEM.
  PERFORM DISPLAY_SUB_MATERIAL.
ENDFORM.                    " detail_rtn
*&---------------------------------------------------------------------*
*&      Form  display_sub_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_SUB_INFO_RECORD.
  CASE SY-PFKEY.
    WHEN 'DETAIL'.
      CHECK NOT IT_DETAIL IS INITIAL.

      SET PARAMETER ID 'LIF' FIELD IT_DETAIL-LIFNR.
      SET PARAMETER ID 'MAT' FIELD IT_DETAIL-IDNRK.
      SET PARAMETER ID 'EKO' FIELD C_EKORG.
      SET PARAMETER ID 'WRK' FIELD ''.

      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

      CLEAR: IT_DETAIL.
    WHEN 'COMPARE'.
      CHECK NOT IT_COMPARE IS INITIAL.

      SET PARAMETER ID 'LIF' FIELD IT_COMPARE-LIFNR.
      SET PARAMETER ID 'MAT' FIELD IT_COMPARE-COMP.
      SET PARAMETER ID 'EKO' FIELD C_EKORG.
      SET PARAMETER ID 'WRK' FIELD ''.

      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

      CLEAR: IT_COMPARE.
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
FORM ASCENDING_RTN.
  CASE SY-PFKEY.
    WHEN 'BASE'.
      PERFORM ASCENDING_MODULE.
    WHEN 'DETAIL'.
      PERFORM ASCENDING_SUB.
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
FORM ASCENDING_MODULE.
  DATA: LW_FIELD(40).

  CHECK NOT IT_MODULE IS INITIAL.

  GET CURSOR FIELD LW_FIELD.

  LW_FIELD = LW_FIELD+10.

  SORT IT_MODULE BY (LW_FIELD).

  SY-LSIND = SY-LSIND - 1.

  PERFORM DISPLAY_DATA.
ENDFORM.                    " ascending_module
*&---------------------------------------------------------------------*
*&      Form  ascending_sub
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASCENDING_SUB.
  DATA: LW_FIELD(40).

  CHECK NOT IT_DETAIL IS INITIAL.

  GET CURSOR FIELD LW_FIELD.

  LW_FIELD = LW_FIELD+10.

  SORT IT_DETAIL BY (LW_FIELD).

  SY-LSIND = SY-LSIND - 1.

  PERFORM DISPLAY_IT_DETAIL.
ENDFORM.                    " ascending_sub
*&---------------------------------------------------------------------*
*&      Form  display_total_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_TOTAL_PAGE.
  DO WA_TOT_PAGE TIMES.
    READ LINE 7 OF PAGE SY-INDEX.
    MODIFY LINE 7 OF PAGE SY-INDEX
                     FIELD VALUE WA_TOT_PAGE FROM WA_TOT_PAGE.
    IF SY-SUBRC NE 0.
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
FORM DESCENDING_RTN.
  CASE SY-PFKEY.
    WHEN 'BASE'.
      PERFORM DESCENDING_MODULE.
    WHEN 'DETAIL'.
      PERFORM DESCENDING_SUB.
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
FORM DESCENDING_MODULE.
  DATA: LW_FIELD(40).

  CHECK NOT IT_MODULE IS INITIAL.

  GET CURSOR FIELD LW_FIELD.

  LW_FIELD = LW_FIELD+10.

  SORT IT_MODULE BY (LW_FIELD) DESCENDING.

  SY-LSIND = SY-LSIND - 1.

  PERFORM DISPLAY_DATA.
ENDFORM.                    " descending_module
*&---------------------------------------------------------------------*
*&      Form  descending_sub
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESCENDING_SUB.
  DATA: LW_FIELD(40).

  CHECK NOT IT_DETAIL IS INITIAL.

  GET CURSOR FIELD LW_FIELD.

  LW_FIELD = LW_FIELD+10.

  SORT IT_DETAIL BY (LW_FIELD) DESCENDING.

  SY-LSIND = SY-LSIND - 1.

  PERFORM DISPLAY_IT_DETAIL.
ENDFORM.                    " descending_sub
*&---------------------------------------------------------------------*
*&      Form  read_selected_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_SELECTED_ITEM.
  DO.
    CLEAR: IT_MODULE.

    READ LINE SY-INDEX FIELD VALUE IT_MODULE-CHBOX.
    IF SY-SUBRC NE 0. EXIT. ENDIF.

    CHECK IT_MODULE-CHBOX EQ 'X'.

    READ TABLE IT_MODULE WITH KEY VTYPE = IT_MODULE-VTYPE
                                  LIFNR = IT_MODULE-LIFNR
                                  MATNR = IT_MODULE-MATNR
                                  DATAB = IT_MODULE-DATAB
                                  DATBI = IT_MODULE-DATBI.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M01.
    ENDIF.

    EXIT.
  ENDDO.

  IF IT_MODULE IS INITIAL.
    MESSAGE E000(ZZ) WITH TEXT-M07.
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
FORM DISPLAY_SUB_MATERIAL.
  PERFORM SET_IT_DETAIL.
  PERFORM DISPLAY_IT_DETAIL.
ENDFORM.                    " display_sub_material
*&---------------------------------------------------------------------*
*&      Form  set_format_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FORMAT_DETAIL.
  IF WA_FORMAT_FLG EQ 'X'.
    CLEAR: WA_FORMAT_FLG.
  ELSE.
    WA_FORMAT_FLG = 'X'.
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
FORM DISPLAY_LINE_DETAIL.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 1      '|' NO-GAP,
          (18) SPACE NO-GAP, '|' NO-GAP,
               IT_DETAIL-SPOSN NO-GAP RIGHT-JUSTIFIED, '|' NO-GAP.

  IF IT_DETAIL-MSG IS INITIAL.
    WRITE: (4) ICON_GREEN_LIGHT  AS ICON NO-GAP,'|' NO-GAP.
  ELSE.
    WRITE: (4) ICON_RED_LIGHT    AS ICON HOTSPOT NO-GAP,'|' NO-GAP.
  ENDIF.

  IF WA_FORMAT_FLG EQ 'X'.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE: (18) IT_DETAIL-IDNRK NO-GAP, '|' NO-GAP,
         (25) IT_DETAIL-MAKTX NO-GAP, '|' NO-GAP,
              IT_DETAIL-DATAB NO-ZERO NO-GAP, '|' NO-GAP,
              IT_DETAIL-DATBI NO-ZERO NO-GAP, '|' NO-GAP,
         (04) IT_DETAIL-KMPMG UNIT IT_DETAIL-KMPME NO-ZERO NO-GAP,
              '|' NO-GAP,
              IT_DETAIL-KMPME NO-GAP, '|' NO-GAP,
         (09) IT_DETAIL-NETPR CURRENCY IT_DETAIL-WAERS ,
         (09) IT_DETAIL-ZP12  CURRENCY IT_DETAIL-WAERS ,
         (09) IT_DETAIL-ZP13  CURRENCY IT_DETAIL-WAERS NO-GAP,
              '|' NO-GAP,
              IT_DETAIL-PEINH NO-GAP, '|' NO-GAP,
              IT_DETAIL-KZUST NO-GAP, '|' NO-GAP,
         (10) IT_DETAIL-DMBTR CURRENCY IT_DETAIL-WAERS NO-GAP, '|'.

  HIDE: IT_DETAIL.
ENDFORM.                    " display_line_detail
*&---------------------------------------------------------------------*
*&      Form  display_upgvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_UPGVC.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 2(18) IT_DETAIL-UPGVC.
ENDFORM.                    " display_upgvc
*&---------------------------------------------------------------------*
*&      Form  display_it_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_IT_DETAIL.
  NEW-PAGE LINE-SIZE 158 LINE-COUNT 58.

  LOOP AT IT_DETAIL.
    NEW-LINE.

    IF SY-LINNO EQ 90. ULINE. ENDIF.

    AT NEW UPGVC.
      PERFORM SET_FORMAT_DETAIL.
    ENDAT.

    PERFORM DISPLAY_LINE_DETAIL.

    AT NEW UPGVC.
      PERFORM DISPLAY_UPGVC.
    ENDAT.

    AT LAST. ULINE. ENDAT.
  ENDLOOP.

  CLEAR: IT_DETAIL.
ENDFORM.                    " display_it_detail
*&---------------------------------------------------------------------*
*&      Form  display_module_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MODULE_INFO_RECORD.
  SET PARAMETER ID 'LIF' FIELD IT_MODULE-LIFNR.
  SET PARAMETER ID 'MAT' FIELD IT_MODULE-MATNR.
  SET PARAMETER ID 'EKO' FIELD C_EKORG.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

  CLEAR: IT_MODULE.
ENDFORM.                    " display_module_info_record
*&---------------------------------------------------------------------*
*&      Form  set_it_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_IT_DETAIL.
  CLEAR: IT_DETAIL, IT_DETAIL[].

  LOOP AT IT_SUB WHERE VTYPE = IT_MODULE-VTYPE
                   AND MATNR = IT_MODULE-MATNR.
    MOVE: IT_SUB-UPGVC   TO IT_DETAIL-UPGVC,
          IT_SUB-PREF    TO IT_DETAIL-SPOSN,
          IT_SUB-COMP    TO IT_DETAIL-IDNRK,
          IT_SUB-MAKTX   TO IT_DETAIL-MAKTX,
          IT_SUB-QNTY    TO IT_DETAIL-KMPMG,
          IT_SUB-UNIT    TO IT_DETAIL-KMPME,
          IT_SUB-LIFNR   TO IT_DETAIL-LIFNR,
          IT_SUB-DATAB   TO IT_DETAIL-DATAB,
          IT_SUB-DATBI   TO IT_DETAIL-DATBI,
          IT_SUB-NETPR   TO IT_DETAIL-NETPR,
          IT_SUB-ZP12    TO IT_DETAIL-ZP12,
          IT_SUB-ZP13    TO IT_DETAIL-ZP13,
          IT_SUB-PEINH   TO IT_DETAIL-PEINH,
          IT_SUB-KZUST   TO IT_DETAIL-KZUST,
          IT_SUB-AMOUNT  TO IT_DETAIL-DMBTR,
          IT_SUB-WAERS   TO IT_DETAIL-WAERS,
          IT_SUB-MSG     TO IT_DETAIL-MSG.
    APPEND IT_DETAIL.
  ENDLOOP.

  SORT IT_DETAIL BY UPGVC SPOSN.
ENDFORM.                    " set_it_detail
*&---------------------------------------------------------------------*
*&      Form  display_detail_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DETAIL_HEADER.
  WRITE: AT /1(SY-LINSZ) TEXT-H13 CENTERED.

  SKIP.
  WRITE:/2   TEXT-H02, (03) IT_MODULE-VTYPE.
  WRITE:/2   TEXT-H03,      IT_MODULE-LIFNR,      IT_MODULE-NAME1,
         65  TEXT-H14,      IT_MODULE-MOAMT CURRENCY IT_MODULE-WAERS.
  WRITE:/2   TEXT-H05, (18) IT_MODULE-MATNR, (25) IT_MODULE-MAKTX,
         65  TEXT-H15,      IT_MODULE-ASYTR CURRENCY IT_MODULE-WAERS,
         141 TEXT-H08,      SY-DATUM.
  WRITE:/2   TEXT-H06, (18) IT_MODULE-EKGRP,
         65  TEXT-H16,      IT_MODULE-DMBTR CURRENCY IT_MODULE-WAERS,
         141 TEXT-H09,      SY-UZEIT.
  WRITE:/2   TEXT-H07, (18) P_DATUM,
         65  TEXT-H17,      IT_MODULE-WAERS,
         141 TEXT-H10, (04) SY-PAGNO NO-GAP,'/',(4) WA_TOT_PAGE NO-ZERO.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/    TEXT-H18 NO-GAP, TEXT-H19.
  ULINE.

  MOVE: SY-PAGNO TO WA_TOT_PAGE.
ENDFORM.                    " display_detail_header
*&---------------------------------------------------------------------*
*&      Form  history_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HISTORY_RTN.
  PERFORM READ_SELECTED_ITEM.

  SUBMIT ZEMMPM48R_MODULE_ASSY_COST
    WITH S_VTYPE = IT_MODULE-VTYPE
    WITH S_EKGRP = IT_MODULE-EKGRP
    WITH S_MCODE = IT_MODULE-MCODE
     AND RETURN.

  CLEAR: IT_MODULE.
ENDFORM.                    " history_rtn
*&---------------------------------------------------------------------*
*&      Form  CHECKED_ITEMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECKED_ITEMS.
  DATA: LW_COUNT TYPE I.                   "Item Count

  CLEAR: WA_COMPARE.

  DO.
    CLEAR: IT_MODULE.

    READ LINE SY-INDEX FIELD VALUE IT_MODULE-CHBOX.
    IF SY-SUBRC NE 0. EXIT. ENDIF.

    CHECK IT_MODULE-CHBOX EQ 'X'.

    READ TABLE IT_MODULE WITH KEY VTYPE = IT_MODULE-VTYPE
                                  LIFNR = IT_MODULE-LIFNR
                                  MATNR = IT_MODULE-MATNR
                                  DATAB = IT_MODULE-DATAB
                                  DATBI = IT_MODULE-DATBI.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M01.
    ENDIF.

    CASE LW_COUNT.
      WHEN 0.
        MOVE: IT_MODULE-VTYPE     TO WA_COMPARE-VTYPE_S,
              IT_MODULE-NAME1     TO WA_COMPARE-NAME1_S,
              IT_MODULE-LIFNR     TO WA_COMPARE-LIFNR_S,
              IT_MODULE-MAKTX     TO WA_COMPARE-MAKTX_S,
              IT_MODULE-MATNR     TO WA_COMPARE-MATNR_S,
              IT_MODULE-DATAB     TO WA_COMPARE-DATAB_S,
              IT_MODULE-DATBI     TO WA_COMPARE-DATBI_S,
              IT_MODULE-MOAMT     TO WA_COMPARE-MOAMT_S,
              IT_MODULE-ASYTR     TO WA_COMPARE-ASYTR_S,
              IT_MODULE-DMBTR     TO WA_COMPARE-DMBTR_S,
              IT_MODULE-INDICATOR TO WA_COMPARE-INDICATOR_S,
              IT_MODULE-WAERS     TO WA_COMPARE-WAERS.
        LW_COUNT = LW_COUNT + 1.
      WHEN 1.
        MOVE: IT_MODULE-VTYPE     TO WA_COMPARE-VTYPE_T,
              IT_MODULE-NAME1     TO WA_COMPARE-NAME1_T,
              IT_MODULE-LIFNR     TO WA_COMPARE-LIFNR_T,
              IT_MODULE-MAKTX     TO WA_COMPARE-MAKTX_T,
              IT_MODULE-MATNR     TO WA_COMPARE-MATNR_T,
              IT_MODULE-DATAB     TO WA_COMPARE-DATAB_T,
              IT_MODULE-DATBI     TO WA_COMPARE-DATBI_T,
              IT_MODULE-MOAMT     TO WA_COMPARE-MOAMT_T,
              IT_MODULE-ASYTR     TO WA_COMPARE-ASYTR_T,
              IT_MODULE-DMBTR     TO WA_COMPARE-DMBTR_T,
              IT_MODULE-INDICATOR TO WA_COMPARE-INDICATOR_T.
        LW_COUNT = LW_COUNT + 1.
        EXIT.
      WHEN OTHERS.
        MESSAGE E000(ZZ) WITH TEXT-M08.
    ENDCASE.
  ENDDO.

  IF LW_COUNT < 2.
    MESSAGE E000(ZZ) WITH TEXT-M08.
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
FORM SET_IT_COMPARE_FROM_SOURCE.
*----- Fill IT_COMPARE for Source Module
  CLEAR: IT_COMPARE, IT_COMPARE[].

  LOOP AT IT_SUB WHERE VTYPE = WA_COMPARE-VTYPE_S
                   AND MATNR = WA_COMPARE-MATNR_S.
    CLEAR: IT_COMPARE.
    MOVE: IT_SUB-UPGVC   TO IT_COMPARE-UPGVC,
          IT_SUB-COMP    TO IT_COMPARE-COMP,
          IT_SUB-LIFNR   TO IT_COMPARE-LIFNR,
          IT_SUB-MSG     TO IT_COMPARE-MSG,
          IT_SUB-PREF    TO IT_COMPARE-PREF,
          IT_SUB-QNTY    TO IT_COMPARE-QNTY_S,
          IT_SUB-UNIT    TO IT_COMPARE-UNIT_S,
          IT_SUB-DATAB   TO IT_COMPARE-DATAB_S,
          IT_SUB-DATBI   TO IT_COMPARE-DATBI_S,
          IT_SUB-NETPR   TO IT_COMPARE-NETPR_S,
          IT_SUB-PEINH   TO IT_COMPARE-PEINH_S,
          IT_SUB-KZUST   TO IT_COMPARE-KZUST_S,
          IT_SUB-AMOUNT  TO IT_COMPARE-NETWR_S.
    APPEND IT_COMPARE.
  ENDLOOP.

*----- Fill IT_COMPARE for Target Module
  LOOP AT IT_SUB WHERE VTYPE = WA_COMPARE-VTYPE_T
                   AND MATNR = WA_COMPARE-MATNR_T.
    CLEAR: IT_COMPARE.

    READ TABLE IT_COMPARE WITH KEY UPGVC = IT_SUB-UPGVC
                                   COMP  = IT_SUB-COMP
                                   PREF  = IT_SUB-PREF.
    IF SY-SUBRC EQ 0.
      MOVE: IT_SUB-QNTY    TO IT_COMPARE-QNTY_T,
            IT_SUB-UNIT    TO IT_COMPARE-UNIT_T,
            IT_SUB-DATAB   TO IT_COMPARE-DATAB_T,
            IT_SUB-DATBI   TO IT_COMPARE-DATBI_T,
            IT_SUB-NETPR   TO IT_COMPARE-NETPR_T,
            IT_SUB-PEINH   TO IT_COMPARE-PEINH_T,
            IT_SUB-KZUST   TO IT_COMPARE-KZUST_T,
            IT_SUB-AMOUNT  TO IT_COMPARE-NETWR_T.
      MODIFY IT_COMPARE INDEX SY-TABIX.
    ELSE.
      MOVE: IT_SUB-UPGVC   TO IT_COMPARE-UPGVC,
            IT_SUB-COMP    TO IT_COMPARE-COMP,
            IT_SUB-LIFNR   TO IT_COMPARE-LIFNR,
            IT_SUB-MSG     TO IT_COMPARE-MSG,
            IT_SUB-PREF    TO IT_COMPARE-PREF,
            IT_SUB-QNTY    TO IT_COMPARE-QNTY_T,
            IT_SUB-UNIT    TO IT_COMPARE-UNIT_T,
            IT_SUB-DATAB   TO IT_COMPARE-DATAB_T,
            IT_SUB-DATBI   TO IT_COMPARE-DATBI_T,
            IT_SUB-NETPR   TO IT_COMPARE-NETPR_T,
            IT_SUB-PEINH   TO IT_COMPARE-PEINH_T,
            IT_SUB-KZUST   TO IT_COMPARE-KZUST_T,
            IT_SUB-AMOUNT  TO IT_COMPARE-NETWR_T.
      APPEND IT_COMPARE.
    ENDIF.
  ENDLOOP.

  SORT IT_COMPARE BY UPGVC COMP.
ENDFORM.                    " set_it_compare_from_source
*&---------------------------------------------------------------------*
*&      Form  display_compare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_COMPARE.
  NEW-PAGE LINE-SIZE 126.

  LOOP AT IT_COMPARE.
    NEW-LINE.

    PERFORM DISPLAY_COMPARE_LINE.

    AT NEW UPGVC.
      PERFORM DISPLAY_COMPARE_UPGVC.
    ENDAT.

    AT END OF UPGVC.
      SUM.
      PERFORM DISPLAY_COMPARE_UPGVC_SUM.
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
FORM DISPLAY_DETAIL_COMPARE.
  WRITE: AT /1(SY-LINSZ) TEXT-H20 CENTERED.

  SKIP.

  FORMAT COLOR COL_POSITIVE INTENSIFIED ON.

  ULINE.
  WRITE:/01 '|' NO-GAP,
            TEXT-H22, P_DATUM,
         30 '|',
         31 TEXT-H23,  (18) WA_COMPARE-MATNR_S,
         78 '|' NO-GAP,
            TEXT-H23,  (18) WA_COMPARE-MATNR_T,
        126 '|'.
  WRITE:/01 '|',
         30 '|',
         46(31) WA_COMPARE-MAKTX_S, '|' NO-GAP,
         94(31) WA_COMPARE-MAKTX_T, '|'.
  WRITE:/01 '|',
         30 '|',
         31 TEXT-H24,   WA_COMPARE-LIFNR_S, (20) WA_COMPARE-NAME1_S,
         78 '|' NO-GAP,
            TEXT-H24,   WA_COMPARE-LIFNR_T, (20) WA_COMPARE-NAME1_T,
         '|'.
  WRITE:/01 '|',
         30 '|',
         31 TEXT-H14,      WA_COMPARE-MOAMT_S CURRENCY WA_COMPARE-WAERS,
         78 '|' NO-GAP,
            TEXT-H14,      WA_COMPARE-MOAMT_T CURRENCY WA_COMPARE-WAERS,
        126 '|'.
  WRITE:/01 '|',
         30 '|',
         31 TEXT-H15,      WA_COMPARE-ASYTR_S CURRENCY WA_COMPARE-WAERS,
         78 '|' NO-GAP,
            TEXT-H15,      WA_COMPARE-ASYTR_T CURRENCY WA_COMPARE-WAERS,
        126 '|'.
  WRITE:/01 '|',
         30 '|',
         31 TEXT-H14,      WA_COMPARE-DMBTR_S CURRENCY WA_COMPARE-WAERS,
         78 '|' NO-GAP,
            TEXT-H14,      WA_COMPARE-DMBTR_T CURRENCY WA_COMPARE-WAERS,
        126 '|'.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/    TEXT-H21.
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
FORM DISPLAY_COMPARE_LINE.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: '|' NO-GAP,
         (09) SPACE,
         (18) IT_COMPARE-COMP NO-GAP,'|' NO-GAP.

  IF IT_COMPARE-QNTY_S EQ 0 OR
     IT_COMPARE-QNTY_T EQ 0.
    FORMAT COLOR COL_GROUP INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE: (04) IT_COMPARE-QNTY_S UNIT IT_COMPARE-UNIT_S NO-ZERO,
              IT_COMPARE-UNIT_S,
              IT_COMPARE-DATBI_S NO-ZERO,
         (09) IT_COMPARE-NETPR_S NO-ZERO,
         (03) IT_COMPARE-PEINH_S NO-ZERO,
              IT_COMPARE-KZUST_S,
         (09) IT_COMPARE-NETWR_S NO-GAP NO-ZERO, '|' NO-GAP.

  IF IT_COMPARE-QNTY_S EQ 0 OR
     IT_COMPARE-QNTY_T EQ 0.
    FORMAT COLOR COL_GROUP INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE: (04) IT_COMPARE-QNTY_T UNIT IT_COMPARE-UNIT_T NO-ZERO,
              IT_COMPARE-UNIT_T,
              IT_COMPARE-DATBI_T NO-ZERO,
         (09) IT_COMPARE-NETPR_T NO-ZERO,
         (03) IT_COMPARE-PEINH_T NO-ZERO,
              IT_COMPARE-KZUST_T,
         (09) IT_COMPARE-NETWR_T NO-GAP NO-ZERO, '|'.

  HIDE IT_COMPARE.
ENDFORM.                    " display_compare_line
*&---------------------------------------------------------------------*
*&      Form  display_compare_upgvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_COMPARE_UPGVC.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 2(9) IT_COMPARE-UPGVC.
ENDFORM.                    " display_compare_upgvc
*&---------------------------------------------------------------------*
*&      Form  display_compare_upgvc_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_COMPARE_UPGVC_SUM.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.

  ULINE.

  WRITE:         '|' NO-GAP,
            (09) IT_COMPARE-UPGVC,
            (18) TEXT-B11 NO-GAP,
                 '|' NO-GAP,
          69(09) IT_COMPARE-NETWR_S NO-GAP,
                 '|' NO-GAP,
         117(09) IT_COMPARE-NETWR_T NO-GAP,
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
FORM DOUBLE_CLICK_COMPARE.
  CHECK NOT IT_COMPARE IS INITIAL.

  SET PARAMETER ID 'LIF' FIELD IT_COMPARE-LIFNR.
  SET PARAMETER ID 'MAT' FIELD IT_COMPARE-COMP.
  SET PARAMETER ID 'EKO' FIELD C_EKORG.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

  CLEAR: IT_COMPARE.
ENDFORM.                    " DOUBLE_CLICK_COMPARE
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWNLOAD_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCEL_DOWNLOAD_RTN.
  CASE SY-PFKEY.
    WHEN 'BASE'.
      PERFORM DOWNLOAD_MODULE_PRICE.
    WHEN 'DETAIL'.
      PERFORM DOWNLOAD_DETAIL_PRICE.
    WHEN 'COMPARE'.
      PERFORM DOWNLOAD_COMPARE.
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
FORM DOWNLOAD_MODULE_PRICE.
  DATA: BEGIN OF LT_DOWNLOAD OCCURS 0,
          VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE, "Vehicle type
          LIFNR     LIKE   LFA1-LIFNR,            "Vendor
          NAME1     LIKE   LFA1-NAME1,            "Vendor name
          MATNR     LIKE   MARA-MATNR,            "Material
          MAKTX     LIKE   MAKT-MAKTX,            "Description
          EKGRP     LIKE   EKKO-EKGRP,            "Purchasing Group
          WAERS     LIKE   T001-WAERS,            "Currency
          MOAMT(16),                              "Module Price
          ASYTR(16),                              "Module Assy Cost
          DMBTR(16),                              "Material Cost
          DATAB(10),                              "Valid on
          DATBI(10),                              "Valid to
          MSG(100),
  END   OF LT_DOWNLOAD.

  LOOP AT IT_MODULE.
    MOVE-CORRESPONDING IT_MODULE TO LT_DOWNLOAD.

    WRITE: IT_MODULE-MOAMT CURRENCY IT_MODULE-WAERS
                           TO LT_DOWNLOAD-MOAMT,
           IT_MODULE-ASYTR CURRENCY IT_MODULE-WAERS
                           TO LT_DOWNLOAD-ASYTR,
           IT_MODULE-DMBTR CURRENCY IT_MODULE-WAERS
                           TO LT_DOWNLOAD-DMBTR,
           IT_MODULE-DATAB TO LT_DOWNLOAD-DATAB,
           IT_MODULE-DATBI TO LT_DOWNLOAD-DATBI.

    APPEND LT_DOWNLOAD.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            FILENAME                = 'C:\TEMP\MODULE_PRICE.XLS'
            FILETYPE                = 'DAT'
       TABLES
            DATA_TAB                = LT_DOWNLOAD
       EXCEPTIONS
            INVALID_FILESIZE        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6
            CUSTOMER_ERROR          = 7
            OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR: IT_MODULE.
ENDFORM.                    " DOWNLOAD_MODULE_PRICE
*&---------------------------------------------------------------------*
*&      Form  download_detail_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_DETAIL_PRICE.
  DATA: BEGIN OF LT_DOWNLOAD OCCURS 0,
          UPGVC LIKE ZSMM_SUB_DETAIL-UPGVC,
          SPOSN LIKE ZSMM_SUB_DETAIL-SPOSN,
          IDNRK LIKE ZSMM_SUB_DETAIL-IDNRK,
          MAKTX LIKE ZSMM_SUB_DETAIL-MAKTX,
          KMPMG(16),
          KMPME LIKE ZSMM_SUB_DETAIL-KMPME,
          DATAB(10),
          DATBI(10),
          NETPR(14),
          PEINH LIKE ZSMM_SUB_DETAIL-PEINH,
          KZUST LIKE ZSMM_SUB_DETAIL-KZUST,
          DMBTR(16),
          WAERS LIKE ZSMM_SUB_DETAIL-WAERS,
          LIFNR LIKE ZSMM_SUB_DETAIL-LIFNR,
          MSG LIKE ZSMM_SUB_DETAIL-MSG,
        END   OF LT_DOWNLOAD.

  LOOP AT IT_DETAIL.
    MOVE-CORRESPONDING IT_DETAIL TO LT_DOWNLOAD.

    WRITE: IT_DETAIL-NETPR CURRENCY IT_DETAIL-WAERS
                           TO LT_DOWNLOAD-NETPR,
           IT_DETAIL-DMBTR CURRENCY IT_DETAIL-WAERS
                           TO LT_DOWNLOAD-DMBTR,
           IT_DETAIL-KMPMG UNIT IT_DETAIL-KMPME
                           TO LT_DOWNLOAD-KMPMG,
           IT_DETAIL-DATAB TO LT_DOWNLOAD-DATAB,
           IT_DETAIL-DATBI TO LT_DOWNLOAD-DATBI.

    APPEND LT_DOWNLOAD.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            FILENAME                = 'C:\TEMP\SUB_PRICE.XLS'
            FILETYPE                = 'DAT'
       TABLES
            DATA_TAB                = LT_DOWNLOAD
       EXCEPTIONS
            INVALID_FILESIZE        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6
            CUSTOMER_ERROR          = 7
            OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR: IT_DETAIL.
ENDFORM.                    " download_detail_price
*&---------------------------------------------------------------------*
*&      Form  download_compare
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_COMPARE.
  DATA: BEGIN OF LT_DOWNLOAD OCCURS 0,
          UPGVC     LIKE   MARA-MATNR,              "UPG-VC
          PREF      LIKE   STPO-POSNR,              "ztbm_abxduldt-pref
          COMP      LIKE   MARA-MATNR,              "BOM Component
          QNTY_S(16),                               "Quantity
          UNIT_S    LIKE   MARA-MEINS,              "Unit of measure(BOM
          DATAB_S(10),                              "Valid on(sub)
          DATBI_S(10),                              "Valid to(Sub)
          NETPR_S(14),                              "Component Amount
          PEINH_S   LIKE   EKPO-PEINH,              "Component PriceUnit
          KZUST_S   LIKE   KONH-KZUST,              "Reason code
          NETWR_S(16),
          QNTY_T(16),                               "Quantity
          UNIT_T    LIKE   MARA-MEINS,              "Unit of measure(BOM
          DATAB_T(10),                              "Valid on(sub)
          DATBI_T(10),                              "Valid to(Sub)
          NETPR_T(14),                              "Component Amount
          PEINH_T   LIKE   EKPO-PEINH,              "Component PriceUnit
          KZUST_T   LIKE   KONH-KZUST,              "Reason code
          NETWR_T(16),
          WAERS     LIKE   T001-WAERS,              "Currency
          LIFNR     LIKE   LFA1-LIFNR,              "Vendor
          MSG(100),                                 "Message
  END   OF LT_DOWNLOAD.

  LOOP AT IT_COMPARE.
    MOVE-CORRESPONDING IT_COMPARE TO LT_DOWNLOAD.

    WRITE: IT_COMPARE-QNTY_S UNIT IT_COMPARE-UNIT_S
                             TO LT_DOWNLOAD-QNTY_S,
           IT_COMPARE-DATAB_S TO LT_DOWNLOAD-DATAB_S,
           IT_COMPARE-DATBI_S TO LT_DOWNLOAD-DATBI_S,
           IT_COMPARE-NETPR_S CURRENCY IT_COMPARE-WAERS
                              TO LT_DOWNLOAD-NETPR_S,
           IT_COMPARE-NETWR_S CURRENCY IT_COMPARE-WAERS
                              TO LT_DOWNLOAD-NETWR_S,
           IT_COMPARE-QNTY_T UNIT IT_COMPARE-UNIT_T
                             TO LT_DOWNLOAD-QNTY_T,
           IT_COMPARE-DATAB_T TO LT_DOWNLOAD-DATAB_T,
           IT_COMPARE-DATBI_T TO LT_DOWNLOAD-DATBI_T,
           IT_COMPARE-NETPR_T CURRENCY IT_COMPARE-WAERS
                              TO LT_DOWNLOAD-NETPR_T,
           IT_COMPARE-NETWR_T CURRENCY IT_COMPARE-WAERS
                              TO LT_DOWNLOAD-NETWR_T.

    APPEND LT_DOWNLOAD.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            FILENAME                = 'C:\TEMP\COMPARISON.XLS'
            FILETYPE                = 'DAT'
       TABLES
            DATA_TAB                = LT_DOWNLOAD
       EXCEPTIONS
            INVALID_FILESIZE        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6
            CUSTOMER_ERROR          = 7
            OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR: IT_COMPARE.
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
FORM CHECK_IT_DETAIL_LIGHT USING    P_LW_RED TYPE C.
  DATA: W_RED(1).
  LOOP AT IT_DETAIL.
    IF NOT IT_DETAIL-MSG IS INITIAL.
      W_RED = '1'.
    ENDIF.
  ENDLOOP.
  IF NOT W_RED IS INITIAL.
    P_LW_RED = W_RED.
  ELSE.
    CLEAR: P_LW_RED.
  ENDIF.
ENDFORM.                    " check_it_detail_light
