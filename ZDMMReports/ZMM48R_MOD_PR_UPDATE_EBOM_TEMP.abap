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
************************************************************************
REPORT ZEMMPM48R_MOD_PR_UPDATE_EBOM NO STANDARD PAGE HEADING
                                     LINE-SIZE  182
                                     LINE-COUNT  58.
*TABLES: zsmm_sub_material,
*        ztmm_cp_color.
TABLES: ZTPP_MOD_BOM_HIS.
INCLUDE: <ICON>.

*----- Type
TYPE-POOLS : SLIS, SP01R.

TABLES: ZTMM_ASSY_COST1,
        ZTMM_ASSY_COST2,
        ZVMM_INFO_CONDI,
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
        SEQNO     LIKE   ZTMM_ASSY_COST1-SEQNO, "Serial No
        DATAB     LIKE   ZTMM_ASSY_COST1-DATAB, "Valid on
        DATBI     LIKE   ZTMM_ASSY_COST1-DATBI, "Valid to
        MOAMT     LIKE   MSEG-DMBTR,            "Module Price
        ASYTR     LIKE   ZTMM_ASSY_COST1-ASYTR, "Module Assy Cost
        DMBTR     LIKE   MSEG-DMBTR,            "Material Cost
        DMAMT     TYPE   F,                     "Material Cost(Floating)
        WAERS     LIKE   T001-WAERS,            "Currency
        EKGRP     LIKE   EKKO-EKGRP,            "Purchasing Group
        NETPR     LIKE   EKPO-NETPR,            "Component Amount
        PEINH     LIKE   EKPO-PEINH,            "Component Price Unit
        MEINS     LIKE   EKPO-MEINS,            "UoM
        KZUST     LIKE   KONH-KZUST,            "Reason code
        NETPR_Y   LIKE   EKPO-NETPR,            "Yeaterday amount
        KZUST_Y   LIKE   KONH-KZUST.            "Yesterday reason code
        INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION_FLOATING.
DATA:   ZTIR     LIKE   EKPO-NETPR,
        STS,                                    "Module Info Status
        SAME_DATE_FLG,                          "Info valid from same?
        MSG(100),
        CHBOX,
      END   OF IT_MODULE.

DATA: BEGIN OF IT_SUB OCCURS 0,
        VTYPE     LIKE   ZTMM_ASSY_COST1-VTYPE, "Vehicle type
        MATNR     LIKE   ZTMM_ASSY_COST1-VTYPE, "Material
        UPGVC     LIKE   MARA-MATNR,              "UPG-VC
        PREF      LIKE   STPO-POSNR,              "BOM Item Number
        COMP      LIKE   MARA-MATNR,              "BOM Component
        MAKTX     LIKE   MAKT-MAKTX,              "Description
        LIFNR     LIKE   LFA1-LIFNR,              "Vendor
        AMOUNT    TYPE   F,                       "Component Amount
        QNTY      LIKE   STPO-MENGE,      "Quantity
        STGB      LIKE   STPO-STGB,      "End item type
        UNIT      LIKE   MARA-MEINS,      "Unit of measure(BOM)
        MEINS     LIKE   MARA-MEINS,              "UoM(sub)
        KMEIN     LIKE   KONP-KMEIN,              "UoM(Info)
        DATAB     LIKE   ZTMM_ASSY_COST1-DATAB,   "Valid on(sub)
        DATBI     LIKE   ZTMM_ASSY_COST1-DATBI,   "Valid to(Sub)
        NETPR     LIKE   EKPO-NETPR,              "Component Amount
        PEINH     LIKE   EKPO-PEINH,              "Component Price Unit
        WAERS     LIKE   T001-WAERS,              "Currency
        KZUST     LIKE   KONH-KZUST,              "Reason code
        AMOUNT_Y  TYPE   F.                       "Yesterday amount
        INCLUDE STRUCTURE ZSMM_CUSTOM_CONDITION_FLOATING.
DATA:   ZTIR      LIKE  EKPO-NETPR,
        STS,                                      "Status
        MSG(100),                                  "Message
      END   OF IT_SUB.

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
       PREF LIKE STPO-POSNR,
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
       SEQNO LIKE ZTMM_ASSY_COST1-SEQNO,
       END OF IT_TAB.

*data : begin of it_comp occurs 0,
*        matnr like mara-matnr,
*        UPGVC like mara-matnr,
*       end of it_comp .

*data : begin of it_scomp occurs 0.
*        include STRUCTURE ZTBM_ABXDULDT.
*data:   maktx     LIKE   makt-maktx,
*       end of it_scomp.
*
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
            PREF  LIKE STPO-POSNR,
            SEQNO LIKE ZTMM_ASSY_COST2-SEQNO,
            COMP  LIKE MARA-MATNR,
            CMAKTX LIKE MAKT-MAKTX,
            QNTY LIKE STPO-MENGE,
            UNIT LIKE MARA-MEINS,
            MEINS LIKE   MARA-MEINS,
            EKGRP LIKE ZTMM_ASSY_COST1-EKGRP,
            MCODE LIKE ZTMM_ASSY_COST1-MCODE,
            CDATAB LIKE ZTMM_ASSY_COST1-DATAB,
            CDATBI LIKE ZTMM_ASSY_COST1-DATBI,
            STGB   LIKE STPO-STGB,
         END OF IT_OUTPUT.

DATA : BEGIN OF IT_ERROR_PART OCCURS 0,
            LIFNR LIKE LFA1-LIFNR,
            MATNR LIKE MARA-MATNR,
            COMP  LIKE MARA-MATNR,
            STS(1),
            MSG(100),
         END OF IT_ERROR_PART.

DATA : BEGIN OF IT_ERROR_MODULE OCCURS 0,
            LIFNR LIKE LFA1-LIFNR,
            MATNR LIKE MARA-MATNR,
*            COMP  LIKE MARA-MATNR,
            STS(1),
            MSG(100),
         END OF IT_ERROR_MODULE.

DATA : BEGIN OF IT_UPDATED_MODULE OCCURS 0,
       MATNR LIKE MARA-MATNR,
       ASYTR LIKE MSEG-DMBTR,
                   MSG(100),
       END OF IT_UPDATED_MODULE.

*DATA: it_detail LIKE zsmm_sub_detail OCCURS 0 WITH HEADER LINE.
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

DATA: BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_TAB.

*----- Global variables & structures
DATA: WA_MODULE LIKE IT_MODULE.
DATA: WA_SUB LIKE IT_SUB,
      L_MCODE(4) TYPE C,
      L_CHECK(6) TYPE C.

DATA: W_MODE(1) VALUE 'N',
      W_UPDATE(1) VALUE 'S'.

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
      L_DYNAME LIKE SY-REPID.

FIELD-SYMBOLS: <MODULE>, <SUB>.

*----- Constants
" Module BOM is same all of plants
DATA: C_WERKS   LIKE   T001W-WERKS   VALUE   'P001',
      C_EKORG   LIKE   EKKO-EKORG    VALUE   'PU01',
      C_KSCHL   LIKE   KONP-KSCHL    VALUE   'PB00',
      C_ZTIR    LIKE   KONP-KSCHL    VALUE   'ZTIR',
      C_BUKRS   LIKE   T001-BUKRS    VALUE   'H201',
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
      C_READY             VALUE   1,
      C_WARNING             VALUE   2,
      C_SUCCESS             VALUE   3,
      C_ERROR             VALUE   4,
      C_INCORRECT            VALUE   5,
      C_NEW             VALUE   1,"Module Status
      C_DELETED           VALUE   2,"Module Status
      C_NO_COND             VALUE   3,"Condition does not exist
      C_DEL_CON             VALUE   4,"Condition was deleted
      C_NO_INFO             VALUE   5,"InfoRecord dosn't exist
      C_UOM_ERR             VALUE   6,"Incorrect UoM
      C_NO_MATL             VALUE   7,"M/M does not exist.
      C_EXIST             VALUE   8,"Info Record is exist.
      C_NO_PRICE             VALUE   9,"No Price.
      C_OVERLAP_INFO(1)              VALUE   'A', "DUPLICATE INFO RECORD
      C_NO_BOM(1)               VALUE   'B', "No Module BOM
      C_BOM_EXP(1)              VALUE   'C', "BOM EXPLOSION
** Chnaged by Furong on 05/19/09
*      C_RSN01(3)                     VALUE  'A0X',"Reason code
*      C_RSN02(3)                     VALUE  'A01',"Reason code
*      C_RSN03(3)                     VALUE  'ADX',"Reason code
*      C_RSN04(3)                     VALUE  'AD1',"Reason code
*      C_RSN05(3)                     VALUE  'AUX',"Reason code
*      C_RSN06(3)                     VALUE  'AU1'."Reason code

      C_RSN01(3)                     VALUE  'XM2',"Reason code
      C_RSN02(3)                     VALUE  'ME2',"Reason code
      C_RSN03(3)                     VALUE  'XM2',"Reason code
      C_RSN04(3)                     VALUE  'MD2',"Reason code
      C_RSN05(3)                     VALUE  'XM2',"Reason code
      C_RSN06(3)                     VALUE  'MU2'."Reason code
** End of change

*----- Table controls

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_VTYPE FOR ZTMM_ASSY_COST1-VTYPE
                            NO-EXTENSION NO INTERVALS.
*                s_mcode FOR ztmm_assy_cost1-mcode
*                            NO-EXTENSION NO INTERVALS,
*                s_lifnr FOR lfa1-lifnr
*                            NO-EXTENSION NO INTERVALS,
*                s_matnr FOR mara-matnr NO-EXTENSION NO INTERVALS,
*                s_ekgrp FOR ztmm_assy_cost1-ekgrp
*                            NO-EXTENSION NO INTERVALS.
PARAMETERS:     P_MCODE LIKE ZTMM_ASSY_COST1-MCODE OBLIGATORY,
                P_LIFNR LIKE LFA1-LIFNR,
                P_MATNR LIKE MARA-MATNR,
                P_EKGRP LIKE ZTMM_ASSY_COST1-EKGRP.

PARAMETERS:     P_DATUM LIKE SY-DATUM OBLIGATORY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) TEXT-T02.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) TEXT-T03 FOR FIELD P_BACK.
SELECTION-SCREEN POSITION 33.
PARAMETERS:     P_BACK  AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) TEXT-T04 FOR FIELD P_BACK.
SELECTION-SCREEN POSITION 33.
PARAMETERS:     P_EMAIL  AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.

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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  P_MCODE.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            MCODE  LIKE ZTMM_ASSY_COST1-MCODE,
           END OF VALUE_TAB.
* Select
  SELECT DISTINCT MCODE  FROM ZTMM_ASSY_COST1
             INTO TABLE VALUE_TAB.

  L_DYNAME = SY-REPID.

* Set F4 values for Module code
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

* Select Vehicle TYpe
  SELECT DISTINCT VTYPE  FROM ZTMM_ASSY_COST1
             INTO TABLE VALUE_TAB.
  L_DYNAME = SY-REPID.


*
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
  ENDCASE.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  SET TITLEBAR  'BASE'.
  SORT IT_MODULE BY INDICATOR LIFNR MATNR.
  IF P_BACK IS INITIAL.
    PERFORM DISPLAY_DATA.
  ELSE.
    PERFORM PROCESSING_RTN.
    IF P_EMAIL = 'X'.
      PERFORM SEND_EMAIL.
    ENDIF.
  ENDIF.

*----- Double click
AT LINE-SELECTION.
  PERFORM DOUBLE_CLICK_RTN.

*----- Function Key
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'EXCUTE'.
      PERFORM EXCUTE_RTN.
      IF P_EMAIL = 'X'.
        PERFORM SEND_EMAIL.
      ENDIF.

    WHEN 'DETAIL'.
      PERFORM DETAIL_RTN.
    WHEN 'MSG'.
      MESSAGE S000(ZZ) WITH IT_MODULE-MSG(50) IT_MODULE-MSG+50(50).
      CLEAR: IT_MODULE.
    WHEN 'SUB_INFO'.
      PERFORM DISPLAY_SUB_INFO_RECORD.
    WHEN 'SUB_MSG'.
      MESSAGE S000(ZZ) WITH IT_DETAIL-MSG(50) IT_DETAIL-MSG+50(50).
      CLEAR: IT_DETAIL.
    WHEN 'S_ALL'.
      PERFORM SELECT_ALL.
    WHEN 'D_ALL'.
      PERFORM DESELECT_ALL.
    WHEN 'ASCENDING'.
      PERFORM ASCENDING_RTN.
    WHEN 'DESCENDING'.
      PERFORM DESCENDING_RTN.
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
  P_DATUM = SY-DATUM.
ENDFORM.                    " initialization_rtn
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  PERFORM GET_SUB_PRICE.
  PERFORM CALCULATE_MODULE_PRICE.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE.
  DATA: L_DATE LIKE SY-DATUM.
  L_DATE = '20110201'.
  IF P_DATUM > L_DATE.
    MESSAGE E000(ZZ) WITH 'The program is no longer valid'.
  ENDIF.

  PERFORM CHECK_COMPANY.
  PERFORM CHECK_VTYPE.
  PERFORM CHECK_MCODE.
  PERFORM CHECK_LIFNR.
  PERFORM CHECK_MATNR.
  PERFORM CHECK_EKGRP.
  PERFORM CHECK_LOCKING.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_EKGRP
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
ENDFORM.                    " CHECK_EKGRP
*&---------------------------------------------------------------------*
*&      Form  CHECK_LIFNR
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
ENDFORM.                    " CHECK_LIFNR
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
    WA_VTYPE_T = 'ZZZ'.
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
*  IF s_mcode EQ ' '.
*    wa_mcode_t = 'ZZ'.
*    move '%' to l_mcode.
*  ELSE.
  WA_MCODE_F = WA_MCODE_T = P_MCODE.
  CONCATENATE '%' P_MCODE '%' INTO L_MCODE.
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
FORM APPEND_ITAB.
  DATA: LW_CONTINUE VALUE 'X'.

  LOOP AT IT_OUTPUT.
    ON CHANGE OF IT_OUTPUT-MATNR OR IT_OUTPUT-LIFNR.
      PERFORM READ_MODULE_INFO_RECORD.
    ENDON.

*  lw_continue = 'X'.
*  PERFORM check_cockpit_module_color USING lw_continue.
*
*  CHECK lw_continue EQ 'X'.

    PERFORM READ_SUB_INFO_RECORD.
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
FORM APPEND_SUB_PRICE.
  CASE WA_SUB-STS.
    WHEN C_NO_INFO.
      MOVE: TEXT-B02 TO WA_SUB-MSG.
    WHEN C_NO_COND.
      MOVE: TEXT-B03 TO WA_SUB-MSG.
    WHEN C_UOM_ERR.
      MOVE: TEXT-B01 TO WA_SUB-MSG.
    WHEN C_NO_MATL.
      MOVE: TEXT-B07 TO WA_SUB-MSG.
    WHEN C_NO_PRICE.
      MOVE: TEXT-B12 TO WA_SUB-MSG.
    WHEN C_OVERLAP_INFO.
      MOVE: TEXT-B13 TO WA_SUB-MSG.
  ENDCASE.

  MOVE: IT_OUTPUT-VTYPE TO WA_SUB-VTYPE,
        IT_OUTPUT-MATNR TO WA_SUB-MATNR.

  IF WA_SUB-PEINH EQ 0.
    WA_SUB-PEINH = 1.
  ENDIF.



  MOVE: WA_SUB TO IT_SUB.
  MOVE-CORRESPONDING IT_OUTPUT TO IT_SUB.
  IT_SUB-AMOUNT = IT_SUB-QNTY * WA_SUB-NETPR / WA_SUB-PEINH.
  IT_SUB-MAKTX = IT_OUTPUT-CMAKTX.
  IT_SUB-LIFNR = WA_SUB-LIFNR.
*  it_sub-datab =  it_output-cDATAB.
*  it_sub-datbi =  it_output-cDATBI.
  IT_SUB-DATAB =  WA_SUB-DATAB.
  IT_SUB-DATBI =  WA_SUB-DATBI.

  IF IT_SUB-DATAB IS INITIAL.
    IT_SUB-DATAB =  IT_OUTPUT-CDATAB.
  ENDIF.

  IF IT_SUB-DATBI IS INITIAL.
    IT_SUB-DATBI =  IT_OUTPUT-CDATBI.
  ENDIF.


  APPEND IT_SUB.
  CLEAR: IT_SUB, WA_SUB.
ENDFORM.                    " APPEND_SUB_PRICE
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
** Changed by Furong on 11/02/10
*        LT_STB TYPE STPOX OCCURS 0 WITH HEADER LINE,
        LT_STB TYPE ZTPP_MOD_BOM_HIS OCCURS 0 WITH HEADER LINE,
        L_VERSON LIKE ZTPP_MOD_BOM_HIS-VERSON,
** End of change
        L_STLAL LIKE MAST-STLAL.

  DATA: LT_COST1 LIKE TABLE OF ZTMM_ASSY_COST1 WITH HEADER LINE,
        LT_TAB LIKE TABLE OF IT_TAB WITH HEADER LINE.
  REFRESH: IT_TAB, IT_OUTPUT, IT_MODULE.
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
                      AND M~MSTAE <> '14'
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
                       AND M~MSTAE <> '14'
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
      IT_TAB-SEQNO = LT_COST1-SEQNO.
      IT_TAB-DATAB = LT_COST1-DATAB.
      IT_TAB-DATBI = LT_COST1-DATBI.
      APPEND IT_TAB.
    ENDLOOP.
    REFRESH LT_TAB.
  ENDLOOP.

*  DELETE ADJACENT DUPLICATES FROM IT_TAB COMPARING MATNR LIFNR.
  LOOP AT IT_TAB.
    CLEAR: L_STLAL.

** Changed by Furong on 11/02/10
    CLEAR: L_VERSON.

    SELECT SINGLE MAX( VERSON ) INTO L_VERSON
       FROM ZTPP_MOD_BOM_HIS
       WHERE MATNR = IT_TAB-MATNR.
*         AND CONF_DATE = P_DATUM.
    IF L_VERSON > 0.
      SELECT * INTO TABLE LT_STB
        FROM ZTPP_MOD_BOM_HIS
        WHERE MATNR = IT_TAB-MATNR
          AND VERSON = L_VERSON.
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

      IF SY-SUBRC <> 0.
        IT_ERROR_MODULE-MATNR = IT_TAB-MATNR.
        IT_ERROR_MODULE-STS = C_BOM_EXP.
        IT_ERROR_MODULE-MSG = 'No Active BOM for usegae 2'.
        APPEND IT_ERROR_MODULE.
        CLEAR IT_ERROR_MODULE.
      ENDIF.

      MOVE-CORRESPONDING IT_TAB TO IT_OUTPUT.
      LOOP AT LT_STB WHERE DATUV <= P_DATUM AND DATUB > P_DATUM.
        IT_OUTPUT-COMP = LT_STB-IDNRK.
        IT_OUTPUT-QNTY = LT_STB-MENGE.
        IT_OUTPUT-DATAB = LT_STB-DATUV.
        IT_OUTPUT-DATBI = LT_STB-DATUB.
        IT_OUTPUT-CDATAB = LT_STB-DATUV.
        IT_OUTPUT-CDATBI = LT_STB-DATUB.
        IT_OUTPUT-UPGVC = LT_STB-UPGN.
        SELECT SINGLE MAKTX INTO IT_OUTPUT-CMAKTX
          FROM MAKT
          WHERE MATNR = LT_STB-IDNRK.
        APPEND IT_OUTPUT.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

** End of change
  PERFORM APPEND_ITAB.

* END of changes - UD1K930385

  READ TABLE IT_MODULE INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M08.
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
FORM CALCULATE_MODULE_PRICE.
  DATA: LW_AMOUNT TYPE F,
        LW_PRICE_FLG,       "undetermined price or not
        LW_NEW_CONDITION.   "New condition or not
  DATA: LW_DETAIL LIKE IT_DETAIL.
  DATA: L_AMOUNT TYPE F.

  SORT IT_MODULE BY VTYPE MATNR LIFNR.
  SORT IT_SUB BY VTYPE MATNR UPGVC PREF.

  LOOP AT IT_MODULE.
    CLEAR: LW_PRICE_FLG, LW_NEW_CONDITION.
    CLEAR: L_AMOUNT.
    LOOP AT IT_SUB WHERE VTYPE = IT_MODULE-VTYPE
                     AND MATNR = IT_MODULE-MATNR.
      L_AMOUNT = L_AMOUNT + + IT_SUB-AMOUNT.
*      IT_MODULE-DMAMT = IT_MODULE-DMAMT + IT_SUB-AMOUNT.
      IT_MODULE-ZTIR = IT_MODULE-ZTIR + IT_SUB-ZTIR
                       / IT_SUB-PEINH * IT_SUB-QNTY.

      DO.
        MOVE: SY-INDEX TO W_INDEX.

        CONCATENATE: 'IT_MODULE-ZP' W_INDEX INTO W_MODULE,
                     'IT_SUB-ZP'    W_INDEX INTO W_SUB.

        ASSIGN: (W_SUB)    TO <SUB>,
                (W_MODULE) TO <MODULE>.
        IF SY-SUBRC NE 0. EXIT. ENDIF.

        <MODULE> = <MODULE> + <SUB> / IT_SUB-PEINH * IT_SUB-QNTY.
      ENDDO.

*      IF IT_SUB-STS NE 0.
      IF NOT IT_SUB-STS IS INITIAL.
        IT_MODULE-INDICATOR = C_WARNING.
      ENDIF.

      IF NOT ( IT_SUB-STS EQ C_NO_MATL OR
               IT_SUB-STS EQ C_NO_COND OR
               IT_SUB-STS EQ C_NO_INFO    ).
        IF NOT ( ( WA_SUB-MEINS EQ WA_SUB-KMEIN AND
                   WA_SUB-MEINS EQ WA_SUB-UNIT  AND
                   WA_SUB-KMEIN EQ WA_SUB-UNIT )   ).
          MOVE: C_UOM_ERR TO WA_SUB-STS.
        ENDIF.
      ENDIF.

      IF IT_SUB-STS IS INITIAL.
        IF IT_SUB-KZUST(1) EQ 'X'.
          LW_PRICE_FLG = 'X'.
        ENDIF.
      ELSE.
        LW_PRICE_FLG = 'X'.
      ENDIF.

      IF IT_SUB-AMOUNT NE IT_SUB-AMOUNT_Y.
        LW_NEW_CONDITION = 'X'.
      ENDIF.
    ENDLOOP.

    IT_MODULE-DMAMT = L_AMOUNT.
    MOVE: IT_MODULE-DMAMT TO IT_MODULE-DMBTR.

    IT_MODULE-MOAMT = IT_MODULE-ASYTR + IT_MODULE-DMBTR.

    PERFORM SET_REASON_CODE USING LW_PRICE_FLG LW_NEW_CONDITION.

    CASE IT_MODULE-STS.
      WHEN C_NEW OR C_NO_COND.
        IF IT_MODULE-INDICATOR NE C_WARNING.
          MOVE: C_READY TO IT_MODULE-INDICATOR.
        ENDIF.
      WHEN C_DELETED OR C_DEL_CON OR C_NO_MATL.
        MOVE: C_INCORRECT TO IT_MODULE-INDICATOR.
      WHEN C_EXIST.
        IF LW_NEW_CONDITION    EQ 'X'.
          IF IT_MODULE-INDICATOR NE C_WARNING.
            MOVE: C_READY     TO IT_MODULE-INDICATOR.
          ENDIF.
        ELSE.
          MOVE: C_SUCCESS   TO IT_MODULE-INDICATOR.
        ENDIF.
    ENDCASE.

    IF IT_MODULE-INDICATOR = C_WARNING.
      MOVE: TEXT-B10 TO IT_MODULE-MSG.
    ENDIF.


    IF NOT ( IT_MODULE-INDICATOR EQ C_INCORRECT OR
             IT_MODULE-INDICATOR EQ C_SUCCESS      ).
    ENDIF.
    MODIFY IT_MODULE.
  ENDLOOP.

*----- If Material cost is 0, can't excute creation Info Record
  LOOP AT IT_MODULE WHERE DMBTR     = 0
                      AND NOT ( INDICATOR = C_INCORRECT OR
                                INDICATOR = C_SUCCESS   ).
    MOVE: TEXT-B11    TO IT_MODULE-MSG,
          C_INCORRECT TO IT_MODULE-INDICATOR,
          C_NO_MATL   TO IT_MODULE-STS.
    MODIFY IT_MODULE.
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
FORM DISPLAY_DATA.
  NEW-PAGE LINE-SIZE 182 LINE-COUNT 58.

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
*&      Form  display_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LINE.
  DATA: LW_ZP12 LIKE IT_MODULE-DMBTR,
        LW_ZP13 LIKE IT_MODULE-DMBTR.

  WRITE:       '|' NO-GAP.

  IF IT_MODULE-INDICATOR EQ C_INCORRECT OR
     IT_MODULE-INDICATOR EQ C_SUCCESS.
    WRITE: IT_MODULE-CHBOX AS CHECKBOX INPUT OFF.
  ELSE.
    WRITE: IT_MODULE-CHBOX AS CHECKBOX.
  ENDIF.

  WRITE:  (03) SPACE NO-GAP, '|' NO-GAP,
          (10) SPACE NO-GAP, '|' NO-GAP,
          (20) SPACE NO-GAP, '|' NO-GAP,
          (18) SPACE NO-GAP, '|' NO-GAP,
          (20) SPACE NO-GAP, '|' NO-GAP.

  SELECT SINGLE B~EFFPR INTO EINE-EFFPR
              FROM EINA AS A INNER JOIN EINE AS B
              ON A~INFNR = B~INFNR
              WHERE A~MATNR = IT_MODULE-MATNR
                AND A~LIFNR = IT_MODULE-LIFNR
                AND A~LOEKZ = ' '
                AND B~WERKS = ' '
                AND B~EKORG = C_EKORG
                AND B~LOEKZ = ' '.
  IF EINE-EFFPR = IT_MODULE-MOAMT.
    WRITE: (4) ICON_GREEN_LIGHT  AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
  ELSE.
    WRITE: (4) ICON_LIGHT_OUT    AS ICON NO-GAP HOTSPOT,'|' NO-GAP.
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

  MOVE: IT_MODULE-ZP12 TO LW_ZP12,
        IT_MODULE-ZP13 TO LW_ZP13.

  WRITE:      IT_MODULE-KZUST NO-GAP, '|' NO-GAP,
              IT_MODULE-WAERS NO-GAP, '|' NO-GAP,
         (11) IT_MODULE-MOAMT CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (10) IT_MODULE-ASYTR CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (10) IT_MODULE-DMBTR CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (10) IT_MODULE-ZTIR CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) LW_ZP12         CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
         (09) LW_ZP13         CURRENCY IT_MODULE-WAERS NO-GAP,
              '|' NO-GAP,
              IT_MODULE-DATAB NO-GAP, '|' NO-GAP,
              IT_MODULE-DATBI NO-GAP,
              '|'.

  HIDE: IT_MODULE.
ENDFORM.                    " display_line
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
*&      Form  display_base_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_BASE_HEADER.
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

  CASE IT_MODULE-INDICATOR.
    WHEN C_ERROR.
      FORMAT COLOR COL_NEGATIVE   INTENSIFIED ON.
    WHEN C_SUCCESS.
      FORMAT COLOR COL_POSITIVE   INTENSIFIED OFF.
    WHEN C_INCORRECT.
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
*&      Form  DETAIL_RTN
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
ENDFORM.                    " DETAIL_RTN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SUB_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_SUB_MATERIAL.
  PERFORM SET_IT_DETAIL.
  PERFORM DISPLAY_IT_DETAIL.
ENDFORM.                    " DISPLAY_SUB_MATERIAL
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
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  CASE SY-DYNNR.
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
MODULE EXIT INPUT.
  CASE SY-UCOMM.
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
FORM DISPLAY_DETAIL_HEADER.
  WRITE: AT /1(SY-LINSZ) TEXT-H13 CENTERED.

  SKIP.
  WRITE:/2   TEXT-H02, (03) IT_MODULE-VTYPE.
  WRITE:/2   TEXT-H04,      IT_MODULE-LIFNR,      IT_MODULE-NAME1,
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
ENDFORM.                    " DOUBLE_CLICK_DETAIL
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_BASE
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
ENDFORM.                    " DOUBLE_CLICK_BASE
*&---------------------------------------------------------------------*
*&      Form  READ_MODULE_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MODULE_INFO_RECORD.
  PERFORM CHECK_MODULE.
  PERFORM APPEND_IT_MODULE.
ENDFORM.                    " READ_MODULE_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  READ_SUB_INFO_RECORD
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

  READ TABLE IT_SUB WITH KEY COMP  = IT_OUTPUT-COMP
                             UPGVC = IT_OUTPUT-UPGVC
                             PREF  = IT_OUTPUT-PREF.
  IF SY-SUBRC NE 0.
    MOVE-CORRESPONDING  IT_OUTPUT TO IT_SUB.
    PERFORM CHECK_RTN.
    PERFORM APPEND_SUB_PRICE.
  ELSE.
    MOVE: IT_SUB-LIFNR    TO WA_SUB-LIFNR,
          IT_SUB-AMOUNT   TO WA_SUB-AMOUNT,
          IT_SUB-AMOUNT_Y TO WA_SUB-AMOUNT_Y,
          IT_SUB-KMEIN    TO WA_SUB-KMEIN,
          IT_SUB-DATAB    TO WA_SUB-DATAB,
          IT_SUB-DATBI    TO WA_SUB-DATBI,
          IT_SUB-NETPR    TO WA_SUB-NETPR,
          IT_SUB-PEINH    TO WA_SUB-PEINH,
          IT_SUB-WAERS    TO WA_SUB-WAERS,
          IT_SUB-KZUST    TO WA_SUB-KZUST,
          IT_SUB-STS      TO WA_SUB-STS,
          IT_SUB-MSG      TO WA_SUB-MSG,
          IT_SUB-ZTIR     TO WA_SUB-ZTIR,
          IT_SUB-DATAB    TO WA_SUB-DATAB,
          IT_SUB-DATBI    TO WA_SUB-DATBI.

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
ENDFORM.                    " READ_SUB_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RTN.
  PERFORM INPUT_DAY_INFO_RECORD.
  PERFORM YESTERDAY_INFO_RECORD.
ENDFORM.                    " check_rtn
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
** changed by furong
  IF WA_MODULE-STS = C_DELETED.
    CLEAR: WA_MODULE.
    EXIT.
  ENDIF.
** end of change
  CASE WA_MODULE-STS.
    WHEN C_NEW.
      MOVE: TEXT-B04 TO WA_MODULE-MSG.
*    WHEN c_deleted.
*      MOVE: text-b05 TO wa_module-msg.
    WHEN C_NO_COND.
    WHEN C_DEL_CON.
      MOVE: TEXT-B06 TO WA_MODULE-MSG.
    WHEN C_NO_MATL.
      MOVE: TEXT-B07 TO WA_MODULE-MSG.
    WHEN C_EXIST.
      MOVE: TEXT-B09 TO WA_MODULE-MSG.
  ENDCASE.

  IF IT_MODULE-PEINH EQ 0.
    IT_MODULE-PEINH = 1.
  ENDIF.

  MOVE: T001-WAERS TO WA_MODULE-WAERS.

  MOVE: WA_MODULE TO IT_MODULE.
  MOVE-CORRESPONDING IT_OUTPUT TO IT_MODULE.

  APPEND IT_MODULE.
  CLEAR: IT_MODULE.
ENDFORM.                    " append_it_module
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
  CLEAR: EINA, A018.
  CLEAR: WA_MODULE-NETPR, WA_MODULE-PEINH, WA_MODULE-MEINS,
         WA_MODULE-MSG,   WA_MODULE-STS.

*----- Check Material Master
  IF IT_OUTPUT-MAKTX IS INITIAL.
    MOVE: C_NO_MATL TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  SELECT SINGLE MATNR A~LOEKZ
    INTO (EINA-MATNR,EINA-LOEKZ)
    FROM EINA AS A INNER JOIN EINE AS B
      ON A~INFNR = B~INFNR
   WHERE A~MATNR = IT_OUTPUT-MATNR
     AND A~LIFNR = IT_OUTPUT-LIFNR
** changed by furong
*     AND a~loekz = ' '
** end of change
     AND B~WERKS = ' '
     AND B~EKORG = C_EKORG.
** changed by furong
*     AND b~loekz = ' '.
** end of change
  IF SY-SUBRC NE 0.
    MOVE: C_NEW    TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  IF EINA-LOEKZ EQ 'X' OR EINE-LOEKZ = 'X'.
    MOVE: C_DELETED TO WA_MODULE-STS.
    EXIT.
  ENDIF.

*----- read info record
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

  SELECT SINGLE KBETR KPEIN KMEIN
    INTO (WA_MODULE-NETPR, WA_MODULE-PEINH, WA_MODULE-MEINS)
    FROM ZVMM_INFO_CONDI
   WHERE KNUMH = A018-KNUMH
     AND KSCHL = C_KSCHL
     AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    MOVE: C_DEL_CON TO WA_MODULE-STS.
    EXIT.
  ENDIF.

  CLEAR: IT_CONDITION, IT_CONDITION[].

  SELECT *
    INTO TABLE IT_CONDITION
    FROM ZTMM_ASSY_COST2
   WHERE VTYPE = IT_OUTPUT-VTYPE
     AND MCODE = IT_OUTPUT-MATNR+3(2)
     AND LIFNR = IT_OUTPUT-LIFNR
     AND SEQNO = IT_OUTPUT-SEQNO.
  IF SY-SUBRC NE 0.
    MOVE C_ERROR  TO WA_MODULE-INDICATOR.
    MOVE TEXT-B08 TO WA_MODULE-MSG.
  ENDIF.

  LOOP AT IT_CONDITION.
    MOVE: IT_CONDITION-KSCHL+2(2) TO W_INDEX.

    CONCATENATE: 'WA_MODULE-ZP' W_INDEX INTO W_MODULE.

    ASSIGN: (W_MODULE) TO <MODULE>.
    IF SY-SUBRC NE 0. CONTINUE. ENDIF.

    MOVE: IT_CONDITION-KBETR TO <MODULE>.
  ENDLOOP.


  IF A018-DATAB < P_DATUM.
    SELECT SINGLE *
      FROM A018
     WHERE KAPPL =  'M'
       AND KSCHL =  'PB00'
       AND MATNR =  IT_OUTPUT-MATNR
       AND LIFNR =  IT_OUTPUT-LIFNR
       AND EKORG =  C_EKORG
       AND ESOKZ =  '0'
       AND DATAB >= P_DATUM.
    IF SY-SUBRC EQ 0.
      MOVE: 'X' TO WA_MODULE-SAME_DATE_FLG.
    ENDIF.
  ELSE.
    MOVE: 'X' TO WA_MODULE-SAME_DATE_FLG.
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
FORM DISPLAY_SUB_INFO_RECORD.
  SET PARAMETER ID 'LIF' FIELD IT_DETAIL-LIFNR.
  SET PARAMETER ID 'MAT' FIELD IT_DETAIL-IDNRK.
  SET PARAMETER ID 'EKO' FIELD C_EKORG.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

  CLEAR: IT_DETAIL.
ENDFORM.                    " DISPLAY_SUB_INFO_RECORD
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
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_ALL.
  DO.
    CLEAR: IT_MODULE.

    READ LINE SY-INDEX FIELD VALUE IT_MODULE-CHBOX.
    IF SY-SUBRC NE 0. EXIT. ENDIF.

    CHECK NOT ( IT_MODULE-INDICATOR EQ C_INCORRECT OR
                IT_MODULE-INDICATOR EQ C_SUCCESS ).

    IT_MODULE-CHBOX = 'X'.

    MODIFY LINE SY-INDEX FIELD VALUE IT_MODULE-CHBOX.
  ENDDO.

  CLEAR: IT_MODULE.
ENDFORM.                    " SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESELECT_ALL.
  DO.
    CLEAR: IT_MODULE.

    READ LINE SY-INDEX FIELD VALUE IT_MODULE-CHBOX.
    IF SY-SUBRC NE 0. EXIT. ENDIF.

    CHECK NOT ( IT_MODULE-INDICATOR EQ C_INCORRECT OR
                IT_MODULE-INDICATOR EQ C_SUCCESS ).

    IT_MODULE-CHBOX = ' '.

    MODIFY LINE SY-INDEX FIELD VALUE IT_MODULE-CHBOX.
  ENDDO.

  CLEAR: IT_MODULE.
ENDFORM.                    " DESELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  ASCENDING_RTN
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
ENDFORM.                    " ASCENDING_RTN
*&---------------------------------------------------------------------*
*&      Form  DESCENDING_RTN
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
ENDFORM.                    " DESCENDING_RTN
*&---------------------------------------------------------------------*
*&      Form  ASCENDING_MODULE
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
ENDFORM.                    " ASCENDING_MODULE
*&---------------------------------------------------------------------*
*&      Form  ascending_SUB
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
ENDFORM.                    " ascending_SUB
*&---------------------------------------------------------------------*
*&      Form  SET_IT_DETAIL
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
          IT_SUB-PEINH   TO IT_DETAIL-PEINH,
          IT_SUB-KZUST   TO IT_DETAIL-KZUST,
          IT_SUB-AMOUNT  TO IT_DETAIL-DMBTR,
          IT_SUB-ZP12    TO IT_DETAIL-ZP12,
          IT_SUB-ZP13    TO IT_DETAIL-ZP13,
          IT_SUB-WAERS   TO IT_DETAIL-WAERS,
          IT_SUB-MSG     TO IT_DETAIL-MSG.
    APPEND IT_DETAIL.
  ENDLOOP.

  SORT IT_DETAIL BY UPGVC SPOSN.
ENDFORM.                    " SET_IT_DETAIL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_IT_DETAIL
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
ENDFORM.                    " DISPLAY_IT_DETAIL
*&---------------------------------------------------------------------*
*&      Form  DEscending_module
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
ENDFORM.                    " DEscending_module
*&---------------------------------------------------------------------*
*&      Form  DEscending_SUB
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
ENDFORM.                    " DEscending_SUB
*&---------------------------------------------------------------------*
*&      Form  excute_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCUTE_RTN.
  DATA: LW_TABIX LIKE SY-TABIX.

  DATA: LT_MODULE LIKE IT_MODULE OCCURS 0 WITH HEADER LINE.

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


** Added by Furong on 10/08/10
    MOVE: SY-TABIX TO LW_TABIX.

    READ TABLE IT_ERROR_MODULE WITH KEY MATNR = IT_MODULE-MATNR.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    READ TABLE IT_ERROR_PART WITH KEY MATNR = IT_MODULE-MATNR.
    IF SY-SUBRC = 0.
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

    CASE IT_MODULE-STS.
      WHEN C_NEW.
        PERFORM CREATE_INFO_RECORD.
      WHEN C_NO_COND OR C_EXIST.
        PERFORM CREATE_INFO_CONDITION.
    ENDCASE.

    MODIFY IT_MODULE INDEX LW_TABIX.
  ENDDO.

** Addition by Furong on 10/11/10
  PERFORM SAVE_PRICE_HISTORY.
** End of addition

  SY-LSIND = SY-LSIND - 1.
  SORT IT_MODULE BY INDICATOR LIFNR MATNR.
  PERFORM DISPLAY_DATA.
ENDFORM.                    " excute_rtn
*&---------------------------------------------------------------------*
*&      Form  create_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_INFO_RECORD.
  CLEAR: IT_MODULE-INDICATOR.

  PERFORM SET_IT_CONDITION.
  PERFORM GENERATE_BDC_ME11.
ENDFORM.                    " create_info_record
*&---------------------------------------------------------------------*
*&      Form  create_info_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_INFO_CONDITION.
  CLEAR: IT_MODULE-INDICATOR.

  PERFORM SET_IT_CONDITION.
  PERFORM GENERATE_BDC_ME12.
ENDFORM.                    " create_info_condition

*------------------------------------------------*
*       FORM DYNPRO
*------------------------------------------------*
*  -->  DYNBEGIN
*  -->  NAME
*  -->  VALUE
*------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR:  BDC_TAB.
    MOVE: NAME  TO BDC_TAB-PROGRAM,
          VALUE TO BDC_TAB-DYNPRO,
          'X'   TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR:  BDC_TAB.
    MOVE: NAME  TO BDC_TAB-FNAM,
          VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_IT_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_IT_CONDITION.
  CLEAR: IT_CONDITION, IT_CONDITION[].

  SELECT *
    INTO TABLE IT_CONDITION
    FROM ZTMM_ASSY_COST2
   WHERE VTYPE = IT_MODULE-VTYPE
     AND MCODE = IT_MODULE-MATNR+3(2)
     AND LIFNR = IT_MODULE-LIFNR
     AND SEQNO = IT_MODULE-SEQNO.
  IF SY-SUBRC NE 0.
    MOVE C_ERROR  TO IT_MODULE-INDICATOR.
    MOVE TEXT-B08 TO IT_MODULE-MSG.
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
FORM GENERATE_BDC_ME11.
  DATA: LW_KBETR(15),
        LW_DATAB(10),
        LW_ZTIR(15),
        WA_KBETR LIKE IT_MODULE-MOAMT.

  CHECK IT_MODULE-INDICATOR NE C_ERROR.

  REFRESH BDC_TAB.

  IF IT_MODULE-ZTIR > 0.
    WRITE IT_MODULE-ZTIR CURRENCY IT_MODULE-WAERS TO LW_ZTIR.
    WA_KBETR = IT_MODULE-MOAMT - IT_MODULE-ZTIR.
    WRITE WA_KBETR CURRENCY IT_MODULE-WAERS TO LW_KBETR.
  ELSE.
    WRITE IT_MODULE-MOAMT CURRENCY IT_MODULE-WAERS TO LW_KBETR.
  ENDIF.
  WRITE P_DATUM TO LW_DATAB.

  PERFORM DYNPRO USING:
        'X' 'SAPMM06I'              '0100',
        ' ' 'EINA-LIFNR'            IT_MODULE-LIFNR,
        ' ' 'EINA-MATNR'            IT_MODULE-MATNR,
        ' ' 'EINE-EKORG'            C_EKORG,
        ' ' 'EINE-WERKS'            ' ',
        ' ' 'EINA-INFNR'            ' ',
        ' ' 'RM06I-NORMB'           'X',
        ' ' 'BDC_OKCODE'            '/00',

        'X' 'SAPMM06I'              '0101',
        ' ' 'EINA-MEINS'            'EA',
        ' ' 'BDC_OKCODE'            '/00',

        'X' 'SAPMM06I'              '0102',
        ' ' 'EINE-EKGRP'            IT_MODULE-EKGRP,
        ' ' 'EINE-NORBM'            '1',
        ' ' 'EINE-UEBTK'            'X',
        ' ' 'EINE-WEBRE'            'X',
        ' ' 'EINE-MWSKZ'            'U0',
        ' ' 'EINE-NETPR'            LW_KBETR,
        ' ' 'EINE-WAERS'            IT_MODULE-WAERS,
        ' ' 'EINE-PEINH'            '1',
        ' ' 'EINE-BPRME'            'EA',
        ' ' 'BDC_OKCODE'            '=KO',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV13A-DATAB'           LW_DATAB,
        ' ' 'KONP-KBETR(01)'        LW_KBETR,
        ' ' 'BDC_OKCODE'            '=KDAT',

        'X' 'SAPMV13A'              '0200',
        ' ' 'KONH-KZUST'            IT_MODULE-KZUST,
        ' ' 'BDC_OKCODE'            '=KPOS',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV130-SELKZ(1)'        'X',
        ' ' 'BDC_CURSOR'            'KONP-KBETR(01)',
        ' ' 'BDC_OKCODE'            '=EINF'.

  IF IT_MODULE-ZTIR > 0.
    PERFORM DYNPRO USING:
         'X' 'SAPMV13A'              '0201',
         ' ' 'KONP-KSCHL(02)'        C_ZTIR,
         ' ' 'KONP-KBETR(02)'        LW_ZTIR,
         ' ' 'BDC_OKCODE'            '/00',

         'X' 'SAPMV13A'              '0201',
         ' ' 'RV130-SELKZ(2)'        'X',
         ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
         ' ' 'BDC_OKCODE'            '=EINF'.
  ENDIF.


  SORT IT_CONDITION BY KSCHL DESCENDING.

  LOOP AT IT_CONDITION.
    WRITE: IT_CONDITION-KBETR CURRENCY IT_MODULE-WAERS TO LW_KBETR.

    PERFORM DYNPRO USING:
          'X' 'SAPMV13A'              '0201',
          ' ' 'KONP-KSCHL(02)'        IT_CONDITION-KSCHL,
          ' ' 'KONP-KBETR(02)'        LW_KBETR,
          ' ' 'BDC_OKCODE'            '/00',

          'X' 'SAPMV13A'              '0201',
          ' ' 'RV130-SELKZ(2)'        'X',
          ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
          ' ' 'BDC_OKCODE'            '=EINF'.

    AT LAST.
      PERFORM DYNPRO USING:
            'X' 'SAPMV13A'              '0201',
            ' ' 'BDC_OKCODE'            '=SICH'.
    ENDAT.
  ENDLOOP.

** Furong on 01/12/11
*  CALL TRANSACTION 'ME11' USING  BDC_TAB
*                          MODE   W_MODE
*                          UPDATE W_UPDATE.

*  IF SY-SUBRC NE 0 OR SY-MSGNO NE '331'.
*    CALL FUNCTION 'RKC_MSG_STRING'
*         EXPORTING
*              ID      = SY-MSGID
*              MTYPE   = SY-MSGTY
*              NUMBER  = SY-MSGNO
*              PAR1    = SY-MSGV1
*              PAR2    = SY-MSGV2
*              PAR3    = SY-MSGV3
*              PAR4    = SY-MSGV4
*         IMPORTING
*              MSG_LIN = IT_MODULE-MSG.
*
*    IT_MODULE-INDICATOR  = C_ERROR.
*
*    IT_ERROR_MODULE-MATNR = IT_MODULE-MATNR.
*    IT_ERROR_MODULE-MSG = IT_MODULE-MSG.
*    IT_ERROR_MODULE-STS = C_ERROR.
*    APPEND IT_ERROR_MODULE.
*  ELSE.
  IT_MODULE-INDICATOR  = C_SUCCESS.
  IT_UPDATED_MODULE-MATNR = IT_MODULE-MATNR.
  IT_UPDATED_MODULE-ASYTR = IT_MODULE-ASYTR.
  IT_UPDATED_MODULE-MSG = 'Successfuly Created'.
  APPEND IT_UPDATED_MODULE.
*  ENDIF.
ENDFORM.                    " generate_bdc_me11
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_ME12
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GENERATE_BDC_ME12.
  DATA: LW_KBETR(15),
        LW_DATAB(10),
        LW_DMBTR LIKE MSEG-DMBTR,
        LW_ZTIR(15),
        WA_KBETR LIKE IT_MODULE-MOAMT.
  .

  CHECK IT_MODULE-INDICATOR NE C_ERROR.

  REFRESH BDC_TAB.

  IF IT_MODULE-ZTIR > 0.
    WRITE IT_MODULE-ZTIR CURRENCY IT_MODULE-WAERS TO LW_ZTIR.
    WA_KBETR = IT_MODULE-MOAMT - IT_MODULE-ZTIR.
    WRITE WA_KBETR CURRENCY IT_MODULE-WAERS TO LW_KBETR.
  ELSE.
    WRITE IT_MODULE-MOAMT CURRENCY IT_MODULE-WAERS TO LW_KBETR.
  ENDIF.

*  WRITE it_module-moamt CURRENCY it_module-waers TO lw_kbetr.
  WRITE P_DATUM TO LW_DATAB.

  PERFORM DYNPRO USING:
        'X' 'SAPMM06I'              '0100',
        ' ' 'EINA-LIFNR'            IT_MODULE-LIFNR,
        ' ' 'EINA-MATNR'            IT_MODULE-MATNR,
        ' ' 'EINE-EKORG'            C_EKORG,
        ' ' 'EINE-WERKS'            ' ',
        ' ' 'EINA-INFNR'            ' ',
        ' ' 'RM06I-NORMB'           'X',
        ' ' 'BDC_OKCODE'            '/00',

        'X' 'SAPMM06I'              '0101',
        ' ' 'BDC_OKCODE'            '=KO',

        'X' 'SAPLV14A'              '0102',
        ' ' 'BDC_OKCODE'            '=NEWD',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV13A-DATAB'           LW_DATAB,
        ' ' 'KONP-KBETR(01)'        LW_KBETR,
        ' ' 'BDC_OKCODE'            '=KDAT',

        'X' 'SAPMV13A'              '0200',
        ' ' 'KONH-KZUST'            IT_MODULE-KZUST,
        ' ' 'BDC_OKCODE'            '=KPOS',

        'X' 'SAPMV13A'              '0201',
        ' ' 'RV130-SELKZ(1)'        'X',
        ' ' 'BDC_CURSOR'            'KONP-KBETR(01)',
        ' ' 'BDC_OKCODE'            '=EINF'.

  IF IT_MODULE-ZTIR > 0.
    PERFORM DYNPRO USING:
         'X' 'SAPMV13A'              '0201',
         ' ' 'KONP-KSCHL(02)'        C_ZTIR,
         ' ' 'KONP-KBETR(02)'        LW_ZTIR,
         ' ' 'BDC_OKCODE'            '/00',

         'X' 'SAPMV13A'              '0201',
         ' ' 'RV130-SELKZ(2)'        'X',
         ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
         ' ' 'BDC_OKCODE'            '=EINF'.
  ENDIF.



  SORT IT_CONDITION BY KSCHL DESCENDING.


  DO 100 TIMES.
    W_INDEX = 100 - SY-INDEX.

    CONCATENATE: 'IT_MODULE-ZP' W_INDEX INTO W_MODULE.

    ASSIGN: (W_MODULE)  TO <MODULE>.

    CHECK SY-SUBRC EQ 0.

    MOVE: <MODULE> TO LW_DMBTR.
    WRITE: LW_DMBTR CURRENCY IT_MODULE-WAERS TO LW_KBETR.

    PERFORM DYNPRO USING:
          'X' 'SAPMV13A'              '0201',
          ' ' 'KONP-KSCHL(02)'        W_MODULE+10(4),
          ' ' 'KONP-KBETR(02)'        LW_KBETR,
          ' ' 'BDC_OKCODE'            '/00',

          'X' 'SAPMV13A'              '0201',
          ' ' 'RV130-SELKZ(2)'        'X',
          ' ' 'BDC_CURSOR'            'KONP-KBETR(02)',
          ' ' 'BDC_OKCODE'            '=EINF'.

    CHECK W_INDEX EQ '01'.

    PERFORM DYNPRO USING:
          'X' 'SAPMV13A'              '0201',
          ' ' 'BDC_OKCODE'            '=SICH'.

    IF IT_MODULE-SAME_DATE_FLG EQ ''.
      PERFORM DYNPRO USING:
            'X' 'SAPMV13A'              '0121',
            ' ' 'BDC_OKCODE'            '=BSTA'.

    ENDIF.

    EXIT.
  ENDDO.

** furong on 01/12/11
*  CALL TRANSACTION 'ME12' USING  BDC_TAB
*                          MODE   W_MODE
*                          UPDATE W_UPDATE.
*
*  IF SY-SUBRC NE 0 OR SY-MSGNO NE '335'.
*    CALL FUNCTION 'RKC_MSG_STRING'
*         EXPORTING
*              ID      = SY-MSGID
*              MTYPE   = SY-MSGTY
*              NUMBER  = SY-MSGNO
*              PAR1    = SY-MSGV1
*              PAR2    = SY-MSGV2
*              PAR3    = SY-MSGV3
*              PAR4    = SY-MSGV4
*         IMPORTING
*              MSG_LIN = IT_MODULE-MSG.
*
*    IT_MODULE-INDICATOR  = C_ERROR.
*    IT_ERROR_MODULE-MATNR = IT_MODULE-MATNR.
*    IT_ERROR_MODULE-MSG = IT_MODULE-MSG.
*    IT_ERROR_MODULE-STS = C_ERROR.
*    APPEND IT_ERROR_MODULE.
*
*  ELSE.
  IT_MODULE-INDICATOR  = C_SUCCESS.
  IT_UPDATED_MODULE-MATNR = IT_MODULE-MATNR.
  IT_UPDATED_MODULE-ASYTR = IT_MODULE-ASYTR.
  IT_UPDATED_MODULE-MSG = 'Successfuly Changed'.
  APPEND IT_UPDATED_MODULE.
*  ENDIF.
ENDFORM.                    " GENERATE_BDC_ME12
*&---------------------------------------------------------------------*
*&      Form  processing_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSING_RTN.

  LOOP AT IT_MODULE.

    CHECK IT_MODULE-INDICATOR EQ C_READY.
    " OR it_module-indicator EQ c_warning.

** Added by Furong on 10/08/10

    READ TABLE IT_ERROR_MODULE WITH KEY MATNR = IT_MODULE-MATNR.
    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    READ TABLE IT_ERROR_PART WITH KEY MATNR = IT_MODULE-MATNR.
    IF SY-SUBRC = 0.
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

    SELECT SINGLE B~EFFPR INTO EINE-EFFPR
                FROM EINA AS A INNER JOIN EINE AS B
                ON A~INFNR = B~INFNR
                WHERE A~MATNR = IT_MODULE-MATNR
                  AND A~LIFNR = IT_MODULE-LIFNR
                  AND A~LOEKZ = ' '
                  AND B~WERKS = ' '
                  AND B~EKORG = C_EKORG
                  AND B~LOEKZ = ' '.

    IF EINE-EFFPR <> IT_MODULE-MOAMT.

      CASE IT_MODULE-STS.
        WHEN C_NEW.
          PERFORM CREATE_INFO_RECORD.
        WHEN C_NO_COND OR C_EXIST.
          PERFORM CREATE_INFO_CONDITION.
      ENDCASE.
      MODIFY IT_MODULE.
    ENDIF.

  ENDLOOP.
** Addition by Furong on 10/11/10
  PERFORM SAVE_PRICE_HISTORY.
** End of addition

  SORT IT_MODULE BY INDICATOR LIFNR MATNR.
  PERFORM DISPLAY_DATA.
ENDFORM.                    " processing_rtn
*&---------------------------------------------------------------------*
*&      Form  check_locking
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_LOCKING.
  CALL FUNCTION 'ENQUEUE_EZ_ZSMM_MODULE'
       EXPORTING
            MODE_ZSMM_MODULE = 'E'
            MANDT            = SY-MANDT
            VTYPE            = S_VTYPE-LOW
            MCODE            = P_MCODE
            LIFNR            = P_LIFNR
            MATNR            = P_MATNR
            EKGRP            = P_EKGRP
            DATUM            = P_DATUM
       EXCEPTIONS
            FOREIGN_LOCK     = 1
            SYSTEM_FAILURE   = 2
            OTHERS           = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
FORM SET_REASON_CODE USING PW_PRICE_FLG PW_NEW_CONDITION.
  DATA: LW_DATUM LIKE SY-DATUM,
        LW_PEINH LIKE EKPO-PEINH,
        LW_NETPR LIKE EKPO-NETPR,
        LW_INFO(50).
  FIELD-SYMBOLS: <LW_INFO>.
*        lw_moamt LIKE it_module-moamt.

  DATA: BEGIN OF LT_CONDITION OCCURS 0,
          KBETR LIKE ZVMM_INFO_CONDI-KBETR,
          KPEIN LIKE ZVMM_INFO_CONDI-KPEIN,
          KMEIN LIKE ZVMM_INFO_CONDI-KMEIN,
          KSCHL LIKE ZVMM_INFO_CONDI-KSCHL,
          KZUST LIKE ZVMM_INFO_CONDI-KZUST,
        END   OF LT_CONDITION.

  DATA: LW_LAST_INFO LIKE ZSMM_CUSTOM_CONDITION_FLOATING.

  CLEAR: EINA, A018, IT_MODULE-NETPR_Y, IT_MODULE-KZUST_Y.
  IF IT_MODULE-STS EQ C_NEW     OR
     IT_MODULE-STS EQ C_DELETED.
    IF PW_PRICE_FLG EQ 'X'.
      IT_MODULE-KZUST = C_RSN01.
    ELSE.
      IT_MODULE-KZUST = C_RSN02.
    ENDIF.
    EXIT.
  ENDIF.

*----- read yesterday info record
  LW_DATUM = P_DATUM - 1.
  SELECT SINGLE *
    FROM A018
   WHERE KAPPL =  'M'
     AND KSCHL =  'PB00'
     AND MATNR =  IT_MODULE-MATNR
     AND LIFNR =  IT_MODULE-LIFNR
     AND EKORG =  C_EKORG
     AND ESOKZ =  '0'
     AND DATAB <= LW_DATUM
     AND DATBI >= LW_DATUM.
  IF SY-SUBRC NE 0.
    IF PW_PRICE_FLG EQ 'X'.
      IT_MODULE-KZUST = C_RSN01.
    ELSE.
      IT_MODULE-KZUST = C_RSN02.
    ENDIF.
    MOVE: 'X' TO PW_NEW_CONDITION.
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


  SELECT KBETR KPEIN KZUST KSCHL
    INTO CORRESPONDING FIELDS OF TABLE LT_CONDITION
    FROM ZVMM_INFO_CONDI
   WHERE KNUMH = A018-KNUMH
     AND ( KSCHL =    C_KSCHL OR
           KSCHL LIKE 'ZP%' )
     AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    IF PW_PRICE_FLG EQ 'X'.
      IT_MODULE-KZUST = C_RSN01.
    ELSE.
      IT_MODULE-KZUST = C_RSN02.
    ENDIF.
    MOVE: 'X' TO PW_NEW_CONDITION.
    EXIT.
  ENDIF.

  LOOP AT LT_CONDITION.
    CASE LT_CONDITION-KSCHL.
      WHEN C_KSCHL.
        MOVE: LT_CONDITION-KZUST TO IT_MODULE-KZUST_Y.
        IT_MODULE-NETPR_Y = LT_CONDITION-KBETR / LT_CONDITION-KPEIN.
      WHEN OTHERS.
        MOVE: LT_CONDITION-KSCHL+2(2) TO W_INDEX.

        CONCATENATE: 'LW_LAST_INFO-ZP' W_INDEX INTO W_MODULE.

        ASSIGN: (W_MODULE) TO <MODULE>.
        IF SY-SUBRC NE 0. CONTINUE. ENDIF.

        IF LT_CONDITION-KPEIN EQ 0.
          <MODULE> = 0.
        ELSE.
          <MODULE> = LT_CONDITION-KBETR / LT_CONDITION-KPEIN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  IF     IT_MODULE-NETPR_Y EQ IT_MODULE-MOAMT.
    IF PW_PRICE_FLG EQ 'X'.
      IT_MODULE-KZUST = C_RSN01.
    ELSE.
      IT_MODULE-KZUST = C_RSN02.
    ENDIF.

    IF IT_MODULE-KZUST NE IT_MODULE-KZUST_Y.
      MOVE: 'X' TO PW_NEW_CONDITION.
    ENDIF.
  ELSEIF IT_MODULE-NETPR_Y > IT_MODULE-MOAMT.
    IF PW_PRICE_FLG EQ 'X'.
      IT_MODULE-KZUST = C_RSN03.
    ELSE.
      IT_MODULE-KZUST = C_RSN04.
    ENDIF.
    MOVE: 'X' TO PW_NEW_CONDITION.
  ELSEIF IT_MODULE-NETPR_Y < IT_MODULE-MOAMT.
    IF PW_PRICE_FLG EQ 'X'.
      IT_MODULE-KZUST = C_RSN05.
    ELSE.
      IT_MODULE-KZUST = C_RSN06.
    ENDIF.
    MOVE: 'X' TO PW_NEW_CONDITION.
  ENDIF.

  IF PW_NEW_CONDITION EQ SPACE.
    DO.
      MOVE: SY-INDEX TO W_INDEX.

      CONCATENATE: 'LW_LAST_INFO-ZP' W_INDEX INTO LW_INFO,
                   'IT_MODULE-ZP'    W_INDEX INTO W_MODULE.

      ASSIGN: (LW_INFO)  TO <LW_INFO>,
              (W_MODULE) TO <MODULE>.
      IF SY-SUBRC NE 0. EXIT. ENDIF.

      IF <LW_INFO> NE <MODULE>.
        MOVE: 'X' TO PW_NEW_CONDITION.
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
FORM INPUT_DAY_INFO_RECORD.
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
        PRDAT LIKE EINE-PRDAT,
        END OF IT_EINA_EINE_TEMP.

  DATA: L_LINE TYPE I.

*----- Check Material Master
  IF IT_OUTPUT-CMAKTX IS INITIAL.
    MOVE: C_NO_MATL TO WA_SUB-STS.
    EXIT.
  ENDIF.

*** INSERTED BY FURONG ON 27/07/2005
  SELECT MATNR A~LIFNR WGLIF PRDAT INTO TABLE IT_EINA_EINE_TEMP
   FROM EINA AS A INNER JOIN EINE AS B
     ON A~INFNR = B~INFNR
  WHERE A~MATNR = IT_OUTPUT-COMP
    AND A~URZZT = 'SUB'
    AND A~LOEKZ = ' '
    AND B~WERKS = ' '
    AND B~EKORG = C_EKORG
    AND B~LOEKZ = ' '
    AND B~PRDAT >= P_DATUM.
  IF SY-SUBRC NE 0.
    MOVE: C_NO_INFO TO WA_SUB-STS.
    CLEAR: IT_ERROR_PART.
    IT_ERROR_PART-LIFNR = IT_OUTPUT-LIFNR.
    IT_ERROR_PART-MATNR = IT_OUTPUT-MATNR.
    IT_ERROR_PART-COMP = IT_OUTPUT-COMP.
    IT_ERROR_PART-STS = C_NO_PRICE.
    IT_ERROR_PART-MSG = TEXT-B02.
    MOVE: C_NO_PRICE TO WA_SUB-STS.
    APPEND IT_ERROR_PART.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_EINA_EINE_TEMP LINES L_LINE.
  IF L_LINE > 1.
    CLEAR: IT_ERROR_PART.
    IT_ERROR_PART-LIFNR = IT_OUTPUT-LIFNR.
    IT_ERROR_PART-MATNR = IT_OUTPUT-MATNR.
    IT_ERROR_PART-COMP = IT_OUTPUT-COMP.
    IT_ERROR_PART-STS = C_OVERLAP_INFO.
    IT_ERROR_PART-MSG = TEXT-B13.
    APPEND IT_ERROR_PART.
    MOVE: C_OVERLAP_INFO TO WA_SUB-STS.
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
** Added by Furong on 10/08/10
        IF LT_CONDITION-KBETR = 0.
          MOVE C_NO_PRICE TO WA_SUB-STS.
          CLEAR: IT_ERROR_PART.
          IT_ERROR_PART-LIFNR = IT_OUTPUT-LIFNR.
          IT_ERROR_PART-MATNR = IT_OUTPUT-MATNR.
          IT_ERROR_PART-COMP = IT_OUTPUT-COMP.
          IT_ERROR_PART-STS = C_NO_PRICE.
          IT_ERROR_PART-MSG = TEXT-B12.
          APPEND IT_ERROR_PART.
        ENDIF.
** End of addition
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
ENDFORM.                    " input_day_info_record
*&---------------------------------------------------------------------*
*&      Form  yesterday_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM YESTERDAY_INFO_RECORD.
  DATA: LW_DATUM LIKE SY-DATUM.

  LW_DATUM = P_DATUM - 1.

*----- Read submaterial price
  SELECT SINGLE *
    FROM A018
   WHERE KAPPL =  'M'
     AND KSCHL =  'PB00'
     AND MATNR =  IT_OUTPUT-COMP
     AND LIFNR =  WA_SUB-LIFNR
     AND EKORG =  C_EKORG
     AND ESOKZ =  '0'
     AND DATAB <= LW_DATUM
     AND DATBI >= LW_DATUM.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM ZVMM_INFO_CONDI
   WHERE KNUMH = A018-KNUMH
     AND KSCHL = C_KSCHL
     AND LOEVM_KO = ' '.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  WA_SUB-AMOUNT_Y = WA_SUB-QNTY * ZVMM_INFO_CONDI-KBETR /
                                  ZVMM_INFO_CONDI-KPEIN.
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
MODULE MOVE_ITAB_TO_SCREEN_9000 OUTPUT.

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
FORM SEND_EMAIL.
  DATA: L_SUBJECT(40) TYPE C VALUE 'Module Bom Change/Creation List'.

  DATA:   IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
          IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
          IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                  WITH HEADER LINE,
          GD_CNT TYPE I,
          GD_SENT_ALL(1) TYPE C,
          GD_DOC_DATA LIKE SODOCCHGI1.
*          GD_ERROR TYPE SY-SUBRC.

  CLEAR: IT_MAIL,IT_MAIL[].

** Make body part of email
  MOVE 'RE: Module Price Update' TO IT_MAIL.

  APPEND IT_MAIL.
  CLEAR: IT_MAIL.
*  MOVE '---------------------------------------------' TO IT_MAIL.
*  APPEND IT_MAIL.
*  CLEAR: IT_MAIL.

  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

  MOVE 'Error with parts:' TO IT_MAIL.
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.
  MOVE '===============' TO IT_MAIL.
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

*  MOVE: 'Suppier' TO IT_MAIL+0(10),
  MOVE: 'Module' TO  IT_MAIL+0(20),
       'Part' TO  IT_MAIL+20(20),
       'Message' TO  IT_MAIL+40(100).
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

*  MOVE: '----------' TO IT_MAIL+0(10),
  MOVE: '--------------------' TO  IT_MAIL+0(20),
        '--------------------' TO  IT_MAIL+20(20),
        '----------------------------------------' TO  IT_MAIL+40(40),
        '----------------------------------------' TO  IT_MAIL+80(40),
        '--------------------' TO  IT_MAIL+120(20).
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

  LOOP AT IT_ERROR_PART.
*    MOVE: IT_ERROR_PART-LIFNR TO IT_MAIL+0(10),
    MOVE:IT_ERROR_PART-MATNR TO  IT_MAIL+0(20),
         IT_ERROR_PART-COMP TO  IT_MAIL+20(20),
         IT_ERROR_PART-MSG TO  IT_MAIL+40(100).
    APPEND IT_MAIL.
    CLEAR: IT_MAIL.
  ENDLOOP.


  APPEND IT_MAIL.
  APPEND IT_MAIL.
  APPEND IT_MAIL.

  MOVE 'Error with Module(s):' TO IT_MAIL.
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.
  MOVE '===================' TO IT_MAIL.
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

*  MOVE: 'Suppier' TO IT_MAIL+0(10),
  MOVE:  'Module' TO  IT_MAIL+0(20),
        'Message' TO  IT_MAIL+20(100).
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

*  MOVE: '----------' TO IT_MAIL+0(10),
  MOVE: '--------------------' TO  IT_MAIL+0(20),
        '----------------------------------------' TO  IT_MAIL+20(40),
        '----------------------------------------' TO  IT_MAIL+60(40),
        '--------------------' TO  IT_MAIL+100(20).
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

  LOOP AT IT_ERROR_MODULE.
*    MOVE: IT_ERROR_MODULE-LIFNR TO IT_MAIL+0(10),
    MOVE: IT_ERROR_MODULE-MATNR TO  IT_MAIL+0(20),
          IT_ERROR_MODULE-MSG TO  IT_MAIL+20(100).
    APPEND IT_MAIL.
  ENDLOOP.


  APPEND IT_MAIL.
  APPEND IT_MAIL.
  APPEND IT_MAIL.

  MOVE 'Successfully Updated Module(s):' TO IT_MAIL.
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.
  MOVE '================================' TO IT_MAIL.
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

*  MOVE: 'Suppier' TO IT_MAIL+0(10),
  MOVE:  'Module' TO  IT_MAIL+0(20),
        'Message' TO  IT_MAIL+20(100).
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

*  MOVE: '----------' TO IT_MAIL+0(10),
  MOVE: '--------------------' TO  IT_MAIL+0(20),
        '----------------------------------------' TO  IT_MAIL+20(40),
        '----------------------------------------' TO  IT_MAIL+60(40),
        '--------------------' TO  IT_MAIL+100(20).
  APPEND IT_MAIL.
  CLEAR: IT_MAIL.

  LOOP AT IT_UPDATED_MODULE.
    MOVE: IT_UPDATED_MODULE-MATNR TO  IT_MAIL+0(20),
          IT_UPDATED_MODULE-MSG TO  IT_MAIL+20(100).
    APPEND IT_MAIL.
  ENDLOOP.


  GD_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  GD_DOC_DATA-OBJ_LANGU = SY-LANGU.
  GD_DOC_DATA-OBJ_NAME  = SY-REPID.
  GD_DOC_DATA-OBJ_DESCR = L_SUBJECT.
  GD_DOC_DATA-SENSITIVTY = 'F'.

* Describe the body of the message
  CLEAR IT_PACKING_LIST.
  REFRESH IT_PACKING_LIST.
  IT_PACKING_LIST-TRANSF_BIN = SPACE.
  IT_PACKING_LIST-HEAD_START = 1.
  IT_PACKING_LIST-HEAD_NUM = 0.
  IT_PACKING_LIST-BODY_START = 1.
  DESCRIBE TABLE IT_MAIL LINES IT_PACKING_LIST-BODY_NUM.
  IT_PACKING_LIST-DOC_TYPE = 'RAW'.
  APPEND IT_PACKING_LIST.

* Add the recipients email address
  CLEAR IT_RECEIVERS.
  REFRESH IT_RECEIVERS.
*  IT_RECEIVERS-RECEIVER = 'SAIFVAL'.
  IT_RECEIVERS-RECEIVER = 'PARTS_DEV'.
*  it_receivers-rec_type = 'U'.  " internet email
  IT_RECEIVERS-REC_TYPE = 'C'.
  IT_RECEIVERS-COM_TYPE = 'INT'.
  IT_RECEIVERS-NOTIF_DEL = 'X'.
  IT_RECEIVERS-NOTIF_NDEL = 'X'.
  APPEND IT_RECEIVERS.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
       EXPORTING
            DOCUMENT_DATA              = GD_DOC_DATA
            PUT_IN_OUTBOX              = 'X'
       IMPORTING
            SENT_TO_ALL                = GD_SENT_ALL
       TABLES
            PACKING_LIST               = IT_PACKING_LIST
            CONTENTS_TXT               = IT_MAIL
            RECEIVERS                  = IT_RECEIVERS
       EXCEPTIONS
            TOO_MANY_RECEIVERS         = 1
            DOCUMENT_NOT_SENT          = 2
            DOCUMENT_TYPE_NOT_EXIST    = 3
            OPERATION_NO_AUTHORIZATION = 4
            PARAMETER_ERROR            = 5
            X_ERROR                    = 6
            ENQUEUE_ERROR              = 7
            OTHERS                     = 8.

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
FORM SAVE_PRICE_HISTORY.
  DATA: IT_MOD_PRI_BK LIKE TABLE OF ZTMM_MOD_PRI_BK WITH HEADER LINE.
  DATA: L_DATE LIKE SY-DATUM,
        L_TIME LIKE SY-UZEIT.

  L_DATE = SY-DATUM.
  L_TIME = SY-UZEIT.
  SORT IT_UPDATED_MODULE BY MATNR.
  LOOP AT IT_UPDATED_MODULE.
    IT_MOD_PRI_BK-MATNR  = IT_UPDATED_MODULE-MATNR.
    IT_MOD_PRI_BK-RUN_DATE = L_DATE.
    IT_MOD_PRI_BK-RUN_TIME = L_TIME.
    IT_MOD_PRI_BK-RUN_USER = SY-UNAME.
    IT_MOD_PRI_BK-ASYTR = IT_UPDATED_MODULE-ASYTR.
    LOOP AT IT_SUB WHERE MATNR = IT_UPDATED_MODULE-MATNR.
*          IT_SUB-PREF    TO IT_DETAIL-SPOSN,
*          IT_SUB-MAKTX   TO IT_DETAIL-MAKTX,
*          IT_SUB-MSG     TO IT_DETAIL-MSG.
      IT_MOD_PRI_BK-COMP = IT_SUB-COMP.
      IT_MOD_PRI_BK-LIFNR = IT_SUB-LIFNR.
      IT_MOD_PRI_BK-QNTY = IT_SUB-QNTY.
      IT_MOD_PRI_BK-UNIT = IT_SUB-UNIT.
      IT_MOD_PRI_BK-UPGVC = IT_SUB-UPGVC.
      IT_MOD_PRI_BK-DATAB = IT_SUB-DATAB.
      IT_MOD_PRI_BK-DATBI = IT_SUB-DATBI.
      IT_MOD_PRI_BK-NETPR = IT_SUB-NETPR.
      IT_MOD_PRI_BK-PEINH = IT_SUB-PEINH.
      IT_MOD_PRI_BK-ZP12 = IT_SUB-ZP12.
      IT_MOD_PRI_BK-ZP13 = IT_SUB-ZP13.
      IT_MOD_PRI_BK-KZUST = IT_SUB-KZUST.
      IT_MOD_PRI_BK-DMBTR = IT_SUB-AMOUNT.
      IT_MOD_PRI_BK-WAERS = IT_SUB-WAERS.

      IT_MOD_PRI_BK-INPUT_DATE = P_DATUM.
      APPEND IT_MOD_PRI_BK.
      CLEAR: IT_MOD_PRI_BK-ASYTR.
    ENDLOOP.
  ENDLOOP.
  INSERT ZTMM_MOD_PRI_BK FROM TABLE IT_MOD_PRI_BK.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " backup_data
