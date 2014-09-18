************************************************************************
* Program Name      : ZPPR_VPC_STOCK
* Creation Date     :
* Development Request No :
* Addl Documentation:
* Description       : copy from zppa9999-88 (vpc list)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 11/02/07        Furong           UD1K942044     Change/Dispaly mode
************************************************************************

REPORT ZPPR_VPC_STOCK NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS, VRM.
CONSTANTS:  C_CG_DATE TYPE D VALUE '20100731'.
DATA : BEGIN OF IT_SUM_APP246 OCCURS 0.
        INCLUDE STRUCTURE ZSPP_SUM_APP246.
DATA :   OBJEK TYPE AUSP-OBJEK,
         MODEL TYPE AUSP-ATWRT,
       END OF   IT_SUM_APP246 .
DATA:  WA_FLAG                 TYPE C .
DATA: BEGIN OF IT_DET_APP246 OCCURS 0.
        INCLUDE STRUCTURE ZSPP_DET_APP246.
DATA:   OBJEK TYPE AUSP-OBJEK,
        MODEL TYPE AUSP-ATWRT,
      END OF   IT_DET_APP246.
*---- LIST BOX DATA
DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST,
      BEGIN OF YLIST     OCCURS 0,
         KEY(40) TYPE C,
         TEXT(80) TYPE C,
      END OF YLIST      .

* Parameters(Screen0110)
DATA: P_BODYNO_APP246 TYPE AUSP-ATWRT,    "P_BODY_SERIAL(09)
      P_LINE_APP246(03),                  "P_TRIM_LINE_NO
      P_PROG_APP246(08),                  "P_RP_STATUS
      P_PROG_APP246_H(08),                  "P_RP_STATUS
      P_STATUS_APP246(10),                " 'S':Summary, 'D':Detail.
      P_WONO_APP246 TYPE MARA-MATNR,      "P_WORK_ORDER
      P_EXTC_APP246(03),                  "P_EXT_COLOR
      P_INTC_APP246(03),                  "P_INT_COLOR
      P_TOTAL_APP246(05) TYPE N.

DATA:  NAME                    TYPE VRM_ID.
RANGES: P_PROG FOR AUSP-ATWRT.

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DATA: BEGIN OF IT_OBJEK OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF IT_OBJEK .

DATA: BEGIN OF GT_VALUE OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF GT_VALUE .

DATA: BEGIN OF TAB_MODEL OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF TAB_MODEL .

DATA: BEGIN OF TAB_BODY_SERIAL OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF TAB_BODY_SERIAL .

DATA: BEGIN OF TAB_WORK_ORDER OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_WORK_ORDER .

DATA: BEGIN OF TAB_EXT_COLOR OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_EXT_COLOR .

DATA: BEGIN OF TAB_INT_COLOR OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_INT_COLOR .

DATA: BEGIN OF TAB_PLAN_ORDER OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_PLAN_ORDER .

DATA: BEGIN OF TAB_RP OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_RP .

DATA: BEGIN OF TAB_VIN OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_VIN .

DATA: BEGIN OF TAB_MI OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_MI .

DATA: BEGIN OF TAB_OCN OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_OCN .

DATA: BEGIN OF TAB_DEALER OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_DEALER .

DATA: BEGIN OF TAB_RP01_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP01_SHOP_DATE .

DATA: BEGIN OF TAB_RP02_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP02_SHOP_DATE .

DATA: BEGIN OF TAB_RP03_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP03_SHOP_DATE .

DATA: BEGIN OF TAB_RP04_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP04_SHOP_DATE .

DATA: BEGIN OF TAB_RP05_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP05_SHOP_DATE .

DATA: BEGIN OF TAB_RP06_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP06_SHOP_DATE .

DATA: BEGIN OF TAB_RP07_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP07_SHOP_DATE .

DATA: BEGIN OF TAB_RP17_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP17_SHOP_DATE .

DATA: BEGIN OF TAB_RP18_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP18_SHOP_DATE .

DATA: BEGIN OF TAB_RP19_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP19_SHOP_DATE .

DATA: BEGIN OF TAB_RP20_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP20_SHOP_DATE .

DATA: BEGIN OF TAB_RP21_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP21_SHOP_DATE .

DATA: BEGIN OF TAB_RP22_SHOP_DATE OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATFLV LIKE AUSP-ATFLV,
      END OF TAB_RP22_SHOP_DATE .

DATA: BEGIN OF TAB_RP_STATUS OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_RP_STATUS.

DATA: BEGIN OF TAB_RP_DATE_TIME OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_RP_DATE_TIME.

DATA: BEGIN OF TAB_SERIAL OCCURS 0,
        OBJEK LIKE AUSP-OBJEK, ATWRT LIKE AUSP-ATWRT,
      END OF TAB_SERIAL.

DATA: WA_ALV_CALLED,
      OK_CODE LIKE SY-UCOMM,
      W_UCOMM LIKE SY-UCOMM,
      W_TOTAL_RECODS TYPE I..
.
*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : WA_FNAME_TX(40),
       WA_SAVELINE_IX     LIKE  SY-INDEX.

DATA : ALV_GRID               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER           TYPE SCRFNAME VALUE 'CONTAINER',
       WA_CC_2108             TYPE SCRFNAME VALUE 'CC_2108'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : GS_VARIANT        TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria

DATA : GT_FIELDCAT_SLIS  TYPE SLIS_T_FIELDCAT_ALV.
DATA : WA_FIELDCAT       TYPE LVC_S_FCAT.
***************************************************
* Display Image
***************************************************
TYPE-POOLS: CNDP.

**********************************
*Define Macro
**********************************
DEFINE FIELDCAT_COMPOSE.
  IT_FIELDCAT-FIELDNAME = &1.
  IT_FIELDCAT-REPTEXT   = &2.
  IT_FIELDCAT-OUTPUTLEN = 20.
  APPEND IT_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.

DEFINE SET_FIELDCAT.
  L_FIELDCAT-REPTEXT   = &1.
  L_FIELDCAT-SCRTEXT_L = &1.
  L_FIELDCAT-SCRTEXT_M = &1.
  L_FIELDCAT-SCRTEXT_S = &1.
  L_FIELDCAT-COLDDICTXT = 'L'.
  L_FIELDCAT-OUTPUTLEN = &2.

END-OF-DEFINITION.
*&---------------------------------------------------------------------*
*&      Module  initialization_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIALIZATION_APP246 OUTPUT.
  IF WA_FLAG IS INITIAL.
    WA_FLAG = 'X'.
    PERFORM MAKE_DROPDOWN_LIST_BOX_APP246.
    P_STATUS_APP246    = 'S'.
    WA_ALV_CALLED = 'X'.
  ENDIF.
ENDMODULE.                 " initialization_app246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_APP246 OUTPUT.

ENDMODULE.                 " status_app246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_grid_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_GRID_APP246 OUTPUT.
  IF WA_ALV_CALLED <> SPACE.
    CLEAR WA_ALV_CALLED .
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT GS_CUSTOM_CONTAINER
    EXPORTING CONTAINER_NAME = WA_CONTAINER.

    CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER.

    PERFORM  BUILD_VARIANT_APP246.
    PERFORM  BUILD_LAYOUT_APP246.
    PERFORM  BUILD_FIELDCAT_APP246.
    IF P_STATUS_APP246 = 'D'.
      PERFORM  CALL_METHOD_DET_APP246 .
    ELSE.
      PERFORM  CALL_METHOD_SUM_APP246.
    ENDIF.

  ENDIF.

ENDMODULE.                 " create_alv_grid_app246  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_cursor_field_app246  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD_APP246 OUTPUT.
  SET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " set_cursor_field_app246  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  build_variant_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_VARIANT_APP246.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " build_variant_app246
*&---------------------------------------------------------------------*
*&      Form  build_layout_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT_APP246.
  GS_LAYOUT-ZEBRA  = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN
ENDFORM.                    " build_layout_APP246
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT_APP246.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.


  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.
  IF P_STATUS_APP246 = 'D'.
    L_STRUCT = 'ZSPP_DET_APP246'.
  ELSE.
    L_STRUCT = 'ZSPP_SUM_APP246'.
  ENDIF.
  CLEAR : WA_FIELDCAT, GT_FIELDCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE        = 'X'
            I_STRUCTURE_NAME       = L_STRUCT
       CHANGING
            CT_FIELDCAT            = GT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

  LOOP AT GT_FIELDCAT INTO WA_FIELDCAT.
*    IF P_STATUS_APP246 = 'D'.
*      PERFORM SET_FIELD_INFO_DET_APP246 USING WA_FIELDCAT.
*    ELSE.
    PERFORM SET_FIELD_INFO_SUM_APP246 USING WA_FIELDCAT.
*    ENDIF.
    MODIFY GT_FIELDCAT FROM WA_FIELDCAT.
    CLEAR WA_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " build_fieldcat_APP246
*&---------------------------------------------------------------------*
*&      Form  set_field_info_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT  text
*----------------------------------------------------------------------*
FORM SET_FIELD_INFO_SUM_APP246 USING    L_FIELDCAT STRUCTURE LVC_S_FCAT.
  CASE L_FIELDCAT-FIELDNAME.
    WHEN 'BODYNO'.
      SET_FIELDCAT  'BODY NO' 10.
      L_FIELDCAT-KEY = 'X'.
    WHEN 'WONO'.
      SET_FIELDCAT 'Order No.' 20.
    WHEN 'EXTC'.
      SET_FIELDCAT 'Ext.C' 20.
*      l_fieldcat-key = 'X'.
    WHEN 'INTC'.
      SET_FIELDCAT 'Int.C' 20.
    WHEN 'BIN'.
      SET_FIELDCAT 'B/In' 20.
    WHEN 'PIN'.
      SET_FIELDCAT 'P/In'  20.
    WHEN 'TC'.
      SET_FIELDCAT 'T/C'  20.
    WHEN 'POUT'.
      SET_FIELDCAT 'P/Out' 20.
    WHEN 'PBSI'.
      SET_FIELDCAT 'PBS/I' 20.
    WHEN 'PBSO'.
      SET_FIELDCAT 'PBS/O' 20.
    WHEN 'TIN'.
      SET_FIELDCAT 'T/In' 20.
    WHEN 'CF'.
      SET_FIELDCAT 'C/F'  20.
    WHEN 'SOFF'.
      SET_FIELDCAT 'S/Off' 20.
    WHEN 'CONTROL'.
      SET_FIELDCAT 'C/Gate' 20.
    WHEN 'PDII'.
      SET_FIELDCAT 'UBC' 20.
    WHEN 'PDIO'.
      SET_FIELDCAT 'ALLO' 20.
    WHEN 'MP'.
      SET_FIELDCAT 'VPC/IN' 20.
  ENDCASE.
ENDFORM.                    " set_field_info_app246
*&---------------------------------------------------------------------*
*&      Form  set_data_for_sum_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATA_FOR_SUM_APP246.
  PERFORM SEARCH_SUM_DATA_APP246.
ENDFORM.                    " set_data_for_sum_app246
*&---------------------------------------------------------------------*
*&      Form  set_data_for_det_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SET_DATA_FOR_DET_APP246.
*  PERFORM SEARCH_DETAIL_DATA_APP246.
*ENDFORM.                    " set_data_for_det_app246
*&---------------------------------------------------------------------*
*&      Form  read_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_APP246_OBJEK  text
*      -->P_L_ATNAM  text
*      <--P_IT_TEMP_APP246_PIN  text
*----------------------------------------------------------------------*
FORM READ_SHOP_DATE USING    P_OBJEK
                             P_ATNAM
                    CHANGING P_DATE. "p_atflv.
  DATA: L_ATFLV   TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N         .
  SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  P_ATNAM       .

  P_DATE = L_NUM = L_ATFLV .
ENDFORM.                    " read_shop_date
*&---------------------------------------------------------------------*
*&      Form  search_detail_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SEARCH_DETAIL_DATA_APP246.
*  DATA: L_ERROR ,
*        L_TEXT(50) .
*  CLEAR L_ERROR.
*  PERFORM SET_PARAMETER_FOR_SRCHNG_DATA USING L_ERROR L_TEXT.
*  IF L_ERROR <> SPACE.
*    CONCATENATE 'Enter The Necessary Parameters!!! -' L_TEXT
*      INTO L_TEXT.
*    MESSAGE I000 WITH L_TEXT.
*    EXIT.
*  ENDIF.
*  CLEAR: IT_OBJEK, IT_OBJEK[], IT_DET_APP246, IT_DET_APP246[].
*  PERFORM GET_VEHICLE_MASTER_NO_APP246 TABLES IT_OBJEK.
**  PERFORM CREATE_DATA_FOR_DET_APP246 .
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  call_method_sum_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_METHOD_SUM_APP246.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.

  L_STRUCT = 'ZSPP_SUM_APP246'.
*-----> SET OBJECT
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = L_STRUCT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_SUM_APP246[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDFORM.                    " call_method_sum_app246
*&---------------------------------------------------------------------*
*&      Module  read_data_app246  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_DATA_APP246 INPUT.
  IF NOT ( GS_CUSTOM_CONTAINER IS INITIAL ).
    CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
    FREE  GS_CUSTOM_CONTAINER.
  ENDIF.
  PERFORM MAKE_PROGRESS_RANGE.                              "UD1K912914
  PERFORM CHECK_AND_READ_DATA_APP246 .
ENDMODULE.                 " read_data_app246  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_cursor_field_app246  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD_APP246 INPUT.
  CLEAR: WA_FNAME_TX, WA_SAVELINE_IX.
  GET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " get_cursor_field_app246  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_app246  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_APP246 INPUT.
  OK_CODE = SY-UCOMM.

  CASE OK_CODE.
*    WHEN 'OLD' OR 'NEW' OR 'PREB'.
    WHEN 'VPC' OR 'SHIP' OR 'ALLO'.
** End of change
      W_UCOMM = SY-UCOMM.
      PERFORM READ_DATA.
    WHEN 'EXIT' OR 'BACK'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      IF NOT ( GS_CUSTOM_CONTAINER IS INITIAL ).
        CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
        FREE  GS_CUSTOM_CONTAINER.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " user_command_app246  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_and_read_data_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AND_READ_DATA_APP246.
  WA_ALV_CALLED = 'X'.
  CASE P_STATUS_APP246.
    WHEN 'S'.  "Summary
      PERFORM SET_DATA_FOR_SUM_APP246.
    WHEN 'D'.  "Detail
      PERFORM SET_DATA_FOR_DET_APP246.
  ENDCASE.

ENDFORM.                    " check_and_read_data_app246
*&---------------------------------------------------------------------*
*&      Form  SEARCH_SUM_DATA_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_SUM_DATA_APP246.
  DATA: L_ERROR ,
        L_TEXT(50) .
  CLEAR L_ERROR.
  CLEAR: IT_OBJEK, IT_OBJEK[], IT_SUM_APP246, IT_SUM_APP246[].
  PERFORM GET_VEHICLE_MASTER_NO_APP246 TABLES IT_OBJEK.
  IF W_TOTAL_RECODS > 0.
    PERFORM CREATE_DATA_FOR_SUM_APP246 .
  ENDIF.
ENDFORM.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER_FOR_SRCHNG_DATA USING P_ERROR P_TEXT .
* Progress
  IF P_PROG_APP246 <> SPACE.
  ELSE.
    P_ERROR = 'X'.
    P_TEXT = 'Progress'.
    EXIT.
  ENDIF.
  IF P_PROG_APP246 GT P_PROG_APP246_H AND
     P_PROG_APP246_H <> SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'Wrong Progress Data'.
    EXIT.
  ENDIF.

ENDFORM.                    " SET_PARAMETER_FOR_SRCHNG_DATA

*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER_NO_APP246 TABLES P_IT_OBJEK STRUCTURE IT_OBJEK .

  DATA: LT_OBJEK   LIKE TABLE OF IT_OBJEK              WITH HEADER LINE,
        L_SUBRC    TYPE SY-SUBRC ,
        L_ATINN    TYPE AUSP-ATINN,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_ATFLV TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N,
        L_INDEX LIKE SY-TABIX.


  DATA: BEGIN OF IT_ATWRT OCCURS 0,
  OBJEK TYPE AUSP-OBJEK,
  ATWRT TYPE AUSP-ATWRT,
  END OF IT_ATWRT .

  DATA: BEGIN OF $IT_OBJEK OCCURS 0,
          OBJEK TYPE AUSP-OBJEK,
          ATFLV LIKE AUSP-ATFLV,
        END OF $IT_OBJEK .

*  DATA: BEGIN OF LT_ATFLV OCCURS 0,
*            OBJEK TYPE AUSP-OBJEK,
*            ATFLV LIKE AUSP-ATFLV,
*          END OF LT_ATFLV.

  DATA : $ATINN TYPE ATINN,
         $OBJEK LIKE AUSP-OBJEK,
         $ATFLV LIKE AUSP-ATFLV,
         $IX TYPE I,
         $ATWRT LIKE AUSP-ATWRT.


  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 10.

*  IF OK_CODE = 'ALLO'.
*    PERFORM READ_ATINN      USING   'P_MANUAL_ORDER'   L_ATINN  .
*    SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
*          FROM AUSP
*         WHERE ATINN = L_ATINN
*           AND KLART = '002'
*           AND ATWRT = 'M'.
*  ELSE.
*    PERFORM READ_ATINN      USING   'P_RP_STATUS'   L_ATINN  .
*    SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
*        FROM AUSP
*       WHERE ATINN = L_ATINN
*         AND KLART = '002'
*         AND ATWRT IN P_PROG.
*  ENDIF.

  PERFORM READ_ATINN      USING   'P_RP_STATUS'   L_ATINN  .
  SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
      FROM AUSP
     WHERE ATINN = L_ATINN
       AND KLART = '002'
       AND ATWRT IN P_PROG.

  PERFORM READ_ATINN      USING   'P_MANUAL_ORDER'   L_ATINN  .

  LOOP AT LT_OBJEK.
    CLEAR: L_ATFLV, L_DATUM, L_NUM.
    L_INDEX = SY-TABIX.

    SELECT SINGLE ATFLV INTO L_ATFLV
          FROM AUSP AS A
          INNER JOIN CABN AS B
          ON A~ATINN = B~ATINN
          WHERE OBJEK = LT_OBJEK-OBJEK
            AND KLART = '002'
            AND ATNAM = 'P_RP19_SHOP_DATE'.

    L_DATUM = L_NUM = L_ATFLV.
    CASE OK_CODE.
      WHEN 'VPC'.
        IF L_DATUM > C_CG_DATE.
          DELETE LT_OBJEK INDEX L_INDEX.
        ENDIF.
      WHEN 'SHIP'.
        IF L_DATUM <= C_CG_DATE.
          DELETE LT_OBJEK INDEX L_INDEX.
        ENDIF.
      WHEN 'ALLO'.
        IF L_DATUM <= C_CG_DATE.
          DELETE LT_OBJEK INDEX L_INDEX.
        ELSE.
          CLEAR: L_ATWRT.
          SELECT SINGLE ATWRT INTO L_ATWRT
              FROM AUSP
              WHERE OBJEK = LT_OBJEK-OBJEK
                AND KLART = '002'
                AND ATINN = L_ATINN.
          IF L_ATWRT <> 'M'.
            DELETE LT_OBJEK INDEX L_INDEX.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  PERFORM READ_ATINN      USING   'P_USAGE_CAR'   L_ATINN  .

  SORT  LT_OBJEK BY OBJEK.

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 20.

  SORT  LT_OBJEK BY OBJEK.

  SELECT SINGLE ATINN INTO $ATINN FROM CABN WHERE ATNAM =
  'P_DESTINATION_CODE'.

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 30.
  SELECT OBJEK ATWRT INTO ($OBJEK,$ATWRT)
   FROM AUSP
   WHERE KLART = '002'
      AND ATINN = $ATINN
      %_HINTS ORACLE 'FIRST_ROWS(1)'.
    IF $ATWRT+3(1) EQ 'A'.
      READ TABLE LT_OBJEK WITH KEY OBJEK = $OBJEK BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LT_OBJEK-ATWRT = $ATWRT.
        MODIFY LT_OBJEK INDEX SY-TABIX TRANSPORTING ATWRT.
        $IT_OBJEK-OBJEK = $OBJEK.
        APPEND $IT_OBJEK.
      ENDIF.
    ENDIF.
    IF $ATWRT+0(3) <> 'B28'.
      READ TABLE LT_OBJEK WITH KEY OBJEK = $OBJEK BINARY SEARCH.
      IF SY-SUBRC = 0.
        DELETE LT_OBJEK INDEX SY-TABIX.
      ENDIF.
    ENDIF.
  ENDSELECT.

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 40.

  SORT $IT_OBJEK.
  LOOP AT LT_OBJEK.
    $IX = SY-TABIX.
    READ TABLE $IT_OBJEK WITH KEY OBJEK = LT_OBJEK-OBJEK
    BINARY SEARCH.
    IF SY-SUBRC NE 0.
      DELETE LT_OBJEK INDEX $IX.
    ENDIF.
  ENDLOOP.

  SORT  LT_OBJEK BY OBJEK.

  CLEAR : $IT_OBJEK[], $IT_OBJEK.

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 50.
  SELECT OBJEK ATWRT INTO ($OBJEK,$ATWRT)
   FROM AUSP
   WHERE KLART = '002'
      AND ATINN = L_ATINN
      AND ATWRT IN ('S', 'D')
      %_HINTS ORACLE 'FIRST_ROWS(1)'.

    READ TABLE LT_OBJEK WITH KEY OBJEK = $OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      $IT_OBJEK-OBJEK = $OBJEK.
      APPEND $IT_OBJEK.
    ENDIF.
  ENDSELECT.

  SORT $IT_OBJEK.

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 60.
  LOOP AT LT_OBJEK.
    READ TABLE $IT_OBJEK WITH KEY OBJEK = LT_OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
    ELSE.
      IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.
      IT_OBJEK-ATWRT = LT_OBJEK-ATWRT.
      APPEND IT_OBJEK.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_OBJEK LINES W_TOTAL_RECODS.
*  IF W_TOTAL_RECODS > 0.

*    PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 70.
*    IF W_UCOMM = 'ALLO'.
*      SELECT SINGLE ATINN INTO L_ATINN FROM CABN WHERE ATNAM =
*        'P_RP21_SHOP_DATE'.
*      SELECT OBJEK ATFLV INTO TABLE LT_ATFLV
*            FROM AUSP
*           FOR ALL ENTRIES IN IT_OBJEK
*            WHERE KLART = '002'
*             AND ATINN = L_ATINN
*         AND OBJEK = IT_OBJEK-OBJEK
*         %_HINTS ORACLE 'FIRST_ROWS(1)'.
*      SORT LT_ATFLV BY OBJEK.
*
*      LOOP AT IT_OBJEK.
*      READ TABLE LT_ATFLV WITH KEY OBJEK = IT_OBJEK-OBJEK BINARY SEARCH
*.
*        IF SY-SUBRC = 0.
*          L_DATUM = L_NUM = LT_ATFLV-ATFLV .
*          IF L_DATUM IS INITIAL.
*            DELETE IT_OBJEK.
*          ENDIF.
*        ELSE.
*          DELETE IT_OBJEK.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 80.

  SORT IT_OBJEK BY OBJEK .

ENDFORM.                    " get_vehicle_master_no
*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DATA_FOR_SUM_APP246.

  DATA: L_RPNO(02) TYPE N          ,
        L_ATNAM    TYPE CABN-ATNAM ,
        L_ATWRT    TYPE AUSP-ATWRT .
  DATA $IX TYPE I.
  DATA L_NUM(08) TYPE N .
  DATA DATE_MAX LIKE AUSP-ATFLV.

  CLEAR: IT_SUM_APP246, IT_SUM_APP246[].

  __CLS IT_SUM_APP246.

  SORT IT_OBJEK BY OBJEK.

  PERFORM SHOW_PROGRESS USING 'Filling... Model.........1/19' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_MODEL USING 'P_MODEL'.

  LOOP AT IT_OBJEK.
    MOVE-CORRESPONDING IT_OBJEK TO IT_SUM_APP246.
    READ TABLE TAB_MODEL WITH KEY OBJEK = IT_OBJEK-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-MODEL = TAB_MODEL-ATWRT.
      APPEND IT_SUM_APP246.
    ENDIF.
  ENDLOOP.

  __CLS TAB_MODEL.

  PERFORM SHOW_PROGRESS USING 'Filling... Body Searial...2/19' 0..

  PERFORM READ_NORMAL_NEW TABLES TAB_BODY_SERIAL USING 'P_BODY_SERIAL'.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_BODY_SERIAL
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-BODYNO = TAB_BODY_SERIAL-ATWRT.
      CONCATENATE IT_SUM_APP246-MODEL IT_SUM_APP246-BODYNO
      INTO IT_SUM_APP246-BODYNO .
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING BODYNO.
    ENDIF.
  ENDLOOP.

  __CLS TAB_BODY_SERIAL.

  PERFORM SHOW_PROGRESS USING 'Filling... Work Order......3/19' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_WORK_ORDER USING 'P_WORK_ORDER'.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_WORK_ORDER
     WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY
 SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-WONO = TAB_WORK_ORDER-ATWRT.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING WONO.
    ENDIF.
  ENDLOOP.

  __CLS TAB_WORK_ORDER.

  PERFORM SHOW_PROGRESS USING 'Filling... Ext. Color......4/19' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_EXT_COLOR USING 'P_EXT_COLOR'.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_EXT_COLOR
     WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY
 SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-EXTC = TAB_EXT_COLOR-ATWRT.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING EXTC.
    ENDIF.
  ENDLOOP.

  __CLS TAB_EXT_COLOR.

  PERFORM SHOW_PROGRESS USING 'Filling... Int. Color......5/19' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_INT_COLOR USING 'P_INT_COLOR'.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_INT_COLOR
    WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-INTC = TAB_INT_COLOR-ATWRT.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING INTC.
    ENDIF.
  ENDLOOP.

  __CLS TAB_INT_COLOR.

  PERFORM SHOW_PROGRESS USING 'Filling... Plan Order......6/19' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_PLAN_ORDER USING 'P_PLAN_ORDER'.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_PLAN_ORDER
         WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-PLNUM = TAB_PLAN_ORDER-ATWRT.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING PLNUM.
    ENDIF.
  ENDLOOP.

  __CLS TAB_PLAN_ORDER.

  PERFORM SHOW_PROGRESS USING 'Filling... B/In Date.......7/19' 0..

  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP01_SHOP_DATE
                              USING 'P_RP01_SHOP_DATE'
                           CHANGING DATE_MAX.

  LOOP AT IT_SUM_APP246.

    $IX = SY-TABIX.
    READ TABLE TAB_RP01_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-BIN = L_NUM = TAB_RP01_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING BIN.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP01_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... P/In Date.......8/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP02_SHOP_DATE
                              USING 'P_RP02_SHOP_DATE'
                           CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP02_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-PIN = L_NUM = TAB_RP02_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING PIN.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP02_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... T/C Date........9/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP03_SHOP_DATE
                              USING 'P_RP03_SHOP_DATE'
                           CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP03_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-TC = L_NUM = TAB_RP03_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING TC.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP03_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... P/Out Date.....10/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP04_SHOP_DATE
                              USING 'P_RP04_SHOP_DATE'
                           CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP04_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-POUT = L_NUM = TAB_RP04_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING POUT.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP04_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... PBS/I Date......11/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP05_SHOP_DATE
                              USING 'P_RP05_SHOP_DATE'
                           CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP05_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-PBSI = L_NUM = TAB_RP05_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING PBSI.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP05_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... PBS/O Date......12/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP06_SHOP_DATE
                              USING 'P_RP06_SHOP_DATE'
                           CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP06_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-PBSO = L_NUM = TAB_RP06_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING PBSO.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP06_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... T/In Date.......13/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP07_SHOP_DATE
                              USING 'P_RP07_SHOP_DATE'
                           CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP07_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-TIN = L_NUM = TAB_RP07_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING TIN.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP07_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... C/F Date........14/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP17_SHOP_DATE
                                USING 'P_RP17_SHOP_DATE'
                             CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP17_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-CF = L_NUM = TAB_RP17_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING CF.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP17_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... S/Off Date......15/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP18_SHOP_DATE
                                USING 'P_RP18_SHOP_DATE'
                             CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP18_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-SOFF = L_NUM = TAB_RP18_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING SOFF.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP18_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... C/Gate Date.....16/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP19_SHOP_DATE
                                USING 'P_RP19_SHOP_DATE'
                             CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP19_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-CONTROL = L_NUM = TAB_RP19_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING CONTROL.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP19_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... UBC Date........17/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP20_SHOP_DATE
                                USING 'P_RP20_SHOP_DATE'
                             CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP20_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-PDII = L_NUM = TAB_RP20_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING PDII.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP20_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... AllO Date.......18/19' 0..
  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP21_SHOP_DATE
                                USING 'P_RP21_SHOP_DATE'
                             CHANGING DATE_MAX.
  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP21_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-PDIO = L_NUM = TAB_RP21_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING PDIO.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP21_SHOP_DATE.

  PERFORM SHOW_PROGRESS USING 'Filling... VPC/In Date.....19/19' 0..

  PERFORM READ_SHOP_DATE_NEW TABLES TAB_RP22_SHOP_DATE
                                USING 'P_RP22_SHOP_DATE'
                             CHANGING DATE_MAX.


  LOOP AT IT_SUM_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP22_SHOP_DATE
      WITH KEY OBJEK = IT_SUM_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_SUM_APP246-MP = L_NUM = TAB_RP22_SHOP_DATE-ATFLV.
      MODIFY IT_SUM_APP246 INDEX $IX TRANSPORTING MP.
    ENDIF.
  ENDLOOP.
  __CLS : TAB_RP22_SHOP_DATE.

  SORT IT_SUM_APP246 BY BODYNO .
  DESCRIBE TABLE IT_SUM_APP246 LINES P_TOTAL_APP246.

  PERFORM SHOW_PROGRESS USING '...Final preparation...' 0.

ENDFORM.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  make_progress_range
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_PROGRESS_RANGE.

** Changed by Furong on 07/30/09
  CLEAR: P_PROG, P_PROG[].
*  p_prog-sign   = 'I'.
*  p_prog-option = 'BT'.
*  p_prog-low    = '19'.
***  FOR TESTING
**  P_PROG-HIGH    = '19'.
*  p_prog-high   = '22'.
*
*  APPEND p_prog.

  CASE W_UCOMM.
    WHEN 'VPC'.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'BT'.
      P_PROG-LOW    = '19'.
      P_PROG-HIGH   = '24'.
      APPEND P_PROG.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'EQ'.
      P_PROG-LOW    = '26'.
      P_PROG-HIGH   = ''.
      APPEND P_PROG.

    WHEN 'SHIP'.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'BT'.
      P_PROG-LOW    = '19'.
      P_PROG-HIGH   = '22'.
      APPEND P_PROG.

*      P_PROG-SIGN   = 'I'.
*      P_PROG-OPTION = 'EQ'.
*      P_PROG-LOW    = '24'.
*      APPEND P_PROG.
*      P_PROG-SIGN   = 'I'.
*      P_PROG-OPTION = 'EQ'.
*      P_PROG-LOW    = '26'.
*      APPEND P_PROG.

    WHEN 'ALLO'.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'BT'.
      P_PROG-LOW    = '19'.
      P_PROG-HIGH   = '23'.
      APPEND P_PROG.

*      P_PROG-SIGN   = 'I'.
*      P_PROG-OPTION = 'BT'.
*      P_PROG-LOW    = '1'.
*      P_PROG-HIGH   = '24'.
*      APPEND P_PROG.
*      P_PROG-SIGN   = 'I'.
*      P_PROG-OPTION = 'EQ'.
*      P_PROG-LOW    = '26'.
*      P_PROG-HIGH   = ''.
*      APPEND P_PROG.

*    WHEN 'VPC' OR 'SHIP'.
*      P_PROG-SIGN   = 'I'.
*      P_PROG-OPTION = 'BT'.
*      P_PROG-LOW    = '19'.
*      P_PROG-HIGH   = '24'.
*      APPEND P_PROG.
*      P_PROG-SIGN   = 'I'.
*      P_PROG-OPTION = 'EQ'.
*      P_PROG-LOW    = '26'.
*      P_PROG-HIGH   = ''.
*      APPEND P_PROG.

  ENDCASE.
** end of change on 07/30/09

ENDFORM.                    " make_progress_range
*&---------------------------------------------------------------------*
*&      Form  READ_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0834   text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
FORM READ_ATINN USING     PA_CHAR  PA_ATINN.
  SELECT SINGLE ATINN INTO PA_ATINN
     FROM CABN
    WHERE ATNAM = PA_CHAR.

ENDFORM.                    " READ_ATINN

*---------------------------------------------------------------------*
*       FORM READ_NORMAL_CLASSIFICATION                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VMNO                                                        *
*  -->  P_CHAR                                                        *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM READ_NORMAL_CLASSIFICATION USING    P_VMNO
                                         P_CHAR
                                CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .

ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  check_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_0827   text
*      -->P_P_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_DATA_OF_VM USING    P_VMNO
                               P_CHAR
                               P_VALUE
                      CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO         AND
          KLART = '002'          AND
          AU~ATWRT = P_VALUE     AND
          CA~ATNAM = P_CHAR      .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " check_data_of_v
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  IF NOT ( GS_CUSTOM_CONTAINER IS INITIAL ).
    CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
    FREE  GS_CUSTOM_CONTAINER.
  ENDIF.

* by ig.moon 6/16/2009 {
** Changed by Furong on 07/30/09
*  IF ok_code NE 'PREB'.
** End of change
* }
  PERFORM MAKE_PROGRESS_RANGE.                              "UD1K912914
  PERFORM CHECK_AND_READ_DATA_APP246 .
** Changed by Furong on 07/30/09
*  ELSE.
** by ig.moon 6/16/2009 { for Pre Billing
*
*    wa_alv_called = 'X'.
*    CASE p_status_app246.
*      WHEN 'S'.  "Summary
*        __cls : it_objek, it_sum_app246.
*        PERFORM get_vehicle_master_no_pre_bill.
*        READ TABLE it_objek INDEX 1.
*        IF sy-subrc NE 0.
*          MESSAGE s000 WITH 'No data.'.
*          EXIT.
*        ENDIF.
*        PERFORM create_data_for_sum_app246 .
*      WHEN 'D'.  "Detail
*        __cls : it_objek, it_det_app246.
*        PERFORM get_vehicle_master_no_pre_bill.
*        READ TABLE it_objek INDEX 1.
*        IF sy-subrc NE 0.
*          MESSAGE s000 WITH 'No data.'.
*          EXIT.
*        ENDIF.
*        PERFORM create_data_for_det_app246 .
*    ENDCASE.
*
** }
*  ENDIF.
** End of change
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'ST9000'.
  SET TITLEBAR 'T9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_DATA_FOR_DET_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATA_FOR_DET_APP246.
  PERFORM SEARCH_DETAIL_DATA_APP246.
ENDFORM.                    " SET_DATA_FOR_DET_APP246
*&---------------------------------------------------------------------*
*&      Form  SEARCH_DETAIL_DATA_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DETAIL_DATA_APP246.
  DATA: L_ERROR ,
      L_TEXT(50) .
  CLEAR L_ERROR.

  CLEAR: IT_OBJEK, IT_OBJEK[], IT_DET_APP246, IT_DET_APP246[].
  PERFORM GET_VEHICLE_MASTER_NO_APP246 TABLES IT_OBJEK.
  PERFORM CREATE_DATA_FOR_DET_APP246 .

ENDFORM.                    " SEARCH_DETAIL_DATA_APP246
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA_FOR_DET_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DATA_FOR_DET_APP246.

  DATA: L_ATNAM    TYPE CABN-ATNAM ,
         L_ATWRT    TYPE AUSP-ATWRT .
  DATA $IX LIKE SY-TABIX.
  __CLS IT_DET_APP246.

  SORT IT_OBJEK BY OBJEK.

  PERFORM SHOW_PROGRESS USING 'Filling... Model.........1/13' 0..

  PERFORM READ_NORMAL_NEW TABLES TAB_MODEL USING 'P_MODEL'.

  LOOP AT IT_OBJEK.
    MOVE-CORRESPONDING IT_OBJEK TO IT_DET_APP246.
    READ TABLE TAB_MODEL WITH KEY OBJEK = IT_OBJEK-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-MODEL = TAB_MODEL-ATWRT.
*      it_det_app246-rp = it_objek-atwrt.
      APPEND IT_DET_APP246.
    ENDIF.
  ENDLOOP.

  __CLS TAB_MODEL.

  PERFORM SHOW_PROGRESS USING 'Filling... Body Searial...2/13' 0..

  PERFORM READ_NORMAL_NEW TABLES TAB_BODY_SERIAL USING 'P_BODY_SERIAL'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_BODY_SERIAL
      WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-BODYNO = TAB_BODY_SERIAL-ATWRT.
      CONCATENATE IT_DET_APP246-MODEL IT_DET_APP246-BODYNO
      INTO IT_DET_APP246-BODYNO .
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING BODYNO.
    ENDIF.
  ENDLOOP.

  __CLS TAB_BODY_SERIAL.

  PERFORM SHOW_PROGRESS USING 'Filling... Work Order.....3/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_WORK_ORDER USING 'P_WORK_ORDER'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_WORK_ORDER
     WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY
 SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-WONO = TAB_WORK_ORDER-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING WONO.
    ENDIF.
  ENDLOOP.

  __CLS TAB_WORK_ORDER.

  PERFORM SHOW_PROGRESS USING 'Filling... Ext. Color.....4/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_EXT_COLOR USING 'P_EXT_COLOR'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_EXT_COLOR
     WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY
 SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-EXTC = TAB_EXT_COLOR-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING EXTC.
    ENDIF.
  ENDLOOP.

  __CLS TAB_EXT_COLOR.

  PERFORM SHOW_PROGRESS USING 'Filling... Int. Color.....5/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_INT_COLOR USING 'P_INT_COLOR'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_INT_COLOR
    WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-INTC = TAB_INT_COLOR-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING INTC.
    ENDIF.
  ENDLOOP.

  __CLS TAB_INT_COLOR.

  PERFORM SHOW_PROGRESS USING 'Filling... VIN............6/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_VIN USING 'P_VIN'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_VIN
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-VIN = TAB_VIN-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING VIN.
    ENDIF.
  ENDLOOP.
  __CLS TAB_VIN.

  PERFORM SHOW_PROGRESS USING 'Filling... MI.............7/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_MI USING 'P_MI'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_MI
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-MI = TAB_MI-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING MI.
    ENDIF.
  ENDLOOP.
  __CLS TAB_MI.

  PERFORM SHOW_PROGRESS USING 'Filling... OCN............8/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_OCN USING 'P_OCN'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_OCN
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-OCN = TAB_OCN-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING OCN.
    ENDIF.
  ENDLOOP.
  __CLS TAB_OCN.

  PERFORM SHOW_PROGRESS USING 'Filling... Dearler........9/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_DEALER USING 'P_DEALER_NO'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_DEALER
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-DEALER = TAB_DEALER-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING DEALER.
    ENDIF.
  ENDLOOP.
  __CLS TAB_DEALER.

  PERFORM SHOW_PROGRESS USING 'Filling... Plan Order....10/13' 0..
  PERFORM READ_NORMAL_NEW TABLES TAB_PLAN_ORDER USING 'P_PLAN_ORDER'.
  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_PLAN_ORDER
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-PLNUM = TAB_PLAN_ORDER-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING PLNUM.
    ENDIF.
  ENDLOOP.

  __CLS TAB_PLAN_ORDER.

  PERFORM SHOW_PROGRESS USING 'Filling... RP...........11/13' 0..

  PERFORM READ_NORMAL_NEW TABLES TAB_RP_STATUS USING 'P_RP_STATUS'.

  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP_STATUS
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-RP = TAB_RP_STATUS-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING RP.
    ENDIF.
  ENDLOOP.

  PERFORM SHOW_PROGRESS USING 'Filling... Date Time.....12/13' 0..

  PERFORM READ_NORMAL_NEW TABLES TAB_RP_DATE_TIME
  USING 'P_RP19_ACTUAL_DATE'.

  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_RP_DATE_TIME
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-REP_DATE =  TAB_RP_DATE_TIME-ATWRT+00(08).
      IT_DET_APP246-REP_TIME = TAB_RP_DATE_TIME-ATWRT+08(06).
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING REP_DATE REP_TIME.
    ENDIF.
  ENDLOOP.

  PERFORM SHOW_PROGRESS USING 'Filling... Serial.........13/13' 0..

  PERFORM READ_NORMAL_NEW TABLES TAB_SERIAL
  USING 'P_RP19_SERIAL'.

  LOOP AT IT_DET_APP246.
    $IX = SY-TABIX.
    READ TABLE TAB_SERIAL
         WITH KEY OBJEK = IT_DET_APP246-OBJEK BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DET_APP246-SERIAL =  TAB_SERIAL-ATWRT.
      MODIFY IT_DET_APP246 INDEX $IX TRANSPORTING SERIAL.
    ENDIF.
  ENDLOOP.

  PERFORM SHOW_PROGRESS USING '...Final Preparation............' 0..
  SORT IT_DET_APP246 BY BODYNO .
  DESCRIBE TABLE IT_DET_APP246 LINES P_TOTAL_APP246.

ENDFORM.                    " CREATE_DATA_FOR_DET_APP246
*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX_APP246.
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_STATUS_APP246'.
  PERFORM SET_FIELD_STATUS_APP246.
  PERFORM CALL_FUNCTION_VRM  USING XLIST.
ENDFORM.                    " make_dropdown_list_box_app246
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_VRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XLIST  text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_VRM USING    P_LIST.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = NAME
            VALUES = P_LIST.
ENDFORM.                    " CALL_FUNCTION_VRM
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_STATUS_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_STATUS_APP246.
  XVALUE-KEY = 'S'.
  XVALUE-TEXT = 'All Point'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY = 'D'.
  XVALUE-TEXT = 'Detail'.
  APPEND XVALUE TO XLIST.

ENDFORM.                    " SET_FIELD_STATUS_APP246
*&---------------------------------------------------------------------*
*&      Form  call_method_det_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_METHOD_DET_APP246.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.

  L_STRUCT = 'ZSPP_DET_APP246'.
*-----> SET OBJECT
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = L_STRUCT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_DET_APP246[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4 .
ENDFORM.                    " call_method_det_app246
*&---------------------------------------------------------------------*
*&      Form  read_normal_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1278   text
*----------------------------------------------------------------------*
FORM READ_NORMAL_NEW TABLES TAB_VALUE STRUCTURE GT_VALUE
                      USING P_CLASS.

  DATA $OBJEK LIKE AUSP-OBJEK.
  DATA $ATWRT LIKE AUSP-ATWRT.
  DATA $ATINN TYPE AUSP-ATINN.
  __CLS TAB_VALUE.

  SELECT SINGLE ATINN INTO $ATINN FROM CABN WHERE ATNAM = P_CLASS.

  SELECT OBJEK ATWRT INTO TABLE TAB_VALUE
   FROM AUSP
   FOR ALL ENTRIES IN IT_OBJEK
   WHERE KLART = '002'
      AND ATINN = $ATINN
      AND OBJEK = IT_OBJEK-OBJEK
      %_HINTS ORACLE 'FIRST_ROWS(1)'.

*  SELECT objek atwrt INTO ($objek,$atwrt)
*   FROM ausp
*   WHERE klart = '002'
*      AND atinn = $atinn
*      %_HINTS ORACLE 'FIRST_ROWS(1)'.
*    READ TABLE it_objek WITH KEY objek = $objek BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      tab_value-objek = $objek.
*      tab_value-atwrt = $atwrt.
*      APPEND tab_value.
*    ENDIF.
*  ENDSELECT.

  SORT TAB_VALUE BY OBJEK.

ENDFORM.                    " read_normal_new
*&---------------------------------------------------------------------*
*&      Form  read_shop_date_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TAB_RP01_SHOP_DATE  text
*      -->P_1658   text
*----------------------------------------------------------------------*
FORM READ_SHOP_DATE_NEW TABLES TAB_VALUE
                            STRUCTURE TAB_RP01_SHOP_DATE
                        USING  P_CLASS
                     CHANGING P_MAXDATE.

  DATA $OBJEK LIKE AUSP-OBJEK.
  DATA $ATFLV LIKE AUSP-ATFLV.

  DATA $ATINN TYPE AUSP-ATINN.
  __CLS TAB_VALUE.

  IF P_MAXDATE IS INITIAL.
    P_MAXDATE = '00000000'.
  ENDIF.

  SELECT SINGLE ATINN INTO $ATINN FROM CABN WHERE ATNAM = P_CLASS.

  SELECT OBJEK ATFLV INTO TABLE TAB_VALUE
   FROM AUSP
   FOR ALL ENTRIES IN IT_OBJEK
   WHERE KLART = '002'
      AND ATINN = $ATINN
      AND OBJEK = IT_OBJEK-OBJEK
      AND ATFLV >= P_MAXDATE
     %_HINTS ORACLE 'FIRST_ROWS(1)'.

*  SELECT objek atflv INTO ($objek,$atflv)
*   FROM ausp
*   WHERE klart = '002'
*      AND atinn = $atinn
*      AND atflv >= p_maxdate
*     %_HINTS ORACLE 'FIRST_ROWS(1)'.
*    READ TABLE it_objek WITH KEY objek = $objek BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      tab_value-objek = $objek.
*      tab_value-atflv = $atflv.
*      APPEND tab_value.
*    ENDIF.
*  ENDSELECT.

  IF P_MAXDATE IS INITIAL.
    SORT TAB_VALUE BY ATFLV.
    READ TABLE TAB_VALUE INDEX 1.
    P_MAXDATE = TAB_VALUE-ATFLV.
  ENDIF.

  SORT TAB_VALUE BY OBJEK.

ENDFORM.                    " read_shop_date_new

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    PF_TEXT
                            VALUE(PF_VAL).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PF_VAL
            TEXT       = PF_TEXT.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no_pre_bill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER_NO_PRE_BILL.

  DATA: LT_OBJEK   LIKE TABLE OF IT_OBJEK              WITH HEADER LINE,
        L_SUBRC    TYPE SY-SUBRC ,
        L_ATINN    TYPE AUSP-ATINN,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_ATFLV_ST TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N,
       $LT_OBJEK   LIKE TABLE OF IT_OBJEK WITH HEADER LINE.

  DATA: BEGIN OF IT_ATWRT OCCURS 0,
          OBJEK TYPE AUSP-OBJEK,
          ATWRT TYPE AUSP-ATWRT,
        END OF IT_ATWRT .

  DATA: BEGIN OF $IT_OBJEK OCCURS 0,
          OBJEK TYPE AUSP-OBJEK,
          ATFLV LIKE AUSP-ATFLV,
        END OF $IT_OBJEK .

  DATA : $ATINN TYPE ATINN,
         $OBJEK LIKE AUSP-OBJEK,
         $ATFLV LIKE AUSP-ATFLV,
         $IX TYPE I,
         $ATWRT LIKE AUSP-ATWRT.

  PERFORM READ_ATINN      USING   'P_RP_STATUS'   L_ATINN  .

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 10.

  SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
    FROM AUSP
   WHERE ATINN = L_ATINN
     AND KLART = '002'
     AND ATWRT EQ '23'.

  SORT  LT_OBJEK BY OBJEK.

  PERFORM READ_ATINN      USING   'P_MANUAL_ORDER'   L_ATINN  .

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 20.

  SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE $LT_OBJEK
    FROM AUSP
    FOR ALL ENTRIES IN LT_OBJEK
   WHERE ATINN = L_ATINN
     AND KLART = '002'
     AND OBJEK = LT_OBJEK-OBJEK
     AND ATWRT EQ 'M'
     %_HINTS ORACLE 'FIRST_ROWS(1)'.

  PERFORM SHOW_PROGRESS USING 'Gathering... Object Number...' 70.
  __CLS IT_OBJEK.

  IT_OBJEK[] = $LT_OBJEK[].

  SORT IT_OBJEK BY OBJEK .

ENDFORM.                    " get_vehicle_master_no_pre_bill
