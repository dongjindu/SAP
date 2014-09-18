************************************************************************
* Program Name      : ZRPP_VPC_VEH_LIST
* Creation Date     :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 11/02/07        Furong           UD1K942044     Change/Dispaly mode
************************************************************************

REPORT ZRPP_VPC_VEH_LIST NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS, VRM.
CONSTANTS:  C_CG_DATE TYPE D VALUE '20090531'.
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

DATA: BEGIN OF IT_OBJEK OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF IT_OBJEK .

DATA:  WA_ALV_CALLED,
      OK_CODE LIKE SY-UCOMM,
      W_UCOMM LIKE SY-UCOMM.
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
    WHEN 'VPC' OR 'SHIP' OR 'ALLO'.
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
*  PERFORM SET_PARAMETER_FOR_SRCHNG_DATA USING L_ERROR L_TEXT.
*  IF L_ERROR <> SPACE.
*    CONCATENATE 'Enter The Necessary Parameters!!! -' L_TEXT
*      INTO L_TEXT.
*    MESSAGE I000 WITH L_TEXT.
*    EXIT.
*  ENDIF.
  CLEAR: IT_OBJEK, IT_OBJEK[], IT_SUM_APP246, IT_SUM_APP246[].
  PERFORM GET_VEHICLE_MASTER_NO_APP246 TABLES IT_OBJEK.
  PERFORM CREATE_DATA_FOR_SUM_APP246 .
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
        L_NUM(08) TYPE N.
* r_prog FOR P_PROG_APP246,       "P_RP_STATUS

  PERFORM SHOW_PROGRESS USING 'Collecting Object Number..' 1.

  PERFORM READ_ATINN      USING   'P_RP_STATUS'   L_ATINN  .

  SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
    FROM AUSP
   WHERE ATINN = L_ATINN
     AND KLART = '002'
      AND ATWRT IN P_PROG.

  " Appending the Other Point for the Processing...
*  CASE P_PROG_APP246 .
*    WHEN '07'.
**      IF p_prog_app246_h LT '17'.
*"UD1K912914
**        SELECT objek atwrt APPENDING TABLE lt_objek
**          FROM ausp
**         WHERE atinn = l_atinn
**           AND klart = '002'
**           AND atwrt IN
**           ('08', '09', '10', '11', '12', '13', '14', '15', '16').
**      ENDIF.
*"UD1K912914
*    WHEN '17'.
**      SELECT objek atwrt APPENDING TABLE lt_objek
**        FROM ausp
**       WHERE atinn = l_atinn
**         AND klart = '002'
**         AND atwrt IN ('10', '11', '12', '13', '14', '15', '16').
*    WHEN '24'.
*      SELECT OBJEK ATWRT APPENDING TABLE LT_OBJEK
*        FROM AUSP
*       WHERE ATINN = L_ATINN
*         AND KLART = '002'
*         AND ATWRT EQ '26'.
*    WHEN '25'.
*      SELECT OBJEK ATWRT APPENDING TABLE LT_OBJEK
*        FROM AUSP
*       WHERE ATINN = L_ATINN
*         AND KLART = '002'
*         AND ATWRT EQ '27'.
*
*  ENDCASE.

  PERFORM SHOW_PROGRESS USING 'Collecting Object ..' 15.
  PERFORM READ_ATINN      USING   'P_USAGE_CAR'   L_ATINN  .
  LOOP AT LT_OBJEK.

    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.
    IT_OBJEK-ATWRT = LT_OBJEK-ATWRT.
    CLEAR L_SUBRC .

** Changed by Furong on 07/30/09
*    PERFORM READ_SHOP_DATE USING    IT_OBJEK-OBJEK
*                               'P_RP19_SHOP_DATE'
*
*                         CHANGING L_DATUM.
*
*    IF W_UCOMM = 'OLD'.
*      IF L_DATUM > C_CG_DATE.
*        CONTINUE.
*      ENDIF.
*    ELSEIF W_UCOMM = 'NEW'.
*      IF L_DATUM <= C_CG_DATE.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
** End of change

    CLEAR L_SUBRC .

    SELECT SINGLE ATWRT
       INTO IT_OBJEK-ATWRT
       FROM AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
       WHERE KLART = '002'
            AND OBJEK = IT_OBJEK-OBJEK
            AND CA~ATNAM = 'P_DESTINATION_CODE'.

    IF IT_OBJEK-ATWRT+3(1) <> 'A'.
      CONTINUE.
    ENDIF.

    IF W_UCOMM = 'ALLO'.
      SELECT SINGLE ATFLV INTO L_ATFLV
         FROM AUSP AS AU
           INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
          WHERE KLART = '002'
              AND OBJEK = IT_OBJEK-OBJEK
              AND CA~ATNAM = 'P_RP21_SHOP_DATE'.

      L_DATUM = L_NUM = L_ATFLV .

      IF  L_DATUM IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.
***    P_BODYNO_APP246,     "P_BODY_SERIAL
*    IF P_BODYNO_APP246 <> SPACE.
*      CLEAR L_SUBRC .
*      MOVE P_BODYNO_APP246 TO L_ATWRT.
*      PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
*                                     'P_BODY_SERIAL'
*                                     L_ATWRT
*                               CHANGING L_SUBRC.
*      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
*    ENDIF.
***    P_WONO_APP246,       "P_WORK_ORDER
*    IF P_WONO_APP246 <> SPACE.
*      CLEAR L_SUBRC .
*      MOVE P_WONO_APP246 TO L_ATWRT .
*      PERFORM CHECK_DATA_OF_VM USING    IT_OBJEK-OBJEK
*                                      'P_WORK_ORDER'
*                                      L_ATWRT
*                             CHANGING L_SUBRC .
*      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
*    ENDIF.
***    P_EXTC_APP246,       "P_EXT_COLOR
*    IF P_EXTC_APP246 <> SPACE.
*      CLEAR L_SUBRC .
*      MOVE P_EXTC_APP246 TO L_ATWRT .
*      PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
*                                     'P_EXT_COLOR'
*                                     L_ATWRT
*                               CHANGING L_SUBRC .
*      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
*    ENDIF.
***    P_INTC_APP246.       "P_INT_COLOR
*    IF P_INTC_APP246 <> SPACE.
*      CLEAR L_SUBRC .
*      MOVE P_INTC_APP246 TO L_ATWRT .
*      PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
*                                     'P_INT_COLOR'
*                                     L_ATWRT
*                               CHANGING L_SUBRC .
*      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
*    ENDIF.
***    p_column01 ~ 10  "P_219_xxx
*    PERFORM CHECK_219_CODE USING    IT_OBJEK-OBJEK
*                           CHANGING L_SUBRC .
*    IF L_SUBRC <> 0.   CONTINUE.   ENDIF.

    " Eliminate the Scrap / Disposal Car.
    SELECT SINGLE OBJEK INTO LT_OBJEK-OBJEK
      FROM AUSP
     WHERE OBJEK = LT_OBJEK-OBJEK
       AND ATINN = L_ATINN
       AND KLART = '002'
       AND ATWRT IN ('S', 'D').

    IF SY-SUBRC = 0.   CONTINUE.   ENDIF.
    APPEND IT_OBJEK.
  ENDLOOP.

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
        L_ATWRT    TYPE AUSP-ATWRT ,
        L_LINES TYPE I,
        L_HALF TYPE I,
        L_QUARTER TYPE I,
        L_TABIX LIKE SY-TABIX.

  CLEAR: IT_SUM_APP246, IT_SUM_APP246[].

  PERFORM SHOW_PROGRESS USING 'Getting Characteristc Data ...' '30'.
  DESCRIBE TABLE IT_OBJEK LINES L_LINES.
  L_HALF = L_LINES / 2.
  L_QUARTER = L_HALF + L_HALF / 2.
  LOOP AT IT_OBJEK.
    L_TABIX = SY-TABIX.
    IF L_TABIX = L_HALF.
      PERFORM SHOW_PROGRESS USING 'Getting Characteristc Data ...' '70'.
    ENDIF.
    IF L_TABIX = L_QUARTER.
      PERFORM SHOW_PROGRESS USING 'Getting Characteristc Data ...' '85'.
    ENDIF.
    CLEAR IT_SUM_APP246.
*   V/M No.
    MOVE-CORRESPONDING IT_OBJEK TO IT_SUM_APP246.
*   Model
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_SUM_APP246-OBJEK
                                             'P_MODEL'
                                       CHANGING IT_SUM_APP246-MODEL .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_SUM_APP246-OBJEK
                                             'P_BODY_SERIAL'
                                       CHANGING IT_SUM_APP246-BODYNO .
    CONCATENATE IT_SUM_APP246-MODEL IT_SUM_APP246-BODYNO
      INTO IT_SUM_APP246-BODYNO .
*   Work Order(Serial)
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_SUM_APP246-OBJEK
                                                'P_WORK_ORDER'
                                       CHANGING IT_SUM_APP246-WONO.
*   External Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_SUM_APP246-OBJEK
                                                'P_EXT_COLOR'
                                        CHANGING IT_SUM_APP246-EXTC.
*   Internal Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_SUM_APP246-OBJEK
                                                'P_INT_COLOR'
                                       CHANGING IT_SUM_APP246-INTC.

    L_ATNAM = 'P_PLAN_ORDER'.
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_SUM_APP246-OBJEK
                                             L_ATNAM
                                    CHANGING IT_SUM_APP246-PLNUM.
**  Date : B/In
    L_ATNAM = 'P_RP01_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-BIN.
**  Date : P/In
    L_ATNAM = 'P_RP02_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PIN.
**  Date : T/C
    L_ATNAM = 'P_RP03_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-TC.
**  Date : P/OUT
    L_ATNAM = 'P_RP04_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-POUT.
**  Date : PBS/I
    L_ATNAM = 'P_RP05_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PBSI.
**  Date : PBS/OUT
    L_ATNAM = 'P_RP06_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PBSO.
**  Date : T/IN
    L_ATNAM = 'P_RP07_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-TIN.
**  Date : C/F
    L_ATNAM = 'P_RP17_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-CF.
**  Date : S/OFF
    L_ATNAM = 'P_RP18_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-SOFF.
**  Date : C/GATE
    L_ATNAM = 'P_RP19_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-CONTROL.

**  Date : UBC
    L_ATNAM = 'P_RP20_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PDII.

**  Date : ALLO
    L_ATNAM = 'P_RP21_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PDIO.
**  Date : VPV/IN
    L_ATNAM = 'P_RP22_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-MP.

**  Date : M/P  --> Not Defined

    APPEND IT_SUM_APP246.
*
  ENDLOOP.
  SORT IT_SUM_APP246 BY BODYNO .
  DESCRIBE TABLE IT_SUM_APP246 LINES P_TOTAL_APP246.
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
  CLEAR: P_PROG, P_PROG[].

  CASE W_UCOMM.
    WHEN 'VPC'.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'BT'.
      P_PROG-LOW    = '19'.
      P_PROG-HIGH   = '22'.
      APPEND P_PROG.
    WHEN 'SHIP'.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'BT'.
      P_PROG-LOW    = '23'.
      P_PROG-HIGH   = '24'.
      APPEND P_PROG.
    WHEN 'ALLO'.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'BT'.
      P_PROG-LOW    = '1'.
      P_PROG-HIGH   = '24'.
      APPEND P_PROG.
      P_PROG-SIGN   = 'I'.
      P_PROG-OPTION = 'EQ'.
      P_PROG-LOW    = '26'.
      P_PROG-HIGH   = ''.
      APPEND P_PROG.
  ENDCASE.
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
  PERFORM MAKE_PROGRESS_RANGE.                              "UD1K912914
  PERFORM CHECK_AND_READ_DATA_APP246 .
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
*  PERFORM SET_PARAMETER_FOR_SRCHNG_DATA USING L_ERROR L_TEXT.
*  IF L_ERROR <> SPACE.
*    CONCATENATE 'Enter The Necessary Parameters!!! -' L_TEXT
*      INTO L_TEXT.
*    MESSAGE I000 WITH L_TEXT.
*    EXIT.
*  ENDIF.
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

  CLEAR: IT_DET_APP246, IT_DET_APP246[].
  LOOP AT IT_OBJEK.
    CLEAR IT_DET_APP246.
*   V/M No & RP Point..
    MOVE-CORRESPONDING IT_OBJEK TO IT_DET_APP246.
    IT_DET_APP246-RP = IT_OBJEK-ATWRT           .
*   Model
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_DET_APP246-OBJEK
                                             'P_MODEL'
                                       CHANGING IT_DET_APP246-MODEL .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_DET_APP246-OBJEK
                                             'P_BODY_SERIAL'
                                       CHANGING IT_DET_APP246-BODYNO .
    CONCATENATE IT_DET_APP246-MODEL IT_DET_APP246-BODYNO
      INTO IT_DET_APP246-BODYNO .
*   Work Order(Serial)
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_WORK_ORDER'
                                       CHANGING IT_DET_APP246-WONO.
*   External Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_EXT_COLOR'
                                        CHANGING IT_DET_APP246-EXTC.
*   Internal Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_INT_COLOR'
                                       CHANGING IT_DET_APP246-INTC.
*   VIN
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_VIN'
                                       CHANGING IT_DET_APP246-VIN.
*   Vendor : Not Defined
*   MI
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_MI'
                                       CHANGING IT_DET_APP246-MI.
*   OCN
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_OCN'
                                       CHANGING IT_DET_APP246-OCN.
*   Planned Oder Number  "ADDING
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_PLAN_ORDER'
                                       CHANGING IT_DET_APP246-PLNUM.
*   Reporting Date / Time / Serial.
    CONCATENATE 'P_RP' IT_DET_APP246-RP '_ACTUAL_DATE'  INTO L_ATNAM .
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                L_ATNAM
                                       CHANGING L_ATWRT .
    IT_DET_APP246-REP_DATE = L_ATWRT+00(08).
    IT_DET_APP246-REP_TIME = L_ATWRT+08(06).

    CONCATENATE 'P_RP' IT_DET_APP246-RP '_SERIAL'  INTO L_ATNAM.
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                L_ATNAM
                                       CHANGING IT_DET_APP246-SERIAL .
*
    APPEND IT_DET_APP246.
  ENDLOOP.

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
