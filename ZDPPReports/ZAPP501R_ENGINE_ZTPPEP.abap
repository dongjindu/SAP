************************************************************************
* Program Name      : ZAPP501R_ENGINE_ZTPPEP
* Author            : JongOh, Kim
* Creation Date     : 2004.02.10
* Specifications By : JongOh, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K906875
* Addl Documentation:
* Description       : Engine Sequencing (Create ZTPPEP table)
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP501R_ENGINE_ZTPPEP  NO STANDARD PAGE HEADING
                               MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : MARA,        "General Material Data
         MARC,        "Plant Data for Material
         MARD,        "Storage Location Data for Material
         ZTPPEP.      "Engine Production Planning (SAP -> MES)

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
*----> Stock Engine Material code
DATA : BEGIN OF IT_STOCK  OCCURS 0,
         MATNR    TYPE   MARC-MATNR,        " Material
         LABST    TYPE   MARD-LABST,        " Stock
         BSTMI    TYPE   MARC-BSTMI.        " Lot Size
DATA : END OF IT_STOCK.

*----> Engine Material Code at Plant
DATA : BEGIN OF IT_MATNR  OCCURS 0,
         WERKS    TYPE   MARC-WERKS,        " Plant
         MATNR    TYPE   MARC-MATNR.        " Material
DATA : END OF IT_MATNR.

*----> Vehicle Production Planning
DATA : BEGIN OF IT_VEHICLE OCCURS 0,
         PDATE    TYPE  ZTPPEP-PDATE,       " Planning date
*         PLNUM    TYPE  ZTPPEP-PLNUM,       " Planned order
         PITEM    TYPE  ZTPPEP-PITEM,       " Engine Material
         GSMNG    TYPE  ZTPPEP-GSMNG,       " Planning Qty
         MEINS    TYPE  ZTPPEP-MEINS,       " UoM of Material
         PLGRP    TYPE  ZTPPEP-PLGRP,       " Production scheduler
         RATIO    TYPE  ZTPPEP-GSMNG,       " Input Ratio
         CUMUL    TYPE  ZTPPEP-GSMNG.       " Cumulative Req Qty
*         EFLAG    TYPE  ZTPPEP-EFLAG,       "
*         FLAG     TYPE  ZTPPEP-FLAG .
DATA : END OF IT_VEHICLE .

*----> Input Engine Priority
DATA : BEGIN OF IT_PRIOR  OCCURS 0,
        PITEM     TYPE  ZTPPEP-PITEM,       " Engine Material
        GSMNG     TYPE  ZTPPEP-GSMNG,       " Planning Qty
        PRIOR     TYPE  ZTPPEP-GSMNG.       " Priority
DATA : END OF IT_PRIOR.

*----> Vehicle Production Planning total &
DATA : BEGIN OF IT_VEHTOTAL OCCURS 0,
         PDATE    TYPE  ZTPPEP-PDATE,       " Planning date
         GSMNG    TYPE  ZTPPEP-GSMNG.       " TOTAL Planning Qty
DATA : END OF IT_VEHTOTAL.

*----> Engine Production Planning
DATA : IT_ZTPPEP  LIKE TABLE OF ZTPPEP     WITH HEADER LINE.
DATA : IT_ZSPPEP  LIKE TABLE OF ZSPPEP_RFC WITH HEADER LINE.

*----> Error
DATA : BEGIN OF IT_ERROR OCCURS 0,
         MATNR   TYPE   MARA-MATNR,
         ZMSG    TYPE   ZTPPEP-ZMSG.
DATA : END OF IT_ERROR.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_STOCKTOTAL   TYPE  MARD-LABST,    " TOTAL STOCK
       WA_GSMNGTOTAL   TYPE  ZTPPEP-GSMNG.  " Requirement total

DATA : WA_ERROR_LINES  TYPE  SY-TABIX  .    " Error Counted
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION FOR BAPI
*----------------------------------------------------------------------*
DATA : WA_MATNR   TYPE  MARC-MATNR,        "Material No
       WA_WERKS   TYPE  MARC-WERKS.        "Plant No
DATA : IT_MDSUX   LIKE  TABLE OF MDSU WITH HEADER LINE.

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

CONSTANTS: C_SEA     TYPE   MARC-FEVOR   VALUE  'SEA',
           C_SEC     TYPE   MARC-FEVOR   VALUE  'SEC'.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : OK_CODE           LIKE  SY-UCOMM,
       SAVE_OK_CODE      LIKE  SY-UCOMM.
DATA : ALV_GRID               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER           TYPE SCRFNAME VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : GS_VARIANT        TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GS_SORT           TYPE LVC_S_SORT ,  "Sort Criteria
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria
DATA : WA_FIELDCAT     TYPE LVC_S_FCAT.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : P_WERKS      LIKE   T001W-WERKS OBLIGATORY MEMORY ID WRK.
SELECT-OPTIONS : S_DATUM  FOR   SY-DATUM     OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) TEXT-100.
PARAMETERS: P_IR AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-101 FOR FIELD P_IR.   "INSERT
PARAMETERS: P_RP AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-102 FOR FIELD P_RP.   "REPLACE
PARAMETERS: P_DL AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-103 FOR FIELD P_DL.   "DELETE
PARAMETERS: P_BA AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-104 FOR FIELD P_BA.   "BACK
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (5) TEXT-200.
PARAMETERS: P_REW  AS CHECKBOX .
SELECTION-SCREEN COMMENT  (15) TEXT-201 FOR FIELD P_REW.   " RE-WORK
SELECTION-SCREEN END OF LINE.

*----------------------------------------------------------------------*
*  MACRO DECLARATION
*----------------------------------------------------------------------*
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


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*


************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  CALL SCREEN 9000.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  P_WERKS = 'E001'.
  S_DATUM-SIGN   = 'I'.
  S_DATUM-OPTION = 'EQ'.
  S_DATUM-LOW    = SY-DATUM.
  APPEND S_DATUM.
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
*-----> SELECT ENGINE MATERIAL
  PERFORM SELECT_ENGINE.

*-----> Generate Vehicle Production Planning & Current Stock
  PERFORM VEHICLE_PLANNING.

  DESCRIBE TABLE IT_ERROR LINES WA_ERROR_LINES.
  IF WA_ERROR_LINES EQ 0.
*-----> Generate Input Ratio of Vehicle Production Planning & Total Plan
    PERFORM VEHICLE_INPUTRATIO.

*-----> Set Engine Production Planning
    PERFORM SET_ENGINE_PRODUCTION.
  ENDIF.
ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECT_ENGINE
*&---------------------------------------------------------------------*
FORM SELECT_ENGINE.
  CLEAR : WA_ERROR_LINES ,
          IT_ZSPPEP, IT_ZSPPEP[],
          IT_ZTPPEP, IT_ZTPPEP[].
  SELECT WERKS MATNR
         INTO TABLE IT_MATNR
         FROM MARC
         WHERE WERKS EQ P_WERKS   "Plant
           AND FEVOR EQ 'SEA'.    "Production scheduler
ENDFORM.                    " SELECT_ENGINE
*&---------------------------------------------------------------------*
*&      Form  VEHICLE_PLANNING
*&---------------------------------------------------------------------*
FORM VEHICLE_PLANNING.
  CLEAR : IT_STOCK  ,   IT_STOCK[],
          IT_VEHICLE,   IT_VEHICLE[].
  LOOP AT IT_MATNR.

*-----> Current Available Stock
    PERFORM CURRENT_STOCK.

*-----> Generate Vehicle Planning Qty of Engine Material
    PERFORM APPEND_VEHICLE.

  ENDLOOP.

  SORT IT_VEHICLE  BY PDATE PITEM.

ENDFORM.                    " VEHICLE_PLANNING
*&---------------------------------------------------------------------*
*&      Form  CURRENT_STOCK
*&---------------------------------------------------------------------*
FORM CURRENT_STOCK.
*-> UoM
  CLEAR : MARA, MARC, MARD.
  SELECT SINGLE *
            FROM MARA
            WHERE MATNR EQ IT_MATNR-MATNR.        " Material

*-> Stock Qty
  SELECT SINGLE *
            FROM MARC
            WHERE MATNR EQ IT_MATNR-MATNR         " Material
              AND WERKS EQ IT_MATNR-WERKS.        " Plant

*-> Available Stock
  SELECT SINGLE *
            FROM MARD
            WHERE MATNR EQ IT_MATNR-MATNR         " Material
              AND WERKS EQ IT_MATNR-WERKS         " Plant
              AND LGORT EQ MARC-LGPRO.            " Storage Loc
  MOVE-CORRESPONDING  MARD  TO  IT_STOCK.
  MOVE  MARC-BSTMI          TO  IT_STOCK-BSTMI.   " Lot Size
  IF IT_STOCK-BSTMI EQ 0.
    IT_ERROR-MATNR  =  IT_STOCK-MATNR.
    IT_ERROR-ZMSG   =  TEXT-301.
    APPEND IT_ERROR.   CLEAR IT_ERROR.
  ENDIF.
  APPEND IT_STOCK.    CLEAR IT_STOCK.

ENDFORM.                    " CURRENT_STOCK
*&---------------------------------------------------------------------*
*&      Form  APPEND_VEHICLE
*&---------------------------------------------------------------------*
FORM APPEND_VEHICLE.
  CLEAR : IT_MDSUX, IT_MDSUX[].
  CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
       EXPORTING
            MATNR                    = IT_MATNR-MATNR  " Material
            WERKS                    = IT_MATNR-WERKS  " Plant
       TABLES
            MDSUX                    = IT_MDSUX
       EXCEPTIONS
            MATERIAL_PLANT_NOT_FOUND = 1
            PLANT_NOT_FOUND          = 2
            OTHERS                   = 3.

  IF SY-SUBRC EQ 0.
    SORT IT_MDSUX BY SORT0.
    LOOP AT IT_MDSUX WHERE DELKZ NE 'WB'.
      MOVE : IT_MDSUX-DAT00   TO  IT_VEHICLE-PDATE,   "Planning date
             IT_MATNR         TO  IT_VEHICLE-PITEM,   "Engine Material
             C_SEA+2(1)       TO  IT_VEHICLE-PLGRP,   " 'A' or 'C'
             MARA-MEINS       TO  IT_VEHICLE-MEINS.   "UoM
      IT_VEHICLE-GSMNG  =  ( -1 ) * IT_MDSUX-MNG02.   "Planning Qty
*       CASE IT_MATNR-FEVOR.
*         WHEN 'SEA'.
*         WHEN 'SEC'.
*       ENDCASE.
      COLLECT IT_VEHICLE.     CLEAR : IT_VEHICLE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " APPEND_VEHICLE
*&---------------------------------------------------------------------*
*&      Form  VEHICLE_INPUTRATIO
*&---------------------------------------------------------------------*
FORM VEHICLE_INPUTRATIO.
  DATA : L_TABIX         TYPE       SY-TABIX.
  LOOP AT IT_VEHICLE.
    L_TABIX = SY-TABIX.
    AT NEW PDATE.
      SUM.
      MOVE : IT_VEHICLE-PDATE    TO    IT_VEHTOTAL-PDATE ,
             IT_VEHICLE-GSMNG    TO    IT_VEHTOTAL-GSMNG .
      APPEND IT_VEHTOTAL.
    ENDAT.

*-> Vehicle Input Ratio
    IT_VEHICLE-RATIO = IT_VEHICLE-GSMNG / IT_VEHTOTAL-GSMNG .

    MODIFY IT_VEHICLE INDEX  L_TABIX.
  ENDLOOP.

ENDFORM.                    " VEHICLE_INPUTRATIO
*&---------------------------------------------------------------------*
*&      Form  GENERATE_PRIORITY
*&---------------------------------------------------------------------*
FORM GENERATE_PRIORITY.
  DATA : L_TABIX            TYPE   SY-TABIX.

*-----> Generate Priority Information
  LOOP AT IT_VEHICLE WHERE PDATE EQ IT_VEHTOTAL-PDATE.
    MOVE-CORRESPONDING IT_VEHICLE TO IT_PRIOR.
    APPEND IT_PRIOR.     CLEAR IT_PRIOR.
  ENDLOOP.

  LOOP AT IT_PRIOR.
    L_TABIX = SY-TABIX.
    READ TABLE IT_STOCK WITH KEY MATNR = IT_PRIOR-PITEM.
    IF SY-SUBRC EQ 0.
*-> ( Stock - Item Requirement ) / Item Requirement
      IT_PRIOR-PRIOR
      = ( IT_STOCK-LABST - IT_PRIOR-GSMNG ) / IT_PRIOR-GSMNG.
      MODIFY IT_PRIOR  INDEX  L_TABIX.
    ENDIF.
  ENDLOOP.

  SORT IT_PRIOR BY PRIOR.

ENDFORM.                    " GENERATE_PRIORITY
*&---------------------------------------------------------------------*
*&      Form  SET_TOTAL_STOCK
*&---------------------------------------------------------------------*
FORM SET_TOTAL_STOCK.
  LOOP AT IT_STOCK.
    AT LAST.
      SUM.
      WA_STOCKTOTAL = IT_STOCK-LABST.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " SET_TOTAL_STOCK
*&---------------------------------------------------------------------*
*&      Form  GENERATE_PRODUCTION_PLANNING
*&---------------------------------------------------------------------*
FORM GENERATE_PRODUCTION_PLANNING.
  DATA : L_PRIOR_TABIX     TYPE   SY-TABIX,
         L_STOCK_TABIX     TYPE   SY-TABIX.
  LOOP AT IT_PRIOR.
    L_PRIOR_TABIX = SY-TABIX.
    READ TABLE IT_STOCK   WITH KEY MATNR = IT_PRIOR-PITEM.
    L_STOCK_TABIX = SY-TABIX.
    READ TABLE IT_VEHICLE WITH KEY PDATE = IT_VEHTOTAL-PDATE
                                   PITEM = IT_PRIOR-PITEM.
*--> Only Priority No1. sets Lot Production Planning Qty &
*    exhausts from item stock for Req ratio
    IF L_PRIOR_TABIX EQ 1.
      IT_STOCK-LABST = ( IT_STOCK-LABST - IT_PRIOR-GSMNG )
              + IT_STOCK-BSTMI - ( WA_STOCKTOTAL * IT_VEHICLE-RATIO ).

      MOVE : IT_VEHICLE-PDATE   TO   IT_ZSPPEP-PDATE,
             IT_VEHICLE-PITEM   TO   IT_ZSPPEP-PITEM,
             IT_VEHICLE-GSMNG   TO   IT_ZSPPEP-GSMNG,
             IT_VEHICLE-MEINS   TO   IT_ZSPPEP-MEINS,
             IT_VEHICLE-PLGRP   TO   IT_ZSPPEP-PLGRP.
*               IT_VEHICLE-EFLAG   TO   IT_ZSPPEP-EFLAG,
*               IT_VEHICLE-FLAG    TO   IT_ZSPPEP-FLAG.
      WA_GSMNGTOTAL = WA_GSMNGTOTAL + IT_STOCK-BSTMI.
      COLLECT IT_ZSPPEP.     CLEAR IT_ZSPPEP.
*--> The Others exhaust from Item Stock for Requirement Ratio
    ELSE.
      IT_STOCK-LABST = ( IT_STOCK-LABST - IT_PRIOR-GSMNG )
                     - ( WA_STOCKTOTAL * IT_VEHICLE-RATIO ).
    ENDIF.
    MODIFY IT_STOCK    INDEX  L_STOCK_TABIX.
  ENDLOOP.

ENDFORM.                    " GENERATE_PRODUCTION_PLANNING
*&---------------------------------------------------------------------*
*&      Form  SET_ENGINE_PRODUCTION
*&---------------------------------------------------------------------*
FORM SET_ENGINE_PRODUCTION.
  LOOP AT IT_VEHTOTAL.
    CLEAR : WA_GSMNGTOTAL.
    DO.
      IF WA_GSMNGTOTAL >= IT_VEHTOTAL-GSMNG.
        EXIT.
      ENDIF.
*-----> Generate Priority Information
      PERFORM GENERATE_PRIORITY.

*-----> Total Stock
      PERFORM SET_TOTAL_STOCK.

*-----> Generate Engine Production Planning
      PERFORM GENERATE_PRODUCTION_PLANNING.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " SET_ENGINE_PRODUCTION
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET TITLEBAR '9000'.
  IF IT_ZSPPEP[] IS INITIAL.
    SET PF-STATUS 'MAIN' . "EXCLUDING 'ZTRAN'.
  ELSE.
    SET PF-STATUS 'MAIN'.
  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.
  IF GS_CUSTOM_CONTAINER IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT GS_CUSTOM_CONTAINER
        EXPORTING CONTAINER_NAME = WA_CONTAINER.

    CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER.

    PERFORM  BUILD_VARIANT.
    PERFORM  BUILD_LAYOUT.
    PERFORM  BUILD_SORT.
    PERFORM  BUILD_FIELDCAT.

*-----> SET OBJECT
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'IT_ZSPPEP'
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
        IT_OUTTAB                     = IT_ZSPPEP[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
        IT_SORT                       = GT_SORT
*        IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4 .

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK_CODE.
    WHEN 'ZTRAN'.   "ALC TRANSFER
*      PERFORM SAVE_AND_TRANSFER.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM BUILD_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " BUILD_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM BUILD_LAYOUT.
  GS_LAYOUT-ZEBRA      = 'X'.   "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
FORM BUILD_SORT.
*GT_SORT-SPOS	    "Sort sequence
*GT_SORT-FIELDNAME    "ALV control: Field name of internal table field
*GT_SORT-UP           "Single-character flag
*GT_SORT-DOWN         "Single-character flag
  CLEAR : GS_SORT, GT_SORT[].
  GS_SORT-SPOS = 1.
  GS_SORT-FIELDNAME = 'PDATE'.
*  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.
  GS_SORT-SPOS = 2.
  GS_SORT-FIELDNAME = 'PITEM'.
*  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.

ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  DATA: L_STRUCT    LIKE  DD02L-TABNAME.
  DATA: L_IND,
        L_TABIX     TYPE  SY-TABIX.

  L_STRUCT = 'ZSPPEP_RFC'.
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
    L_TABIX = SY-TABIX.
    CLEAR L_IND.
    PERFORM SET_FIELD_INFO USING WA_FIELDCAT
                           CHANGING L_IND.
    IF L_IND IS INITIAL.
      WA_FIELDCAT-COL_POS = L_TABIX.
      MODIFY GT_FIELDCAT FROM WA_FIELDCAT.
    ELSE.
      DELETE GT_FIELDCAT INDEX L_TABIX.
    ENDIF.
    CLEAR WA_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INFO
*&---------------------------------------------------------------------*
FORM SET_FIELD_INFO USING L_FIELDCAT STRUCTURE LVC_S_FCAT
                    CHANGING P_IND.

  CASE L_FIELDCAT-FIELDNAME.
    WHEN 'PDATE'.
      SET_FIELDCAT  'PDATE' 10.
      L_FIELDCAT-KEY = 'X'.

    WHEN 'PITEM'.
*      SET_FIELDCAT 'PITEM' 3.
      L_FIELDCAT-KEY = 'X'.

    WHEN 'GSMNG'.
*      SET_FIELDCAT 'BODY NO.' 7.
*      L_FIELDCAT-KEY = 'X'.
    WHEN 'MEINS'.
*      SET_FIELDCAT 'WORK ORDER' 9.
    WHEN 'PLGRP'.
*      SET_FIELDCAT 'DIST.' 5.
    WHEN 'EFLAG'.
*      SET_FIELDCAT 'INT'  3.
    WHEN 'FLAG'.
*      SET_FIELDCAT 'EXT'  3.
    WHEN OTHERS.
      P_IND = 'X'.
  ENDCASE.

ENDFORM.                    " SET_FIELD_INFO
