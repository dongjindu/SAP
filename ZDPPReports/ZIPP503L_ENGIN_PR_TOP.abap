*----------------------------------------------------------------------*
*   INCLUDE ZIPP503L_ENGIN_PR_TOP                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: ZTPPER,       "Transfer of Engine Production Result (MES -> PP)
        MARA,           "General Material Data
        MARC,           "Plant Data for Material
        EQUI,           "Equipment master data
        ZTPPER_MAPPING. "Basis info table to use in ZIPP503I_ENGIN_PR
TABLES : ZTPP_MIP_STK_TRA .
TABLES : BLPK ,        "Document log header
         BLPP .        "Document log item
TABLES : ZTPP_PP_LOG_DETA.
TYPE-POOLS : SLIS, SP01R.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA: IT_ZTPPER   LIKE ZTPPER OCCURS 0 WITH HEADER LINE,
      IT_VMASTER  LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
      IT_VALS     LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
      IT_ERROR    LIKE TABLE OF ZTPPER         WITH HEADER LINE.
data: WA_AUFNR    type  AUFNR ,
      WA_MATNR    LIKE  IT_ZTPPeR-eitem,
      WA_ERPID(3) TYPE  C .

*----------------------------------------------------------------------*
* BAPI
*----------------------------------------------------------------------*
** For Cerate Functional Location BAPI
DATA: EXTERNAL_NUMBER LIKE BAPI_ITOB_PARMS-EQUIPMENT,
      DATA_GENERAL    LIKE BAPI_ITOB,
      DATA_SPECIFIC   LIKE BAPI_ITOB_EQ_ONLY,
      VALID_DATE      LIKE BAPI_ITOB_PARMS-INST_DATE,
      DATA_INSTALL    LIKE BAPI_ITOB_EQ_INSTALL,
** For Change Functional Location BAPI
      DATA_GENERALX   LIKE BAPI_ITOBX,
      DATA_SPECIFICX  LIKE BAPI_ITOB_EQ_ONLYX,
      RETURN          LIKE BAPIRET2 .

DATA: BFLUSHFLAGS   LIKE BAPI_RM_FLG,
      BFLUSHDATAGEN LIKE BAPI_RM_DATGEN,
      BFLUSHDATAMTS LIKE BAPI_RM_DATSTOCK,
      WA_RETURN LIKE BAPIRET2 ,
      IT_SERIALNR LIKE BAPI_RM_DATSERIAL OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_EQUNR       LIKE EQUI-EQUNR,
      WA_FLAG                       ,
      WA_ZTPPER       LIKE ZTPPER,
      WA_DATUM        LIKE SY-DATUM ,
      WA_MSGID        LIKE SY-MSGID,
      WA_MSGTY        LIKE SY-MSGTY,
      WA_MSGNO        LIKE SY-MSGNO,
      WA_MSGV1        LIKE SY-MSGV1,
      WA_MSGV2        LIKE SY-MSGV2,
      WA_MSGV3        LIKE SY-MSGV3,
      WA_MSGV4        LIKE SY-MSGV4,
      WA_CONFIRMATION TYPE BAPI_RM_DATKEY-CONFIRMATION,
      WA_CANCCO       TYPE BAPI_RM_DATKEY-CANCCONFIRMATION.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : OK_CODE       LIKE  SY-UCOMM,
       SAVE_OK_CODE  LIKE  SY-UCOMM.
DATA : ALV_GRID               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER           TYPE SCRFNAME VALUE 'CONTAINER'.
DATA : GS_VARIANT        TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT ,  "Filter criteria
       WA_FIELDCAT     TYPE LVC_S_FCAT   ,
       WA_FNAME_TX(40),
       WA_SAVELINE_IX     LIKE  SY-INDEX.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS: C_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                        VALUE 'TOP_OF_PAGE',
           C_MARK     VALUE 'X' .

*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
DEFINE APPEND_FIELDCAT.
  &1 = &1 + 1.
  W_FIELDCAT-COL_POS       = &1.
  W_FIELDCAT-FIELDNAME     = &2.
  W_FIELDCAT-REF_FIELDNAME = &3.
  W_FIELDCAT-KEY           = &4.
  W_FIELDCAT-QFIELDNAME    = &5.
  W_FIELDCAT-CFIELDNAME    = &6.
  W_FIELDCAT-SELTEXT_L     = &7.
  W_FIELDCAT-SELTEXT_M     = &7.
  W_FIELDCAT-SELTEXT_S     = &7.
  W_FIELDCAT-OUTPUTLEN     = &8.
  W_FIELDCAT-NO_OUT        = &9.
  APPEND W_FIELDCAT.
  CLEAR : W_FIELDCAT.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_ZSDAT FOR ZTPPER-ZSDAT NO-EXTENSION.
SELECT-OPTIONS: S_ZSLNO FOR ZTPPER-ZSLNO. "NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       R1 RADIOBUTTON GROUP RADI DEFAULT 'X'.  "PROCESSING
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(20) TEXT-002 FOR FIELD R1.
PARAMETERS       R2 RADIOBUTTON GROUP RADI.       "ERRROR RE-PROCESSING
SELECTION-SCREEN COMMENT  (25) TEXT-003 FOR FIELD R2.
PARAMETERS       R3 RADIOBUTTON GROUP RADI.       "DISPLAY
SELECTION-SCREEN COMMENT  (20) TEXT-004 FOR FIELD R3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.
