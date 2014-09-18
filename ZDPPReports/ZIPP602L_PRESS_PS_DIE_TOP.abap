*----------------------------------------------------------------------*
*   INCLUDE ZIPP601L_PRESS_PS_DIE_TOP                                 *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: MARA,           "General Material Data
        MAKT,           "Material Descriptions
        MARM,           "Units of Measure for Material
        ZTPPPS_DIE,     "Press Production Spec PP to MES - Die Master
        ZSPPPS_PNL_RFC.

TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
* INTERNALTABLE
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MARC OCCURS 0,
        MATNR LIKE MARC-MATNR,   " Material
        WERKS LIKE MARC-WERKS,   " Plant
*        EISBE LIKE MARC-EISBE,   " Safety stock
*        BSTMI LIKE MARC-BSTMI,   " Minimum lot size
*        BSTMA LIKE MARC-BSTMA,   " Maximum lot size
*        BSTFE LIKE MARC-BSTFE,   " Fixed lot size
*        GROES LIKE MARA-GROES,   " Size/dimensions
*        NTGEW LIKE MARA-NTGEW,   " Net weight
*        UMREZ LIKE MARM-UMREZ,   " Numerator for Conversion to UoM
      END OF IT_MARC.

DATA: IT_ZSPPPS      LIKE TABLE OF ZSPPPS_DIE_RFC WITH HEADER LINE.
DATA: IT_ZTPPPS      LIKE TABLE OF ZTPPPS_DIE     WITH HEADER LINE.
DATA: *IT_ZTPPPS     LIKE TABLE OF ZTPPPS_DIE     WITH HEADER LINE.
DATA: IT_VM          LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

DATA: BEGIN OF IT_LIST OCCURS 0,
        WERKS          LIKE ZTPPPS_DIE-WERKS          ,
        PRS_DIE_MWC    LIKE ZTPPPS_DIE-PRS_die_MWC    ,
        MATNR          LIKE ZTPPPS_DIE-MATNR          ,
        MAKTX          LIKE ZTPPPS_DIE-MAKTX          ,
        PRS_DIE_OPN    LIKE ZTPPPS_DIE-PRS_DIE_OPN    ,
        PRS_DIE_PRP    LIKE ZTPPPS_DIE-PRS_DIE_PRP    ,
        PRS_DIE_PLTN   LIKE ZTPPPS_DIE-PRS_DIE_PLTN   ,
        PRS_DIE_TPLT   LIKE ZTPPPS_DIE-PRS_DIE_TPLT   ,
        PRS_DIE_PWT    LIKE ZTPPPS_DIE-PRS_DIE_PWT    ,
        PRS_DIE_GBP    LIKE ZTPPPS_DIE-PRS_DIE_GBP    ,
        PRS_DIE_VMDL   LIKE ZTPPPS_DIE-PRS_DIE_VMDL   ,
        PRS_DIE_FPQ    LIKE ZTPPPS_DIE-PRS_DIE_FPQ    ,
        PRS_DIE_DIEL   LIKE ZTPPPS_DIE-PRS_DIE_DIEL   ,
        PRS_DIE_SWC    LIKE ZTPPPS_DIE-PRS_DIE_SWC    ,
        PRS_DIE_ALW    LIKE ZTPPPS_DIE-PRS_DIE_ALW    ,
        PRS_DIE_SPH    LIKE ZTPPPS_DIE-PRS_DIE_SPH    ,
        PRS_DIE_CGT    LIKE ZTPPPS_DIE-PRS_DIE_CGT    ,
      END OF IT_LIST.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: OK_CODE  LIKE  SY-UCOMM,
      OKCODE   LIKE  SY-UCOMM.

DATA: Z_TOTAL  TYPE  SY-TABIX ,
      Z_FAIL   TYPE  SY-TABIX ,
      Z_SUCC   TYPE  SY-TABIX .

RANGES : R_MATNR  FOR  MARA-MATNR .
*----------------------------------------------------------------------*
*  CONSTANS
*----------------------------------------------------------------------*
CONSTANTS : C_DEST(10) VALUE 'WMPP01'.
             "Outbound Interface Destination
CONSTANTS : C_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                        VALUE 'TOP_OF_PAGE'.
CONSTANTS : C_MARK VALUE 'X' ,
            C_SPD    LIKE  MARC-FEVOR  VALUE  'SPD' , "Prod scheduler
            C_MSTAE  LIKE  MARA-MSTAE  VALUE  '02'  , "Material status
            C_DISPO  LIKE  MARC-DISPO  VALUE  'MP1' . "MRP controller
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
*    W_FIELDCAT-NO_OUT        = &9.
    W_FIELDCAT-QUANTITY      = &9.
    APPEND W_FIELDCAT.
    CLEAR : W_FIELDCAT.
  END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Define variable for ALV
*----------------------------------------------------------------------*
DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*----------------------------------------------------------------------*

* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_WERKS    TYPE  T001W-WERKS OBLIGATORY .
SELECT-OPTIONS: S_MATNR FOR MARA-MATNR OBLIGATORY NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS      P_IR RADIOBUTTON GROUP RAD1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (25) TEXT-006 FOR FIELD P_IR.
PARAMETERS      P_RP RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN COMMENT (25) TEXT-007 FOR FIELD P_RP.
PARAMETERS      P_DL RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN COMMENT (20) TEXT-008 FOR FIELD P_DL.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       P_TRAN RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (30) TEXT-002 FOR FIELD P_TRAN.
PARAMETERS       P_RETR RADIOBUTTON GROUP RADI.
SELECTION-SCREEN COMMENT (25) TEXT-003 FOR FIELD P_RETR.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.
