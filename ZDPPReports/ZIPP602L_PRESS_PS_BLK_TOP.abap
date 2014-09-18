*----------------------------------------------------------------------*
*   INCLUDE ZIPP601L_PRESS_PS__BLK_TOP                                 *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: MARA,           "General Material Data
        MAKT,           "Material Descriptions
        MARM,           "Units of Measure for Material
        ZTPPPS_BLK,     "Press Production Spec PP to MES - Blank Master
        ZSPPPS_BLK_RFC.

TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
* INTERNALTABLE
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MARC OCCURS 0,
        MATNR LIKE MARC-MATNR,   " Material
        WERKS LIKE MARC-WERKS,   " Plant
        EISBE LIKE MARC-EISBE,   " Safety stock
        BSTMI LIKE MARC-BSTMI,   " Minimum lot size
        BSTMA LIKE MARC-BSTMA,   " Maximum lot size
        BSTFE LIKE MARC-BSTFE,   " Fixed lot size
        GROES LIKE MARA-GROES,   " Size/dimensions
        NTGEW LIKE MARA-NTGEW,   " Net weight
        UMREZ LIKE MARM-UMREZ,   " Numerator for Conversion to UoM
      END OF IT_MARC.

DATA: IT_ZTPPPS      LIKE TABLE OF ZTPPPS_BLK     WITH HEADER LINE .
DATA: *IT_ZTPPPS     LIKE TABLE OF ZTPPPS_BLK     WITH HEADER LINE .
DATA: IT_ZSPPPS      LIKE TABLE OF ZSPPPS_BLK_RFC WITH HEADER LINE .
DATA: IT_VM          LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE .

DATA: IT_COMPA LIKE ZTPPEP OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_LIST OCCURS 0,
        WERKS            LIKE ZTPPPS_BLK-WERKS         ,
        PRS_BLK_MWC      LIKE ZTPPPS_BLK-PRS_BLK_MWC   ,
        MATNR            LIKE ZTPPPS_BLK-MATNR         ,
        MAKTX            LIKE ZTPPPS_BLK-MAKTX         ,
        PRS_BLK_PNLNW    LIKE ZTPPPS_BLK-PRS_BLK_PNLNW ,
        PRS_BLK_BQTY     LIKE ZTPPPS_BLK-PRS_BLK_BQTY  ,
        PRS_BLK_TPLT     LIKE ZTPPPS_BLK-PRS_BLK_TPLT  ,
        PRS_BLK_VMDL     LIKE ZTPPPS_BLK-PRS_BLK_VMDL  ,
        PRS_BLK_IOC      LIKE ZTPPPS_BLK-PRS_BLK_IOC   ,
        PRS_BLK_SWC      LIKE ZTPPPS_BLK-PRS_BLK_SWC   ,
        PRS_BLK_SCFC     LIKE ZTPPPS_BLK-PRS_BLK_SCFC  ,
        PRS_BLK_LLB      LIKE ZTPPPS_BLK-PRS_BLK_LLB   ,
        PRS_BLK_MCOLN    LIKE ZTPPPS_BLK-PRS_BLK_MCOLN ,
        PRS_BLK_SCOLN    LIKE ZTPPPS_BLK-PRS_BLK_SCOLN ,
        PRS_BLK_CUSG1    LIKE ZTPPPS_BLK-PRS_BLK_CUSG1 ,
        PRS_BLK_COLIW    LIKE ZTPPPS_BLK-PRS_BLK_COLIW ,
        PRS_BLK_COLQ     LIKE ZTPPPS_BLK-PRS_BLK_COLQ  ,
        PRS_BLK_COLT     LIKE ZTPPPS_BLK-PRS_BLK_COLT  ,
        PRS_BLK_COLW     LIKE ZTPPPS_BLK-PRS_BLK_COLW  ,
        PRS_BLK_COLL     LIKE ZTPPPS_BLK-PRS_BLK_COLL  ,
        PRS_BLK_COLPF    LIKE ZTPPPS_BLK-PRS_BLK_COLPF ,
        PRS_BLK_COLPR    LIKE ZTPPPS_BLK-PRS_BLK_COLPR ,
        PRS_BLK_COLD     LIKE ZTPPPS_BLK-PRS_BLK_COLD  ,
        PRS_BLK_DIEN     LIKE ZTPPPS_BLK-PRS_BLK_DIEN  ,
        PRS_BLK_PQTY     LIKE ZTPPPS_BLK-PRS_BLK_PQTY  ,
        PRS_BLK_PNLN1    LIKE ZTPPPS_BLK-PRS_BLK_PNLN1 ,
        PRS_BLK_PNLN2    LIKE ZTPPPS_BLK-PRS_BLK_PNLN2 ,
        UMREZ            LIKE ZTPPPS_BLK-UMREZ         ,
        BSTMI            LIKE ZTPPPS_BLK-BSTMI         ,
        BSTFE            LIKE ZTPPPS_BLK-BSTFE         ,
        BSTMA            LIKE ZTPPPS_BLK-BSTMA         ,
        EISBE            LIKE ZTPPPS_BLK-EISBE         ,
        GROES            LIKE ZTPPPS_BLK-GROES         ,
        MEINS            LIKE ZTPPPS_BLK-MEINS         ,
      END OF IT_LIST.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: OK_CODE LIKE SY-UCOMM,
      OKCODE LIKE SY-UCOMM.
DATA: Z_TOTAL  TYPE  SY-TABIX ,
      Z_FAIL   TYPE  SY-TABIX ,
      Z_SUCC   TYPE  SY-TABIX .
*----------------------------------------------------------------------*
*  CONSTANS
*----------------------------------------------------------------------*
CONSTANTS : C_DEST(10) VALUE 'WMPP01',     "Outbound I/F Destination
            C_SPB    LIKE  MARC-FEVOR  VALUE  'SPB' , "Prod scheduler
            C_MSTAE  LIKE  MARA-MSTAE  VALUE  '02'  , "Material status
            C_DISPO  LIKE  MARC-DISPO  VALUE  'MP1' . "MRP controller
CONSTANTS : C_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                        VALUE 'TOP_OF_PAGE'.
CONSTANTS : C_MARK VALUE 'X'.
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
SELECT-OPTIONS: S_MATNR FOR MARA-MATNR.
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
