*----------------------------------------------------------------------*
*   INCLUDE ZIPP501L_ENGIN_PP_TOP                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: PLAF,
        ZTPPEP,
        ZSPPEP_RFC.

TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
* INTERNALTABLE
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_PLAF OCCURS 0,
        PLNUM LIKE PLAF-PLNUM,
        PSTTR LIKE PLAF-PSTTR,
        MATNR LIKE PLAF-MATNR,
        GSMNG LIKE PLAF-GSMNG,
        MEINS LIKE PLAF-MEINS,
        PLGRP LIKE PLAF-PLGRP,
      END OF IT_PLAF.

DATA: *IT_PLAF LIKE IT_PLAF OCCURS 0 WITH HEADER LINE.
DATA: IT_ZTPPEP LIKE ZSPPEP_RFC OCCURS 0 WITH HEADER LINE.
DATA: *IT_ZTPPEP LIKE ZTPPEP OCCURS 0 WITH HEADER LINE.
DATA: IT_COMPA LIKE ZTPPEP OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_LIST OCCURS 0,
        FLAG(3),
        PDATE   LIKE ZTPPEP-PDATE,
        PLNUM   LIKE ZTPPEP-PLNUM,
        PITEM   LIKE ZTPPEP-PITEM,
        MAKTX   LIKE MAKT-MAKTX  ,
        GSMNG   LIKE ZTPPEP-GSMNG,
        MEINS(4),
        PLGRP(4),
        ZUSER   LIKE ZTPPEP-ZUSER,
        ZEDAT   LIKE ZTPPEP-ZEDAT,
        ZETIM   LIKE ZTPPEP-ZETIM,
        ZMSG(50),
      END OF IT_LIST.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_ZTPPEP LIKE ZTPPEP.
DATA : Z_TOTAL LIKE ZTCA_IF_LOG-TOTAL,  "TOTAL COUNT
       Z_SUCC LIKE ZTCA_IF_LOG-ZSUCC,   "SUCCESS COUNT
       Z_FAIL LIKE ZTCA_IF_LOG-ZSUCC.   "ERROR COUNT
DATA: I_ZTCA_IF_LOG LIKE ZTCA_IF_LOG.

DATA: OK_CODE LIKE SY-UCOMM,
      OKCODE LIKE SY-UCOMM.

*----------------------------------------------------------------------*
*  CONSTANS
*----------------------------------------------------------------------*
CONSTANTS  : C_DEST(10) VALUE 'WMPP01'.
             "Outbound Interface Destination
CONSTANTS : C_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                        VALUE 'TOP_OF_PAGE'.
CONSTANTS : C_MARK  VALUE 'X'.
CONSTANTS : C_E001  TYPE  T001W-WERKS   VALUE  'E001' ,
            C_SEA   TYPE  MARC-FEVOR    VALUE  'SEA'  ,
            C_SEC   TYPE  MARC-FEVOR    VALUE  'SEC'  ,
            C_ME1   TYPE  PLAF-DISPO    VALUE  'ME1'  .
*----------------------------------------------------------------------*
*  RANGES
*----------------------------------------------------------------------*
RANGES: S_DATE   FOR ZTPPEP-PDATE,
        R_PLGRP  FOR PLAF-PLGRP.    "ProdScheduler

*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
*  DEFINE APPEND_FIELDCAT.
*    &1 = &1 + 1.
*    W_FIELDCAT-COL_POS       = &1.
*    W_FIELDCAT-FIELDNAME     = &2.
*    W_FIELDCAT-REF_FIELDNAME = &3.
*    W_FIELDCAT-KEY           = &4.
*    W_FIELDCAT-QFIELDNAME    = &5.
*    W_FIELDCAT-CFIELDNAME    = &6.
*    W_FIELDCAT-QTABNAME      = &10.
*    W_FIELDCAT-SELTEXT_L     = &7.
*    W_FIELDCAT-SELTEXT_M     = &7.
*    W_FIELDCAT-SELTEXT_S     = &7.
*    W_FIELDCAT-OUTPUTLEN     = &8.
*    W_FIELDCAT-NO_OUT        = &9.
*    APPEND W_FIELDCAT.
*    CLEAR : W_FIELDCAT.
*  END-OF-DEFINITION.

*----------------------------------------------------------------------
* BDC
*----------------------------------------------------------------------
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.

DATA: IT_MSG LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA: IT_BDCDATA LIKE TABLE OF BDCDATA WITH HEADER LINE.
DATA: WA_BDCGROUP LIKE  SY-UNAME,
      P_TCODE     LIKE  TSTC-TCODE.

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

SELECT-OPTIONS: S_PDATE FOR ZTPPEP-PDATE OBLIGATORY NO-EXTENSION,
                S_PLNUM FOR ZTPPEP-PLNUM MODIF ID ABC,
                S_MATNR FOR ZTPPEP-PITEM MODIF ID ABC.
*                                         NO-EXTENSION
*                                         NO INTERVALS
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-005.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS      R_1 RADIOBUTTON GROUP RAD1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (25) TEXT-006 FOR FIELD R_1.
*SELECTION-SCREEN POSITION 25.
PARAMETERS      R_2 RADIOBUTTON GROUP RAD1.
*SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT (25) TEXT-007 FOR FIELD R_2.
*SELECTION-SCREEN POSITION 55.
PARAMETERS      R_3 RADIOBUTTON GROUP RAD1.
*SELECTION-SCREEN POSITION 56.
SELECTION-SCREEN COMMENT (20) TEXT-008 FOR FIELD R_3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       R1 RADIOBUTTON GROUP RAD2 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (30) TEXT-002 FOR FIELD R1.
*SELECTION-SCREEN POSITION 40.
PARAMETERS       R2 RADIOBUTTON GROUP RAD2.
*SELECTION-SCREEN POSITION 41.
SELECTION-SCREEN COMMENT (25) TEXT-003 FOR FIELD R2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

PARAMETERS : P_MODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N' NO-DISPLAY .
