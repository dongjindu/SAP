*----------------------------------------------------------------------*
*   INCLUDE ZIPP601L_PRESS_PP_TOP                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: MARC,
        MARD,
        ZTPPPP.

TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
* INTERNALTABLE
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_AFKO OCCURS 0,
        GSTRP    LIKE AFKO-GSTRP,
        GAMNG    LIKE AFKO-GAMNG,
        FEVOR    LIKE AFKO-FEVOR,
        AUFNR    LIKE AFKO-AUFNR,
        PLNBEZ   LIKE AFKO-PLNBEZ,
        GMEIN    LIKE AFKO-GMEIN,
        CY_SEQNR LIKE AFKO-CY_SEQNR,
      END OF IT_AFKO.

DATA: BEGIN OF IT_PLAF OCCURS 0,
        PSTTR LIKE PLAF-PSTTR,
        GSMNG LIKE PLAF-GSMNG,
        PLGRP LIKE PLAF-PLGRP,
        PLNUM LIKE PLAF-PLNUM,
        MATNR LIKE PLAF-MATNR,
        MEINS LIKE PLAF-MEINS,
      END OF IT_PLAF.

DATA: BEGIN OF IT_LIST OCCURS 0,
*        FLAG(3),
        GSTRP    LIKE ZTPPPP-GSTRP,
        CY_SEQNR LIKE ZTPPPP-CY_SEQNR,
        GAMNG    LIKE ZTPPPP-GAMNG,
        GMEIN(4),
        FEVOR    LIKE ZTPPPP-FEVOR,
        MATNR    LIKE ZTPPPP-MATNR,
        AUFNR    LIKE ZTPPPP-AUFNR,
        ZUSER    LIKE ZTPPPP-ZUSER,
        ZEDAT    LIKE ZTPPPP-ZEDAT,
        ZETIM    LIKE ZTPPPP-ZETIM,
        ZMSG(50),
      END OF IT_LIST.

DATA: *IT_AFKO   LIKE IT_AFKO OCCURS 0 WITH HEADER LINE,
      IT_ZTPPPP  LIKE ZSPPPP_RFC OCCURS 0 WITH HEADER LINE,
      *IT_ZTPPPP LIKE ZTPPPP OCCURS 0 WITH HEADER LINE,
      IT_COMPA   LIKE ZTPPPP OCCURS 0 WITH HEADER LINE,
      IT_VMASTER1 LIKE TABLE OF BAPI1003_ALLOC_VALUES_NUM
                                         WITH HEADER LINE,
      IT_VMASTER LIKE TABLE OF BAPI1003_ALLOC_VALUES_CHAR
                                         WITH HEADER LINE,
      IT_VMASTER2 LIKE TABLE OF BAPI1003_ALLOC_VALUES_CURR
                                         WITH HEADER LINE,
      RETURN LIKE TABLE OF BAPIRET2 WITH HEADER LINE.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA : Z_TOTAL LIKE ZTCA_IF_LOG-TOTAL,
       Z_SUCC LIKE ZTCA_IF_LOG-ZSUCC,
       Z_FAIL LIKE ZTCA_IF_LOG-ZSUCC.
DATA: I_ZTCA_IF_LOG LIKE ZTCA_IF_LOG.

DATA: OK_CODE LIKE SY-UCOMM,
      OKCODE LIKE SY-UCOMM.

DATA: Z_ARBPL LIKE ZTPPPP-ARBPL.

*----------------------------------------------------------------------*
*  RANGES
*----------------------------------------------------------------------*
RANGES: S_DATE FOR ZTPPPP-GSTRP,
        S_PSTTR FOR PLAF-PSTTR.
*----------------------------------------------------------------------*
*  CONSTANS
*----------------------------------------------------------------------*
CONSTANTS  : C_DEST(10) VALUE 'WMPP01'.
             "Outbound Interface Destination
CONSTANTS : C_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                        VALUE 'TOP_OF_PAGE'.
CONSTANTS : C_MARK VALUE 'X',
            C_SPP     TYPE   MARC-FEVOR   VALUE  'SPP',
            C_SPB     TYPE   MARC-FEVOR   VALUE  'SPB',
            C_P128    TYPE   MARD-LGORT   VALUE  'P128',
            C_P125    TYPE   MARD-LGORT   VALUE  'P125'.
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
*       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF SCREEN 9999.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS: P_PWERK LIKE ZTPPPP-PWERK DEFAULT 'P001' OBLIGATORY,
*            P_ARBPL LIKE ZTPPPP-ARBPL MODIF ID ABC.
*            P_GSTRP LIKE ZTPPPP-GSTRP.
SELECT-OPTIONS: S_GSTRP  FOR ZTPPPP-GSTRP OBLIGATORY NO-EXTENSION,
                S_AUFNR  FOR ZTPPPP-AUFNR,
                S_PLNBEZ FOR ZTPPPP-MATNR,
                S_SEQNR FOR ZTPPPP-CY_SEQNR.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) TEXT-100.
PARAMETERS: P_SEQ RADIOBUTTON GROUP R .
SELECTION-SCREEN COMMENT  (25) TEXT-101 FOR FIELD P_SEQ.   "SEQ LOGIC
PARAMETERS: P_MRP RADIOBUTTON GROUP R .
SELECTION-SCREEN COMMENT  (25) TEXT-102 FOR FIELD P_MRP.   "MRP LOGIC
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS      R_1 RADIOBUTTON GROUP RAD1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (25) TEXT-006 FOR FIELD R_1.
PARAMETERS      R_2 RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN COMMENT (25) TEXT-007 FOR FIELD R_2.
PARAMETERS      R_3 RADIOBUTTON GROUP RAD1.
SELECTION-SCREEN COMMENT (20) TEXT-008 FOR FIELD R_3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       R1 RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (30) TEXT-002 FOR FIELD R1.
PARAMETERS       R2 RADIOBUTTON GROUP RADI.
SELECTION-SCREEN COMMENT (25) TEXT-003 FOR FIELD R2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

PARAMETERS : P_MODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N' NO-DISPLAY.

*SELECTION-SCREEN END OF SCREEN 9999.
