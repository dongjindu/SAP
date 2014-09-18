************************************************************************
* Program Name      : ZAPP903R_DAILY_PRODUCTION_MUL
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : program made by modifying existing program
*                     ZAPP903R_DAILY_PRODUCTION to allow multi date
* Addl Documentation:
* Description       : Daily Production Report by date
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 04/24/07   Furong Wang               Multi-date entries allowed
************************************************************************
REPORT  ZAPP903R_DAILY_PRODUCTION  MESSAGE-ID ZMPP.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ZVPP_VEHICLE,
        ZTPP_INPUT_PLAN,
        AUSP ,
        CRHD .

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: IT_WORDER          LIKE TABLE OF ZSPP_DAY_WORDER WITH HEADER LINE,
      IT_PRODUCT         LIKE TABLE OF ZTPP_DAY_SUM    WITH HEADER LINE.

DATA: W_DATE_MIN LIKE SY-DATUM,
      W_DATE_MAX LIKE SY-DATUM,
      W_DATE LIKE SY-DATUM.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_DATE FOR SY-DATUM NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  IF S_DATE IS INITIAL.
    S_DATE-LOW = SY-DATUM - 1.
    S_DATE-HIGH = S_DATE-LOW.
  ENDIF.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  IF S_DATE-HIGH IS INITIAL.
    W_DATE_MIN = S_DATE-LOW.
    W_DATE_MAX = S_DATE-LOW.
  ELSE.
    W_DATE_MIN = S_DATE-LOW.
    W_DATE_MAX = S_DATE-HIGH.
  ENDIF.

  WHILE W_DATE_MIN <= W_DATE_MAX.
    W_DATE = W_DATE_MIN.
    PERFORM FUNCTION_CALL.
    PERFORM SAVE_TABLE.
    W_DATE_MIN  =  W_DATE_MIN + 1.
    CLEAR: IT_WORDER, IT_PRODUCT, IT_WORDER[], IT_PRODUCT[].
  ENDWHILE.
*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  FUNCTION_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FUNCTION_CALL .
  CALL FUNCTION 'Z_FPP_RP_PRODUCTION'
       EXPORTING
            WDATE       = W_DATE
** Furong on 04/24/13  for D-1
 I_INPUT_PLAN_CHK       = 'X'
** enD ON 04/24/13
       TABLES
            T_WORDER    = IT_WORDER
       EXCEPTIONS
            INPUT_ERROR = 1
            OTHERS      = 2.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FUNCTION_CALL

*&---------------------------------------------------------------------*
*&      Form  SAVE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TABLE    .
  FIELD-SYMBOLS: <FIELDS>   TYPE ANY.
  DATA: LT_PRODUCT          LIKE TABLE OF ZTPP_DAY_SUM WITH HEADER LINE,
        L_NAMES(30)         TYPE C         ,
        L_WOHEAD            LIKE MARA-MATNR,
        L_COUNT             TYPE I         ,
        L_PQTY              TYPE I         ,
        L_MQTY              TYPE I         ,
        L_STATUS            TYPE ZPP_STATUS       ,
        L_WO_SER            LIKE ZTPP_WOSUM-WO_SER,
        L_NATION            LIKE ZTPP_WOSUM-NATION,
        L_DEALER            LIKE ZTPP_WOSUM-DEALER,
        L_EXTC              LIKE ZTPP_WOSUM-EXTC  ,
        L_INTC              LIKE ZTPP_WOSUM-INTC  .

  SORT IT_WORDER BY WORDER STATUS   .
  READ TABLE IT_WORDER INDEX 1      .
  L_WOHEAD = IT_WORDER-WORDER       .
  L_WO_SER = IT_WORDER-WORDER(09)   .
  L_NATION = IT_WORDER-WORDER+09(03).
  L_DEALER = IT_WORDER-WORDER+12(02).
  L_EXTC   = IT_WORDER-WORDER+14(02).
  L_INTC   = IT_WORDER-WORDER+16(02).
  LOOP AT IT_WORDER.
    L_STATUS = IT_WORDER-STATUS    .
    IF L_WOHEAD = IT_WORDER-WORDER .
      CONCATENATE 'LT_PRODUCT-RP' L_STATUS(2) 'Q' INTO  L_NAMES.
      ASSIGN (L_NAMES)  TO    <FIELDS> .
      <FIELDS> = IT_WORDER-WO_QTY      .
      CONTINUE .
    ELSE.
      LT_PRODUCT-WDATE  = W_DATE .
      LT_PRODUCT-WO_SER = L_WO_SER.
      LT_PRODUCT-NATION = L_NATION.
      LT_PRODUCT-DEALER = L_DEALER.
      LT_PRODUCT-EXTC   = L_EXTC  .
      LT_PRODUCT-INTC   = L_INTC  .
      APPEND LT_PRODUCT.  CLEAR: LT_PRODUCT.
      CONCATENATE 'LT_PRODUCT-RP' L_STATUS(2) 'Q' INTO  L_NAMES.
      ASSIGN (L_NAMES)  TO    <FIELDS> .
      <FIELDS> = IT_WORDER-WO_QTY      .
      L_WOHEAD = IT_WORDER-WORDER       .
      L_WO_SER = IT_WORDER-WORDER(09)   .
      L_NATION = IT_WORDER-WORDER+09(03).
      L_DEALER = IT_WORDER-WORDER+12(02).
      L_EXTC   = IT_WORDER-WORDER+14(02).
      L_INTC   = IT_WORDER-WORDER+16(02).
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_WORDER LINES L_COUNT.
  IF L_COUNT > 0.
    LT_PRODUCT-WDATE  = W_DATE .
    LT_PRODUCT-WO_SER = L_WO_SER.
    LT_PRODUCT-NATION = L_NATION.
    LT_PRODUCT-DEALER = L_DEALER.
    LT_PRODUCT-EXTC   = L_EXTC  .
    LT_PRODUCT-INTC   = L_INTC  .
    APPEND LT_PRODUCT.  CLEAR: LT_PRODUCT.
  ENDIF.

  SORT LT_PRODUCT BY WO_SER NATION DEALER EXTC INTC   .
  READ TABLE LT_PRODUCT INDEX 1     .
  CLEAR: L_PQTY, L_MQTY.
  L_WO_SER   = LT_PRODUCT-WO_SER    .
  L_NATION   = LT_PRODUCT-NATION    .
  L_DEALER   = LT_PRODUCT-DEALER    .
  L_EXTC     = LT_PRODUCT-EXTC      .
  L_INTC     = LT_PRODUCT-INTC      .

  LOOP AT LT_PRODUCT.
    IF   L_WO_SER = LT_PRODUCT-WO_SER AND L_NATION = LT_PRODUCT-NATION
     AND L_DEALER = LT_PRODUCT-DEALER AND L_EXTC   = LT_PRODUCT-EXTC
     AND L_INTC   = LT_PRODUCT-INTC     .
      IT_PRODUCT-RP01Q = IT_PRODUCT-RP01Q + LT_PRODUCT-RP01Q .
      IT_PRODUCT-RP02Q = IT_PRODUCT-RP02Q + LT_PRODUCT-RP02Q .
      IT_PRODUCT-RP03Q = IT_PRODUCT-RP03Q + LT_PRODUCT-RP03Q .
      IT_PRODUCT-RP04Q = IT_PRODUCT-RP04Q + LT_PRODUCT-RP04Q .
      IT_PRODUCT-RP05Q = IT_PRODUCT-RP05Q + LT_PRODUCT-RP05Q .
      IT_PRODUCT-RP06Q = IT_PRODUCT-RP06Q + LT_PRODUCT-RP06Q .
      IT_PRODUCT-RP07Q = IT_PRODUCT-RP07Q + LT_PRODUCT-RP07Q .
      IT_PRODUCT-RP08Q = IT_PRODUCT-RP08Q + LT_PRODUCT-RP08Q .
      IT_PRODUCT-RP09Q = IT_PRODUCT-RP09Q + LT_PRODUCT-RP09Q .
      IT_PRODUCT-RP10Q = IT_PRODUCT-RP10Q + LT_PRODUCT-RP10Q .
      IT_PRODUCT-RP11Q = IT_PRODUCT-RP11Q + LT_PRODUCT-RP11Q .
      IT_PRODUCT-RP12Q = IT_PRODUCT-RP12Q + LT_PRODUCT-RP12Q .
      IT_PRODUCT-RP13Q = IT_PRODUCT-RP13Q + LT_PRODUCT-RP13Q .
      IT_PRODUCT-RP14Q = IT_PRODUCT-RP14Q + LT_PRODUCT-RP14Q .
      IT_PRODUCT-RP15Q = IT_PRODUCT-RP15Q + LT_PRODUCT-RP15Q .
      IT_PRODUCT-RP16Q = IT_PRODUCT-RP16Q + LT_PRODUCT-RP16Q .
      IT_PRODUCT-RP17Q = IT_PRODUCT-RP17Q + LT_PRODUCT-RP17Q .
      IT_PRODUCT-RP18Q = IT_PRODUCT-RP18Q + LT_PRODUCT-RP18Q .
      IT_PRODUCT-RP19Q = IT_PRODUCT-RP19Q + LT_PRODUCT-RP19Q .
      IT_PRODUCT-RP20Q = IT_PRODUCT-RP20Q + LT_PRODUCT-RP20Q .
      IT_PRODUCT-RP21Q = IT_PRODUCT-RP21Q + LT_PRODUCT-RP21Q .
      IT_PRODUCT-RP22Q = IT_PRODUCT-RP22Q + LT_PRODUCT-RP22Q .
      IT_PRODUCT-RP23Q = IT_PRODUCT-RP23Q + LT_PRODUCT-RP23Q .
      IT_PRODUCT-RP24Q = IT_PRODUCT-RP24Q + LT_PRODUCT-RP24Q .
      IT_PRODUCT-RP25Q = IT_PRODUCT-RP25Q + LT_PRODUCT-RP25Q .
      IT_PRODUCT-RP26Q = IT_PRODUCT-RP26Q + LT_PRODUCT-RP26Q .
      IT_PRODUCT-RP27Q = IT_PRODUCT-RP27Q + LT_PRODUCT-RP27Q .
*       IT_PRODUCT-RP28Q = IT_PRODUCT-RP28Q + LT_PRODUCT-RP28Q .
    ELSE.
      IT_PRODUCT-WDATE  = W_DATE  .
      IT_PRODUCT-WO_SER = L_WO_SER .
      IT_PRODUCT-DEALER = L_DEALER .
      IT_PRODUCT-NATION = L_NATION .
      IT_PRODUCT-EXTC   = L_EXTC   .
      IT_PRODUCT-INTC   = L_INTC   .
      APPEND IT_PRODUCT. CLEAR: IT_PRODUCT.
      IT_PRODUCT-RP01Q = IT_PRODUCT-RP01Q + LT_PRODUCT-RP01Q .
      IT_PRODUCT-RP02Q = IT_PRODUCT-RP02Q + LT_PRODUCT-RP02Q .
      IT_PRODUCT-RP03Q = IT_PRODUCT-RP03Q + LT_PRODUCT-RP03Q .
      IT_PRODUCT-RP04Q = IT_PRODUCT-RP04Q + LT_PRODUCT-RP04Q .
      IT_PRODUCT-RP05Q = IT_PRODUCT-RP05Q + LT_PRODUCT-RP05Q .
      IT_PRODUCT-RP06Q = IT_PRODUCT-RP06Q + LT_PRODUCT-RP06Q .
      IT_PRODUCT-RP07Q = IT_PRODUCT-RP07Q + LT_PRODUCT-RP07Q .
      IT_PRODUCT-RP08Q = IT_PRODUCT-RP08Q + LT_PRODUCT-RP08Q .
      IT_PRODUCT-RP09Q = IT_PRODUCT-RP09Q + LT_PRODUCT-RP09Q .
      IT_PRODUCT-RP10Q = IT_PRODUCT-RP10Q + LT_PRODUCT-RP10Q .
      IT_PRODUCT-RP11Q = IT_PRODUCT-RP11Q + LT_PRODUCT-RP11Q .
      IT_PRODUCT-RP12Q = IT_PRODUCT-RP12Q + LT_PRODUCT-RP12Q .
      IT_PRODUCT-RP13Q = IT_PRODUCT-RP13Q + LT_PRODUCT-RP13Q .
      IT_PRODUCT-RP14Q = IT_PRODUCT-RP14Q + LT_PRODUCT-RP14Q .
      IT_PRODUCT-RP15Q = IT_PRODUCT-RP15Q + LT_PRODUCT-RP15Q .
      IT_PRODUCT-RP16Q = IT_PRODUCT-RP16Q + LT_PRODUCT-RP16Q .
      IT_PRODUCT-RP17Q = IT_PRODUCT-RP17Q + LT_PRODUCT-RP17Q .
      IT_PRODUCT-RP18Q = IT_PRODUCT-RP18Q + LT_PRODUCT-RP18Q .
      IT_PRODUCT-RP19Q = IT_PRODUCT-RP19Q + LT_PRODUCT-RP19Q .
      IT_PRODUCT-RP20Q = IT_PRODUCT-RP20Q + LT_PRODUCT-RP20Q .
      IT_PRODUCT-RP21Q = IT_PRODUCT-RP21Q + LT_PRODUCT-RP21Q .
      IT_PRODUCT-RP22Q = IT_PRODUCT-RP22Q + LT_PRODUCT-RP22Q .
      IT_PRODUCT-RP23Q = IT_PRODUCT-RP23Q + LT_PRODUCT-RP23Q .
      IT_PRODUCT-RP24Q = IT_PRODUCT-RP24Q + LT_PRODUCT-RP24Q .
      IT_PRODUCT-RP25Q = IT_PRODUCT-RP25Q + LT_PRODUCT-RP25Q .
      IT_PRODUCT-RP26Q = IT_PRODUCT-RP26Q + LT_PRODUCT-RP26Q .
      IT_PRODUCT-RP27Q = IT_PRODUCT-RP27Q + LT_PRODUCT-RP27Q .
*       IT_PRODUCT-RP28Q = IT_PRODUCT-RP28Q + LT_PRODUCT-RP28Q .
      L_WO_SER   = LT_PRODUCT-WO_SER    .
      L_NATION   = LT_PRODUCT-NATION    .
      L_DEALER   = LT_PRODUCT-DEALER    .
      L_EXTC     = LT_PRODUCT-EXTC      .
      L_INTC     = LT_PRODUCT-INTC      .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_PRODUCT LINES L_COUNT.
  IF L_COUNT > 0.
    IT_PRODUCT-WDATE  = W_DATE  .
    IT_PRODUCT-WO_SER = L_WO_SER .
    IT_PRODUCT-DEALER = L_DEALER .
    IT_PRODUCT-NATION = L_NATION .
    IT_PRODUCT-EXTC   = L_EXTC   .
    IT_PRODUCT-INTC   = L_INTC   .
    APPEND IT_PRODUCT.  CLEAR: IT_PRODUCT.
  ENDIF.

  DELETE FROM ZTPP_DAY_SUM WHERE WDATE = W_DATE.

  MODIFY ZTPP_DAY_SUM FROM TABLE IT_PRODUCT.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    WRITE: 'Update table ZTPP_DAY_SUM failed on ', W_DATE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_TABLE
