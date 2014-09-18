*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST001_F01                                            *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST029_F01                                            *
*----------------------------------------------------------------------*
DEFINE _MONDAY.
  &2 = &1 + 1.
END-OF-DEFINITION.

DEFINE _SUNDAY.
  &2 = &1 + 6 .
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_DATA.

  DATA : LT_IPLAN LIKE TABLE OF ZTPP_INPUT_PLAN WITH HEADER LINE,
         LT_T07JB LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE.

  DATA : LT_KEY TYPE TABLE OF T_KEY WITH HEADER LINE,
         LT_DAY TYPE TABLE OF T_KEY WITH HEADER LINE,
         LT_WEEK TYPE TABLE OF T_KEY WITH HEADER LINE,
         LT_PLAN TYPE TABLE OF T_KEY WITH HEADER LINE.

  DATA : LV_SDATE LIKE SY-DATUM.


  PERFORM P2010_SELECT_IPLAN TABLES LT_IPLAN .
  CHECK NOT LT_IPLAN[] IS INITIAL.
  PERFORM P2100_CREATE_DATA_IPLAN TABLES LT_KEY
                                         LT_DAY
                                         LT_WEEK
                                         LT_IPLAN .


  IF P_LPLAN EQ 'X'.
    PERFORM P2020_SELECT_T07JB TABLES LT_T07JB.
    PERFORM P2200_CREATE_DATA_T07JB TABLES LT_T07JB
                                           LT_PLAN
                                           LT_KEY .

  ENDIF.

  PERFORM P2030_DELETE_ADJAC_ITEM_KEY TABLES LT_KEY.

*# create Dynamic Itab

  PERFORM P2300_DEFINE_DYNAMIC_ITAB.



*# Assign Data to Dynamic Table
  PERFORM P2400_INSERT_DATA TABLES LT_KEY
                                   LT_DAY
                                   LT_WEEK
                                   LT_PLAN .

ENDFORM.                    " P2000_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  P2010_SELECT_IPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_IPLAN  text
*----------------------------------------------------------------------*
FORM P2010_SELECT_IPLAN TABLES   P_IPLAN STRUCTURE ZTPP_INPUT_PLAN.

  SELECT WORK_ORDER EXTC INTC RSNUM MODL MI OCNN VERS
  INTO CORRESPONDING FIELDS OF TABLE P_IPLAN
  FROM ZTPP_INPUT_PLAN
  WHERE RSNUM IN S_RSNUM.

  DELETE  P_IPLAN WHERE RSNUM = '        ' OR RSNUM = '00000000'.

ENDFORM.                    " P2010_SELECT_IPLAN

*&---------------------------------------------------------------------*
*&      Form  P2020_SELECT_T07JB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_T07JB  text
*----------------------------------------------------------------------*
FORM P2020_SELECT_T07JB TABLES   P_T07JB STRUCTURE ZTPP_PMT07JB_A.
  SELECT SQDT ORDR DIST EXTC INTC OCNN VERS PQTY MODL BMDL
  INTO CORRESPONDING FIELDS OF TABLE P_T07JB
  FROM ZTPP_PMT07JB_A
  WHERE GUBB EQ 'B'.

ENDFORM.                    " P2020_SELECT_T07JB
*&---------------------------------------------------------------------*
*&      Form  P2030_DELETE_ADJAC_ITEM_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2030_DELETE_ADJAC_ITEM_KEY TABLES P_KEY STRUCTURE GT_KEY.

  SORT P_KEY BY WORD EXTC INTC OCNN VERS .
  DELETE ADJACENT DUPLICATES FROM P_KEY
  COMPARING WORD EXTC INTC OCNN VERS.

ENDFORM.                    " P2030_DELETE_ADJAC_ITEM_KEY
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA_IPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DAY  text
*      -->P_LT_WEEK  text
*      -->P_LT_KEY  text
*      -->P_LT_IPLAN  text
*----------------------------------------------------------------------*
FORM P2100_CREATE_DATA_IPLAN TABLES
                              P_KEY   STRUCTURE GT_KEY
                              P_DAY   STRUCTURE GT_KEY
                              P_WEEK  STRUCTURE GT_KEY
                              P_IPLAN STRUCTURE ZTPP_INPUT_PLAN.
  RANGES : R_WEEK1 FOR SY-DATUM ,
           R_WEEK2 FOR SY-DATUM ,
           R_WEEK3 FOR SY-DATUM ,
           R_WEEK4 FOR SY-DATUM ,
           R_WEEK5 FOR SY-DATUM .

  READ TABLE S_RSNUM INDEX 1 .

  R_WEEK1-SIGN = R_WEEK2-SIGN =
  R_WEEK3-SIGN = R_WEEK4-SIGN =
  R_WEEK5-SIGN = 'I'.

  R_WEEK1-OPTION = R_WEEK2-OPTION =
  R_WEEK3-OPTION = R_WEEK4-OPTION =
  R_WEEK5-OPTION = 'BT'.

*# first week
  R_WEEK1-LOW = S_RSNUM-LOW.
  PERFORM GET_MONDAY_NUM USING R_WEEK1-LOW
                               R_WEEK1-HIGH.

*# second week
  _MONDAY R_WEEK1-HIGH R_WEEK2-LOW.
  _SUNDAY R_WEEK2-LOW  R_WEEK2-HIGH.

*# third week
  _MONDAY R_WEEK2-HIGH R_WEEK3-LOW.
  _SUNDAY R_WEEK3-LOW  R_WEEK3-HIGH.

*# forth week
  _MONDAY R_WEEK3-HIGH R_WEEK4-LOW.
  _SUNDAY R_WEEK4-LOW  R_WEEK4-HIGH.

*# fifth week
  _MONDAY R_WEEK4-HIGH R_WEEK5-LOW.
  _SUNDAY R_WEEK5-LOW  R_WEEK5-HIGH.

  APPEND : R_WEEK1, R_WEEK2, R_WEEK3, R_WEEK4, R_WEEK5.

  SORT P_IPLAN BY WORK_ORDER INTC EXTC OCNN VERS SEQ_DATE.

  LOOP AT P_IPLAN.

    MOVE-CORRESPONDING P_IPLAN TO P_DAY.
    MOVE-CORRESPONDING P_IPLAN TO P_WEEK.
    MOVE-CORRESPONDING P_IPLAN TO P_KEY.

    P_DAY-WORD  =
    P_WEEK-WORD =
    P_KEY-WORD = P_IPLAN-WORK_ORDER.

    P_DAY-SEQ_DATE = P_IPLAN-RSNUM.
    P_DAY-NQTY = P_WEEK-NQTY = 1.

    CLEAR : P_WEEK-SEQ_DATE, P_KEY-SEQ_DATE.

    IF P_IPLAN-RSNUM IN R_WEEK1.
      P_WEEK-SEQ_WEEK = 1.
    ELSEIF P_IPLAN-RSNUM IN R_WEEK2.
      P_WEEK-SEQ_WEEK = 2.
    ELSEIF P_IPLAN-RSNUM IN R_WEEK3.
      P_WEEK-SEQ_WEEK = 3.

    ELSEIF P_IPLAN-RSNUM IN R_WEEK4.
      P_WEEK-SEQ_WEEK = 4.

    ELSEIF P_IPLAN-RSNUM IN R_WEEK5.
      P_WEEK-SEQ_WEEK = 5.
    ENDIF.

    COLLECT : P_DAY , P_WEEK , P_KEY.

  ENDLOOP.

ENDFORM.                    " CREATE_DATA_IPLAN


*&---------------------------------------------------------------------*
*&      Form  P2200_CREATE_DATA_T07JB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_T07JB  text
*      -->P_LT_PLAN  text
*      -->P_LT_KEY  text
*----------------------------------------------------------------------*
FORM P2200_CREATE_DATA_T07JB TABLES   P_T07JB STRUCTURE ZTPP_PMT07JB_A
                                      P_PLAN  STRUCTURE GT_KEY
                                      P_KEY   STRUCTURE GT_KEY.
  DATA : P_LINE TYPE I.

  CLEAR : P_LINE.
  P_LINE = 0 .

  SORT P_KEY BY WORD EXTC INTC OCNN VERS.
  SORT P_T07JB BY SQDT.

  LOOP AT P_T07JB.
    AT NEW SQDT .
      P_LINE = P_LINE + 1 .
    ENDAT.

    P_PLAN-SEQ_WEEK =  P_LINE.

    CONCATENATE P_T07JB-ORDR P_T07JB-DIST
    INTO P_PLAN-WORD.

    P_PLAN-EXTC  = P_T07JB-EXTC.
    P_PLAN-INTC  = P_T07JB-INTC.
    P_PLAN-OCNN  = P_T07JB-OCNN.
    P_PLAN-MODL  = P_T07JB-MODL.
    P_PLAN-MI    = P_T07JB-BMDL.

    P_PLAN-VERS = P_T07JB-VERS.
    P_PLAN-NQTY = P_T07JB-PQTY.

    P_KEY = P_PLAN.

    CLEAR : P_KEY-SEQ_WEEK, P_KEY-NQTY.

    COLLECT : P_PLAN, P_KEY.
  ENDLOOP.

  GV_LINE = P_LINE.

*  DESCRIBE TABLE P_PLAN LINES GV_LINE.
ENDFORM.                    " P2200_CREATE_DATA_T07JB

*&---------------------------------------------------------------------*
*&      Form  P2300_DEFINE_DYNAMIC_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2300_DEFINE_DYNAMIC_ITAB.
  PERFORM P2310_CREATE_FIELD_CONF.
  PERFORM P2320_CREATE_DYNAMIC_TABLE.

ENDFORM.                    " P2300_DEFINE_DYNAMIC_ITAB

*---------------------------------------------------------------------*
*       FORM GET_MONDAY_NUM                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_SDATE                                                       *
*  -->  P_EDATE                                                       *
*---------------------------------------------------------------------*
FORM GET_MONDAY_NUM USING P_SDATE P_EDATE.

  DATA : LV_DAY.
  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            DATE = P_SDATE
       IMPORTING
            DAY  = LV_DAY.

  P_EDATE = P_SDATE + ( 7 - LV_DAY ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2310_CREATE_FIELD_CONF.

  DATA: D_TAB TYPE REF TO DATA,
        LT_FCAT TYPE TABLE OF LVC_S_FCAT,
        LS_FCAT LIKE LINE OF LT_FCAT.

  DATA : FCNT(2) TYPE N.

  CLEAR : FCNT, LS_FCAT, LT_FCAT[].

  LS_FCAT-FIELDNAME = 'PLNT'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '2'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'DATE'.
  LS_FCAT-INTTYPE   = 'D'.
  LS_FCAT-INTLEN    = '8'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'WORD'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '15'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'EGMIP'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '4'.
  LS_FCAT-KEY       = ''.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'TMMIP'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '4'.
  LS_FCAT-KEY       = ''.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'SEAT'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '4'.
  LS_FCAT-KEY       = ''.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'MODEL'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '2'.
  LS_FCAT-KEY       = ''.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'YEAR'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '1'.
  LS_FCAT-KEY       = ''.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'NATN'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '5'.
  LS_FCAT-KEY       = ''.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'MODL'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '12'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.


  LS_FCAT-FIELDNAME = 'OCNN'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '4'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'VERS'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '3'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.


  LS_FCAT-FIELDNAME = 'EXTC'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '3'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.

  LS_FCAT-FIELDNAME = 'INTC'.
  LS_FCAT-INTTYPE   = 'C'.
  LS_FCAT-INTLEN    = '3'.
  LS_FCAT-KEY       = 'X'.
  APPEND LS_FCAT TO LT_FCAT.


  CLEAR : LS_FCAT.
  FCNT = 0 .
  DO 31 TIMES .
    LS_FCAT-TOOLTIP   = S_RSNUM-LOW + FCNT.
    FCNT = FCNT + 1 .
    CONCATENATE 'D' FCNT INTO LS_FCAT-FIELDNAME.
    LS_FCAT-INTTYPE   = 'C'.
    LS_FCAT-INTLEN    = 4.
    IF FCNT < 22 .
      LS_FCAT-EMPHASIZE = 'C300'.
    ELSE .
      LS_FCAT-EMPHASIZE = 'C400'.
    ENDIF.
    APPEND LS_FCAT TO LT_FCAT.
  ENDDO.

  CLEAR : LS_FCAT.

  IF P_LPLAN EQ 'X'.
    FCNT = 0 .
    DO 5 TIMES.
      FCNT = FCNT + 1 .
      CONCATENATE 'W' FCNT INTO LS_FCAT-FIELDNAME.
      LS_FCAT-INTTYPE   = 'C'.
      LS_FCAT-INTLEN    = 4.
      LS_FCAT-EMPHASIZE = 'C500'.
      APPEND LS_FCAT TO LT_FCAT.
    ENDDO.

    CLEAR : LS_FCAT.
    FCNT = 0 .
    GV_LINE = 16.
    DO GV_LINE TIMES.
      FCNT = FCNT + 1 .
      CONCATENATE 'PLAN_' FCNT INTO LS_FCAT-FIELDNAME.
      LS_FCAT-INTTYPE   = 'C'.
      LS_FCAT-INTLEN    = 4.
      LS_FCAT-EMPHASIZE = 'C700'.
      APPEND LS_FCAT TO LT_FCAT.
    ENDDO.
  ENDIF.

  GT_FIELDCAT[] = LT_FCAT[].



ENDFORM.                    " CREATE_FIELD_CONF


*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2320_CREATE_DYNAMIC_TABLE.
  DATA :LT_DTAB TYPE REF TO DATA.


  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = GT_FIELDCAT
    IMPORTING
      EP_TABLE        = LT_DTAB.

  ASSIGN LT_DTAB->* TO <ITAB>.


*  CREATE DATA NEW_LINE LIKE LINE OF <NEW_TAB>.
*  ASSIGN NEW_LINE->* TO <STATUS>.
*
*  FIELD-SYMBOLS : <FS_WA> TYPE ANY.
*  DATA : F_TXT(50), PNRDESC LIKE TEDSTSTRUC-NAME1.

ENDFORM.                    " CREATE_DYNAMIC_TABLE

*---------------------------------------------------------------------*
*       FORM GET_ATINN                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ZATINN                                                      *
*---------------------------------------------------------------------*


FORM GETSINGLE_ATWRT USING P_OBJEK P_ATWRT P_ATNAM.
  DATA : LV_ATINN LIKE AUSP-ATINN.
  RANGES : R_SHOP FOR CABN-ATNAM .

  R_SHOP-SIGN = 'E'.
  R_SHOP-OPTION = 'CP'.
  R_SHOP-LOW = '*SHOP_DATE'.
  APPEND R_SHOP.

  PERFORM CONVERSION_ATINN USING P_ATNAM LV_ATINN.

  IF P_ATNAM IN R_SHOP.
    SELECT SINGLE ATWRT INTO P_ATWRT
    FROM AUSP
    WHERE KLART = '001'
      AND ATINN = LV_ATINN
      AND OBJEK = P_OBJEK
      AND MAFID EQ 'O'
        %_HINTS ORACLE 'FIRST_ROWS(1)'.

    IF SY-SUBRC <> 0 . CLEAR P_ATWRT . ENDIF.

  ELSE. "'P_RP01_SHOP_DATE'.
    DATA :  LV_MAXDAT LIKE SY-DATUM.
    DATA :  LV_ATFLV LIKE AUSP-ATFLV.
    DATA : LV_TEMP(8) TYPE N.

    LV_MAXDAT = '00000000'.

    SELECT SINGLE ATFLV INTO   LV_ATFLV
    FROM AUSP
     WHERE  KLART = '002'
        AND ATINN = LV_ATINN
        AND OBJEK = P_OBJEK
        AND ATFLV >= LV_MAXDAT
        %_HINTS ORACLE 'FIRST_ROWS(1)'.

    LV_TEMP = LV_ATFLV.

    P_ATWRT = LV_TEMP.

  ENDIF.
ENDFORM.                    " GETSINGLE_ATWRT

*---------------------------------------------------------------------*
*       FORM CONVERSION_ATINN                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*  -->  P_ATINN                                                       *
*---------------------------------------------------------------------*
FORM CONVERSION_ATINN USING P_VALUE P_ATINN .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = P_VALUE
       IMPORTING
            OUTPUT = P_ATINN.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P2400_INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_KEY  text
*----------------------------------------------------------------------*
FORM P2400_INSERT_DATA TABLES   P_KEY  STRUCTURE GT_KEY
                                P_DAY  STRUCTURE GT_KEY
                                P_WEEK STRUCTURE GT_KEY
                                P_PLAN STRUCTURE GT_KEY.

  DATA : LT_MODEL LIKE TABLE OF ZTPP_MODEL_CONV WITH HEADER LINE,
         LT_COLOR LIKE TABLE OF ZTBM_ABYCOLDT   WITH HEADER LINE.

  DATA: NEW_LINE TYPE REF TO DATA,
        F_TXT(100).

  FIELD-SYMBOLS : <FIELD> TYPE ANY.


  CREATE DATA NEW_LINE LIKE LINE OF <ITAB>.
  ASSIGN NEW_LINE->* TO <LINE>.


  DATA : LV_CNT(2) TYPE N,
         LV_OBJEK LIKE AUSP-OBJEK,
         LV_ATWRT LIKE AUSP-ATWRT,
         LV_SDATE LIKE SY-DATUM.

  CLEAR : LV_SDATE.

  PERFORM P2410_GET_CONVTABLE_MODEL TABLES LT_MODEL.
  PERFORM P2420_GET_CONVTABLE_COLOR TABLES LT_COLOR LT_MODEL.

  SORT : P_DAY  BY WORD EXTC INTC OCNN VERS SEQ_DATE,
         P_WEEK BY WORD EXTC INTC OCNN VERS SEQ_WEEK,
         P_PLAN BY WORD EXTC INTC OCNN VERS SEQ_DATE.


  LOOP AT P_KEY.

    CONCATENATE 'PLNT' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
    <FIELD>  = C_PLANT.

    CONCATENATE 'DATE' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
    <FIELD>  = SY-DATUM.

*# GET LT_KEY
    CONCATENATE 'WORD' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
    <FIELD>  = P_KEY-WORD.

    CONCATENATE 'OCNN' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
    <FIELD>  = P_KEY-OCNN.

    CONCATENATE 'VERS' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
    <FIELD>  = P_KEY-VERS.

    IF P_KEY-VERS EQ '000'.
      <FIELD> = '    '.
    ENDIF.


    CONCATENATE 'MODL' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
    <FIELD>  = P_KEY-MI.

    CONCATENATE 'EXTC' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

    READ TABLE LT_COLOR WITH KEY CTRN_GUBN_C = 'EXT'
                                 CTRN_CARS_C = P_KEY-MODL+0(2)
                                 CTRN_CONF_COLR = P_KEY-EXTC.
    IF SY-SUBRC <> 0 .
      CLEAR LT_COLOR.
      <FIELD>  = P_KEY-EXTC.
    ELSE .
      <FIELD>  = LT_COLOR-CTRN_KEY_COLR.
    ENDIF.

    CONCATENATE 'INTC' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

    READ TABLE LT_COLOR WITH KEY CTRN_GUBN_C = 'INT'
                                 CTRN_CARS_C = P_KEY-MODL+0(2)
                                 CTRN_CONF_COLR = P_KEY-INTC.
    IF SY-SUBRC <> 0 .
      CLEAR LT_COLOR.
      <FIELD>  = P_KEY-INTC.
    ELSE .
      <FIELD>  = LT_COLOR-CTRN_KEY_COLR.
    ENDIF.

*    CONCATENATE 'INTC' '' INTO F_TXT.
*    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
*    <FIELD>  = P_KEY-INTC.

*# Get CONV Model for mobis
    READ TABLE LT_MODEL WITH KEY BMDL = P_KEY-MODL+0(2).
    IF SY-SUBRC <> 0 . CLEAR LT_MODEL . ENDIF.

    CONCATENATE 'MODEL' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.
    <FIELD>  = LT_MODEL-MOBIS.

*# Get AUSP
    LV_OBJEK = P_KEY-WORD .

    CONCATENATE 'EGMIP' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

    PERFORM GETSINGLE_ATWRT
            USING  LV_OBJEK LV_ATWRT  'P_ALC_U_1'.

    <FIELD> = LV_ATWRT.

    CONCATENATE 'TMMIP' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

    PERFORM GETSINGLE_ATWRT
            USING  LV_OBJEK LV_ATWRT   'P_ALC_U_2'.

    <FIELD> = LV_ATWRT.

    CONCATENATE 'YEAR' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

    PERFORM GETSINGLE_ATWRT
            USING  LV_OBJEK LV_ATWRT   'P_219_1'.

    <FIELD> = LV_ATWRT.

    CONCATENATE 'NATN' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

    PERFORM GETSINGLE_ATWRT
            USING  LV_OBJEK LV_ATWRT   'P_DESTINATION_CODE'.

    <FIELD> = LV_ATWRT.


    CONCATENATE LV_OBJEK P_KEY-EXTC P_KEY-INTC INTO LV_OBJEK.

    CONCATENATE 'SEAT' '' INTO F_TXT.
    ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

    PERFORM GETSINGLE_ATWRT
            USING  LV_OBJEK LV_ATWRT   'P_ALC_C_1'.
    <FIELD> = LV_ATWRT.

    LV_CNT = 0 .

    DO 31 TIMES.
      LV_SDATE = S_RSNUM-LOW + LV_CNT.
      LV_CNT = LV_CNT + 1.
      CONCATENATE 'D' LV_CNT INTO F_TXT.
      ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

      READ TABLE P_DAY WITH KEY WORD      = P_KEY-WORD
                                 EXTC     = P_KEY-EXTC
                                 INTC     = P_KEY-INTC
                                 OCNN     = P_KEY-OCNN
                                 VERS     = P_KEY-VERS
                                 SEQ_DATE = LV_SDATE
                                 BINARY SEARCH.

      IF SY-SUBRC EQ 0 .
        <FIELD> = P_DAY-NQTY.

      ELSE.
        CLEAR P_DAY.
        <FIELD> = '0000'.
      ENDIF.
      PERFORM P2430_CONV_NUMC_ALPHA USING <FIELD>.
    ENDDO.

    IF P_LPLAN EQ 'X'.

      LV_CNT = 0.
      SORT P_WEEK BY SEQ_WEEK.

      DO 5 TIMES.
        LV_CNT = LV_CNT + 1.

        CONCATENATE 'W' LV_CNT INTO F_TXT.
        ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.

        READ TABLE P_WEEK WITH KEY WORD = P_KEY-WORD
                                    EXTC = P_KEY-EXTC
                                    INTC = P_KEY-INTC
                                    OCNN = P_KEY-OCNN
                                    VERS = P_KEY-VERS
                                    SEQ_WEEK = LV_CNT.


        IF SY-SUBRC = 0 .
          <FIELD> = P_WEEK-NQTY.
        ELSE.
          <FIELD> = '0000'.
          CLEAR : P_WEEK.
        ENDIF.
        PERFORM P2430_CONV_NUMC_ALPHA USING <FIELD>.
      ENDDO.

      LV_CNT = 0 .


      DO GV_LINE TIMES.

        LV_CNT = LV_CNT + 1.

        CONCATENATE 'PLAN_' LV_CNT INTO F_TXT.
        ASSIGN COMPONENT F_TXT OF STRUCTURE <LINE> TO <FIELD>.


        READ TABLE P_PLAN WITH KEY  WORD = P_KEY-WORD
                                    EXTC = P_KEY-EXTC
                                    INTC = P_KEY-INTC
                                    OCNN = P_KEY-OCNN
                                    VERS = P_KEY-VERS
                                    SEQ_WEEK = LV_CNT.


        IF SY-SUBRC EQ 0 .
          <FIELD> = P_PLAN-NQTY.
        ELSE.
          CLEAR : P_PLAN.
          <FIELD> = '0000'.

        ENDIF.
        PERFORM P2430_CONV_NUMC_ALPHA USING <FIELD>.
      ENDDO.

    ENDIF.
    APPEND <LINE> TO <ITAB>.
    CLEAR <LINE>.
  ENDLOOP.


ENDFORM.                    " P2400_INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  P2410_GET_CONVTABLE_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MODEL  text
*----------------------------------------------------------------------*
FORM P2410_GET_CONVTABLE_MODEL TABLES
                           P_MODEL STRUCTURE ZTPP_MODEL_CONV.

  SELECT * INTO TABLE P_MODEL
    FROM ZTPP_MODEL_CONV.

ENDFORM.                    " P2410_GET_CONVTABLE_MODEL
*&---------------------------------------------------------------------*
*&      Form  P2420_GET_CONVTABLE_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_COLOR  text
*----------------------------------------------------------------------*
FORM P2420_GET_CONVTABLE_COLOR TABLES
                                P_COLOR STRUCTURE ZTBM_ABYCOLDT
                                P_MODEL STRUCTURE ZTPP_MODEL_CONV.

  RANGES : R_MODEL FOR ZTBM_ABYCOLDT-CTRN_CARS_C .
  R_MODEL-SIGN = 'I'.
  R_MODEL-OPTION = 'EQ'.


  LOOP AT P_MODEL .
    R_MODEL-LOW = P_MODEL-BMDL.
    COLLECT R_MODEL.
  ENDLOOP.

  SELECT CTRN_CARS_C
         CTRN_GUBN_C
         CTRN_KEY_COLR
         CTRN_CONF_COLR
         INTO CORRESPONDING FIELDS OF TABLE P_COLOR
     FROM ZTBM_ABYCOLDT
     WHERE CTRN_CARS_C IN R_MODEL
     AND CTRN_GUBN_C = 'INT' OR
           CTRN_GUBN_C = 'EXT' .



ENDFORM.                    " P2420_GET_CONVTABLE_COLOR
*&---------------------------------------------------------------------*
*&      Form  P3000_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_SAVE_DATA.

  DATA : P_FILE LIKE RLGRAP-FILENAME,
         P_PATH  LIKE FILENAMECI-PATHINTERN,
         REC_CNT TYPE I.
  DATA : LV_FILENAME(100) .

  DATA: NEW_LINE TYPE REF TO DATA,
        F_TXT(100).

  P_PATH = C_PATH."'/usr/sap/EDI_SAP/'.
  IF P_LPLAN EQ ''.
    LV_FILENAME = C_DAYFILE ."'MB_21_DAY'.
  ELSEIF P_LPLAN EQ 'X'.
    LV_FILENAME = C_WEEKFILE. "'MB_21_WEEK'.
  ENDIF.
  CONCATENATE P_PATH LV_FILENAME SY-DATUM '.TXT'
  INTO P_FILE.


  OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE.

  IF SY-SUBRC <> 0.
    WRITE: / 'ERROR IN CREATING A FILE', SY-SUBRC.
    STOP.
  ENDIF.

  CREATE DATA NEW_LINE LIKE LINE OF <ITAB>.
  ASSIGN NEW_LINE->* TO <LINE>.
**begin loop IT_TEMP
  LOOP AT <ITAB> ASSIGNING <LINE>  .

    TRANSFER <LINE> TO P_FILE.

    IF SY-SUBRC <> 0.
      WRITE: /'***ERROR writing to file', P_FILE, 'rc=', SY-SUBRC.
      WRITE: /'Record:'.
      WRITE: / P_FILE.
      STOP.
    ENDIF.

  ENDLOOP.

**  end of loop IT_TEMP
  CLOSE DATASET P_FILE.
  IF SY-SUBRC <> 0.
    WRITE: /'***ERROR closing file', P_FILE, 'rc=', SY-SUBRC.
    STOP.
  ELSE.
    DESCRIBE TABLE <ITAB> LINES REC_CNT.
    WRITE : / 'FILE', P_FILE, 'CREATED SUCESSFULLY'.
    WRITE : / 'TOTAL NUMBER OF RECORDS :' ,REC_CNT.
  ENDIF.
ENDFORM.                    " P3000_SAVE_DATA

*---------------------------------------------------------------------*
*       FORM P2430_CONV_NUMC_ALPHA                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM P2430_CONV_NUMC_ALPHA USING P_VALUE.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_VALUE
       IMPORTING
            OUTPUT = P_VALUE.

ENDFORM.
