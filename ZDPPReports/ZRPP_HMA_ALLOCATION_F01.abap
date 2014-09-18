*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_START_PROGRESSBAR USING PERCENT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PERCENT
            TEXT       = TEXT-001
       EXCEPTIONS
            OTHERS     = 1.
ENDFORM.                    " P1000_START_PROGRESSBAR

*---------------------------------------------------------------------*
*       FORM P2000_GET_DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM P2000_GET_DATA.
  DATA : LT_07JB LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE,
         LT_UM   LIKE TABLE OF ZTSD_UM        WITH HEADER LINE,
         LT_VM   LIKE TABLE OF ZTPP_VM        WITH HEADER LINE,
         LT_BMDL LIKE TABLE OF ZTPP_MODEL_CONV WITH HEADER LINE.
  DATA : BEGIN OF LT_OSR OCCURS 0 .
          INCLUDE STRUCTURE ZTSD_OSR.
  DATA :  ZVIN_UM LIKE ZTSD_UM-ZVIN,
         END OF LT_OSR .

  DATA : BEGIN OF LT_KEY OCCURS 0 ,
          NATION  LIKE ZTSD_UM-WO_NATION  ,
          MODEL   LIKE ZTSD_UM-MODEL_CODE ,
          GUBB    LIKE ZTPP_PMT07JB_A-GUBB,
          PQTY    LIKE ZTPP_PMT07JB_A-PQTY,
        END OF LT_KEY .
  DATA : LT_DEAL LIKE TABLE OF LT_KEY WITH HEADER LINE,
         LT_DATA LIKE TABLE OF GT_DATA WITH HEADER LINE.

  RANGES : R_DT06 FOR ZTSD_OSR-DT06.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_07JB
    FROM ZTPP_PMT07JB_A
    WHERE GUBB = 'A'
       OR GUBB = 'B' .

  R_DT06-SIGN = 'I'.
  R_DT06-OPTION = 'EQ'.

  LOOP AT LT_07JB .
    CLEAR : LT_KEY .
    LT_KEY-NATION = LT_07JB-DIST+0(3).
    LT_KEY-MODEL  = LT_07JB-MODL .
    LT_KEY-GUBB   = LT_07JB-GUBB . "FORE = 'B' PLAN = 'A'
    LT_KEY-PQTY   = LT_07JB-PQTY .
    COLLECT LT_KEY.
    R_DT06-LOW = LT_07JB-SQDT.
    COLLECT R_DT06.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_OSR
    FROM ZTSD_OSR
    WHERE ST06 = '1'
      AND DT06 IN R_DT06.

  LOOP AT LT_OSR .
    LT_OSR-ZVIN_UM = LT_OSR-ZVIN.
    MODIFY LT_OSR.
  ENDLOOP.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_UM
    FROM ZTSD_UM
   FOR ALL ENTRIES IN LT_OSR
   WHERE ZVIN  = LT_OSR-ZVIN
   %_HINTS ORACLE 'index("ZTSD_UM","ZTSD_UM~N2")'.
*     AND WO_DEALER1 <> ''.
*     AND STATUS EQ ''.

  SORT : LT_UM BY ZVIN,
         LT_07JB BY ORDR DIST EXTC INTC.
  DELETE ADJACENT DUPLICATES FROM LT_UM COMPARING ZVIN.
  DELETE LT_UM WHERE WO_DEALER1 = '' AND STATUS <> ''.


  DATA : LV_DIST LIKE ZTPP_PMT07JB_A-DIST .
  LOOP AT LT_UM .
    CLEAR : LT_DEAL.
    CONCATENATE LT_UM-WO_NATION LT_UM-WO_DEALER INTO LV_DIST.
    READ TABLE LT_07JB WITH KEY ORDR = LT_UM-WO_SERIAL
                                DIST = LV_DIST
                                EXTC = LT_UM-WO_EXTC
                                INTC = LT_UM-WO_INTC
                                BINARY SEARCH.
    IF SY-SUBRC <> 0 .
      CLEAR LT_07JB .
      CONTINUE.
    ENDIF.

    LT_DEAL-NATION = LT_UM-WO_NATION.
    LT_DEAL-MODEL  = LT_UM-MODEL_CODE .
    LT_DEAL-GUBB   = LT_07JB-GUBB. "FORE = 'B' PLAN = 'A'
    LT_DEAL-PQTY   = 1 .
    COLLECT LT_DEAL.
  ENDLOOP.

  SORT : LT_DEAL BY NATION MODEL GUBB,
         LT_KEY  BY NATION MODEL GUBB.

  LOOP AT LT_KEY.
    CLEAR : GS_DATA.
    GS_DATA-WO_NATION  = LT_KEY-NATION.
    GS_DATA-MODEL_CODE = LT_KEY-MODEL.
    GS_DATA-WO_DEALER1 = ''.
    GS_DATA-DTEXT      = 'NON'.

    CASE LT_KEY-GUBB.
      WHEN 'A'.
        READ TABLE LT_DEAL WITH KEY NATION = LT_KEY-NATION
                                    MODEL  = LT_KEY-MODEL
                                    GUBB   = 'A'
                                    BINARY SEARCH.
        IF SY-SUBRC <> 0 . CLEAR :  LT_DEAL . ENDIF.
        GS_DATA-PLQTY = LT_KEY-PQTY - LT_DEAL-PQTY.
      WHEN 'B'.
        READ TABLE LT_DEAL WITH KEY NATION = LT_KEY-NATION
                                    MODEL  = LT_KEY-MODEL
                                     GUBB   = 'B'
                                    BINARY SEARCH.
        IF SY-SUBRC <> 0 . CLEAR :  LT_DEAL . ENDIF.
        GS_DATA-FRQTY = LT_KEY-PQTY - LT_DEAL-PQTY.

    ENDCASE.
    COLLECT GS_DATA INTO LT_DATA.

    GS_DATA-WO_DEALER1 = 'X'.
    GS_DATA-DTEXT      = 'ALLOC'.
    CASE LT_KEY-GUBB.
      WHEN 'A'.
        GS_DATA-PLQTY = LT_DEAL-PQTY.
      WHEN 'B'.
        GS_DATA-FRQTY = LT_DEAL-PQTY.
    ENDCASE.
    COLLECT GS_DATA INTO LT_DATA.
  ENDLOOP.

  RANGES : R_DEALER FOR ZTPP_VM-WO_DEALER .
  R_DEALER-SIGN = 'I'.
  R_DEALER-OPTION = 'CP'.
  R_DEALER-LOW = 'A+'.
  APPEND R_DEALER .

** On 05/17/12 for SAP tuning
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_VM
*    FROM ZTPP_VM
*    WHERE RP_CSTATUS NE '25'
*      AND RP_CSTATUS < '27'
*      AND USG_CAR    = 'P'
*      AND WO_DEALER  IN R_DEALER .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_VM
        FROM ZTPP_VM
   WHERE ( RP_CSTATUS < '25' OR RP_CSTATUS  = '26') "Addition
     AND USG_CAR    = 'P'
     AND WO_DEALER  IN R_DEALER
     %_HINTS ORACLE 'INDEX (ZTPP_VM "ZTPP_VM~Z04")'.  "Addition

** End on 05/17/12

  SORT LT_VM BY MODEL_CODE BODY_NO .
  CLEAR : LT_UM[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_UM
     FROM ZTSD_UM
     FOR ALL ENTRIES IN LT_VM
     WHERE MODEL_CODE = LT_VM-MODEL_CODE
     AND BODY_NO      = LT_VM-BODY_NO
     AND WO_DEALER1 <> ''
     AND STATUS     EQ ''.

  SORT : LT_UM BY MODEL_CODE BODY_NO,
         LT_VM BY WO_NATION MODEL_CODE .

  LOOP AT LT_VM.
    CLEAR : GS_DATA.

    READ TABLE LT_UM WITH KEY MODEL_CODE = LT_VM-MODEL_CODE
                              BODY_NO    = LT_VM-BODY_NO
                              BINARY SEARCH.
    IF SY-SUBRC <> 0 .
      CLEAR : LT_UM.
    ENDIF.
    GS_DATA-WO_NATION  = LT_VM-WO_NATION.
    GS_DATA-MODEL_CODE = LT_VM-MODEL_CODE.
    GS_DATA-WO_DEALER1 = ''.
    GS_DATA-DTEXT      = 'NON'.
    COLLECT GS_DATA INTO LT_DATA.

    GS_DATA-WO_DEALER1 = 'X'.
    GS_DATA-DTEXT      = 'ALLOC'.
    COLLECT GS_DATA INTO LT_DATA.

    IF LT_UM-WO_DEALER1 IS INITIAL.
      GS_DATA-WO_DEALER1 = ''.
      GS_DATA-DTEXT      = 'NON'.
    ELSE.
      GS_DATA-WO_DEALER1 = 'X'.
      GS_DATA-DTEXT      = 'ALLOC'.
    ENDIF.

    CASE LT_VM-RP_CSTATUS .
        "SEQ Qty
      WHEN '00' .
        GS_DATA-SQQTY  = 1 .
        "Weld Qty
      WHEN '01' .
        GS_DATA-WLQTY  = 1.
        "Paint Qty
      WHEN '02' OR '03' OR '04' OR '05'.
        GS_DATA-PTQTY = 1.
        "GA Qty
      WHEN '06' OR '07' OR '08' OR '09' OR '10' OR '11' OR
           '12' OR '13' OR '14' OR '15' OR '16' OR '17'.

        GS_DATA-GAQTY   = 1.
        "S/Off Qty
      WHEN '18' .
        GS_DATA-SFQTY = 1.
        "-M/G Qty
      WHEN '19' OR '20' OR
           '21' OR '22' .
        GS_DATA-MMQTY = 1.
        "+M/G Qty
      WHEN '23' OR '24' OR '26'.
        GS_DATA-PMQTY = 1.
    ENDCASE.

    COLLECT GS_DATA INTO LT_DATA.
  ENDLOOP.

  SORT LT_DATA BY WO_NATION MODEL_CODE WO_DEALER1 .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_BMDL
  FROM  ZTPP_MODEL_CONV
  FOR ALL ENTRIES IN LT_DATA
  WHERE MODEL = LT_DATA-MODEL_CODE.

  CLEAR : GT_DATA[].
  LOOP AT LT_DATA.
    CLEAR GS_DATA.
    READ TABLE LT_BMDL WITH KEY MODEL = LT_DATA-MODEL_CODE.
    IF SY-SUBRC <> 0 . CLEAR LT_BMDL. ENDIF.
    LT_DATA-BMDL = LT_BMDL-BMDL.
    MOVE-CORRESPONDING LT_DATA TO GS_DATA.
    COLLECT GS_DATA INTO GT_DATA.
  ENDLOOP.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_CBO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_CBO_DATA.
  DATA : LT_ALLC LIKE TABLE OF ZTPP_ALLOCATION WITH HEADER LINE,
         LT_BMDL LIKE TABLE OF ZTPP_MODEL_CONV WITH HEADER LINE.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ALLC
  FROM ZTPP_ALLOCATION
  WHERE UDATE = P_DATUM
    AND UZEIT IN S_UZEIT.

  SORT LT_ALLC BY NATION MODEL_CODE ALINX
                  UDATE DESCENDING UZEIT DESCENDING.

  DELETE ADJACENT DUPLICATES FROM
  LT_ALLC COMPARING NATION MODEL_CODE ALINX.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_BMDL
   FROM  ZTPP_MODEL_CONV
   FOR ALL ENTRIES IN LT_ALLC
   WHERE MODEL = LT_ALLC-MODEL_CODE.

  LOOP AT LT_ALLC.
    WRITE LT_ALLC-UZEIT USING EDIT MASK '__:__:__' TO GV_TITLE .
    GT_DATA-WO_NATION   = LT_ALLC-NATION     .
    GT_DATA-MODEL_CODE  = LT_ALLC-MODEL_CODE .
    GT_DATA-BMDL        = LT_ALLC-MODEL_CODE+0(2).
    GT_DATA-DTEXT       = LT_ALLC-ALINX      .
    GT_DATA-FRQTY       = LT_ALLC-FRQTY      .
    GT_DATA-PLQTY       = LT_ALLC-PLQTY      .
    GT_DATA-SQQTY       = LT_ALLC-SQQTY      .
    GT_DATA-WLQTY       = LT_ALLC-WLQTY      .
    GT_DATA-PTQTY       = LT_ALLC-PNQTY      .
    GT_DATA-GAQTY       = LT_ALLC-GAQTY      .
    GT_DATA-SFQTY       = LT_ALLC-SFQTY      .
    GT_DATA-MMQTY       = LT_ALLC-MGQTY_M    .
    GT_DATA-PMQTY       = LT_ALLC-MGQTY_P    .

*    READ TABLE LT_BMDL WITH KEY MODEL = LT_ALLC-MODEL_CODE .
*    IF SY-SUBRC <> 0 . CLEAR : LT_BMDL . ENDIF.

*    GT_DATA-BMDL = LT_BMDL-BMDL.
    APPEND GT_DATA.
  ENDLOOP.

ENDFORM.                    " P2000_GET_CBO_DATA

*&---------------------------------------------------------------------*
*&      Form  P2100_CHECK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2100_CHECK_DATE.
  IF P_DATUM < SY-DATUM.
    GV_NEW = ''.
    EXIT.
  ENDIF.

  IF NOT P_DATUM     IS INITIAL AND
     NOT S_UZEIT-LOW IS INITIAL.
    GV_NEW = ''.
  ELSEIF NOT P_DATUM      IS INITIAL AND
             S_UZEIT-LOW IS INITIAL.
    IF P_DATUM >= SY-DATUM.
      GV_NEW = 'X'.
    ELSE .
      GV_NEW = ''.
    ENDIF.

  ENDIF.


ENDFORM.                    " P2100_CHECK_DATE

*&---------------------------------------------------------------------*
*&      Form  P2200_SAVE_CBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2200_SAVE_CBO.
  DATA : LT_ALLC LIKE TABLE OF ZTPP_ALLOCATION WITH HEADER LINE ,
         LV_CNT TYPE I.

  LOOP AT GT_DATA.
    MOVE-CORRESPONDING GT_DATA TO LT_ALLC.

    LT_ALLC-UDATE = SY-DATUM.
    LT_ALLC-UZEIT = SY-UZEIT.
    LT_ALLC-NATION      =     GT_DATA-WO_NATION .
    LT_ALLC-MODEL_CODE  =     GT_DATA-BMDL  ."GT_DATA-MODEL_CODE.
    LT_ALLC-ALINX       =     GT_DATA-DTEXT     .
    LT_ALLC-FRQTY       =     GT_DATA-FRQTY     .
    LT_ALLC-PLQTY       =     GT_DATA-PLQTY     .
    LT_ALLC-SQQTY       =     GT_DATA-SQQTY     .
    LT_ALLC-WLQTY       =     GT_DATA-WLQTY     .
    LT_ALLC-PNQTY       =     GT_DATA-PTQTY     .
    LT_ALLC-GAQTY       =     GT_DATA-GAQTY     .
    LT_ALLC-SFQTY       =     GT_DATA-SFQTY     .
    LT_ALLC-MGQTY_M     =     GT_DATA-MMQTY     .
    LT_ALLC-MGQTY_P     =     GT_DATA-PMQTY     .
    LT_ALLC-MEINS       =     'EA'.
    LT_ALLC-ERDAT =   SY-DATUM.
    LT_ALLC-ERZET =   SY-UZEIT.
    LT_ALLC-ERNAM =   SY-UNAME.
    LT_ALLC-AEDAT =   SY-DATUM.
    LT_ALLC-AEZET =   SY-UZEIT.
    LT_ALLC-AENAM =   SY-UNAME.


    APPEND LT_ALLC.
  ENDLOOP.

  INSERT ZTPP_ALLOCATION FROM TABLE LT_ALLC.


  IF SY-SUBRC EQ 0 .
    DESCRIBE TABLE LT_ALLC LINES LV_CNT.
    MESSAGE S001 WITH TEXT-S01 LV_CNT .
    GV_FLAG = 'X'.
  ELSE.
    MESSAGE I000 WITH TEXT-S02.
  ENDIF.
ENDFORM.                    " P2200_SAVE_CBO




*---------------------------------------------------------------------*
*       FORM P4000_CONVERSION_ATINN                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*  -->  P_ATINN                                                       *
*---------------------------------------------------------------------*
FORM P4000_CONVERSION_ATINN USING P_VALUE P_ATINN .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = P_VALUE
       IMPORTING
            OUTPUT = P_ATINN.

ENDFORM.
