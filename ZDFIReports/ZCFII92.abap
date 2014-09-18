*&---------------------------------------------------------------------
*& Report  ZCFII92
*& Author                 : WSKIM
*& Creation Date          : 10/08/2004
*& Specification By       : YC, YOON
*& Pattern                : Appropriate Request Variance Maintenance
*& Development Request No :
*& Addl documentation     :
*& Description            : AR  Upload
*&
*& Modification Log
*& Date         Developer   Request ID      Description
*& 10/18/2006   Michelle J  Andy Choi       Change Logic
*&--------------------------------------------------------------------
REPORT ZCFII92 NO STANDARD PAGE HEADING MESSAGE-ID ZMFI.

INCLUDE ZACOUI00.
INCLUDE ZCFII92_TOP.

*----------------------------------------------------------------------*
* Select-Options & Parameters
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-000.
PARAMETERS:
  P_COMP   LIKE BAPIAPPREQMASTER-RSP_COMP_CODE DEFAULT 'H201',
  P_GJAHR  LIKE BAPIAPPREQPLANYEARMULTI-FISCAL_YEAR
                                        OBLIGATORY DEFAULT '2006',
  P_MON(2) TYPE N OBLIGATORY,
  P_VERFR  LIKE IMAVZ-VERSI DEFAULT 'IM',
  P_VERTO  LIKE IMAVZ-VERSI DEFAULT 'ID'.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_VKOKRS FOR IMAK-VKOKRS,
                 S_ABUKRS FOR IMAK-ABUKRS,
                 S_USR03  FOR IMAK-USR03,
                 S_GJAHR  FOR IMAK-GJAHR,
                 S_POSNR  FOR IMAK-POSNR,
                 S_IVART  FOR IMAK-IVART.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_DP  AS CHECKBOX,
            P_ACT AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM SCREEN_MONTH.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.

  IF NOT GV_MSG IS INITIAL.
    MESSAGE S000 WITH GV_MSG.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM DISP_RESULT.

*&---------------------------------------------------------------------*
*&      Form  screen_month
*&---------------------------------------------------------------------*
FORM SCREEN_MONTH.
  CLEAR P_FLAG.
  CHECK P_MON <> 0.
  IF P_MON < 1 OR P_MON > 12.
    MESSAGE I011 WITH 'Check input data Month'.
    P_FLAG = 'X'.
  ENDIF.

ENDFORM.                    " screen_month
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_CNT   TYPE I,
        L_TAIF2 TYPE I.

  CLEAR: L_CNT, L_TAIF2, GV_MSG.

  IF P_FLAG <> 'X'.
    CLEAR IT_OUT.
    REFRESH IT_OUT.

    SELECT POSID INTO TABLE IT_OUT FROM IMAK
      WHERE POSNR  IN S_POSNR
        AND IVART  IN S_IVART
        AND VKOKRS IN S_VKOKRS
        AND ABUKRS IN S_ABUKRS
        AND USR03  IN S_USR03
        AND GJAHR  IN S_GJAHR.

    DESCRIBE TABLE IT_OUT LINES L_CNT.
  ENDIF.

  IF L_CNT > 0.
*  check : approval year
    SELECT SINGLE COUNT(*) INTO L_TAIF2
      FROM TAIF2
     WHERE GJAHR = P_GJAHR
       AND VERSI = P_VERFR.

    IF L_TAIF2 = 0.
      REFRESH IT_OUT.
      CLEAR IT_OUT.
      GV_MSG = 'No Data Found.'.

    ELSE.
      PERFORM GET_GT_OUT.
    ENDIF.

  ELSE.
    GV_MSG = 'No Data Found.'.
  ENDIF.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
FORM GET_GT_OUT.
  CLEAR: GT_OUT, GT_VARIANT_TO_VERSION, GT_PLAN, GT_PLAN1, GT_PLAN_TOT.
  REFRESH: GT_OUT, GT_VARIANT_TO_VERSION,
           GT_PLAN, GT_PLAN1, GT_PLAN_TOT.

* if select [deduct downpayment], get position information
  IF P_DP = 'X'.
    PERFORM GET_IT_IMPR.
  ENDIF.

  LOOP AT IT_OUT.
    CLEAR: GV_AR, GV_PI_PLAN, GV_PI_ACT, GV_DP.

*   Get AR
    PERFORM GET_AR.
    SORT IT_PLAN_YEAR BY FISCAL_YEAR.

*   Get PI budget
    PERFORM GET_PI_PLAN.

*   Get PI actual
    IF P_ACT = 'X'.
      PERFORM GET_PI_ACTUAL.
    ENDIF.

*   Get Downpayment
    IF P_DP = 'X'.
      PERFORM GET_DOWNPAYMENT.
    ENDIF.

*   Get Plan Values
    PERFORM GET_PLAN_VALUES.

*   Create Internal Table for Display
    GT_OUT-POSID = IT_OUT-POSID.             " AR Number
    GT_OUT-AR = GV_AR.                       " AR Amount
    GT_OUT-PI_PLAN = GV_PI_PLAN.             " PI Budjet
    GT_OUT-PI_ACT = GV_PI_ACT.               " PI Actual
    GT_OUT-DP = GT_OUT-DP.                   " Downpayment
*   Values for copy version
    GT_OUT-COST = GV_AR - GV_PI_PLAN - GV_PI_ACT - GV_DP.

    APPEND GT_OUT.
    CLEAR GT_OUT.
  ENDLOOP.

ENDFORM.                    " GET_GT_OUT.
*&---------------------------------------------------------------------*
*&      Form  GET_AR
*&---------------------------------------------------------------------*
*       Get AR
*----------------------------------------------------------------------*
FORM GET_AR.
  CLEAR: IT_VARIANT,
         IT_PLAN_TOT,
         IT_PLAN_YEAR,
         IT_INVEST_RESON,
         IT_ORG_UNITS,
         IT_INVEST_RESON,
         IT_VARIANT_TO_VERSION,
         IT_ENV_INVEST.

  REFRESH: IT_VARIANT,
           IT_PLAN_TOT,
           IT_PLAN_YEAR,
           IT_INVEST_RESON,
           IT_ORG_UNITS,
           IT_INVEST_RESON,
           IT_VARIANT_TO_VERSION,
           IT_ENV_INVEST.

  CLEAR: WA_MASTER_DATA, WA_USER_FIELD, WA_CO_AREA.

* Display Appropriation Request
  CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
       EXPORTING
            EXTERNALNUMBER     = IT_OUT-POSID
            LANGUAGE           = SY-LANGU
       IMPORTING
            MASTER_DATA        = WA_MASTER_DATA
            USER_FIELDS        = WA_USER_FIELD
            CONTROLLING_AREA   = WA_CO_AREA
       TABLES
            ORG_UNITS          = IT_ORG_UNITS
            INVEST_REASON      = IT_INVEST_RESON
            ENVIRONMNT_INVEST  = IT_ENV_INVEST
            VARIANT            = IT_VARIANT
            VARIANT_TO_VERSION = IT_VARIANT_TO_VERSION
            PLAN_TOTAL         = IT_PLAN_TOT
            PLAN_YEAR          = IT_PLAN_YEAR.

* Internal Table GT_PLAN:  for create AR variant
* Internal Table GT_PLAN1: for change valus of plan AR variant

  READ TABLE IT_VARIANT_TO_VERSION WITH KEY APPR_YEAR    = P_GJAHR
                                            PLAN_VERSION = P_VERFR.

  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING IT_VARIANT_TO_VERSION TO GT_VARIANT_TO_VERSION.
    GT_VARIANT_TO_VERSION-POSID = IT_OUT-POSID.
    APPEND GT_VARIANT_TO_VERSION.
    CLEAR GT_VARIANT_TO_VERSION.

    LOOP AT IT_PLAN_TOT
       WHERE APPREQVRNT = IT_VARIANT_TO_VERSION-APPREQVRNT.
      GV_AR = GV_AR + IT_PLAN_TOT-INVESTMENT_COSTS.
      GT_PLAN-POSID = IT_OUT-POSID.
      GT_PLAN-FISCAL_YEAR = P_GJAHR.
      GT_PLAN-INVESTMENT_COSTS = IT_PLAN_TOT-INVESTMENT_COSTS.

      MOVE-CORRESPONDING GT_PLAN TO GT_PLAN1.
      APPEND: GT_PLAN, GT_PLAN1.
      CLEAR : GT_PLAN, GT_PLAN1.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_AR
*&---------------------------------------------------------------------*
*&      Form  GET_PI_PLAN
*&---------------------------------------------------------------------*
*       PI Budget
*----------------------------------------------------------------------*
FORM GET_PI_PLAN.
  CLEAR: IT_BUDGET, GT_PI_PLAN.
  REFRESH: IT_BUDGET, GT_PI_PLAN.

* Get PI Budget
  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET'
       EXPORTING
            POSID = IT_OUT-POSID
            GJAHR = P_GJAHR
       TABLES
            OUT   = IT_BUDGET.

  LOOP AT IT_BUDGET.
*   Create Internal Table GT_PI_PLAN for PI Budget
    GT_PI_PLAN-YEAR = IT_BUDGET-GJAHR.
    GT_PI_PLAN-AMT = IT_BUDGET-PLAN.
    APPEND GT_PI_PLAN.
    CLEAR GT_PI_PLAN.

*   Variable for PI Budget by AR number
    GV_PI_PLAN = GV_PI_PLAN + IT_BUDGET-PLAN.
  ENDLOOP.

ENDFORM.                    " GET_PI_PLAN
*&---------------------------------------------------------------------*
*&      Form  GET_PI_ACTUAL
*&---------------------------------------------------------------------*
FORM GET_PI_ACTUAL.
  REFRESH: IT_PI_ACTUAL, GT_PI_ACT.
  CLEAR: IT_PI_ACTUAL, GT_PI_ACT.

* Get PI Actual value
  CALL FUNCTION 'Z_FFI_GET_PI_ACTUAL'
       EXPORTING
            POSID = IT_OUT-POSID
       TABLES
            OUT   = IT_PI_ACTUAL.

  SORT IT_PI_ACTUAL BY GJAHR.

* Overall
  LOOP AT IT_PI_ACTUAL WHERE POSID = IT_OUT-POSID
                         AND IPPOS = ' '
                         AND WRTTP = 'I'.   " Invoice

*   Variable for PI Actual value by AR no.
    GV_PI_ACT = GV_PI_ACT
                + IT_PI_ACTUAL-WTG001
                + IT_PI_ACTUAL-WTG002
                + IT_PI_ACTUAL-WTG003
                + IT_PI_ACTUAL-WTG004
                + IT_PI_ACTUAL-WTG005
                + IT_PI_ACTUAL-WTG006
                + IT_PI_ACTUAL-WTG007
                + IT_PI_ACTUAL-WTG008
                + IT_PI_ACTUAL-WTG009
                + IT_PI_ACTUAL-WTG010
                + IT_PI_ACTUAL-WTG011
                + IT_PI_ACTUAL-WTG012.

*   Create internal table GT_PI_ACT for PI Actual value
    GT_PI_ACT-YEAR = IT_PI_ACTUAL-GJAHR.

    GT_PI_ACT-AMT =  IT_PI_ACTUAL-WTG001
                    + IT_PI_ACTUAL-WTG002
                    + IT_PI_ACTUAL-WTG003
                    + IT_PI_ACTUAL-WTG004
                    + IT_PI_ACTUAL-WTG005
                    + IT_PI_ACTUAL-WTG006
                    + IT_PI_ACTUAL-WTG007
                    + IT_PI_ACTUAL-WTG008
                    + IT_PI_ACTUAL-WTG009
                    + IT_PI_ACTUAL-WTG010
                    + IT_PI_ACTUAL-WTG011
                    + IT_PI_ACTUAL-WTG012.
    APPEND GT_PI_ACT.
    CLEAR GT_PI_ACT.
  ENDLOOP.

ENDFORM.                    " GET_PI_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  GET_DOWNPAYMENT
*&---------------------------------------------------------------------*
FORM GET_DOWNPAYMENT.
  CLEAR: IT_AUFK, IT_IMZO.
  REFRESH: IT_AUFK, IT_IMZO.

* Get CO Objects from table IMZO
  READ TABLE IT_IMPR WITH KEY POSID = IT_OUT-POSID BINARY SEARCH.

  IF SY-SUBRC = 0.
    SELECT POSNR OBJNR GJAHR
      INTO TABLE IT_IMZO
      FROM IMZO
       FOR ALL ENTRIES IN IT_IMPR
     WHERE POSNR = IT_IMPR-POSNR
       AND GJAHR = IT_IMPR-GJAHR.

* Get production orders from tabel AUFK
    IF SY-SUBRC = 0.
      SELECT AUFNR OBJNR INTO TABLE IT_AUFK
        FROM AUFK
         FOR ALL ENTRIES IN IT_IMZO
       WHERE AUTYP = '01'
         AND OBJNR = IT_IMZO-OBJNR.

      CLEAR WA_T_CNT.
      DESCRIBE TABLE IT_AUFK LINES WA_T_CNT.
    ENDIF.
  ENDIF.

  IF WA_T_CNT > 0.
* Get data from previous year
    B_GJAHR =  P_GJAHR - 1.

    CLEAR IT_ORD.
    REFRESH IT_ORD.

    LOOP AT IT_AUFK.
      READ TABLE IT_IMZO WITH KEY OBJNR = IT_AUFK-OBJNR
                                  GJAHR = B_GJAHR.
      IF SY-SUBRC = 0.
        READ TABLE IT_IMPR WITH KEY POSNR = IT_IMZO-POSNR
                                    GJAHR = IT_IMZO-GJAHR.
        IF SY-SUBRC = 0.
          IT_ORD-AUFNR = IT_AUFK-AUFNR.     " Order No.
          IT_ORD-POSID = IT_OUT-POSID.      " AR No.

          APPEND IT_ORD.
          CLEAR  IT_ORD.
        ENDIF.

      ENDIF.
    ENDLOOP.

****
*  SELECT AUFNR OBJNR INTO TABLE IT_AUFK
*    FROM AUFK
*   WHERE AUTYP = '01'.
**
*  CLEAR WA_T_CNT.
*  DESCRIBE TABLE IT_AUFK LINES WA_T_CNT.
*
*  IF WA_T_CNT > 0.
*    SELECT POSNR OBJNR GJAHR
*      INTO TABLE IT_IMZO FROM IMZO
*       FOR ALL ENTRIES IN IT_AUFK
*     WHERE OBJNR = IT_AUFK-OBJNR.
*  ENDIF.
**
*  CLEAR WA_T_CNT.
*  DESCRIBE TABLE IT_IMZO LINES WA_T_CNT.
*
*  IF WA_T_CNT > 0.
*    SELECT POSNR GJAHR
*      INTO TABLE IT_IMPR
*      FROM IMPR
*       FOR ALL ENTRIES IN IT_IMZO
*     WHERE POSNR = IT_IMZO-POSNR
*       AND POSID = IT_OUT-POSID
*       AND GJAHR = IT_IMZO-GJAHR.
*  ENDIF.
*
** Get data from previous year
*    B_GJAHR =  P_GJAHR - 1.
*
*    CLEAR IT_ORD.
*    REFRESH IT_ORD.
*
*    LOOP AT IT_AUFK.
*      READ TABLE IT_IMZO WITH KEY OBJNR = IT_AUFK-OBJNR
*                                  GJAHR = B_GJAHR.
*      IF SY-SUBRC = 0.
*        READ TABLE IT_IMPR WITH KEY POSNR = IT_IMZO-POSNR
*                                    GJAHR = IT_IMZO-GJAHR.
*        IF SY-SUBRC = 0.
*          IT_ORD-AUFNR = IT_AUFK-AUFNR.
*          IT_ORD-POSID = IT_OUT-POSID.
*
*          APPEND IT_ORD.
*          CLEAR  IT_ORD.
*        ENDIF.
*
*      ENDIF.
*    ENDLOOP.
*
    SORT IT_ORD BY AUFNR.

    CLEAR GT_DP.
    REFRESH GT_DP.
    LOOP AT IT_ORD.
*   get i/o budget
      AT NEW AUFNR.
        REFRESH IT_ACTUAL.
        CLEAR IT_ACTUAL.

        CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
             EXPORTING
                  AUFNR = IT_ORD-AUFNR
             TABLES
                  OUT   = IT_ACTUAL.
      ENDAT.

      DELETE IT_ACTUAL WHERE WRTTP <> '12'.

      IF NOT IT_ACTUAL[] IS INITIAL.
        PERFORM GET_GT_DP.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_DOWNPAYMENT
*&---------------------------------------------------------------------*
*&      Form  GET_GT_DP
*&---------------------------------------------------------------------*
FORM GET_GT_DP.
  DATA: M_NUM(3) TYPE N,
        C_NUM(3) TYPE N,
        FIELD(16),
        D_SUM LIKE IT_ACTUAL-TOT .

  CLEAR: M_NUM,C_NUM,D_SUM.

  FIELD-SYMBOLS <FS> TYPE ANY.

  LOOP AT IT_ACTUAL WHERE WRTTP = '12'.   " Downpayment
    IF IT_ACTUAL-GJAHR <> SY-DATUM(4).
      GT_DP-YEAR = IT_ACTUAL-GJAHR.
      GT_DP-AMT = IT_ACTUAL-TOT.
      APPEND GT_DP.

      GV_DP = GV_DP + IT_ACTUAL-TOT.
    ELSE.
      M_NUM = 1.
      DO P_MON TIMES.
        CONCATENATE 'IT_ACTUAL-' 'WTG' M_NUM INTO FIELD.
        ASSIGN (FIELD) TO <FS>.
        D_SUM = D_SUM + <FS>.
        M_NUM = M_NUM + 1.
      ENDDO.

      GT_DP-YEAR = IT_ACTUAL-GJAHR.
      GT_DP-AMT = D_SUM.
      APPEND GT_DP.

      GV_DP = GV_DP + IT_ACTUAL-TOT.

      CLEAR: D_SUM,M_NUM.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_GT_DP
*&---------------------------------------------------------------------*
*&      Form  GET_PLAN_VALUES
*&---------------------------------------------------------------------*
FORM GET_PLAN_VALUES.
  DATA: L_PI_PLAN TYPE BP_WJT,   " PI Budjet Amount
        L_PI_ACT  TYPE WTGXXX,   " PI Actual Amount
        L_DP      TYPE WTGXXX.   " Downpayment Amount

  GT_PLAN_TOT-POSID = IT_OUT-POSID.

  LOOP AT IT_PLAN_YEAR.
    CLEAR: L_PI_PLAN, L_PI_ACT, L_DP.

*   PI Budjet
    READ TABLE GT_PI_PLAN WITH KEY YEAR = IT_PLAN_YEAR-FISCAL_YEAR.
    IF SY-SUBRC = 0.
      L_PI_PLAN = L_PI_PLAN + GT_PI_PLAN-AMT.
    ENDIF.

*   PI Actual
    IF P_ACT = 'X'.
      READ TABLE GT_PI_ACT WITH KEY YEAR = IT_PLAN_YEAR-FISCAL_YEAR.
      IF SY-SUBRC = 0.
        L_PI_ACT = L_PI_ACT + GT_PI_ACT-AMT.
      ENDIF.
    ENDIF.

*   Downpayment
    IF P_DP = 'X'.
      READ TABLE GT_DP WITH KEY YEAR = IT_PLAN_YEAR-FISCAL_YEAR.
      IF SY-SUBRC = 0.
        L_DP = L_DP + GT_DP-AMT.
      ENDIF.
    ENDIF.

    IF NOT GT_PI_PLAN-AMT IS INITIAL OR
       NOT GT_PI_ACT-AMT IS INITIAL OR
       NOT GT_DP-AMT IS INITIAL.

*     Collect internal table for change valus of plan for AR variant
      GT_PLAN1-POSID = IT_OUT-POSID.
      GT_PLAN1-FISCAL_YEAR = IT_PLAN_YEAR-FISCAL_YEAR.

*     Investment cost: AR - PI Budjet - PI Actual - Downpament
      GT_PLAN1-INVESTMENT_COSTS = IT_PLAN_YEAR-INVESTMENT_COSTS
                                  - GT_PI_PLAN-AMT
                                  - GT_PI_ACT-AMT
                                  - GT_DP-AMT.

      COLLECT: GT_PLAN1, GT_PLAN_TOT.
      CLEAR: GT_PLAN1, GT_PLAN_TOT.
    ENDIF.

  ENDLOOP.

  GT_PLAN_TOT-INVESTMENT_COSTS = GV_AR + GV_PI_PLAN - GV_PI_ACT - GV_DP.

ENDFORM.                    " GET_PLAN_VALUES
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
FORM DISP_RESULT.
  CLEAR: GT_FIELDCAT, GS_LAYOUT, GT_EVENTS, GT_FIELDCAT[], GT_EVENTS[].

  PERFORM BUILD_FIELD_CATEGORY USING:
    'POSID'    'X'  'AR'                         10  'CHAR',
    'AR'       ' '  'AR Plan'                    23  'DEC',
    'PI_PLAN'  ' '  'PI Budget'                  15  'CURR',
    'PI_ACT'   ' '  'PI Actual'                  15  'CURR',
    'DP'       ' '  'Downpayment'                15  'CURR',
    'COST'     ' '  'Values for copy version'    15  'CURR',
    'CICON'    ' '  'Status:create variant'      1   'ICON',
    'MSG'      ' '  'Message of create variant'  25  'CHAR',
    'SICON'    ' '  'Status:copy variant to target'    1   'CHAR',
    'MSG2'     ' '  'Message of create variant'  25  'ICON'.

  GS_FIELDCAT-DECIMALS_OUT = 4.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING DECIMALS_OUT
         WHERE FIELDNAME = 'AR'.

  GS_FIELDCAT-DECIMALS_OUT = 2.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING DECIMALS_OUT
         WHERE DATATYPE = 'CURR'.

  GS_FIELDCAT-ICON = 'X'.
  MODIFY GT_FIELDCAT FROM GS_FIELDCAT TRANSPORTING DECIMALS_OUT
         WHERE DATATYPE = 'ICON'.

  CLEAR GS_LAYOUT.
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-BOX_FIELDNAME     = 'CHK'.

  PERFORM COMMENT1 USING GT_LIST_TOP_OF_PAGE.
  PERFORM SET_EVENTS CHANGING GT_EVENTS.

  GV_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GC_PF_STATUS_SET
            I_CALLBACK_USER_COMMAND  = GC_USER_COMMAND
            IS_LAYOUT                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            IT_EVENTS                = GT_EVENTS[]
            I_SAVE                   = 'A'
       TABLES
            T_OUTTAB                 = GT_OUT.

ENDFORM.                    " DISP_RESULT
*&---------------------------------------------------------------------*
*&      Form  COMMENT
*&---------------------------------------------------------------------*
FORM COMMENT1 USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA LS_LINE TYPE SLIS_LISTHEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.

  IF P_DP = 'X'.
    LS_LINE-INFO = '* Deduct Downpayment'.

    IF P_ACT = 'X'.
      LS_LINE-INFO = '* Deduct Downpayment & Actual'.
    ELSE.
      LS_LINE-INFO = '* Deduct Actual'.
    ENDIF.
    APPEND LS_LINE TO LT_TOP_OF_PAGE.

  ELSE.
    IF P_ACT = 'X'.
      LS_LINE-INFO = '* Deduct Actual'.
    ELSE.
      LS_LINE-INFO = '* Same with Source Version'.
    ENDIF.

    APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Company'.
  LS_LINE-INFO = P_COMP.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Planning year'.
  LS_LINE-INFO = P_GJAHR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Month/Fiscal Year:'.
  LS_LINE-INFO = P_MON.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Calculation Month:Current year'.
  LS_LINE-INFO = P_VERFR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'Source'.
  LS_LINE-INFO = P_VERTO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CLEAR: LS_LINE-KEY, LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    " COMMENT
*&-------------------------------------------------------------------*
*&      USER_COMMAND
*&-------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
* Create Variant: if have no assignment of approp. request variant to
*                 plan version, create appropriation request variant
  IF R_UCOMM = 'CRAT'.
    LOOP AT GT_OUT WHERE CHK = 'X'.
      READ TABLE GT_PLAN WITH KEY POSID = GT_OUT-POSID.
      IF SY-SUBRC <> 0.
        PERFORM CREAT_VARIANT.
      ELSE.
        GT_OUT-CRV = 'W'.
        GT_OUT-CICON = ICON_LED_YELLOW.
        GT_OUT-MSG = 'Created AR variant aleady'.
      ENDIF.

      MODIFY GT_OUT.
    ENDLOOP.

    RS_SELFIELD-REFRESH = 'X'.

* Copy to target version by source version
** : Change Plan Values of Appropriation Request Variant
  ELSEIF R_UCOMM = 'COPY'.
    LOOP AT GT_OUT WHERE CHK = 'X'.
      PERFORM SET_PLAN_VALUES.
    ENDLOOP.

    RS_SELFIELD-REFRESH = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREAT_VARIANT
*&---------------------------------------------------------------------*
*       Create Variant
*----------------------------------------------------------------------*
FORM CREAT_VARIANT.
  DATA: LT_VARIANT_TO_VERSION TYPE TABLE OF BAPIAPPREQVARNTASSIGN
                                                  WITH HEADER LINE,
        LT_PLAN_YEAR TYPE TABLE OF BAPIAPPREQPLANYEAR WITH HEADER LINE,
        LT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE.

  CLEAR: LT_VARIANT_TO_VERSION, LT_PLAN_YEAR, LT_RETURN.
  REFRESH: LT_VARIANT_TO_VERSION, LT_PLAN_YEAR, LT_RETURN.

* Create Appropriation Request Variant
  CALL FUNCTION 'BAPI_APPREQUEST_ADDVARIANT'
       EXPORTING
            EXTERNALNUMBER     = GT_OUT-POSID
       TABLES
            VARIANT_TO_VERSION = LT_VARIANT_TO_VERSION
            PLAN_YEAR          = LT_PLAN_YEAR
            RETURN             = LT_RETURN.

* Get message
  READ TABLE LT_RETURN WITH KEY TYPE = 'E'.

  IF SY-SUBRC <> 0.
    GT_OUT-CRV = 'S'.
    GT_OUT-CICON = ICON_LED_GREEN.
    GT_OUT-MSG = 'Created plan values of AR variant'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  ELSE.
    GT_OUT-CRV = 'E'.
    GT_OUT-CICON = ICON_LED_RED.
    GT_OUT-MSG = 'Failed create plan values of AR variant'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.                    " CREAT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_PLAN_VALUES
*&---------------------------------------------------------------------*
*       Change Plan Values of Appropriation Request Variant
*----------------------------------------------------------------------*
FORM SET_PLAN_VALUES.
  DATA LT_PLAN TYPE TABLE OF BAPIAPPREQPLANYEAR WITH HEADER LINE.

  READ TABLE GT_VARIANT_TO_VERSION WITH KEY POSID = GT_OUT-POSID.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  LOOP AT GT_VARIANT_TO_VERSION FROM SY-TABIX.
    IF GT_VARIANT_TO_VERSION-POSID = GT_OUT-POSID.
      CLEAR: PLAN_TOTAL, LT_PLAN, IT_RETURN.
      REFRESH: LT_PLAN, IT_RETURN.

*     Overall Plan Values by AR
      READ TABLE GT_PLAN_TOT WITH KEY POSID = GT_OUT-POSID
                          APPREQVRNT = IT_VARIANT_TO_VERSION-APPREQVRNT.

      IF SY-SUBRC = 0.
        PLAN_TOTAL-INVESTMENT_COSTS = GT_PLAN_TOT-INVESTMENT_COSTS.
      ENDIF.

*    Create internal table LT_PLAN for annual plan values by AR
      READ TABLE GT_PLAN1 WITH KEY POSID = GT_OUT-POSID.
      LOOP AT GT_PLAN1 FROM SY-TABIX.
        IF GT_PLAN1-POSID = GT_OUT-POSID.
          MOVE-CORRESPONDING GT_PLAN1 TO LT_PLAN.
          APPEND LT_PLAN.
          CLEAR LT_PLAN.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

*     Change Plan Values of Appropriation Request Variant
      CALL FUNCTION 'BAPI_APPREQUEST_SETPLANVALUES'
           EXPORTING
              EXTERNALNUMBER = GT_OUT-POSID
              APPROPRIATIONREQUESTVARIANT =
                                 GT_VARIANT_TO_VERSION-APPREQVRNT
              PLAN_TOTAL = PLAN_TOTAL
           TABLES
              PLAN_YEAR = LT_PLAN
              RETURN = IT_RETURN.

*     Get message
      READ TABLE IT_RETURN WITH KEY TYPE = 'E'.

      IF SY-SUBRC <> 0.
        GT_OUT-SETV = 'S'.
        GT_OUT-SICON = ICON_LED_GREEN.
        GT_OUT-MSG2 = 'Changed plan values of AR variant'.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      ELSE.
        GT_OUT-SETV = 'E'.
        GT_OUT-SICON = ICON_LED_RED.
        GT_OUT-MSG2 = 'Failed change plan values of AR variant'.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

    ELSE.
      EXIT.
    ENDIF.

  ENDLOOP.

  MODIFY GT_OUT.

ENDFORM.                    " SET_PLAN_VALUES
*&---------------------------------------------------------------------*
*&      Form  GET_IT_IMPR
*&---------------------------------------------------------------------*
*       Get position information from table IMPR
*----------------------------------------------------------------------*
FORM GET_IT_IMPR.
  CLEAR IT_IMPR.
  REFRESH IT_IMPR.

* Get position information from table IMPR
  SELECT POSID POSNR GJAHR
    INTO TABLE IT_IMPR
    FROM IMPR
    FOR ALL ENTRIES IN IT_OUT
   WHERE POSID = IT_OUT-POSID.

  SORT IT_IMPR BY POSID.

ENDFORM.                    " GET_IT_IMPR
