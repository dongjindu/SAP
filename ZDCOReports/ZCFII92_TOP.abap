*----------------------------------------------------------------------*
*   INCLUDE ZCFII92_TOP                                                *
*----------------------------------------------------------------------*
* Define Tables
TABLES: IMAK,
        TAIF2.

* Define Types for Internal tables
TYPES: BEGIN OF TY_POSID,
         POSID TYPE IMA_POSID,   " AR Number
       END OF TY_POSID.

TYPES: BEGIN OF TY_AUFK,
         AUFNR TYPE AUFNR,
         OBJNR TYPE J_OBJNR,
       END OF TY_AUFK.

TYPES: BEGIN OF TY_IMZO,
         POSNR TYPE IM_POSNR,
         OBJNR TYPE IM_OBJNR,
         GJAHR TYPE IM_GNJHR,
       END OF TY_IMZO.

TYPES: BEGIN OF TY_IMPR,
         POSID TYPE IM_POSID,
         POSNR TYPE IM_POSNR,
         GJAHR TYPE IM_GNJHR,
       END OF TY_IMPR.

TYPES: BEGIN OF TY_ORD,
         AUFNR TYPE AUFNR,      " Production Order No.
         POSID TYPE IM_POSID,   " ID for an investment program position
       END OF TY_ORD.

TYPES: BEGIN OF TY_AMT,
         YEAR TYPE GJAHR,       " Year
         AMT  TYPE WTGXXX,      " Amount
       END OF TY_AMT.

TYPES: BEGIN OF TY_TVSN,
         POSID TYPE IMA_POSID.
INCLUDE TYPE BAPIAPPREQVARNTASSIGNMULTI.
TYPES  END OF TY_TVSN.

TYPES: BEGIN OF TY_PLAN_TOT,
         POSID TYPE IMA_POSID.
INCLUDE TYPE BAPIAPPREQPLANTOTALMULTI.
TYPES  END OF TY_PLAN_TOT.

TYPES: BEGIN OF TY_PLAN,
         POSID TYPE IMA_POSID.
         INCLUDE TYPE BAPIAPPREQPLANYEAR.
TYPES  END OF TY_PLAN.

TYPES: BEGIN OF TY_RETURN,
         POSID       TYPE IMA_POSID.
         INCLUDE     TYPE BAPIRET2.
TYPES:   MESSAGE2    TYPE BAPI_MSG,
         MESSAGE2_V1 TYPE SYMSGV,
       END OF TY_RETURN.

TYPES: BEGIN OF TY_OUT,
         POSID   TYPE IMA_POSID, " ID for an investment program position
         AR      TYPE BAPICURR_D,      " AR Amount
         PI_PLAN TYPE WTGXXX,          " PI Budjet
         PI_ACT  TYPE WTGXXX,          " PI Actual
         DP      TYPE WTGXXX,          " Downpayment Amount
         COST    TYPE BAPICURR_D,      " Values for copy version
         CRV,         " Flag of create AR variant(S:Success, F:Fail)
         MSG(50),     " Message of create AR variant
         SETV,        " Flag of copy vatiant to target version
         MSG2(50),    " Message of change AR variant
         CICON   TYPE ICON_D, " create AR variant status
         SICON   TYPE ICON_D, " copy vatiant to target version status
         CHK,
       END OF TY_OUT.

* Internal Tables for BAPI Functions
DATA: IT_OUT TYPE TABLE OF TY_POSID WITH HEADER LINE,
      IT_VARIANT TYPE TABLE OF BAPIAPPREQVARNTMULTI WITH HEADER LINE,
      IT_VARIANT_TO_VERSION TYPE TABLE OF BAPIAPPREQVARNTASSIGNMULTI
                                          WITH HEADER LINE,
      IT_INVEST_RESON TYPE TABLE OF BAPIAPPREQINVREASON
                                    WITH HEADER LINE,
      IT_ENV_INVEST TYPE TABLE OF BAPIAPPREQENVINVEST WITH HEADER LINE,
      IT_ORG_UNITS TYPE TABLE OF BAPIAPPREQORGUNIT WITH HEADER LINE,
      IT_PLAN  TYPE TABLE OF BAPIAPPREQPLANYEAR WITH HEADER LINE,
      IT_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
      IT_PLAN_TOT TYPE TABLE OF BAPIAPPREQPLANTOTALMULTI
                                WITH HEADER LINE,
      IT_PLAN_YEAR TYPE TABLE OF BAPIAPPREQPLANYEARMULTI
                                WITH HEADER LINE.

DATA: WA_T_CNT TYPE I,
      WA_MASTER_DATA LIKE	BAPIAPPREQMASTER,
      WA_USER_FIELD  LIKE BAPIAPPREQUSER,
      WA_PARENT      LIKE BAPIPROGAUX-PARENT,
      WA_CO_AREA     LIKE BAPI_APPREQ_ID-CNTRL_AREA.

* Internal Tables for downpayment
DATA: IT_PI_ACTUAL TYPE TABLE OF ZFI_PI_ACTUAL WITH HEADER LINE,
      IT_AUFK      TYPE TABLE OF TY_AUFK       WITH HEADER LINE,
      IT_IMZO      TYPE TABLE OF TY_IMZO       WITH HEADER LINE,
      IT_IMPR      TYPE TABLE OF TY_IMPR       WITH HEADER LINE,
      IT_ORD       TYPE TABLE OF TY_ORD        WITH HEADER LINE,
      IT_ACTUAL    TYPE TABLE OF ZFI_IO_ACTUAL WITH HEADER LINE,
      IT_BUDGET    TYPE TABLE OF ZFI_PI_BUDGET WITH HEADER LINE,
      IT_PACTUAL   TYPE TABLE OF ZFI_PI_ACTUAL_ACT WITH HEADER LINE.

* Internal Tables for create AR variant
DATA: GT_VARIANT_TO_VERSION TYPE TABLE OF TY_TVSN WITH HEADER LINE,
      GT_PLAN_TOT           TYPE TABLE OF TY_PLAN_TOT WITH HEADER LINE,
      GT_PLAN               TYPE TABLE OF TY_PLAN WITH HEADER LINE,
      GT_PLAN1              TYPE TABLE OF TY_PLAN WITH HEADER LINE.

* Internal Tables for copy to target version
DATA: GT_PI_PLAN TYPE TABLE OF TY_AMT WITH HEADER LINE, " PI Budget
      GT_PI_ACT  TYPE TABLE OF TY_AMT WITH HEADER LINE, " PI Actual
      GT_DP      TYPE TABLE OF TY_AMT WITH HEADER LINE, " Downpayment
      GT_OUT     TYPE TABLE OF TY_OUT WITH HEADER LINE. " for Display

DATA: GV_MSG(20),
      P_FLAG,
      B_GJAHR    LIKE BAPIAPPREQPLANYEARMULTI-FISCAL_YEAR,
      PLAN_TOTAL LIKE BAPIAPPREQPLANTOTAL,
      GV_PI_PLAN TYPE BP_WJT,
      GV_PI_ACT  TYPE BP_WJT,
      GV_AR      TYPE BAPICURR_D,      " AR Amount
      GV_DP      TYPE WTGXXX.          " Downpayment Amount
