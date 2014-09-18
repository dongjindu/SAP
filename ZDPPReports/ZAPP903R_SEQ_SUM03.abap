************************************************************************
* Program Name      : ZAPP903R_SEQ_SUM03
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K930702,
* Addl Documentation:
* Description       : This program is copy from ZAPP903R_SEQ_SUM03 for
*                     performance issue
*          Sequence Parts Summary (Bucket: Weekly, Horz.: 21 Weeks)
* Modification Logs : this program is copy from
* Date       Developer    RequestNo    Description
*       Change the Time Tule(Tack-Time --> Lead Time)
*
************************************************************************
REPORT  ZAPP903R_SEQ_SUM03    MESSAGE-ID ZMPP.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ZTPP_COMMON_VALS,
        ZTPP_INPUT_PLAN,
        AUSP .

*----------------------------------------------------------------------
* Gloval Variables Definition
*----------------------------------------------------------------------
*DATA: wa_uph_b                TYPE zvpp_ld-lrate,
*      wa_uph_p                TYPE zvpp_ld-lrate,
*      wa_uph_t                TYPE zvpp_ld-lrate.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF IT_DATA        OCCURS 0,
        OBJEK                 LIKE AUSP-OBJEK.      " Vehicle Code
        INCLUDE STRUCTURE     ZTPP_INPUT_PLAN.
DATA: END OF IT_DATA.

DATA: BEGIN OF IS_PROD        OCCURS 0,
        OBJEK                 LIKE AUSP-OBJEK.      " Vehicle Code
        INCLUDE STRUCTURE     ZTPP_DAY_SUM   .
DATA:   CNT                   TYPE I         ,
        MODEL(4)              TYPE C         ,
      END OF IS_PROD.
DATA: C_FACTOR(13)            TYPE P DECIMALS 3 VALUE 1.
DATA: BEGIN OF IT_ALC         OCCURS 0.
        INCLUDE STRUCTURE     CUKB    .
DATA:   MODEL(3)              TYPE C  ,
        KNKTX                 LIKE CUKBT-KNKTX,
        CODE(3)               TYPE C  ,
        RP(2)                 TYPE N  ,
        TYPE_ALC              TYPE C  ,
        CHAR_ALC              LIKE CABN-ATNAM,
        FULL_ALC(4)           TYPE C  ,
      END OF IT_ALC .

DATA: BEGIN OF IT_MODEL       OCCURS 0,
        MODL                  TYPE ZPP_MODEL,
      END OF IT_MODEL.

DATA: BEGIN OF IT_SUM         OCCURS 0,
        RP(2)                 TYPE N  ,
        ALC(9)                TYPE C  ,              " For Summary Field
        WORDER                LIKE MARA-MATNR,
        KNNAM                 LIKE CUKB-KNNAM,
        STATUS                LIKE ZTPP_INPUT_PLAN-STATUS,
        CODE(4)               TYPE C  ,              " ALC CODE
        VALS(5)               TYPE C  ,              " ALC CODE VALUE
        DAYS                  TYPE P DECIMALS 0         ,
        VM_MODEL              LIKE ZTPP_INPUT_PLAN-MODL ,
        VM_BODYSER            LIKE ZTPP_INPUT_PLAN-BODY_SER,
        EXTC                  LIKE ZTPP_INPUT_PLAN-EXTC,
        INTC                  LIKE ZTPP_INPUT_PLAN-INTC,
        MITU                  TYPE ZMITU,
        MITUCNT               TYPE I  ,
        CNT                   TYPE I  ,
        SERIAL                LIKE ZTPP_INPUT_PLAN-SERIAL,
      END OF IT_SUM .

*DATA: BEGIN OF it_seq         OCCURS 0.
*        INCLUDE STRUCTURE     zspp_vm_gen.
*DATA:   alc(11)               TYPE c  ,              " For Summary Fiel
*        knnam                 LIKE cukb-knnam,
*        cnt                   TYPE i  ,
*        mitu                  TYPE i  ,
*        code(4)               TYPE c  ,              " ALC CODE
*        vals(5)               TYPE c  ,              " ALC CODE VALUE
*      END OF it_seq.

DATA: BEGIN OF IT_MASTER  OCCURS 0,
        SEQ               TYPE I  ,             " Sequence
        DATE              TYPE D  ,             " Date
        DAY               LIKE KAPA-TAGNR,      " Day
        SHIFT             LIKE KAPA-SCHNR,      " Shift
        TIME              TYPE KAPENDZT  ,      " Times for working
        UPH               TYPE ZVPP_LD-LRATE,   " UPH
      END OF IT_MASTER.
* requested by my hur changed by chris
DATA: IT_FWO              LIKE ZTPP_FWO OCCURS 0       WITH HEADER LINE.

*end of change on 02/23/2005
DATA: IT_PROD             LIKE TABLE OF IS_PROD        WITH HEADER LINE,
      IT_SEQ              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_D1               LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_BI               LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_WBS              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_PI               LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_PRJ              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_PBS              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_D21              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_DISP_D1          LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP_SEQ         LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP_BI          LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP_PI          LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP_PRJ         LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP_WBS         LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP_PBS         LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP_D21         LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE,
      IT_DISP             LIKE TABLE OF ZTPP_SEQ_SUM03 WITH HEADER LINE.
DATA: LT_FWO              LIKE ZTPP_FWO OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: WA_DISP             LIKE IT_DISP                             ,
      WA_DATA             LIKE IT_DATA                             ,
      WA_WDATE            LIKE SY-DATUM                            ,
      WA_KALID            LIKE KAKO-KALID                          ,
      WA_REPID            LIKE SY-REPID                            ,
      WA_UZEIT            LIKE SY-UZEIT                            ,
      WA_INDEX            LIKE SY-TABIX                            ,
      WA_MODEL(3)         TYPE C                                   ,
      WA_ERROR            TYPE C                                   ,
      WA_FLAG             TYPE C                                   ,
      WA_DAYS             TYPE I                                   .
DATA: G_MRP_LAST_WEEK      TYPE I.

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <WA_DFIELD>    TYPE ANY.


*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: C_JOBS(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      C_KEY1(18)              VALUE 'SEQ_SUM03' .

DATA: BEGIN OF IT_CHAR OCCURS 0,
      ATNAM LIKE CABN-ATNAM,
      ATINN LIKE CABN-ATINN,
      END OF IT_CHAR.


*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
*ARAMETERS: p_model(3)    TYPE c        ,
*           p_uph(2)      TYPE n        OBLIGATORY.
PARAMETERS: P_DATES       TYPE D        OBLIGATORY,
            P_TEST        TYPE C                  ,
            P_MITU        TYPE C,
            P_FWO        TYPE C.
SELECTION-SCREEN END OF BLOCK B3.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  DATA: L_VALS                LIKE ZTPP_COMMON_VALS-ITEM4.

  " Get the Date for the Production Reporting Date(Last Date)
* requested by MY Mur changed by chris
*  PERFORM get_start_day     USING wa_wdate     .
*  p_dates = wa_wdate     .
  P_DATES  = WA_WDATE = SY-DATUM.
* end of change on 03/03/2005
  WA_WDATE = WA_WDATE - 1.  WA_REPID = SY-REPID.
  PERFORM READ_SHOP_CALID   USING WA_KALID.
  PERFORM READ_WORKING_DATE USING '-'  WA_KALID  WA_WDATE.


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM CLEAR_VARIABLE               .
  PERFORM SET_INFORMATION              .
  PERFORM READ_INPUTPLAN               .
  PERFORM READ_ALC_MODEL               .
  PERFORM CREATE_SUMMARY               .
  PERFORM INSERT_FIELD_VALS            .
  PERFORM SUMMARY_DISP_FINAL           .
  PERFORM DISPLAY_DATA                 .

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VARIABLE.
  CLEAR: IT_DATA,   IT_ALC,   IT_SUM,   IT_DISP,
         IT_DATA[], IT_ALC[], IT_SUM[], IT_DISP[],
         WA_DATA,  WA_UZEIT, WA_INDEX, WA_DAYS.
ENDFORM.                    " CLEAR_VARIABLE

*&---------------------------------------------------------------------*
*&      Form  READ_INPUTPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_INPUTPLAN.
  DATA: L_LEN TYPE I,
          L_NEW_DEALER(1).

  DATA: LT_DATA           LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE,
        L_SERIAL          LIKE ZTPP_INPUT_PLAN-SERIAL,
        L_LOOPS           TYPE I       ,
        LF_DATE           LIKE SY-DATUM,
        LT_DATE           LIKE SY-DATUM.

  IF P_MITU = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_INPUT_PLAN
    WHERE NOT ( MITU EQ 'X' OR MITU EQ 'Y' ).
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_INPUT_PLAN .
  ENDIF.

  " Append the it_data for the Weekly Data in ZTPP_PMT07JB_A..
  SELECT MAX( SERIAL ) INTO L_SERIAL
    FROM ZTPP_INPUT_PLAN .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM ZTPP_PMT07JB_A
      WHERE GUBB = 'B'.


*  DELETE lt_data WHERE gubb = '*'.
*  DELETE lt_data WHERE gubb = 'A'.
  SORT LT_DATA BY SQDT SSR1      .

* requested by MY. HUR changed by chris
* note: We didn't have alc code for forcast workorder, so when
*       doing the alc summary for forcast work order, no alc calue
*       for those qty. So new table ztpp_fwo is created for store
*       the alc code for forcast workorder. The forcast workorder
*       is the same with actual workoder that has the same FSC code

* end of change on 02/23/2005


  PERFORM CHECK_CREATE_FWO TABLES LT_DATA.


  LOOP AT LT_DATA.
* requested by MY. HUR changed by CHRIS
*    IF lt_data-ordr(1) = 'F'.
*      DELETE lt_data.           " Except the Forecast Order....
*      CONTINUE.
*    ENDIF.
* end of change on 02/21/2005

    L_LOOPS = LT_DATA-PQTY .
    DO L_LOOPS TIMES.
      L_SERIAL           = L_SERIAL + 1 .
      IT_DATA-PLNT       = LT_DATA-PLNT .
      IT_DATA-LINE       = LT_DATA-LINE .
      IT_DATA-MODL       = LT_DATA-MODL .
*      IT_DATA-BODY_SER   = LT_DATA-PLNT .
      IT_DATA-SEQ_DATE   = LT_DATA-SQDT .
      IT_DATA-SEQ_SERIAL = LT_DATA-SSR1 .
      IT_DATA-SEQ_CODE   = LT_DATA-SQCD .
      IT_DATA-SERIAL     = L_SERIAL     .

      CONCATENATE LT_DATA-ORDR LT_DATA-DIST INTO IT_DATA-WORK_ORDER.

      IT_DATA-EXTC       = LT_DATA-EXTC .
      IT_DATA-INTC       = LT_DATA-INTC .
      IT_DATA-MI         = LT_DATA-BMDL .
      IT_DATA-OCNN       = LT_DATA-OCNN .
      IT_DATA-VERS       = LT_DATA-VERS .
      IT_DATA-STATUS     = 'B'.
      APPEND IT_DATA.
    ENDDO.
  ENDLOOP.

  " Elemenate the TEST-CAR that Dealer code is 'XX', 'XY' .
  IF P_TEST = 'X'.
    LOOP AT IT_DATA.
      IF IT_DATA-WORK_ORDER+12(2) = 'XX' OR
         IT_DATA-WORK_ORDER+12(2) = 'XY' .
        DELETE IT_DATA.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE IT_DATA LINES  WA_DAYS .
  IF WA_DAYS = 0.
    DELETE FROM ZTPP_SEQ_SUM03 CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
    LEAVE PROGRAM .
  ENDIF.

  PERFORM GET_WEEKS USING '-' LF_DATE LT_DATE .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PROD
    FROM ZTPP_DAY_SUM
   WHERE WDATE >= LF_DATE
     AND WDATE <= LT_DATE
     AND DEALER NE 'XX'       "ADDED BY CHRIS ON 02/21/2005
     AND DEALER NE 'XY'.      "ADDED BY CHRIS ON 02/21/2005

  SORT IT_DATA BY RSNUM SERIAL .
*---start#1 wskim : check holding car
  PERFORM CHECK_HOLDING_CAR.
*---end
  DATA: L_FWOCL LIKE ZTPP_FWO-WORDER,
        L_WORDER LIKE ZTPP_FWO-O_WORDER,
        L_ORDR LIKE ZTPP_PMT07JB_A-ORDR,
        L_DIST LIKE ZTPP_PMT07JB_A-DIST,
        L_FSC LIKE ZTPP_WOSUM-FSC,
        L_YEAR LIKE ZTPP_PMT07JB_A-MOYE.
*  DATA: lt_fwo LIKE TABLE OF ztpp_fwo WITH HEADER LINE.

  IF NOT P_FWO IS INITIAL.
*    SELECT DISTINCT model worder fsc version o_worder
*           INTO CORRESPONDING FIELDS OF TABLE lt_fwo FROM ztpp_fwo.
** change on 05/15/2006 requestd by ignacio for performance issue.
    SORT IT_DATA BY WORK_ORDER.
*    LOOP AT it_data.
    LOOP AT IT_DATA WHERE WORK_ORDER+0(1) = 'F'.
*      IF it_data-work_order+0(1) = 'F'.
      CONCATENATE IT_DATA-WORK_ORDER IT_DATA-EXTC IT_DATA-INTC
             INTO L_FWOCL.
      READ TABLE LT_FWO WITH KEY WORDER = L_FWOCL.
*        SELECT SINGLE o_worder INTO l_worder
*          FROM ztpp_fwo
*          WHERE worder = l_fwocl.
      IF SY-SUBRC = 0.
        IT_DATA-WORK_ORDER = LT_FWO-O_WORDER.
*          it_data-work_order = l_worder.
        MODIFY IT_DATA.
      ELSE.
* ADDED BY FURONG ON 03/27/06
        L_ORDR = IT_DATA-WORK_ORDER+0(9).
        L_DIST = IT_DATA-WORK_ORDER+9(5).

        SELECT SINGLE MOYE INTO L_YEAR
          FROM ZTPP_PMT07JB_A
         WHERE ORDR = L_ORDR
           AND DIST = L_DIST
           AND EXTC = IT_DATA-EXTC
           AND INTC = IT_DATA-INTC.
** Changed by Furong on 10/10/07 for EBOM
*        CONCATENATE l_year l_dist it_data-mi INTO l_fsc.
*        CONCATENATE l_fsc it_data-ocnn INTO l_fsc
*                SEPARATED BY space.
        L_LEN = STRLEN( IT_DATA-MI ).
        IF L_LEN = 7.
          CONCATENATE L_YEAR L_DIST IT_DATA-MI INTO L_FSC.
          CONCATENATE L_FSC IT_DATA-OCNN INTO L_FSC
                  SEPARATED BY SPACE.
        ELSE.
          CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
               EXPORTING
                    OLD_DEALER = L_DIST+3(2)
               IMPORTING
                    NEW_DEALER = L_NEW_DEALER.
          CONCATENATE L_YEAR L_DIST+0(3) L_NEW_DEALER IT_DATA-MI
                      INTO L_FSC.
          CONCATENATE L_FSC IT_DATA-OCNN INTO L_FSC.
        ENDIF.
** End of change

        SELECT SINGLE WO_SER INTO L_WORDER
          FROM ZTPP_WOSUM
         WHERE EXTC = IT_DATA-EXTC
           AND INTC = IT_DATA-INTC
           AND VERSION = IT_DATA-VERS
           AND FSC = L_FSC.
        IF SY-SUBRC = 0.
          CONCATENATE L_WORDER L_DIST INTO L_WORDER.
          IT_DATA-WORK_ORDER = L_WORDER.
        ELSE.
          CLEAR: IT_DATA-WORK_ORDER.
        ENDIF.
        MODIFY IT_DATA.
      ENDIF.
* END OF CHANGE
      CLEAR: L_FWOCL, L_WORDER.
*      ENDIF.
    ENDLOOP.
    SORT IT_DATA BY RSNUM SERIAL .
  ENDIF.
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  READ_ALC_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MODEL  text
*----------------------------------------------------------------------*
FORM READ_ALC_MODEL   .
  DATA: LC_MODEL(30)         TYPE C         ,
        L_KNOBJ              LIKE CUCO-KNOBJ,
        L_KNNUM              LIKE CUOB-KNOBJ,
        L_KNNAM              LIKE CUKB-KNNAM.

  CLEAR: IT_MODEL, IT_MODEL[].
  PERFORM GET_MODELS .

  LOOP AT IT_MODEL   .
    WA_MODEL = IT_MODEL-MODL .
    CONCATENATE 'D_' WA_MODEL '_ALC_'  INTO LC_MODEL    .
    CONCATENATE WA_MODEL '_WOHD'           INTO  L_KNNAM.
    PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
    PERFORM GET_KNNUM                      USING L_KNOBJ.
    CONCATENATE WA_MODEL '_WOCL'           INTO  L_KNNAM.
    PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
    PERFORM GET_KNNUM                      USING L_KNOBJ.
    " Set the Model Code...
    LOOP AT IT_ALC  WHERE MODEL = SPACE.
      IT_ALC-MODEL = WA_MODEL .
      MODIFY IT_ALC.
    ENDLOOP.
  ENDLOOP.

  LOOP AT IT_ALC.
    CONCATENATE 'D_' IT_ALC-MODEL '_ALC_'  INTO LC_MODEL    .
    SELECT SINGLE B~KNNAM T~KNKTX
      INTO CORRESPONDING FIELDS OF IT_ALC
      FROM CUKB AS B INNER JOIN CUKBT AS T
        ON B~KNNUM = T~KNNUM
     WHERE B~KNNUM = IT_ALC-KNNUM
       AND T~SPRAS = SY-LANGU   .

    IF IT_ALC-KNNAM(10) NE LC_MODEL    .
      DELETE IT_ALC .
      CONTINUE .
    ENDIF.
    IT_ALC-CODE     = IT_ALC-KNNAM+12(3) .
    IT_ALC-TYPE_ALC = IT_ALC-KNNAM+10(1) .
    IT_ALC-RP       = IT_ALC-KNKTX(2)    .
    CONCATENATE IT_ALC-TYPE_ALC IT_ALC-CODE INTO IT_ALC-FULL_ALC .
    CONCATENATE 'P' IT_ALC-KNNAM+5(10)      INTO IT_ALC-CHAR_ALC .
    MODIFY IT_ALC .
  ENDLOOP.
  SORT IT_ALC BY KNNUM RP CODE .
ENDFORM.                    " READ_ALC_MODEL

*&---------------------------------------------------------------------*
*&      Form  CREATE_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_SUMMARY.
  DATA: LT_ALC               LIKE TABLE OF IT_ALC      WITH HEADER LINE,
        LT_WBS               LIKE TABLE OF IT_WBS      WITH HEADER LINE,
        LT_PRJ               LIKE TABLE OF IT_PRJ      WITH HEADER LINE,
        LT_SEQ               LIKE TABLE OF IT_SEQ      WITH HEADER LINE,
        LT_BI                LIKE TABLE OF IT_BI       WITH HEADER LINE,
        LT_PI                LIKE TABLE OF IT_PI       WITH HEADER LINE,
        LT_D1                LIKE TABLE OF IT_D1       WITH HEADER LINE,
        LT_DATA              LIKE TABLE OF IT_DATA     WITH HEADER LINE.

  PERFORM GET_CHAR_ATINN.
  PERFORM INLINE_STATUS .
  LT_DATA[] = IT_DATA[] .  LT_WBS[] = IT_WBS[].  LT_BI[] = IT_BI[].
  LT_D1[]   = IT_D1[]   .  LT_PRJ[] = IT_PRJ[].  LT_PI[] = IT_PI[].
  LT_SEQ[]  = IT_SEQ[]  .  LT_ALC[] = IT_ALC[].

  CLEAR: WA_INDEX, WA_DAYS, IT_SUM, IT_SUM[].
  PERFORM READ_INTERNAL_TABLE .
  PERFORM READ_ALC            .
  PERFORM CALC_ALC            .

  CLEAR: WA_INDEX, WA_DAYS, IT_SUM, IT_SUM[].
  PERFORM CALC_ALC_PROD       .
  PERFORM CALC_ALC_SEQ        .
  PERFORM CALC_ALC_BI         .
* PERFORM calc_alc_wbs        .
  PERFORM CALC_ALC_PI         .
* PERFORM calc_alc_prj        .
  PERFORM CALC_ALC_PBS        .

  PERFORM SUMMARY_DISP          .
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_RP  text
*----------------------------------------------------------------------*
FORM READ_INTERNAL_TABLE .
  DATA: L_NAME(40)           TYPE C ,
        L_FLAG               TYPE C ,
        L_MAX                TYPE I ,
        L_DATE               TYPE D ,
        L_DAYS               TYPE I ,
        L_POS                TYPE I ,
        L_CHK                TYPE P DECIMALS 3,
        L_TABIX              LIKE SY-TABIX,
        L_START              LIKE SY-TABIX,
        L_END                LIKE SY-TABIX,
        L_INDEX              LIKE SY-TABIX.
  DATA : IT_DATA_TEMP LIKE IT_DATA OCCURS 0 WITH HEADER LINE.

  REFRESH IT_DATA_TEMP.
  IT_DATA_TEMP[] = IT_DATA[].
  DELETE IT_DATA WHERE STATUS = 'B'.

  READ TABLE IT_DATA WITH KEY RP06 = SPACE .
  L_INDEX = SY-TABIX .
  IF L_INDEX > 1.
    L_INDEX = L_INDEX - 1 .
    DELETE IT_DATA FROM 1 TO L_INDEX.
  ENDIF.
  L_INDEX = 1.

*  " Skip the this weeks data...
*  LOOP AT it_master WHERE seq = 0   .
*    l_chk = it_master-time / 3600 .
*    l_pos = l_pos + CEIL( it_master-uph * l_chk )  .
*  ENDLOOP.
*  DESCRIBE TABLE it_data LINES l_index.
*  IF l_pos > 0 .
*    IF l_pos < l_index .
*      DELETE it_data FROM 1 TO l_pos.
*    ELSE.
*      DELETE TABLE it_data .
*    ENDIF.
*  ENDIF.
  L_INDEX = 1.

  " 1 Days * 147 Times
  SORT IT_MASTER BY SEQ DATE. CLEAR: L_TABIX, L_POS, L_CHK.
  DESCRIBE TABLE IT_DATA LINES L_MAX.

* REQUESTED BY MY HUR CHANGED BY CHRIS
*  l_date = p_dates - 1 .
*  PERFORM get_weeks USING '+' l_date wa_wdate .
  DATA: L_REMAIN_DAYS TYPE I.
  L_DATE = P_DATES.
  PERFORM GET_1STWEEK_DATE USING L_DATE WA_WDATE L_REMAIN_DAYS.
* END OF CHANGE ON 02/18/2005
  L_DATE = L_DATE - 1 .
  DO 147 TIMES.
    L_TABIX = L_TABIX + 1 .
    L_DATE  = L_DATE  + 1 .
    CLEAR: IT_MASTER.
    READ TABLE IT_MASTER WITH KEY DATE = L_DATE.
    IF IT_MASTER-UPH = 0.
      L_DAYS = L_DAYS + 1 .
    ELSE.
      L_CHK = IT_MASTER-TIME / 3600 .
      IF L_FLAG = 'X'.   EXIT.   ENDIF.
* REQUESTED BY MY. HUR CHANGED BY CHRIS
*      l_pos = l_pos + CEIL( it_master-uph * l_chk )  .
      L_POS = L_POS +  IT_MASTER-UPH * L_CHK   .
* END OF CHANGE ON 02/18/2005
      IF L_POS >= L_MAX.
        WA_INDEX = L_MAX - L_INDEX.
        WA_DAYS  = L_DAYS .
        L_POS = L_MAX.
        L_FLAG = 'X' .
      ENDIF.
      L_DAYS  = L_DAYS  + 1 .

* REQUESTED BY MY.HUR CHANGED BY CHRIS
      DATA: L_WEEK TYPE I.

      IF L_DAYS LE L_REMAIN_DAYS.
        L_WEEK   = 1.
      ELSE.
        L_WEEK   = CEIL( ( L_DAYS + 7 - L_REMAIN_DAYS )
                        * C_FACTOR / 7 ) .
      ENDIF.
      IF L_WEEK GT 21.
        EXIT.
      ELSEIF L_WEEK GT G_MRP_LAST_WEEK AND
             G_MRP_LAST_WEEK NE 0.
        L_WEEK = 99.    "remain qty
      ENDIF.
* END OF CHANGE ON 02/18/2005

      LOOP AT IT_DATA FROM L_INDEX TO L_POS.
        CLEAR: IT_SUM.

*       REQUESTED BY MY.HUR CHNGED BY CHRIS
*        it_sum-days      = ceil( l_days / 7 ) .
        IT_SUM-DAYS       = L_WEEK .
*       END OF CHANGE ON 02/18/2005

        IT_SUM-VM_MODEL   = IT_DATA-MODL       .
        IT_SUM-VM_BODYSER = IT_DATA-BODY_SER   .
        IT_SUM-WORDER     = IT_DATA-WORK_ORDER .
        IT_SUM-MITU       = IT_DATA-MITU       .
        IT_SUM-EXTC       = IT_DATA-EXTC       .
        IT_SUM-INTC       = IT_DATA-INTC       .
        IT_SUM-STATUS     = IT_DATA-STATUS     .
        IT_SUM-RP         = IT_ALC-RP          .
        IT_SUM-KNNAM      = IT_ALC-KNNAM       .
        IT_SUM-CNT        = 1                  .
        CONCATENATE IT_ALC-TYPE_ALC IT_ALC-CODE INTO IT_SUM-CODE .
        APPEND IT_SUM.
      ENDLOOP.
    ENDIF.
    L_INDEX = L_POS + 1 .
  ENDDO.
*


  DATA: C_WEEK LIKE SCAL-WEEK,
        B_WEEK LIKE C_WEEK,
        A_WEEK TYPE I,
        Z_NUM TYPE I.

  SORT IT_SUM BY DAYS DESCENDING.
  READ TABLE IT_SUM INDEX 1.
  IF G_MRP_LAST_WEEK NE 0.             "add by chris on 4/05/2005
    L_WEEK = G_MRP_LAST_WEEK + 1.      "add by chris on 4/05/2005
  ELSE.                                "add by chris on 4/05/2005
    L_WEEK = IT_SUM-DAYS + 1."week count
  ENDIF.                               "add by chris on 4/05/2005

  Z_NUM = 1.
  LOOP AT IT_DATA_TEMP WHERE STATUS = 'B'.
    IF Z_NUM <> 1.
      L_WEEK = A_WEEK.
    ENDIF.
    PERFORM GET_WEEK USING IT_DATA_TEMP-SEQ_DATE L_WEEK C_WEEK Z_NUM
                     CHANGING A_WEEK B_WEEK.
    IF A_WEEK GT 21.                  "add by chris on 4/05/2005
      EXIT.                           "add by chris on 4/05/2005
    ENDIF.                            "add by chris on 4/05/2005
    IT_SUM-DAYS       = A_WEEK                  .
    C_WEEK            = B_WEEK.
    IT_SUM-VM_MODEL   = IT_DATA_TEMP-MODL       .
    IT_SUM-VM_BODYSER = IT_DATA_TEMP-BODY_SER   .
    IT_SUM-WORDER     = IT_DATA_TEMP-WORK_ORDER .
    IT_SUM-MITU       = IT_DATA_TEMP-MITU       .
    IT_SUM-EXTC       = IT_DATA_TEMP-EXTC       .
    IT_SUM-INTC       = IT_DATA_TEMP-INTC       .
    IT_SUM-STATUS     = IT_DATA_TEMP-STATUS     .
    IT_SUM-RP         = IT_ALC-RP          .
    IT_SUM-KNNAM      = IT_ALC-KNNAM       .
    IT_SUM-CNT        = 1                  .
    CONCATENATE IT_ALC-TYPE_ALC IT_ALC-CODE INTO IT_SUM-CODE .
    APPEND IT_SUM.CLEAR : IT_SUM,Z_NUM,IT_DATA_TEMP.
  ENDLOOP.

ENDFORM.                    " READ_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*&      Form  calc_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC.
  DATA: L_NAME(40)              TYPE C  ,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_DAYS(2)               TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP.
  DESCRIBE TABLE IT_SUM LINES L_LINE.
  CHECK L_LINE > 0 .
  SORT IT_SUM BY DAYS VM_MODEL CODE VALS.
  READ TABLE IT_SUM INDEX 1.
  L_DAYS   = IT_SUM-DAYS    .
  L_CODE   = IT_SUM-CODE    .
  L_VALS   = IT_SUM-VALS    .
  L_MODEL  = IT_SUM-VM_MODEL.

  CONCATENATE 'IT_DISP-W'  L_DAYS   INTO L_NAME.
  ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
  CLEAR: <WA_DFIELD>.

  LOOP AT IT_SUM.
    IF L_DAYS  = IT_SUM-DAYS  AND L_CODE  = IT_SUM-CODE AND
       L_VALS  = IT_SUM-VALS  AND L_MODEL  = IT_SUM-VM_MODEL.
      <WA_DFIELD> = <WA_DFIELD> + IT_SUM-CNT .
      CONTINUE.
    ELSE.
      IT_DISP-MODEL    = L_MODEL  .
      IT_DISP-ALC_CODE = L_CODE   .
      IT_DISP-ALC_VALS = L_VALS   .
      IT_DISP-RP       = IT_ALC-RP.
      APPEND IT_DISP.    CLEAR: IT_DISP.
      L_DAYS     = IT_SUM-DAYS     .
      L_CODE     = IT_SUM-CODE     .
      L_VALS     = IT_SUM-VALS     .
      L_MODEL    = IT_SUM-VM_MODEL .
      CONCATENATE 'IT_DISP-W'  L_DAYS   INTO L_NAME.
      ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
      <WA_DFIELD>  = IT_SUM-CNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM  LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP-MODEL    = L_MODEL  .
    IT_DISP-ALC_CODE = L_CODE   .
    IT_DISP-ALC_VALS = L_VALS   .
    IT_DISP-RP       = IT_ALC-RP.
    APPEND IT_DISP.
  ENDIF.
ENDFORM.                    " calc_alc

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC_PROD.
  DATA: L_COUNT                 TYPE I  ,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_D1  .
  SORT IT_D1   BY VM_MODEL DAYS  CODE VALS.
  READ TABLE IT_D1 INDEX 1.  CLEAR: L_COUNT.
* l_days   = IT_D1-days    .
  L_CODE   = IT_D1-CODE    .
  L_VALS   = IT_D1-VALS    .
  L_MODEL  = IT_D1-VM_MODEL.

  LOOP AT IT_D1  .
    IF L_VALS  = IT_D1-VALS  AND L_CODE  = IT_D1-CODE AND
       L_MODEL  = IT_D1-VM_MODEL.
*      l_hours = IT_D1-hours.
      L_COUNT  = L_COUNT + IT_D1-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_D1-MODEL    = L_MODEL  .
      IT_DISP_D1-ALC_CODE = L_CODE   .
      IT_DISP_D1-ALC_VALS = L_VALS   .
      IT_DISP_D1-RP       = IT_ALC-RP.
      IT_DISP_D1-W_1      = L_COUNT  .
      APPEND IT_DISP_D1.    CLEAR: IT_DISP_D1.
*     l_hours = IT_D1-hours      .
      L_CODE  = IT_D1-CODE       .
      L_VALS  = IT_D1-VALS       .
      L_MODEL = IT_D1-VM_MODEL   .
      L_COUNT = IT_D1-CNT        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_D1    LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_D1-MODEL    = L_MODEL  .
    IT_DISP_D1-ALC_CODE = L_CODE   .
    IT_DISP_D1-ALC_VALS = L_VALS   .
    IT_DISP_D1-RP       = IT_ALC-RP.
    IT_DISP_D1-W_1      = L_COUNT  .
    APPEND IT_DISP_D1.
  ENDIF.
ENDFORM.                    " calc_alc_PROD

*&---------------------------------------------------------------------*
*&      Form  calc_alc_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC_SEQ .
  DATA: L_COUNT                 TYPE I  ,
        L_LINE                  TYPE I  ,
        L_MITUCNT               TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_SEQ .
  SORT IT_SEQ  BY VM_MODEL CODE VALS.
  READ TABLE IT_SEQ  INDEX 1.  CLEAR: L_COUNT.
* l_hours  = it_SEQ-hours   .
  L_CODE   = IT_SEQ-CODE    .
  L_VALS   = IT_SEQ-VALS    .
  L_MODEL  = IT_SEQ-VM_MODEL.

  LOOP AT IT_SEQ .
    IF L_VALS  = IT_SEQ-VALS  AND L_CODE  = IT_SEQ-CODE AND
       L_MODEL  = IT_SEQ-VM_MODEL.
      L_COUNT  = L_COUNT + IT_SEQ-CNT .
      L_MITUCNT = L_MITUCNT + IT_SEQ-MITUCNT .
      CONTINUE.
    ELSE.
      IT_DISP_SEQ-MODEL    = L_MODEL  .
      IT_DISP_SEQ-ALC_CODE = L_CODE   .
      IT_DISP_SEQ-ALC_VALS = L_VALS   .
      IT_DISP_SEQ-RP       = IT_ALC-RP.
      IT_DISP_SEQ-W_1      = L_COUNT  .
      IT_DISP_SEQ-MITU     = L_MITUCNT.
      APPEND IT_DISP_SEQ .    CLEAR: IT_DISP_SEQ .
*     l_hours = it_SEQ-hours      .
      L_CODE    = IT_SEQ-CODE       .
      L_VALS    = IT_SEQ-VALS       .
      L_MODEL   = IT_SEQ-VM_MODEL   .
      L_COUNT   = IT_SEQ-CNT        .
      L_MITUCNT = IT_SEQ-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SEQ   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_SEQ-MODEL    = L_MODEL  .
    IT_DISP_SEQ-ALC_CODE = L_CODE   .
    IT_DISP_SEQ-ALC_VALS = L_VALS   .
    IT_DISP_SEQ-RP       = IT_ALC-RP.
    IT_DISP_SEQ-W_1      = L_COUNT  .
    IT_DISP_SEQ-MITU     = L_MITUCNT.
    APPEND IT_DISP_SEQ .
  ENDIF.
ENDFORM.                    " calc_alc_SEQ

*&---------------------------------------------------------------------*
*&      Form  calc_alc_BI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC_BI  .
  DATA: L_COUNT                 TYPE I  ,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_BI .
  SORT IT_BI   BY VM_MODEL CODE VALS.
  READ TABLE IT_BI   INDEX 1.  CLEAR: L_COUNT.
  L_CODE   = IT_BI-CODE    .
  L_VALS   = IT_BI-VALS    .
  L_MODEL  = IT_BI-VM_MODEL.

  LOOP AT IT_BI  .
    IF L_VALS  = IT_BI-VALS  AND L_CODE  = IT_BI-CODE  AND
       L_MODEL  = IT_BI-VM_MODEL.
      L_COUNT  = L_COUNT + IT_BI-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_BI-MODEL    = L_MODEL  .
      IT_DISP_BI-ALC_CODE = L_CODE   .
      IT_DISP_BI-ALC_VALS = L_VALS   .
      IT_DISP_BI-RP       = IT_ALC-RP.
      IT_DISP_BI-W_1      = L_COUNT  .
      APPEND IT_DISP_BI .    CLEAR: IT_DISP_BI .
      L_CODE  = IT_BI-CODE       .
      L_VALS  = IT_BI-VALS       .
      L_MODEL = IT_BI-VM_MODEL   .
      L_COUNT = IT_BI-CNT        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_BI    LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_BI-MODEL    = L_MODEL  .
    IT_DISP_BI-ALC_CODE = L_CODE   .
    IT_DISP_BI-ALC_VALS = L_VALS   .
    IT_DISP_BI-RP       = IT_ALC-RP.
    IT_DISP_BI-W_1      = L_COUNT  .
    APPEND IT_DISP_BI  .
  ENDIF.
ENDFORM.                    " calc_alc_BI

*&---------------------------------------------------------------------*
*&      Form  calc_alc_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC_WBS .
  DATA: L_COUNT                 TYPE I  ,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_WBS .
  SORT IT_WBS  BY VM_MODEL CODE VALS.
  READ TABLE IT_WBS  INDEX 1.  CLEAR: L_COUNT.
* l_hours  = it_WBS-hours   .
  L_CODE   = IT_WBS-CODE    .
  L_VALS   = IT_WBS-VALS    .
  L_MODEL  = IT_WBS-VM_MODEL.

  LOOP AT IT_WBS .
    IF L_VALS  = IT_WBS-VALS  AND L_CODE  = IT_WBS-CODE AND
       L_MODEL  = IT_WBS-VM_MODEL.
      L_COUNT  = L_COUNT + IT_WBS-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_WBS-MODEL    = L_MODEL  .
      IT_DISP_WBS-ALC_CODE = L_CODE   .
      IT_DISP_WBS-ALC_VALS = L_VALS   .
      IT_DISP_WBS-RP       = IT_ALC-RP.
      IT_DISP_WBS-W_1      = L_COUNT  .
      APPEND IT_DISP_WBS .    CLEAR: IT_DISP_WBS .
      L_CODE  = IT_WBS-CODE       .
      L_VALS  = IT_WBS-VALS       .
      L_MODEL = IT_WBS-VM_MODEL   .
      L_COUNT = IT_WBS-CNT        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_WBS   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_WBS-MODEL    = L_MODEL  .
    IT_DISP_WBS-ALC_CODE = L_CODE   .
    IT_DISP_WBS-ALC_VALS = L_VALS   .
    IT_DISP_WBS-RP       = IT_ALC-RP.
    IT_DISP_WBS-W_1      = L_COUNT  .
    APPEND IT_DISP_WBS .
  ENDIF.
ENDFORM.                    " calc_alc_WBS

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC_PI  .
  DATA: L_COUNT                 TYPE I  ,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_PI .
  SORT IT_PI   BY VM_MODEL CODE VALS.
  READ TABLE IT_PI   INDEX 1.  CLEAR: L_COUNT.
  L_CODE   = IT_PI-CODE    .
  L_VALS   = IT_PI-VALS    .
  L_MODEL  = IT_PI-VM_MODEL.

  LOOP AT IT_PI  .
    IF L_VALS  = IT_PI-VALS  AND L_CODE  = IT_PI-CODE AND
       L_MODEL  = IT_PI-VM_MODEL.
      L_COUNT  = L_COUNT + IT_PI-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_PI-MODEL    = L_MODEL  .
      IT_DISP_PI-ALC_CODE = L_CODE   .
      IT_DISP_PI-ALC_VALS = L_VALS   .
      IT_DISP_PI-RP       = IT_ALC-RP.
      IT_DISP_PI-W_1      = L_COUNT  .
      APPEND IT_DISP_PI .    CLEAR: IT_DISP_PI .
*     l_hours = it_PI-Hours      .
      L_CODE  = IT_PI-CODE       .
      L_VALS  = IT_PI-VALS       .
      L_MODEL = IT_PI-VM_MODEL   .
      L_COUNT = IT_PI-CNT        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PI    LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_PI-MODEL    = L_MODEL  .
    IT_DISP_PI-ALC_CODE = L_CODE   .
    IT_DISP_PI-ALC_VALS = L_VALS   .
    IT_DISP_PI-RP       = IT_ALC-RP.
    IT_DISP_PI-W_1      = L_COUNT  .
    APPEND IT_DISP_PI  .
  ENDIF.
ENDFORM.                    " calc_alc_PI

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PRJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC_PRJ .
  DATA: L_COUNT                 TYPE I  ,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_PRJ .
  SORT IT_PRJ BY VM_MODEL CODE VALS.
  READ TABLE IT_PRJ INDEX 1.  CLEAR: L_COUNT.
  L_CODE   = IT_PRJ-CODE    .
  L_VALS   = IT_PRJ-VALS    .
  L_MODEL  = IT_PRJ-VM_MODEL.

  LOOP AT IT_PRJ .
    IF L_VALS  = IT_PRJ-VALS  AND L_CODE  = IT_PRJ-CODE AND
       L_MODEL  = IT_PRJ-VM_MODEL.
*      l_hours = it_PRJ-hours.
      L_COUNT  = L_COUNT + IT_PRJ-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_PRJ-MODEL    = L_MODEL  .
      IT_DISP_PRJ-ALC_CODE = L_CODE   .
      IT_DISP_PRJ-ALC_VALS = L_VALS   .
      IT_DISP_PRJ-RP       = IT_ALC-RP.
      IT_DISP_PRJ-W_1      = L_COUNT  .
      APPEND IT_DISP_PRJ.    CLEAR: IT_DISP_PRJ .
*     l_hours = it_PRJ-hours      .
      L_CODE  = IT_PRJ-CODE       .
      L_VALS  = IT_PRJ-VALS       .
      L_MODEL = IT_PRJ-VM_MODEL   .
      L_COUNT = IT_PRJ-CNT        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PRJ   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_PRJ-MODEL    = L_MODEL  .
    IT_DISP_PRJ-ALC_CODE = L_CODE   .
    IT_DISP_PRJ-ALC_VALS = L_VALS   .
    IT_DISP_PRJ-RP       = IT_ALC-RP.
    IT_DISP_PRJ-W_1      = L_COUNT  .
    APPEND IT_DISP_PRJ.
  ENDIF.
ENDFORM.                    " calc_alc_PRJ

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_ALC_PBS .
  DATA: L_COUNT                 TYPE I  ,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_PBS .
  SORT IT_PBS  BY VM_MODEL CODE VALS.
  READ TABLE IT_PBS  INDEX 1.  CLEAR: L_COUNT.
* l_hours  = it_PBS-hours   .
  L_CODE   = IT_PBS-CODE    .
  L_VALS   = IT_PBS-VALS    .
  L_MODEL  = IT_PBS-VM_MODEL.

  LOOP AT IT_PBS .
    IF L_VALS  = IT_PBS-VALS  AND L_CODE  = IT_PBS-CODE AND
       L_MODEL  = IT_PBS-VM_MODEL.
      L_COUNT  = L_COUNT + IT_PBS-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_PBS-MODEL    = L_MODEL  .
      IT_DISP_PBS-ALC_CODE = L_CODE   .
      IT_DISP_PBS-ALC_VALS = L_VALS   .
      IT_DISP_PBS-RP       = IT_ALC-RP.
      IT_DISP_PBS-W_1      = L_COUNT  .
      APPEND IT_DISP_PBS .    CLEAR: IT_DISP_PBS .
      L_CODE  = IT_PBS-CODE       .
      L_VALS  = IT_PBS-VALS       .
      L_MODEL = IT_PBS-VM_MODEL   .
      L_COUNT = IT_PBS-CNT        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PBS   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_PBS-MODEL    = L_MODEL  .
    IT_DISP_PBS-ALC_CODE = L_CODE   .
    IT_DISP_PBS-ALC_VALS = L_VALS   .
    IT_DISP_PBS-RP       = IT_ALC-RP.
    IT_DISP_PBS-W_1      = L_COUNT  .
    APPEND IT_DISP_PBS .
  ENDIF.
ENDFORM.                    " calc_alc_PBS

*&---------------------------------------------------------------------*
*&      Form  GET_ALC_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_WORDER  text
*      -->P_IT_SUM_CODE  text
*      <--P_IT_SUM_VALS  text
*----------------------------------------------------------------------*
FORM GET_ALC_VALUE USING    PA_WORDER  PA_CODE
                   CHANGING PA_VALS.
  DATA: L_VALS LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
        L_MATNR LIKE MARA-MATNR.
*        l_atinn like ausp-atinn.

  CLEAR: PA_VALS.
  CLEAR: L_VALS, L_VALS[].
  CASE PA_CODE(1).
    WHEN 'U'.
      L_MATNR = PA_WORDER(14).
      CONCATENATE 'P_ALC_U_' PA_CODE+1(3) INTO L_VALS-ATNAM.
    WHEN 'C'.
      L_MATNR = PA_WORDER    .
      CONCATENATE 'P_ALC_C_' PA_CODE+1(3) INTO L_VALS-ATNAM.
  ENDCASE.
  APPEND L_VALS.

  READ TABLE IT_CHAR WITH KEY ATNAM = L_VALS-ATNAM.

  SELECT SINGLE ATWRT INTO L_VALS-ATWRT
      FROM AUSP
      WHERE OBJEK = L_MATNR
        AND KLART = '001'
        AND ATINN = IT_CHAR-ATINN.

*  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*       EXPORTING
*            object       = l_matnr
*            ctype        = '001'
*       TABLES
*            val_table    = l_vals
*       EXCEPTIONS
*            no_data      = 1
*            error_mode   = 2
*            error_object = 3
*            error_value  = 4
*            OTHERS       = 5.

* REQUESTED BY MY.HUR CHANGED BY CHRIS
*  NOTE: FOR FORCAST WORK ORDER, THERE IS NOT ALC CODE EXIST IN VM
*        CHECK THE CUSTOM TABLE ZTPP_FWO FOR THE ALC FOR FORCAST WO

  IF SY-SUBRC = 0.                                "  UD1K914617
*    READ TABLE l_vals INDEX 1  .
*
    PA_VALS = L_VALS-ATWRT     .

  ELSE.                                           "  UD1K914617
*    SELECT SINGLE alc_vals INTO pa_vals           "  UD1K914617
*      FROM ztpp_fwo                               "  UD1K914617
*      WHERE worder    = l_matnr  AND              "  UD1K914617
*            alc_code = l_vals-atnam.              "  UD1K914617
    READ TABLE LT_FWO WITH KEY WORDER   = L_MATNR
                               ALC_CODE = L_VALS-ATNAM
                               BINARY SEARCH.
    IF SY-SUBRC = 0.
      PA_VALS = LT_FWO-ALC_VALS.
    ENDIF.
  ENDIF.                                          "  UD1K914617


* END OF CHANGE ON 02/24/2005


ENDFORM.                    " GET_ALC_VALUE

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  DATA: L_CNT                LIKE IT_DISP-SERIAL.
  DELETE FROM ZTPP_SEQ_SUM03 CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  SORT IT_DISP BY SERIAL ALC_CODE MODEL DESCENDING ALC_VALS .
*  LOOP AT it_disp.
*    LOOP AT it_seq WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-seq = it_disp-seq + it_seq-cnt     .
*    ENDLOOP.
*    LOOP AT it_bi  WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-bodyin = it_disp-bodyin + it_bi-cnt     .
*    ENDLOOP.
*    LOOP AT it_pi WHERE  model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-paint = it_disp-paint + it_pi-cnt     .
*    ENDLOOP.
*    LOOP AT it_pbs WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-pbs = it_disp-pbs + it_pbs-cnt     .
*    ENDLOOP.
*    LOOP AT it_prj WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-prj = it_disp-prj + it_prj-cnt     .
*    ENDLOOP.
*    LOOP AT it_wbs WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-wbs = it_disp-wbs + it_wbs-cnt     .
*    ENDLOOP.
*    MODIFY it_disp.
*  ENDLOOP.
  MODIFY ZTPP_SEQ_SUM03     FROM TABLE IT_DISP .
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000 WITH 'Sum03 Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'Sum03 Update failed'.
  ENDIF.

ENDFORM.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  INLINE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INLINE_STATUS.
  " Summary of the ALC for the Current MIP Vehicle..
  PERFORM CREATE_DATA   TABLES  IT_SEQ   USING '00'.
  PERFORM CALC_SEQ                                 .
  PERFORM CREATE_DATA   TABLES  IT_BI    USING '01'.
  PERFORM CALC_BODYINPUT                           .
* PERFORM create_data   TABLES  it_wbs   USING '99'.
* PERFORM calc_wbs                                 .
  PERFORM CREATE_DATA   TABLES  IT_PI    USING '02'.
  PERFORM CALC_PAINTINPUT                          .
* PERFORM create_data   TABLES  it_prj   USING '88'.
* PERFORM calc_paintreject                         .
  PERFORM CREATE_DATA   TABLES  IT_PBS   USING '06'.
  PERFORM CALC_PBS                                 .
  PERFORM CALC_PROD                                .
ENDFORM.                    " INLINE_STATUS

*&---------------------------------------------------------------------*
*&      Form  add_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
FORM ADD_ROUTINE USING    PA_DISP  LIKE  IT_DISP.
  IT_DISP-W_1    = IT_DISP-W_1     + PA_DISP-W_1 .
  IT_DISP-SEQ    = IT_DISP-SEQ     + PA_DISP-SEQ .
  IT_DISP-BODYIN = IT_DISP-BODYIN  + PA_DISP-BODYIN .
  IT_DISP-WBS    = IT_DISP-WBS     + PA_DISP-WBS .
  IT_DISP-PAINT  = IT_DISP-PAINT   + PA_DISP-PAINT .
  IT_DISP-PRJ    = IT_DISP-PRJ     + PA_DISP-PRJ .
  IT_DISP-PBS    = IT_DISP-PBS     + PA_DISP-PBS .
  IT_DISP-W01    = IT_DISP-W01     + PA_DISP-W01 .
  IT_DISP-W02    = IT_DISP-W02     + PA_DISP-W02 .
  IT_DISP-W03    = IT_DISP-W03     + PA_DISP-W03 .
  IT_DISP-W04    = IT_DISP-W04     + PA_DISP-W04 .
  IT_DISP-W05    = IT_DISP-W05     + PA_DISP-W05 .
  IT_DISP-W06    = IT_DISP-W06     + PA_DISP-W06 .
  IT_DISP-W07    = IT_DISP-W07     + PA_DISP-W07 .
  IT_DISP-W08    = IT_DISP-W08     + PA_DISP-W08 .
  IT_DISP-W09    = IT_DISP-W09     + PA_DISP-W09 .
  IT_DISP-W10    = IT_DISP-W10     + PA_DISP-W10 .
  IT_DISP-W11    = IT_DISP-W11     + PA_DISP-W11 .
  IT_DISP-W12    = IT_DISP-W12     + PA_DISP-W12 .
  IT_DISP-W13    = IT_DISP-W13     + PA_DISP-W13 .
  IT_DISP-W14    = IT_DISP-W14     + PA_DISP-W14 .
  IT_DISP-W15    = IT_DISP-W15     + PA_DISP-W15 .
  IT_DISP-W16    = IT_DISP-W16     + PA_DISP-W16 .
  IT_DISP-W17    = IT_DISP-W17     + PA_DISP-W17 .
  IT_DISP-W18    = IT_DISP-W18     + PA_DISP-W18 .
  IT_DISP-W19    = IT_DISP-W19     + PA_DISP-W19 .
  IT_DISP-W20    = IT_DISP-W20     + PA_DISP-W20 .
  IT_DISP-W21    = IT_DISP-W21     + PA_DISP-W21 .
  IT_DISP-W99    = IT_DISP-W99     + PA_DISP-W99 .
  IT_DISP-MITU   = IT_DISP-MITU    + PA_DISP-MITU.
ENDFORM.                    " add_routine

*&---------------------------------------------------------------------*
*&      Form  add_routine_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
FORM ADD_ROUTINE_PROD USING    PA_DISP  LIKE  IT_DISP     .
  IT_DISP-W_1    = IT_DISP-W_1  + PA_DISP-W_1 .
  IT_DISP-SEQ    = IT_DISP-SEQ  + PA_DISP-SEQ .
  IT_DISP-BODYIN = IT_DISP-BODYIN  + PA_DISP-BODYIN .
  IT_DISP-WBS    = IT_DISP-WBS  + PA_DISP-WBS .
  IT_DISP-PAINT  = IT_DISP-PAINT  + PA_DISP-PAINT .
  IT_DISP-PRJ    = IT_DISP-PRJ  + PA_DISP-PRJ .
  IT_DISP-PBS    = IT_DISP-PBS  + PA_DISP-PBS .
  IT_DISP-W01  = IT_DISP-W01  + PA_DISP-W01 .
  IT_DISP-W02  = IT_DISP-W02  + PA_DISP-W02 .
  IT_DISP-W03  = IT_DISP-W03  + PA_DISP-W03 .
  IT_DISP-W04  = IT_DISP-W04  + PA_DISP-W04 .
  IT_DISP-W05  = IT_DISP-W05  + PA_DISP-W05 .
  IT_DISP-W06  = IT_DISP-W06  + PA_DISP-W06 .
  IT_DISP-W07  = IT_DISP-W07  + PA_DISP-W07 .
  IT_DISP-W08  = IT_DISP-W08  + PA_DISP-W08 .
  IT_DISP-W09  = IT_DISP-W09  + PA_DISP-W09 .
  IT_DISP-W10  = IT_DISP-W10  + PA_DISP-W10 .
  IT_DISP-W11  = IT_DISP-W11  + PA_DISP-W11 .
  IT_DISP-W12  = IT_DISP-W12  + PA_DISP-W12 .
  IT_DISP-W13  = IT_DISP-W13  + PA_DISP-W13 .
  IT_DISP-W14  = IT_DISP-W14  + PA_DISP-W14 .
  IT_DISP-W15  = IT_DISP-W15  + PA_DISP-W15 .
  IT_DISP-W16  = IT_DISP-W16  + PA_DISP-W16 .
  IT_DISP-W17  = IT_DISP-W17  + PA_DISP-W17 .
  IT_DISP-W18  = IT_DISP-W18  + PA_DISP-W18 .
  IT_DISP-W19  = IT_DISP-W19  + PA_DISP-W19 .
  IT_DISP-W20  = IT_DISP-W20  + PA_DISP-W20 .
  IT_DISP-W21  = IT_DISP-W21  + PA_DISP-W21 .
  IT_DISP-W99  = IT_DISP-W99  + PA_DISP-W99 .
  IT_DISP-MITU = IT_DISP-MITU + PA_DISP-MITU.
ENDFORM.                    " add_routine_PROD

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMMARY_DISP.
  DATA: LT_DISP                 LIKE TABLE OF IT_DISP  WITH HEADER LINE,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_STATUS(3)             TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  LT_DISP[] = IT_DISP[].   IT_DISP_D21[] = IT_DISP[].

  " Accumulate the Data..
  CLEAR: IT_DISP, IT_DISP[].
  SORT LT_DISP BY ALC_CODE MODEL ALC_VALS.
  READ TABLE LT_DISP INDEX 1.
  L_MODEL = LT_DISP-MODEL   .
  L_CODE  = LT_DISP-ALC_CODE.
  L_VALS  = LT_DISP-ALC_VALS.
  IT_DISP     = LT_DISP         .
  PERFORM CLEAR_QTY_DISP  USING IT_DISP     .

  LOOP AT LT_DISP.
    IF L_MODEL = LT_DISP-MODEL  AND  L_CODE  = LT_DISP-ALC_CODE  AND
       L_VALS  = LT_DISP-ALC_VALS.
      PERFORM ADD_ROUTINE  USING  LT_DISP .
      CONTINUE.
    ELSE.
      CONDENSE LT_DISP-ALC_CODE .
      IT_DISP-SERIAL = STRLEN( L_VALS )    .
      APPEND IT_DISP.     CLEAR: IT_DISP   .
      IT_DISP     = LT_DISP         .
*      PERFORM clear_qty_disp  USING it_disp     .
      L_MODEL = LT_DISP-MODEL   .
      L_CODE  = LT_DISP-ALC_CODE.
      L_VALS  = LT_DISP-ALC_VALS.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_DISP LINES  L_LINE.
  IF L_LINE > 0.
    CONDENSE LT_DISP-ALC_CODE .
    IT_DISP-SERIAL = STRLEN( L_VALS ) .
    APPEND IT_DISP.
  ENDIF.
ENDFORM.                    " SUMMARY_DISP

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DISP_final
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMMARY_DISP_FINAL.
  DATA: LT_DISP                 LIKE TABLE OF IT_DISP  WITH HEADER LINE,
        L_LINE                  TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_STATUS(3)             TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS(5)               TYPE C  .              " ALC CODE VALUE

  " Final Summarize...
  LT_DISP[] = IT_DISP[].    CLEAR: IT_DISP, IT_DISP[].
  SORT LT_DISP BY MODEL ALC_CODE ALC_VALS .
  READ TABLE LT_DISP INDEX 1.
  L_MODEL = LT_DISP-MODEL   .
  L_CODE  = LT_DISP-ALC_CODE.
  L_VALS  = LT_DISP-ALC_VALS.
  IT_DISP     = LT_DISP         .
  PERFORM CLEAR_QTY_DISP  USING IT_DISP     .

  LOOP AT LT_DISP.
    IF L_MODEL = LT_DISP-MODEL  AND  L_CODE  = LT_DISP-ALC_CODE  AND
       L_VALS  = LT_DISP-ALC_VALS.
      PERFORM ADD_ROUTINE_PROD  USING  LT_DISP .
      CONTINUE.
    ELSE.
      CONDENSE LT_DISP-ALC_CODE .
      IT_DISP-SERIAL = STRLEN( L_VALS )    .
      APPEND IT_DISP.     CLEAR: IT_DISP   .
      IT_DISP     = LT_DISP         .
*     PERFORM clear_qty_disp  USING it_disp     .
      L_MODEL = LT_DISP-MODEL   .
      L_CODE  = LT_DISP-ALC_CODE.
      L_VALS  = LT_DISP-ALC_VALS.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_DISP LINES  L_LINE.
  IF L_LINE > 0.
    CONDENSE LT_DISP-ALC_CODE .
    IT_DISP-SERIAL = STRLEN( L_VALS ) .
    APPEND IT_DISP.
  ENDIF.
ENDFORM.                    " SUMMARY_DISP_FINAL

*&---------------------------------------------------------------------*
*&      Form  GET_KNOBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNOBJ USING    PA_KNNAM  PA_KNOBJ.
  CLEAR: PA_KNOBJ.
  SELECT SINGLE KNOBJ INTO PA_KNOBJ
    FROM CUCO
   WHERE OBTAB = 'MARA'
     AND OBJEK = PA_KNNAM .
ENDFORM.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNNUM USING    PA_KNOBJ.
  SELECT KNNUM APPENDING CORRESPONDING FIELDS OF TABLE IT_ALC
    FROM CUOB
   WHERE KNOBJ = PA_KNOBJ.
ENDFORM.                    " get_KNNUM

*&---------------------------------------------------------------------*
*&      Form  read_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ALC.
  DATA: LT_SUM              LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  SORT IT_SUM BY DAYS  WORDER EXTC INTC .
  READ TABLE IT_SUM INDEX 1.
  L_DAYS   = IT_SUM-DAYS    .
  L_WORDER = IT_SUM-WORDER  .
  L_EXT    = IT_SUM-EXTC    .
  L_INT    = IT_SUM-INTC    .
  L_MODEL  = IT_SUM-VM_MODEL  .

  SORT LT_FWO BY WORDER ALC_CODE.

  " Work Order Summarize in the same time terms.
  LOOP AT IT_SUM.
    IF L_DAYS  = IT_SUM-DAYS  AND L_WORDER = IT_SUM-WORDER AND
       L_EXT   = IT_SUM-EXTC  AND L_INT    = IT_SUM-INTC   AND
       L_MODEL = IT_SUM-VM_MODEL .
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        CONCATENATE  L_WORDER  L_EXT    L_INT   INTO L_WORDER       .
        PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                            CHANGING LT_SUM-VALS                .
        LT_SUM-VM_MODEL   = L_MODEL      .
        LT_SUM-DAYS       = L_DAYS            .
        LT_SUM-WORDER     = L_WORDER          .
        LT_SUM-EXTC       = L_EXT             .
        LT_SUM-INTC       = L_INT             .
        LT_SUM-CNT        = L_CNT             .
        LT_SUM-CODE       = IT_ALC-FULL_ALC   .
        APPEND LT_SUM      .
      ENDLOOP.
      LT_SUM   = IT_SUM         .
      L_DAYS   = IT_SUM-DAYS    .
      L_WORDER = IT_SUM-WORDER  .
      L_EXT    = IT_SUM-EXTC    .
      L_INT    = IT_SUM-INTC    .
      L_MODEL  = IT_SUM-VM_MODEL.
      L_CNT    = 1              .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM LINES L_SIZE .
  IF L_SIZE > 0 .
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      CONCATENATE  L_WORDER  L_EXT    L_INT   INTO L_WORDER       .
      PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                         CHANGING LT_SUM-VALS                .
      LT_SUM-VM_MODEL   = L_MODEL      .
      LT_SUM-WORDER     = L_WORDER          .
      LT_SUM-EXTC       = L_EXT             .
      LT_SUM-INTC       = L_INT             .
      LT_SUM-CNT        = L_CNT             .
      LT_SUM-CODE       = IT_ALC-FULL_ALC   .
      APPEND       LT_SUM      .
    ENDLOOP.
  ENDIF.

  IT_SUM[] = LT_SUM[].
ENDFORM.                    " read_alc

*&---------------------------------------------------------------------*
*&      Form  create_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SEQ  text
*      -->P_1320   text
*----------------------------------------------------------------------*
FORM CREATE_DATA  TABLES PA_TAB STRUCTURE IT_SUM  USING  PA_RP .
  DATA: L_DATA    LIKE TABLE OF ZTPP_INPUT_PLAN      WITH HEADER LINE.

  CASE PA_RP.
    WHEN '00' OR '01'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE L_DATA
        FROM ZTPP_INPUT_PLAN
       WHERE STATUS = PA_RP .
    WHEN '02' OR '88'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE L_DATA
        FROM ZTPP_INPUT_PLAN
       WHERE STATUS = '02'  .

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE L_DATA
        FROM ZTPP_INPUT_PLAN
       WHERE STATUS = '03'  .
    WHEN '06'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE L_DATA
        FROM ZTPP_INPUT_PLAN
       WHERE STATUS = '04'  .

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE L_DATA
        FROM ZTPP_INPUT_PLAN
       WHERE STATUS = '05'  .
    WHEN '99'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE L_DATA
        FROM ZTPP_INPUT_PLAN
       WHERE STATUS = '01'  .
  ENDCASE.

  IF P_TEST = 'X'.
    LOOP AT L_DATA.
      IF L_DATA-WORK_ORDER+12(2) = 'XX' OR
         L_DATA-WORK_ORDER+12(2) = 'XY' .
        DELETE L_DATA.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT L_DATA BY SERIAL DESCENDING.
  LOOP AT L_DATA.
    CLEAR: PA_TAB.
    PA_TAB-VM_MODEL   = L_DATA-MODL       .
    PA_TAB-VM_BODYSER = L_DATA-BODY_SER   .
    PA_TAB-WORDER     = L_DATA-WORK_ORDER .
    PA_TAB-MITU       = L_DATA-MITU       .
    PA_TAB-EXTC       = L_DATA-EXTC       .
    PA_TAB-INTC       = L_DATA-INTC       .
    PA_TAB-STATUS     = L_DATA-STATUS     .
    PA_TAB-RP         = '06'              .
    PA_TAB-CNT        = 1                 .
    PA_TAB-SERIAL     = L_DATA-SERIAL     .
    APPEND PA_TAB .
  ENDLOOP.
ENDFORM.                    " create_DATA

*&---------------------------------------------------------------------*
*&      Form  CALC_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_SEQ.
  DATA: L_MITUCNT            TYPE I ,
        L_CNT                TYPE I ,
        L_SIZE               TYPE I ,
        L_MODEL              LIKE IT_SEQ-VM_MODEL,
        L_WORDER             LIKE MARA-MATNR  ,
        L_EXTC               LIKE ZTPP_INPUT_PLAN-EXTC,
        L_INTC               LIKE ZTPP_INPUT_PLAN-INTC,
        L_CODE               LIKE IT_SEQ-CODE ,
        L_DAYS(2)            TYPE N           ,
        L_VALS               LIKE IT_SEQ-VALS ,
        LT_SEQ               LIKE TABLE OF IT_SUM      WITH HEADER LINE.

  DESCRIBE TABLE IT_SEQ LINES L_CNT  .
  CHECK L_CNT > 0 .

  SORT IT_SEQ BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_SEQ  INDEX 1.    CLEAR: L_CNT, L_MITUCNT.
  L_WORDER = IT_SEQ-WORDER  .
  L_EXTC   = IT_SEQ-EXTC    .
  L_INTC   = IT_SEQ-INTC    .
  L_MODEL  = IT_SEQ-VM_MODEL.
  LT_SEQ   = IT_SEQ         .

  LOOP AT IT_SEQ            .
    IF L_DAYS  = IT_SEQ-DAYS  AND L_WORDER = IT_SEQ-WORDER AND
       L_EXTC  = IT_SEQ-EXTC  AND L_INTC   = IT_SEQ-INTC   AND
       L_MODEL = IT_SEQ-VM_MODEL .
      L_CNT    = L_CNT  + 1          .
      IF IT_SEQ-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      CONTINUE .
    ELSE.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
        PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                            CHANGING LT_SEQ-VALS                    .
        LT_SEQ-VM_MODEL   = L_MODEL      .
        LT_SEQ-WORDER     = L_WORDER          .
        LT_SEQ-EXTC       = L_EXTC            .
        LT_SEQ-INTC       = L_INTC            .
        LT_SEQ-CNT        = L_CNT             .
        LT_SEQ-MITUCNT    = L_MITUCNT         .
        LT_SEQ-CODE       = IT_ALC-FULL_ALC   .
        APPEND LT_SEQ               .
      ENDLOOP.
      LT_SEQ   = IT_SEQ             .
      L_WORDER = IT_SEQ-WORDER      .
      L_EXTC   = IT_SEQ-EXTC        .
      L_INTC   = IT_SEQ-INTC        .
      L_MODEL  = IT_SEQ-VM_MODEL    .
      L_CNT    = 1                  .
      IF IT_SEQ-MITU = 'Y'      .
        L_MITUCNT = 1.                                      "UD1K912950
*        l_mitucnt = l_mitucnt + 1    .               "UD1K912950
      ELSE.
        CLEAR: L_MITUCNT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SEQ  LINES L_SIZE.
  IF L_SIZE > 0 .
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
      PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                          CHANGING LT_SEQ-VALS                    .
      LT_SEQ-VM_MODEL   = L_MODEL      .
      LT_SEQ-WORDER     = L_WORDER          .
      LT_SEQ-EXTC       = L_EXTC            .
      LT_SEQ-INTC       = L_INTC            .
      LT_SEQ-CNT        = L_CNT             .
      LT_SEQ-MITUCNT    = L_MITUCNT         .
      LT_SEQ-CODE       = IT_ALC-FULL_ALC   .
      APPEND LT_SEQ               .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT LT_SEQ BY VM_MODEL CODE VALS.
  CLEAR: IT_SEQ, IT_SEQ[], L_CNT, L_MITUCNT.
  READ TABLE LT_SEQ INDEX 1.
  L_MODEL =  LT_SEQ-VM_MODEL  .
  L_CODE  =  LT_SEQ-CODE   .
  L_VALS  =  LT_SEQ-VALS   .
  IT_SEQ  =  LT_SEQ        .

  LOOP AT LT_SEQ.
    IF L_MODEL = LT_SEQ-VM_MODEL AND L_CODE = LT_SEQ-CODE AND
       L_VALS  = LT_SEQ-VALS   .
      L_CNT   = L_CNT   + LT_SEQ-CNT  .
      L_MITUCNT  = L_MITUCNT  + LT_SEQ-MITUCNT .
      CONTINUE.
    ELSE.
      IT_SEQ-VM_MODEL = L_MODEL .
      IT_SEQ-CNT      = L_CNT   .
      IT_SEQ-MITUCNT  = L_MITUCNT  .
      APPEND IT_SEQ          .
      IT_SEQ  = LT_SEQ    .
      L_MODEL = LT_SEQ-VM_MODEL .
      L_CODE  = LT_SEQ-CODE  .
      L_VALS  = LT_SEQ-VALS  .
      L_CNT   = LT_SEQ-CNT  .
      L_MITUCNT  = LT_SEQ-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_SEQ LINES L_SIZE.
  IF L_SIZE > 0.
    IT_SEQ-VM_MODEL = L_MODEL .
    IT_SEQ-CNT  = L_CNT    .
    IT_SEQ-MITUCNT = L_MITUCNT   .
    APPEND IT_SEQ         .
  ENDIF.
ENDFORM.                    " CALC_SEQ

*&---------------------------------------------------------------------*
*&      Form  CALC_BODYINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_BODYINPUT.
  DATA: L_CNT                TYPE I ,
        L_SIZE               TYPE I ,
        L_MODEL              LIKE IT_SEQ-VM_MODEL,
        L_WORDER             LIKE MARA-MATNR  ,
        L_EXTC               LIKE ZTPP_INPUT_PLAN-EXTC,
        L_INTC               LIKE ZTPP_INPUT_PLAN-INTC,
        L_CODE               LIKE IT_SEQ-CODE ,
        L_DAYS(2)            TYPE N           ,
        L_VALS               LIKE IT_SEQ-VALS ,
        LT_BI                LIKE TABLE OF IT_SUM      WITH HEADER LINE.

  DESCRIBE TABLE IT_BI LINES L_CNT  .
  CHECK L_CNT > 0 .

  SORT IT_BI BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_BI   INDEX 1.    CLEAR: L_CNT.
  L_DAYS   = IT_SEQ-DAYS    .
  L_WORDER = IT_BI-WORDER  .
  L_EXTC   = IT_BI-EXTC    .
  L_INTC   = IT_BI-INTC    .
  L_MODEL  = IT_BI-VM_MODEL.
  LT_BI    = IT_BI         .

  LOOP AT IT_BI             .
    IF L_DAYS  = IT_BI-DAYS  AND L_WORDER = IT_BI-WORDER AND
       L_EXTC   = IT_BI-EXTC  AND L_INTC = IT_BI-INTC   AND
       L_MODEL = IT_BI-VM_MODEL .
      L_CNT    = L_CNT  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
        PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                            CHANGING LT_BI-VALS                    .
        LT_BI-VM_MODEL   = L_MODEL      .
        LT_BI-WORDER     = L_WORDER          .
        LT_BI-EXTC       = L_EXTC            .
        LT_BI-INTC       = L_INTC            .
        LT_BI-CNT        = L_CNT             .
        LT_BI-CODE       = IT_ALC-FULL_ALC   .
        APPEND LT_BI               .
      ENDLOOP.
      LT_BI    = IT_BI             .
      L_WORDER = IT_BI-WORDER      .
      L_EXTC   = IT_BI-EXTC        .
      L_INTC   = IT_BI-INTC        .
      L_MODEL  = IT_BI-VM_MODEL    .
      L_CNT    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_BI   LINES L_SIZE .
  IF L_SIZE > 0 .
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
      PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                          CHANGING LT_BI-VALS                    .
      LT_BI-VM_MODEL   = L_MODEL      .
      LT_BI-WORDER     = L_WORDER          .
      LT_BI-EXTC       = L_EXTC            .
      LT_BI-INTC       = L_INTC            .
      LT_BI-CNT        = L_CNT             .
      LT_BI-CODE       = IT_ALC-FULL_ALC   .
      APPEND LT_BI                .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT LT_BI  BY VM_MODEL CODE VALS.
  CLEAR: IT_BI, IT_BI[], L_CNT.
  READ TABLE LT_BI  INDEX 1.
  L_MODEL =  LT_BI-VM_MODEL  .
  L_CODE  =  LT_BI-CODE   .
  L_VALS  =  LT_BI-VALS   .
  IT_BI   =  LT_BI        .

  LOOP AT LT_BI .
    IF L_MODEL = LT_BI-VM_MODEL AND L_CODE = LT_BI-CODE AND
       L_VALS  = LT_BI-VALS   .
      L_CNT   = L_CNT   + LT_BI-CNT  .
      CONTINUE.
    ELSE.
      IT_BI-VM_MODEL = L_MODEL .
      IT_BI-CNT   = L_CNT   .
      APPEND IT_BI           .
      IT_BI   = LT_BI    .
      L_MODEL = LT_BI-VM_MODEL .
      L_CODE  = LT_BI-CODE  .
      L_VALS  = LT_BI-VALS  .
      L_CNT   = LT_BI-CNT  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_BI  LINES L_SIZE.
  IF L_SIZE > 0.
    IT_BI-VM_MODEL = L_MODEL .
    IT_BI-CNT  = L_CNT    .
    APPEND IT_BI          .
  ENDIF.
ENDFORM.                    " CALC_BODYINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_PAINTINPUT.
  DATA: L_CNT                TYPE I ,
        L_SIZE               TYPE I ,
        L_ATINN              LIKE CABN-ATINN,
        L_MODEL              LIKE IT_SEQ-VM_MODEL,
        L_WORDER             LIKE MARA-MATNR  ,
        L_EXTC               LIKE ZTPP_INPUT_PLAN-EXTC,
        L_INTC               LIKE ZTPP_INPUT_PLAN-INTC,
        L_DAYS(2)            TYPE N           ,
        L_CODE               LIKE IT_SEQ-CODE ,
        L_VALS               LIKE IT_SEQ-VALS ,
        LT_PI                LIKE TABLE OF IT_SUM      WITH HEADER LINE.

  DESCRIBE TABLE IT_PI LINES L_CNT  .
  CHECK L_CNT > 0 .

  SORT IT_PI BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_PI   INDEX 1.    CLEAR: L_CNT.
  L_DAYS   = IT_PI-DAYS    .
  L_WORDER = IT_PI-WORDER  .
  L_EXTC   = IT_PI-EXTC    .
  L_INTC   = IT_PI-INTC    .
  L_MODEL  = IT_PI-VM_MODEL.
  LT_PI    = IT_PI         .

  LOOP AT IT_PI             .
    IF L_DAYS  = IT_PI-DAYS  AND L_WORDER  = IT_PI-WORDER AND
       L_EXTC   = IT_PI-EXTC  AND L_INTC = IT_PI-INTC   AND
       L_MODEL = IT_PI-VM_MODEL .
      L_CNT    = L_CNT  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
        PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                            CHANGING LT_PI-VALS                    .
        LT_PI-VM_MODEL   = L_MODEL      .
        LT_PI-WORDER     = L_WORDER          .
        LT_PI-EXTC       = L_EXTC            .
        LT_PI-INTC       = L_INTC            .
        LT_PI-CNT        = L_CNT             .
        LT_PI-CODE       = IT_ALC-FULL_ALC   .
        APPEND LT_PI               .
      ENDLOOP.
      LT_PI    = IT_PI             .
      L_WORDER = IT_PI-WORDER      .
      L_EXTC   = IT_PI-EXTC        .
      L_INTC   = IT_PI-INTC        .
      L_MODEL  = IT_PI-VM_MODEL    .
      L_CNT    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PI   LINES L_SIZE.
  IF L_SIZE > 0 .
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
      PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                          CHANGING LT_PI-VALS                    .
      LT_PI-VM_MODEL   = L_MODEL      .
      LT_PI-WORDER     = L_WORDER          .
      LT_PI-EXTC       = L_EXTC            .
      LT_PI-INTC       = L_INTC            .
      LT_PI-CNT        = L_CNT             .
      LT_PI-CODE       = IT_ALC-FULL_ALC   .
      APPEND LT_PI                .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT LT_PI  BY VM_MODEL CODE VALS.
  CLEAR: IT_PI, IT_PI[], L_CNT.
  READ TABLE LT_PI  INDEX 1.
  L_MODEL =  LT_PI-VM_MODEL  .
  L_CODE  =  LT_PI-CODE   .
  L_VALS  =  LT_PI-VALS   .
  IT_PI   =  LT_PI        .

  LOOP AT LT_PI .
    IF L_MODEL = LT_PI-VM_MODEL AND L_CODE = LT_PI-CODE AND
       L_VALS  = LT_PI-VALS   .
      L_CNT   = L_CNT   + LT_PI-CNT  .
      CONTINUE.
    ELSE.
      IT_PI-VM_MODEL = L_MODEL .
      IT_PI-CNT   = L_CNT   .
      APPEND IT_PI           .
      IT_PI   = LT_PI    .
      L_MODEL = LT_PI-VM_MODEL .
      L_CODE  = LT_PI-CODE  .
      L_VALS  = LT_PI-VALS  .
      L_CNT   = LT_PI-CNT  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_PI  LINES L_SIZE.
  IF L_SIZE > 0.
    IT_PI-VM_MODEL = L_MODEL .
    IT_PI-CNT  = L_CNT    .
    APPEND IT_PI          .
  ENDIF.
ENDFORM.                    " CALC_PAINTINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_PBS.
  DATA: L_CNT                TYPE I ,
        L_SIZE               TYPE I ,
        L_MODEL              LIKE IT_SEQ-VM_MODEL,
        L_WORDER             LIKE MARA-MATNR  ,
        L_EXTC               LIKE ZTPP_INPUT_PLAN-EXTC,
        L_INTC               LIKE ZTPP_INPUT_PLAN-INTC,
        L_DAYS(2)            TYPE N           ,
        L_CODE               LIKE IT_SEQ-CODE ,
        L_VALS               LIKE IT_SEQ-VALS ,
        LT_PBS               LIKE TABLE OF IT_SUM      WITH HEADER LINE.

  DESCRIBE TABLE IT_PBS LINES L_CNT  .
  CHECK L_CNT > 0 .

  SORT IT_PBS BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_PBS  INDEX 1.    CLEAR: L_CNT.
  L_DAYS   = IT_PBS-DAYS    .
  L_WORDER = IT_PBS-WORDER  .
  L_EXTC   = IT_PBS-EXTC    .
  L_INTC   = IT_PBS-INTC    .
  L_MODEL  = IT_PBS-VM_MODEL.
  LT_PBS   = IT_PBS         .

  LOOP AT IT_PBS            .
    IF L_DAYS  = IT_PBS-DAYS  AND L_WORDER  = IT_PBS-WORDER AND
       L_EXTC   = IT_PBS-EXTC  AND L_INTC = IT_PBS-INTC   AND
       L_MODEL = IT_PBS-VM_MODEL .
      L_CNT    = L_CNT  + 1          .
      CONTINUE .
    ELSE.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
        PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                            CHANGING LT_PBS-VALS                    .
        LT_PBS-VM_MODEL   = L_MODEL      .
        LT_PBS-WORDER     = L_WORDER          .
        LT_PBS-EXTC       = L_EXTC            .
        LT_PBS-INTC       = L_INTC            .
        LT_PBS-CNT        = L_CNT             .
        LT_PBS-CODE       = IT_ALC-FULL_ALC   .
        APPEND LT_PBS               .
      ENDLOOP.
      LT_PBS   = IT_PBS             .
      L_WORDER = IT_PBS-WORDER      .
      L_EXTC   = IT_PBS-EXTC        .
      L_INTC   = IT_PBS-INTC        .
      L_MODEL  = IT_PBS-VM_MODEL    .
      L_CNT    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PBS  LINES L_SIZE.
  IF L_SIZE > 0 .
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
      PERFORM GET_ALC_VALUE  USING L_WORDER        IT_ALC-FULL_ALC
                          CHANGING LT_PBS-VALS                    .
      LT_PBS-VM_MODEL   = L_MODEL      .
      LT_PBS-WORDER     = L_WORDER          .
      LT_PBS-EXTC       = L_EXTC            .
      LT_PBS-INTC       = L_INTC            .
      LT_PBS-CNT        = L_CNT             .
      LT_PBS-CODE       = IT_ALC-FULL_ALC   .
      APPEND LT_PBS               .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT LT_PBS BY VM_MODEL CODE VALS.
  CLEAR: IT_PBS, IT_PBS[], L_CNT.
  READ TABLE LT_PBS INDEX 1.
  L_MODEL =  LT_PBS-VM_MODEL  .
  L_CODE  =  LT_PBS-CODE   .
  L_VALS  =  LT_PBS-VALS   .
  IT_PBS  =  LT_PBS        .

  LOOP AT LT_PBS.
    IF L_MODEL = LT_PBS-VM_MODEL AND L_CODE = LT_PBS-CODE AND
       L_VALS  = LT_PBS-VALS   .
      L_CNT   = L_CNT   + LT_PBS-CNT  .
      CONTINUE.
    ELSE.
      IT_PBS-VM_MODEL = L_MODEL .
      IT_PBS-CNT   = L_CNT   .
      APPEND IT_PBS          .
      IT_PBS  = LT_PBS    .
      L_MODEL = LT_PBS-VM_MODEL .
      L_CODE  = LT_PBS-CODE  .
      L_VALS  = LT_PBS-VALS  .
      L_CNT   = LT_PBS-CNT  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_PBS LINES L_SIZE.
  IF L_SIZE > 0.
    IT_PBS-VM_MODEL = L_MODEL .
    IT_PBS-CNT  = L_CNT    .
    APPEND IT_PBS         .
  ENDIF.
ENDFORM.                    " CALC_PBS

*&---------------------------------------------------------------------*
*&      Form  GET_WORORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_TYPE_ALC  text
*      -->P_L_WORDER  text
*----------------------------------------------------------------------*
FORM GET_WORORDER USING  PA_TYPE  PA_WORDER  PA_WO  PA_EXT  PA_INT .
  IF PA_TYPE = 'C'.
    CONCATENATE PA_WO  PA_EXT  PA_INT  INTO  PA_WORDER .
  ELSE.
    PA_WORDER = PA_WO .
  ENDIF.
ENDFORM.                    " GET_WORORDER

*&---------------------------------------------------------------------*
*&      Form  INSERT_FIELD_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_FIELD_VALS.
  " Insert w-1 Field Value into Internal Table IT_DISP...
  LOOP AT IT_DISP.
    READ TABLE IT_DISP_D1 WITH KEY MODEL    = IT_DISP-MODEL
                                   ALC_CODE = IT_DISP-ALC_CODE
                                   ALC_VALS = IT_DISP-ALC_VALS .
    IF SY-SUBRC = 0.
      IT_DISP-W_1  = IT_DISP_D1-W_1 .
      DELETE IT_DISP_D1 WHERE MODEL    = IT_DISP-MODEL
                          AND ALC_CODE = IT_DISP-ALC_CODE
                          AND ALC_VALS = IT_DISP-ALC_VALS .
    ELSE.
      CLEAR: IT_DISP-W_1 .
    ENDIF.

    READ TABLE IT_DISP_SEQ  WITH KEY MODEL    = IT_DISP-MODEL
                                     ALC_CODE = IT_DISP-ALC_CODE
                                     ALC_VALS = IT_DISP-ALC_VALS .
    IF SY-SUBRC = 0.
      IT_DISP-SEQ = IT_DISP_SEQ-W_1 .
      IT_DISP-MITU = IT_DISP_SEQ-MITU.
      DELETE IT_DISP_SEQ  WHERE ALC_CODE = IT_DISP-ALC_CODE
                            AND MODEL    = IT_DISP-MODEL
                            AND ALC_VALS = IT_DISP-ALC_VALS .
    ELSE.
      CLEAR: IT_DISP-SEQ, IT_DISP-MITU.
    ENDIF.

    READ TABLE IT_DISP_BI   WITH KEY MODEL    = IT_DISP-MODEL
                                     ALC_CODE = IT_DISP-ALC_CODE
                                     ALC_VALS = IT_DISP-ALC_VALS .
    IF SY-SUBRC = 0.
      IT_DISP-BODYIN = IT_DISP_BI-W_1 .
      DELETE IT_DISP_BI   WHERE ALC_CODE = IT_DISP-ALC_CODE
                            AND MODEL    = IT_DISP-MODEL
                            AND ALC_VALS = IT_DISP-ALC_VALS .
    ELSE.
      CLEAR: IT_DISP-BODYIN  .
    ENDIF.

*    READ TABLE it_disp_wbs  WITH KEY model    = it_disp-model
*                                     alc_code = it_disp-alc_code
*                                     alc_vals = it_disp-alc_vals .
*    IF sy-subrc = 0.
*      it_disp-wbs = it_disp_wbs-w_1 .
*      DELETE it_disp_wbs  WHERE alc_code = it_disp-alc_code
*                            AND model    = it_disp-model
*                            AND alc_vals = it_disp-alc_vals .
*    ELSE.
*      CLEAR: it_disp-wbs .
*    ENDIF.

    READ TABLE IT_DISP_PI   WITH KEY MODEL    = IT_DISP-MODEL
                                     ALC_CODE = IT_DISP-ALC_CODE
                                     ALC_VALS = IT_DISP-ALC_VALS .
    IF SY-SUBRC = 0.
      IT_DISP-PAINT  = IT_DISP_PI-W_1 .
      DELETE IT_DISP_PI   WHERE ALC_CODE = IT_DISP-ALC_CODE
                            AND MODEL    = IT_DISP-MODEL
                            AND ALC_VALS = IT_DISP-ALC_VALS .
    ELSE.
      CLEAR: IT_DISP-PAINT  .
    ENDIF.

*    READ TABLE it_disp_prj  WITH KEY model    = it_disp-model
*                                     alc_code = it_disp-alc_code
*                                     alc_vals = it_disp-alc_vals .
*    IF sy-subrc = 0.
*      it_disp-prj = it_disp_prj-w_1 .
*      DELETE it_disp_prj  WHERE alc_code = it_disp-alc_code
*                            AND model    = it_disp-model
*                            AND alc_vals = it_disp-alc_vals .
*    ELSE.
*      CLEAR: it_disp-prj .
*    ENDIF.

    READ TABLE IT_DISP_PBS  WITH KEY MODEL    = IT_DISP-MODEL
                                     ALC_CODE = IT_DISP-ALC_CODE
                                     ALC_VALS = IT_DISP-ALC_VALS .
    IF SY-SUBRC = 0.
      IT_DISP-PBS = IT_DISP_PBS-W_1 .
      DELETE IT_DISP_PBS  WHERE ALC_CODE = IT_DISP-ALC_CODE
                            AND MODEL    = IT_DISP-MODEL
                            AND ALC_VALS = IT_DISP-ALC_VALS .
    ELSE.
      CLEAR: IT_DISP-PBS .
    ENDIF.

    MODIFY IT_DISP.
  ENDLOOP.

  " Remain Fiedls Insert.....
  LOOP AT IT_DISP_D1 .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_D1 TO IT_DISP.
    CLEAR: IT_DISP-W_1, IT_DISP-W_1.
    IT_DISP-W_1 = IT_DISP_D1-W_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_D1, IT_DISP_D1[].

  LOOP AT IT_DISP_SEQ  .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_SEQ  TO IT_DISP.
    CLEAR: IT_DISP-W_1, IT_DISP-SEQ, IT_DISP-MITU.
    IT_DISP-SEQ = IT_DISP_SEQ-W_1.
    IT_DISP-MITU = IT_DISP_SEQ-MITU.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_SEQ , IT_DISP_SEQ[].

  LOOP AT IT_DISP_BI   .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_BI   TO IT_DISP.
    CLEAR: IT_DISP-W_1, IT_DISP-BODYIN.
    IT_DISP-BODYIN = IT_DISP_BI-W_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_BI  , IT_DISP_BI[].

*  LOOP AT it_disp_wbs  .
*    CLEAR: it_disp.
*    MOVE-CORRESPONDING it_disp_wbs  TO it_disp.
*    CLEAR: it_disp-w_1, it_disp-wbs.
*    it_disp-wbs = it_disp_wbs-w_1.
*    APPEND it_disp.
*  ENDLOOP.
*  CLEAR: it_disp_wbs , it_disp_wbs[].

  LOOP AT IT_DISP_PI   .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_PI   TO IT_DISP.
    CLEAR: IT_DISP-W_1, IT_DISP-PAINT.
    IT_DISP-PAINT  = IT_DISP_PI-W_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_PI  , IT_DISP_PI[].

*  LOOP AT it_disp_prj  .
*    CLEAR: it_disp.
*    MOVE-CORRESPONDING it_disp_prj  TO it_disp.
*    CLEAR: it_disp-w_1, it_disp-prj.
*    it_disp-prj = it_disp_prj-w_1.
*    APPEND it_disp.
*  ENDLOOP.
*  CLEAR: it_disp_prj , it_disp_prj[].

  LOOP AT IT_DISP_PBS  .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_PBS  TO IT_DISP.
    CLEAR: IT_DISP-W_1, IT_DISP-PBS.
    IT_DISP-PBS = IT_DISP_PBS-W_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_PBS , IT_DISP_PBS[].
ENDFORM.                    " INSERT_FIELD_VALS

*&---------------------------------------------------------------------*
*&      Form  CALC_prod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_PROD .
  DATA: L_CNT                TYPE I ,
        L_SIZE               TYPE I ,
        L_MODEL              LIKE IT_SEQ-VM_MODEL,
        L_WORDER             LIKE MARA-MATNR  ,
        L_EXTC               LIKE ZTPP_INPUT_PLAN-EXTC,
        L_INTC               LIKE ZTPP_INPUT_PLAN-INTC,
        L_CODE               LIKE IT_SEQ-CODE ,
        L_VALS               LIKE IT_SEQ-VALS ,
        LT_D1                LIKE TABLE OF IT_SUM      WITH HEADER LINE.

  "1. Take the data of The Table(IT_PROD) of which RP06's Qty is GT 0.

  DELETE IT_PROD WHERE RP06Q = 0 .

  "2. Conversion the Data for the Internal Table Format..
  LOOP AT IT_PROD.
    CLEAR: IT_D1 .
    CONCATENATE IT_PROD-WO_SER IT_PROD-NATION IT_PROD-DEALER
                IT_PROD-EXTC   IT_PROD-INTC   INTO L_WORDER .
    PERFORM GET_MODEL USING L_WORDER  IT_D1-VM_MODEL .
    DO IT_PROD-RP06Q TIMES.
      IT_D1-WORDER     = L_WORDER          .
      IT_D1-EXTC       = IT_PROD-EXTC      .
      IT_D1-INTC       = IT_PROD-INTC      .
      IT_D1-RP         = '06'              .
      IT_D1-CNT        = 1                 .
      APPEND IT_D1  .
    ENDDO.
  ENDLOOP.

  " Create the Previous Production Data..
  SORT IT_D1 BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE  IT_D1 INDEX 1.   CLEAR: L_CNT.
  L_WORDER = IT_D1-WORDER  .
  L_EXTC   = IT_D1-EXTC    .
  L_INTC   = IT_D1-INTC    .
  L_MODEL  = IT_D1-VM_MODEL.
  LT_D1    = IT_D1         .

  LOOP AT IT_D1.
    IF L_WORDER = IT_D1-WORDER AND  L_EXTC   = IT_D1-EXTC   AND
       L_INTC   = IT_D1-INTC   AND  L_MODEL  = IT_D1-VM_MODEL .
      L_CNT    = L_CNT  + 1.
      CONTINUE .
    ELSE.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
        PERFORM GET_ALC_VALUE  USING L_WORDER      IT_ALC-FULL_ALC
                            CHANGING LT_D1-VALS                    .
        LT_D1-VM_MODEL   = L_MODEL           .
        LT_D1-WORDER     = L_WORDER          .
        LT_D1-EXTC       = L_EXTC            .
        LT_D1-INTC       = L_INTC            .
        LT_D1-CNT        = L_CNT             .
        LT_D1-CODE       = IT_ALC-FULL_ALC   .
        APPEND LT_D1                .
      ENDLOOP.
      LT_D1    = IT_D1              .
      L_WORDER = IT_D1-WORDER      .
      L_EXTC   = IT_D1-EXTC        .
      L_INTC   = IT_D1-INTC        .
      L_MODEL  = IT_D1-VM_MODEL    .
      L_CNT    = 1                  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_D1 LINES L_SIZE .
  IF L_SIZE > 0 .
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      CONCATENATE  L_WORDER  L_EXTC   L_INTC  INTO L_WORDER       .
      PERFORM GET_ALC_VALUE  USING L_WORDER      IT_ALC-FULL_ALC
                          CHANGING LT_D1-VALS                    .
      LT_D1-VM_MODEL   = L_MODEL           .
      LT_D1-WORDER     = L_WORDER          .
      LT_D1-EXTC       = L_EXTC            .
      LT_D1-INTC       = L_INTC            .
      LT_D1-CNT        = L_CNT             .
      LT_D1-CODE       = IT_ALC-FULL_ALC   .
      APPEND LT_D1                .
    ENDLOOP.
  ENDIF.

  " Summary ...
  SORT LT_D1 BY VM_MODEL CODE VALS.
  CLEAR: IT_D1, IT_D1[], L_CNT.
  READ TABLE LT_D1 INDEX 1.
  L_MODEL =  LT_D1-VM_MODEL  .
  L_CODE  =  LT_D1-CODE   .
  L_VALS  =  LT_D1-VALS   .
  IT_D1   =  LT_D1        .

  LOOP AT LT_D1.
    IF L_MODEL = LT_D1-VM_MODEL AND L_CODE = LT_D1-CODE AND
       L_VALS  = LT_D1-VALS   .
      L_CNT   = L_CNT   + LT_D1-CNT  .
      CONTINUE.
    ELSE.
      IT_D1-VM_MODEL = L_MODEL .
      IT_D1-CNT   = L_CNT   .
      APPEND IT_D1          .
      IT_D1   = LT_D1    .
      L_MODEL = LT_D1-VM_MODEL .
      L_CODE  = LT_D1-CODE  .
      L_VALS  = LT_D1-VALS  .
      L_CNT   = LT_D1-CNT  .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_D1 LINES L_SIZE.
  IF L_SIZE > 0.
    IT_D1-VM_MODEL = L_MODEL .
    IT_D1-CNT  = L_CNT    .
    APPEND IT_D1         .
  ENDIF.
ENDFORM.                    " calc_prod

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = PA_TYPE
            DATE                         = PA_WDATE
            FACTORY_CALENDAR_ID          = PA_KALID
       IMPORTING
            DATE                         = PA_WDATE
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING    PA_KALID.
  SELECT SINGLE KALID INTO PA_KALID
    FROM ZVPP_CAPACITY
   WHERE ARBPL = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  GET_MODELS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MODELS.
  DATA: L_ATWRT          LIKE AUSP-ATWRT,
        L_ATINN          LIKE CABN-ATINN.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT INTO L_ATWRT
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    IT_MODEL-MODL = L_ATWRT(3).
    APPEND IT_MODEL .
  ENDSELECT.
ENDFORM.                    " GET_MODELS

*&---------------------------------------------------------------------*
*&      Form  SET_INFORMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_INFORMATION.
  DATA: L_DATE               TYPE D ,
        L_WDATE              TYPE D ,
        L_WEEK               TYPE I ,
        L_COUNT              TYPE I .

  " Set the BASIC Information for the UPH & Work Time...
  CLEAR: L_DATE, L_COUNT.
  L_DATE = P_DATES - 1 .
  PERFORM LAST_OF_WEEK        USING L_DATE  L_WDATE      .
  DO 7 TIMES.
    L_DATE = L_DATE + 1 .
    PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE.
    IF L_DATE <=  L_WDATE .   "09.16.2014
      PERFORM GET_DAY         USING L_DATE IT_MASTER-DAY  .
      PERFORM GET_WORKTIME1   USING L_DATE IT_MASTER-TIME
                                    IT_MASTER-DAY      'T'.
      PERFORM GET_UPH         USING L_DATE IT_MASTER-UPH
                                    IT_MASTER-SHIFT    'T'.
    ELSE.
      EXIT.
    ENDIF.
    IT_MASTER-SEQ    = 1.     IT_MASTER-DATE   = L_DATE .
    APPEND IT_MASTER.  CLEAR: IT_MASTER.
  ENDDO.

  " From W+2 Day To W+21
  L_DATE = WA_WDATE = L_WDATE + 1.
*  PERFORM get_weeks USING '+' l_date wa_wdate .
  L_DATE = L_DATE - 1 .
  DO 140 TIMES.
    L_COUNT  = L_COUNT + 1.
    PERFORM SET_COUNT_WEEK USING L_COUNT L_WEEK.
    WA_WDATE  = L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+'  WA_KALID  WA_WDATE.
    IF WA_WDATE  NE L_DATE .
      IT_MASTER-SEQ = L_WEEK + 2.   IT_MASTER-DATE   = WA_WDATE .
      PERFORM GET_DAY          USING WA_WDATE IT_MASTER-DAY  .
      PERFORM GET_WORKTIME1    USING WA_WDATE IT_MASTER-TIME
                                     IT_MASTER-DAY     'T' .
      PERFORM GET_UPH          USING WA_WDATE IT_MASTER-UPH
                                     IT_MASTER-SHIFT   'T' .
      APPEND IT_MASTER.  CLEAR: IT_MASTER.
      L_DATE = WA_WDATE.
      CONTINUE.
    ENDIF.
    PERFORM GET_DAY       USING L_DATE IT_MASTER-DAY  .
   PERFORM GET_WORKTIME1 USING L_DATE IT_MASTER-TIME IT_MASTER-DAY  'T'.
   PERFORM GET_UPH       USING L_DATE IT_MASTER-UPH IT_MASTER-SHIFT 'T'.
    IT_MASTER-SEQ    = L_WEEK + 2 .  IT_MASTER-DATE   = L_DATE .
    APPEND IT_MASTER.  CLEAR: IT_MASTER.
  ENDDO.

  LOOP AT IT_MASTER WHERE DAY = 7 AND SEQ > 1 .
    IT_MASTER-SEQ =  IT_MASTER-SEQ - 1.
    MODIFY IT_MASTER.
  ENDLOOP.

* set MRP schedule last week---chris li
  PERFORM GET_MRP_LAST_WEEK CHANGING G_MRP_LAST_WEEK.
* end of change on 04/05/2005
ENDFORM.                    " SET_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM GET_WORKTIME1 USING    PA_WDATE  PA_WKTIME  PA_DAY  PA_ARBPL.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA       LIKE TABLE OF ZVPP_CAPACITY      WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL =  PA_ARBPL
     AND DATUB >= PA_WDATE .

  SORT LT_CAPA BY DATUB .
  READ TABLE LT_CAPA INDEX 1.
  L_DATE = LT_CAPA-DATUB    .

  LOOP AT LT_CAPA WHERE DATUB = L_DATE AND TAGNR = PA_DAY .
    CLEAR: L_EINZT.
    SELECT SINGLE EINZT INTO L_EINZT
      FROM TC37A
     WHERE SCHGRUP  = LT_CAPA-MOSID
       AND KAPTPROG = LT_CAPA-TPROG
       AND ENDDA   >= PA_WDATE
       AND BEGDA   <= PA_WDATE     .
    L_WTIME  = L_WTIME  + L_EINZT.
  ENDLOOP.
  PA_WKTIME = L_WTIME .
ENDFORM.                    " GET_WORKTIME1

*&---------------------------------------------------------------------*
*&      Form  GET_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM GET_DAY USING    PA_WDATE  PA_DAY.
  DATA: L_DAY         LIKE SCAL-INDICATOR .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            DATE = PA_WDATE
       IMPORTING
            DAY  = L_DAY.

  PA_DAY = L_DAY.
ENDFORM.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  GET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM GET_UPH USING    PA_WDATE  PA_UPH  PA_SHIFT  PA_ARBPL .
  DATA: W_UPH  LIKE ZTPP_STATUS-UPH.
  CALL FUNCTION 'Z_FPP_GET_UPH'
       EXPORTING
            DATE  = PA_WDATE
            SHIFT = PA_SHIFT
            SHOP  = PA_ARBPL
       IMPORTING
            UPH   = W_UPH.
  PA_UPH = W_UPH.

*  DATA lw_ld          LIKE zvpp_ld .
*
*  data: lt_ld   like zvpp_ld occurs 0 with header line.
*
*  IF pa_shift IS INITIAL .
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = pa_arbpl   .
*
** end of change on 06/13/2005
*  ELSE.
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
** and one shift could have more than one record
** to difine diferent rate for different period
** of time
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND ld_shift  = pa_shift
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND ld_shift  = pa_shift
*       AND arbpl     = pa_arbpl   .
*
*
*  ENDIF.
** add by chris on 06/13/2005
*    loop at lt_ld.
*      lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
*      lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
*    endloop.
** end of add.
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH

*&---------------------------------------------------------------------*
*&      Form  GET_START_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM GET_START_DAY USING    PA_DATUM.
  CLEAR: ZTPP_COMMON_VALS.
  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = C_JOBS
     AND KEY2 = C_KEY1 .

  PA_DATUM = ZTPP_COMMON_VALS-DATES.
ENDFORM.                    " GET_START_DAY

*&---------------------------------------------------------------------*
*&      Form  CLEAR_QTY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DISP_PRJ  text
*----------------------------------------------------------------------*
FORM CLEAR_QTY_DISP USING    PA_DISP  STRUCTURE IT_DISP .
  CLEAR: PA_DISP-W01, PA_DISP-W02, PA_DISP-W03, PA_DISP-W04,
         PA_DISP-W05, PA_DISP-W06, PA_DISP-W07, PA_DISP-W08,
         PA_DISP-W09, PA_DISP-W10, PA_DISP-W11, PA_DISP-W12,
         PA_DISP-W13, PA_DISP-W14, PA_DISP-W15, PA_DISP-W16,
         PA_DISP-W17, PA_DISP-W18, PA_DISP-W19, PA_DISP-W20,
         PA_DISP-W21, PA_DISP-W_1, PA_DISP-SEQ, PA_DISP-BODYIN,
         PA_DISP-WBS, PA_DISP-PRJ, PA_DISP-PBS, PA_DISP-PAINT,
         PA_DISP-MITU, PA_DISP-W99.                 "Missed UD1K912950
ENDFORM.                    " CLEAR_QTY_DISP

*&---------------------------------------------------------------------*
*&      Form  seT_count_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
FORM SET_COUNT_WEEK USING    PA_COUNT  PA_DAY .
  DATA: L_MOD       TYPE     P DECIMALS 1.

  L_MOD = CEIL( PA_COUNT DIV 7 ).
  PA_DAY  = L_MOD.
ENDFORM.                    " seT_count_week

*&---------------------------------------------------------------------*
*&      Form  LAST_OF_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LF_DATE  text
*      -->P_LT_DATE  text
*----------------------------------------------------------------------*
FORM LAST_OF_WEEK USING  PA_DATE  PA_LDATE.
  DATA: L_WEEK          LIKE SCAL-WEEK,
        L_DATE          TYPE D.

  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            DATE         = PA_DATE
       IMPORTING
            WEEK         = L_WEEK
       EXCEPTIONS
            DATE_INVALID = 1
            OTHERS       = 2.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
       EXPORTING
            WEEK         = L_WEEK
       IMPORTING
            DATE         = L_DATE
       EXCEPTIONS
            WEEK_INVALID = 1
            OTHERS       = 2.

  PA_LDATE = L_DATE + 6    .
ENDFORM.                    " LAST_OF_WEEK

*&---------------------------------------------------------------------*
*&      Form  GET_WEEKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LF_DATE  text
*      -->P_LT_DATE  text
*----------------------------------------------------------------------*
FORM GET_WEEKS USING  PA_TYPE  PA_FDATE  PA_TDATE.
  DATA: L_WEEK          LIKE SCAL-WEEK,
        L_DATE          TYPE D.

* REQUESTED BY MY. HUR CHANGED BY CHRIS ON 02/22/2005

*  CASE pa_type.
*    WHEN '-'.
*      l_date = p_dates - 7 .
*    WHEN '+'.
*      l_date = p_dates + 7 .
*  ENDCASE.


*  CALL FUNCTION 'DATE_GET_WEEK'
*       EXPORTING
*            date         = l_date
*       IMPORTING
*            week         = l_week
*       EXCEPTIONS
*            date_invalid = 1
*            OTHERS       = 2.
*
*  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
*       EXPORTING
*            week         = l_week
*       IMPORTING
*            date         = pa_fdate
*       EXCEPTIONS
*            week_invalid = 1
*            OTHERS       = 2.
  PA_FDATE = P_DATES - 7    .
  PA_TDATE = P_DATES - 1    .

ENDFORM.                    " GET_WEEKS

*&---------------------------------------------------------------------*
*&      Form  get_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*      -->P_IT_SUM_PROD_VM_MODEL  text
*----------------------------------------------------------------------*
FORM GET_MODEL USING    PA_WORDER  PA_MODEL.
  DATA: L_ATINN         LIKE AUSP-ATINN.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT SINGLE ATWRT INTO PA_MODEL
    FROM AUSP
   WHERE OBJEK = PA_WORDER
     AND ATINN = L_ATINN
     AND KLART = '001'   .
ENDFORM.                    " get_model
*&---------------------------------------------------------------------*
*&      Form  get_1stweek_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_1STWEEK_DATE USING PA_FDATE PA_TDATE P_REMAIN.
  DATA: L_WEEK          LIKE SCAL-WEEK,
        L_DATE          TYPE D.


  L_DATE = PA_FDATE.
  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            DATE         = L_DATE
       IMPORTING
            WEEK         = L_WEEK
       EXCEPTIONS
            DATE_INVALID = 1
            OTHERS       = 2.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
       EXPORTING
            WEEK         = L_WEEK
       IMPORTING
            DATE         = L_DATE
       EXCEPTIONS
            WEEK_INVALID = 1
            OTHERS       = 2.

  PA_TDATE = L_DATE + 6    .
  P_REMAIN = PA_TDATE - PA_FDATE + 1.
ENDFORM.                    " get_1stweek_date
*&---------------------------------------------------------------------*
*&      Form  check_create_fwo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DATA  text
*----------------------------------------------------------------------*
FORM CHECK_CREATE_FWO TABLES   PT_DATA STRUCTURE ZTPP_PMT07JB_A.
  DATA: BEGIN OF LT_DATA OCCURS 0.
          INCLUDE STRUCTURE ZTPP_PMT07JB_A.
  DATA:   EORDER(14) TYPE C.
  DATA:   MARK  TYPE C,
          MARKC TYPE C.
  DATA: END OF LT_DATA.
*  DATA: lt_fwo  LIKE ztpp_fwo       OCCURS 0 WITH HEADER LINE.
  DATA: L_WO    LIKE LT_FWO-WORDER.
  DATA: L_FWO   LIKE LT_FWO-WORDER.
  DATA: L_FWOC  LIKE LT_FWO-WORDER.
  DATA: L_WOC   LIKE LT_FWO-WORDER.
  DATA: FSC     LIKE ZTPP_WOSUM-FSC.
  DATA: BEGIN OF LT_KH OCCURS 0,
          OBJEK LIKE ZKHAUSP-OBJEK,
          ATNAM LIKE ZKHAUSP-ATNAM,
          ATWRT LIKE ZKHAUSP-ATWRT,
        END OF LT_KH.
  DATA: I_LINE  TYPE I.
  DATA: BEGIN OF LT_WO OCCURS 0,
          WO_SER  LIKE ZTPP_WOSUM-WO_SER,
          NATION  LIKE ZTPP_WOSUM-NATION,
          DEALER  LIKE ZTPP_WOSUM-DEALER,
          EXTC    LIKE ZTPP_WOSUM-EXTC,
          INTC    LIKE ZTPP_WOSUM-INTC,
          FSC     LIKE ZTPP_WOSUM-FSC,
          VERSION LIKE ZTPP_WOSUM-VERSION,
          WOMODDATE LIKE ZTPP_WOSUM-WOMODDATE,
        END OF LT_WO.
  DATA: L_TEXT(30).
  DATA: L_LEN TYPE I,
          L_NEW_DEALER(1).

* read work order summary
  SELECT WO_SER NATION DEALER EXTC INTC
        FSC VERSION WOMODDATE
    INTO TABLE LT_WO
    FROM ZTPP_WOSUM.
  SORT LT_WO BY  FSC       ASCENDING
                 EXTC      ASCENDING
                 INTC      ASCENDING
** changed by Furong on 05/04/2006, requested by MY Hur
*                 version   ASCENDING
                 VERSION   DESCENDING
** end of change
                 WOMODDATE DESCENDING.

  LT_DATA[]   =  PT_DATA[].

** added by Furong on 05/02/2006
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM ZTPP_PMT07JB_A
      WHERE GUBB = 'A'.

  SORT LT_DATA BY SQDT SSR1      .
** end of addition
* CHECK IF FORCAST ORDER EXIST

  LOOP AT LT_DATA.
    IF LT_DATA-ORDR(1) NE 'F'.
      DELETE LT_DATA.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE LT_DATA LINES I_LINE.
  IF I_LINE EQ 0.
    EXIT.
  ENDIF.
*************
  DATA : IT_TEMP LIKE ZTPP_FWO OCCURS 0 WITH HEADER LINE.
  REFRESH IT_TEMP.
  SELECT * INTO TABLE IT_TEMP FROM ZTPP_FWO.
  DELETE  ZTPP_FWO     FROM TABLE IT_TEMP.
  COMMIT WORK.
  REFRESH IT_TEMP.
  FREE IT_TEMP.
***************

* make the alc data for f-order with out alc code
  LOOP AT LT_DATA .

* MAKE FSC
** Changed by Furong on 10/10/07 for EBOM
    L_LEN = STRLEN( LT_DATA-BMDL ).
    IF L_LEN = 7.

      CONCATENATE LT_DATA-MOYE
                  LT_DATA-DIST
                  LT_DATA-BMDL
             INTO FSC.
      CONCATENATE FSC LT_DATA-OCNN INTO FSC
             SEPARATED BY SPACE.
    ELSE.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
           EXPORTING
                OLD_DEALER = LT_DATA-DIST+3(2)
           IMPORTING
                NEW_DEALER = L_NEW_DEALER.


      CONCATENATE LT_DATA-MOYE
                   LT_DATA-DIST+0(3)
                   L_NEW_DEALER
                   LT_DATA-BMDL
              INTO FSC.
      CONCATENATE FSC LT_DATA-OCNN INTO FSC.
    ENDIF.
** End of change
* READ ACUTUAL WORK ORDER
    CLEAR: LT_WO.
    READ TABLE LT_WO WITH KEY  FSC      = FSC
                               EXTC     = LT_DATA-EXTC
                               INTC     = LT_DATA-INTC
                               VERSION  = LT_DATA-VERS
                               BINARY SEARCH.
*  if not exist, don't use version and try again
    IF SY-SUBRC NE 0.
*     to get the latest one, sort it first
*      SORT lt_wo BY womoddate DESCENDING.
      CLEAR: LT_WO.
      READ TABLE LT_WO WITH KEY  FSC      = FSC
                                 EXTC     = LT_DATA-EXTC
                                 INTC     = LT_DATA-INTC
                                 BINARY SEARCH.
*     if found, save the actual work order
      IF SY-SUBRC EQ 0.
        CONCATENATE LT_WO-WO_SER
                    LT_WO-NATION
                    LT_WO-DEALER
               INTO LT_DATA-EORDER.
        LT_DATA-MARK = 'Y'.
      ELSE.
*         if not found, set the mark
        LT_DATA-MARK = 'N'.
      ENDIF.
    ELSE.
*     found the work order for fsc + version
      CONCATENATE   LT_WO-WO_SER
                    LT_WO-NATION
                    LT_WO-DEALER
               INTO LT_DATA-EORDER.
      LT_DATA-MARK = 'Y'.
    ENDIF.

    MODIFY LT_DATA.

  ENDLOOP.

** read the all ALC code for f-order with E-order
*  REFRESH lt_kh.
*  LOOP AT lt_data.
*    CONCATENATE lt_data-eorder '%' INTO l_text.
*    SELECT objek atnam atwrt
*      APPENDING TABLE lt_kh
*      FROM zkhausp
*      WHERE klart = '001'
*        AND objek LIKE l_text
*        AND ( atnam LIKE 'P_ALC_U%' OR
*              atnam  LIKE 'P_ALC_C%' ).
*  ENDLOOP .
*
*  SORT lt_kh BY objek atnam.
*  DELETE ADJACENT DUPLICATES FROM lt_kh
*     COMPARING objek atnam.

  LOOP AT LT_DATA.
*   make the worder order and work order color
    CONCATENATE LT_DATA-ORDR LT_DATA-DIST INTO L_FWO.
    CONCATENATE L_FWO
                LT_DATA-EXTC
                LT_DATA-INTC
           INTO L_FWOC.
    L_WO = LT_DATA-EORDER.
    CONCATENATE L_WO
                LT_DATA-EXTC
                LT_DATA-INTC
           INTO L_WOC.

** Changed by Furong on 10/10/07 for EBOM
*  CONCATENATE LT_DATA-MOYE
*                 LT_DATA-DIST
*                 LT_DATA-BMDL
*            INTO FSC.
*    CONCATENATE FSC LT_DATA-OCNN INTO FSC
*           SEPARATED BY SPACE.
    L_LEN = STRLEN( LT_DATA-BMDL ).
    IF L_LEN = 7.

      CONCATENATE LT_DATA-MOYE
                  LT_DATA-DIST
                  LT_DATA-BMDL
             INTO FSC.
      CONCATENATE FSC LT_DATA-OCNN INTO FSC
             SEPARATED BY SPACE.
    ELSE.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
           EXPORTING
                OLD_DEALER = LT_DATA-DIST+3(2)
           IMPORTING
                NEW_DEALER = L_NEW_DEALER.


      CONCATENATE LT_DATA-MOYE
                   LT_DATA-DIST+0(3)
                   L_NEW_DEALER
                   LT_DATA-BMDL
              INTO FSC.
      CONCATENATE FSC LT_DATA-OCNN INTO FSC.
    ENDIF.
** End of change

    IF  LT_DATA-MARK EQ 'Y'.
*     read the ALC_U VALUES
      REFRESH LT_KH.
      SELECT OBJEK ATNAM ATWRT
        INTO TABLE LT_KH
        FROM ZKHAUSP
        WHERE KLART = '001'
          AND OBJEK EQ L_WO
*          AND ( atnam LIKE 'P_ALC_U%' OR
*                atnam  LIKE 'P_ALC_C%' ).
          AND ( ATNAM BETWEEN 'P_ALC_U_1' AND 'P_ALC_U_299' ).
*          OR ( atnam between 'P_ALC_C_1' and 'P_ALC_C_50' ).


*     SAVE THE ALC CODE TO F-TABLE
      IF SY-SUBRC = 0.
        LOOP AT LT_KH  .         " WHERE   objek = l_wo
          " AND atnam CS 'P_ALC_U'.
          LT_FWO-WORDER        = L_FWO.
          LT_FWO-MODEL         = LT_DATA-MODL.
          LT_FWO-FSC           = FSC.
          LT_FWO-VERSION       = LT_DATA-VERS.
          LT_FWO-O_WORDER      = LT_KH-OBJEK.
          LT_FWO-ALC_CODE      = LT_KH-ATNAM.
          LT_FWO-ALC_VALS      = LT_KH-ATWRT.
          LT_FWO-ERDAT         = SY-DATUM.
          LT_FWO-ERNAM         = SY-UNAME.
          LT_FWO-ZUZEIT        = SY-UZEIT.
          APPEND LT_FWO.
          CLEAR: LT_FWO.
        ENDLOOP.
      ENDIF.
*     SAVE THE ALC CODE TO F-TABLE
*     READ THE WORK ORDER COLOR ALC_C VALUE
      REFRESH LT_KH.
      SELECT OBJEK ATNAM ATWRT
        INTO TABLE LT_KH
        FROM ZKHAUSP
        WHERE KLART = '001'
          AND OBJEK EQ L_WOC
*          AND atnam LIKE 'P_ALC_C%' .
          AND ATNAM BETWEEN 'P_ALC_C_1' AND 'P_ALC_C_99'.

      IF SY-SUBRC EQ 0.
        LOOP AT LT_KH WHERE  OBJEK = L_WOC
                        AND ATNAM CS 'P_ALC_C'.

          LT_FWO-WORDER       = L_FWOC.
          LT_FWO-MODEL        = LT_DATA-MODL.
          LT_FWO-FSC          = FSC.
          LT_FWO-VERSION      = LT_DATA-VERS.
          LT_FWO-O_WORDER     = LT_KH-OBJEK.
          LT_FWO-ALC_CODE     = LT_KH-ATNAM.
          LT_FWO-ALC_VALS     = LT_KH-ATWRT.
          LT_FWO-ERDAT        = SY-DATUM.
          LT_FWO-ERNAM        = SY-UNAME.
          LT_FWO-ZUZEIT       = SY-UZEIT.

          APPEND LT_FWO.
          CLEAR: LT_FWO.
        ENDLOOP.
      ENDIF.
    ELSE.
      LT_FWO-WORDER       = L_FWOC.
      LT_FWO-MODEL        = LT_DATA-MODL.
      LT_FWO-FSC          = FSC.
      LT_FWO-VERSION      = LT_DATA-VERS.
      LT_FWO-O_WORDER     = 'ERROR'.
      LT_FWO-ALC_CODE     = 'ERROR'.
      LT_FWO-SPARE        = 'No work order exist'.
      APPEND LT_FWO.
      CLEAR: LT_FWO.

    ENDIF.
  ENDLOOP.

  FREE : LT_WO, LT_KH.
  SORT LT_FWO BY WORDER MODEL FSC VERSION ALC_CODE.
  DELETE ADJACENT DUPLICATES FROM LT_FWO
     COMPARING WORDER MODEL FSC VERSION ALC_CODE.
* SAVE THE REAULT TO F-WORK ORDER TABLE
  DESCRIBE TABLE LT_FWO LINES I_LINE.
  IF I_LINE NE 0.
    MODIFY ZTPP_FWO FROM TABLE LT_FWO.
    COMMIT WORK.
    WRITE: / 'FWO has been created. Total number of FWO: ', I_LINE.
  ENDIF.

ENDFORM.                    " check_create_fwo
*&---------------------------------------------------------------------*
*&      Form  check_holding_car
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_HOLDING_CAR.
  DATA : IT_HOLD LIKE ZTPP_HOLD_CAR OCCURS 0 WITH HEADER LINE.

  REFRESH IT_HOLD.

  SELECT * INTO TABLE IT_HOLD FROM ZTPP_HOLD_CAR
         WHERE  ( STATUS EQ 'W' ) OR ( STATUS EQ SPACE ).

  LOOP AT IT_HOLD WHERE RES_DATE > P_DATES.
    READ TABLE IT_DATA WITH KEY MODL = IT_HOLD-MODL
                            BODY_SER = IT_HOLD-BODY_SER.
    IF SY-SUBRC = 0.
*          DELETE TABLE it_data FROM it_data.
      IT_HOLD-STATUS = 'W'.
      MODIFY IT_HOLD FROM IT_HOLD.
    ELSE.
      IT_HOLD-STATUS = 'P'.
      MODIFY IT_HOLD FROM IT_HOLD.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_HOLD WHERE RES_DATE <= P_DATES.
    IF IT_HOLD-RES_DATE =  P_DATES.
      IT_HOLD-STATUS = 'D'.
    ELSE.
      READ TABLE IT_DATA WITH KEY MODL = IT_HOLD-MODL
                           BODY_SER = IT_HOLD-BODY_SER.
      IF SY-SUBRC = 0.
        DELETE TABLE IT_DATA FROM IT_DATA.
      ENDIF.
      IT_HOLD-STATUS = 'P'.
    ENDIF.
    MODIFY IT_HOLD FROM IT_HOLD.
  ENDLOOP.

  MODIFY ZTPP_HOLD_CAR FROM TABLE IT_HOLD.
  IF SY-SUBRC = 0.
    MESSAGE S000 WITH TEXT-003.
    COMMIT WORK.
  ELSE.
    MESSAGE S000 WITH TEXT-004.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " check_holding_car
*&---------------------------------------------------------------------*
*&      Form  GET_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_TEMP_SEQ_DATE  text
*----------------------------------------------------------------------*
FORM GET_WEEK USING    P_SEQ_DATE L_WEEK C_WEEK P_TABIX
              CHANGING A_WEEK B_WEEK.

  DATA: L_DATE  TYPE D,
        WEEK LIKE SCAL-WEEK.

  L_DATE =  P_SEQ_DATE.

  CALL FUNCTION 'DATE_GET_WEEK'
       EXPORTING
            DATE         = L_DATE
       IMPORTING
            WEEK         = WEEK
       EXCEPTIONS
            DATE_INVALID = 1
            OTHERS       = 2.
  IF P_TABIX <> 1.
    IF C_WEEK <> WEEK.
      A_WEEK = L_WEEK + 1.
      B_WEEK = WEEK.
    ELSE.
      A_WEEK = L_WEEK.
      B_WEEK = WEEK.
    ENDIF.
  ELSE.
    A_WEEK = L_WEEK.
    B_WEEK = WEEK.
  ENDIF.
ENDFORM.                    " GET_WEEK
*&---------------------------------------------------------------------*
*&      Form  GET_MRP_LAST_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_Z_DATE_COUNT  text
*----------------------------------------------------------------------*
FORM GET_MRP_LAST_WEEK CHANGING P_WEEK.
  DATA: L_WK   LIKE SCAL-WEEK.
  DATA: L_WK1  TYPE SCAL-WEEK.
  DATA: L_WKD TYPE I.
  DATA: Z_MAX_DATE LIKE SY-DATUM.

  SELECT MAX( SQDT ) INTO Z_MAX_DATE
   FROM ZTPP_PMT07JB_A
    WHERE GUBB = 'A'.
  IF Z_MAX_DATE LT SY-DATUM .
*     message i000 with 'The last MRP date is past'.
    EXIT.
  ENDIF .
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      DATE               = SY-DATUM
    IMPORTING
      WEEK               = L_WK
*    EXCEPTIONS
*      DATE_INVALID       = 1
*      OTHERS             = 2
            .
  CALL FUNCTION 'DATE_GET_WEEK'
   EXPORTING
     DATE               = Z_MAX_DATE
   IMPORTING
     WEEK               = L_WK1
*    EXCEPTIONS
*      DATE_INVALID       = 1
*      OTHERS             = 2
           .

  L_WKD  = L_WK1 - L_WK.

  IF L_WK1(04) GT L_WK(04).
    L_WKD = L_WKD + 52 - 100.
  ENDIF.
  P_WEEK = L_WKD + 1.


ENDFORM.                    " GET_MRP_LAST_WEEK
*&---------------------------------------------------------------------*
*&      Form  GET_CHAR_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CHAR_ATINN.
  LOOP AT IT_ALC.
    SELECT SINGLE ATINN INTO IT_CHAR-ATINN
      FROM CABN
      WHERE ATNAM = IT_ALC-CHAR_ALC.
    IT_CHAR-ATNAM = IT_ALC-CHAR_ALC.
    APPEND IT_CHAR.
    CLEAR: IT_CHAR.
  ENDLOOP.
ENDFORM.                    " GET_CHAR_ATINN
