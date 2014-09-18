************************************************************************
* Program Name      : ZAPP903R_WIRE_DAY
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : Wire Parts Summary(Bucket: Daily , Horz.: 21 days)
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
* 03/03/2005 chris       UD1K914781   start date => system date
************************************************************************
REPORT  ZAPP903R_WIRE_DAY     MESSAGE-ID ZMPP.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ZTPP_COMMON_VALS,
        ZTPP_INPUT_PLAN,
        AUSP .

*----------------------------------------------------------------------
* Gloval Variables Definition
*----------------------------------------------------------------------
DATA: WA_UPH_B                TYPE ZVPP_LD-LRATE,
      WA_UPH_P                TYPE ZVPP_LD-LRATE,
      WA_UPH_T                TYPE ZVPP_LD-LRATE.

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

DATA: BEGIN OF IT_ALC         OCCURS 0.
        INCLUDE STRUCTURE     CUVTAB_VALC.
DATA:   MODEL(3)              TYPE C  ,
        CODE(3)               TYPE C  ,
        RP(2)                 TYPE N  ,
        TYPE_ALC              TYPE C  ,
*       char_alc              LIKE cabn-atnam,
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
        ALC_VALS1             LIKE ZTPP_WIRE_DAY-ALC_VALS1,
        ALC_VALS2             LIKE ZTPP_WIRE_DAY-ALC_VALS2,
        ALC_VALS3             LIKE ZTPP_WIRE_DAY-ALC_VALS3,
        ALC_VALS4             LIKE ZTPP_WIRE_DAY-ALC_VALS4,
        ALC_VALS5             LIKE ZTPP_WIRE_DAY-ALC_VALS5,
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

DATA: BEGIN OF IT_MASTER  OCCURS 0,
        SEQ               TYPE I  ,             " Sequence
        DATE              TYPE D  ,             " Date
        DAY               LIKE KAPA-TAGNR,      " Day
        SHIFT             LIKE KAPA-SCHNR,      " Shift
        TIME              TYPE KAPENDZT  ,      " Times for working
        UPH               TYPE ZVPP_LD-LRATE,   " UPH
      END OF IT_MASTER.

DATA: IT_PROD             LIKE TABLE OF IS_PROD        WITH HEADER LINE,
      IT_SEQ              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_D1               LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_BI               LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_WBS              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_PI               LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_PRJ              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_PBS              LIKE TABLE OF IT_SUM         WITH HEADER LINE,
      IT_DISP_D1          LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE,
      IT_DISP_SEQ         LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE,
      IT_DISP_BI          LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE,
      IT_DISP_PI          LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE,
      IT_DISP_PRJ         LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE,
      IT_DISP_WBS         LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE,
      IT_DISP_PBS         LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE,
      IT_DISP             LIKE TABLE OF ZTPP_WIRE_DAY WITH HEADER LINE.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: WA_DISP             LIKE IT_DISP                             ,
      WA_DATA             LIKE IT_DATA                             ,
      WA_WDATE            LIKE ZTPP_DAY_SUM-WDATE                  ,
      WA_KALID            LIKE KAKO-KALID                          ,
      WA_REPID            LIKE SY-REPID                            ,
      WA_UZEIT            LIKE SY-UZEIT                            ,
      WA_INDEX            LIKE SY-TABIX                            ,
      WA_MODEL(3)         TYPE C                                   ,
      WA_ERROR            TYPE C                                   ,
      WA_FLAG             TYPE C                                   ,
      WA_COUNT            TYPE N                                   ,
      WA_DAYS             TYPE I                                   .


*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <WA_VALS>      TYPE ANY,
               <WA_DFIELD>    TYPE ANY.


*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: C_NAME(30)              VALUE 'WIRE'               ,
      C_JOBS(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      C_KEY1(18)              VALUE 'SEQ_SUM02' .


*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
*ARAMETERS: p_model(3)    TYPE c        ,
PARAMETERS: P_DATES       TYPE D        OBLIGATORY,
            P_TEST        TYPE C                  ,
            P_MITU        TYPE C                  ,
            P_WBS(2)      TYPE N        OBLIGATORY,
            P_PRJ(2)      TYPE N        OBLIGATORY,
            P_FWO        TYPE C        .
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

  IF P_WBS IS INITIAL.
    SELECT SINGLE ITEM4  INTO L_VALS
      FROM ZTPP_COMMON_VALS
     WHERE JOBS = WA_REPID
       AND KEY2 = 'WBS'   .
    P_WBS = L_VALS        .   CLEAR: L_VALS.
  ENDIF.

  IF P_PRJ IS INITIAL.
    SELECT SINGLE ITEM4  INTO L_VALS
      FROM ZTPP_COMMON_VALS
     WHERE JOBS = WA_REPID
       AND KEY2 = 'PRJ'   .
    P_PRJ = L_VALS        .   CLEAR: L_VALS.
  ENDIF.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM CLEAR_VARIABLE               .
  PERFORM SET_INFORMATION              .
  PERFORM READ_INPUTPLAN               .
  PERFORM READ_ALC_MODEL               .
  PERFORM CREATE_SUMMARY               .
  PERFORM INSERT_FIELD_VALS            .
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
         WA_DATA,  WA_UZEIT, WA_INDEX, WA_DAYS .
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

  IF P_MITU = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_INPUT_PLAN
    WHERE NOT ( MITU EQ 'X' OR MITU EQ 'Y' ).
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_INPUT_PLAN .
  ENDIF.

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
    DELETE FROM ZTPP_WIRE_DAY  CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
    LEAVE PROGRAM .
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PROD
    FROM ZTPP_DAY_SUM
   WHERE WDATE = WA_WDATE AND
         DEALER NE 'XX'   AND    " changed by chris on 02/21/2005
         DEALER NE 'XY'.         " changed by chris on 02/21/2005
  " Previous Daily Producion Report... (it_prod)
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

  IF NOT P_FWO IS INITIAL.
    LOOP AT IT_DATA.
      IF IT_DATA-WORK_ORDER+0(1) = 'F'.
*        CONCATENATE it_data-work_order it_data-extc it_data-intc
*               INTO l_fwocl.
*        SELECT SINGLE o_worder INTO l_worder
*          FROM ztpp_fwo
*          WHERE worder = l_fwocl.
*        IF sy-subrc = 0.
*          it_data-work_order = l_worder.
*          MODIFY it_data.
*        ELSE.
        L_ORDR = IT_DATA-WORK_ORDER+0(9).
        L_DIST = IT_DATA-WORK_ORDER+9(5).

        SELECT SINGLE MOYE INTO L_YEAR
          FROM ZTPP_PMT07JB_A
         WHERE ORDR = L_ORDR
           AND DIST = L_DIST
           AND EXTC = IT_DATA-EXTC
           AND INTC = IT_DATA-INTC.
** Changed by Furong on 10/10/07 for EBOM
*          CONCATENATE l_year l_dist it_data-mi INTO l_fsc.
*          CONCATENATE l_fsc it_data-ocnn INTO l_fsc
*                  SEPARATED BY space.
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
         WHERE FSC = L_FSC
           AND EXTC = IT_DATA-EXTC
           AND INTC = IT_DATA-INTC
           AND VERSION = IT_DATA-VERS.

        IF SY-SUBRC = 0.
          CONCATENATE L_WORDER L_DIST INTO L_WORDER.
          IT_DATA-WORK_ORDER = L_WORDER.
        ELSE.
          CLEAR: IT_DATA-WORK_ORDER.
        ENDIF.
        MODIFY IT_DATA.
*        ENDIF.
        CLEAR: L_FWOCL, L_WORDER.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT IT_DATA BY RSNUM SERIAL .
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  READ_ALC_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MODEL  text
*----------------------------------------------------------------------*
FORM READ_ALC_MODEL  .
  DATA: L_VTINT              LIKE CUVTAB-VTINT,
        L_VTNAM              LIKE CUVTAB-VTNAM.

  CLEAR: IT_MODEL, IT_MODEL[].
  PERFORM GET_MODELS .

  LOOP AT IT_MODEL   .
    WA_MODEL = IT_MODEL-MODL .
    CONCATENATE WA_MODEL '_MIX'            INTO  L_VTNAM    .
    PERFORM GET_CUVTAB                     USING L_VTNAM  L_VTINT.
    PERFORM GET_ALC                        USING L_VTINT.
    " Set the Model Code...
    LOOP AT IT_ALC  WHERE MODEL = SPACE.
      IT_ALC-MODEL = WA_MODEL .
      IT_ALC-TYPE_ALC = IT_ALC-VALC+6(1).
      IT_ALC-RP       = '06'            .
      MODIFY IT_ALC.
    ENDLOOP.
  ENDLOOP.

** On 08/05/13 by Furong
*  SORT IT_ALC BY MODEL VALC .
  SORT IT_ALC BY MODEL ATINN.
** End on 08/05/13
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
  PERFORM CALC_ALC_WBS        .
  PERFORM CALC_ALC_PI         .
  PERFORM CALC_ALC_PRJ        .
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
*       l_loops              TYPE i ,
        L_DAYS               TYPE I ,
        L_POS                TYPE I ,
        L_CHK                TYPE P DECIMALS 3,
        L_TABIX              LIKE SY-TABIX,
        L_START              LIKE SY-TABIX,
        L_END                LIKE SY-TABIX,
        L_INDEX              LIKE SY-TABIX.

*  CONCATENATE 'RP' it_alc-rp  INTO  l_name .
*  DESCRIBE TABLE it_data      LINES l_max  .
*  CASE it_alc-rp.
*    WHEN '01'.             " REPORTING POINT 01
*      READ TABLE it_data WITH KEY rp01 = space .
*    WHEN '02'.             " REPORTING POINT 02
*      READ TABLE it_data WITH KEY rp02 = space .
*    WHEN '03'.             " REPORTING POINT 03
*      READ TABLE it_data WITH KEY rp03 = space .
*    WHEN '04'.             " REPORTING POINT 04
*      READ TABLE it_data WITH KEY rp04 = space .
*    WHEN '05'.             " REPORTING POINT 05
*      READ TABLE it_data WITH KEY rp05 = space .
*    WHEN '06'.             " REPORTING POINT 06
  READ TABLE IT_DATA WITH KEY RP06 = SPACE .
*    WHEN '07'.             " REPORTING POINT 07
*      READ TABLE it_data WITH KEY rp07 = space .
*    WHEN '08'.             " REPORTING POINT 08
*      READ TABLE it_data WITH KEY rp08 = space .
*    WHEN '09'.             " REPORTING POINT 09
*      READ TABLE it_data WITH KEY rp09 = space .
*    WHEN '10'.             " REPORTING POINT 10
*      READ TABLE it_data WITH KEY rp10 = space .
*    WHEN '11'.             " REPORTING POINT 11
*      READ TABLE it_data WITH KEY rp11 = space .
*    WHEN '12'.             " REPORTING POINT 12
*      READ TABLE it_data WITH KEY rp12 = space .
*    WHEN '13'.             " REPORTING POINT 13
*      READ TABLE it_data WITH KEY rp13 = space .
*    WHEN '14'.             " REPORTING POINT 14
*      READ TABLE it_data WITH KEY rp14 = space .
*    WHEN '15'.             " REPORTING POINT 15
*      READ TABLE it_data WITH KEY rp15 = space .
*    WHEN '16'.             " REPORTING POINT 16
*      READ TABLE it_data WITH KEY rp16 = space .
*    WHEN '17'.             " REPORTING POINT 17
*      READ TABLE it_data WITH KEY rp17 = space .
*    WHEN '18'.             " REPORTING POINT 18
*      READ TABLE it_data WITH KEY rp18 = space .
*  ENDCASE.
  L_INDEX = SY-TABIX .
  IF L_INDEX > 1.
    L_INDEX = L_INDEX - 1 .
    DELETE IT_DATA FROM 1 TO L_INDEX.
  ENDIF.
  L_INDEX = 1.

  " 1 Days * 21 Times
  DESCRIBE TABLE IT_DATA      LINES L_MAX  .
  SORT IT_MASTER BY SEQ .     CLEAR: L_TABIX.
  DO 21 TIMES.
    L_TABIX = L_TABIX + 1 .
    READ TABLE IT_MASTER WITH KEY SEQ = L_TABIX.
    IF IT_MASTER-UPH = 0.
      L_DAYS = L_DAYS + 1 .
    ELSE.
      L_CHK = IT_MASTER-TIME / 3600 .
      IF L_FLAG = 'X'.   EXIT.   ENDIF.
      L_POS = L_POS +  IT_MASTER-UPH * L_CHK  .
      IF L_POS >= L_MAX.
        WA_INDEX = L_MAX - L_INDEX.
        WA_DAYS  = L_DAYS .
        L_POS = L_MAX.
        L_FLAG = 'X' .
      ENDIF.
      L_DAYS  = L_DAYS  + 1 .
      LOOP AT IT_DATA FROM L_INDEX TO L_POS.
        CLEAR: IT_SUM.
        IT_SUM-DAYS       = L_DAYS             .
        IT_SUM-VM_MODEL   = IT_DATA-MODL       .
        IT_SUM-VM_BODYSER = IT_DATA-BODY_SER   .
        IT_SUM-WORDER     = IT_DATA-WORK_ORDER .
        IT_SUM-MITU       = IT_DATA-MITU       .
        IT_SUM-EXTC       = IT_DATA-EXTC       .
        IT_SUM-INTC       = IT_DATA-INTC       .
        IT_SUM-STATUS     = IT_DATA-STATUS     .
        IT_SUM-RP         = '06'               .
        IT_SUM-CNT        = 1                  .
*       it_sum-knnam      = it_alc-knnam       .
*       CONCATENATE it_alc-type_alc it_alc-code INTO it_sum-code .
        PERFORM UPDATE_INPUT_PLAN  USING L_DAYS.
        APPEND IT_SUM.
      ENDLOOP.
    ENDIF.
    L_INDEX = L_POS + 1 .
  ENDDO.
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
        L_MITUCNT               TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_DAYS(2)               TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,              " ALC CODE
        L_VALS2(5)              TYPE C  ,              " ALC CODE
        L_VALS3(5)              TYPE C  ,              " ALC CODE
        L_VALS4(5)              TYPE C  ,              " ALC CODE
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP.
  DESCRIBE TABLE IT_SUM LINES L_LINE.
  CHECK L_LINE > 0 .
  SORT IT_SUM BY DAYS       VM_MODEL   ALC_VALS1   ALC_VALS2
                 ALC_VALS3  ALC_VALS4  ALC_VALS5          .
  READ TABLE IT_SUM INDEX 1.
  L_DAYS   = IT_SUM-DAYS    .
*  l_code   = it_sum-code    .
  L_VALS1  = IT_SUM-ALC_VALS1.
  L_VALS2  = IT_SUM-ALC_VALS2.
  L_VALS3  = IT_SUM-ALC_VALS3.
  L_VALS4  = IT_SUM-ALC_VALS4.
  L_VALS5  = IT_SUM-ALC_VALS5.
  L_MODEL  = IT_SUM-VM_MODEL.

  CONCATENATE 'IT_DISP-D'  L_DAYS   INTO L_NAME.
  ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
  CLEAR: <WA_DFIELD>.

  LOOP AT IT_SUM.
    IF L_DAYS  = IT_SUM-DAYS        AND L_VALS1  = IT_SUM-ALC_VALS1 AND
        L_VALS2  = IT_SUM-ALC_VALS2 AND L_VALS3  = IT_SUM-ALC_VALS3 AND
        L_VALS4  = IT_SUM-ALC_VALS4 AND L_VALS5  = IT_SUM-ALC_VALS5 AND
        L_MODEL  = IT_SUM-VM_MODEL  .
      <WA_DFIELD> = <WA_DFIELD> + IT_SUM-CNT .
      L_MITUCNT   = L_MITUCNT   + IT_SUM-MITUCNT .
      CONTINUE.
    ELSE.
      IT_DISP-MODEL    = L_MODEL  .
*      it_disp-alc_code = l_code   .
      IT_DISP-ALC_VALS1 = L_VALS1  .
      IT_DISP-ALC_VALS2 = L_VALS2  .
      IT_DISP-ALC_VALS3 = L_VALS3  .
      IT_DISP-ALC_VALS4 = L_VALS4  .
      IT_DISP-ALC_VALS5 = L_VALS5  .
      IT_DISP-RP       = IT_ALC-RP.
      IT_DISP-MITU     = L_MITUCNT.
      APPEND IT_DISP.    CLEAR: IT_DISP.
      L_DAYS     = IT_SUM-DAYS     .
*      l_code     = it_sum-code     .
      L_VALS1    = IT_SUM-ALC_VALS1.
      L_VALS2    = IT_SUM-ALC_VALS2.
      L_VALS3    = IT_SUM-ALC_VALS3.
      L_VALS4    = IT_SUM-ALC_VALS4.
      L_VALS5    = IT_SUM-ALC_VALS5.
      L_MODEL    = IT_SUM-VM_MODEL .
      L_MITUCNT   = IT_SUM-MITUCNT .
      CONCATENATE 'IT_DISP-D'  L_DAYS   INTO L_NAME.
      ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
      <WA_DFIELD>  = IT_SUM-CNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM  LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP-MODEL    = L_MODEL  .
*    it_disp-alc_code = l_code   .
    IT_DISP-ALC_VALS1 = L_VALS1  .
    IT_DISP-ALC_VALS2 = L_VALS2  .
    IT_DISP-ALC_VALS3 = L_VALS3  .
    IT_DISP-ALC_VALS4 = L_VALS4  .
    IT_DISP-ALC_VALS5 = L_VALS5  .
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
        L_DAYS(2)               TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,              "
        L_VALS2(5)              TYPE C  ,              "
        L_VALS3(5)              TYPE C  ,              "
        L_VALS4(5)              TYPE C  ,              "
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_D1  .
  SORT IT_D1       BY VM_MODEL   DAYS       ALC_VALS1  ALC_VALS2
                      ALC_VALS3  ALC_VALS4  ALC_VALS5.
  READ TABLE IT_D1 INDEX 1.  CLEAR: L_COUNT.
  L_DAYS   = IT_D1-DAYS    .
  L_VALS1  = IT_D1-ALC_VALS1.
  L_VALS2  = IT_D1-ALC_VALS2.
  L_VALS3  = IT_D1-ALC_VALS3.
  L_VALS4  = IT_D1-ALC_VALS4.
  L_VALS5  = IT_D1-ALC_VALS5.
  L_MODEL  = IT_D1-VM_MODEL.

  LOOP AT IT_D1.
    IF L_VALS1  = IT_D1-ALC_VALS1 AND  L_MODEL  = IT_D1-VM_MODEL  AND
       L_VALS2  = IT_D1-ALC_VALS2 AND  L_VALS3  = IT_D1-ALC_VALS3 AND
       L_VALS4  = IT_D1-ALC_VALS4 AND  L_VALS5  = IT_D1-ALC_VALS5 .
**      l_hours = it_sum_PROD-hours.
      L_COUNT  = L_COUNT + IT_D1-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_D1-MODEL     = L_MODEL  .
      IT_DISP_D1-ALC_VALS1 = L_VALS1(5).
      IT_DISP_D1-ALC_VALS2 = L_VALS2(5).
      IT_DISP_D1-ALC_VALS3 = L_VALS3(5).
      IT_DISP_D1-ALC_VALS4 = L_VALS4(5).
      IT_DISP_D1-ALC_VALS5 = L_VALS5(5).
      IT_DISP_D1-RP        = IT_ALC-RP.
      IT_DISP_D1-D_1       = L_COUNT  .
*     MOVE-CORRESPONDING IT_SUM_PROD  TO it_disp_D1  .
      APPEND IT_DISP_D1  .    CLEAR: IT_DISP_D1  .
*     l_hours = it_sum_PROD-hours      .
      L_VALS1  = IT_D1-ALC_VALS1.
      L_VALS2  = IT_D1-ALC_VALS2.
      L_VALS3  = IT_D1-ALC_VALS3.
      L_VALS4  = IT_D1-ALC_VALS4.
      L_VALS5  = IT_D1-ALC_VALS5.
      L_MODEL = IT_D1-VM_MODEL   .
      L_COUNT = IT_D1-CNT        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_D1  LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_D1-MODEL    = L_MODEL  .
    IT_DISP_D1-ALC_VALS1 = L_VALS1(5).
    IT_DISP_D1-ALC_VALS2 = L_VALS2(5).
    IT_DISP_D1-ALC_VALS3 = L_VALS3(5).
    IT_DISP_D1-ALC_VALS4 = L_VALS4(5).
    IT_DISP_D1-ALC_VALS5 = L_VALS5(5).
    IT_DISP_D1-RP        = IT_ALC-RP.
    IT_DISP_D1-D_1       = L_COUNT  .
    APPEND IT_DISP_D1  .
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
        L_DAYS(2)               TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,              " ALC CODE
        L_VALS2(5)              TYPE C  ,              " ALC CODE
        L_VALS3(5)              TYPE C  ,              " ALC CODE
        L_VALS4(5)              TYPE C  ,              " ALC CODE
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_SEQ .
  SORT IT_SEQ  BY VM_MODEL      ALC_VALS1  ALC_VALS2
                  ALC_VALS3  ALC_VALS4  ALC_VALS5.
  READ TABLE IT_SEQ  INDEX 1.  CLEAR: L_COUNT.
* l_days   = it_sum_PROD-days    .
  L_VALS1  = IT_SEQ-ALC_VALS1.
  L_VALS2  = IT_SEQ-ALC_VALS2.
  L_VALS3  = IT_SEQ-ALC_VALS3.
  L_VALS4  = IT_SEQ-ALC_VALS4.
  L_VALS5  = IT_SEQ-ALC_VALS5.
  L_MODEL  = IT_SEQ-VM_MODEL .

  LOOP AT IT_SEQ .
    IF L_VALS1  = IT_SEQ-ALC_VALS1 AND  L_MODEL  = IT_SEQ-VM_MODEL  AND
       L_VALS2  = IT_SEQ-ALC_VALS2 AND  L_VALS3  = IT_SEQ-ALC_VALS3 AND
       L_VALS4  = IT_SEQ-ALC_VALS4 AND  L_VALS5  = IT_SEQ-ALC_VALS5 .
      L_COUNT     = L_COUNT     + IT_SEQ-CNT .
      L_MITUCNT   = L_MITUCNT   + IT_SEQ-MITUCNT .
      CONTINUE.
    ELSE.
      IT_DISP_SEQ-MODEL     = L_MODEL  .
      IT_DISP_SEQ-ALC_VALS1 = L_VALS1(5).
      IT_DISP_SEQ-ALC_VALS2 = L_VALS2(5).
      IT_DISP_SEQ-ALC_VALS3 = L_VALS3(5).
      IT_DISP_SEQ-ALC_VALS4 = L_VALS4(5).
      IT_DISP_SEQ-ALC_VALS5 = L_VALS5(5).
      IT_DISP_SEQ-RP        = IT_ALC-RP.
      IT_DISP_SEQ-D_1       = L_COUNT  .
      IT_DISP_SEQ-MITU      = L_MITUCNT.
      APPEND IT_DISP_SEQ .    CLEAR: IT_DISP_SEQ .
*     l_hours   = it_SEQ-hours      .
*     l_code    = it_seq-code      .
      L_VALS1   = IT_SEQ-ALC_VALS1     .
      L_VALS2   = IT_SEQ-ALC_VALS2     .
      L_VALS3   = IT_SEQ-ALC_VALS3     .
      L_VALS4   = IT_SEQ-ALC_VALS4     .
      L_VALS5   = IT_SEQ-ALC_VALS5     .
      L_MODEL   = IT_SEQ-VM_MODEL  .
      L_COUNT   = IT_SEQ-CNT        .
      L_MITUCNT = IT_SEQ-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SEQ   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_SEQ-MODEL     = L_MODEL  .
    IT_DISP_SEQ-ALC_VALS1 = L_VALS1(5).
    IT_DISP_SEQ-ALC_VALS2 = L_VALS2(5).
    IT_DISP_SEQ-ALC_VALS3 = L_VALS3(5).
    IT_DISP_SEQ-ALC_VALS4 = L_VALS4(5).
    IT_DISP_SEQ-ALC_VALS5 = L_VALS5(5).
*    it_disp_seq-alc_code = l_code   .
*    it_disp_seq-alc_vals = l_vals   .
    IT_DISP_SEQ-RP        = IT_ALC-RP.
    IT_DISP_SEQ-D_1       = L_COUNT  .
    IT_DISP_SEQ-MITU      = L_MITUCNT.
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
        L_MITUCNT               TYPE I  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,
        L_VALS2(5)              TYPE C  ,
        L_VALS3(5)              TYPE C  ,
        L_VALS4(5)              TYPE C  ,
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_BI .
  SORT IT_BI   BY VM_MODEL  ALC_VALS1  ALC_VALS2
                  ALC_VALS3 ALC_VALS4  ALC_VALS5.
  READ TABLE IT_BI   INDEX 1.  CLEAR: L_COUNT.
*  l_code   = it_bi-code    .
  L_VALS1   = IT_BI-ALC_VALS1   .
  L_VALS2   = IT_BI-ALC_VALS2   .
  L_VALS3   = IT_BI-ALC_VALS3   .
  L_VALS4   = IT_BI-ALC_VALS4   .
  L_VALS5   = IT_BI-ALC_VALS5   .
  L_MODEL  = IT_BI-VM_MODEL.

  LOOP AT IT_BI  .
    IF L_MODEL  = IT_BI-VM_MODEL  AND L_VALS1 = IT_BI-ALC_VALS1  AND
       L_VALS2 = IT_BI-ALC_VALS2  AND L_VALS3 = IT_BI-ALC_VALS3  AND
       L_VALS4 = IT_BI-ALC_VALS4  AND L_VALS5 = IT_BI-ALC_VALS5  .
      L_COUNT  = L_COUNT + IT_BI-CNT .
      L_MITUCNT = L_MITUCNT   + IT_BI-MITUCNT .
      CONTINUE.
    ELSE.
      IT_DISP_BI-MODEL     = L_MODEL  .
      IT_DISP_BI-ALC_VALS1 = L_VALS1(5).
      IT_DISP_BI-ALC_VALS2 = L_VALS2(5).
      IT_DISP_BI-ALC_VALS3 = L_VALS3(5).
      IT_DISP_BI-ALC_VALS4 = L_VALS4(5).
      IT_DISP_BI-ALC_VALS5 = L_VALS5(5).
      IT_DISP_BI-RP        = IT_ALC-RP.
      IT_DISP_BI-D_1       = L_COUNT  .
      IT_DISP_BI-MITU      = L_MITUCNT.
      APPEND IT_DISP_BI .    CLEAR: IT_DISP_BI .
*      l_code  = it_bi-code       .
*      l_vals  = it_bi-vals       .
      L_VALS1 = IT_BI-ALC_VALS1       .
      L_VALS2 = IT_BI-ALC_VALS2       .
      L_VALS3 = IT_BI-ALC_VALS3       .
      L_VALS4 = IT_BI-ALC_VALS4       .
      L_VALS5 = IT_BI-ALC_VALS5       .
      L_MODEL = IT_BI-VM_MODEL   .
      L_COUNT = IT_BI-CNT        .
      L_MITUCNT = IT_BI-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_BI    LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_BI-MODEL     = L_MODEL  .
    IT_DISP_BI-ALC_VALS1 = L_VALS1(5).
    IT_DISP_BI-ALC_VALS2 = L_VALS2(5).
    IT_DISP_BI-ALC_VALS3 = L_VALS3(5).
    IT_DISP_BI-ALC_VALS4 = L_VALS4(5).
    IT_DISP_BI-ALC_VALS5 = L_VALS5(5).
*    it_disp_bi-alc_code = l_code   .
*    it_disp_bi-alc_vals = l_vals   .
    IT_DISP_BI-RP        = IT_ALC-RP.
    IT_DISP_BI-D_1       = L_COUNT  .
    IT_DISP_BI-MITU      = L_MITUCNT.
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
        L_MITUCNT               TYPE I  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,              " ALC CODE
        L_VALS2(5)              TYPE C  ,              " ALC CODE
        L_VALS3(5)              TYPE C  ,              " ALC CODE
        L_VALS4(5)              TYPE C  ,              " ALC CODE
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_WBS .
  SORT IT_WBS  BY VM_MODEL   ALC_VALS1   ALC_VALS2
                  ALC_VALS3  ALC_VALS4   ALC_VALS5.
  READ TABLE IT_WBS  INDEX 1.  CLEAR: L_COUNT.
* l_hours  = it_WBS-hours   .
  L_VALS1  = IT_WBS-ALC_VALS1  .
  L_VALS2  = IT_WBS-ALC_VALS2  .
  L_VALS3  = IT_WBS-ALC_VALS3  .
  L_VALS4  = IT_WBS-ALC_VALS4  .
  L_VALS5  = IT_WBS-ALC_VALS5  .
  L_MODEL  = IT_WBS-VM_MODEL.

  LOOP AT IT_WBS .
    IF L_VALS1 = IT_WBS-ALC_VALS1 AND L_VALS2 = IT_WBS-ALC_VALS2  AND
       L_VALS3 = IT_WBS-ALC_VALS3 AND L_VALS4 = IT_WBS-ALC_VALS4  AND
       L_VALS5 = IT_WBS-ALC_VALS5 AND L_MODEL  = IT_WBS-VM_MODEL.
      L_COUNT  = L_COUNT + IT_WBS-CNT .
      L_MITUCNT   = L_MITUCNT   + IT_WBS-MITUCNT .
      CONTINUE.
    ELSE.
      IT_DISP_WBS-MODEL    = L_MODEL  .
      IT_DISP_WBS-ALC_VALS1 = L_VALS1(5).
      IT_DISP_WBS-ALC_VALS2 = L_VALS2(5).
      IT_DISP_WBS-ALC_VALS3 = L_VALS3(5).
      IT_DISP_WBS-ALC_VALS4 = L_VALS4(5).
      IT_DISP_WBS-ALC_VALS5 = L_VALS5(5).
      IT_DISP_WBS-RP       = IT_ALC-RP.
      IT_DISP_WBS-D_1      = L_COUNT    .
      IT_DISP_WBS-MITU     = L_MITUCNT  .
*     MOVE-CORRESPONDING IT_WBS TO it_disp_wbs .
      APPEND IT_DISP_WBS .    CLEAR: IT_DISP_WBS .
*      l_code  = it_wbs-code       .
      L_VALS1 = IT_WBS-ALC_VALS1      .
      L_VALS2 = IT_WBS-ALC_VALS2      .
      L_VALS3 = IT_WBS-ALC_VALS3      .
      L_VALS4 = IT_WBS-ALC_VALS4      .
      L_VALS5 = IT_WBS-ALC_VALS5      .
      L_MODEL = IT_WBS-VM_MODEL   .
      L_COUNT = IT_WBS-CNT        .
      L_MITUCNT = IT_WBS-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_WBS   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_WBS-MODEL    = L_MODEL  .
    IT_DISP_WBS-ALC_VALS1 = L_VALS1(5).
    IT_DISP_WBS-ALC_VALS2 = L_VALS2(5).
    IT_DISP_WBS-ALC_VALS3 = L_VALS3(5).
    IT_DISP_WBS-ALC_VALS4 = L_VALS4(5).
    IT_DISP_WBS-ALC_VALS5 = L_VALS5(5).
*   it_disp_wbs-alc_code = l_code   .
*   it_disp_wbs-alc_vals = l_vals   .
    IT_DISP_WBS-RP       = IT_ALC-RP.
    IT_DISP_WBS-D_1      = L_COUNT  .
    IT_DISP_WBS-MITU     = L_MITUCNT  .
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
        L_MITUCNT               TYPE I  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,
        L_VALS2(5)              TYPE C  ,
        L_VALS3(5)              TYPE C  ,
        L_VALS4(5)              TYPE C  ,
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_PI .
  SORT IT_PI   BY VM_MODEL   ALC_VALS1  ALC_VALS2
                  ALC_VALS3  ALC_VALS4  ALC_VALS5.
  READ TABLE IT_PI   INDEX 1.  CLEAR: L_COUNT.
*  l_code   = it_pi-code    .
  L_VALS1  = IT_PI-ALC_VALS1.
  L_VALS2  = IT_PI-ALC_VALS2.
  L_VALS3  = IT_PI-ALC_VALS3.
  L_VALS4  = IT_PI-ALC_VALS4.
  L_VALS5  = IT_PI-ALC_VALS5.
  L_MODEL  = IT_PI-VM_MODEL.

  LOOP AT IT_PI  .
    IF L_VALS1 = IT_PI-ALC_VALS1 AND  L_VALS2  = IT_PI-ALC_VALS2  AND
       L_VALS3 = IT_PI-ALC_VALS3 AND  L_VALS4  = IT_PI-ALC_VALS4  AND
       L_VALS5 = IT_PI-ALC_VALS5 AND  L_MODEL  = IT_PI-VM_MODEL.
      L_COUNT  = L_COUNT + IT_PI-CNT .
      L_MITUCNT = L_MITUCNT   + IT_PI-MITUCNT .
      CONTINUE.
    ELSE.
      IT_DISP_PI-MODEL    = L_MODEL  .
      IT_DISP_PI-ALC_VALS1 = L_VALS1(5).
      IT_DISP_PI-ALC_VALS2 = L_VALS2(5).
      IT_DISP_PI-ALC_VALS3 = L_VALS3(5).
      IT_DISP_PI-ALC_VALS4 = L_VALS4(5).
      IT_DISP_PI-ALC_VALS5 = L_VALS5(5).
      IT_DISP_PI-RP        = IT_ALC-RP.
      IT_DISP_PI-D_1       = L_COUNT  .
      IT_DISP_PI-MITU      = L_MITUCNT.
*      MOVE-CORRESPONDING IT_PI  TO it_disp_PI  .
      APPEND IT_DISP_PI .    CLEAR: IT_DISP_PI .
*     l_hours = it_PI-Hours      .
*      l_code  = it_pi-code       .
      L_VALS1 = IT_PI-ALC_VALS1       .
      L_VALS2 = IT_PI-ALC_VALS2       .
      L_VALS3 = IT_PI-ALC_VALS3       .
      L_VALS4 = IT_PI-ALC_VALS4       .
      L_VALS5 = IT_PI-ALC_VALS5       .
      L_MODEL = IT_PI-VM_MODEL   .
      L_COUNT = IT_PI-CNT        .
      L_MITUCNT = IT_PI-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PI    LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_PI-MODEL    = L_MODEL  .
    IT_DISP_PI-ALC_VALS1 = L_VALS1(5).
    IT_DISP_PI-ALC_VALS2 = L_VALS2(5).
    IT_DISP_PI-ALC_VALS3 = L_VALS3(5).
    IT_DISP_PI-ALC_VALS4 = L_VALS4(5).
    IT_DISP_PI-ALC_VALS5 = L_VALS5(5).
*   it_disp_pi-alc_code = l_code   .
*   it_disp_pi-alc_vals = l_vals   .
    IT_DISP_PI-RP       = IT_ALC-RP.
    IT_DISP_PI-D_1      = L_COUNT  .
    IT_DISP_PI-MITU      = L_MITUCNT.
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
        L_MITUCNT               TYPE I  ,
        L_MODEL(3)              TYPE C  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,              "
        L_VALS2(5)              TYPE C  ,              "
        L_VALS3(5)              TYPE C  ,              "
        L_VALS4(5)              TYPE C  ,              "
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_PRJ .
  SORT IT_PRJ BY VM_MODEL   ALC_VALS1  ALC_VALS2
                 ALC_VALS3  ALC_VALS4  ALC_VALS5.
  READ TABLE IT_PRJ INDEX 1.  CLEAR: L_COUNT.
  L_VALS1  = IT_PRJ-ALC_VALS1 .
  L_VALS2  = IT_PRJ-ALC_VALS2 .
  L_VALS3  = IT_PRJ-ALC_VALS3 .
  L_VALS4  = IT_PRJ-ALC_VALS4 .
  L_VALS5  = IT_PRJ-ALC_VALS5 .
  L_MODEL  = IT_PRJ-VM_MODEL.

  LOOP AT IT_PRJ .
    IF L_VALS1 = IT_PRJ-ALC_VALS1  AND L_VALS2  = IT_PRJ-ALC_VALS2  AND
       L_VALS3 = IT_PRJ-ALC_VALS3  AND L_VALS4  = IT_PRJ-ALC_VALS4  AND
       L_VALS5 = IT_PRJ-ALC_VALS5  AND L_MODEL  = IT_PRJ-VM_MODEL.
      L_MITUCNT   = L_MITUCNT   + IT_PRJ-MITUCNT .
      L_COUNT  = L_COUNT + IT_PRJ-CNT .
      CONTINUE.
    ELSE.
      IT_DISP_PRJ-MODEL    = L_MODEL  .
      IT_DISP_PRJ-ALC_VALS1 = L_VALS1(5).
      IT_DISP_PRJ-ALC_VALS2 = L_VALS2(5).
      IT_DISP_PRJ-ALC_VALS3 = L_VALS3(5).
      IT_DISP_PRJ-ALC_VALS4 = L_VALS4(5).
      IT_DISP_PRJ-ALC_VALS5 = L_VALS5(5).
      IT_DISP_PRJ-RP       = IT_ALC-RP.
      IT_DISP_PRJ-D_1      = L_COUNT   .
      IT_DISP_PRJ-MITU      = L_MITUCNT.
*     MOVE-CORRESPONDING IT_PRJ TO it_disp_PRJ .
      APPEND IT_DISP_PRJ.    CLEAR: IT_DISP_PRJ .
*     l_hours = it_PRJ-hours      .
*      l_code  = it_prj-code       .
      L_VALS1 = IT_PRJ-ALC_VALS1      .
      L_VALS2 = IT_PRJ-ALC_VALS2      .
      L_VALS3 = IT_PRJ-ALC_VALS3      .
      L_VALS4 = IT_PRJ-ALC_VALS4      .
      L_VALS5 = IT_PRJ-ALC_VALS5      .
      L_MODEL = IT_PRJ-VM_MODEL   .
      L_COUNT = IT_PRJ-CNT        .
      L_MITUCNT = IT_PRJ-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PRJ   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_PRJ-MODEL    = L_MODEL  .
    IT_DISP_PRJ-ALC_VALS1 = L_VALS1(5).
    IT_DISP_PRJ-ALC_VALS2 = L_VALS2(5).
    IT_DISP_PRJ-ALC_VALS3 = L_VALS3(5).
    IT_DISP_PRJ-ALC_VALS4 = L_VALS4(5).
    IT_DISP_PRJ-ALC_VALS5 = L_VALS5(5).
*   it_disp_prj-alc_code = l_code   .
*   it_disp_prj-alc_vals = l_vals   .
    IT_DISP_PRJ-RP       = IT_ALC-RP.
    IT_DISP_PRJ-D_1      = L_COUNT  .
    IT_DISP_PRJ-MITU      = L_MITUCNT.
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
        L_MITUCNT               TYPE I  ,
        L_HOURS(2)              TYPE N  ,
        L_CODE(4)               TYPE C  ,              " ALC CODE
        L_VALS1(5)              TYPE C  ,              "
        L_VALS2(5)              TYPE C  ,              "
        L_VALS3(5)              TYPE C  ,              "
        L_VALS4(5)              TYPE C  ,              "
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP_PBS .
  SORT IT_PBS  BY VM_MODEL   ALC_VALS1  ALC_VALS2
                  ALC_VALS3  ALC_VALS4  ALC_VALS5.
  READ TABLE IT_PBS  INDEX 1.  CLEAR: L_COUNT.
* l_hours  = it_PBS-hours   .
  L_VALS1 = IT_PBS-ALC_VALS1.
  L_VALS2 = IT_PBS-ALC_VALS2.
  L_VALS3 = IT_PBS-ALC_VALS3.
  L_VALS4 = IT_PBS-ALC_VALS4.
  L_VALS5 = IT_PBS-ALC_VALS5.
  L_MODEL = IT_PBS-VM_MODEL.

  LOOP AT IT_PBS .
    IF L_VALS1 = IT_PBS-ALC_VALS1  AND  L_VALS2 = IT_PBS-ALC_VALS2  AND
       L_VALS3 = IT_PBS-ALC_VALS3  AND  L_VALS4 = IT_PBS-ALC_VALS4  AND
       L_VALS5 = IT_PBS-ALC_VALS5  AND  L_MODEL = IT_PBS-VM_MODEL.
      L_COUNT  = L_COUNT + IT_PBS-CNT .
      L_MITUCNT = L_MITUCNT   + IT_PBS-MITUCNT .
      CONTINUE.
    ELSE.
      IT_DISP_PBS-MODEL    = L_MODEL  .
      IT_DISP_PBS-ALC_VALS1 = L_VALS1(5).
      IT_DISP_PBS-ALC_VALS2 = L_VALS2(5).
      IT_DISP_PBS-ALC_VALS3 = L_VALS3(5).
      IT_DISP_PBS-ALC_VALS4 = L_VALS4(5).
      IT_DISP_PBS-ALC_VALS5 = L_VALS5(5).
      IT_DISP_PBS-RP       = IT_ALC-RP.
      IT_DISP_PBS-D_1      =  L_COUNT  .
      IT_DISP_PBS-MITU      = L_MITUCNT.
*     MOVE-CORRESPONDING IT_PBS TO it_disp_PBS  .
      APPEND IT_DISP_PBS .    CLEAR: IT_DISP_PBS .
*      l_code  = it_pbs-code       .
      L_VALS1 = IT_PBS-ALC_VALS1       .
      L_VALS2 = IT_PBS-ALC_VALS2       .
      L_VALS3 = IT_PBS-ALC_VALS3       .
      L_VALS4 = IT_PBS-ALC_VALS4       .
      L_VALS5 = IT_PBS-ALC_VALS5       .
      L_MODEL = IT_PBS-VM_MODEL   .
      L_COUNT = IT_PBS-CNT        .
      L_MITUCNT = IT_PBS-MITUCNT .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PBS   LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_PBS-MODEL    = L_MODEL  .
    IT_DISP_PBS-ALC_VALS1 = L_VALS1(5).
    IT_DISP_PBS-ALC_VALS2 = L_VALS2(5).
    IT_DISP_PBS-ALC_VALS3 = L_VALS3(5).
    IT_DISP_PBS-ALC_VALS4 = L_VALS4(5).
    IT_DISP_PBS-ALC_VALS5 = L_VALS5(5).
    IT_DISP_PBS-RP       = IT_ALC-RP.
    IT_DISP_PBS-D_1      = L_COUNT  .
    IT_DISP_PBS-MITU      = L_MITUCNT.
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
  DATA: L_VALS            LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  CLEAR: L_VALS, L_VALS[].
  L_VALS-ATNAM = PA_CODE .   APPEND L_VALS.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PA_WORDER
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VALS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  READ TABLE L_VALS INDEX 1  .
  PA_VALS = L_VALS-ATWRT     .
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
  PERFORM SUB_TOTAL.                                        "UD1K912950
  DELETE FROM ZTPP_WIRE_DAY CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  SORT IT_DISP BY SERIAL MODEL  ALC_VALS1 ALC_VALS2 ALC_VALS3
                                ALC_VALS4 ALC_VALS5 .
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
  MODIFY ZTPP_WIRE_DAY     FROM TABLE IT_DISP .
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000 WITH 'Wire_day Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'Wire_day Update failed'.
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
  PERFORM CREATE_DATA   TABLES  IT_WBS   USING '99'.
  PERFORM CALC_WBS                                 .
  PERFORM CREATE_DATA   TABLES  IT_PI    USING '02'.
  PERFORM CALC_PAINTINPUT                          .
  PERFORM CREATE_DATA   TABLES  IT_PRJ   USING '88'.
  PERFORM CALC_PAINTREJECT                         .
  PERFORM CREATE_DATA   TABLES  IT_PBS   USING '06'.
  PERFORM CALC_PBS                                 .
* PERFORM calc_d21                                 .
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
  IT_DISP-D01  = IT_DISP-D01  + PA_DISP-D01 .
  IT_DISP-D02  = IT_DISP-D02  + PA_DISP-D02 .
  IT_DISP-D03  = IT_DISP-D03  + PA_DISP-D03 .
  IT_DISP-D04  = IT_DISP-D04  + PA_DISP-D04 .
  IT_DISP-D05  = IT_DISP-D05  + PA_DISP-D05 .
  IT_DISP-D06  = IT_DISP-D06  + PA_DISP-D06 .
  IT_DISP-D07  = IT_DISP-D07  + PA_DISP-D07 .
  IT_DISP-D08  = IT_DISP-D08  + PA_DISP-D08 .
  IT_DISP-D09  = IT_DISP-D09  + PA_DISP-D09 .
  IT_DISP-D10  = IT_DISP-D10  + PA_DISP-D10 .
  IT_DISP-D11  = IT_DISP-D11  + PA_DISP-D11 .
  IT_DISP-D12  = IT_DISP-D12  + PA_DISP-D12 .
  IT_DISP-D13  = IT_DISP-D13  + PA_DISP-D13 .
  IT_DISP-D14  = IT_DISP-D14  + PA_DISP-D14 .
  IT_DISP-D15  = IT_DISP-D15  + PA_DISP-D15 .
  IT_DISP-D16  = IT_DISP-D16  + PA_DISP-D16 .
  IT_DISP-D17  = IT_DISP-D17  + PA_DISP-D17 .
  IT_DISP-D18  = IT_DISP-D18  + PA_DISP-D18 .
  IT_DISP-D19  = IT_DISP-D19  + PA_DISP-D19 .
  IT_DISP-D20  = IT_DISP-D20  + PA_DISP-D20 .
  IT_DISP-D21  = IT_DISP-D21  + PA_DISP-D21 .
  IT_DISP-MITU = IT_DISP-MITU + PA_DISP-MITU.
ENDFORM.                    " add_routine

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
        L_VALS1(5)              TYPE C  ,
        L_VALS2(5)              TYPE C  ,
        L_VALS3(5)              TYPE C  ,
        L_VALS4(5)              TYPE C  ,
        L_VALS5(5)              TYPE C  .              " ALC CODE VALUE

  LT_DISP[] = IT_DISP[].

  " Accumulate the Data..
  CLEAR: IT_DISP, IT_DISP[].
  SORT LT_DISP BY MODEL     ALC_VALS1 ALC_VALS2
                  ALC_VALS3 ALC_VALS4 ALC_VALS5.
  READ TABLE LT_DISP INDEX 1.
  L_MODEL = LT_DISP-MODEL   .
*  l_code  = lt_disp-alc_code.
  L_VALS1 = LT_DISP-ALC_VALS1.
  L_VALS2 = LT_DISP-ALC_VALS2.
  L_VALS3 = LT_DISP-ALC_VALS3.
  L_VALS4 = LT_DISP-ALC_VALS4.
  L_VALS5 = LT_DISP-ALC_VALS5.
  IT_DISP     = LT_DISP         .
  PERFORM CLEAR_QTY_DISP  USING IT_DISP     .

  LOOP AT LT_DISP.
   IF L_MODEL = LT_DISP-MODEL      AND  L_VALS1 = LT_DISP-ALC_VALS1 AND
      L_VALS2 = LT_DISP-ALC_VALS2  AND  L_VALS3 = LT_DISP-ALC_VALS3 AND
         L_VALS4 = LT_DISP-ALC_VALS4  AND  L_VALS5 = LT_DISP-ALC_VALS5 .
      PERFORM ADD_ROUTINE  USING  LT_DISP .
      CONTINUE.
    ELSE.
*      CONDENSE lt_disp-alc_code .
      IT_DISP-SERIAL = STRLEN( L_VALS )    .
      APPEND IT_DISP.     CLEAR: IT_DISP   .
      IT_DISP     = LT_DISP         .
*     PERFORM clear_qty_disp  USING it_disp     .
      L_MODEL = LT_DISP-MODEL   .
      L_VALS1 = LT_DISP-ALC_VALS1.
      L_VALS2 = LT_DISP-ALC_VALS2.
      L_VALS3 = LT_DISP-ALC_VALS3.
      L_VALS4 = LT_DISP-ALC_VALS4.
      L_VALS5 = LT_DISP-ALC_VALS5.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_DISP LINES  L_LINE.
  IF L_LINE > 0.
*    CONDENSE lt_disp-alc_code .
    IT_DISP-SERIAL = STRLEN( L_VALS ) .
    APPEND IT_DISP.
  ENDIF.
ENDFORM.                    " SUMMARY_DISP

*&---------------------------------------------------------------------*
*&      Form  GET_CUVTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_CUVTAB  USING    PA_VTNAM  PA_KNOBJ.
  CLEAR: PA_KNOBJ.
  SELECT SINGLE VTINT INTO PA_KNOBJ
    FROM CUVTAB
   WHERE VTNAM = PA_VTNAM .
ENDFORM.                    " GET_CUVTAB

*&---------------------------------------------------------------------*
*&      Form  get_ALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_ALC   USING    PA_ATINN.
  DATA:  L_VALC         LIKE CUVTAB_VALC-VALC,
         LW_CUVTAB_VALC LIKE CUVTAB_VALC.

  L_VALC = C_NAME       .
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF LW_CUVTAB_VALC
    FROM CUVTAB_VALC
   WHERE VTINT = PA_ATINN
     AND VALC  = L_VALC  .

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE IT_ALC
    FROM CUVTAB_VALC
   WHERE VTINT = PA_ATINN
     AND SLNID = LW_CUVTAB_VALC-SLNID
     AND VALC  NE L_VALC  .
ENDFORM.                    " get_ALC

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
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  SORT IT_SUM BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_SUM INDEX 1.
  L_DAYS  = IT_SUM-DAYS    .
  L_WO    = IT_SUM-WORDER  .
  L_EXT   = IT_SUM-EXTC    .
  L_INT   = IT_SUM-INTC    .
  L_MODEL = IT_SUM-VM_MODEL  .
  LT_SUM  = IT_SUM          .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_SUM.
    IF L_DAYS  = IT_SUM-DAYS  AND L_WO  = IT_SUM-WORDER AND
       L_EXT   = IT_SUM-EXTC  AND L_INT = IT_SUM-INTC   AND
       L_MODEL = IT_SUM-VM_MODEL .
      IF IT_SUM-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_SUM-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_SUM-CNT = L_CNT       .
      LT_SUM-MITUCNT = L_MITUCNT.
      APPEND       LT_SUM      .
      LT_SUM  = IT_SUM         .
      L_CNT   = 1              .
      L_DAYS  = IT_SUM-DAYS    .
      L_WO    = IT_SUM-WORDER  .
      L_EXT   = IT_SUM-EXTC    .
      L_INT   = IT_SUM-INTC    .
      L_MODEL = IT_SUM-VM_MODEL.
      IF IT_SUM-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_SUM-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                     IT_SUM-WORDER IT_SUM-EXTC      IT_SUM-INTC .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_SUM-CNT = L_CNT       .
    LT_SUM-MITUCNT = L_MITUCNT.
    APPEND       LT_SUM      .
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
  DATA: L_DATA    LIKE TABLE OF ZTPP_INPUT_PLAN        WITH HEADER LINE.

  CASE PA_RP.
    WHEN '00' OR '01' .
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
*   PA_TAB-days       = l_days            .
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
*   PERFORM UPDATE_INPUT_PLAN  USING L_DAYS.
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
  DATA: LT_SEQ              LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  SORT IT_SEQ BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_SEQ INDEX 1.
  L_DAYS  = IT_SEQ-DAYS    .
  L_WO    = IT_SEQ-WORDER  .
  L_EXT   = IT_SEQ-EXTC    .
  L_INT   = IT_SEQ-INTC    .
  L_MODEL = IT_SEQ-VM_MODEL.
  LT_SEQ  = IT_SEQ         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_SEQ.
    IF L_DAYS  = IT_SEQ-DAYS  AND L_WO  = IT_SEQ-WORDER AND
       L_EXT   = IT_SEQ-EXTC  AND L_INT = IT_SEQ-INTC   AND
       L_MODEL = IT_SEQ-VM_MODEL .
      IF IT_SEQ-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_SEQ-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_SEQ-CNT = L_CNT       .
      LT_SEQ-MITUCNT = L_MITUCNT.
      APPEND       LT_SEQ      .
      LT_SEQ  = IT_SEQ         .
      L_CNT   = 1              .
      L_DAYS  = IT_SEQ-DAYS    .
      L_WO    = IT_SEQ-WORDER  .
      L_EXT   = IT_SEQ-EXTC    .
      L_INT   = IT_SEQ-INTC    .
      L_MODEL = IT_SEQ-VM_MODEL.
      IF IT_SEQ-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SEQ LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_SEQ-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                   L_WO   L_EXT     L_INT      .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_SEQ-CNT = L_CNT       .
    LT_SEQ-MITUCNT = L_MITUCNT.
    APPEND       LT_SEQ      .
  ENDIF.

  IT_SEQ[] = LT_SEQ[].
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
  DATA: LT_BI               LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  SORT IT_BI  BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_BI  INDEX 1.
  L_DAYS  = IT_BI-DAYS    .
  L_WO    = IT_BI-WORDER  .
  L_EXT   = IT_BI-EXTC    .
  L_INT   = IT_BI-INTC    .
  L_MODEL = IT_BI-VM_MODEL.
  LT_BI   = IT_BI         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_BI .
    IF L_DAYS  = IT_BI-DAYS  AND L_WO  = IT_BI-WORDER AND
       L_EXT   = IT_BI-EXTC  AND L_INT = IT_BI-INTC   AND
       L_MODEL = IT_BI-VM_MODEL .
      IF IT_BI-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_BI-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_BI-CNT = L_CNT       .
      LT_BI-MITUCNT = L_MITUCNT.
      APPEND       LT_BI      .
      LT_BI   = IT_BI         .
      L_CNT   = 1              .
      L_DAYS  = IT_BI-DAYS    .
      L_WO    = IT_BI-WORDER  .
      L_EXT   = IT_BI-EXTC    .
      L_INT   = IT_BI-INTC    .
      L_MODEL = IT_BI-VM_MODEL.
      IF IT_BI-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_BI  LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_BI-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                   L_WO   L_EXT     L_INT      .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_BI-CNT = L_CNT       .
    LT_BI-MITUCNT = L_MITUCNT.
    APPEND       LT_BI       .
  ENDIF.

  IT_BI[] = LT_BI[].
ENDFORM.                    " CALC_BODYINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_WBS.
  DATA: LT_WBS              LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  DESCRIBE TABLE IT_WBS LINES L_CNT .
  CHECK L_CNT > 0 .

  L_MITUCNT =  P_WBS * WA_UPH_B  .
  IF L_MITUCNT > 0 .
    IF L_MITUCNT < L_CNT .
      DELETE IT_WBS TO L_MITUCNT.
    ELSE.
      DELETE IT_WBS TO L_CNT    .
    ENDIF.
  ELSE.
    DELETE IT_WBS TO L_CNT    .
  ENDIF.
  CLEAR: L_MITUCNT, L_CNT.

  SORT IT_WBS BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_WBS INDEX 1.
  L_DAYS  = IT_WBS-DAYS    .
  L_WO    = IT_WBS-WORDER  .
  L_EXT   = IT_WBS-EXTC    .
  L_INT   = IT_WBS-INTC    .
  L_MODEL = IT_WBS-VM_MODEL.
  LT_WBS  = IT_WBS         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_WBS.
    IF L_DAYS  = IT_WBS-DAYS  AND L_WO  = IT_WBS-WORDER AND
       L_EXT   = IT_WBS-EXTC  AND L_INT = IT_WBS-INTC   AND
       L_MODEL = IT_WBS-VM_MODEL .
      IF IT_WBS-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_WBS-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_WBS-CNT = L_CNT       .
      LT_WBS-MITUCNT = L_MITUCNT.
      APPEND       LT_WBS      .
      LT_WBS  = IT_WBS         .
      L_CNT   = 1              .
      L_DAYS  = IT_WBS-DAYS    .
      L_WO    = IT_WBS-WORDER  .
      L_EXT   = IT_WBS-EXTC    .
      L_INT   = IT_WBS-INTC    .
      L_MODEL = IT_WBS-VM_MODEL.
      IF IT_WBS-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_WBS LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_WBS-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                   L_WO   L_EXT     L_INT      .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_WBS-CNT = L_CNT       .
    LT_WBS-MITUCNT = L_MITUCNT.
    APPEND       LT_WBS      .
  ENDIF.

  IT_WBS[] = LT_WBS[].
ENDFORM.                    " CALC_WBS

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_PAINTINPUT.
  DATA: LT_PI               LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  SORT IT_PI  BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_PI  INDEX 1.
  L_DAYS  = IT_PI-DAYS    .
  L_WO    = IT_PI-WORDER  .
  L_EXT   = IT_PI-EXTC    .
  L_INT   = IT_PI-INTC    .
  L_MODEL = IT_PI-VM_MODEL.
  LT_PI   = IT_PI         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_PI .
    IF L_DAYS  = IT_PI-DAYS  AND L_WO  = IT_PI-WORDER AND
       L_EXT   = IT_PI-EXTC  AND L_INT = IT_PI-INTC   AND
       L_MODEL = IT_PI-VM_MODEL .
      IF IT_PI-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_PI-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_PI-CNT = L_CNT       .
      LT_PI-MITUCNT = L_MITUCNT.
      APPEND       LT_PI      .
      LT_PI   = IT_PI          .
      L_CNT   = 1              .
      L_DAYS  = IT_PI-DAYS    .
      L_WO    = IT_PI-WORDER  .
      L_EXT   = IT_PI-EXTC    .
      L_INT   = IT_PI-INTC    .
      L_MODEL = IT_PI-VM_MODEL.
      IF IT_PI-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PI LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_PI-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                   L_WO   L_EXT     L_INT      .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_PI-CNT = L_CNT       .
    LT_PI-MITUCNT = L_MITUCNT.
    APPEND       LT_PI      .
  ENDIF.

  IT_PI[] = LT_PI[].
ENDFORM.                    " CALC_PAINTINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTREJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_PAINTREJECT.
  DATA: LT_PRJ              LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  DESCRIBE TABLE IT_PRJ LINES L_CNT  .
  CHECK L_CNT > 0 .

  L_MITUCNT = P_PRJ * WA_UPH_P.
  IF L_MITUCNT > 0 .
    IF L_MITUCNT < L_CNT .
      DELETE IT_PRJ TO L_MITUCNT.
    ELSE.
      DELETE IT_PRJ TO L_CNT    .
    ENDIF.
  ELSE.
    DELETE IT_PRJ TO L_CNT.
  ENDIF.
  CLEAR: L_MITUCNT, L_CNT.

  SORT IT_PRJ BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_PRJ INDEX 1.
  L_DAYS  = IT_PRJ-DAYS    .
  L_WO    = IT_PRJ-WORDER  .
  L_EXT   = IT_PRJ-EXTC    .
  L_INT   = IT_PRJ-INTC    .
  L_MODEL = IT_PRJ-VM_MODEL.
  LT_PRJ  = IT_PRJ         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_PRJ.
    IF L_DAYS  = IT_PRJ-DAYS  AND L_WO  = IT_PRJ-WORDER AND
       L_EXT   = IT_PRJ-EXTC  AND L_INT = IT_PRJ-INTC   AND
       L_MODEL = IT_PRJ-VM_MODEL .
      IF IT_PRJ-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_PRJ-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_PRJ-CNT = L_CNT       .
      LT_PRJ-MITUCNT = L_MITUCNT.
      APPEND       LT_PRJ      .
      LT_PRJ  = IT_PRJ         .
      L_CNT   = 1              .
      L_DAYS  = IT_PRJ-DAYS    .
      L_WO    = IT_PRJ-WORDER  .
      L_EXT   = IT_PRJ-EXTC    .
      L_INT   = IT_PRJ-INTC    .
      L_MODEL = IT_PRJ-VM_MODEL.
      IF IT_PRJ-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PRJ LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_PRJ-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                   L_WO   L_EXT     L_INT      .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_PRJ-CNT = L_CNT       .
    LT_PRJ-MITUCNT = L_MITUCNT.
    APPEND       LT_PRJ      .
  ENDIF.

  IT_PRJ[] = LT_PRJ[].
ENDFORM.                    " CALC_PAINTREJECT

*&---------------------------------------------------------------------*
*&      Form  CALC_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_PBS.
  DATA: LT_PBS              LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  SORT IT_PBS BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_PBS INDEX 1.
  L_DAYS  = IT_PBS-DAYS    .
  L_WO    = IT_PBS-WORDER  .
  L_EXT   = IT_PBS-EXTC    .
  L_INT   = IT_PBS-INTC    .
  L_MODEL = IT_PBS-VM_MODEL.
  LT_PBS  = IT_PBS         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_PBS.
    IF L_DAYS  = IT_PBS-DAYS  AND L_WO  = IT_PBS-WORDER AND
       L_EXT   = IT_PBS-EXTC  AND L_INT = IT_PBS-INTC   AND
       L_MODEL = IT_PBS-VM_MODEL .
      IF IT_PBS-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_PBS-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_PBS-CNT = L_CNT       .
      LT_PBS-MITUCNT = L_MITUCNT.
      APPEND       LT_PBS      .
      LT_PBS  = IT_PBS         .
      L_CNT   = 1              .
      L_DAYS  = IT_PBS-DAYS    .
      L_WO    = IT_PBS-WORDER  .
      L_EXT   = IT_PBS-EXTC    .
      L_INT   = IT_PBS-INTC    .
      L_MODEL = IT_PBS-VM_MODEL.
      IF IT_PBS-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_PBS LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_PBS-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                   L_WO   L_EXT     L_INT      .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_PBS-CNT = L_CNT       .
    LT_PBS-MITUCNT = L_MITUCNT.
    APPEND       LT_PBS      .
  ENDIF.

  IT_PBS[] = LT_PBS[].
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
  " Insert D-1 Field Value into Internal Table IT_DISP...
  LOOP AT IT_DISP.
    READ TABLE IT_DISP_D1 WITH KEY MODEL     = IT_DISP-MODEL
                                     ALC_VALS1 = IT_DISP-ALC_VALS1
                                     ALC_VALS2 = IT_DISP-ALC_VALS2
                                     ALC_VALS3 = IT_DISP-ALC_VALS3
                                     ALC_VALS4 = IT_DISP-ALC_VALS4
                                     ALC_VALS5 = IT_DISP-ALC_VALS5.
    IF SY-SUBRC = 0.
      IT_DISP-D_1 = IT_DISP_D1-D_1 .
      DELETE IT_DISP_D1 WHERE MODEL     = IT_DISP-MODEL
                            AND ALC_VALS1 = IT_DISP-ALC_VALS1
                            AND ALC_VALS2 = IT_DISP-ALC_VALS2
                            AND ALC_VALS3 = IT_DISP-ALC_VALS3
                            AND ALC_VALS4 = IT_DISP-ALC_VALS4
                            AND ALC_VALS5 = IT_DISP-ALC_VALS5 .
    ELSE.
      CLEAR: IT_DISP-D_1 .
    ENDIF.

    READ TABLE IT_DISP_SEQ  WITH KEY MODEL     = IT_DISP-MODEL
                                     ALC_VALS1 = IT_DISP-ALC_VALS1
                                     ALC_VALS2 = IT_DISP-ALC_VALS2
                                     ALC_VALS3 = IT_DISP-ALC_VALS3
                                     ALC_VALS4 = IT_DISP-ALC_VALS4
                                     ALC_VALS5 = IT_DISP-ALC_VALS5.
    IF SY-SUBRC = 0.
      IT_DISP-SEQ  = IT_DISP_SEQ-D_1 .
      IT_DISP-MITU = IT_DISP_SEQ-MITU.
      DELETE IT_DISP_SEQ  WHERE MODEL     = IT_DISP-MODEL
                            AND ALC_VALS1 = IT_DISP-ALC_VALS1
                            AND ALC_VALS2 = IT_DISP-ALC_VALS2
                            AND ALC_VALS3 = IT_DISP-ALC_VALS3
                            AND ALC_VALS4 = IT_DISP-ALC_VALS4
                            AND ALC_VALS5 = IT_DISP-ALC_VALS5 .
    ELSE.
      CLEAR: IT_DISP-SEQ, IT_DISP-MITU.
    ENDIF.

    READ TABLE IT_DISP_BI   WITH KEY MODEL     = IT_DISP-MODEL
                                     ALC_VALS1 = IT_DISP-ALC_VALS1
                                     ALC_VALS2 = IT_DISP-ALC_VALS2
                                     ALC_VALS3 = IT_DISP-ALC_VALS3
                                     ALC_VALS4 = IT_DISP-ALC_VALS4
                                     ALC_VALS5 = IT_DISP-ALC_VALS5.
    IF SY-SUBRC = 0.
      IT_DISP-BODYIN = IT_DISP_BI-D_1 .
      DELETE IT_DISP_BI   WHERE MODEL     = IT_DISP-MODEL
                            AND ALC_VALS1 = IT_DISP-ALC_VALS1
                            AND ALC_VALS2 = IT_DISP-ALC_VALS2
                            AND ALC_VALS3 = IT_DISP-ALC_VALS3
                            AND ALC_VALS4 = IT_DISP-ALC_VALS4
                            AND ALC_VALS5 = IT_DISP-ALC_VALS5 .
    ELSE.
      CLEAR: IT_DISP-BODYIN  .
    ENDIF.

    READ TABLE IT_DISP_WBS  WITH KEY MODEL     = IT_DISP-MODEL
                                     ALC_VALS1 = IT_DISP-ALC_VALS1
                                     ALC_VALS2 = IT_DISP-ALC_VALS2
                                     ALC_VALS3 = IT_DISP-ALC_VALS3
                                     ALC_VALS4 = IT_DISP-ALC_VALS4
                                     ALC_VALS5 = IT_DISP-ALC_VALS5.
    IF SY-SUBRC = 0.
      IT_DISP-WBS = IT_DISP_WBS-D_1 .
      DELETE IT_DISP_WBS  WHERE MODEL     = IT_DISP-MODEL
                            AND ALC_VALS1 = IT_DISP-ALC_VALS1
                            AND ALC_VALS2 = IT_DISP-ALC_VALS2
                            AND ALC_VALS3 = IT_DISP-ALC_VALS3
                            AND ALC_VALS4 = IT_DISP-ALC_VALS4
                            AND ALC_VALS5 = IT_DISP-ALC_VALS5 .
    ELSE.
      CLEAR: IT_DISP-WBS .
    ENDIF.

    READ TABLE IT_DISP_PI   WITH KEY MODEL     = IT_DISP-MODEL
                                     ALC_VALS1 = IT_DISP-ALC_VALS1
                                     ALC_VALS2 = IT_DISP-ALC_VALS2
                                     ALC_VALS3 = IT_DISP-ALC_VALS3
                                     ALC_VALS4 = IT_DISP-ALC_VALS4
                                     ALC_VALS5 = IT_DISP-ALC_VALS5.
    IF SY-SUBRC = 0.
      IT_DISP-PAINT  = IT_DISP_PI-D_1 .
      DELETE IT_DISP_PI   WHERE MODEL     = IT_DISP-MODEL
                            AND ALC_VALS1 = IT_DISP-ALC_VALS1
                            AND ALC_VALS2 = IT_DISP-ALC_VALS2
                            AND ALC_VALS3 = IT_DISP-ALC_VALS3
                            AND ALC_VALS4 = IT_DISP-ALC_VALS4
                            AND ALC_VALS5 = IT_DISP-ALC_VALS5 .
    ELSE.
      CLEAR: IT_DISP-PAINT  .
    ENDIF.

    READ TABLE IT_DISP_PRJ  WITH KEY MODEL     = IT_DISP-MODEL
                                     ALC_VALS1 = IT_DISP-ALC_VALS1
                                     ALC_VALS2 = IT_DISP-ALC_VALS2
                                     ALC_VALS3 = IT_DISP-ALC_VALS3
                                     ALC_VALS4 = IT_DISP-ALC_VALS4
                                     ALC_VALS5 = IT_DISP-ALC_VALS5.
    IF SY-SUBRC = 0.
      IT_DISP-PRJ = IT_DISP_PRJ-D_1 .
      DELETE IT_DISP_PRJ  WHERE MODEL     = IT_DISP-MODEL
                            AND ALC_VALS1 = IT_DISP-ALC_VALS1
                            AND ALC_VALS2 = IT_DISP-ALC_VALS2
                            AND ALC_VALS3 = IT_DISP-ALC_VALS3
                            AND ALC_VALS4 = IT_DISP-ALC_VALS4
                            AND ALC_VALS5 = IT_DISP-ALC_VALS5 .
    ELSE.
      CLEAR: IT_DISP-PRJ .
    ENDIF.

    READ TABLE IT_DISP_PBS  WITH KEY MODEL     = IT_DISP-MODEL
                                     ALC_VALS1 = IT_DISP-ALC_VALS1
                                     ALC_VALS2 = IT_DISP-ALC_VALS2
                                     ALC_VALS3 = IT_DISP-ALC_VALS3
                                     ALC_VALS4 = IT_DISP-ALC_VALS4
                                     ALC_VALS5 = IT_DISP-ALC_VALS5.
    IF SY-SUBRC = 0.
      IT_DISP-PBS = IT_DISP_PBS-D_1 .
      DELETE IT_DISP_PBS  WHERE MODEL     = IT_DISP-MODEL
                            AND ALC_VALS1 = IT_DISP-ALC_VALS1
                            AND ALC_VALS2 = IT_DISP-ALC_VALS2
                            AND ALC_VALS3 = IT_DISP-ALC_VALS3
                            AND ALC_VALS4 = IT_DISP-ALC_VALS4
                            AND ALC_VALS5 = IT_DISP-ALC_VALS5 .
    ELSE.
      CLEAR: IT_DISP-PBS .
    ENDIF.

    MODIFY IT_DISP.
  ENDLOOP.

  " Remain Fiedls Insert.....
  LOOP AT IT_DISP_D1 .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_D1 TO IT_DISP.
    CLEAR: IT_DISP-D_1, IT_DISP-D_1.
    IT_DISP-D_1 = IT_DISP_D1-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_D1, IT_DISP_D1[].

  LOOP AT IT_DISP_SEQ  .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_SEQ  TO IT_DISP.
    CLEAR: IT_DISP-D_1, IT_DISP-SEQ.
    IT_DISP-SEQ = IT_DISP_SEQ-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_SEQ , IT_DISP_SEQ[].

  LOOP AT IT_DISP_BI   .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_BI   TO IT_DISP.
    CLEAR: IT_DISP-D_1, IT_DISP-BODYIN.
    IT_DISP-BODYIN = IT_DISP_BI-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_BI  , IT_DISP_BI[].

  LOOP AT IT_DISP_WBS  .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_WBS  TO IT_DISP.
    CLEAR: IT_DISP-D_1, IT_DISP-WBS.
    IT_DISP-WBS = IT_DISP_WBS-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_WBS , IT_DISP_WBS[].

  LOOP AT IT_DISP_PI   .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_PI   TO IT_DISP.
    CLEAR: IT_DISP-D_1, IT_DISP-PAINT.
    IT_DISP-PAINT  = IT_DISP_PI-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_PI  , IT_DISP_PI[].

  LOOP AT IT_DISP_PRJ  .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_PRJ  TO IT_DISP.
    CLEAR: IT_DISP-D_1, IT_DISP-PRJ.
    IT_DISP-PRJ = IT_DISP_PRJ-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_PRJ , IT_DISP_PRJ[].

  LOOP AT IT_DISP_PBS  .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_PBS  TO IT_DISP.
    CLEAR: IT_DISP-D_1, IT_DISP-PBS.
    IT_DISP-PBS = IT_DISP_PBS-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_PBS , IT_DISP_PBS[].
ENDFORM.                    " INSERT_FIELD_VALS

*&---------------------------------------------------------------------*
*&      Form  calc_prod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALC_PROD.
  DATA: LT_D1               LIKE TABLE OF IT_SUM       WITH HEADER LINE,
        L_NAME(40)          TYPE C              ,
        L_MODEL             LIKE IT_SUM-VM_MODEL,
        L_WORDER            LIKE MARA-MATNR  ,
        L_DAYS              LIKE IT_SUM-DAYS ,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  "1. Take the data of The Table(IT_PROD) of which RP06's Qty is GT 0.
  DELETE IT_PROD WHERE RP06Q = 0 .

  LOOP AT IT_PROD.
    CLEAR: IT_D1 .
    CONCATENATE IT_PROD-WO_SER IT_PROD-NATION IT_PROD-DEALER
                IT_PROD-EXTC   IT_PROD-INTC   INTO L_WORDER .
    PERFORM GET_MODEL USING L_WORDER  IT_D1-VM_MODEL .
    DO IT_PROD-RP06Q TIMES.
*     IT_D1-vm_bodyser = l_data-body_ser   .
      IT_D1-WORDER     = L_WORDER(14)      .
*     IT_D1-mitu       = IT_PROD-mitu      .
      IT_D1-EXTC       = IT_PROD-EXTC      .
      IT_D1-INTC       = IT_PROD-INTC      .
*     IT_D1-status     = l_data-status     .
      IT_D1-RP         = '06'              .
      IT_D1-CNT        = 1                 .
      APPEND IT_D1  .
    ENDDO.
  ENDLOOP.

  SORT IT_D1 BY DAYS  WORDER EXTC INTC VM_MODEL.
  READ TABLE IT_D1 INDEX 1.
  L_DAYS  = IT_D1-DAYS    .
  L_WO    = IT_D1-WORDER  .
  L_EXT   = IT_D1-EXTC    .
  L_INT   = IT_D1-INTC    .
  L_MODEL = IT_D1-VM_MODEL.
  LT_D1   = IT_D1         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_D1  .
    IF L_DAYS  = IT_D1-DAYS  AND L_WO  = IT_D1-WORDER AND
       L_EXT   = IT_D1-EXTC  AND L_INT = IT_D1-INTC   AND
       L_MODEL = IT_D1-VM_MODEL .
      IF IT_D1-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      CLEAR: WA_COUNT.
      LOOP AT IT_ALC WHERE MODEL = L_MODEL .
        WA_COUNT = WA_COUNT + 1.
        CONCATENATE 'LT_D1-ALC_VALS' WA_COUNT  INTO L_NAME .
        ASSIGN   (L_NAME)                       TO   <WA_VALS>.
        PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                     L_WO   L_EXT     L_INT      .
        PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                            CHANGING <WA_VALS>                   .
      ENDLOOP.
      LT_D1-CNT = L_CNT       .
      LT_D1-MITUCNT = L_MITUCNT.
      APPEND       LT_D1      .
      LT_D1   = IT_D1          .
      L_CNT   = 1              .
      L_DAYS  = IT_D1-DAYS    .
      L_WO    = IT_D1-WORDER  .
      L_EXT   = IT_D1-EXTC    .
      L_INT   = IT_D1-INTC    .
      L_MODEL = IT_D1-VM_MODEL.
      IF IT_D1-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_D1 LINES L_SIZE .
  IF L_SIZE > 0 .
    CLEAR: WA_COUNT.
    LOOP AT IT_ALC WHERE MODEL = L_MODEL .
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'LT_D1-ALC_VALS' WA_COUNT  INTO L_NAME .
      ASSIGN   (L_NAME)                       TO   <WA_VALS>.
      PERFORM GET_WORORDER   USING IT_ALC-TYPE_ALC  L_WORDER
                                   L_WO   L_EXT     L_INT      .
      PERFORM GET_ALC_VALUE  USING L_WORDER         IT_ALC-VALC
                          CHANGING <WA_VALS>                  .
    ENDLOOP.
    LT_D1-CNT = L_CNT       .
    LT_D1-MITUCNT = L_MITUCNT.
    APPEND       LT_D1      .
  ENDIF.

  IT_D1[] = LT_D1[].
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
        L_COUNT              TYPE I .

  " Set the BASIC Information for the UPH & Work Time...
  CLEAR: L_DATE, L_COUNT.
  L_DATE = P_DATES  .
  PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE.
  IF L_DATE = P_DATES  .
    PERFORM GET_DAY       USING L_DATE IT_MASTER-DAY  .
    PERFORM GET_WORKTIME1 USING L_DATE IT_MASTER-TIME IT_MASTER-DAY 'T'.
    PERFORM GET_UPH      USING L_DATE IT_MASTER-UPH IT_MASTER-SHIFT 'T'.
    PERFORM GET_UPH_SHOP     USING L_DATE  WA_UPH_T   'T'              .
    PERFORM GET_UPH_SHOP     USING L_DATE  WA_UPH_B   'B'              .
    PERFORM GET_UPH_SHOP     USING L_DATE  WA_UPH_P   'P'              .
  ELSE.
    L_DATE = P_DATES .
    CLEAR: WA_UPH_T, WA_UPH_B, WA_UPH_P.
    PERFORM GET_DAY         USING L_DATE IT_MASTER-DAY  .
  ENDIF.
  IT_MASTER-SEQ    = L_COUNT.    IT_MASTER-DATE   = L_DATE .
  APPEND IT_MASTER.  CLEAR: IT_MASTER.

  " From D+1 Day To D+21 Day....(Only Working Dates in FACTORY-Calendar)
  L_DATE = P_DATES  .
  DO 21 TIMES.
    L_COUNT  = L_COUNT + 1.
    PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE.
    PERFORM GET_DAY       USING L_DATE IT_MASTER-DAY  .
   PERFORM GET_WORKTIME1 USING L_DATE IT_MASTER-TIME IT_MASTER-DAY  'T'.
   PERFORM GET_UPH       USING L_DATE IT_MASTER-UPH IT_MASTER-SHIFT 'T'.
    IT_MASTER-SEQ    = L_COUNT.
    IT_MASTER-DATE   = L_DATE .
    APPEND IT_MASTER.  CLEAR: IT_MASTER.
    L_DATE   = L_DATE  + 1.
  ENDDO.
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
  PA_UPH  = W_UPH.

*  DATA lw_ld          LIKE zvpp_ld .
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
*       AND arbpl     = pa_arbpl     .
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
*       AND arbpl     = pa_arbpl    .
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
*&      Form  GET_UPH_SHOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM GET_UPH_SHOP USING    PA_WDATE  PA_UPH  PA_ARBPL .

  DATA: W_UPH  LIKE ZTPP_STATUS-UPH.

  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      DATE          = PA_WDATE
*      SHIFT         =
      SHOP          = PA_ARBPL
    IMPORTING
      UPH           = W_UPH
            .
  PA_UPH  = W_UPH.

*  DATA lw_ld          LIKE zvpp_ld .
*
*  SELECT SINGLE * INTO lw_ld
*    FROM zvpp_ld
*   WHERE ld_perst <= pa_wdate
*     AND ld_pered >= pa_wdate
*     AND arbpl     = pa_arbpl .
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH_SHOP

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
  CLEAR: PA_DISP-D01, PA_DISP-D02, PA_DISP-D03, PA_DISP-D04,
         PA_DISP-D05, PA_DISP-D06, PA_DISP-D07, PA_DISP-D08,
         PA_DISP-D09, PA_DISP-D10, PA_DISP-D11, PA_DISP-D12,
         PA_DISP-D13, PA_DISP-D14, PA_DISP-D15, PA_DISP-D16,
         PA_DISP-D17, PA_DISP-D18, PA_DISP-D19, PA_DISP-D20,
         PA_DISP-D21, PA_DISP-D_1, PA_DISP-SEQ, PA_DISP-BODYIN,
         PA_DISP-WBS, PA_DISP-PRJ, PA_DISP-PBS, PA_DISP-PAINT,
         PA_DISP-MITU.                 "Missed UD1K912950
ENDFORM.                    " CLEAR_QTY_DISP

*&---------------------------------------------------------------------*
*&      Form  UPDATE_INPUT_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DAYS  text
*----------------------------------------------------------------------*
FORM UPDATE_INPUT_PLAN USING    PA_DAYS.
  UPDATE ZTPP_INPUT_PLAN   SET RS01 = PA_DAYS
                         WHERE PLNT = IT_DATA-PLNT
                           AND LINE = IT_DATA-LINE
                           AND MODL = IT_DATA-MODL
                           AND BODY_SER = IT_DATA-BODY_SER
                           AND SEQ_DATE = IT_DATA-SEQ_DATE
                           AND SEQ_SERIAL = IT_DATA-SEQ_SERIAL
                           AND SEQ_CODE = IT_DATA-SEQ_CODE .
ENDFORM.                    " UPDATE_INPUT_PLAN

*&---------------------------------------------------------------------*
*&      Form  GET_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*      -->P_IT_D1_VM_MODEL  text
*----------------------------------------------------------------------*
FORM GET_MODEL USING    PA_WORDER   PA_MODEL.
  DATA: LT_VAL          LIKE TABLE OF ZSPP_VIN_VALUE   WITH HEADER LINE.

  LT_VAL-ATNAM = 'P_MODEL'.  APPEND LT_VAL.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = PA_WORDER
      CTYPE              = '001'
*     DISPLAY            = 'D'
    TABLES
      VAL_TABLE          = LT_VAL
    EXCEPTIONS
      NO_DATA            = 1
      ERROR_MODE         = 2
      ERROR_OBJECT       = 3
      ERROR_VALUE        = 4
      OTHERS             = 5 .

  READ TABLE LT_VAL  INDEX 1.
  IF SY-SUBRC = 0 AND LT_VAL-ZFLAG IS INITIAL.
    PA_MODEL = LT_VAL-ATWRT.
  ELSE.
    CLEAR: PA_MODEL.
  ENDIF.
ENDFORM.                    " GET_MODEL
*&---------------------------------------------------------------------*
*&      Form  SUB_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUB_TOTAL.
  LOOP AT IT_DISP.
    IT_DISP-STOT =
           IT_DISP-D01 + IT_DISP-D02 + IT_DISP-D03 + IT_DISP-D04
         + IT_DISP-D05 + IT_DISP-D06 + IT_DISP-D07 + IT_DISP-D08
         + IT_DISP-D09 + IT_DISP-D10 + IT_DISP-D11 + IT_DISP-D12
         + IT_DISP-D13 + IT_DISP-D14 + IT_DISP-D15 + IT_DISP-D16
         + IT_DISP-D17 + IT_DISP-D18 + IT_DISP-D19 + IT_DISP-D20
         + IT_DISP-D21.
    MODIFY IT_DISP.
  ENDLOOP.
ENDFORM.                    " SUB_TOTAL
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
