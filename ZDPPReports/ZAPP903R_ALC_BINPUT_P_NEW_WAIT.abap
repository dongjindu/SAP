*****************************************************************
* Program Name      : ZAPP903R_ALC_BINPUT
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : Body-Input Plan
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 04/28/06   Furong Wang               Parallel
* Change : Change the Time Tule(Tack-Time --> Lead Time)
*****************************************************************
* 09/29/2004  YONGPING LI  UD1K912346 MOVE PREVIOUS DAY INITIAL
*                                     DATA TO
*                                    EVENT AT SELECTION-SCREEN.
*#1 03/08/2005   wskim    UD1K914858 Condition logic supplement
*#2 03/22/2005   wskim                Extend plan
*****************************************************************
REPORT  ZAPP903R_ALC_BINPUT   MESSAGE-ID ZMPP.

*----------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------
TABLES: AUSP,ZTPP_FWO.

*----------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------
DATA: BEGIN OF IT_DATA        OCCURS 0,
        OBJEK                 LIKE AUSP-OBJEK.     " Vehicle Code
        INCLUDE STRUCTURE     ZTPP_INPUT_PLAN.
DATA: END OF IT_DATA.

DATA: BEGIN OF IT_PHARSE      OCCURS 0        .
        INCLUDE STRUCTURE     ZSPP_CONDITION  .
DATA:   ATINN                 LIKE CABN-ATINN ,
        ATWRT                 LIKE AUSP-ATWRT ,
        ATFLV                 LIKE AUSP-ATFLV ,
      END OF IT_PHARSE.

***** PARALLEL
*DATA: BEGIN OF it_sum         OCCURS 0,
*        rp                    LIKE ztpp_plan_key-serial,
*        worder                LIKE mara-matnr,
*        status                LIKE ztpp_input_plan-status,
*        cond                  LIKE ztpp_plan_key-key_code,
*        hours                 TYPE i  ,
*        extc                  LIKE ztpp_input_plan-extc,
*        intc                  LIKE ztpp_input_plan-intc,
*        mitu                  TYPE zmitu,
*        mitucnt               TYPE i  ,
*        cnt                   TYPE i  ,
*      END OF it_sum .

DATA: IT_SUM LIKE TABLE OF ZSPP_INPUT_SUM WITH HEADER LINE.

DATA: IT_SUM_P LIKE TABLE OF IT_SUM WITH HEADER LINE.

DATA: TASKNAME(4) TYPE N VALUE '0001',
      SND_JOBS    TYPE I ,
      RCV_JOBS    TYPE I ,
      EXCEP_FLAG  TYPE C.
** end

DATA: BEGIN OF IT_MASTER      OCCURS 0,
        SEQ                   TYPE I  ,             " Sequence
        DATE                  TYPE D  ,             " Date
        DAY                   LIKE KAPA-TAGNR,      " Day
        SHIFT                 LIKE KAPA-SCHNR,      " Shift
        TIME            TYPE KAPENDZT  ,      " Times for working
        TUN              TYPE LD_LANTU  ,      " Unit  for Time
        UPH                   TYPE ZVPP_LD-LRATE,   " UPH
      END OF IT_MASTER.

DATA: IT_PROD     LIKE TABLE OF ZTPP_DAY_SUM    WITH HEADER LINE,
      IT_COND     LIKE TABLE OF ZTPP_PLAN_KEY   WITH HEADER LINE,
     IT_SUM_PROD LIKE TABLE OF IT_SUM          WITH HEADER LINE,
     IT_DISP_PROD LIKE TABLE OF ZTPP_ALC_BINPUT WITH HEADER LINE,
     IT_DISP      LIKE TABLE OF ZTPP_ALC_BINPUT WITH HEADER LINE.

*----------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------
DATA: WA_DISP             LIKE IT_DISP                         ,
      WA_DATA             LIKE IT_DATA                         ,
      WA_WDATE            LIKE SY-DATUM                        ,
      WA_KALID            LIKE KAKO-KALID                      ,
      WA_UZEIT            LIKE SY-UZEIT                        ,
      WA_INDEX            LIKE SY-TABIX                        ,
      WA_ERROR            TYPE C                               ,
      WA_FLAG             TYPE C                               ,
      WA_HOUR             TYPE I                               .

*----------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------
FIELD-SYMBOLS: <WA_DFIELD>    TYPE ANY.

*----------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------

*----------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_DATES      TYPE D        OBLIGATORY,
            P_MITU       TYPE C,
            P_FWO        TYPE C        .

PARAMETERS: P_SRVGRP LIKE RZLLITAB-CLASSNAME OBLIGATORY
                     DEFAULT 'PG_SEQ'.
PARAMETERS: W_MAX_W TYPE I DEFAULT 3.
SELECTION-SCREEN END OF BLOCK B3.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------

*BEGIN OF CHANGE BY CHRIS ON 09/29/2004 --UD1K912346
*----------------------------------------------------------------
*INITIALIZATION.    " DELETE THIS EVENT
*----------------------------------------------------------------

AT SELECTION-SCREEN.   "ADD THIS EVENT
*----------------------------------------------------------------
*END OF CHANGE BY CHRIS ON 09/29/2004   --UD1K912346
*----------------------------------------------------------------
  " Get the Date for the Production Reporting Date(Last Date)
  WA_WDATE = P_DATES - 1.
  PERFORM READ_SHOP_CALID   USING WA_KALID.
  PERFORM READ_WORKING_DATE USING '-'  WA_KALID  WA_WDATE.
  PERFORM GET_DAY          USING WA_WDATE IT_MASTER-DAY .
  PERFORM GET_WORKING_TIME USING WA_WDATE IT_MASTER-TIME
                           IT_MASTER-DAY .
  PERFORM GET_UPH          USING WA_WDATE IT_MASTER-UPH
                           IT_MASTER-SHIFT.
  IT_MASTER-SEQ    = 99.
  IT_MASTER-DATE = WA_WDATE .
  APPEND IT_MASTER.  CLEAR: IT_MASTER.


*----------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------
  PERFORM CLEAR_VARIABLE               .
  PERFORM SET_INFORMATION              .
  PERFORM READ_INPUTPLAN               .
  PERFORM CREATE_SUMMARY               .
  PERFORM INSERT_FIELD_VALS            .
  PERFORM DISPLAY_DATA                 .
*----------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------


*----------------------------------------------------------------
*&      Form  CLEAR_VARIABLE
*----------------------------------------------------------------
*       text
*----------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------
FORM CLEAR_VARIABLE.
  CLEAR: IT_DATA,   IT_COND,   IT_SUM,   IT_DISP,
         IT_DATA[], IT_COND[], IT_SUM[], IT_DISP[],
         WA_DATA,  WA_UZEIT, WA_INDEX, WA_HOUR.
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
    WHERE STATUS <= '00'  AND
          NOT ( MITU EQ 'X' OR MITU EQ 'Y' ).
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_INPUT_PLAN
    WHERE STATUS <= '00'   .
  ENDIF.

  DESCRIBE TABLE IT_DATA LINES  WA_HOUR .
  IF WA_HOUR = 0.
    DELETE FROM ZTPP_ALC_BINPUT CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
    LEAVE PROGRAM .
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PROD
    FROM ZTPP_DAY_SUM
   WHERE WDATE = WA_WDATE AND
         RP01Q NE 0.
**

  SORT IT_DATA BY RSNUM SERIAL.
  PERFORM CHECK_HOLDING_CAR.

  DATA: L_FWOCL LIKE ZTPP_FWO-WORDER,
        L_WORDER LIKE ZTPP_FWO-O_WORDER,
        L_ORDR LIKE ZTPP_PMT07JB_A-ORDR,
        L_DIST LIKE ZTPP_PMT07JB_A-DIST,
        L_FSC LIKE ZTPP_WOSUM-FSC,
        L_YEAR LIKE ZTPP_PMT07JB_A-MOYE.

  IF NOT P_FWO IS INITIAL.
    LOOP AT IT_DATA.
      IF IT_DATA-WORK_ORDER+0(1) = 'F'.
        CONCATENATE IT_DATA-WORK_ORDER IT_DATA-EXTC IT_DATA-INTC
               INTO L_FWOCL.
        SELECT SINGLE O_WORDER INTO L_WORDER
          FROM ZTPP_FWO
          WHERE WORDER = L_FWOCL.
        IF SY-SUBRC = 0.
          IT_DATA-WORK_ORDER = L_WORDER.
          MODIFY IT_DATA.
        ELSE.
          L_ORDR = IT_DATA-WORK_ORDER+0(9).
          L_DIST = IT_DATA-WORK_ORDER+9(5).

          SELECT SINGLE MOYE INTO L_YEAR
            FROM ZTPP_PMT07JB_A
           WHERE ORDR = L_ORDR
             AND DIST = L_DIST
             AND EXTC = IT_DATA-EXTC
             AND INTC = IT_DATA-INTC.
** Changed by Furong on 10/10/07 for EBOM
*       CONCATENATE l_year l_dist it_data-mi INTO l_fsc.
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
            MODIFY IT_DATA.
*        ELSE.
*          CLEAR: it_data-work_order.
          ENDIF.
        ENDIF.
        CLEAR: L_FWOCL, L_WORDER.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  SORT it_data BY rsnum serial .

ENDFORM.                    " READ_INPUTPLAN

*----------------------------------------------------------------
*&      Form  CREATE_SUMMARY
*----------------------------------------------------------------
*       text
*----------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------
FORM CREATE_SUMMARY.
  DATA: L_ATINN              LIKE CABN-ATINN ,
        L_ATWRT              LIKE AUSP-ATWRT ,
        L_ATFLV              LIKE AUSP-ATFLV ,
        L_ERROR              TYPE C          ,
        L_INDEX              TYPE I          ,
        L_WORDER        LIKE MARA-MATNR ,
        LT_DISP         LIKE TABLE OF IT_DISP WITH HEADER LINE,
        LT_DISP_PROD    LIKE TABLE OF IT_DISP WITH HEADER LINE,
        LT_SUM_PROD     LIKE TABLE OF IT_PROD WITH HEADER LINE,
        LT_SUM          LIKE TABLE OF IT_SUM  WITH HEADER LINE,
        WA_SUM          LIKE IT_SUM,
        IT_SUM_TEMP     LIKE TABLE OF IT_SUM  WITH HEADER LINE.

  DATA: W_LINES TYPE I.
*        w_clen  TYPE i,
*        w_max   TYPE i,
*        w_free  TYPE i,
*        w_no_times TYPE i,
*        w_lp_idx   TYPE i,
*        w_frm_idx  TYPE i,
*        w_to_idx   TYPE i,
*        w_rem      TYPE i.

  DATA:  W_TOT_REC TYPE I,
         TIMEOUT   TYPE I.

  PERFORM GET_CONDITION        .
  PERFORM READ_INTERNAL_TABLE  . " Separate the Time-Horizon.
  PERFORM SUM_PLAN             . " Summarize the Time-Horizon
*---start1 wskim
*get data ZTPP_PMT07JB_A gubun 'B'
  PERFORM GET_DATA_ZTPP_PMT07JB.
*---end

  SORT IT_SUM BY WORDER EXTC INTC HOURS.
  CLEAR W_LINES.
  DESCRIBE TABLE IT_SUM LINES W_LINES.

  LT_SUM[]      = IT_SUM[]     .
  LT_SUM_PROD[] = IT_SUM_PROD[].

  LOOP AT IT_COND.
    CLEAR: RCV_JOBS, SND_JOBS.
    PERFORM GET_COND_PARSING  .
    IT_SUM[] = LT_SUM[]       .
    IT_SUM_PROD[] = LT_SUM_PROD[].

    READ TABLE IT_SUM INDEX 1.
    MOVE-CORRESPONDING IT_SUM TO WA_SUM.
    WA_SUM-RP     = IT_COND-SERIAL.
    WA_SUM-COND   = IT_COND-KEY_CODE.

    MODIFY IT_SUM FROM WA_SUM TRANSPORTING RP COND
                 WHERE WORDER <> SPACE .

    CLEAR: IT_SUM_TEMP, IT_SUM_TEMP[], IT_SUM_P, IT_SUM_P[].
    READ TABLE IT_SUM INDEX 1.
    IT_SUM_TEMP = IT_SUM.
    W_TOT_REC = 0.

    LOOP AT IT_SUM.

      IF IT_SUM-WORDER <> IT_SUM_TEMP-WORDER.
        W_TOT_REC = W_TOT_REC + 1.
      ENDIF.

      IF W_TOT_REC > W_MAX_W OR SY-TABIX >= W_LINES.

        IF IT_SUM_TEMP[] IS INITIAL.
          CONTINUE.
        ENDIF.
        IF SY-TABIX >= W_LINES.
          IT_SUM_TEMP = IT_SUM.
          APPEND IT_SUM_TEMP.
        ENDIF.

*   submit parallel processes
        DO.
          CALL FUNCTION 'ZPP_CHECK_CONDITION'
           STARTING NEW TASK TASKNAME
           DESTINATION IN GROUP P_SRVGRP
           PERFORMING TASK_RECEIVE ON END OF TASK
          TABLES
            P_PHARSE       = IT_PHARSE
            P_SUM          = IT_SUM_TEMP
          EXCEPTIONS
            SYSTEM_FAILURE        = 1
            COMMUNICATION_FAILURE = 2
            RESOURCE_FAILURE      = 3.
          CASE SY-SUBRC.
            WHEN 0.
              TASKNAME = TASKNAME + 1.
              SND_JOBS = SND_JOBS + 1.
              CLEAR: EXCEP_FLAG.
              EXIT.
            WHEN 1 OR 2.
              EXCEP_FLAG = 'X'.
            WHEN 3.
              IF EXCEP_FLAG = SPACE.
                EXCEP_FLAG = 'X'.
             WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '0.1' SECONDS.
             WRITE AT: /001(40) 'Waiting for Resource '.
             WRITE AT: /001(40) IT_COND-KEY_CODE, (70) SY-UZEIT.
              ELSE.
             WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '1.0' SECONDS.
             WRITE AT: /001(40) 'Waiting for Resource '.
             WRITE AT: /001(40) IT_COND-KEY_CODE, (70) SY-UZEIT.
            ENDIF.
              IF SY-SUBRC EQ 0.
                CLEAR EXCEP_FLAG.
              ENDIF.
          ENDCASE.
        ENDDO.
        CLEAR: IT_SUM_TEMP, IT_SUM_TEMP[],W_TOT_REC.
        IT_SUM_TEMP = IT_SUM.
        APPEND IT_SUM_TEMP.
      ELSE.
        IT_SUM_TEMP = IT_SUM.
        APPEND IT_SUM_TEMP.
      ENDIF.
    ENDLOOP.

*    WAIT UNTIL rcv_jobs >= snd_jobs. Replaced by following code.

    TIMEOUT  = 0.

    DO.
      WAIT UNTIL RCV_JOBS >= SND_JOBS.
      CASE SY-SUBRC.
        WHEN 0.
*     Condition met
          IF RCV_JOBS >= SND_JOBS.
            WRITE AT: /001(20) 'All jobs'.
            WRITE AT:   35(10)  RCV_JOBS.
            WRITE AT:   45(10)  SND_JOBS.
            EXIT.
          ENDIF.
        WHEN 4.
          WRITE AT: /001(50) 'Parallel process error No more jobs'.
          WRITE AT:   55(10)  RCV_JOBS.
          WRITE AT:   65(10)  SND_JOBS.
          WRITE AT: /055(30) IT_COND-KEY_CODE.
          TIMEOUT = TIMEOUT + 1.
        WHEN 8.
          WRITE AT: /001(30) 'Waiting for jobs'.
          WRITE AT:   35(10)  RCV_JOBS.
          WRITE AT:   45(10)  SND_JOBS.
          WRITE AT:   55(10)  TIMEOUT.
          WRITE AT:   75(30) IT_COND-KEY_CODE.
          TIMEOUT = TIMEOUT + 1.
      ENDCASE.


      IF TIMEOUT >= 100.
        EXIT.
      ENDIF.
    ENDDO.


*    IF sy-subrc = 0. Replaced by following code

    IF TIMEOUT < 100.

      CLEAR: IT_SUM[], IT_SUM.
      IT_SUM[] = IT_SUM_P[].

      " Summarize the IT_SUM-CNT.
      PERFORM CALC_ALC.

      LOOP AT IT_SUM_PROD.
        CLEAR: L_ERROR.
       CONCATENATE IT_SUM_PROD-WORDER IT_SUM_PROD-EXTC
                iT_SUM_PROD-INTC INTO L_WORDER.
*---Start
        PERFORM CHECK_CONDITION USING IT_SUM L_WORDER L_ERROR.
*---End
        IT_SUM_PROD-MITU   = L_ERROR .
        IT_SUM_PROD-RP     = IT_COND-SERIAL.
        IT_SUM_PROD-COND   = IT_COND-KEY_CODE.
        MODIFY IT_SUM_PROD.
      ENDLOOP .

      " Summarize the IT_SUM_PROD-CNT..
      PERFORM CALC_ALC_PROD .
    ELSE.
      WRITE AT: /001(40) SY-SUBRC, (70) SY-UZEIT.
      WRITE AT: /001(40) 'Parallel process error'.
      WRITE AT: /045(30) IT_COND-KEY_CODE.
    ENDIF.
  ENDLOOP.

  " Summary of the Internal Table - IT_DISP / IT_DISP_PROD
  SORT IT_DISP       BY SERIAL  .
  SORT IT_DISP_PROD  BY SERIAL  .
  READ TABLE IT_DISP      INDEX 1.
  LT_DISP-SERIAL          = IT_DISP-SERIAL     .
  LT_DISP-KEY_CODE        = IT_DISP-KEY_CODE   .
  READ TABLE IT_DISP_PROD INDEX 1.
  LT_DISP_PROD-SERIAL     = IT_DISP_PROD-SERIAL.
  LT_DISP_PROD-KEY_CODE   = IT_DISP_PROD-KEY_CODE   .

  LOOP AT IT_DISP .
    IF IT_DISP-SERIAL = LT_DISP-SERIAL.
      LT_DISP-D_1 = LT_DISP-D_1 + IT_DISP-D_1 .
      LT_DISP-D01 = LT_DISP-D01 + IT_DISP-D01 .
      LT_DISP-D02 = LT_DISP-D02 + IT_DISP-D02 .
      LT_DISP-D03 = LT_DISP-D03 + IT_DISP-D03 .
      LT_DISP-D04 = LT_DISP-D04 + IT_DISP-D04 .
      LT_DISP-D05 = LT_DISP-D05 + IT_DISP-D05 .
      LT_DISP-D06 = LT_DISP-D06 + IT_DISP-D06 .
      LT_DISP-D07 = LT_DISP-D07 + IT_DISP-D07 .
      LT_DISP-D08 = LT_DISP-D08 + IT_DISP-D08 .
      LT_DISP-D09 = LT_DISP-D09 + IT_DISP-D09 .
      LT_DISP-D10 = LT_DISP-D10 + IT_DISP-D10 .
      LT_DISP-D11 = LT_DISP-D11 + IT_DISP-D11 .
      LT_DISP-D12 = LT_DISP-D12 + IT_DISP-D12 .
      LT_DISP-D13 = LT_DISP-D13 + IT_DISP-D13 .
      LT_DISP-D14 = LT_DISP-D14 + IT_DISP-D14 .
      LT_DISP-D15 = LT_DISP-D15 + IT_DISP-D15 .
      LT_DISP-D16 = LT_DISP-D16 + IT_DISP-D16 .
      LT_DISP-D17 = LT_DISP-D17 + IT_DISP-D17 .
      LT_DISP-D18 = LT_DISP-D18 + IT_DISP-D18 .
      LT_DISP-D19 = LT_DISP-D19 + IT_DISP-D19 .
      LT_DISP-D20 = LT_DISP-D20 + IT_DISP-D20 .
      LT_DISP-D21 = LT_DISP-D21 + IT_DISP-D21 .
      LT_DISP-D22 = LT_DISP-D22 + IT_DISP-D22 .
      LT_DISP-D23 = LT_DISP-D23 + IT_DISP-D23 .
      LT_DISP-REM = LT_DISP-REM + IT_DISP-REM .
      LT_DISP-TOT = LT_DISP-TOT + IT_DISP-TOT .
      LT_DISP-W01 = LT_DISP-W01 + IT_DISP-W01 .
      LT_DISP-W02 = LT_DISP-W02 + IT_DISP-W02 .
      LT_DISP-W03 = LT_DISP-W03 + IT_DISP-W03 .
      LT_DISP-W04 = LT_DISP-W04 + IT_DISP-W04 .
      LT_DISP-W05 = LT_DISP-W05 + IT_DISP-W05 .
      LT_DISP-W06 = LT_DISP-W06 + IT_DISP-W06 .
      LT_DISP-W07 = LT_DISP-W07 + IT_DISP-W07 .
      LT_DISP-W08 = LT_DISP-W08 + IT_DISP-W08 .
      LT_DISP-W09 = LT_DISP-W09 + IT_DISP-W09 .
      LT_DISP-W10 = LT_DISP-W10 + IT_DISP-W10 .
      LT_DISP-W11 = LT_DISP-W11 + IT_DISP-W11 .
      LT_DISP-W12 = LT_DISP-W12 + IT_DISP-W12 .
      LT_DISP-W13 = LT_DISP-W13 + IT_DISP-W13 .
      LT_DISP-W14 = LT_DISP-W14 + IT_DISP-W14 .
      LT_DISP-W15 = LT_DISP-W15 + IT_DISP-W15 .
      LT_DISP-W16 = LT_DISP-W16 + IT_DISP-W16 .
      LT_DISP-W17 = LT_DISP-W17 + IT_DISP-W17 .
      LT_DISP-W18 = LT_DISP-W18 + IT_DISP-W18 .
      LT_DISP-W19 = LT_DISP-W19 + IT_DISP-W19 .
      LT_DISP-W20 = LT_DISP-W20 + IT_DISP-W20 .
      LT_DISP-W21 = LT_DISP-W21 + IT_DISP-W21 .
      LT_DISP-W22 = LT_DISP-W22 + IT_DISP-W22 .
      LT_DISP-W23 = LT_DISP-W23 + IT_DISP-W23 .
      CONTINUE               .
    ELSE.
      APPEND LT_DISP.
      LT_DISP      = IT_DISP .
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE IT_DISP  LINES L_INDEX.
  IF L_INDEX > 0.
    APPEND LT_DISP.
  ENDIF.
  IT_DISP[] = LT_DISP[].

  LOOP AT IT_DISP_PROD .
    IF IT_DISP_PROD-SERIAL = LT_DISP_PROD-SERIAL.
      LT_DISP_PROD-D_1 = LT_DISP_PROD-D_1 + IT_DISP_PROD-D_1 .
      LT_DISP_PROD-D01 = LT_DISP_PROD-D01 + IT_DISP_PROD-D01 .
      LT_DISP_PROD-D02 = LT_DISP_PROD-D02 + IT_DISP_PROD-D02 .
      LT_DISP_PROD-D03 = LT_DISP_PROD-D03 + IT_DISP_PROD-D03 .
      LT_DISP_PROD-D04 = LT_DISP_PROD-D04 + IT_DISP_PROD-D04 .
      LT_DISP_PROD-D05 = LT_DISP_PROD-D05 + IT_DISP_PROD-D05 .
      LT_DISP_PROD-D06 = LT_DISP_PROD-D06 + IT_DISP_PROD-D06 .
      LT_DISP_PROD-D07 = LT_DISP_PROD-D07 + IT_DISP_PROD-D07 .
      LT_DISP_PROD-D08 = LT_DISP_PROD-D08 + IT_DISP_PROD-D08 .
      LT_DISP_PROD-D09 = LT_DISP_PROD-D09 + IT_DISP_PROD-D09 .
      LT_DISP_PROD-D10 = LT_DISP_PROD-D10 + IT_DISP_PROD-D10 .
      LT_DISP_PROD-D11 = LT_DISP_PROD-D11 + IT_DISP_PROD-D11 .
      LT_DISP_PROD-D12 = LT_DISP_PROD-D12 + IT_DISP_PROD-D12 .
      LT_DISP_PROD-D13 = LT_DISP_PROD-D13 + IT_DISP_PROD-D13 .
      LT_DISP_PROD-D14 = LT_DISP_PROD-D14 + IT_DISP_PROD-D14 .
      LT_DISP_PROD-D15 = LT_DISP_PROD-D15 + IT_DISP_PROD-D15 .
      LT_DISP_PROD-D16 = LT_DISP_PROD-D16 + IT_DISP_PROD-D16 .
      LT_DISP_PROD-D17 = LT_DISP_PROD-D17 + IT_DISP_PROD-D17 .
      LT_DISP_PROD-D18 = LT_DISP_PROD-D18 + IT_DISP_PROD-D18 .
      LT_DISP_PROD-D19 = LT_DISP_PROD-D19 + IT_DISP_PROD-D19 .
      LT_DISP_PROD-D20 = LT_DISP_PROD-D20 + IT_DISP_PROD-D20 .
      LT_DISP_PROD-D21 = LT_DISP_PROD-D21 + IT_DISP_PROD-D21 .
      LT_DISP_PROD-D22 = LT_DISP_PROD-D22 + IT_DISP_PROD-D22 .
      LT_DISP_PROD-D23 = LT_DISP_PROD-D23 + IT_DISP_PROD-D23 .
      LT_DISP_PROD-REM = LT_DISP_PROD-REM + IT_DISP_PROD-REM .
      LT_DISP_PROD-TOT = LT_DISP_PROD-TOT + IT_DISP_PROD-TOT .
      CONTINUE               .
    ELSE.
      APPEND LT_DISP_PROD.
      LT_DISP_PROD = IT_DISP_PROD .
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE IT_DISP_PROD  LINES L_INDEX.
  IF L_INDEX > 0.
    APPEND LT_DISP_PROD.
  ENDIF.
  IT_DISP_PROD[] = LT_DISP_PROD[].
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_it_COND_RP  text
*----------------------------------------------------------------------*
FORM READ_INTERNAL_TABLE .
  DATA: L_WORDER             LIKE MARA-MATNR,
        L_FLAG               TYPE C ,
        L_MAX                TYPE I ,
        L_HOURS              TYPE I ,
        L_POS                TYPE I ,
        L_LOOP               TYPE I ,
        L_SKIP               TYPE C ,
        L_CHK                TYPE P DECIMALS 3,
        L_TABIX              LIKE SY-TABIX,
        L_INDEX              LIKE SY-TABIX.

  DESCRIBE TABLE IT_DATA LINES L_MAX.
  L_LOOP = L_INDEX = 1.

  " First Days Data...
  SORT IT_MASTER BY SEQ .
  LOOP AT IT_MASTER WHERE SEQ > 80 AND SEQ < 90 .
    L_CHK = IT_MASTER-TIME / 3600         .
    L_POS = L_POS +   IT_MASTER-UPH * L_CHK   .
    IF L_POS >= L_MAX.
      WA_INDEX = L_MAX - L_INDEX.
      WA_HOUR  = L_HOURS.
      L_POS = L_MAX.
      L_FLAG = 'X' .
    ENDIF.
    L_HOURS = L_HOURS + 1 .
    LOOP AT IT_DATA FROM L_INDEX TO L_POS.
      CLEAR: IT_SUM.
      IT_SUM-HOURS      = L_HOURS            .
      IT_SUM-WORDER     = IT_DATA-WORK_ORDER .
      IT_SUM-MITU       = IT_DATA-MITU       .
      IT_SUM-EXTC       = IT_DATA-EXTC       .
      IT_SUM-INTC       = IT_DATA-INTC       .
      IT_SUM-STATUS     = IT_DATA-STATUS     .
      IT_SUM-CNT        = 1                  .
      APPEND IT_SUM.
    ENDLOOP.
    L_INDEX = L_POS + 1 .
  ENDLOOP.

** Furong on 06/06/12 for 3 shift
*  L_HOURS = 2 .
  L_HOURS = 3 .
** End on 06/06/12

  " Daily Data...
  DO 21 TIMES.
    L_TABIX = L_TABIX + 1 .
    READ TABLE IT_MASTER WITH KEY SEQ = L_TABIX.
    IF IT_MASTER-UPH = 0.
      L_HOURS = L_HOURS + 1 .
*     l_pos   = l_pos + 1  .
    ELSE.
      L_CHK = IT_MASTER-TIME / 3600 .
      IF L_FLAG = 'X'.
        EXIT.
      ENDIF.
      L_POS = L_POS +  IT_MASTER-UPH * L_CHK   .
      IF L_POS >= L_MAX.
        WA_INDEX = L_MAX - L_INDEX.
        WA_HOUR  = L_HOURS.
        L_POS = L_MAX.
        L_FLAG = 'X' .
      ENDIF.
      L_HOURS = L_HOURS + 1 .
      LOOP AT IT_DATA FROM L_INDEX TO L_POS.
        CLEAR: IT_SUM.
        IT_SUM-HOURS      = L_HOURS            .
        IT_SUM-WORDER     = IT_DATA-WORK_ORDER .
        IT_SUM-MITU       = IT_DATA-MITU       .
        IT_SUM-EXTC       = IT_DATA-EXTC       .
        IT_SUM-INTC       = IT_DATA-INTC       .
        IT_SUM-STATUS     = IT_DATA-STATUS     .
*     it_sum-rp         = it_cond-serial     .
*     it_sum-knnam      = it_cond-key_code   .
        IT_SUM-CNT        = 1                  .
*     CONCATENATE it_COND-type_alc it_COND-code INTO it_sum-code .
        APPEND IT_SUM.
      ENDLOOP.
    ENDIF.
    L_INDEX = L_POS + 1 .
  ENDDO.

  " Make the it_sum_prod...(Previous Day's Production)
  LOOP AT IT_PROD     .
    CLEAR: IT_SUM_PROD, L_WORDER.
    CONCATENATE IT_PROD-WO_SER IT_PROD-NATION IT_PROD-DEALER
           INTO L_WORDER .

    IT_SUM_PROD-WORDER = L_WORDER           .
    IT_SUM_PROD-EXTC   = IT_PROD-EXTC       .
    IT_SUM_PROD-INTC   = IT_PROD-INTC       .
    IT_SUM_PROD-CNT    = IT_PROD-RP01Q      .
    APPEND IT_SUM_PROD.
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
        L_CNT                   TYPE I  ,
        L_NO(2)                 TYPE N  ,
        L_COND                  LIKE ZTPP_PLAN_KEY-KEY_CODE,
        L_HOURS                 TYPE I  .

  " Summary of the data by Time-stamp...
  CLEAR: IT_DISP,L_CNT.

  SORT IT_SUM BY RP ASCENDING STATUS DESCENDING  COND HOURS MITU
                    ASCENDING.
  READ TABLE IT_SUM WITH KEY STATUS = '00'.
  L_NO = L_HOURS  = IT_SUM-HOURS   .
  L_COND   = IT_SUM-COND    .

  CONCATENATE 'IT_DISP-D'  L_NO     INTO L_NAME.
  ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
  CLEAR: <WA_DFIELD>.

  LOOP AT IT_SUM WHERE STATUS <> 'B'.
    IF L_HOURS = IT_SUM-HOURS .
      IF IT_SUM-MITU = SPACE .
        L_CNT = L_CNT + IT_SUM-CNT.
        CONTINUE.
      ENDIF.
    ELSE.
      <WA_DFIELD> = <WA_DFIELD> + L_CNT .
      IT_DISP-SERIAL   = IT_COND-SERIAL.
      IT_DISP-KEY_CODE = IT_COND-KEY_CODE.
      APPEND IT_DISP.    CLEAR: IT_DISP, L_CNT.

      L_NO = L_HOURS   = IT_SUM-HOURS  .
      CONCATENATE 'IT_DISP-D'  L_NO     INTO L_NAME.
      ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
      CLEAR: <WA_DFIELD>, L_CNT.
      IF IT_SUM-MITU = SPACE .
*       <wa_dfield>  = <wa_dfield> + it_sum-cnt .
        L_CNT = IT_SUM-CNT .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM  LINES  L_LINE.
  IF L_LINE > 0.
    <WA_DFIELD> = <WA_DFIELD> + L_CNT .
    IT_DISP-SERIAL   = IT_COND-SERIAL.
    IT_DISP-KEY_CODE = IT_COND-KEY_CODE.
    APPEND IT_DISP.CLEAR: IT_DISP,L_CNT.
  ENDIF.

*7JB
  READ TABLE IT_SUM WITH KEY STATUS = 'B'.
  L_NO = L_HOURS  = IT_SUM-HOURS   .
  L_COND   = IT_SUM-COND    .
  CONCATENATE 'IT_DISP-W'  L_NO     INTO L_NAME.
  ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
  CLEAR: <WA_DFIELD>,L_CNT.

  LOOP AT IT_SUM WHERE STATUS EQ 'B'.
    IF L_HOURS = IT_SUM-HOURS .
      IF IT_SUM-MITU = SPACE .
        L_CNT = L_CNT + IT_SUM-CNT.
        CONTINUE.
      ENDIF.
    ELSE.
      <WA_DFIELD> = <WA_DFIELD> + L_CNT .
      IT_DISP-SERIAL   = IT_COND-SERIAL.
      IT_DISP-KEY_CODE = IT_COND-KEY_CODE.
      COLLECT IT_DISP. CLEAR: IT_DISP,L_CNT.

      L_NO = L_HOURS   = IT_SUM-HOURS  .
      CONCATENATE 'IT_DISP-W'  L_NO     INTO L_NAME.
      ASSIGN (L_NAME)                   TO   <WA_DFIELD>.
      CLEAR: <WA_DFIELD>, L_CNT.
      IF IT_SUM-MITU = SPACE .
*       <wa_dfield>  = <wa_dfield> + it_sum-cnt .
        L_CNT = IT_SUM-CNT .
      ENDIF.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE IT_SUM  LINES  L_LINE.
  IF L_LINE > 0.
    <WA_DFIELD> = <WA_DFIELD> + L_CNT .
    IT_DISP-SERIAL   = IT_COND-SERIAL.
    IT_DISP-KEY_CODE = IT_COND-KEY_CODE.
    COLLECT IT_DISP. CLEAR: IT_DISP,L_CNT.
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
  DATA: L_NAME(40)              TYPE C  ,
        L_LINE                  TYPE I  ,
        L_CNT                   TYPE I  ,
        L_COND                  LIKE ZTPP_PLAN_KEY-KEY_CODE,
        L_HOURS                 TYPE I  .

  " Summary of the data by Time-stamp...
  SORT IT_SUM_PROD BY HOURS RP   .
  READ TABLE IT_SUM_PROD INDEX 1 .
  L_HOURS  = IT_SUM_PROD-HOURS .
  L_COND   = IT_SUM_PROD-COND    .

  LOOP AT IT_SUM_PROD.
    IF L_HOURS = IT_SUM_PROD-HOURS .
      IF IT_SUM_PROD-MITU = SPACE .
        L_CNT = L_CNT + IT_SUM_PROD-CNT.
        CONTINUE.
      ENDIF.
    ELSE.
      IT_DISP_PROD-D_1 = L_CNT .
      IT_DISP_PROD-SERIAL   = IT_COND-SERIAL.
      IT_DISP_PROD-KEY_CODE = L_COND        .
      APPEND IT_DISP_PROD.    CLEAR: IT_DISP_PROD.
      L_HOURS   = IT_SUM_PROD-HOURS  .
      IF IT_SUM_PROD-MITU = SPACE .
        IT_DISP_PROD-D_1 = IT_SUM_PROD-CNT .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM_PROD  LINES  L_LINE.
  IF L_LINE > 0.
    IT_DISP_PROD-D_1 = L_CNT .
    IT_DISP_PROD-SERIAL = IT_COND-SERIAL.
    IT_DISP_PROD-KEY_CODE = L_COND      .
    APPEND IT_DISP_PROD.
  ENDIF.
ENDFORM.                    " calc_alc_PROD

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
  DELETE FROM ZTPP_ALC_BINPUT CLIENT SPECIFIED
              WHERE MANDT = SY-MANDT.

  SORT IT_DISP BY SERIAL .
  MODIFY ZTPP_ALC_BINPUT    FROM TABLE IT_DISP .
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000 WITH 'ALC_BINPUT Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'ALC_BINPUT Update failed'.
  ENDIF.

ENDFORM.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  SUM_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUM_PLAN .
  DATA: LT_SUM       LIKE TABLE OF IT_SUM WITH HEADER LINE,
        LT_SUM_TEMP  LIKE TABLE OF IT_SUM WITH HEADER LINE,
        L_HOURS             LIKE IT_SUM-HOURS,
        L_WO                LIKE MARA-MATNR  ,
        L_MITUCNT           TYPE I           ,
        L_CNT               LIKE IT_SUM-CNT  ,
        L_EXT               LIKE IT_SUM-EXTC ,
        L_INT               LIKE IT_SUM-INTC ,
        L_SIZE              LIKE IT_SUM-CNT  .

  LT_SUM_TEMP[] = IT_SUM[] .
  CLEAR: LT_SUM[], LT_SUM  .
  DELETE IT_SUM WHERE HOURS > 16        .
  SORT IT_SUM BY HOURS WORDER EXTC INTC .
  READ TABLE IT_SUM INDEX 1.
  L_HOURS = IT_SUM-HOURS   .
  L_WO    = IT_SUM-WORDER  .
  L_EXT   = IT_SUM-EXTC    .
  L_INT   = IT_SUM-INTC    .
  LT_SUM  = IT_SUM         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_SUM  .
    IF L_HOURS = IT_SUM-HOURS AND L_WO  = IT_SUM-WORDER AND
       L_EXT   = IT_SUM-EXTC  AND L_INT = IT_SUM-INTC   .
      IF IT_SUM-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      LT_SUM-CNT     = L_CNT    .
      LT_SUM-MITUCNT = L_MITUCNT.
      APPEND         LT_SUM     .
      LT_SUM  = IT_SUM         .
      L_CNT   = 1              .
      L_HOURS = IT_SUM-HOURS   .
      L_WO    = IT_SUM-WORDER  .
      L_EXT   = IT_SUM-EXTC    .
      L_INT   = IT_SUM-INTC    .
      IF IT_SUM-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM LINES L_SIZE .
  IF L_SIZE > 0 .
    LT_SUM-CNT = L_CNT       .
    LT_SUM-MITUCNT = L_MITUCNT.
    APPEND       LT_SUM      .
  ENDIF.

  IT_SUM[] = LT_SUM_TEMP[].
  LT_SUM_TEMP[] = LT_SUM[].
  CLEAR: LT_SUM[], LT_SUM, L_CNT .
  DELETE IT_SUM WHERE HOURS <= 16        .
  SORT IT_SUM BY HOURS WORDER EXTC INTC .
  READ TABLE IT_SUM INDEX 1.
  L_HOURS = IT_SUM-HOURS   .
  L_WO    = IT_SUM-WORDER  .
  L_EXT   = IT_SUM-EXTC    .
  L_INT   = IT_SUM-INTC    .
  LT_SUM  = IT_SUM         .

  " Work Order Summarize in the same time terms.
  LOOP AT IT_SUM  .
    IF L_HOURS = IT_SUM-HOURS AND L_WO  = IT_SUM-WORDER AND
       L_EXT   = IT_SUM-EXTC  AND L_INT = IT_SUM-INTC   .
      IF IT_SUM-MITU = 'Y'      .
        L_MITUCNT = L_MITUCNT + 1    .
      ENDIF.
      L_CNT   = L_CNT + 1       .
      CONTINUE.
    ELSE.
      LT_SUM-CNT     = L_CNT    .
      LT_SUM-MITUCNT = L_MITUCNT.
      APPEND         LT_SUM     .
      LT_SUM  = IT_SUM         .
      L_CNT   = 1              .
      L_HOURS = IT_SUM-HOURS   .
      L_WO    = IT_SUM-WORDER  .
      L_EXT   = IT_SUM-EXTC    .
      L_INT   = IT_SUM-INTC    .
      IF IT_SUM-MITU = 'Y'     .
        L_MITUCNT = 1          .
      ELSE.
        CLEAR: L_MITUCNT       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_SUM LINES L_SIZE .
  IF L_SIZE > 0 .
    LT_SUM-CNT = L_CNT       .
    LT_SUM-MITUCNT = L_MITUCNT.
    APPEND       LT_SUM      .
  ENDIF.

  CLEAR: IT_SUM[], IT_SUM.
  IT_SUM[] = LT_SUM[].
  APPEND LINES OF LT_SUM_TEMP TO IT_SUM.

ENDFORM.                    " SUM_PLAN

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
    READ TABLE IT_DISP_PROD WITH KEY SERIAL  = IT_DISP-SERIAL .
    IF SY-SUBRC = 0.
      IT_DISP-D_1 = IT_DISP_PROD-D_1 .
      DELETE IT_DISP_PROD WHERE SERIAL  = IT_DISP-SERIAL .
    ELSE.
      CLEAR: IT_DISP-D_1 .
    ENDIF.

    MODIFY IT_DISP.
  ENDLOOP.

  " Remain Fiedls Insert.....
  LOOP AT IT_DISP_PROD .
    CLEAR: IT_DISP.
    MOVE-CORRESPONDING IT_DISP_PROD TO IT_DISP.
    IT_DISP-D_1 = IT_DISP_PROD-D_1.
    APPEND IT_DISP.
  ENDLOOP.
  CLEAR: IT_DISP_PROD, IT_DISP_PROD[].
ENDFORM.                    " INSERT_FIELD_VALS

*&---------------------------------------------------------------------*
*&      Form  GET_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CONDITION.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COND
    FROM ZTPP_PLAN_KEY .
ENDFORM.                    " GET_CONDITION

*&---------------------------------------------------------------------*
*&      Form  GET_COND_PARSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COND  text
*----------------------------------------------------------------------*
FORM GET_COND_PARSING  .
  DATA: LT_WORDER lIKE TABLE OF ZSPP_MASTER_ASUP
                  WITH HEADER LINE,
        LT_COND  LIKE TABLE OF ZSPP_CONDITION   WITH HEADER LINE,
        L_COND    LIKE ZSPP_CONDITION-VALS ,
        L_SUCCESS, L_FAIL.

  CLEAR: IT_PHARSE, IT_PHARSE[].
  L_COND = IT_COND-COND.

  CALL FUNCTION 'Z_FPP_COMPILE_CONDITION'
       EXPORTING
            I_COND          = L_COND
            I_CHECK         = 'X'
       IMPORTING
            O_SUCCESS       = L_SUCCESS
            O_FAIL          = L_FAIL
       TABLES
            T_WORDER        = LT_WORDER
            T_COND          = LT_COND
       EXCEPTIONS
            CONDITION_ERROR = 1
            ERR_PAREN       = 2
            ERR_OPERATION   = 3
            ERR_RELATION    = 4
            ERR_VALUES      = 5
            ERR_FIELDS      = 6
            OTHERS          = 7.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    LOOP AT LT_COND   .
      MOVE-CORRESPONDING LT_COND  TO IT_PHARSE.
      PERFORM GET_ATINN  USING LT_COND-VALS   .
      APPEND IT_PHARSE.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_COND_PARSING

*&---------------------------------------------------------------------*
*&      Form  get_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ATINN               USING PA_VALS  .
  DATA: LW_CABN              LIKE CABN      ,
        L_SIZE               TYPE I         ,
        L_VALS               LIKE ZSPP_CONDITION-VALS,
        L_ATNAM              LIKE CABN-ATNAM.

  L_ATNAM = IT_PHARSE-STRING .
  L_VALS  = PA_VALS          .
  L_SIZE  = STRLEN( L_VALS ) - 2 .
  L_VALS  = L_VALS+1(L_SIZE) .
* SHIFT   L_VALS  LEFT       .
* SHIFT   L_VALS  RIGHT      .

  SELECT SINGLE * INTO LW_CABN
    FROM CABN
   WHERE ATNAM = L_ATNAM .

  IT_PHARSE-ATINN = LW_CABN-ATINN .
  CASE LW_CABN-ATFOR.
    WHEN 'CHAR' .
      IT_PHARSE-ATWRT = L_VALS       .
    WHEN 'CURR' .
    WHEN 'DATE' OR 'NUM'  .
      IT_PHARSE-ATFLV = L_VALS       .
    WHEN 'TIME' .
    WHEN 'UDEF' .
  ENDCASE.
ENDFORM.                    " get_atinn

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
   WHERE ARBPL = 'B'   .
ENDFORM.                    " READ_SHOP_CALID

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
  CLEAR: L_COUNT.                 L_DATE = P_DATES  .
  PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE .
  IF L_DATE = P_DATES .
    IT_MASTER-SEQ    = '80'.       IT_MASTER-DATE   = L_DATE .
    PERFORM GET_DAY          USING L_DATE IT_MASTER-DAY  .
    PERFORM GET_WORKTIME1    USING L_DATE IT_MASTER-TIME
                                   IT_MASTER-DAY .
*   PERFORM get_uph          USING l_date it_master-uph it_master-shift.
  ELSE.
    L_DATE = P_DATES .
    PERFORM GET_DAY         USING L_DATE IT_MASTER-DAY  .
    IT_MASTER-SEQ    = '81'.       IT_MASTER-DATE   = L_DATE .
    APPEND IT_MASTER.       CLEAR: IT_MASTER.
  ENDIF.

  " From D+1 Day To D+21 Day..
  " (Only Working Dates in FACTORY-Calendar)
  L_DATE = P_DATES .
  DO 21 TIMES.
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE.
    PERFORM GET_DAY          USING L_DATE IT_MASTER-DAY  .
    PERFORM GET_WORKING_TIME USING L_DATE IT_MASTER-TIME
                                   IT_MASTER-DAY.
    PERFORM GET_UPH          USING L_DATE IT_MASTER-UPH
                                   IT_MASTER-SHIFT.
    IT_MASTER-SEQ    = L_COUNT.
    IT_MASTER-DATE   = L_DATE .
    APPEND IT_MASTER.  CLEAR: IT_MASTER.
  ENDDO.
ENDFORM.                    " SET_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM GET_WORKING_TIME USING    PA_WDATE  PA_WKTIME  PA_DAY.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA LIKE TABLE OF ZVPP_CAPACITY WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL = 'B'
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
    L_WTIME = L_WTIME + L_EINZT    .
  ENDLOOP.
  PA_WKTIME = L_WTIME .
ENDFORM.                    " GET_WORKING_TIME

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM GET_WORKTIME1 USING    PA_WDATE  PA_WKTIME  PA_DAY.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_FLAG        TYPE C ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA LIKE TABLE OF ZVPP_CAPACITY WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL = 'B'
     AND DATUB >= PA_WDATE .

  SORT LT_CAPA BY DATUB TAGNR SCHNR .
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
    IT_MASTER-TIME  = L_EINZT      .
    IT_MASTER-SHIFT = LT_CAPA-SCHNR.
    IT_MASTER-SEQ   = 80 + LT_CAPA-SCHNR .
    PERFORM GET_UPH USING PA_WDATE IT_MASTER-UPH IT_MASTER-SHIFT.
    APPEND IT_MASTER.
    L_FLAG = 'X' .
  ENDLOOP.
  IF L_FLAG = SPACE.
    PERFORM GET_UPH USING PA_WDATE IT_MASTER-UPH IT_MASTER-SHIFT.
    APPEND IT_MASTER.
  ENDIF.
  CLEAR: IT_MASTER.
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
FORM GET_UPH USING    PA_WDATE  PA_UPH  PA_SHIFT .
  DATA: W_UPH  LIKE ZTPP_STATUS-UPH.
  CALL FUNCTION 'Z_FPP_GET_UPH'
       EXPORTING
            DATE  = PA_WDATE
            SHIFT = PA_SHIFT
            SHOP  = 'B'
       IMPORTING
            UPH   = W_UPH.
  PA_UPH  = W_UPH.

*  DATA lw_ld          LIKE zvpp_ld .
*  DATA: LT_LD   LIKE ZVPp_LD OCCURS 0 WITH HEADER LINE.
*
*  IF pa_shift IS INITIAL .
** requested by MY hur changed by chris
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND arbpl     = 'B'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = 'B'      .
*
** end of change on 06/13/2055
*  ELSE.
** requested by MY hur changed by chris
*
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND ld_shift  = pa_shift
**       AND arbpl     = 'B'      .
*   SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND ld_shift  = pa_shift
*       AND arbpl     = 'B'      .
*
** end of change on 06/13/2055
*  ENDIF.
** added by chris on 06/13/2005
*    loop at lt_ld.
*     lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
*     lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
*    endloop.
** end of add
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH
*&---------------------------------------------------------------------*
*&      Form  GET_DATa_ztpp_pmt07jb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_ZTPP_PMT07JB.
  DATA :IT_7JB_A LIKE ZTPP_PMT07JB_A OCCURS 0 WITH HEADER LINE.
  DATA :L_HOURS TYPE I,
        Z_WORK_ORDER LIKE ZTPP_INPUT_PLAN-WORK_ORDER.

  DATA: L_FSC LIKE ZTPP_WOSUM-FSC,
         L_WORDER LIKE ZTPP_WOSUM-WO_SER.
  REFRESH IT_7JB_A.

  SELECT * INTO TABLE IT_7JB_A
   FROM ZTPP_PMT07JB_A
    WHERE GUBB EQ 'B'.

  SORT IT_7JB_A BY SQDT  .           "UD1K917193

  LOOP AT IT_7JB_A .
    CLEAR: IT_SUM.
    ON CHANGE OF IT_7JB_A-SQDT.
      L_HOURS = L_HOURS + 1 .
    ENDON.

*    IF it_7jb_a-ordr+0(1) = 'F'.
*      CONCATENATE it_7jb_a-moye it_7jb_a-dist it_7jb_a-bmdl
*                   INTO l_fsc.
*      CONCATENATE l_fsc it_7jb_a-ocnn INTO l_fsc
*              SEPARATED BY space.
*      SELECT SINGLE wo_ser INTO l_worder
*        FROM ztpp_wosum
*       WHERE extc = it_7jb_a-extc
*         AND intc = it_7jb_a-intc
*         AND version = it_7jb_a-vers
*         AND fsc = l_fsc.
*      IF sy-subrc = 0.
*        CONCATENATE l_worder it_7jb_a-dist INTO z_work_order.
*      ENDIF.
*    ELSE.
    CONCATENATE IT_7JB_A-ORDR IT_7JB_A-DIST INTO Z_WORK_ORDER.
*    ENDIF.
    IT_SUM-HOURS      = L_HOURS.
    IT_SUM-WORDER     = Z_WORK_ORDER.
    IT_SUM-MITU       = ' '.
    IT_SUM-EXTC       = IT_7JB_A-EXTC.
    IT_SUM-INTC       = IT_7JB_A-INTC.
    IT_SUM-STATUS     = 'B'.
*     it_sum-rp         = it_cond-serial     .
*     it_sum-knnam      = it_cond-key_code   .
    IT_SUM-CNT        =  IT_7JB_A-PQTY.
    APPEND IT_SUM.    CLEAR : IT_SUM,Z_WORK_ORDER.
  ENDLOOP.


ENDFORM.                    " GET_DATa_ztpp_pmt07jb
*&---------------------------------------------------------------------*
*&      Form  check_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ERROR  text
*----------------------------------------------------------------------*
FORM CHECK_CONDITION USING P_SUM LIKE IT_SUM
                           P_VALUE   P_ERROR.
*      LOOP AT it_pharse       .
*        IF it_pharse-oper = '@'.
*          CLEAR: l_error.
*        ENDIF.
*        IF l_error = 'X'.
*          CONTINUE.
*        ENDIF.
*        SELECT SINGLE *
*          FROM ausp
*         WHERE objek = l_worder
*           AND atinn = it_pharse-atinn
*           AND klart = '001'
*           AND atwrt = it_pharse-atwrt
*           AND atflv = it_pharse-atflv .
*
*        IF sy-subrc NE 0.
*          l_error  = 'X'.
*        ENDIF.
*       ENDLOOP.
  DATA : F_OPER LIKE IT_PHARSE-OPER.
  CLEAR F_OPER.
  LOOP AT IT_PHARSE.
* In case of 'Not'
    IF IT_PHARSE-OP = '!'.
      IF IT_SUM-STATUS <> 'B'.   "
        SELECT SINGLE *  FROM AUSP
         WHERE OBJEK = P_VALUE
           AND ATINN = IT_PHARSE-ATINN
           AND KLART = '001'
           AND ATWRT = IT_PHARSE-ATWRT
           AND ATFLV = IT_PHARSE-ATFLV .
        IF SY-SUBRC EQ 0.
          P_ERROR  = 'X'.
          CONTINUE.
        ELSE.
          CLEAR P_ERROR.
        ENDIF.
      ELSE.
* by Daniel on 04/28/11 {
*        IF p_value(1) EQ 'E'.
        IF p_value(1) EQ 'E' OR p_value(1) EQ 'D'.
* }
          SELECT SINGLE *  FROM AUSP
            WHERE OBJEK = P_VALUE
              AND KLART = '001'.
          IF SY-SUBRC = 0.
            SELECT SINGLE *  FROM AUSP
             WHERE OBJEK = P_VALUE
               AND ATINN = IT_PHARSE-ATINN
               AND KLART = '001'
               AND ATWRT = IT_PHARSE-ATWRT
               AND ATFLV = IT_PHARSE-ATFLV .
            IF SY-SUBRC EQ 0.
              P_ERROR  = 'X'.
              CONTINUE.
            ELSE.
              CLEAR P_ERROR.
            ENDIF.
          ELSE.
            P_ERROR  = 'X'.
            CONTINUE.
          ENDIF.
        ELSE. " F
          SELECT SINGLE * FROM ZTPP_FWO
            WHERE WORDER EQ P_VALUE(18).

          SELECT SINGLE *  FROM AUSP
            WHERE OBJEK = ZTPP_FWO-O_WORDER(18)
              AND ATINN = IT_PHARSE-ATINN
              AND KLART = '001'
              AND ATWRT = IT_PHARSE-ATWRT
              AND ATFLV = IT_PHARSE-ATFLV .
          IF SY-SUBRC EQ 0.
            P_ERROR  = 'X'.
            CONTINUE.
          ELSE.
            CLEAR P_ERROR.
          ENDIF.
        ENDIF.
      ENDIF.
* In case of 'OR'
    ELSE.
*      IF it_pharse-oper = '@' OR f_oper EQ 'X'.
*        CLEAR : p_error. f_oper = 'X'.
*      ENDIF.
*      IF p_error = 'X'.
*        CONTINUE.
*      ENDIF.
      IF F_OPER EQ 'X' AND P_ERROR EQ ' '.
        READ TABLE IT_PHARSE WITH KEY OPER =  '@'.
        IF SY-SUBRC = 0.
          EXIT.
        ENDIF.
      ENDIF.
      IF P_ERROR = 'X'.
        READ TABLE IT_PHARSE WITH KEY OPER =  '&'.
        IF SY-SUBRC = 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF IT_SUM-STATUS <> 'B'.
        SELECT SINGLE *  FROM AUSP
         WHERE OBJEK = P_VALUE
           AND ATINN = IT_PHARSE-ATINN
           AND KLART = '001'
           AND ATWRT = IT_PHARSE-ATWRT
           AND ATFLV = IT_PHARSE-ATFLV .
        IF SY-SUBRC NE 0.
          P_ERROR  = 'X'.
        ELSE.
          CLEAR P_ERROR.F_OPER = 'X'.
*          EXIT.
        ENDIF.
      ELSE."WEEKLY
* by Daniel on 04/28/11 {
*        IF p_value(1) EQ 'E'.
        IF p_value(1) EQ 'E' OR p_value(1) EQ 'D'.
* }
          SELECT SINGLE *  FROM AUSP
            WHERE OBJEK = P_VALUE
             AND KLART = '001'.
          IF SY-SUBRC = 0.
            SELECT SINGLE *  FROM AUSP
             WHERE OBJEK = P_VALUE
               AND ATINN = IT_PHARSE-ATINN
               AND KLART = '001'
               AND ATWRT = IT_PHARSE-ATWRT
               AND ATFLV = IT_PHARSE-ATFLV .
            IF SY-SUBRC NE 0.
              P_ERROR  = 'X'. CONTINUE.
            ELSE.
              CLEAR P_ERROR.  F_OPER = 'X'.
            ENDIF.
          ELSE.
            P_ERROR  = 'X'.
            CONTINUE.
          ENDIF.
        ELSE. "F
          SELECT SINGLE * FROM ZTPP_FWO
            WHERE WORDER EQ P_VALUE(18).
          IF SY-SUBRC = 0.
            SELECT SINGLE *  FROM AUSP
              WHERE OBJEK = ZTPP_FWO-O_WORDER(18)
                AND ATINN = IT_PHARSE-ATINN
                AND KLART = '001'
                AND ATWRT = IT_PHARSE-ATWRT
                AND ATFLV = IT_PHARSE-ATFLV .

            IF SY-SUBRC <> 0.
              P_ERROR  = 'X'.
              CONTINUE.
            ELSE.
              CLEAR P_ERROR.F_OPER = 'X'.
            ENDIF.
          ELSE.
            P_ERROR  = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_condition

*---------------------------------------------------------------------*
*       FORM check_holding_car                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
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

** Furong on 05/22/14 for remove holding car, requested by Mr. Bae (
*  MODIFY ZTPP_HOLD_CAR FROM TABLE IT_HOLD.
** )
  IF SY-SUBRC = 0.
    MESSAGE S000 WITH TEXT-003.
    COMMIT WORK.
  ELSE.
    MESSAGE S000 WITH TEXT-004.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " check_holding_car

*---------------------------------------------------------------------*
*       FORM task_receive                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TASKNAME                                                      *
*---------------------------------------------------------------------*
FORM TASK_RECEIVE USING TASKNAME.

  DATA: LT_SUM_TEMP LIKE TABLE OF IT_SUM WITH HEADER LINE.
  RECEIVE RESULTS FROM FUNCTION 'ZPP_CHECK_CONDITION'
*  IMPORTING
*      p_error        = w_error
*       p_tabix_o     = l_tabix
  TABLES
      P_SUM   = LT_SUM_TEMP
  EXCEPTIONS
          COMMUNICATION_FAILURE       = 1
            SYSTEM_FAILURE              = 2
            RESOURCE_FAILURE            = 3 .
  IF SY-SUBRC NE 0.
    EXCEP_FLAG = 'X'.
    MESSAGE S000 WITH 'Parallel processing error' SY-SUBRC.
    EXIT.
  ENDIF.
  RCV_JOBS = RCV_JOBS + 1.

  APPEND LINES OF LT_SUM_TEMP TO IT_SUM_P.
  CLEAR: LT_SUM_TEMP, LT_SUM_TEMP[].

ENDFORM.                    " TASK_RECEIVE
