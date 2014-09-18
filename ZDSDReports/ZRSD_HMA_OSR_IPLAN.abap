************************************************************************
* Program Name      : ZAPP903R_INPUT_PLAN
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : APP903: VEHICLE SCHEDULING
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 11/11/2004 Chris LI    UD1K912950   Exclude the MITU cars from the
*                                      input plan.Requested by MY Hur.
* 05/10/2005 Chris       UD1K915976   commit database update
* Change : Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT  ZAPP903R_INPUT_PLAN   MESSAGE-ID ZMPP.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ZTPP_COMMON_VALS,ZTPP_INPUT_OSR.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF IT_LOG        OCCURS 0,
        PRO_TYPE             TYPE N  ,
        LOG_SEQ              TYPE I  ,
        LOG_TYPE             TYPE C  ,
        LOG_MSG              LIKE BAPIRET2-MESSAGE ,
      END OF IT_LOG.
DATA: BEGIN OF IT_TEMP    OCCURS 0,
        OBJEK             LIKE AUSP-OBJEK,      " EQUI-EQUNR
        ATINN             LIKE AUSP-ATINN,      " CHARACTERISTIC
        ATWRT             LIKE AUSP-ATWRT,      " CHARACTERISTIC VAL
        ATFLV             LIKE AUSP-ATFLV,      " Date & Number  VAL
      END OF IT_TEMP .

DATA: BEGIN OF IT_DATA        OCCURS 0,
        OBJEK                 LIKE AUSP-OBJEK.      " Vehicle Code
        INCLUDE STRUCTURE     ZTPP_INPUT_OSR.
DATA:   USAGE                 TYPE C         ,
      END OF IT_DATA.
*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: WA_UPH                TYPE I,
      WA_MODEL              TYPE ZPP_MODEL,
      WA_DATES              TYPE D,
      WA_PRJ                TYPE C,
      WA_CLOSE              TYPE C,
      WA_WBS                TYPE C,
      WA_SERIAL           LIKE ZTPP_DVRT1-SERIAL,
      WA_DATA             LIKE IT_DATA                            .
*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: C_KEY1(18)            VALUE 'INPUT_PLAN',
      C_KEY2(18)            VALUE 'BODY_INPUT',
      C_KEY3(18)            VALUE 'TRIM_INPUT',
      C_KEY4(18)            VALUE 'SEQ_SUM01' ,
      C_KEY5(18)            VALUE 'SEQ_SUM02' ,
      C_KEY6(18)            VALUE 'SEQ_SUM03' ,
      C_KEY7(18)            VALUE 'SEQ_PROD'  ,
      C_KEY8(18)            VALUE 'WIRE_REP'  .


*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_RUN         AS CHECKBOX  DEFAULT 'X',
*            p_real        AS CHECKBOX  DEFAULT ' ',
            P_TEST        AS CHECKBOX  DEFAULT 'X',
            P_LOG         AS CHECKBOX  DEFAULT 'X',
*            p_mitu         AS CHECKBOX  DEFAULT ' ',
*            p_fwo          AS CHECKBOX  DEFAULT ' '.
            P_MSG         LIKE ZTPP_COMMON_VALS-ITEM3,
            P_WBS(2)      TYPE N        NO-DISPLAY,
            P_PRJ(2)      TYPE N        NO-DISPLAY,
            P_CLOSE       TYPE SPMON    NO-DISPLAY.
PARAMETERS: P_SRVGRP LIKE RZLLITAB-CLASSNAME "OBLIGATORY
                     DEFAULT 'PG_INP' NO-DISPLAY.
PARAMETERS: W_MAX_W TYPE I DEFAULT 10.

SELECTION-SCREEN END OF BLOCK B3.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*PARAMETERS :
*             p_short      AS CHECKBOX   ,
*             p_long       AS CHECKBOX   ,
*             p_week       AS CHECKBOX   ,
*             p_binput     AS CHECKBOX  DEFAULT 'X' .
*             p_tinput     AS CHECKBOX   ,
*             p_sum        AS CHECKBOX   .

SELECTION-SCREEN END OF BLOCK B2.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  PERFORM SETTING_DEFAULT."???

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM CLEAR_VARIABLE .
  PERFORM RUN_PROCESS.
  PERFORM CALL_SUBPROCESS.

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM DISPLAY_RESULT .

*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VARIABLE.
  IF P_RUN = 'X'   .
    GET TIME.
    IF P_LOG EQ 'X'.
      WRITE AT: /001(46)
      'Start: INPUT-PLAN & Prod. Result of prev. day.' ,
                                      048(11) SY-DATUM,
                                      060(10) SY-UZEIT.
    ENDIF.
  ENDIF.

  " Save the Input Values...
  IF WA_WBS = 'X'.
    ZTPP_COMMON_VALS-JOBS  = SY-REPID .
    ZTPP_COMMON_VALS-KEY2  = 'WBS'    .
    ZTPP_COMMON_VALS-ITEM4 = P_WBS    .
    ZTPP_COMMON_VALS-DESCRIPTION = TEXT-003.
*    INSERT INTO ztpp_common_vals VALUES ztpp_common_vals .
  ELSE.
*    UPDATE ztpp_common_vals   SET item4 = p_wbs
*                            WHERE jobs  = sy-repid
*                              AND key2  = 'WBS'    .
  ENDIF.

  IF WA_PRJ = 'X'.
    ZTPP_COMMON_VALS-JOBS  = SY-REPID .
    ZTPP_COMMON_VALS-KEY2  = 'PRJ'    .
    ZTPP_COMMON_VALS-ITEM4 = P_PRJ    .
    ZTPP_COMMON_VALS-DESCRIPTION = TEXT-004.
*    INSERT INTO ztpp_common_vals VALUES ztpp_common_vals .
  ELSE.
*    UPDATE ztpp_common_vals   SET item4 = p_prj
*                            WHERE jobs  = sy-repid
*                              AND key2  = 'PRJ'    .
  ENDIF.

  IF WA_CLOSE = 'X'.
    ZTPP_COMMON_VALS-JOBS  = SY-REPID   .
    ZTPP_COMMON_VALS-KEY2  = 'CLOSE'    .
    ZTPP_COMMON_VALS-ITEM4 = P_CLOSE    .
    ZTPP_COMMON_VALS-DESCRIPTION = TEXT-005.
*    INSERT INTO ztpp_common_vals VALUES ztpp_common_vals .
  ELSE.
*    UPDATE ztpp_common_vals   SET item4 = p_close
*                            WHERE jobs  = sy-repid
*                              AND key2  = 'CLOSE'    .
  ENDIF.

  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " CLEAR_VARIABLE

*&---------------------------------------------------------------------*
*&      Form  SETTING_DEFAULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETTING_DEFAULT.
* Considering : If 2 Model is running, The UPH is input the 2 Values..
*               If More Model is running, How to programming????
  DATA: L_JOBS                TYPE PROGRAMM  ,
        L_LRATE               LIKE LDLH-LRATE.

  GET PARAMETER ID 'ZRUN' FIELD P_RUN.
  GET TIME.    CLEAR: WA_PRJ, WA_WBS.
  WA_MODEL = 'EMF'   .
  WA_DATES = SY-DATUM.
  P_RUN = 'X'.
  P_TEST = 'X'.
  SELECT SINGLE LRATE INTO L_LRATE
    FROM CRHD AS C INNER JOIN LDLH AS L
      ON C~OBJID = L~LNID
   WHERE C~ARBPL = '1'  .
  IF SY-SUBRC NE 0.
    WA_UPH = 63   .
  ELSE.
    WA_UPH = L_LRATE .
  ENDIF.

  CLEAR: ZTPP_COMMON_VALS.  L_JOBS = SY-REPID.
  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = L_JOBS
     AND KEY2 = 'WBS' .

  IF SY-SUBRC = 0.
    P_WBS    = ZTPP_COMMON_VALS-ITEM4 .
  ELSE.
    WA_WBS   = 'X'.
    P_WBS    = 4 .
  ENDIF.

  CLEAR: ZTPP_COMMON_VALS.  L_JOBS = SY-REPID.
  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = L_JOBS
     AND KEY2 = 'PRJ' .

  IF SY-SUBRC = 0.
    P_PRJ    = ZTPP_COMMON_VALS-ITEM4 .
  ELSE.
    WA_PRJ   = 'X'.
    P_PRJ    = 20.
  ENDIF.

  CLEAR: ZTPP_COMMON_VALS.  L_JOBS = SY-REPID.
  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = L_JOBS
     AND KEY2 = 'CLOSE'  .

  IF SY-SUBRC = 0.
    P_CLOSE  = ZTPP_COMMON_VALS-ITEM4 .
  ELSE.
    WA_CLOSE  = 'X'.
    P_CLOSE  = SY-DATUM(6)            .
  ENDIF.
ENDFORM.                    " SETTING_DEFAULT

*&---------------------------------------------------------------------*
*&      Form  RUN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RUN_PROCESS.
  DATA: L_KALID             LIKE KAKO-KALID                          ,
        L_DATE              TYPE D .

  IF P_RUN = 'X' .
    PERFORM SAVE_RECORD_COMMON_VALS  USING C_KEY1.
*    CALL FUNCTION 'Z_FPP_CREATE_INPUTPLAN'
*         EXPORTING
*              I_DEL = 'X'.

    DATA WA_UPH TYPE NUM03 VALUE '063'.


    PERFORM CREATE_INPUTPLAN  USING  'X' .

*    " Create the Previous Working Date's Prod. result.
    L_DATE = WA_DATES - 1 .
    PERFORM READ_SHOP_CALID   USING L_KALID.
    PERFORM READ_WORKING_DATE USING '-'  L_KALID  L_DATE  .

*    SUBMIT zapp903r_daily_production
*           WITH p_wdate = l_date      AND RETURN .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              WAIT = 'X'.

    GET TIME.
    IF P_LOG EQ 'X'.
      WRITE AT: /001(40) 'End of the Data Creation ...........' ,
                 041(11) SY-DATUM,
                 053(10) SY-UZEIT.
    ENDIF.
*---Start wskim: check holding car
    PERFORM CHECK_HOLDING_CAR.
*---End
  ENDIF.

ENDFORM.                    " RUN_PROCESS

*&---------------------------------------------------------------------*
*&      Form  CALL_SUBPROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_SUBPROCESS.


*  " Call the Sub-Process.. Call Transaction...
*  IF p_real   = 'X' .
*    GET TIME.
*    WRITE AT: /001(40) 'Start : ZAPP903R_ALC_SUM - MIP DATA..' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*
*    SUBMIT zapp903r_alc_sum    AND RETURN .
*
*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*  ENDIF.
*
*  IF p_short  = 'X' .
*    GET TIME.
*    WRITE AT: /001(40) 'Start : ZAPP903R_SEQ_SUM01 - 3 DAY   ' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*
*    SUBMIT zapp903r_seq_sum01_v1          " ZAPP903R_ALC_SHORT
*               WITH  p_dates = wa_dates
*               WITH  p_test  = p_test
*               WITH  p_mitu  = p_mitu
*               WITH  p_wbs   = p_wbs
*               WITH  p_prj   = p_prj      AND RETURN          .
*
*    SUBMIT zapp903r_wire_hour_v1          " zapp903r_WIRE_HOURLY
*               WITH  p_dates = wa_dates
*               WITH  p_mitu  = p_mitu
*               WITH  p_test  = p_test
*               WITH  p_wbs   = p_wbs
*               WITH  p_prj   = p_prj      AND RETURN          .
*
*    PERFORM save_record_common_vals  USING c_key4             .
*
*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*  ENDIF.
*
*  IF p_long   = 'X' .
*    GET TIME.
*    WRITE AT: /001(40) 'Start : ZAPP903R_SEQ_SUM02 - 3 WEEK. ' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*
*    SUBMIT zapp903r_seq_sum02_v1          " ZAPP903R_ALC_LONG
*               WITH  p_dates = wa_dates
*               WITH  p_test  = p_test
*               WITH  p_mitu  = p_mitu
*               WITH  p_wbs   = p_wbs
*               WITH  p_prj   = p_prj
*               with  p_fwo    = p_fwo AND RETURN          .
*
*    SUBMIT zapp903r_wire_day_v1          " ZAPP903R_WIRE_DAY
*               WITH  p_dates = wa_dates
*               WITH  p_test  = p_test
*               WITH  p_mitu  = p_mitu
*               WITH  p_wbs   = p_wbs
*               WITH  p_prj   = p_prj
*               with  p_fwo    = p_fwo AND RETURN.
*
*    PERFORM save_record_common_vals  USING c_key5              .
*
*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*  ENDIF.
*
*  IF p_week   = 'X' .
*    GET TIME.
*    WRITE AT: /001(40) 'Start : ZAPP903R_SEQ_SUM03 - 21 WEEK.' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*    SUBMIT zapp903r_seq_sum03
*               WITH  p_dates = wa_dates
*               WITH  p_test  = p_test
*               WITH  p_mitu  = p_mitu
*               with  p_fwo    = p_fwo AND RETURN.
*
*    PERFORM save_record_common_vals  USING c_key6              .
*
*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*  ENDIF.

*  IF p_binput = 'X'.
*
*    SUBMIT     zapp903r_alc_binput_p
*               WITH  p_dates  = wa_dates
*               WITH  p_mitu   = p_mitu
*               with  p_fwo    = p_fwo
*               with  p_srvgrp = p_srvgrp
*               with  w_max_w =  w_max_w
*               AND RETURN EXPORTING LIST TO MEMORY .
*
*    perform display_list.
*
*    PERFORM save_record_common_vals  USING c_key2              .
*
*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*  ENDIF.

*  IF p_tinput = 'X'.
**    GET TIME.
**    WRITE AT: /001(40) 'Start : ZAPP903R_ALC_TINPUT - Trim   ' ,
**               041(11) sy-datum,
**               053(10) sy-uzeit.
*
*    SUBMIT  zapp903r_alc_tinput_p
*               WITH  p_dates  = wa_dates
*               WITH  p_test   = p_test
*               WITH  p_mitu   = p_mitu
*               WITH  p_fwo = p_fwo
*               with  p_srvgrp = p_srvgrp
*               with  w_max_w =  w_max_w
*               AND RETURN  EXPORTING LIST TO MEMORY .
*
*    perform display_list.
*
*    PERFORM save_record_common_vals  USING c_key3              .
*
**    GET TIME.
**    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
**               041(11) sy-datum,
**               053(10) sy-uzeit.
*  ENDIF.
*
*  IF p_sum    = 'X'.
*    GET TIME.
*    WRITE AT: /001(40) 'Start : ZAPP903R_PROD_REPORT         ' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*
*    SUBMIT  zapp903r_prod_result
*               WITH  p_spmon  = p_close    AND RETURN          .
*
*    PERFORM save_record_common_vals  USING c_key7              .
*
*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*  ENDIF.
ENDFORM.                    " CALL_SUBPROCESS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_RESULT.

ENDFORM.                    " DISPLAY_RESULT

*&---------------------------------------------------------------------*
*&      Form  SET_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  read_working_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0406   text
*      -->P_0407   text
*      -->P_L_DATE  text
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
ENDFORM.                    " read_working_date

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
*&      Form  SAVE_RECORD_COMMON_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_RECORD_COMMON_VALS  USING PA_KEY .
  DATA: L_REPID             LIKE SY-REPID.

  " Check the Table Record: ZTPP_COMMON_VALS-ITEM3 for INPUT_PLAN
  L_REPID = SY-REPID.
  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = L_REPID
     AND KEY2 = PA_KEY    .

  IF SY-SUBRC NE 0.
    CLEAR: ZTPP_COMMON_VALS.
    ZTPP_COMMON_VALS-JOBS  = L_REPID.
    ZTPP_COMMON_VALS-KEY2  = PA_KEY .
  ENDIF.

  ZTPP_COMMON_VALS-DATES = WA_DATES.
  ZTPP_COMMON_VALS-TIMES = SY-UZEIT.
  ZTPP_COMMON_VALS-ITEM3 = P_MSG   .
*  MODIFY ztpp_common_vals .
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000 WITH 'Common_vals Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'Common_vals Update failed'.
  ENDIF.

ENDFORM.                    " SAVE_RECORD_COMMON_VALS
*&---------------------------------------------------------------------*
*&      Form  check_holding_car
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_HOLDING_CAR.
  DATA : IT_HOLD LIKE ZTPP_HOLD_CAR OCCURS 0 WITH HEADER LINE,
         IT_DATA LIKE ZTPP_INPUT_OSR OCCURS 0 WITH HEADER LINE.

  REFRESH :IT_HOLD,IT_DATA.

  SELECT * INTO TABLE IT_HOLD FROM ZTPP_HOLD_CAR
         WHERE  ( STATUS NE 'D' )
             OR ( STATUS NE 'P' ).

  SELECT * INTO TABLE IT_DATA FROM ZTPP_INPUT_OSR
         WHERE MITU <> 'Y'
           AND STATUS < '06'.

  LOOP AT IT_DATA.
    IF IT_DATA-WORK_ORDER+12(2) = 'XX' OR
       IT_DATA-WORK_ORDER+12(2) = 'XY' .
      DELETE IT_DATA.
    ENDIF.
  ENDLOOP.


  LOOP AT IT_HOLD WHERE RES_DATE > WA_DATES.
    READ TABLE IT_DATA WITH KEY MODL = IT_HOLD-MODL
                            BODY_SER = IT_HOLD-BODY_SER.
    IF SY-SUBRC = 0.
      IT_HOLD-STATUS = 'W'.
      MODIFY IT_HOLD FROM IT_HOLD.

      UPDATE ZTPP_INPUT_OSR SET T = 'HOLD'
               WHERE MODL = IT_HOLD-MODL
                 AND BODY_SER = IT_HOLD-BODY_SER.

    ENDIF.
  ENDLOOP.

  LOOP AT IT_HOLD WHERE RES_DATE <= WA_DATES.
    IF IT_HOLD-RES_DATE =  WA_DATES.
      IT_HOLD-STATUS = 'D'.
    ELSE.
      IT_HOLD-STATUS = 'P'.
    ENDIF.
    MODIFY IT_HOLD FROM IT_HOLD.
  ENDLOOP.

*  MODIFY ztpp_hold_car FROM TABLE it_hold.
  IF SY-SUBRC = 0.
    MESSAGE S000 WITH TEXT-003.
    COMMIT WORK.
  ELSE.
    MESSAGE S000 WITH TEXT-004.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " check_holding_car

*---------------------------------------------------------------------*
*       FORM create_inputplan                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_FLAG                                                       *
*---------------------------------------------------------------------*
FORM CREATE_INPUTPLAN  USING PA_FLAG.
  PERFORM DELETE_OLD_INPUTPLAN USING PA_FLAG.
  PERFORM READ_VEHICLE         .
  PERFORM GENERATE_INPUTPLAN   .
ENDFORM.                    " CREATE_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  delete_old_inputplan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_OLD_INPUTPLAN  USING PA_FLAG.
  CHECK PA_FLAG = 'X'.
  DELETE FROM ZTPP_INPUT_OSR CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
ENDFORM.                    " delete_old_inputplan

*---------------------------------------------------------------------*
*       FORM read_vehicle                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM READ_VEHICLE.
  DATA: L_OBJEK              LIKE AUSP-OBJEK,
        L_OBJID              LIKE CRHD-OBJID,
        L_CLINT              LIKE KLAH-CLINT,
        L_ATINN1             LIKE AUSP-ATINN,    " Internal Color
        L_ATINN2             LIKE AUSP-ATINN,    " Plan Order
        L_ATINN3             LIKE AUSP-ATINN,    " MITU
        L_ATINN4             LIKE AUSP-ATINN,    " Version
        L_ATINN5             LIKE AUSP-ATINN,    " Model Index
        L_ATINN6             LIKE AUSP-ATINN,    " OCN
        L_ATINN7             LIKE AUSP-ATINN,    " Serial Code
        L_ATINN8             LIKE AUSP-ATINN,    " Work Order
        L_ATINN9             LIKE AUSP-ATINN,    " External Color
        L_ATINNA             LIKE AUSP-ATINN,    " Status
        L_ATINN01            LIKE AUSP-ATINN,               " RP01
        L_ATINN02            LIKE AUSP-ATINN,               " RP02
        L_ATINN03            LIKE AUSP-ATINN,               " RP03
        L_ATINN04            LIKE AUSP-ATINN,               " RP04
        L_ATINN05            LIKE AUSP-ATINN,               " RP05
        L_ATINN06            LIKE AUSP-ATINN,               " RP06
        L_ATINN07            LIKE AUSP-ATINN,               " RP07
        L_ATINN08            LIKE AUSP-ATINN,               " RP08
        L_ATINN09            LIKE AUSP-ATINN,               " RP09
        L_ATINN10            LIKE AUSP-ATINN,               " RP10
        L_ATINN11            LIKE AUSP-ATINN,               " RP11
        L_ATINN12            LIKE AUSP-ATINN,               " RP12
        L_ATINN13            LIKE AUSP-ATINN,               " RP13
        L_ATINN14            LIKE AUSP-ATINN,               " RP14
        L_ATINN15            LIKE AUSP-ATINN,               " RP15
        L_ATINN16            LIKE AUSP-ATINN,               " RP16
        L_ATINN17            LIKE AUSP-ATINN,               " RP17
        L_ATINN18            LIKE AUSP-ATINN,               " RP18
        L_ATINN19            LIKE AUSP-ATINN,    " P_USAGE_CAR
        L_ATINN20            LIKE AUSP-ATINN,    " P_STATUS
        L_ATINN21            LIKE AUSP-ATINN,    " Model
        L_ATINN22            LIKE AUSP-ATINN,    " Body-Serial
        L_ATINN23            LIKE AUSP-ATINN,    " SEQUENCE_DATE
        L_ATINN24            LIKE AUSP-ATINN,    " SEQUENCE_SERIAL

        L_COUNT              TYPE I         ,
        L_VALS(8)            TYPE N         ,
        L_DATE               TYPE D         .

  DATA: BEGIN OF L1_KEYS     OCCURS 0,
          OBJEK              LIKE AUSP-ATWRT,
        END OF L1_KEYS.

  PERFORM CALL_ATINN USING 'P_INT_COLOR'         L_ATINN1.
  PERFORM CALL_ATINN USING 'P_PLAN_ORDER'        L_ATINN2.
  PERFORM CALL_ATINN USING 'P_MITU'              L_ATINN3.
  PERFORM CALL_ATINN USING 'P_VERSION'           L_ATINN4.
  PERFORM CALL_ATINN USING 'P_MI'                L_ATINN5.
  PERFORM CALL_ATINN USING 'P_OCN'               L_ATINN6.
  PERFORM CALL_ATINN USING 'P_SEQUENCE_CODE'     L_ATINN7.
  PERFORM CALL_ATINN USING 'P_WORK_ORDER'        L_ATINN8.
  PERFORM CALL_ATINN USING 'P_EXT_COLOR'         L_ATINN9.
  PERFORM CALL_ATINN USING 'P_RP_STATUS'         L_ATINNA.
  PERFORM CALL_ATINN USING 'P_USAGE_CAR'         L_ATINN19.
  PERFORM CALL_ATINN USING 'P_STATUS'            L_ATINN20.
  PERFORM CALL_ATINN USING 'P_MODEL'             L_ATINN21.
  PERFORM CALL_ATINN USING 'P_BODY_SERIAL'       L_ATINN22.
  PERFORM CALL_ATINN USING 'P_SEQUENCE_DATE'     L_ATINN23.
  PERFORM CALL_ATINN USING 'P_SEQUENCE_SERIAL'   L_ATINN24.

  PERFORM CALL_ATINN USING 'P_RP01_ACTUAL_DATE'  L_ATINN01.
  PERFORM CALL_ATINN USING 'P_RP02_ACTUAL_DATE'  L_ATINN02.
  PERFORM CALL_ATINN USING 'P_RP03_ACTUAL_DATE'  L_ATINN03.
  PERFORM CALL_ATINN USING 'P_RP04_ACTUAL_DATE'  L_ATINN04.
  PERFORM CALL_ATINN USING 'P_RP05_ACTUAL_DATE'  L_ATINN05.
  PERFORM CALL_ATINN USING 'P_RP06_ACTUAL_DATE'  L_ATINN06.
  PERFORM CALL_ATINN USING 'P_RP07_ACTUAL_DATE'  L_ATINN07.
  PERFORM CALL_ATINN USING 'P_RP08_ACTUAL_DATE'  L_ATINN08.
  PERFORM CALL_ATINN USING 'P_RP09_ACTUAL_DATE'  L_ATINN09.
  PERFORM CALL_ATINN USING 'P_RP10_ACTUAL_DATE'  L_ATINN10.
  PERFORM CALL_ATINN USING 'P_RP11_ACTUAL_DATE'  L_ATINN11.
  PERFORM CALL_ATINN USING 'P_RP12_ACTUAL_DATE'  L_ATINN12.
  PERFORM CALL_ATINN USING 'P_RP13_ACTUAL_DATE'  L_ATINN13.
  PERFORM CALL_ATINN USING 'P_RP14_ACTUAL_DATE'  L_ATINN14.
  PERFORM CALL_ATINN USING 'P_RP15_ACTUAL_DATE'  L_ATINN15.
  PERFORM CALL_ATINN USING 'P_RP16_ACTUAL_DATE'  L_ATINN16.
  PERFORM CALL_ATINN USING 'P_RP17_ACTUAL_DATE'  L_ATINN17.
  PERFORM CALL_ATINN USING 'P_RP18_ACTUAL_DATE'  L_ATINN18.

  SELECT SINGLE CLINT INTO L_CLINT
    FROM KLAH
   WHERE CLASS = 'P_VEHICLE_MASTER'.

  SELECT OBJEK INTO TABLE L1_KEYS
    FROM AUSP
   WHERE KLART = '002'
     AND ATINN = L_ATINNA
** changed by furong  from 18 -> 19
     AND ATWRT < '19'.
** end of change

  RANGES: R_ATINN FOR AUSP-ATINN.
  R_ATINN-OPTION = 'EQ'.
  R_ATINN-SIGN   = 'I'.
  R_ATINN-LOW = L_ATINN1. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN2. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN3. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN4. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN5. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN6. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN7. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN8. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN9. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINNA. APPEND R_ATINN.

  R_ATINN-LOW = L_ATINN01. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN02. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN03. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN04. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN05. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN06. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN07. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN08. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN09. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN10. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN11. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN12. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN13. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN14. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN15. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN16. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN17. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN18. APPEND R_ATINN.

  R_ATINN-LOW = L_ATINN19. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN20. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN21. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN22. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN23. APPEND R_ATINN.
  R_ATINN-LOW = L_ATINN24. APPEND R_ATINN.


  LOOP AT L1_KEYS.
*Tuning by Andy
*    SELECT OBJEK ATINN ATWRT ATFLV APPENDING TABLE IT_TEMP
*      FROM AUSP
*     WHERE OBJEK =  L1_KEYS-OBJEK
*       AND KLART = '002' .

    SELECT OBJEK ATINN ATWRT ATFLV APPENDING TABLE IT_TEMP
      FROM AUSP
     WHERE OBJEK =  L1_KEYS-OBJEK
       AND KLART = '002'
       AND ATINN IN R_ATINN.

    IT_DATA-OBJEK = L1_KEYS-OBJEK. APPEND IT_DATA.
  ENDLOOP.

  SORT IT_TEMP BY OBJEK.
  SORT IT_DATA BY OBJEK.
  READ TABLE IT_TEMP INDEX 1 .
  L_OBJEK = IT_TEMP-OBJEK    .

  LOOP AT IT_TEMP.
*    IF l_objek NE it_temp-objek         .
*      it_data-objek = l_objek           .
*
*      APPEND it_data.
*      CLEAR: it_data.
*      l_objek = it_temp-objek .
*    ENDIF.
    READ TABLE IT_DATA WITH KEY OBJEK = IT_TEMP-OBJEK BINARY SEARCH.

    CASE IT_TEMP-ATINN.
      WHEN L_ATINN2   .                   " PLAN ORDER
        IT_DATA-PLNUM = IT_TEMP-ATWRT(10).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING PLNUM.
      WHEN L_ATINN3   .                   " MITU
        IT_DATA-MITU = IT_TEMP-ATWRT(1)  .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING MITU.
      WHEN L_ATINN1   .                   " Internal Color
        IT_DATA-INTC   = IT_TEMP-ATWRT(3) .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING INTC.
      WHEN L_ATINN4   .                   " External Color
        IT_DATA-VERS   = IT_TEMP-ATWRT(3) .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING VERS.
      WHEN L_ATINN5   .                   " MI
        IT_DATA-MI     = IT_TEMP-ATWRT(9) .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING MI.
      WHEN L_ATINN6   .                   " OCNN
        IT_DATA-OCNN   = IT_TEMP-ATWRT(4).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING OCNN.
      WHEN L_ATINN7   .                   " SEQ_CODE
        IT_DATA-SEQ_CODE = IT_TEMP-ATWRT(2).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING SEQ_CODE.
      WHEN L_ATINN8   .                   " Work Order
        IT_DATA-WORK_ORDER = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING WORK_ORDER.
      WHEN L_ATINN9   .                   " External Color
        IT_DATA-EXTC   = IT_TEMP-ATWRT(3) .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING EXTC.
      WHEN L_ATINNA   .                   " Status
        IT_DATA-STATUS = IT_TEMP-ATWRT(2) .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING STATUS.
      WHEN L_ATINN01  .                                     " RP01
        IT_DATA-RP01   = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP01.
      WHEN L_ATINN02  .                                     " RP02
        IT_DATA-RP02  = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP02.
      WHEN L_ATINN03  .                                     " RP03
        IT_DATA-RP03  = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP03.
      WHEN L_ATINN04  .                                     " RP04
        IT_DATA-RP04 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP04.
      WHEN L_ATINN05  .                                     " RP05
        IT_DATA-RP05 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP05.
      WHEN L_ATINN06  .                                     " RP06
        IT_DATA-RP06 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP06.
      WHEN L_ATINN07  .                                     " RP07
        IT_DATA-RP07 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP07.
      WHEN L_ATINN08  .                                     " RP08
        IT_DATA-RP08 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP08.
      WHEN L_ATINN09  .                                     " RP09
        IT_DATA-RP09 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP09.
      WHEN L_ATINN10  .                                     " RP10
        IT_DATA-RP10 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP10.
      WHEN L_ATINN11  .                                     " RP11
        IT_DATA-RP11 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP11.
      WHEN L_ATINN12  .                                     " RP12
        IT_DATA-RP12 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP12.
      WHEN L_ATINN13  .                                     " RP13
        IT_DATA-RP13 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP13.
      WHEN L_ATINN14  .                                     " RP14
        IT_DATA-RP14 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP14.
      WHEN L_ATINN15  .                                     " RP15
        IT_DATA-RP15 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP15.
      WHEN L_ATINN16  .                                     " RP16
        IT_DATA-RP16 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP16.
      WHEN L_ATINN17  .                                     " RP17
        IT_DATA-RP17 = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP17.
      WHEN L_ATINN18  .                                     " RP18
        IT_DATA-RP18  = IT_TEMP-ATWRT(14).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING RP18.
      WHEN L_ATINN19  .                                " P_USAGE_CAR
        IT_DATA-USAGE = IT_TEMP-ATWRT(01).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING USAGE.
      WHEN L_ATINN20  .                                " P_STATUS
        IT_DATA-B     = IT_TEMP-ATWRT    .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING B.
      WHEN L_ATINN21  .                                     " Model
        IT_DATA-MODL  = IT_TEMP-ATWRT(03).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING MODL.
      WHEN L_ATINN22  .                               " Body-Serial
        IT_DATA-BODY_SER  = IT_TEMP-ATWRT(06).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING BODY_SER.
      WHEN L_ATINN23  .                               " SEQUENCE_DATE
        IT_DATA-SEQ_DATE  = L_DATE  = L_VALS = IT_TEMP-ATFLV    .
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING SEQ_DATE.
      WHEN L_ATINN24  .                               " SEQUENCE_SERIAL
        IT_DATA-SEQ_SERIAL = IT_TEMP-ATWRT(05).
        MODIFY IT_DATA INDEX SY-TABIX TRANSPORTING SEQ_SERIAL.

    ENDCASE.

  ENDLOOP.

*  DESCRIBE TABLE it_temp LINES l_count .
*  IF l_count > 0 .
*    it_data-objek = l_objek .
*    APPEND it_data.
*  ENDIF.
ENDFORM.                    " read_vehicle

*&---------------------------------------------------------------------*
*&      Form  call_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0339   text
*      -->P_L_ATINN1  text
*----------------------------------------------------------------------*
FORM CALL_ATINN USING    PA_ATNAM  PA_ATINN.
  CLEAR: PA_ATINN.
  SELECT SINGLE ATINN INTO PA_ATINN
    FROM CABN
   WHERE ATNAM = PA_ATNAM.
ENDFORM.                    " call_atinn

*---------------------------------------------------------------------*
*       FORM generate_inputplan                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GENERATE_INPUTPLAN.
  DATA: L_SDAT            LIKE SY-DATUM         ,
        L_TIME            LIKE SY-UZEIT         ,
        L_ADDTIME         LIKE SY-UZEIT         ,
        L_FLAG            TYPE C                ,
        L_FLAG2           TYPE C                ,
        L_DNAME(30)       TYPE C                ,
        L_NO(2)           TYPE N                ,
        L_PRE(2)          TYPE N                ,
        L_WBS             LIKE ZTPP_COMMON_VALS-ITEM4,
        L_PRJ             LIKE ZTPP_COMMON_VALS-ITEM4,
        L_MODEL           LIKE ZTPP_COMMON_VALS-KEY3 ,
        L_KALID           LIKE KAKO-KALID            ,
        L_PCNT            TYPE I                     ,
        L_BCNT            TYPE I                     ,
        L_COUNT           TYPE I                     ,
        L_STATUS          LIKE ZTPP_DVRT1-STATUS,
        L_7JB             LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE,
        L_DATA            LIKE TABLE OF IT_DATA        WITH HEADER LINE.

  " Make the DVRT Layout
  SORT IT_DATA BY STATUS DESCENDING RP17 RP16 RP15 RP14 RP13 RP12 RP11
            RP10 RP09 RP08 RP07 RP06 RP05 RP04 RP03 RP02 RP01 MITU
            SEQ_DATE SEQ_SERIAL.

  " Scarp-Car Delete...
  DELETE IT_DATA WHERE ( USAGE = 'S' OR USAGE = 'D' ).
  L_DATA[] = IT_DATA[].

  " Fill the Time-Stamp.... in the production-line..
  CLEAR: WA_SERIAL.
  LOOP AT L_DATA .
    WA_SERIAL = WA_SERIAL +  1 .
    L_DATA-SERIAL = WA_SERIAL.
    L_DATA-RS18   = 'A' .
    MODIFY L_DATA.
    MOVE-CORRESPONDING L_DATA TO WA_DATA.
  ENDLOOP.

  RANGES : R_GUBB FOR ZTPP_PMT07JB_A-GUBB .

  R_GUBB-OPTION = 'EQ'.
  R_GUBB-SIGN   = 'I'.
  R_GUBB-LOW    = 'A'. APPEND R_GUBB.
  R_GUBB-LOW    = 'B'. APPEND R_GUBB.

  SELECT * INTO TABLE L_7JB
    FROM ZTPP_PMT07JB_A
  WHERE GUBB IN R_GUBB.


  DATA : LV_CNT  TYPE I,
         LV_DATE LIKE SY-DATUM.
  CLEAR : LV_CNT.
  LV_CNT = 1 .
  SORT L_7JB BY SQDT SSR1 SSR2.

  LOOP AT L_7JB.
    CLEAR: L_DATA.
    MOVE-CORRESPONDING L_7JB TO L_DATA .

    WA_SERIAL         = WA_SERIAL +  1 .

    L_DATA-SEQ_DATE   = L_7JB-SQDT     .
    L_DATA-SEQ_SERIAL = L_7JB-SSR1     .
    L_DATA-SEQ_CODE   = L_7JB-SQCD     .
    L_DATA-SERIAL     = WA_SERIAL      .
*
*    IF L_7JB-GUBB     = 'A'  . "AND L_7JB-GUB1 = '2'.
*      L_DATA-RS18     = 'B' .
*    ELSE.
*      L_DATA-RS18     = 'A' .
*    ENDIF.
    L_DATA-RS18 = L_7JB-GUBB .

    CONCATENATE L_7JB-ORDR L_7JB-DIST  INTO L_DATA-WORK_ORDER.
    IF L_7JB-ORDR+(1) = 'F'.
      L_DATA-MI = L_7JB-BMDL.
    ELSE.
      PERFORM GET_MI    USING L_DATA-WORK_ORDER L_DATA-EXTC L_DATA-INTC
                              L_DATA-MI  .
    ENDIF.

    DO  L_7JB-PQTY TIMES.
      L_DATA-SEQ_NO   = LV_CNT .
      L_DATA-RD01     = L_DATA-SEQ_DATE.
      APPEND L_DATA.
      LV_CNT = LV_CNT + 1.
    ENDDO.

    CLEAR : LV_CNT. LV_CNT = 1 .
  ENDLOOP.

  " Set the Paint-Reject Vehicle and WBS Vehicle in the T Field


  SORT L_DATA BY SERIAL.
  LOOP AT L_DATA WHERE STATUS = '01' .     " WBS Setting..
    IF L_MODEL NE L_DATA-MODL .
      L_MODEL = L_DATA-MODL .
      PERFORM GET_WAIT_TIME    USING 'PRJ'  L_PRJ L_MODEL      .
      PERFORM GET_WAIT_TIME    USING 'WBS'  L_WBS L_MODEL      .
      PERFORM READ_SHOP_CALID  USING  L_KALID                  .
      PERFORM GET_MASTER       USING  L_PCNT L_BCNT L_KALID
                                      L_WBS L_PRJ L_MODEL      .
    ENDIF.
    L_COUNT = L_COUNT + 1.
    IF L_COUNT > L_BCNT  .
      L_DATA-T = 'WBS'   .
      MODIFY L_DATA      .
    ENDIF.
  ENDLOOP.

  CLEAR: L_MODEL.

  LOOP AT L_DATA WHERE STATUS > '01' AND STATUS < '04'.  " PRJ Setting..
    IF L_MODEL NE L_DATA-MODL .
      L_MODEL = L_DATA-MODL .
      PERFORM GET_WAIT_TIME    USING 'PRJ'  L_PRJ L_MODEL      .
      PERFORM GET_WAIT_TIME    USING 'WBS'  L_WBS L_MODEL      .
      PERFORM READ_SHOP_CALID  USING  L_KALID                  .
      PERFORM GET_MASTER       USING  L_PCNT L_BCNT L_KALID
                                      L_WBS L_PRJ L_MODEL      .
    ENDIF.
    L_COUNT = L_COUNT + 1.
    IF L_COUNT > L_PCNT  .
      L_DATA-T = 'PRJ'   .
      MODIFY L_DATA      .
    ENDIF.
  ENDLOOP.
  IT_DATA[] = L_DATA[].     CLEAR: L_DATA, L_DATA[].

  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA  TO    ZTPP_INPUT_OSR    .
    ZTPP_INPUT_OSR-ZSTIM = SY-UZEIT.
    ZTPP_INPUT_OSR-ZSDAT = SY-DATUM.
    MODIFY ZTPP_INPUT_OSR      FROM  ZTPP_INPUT_OSR.
  ENDLOOP.
ENDFORM.                    " generate_inputplan

*---------------------------------------------------------------------*
*       FORM get_master                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_PCNT                                                       *
*  -->  PA_BCNT                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WBS                                                        *
*  -->  PA_PRJ                                                        *
*  -->  PA_MODEL                                                      *
*---------------------------------------------------------------------*
FORM GET_MASTER USING    PA_PCNT  PA_BCNT  PA_KALID
                         PA_WBS  PA_PRJ    PA_MODEL .
  DATA: L_DATE               TYPE D ,
        L_CHK                TYPE P DECIMALS 3,
        L_DAY                LIKE KAPA-TAGNR,      " Day
        L_TIME               TYPE KAPENDZT  ,      " Times for working
        L_UPH                TYPE ZVPP_LD-LRATE,   " UPH
        L_COUNT              TYPE I .

  L_DATE = SY-DATUM .
  PERFORM READ_WORKING_DATE USING '+'  PA_KALID  L_DATE.
  IF L_DATE = SY-DATUM .
    PERFORM GET_DAY          USING L_DATE  L_DAY         .
    PERFORM GET_WORKTIME     USING L_DATE  L_TIME  L_DAY  'B'.
    PERFORM GET_UPH          USING L_DATE  L_UPH   'B'   .
    L_CHK = L_TIME / 3600 .
    PA_BCNT = CEIL( L_UPH * L_CHK )  .
    PERFORM GET_WORKTIME     USING L_DATE  L_TIME  L_DAY  'T'.
    PERFORM GET_UPH          USING L_DATE  L_UPH   'T'   .
    L_CHK = L_TIME / 3600 .
    PA_BCNT = CEIL( L_UPH * L_CHK )  .
  ELSE.
    PA_BCNT = PA_PCNT = 0 .
  ENDIF.
ENDFORM.                    " GET_MASTER

*&---------------------------------------------------------------------*
*&      Form  get_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
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
*&      Form  get_worktime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_TIME  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM GET_WORKTIME     USING    PA_WDATE  PA_WKTIME  PA_DAY  PA_WC.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_FLAG        TYPE C ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA       LIKE TABLE OF ZVPP_CAPACITY      WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL = PA_WC
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
ENDFORM.                    " GET_WORKTIME

*&---------------------------------------------------------------------*
*&      Form  get_uph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_UPH  text
*      -->P_IT_MASTER_SHIFT  text
*----------------------------------------------------------------------*
FORM GET_UPH USING    PA_WDATE  PA_UPH  PA_WC.
  DATA: W_UPH  LIKE ZTPP_STATUS-UPH.
  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      DATE          = PA_WDATE
*       SHIFT         =
      SHOP          = PA_WC
    IMPORTING
      UPH           = W_UPH
            .
  PA_UPH = W_UPH.

*  DATA lw_ld          LIKE zvpp_ld .
*
*    SELECT SINGLE * INTO lw_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = PA_WC     .
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH

*----------------------------------------------------------------------*
*      -->P_L_DATA_WORK_ORDER  text
*      -->P_L_DATA_EXTC  text
*      -->P_L_DATA_INTC  text
*      -->P_L_DATA_MI  text
*----------------------------------------------------------------------*
FORM GET_MI USING    PA_WORDER  PA_EXTC  PA_INTC  PA_MI.
  DATA: L_VALS       LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
        L_WORDER     LIKE MARA-MATNR.

  CLEAR: L_VALS, L_VALS[].
  CONCATENATE PA_WORDER PA_EXTC PA_INTC INTO L_WORDER.

  L_VALS-ATNAM = 'P_MI'  .    APPEND L_VALS.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = L_WORDER
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VALS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC = 0.
    READ TABLE L_VALS INDEX 1 .
    PA_MI = L_VALS-ATWRT      .
  ENDIF.
ENDFORM.                    " GET_MI

*&---------------------------------------------------------------------*
*&      Form  GET_WAIT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0874   text
*      -->P_L_PRJ  text
*----------------------------------------------------------------------*
FORM GET_WAIT_TIME USING    PA_VAL  PA_RETURN  PA_MODEL.
  CLEAR: PA_RETURN .

  SELECT SINGLE ITEM4  INTO PA_RETURN
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = 'ZAPP903R_INPUT_PLAN'
     AND KEY2 = PA_VAL
     AND KEY3 = PA_MODEL.
ENDFORM.                    " GET_WAIT_TIME
