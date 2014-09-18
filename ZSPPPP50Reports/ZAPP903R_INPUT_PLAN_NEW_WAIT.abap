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
REPORT  zapp903r_input_plan   MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztpp_common_vals.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_log        OCCURS 0,
        pro_type             TYPE n  ,
        log_seq              TYPE i  ,
        log_type             TYPE c  ,
        log_msg              LIKE bapiret2-message ,
      END OF it_log.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_uph                TYPE i,
      wa_model              TYPE zpp_model,
      wa_dates              TYPE d,
      wa_prj                TYPE c,
      wa_close              TYPE c,
      wa_wbs                TYPE c.

*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_key1(18)            VALUE 'INPUT_PLAN',
      c_key2(18)            VALUE 'BODY_INPUT',
      c_key3(18)            VALUE 'TRIM_INPUT',
      c_key4(18)            VALUE 'SEQ_SUM01' ,
      c_key5(18)            VALUE 'SEQ_SUM02' ,
      c_key6(18)            VALUE 'SEQ_SUM03' ,
      c_key7(18)            VALUE 'SEQ_PROD'  ,
      c_key8(18)            VALUE 'WIRE_REP'  .

DATA: BEGIN OF ABAPLIST OCCURS 1.
        INCLUDE STRUCTURE ABAPLIST.
DATA: END OF ABAPLIST.


*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
** Furong on 07/11/12 for 3 shift
*PARAMETERS: p_run         AS CHECKBOX  DEFAULT ' ',
*            p_real        AS CHECKBOX  DEFAULT ' ',
*            p_test        AS CHECKBOX  DEFAULT ' ',
*            p_mitu         AS CHECKBOX  DEFAULT ' ',
*            p_fwo          AS CHECKBOX  DEFAULT ' ',
*            p_msg         LIKE ztpp_common_vals-item3,
*            p_wbs(2)      TYPE n        OBLIGATORY,
*            p_prj(2)      TYPE n        OBLIGATORY,
*            p_close       TYPE spmon    OBLIGATORY.

PARAMETERS: p_run         AS CHECKBOX  DEFAULT ' ',
            p_real        AS CHECKBOX  DEFAULT ' ',
            p_test        AS CHECKBOX  DEFAULT ' ',
            p_mitu         AS CHECKBOX  DEFAULT ' ',
            p_fwo          AS CHECKBOX  DEFAULT ' '.

SELECTION-SCREEN skip.

PARAMETERS: p_rp06tm(6) type n DEFAULT '063000'.


SELECTION-SCREEN begin of line.
SELECTION-SCREEN COMMENT 1(20) text-t01.
SELECTION-SCREEN COMMENT 25(6) text-t02.
PARAMETERS: p_fifo radiobutton group grp1.

SELECTION-SCREEN COMMENT 40(12) text-t03.
PARAMETERS: p_mod radiobutton group grp1.

SELECTION-SCREEN COMMENT 58(5) text-t04.
parameters: p_mod1 like ZTPP_INPUT_PLAN-modl,
            p_mod2 like ZTPP_INPUT_PLAN-modl,
            p_mod3 like ZTPP_INPUT_PLAN-modl.
SELECTION-SCREEN end of line.

SELECTION-SCREEN begin of line.
SELECTION-SCREEN COMMENT 58(5) text-t05.

parameters: p_rat1(4) type n,
            p_rat2(4) type n,
            p_rat3(4) type n.

SELECTION-SCREEN end of line.

SELECTION-SCREEN skip.

PARAMETERS: p_msg         LIKE ztpp_common_vals-item3,
            p_wbs(2)      TYPE n        OBLIGATORY,
            p_prj(2)      TYPE n        OBLIGATORY,
            p_close       TYPE spmon    OBLIGATORY.
** End
PARAMETERS: p_srvgrp LIKE rzllitab-classname OBLIGATORY
                     DEFAULT 'PG_INP'.
PARAMETERS: w_max_w TYPE i DEFAULT 10.

SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS : p_short      AS CHECKBOX   ,
             p_long       AS CHECKBOX   ,
             p_week       AS CHECKBOX   ,
             p_binput     AS CHECKBOX   ,
             p_tinput     AS CHECKBOX   ,
             p_sum        AS CHECKBOX   .

SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  PERFORM setting_default.

** Fuorng for 3 shift

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MOD1.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         MODL LIKE ZTPP_INPUT_PLAN-MODL,
         DESC(20),
         END OF VALUE_TAB.
  DATA: L_ATINN LIKE CABN-ATINN,
      L_ATWRT              LIKE AUSP-ATWRT,
        L_ATWTB              LIKE CAWNT-ATWTB.
  DATA: W_REPID LIKE SY-REPID,
        W_DYNNR LIKE SY-DYNNR.

  SELECT SINGLE ATINN INTO L_ATINN
   FROM CABN
  WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    VALUE_TAB-DESC = L_ATWRT.  "ZTPP_VEH_MODEL-NAME.  l_atwtb
    VALUE_TAB-MODL  = L_ATWRT.  "ZTPP_VEH_MODEL-MODEL.
    APPEND VALUE_TAB.
  ENDSELECT.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DISPO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'DISPO'
      WINDOW_TITLE    = 'MRP Controller'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MOD2.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         MODL LIKE ZTPP_INPUT_PLAN-MODL,
         DESC(20),
         END OF VALUE_TAB.
  DATA: L_ATINN LIKE CABN-ATINN,
      L_ATWRT              LIKE AUSP-ATWRT,
        L_ATWTB              LIKE CAWNT-ATWTB.
  DATA: W_REPID LIKE SY-REPID,
        W_DYNNR LIKE SY-DYNNR.

  SELECT SINGLE ATINN INTO L_ATINN
   FROM CABN
  WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    VALUE_TAB-DESC = L_ATWRT.  "ZTPP_VEH_MODEL-NAME.  l_atwtb
    VALUE_TAB-MODL  = L_ATWRT.  "ZTPP_VEH_MODEL-MODEL.
    APPEND VALUE_TAB.
  ENDSELECT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DISPO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'DISPO'
      WINDOW_TITLE    = 'MRP Controller'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MOD3.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         MODL LIKE ZTPP_INPUT_PLAN-MODL,
         DESC(20),
         END OF VALUE_TAB.
  DATA: L_ATINN LIKE CABN-ATINN,
      L_ATWRT              LIKE AUSP-ATWRT,
        L_ATWTB              LIKE CAWNT-ATWTB.
  DATA: W_REPID LIKE SY-REPID,
        W_DYNNR LIKE SY-DYNNR.

  SELECT SINGLE ATINN INTO L_ATINN
   FROM CABN
  WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    VALUE_TAB-DESC = L_ATWRT.  "ZTPP_VEH_MODEL-NAME.  l_atwtb
    VALUE_TAB-MODL  = L_ATWRT.  "ZTPP_VEH_MODEL-MODEL.
    APPEND VALUE_TAB.
  ENDSELECT.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DISPO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'DISPO'
      WINDOW_TITLE    = 'MRP Controller'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

** End for 3 shift
*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM set_possible_entry.
  PERFORM clear_variable .
  PERFORM run_process.
  PERFORM call_subprocess.

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM display_result .

*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_variable.
  IF p_run = 'X'   .
    GET TIME.
   WRITE AT: /001(46) 'Start: INPUT-PLAN & Prod. Result of prev. day.' ,
                     048(11) sy-datum,
                     060(10) sy-uzeit.
  ENDIF.

  " Save the Input Values...
  IF wa_wbs = 'X'.
    ztpp_common_vals-jobs  = 'ZAPP903R_INPUT_PLAN' .
    ztpp_common_vals-key2  = 'WBS'    .
    ztpp_common_vals-item4 = p_wbs    .
    ztpp_common_vals-description = text-003.
    INSERT INTO ztpp_common_vals VALUES ztpp_common_vals .
  ELSE.
    UPDATE ztpp_common_vals   SET item4 = p_wbs
                            WHERE jobs  = 'ZAPP903R_INPUT_PLAN'
                              AND key2  = 'WBS'    .
  ENDIF.

  IF wa_prj = 'X'.
    ztpp_common_vals-jobs  = 'ZAPP903R_INPUT_PLAN' .
    ztpp_common_vals-key2  = 'PRJ'    .
    ztpp_common_vals-item4 = p_prj    .
    ztpp_common_vals-description = text-004.
    INSERT INTO ztpp_common_vals VALUES ztpp_common_vals .
  ELSE.
    UPDATE ztpp_common_vals   SET item4 = p_prj
                            WHERE jobs  = 'ZAPP903R_INPUT_PLAN'
                              AND key2  = 'PRJ'    .
  ENDIF.

  IF wa_close = 'X'.
    ztpp_common_vals-jobs  = 'ZAPP903R_INPUT_PLAN'   .
    ztpp_common_vals-key2  = 'CLOSE'    .
    ztpp_common_vals-item4 = p_close    .
    ztpp_common_vals-description = text-005.
    INSERT INTO ztpp_common_vals VALUES ztpp_common_vals .
  ELSE.
    UPDATE ztpp_common_vals   SET item4 = p_close
                            WHERE jobs  = 'ZAPP903R_INPUT_PLAN'
                              AND key2  = 'CLOSE'    .
  ENDIF.

  IF sy-subrc = 0.
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
FORM setting_default.
* Considering : If 2 Model is running, The UPH is input the 2 Values..
*               If More Model is running, How to programming????
  DATA: l_jobs                TYPE programm  ,
        l_lrate               LIKE ldlh-lrate.

  GET PARAMETER ID 'ZRUN' FIELD p_run.
  GET TIME.    CLEAR: wa_prj, wa_wbs.
  wa_model = 'EMF'   .
  wa_dates = sy-datum.
  SELECT SINGLE lrate INTO l_lrate
    FROM crhd AS c INNER JOIN ldlh AS l
      ON c~objid = l~lnid
   WHERE c~arbpl = '1'  .
  IF sy-subrc NE 0.
    wa_uph = 63   .
  ELSE.
    wa_uph = l_lrate .
  ENDIF.

  CLEAR: ztpp_common_vals.  l_jobs = 'ZAPP903R_INPUT_PLAN'.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = l_jobs
     AND key2 = 'WBS' .

  IF sy-subrc = 0.
    p_wbs    = ztpp_common_vals-item4 .
  ELSE.
    wa_wbs   = 'X'.
    p_wbs    = 4 .
  ENDIF.

  CLEAR: ztpp_common_vals.  l_jobs = 'ZAPP903R_INPUT_PLAN'.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = l_jobs
     AND key2 = 'PRJ' .

  IF sy-subrc = 0.
    p_prj    = ztpp_common_vals-item4 .
  ELSE.
    wa_prj   = 'X'.
    p_prj    = 20.
  ENDIF.

  CLEAR: ztpp_common_vals.  l_jobs = 'ZAPP903R_INPUT_PLAN'.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = l_jobs
     AND key2 = 'CLOSE'  .

  IF sy-subrc = 0.
    p_close  = ztpp_common_vals-item4 .
  ELSE.
    wa_close  = 'X'.
    p_close  = sy-datum(6)            .
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
FORM run_process.
  DATA: l_kalid             LIKE kako-kalid                          ,
        l_date              TYPE d .

  IF p_run = 'X' .
    PERFORM save_record_common_vals  USING c_key1.
    CALL FUNCTION 'Z_FPP_CREATE_INPUTPLAN'
         EXPORTING
              i_del = 'X'
** Furong on 07/11/12 for 3 shift
              I_FIFO = p_fifo
              I_MOD1 = p_mod1
              I_MOD2 = p_mod2
              I_MOD3 = p_mod3
              I_RAT1 = p_RAT1
              I_RAT2 = p_RAT2
              I_RAT3 = p_RAT3
              I_RP06TM = p_RP06TM.
** End

*    " Create the Previous Working Date's Prod. result.
    l_date = wa_dates - 1 .
    PERFORM read_shop_calid   USING l_kalid.
    PERFORM read_working_date USING '-'  l_kalid  l_date  .
    SUBMIT zapp903r_daily_production
           WITH p_wdate = l_date      AND RETURN .

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

    GET TIME.
    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
               041(11) sy-datum,
               053(10) sy-uzeit.
*---Start wskim: check holding car
    PERFORM check_holding_car.
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
FORM call_subprocess.
*  DATA: BEGIN OF it_wo OCCURS 0,
*          wo_ser  LIKE ztpp_wosum-wo_ser,
*          nation  LIKE ztpp_wosum-nation,
*          dealer  LIKE ztpp_wosum-dealer,
*          extc    LIKE ztpp_wosum-extc,
*          intc    LIKE ztpp_wosum-intc,
*          fsc     LIKE ztpp_wosum-fsc,
*          version LIKE ztpp_wosum-version,
*          womoddate LIKE ztpp_wosum-womoddate,
*       END OF it_wo.
*  SELECT wo_ser nation dealer extc intc
*          fsc version womoddate
*      INTO TABLE it_wo
*      FROM ztpp_wosum.
*
*  SORT it_wo BY  fsc       ASCENDING
*                   extc      ASCENDING
*                   intc      ASCENDING
*                   version   ASCENDING
*                   womoddate DESCENDING.

  " Call the Sub-Process.. Call Transaction...
  IF p_real   = 'X' .
    GET TIME.
    WRITE AT: /001(40) 'Start : ZAPP903R_ALC_SUM - MIP DATA..' ,
               041(11) sy-datum,
               053(10) sy-uzeit.

    SUBMIT zapp903r_alc_sum    AND RETURN .

    GET TIME.
    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
               041(11) sy-datum,
               053(10) sy-uzeit.
  ENDIF.

  IF p_short  = 'X' .
    GET TIME.
    WRITE AT: /001(40) 'Start : ZAPP903R_SEQ_SUM01 - 3 DAY   ' ,
               041(11) sy-datum,
               053(10) sy-uzeit.

    SUBMIT zapp903r_seq_sum01          " ZAPP903R_ALC_SHORT
               WITH  p_dates = wa_dates
               WITH  p_test  = p_test
               WITH  p_mitu  = p_mitu
               WITH  p_wbs   = p_wbs
               WITH  p_prj   = p_prj      AND RETURN          .

    SUBMIT zapp903r_wire_hour          " zapp903r_WIRE_HOURLY
               WITH  p_dates = wa_dates
               WITH  p_mitu  = p_mitu
               WITH  p_test  = p_test
               WITH  p_wbs   = p_wbs
               WITH  p_prj   = p_prj      AND RETURN          .

    PERFORM save_record_common_vals  USING c_key4             .

    GET TIME.
    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
               041(11) sy-datum,
               053(10) sy-uzeit.
  ENDIF.

  IF p_long   = 'X' .
    GET TIME.
    WRITE AT: /001(40) 'Start : ZAPP903R_SEQ_SUM02 - 3 WEEK. ' ,
               041(11) sy-datum,
               053(10) sy-uzeit.

    SUBMIT zapp903r_seq_sum02          " ZAPP903R_ALC_LONG
               WITH  p_dates = wa_dates
               WITH  p_test  = p_test
               WITH  p_mitu  = p_mitu
               WITH  p_wbs   = p_wbs
               WITH  p_prj   = p_prj
               with  p_fwo    = p_fwo AND RETURN          .

    SUBMIT zapp903r_wire_day           " ZAPP903R_WIRE_DAY
               WITH  p_dates = wa_dates
               WITH  p_test  = p_test
               WITH  p_mitu  = p_mitu
               WITH  p_wbs   = p_wbs
               WITH  p_prj   = p_prj
               with  p_fwo    = p_fwo AND RETURN.

    PERFORM save_record_common_vals  USING c_key5              .

    GET TIME.
    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
               041(11) sy-datum,
               053(10) sy-uzeit.
  ENDIF.

  IF p_week   = 'X' .
    GET TIME.
    WRITE AT: /001(40) 'Start : ZAPP903R_SEQ_SUM03 - 21 WEEK.' ,
               041(11) sy-datum,
               053(10) sy-uzeit.
    SUBMIT zapp903r_seq_sum03
               WITH  p_dates = wa_dates
               WITH  p_test  = p_test
               WITH  p_mitu  = p_mitu
               with  p_fwo    = p_fwo AND RETURN.

    PERFORM save_record_common_vals  USING c_key6              .

    GET TIME.
    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
               041(11) sy-datum,
               053(10) sy-uzeit.
  ENDIF.

  IF p_binput = 'X'.
*    GET TIME.
*    WRITE AT: /001(40) 'Start : ZAPP903R_ALC_BINPUT - Body   ' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
*
    SUBMIT     zapp903r_alc_binput_p_new_wait
               WITH  p_dates  = wa_dates
               WITH  p_mitu   = p_mitu
               with  p_fwo    = p_fwo
               with  p_srvgrp = p_srvgrp
               with  w_max_w =  w_max_w
               AND RETURN EXPORTING LIST TO MEMORY .

    perform display_list.

    PERFORM save_record_common_vals  USING c_key2              .

*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
  ENDIF.

  IF p_tinput = 'X'.
*    GET TIME.
*    WRITE AT: /001(40) 'Start : ZAPP903R_ALC_TINPUT - Trim   ' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.

    SUBMIT  zapp903r_alc_tinput_p_new_wait
               WITH  p_dates  = wa_dates
               WITH  p_test   = p_test
               WITH  p_mitu   = p_mitu
               WITH  p_fwo = p_fwo
               with  p_srvgrp = p_srvgrp
               with  w_max_w =  w_max_w
               AND RETURN  EXPORTING LIST TO MEMORY .

    perform display_list.

    PERFORM save_record_common_vals  USING c_key3              .

*    GET TIME.
*    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
*               041(11) sy-datum,
*               053(10) sy-uzeit.
  ENDIF.

  IF p_sum    = 'X'.
    GET TIME.
    WRITE AT: /001(40) 'Start : ZAPP903R_PROD_REPORT         ' ,
               041(11) sy-datum,
               053(10) sy-uzeit.

    SUBMIT  zapp903r_prod_result
               WITH  p_spmon  = p_close    AND RETURN          .

    PERFORM save_record_common_vals  USING c_key7              .

    GET TIME.
    WRITE AT: /001(40) 'End of the Data Creation ...........' ,
               041(11) sy-datum,
               053(10) sy-uzeit.
  ENDIF.
ENDFORM.                    " CALL_SUBPROCESS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_result.

ENDFORM.                    " DISPLAY_RESULT

*&---------------------------------------------------------------------*
*&      Form  SET_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_possible_entry.

ENDFORM.                    " SET_POSSIBLE_ENTRY

*&---------------------------------------------------------------------*
*&      Form  read_working_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0406   text
*      -->P_0407   text
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " read_working_date

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'B'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  SAVE_RECORD_COMMON_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_record_common_vals  USING pa_key .
  DATA: l_repid             LIKE sy-repid.

  " Check the Table Record: ZTPP_COMMON_VALS-ITEM3 for INPUT_PLAN
  l_repid = 'ZAPP903R_INPUT_PLAN'.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = l_repid
     AND key2 = pa_key    .

  IF sy-subrc NE 0.
    CLEAR: ztpp_common_vals.
    ztpp_common_vals-jobs  = l_repid.
    ztpp_common_vals-key2  = pa_key .
  ENDIF.

  ztpp_common_vals-dates = wa_dates.
  ztpp_common_vals-times = sy-uzeit.
  ztpp_common_vals-item3 = p_msg   .
  MODIFY ztpp_common_vals .
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000 WITH 'Common_vals Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000 WITH 'Common_vals Update failed'.
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
FORM check_holding_car.
  DATA : it_hold LIKE ztpp_hold_car OCCURS 0 WITH HEADER LINE,
         it_data LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE.

  REFRESH :it_hold,it_data.

  SELECT * INTO TABLE it_hold FROM ztpp_hold_car
         WHERE  ( status NE 'D' )
             OR ( status NE 'P' ).

  SELECT * INTO TABLE it_data FROM ztpp_input_plan
         WHERE mitu <> 'Y'
           AND status < '06'.

  LOOP AT it_data.
    IF it_data-work_order+12(2) = 'XX' OR
       it_data-work_order+12(2) = 'XY' .
      DELETE it_data.
    ENDIF.
  ENDLOOP.

** Furong on 06/08/12 for tuning (furong)
SORT it_data BY modl body_ser.
** End on 06/08/12

  LOOP AT it_hold WHERE res_date > wa_dates.
    READ TABLE it_data WITH KEY modl = it_hold-modl
                            body_ser = it_hold-body_ser
** Furong on 06/08/12 for tuning (furong)
                           BINARY SEARCH. "Addition
** End on 05/23/12
    IF sy-subrc = 0.
      it_hold-status = 'W'.
      MODIFY it_hold FROM it_hold.

      UPDATE ztpp_input_plan SET t = 'HOLD'
               WHERE modl = it_hold-modl
                 AND body_ser = it_hold-body_ser.

    ENDIF.
  ENDLOOP.

  LOOP AT it_hold WHERE res_date <= wa_dates.
    IF it_hold-res_date =  wa_dates.
      it_hold-status = 'D'.
    ELSE.
      it_hold-status = 'P'.
    ENDIF.
    MODIFY it_hold FROM it_hold.
  ENDLOOP.

  MODIFY ztpp_hold_car FROM TABLE it_hold.
  IF sy-subrc = 0.
    MESSAGE s000 WITH text-003.
    COMMIT WORK.
  ELSE.
    MESSAGE s000 WITH text-004.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " check_holding_car
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_list.

  CALL FUNCTION 'LIST_FROM_MEMORY'
       TABLES
            LISTOBJECT = ABAPLIST
       EXCEPTIONS
            NOT_FOUND  = 01.

  CALL FUNCTION 'WRITE_LIST'
       TABLES
            LISTOBJECT = ABAPLIST
       EXCEPTIONS
            EMPTY_LIST = 01.

endform.                    " display_list
