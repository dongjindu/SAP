************************************************************************
* Program Name      : ZAPP903R_PROD_RESULT
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : Monthly Prod. Result
*
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
*#1 03/18/2005   wskim     UD1K914858 Condition logic supplement
************************************************************************
REPORT  zapp903r_prod_result  MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztpp_common_vals,
        ztpp_alc_prod,
        ausp .

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_ausp        OCCURS 0,
        status                LIKE ztpp_input_plan-status. " Veh. Status
        INCLUDE STRUCTURE     ausp           .
DATA: END OF it_ausp .

DATA: BEGIN OF it_pharse      OCCURS 0        .
        INCLUDE STRUCTURE     zspp_condition  .
DATA:   atinn                 LIKE cabn-atinn ,
        atwrt                 LIKE ausp-atwrt ,
        atflv                 LIKE ausp-atflv ,
      END OF it_pharse.

DATA: BEGIN OF it_sum         OCCURS 0,
        worder                LIKE mara-matnr,
        status                LIKE ztpp_input_plan-status,
        mark                  TYPE c                     ,
        extc                  LIKE ztpp_input_plan-extc,
        intc                  LIKE ztpp_input_plan-intc,
        objek                 LIKE ausp-objek          ,
        atwrt                 LIKE ausp-atwrt          ,
        atflv                 LIKE ausp-atflv          .
        INCLUDE STRUCTURE     ztpp_alc_prod  .
DATA: END OF it_sum .

DATA: it_data            LIKE TABLE OF ztpp_input_plan WITH HEADER LINE,
      it_7jb             LIKE TABLE OF ztpp_pmt07jb_a  WITH HEADER LINE,
      it_prev            LIKE TABLE OF it_sum          WITH HEADER LINE,
      it_today           LIKE TABLE OF it_sum          WITH HEADER LINE,
      it_month           LIKE TABLE OF it_sum          WITH HEADER LINE,
      it_day             LIKE TABLE OF it_sum          WITH HEADER LINE,
      it_plan            LIKE TABLE OF it_sum          WITH HEADER LINE,
      it_mitu            LIKE TABLE OF it_sum          WITH HEADER LINE,
      it_cond            LIKE TABLE OF ztpp_plan_key   WITH HEADER LINE,
      it_disp            LIKE TABLE OF ztpp_alc_prod   WITH HEADER LINE.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_disp             LIKE it_disp                             ,
      wa_data             LIKE it_data                             ,
      wa_wdate            LIKE ztpp_day_sum-wdate                  ,
      wa_kalid            LIKE kako-kalid                          ,
      wa_uzeit            LIKE sy-uzeit                            ,
      wa_index            LIKE sy-tabix                            ,
      wa_error            TYPE c                                   ,
      wa_flag             TYPE c                                   ,
      wa_hour             TYPE i                                   .

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <wa_dfield>    TYPE ANY.


*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_jobs(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              VALUE 'SEQ_PROD'  .


*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_spmon      TYPE spmon    OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  " Get the Date for the Production Reporting Date(Last Date)
  PERFORM get_start_day     USING wa_wdate     .
* requested by my hur changed by chris
*  wa_wdate = wa_wdate - 1 .
  wa_wdate = sy-datum - 1.
* end of change on 04/11/2005
  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM clear_variable               .
  PERFORM read_inputplan               .
  PERFORM create_summary               .
  PERFORM summary_data                 .
  PERFORM display_data                 .

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
FORM clear_variable.
  CLEAR: it_data,   it_cond,   it_sum,   it_disp,
         it_data[], it_cond[], it_sum[], it_disp[],
         wa_data,  wa_uzeit, wa_index, wa_hour.
ENDFORM.                    " CLEAR_VARIABLE

*&---------------------------------------------------------------------*
*&      Form  READ_INPUTPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_inputplan.
  " Get the Monthly Data ( From This month 1st day to Current day )
  PERFORM get_data_month   USING p_spmon                  .

  " Get the Today's Data
  PERFORM get_data_prev    USING sy-datum                 .
  it_today[] = it_prev[].  CLEAR: it_prev, it_prev[]      .

  " Get the Previous day's Data
  PERFORM get_data_prev    USING wa_wdate                 .

  " Get the Current Data
  PERFORM get_data_curr                                   .
  SORT it_data BY serial .

  " Get the Pre-Planing Data frm ZTPP_PMT07JB_A Table
  PERFORM get_data_plan                                   .

  " Get the MITU Data from Vehicle Master
  PERFORM get_data_mitu                                   .
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  CREATE_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_summary.
  DATA: l_atinn              LIKE cabn-atinn ,
        l_atwrt              LIKE ausp-atwrt ,
        l_atflv              LIKE ausp-atflv ,
        l_error              TYPE c          ,
        l_index              TYPE i          ,
        l_worder             LIKE mara-matnr ,
        lt_month             LIKE TABLE OF it_sum      WITH HEADER LINE,
        lt_plan              LIKE TABLE OF it_sum      WITH HEADER LINE,
        lt_day               LIKE TABLE OF it_sum      WITH HEADER LINE,
        lt_prev              LIKE TABLE OF it_sum      WITH HEADER LINE,
        lt_today             LIKE TABLE OF it_sum      WITH HEADER LINE,
        lt_disp              LIKE TABLE OF it_disp     WITH HEADER LINE.

  PERFORM get_condition        .
  lt_month[] = it_month[].  lt_prev[] = it_prev[].
  lt_day[]   = it_day[]  .  lt_plan[] = it_plan[].
  lt_today[] = it_today[].

  LOOP AT it_cond.
    PERFORM get_cond_parsing  .
    it_month[] = lt_month[]   .  it_prev[] = lt_prev[].
    it_day[]   = lt_day[]     .  it_plan[] = lt_plan[].
    it_today[] = lt_today[]   .

    LOOP AT it_month.
      CLEAR: l_error.
*---start#1 wskim 03/18/2005 by hur
*      LOOP AT it_pharse       .
*        IF it_pharse-oper = 'OR' .
*          CLEAR: l_error     .
*        ENDIF.
*        IF l_error = 'X'     .
*          CONTINUE           .
*        ENDIF.
*        SELECT SINGLE *
*          FROM ausp
*         WHERE objek = it_month-worder
*           AND atinn = it_pharse-atinn
*           AND klart = '001'
*           AND atwrt = it_pharse-atwrt
*           AND atflv = it_pharse-atflv .
*
*        IF sy-subrc NE 0      .
*          l_error  = 'X'     .
*        ENDIF.
*      ENDLOOP
      PERFORM condition_error USING it_month-worder l_error.
*---end
      it_month-serial   = it_cond-serial.
      it_month-key_code = it_cond-key_code.
      it_month-mark     = l_error.
      MODIFY it_month.
    ENDLOOP.

    LOOP AT it_prev.
      CLEAR: l_error.
*---start#1 wskim 03/18/2005 by hur
*      LOOP AT it_pharse.
*        IF it_pharse-oper = 'OR' .
*          CLEAR: l_error     .
*        ENDIF.
*        IF l_error = 'X'     .
*          CONTINUE           .
*        ENDIF.
*        SELECT SINGLE *
*          FROM ausp
*         WHERE objek = it_prev-worder
*           AND atinn = it_pharse-atinn
*           AND klart = '001'
*           AND atwrt = it_pharse-atwrt
*           AND atflv = it_pharse-atflv .
*
*        IF sy-subrc NE 0      .
*          l_error  = 'X'     .
*        ENDIF.
*      ENDLOOP    .

      PERFORM condition_error USING it_prev-worder l_error.
*---end
      it_prev-serial   = it_cond-serial.
      it_prev-key_code = it_cond-key_code.
      it_prev-mark     = l_error.
      MODIFY it_prev.
    ENDLOOP.


    LOOP AT it_today          .
      CLEAR: l_error          .
*---start#1 wskim 03/18/2005 by hur
*      LOOP AT it_pharse       .
*        IF it_pharse-oper = 'OR' .
*          CLEAR: l_error     .
*        ENDIF.
*        IF l_error = 'X'     .
*          CONTINUE           .
*        ENDIF.
*        SELECT SINGLE *
*          FROM ausp
*         WHERE objek = it_today-worder
*           AND atinn = it_pharse-atinn
*           AND klart = '001'
*           AND atwrt = it_pharse-atwrt
*           AND atflv = it_pharse-atflv .
*
*        IF sy-subrc NE 0      .
*          l_error  = 'X'     .
*        ENDIF.
*      ENDLOOP.
      PERFORM condition_error USING it_today-worder l_error.
*---end
      it_today-serial   = it_cond-serial.
      it_today-key_code = it_cond-key_code.
      it_today-mark     = l_error.
      MODIFY it_today .
    ENDLOOP.

    LOOP AT it_day.
      CLEAR: l_error.
*---start#1 wskim 03/18/2005 by hur
*      LOOP AT it_pharse.
*        IF it_pharse-oper = 'OR' .
*          CLEAR: l_error     .
*        ENDIF.
*        IF l_error = 'X'     .
*          CONTINUE           .
*        ENDIF.
*        SELECT SINGLE *
*          FROM ausp
*         WHERE objek = it_day-worder
*           AND atinn = it_pharse-atinn
*           AND klart = '001'
*           AND atwrt = it_pharse-atwrt
*           AND atflv = it_pharse-atflv .
*
*        IF sy-subrc NE 0      .
*          l_error  = 'X'     .
*        ENDIF.
*      ENDLOOP                 .
      PERFORM condition_error USING it_day-worder l_error.
*---end
      it_day-serial   = it_cond-serial.
      it_day-key_code = it_cond-key_code.
      it_day-mark     = l_error.
      MODIFY it_day.
    ENDLOOP.

    LOOP AT it_plan.
      CLEAR: l_error.
*---start#1 wskim 03/18/2005 by hur
*      LOOP AT it_pharse       .
*        IF it_pharse-oper = 'OR' .
*          CLEAR: l_error     .
*        ENDIF.
*        IF l_error = 'X'     .
*          CONTINUE           .
*        ENDIF.
*        SELECT SINGLE *
*          FROM ausp
*         WHERE objek = it_plan-worder
*           AND atinn = it_pharse-atinn
*           AND klart = '001'
*           AND atwrt = it_pharse-atwrt
*           AND atflv = it_pharse-atflv .
*
*        IF sy-subrc NE 0      .
*          l_error  = 'X'     .
*        ENDIF.
*      ENDLOOP                 .
      PERFORM condition_error USING it_plan-worder l_error.
*---end
      it_plan-serial   = it_cond-serial.
      it_plan-key_code = it_cond-key_code.
      it_plan-mark     = l_error.
      MODIFY it_plan.
    ENDLOOP.

    LOOP AT it_mitu.
      CLEAR: l_error.
*---start#1 wskim 03/18/2005 by hur
*      LOOP AT it_pharse.
*        IF it_pharse-oper = 'OR' .
*          CLEAR: l_error     .
*        ENDIF.
*        IF l_error = 'X'     .
*          CONTINUE           .
*        ENDIF.
*        SELECT SINGLE *
*          FROM ausp
*         WHERE objek = it_mitu-worder
*           AND atinn = it_pharse-atinn
*           AND klart = '001'
*           AND atwrt = it_pharse-atwrt
*           AND atflv = it_pharse-atflv .
*
*        IF sy-subrc NE 0      .
*          l_error  = 'X'     .
*        ENDIF.
*      ENDLOOP                 .
      PERFORM condition_error USING it_mitu-worder l_error.
*---end
      it_mitu-serial   = it_cond-serial.
      it_mitu-key_code = it_cond-key_code.
      it_mitu-mark     = l_error.
      MODIFY it_mitu.
    ENDLOOP.

    " Summarize the IT_SUM-CNT.
    PERFORM calc_month.
    PERFORM calc_prev.
    PERFORM calc_today.
    PERFORM calc_day.
    PERFORM calc_plan.
    PERFORM calc_mitu.
  ENDLOOP.

  " Summary of the Internal Table - IT_DISP
  SORT it_disp       BY serial  .
  READ TABLE it_disp      INDEX 1.
  lt_disp-serial          = it_disp-serial.
  lt_disp-key_code        = it_disp-key_code.

  LOOP AT it_disp .
    IF it_disp-serial = lt_disp-serial.
      lt_disp-m_soff  = lt_disp-m_soff  + it_disp-m_soff  .
      lt_disp-m_trim  = lt_disp-m_trim  + it_disp-m_trim  .
      lt_disp-m_pout  = lt_disp-m_pout  + it_disp-m_pout  .
      lt_disp-m_pin   = lt_disp-m_pin   + it_disp-m_pin   .
      lt_disp-m_bin   = lt_disp-m_bin   + it_disp-m_bin   .
      lt_disp-d_soff  = lt_disp-d_soff  + it_disp-d_soff  .
      lt_disp-d_trim  = lt_disp-d_trim  + it_disp-d_trim  .
      lt_disp-d_pout  = lt_disp-d_pout  + it_disp-d_pout  .
      lt_disp-d_pin   = lt_disp-d_pin   + it_disp-d_pin   .
      lt_disp-d_bin   = lt_disp-d_bin   + it_disp-d_bin   .
      lt_disp-w_soff  = lt_disp-w_soff  + it_disp-w_soff  .
      lt_disp-w_trim  = lt_disp-w_trim  + it_disp-w_trim  .
      lt_disp-w_pout  = lt_disp-w_pout  + it_disp-w_pout  .
      lt_disp-w_pin   = lt_disp-w_pin   + it_disp-w_pin   .
      lt_disp-w_bin   = lt_disp-w_bin   + it_disp-w_bin   .
      lt_disp-c_bin   = lt_disp-c_bin   + it_disp-c_bin   .
      lt_disp-c_paint = lt_disp-c_paint + it_disp-c_paint .
      lt_disp-c_trim  = lt_disp-c_trim  + it_disp-c_trim  .
*     lt_disp-c_tot   = lt_disp-c_tot   + it_disp-c_tot   .
      lt_disp-c_seq   = lt_disp-c_seq   + it_disp-c_seq   .
      lt_disp-p_seq   = lt_disp-p_seq   + it_disp-p_seq   .
      lt_disp-p_mitu  = lt_disp-p_mitu  + it_disp-p_mitu  .
*     lt_disp-p_tot   = lt_disp-p_tot   + it_disp-p_tot   .
      CONTINUE.
    ELSE.
      APPEND lt_disp.
      lt_disp      = it_disp .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_disp  LINES l_index.
  IF l_index > 0.
    APPEND lt_disp.
  ENDIF.
  it_disp[] = lt_disp[].
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  calc_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_month .
  " Summary of the data by Time-stamp...
  CLEAR: it_disp            .
  SORT it_month             .

  LOOP AT it_month .
    IF it_month-mark = space .
      it_disp-m_soff   = it_disp-m_soff + it_month-m_soff .
      it_disp-m_trim   = it_disp-m_trim + it_month-m_trim .
      it_disp-m_pout   = it_disp-m_pout + it_month-m_pout .
      it_disp-m_pin    = it_disp-m_pin  + it_month-m_pin  .
      it_disp-m_bin    = it_disp-m_bin  + it_month-m_bin  .
    ENDIF.
    it_disp-serial   = it_month-serial  .
    it_disp-key_code = it_month-key_code.
    APPEND it_disp.    CLEAR: it_disp  .
  ENDLOOP.
ENDFORM.                    " calc_MONTH

*&---------------------------------------------------------------------*
*&      Form  calc_PREV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_prev  .
  " Summary of the data by Time-stamp...
  CLEAR: it_disp            .
  SORT it_prev              .

  LOOP AT it_prev  .
    IF it_prev-mark = space .
      it_disp-d_soff   = it_disp-d_soff + it_prev-d_soff .
      it_disp-d_trim   = it_disp-d_trim + it_prev-d_trim .
      it_disp-d_pout   = it_disp-d_pout + it_prev-d_pout .
      it_disp-d_pin    = it_disp-d_pin  + it_prev-d_pin  .
      it_disp-d_bin    = it_disp-d_bin  + it_prev-d_bin  .
    ENDIF.
    it_disp-serial   = it_prev-serial  .
    it_disp-key_code = it_prev-key_code.
    APPEND it_disp.    CLEAR: it_disp  .
  ENDLOOP.
ENDFORM.                    " calc_PREV

*&---------------------------------------------------------------------*
*&      Form  calc_today
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_today .
  " Summary of the data by Time-stamp...
  CLEAR: it_disp            .
  SORT it_today             .

  LOOP AT it_today .
    IF it_today-mark = space .
      it_disp-w_soff   = it_disp-w_soff + it_today-d_soff .
      it_disp-w_trim   = it_disp-w_trim + it_today-d_trim .
      it_disp-w_pout   = it_disp-w_pout + it_today-d_pout .
      it_disp-w_pin    = it_disp-w_pin  + it_today-d_pin  .
      it_disp-w_bin    = it_disp-w_bin  + it_today-d_bin  .
    ENDIF.
    it_disp-serial   = it_today-serial  .
    it_disp-key_code = it_today-key_code.
    APPEND it_disp.    CLEAR: it_disp  .
  ENDLOOP.
ENDFORM.                    " calc_today

*&---------------------------------------------------------------------*
*&      Form  calc_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_day   .
  " Summary of the data by Time-stamp...
  CLEAR: it_disp            .
  SORT it_day               .

  LOOP AT it_day   .
    IF it_day-mark = space .
      it_disp-c_bin    = it_disp-c_bin   + it_day-c_bin  .
      it_disp-c_trim   = it_disp-c_trim  + it_day-c_trim .
      it_disp-c_paint  = it_disp-c_paint + it_day-c_paint.
      it_disp-c_seq    = it_disp-c_seq   + it_day-c_seq  .
    ENDIF.
    it_disp-serial   = it_day-serial  .
    it_disp-key_code = it_day-key_code.
    APPEND it_disp.    CLEAR: it_disp  .
  ENDLOOP.
ENDFORM.                    " calc_DAY

*&---------------------------------------------------------------------*
*&      Form  calc_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_plan  .
  " Summary of the data by Time-stamp...
  CLEAR: it_disp            .
  SORT it_plan              .

  LOOP AT it_plan  .
    IF it_plan-mark = space.
      it_disp-p_seq    = it_disp-p_seq  + it_plan-p_seq  .
    ENDIF.
    it_disp-serial   = it_plan-serial  .
    it_disp-key_code = it_plan-key_code.
    APPEND it_disp.    CLEAR: it_disp  .
  ENDLOOP.
ENDFORM.                    " calc_PLAN

*&---------------------------------------------------------------------*
*&      Form  calc_MITU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_mitu  .
  " Summary of the data by Time-stamp...
  CLEAR: it_disp            .
  SORT it_mitu              .

  LOOP AT it_mitu  .
    IF it_mitu-mark = space.
      it_disp-p_mitu   = it_disp-p_mitu + it_mitu-p_mitu .
    ENDIF.
    it_disp-serial   = it_mitu-serial  .
    it_disp-key_code = it_mitu-key_code.
    APPEND it_disp.    CLEAR: it_disp  .
  ENDLOOP.
ENDFORM.                    " calc_MITU

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  DATA: l_cnt                LIKE it_disp-serial.
  DELETE FROM ztpp_alc_prod CLIENT SPECIFIED WHERE mandt = sy-mandt.

  SORT it_disp BY serial .
  MODIFY ztpp_alc_prod    FROM TABLE it_disp .
ENDFORM.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summary_data     .
  DATA: l_size           TYPE i      ,
        lw_disp          LIKE it_disp,
        lt_disp          LIKE TABLE OF it_disp         WITH HEADER LINE.

  " Summary Internal Table of IT_DISP...
  SORT it_disp BY serial key_code alc_vals.
  READ TABLE it_disp INDEX 1.
  lw_disp = it_disp.

  LOOP AT it_disp.
    IF it_disp-serial   = lw_disp-serial   AND
       it_disp-key_code = lw_disp-key_code AND
       it_disp-alc_vals = lw_disp-alc_vals   .
      lt_disp-m_soff   = lt_disp-m_soff   + it_disp-m_soff   .
      lt_disp-m_trim   = lt_disp-m_trim   + it_disp-m_trim   .
      lt_disp-m_pout   = lt_disp-m_pout   + it_disp-m_pout   .
      lt_disp-m_pin    = lt_disp-m_pin    + it_disp-m_pin    .
      lt_disp-m_bin    = lt_disp-m_bin    + it_disp-m_bin    .
      lt_disp-d_soff   = lt_disp-d_soff   + it_disp-d_soff   .
      lt_disp-d_trim   = lt_disp-d_trim   + it_disp-d_trim   .
      lt_disp-d_pout   = lt_disp-d_pout   + it_disp-d_pout   .
      lt_disp-d_pin    = lt_disp-d_pin    + it_disp-d_pin    .
      lt_disp-d_bin    = lt_disp-d_bin    + it_disp-d_bin    .
      lt_disp-w_soff   = lt_disp-w_soff   + it_disp-w_soff   .
      lt_disp-w_trim   = lt_disp-w_trim   + it_disp-w_trim   .
      lt_disp-w_pout   = lt_disp-w_pout   + it_disp-w_pout   .
      lt_disp-w_pin    = lt_disp-w_pin    + it_disp-w_pin    .
      lt_disp-w_bin    = lt_disp-w_bin    + it_disp-w_bin    .
      lt_disp-c_bin    = lt_disp-c_bin    + it_disp-c_bin    .
      lt_disp-c_paint  = lt_disp-c_paint  + it_disp-c_paint  .
      lt_disp-c_trim   = lt_disp-c_trim   + it_disp-c_trim   .
      lt_disp-c_seq    = lt_disp-c_seq    + it_disp-c_seq    .
      lt_disp-p_seq    = lt_disp-p_seq    + it_disp-p_seq    .
      lt_disp-p_mitu   = lt_disp-p_mitu   + it_disp-p_mitu   .
    ELSE.
      lt_disp-serial   = lw_disp-serial   .
      lt_disp-key_code = lw_disp-key_code .
      lt_disp-alc_vals = lw_disp-alc_vals .
      lt_disp-model    = lw_disp-model    .
      lt_disp-body_ser = lw_disp-body_ser .
      lt_disp-p_tot    = lt_disp-c_seq + lt_disp-p_seq   .
      lt_disp-c_tot    = lt_disp-c_bin + lt_disp-c_paint +
                         lt_disp-c_trim .
      APPEND lt_disp.  CLEAR: lt_disp       .
      lw_disp = it_disp .  lt_disp = it_disp .
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE it_disp LINES l_size.
  IF l_size > 0.
    lt_disp-serial   = lw_disp-serial   .
    lt_disp-key_code = lw_disp-key_code .
    lt_disp-alc_vals = lw_disp-alc_vals .
    lt_disp-model    = lw_disp-model    .
    lt_disp-body_ser = lw_disp-body_ser .
    lt_disp-p_tot    = lt_disp-c_seq + lt_disp-p_seq   .
    lt_disp-c_tot    = lt_disp-c_bin + lt_disp-c_paint +
                       lt_disp-c_trim .
    APPEND lt_disp.  CLEAR: lt_disp       .
  ENDIF.
  it_disp[] = lt_disp[].
ENDFORM.                    " SUMMARY_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_condition.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cond
    FROM ztpp_plan_key .
ENDFORM.                    " GET_CONDITION

*&---------------------------------------------------------------------*
*&      Form  GET_COND_PARSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COND  text
*----------------------------------------------------------------------*
FORM get_cond_parsing  .
  DATA: lt_worder       LIKE TABLE OF zspp_master_asup WITH HEADER LINE,
        lt_cond         LIKE TABLE OF zspp_condition   WITH HEADER LINE,
        l_cond          LIKE zspp_condition-vals ,
        l_success, l_fail.

  CLEAR: it_pharse, it_pharse[].
  l_cond = it_cond-cond.

  CALL FUNCTION 'Z_FPP_COMPILE_CONDITION'
       EXPORTING
            i_cond          = l_cond
            i_check         = 'X'
       IMPORTING
            o_success       = l_success
            o_fail          = l_fail
       TABLES
            t_worder        = lt_worder
            t_cond          = lt_cond
       EXCEPTIONS
            condition_error = 1
            err_paren       = 2
            err_operation   = 3
            err_relation    = 4
            err_values      = 5
            err_fields      = 6
            OTHERS          = 7.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    LOOP AT lt_cond   .
      MOVE-CORRESPONDING lt_cond  TO it_pharse.
      PERFORM get_atinn  USING lt_cond-vals   .
      APPEND it_pharse.
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
FORM get_atinn               USING pa_vals  .
  DATA: lw_cabn              LIKE cabn      ,
        l_size               TYPE i         ,
        l_vals               LIKE zspp_condition-vals,
        l_atnam              LIKE cabn-atnam.

  l_atnam = it_pharse-string .
  l_vals  = pa_vals          .
  l_size  = strlen( l_vals ) - 2 .
  l_vals  = l_vals+1(l_size) .
* SHIFT   L_VALS  LEFT       .
* SHIFT   L_VALS  RIGHT      .

  SELECT SINGLE * INTO lw_cabn
    FROM cabn
   WHERE atnam = l_atnam .

  it_pharse-atinn = lw_cabn-atinn .
  CASE lw_cabn-atfor.
    WHEN 'CHAR' .
      it_pharse-atwrt = l_vals       .
    WHEN 'CURR' .
    WHEN 'DATE' OR 'NUM'  .
      it_pharse-atflv = l_vals       .
    WHEN 'TIME' .
    WHEN 'UDEF' .
  ENDCASE.
ENDFORM.                    " get_atinn

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*----------------------------------------------------------------------*
FORM get_data_month USING    pa_month.
  DATA: l_fdate     TYPE d           ,
        l_tdate     TYPE d           ,
        l_atinn     LIKE ausp-atinn  ,
        l_atinne    LIKE ausp-atinn  ,
        l_atinni    LIKE ausp-atinn  ,
        l_atwrt     LIKE ausp-atwrt  ,
        l_worder    LIKE ausp-atinn  ,
        l_fval      LIKE ausp-atflv  ,
        l_tval      LIKE ausp-atflv  ,
        l_no(8)     TYPE n           .

  CONCATENATE pa_month '01'  INTO l_fdate.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_fdate
       IMPORTING
            last_day_of_month = l_tdate
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

  IF l_tdate > sy-datum.
    l_tdate = sy-datum  .
  ENDIF.

  l_fval = l_no = l_fdate .
  l_tval = l_no = l_tdate .

  PERFORM get_date_atinn  USING 'P_WORK_ORDER'     l_worder .
  PERFORM get_date_atinn  USING 'P_EXT_COLOR'      l_atinne .
  PERFORM get_date_atinn  USING 'P_INT_COLOR'      l_atinni .
  PERFORM get_date_atinn  USING 'P_RP18_SHOP_DATE' l_atinn  .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv >= l_fval
                                  AND atflv <= l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '18'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP06_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv >= l_fval
                                  AND atflv <= l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '06'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP04_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv >= l_fval
                                  AND atflv <= l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  it_ausp-status = '04'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP02_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv >= l_fval
                                  AND atflv <= l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '02'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP01_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv >= l_fval
                                  AND atflv <= l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '01'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM arrange_month  .
ENDFORM.                    " GET_DATA_MONTH

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_PREV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*----------------------------------------------------------------------*
FORM get_data_prev  USING    pa_prev .
  DATA: l_atinn     LIKE ausp-atinn  ,
        l_atinne    LIKE ausp-atinn  ,
        l_atinni    LIKE ausp-atinn  ,
        l_atwrt     LIKE ausp-atwrt  ,
        l_tval      LIKE ausp-atflv  ,
        l_worder    LIKE ausp-atinn  ,
        l_no(8)     TYPE n           .

  l_tval = l_no = pa_prev.

  PERFORM get_date_atinn  USING 'P_WORK_ORDER'     l_worder .
  PERFORM get_date_atinn  USING 'P_EXT_COLOR'      l_atinne .
  PERFORM get_date_atinn  USING 'P_INT_COLOR'      l_atinni .
  PERFORM get_date_atinn  USING 'P_RP18_SHOP_DATE' l_atinn  .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv = l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '18'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP06_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv = l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '06'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP04_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv = l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '04'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP02_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv = l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '02'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM get_date_atinn  USING 'P_RP01_SHOP_DATE' l_atinn  .
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atflv = l_tval )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '01'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM arrange_prev   .
ENDFORM.                    " GET_DATA_PREV

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_CURR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*----------------------------------------------------------------------*
FORM get_data_curr  .
  DATA: l_atinn     LIKE ausp-atinn  ,
        l_tval      LIKE ausp-atflv  ,
        l_worder    LIKE ausp-atinn  ,
        l_no(8)     TYPE n           .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan
   WHERE status NE space .

  PERFORM arrange_curr   .
ENDFORM.                    " GET_DATA_CURR

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*----------------------------------------------------------------------*
FORM get_data_plan  .
  DATA: l_atinn     LIKE ausp-atinn  ,
        l_tval      LIKE ausp-atflv  ,
        l_worder    LIKE ausp-atinn  ,
        l_no(8)     TYPE n           .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE gubb = 'A'      .

  PERFORM arrange_plan   .
ENDFORM.                    " GET_DATA_PLAN

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_MITU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*----------------------------------------------------------------------*
FORM get_data_mitu  .
  DATA: l_atinn     LIKE ausp-atinn  ,
        l_atinne    LIKE ausp-atinn  ,
        l_atinni    LIKE ausp-atinn  ,
        l_atwrt     LIKE ausp-atwrt  ,
        l_tval      LIKE ausp-atflv  ,
        l_worder    LIKE ausp-atinn  ,
        l_no(8)     TYPE n           .

  PERFORM get_date_atinn  USING 'P_WORK_ORDER'     l_worder .
  PERFORM get_date_atinn  USING 'P_EXT_COLOR'      l_atinne .
  PERFORM get_date_atinn  USING 'P_INT_COLOR'      l_atinni .
  PERFORM get_date_atinn  USING 'P_MITU'           l_atinn  .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP
                                WHERE atinn = l_atinn
                                  AND klart = '002'
                                  AND atwrt = 'Y'    )
     AND atinn = l_worder
     AND klart = '002'    .

  LOOP AT it_ausp WHERE status = space.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinne
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp
     WHERE objek = it_ausp-objek
       AND atinn = l_atinni
       AND klart = '002'        .
    CONCATENATE it_ausp-atwrt l_atwrt INTO it_ausp-atwrt.
    MODIFY it_ausp.
  ENDLOOP.

  it_ausp-status = '01'.
  MODIFY it_ausp TRANSPORTING status WHERE status = space.

  PERFORM arrange_mitu   .
ENDFORM.                    " GET_DATA_MITU

*&---------------------------------------------------------------------*
*&      Form  get_start_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM get_start_day USING    pa_datum.
  CLEAR: ztpp_common_vals.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1 .

  pa_datum = ztpp_common_vals-dates.
ENDFORM.                    " GET_START_DAY

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
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
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
ENDFORM.                    " READ_WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  get_DATE_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1673   text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
FORM get_date_atinn USING    pa_char  pa_atinn.
  CLEAR: pa_atinn.
  SELECT SINGLE atinn INTO pa_atinn
    FROM cabn
   WHERE atnam = pa_char .
ENDFORM.                    " get_DATE_atinn

*&---------------------------------------------------------------------*
*&      Form  ARRANGE_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM arrange_month.
  DATA: l_cnt              TYPE i         ,
        l_size             TYPE i         ,
        lt_month           LIKE TABLE OF it_month      WITH HEADER LINE,
        l_worder           LIKE ausp-atwrt,
        l_model            LIKE ausp-objek,
        l_status           LIKE ztpp_input_plan-status.

  SORT it_ausp BY atwrt status .
  READ TABLE it_ausp INDEX 1   .
  l_worder = it_ausp-atwrt     .    l_status = it_ausp-status.
  l_model  = it_ausp-objek(3)  .

  LOOP AT it_ausp.
    IF l_worder = it_ausp-atwrt AND  l_model  = it_ausp-objek(3) AND
       l_status = it_ausp-status   .
      l_cnt = l_cnt + 1           .
      CONTINUE                    .
    ELSE.
      CASE l_status.
        WHEN '18'  .
          it_month-m_soff = l_cnt .
        WHEN '06'  .
          it_month-m_trim = l_cnt .
        WHEN '04'  .
          it_month-m_pout = l_cnt .
        WHEN '02'  .
          it_month-m_pin  = l_cnt .
        WHEN '01'  .
          it_month-m_bin  = l_cnt .
        WHEN OTHERS     .
*          IT_MONTH-M_SOFF = L_CNT .
      ENDCASE.
      it_month-model   = l_model .
      it_month-worder  = l_worder.
      it_month-status  = l_status.
      APPEND it_month .  CLEAR: it_month.
      l_worder = it_ausp-atwrt     .    l_status = it_ausp-status.
      l_model  = it_ausp-objek(3)  .    l_cnt = 1 .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_ausp LINES l_size.
  IF l_size > 0 .
    CASE l_status.
      WHEN '18'  .
        it_month-m_soff = l_cnt .
      WHEN '06'  .
        it_month-m_trim = l_cnt .
      WHEN '04'  .
        it_month-m_pout = l_cnt .
      WHEN '02'  .
        it_month-m_pin  = l_cnt .
      WHEN '01'  .
        it_month-m_bin  = l_cnt .
      WHEN OTHERS     .
*          IT_MONTH-M_SOFF = L_CNT .
    ENDCASE.
    it_month-model   = l_model .
    it_month-worder  = l_worder.
    it_month-status  = l_status.
    APPEND it_month .  CLEAR: it_month.
  ENDIF.

  SORT it_month BY worder model status .
  lt_month[] = it_month[].  CLEAR: it_month, it_month[].
  READ TABLE lt_month INDEX 1  .
  l_worder = lt_month-worder   .    l_model  = lt_month-model.

  LOOP AT lt_month.
    IF l_worder = lt_month-worder AND l_model  = lt_month-model.
      it_month-m_soff = it_month-m_soff + lt_month-m_soff.
      it_month-m_trim = it_month-m_trim + lt_month-m_trim.
      it_month-m_pout = it_month-m_pout + lt_month-m_pout.
      it_month-m_pin  = it_month-m_pin  + lt_month-m_pin .
      it_month-m_bin  = it_month-m_bin  + lt_month-m_bin .
      CONTINUE .
    ELSE.
      it_month-model  = l_model.
      it_month-worder = l_worder .
      APPEND it_month.  CLEAR: it_month.
      it_month-m_soff = lt_month-m_soff.
      it_month-m_trim = lt_month-m_trim.
      it_month-m_pout = lt_month-m_pout.
      it_month-m_pin  = lt_month-m_pin .
      it_month-m_bin  = lt_month-m_bin .
      l_worder = lt_month-worder   .    l_model  = lt_month-model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_month LINES l_size.
  IF l_size > 0 .
    it_month-model  = l_model.
    it_month-worder = l_worder .
    APPEND it_month.  CLEAR: it_month.
  ENDIF.
ENDFORM.                    " ARRANGE_MONTH

*&---------------------------------------------------------------------*
*&      Form  ARRANGE_PREV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM arrange_prev .
  DATA: l_cnt              TYPE i         ,
        l_size             TYPE i         ,
        lt_prev            LIKE TABLE OF it_prev       WITH HEADER LINE,
        l_worder           LIKE ausp-atwrt,
        l_model            LIKE ausp-objek,
        l_status           LIKE ztpp_input_plan-status.

  SORT it_ausp BY atwrt status .
  READ TABLE it_ausp INDEX 1   .
  l_worder = it_ausp-atwrt     .    l_status = it_ausp-status.
  l_model  = it_ausp-objek(3)  .

  LOOP AT it_ausp.
    IF l_worder = it_ausp-atwrt AND  l_model  = it_ausp-objek(3) AND
       l_status = it_ausp-status   .
      l_cnt = l_cnt + 1           .
      CONTINUE                    .
    ELSE.
      CASE l_status.
        WHEN '18'  .
          it_prev-d_soff = l_cnt .
        WHEN '06'  .
          it_prev-d_trim = l_cnt .
        WHEN '04'  .
          it_prev-d_pout = l_cnt .
        WHEN '02'  .
          it_prev-d_pin  = l_cnt .
        WHEN '01'  .
          it_prev-d_bin  = l_cnt .
        WHEN OTHERS     .
*          it_PREV-D_SOFF = L_CNT .
      ENDCASE.
      it_prev-model   = l_model .
      it_prev-worder  = l_worder.
      it_prev-status  = l_status.
      APPEND it_prev .  CLEAR: it_prev .
      l_worder = it_ausp-atwrt     .    l_status = it_ausp-status.
      l_model  = it_ausp-objek(3)  .    l_cnt = 1 .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_ausp LINES l_size.
  IF l_size > 0 .
    CASE l_status.
      WHEN '18'  .
        it_prev-d_soff = l_cnt .
      WHEN '06'  .
        it_prev-d_trim = l_cnt .
      WHEN '04'  .
        it_prev-d_pout = l_cnt .
      WHEN '02'  .
        it_prev-d_pin  = l_cnt .
      WHEN '01'  .
        it_prev-d_bin  = l_cnt .
      WHEN OTHERS     .
*          it_PREV-D_SOFF = L_CNT .
    ENDCASE.
    it_prev-model   = l_model .
    it_prev-worder  = l_worder.
    it_prev-status  = l_status.
    APPEND it_prev .  CLEAR: it_prev.
  ENDIF.

  SORT it_prev BY worder model status.
  lt_prev[] = it_prev[].  CLEAR: it_prev, it_prev[].
  READ TABLE lt_prev INDEX 1  .
  l_worder = lt_prev-worder   .    l_model  = lt_prev-model.

  LOOP AT lt_prev.
    IF l_worder = lt_prev-worder AND l_model  = lt_prev-model.
      it_prev-d_soff = it_prev-d_soff + lt_prev-d_soff.
      it_prev-d_trim = it_prev-d_trim + lt_prev-d_trim.
      it_prev-d_pout = it_prev-d_pout + lt_prev-d_pout.
      it_prev-d_pin  = it_prev-d_pin  + lt_prev-d_pin .
      it_prev-d_bin  = it_prev-d_bin  + lt_prev-d_bin .
      CONTINUE .
    ELSE.
      it_prev-model  = l_model.
      it_prev-worder = l_worder .
      APPEND it_prev.  CLEAR: it_prev.
      it_prev-d_soff = lt_prev-d_soff.
      it_prev-d_trim = lt_prev-d_trim.
      it_prev-d_pout = lt_prev-d_pout.
      it_prev-d_pin  = lt_prev-d_pin .
      it_prev-d_bin  = lt_prev-d_bin .
      l_worder = lt_prev-worder   .    l_model  = lt_prev-model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_prev LINES l_size.
  IF l_size > 0 .
    it_prev-model  = l_model.
    it_prev-worder = l_worder ..
    APPEND it_prev.  CLEAR: it_prev.
  ENDIF.
ENDFORM.                    " ARRANGE_PREV

*&---------------------------------------------------------------------*
*&      Form  ARRANGE_CURR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM arrange_curr .
  DATA: l_cnt              TYPE i         ,
        l_size             TYPE i         ,
        lt_day             LIKE TABLE OF it_day        WITH HEADER LINE,
        l_worder           LIKE mara-matnr,
        l_extc             TYPE zextc     ,
        l_intc             TYPE zintc     ,
        l_model            TYPE zpp_model ,
        l_status           LIKE ztpp_input_plan-status.

  SORT it_data BY work_order extc intc status .
  READ TABLE it_data INDEX 1   .
  l_model  = it_data-modl      .    l_status = it_data-status.
  l_worder = it_data-work_order.    l_extc   = it_data-extc  .
  l_intc   = it_data-intc      .

  LOOP AT it_data.
    IF l_worder = it_data-work_order AND l_intc   = it_data-intc  AND
       l_model  = it_data-modl       AND l_extc   = it_data-extc  AND
       l_status = it_data-status   .
      l_cnt = l_cnt + 1           .
      CONTINUE                    .
    ELSE.
      CASE l_status.
** changed by Furong on 10/26/09
*        WHEN '07' OR '08' OR '09' OR '10' OR '11' OR '12' OR
*             '13' OR '14' OR '15' OR '16' OR '17' OR '18' OR '06' .
        WHEN '07' OR '08' OR '09' OR '10' OR '11' OR '12' OR
             '13' OR '14' OR '15' OR '16' OR '17' OR '06' .
** End of change
          it_day-c_trim  = l_cnt  .
        WHEN '02' OR '03' OR '04' OR '05' .
          it_day-c_paint = l_cnt  .
        WHEN '01'  .
          it_day-c_bin   = l_cnt  .
        WHEN '00'  .
          it_day-c_seq   = l_cnt  .
      ENDCASE.
      it_day-model   = l_model   .
      CONCATENATE l_worder l_extc l_intc INTO  it_day-worder.
      it_day-status  = l_status.
      APPEND it_day .  CLEAR: it_day .
      l_worder = it_data-work_order.    l_status = it_data-status.
      l_extc   = it_data-extc      .    l_intc   = it_data-intc  .
      l_model  = it_data-modl      .    l_cnt = 1 .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_data LINES l_size.
  IF l_size > 0 .

** changed by Furong on 10/26/09
*    CASE l_status.
*      WHEN '07' OR '08' OR '09' OR '10' OR '11' OR '12' OR
*           '13' OR '14' OR '15' OR '16' OR '17' OR '18' .
*        it_day-c_trim  = l_cnt  .
*      WHEN '05' OR '06'         .
**       it_day-c_trim  = l_cnt  .
*      WHEN '02' OR '03' OR '04' .
*        it_day-c_paint = l_cnt  .
*      WHEN '01'  .
*        it_day-c_bin   = l_cnt  .
*      WHEN '00'  .
*        it_day-c_seq   = l_cnt  .
*    ENDCASE.
*    it_day-model   = l_model   .
*    CONCATENATE l_worder l_extc l_intc INTO  it_day-worder.
*    it_day-status  = l_status.
*    APPEND it_day .  CLEAR: it_day .
*

      CASE l_status.
** changed by Furong on 10/26/09
*        WHEN '07' OR '08' OR '09' OR '10' OR '11' OR '12' OR
*             '13' OR '14' OR '15' OR '16' OR '17' OR '18' OR '06' .
        WHEN '07' OR '08' OR '09' OR '10' OR '11' OR '12' OR
             '13' OR '14' OR '15' OR '16' OR '17' OR '06' .
** End of change
          it_day-c_trim  = l_cnt  .
        WHEN '02' OR '03' OR '04' OR '05' .
          it_day-c_paint = l_cnt  .
        WHEN '01'  .
          it_day-c_bin   = l_cnt  .
        WHEN '00'  .
          it_day-c_seq   = l_cnt  .
      ENDCASE.
      it_day-model   = l_model   .
      CONCATENATE l_worder l_extc l_intc INTO  it_day-worder.
      it_day-status  = l_status.
      APPEND it_day .  CLEAR: it_day .

*** End of change on 10/26/09

  ENDIF.

  SORT it_day BY worder model status.
  lt_day[] = it_day[].  CLEAR: it_day, it_day[].
  READ TABLE lt_day INDEX 1  .
  l_worder = lt_day-worder   .    l_model  = lt_day-model.

  LOOP AT lt_day.
    IF l_worder = lt_day-worder AND l_model  = lt_day-model.
      it_day-c_trim  = it_day-c_trim  + lt_day-c_trim .
      it_day-c_paint = it_day-c_paint + lt_day-c_paint.
      it_day-c_bin   = it_day-c_bin   + lt_day-c_bin .
      it_day-c_seq   = it_day-c_seq   + lt_day-c_seq .
      CONTINUE .
    ELSE.
      it_day-model  = l_model.
      it_day-worder = l_worder .
      APPEND it_day.  CLEAR: it_day.
      it_day-c_trim  = lt_day-c_trim .
      it_day-c_paint = lt_day-c_paint.
      it_day-c_bin   = lt_day-c_bin .
      it_day-c_seq   = lt_day-c_seq .
      l_worder = lt_day-worder   .    l_model  = lt_day-model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_day LINES l_size.
  IF l_size > 0.
    it_day-model  = l_model.
    it_day-worder = l_worder .
    APPEND it_day.  CLEAR: it_day.
  ENDIF.
ENDFORM.                    " ARRANGE_CURR

*&---------------------------------------------------------------------*
*&      Form  ARRANGE_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM arrange_plan .
  DATA: l_cnt              TYPE i         ,
        l_size             TYPE i         ,
        l_ordr             TYPE zordr     ,
        l_dist             TYPE zdist     ,
        l_extc             TYPE zextc     ,
        l_intc             TYPE zintc     ,
        l_modl             TYPE zmodl_a   ,
        l_worder           LIKE mara-matnr,
        lt_plan            LIKE TABLE OF it_plan       WITH HEADER LINE.

  SORT it_7jb  BY ordr dist extc intc .
  READ TABLE it_7jb  INDEX 1   .
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.  l_extc = it_7jb-extc.
  l_modl = it_7jb-modl.  l_intc = it_7jb-intc.

  LOOP AT it_7jb .
    IF l_ordr = it_7jb-ordr AND  l_dist = it_7jb-dist  AND
       l_extc = it_7jb-extc AND  l_intc = it_7jb-intc  AND
       l_modl = it_7jb-modl.
      l_cnt = l_cnt + 1           .
      CONTINUE                    .
    ELSE.
      it_plan-model   = l_modl  .
      it_plan-p_seq   = l_cnt   .
      CONCATENATE l_ordr l_dist l_extc l_intc INTO it_plan-worder .
      APPEND it_plan .  CLEAR: it_plan .
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      l_modl = it_7jb-modl.  l_intc = it_7jb-intc.
      l_extc = it_7jb-extc.            l_cnt = 1 .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_7jb  LINES l_size.
  IF l_size > 0 .
    it_plan-model   = l_modl  .
    it_plan-p_seq   = l_cnt   .
    CONCATENATE l_ordr l_dist l_extc l_intc INTO it_plan-worder .
    APPEND it_plan .  CLEAR: it_plan.
  ENDIF.

  SORT it_plan BY worder model .
  lt_plan[] = it_plan[].  CLEAR: it_plan, it_plan[].
  READ TABLE lt_plan INDEX 1  .
  l_worder = lt_plan-worder   .    l_modl   = lt_plan-model.

  LOOP AT lt_plan.
    IF l_worder = lt_plan-worder AND l_modl   = lt_plan-model.
      it_plan-p_seq  = it_plan-p_seq  + lt_plan-p_seq .
      CONTINUE .
    ELSE.
      it_plan-model  = l_modl .
      it_plan-worder = l_worder .
      APPEND it_plan.  CLEAR: it_plan.
      it_plan-p_seq  = lt_plan-p_seq .
      l_worder = lt_plan-worder   .    l_modl   = lt_plan-model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_plan LINES l_size.
  IF l_size > 0.
    it_plan-model  = l_modl .
    it_plan-worder = l_worder .
    APPEND it_plan.  CLEAR: it_plan.
  ENDIF.
ENDFORM.                    " ARRANGE_PLAN

*&---------------------------------------------------------------------*
*&      Form  ARRANGE_MITU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM arrange_mitu .
  DATA: l_cnt              TYPE i         ,
        l_size             TYPE i         ,
        lt_mitu            LIKE TABLE OF it_mitu       WITH HEADER LINE,
        l_worder           LIKE ausp-atwrt,
        l_model            LIKE ausp-objek,
        l_status           LIKE ztpp_input_plan-status.

  SORT it_ausp BY atwrt        .
  READ TABLE it_ausp INDEX 1   .
  l_worder = it_ausp-atwrt     .    l_model  = it_ausp-objek(3)  .

  LOOP AT it_ausp.
    IF l_worder = it_ausp-atwrt AND  l_model  = it_ausp-objek(3) .
      l_cnt = l_cnt + 1           .
      CONTINUE                    .
    ELSE.
      it_mitu-p_mitu  = l_cnt .
      it_mitu-model   = l_model .
      it_mitu-worder  = l_worder.
      APPEND it_mitu .  CLEAR: it_mitu .
      l_worder = it_ausp-atwrt     .    l_status = it_ausp-status.
      l_model  = it_ausp-objek(3)  .    l_cnt = 1 .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_ausp LINES l_size.
  IF l_size > 0 .
    it_mitu-p_mitu  = l_cnt .
    it_mitu-model   = l_model .
    it_mitu-worder  = l_worder.
    it_mitu-status  = l_status.
    APPEND it_mitu .  CLEAR: it_mitu.
  ENDIF.

  SORT it_mitu BY worder model status.
  lt_mitu[] = it_mitu[].  CLEAR: it_mitu, it_mitu[].
  READ TABLE lt_mitu INDEX 1  .
  l_worder = lt_mitu-worder   .    l_model  = lt_mitu-model.

  LOOP AT lt_mitu.
    IF l_worder = lt_mitu-worder AND l_model  = lt_mitu-model.
      it_mitu-p_mitu = it_mitu-p_mitu + lt_mitu-p_mitu.
      CONTINUE .
    ELSE.
      it_mitu-model  = l_model.
      it_mitu-worder = l_worder .
      APPEND it_mitu.  CLEAR: it_mitu.
      it_mitu-p_mitu = lt_mitu-p_mitu.
      l_worder = lt_mitu-worder   .    l_model  = lt_mitu-model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_mitu LINES l_size.
  IF l_size > 0 .
    it_mitu-model  = l_model.
    it_mitu-worder = l_worder .
    APPEND it_mitu.  CLEAR: it_mitu.
  ENDIF.
ENDFORM.                    " ARRANGE_MITU
*&---------------------------------------------------------------------*
*&      Form  condition_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MONTH_WORDER  text
*      -->P_L_ERROR  text
*----------------------------------------------------------------------*
FORM condition_error USING    p_value
                              l_error.
  DATA : f_oper LIKE it_pharse-oper.
  CLEAR f_oper.
  LOOP AT it_pharse.
    IF it_pharse-op = '!'.
      SELECT SINGLE * FROM ausp
       WHERE objek = p_value
         AND atinn = it_pharse-atinn
         AND klart = '001'
         AND atwrt = it_pharse-atwrt
         AND atflv = it_pharse-atflv .
      IF sy-subrc EQ 0.
        l_error  = 'X'.
        CONTINUE.
      ELSE.
        CLEAR l_error.
      ENDIF.
    ELSE.
      IF f_oper EQ 'X' AND l_error EQ ' '.
        READ TABLE it_pharse WITH KEY oper =  '@'.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDIF.
      IF l_error = 'X'.
        READ TABLE it_pharse WITH KEY oper =  '&'.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      SELECT SINGLE * FROM ausp
       WHERE objek = p_value
         AND atinn = it_pharse-atinn
         AND klart = '001'
         AND atwrt = it_pharse-atwrt
         AND atflv = it_pharse-atflv .
      IF sy-subrc NE 0.
        l_error  = 'X'.
        CONTINUE.
      ELSE.
        CLEAR l_error.f_oper = 'X'.
*        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " condition_error
