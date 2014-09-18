************************************************************************
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
* Change : Change the Time Tule(Tack-Time --> Lead Time)
***********************************************************************
* 09/29/2004  YONGPING LI  UD1K912346 MOVE PREVIOUS DAY INITIAL DATA TO
*                                     EVENT AT SELECTION-SCREEN.
*#1 03/08/2005   wskim     UD1K914858 Condition logic supplement
*#2 03/22/2005   wskim                Extend plan
***********************************************************************
REPORT  zapp903r_alc_binput   MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ausp,ztpp_fwo.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_data        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_input_plan.
DATA: END OF it_data.

DATA: BEGIN OF it_pharse      OCCURS 0        .
        INCLUDE STRUCTURE     zspp_condition  .
DATA:   atinn                 LIKE cabn-atinn ,
        atwrt                 LIKE ausp-atwrt ,
        atflv                 LIKE ausp-atflv ,
      END OF it_pharse.

DATA: BEGIN OF it_sum         OCCURS 0,
        rp                    LIKE ztpp_plan_key-serial,
        worder                LIKE mara-matnr,
        status                LIKE ztpp_input_plan-status,
        cond                  LIKE ztpp_plan_key-key_code,
        hours                 TYPE i  ,
        extc                  LIKE ztpp_input_plan-extc,
        intc                  LIKE ztpp_input_plan-intc,
        mitu                  TYPE zmitu,
        mitucnt               TYPE i  ,
        cnt                   TYPE i  ,
      END OF it_sum .

DATA: BEGIN OF it_master      OCCURS 0,
        seq                   TYPE i  ,             " Sequence
        date                  TYPE d  ,             " Date
        day                   LIKE kapa-tagnr,      " Day
        shift                 LIKE kapa-schnr,      " Shift
        time                  TYPE kapendzt  ,      " Times for working
        tun                   TYPE ld_lantu  ,      " Unit  for Time
        uph                   TYPE zvpp_ld-lrate,   " UPH
      END OF it_master.

DATA: it_prod            LIKE TABLE OF ztpp_day_sum    WITH HEADER LINE,
      it_cond            LIKE TABLE OF ztpp_plan_key   WITH HEADER LINE,
      it_sum_prod        LIKE TABLE OF it_sum          WITH HEADER LINE,
      it_disp_prod       LIKE TABLE OF ztpp_alc_binput WITH HEADER LINE,
      it_disp            LIKE TABLE OF ztpp_alc_binput WITH HEADER LINE.
*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_disp             LIKE it_disp                             ,
      wa_data             LIKE it_data                             ,
      wa_wdate            LIKE sy-datum                            ,
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
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_dates      TYPE d        OBLIGATORY,
            p_mitu       TYPE c,
            p_fwo        TYPE c        .
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*BEGIN OF CHANGE BY CHRIS ON 09/29/2004 --UD1K912346
*----------------------------------------------------------------------
*INITIALIZATION.    " DELETE THIS EVENT
*----------------------------------------------------------------------
*----------------------------------------------------------------------
AT SELECTION-SCREEN.   "ADD THIS EVENT
*----------------------------------------------------------------------
*END OF CHANGE BY CHRIS ON 09/29/2004   --UD1K912346
*----------------------------------------------------------------------
  " Get the Date for the Production Reporting Date(Last Date)
  wa_wdate = p_dates - 1.
  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.
  PERFORM get_day          USING wa_wdate it_master-day .
  PERFORM get_working_time USING wa_wdate it_master-time it_master-day .
  PERFORM get_uph          USING wa_wdate it_master-uph it_master-shift.
  it_master-seq    = 99.
  it_master-date = wa_wdate .
  APPEND it_master.  CLEAR: it_master.


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM clear_variable               .
  PERFORM set_information              .
  PERFORM read_inputplan               .
  PERFORM create_summary               .
  PERFORM insert_field_vals            .
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
  IF p_mitu = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan
    WHERE status <= '00'  AND
          NOT ( mitu EQ 'X' OR mitu EQ 'Y' ).
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan
    WHERE status <= '00'   .
  ENDIF.

  DESCRIBE TABLE it_data LINES  wa_hour .
  IF wa_hour = 0.
    DELETE FROM ztpp_alc_binput CLIENT SPECIFIED WHERE mandt = sy-mandt.
    LEAVE PROGRAM .
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_prod
    FROM ztpp_day_sum
   WHERE wdate = wa_wdate AND
         rp01q NE 0.
**

  SORT it_data BY rsnum serial .

  PERFORM check_holding_car.

  DATA: l_fwocl LIKE ztpp_fwo-worder,
        l_worder LIKE ztpp_fwo-o_worder,
        l_ordr LIKE ztpp_pmt07jb_a-ordr,
        l_dist LIKE ztpp_pmt07jb_a-dist,
        l_fsc LIKE ztpp_wosum-fsc,
        l_year LIKE ztpp_pmt07jb_a-moye.

  IF NOT p_fwo IS INITIAL.
    LOOP AT it_data.
      IF it_data-work_order+0(1) = 'F'.
        CONCATENATE it_data-work_order it_data-extc it_data-intc
               INTO l_fwocl.
        SELECT SINGLE o_worder INTO l_worder
          FROM ztpp_fwo
          WHERE worder = l_fwocl.
        IF sy-subrc = 0.
          it_data-work_order = l_worder.
          MODIFY it_data.
        ELSE.
        l_ordr = it_data-work_order+0(9).
        l_dist = it_data-work_order+9(5).

        SELECT SINGLE moye INTO l_year
          FROM ztpp_pmt07jb_a
         WHERE ordr = l_ordr
           AND dist = l_dist
           AND extc = it_data-extc
           AND intc = it_data-intc.

        CONCATENATE l_year l_dist it_data-mi INTO l_fsc.
        CONCATENATE l_fsc it_data-ocnn INTO l_fsc
                SEPARATED BY space.
        SELECT SINGLE wo_ser INTO l_worder
          FROM ztpp_wosum
         WHERE extc = it_data-extc
           AND intc = it_data-intc
           AND version = it_data-vers
           AND fsc = l_fsc.
        IF sy-subrc = 0.
          CONCATENATE l_worder l_dist INTO l_worder.
          it_data-work_order = l_worder.
          MODIFY it_data.
*        ELSE.
*          CLEAR: it_data-work_order.
        ENDIF.
        ENDIF.
        CLEAR: l_fwocl, l_worder.
      ENDIF.
    ENDLOOP.
  ENDIF.

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
        lt_disp              LIKE TABLE OF it_disp     WITH HEADER LINE,
        lt_disp_prod         LIKE TABLE OF it_disp     WITH HEADER LINE,
        lt_sum_prod          LIKE TABLE OF it_prod     WITH HEADER LINE,
        lt_sum               LIKE TABLE OF it_sum      WITH HEADER LINE.

  PERFORM get_condition        .
  PERFORM read_internal_table  .         " Separate the Time-Horizon.
  PERFORM sum_plan             .         " Summarize the Time-Horizon
*---start1 wskim
*get data ZTPP_PMT07JB_A gubun 'B'
  PERFORM get_data_ztpp_pmt07jb.
*---end
  lt_sum[]      = it_sum[]     .
  lt_sum_prod[] = it_sum_prod[].

  LOOP AT it_cond.
    PERFORM get_cond_parsing  .
    it_sum[] = lt_sum[]       .
    it_sum_prod[] = lt_sum_prod[].
    LOOP AT it_sum.
      CLEAR: l_error.
      CONCATENATE it_sum-worder it_sum-extc it_sum-intc INTO l_worder.
*---start#1 wskim 03/08/2005 by hur
      PERFORM check_condition USING it_sum l_worder l_error.
*---end
      it_sum-mitu   = l_error .
      it_sum-rp     = it_cond-serial.
      it_sum-cond   = it_cond-key_code.
      MODIFY it_sum.
    ENDLOOP.

    " Summarize the IT_SUM-CNT.
    PERFORM calc_alc.

    LOOP AT it_sum_prod.
      CLEAR: l_error.
      CONCATENATE it_sum_prod-worder it_sum_prod-extc it_sum_prod-intc
              INTO l_worder.
*---Start
      PERFORM check_condition USING it_sum l_worder l_error.
*---End
      it_sum_prod-mitu   = l_error .
      it_sum_prod-rp     = it_cond-serial.
      it_sum_prod-cond   = it_cond-key_code.
      MODIFY it_sum_prod.
    ENDLOOP .

    " Summarize the IT_SUM_PROD-CNT..
    PERFORM calc_alc_prod .
  ENDLOOP.

  " Summary of the Internal Table - IT_DISP / IT_DISP_PROD
  SORT it_disp       BY serial  .
  SORT it_disp_prod  BY serial  .
  READ TABLE it_disp      INDEX 1.
  lt_disp-serial          = it_disp-serial     .
  lt_disp-key_code        = it_disp-key_code   .
  READ TABLE it_disp_prod INDEX 1.
  lt_disp_prod-serial     = it_disp_prod-serial.
  lt_disp_prod-key_code   = it_disp_prod-key_code   .

  LOOP AT it_disp .
    IF it_disp-serial = lt_disp-serial.
      lt_disp-d_1 = lt_disp-d_1 + it_disp-d_1 .
      lt_disp-d01 = lt_disp-d01 + it_disp-d01 .
      lt_disp-d02 = lt_disp-d02 + it_disp-d02 .
      lt_disp-d03 = lt_disp-d03 + it_disp-d03 .
      lt_disp-d04 = lt_disp-d04 + it_disp-d04 .
      lt_disp-d05 = lt_disp-d05 + it_disp-d05 .
      lt_disp-d06 = lt_disp-d06 + it_disp-d06 .
      lt_disp-d07 = lt_disp-d07 + it_disp-d07 .
      lt_disp-d08 = lt_disp-d08 + it_disp-d08 .
      lt_disp-d09 = lt_disp-d09 + it_disp-d09 .
      lt_disp-d10 = lt_disp-d10 + it_disp-d10 .
      lt_disp-d11 = lt_disp-d11 + it_disp-d11 .
      lt_disp-d12 = lt_disp-d12 + it_disp-d12 .
      lt_disp-d13 = lt_disp-d13 + it_disp-d13 .
      lt_disp-d14 = lt_disp-d14 + it_disp-d14 .
      lt_disp-d15 = lt_disp-d15 + it_disp-d15 .
      lt_disp-d16 = lt_disp-d16 + it_disp-d16 .
      lt_disp-d17 = lt_disp-d17 + it_disp-d17 .
      lt_disp-d18 = lt_disp-d18 + it_disp-d18 .
      lt_disp-d19 = lt_disp-d19 + it_disp-d19 .
      lt_disp-d20 = lt_disp-d20 + it_disp-d20 .
      lt_disp-d21 = lt_disp-d21 + it_disp-d21 .
      lt_disp-d22 = lt_disp-d22 + it_disp-d22 .
      lt_disp-d23 = lt_disp-d23 + it_disp-d23 .
      lt_disp-rem = lt_disp-rem + it_disp-rem .
      lt_disp-tot = lt_disp-tot + it_disp-tot .
      lt_disp-w01 = lt_disp-w01 + it_disp-w01 .
      lt_disp-w02 = lt_disp-w02 + it_disp-w02 .
      lt_disp-w03 = lt_disp-w03 + it_disp-w03 .
      lt_disp-w04 = lt_disp-w04 + it_disp-w04 .
      lt_disp-w05 = lt_disp-w05 + it_disp-w05 .
      lt_disp-w06 = lt_disp-w06 + it_disp-w06 .
      lt_disp-w07 = lt_disp-w07 + it_disp-w07 .
      lt_disp-w08 = lt_disp-w08 + it_disp-w08 .
      lt_disp-w09 = lt_disp-w09 + it_disp-w09 .
      lt_disp-w10 = lt_disp-w10 + it_disp-w10 .
      lt_disp-w11 = lt_disp-w11 + it_disp-w11 .
      lt_disp-w12 = lt_disp-w12 + it_disp-w12 .
      lt_disp-w13 = lt_disp-w13 + it_disp-w13 .
      lt_disp-w14 = lt_disp-w14 + it_disp-w14 .
      lt_disp-w15 = lt_disp-w15 + it_disp-w15 .
      lt_disp-w16 = lt_disp-w16 + it_disp-w16 .
      lt_disp-w17 = lt_disp-w17 + it_disp-w17 .
      lt_disp-w18 = lt_disp-w18 + it_disp-w18 .
      lt_disp-w19 = lt_disp-w19 + it_disp-w19 .
      lt_disp-w20 = lt_disp-w20 + it_disp-w20 .
      lt_disp-w21 = lt_disp-w21 + it_disp-w21 .
      lt_disp-w22 = lt_disp-w22 + it_disp-w22 .
      lt_disp-w23 = lt_disp-w23 + it_disp-w23 .
      CONTINUE               .
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

  LOOP AT it_disp_prod .
    IF it_disp_prod-serial = lt_disp_prod-serial.
      lt_disp_prod-d_1 = lt_disp_prod-d_1 + it_disp_prod-d_1 .
      lt_disp_prod-d01 = lt_disp_prod-d01 + it_disp_prod-d01 .
      lt_disp_prod-d02 = lt_disp_prod-d02 + it_disp_prod-d02 .
      lt_disp_prod-d03 = lt_disp_prod-d03 + it_disp_prod-d03 .
      lt_disp_prod-d04 = lt_disp_prod-d04 + it_disp_prod-d04 .
      lt_disp_prod-d05 = lt_disp_prod-d05 + it_disp_prod-d05 .
      lt_disp_prod-d06 = lt_disp_prod-d06 + it_disp_prod-d06 .
      lt_disp_prod-d07 = lt_disp_prod-d07 + it_disp_prod-d07 .
      lt_disp_prod-d08 = lt_disp_prod-d08 + it_disp_prod-d08 .
      lt_disp_prod-d09 = lt_disp_prod-d09 + it_disp_prod-d09 .
      lt_disp_prod-d10 = lt_disp_prod-d10 + it_disp_prod-d10 .
      lt_disp_prod-d11 = lt_disp_prod-d11 + it_disp_prod-d11 .
      lt_disp_prod-d12 = lt_disp_prod-d12 + it_disp_prod-d12 .
      lt_disp_prod-d13 = lt_disp_prod-d13 + it_disp_prod-d13 .
      lt_disp_prod-d14 = lt_disp_prod-d14 + it_disp_prod-d14 .
      lt_disp_prod-d15 = lt_disp_prod-d15 + it_disp_prod-d15 .
      lt_disp_prod-d16 = lt_disp_prod-d16 + it_disp_prod-d16 .
      lt_disp_prod-d17 = lt_disp_prod-d17 + it_disp_prod-d17 .
      lt_disp_prod-d18 = lt_disp_prod-d18 + it_disp_prod-d18 .
      lt_disp_prod-d19 = lt_disp_prod-d19 + it_disp_prod-d19 .
      lt_disp_prod-d20 = lt_disp_prod-d20 + it_disp_prod-d20 .
      lt_disp_prod-d21 = lt_disp_prod-d21 + it_disp_prod-d21 .
      lt_disp_prod-d22 = lt_disp_prod-d22 + it_disp_prod-d22 .
      lt_disp_prod-d23 = lt_disp_prod-d23 + it_disp_prod-d23 .
      lt_disp_prod-rem = lt_disp_prod-rem + it_disp_prod-rem .
      lt_disp_prod-tot = lt_disp_prod-tot + it_disp_prod-tot .
      CONTINUE               .
    ELSE.
      APPEND lt_disp_prod.
      lt_disp_prod = it_disp_prod .
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE it_disp_prod  LINES l_index.
  IF l_index > 0.
    APPEND lt_disp_prod.
  ENDIF.
  it_disp_prod[] = lt_disp_prod[].
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_it_COND_RP  text
*----------------------------------------------------------------------*
FORM read_internal_table .
  DATA: l_worder             LIKE mara-matnr,
        l_flag               TYPE c ,
        l_max                TYPE i ,
        l_hours              TYPE i ,
        l_pos                TYPE i ,
        l_loop               TYPE i ,
        l_skip               TYPE c ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_index              LIKE sy-tabix.

  DESCRIBE TABLE it_data LINES l_max.
  l_loop = l_index = 1.

  " First Days Data...
  SORT it_master BY seq .
  LOOP AT it_master WHERE seq > 80 AND seq < 90 .
    l_chk = it_master-time / 3600         .
    l_pos = l_pos +   it_master-uph * l_chk   .
    IF l_pos >= l_max.
      wa_index = l_max - l_index.
      wa_hour  = l_hours.
      l_pos = l_max.
      l_flag = 'X' .
    ENDIF.
    l_hours = l_hours + 1 .
    LOOP AT it_data FROM l_index TO l_pos.
      CLEAR: it_sum.
      it_sum-hours      = l_hours            .
      it_sum-worder     = it_data-work_order .
      it_sum-mitu       = it_data-mitu       .
      it_sum-extc       = it_data-extc       .
      it_sum-intc       = it_data-intc       .
      it_sum-status     = it_data-status     .
      it_sum-cnt        = 1                  .
      APPEND it_sum.
    ENDLOOP.
    l_index = l_pos + 1 .
  ENDLOOP.

  l_hours = 2 .

  " Daily Data...
  DO 21 TIMES.
    l_tabix = l_tabix + 1 .
    READ TABLE it_master WITH KEY seq = l_tabix.
    IF it_master-uph = 0.
      l_hours = l_hours + 1 .
*     l_pos   = l_pos + 1  .
    ELSE.
      l_chk = it_master-time / 3600 .
      IF l_flag = 'X'.
        EXIT.
      ENDIF.
      l_pos = l_pos +  it_master-uph * l_chk   .
      IF l_pos >= l_max.
        wa_index = l_max - l_index.
        wa_hour  = l_hours.
        l_pos = l_max.
        l_flag = 'X' .
      ENDIF.
      l_hours = l_hours + 1 .
      LOOP AT it_data FROM l_index TO l_pos.
        CLEAR: it_sum.
        it_sum-hours      = l_hours            .
        it_sum-worder     = it_data-work_order .
        it_sum-mitu       = it_data-mitu       .
        it_sum-extc       = it_data-extc       .
        it_sum-intc       = it_data-intc       .
        it_sum-status     = it_data-status     .
*     it_sum-rp         = it_cond-serial     .
*     it_sum-knnam      = it_cond-key_code   .
        it_sum-cnt        = 1                  .
*     CONCATENATE it_COND-type_alc it_COND-code INTO it_sum-code .
        APPEND it_sum.
      ENDLOOP.
    ENDIF.
    l_index = l_pos + 1 .
  ENDDO.

  " Make the it_sum_prod...(Previous Day's Production)
  LOOP AT it_prod     .
    CLEAR: it_sum_prod, l_worder.
    CONCATENATE it_prod-wo_ser it_prod-nation it_prod-dealer
           INTO l_worder .

    it_sum_prod-worder = l_worder           .
    it_sum_prod-extc   = it_prod-extc       .
    it_sum_prod-intc   = it_prod-intc       .
    it_sum_prod-cnt    = it_prod-rp01q      .
    APPEND it_sum_prod.
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
FORM calc_alc.
  DATA: l_name(40)              TYPE c  ,
        l_line                  TYPE i  ,
        l_cnt                   TYPE i  ,
        l_no(2)                 TYPE n  ,
        l_cond                  LIKE ztpp_plan_key-key_code,
        l_hours                 TYPE i  .

  " Summary of the data by Time-stamp...
  CLEAR: it_disp,l_cnt.

  SORT it_sum BY rp ASCENDING status DESCENDING  cond hours mitu
                    ASCENDING.
  READ TABLE it_sum WITH KEY status = '00'.
  l_no = l_hours  = it_sum-hours   .
  l_cond   = it_sum-cond    .

  CONCATENATE 'IT_DISP-D'  l_no     INTO l_name.
  ASSIGN (l_name)                   TO   <wa_dfield>.
  CLEAR: <wa_dfield>.

  LOOP AT it_sum WHERE status <> 'B'.
    IF l_hours = it_sum-hours .
      IF it_sum-mitu = space .
        l_cnt = l_cnt + it_sum-cnt.
        CONTINUE.
      ENDIF.
    ELSE.
      <wa_dfield> = <wa_dfield> + l_cnt .
      it_disp-serial   = it_cond-serial.
      it_disp-key_code = it_cond-key_code.
      APPEND it_disp.    CLEAR: it_disp, l_cnt.

      l_no = l_hours   = it_sum-hours  .
      CONCATENATE 'IT_DISP-D'  l_no     INTO l_name.
      ASSIGN (l_name)                   TO   <wa_dfield>.
      CLEAR: <wa_dfield>, l_cnt.
      IF it_sum-mitu = space .
*       <wa_dfield>  = <wa_dfield> + it_sum-cnt .
        l_cnt = it_sum-cnt .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum  LINES  l_line.
  IF l_line > 0.
    <wa_dfield> = <wa_dfield> + l_cnt .
    it_disp-serial   = it_cond-serial.
    it_disp-key_code = it_cond-key_code.
    APPEND it_disp.CLEAR: it_disp,l_cnt.
  ENDIF.

*7JB
  READ TABLE it_sum WITH KEY status = 'B'.
  l_no = l_hours  = it_sum-hours   .
  l_cond   = it_sum-cond    .
  CONCATENATE 'IT_DISP-W'  l_no     INTO l_name.
  ASSIGN (l_name)                   TO   <wa_dfield>.
  CLEAR: <wa_dfield>,l_cnt.

  LOOP AT it_sum WHERE status EQ 'B'.
    IF l_hours = it_sum-hours .
      IF it_sum-mitu = space .
        l_cnt = l_cnt + it_sum-cnt.
        CONTINUE.
      ENDIF.
    ELSE.
      <wa_dfield> = <wa_dfield> + l_cnt .
      it_disp-serial   = it_cond-serial.
      it_disp-key_code = it_cond-key_code.
      COLLECT it_disp. CLEAR: it_disp,l_cnt.

      l_no = l_hours   = it_sum-hours  .
      CONCATENATE 'IT_DISP-W'  l_no     INTO l_name.
      ASSIGN (l_name)                   TO   <wa_dfield>.
      CLEAR: <wa_dfield>, l_cnt.
      IF it_sum-mitu = space .
*       <wa_dfield>  = <wa_dfield> + it_sum-cnt .
        l_cnt = it_sum-cnt .
      ENDIF.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE it_sum  LINES  l_line.
  IF l_line > 0.
    <wa_dfield> = <wa_dfield> + l_cnt .
    it_disp-serial   = it_cond-serial.
    it_disp-key_code = it_cond-key_code.
    COLLECT it_disp. CLEAR: it_disp,l_cnt.
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
FORM calc_alc_prod.
  DATA: l_name(40)              TYPE c  ,
        l_line                  TYPE i  ,
        l_cnt                   TYPE i  ,
        l_cond                  LIKE ztpp_plan_key-key_code,
        l_hours                 TYPE i  .

  " Summary of the data by Time-stamp...
  SORT it_sum_prod BY hours rp   .
  READ TABLE it_sum_prod INDEX 1 .
  l_hours  = it_sum_prod-hours .
  l_cond   = it_sum_prod-cond    .

  LOOP AT it_sum_prod.
    IF l_hours = it_sum_prod-hours .
      IF it_sum_prod-mitu = space .
        l_cnt = l_cnt + it_sum_prod-cnt.
        CONTINUE.
      ENDIF.
    ELSE.
      it_disp_prod-d_1 = l_cnt .
      it_disp_prod-serial   = it_cond-serial.
      it_disp_prod-key_code = l_cond        .
      APPEND it_disp_prod.    CLEAR: it_disp_prod.
      l_hours   = it_sum_prod-hours  .
      IF it_sum_prod-mitu = space .
        it_disp_prod-d_1 = it_sum_prod-cnt .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum_prod  LINES  l_line.
  IF l_line > 0.
    it_disp_prod-d_1 = l_cnt .
    it_disp_prod-serial = it_cond-serial.
    it_disp_prod-key_code = l_cond      .
    APPEND it_disp_prod.
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
FORM display_data.
  DATA: l_cnt                LIKE it_disp-serial.
  DELETE FROM ztpp_alc_binput CLIENT SPECIFIED WHERE mandt = sy-mandt.

  SORT it_disp BY serial .
  MODIFY ztpp_alc_binput    FROM TABLE it_disp .
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000 WITH 'ALC_BINPUT Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000 WITH 'ALC_BINPUT Update failed'.
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
FORM sum_plan .
  DATA: lt_sum              LIKE TABLE OF it_sum       WITH HEADER LINE,
        lt_sum_temp         LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  lt_sum_temp[] = it_sum[] .
  CLEAR: lt_sum[], lt_sum  .
  DELETE it_sum WHERE hours > 16        .
  SORT it_sum BY hours worder extc intc .
  READ TABLE it_sum INDEX 1.
  l_hours = it_sum-hours   .
  l_wo    = it_sum-worder  .
  l_ext   = it_sum-extc    .
  l_int   = it_sum-intc    .
  lt_sum  = it_sum         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_sum  .
    IF l_hours = it_sum-hours AND l_wo  = it_sum-worder AND
       l_ext   = it_sum-extc  AND l_int = it_sum-intc   .
      IF it_sum-mitu = 'Y'      .
        l_mitucnt = l_mitucnt + 1    .
      ENDIF.
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      lt_sum-cnt     = l_cnt    .
      lt_sum-mitucnt = l_mitucnt.
      APPEND         lt_sum     .
      lt_sum  = it_sum         .
      l_cnt   = 1              .
      l_hours = it_sum-hours   .
      l_wo    = it_sum-worder  .
      l_ext   = it_sum-extc    .
      l_int   = it_sum-intc    .
      IF it_sum-mitu = 'Y'     .
        l_mitucnt = 1          .
      ELSE.
        CLEAR: l_mitucnt       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum LINES l_size .
  IF l_size > 0 .
    lt_sum-cnt = l_cnt       .
    lt_sum-mitucnt = l_mitucnt.
    APPEND       lt_sum      .
  ENDIF.

  it_sum[] = lt_sum_temp[].
  lt_sum_temp[] = lt_sum[].
  CLEAR: lt_sum[], lt_sum, l_cnt .
  DELETE it_sum WHERE hours <= 16        .
  SORT it_sum BY hours worder extc intc .
  READ TABLE it_sum INDEX 1.
  l_hours = it_sum-hours   .
  l_wo    = it_sum-worder  .
  l_ext   = it_sum-extc    .
  l_int   = it_sum-intc    .
  lt_sum  = it_sum         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_sum  .
    IF l_hours = it_sum-hours AND l_wo  = it_sum-worder AND
       l_ext   = it_sum-extc  AND l_int = it_sum-intc   .
      IF it_sum-mitu = 'Y'      .
        l_mitucnt = l_mitucnt + 1    .
      ENDIF.
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      lt_sum-cnt     = l_cnt    .
      lt_sum-mitucnt = l_mitucnt.
      APPEND         lt_sum     .
      lt_sum  = it_sum         .
      l_cnt   = 1              .
      l_hours = it_sum-hours   .
      l_wo    = it_sum-worder  .
      l_ext   = it_sum-extc    .
      l_int   = it_sum-intc    .
      IF it_sum-mitu = 'Y'     .
        l_mitucnt = 1          .
      ELSE.
        CLEAR: l_mitucnt       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum LINES l_size .
  IF l_size > 0 .
    lt_sum-cnt = l_cnt       .
    lt_sum-mitucnt = l_mitucnt.
    APPEND       lt_sum      .
  ENDIF.

  CLEAR: it_sum[], it_sum.
  it_sum[] = lt_sum[].
  APPEND LINES OF lt_sum_temp TO it_sum.

ENDFORM.                    " SUM_PLAN

*&---------------------------------------------------------------------*
*&      Form  INSERT_FIELD_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_field_vals.
  " Insert D-1 Field Value into Internal Table IT_DISP...
  LOOP AT it_disp.
    READ TABLE it_disp_prod WITH KEY serial  = it_disp-serial .
    IF sy-subrc = 0.
      it_disp-d_1 = it_disp_prod-d_1 .
      DELETE it_disp_prod WHERE serial  = it_disp-serial .
    ELSE.
      CLEAR: it_disp-d_1 .
    ENDIF.

    MODIFY it_disp.
  ENDLOOP.

  " Remain Fiedls Insert.....
  LOOP AT it_disp_prod .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_prod TO it_disp.
    it_disp-d_1 = it_disp_prod-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_prod, it_disp_prod[].
ENDFORM.                    " INSERT_FIELD_VALS

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
*&      Form  SET_INFORMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_information.
  DATA: l_date               TYPE d ,
        l_count              TYPE i .

  " Set the BASIC Information for the UPH & Work Time...
  CLEAR: l_count.                 l_date = p_dates  .
  PERFORM read_working_date USING '+'  wa_kalid  l_date .
  IF l_date = p_dates .
    it_master-seq    = '80'.       it_master-date   = l_date .
    PERFORM get_day          USING l_date it_master-day  .
    PERFORM get_worktime1    USING l_date it_master-time it_master-day .
*   PERFORM get_uph          USING l_date it_master-uph it_master-shift.
  ELSE.
    l_date = p_dates .
    PERFORM get_day         USING l_date it_master-day  .
    it_master-seq    = '81'.       it_master-date   = l_date .
    APPEND it_master.       CLEAR: it_master.
  ENDIF.

  " From D+1 Day To D+21 Day..  (Only Working Dates in FACTORY-Calendar)
  l_date = p_dates .
  DO 21 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    PERFORM get_day          USING l_date it_master-day  .
    PERFORM get_working_time USING l_date it_master-time it_master-day.
    PERFORM get_uph          USING l_date it_master-uph it_master-shift.
    it_master-seq    = l_count.
    it_master-date   = l_date .
    APPEND it_master.  CLEAR: it_master.
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
FORM get_working_time USING    pa_wdate  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = 'B'
     AND datub >= pa_wdate .

  SORT lt_capa BY datub .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    CLEAR: l_einzt.
    SELECT SINGLE einzt INTO l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    l_wtime = l_wtime + l_einzt    .
  ENDLOOP.
  pa_wktime = l_wtime .
ENDFORM.                    " GET_WORKING_TIME

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM get_worktime1 USING    pa_wdate  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_flag        TYPE c ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = 'B'
     AND datub >= pa_wdate .

  SORT lt_capa BY datub tagnr schnr .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    CLEAR: l_einzt.
    SELECT SINGLE einzt INTO l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    it_master-time  = l_einzt      .
    it_master-shift = lt_capa-schnr.
    it_master-seq   = 80 + lt_capa-schnr .
    PERFORM get_uph   USING pa_wdate it_master-uph it_master-shift.
    APPEND it_master.
    l_flag = 'X' .
  ENDLOOP.
  IF l_flag = space.
    PERFORM get_uph   USING pa_wdate it_master-uph it_master-shift.
    APPEND it_master.
  ENDIF.
  CLEAR: it_master.
  pa_wktime = l_wtime .
ENDFORM.                    " GET_WORKTIME1

*&---------------------------------------------------------------------*
*&      Form  GET_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM get_day USING    pa_wdate  pa_day.
  DATA: l_day         LIKE scal-indicator .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            date = pa_wdate
       IMPORTING
            day  = l_day.

  pa_day = l_day.
ENDFORM.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  GET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM get_uph USING    pa_wdate  pa_uph  pa_shift .
  DATA: w_uph  LIKE ztpp_status-uph.
  CALL FUNCTION 'Z_FPP_GET_UPH'
       EXPORTING
            date  = pa_wdate
            shift = pa_shift
            shop  = 'B'
       IMPORTING
            uph   = w_uph.
  pa_uph  = w_uph.

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
FORM get_data_ztpp_pmt07jb.
  DATA :it_7jb_a LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE.
  DATA :l_hours TYPE i,
        z_work_order LIKE ztpp_input_plan-work_order.

  DATA: l_fsc LIKE ztpp_wosum-fsc,
         l_worder LIKE ztpp_wosum-wo_ser.
  REFRESH it_7jb_a.

  SELECT * INTO TABLE it_7jb_a
   FROM ztpp_pmt07jb_a
    WHERE gubb EQ 'B'.

  SORT it_7jb_a BY sqdt  .                                  "UD1K917193

  LOOP AT it_7jb_a .
    CLEAR: it_sum.
    ON CHANGE OF it_7jb_a-sqdt.
      l_hours = l_hours + 1 .
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
      CONCATENATE it_7jb_a-ordr it_7jb_a-dist INTO z_work_order.
*    ENDIF.
    it_sum-hours      = l_hours.
    it_sum-worder     = z_work_order.
    it_sum-mitu       = ' '.
    it_sum-extc       = it_7jb_a-extc.
    it_sum-intc       = it_7jb_a-intc.
    it_sum-status     = 'B'.
*     it_sum-rp         = it_cond-serial     .
*     it_sum-knnam      = it_cond-key_code   .
    it_sum-cnt        =  it_7jb_a-pqty.
    APPEND it_sum.    CLEAR : it_sum,z_work_order.
  ENDLOOP.


ENDFORM.                    " GET_DATa_ztpp_pmt07jb
*&---------------------------------------------------------------------*
*&      Form  check_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ERROR  text
*----------------------------------------------------------------------*
FORM check_condition USING p_sum LIKE it_sum
                           p_value   p_error.
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
  DATA : f_oper LIKE it_pharse-oper.
  CLEAR f_oper.
  LOOP AT it_pharse.
* In case of 'Not'
    IF it_pharse-op = '!'.
      IF it_sum-status <> 'B'.   "
        SELECT SINGLE *  FROM ausp
         WHERE objek = p_value
           AND atinn = it_pharse-atinn
           AND klart = '001'
           AND atwrt = it_pharse-atwrt
           AND atflv = it_pharse-atflv .
        IF sy-subrc EQ 0.
          p_error  = 'X'.
          CONTINUE.
        ELSE.
          CLEAR p_error.
        ENDIF.
      ELSE.
        IF p_value(1) EQ 'E'.
          SELECT SINGLE *  FROM ausp
            WHERE objek = p_value
              AND klart = '001'.
          IF sy-subrc = 0.
            SELECT SINGLE *  FROM ausp
             WHERE objek = p_value
               AND atinn = it_pharse-atinn
               AND klart = '001'
               AND atwrt = it_pharse-atwrt
               AND atflv = it_pharse-atflv .
            IF sy-subrc EQ 0.
              p_error  = 'X'.
              CONTINUE.
            ELSE.
              CLEAR p_error.
            ENDIF.
          ELSE.
            p_error  = 'X'.
            CONTINUE.
          ENDIF.
        ELSE. " F
          SELECT SINGLE * FROM ztpp_fwo
            WHERE worder EQ p_value(18).

          SELECT SINGLE *  FROM ausp
            WHERE objek = ztpp_fwo-o_worder(18)
              AND atinn = it_pharse-atinn
              AND klart = '001'
              AND atwrt = it_pharse-atwrt
              AND atflv = it_pharse-atflv .
          IF sy-subrc EQ 0.
            p_error  = 'X'.
            CONTINUE.
          ELSE.
            CLEAR p_error.
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
      IF f_oper EQ 'X' AND p_error EQ ' '.
        READ TABLE it_pharse WITH KEY oper =  '@'.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDIF.
      IF p_error = 'X'.
        READ TABLE it_pharse WITH KEY oper =  '&'.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF it_sum-status <> 'B'.
        SELECT SINGLE *  FROM ausp
         WHERE objek = p_value
           AND atinn = it_pharse-atinn
           AND klart = '001'
           AND atwrt = it_pharse-atwrt
           AND atflv = it_pharse-atflv .
        IF sy-subrc NE 0.
          p_error  = 'X'.
        ELSE.
          CLEAR p_error.f_oper = 'X'.
*          EXIT.
        ENDIF.
      ELSE."WEEKLY
        IF p_value(1) EQ 'E'.
          SELECT SINGLE *  FROM ausp
            WHERE objek = p_value
             AND klart = '001'.
          IF sy-subrc = 0.
            SELECT SINGLE *  FROM ausp
             WHERE objek = p_value
               AND atinn = it_pharse-atinn
               AND klart = '001'
               AND atwrt = it_pharse-atwrt
               AND atflv = it_pharse-atflv .
            IF sy-subrc NE 0.
              p_error  = 'X'. CONTINUE.
            ELSE.
              CLEAR p_error.  f_oper = 'X'.
            ENDIF.
          ELSE.
            p_error  = 'X'.
            CONTINUE.
          ENDIF.
        ELSE. "F
          SELECT SINGLE * FROM ztpp_fwo
            WHERE worder EQ p_value(18).
          IF sy-subrc = 0.
            SELECT SINGLE *  FROM ausp
              WHERE objek = ztpp_fwo-o_worder(18)
                AND atinn = it_pharse-atinn
                AND klart = '001'
                AND atwrt = it_pharse-atwrt
                AND atflv = it_pharse-atflv .

            IF sy-subrc <> 0.
              p_error  = 'X'.
              CONTINUE.
            ELSE.
              CLEAR p_error.f_oper = 'X'.
            ENDIF.
          ELSE.
            p_error  = 'X'.
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
FORM check_holding_car.
  DATA : it_hold LIKE ztpp_hold_car OCCURS 0 WITH HEADER LINE.

  REFRESH it_hold.

  SELECT * INTO TABLE it_hold FROM ztpp_hold_car
         WHERE  ( status EQ 'W' ) OR ( status EQ space ).

  LOOP AT it_hold WHERE res_date > p_dates.
    READ TABLE it_data WITH KEY modl = it_hold-modl
                            body_ser = it_hold-body_ser.
    IF sy-subrc = 0.
*          DELETE TABLE it_data FROM it_data.
      it_hold-status = 'W'.
      MODIFY it_hold FROM it_hold.
    ELSE.
      it_hold-status = 'P'.
      MODIFY it_hold FROM it_hold.
    ENDIF.
  ENDLOOP.

  LOOP AT it_hold WHERE res_date <= p_dates.
    IF it_hold-res_date =  p_dates.
      it_hold-status = 'D'.
    ELSE.
      READ TABLE it_data WITH KEY modl = it_hold-modl
                           body_ser = it_hold-body_ser.
      IF sy-subrc = 0.
        DELETE TABLE it_data FROM it_data.
      ENDIF.
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
