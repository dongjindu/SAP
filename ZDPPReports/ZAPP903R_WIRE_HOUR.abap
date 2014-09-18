************************************************************************
* Program Name      : ZAPP903R_WIRE_HOUR
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : Wire Parts Summary(Bucket: 2 Hourly, Horz.: 3 Day)
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
* 03/03/2005 chris      UD1K914781     start date => system date
************************************************************************
REPORT  zapp903r_wire_hour    MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztpp_common_vals,
        ztpp_input_plan,
        ausp .

*----------------------------------------------------------------------
* Gloval Variables Definition
*----------------------------------------------------------------------
DATA: wa_uph_b                TYPE zvpp_ld-lrate,
      wa_uph_p                TYPE zvpp_ld-lrate,
      wa_uph_t                TYPE zvpp_ld-lrate.
DATA: c_factor(13)            TYPE p DECIMALS 3 VALUE 1.
*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_data        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_input_plan.
DATA: END OF it_data.

DATA: BEGIN OF is_prod        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_day_sum   .
DATA:   cnt                   TYPE i         ,
        model(4)              TYPE c         ,
      END OF is_prod.

DATA: BEGIN OF it_alc         OCCURS 0.
        INCLUDE STRUCTURE     cuvtab_valc.
DATA:   model(3)              TYPE c  ,
        code(3)               TYPE c  ,
        rp(2)                 TYPE n  ,
        type_alc              TYPE c  ,
*       char_alc              LIKE cabn-atnam,
      END OF it_alc .

DATA: BEGIN OF it_model       OCCURS 0,
        modl                  TYPE zpp_model,
      END OF it_model.

DATA: BEGIN OF it_sum         OCCURS 0,
        rp(2)                 TYPE n  ,
        alc(9)                TYPE c  ,              " For Summary Field
        worder                LIKE mara-matnr,
        knnam                 LIKE cukb-knnam,
        status                LIKE ztpp_input_plan-status,
        alc_vals1             LIKE ztpp_wire_day-alc_vals1,
        alc_vals2             LIKE ztpp_wire_day-alc_vals2,
        alc_vals3             LIKE ztpp_wire_day-alc_vals3,
        alc_vals4             LIKE ztpp_wire_day-alc_vals4,
        alc_vals5             LIKE ztpp_wire_day-alc_vals5,
        hours                 TYPE p DECIMALS 0         ,
        vm_model              LIKE ztpp_input_plan-modl ,
        vm_bodyser            LIKE ztpp_input_plan-body_ser,
        extc                  LIKE ztpp_input_plan-extc,
        intc                  LIKE ztpp_input_plan-intc,
        mitu                  TYPE zmitu,
        mitucnt               TYPE i  ,
        cnt                   TYPE i  ,
        serial                LIKE ztpp_input_plan-serial,
      END OF it_sum .

DATA: BEGIN OF it_master  OCCURS 0,
        seq               TYPE i  ,             " Sequence
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working
        uph               TYPE zvpp_ld-lrate,   " UPH
        tqty              TYPE i  ,             " Day's Total Veh.
      END OF it_master.

DATA: BEGIN OF it_shift   OCCURS 0,
        ser               TYPE i  ,             " Serial
        seq               TYPE i  ,             " Sequence
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working(NET)
        total             TYPE kapendzt  ,      " Times for working(GRS)
        ftime             TYPE kapendzt  ,      " Start Time.
        uph               TYPE zvpp_ld-lrate,   " UPH
        tqty              TYPE i  ,             " Shift's Total Veh.
        hqty              TYPE i  ,             " Hour's Total Veh.
        hloop             TYPE i  ,             " Hour's LOOP.
      END OF it_shift .

DATA: it_prod             LIKE TABLE OF is_prod        WITH HEADER LINE,
      it_seq              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_d1               LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_bi               LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_wbs              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_pi               LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_prj              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_pbs              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_disp_d1          LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE,
      it_disp_seq         LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE,
      it_disp_bi          LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE,
      it_disp_pi          LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE,
      it_disp_prj         LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE,
      it_disp_wbs         LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE,
      it_disp_pbs         LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE,
      it_disp             LIKE TABLE OF ztpp_wire_hour WITH HEADER LINE.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_disp             LIKE it_disp                             ,
      wa_data             LIKE it_data                             ,
      wa_wdate            LIKE ztpp_day_sum-wdate                  ,
      wa_kalid            LIKE kako-kalid                          ,
      wa_repid            LIKE sy-repid                            ,
      wa_uzeit            LIKE sy-uzeit                            ,
      wa_index            LIKE sy-tabix                            ,
      wa_model(3)         TYPE c                                   ,
      wa_error            TYPE c                                   ,
      wa_flag             TYPE c                                   ,
      wa_count            TYPE n                                   ,
      wa_hour             TYPE i                                   .


*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <wa_vals>      TYPE any,
               <wa_dfield>    TYPE any.


*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_name(30)              VALUE 'WIRE'               ,
      c_jobs(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              VALUE 'SEQ_SUM01' .


*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_dates       TYPE d        OBLIGATORY,
            p_test        TYPE c                  ,
            p_mitu        TYPE c                  ,
            p_wbs(2)      TYPE n        OBLIGATORY,
            p_prj(2)      TYPE n        OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  DATA: l_vals                LIKE ztpp_common_vals-item4.

  " Get the Date for the Production Reporting Date(Last Date)
* requested by MY Mur changed by chris
*  PERFORM get_start_day     USING wa_wdate     .
*  p_dates = wa_wdate     .
  p_dates  = wa_wdate = sy-datum.
* end of change on 03/03/2005
  wa_wdate = wa_wdate - 1.  wa_repid = sy-repid.
  PERFORM read_shop_calid   USING wa_kalid.
  PERFORM read_working_date USING '-'  wa_kalid  wa_wdate.

  IF p_wbs IS INITIAL.
    SELECT SINGLE item4  INTO l_vals
      FROM ztpp_common_vals
     WHERE jobs = wa_repid
       AND key2 = 'WBS'   .
    p_wbs = l_vals        .   CLEAR: l_vals.
  ENDIF.

  IF p_prj IS INITIAL.
    SELECT SINGLE item4  INTO l_vals
      FROM ztpp_common_vals
     WHERE jobs = wa_repid
       AND key2 = 'PRJ'   .
    p_prj = l_vals        .   CLEAR: l_vals.
  ENDIF.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM clear_variable               .
  PERFORM set_information              .
  PERFORM read_inputplan               .
  PERFORM read_alc_model               .
  PERFORM create_summary               .
  PERFORM insert_field_vals            .
* PERFORM summary_disp_final           .
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
  CLEAR: it_data,   it_alc,   it_sum,   it_disp,
         it_data[], it_alc[], it_sum[], it_disp[],
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

  IF p_mitu = 'X'.                                          "UD1K912950
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data     "UD1K912950
    FROM ztpp_input_plan                                    "UD1K912950
    WHERE  mitu NE 'Y' .                                    "UD1K912950
  ELSE.                                                     "UD1K912950
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan .
  ENDIF.

  " Elemenate the TEST-CAR that Dealer code is 'XX', 'XY' .
  IF p_test = 'X'.
    LOOP AT it_data.
      IF it_data-work_order+12(2) = 'XX' OR
         it_data-work_order+12(2) = 'XY' .
        DELETE it_data.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE it_data LINES  wa_hour .
  IF wa_hour = 0.
    DELETE FROM ztpp_wire_hour  CLIENT SPECIFIED WHERE mandt = sy-mandt.
    LEAVE PROGRAM .
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_prod
    FROM ztpp_day_sum
   WHERE wdate = wa_wdate AND
         dealer NE 'XX'   AND
         dealer NE 'XY'   AND
         rp06q  NE 0.

  SORT it_data BY rsnum serial .
*---start#1 wskim : check holding car
  PERFORM check_holding_car.
*---end

ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  READ_ALC_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MODEL  text
*----------------------------------------------------------------------*
FORM read_alc_model  .
  DATA: l_vtint              LIKE cuvtab-vtint,
        l_vtnam              LIKE cuvtab-vtnam.

  CLEAR: it_model, it_model[].
  PERFORM get_models .

  LOOP AT it_model   .
    wa_model = it_model-modl .
    CONCATENATE wa_model '_MIX'            INTO  l_vtnam    .
    PERFORM get_cuvtab                     USING l_vtnam  l_vtint.
    PERFORM get_alc                        USING l_vtint.
    " Set the Model Code...
    LOOP AT it_alc  WHERE model = space.
      it_alc-model = wa_model .
      it_alc-type_alc = it_alc-valc+6(1).
      it_alc-rp       = '06'            .
      MODIFY it_alc.
    ENDLOOP.
  ENDLOOP.

** On 08/05/13 by Furong
*  SORT it_alc BY model valc .
  SORT it_alc BY model atinn.
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
FORM create_summary.
  DATA: lt_alc               LIKE TABLE OF it_alc      WITH HEADER LINE,
        lt_wbs               LIKE TABLE OF it_wbs      WITH HEADER LINE,
        lt_prj               LIKE TABLE OF it_prj      WITH HEADER LINE,
        lt_seq               LIKE TABLE OF it_seq      WITH HEADER LINE,
        lt_bi                LIKE TABLE OF it_bi       WITH HEADER LINE,
        lt_pi                LIKE TABLE OF it_pi       WITH HEADER LINE,
        lt_d1                LIKE TABLE OF it_d1       WITH HEADER LINE,
        lt_data              LIKE TABLE OF it_data     WITH HEADER LINE.

  PERFORM inline_status .
  lt_data[] = it_data[] .  lt_wbs[] = it_wbs[].  lt_bi[] = it_bi[].
  lt_d1[]   = it_d1[]   .  lt_prj[] = it_prj[].  lt_pi[] = it_pi[].
  lt_seq[]  = it_seq[]  .  lt_alc[] = it_alc[].

  CLEAR: wa_index, wa_hour, it_sum, it_sum[].
  PERFORM read_internal_table .
  PERFORM read_alc            .
  PERFORM calc_alc            .

  CLEAR: wa_index, wa_hour, it_sum, it_sum[].
  PERFORM calc_alc_prod       .
  PERFORM calc_alc_seq        .
  PERFORM calc_alc_bi         .
  PERFORM calc_alc_wbs        .
  PERFORM calc_alc_pi         .
  PERFORM calc_alc_prj        .
  PERFORM calc_alc_pbs        .

  PERFORM summary_disp          .
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_RP  text
*----------------------------------------------------------------------*
FORM read_internal_table .
  DATA: l_name(40)           TYPE c ,
        l_flag               TYPE c ,
        l_max                TYPE i ,
        l_cnt                TYPE i ,
        l_shift              LIKE kapa-schnr,
        l_hours              TYPE i ,
        l_pos                TYPE i ,
        l_no2(2)             TYPE n ,
        l_chk                TYPE p DECIMALS 3,
        l_tabix              LIKE sy-tabix,
        l_start              LIKE sy-tabix,
        l_end                LIKE sy-tabix,
        l_index              LIKE sy-tabix.
  DATA:   l_rq                 TYPE i,
          l_lpcnt              TYPE i.

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
  READ TABLE it_data WITH KEY rp06 = space .
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
  l_index = sy-tabix .
  IF l_index > 1.
    l_index = l_index - 1 .
    DELETE it_data FROM 1 TO l_index.
  ENDIF.
  l_index = 1.

  " 3 Days Data...
  CLEAR: l_pos.   SORT it_shift BY ser seq shift.
  LOOP AT it_master.
** Furong on 06/13/12
    l_hours = 24 * it_master-seq - 24 .
    l_shift = 1.
    LOOP AT it_shift WHERE seq = it_master-seq .
      IF it_shift-shift NE 1.
        IF it_shift-shift = 2.
          l_hours = 24 * it_master-seq - 16.
        ELSE.
          l_hours = 24 * it_master-seq - 8.
        ENDIF.
        l_shift = it_shift-shift .
      ENDIF.

*      l_hours = 20 * it_master-seq - 20 .
*      l_shift = 1.
*      LOOP AT it_shift WHERE seq = it_master-seq .
*        IF it_shift-shift NE l_shift.
*          l_hours = 20 * it_master-seq - 10 .
*          l_shift = it_shift-shift .
*        ENDIF.
** End 06/13/12

      CLEAR: l_cnt.
      l_rq    = it_shift-tqty.
      l_lpcnt = it_shift-hloop.

      DO it_shift-hloop TIMES.

        IF sy-index EQ it_shift-hloop.
          it_shift-hqty = l_rq.
        ENDIF.

        l_rq    = l_rq - it_shift-hqty.
        l_lpcnt = l_lpcnt - 1.

*        L_CNT = L_CNT + it_shift-Hqty            .
*        l_chk = it_master-tqty - l_CNT           .
*        IF l_CHK < 0                             .
*          it_shift-Hqty = it_shift-Hqty + l_chk  .
*        ENDIF.

        l_pos = it_shift-hqty + l_index - 1 .
        l_hours = l_hours + 2 .
        LOOP AT it_data FROM l_index TO l_pos.
          CLEAR: it_sum.
          it_sum-hours      = l_hours .
          it_sum-vm_model   = it_data-modl .
          it_sum-vm_bodyser = it_data-body_ser.
          it_sum-worder     = it_data-work_order .
          it_sum-mitu       = it_data-mitu       .
          it_sum-extc       = it_data-extc       .
          it_sum-intc       = it_data-intc       .
          it_sum-status     = it_data-status     .
          it_sum-rp         = it_alc-rp          .
          it_sum-cnt        = 1                  .
*         CONCATENATE it_alc-type_alc it_alc-code INTO it_sum-code .
          PERFORM update_input_plan  USING l_hours.
          APPEND it_sum.
        ENDLOOP.
        l_index = l_pos + 1 .
      ENDDO.
    ENDLOOP.
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
        l_mitucnt               TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,              " ALC CODE
        l_vals2(5)              TYPE c  ,              " ALC CODE
        l_vals3(5)              TYPE c  ,              " ALC CODE
        l_vals4(5)              TYPE c  ,              " ALC CODE
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp.
  DESCRIBE TABLE it_sum LINES l_line.
  CHECK l_line > 0 .
  SORT it_sum BY hours      vm_model   alc_vals1   alc_vals2
                 alc_vals3  alc_vals4  alc_vals5          .
  READ TABLE it_sum INDEX 1.
  l_hours  = it_sum-hours   .
* l_code   = it_sum-code    .
  l_vals1  = it_sum-alc_vals1.
  l_vals2  = it_sum-alc_vals2.
  l_vals3  = it_sum-alc_vals3.
  l_vals4  = it_sum-alc_vals4.
  l_vals5  = it_sum-alc_vals5.
  l_model  = it_sum-vm_model.

  CONCATENATE 'IT_DISP-H'  l_hours  INTO l_name.
  ASSIGN (l_name)                   TO   <wa_dfield>.
  CLEAR: <wa_dfield>.

  LOOP AT it_sum.
    IF l_hours = it_sum-hours       AND l_vals1  = it_sum-alc_vals1 AND
        l_vals2  = it_sum-alc_vals2 AND l_vals3  = it_sum-alc_vals3 AND
        l_vals4  = it_sum-alc_vals4 AND l_vals5  = it_sum-alc_vals5 AND
        l_model  = it_sum-vm_model  .
      <wa_dfield> = <wa_dfield> + it_sum-cnt .
      l_mitucnt   = l_mitucnt   + it_sum-mitucnt .
      CONTINUE.
    ELSE.
      it_disp-model    = l_model  .
*     it_disp-alc_code = l_code   .
      it_disp-alc_vals1 = l_vals1  .
      it_disp-alc_vals2 = l_vals2  .
      it_disp-alc_vals3 = l_vals3  .
      it_disp-alc_vals4 = l_vals4  .
      it_disp-alc_vals5 = l_vals5  .
      it_disp-rp       = it_alc-rp.
      it_disp-mitu     = l_mitucnt.
      APPEND it_disp.    CLEAR: it_disp.
      l_hours    = it_sum-hours    .
*      l_code     = it_sum-code     .
      l_vals1    = it_sum-alc_vals1.
      l_vals2    = it_sum-alc_vals2.
      l_vals3    = it_sum-alc_vals3.
      l_vals4    = it_sum-alc_vals4.
      l_vals5    = it_sum-alc_vals5.
      l_model    = it_sum-vm_model .
      l_mitucnt   = it_sum-mitucnt .
      CONCATENATE 'IT_DISP-H'  l_hours  INTO l_name.
      ASSIGN (l_name)                   TO   <wa_dfield>.
      <wa_dfield>  = it_sum-cnt .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum  LINES  l_line.
  IF l_line > 0.
    it_disp-model    = l_model  .
*   it_disp-alc_code = l_code   .
    it_disp-alc_vals1 = l_vals1  .
    it_disp-alc_vals2 = l_vals2  .
    it_disp-alc_vals3 = l_vals3  .
    it_disp-alc_vals4 = l_vals4  .
    it_disp-alc_vals5 = l_vals5  .
    it_disp-rp       = it_alc-rp.
    APPEND it_disp.
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
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,              "
        l_vals2(5)              TYPE c  ,              "
        l_vals3(5)              TYPE c  ,              "
        l_vals4(5)              TYPE c  ,              "
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_d1  .
  SORT it_d1       BY vm_model   hours      alc_vals1  alc_vals2
                      alc_vals3  alc_vals4  alc_vals5.
  READ TABLE it_d1 INDEX 1.  CLEAR: l_count.
  l_hours  = it_d1-hours   .
  l_vals1  = it_d1-alc_vals1.
  l_vals2  = it_d1-alc_vals2.
  l_vals3  = it_d1-alc_vals3.
  l_vals4  = it_d1-alc_vals4.
  l_vals5  = it_d1-alc_vals5.
  l_model  = it_d1-vm_model.

  LOOP AT it_d1.
    IF l_vals1  = it_d1-alc_vals1 AND  l_model  = it_d1-vm_model  AND
       l_vals2  = it_d1-alc_vals2 AND  l_vals3  = it_d1-alc_vals3 AND
       l_vals4  = it_d1-alc_vals4 AND  l_vals5  = it_d1-alc_vals5 .
**      l_hours = it_sum_PROD-hours.
      l_count  = l_count + it_d1-cnt .
      CONTINUE.
    ELSE.
      it_disp_d1-model     = l_model  .
      it_disp_d1-alc_vals1 = l_vals1(5).
      it_disp_d1-alc_vals2 = l_vals2(5).
      it_disp_d1-alc_vals3 = l_vals3(5).
      it_disp_d1-alc_vals4 = l_vals4(5).
      it_disp_d1-alc_vals5 = l_vals5(5).
      it_disp_d1-rp       = it_alc-rp.
      it_disp_d1-d_1      = l_count  .
      APPEND it_disp_d1  .    CLEAR: it_disp_d1  .
      l_vals1  = it_d1-alc_vals1.
      l_vals2  = it_d1-alc_vals2.
      l_vals3  = it_d1-alc_vals3.
      l_vals4  = it_d1-alc_vals4.
      l_vals5  = it_d1-alc_vals5.
      l_model = it_d1-vm_model   .
      l_count = it_d1-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_d1  LINES  l_line.
  IF l_line > 0.
    it_disp_d1-model    = l_model  .
    it_disp_d1-alc_vals1 = l_vals1(5).
    it_disp_d1-alc_vals2 = l_vals2(5).
    it_disp_d1-alc_vals3 = l_vals3(5).
    it_disp_d1-alc_vals4 = l_vals4(5).
    it_disp_d1-alc_vals5 = l_vals5(5).
    it_disp_d1-rp        = it_alc-rp.
    it_disp_d1-d_1       = l_count  .
    APPEND it_disp_d1  .
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
FORM calc_alc_seq .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_mitucnt               TYPE i  ,
        l_model(3)              TYPE c  ,
        l_days(2)               TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,              " ALC CODE
        l_vals2(5)              TYPE c  ,              " ALC CODE
        l_vals3(5)              TYPE c  ,              " ALC CODE
        l_vals4(5)              TYPE c  ,              " ALC CODE
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_seq .
  SORT it_seq  BY vm_model      alc_vals1  alc_vals2
                  alc_vals3  alc_vals4  alc_vals5.
  READ TABLE it_seq  INDEX 1.  CLEAR: l_count.
* l_days   = it_sum_PROD-days    .
  l_vals1  = it_seq-alc_vals1.
  l_vals2  = it_seq-alc_vals2.
  l_vals3  = it_seq-alc_vals3.
  l_vals4  = it_seq-alc_vals4.
  l_vals5  = it_seq-alc_vals5.
  l_model  = it_seq-vm_model .

  LOOP AT it_seq .
    IF l_vals1  = it_seq-alc_vals1 AND  l_model  = it_seq-vm_model  AND
       l_vals2  = it_seq-alc_vals2 AND  l_vals3  = it_seq-alc_vals3 AND
       l_vals4  = it_seq-alc_vals4 AND  l_vals5  = it_seq-alc_vals5 .
      l_count     = l_count     + it_seq-cnt .
      l_mitucnt   = l_mitucnt   + it_seq-mitucnt .
      CONTINUE.
    ELSE.
      it_disp_seq-model    = l_model  .
      it_disp_seq-alc_vals1 = l_vals1(5).
      it_disp_seq-alc_vals2 = l_vals2(5).
      it_disp_seq-alc_vals3 = l_vals3(5).
      it_disp_seq-alc_vals4 = l_vals4(5).
      it_disp_seq-alc_vals5 = l_vals5(5).
      it_disp_seq-rp       = it_alc-rp.
      it_disp_seq-d_1      = l_count  .
      it_disp_seq-mitu      = l_mitucnt.
      APPEND it_disp_seq .    CLEAR: it_disp_seq .
*     l_hours   = it_SEQ-hours      .
*     l_code    = it_seq-code      .
      l_vals1   = it_seq-alc_vals1     .
      l_vals2   = it_seq-alc_vals2     .
      l_vals3   = it_seq-alc_vals3     .
      l_vals4   = it_seq-alc_vals4     .
      l_vals5   = it_seq-alc_vals5     .
      l_model   = it_seq-vm_model  .
      l_count   = it_seq-cnt        .
      l_mitucnt = it_seq-mitucnt .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_seq   LINES  l_line.
  IF l_line > 0.
    it_disp_seq-model     = l_model  .
    it_disp_seq-alc_vals1 = l_vals1(5).
    it_disp_seq-alc_vals2 = l_vals2(5).
    it_disp_seq-alc_vals3 = l_vals3(5).
    it_disp_seq-alc_vals4 = l_vals4(5).
    it_disp_seq-alc_vals5 = l_vals5(5).
*    it_disp_seq-alc_code = l_code   .
*    it_disp_seq-alc_vals = l_vals   .
    it_disp_seq-rp        = it_alc-rp.
    it_disp_seq-d_1       = l_count  .
    it_disp_seq-mitu      = l_mitucnt.
    APPEND it_disp_seq .
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
FORM calc_alc_bi  .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_mitucnt               TYPE i  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,
        l_vals2(5)              TYPE c  ,
        l_vals3(5)              TYPE c  ,
        l_vals4(5)              TYPE c  ,
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_bi .
  SORT it_bi   BY vm_model  alc_vals1  alc_vals2
                  alc_vals3 alc_vals4  alc_vals5.
  READ TABLE it_bi   INDEX 1.  CLEAR: l_count.
*  l_code   = it_bi-code    .
  l_vals1   = it_bi-alc_vals1   .
  l_vals2   = it_bi-alc_vals2   .
  l_vals3   = it_bi-alc_vals3   .
  l_vals4   = it_bi-alc_vals4   .
  l_vals5   = it_bi-alc_vals5   .
  l_model  = it_bi-vm_model.

  LOOP AT it_bi  .
    IF l_model  = it_bi-vm_model  AND l_vals1 = it_bi-alc_vals1  AND
       l_vals2 = it_bi-alc_vals2  AND l_vals3 = it_bi-alc_vals3  AND
       l_vals4 = it_bi-alc_vals4  AND l_vals5 = it_bi-alc_vals5  .
      l_count  = l_count + it_bi-cnt .
      CONTINUE.
    ELSE.
      it_disp_bi-model     = l_model  .
      it_disp_bi-alc_vals1 = l_vals1(5).
      it_disp_bi-alc_vals2 = l_vals2(5).
      it_disp_bi-alc_vals3 = l_vals3(5).
      it_disp_bi-alc_vals4 = l_vals4(5).
      it_disp_bi-alc_vals5 = l_vals5(5).
      it_disp_bi-rp        = it_alc-rp.
      it_disp_bi-d_1       = l_count  .
      APPEND it_disp_bi .    CLEAR: it_disp_bi .
*      l_code  = it_bi-code       .
*      l_vals  = it_bi-vals       .
      l_vals1 = it_bi-alc_vals1       .
      l_vals2 = it_bi-alc_vals2       .
      l_vals3 = it_bi-alc_vals3       .
      l_vals4 = it_bi-alc_vals4       .
      l_vals5 = it_bi-alc_vals5       .
      l_model = it_bi-vm_model   .
      l_count = it_bi-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_bi    LINES  l_line.
  IF l_line > 0.
    it_disp_bi-model     = l_model  .
    it_disp_bi-alc_vals1 = l_vals1(5).
    it_disp_bi-alc_vals2 = l_vals2(5).
    it_disp_bi-alc_vals3 = l_vals3(5).
    it_disp_bi-alc_vals4 = l_vals4(5).
    it_disp_bi-alc_vals5 = l_vals5(5).
*    it_disp_bi-alc_code = l_code   .
*    it_disp_bi-alc_vals = l_vals   .
    it_disp_bi-rp        = it_alc-rp.
    it_disp_bi-d_1       = l_count  .
    APPEND it_disp_bi  .
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
FORM calc_alc_wbs .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_mitucnt               TYPE i  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,              " ALC CODE
        l_vals2(5)              TYPE c  ,              " ALC CODE
        l_vals3(5)              TYPE c  ,              " ALC CODE
        l_vals4(5)              TYPE c  ,              " ALC CODE
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_wbs .
  SORT it_wbs  BY vm_model   alc_vals1   alc_vals2
                  alc_vals3  alc_vals4   alc_vals5.
  READ TABLE it_wbs  INDEX 1.  CLEAR: l_count.
* l_hours  = it_WBS-hours   .
  l_vals1  = it_wbs-alc_vals1  .
  l_vals2  = it_wbs-alc_vals2  .
  l_vals3  = it_wbs-alc_vals3  .
  l_vals4  = it_wbs-alc_vals4  .
  l_vals5  = it_wbs-alc_vals5  .
  l_model  = it_wbs-vm_model.

  LOOP AT it_wbs .
    IF l_vals1 = it_wbs-alc_vals1 AND l_vals2 = it_wbs-alc_vals2  AND
       l_vals3 = it_wbs-alc_vals3 AND l_vals4 = it_wbs-alc_vals4  AND
       l_vals5 = it_wbs-alc_vals5 AND l_model  = it_wbs-vm_model.
      l_count  = l_count + it_wbs-cnt .
      CONTINUE.
    ELSE.
      it_disp_wbs-model    = l_model  .
      it_disp_wbs-alc_vals1 = l_vals1(5).
      it_disp_wbs-alc_vals2 = l_vals2(5).
      it_disp_wbs-alc_vals3 = l_vals3(5).
      it_disp_wbs-alc_vals4 = l_vals4(5).
      it_disp_wbs-alc_vals5 = l_vals5(5).
      it_disp_wbs-rp       = it_alc-rp.
      it_disp_wbs-d_1      = l_count    .
*     MOVE-CORRESPONDING IT_WBS TO it_disp_wbs .
      APPEND it_disp_wbs .    CLEAR: it_disp_wbs .
*      l_code  = it_wbs-code       .
      l_vals1 = it_wbs-alc_vals1      .
      l_vals2 = it_wbs-alc_vals2      .
      l_vals3 = it_wbs-alc_vals3      .
      l_vals4 = it_wbs-alc_vals4      .
      l_vals5 = it_wbs-alc_vals5      .
      l_model = it_wbs-vm_model   .
      l_count = it_wbs-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_wbs   LINES  l_line.
  IF l_line > 0.
    it_disp_wbs-model    = l_model  .
    it_disp_wbs-alc_vals1 = l_vals1(5).
    it_disp_wbs-alc_vals2 = l_vals2(5).
    it_disp_wbs-alc_vals3 = l_vals3(5).
    it_disp_wbs-alc_vals4 = l_vals4(5).
    it_disp_wbs-alc_vals5 = l_vals5(5).
*   it_disp_wbs-alc_code = l_code   .
*   it_disp_wbs-alc_vals = l_vals   .
    it_disp_wbs-rp       = it_alc-rp.
    it_disp_wbs-d_1      = l_count  .
    APPEND it_disp_wbs .
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
FORM calc_alc_pi  .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,
        l_vals2(5)              TYPE c  ,
        l_vals3(5)              TYPE c  ,
        l_vals4(5)              TYPE c  ,
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_pi .
  SORT it_pi   BY vm_model   alc_vals1  alc_vals2
                  alc_vals3  alc_vals4  alc_vals5.
  READ TABLE it_pi   INDEX 1.  CLEAR: l_count.
*  l_code   = it_pi-code    .
  l_vals1  = it_pi-alc_vals1.
  l_vals2  = it_pi-alc_vals2.
  l_vals3  = it_pi-alc_vals3.
  l_vals4  = it_pi-alc_vals4.
  l_vals5  = it_pi-alc_vals5.
  l_model  = it_pi-vm_model.

  LOOP AT it_pi  .
    IF l_vals1 = it_pi-alc_vals1 AND  l_vals2  = it_pi-alc_vals2  AND
       l_vals3 = it_pi-alc_vals3 AND  l_vals4  = it_pi-alc_vals4  AND
       l_vals5 = it_pi-alc_vals5 AND  l_model  = it_pi-vm_model.
      l_count  = l_count + it_pi-cnt .
      CONTINUE.
    ELSE.
      it_disp_pi-model    = l_model  .
      it_disp_pi-alc_vals1 = l_vals1(5).
      it_disp_pi-alc_vals2 = l_vals2(5).
      it_disp_pi-alc_vals3 = l_vals3(5).
      it_disp_pi-alc_vals4 = l_vals4(5).
      it_disp_pi-alc_vals5 = l_vals5(5).
      it_disp_pi-rp        = it_alc-rp.
      it_disp_pi-d_1       = l_count  .
*      MOVE-CORRESPONDING IT_PI  TO it_disp_PI  .
      APPEND it_disp_pi .    CLEAR: it_disp_pi .
*     l_hours = it_PI-Hours      .
*      l_code  = it_pi-code       .
      l_vals1 = it_pi-alc_vals1       .
      l_vals2 = it_pi-alc_vals2       .
      l_vals3 = it_pi-alc_vals3       .
      l_vals4 = it_pi-alc_vals4       .
      l_vals5 = it_pi-alc_vals5       .
      l_model = it_pi-vm_model   .
      l_count = it_pi-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pi    LINES  l_line.
  IF l_line > 0.
    it_disp_pi-model    = l_model  .
    it_disp_pi-alc_vals1 = l_vals1(5).
    it_disp_pi-alc_vals2 = l_vals2(5).
    it_disp_pi-alc_vals3 = l_vals3(5).
    it_disp_pi-alc_vals4 = l_vals4(5).
    it_disp_pi-alc_vals5 = l_vals5(5).
*   it_disp_pi-alc_code = l_code   .
*   it_disp_pi-alc_vals = l_vals   .
    it_disp_pi-rp       = it_alc-rp.
    it_disp_pi-d_1      = l_count  .
    APPEND it_disp_pi  .
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
FORM calc_alc_prj .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,              "
        l_vals2(5)              TYPE c  ,              "
        l_vals3(5)              TYPE c  ,              "
        l_vals4(5)              TYPE c  ,              "
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_prj .
  SORT it_prj BY vm_model   alc_vals1  alc_vals2
                 alc_vals3  alc_vals4  alc_vals5.
  READ TABLE it_prj INDEX 1.  CLEAR: l_count.
  l_vals1  = it_prj-alc_vals1 .
  l_vals2  = it_prj-alc_vals2 .
  l_vals3  = it_prj-alc_vals3 .
  l_vals4  = it_prj-alc_vals4 .
  l_vals5  = it_prj-alc_vals5 .
  l_model  = it_prj-vm_model.

  LOOP AT it_prj .
    IF l_vals1 = it_prj-alc_vals1  AND l_vals2  = it_prj-alc_vals2  AND
       l_vals3 = it_prj-alc_vals3  AND l_vals4  = it_prj-alc_vals4  AND
       l_vals5 = it_prj-alc_vals5  AND l_model  = it_prj-vm_model.
      l_count  = l_count + it_prj-cnt .
      CONTINUE.
    ELSE.
      it_disp_prj-model    = l_model  .
      it_disp_prj-alc_vals1 = l_vals1(5).
      it_disp_prj-alc_vals2 = l_vals2(5).
      it_disp_prj-alc_vals3 = l_vals3(5).
      it_disp_prj-alc_vals4 = l_vals4(5).
      it_disp_prj-alc_vals5 = l_vals5(5).
      it_disp_prj-rp       = it_alc-rp.
      it_disp_prj-d_1      = l_count   .
*     MOVE-CORRESPONDING IT_PRJ TO it_disp_PRJ .
      APPEND it_disp_prj.    CLEAR: it_disp_prj .
*     l_hours = it_PRJ-hours      .
*      l_code  = it_prj-code       .
      l_vals1 = it_prj-alc_vals1      .
      l_vals2 = it_prj-alc_vals2      .
      l_vals3 = it_prj-alc_vals3      .
      l_vals4 = it_prj-alc_vals4      .
      l_vals5 = it_prj-alc_vals5      .
      l_model = it_prj-vm_model   .
      l_count = it_prj-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_prj   LINES  l_line.
  IF l_line > 0.
    it_disp_prj-model    = l_model  .
    it_disp_prj-alc_vals1 = l_vals1(5).
    it_disp_prj-alc_vals2 = l_vals2(5).
    it_disp_prj-alc_vals3 = l_vals3(5).
    it_disp_prj-alc_vals4 = l_vals4(5).
    it_disp_prj-alc_vals5 = l_vals5(5).
*   it_disp_prj-alc_code = l_code   .
*   it_disp_prj-alc_vals = l_vals   .
    it_disp_prj-rp       = it_alc-rp.
    it_disp_prj-d_1      = l_count  .
    APPEND it_disp_prj.
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
FORM calc_alc_pbs .
  DATA: l_count                 TYPE i  ,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_mitucnt               TYPE i  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,              "
        l_vals2(5)              TYPE c  ,              "
        l_vals3(5)              TYPE c  ,              "
        l_vals4(5)              TYPE c  ,              "
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  CLEAR: it_disp_pbs .
  SORT it_pbs  BY vm_model   alc_vals1  alc_vals2
                  alc_vals3  alc_vals4  alc_vals5.
  READ TABLE it_pbs  INDEX 1.  CLEAR: l_count.
* l_hours  = it_PBS-hours   .
  l_vals1 = it_pbs-alc_vals1.
  l_vals2 = it_pbs-alc_vals2.
  l_vals3 = it_pbs-alc_vals3.
  l_vals4 = it_pbs-alc_vals4.
  l_vals5 = it_pbs-alc_vals5.
  l_model = it_pbs-vm_model.

  LOOP AT it_pbs .
    IF l_vals1 = it_pbs-alc_vals1  AND  l_vals2 = it_pbs-alc_vals2  AND
       l_vals3 = it_pbs-alc_vals3  AND  l_vals4 = it_pbs-alc_vals4  AND
       l_vals5 = it_pbs-alc_vals5  AND  l_model = it_pbs-vm_model.
      l_count  = l_count + it_pbs-cnt .
      CONTINUE.
    ELSE.
      it_disp_pbs-model    = l_model  .
      it_disp_pbs-alc_vals1 = l_vals1(5).
      it_disp_pbs-alc_vals2 = l_vals2(5).
      it_disp_pbs-alc_vals3 = l_vals3(5).
      it_disp_pbs-alc_vals4 = l_vals4(5).
      it_disp_pbs-alc_vals5 = l_vals5(5).
      it_disp_pbs-rp       = it_alc-rp.
      it_disp_pbs-d_1      = l_count    .
      APPEND it_disp_pbs .    CLEAR: it_disp_pbs .
*      l_code  = it_pbs-code       .
      l_vals1 = it_pbs-alc_vals1       .
      l_vals2 = it_pbs-alc_vals2       .
      l_vals3 = it_pbs-alc_vals3       .
      l_vals4 = it_pbs-alc_vals4       .
      l_vals5 = it_pbs-alc_vals5       .
      l_model = it_pbs-vm_model   .
      l_count = it_pbs-cnt        .
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pbs   LINES  l_line.
  IF l_line > 0.
    it_disp_pbs-model    = l_model  .
    it_disp_pbs-alc_vals1 = l_vals1(5).
    it_disp_pbs-alc_vals2 = l_vals2(5).
    it_disp_pbs-alc_vals3 = l_vals3(5).
    it_disp_pbs-alc_vals4 = l_vals4(5).
    it_disp_pbs-alc_vals5 = l_vals5(5).
    it_disp_pbs-rp       = it_alc-rp.
    it_disp_pbs-d_1      = l_count  .
    APPEND it_disp_pbs .
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
FORM get_alc_value USING    pa_worder  pa_code
                   CHANGING pa_vals.
  DATA: l_vals            LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  CLEAR: l_vals, l_vals[].
  l_vals-atnam = pa_code .   APPEND l_vals.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = pa_worder
      ctype        = '001'
    TABLES
      val_table    = l_vals
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  READ TABLE l_vals INDEX 1  .
  pa_vals = l_vals-atwrt     .
ENDFORM.                    " GET_ALC_VALUE

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
* SUB TOTAL OF THE RESULT                                 "UD1K912950
  PERFORM sub_total .
  DELETE FROM ztpp_wire_hour CLIENT SPECIFIED WHERE mandt = sy-mandt.
  SORT it_disp BY serial model  alc_vals1 alc_vals2 alc_vals3
                                alc_vals4 alc_vals5 .
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
  MODIFY ztpp_wire_hour     FROM TABLE it_disp .
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000 WITH 'Update successful'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000 WITH 'Update failed'.
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
FORM inline_status.
  " Summary of the ALC for the Current MIP Vehicle..
  PERFORM create_data   TABLES  it_seq   USING '00'.
  PERFORM calc_seq                                 .
  PERFORM create_data   TABLES  it_bi    USING '01'.
  PERFORM calc_bodyinput                           .
  PERFORM create_data   TABLES  it_wbs   USING '99'.
  PERFORM calc_wbs                                 .
  PERFORM create_data   TABLES  it_pi    USING '02'.
  PERFORM calc_paintinput                          .
  PERFORM create_data   TABLES  it_prj   USING '88'.
  PERFORM calc_paintreject                         .
  PERFORM create_data   TABLES  it_pbs   USING '06'.
  PERFORM calc_pbs                                 .
  PERFORM calc_prod                                .
ENDFORM.                    " INLINE_STATUS

*&---------------------------------------------------------------------*
*&      Form  add_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
FORM add_routine USING    pa_disp  LIKE  it_disp.
  it_disp-h02  = it_disp-h02  + pa_disp-h02 .
  it_disp-h04  = it_disp-h04  + pa_disp-h04 .
  it_disp-h06  = it_disp-h06  + pa_disp-h06 .
  it_disp-h08  = it_disp-h08  + pa_disp-h08 .
  it_disp-h10  = it_disp-h10  + pa_disp-h10 .
  it_disp-h12  = it_disp-h12  + pa_disp-h12 .
  it_disp-h14  = it_disp-h14  + pa_disp-h14 .
  it_disp-h16  = it_disp-h16  + pa_disp-h16 .
  it_disp-h18  = it_disp-h18  + pa_disp-h18 .
  it_disp-h20  = it_disp-h20  + pa_disp-h20 .
  it_disp-h22  = it_disp-h22  + pa_disp-h22 .
  it_disp-h24  = it_disp-h24  + pa_disp-h24 .
  it_disp-h26  = it_disp-h26  + pa_disp-h26 .
  it_disp-h28  = it_disp-h28  + pa_disp-h28 .
  it_disp-h30  = it_disp-h30  + pa_disp-h30 .
  it_disp-h32  = it_disp-h32  + pa_disp-h32 .
  it_disp-h34  = it_disp-h34  + pa_disp-h34 .
  it_disp-h36  = it_disp-h36  + pa_disp-h36 .
  it_disp-h38  = it_disp-h38  + pa_disp-h38 .
  it_disp-h40  = it_disp-h40  + pa_disp-h40 .
  it_disp-h42  = it_disp-h42  + pa_disp-h42 .
  it_disp-h44  = it_disp-h44  + pa_disp-h44 .
  it_disp-h46  = it_disp-h46  + pa_disp-h46 .
  it_disp-h48  = it_disp-h48  + pa_disp-h48 .
  it_disp-h50  = it_disp-h50  + pa_disp-h50 .
  it_disp-h52  = it_disp-h52  + pa_disp-h52 .
  it_disp-h54  = it_disp-h54  + pa_disp-h54 .
  it_disp-h56  = it_disp-h56  + pa_disp-h56 .
  it_disp-h58  = it_disp-h58  + pa_disp-h58 .
  it_disp-h60  = it_disp-h60  + pa_disp-h60 .
** Furong on 07/17/12 for 3 shift
  it_disp-h62  = it_disp-h62  + pa_disp-h62 .
  it_disp-h64  = it_disp-h64  + pa_disp-h64 .
  it_disp-h66  = it_disp-h66  + pa_disp-h66 .
  it_disp-h68  = it_disp-h68  + pa_disp-h68 .
  it_disp-h70  = it_disp-h70  + pa_disp-h70 .
  it_disp-h72  = it_disp-h72  + pa_disp-h72 .
** End on 07/17/12

  it_disp-mitu = it_disp-mitu + pa_disp-mitu.
ENDFORM.                    " add_routine

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summary_disp.
  DATA: lt_disp                 LIKE TABLE OF it_disp  WITH HEADER LINE,
        l_line                  TYPE i  ,
        l_model(3)              TYPE c  ,
        l_status(3)             TYPE c  ,
        l_hours(2)              TYPE n  ,
        l_code(4)               TYPE c  ,              " ALC CODE
        l_vals1(5)              TYPE c  ,
        l_vals2(5)              TYPE c  ,
        l_vals3(5)              TYPE c  ,
        l_vals4(5)              TYPE c  ,
        l_vals5(5)              TYPE c  .              " ALC CODE VALUE

  lt_disp[] = it_disp[].

  " Accumulate the Data..
  CLEAR: it_disp, it_disp[].
  SORT lt_disp BY model     alc_vals1 alc_vals2
                  alc_vals3 alc_vals4 alc_vals5.
  READ TABLE lt_disp INDEX 1.
  l_model = lt_disp-model   .
* l_code  = lt_disp-alc_code.
  l_vals1 = lt_disp-alc_vals1.
  l_vals2 = lt_disp-alc_vals2.
  l_vals3 = lt_disp-alc_vals3.
  l_vals4 = lt_disp-alc_vals4.
  l_vals5 = lt_disp-alc_vals5.
  it_disp = lt_disp.
* CHANGED BY YONGPING LI
* NEED TO CLEAR THE IT_DISP WORK AREA QTY FIELDS
  PERFORM clear_qty_disp USING it_disp.                     "UD1K912950
* END OF CHANGE

  LOOP AT lt_disp.
    IF l_model = lt_disp-model      AND  l_vals1 = lt_disp-alc_vals1 AND
        l_vals2 = lt_disp-alc_vals2  AND  l_vals3 = lt_disp-alc_vals3 AND
        l_vals4 = lt_disp-alc_vals4  AND  l_vals5 = lt_disp-alc_vals5 .
      PERFORM add_routine  USING  lt_disp .
      CONTINUE.
    ELSE.
*     CONDENSE lt_disp-alc_code .
      it_disp-serial = strlen( l_vals )    .
      APPEND it_disp.     CLEAR: it_disp   .
      it_disp     = lt_disp         .
*     PERFORM clear_qty_disp  USING it_disp     .
      l_model = lt_disp-model   .
      l_vals1 = lt_disp-alc_vals1.
      l_vals2 = lt_disp-alc_vals2.
      l_vals3 = lt_disp-alc_vals3.
      l_vals4 = lt_disp-alc_vals4.
      l_vals5 = lt_disp-alc_vals5.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_disp LINES  l_line.
  IF l_line > 0.
*   CONDENSE lt_disp-alc_code .
    it_disp-serial = strlen( l_vals ) .
    APPEND it_disp.
  ENDIF.
ENDFORM.                    " SUMMARY_DISP

*&---------------------------------------------------------------------*
*&      Form  get_cuvtab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM get_cuvtab USING    pa_vtnam  pa_knobj.
  CLEAR: pa_knobj.
  SELECT SINGLE vtint INTO pa_knobj
    FROM cuvtab
   WHERE vtnam = pa_vtnam .
ENDFORM.                    " get_cuvtab

*&---------------------------------------------------------------------*
*&      Form  get_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM get_alc   USING    pa_atinn.
  DATA:  l_valc         LIKE cuvtab_valc-valc,
         lw_cuvtab_valc LIKE cuvtab_valc.

  l_valc = c_name       .
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_cuvtab_valc
    FROM cuvtab_valc
   WHERE vtint = pa_atinn
     AND valc  = l_valc  .

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_alc
    FROM cuvtab_valc
   WHERE vtint = pa_atinn
     AND slnid = lw_cuvtab_valc-slnid
     AND valc  NE l_valc  .
ENDFORM.                    " get_alc

*&---------------------------------------------------------------------*
*&      Form  read_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_alc.
  DATA: lt_sum              LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  SORT it_sum BY hours worder extc intc vm_model.
  READ TABLE it_sum INDEX 1.
  l_hours = it_sum-hours   .
  l_wo    = it_sum-worder  .
  l_ext   = it_sum-extc    .
  l_int   = it_sum-intc    .
  l_model = it_sum-vm_model  .
  lt_sum  = it_sum          .

  " Work Order Summarize in the same time terms.
  LOOP AT it_sum.
    IF l_hours = it_sum-hours AND l_wo  = it_sum-worder AND
       l_ext   = it_sum-extc  AND l_int = it_sum-intc   AND
       l_model = it_sum-vm_model .
      IF it_sum-mitu = 'Y'      .
        l_mitucnt = l_mitucnt + 1    .
      ENDIF.
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_SUM-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_sum-cnt = l_cnt       .
      lt_sum-mitucnt = l_mitucnt.
      APPEND       lt_sum      .
      lt_sum  = it_sum         .
      l_cnt   = 1              .
      l_hours = it_sum-hours   .
      l_wo    = it_sum-worder  .
      l_ext   = it_sum-extc    .
      l_int   = it_sum-intc    .
      l_model = it_sum-vm_model.
      IF it_sum-mitu = 'Y'     .
        l_mitucnt = 1          .
      ELSE.
        CLEAR: l_mitucnt       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_sum LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_SUM-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                     it_sum-worder it_sum-extc      it_sum-intc .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_sum-cnt = l_cnt       .
    lt_sum-mitucnt = l_mitucnt.
    APPEND       lt_sum      .
  ENDIF.

  it_sum[] = lt_sum[].
ENDFORM.                    " read_alc

*&---------------------------------------------------------------------*
*&      Form  read_alc_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_alc_prod.
* DATA: lt_sum_prod         LIKE TABLE OF it_sum       WITH HEADER LINE,
*        l_model             LIKE it_sum-vm_model,
*        l_worder            LIKE mara-matnr  ,
*        l_hours             LIKE it_sum-hours,
*        l_wo                LIKE mara-matnr  ,
*        l_cnt               LIKE it_sum-cnt  ,
*        l_ext               LIKE it_sum-extc ,
*        l_int               LIKE it_sum-intc ,
*        l_size              LIKE it_sum-cnt  .
*
*  SORT it_sum_prod BY hours worder extc intc .
*  READ TABLE it_sum_prod INDEX 1.
*  l_hours = it_sum_prod-hours   .
*  l_wo    = it_sum_prod-worder  .
*  l_ext   = it_sum_prod-extc    .
*  l_int   = it_sum_prod-intc    .
*  l_model = it_sum_prod-vm_model  .
*
*  " Work Order Summarize in the same time terms.
*  LOOP AT it_sum_prod.
*    IF l_hours = it_sum_prod-hours AND l_wo  = it_sum_prod-worder AND
*       l_ext   = it_sum_prod-extc  AND l_int = it_sum_prod-intc   AND
*       l_model = it_sum_prod-vm_model .
*      lt_sum_prod  = it_sum_prod           .
*      l_cnt   = l_cnt + 1       .
*      CONTINUE.
*    ELSE.
*      PERFORM get_wororder   USING it_alc-type_alc  l_worder
*               it_sum_prod-worder it_sum_prod-extc  it_sum_prod-intc .
*      PERFORM get_alc_value  USING l_worder         lt_sum_prod-code
*                          CHANGING lt_sum_prod-vals                  .
*      lt_sum_prod-cnt = l_cnt       .
*      APPEND       lt_sum_prod      .
*      lt_sum_prod  = it_sum_prod    .
*      l_cnt   = 1              .
*      l_hours = it_sum_prod-hours   .
*      l_wo    = it_sum_prod-worder  .
*      l_ext   = it_sum_prod-extc    .
*      l_int   = it_sum_prod-intc    .
*      l_model = it_sum_prod-vm_model.
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE it_sum_prod LINES l_size .
*  IF l_size > 0 .
*    PERFORM get_alc_value  USING lt_sum_prod-worder  lt_sum_prod-code
*                        CHANGING lt_sum_prod-vals                .
*    lt_sum_prod-cnt = l_cnt       .
*    APPEND       lt_sum_prod      .
*  ENDIF.
*
*  it_sum_prod[] = lt_sum_prod[].
ENDFORM.                    " read_alc_PROD

*&---------------------------------------------------------------------*
*&      Form  create_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SEQ  text
*      -->P_1320   text
*----------------------------------------------------------------------*
FORM create_data  TABLES pa_tab STRUCTURE it_sum  USING  pa_rp .
  DATA: l_data    LIKE TABLE OF ztpp_input_plan      WITH HEADER LINE.

  CASE pa_rp.
    WHEN '00' OR '01'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = pa_rp .
    WHEN '02' OR '88'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '02'  .

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '03'  .
    WHEN '06'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '04'  .

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '05'  .
    WHEN '99'.
      SELECT * INTO      CORRESPONDING FIELDS OF TABLE l_data
        FROM ztpp_input_plan
       WHERE status = '01'  .
  ENDCASE.

  IF p_test = 'X'.
    LOOP AT l_data.
      IF l_data-work_order+12(2) = 'XX' OR
         l_data-work_order+12(2) = 'XY' .
        DELETE l_data.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT l_data BY serial DESCENDING.
  LOOP AT l_data.
    CLEAR: pa_tab.
    pa_tab-vm_model   = l_data-modl       .
    pa_tab-vm_bodyser = l_data-body_ser   .
    pa_tab-worder     = l_data-work_order .
    pa_tab-mitu       = l_data-mitu       .
    pa_tab-extc       = l_data-extc       .
    pa_tab-intc       = l_data-intc       .
    pa_tab-status     = l_data-status     .
    pa_tab-rp         = '06'              .
    pa_tab-cnt        = 1                 .
    pa_tab-serial     = l_data-serial     .
    APPEND pa_tab .
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
FORM calc_seq.
  DATA: lt_seq              LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  SORT it_seq BY hours worder extc intc vm_model.
  READ TABLE it_seq INDEX 1.    CLEAR: l_cnt, l_mitucnt.
  l_hours = it_seq-hours   .
  l_wo    = it_seq-worder  .
  l_ext   = it_seq-extc    .
  l_int   = it_seq-intc    .
  l_model = it_seq-vm_model.
  lt_seq  = it_seq         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_seq.
    IF l_hours = it_seq-hours AND l_wo  = it_seq-worder AND
       l_ext   = it_seq-extc  AND l_int = it_seq-intc   AND
       l_model = it_seq-vm_model .
      IF it_seq-mitu = 'Y'      .
        l_mitucnt = l_mitucnt + 1    .
      ENDIF.
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_SEQ-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_seq-cnt = l_cnt       .
      lt_seq-mitucnt = l_mitucnt.
      APPEND       lt_seq      .
      lt_seq  = it_seq         .
      l_cnt   = 1              .
      l_hours = it_seq-hours   .
      l_wo    = it_seq-worder  .
      l_ext   = it_seq-extc    .
      l_int   = it_seq-intc    .
      l_model = it_seq-vm_model.
      IF it_seq-mitu = 'Y'     .
        l_mitucnt = 1          .
      ELSE.
        CLEAR: l_mitucnt       .
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_seq LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_SEQ-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                   l_wo   l_ext     l_int      .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_seq-cnt = l_cnt       .
    lt_seq-mitucnt = l_mitucnt.
    APPEND       lt_seq      .
  ENDIF.

  it_seq[] = lt_seq[].
ENDFORM.                    " CALC_SEQ

*&---------------------------------------------------------------------*
*&      Form  CALC_BODYINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_bodyinput.
  DATA: lt_bi               LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  SORT it_bi  BY hours  worder extc intc vm_model.
  READ TABLE it_bi  INDEX 1.
  l_hours = it_bi-hours   .
  l_wo    = it_bi-worder  .
  l_ext   = it_bi-extc    .
  l_int   = it_bi-intc    .
  l_model = it_bi-vm_model.
  lt_bi   = it_bi         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_bi .
    IF l_hours = it_bi-hours  AND l_wo  = it_bi-worder AND
       l_ext   = it_bi-extc   AND l_int = it_bi-intc   AND
       l_model = it_bi-vm_model .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_BI-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_bi-cnt = l_cnt       .
      lt_bi-mitucnt = l_mitucnt.
      APPEND       lt_bi      .
      lt_bi   = it_bi         .
      l_cnt   = 1             .
      l_hours = it_bi-hours   .
      l_wo    = it_bi-worder  .
      l_ext   = it_bi-extc    .
      l_int   = it_bi-intc    .
      l_model = it_bi-vm_model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_bi  LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_BI-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                   l_wo   l_ext     l_int      .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_bi-cnt = l_cnt       .
    APPEND       lt_bi       .
  ENDIF.

  it_bi[] = lt_bi[].
ENDFORM.                    " CALC_BODYINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_wbs.
  DATA: lt_wbs              LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  DESCRIBE TABLE it_wbs LINES l_cnt .
  CHECK l_cnt > 0 .

  l_mitucnt = p_wbs * wa_uph_b.
  IF l_mitucnt > 0 .
    IF l_mitucnt < l_cnt .
      DELETE it_wbs TO l_mitucnt.
    ELSE.
      DELETE it_wbs TO l_cnt    .
    ENDIF.
  ELSE.
    DELETE it_wbs TO l_cnt.
  ENDIF.
  CLEAR: l_mitucnt, l_cnt.

  SORT it_wbs BY hours  worder extc intc vm_model.
  READ TABLE it_wbs INDEX 1.
  l_hours  = it_wbs-hours   .
  l_wo     = it_wbs-worder  .
  l_ext    = it_wbs-extc    .
  l_int    = it_wbs-intc    .
  l_model  = it_wbs-vm_model.
  lt_wbs   = it_wbs         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_wbs.
    IF l_hours = it_wbs-hours AND l_wo  = it_wbs-worder AND
       l_ext   = it_wbs-extc  AND l_int = it_wbs-intc   AND
       l_model = it_wbs-vm_model .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_WBS-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_wbs-cnt = l_cnt       .
      APPEND       lt_wbs      .
      lt_wbs  = it_wbs         .
      l_cnt   = 1              .
      l_hours = it_wbs-hours   .
      l_wo    = it_wbs-worder  .
      l_ext   = it_wbs-extc    .
      l_int   = it_wbs-intc    .
      l_model = it_wbs-vm_model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_wbs LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_WBS-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                   l_wo   l_ext     l_int      .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_wbs-cnt = l_cnt       .
    APPEND       lt_wbs      .
  ENDIF.

  it_wbs[] = lt_wbs[].
ENDFORM.                    " CALC_WBS

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_paintinput.
  DATA: lt_pi               LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  SORT it_pi  BY hours worder extc intc vm_model.
  READ TABLE it_pi  INDEX 1.
  l_hours = it_pi-hours   .
  l_wo    = it_pi-worder  .
  l_ext   = it_pi-extc    .
  l_int   = it_pi-intc    .
  l_model = it_pi-vm_model.
  lt_pi   = it_pi         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_pi .
    IF l_hours = it_pi-hours AND l_wo  = it_pi-worder AND
       l_ext   = it_pi-extc  AND l_int = it_pi-intc   AND
       l_model = it_pi-vm_model .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_PI-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_pi-cnt = l_cnt       .
      APPEND       lt_pi      .
      lt_pi   = it_pi          .
      l_cnt   = 1              .
      l_hours = it_pi-hours   .
      l_wo    = it_pi-worder  .
      l_ext   = it_pi-extc    .
      l_int   = it_pi-intc    .
      l_model = it_pi-vm_model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pi LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_PI-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                   l_wo   l_ext     l_int      .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_pi-cnt = l_cnt       .
    APPEND       lt_pi      .
  ENDIF.

  it_pi[] = lt_pi[].
ENDFORM.                    " CALC_PAINTINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTREJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_paintreject.
  DATA: lt_prj              LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  DESCRIBE TABLE it_prj LINES l_cnt  .
  CHECK l_cnt > 0 .

  l_mitucnt = p_prj * wa_uph_p.
  IF l_mitucnt > 0 .
    IF l_mitucnt < l_cnt .
      DELETE it_prj TO l_mitucnt.
    ELSE.
      DELETE it_prj TO l_cnt    .
    ENDIF.
  ELSE.
    DELETE it_prj TO l_cnt.
  ENDIF.
  CLEAR: l_mitucnt, l_cnt.

  SORT it_prj BY hours worder extc intc vm_model.
  READ TABLE it_prj INDEX 1.
  l_hours = it_prj-hours   .
  l_wo    = it_prj-worder  .
  l_ext   = it_prj-extc    .
  l_int   = it_prj-intc    .
  l_model = it_prj-vm_model.
  lt_prj  = it_prj         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_prj.
    IF l_hours = it_prj-hours AND l_wo  = it_prj-worder AND
       l_ext   = it_prj-extc  AND l_int = it_prj-intc   AND
       l_model = it_prj-vm_model .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_PRJ-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_prj-cnt = l_cnt       .
      APPEND       lt_prj      .
      lt_prj  = it_prj         .
      l_cnt   = 1              .
      l_hours = it_prj-hours   .
      l_wo    = it_prj-worder  .
      l_ext   = it_prj-extc    .
      l_int   = it_prj-intc    .
      l_model = it_prj-vm_model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_prj LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_PRJ-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                   l_wo   l_ext     l_int      .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_prj-cnt = l_cnt       .
    APPEND       lt_prj      .
  ENDIF.

  it_prj[] = lt_prj[].
ENDFORM.                    " CALC_PAINTREJECT

*&---------------------------------------------------------------------*
*&      Form  CALC_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_pbs.
  DATA: lt_pbs              LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  SORT it_pbs BY hours worder extc intc vm_model.
  READ TABLE it_pbs INDEX 1.
  l_hours = it_pbs-hours   .
  l_wo    = it_pbs-worder  .
  l_ext   = it_pbs-extc    .
  l_int   = it_pbs-intc    .
  l_model = it_pbs-vm_model.
  lt_pbs  = it_pbs         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_pbs.
    IF l_hours = it_pbs-hours AND l_wo  = it_pbs-worder AND
       l_ext   = it_pbs-extc  AND l_int = it_pbs-intc   AND
       l_model = it_pbs-vm_model .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_PBS-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_pbs-cnt = l_cnt       .
      APPEND       lt_pbs      .
      lt_pbs  = it_pbs         .
      l_cnt   = 1              .
      l_hours = it_pbs-hours   .
      l_wo    = it_pbs-worder  .
      l_ext   = it_pbs-extc    .
      l_int   = it_pbs-intc    .
      l_model = it_pbs-vm_model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_pbs LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_PBS-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                   l_wo   l_ext     l_int      .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_pbs-cnt = l_cnt       .
    APPEND       lt_pbs      .
  ENDIF.

  it_pbs[] = lt_pbs[].
ENDFORM.                    " CALC_PBS

*&---------------------------------------------------------------------*
*&      Form  GET_WORORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_TYPE_ALC  text
*      -->P_L_WORDER  text
*----------------------------------------------------------------------*
FORM get_wororder USING  pa_type  pa_worder  pa_wo  pa_ext  pa_int .
  IF pa_type = 'C'.
    CONCATENATE pa_wo  pa_ext  pa_int  INTO  pa_worder .
  ELSE.
    pa_worder = pa_wo .
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
FORM insert_field_vals.
  " Insert D-1 Field Value into Internal Table IT_DISP...
  LOOP AT it_disp.
    READ TABLE it_disp_d1 WITH KEY model     = it_disp-model
                                     alc_vals1 = it_disp-alc_vals1
                                     alc_vals2 = it_disp-alc_vals2
                                     alc_vals3 = it_disp-alc_vals3
                                     alc_vals4 = it_disp-alc_vals4
                                     alc_vals5 = it_disp-alc_vals5.
    IF sy-subrc = 0.
      it_disp-d_1 = it_disp_d1-d_1 .
      DELETE it_disp_d1 WHERE model     = it_disp-model
                            AND alc_vals1 = it_disp-alc_vals1
                            AND alc_vals2 = it_disp-alc_vals2
                            AND alc_vals3 = it_disp-alc_vals3
                            AND alc_vals4 = it_disp-alc_vals4
                            AND alc_vals5 = it_disp-alc_vals5 .
    ELSE.
      CLEAR: it_disp-d_1 .
    ENDIF.

    READ TABLE it_disp_seq  WITH KEY model     = it_disp-model
                                     alc_vals1 = it_disp-alc_vals1
                                     alc_vals2 = it_disp-alc_vals2
                                     alc_vals3 = it_disp-alc_vals3
                                     alc_vals4 = it_disp-alc_vals4
                                     alc_vals5 = it_disp-alc_vals5.
    IF sy-subrc = 0.
      it_disp-seq  = it_disp_seq-d_1 .
      it_disp-mitu = it_disp_seq-mitu.
      DELETE it_disp_seq  WHERE model     = it_disp-model
                            AND alc_vals1 = it_disp-alc_vals1
                            AND alc_vals2 = it_disp-alc_vals2
                            AND alc_vals3 = it_disp-alc_vals3
                            AND alc_vals4 = it_disp-alc_vals4
                            AND alc_vals5 = it_disp-alc_vals5 .
    ELSE.
      CLEAR: it_disp-seq, it_disp-mitu.
    ENDIF.

    READ TABLE it_disp_bi   WITH KEY model     = it_disp-model
                                     alc_vals1 = it_disp-alc_vals1
                                     alc_vals2 = it_disp-alc_vals2
                                     alc_vals3 = it_disp-alc_vals3
                                     alc_vals4 = it_disp-alc_vals4
                                     alc_vals5 = it_disp-alc_vals5.
    IF sy-subrc = 0.
      it_disp-bodyin = it_disp_bi-d_1 .
      DELETE it_disp_bi   WHERE model     = it_disp-model
                            AND alc_vals1 = it_disp-alc_vals1
                            AND alc_vals2 = it_disp-alc_vals2
                            AND alc_vals3 = it_disp-alc_vals3
                            AND alc_vals4 = it_disp-alc_vals4
                            AND alc_vals5 = it_disp-alc_vals5 .
    ELSE.
      CLEAR: it_disp-bodyin  .
    ENDIF.

    READ TABLE it_disp_wbs  WITH KEY model     = it_disp-model
                                     alc_vals1 = it_disp-alc_vals1
                                     alc_vals2 = it_disp-alc_vals2
                                     alc_vals3 = it_disp-alc_vals3
                                     alc_vals4 = it_disp-alc_vals4
                                     alc_vals5 = it_disp-alc_vals5.
    IF sy-subrc = 0.
      it_disp-wbs = it_disp_wbs-d_1 .
      DELETE it_disp_wbs  WHERE model     = it_disp-model
                            AND alc_vals1 = it_disp-alc_vals1
                            AND alc_vals2 = it_disp-alc_vals2
                            AND alc_vals3 = it_disp-alc_vals3
                            AND alc_vals4 = it_disp-alc_vals4
                            AND alc_vals5 = it_disp-alc_vals5 .
    ELSE.
      CLEAR: it_disp-wbs .
    ENDIF.

    READ TABLE it_disp_pi   WITH KEY model     = it_disp-model
                                     alc_vals1 = it_disp-alc_vals1
                                     alc_vals2 = it_disp-alc_vals2
                                     alc_vals3 = it_disp-alc_vals3
                                     alc_vals4 = it_disp-alc_vals4
                                     alc_vals5 = it_disp-alc_vals5.
    IF sy-subrc = 0.
      it_disp-paint  = it_disp_pi-d_1 .
      DELETE it_disp_pi   WHERE model     = it_disp-model
                            AND alc_vals1 = it_disp-alc_vals1
                            AND alc_vals2 = it_disp-alc_vals2
                            AND alc_vals3 = it_disp-alc_vals3
                            AND alc_vals4 = it_disp-alc_vals4
                            AND alc_vals5 = it_disp-alc_vals5 .
    ELSE.
      CLEAR: it_disp-paint  .
    ENDIF.

    READ TABLE it_disp_prj  WITH KEY model     = it_disp-model
                                     alc_vals1 = it_disp-alc_vals1
                                     alc_vals2 = it_disp-alc_vals2
                                     alc_vals3 = it_disp-alc_vals3
                                     alc_vals4 = it_disp-alc_vals4
                                     alc_vals5 = it_disp-alc_vals5.
    IF sy-subrc = 0.
      it_disp-prj = it_disp_prj-d_1 .
      DELETE it_disp_prj  WHERE model     = it_disp-model
                            AND alc_vals1 = it_disp-alc_vals1
                            AND alc_vals2 = it_disp-alc_vals2
                            AND alc_vals3 = it_disp-alc_vals3
                            AND alc_vals4 = it_disp-alc_vals4
                            AND alc_vals5 = it_disp-alc_vals5 .
    ELSE.
      CLEAR: it_disp-prj .
    ENDIF.

    READ TABLE it_disp_pbs  WITH KEY model     = it_disp-model
                                     alc_vals1 = it_disp-alc_vals1
                                     alc_vals2 = it_disp-alc_vals2
                                     alc_vals3 = it_disp-alc_vals3
                                     alc_vals4 = it_disp-alc_vals4
                                     alc_vals5 = it_disp-alc_vals5.
    IF sy-subrc = 0.
      it_disp-pbs = it_disp_pbs-d_1 .
      DELETE it_disp_pbs  WHERE model     = it_disp-model
                            AND alc_vals1 = it_disp-alc_vals1
                            AND alc_vals2 = it_disp-alc_vals2
                            AND alc_vals3 = it_disp-alc_vals3
                            AND alc_vals4 = it_disp-alc_vals4
                            AND alc_vals5 = it_disp-alc_vals5 .
    ELSE.
      CLEAR: it_disp-pbs .
    ENDIF.

    MODIFY it_disp.
  ENDLOOP.

  " Remain Fiedls Insert.....
  LOOP AT it_disp_d1 .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_d1 TO it_disp.
    CLEAR: it_disp-d_1, it_disp-d_1.
    it_disp-d_1 = it_disp_d1-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_d1, it_disp_d1[].

  LOOP AT it_disp_seq  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_seq  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-seq.
    it_disp-seq = it_disp_seq-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_seq , it_disp_seq[].

  LOOP AT it_disp_bi   .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_bi   TO it_disp.
    CLEAR: it_disp-d_1, it_disp-bodyin.
    it_disp-bodyin = it_disp_bi-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_bi  , it_disp_bi[].

  LOOP AT it_disp_wbs  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_wbs  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-wbs.
    it_disp-wbs = it_disp_wbs-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_wbs , it_disp_wbs[].

  LOOP AT it_disp_pi   .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_pi   TO it_disp.
    CLEAR: it_disp-d_1, it_disp-paint.
    it_disp-paint  = it_disp_pi-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_pi  , it_disp_pi[].

  LOOP AT it_disp_prj  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_prj  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-prj.
    it_disp-prj = it_disp_prj-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_prj , it_disp_prj[].

  LOOP AT it_disp_pbs  .
    CLEAR: it_disp.
    MOVE-CORRESPONDING it_disp_pbs  TO it_disp.
    CLEAR: it_disp-d_1, it_disp-pbs.
    it_disp-pbs = it_disp_pbs-d_1.
    APPEND it_disp.
  ENDLOOP.
  CLEAR: it_disp_pbs , it_disp_pbs[].
ENDFORM.                    " INSERT_FIELD_VALS

*&---------------------------------------------------------------------*
*&      Form  calc_prod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_prod.
  DATA: lt_d1               LIKE TABLE OF it_sum       WITH HEADER LINE,
        l_name(40)          TYPE c              ,
        l_model             LIKE it_sum-vm_model,
        l_worder            LIKE mara-matnr  ,
        l_hours             LIKE it_sum-hours,
        l_wo                LIKE mara-matnr  ,
        l_mitucnt           TYPE i           ,
        l_cnt               LIKE it_sum-cnt  ,
        l_ext               LIKE it_sum-extc ,
        l_int               LIKE it_sum-intc ,
        l_size              LIKE it_sum-cnt  .

  LOOP AT it_prod.
    CLEAR: it_d1 .
    CONCATENATE it_prod-wo_ser it_prod-nation it_prod-dealer
                it_prod-extc   it_prod-intc   INTO l_worder .
    PERFORM get_model  USING l_worder  it_d1-vm_model .
    DO it_prod-rp06q TIMES.
*     IT_D1-vm_bodyser = l_data-body_ser   .
      it_d1-worder     = l_worder(14)      .
*     IT_D1-mitu       = IT_PROD-mitu      .
      it_d1-extc       = it_prod-extc      .
      it_d1-intc       = it_prod-intc      .
*     IT_D1-status     = l_data-status     .
      it_d1-rp         = '06'              .
      it_d1-cnt        = 1                 .
      APPEND it_d1  .
    ENDDO.
  ENDLOOP.

  SORT it_d1 BY hours  worder extc intc vm_model.
  READ TABLE it_d1 INDEX 1.
  l_hours = it_d1-hours   .
  l_wo    = it_d1-worder  .
  l_ext   = it_d1-extc    .
  l_int   = it_d1-intc    .
  l_model = it_d1-vm_model.
  lt_d1   = it_d1         .

  " Work Order Summarize in the same time terms.
  LOOP AT it_d1  .
    IF l_hours = it_d1-hours AND l_wo  = it_d1-worder AND
       l_ext   = it_d1-extc  AND l_int = it_d1-intc   AND
       l_model = it_d1-vm_model .
      l_cnt   = l_cnt + 1       .
      CONTINUE.
    ELSE.
      CLEAR: wa_count.
      LOOP AT it_alc WHERE model = l_model .
        wa_count = wa_count + 1.
        CONCATENATE 'LT_D1-ALC_VALS' wa_count  INTO l_name .
        ASSIGN   (l_name)                       TO   <wa_vals>.
        PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                     l_wo   l_ext     l_int      .
        PERFORM get_alc_value  USING l_worder         it_alc-valc
                            CHANGING <wa_vals>                   .
      ENDLOOP.
      lt_d1-cnt = l_cnt       .
      APPEND       lt_d1      .
      lt_d1   = it_d1          .
      l_cnt   = 1              .
      l_hours = it_d1-hours   .
      l_wo    = it_d1-worder  .
      l_ext   = it_d1-extc    .
      l_int   = it_d1-intc    .
      l_model = it_d1-vm_model.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_d1 LINES l_size .
  IF l_size > 0 .
    CLEAR: wa_count.
    LOOP AT it_alc WHERE model = l_model .
      wa_count = wa_count + 1.
      CONCATENATE 'LT_D1-ALC_VALS' wa_count  INTO l_name .
      ASSIGN   (l_name)                       TO   <wa_vals>.
      PERFORM get_wororder   USING it_alc-type_alc  l_worder
                                   l_wo   l_ext     l_int      .
      PERFORM get_alc_value  USING l_worder         it_alc-valc
                          CHANGING <wa_vals>                  .
    ENDLOOP.
    lt_d1-cnt = l_cnt       .
    APPEND       lt_d1      .
  ENDIF.

  it_d1[] = lt_d1[].
ENDFORM.                    " calc_prod

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
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  GET_MODELS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_models.
  DATA: l_atwrt          LIKE ausp-atwrt,
        l_atinn          LIKE cabn-atinn.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_MODEL'.

  SELECT n~atwrt INTO l_atwrt
    FROM cawn AS n INNER JOIN cawnt AS t
      ON n~atinn = t~atinn
     AND n~atzhl = t~atzhl
   WHERE n~atinn = l_atinn
     AND t~spras = sy-langu .
    it_model-modl = l_atwrt(3).
    APPEND it_model .
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
FORM set_information.
  DATA: l_check              TYPE c           ,
        l_chk                TYPE p DECIMALS 3,
        l_tot                TYPE p DECIMALS 3,
        l_htot               TYPE p DECIMALS 3,
        l_date               TYPE d ,
        l_count              TYPE i .

  " Set the BASIC Information for the UPH & Work Time...
  " Gather the 3 day's UPH Except shift...
  CLEAR: l_date, l_count.        l_date = p_dates - 1 .
  DO 3 TIMES.
    l_count  = l_count + 1.      CLEAR: l_chk.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
* -- start
*    perform get_day          using l_date it_master-day  .
*    perform get_working_time using l_date it_master-time it_master-day.
*    perform get_uph          using l_date it_master-uph it_master-shift.
*    if l_check = space.
*      perform get_uph_shop   using l_date  wa_uph_t   'T'              .
*      perform get_uph_shop   using l_date  wa_uph_b   'B'              .
*      perform get_uph_shop   using l_date  wa_uph_p   'P'              .
*      l_check = 'X'.
*    endif.
*    it_master-tqty = it_master-uph * ( it_master-time / 3600 ).
*-- end replaced with function

    CALL FUNCTION 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        i_date = l_date
        i_shop = 'T'
      IMPORTING
        e_qty  = it_master-tqty.


    it_master-seq    = l_count.
    it_master-date   = l_date .
    APPEND it_master.  CLEAR: it_master.
  ENDDO.

  CLEAR: l_date, l_count, wa_hour.  l_date = p_dates - 1  .
  DO 3 TIMES   .
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date .
    PERFORM get_day       USING l_date it_shift-day  .
    PERFORM get_uph       USING l_date it_shift-uph it_shift-shift.
    it_shift-seq    = l_count .   it_shift-date   = l_date .
    PERFORM get_worktime1 USING l_date        l_count
                                it_shift-time it_shift-day .
    CLEAR: it_shift.
  ENDDO.

  " Set the Day's Total Quantity(IT_MASTER, IT_SHIFT)
  DATA: wa_shift LIKE it_shift, l_idx LIKE sy-tabix.
  SORT it_shift BY ser seq shift .
  LOOP AT it_master.
    CLEAR: l_chk, l_tot, l_htot  .

    LOOP AT it_shift WHERE seq = it_master-seq .
      l_tot = l_tot + it_shift-tqty            .
      wa_shift = it_shift.
      l_idx = sy-tabix.
    ENDLOOP.
    l_chk = it_master-tqty - l_tot           .
    IF l_chk <> 0.                                        "7/31/2012
*FIXIT LATER
*      wa_shift-tqty = wa_shift-tqty + l_chk  .
*      wa_shift-hqty = wa_shift-tqty / wa_shift-hloop.
*      modify it_shift index l_idx from wa_shift transporting tqty hqty.
    ENDIF.

  ENDLOOP.
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
   WHERE arbpl = 'T'
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
FORM get_worktime1 USING    pa_wdate  pa_cnt  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_chk         TYPE p DECIMALS 3        ,
        l_tc37a       LIKE tc37a               ,
        l_date        TYPE d ,
        l_flag        TYPE c ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = 'T'
     AND datub >= pa_wdate .

  SORT lt_capa BY datub tagnr schnr .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    wa_hour = wa_hour + 1 .
    CLEAR: l_einzt.
    SELECT SINGLE * INTO l_tc37a       " l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    it_shift-ser   = wa_hour       .
    it_shift-time  = l_tc37a-einzt .
    IF l_tc37a-begzt >= l_tc37a-endzt.
      it_shift-total = l_tc37a-endzt +   l_tc37a-begzt            .
    ELSE.
      it_shift-total = l_tc37a-endzt -   l_tc37a-begzt            .
    ENDIF.
    it_shift-ftime = l_tc37a-begzt .
    it_shift-shift = lt_capa-schnr .
*7/31/12 requested by MY Hur (ceil -> floor)
    CALL FUNCTION 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        i_date  = pa_wdate
        i_shop  = 'T'
        i_shift = it_shift-shift
      IMPORTING
        e_qty   = it_shift-tqty.
*    it_shift-tqty  =  floor( it_shift-uph * ( it_shift-time / 3600 ) ).
*    l_chk = ceil( it_shift-time / 7200 ) .
    it_shift-hqty  = floor( it_shift-tqty / ( it_shift-time / 7200 ) ).

    it_shift-hloop = ceil( it_shift-time / 7200 )  .
    APPEND it_shift.
  ENDLOOP.
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
      shop  = 'T'
    IMPORTING
      uph   = w_uph.
  pa_uph  = w_uph.

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
*       AND arbpl     = 'T'     .
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
*       AND arbpl     = 'T'    .
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
FORM get_start_day USING    pa_datum.
  CLEAR: ztpp_common_vals.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1 .

  pa_datum = ztpp_common_vals-dates.
ENDFORM.                    " GET_START_DAY

*&---------------------------------------------------------------------*
*&      Form  CHECK_CNT_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_cnt_days.

ENDFORM.                    " CHECK_CNT_DAYS

*&---------------------------------------------------------------------*
*&      Form  UPDATE_INPUT_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DAYS  text
*----------------------------------------------------------------------*
FORM update_input_plan USING    pa_days.
  UPDATE ztpp_input_plan   SET rs01 = pa_days
                         WHERE plnt = it_data-plnt
                           AND line = it_data-line
                           AND modl = it_data-modl
                           AND body_ser = it_data-body_ser
                           AND seq_date = it_data-seq_date
                           AND seq_serial = it_data-seq_serial
                           AND seq_code = it_data-seq_code .
ENDFORM.                    " UPDATE_INPUT_PLAN

*&---------------------------------------------------------------------*
*&      Form  GET_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*      -->P_IT_D1_VM_MODEL  text
*----------------------------------------------------------------------*
FORM get_model USING    pa_worder   pa_model.
  DATA: lt_val          LIKE TABLE OF zspp_vin_value   WITH HEADER LINE.

  lt_val-atnam = 'P_MODEL'.  APPEND lt_val.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = pa_worder
      ctype        = '001'
*     DISPLAY      = 'D'
    TABLES
      val_table    = lt_val
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      OTHERS       = 5.

  READ TABLE lt_val  INDEX 1.
  IF sy-subrc = 0 AND lt_val-zflag IS INITIAL.
    pa_model = lt_val-atwrt.
  ELSE.
    CLEAR: pa_model.
  ENDIF.
ENDFORM.                    " GET_MODEL

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_WA_UPH_T  text
*      -->P_6752   text
*----------------------------------------------------------------------*
FORM get_uph_shop USING    pa_wdate  pa_uph  pa_arbpl .
  DATA: w_uph  LIKE ztpp_status-uph.

  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date  = pa_wdate
*     SHIFT =
      shop  = pa_arbpl
    IMPORTING
      uph   = w_uph.
  pa_uph  = w_uph.

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
ENDFORM.                    " get_uph_shop
*&---------------------------------------------------------------------*
*&      Form  CLEAR_QTY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DISP  text
*----------------------------------------------------------------------*
FORM clear_qty_disp USING    pa_disp  STRUCTURE it_disp .
  CLEAR: pa_disp-h02, pa_disp-h04, pa_disp-h06, pa_disp-h08,
         pa_disp-h10, pa_disp-h12, pa_disp-h14, pa_disp-h16,
         pa_disp-h18, pa_disp-h20, pa_disp-h22, pa_disp-h24,
         pa_disp-h26, pa_disp-h28, pa_disp-h30, pa_disp-h32,
         pa_disp-h34, pa_disp-h36, pa_disp-h38, pa_disp-h40,
         pa_disp-h42, pa_disp-h44, pa_disp-h46, pa_disp-h48,
         pa_disp-h50, pa_disp-h52, pa_disp-h54, pa_disp-h56,
         pa_disp-h58, pa_disp-h60, pa_disp-h62, pa_disp-h64,
         pa_disp-h66, pa_disp-h68, pa_disp-h70, pa_disp-h72,
         pa_disp-mitu, pa_disp-d_1, pa_disp-seq, pa_disp-bodyin,
         pa_disp-wbs, pa_disp-prj, pa_disp-pbs, pa_disp-paint.
ENDFORM.                    " CLEAR_QTY_DISP
*&---------------------------------------------------------------------*
*&      Form  SUB_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_total.
  LOOP AT it_disp.
    it_disp-stot =
           it_disp-h02 + it_disp-h04 + it_disp-h06 + it_disp-h08
         + it_disp-h10 + it_disp-h12 + it_disp-h14 + it_disp-h16
         + it_disp-h18 + it_disp-h20 + it_disp-h22 + it_disp-h24
         + it_disp-h26 + it_disp-h28 + it_disp-h30 + it_disp-h32
         + it_disp-h34 + it_disp-h36 + it_disp-h38 + it_disp-h40
         + it_disp-h42 + it_disp-h44 + it_disp-h46 + it_disp-h48
         + it_disp-h50 + it_disp-h52 + it_disp-h54 + it_disp-h56
         + it_disp-h58 + it_disp-h60
** furong on 07/17/12 for 3 shift
         + it_disp-h62 + it_disp-h64 + it_disp-h66 + it_disp-h68
         + it_disp-h70 + it_disp-h72.
** End
    MODIFY it_disp.
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
