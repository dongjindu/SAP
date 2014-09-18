************************************************************************
* Program Name      : ZRSD_OSR_SIGNOFF
* Author            : Furong Wang
* Creation Date     : 03/23/12
* Specifications By : Lee
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 05.21.2014 Victor         Read UM data differently per Nation
************************************************************************

REPORT zrsd_osr_signoff MESSAGE-ID zm_hma.

*---------------------------------------------------------------------*
*  INCLUDE
*---------------------------------------------------------------------*
INCLUDE zrsd_osr_signoff_top.

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  gv_repid = sy-repid.

*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM start_progressbar USING
          'Getting OSR data and assgin Sign-off date' '40'.

  w_date = sy-datum.
  w_time = sy-uzeit.

  IF p_dis = 'X'.
    PERFORM get_from_ztable.
  ELSE.
    PERFORM get_input_osr.
    IF w_cn > 0.
      PERFORM start_progressbar USING 'Assign VIN No' '40'.
      PERFORM mod_data.
      PERFORM get_est_soff_from_delay_veh.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE it_plan LINES w_cn.
  IF  w_cn > 0.
    IF p_dis = 'X'.
      PERFORM display_data.
    ELSE.
      PERFORM start_progressbar USING 'Save Data' '20'.
      PERFORM save_data.
    ENDIF.
  ELSE.
    MESSAGE s000.
  ENDIF.

END-OF-SELECTION.

*************************************************
FORM start_progressbar USING pf_text value(pf_val).
  DATA: percent(3) TYPE n.

  MOVE: sy-index TO percent.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text
    EXCEPTIONS
      OTHERS     = 1.
ENDFORM.                    " P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_input_osr.
  DATA: l_kalid LIKE t001w-fabkl,
        l_index LIKE sy-tabix.

  DATA: l_date               TYPE d ,
        l_chk                TYPE p DECIMALS 3,
        l_day                LIKE kapa-tagnr,      " Day
        l_time               TYPE kapendzt  ,      " Times for working
        l_uph                TYPE zvpp_ld-lrate,   " UPH
        l_count              TYPE i,
        l_cap                TYPE i,
        l_round              TYPE p DECIMALS 3,
        l_point              TYPE p DECIMALS 3 VALUE '0.500'.


  SELECT * INTO TABLE it_input_osr
    FROM ztpp_input_osr
    WHERE work_order IN s_worder.

  LOOP AT it_input_osr.
    l_index = sy-tabix.
    IF it_input_osr-work_order+12(2) <> 'AA' AND
      it_input_osr-work_order+12(2) <> 'AC'.
      DELETE it_input_osr INDEX l_index.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_input_osr LINES w_cn.
  IF w_cn = 0.
    EXIT.
  ENDIF.

  SELECT SINGLE fabkl INTO l_kalid
    FROM t001w
   WHERE werks = 'P001' .

  l_date = p_date.

  DO 150 TIMES.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
        correct_option               = '+'
        date                         = l_date
        factory_calendar_id          = l_kalid
      IMPORTING
        date                         = l_date
      EXCEPTIONS
        calendar_buffer_not_loadable = 1
        correct_option_invalid       = 2
        date_after_range             = 3
        date_before_range            = 4
        date_invalid                 = 5
        factory_calendar_not_found   = 6
        OTHERS                       = 7.

    PERFORM get_day      USING l_date  l_day.
    PERFORM get_worktime USING l_date  l_time l_day  'T'.
    PERFORM get_uph      USING l_date  l_uph   'T'   .
    l_chk = l_time / 3600.

    l_round  = l_uph * l_chk.
    IF l_round >= l_point.
      l_round  = l_round  - l_point.
    ENDIF.
    g_tcap = ceil( l_round ).
*    G_TCAP =   CEIL( L_UPH * L_CHK ).
    it_capacity-date = l_date.
    it_capacity-cap = g_tcap.
    APPEND it_capacity.
    l_date = l_date + 1.
    l_cap = l_cap + g_tcap.
    IF l_cap >= w_cn.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  get_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
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
*&      Form  get_worktime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_TIME  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM get_worktime     USING pa_wdate  pa_wktime  pa_day  pa_wc.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_flag        TYPE c ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = pa_wc
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
    l_wtime  = l_wtime  + l_einzt.
  ENDLOOP.

  pa_wktime = l_wtime .
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
FORM get_uph USING pa_wdate  pa_uph pa_wc.
  DATA: w_uph  LIKE ztpp_status-uph.

  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date  = pa_wdate
*     SHIFT =
      shop  = pa_wc
    IMPORTING
      uph   = w_uph.
  pa_uph = w_uph.

ENDFORM.                    " GET_UPH
*&---------------------------------------------------------------------*
*&      Form  MOD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mod_data.
  DATA : BEGIN OF lt_um OCCURS 0.
          INCLUDE STRUCTURE ztsd_um.
  DATA: assign(1).
  DATA : END OF lt_um.

  DATA: lt_b28      LIKE lt_um OCCURS 0 WITH HEADER LINE,
        lt_oth_natn LIKE lt_um OCCURS 0 WITH HEADER LINE.

  DATA:  l_wo LIKE it_um-wo_serial,
         l_nation LIKE it_um-wo_nation,
         l_dealer LIKE it_um-wo_dealer,
         l_index LIKE sy-tabix.

  DATA : BEGIN OF lt_plan_b OCCURS 0.
          INCLUDE STRUCTURE ztpp_soff_plan.
  DATA : END OF lt_plan_b.

** Assign sign-off date
*  SORT IT_INPUT_OSR BY SERIAL SEQ_NO.
*  LOOP AT IT_INPUT_OSR.
*    MOVE-CORRESPONDING IT_INPUT_OSR TO IT_PLAN.
*    IT_PLAN-SEQ_NO = IT_INPUT_OSR-SEQ_NO.
*    CONCATENATE IT_PLAN-SERIAL IT_PLAN-SEQ_SERIAL
*           INTO IT_PLAN-ZSER_NO.
*    APPEND IT_PLAN.
*  ENDLOOP.
*
*  DESCRIBE TABLE IT_PLAN LINES W_CN.
*  L_INDEX = 1.
*  LOOP AT IT_CAPACITY.
*    G_TCAP = IT_CAPACITY-CAP.
*    DO G_TCAP TIMES.
*      READ TABLE IT_PLAN INDEX L_INDEX.
*      IT_PLAN-DATE_SOFF = IT_CAPACITY-DATE.
*      MODIFY IT_PLAN INDEX L_INDEX TRANSPORTING DATE_SOFF.
*      L_INDEX = L_INDEX + 1.
*      IF L_INDEX > W_CN.
*        EXIT.
*      ENDIF.
*    ENDDO.
*  ENDLOOP.

  LOOP AT it_input_osr.
    IF it_input_osr-rs18 = 'B'.
      MOVE-CORRESPONDING it_input_osr TO lt_plan_b.
      lt_plan_b-date_soff = it_input_osr-seq_date.
      CONCATENATE it_input_osr-serial it_input_osr-seq_serial
             INTO lt_plan_b-zser_no.
      APPEND lt_plan_b.
    ELSE.
      MOVE-CORRESPONDING it_input_osr TO it_plan.
      CONCATENATE it_plan-serial it_plan-seq_serial
             INTO it_plan-zser_no.
      APPEND it_plan.
    ENDIF.
  ENDLOOP.

  SORT it_plan BY serial seq_no.
  DESCRIBE TABLE it_plan LINES w_cn.
  l_index = 1.
  LOOP AT it_capacity.
    g_tcap = it_capacity-cap.
    DO g_tcap TIMES.
      READ TABLE it_plan INDEX l_index.
      it_plan-date_soff = it_capacity-date.
      MODIFY it_plan INDEX l_index TRANSPORTING date_soff.
      l_index = l_index + 1.
      IF l_index > w_cn.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.

  APPEND LINES OF lt_plan_b TO it_plan.
  SORT it_plan BY serial seq_no.
** Assign ZVIN

  SELECT * INTO TABLE it_um
  FROM ztsd_um
  WHERE status = ' '
*   AND BODY_NO <> ' '
** Furong on 05/21/12 sap tuning
%_HINTS ORACLE 'RULE'.
** End

  LOOP AT it_um.
    MOVE-CORRESPONDING it_um TO lt_um.
    IF lt_um-urgcdate IS INITIAL.
      lt_um-urgcdate = '12319999'.
    ENDIF.
    IF lt_um-dealer_dt IS INITIAL.
      lt_um-dealer_dt = '12319999'.
    ENDIF.
    APPEND lt_um.

*-< added on 05.21.2014 Victor
    IF lt_um-wo_nation = 'B28'.
      lt_b28 = lt_um.
      APPEND lt_b28.
      CLEAR: lt_b28.
    ELSE.
      lt_oth_natn = lt_um.
      APPEND lt_oth_natn.
      CLEAR: lt_oth_natn.
    ENDIF.
*->
  ENDLOOP.


*  SORT lt_um BY wo_serial wo_nation wo_dealer wo_extc
*                wo_intc plan_date urgcdate dealer_dt zvin.
  SORT lt_b28 BY wo_serial wo_nation wo_dealer wo_extc
                 wo_intc   zvin.
  SORT lt_oth_natn BY wo_serial wo_nation wo_dealer wo_extc
                      wo_intc   plan_date urgcdate dealer_dt zvin.

*-Read UM data differently per Nation
  CLEAR: g_index.
  LOOP AT it_plan.
    g_index = sy-tabix.

    l_wo = it_plan-work_order+0(9).
    l_nation  = it_plan-work_order+9(3).
    l_dealer = it_plan-work_order+12(2).

    IF l_nation = 'B28'.
      IF it_plan-body_ser IS INITIAL.
        READ TABLE lt_b28 WITH KEY wo_serial = l_wo
                                  wo_nation = l_nation
                                  wo_dealer = l_dealer
                                  wo_extc = it_plan-extc
                                  wo_intc = it_plan-intc
                                  BINARY SEARCH.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          LOOP AT lt_b28 FROM l_index WHERE wo_serial = l_wo
                                        AND wo_nation = l_nation
                                        AND wo_dealer = l_dealer
                                        AND wo_extc = it_plan-extc
                                        AND wo_intc = it_plan-intc.
            l_index = sy-tabix.
            IF lt_b28-assign IS INITIAL
                              AND lt_b28-body_no IS INITIAL.
              it_plan-zvin  = lt_b28-zvin.
              lt_b28-assign = 'X'.

              MODIFY lt_b28 TRANSPORTING assign.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF sy-subrc <> 0.
*        MESSAGE S002 WITH 'No Vin found ' IT_PLAN-WORK_ORDER.
        ENDIF.
      ELSE.
        READ TABLE lt_b28 WITH KEY  model_code = it_plan-modl
                                    body_no = it_plan-body_ser.
        l_index = sy-tabix.
        IF sy-subrc = 0 AND lt_b28-assign IS INITIAL.
          it_plan-zvin  = lt_b28-zvin.
          lt_b28-assign = 'X'.

          MODIFY lt_b28 INDEX l_index TRANSPORTING assign.
*      ELSE.
*        MESSAGE S002 WITH 'No Vin found ' IT_PLAN-BODY_SER.
        ENDIF.
      ENDIF.

    ELSE.
      IF it_plan-body_ser IS INITIAL.
        READ TABLE lt_oth_natn WITH KEY wo_serial = l_wo
                                        wo_nation = l_nation
                                        wo_dealer = l_dealer
                                        wo_extc = it_plan-extc
                                        wo_intc = it_plan-intc
                                        BINARY SEARCH.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          LOOP AT lt_oth_natn FROM l_index WHERE wo_serial = l_wo
                                        AND wo_nation = l_nation
                                        AND wo_dealer = l_dealer
                                        AND wo_extc = it_plan-extc
                                        AND wo_intc = it_plan-intc.
            l_index = sy-tabix.
            IF lt_oth_natn-assign IS INITIAL
                              AND lt_oth_natn-body_no IS INITIAL.
              it_plan-zvin       = lt_oth_natn-zvin.
              lt_oth_natn-assign = 'X'.

              MODIFY lt_oth_natn TRANSPORTING assign.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF sy-subrc <> 0.
*        MESSAGE S002 WITH 'No Vin found ' IT_PLAN-WORK_ORDER.
        ENDIF.
      ELSE.
        READ TABLE lt_oth_natn WITH KEY  model_code = it_plan-modl
                                         body_no = it_plan-body_ser.
        l_index = sy-tabix.
        IF sy-subrc = 0 AND lt_oth_natn-assign IS INITIAL.
          it_plan-zvin       = lt_oth_natn-zvin.
          lt_oth_natn-assign = 'X'.

          MODIFY lt_oth_natn INDEX l_index TRANSPORTING assign.
*      ELSE.
*        MESSAGE S002 WITH 'No Vin found ' IT_PLAN-BODY_SER.
        ENDIF.
      ENDIF.

    ENDIF.

    it_plan-erdat = w_date.
    it_plan-erzet = w_time.
    it_plan-ernam = sy-uname.
    MODIFY it_plan INDEX g_index TRANSPORTING zvin erdat erzet ernam.
    CLEAR: it_plan, lt_b28, lt_oth_natn.

  ENDLOOP.


ENDFORM.                    " MOD_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .

  DATA: it_plan_h LIKE TABLE OF ztpp_soff_plan_h WITH HEADER LINE.

  DATA: l_lpdd_d LIKE ztsd_osr-lpdd_d,
        l_lpdd LIKE ztsd_osr-lpdd.

  DELETE FROM ztpp_soff_plan CLIENT SPECIFIED
        WHERE modl <> ' '.
  INSERT ztpp_soff_plan FROM TABLE it_plan ACCEPTING DUPLICATE KEYS.

** ON 04/19/12 do not need to keep log
  COMMIT WORK.

*  LOOP AT IT_PLAN.
*    MOVE-CORRESPONDING IT_PLAN TO IT_PLAN_H.
*    CONCATENATE IT_PLAN-ERDAT IT_PLAN-ERZET INTO IT_PLAN_H-INS_TIME.
*    APPEND IT_PLAN_H.
*  ENDLOOP.

*  INSERT ZTPP_SOFF_PLAN_H FROM TABLE IT_PLAN_H ACCEPTING DUPLICATE KEYS
*.
*  COMMIT WORK.

*  updatr signoff date and set indicator for changes
  LOOP AT it_plan.
    SELECT SINGLE lpdd lpdd_d INTO (l_lpdd, l_lpdd_d)
    FROM ztsd_osr
    WHERE hkmc   = 'HMG'
      AND nation = it_plan-work_order+9(3)
*      AND dealer = it_plan-work_order+12(2)
      AND zvin   = it_plan-zvin.
    IF sy-subrc = 0.
      IF l_lpdd_d = 'X'.
      ELSE.
        IF l_lpdd = it_plan-date_soff.
          l_lpdd_d = ' '.
        ELSE.
          l_lpdd_d = 'X'.
        ENDIF.
      ENDIF.
      UPDATE ztsd_osr SET: lpdd_d = l_lpdd_d
                           lpdd = it_plan-date_soff
      WHERE hkmc   = 'HMG'
        AND nation = it_plan-work_order+9(3)
*        AND dealer = it_plan-work_order+12(2)
        AND zvin   = it_plan-zvin.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.
** End on 04/19/12

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  CALL SCREEN 200.
ENDFORM.                    " DISPLAY_DATA

INCLUDE zrsd_osr_signoff_alv.
*&---------------------------------------------------------------------*
*&      Form  GET_FROM_ZSIGNOFF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_from_ztable.
  SELECT * INTO TABLE it_plan
   FROM ztpp_soff_plan
   WHERE work_order IN s_worder.

ENDFORM.                    " GET_FROM_ZSIGNOFF
*&---------------------------------------------------------------------*
*&      Form  GET_EST_SOFF_FROM_DELAY_VEH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_est_soff_from_delay_veh .
  DATA: lt_delay LIKE ztpp_delay_veh OCCURS 0 WITH HEADER LINE.

  DATA: l_datum LIKE sy-datum.

  CHECK it_plan[] IS NOT INITIAL.

  SELECT SINGLE MAX( datum ) INTO l_datum
    FROM ztpp_delay_veh.

  CHECK l_datum IS NOT INITIAL.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_delay
    FROM ztpp_delay_veh
     FOR ALL ENTRIES IN it_plan
   WHERE datum = l_datum
     AND model_code =  it_plan-modl
     AND body_no    =  it_plan-body_ser
     AND zesoff     >= '19000101'.

  SORT it_plan BY modl body_ser.

  LOOP AT lt_delay.
    READ TABLE it_plan WITH KEY modl     = lt_delay-model_code
                                body_ser = lt_delay-body_no
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_plan-date_soff = lt_delay-zesoff.
      MODIFY it_plan INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_EST_SOFF_FROM_DELAY_VEH
