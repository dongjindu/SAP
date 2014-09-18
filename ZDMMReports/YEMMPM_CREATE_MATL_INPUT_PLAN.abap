************************************************************************
* Program Name      : ZEMMPM_CREATE_PLAN_REQUIREMENT
* Author            : Byung Sung Bae
* Creation Date     : 2005.03.01.
* Specifications By : Byung Sung Bae
* Development Request No : UD1K914654
* Addl Documentation:
* Description       : Create Material Requirement for Monthly
* Modification Logs
* Date            Developer        RequestNo      Description
*
*
************************************************************************
REPORT zemmpm_create_plan_requirement.
TABLES: mara, marc.

DATA: it_vin       LIKE zspp_vin_info_for_nstl   OCCURS 0
                                                 WITH HEADER LINE.

DATA: it_dvrt      LIKE ztmm_dvrt_866 OCCURS 0 WITH HEADER LINE.

*DATA: it_parts     LIKE ztmm_parts_sch OCCURS 0 WITH HEADER LINE.

DATA: it_3days     LIKE ztmm_parts_3days OCCURS 0 WITH HEADER LINE.

DATA: it_21days    LIKE ztmm_parts_21day OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
        lifnr   LIKE   lfa1-lifnr,
        profl   LIKE   mara-profl,
        tempb   LIKE   mara-tempb,
        vspvb   LIKE   marc-vspvb,
        arbpl   LIKE   crhd-arbpl,
      END   OF it_matnr.

DATA: BEGIN OF it_master  OCCURS 0,
        arbpl             LIKE crhd-arbpl,      " Shop
        seq               TYPE i  ,             " Sequence
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working
        uph               TYPE zvpp_ld-lrate,   " UPH
        tqty              TYPE i  ,             " Day's Total Veh.
      END OF it_master.

DATA: BEGIN OF it_data       OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_input_plan.
DATA: END OF it_data.

DATA: it_mitu LIKE it_data OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_shift   OCCURS 0,
        arbpl             LIKE crhd-arbpl,      " Shop
        ser               TYPE i  ,             " Serial
        seq               TYPE n  ,             " Sequence
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working(NET)
        total             TYPE kapendzt  ,      " Times for working(GRS)
        ftime             TYPE t  ,             " Start Time.
        uph               TYPE zvpp_ld-lrate,   " UPH
        tqty              TYPE i,               " Shift's Total Veh.
        hqty              TYPE i  ,             " Hour's Total Veh.
        hloop             TYPE i  ,             " Hour's LOOP.
      END OF it_shift .

DATA: BEGIN OF it_worktime OCCURS 0,
        arbpl   LIKE   crhd-arbpl.
        INCLUDE STRUCTURE zsmm_working_time.
DATA: END   OF it_worktime.

DATA: BEGIN OF it_wbs OCCURS 0,
        model       LIKE ztpp_input_plan-modl,
        body_serial LIKE ztpp_input_plan-body_ser,
      END   OF it_wbs.

DATA: it_prj LIKE it_wbs OCCURS 0 WITH HEADER LINE.

*---// Work area : Global variables & Structures
DATA : BEGIN OF w_order OCCURS 0,
         datum LIKE ztmm_dvrt-datum,
         matnr LIKE mara-matnr,
         uzeit LIKE ztmm_dvrt-uzeit,
         bdmng LIKE resb-bdmng,
         meins LIKE resb-meins,
         tprog LIKE ztmm_dvrt-tprog,
       END OF w_order.

DATA: BEGIN OF w_requirement,
        matnr     LIKE mara-matnr,
        bdter     LIKE resb-bdter,
        rp01      LIKE ztmm_dvrt_866-rp01,
        rp02      LIKE ztmm_dvrt_866-rp02,
        rp06      LIKE ztmm_dvrt_866-rp06,
        bdmng     LIKE resb-bdmng,
        meins     LIKE resb-meins,
        mitu      LIKE ztmm_dvrt_866-mitu,
        werks     LIKE marc-werks,
        tprog     LIKE ztmm_dvrt_866-tprog,
        wkord     LIKE ztmm_dvrt_866-wkord,
        model     LIKE ztmm_dvrt_866-p_model,
        body_serial LIKE ztmm_dvrt_866-p_body_serial,
      END   OF w_requirement.

DATA:
*w_parts     LIKE it_parts,
      wa_kalid    LIKE kako-kalid,
      wa_uph_b    TYPE zvpp_ld-lrate,
      wa_uph_p    TYPE zvpp_ld-lrate,
      wa_uph_t    TYPE zvpp_ld-lrate,
      wa_hour     TYPE i.

DATA: c_factor(13)   TYPE p DECIMALS 3 VALUE 1.

DATA: w_datum_f     LIKE   sy-datum,
      w_datum_t     LIKE   sy-datum,
      w_uzeit       LIKE   sy-uzeit,
      w_body_uph    TYPE zvpp_ld-lrate,
      w_paint_uph   TYPE zvpp_ld-lrate,
      w_trim_uph    TYPE zvpp_ld-lrate.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001'.
*             p_day   TYPE i           OBLIGATORY DEFAULT 21.
*SELECT-OPTIONS : s_matnr FOR mara-matnr NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-t03.
PARAMETERS :
*p_extest AS CHECKBOX,
             p_wbs(2) TYPE n OBLIGATORY,
             p_prj(2) TYPE n OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl3.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02.
PARAMETERS : p_vinupt AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK bl2.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.
  PERFORM update_table.

*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
    IF screen-name = 'P_WERKS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  MOVE: sy-datum TO w_datum_f,
        sy-uzeit TO w_uzeit.
*  w_datum_f = '20050326'.
*  w_datum_t = w_datum_f + p_day - 1.

*  SELECT SINGLE * FROM marc WHERE werks =  p_werks
*                              AND matnr IN s_matnr.
*  IF sy-subrc NE 0.
*    MESSAGE e000(zz) WITH text-m02 p_werks.
*  ENDIF.
*
*  CLEAR: s_matnr.
*  READ TABLE s_matnr INDEX 1.
*
*  IF     s_matnr-low EQ ' ' AND s_matnr-high EQ ' '.
*    w_matnr_t = 'ZZZZZZZZZZZZZZZZZ'.
*  ELSEIF s_matnr-low EQ ' ' AND s_matnr-high NE ' '.
*    w_matnr_t = s_matnr-high.
*  ELSEIF s_matnr-low NE ' ' AND s_matnr-high EQ ' '.
*    w_matnr_f = w_matnr_t = s_matnr-low.
*  ELSEIF s_matnr-low NE ' ' AND s_matnr-high NE ' '.
*    w_matnr_f = s_matnr-low. w_matnr_t = s_matnr-high.
*  ENDIF.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_uph_working_time.
  PERFORM set_dvrt_866.
  PERFORM create_dvrt_866.
  PERFORM get_wbs_prj_vehicle.
  PERFORM get_material_master.
  PERFORM get_material_requirement.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  APPEND_MATL_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_matl_master.
  IF it_matnr-lifnr EQ space.
    PERFORM read_vendor_from_sa.
  ENDIF.

  IF it_matnr-lifnr EQ space.
    PERFORM read_vendor_from_info.
  ENDIF.

  APPEND: it_matnr.

  CLEAR: it_matnr.
ENDFORM.                    " APPEND_MATL_MASTER
*&---------------------------------------------------------------------*
*&      Form  create_dvrt_866
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_dvrt_866.
  DATA: lw_datum LIKE sy-datum.

  lw_datum = w_datum_f - 30.

  DELETE FROM ztmm_dvrt_866 WHERE erdat <= lw_datum.

  CHECK p_vinupt EQ 'X'.

  DELETE FROM ztmm_dvrt_866 WHERE erdat = w_datum_f.

  INSERT ztmm_dvrt_866 FROM TABLE it_dvrt ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m04.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.                    " create_dvrt_866
*&---------------------------------------------------------------------*
*&      Form  get_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_master.
  EXEC SQL PERFORMING APPEND_MATL_MASTER.
    SELECT V.MATNR, Y.LIFNR, V.PROFL, V.TEMPB, W.VSPVB, X.ARBPL
      INTO :IT_MATNR
      FROM MARA V, MARC W, ZVMM_TASK_LIST X, EORD Y
     WHERE V.MANDT =  :SY-MANDT
       AND V.MTART =  'ROH'
       AND W.MANDT =  V.MANDT
       AND W.MATNR =  V.MATNR
       AND W.WERKS =  :P_WERKS
       AND X.MANDT(+) =  W.MANDT
       AND X.PLNNR(+) =  'RP'
       AND X.USR00(+) =  W.VSPVB
       AND Y.MANDT(+) =  W.MANDT
       AND Y.WERKS(+) =  W.WERKS
       AND Y.MATNR(+) =  W.MATNR
       AND Y.VDATU(+) <= :W_DATUM_F
       AND Y.BDATU(+) >= :W_DATUM_F
  ENDEXEC.

  SORT it_matnr BY matnr.
ENDFORM.                    " get_material_master
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  PERFORM update_db_table.

*  DATA: lw_mod TYPE i.
*  DATA: lt_parts LIKE it_parts OCCURS 0 WITH HEADER LINE.
*
*  DELETE FROM ztmm_parts_sch WHERE bdter >= '00000000'.
*
*  LOOP AT it_parts.
*    lw_mod = sy-tabix MOD 1000.
*    MOVE: it_parts TO lt_parts.
*    APPEND lt_parts.
*
*    IF lw_mod EQ 0.
*     INSERT ztmm_parts_sch FROM TABLE lt_parts ACCEPTING DUPLICATE KEYS
  .
*      IF sy-subrc NE 0.
*        ROLLBACK WORK.
*        MESSAGE e000(zz) WITH text-m09.
*      ENDIF.
*
*      CLEAR: lt_parts, lt_parts[].
*    ENDIF.
*  ENDLOOP.
*
*  INSERT ztmm_parts_sch FROM TABLE lt_parts ACCEPTING DUPLICATE KEYS.
*  IF sy-subrc NE 0.
*    ROLLBACK WORK.
*    MESSAGE e000(zz) WITH text-m09.
*  ENDIF.
*
*  COMMIT WORK AND WAIT.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  update_db_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_db_table.
  DELETE FROM ztmm_parts_3days WHERE werks = p_werks.

  INSERT ztmm_parts_3days FROM TABLE it_3days  ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m09.
  ENDIF.

  DELETE FROM ztmm_parts_21day WHERE werks = p_werks.

  INSERT ztmm_parts_21day FROM TABLE it_21days ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m09.
  ENDIF.
ENDFORM.                    " update_db_table
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  DATA: l_vals                LIKE ztpp_common_vals-item4.

  SELECT SINGLE item4  INTO l_vals
    FROM ztpp_common_vals
   WHERE jobs = 'ZAPP903R_INPUT_PLAN'
     AND key2 = 'WBS'   .
  IF sy-subrc EQ 0.
    p_wbs = l_vals        .   CLEAR: l_vals.
  ELSE.
    MOVE: 4 TO p_wbs.
  ENDIF.

  IF p_prj IS INITIAL.
    SELECT SINGLE item4  INTO l_vals
      FROM ztpp_common_vals
     WHERE jobs = 'ZAPP903R_INPUT_PLAN'
       AND key2 = 'PRJ'   .
    IF sy-subrc EQ 0.
      p_prj = l_vals        .   CLEAR: l_vals.
    ELSE.
      MOVE: 20 TO p_prj.
    ENDIF.
  ENDIF.
ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  get_wbs_prj_vehicle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wbs_prj_vehicle.
  DATA: lw_count TYPE i.

  DATA: lt_input_plan LIKE ztpp_input_plan OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE lt_input_plan
    FROM ztpp_input_plan
   WHERE status = '01'.

  lw_count = p_wbs * w_body_uph.

  SORT lt_input_plan BY serial DESCENDING.
  LOOP AT lt_input_plan.
    IF sy-tabix <= lw_count.
      DELETE lt_input_plan.
    ELSE.
      MOVE: lt_input_plan-modl     TO it_wbs-model,
            lt_input_plan-body_ser TO it_wbs-body_serial.
      APPEND it_wbs.
    ENDIF.
  ENDLOOP.
  SORT it_wbs BY model body_serial.

  SELECT * INTO TABLE lt_input_plan
    FROM ztpp_input_plan
   WHERE status = '02'.

  lw_count = p_prj * w_paint_uph.

  SORT lt_input_plan BY serial DESCENDING.
  LOOP AT lt_input_plan.
    IF sy-tabix <= lw_count.
      DELETE lt_input_plan.
    ELSE.
      MOVE: lt_input_plan-modl     TO it_prj-model,
            lt_input_plan-body_ser TO it_prj-body_serial.
      APPEND it_prj.
    ENDIF.
  ENDLOOP.
  SORT it_prj BY model body_serial.
ENDFORM.                    " get_wbs_prj_vehicle
*&---------------------------------------------------------------------*
*&      Form  get_uph_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_uph_working_time.
*---// This logic is copyed from Input Plan(Pgm:ZAPP903R_SEQ_SUM01)
  DATA: l_date   LIKE sy-datum,
        l_chk    TYPE p DECIMALS 3,
        l_tot    TYPE p DECIMALS 3,
        l_htot   TYPE p DECIMALS 3,
        l_count  TYPE i .

  " Set the BASIC Information for the UPH & Work Time...
  " Gather the 3 day's UPH Except shift...
  CLEAR: l_date, l_count.
  l_date = w_datum_f - 1 .

  DO 3 TIMES.
    l_date   = l_date  + 1.
    l_count  = l_count + 1.

    PERFORM append_it_master USING l_date 'B' l_count.
    PERFORM append_it_master USING l_date 'P' l_count.
    PERFORM append_it_master USING l_date 'T' l_count.
  ENDDO.

  CLEAR: l_date, l_count, wa_hour.
  l_date = w_datum_f - 1  .
  DO 3 TIMES   .
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    wa_hour  = sy-index - 1.
    PERFORM append_it_shift USING l_date 'B' l_count wa_hour.
    wa_hour  = sy-index - 1.
    PERFORM append_it_shift USING l_date 'P' l_count wa_hour.
    wa_hour  = sy-index - 1.
    PERFORM append_it_shift USING l_date 'T' l_count wa_hour.
  ENDDO.

  DATA: lw_ser TYPE i.
  SORT it_shift BY arbpl seq shift.
  LOOP AT it_shift.
    AT NEW arbpl.
      CLEAR: lw_ser.
    ENDAT.

    lw_ser = lw_ser + 1.

    MOVE: lw_ser TO it_shift-ser.
    MODIFY it_shift.
  ENDLOOP.

  " Set the Day's Total Quantity(IT_MASTER, IT_SHIFT)
  SORT it_shift BY arbpl ser seq shift .
  LOOP AT it_master.
    CLEAR: l_chk, l_tot, l_htot  .
    LOOP AT it_shift WHERE arbpl = it_master-arbpl
                       AND seq   = it_master-seq .
      l_tot = l_tot + it_shift-tqty            .
      l_chk = it_master-tqty - l_tot           .
      IF l_chk < 0                             .
        it_shift-tqty = it_shift-tqty + l_chk  .
        MODIFY it_shift                        .
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " get_uph_working_time
*&---------------------------------------------------------------------*
*&      Form  read_working_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2032   text
*      -->P_WA_KALID  text
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
ENDFORM.                    " get_day
*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_TIME  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM get_working_time USING    pa_wdate  pa_wktime  pa_day pw_shop.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = pw_shop
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
ENDFORM.                    " get_working_time
*&---------------------------------------------------------------------*
*&      Form  get_uph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_UPH  text
*      -->P_IT_MASTER_SHIFT  text
*----------------------------------------------------------------------*
FORM get_uph USING    pa_wdate  pa_uph  pa_shift pw_shop.
  DATA lw_ld          LIKE zvpp_ld .

  IF pa_shift IS INITIAL .
    SELECT SINGLE * INTO lw_ld
      FROM zvpp_ld
     WHERE ld_perst <= pa_wdate
       AND ld_pered >= pa_wdate
       AND arbpl     = pw_shop.
  ELSE.
    SELECT SINGLE * INTO lw_ld
      FROM zvpp_ld
     WHERE ld_perst <= pa_wdate
       AND ld_pered >= pa_wdate
       AND ld_shift  = pa_shift
       AND arbpl     = pw_shop.
  ENDIF.

  IF lw_ld-lantu = 0.
    pa_uph = 0 .
  ELSE.
    pa_uph = lw_ld-lrate / lw_ld-lantu .
  ENDIF.

  CASE pw_shop.
    WHEN 'B'.
      MOVE: pa_uph TO w_body_uph.
    WHEN 'P'.
      MOVE: pa_uph TO w_paint_uph.
    WHEN 'T'.
      MOVE: pa_uph TO w_trim_uph.
  ENDCASE.
ENDFORM.                    " get_uph
*&---------------------------------------------------------------------*
*&      Form  get_uph_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_WA_UPH_T  text
*      -->P_2061   text
*----------------------------------------------------------------------*
FORM get_uph_shop USING    pa_wdate  pa_uph  pa_arbpl .
  DATA lw_ld          LIKE zvpp_ld .

  SELECT SINGLE * INTO lw_ld
    FROM zvpp_ld
   WHERE ld_perst <= pa_wdate
     AND ld_pered >= pa_wdate
     AND arbpl     = pa_arbpl .

  IF lw_ld-lantu = 0.
    pa_uph = 0 .
  ELSE.
    pa_uph = lw_ld-lrate / lw_ld-lantu .
  ENDIF.
ENDFORM.                    " get_uph_shop
*&---------------------------------------------------------------------*
*&      Form  get_worktime1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_L_COUNT  text
*      -->P_IT_SHIFT_TIME  text
*      -->P_IT_SHIFT_DAY  text
*----------------------------------------------------------------------*
FORM get_worktime1 USING pa_wdate pa_cnt pa_wktime pa_day
                         pw_shop pw_hour.

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
   WHERE arbpl =  pw_shop
     AND datub >= pa_wdate .

  SORT lt_capa BY datub tagnr schnr .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    pw_hour = pw_hour + 1 .
    CLEAR: l_einzt.
    SELECT SINGLE * INTO l_tc37a       " l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    it_shift-ser   = pw_hour       .
    it_shift-time  = l_tc37a-einzt .
    IF l_tc37a-begzt >= l_tc37a-endzt.
      it_shift-total = l_tc37a-endzt +   l_tc37a-begzt            .
    ELSE.
      it_shift-total = l_tc37a-endzt -   l_tc37a-begzt            .
    ENDIF.
    it_shift-arbpl = pw_shop.
    it_shift-ftime = l_tc37a-begzt .
    it_shift-shift = lt_capa-schnr .
    l_chk = it_shift-time / 3600 .
    it_shift-tqty  =  it_shift-uph * l_chk  .
    l_chk = ceil( it_shift-time / 7200 ) .
    it_shift-hqty  = ceil( it_shift-tqty / l_chk ) .
    it_shift-hloop = ceil( it_shift-time / 7200 )  .
    APPEND it_shift.
  ENDLOOP.
ENDFORM.                    " get_worktime1
*&---------------------------------------------------------------------*
*&      Form  append_it_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2044   text
*----------------------------------------------------------------------*
FORM append_it_master USING pw_date pw_shop pw_count.
  DATA: l_check              TYPE c           ,
        l_chk                TYPE p DECIMALS 3,
        l_tot                TYPE p DECIMALS 3,
        l_htot               TYPE p DECIMALS 3.

  CLEAR: it_master.

  SELECT SINGLE kalid INTO wa_kalid
    FROM zvpp_capacity
   WHERE arbpl = pw_shop.

  PERFORM read_working_date USING '+'  wa_kalid  pw_date.
  PERFORM get_day          USING pw_date it_master-day  .
  PERFORM get_working_time USING pw_date it_master-time it_master-day
                                 pw_shop.
  PERFORM get_uph          USING pw_date it_master-uph it_master-shift
                                 pw_shop.
  it_master-arbpl  = pw_shop.
  it_master-seq    = pw_count.
  it_master-date   = pw_date .
  l_chk = it_master-time / 3600 .
  it_master-tqty   = ceil( it_master-uph * l_chk )  .
  APPEND it_master.
ENDFORM.                    " append_it_master
*&---------------------------------------------------------------------*
*&      Form  append_it_shift
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_2066   text
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
FORM append_it_shift USING pw_date pw_shop pw_count pw_hour.
  SELECT SINGLE kalid INTO wa_kalid
    FROM zvpp_capacity
   WHERE arbpl = pw_shop.

  PERFORM read_working_date USING '+'  wa_kalid  pw_date .
  PERFORM get_day       USING pw_date it_shift-day  .
  PERFORM get_uph       USING pw_date it_shift-uph it_shift-shift
                              pw_shop.
  it_shift-seq    = pw_count .   it_shift-date   = pw_date .
  PERFORM get_worktime1 USING pw_date       pw_count
                              it_shift-time it_shift-day
                              pw_shop       pw_hour.
  CLEAR: it_shift.
ENDFORM.                    " append_it_shift
*&---------------------------------------------------------------------*
*&      Form  get_input_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_input_plan USING pw_rp.
  CLEAR: it_data, it_mitu.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
   FROM ztpp_input_plan
  WHERE status <= pw_rp
*    AND plnum  >  space.
    AND plnum BETWEEN '0000255487' and '0000255682'.

  LOOP AT it_data.
    CASE pw_rp.
      WHEN 'B'.

      WHEN 'P'.
        IF it_data-work_order+12(2) EQ 'XX'.
          DELETE it_data. CONTINUE.
        ENDIF.
      WHEN 'T'.
        IF it_data-work_order+12(2) EQ 'XX' OR
           it_data-work_order+12(2) EQ 'XY'.
          DELETE it_data. CONTINUE.
        ENDIF.
    ENDCASE.

    IF it_data-mitu = 'Y'.
      MOVE: it_data TO it_mitu.
      APPEND it_mitu.
      DELETE it_data.
    ENDIF.
  ENDLOOP.

  SORT it_data BY serial.
ENDFORM.                    " get_input_plan
*&---------------------------------------------------------------------*
*&      Form  set_dvrt_866
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_dvrt_866.
  PERFORM set_dvrt_866_per_shop USING '06' 'T'.
  PERFORM set_dvrt_866_per_shop USING '02' 'P'.
  PERFORM set_dvrt_866_per_shop USING '01' 'B'.
ENDFORM.                    " set_dvrt_866
*&---------------------------------------------------------------------*
*&      Form  set_dvrt_866_per_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2811   text
*----------------------------------------------------------------------*
FORM set_dvrt_866_per_shop USING pw_rp pw_shop.
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
  DATA: l_rq                 TYPE i,
        l_lpcnt              TYPE i,
        l_ftime              TYPE t,
        l_fdate              TYPE d.

  DATA: lw_rp_date(50).
  FIELD-SYMBOLS: <rp_date>.

  PERFORM get_input_plan USING pw_rp.

  l_index = 1.

  " 3 Days Data...
  CLEAR: l_pos.   SORT it_shift BY arbpl ser seq shift.
  LOOP AT it_master WHERE arbpl = pw_shop.
    l_hours = 20 * it_master-seq - 20 .
    CLEAR: l_ftime.
    LOOP AT it_shift WHERE arbpl = it_master-arbpl
                       AND seq   = it_master-seq .
      IF it_shift-shift NE l_shift.
        l_hours = 20 * it_master-seq - 10.
        l_shift = it_shift-shift .
      ENDIF.

      l_ftime = it_shift-ftime - 7200.

      CLEAR: l_cnt,
             l_lpcnt.                                       "UD1K914561
      l_rq      = it_shift-tqty.                            "UD1K914561
      l_lpcnt  = it_shift-hloop.                            "UD1K914561
      DO it_shift-hloop TIMES.
        it_shift-hqty  = ceil( l_rq * c_factor
                               / l_lpcnt ) .                "UD1K914561
        l_rq    = l_rq - it_shift-hqty.                     "UD1K914561
        l_lpcnt = l_lpcnt - 1.                              "UD1K914561

        l_cnt = l_cnt + it_shift-hqty            .
        l_pos = it_shift-hqty + l_index - 1 .
        l_hours = l_hours + 2 .
        l_ftime = l_ftime + 7200.

        CONCATENATE 'IT_DVRT-RP' pw_rp INTO lw_rp_date.
        ASSIGN (lw_rp_date) TO <rp_date>.

        LOOP AT it_data FROM l_index TO l_pos.
          READ TABLE it_dvrt WITH KEY erdat         = w_datum_f
                                      p_model       = it_data-modl
                                      p_body_serial = it_data-body_ser.
          IF sy-subrc EQ 0.
            IF l_ftime < '020000' AND l_hours NE 2.
              l_fdate = it_shift-date + 1.
            ELSE.
              l_fdate = it_shift-date.
            ENDIF.

            CONCATENATE l_fdate l_ftime INTO <rp_date>.

            MODIFY it_dvrt INDEX sy-tabix.
          ELSE.
            CLEAR: it_dvrt.
            MOVE: w_datum_f          TO it_dvrt-erdat,
                  it_data-plnum      TO it_dvrt-plnum,
                  it_data-modl       TO it_dvrt-p_model,
                  it_data-body_ser   TO it_dvrt-p_body_serial,
                  it_data-mitu       TO it_dvrt-mitu,
                  it_shift-shift     TO it_dvrt-tprog,
                  it_data-work_order TO it_dvrt-wkord,
                  it_data-status     TO it_dvrt-rpsts,
                  sy-uzeit           TO it_dvrt-uzeit,
                  sy-uname           TO it_dvrt-ernam.

            IF l_ftime < '020000' AND l_hours NE 2.
              l_fdate = it_shift-date + 1.
            ELSE.
              l_fdate = it_shift-date.
            ENDIF.

            CONCATENATE l_fdate l_ftime INTO <rp_date>.

            APPEND it_dvrt.
          ENDIF.
        ENDLOOP.
        l_index = l_pos + 1 .
      ENDDO.
    ENDLOOP.
  ENDLOOP.

  LOOP AT it_mitu.
    MOVE: w_datum_f          TO it_dvrt-erdat,
          it_mitu-plnum      TO it_dvrt-plnum,
          it_mitu-modl       TO it_dvrt-p_model,
          it_mitu-body_ser   TO it_dvrt-p_body_serial,
          it_mitu-mitu       TO it_dvrt-mitu,
          '9'                TO it_dvrt-tprog,
          it_data-work_order TO it_dvrt-wkord,
          it_data-status     TO it_dvrt-rpsts,
          sy-uzeit           TO it_dvrt-uzeit,
          sy-uname           TO it_dvrt-ernam.
    APPEND it_dvrt.
  ENDLOOP.
ENDFORM.                    " set_dvrt_866_per_shop
*&---------------------------------------------------------------------*
*&      Form  get_material_requirement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_requirement.
  EXEC SQL PERFORMING APPEND_MATERIAL_REQUIREMENT.
    SELECT /*+ ORDERED+*/
           C.MATNR, '00000000', A.RP01, A.RP02,  A.RP06,
           C.BDMNG, C.MEINS,    A.MITU, C.WERKS, A.TPROG,
           A.WKORD, A.MODEL,    A.BODY_SERIAL
      INTO :W_REQUIREMENT
      FROM ZTMM_DVRT_866 A, PLAF B, RESB C
     WHERE A.MANDT =  :SY-MANDT
       AND A.ERDAT =  :W_DATUM_F
       AND B.MANDT =  A.MANDT
       AND B.PLNUM =  A.PLNUM
       AND C.MANDT =  B.MANDT
       AND C.RSNUM =  B.RSNUM
       AND C.MATNR = '1249205163'
*       AND B.MATNR BETWEEN :W_MATNR_F AND :W_MATNR_T
       AND C.WERKS =  :P_WERKS
       AND C.XLOEK =  ' '
       AND C.KZEAR >= ' '
  ENDEXEC.
ENDFORM.                    " get_material_requirement
*&---------------------------------------------------------------------*
*&      Form  APPEND_MATERIAL_REQUIREMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_material_requirement.
  DATA: lw_index LIKE sy-tabix.

  CLEAR: it_3days.

  READ TABLE it_matnr WITH KEY matnr = w_requirement-matnr
                               BINARY SEARCH.

  IF sy-subrc NE 0.
    CLEAR: w_requirement. EXIT.
  ENDIF.

  READ TABLE it_3days WITH KEY werks = p_werks
                               matnr = w_requirement-matnr.
  IF sy-subrc EQ 0.
    MOVE: sy-tabix TO lw_index.

    CASE w_requirement-mitu.
      WHEN 'Y'.
        it_3days-qtymt = it_3days-qtymt + w_requirement-bdmng.
        PERFORM set_other_quantiry.
      WHEN OTHERS.
        PERFORM set_time_zone.
        PERFORM set_other_quantiry.
    ENDCASE.

    MODIFY it_3days INDEX lw_index.
  ELSE.
    PERFORM set_other_fields.

    CASE w_requirement-mitu.
      WHEN 'Y'.
        it_3days-qtymt = it_3days-qtymt + w_requirement-bdmng.
        PERFORM set_other_quantiry.
      WHEN OTHERS.
        PERFORM set_time_zone.
        PERFORM set_other_quantiry.
    ENDCASE.

    APPEND it_3days.
  ENDIF.
ENDFORM.                    " APPEND_MATERIAL_REQUIREMENT
*&---------------------------------------------------------------------*
*&      Form  set_time_zone
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_time_zone.
  DATA: lw_date_time(14),
        lw_date LIKE sy-datum,
        lw_time LIKE sy-uzeit,
        lw_qty(50),
        lw_zone(2) TYPE n,
        lw_shop.

  FIELD-SYMBOLS <qty>.

  CASE it_matnr-arbpl(1).
    WHEN 'B'.                                     "BODY Shop
      IF w_requirement-rp01 IS INITIAL. EXIT. ENDIF.
      MOVE: w_requirement-rp01 TO lw_date_time,
            it_matnr-arbpl(1)  TO lw_shop.
    WHEN 'P'.                                     "PAINT Shop
      IF w_requirement-rp02 IS INITIAL. EXIT. ENDIF.
      MOVE: w_requirement-rp02 TO lw_date_time,
            it_matnr-arbpl(1)  TO lw_shop.
    WHEN 'T'.                                     "TRIM Shop
      IF w_requirement-rp06 IS INITIAL. EXIT. ENDIF.
      MOVE: w_requirement-rp06 TO lw_date_time,
            it_matnr-arbpl(1)  TO lw_shop.
    WHEN OTHERS.
      IF NOT w_requirement-rp06 IS INITIAL.
        MOVE: w_requirement-rp06 TO lw_date_time,
              'T'                TO lw_shop.
      ENDIF.

      IF NOT w_requirement-rp02 IS INITIAL.
        MOVE: w_requirement-rp02 TO lw_date_time,
              'P'                TO lw_shop.
      ENDIF.

      IF NOT w_requirement-rp01 IS INITIAL.
        MOVE: w_requirement-rp01 TO lw_date_time,
              'B'                TO lw_shop.
      ENDIF.
  ENDCASE.

  MOVE: lw_date_time(8)   TO lw_date,
        lw_date_time+8(6) TO lw_time.

  LOOP AT it_shift WHERE arbpl =  lw_shop
                     AND date  =  lw_date
                     AND ftime <= lw_time.
  ENDLOOP.

  lw_zone = ( ( it_shift-seq - 1 ) * 20 + ( it_shift-shift - 1 ) * 10 +
              ( lw_time - it_shift-ftime ) / 7200 * 2 + 2 ).

  CONCATENATE 'IT_3DAYS-QTY' lw_zone INTO lw_qty.
  ASSIGN (lw_qty) TO <qty>.

  <qty> = <qty> + w_requirement-bdmng.
ENDFORM.                    " set_time_zone
*&---------------------------------------------------------------------*
*&      Form  SET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_other_fields.
  DATA: lw_day(2) TYPE n,
        lw_day_shift(2),
        lw_date(50),
        lw_time(50).

  FIELD-SYMBOLS: <date>, <time>.

  MOVE: w_requirement-matnr    TO it_3days-matnr,
        w_requirement-meins    TO it_3days-meins,
        w_requirement-werks    TO it_3days-werks,
        it_matnr-vspvb         TO it_3days-vspvb,
        it_matnr-arbpl         TO it_3days-arbpl,
        it_matnr-arbpl(1)      TO it_3days-shop,
        it_matnr-lifnr         TO it_3days-lifnr.

  CASE it_matnr-profl.
    WHEN 'V'.
      CASE it_matnr-tempb.
        WHEN '11'.                                  "JIS
          MOVE: 'S' TO it_3days-ptype.
        WHEN OTHERS.                                "JIT
          MOVE: 'T' TO it_3days-ptype.
      ENDCASE.
    WHEN OTHERS.                                    "KD/MIP
      MOVE: it_matnr-profl TO it_3days-ptype.
  ENDCASE.

  LOOP AT it_shift WHERE arbpl = it_3days-arbpl(1).
    MOVE: it_shift-seq TO lw_day.
    CONCATENATE it_shift-seq it_shift-shift INTO lw_day_shift.

    CONCATENATE: 'IT_3DAYS-DAT' lw_day INTO lw_date,
                 'IT_3DAYS-ZET' lw_day_shift INTO lw_time.

    ASSIGN: (lw_date) TO <date>,
            (lw_time) TO <time>.

    MOVE: it_shift-date TO <date>,
          it_shift-ftime TO <time>.
  ENDLOOP.
ENDFORM.                    " SET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*&      Form  read_vendor_form_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vendor_from_sa.
  SELECT SINGLE lifnr INTO it_matnr-lifnr
    FROM ekko AS a INNER JOIN ekpo AS b
                      ON a~mandt EQ b~mandt
                     AND a~ebeln EQ b~ebeln
   WHERE matnr   EQ it_matnr-matnr
     AND werks   EQ p_werks
     AND a~loekz EQ space
     AND b~loekz EQ space
     AND kdatb   <= sy-datum
     AND kdate   >= sy-datum.
ENDFORM.                    " read_vendor_form_SA
*&---------------------------------------------------------------------*
*&      Form  read_vendor_from_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vendor_from_info.
  DATA: lt_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.

  READ TABLE it_master INDEX 1.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_a018
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_matnr-matnr
     AND ekorg =  'PU01'
     AND esokz =  '0'
     AND datab <= it_master-date
     AND datbi >= it_master-date.

  LOOP AT lt_a018.
    SELECT SINGLE a~matnr INTO it_matnr-matnr
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = it_matnr-matnr
       AND a~lifnr = lt_a018-lifnr
       AND a~loekz = ' '
       AND b~werks = ' '
       AND b~ekorg = 'PU01'
       AND b~loekz = ' '.
    IF sy-subrc NE 0.
      DELETE lt_a018.
    ENDIF.
  ENDLOOP.

  SORT lt_a018 BY datab DESCENDING.

  READ TABLE lt_a018 INDEX 1.

  MOVE: lt_a018-lifnr TO it_matnr-lifnr.
ENDFORM.                    " read_vendor_from_info
*&---------------------------------------------------------------------*
*&      Form  SET_WBS_QUANTIRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_other_quantiry.
  READ TABLE it_wbs WITH KEY model       = w_requirement-model
                             body_serial = w_requirement-body_serial
                    BINARY SEARCH.
  IF sy-subrc EQ 0.
    it_3days-wbs = it_3days-wbs + w_requirement-bdmng.
  ENDIF.

  READ TABLE it_prj WITH KEY model       = w_requirement-model
                             body_serial = w_requirement-body_serial
                    BINARY SEARCH.
  IF sy-subrc EQ 0.
    it_3days-prj = it_3days-prj + w_requirement-bdmng.
  ENDIF.
ENDFORM.                    " SET_WBS_QUANTIRY
