*----------------------------------------------------------------------*
***INCLUDE MZPP_APPLICATIONF04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_5290_INPUT_UPH_L  text
*      -->P_4103   text
*      -->P_ST_5290_INPUT_DATE  text
*----------------------------------------------------------------------*
FORM set_uph       .
*        USING pa_uph1  pa_uph2  pa_arbpl  pa_date     "UD1K912931
*              pa_day   pa_shift pa_ftime  pa_ttime.   "UD1K912931
  data: lw_time_s like st_5290_input-ftime,
        lw_time like st_5290_input-btime.

  DATA: l_kalid     LIKE kako-kalid   ,
        lw_ld       LIKE zvpp_ld      ,
        l_uph       TYPE p DECIMALS 3 ,
        l_chk       TYPE p DECIMALS 3,
** furong on 06/12/12 for 3 shift
        l_uph_3       TYPE p DECIMALS 3 ,
** End on 06/12/12
        l_date      TYPE d            .
  DATA: BEGIN OF lt_master  ,
          seq               TYPE i  ,             " Sequence
          date              TYPE d  ,             " Date
          day               LIKE kapa-tagnr,      " Day
          shift             LIKE kapa-schnr,      " Shift
          time    TYPE kapendzt  ,      " Times for working
          time_2  TYPE kapendzt  ,     " time for second shift
** Furong on 06/12/12 for 3 shift
          time_3  TYPE kapendzt  ,     " time for second shift
** End
          uph     TYPE zvpp_ld-lrate,   " UPH
        END OF lt_master.
  DATA: la_date LIKE sy-datum,                              "UD1K912931
        l_day_veh TYPE i,                                   "UD1K912931
        l_shift LIKE ausp-atwrt,                            "UD1K912931
        wa_shift LIKE ausp-atwrt,                           "UD1K912931
        l_arbpl LIKE crhd-arbpl,                            "UD1K912931
        l_ftime TYPE kapendzt,                              "UD1K912931
        l_ttime TYPE kapendzt,                              "UD1K912931
        l_btime LIKE sy-uzeit,                              "UD1K912931
        l_etime LIKE sy-uzeit.                              "UD1K912931

  data: l_objid like crhd-objid.

  CLEAR: st_5290_input-dayu,
         st_5290_input-uph_l,
         st_5290_input-uph_h,
         st_5290_input-uph_t,
         st_5290_input-ftime,
         st_5290_input-ttime,
         st_5290_input-btime,
         st_5290_input-etime,
         st_5290_input-ftime_2,
         st_5290_input-ttime_2,
         st_5290_input-btime_2,
         st_5290_input-etime_2,
         it_break, it_break[].

  la_date = st_5290_input-date.                             "UD1K912931
  l_shift  = st_5290_input-day.                             "UD1K912931
*  l_arbpl = 'T'.                                    "UD1K912931


  SELECT SINGLE kalid INTO l_kalid
    FROM zvpp_capacity
   WHERE arbpl = WA_arbpl .

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = '+'
      date                         = la_date
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

  IF la_date = l_date.
    SELECT SINGLE * INTO lw_ld
      FROM zvpp_ld
     WHERE ld_perst <= l_date
       AND ld_pered >= l_date
       AND arbpl     = WA_arbpl
       AND ld_shift  = 1       .

    IF lw_ld-lantu = 0.
      l_uph = 0.
    ELSE.
      l_uph = lw_ld-lrate / lw_ld-lantu .
    ENDIF.
*    pa_uph1 = l_uph.                               "UD1K912931
    st_5290_input-uph_l = l_uph.                            "UD1K912931

    CLEAR: lw_ld.
    SELECT SINGLE * INTO lw_ld
      FROM zvpp_ld
     WHERE ld_perst <= l_date
       AND ld_pered >= l_date
       AND arbpl     = WA_arbpl
       AND ld_shift  = 2       .

    IF lw_ld-lantu = 0.
      l_uph = 0.
    ELSE.
      l_uph = lw_ld-lrate / lw_ld-lantu .
    ENDIF.
*    pa_uph2 = l_uph.                             "UD1K912931
    st_5290_input-uph_h = l_uph.                            "UD1K912931

** Furong on 06/12/12 for 3 shift
    CLEAR: lw_ld.
    SELECT SINGLE * INTO lw_ld
      FROM zvpp_ld
     WHERE ld_perst <= l_date
       AND ld_pered >= l_date
       AND arbpl     = WA_arbpl
       AND ld_shift  = 3.

    IF lw_ld-lantu = 0.
      l_uph = 0.
    ELSE.
      l_uph = lw_ld-lrate / lw_ld-lantu .
    ENDIF.
    st_5290_input-uph_t = l_uph.
** End on 06/12/12

  ELSE.
    CLEAR: st_5290_input-uph_h,                             "UD1K912931
           st_5290_input-uph_l,                             "UD1K912931
** Furong on 06/12/12 for 3 shift
           st_5290_input-uph_t,
** End 3 shift
           st_5290_input-ftime,                             "UD1K912931
           st_5290_input-ttime.                             "UD1K912931
    l_date = la_date.
  ENDIF.

** Changed by Furong on 11/21/08

** changed by Furong on 02/27/06
*  select single objid into l_objid
*    from crhd
*    where ARBPL = WA_ARBPL.
*  select single LRATE into st_5290_input-uph_l
*    from ldlh
*    where LNID = l_objid.
*  st_5290_input-uph_h = st_5290_input-uph_l.

** Furong on 06/12/12 for 3 shift
*  select single uph into st_5290_input-uph_l
*    from ztpp_plan_day
*    where prdt_date = ST_5290_INPUT-DATE
*      and model = 'EMF'.
*  st_5290_input-uph_h = st_5290_input-uph_l.
*  st_5290_input-uph_T = st_5290_input-uph_l. " add new for 3 shift
** End on 06/12/12

** End of change on 11/21/08

  CLEAR: lt_master.
  PERFORM get_day          USING l_date lt_master-day  .

*CHECK WHICH SHIFT
** changed by Furong on 02/21/2006
*  IF l_shift = 1 OR l_shift = 3.           "UD1K912931
  wa_shift = 1.                                             "UD1K912931
  PERFORM get_working_time USING l_date lt_master-time
                           lt_master-time_2                 "UD1K912931
** Furong on 06/12/12 for 3 shift
                               lt_master-time_3
** End on 06/12/12
                               lt_master-day  wa_shift
                               l_ftime  l_ttime
                               l_btime  l_etime.
*   STORE THE FIRST SHIFT DATA
  st_5290_input-ftime = l_ftime.                            "UD1K912931
  st_5290_input-ttime = l_ttime.                            "UD1K912931
  st_5290_input-btime = l_btime.                            "UD1K912931
  st_5290_input-etime = l_etime.                            "UD1K912931

  lw_time = l_btime.

*  ENDIF.
*  IF l_shift = 2 OR l_shift = 3 .          "UD1K912931
  wa_shift = 2.
  PERFORM get_working_time USING l_date lt_master-time
                               lt_master-time_2
** Furong on 06/12/12 for 3 shift
                               lt_master-time_3
** End on 06/12/12
                               lt_master-day  wa_shift
                               l_ftime  l_ttime
                               l_btime  l_etime.            "UD1K912931
*   STORE THE SECOND SHIFT DATA
  st_5290_input-ftime_2 = l_ftime.                          "UD1K912931
  st_5290_input-ttime_2 = l_ttime.                          "UD1K912931
  st_5290_input-btime_2 = l_btime.                          "UD1K912931
  st_5290_input-etime_2 = l_etime.                          "UD1K912931

** Furong on 06/12/12 for 3 shift
  wa_shift = 3.
  PERFORM get_working_time USING l_date lt_master-time
                               lt_master-time_2
                               lt_master-time_3
                               lt_master-day  wa_shift
                               l_ftime  l_ttime
                               l_btime  l_etime.            "UD1K912931
*   STORE THE SECOND SHIFT DATA
  st_5290_input-ftime_3 = l_ftime.
  st_5290_input-ttime_3 = l_ttime.
  st_5290_input-btime_3 = l_btime.
  st_5290_input-etime_3 = l_etime.                          "UD1K912931

  st_5290_input-ttime_3 = l_ttime + 900.                    "UD1K912931

** End on 06/12/12

** added by Furong
*    st_5290_input-etime = l_btime.
*    st_5290_input-etime_2 = st_5290_input-btime.
*
*    st_5290_input-ttime = l_ftime.
*    st_5290_input-ttime_2 = st_5290_input-ftime.
*
** Furong on 06/12/12 for 3 shift
  "   ?????????????????????????????????????????
** End on 06/12/12

** end of addd
*  ENDIF.
** end of change
  l_chk =  st_5290_input-uph_l * lt_master-time / 3600 .
  l_uph =  st_5290_input-uph_h * lt_master-time_2 / 3600 .

  st_5290_input-lqty = l_chk .
  st_5290_input-hqty = l_uph.

** Furong on 06/12/12 for 3 shift
  l_uph_3 =  st_5290_input-uph_t * lt_master-time_3 / 3600 .
  st_5290_input-tqty = l_uph_3.
  st_5290_input-dayu = l_uph + l_chk + l_uph_3.

*  st_5290_input-dayu = l_uph + l_chk .
** end on 06/12/12 for 3 shift





ENDFORM.                    " SET_UPH

*&---------------------------------------------------------------------*
*&      Form  save_vehicle_2202
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_vehicle_2202.
  DATA: l_equnr LIKE equi-equnr,
        lt_vals LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  LOOP AT it_2202.
    CLEAR: lt_vals, lt_vals[].
    l_equnr = it_2202-body   .   CONDENSE l_equnr NO-GAPS.
    lt_vals-atnam = 'P_RETURN_REWORK_DATE'.
    lt_vals-atwrt = it_2202-wdate         .
    APPEND lt_vals .
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = l_equnr
        mode         = 'W'
        ctype        = '002'
      TABLES
        val_table    = lt_vals
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        error_value  = 4
        OTHERS       = 5.
  ENDLOOP.
ENDFORM.                    " save_vehicle_2202

*&---------------------------------------------------------------------*
*&      Form  DELT_vehicle_2202
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delt_vehicle_2202.
  DATA: l_equnr    LIKE equi-equnr,
        lt_vals    LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  LOOP AT it_2202 WHERE mark = 'X' .
    CLEAR: lt_vals, lt_vals[].
    l_equnr = it_2202-body   .   CONDENSE l_equnr NO-GAPS.
    lt_vals-atnam = 'P_RETURN_REWORK_DATE'.
    APPEND lt_vals .
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = l_equnr
        mode         = 'W'
        ctype        = '002'
      TABLES
        val_table    = lt_vals
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        error_value  = 4
        OTHERS       = 5.
  ENDLOOP.
ENDFORM.                    " DELT_vehicle_2202

*&---------------------------------------------------------------------*
*&      Form  READ_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7732   text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
FORM read_atinn USING    pa_char  pa_atinn.
  SELECT SINGLE atinn INTO pa_atinn
    FROM cabn
   WHERE atnam = pa_char.
ENDFORM.                    " READ_ATINN

*&---------------------------------------------------------------------*
*&      Form  call_workday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
FORM call_workday USING    pa_date.
  DATA: l_ident             LIKE t001w-fabkl.

  SELECT SINGLE fabkl  INTO l_ident
    FROM t001w
   WHERE werks = 'P001'   .

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = '+'
      date                         = pa_date
      factory_calendar_id          = l_ident
    IMPORTING
      date                         = pa_date
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
ENDFORM.                    " call_workday

*&---------------------------------------------------------------------*
*&      Form  READ_ALC_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_alc_model          USING pa_model.
  DATA: lt_cukb              LIKE TABLE OF cukb WITH HEADER LINE,
        lc_model(15)         TYPE c  ,
        l_knobj              LIKE cuco-knobj,
        l_knnum              LIKE cuob-knobj,
        l_knnam              LIKE cukb-knnam.

  CLEAR: it_model, it_model[], it_alc, it_alc[].
  PERFORM get_models USING pa_model.

  LOOP AT it_model   .
    wa_model = it_model-modl .
    CONCATENATE 'D_' wa_model '_ALC_'  INTO lc_model    .
    CONCATENATE wa_model '_WOHD'    INTO  l_knnam.
    PERFORM get_knobj               USING l_knnam  l_knobj.
    PERFORM get_knnum               USING l_knobj.
    CONCATENATE wa_model '_WOCL'    INTO  l_knnam.
    PERFORM get_knobj               USING l_knnam  l_knobj.
    PERFORM get_knnum               USING l_knobj.
    " Set the Model Code...
    LOOP AT it_alc  WHERE model = space.
      it_alc-model = wa_model .
      MODIFY it_alc.
    ENDLOOP.
  ENDLOOP.

  LOOP AT it_alc.
    CONCATENATE 'D_' it_alc-model '_ALC_'  INTO lc_model    .
    SELECT SINGLE b~knnam t~knktx
      INTO CORRESPONDING FIELDS OF it_alc
      FROM cukb AS b INNER JOIN cukbt AS t
        ON b~knnum = t~knnum
     WHERE b~knnum = it_alc-knnum
       AND t~spras = sy-langu   .

    IF it_alc-knnam(10) NE lc_model .
      DELETE it_alc .
      CONTINUE .
    ENDIF.
    it_alc-code     = it_alc-knnam.                         "+12(3) .
    it_alc-type_alc = it_alc-knnam.                         "+10(1) .
    it_alc-rp       = it_alc-knktx(2)    .
    CONCATENATE 'P' it_alc-knnam+5(10)  INTO it_alc-char_alc .
    MODIFY it_alc .
  ENDLOOP.
  SORT it_alc BY knnum rp code .
ENDFORM.                    " READ_ALC_MODEL

*&---------------------------------------------------------------------*
*&      Form  READ_VARIANT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_variant_table      USING pa_model.
  DATA: lt_cukb            LIKE TABLE OF cukb  WITH HEADER LINE,
        lc_model(15)       TYPE c  ,
        l_knobj            LIKE cuco-knobj,
        l_knnum            LIKE cuob-knobj,
        l_knnam            LIKE cukb-knnam.

  CLEAR: it_model, it_model[], it_alc, it_alc[].
*  PERFORM get_models USING pa_model.

*  LOOP AT it_model   .
*    wa_model = it_model-modl .
  CONCATENATE wa_model '_%'              INTO lc_model .
  PERFORM get_cuvtab                     USING lc_model.
  " Set the Model Code...
  LOOP AT it_alc  WHERE model = space.
    it_alc-model = wa_model .
    MODIFY it_alc.
  ENDLOOP.
*  ENDLOOP.

  LOOP AT it_alc.
*    CONCATENATE 'D_' it_alc-model '_ALC_'  INTO lc_model    .
    SELECT SINGLE vttxt  INTO it_alc-knktx
      FROM cuvtab_tx
     WHERE vtint = it_alc-knnum
       AND spras = sy-langu   .

*    IF it_alc-knnam(10) NE lc_model .
*      DELETE it_alc .
*      CONTINUE .
*    ENDIF.
*   it_alc-code     = it_alc-knnam.   "+12(3) .
*   it_alc-type_alc = it_alc-knnam.   "+10(1) .
*   it_alc-rp       = it_alc-knktx(2)    .
*   CONCATENATE 'P' it_alc-knnam+5(10)  INTO it_alc-char_alc .
    MODIFY it_alc .
  ENDLOOP.
  SORT it_alc BY knnum rp code .
ENDFORM.                    " READ_VARIANT_TABLE

*&---------------------------------------------------------------------*
*&      Form  GET_MODELS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_models          USING pa_model .
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

  CHECK pa_model NE space.
  DELETE it_model WHERE modl NE pa_model.
ENDFORM.                    " GET_MODELS

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
  DATA lw_ld          LIKE zvpp_ld .

  IF pa_shift IS INITIAL .
    SELECT SINGLE * INTO lw_ld
      FROM zvpp_ld
     WHERE ld_perst <= pa_wdate
       AND ld_pered >= pa_wdate
       AND arbpl     = 'T'      .
  ELSE.
    SELECT SINGLE * INTO lw_ld
      FROM zvpp_ld
     WHERE ld_perst <= pa_wdate
       AND ld_pered >= pa_wdate
       AND ld_shift  = pa_shift
       AND arbpl     = 'T'      .
  ENDIF.

  IF lw_ld-lantu = 0.
    pa_uph = 0 .
  ELSE.
    pa_uph = lw_ld-lrate / lw_ld-lantu .
  ENDIF.
ENDFORM.                    " GET_UPH

*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
** Furong on 06/12/12 for 3 shift
*FORM get_working_time USING    pa_wdate  pa_wktime_1 pa_wktime_2 pa_day
*                               pa_shift  pa_ftime   pa_ttime

FORM get_working_time USING pa_wdate pa_wktime_1 pa_wktime_2
                            pa_wktime_3
                            pa_day pa_shift  pa_ftime pa_ttime
** End on 06/12/12
                               pa_btime  pa_etime.          "UD1K912931
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_wtime_2     LIKE zvpp_capacity-endzt,             "UD1K912931
        l_wtime_3     LIKE zvpp_capacity-endzt,
        l_date        TYPE d ,
        l_begzt       LIKE tc37a-begzt ,
        l_einzt       LIKE tc37a-einzt ,
        l_endzt       LIKE tc37a-endzt ,
        lt_capa   LIKE TABLE OF zvpp_capacity WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = WA_ARBPL      "'T'
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
    IF lt_capa-schnr = '1'     .                            "UD1K912931
      l_wtime = l_wtime + l_einzt    .
* reading the shift break time
      PERFORM read_break USING '1' lt_capa-tprog.
    ELSEif lt_capa-schnr = '2'.                             "UD1K912931

      l_wtime_2 = l_wtime_2 + l_einzt.                      "UD1K912931
* reading the shift break time
      PERFORM read_break USING '2' lt_capa-tprog.
** Furong on 06/12/12 for 3 shift
    ELSEif lt_capa-schnr = '3'.                             "UD1K912931

      l_wtime_3 = l_wtime_3 + l_einzt.                      "UD1K912931

      PERFORM read_break USING '3' lt_capa-tprog.
** End on 06/12/12

    ENDIF.                                                  "UD1K912931

  ENDLOOP.
  pa_wktime_1 = l_wtime .
  pa_wktime_2 = l_wtime_2.                                  "UD1K912931
** Furong on 06/12/12 for 3 shift
  pa_wktime_3 = l_wtime_3.
** End
  CLEAR lt_capa.                                            "UD1K912931
  READ TABLE lt_capa
       WITH KEY datub = l_date tagnr = pa_day schnr = pa_shift .
  CLEAR: l_begzt, l_endzt.
  SELECT SINGLE begzt endzt INTO (l_begzt, l_endzt)
    FROM tc37a
   WHERE schgrup  = lt_capa-mosid
     AND kaptprog = lt_capa-tprog
     AND endda   >= pa_wdate
     AND begda   <= pa_wdate     .

  pa_btime    = l_begzt.                                    "UD1K912931
  pa_etime    = l_endzt.                                    "UD1K912931
  pa_ftime    = l_begzt.
  pa_ttime    = l_endzt.

ENDFORM.                    " GET_WORKING_TIME

*&---------------------------------------------------------------------*
*&      Form  SEARCH_DATA
*&---------------------------------------------------------------------*
*       Searching Data & Setting Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_data_1205.
  CLEAR it_vtentries.
  REFRESH it_vtentries.

  CALL FUNCTION 'CARD_TABLE_READ_ENTRIES'
    EXPORTING
      var_table       = p_full_code
    TABLES
      var_tab_entries = it_vtentries
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.

* DEFINE COLUMN.
  DATA: l_column TYPE vtentries-vtlineno.
  DATA: l_row TYPE vtentries-vtlineno.
  LOOP AT it_vtentries.
    IF sy-tabix = 1.  "First Row
      l_row = 1.
    ENDIF.

    IF l_row = it_vtentries-vtlineno.   "Past Row = Recent Row
      l_column = l_column + 1.  "Increase column number
*
      READ TABLE it_column WITH KEY charact =
                                    it_vtentries-vtcharact.
      IF sy-tabix <> l_column. "If there is not a value of column
        l_column = l_column + 1.
      ENDIF.
    ELSE.                               "Past Row <> Recent Row
      l_row = it_vtentries-vtlineno.  "Set Row
      l_column = 1.  "Reset Column
    ENDIF.

    CONDENSE l_column.
    MOVE l_column TO it_vtentries-column.
    MODIFY it_vtentries.

  ENDLOOP.

* SET THE INTERNAL TABLE
  CLEAR l_row.
  CLEAR: it_app223.
  REFRESH: it_app223.
  LOOP AT it_vtentries.
    AT NEW vtlineno.
      CLEAR it_app223.
      MOVE it_vtentries-vtlineno TO it_app223-line.
      MOVE it_vtentries-vtvalue TO it_app223-code.
      APPEND it_app223.
      l_row = l_row + 1.
    ENDAT.

    CASE it_vtentries-column.
      WHEN '1'.  "CODE
        MOVE it_vtentries-vtvalue TO it_app223-code.
        MODIFY it_app223 INDEX l_row.
      WHEN '2'.  "DATE
*
        MOVE it_vtentries-vtvalue TO it_app223-date.
*
        MODIFY it_app223 INDEX l_row.
      WHEN '3'.
        MOVE it_vtentries-vtvalue TO it_app223-col_01.
        MODIFY it_app223 INDEX l_row.
      WHEN '4'.
        MOVE it_vtentries-vtvalue TO it_app223-col_02.
        MODIFY it_app223 INDEX l_row.
      WHEN '5'.
        MOVE it_vtentries-vtvalue TO it_app223-col_03.
        MODIFY it_app223 INDEX l_row.
      WHEN '6'.
        MOVE it_vtentries-vtvalue TO it_app223-col_04.
        MODIFY it_app223 INDEX l_row.
      WHEN '7'.
        MOVE it_vtentries-vtvalue TO it_app223-col_05.
        MODIFY it_app223 INDEX l_row.
      WHEN '8'.
        MOVE it_vtentries-vtvalue TO it_app223-col_06.
        MODIFY it_app223 INDEX l_row.
      WHEN '9'.
        MOVE it_vtentries-vtvalue TO it_app223-col_07.
        MODIFY it_app223 INDEX l_row.
      WHEN '10'.
        MOVE it_vtentries-vtvalue TO it_app223-col_08.
        MODIFY it_app223 INDEX l_row.
      WHEN '11'.
        MOVE it_vtentries-vtvalue TO it_app223-col_09.
        MODIFY it_app223 INDEX l_row.
      WHEN '12'.
        MOVE it_vtentries-vtvalue TO it_app223-col_10.
        MODIFY it_app223 INDEX l_row.
      WHEN '13'.
        MOVE it_vtentries-vtvalue TO it_app223-col_11.
        MODIFY it_app223 INDEX l_row.
      WHEN '14'.
        MOVE it_vtentries-vtvalue TO it_app223-col_12.
        MODIFY it_app223 INDEX l_row.
      WHEN '15'.
        MOVE it_vtentries-vtvalue TO it_app223-col_13.
        MODIFY it_app223 INDEX l_row.
      WHEN '16'.
        MOVE it_vtentries-vtvalue TO it_app223-col_14.
        MODIFY it_app223 INDEX l_row.
      WHEN '17'.
        MOVE it_vtentries-vtvalue TO it_app223-col_15.
        MODIFY it_app223 INDEX l_row.
      WHEN '18'.
        MOVE it_vtentries-vtvalue TO it_app223-col_16.
        MODIFY it_app223 INDEX l_row.
      WHEN '19'.
        MOVE it_vtentries-vtvalue TO it_app223-col_17.
        MODIFY it_app223 INDEX l_row.
      WHEN '20'.
        MOVE it_vtentries-vtvalue TO it_app223-col_18.
        MODIFY it_app223 INDEX l_row.
      WHEN '21'.
        MOVE it_vtentries-vtvalue TO it_app223-col_19.
        MODIFY it_app223 INDEX l_row.
      WHEN '22'.
        MOVE it_vtentries-vtvalue TO it_app223-col_20.
        MODIFY it_app223 INDEX l_row.
      WHEN '23'.
        MOVE it_vtentries-vtvalue TO it_app223-col_21.
        MODIFY it_app223 INDEX l_row.
      WHEN '24'.
        MOVE it_vtentries-vtvalue TO it_app223-col_22.
        MODIFY it_app223 INDEX l_row.
      WHEN '25'.
        MOVE it_vtentries-vtvalue TO it_app223-col_23.
        MODIFY it_app223 INDEX l_row.
    ENDCASE.

  ENDLOOP.

  LOOP AT it_app223.
    CONCATENATE it_app223-col_01 it_app223-col_02
                it_app223-col_03 it_app223-col_04
                it_app223-col_05 it_app223-col_06
                it_app223-col_07 it_app223-col_08
                it_app223-col_09 it_app223-col_10
                it_app223-col_11 it_app223-col_12
                it_app223-col_13 it_app223-col_14
                it_app223-col_15 it_app223-col_16
                it_app223-col_17 it_app223-col_18
                it_app223-col_19 it_app223-col_20
      INTO it_app223-con_col .
    MODIFY it_app223.
  ENDLOOP.

ENDFORM.                    " SEARCH_DATA
*&---------------------------------------------------------------------*
*&      Form  set_excel
*&---------------------------------------------------------------------*
*       Setting Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_excel_1205.
*
  CLEAR it_excel_1205.
  REFRESH it_excel_1205.
  MOVE p_full_code TO it_excel_1205-code.
  APPEND it_excel_1205.

  CLEAR it_excel_1205.
  MOVE 'CODE' TO it_excel_1205-code.
  MOVE 'Full Key' TO it_excel_1205-con_col.
  MOVE 'DATE' TO it_excel_1205-date.
  READ TABLE it_column INDEX 3.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_01.
  ENDIF.
  READ TABLE it_column INDEX 4.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_02.
  ENDIF.
  READ TABLE it_column INDEX 5.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_03.
  ENDIF.
  READ TABLE it_column INDEX 6.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_04.
  ENDIF.
  READ TABLE it_column INDEX 7.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_05.
  ENDIF.
  READ TABLE it_column INDEX 8.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_06.
  ENDIF.
  READ TABLE it_column INDEX 9.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_07.
  ENDIF.
  READ TABLE it_column INDEX 10.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_08.
  ENDIF.
  READ TABLE it_column INDEX 11.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_09.
  ENDIF.
  READ TABLE it_column INDEX 12.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_10.
  ENDIF.
  READ TABLE it_column INDEX 13.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_11.
  ENDIF.
  READ TABLE it_column INDEX 14.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_12.
  ENDIF.
  READ TABLE it_column INDEX 15.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_13.
  ENDIF.
  READ TABLE it_column INDEX 16.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_14.
  ENDIF.
  READ TABLE it_column INDEX 17.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_15.
  ENDIF.
  READ TABLE it_column INDEX 18.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_16.
  ENDIF.
  READ TABLE it_column INDEX 19.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_17.
  ENDIF.
  READ TABLE it_column INDEX 20.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_18.
  ENDIF.
  READ TABLE it_column INDEX 21.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_19.
  ENDIF.
  READ TABLE it_column INDEX 22.
  IF sy-subrc = 0.
    MOVE it_column-charact TO it_excel_1205-col_20.
  ENDIF.
  APPEND it_excel_1205.

  LOOP AT it_app223.
    CLEAR it_excel_1205.
    MOVE-CORRESPONDING it_app223 TO it_excel_1205.
    APPEND it_excel_1205.
  ENDLOOP.

ENDFORM.                    " set_excel
*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
*       Calling a Func. For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_func_download_1205.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      filename                = 'ALC Code.XLS'
      filetype                = 'DAT'
      item                    = ' '
      filetype_no_change      = 'X'
      filetype_no_show        = 'X'
    TABLES
      data_tab                = it_excel_1205
    EXCEPTIONS
      invalid_filesize        = 1
      invalid_table_width     = 2
      invalid_type            = 3
      no_batch                = 4
      unknown_error           = 5
      gui_refuse_filetransfer = 6
      OTHERS                  = 7.
ENDFORM.                    " call_func_download

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ALC
*&---------------------------------------------------------------------*
*       Updating ALC Code
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_alc_1205.
  DATA: l_vtint.
  DATA: l_slnid TYPE cuvtln-slnid.  " INDEX

  CLEAR: it_table_header.
  REFRESH: it_table_header.
* Read Table of Variant table basic data
  PERFORM read_cuvtab.  " Variant table basic data
*
  READ TABLE it_table_header INDEX 1.
  LOOP AT it_app223 WHERE mark = 'X'.
    CLEAR: it_lines_old,
           it_lines_new,
           it_values_c_old,
           it_values_c_new,
           it_values_n_old,
           it_values_n_new .
    REFRESH: it_lines_old,
             it_lines_new,
             it_values_c_old,
             it_values_c_new,
             it_values_n_old,
             it_values_n_new .

    CLEAR l_slnid.
*   READ INDEX BY table_header-vtint and IT_APP223-line.
    PERFORM read_cuvtln USING it_table_header-vtint
                              it_app223-line
                              l_slnid.
*   Read old Data
    PERFORM read_old_data_1205 USING it_table_header-vtint
                                     l_slnid .
*   Create New Data
    PERFORM make_new_data TABLES it_app223
                          USING  it_table_header-vtint
                                 l_slnid .

*   Call a Func. For Updating Data
    CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
      EXPORTING
        table_header = it_table_header
*       ECM_NUMBER   =
      TABLES
        lines_old    = it_lines_old
        lines_new    = it_lines_new
        values_c_old = it_values_c_old
        values_c_new = it_values_c_new
        values_n_old = it_values_n_old
        values_n_new = it_values_n_new.

  ENDLOOP.

ENDFORM.                    " UPDATE_ALC
*&---------------------------------------------------------------------*
*&      Form  MAKE_TABLE_HEADER
*&---------------------------------------------------------------------*
*       Reading Table - CUVTAB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cuvtab.
  DATA: wa_table_header TYPE cuvtab.
  CLEAR: wa_table_header, it_table_header.
  REFRESH it_table_header.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF wa_table_header
    FROM cuvtab
    WHERE vtnam = p_full_code AND
          vtsta = '1'.
  APPEND wa_table_header TO it_table_header.

ENDFORM.                    " MAKE_TABLE_HEADER
*&---------------------------------------------------------------------*
*&      Form  READ_CUVTLN
*&---------------------------------------------------------------------*
*       Reading Table - CUVTLN
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cuvtln USING p_vtint
                       p_line
                       p_slnid.
  CLEAR p_slnid.
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF wa_lines_1205
    FROM cuvtln
    WHERE vtint = p_vtint AND
          vtlin = p_line .

  APPEND wa_lines_1205 TO it_lines_new.
  APPEND wa_lines_1205 TO it_lines_old.
  MOVE wa_lines_1205-slnid TO p_slnid.

ENDFORM.                    " READ_CUVTLN

*&---------------------------------------------------------------------*
*&      Form  sort_SCREEN_1205
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_screen_1205     USING pa_stype .
  DATA: lw_screen   TYPE TABLE OF cxtab_column  WITH HEADER LINE,
        field_name01(40),
        offset01 TYPE i.
*
  CLEAR:  field_name01.
  LOOP AT tc_app223-cols INTO lw_screen.
    IF lw_screen-selected = 'X' .
      field_name01 = lw_screen-screen-name .
      field_name01 = field_name01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE pa_stype.
    WHEN 'A'.
      SORT it_app223 ASCENDING BY (field_name01).
    WHEN 'D'.
      SORT it_app223 DESCENDING BY (field_name01).
  ENDCASE.
ENDFORM.                    " sort_SCREEN_1205

*&---------------------------------------------------------------------*
*&      Form  MAKE_NEW_DATA
*&---------------------------------------------------------------------*
*       Setting Data Per Code Part Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_new_data TABLES p_it_app223 STRUCTURE it_app223
                   USING  p_vtint
                          p_slnid .
  DATA: l_date_d LIKE sy-datum,
        l_date_c(08).
  DATA original_date TYPE d.

  DATA l_tabix LIKE sy-tabix.
  LOOP AT it_column.
    CLEAR: it_values_n_new, it_values_c_new.
    l_tabix = sy-tabix.

    SEARCH it_column-charact FOR 'DATE'.
    IF sy-subrc = 0.   " If it is date type ...
      MOVE p_vtint TO it_values_n_new-vtint.
      MOVE p_slnid TO it_values_n_new-slnid.
      PERFORM call_function_conversion USING it_column-charact
                                          it_values_n_new-atinn.
      MOVE 1 TO it_values_n_new-vlcnt.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*     DATE CONTROL : IT_VALUES_N_NEW-VAL_FROM
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      CLEAR: l_date_c.
      CALL 'DATE_CONV_EXT_TO_INT'
        ID 'DATEXT' FIELD p_it_app223-date
        ID 'DATINT' FIELD l_date_d.

*      PERFORM change_to_sys_date_type USING p_IT_APP223-date
*                                            l_date_c .
      MOVE l_date_d TO l_date_c.
      MOVE l_date_c TO it_values_n_new-val_from.

      MOVE '1' TO it_values_n_new-val_code.
      APPEND it_values_n_new .

    ELSE.
      CASE l_tabix.
        WHEN 1 .  "CODE
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                          it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-code TO it_values_c_new-valc.
          APPEND it_values_c_new.
*        WHEN 2 .  "DATE USUALLY
        WHEN 3 .                                            " 1ST KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_01 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 4 .                                            " 2ND KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                     USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_02 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 5 .                                            " 3RD KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_03 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 6 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_04 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 7 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_05 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 8 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_06 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 9 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_07 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 10 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_08 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 11 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_09 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 12 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_10 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 13 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_11 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 14 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_12 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 15 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_13 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 16 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_14 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 17 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_15 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 18 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_16 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 19 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_17 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 20 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_18 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 21 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_19 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 22 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                           it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_it_app223-col_20 TO it_values_c_new-valc.
          APPEND it_values_c_new.
      ENDCASE.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MAKE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  read_old_data
*&---------------------------------------------------------------------*
*       Reading Previous Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_old_data_1205 USING p_vtint    "
                              p_slnid .  "INDEX
*
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_values_n_old
    FROM cuvtab_valn
    WHERE vtint = p_vtint AND
          slnid = p_slnid .
*
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_values_c_old
    FROM cuvtab_valc
    WHERE vtint = p_vtint AND
          slnid = p_slnid .

ENDFORM.                    " read_old_data
*&---------------------------------------------------------------------*
*&      Form  call_function_conversion
*&---------------------------------------------------------------------*
*       Reading a Internal Characteristic
*----------------------------------------------------------------------*
*      -->P_IT_VTENTRIES_VTCHARACT  text
*      -->P_L_ATINN_N  text
*----------------------------------------------------------------------*
FORM call_function_conversion USING    p_char_c
                                       p_numb_n.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = p_char_c
    IMPORTING
      output = p_numb_n.

ENDFORM.                    " call_function_conversion
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       Deletion of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data_1205.
  DATA: l_answer,
        l_count_i TYPE i,
        l_text(40),
        l_count_c(03).

  CLEAR l_count_i .
  LOOP AT it_app223 WHERE mark = 'X'.
    l_count_i = l_count_i + 1.
  ENDLOOP.

  WRITE l_count_i TO l_count_c .
  CONCATENATE 'Do you want to delete ' l_count_c 'EA data ? '
              INTO l_text.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'Y'
      textline1      = l_text
      titel          = 'Delete'
      start_column   = 25
      start_row      = 6
      cancel_display = space
    IMPORTING
      answer         = l_answer.

  IF l_answer = 'J'.
    PERFORM delete_process_1205.
  ENDIF.

ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_PROCESS
*&---------------------------------------------------------------------*
*       Deletion of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_process_1205.
  DATA: l_vtint.
  DATA: l_slnid TYPE cuvtln-slnid.  " INDEX

  CLEAR: it_table_header.
  REFRESH: it_table_header.
*
  PERFORM read_cuvtab.  " Variant table basic data
*
  READ TABLE it_table_header INDEX 1.
  LOOP AT it_app223 WHERE mark = 'X'.
*
    CLEAR l_slnid.

    PERFORM read_old_lines USING it_table_header-vtint.
    PERFORM set_new_lines USING it_table_header-vtint
                                it_app223-line   "LINE
                                l_slnid     .  "INDEX
    PERFORM read_old_data_1205 USING it_table_header-vtint
                                     l_slnid .

    CLEAR: it_values_c_new, it_values_n_new .
    REFRESH: it_values_c_new, it_values_n_new .

    CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
      EXPORTING
        table_header = it_table_header
*       ECM_NUMBER   =
      TABLES
        lines_old    = it_lines_old
        lines_new    = it_lines_new
        values_c_old = it_values_c_old
        values_c_new = it_values_c_new
        values_n_old = it_values_n_old
        values_n_new = it_values_n_new.

  ENDLOOP.


ENDFORM.                    " DELETE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_OLD_LINES
*&---------------------------------------------------------------------*
*       Reading The Previous Lines
*----------------------------------------------------------------------*
*      -->P_IT_TABLE_HEADER_VTINT  text
*----------------------------------------------------------------------*
FORM read_old_lines USING    p_vtint.
  CLEAR: it_lines_old .
  REFRESH: it_lines_old .
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_lines_old
    FROM cuvtln
    WHERE vtint = p_vtint .

ENDFORM.                    " READ_OLD_LINES
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_LINES
*&---------------------------------------------------------------------*
*       Setting New Lines
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_new_lines USING  p_vtint
                          p_line
                          p_slnid     .

  DATA: l_vtlin TYPE cuvtln-vtlin.

  CLEAR: it_lines_new,  l_vtlin .
  REFRESH: it_lines_new .

  CLEAR p_slnid.
  SELECT SINGLE slnid
    INTO p_slnid
    FROM cuvtln
    WHERE vtint = p_vtint AND
          vtlin = p_line .

  SORT it_lines_old BY vtlin .
  LOOP AT it_lines_old.
    CLEAR it_lines_new.
    IF it_lines_old-slnid = p_slnid.
      CONTINUE.
    ELSE.
      l_vtlin = l_vtlin + 1.
      MOVE-CORRESPONDING it_lines_old TO it_lines_new.
      MOVE l_vtlin TO it_lines_new-vtlin .
      APPEND it_lines_new.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " SET_NEW_LINES
*&---------------------------------------------------------------------*
*&      Form  setting_p_full_code
*&---------------------------------------------------------------------*
*       Setting Parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_parameters_1206.
* Full Code(ALC) = p_model+'_ALC_'+p_part+'_'+p_key.
  CONDENSE p_key.
  CONCATENATE wa_model '_ALC_' p_part '_' p_key
    INTO p_full_code.

  CLEAR: wa_descriptions, it_column.
  REFRESH: wa_descriptions, it_column.

  IF ( wa_model IS INITIAL ) OR
     ( p_part IS INITIAL ) OR
     ( p_key IS INITIAL ) .
    EXIT.
  ENDIF.

  CALL FUNCTION 'CARD_TABLE_READ_STRUCTURE'
    EXPORTING
      var_tab                    = p_full_code
*   CHANGE_NO                  =
*   DATE                       =
      language                   = 'E'
* IMPORTING
*   BASIC_DATA                 =
*   RETURN                     =
    TABLES
      descriptions               = wa_descriptions
      characteristics            = it_column
*   VALUE_ASSIGNMENT_ALT       =
    EXCEPTIONS
      error                      = 1
      OTHERS                     = 2 .

  MOVE wa_descriptions-descript TO p_col_name.

  DATA: l_offset TYPE i.
  DATA: l_col_name(20).

  CLEAR: p_key_01, p_key_02, p_key_03, p_key_04, p_key_05,
         p_key_06, p_key_07, p_key_08, p_key_09, p_key_10,
         p_key_11, p_key_12, p_key_13, p_key_14, p_key_15,
         p_key_16, p_key_17, p_key_18, p_key_19, p_key_20.

  LOOP AT it_column.
    SEARCH it_column-charact FOR 'ALC'.
    IF sy-subrc = 0.
      CONTINUE.
    ELSE.
      CLEAR: l_offset, l_col_name.
      SEARCH it_column-charact FOR '219'.
      IF sy-subrc = 0.
        l_offset = sy-fdpos + 4.
        l_col_name = it_column-charact+l_offset.
      ELSE.
        l_col_name = it_column-charact+2.
      ENDIF.
      CASE sy-tabix.
        WHEN 3.
          p_key_01 = l_col_name.
        WHEN 4.
          p_key_02 = l_col_name.
        WHEN 5.
          p_key_03 = l_col_name.
        WHEN 6.
          p_key_04 = l_col_name.
        WHEN 7.
          p_key_05 = l_col_name.
        WHEN 8.
          p_key_06 = l_col_name.
        WHEN 9.
          p_key_07 = l_col_name.
        WHEN 10.
          p_key_08 = l_col_name.
        WHEN 11.
          p_key_09 = l_col_name.
        WHEN 12.
          p_key_10 = l_col_name.
        WHEN 13.
          p_key_11 = l_col_name.
        WHEN 14.
          p_key_12 = l_col_name.
        WHEN 15.
          p_key_13 = l_col_name.
        WHEN 16.
          p_key_14 = l_col_name.
        WHEN 17.
          p_key_15 = l_col_name.
        WHEN 18.
          p_key_16 = l_col_name.
        WHEN 19.
          p_key_17 = l_col_name.
        WHEN 20.
          p_key_18 = l_col_name.
        WHEN 21.
          p_key_19 = l_col_name.
        WHEN 22.
          p_key_20 = l_col_name.

      ENDCASE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " setting_p_full_code
*&---------------------------------------------------------------------*
*&      Form  CLEAR_IT_APP223_NEW
*&---------------------------------------------------------------------*
*       Initialization of Internal Table - IT_APP223_NEW
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_it_app223_new.
  CLEAR it_app223_new.
  REFRESH it_app223_new.

ENDFORM.                    " CLEAR_IT_APP223_NEW
*&---------------------------------------------------------------------*
*&      Form  CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*       Creation of New Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_new_data_1206.
  DATA: l_vtint.
  DATA: l_slnid TYPE cuvtln-slnid.  " INDEX

  CLEAR: it_table_header.
  REFRESH: it_table_header.
*
  PERFORM read_cuvtab.  " Variant table basic data
*
  READ TABLE it_table_header INDEX 1.
  LOOP AT it_app223_new.
    CLEAR: it_lines_old,
           it_lines_new,
           it_values_c_old,
           it_values_c_new,
           it_values_n_old,
           it_values_n_new .
    REFRESH: it_lines_old,
             it_lines_new,
             it_values_c_old,
             it_values_c_new,
             it_values_n_old,
             it_values_n_new .

    PERFORM make_new_line USING it_table_header-vtint
                                it_app223_new-line
                                l_slnid .

    PERFORM make_new_data TABLES it_app223_new
                          USING  it_table_header-vtint
                                 l_slnid .

    CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
      EXPORTING
        table_header = it_table_header
*       ECM_NUMBER   =
      TABLES
        lines_old    = it_lines_old
        lines_new    = it_lines_new
        values_c_old = it_values_c_old
        values_c_new = it_values_c_new
        values_n_old = it_values_n_old
        values_n_new = it_values_n_new.

  ENDLOOP.


ENDFORM.                    " CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  make_new_line
*&---------------------------------------------------------------------*
*       Creation of New Line
*----------------------------------------------------------------------*
*      -->P_IT_TABLE_HEADER_VTINT  text
*      -->P_L_SLNID  text
*----------------------------------------------------------------------*
FORM make_new_line USING    p_vtint
                            p_line
                            p_slnid.
  CLEAR: p_slnid, p_line .

  SELECT MAX( slnid ) MAX( vtlin )
    INTO (p_slnid , p_line)
    FROM cuvtln
    WHERE vtint = p_vtint.

  p_slnid = p_slnid + 1 .
  p_line = p_line + 1 .

  MOVE p_vtint TO it_lines_new-vtint .
  MOVE p_slnid TO it_lines_new-slnid .
  MOVE p_line  TO it_lines_new-vtlin .
  APPEND it_lines_new.

ENDFORM.                    " make_new_line
*&---------------------------------------------------------------------*
*&      Form  change_to_sys_date_type
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->P_P_IT_APP223_DATE  text
*      -->P_L_DATE_C  text
*----------------------------------------------------------------------*
FORM change_to_sys_date_type USING    p_char10
                                      p_date_c.
  DATA: l_date_01(04),
        l_date_02(04),
        l_date_03(04),
        l_date_c(08),
        l_temp(08),
        l_offset TYPE i,
        l_pos TYPE i.

  CLEAR: l_date_01,
         l_date_02,
         l_date_03,
         l_date_c,
         l_temp,
         l_offset,
         l_pos.

  IF p_char10 IS INITIAL .
    EXIT.
  ENDIF.

  SEARCH p_char10 FOR '/'.
  MOVE sy-fdpos TO l_offset.
  l_pos = l_offset .
  MOVE p_char10(l_pos) TO l_date_01.
  l_pos = l_offset + 1.
  MOVE p_char10+l_pos TO l_temp.

  SEARCH l_temp FOR '/'.
  MOVE sy-fdpos TO l_offset.
  l_pos = l_offset .
  MOVE l_temp(l_pos) TO l_date_02.
  l_pos = l_offset + 1.
  MOVE l_temp+l_pos TO l_temp.
  MOVE l_temp TO l_date_03.

  CONCATENATE l_date_01 l_date_02 l_date_03
    INTO p_date_c.


ENDFORM.                    " change_to_sys_date_type
*&---------------------------------------------------------------------*
*&      Form  set_title_bar_1206
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_title_bar_1206.
  DATA: l_text(70) TYPE c .
  l_text = 'Creation of ALC''s New Data'.
*  SET PF-STATUS 'STATUS111'.
  SET TITLEBAR 'TB' WITH l_text .


ENDFORM.                    " set_title_bar_1206
*&---------------------------------------------------------------------*
*&      Form  set_parameter
*&---------------------------------------------------------------------*
*       Setting Parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_parameter_1209.
* Plant
  CLEAR : xlist[], xvalue.
  name = 'WA_PLANT'.
  PERFORM set_field_plant   USING name  wa_plant   .

* Model
  CLEAR : xlist[], xvalue.
  name = 'WA_MODEL'.
  PERFORM set_field_model USING name  wa_model .

* Operation Date
  p_opdate = sy-datum.

* Operation Count
  p_opcount = 1.

* ORDER NO

ENDFORM.                    " set_parameter
*&---------------------------------------------------------------------*
*&      Form  set_field
*&---------------------------------------------------------------------*
*       Setting a Field For Dropdown List Box - PLANT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_plant    USING p_name  p_parameter .
  CLEAR xvalue.
  MOVE '1' TO xvalue-key.
  MOVE '1st Plant' TO xvalue-text.
  APPEND xvalue TO xlist.

  CLEAR xvalue.
  MOVE '2' TO xvalue-key.
  MOVE '2nd Plant' TO xvalue-text.
  APPEND xvalue TO xlist.

  CLEAR xvalue.
  MOVE '3' TO xvalue-key.
  MOVE '3rd Plant' TO xvalue-text.
  APPEND xvalue TO xlist.

  CLEAR xvalue.
  MOVE '4' TO xvalue-key.
  MOVE '4th Plant' TO xvalue-text.
  APPEND xvalue TO xlist.

* LIST BOX SETTING
  PERFORM list_box_function USING p_name.
  IF p_parameter IS INITIAL.
    READ TABLE xlist INTO xvalue  INDEX 1.
    p_parameter = xvalue-key.
  ENDIF.
ENDFORM.                    " set_field

*&---------------------------------------------------------------------*
*&      Form  SETUP_PARAMETER
*&---------------------------------------------------------------------*
*       Setting Parameters when they are not empty.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setup_parameter_1209.
  IF wa_plant <> space.
    CLEAR   r_plant.
    REFRESH r_plant.
    r_plant-option = 'EQ'.
    r_plant-sign   = 'I'.
    r_plant-low = wa_plant.
    APPEND r_plant.
  ELSE.
    CLEAR  r_plant.
    REFRESH r_plant.
  ENDIF.
ENDFORM.                    " SETUP_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Searching Data with parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_1209.
  DATA: l_model(04),
        l_extc(04),
        l_intc(04).

  CONCATENATE p_extc    '%'    INTO l_extc.
  CONCATENATE p_intc    '%'    INTO l_intc.
  CONCATENATE wa_model  '%'    INTO l_model.

  SELECT *
    FROM ztpp_spec
    INTO CORRESPONDING FIELDS OF TABLE it_app227
    WHERE plant IN r_plant    AND
          model LIKE l_model  AND
          opdate = p_opdate   AND
          opcount = p_opcount AND
          worder >= p_worder  AND
          extc LIKE l_extc    AND
          intc LIKE l_intc    .
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'There is no data. '.
  ENDIF.

  CLEAR p_tot_count.
  LOOP AT it_app227.
    p_tot_count = p_tot_count + it_app227-opcount.
  ENDLOOP.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_WORK_ORDER
*&---------------------------------------------------------------------*
*       Checking Work Order No. By MARA
*----------------------------------------------------------------------*
*      -->P_IT_NEW_APP227_WORDER  text
*----------------------------------------------------------------------*
FORM check_work_order_1210 USING p_worder.
  DATA p_matnr TYPE mara-matnr.

  SELECT SINGLE *
    FROM mara
    WHERE matnr = p_worder AND
          mtart = 'WOCL'     .

  IF sy-subrc <> 0.
    MOVE p_worder TO it_error_1210-forder.
    MOVE p_worder(14) TO it_error_1210-worder.
    MOVE p_worder+14(02) TO it_error_1210-extc.
    MOVE p_worder+16(02) TO it_error_1210-intc.
    MOVE p_worder TO it_error_1210-matnr.
    APPEND it_error_1210.
*    message e002 with 'THE WORK ORDER,'
*                      p_matnr
*                      ', IS NOT RESISTERED !'.
  ENDIF.

ENDFORM.                    " CHECK_WORK_ORDER
*&---------------------------------------------------------------------*
*&      Form  save_new_data
*&---------------------------------------------------------------------*
*       Creation of New Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_new_data_1210.
  DATA l_count TYPE i.
  DATA: l_tabix TYPE sy-tabix.
  LOOP AT it_new_app227.
    l_tabix = sy-tabix.
*   checking work_order_number.
    READ TABLE it_error_1210 WITH KEY forder =
                                   it_new_app227-forder.
    IF sy-subrc = 0 .
      CONTINUE.
    ENDIF.
*
    IF it_new_app227-opdate <> space  AND
       it_new_app227-opcount <> space AND
       wa_plant            <> space   AND
       wa_model            <> space   AND
       it_new_app227-forder <> space     .
      MOVE it_new_app227-forder(14) TO
        it_new_app227-worder.
      MOVE it_new_app227-forder+14(02) TO
        it_new_app227-extc.
      MOVE it_new_app227-forder+16(02) TO
        it_new_app227-intc.
      it_new_app227-mark = 'I'.
      MODIFY it_new_app227 INDEX l_tabix.

      MOVE-CORRESPONDING it_new_app227 TO ztpp_spec.
      MOVE p_keycode TO ztpp_spec-keycode.
      MOVE p_erdat TO ztpp_spec-erdat.
      MOVE p_erzet TO ztpp_spec-erzet.
      MOVE p_ernam TO ztpp_spec-ernam.
      MODIFY ztpp_spec.
*
      it_new_app227-check = 'X'.
      MODIFY it_new_app227.
      l_count = l_count + 1.
*
    ELSE.
*
      CONTINUE.
*
    ENDIF.
  ENDLOOP.

  MESSAGE i001 WITH 'The number of data created is '
                    l_count .

ENDFORM.                    " save_new_data
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       Deletion of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data_1209.
  DATA: l_wa_ztpp_spec TYPE ztpp_spec.
  LOOP AT it_app227 WHERE check = 'X'.
    MOVE-CORRESPONDING it_app227 TO l_wa_ztpp_spec.
    DELETE ztpp_spec FROM l_wa_ztpp_spec.
  ENDLOOP.
ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  release
*&---------------------------------------------------------------------*
*       Calling The Next Process
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM release_1209.
  DATA: l_return(05).

  LOOP AT it_app227 WHERE check = 'X'.
    CLEAR it_spec.
    MOVE-CORRESPONDING it_app227 TO it_spec.
    APPEND it_spec.
  ENDLOOP.
*
*
  DATA: lp_chk LIKE ztpp_spec-mark.
  RANGES: l_datum FOR sy-datum.
*
  lp_chk = 'X'.
*
  l_datum-option = 'EQ'.
  l_datum-sign = 'I'.
  l_datum-low = sy-datum.
  APPEND l_datum.
*
  EXPORT it_spec TO MEMORY ID 'SPEC'.
*
  SUBMIT zipp202i_ztppvs
       WITH p_chk EQ lp_chk
       WITH s_datum IN l_datum
       AND RETURN.
*
  l_return = sy-subrc.

ENDFORM.                    " release
*&---------------------------------------------------------------------*
*&      Form  make_file
*&---------------------------------------------------------------------*
*       Making Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_file_1209.
  CLEAR it_excel_1209.
  REFRESH it_excel_1209.
  MOVE 'KEY CODE' TO it_excel_1209-keycode.
  MOVE 'OPERATION DATE' TO it_excel_1209-opdate.
  MOVE 'OPERATION COUNT' TO it_excel_1209-opcount.
  MOVE 'MARKING' TO it_excel_1209-mark .
  MOVE 'PLANT' TO it_excel_1209-plant.
  MOVE 'MODEL CODE' TO it_excel_1209-model .
  MOVE 'WORK ORDER' TO it_excel_1209-worder .
  MOVE 'EXTERNAL COLOR' TO it_excel_1209-extc .
  MOVE 'INTERNAL COLOR' TO it_excel_1209-intc .
  MOVE 'LAST CHANGED ON' TO it_excel_1209-erdat .
  MOVE 'TIME LAST CHANGE MADE' TO it_excel_1209-erzet .
  MOVE 'PERSON CHANGING OBJ' TO it_excel_1209-ernam .
  APPEND it_excel_1209.

  LOOP AT it_app227.
    CLEAR it_excel_1209.
    MOVE-CORRESPONDING it_app227 TO it_excel_1209.
    APPEND it_excel_1209.
  ENDLOOP.

ENDFORM.                    " make_file
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       Calling a Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_1209.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      filename                = 'LIST OF WORDER.XLS'
      filetype                = 'DAT'
      item                    = ' '
      filetype_no_change      = 'X'
      filetype_no_show        = 'X'
    TABLES
      data_tab                = it_excel_1209
    EXCEPTIONS
      invalid_filesize        = 1
      invalid_table_width     = 2
      invalid_type            = 3
      no_batch                = 4
      unknown_error           = 5
      gui_refuse_filetransfer = 6
      OTHERS                  = 7.

ENDFORM.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  setting_internal_fields
*&---------------------------------------------------------------------*
*       Setting Parameters - Sub Screen
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_internal_fields_1210.
* Plant
  CLEAR : xlist, xvalue.
  name = 'WA_PLANT'.
  PERFORM set_field_plant   USING name  wa_plant   .

* Model
  CLEAR : xlist, xvalue.
  name = 'WA_MODEL'           .
  PERFORM set_field_model USING name  wa_model .
ENDFORM.                    " setting_internal_fields
*&---------------------------------------------------------------------*
*&      Form  SORT_ASCENDING
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_ascending_1209.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP227'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app227 ASCENDING BY (field_name01).
  ENDIF.
*
ENDFORM.                    " SORT_ASCENDING
*&---------------------------------------------------------------------*
*&      Form  SORT_DESCENDING
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_descending_1209.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP227'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app227 DESCENDING BY (field_name01).
  ENDIF.
ENDFORM.                    " SORT_DESCENDING

*&---------------------------------------------------------------------*
*&      Form  NATION_DATA_SAVE
*&---------------------------------------------------------------------*
*       Updating Data with toggle( C: Change, A: Insert)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nation_data_save_1203.
  IF wa_change = 'X'  AND wa_edit = 'X' AND wa_insert IS INITIAL.
    UPDATE  ztpp_nation_def  SET: name      =  is_app221-name
                                  dealer    =  is_app221-dealer
                                  address1  =  is_app221-address1
                                  address2  =  is_app221-address2
                                  address3  =  is_app221-address3
                                  drive     =  is_app221-drive
                                  weather   =  is_app221-weather
                                  region    =  is_app221-region
                                  spec      =  is_app221-spec
                                  port      =  is_app221-port
                                  portname  =  is_app221-portname
                                  langu     =  is_app221-langu
                                  warranty  =  is_app221-warranty
                                 anti_rust =  is_app221-anti_rust
                                  emission  =  is_app221-emission
                                  em_rate   =  is_app221-em_rate
                                  n_code    =  is_app221-n_code
                                  aedat     =  sy-datum
                                  aezet     =  sy-uzeit
                                  aenam     =  sy-uname
            WHERE   nation EQ  is_app221-nation.

    IF sy-subrc  EQ  0.
      MESSAGE s000  WITH  'Data Change Succesful !!'.
      PERFORM  it_app221_modyfy.
    ELSE.
      MESSAGE e003  WITH  is_app221-nation 'Data Change Fail'.
    ENDIF.
  ENDIF.

  IF wa_insert = 'X' AND wa_edit = 'X'.
    CLEAR  ztpp_nation_def.
    MOVE-CORRESPONDING  is_app221  TO  ztpp_nation_def.
    MOVE   sy-datum                TO  ztpp_nation_def-erdat.
    MOVE   sy-uzeit                TO  ztpp_nation_def-erzet.
    MOVE   sy-uname                TO  ztpp_nation_def-ernam.

    INSERT INTO ztpp_nation_def VALUES ztpp_nation_def .
    CASE  sy-subrc.
      WHEN   0.
        PERFORM  it_app221_append.
        MESSAGE s000  WITH  'Data Insert Succesful !!'.
      WHEN   4.
        MESSAGE e000  WITH  'Dupplicate Nation code!'.
      WHEN   OTHERS.
        MESSAGE e003  WITH  is_app221-nation 'Data Change Fail'.
    ENDCASE.
  ENDIF.
  CLEAR: wa_edit, wa_insert.
ENDFORM.                    " NATION_DATA_SAVE
*&---------------------------------------------------------------------*
*&      Form  nation_display
*&---------------------------------------------------------------------*
*       Searching Data & Handling Error
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nation_display_1203.
  DATA: l_nation  LIKE  ztpp_nation_def-nation.

  IF wa_edit = 'X' OR wa_insert = 'X'.
    PERFORM process_confirm_1203 .
  ENDIF.

  SELECT  SINGLE * FROM  ztpp_nation_def
    INTO CORRESPONDING FIELDS OF is_app221
    WHERE nation = is_app221-nation.

  IF  sy-subrc NE  0.
    l_nation = is_app221-nation.
    CLEAR is_app221.
    is_app221-nation = l_nation.
    MESSAGE s000 WITH 'It is country code that is not registered.'.
  ENDIF.
ENDFORM.                    " nation_display
*&---------------------------------------------------------------------*
*&      Form  nation_data_insert
*&---------------------------------------------------------------------*
*       Setting Toggle's Mode For New Data Creation
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nation_data_insert_1203.
  CLEAR  is_app221.
  wa_insert = 'X' .       "insert mode
ENDFORM.                    " nation_data_insert

*&---------------------------------------------------------------------*
*&      Form  IT_APP221_MODYFY
*&---------------------------------------------------------------------*
*       Modification of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_app221_modyfy.
  READ TABLE it_app221  WITH KEY  nation = is_app221-nation.
  CHECK sy-subrc EQ 0.
  MOVE-CORRESPONDING is_app221  TO  it_app221.
  MODIFY  it_app221  INDEX  sy-tabix.
ENDFORM.                    " IT_APP221_MODYFY

*&---------------------------------------------------------------------*
*&      Form  IT_APP221_APPEND
*&---------------------------------------------------------------------*
*       Setting Screen's Data with Internal Table.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_app221_append.
  CLEAR  it_app221.
  MOVE-CORRESPONDING  is_app221  TO  it_app221.
  APPEND  it_app221.

  SORT  it_app221  BY  nation.
  LOOP AT  it_app221.
    MOVE  sy-tabix   TO  it_app221-seq.
    MODIFY it_app221.
  ENDLOOP.

  DESCRIBE TABLE it_app221  LINES  g_seq_1203.

  READ TABLE it_app221  WITH KEY nation = is_app221-nation.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING   it_app221   TO   is_app221.
  ENDIF.
ENDFORM.                    " IT_APP221_APPEND

*&---------------------------------------------------------------------*
*&      Form  transport_to_alc
*&---------------------------------------------------------------------*
FORM transport_to_alc_1203.
  DATA l_msgtxt(100).
  REFRESH it_zsppvn1. CLEAR it_zsppvn1.

  MOVE-CORRESPONDING is_app221   TO  it_zsppvn1.
  it_zsppvn1-zsdat = sy-datum.
  it_zsppvn1-modi_date = sy-datum.
  it_zsppvn1-zstim = sy-uzeit.
  APPEND it_zsppvn1. CLEAR it_zsppvn1.

  CALL FUNCTION 'Z_FPP_SET_ZTPPVN1'
    DESTINATION c_dest
    TABLES
      i_zsppvn1             = it_zsppvn1
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc = 0.
    MESSAGE s001 WITH 'Completed Update Successfully'.
    MODIFY ztppvn1 FROM TABLE it_zsppvn1.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    MESSAGE e001 WITH l_msgtxt.
  ENDIF.
ENDFORM.                    " transport_to_alc
*&---------------------------------------------------------------------*
*&      Form  next_nation_display
*&---------------------------------------------------------------------*
*       Displaying The Next Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM next_nation_display_1203.
  DATA: l_seq  TYPE  i.

  IF wa_edit = 'X'  OR wa_insert = 'X'.
    PERFORM process_confirm_1203 .
  ENDIF.

  PERFORM clear_structure_app221.
  READ  TABLE  it_app221  WITH KEY nation = is_app221-nation.

  CHECK sy-subrc EQ 0.
  MOVE  it_app221-seq  TO  l_seq.
  l_seq = l_seq + 1.
  IF l_seq  >  g_seq_1203.
    l_seq = g_seq_1203.
    MESSAGE s000 WITH 'Last Nation Code.'.
  ENDIF.
  READ  TABLE  it_app221  WITH KEY seq = l_seq.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING  it_app221   TO  is_app221.
  ENDIF.
ENDFORM.                    " next_nation_display
*&---------------------------------------------------------------------*
*&      Form  prev_nation_display
*&---------------------------------------------------------------------*
*       Displaying The Previous Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prev_nation_display_1203.
  DATA: l_seq  TYPE  i.

  IF wa_edit = 'X'  OR wa_insert = 'X'.
    PERFORM process_confirm_1203 .
  ENDIF.

  PERFORM clear_structure_app221.
  READ  TABLE  it_app221  WITH KEY nation = is_app221-nation.

  CHECK sy-subrc EQ 0.
  MOVE  it_app221-seq  TO  l_seq.
  l_seq = l_seq - 1.
  IF l_seq  <   1.
    l_seq =  1.
    MESSAGE s000 WITH 'First Nation Code.'.
  ENDIF.
  READ  TABLE  it_app221  WITH KEY seq = l_seq.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING  it_app221   TO  is_app221.
  ENDIF.
ENDFORM.                    " prev_nation_display

*&---------------------------------------------------------------------*
*&      Form  READ_TODAY_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_today_status    TABLES pa_data   STRUCTURE it_app302 .
  DATA: lt_ausp           LIKE TABLE OF ausp   WITH HEADER LINE,
        l_num(8)          TYPE n         ,
        l_size            TYPE i         ,
        l_atflv           LIKE ausp-atflv,
        l_worder          LIKE ausp-atinn,
        l_atinn           LIKE ausp-atinn,
        l_atinn2          LIKE ausp-atinn.

  l_atflv = l_num = sy-datum .
  PERFORM read_atinn  USING 'P_RP06_SHOP_DATE'  l_atinn .
  PERFORM read_atinn  USING 'P_WORK_ORDER'      l_atinn2.
  SELECT *  INTO TABLE lt_ausp
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP WHERE atinn = l_atinn
                                             AND klart = '002'
                                            AND atflv = l_atflv )
     AND atinn = l_atinn2
     AND klart = '002'   .

  SORT lt_ausp BY atwrt.
  READ TABLE lt_ausp INDEX 1.
  pa_data-worder = lt_ausp-atwrt.
  LOOP AT lt_ausp      .
    IF pa_data-worder = lt_ausp-atwrt.
      pa_data-tot_p = pa_data-tot_p + 1 .
    ELSE.
      APPEND pa_data.   CLEAR: pa_data.
      pa_data-worder = lt_ausp-atwrt.
      pa_data-tot_p = 1 .
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE lt_ausp LINES l_size.
  IF l_size > 0.
    APPEND pa_data.   CLEAR: pa_data.
  ENDIF.
ENDFORM.                    " READ_TODAY_STATUS

*&---------------------------------------------------------------------*
*&      Form  CONCATE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM concate_result         TABLES pa_data   STRUCTURE it_app302 .
  DATA: l_atinn             LIKE   ausp-atinn,
        l_atwrt             LIKE   ausp-atwrt,
        l_seq               TYPE   i,
        l_char(30)          TYPE   c.

  " Delete the Value (BIWP/BIW)
  DELETE it_app302 WHERE alc_vals = 'BIWP' OR alc_vals = 'BIW' .
  LOOP AT it_app302.
    CONCATENATE 'P_ALC_' it_app302-alc_code(1) '_'
                it_app302-alc_code+1(3)            INTO l_char .
    PERFORM read_atinn   USING l_char  l_atinn .
    LOOP AT pa_data.
      SELECT SINGLE atwrt INTO l_atwrt
        FROM ausp
       WHERE objek = pa_data-worder
         AND atinn = l_atinn
         AND klart = '001'
         AND atwrt = it_app302-alc_vals .
      IF sy-subrc = 0.
*       it_app302-today = it_app302-today + pa_data-tot_p .
      ENDIF.
    ENDLOOP.
    it_app302-order = l_seq  =  l_seq + 1 .
    MODIFY it_app302 .
  ENDLOOP.
ENDFORM.                    " CONCATE_RESULT

*&---------------------------------------------------------------------*
*&      Form  get_data_from_mm03_3107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_mm03_3107.
  DATA: l_int      TYPE i,
        l_char(30) , l_wono(18) .

  LOOP AT it_3107.
    CONCATENATE it_3107-order  it_3107-extc  it_3107-intc
                INTO l_wono .

*   ocn(P_OCN)
    PERFORM search_data_from_class_3107 USING    l_wono
                                                 'P_OCN'
                                        CHANGING it_3107-ocn.
*   ver(P_VERSION)
    PERFORM search_data_from_class_3107 USING    l_wono
                                                 'P_VERSION'
                                        CHANGING it_3107-ver.
*   mi(P_MI)
    PERFORM search_data_from_class_3107 USING    l_wono
                                                 'P_MI'
                                        CHANGING it_3107-mi.
*   VIN(P_VIN_SPEC) --> Search in the WOHD
    PERFORM search_data_from_class_3107 USING    l_wono(14)
                                                 'P_VIN_SPEC'
                                        CHANGING it_3107-vin .
*   cod1
    st_3107_input-cola = l_int = st_3107_input-cola.
    CONDENSE  st_3107_input-cola .
    CONCATENATE st_3107_input-tablea st_3107_input-cola
             INTO l_char .
    PERFORM search_code_3107 USING    l_wono  l_char  'A'
                             CHANGING it_3107-cod1       .
*   cod2
    st_3107_input-colb = l_int = st_3107_input-colb.
    CONDENSE  st_3107_input-colb .
    CONCATENATE st_3107_input-tableb st_3107_input-colb
      INTO l_char .
    PERFORM search_code_3107 USING    l_wono  l_char  'B'
                             CHANGING it_3107-cod2       .
    MODIFY it_3107.
  ENDLOOP.
ENDFORM.                    " get_data_from_mm03_3107

*&---------------------------------------------------------------------*
*&      Form  search_data_from_class_3107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WONO  text
*      -->P_5227   text
*      <--P_IT_3107_OCN  text
*----------------------------------------------------------------------*
FORM search_data_from_class_3107 USING    p_wono   p_atnam
                                 CHANGING p_atwrt.
  SELECT SINGLE au~atwrt
    INTO p_atwrt
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE au~klart = '001'   AND
          au~objek = p_wono  AND
          ca~atnam = p_atnam   .
ENDFORM.                    " search_data_from_class_3107

*&---------------------------------------------------------------------*
*&      Form  search_code_3107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_3107_ORDER  text
*      -->P_IT_3107_EXTC  text
*      -->P_IT_3107_INTC  text
*      -->P_ST_3107_INPUT_TABLEA  text
*      -->P_ST_3107_INPUT_COLA  text
*      <--P_IT_3107_COD1  text
*----------------------------------------------------------------------*
FORM search_code_3107 USING    p_worder  p_char  p_type
                      CHANGING p_code  .
  DATA: l_num(03) TYPE n,
        l_type,
        l_char(03),
        l_atnam TYPE cabn-atnam,
        l_objek TYPE ausp-objek.

  CASE p_type.
    WHEN 'A' .
      l_type = p_char+6(1).
      CASE l_type      .
        WHEN 'U'       .
          l_objek = p_worder(14) .
        WHEN 'C'       .
          l_objek = p_worder     .
      ENDCASE.
    WHEN 'B' .
      l_type = p_char+9(1).
      CASE l_type      .
        WHEN 'P'       .
          l_objek = p_worder(14) .
        WHEN 'B'       .
          l_objek = p_worder(14) .
        WHEN 'Q'       .
          l_objek = p_worder     .
      ENDCASE.
  ENDCASE.
  PERFORM search_data_from_class_3107 USING    l_objek  p_char
                                      CHANGING p_code  .
ENDFORM.                    " search_code_3107

*&---------------------------------------------------------------------*
*&      Form  check_essential_prmtr_3107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TEXT  text
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
FORM check_essential_prmtr_3107 USING    p_text  p_flag.

  IF st_3107_input-tablea = space.
    p_text = 'Table(A)'.
    p_flag = 'X'.
    EXIT.
  ENDIF.
  IF st_3107_input-tableb = space.
    p_text = 'Table(B)'.
    p_flag = 'X'.
    EXIT.
  ENDIF.
  IF st_3107_input-cola = space.
    p_text = 'Column(A)'.
    p_flag = 'X'.
    EXIT.
  ENDIF.
  IF st_3107_input-colb = space.
    p_text = 'Column(B)'.
    p_flag = 'X'.
    EXIT.
  ENDIF.
ENDFORM.                    " check_essential_prmtr_3107

*&---------------------------------------------------------------------*
*&      Form  read_6gb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_WOSUM_WO_SER  text
*      -->P_LT_WOSUM_EXTC  text
*      -->P_LT_WOSUM_INTC  text
*----------------------------------------------------------------------*
FORM read_6gb USING    pa_ordr  pa_extc  pa_intc .
  SELECT SUM( bukt01 ) SUM( bukt02 ) SUM( bukt03 ) SUM( bukt04 )
         SUM( bukt05 ) SUM( bukt06 ) SUM( bukt07 ) SUM( bukt08 )
         SUM( bukt09 ) SUM( bukt10 ) SUM( bukt11 ) SUM( bukt12 )
         SUM( bukt13 ) SUM( bukt14 )
   INTO (it_app272_01-p008, it_app272_01-p009, it_app272_01-p010,
         it_app272_01-p011, it_app272_01-p012, it_app272_01-p013,
         it_app272_01-p014, it_app272_01-p015, it_app272_01-p016,
         it_app272_01-p017, it_app272_01-p018, it_app272_01-p019,
         it_app272_01-p020, it_app272_01-p021)
    FROM ztpp_pmt06gb
   WHERE gubb = 'B'
     AND ordr = pa_ordr
     AND extc = pa_extc
     AND intc = pa_intc  .
ENDFORM.                                                    " read_6gb

*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATNR_HD  text
*      -->P_L_MATNR_CL  text
*      -->P_L_CHK  text
*----------------------------------------------------------------------*
FORM check_input_vals USING    pa_wohd  pa_wocl  pa_err.

ENDFORM.                    " CHECK_INPUT_VALS
*&---------------------------------------------------------------------*
*&      Form  download_0105
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_0105.
  CLEAR: it_excel000, it_excel000[].
  MOVE: 'Company'  TO it_excel000-col01,
        'Order'    TO it_excel000-col02,
        'Car Type' TO it_excel000-col03,
        'MI'       TO it_excel000-col04,
        'OCN'      TO it_excel000-col05,
        'Version'  TO it_excel000-col06.
  APPEND it_excel000.
  MOVE: c_company  TO it_excel000-col01,
        wa_order   TO it_excel000-col02,
        wa_car     TO it_excel000-col03,
        wa_mi      TO it_excel000-col04,
        wa_ocn     TO it_excel000-col05,
        wa_version TO it_excel000-col06.
  APPEND it_excel000.

  CLEAR it_excel000.
  APPEND: it_excel000, it_excel000.
  MOVE: 'Column' TO it_excel000-col01,
        'Code'   TO it_excel000-col02.
  APPEND it_excel000.

  LOOP AT it_alcu_a.
    CLEAR it_excel000.
    MOVE: it_alcu_a-clm  TO it_excel000-col01,
          it_alcu_a-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  LOOP AT it_alcu_b.
    CLEAR it_excel000.
    MOVE: it_alcu_b-clm  TO it_excel000-col01,
          it_alcu_b-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  LOOP AT it_alcu_c.
    CLEAR it_excel000.
    MOVE: it_alcu_c-clm  TO it_excel000-col01,
          it_alcu_c-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  LOOP AT it_alcu_d.
    CLEAR it_excel000.
    MOVE: it_alcu_d-clm  TO it_excel000-col01,
          it_alcu_d-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  PERFORM call_func_for_download TABLES it_excel000
                          USING  'ALC Code For Unique Part.XLS'.

ENDFORM.                    " download_0105
*&---------------------------------------------------------------------*
*&      Form  call_func_for_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL000  text
*----------------------------------------------------------------------*
FORM call_func_for_download TABLES pt_excel STRUCTURE it_excel000
                            USING  p_file_name.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      filename                = p_file_name
      filetype                = 'DAT'
      item                    = ' '
      filetype_no_change      = 'X'
      filetype_no_show        = 'X'
    TABLES
      data_tab                = pt_excel
    EXCEPTIONS
      invalid_filesize        = 1
      invalid_table_width     = 2
      invalid_type            = 3
      no_batch                = 4
      unknown_error           = 5
      gui_refuse_filetransfer = 6
      OTHERS                  = 7.


ENDFORM.                    " call_func_for_download
*&---------------------------------------------------------------------*
*&      Form  download_0106
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_0106.
  CLEAR: it_excel000, it_excel000[].
  MOVE: 'Company'  TO it_excel000-col01,
        'Order'    TO it_excel000-col02,
        'Car Type' TO it_excel000-col03,
        'MI'       TO it_excel000-col04,
        'OCN'      TO it_excel000-col05.
  APPEND it_excel000.
  CLEAR it_excel000.
  MOVE: c_company TO it_excel000-col01,
        wa_order  TO it_excel000-col02,
        wa_car    TO it_excel000-col03,
        wa_mi     TO it_excel000-col04,
        wa_ocn    TO it_excel000-col05.
  APPEND it_excel000.
  CLEAR it_excel000.
  APPEND: it_excel000.

  CLEAR it_excel000.
  MOVE: '219'      TO it_excel000-col01,
        wa_219-i01 TO it_excel000-col02,
        wa_219-i02 TO it_excel000-col03,
        wa_219-i03 TO it_excel000-col04,
        wa_219-i04 TO it_excel000-col05,
        wa_219-i05 TO it_excel000-col06,
        wa_219-i06 TO it_excel000-col07,
        wa_219-i07 TO it_excel000-col08,
        wa_219-i08 TO it_excel000-col09,
        wa_219-i09 TO it_excel000-col10,
        wa_219-i10 TO it_excel000-col11,
        wa_219-i11 TO it_excel000-col12,
        wa_219-i12 TO it_excel000-col13,
        wa_219-i13 TO it_excel000-col14,
        wa_219-i14 TO it_excel000-col15,
        wa_219-i15 TO it_excel000-col16,
        wa_219-i16 TO it_excel000-col17,
        wa_219-i17 TO it_excel000-col18,
        wa_219-i18 TO it_excel000-col19,
        wa_219-i19 TO it_excel000-col20,
        wa_219-i20 TO it_excel000-col21.
  APPEND it_excel000.
  CLEAR it_excel000.
  MOVE: 'Option'   TO it_excel000-col01,
        wa_219-o01 TO it_excel000-col02,
        wa_219-o02 TO it_excel000-col03,
        wa_219-o03 TO it_excel000-col04,
        wa_219-o04 TO it_excel000-col05,
        wa_219-o05 TO it_excel000-col06,
        wa_219-o06 TO it_excel000-col07,
        wa_219-o07 TO it_excel000-col08,
        wa_219-o08 TO it_excel000-col09,
        wa_219-o09 TO it_excel000-col10,
        wa_219-o10 TO it_excel000-col11,
        wa_219-o11 TO it_excel000-col12,
        wa_219-o12 TO it_excel000-col13,
        wa_219-o13 TO it_excel000-col14,
        wa_219-o14 TO it_excel000-col15,
        wa_219-o15 TO it_excel000-col16,
        wa_219-o16 TO it_excel000-col17,
        wa_219-o17 TO it_excel000-col18,
        wa_219-o18 TO it_excel000-col19,
        wa_219-o19 TO it_excel000-col20,
        wa_219-o20 TO it_excel000-col21.
  APPEND it_excel000.

  CLEAR it_excel000.
  APPEND: it_excel000, it_excel000.
  MOVE: 'Column' TO it_excel000-col01,
        'Code'   TO it_excel000-col02.
  APPEND it_excel000.

  LOOP AT it_alcu_a.
    CLEAR it_excel000.
    MOVE: it_alcu_a-clm  TO it_excel000-col01,
          it_alcu_a-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  LOOP AT it_alcu_b.
    CLEAR it_excel000.
    MOVE: it_alcu_b-clm  TO it_excel000-col01,
          it_alcu_b-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  LOOP AT it_alcu_c.
    CLEAR it_excel000.
    MOVE: it_alcu_c-clm  TO it_excel000-col01,
          it_alcu_c-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  LOOP AT it_alcu_d.
    CLEAR it_excel000.
    MOVE: it_alcu_d-clm  TO it_excel000-col01,
          it_alcu_d-vals TO it_excel000-col02.
    APPEND it_excel000.
  ENDLOOP.

  PERFORM call_func_for_download TABLES it_excel000
            USING  'ALC Unique Management2.XLS'.

ENDFORM.                    " download_0106
*&---------------------------------------------------------------------*
*&      Form  download_0107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_0107.
  CLEAR: it_excel000, it_excel000[].
  MOVE: 'Company'  TO it_excel000-col01,
        'Car Type' TO it_excel000-col02,
        'W/O Pack' TO it_excel000-col03.
  APPEND it_excel000.
  CLEAR it_excel000.
  APPEND it_excel000.

  MOVE: 'W/O Creating Date'   TO it_excel000-col01,
        'Error Occursed Date' TO it_excel000-col02,
        'Factory'             TO it_excel000-col03,
        'Model Index'         TO it_excel000-col04,
        'OCN'                 TO it_excel000-col05,
        'Destination Country' TO it_excel000-col06,
        'L/C'                 TO it_excel000-col07,
        'VIN'                 TO it_excel000-col08,
        'Init.QTY'            TO it_excel000-col09,
        'Midified QTY'        TO it_excel000-col10,
        'Error Total'         TO it_excel000-col11.
  APPEND it_excel000.
  CLEAR it_excel000.
  MOVE: wa_woc_date         TO it_excel000-col01,
        wa_wom_date         TO it_excel000-col02,
        wa_trim_plant_no    TO it_excel000-col03,
        wa_mi               TO it_excel000-col04,
        wa_ocn              TO it_excel000-col05,
        wa_destination_code TO it_excel000-col06,
        wa_lc_no            TO it_excel000-col07,
        wa_vin_spec         TO it_excel000-col08,
        wa_init_qty         TO it_excel000-col09,
        wa_mod_qty          TO it_excel000-col10,
        wa_totqty           TO it_excel000-col11.
  APPEND it_excel000.
  CLEAR it_excel000.
  APPEND: it_excel000, it_excel000.

  MOVE: 'W/O Color'   TO it_excel000-col01,
        'Col'         TO it_excel000-col02,
        'Column'      TO it_excel000-col03,
        'Description' TO it_excel000-col04.
  APPEND it_excel000.
  LOOP AT it_err.
    CLEAR it_excel000.
    MOVE: it_err-objkey TO it_excel000-col01,
          it_err-col    TO it_excel000-col02,
          it_err-colnm  TO it_excel000-col03,
          it_err-coldc  TO it_excel000-col04.
    APPEND it_excel000.
  ENDLOOP.
  CLEAR it_excel000.
  APPEND: it_excel000, it_excel000.

  MOVE: 'Column' TO it_excel000-col01,
        'Option' TO it_excel000-col02,
        'Option Name' TO it_excel000-col03,
        'Column Name' TO it_excel000-col04.
  APPEND it_excel000.
  LOOP AT it_219.
    CLEAR it_excel000.
    MOVE: it_219-no      TO it_excel000-col01,
          it_219-219vals TO it_excel000-col02,
          it_219-219code TO it_excel000-col03,
          it_219-219desc TO it_excel000-col04.
    APPEND it_excel000.
  ENDLOOP.

  PERFORM call_func_for_download TABLES it_excel000
            USING  'Error Work Order Management.XLS'.

ENDFORM.                    " download_0107
*&---------------------------------------------------------------------*
*&      Form  download_0109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_0109.
  CLEAR: it_excel000, it_excel000[].
  MOVE: 'Model'        TO it_excel000-col01,
        'Usage'        TO it_excel000-col02,
        'Monthly Pack' TO it_excel000-col03,
        'Nation'       TO it_excel000-col04,
        'Order Status' TO it_excel000-col05,
        'Sum QTY'      TO it_excel000-col06.
  APPEND it_excel000.
  CLEAR it_excel000.
  MOVE: wa_model             TO it_excel000-col01,
        st_0109_input-use    TO it_excel000-col02,
        st_0109_input-monty  TO it_excel000-col03,
        st_0109_input-nation TO it_excel000-col04,
        st_0109_input-full   TO it_excel000-col05,
        st_0109_input-qty    TO it_excel000-col06.
  APPEND it_excel000.
  CLEAR it_excel000.
  APPEND it_excel000.

  MOVE: '219 Options'          TO it_excel000-col01,
        st_0109_input-219_1    TO it_excel000-col02,
        st_0109_input-219_1_v  TO it_excel000-col03,
        st_0109_input-219_2    TO it_excel000-col04,
        st_0109_input-219_2_v  TO it_excel000-col05,
        st_0109_input-219_3    TO it_excel000-col06,
        st_0109_input-219_3_v  TO it_excel000-col07,
        st_0109_input-219_4    TO it_excel000-col08,
        st_0109_input-219_4_v  TO it_excel000-col09,
        st_0109_input-219_5    TO it_excel000-col10,
        st_0109_input-219_5_v  TO it_excel000-col11,
        st_0109_input-219_6    TO it_excel000-col12,
        st_0109_input-219_6_v  TO it_excel000-col13,
        st_0109_input-219_7    TO it_excel000-col14,
        st_0109_input-219_7_v  TO it_excel000-col15,
        st_0109_input-219_8    TO it_excel000-col16,
        st_0109_input-219_8_v  TO it_excel000-col17,
        st_0109_input-219_9    TO it_excel000-col18,
        st_0109_input-219_9_v  TO it_excel000-col19,
        st_0109_input-219_10   TO it_excel000-col20,
        st_0109_input-219_10_v TO it_excel000-col21.
  APPEND it_excel000.
  CLEAR it_excel000.
  APPEND it_excel000.

  MOVE: 'Nation'   TO it_excel000-col01,
        'Order-No' TO it_excel000-col02,
        'QTY'      TO it_excel000-col03,
        'Plant'    TO it_excel000-col04,
        'MI'       TO it_excel000-col05,
        'OCN'      TO it_excel000-col06,
        'Version'  TO it_excel000-col07,
        'Gen.Date' TO it_excel000-col08,
        'ALC Date' TO it_excel000-col09,
        'Flag'     TO it_excel000-col10,
        '2'        TO it_excel000-col11,
        '1'        TO it_excel000-col12,
        '9'        TO it_excel000-col13,
        'O'        TO it_excel000-col14,
        'P'        TO it_excel000-col15,
        'T'        TO it_excel000-col16,
        'I'        TO it_excel000-col17,
        'O'        TO it_excel000-col18,
        'N'        TO it_excel000-col19,
        'S'        TO it_excel000-col20.
  APPEND it_excel000.
  LOOP AT it_0109.
    CLEAR it_excel000.
    MOVE: it_0109-nation TO it_excel000-col01,
          it_0109-order  TO it_excel000-col02,
          it_0109-modqty TO it_excel000-col03,
          it_0109-plant  TO it_excel000-col04,
          it_0109-mi     TO it_excel000-col05,
          it_0109-ocn    TO it_excel000-col06,
          it_0109-ver    TO it_excel000-col07,
          it_0109-g_date TO it_excel000-col08,
          it_0109-a_date TO it_excel000-col09,
          it_0109-flag   TO it_excel000-col10,
          it_0109-219_1  TO it_excel000-col11,
          it_0109-219_2  TO it_excel000-col12,
          it_0109-219_3  TO it_excel000-col13,
          it_0109-219_4  TO it_excel000-col14,
          it_0109-219_5  TO it_excel000-col15,
          it_0109-219_6  TO it_excel000-col16,
          it_0109-219_7  TO it_excel000-col17,
          it_0109-219_8  TO it_excel000-col18,
          it_0109-219_9  TO it_excel000-col19,
          it_0109-219_10 TO it_excel000-col20.
    APPEND it_excel000.
  ENDLOOP.

  PERFORM call_func_for_download TABLES it_excel000
            USING  'Order Status By Model.XLS'.

ENDFORM.                    " download_0109
*&---------------------------------------------------------------------*
*&      Form  download_0110
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_0110.
  CLEAR: it_excel000, it_excel000[].
  MOVE: 'Order No.'    TO it_excel000-col01,
        'Ext.C'        TO it_excel000-col02,
        'From Status'  TO it_excel000-col03,
        'To Status'    TO it_excel000-col04,
        'MI'           TO it_excel000-col05,
        'OCN'          TO it_excel000-col06,
        'Version'      TO it_excel000-col07,
        'Option(1~20)' TO it_excel000-col08,
        'Count'        TO it_excel000-col09.
  APPEND it_excel000.
  CLEAR it_excel000.
  MOVE: st_0110_input-order   TO it_excel000-col01,
        st_0110_input-exclr   TO it_excel000-col02,
        st_0110_input-status  TO it_excel000-col03,
        st_0110_input-status2 TO it_excel000-col04,
        st_0110_input-mi      TO it_excel000-col05,
        st_0110_input-ocn     TO it_excel000-col06,
        st_0110_input-ver     TO it_excel000-col07,
        st_0110_input-tbd     TO it_excel000-col08,
        st_0110_input-count   TO it_excel000-col09.
  APPEND it_excel000.
  CLEAR it_excel000.
  APPEND it_excel000.

  MOVE: 'Ext.C'       TO it_excel000-col01,
        'Int.C'       TO it_excel000-col02,
        'Body No.'    TO it_excel000-col03,
        'MITU'        TO it_excel000-col04,
        'Status'      TO it_excel000-col05,
        'RP Date'     TO it_excel000-col06,
        'Engine No.'  TO it_excel000-col07,
        'Destination' TO it_excel000-col08,
        'Plan Order'  TO it_excel000-col09,
        'Sales Order' TO it_excel000-col10.
  APPEND it_excel000.
  LOOP AT it_0110.
    MOVE: it_0110-exclr  TO it_excel000-col01,
          it_0110-inclr  TO it_excel000-col02,
          it_0110-body   TO it_excel000-col03,
          it_0110-mitu   TO it_excel000-col04,
          it_0110-status TO it_excel000-col05,
          it_0110-rdate  TO it_excel000-col06,
          it_0110-engine TO it_excel000-col07,
          it_0110-nation TO it_excel000-col08,
          it_0110-porder TO it_excel000-col09,
          it_0110-sales  TO it_excel000-col10.
    APPEND it_excel000.
  ENDLOOP.
ENDFORM.                    " download_0110

*&---------------------------------------------------------------------*
*&      Form  READ_TABLE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PART_APP302  text
*      -->P_P_COLUMN_APP302  text
*      -->P_P_COL_NAME_APP302  text
*----------------------------------------------------------------------*
FORM read_table_text USING  pa_model  pa_type  pa_num  pa_name .
  DATA: l_table      LIKE cuvtab-vtnam   ,
        l_vtint      LIKE cuvtab-vtint   ,
        l_num(3)     TYPE c              ,
        l_int        TYPE i              ,
        l_name       LIKE cuvtab_tx-vttxt.

* MOVE pA_NUM TO L_NUM.
  l_num = l_int = pa_num .
  CONDENSE l_num         .
  CONCATENATE pa_model '_ALC_' pa_type '_' l_num INTO l_table.

  SELECT SINGLE vtint INTO l_vtint
    FROM cuvtab
   WHERE vtnam = l_table.

  SELECT SINGLE vttxt INTO l_name
    FROM cuvtab_tx
   WHERE vtint = l_vtint
     AND spras = sy-langu.

  pa_name = l_name.
ENDFORM.                    " READ_TABLE_TEXT

*&---------------------------------------------------------------------*
*&      Form  SET_LIST_USAGE-CAR
*&---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_list_usagecar USING pa_name  pa_parameter .
  DATA: l_atinn              LIKE cabn-atinn,
        l_equnr              LIKE equi-equnr,
        l_atnam              LIKE cabn-atnam,
        l_atwrt              LIKE ausp-atwrt,
        l_atwtb              LIKE cawnt-atwtb.

  CLEAR : xlist[],xvalue.

  PERFORM read_atinn  USING 'P_USAGE_CAR'  l_atinn.

  SELECT n~atwrt t~atwtb INTO (l_atwrt, l_atwtb)
    FROM cawn AS n INNER JOIN cawnt AS t
      ON n~atinn = t~atinn
     AND n~atzhl = t~atzhl
   WHERE n~atinn = l_atinn
     AND t~spras = sy-langu .
    xvalue-text = l_atwtb.
    xvalue-key  = l_atwrt.
    APPEND xvalue TO xlist .
  ENDSELECT.

* Delete the List Item..
  IF sy-dynnr = '2206'.
    xvalue-key  = 'T' .
    DELETE xlist WHERE key = 'T' .
  ENDIF.

* LIST BOX SETTING
  PERFORM list_box_function USING pa_name.
  " Current Status Check...
  PERFORM read_atinn  USING 'P_RP_STATUS'    l_atinn.
  CONCATENATE wa_model  st_2204_input-body  INTO l_equnr.
  SELECT SINGLE atwrt INTO l_atwrt
    FROM ausp
   WHERE objek = l_equnr
     AND atinn = l_atinn
     AND klart = '002'  .
  IF l_atwrt > '17'.
    pa_parameter = 'D' .
  ELSE.  " Initial..
    pa_parameter = 'S' .
  ENDIF.
ENDFORM.                    " SET_LIST_USAGECAR

*&---------------------------------------------------------------------*
*&      Form  SET_LIST_CHAR
*&---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_list_char  USING pa_char pa_name  pa_parameter .
  DATA: l_atinn              LIKE cabn-atinn,
        l_atnam              LIKE cabn-atnam,
        l_atwrt              LIKE ausp-atwrt,
        l_atwtb              LIKE cawnt-atwtb.

  CLEAR : xlist[],xvalue.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = pa_char .

  SELECT n~atwrt t~atwtb INTO (l_atwrt, l_atwtb)
    FROM cawn AS n INNER JOIN cawnt AS t
      ON n~atinn = t~atinn
     AND n~atzhl = t~atzhl
   WHERE n~atinn = l_atinn
     AND t~spras = sy-langu .
    xvalue-text = l_atwtb.
    xvalue-key  = l_atwrt.
    APPEND xvalue TO xlist .
  ENDSELECT.

* LIST BOX SETTING
  PERFORM list_box_function USING pa_name.
  IF pa_parameter IS INITIAL.
    READ TABLE xlist INTO xvalue  INDEX 1.
    pa_parameter = xvalue-key.
  ENDIF.
ENDFORM.                    " set_LIST_CHAR
*&---------------------------------------------------------------------*
*&      Form  read_break
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CAPA_TPROG  text
*----------------------------------------------------------------------*
FORM read_break USING p_shift   p_tprog.

** Furong on 06/12/12 for 3 shift - breaktime not count
*  DATA: lt_break LIKE it_break OCCURS 5 WITH HEADER LINE.
*
*  SELECT * INTO TABLE lt_break
*    FROM tc37p
*    WHERE schgrup = 'HA'
*      AND paplan   = p_tprog.
*  check sy-subrc = 0.
*  SORT lt_break BY padauer DESCENDING.
*  READ TABLE lt_break INDEX 1.
*  lt_break-paunr = p_shift.
** if the time is less than 30 minutes, ignore
** the break.
*  if lt_break-PADAUER ge 1800.
*     APPEND lt_break TO it_break.
*  endif .
** End on 06/12/12

ENDFORM.                    " read_break
