*----------------------------------------------------------------------*
***INCLUDE MZPP_APPLICATIONF05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CLEAR_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_tables.
  CLEAR: cuxref, crtx,  cabn,  cawn,  cuvtln,  cuvtab, cuvtab_valc,
         mara,   ausp,  equi,  cuvtab_tx,      mast,   cuvtab_valn,
         mkal,   image_control_created  ,
         ztpp_wosum,  zvpp_model    ,  ztbm_abxopvdt, ztbm_option_item,
         ztpp_change, ztpp_process,    ztpp_alclog2,  ztpp_nation_def,
         ztpp_spec,   ztppvn1,         zvpp_rp2 ,     zvpp_bom ,
         ztpp_seq_sum01, ztpp_seq_sum02,     ztpp_day_sum,
         zsppvn1,        zspp_vm_value,      ztbm_abxpcldt,
         ztbm_abxplidt,  ztbm_abxplcdt,      ztbm_abxplbdt,
         image_control,  custom_container,   start_image_resize,
         it_alc, it_alc[],  it_model,  it_model[],

         wa_alc1date,  wa_alc2date, wa_val32,   wa_val36,
                       wa_indexs,   wa_val29,   wa_chg_1001_flg,
                       wa_val30,    wa_val31,   st_0118_input,
         wa_val33,     wa_val34,    wa_val35,   st_0111_input ,
         wa_val37,     wa_val38,    wa_val39,   wa_chg_1002_flg,
         wa_tot,       wa_iqty,     wa_mqty,    wa_flag,
         wa_model,     wa_extc,     wa_intc,    wa_chg_1005_flg,

                       wa_init_flg,  wa_scn_flag,  wa_save_flg,
         wa_change  ,  wa_err_flag,  wa_lines   ,
         wa_filename,  wa_order,                   wa_color   ,
         wa_atinn   ,  wa_atwrt,     wa_atflv   ,  wa_dep     ,
         wa_num     ,

         it_alcu_a  ,  it_menu  ,    it_219     ,  it_alcu_c  ,
         it_alcu_a[],  it_menu[],    it_219[]   ,  it_alcu_c[],
         it_alcu_b  ,  it_mara  ,    it_color   ,  it_alcu_d  ,
         it_alcu_b[],  it_mara[],    it_color[] ,  it_alcu_d[],

         it_vals_app207  ,    it_result1001   ,   it_excel000   ,
         it_vals_app207[],    it_result1001[] ,   it_excel000[] ,
         it_err  ,            it_wosum   ,        wa_wosum_key  ,
         it_err[],            it_wosum[] ,        wa_wosum_key[],
         wa_wosum   ,         wa_219     ,        st_0109_input ,
         wa_wosum[] ,                             st_0109_input[],
         it_0108  ,           it_0109    ,        it_0110       ,
         it_0108[],           it_0109[]  ,        it_0110[]     ,
         st_0110_input,       it_0111    ,        it_0111_c     ,
                              it_0111[]  ,        it_0111_c[]   ,
         it_0118,             r_atnam,
         it_0118[],           r_atnam[],
                              it_2101,            it_ausp,
                              it_2101[],          it_ausp[],
         it_2102,             it_back2101,        it_2103,
         it_2102[],           it_back2101[],      it_2103[],
         it_2104,             it_2105,            it_2202 ,
         it_2104[],           it_2105[],          it_2202[],
         it_2205,             it_2206,            it_3107,
         it_2205[],           it_2206[],          it_3107[],
         st_3107_input,       it_3109,            st_3109_input,
         st_3107_input[],     it_3109[],          st_3109_input[],
         it_4279,             it_5290,            it_5291,
         it_4279[],           it_5290[],          it_5291[],
         it_5293,             it_6299,            it_app250,
         it_5293[],           it_6299[],          it_app250[],
         it_excel_2200,       it_app252,          it_excel_2201,
         it_excel_2200[],     it_app252[],        it_excel_2201[],
                              wa_descriptions,    it_column,
                              wa_descriptions[],  it_column[],
         it_vtentries,        it_table_header,    it_cuvtln,
         it_vtentries[],      it_table_header[],  it_cuvtln[],
         it_lines_old,        it_lines_new,       it_values_c_old,
         it_lines_old[],      it_lines_new[],     it_values_c_old[],
         it_values_c_new,     it_values_n_old,    it_values_n_new,
         it_values_c_new[],   it_values_n_old[],  it_values_n_new[],
         it_app223,           it_app223_new,      it_excel_1205,
         it_app223[],         it_app223_new[],    it_excel_1205[],
         r_part,              r_key,              it_app227,
         r_part[],            r_key[],            it_app227[],
         it_excel_1209,       it_new_app227,      it_error_1210,
         it_excel_1209[],     it_new_app227[],    it_error_1210[],
         it_spec,             r_plant,            reason_tab,
         it_spec[],           r_plant[],          reason_tab[],
         dynpfields,          it_equnr_2107,      it_vmv_2107,
         dynpfields[],        it_equnr_2107[],    it_vmv_2107[],
         it_dly_2107,         it_dls_2107,        it_app221,
         it_dly_2107[],       it_dls_2107[],      it_app221[],
         it_func_1203,        it_zsppvn1,         it_app219,
         it_func_1203[],      it_zsppvn1[],       it_app219[],
         it_opt1_app219,      it_opt2_app219,     it_vmv_app236,
         it_opt1_app219[],    it_opt2_app219[],   it_vmv_app236[],
         it_wip_app236,       it_eng_app236,      it_wo_app236,
         it_wip_app236[],     it_eng_app236[],    it_wo_app236[],
         it_219_app236,       it_part_app236,     it_upart_app236,
         it_219_app236[],     it_part_app236[],   it_upart_app236[],
         it_cpart_app236,     it_ucpart_app236,   it_abag_app236,
         it_cpart_app236[],   it_ucpart_app236[], it_abag_app236[],
         it_rp_app236,                            reason_tab_app207,
         it_rp_app236[],                          reason_tab_app207[],
         dynpfields_app207,                       it_wosum2_c_app207,
         dynpfields_app207[],                     it_wosum2_c_app207[],
         it_conf_app207,      it_char_app207,     it_matnr_app207,
         it_conf_app207[],    it_char_app207[],   it_matnr_app207[],
         it_spec_app207,      it_func_app236,     it_sum_app246,
         it_spec_app207[],    it_func_app236[],   it_sum_app246[],
         it_det_app246,       it_objek,           it_temp_app245,
         it_det_app246[],     it_objek[],         it_temp_app245[],
         it_app245,           it_excel_app245,    it_date,
         it_app245[],         it_excel_app245[],  it_date[],
         it_app244,           it_excel_app244,    it_app240,
         it_app244[],         it_excel_app244[],  it_app240[],
         it_excel_app240,     r_plant_app240,     r_model_app240,
         it_excel_app240[],   r_plant_app240[],   r_model_app240[],
         r_line_app240,       r_prog_app240,      r_part_app240,
         r_line_app240[],     r_prog_app240[],    r_part_app240[],
         r_column_app240,     r_bodyno_app240,    r_wono_app240,
         r_column_app240[],   r_bodyno_app240[],  r_wono_app240[],
         r_extc_app240,       r_intc_app240,      it_objek_app239,
         r_extc_app240[],     r_intc_app240[],    it_objek_app239[],
         r_objek_app239,      r_atinn_app239,     it_char_app239,
         r_objek_app239[],    r_atinn_app239[],   it_char_app239[],
         it_app239,           it_excel_239,
         it_app239[],         it_excel_239[],
         it_hour_app301,      it_day_app301,      it_week_app301,
         it_hour_app301[],    it_day_app301[],    it_week_app301[],
         it_app272_01,        it_app263,          it_error_mat_app207,
         it_app272_01[],      it_app263[],        it_error_mat_app207[],
         it_alc_app207,       it_hpcs_app207,     it_app207,
         it_alc_app207[],     it_hpcs_app207[],   it_app207[],
         it_wosum_app207,     it_wo_app302,       it_app302,
         it_wosum_app207[],   it_wo_app302[],     it_app302[],
         it_app220,           it_219val,          gt_header,
         it_app220[],         it_219val[],        gt_header[],

          g_tc_2101_lines ,   g_body_no,  st_2104 , st_2105  ,
          st_sql, st_2101 , xname, xlist, xvalue, st_2102, st_2202 ,
          st_2203 ,  st_2203_input ,                st_2204_input ,
          st_2204_b, st_2204 , st_2205_input , st_2206_input ,
          st_5290_input ,   st_5293_input , st_4279_input ,
          st_6299_input ,   name,
          p_path, p_part, p_key,            p_yyyymm,  wa_lines_1205,
                     p_column,  p_col_name, p_sort_seq, p_code,
          p_code_chg, p_full_code, p_f_name, p_f_type, wa_init_1205,
          wa_upd_1205, g_seq_1203, p_opdate, p_opcount,
          p_worder, p_tot_count, p_keycode, p_erdat, p_erzet,
      p_ernam, p_extc, p_intc,
      zspp_app237, sv_key, st_app237, p_equnr_2107, p_status_2107,
      p_atwrt_2107, wa_model_2107, wa_serial_2107,
      wa_vmv_lines_2107,   wa_dly_lines_2107, is_app221,
      g_it_line_app219,  g_opt1_lines_app219,  g_opt2_lines_app219,
      st_app236, g_shop_date_app236, g_serial_app236,
      g_act_date_app236, g_cuobf_app236,  list_app236,
                    st_key_app236, st_iss_app236,
      p_body01_app236,  p_body02_app236,  p_paint01_app236,
      p_paint02_app236,  p_trim01_app236,  p_trim02_app236,
      g_equnr_app236,                      g_engno_app236,
      g_crsr_fld_app236, g_attr_app236,  g_vin_app236,
      g_tmno_app236,   g_part_app236,  g_parttit_app236,
      it_lines_app236,  wip_lines,     g_ss2106,
      p_bodyno_app246,  p_line_app246,  p_prog_app246,
      p_status_app246,  p_wono_app246,  p_extc_app246,
      p_intc_app246,  p_total_app246,
                          p_line_app245,  p_prog_app245,
      wa_alv_called,                     p_wono_app245    ,
      p_extc_app245    ,  p_intc_app245    ,  p_type_app245    ,
      p_color_app245    , p_end_date_app245,
      p_shop_date_app245,
                                                p_d01_app245    ,
      p_d02_app245,  p_d03_app245, p_d04_app245,  p_d05_app245,
      p_d06_app245,  p_d07_app245,  p_d08_app245,  p_d09_app245,
      p_d10_app245,  p_d11_app245,  p_d12_app245,  p_d13_app245,
      p_d14_app245,  p_d15_app245, p_d16_app245,  p_d17_app245,
      p_d18_app245,  p_d19_app245, p_d20_app245,  p_d21_app245,
      p_d22_app245, p_d23_app245,  p_d24_app245, p_d25_app245,
      p_d26_app245,  p_d27_app245,  p_d28_app245,  p_d29_app245,
      p_d30_app245, p_d31_app245,  p_carx_app272, p_gubn_app272,
      p_extc_app244,  p_intc_app244, p_total_app244,
      p_prod_date_app244,  p_prog_app244,  p_wono_app244,
      p_bodyno_app239,   p_line_app240, p_prog_app240,
      p_bodyno_app240,  p_wono_app240,  p_extc_app240,  p_intc_app240,
      p_part_app240,    p_column_app240,  p_bodyser_app239,
      p_orderno_app239, p_ext_color_app239,  p_int_color_app239,
      p_code_app301,  sv_prog_app272, sv_dynnr_app272,
      p_hpcc_app272, wa_y_app272, wa_t_app272, wa_p_app272, wa_m_app272,
     wa_clm1_app272, wa_d_app272, wa_s_app272, wa_h_app272, wa_x_app272,
      wa_clm2_app272,  wa_urgen_app272,
      p_date_app301,  p_bt_app301(02), p_sl_app301(02),
      p_part_app301, p_column_app301(03), p_col_name_app301(20),
      p_01_4104,       p_date_app302, p_part_app302,  is219,
      p_column_app302,  p_col_name_app302,  p_code_app302,
      p_02_4104,  p_03_4104, p_04_4104, p_05_4104,
      st_app263,  p_08_4104,  p_07_4104,  p_06_4104,
      wa_wosum_app207,  wa_atinn_app207, wa_atwrt_app207,
      wa_point_app207,  wa_total_app207, wa_change_app207,
      wa_filename_app207, wa_fsc_app207, wa_order_app207,
      wa_color_app207,  wa_color_dis_app207, wa_car_app207,
      wa_mi_app207,  wa_ocn_app207, wa_vers_app207,
      wa_mddat_app207,  wa_crdat_app207,  wa_dest_app207,
      wa_aldat_app207, wa_perf_app207,  wa_initq_app207,
      wa_modq_app207, wa_sqty_app207,  wa_pqty_app207,
      wa_fqty_app207,  wa_mqty_app207,  wa_lcno_app207,
      wa_plnt_app207,  wa_vin_app207,  wa_prod_app207,
      wa_recs_app207,  wa_ztpp_spec_app207, name_app207,
      p_worder_app207, p_matnr_app207, p_keycode_app207, p_erdat_app207,
      p_erzet_app207, p_ernam_app207,  p_extc_app207,
      wa_cuobf_app207,                wa_chg_1003_flg,
      wa_ok2_flag,  wa_ok3_flag,  wa_ok4_flag,  wa_1004_cod05,
      wa_ok1_flag,  wa_chg_1004_flg,  xname_app207,
      wa_fname_tx,     wa_saveline_ix,  alv_grid,   gs_custom_container,
      wa_container,    gs_variant,      gs_layout,  gs_print,
      gt_special_groups, gt_fieldcat,  gt_sort,  gt_toolbar_excluding,
      gt_filter,       gt_fieldcat_slis, wa_fieldcat,
      p_01_8088, p_02_8088, p_03_8088, p_04_8088, p_05_8088,
      P_06_8088,p_11_8088, p_12_8088, p_13_8088, p_14_8088, p_15_8088.
ENDFORM.                    " CLEAR_TABLES

*&---------------------------------------------------------------------*
*&      Form  CLEAR_TABLE_2106
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_table_2106.
  CLEAR: it_abag_app236,    it_219_app236,   it_upart_app236,
         it_abag_app236[],  it_219_app236[], it_upart_app236[],
         it_cpart_app236,   it_rp_app236,
         it_cpart_app236[], it_rp_app236[].
ENDFORM.                    " CLEAR_TABLE_2106

*&---------------------------------------------------------------------*
*&      Form  PROCESS_CONFIRM_1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_confirm_1203  .
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1 = 'Nation Data has been changed.'
            textline2 = 'Save data?'
            titel     = 'Save Confirm'
       IMPORTING
            answer    = wa_answer.

  CASE  wa_answer    .
    WHEN  'J'.
      PERFORM  nation_data_save_1203.
  ENDCASE.
ENDFORM.                    " PROCESS_CONFIRM_1203

*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3111   text
*----------------------------------------------------------------------*
FORM get_number USING    pa_type.
  DATA: l_equnr          LIKE equi-equnr.

  CONCATENATE  wa_model st_app236-bodyno  INTO  l_equnr .
  CASE pa_type.
    WHEN '+'.
      SELECT SINGLE equnr INTO l_equnr
        FROM equi
       WHERE equnr > l_equnr
         AND eqtyp = 'V'    .
      IF sy-subrc = 0.
        IF l_equnr(3) = wa_model.
          st_app236-bodyno = l_equnr+3(7) .
        ELSE.
          MESSAGE w001 WITH text-009.
        ENDIF.
      ELSE.
        MESSAGE w001 WITH text-010.
      ENDIF.
    WHEN '-'.
      SELECT equnr INTO l_equnr
        FROM equi UP TO 1 ROWS
       WHERE equnr < l_equnr
         AND eqtyp = 'V'
       ORDER BY equnr DESCENDING.
      ENDSELECT.
      IF sy-subrc = 0.
        IF l_equnr(3) = wa_model.
          st_app236-bodyno = l_equnr+3(7) .
        ELSE.
          MESSAGE w001 WITH text-009.
        ENDIF.
      ELSE.
        MESSAGE w001 WITH text-009.
      ENDIF.
  ENDCASE.
ENDFORM.                    " GET_NUMBER

*&---------------------------------------------------------------------*
*&      Form  set_list_SREASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0930   text
*----------------------------------------------------------------------*
FORM set_list_SREASON                       .
  CLEAR : xlist[] , xvalue, ylist[]         .

  xvalue-text = '[WO]Careless Working'.
  xvalue-key  = 'WO'.
  APPEND xvalue TO xlist .

  xvalue-text = '[DR]Careless Driving'.
  xvalue-key  = 'DR'.
  APPEND xvalue TO xlist .

  xvalue-text = '[SP]Spec Error'.
  xvalue-key  = 'SP'.
  APPEND xvalue TO xlist .

  xvalue-text = '[TR]Careless Consign'.
  xvalue-key  = 'TR'.
  APPEND xvalue TO xlist .

  xvalue-text = '[MA]Inferior Parts'.
  xvalue-key  = 'MA'.
  APPEND xvalue TO xlist .

  xvalue-text = '[CA]Cause Ambiguity'.
  xvalue-key  = 'CA'.
  APPEND xvalue TO xlist .

  xvalue-text = '[RE]Poor Return'.
  xvalue-key  = 'RE'.
  APPEND xvalue TO xlist .

  xvalue-text = '[KD]KD'.
  xvalue-key  = 'KD'.
  APPEND xvalue TO xlist .

  xvalue-text = '[AS]A/S'.
  xvalue-key  = 'AS'.
  APPEND xvalue TO xlist .

  xvalue-text = '[OT]The others'.
  xvalue-key  = 'OT'.
  APPEND xvalue TO xlist .

  xvalue-text = '[TS]Test     '.
  xvalue-key  = 'TS'.
  APPEND xvalue TO xlist .

  xvalue-text = '[YD]Usage Car'.
  xvalue-key  = 'YD'.
  APPEND xvalue TO xlist .
  Ylist[] = xlist[].
ENDFORM.                    " set_list_SREASON

*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_ATWRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MODEL  text
*      -->P_IT_2205_MODEL  text
*----------------------------------------------------------------------*
FORM GET_VALUE_ATWRT USING    pA_ATINN   PA_ATWRT .
  SELECT SINGLE ATWRT INTO PA_ATWRT
    FROM AUSP
   WHERE OBJEK = IT_AUSP-OBJEK
     AND ATINN = PA_ATINN
     AND KLART = '002'   .
ENDFORM.                    " GET_VALUE_ATWRT

*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MODEL  text
*      -->P_IT_2205_MODEL  text
*----------------------------------------------------------------------*
FORM GET_VALUE_ATFLV USING    pA_ATINN   PA_VALUE .
  DATA: L_ATFLV      LIKE AUSP-ATFLV,
        L_NUM(8)     TYPE N         .

  SELECT SINGLE ATFLV INTO L_ATFLV
    FROM AUSP
   WHERE OBJEK = IT_AUSP-OBJEK
     AND ATINN = PA_ATINN
     AND KLART = '002'   .

  IF SY-SUBRC = 0.
    PA_VALUE = L_NUM = L_ATFLV .
  ENDIF.
ENDFORM.                    " GET_VALUE_ATFLV

*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETRS_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PARAMETRS_VALS.
  DATA: L_DATE              TYPE D.

* Generation Date
  select single *
    from ZTPP_COMMON_VALS
   WHERE JOBS = C_JOBS
     AND KEY2 = C_KEY2 .

  l_date = ST_APP302-GDATE = ZTPP_COMMON_VALS-datES.
  ST_APP302-D0  = L_date+4(4) .  l_date = l_date + 1 .
  CONCATENATE ST_APP302-D0+2(2) '/'  ST_APP302-D0(2) INTO ST_APP302-D0.
  ST_APP302-GTIME = ZTPP_COMMON_VALS-TIMES.
  PERFORM read_working_date USING '+'  'HM'  l_date.
  ST_APP302-D1  = l_date+4(4).  l_date = l_date + 1 .
  CONCATENATE ST_APP302-D1+2(2) '/'  ST_APP302-D1(2) INTO ST_APP302-D1.
  PERFORM read_working_date USING '+'  'HM'  l_date.
  ST_APP302-D2  = l_date+4(4).  L_DATE = ST_APP302-GDATE - 1 .
  CONCATENATE ST_APP302-D2+2(2) '/'  ST_APP302-D2(2) INTO ST_APP302-D2.
  PERFORM read_working_date USING '-'  'HM'  l_date.
  ST_APP302-PR  = l_date+4(4).
  CONCATENATE ST_APP302-PR+2(2) '/'  ST_APP302-PR(2) INTO ST_APP302-PR.
ENDFORM.                    " GET_PARAMETRS_VALS

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
*&      Form  WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form WRITE_DATA.
  data: l_skip,
        lt_sum01       like table of ztpp_seq_sum01    with header line,
        lt_sum02       like table of ztpp_seq_sum02    with header line,
        lt_sum03       like table of ztpp_seq_sum03    with header line,
        lt_sum04       like table of ztpp_alc_binput   with header line,
        lt_sum05       like table of ztpp_alc_tinput   with header line,
        lt_sum06       like table of ZTPP_ALC_PROD     with header line,
*       lt_sum07       like table of ztpp_seq_sum02    with header line,
        lt_sum08       like table of ztpp_input_plan   with header line,
        lt_sum09       like table of ZTPP_WIRE_hour    with header line,
        lt_sum10       like table of ZTPP_WIRE_DAY     with header line.

  " 1. Set the Data according to the Select
  " 2. Set the Default Printer...
  CASE 'X'.
    WHEN p_01_4104.  " ALC Sequence Summary (Hourly)
      PERFORM MAKE_DATA_SUM01     .
      PERFORM MAKE_DATA_SUM01     .
    WHEN p_02_4104.  " ALC Sequence Summary (Daily)
      select * into table lt_sum02
        from ztpp_seq_sum02       .
    WHEN p_03_4104.  " ALC Sequence Summary (Weekly)
      select * into table lt_sum03
        from ztpp_seq_sum03       .
    WHEN p_04_4104.  " Body Input Plan List
      select * into table lt_sum04
        from ztpp_alc_binput      .
    WHEN p_05_4104.  " Trim Input Plan List
      select * into table lt_sum05
        from ztpp_alc_tinput      .
    WHEN p_06_4104.  " Monthly Production Result List
      select * into table lt_sum06
        from ZTPP_ALC_PROD        .
    WHEN p_07_4104.  " Vehicle Sequence List
      l_skip = 'X' .
    WHEN p_08_4104.  " Vehicle Status List
      select * into table lt_sum08
        from ztpp_input_plan      .
    WHEN p_09_4104.  " Wire Mixture - Hourly
      select * into table lt_sum09
        from ZTPP_WIRE_hour       .
    WHEN p_10_4104.  " Wire Mixture - Daily
      select * into table lt_sum10
        from ZTPP_WIRE_day        .
  ENDCASE.

  check l_skip = space.
endform.                    " WRITE_DATA
