*----------------------------------------------------------------------*
*   INCLUDE ZAPP931R_PRO_SUMMARY_TOP                                   *
*----------------------------------------------------------------------*
 REPORT zapp931r_pro_summary MESSAGE-ID zmpp.

*Table definition
 TABLES : equi,ausp,ztpp_pro_sum,ztpp_month_plan .
*Internal table definition.
 DATA : it_value LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.

 DATA : BEGIN OF it_equi OCCURS 0,
        equnr LIKE equi-equnr,
*        plant LIKE ztpp_bfst-plant,
       END OF it_equi.

 DATA : it_temp_value  LIKE ztpp_pro_sum OCCURS 0 WITH HEADER LINE.
 DATA : BEGIN OF it_sum OCCURS 0,
         model LIKE ztpp_pro_sum-model,
         rp01 LIKE ztpp_pro_sum-rp01,
         rp02 LIKE ztpp_pro_sum-rp01,
         rp04 LIKE ztpp_pro_sum-rp01,
         rp06 LIKE ztpp_pro_sum-rp01,
         rp07 LIKE ztpp_pro_sum-rp01,
         rp18 LIKE ztpp_pro_sum-rp01,
        END OF it_sum.
 DATA : it_ztpp_pro LIKE ztpp_pro_sum OCCURS 0 WITH HEADER LINE.

 DATA : cuvtab LIKE cuvtab.

 DATA :  tab_api_vtab_fields LIKE api_vtab_field OCCURS 0
                                     WITH HEADER LINE.
 DATA : l_vtab_field LIKE api_vtab_field,
        l_cabn_tab LIKE cabn OCCURS 0 WITH HEADER LINE.
 DATA : BEGIN OF it_cuvtln OCCURS 0,
         slnid LIKE cuvtln-slnid,
        END OF it_cuvtln.
 DATA : it_plan LIKE ztpp_month_plan OCCURS 0 WITH HEADER LINE.
 DATA : it_working_time LIKE zsmm_working_time OCCURS 0
                               WITH HEADER LINE.
 DATA  :it_condition LIKE zsca_characteristic_value OCCURS 0
                          WITH HEADER LINE,
        it_value_v  LIKE  zsca_char_value  OCCURS 0
                          WITH HEADER LINE,
        it_vehicle LIKE zsca_vehicle_char_value OCCURS 0
                          WITH HEADER LINE,
        it_vehi2 LIKE zsca_vehicle_char_value OCCURS 0
                          WITH HEADER LINE,
        it_vehi LIKE it_vehicle OCCURS 0 WITH HEADER LINE.
 DATA : BEGIN OF it_wtsum OCCURS 0,
         opsec(10) type p decimals 2," LIKE zsmm_working_time-opsec,
        END OF it_wtsum,
        BEGIN OF it_wdsum OCCURS 0,
         datum LIKE zsmm_working_time-datum,
         opsec(10) type p decimals 2," zsmm_working_time-opsec,
        END OF it_wdsum.

 RANGES : l_r_atinn FOR cabn-atinn.

*data definition
 DATA : w_int TYPE i,
        c_date(8)." like sy-datum.
 DATA : vtnam  LIKE cuvtab-vtnam VALUE 'HMMA_MONTH_PLAN',
        f_valc LIKE cuvtab_valc-valc.
 DATA : l_val_from LIKE cuvtab_valn-val_from,
        y_date LIKE sy-datum,
        yy_date LIKE sy-datum,
        a07_date(30).
 DATA :d_days LIKE  vtbbewe-atage.
 DATA: lw_date  TYPE d,
       p_flag ,
       p_char_status LIKE  cabn-atnam ,
       p_i_atwrt_s LIKE ausp-atwrt,
       p_i_atwrt_e LIKE ausp-atwrt,
       t_count TYPE i,
       w_kalid     LIKE kako-kalid,                  "Calender ID
       w_mosid     LIKE kako-mosid,                  "Schedule group
       w_kapid     LIKE kako-kapid,                  "Capacity ID
       lw_lines TYPE i,
       lw_daynr LIKE hrvsched-daynr,
       lw_dayfree LIKE hrvsched-noday,
       z_atinn LIKE cawn-atinn.
 DATA: IT_DELAY LIKE ZTPP_DELAY_CAR OCCURS 0 WITH HEADER LINE.
