*----------------------------------------------------------------------*
*   INCLUDE ZRWFZTOP                                                   *
*----------------------------------------------------------------------*
data: begin of wa_plaf,
        matnr like plaf-matnr,
        plwrk like plaf-plwrk,
        gsmng like plaf-gsmng,
        paltr like plaf-paltr,
      end of wa_plaf.
data: begin of wa_mat_info,
       matnr like mara-matnr,
       profl like mara-profl,
       kordb like marc-kordb,
       maktx like makt-maktx,
      end of wa_mat_info.
data: begin of wa_hts_info,
        stawn like t604-stawn,
        text1 like t604t-text1,
        kbetr like konp-kbetr,
     end of wa_hts_info.
data: begin of wa_mat_price,
        matnr like mara-matnr,
        kbetr type p decimals 3,
      end of wa_mat_price.
*data: begin of wa_stpox.
*        include structure stpox.
*data: pmatnr like mara-matnr,
*      end of wa_stpox.
data: wa_stpox like stpox.
data: begin of wa_comp_info,
        matnr like mara-matnr,
        stawn like marc-stawn,
        menge like stpox-mngko,
      end of wa_comp_info.
data: begin of wa_result,
        week(2) type n,
        profl like mara-profl,
        matnr like mara-matnr,
        maktx like makt-maktx,
        stawn like marc-stawn,
        text1 like t604t-text1,
        menge like stpox-mngko,
        kbetr1 type p decimals 3,           "Price
*Changed by Furong on 06/14/05
*       kbetr2 like konp-kbetr,           "Duty
        kbetr2 type p decimals 3,
*end of change
        salk31 like mbew-salk3,           "price vlaue
        salk32 like mbew-salk3,           "duty value
        salk33 like mbew-salk3,           "total value
      end of wa_result.
data: it_plaf like table of wa_plaf,
      it_stpox like table of wa_stpox,
      it_mat_info like table of wa_mat_info,
      it_hts_info like table of wa_hts_info,
      it_mat_price like table of wa_mat_price,
      it_comp_info like table of wa_comp_info,
      it_result like table of wa_result.

ranges: r_mtart for mara-mtart.

data: w_cur_week like scal-week,
      w_nxt_week like scal-week,
      w_fdate like sy-datum,
      w_tdate like sy-datum,
      w_taskname(4) type n value '0001',
      w_snd_jobs type i value 1,
      w_rcv_jobs type i value 1,
      w_exc_flag type c,
      w_lines type i,
      ok_code(20) type c.
*&---------------Custom control---------------------------------------
data: w_custom_container type ref to cl_gui_custom_container,
      w_grid type ref to cl_gui_alv_grid,
      it_fldclog type    lvc_t_fcat,
      w_layo     type    lvc_s_layo,
      it_srt     type    lvc_t_sort.

field-symbols: <fs_plaf> like line of it_plaf,
               <fs_stpox> like line of it_stpox.
