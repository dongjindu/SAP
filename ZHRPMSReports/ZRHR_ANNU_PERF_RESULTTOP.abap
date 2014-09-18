*&---------------------------------------------------------------------*
*&  Include           ZRHR_ANNU_PERF_RESULTTOP
*&---------------------------------------------------------------------*
  type-pools: vrm.

  tables: pa0001.

  data: ok_code                 type sy-ucomm.

* alv variable definition
  data: gr_cont                 type ref to cl_gui_custom_container,
        gr_grid                 type ref to cl_gui_alv_grid.
  data: gs_layo                 type lvc_s_layo.
  data: gt_fcat                 type lvc_t_fcat with header line.

  data: gs_result               type zshr_annp_result,
        gt_result               like table of gs_result.

  data: gt_hrp9873              TYPE TABLE OF hrp9873.

  constants: c_column_iid_fapp  type hap_column_iid value 9,    " Column Annual Rating
             c_column_iid_zp15  type hap_column_iid value 6,    " Column Raing
             c_column_iid_zp16  type hap_column_iid value 10.   " Column Raing


*&********************************************************************
*    Selection Screen
*&********************************************************************
  selection-screen begin of screen 200 as subscreen.
  selection-screen begin of block b1 with frame title text-t01.
  parameters: p_year      type zdhr_year as listbox visible length 10.
  select-options: s_kostl for pa0001-kostl no intervals matchcode object kost,
                  s_pernr for pa0001-pernr no intervals matchcode object prem.
  parameters:     p_stats type hap_ap_status as listbox visible length 18.
  selection-screen end of block b1.
  selection-screen end of screen 200 .
