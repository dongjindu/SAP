*&---------------------------------------------------------------------*
*&  Include           ZRHR_ANNU_PERF_RESULTTOP
*&---------------------------------------------------------------------*
type-pools: vrm.

tables: pa0001, hrp1001.

data: ok_code                 type sy-ucomm.

* alv variable definition
data: gr_cont                 type ref to cl_gui_custom_container,
      gr_grid                 type ref to cl_gui_alv_grid.
data: gs_layo                 type lvc_s_layo.
data: gt_fcat                 type lvc_t_fcat with header line,
      gt_sort                 type lvc_t_sort with header line.

* dropdown definition
data: gt_values               type vrm_values,
      gs_value                like line of gt_values,
      g_fieldname             type vrm_id.

data: begin of gt_hrp1000 occurs 0,
          plvar               type hrp1000-plvar,
          otype               type hrp1000-otype,
          objid               type hrp1000-objid,
          stext               type hrp1000-stext,
        end of gt_hrp1000.

data: gs_result               type zshr_comp_result,
      gt_result               like table of gs_result.

constants: c_column_iid_zp13  type hap_column_iid value 7,    " Column Self Appraisal (YE)
           c_column_iid_zp14  type hap_column_iid value 8.

*&********************************************************************
*    Selection Screen
*&********************************************************************
selection-screen begin of screen 200 as subscreen.
selection-screen begin of block b1 with frame title text-t01.

selection-screen begin of line.
selection-screen comment 2(19) text-t80 for field p_year.
parameters: p_year      type zdhr_year as listbox visible length 10.
selection-screen comment 50(20) text-t82 for field p_compg.
parameters: p_compg     type stext as listbox visible length 30
                        user-command compg.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 2(16) text-t83 for field s_kostl.
select-options: s_kostl for pa0001-kostl no intervals matchcode object kost.
selection-screen comment 50(20) text-t84 for field p_compt.
parameters: p_compt      type stext as listbox visible length 30
                         user-command compt.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 2(16) text-t85 for field s_pernr.
select-options: s_pernr for pa0001-pernr no intervals matchcode object prem.
selection-screen comment 50(20) text-t86 for field p_beatt.
parameters: p_beatt     type stext as listbox visible length 30.
selection-screen end of line.

selection-screen begin of line.
*   07/22/2013 - T00306 Start
selection-screen comment 2(19) text-t87 for field p_staus.
parameters: p_staus type char01 as listbox visible length 18.
*   07/22/2013 - T00306 End
selection-screen comment 50(20) text-t81 for field p_rating.
parameters: p_rating    type rating as listbox visible length 22.
selection-screen end of line.

selection-screen end of block b1.
selection-screen end of screen 200 .
