*&---------------------------------------------------------------------*
*& Include YAPP803L_ZTPPWOSUM_MNTNC_TOP                               *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp246r_prod_inl_by_progress  message-id zmpp    .

*CONTROLS: tc_APP246 TYPE TABLEVIEW USING SCREEN 110.

tables: ausp,  "Characteristic Values
        cabn.  "Characteristic

data : begin of it_sum_app246 occurs 0.
         include structure zspp_sum_app246.
data :   objek type ausp-objek,
         model type ausp-atwrt,
       end of   it_sum_app246 .

data: begin of it_det_app246 occurs 0.
        include structure zspp_det_app246.
data:   objek type ausp-objek,
        model type ausp-atwrt,
      end of   it_det_app246.

* Parameters(Screen0110)
data: p_company_app246(04),
      p_plant_app246(08),                 "P_TRIM_PLANT_NO
      p_model_app246(03),                 "P_MODEL
      p_bodyno_app246 type ausp-atwrt,    "P_BODY_SERIAL(09)
      p_line_app246(03),                  "P_TRIM_LINE_NO
      p_prog_app246(08),                  "P_RP_STATUS
      p_status_app246(10),                " 'S':Summary, 'D':Detail.
      p_wono_app246(14),                  "P_WORK_ORDER
      p_extc_app246(03),                  "P_EXT_COLOR
      p_intc_app246(03),                  "P_INT_COLOR
      p_total_app246(05) type n,
**    P_219_xxx
      p_column01_app246(03),
      p_column02_app246(03),
      p_column03_app246(03),
      p_column04_app246(03),
      p_column05_app246(03),
      p_column06_app246(03),
      p_column07_app246(03),
      p_column08_app246(03),
      p_column09_app246(03),
      p_column10_app246(03),
      p_value01_app246 type ausp-atwrt,
      p_value02_app246 type ausp-atwrt,
      p_value03_app246 type ausp-atwrt,
      p_value04_APP246 type ausp-atwrt,
      p_value05_APP246 type ausp-atwrt,
      p_value06_APP246 type ausp-atwrt,
      p_value07_APP246 type ausp-atwrt,
      p_value08_APP246 type ausp-atwrt,
      p_value09_APP246 type ausp-atwrt,
      p_value10_APP246 type ausp-atwrt.

* DROPDOWN LIST for Parameters
type-pools: vrm.
data: name        type vrm_id,
*     Plant
      plant_list  type vrm_values,
      plant_value like line of plant_list,
*     Model
      model_list  type vrm_values,
      model_value like line of model_list,
*     Body Serial
      body_ser_list  type vrm_values,
      body_ser_value like line of body_ser_list,
*     Line
      line_list  type vrm_values,
      line_value like line of line_list,
*     Status
      status_list type vrm_values,
      status_value like line of status_list,
*     Progress
      progress_list  type vrm_values,
      progress_value like line of progress_list,
*     Work Order
      wono_list type vrm_values,
      wono_value like line of wono_list,
*     External Color
      extc_list type vrm_values,
      extc_value like line of extc_list,
*     Internal Color
      intc_list type vrm_values,
      intc_value like line of intc_list,
*     Column01
      column01_list type vrm_values,
      column01_value like line of column01_list,
*     COLUMN02
      column02_list type vrm_values,
      column02_value like line of column02_list,
*     COLUMN03
      column03_list type vrm_values,
      column03_value like line of column03_list,
*     COLUMN04
      column04_list type vrm_values,
      column04_value like line of column04_list,
*     COLUMN05
      column05_list type vrm_values,
      column05_value like line of column05_list,
*     Column06
      column06_list type vrm_values,
      column06_value like line of column06_list,
*     COLUMN07
      column07_list type vrm_values,
      column07_value like line of column07_list,
*     COLUMN08
      column08_list type vrm_values,
      column08_value like line of column08_list,
*     COLUMN09
      column09_list type vrm_values,
      column09_value like line of column09_list,
*     COLUMN10
      column10_list type vrm_values,
      column10_value like line of column10_list.

data: begin of it_objek occurs 0,
        objek type ausp-objek,
      end of it_objek .

data:  wa_init_flg_app246,
       wa_alv_called.
************************************************************************
*  FOR ALV GRID
************************************************************************
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
data : wa_fname_tx(40),
       wa_saveline_ix     like  sy-index.
*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
data : alv_grid               type ref to cl_gui_alv_grid,
       gs_custom_container    type ref to cl_gui_custom_container,
       wa_container           type scrfname value 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
data : gs_variant        type disvariant ,  "Display Variant
       gs_layout         type lvc_s_layo ,  "Layout
       gs_print          type lvc_s_prnt ,  "Print control
       gt_special_groups type lvc_t_sgrp ,  "Field groups
       gt_toolbar_excluding type ui_functions , "Exclu Toolbar Std FUNC
       gt_header         type table of slis_listheader with header line,
       gt_fieldcat       type lvc_t_fcat ,  "Field Catalog
       gt_sort           type lvc_t_sort ,  "Sort criteria
       gt_filter         type lvc_t_filt .  "Filter criteria
data : wa_fieldcat     type lvc_s_fcat.

**********************************
*Define Macro
**********************************
define fieldcat_compose.
  it_fieldcat-fieldname = &1.
  it_fieldcat-reptext   = &2.
  it_fieldcat-outputlen = 20.
  append it_fieldcat to gt_fieldcat.

end-of-definition.

define set_fieldcat.
  l_fieldcat-reptext   = &1.
  l_fieldcat-scrtext_l = &1.
  l_fieldcat-scrtext_m = &1.
  l_fieldcat-scrtext_s = &1.
  l_fieldcat-colddictxt = 'L'.
  l_fieldcat-outputlen = &2.

end-of-definition.
