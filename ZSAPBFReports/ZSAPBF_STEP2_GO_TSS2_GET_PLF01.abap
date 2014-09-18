*----------------------------------------------------------------------*
***INCLUDE ZSAPBF_STEP2_GO_TSS2_GET_PLF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT_PARA_SETTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_PLANT_PARA_SETTING  text
*----------------------------------------------------------------------*
form GET_PLANT_PARA_SETTING  changing et_plant_para_setting type zsapbf_tt_plant_para_setting.
  DATA lt_sel_plt_para TYPE STANDARD TABLE OF zsapbf_plt_para.
  DATA ls_sel_plt_para TYPE zsapbf_plt_para.
  DATA ls_sel_plant_para_setting TYPE zsapbf_plant_para_setting.
* Get plant setting from DB
  SELECT * FROM zsapbf_plt_para
     INTO CORRESPONDING FIELDS OF TABLE lt_sel_plt_para
    WHERE tran_code = gv_tran_code.

  LOOP AT lt_sel_plt_para INTO ls_sel_plt_para.
    ls_sel_plant_para_setting-werks = ls_sel_plt_para-werks.
    ls_sel_plant_para_setting-servgr = ls_sel_plt_para-classname.
    ls_sel_plant_para_setting-wps_re = ls_sel_plt_para-wp_quota.
    APPEND ls_sel_plant_para_setting TO et_plant_para_setting.
  ENDLOOP.
endform.                    " GET_PLANT_PARA_SETTING
