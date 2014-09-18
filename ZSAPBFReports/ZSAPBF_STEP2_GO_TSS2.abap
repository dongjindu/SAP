*&---------------------------------------------------------------------*
*& Report  ZSAPBF_STEP2_GO_TSS2                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT zsapbf_step2_go_tss2 MESSAGE-ID ppc1pa.

INCLUDE zsapbf_conf_go_top.
INCLUDE zsapbf_step2_go_tss2top.
INCLUDE zsapbf_step2_go_tss2f01.
*CDP2 Start
INCLUDE zsapbf_conf_go_f01.

DATA lv_tss2_rpt TYPE progname.

INITIALIZATION.
  gv_tran_code = '3'.                                       "TSS2

  lv_tss2_rpt = lc_tss2_rpt.
  tabs1 = text-011.
  tabs2 = text-012.
  MOVE text-tpt TO gs_button-text.
  MOVE '@3R@' TO gs_button-icon_id.
  MOVE text-tpt TO gs_button-icon_text.
  MOVE text-tpt TO gs_button-quickinfo.
  MOVE gs_button TO sscrfields-functxt_02.
* check the flags to see how the report is triggered

* Flag: if triggered by generate dynamic screen
  IMPORT lv_flag FROM MEMORY ID lc_methdflag.

* Flag: if riggered by clear dynamic screen
  IMPORT lv_clear_rpt FROM MEMORY ID lc_clear.


  IF lv_flag = gc_charx.
* Triggered by generate dynamic screen
**Defalult tab
    GET PARAMETER ID:  'DYNNR' FIELD lv_dynnr,              "#EC EXISTS
                       'UCOMM' FIELD lv_activetab.          "#EC EXISTS
    IF lv_dynnr IS NOT INITIAL.
      test1-dynnr = lv_dynnr.                               "'0002'.
      test1-activetab = lv_activetab.                       "'UCOMM2'.
    ENDIF.
** Plant parallelization setting.
    IMPORT it_plt_para FROM MEMORY ID lc_plants.
    IMPORT lt_plant_para_setting FROM MEMORY ID lc_pltset.
    CLEAR lv_flag.
    EXPORT lv_flag TO MEMORY ID lc_methdflag.
  ELSE.
** Initial run
*    IF lv_clear_rpt IS INITIAL.
*      PERFORM clear_dynamic_screen USING lv_tss2_rpt.
*      lv_clear_rpt = gc_charx.
*      EXPORT lv_clear_rpt TO MEMORY ID lc_clear.
*      CLEAR lt_plant_para_setting.
*      EXPORT lt_plant_para_setting TO MEMORY ID lc_plants.
*      SUBMIT (sy-cprog) VIA SELECTION-SCREEN
*      USING SELECTION-SET sy-slset.
*    ELSE.
** Triggered by clear dynamic screen
*      CLEAR lv_clear_rpt.
*      EXPORT lv_clear_rpt TO MEMORY ID lc_clear.
*    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF p_meth2 = gc_charx.
    LOOP AT SCREEN.
      IF screen-group1 = lc_bl1.
        screen-input = lc_1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = lc_bl1.
        screen-input = lc_0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_meth.
* F4 help
  PERFORM get_f4help USING lc_3.
*CDP2 End
AT SELECTION-SCREEN.
*CDP2 Start
  IF sy-ucomm = lc_fc02 AND sy-dynnr = 1000.
    CALL SCREEN 0100 STARTING AT 10 5.
  ENDIF.

  IF sy-ucomm = lc_meth.
* If customer parallelization method is used

* 1. Generate the dynamic selection according to the customizing
    PERFORM dynamic_screen_generate USING    p_meth
                                             lc_3
                                             lv_tss2_rpt
                                    CHANGING lt_ui_maint.
* 2. Generate the description of the parameters
    PERFORM param_descpt_get USING lt_ui_maint.

* 3. Reserve the current input to memory
    SET PARAMETER ID: 'DYNNR' FIELD '0002',                 "#EC EXISTS
                  'UCOMM' FIELD 'UCOMM2'.                   "#EC EXISTS
    lv_flag = gc_charx.
    EXPORT lv_flag TO MEMORY ID lc_methdflag.
    EXPORT lt_ui_maint TO MEMORY ID lc_paratsst.
    EXPORT lt_plant_para_setting TO MEMORY ID lc_pltset.

* 4. Submit me again
    SUBMIT (sy-cprog)
      WITH so_date    IN so_date[]
      WITH p_dtsta    EQ p_dtsta
      WITH p_tista    EQ p_tista
      WITH p_dtend    EQ p_dtend
      WITH p_tiend    EQ p_tiend
      WITH p_delay    EQ p_delay
      WITH so_unam    IN so_unam[]
      WITH so_werks   IN so_werks[]
      WITH p_proto    EQ p_proto
      WITH p_limit    EQ p_limit
      WITH p_par      EQ p_par
      WITH p_sgr      EQ p_sgr
      WITH p_wps      EQ p_wps
      WITH p_del_hl   EQ p_del_hl
      WITH p_wttime   EQ p_wttime
      WITH p_retryc   EQ p_retryc
      WITH p_retrys   EQ p_retrys
      WITH p_retryr   EQ p_retryr
      WITH p_meth     EQ p_meth
      WITH p_meth1    EQ p_meth1
      WITH p_meth2    EQ p_meth2
      VIA SELECTION-SCREEN.
  ENDIF.
*CDP2 End
* parameter check
  PERFORM parameter_check.

START-OF-SELECTION.
  IF p_meth2 IS NOT INITIAL AND p_meth IS INITIAL.
    MESSAGE i041(zsapbf).
    STOP.
  ENDIF.
*CDP2 Start
  IMPORT lt_ui_maint FROM MEMORY ID lc_paratsst.
  PERFORM parameter_fill USING lt_ui_maint
                      CHANGING lt_ui_options.
  FREE MEMORY ID lc_paratsst.
*CDP2 End
  PERFORM parallel_fill CHANGING gs_parallel.

  LOOP AT so_date INTO gs_seldate.
    APPEND gs_seldate TO gt_date_range.
  ENDLOOP.
  LOOP AT so_unam INTO gs_selunam.
    APPEND gs_selunam TO gt_unam_range.
  ENDLOOP.

  MOVE so_werks[] TO gr_werks_range.

  PERFORM get_plant_para_setting CHANGING lt_plant_para_setting.

*HMC 2-Step using method
  CALL FUNCTION 'ZSAPBF_CDP2_STEP2_EXE'
    EXPORTING
      it_date_range     = gt_date_range
      it_user_range     = gt_unam_range
      it_plant_range    = gr_werks_range[]
      it_conftime_range = gr_conftime
      iv_limit          = p_limit
      iv_delay          = p_delay
      iv_protocol_show  = p_proto
      iv_show_log_all   = p_show_l
      is_parallel       = gs_parallel
      it_ui_options     = lt_ui_options
      it_plant_setting  = lt_plant_para_setting
    IMPORTING
      ev_lognumber      = gv_lognr
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE gc_chars NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE s017(ppc1pr) WITH gv_lognr.
  ENDIF.

  INCLUDE zsapbf_step2_go_tss2_get_plf01.
