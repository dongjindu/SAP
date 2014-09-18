*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CPZP_CORRECTION_REPF03
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  substract_reversals_var_mat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM substract_reversals_var_mat .  "#EC CALLED
*
*  SORT gt_mat_comp_var_orderid BY ppc_mat-mat_number ppc_mat-reppoint
*                                  orderid.
*  LOOP AT gt_mat_rev_var_orderid ASSIGNING <fs_mat_rev_var_orderid>.
*    READ TABLE gt_mat_comp_var_orderid BINARY SEARCH ASSIGNING
*          <fs_mat_comp_var_orderid> WITH KEY
*          ppc_mat-mat_number = <fs_mat_rev_var_orderid>-ppc_mat-mat_number
*          ppc_mat-reppoint   = <fs_mat_rev_var_orderid>-ppc_mat-reppoint
*          orderid            = <fs_mat_rev_var_orderid>-orderid.
*    IF sy-subrc = 0.
*      <fs_mat_comp_var_orderid>-ppc_mat-quantity =
*      <fs_mat_comp_var_orderid>-ppc_mat-quantity -
*      <fs_mat_rev_var_orderid>-ppc_mat-quantity.
*    ELSE.
*      ASSIGN <fs_mat_rev_var_orderid> TO <fs_mat_comp_var_orderid>.
**      <fs_mat_comp_var_orderid> = <fs_mat_rev_var_orderid>.
*      APPEND <fs_mat_comp_var_orderid> TO gt_mat_comp_var_orderid.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " substract_reversals_var_mat



*&---------------------------------------------------------------------*
*&      Form  substract_reversals_var_act
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM substract_reversals_var_act . "#EC CALLED
*
*  SORT gt_act_comp_var_orderid BY
*                    ppc_act-reppoint ppc_act-ressource_guid orderid.
*  LOOP AT gt_act_rev_var_orderid ASSIGNING <fs_act_rev_var_orderid>.
*    READ TABLE gt_act_comp_var_orderid BINARY SEARCH ASSIGNING
*          <fs_act_comp_var_orderid> WITH KEY
*          ppc_act-reppoint = <fs_act_rev_var_orderid>-ppc_act-reppoint
*          ppc_act-ressource_guid =
*          <fs_act_rev_var_orderid>-ppc_act-ressource_guid
*          orderid = <fs_act_rev_var_orderid>-orderid.
*    IF sy-subrc = 0.
*      <fs_act_comp_var_orderid>-ppc_act-duration_var =
*      <fs_act_comp_var_orderid>-ppc_act-duration_var -
*      <fs_act_rev_var_orderid>-ppc_act-duration_var.
*      <fs_act_comp_var_orderid>-ppc_act-duration_fix =
*      <fs_act_comp_var_orderid>-ppc_act-duration_fix -
*      <fs_act_rev_var_orderid>-ppc_act-duration_fix.
*    ENDIF.
*  ENDLOOP.
*
*
*
*ENDFORM.                    " substract_reversals_var_act
*&---------------------------------------------------------------------*
*&      Form  get_gjahr_period_from_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gjahr_period_from_date .                           "#EC CALLED

** lokale Variablen
*  DATA:  l_kokrs TYPE kokrs.      "Kostenrechungskreis
*
*  DATA: lv_last_month TYPE sy-datum.
*
** Determine Controlling Area
*  CALL FUNCTION 'RM_KOKRS_TO_PLANT_FIND'
*    EXPORTING
*      werks   = plant
*    IMPORTING
*      e_kokrs = l_kokrs
*    EXCEPTIONS
*      OTHERS  = 1.
*  IF sy-subrc NE 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*  gd_kokrs = l_kokrs.
*
*
*
** Get Fiscal year and Period
*  CALL FUNCTION 'K_DATE_TO_PERIOD_CONVERT'
*    EXPORTING
*      i_date  = lv_last_month
*      i_kokrs = l_kokrs
*    IMPORTING
*      e_gjahr = gjahr
*      e_perio = periode
*    EXCEPTIONS
*      OTHERS  = 1.
*  IF NOT sy-subrc IS INITIAL.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.


ENDFORM.                    " get_gjahr_period_from_date
*&---------------------------------------------------------------------*
*&      Form  get_act_objnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_act_objnr USING it_act_wip_credit TYPE tt_act_comp_orderid
                CHANGING et_act_wip_credit_objnr TYPE tt_act_comp_objnr.

  DATA: ls_act_wip_credit_objnr TYPE gt_act_comp_objnr_typ.


*get ACT_OBJNR from activities for method


  LOOP AT it_act_wip_credit ASSIGNING <fs_act_wip_credit>.
*    MOVE-CORRESPONDING <fs_act_wip_credit> TO ls_act_wip_credit_objnr.
    ls_act_wip_credit_objnr-reppoint = <fs_act_wip_credit>-ppc_act-reppoint.
    ls_act_wip_credit_objnr-ressource_guid =
             <fs_act_wip_credit>-ppc_act-ressource_guid.
    ls_act_wip_credit_objnr-mode_guid =
             <fs_act_wip_credit>-ppc_act-mode_guid.
    ls_act_wip_credit_objnr-duration_var =
             <fs_act_wip_credit>-ppc_act-duration_var.
    ls_act_wip_credit_objnr-duration_fix =
             <fs_act_wip_credit>-ppc_act-duration_fix.
    ls_act_wip_credit_objnr-delta_dur_var =
             <fs_act_wip_credit>-ppc_act-delta_dur_var.
    ls_act_wip_credit_objnr-delta_dur_fix =
             <fs_act_wip_credit>-ppc_act-delta_dur_fix.
    ls_act_wip_credit_objnr-durunit =
             <fs_act_wip_credit>-ppc_act-durunit.
    ls_act_wip_credit_objnr-orderid =
             <fs_act_wip_credit>-orderid.
    ls_act_wip_credit_objnr-gjper =
             <fs_act_wip_credit>-gjper.
    CALL FUNCTION 'KCR01_GET_COST_RESOURCE'
      EXPORTING
        i_resource_guid     = <fs_act_wip_credit>-ppc_act-ressource_guid
*   I_CONTROLLING_AREA             =
       i_key_date                  = sy-datlo
     IMPORTING
       e_object_number        = ls_act_wip_credit_objnr-act_objnr
*   E_RESOURCE_TYPE                =
*   E_CONTROLLING_AREA             =
   e_cost_center              = ls_act_wip_credit_objnr-cost_center
   e_activity_type            = ls_act_wip_credit_objnr-activity_type
*   E_BUSINESS_PROCESS              =
     EXCEPTIONS
       not_found                       = 1
       controlling_area_mismatch       = 2
       OTHERS                          = 3
      .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    APPEND ls_act_wip_credit_objnr TO et_act_wip_credit_objnr.
  ENDLOOP.


ENDFORM.                    " get_act_objnr
*&---------------------------------------------------------------------*
*&      Form  compress_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compress_wip USING it_act_wip_credit_objnr TYPE tt_act_comp_objnr
                        it_comp_wip_credit TYPE tt_mat_comp_orderid
               CHANGING et_act_method TYPE tt_act_method
                        et_comp_method TYPE tt_comp_method
                         .

  DATA: ls_act_method TYPE gt_act_method_typ,
        ls_comp_method TYPE gt_comp_method_typ.

* compress WIP-credit/debit over reporting points
* activities
  CLEAR ls_act_method.
  LOOP AT it_act_wip_credit_objnr ASSIGNING <fs_act_wip_credit_objnr>.
    MOVE-CORRESPONDING <fs_act_wip_credit_objnr> TO
    ls_act_method.
    COLLECT ls_act_method INTO et_act_method.
    CLEAR ls_act_method.
  ENDLOOP.

* compress WIP-credit/debit over reporting points
* components
  CLEAR ls_comp_method.
  LOOP AT it_comp_wip_credit ASSIGNING <fs_comp_wip_credit>.
    MOVE-CORRESPONDING <fs_comp_wip_credit>-ppc_mat TO ls_comp_method. "#EC ENHOK
    IF NOT ls_comp_method IS INITIAL.
      ls_comp_method-orderid = <fs_comp_wip_credit>-orderid.
      ls_comp_method-gjper = <fs_comp_wip_credit>-gjper.
    ENDIF.
    COLLECT ls_comp_method INTO et_comp_method.
    CLEAR ls_comp_method.
  ENDLOOP.
ENDFORM.                    " compress_wip
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify .
  DATA: lv_current TYPE c.

***** Start; Commentated by James Sung-Kon Kim 2011.03.02
** check date
*  PERFORM period_check USING p_aufnr
*                             p_year
*                             p_month
*                    CHANGING lv_current
***** End; Commentated by James Sung-Kon Kim 2011.03.02
  .
  IF lv_current IS NOT INITIAL.
    p_test = 'X'.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_TEST'.
        screen-input = '0'.
        screen-value_help = '2'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

**** Start : Added by James Sung-Kon Kim 2011.03.02

  DATA: BEGIN OF ls_instance.
          INCLUDE STRUCTURE rzlligroup.
  DATA: END OF ls_instance.

  DATA lt_instance LIKE STANDARD TABLE OF ls_instance.

  IF p_sgr IS INITIAL.
    CALL FUNCTION 'SMLG_GET_DEFINED_GROUPS'
      EXPORTING
        grouptype          = 'S'
      TABLES
        groups             = lt_instance
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        invalid_group_type = 3
        no_groups_found    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*    READ TABLE lt_instance INTO ls_instance
*         WITH KEY groupname = 'CO_CLOSING'.
*
*    IF sy-subrc <> 0.
    READ TABLE lt_instance INTO ls_instance
      WITH KEY groupname = 'parallel_generators'.
*    ENDIF.

    IF ls_instance IS NOT INITIAL.
      p_sgr = ls_instance-groupname.
    ENDIF.

    IF p_sgr IS INITIAL.
      MESSAGE e010 DISPLAY LIKE 'E'.
    ENDIF.

  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 EQ '50'.
      screen-input = '0'.
    ELSEIF screen-group1 EQ '40'.
      screen-input = '1'.
    ENDIF.
    MODIFY SCREEN.

*    ENDIF.
  ENDLOOP.
**** End : Added by James Sung-Kon Kim 2011.03.02

ENDFORM.                    " SCREEN_MODIFY

*&---------------------------------------------------------------------*
*&      Form  SCREEN_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_check.


  DATA: "lv_month TYPE monat,
        lv_answer TYPE char1,
*        lv_guid TYPE qrp_accassobj,
        lv_current TYPE c.

***** Start : Check Available Server Group  by James Sung-Kon Kim 2011.03.02 *****
  DATA: l_free_wps     TYPE i.
  DATA: l_par_process  TYPE  int4 .
***** End : Check Available Server Group   by James Sung-Kon Kim 2011.03.02 *****

  l_par_process = p_wps * p_wpssub . "Added by James Sung-Kon Kim 2011.03.02

  "Added by James Sung-Kon Kim 2011.03.02
  CLEAR gv_ucomm.
  gv_ucomm = sy-ucomm.

**** Start; Added by James Sung-Kon Kim 2011/03/02
  IF p_wps IS NOT INITIAL AND p_wpssub IS NOT INITIAL.
    p_wpstot = p_wps * p_wpssub.
  ELSE.
    p_wpstot = 0.
  ENDIF.
**** End; Added by James Sung-Kon Kim 2011/03/02

***** Start : Check Parallel Process limit ***** added by James Sung-Kon Kim 2011.03.02
  IF p_par = 'X' AND p_wps IS INITIAL.
    CLEAR gv_ucomm.
    MESSAGE e012.
  ENDIF.

  IF p_par = 'X' AND p_wpssub IS INITIAL.
    CLEAR gv_ucomm.
    MESSAGE e013.
  ENDIF.
***** End : Check Parallel Process limit ***** added by James Sung-Kon Kim 2011.03.02

***** Start : Check Parallel Process limit *****
*  IF p_par = 'X' AND p_wps > 40. "Disabled by James Sung-Kon Kim 2011.03.02

  IF p_par = 'X' AND l_par_process > 40. "Disabled by James Sung-Kon Kim 2011.03.02
    CLEAR gv_ucomm.

    MESSAGE e009 WITH '40'.

  ENDIF.
***** End : Check Parallel Process limit   *****

***** Start : Check Available Server Group  by James Sung-Kon Kim 2011.03.02 *****
  PERFORM check_parallel_servergroup CHANGING l_free_wps.

*  IF l_free_wps <  p_wps. "Disabled by James Sung-Kon Kim 2011.03.02
  IF l_free_wps <  l_par_process. "Added by James Sung-Kon Kim 2011.03.02

    MESSAGE s011 WITH l_free_wps l_par_process DISPLAY LIKE 'E'.
*    EXIT. "Disabled by James Sung-Kon Kim 2011.03.02
*    STOP. "Added by James Sung-Kon Kim 2011.03.02
    EXIT. "Added by James Sung-Kon Kim 2011.03.02

  ENDIF.
***** End : Check Available Server Group  by James Sung-Kon Kim 2011.03.02 *****

* read CC_GUID from PKOSA for table PPC_ORD_INF
  CALL FUNCTION 'QRP_QRP002_READ'
    EXPORTING
      if_aufnr   = p_aufnr
*    IMPORTING
*      ef_cc_guid = lv_guid
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
    MESSAGE e407 WITH p_aufnr.
  ENDIF.

* check date
  PERFORM period_check USING p_aufnr
                             p_year
                             p_month
                    CHANGING lv_current
                             .
  IF lv_current = 'X'.
*    MESSAGE e410. "Changed from "e410" to "w417" by James Kim to fix bug 2011/01/27
    MESSAGE w417.
    p_test = 'X'.
  ENDIF.

  IF p_test = ''.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = text-407
        text_question  = text-408
        text_button_1  = text-409
        icon_button_1  = 'ICON_SYSTEM_OKAY'
        text_button_2  = text-410
        icon_button_2  = 'ICON_SYSTEM_CANCEL'
      IMPORTING
        answer         = lv_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lv_answer NE '1'.
      RETURN. "STOP. "Changed from "STOP" to "RETURN" 2011/02/21
*      MESSAGE w412. "Added by James Kim to display message 2011/02/21
    ENDIF.
  ENDIF.

ENDFORM.                    " SCREEN_CHECK
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DATLO  text
*      <--P_LV_LAST_MONTH  text
*----------------------------------------------------------------------*
FORM get_last_period USING iv_datlo TYPE sy-datum           "#EC CALLED
                  CHANGING ev_last_month TYPE sy-datum.
  ev_last_month = iv_datlo.
  ev_last_month+6(2) = 01.
  ev_last_month = ev_last_month - 1.
ENDFORM.                    " GET_LAST_PERIOD
*&---------------------------------------------------------------------*
*&      Form  PERIOD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_AUFNR  text
*----------------------------------------------------------------------*
FORM period_check USING iv_aufnr TYPE aufnr
                        iv_year TYPE gjahr
                        iv_month TYPE monat
               CHANGING ev_current TYPE flag.

  DATA: lv_gjper_post TYPE co_gjper,
        lv_gjper_curr TYPE co_gjper,
        lv_gjper_prev TYPE co_gjper,
        lv_werks TYPE werks_d.

  CHECK iv_aufnr IS NOT INITIAL.
  CHECK iv_month IS NOT INITIAL.

  SELECT SINGLE prwrk
           FROM ckmlmv013
           INTO lv_werks
          WHERE aufnr = iv_aufnr.

  PERFORM periods_get IN PROGRAM saplqrprp USING lv_werks
                                                 sy-datlo
                                        CHANGING lv_gjper_post
                                                 lv_gjper_curr
                                                 lv_gjper_prev.

  IF iv_month = lv_gjper_curr+5(2) AND
     iv_year = lv_gjper_curr(4).
    ev_current = 'X'.
  ELSEIF iv_month = lv_gjper_prev+5(2) AND
*    iv_year = lv_gjper_curr(4). " Fix bug by 2011/01/20 James Kim
    iv_year = lv_gjper_prev(4).  " Fix bug by 2011/01/20 James Kim
    ev_current = ''.
  ELSE.

**** Important; In Order To allow for the past period before previous period.  2011/01/27
**** Start; Added by James Kim 2011/01/27
    lv_gjper_post = iv_year * 1000 + iv_month.

    IF lv_gjper_post GT lv_gjper_curr.
      MESSAGE e419.
    ELSE.
      lv_gjper_curr = iv_year * 1000 + iv_month.
      lv_gjper_prev = iv_year * 1000 + iv_month.
**** End; Added by James Kim 2011/01/27

*    MESSAGE e405.
*    MESSAGE w405. "Chaned from "E" to "W;Warning" by James Kim 2011/01/24
      MESSAGE w416. "Chaned from "W405" to "W416" by James Kim 2011/01/27
    ENDIF.
  ENDIF.

ENDFORM.                    " PERIOD_CHECK
*&---------------------------------------------------------------------*
*&      Form  INITIAL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_screen .
  p_month = sy-datlo+4(2) - 1.
  IF p_month = 0.
    p_month = 12.
    p_year = p_year - 1.
  ENDIF.
ENDFORM.                    " INITIAL_SCREEN
