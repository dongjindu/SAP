*&---------------------------------------------------------------------*
*& Report  ZSAPBF_CPZP_CORRECTION_FORCE                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zsapbf_cpzp_correction_forced MESSAGE-ID zsapbf_qrprp.

INCLUDE zsapbf_cpzp_correction_fortop.
INCLUDE zsapbf_cpzp_correction_forsel.
*INCLUDE zsapbf_cpzp_correction_forf01.
*INCLUDE zsapbf_cpzp_correction_forf02.
*INCLUDE zsapbf_cpzp_correction_forf03.

INITIALIZATION.
  PERFORM initial_screen.

AT SELECTION-SCREEN.
  PERFORM screen_check.

* procnr must not be ready for input
AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

*&---------------------------------------------------------------------*
*& Start of selection                                                  *
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM set_period.
  PERFORM select_objnr_data.
  IF gv_objnr IS INITIAL.
    EXIT.
  ENDIF.
  PERFORM select_cpzp_data.
  IF gt_cpzp[] IS INITIAL.
    EXIT.
  ENDIF.
  PERFORM process_data.
  IF p_test IS INITIAL.
    PERFORM wirte_cpzp_new.
  ENDIF.

*&---------------------------------------------------------------------*
*& fill ALV-structure and display                                      *
*&---------------------------------------------------------------------*
  IF p_test IS NOT INITIAL.
    PERFORM fill_data.
    PERFORM alv_diplay_cpzp.
  ELSE.
    IF gv_update_error IS INITIAL.
      MESSAGE i411 WITH p_aufnr.
    ELSE.
      MESSAGE i412 WITH p_aufnr.
    ENDIF.
  ENDIF.

************************************************************************
******************************SUBROUTINES*******************************
************************************************************************

***************************Screen Management****************************
*&---------------------------------------------------------------------*
*&      Form  INITIAL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initial_screen .
  p_month = sy-datlo+4(2) - 1.
  IF p_month = 0.
    p_month = 12.
    p_year = p_year - 1.
  ENDIF.
ENDFORM.                    " INITIAL_SCREEN
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
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " SCREEN_CHECK
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

ENDFORM.                    " SCREEN_MODIFY

****************************Data Management*****************************
*&---------------------------------------------------------------------*
*&      Form  SET_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_period .

  CONCATENATE p_year '0' p_month INTO gv_gjper.

ENDFORM.                    " SET_PERIOD
*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJNR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_objnr_data .

  SELECT SINGLE objnr
    INTO gv_objnr
    FROM aufk
   WHERE aufnr = p_aufnr.

ENDFORM.                    " SELECT_OBJNR_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_CPZP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_cpzp_data .

  SELECT *
    INTO TABLE gt_cpzp_backup
    FROM cpzp
   WHERE objnr = gv_objnr
     AND gjper = gv_gjper.

  gt_cpzp[] = gt_cpzp_backup[].

ENDFORM.                    " SELECT_CPZP_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_data .

  LOOP AT gt_cpzp ASSIGNING <fs_cpzp>.
*    <fs_cpzp>-gmsum = <fs_cpzp>-istmn + <fs_cpzp>-xmper.
* Changed by James(Sung-Kon) Kim 2010.09.03
* Because, the "istmn" include the input quantity for the scrap also.
    <fs_cpzp>-gmsum = <fs_cpzp>-istmn.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  WIRTE_CPZP_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_UPDATE_ERROR  text
*----------------------------------------------------------------------*
FORM wirte_cpzp_new.

  DATA  ls_cpzp TYPE cpzp.
  DATA: lt_zcpzp_backup TYPE zsapbf_tt_cpzp_backup,
        ls_zcpzp_backup TYPE zcpzp_backup.
  DATA  lv_time_stamp TYPE tzntstmps.

  CHECK gt_cpzp_backup IS NOT INITIAL.

* fillup ZCPZP_BACKUP
  CONVERT DATE sy-datum
          TIME sy-uzeit
          INTO TIME STAMP lv_time_stamp TIME ZONE sy-zonlo.

  LOOP AT gt_cpzp_backup INTO ls_cpzp.
    MOVE-CORRESPONDING ls_cpzp TO ls_zcpzp_backup.
    ls_zcpzp_backup-backuptime = lv_time_stamp.
    APPEND ls_zcpzp_backup TO lt_zcpzp_backup.
    CLEAR ls_zcpzp_backup.
  ENDLOOP.

* Copy GT_CPZP_TEMP into CPZPTEMP
  IF lt_zcpzp_backup IS NOT INITIAL.
    INSERT zcpzp_backup FROM TABLE lt_zcpzp_backup.
  ENDIF.

  LOOP AT gt_cpzp INTO ls_cpzp.
    UPDATE cpzp SET gmsum = ls_cpzp-gmsum
                      WHERE objnr   EQ ls_cpzp-objnr
                        AND f_objnr EQ ls_cpzp-f_objnr
                        AND gjper   EQ ls_cpzp-gjper
                        AND zaehl   EQ ls_cpzp-zaehl.
  ENDLOOP.

  IF NOT sy-subrc IS INITIAL.
    gv_update_error = 'U'.
    ROLLBACK WORK.
    EXIT.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " WIRTE_CPZP_NEW

******************************ALV Display*******************************
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_data .

  DATA: ls_cpzp_backup LIKE cpzp,
        ls_cpzp        LIKE cpzp.

  LOOP AT gt_cpzp_backup INTO ls_cpzp_backup.
    MOVE-CORRESPONDING ls_cpzp_backup TO gs_alv_cpzp.
    READ TABLE gt_cpzp WITH KEY objnr   = ls_cpzp_backup-objnr
                                f_objnr = ls_cpzp_backup-f_objnr
                                gjper   = ls_cpzp_backup-gjper
                                zaehl   = ls_cpzp_backup-zaehl
                           INTO ls_cpzp.

    IF sy-subrc = 0.
      MOVE: ls_cpzp-istmn TO gs_alv_cpzp-istmn_n,
            ls_cpzp-gmper TO gs_alv_cpzp-gmper_n,
            ls_cpzp-xmper TO gs_alv_cpzp-xmper_n,
            ls_cpzp-gmsum TO gs_alv_cpzp-gmsum_n.
      gs_alv_cpzp-istmn_v = ls_cpzp_backup-istmn - ls_cpzp-istmn.
      gs_alv_cpzp-gmper_v = ls_cpzp_backup-gmper - ls_cpzp-gmper.
      gs_alv_cpzp-xmper_v = ls_cpzp_backup-xmper - ls_cpzp-xmper.
      gs_alv_cpzp-gmsum_v = ls_cpzp_backup-gmsum - ls_cpzp-gmsum.
      APPEND gs_alv_cpzp TO gt_alv_cpzp.
      CLEAR: ls_cpzp, gs_alv_cpzp.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_DIPLAY_CPZP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_diplay_cpzp .
* ALV variables

  DATA: ls_variant  TYPE disvariant,
        lv_alv_structure TYPE slis_tabname VALUE 'GT_ALV_CPZP',
        lt_fieldcat TYPE STANDARD TABLE OF slis_fieldcat_alv,
        ls_layout TYPE  slis_layout_alv.

  DATA lv_display_lines TYPE i.

  ls_layout-colwidth_optimize = charx.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = lv_alv_structure
      i_inclname         = 'ZSAPBF_CPZP_CORRECTION_FORTOP'
    CHANGING
      ct_fieldcat        = lt_fieldcat
    EXCEPTIONS
      OTHERS             = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM fieldcat_change CHANGING lt_fieldcat.


  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.

  lv_display_lines = LINES( gt_cpzp_display ).

  lv_display_lines = lv_display_lines / 1000.

  IF lv_display_lines GT 30.
    lv_display_lines = 30.
  ENDIF.

  WAIT UP TO lv_display_lines SECONDS.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title             = space
      it_fieldcat              = lt_fieldcat
      i_callback_program       = 'ZSAPBF_CPZP_CORRECTION_FORCED' "sy-cprog
*      i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'SET_COMMAND'
      is_variant               = ls_variant
      i_save                   = 'A'
      is_layout                = ls_layout
    TABLES
      t_outtab                 = gt_alv_cpzp"gt_cpzp_display_numpcc
    EXCEPTIONS
      OTHERS                   = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_DIPLAY_CPZP
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fieldcat_change  CHANGING ct_fieldcat TYPE tt_slis_fieldcat_alv.
  FIELD-SYMBOLS: <fs_fieldcat> TYPE slis_fieldcat_alv.

  LOOP AT ct_fieldcat ASSIGNING <fs_fieldcat>.
    CASE <fs_fieldcat>-fieldname.
      WHEN 'ISTMN'.
        <fs_fieldcat>-seltext_l = text-046.
        <fs_fieldcat>-seltext_m = text-046.
        <fs_fieldcat>-seltext_s = text-046.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'GMPER'.
        <fs_fieldcat>-seltext_l = text-047.
        <fs_fieldcat>-seltext_m = text-047.
        <fs_fieldcat>-seltext_s = text-047.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'XMPER'.
        <fs_fieldcat>-seltext_l = text-048.
        <fs_fieldcat>-seltext_m = text-048.
        <fs_fieldcat>-seltext_s = text-048.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'GMSUM'.
        <fs_fieldcat>-seltext_l = text-049.
        <fs_fieldcat>-seltext_m = text-049.
        <fs_fieldcat>-seltext_s = text-049.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'ISTMN_N'.
        <fs_fieldcat>-seltext_l = text-050.
        <fs_fieldcat>-seltext_m = text-050.
        <fs_fieldcat>-seltext_s = text-050.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'GMPER_N'.
        <fs_fieldcat>-seltext_l = text-051.
        <fs_fieldcat>-seltext_m = text-051.
        <fs_fieldcat>-seltext_s = text-051.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'XMPER_N'.
        <fs_fieldcat>-seltext_l = text-052.
        <fs_fieldcat>-seltext_m = text-052.
        <fs_fieldcat>-seltext_s = text-052.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'GMSUM_N'.
        <fs_fieldcat>-seltext_l = text-053.
        <fs_fieldcat>-seltext_m = text-053.
        <fs_fieldcat>-seltext_s = text-053.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'ISTMN_V'.
        <fs_fieldcat>-seltext_l = text-056.
        <fs_fieldcat>-seltext_m = text-056.
        <fs_fieldcat>-seltext_s = text-056.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'GMPER_V'.
        <fs_fieldcat>-seltext_l = text-057.
        <fs_fieldcat>-seltext_m = text-057.
        <fs_fieldcat>-seltext_s = text-057.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'XMPER_V'.
        <fs_fieldcat>-seltext_l = text-058.
        <fs_fieldcat>-seltext_m = text-058.
        <fs_fieldcat>-seltext_s = text-058.
        <fs_fieldcat>-ddictxt = 'S'.
      WHEN 'GMSUM_V'.
        <fs_fieldcat>-seltext_l = text-059.
        <fs_fieldcat>-seltext_m = text-059.
        <fs_fieldcat>-seltext_s = text-059.
        <fs_fieldcat>-ddictxt = 'S'.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " FIELDCAT_CHANGE
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_pf_status                                          "#EC CALLED
                  USING rt_extab TYPE slis_t_extab .        "#EC NEEDED
  SET PF-STATUS 'ZSAPBF_WIP_STATUS'.
ENDFORM.                    " SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  SET_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COMMAND  text
*----------------------------------------------------------------------*
FORM set_command  USING    piv_ucomm     TYPE syucomm       "#EC CALLED
                           pis_selfield TYPE slis_selfield.
*  DATA lt_cpzp_display TYPE STANDARD TABLE OF zsapbf_cpzp_display.
  CASE piv_ucomm.
*    WHEN '&CRB'.
    WHEN '&CRL'. " Previous page
      CLEAR gt_cpzp_display_numpcc.
      PERFORM fill_display_cpzp USING 'P'
                          CHANGING gt_cpzp_display_numpcc.
    WHEN '&CRR'. " Next page
      CLEAR gt_cpzp_display_numpcc.
      PERFORM fill_display_cpzp USING 'N'
                          CHANGING gt_cpzp_display_numpcc.
*    WHEN '&CRE'.
    WHEN '&CRB'. " First page
      CLEAR gt_cpzp_display_numpcc.
      PERFORM fill_display_cpzp USING 'F'
                          CHANGING gt_cpzp_display_numpcc.
    WHEN '&CRE'. " Last page
      CLEAR gt_cpzp_display_numpcc.
      PERFORM fill_display_cpzp USING 'L'
                          CHANGING gt_cpzp_display_numpcc.
    WHEN OTHERS.
  ENDCASE.
  pis_selfield-refresh = 'X'.
ENDFORM.                    " SET_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FILL_DISPLAY_CPZP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CPZP_DISPLAY  text
*      <--P_LT_CPZP_DISPLAY  text
*----------------------------------------------------------------------*
FORM fill_display_cpzp  USING iv_idx_type TYPE c
                        CHANGING pet_cpzp_display TYPE zsapbf_tt_cpzp_display.
  DATA ls_pcc_obj TYPE gty_pcc_obj_s.
  DATA ls_cpzp_display TYPE zsapbf_cpzp_display.
  DATA lv_max_num TYPE i.
  DATA lv_last_idx_num TYPE i.

  CHECK NOT gv_num_pcc IS INITIAL.

* Number of lines in all result list
  lv_max_num = LINES( gt_pcc_obj ).

* Number of lines in last page.
  lv_last_idx_num = lv_max_num MOD gv_num_pcc.

  IF lv_last_idx_num = 0.
    lv_last_idx_num = gv_num_pcc.
  ENDIF.

  CASE iv_idx_type.
    WHEN 'P'. " Previous page
      IF gv_pcc_idx = lv_max_num.
        gv_pcc_idx = gv_pcc_idx - lv_last_idx_num - gv_num_pcc.
      ELSE.
        gv_pcc_idx = gv_pcc_idx - 2 * gv_num_pcc.
      ENDIF.
      IF gv_pcc_idx < 0.
        gv_pcc_idx = 0.
      ENDIF.
    WHEN 'N'. " Next page
      IF gv_pcc_idx = lv_max_num.
        gv_pcc_idx = lv_max_num - lv_last_idx_num.
        IF gv_pcc_idx < 0.
          gv_pcc_idx = 0.
        ENDIF.
      ENDIF.
    WHEN 'F'. " First page
      gv_pcc_idx = 0.
    WHEN 'L'. " Last page
      gv_pcc_idx = lv_max_num - lv_last_idx_num.

    WHEN OTHERS.
  ENDCASE.

  DO gv_num_pcc TIMES.
    IF gv_pcc_idx GE lv_max_num.
      EXIT.
    ENDIF.
    gv_pcc_idx = gv_pcc_idx + 1.
    READ TABLE gt_pcc_obj INTO ls_pcc_obj INDEX gv_pcc_idx.
    CHECK sy-subrc IS INITIAL.
    LOOP AT gt_cpzp_display INTO ls_cpzp_display WHERE objnr = ls_pcc_obj-objnr.
      APPEND ls_cpzp_display TO pet_cpzp_display.
    ENDLOOP.
  ENDDO.

ENDFORM.                    " FILL_DISPLAY_CPZP
