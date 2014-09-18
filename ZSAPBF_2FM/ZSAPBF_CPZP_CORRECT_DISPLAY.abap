FUNCTION zsapbf_cpzp_correct_display.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TEST) TYPE  FLAG DEFAULT 'X'
*"     VALUE(IV_ERROR) TYPE  FLAG
*"     VALUE(IV_NUM_PCC) TYPE  INT1 DEFAULT 1
*"  TABLES
*"      IT_CPZP TYPE  ZSAPBF_TT_CPZP OPTIONAL
*"      IT_CPZP_BK TYPE  ZSAPBF_TT_CPZP OPTIONAL
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& fill ALV-structure and display                                      *
*&---------------------------------------------------------------------*
  DATA lt_pcc_obj TYPE gty_pcc_obj_t.
  DATA ls_pcc_obj TYPE gty_pcc_obj_s.

  DATA ls_cpzp    TYPE cpzp.
  DATA ls_cpzp_bk TYPE cpzp.
  DATA lt_cpzp TYPE zsapbf_tt_cpzp.
  DATA lt_cpzp_backup TYPE zsapbf_tt_cpzp.

  DATA lv_display_idx TYPE i VALUE 1.
  DATA lv_max_num_pcc TYPE i VALUE 0.

  IF gt_cpzp[] IS INITIAL.
    gt_cpzp[] = it_cpzp[].
    gt_cpzp_backup[] = it_cpzp_bk[].
  ENDIF.

** Calculate number of PCC.
*  SORT gt_cpzp BY objnr.
*  LOOP AT gt_cpzp INTO ls_cpzp.
*    AT NEW objnr.
*      ls_pcc_obj-objnr = ls_cpzp-objnr.
*      APPEND ls_pcc_obj TO lt_pcc_obj.
*    ENDAT.
*  ENDLOOP.
*
*  lv_max_num_pcc = LINES( lt_pcc_obj ).
*
*  WHILE lv_display_idx LE lv_max_num_pcc.
*    CLEAR: ls_pcc_obj, ls_cpzp, ls_cpzp_bk.
*    REFRESH: lt_cpzp, lt_cpzp_backup.
*
*    DO iv_num_pcc TIMES.
*      READ TABLE lt_pcc_obj INTO ls_pcc_obj INDEX lv_display_idx.
*      IF sy-subrc IS INITIAL.
*        LOOP AT gt_cpzp INTO ls_cpzp WHERE objnr = ls_pcc_obj-objnr.
*          APPEND ls_cpzp TO lt_cpzp.
*        ENDLOOP.
*        LOOP AT lt_cpzp_backup INTO ls_cpzp_bk WHERE objnr = ls_pcc_obj-objnr..
*          APPEND ls_cpzp_bk TO lt_cpzp_backup.
*        ENDLOOP.
*        lv_display_idx = lv_display_idx + 1.
*      ELSE.
*        lv_display_idx = lv_display_idx + 1.
*        CONTINUE.
*      ENDIF.
*    ENDDO.
*
*    IF iv_test IS NOT INITIAL.
*      IF lt_cpzp[] IS NOT INITIAL.
*        PERFORM alv_cpzp_output IN PROGRAM  zsapbf_cpzp_correction_report
*                                USING iv_error
*                                      lt_cpzp
*                                      lt_cpzp_backup
*                                      .
*      ELSE.
*        MESSAGE i415(zsapbf_qrprp).
*      ENDIF.
*    ENDIF.
*
*  ENDWHILE.

  IF iv_test IS NOT INITIAL.
    IF gt_cpzp IS NOT INITIAL.
** HMMA Tuning
*      PERFORM alv_cpzp_output IN PROGRAM  zsapbf_cpzp_correction_report
            PERFORM alv_cpzp_output IN PROGRAM  zsapbf_cpzp_correction_report2
** Tuning End
                              USING    iv_error
                                       gt_cpzp
                                       iv_num_pcc
                              CHANGING gt_cpzp_backup
                                    .
    ELSE.
      MESSAGE i415(zsapbf_qrprp).
    ENDIF.
  ENDIF.
ENDFUNCTION.
