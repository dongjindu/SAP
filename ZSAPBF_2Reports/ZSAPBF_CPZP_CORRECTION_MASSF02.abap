*----------------------------------------------------------------------*
***INCLUDE ZSAPBF_CPZP_CORRECTION_MASSF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CPZP_BACKUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SEL  text
*      -->P_P_ALL  text
*      -->P_P_FULL  text
*----------------------------------------------------------------------*
FORM cpzp_backup USING iv_year
                       iv_month
                       iv_sel
*                       iv_all
                       iv_full.
  DATA: lt_aufnr TYPE tt_aufnr,
        ls_aufnr TYPE ts_aufnr.

  DATA: rt_objnr TYPE RANGE OF objnr,
        rs_objnr LIKE LINE OF rt_objnr.

  DATA: rt_gjper TYPE RANGE OF co_gjper,
        rs_gjper LIKE LINE OF rt_gjper.

  DATA: lv_gjper_low TYPE co_gjper,
        lv_gjper_high TYPE co_gjper.

  DATA: lt_cpzp TYPE zsapbf_tt_cpzp,
        ls_cpzp TYPE cpzp,

        lt_cpzp_backup TYPE zsapbf_tt_cpzp_backup,
        ls_cpzp_backup TYPE zcpzp_backup.

  DATA: "lv_aufnr TYPE aufnr,
        lv_update_error TYPE char1.

  DATA: lv_time_stamp TYPE tzntstmps.

  IF iv_sel = 'X'.
    PERFORM get_pcc USING iv_year iv_month CHANGING lt_aufnr.
* Fillup P.C.C
    LOOP AT lt_aufnr INTO ls_aufnr.
      rs_objnr-sign = 'I'.
      rs_objnr-option = 'EQ'.
      CONCATENATE 'OR' ls_aufnr-aufnr INTO rs_objnr-low.
      CONDENSE rs_objnr-low NO-GAPS.

      APPEND rs_objnr TO rt_objnr.
      CLEAR rs_objnr.
    ENDLOOP.
  ELSE.

  ENDIF.

* Fillup period
  IF iv_full = 'X'.
    " There is no source code "James Sung-Kon Kim 2011/02/21
    " It might takes too much long time if full backup is performed.
  ELSE.
    DO 2 TIMES.
      IF gv_gjper_prev+4(3) NE '001'.
        gv_gjper_prev = gv_gjper_prev - 1.
      ELSE.
        gv_gjper_prev+4(3) = '012'.
        gv_gjper_prev(4) = gv_gjper_prev(4) - 1.
      ENDIF.
    ENDDO.

    lv_gjper_high = gv_gjper_curr.
    lv_gjper_low = gv_gjper_prev.

    rs_gjper-sign = 'I'.
    rs_gjper-option = 'BT'.
    rs_gjper-low = lv_gjper_low.
    rs_gjper-high = lv_gjper_high.
    APPEND rs_gjper TO rt_gjper.
    CLEAR rs_gjper.
  ENDIF.

  SELECT * FROM cpzp
           INTO TABLE lt_cpzp
          WHERE objnr IN rt_objnr
            AND gjper IN rt_gjper.


  CHECK lt_cpzp IS NOT INITIAL.
  SORT lt_cpzp BY objnr gjper.

  CONVERT DATE sy-datum
          TIME sy-uzeit
          INTO TIME STAMP lv_time_stamp TIME ZONE sy-zonlo. "'UTC   '.

  LOOP AT lt_cpzp INTO ls_cpzp.
    MOVE-CORRESPONDING ls_cpzp TO ls_cpzp_backup.           "#EC ENHOK
    ls_cpzp_backup-backuptime = lv_time_stamp.
    APPEND ls_cpzp_backup TO lt_cpzp_backup.
    CLEAR ls_cpzp_backup.
  ENDLOOP.
* Lock the P.C.C
*&---------------------------------------------------------------------*
*&      Form  write_cpzp
*&---------------------------------------------------------------------*
* Copy GT_CPZP_TEMP into CPZPTEMP
  IF lt_cpzp_backup IS NOT INITIAL.
    INSERT zcpzp_backup FROM TABLE lt_cpzp_backup.
    IF NOT sy-subrc IS INITIAL.
      lv_update_error = 'U'.
      ROLLBACK WORK.
      EXIT.
    ELSE.
      COMMIT WORK.
    ENDIF.
  ENDIF.


  IF lv_update_error IS INITIAL.
    MESSAGE i413 WITH lv_time_stamp.
  ELSE.
    MESSAGE i414.
*    WRITE:/ text-051.
*    WRITE:/ sy-uline(255).
*    WRITE:/ text-056, lv_time_stamp, text-055.
  ENDIF.
ENDFORM.                    " CPZP_BACKUP
**&---------------------------------------------------------------------*
**&      Form  enqueue
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM enqueue USING iv_objnr TYPE objnr
*          CHANGING ev_error TYPE char1.
*  DATA: lv_aufnr TYPE qrp002-aufnr.
** it's safe to lock all periods of this P.C.C
*  lv_aufnr = iv_objnr+2.
*
*  CALL FUNCTION 'ENQUEUE_E_QRP002'
*    EXPORTING
*      mode_qrp002    = 'E'
*      aufnr          = lv_aufnr
*      _scope         = '3'
*      _wait          = 'X'
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.
*  IF sy-subrc <> 0.
*    ev_error = 'L'.
**    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*ENDFORM.                    " enqueue
**&---------------------------------------------------------------------*
**&      Form  dequeue
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM dequeue USING iv_objnr TYPE objnr.
*  DATA: lv_aufnr TYPE qrp002-aufnr.
*
*  lv_aufnr = iv_objnr+2.
*  CALL FUNCTION 'DEQUEUE_E_QRP002'
*    EXPORTING
*      mode_qrp002 = 'E'
*      aufnr       = lv_aufnr
*      _scope      = '3'.
*
*ENDFORM.                    " dequeue
