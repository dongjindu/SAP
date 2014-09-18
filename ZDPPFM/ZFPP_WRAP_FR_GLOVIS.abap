FUNCTION zfpp_wrap_fr_glovis.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(RESULT) TYPE  ZRESULT
*"     VALUE(MESSAGE) TYPE  ZMSG
*"  TABLES
*"      T_WRAP_MST STRUCTURE  ZSPP_WRAP_MST
*"----------------------------------------------------------------------

  DATA: it_wrap_mst LIKE TABLE OF ztpp_wrap_master WITH HEADER LINE,
        it_wrap_mst_h LIKE TABLE OF ztpp_wrap_mst_h WITH HEADER LINE.

  DATA: l_cn TYPE i,
        l_date_del TYPE sy-datum.

  DESCRIBE TABLE t_wrap_mst LINES l_cn.

  IF l_cn = 0.
    MOVE: 'S' TO result,
          'No interface data' TO message.
    EXIT.
  ENDIF.

  PERFORM locking_rtn USING sy-repid result.

  IF result = 'E'.
    MOVE: 'Previous I/F was not finished' TO message.
    EXIT.
  ENDIF.

  SORT t_wrap_mst BY dealer rec_id.

  REFRESH it_dealer.

  SELECT dealer INTO TABLE it_dealer
    FROM ztpp_wrap_master
    FOR ALL ENTRIES IN t_wrap_mst
    WHERE dealer =  t_wrap_mst-dealer.

  LOOP AT t_wrap_mst.
    MOVE-CORRESPONDING t_wrap_mst TO it_wrap_mst.
    it_wrap_mst-zuser = sy-uname.
    it_wrap_mst-zedat = sy-datum.
    it_wrap_mst-zetim = sy-uzeit.
    it_wrap_mst-zresult = 'I'.

    MOVE-CORRESPONDING t_wrap_mst TO it_wrap_mst_h.
    it_wrap_mst_h-zuser = sy-uname.
    it_wrap_mst_h-zedat = sy-datum.
    it_wrap_mst_h-zetim = sy-uzeit.
    it_wrap_mst_h-zresult = 'I'.

    READ TABLE it_dealer WITH TABLE KEY dealer = t_wrap_mst-dealer.
    IF sy-subrc = 0.
      it_wrap_mst-zmode   = 'U'.
      it_wrap_mst_h-zmode = 'U'.
    ELSE.
      it_wrap_mst-zmode   = 'C'.
      it_wrap_mst_h-zmode = 'C'.
    ENDIF.
    APPEND it_wrap_mst.
    APPEND it_wrap_mst_h.
  ENDLOOP.

  REFRESH it_dealer.

  l_date_del = sy-datum - 90.
  DELETE FROM ztpp_wrap_mst_h WHERE zedat < l_date_del.
  INSERT ztpp_wrap_mst_h FROM TABLE it_wrap_mst_h
                        ACCEPTING DUPLICATE KEYS .
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MODIFY ztpp_wrap_master FROM TABLE it_wrap_mst.
    IF sy-subrc = 0.
      COMMIT WORK.

      LOOP AT it_wrap_mst.
        it_um_dealer-wo_dealer1 = it_wrap_mst-dealer.
        COLLECT it_um_dealer.
      ENDLOOP.

      MOVE: 'S'   TO result,
            space TO message.

      PERFORM submit_alc_interface tables it_wrap_mst
                                          it_wrap_mst_h.
    ELSE.
      MOVE: 'E' TO result,
            'Update failed for WRAP master' TO message.
      ROLLBACK WORK.
    ENDIF.
  ELSE.
    MOVE:'E' TO result,
         'Update failed for WRAP master history' TO message.
    ROLLBACK WORK.
  ENDIF.
ENDFUNCTION.
