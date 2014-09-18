FUNCTION zfpp_asmp_pr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  ZRESULT
*"     VALUE(E_MSG) TYPE  BAPI_MSG
*"  TABLES
*"      T_ASMP_PR STRUCTURE  ZSPP_ASMPPR
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCE_FAILURE
*"----------------------------------------------------------------------
  DATA : it_data   LIKE ztpp_asmppr   OCCURS 0 WITH HEADER LINE,
         it_data_h LIKE ztpp_asmppr_h OCCURS 0 WITH HEADER LINE,
         wa_asmppr TYPE ztpp_asmppr.

  DATA : BEGIN OF it_duplicate  OCCURS 0,
            werks LIKE ztpp_asmppr-werks,
            matnr LIKE ztpp_asmppr-matnr,
            rdatu LIKE ztpp_asmppr-rdatu,
            tseq  LIKE ztpp_asmppr-tseq,
            cnt   TYPE i,
        END OF it_duplicate.

  DATA : l_datum TYPE sy-datum,
         l_update_error(1).

  CLEAR : it_duplicate[], it_duplicate, it_data[], it_data,
          it_data_h[],    it_data_h, l_update_error.

  IF t_asmp_pr[] IS INITIAL.
    e_result  = 'S'.
    e_msg     = 'No Interface Data'.
    EXIT.
  ENDIF.


  PERFORM locking_rtn USING sy-repid e_result.

  IF e_result = 'E'.
    MOVE: 'Previous I/F was not finished' TO e_msg.
    EXIT.
  ENDIF.

*-duplicate check
  LOOP AT t_asmp_pr.
    if t_asmp_pr-werks = '1'.
      t_asmp_pr-werks = 'P001'.
    endif.

    MOVE-CORRESPONDING t_asmp_pr TO it_duplicate.
    it_duplicate-cnt  = 1.

    COLLECT it_duplicate.
    modify t_asmp_pr.
  ENDLOOP.

  DELETE it_duplicate WHERE cnt <= 1.
  SORT it_duplicate BY werks matnr  rdatu tseq.


  SORT t_asmp_pr BY werks rdatu tseq.

  LOOP AT t_asmp_pr.

*-Commented Check logic during Intergation Test on 07.18.2014 by MIT

*    AT NEW werks.
*      PERFORM check_plant USING t_asmp_pr
*                          CHANGING t_asmp_pr-zresult t_asmp_pr-zmsg.
*    ENDAT.
*
*    PERFORM check_material USING t_asmp_pr
*                           CHANGING t_asmp_pr-zresult t_asmp_pr-zmsg.
*
*    IF t_asmp_pr-tseq IS INITIAL.
*      t_asmp_pr-zresult = 'E'.
*      t_asmp_pr-zmsg    = 'Total SEQ is initial'.
*    ENDIF.
*
*    IF t_asmp_pr-line IS INITIAL.
*      t_asmp_pr-zresult = 'E'.
*      t_asmp_pr-zmsg    = 'Production Line is initial'.
*    ENDIF.
*
**-  duplicate check
*    READ TABLE it_duplicate WITH KEY werks =  t_asmp_pr-werks
*                                     matnr =  t_asmp_pr-matnr
*                                     rdatu =  t_asmp_pr-rdatu
*                                     tseq  =  t_asmp_pr-tseq
*                                     BINARY SEARCH.
*    IF sy-subrc = 0.
*      t_asmp_pr-zresult = 'E'.
*      t_asmp_pr-zmsg    = 'Key field is duplicated'.
*    ENDIF.

    MOVE-CORRESPONDING t_asmp_pr TO it_data.
    it_data-zuser = sy-uname.
    it_data-zedat = it_data-zsdat = sy-datum.
    it_data-zetim = it_data-zstim = sy-uzeit.

    MOVE-CORRESPONDING it_data TO it_data_h.

*-  duplicate from table   ??????????
    SELECT SINGLE * INTO wa_asmppr
    FROM ztpp_asmppr
    WHERE werks =  t_asmp_pr-werks
      AND matnr =  t_asmp_pr-matnr
      AND rdatu =  t_asmp_pr-rdatu
      AND tseq  =  t_asmp_pr-tseq.
    IF sy-subrc = 0.
      IF wa_asmppr-aufnr IS INITIAL.
        t_asmp_pr-zresult = 'S'.
        it_data-zresult    = 'I'.
        it_data-zmsg       = ''.
        it_data_h-zresult  = 'S'.
        it_data_h-zmsg     = ''.
      ELSE.
        IF wa_asmppr-pqty = t_asmp_pr-pqty.
          t_asmp_pr-zresult = 'S'.
          it_data_h-zresult  = 'S'.
          it_data_h-zmsg     = ''.
        ELSE.
          t_asmp_pr-zresult = 'E'.
          t_asmp_pr-zmsg    = 'Processed already'.
          it_data_h-zresult  = 'E'.
          it_data_h-zmsg    = 'Processed already'.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY t_asmp_pr.

    APPEND it_data.
    APPEND it_data_h.
  ENDLOOP.

  SORT it_data BY werks matnr  rdatu tseq.
  SORT it_data_h BY werks matnr  rdatu tseq.
  DELETE ADJACENT DUPLICATES FROM it_data
                              COMPARING werks matnr  rdatu tseq.
  DELETE ADJACENT DUPLICATES FROM it_data_h
                              COMPARING werks matnr  rdatu tseq.

*-table update
  l_datum = sy-datum - 90.
  DELETE FROM ztpp_asmppr_h WHERE zedat < l_datum.

  MODIFY ztpp_asmppr FROM TABLE it_data.
  IF sy-subrc <> 0.
    l_update_error  = 'X'.

  ELSE.
    INSERT ztpp_asmppr_h FROM TABLE it_data_h.
    IF sy-subrc <> 0.
      l_update_error  = 'X'.
    ENDIF.
  ENDIF.

  IF l_update_error  = 'X'.
    e_result = 'E'.
    e_msg    = 'Update failed for Prod. Logs history'.
    ROLLBACK WORK.
  ELSE.
    e_result  = 'S'.
    COMMIT WORK.
  ENDIF.
ENDFUNCTION.
