FUNCTION z_fpp_engine_pr_2.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ENGINE STRUCTURE  ZSPPER2_RFC
*"----------------------------------------------------------------------
  DATA : it_ztpper2    LIKE TABLE OF ztpper2 WITH HEADER LINE,
          it_ztpperm    LIKE TABLE OF ztpperm WITH HEADER LINE.
  DATA : l_tabix      TYPE sy-tabix  .


  DATA: wa_matnr LIKE ztpper2-eitem,
        l_matnr LIKE ztpper2-eitem.
*
  DATA: wa_ztpper2       LIKE ztpper2.

  DATA:  l_msgtyp(1),
         l_msg LIKE cfgnl-msglin.


*----> Check PP Log
  CLEAR : ztpp_if_status .
  SELECT SINGLE *
              FROM ztpp_if_status
              WHERE tabname EQ 'ZTPPER' .
  IF ztpp_if_status-zgo EQ 'X' .
    t_engine-zzret = 'E' .
    MODIFY t_engine TRANSPORTING zzret WHERE zzret EQ space .
  ENDIF.

  CHECK ztpp_if_status-zgo NE 'X' .
  LOOP AT t_engine .
    MOVE sy-tabix       TO      l_tabix           .
    MOVE sy-mandt       TO      t_engine-mandt    .
    MOVE 'I'            TO      t_engine-zresult  .
    MOVE-CORRESPONDING t_engine  TO  it_ztpper2    .
** Changed by Furong on 06/19/08
    IF t_engine-eassyid IS INITIAL.
      CASE t_engine-erpid.
** Changed by Furon on 01/14/09
*        WHEN 'E01'.
*          IT_ZTPPER2-EASSYID = T_ENGINE-BLOCK_SRL_NO.
*        WHEN 'E02'.
*          IT_ZTPPER2-EASSYID = T_ENGINE-CRANK_SRL_NO.
*        WHEN 'E03'.
*          IT_ZTPPER2-EASSYID = T_ENGINE-HEAD_SRL_N.
        WHEN 'E01' OR 'E91'.
          it_ztpper2-eassyid = t_engine-block_srl_no.
        WHEN 'E02' OR 'E92'.
          it_ztpper2-eassyid = t_engine-crank_srl_no.
        WHEN 'E03' OR 'E93'.

** Changed by Furong on 07/22/09
*          IT_ZTPPER2-EASSYID = T_ENGINE-HEAD_SRL_N.
          IF it_ztpper2-head_srl_n IS INITIAL.
            it_ztpper2-eassyid = t_engine-head_srl_n_rh.
          ELSE.
            it_ztpper2-eassyid = t_engine-head_srl_n.
          ENDIF.
** End of change on 07/22/09

** End of change
      ENDCASE.
    ENDIF.
** End of change
    INSERT ztpper2 FROM it_ztpper2.
    IF sy-subrc EQ 0.
      MOVE  'S'         TO      t_engine-zzret    .
      MODIFY t_engine   INDEX   l_tabix           .
    ELSE.
      MOVE  'E'         TO      t_engine-zzret    .
      MODIFY t_engine   INDEX   l_tabix           .
    ENDIF.
    CLEAR : it_ztpper2 .
  ENDLOOP.


** Added by Furong on 08/17/09 , process E09, E10 and E98.
  REFRESH it_ztpper2.
  CLEAR: it_ztpper2.

  LOOP AT t_engine.

* by ig.moon 5/25/2012 {
    IF t_engine-zzret EQ 'E'.
      CONTINUE.
    ENDIF.
* }

    IF t_engine-erpid = 'E09' OR
       t_engine-erpid = 'E10' OR
       t_engine-erpid = 'E98'.
      MOVE-CORRESPONDING t_engine TO it_ztpper2.
      APPEND it_ztpper2.
    ENDIF.
  ENDLOOP.


  LOOP AT it_ztpper2.
    IF wa_matnr = it_ztpper2-eitem.
      WAIT UP TO 3 SECONDS .
    ENDIF.

    l_tabix = sy-tabix.

    SELECT SINGLE matnr INTO l_matnr
              FROM mara
              WHERE matnr EQ it_ztpper2-eitem .
    IF sy-subrc EQ 0.

      CASE it_ztpper2-erpid.
        WHEN 'E09'.
          PERFORM process_e09 USING it_ztpper2
                              CHANGING l_msgtyp l_msg.
*
*          IF L_MSGTYP = 'S'.
*            PERFORM POST_MAT_MB1B  USING IT_ZTPPER2
*                              CHANGING L_MSGTYP L_MSG.
*
*          ENDIF.
*
*        PERFORM EN_CID_E09 tables iT_ZTPPER2.
        WHEN 'E10'.
          PERFORM process_e10 USING it_ztpper2
                              CHANGING l_msgtyp l_msg.
          .
        WHEN 'E98'.    "
          PERFORM process_e98  USING it_ztpper2
                              CHANGING l_msgtyp l_msg.

      ENDCASE.
    ELSE.
      l_msgtyp = 'E'.
      l_msg = text-201.   "Material Code is Invalid!!
    ENDIF.

    it_ztpper2-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
    it_ztpper2-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
    it_ztpper2-zbnam = sy-uname.  "BDC User ID
*      IT_ZTPPER2-ZMODE    = 'C'.       "C:CREATE
    it_ztpper2-zmsg     = l_msg.
    it_ztpper2-zresult  = l_msgtyp.

    MODIFY it_ztpper2 INDEX l_tabix TRANSPORTING zbdat
                                            zbtim
                                            zbnam
                                            zmode
                                            zresult
                                            zmsg.

    MOVE-CORRESPONDING it_ztpper2 TO wa_ztpper2.

    UPDATE ztpper2 FROM wa_ztpper2.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    ENDIF.
    CLEAR: wa_ztpper2.

    wa_matnr = it_ztpper2-eitem.
    MOVE-CORRESPONDING it_ztpper2 TO it_ztpperm.
    APPEND it_ztpperm.
  ENDLOOP.

  MODIFY ztpperm FROM TABLE it_ztpperm.
  IF sy-subrc = 0.
  ENDIF.
** End of addition

ENDFUNCTION.
