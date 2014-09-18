*----------------------------------------------------------------------*
***INCLUDE ZSAPBF_PPC0_OCDF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_ORDDAY      Order finished days ago
*      -->IV_CONFDS      Backflush Entered Days Ago
*      -->IV_DATUM       Current Date of Application Server
*      -->IV_ZONLO       Time Zone of Current User
*      -->IV_BLOCK_SIZE  text
*      <--EV_SELORDTIME  Order finished timestamp
*      <--EV_SELCONFTIME Backflush Entered timestamp
*      <--CV_BLSIZE      text
*----------------------------------------------------------------------*
FORM init_data  USING    "iv_ordday      TYPE zsapbf_ppc_order_days_in_past
                         iv_confds      TYPE zsapbf_ppc_conf_days_in_past
                         iv_time        TYPE syuzeit
                         iv_datum       TYPE sydatum
*                         iv_zonlo       TYPE systzonlo
*                         iv_block_size  TYPE zsapbf_ppc_block_size
                CHANGING "ev_selordtime  TYPE ppc_conftime
                         ev_selconftime TYPE ppc_conftime.
*                         cv_blsize      TYPE zsapbf_ppc_block_size.

  CLEAR: "ev_selordtime,
         ev_selconftime.

* Convert time stamp
  PERFORM convert_timestamp USING    "iv_ordday
                                     iv_confds
                                     iv_time
                                     iv_datum
*                                     iv_zonlo
                            CHANGING "ev_selordtime
                                     ev_selconftime.

** Set initial block size
*  IF cv_blsize IS INITIAL.
*    cv_blsize = iv_block_size.
*  ENDIF.

ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  TIMESTAMP_CONVERT
*&---------------------------------------------------------------------*
*       Convert days in selection criteria to timestamp
*----------------------------------------------------------------------*
*      -->IV_ORDDAY      Order finished days ago
*      -->IV_CONFDS      Backflush Entered Days Ago
*      -->IV_DATUM       Current Date of Application Server
*      -->IV_ZONLO       Time Zone of Current User
*      <--EV_SELORDTIME  Order finished timestamp
*      <--EV_SELCONFTIME Backflush Entered timestamp
*----------------------------------------------------------------------*
FORM convert_timestamp  USING    "iv_ordday      TYPE zsapbf_ppc_order_days_in_past
                                 iv_confds      TYPE zsapbf_ppc_conf_days_in_past
                                 iv_time        TYPE syuzeit
                                 iv_datum       TYPE sydatum
*                                 iv_zonlo       TYPE systzonlo
                        CHANGING "ev_selordtime  TYPE ppc_conftime
                                 ev_selconftime TYPE ppc_conftime.

  DATA  lv_datum TYPE sydatum.

*  lv_datum = iv_datum - iv_ordday.
*  CONVERT DATE lv_datum TIME iv_time INTO
*          TIME STAMP ev_selordtime TIME ZONE sy-zonlo. "'UTC   '.
  lv_datum = iv_datum - iv_confds.
  CONVERT DATE lv_datum TIME iv_time INTO
          TIME STAMP ev_selconftime TIME ZONE sy-zonlo. "'UTC   '.

ENDFORM.                    " TIMESTAMP_CONVERT
*&---------------------------------------------------------------------*
*&      Form  LOG_CREATE
*&---------------------------------------------------------------------*
*       Create application log
*----------------------------------------------------------------------*
*      -->IV_DATLO       Local Date for Current User
*      -->IV_TIMLO       Local Time for Current User
*      -->IV_UNAME       Current User
*      -->IV_TCODE       Current Transaction
*      -->IV_REPID       Current Report
*      <--EV_LOG_HANDLE  Log handle ID
*----------------------------------------------------------------------*
FORM log_create USING    iv_datlo      TYPE systdatlo
                         iv_timlo      TYPE systtimlo
                         iv_uname      TYPE syuname
                         iv_tcode      TYPE sytcode
                         iv_repid      TYPE syrepid
*                         iv_al_extnum  TYPE balnrext
                         iv_al_object  TYPE balobj_d
                         iv_al_subobj  TYPE balsubobj
                CHANGING ev_log_handle TYPE balloghndl.

  DATA  ls_log TYPE bal_s_log.

*  ls_log-extnumber = iv_al_extnum.
  ls_log-object    = iv_al_object.
  ls_log-subobject = iv_al_subobj.
  ls_log-aldate    = iv_datlo.
  ls_log-altime    = iv_timlo.
  ls_log-aluser    = iv_uname.
  ls_log-altcode   = iv_tcode.
  ls_log-alprog    = iv_repid.
* create an initial log file
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = ev_log_handle
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " LOG_CREATE
*&---------------------------------------------------------------------*
*&      Form  add_msg_free
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_MSGTY     text
*      -->IV_DETLEVEL  text
*      -->IV_PROBCLAS  text
*      -->IV_TEXT      text
*----------------------------------------------------------------------*
FORM add_msg USING    iv_msgid     TYPE symsgid
                      iv_msgno     TYPE symsgno
                      iv_msgty     TYPE symsgty
                      iv_msgv1     TYPE any "symsgv
                      iv_msgv2     TYPE any "symsgv
                      iv_msgv3     TYPE any "symsgv
                      iv_msgv4     TYPE any "symsgv
                      iv_exist     TYPE xfeld
*                      iv_detlevel TYPE ballevel
*                      iv_probclas TYPE balprobcl
                      iv_msg_text  TYPE bapi_msg
             CHANGING ev_log_exist TYPE xfeld.

  DATA:
    ls_msg    TYPE bal_s_msg,
    ls_string TYPE gty_s_string.

**********************************************************************
* create a message
**********************************************************************
**  Add free msg
  ls_msg-msgty     = iv_msgty.
  ls_msg-msgid     = iv_msgid.
  ls_msg-msgno     = iv_msgno.
  IF NOT iv_msg_text IS INITIAL.
    ls_string        = iv_msg_text.
    ls_msg-msgv1     = ls_string-part1.
    ls_msg-msgv2     = ls_string-part2.
    ls_msg-msgv3     = ls_string-part3.
    ls_msg-msgv4     = ls_string-part4.
  ELSE.
    ls_msg-msgv1     = iv_msgv1.
    ls_msg-msgv2     = iv_msgv2.
    ls_msg-msgv3     = iv_msgv3.
    ls_msg-msgv4     = iv_msgv4.
  ENDIF.

  CONDENSE: ls_msg-msgv1, ls_msg-msgv2,
            ls_msg-msgv3, ls_msg-msgv4.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
         EXPORTING
*             I_LOG_HANDLE        = iv_LOG_HANDLE
              i_s_msg             = ls_msg
         EXCEPTIONS
               OTHERS              = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  ev_log_exist = iv_exist.

ENDFORM.                    " message_add
*&---------------------------------------------------------------------*
*&      Form  log_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LOG_HANDLE  text
*----------------------------------------------------------------------*
FORM log_show USING iv_log_handle TYPE balloghndl
                    iv_true       TYPE xfeld.

  DATA:
        lt_log_handle      TYPE bal_t_logh,
        ls_display_profile TYPE bal_s_prof.

  APPEND iv_log_handle TO lt_log_handle.

* get display profile
  CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
    IMPORTING
      e_s_display_profile = ls_display_profile.

* Adopt display
  CLEAR ls_display_profile.
  ls_display_profile-tree_ontop = iv_true.
  ls_display_profile-not_empty  = iv_true.
  ls_display_profile-title      = text-010.
  ls_display_profile-cwidth_opt = iv_true.
  ls_display_profile-show_all   = iv_true.
* use grid for display if wanted
  ls_display_profile-use_grid   = iv_true.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile = ls_display_profile
      i_t_log_handle      = lt_log_handle
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*   save logs
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all = iv_true
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " LOG_SHOW
*&---------------------------------------------------------------------*
*&      Form  GET_ORDERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IR_ORDID   Range table for OrderID
*      -->IR_PLORD   Range table for planned order number
*      -->IR_MATNR   Range table for material
*      -->IR_PLANT   Range table for plant
*      <--ET_ORD_INF Order info internal table
*----------------------------------------------------------------------*
FORM get_orders  USING    ir_ordid       TYPE gtr_ordid
                          value(ir_ordnr) TYPE gtr_ordnr
                          ir_matnr       TYPE gtr_matnr
                          ir_plant       TYPE gtr_werks
                          "iv_selordtime  TYPE ppc_conftime
                          iv_selconftime TYPE ppc_conftime
                 CHANGING et_ordid       TYPE ppc_t_orderid.

  DATA:
    ls_ordnr_r TYPE range_s_plnum,
    lt_ord     TYPE gty_t_order,
    ls_ord_del TYPE ts_ord_del,
    lt_ord_del TYPE SORTED TABLE OF ts_ord_del
      WITH NON-UNIQUE KEY orderid,
    lt_tmp_del TYPE SORTED TABLE OF ts_ord_del
      WITH NON-UNIQUE KEY orderid.

  CLEAR et_ordid.

* Convert order number output length
  LOOP AT ir_ordnr INTO ls_ordnr_r.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_ordnr_r-low
      IMPORTING
        output = ls_ordnr_r-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_ordnr_r-high
      IMPORTING
        output = ls_ordnr_r-high.
    MODIFY ir_ordnr INDEX sy-tabix FROM ls_ordnr_r
    TRANSPORTING low high.
  ENDLOOP.

* Get the orders before selection date
  SELECT orderid materialnr plant ordernr
    FROM ppc_ord_inf INTO TABLE lt_ord
   WHERE ordernr    IN ir_ordnr AND
         orderid    IN ir_ordid AND
         materialnr IN ir_matnr AND
         plant      IN ir_plant "AND
*         zz_tmstp_final > 0 AND
*         zz_tmstp_final < iv_selordtime
    .

  IF NOT sy-subrc IS INITIAL.
    RETURN.
  ENDIF.

  SORT lt_ord BY orderid.
  DELETE ADJACENT DUPLICATES FROM lt_ord.

* Check synchronous and asynchronous movements,
* asynchronous activity posting are made to order
* If already post goods movement synch./asynch., escape from deleting
  SELECT DISTINCT orderid flg_synch flg_asynch flg_asynch_a
    FROM ppc_head
    INTO TABLE lt_ord_del
     FOR ALL ENTRIES IN lt_ord
   WHERE orderid      = lt_ord-orderid AND
         crtime       < iv_selconftime AND
         flg_del      = space.    "already deleted

  lt_tmp_del = lt_ord_del.
  LOOP AT lt_tmp_del INTO ls_ord_del
    WHERE ( flg_synch    IS INITIAL OR flg_synch = 'B' ) OR
          ( flg_asynch   IS INITIAL OR flg_asynch = 'B' ) OR
          ( flg_asynch_a IS INITIAL OR flg_asynch = 'B' ).
    READ TABLE lt_ord_del WITH KEY orderid = ls_ord_del-orderid
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      DELETE lt_ord_del WHERE orderid = ls_ord_del-orderid.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_ord_del INTO ls_ord_del.
    APPEND ls_ord_del-orderid TO et_ordid.
  ENDLOOP.

ENDFORM.                    " GET_ORDERS
*&---------------------------------------------------------------------*
*&      Form  BLOCK_PROCESS
*&---------------------------------------------------------------------*
*       Set deletion indicator for confirmations of final bf orders
*----------------------------------------------------------------------*
*      -->IT_CONF  Confirmation IDs
*----------------------------------------------------------------------*
FORM block_process  USING    it_conf_ordid TYPE ppc_t_orderid
                    CHANGING ev_log_exist  TYPE xfeld.

  DATA:
       lv_orderid  TYPE ppc_orderid,
       lv_msg_text TYPE bapi_msg,
       ls_return   TYPE bapiret2.

  CHECK NOT it_conf_ordid IS INITIAL.

  CALL FUNCTION 'BAPI_MNFCTCONFRCVR_DELETE'
    IMPORTING
      return          = ls_return
    TABLES
      it_conf_orderid = it_conf_ordid.

  IF ls_return IS INITIAL.
* Write messages: "Confirmations for order deleted ( XXX )"
    LOOP AT it_conf_ordid INTO lv_orderid.
      CONCATENATE text-003 lv_orderid ')' INTO lv_msg_text
        SEPARATED BY space.
      PERFORM add_msg USING gc_freetext_msgid
                            gc_freetext_msgno
                            gc_msgty-info
                            '' '' '' ''
                            gc_true
                            lv_msg_text
                   CHANGING ev_log_exist.
    ENDLOOP.

*  DNE 04-Sept-2008 Begin
*  If there had been no problem in setting the deletion indicator in PPC_HEAD
*  the entries in table ZSAPBF_ACT_FIX have to be deleted for the relevant ORDERIDs

*FIXME - HISNA
*    PERFORM delete_zsapbf_act_fix USING it_conf_ordid
*                               CHANGING ev_log_exist.

*  DNE 04-Sept-2008 Begin

*    COMMIT WORK.

  ELSE.
* Write messages: "Deletion indicator cannot be set"
    PERFORM add_msg USING ls_return-id
                          ls_return-number
                          ls_return-type
                          ls_return-message_v1
                          ls_return-message_v2
                          ls_return-message_v3
                          ls_return-message_v4
                          gc_true
                          ls_return-message
                 CHANGING ev_log_exist.
* Write messages text-009
* "Error occurred when deleting confirmations: Confirmation not deleted"
    lv_msg_text = text-009.
    PERFORM add_msg USING gc_freetext_msgid
                          gc_freetext_msgno
                          gc_msgty-error
                          '' '' '' ''
                          gc_true
                          lv_msg_text
                 CHANGING ev_log_exist.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " BLOCK_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DELETE_ZSAPBF_ACT_FIX
*&---------------------------------------------------------------------*
*      Deletion of relevant entries in table ZSAPBF_ACT_FIX
*----------------------------------------------------------------------*
*FORM delete_zsapbf_act_fix  USING    it_conf_ordid TYPE ppc_t_orderid
*                            CHANGING ev_log_exist  TYPE xfeld.
*
*  DATA: ls_orderid TYPE ppc_orderid,
*        lv_msgv1   TYPE symsgv.
*
*  CLEAR: ev_log_exist.
*
** Ensure that there are orderids.
*  IF it_conf_ordid IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  LOOP AT it_conf_ordid INTO ls_orderid.
*
*    lv_msgv1 = ls_orderid.
*
*    DELETE FROM zsapbf_act_fix WHERE orderid = ls_orderid.
*    IF sy-subrc NE 0.
**     Error during update; Set error flag an leave the loop
*      PERFORM add_msg USING    gc_msgid
*                               gc_msgno_944
*                               gc_msgty-error
*                               lv_msgv1 '' '' ''
*                               gc_true ''
*                      CHANGING ev_log_exist.
*    ELSE.
**   Successfull update
*      PERFORM add_msg USING    gc_msgid
*                               gc_msgno_943
*                               gc_msgty-soft
*                               lv_msgv1 '' '' ''
*                               gc_true ''
*                      CHANGING ev_log_exist.
*    ENDIF.
*
*  ENDLOOP.
*
** Cherry Delete Begin 2008-09-17
*** Post changes to database!
**  COMMIT WORK.
** Cherry Delete End 2008-09-17
*
*ENDFORM.                    " DELETE_ZSAPBF_ACT_FIX
