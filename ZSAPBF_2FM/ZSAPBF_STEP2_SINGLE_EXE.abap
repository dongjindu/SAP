FUNCTION zsapbf_step2_single_exe.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CHRON) TYPE  PPC_CHRON DEFAULT 'A'
*"     VALUE(IV_LOG_HANDLE) TYPE  BALLOGHNDL
*"     VALUE(IV_POST_DATE) TYPE  BUDAT OPTIONAL
*"     VALUE(IV_ACCASSOBJ) TYPE  PPC_ACCASSOBJ_INT OPTIONAL
*"     VALUE(IV_PLANT) TYPE  WERKS_D OPTIONAL
*"     VALUE(IV_VERSION) TYPE  VERID OPTIONAL
*"     VALUE(IV_BAUGR) TYPE  MATNR OPTIONAL
*"     VALUE(IV_RMPROFILE) TYPE  PPC_PROFILE_GEN OPTIONAL
*"     VALUE(IV_AUFNR) TYPE  AUFNR OPTIONAL
*"     VALUE(IV_FLGINFODEST) TYPE  PPC_FLG_INFO_DEST OPTIONAL
*"     VALUE(IV_CONFLOGSYS) TYPE  PPC_CONFLOGSYS OPTIONAL
*"     VALUE(IV_COMPCOUNT) TYPE  I OPTIONAL
*"     VALUE(IV_COMPLMODE) TYPE  CHAR1 DEFAULT CHARD
*"     VALUE(IV_SHOW_LOG_ALL) TYPE  AS4FLAG OPTIONAL
*"     VALUE(IV_CCAR) TYPE  ZCCAR OPTIONAL
*"     VALUE(IV_CYEAR) TYPE  ZCYEAR OPTIONAL
*"     VALUE(IT_COMPLIST) TYPE  PPC_T_APOCOMPLIST_EXT OPTIONAL
*"     VALUE(IT_INDEXLIST) TYPE  ZSAPBF_TT_MATPOSTID OPTIONAL
*"  EXPORTING
*"     VALUE(EV_NUMMAT_OK) TYPE  I
*"     VALUE(EV_NUMMAT_FAIL) TYPE  I
*"     VALUE(ET_MESSAGE) TYPE  ZSAPBF_TT_BALMIP
*"  EXCEPTIONS
*"      APPLICATION_ERROR
*"----------------------------------------------------------------------
************************************************************************
**
**   Function Module: ZSAPBF_STEP2_SINGLE_EXE
**
**   Development request: DI Backflush
**
**   Short description: RFC Function for material posting
**
**   Created by: SAPCD07
**
**   Date: 25/08/2008
**
************************************************************************
** VERS DATE______ CHANGED BY___  SHORT DESCRIPTION____________________
** §001 25/08/2008 SAPCD07        Initial version
** §001 23/10/2008 SAPCD07        Msg#824440:Add detail log in Para Mode
************************************************************************
* RFC Enabled Function Module
  DATA:
    lv_dummy      TYPE c,                                   "#EC NEEDED
    lv_msgtype    TYPE c,
    ls_msg        TYPE ppcpr_type_msg,
    lt_log_handle TYPE bal_t_logh.

*§ Begin: CSS # 824440 2008
  DATA lv_all TYPE zsapbf_show_log_all.
*  CALL FUNCTION 'BAL_LOG_HDR_READ'
*    EXPORTING
*      i_log_handle  = iv_log_handle
*    EXCEPTIONS
*      log_not_found = 1
*      OTHERS        = 2.
**When PPC1PR_MAT_TRANSFER_NEW is called during Parallel processing, the
**log that was created in the parent process is not available. So,
**create
** a new log.
*  IF sy-subrc <> 0.
*    CLEAR iv_log_handle.
*    PERFORM protocol_start IN PROGRAM saplppc1pr
*                           USING iv_log_handle
*                                 iv_chron.
*  ENDIF.
*§ End: CSS # 824440 2008
*§ Begin: CSS # 824440 2008
  CLEAR iv_log_handle.
  PERFORM protocol_start IN PROGRAM saplppc1pr
                         USING iv_log_handle
                               iv_chron.
*§ End: CSS # 824440 2008

  CALL FUNCTION 'PPC1PR_STEP2_SINGLE_EXE'
    EXPORTING
      if_log_handle   = iv_log_handle
      if_post_date    = iv_post_date
      if_accassobj    = iv_accassobj
      if_plant        = iv_plant
      if_version      = iv_version
      if_baugr        = iv_baugr
      if_rmprofile    = iv_rmprofile
      if_aufnr        = iv_aufnr
      if_flginfodest  = iv_flginfodest
      if_conflogsys   = iv_conflogsys
      if_compcount    = iv_compcount
      if_complmode    = iv_complmode
    IMPORTING
      ef_nummat_ok    = ev_nummat_ok
      ef_nummat_fail  = ev_nummat_fail
    TABLES
      it_complist     = it_complist
      it_indexlist    = it_indexlist
    EXCEPTIONS
      protocol_error  = 1
      profile_error   = 2
      matpos_overflow = 3
      OTHERS          = 4.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
*§ Begin: CSS # 824440 2008
    GET PARAMETER ID 'ZSAPBF_SHOW_LOG_ALL' FIELD lv_all.
* Commented by SAPCD08 on 2010.05.19
*    IF sy-subrc <> 0 OR lv_all = charx.
    IF ( sy-subrc <> 0 OR lv_all = charx ) AND
      NOT ( iv_show_log_all = charx ).
      PERFORM read_del_dbmsg_by_logno CHANGING et_message.
    ENDIF.
*§ End: CSS # 824440 2008
  ELSE.
*     if errors, then update the protocol accordingly
    CLEAR: ls_msg.
    MOVE-CORRESPONDING syst TO ls_msg.
    ROLLBACK WORK.

*     ...and update the protocol accordingly
    MESSAGE i203 INTO lv_dummy.
    PERFORM protocol_msg_add IN PROGRAM saplppc1pr
      USING gv_log_handle
            cf_al_msgid lv_msgtype '203'
            space space space space
            cf_al_detlevel3.

    PERFORM protocol_msg_add IN PROGRAM saplppc1pr
      USING gv_log_handle
            ls_msg-msgid ls_msg-msgty ls_msg-msgno
            ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3
            ls_msg-msgv4 cf_al_detlevel4.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = space
        i_t_log_handle   = lt_log_handle.

*§ Begin: CSS # 824440 2008
    GET PARAMETER ID 'ZSAPBF_SHOW_LOG_ALL' FIELD lv_all.
* Commented by SAPCD08 on 2010.05.19
*    IF sy-subrc <> 0 OR lv_all = charx.
    IF ( sy-subrc <> 0 OR lv_all = charx ) AND
      NOT ( iv_show_log_all = charx ).
      PERFORM read_del_dbmsg_by_logno CHANGING et_message.
    ENDIF.
*§ End: CSS # 824440 2008

    RAISE application_error.
  ENDIF.


ENDFUNCTION.
