FUNCTION ZZFPP_PPC1PR_STEP2_SINGLE_EXE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IF_LOG_HANDLE) TYPE  BALLOGHNDL
*"     VALUE(IF_POST_DATE) TYPE  BUDAT
*"     VALUE(IF_ACCASSOBJ) TYPE  PPC_ACCASSOBJ_INT
*"     VALUE(IF_PLANT) TYPE  WERKS_D
*"     VALUE(IF_VERSION) TYPE  VERID
*"     VALUE(IF_BAUGR) TYPE  MATNR
*"     VALUE(IF_RMPROFILE) TYPE  PPC_PROFILE_GEN
*"     VALUE(IF_AUFNR) TYPE  AUFNR
*"     VALUE(IF_FLGINFODEST) TYPE  PPC_FLG_INFO_DEST
*"     VALUE(IF_CONFLOGSYS) TYPE  PPC_CONFLOGSYS
*"     VALUE(IF_COMPCOUNT) TYPE  INT4
*"     VALUE(IF_COMPLMODE) TYPE  CHAR1 DEFAULT 'D'
*"  EXPORTING
*"     VALUE(EF_NUMMAT_OK) TYPE  INT4
*"     VALUE(EF_NUMMAT_FAIL) TYPE  INT4
*"     VALUE(RETURN) TYPE  CHAR_LG_01
*"  TABLES
*"      IT_COMPLIST TYPE  PPC_T_APOCOMPLIST_EXT
*"      IT_INDEXLIST TYPE  ZSPP_LOG_MSG_T
*"      IF_LOG_MSG TYPE  ZSPP_LOG_MSG_T
*"----------------------------------------------------------------------
*
  DATA: LT_INDEXLIST TYPE ppcpr_type_tab_matpostid.
  data: lf_subrc like sy-subrc.
  data: lt_log_handle  TYPE bal_t_logh.
  data: lf_text(100).
  data: ls_step2_log  like ztpp_bfstep2_log.

  clear: return.
  append lines of it_indexlist to lt_indexlist.

*{   DELETE         UP1K900060                                        1
*\  call function 'ZPPC1PR_STEP2_SINGLE_EXE'
*}   DELETE
*{   INSERT         UP1K900060                                        2
    call function 'ZZPPC1PR_STEP2_SINGLE_EXE'

*}   INSERT
      exporting
        if_log_handle   = if_log_handle
        if_post_date    = if_post_date
        if_accassobj    = if_accassobj
        if_plant        = if_plant
        if_version      = if_version
        if_baugr        = if_baugr
        if_rmprofile    = if_rmprofile
        if_aufnr        = if_aufnr
        if_flginfodest  = if_flginfodest
        if_conflogsys   = if_conflogsys
        if_compcount    = if_compcount
        if_complmode    = if_complmode
      importing
        ef_nummat_ok    = ef_nummat_ok
        ef_nummat_fail  = ef_nummat_fail
      tables
        it_complist     = it_complist
        it_indexlist    = lt_indexlist
        IT_LOG_MSG      = IF_LOG_MSG
      exceptions
        protocol_error  = 1
        profile_error   = 2
        update_error    = 3
        matpos_overflow = 4
        others          = 5.

     move sy-subrc to lf_subrc.
     IF  lf_SUBRC EQ 0.
       COMMIT WORK AND WAIT.
     ELSE.
       ROLLBACK WORK.
*    following code for test
     ls_step2_log-aufnr  = if_aufnr.
     ls_step2_log-ACCOBJ = if_accassobj.
     ls_step2_log-NUMMAT_OK  = ef_nummat_ok.
     ls_step2_log-NUMMAT_FAIL = ef_nummat_fail.
     ls_step2_log-SUBRC  = lf_subrc.
     concatenate SY-MSGV1 SY-MSGV2 SY-MSGV3
         into ls_step2_log-message separated by space.
     insert into ztpp_bfstep2_log values ls_step2_log.
     commit work.
*    end of test code
     ENDIF.

     case lf_subrc.
       when 0.
       when 1.
         lf_text  = text-101.
       when 2.
         lf_text  = text-102.
       when 3.
         lf_text  = text-103.
       when 4.
         lf_text  = text-104.
       when 5.
         lf_text  = text-105.
     endcase.
*    save the log message
     log_msg_s-msglevl = CF_AL_DETLEVEL2.
     log_msg_s-msg     = lf_text.
     append log_msg_s to if_log_msg.
     IF LF_SUBRC NE 0.
       return = 'X'.
     ENDIF.
     clear: lf_subrc.
*   end

ENDFUNCTION.
