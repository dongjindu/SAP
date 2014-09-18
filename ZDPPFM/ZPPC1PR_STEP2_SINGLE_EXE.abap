FUNCTION zppc1pr_step2_single_exe.
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
*"     VALUE(IF_COMPCOUNT) TYPE  I
*"     VALUE(IF_COMPLMODE) TYPE  C DEFAULT 'D'
*"  EXPORTING
*"     REFERENCE(EF_NUMMAT_OK) TYPE  I
*"     REFERENCE(EF_NUMMAT_FAIL) TYPE  I
*"  TABLES
*"      IT_COMPLIST TYPE  PPC_T_APOCOMPLIST_EXT
*"      IT_INDEXLIST TYPE  PPCPR_TYPE_TAB_MATPOSTID
*"      IT_LOG_MSG TYPE  ZSPP_LOG_MSG_T
*"  EXCEPTIONS
*"      PROTOCOL_ERROR
*"      PROFILE_ERROR
*"      UPDATE_ERROR
*"      MATPOS_OVERFLOW
*"----------------------------------------------------------------------
* modification assistant was turned off because this function is not
* part of DI 4.6C2 standard
* .ASTAT recording was removed from the coding (this function module
* is NOT synchronized with DIMP 4.71 standard.

  DATA:
    ls_msg         TYPE ppcpr_type_msg,
    ls_documents   TYPE ppcpr_type_bf_doc,
    lt_log_handle  TYPE bal_t_logh,
    lt_rdoc        TYPE ppcpr_type_tab_ebelnr,
    NO_LOG         TYPE C.

  DATA:
    ls_imkpf        LIKE imkpf,
    ls_emkpf        LIKE emkpf,
    lt_imseg        LIKE imseg OCCURS 0,
    lt_emseg        LIKE emseg OCCURS 0,
    lt_imseg_failed LIKE imseg OCCURS 0,
    lt_emseg_failed LIKE emseg OCCURS 0,

    lt_complist_failed TYPE ppc_t_apocomplist_ext,
    lt_complist_temp   TYPE ppc_t_apocomplist_ext,
    lt_complist_det    TYPE ppc_t_apocomplist_ext,
    lt_complist_wip    TYPE ppc_t_apocomplist_ext,
    ls_complist        TYPE ppc_apocomplist_ext.

  DATA:
    lf_compcount      TYPE i,
    lf_batch_or_stock TYPE c,
    lf_wmrel          TYPE c,
    lf_wm_check       TYPE c.


* --> initialization
  CLEAR ef_nummat_ok.
  CLEAR ef_nummat_fail.
  CLEAR LOG_MSG_TS.
  DESCRIBE TABLE it_complist LINES lf_compcount.
  append if_log_handle TO lt_log_handle.


* --> deal with the protocol entries for the specific cost coll.
*  IF NO_LOG ne 'X'.

    perform protocol_costcoll_head
                  using if_log_handle
                        if_post_date
                        if_accassobj
                        if_plant
                        if_version
                        if_baugr
                        if_rmprofile
                        if_aufnr
                        lf_compcount.

     append lines of log_msg_ts to it_log_msg.
     clear: log_msg_ts.

* --> check authority
    perform authority_check_copy
                  using if_plant
                        chara
                        if_log_handle
                        if_accassobj
                        if_post_date.

*  ENDIF.
* --> Init WIP
  CALL FUNCTION 'QRP_APO_CPZP_MEMORY_REFRESH'.

* --> fill header for material posting
  PERFORM imkpf_fill USING if_post_date
                           ls_imkpf.

* --> enrich/complete material position data
  PERFORM det_step2_movmnt_type
                   USING it_complist[]
                         if_rmprofile
                         if_log_handle
                         if_accassobj
                         if_plant
                         if_post_date.

   append lines of log_msg_ts to it_log_msg.
   clear: log_msg_ts.


  LOOP AT it_complist INTO ls_complist.

    ls_complist-gmove_ind = cf_gmove_wa.
    APPEND ls_complist TO lt_complist_wip.

    " wm determination
    PERFORM wm_data_fill TABLES lt_complist_failed
                                lt_emseg_failed
                         USING  ls_complist lf_wmrel
                                lf_wm_check.
    IF lf_wmrel EQ charx AND NOT lf_wm_check IS INITIAL.
      CONTINUE.
    ENDIF.

    " stock/batch determination
    PERFORM material_info_add TABLES lt_complist_det
                                     lt_complist_failed
                                     lt_emseg_failed
                              USING  ls_complist
                                     lf_batch_or_stock
                                     if_post_date
                                     if_rmprofile
                                     lf_wmrel.
    IF lf_batch_or_stock = charx.
      APPEND LINES OF lt_complist_det TO lt_complist_temp.
    ELSE.
      APPEND ls_complist TO lt_complist_temp.
    ENDIF.

  ENDLOOP.


  " move back the complist_temp to complist after stock/batch det.
  REFRESH it_complist.
  APPEND LINES OF lt_complist_temp TO it_complist.
  REFRESH lt_complist_temp.

* --> check whether we can continue (mm doc limitation)
  DESCRIBE TABLE it_complist LINES lf_compcount.
  IF lf_compcount GT gc_mmdoclimit.
    MESSAGE e216
        WITH if_baugr if_plant
        RAISING matpos_overflow.
  ENDIF.

* --> fill material positions
  PERFORM imseg_fill TABLES it_complist
                            lt_imseg.

* --> fill postprocessing material positions
  IF NOT lt_complist_failed[] IS INITIAL.
    PERFORM imseg_fill TABLES lt_complist_failed
                              lt_imseg_failed.
  ENDIF.

* --> call inventory management
  PERFORM goods_movements_create
                     TABLES lt_imseg
                            lt_emseg
                            ls_documents-mdoc
                     USING  ls_imkpf
                            ls_emkpf.

* --> handle failed goods movements
  PERFORM goods_movements_error_handling
                     TABLES lt_imseg
                            lt_emseg
                            lt_imseg_failed
                            lt_emseg_failed
                            ls_documents-edoc
                     USING  ls_emkpf
                            if_aufnr
                            if_post_date
                            if_baugr
                            if_version
                            if_plant
                            ef_nummat_ok
                            ef_nummat_fail
                            if_flginfodest
                            if_conflogsys.

* --> trigger goods movements
  PERFORM goods_movements_post USING ls_emkpf.

* --> increase WIP for components
  IF NOT lt_complist_wip[] IS INITIAL.
    PERFORM enqueue_e_ppc_step2.
    PERFORM wip_increase_mat TABLES lt_complist_wip
                                    lt_rdoc
                             USING  if_accassobj
                                    if_post_date.
    APPEND LINES OF lt_rdoc TO ls_documents-rdoc.
  ENDIF.


* --> final tasks about protocol...
*  IF NO_LOG NE 'X'.
    perform protocol_costcoll_close
                using if_log_handle
                      ef_nummat_ok
                      ef_nummat_fail
                      lt_emseg
                      if_accassobj
                      if_plant
                      if_post_date
                      ls_msg.

   perform protocol_doc_list_c
                using ls_documents
                      if_log_handle.

*
*  return the message log
   append lines of log_msg_ts to it_log_msg.
   clear: log_msg_ts.

* --> clear tables
  REFRESH: lt_log_handle,
           lt_rdoc,
           lt_imseg,
           lt_emseg,
           lt_imseg_failed,
           lt_emseg_failed,
           lt_complist_failed,
           lt_complist_temp,
           lt_complist_det,
           lt_complist_wip.


ENDFUNCTION.
