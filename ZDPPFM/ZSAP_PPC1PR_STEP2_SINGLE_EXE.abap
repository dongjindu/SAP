FUNCTION zsap_ppc1pr_step2_single_exe.
*"----------------------------------------------------------------------
*"*"Local Interface:
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
*"     VALUE(IF_COMPCOUNT) TYPE  PPC_COMPNUMBER
*"     VALUE(IF_COMPLMODE) TYPE  CHAR1 DEFAULT CHARD
*"  EXPORTING
*"     VALUE(EF_NUMMAT_OK) TYPE  PPC_COMPNUMBER
*"     VALUE(EF_NUMMAT_FAIL) TYPE  PPC_COMPNUMBER
*"  CHANGING
*"     VALUE(IT_COMPLIST) TYPE  PPC_T_APOCOMPLIST_EXT
*"     VALUE(IT_INDEXLIST) TYPE  ZTAB_MATPOSTID
*"  EXCEPTIONS
*"      PROTOCOL_ERROR
*"      PROFILE_ERROR
*"      MATPOS_OVERFLOW
*"----------------------------------------------------------------------
  DATA: lv_compcount   TYPE i,
        lv_nummat_ok   TYPE i,
        lv_nummat_fail TYPE i,
        lf_log_handle  TYPE balloghndl.
*
*  WAIT UP TO 20 SECONDS.
*  EXIT.
  IF 1 = 1.
    lv_compcount = if_compcount.
*create own protocol
    PERFORM protocol_start_step2(saplppc1pr)
              USING lf_log_handle.

    CALL FUNCTION 'PPC1PR_STEP2_SINGLE_EXE'
      EXPORTING
        if_log_handle   = lf_log_handle  "if_log_handle
        if_post_date    = if_post_date
        if_accassobj    = if_accassobj
        if_plant        = if_plant
        if_version      = if_version
        if_baugr        = if_baugr
        if_rmprofile    = if_rmprofile
        if_aufnr        = if_aufnr
        if_flginfodest  = if_flginfodest
        if_conflogsys   = if_conflogsys
        if_compcount    = lv_compcount "if_compcount
      IMPORTING
        ef_nummat_ok    = lv_nummat_ok
        ef_nummat_fail  = lv_nummat_fail
      TABLES
        it_complist     = it_complist
        it_indexlist    = it_indexlist   "PPCPR_TYPE_TAB_MATPOSTID
      EXCEPTIONS
        protocol_error  = 1
        profile_error   = 2
        matpos_overflow = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      ef_nummat_fail = lv_nummat_fail.
      ef_nummat_ok   = lv_nummat_ok.
* Implement suitable error handling here
      CASE sy-subrc.
        WHEN 1.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING protocol_error.
        WHEN 2.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING profile_error.
        WHEN 3.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING matpos_overflow.
        WHEN 4.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING others.
      ENDCASE.
    ELSE.
      COMMIT WORK AND WAIT.
      ef_nummat_fail = lv_nummat_fail.
      ef_nummat_ok   = lv_nummat_ok.
    ENDIF.

  ELSE.

    DATA:
      ls_msg         TYPE ppcpr_type_msg,
      ls_documents   TYPE ppcpr_type_bf_doc,
      lt_log_handle  TYPE bal_t_logh,
      lt_rdoc        TYPE ppcpr_type_tab_ebelnr.

    DATA:
      ls_imkpf        TYPE imkpf,
      ls_emkpf        TYPE emkpf,
      lt_imseg        TYPE TABLE OF imseg,
      lt_emseg        TYPE TABLE OF emseg,
      lt_imseg_failed TYPE TABLE OF imseg,
      lt_emseg_failed TYPE TABLE OF emseg,

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

    DATA:
      ls_head           TYPE ppc_head.

* --> initialization
    CLEAR ef_nummat_ok.
    CLEAR ef_nummat_fail.
    DESCRIBE TABLE it_complist LINES lf_compcount.
    APPEND if_log_handle TO lt_log_handle.


* --> protocol entries for the specific assembly.
    PERFORM protocol_costcoll_head(saplppc1pr)
                  USING if_log_handle
                        if_post_date
                        if_accassobj
                        if_plant
                        if_version
                        if_baugr
                        if_rmprofile
                        if_aufnr
                        lf_compcount.


* --> check authority
    PERFORM authority_check(saplppc1pr)
                  USING if_plant
                        chara
                        if_log_handle
                        if_accassobj
                        if_post_date.


* --> Init WIP
    CALL FUNCTION 'QRP_APO_CPZP_MEMORY_REFRESH'.


* --> fill header for material posting
    PERFORM imkpf_fill(saplppc1pr)
        USING if_post_date
              ls_imkpf.


* --> enrich/complete material position data
    PERFORM det_step2_movmnt_type(saplppc1pr)
        USING    if_rmprofile
                           if_log_handle
                           if_accassobj
                           if_plant
                 if_post_date
        CHANGING it_complist[].

    LOOP AT it_complist INTO ls_complist.
      IF NOT ls_complist-kdauf IS INITIAL AND
             ls_complist-mat_kdauf IS INITIAL.
        ls_complist-mat_kdauf = ls_complist-kdauf.
        ls_complist-mat_kdpos = ls_complist-kdpos.
      ENDIF.
      IF NOT ls_complist-pspnr IS INITIAL AND
         ls_complist-mat_pspnr IS INITIAL.
        ls_complist-mat_pspnr = ls_complist-pspnr.
      ENDIF.
      APPEND ls_complist TO lt_complist_wip.

*   WM determination
      PERFORM wm_data_fill(saplppc1pr)
          TABLES   lt_complist_failed
                   lt_emseg_failed
          CHANGING ls_complist
                   lf_wmrel
                   lf_wm_check.
      IF lf_wmrel EQ charx AND NOT lf_wm_check IS INITIAL.
        CONTINUE.
      ENDIF.

*   stock/batch determination
      PERFORM material_info_add(saplppc1pr)
          TABLES lt_complist_det
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

    REFRESH it_complist.
    APPEND LINES OF lt_complist_temp TO it_complist.
    REFRESH lt_complist_temp.


* --> check: can we continue? (mm doc limitation)
    DESCRIBE TABLE it_complist LINES lf_compcount.
    IF lf_compcount GT gf_mmdoclimit.
      MESSAGE e216 WITH if_baugr if_plant RAISING matpos_overflow.
    ENDIF.


* --> fill material positions
    PERFORM imseg_fill(saplppc1pr)
        TABLES it_complist
                              lt_imseg.

* --> fill postprocessing material positions
    IF NOT lt_complist_failed[] IS INITIAL.
      PERFORM imseg_fill(saplppc1pr)
          TABLES lt_complist_failed
                                lt_imseg_failed.
    ENDIF.


* --> call inventory management
    PERFORM goods_movements_create(saplppc1pr)
                       TABLES lt_imseg
                              lt_emseg
                              ls_documents-mdoc
                       USING  ls_imkpf
                              ls_emkpf
               char2 ls_head.

* --> trigger goods movements
    PERFORM goods_movements_post(saplppc1pr)
        USING ls_emkpf
              char2.


* --> handle failed goods movements
    PERFORM goods_movements_error_handling(saplppc1pr)
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


* --> increase WIP for components
    IF NOT lt_complist_wip[] IS INITIAL.
      PERFORM wip_increase_mat(saplppc1pr)
          TABLES lt_complist_wip
                                      lt_rdoc
                               USING  if_accassobj
                                      if_post_date.
      APPEND LINES OF lt_rdoc TO ls_documents-rdoc.
    ENDIF.


* --> delete the items from PPC_STEP2 table
    PERFORM delete_processed_items(saplppc1pr)
        USING it_indexlist[].


* --> final tasks about protocol...
    PERFORM protocol_costcoll_close(saplppc1pr)
                USING if_log_handle
                      ef_nummat_ok
                      ef_nummat_fail
                      lt_emseg
                      if_accassobj
                      if_plant
                      if_post_date
                      ls_msg.
    PERFORM protocol_doc_list(saplppc1pr)
                USING ls_documents
                      if_log_handle.

* --> write protocol on DB (in update task)
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = charx
        i_t_log_handle   = lt_log_handle.


* --> clear tables
    FREE: lt_log_handle,
          lt_rdoc,
          lt_imseg,
          lt_emseg,
          lt_imseg_failed,
          lt_emseg_failed,
          lt_complist_failed,
          lt_complist_temp,
          lt_complist_det,
          lt_complist_wip.

  ENDIF.

ENDFUNCTION.
