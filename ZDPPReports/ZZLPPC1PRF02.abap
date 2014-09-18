*----------------------------------------------------------------------*
*   INCLUDE LPPC1PRF02                                                 *
*----------------------------------------------------------------------*

*{   INSERT         UD1K904298                                        1
*   This include contains the subroutines called from second step of
*   PPC-2-steps design: PPC1PR_STEP2_EXE and PPC1PR_STEP2_SINGLE_EXE
*   .ASTAT recording was removed from the coding (this include is NOT
*   synchronized with SAP standard.

*&---------------------------------------------------------------------*
*&      Form  ppc_step2_enqueue
*&---------------------------------------------------------------------*
*  This form locks the PPC_STEP2 table. Has the same logic as the form
*  enqueue_ppc_conf_mat...
*----------------------------------------------------------------------*
FORM ppc_step2_enqueue USING lp_log_handle TYPE balloghndl.

  DATA:
    lf_loop  LIKE sy-index,
    lf_subrc LIKE sy-subrc,
    ls_msg   TYPE ppcpr_type_msg.

  WHILE lf_loop < gc_enq_attempts.
    CALL FUNCTION 'ENQUEUE_E_PPC_STEP2'
      EXPORTING
        mode_ppc_step2 = 'E'
        mandt          = sy-mandt
        _scope         = '2'
        _wait          = 'X'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    lf_subrc = sy-subrc.
    IF lf_subrc = 0.
      EXIT.
    ENDIF.
    lf_loop = lf_loop + 1.
    IF lf_loop < gc_enq_attempts.
      WAIT UP TO gc_waittime SECONDS.
    ENDIF.
  ENDWHILE.

  CASE lf_subrc.
    WHEN 0.
    WHEN 1.
      ls_msg-msgid = cf_al_msgid.
      ls_msg-msgty = chare.
      ls_msg-msgno = '020'.
      ls_msg-msgv1 = sy-msgv1.
      PERFORM protocol_enqueue_error
                  USING lp_log_handle ls_msg.
    WHEN OTHERS.
      MOVE-CORRESPONDING syst TO ls_msg.
      PERFORM protocol_enqueue_error
                  USING lp_log_handle ls_msg.
  ENDCASE.

ENDFORM.                    " ppc_step2_enqueue


*&---------------------------------------------------------------------*
*&      Form  ppc_step2_dequeue
*&---------------------------------------------------------------------*
*   Unlocks the ppc_step2 table
*----------------------------------------------------------------------*
FORM ppc_step2_dequeue .

  CALL FUNCTION 'DEQUEUE_E_PPC_STEP2'
    EXPORTING
      mode_ppc_step2 = 'E'
      mandt          = sy-mandt.

ENDFORM.                    " ppc_step2_dequeue


*&--------------------------------------------------------------------*
*&      Form  lock_ppc_go2
*&--------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM lock_ppc_go2 USING lp_log_handle TYPE balloghndl.

  DATA:
    lf_loop  LIKE sy-index,
    lf_subrc LIKE sy-subrc,
    ls_msg   TYPE ppcpr_type_msg.

* set a shared lock on PPCGO object, to avoid external interferences
  CALL FUNCTION 'ENQUEUE_E_PPC_PPCGO'
    EXPORTING
      mode_ppc_ppcgo2 = 'S'
      mandt           = sy-mandt
      _scope          = '3'
      _wait           = 'X'
    EXCEPTIONS
      foreign_lock    = 1
      system_failure  = 2
      OTHERS          = 3.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      ls_msg-msgid = cf_al_msgid.
      ls_msg-msgty = chare.
      ls_msg-msgno = '020'.
      ls_msg-msgv1 = sy-msgv1.
      PERFORM protocol_enqueue_error
                  USING lp_log_handle ls_msg.
    WHEN OTHERS.
      MOVE-CORRESPONDING syst TO ls_msg.
      PERFORM protocol_enqueue_error
                  USING lp_log_handle ls_msg.
  ENDCASE.


* set an exclusive lock on PPCGO2 transaction
  WHILE lf_loop < gc_enq_attempts.
    CALL FUNCTION 'ENQUEUE_E_PPC_PPCGO2'
      EXPORTING
        mode_ppc_ppcgo2 = 'E'
        mandt           = sy-mandt
        _scope          = '2'
        _wait           = ' '
      EXCEPTIONS
        foreign_lock    = 1
        system_failure  = 2
        OTHERS          = 3.

    lf_subrc = sy-subrc.
    IF lf_subrc = 0.
      EXIT.
    ENDIF.
    lf_loop = lf_loop + 1.
    IF lf_loop < gc_enq_attempts.
      WAIT UP TO gc_waittime SECONDS.
    ENDIF.
  ENDWHILE.

  CASE lf_subrc.
    WHEN 0.
    WHEN 1.
      ls_msg-msgid = cf_al_msgid.
      ls_msg-msgty = chare.
      ls_msg-msgno = '020'.
      ls_msg-msgv1 = sy-msgv1.
      PERFORM protocol_enqueue_error
                  USING lp_log_handle ls_msg.
    WHEN OTHERS.
      MOVE-CORRESPONDING syst TO ls_msg.
      PERFORM protocol_enqueue_error
                  USING lp_log_handle ls_msg.
  ENDCASE.


ENDFORM.                    "lock_ppc_go2


*&--------------------------------------------------------------------*
*&      Form  unlock_ppc_go2
*&--------------------------------------------------------------------*
*    Unlocks everything was locked by lock_ppc_go2 subroutine
*---------------------------------------------------------------------*
FORM unlock_ppc_go2.

  CALL FUNCTION 'DEQUEUE_E_PPC_PPCGO'
    EXPORTING
      mode_ppc_ppcgo2 = 'S'
      mandt           = sy-mandt.
  CALL FUNCTION 'DEQUEUE_E_PPC_PPCGO2'
    EXPORTING
      mode_ppc_ppcgo2 = 'E'
      mandt           = sy-mandt.

ENDFORM.                    "unlock_ppc_go2


*&---------------------------------------------------------------------*
*&      Form  read_step2_table
*&---------------------------------------------------------------------*
*   This form reads the PPC_STEP2 table and aggregates data
*   by posting date and cost collector, regardless of the
*   backflush they came.
*----------------------------------------------------------------------*
*   -->LT_PPC_STEP2_AGG  output table with the aggregated data
*   -->LT_PPC_STEP2_AGG  output table with the inconsistend cost coll
*   -->LT_SELDT          select options for posting date
*   -->LP_LOG_HANDLE     log handle
*----------------------------------------------------------------------*
FORM read_step2_table
        using    IF_LOG_HANDLE    type BALLOGHNDL
                 IF_LIMIT         type PPC_COUNT
                 IR_SELDATE       type PPCPR_POSTDATE_RANGE
                 IR_SELUSER       type PPCPR_USERNAME_RANGE
        changing ET_PPC_STEP2_AGG type PPCPR_TYPE_TAB_STEP2_AGG
                 ET_FAILED_CCOLL  type PPCPR_TYPE_TAB_STEP2_AGG.

  DATA:
    BEGIN OF ls_costcolls,
        accassobj TYPE ppc_step2-accassobj,
        post_date TYPE ppc_step2-post_date,
    END OF ls_costcolls,
*    ls_ppc_step2_agg TYPE ppcpr_type_step2_agg,
    lt_costcolls LIKE TABLE OF ls_costcolls.

  data:
    LF_MATPOSTID type PPC_MATPOSTID,
    begin of LS_TEMPCOMP,
      MATNR type MATNR,
      WERKS type WERKS_D,
      LGORT type LGORT_D,
      CHARG type CHARG_D,
      CONFQUANT type PPC_CONFQUANT,
      DELTA_CONFQUANT type  PPC_CONFQUANT,
      CONFUNIT type PPC_CONFUNIT,
      SOBKZ type SOBKZ,
      PSPNR type PS_PSP_PNR,
      KZVBR type KZVBR,
      KZBWS type KZBWS,
      KDAUF type KDAUF,
      KDPOS type KSPOS,
      POSTID_TAB type standard table of PPC_MATPOSTID,
    end of LS_TEMPCOMP,
    LT_TEMPCOMP like hashed table of LS_TEMPCOMP with unique key
                     MATNR WERKS LGORT CHARG CONFUNIT
                     SOBKZ PSPNR KZVBR KZBWS KDAUF KDPOS.

  data:
    LF_ORDERID       type PPC_ORDERID,
    LF_TIMESTAMP     type PPC_CONFTIME,
    LF_START_TIME    type T,
    LF_START_DATE    type D,
    LF_SECONDS       type P , " length 16,
    LS_COMPLIST_EXT  type PPC_APOCOMPLIST_EXT,
    LS_PPC_STEP2_AGG type PPCPR_TYPE_STEP2_AGG.

  FIELD-SYMBOLS:
    <FS_comp> like LS_TEMPCOMP.

  call function 'PPC1PR_ASTAT'
    exporting
      IF_ACTION  = GC_ASTATOPEN
      IF_OPENKEY = GC_PPCPA_KEY8.

  refresh: ET_PPC_STEP2_AGG, ET_FAILED_CCOLL.

* use a read timestamp from the past, to filter out the records
* inserted recently by PPCGO (some of them may be 'dirty reads')
  LF_SECONDS = SY-DATLO * 86400 + SY-TIMLO - 600.
  LF_START_DATE = LF_SECONDS div 86400.
  LF_START_TIME = LF_SECONDS mod 86400.
  convert date LF_START_DATE time LF_START_TIME
      into time stamp LF_TIMESTAMP time zone SY-ZONLO.

* are there any entries in ppc_step2 according to input date?
  select distinct ACCASSOBJ POST_DATE
      from PPC_STEP2
      into corresponding fields of table LT_COSTCOLLS
      where POST_DATE in IR_SELDATE
        and CRNAME in IR_SELUSER
        and CRTIME lt LF_TIMESTAMP.
  if SY-DBCNT = 0.
    perform PROTOCOL_NOTHING_SELECTED
        using IF_LOG_HANDLE.
  endif.

* loop through all the cost collectors...
  loop at LT_COSTCOLLS into LS_COSTCOLLS.

    clear LS_PPC_STEP2_AGG.
    LS_PPC_STEP2_AGG-POST_DATE = LS_COSTCOLLS-POST_DATE.
    LS_PPC_STEP2_AGG-ACCASSOBJ = LS_COSTCOLLS-ACCASSOBJ.

*   ...determine cost collector order number by accounting GUID
    call function 'QRP_QRP002_READ'
      exporting
        IF_CC_GUID = LS_PPC_STEP2_AGG-ACCASSOBJ
      importing
        EF_AUFNR   = LS_PPC_STEP2_AGG-AUFNR
      exceptions
        NOT_FOUND  = 1
        others     = 2.
    if SY-SUBRC <> 0.
      clear  LS_PPC_STEP2_AGG-AUFNR.
      append LS_PPC_STEP2_AGG to ET_FAILED_CCOLL.
      clear  LS_PPC_STEP2_AGG.
      continue.
    endif.

*   ... determine backflushing logical system
    clear LF_ORDERID.
    select single ORDERID CONFLOGSYS FLG_INFO_DEST
        from  PPC_HEAD
        into  (LF_ORDERID,
               LS_PPC_STEP2_AGG-CONFSYS,
               LS_PPC_STEP2_AGG-FLG_DEST)
        where POSTDATE  = LS_PPC_STEP2_AGG-POST_DATE
          and ACCASSOBJ = LS_PPC_STEP2_AGG-ACCASSOBJ.   "#EC CI_NOFIELD
    if SY-SUBRC <> 0.
      clear  LS_PPC_STEP2_AGG-CONFSYS.
      append LS_PPC_STEP2_AGG to ET_FAILED_CCOLL.
      clear  LS_PPC_STEP2_AGG.
      continue.
    endif.

*   ...determine the plant and matnr for the cost collector
    call function 'PPC1DC_ORD_INF_READ'
      exporting
        IF_ORDERID    = LF_ORDERID
      importing
        EF_MATERIALNR = LS_PPC_STEP2_AGG-BAUGR
        EF_PLANT      = LS_PPC_STEP2_AGG-PLANT
        EF_VERSION    = LS_PPC_STEP2_AGG-VERSION
      exceptions
        others        = 1.
    if SY-SUBRC <> 0.
      clear  LS_PPC_STEP2_AGG-CONFSYS.
      append LS_PPC_STEP2_AGG to ET_FAILED_CCOLL.
      clear  LS_PPC_STEP2_AGG.
      continue.
    endif.

*   ...determine the backflush profile name
    call function 'PPC1DM_RMPROFILE_NAME_GET'
      exporting
        IF_MATNR       = LS_PPC_STEP2_AGG-BAUGR
        IF_PLANT       = LS_PPC_STEP2_AGG-PLANT
        IF_DUMMY_ORDER = SPACE
      importing
        EF_RMPROFILE   = LS_PPC_STEP2_AGG-RMPROFILE
      exceptions
        others         = 1.
    if SY-SUBRC <> 0.
      clear  LS_PPC_STEP2_AGG-CONFSYS.
      append LS_PPC_STEP2_AGG to ET_FAILED_CCOLL.
      clear  LS_PPC_STEP2_AGG.
      continue.
    endif.

    refresh LT_TEMPCOMP.
    clear LS_TEMPCOMP.

*   ...read the data from STEP2 - aggregate when necessary
    select MATPOSTID
           MATNR
           WERKS
           LGORT
           CHARG
           CONFQUANT
           DELTA_CONFQUANT
           CONFUNIT
           SOBKZ
           PSPNR
           KZVBR
           KZBWS
           KDAUF
           KDPOS
       from  PPC_STEP2
       into (LF_MATPOSTID,
             LS_TEMPCOMP-MATNR,
             LS_TEMPCOMP-WERKS,
             LS_TEMPCOMP-LGORT,
             LS_TEMPCOMP-CHARG,
             LS_TEMPCOMP-CONFQUANT,
             LS_TEMPCOMP-DELTA_CONFQUANT,
             LS_TEMPCOMP-CONFUNIT,
             LS_TEMPCOMP-SOBKZ,
             LS_TEMPCOMP-PSPNR,
             LS_TEMPCOMP-KZVBR,
             LS_TEMPCOMP-KZBWS,
             LS_TEMPCOMP-KDAUF,
             LS_TEMPCOMP-KDPOS)
       where ACCASSOBJ = LS_PPC_STEP2_AGG-ACCASSOBJ and
             POST_DATE = LS_PPC_STEP2_AGG-POST_DATE and
             CRNAME in IR_SELUSER and
             CRTIME lt LF_TIMESTAMP.

      read table LT_TEMPCOMP
          with table key MATNR = LS_TEMPCOMP-MATNR
                         WERKS = LS_TEMPCOMP-WERKS
                         LGORT = LS_TEMPCOMP-LGORT
                         CHARG = LS_TEMPCOMP-CHARG
                         CONFUNIT = LS_TEMPCOMP-CONFUNIT
                         SOBKZ = LS_TEMPCOMP-SOBKZ
                         PSPNR = LS_TEMPCOMP-PSPNR
                         KZVBR = LS_TEMPCOMP-KZVBR
                         KZBWS = LS_TEMPCOMP-KZBWS
                         KDAUF = LS_TEMPCOMP-KDAUF
                         KDPOS = LS_TEMPCOMP-KDPOS
          assigning <FS_COMP>.

      if SY-SUBRC eq 0.
        "component already read -> collect quantity and matpostid
        add LS_TEMPCOMP-CONFQUANT to <FS_COMP>-CONFQUANT.
        add LS_TEMPCOMP-DELTA_CONFQUANT to <FS_COMP>-DELTA_CONFQUANT.
        append LF_MATPOSTID to <FS_COMP>-POSTID_TAB.
        unassign <FS_COMP>.
      else.
        "the component is read for the first time
        append LF_MATPOSTID to LS_TEMPCOMP-POSTID_TAB.
        insert LS_TEMPCOMP into table LT_TEMPCOMP.
      endif.

      clear LS_TEMPCOMP.

    endselect.

*   ...move components to the output structure; if the component limit
*      is already reached, then create additional packages
    loop at LT_TEMPCOMP assigning <FS_COMP>.

      if LS_PPC_STEP2_AGG-COMPCOUNT ge IF_LIMIT.
        "insert this full package and start another one
        append LS_PPC_STEP2_AGG to ET_PPC_STEP2_AGG.
        refresh LS_PPC_STEP2_AGG-COMPLIST.
        refresh LS_PPC_STEP2_AGG-INDEXLIST.
      endif.

      clear LS_COMPLIST_EXT.
      move-corresponding <FS_COMP> to LS_COMPLIST_EXT.
      move LS_PPC_STEP2_AGG-AUFNR  to LS_COMPLIST_EXT-AUFNR.
      append LS_COMPLIST_EXT to LS_PPC_STEP2_AGG-COMPLIST.
      append lines of <FS_COMP>-POSTID_TAB
          to LS_PPC_STEP2_AGG-INDEXLIST.

      describe table LS_PPC_STEP2_AGG-COMPLIST
          lines LS_PPC_STEP2_AGG-COMPCOUNT.

    endloop.


*   ...add the (last) processing package to the big table
    append LS_PPC_STEP2_AGG to ET_PPC_STEP2_AGG.


  endloop.


* performance analysis
  call function 'PPC1PR_ASTAT'
    exporting
      IF_ACTION  = GC_ASTATCLOSE
      IF_OPENKEY = GC_PPCPA_KEY8.

ENDFORM.                    " read_step2_table


**&---------------------------------------------------------------------
**
**&      Form  split_if_needed
**&---------------------------------------------------------------------
**
**  This form checks individual rows, some assemblies might have
**  more components than specified in LP_LIMIT. In such a case, we
**  should split the cost collector row accordingly.
**----------------------------------------------------------------------
**
**   -->LP_LIMIT         The maximum number of materials in a package
**   -->LT_PPC_STEP2_AGG The table with the cost colectors to be grouped
**----------------------------------------------------------------------
**
*FORM split_if_needed
*        USING lt_ppc_step2_agg TYPE ppcpr_type_tab_step2_agg
*              lp_limit         TYPE ppc_count.
*
*  FIELD-SYMBOLS:
*    <cclist> TYPE ppcpr_type_step2_agg.
*  DATA:
*    start_index           TYPE i,
*    ls_ppc_step2_agg_temp TYPE ppcpr_type_step2_agg.
*
*  LOOP AT lt_ppc_step2_agg ASSIGNING <cclist>.
*    DO.
*      IF <cclist>-compcount LE lp_limit.
*        " kein problem with the row, so exit!
*        EXIT.
*      ELSE.
*
*        " invalidate the matpostid, they must be read by component
*        REFRESH <cclist>-indexlist.
*
*        " first, create the second item ...
*        MOVE <cclist> TO ls_ppc_step2_agg_temp.
*
*        " from the first one, delete the first if_limit mats ...
*        DELETE <cclist>-complist TO lp_limit.
*        SUBTRACT lp_limit FROM <cclist>-compcount.
*
*        " from the second one, we keep the first if_limit mats ...
*        start_index = lp_limit + 1.
*        DELETE ls_ppc_step2_agg_temp-complist FROM start_index.
*        ls_ppc_step2_agg_temp-compcount = lp_limit.
*        INSERT ls_ppc_step2_agg_temp INTO lt_ppc_step2_agg.
*
*      ENDIF.
*    ENDDO.
*  ENDLOOP.
*
*  UNASSIGN <cclist>.
*
*ENDFORM.                    " split_if_needed
*
*
**&---------------------------------------------------------------------
**
**&      Form  fill_matpostids
**&---------------------------------------------------------------------
**
**  This form fills (when necessary - splitted packages) the internal
**  table of matpostid's (=key of PPC_STEP2 table). The data from
**  PPC_STEP2 table has been entered according to execution of first
**  step of PPCGO, which processes items per backflush received. For
**  better performance, before trying to post the goods issue, the
**  components of an assembly are aggregated in COMPLIST, regardless of
**  the backflush they came from, and then splitted if the number of
**  materials exceedes the given limit. However, their keys in the table
**  PPC_STEP2 must be saved in order to delete the unused entries
**  after posting. This data is already read via array fetch, but is
**  cleared when a cost collector is splitted.
**----------------------------------------------------------------------
**
**   -->LT_PPC_STEP2_AGG The table with the GROUPED cost colectors
**----------------------------------------------------------------------
**
*FORM fill_matpostids
*        USING lt_ppc_step2_agg TYPE ppcpr_type_tab_step2_agg.
*
*  FIELD-SYMBOLS:
*    <cclist>      TYPE ppcpr_type_step2_agg,
*    <compts>      TYPE ppc_apocomplist_ext.
*  DATA:
*    lt_dummyindex TYPE ppcpr_type_tab_matpostid.
*
*
** for every cost collector package...
*  LOOP AT lt_ppc_step2_agg ASSIGNING <cclist>.
*
**   ... if the matpostids are there, skip to the next package
*    CHECK <cclist>-indexlist IS INITIAL.
*
**   ... for each component of the current package
*    LOOP AT <cclist>-complist ASSIGNING <compts>.
*      SELECT matpostid FROM ppc_step2
*            INTO TABLE lt_dummyindex
*            WHERE accassobj = <cclist>-accassobj
*              AND post_date = <cclist>-post_date
*              AND matnr     = <compts>-matnr
*              AND werks     = <compts>-werks
*              AND charg     = <compts>-charg
*              AND sobkz     = <compts>-sobkz
*              AND kdauf     = <compts>-kdauf
*              AND kdpos     = <compts>-kdpos.
*      APPEND LINES OF lt_dummyindex
*              TO <cclist>-indexlist.
*      REFRESH lt_dummyindex.
*    ENDLOOP.
*
*  ENDLOOP.
*
*  UNASSIGN <cclist>.
*
*ENDFORM.                    "fill_matpostids


*&---------------------------------------------------------------------*
*&      Form  delete_processed_items
*&---------------------------------------------------------------------*
*   This form deletes from database the processed items. To do that, it
*   uses the table with the keys from PPC_STEP2 table (field MATPOSTID).
*----------------------------------------------------------------------*
*   --> lt_INDEXLIST The table with MATPOSTID-s (the key of PPC_STEP2)
*----------------------------------------------------------------------*
FORM delete_processed_items
               USING lt_indexlist TYPE ppcpr_type_tab_matpostid.

  DATA:
    lf_ppcstep2  TYPE ppc_step2,
    lt_ppcstep2  TYPE TABLE OF ppc_step2.

* delete database entries for every line in INDEXLIST
  LOOP AT lt_indexlist INTO lf_ppcstep2-matpostid.
    APPEND lf_ppcstep2 TO lt_ppcstep2.
  ENDLOOP.
  IF lt_ppcstep2 IS INITIAL.
    MESSAGE e210(ppc1pr) RAISING update_error.
  ENDIF.
  CALL FUNCTION 'PPC1DM_DELETE_STEP2' IN UPDATE TASK
    TABLES
      it_step2 = lt_ppcstep2.

  FREE lt_ppcstep2.

ENDFORM.                    " delete_processed_items


*&---------------------------------------------------------------------*
*&      Form  det_step2_movmnt_type
*&---------------------------------------------------------------------*
*  This form determines the movement type BWART for the components of
*  the specified cost collector.
*&---------------------------------------------------------------------*
*   -->LT_COMPLIST   The table with the material components of the CC
*   -->LP_RMPROFILE  REM profile for the cost collector
*   -->LP_LOG_HANDLE Log handle
*   -->LP_ACCASSOBJ  Cost collector GUID (used here in logging)
*   -->LP_WERKS      Plant               (used here in logging)
*   -->LP_BUDAT      Posting Date        (used here in logging)
*----------------------------------------------------------------------*
FORM det_step2_movmnt_type
                 USING lt_complist   TYPE ppc_t_apocomplist_ext
                       lp_rmprofile  TYPE ppc_profile_gen
                       lp_log_handle TYPE balloghndl
                       lp_accassobj  TYPE ppc_accassobj_int
                       lp_werks      TYPE werks_d
                       lp_budat      TYPE budat.

  FIELD-SYMBOLS:
    <compls>      TYPE ppc_apocomplist_ext.
  DATA:
    lf_profile  TYPE ppc_profile,
    lf_lgort    TYPE ppc_lgort,
    lf_shkzg    TYPE shkzg,
    ls_msg      TYPE ppcpr_type_msg,
    lf_text(100).

* determine the movement type from PPC profile...
  CALL FUNCTION 'PPC1DC_RMPROFILE_READ'
    EXPORTING
      if_rmprofile_name = lp_rmprofile
    IMPORTING
      es_rmprofile_data = lf_profile
      e_lgonmrp         = lf_lgort
    EXCEPTIONS
      invalid_rmprofile = 1
      no_profilename    = 2
      profile_not_found = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    MOVE-CORRESPONDING syst TO ls_msg.
*    PERFORM protocol_bwart_error
*                    USING lp_log_handle
*                          lp_accassobj
*                          lp_rmprofile
*                          lp_werks
*                          lp_budat
*                          ls_msg.
    CLEAR : LF_TEXT.
      CONCATENATE LS_MSG-MSGV1 LS_MSG-MSGV2 LS_MSG-MSGV3
             INTO LF_TEXT.

* SAVE THE LOG MSG
      LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL5.
      LOG_MSG_S-MSG    = LF_TEXT.
      APPEND LOG_MSG_S TO LOG_MSG_TS.

*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*              raising progile_error.
     RAISE profile_error.
  ENDIF.

* ... and the debit/credit indicator, as well!
  lf_shkzg = charh.    " we do good issue, so it'll be always credit (H)

* update the components
  LOOP AT lt_complist ASSIGNING <compls>.
    <compls>-bwart = lf_profile-bwawa.
    <compls>-lgort = lf_lgort.
    <compls>-shkzg = lf_shkzg.
  ENDLOOP.

ENDFORM.                    "det_step2_movmnt_type




*&---------------------------------------------------------------------*
*&      Form  PROTOCOL_START_STEP2
*&---------------------------------------------------------------------*
*  -->  ef_log_handle  The log handle to be used
*----------------------------------------------------------------------*
FORM protocol_start_step2
            USING ef_log_handle TYPE balloghndl.


  DATA: ls_log TYPE bal_s_log.

* define the header data for this log
  ls_log-extnumber = cf_al_extnumber_step2.
  ls_log-object    = cf_al_object.
  ls_log-subobject = cf_al_subobj_pr.
  ls_log-aldate    = sy-datlo.
  ls_log-altime    = sy-timlo.
  ls_log-aluser    = sy-uname.
  ls_log-altcode   = sy-tcode.
  ls_log-alprog    = sy-repid.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = ef_log_handle
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

ENDFORM.                               " protocol_start_step2


*&---------------------------------------------------------------------*
*&      Form  PROTOCOL_TOP_LINE_STEP2
*&---------------------------------------------------------------------*
*   -->P_LF_LOG_HANDLE  text
*----------------------------------------------------------------------*
FORM protocol_top_line_step2
           USING if_log_handle TYPE balloghndl
                 is_statistics TYPE ppcpr_type_statist.

  DATA:
    lf_text(100)  TYPE c,
    lf_dummy(70)  TYPE c,
    lt_log_handle TYPE bal_t_logh.

  APPEND if_log_handle TO lt_log_handle.

*--> Top lines (one per TA execution):
* User   Time   Date
* triggered by

* user
  CLEAR lf_text.
  WRITE text-002 TO lf_text.
  WRITE is_statistics-uname TO lf_text+20.
* start date
  WRITE text-003 TO lf_text+33.
  WRITE is_statistics-stdat TO lf_text+52 DD/MM/YYYY.
* start time
  WRITE text-004 TO lf_text+64.
  WRITE  is_statistics-sttim TO lf_text+82
             USING EDIT MASK '__:__:__'.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = if_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel1
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* triggered by
  CLEAR lf_text.
  WRITE text-033 TO lf_text.
  CONCATENATE text-035 text-038 INTO lf_dummy.
  WRITE lf_dummy TO lf_text+15.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = if_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel2
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> Write protocol in DB without 'update task'
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = char_
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

ENDFORM.                    " PROTOCOL_TOP_LINE


*&--------------------------------------------------------------------*
*&      Form  protocol_enqueue_error
*&--------------------------------------------------------------------*
*      -->LP_LOG_HAND  Log handler
*      -->LS_MSG       Message structure to be logged
*---------------------------------------------------------------------*
FORM protocol_enqueue_error
           USING lp_log_handle TYPE balloghndl
                 ls_msg TYPE ppcpr_type_msg.

  DATA:
    lt_log_handle TYPE bal_t_logh,
    lf_text(100)  TYPE c,
    lf_lognumber  TYPE balognr.

  APPEND lp_log_handle TO lt_log_handle.

* --> add message to protocol: backflush locked or whatever
  PERFORM protocol_msg_add USING lp_log_handle
          ls_msg-msgid ls_msg-msgty ls_msg-msgno
          ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3
          ls_msg-msgv4 cf_al_detlevel2.

* --> get external log number
  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle  = lp_log_handle
    IMPORTING
      e_lognumber   = lf_lognumber
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> log external log number
  CLEAR lf_text.
  WRITE text-037 TO lf_text.
  WRITE lf_lognumber TO lf_text+30.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = lp_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel2
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

*--> write protocol on DB (no update)
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = char_
      i_t_log_handle   = lt_log_handle.

  MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
          WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
          RAISING enqueue_error.

ENDFORM.                    "protocol_enqueue_error


*&--------------------------------------------------------------------*
*&      Form  protocol_nothing_selected
*&--------------------------------------------------------------------*
*      -->LP_LOG_HAND  Log handler
*---------------------------------------------------------------------*
FORM protocol_nothing_selected
           USING lp_log_handle TYPE balloghndl.

  DATA:
    lt_log_handle TYPE bal_t_logh,
    lf_text(100)  TYPE c,
    lf_lognumber  TYPE balognr.

  APPEND lp_log_handle TO lt_log_handle.

* --> add message to protocol: nothing selected
  PERFORM protocol_msg_add USING lp_log_handle
          cf_al_msgid chari '022' space
          space space space cf_al_detlevel2.

* --> get external log number
  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle  = lp_log_handle
    IMPORTING
      e_lognumber   = lf_lognumber
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> log external log number
  CLEAR lf_text.
  WRITE text-037 TO lf_text.
  WRITE lf_lognumber TO lf_text+30.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = lp_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel2
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

*--> write protocol on DB (no update)
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = char_
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

  MESSAGE i022 RAISING nothing_selected.

ENDFORM.                    "protocol_nothing_selected


*&---------------------------------------------------------------------*
*&      Form  protocol_costcoll_read_error
*&---------------------------------------------------------------------*
*      -->P_IT_FAILED_CCOLL  text
*      -->P_LF_LOG_HANDLE  text
*----------------------------------------------------------------------*
FORM protocol_costcoll_read_error
                USING lt_failed_ccoll TYPE ppcpr_type_tab_step2_agg
                      lp_log_handle   TYPE balloghndl.

  DATA:
    ls_ppc_step2_agg TYPE ppcpr_type_step2_agg,
    lt_log_handle    TYPE bal_t_logh,
    lf_text(100)     TYPE c,
    lf_dummy         TYPE c,
    lf_countofcc     TYPE i.

  APPEND lp_log_handle TO lt_log_handle.
  DESCRIBE TABLE lt_failed_ccoll LINES lf_countofcc.
  CHECK NOT lf_countofcc IS INITIAL.

* --> first line, 'inconsistent CC found'
  CLEAR lf_text.
  WRITE text-046 TO lf_text.
  WRITE lf_countofcc TO lf_text+45 LEFT-JUSTIFIED.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = lp_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel2
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> and then add every failed cost collector
  LOOP AT lt_failed_ccoll INTO ls_ppc_step2_agg.

    IF ls_ppc_step2_agg-rmprofile IS INITIAL.
      " no rmprofile, plant, storloc found...
      MESSAGE i211 INTO lf_dummy
            WITH ls_ppc_step2_agg-accassobj.
      PERFORM protocol_msg_add USING lp_log_handle
              cf_al_msgid charw '211'
              syst-msgv1 space space space
              cf_al_detlevel3.
    ELSEIF ls_ppc_step2_agg-aufnr IS INITIAL.
      " no aufnr found...
      MESSAGE i212 INTO lf_dummy
            WITH ls_ppc_step2_agg-accassobj.
      PERFORM protocol_msg_add USING lp_log_handle
              cf_al_msgid charw '212'
              syst-msgv1 space space space
              cf_al_detlevel3.
    ELSE.
      " no conflogsys found...
      MESSAGE i213 INTO lf_dummy
            WITH ls_ppc_step2_agg-accassobj.
      PERFORM protocol_msg_add USING lp_log_handle
              cf_al_msgid charw '213'
              syst-msgv1 space space space
              cf_al_detlevel3.
    ENDIF.

  ENDLOOP.

* --> write protocol on DB (no update)
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = char_
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> clear the unneeded tables
  REFRESH: lt_failed_ccoll, lt_log_handle.

ENDFORM.                    " protocol_costcoll_read_error


*&---------------------------------------------------------------------*
*&      Form  protocol_total_ccoll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LF_LOG_HANDLE  text
*      -->P_LF_CC_COUNT  text
*----------------------------------------------------------------------*
FORM protocol_total_ccoll
           USING lp_log_handle TYPE balloghndl
                 lp_cc_count   TYPE i.

  DATA:
    lf_text(100)  TYPE c,
    lt_log_handle TYPE bal_t_logh.

  APPEND lp_log_handle TO lt_log_handle.

* cost collectors processed
  CLEAR lf_text.
  WRITE text-045 TO lf_text.
  WRITE lp_cc_count TO lf_text+41 LEFT-JUSTIFIED.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = lp_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel2
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> Write protocol in DB without 'update task'
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = char_
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

ENDFORM.                    " protocol_total_ccoll


*&---------------------------------------------------------------------*
*&      Form  PROTOCOL_STATISTICS_STEP2
*&---------------------------------------------------------------------*
*      -->P_LT_COMPL_DATA_LUW  text
*----------------------------------------------------------------------*
FORM protocol_statistics_step2
          USING  is_statistics TYPE ppcpr_type_statist
                 if_log_handle TYPE balloghndl
                 ef_lognumber  TYPE balognr
                 if_numcc_ok   TYPE i
                 if_numcc_fail TYPE i
                 it_luw        TYPE ppcpr_type_tab_step2_agg.

  FIELD-SYMBOLS:
     <cclist>     TYPE ppcpr_type_step2_agg.
  DATA:
    lt_log_handle TYPE bal_t_logh,
    lf_text(100)  TYPE c,
    lt_text       LIKE lf_text OCCURS 20,
    lf_numcc_tot  TYPE i,
    lf_delta(8)   TYPE p DECIMALS 2,
    ls_msg        TYPE ppcpr_type_msg.

  APPEND if_log_handle TO lt_log_handle.

  is_statistics-endat = sy-datlo.
  is_statistics-entim = sy-timlo.
  is_statistics-gstim = is_statistics-entim - is_statistics-sttim.
  is_statistics-nummat_total = is_statistics-nummat_ok +
                               is_statistics-nummat_fail.
  lf_numcc_tot = if_numcc_ok + if_numcc_fail.

* 1. Zeile: Überschrift
  CLEAR lf_text.
  WRITE text-001 TO lf_text.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = if_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel2
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* 2. Zeile: Benutzer
  CLEAR lf_text.
  WRITE text-002 TO lf_text.
  WRITE is_statistics-uname TO lf_text+65.
  APPEND lf_text TO lt_text.
* 3. Zeile: Startdatum
  CLEAR lf_text.
  WRITE text-003 TO lf_text.
  WRITE is_statistics-stdat TO lf_text+65 DD/MM/YYYY.
  APPEND lf_text TO lt_text.
* 4. Zeile: Startzeit
  CLEAR lf_text.
  WRITE text-004 TO lf_text.
  WRITE  is_statistics-sttim TO lf_text+65
             USING EDIT MASK '__:__:__'.
  APPEND lf_text TO lt_text.
* 5. Zeile: Enddatum
  CLEAR lf_text.
  WRITE text-005 TO lf_text.
  WRITE is_statistics-endat TO lf_text+65 DD/MM/YYYY.
  APPEND lf_text TO lt_text.
* 6. Zeile: Endzeit
  CLEAR lf_text.
  WRITE text-006 TO lf_text.
  WRITE is_statistics-entim TO lf_text+65
            USING EDIT MASK '__:__:__'.
  APPEND lf_text TO lt_text.
* 7. Zeile: Laufzeit
  CLEAR lf_text.
  WRITE text-007 TO lf_text.
  WRITE is_statistics-gstim TO lf_text+65
            USING EDIT MASK '__:__:__'.
  APPEND lf_text TO lt_text.

  LOOP AT lt_text INTO lf_text.
    CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle = if_log_handle
        i_msgty      = chari
        i_text       = lf_text
        i_detlevel   = cf_al_detlevel3
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING protocol_error.
    ENDIF.
  ENDLOOP.

* 8...10 Detailed runtimes
  ls_msg-msgid = cf_al_msgid.
  ls_msg-msgty = chari.
  ls_msg-msgno = '214'.
  LOOP AT it_luw ASSIGNING <cclist>.
    lf_delta = <cclist>-packagenr / 1000000.
    WRITE lf_delta TO ls_msg-msgv1 LEFT-JUSTIFIED.
    WRITE <cclist>-compcount TO ls_msg-msgv2 LEFT-JUSTIFIED..
    WRITE <cclist>-baugr TO ls_msg-msgv3.
    WRITE <cclist>-post_date TO ls_msg-msgv4
        USING EDIT MASK '__.__.____'.
    PERFORM protocol_msg_add USING if_log_handle
          ls_msg-msgid ls_msg-msgty ls_msg-msgno
          ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3
          ls_msg-msgv4 cf_al_detlevel4.
    CLEAR lf_delta.
  ENDLOOP.

* 11. Zeile: all Cost Collectors
  REFRESH lt_text.
  CLEAR lf_text.
  WRITE text-041 TO lf_text.
  WRITE lf_numcc_tot TO lf_text+80 LEFT-JUSTIFIED.
  APPEND lf_text TO lt_text.
* 12. Zeile: Cost Collecters OK
  CLEAR lf_text.
  WRITE text-042 TO lf_text.
  WRITE if_numcc_ok TO lf_text+80 LEFT-JUSTIFIED.
  APPEND lf_text TO lt_text.
* 13. Zeile: Cost Collectors failed
  CLEAR lf_text.
  WRITE text-043 TO lf_text.
  WRITE if_numcc_fail TO lf_text+80 LEFT-JUSTIFIED.
  APPEND lf_text TO lt_text.
* 14. Zeile: alle Materialpositionen
  CLEAR lf_text.
  WRITE text-011 TO lf_text.
  WRITE is_statistics-nummat_total TO lf_text+80 LEFT-JUSTIFIED.
  APPEND lf_text TO lt_text.
* 15. Zeile: erfolgreich gebuchte Materialpositionen
  CLEAR lf_text.
  WRITE text-012 TO lf_text.
  WRITE is_statistics-nummat_ok TO lf_text+80 LEFT-JUSTIFIED.
  APPEND lf_text TO lt_text.
* 16. Zeile: nicht erfolgreich gebuchte Materialpositionen
  CLEAR lf_text.
  WRITE text-013 TO lf_text.
  WRITE is_statistics-nummat_fail TO lf_text+80 LEFT-JUSTIFIED.
  APPEND lf_text TO lt_text.

  LOOP AT lt_text INTO lf_text.
    CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle = if_log_handle
        i_msgty      = chari
        i_text       = lf_text
        i_detlevel   = cf_al_detlevel3
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING protocol_error.
    ENDIF.
  ENDLOOP.

* --> get external log number
  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle  = if_log_handle
    IMPORTING
      e_lognumber   = ef_lognumber
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> log external log number
  CLEAR lf_text.
  WRITE text-037 TO lf_text.
  WRITE ef_lognumber TO lf_text+30.

  CALL FUNCTION 'PPC1PR_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle = if_log_handle
      i_msgty      = chari
      i_text       = lf_text
      i_detlevel   = cf_al_detlevel2
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> Protokolle auf DB ohne 'update task' ablegen
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = char_
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING protocol_error.
  ENDIF.

* --> clear the tables
  REFRESH: lt_log_handle, it_luw.

ENDFORM.                               " PROTOCOL_STATISTICS_STEP2



*&---------------------------------------------------------------------*
*&      Form  protocol_costcoll_head
*&---------------------------------------------------------------------*
*      -->P_IF_LOG_HANDLE  text
*      -->P_IF_POST_DATE  text
*      -->P_IF_ACCASSOBJ  text
*      -->P_IF_PLANT  text
*      -->P_IF_VERSION  text
*      -->P_IF_BAUGR  text
*      -->P_IF_RMPROFILE  text
*----------------------------------------------------------------------*
FORM protocol_costcoll_head
     USING lp_log_handle TYPE balloghndl
           lp_post_date  TYPE budat
           lp_accassobj  TYPE ppc_accassobj_int
           lp_plant      TYPE werks_d
           lp_version    TYPE verid
           lp_baugr      TYPE matnr
           lp_rmprofile  TYPE ppc_profile_gen
           lp_aufnr      TYPE aufnr
           lp_compcount  TYPE i.

  DATA:
    lf_text(100) TYPE c,
    ls_msg       TYPE ppcpr_type_msg.


*--> first line:
  CLEAR lf_text.
* assembly
  WRITE text-029 TO lf_text.
  WRITE lp_baugr TO lf_text+16.
* posting date
  WRITE text-017 TO lf_text+36.
  WRITE lp_post_date TO lf_text+52 DD/MM/YYYY.

* SAVE THE LOG MSG
  LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL2.
  LOG_MSG_S-MSG    = LF_TEXT.
  APPEND LOG_MSG_S TO LOG_MSG_TS.

*--> second line
  CLEAR lf_text.
* plant
  WRITE text-014 TO lf_text.
  WRITE lp_plant TO lf_text+16.
* accounting objects
  WRITE text-015 TO lf_text+22.
  WRITE lp_aufnr TO lf_text+48.

* SAVE THE LOG MSG
  LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL3.
  LOG_MSG_S-MSG    = LF_TEXT.
  APPEND LOG_MSG_S TO LOG_MSG_TS.

*--> third line:
  CLEAR lf_text.
* REM profile, version
  WRITE text-016 TO lf_text.
  WRITE lp_rmprofile TO lf_text+26.
  WRITE text-016 TO lf_text+35.
  WRITE lp_version TO lf_text+60.
* SAVE THE LOG MSG
  LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL3.
  LOG_MSG_S-MSG    = LF_TEXT.
  APPEND LOG_MSG_S TO LOG_MSG_TS.

*--> forth line:
  CLEAR lf_text.
* Component count
  WRITE text-044 TO lf_text.
  WRITE lp_compcount TO lf_text+33 LEFT-JUSTIFIED.
* SAVE THE LOG MSG
  LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL3.
  LOG_MSG_S-MSG    = LF_TEXT.
  APPEND LOG_MSG_S TO LOG_MSG_TS.

ENDFORM.                    " protocol_costcoll_head


*&---------------------------------------------------------------------*
*&      Form  protocol_bwart_error
*&---------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM protocol_bwart_error
                USING if_log_handle TYPE balloghndl
                      if_accassobj  TYPE ppc_accassobj_int
                      if_profile    TYPE ppc_profile_gen
                      if_werks      TYPE werks_d
                      if_budat      TYPE budat
                      is_msg        TYPE ppcpr_type_msg.

  DATA: lf_dummy TYPE c.
  DATA: lf_msgv1 LIKE sy-msgv1.
  DATA: lf_msgv2 LIKE sy-msgv2.
  DATA: lf_msgv3 LIKE sy-msgv3.
  DATA: lf_msgv4 LIKE sy-msgv4.
  DATA: lt_log_handle  TYPE  bal_t_logh.

* --> Create table of protocol handles: one entry
  APPEND if_log_handle TO lt_log_handle.

  MOVE if_accassobj TO lf_msgv1.
  MOVE if_werks     TO lf_msgv2.
  MOVE if_budat     TO lf_msgv3.
  MOVE if_profile   TO lf_msgv4.

  MESSAGE i209 WITH if_profile INTO lf_dummy.
  PERFORM protocol_msg_add USING if_log_handle
          cf_al_msgid chare '209'
          lf_msgv4 space space space
          cf_al_detlevel3.

  MESSAGE i207 INTO lf_dummy.
  PERFORM protocol_msg_add USING if_log_handle
          cf_al_msgid chari '207'
          lf_msgv1 lf_msgv2
          lf_msgv3 space
          cf_al_detlevel4.

  PERFORM protocol_msg_add USING if_log_handle
          is_msg-msgid is_msg-msgty is_msg-msgno
          is_msg-msgv1 is_msg-msgv2 is_msg-msgv3
          is_msg-msgv4 cf_al_detlevel4.

* --> write protocol on DB (no update)
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = char_
      i_t_log_handle   = lt_log_handle.

ENDFORM.                    " protocol_bwart_error


*&---------------------------------------------------------------------*
*&      Form  protocol_costcoll_close
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LP_LOG_HANDLE  text
*      -->LP_NUMMAT_OK  text
*      -->LP_NUMMMAT_FAILED  text
*----------------------------------------------------------------------*
FORM protocol_costcoll_close
                USING lp_log_handle     TYPE balloghndl
                      lp_nummat_ok      TYPE i
                      lp_nummat_fail    TYPE i
                      lt_emseg_failed   TYPE ppcpr_type_tab_emseg
                      lp_accassobj  TYPE ppc_accassobj_int
                      lp_werks      TYPE werks_d
                      lp_budat      TYPE budat
                      ls_msg        TYPE ppcpr_type_msg.


  DATA:
    lf_text(100)  TYPE c,
    lf_dummy      TYPE c,
    ls_emseg      TYPE emseg,
    lt_log_handle TYPE bal_t_logh.

  APPEND lp_log_handle TO lt_log_handle.

* write the number of successfully processed materials
  CLEAR lf_text.
  WRITE text-012 TO lf_text.
  WRITE lp_nummat_ok TO lf_text+80 LEFT-JUSTIFIED.
* SAVE THE LOG MSG
  LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL4.
  LOG_MSG_S-MSG    = LF_TEXT.
  APPEND LOG_MSG_S TO LOG_MSG_TS.

* write the number of unsuccessfully processed materials and errors,
* only if we have errors in material posting
  IF lp_nummat_fail GT 0.
    CLEAR lf_text.
    WRITE text-013 TO lf_text.
    WRITE lp_nummat_fail TO lf_text+80 LEFT-JUSTIFIED.
* SAVE THE LOG MSG
  LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL4.
  LOG_MSG_S-MSG    = LF_TEXT.
  APPEND LOG_MSG_S TO LOG_MSG_TS.

    LOOP AT lt_emseg_failed INTO ls_emseg.
      CHECK NOT ls_emseg-msgid IS INITIAL.
      CHECK NOT ls_emseg-msgno IS INITIAL.
      CLEAR ls_msg.
      MOVE-CORRESPONDING ls_emseg TO ls_msg.

      CLEAR : LF_TEXT.
      CONCATENATE LS_MSG-MSGV1 LS_MSG-MSGV2 LS_MSG-MSGV3
             INTO LF_TEXT.

* SAVE THE LOG MSG
      LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL5.
      LOG_MSG_S-MSG    = LF_TEXT.
      APPEND LOG_MSG_S TO LOG_MSG_TS.

    ENDLOOP.

  ENDIF.

* issue success message (protocol will be saved after the writing
* of the documents list)

   CLEAR: LF_TEXT.
   LF_TEXT = TEXT-100.
* SAVE THE LOG MSG
    LOG_MSG_S-MSGLEVL = CF_AL_DETLEVEL3.
    LOG_MSG_S-MSG    = LF_TEXT.
    APPEND LOG_MSG_S TO LOG_MSG_TS.


ENDFORM.                    " protocol_costcoll_close



*&--------------------------------------------------------------------*
*&      Form  progress_status
*&--------------------------------------------------------------------*
*    This form displays the progress indicator on status bar.
*---------------------------------------------------------------------*
FORM progress_status
         USING lp_case TYPE c
               lp_pm01 TYPE i
               lp_pm02 TYPE i.

  DATA:
    lf_text(120) TYPE c,
    lf_dumm(10)  TYPE c,
    lf_perc      TYPE i.

  CASE lp_case.
    WHEN cf_statusone.
      lf_perc = 2.
      lf_text = 'Backflush process started: Selecting data...'(051).
    WHEN cf_statustwo.
      lf_perc = 5.
      lf_text = 'Data sel. ended: found &&& processing packages.'(052).
      WRITE lp_pm01 TO lf_dumm.
      REPLACE '&&&' WITH lf_dumm INTO lf_text.
      CONDENSE lf_text.
      CLEAR lf_dumm.
    WHEN cf_statusthree.
      CHECK NOT lp_pm02 IS INITIAL.
      CHECK lp_pm01 LE lp_pm02.
      lf_perc = 8 + 90 * ( lp_pm01 - 1 ) /  lp_pm02 .
      lf_text = 'Processing package &*1 of &*2 (&*3, &*4) ...'(053).
      WRITE lp_pm01 TO lf_dumm.
      REPLACE '&*1' WITH lf_dumm INTO lf_text.
      CONDENSE lf_text.
      CLEAR lf_dumm.
      WRITE lp_pm02 TO lf_dumm.
      REPLACE '&*2 (&*3, &*4)' WITH lf_dumm INTO lf_text.
      CONDENSE lf_text.
      CLEAR lf_dumm.
    WHEN cf_statusfour.
      CHECK NOT lp_pm02 IS INITIAL.
      CHECK lp_pm01 LE lp_pm02.
      lf_perc = 8 + 90 * ( ( 2 * lp_pm01 ) - 1 ) / ( 2 * lp_pm02 ).
      lf_text =
        'Processing package &*1 of &*2 (&*3, &*4) - failed'(054).
      WRITE lp_pm01 TO lf_dumm.
      REPLACE '&*1' WITH lf_dumm INTO lf_text.
      CONDENSE lf_text.
      CLEAR lf_dumm.
      WRITE lp_pm02 TO lf_dumm.
      REPLACE '&*2 (&*3, &*4)' WITH lf_dumm INTO lf_text.
      CONDENSE lf_text.
      CLEAR lf_dumm.
    WHEN cf_statusfive.
      CHECK NOT lp_pm02 IS INITIAL.
      CHECK lp_pm01 LE lp_pm02.
      lf_perc = 8 + 90 * ( ( 2 * lp_pm01 ) - 1 ) / ( 2 * lp_pm02 ).
      lf_text =
        'Processing package &*1 of &*2 (&*3, &*4) - success'(055).
      WRITE lp_pm01 TO lf_dumm.
      REPLACE '&*1' WITH lf_dumm INTO lf_text.
      CONDENSE lf_text.
      CLEAR lf_dumm.
      WRITE lp_pm02 TO lf_dumm.
      REPLACE '&*2 (&*3, &*4)' WITH lf_dumm INTO lf_text.
      CONDENSE lf_text.
      CLEAR lf_dumm.
    WHEN cf_statussix.
      lf_perc = 98.
      lf_text = 'Closing log ...'(056).

  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lf_perc
      text       = lf_text.

ENDFORM.                    "progress_status

*}   INSERT
*&--------------------------------------------------------------------*
*&      Form  enqueue_ppcgo
*&--------------------------------------------------------------------*
form enqueue_ppcgo.

* shared lock on PPCGO object, so that synch. PPCGO, asnych PPCGO
* and PPCGO2 can run simultaneously, but not in the same time
* with the WIP correction report, which sets the lock exclusively
  call function 'ENQUEUE_E_PPC_PPCGO'
    exporting
      mode_ppc_ppcgo2 = 'S'
      mandt           = sy-mandt
      _scope          = '3'
      _wait           = 'X'
    exceptions
      foreign_lock    = 1
      system_failure  = 2
      others          = 3.
  if sy-subrc ne 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            raising enqueue_error.
  endif.

endform.                    "enqueue_ppcgo
