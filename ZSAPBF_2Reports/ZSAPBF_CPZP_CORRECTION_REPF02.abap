*----------------------------------------------------------------------*
***INCLUDE ZSAPBF_CPZP_CORRECTION_REPF02 .
*----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&      Form  GET_FIRST_DAY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_P_YEAR  text
**      -->P_P_MONTH  text
**      <--P_LV_FIRST_DAT  text
**----------------------------------------------------------------------*
*FORM get_first_day USING iv_year TYPE gjahr
*                         iv_month TYPE monat
*                CHANGING ev_first_day TYPE sy-datum.
*  ev_first_day+0(4) = iv_year.
*  ev_first_day+4(2) = iv_month.
*  ev_first_day+6(2) = '01'.
*
*  gv_year = iv_year.
*  gv_month = iv_month.
*ENDFORM.                    " GET_FIRST_DAY
*&---------------------------------------------------------------------*
*&      Form  master_data_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MASTER_DATA_SELECTION USING IV_AUFNR TYPE AUFNR
                        CHANGING EV_GUID TYPE QRP_ACCASSOBJ
                                 EV_PKOSA_OBJNR TYPE OBJNR
                                 EV_WERKS TYPE WERKS_D
                                 EV_MATNR TYPE MATNR
                                 EV_VERID TYPE VERID
                                 EV_KOKRS TYPE KOKRS.
* read OBJNR of PKOSA
  SELECT SINGLE OBJNR
           FROM AUFK
           INTO EV_PKOSA_OBJNR
          WHERE AUFNR = IV_AUFNR.




* read CC_GUID from PKOSA for table PPC_ORD_INF
  CALL FUNCTION 'QRP_QRP002_READ'
    EXPORTING
      IF_AUFNR   = IV_AUFNR
    IMPORTING
      EF_CC_GUID = EV_GUID
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  SELECT SINGLE PRWRK
                PMATN
                VERID
           FROM CKMLMV013
           INTO (EV_WERKS,
                 EV_MATNR,
                 EV_VERID)
          WHERE AUFNR = IV_AUFNR.

  CALL FUNCTION 'RM_KOKRS_TO_PLANT_FIND'
    EXPORTING
      WERKS   = EV_WERKS
    IMPORTING
      E_KOKRS = EV_KOKRS
    EXCEPTIONS
      OTHERS  = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " mater_data_selection
*&---------------------------------------------------------------------*
*&      Form  enqueue
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENQUEUE USING IV_OBJNR TYPE OBJNR
          CHANGING EV_ERROR TYPE CHAR1.
  DATA: LV_AUFNR TYPE QRP002-AUFNR.
* it's safe to lock all periods of this P.C.C
  LV_AUFNR = IV_OBJNR+2.

  CALL FUNCTION 'ENQUEUE_E_QRP002'
    EXPORTING
      MODE_QRP002    = 'E'
      AUFNR          = LV_AUFNR
      _SCOPE         = '3'
      _WAIT          = 'X'
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    EV_ERROR = 'L'.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " enqueue
*&---------------------------------------------------------------------*
*&      Form  read_ppc_orderid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PPC_ORDERID USING IV_GUID TYPE QRP_ACCASSOBJ
                   CHANGING ET_PPC_ORD_INF TYPE ZSAPBF_TT_PPC_ORD_INF.

*read orderid from PPC_ORD_INF with guid from PKOSA

  SELECT * APPENDING TABLE ET_PPC_ORD_INF
           FROM PPC_ORD_INF
          WHERE ACCASSOBJ = IV_GUID.
*            AND dummy_order = space.
  IF SY-SUBRC <> 0.
    MESSAGE E314.
  ENDIF.


ENDFORM.                    " read_ppc_orderid

*&---------------------------------------------------------------------*
*&      Form  read_ppc_headid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PPC_HEADID USING IV_FIRST_DAY TYPE SY-DATUM
                           IV_LAST_DAY TYPE SY-DATUM "Added by James Kim 2011/01/25
                           IT_PPC_ORD_INF TYPE ZSAPBF_TT_PPC_ORD_INF
                  CHANGING ET_PPC_HEAD TYPE PPC_T_HEAD
                           LV_ERROR_FLAG.

* select PPC_HEAD entries, which are in the current period ,
* needed for determination of WIP credit in
* actual period (function modul PPC1DM_COMP_ORD_PART_GET) and WIP debit
* in actual period (funtion modul PPC1DC_COMP_ORD_READ_NEW)

*{ Changed by SAPCDP08 2010-07-07 WIP correction report performance tunning.
  CHECK NOT IT_PPC_ORD_INF IS INITIAL.
  SELECT * INTO TABLE ET_PPC_HEAD
    FROM PPC_HEAD
    FOR ALL ENTRIES IN IT_PPC_ORD_INF
   WHERE ORDERID EQ IT_PPC_ORD_INF-ORDERID
*     AND flg_del EQ space " Remove for HMMA case, PPC_HEAD will be deleted after shipping.
     AND FLG_SYNCH = 'X'
     AND POSTDATE BETWEEN IV_FIRST_DAY AND IV_LAST_DAY
     " The postdate range was Chaged; Explained by Sung-Kon James Kim 2011/01/24
     " The postdate range will be suitable,
     " because [et_ppc_head] will be used only when calculation of the field "GMPER" and "VARMN"
     " for the posting period.
    %_HINTS ORACLE '&max_blocking_factor  10& &max_in_blocking_factor  10&'
    .

*  LOOP AT it_ppc_ord_inf ASSIGNING <fs_ppc_ord_inf>.
*    SELECT * APPENDING TABLE et_ppc_head
*      FROM ppc_head
*     WHERE orderid EQ <fs_ppc_ord_inf>-orderid
*       AND flg_del EQ space
*       AND flg_synch = 'X'
**       AND ( ( flg_synch = 'X' OR flg_synch = 'V' ) OR
**             ( flg_asynch = 'X' OR flg_asynch = 'V' ) OR
**             ( flg_asynch_a = 'X' OR flg_asynch_a = 'V' ) )
*       AND postdate >= iv_first_day.
**       AND crtime <= gd_timestmp
*  ENDLOOP.
*} End of change.
  IF ET_PPC_HEAD IS INITIAL.
    MESSAGE S304 DISPLAY LIKE 'E'.
    LV_ERROR_FLAG = 'X'.
  ENDIF.


ENDFORM.                    " read_ppc_headid
*&---------------------------------------------------------------------*
*&      Form  dequeue
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEQUEUE USING IV_OBJNR TYPE OBJNR.
  DATA: LV_AUFNR TYPE QRP002-AUFNR.

  LV_AUFNR = IV_OBJNR+2.
  CALL FUNCTION 'DEQUEUE_E_QRP002'
    EXPORTING
      MODE_QRP002 = 'E'
      AUFNR       = LV_AUFNR
      _SCOPE      = '3'.

ENDFORM.                    " dequeue
*&---------------------------------------------------------------------*
*&      Form  read_documents
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DOCUMENTS USING IT_PPC_ORD_INF TYPE ZSAPBF_TT_PPC_ORD_INF
                          IT_PPC_HEAD TYPE PPC_T_HEAD
                          IT_PERIODS TYPE TT_PERIOD
                 CHANGING ET_MAT_COMP_ORDERID TYPE TT_MAT_COMP_ORDERID      "component: forward
                          ET_MAT_REV_ORDERID TYPE TT_MAT_COMP_ORDERID       "component: reverse
                          ET_MAT_COMP_VAR_ORDERID TYPE TT_MAT_COMP_ORDERID  "component: forward variance
                          ET_MAT_REV_VAR_ORDERID TYPE TT_MAT_COMP_ORDERID   "component: reverse variance

                          ET_ACT_COMP_ORDERID TYPE TT_ACT_COMP_ORDERID      "activity: forward
                          ET_ACT_REV_ORDERID TYPE TT_ACT_COMP_ORDERID       "activity: reverse
                          ET_ACT_COMP_VAR_ORDERID TYPE TT_ACT_COMP_ORDERID  "activity: forward variance
                          ET_ACT_REV_VAR_ORDERID TYPE TT_ACT_COMP_ORDERID   "activity: reverse variance .
                          .

  DATA: FT_MATERIAL_COMPONENTS TYPE	STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,
        FT_REV_MAT_COMPONENTS TYPE STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,
        FT_MAT_COMP_VAR TYPE STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,
        FT_REV_MAT_COMP_VAR TYPE STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,

        FT_ACT_COMP TYPE STANDARD TABLE OF  PPC_ACTIVITY_COMPONENTS,
        FT_REV_ACT_COMP TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS,
        FT_ACT_COMP_VAR TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS,
        FT_REV_ACT_COMP_VAR TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS,
*        ft_reppoints TYPE STANDARD TABLE OF ppc_reppoints,
*        ft_rev_reppoints TYPE STANDARD TABLE OF ppc_reppoints,

        LT_MAT_COMP_ORDERID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,
        LT_ACT_COMP_ORDERID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
        LT_MAT_COMP_VAR_ORDERID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,
        LT_REV_MAT_COM_ORDERID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,

        LT_ACT_COMP_VAR_ORDERID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
        LT_REV_MAT_COMP_VAR_ID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,
        LT_REV_ACT_COMP_ORDERID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
        LT_REV_ACT_COMP_VAR_ID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP.

*
  DATA: FT_PPC_REPPOINT TYPE TABLE OF PPC_REPPOINT_INT,
        LS_PPC_REPPOINT TYPE PPC_REPPOINT_INT.

* -B RWU- Unit unify with CSSL 2008-11-18
  DATA LT_ACTCOMP_UNIT_UNIFY_PRE_TAB TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS.
* -E RWU-
  FIELD-SYMBOLS: <LFS_MAT_COMP_ORDERID> TYPE GT_MAT_COMP_ORDERID_TYP,
                 <LFS_ACT_COMP_ORDERID> TYPE GT_ACT_COMP_ORDERID_TYP,
                 <LFS_ACT_COMP_VAR_ORDERID> TYPE GT_ACT_COMP_ORDERID_TYP,
                 <LFS_MAT_COMP_VAR_ORDERID> TYPE GT_MAT_COMP_ORDERID_TYP,
                 <FS_PERIOD> TYPE TS_PERIOD.



  REFRESH: LT_MAT_COMP_ORDERID,
           LT_ACT_COMP_ORDERID.

* read all quantities for every APO order on reporting points base
* get orderid
  LOOP AT IT_PPC_ORD_INF ASSIGNING <FS_PPC_ORD_INF>.
    LOOP AT IT_PERIODS ASSIGNING <FS_PERIOD>.
* get reporting points
      LOOP AT IT_PPC_HEAD ASSIGNING <FS_PPC_HEAD>
                              WHERE ORDERID = <FS_PPC_ORD_INF>-ORDERID
                                AND POSTDATE >= <FS_PERIOD>-STARTDAY
                                AND POSTDATE <= <FS_PERIOD>-ENDDAY.
        CHECK NOT <FS_PPC_HEAD>-REPPOINT IS INITIAL.
        LS_PPC_REPPOINT = <FS_PPC_HEAD>-REPPOINT.
        APPEND LS_PPC_REPPOINT TO FT_PPC_REPPOINT.
      ENDLOOP.
      IF SY-SUBRC = 0.
*        CALL FUNCTION 'PPC1DC_COMP_ORD_READ_NEW'
        CALL FUNCTION 'ZSAPBF_PPC1_COMP_ORD_READ_NEW'
          EXPORTING
            IF_ORDERID             = <FS_PPC_ORD_INF>-ORDERID
            IT_REPPOINTS           = FT_PPC_REPPOINT
            I_DATE_LOW             = <FS_PERIOD>-STARTDAY
            I_DATE_HIGH            = <FS_PERIOD>-ENDDAY
          IMPORTING
            ET_MATERIAL_COMPONENTS = FT_MATERIAL_COMPONENTS
            ET_REV_MAT_COMPONENTS  = FT_REV_MAT_COMPONENTS
            ET_MAT_COMP_VAR        = FT_MAT_COMP_VAR
            ET_REV_MAT_COMP_VAR    = FT_REV_MAT_COMP_VAR
            ET_ACT_COMP            = FT_ACT_COMP
            ET_REV_ACT_COMP        = FT_REV_ACT_COMP
            ET_ACT_COMP_VAR        = FT_ACT_COMP_VAR
            ET_REV_ACT_COMP_VAR    = FT_REV_ACT_COMP_VAR
*           et_reppoints           = ft_reppoints
*           et_rev_reppoints       = ft_rev_reppoints
          EXCEPTIONS
            LOCK_ERROR             = 1
            OTHERS                 = 2.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        APPEND LINES OF FT_MATERIAL_COMPONENTS TO LT_MAT_COMP_ORDERID.
        APPEND LINES OF FT_REV_MAT_COMPONENTS TO LT_REV_MAT_COM_ORDERID.
        APPEND LINES OF FT_MAT_COMP_VAR TO LT_MAT_COMP_VAR_ORDERID.
        APPEND LINES OF FT_REV_MAT_COMP_VAR TO LT_REV_MAT_COMP_VAR_ID.
        APPEND LINES OF FT_ACT_COMP TO LT_ACT_COMP_ORDERID.
        APPEND LINES OF FT_REV_ACT_COMP TO LT_REV_ACT_COMP_ORDERID.
        APPEND LINES OF FT_ACT_COMP_VAR TO LT_ACT_COMP_VAR_ORDERID.
        APPEND LINES OF FT_REV_ACT_COMP_VAR TO LT_REV_ACT_COMP_VAR_ID.
*        APPEND LINES OF ft_reppoints TO gt_reppoints.
*        APPEND LINES OF ft_rev_reppoints TO gt_rev_reppoints.

* -B RWU- Unit unify with CSSL 2008-11-18
        APPEND LINES OF FT_ACT_COMP TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.
        APPEND LINES OF FT_REV_ACT_COMP TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.
        APPEND LINES OF FT_ACT_COMP_VAR TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.
        APPEND LINES OF FT_REV_ACT_COMP_VAR TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.

        SORT LT_ACTCOMP_UNIT_UNIFY_PRE_TAB BY RESSOURCE_GUID.
        DELETE ADJACENT DUPLICATES FROM LT_ACTCOMP_UNIT_UNIFY_PRE_TAB COMPARING RESSOURCE_GUID.

        PERFORM PREPARE_UNIT_UNIFY_FOR_ACTCOMP USING LT_ACTCOMP_UNIT_UNIFY_PRE_TAB
                                                     <FS_PERIOD>-GJPER(4)
                                                     .
* -E RWU-

* substract variance from components and append components and orderid
* to itab
        LOOP AT LT_MAT_COMP_ORDERID ASSIGNING <LFS_MAT_COMP_ORDERID>.
          <LFS_MAT_COMP_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_MAT_COMP_ORDERID>-GJPER = <FS_PERIOD>-GJPER.

          <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY =
          <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY -
          <LFS_MAT_COMP_ORDERID>-PPC_MAT-DELTA_QUANTITY.
        ENDLOOP.
        APPEND LINES OF LT_MAT_COMP_ORDERID TO ET_MAT_COMP_ORDERID.
        REFRESH LT_MAT_COMP_ORDERID.

* append reversals (components) and orderid to itab
        LOOP AT LT_REV_MAT_COM_ORDERID ASSIGNING <LFS_MAT_COMP_ORDERID>.
          <LFS_MAT_COMP_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_MAT_COMP_ORDERID>-GJPER = <FS_PERIOD>-GJPER.

          <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY =
          <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY -
          <LFS_MAT_COMP_ORDERID>-PPC_MAT-DELTA_QUANTITY.
        ENDLOOP.
        APPEND LINES OF LT_REV_MAT_COM_ORDERID TO ET_MAT_REV_ORDERID.
        REFRESH LT_REV_MAT_COM_ORDERID.

* substract variances from activities and append activities and orderid
* to itab
        LOOP AT LT_ACT_COMP_ORDERID ASSIGNING <LFS_ACT_COMP_ORDERID>.
          <LFS_ACT_COMP_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_ACT_COMP_ORDERID>-GJPER = <FS_PERIOD>-GJPER.

* -B RWU- Unit unify with CSSL 2008-11-18
          PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_ORDERID>.

          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR =
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR -
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_VAR.
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX =
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX -
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_FIX.
* -E RWU-                                    .
        ENDLOOP.
        APPEND LINES OF LT_ACT_COMP_ORDERID TO ET_ACT_COMP_ORDERID.
        REFRESH LT_ACT_COMP_ORDERID.

* append reversals (activities) and orderid to itab
        LOOP AT LT_REV_ACT_COMP_ORDERID ASSIGNING <LFS_ACT_COMP_ORDERID>.
          <LFS_ACT_COMP_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_ACT_COMP_ORDERID>-GJPER = <FS_PERIOD>-GJPER.

* -B RWU- Unit unify with CSSL 2008-11-18
          PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_ORDERID>.

          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR =
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR -
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_VAR.
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX =
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX -
          <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_FIX.
* -E RWU-                               .
        ENDLOOP.
        APPEND LINES OF LT_REV_ACT_COMP_ORDERID TO ET_ACT_REV_ORDERID.
        REFRESH LT_REV_ACT_COMP_ORDERID.

* append variances (components) and orderid to itab
        LOOP AT LT_MAT_COMP_VAR_ORDERID ASSIGNING <LFS_MAT_COMP_VAR_ORDERID>.
          <LFS_MAT_COMP_VAR_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_MAT_COMP_VAR_ORDERID>-GJPER = <FS_PERIOD>-GJPER.
        ENDLOOP.
        APPEND LINES OF LT_MAT_COMP_VAR_ORDERID TO ET_MAT_COMP_VAR_ORDERID.
        REFRESH LT_MAT_COMP_VAR_ORDERID.

* append reversals of variances (components) and orderid to itab
        LOOP AT LT_REV_MAT_COMP_VAR_ID ASSIGNING <LFS_MAT_COMP_VAR_ORDERID>.
          <LFS_MAT_COMP_VAR_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_MAT_COMP_VAR_ORDERID>-GJPER = <FS_PERIOD>-GJPER.
        ENDLOOP.
        APPEND LINES OF LT_REV_MAT_COMP_VAR_ID TO ET_MAT_REV_VAR_ORDERID.
        REFRESH LT_REV_MAT_COMP_VAR_ID.

* append variances (activities) and orderid to itab
        LOOP AT LT_ACT_COMP_VAR_ORDERID ASSIGNING <LFS_ACT_COMP_VAR_ORDERID>.
          <LFS_ACT_COMP_VAR_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_ACT_COMP_VAR_ORDERID>-GJPER = <FS_PERIOD>-GJPER.
* -B RWU- Unit unify with CSSL 2008-11-18
          PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_VAR_ORDERID>.
* -E RWU-                                    .
        ENDLOOP.
        APPEND LINES OF LT_ACT_COMP_VAR_ORDERID TO ET_ACT_COMP_VAR_ORDERID.
        REFRESH LT_ACT_COMP_VAR_ORDERID.

* append reversals of variances (activities) and orderid to itab
        LOOP AT LT_REV_ACT_COMP_VAR_ID ASSIGNING <LFS_ACT_COMP_VAR_ORDERID>  .
          <LFS_ACT_COMP_VAR_ORDERID>-ORDERID = <FS_PPC_HEAD>-ORDERID.
          <LFS_ACT_COMP_VAR_ORDERID>-GJPER = <FS_PERIOD>-GJPER.
* -B RWU- Unit unify with CSSL 2008-11-18
          PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_VAR_ORDERID>.
* -E RWU-                                .
        ENDLOOP.
        APPEND LINES OF LT_REV_ACT_COMP_VAR_ID TO ET_ACT_REV_VAR_ORDERID.
        REFRESH LT_REV_ACT_COMP_VAR_ID.
      ENDIF.
      REFRESH FT_PPC_REPPOINT.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " read_documents
*&---------------------------------------------------------------------*
*&      Form  substract_reversals_mat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBSTRACT_REVERSALS_MAT USING IT_MAT_REV_ORDERID TYPE TT_MAT_COMP_ORDERID
                          CHANGING CT_MAT_COMP_ORDERID TYPE TT_MAT_COMP_ORDERID.

* substract reversals from components for later valuation
*  SORT gt_mat_rev_orderid BY ppc_mat-mat_number ppc_mat-reppoint orderid.
*
*  LOOP AT gt_mat_comp_orderid ASSIGNING <fs_mat_comp_orderid>.
*    READ TABLE gt_mat_rev_orderid BINARY SEARCH ASSIGNING
*          <fs_mat_rev_orderid> WITH KEY
*          ppc_mat-mat_number = <fs_mat_comp_orderid>-ppc_mat-mat_number
*          ppc_mat-reppoint   = <fs_mat_comp_orderid>-ppc_mat-reppoint
*          orderid            = <fs_mat_comp_orderid>-orderid
*          gjper              = <fs_mat_comp_orderid>-gjper.
*    IF sy-subrc = 0.
*      <fs_mat_comp_orderid>-ppc_mat-quantity = <fs_mat_comp_orderid>-ppc_mat-quantity -
*                                               <fs_mat_rev_orderid>-ppc_mat-quantity.
*    ENDIF.
*  ENDLOOP.


  LOOP AT IT_MAT_REV_ORDERID ASSIGNING <FS_MAT_REV_ORDERID>.
    <FS_MAT_REV_ORDERID>-PPC_MAT-QUANTITY = 0 - <FS_MAT_REV_ORDERID>-PPC_MAT-QUANTITY.
    <FS_MAT_REV_ORDERID>-PPC_MAT-DELTA_QUANTITY = 0 - <FS_MAT_REV_ORDERID>-PPC_MAT-DELTA_QUANTITY.
    COLLECT <FS_MAT_REV_ORDERID> INTO CT_MAT_COMP_ORDERID.
  ENDLOOP.
ENDFORM.                    " substract_reversals_mat
*&---------------------------------------------------------------------*
*&      Form  substract_reversals_act
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBSTRACT_REVERSALS_ACT USING IT_ACT_REV_ORDERID TYPE TT_ACT_COMP_ORDERID
                          CHANGING CT_ACT_COMP_ORDERID TYPE TT_ACT_COMP_ORDERID .

* substract reversals from activities for later valuation
*  SORT gt_act_rev_orderid BY ppc_act-reppoint ppc_act-ressource_guid orderid.
*  LOOP AT gt_act_comp_orderid ASSIGNING <fs_act_comp_orderid>.
*    READ TABLE gt_act_rev_orderid BINARY SEARCH ASSIGNING
*          <fs_act_rev_orderid> WITH KEY
*          ppc_act-reppoint       = <fs_act_comp_orderid>-ppc_act-reppoint
*          ppc_act-ressource_guid = <fs_act_comp_orderid>-ppc_act-ressource_guid
*          orderid                = <fs_act_comp_orderid>-orderid
*          gjper                  = <fs_act_comp_orderid>-gjper.
*    IF sy-subrc = 0.
*      <fs_act_comp_orderid>-ppc_act-duration_var = <fs_act_comp_orderid>-ppc_act-duration_var -
*                                                   <fs_act_rev_orderid>-ppc_act-duration_var.
*      <fs_act_comp_orderid>-ppc_act-duration_fix = <fs_act_comp_orderid>-ppc_act-duration_fix -
*                                                   <fs_act_rev_orderid>-ppc_act-duration_fix.
*    ENDIF.
*
*  ENDLOOP.

  LOOP AT IT_ACT_REV_ORDERID ASSIGNING <FS_ACT_COMP_ORDERID>.
    <FS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR = 0 - <FS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR.
    <FS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_VAR = 0 - <FS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_VAR.

    <FS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX = 0 - <FS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX.
    <FS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_FIX = 0 - <FS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_FIX.

    COLLECT <FS_ACT_COMP_ORDERID> INTO CT_ACT_COMP_ORDERID.
  ENDLOOP.

ENDFORM.                    " substract_reversals_act
*&---------------------------------------------------------------------*
*&      Form  compress_wip_quant_credit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPRESS_WIP_QUANT_CREDIT USING IT_PPC_HEAD TYPE PPC_T_HEAD
                            CHANGING ET_PPC_HEAD_SMALL TYPE TT_PPC_HEAD_SMALL
                              .
  DATA: LS_PPC_HEAD_SMALL TYPE GT_PPC_HEAD_SMALL_TYP.
* PPC_HEAD is compressed in PPC_HEAD_SMALL, only orderid, reppoint and
* quantity per orderid/reppoint is needed
  REFRESH ET_PPC_HEAD_SMALL.
  LOOP AT IT_PPC_HEAD ASSIGNING <FS_PPC_HEAD>.
    IF <FS_PPC_HEAD>-FLG_REVERSAL = CHARX.
      <FS_PPC_HEAD>-CONFQUANT = <FS_PPC_HEAD>-CONFQUANT * -1.
    ENDIF.
    IF <FS_PPC_HEAD>-FLG_GR_HEAD = CHARX
    OR <FS_PPC_HEAD>-FLG_SCRAP = CHARX.
      MOVE-CORRESPONDING <FS_PPC_HEAD> TO LS_PPC_HEAD_SMALL. "#EC ENHOK
      LS_PPC_HEAD_SMALL-GJPER(4) = <FS_PPC_HEAD>-POSTDATE(4).
      LS_PPC_HEAD_SMALL-GJPER+5(2) = <FS_PPC_HEAD>-POSTDATE+4(2).
      COLLECT LS_PPC_HEAD_SMALL INTO ET_PPC_HEAD_SMALL.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " compress_wip_quant_credit
*&---------------------------------------------------------------------*
*&      Form  determin_wip_credit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETERMIN_WIP_CREDIT USING IT_PPC_HEAD_SMALL TYPE TT_PPC_HEAD_SMALL
                               IT_PERIODS TYPE TT_PERIOD
                      CHANGING ET_COMP_METHOD_CREDIT TYPE TT_COMP_METHOD
                               ET_ACT_METHOD_CREDIT TYPE TT_ACT_METHOD
                              .


  DATA: FTAB_MATERIAL TYPE STANDARD TABLE OF
        PPC_MATERIAL_COMPONENTS,
        FTAB_ACTIVITY TYPE STANDARD TABLE OF
        PPC_ACTIVITY_COMPONENTS.

  DATA: LT_ACT_WIP_CREDIT TYPE STANDARD TABLE OF
        GT_ACT_COMP_ORDERID_TYP,
        LT_COMP_WIP_CREDIT TYPE STANDARD TABLE OF
        GT_MAT_COMP_ORDERID_TYP.
*----------------------------------------------------------------------*
* table with components quantities (WIP-CREDIT)
  DATA: LT_COMP_WIP_CREDIT_FINAL TYPE	STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,

* table with activities quantities  (WIP-CREDIT)
        LT_ACT_WIP_CREDIT_FINAL TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,

* table with activities quantities with ACT_OBJNR (KALNR) (WIP-CREDIT)
        LT_ACT_WIP_CREDIT_OBJNR TYPE STANDARD TABLE OF GT_ACT_COMP_OBJNR_TYP.

  DATA DUMMY_T_REPPOINT TYPE PPC_T_REPPOINT.

  FIELD-SYMBOLS: <FS_ACT_WIP_CREDIT> TYPE GT_ACT_COMP_ORDERID_TYP,
                 <FS_COMP_WIP_CREDIT> TYPE GT_MAT_COMP_ORDERID_TYP,
                 <FS_PERIOD> TYPE TS_PERIOD.


  LOOP AT IT_PERIODS ASSIGNING <FS_PERIOD>.
    LOOP AT IT_PPC_HEAD_SMALL ASSIGNING <FS_PPC_HEAD_SMALL> WHERE GJPER = <FS_PERIOD>-GJPER.

      IF NOT <FS_PPC_HEAD_SMALL>-REPPOINT IS INITIAL.
*        CALL FUNCTION 'PPC1DM_COMP_ORD_PART_GET' "Commentated by Sung-Kon Kim 2010.09.13
        CALL FUNCTION 'ZSAPBF_PPC1_COMP_ORD_PART_GET' "revoked by Sung-Kon Kim 2010.09.13
            EXPORTING
              IF_ORDERID                   = <FS_PPC_HEAD_SMALL>-ORDERID
              IF_HEADQUANT                 = <FS_PPC_HEAD_SMALL>-CONFQUANT
              IF_REPPOINT                  = <FS_PPC_HEAD_SMALL>-REPPOINT
*          i_date_low                   = <fs_period>-startday
*          i_date_high                  = <fs_period>-endday
*   IF_BW_CALL                   = ' '
        "    i_date_low                   = <fs_period>-startday
            I_DATE_LOW                   = '19000101' "revoked 2010.09.13 / Changed by Sung-Kon Kim 2010.01.15
            I_DATE_HIGH                  = <FS_PERIOD>-ENDDAY "revoked by Sung-Kon Kim 2010.09.13

***** Important Remark; by Sung Kon James Kime 2011/01/25 ********************************************
* The period should be between '19000101' and The end of the periods (posting period + current period)
*-----------------------------------------------------------------------------------------------------
* Above 1 line; from <fs_period>-startday to '19000101' Changed by James Kim 2011/01/25
* because although so many months ago,
* all previous period's confirmation components/activities should be included,
* When calculation of the fields "GMSUM, XMSUM".
* If the i_date_low is initial, then the 'ZSAPBF_PPC1_COMP_ORD_PART_GET' will act incorrectly,
* due to stupid selection option processing within the function module.
******************************************************************************************************
           IMPORTING
             ET_MATERIAL_COMPONENTS       = FTAB_MATERIAL
             ET_ACTIVITY_COMPONENTS       = FTAB_ACTIVITY
           EXCEPTIONS
             GET_COMP_ERROR               = 1
             INPUT_ERROR                  = 2
             NO_RP_FROM_BW                = 3
             OTHERS                       = 4
                    .
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSE. " This routine does not make sence.
        " Because the GR can be performed with reporting point/orderid.
        " Explained by James Kim 2011/01/25
        CALL FUNCTION 'ZSAPBF_PPC1_COMP_ORD_READ_NEW'
          EXPORTING
            IF_ORDERID             = <FS_PPC_HEAD_SMALL>-ORDERID
            IT_REPPOINTS           = DUMMY_T_REPPOINT
            I_DATE_LOW             = <FS_PERIOD>-STARTDAY
            I_DATE_HIGH            = <FS_PERIOD>-ENDDAY
          IMPORTING
            ET_MATERIAL_COMPONENTS = FTAB_MATERIAL
*           ET_REV_MAT_COMPONENTS  =
*           ET_MAT_COMP_VAR        =
*           ET_REV_MAT_COMP_VAR    =
            ET_ACT_COMP            = FTAB_ACTIVITY
*           ET_REV_ACT_COMP        =
*           ET_ACT_COMP_VAR        =
*           ET_REV_ACT_COMP_VAR    =
*           ET_REPPOINTS           =
*           ET_REV_REPPOINTS       =
          EXCEPTIONS
            LOCK_ERROR             = 1
            OTHERS                 = 2.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

      APPEND LINES OF FTAB_MATERIAL TO LT_COMP_WIP_CREDIT.
      APPEND LINES OF FTAB_ACTIVITY TO LT_ACT_WIP_CREDIT.
* substract variances (components), add orderid and append to itab
      LOOP AT LT_COMP_WIP_CREDIT ASSIGNING <FS_COMP_WIP_CREDIT>.
        <FS_COMP_WIP_CREDIT>-ORDERID = <FS_PPC_HEAD_SMALL>-ORDERID.
        <FS_COMP_WIP_CREDIT>-GJPER = <FS_PERIOD>-GJPER.

        <FS_COMP_WIP_CREDIT>-PPC_MAT-QUANTITY =
        <FS_COMP_WIP_CREDIT>-PPC_MAT-QUANTITY -
        <FS_COMP_WIP_CREDIT>-PPC_MAT-DELTA_QUANTITY.
      ENDLOOP.
      APPEND LINES OF LT_COMP_WIP_CREDIT TO LT_COMP_WIP_CREDIT_FINAL.
      REFRESH LT_COMP_WIP_CREDIT.

* substract variances (activities), add orderid and append to itab
      LOOP AT LT_ACT_WIP_CREDIT ASSIGNING <FS_ACT_WIP_CREDIT>.
        <FS_ACT_WIP_CREDIT>-ORDERID = <FS_PPC_HEAD_SMALL>-ORDERID.
        <FS_ACT_WIP_CREDIT>-GJPER = <FS_PERIOD>-GJPER.
* -B RWU- Unit unify with CSSL 2008-11-18
        PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <FS_ACT_WIP_CREDIT>.

        <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_VAR =
        <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_VAR -
        <FS_ACT_WIP_CREDIT>-PPC_ACT-DELTA_DUR_VAR.
        <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_FIX =
        <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_FIX -
        <FS_ACT_WIP_CREDIT>-PPC_ACT-DELTA_DUR_FIX.
      ENDLOOP.
      APPEND LINES OF LT_ACT_WIP_CREDIT TO LT_ACT_WIP_CREDIT_FINAL.
      REFRESH LT_ACT_WIP_CREDIT.


    ENDLOOP.
  ENDLOOP.
  PERFORM GET_ACT_OBJNR USING LT_ACT_WIP_CREDIT_FINAL
                     CHANGING LT_ACT_WIP_CREDIT_OBJNR.

  PERFORM COMPRESS_WIP USING LT_ACT_WIP_CREDIT_OBJNR
                             LT_COMP_WIP_CREDIT_FINAL
                    CHANGING ET_ACT_METHOD_CREDIT
                             ET_COMP_METHOD_CREDIT
                             .
  SORT ET_COMP_METHOD_CREDIT BY REPPOINT COSTING_NUM MAT_NUMBER ORDERID
                                COST_CENTER ACTIVITY_TYPE.
  SORT ET_ACT_METHOD_CREDIT BY REPPOINT RESSOURCE_GUID ACT_OBJNR
                               DURUNIT ORDERID COST_CENTER ACTIVITY_TYPE.
ENDFORM.                    " determin_wip_credit
*&---------------------------------------------------------------------*
*&      Form  determin_wip_debit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETERMIN_WIP_DEBIT USING IT_MAT_COMP_ORDERID TYPE TT_MAT_COMP_ORDERID
                              IT_ACT_COMP_ORDERID TYPE TT_ACT_COMP_ORDERID
                     CHANGING ET_COMP_METHOD_DEBIT TYPE TT_COMP_METHOD
                              ET_ACT_METHOD_DEBIT TYPE TT_ACT_METHOD
                              .
* table with activities quantities with ACT_OBJNR (KALNR) (WIP-DEBIT)
  DATA: LT_ACT_COMP_OBJNR TYPE STANDARD TABLE OF GT_ACT_COMP_OBJNR_TYP.

  PERFORM GET_ACT_OBJNR USING IT_ACT_COMP_ORDERID
                     CHANGING LT_ACT_COMP_OBJNR.

  PERFORM COMPRESS_WIP USING LT_ACT_COMP_OBJNR
                             IT_MAT_COMP_ORDERID
                    CHANGING ET_ACT_METHOD_DEBIT
                             ET_COMP_METHOD_DEBIT
                            .


ENDFORM.                    " determin_wip_debit
*&---------------------------------------------------------------------*
*&      Form  substract_wip_credit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBSTRACT_WIP_CREDIT USING IT_ACT_METHOD_CREDIT TYPE TT_ACT_METHOD
                                IT_COMP_METHOD_CREDIT TYPE TT_COMP_METHOD
                       CHANGING CT_COMP_METHOD_DEBIT TYPE TT_COMP_METHOD
                                CT_ACT_METHOD_DEBIT TYPE TT_ACT_METHOD
                                .                           "#EC CALLED
* the WIP-credit (COMP) is substracted from the WIP-debit here
  LOOP AT CT_COMP_METHOD_DEBIT ASSIGNING <FS_COMP_METHOD_DEBIT>.
    READ TABLE IT_COMP_METHOD_CREDIT BINARY SEARCH WITH KEY
               REPPOINT = <FS_COMP_METHOD_DEBIT>-REPPOINT
               COSTING_NUM = <FS_COMP_METHOD_DEBIT>-COSTING_NUM
               MAT_NUMBER = <FS_COMP_METHOD_DEBIT>-MAT_NUMBER
               ORDERID = <FS_COMP_METHOD_DEBIT>-ORDERID
               COST_CENTER = <FS_COMP_METHOD_DEBIT>-COST_CENTER
               ACTIVITY_TYPE = <FS_COMP_METHOD_DEBIT>-ACTIVITY_TYPE
               ASSIGNING <FS_COMP_METHOD_CREDIT>.
    IF SY-SUBRC = 0.
      <FS_COMP_METHOD_DEBIT>-QUANTITY = <FS_COMP_METHOD_DEBIT>-QUANTITY -
                                         <FS_COMP_METHOD_CREDIT>-QUANTITY.
    ENDIF.
  ENDLOOP.

* the WIP-credit (ACT) is substracted from the WIP-debit here
  LOOP AT CT_ACT_METHOD_DEBIT ASSIGNING <FS_ACT_METHOD_DEBIT>.
    READ TABLE IT_ACT_METHOD_CREDIT BINARY SEARCH WITH KEY
               REPPOINT = <FS_ACT_METHOD_DEBIT>-REPPOINT
               RESSOURCE_GUID = <FS_ACT_METHOD_DEBIT>-RESSOURCE_GUID
               ORDERID = <FS_ACT_METHOD_DEBIT>-ORDERID
               COST_CENTER = <FS_ACT_METHOD_DEBIT>-COST_CENTER
               ACTIVITY_TYPE = <FS_ACT_METHOD_DEBIT>-ACTIVITY_TYPE
               ACT_OBJNR = <FS_ACT_METHOD_DEBIT>-ACT_OBJNR
               ASSIGNING <FS_ACT_METHOD_CREDIT>.
    IF SY-SUBRC = 0.
      <FS_ACT_METHOD_DEBIT>-DURATION_VAR = <FS_ACT_METHOD_DEBIT>-DURATION_VAR -
                                           <FS_ACT_METHOD_CREDIT>-DURATION_VAR.
      <FS_ACT_METHOD_DEBIT>-DURATION_FIX = <FS_ACT_METHOD_DEBIT>-DURATION_FIX -
                                           <FS_ACT_METHOD_CREDIT>-DURATION_FIX.
      <FS_ACT_METHOD_DEBIT>-DELTA_DUR_VAR = <FS_ACT_METHOD_DEBIT>-DELTA_DUR_VAR -
                                            <FS_ACT_METHOD_CREDIT>-DELTA_DUR_VAR.
      <FS_ACT_METHOD_DEBIT>-DELTA_DUR_FIX = <FS_ACT_METHOD_DEBIT>-DELTA_DUR_FIX -
                                            <FS_ACT_METHOD_CREDIT>-DELTA_DUR_FIX.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " substract_wip_credit
*&---------------------------------------------------------------------*
*&      Form  CPZP_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CPZP_BUILD USING IT_COMP_METHOD_DEBIT TYPE TT_COMP_METHOD
                      IT_ACT_METHOD_DEBIT TYPE TT_ACT_METHOD
                      IV_PKOSA_OBJNR TYPE OBJNR
                      IV_KOKRS TYPE KOKRS
             CHANGING ET_CPZP TYPE ZSAPBF_TT_CPZP.

  DATA: LS_CPZP TYPE CPZP,
        LV_CON_KEY_VAR TYPE COUNT_ZP VALUE '00000000'.
*        lv_con_key_fix TYPE count_zp VALUE '00000001'.


* read line items from gt_comp_method_debit (WIP debit components) to *
* get f_objnr via ENCODE-function module
  LOOP AT IT_COMP_METHOD_DEBIT ASSIGNING <FS_COMP_METHOD_DEBIT>.
*    CHECK NOT <fs_comp_method_debit>-quantity IS INITIAL OR
*          NOT <fs_comp_method_debit>-delta_quantity IS INITIAL.

****Start; --- "Temporary Source Code by James Sung Kon Kim 2011.02.22
    IF <FS_COMP_METHOD_DEBIT>-BATCH IS NOT INITIAL.
      CLEAR <FS_COMP_METHOD_DEBIT>-COSTING_NUM.
    ENDIF.
****End; --- "Temporary Source Code by James Sung Kon Kim 2011.02.22

    CALL FUNCTION 'QRP_APO_COMP_OBJNR_ENCODE'
     EXPORTING
       IF_MATNR               = <FS_COMP_METHOD_DEBIT>-MAT_NUMBER
       IF_WERKS               = <FS_COMP_METHOD_DEBIT>-PLANT
**** "Temporary Blocking by James Sung Kon Kim 2011.02.22
****       if_bwtar               = <fs_comp_method_debit>-batch
       IF_VBELN               = <FS_COMP_METHOD_DEBIT>-SALES_DOC
       IF_POSNR               = <FS_COMP_METHOD_DEBIT>-SALES_DOC_ITEM
*       IF_PSPNR               = ' '
       IF_KZBWS               = <FS_COMP_METHOD_DEBIT>-SPECIAL_STOCK_VAL
       IF_SOBKZ               = <FS_COMP_METHOD_DEBIT>-SPECIAL_STOCK
       IF_KALN1               = <FS_COMP_METHOD_DEBIT>-COSTING_NUM
*   IF_PPC_COMAT_NEW       = ' '
       IF_MANDT               = SY-MANDT
     IMPORTING
       EF_F_OBJNR             = LS_CPZP-F_OBJNR
*   EF_KALN1               =
     EXCEPTIONS
       NOT_FOUND              = 1
       NR_ERROR               = 2
       OTHERS                 = 3
              .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* complement CPZP-structure
    LS_CPZP-OBJNR = IV_PKOSA_OBJNR.
    LS_CPZP-GJPER = <FS_COMP_METHOD_DEBIT>-GJPER.
    LS_CPZP-MANDT = SY-MANDT.
    LS_CPZP-MEINH = <FS_COMP_METHOD_DEBIT>-UNIT_OF_MEASURE.
    LS_CPZP-UMREZ = 1.
    LS_CPZP-UMREN = 1.
    LS_CPZP-KZZPK = SPACE.
* check if this objects is already in itab gt_cpzp
    READ TABLE ET_CPZP WITH KEY
               OBJNR = LS_CPZP-OBJNR
               F_OBJNR = LS_CPZP-F_OBJNR
               GJPER = LS_CPZP-GJPER
               ASSIGNING <FS_CPZP>.
* if not, append it and fill GMPER and VARMN
    IF SY-SUBRC <> 0.
      LS_CPZP-GMPER = <FS_COMP_METHOD_DEBIT>-QUANTITY
*                      - <fs_comp_method_debit>-delta_quantity
                      .
      LS_CPZP-VARMN = <FS_COMP_METHOD_DEBIT>-DELTA_QUANTITY.
      APPEND LS_CPZP TO ET_CPZP.

    ELSE.
* if it is, add GMPER and VARMN
      <FS_CPZP>-GMPER = <FS_CPZP>-GMPER +
                                 <FS_COMP_METHOD_DEBIT>-QUANTITY
*                                 - <fs_comp_method_debit>-delta_quantity
                                 .
      <FS_CPZP>-VARMN = <FS_CPZP>-VARMN +
                                 <FS_COMP_METHOD_DEBIT>-DELTA_QUANTITY.

    ENDIF.
    CLEAR LS_CPZP.
  ENDLOOP.

* read line items from gt_act_method_debit (WIP debit activities) to get
* f_objnr via ENCODE-function module
  LOOP AT IT_ACT_METHOD_DEBIT ASSIGNING <FS_ACT_METHOD_DEBIT>.
    CALL FUNCTION 'KCR01_GET_COST_RESOURCE'
      EXPORTING
        I_RESOURCE_GUID           = <FS_ACT_METHOD_DEBIT>-RESSOURCE_GUID
        I_CONTROLLING_AREA        = IV_KOKRS
        I_KEY_DATE                = SY-DATLO
      IMPORTING
        E_OBJECT_NUMBER           = LS_CPZP-F_OBJNR
*       E_RESOURCE_TYPE           =
*       E_CONTROLLING_AREA        =
*       E_COST_CENTER             =
*       E_ACTIVITY_TYPE           =
*       E_BUSINESS_PROCESS        =
      EXCEPTIONS
        NOT_FOUND                 = 1
        CONTROLLING_AREA_MISMATCH = 2
        OTHERS                    = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* complement CPZP-structure
    LS_CPZP-OBJNR = IV_PKOSA_OBJNR.
    LS_CPZP-GJPER = <FS_ACT_METHOD_DEBIT>-GJPER.
    LS_CPZP-MANDT = SY-MANDT.
    LS_CPZP-MEINH = <FS_ACT_METHOD_DEBIT>-DURUNIT.
    LS_CPZP-UMREZ = 1.
    LS_CPZP-UMREN = 1.
    LS_CPZP-KZZPK = SPACE.
* variable quantities

*    IF NOT <fs_act_method_debit>-duration_var IS INITIAL OR
*       NOT <fs_act_method_debit>-delta_dur_var IS INITIAL.
* check if this objects is already in itab gt_cpzp
    READ TABLE ET_CPZP WITH KEY
               OBJNR = LS_CPZP-OBJNR
               F_OBJNR = LS_CPZP-F_OBJNR
               GJPER = LS_CPZP-GJPER
               ZAEHL = LV_CON_KEY_VAR
               ASSIGNING <FS_CPZP>.
* if not, append it and fill GMPER and VARMN
    IF SY-SUBRC <> 0.
* activities variable
      LS_CPZP-GMPER = <FS_ACT_METHOD_DEBIT>-DURATION_VAR
*                        - <fs_act_method_debit>-delta_dur_var
                      .
      LS_CPZP-VARMN = <FS_ACT_METHOD_DEBIT>-DELTA_DUR_VAR.
      LS_CPZP-ZAEHL = LV_CON_KEY_VAR.

      APPEND LS_CPZP TO ET_CPZP.
    ELSE.
* if it is, add GMPER and VARMN
      <FS_CPZP>-GMPER = <FS_CPZP>-GMPER +
                        <FS_ACT_METHOD_DEBIT>-DURATION_VAR
*                          - <fs_act_method_debit>-delta_dur_var
                        .


      <FS_CPZP>-VARMN = <FS_CPZP>-VARMN +
                        <FS_ACT_METHOD_DEBIT>-DELTA_DUR_VAR.

* fix quantities
    ENDIF.
*    ENDIF.
*    IF NOT <fs_act_method_debit>-duration_fix IS INITIAL OR
*       NOT <fs_act_method_debit>-delta_dur_fix IS INITIAL.
* check if this objects is already in itab gt_cpzp
*      READ TABLE gt_cpzp WITH KEY
*                 objnr = ls_cpzp-objnr
*                 f_objnr = ls_cpzp-f_objnr
*                 gjper = ls_cpzp-gjper
*                 zaehl = lv_con_key_fix
*                 ASSIGNING <fs_cpzp>.
** if not, append it and fill GMPER and VARMN
*      IF sy-subrc <> 0.
** activities variable
*        ls_cpzp-gmper = <fs_act_method_debit>-duration_fix
**                        - <fs_act_method_debit>-delta_dur_fix
*                        .
*        ls_cpzp-varmn = <fs_act_method_debit>-delta_dur_fix.
*        ls_cpzp-zaehl = lv_con_key_fix.
*        APPEND ls_cpzp TO gt_cpzp.
*      ELSE.
** if it is, add GMPER and VARMN
*        <fs_cpzp>-gmper = <fs_cpzp>-gmper +
*                          <fs_act_method_debit>-duration_fix
**                          - <fs_act_method_debit>-delta_dur_fix
*                          .
*        <fs_cpzp>-varmn = <fs_cpzp>-varmn +
*                          <fs_act_method_debit>-delta_dur_fix.
*
*      ENDIF.
*    ENDIF.
    CLEAR LS_CPZP.
  ENDLOOP.

  SORT ET_CPZP BY GJPER F_OBJNR.
ENDFORM.                    " CPZP_BUILD
*&---------------------------------------------------------------------*
*&      Form  fill_gmsum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_GMSUM USING IT_COMP_METHOD_CREDIT TYPE TT_COMP_METHOD
                      IT_ACT_METHOD_CREDIT TYPE TT_ACT_METHOD
                      IV_PKOSA_OBJNR TYPE OBJNR
                      IV_KOKRS TYPE KOKRS
             CHANGING CT_CPZP TYPE ZSAPBF_TT_CPZP.

  DATA: LS_CPZP TYPE CPZP,
        LS_SCRAP TYPE PPE_ASSEMBLY_SCRAP,
        LS_TMP_QUANT TYPE PPC_CONFQUANT,
        LV_CON_KEY_VAR TYPE COUNT_ZP VALUE '00000000',
        LV_CON_KEY_FIX TYPE COUNT_ZP VALUE '00000001'.

* components

* read line items from gt_comp_method_credit (WIP debit components) to *
* get f_objnr via ENCODE-function module
  LOOP AT IT_COMP_METHOD_CREDIT ASSIGNING <FS_COMP_METHOD_CREDIT>.

****Start; --- "Temporary Source Code by James Sung Kon Kim 2011.02.22
    IF <FS_COMP_METHOD_CREDIT>-BATCH IS NOT INITIAL.
      CLEAR <FS_COMP_METHOD_CREDIT>-COSTING_NUM.
    ENDIF.
****End; --- "Temporary Source Code by James Sung Kon Kim 2011.02.22
    CALL FUNCTION 'QRP_APO_COMP_OBJNR_ENCODE'
    EXPORTING
      IF_MATNR               = <FS_COMP_METHOD_CREDIT>-MAT_NUMBER
      IF_WERKS               = <FS_COMP_METHOD_CREDIT>-PLANT
**** "Temporary Blocking by James Sung Kon Kim 2011.02.22
****      if_bwtar               = <fs_comp_method_credit>-batch
      IF_VBELN               = <FS_COMP_METHOD_CREDIT>-SALES_DOC
      IF_POSNR               = <FS_COMP_METHOD_CREDIT>-SALES_DOC_ITEM
*       IF_PSPNR               = ' '
      IF_KZBWS               = <FS_COMP_METHOD_CREDIT>-SPECIAL_STOCK_VAL
      IF_SOBKZ               = <FS_COMP_METHOD_CREDIT>-SPECIAL_STOCK
      IF_KALN1               = <FS_COMP_METHOD_CREDIT>-COSTING_NUM
*   IF_PPC_COMAT_NEW       = ' '
      IF_MANDT               = SY-MANDT
    IMPORTING
      EF_F_OBJNR             = LS_CPZP-F_OBJNR
*   EF_KALN1               =
    EXCEPTIONS
      NOT_FOUND              = 1
      NR_ERROR               = 2
      OTHERS                 = 3
             .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* complement CPZP-structure
    LS_CPZP-OBJNR = IV_PKOSA_OBJNR.
    LS_CPZP-GJPER = <FS_COMP_METHOD_CREDIT>-GJPER.
    LS_CPZP-MANDT = SY-MANDT.
    LS_CPZP-UMREZ = 1.
    LS_CPZP-UMREN = 1.
    LS_CPZP-KZZPK = SPACE.
* check if this objects is already in itab gt_cpzp
    READ TABLE CT_CPZP WITH KEY
               OBJNR = LS_CPZP-OBJNR
               F_OBJNR = LS_CPZP-F_OBJNR
               GJPER = LS_CPZP-GJPER
               ASSIGNING <FS_CPZP>.
* if not, append it and fill GMPER and VARMN
    IF SY-SUBRC <> 0.
      LS_CPZP-GMSUM = <FS_COMP_METHOD_CREDIT>-QUANTITY.
*                     <fs_comp_method_credit>-delta_quantity.
      LS_CPZP-MEINH = <FS_COMP_METHOD_CREDIT>-UNIT_OF_MEASURE.
      APPEND LS_CPZP TO CT_CPZP.
      READ TABLE CT_CPZP WITH KEY
                 OBJNR = LS_CPZP-OBJNR
                 F_OBJNR = LS_CPZP-F_OBJNR
                 GJPER = LS_CPZP-GJPER
                 ASSIGNING <FS_CPZP>.
    ELSE.
* if it is, add GMSUM
      <FS_CPZP>-GMSUM = <FS_CPZP>-GMSUM +
                        <FS_COMP_METHOD_CREDIT>-QUANTITY.
*                       <fs_comp_method_debit>-delta_quantity.
    ENDIF.
    CLEAR LS_CPZP.

* get scrap factor
* -B RWU- add check read ippe
    IF NOT GV_READ_IPPE IS INITIAL.
      PERFORM GET_SCRAP_DATA IN PROGRAM SAPLPPC1WP USING
              <FS_COMP_METHOD_CREDIT>-ORDERID
              LS_SCRAP.
    ELSE.
      LS_SCRAP = 0.
    ENDIF.
* - E RWU- 20081208

* ... calculate scrap quantity, fill XMSUM
    LS_TMP_QUANT = <FS_COMP_METHOD_CREDIT>-QUANTITY -
                   <FS_COMP_METHOD_CREDIT>-DELTA_QUANTITY.
    IF LS_SCRAP < 100.
      <FS_CPZP>-XMSUM = LS_TMP_QUANT /
                        ( 1 - ( LS_SCRAP / 100 ) ) -
                        LS_TMP_QUANT.
    ELSE.
      <FS_CPZP>-XMSUM = LS_TMP_QUANT.
    ENDIF.
    CLEAR: LS_TMP_QUANT,
           LS_SCRAP.

  ENDLOOP.

* read line items from gt_act_method_credit (WIP debit activities) to get
* f_objnr via ENCODE-function module
  LOOP AT IT_ACT_METHOD_CREDIT ASSIGNING <FS_ACT_METHOD_CREDIT>.
    CALL FUNCTION 'KCR01_GET_COST_RESOURCE'
      EXPORTING
        I_RESOURCE_GUID           = <FS_ACT_METHOD_CREDIT>-RESSOURCE_GUID
        I_CONTROLLING_AREA        = IV_KOKRS
        I_KEY_DATE                = SY-DATLO
      IMPORTING
        E_OBJECT_NUMBER           = LS_CPZP-F_OBJNR
*       E_RESOURCE_TYPE           =
*       E_CONTROLLING_AREA        =
*       E_COST_CENTER             =
*       E_ACTIVITY_TYPE           =
*       E_BUSINESS_PROCESS        =
      EXCEPTIONS
        NOT_FOUND                 = 1
        CONTROLLING_AREA_MISMATCH = 2
        OTHERS                    = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* complement CPZP-structure
    LS_CPZP-OBJNR = IV_PKOSA_OBJNR.
    LS_CPZP-GJPER = <FS_ACT_METHOD_CREDIT>-GJPER.
    LS_CPZP-MANDT = SY-MANDT.
    LS_CPZP-UMREZ = 1.
    LS_CPZP-UMREN = 1.
    LS_CPZP-KZZPK = SPACE.
*   ls_cpzp-mat_number = <fs_act_method_debit>-ressource_guid.

* variable quantities

    IF NOT <FS_ACT_METHOD_CREDIT>-DURATION_VAR IS INITIAL.
* check if this objects is already in itab gt_cpzp
      READ TABLE CT_CPZP WITH KEY
                 OBJNR = LS_CPZP-OBJNR
                 F_OBJNR = LS_CPZP-F_OBJNR
                 GJPER = LS_CPZP-GJPER
                 ZAEHL = LV_CON_KEY_VAR
                 ASSIGNING <FS_CPZP>.
* if not, append it and fill GMSUM
      IF SY-SUBRC <> 0.
* activities variable
        LS_CPZP-GMSUM = <FS_ACT_METHOD_CREDIT>-DURATION_VAR.
*                       <fs_act_method_credit>-delta_dur_var.
*        ls_cpzp-varmn = <fs_act_method_debit>-delta_dur_var.
        LS_CPZP-ZAEHL = LV_CON_KEY_VAR.
        LS_CPZP-MEINH = <FS_ACT_METHOD_CREDIT>-DURUNIT.
* get scrap factor
* -B RWU- add check read ippe
        IF NOT GV_READ_IPPE IS INITIAL.
          PERFORM GET_SCRAP_DATA IN PROGRAM SAPLPPC1WP USING
                  <FS_COMP_METHOD_CREDIT>-ORDERID
                  LS_SCRAP.
        ELSE.
          LS_SCRAP = 0.
        ENDIF.
* - E RWU- 20081208

* ... calculate scrap quantity, fill XMSUM
        LS_TMP_QUANT = <FS_ACT_METHOD_CREDIT>-DURATION_VAR -
                       <FS_ACT_METHOD_CREDIT>-DELTA_DUR_VAR.
        IF LS_SCRAP < 100.
          LS_CPZP-XMSUM = LS_TMP_QUANT /
                            ( 1 - ( LS_SCRAP / 100 ) ) -
                            LS_TMP_QUANT.
        ELSE.
          LS_CPZP-XMSUM = LS_TMP_QUANT.
        ENDIF.
        APPEND LS_CPZP TO CT_CPZP.
        CLEAR: LS_TMP_QUANT,
               LS_SCRAP.
      ELSE.
* if it is, add GMSUM
        <FS_CPZP>-GMSUM = <FS_CPZP>-GMSUM +
                          <FS_ACT_METHOD_CREDIT>-DURATION_VAR.

      ENDIF.
    ENDIF.
* fix quantities
    IF NOT <FS_ACT_METHOD_CREDIT>-DURATION_FIX IS INITIAL.
* check if this objects is already in itab gt_cpzp
      READ TABLE CT_CPZP WITH KEY
                 OBJNR = LS_CPZP-OBJNR
                 F_OBJNR = LS_CPZP-F_OBJNR
                 GJPER = LS_CPZP-GJPER
                 ZAEHL = LV_CON_KEY_FIX
                 ASSIGNING <FS_CPZP>.
* if not, append it and fill GMPER and VARMN
      IF SY-SUBRC <> 0.
* activities fix
        LS_CPZP-GMSUM = <FS_ACT_METHOD_CREDIT>-DURATION_FIX.
*                       <fs_act_method_credit>-delta_dur_fix.
        LS_CPZP-ZAEHL = LV_CON_KEY_FIX.
* get scrap factor
* -B RWU- add check read ippe
        IF NOT GV_READ_IPPE IS INITIAL.
          PERFORM GET_SCRAP_DATA IN PROGRAM SAPLPPC1WP USING
                  <FS_COMP_METHOD_CREDIT>-ORDERID
                  LS_SCRAP.
        ELSE.
          LS_SCRAP = 0.
        ENDIF.
* - E RWU- 20081208

* ... calculate scrap quantity, fill XMSUM
        LS_TMP_QUANT = <FS_ACT_METHOD_CREDIT>-DURATION_FIX -
                       <FS_ACT_METHOD_CREDIT>-DELTA_DUR_FIX.
        IF LS_SCRAP < 100.
          LS_CPZP-XMSUM = LS_TMP_QUANT /
                            ( 1 - ( LS_SCRAP / 100 ) ) -
                            LS_TMP_QUANT.
        ELSE.
          LS_CPZP-XMSUM = LS_TMP_QUANT.
        ENDIF.
        APPEND LS_CPZP TO CT_CPZP.
        CLEAR: LS_TMP_QUANT,
               LS_SCRAP.

      ELSE.
* if it is, add GMSUM
        <FS_CPZP>-GMSUM = <FS_CPZP>-GMSUM +
                          <FS_ACT_METHOD_CREDIT>-DURATION_FIX.

      ENDIF.
    ENDIF.
    CLEAR LS_CPZP.
  ENDLOOP.
ENDFORM.                    " fill_gmsum
*&---------------------------------------------------------------------*
*&      Form  compress_scrap_quant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPRESS_SCRAP_QUANT USING IT_PPC_HEAD TYPE PPC_T_HEAD
                       CHANGING ET_PPC_HEAD_SMALL TYPE TT_PPC_HEAD_SMALL .

  DATA: LS_PPC_HEAD_SMALL TYPE GT_PPC_HEAD_SMALL_TYP.

  REFRESH ET_PPC_HEAD_SMALL.
  LOOP AT IT_PPC_HEAD ASSIGNING <FS_PPC_HEAD>.
    IF <FS_PPC_HEAD>-FLG_SCRAP = CHARX.
      MOVE-CORRESPONDING <FS_PPC_HEAD> TO LS_PPC_HEAD_SMALL. "#EC ENHOK
      LS_PPC_HEAD_SMALL-GJPER(4) = <FS_PPC_HEAD>-POSTDATE(4).
      LS_PPC_HEAD_SMALL-GJPER+5(2) = <FS_PPC_HEAD>-POSTDATE+4(2).
      COLLECT LS_PPC_HEAD_SMALL INTO ET_PPC_HEAD_SMALL.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " compress_scrap_quant
*&---------------------------------------------------------------------*
*&      Form  determin_scrap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DETERMIN_SCRAP USING IT_PPC_HEAD_SMALL TYPE TT_PPC_HEAD_SMALL
                          IT_PERIODS TYPE TT_PERIOD
                 CHANGING ET_COMP_METHOD_SCRAP TYPE TT_COMP_METHOD
                          ET_ACT_METHOD_SCRAP TYPE TT_ACT_METHOD
                          .

  DATA: FTAB_MATERIAL TYPE STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,
        FTAB_ACTIVITY TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS.

  DATA: LT_ACT_SCRAP TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
        LT_COMP_SCRAP TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP.

  FIELD-SYMBOLS: <FS_COMP_SCRAP> TYPE GT_MAT_COMP_ORDERID_TYP,
                 <FS_ACT_SCRAP> TYPE GT_ACT_COMP_ORDERID_TYP,
                 <FS_PERIOD> TYPE TS_PERIOD.
*----------------------------------------------------------------------*
* table with components quantities (SCRAP)
  DATA: LT_COMP_SCRAP_FINAL TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,

* table with activities quantities  (SCRAP)
        LT_ACT_SCRAP_FINAL TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
*----------------------------------------------------------------------*
* table with activities quantities with ACT_OBJNR (KALNR) (SCRAP)
        LT_ACT_SCRAP_OBJNR TYPE STANDARD TABLE OF GT_ACT_COMP_OBJNR_TYP.

  LOOP AT IT_PERIODS ASSIGNING <FS_PERIOD>.
    LOOP AT IT_PPC_HEAD_SMALL ASSIGNING <FS_PPC_HEAD_SMALL> WHERE GJPER = <FS_PERIOD>-GJPER.
      IF NOT <FS_PPC_HEAD_SMALL>-REPPOINT IS INITIAL.
        CALL FUNCTION 'PPC1DM_COMP_ORD_PART_GET'
          EXPORTING
            IF_ORDERID             = <FS_PPC_HEAD_SMALL>-ORDERID
            IF_HEADQUANT           = <FS_PPC_HEAD_SMALL>-CONFQUANT
            IF_REPPOINT            = <FS_PPC_HEAD_SMALL>-REPPOINT
*           IF_BW_CALL             = ' '
          IMPORTING
            ET_MATERIAL_COMPONENTS = FTAB_MATERIAL
            ET_ACTIVITY_COMPONENTS = FTAB_ACTIVITY
          EXCEPTIONS
            GET_COMP_ERROR         = 1
            INPUT_ERROR            = 2
            NO_RP_FROM_BW          = 3
            OTHERS                 = 4.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSE.
* -B RWU- 2008-12-30 No need to update scrap fields for PPCVAR.
        CONTINUE.
      ENDIF.
      APPEND LINES OF FTAB_MATERIAL TO LT_COMP_SCRAP.
      APPEND LINES OF FTAB_ACTIVITY TO LT_ACT_SCRAP.

      LOOP AT LT_COMP_SCRAP ASSIGNING <FS_COMP_SCRAP>.
        <FS_COMP_SCRAP>-ORDERID = <FS_PPC_HEAD_SMALL>-ORDERID.
        <FS_COMP_SCRAP>-GJPER = <FS_PERIOD>-GJPER.

        <FS_COMP_SCRAP>-PPC_MAT-QUANTITY =
        <FS_COMP_SCRAP>-PPC_MAT-QUANTITY -
        <FS_COMP_SCRAP>-PPC_MAT-DELTA_QUANTITY.
      ENDLOOP.
      APPEND LINES OF LT_COMP_SCRAP TO LT_COMP_SCRAP_FINAL.
      REFRESH LT_COMP_SCRAP.

      LOOP AT LT_ACT_SCRAP ASSIGNING <FS_ACT_SCRAP>.
        <FS_ACT_SCRAP>-ORDERID = <FS_PPC_HEAD_SMALL>-ORDERID.
        <FS_ACT_SCRAP>-GJPER = <FS_PERIOD>-GJPER.

        <FS_ACT_SCRAP>-PPC_ACT-DURATION_VAR =
        <FS_ACT_SCRAP>-PPC_ACT-DURATION_VAR -
        <FS_ACT_SCRAP>-PPC_ACT-DELTA_DUR_VAR.
        <FS_ACT_SCRAP>-PPC_ACT-DURATION_FIX =
        <FS_ACT_SCRAP>-PPC_ACT-DURATION_FIX -
        <FS_ACT_SCRAP>-PPC_ACT-DELTA_DUR_FIX.
      ENDLOOP.
      APPEND LINES OF LT_ACT_SCRAP TO LT_ACT_SCRAP_FINAL.
      REFRESH LT_ACT_SCRAP.


    ENDLOOP.
  ENDLOOP.

  PERFORM GET_ACT_OBJNR USING LT_ACT_SCRAP_FINAL
                     CHANGING LT_ACT_SCRAP_OBJNR.

  PERFORM COMPRESS_WIP USING LT_ACT_SCRAP_OBJNR
                             LT_COMP_SCRAP_FINAL
                    CHANGING ET_ACT_METHOD_SCRAP
                             ET_COMP_METHOD_SCRAP
                             .

ENDFORM.                    " determin_scrap
*&---------------------------------------------------------------------*
*&      Form  fill_xmper
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_XMPER USING IT_COMP_METHOD_SCRAP TYPE TT_COMP_METHOD
                      IT_ACT_METHOD_SCRAP TYPE TT_ACT_METHOD
                      IV_PKOSA_OBJNR TYPE OBJNR
                      IV_KOKRS TYPE KOKRS
             CHANGING CT_CPZP TYPE ZSAPBF_TT_CPZP.

  DATA: LS_CPZP TYPE CPZP,
        LV_CON_KEY_VAR TYPE COUNT_ZP VALUE '00000000',
        LV_CON_KEY_FIX TYPE COUNT_ZP VALUE '00000001'.

* components

* read line items from gt_comp_method_scrapto get
* get f_objnr via ENCODE-function module
  LOOP AT IT_COMP_METHOD_SCRAP ASSIGNING <FS_COMP_METHOD_SCRAP>.
****Start; --- "Temporary Source Code by James Sung Kon Kim 2011.02.22
    IF <FS_COMP_METHOD_SCRAP>-BATCH IS NOT INITIAL.
      CLEAR <FS_COMP_METHOD_SCRAP>-COSTING_NUM.
    ENDIF.
****End; --- "Temporary Source Code by James Sung Kon Kim 2011.02.22
    CALL FUNCTION 'QRP_APO_COMP_OBJNR_ENCODE'
    EXPORTING
      IF_MATNR               = <FS_COMP_METHOD_SCRAP>-MAT_NUMBER
      IF_WERKS               = <FS_COMP_METHOD_SCRAP>-PLANT
**** "Temporary Blocking by James Sung Kon Kim 2011.02.22
****      if_bwtar               = <fs_comp_method_scrap>-batch
      IF_VBELN               = <FS_COMP_METHOD_SCRAP>-SALES_DOC
      IF_POSNR               = <FS_COMP_METHOD_SCRAP>-SALES_DOC_ITEM
*       IF_PSPNR               = ' '
      IF_KZBWS               = <FS_COMP_METHOD_SCRAP>-SPECIAL_STOCK_VAL
      IF_SOBKZ               = <FS_COMP_METHOD_SCRAP>-SPECIAL_STOCK
      IF_KALN1               = <FS_COMP_METHOD_SCRAP>-COSTING_NUM
*   IF_PPC_COMAT_NEW       = ' '
      IF_MANDT               = SY-MANDT
    IMPORTING
      EF_F_OBJNR             = LS_CPZP-F_OBJNR
*   EF_KALN1               =
    EXCEPTIONS
      NOT_FOUND              = 1
      NR_ERROR               = 2
      OTHERS                 = 3
             .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* complement CPZP-structure
    LS_CPZP-OBJNR = IV_PKOSA_OBJNR.
    LS_CPZP-GJPER = <FS_COMP_METHOD_SCRAP>-GJPER.
    LS_CPZP-MANDT = SY-MANDT.


* check if this objects is already in itab gt_cpzp
    READ TABLE CT_CPZP WITH KEY
               OBJNR = LS_CPZP-OBJNR
               F_OBJNR = LS_CPZP-F_OBJNR
               GJPER = LS_CPZP-GJPER
               ASSIGNING <FS_CPZP>.
* if not, append it and fill xmper
    IF SY-SUBRC <> 0.
      LS_CPZP-XMPER = <FS_COMP_METHOD_SCRAP>-QUANTITY.
*                     <fs_comp_method_scrap>-delta_quantity.
      APPEND LS_CPZP TO CT_CPZP.
    ELSE.
* if it is, add xmper
      <FS_CPZP>-XMPER = <FS_CPZP>-XMPER +
                        <FS_COMP_METHOD_SCRAP>-QUANTITY.
*                       <fs_comp_method_scrap>-delta_quantity.
    ENDIF.
    CLEAR LS_CPZP.
  ENDLOOP.

* read line items from gt_act_method_scrap to get
* f_objnr via ENCODE-function module
  LOOP AT IT_ACT_METHOD_SCRAP ASSIGNING <FS_ACT_METHOD_SCRAP>.
    CALL FUNCTION 'KCR01_GET_COST_RESOURCE'
      EXPORTING
        I_RESOURCE_GUID           = <FS_ACT_METHOD_SCRAP>-RESSOURCE_GUID
        I_CONTROLLING_AREA        = IV_KOKRS
        I_KEY_DATE                = SY-DATLO
      IMPORTING
        E_OBJECT_NUMBER           = LS_CPZP-F_OBJNR
      EXCEPTIONS
        NOT_FOUND                 = 1
        CONTROLLING_AREA_MISMATCH = 2
        OTHERS                    = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* complement CPZP-structure
    LS_CPZP-OBJNR = IV_PKOSA_OBJNR.
    LS_CPZP-GJPER = <FS_ACT_METHOD_SCRAP>-GJPER.
    LS_CPZP-MANDT = SY-MANDT.

* variable quantities

    IF NOT <FS_ACT_METHOD_SCRAP>-DURATION_VAR IS INITIAL.
* check if this objects is already in itab gt_cpzp
      READ TABLE CT_CPZP WITH KEY
                 OBJNR = LS_CPZP-OBJNR
                 F_OBJNR = LS_CPZP-F_OBJNR
                 GJPER = LS_CPZP-GJPER
                 ZAEHL = LV_CON_KEY_VAR
                 ASSIGNING <FS_CPZP>.
* if not, append it and fill XMPER
      IF SY-SUBRC <> 0.
* activities variable
        LS_CPZP-XMPER = <FS_ACT_METHOD_SCRAP>-DURATION_VAR.
*                       <fs_act_method_scrap>-delta_dur_var.
        LS_CPZP-ZAEHL = LV_CON_KEY_VAR.
        APPEND LS_CPZP TO CT_CPZP.
      ELSE.
* if it is, add XMPER
        <FS_CPZP>-XMPER = <FS_CPZP>-XMPER +
                          <FS_ACT_METHOD_SCRAP>-DURATION_VAR.
      ENDIF.
    ENDIF.
* fix quantities

    IF NOT <FS_ACT_METHOD_DEBIT>-DURATION_FIX IS INITIAL.
* check if this objects is already in itab gt_cpzp
      READ TABLE CT_CPZP WITH KEY
                 OBJNR = LS_CPZP-OBJNR
                 F_OBJNR = LS_CPZP-F_OBJNR
                 GJPER = LS_CPZP-GJPER
                 ZAEHL = LV_CON_KEY_FIX
                 ASSIGNING <FS_CPZP>.
* if not, append it and fill XMPER
      IF SY-SUBRC <> 0.
* activities variable
        LS_CPZP-XMPER = <FS_ACT_METHOD_SCRAP>-DURATION_FIX.
*                       <fs_act_method_scrap>-delta_dur_fix.
        LS_CPZP-ZAEHL = LV_CON_KEY_FIX.
        APPEND LS_CPZP TO CT_CPZP.
      ELSE.
* if it is, add XMPER
        <FS_CPZP>-XMPER = <FS_CPZP>-XMPER +
                          <FS_ACT_METHOD_SCRAP>-DURATION_FIX.
      ENDIF.
    ENDIF.
    CLEAR LS_CPZP.
  ENDLOOP.
ENDFORM.                    " fill_xmper
*&---------------------------------------------------------------------*
*&      Form  cpzp_sel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CPZP_SEL_LAST USING IV_GJPER_POST TYPE CO_GJPER " 1 line revoked by Sung-Kon James Kim 2010.09.15
                         "iv_gjper_curr TYPE co_gjper
                         "iv_gjper_prev TYPE co_gjper " 1 line commentated by Sung-Kon James Kim 2010.09.15
                         IV_PKOSA_OBJNR TYPE OBJNR
                CHANGING ET_CPZP_LAST TYPE ZSAPBF_TT_CPZP.

  DATA: "lv_gjper TYPE co_gjper,
        LV_GJPER_LAST  TYPE CO_GJPER,
        LV_GJPER_FIRST TYPE CO_GJPER. "1 line added by Sung-Kon James Kim2011/01/26

  DATA: LV_MAX_GJPER TYPE CO_GJPER. "1 line commentated by Sung-Kon James Kim2010.09.15

*  lv_gjper_last = iv_gjper_prev - 1. "consider from Jan to Dec "1 line commentated by Sung-Kon James Kim 2010.09.15
  LV_GJPER_LAST = IV_GJPER_POST - 1.  "consider from Jan to Dec "1 line added by Sung-Kon James Kim 2010.09.15

  IF LV_GJPER_LAST+4(3) = '000'.
    LV_GJPER_LAST+4(3) = '012'.
    LV_GJPER_LAST(4) = LV_GJPER_LAST(4) - 1.
  ENDIF.

*read CPZP of last period

*** Start; Replaced by Sung-Kon James Kim with New logic 2010.09.15
*  DO. " commentated by Sung-Kon James Kim2010.09.15

*    SELECT * FROM cpzp INTO TABLE et_cpzp_last
*            WHERE objnr = iv_pkosa_objnr
*              AND gjper = lv_gjper_last.

*    IF sy-subrc EQ 0 OR lv_gjper_last+4(3) = '001'.
*      EXIT.
*    ELSE.
*      lv_gjper_last = lv_gjper_last - 1.
*    ENDIF.
** -B RWU- Unit unify with CSSL 2008-11-18
*  PERFORM cssl_unit_unify_for_cpzp_tab CHANGING et_cpzp_last.
** -E RWU-
*
*  ENDDO. " commentated by Sung-Kon James Kim2010.09.15

  LV_GJPER_FIRST = LV_GJPER_LAST.           "1 line added by Sung-Kon James Kim 2011/01/26
  " the first period is 1 year earlier than the last period.
*  lv_gjper_first(4) = lv_gjper_last(4) - 1. "1 line added by Sung-Kon James Kim 2011/01/26
  "1 line Commentated by Sung-Kon James Kim 2011/03/07

  SELECT MAX( GJPER ) FROM CPZP INTO LV_MAX_GJPER
   WHERE OBJNR = IV_PKOSA_OBJNR
     AND GJPER <= LV_GJPER_LAST.

*  IF sy-subrc EQ 0 AND lv_max_gjper NE '0000000'. "1 line Commentated by Sung-Kon James Kim 2011/03/07
  IF SY-DBCNT NE 0. "1 line Added by Sung-Kon James Kim 2011/03/07
    SELECT * FROM CPZP INTO TABLE ET_CPZP_LAST
     WHERE OBJNR = IV_PKOSA_OBJNR
*       AND gjper = lv_max_gjper. "1 line commentated by Sung-Kon James Kim 2011/01/26
*       AND gjper BETWEEN lv_gjper_first AND lv_max_gjper. "1 line Added by Sung-Kon James Kim 2011/01/26
                                                           "1 line Commentated by Sung-Kon James Kim 2011/03/07
       AND GJPER = LV_MAX_GJPER. "1 line Replaced with upper 1 line by Sung-Kon James Kim 2011/03/07

    IF  SY-SUBRC EQ 0.
* -B RWU- Unit unify with CSSL 2008-11-18
      PERFORM CSSL_UNIT_UNIFY_FOR_CPZP_TAB CHANGING ET_CPZP_LAST.
* -E RWU-
    ENDIF.
  ENDIF.

*** End; Replaced by Sung-Kon James Kim with New logic 2010.09.15

*  SORT et_cpzp_last BY gjper f_objnr. "Commentated by Sung-Kon James Kim 2011/01/26
  "Sort will be done in the form "insert_istmn_new" again

ENDFORM.                    " cpzp_sel
**&---------------------------------------------------------------------*
**&      Form  insert_istmn
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM insert_istmn USING it_cpzp_last TYPE zsapbf_tt_cpzp
*               CHANGING ct_cpzp TYPE zsapbf_tt_cpzp.
*
*  DATA: lv_gjper TYPE co_gjper,
*        lv_gjper_current TYPE co_gjper.
*
*  FIELD-SYMBOLS: <fs_cpzp_last> TYPE cpzp.
*
*  lv_gjper_current = p_year * 1000 + p_month.
*
*  LOOP AT ct_cpzp ASSIGNING <fs_cpzp>.
*    IF <fs_cpzp>-gjper = lv_gjper_current.  "First correction period
*      lv_gjper = <fs_cpzp>-gjper - 1.
** for activity
*      IF <fs_cpzp>-f_objnr(2) = 'KL'.
*        READ TABLE it_cpzp_last ASSIGNING <fs_cpzp_last> WITH KEY gjper = lv_gjper
*                                                                  f_objnr = <fs_cpzp>-f_objnr
*                                                                  zaehl = <fs_cpzp>-zaehl.
*        IF sy-subrc = 0.
*          <fs_cpzp>-istmn = <fs_cpzp_last>-istmn - <fs_cpzp_last>-gmsum
*                           + <fs_cpzp>-gmper.
*        ELSE.
*          <fs_cpzp>-istmn = <fs_cpzp>-gmper.
*        ENDIF.
*
** for component
*      ELSEIF <fs_cpzp>-f_objnr(2) = 'MK'.
*        READ TABLE it_cpzp_last ASSIGNING <fs_cpzp_last> WITH KEY gjper = lv_gjper
*                                                                  f_objnr = <fs_cpzp>-f_objnr.
*        IF sy-subrc = 0.
*          <fs_cpzp>-istmn = <fs_cpzp_last>-istmn - <fs_cpzp_last>-gmsum
*                           + <fs_cpzp>-gmper.
*        ELSE.
*          <fs_cpzp>-istmn = <fs_cpzp>-gmper.
*        ENDIF.
*
*      ENDIF.
*    ELSE.                                   "Successor correction period
*      lv_gjper = <fs_cpzp>-gjper - 1.
*
** for activity
*      IF <fs_cpzp>-f_objnr(2) = 'KL'.
*        READ TABLE ct_cpzp ASSIGNING <fs_cpzp_last> WITH KEY gjper = lv_gjper
*                                                                  f_objnr = <fs_cpzp>-f_objnr
*                                                                  zaehl = <fs_cpzp>-zaehl.
*        IF sy-subrc = 0.
*          <fs_cpzp>-istmn = <fs_cpzp_last>-istmn - <fs_cpzp_last>-gmsum
*                           + <fs_cpzp>-gmper.
*        ELSE.
*          <fs_cpzp>-istmn = <fs_cpzp>-gmper.
*        ENDIF.
*
** for component
*      ELSEIF <fs_cpzp>-f_objnr(2) = 'MK'.
*        READ TABLE ct_cpzp ASSIGNING <fs_cpzp_last> WITH KEY gjper = lv_gjper
*                                                                  f_objnr = <fs_cpzp>-f_objnr.
*        IF sy-subrc = 0.
*          <fs_cpzp>-istmn = <fs_cpzp_last>-istmn - <fs_cpzp_last>-gmsum
*                           + <fs_cpzp>-gmper.
*        ELSE.
*          <fs_cpzp>-istmn = <fs_cpzp>-gmper.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " insert_istmn
*&---------------------------------------------------------------------*
*&      Form  insert_istmn_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_ISTMN_NEW USING IV_GJPER_POST TYPE CO_GJPER " 1 line revoked by Sung-Kon James Kim 2011/01/26
                            IV_GJPER_CURR TYPE CO_GJPER " 1 line revoked by Sung-Kon James Kim 2011/01/26
                            "iv_gjper_prev TYPE co_gjper " 1 Line Commentated //2010.09.15, Sung-Kon James Kim
                   CHANGING CT_CPZP_LAST TYPE ZSAPBF_TT_CPZP
                            CT_CPZP TYPE ZSAPBF_TT_CPZP.

  DATA: LV_GJPER TYPE CO_GJPER.
*        lv_gjper_current TYPE co_gjper.


  FIELD-SYMBOLS: <FS_CPZP_LAST> TYPE CPZP,
                 <FS_CPZP>      TYPE CPZP. " 1 Line Added //2011.03.07, Sung-Kon James Kim

***** Start; Added by Sung-Kon James Kim 2011.03.07
*****************************************************
  DATA: LS_CPZP_LAST TYPE CPZP,
        LS_CPZP      TYPE CPZP.

* At First, Populate the last cpzp into the post period's CPZP.
  IF CT_CPZP_LAST[] IS NOT INITIAL.
    CLEAR LS_CPZP_LAST.
    LOOP AT CT_CPZP_LAST ASSIGNING <FS_CPZP_LAST>.
      MOVE-CORRESPONDING <FS_CPZP_LAST> TO LS_CPZP_LAST.

      "According to the Standard Logic; only if neither istmn nor gmsum is 0. then perform period copy.
      IF LS_CPZP_LAST-ISTMN NE 0 OR LS_CPZP_LAST-GMSUM NE 0.
        READ TABLE CT_CPZP WITH KEY GJPER = IV_GJPER_POST
                                    OBJNR = LS_CPZP_LAST-OBJNR
                                    F_OBJNR = LS_CPZP_LAST-F_OBJNR
                                    ZAEHL = LS_CPZP_LAST-ZAEHL
                                    TRANSPORTING NO FIELDS.
        IF SY-SUBRC NE 0.
          LS_CPZP_LAST-GJPER = IV_GJPER_POST.
          LS_CPZP_LAST-ISTMN = 0.
          LS_CPZP_LAST-GMPER = 0.
          LS_CPZP_LAST-XMPER = 0.
          LS_CPZP_LAST-GMSUM = 0.
          LS_CPZP_LAST-XMSUM = 0.
          LS_CPZP_LAST-UMREZ = 1.
          LS_CPZP_LAST-UMREN = 1.
          LS_CPZP_LAST-VARMN = 0.

          IF LS_CPZP_LAST-F_OBJNR(2) = 'KL'.
            PERFORM CSSL_UNIT_UNIFY_FOR_LAST_CPZP USING LS_CPZP_LAST-GJPER(4)
                                                  CHANGING LS_CPZP_LAST.
          ENDIF.

          COLLECT LS_CPZP_LAST INTO CT_CPZP.
        ENDIF.
      ENDIF.

      CLEAR LS_CPZP_LAST.
    ENDLOOP.
  ENDIF.

* At Second, Populate the post period's CPZP into the current period's CPZP.
  IF IV_GJPER_POST NE IV_GJPER_CURR.

    READ TABLE CT_CPZP WITH KEY GJPER = IV_GJPER_CURR TRANSPORTING NO FIELDS.

    IF SY-SUBRC = 0. "If any current period's record exists.
      CLEAR LS_CPZP.
      LOOP AT CT_CPZP ASSIGNING <FS_CPZP> WHERE GJPER = IV_GJPER_POST.
        MOVE-CORRESPONDING <FS_CPZP> TO LS_CPZP.

        "According to the Standard Logic; only if neither istmn nor gmsum is 0. then perform period copy.
        IF LS_CPZP-ISTMN NE 0 OR LS_CPZP-GMSUM NE 0.
          READ TABLE CT_CPZP WITH KEY GJPER = IV_GJPER_CURR
                                      OBJNR = LS_CPZP-OBJNR
                                      F_OBJNR = LS_CPZP-F_OBJNR
                                      ZAEHL = LS_CPZP-ZAEHL
                                      TRANSPORTING NO FIELDS.
          IF SY-SUBRC NE 0.
            LS_CPZP-GJPER = IV_GJPER_CURR.
            LS_CPZP-ISTMN = 0.
            LS_CPZP-GMPER = 0.
            LS_CPZP-XMPER = 0.
            LS_CPZP-GMSUM = 0.
            LS_CPZP-XMSUM = 0.
            LS_CPZP-UMREZ = 1.
            LS_CPZP-UMREN = 1.
            LS_CPZP-VARMN = 0.

            IF LS_CPZP-F_OBJNR(2) = 'KL'.
              PERFORM CSSL_UNIT_UNIFY_FOR_LAST_CPZP USING LS_CPZP-GJPER(4)
                                                    CHANGING LS_CPZP.
            ENDIF.

            COLLECT LS_CPZP INTO CT_CPZP.
          ENDIF.
        ENDIF.

        CLEAR LS_CPZP.
      ENDLOOP.
    ENDIF.
  ENDIF.
*****************************************************
***** End; Added by Sung-Kon James Kim 2011.03.07

**** Start; Commentated by Sung-Kon James Kim 2011.03.07
*  SORT ct_cpzp_last BY objnr f_objnr ASCENDING gjper DESCENDING. "Descending : Very Important; Added by S.K. James Kim 2011/01/26
*  DELETE ADJACENT DUPLICATES FROM ct_cpzp_last COMPARING objnr f_objnr. "Very Important; Added by S.K. James Kim 2011/01/27
**** End; Commentated by Sung-Kon James Kim 2011.03.07

  SORT CT_CPZP BY OBJNR F_OBJNR ASCENDING GJPER ASCENDING. "Very Important; Added by Sung-Kon James Kim 2011/01/26

  LOOP AT CT_CPZP ASSIGNING <FS_CPZP>.
*    IF <fs_cpzp>-gjper = iv_gjper_prev.  "First correction period  " 1 Line Commentated //2010.09.15, Sung-Kon James Kim
*    lv_gjper = <fs_cpzp>-gjper - 1."after this, "000" should be checked. " 1 Line Commentated //2010.09.15, Sung-Kon James Kim

    IF <FS_CPZP>-GJPER = IV_GJPER_POST. "Added by Sung-Kon James Kim 2011/01/26

****** [1] Important Remark by Sung-Kon James Kim 2011/01/26 after intentional debuggings ************
* The last period before post period could be several months ago.
* (This is actual environment, the production record could not exist
*  during several months after first production record)
* Therefore, the deduction "one" from post period can result in the erroneous correction.
* Don't use bellow statements.
* lv_gjper = <fs_cpzp>-gjper - 1.
* READ TABLE it_cpzp_last ASSIGNING <fs_cpzp_last> WITH KEY gjper = lv_gjper
******************************************************************************************************

****** [2] Important Remark by Sung-Kon James Kim 2011/01/26 after intentional debuggings ************
* The posting period can be equal to the current period.
* 1) the user can use current period as posting period.
* 2) the user can use one month before current period as posting period.
* Therefore, In the IF condition, the posting period should be checked firstly.
* If the CPZP records are not for the posting period, then the CPZP records are for the current period.
******************************************************************************************************

* for activity
      IF <FS_CPZP>-F_OBJNR(2) = 'KL'.
        READ TABLE CT_CPZP_LAST ASSIGNING <FS_CPZP_LAST> WITH KEY "gjper = lv_gjper
                                                              OBJNR = <FS_CPZP>-OBJNR
                                                              F_OBJNR = <FS_CPZP>-F_OBJNR
                                                              ZAEHL = <FS_CPZP>-ZAEHL.
        IF SY-SUBRC = 0.
          " The base of unit (activity) in the table "CSSL" could be different, between two fiscal years.
          PERFORM CSSL_UNIT_UNIFY_FOR_LAST_CPZP USING <FS_CPZP>-GJPER(4) "Important !!!; Using post period
                                                CHANGING <FS_CPZP_LAST>. "2 Lines; Added by James Sung-Kon Kim 2011/02/01

          <FS_CPZP>-ISTMN = <FS_CPZP_LAST>-ISTMN - <FS_CPZP_LAST>-GMSUM
                           + <FS_CPZP>-GMPER.
        ELSE.
          <FS_CPZP>-ISTMN = <FS_CPZP>-GMPER.
        ENDIF.

* for component
      ELSEIF <FS_CPZP>-F_OBJNR(2) = 'MK'.
        READ TABLE CT_CPZP_LAST ASSIGNING <FS_CPZP_LAST> WITH KEY "gjper = lv_gjper
                                                                  OBJNR = <FS_CPZP>-OBJNR
                                                                  F_OBJNR = <FS_CPZP>-F_OBJNR.
        IF SY-SUBRC = 0.
          <FS_CPZP>-ISTMN = <FS_CPZP_LAST>-ISTMN - <FS_CPZP_LAST>-GMSUM
                           + <FS_CPZP>-GMPER.
        ELSE.
          <FS_CPZP>-ISTMN = <FS_CPZP>-GMPER.
        ENDIF.

      ENDIF.

**** Start ; Added by Sung-Kon James Kim 2011/01/26
    ELSEIF <FS_CPZP>-GJPER = IV_GJPER_CURR.

****** [3] Important Remark by Sung-Kon James Kim 2011/01/26 after intentional debuggings ************
* If the posting period is one month before current period,
* then, the CPZP records should be calculated based on the new records of the posting period not
*       the old records of the last period before posting period.
******************************************************************************************************

      LV_GJPER = <FS_CPZP>-GJPER - 1. "This means posting period.
      IF LV_GJPER+4(3) = '000'.
        LV_GJPER+4(3) = '012'.
        LV_GJPER(4) = LV_GJPER(4) - 1.
      ENDIF.

* for activity
      IF <FS_CPZP>-F_OBJNR(2) = 'KL'.
        "-- first; from the posting period's new CPZP
        READ TABLE CT_CPZP ASSIGNING <FS_CPZP_LAST> WITH KEY GJPER = LV_GJPER
                                                             OBJNR = <FS_CPZP>-OBJNR
                                                             F_OBJNR = <FS_CPZP>-F_OBJNR
                                                             ZAEHL = <FS_CPZP>-ZAEHL.
        IF SY-SUBRC = 0.
          " The base of unit (activity) in the table "CSSL" could be different, between two fiscal years.
          PERFORM CSSL_UNIT_UNIFY_FOR_LAST_CPZP USING <FS_CPZP>-GJPER(4) "Important !!!; Using post period
                                                CHANGING <FS_CPZP_LAST>. "2 Lines; Added by James Sung-Kon Kim 2011/02/01

          <FS_CPZP>-ISTMN = <FS_CPZP_LAST>-ISTMN - <FS_CPZP_LAST>-GMSUM
                           + <FS_CPZP>-GMPER.
        ELSE.
          "-- Second; from the newest(latest) period CPZP of the old CPZP.
          READ TABLE CT_CPZP_LAST ASSIGNING <FS_CPZP_LAST> WITH KEY "gjper = lv_gjper
                                                               OBJNR = <FS_CPZP>-OBJNR
                                                               F_OBJNR = <FS_CPZP>-F_OBJNR
                                                               ZAEHL = <FS_CPZP>-ZAEHL.
          IF SY-SUBRC = 0.
            " The base of unit (activity) in the table "CSSL" could be different, between two fiscal years.
            PERFORM CSSL_UNIT_UNIFY_FOR_LAST_CPZP USING <FS_CPZP>-GJPER(4) "Important !!!; Using post period
                                                  CHANGING <FS_CPZP_LAST>. "2 Lines; Added by James Sung-Kon Kim 2011/02/01

            <FS_CPZP>-ISTMN = <FS_CPZP_LAST>-ISTMN - <FS_CPZP_LAST>-GMSUM
                             + <FS_CPZP>-GMPER.
          ELSE.
            <FS_CPZP>-ISTMN = <FS_CPZP>-GMPER.
          ENDIF.
        ENDIF.

* for component
      ELSEIF <FS_CPZP>-F_OBJNR(2) = 'MK'.
        "-- first; from the posting period's new CPZP
        READ TABLE CT_CPZP ASSIGNING <FS_CPZP_LAST> WITH KEY GJPER = LV_GJPER
                                                             OBJNR = <FS_CPZP>-OBJNR
                                                             F_OBJNR = <FS_CPZP>-F_OBJNR.
        IF SY-SUBRC = 0.
          <FS_CPZP>-ISTMN = <FS_CPZP_LAST>-ISTMN - <FS_CPZP_LAST>-GMSUM
                           + <FS_CPZP>-GMPER.
        ELSE.
          "-- Second; from the newest(latest) period CPZP of the old CPZP.
          READ TABLE CT_CPZP_LAST ASSIGNING <FS_CPZP_LAST> WITH KEY "gjper = lv_gjper
                                                               OBJNR = <FS_CPZP>-OBJNR
                                                               F_OBJNR = <FS_CPZP>-F_OBJNR.
          IF SY-SUBRC = 0.
            <FS_CPZP>-ISTMN = <FS_CPZP_LAST>-ISTMN - <FS_CPZP_LAST>-GMSUM
                             + <FS_CPZP>-GMPER.
          ELSE.
            <FS_CPZP>-ISTMN = <FS_CPZP>-GMPER.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.
**** End ; Added by Sung-Kon James Kim 2011/01/26

**** Following Logic is meaningless ; Commentated by Sung-Kon James Kim 2010/09/15
****    ELSE.                                   "Successor correction period
*****      lv_gjper = <fs_cpzp>-gjper - 1.
****
***** for activity
****      IF <fs_cpzp>-f_objnr(2) = 'KL'.
****        READ TABLE ct_cpzp ASSIGNING <fs_cpzp_last> WITH KEY gjper = iv_gjper_prev "lv_gjper
****                                                             objnr = <fs_cpzp>-objnr
****                                                             f_objnr = <fs_cpzp>-f_objnr
****                                                             zaehl = <fs_cpzp>-zaehl.
****        IF sy-subrc = 0.
****          <fs_cpzp>-istmn = <fs_cpzp_last>-istmn - <fs_cpzp_last>-gmsum
****                           + <fs_cpzp>-gmper.
****        ELSE.
****          <fs_cpzp>-istmn = <fs_cpzp>-gmper.
****        ENDIF.
****
***** for component
****      ELSEIF <fs_cpzp>-f_objnr(2) = 'MK'.
****        READ TABLE ct_cpzp ASSIGNING <fs_cpzp_last> WITH KEY gjper = iv_gjper_prev "lv_gjper
****                                                             objnr = <fs_cpzp>-objnr
****                                                             f_objnr = <fs_cpzp>-f_objnr.
****        IF sy-subrc = 0.
****          <fs_cpzp>-istmn = <fs_cpzp_last>-istmn - <fs_cpzp_last>-gmsum
****                           + <fs_cpzp>-gmper.
****        ELSE.
****          <fs_cpzp>-istmn = <fs_cpzp>-gmper.
****        ENDIF.
****
****      ENDIF.
****    ENDIF.
  ENDLOOP.

*    SORT ct_cpzp BY f_objnr. "Commentated by Sung-Kon James Kim 2011/01/26

ENDFORM.                    " insert_istmn
*&---------------------------------------------------------------------*
*&      Form  CPZP_SEL_BACKUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_YEAR  text
*      -->P_LV_GJPER_POST  text
*      -->P_LV_GJPER_CURR  text
*      -->P_LV_GJPER_PREV  text
*      -->P_LV_PKOSA_OBJNR  text
*      <--P_LT_CPZP_BACKUP  text
*----------------------------------------------------------------------*
FORM CPZP_SEL_BACKUP USING IV_GJPER_POST TYPE CO_GJPER
                           IV_PKOSA_OBJNR TYPE OBJNR
                  CHANGING ET_CPZP_BACKUP TYPE ZSAPBF_TT_CPZP.

* backup CPZP from correction periods
  SELECT * FROM CPZP INTO TABLE ET_CPZP_BACKUP
          WHERE OBJNR = IV_PKOSA_OBJNR
*            AND gjper >= iv_gjper_post " Commentated by James Sung-Kon Kim 2011/03/07
            AND GJPER = IV_GJPER_POST   " Added by James Sung-Kon Kim 2011/03/07
                .
* -B RWU- Unit unify with CSSL 2008-11-18
  PERFORM CSSL_UNIT_UNIFY_FOR_CPZP_TAB CHANGING ET_CPZP_BACKUP.
* -E RWU-
ENDFORM.                    " CPZP_SEL_BACKUP
**&---------------------------------------------------------------------*
**&      Form  write_cpzp
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM write_cpzp USING iv_pkosa_objnr TYPE objnr
*                      iv_gjper_post TYPE co_gjper
*                      iv_gjper_curr TYPE co_gjper
*                      iv_gjper_prev TYPE co_gjper
*                      it_cpzp TYPE zsapbf_tt_cpzp
*             CHANGING ev_update_error TYPE char1
*                      et_cpzp_backup TYPE zsapbf_tt_cpzp.
*
*  DATA:lt_cpzp_current TYPE zsapbf_tt_cpzp,
*       lt_cpzp_previous TYPE zsapbf_tt_cpzp,
*
*       lt_cpzp_backup_current TYPE zsapbf_tt_cpzp,
*       lt_cpzp_backup_previous TYPE zsapbf_tt_cpzp,
*
*       ls_cpzp TYPE cpzp.
*
*  DATA: lv_objnr TYPE objnr.
*
** Sorr it_CPZP
*  SORT it_cpzp BY f_objnr.
*
** Separate it to current and previous
*  LOOP AT it_cpzp INTO ls_cpzp.
*    IF ls_cpzp-gjper = iv_gjper_curr.
*      APPEND ls_cpzp TO lt_cpzp_current.
*    ELSE.
*      APPEND ls_cpzp TO lt_cpzp_previous.
*    ENDIF.
*  ENDLOOP.
*
*
** Lock the P.C.C
**&---------------------------------------------------------------------*
**&      Form  write_cpzp
**&---------------------------------------------------------------------*
*  PERFORM enqueue USING iv_pkosa_objnr
*               CHANGING ev_update_error.
*  CHECK ev_update_error IS INITIAL.
*
** backup CPZP from correction periods
*  SELECT * FROM cpzp INTO TABLE et_cpzp_backup
*          WHERE objnr = iv_pkosa_objnr
*            AND gjper >= iv_gjper_post
*                .
** -B RWU- Unit unify with CSSL 2008-11-18
*  PERFORM cssl_unit_unify_for_cpzp_tab CHANGING et_cpzp_backup.
** -E RWU-
** Separate it to current and previous
*  LOOP AT et_cpzp_backup INTO ls_cpzp.
*    IF ls_cpzp-gjper = iv_gjper_curr.
*      APPEND ls_cpzp TO lt_cpzp_backup_current.
*    ELSE.
*      APPEND ls_cpzp TO lt_cpzp_backup_previous.
*    ENDIF.
*  ENDLOOP.
*
** Copy GT_CPZP_TEMP into CPZPTEMP
*  IF et_cpzp_backup IS NOT INITIAL.
*    DELETE cpzptemp FROM TABLE et_cpzp_backup.
*    INSERT cpzptemp FROM TABLE et_cpzp_backup.
*  ENDIF.
** Both previous periods and current periods(ISTMN) should be updated
*
** step1 For previous period correct all records
*  IF lt_cpzp_backup_previous IS NOT INITIAL.
*    DELETE cpzp FROM TABLE lt_cpzp_backup_previous.
*  ENDIF.
*
*  IF lt_cpzp_previous IS NOT INITIAL.
*    INSERT cpzp FROM TABLE lt_cpzp_previous.
*  ENDIF.
**    IF NOT sy-subrc IS INITIAL.
**      ev_update_error = 'U'.
**      ROLLBACK WORK.
**      EXIT.
**    ENDIF.
*
** step2 For current period
*** Option1 only update ISTMN fields
**  IF sy-uname NE 'SAPCD06'.
*  IF lt_cpzp_current IS NOT INITIAL.
*    LOOP AT lt_cpzp_current INTO ls_cpzp.
*      UPDATE cpzp SET istmn = ls_cpzp-istmn
*                        WHERE objnr EQ ls_cpzp-objnr
*                          AND f_objnr EQ ls_cpzp-f_objnr
*                          AND gjper EQ ls_cpzp-gjper
*                          AND zaehl EQ ls_cpzp-zaehl.
*      IF NOT sy-subrc IS INITIAL.
*        INSERT into cpzp values ls_cpzp.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
**  ELSE.
*** Option2 full update
**    DELETE cpzp FROM TABLE lt_cpzp_backup_current.
**    INSERT cpzp FROM TABLE lt_cpzp_current.
**  ENDIF.
*
*  IF NOT sy-subrc IS INITIAL.
*    ev_update_error = 'U'.
*    ROLLBACK WORK.
*    EXIT.
*  ELSE.
*    COMMIT WORK.
*  ENDIF.
** Unlock the P.C.C
*  PERFORM dequeue USING lv_objnr.
*
*ENDFORM.                    " write_cpzp
*&---------------------------------------------------------------------*
*&      Form  ALV_CPZP_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CPZP  text
*----------------------------------------------------------------------*
FORM ALV_CPZP_OUTPUT USING IV_ERROR TYPE FLAG
                           IT_CPZP TYPE ZSAPBF_TT_CPZP
                           IV_NUM_PCC TYPE  INT1
                  CHANGING CT_CPZP_BACKUP TYPE ZSAPBF_TT_CPZP
                           .
* ALV variables

  DATA: LS_VARIANT  TYPE DISVARIANT,
        LV_ALV_STRUCTURE TYPE DD02L-TABNAME VALUE 'ZSAPBF_CPZP_DISPLAY',
        LT_FIELDCAT TYPE STANDARD TABLE OF SLIS_FIELDCAT_ALV,
        LS_LAYOUT TYPE  SLIS_LAYOUT_ALV.
*        lv_records TYPE i.

*  DATA: lt_cpzp_display TYPE STANDARD TABLE OF zsapbf_cpzp_display.

  DATA LV_DISPLAY_LINES TYPE I.

  LS_LAYOUT-COLWIDTH_OPTIMIZE = CHARX.

*  IF sy-uname NE 'SAPCD06'.
*    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*      EXPORTING
*        i_structure_name = 'CPZP'
*        i_inclname       = sy-cprog
*      CHANGING
*        ct_fieldcat      = lt_fieldcat
*      EXCEPTIONS
*        OTHERS           = 1.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    ls_variant-report   = sy-repid.
*    ls_variant-username = sy-uname.
*
*    SORT it_cpzp BY gjper f_objnr.
*    DESCRIBE TABLE it_cpzp LINES lv_records.
*
**  gv_werks = iv_werks.
**  gv_matnr = iv_matnr.
**  gv_verid = iv_verid.
*    gv_pkosa_objnr = iv_pkosa_objnr.
*    gv_records = lv_records.
*
*
*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        i_grid_title             = space
*        it_fieldcat              = lt_fieldcat
*        i_callback_program       = 'ZSAPBF_CPZP_CORRECTION_REPORT' "sy-cprog
**      i_callback_top_of_page   = 'TOP_OF_PAGE'
**        i_callback_pf_status_set = 'SET_PF_STATUS'
*        is_variant               = ls_variant
*        i_save                   = 'A'
*        is_layout                = ls_layout
*      TABLES
*        t_outtab                 = it_cpzp
*      EXCEPTIONS
*        OTHERS                   = 1.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ELSE.
* fill the ALV display table and filter out the consistency items

  IF IV_NUM_PCC > 0.
    GV_NUM_PCC = IV_NUM_PCC.
  ELSE.
    GV_NUM_PCC = 1.
  ENDIF.

  PERFORM FILL_DATA USING IV_ERROR
                          IT_CPZP
                 CHANGING CT_CPZP_BACKUP
                          GT_CPZP_DISPLAY_NUMPCC.

* -B RWU- 2008-12-10
*  CHECK gt_cpzp_display_numpcc IS NOT INITIAL.
  IF GT_CPZP_DISPLAY_NUMPCC IS INITIAL.
    IF IV_ERROR IS INITIAL.
      MESSAGE I408.
      EXIT.
    ELSE.
      MESSAGE I409 WITH P_AUFNR. "Add 'p_aufnr' James Kim 2011/02/21
      EXIT.
    ENDIF.

  ENDIF.
* -E RWU-

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = LV_ALV_STRUCTURE
      I_INCLNAME       = SY-CPROG
    CHANGING
      CT_FIELDCAT      = LT_FIELDCAT
    EXCEPTIONS
      OTHERS           = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM FIELDCAT_CHANGE CHANGING LT_FIELDCAT.


  LS_VARIANT-REPORT   = SY-REPID.
  LS_VARIANT-USERNAME = SY-UNAME.

*
*    gv_pkosa_objnr = iv_pkosa_objnr.
*    gv_records = lv_records.

  LV_DISPLAY_LINES = LINES( GT_CPZP_DISPLAY ).

  LV_DISPLAY_LINES = LV_DISPLAY_LINES / 1000.

  IF LV_DISPLAY_LINES GT 30.
    LV_DISPLAY_LINES = 30.
  ENDIF.

  WAIT UP TO LV_DISPLAY_LINES SECONDS.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_GRID_TITLE             = SPACE
      IT_FIELDCAT              = LT_FIELDCAT
      I_CALLBACK_PROGRAM       = 'ZSAPBF_CPZP_CORRECTION_REPORT' "sy-cprog
*     i_callback_top_of_page   = 'TOP_OF_PAGE'
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'SET_COMMAND'
      IS_VARIANT               = LS_VARIANT
      I_SAVE                   = 'A'
      IS_LAYOUT                = LS_LAYOUT
    TABLES
      T_OUTTAB                 = GT_CPZP_DISPLAY_NUMPCC
    EXCEPTIONS
      OTHERS                   = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  ENDIF.
ENDFORM.                    " ALV_CPZP_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->RT_EXTAB   text
**      -->ENDFORM    text
**----------------------------------------------------------------------*
*FORM set_pf_status USING rt_extab TYPE slis_t_extab.
**FIELD-SYMBOLS: <ls_extab> TYPE LINE OF rt_extab.
***delete FROM rt_extab WHERE
*  SET PF-STATUS 'ZSTANDARD_FULLSCREEN' EXCLUDING rt_extab.
*
*
*ENDFORM.                    "set_pf_status
*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       Callback-Routine für HTML-Header                              *
*       call back for HTML header                                     *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.                                           "#EC CALLED
  STATICS HEADER_READY.
  DATA: LT_LISTHEADER TYPE STANDARD TABLE OF SLIS_LISTHEADER,
        LS_LISTHEADER TYPE SLIS_LISTHEADER.

  IF HEADER_READY IS INITIAL.

    LS_LISTHEADER-TYP = 'H'.
    LS_LISTHEADER-INFO = TEXT-016.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.

    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-022.
    LS_LISTHEADER-INFO = GV_PKOSA_OBJNR.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
*
*    ls_listheader-typ = 'S'.
*    ls_listheader-key = text-018.
*    ls_listheader-info = gv_werks.
*    APPEND ls_listheader TO lt_listheader.
*
*    ls_listheader-typ = 'S'.
*    ls_listheader-key = text-017.
*    ls_listheader-info = gv_matnr.
*    APPEND ls_listheader TO lt_listheader.
*
*    ls_listheader-typ = 'S'.
*    ls_listheader-key = text-019.
*    ls_listheader-info = gv_verid.
*    APPEND ls_listheader TO lt_listheader.

    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-020.
    LS_LISTHEADER-INFO = GV_MONTH.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.

    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-021.
    LS_LISTHEADER-INFO = GV_YEAR.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.

    LS_LISTHEADER-KEY = TEXT-043.
    LS_LISTHEADER-INFO = GV_RECORDS.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
    HEADER_READY = 'X'.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_LISTHEADER.

ENDFORM.                                          "top_of_page
*&---------------------------------------------------------------------*
*&      Form  GET_PERIOD_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_YEAR  text
*      -->P_P_MONTH  text
*      <--P_GT_PERIOD  text
*----------------------------------------------------------------------*
FORM GET_PERIOD_LIST USING IV_YEAR TYPE GJAHR
                           IV_MONTH TYPE MONAT
                           IV_WERKS TYPE WERKS_D
                           IV_KOKRS TYPE KOKRS
                  CHANGING ET_PERIODS TYPE TT_PERIOD
                           EV_FIRST_DAY TYPE SY-DATUM
                           EV_LAST_DAY TYPE SY-DATUM. "Added by James Kim 2011/01/25

  DATA: LV_GJPER_POST TYPE CO_GJPER,
        LV_GJPER_CURR TYPE CO_GJPER,
        LV_GJPER_PREV TYPE CO_GJPER.

  DATA: LT_PERS TYPE KALC_T_PERIODS,
        LT_PERS_CURR TYPE KALC_T_PERIODS,
        LT_PERS_POST TYPE KALC_T_PERIODS, "Added by James Kim 2011/01/25
        LT_PERS_PREV TYPE KALC_T_PERIODS,
        LS_PERIOD TYPE TS_PERIOD.

  DATA: LV_LINES TYPE I. "Added by James Kim 2011/01/25

*  DATA: "lv_buper_small TYPE poper, "Commentated by Sung-Kon James Kim 2010.09.15
*        lv_buper_big TYPE poper.    "Commentated by Sung-Kon James Kim 2010.09.15

  DATA: LV_BUPER_INPUT TYPE POPER. "Added by Sung-Kon James Kim 2010.09.15

  DATA: LV_YEAR TYPE GJAHR.
*  DATA: lv_month TYPE monat.

  DATA LV_PERAB TYPE POPER.
  DATA LV_PERBI TYPE POPER.

  FIELD-SYMBOLS: <FS_PERS> TYPE PERIODS,
                 <FS_PERIOD> TYPE TS_PERIOD. "Added by Sung-Kon James Kim 2011/01/25

***** Important Remark; by Sung Kon James Kime 2011/01/25 ********************************************
* For the period; posting period, current period, previous period.
*-----------------------------------------------------------------------------------------------------
* In case; calling program is 'ZSAPBF_CPZP_CORRECTION_REPORT'
*  Always, the posting period, current period and previous period will be treated as the same as the user's INPUT PERIOD.
*  The period list will contain only one record which stands for the user's input period
*  The GMPER, GMSUM and XMSUM will be calculated for the only one period(posting period)
*
*-----------------------------------------------------------------------------------------------------
* In case; calling program is 'ZSAPBF_CPZP_CORRECTION_MASSIVE'
*
*  IF THE USER USE the current period as the posting period,
*    then the period list will contain only one month (Because posting period and current period are the same).
*    in this case, The GMPER, GMSUM and XMSUM will be calculated for the one period (posting period)
*    therefore, checking the differnce between old CPZP and new CPZP will be performed using posting period.
*    if the difference exists, then the new CPZP will be updated for the posting period.
*
*  IF THE USER USE the previous period as the posting period,
*    then the period list will contain the two months (posting period and current period).
*    in this case, The GMPER, GMSUM and XMSUM will be calculated for the two periods(posting period and current period)
*    But, checking the differnce between old CPZP and new CPZP will be performed only using posting period.
*    Anyway, if the difference exists, then the new CPZP will be updated for the both periods.
*
*  IF THE USER USE the PAST Period before the previous period as the posting period, (THIS CHANGE IS VERY IMPORTANT)
*    Always, the posting period, current period and previous period will be treated as the same as the user's INPUT PERIOD.
*    The period list will contain only one record which stands for the user's input period
*    The GMPER, GMSUM and XMSUM will be calculated for the only one period(posting period)
*
******************************************************************************************************
* #### Always, the user's input period will be treated as the posting period.
* #### In result; the previous period is not meaningful any longer after 2011/01/25.
* #### the ev_first_day and ev_last_day will be so important in all area of this program.
* #### this changes are done to avoid confusing the source code regarding the posting period/current period/previous period.
*******************************************************************************************************

* Start; Commentated by Sung-Kon James Kim 2011/01/26
*    lv_gjper_prev = iv_year * 1000 + iv_month - 1.

*    IF lv_gjper_prev+4(3) = '000'.
*      lv_gjper_prev+4(3) = '012'.
*      lv_gjper_prev(4) = lv_gjper_prev(4) - 1.
*    ENDIF.
* Start; Commentated by Sung-Kon James Kim 2011/01/26

  PERFORM PERIODS_GET IN PROGRAM SAPLQRPRP USING IV_WERKS
                                                 SY-DATLO
                                        CHANGING LV_GJPER_POST
                                                 LV_GJPER_CURR
                                                 LV_GJPER_PREV.

**** Important; In Order To allow for the past period before previous period.  2011/01/25
**** Start; Added by James Kim 2011/01/25
  LV_GJPER_POST = IV_YEAR * 1000 + IV_MONTH.

  IF LV_GJPER_POST NE LV_GJPER_CURR AND
    LV_GJPER_POST NE LV_GJPER_PREV.

    LV_GJPER_POST = IV_YEAR * 1000 + IV_MONTH.
    LV_GJPER_CURR = IV_YEAR * 1000 + IV_MONTH.
    LV_GJPER_PREV = IV_YEAR * 1000 + IV_MONTH.
  ENDIF.
**** End; Added by James Kim 2011/01/25

* Start; Commentated by Sung-Kon James Kim 2011/01/26
*  IF lv_gjper_prev+4(3) = '012'.
** Cross fiscal year
*  lv_year = lv_gjper_prev+0(4).
*  lv_perab = lv_gjper_prev+4(3).
*  lv_perbi = lv_gjper_prev+4(3).
*
*  CALL FUNCTION 'K_PERIODS_GET_FOR_GJAHR'
*    EXPORTING
*      i_kokrs          = iv_kokrs
*      i_gjahr          = lv_year
*      i_perab          = lv_perab
*      i_perbi          = lv_perbi
*    TABLES
*      t_periods        = lt_pers_prev
*    EXCEPTIONS
*      kokrs_not_found  = 1
*      gjahr_not_found  = 2
*      error_in_periods = 3
*      OTHERS           = 4.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    APPEND LINES OF lt_pers_prev TO lt_pers.
*  ENDIF.
*
* End; Commentated by Sung-Kon James Kim 2011/01/26

  LV_YEAR = LV_GJPER_POST+0(4).
  LV_PERAB = LV_GJPER_POST+4(3).
  LV_PERBI = LV_GJPER_POST+4(3).

  CALL FUNCTION 'K_PERIODS_GET_FOR_GJAHR'
    EXPORTING
      I_KOKRS          = IV_KOKRS
      I_GJAHR          = LV_YEAR
      I_PERAB          = LV_PERAB
      I_PERBI          = LV_PERBI
    TABLES
      T_PERIODS        = LT_PERS_POST
    EXCEPTIONS
      KOKRS_NOT_FOUND  = 1
      GJAHR_NOT_FOUND  = 2
      ERROR_IN_PERIODS = 3
      OTHERS           = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    APPEND LINES OF LT_PERS_POST TO LT_PERS.
  ENDIF.

  LV_YEAR = LV_GJPER_CURR+0(4).
  LV_PERAB = LV_GJPER_CURR+4(3).
  LV_PERBI = LV_GJPER_CURR+4(3).

  CALL FUNCTION 'K_PERIODS_GET_FOR_GJAHR'
    EXPORTING
      I_KOKRS          = IV_KOKRS
      I_GJAHR          = LV_YEAR
      I_PERAB          = LV_PERAB
      I_PERBI          = LV_PERBI
    TABLES
      T_PERIODS        = LT_PERS_CURR
    EXCEPTIONS
      KOKRS_NOT_FOUND  = 1
      GJAHR_NOT_FOUND  = 2
      ERROR_IN_PERIODS = 3
      OTHERS           = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    APPEND LINES OF LT_PERS_CURR TO LT_PERS.
  ENDIF.

* Start; Commentated by Sung-Kon James Kim 2011/01/26
*  ELSE.
** In same fiscal year
*    lv_perab = iv_month.
*    lv_perbi = lv_gjper_curr+4(3).
*    CALL FUNCTION 'K_PERIODS_GET_FOR_GJAHR'
*      EXPORTING
*        i_kokrs          = iv_kokrs
*        i_gjahr          = iv_year
*        i_perab          = lv_perab
*        i_perbi          = lv_perbi
*      TABLES
*        t_periods        = lt_pers
*      EXCEPTIONS
*        kokrs_not_found  = 1
*        gjahr_not_found  = 2
*        error_in_periods = 3
*        OTHERS           = 4.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    lv_buper_input = iv_month.  " Added by Sung-Kon James Kim 2010.09.15
*    DELETE lt_pers WHERE buper <> lv_buper_input. " Added by Sung-Kon James Kim 2010.09.15
*
**    lv_buper_small = iv_month. "Commentated by Sung-Kon James Kim 2010.09.15
**    lv_buper_big = lv_gjper_curr+4(3). "Commentated by Sung-Kon James Kim 2010.09.15
**
**    DELETE lt_pers WHERE buper < lv_buper_small "Commentated by Sung-Kon James Kim 2010.09.15
**                      OR buper > lv_buper_big. "Commentated by Sung-Kon James Kim 2010.09.15
*  ENDIF.
* End; Commentated by Sung-Kon James Kim 2011/01/26

  LOOP AT LT_PERS ASSIGNING <FS_PERS>.
    LS_PERIOD-GJPER = <FS_PERS>-DATAB+0(4) * 1000 + <FS_PERS>-BUPER.
    LS_PERIOD-STARTDAY = <FS_PERS>-DATAB.
    LS_PERIOD-ENDDAY = <FS_PERS>-DATBI.
    APPEND LS_PERIOD TO ET_PERIODS.
    CLEAR LS_PERIOD.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM ET_PERIODS. "Added by Sung-Kon James Kim 2011/01/26

*  READ TABLE lt_pers ASSIGNING <fs_pers> INDEX 1. "Commentated by Sung-Kon James Kim 2011/01/25
*  ev_first_day = <fs_pers>-datab.                 "Commentated by Sung-Kon James Kim 2011/01/25

  SORT ET_PERIODS ASCENDING BY GJPER.                  "Added by Sung-Kon James Kim 2011/01/25
  READ TABLE ET_PERIODS ASSIGNING <FS_PERIOD> INDEX 1. "Added by Sung-Kon James Kim 2011/01/25
  EV_FIRST_DAY = <FS_PERIOD>-STARTDAY.                 "Added by Sung-Kon James Kim 2011/01/25

** Start; Added by Sung-Kon James Kim 2011/01/25
  DESCRIBE TABLE ET_PERIODS LINES LV_LINES.
  READ TABLE ET_PERIODS ASSIGNING <FS_PERIOD> INDEX LV_LINES.
  EV_LAST_DAY = <FS_PERIOD>-ENDDAY.
** End; Added by Sung-Kon James Kim 2011/01/25

ENDFORM.                    " GET_PERIOD_LIST
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CPZP  text
*      -->P_IT_CPZP_BACKUP  text
*      <--P_LT_CPZP_DISPLAY  text
*----------------------------------------------------------------------*
FORM FILL_DATA USING IV_ERROR TYPE FLAG
                     IT_CPZP TYPE ZSAPBF_TT_CPZP
            CHANGING CT_CPZP_BACKUP TYPE ZSAPBF_TT_CPZP
                     ET_CPZP_DISPLAY TYPE ZSAPBF_TT_CPZP_DISPLAY.

  FIELD-SYMBOLS: <FS_CPZP> TYPE CPZP,
                 <FS_CPZP_BACKUP> TYPE CPZP,
                 <FS_CPZP_DISPLAY> TYPE ZSAPBF_CPZP_DISPLAY,
                 <FS_PPC_HEAD> TYPE TS_PCC_HEAD.

  DATA: LS_CPZP_DISPLAY TYPE ZSAPBF_CPZP_DISPLAY.
  DATA: LV_TABIX TYPE SY-TABIX.
  DATA: LT_PCC_HEAD TYPE TT_PCC_HEAD.

  DATA LT_PCC_OBJ TYPE GTY_PCC_OBJ_T.
  DATA LS_PCC_OBJ TYPE GTY_PCC_OBJ_S.

******* Start; Commentated by Kim Jong Oh 2011/03/07
*  DATA lv_tabix_fr TYPE sytabix.
*  DATA lv_tabix_to TYPE sytabix.
*  DATA lv_flg_no_var TYPE xfeld.
******* End; Commentated by Kim Jong Oh 2011/03/07

******* Start; Added by Kim Jong Oh 2011/03/07
  DATA LT_CPZP_TEMP TYPE ZSAPBF_TT_CPZP .
  DATA LT_CPZP_BACKUP_TEMP TYPE ZSAPBF_TT_CPZP .
  DATA LT_CPZP_BACKUP_TMP TYPE ZSAPBF_TT_CPZP .
  DATA LS_CPZP_BACKUP_TMP TYPE CPZP .
  DATA LS_CPZP_TEMP TYPE CPZP .
******* End; Added by Kim Jong Oh 2011/03/07

* base on new items
  LOOP AT IT_CPZP ASSIGNING <FS_CPZP>.
    MOVE-CORRESPONDING <FS_CPZP> TO LS_CPZP_DISPLAY.        "#EC ENHOK
    LS_CPZP_DISPLAY-AUFNR = LS_CPZP_DISPLAY-OBJNR+2.

    READ TABLE CT_CPZP_BACKUP ASSIGNING <FS_CPZP_BACKUP>
                               WITH KEY OBJNR = <FS_CPZP>-OBJNR
                                      F_OBJNR = <FS_CPZP>-F_OBJNR
                                        GJPER = <FS_CPZP>-GJPER
                                        ZAEHL = <FS_CPZP>-ZAEHL.

    IF SY-SUBRC EQ 0.
      LV_TABIX = SY-TABIX.

      LS_CPZP_DISPLAY-ISTMN_CPZP = <FS_CPZP_BACKUP>-ISTMN.
      LS_CPZP_DISPLAY-GMPER_CPZP = <FS_CPZP_BACKUP>-GMPER.
      LS_CPZP_DISPLAY-XMPER_CPZP = <FS_CPZP_BACKUP>-XMPER.
      LS_CPZP_DISPLAY-GMSUM_CPZP = <FS_CPZP_BACKUP>-GMSUM.
      LS_CPZP_DISPLAY-XMSUM_CPZP = <FS_CPZP_BACKUP>-XMSUM.
      LS_CPZP_DISPLAY-VARMN_CPZP = <FS_CPZP_BACKUP>-VARMN.

      LS_CPZP_DISPLAY-ISTMN_VAR = LS_CPZP_DISPLAY-ISTMN - LS_CPZP_DISPLAY-ISTMN_CPZP .
      LS_CPZP_DISPLAY-GMPER_VAR = LS_CPZP_DISPLAY-GMPER - LS_CPZP_DISPLAY-GMPER_CPZP .
      LS_CPZP_DISPLAY-XMPER_VAR = LS_CPZP_DISPLAY-XMPER - LS_CPZP_DISPLAY-XMPER_CPZP .
      LS_CPZP_DISPLAY-GMSUM_VAR = LS_CPZP_DISPLAY-GMSUM - LS_CPZP_DISPLAY-GMSUM_CPZP .
      LS_CPZP_DISPLAY-XMSUM_VAR = LS_CPZP_DISPLAY-XMSUM - LS_CPZP_DISPLAY-XMSUM_CPZP .
      LS_CPZP_DISPLAY-VARMN_VAR = LS_CPZP_DISPLAY-VARMN - LS_CPZP_DISPLAY-VARMN_CPZP .
      APPEND LS_CPZP_DISPLAY TO ET_CPZP_DISPLAY.

******* Start; Added by Kim Jong Oh 2011/03/07
      APPEND <FS_CPZP_BACKUP> TO LT_CPZP_BACKUP_TMP.
******* End; Added by Kim Jong Oh 2011/03/07
      DELETE CT_CPZP_BACKUP INDEX LV_TABIX.
    ELSE.
*      ls_cpzp_display-istmn_cpzp = <fs_cpzp_backup>-istmn.
*      ls_cpzp_display-gmper_cpzp = <fs_cpzp_backup>-gmper.
*      ls_cpzp_display-xmper_cpzp = <fs_cpzp_backup>-xmper.
*      ls_cpzp_display-gmsum_cpzp = <fs_cpzp_backup>-gmsum.
*      ls_cpzp_display-xmsum_cpzp = <fs_cpzp_backup>-xmsum.
*      ls_cpzp_display-varmn_cpzp = <fs_cpzp_backup>-varmn.

      LS_CPZP_DISPLAY-ISTMN_VAR = LS_CPZP_DISPLAY-ISTMN - LS_CPZP_DISPLAY-ISTMN_CPZP .
      LS_CPZP_DISPLAY-GMPER_VAR = LS_CPZP_DISPLAY-GMPER - LS_CPZP_DISPLAY-GMPER_CPZP .
      LS_CPZP_DISPLAY-XMPER_VAR = LS_CPZP_DISPLAY-XMPER - LS_CPZP_DISPLAY-XMPER_CPZP .
      LS_CPZP_DISPLAY-GMSUM_VAR = LS_CPZP_DISPLAY-GMSUM - LS_CPZP_DISPLAY-GMSUM_CPZP .
      LS_CPZP_DISPLAY-XMSUM_VAR = LS_CPZP_DISPLAY-XMSUM - LS_CPZP_DISPLAY-XMSUM_CPZP .
      LS_CPZP_DISPLAY-VARMN_VAR = LS_CPZP_DISPLAY-VARMN - LS_CPZP_DISPLAY-VARMN_CPZP .
      APPEND LS_CPZP_DISPLAY TO ET_CPZP_DISPLAY.
    ENDIF.
    CLEAR LS_CPZP_DISPLAY.
* -B RWU- Display varaint PCC 20081103
    LS_PCC_OBJ-OBJNR = <FS_CPZP>-OBJNR.
    APPEND LS_PCC_OBJ TO LT_PCC_OBJ.
    CLEAR LS_PCC_OBJ.
* -E RWU-
  ENDLOOP.

* Append the left CPZP items
  IF CT_CPZP_BACKUP IS NOT INITIAL.
    LOOP AT CT_CPZP_BACKUP ASSIGNING <FS_CPZP_BACKUP>.
* -B RWU- 2008-12-15
      LS_CPZP_DISPLAY-AUFNR = <FS_CPZP_BACKUP>-OBJNR+2.
* -E RWU-
      LS_CPZP_DISPLAY-OBJNR = <FS_CPZP_BACKUP>-OBJNR.
      LS_CPZP_DISPLAY-F_OBJNR = <FS_CPZP_BACKUP>-F_OBJNR.
      LS_CPZP_DISPLAY-GJPER = <FS_CPZP_BACKUP>-GJPER.
      LS_CPZP_DISPLAY-ZAEHL = <FS_CPZP_BACKUP>-ZAEHL.
      LS_CPZP_DISPLAY-PLNKN = <FS_CPZP_BACKUP>-PLNKN.
      LS_CPZP_DISPLAY-VORNE = <FS_CPZP_BACKUP>-VORNE.
      LS_CPZP_DISPLAY-PLNFL = <FS_CPZP_BACKUP>-PLNFL.
**ISTMN
**GMPER
**XMPER
**GMSUM
**XMSUM
**VARMN
      LS_CPZP_DISPLAY-MEINH = <FS_CPZP_BACKUP>-MEINH.
      LS_CPZP_DISPLAY-AUFAK = <FS_CPZP_BACKUP>-AUFAK.
      LS_CPZP_DISPLAY-UMREZ = <FS_CPZP_BACKUP>-UMREZ.
      LS_CPZP_DISPLAY-UMREN = <FS_CPZP_BACKUP>-UMREN.
      LS_CPZP_DISPLAY-PLNME = <FS_CPZP_BACKUP>-PLNME.
      LS_CPZP_DISPLAY-KZZPK = <FS_CPZP_BACKUP>-KZZPK.
      LS_CPZP_DISPLAY-REWQTY = <FS_CPZP_BACKUP>-REWQTY.

      LS_CPZP_DISPLAY-ISTMN_CPZP = <FS_CPZP_BACKUP>-ISTMN.
      LS_CPZP_DISPLAY-GMPER_CPZP = <FS_CPZP_BACKUP>-GMPER.
      LS_CPZP_DISPLAY-XMPER_CPZP = <FS_CPZP_BACKUP>-XMPER.
      LS_CPZP_DISPLAY-GMSUM_CPZP = <FS_CPZP_BACKUP>-GMSUM.
      LS_CPZP_DISPLAY-XMSUM_CPZP = <FS_CPZP_BACKUP>-XMSUM.
      LS_CPZP_DISPLAY-VARMN_CPZP = <FS_CPZP_BACKUP>-VARMN.

      LS_CPZP_DISPLAY-ISTMN_VAR = LS_CPZP_DISPLAY-ISTMN_CPZP - LS_CPZP_DISPLAY-ISTMN.
      LS_CPZP_DISPLAY-GMPER_VAR = LS_CPZP_DISPLAY-GMPER_CPZP - LS_CPZP_DISPLAY-GMPER.
      LS_CPZP_DISPLAY-XMPER_VAR = LS_CPZP_DISPLAY-XMPER_CPZP - LS_CPZP_DISPLAY-XMPER.
      LS_CPZP_DISPLAY-GMSUM_VAR = LS_CPZP_DISPLAY-GMSUM_CPZP - LS_CPZP_DISPLAY-GMSUM.
      LS_CPZP_DISPLAY-XMSUM_VAR = LS_CPZP_DISPLAY-XMSUM_CPZP - LS_CPZP_DISPLAY-XMSUM.
      LS_CPZP_DISPLAY-VARMN_VAR = LS_CPZP_DISPLAY-VARMN_CPZP - LS_CPZP_DISPLAY-VARMN.
      APPEND LS_CPZP_DISPLAY TO ET_CPZP_DISPLAY.
      CLEAR LS_CPZP_DISPLAY.
* -B RWU- Display varaint PCC 20081103
      LS_PCC_OBJ-OBJNR = <FS_CPZP_BACKUP>-OBJNR.
      APPEND LS_PCC_OBJ TO LT_PCC_OBJ.
      CLEAR LS_PCC_OBJ.
******* Start; Added by Kim Jong Oh 2011/03/07
      APPEND <FS_CPZP_BACKUP> TO LT_CPZP_BACKUP_TMP.
******* End; Added by Kim Jong Oh 2011/03/07
* -E RWU-
    ENDLOOP.
  ENDIF.

******* Start; Added by James Sung-Kon Kim 2011/04/03
  CLEAR: CT_CPZP_BACKUP, CT_CPZP_BACKUP[].
******* End; Added by James Sung-Kon Kim 2011/04/03

  SORT ET_CPZP_DISPLAY BY OBJNR GJPER F_OBJNR.
  SORT LT_PCC_OBJ BY OBJNR.
  DELETE ADJACENT DUPLICATES FROM LT_PCC_OBJ COMPARING OBJNR.

  IF IV_ERROR = 'X'.
* -B RWU- Display varaint PCC

****************************************************
******* Start; Added by Kim Jong Oh 2011/03/07

    LOOP AT LT_PCC_OBJ INTO LS_PCC_OBJ.

      CLEAR : LT_CPZP_TEMP[] , LT_CPZP_BACKUP_TEMP[] , LS_CPZP_TEMP.

      LOOP AT IT_CPZP INTO LS_CPZP_TEMP WHERE OBJNR EQ LS_PCC_OBJ-OBJNR .
        APPEND LS_CPZP_TEMP TO LT_CPZP_TEMP .
      ENDLOOP .

      CLEAR LS_CPZP_TEMP.

      LOOP AT LT_CPZP_BACKUP_TMP INTO LS_CPZP_TEMP WHERE OBJNR EQ LS_PCC_OBJ-OBJNR .
        APPEND LS_CPZP_TEMP TO LT_CPZP_BACKUP_TEMP .
      ENDLOOP .

      SORT LT_CPZP_TEMP BY OBJNR F_OBJNR GJPER ZAEHL.
      SORT LT_CPZP_BACKUP_TEMP BY OBJNR F_OBJNR GJPER ZAEHL.

      IF LT_CPZP_TEMP[] EQ LT_CPZP_BACKUP_TEMP[] . "Not Display because there is no difference.
        DELETE ET_CPZP_DISPLAY WHERE OBJNR = LS_PCC_OBJ-OBJNR .
        DELETE TABLE LT_PCC_OBJ FROM LS_PCC_OBJ.
      ENDIF .

      DELETE LT_CPZP_TEMP WHERE OBJNR = LS_PCC_OBJ-OBJNR .
      DELETE LT_CPZP_BACKUP_TMP WHERE OBJNR = LS_PCC_OBJ-OBJNR .
      DELETE LT_CPZP_BACKUP_TEMP WHERE OBJNR = LS_PCC_OBJ-OBJNR .

******* End; Added by Kim Jong Oh 2011/03/07
****************************************************

******* Start; Commentated by Kim Jong Oh 2011/03/07
*      CLEAR: lv_tabix_fr, lv_tabix_to, ls_cpzp_display.
*      lv_flg_no_var = charx.
*
*      READ TABLE et_cpzp_display TRANSPORTING NO FIELDS
*                                 WITH KEY objnr = ls_pcc_obj-objnr
*                                 BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        lv_tabix_fr = sy-tabix.
*      ENDIF.
*
*      LOOP AT et_cpzp_display INTO ls_cpzp_display FROM lv_tabix_fr.
*        IF ls_cpzp_display-objnr = ls_pcc_obj-objnr.
*          lv_tabix_to = sy-tabix.
*        ELSE.
*          EXIT.
*        ENDIF.
*
*        IF ls_cpzp_display-istmn_var NE 0 OR
*           ls_cpzp_display-gmper_var NE 0 OR
*           ls_cpzp_display-xmper_var NE 0 OR
*           ls_cpzp_display-gmsum_var NE 0 OR
*           ls_cpzp_display-xmsum_var NE 0 OR
*           ls_cpzp_display-varmn_var NE 0.
*          CLEAR lv_flg_no_var.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*
*      IF NOT lv_flg_no_var IS INITIAL.
*        DELETE et_cpzp_display  FROM lv_tabix_fr TO lv_tabix_to.
*        DELETE TABLE lt_pcc_obj FROM ls_pcc_obj.
*      ENDIF.
******* End; Commentated by Kim Jong Oh 2011/03/07

    ENDLOOP.

*    DELETE et_cpzp_display WHERE istmn_var = 0
*                             AND gmper_var = 0
*                             AND xmper_var = 0
*                             AND gmsum_var = 0
*                             AND xmsum_var = 0
*                             AND varmn_var = 0
*                             .
* -E RWU-
  ENDIF.

  IF ET_CPZP_DISPLAY IS NOT INITIAL.
    SELECT AUFNR
           PRWRK
           PMATN
           VERID
      FROM CKMLMV013
      INTO TABLE LT_PCC_HEAD
      FOR ALL ENTRIES IN ET_CPZP_DISPLAY
     WHERE AUFNR = ET_CPZP_DISPLAY-AUFNR.

    LOOP AT ET_CPZP_DISPLAY ASSIGNING <FS_CPZP_DISPLAY>.
      READ TABLE LT_PCC_HEAD ASSIGNING <FS_PPC_HEAD> WITH KEY AUFNR = <FS_CPZP_DISPLAY>-AUFNR.
      IF SY-SUBRC = 0.
        <FS_CPZP_DISPLAY>-MATNR = <FS_PPC_HEAD>-PMATN.
        <FS_CPZP_DISPLAY>-WERKS = <FS_PPC_HEAD>-PRWRK.
        <FS_CPZP_DISPLAY>-VERID = <FS_PPC_HEAD>-VERID.
      ENDIF.
    ENDLOOP.

    SORT ET_CPZP_DISPLAY BY OBJNR GJPER F_OBJNR.
  ENDIF.
  IF SY-BATCH IS INITIAL.
    GT_CPZP_DISPLAY = ET_CPZP_DISPLAY.
    GT_PCC_OBJ      = LT_PCC_OBJ.
    CLEAR ET_CPZP_DISPLAY.
    PERFORM FILL_DISPLAY_CPZP USING 'F'
                              CHANGING ET_CPZP_DISPLAY.
  ENDIF.
*  gt_cpzp_display_numpcc = et_cpzp_display.
ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM FIELDCAT_CHANGE CHANGING CT_FIELDCAT TYPE TT_SLIS_FIELDCAT_ALV.
  FIELD-SYMBOLS: <FS_FIELDCAT> TYPE SLIS_FIELDCAT_ALV.

  LOOP AT CT_FIELDCAT ASSIGNING <FS_FIELDCAT>.
    CASE <FS_FIELDCAT>-FIELDNAME.
      WHEN 'ISTMN_CPZP'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-050.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-050.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-050.
      WHEN 'GMPER_CPZP'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-051.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-051.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-051.
      WHEN 'XMPER_CPZP'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-052.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-052.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-052.
      WHEN 'GMSUM_CPZP'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-053.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-053.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-053.
      WHEN 'XMSUM_CPZP'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-054.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-054.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-054.
      WHEN 'VARMN_CPZP'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-055.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-055.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-055.

      WHEN 'ISTMN_VAR'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-056.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-056.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-056.
      WHEN 'GMPER_VAR'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-057.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-057.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-057.
      WHEN 'XMPER_VAR'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-058.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-058.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-058.
      WHEN 'GMSUM_VAR'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-059.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-059.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-059.
      WHEN 'XMSUM_VAR'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-060.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-060.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-060.
      WHEN 'VARMN_VAR'.
        <FS_FIELDCAT>-SELTEXT_L = TEXT-061.
        <FS_FIELDCAT>-SELTEXT_M = TEXT-061.
        <FS_FIELDCAT>-SELTEXT_S = TEXT-061.

    ENDCASE.

  ENDLOOP.
ENDFORM.                    " FIELDCAT_CHANGE
**&---------------------------------------------------------------------*
**&      Form  GET_PERIODS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_IV_AUFNR  text
**      <--P_LV_GJPER_POST  text
**      <--P_LV_GJPER_CURR  text
**      <--P_LV_GJPER_PREV  text
**----------------------------------------------------------------------*
*FORM get_periods USING iv_aufnr TYPE aufnr
*                       iv_first_day TYPE sy-datum
*              CHANGING ev_gjper_post TYPE co_gjper
*                       ev_gjper_curr TYPE co_gjper
*                       ev_gjper_prev TYPE co_gjper.
*
*  DATA: lv_werks TYPE werks_d.
*
*  CHECK iv_aufnr IS NOT INITIAL.
*
*  SELECT SINGLE prwrk
*           FROM ckmlmv013
*           INTO lv_werks
*          WHERE aufnr = iv_aufnr.
*
*  PERFORM periods_get IN PROGRAM saplqrprp USING lv_werks
*                                                 iv_first_day
*                                        CHANGING ev_gjper_post
*                                                 ev_gjper_curr
*                                                 ev_gjper_prev.
*ENDFORM.                    " GET_PERIODS
*&---------------------------------------------------------------------*
*&      Form  GET_PERIODS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_AUFNR  text
*      <--P_LV_GJPER_POST  text
*      <--P_LV_GJPER_CURR  text
*      <--P_LV_GJPER_PREV  text
*----------------------------------------------------------------------*
FORM GET_PERIODS_NEW USING IV_AUFNR TYPE AUFNR
                           IV_YEAR TYPE GJAHR
                           IV_MONTH TYPE MONAT
              CHANGING EV_GJPER_POST TYPE CO_GJPER
                       EV_GJPER_CURR TYPE CO_GJPER
                       EV_GJPER_PREV TYPE CO_GJPER.

  DATA: LV_WERKS TYPE WERKS_D.

  CHECK IV_AUFNR IS NOT INITIAL.

* Start; Commentated by Sung-Kon James Kim 2011/01/26
*    ev_gjper_prev = iv_year * 1000 + iv_month - 1.

*    IF ev_gjper_prev+4(3) = '000'.
*      ev_gjper_prev+4(3) = '012'.
*      ev_gjper_prev(4) = ev_gjper_prev(4) - 1.
*    ENDIF.
* End; Commentated by Sung-Kon James Kim 2011/01/26

  SELECT SINGLE PRWRK
           FROM CKMLMV013
           INTO LV_WERKS
          WHERE AUFNR = IV_AUFNR.

  PERFORM PERIODS_GET IN PROGRAM SAPLQRPRP USING LV_WERKS
                                             SY-DATUM
                                    CHANGING EV_GJPER_POST
                                             EV_GJPER_CURR
                                             EV_GJPER_PREV.

* -B RWU- posting period base on user entered from selection screen.
  EV_GJPER_POST = IV_YEAR * 1000 + IV_MONTH.
* -E RWU-

**** Important; In Order To allow for the past period before previous period.  2011/01/25
**** Start; Added by James Kim 2011/01/25
  IF EV_GJPER_POST NE EV_GJPER_CURR AND
    EV_GJPER_POST NE EV_GJPER_PREV.

    EV_GJPER_POST = IV_YEAR * 1000 + IV_MONTH.
    EV_GJPER_CURR = IV_YEAR * 1000 + IV_MONTH.
    EV_GJPER_PREV = IV_YEAR * 1000 + IV_MONTH.
  ENDIF.
**** End; Added by James Kim 2011/01/25

ENDFORM.                    " GET_PERIODS_NEW
*&---------------------------------------------------------------------*
*&      Form  WRITE_CPZP_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PKOSA_OBJNR  text
*      -->P_LV_GJPER_POST  text
*      -->P_LV_GJPER_CURR  text
*      -->P_LV_GJPER_PREV  text
*      -->P_LT_CPZP  text
*      <--P_EV_UPDATE_ERROR  text
*      <--P_LT_CPZP_BACKUP  text
*----------------------------------------------------------------------*
FORM WRITE_CPZP_NEW USING IV_PKOSA_OBJNR TYPE OBJNR
                          IV_GJPER_POST TYPE CO_GJPER
                          IV_GJPER_CURR TYPE CO_GJPER
                          "iv_gjper_prev TYPE co_gjper
                          IV_UPDCUR TYPE FLAG
                          IT_CPZP TYPE ZSAPBF_TT_CPZP
                 CHANGING EV_UPDATE_ERROR TYPE CHAR1
                          ET_CPZP_BACKUP TYPE ZSAPBF_TT_CPZP.

  DATA:LT_CPZP_CURRENT TYPE ZSAPBF_TT_CPZP,
*       lt_cpzp_previous TYPE zsapbf_tt_cpzp, "Commentated by Sung-Kon James Kim 2011/01/26
       LT_CPZP_POST TYPE ZSAPBF_TT_CPZP, "Added by Sung-Kon James Kim 2011/01/26

       LT_CPZP_BACKUP_CURRENT TYPE ZSAPBF_TT_CPZP, "Revoked by Sung-Kon James Kim 2011/01/26
*       lt_cpzp_backup_previous TYPE zsapbf_tt_cpzp, "Commentated by Sung-Kon James Kim 2011/01/26
       LT_CPZP_BACKUP_POST TYPE ZSAPBF_TT_CPZP, "Added by Sung-Kon James Kim 2011/01/26

       LS_CPZP TYPE CPZP,

       LT_ZCPZP_BACKUP TYPE ZSAPBF_TT_CPZP_BACKUP,
       LS_ZCPZP_BACKUP TYPE ZCPZP_BACKUP.

  DATA: LV_OBJNR TYPE OBJNR.
  DATA: LV_TIME_STAMP TYPE TZNTSTMPS.

*  FIELD-SYMBOLS <ls_cpzp> TYPE cpzp.

* Sorr it_CPZP

* Start; Added by Sung-Kon James Kim 2011/01/26
* Separate it to Post Period and Current Period

  LOOP AT IT_CPZP INTO LS_CPZP.
    IF LS_CPZP-GJPER = IV_GJPER_POST.
      APPEND LS_CPZP TO LT_CPZP_POST.
    ELSE.
      APPEND LS_CPZP TO LT_CPZP_CURRENT.
    ENDIF.
  ENDLOOP.
* End; Added by Sung-Kon James Kim 2011/01/26

* Start; Commentated by Sung-Kon James Kim 2011/01/26
** Separate it to current and previous
*
*  LOOP AT it_cpzp INTO ls_cpzp.
*    IF ls_cpzp-gjper = iv_gjper_curr.
*      APPEND ls_cpzp TO lt_cpzp_current.
*    ELSE.
*      APPEND ls_cpzp TO lt_cpzp_previous.
*    ENDIF.
*  ENDLOOP.
* End; Commentated by Sung-Kon James Kim 2011/01/26

***Start; Bellow 1 line; Commentated by Sung-Kon James Kim 2011/01/25
*  CHECK lt_cpzp_previous IS NOT INITIAL. "Stop if the correction(previous) period has no data
***End; Commentated by Sung-Kon James Kim 2011/01/25

**Start; Bellow 1 line; Added by Sung-Kon James Kim 2011/01/26
  CHECK LT_CPZP_POST IS NOT INITIAL.
  "Stop if the correction(Post / current Period) period has no data
**End; Added by Sung-Kon James Kim 2011/01/26

* Lock the P.C.C
*&---------------------------------------------------------------------*
*&      Form  write_cpzp
*&---------------------------------------------------------------------*
  PERFORM ENQUEUE USING IV_PKOSA_OBJNR
               CHANGING EV_UPDATE_ERROR.
  CHECK EV_UPDATE_ERROR IS INITIAL.

* backup CPZP from correction periods
  SELECT * FROM CPZP INTO TABLE ET_CPZP_BACKUP
          WHERE OBJNR = IV_PKOSA_OBJNR
*            AND gjper >= iv_gjper_post "Commentated by James Sung-Kon Kim 2011/02/21
            AND ( GJPER = IV_GJPER_POST OR GJPER = IV_GJPER_CURR ) "Added by James Sung-Kon Kim 2011/02/21
                .
* -B RWU- Unit unify with CSSL 2008-11-18
  PERFORM CSSL_UNIT_UNIFY_FOR_CPZP_TAB CHANGING ET_CPZP_BACKUP.
* -E RWU-

**** Start; Commentated by Sung-Kon James Kim 2011/01/26
**** Moved by Sung-Kon James Kim 2011/01/26 the routines to after Difference Existence Checking
** fillup ZCPZP_BACKUP
*  CONVERT DATE sy-datum
*          TIME sy-uzeit
*          INTO TIME STAMP lv_time_stamp TIME ZONE sy-zonlo. "'UTC   '.
*  LOOP AT et_cpzp_backup INTO ls_cpzp.
*    MOVE-CORRESPONDING ls_cpzp TO ls_zcpzp_backup.          "#EC ENHOK
*    ls_zcpzp_backup-backuptime = lv_time_stamp.
*    APPEND ls_zcpzp_backup TO lt_zcpzp_backup.
*    CLEAR ls_zcpzp_backup.
*  ENDLOOP.

** Copy GT_CPZP_TEMP into CPZPTEMP
*  IF lt_zcpzp_backup IS NOT INITIAL.
*    INSERT zcpzp_backup FROM TABLE lt_zcpzp_backup.
*  ENDIF.
**** End; Commentated by Sung-Kon James Kim 2011/01/26

**** Start; Commentated by Sung-Kon James Kim 2011/01/26
** Both previous periods and current periods(ISTMN) should be updated

** Separate it to current and previous
*  LOOP AT et_cpzp_backup INTO ls_cpzp.
*    IF ls_cpzp-gjper = iv_gjper_curr.
**      APPEND ls_cpzp TO lt_cpzp_backup_current.
*    ELSE.
*      APPEND ls_cpzp TO lt_cpzp_backup_previous.
*    ENDIF.
*    CLEAR ls_cpzp.
*  ENDLOOP.
**** End; Commentated by Sung-Kon James Kim 2011/01/26

*** Start; Added by Sung-Kon James Kim 2011/01/26
* Both Posting periods and current periods(ISTMN) should be updated

* Separate it to Post and Current
  LOOP AT ET_CPZP_BACKUP INTO LS_CPZP.
    IF LS_CPZP-GJPER = IV_GJPER_POST.
      APPEND LS_CPZP TO LT_CPZP_BACKUP_POST.
    ELSE.
      APPEND LS_CPZP TO LT_CPZP_BACKUP_CURRENT.
    ENDIF.
    CLEAR LS_CPZP.
  ENDLOOP.
*** End; Added by Sung-Kon James Kim 2011/01/26

**** Start; Commentated by Sung-Kon James Kim 2011/01/26
** step1 For previous period correct all records
*  IF lt_cpzp_backup_previous IS NOT INITIAL.
*    DELETE cpzp FROM TABLE lt_cpzp_backup_previous.
*  ENDIF.
*
*  IF lt_cpzp_previous IS NOT INITIAL.
*    INSERT cpzp FROM TABLE lt_cpzp_previous.
*  ENDIF.
**** End; Commentated by Sung-Kon James Kim 2011/01/26

**** Start; Added by Sung-Kon James Kim 2011/01/26

*********************************************************
* Bellow two routines can be used exclusively but equally.
* 2011/01/26 (Sung-Kon James Kim)
*--------------------------------------------------------
****** IF lt_cpzp_backup_post[] NE lt_cpzp_post[]
*--------------------------------------------------------
******        IF ls_cpzp_backup-istmn NE ls_cpzp-istmn OR
******           ls_cpzp_backup-gmper NE ls_cpzp-gmper OR
******           ls_cpzp_backup-xmper NE ls_cpzp-xmper OR
******           ls_cpzp_backup-gmsum NE ls_cpzp-gmsum OR
******           ls_cpzp_backup-xmsum NE ls_cpzp-xmsum OR
******           ls_cpzp_backup-meinh NE ls_cpzp-meinh OR
******           ls_cpzp_backup-varmn NE ls_cpzp-varmn .
*--------------------------------------------------------
*********************************************************

  DATA : L_FLAG_DIFFERENCE .

  IF LT_CPZP_BACKUP_POST[] NE LT_CPZP_POST[] .
    L_FLAG_DIFFERENCE = 'X' .
  ELSE.
    EV_UPDATE_ERROR = 'P' . " There is no difference (Successful)
    CLEAR L_FLAG_DIFFERENCE .
  ENDIF .

*  DATA : l_bkup_line  TYPE sy-tabix .
*  DATA : l_post_line  TYPE sy-tabix .
*  DATA : l_bkup_tabix TYPE sy-tabix .
*  DATA : l_post_tabix TYPE sy-tabix .
*  DATA : ls_cpzp_backup TYPE cpzp .

*  DESCRIBE TABLE lt_cpzp_backup_post LINES l_bkup_line .
*  DESCRIBE TABLE lt_cpzp_post LINES l_post_line .
*
*  IF l_bkup_line NE l_post_line OR
*     l_post_line IS INITIAL .
*    l_flag_difference = 'X' .
*  ELSE.
*    CLEAR l_flag_difference .
*  ENDIF .
*
*  IF l_flag_difference NE 'X' .
*    SORT lt_cpzp_backup_post BY objnr f_objnr gjper .
*    SORT lt_cpzp_post BY objnr f_objnr gjper .
*
*    LOOP AT lt_cpzp_post INTO ls_cpzp .
*      READ TABLE lt_cpzp_backup_post INTO ls_cpzp_backup
*                                     WITH KEY objnr    = ls_cpzp-objnr
*                                              f_objnr  = ls_cpzp-f_objnr
*                                              gjper    = ls_cpzp-gjper BINARY SEARCH .
*      IF sy-subrc IS NOT INITIAL .
*        l_flag_difference = 'X' .
*        EXIT .
*      ELSE .
*        IF ls_cpzp_backup-istmn NE ls_cpzp-istmn OR
*           ls_cpzp_backup-gmper NE ls_cpzp-gmper OR
*           ls_cpzp_backup-xmper NE ls_cpzp-xmper OR
*           ls_cpzp_backup-gmsum NE ls_cpzp-gmsum OR
*           ls_cpzp_backup-xmsum NE ls_cpzp-xmsum OR
*           ls_cpzp_backup-meinh NE ls_cpzp-meinh OR
*           ls_cpzp_backup-varmn NE ls_cpzp-varmn .
*          l_flag_difference = 'X' .
*          EXIT .
*        ENDIF .
*      ENDIF .
*    ENDLOOP .
*  ENDIF .

  IF  L_FLAG_DIFFERENCE = 'X'.
**** End; Added by Sung-Kon James Kim 2011/01/26

**** Start; Added by Sung-Kon James Kim 2011/01/26
* fillup ZCPZP_BACKUP
    CONVERT DATE SY-DATUM
            TIME SY-UZEIT
            INTO TIME STAMP LV_TIME_STAMP TIME ZONE SY-ZONLO. "'UTC   '.
    LOOP AT ET_CPZP_BACKUP INTO LS_CPZP.
      MOVE-CORRESPONDING LS_CPZP TO LS_ZCPZP_BACKUP.        "#EC ENHOK
      LS_ZCPZP_BACKUP-BACKUPTIME = LV_TIME_STAMP.
      APPEND LS_ZCPZP_BACKUP TO LT_ZCPZP_BACKUP.
      CLEAR LS_ZCPZP_BACKUP.
    ENDLOOP.

* Copy GT_CPZP_TEMP into ZCPZP_BACKUP
*--- Only WHEN THE DIFFERENCE EXISTS.
    IF LT_ZCPZP_BACKUP[] IS NOT INITIAL.
      INSERT ZCPZP_BACKUP FROM TABLE LT_ZCPZP_BACKUP.

      IF NOT SY-SUBRC IS INITIAL.
        EV_UPDATE_ERROR = 'U'.
        ROLLBACK WORK.
        EXIT.
      ENDIF.

    ENDIF.
**** End; Added by Sung-Kon James Kim 2011/01/26

**** Start; Added by Sung-Kon James Kim 2011/01/26
* step1 For posting period correct all records
    IF LT_CPZP_BACKUP_POST[] IS NOT INITIAL.
      DELETE CPZP FROM TABLE LT_CPZP_BACKUP_POST.

      IF NOT SY-SUBRC IS INITIAL.
        EV_UPDATE_ERROR = 'U'.
        ROLLBACK WORK.
        EXIT.
      ENDIF.

    ENDIF.

    IF LT_CPZP_POST[] IS NOT INITIAL.
      INSERT CPZP FROM TABLE LT_CPZP_POST.
    ENDIF.
**** End; Added by Sung-Kon James Kim 2011/01/26

    IF NOT SY-SUBRC IS INITIAL.
      EV_UPDATE_ERROR = 'U'.
      ROLLBACK WORK.
      EXIT.
    ENDIF.

**** Start; Commentated by Sung-Kon James Kim 2011/01/26
** step2 For current period
*** Option1 only update ISTMN fields
**  IF sy-uname NE 'SAPCD06'.
*  IF lt_cpzp_current IS NOT INITIAL.
*    LOOP AT lt_cpzp_current INTO ls_cpzp.
*      UPDATE cpzp SET istmn = ls_cpzp-istmn
*                        WHERE objnr EQ ls_cpzp-objnr
*                          AND f_objnr EQ ls_cpzp-f_objnr
*                          AND gjper EQ ls_cpzp-gjper
*                          AND zaehl EQ ls_cpzp-zaehl.
*      IF NOT sy-subrc IS INITIAL.
*        INSERT into cpzp values ls_cpzp.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
**** End; Commentated by Sung-Kon James Kim 2011/01/26

**** Start; Added by Sung-Kon James Kim 2011/01/26
* step2 For current period
** Option1 only update ISTMN fields
*  IF sy-uname NE 'SAPCD06'.
    IF LT_CPZP_CURRENT[] IS NOT INITIAL AND
       IV_UPDCUR IS NOT INITIAL .
      LOOP AT LT_CPZP_CURRENT INTO LS_CPZP.
        UPDATE CPZP
           SET ISTMN = LS_CPZP-ISTMN
               GMPER = LS_CPZP-GMPER "Added by Sung-Kon James Kim 2011/01/26
               XMPER = LS_CPZP-XMPER "Added by Sung-Kon James Kim 2011/01/26
               GMSUM = LS_CPZP-GMSUM "Added by Sung-Kon James Kim 2011/01/26
               XMSUM = LS_CPZP-XMSUM "Added by Sung-Kon James Kim 2011/01/26
               MEINH = LS_CPZP-MEINH "Added by Sung-Kon James Kim 2011/01/26
               VARMN = LS_CPZP-VARMN "Added by Sung-Kon James Kim 2011/01/26
         WHERE OBJNR EQ LS_CPZP-OBJNR
           AND F_OBJNR EQ LS_CPZP-F_OBJNR
           AND GJPER EQ LS_CPZP-GJPER
           AND ZAEHL EQ LS_CPZP-ZAEHL.

        IF NOT SY-SUBRC IS INITIAL.
          INSERT INTO CPZP VALUES LS_CPZP.
        ENDIF.

        IF NOT SY-SUBRC IS INITIAL.
          EV_UPDATE_ERROR = 'U'.
          ROLLBACK WORK.
          EXIT.
        ENDIF.

      ENDLOOP.
    ENDIF.
**** End; Added by Sung-Kon James Kim 2011/01/26

*  ELSE.
** Option2 full update
*    IF lt_cpzp_backup_previous IS NOT INITIAL.
*      DELETE cpzp FROM TABLE lt_cpzp_backup_current.
*    ENDIF.
*
*    IF lt_cpzp_previous IS NOT INITIAL.
*      INSERT cpzp FROM TABLE lt_cpzp_current.
*    ENDIF.
*  ENDIF.

    IF NOT SY-SUBRC IS INITIAL.
      EV_UPDATE_ERROR = 'U'.
      ROLLBACK WORK.
      EXIT.
    ELSE.
      COMMIT WORK.
    ENDIF.

  ENDIF.  " Added by Sung-Kon James Kim 2011/01/26

* Unlock the P.C.C
  PERFORM DEQUEUE USING LV_OBJNR.

ENDFORM.                    " write_cpzp_new
*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PF_STATUS                                          "#EC CALLED
                  USING RT_EXTAB TYPE SLIS_T_EXTAB .        "#EC NEEDED
  SET PF-STATUS 'ZSAPBF_WIP_STATUS'.
ENDFORM.                    " SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  SET_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COMMAND  text
*----------------------------------------------------------------------*
FORM SET_COMMAND  USING    PIV_UCOMM     TYPE SYUCOMM       "#EC CALLED
                           PIS_SELFIELD TYPE SLIS_SELFIELD.
*  DATA lt_cpzp_display TYPE STANDARD TABLE OF zsapbf_cpzp_display.
  CASE PIV_UCOMM.
*    WHEN '&CRB'.
    WHEN '&CRL'. " Previous page
      CLEAR GT_CPZP_DISPLAY_NUMPCC.
      PERFORM FILL_DISPLAY_CPZP USING 'P'
                          CHANGING GT_CPZP_DISPLAY_NUMPCC.
    WHEN '&CRR'. " Next page
      CLEAR GT_CPZP_DISPLAY_NUMPCC.
      PERFORM FILL_DISPLAY_CPZP USING 'N'
                          CHANGING GT_CPZP_DISPLAY_NUMPCC.
*    WHEN '&CRE'.
    WHEN '&CRB'. " First page
      CLEAR GT_CPZP_DISPLAY_NUMPCC.
      PERFORM FILL_DISPLAY_CPZP USING 'F'
                          CHANGING GT_CPZP_DISPLAY_NUMPCC.
    WHEN '&CRE'. " Last page
      CLEAR GT_CPZP_DISPLAY_NUMPCC.
      PERFORM FILL_DISPLAY_CPZP USING 'L'
                          CHANGING GT_CPZP_DISPLAY_NUMPCC.
    WHEN OTHERS.
  ENDCASE.
  PIS_SELFIELD-REFRESH = 'X'.
ENDFORM.                    " SET_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FILL_DISPLAY_CPZP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_CPZP_DISPLAY  text
*      <--P_LT_CPZP_DISPLAY  text
*----------------------------------------------------------------------*
FORM FILL_DISPLAY_CPZP  USING IV_IDX_TYPE TYPE C
                        CHANGING PET_CPZP_DISPLAY TYPE ZSAPBF_TT_CPZP_DISPLAY.
  DATA LS_PCC_OBJ TYPE GTY_PCC_OBJ_S.
  DATA LS_CPZP_DISPLAY TYPE ZSAPBF_CPZP_DISPLAY.
  DATA LV_MAX_NUM TYPE I.
  DATA LV_LAST_IDX_NUM TYPE I.

  CHECK NOT GV_NUM_PCC IS INITIAL.

* Number of lines in all result list
  LV_MAX_NUM = LINES( GT_PCC_OBJ ).

* Number of lines in last page.
  LV_LAST_IDX_NUM = LV_MAX_NUM MOD GV_NUM_PCC.

  IF LV_LAST_IDX_NUM = 0.
    LV_LAST_IDX_NUM = GV_NUM_PCC.
  ENDIF.

  CASE IV_IDX_TYPE.
    WHEN 'P'. " Previous page
      IF GV_PCC_IDX = LV_MAX_NUM.
        GV_PCC_IDX = GV_PCC_IDX - LV_LAST_IDX_NUM - GV_NUM_PCC.
      ELSE.
        GV_PCC_IDX = GV_PCC_IDX - 2 * GV_NUM_PCC.
      ENDIF.
      IF GV_PCC_IDX < 0.
        GV_PCC_IDX = 0.
      ENDIF.
    WHEN 'N'. " Next page
      IF GV_PCC_IDX = LV_MAX_NUM.
        GV_PCC_IDX = LV_MAX_NUM - LV_LAST_IDX_NUM.
        IF GV_PCC_IDX < 0.
          GV_PCC_IDX = 0.
        ENDIF.
      ENDIF.
    WHEN 'F'. " First page
      GV_PCC_IDX = 0.
    WHEN 'L'. " Last page
      GV_PCC_IDX = LV_MAX_NUM - LV_LAST_IDX_NUM.

    WHEN OTHERS.
  ENDCASE.

  DO GV_NUM_PCC TIMES.
    IF GV_PCC_IDX GE LV_MAX_NUM.
      EXIT.
    ENDIF.
    GV_PCC_IDX = GV_PCC_IDX + 1.
    READ TABLE GT_PCC_OBJ INTO LS_PCC_OBJ INDEX GV_PCC_IDX.
    CHECK SY-SUBRC IS INITIAL.
    LOOP AT GT_CPZP_DISPLAY INTO LS_CPZP_DISPLAY WHERE OBJNR = LS_PCC_OBJ-OBJNR.
      APPEND LS_CPZP_DISPLAY TO PET_CPZP_DISPLAY.
    ENDLOOP.
  ENDDO.

ENDFORM.                    " FILL_DISPLAY_CPZP
*&---------------------------------------------------------------------*
*&      Form  CSSL_UNIT_UNIFY_FOR_ACTCOMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LFS_ACT_COMP_ORDERID>  text
*----------------------------------------------------------------------*
FORM CSSL_UNIT_UNIFY_FOR_ACTCOMP  CHANGING CS_ACT_COMP TYPE GT_ACT_COMP_ORDERID_TYP.

  IF NOT CS_ACT_COMP-PPC_ACT-DURATION_VAR IS INITIAL.
    PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING CS_ACT_COMP-PPC_ACT-RESSOURCE_GUID
                                              CS_ACT_COMP-GJPER(4)
                                        CHANGING CS_ACT_COMP-PPC_ACT-DURUNIT
                                                 CS_ACT_COMP-PPC_ACT-DURATION_VAR.
  ENDIF.

  IF NOT CS_ACT_COMP-PPC_ACT-DELTA_DUR_VAR IS INITIAL.
    PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING CS_ACT_COMP-PPC_ACT-RESSOURCE_GUID
                                              CS_ACT_COMP-GJPER(4)
                                        CHANGING CS_ACT_COMP-PPC_ACT-DURUNIT
                                                 CS_ACT_COMP-PPC_ACT-DELTA_DUR_VAR.
  ENDIF.

  IF NOT CS_ACT_COMP-PPC_ACT-DURATION_FIX IS INITIAL.
    PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING CS_ACT_COMP-PPC_ACT-RESSOURCE_GUID
                                              CS_ACT_COMP-GJPER(4)
                                        CHANGING CS_ACT_COMP-PPC_ACT-DURUNIT
                                                 CS_ACT_COMP-PPC_ACT-DURATION_FIX.
  ENDIF.

  IF NOT CS_ACT_COMP-PPC_ACT-DELTA_DUR_FIX IS INITIAL.
    PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING CS_ACT_COMP-PPC_ACT-RESSOURCE_GUID
                                              CS_ACT_COMP-GJPER(4)
                                        CHANGING CS_ACT_COMP-PPC_ACT-DURUNIT
                                                 CS_ACT_COMP-PPC_ACT-DELTA_DUR_FIX.
  ENDIF.

ENDFORM.                    " CSSL_UNIT_UNIFY_FOR_ACTCOMP

*&---------------------------------------------------------------------*
*&      Form  CSSL_UNIT_UNIFY_FOR_CPZP_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_ET_CPZP_LAST  text
*----------------------------------------------------------------------*
FORM CSSL_UNIT_UNIFY_FOR_CPZP_TAB  CHANGING CT_CPZP_TAB TYPE ZSAPBF_TT_CPZP.
  FIELD-SYMBOLS <LS_CPZP> TYPE CPZP.
  DATA LT_RESGUID_TAB TYPE KCR_GUID_16_TAB.
  DATA LS_RESGUID     TYPE KCR_D_GUID_16.
  LOOP AT CT_CPZP_TAB ASSIGNING <LS_CPZP> WHERE F_OBJNR(2) = 'KL'.
    CLEAR LS_RESGUID.
    CALL FUNCTION 'KCR04_GET_IPPE_RESOURCE'
      EXPORTING
        I_OBJECT_NUMBER     = <LS_CPZP>-F_OBJNR
        I_KEY_DATE          = SY-DATLO
      IMPORTING
        E_RESOURCE_GUID_TAB = LT_RESGUID_TAB
      EXCEPTIONS
        NOT_FOUND           = 1
        OTHERS              = 2.
    IF SY-SUBRC IS INITIAL.
      LOOP AT LT_RESGUID_TAB INTO LS_RESGUID.
        IF NOT LT_RESGUID_TAB IS INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF SY-SUBRC IS INITIAL.
        PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                  <LS_CPZP>-GJPER(4)
                                            CHANGING <LS_CPZP>-MEINH
                                                     <LS_CPZP>-ISTMN.
        PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                  <LS_CPZP>-GJPER(4)
                                            CHANGING <LS_CPZP>-MEINH
                                                     <LS_CPZP>-GMPER.
        PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                  <LS_CPZP>-GJPER(4)
                                            CHANGING <LS_CPZP>-MEINH
                                                     <LS_CPZP>-XMPER.
        PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                  <LS_CPZP>-GJPER(4)
                                            CHANGING <LS_CPZP>-MEINH
                                                     <LS_CPZP>-GMSUM.
        PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                  <LS_CPZP>-GJPER(4)
                                            CHANGING <LS_CPZP>-MEINH
                                                     <LS_CPZP>-XMSUM.

        PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID "VARMN; Added by James Sung-Kon Kim 2011/02/01
                                                  <LS_CPZP>-GJPER(4)
                                            CHANGING <LS_CPZP>-MEINH
                                                     <LS_CPZP>-VARMN.
      ENDIF.
    ELSE.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CSSL_UNIT_UNIFY_FOR_CPZP_TAB

*&---------------------------------------------------------------------*
*&      Form  CSSL_UNIT_UNIFY_FOR_LAST_CPZP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_ACT_COMP_GJPER(4)  text
*      <--P_ET_CPZP_LAST  text
*----------------------------------------------------------------------*
FORM CSSL_UNIT_UNIFY_FOR_LAST_CPZP  USING    IV_GJAHR    TYPE GJAHR
                                    CHANGING CS_CPZP_TAB TYPE CPZP.
*  FIELD-SYMBOLS <ls_cpzp> TYPE cpzp.
  DATA LT_RESGUID_TAB TYPE KCR_GUID_16_TAB.
  DATA LS_RESGUID     TYPE KCR_D_GUID_16.

*  LOOP AT ct_cpzp_tab ASSIGNING <ls_cpzp> WHERE f_objnr(2) = 'KL'.
  CLEAR LS_RESGUID.
  CALL FUNCTION 'KCR04_GET_IPPE_RESOURCE'
    EXPORTING
      I_OBJECT_NUMBER     = CS_CPZP_TAB-F_OBJNR
      I_KEY_DATE          = SY-DATLO
    IMPORTING
      E_RESOURCE_GUID_TAB = LT_RESGUID_TAB
    EXCEPTIONS
      NOT_FOUND           = 1
      OTHERS              = 2.

  IF SY-SUBRC IS INITIAL.
    LOOP AT LT_RESGUID_TAB INTO LS_RESGUID.
      IF NOT LT_RESGUID_TAB IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF SY-SUBRC IS INITIAL.
      PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                IV_GJAHR
                                          CHANGING CS_CPZP_TAB-MEINH
                                                   CS_CPZP_TAB-ISTMN.
      PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                IV_GJAHR
                                          CHANGING CS_CPZP_TAB-MEINH
                                                   CS_CPZP_TAB-GMPER.
      PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                IV_GJAHR
                                          CHANGING CS_CPZP_TAB-MEINH
                                                   CS_CPZP_TAB-XMPER.
      PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                IV_GJAHR
                                          CHANGING CS_CPZP_TAB-MEINH
                                                   CS_CPZP_TAB-GMSUM.
      PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID
                                                IV_GJAHR
                                          CHANGING CS_CPZP_TAB-MEINH
                                                   CS_CPZP_TAB-XMSUM.

      PERFORM CSSL_UNIT_UNIFY_FOR_RESGUID USING LS_RESGUID "VARMN; Added by James Sung-Kon Kim 2011/02/01
                                                IV_GJAHR
                                          CHANGING CS_CPZP_TAB-MEINH
                                                   CS_CPZP_TAB-VARMN.
    ENDIF.
  ELSE.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*  ENDLOOP.
ENDFORM.                    " CSSL_UNIT_UNIFY_FOR_CPZP_TAB


*&---------------------------------------------------------------------*
*&      Form  CSSL_UNIT_UNIFY_FOR_RESGUID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_ACT_COMP_PPC_ACT_RESSOURCE_  text
*      -->P_CS_ACT_COMP_GJPER(4)  text
*      -->P_CS_ACT_COMP_PPC_ACT_DURUNIT  text
*      <--P_CS_ACT_COMP_PPC_ACT_DURATION_V  text
*----------------------------------------------------------------------*
FORM CSSL_UNIT_UNIFY_FOR_RESGUID  USING    IV_RESGUID  TYPE PPC_RESGUID_INT
                                           IV_GJAHR    TYPE GJAHR
                                  CHANGING CV_DURUNIT  TYPE PPC_DURUNIT
                                           CV_DURATION TYPE PPC_DURATION_VAR.
  DATA LS_ACT_IPPE TYPE ZSAPBF_ACT_IPPE.
  DATA LS_CSSL     TYPE CSSL.

*-------------------------------------
* Read dummy IPPE data
  READ TABLE GT_ACT_IPPE INTO LS_ACT_IPPE WITH KEY RESOURCE_GUID = IV_RESGUID
                                                   .
  IF NOT SY-SUBRC IS INITIAL.
*    break-point.  -- HMMA FIX IT lATER
    SELECT SINGLE * INTO LS_ACT_IPPE
                    FROM ZSAPBF_ACT_IPPE                    "#EC *
     WHERE RESOURCE_GUID = IV_RESGUID
     .
    IF SY-SUBRC IS INITIAL.
      APPEND LS_ACT_IPPE TO GT_ACT_IPPE.
    ENDIF.
  ENDIF.

*-------------------------------------
* Read CSSL unit.
  CHECK NOT LS_ACT_IPPE IS INITIAL.
  READ TABLE GT_CSSL INTO LS_CSSL WITH KEY
                                   KOKRS = LS_ACT_IPPE-KOKRS
                                   KOSTL = LS_ACT_IPPE-COST_CENTER
                                   LSTAR = LS_ACT_IPPE-ACTIVITY_TYPE
                                   GJAHR = IV_GJAHR.
  IF NOT SY-SUBRC IS INITIAL.
    SELECT SINGLE * INTO LS_CSSL
                    FROM CSSL
     WHERE KOKRS = LS_ACT_IPPE-KOKRS
       AND KOSTL = LS_ACT_IPPE-COST_CENTER
       AND LSTAR = LS_ACT_IPPE-ACTIVITY_TYPE
       AND GJAHR = IV_GJAHR.
    IF SY-SUBRC IS INITIAL.
      APPEND LS_CSSL TO GT_CSSL.
    ENDIF.
  ENDIF.

*-------------------------------------
* Conver unit.
  CHECK NOT LS_CSSL IS INITIAL.
  CHECK ( LS_CSSL-LEINH NE CV_DURUNIT ) AND ( NOT CV_DURUNIT IS INITIAL ) AND ( NOT CV_DURATION IS INITIAL ).

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      INPUT                      = CV_DURATION
*      no_type_check              = 'x'
*      round_sign                 = 'X'
* Above 1 line(round_sign); deactivated on 2010.09.08  by PPAB10
* To avoid the quantity difference.
      UNIT_IN                    = CV_DURUNIT
      UNIT_OUT                   = LS_CSSL-LEINH
    IMPORTING
*      add_const                  =
*      decimals                   =
*      denominator                =
*      numerator                  =
      OUTPUT                     = CV_DURATION
    EXCEPTIONS
      CONVERSION_NOT_FOUND       = 1
      DIVISION_BY_ZERO           = 2
      INPUT_INVALID              = 3
      OUTPUT_INVALID             = 4
      OVERFLOW                   = 5
      TYPE_INVALID               = 6
      UNITS_MISSING              = 7
      UNIT_IN_NOT_FOUND          = 8
      UNIT_OUT_NOT_FOUND         = 9
      OTHERS                     = 10
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Add on 2010.09.08 ---> Change Unit
  CV_DURUNIT = LS_CSSL-LEINH.

ENDFORM.                    " CSSL_UNIT_UNIFY_FOR_RESGUID
*&---------------------------------------------------------------------*
*&      Form  PREPARE_UNIT_UNIFY_FOR_ACTCOMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ACTCOMP_UNIT_UNIFY_PRE_TAB  text
*      -->P_<FS_PERIOD>_GJPER(4)  text
*----------------------------------------------------------------------*
FORM PREPARE_UNIT_UNIFY_FOR_ACTCOMP  USING    IT_ACTCOMP TYPE PPC_T_ACTIVITY_COMPONENTS
                                              IV_GJAHR   TYPE GJAHR.

  DATA LT_ACT_IPPE TYPE STANDARD TABLE OF ZSAPBF_ACT_IPPE." Global dummy Ippe buffer.
  DATA LT_CSSL     TYPE STANDARD TABLE OF CSSL." Global CSSL buffer.

* Get dummp Ippe for all activity components
  CHECK NOT IT_ACTCOMP IS INITIAL.

  SELECT * INTO TABLE LT_ACT_IPPE
           FROM ZSAPBF_ACT_IPPE
     FOR ALL ENTRIES IN IT_ACTCOMP
   WHERE RESOURCE_GUID = IT_ACTCOMP-RESSOURCE_GUID
   .
  CHECK SY-SUBRC IS INITIAL.
  APPEND LINES OF LT_ACT_IPPE TO GT_ACT_IPPE.
  SORT GT_ACT_IPPE.
  DELETE ADJACENT DUPLICATES FROM GT_ACT_IPPE.

* Get all CSSL data for all activity components
  SORT LT_ACT_IPPE.
  DELETE ADJACENT DUPLICATES FROM LT_ACT_IPPE.

  CHECK NOT LT_ACT_IPPE IS INITIAL.

  SELECT * APPENDING TABLE LT_CSSL
                FROM CSSL
     FOR ALL ENTRIES IN LT_ACT_IPPE
   WHERE KOKRS = LT_ACT_IPPE-KOKRS
     AND KOSTL = LT_ACT_IPPE-COST_CENTER
     AND LSTAR = LT_ACT_IPPE-ACTIVITY_TYPE
     AND GJAHR = IV_GJAHR
   .
  CHECK SY-SUBRC IS INITIAL.
  APPEND LINES OF LT_CSSL TO GT_CSSL.
  SORT GT_CSSL.
  DELETE ADJACENT DUPLICATES FROM GT_CSSL.


ENDFORM.                    " PREPARE_UNIT_UNIFY_FOR_ACTCOMP
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENTS_PARALLEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PPC_ORD_INF  text
*      -->P_LT_PPC_HEAD  text
*      -->P_LT_PERIODS  text
*      -->P_IS_PARALLEL  text
*      -->P_IV_WPSSUB  text
*      <--P_LT_MAT_COMP_ORDERID  text
*      <--P_LT_MAT_REV_ORDERID  text
*      <--P_LT_MAT_COMP_VAR_ORDERID  text
*      <--P_LT_MAT_REV_VAR_ORDERID  text
*      <--P_LT_ACT_COMP_ORDERID  text
*      <--P_LT_ACT_REV_ORDERID  text
*      <--P_LT_ACT_COMP_VAR_ORDERID  text
*      <--P_LT_ACT_REV_VAR_ORDERID  text
*----------------------------------------------------------------------*
FORM READ_DOCUMENTS_PARALLEL
                    USING IT_PPC_ORD_INF TYPE ZSAPBF_TT_PPC_ORD_INF
                          IT_PPC_HEAD TYPE PPC_T_HEAD
                          IT_PERIODS TYPE TT_PERIOD
                          IS_PARALLEL TYPE PPC_PARALLEL
                          IV_WPSSUB TYPE INT4
                 CHANGING ET_MAT_COMP_ORDERID TYPE TT_MAT_COMP_ORDERID      "component: forward
                          ET_MAT_REV_ORDERID TYPE TT_MAT_COMP_ORDERID       "component: reverse
                          ET_MAT_COMP_VAR_ORDERID TYPE TT_MAT_COMP_ORDERID  "component: forward variance
                          ET_MAT_REV_VAR_ORDERID TYPE TT_MAT_COMP_ORDERID   "component: reverse variance

                          ET_ACT_COMP_ORDERID TYPE TT_ACT_COMP_ORDERID      "activity: forward
                          ET_ACT_REV_ORDERID TYPE TT_ACT_COMP_ORDERID       "activity: reverse
                          ET_ACT_COMP_VAR_ORDERID TYPE TT_ACT_COMP_ORDERID  "activity: forward variance
                          ET_ACT_REV_VAR_ORDERID TYPE TT_ACT_COMP_ORDERID   "activity: reverse variance .
                          .
  DATA: FT_PPC_REPPOINT TYPE TABLE OF PPC_REPPOINT_INT,
        LS_PPC_REPPOINT TYPE PPC_REPPOINT_INT.

  DATA: LV_TASK_CNT(4) TYPE N.
  DATA: L_INDEX       LIKE SY-TABIX.

* Task List Create
  DO  IV_WPSSUB TIMES.
    LV_TASK_CNT = LV_TASK_CNT + 1.
    CONCATENATE 'READ_DEBIT' LV_TASK_CNT INTO GT_TASK_SUB-NAME.
    GT_TASK_SUB-CLASSNAME = IS_PARALLEL-SERVERGROUP.
    GT_TASK_SUB-STATUS = 'I'.
    APPEND GT_TASK_SUB.
    CLEAR GT_TASK_SUB.
  ENDDO.

* read all quantities for every APO order on reporting points base
* get orderid
  LOOP AT IT_PPC_ORD_INF ASSIGNING <FS_PPC_ORD_INF>.
    LOOP AT IT_PERIODS ASSIGNING <FS_PERIOD>.
* get reporting points
      LOOP AT IT_PPC_HEAD ASSIGNING <FS_PPC_HEAD>
                              WHERE ORDERID = <FS_PPC_ORD_INF>-ORDERID
                                AND POSTDATE >= <FS_PERIOD>-STARTDAY
                                AND POSTDATE <= <FS_PERIOD>-ENDDAY.
        CHECK NOT <FS_PPC_HEAD>-REPPOINT IS INITIAL.
        LS_PPC_REPPOINT = <FS_PPC_HEAD>-REPPOINT.
        APPEND LS_PPC_REPPOINT TO FT_PPC_REPPOINT.
      ENDLOOP.

      IF SY-SUBRC = 0.
        DO.
          READ TABLE GT_TASK_SUB WITH KEY STATUS = 'I'.

          IF SY-SUBRC = 0.
            L_INDEX = SY-TABIX.
*        CALL FUNCTION 'PPC1DC_COMP_ORD_READ_NEW'
            CALL FUNCTION 'ZSAPBF_PPC1_COMP_ORD_READ_DOC'
              STARTING NEW TASK GT_TASK_SUB-NAME
              DESTINATION IN GROUP GT_TASK_SUB-CLASSNAME
              PERFORMING RETURN_DATA_DEBIT ON END OF TASK
              EXPORTING
                IF_ORDERID            = <FS_PPC_ORD_INF>-ORDERID
                IT_REPPOINTS          = FT_PPC_REPPOINT
                I_GJPER               = <FS_PERIOD>-GJPER
                I_DATE_LOW            = <FS_PERIOD>-STARTDAY
                I_DATE_HIGH           = <FS_PERIOD>-ENDDAY
              EXCEPTIONS
                COMMUNICATION_FAILURE = 1
                SYSTEM_FAILURE        = 2
                RESOURCE_FAILURE      = 3
                LOCK_ERROR            = 4
                OTHERS                = 5.

            IF SY-SUBRC <> 0.
              WAIT UNTIL GV_RCV_SUB >= GV_SND_SUB UP TO 1 SECONDS.
            ELSE.
              READ TABLE GT_TASK_SUB INDEX L_INDEX.
              GT_TASK_SUB-STATUS = 'W'.
              MODIFY GT_TASK_SUB INDEX L_INDEX.
*           send data
              GV_SND_SUB = GV_SND_SUB + 1.
              EXIT.
            ENDIF.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.

        ENDDO.
        REFRESH FT_PPC_REPPOINT.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

  WAIT UNTIL GV_RCV_SUB >= GV_SND_SUB.

  APPEND LINES OF GT_MAT_COMP_ORDERID TO ET_MAT_COMP_ORDERID.
  APPEND LINES OF GT_MAT_REV_ORDERID TO ET_MAT_REV_ORDERID.
  APPEND LINES OF GT_MAT_COMP_VAR_ORDERID TO ET_MAT_COMP_VAR_ORDERID.
  APPEND LINES OF GT_MAT_REV_VAR_ORDERID TO ET_MAT_REV_VAR_ORDERID.
  APPEND LINES OF GT_ACT_COMP_ORDERID TO ET_ACT_COMP_ORDERID.
  APPEND LINES OF GT_ACT_REV_ORDERID TO ET_ACT_REV_ORDERID.
  APPEND LINES OF GT_ACT_COMP_VAR_ORDERID TO ET_ACT_COMP_VAR_ORDERID.
  APPEND LINES OF GT_ACT_REV_VAR_ORDERID TO ET_ACT_REV_VAR_ORDERID.

  CLEAR: GT_MAT_COMP_ORDERID[], GT_MAT_COMP_ORDERID.
  CLEAR: GT_MAT_REV_ORDERID[], GT_MAT_REV_ORDERID.
  CLEAR: GT_MAT_COMP_VAR_ORDERID[], GT_MAT_COMP_VAR_ORDERID.
  CLEAR: GT_MAT_REV_VAR_ORDERID[], GT_MAT_REV_VAR_ORDERID.
  CLEAR: GT_ACT_COMP_ORDERID[], GT_ACT_COMP_ORDERID.
  CLEAR: GT_ACT_REV_ORDERID[], GT_ACT_REV_ORDERID.
  CLEAR: GT_ACT_COMP_VAR_ORDERID[], GT_ACT_COMP_VAR_ORDERID.
  CLEAR: GT_ACT_REV_VAR_ORDERID[], GT_ACT_REV_VAR_ORDERID.

  CLEAR: GT_TASK_SUB[], GT_TASK_SUB.
  CLEAR: GV_SND_SUB, GV_RCV_SUB.

ENDFORM.                    " READ_DOCUMENTS_PARALLEL
*&---------------------------------------------------------------------*
*&      Form  RETURN_DATA_DEBIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RETURN_DATA_DEBIT USING TASKNAME.

  DATA: LV_TABIX TYPE SYTABIX.

  DATA: FT_MATERIAL_COMPONENTS TYPE  STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,
        FT_REV_MAT_COMPONENTS TYPE STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,
        FT_MAT_COMP_VAR TYPE STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,
        FT_REV_MAT_COMP_VAR TYPE STANDARD TABLE OF PPC_MATERIAL_COMPONENTS,

        FT_ACT_COMP TYPE STANDARD TABLE OF  PPC_ACTIVITY_COMPONENTS,
        FT_REV_ACT_COMP TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS,
        FT_ACT_COMP_VAR TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS,
        FT_REV_ACT_COMP_VAR TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS.
*        ft_reppoints TYPE STANDARD TABLE OF ppc_reppoints,
*        ft_rev_reppoints TYPE STANDARD TABLE OF ppc_reppoints,

  DATA: LT_MAT_COMP_ORDERID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,
        LT_ACT_COMP_ORDERID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
        LT_MAT_COMP_VAR_ORDERID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,
        LT_REV_MAT_COM_ORDERID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,

        LT_ACT_COMP_VAR_ORDERID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
        LT_REV_MAT_COMP_VAR_ID TYPE STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,
        LT_REV_ACT_COMP_ORDERID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,
        LT_REV_ACT_COMP_VAR_ID TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP.

* -B RWU- Unit unify with CSSL 2008-11-18
  DATA LT_ACTCOMP_UNIT_UNIFY_PRE_TAB TYPE STANDARD TABLE OF PPC_ACTIVITY_COMPONENTS.
* -E RWU-

  FIELD-SYMBOLS: <LFS_MAT_COMP_ORDERID> TYPE GT_MAT_COMP_ORDERID_TYP,
                 <LFS_ACT_COMP_ORDERID> TYPE GT_ACT_COMP_ORDERID_TYP,
                 <LFS_ACT_COMP_VAR_ORDERID> TYPE GT_ACT_COMP_ORDERID_TYP,
                 <LFS_MAT_COMP_VAR_ORDERID> TYPE GT_MAT_COMP_ORDERID_TYP.

  DATA: LS_PPC_ORD_INF TYPE PPC_ORD_INF,
        LS_PPC_HEAD TYPE PPC_HEAD,
        LS_PERIOD TYPE TS_PERIOD.

  RECEIVE RESULTS FROM FUNCTION 'ZSAPBF_PPC1_COMP_ORD_READ_DOC'
       IMPORTING
            ET_MATERIAL_COMPONENTS = FT_MATERIAL_COMPONENTS
            ET_REV_MAT_COMPONENTS  = FT_REV_MAT_COMPONENTS
            ET_MAT_COMP_VAR        = FT_MAT_COMP_VAR
            ET_REV_MAT_COMP_VAR    = FT_REV_MAT_COMP_VAR
            ET_ACT_COMP            = FT_ACT_COMP
            ET_REV_ACT_COMP        = FT_REV_ACT_COMP
            ET_ACT_COMP_VAR        = FT_ACT_COMP_VAR
            ET_REV_ACT_COMP_VAR    = FT_REV_ACT_COMP_VAR
*            et_reppoints           = ft_reppoints
*            et_rev_reppoints       = ft_rev_reppoints
       CHANGING
            CF_ORDERID             = LS_PPC_ORD_INF-ORDERID
*            ct_reppoints           = ft_ppc_reppoint
            C_GJPER                = LS_PERIOD-GJPER
            C_DATE_LOW             = LS_PERIOD-STARTDAY
            C_DATE_HIGH            = LS_PERIOD-ENDDAY
       EXCEPTIONS
            COMMUNICATION_FAILURE = 1
            SYSTEM_FAILURE        = 2
            RESOURCE_FAILURE      = 3
            LOCK_ERROR            = 4
            OTHERS                = 5.

  GV_RCV_SUB = GV_RCV_SUB + 1.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    APPEND LINES OF FT_MATERIAL_COMPONENTS TO LT_MAT_COMP_ORDERID.
    APPEND LINES OF FT_REV_MAT_COMPONENTS TO LT_REV_MAT_COM_ORDERID.
    APPEND LINES OF FT_MAT_COMP_VAR TO LT_MAT_COMP_VAR_ORDERID.
    APPEND LINES OF FT_REV_MAT_COMP_VAR TO LT_REV_MAT_COMP_VAR_ID.
    APPEND LINES OF FT_ACT_COMP TO LT_ACT_COMP_ORDERID.
    APPEND LINES OF FT_REV_ACT_COMP TO LT_REV_ACT_COMP_ORDERID.
    APPEND LINES OF FT_ACT_COMP_VAR TO LT_ACT_COMP_VAR_ORDERID.
    APPEND LINES OF FT_REV_ACT_COMP_VAR TO LT_REV_ACT_COMP_VAR_ID.
*        APPEND LINES OF ft_reppoints TO gt_reppoints.
*        APPEND LINES OF ft_rev_reppoints TO gt_rev_reppoints.

* -B RWU- Unit unify with CSSL 2008-11-18
    APPEND LINES OF FT_ACT_COMP     TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.
    APPEND LINES OF FT_REV_ACT_COMP TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.
    APPEND LINES OF FT_ACT_COMP_VAR TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.
    APPEND LINES OF FT_REV_ACT_COMP_VAR TO LT_ACTCOMP_UNIT_UNIFY_PRE_TAB.

    SORT LT_ACTCOMP_UNIT_UNIFY_PRE_TAB BY RESSOURCE_GUID.
    DELETE ADJACENT DUPLICATES FROM LT_ACTCOMP_UNIT_UNIFY_PRE_TAB COMPARING RESSOURCE_GUID.

    PERFORM PREPARE_UNIT_UNIFY_FOR_ACTCOMP USING LT_ACTCOMP_UNIT_UNIFY_PRE_TAB
                                                 LS_PERIOD-GJPER(4)
                                                 .
* -E RWU-

* substract variance from components and append components and orderid
* to itab
    LOOP AT LT_MAT_COMP_ORDERID ASSIGNING <LFS_MAT_COMP_ORDERID>.
      <LFS_MAT_COMP_ORDERID>-ORDERID = LS_PPC_ORD_INF-ORDERID.
      <LFS_MAT_COMP_ORDERID>-GJPER = LS_PERIOD-GJPER.

      <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY =
      <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY -
      <LFS_MAT_COMP_ORDERID>-PPC_MAT-DELTA_QUANTITY.
    ENDLOOP.
    APPEND LINES OF LT_MAT_COMP_ORDERID TO GT_MAT_COMP_ORDERID.
    REFRESH LT_MAT_COMP_ORDERID.

* append reversals (components) and orderid to itab
    LOOP AT LT_REV_MAT_COM_ORDERID ASSIGNING <LFS_MAT_COMP_ORDERID>.
      <LFS_MAT_COMP_ORDERID>-ORDERID = LS_PPC_HEAD-ORDERID.
      <LFS_MAT_COMP_ORDERID>-GJPER = LS_PERIOD-GJPER.

      <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY =
      <LFS_MAT_COMP_ORDERID>-PPC_MAT-QUANTITY -
      <LFS_MAT_COMP_ORDERID>-PPC_MAT-DELTA_QUANTITY.
    ENDLOOP.
    APPEND LINES OF LT_REV_MAT_COM_ORDERID TO GT_MAT_REV_ORDERID.
    REFRESH LT_REV_MAT_COM_ORDERID.

* substract variances from activities and append activities and orderid
* to itab
    LOOP AT LT_ACT_COMP_ORDERID ASSIGNING <LFS_ACT_COMP_ORDERID>.
      <LFS_ACT_COMP_ORDERID>-ORDERID = LS_PPC_HEAD-ORDERID.
      <LFS_ACT_COMP_ORDERID>-GJPER = LS_PERIOD-GJPER.

* -B RWU- Unit unify with CSSL 2008-11-18
      PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_ORDERID>.

      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR =
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR -
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_VAR.
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX =
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX -
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_FIX.
* -E RWU-                                    .
    ENDLOOP.
    APPEND LINES OF LT_ACT_COMP_ORDERID TO GT_ACT_COMP_ORDERID.
    REFRESH LT_ACT_COMP_ORDERID.

* append reversals (activities) and orderid to itab
    LOOP AT LT_REV_ACT_COMP_ORDERID ASSIGNING <LFS_ACT_COMP_ORDERID>.
      <LFS_ACT_COMP_ORDERID>-ORDERID = LS_PPC_HEAD-ORDERID.
      <LFS_ACT_COMP_ORDERID>-GJPER = LS_PERIOD-GJPER.

* -B RWU- Unit unify with CSSL 2008-11-18
      PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_ORDERID>.

      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR =
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_VAR -
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_VAR.
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX =
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DURATION_FIX -
      <LFS_ACT_COMP_ORDERID>-PPC_ACT-DELTA_DUR_FIX.
* -E RWU-                               .
    ENDLOOP.
    APPEND LINES OF LT_REV_ACT_COMP_ORDERID TO GT_ACT_REV_ORDERID.
    REFRESH LT_REV_ACT_COMP_ORDERID.

* append variances (components) and orderid to itab
    LOOP AT LT_MAT_COMP_VAR_ORDERID ASSIGNING <LFS_MAT_COMP_VAR_ORDERID>.
      <LFS_MAT_COMP_VAR_ORDERID>-ORDERID = LS_PPC_HEAD-ORDERID.
      <LFS_MAT_COMP_VAR_ORDERID>-GJPER = LS_PERIOD-GJPER.
    ENDLOOP.
    APPEND LINES OF LT_MAT_COMP_VAR_ORDERID TO GT_MAT_COMP_VAR_ORDERID.
    REFRESH LT_MAT_COMP_VAR_ORDERID.

* append reversals of variances (components) and orderid to itab
    LOOP AT LT_REV_MAT_COMP_VAR_ID ASSIGNING <LFS_MAT_COMP_VAR_ORDERID>.
      <LFS_MAT_COMP_VAR_ORDERID>-ORDERID = LS_PPC_HEAD-ORDERID.
      <LFS_MAT_COMP_VAR_ORDERID>-GJPER = LS_PERIOD-GJPER.
    ENDLOOP.
    APPEND LINES OF LT_REV_MAT_COMP_VAR_ID TO GT_MAT_REV_VAR_ORDERID.
    REFRESH LT_REV_MAT_COMP_VAR_ID.

* append variances (activities) and orderid to itab
    LOOP AT LT_ACT_COMP_VAR_ORDERID ASSIGNING <LFS_ACT_COMP_VAR_ORDERID>.
      <LFS_ACT_COMP_VAR_ORDERID>-ORDERID = LS_PPC_HEAD-ORDERID.
      <LFS_ACT_COMP_VAR_ORDERID>-GJPER = LS_PERIOD-GJPER.
* -B RWU- Unit unify with CSSL 2008-11-18
      PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_VAR_ORDERID>.
* -E RWU-                                    .
    ENDLOOP.
    APPEND LINES OF LT_ACT_COMP_VAR_ORDERID TO GT_ACT_COMP_VAR_ORDERID.
    REFRESH LT_ACT_COMP_VAR_ORDERID.

* append reversals of variances (activities) and orderid to itab
    LOOP AT LT_REV_ACT_COMP_VAR_ID ASSIGNING <LFS_ACT_COMP_VAR_ORDERID>  .
      <LFS_ACT_COMP_VAR_ORDERID>-ORDERID = LS_PPC_HEAD-ORDERID.
      <LFS_ACT_COMP_VAR_ORDERID>-GJPER = LS_PERIOD-GJPER.
* -B RWU- Unit unify with CSSL 2008-11-18
      PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <LFS_ACT_COMP_VAR_ORDERID>.
* -E RWU-                                .
    ENDLOOP.
    APPEND LINES OF LT_REV_ACT_COMP_VAR_ID TO GT_ACT_REV_VAR_ORDERID.
    REFRESH LT_REV_ACT_COMP_VAR_ID.

*** Typical Routine ---------------------------------------
* receive data
    READ TABLE GT_TASK_SUB WITH KEY NAME = TASKNAME.
    IF SY-SUBRC = 0.    "Register data
      LV_TABIX = SY-TABIX.
      GT_TASK_SUB-STATUS = 'I'.
      MODIFY GT_TASK_SUB INDEX LV_TABIX.
      CLEAR LV_TABIX.
    ENDIF.

  ENDIF.

ENDFORM.                    " RETURN_DATA_SUB
*&---------------------------------------------------------------------*
*&      Form  DETERMIN_WIP_CREDIT_PARALLEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PPC_HEAD_SMALL  text
*      -->P_LT_PERIODS  text
*      -->P_IS_PARALLEL  text
*      -->P_IV_WPSSUB  text
*      <--P_LT_COMP_METHOD_CREDIT  text
*      <--P_LT_ACT_METHOD_CREDIT  text
*----------------------------------------------------------------------*
FORM DETERMIN_WIP_CREDIT_PARALLEL  USING IT_PPC_HEAD_SMALL TYPE TT_PPC_HEAD_SMALL
                                         IT_PERIODS TYPE TT_PERIOD
                                         IS_PARALLEL TYPE PPC_PARALLEL
                                         IV_WPSSUB TYPE INT4
                                CHANGING ET_COMP_METHOD_CREDIT TYPE TT_COMP_METHOD
                                         ET_ACT_METHOD_CREDIT TYPE TT_ACT_METHOD
                                         .

  DATA: FTAB_MATERIAL TYPE STANDARD TABLE OF
        PPC_MATERIAL_COMPONENTS,
        FTAB_ACTIVITY TYPE STANDARD TABLE OF
        PPC_ACTIVITY_COMPONENTS.

  DATA: LT_ACT_WIP_CREDIT TYPE STANDARD TABLE OF
        GT_ACT_COMP_ORDERID_TYP,
        LT_COMP_WIP_CREDIT TYPE STANDARD TABLE OF
        GT_MAT_COMP_ORDERID_TYP.
*----------------------------------------------------------------------*
* table with components quantities (WIP-CREDIT)
  DATA: LT_COMP_WIP_CREDIT_FINAL TYPE	STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,

* table with activities quantities  (WIP-CREDIT)
        LT_ACT_WIP_CREDIT_FINAL TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,

* table with activities quantities with ACT_OBJNR (KALNR) (WIP-CREDIT)
        LT_ACT_WIP_CREDIT_OBJNR TYPE STANDARD TABLE OF GT_ACT_COMP_OBJNR_TYP.

  DATA DUMMY_T_REPPOINT TYPE PPC_T_REPPOINT.

  FIELD-SYMBOLS: <FS_ACT_WIP_CREDIT> TYPE GT_ACT_COMP_ORDERID_TYP,
                 <FS_COMP_WIP_CREDIT> TYPE GT_MAT_COMP_ORDERID_TYP,
                 <FS_PERIOD> TYPE TS_PERIOD.

  DATA: LV_TASK_CNT(4) TYPE N.
  DATA: L_INDEX       LIKE SY-TABIX.

* Task List Create
  DO IV_WPSSUB TIMES.
    LV_TASK_CNT = LV_TASK_CNT + 1.
    CONCATENATE 'READ_CREDIT' LV_TASK_CNT INTO GT_TASK_SUB-NAME.
    GT_TASK_SUB-CLASSNAME = IS_PARALLEL-SERVERGROUP.
    GT_TASK_SUB-STATUS = 'I'.
    APPEND GT_TASK_SUB.
    CLEAR GT_TASK_SUB.
  ENDDO.


  LOOP AT IT_PERIODS ASSIGNING <FS_PERIOD>.
    LOOP AT IT_PPC_HEAD_SMALL ASSIGNING <FS_PPC_HEAD_SMALL> WHERE GJPER = <FS_PERIOD>-GJPER.

      DO.
        READ TABLE GT_TASK_SUB WITH KEY STATUS = 'I'.

        IF SY-SUBRC = 0.
          L_INDEX = SY-TABIX.

          IF NOT <FS_PPC_HEAD_SMALL>-REPPOINT IS INITIAL.
*        CALL FUNCTION 'PPC1DM_COMP_ORD_PART_GET' "Commentated by Sung-Kon Kim 2010.09.13
            CALL FUNCTION 'ZSAPBF_PPC1_COMP_ORD_PART_GET' "revoked by Sung-Kon Kim 2010.09.13
                  STARTING NEW TASK GT_TASK_SUB-NAME
                  DESTINATION IN GROUP GT_TASK_SUB-CLASSNAME
                  PERFORMING RETURN_DATA_CREDIT ON END OF TASK
                EXPORTING
                  IF_ORDERID                   = <FS_PPC_HEAD_SMALL>-ORDERID
                  IF_HEADQUANT                 = <FS_PPC_HEAD_SMALL>-CONFQUANT
                  IF_REPPOINT                  = <FS_PPC_HEAD_SMALL>-REPPOINT
                  I_DATE_LOW                   = '19000101' "revoked 2010.09.13 / Changed by Sung-Kon Kim 2010.01.15
                  I_DATE_HIGH                  = <FS_PERIOD>-ENDDAY "revoked by Sung-Kon Kim 2010.09.13

***** Important Remark; by Sung Kon James Kime 2011/01/25 ********************************************
* The period should be between '19000101' and The end of the periods (posting period + current period)
*-----------------------------------------------------------------------------------------------------
* Above 1 line; from <fs_period>-startday to '19000101' Changed by James Kim 2011/01/25
* because although so many months ago,
* all previous period's confirmation components/activities should be included,
* When calculation of the fields "GMSUM, XMSUM".
* If the i_date_low is initial, then the 'ZSAPBF_PPC1_COMP_ORD_PART_GET' will act incorrectly,
* due to stupid selection option processing within the function module.
******************************************************************************************************
*               IMPORTING
*                 et_material_components       = ftab_material
*                 et_activity_components       = ftab_activity
               EXCEPTIONS
                COMMUNICATION_FAILURE = 1
                SYSTEM_FAILURE        = 2
                RESOURCE_FAILURE      = 3
                GET_COMP_ERROR        = 4
                INPUT_ERROR           = 5
                NO_RP_FROM_BW         = 6
                OTHERS                = 7
                        .
          ELSE. " This routine does not make sence.
            " Because the GR can be performed with reporting point/orderid.
            " Explained by James Kim 2011/01/25
            CALL FUNCTION 'ZSAPBF_PPC1_COMP_ORD_READ_NEW'
                  STARTING NEW TASK GT_TASK_SUB-NAME
                  DESTINATION IN GROUP GT_TASK_SUB-CLASSNAME
                  PERFORMING RETURN_DATA_CREDIT ON END OF TASK
              EXPORTING
                IF_ORDERID                   = <FS_PPC_HEAD_SMALL>-ORDERID
                IT_REPPOINTS                 = DUMMY_T_REPPOINT
                I_DATE_LOW                   = <FS_PERIOD>-STARTDAY
                I_DATE_HIGH                  = <FS_PERIOD>-ENDDAY
*              IMPORTING
*                et_material_components       = ftab_material
*                et_act_comp                  = ftab_activity
              EXCEPTIONS
                COMMUNICATION_FAILURE = 1
                SYSTEM_FAILURE        = 2
                RESOURCE_FAILURE      = 3
                LOCK_ERROR            = 4
                OTHERS                = 5
                      .
          ENDIF.

          IF SY-SUBRC <> 0.
            WAIT UNTIL GV_RCV_SUB >= GV_SND_SUB UP TO 1 SECONDS.
          ELSE.
            READ TABLE GT_TASK_SUB INDEX L_INDEX.
            GT_TASK_SUB-STATUS = 'W'.
            MODIFY GT_TASK_SUB INDEX L_INDEX.
*           send data
            GV_SND_SUB = GV_SND_SUB + 1.
            EXIT.
          ENDIF.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.

      ENDDO.
*    ENDIF.

      REFRESH LT_ACT_WIP_CREDIT.
    ENDLOOP.
  ENDLOOP.

  PERFORM GET_ACT_OBJNR USING LT_ACT_WIP_CREDIT_FINAL
                     CHANGING LT_ACT_WIP_CREDIT_OBJNR.

  PERFORM COMPRESS_WIP USING LT_ACT_WIP_CREDIT_OBJNR
                             LT_COMP_WIP_CREDIT_FINAL
                    CHANGING ET_ACT_METHOD_CREDIT
                             ET_COMP_METHOD_CREDIT
                             .
  SORT ET_COMP_METHOD_CREDIT BY REPPOINT COSTING_NUM MAT_NUMBER ORDERID
                                COST_CENTER ACTIVITY_TYPE.
  SORT ET_ACT_METHOD_CREDIT BY REPPOINT RESSOURCE_GUID ACT_OBJNR
                               DURUNIT ORDERID COST_CENTER ACTIVITY_TYPE.

ENDFORM.                    " DETERMIN_WIP_CREDIT_PARALLEL
*&---------------------------------------------------------------------*
*&      Form  RETURN_DATA_CREDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RETURN_DATA_CREDIT USING TASKNAME.

  DATA: LV_TABIX TYPE SYTABIX.

  DATA: FTAB_MATERIAL TYPE STANDARD TABLE OF
                      PPC_MATERIAL_COMPONENTS,
        FTAB_ACTIVITY TYPE STANDARD TABLE OF
                      PPC_ACTIVITY_COMPONENTS.

  DATA: LT_ACT_WIP_CREDIT TYPE STANDARD TABLE OF
        GT_ACT_COMP_ORDERID_TYP,
        LT_COMP_WIP_CREDIT TYPE STANDARD TABLE OF
        GT_MAT_COMP_ORDERID_TYP.
*----------------------------------------------------------------------*
* table with components quantities (WIP-CREDIT)
  DATA: LT_COMP_WIP_CREDIT_FINAL TYPE	STANDARD TABLE OF GT_MAT_COMP_ORDERID_TYP,

* table with activities quantities  (WIP-CREDIT)
        LT_ACT_WIP_CREDIT_FINAL TYPE STANDARD TABLE OF GT_ACT_COMP_ORDERID_TYP,

* table with activities quantities with ACT_OBJNR (KALNR) (WIP-CREDIT)
        LT_ACT_WIP_CREDIT_OBJNR TYPE STANDARD TABLE OF GT_ACT_COMP_OBJNR_TYP.


  DATA: LS_PPC_ORD_INF TYPE PPC_ORD_INF,
        LS_PERIOD TYPE TS_PERIOD.

  RECEIVE RESULTS FROM FUNCTION 'ZSAPBF_PPC1_COMP_ORD_PART_GET'
       IMPORTING
           ET_MATERIAL_COMPONENTS   = FTAB_MATERIAL
           ET_ACTIVITY_COMPONENTS   = FTAB_ACTIVITY
       CHANGING
           CF_ORDERID               = LS_PPC_ORD_INF-ORDERID
           C_GJPER                  = LS_PERIOD-GJPER
       EXCEPTIONS
           COMMUNICATION_FAILURE    = 1
           SYSTEM_FAILURE           = 2
           RESOURCE_FAILURE         = 3
           GET_COMP_ERROR           = 4
           INPUT_ERROR              = 5
           NO_RP_FROM_BW            = 6
           OTHERS                   = 7
           .

  GV_RCV_SUB = GV_RCV_SUB + 1.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.

    APPEND LINES OF FTAB_MATERIAL TO LT_COMP_WIP_CREDIT.
    APPEND LINES OF FTAB_ACTIVITY TO LT_ACT_WIP_CREDIT.
* substract variances (components), add orderid and append to itab
    LOOP AT LT_COMP_WIP_CREDIT ASSIGNING <FS_COMP_WIP_CREDIT>.
      <FS_COMP_WIP_CREDIT>-ORDERID = <FS_PPC_HEAD_SMALL>-ORDERID.
      <FS_COMP_WIP_CREDIT>-GJPER = <FS_PERIOD>-GJPER.

      <FS_COMP_WIP_CREDIT>-PPC_MAT-QUANTITY =
      <FS_COMP_WIP_CREDIT>-PPC_MAT-QUANTITY -
      <FS_COMP_WIP_CREDIT>-PPC_MAT-DELTA_QUANTITY.
    ENDLOOP.
    APPEND LINES OF LT_COMP_WIP_CREDIT TO GT_COMP_WIP_CREDIT_FINAL.
    REFRESH LT_COMP_WIP_CREDIT.

* substract variances (activities), add orderid and append to itab
    LOOP AT LT_ACT_WIP_CREDIT ASSIGNING <FS_ACT_WIP_CREDIT>.
      <FS_ACT_WIP_CREDIT>-ORDERID = <FS_PPC_HEAD_SMALL>-ORDERID.
      <FS_ACT_WIP_CREDIT>-GJPER = <FS_PERIOD>-GJPER.
* -B RWU- Unit unify with CSSL 2008-11-18
      PERFORM CSSL_UNIT_UNIFY_FOR_ACTCOMP CHANGING <FS_ACT_WIP_CREDIT>.

      <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_VAR =
      <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_VAR -
      <FS_ACT_WIP_CREDIT>-PPC_ACT-DELTA_DUR_VAR.
      <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_FIX =
      <FS_ACT_WIP_CREDIT>-PPC_ACT-DURATION_FIX -
      <FS_ACT_WIP_CREDIT>-PPC_ACT-DELTA_DUR_FIX.
    ENDLOOP.
    APPEND LINES OF LT_ACT_WIP_CREDIT TO GT_ACT_WIP_CREDIT_FINAL.

*** Typical Routine ---------------------------------------
* receive data
    READ TABLE GT_TASK_SUB WITH KEY NAME = TASKNAME.
    IF SY-SUBRC = 0.    "Register data
      LV_TABIX = SY-TABIX.
      GT_TASK_SUB-STATUS = 'I'.
      MODIFY GT_TASK_SUB INDEX LV_TABIX.
      CLEAR LV_TABIX.
    ENDIF.
  ENDIF.

ENDFORM.                    " RETURN_DATA_CREDIT
