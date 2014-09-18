*&---------------------------------------------------------------------*
*& Report  ZCO_SHOP_PAR
*& Shop Summary - Cost Component break down
*&  P_PR_CNT : how many line item is assigned 1 Task
*&  P_TA_CNT : how many task is process in parallelly
*&  P_WT_SCN : how many time would wait until starting next task
*&  Call Z_SHOP_CC_PARALLEL (This function submit original Shop Cost
*&  Summary program ZACO19U_SHOP_SUMMARY_NEW )
*&---------------------------------------------------------------------*
REPORT zco_shop_summary_par .
TABLES : keko, mara, marc, aufk.
DATA : g_count(5) TYPE n.

TYPE-POOLS:
  ccs00,
  ccs01,
  ckmv0,
  ckmv3,
  vrm,
  ckmd,
  slis,
  ckru0.

* PARAMETER for PROGRESSING
DATA : line_cnt           TYPE i,
       tot_data_cnt(5)    TYPE n, "Total DATA count
       task_cnt(5)        TYPE n, "processing TASK count
       stored_task_cnt(5) TYPE n, "STORED TASK count
       stored_data_cnt(5) TYPE n, "STORED DATA count
       rest_data_cnt(5)   TYPE n. "Remained DATA count

DATA : BEGIN OF it_task OCCURS 0,
       name(20),
       status(1),
       END OF it_task.

DATA : BEGIN OF it_return OCCURS 0,
       times(10),
       return(1),
       END OF it_return.

DATA : it_mat      LIKE zsco_shop_mat OCCURS 0 WITH HEADER LINE.
DATA : it_fsc_mat  LIKE zsco_shop_fsc_mat OCCURS 0 WITH HEADER LINE.
DATA : it_fsc_temp LIKE zsco_shop_fsc_mat OCCURS 0 WITH HEADER LINE.


DATA: tper_kalnr TYPE ckmv0_matobj_str OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF t_ckmlpp OCCURS 10.
        INCLUDE STRUCTURE ckmlpp.
DATA: END OF t_ckmlpp.

DATA: BEGIN OF t_ckmlcr OCCURS 10.
        INCLUDE STRUCTURE ckmlcr.
DATA: END OF t_ckmlcr.


************************************************************************
* SELECTION SCREEN ( SELECT-OPTIONS & PARAMETERS )
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY ID cac  OBLIGATORY.

* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY.
* periods
PARAMETERS : p_perab LIKE covja-perab MEMORY ID vpe
             MODIF ID per OBLIGATORY.
SELECT-OPTIONS : s_matnr FOR keko-matnr,
                 s_mtart FOR mara-mtart,
                 s_werks FOR marc-werks,
                 s_aufnr FOR aufk-aufnr MEMORY ID anr.


SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS :  p_pr_cnt  TYPE i DEFAULT 1,
              p_ta_cnt  TYPE i DEFAULT 5,
              p_wt_scn  TYPE i DEFAULT 5.
SELECTION-SCREEN END OF BLOCK bl2.

PARAMETERS: p_ccs AS CHECKBOX DEFAULT 'X'.

************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.

  PERFORM get_shop_summary .

  PERFORM call_paralle_function.

*&---------------------------------------------------------------------*
*&      Form  select_ext_spec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_shop_summary .
  DATA : l_index(2) TYPE n.

* Read Base Information
  PERFORM read_materials.

  DESCRIBE TABLE it_fsc_temp LINES line_cnt.
  SORT it_fsc_temp.

* define working task
  DO p_ta_cnt TIMES.
    l_index = sy-index.

    CONCATENATE 'SHOP_' l_index INTO it_task-name.
    it_task-status = 'I'.

    APPEND it_task. CLEAR it_task.
  ENDDO.


ENDFORM.                    " select_ext_spec
*&---------------------------------------------------------------------*
*&      Form  call_paralle_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_paralle_function .
  DATA : l_start TYPE i,
         l_end   TYPE i.

  DATA : l_text(100).

* P_PR_CNT : line count for one tasking
  l_start = 1.
  l_end   = p_pr_cnt.

  CHECK NOT it_fsc_temp[] IS INITIAL.
  SORT it_fsc_temp BY mtart matnr.

  DO.
    g_count = g_count + 1.
    it_return-times = g_count.
    APPEND it_return.


    REFRESH it_fsc_mat.
    APPEND LINES OF it_fsc_temp FROM l_start TO l_end TO it_fsc_mat.

    DELETE it_fsc_temp FROM l_start TO l_end.
    READ TABLE it_fsc_mat INDEX 1.

* Call function for submit real program.
    PERFORM call_rfc_function TABLES it_mat
                                     it_fsc_mat
                              USING sy-index.

    IF it_fsc_temp[] IS INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

* Finish if no working Job
  DO.
    READ TABLE it_task WITH KEY status = 'W'.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    WAIT UP TO 5 SECONDS.
  ENDDO.


ENDFORM.                    " call_paralle_function
*&---------------------------------------------------------------------*
*&      Form  CALL_RFC_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXT  text
*      -->P_SY_INDEX  text
*----------------------------------------------------------------------*
FORM call_rfc_function  TABLES   pt_mat     STRUCTURE it_mat
                                 pt_fsc_mat STRUCTURE it_fsc_mat
                        USING    p_times LIKE sy-index.

  DATA : msg_txt(80),
         l_index  TYPE sy-tabix,
         l_text(100),
         l_serial TYPE i ,
         l_subrc  TYPE subrc.

  DATA : l_line(2) TYPE n .
  DATA : l_matnr(50).


  DO.
* find working idle task
    READ TABLE it_task WITH KEY status = 'I'.
    IF sy-subrc = 0 .
      l_index = sy-tabix.
      READ TABLE pt_fsc_mat INDEX 1.
      CONCATENATE it_task-name pt_fsc_mat-matnr g_count it_task-status
                  '...started' INTO l_text SEPARATED BY space.
      PERFORM progress_ind USING '10' l_text.

      CALL FUNCTION 'Z_SHOP_CC_PARALLEL'
         STARTING NEW TASK it_task-name
         DESTINATION IN GROUP ''
         PERFORMING get_result ON END OF TASK
         EXPORTING
            p_kokrs     = p_kokrs
            p_bdatj     = p_bdatj
            p_perab     = p_perab
            p_ccs       = p_ccs
         TABLES
            it_mat      = pt_mat
            it_fsc_mat  = pt_fsc_mat
         EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            resource failure      = 3.

      IF sy-subrc <> 0.  "Don't assign JOB in Error TASK
        it_task-status = 'E'.
        MODIFY it_task INDEX l_index.
      ELSE.
        it_task-status = 'W'.
        MODIFY it_task INDEX l_index.
      ENDIF.
      EXIT.
    ELSE.
      WAIT UP TO p_wt_scn SECONDS.  "waiting
    ENDIF.


*    IF sy-subrc <> 0.
*      l_subrc = sy-subrc.
*      CONCATENATE pt_fsc_mat-matnr g_count l_subrc
*                  '...Error' INTO l_text SEPARATED BY space.
*      PERFORM progress_ind USING '50' l_text.
*    ENDIF.
  ENDDO.
  REFRESH : pt_fsc_mat.
ENDFORM.                    " CALL_RFC_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXT  text
*      -->P_=  text
*      -->P_PT_EXT  text
*      -->P_EXCEPTION  text
*      -->P_COMMUNICATION_FAILURE  text
*      -->P_=  text
*      -->P_1  text
*      -->P_SYSTEM_FAILURE  text
*      -->P_=  text
*      -->P_2  text
*      -->P_RESOURCE  text
*      -->P_FAILURE  text
*      -->P_=  text
*      -->P_3  text
*----------------------------------------------------------------------*
FORM get_result  USING task_name.

  DATA : it_error TYPE zsco_shop_mat OCCURS 0 WITH HEADER LINE,
         l_times  LIKE sy-index.
  DATA : l_subrc TYPE subrc,
         l_artnr(50),
         l_text(50),
         l_err(25).

  DATA : tt_fsc_mat LIKE it_fsc_mat OCCURS 0 WITH HEADER LINE.
* Get Result
  RECEIVE RESULTS FROM FUNCTION 'Z_SHOP_CC_PARALLEL'
     IMPORTING
        subrc  = l_subrc
        artnr  = l_artnr
     TABLES
        it_fsc_mat = tt_fsc_mat
*       it_mat     = it_mat
       EXCEPTIONS
           communication_failure = 1
           system_failure        = 2 .


  IF l_subrc <> 0 .
    IF l_subrc = 1.
      l_err   = 'Arithmetic_errors'.
    ELSEIF l_subrc = 2.
      l_err   = 'Export_buffer_no_memory'.
    ELSEIF l_subrc = 3.
      l_err   = 'Rmc_communication_failure'.
    ELSEIF l_subrc = 4.
      l_err   = 'Rmc_invalid_status'.
    ELSEIF l_subrc = 5.
      l_err   = 'Rmc_system_failure'.
    ELSE.  "IF l_subrc = 6.
      l_err   = 'Parallel Error-Other'.
    ENDIF.

    IF l_artnr IS INITIAL.
      READ TABLE tt_fsc_mat INDEX 1.
      CONCATENATE tt_fsc_mat-matnr l_subrc
                  l_err INTO l_text SEPARATED BY space.
    ELSE.
      CONCATENATE l_artnr l_subrc
                  l_err INTO l_text SEPARATED BY space.
    ENDIF.
    PERFORM progress_ind USING '99' l_text.
  ENDIF.

  READ TABLE it_return WITH KEY times = l_times.
  IF sy-subrc = 0 .
    it_return-return = 'X'.
    MODIFY it_return INDEX sy-tabix.
  ENDIF.

* TASK RELEASE.
  READ TABLE it_task WITH KEY name = task_name.
  it_task-status = 'I'.
  MODIFY it_task INDEX sy-tabix.


ENDFORM.                    " GET_RESULT



*Selection texts
*---------------
*SP_PR_CNT        Processing Count
*SP_TA_CNT        Task Count
*&---------------------------------------------------------------------*
*&      Form  read_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_materials.
  DATA: $it_fsc_mat LIKE it_fsc_mat OCCURS 0 WITH HEADER LINE.
  DATA: tper_kalnr TYPE ckmv0_matobj_str OCCURS 0 WITH HEADER LINE.

  RANGES: r_bwkey FOR ckmlhd-bwkey.
  TABLES: t001k, tckh4.

* Read Active Cost Component Structure
  CLEAR tckh4.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF tckh4
                  FROM tckh4
                 WHERE elehk = 'H1'
                   AND aktiv = 'X'.

* select materials
  r_bwkey-sign = 'I'. r_bwkey-option = 'EQ'.
  SELECT * FROM t001k WHERE bukrs = p_kokrs.
    r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
  ENDSELECT.

* MARA + MBEW(current period) = MACKU, MARC
  DATA: l_abrechdat LIKE ckmlhd-abrechdat.
  SELECT ckmlmv011~matnr   ckmlmv011~bwkey  ckmlmv011~bklas
         ckmlmv011~kalst   ckmlmv011~error_status
         marc~werks marc~profil marc~sauft marc~beskz marc~sobsl
         marc~vspvb marc~fevor mara~meins mara~mtart
         ckmlhd~kalnr AS kaln1
         ckmlhd~abrechdat
   INTO CORRESPONDING FIELDS OF TABLE  it_mat
           FROM ( ckmlmv011 INNER JOIN mara
                          ON ckmlmv011~matnr = mara~matnr
                       INNER JOIN marc
                          ON ckmlmv011~matnr = marc~matnr
                         AND ckmlmv011~bwkey = marc~werks
                       INNER JOIN ckmlrunperiod
                          ON ckmlmv011~laufid = ckmlrunperiod~run_id
                       INNER JOIN ckmlhd
                          ON ckmlmv011~kalnr = ckmlhd~kalnr )
           WHERE ckmlrunperiod~gjahr = p_bdatj
             AND ckmlrunperiod~poper = p_perab
             AND ckmlmv011~bwkey IN r_bwkey
             AND ckmlhd~abrechdat <> l_abrechdat.

* Read FSC/HALB Mat.
  SORT it_mat BY werks mtart.

  CLEAR : it_fsc_mat, it_fsc_mat[].
  LOOP AT it_mat WHERE matnr IN s_matnr
                   AND mtart IN s_mtart
                   AND bwkey IN s_werks.

*FIXME; is it OK???
*    check it_mat-kalst > 0.
    CHECK it_mat-fevor <> space.

    CLEAR $it_fsc_mat.
    MOVE-CORRESPONDING it_mat  TO $it_fsc_mat .
    APPEND $it_fsc_mat.
  ENDLOOP.

  LOOP AT $it_fsc_mat.
    tper_kalnr-kalnr   = $it_fsc_mat-kaln1.
    tper_kalnr-bwkey   = $it_fsc_mat-bwkey.
    APPEND tper_kalnr.
  ENDLOOP.
  PERFORM read_mlperiods TABLES tper_kalnr
          USING p_bdatj p_perab.

  LOOP AT $it_fsc_mat.
    MOVE-CORRESPONDING $it_fsc_mat  TO it_fsc_temp.
    READ TABLE t_ckmlpp WITH KEY
     kalnr = $it_fsc_mat-kaln1
     bdatj = p_bdatj
     poper = p_perab
     untper = '00'
     BINARY SEARCH.
    PERFORM get_ml_status USING sy-subrc.
    IF sy-subrc = 0.
      APPEND it_fsc_temp.
    ENDIF.
  ENDLOOP.


*sort for binary search
  SORT it_mat BY matnr bwkey bwtar.

  CHECK NOT it_fsc_temp[] IS INITIAL.
  PERFORM read_obj_for_pcc.

ENDFORM.                    " read_materials
*&---------------------------------------------------------------------*
*&      Form  progress_ind
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0400   text
*      -->P_L_TEXT  text
*----------------------------------------------------------------------*
FORM progress_ind USING    p_%
                           p_text.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = p_%
            text       = p_text.


ENDFORM.                    " progress_ind
*&---------------------------------------------------------------------*
*&      Form  read_obj_for_pcc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_obj_for_pcc.
  DATA: BEGIN OF lt_pcc OCCURS 0,
          objnr       LIKE aufk-objnr,
          aufnr       LIKE aufk-aufnr,
          pkosa       LIKE ckmlmv013-pkosa,  "Cost Collector
          kalnr_proc  LIKE ckmlmv013-kalnr_proc,
          prwrk       LIKE ckmlmv013-prwrk,
          pmatn       LIKE ckmlmv013-pmatn,
          verid       LIKE ckmlmv013-verid,
          klvarp      LIKE afko-klvarp,      "CostingVariant-plan
        END OF lt_pcc.


* Create index of CKMLMV013!!!  (MANDT/PRWRK/PMATN)
  SELECT aufk~objnr aufk~aufnr
         ckmlmv013~pkosa   ckmlmv013~kalnr_proc
         ckmlmv013~prwrk   ckmlmv013~pmatn      ckmlmv013~verid
         afko~klvarp
     INTO CORRESPONDING FIELDS OF TABLE lt_pcc
     FROM ckmlmv013
        INNER JOIN aufk
           ON aufk~aufnr   = ckmlmv013~pkosa
        INNER JOIN afko
           ON afko~aufnr   = aufk~aufnr
        INNER JOIN mkal
           ON mkal~matnr   = ckmlmv013~pmatn
     FOR ALL entries IN it_fsc_temp
     WHERE ckmlmv013~prwrk = it_fsc_temp-werks
       AND ckmlmv013~pmatn = it_fsc_temp-matnr
       AND ckmlmv013~loekz = space       "deletion
       AND ckmlmv013~autyp = '05'.       "PCC


  SORT lt_pcc BY pmatn prwrk.
  LOOP AT it_fsc_temp.
    CLEAR lt_pcc.
    READ TABLE lt_pcc WITH KEY pmatn = it_fsc_temp-matnr
                               prwrk = it_fsc_temp-werks
                      BINARY SEARCH.
    IF sy-subrc <> 0 .
      DELETE it_fsc_temp.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " read_obj_for_pcc
*&---------------------------------------------------------------------*
*&      Form  get_ml_status
*&---------------------------------------------------------------------*
FORM get_ml_status  USING r_rc LIKE sy-subrc.

  DATA: ls_material  TYPE  ckml_s_matstatus_single_in.
  DATA: ls_matstatus TYPE  ckml_s_matstatus_single_out.
  CLEAR: ls_material.

  ls_material-kalnr        = it_fsc_temp-kaln1. "ckmlmv011-kalnr.
  ls_material-error_status = it_fsc_temp-error_status.
  ls_material-status       = t_ckmlpp-status.
  ls_material-xabrerr      = t_ckmlpp-xerror.

  CALL FUNCTION 'CKML_RUN_MAT_STATUS_EINZELN'
       EXPORTING
            i_appl       = 'ACRU'  "costing - LCKML_RUN_ALVF01
            is_material  = ls_material
       IMPORTING
            es_matstatus = ls_matstatus.

  IF ls_matstatus-mehrstufig = 4.
    r_rc = 4.
  ELSE.
    r_rc = 0.
  ENDIF.

ENDFORM.                    " get_ml_status
*&---------------------------------------------------------------------*
*&      Form  READ_MLPERIODS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mlperiods TABLES ut_kalnr STRUCTURE tper_kalnr
                    USING ud_gjahr LIKE ckmlpp-bdatj
                          ud_poper LIKE ckmlpp-poper.

  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
  EXPORTING
  i_refresh_buffer          = 'X'
*         I_READ_ONLY_BUFFER        = ' '
  i_use_buffer              = ' '
  i_build_smbew             = ' '
  i_bdatj_1                 = ud_gjahr
  i_poper_1                 = ud_poper
  i_untper                  = '00'  "lf_runperiod-untper
  i_call_by_reporting       = 'X'
  i_no_chk_periods_complete = 'X'
  TABLES
  t_kalnr                   = ut_kalnr
  t_ckmlpp                  = t_ckmlpp
  t_ckmlcr                  = t_ckmlcr
*         T_MISS_CKMLPP             =
*         T_MISS_CKMLCR             =
  EXCEPTIONS
  no_data_found             = 1
  input_data_inconsistent   = 2
  buffer_inconsistent       = 3
  OTHERS                    = 4
  .

  IF sy-subrc <> 0.
    IF t_ckmlpp[] IS INITIAL
    OR t_ckmlcr[] IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  SORT t_ckmlpp BY kalnr bdatj poper untper.
  SORT t_ckmlcr BY kalnr bdatj poper untper curtp.

ENDFORM.                               " READ_MLPERIODS
