*&---------------------------------------------------------------------*
*& Report  ZACO11R_ML03_PAR
*& Shop Summary - Cost Component break down
*&  P_PR_CNT : how many line item is assigned 1 Task
*&  P_TA_CNT : how many task is process in parallelly
*&  P_WT_SCN : how many time would wait until starting next task
*&  Call Z_ML03_PARALLEL (This function submit original Material
*&  ledger creation program ZACO11R_ML03_NEW )
*&---------------------------------------------------------------------*
REPORT zaco11r_ml03_par .
TABLES : macku, mbew, mara.
DATA : g_count(5) TYPE n.

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

DATA: BEGIN OF t_mats OCCURS 0,
     kalnr TYPE ckmlhd-kalnr,
     matnr TYPE ckmlhd-matnr,
     bwkey TYPE ckmlhd-bwkey,
     bwtar TYPE ckmlhd-bwtar,
*     sobkz TYPE ckmlhd-sobkz,
*     vbeln TYPE ckmlhd-vbeln,
*     posnr TYPE ckmlhd-posnr,
*     pspnr TYPE ckmlhd-pspnr,
     mtart TYPE mara-mtart,
     matkl TYPE mara-matkl,
     spart TYPE mara-spart,
*    prctr TYPE marc-prctr,
     meins TYPE mara-meins,
     bklas TYPE mbew-bklas,   "val class
     lfgja TYPE mbew-lfgja,
     lfmon TYPE mbew-lfmon,
     maktg TYPE makt-maktg,
     stprs TYPE mbew-stprs,
     verpr TYPE mbew-verpr,

     salk3 TYPE mbew-salk3,
*    lbkum TYPE mbew-lbkum,
     ABKUMO   like ckmlpp-ABKUMO,    "Begin
     UMKUMO   like ckmlpp-UMKUMO,    "Prev Posting
     ZUKUMO   like ckmlpp-ZUKUMO,    "GR
     VNKUMO   like ckmlpp-VNKUMO,    "GI
     LBKUM    like ckmlpp-LBKUM ,    "End
     EKKUMO   like ckmlpp-EKKUMO,    "PO GR
     END OF t_mats.

DATA: t_mats_temp LIKE t_mats OCCURS 0 WITH HEADER LINE.
DATA: t_mats_this LIKE t_mats OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_mlit OCCURS 0,
        bukrs     LIKE ztco_mlit-bukrs,
        bdatj     LIKE ztco_mlit-bdatj,
        poper     LIKE ztco_mlit-poper,
        bklas     LIKE ztco_mlit-bklas,
        matnr     LIKE ztco_mlit-matnr,
        kalnr     LIKE ztco_mlit-kalnr,
        bwkey     LIKE ztco_mlit-bwkey,
        mtart     LIKE ztco_mlit-mtart,
        bwtar     LIKE ztco_mlit-bwtar,
*        waers     LIKE ztco_mlit-waers,
        meins     LIKE ztco_mlit-meins,
*  Beginning
        ab_lbkum  LIKE ztco_mlit-ab_lbkum,
        ab_salk3  LIKE ztco_mlit-ab_salk3,
        ab_rd     LIKE ztco_mlit-ab_rd,
*  Ending
        eb_lbkum  LIKE ztco_mlit-eb_lbkum,
        eb_salk3  LIKE ztco_mlit-eb_salk3,
        eb_rd     LIKE ztco_mlit-eb_rd,
     END OF it_mlit.

DATA :it_ztco_mlit LIKE ztco_mlit OCCURS 0 WITH HEADER LINE.
************************************************************************
* SELECTION SCREEN ( SELECT-OPTIONS & PARAMETERS )
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_kokrs LIKE csks-kokrs     MEMORY ID cac  OBLIGATORY
                                         VALUE CHECK,
             p_bdatj LIKE mlcd_key-bdatj MEMORY ID bdtj OBLIGATORY,
             p_perab LIKE mlcd_key-poper MEMORY ID popr OBLIGATORY,
             p_curtp LIKE mlcd-curtp     DEFAULT '10'   OBLIGATORY.

SELECT-OPTIONS : s_matnr FOR macku-matnr MEMORY ID mat.
select-options : s_bklas for mbew-bklas.
select-options : s_mtart for mara-mtart.
SELECTION-SCREEN END OF BLOCK bl1.
PARAMETERS: p_ml AS CHECKBOX default 'X'.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS :  p_pr_cnt  TYPE i DEFAULT 25,
              p_ta_cnt  TYPE i DEFAULT 10,
              p_wt_scn  TYPE i DEFAULT 5.
SELECTION-SCREEN END OF BLOCK bl2.


************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.

  PERFORM get_ml03_data .

  PERFORM call_paralle_function.

  PERFORM get_ztco_mlit_previous_data.

*&---------------------------------------------------------------------*
*&      Form  select_ML03_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ml03_data .
  DATA : l_index(2) TYPE n.

* Read Base Information
  PERFORM get_materials_from_closing.

  DESCRIBE TABLE t_mats_temp LINES line_cnt.
  SORT t_mats_temp.

  t_mats_this[] = t_mats_temp[] .
* define working task
  DO p_ta_cnt TIMES.
    l_index = sy-index.

    CONCATENATE 'ML03P_' l_index INTO it_task-name.
    it_task-status = 'I'.

    APPEND it_task. CLEAR it_task.
  ENDDO.


ENDFORM.                    " select_ML03_DATA
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

  CHECK NOT t_mats_temp[] IS INITIAL.
  SORT t_mats_temp BY mtart matnr.

  clear g_count .

  DO.
    g_count = g_count + 1.
    it_return-times = g_count.
    APPEND it_return.


    REFRESH t_mats.
    APPEND LINES OF t_mats_temp FROM l_start TO l_end TO t_mats.

    DELETE t_mats_temp FROM l_start TO l_end.
    READ TABLE t_mats INDEX 1.


* Call function for submit real program.
    PERFORM call_rfc_function TABLES t_mats
                              USING sy-index.

    IF t_mats_temp[] IS INITIAL.
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
FORM call_rfc_function  TABLES   pt_mats  STRUCTURE t_mats
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
      READ TABLE pt_mats INDEX 1.
      CONCATENATE it_task-name pt_mats-kalnr pt_mats-matnr g_count
                  it_task-status
                  '...started' INTO l_text SEPARATED BY space.
      PERFORM progress_ind USING '10' l_text.

      CALL FUNCTION 'Z_ML03_PARALLEL'
         STARTING NEW TASK it_task-name
         DESTINATION IN GROUP ''
         PERFORMING get_result ON END OF TASK
         EXPORTING
            p_kokrs     = p_kokrs
            p_bdatj     = p_bdatj
            p_perab     = p_perab
            p_curtp     = p_curtp
         TABLES
            t_mats      = pt_mats
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

  ENDDO.
  REFRESH : pt_mats.
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

  DATA : tt_mats LIKE t_mats OCCURS 0 WITH HEADER LINE.
* Get Result
  RECEIVE RESULTS FROM FUNCTION 'Z_ML03_PARALLEL'
     IMPORTING
        subrc  = l_subrc
        artnr  = l_artnr
     TABLES
        t_mats =  tt_mats
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
    ELSEIF l_subrc = 6.
      l_err   = 'Insert error'.
    ENDIF.

    IF l_artnr IS INITIAL.
      READ TABLE tt_mats INDEX 1.
      CONCATENATE tt_mats-matnr l_subrc
                  l_err INTO l_text SEPARATED BY space.
    ELSE.
      CONCATENATE l_artnr l_subrc
                  l_err INTO l_text SEPARATED BY space.
    ENDIF.
    PERFORM progress_ind USING '90' l_text.
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
*&      Form  get_materials_from_closing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_materials_from_closing.
  TABLES: t001k.
  RANGES: r_bwkey FOR t001k-bwkey.
  r_bwkey-sign   = 'I'.
  r_bwkey-option = 'EQ'.
  REFRESH: t_mats.
  SELECT * FROM t001k
         WHERE bukrs = p_kokrs.
    r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
  ENDSELECT.

  DATA: l_abrechdat LIKE ckmlhd-abrechdat.

  IF p_ml = space.
    SELECT mbew~bwkey mbew~matnr mbew~bwtar
           makt~maktg
           ckmlhd~kalnr
*        ckmlhd~sobkz ckmlhd~vbeln ckmlhd~posnr ckmlhd~pspnr
           mara~mtart mara~matkl mbew~bklas
           ckmlcr~stprs
           ckmlcr~pvprs AS verpr
           mara~spart mara~meins
           mbew~lfgja mbew~lfmon
           ckmlpp~abkumo ckmlpp~umkumo ckmlpp~zukumo
           ckmlpp~vnkumo ckmlpp~lbkum  ckmlpp~ekkumo
       INTO CORRESPONDING FIELDS OF TABLE t_mats_temp
             FROM ( mbew
                    INNER JOIN mara
                      ON  mbew~matnr = mara~matnr
                    INNER JOIN makt
                      ON  makt~matnr = mara~matnr
                     AND  makt~spras = sy-langu
                    INNER JOIN marc
                      ON  mbew~matnr = marc~matnr
                      AND mbew~bwkey = marc~werks
*                  INNER JOIN ckmlmv011
*                    ON  ckmlmv011~matnr = mbew~matnr
*                    AND ckmlmv011~bwkey = mbew~bwkey
*                  INNER JOIN ckmlrunperiod
*                    ON  ckmlmv011~laufid = ckmlrunperiod~run_id
                    INNER JOIN ckmlcr
                      ON  ckmlcr~kalnr = mbew~kaln1
                     AND  ckmlcr~bdatj = p_bdatj
                     AND  ckmlcr~poper = p_perab
                     AND  ckmlcr~curtp = p_curtp
                    INNER JOIN ckmlpp
                      ON  ckmlpp~kalnr  = ckmlcr~kalnr
                     AND  ckmlpp~bdatj  = p_bdatj
                     AND  ckmlpp~poper  = p_perab
                    INNER JOIN ckmlhd
                      ON  ckmlhd~kalnr = mbew~kaln1 )
             WHERE mbew~bwkey IN r_bwkey
               AND mbew~bklas IN s_bklas
               AND mara~mtart IN s_mtart
               AND mbew~matnr IN s_matnr
               AND ckmlhd~abrechdat <> l_abrechdat
               AND ckmlcr~untper = space
               AND ckmlpp~untper = space
               AND (   ckmlpp~zukumo <> 0 OR ckmlpp~vnkumo <> 0
                    OR ckmlpp~abkumo <> 0 OR ckmlpp~umkumo <> 0
                    OR ckmlpp~lbkum <> 0 )
      ORDER BY ckmlhd~kalnr.
  ELSE.

    SELECT mbew~bwkey mbew~matnr mbew~bwtar
           makt~maktg
           ckmlhd~kalnr
*        ckmlhd~sobkz ckmlhd~vbeln ckmlhd~posnr ckmlhd~pspnr
           ckmlmv011~mtart ckmlmv011~matkl ckmlmv011~bklas
           ckmlcr~stprs
           ckmlcr~pvprs AS verpr
           mara~spart mara~meins
           mbew~lfgja mbew~lfmon
           ckmlpp~abkumo ckmlpp~umkumo ckmlpp~zukumo
           ckmlpp~vnkumo ckmlpp~lbkum  ckmlpp~ekkumo
       INTO CORRESPONDING FIELDS OF TABLE t_mats_temp
             FROM ( mbew
                    INNER JOIN mara
                      ON  mbew~matnr = mara~matnr
                    INNER JOIN makt
                      ON  makt~matnr = mara~matnr
                     AND  makt~spras = sy-langu
                    INNER JOIN marc
                      ON  mbew~matnr = marc~matnr
                      AND mbew~bwkey = marc~werks
                    INNER JOIN ckmlmv011
                      ON  ckmlmv011~matnr = mbew~matnr
                      AND ckmlmv011~bwkey = mbew~bwkey
                    INNER JOIN ckmlrunperiod
                      ON  ckmlmv011~laufid = ckmlrunperiod~run_id
                    INNER JOIN ckmlcr
                      ON  ckmlmv011~kalnr     = ckmlcr~kalnr
                     AND  ckmlrunperiod~gjahr = ckmlcr~bdatj
                     AND  ckmlrunperiod~poper = ckmlcr~poper
                    INNER JOIN ckmlpp
                      ON  ckmlpp~kalnr  = ckmlcr~kalnr
                     AND  ckmlpp~bdatj  = p_bdatj
                     AND  ckmlpp~poper  = p_perab
                    INNER JOIN ckmlhd
                      ON  ckmlmv011~kalnr = ckmlhd~kalnr )
             WHERE ckmlrunperiod~gjahr = p_bdatj
               AND ckmlrunperiod~poper = p_perab
               AND ckmlmv011~bwkey IN r_bwkey
               AND ckmlmv011~bklas IN s_bklas
               AND ckmlmv011~mtart IN s_mtart
               AND ckmlmv011~matnr IN s_matnr
               AND ckmlhd~abrechdat <> l_abrechdat
               AND ckmlcr~curtp  = '10'
               AND ckmlcr~untper = space
               AND ckmlpp~untper = space
               AND (   ckmlpp~zukumo <> 0 OR ckmlpp~vnkumo <> 0
                    OR ckmlpp~abkumo <> 0 OR ckmlpp~umkumo <> 0
                    OR ckmlpp~lbkum <> 0 )
      ORDER BY ckmlhd~kalnr.

  PERFORM get_extra_data_mbew.
endif.

ENDFORM.                    " get_materials_from_closing
*&---------------------------------------------------------------------*
*&      Form  get_extra_data_mbew
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_extra_data_mbew.
  SELECT mbewh~bwkey mbewh~matnr mbewh~bwtar mbewh~bklas
         mbewh~verpr AS pvprs
         mbewh~stprs mbewh~lfgja mbewh~lfmon
         makt~maktg
         ckmlhd~kalnr ckmlhd~sobkz ckmlhd~vbeln ckmlhd~posnr
         ckmlhd~pspnr
         mara~spart mara~meins mara~mtart mara~matkl
     APPENDING CORRESPONDING FIELDS OF TABLE t_mats_temp
           FROM ( mbewh
                  INNER JOIN mara
                    ON  mbewh~matnr = mara~matnr
                  INNER JOIN makt
                    ON  makt~matnr = mara~matnr
                   AND  makt~spras = sy-langu
                  INNER JOIN ckmlhd
                    ON  mbewh~matnr = ckmlhd~matnr
                   AND  MBEWH~BWKEY = ckmlhd~BWKEY )
           WHERE mbewh~lfgja = p_bdatj
             AND mbewh~lfmon = p_perab
             AND mbewh~bklas IN s_bklas
             AND mbewh~matnr IN s_matnr
             AND mbewh~vprsv = 'V'
    ORDER BY ckmlhd~kalnr.

ENDFORM.                    " get_extra_data_mbew
*&---------------------------------------------------------------------*
*&      Form  get_ztco_mlit_previous_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ztco_mlit_previous_data.
  DATA : l_perab LIKE ztco_mlit-poper.
  DATA : l_bdatj LIKE p_bdatj.

  check s_matnr[] is initial.
  IF p_perab = 12.
    l_bdatj = p_bdatj - 1.
    l_perab = '01'.
  ELSE.
    l_bdatj = p_bdatj .
    l_perab = p_perab - 1.
  ENDIF.

* Previous data select
* (no trasaction material in this month)
  SELECT  matnr INTO CORRESPONDING FIELDS OF TABLE it_mlit
      FROM ztco_mlit
      FOR ALL ENTRIES IN t_mats_this
     WHERE bukrs = p_kokrs
       AND bdatj = l_bdatj
       AND poper = l_perab
       AND matnr <> t_mats_this-matnr
       and matnr <> ''
       and eb_lbkum <> 0.       "except zero inventory

  LOOP AT it_mlit.
    it_ztco_mlit-bukrs     = it_mlit-bukrs.
    it_ztco_mlit-bdatj     = p_bdatj.
    it_ztco_mlit-poper     = p_perab.
    it_ztco_mlit-bklas     = it_mlit-bklas.
    it_ztco_mlit-matnr     = it_mlit-matnr.
    it_ztco_mlit-kalnr     = it_mlit-kalnr.
    it_ztco_mlit-bwkey     = it_mlit-bwkey.
    it_ztco_mlit-mtart     = it_mlit-mtart.
    it_ztco_mlit-bwtar     = it_mlit-bwtar.
*    it_ztco_mlit-waers     = it_mlit-waers.
    it_ztco_mlit-meins     = it_mlit-meins.
*  Beginning
    it_ztco_mlit-ab_lbkum  = it_mlit-ab_lbkum.
    it_ztco_mlit-ab_salk3  = it_mlit-ab_salk3.
    it_ztco_mlit-ab_rd     = it_mlit-ab_rd.

*  Ending
    it_ztco_mlit-eb_lbkum  = it_mlit-eb_lbkum.
    it_ztco_mlit-eb_salk3  = it_mlit-eb_salk3.
    it_ztco_mlit-eb_rd     = it_mlit-eb_rd.

    COLLECT it_ztco_mlit. CLEAR it_ztco_mlit.
  ENDLOOP.

  IF NOT it_ztco_mlit[] IS INITIAL.
    MODIFY ztco_mlit FROM TABLE it_ztco_mlit.

    IF sy-subrc <> 0.
      WRITE:/ 'error during insert(ZTCO_MLIT)'.
    ELSE.
      WRITE:/ 'Data is saved(ZTCO_MLIT)'.
    ENDIF.
  ELSE.
    WRITE:/ 'NO Data to  save(ZTCO_MLIT)'.
  ENDIF.

ENDFORM.                    " get_ztco_mlit_previous_data
