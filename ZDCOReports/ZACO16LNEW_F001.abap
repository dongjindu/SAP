*----------------------------------------------------------------------*
***INCLUDE ZACO16L_F001 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_INIT
*&---------------------------------------------------------------------*
*       Default Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_init.
  CLEAR : s_mtart, s_mtart[],
*          s_aufnr, s_aufnr[],
          s_kstar, s_kstar[].

* Material Code
  s_mtart-low    = 'ROH'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  APPEND s_mtart.
  s_mtart-low    = 'ROH1'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  APPEND s_mtart.
  s_mtart-low    = 'HALB'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  APPEND s_mtart.

* internal Order
*  s_aufnr-low    = 'CP001'.
*  s_aufnr-sign   = 'I'.
*  s_aufnr-option = 'EQ'.
*  APPEND s_aufnr.
*  s_aufnr-low    = 'CE001'.
*  s_aufnr-sign   = 'I'.
*  s_aufnr-option = 'EQ'.
*  APPEND s_aufnr.

* Cost element
* All account for I/O 'P001' and 'E001'.
  s_kstar-low    = '0000540000'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  APPEND s_kstar.
  s_kstar-low    = '0000540010'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  APPEND s_kstar.
  s_kstar-low    = '0000540100'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  APPEND s_kstar.
  s_kstar-low    = '0000540110'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  APPEND s_kstar.
  s_kstar-low    = '0000540200'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  APPEND s_kstar.
  s_kstar-low    = '0000540300'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  APPEND s_kstar.
  s_kstar-low    = '0000540400'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  APPEND s_kstar.

ENDFORM.                    " SELECT_INIT

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_tka01.
  CLEAR tka01.
  SELECT SINGLE * FROM tka01
                 WHERE kokrs = p_kokrs.
  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.
ENDFORM.                    " Read_TKA01
*&---------------------------------------------------------------------*
*&      Form  SET_MTYPE_FK
*&---------------------------------------------------------------------*
*       Set Material Type to select materials
*       to be used as allocation Factors
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mtype_fk.
* Only FERT and HALB
  CLEAR : r_fk_mtart, r_fk_mtart[].

  r_fk_mtart-low    = 'FERT'.
  r_fk_mtart-sign   = 'I'.
  r_fk_mtart-option = 'EQ'.
  APPEND r_fk_mtart.
  CLEAR  r_fk_mtart.

  r_fk_mtart-low    = 'HALB'.
  r_fk_mtart-sign   = 'I'.
  r_fk_mtart-option = 'EQ'.
  APPEND r_fk_mtart.
  CLEAR  r_fk_mtart.

ENDFORM.                    " SET_MTYPE_FK

*&---------------------------------------------------------------------*
*&      Form  READ_B_F_DATA
*&---------------------------------------------------------------------*
*       Read B/F data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_b_f_data.
* Progress Ind.
  PERFORM progress_ind USING '30'
                             text-022.
* Read Object Key for PCC order .
  PERFORM read_obj_for_pcc.
* Read B/F data in PCC
  PERFORM read_bf_data_in_pcc.

ENDFORM.                    " READ_B_F_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_OBJ_FOR_PCC
*&---------------------------------------------------------------------*
*       Read Object Key for PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_obj_for_pcc.

* Local DATA definition.
  DATA : it_l_e_vkks0	   LIKE TABLE OF vkks0
                           WITH HEADER LINE.
  DATA : it_l_tmp_pcc_mat  LIKE STANDARD TABLE OF it_pcc_mat
                           WITH HEADER LINE.

  CLEAR : it_pcc_mat, it_pcc_mat[].
  CLEAR : it_l_tmp_pcc_mat, it_l_tmp_pcc_mat[].
  check not it_mard[] is initial.
* INDEX : MARA-T
  SELECT matnr werks fevor
         INTO CORRESPONDING FIELDS OF TABLE it_l_tmp_pcc_mat
         FROM ma61v
          for all entries in it_mard
        WHERE mtart IN  r_fk_mtart
          and matnr = it_mard-matnr
          AND lvorm =  ''
          AND ( beskz = 'E' OR beskz = 'X' )
          AND sobsl <> '50'.

  IF it_l_tmp_pcc_mat[] IS INITIAL.
    MESSAGE e059.
  ENDIF.

* Read PCC orders
  LOOP AT it_l_tmp_pcc_mat.

    CLEAR : it_l_e_vkks0, it_l_e_vkks0[].
    CALL FUNCTION 'KK_F_PKOSA_FIND'
      EXPORTING
        i_matnr                     = it_l_tmp_pcc_mat-matnr
        i_werks                     = it_l_tmp_pcc_mat-werks
        i_pwerk                     = it_l_tmp_pcc_mat-werks
*       I_PROCNR                    = ' '
*       I_SA_AUFNR                  = ' '
*       I_FA_AUFNR                  = ' '
*       I_VERID                     = ' '
*       I_STLAN                     = ' '
*       I_STLAL                     = ' '
*       I_PLNTY                     = ' '
*       I_PLNNR                     = ' '
*       I_PLNAL                     = ' '
*       I_DATE                      = '00000000'
*       I_POPUP                     = ' '
*       I_REM                       = ' '
*       I_INCL_LOEKZ                = ' '
*       I_NO_OLD_PKOSA              = ' '
*     IMPORTING
*       E_PROCNR                    =
*       E_VERID                     =
*       E_STLAN                     =
*       E_STLAL                     =
*       E_PLNTY                     =
*       E_PLNNR                     =
*       E_PLNAL                     =
*       E_AUFNR                     =
      TABLES
        e_vkks0                     = it_l_e_vkks0
*       E_PKOSA                     =
      EXCEPTIONS
        none_found                  = 1
        wrong_input                 = 2
        none_picked                 = 3
        wrong_rule                  = 4
        rsh_not_valid               = 5
        wrong_characteristics       = 6
        no_rule                     = 7
        version_not_valid           = 8
        OTHERS                      = 9.

* if No PCC order, Skip the record .
    IF sy-subrc <> 0.
      DELETE it_l_tmp_pcc_mat.
      CONTINUE.
    ENDIF.
    IF it_l_e_vkks0[]  IS INITIAL .
      DELETE it_l_tmp_pcc_mat.
      CONTINUE.
    ENDIF.

    LOOP AT it_l_e_vkks0.
* Copying Data
      MOVE-CORRESPONDING it_l_tmp_pcc_mat TO it_pcc_mat.
      it_pcc_mat-aufnr = it_l_e_vkks0-aufnr.
      it_pcc_mat-objnr = it_l_e_vkks0-objnr.
* Making ITAB for PCC orders
      COLLECT   it_pcc_mat.
      CLEAR     it_pcc_mat.
      CLEAR     it_l_e_vkks0.
    ENDLOOP.

    CLEAR it_l_tmp_pcc_mat.
  ENDLOOP.

  CLEAR : it_l_tmp_pcc_mat, it_l_tmp_pcc_mat[].
  FREE  : it_l_tmp_pcc_mat.

* Get Production Scheduler
  LOOP AT it_pcc_mat.
    SELECT SINGLE fevor INTO it_pcc_mat-fevor
      FROM marc
     WHERE matnr = it_pcc_mat-matnr
       AND werks = it_pcc_mat-werks.
    IF sy-subrc EQ 0.
      IF NOT ( it_pcc_mat-fevor EQ c_press OR      "Press
               it_pcc_mat-fevor EQ c_blank OR      "Blank
               it_pcc_mat-fevor EQ c_engine ).     "Engine
        CLEAR: it_pcc_mat-fevor.
      ENDIF.
    ENDIF.
    MODIFY it_pcc_mat.
  ENDLOOP.
ENDFORM.                    " READ_OBJ_FOR_PCC

*&---------------------------------------------------------------------*
*&      Form  READ_BF_DATA_IN_PCC
*&---------------------------------------------------------------------*
*       Read B/F data in PCC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bf_data_in_pcc.
* Local Data Definition
  DATA  : it_l_tmp_pcc_coep LIKE STANDARD TABLE OF coep
                            WITH HEADER LINE .
* WKGBTR - Total value in CO area currency
* Index COEP - 1
* !! Do Not change field list after select command
  CLEAR : it_l_tmp_pcc_coep , it_l_tmp_pcc_coep[].
  SELECT
* -> Field List
  mandt kokrs  objnr kstar wkgbtr werks matnr
* OBJNR KSTAR WKGBTR WERKS MATNR
* // Mod. By Hyung Jin Youn 2004.04.16
* Read QTY. From PCC
                megbtr
                mefbtr
                mbgbtr
                mbfbtr
                meinb
* End of Mod.
           INTO CORRESPONDING FIELDS OF TABLE it_l_tmp_pcc_coep
           FROM coep
            FOR ALL ENTRIES IN it_pcc_mat
          WHERE
                kokrs = p_kokrs
            AND lednr = '00'               "Standard Ledger
            AND perio = p_perio
            AND gjahr = p_gjahr
            AND kstar IN s_kstar
            AND objnr =  it_pcc_mat-objnr  "PCC order OBJ
            AND wrttp = p_wrttp
            AND versn = p_versn
            AND matnr <> it_pcc_mat-matnr  "Only for child materials
            AND matnr NE space
            AND werks =  it_pcc_mat-werks
            AND scope = 'PRODT'.           "Production

  IF it_l_tmp_pcc_coep[] IS INITIAL.
    MESSAGE e058.
  ENDIF.

* Collect data
  CLEAR : it_pcc_coep,        it_pcc_coep[].
  LOOP AT it_l_tmp_pcc_coep.
    MOVE-CORRESPONDING it_l_tmp_pcc_coep TO it_pcc_coep.
    COLLECT it_pcc_coep.
    CLEAR   it_pcc_coep.
    CLEAR it_l_tmp_pcc_coep.
  ENDLOOP.

  CLEAR : it_l_tmp_pcc_coep, it_l_tmp_pcc_coep[].
  CLEAR   it_pcc_coep.

* Fill up currency fields with Controlling Area Currency
*  WKGBTR - Total value in CO area currency
  it_pcc_coep-waers = tka01-waers.
  MODIFY it_pcc_coep TRANSPORTING waers WHERE waers EQ space.

*// Mod. by Hyung Jin Youn 2004.04.21
* If some records have minus qty, remove them .
* Those records can not be used as allocation factor.
* (Requested by Functional Team Member)
* Delete QTY ZERO
  DELETE it_pcc_coep
   WHERE mefbtr < 0
     OR  mbgbtr < 0
     OR  mbfbtr < 0.

* End of Mod.
ENDFORM.                    " READ_BF_DATA_IN_PCC

*&---------------------------------------------------------------------*
*&      Form  CAL_COST_RATIO
*&---------------------------------------------------------------------*
* Cal. Total material cost and the ratio for parents materials,
*      The cost ratio of child materials
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_cost_ratio.
*
** Local Data Definition
*  DATA : BEGIN OF IT_L_CHM_RATE OCCURS 0.
*  DATA :  KSTAR  LIKE COEP-KSTAR,
*          MATNR  LIKE COEP-MATNR,
*          WERKS  LIKE COEP-WERKS,
**          OBJNR  LIKE COEP-OBJNR,
*          PCC_WKGBTR LIKE COEP-WKGBTR.
*  DATA : END OF  IT_L_CHM_RATE.
*
*  DATA : BEGIN OF IT_L_TOT_RATE OCCURS 0.
*  DATA :
**          KSTAR  LIKE COEP-KSTAR,
**          MATNR  LIKE COEP-MATNR,
*          WERKS  LIKE COEP-WERKS,
*          OBJNR  LIKE COEP-OBJNR,
*          TOT_WKGBTR LIKE COEP-WKGBTR,
*          KSTAR_RATE LIKE ZTCO_ABISPOST-KSTAR_RATE.
*  DATA : END OF  IT_L_TOT_RATE.
*
*  DATA : BEGIN OF IT_L_PCCSUM OCCURS 0,
*            WERKS  LIKE COEP-WERKS,
*            WK_SUM LIKE COEP-WKGBTR,
*         END OF   IT_L_PCCSUM.
*
** Calculation the cost ratio
*  CLEAR : IT_L_CHM_RATE, IT_L_CHM_RATE[].
*  CLEAR : IT_L_TOT_RATE, IT_L_TOT_RATE[].
*  CLEAR : IT_L_PCCSUM  , IT_L_PCCSUM[]  .
*
*  SORT IT_PCC_COEP BY KSTAR MATNR WERKS OBJNR.
*
*  LOOP AT  IT_PCC_COEP.
** Calculation the total cost by child materials
*    MOVE-CORRESPONDING  IT_PCC_COEP TO  IT_L_CHM_RATE.
*    IT_L_CHM_RATE-PCC_WKGBTR = IT_PCC_COEP-WKGBTR.
*    COLLECT IT_L_CHM_RATE.
*    CLEAR   IT_L_CHM_RATE.
** Calculation the total cost by PCC (Parents materials)
*    MOVE-CORRESPONDING  IT_PCC_COEP TO  IT_L_TOT_RATE.
*    IT_L_TOT_RATE-TOT_WKGBTR = IT_PCC_COEP-WKGBTR.
*    COLLECT IT_L_TOT_RATE.
*    CLEAR   IT_L_TOT_RATE.
** PCC SUM
*    IT_L_PCCSUM-WERKS  = IT_PCC_COEP-WERKS.
*    IT_L_PCCSUM-WK_SUM = IT_PCC_COEP-WKGBTR.
*    COLLECT IT_L_PCCSUM.
*    CLEAR   IT_L_PCCSUM.
*    CLEAR IT_PCC_COEP.
*  ENDLOOP.
*
** Copy to Global
*  CLEAR : IT_TOT_RATE, IT_TOT_RATE[].
*  LOOP AT IT_L_TOT_RATE.
*    CLEAR IT_L_PCCSUM.
*    READ TABLE IT_L_PCCSUM WITH KEY WERKS = IT_L_TOT_RATE-WERKS.
*    IF SY-SUBRC <> 0.
*      CONTINUE.
*    ENDIF.
*    IT_L_TOT_RATE-KSTAR_RATE
*      = IT_L_TOT_RATE-TOT_WKGBTR  / IT_L_PCCSUM-WK_SUM.
*    MODIFY IT_L_TOT_RATE.
** Copy to global Itab
*    MOVE-CORRESPONDING  IT_L_TOT_RATE TO IT_TOT_RATE.
*    APPEND IT_TOT_RATE.
*    CLEAR  IT_TOT_RATE.
*    CLEAR IT_L_TOT_RATE.
*  ENDLOOP.
*
** Sort
*  SORT IT_L_CHM_RATE BY KSTAR MATNR WERKS.
*  CLEAR   IT_L_CHM_RATE.
**
**  RATE_CHILD(16)    LIKE ZTCO_ABISPOST-RATE_CHILD.
*  LOOP AT IT_PCC_COEP.
** ratio (Child)
*    CLEAR   IT_L_CHM_RATE.
*    READ TABLE IT_L_CHM_RATE WITH KEY KSTAR = IT_PCC_COEP-KSTAR
*                                      MATNR = IT_PCC_COEP-MATNR
*                                      WERKS = IT_PCC_COEP-WERKS.
*    IF   SY-SUBRC = 0
*     AND IT_L_CHM_RATE-PCC_WKGBTR NE SPACE.
*      IT_PCC_COEP-RATE_CHILD
*       = IT_PCC_COEP-WKGBTR / IT_L_CHM_RATE-PCC_WKGBTR.
*    ENDIF.
*    MODIFY IT_PCC_COEP.
*    CLEAR  IT_PCC_COEP.
*  ENDLOOP.
*
*  CLEAR  IT_PCC_COEP.
*  CLEAR  IT_TOT_RATE.
*
ENDFORM.                    " CAL_COST_RATIO

*&---------------------------------------------------------------------*
*&      Form  MAKE_PCC_POST_TABLE
*&---------------------------------------------------------------------*
*       Making PCC post data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_pcc_post_table.

* Local Data Definition
  DATA : lv_sum_qty LIKE it_mard-labst.
  DATA : lv_res_qty LIKE it_mard-labst.
  DATA : lw_fevor   LIKE marc-fevor.

* Sort
  SORT it_pcc_coep
                   ASCENDING  BY  kstar matnr stype werks objnr
                   DESCENDING     rate_child.
  SORT it_tot_rate DESCENDING BY  kstar_rate.
  SORT it_mard  BY kstar matnr stype werks .

* Read Objnr for Internal Order

  CLEAR : it_post, it_post[].
  LOOP AT it_mard.
* SUM of Qty (Issued Qty from MM)
    CLEAR : lv_sum_qty.
    CLEAR : it_post.
** Child Mat. rate
    LOOP AT it_pcc_coep WHERE kstar = it_mard-kstar
                          AND matnr = it_mard-matnr
                          AND werks = it_mard-werks
                          AND rate_child <> 0.

*      it_post-waers = it_mard-waers.
      it_post-kstar = it_mard-kstar.
      it_post-matnr = it_mard-matnr.
      it_post-werks = it_mard-werks.
      it_post-stype = it_mard-stype.
      IT_POST-LGORT = IT_MARD-lgort.
*  read PCC order
      CLEAR it_pcc_mat.
      READ TABLE it_pcc_mat WITH KEY objnr  = it_pcc_coep-objnr.
      it_post-pcc_aufnr = it_pcc_mat-aufnr.

      it_post-type = 'Q'.

*  Child rate
      it_post-rate_child = it_pcc_coep-rate_child.

*  Base on Qty Ratio.
*FIXME EA only?
      IF it_mard-meins EQ 'EA'.
        cal_by_qty rate_child.
      ELSE.
        it_post-mbgbtr     = it_mard-labst * it_post-rate_child.
        it_post-chg_wkgbtr = it_mard-labst * it_mard-stprs *
                             it_post-rate_child.
      ENDIF.
*  Unit
      it_post-meinb  = it_mard-meins.

* Check UoM Rounding
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
           EXPORTING
                input      = it_post-mbgbtr
                round_sign = 'X'
                unit_in    = it_mard-meins
                unit_out   = it_mard-meins
           IMPORTING
                output     = it_post-mbgbtr.

* Get Production order
     PERFORM GET_PRODUCT_ORDER changing it_post-io_aufnr.

* Append
      APPEND it_post.
      CLEAR  it_post.
      CLEAR it_pcc_coep.
    ENDLOOP.
** Total PCC Rate
    IF sy-subrc <> 0.
*  Get Production Scheduler
      PERFORM get_parent_production_schedule CHANGING lw_fevor.
      PERFORM make_ppc_post_table_for_no_bf USING lw_fevor.

    ENDIF.
    CLEAR it_mard.
  ENDLOOP.
  CLEAR  it_post.

* Remove Initial Value
  DELETE it_post WHERE chg_wkgbtr EQ 0
                    OR mbgbtr     EQ 0.


ENDFORM.                    " MAKE_PCC_POST_TABLE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA_TO_TABLE
*&---------------------------------------------------------------------*
*       Update the result data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_data_to_table.


  SORT it_post BY kstar io_aufnr pcc_aufnr.
  LOOP AT it_post.
    CLEAR ztco_abispost.
    MOVE-CORRESPONDING it_post TO ztco_abispost.
    ztco_abispost-KSTAR = '0000540300'.
* Period
    ztco_abispost-kokrs  = p_kokrs.
    ztco_abispost-gjahr  = p_gjahr.
    ztco_abispost-period = p_perio.
    ztco_abispost-versn  = p_versn.
* Log
    ztco_abispost-erdat  = sy-datum.
    ztco_abispost-erzet  = sy-uzeit.
    ztco_abispost-ernam  = sy-uname.
*// Mod. by Hyung Jin Youn 2004.04.19
* Read Production version from PCC
    PERFORM read_verid_from_pcc.
*// End Of Mod
    INSERT ztco_abispost.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e045.
    ENDIF.
  ENDLOOP.
* Commit Work
  COMMIT WORK AND WAIT.
  MESSAGE s000 WITH 'The process for reporting was Completed'.
ENDFORM.                    " UPDATE_DATA_TO_TABLE

*&---------------------------------------------------------------------*
*&      Form  enqueue
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue.

  CALL FUNCTION 'ENQUEUE_EZCO_ZTCO_ABISPO'
   EXPORTING
     mode_ztco_abispost       = 'E'
     mandt                    = sy-mandt
     kokrs                    = p_kokrs
     gjahr                    = p_gjahr
     period                   = p_perio
     versn                    = p_versn
*     KSTAR                    =
*     MATNR                    =
*     WERKS                    =
*     IO_AUFNR                 =
*     PCC_AUFNR                =
*     X_KOKRS                  = ' '
*     X_GJAHR                  = ' '
*     X_PERIOD                 = ' '
*     X_VERSN                  = ' '
*     X_KSTAR                  = ' '
*     X_MATNR                  = ' '
*     X_WERKS                  = ' '
*     X_IO_AUFNR               = ' '
*     X_PCC_AUFNR              = ' '
*     _SCOPE                   = '2'
*     _WAIT                    = ' '
*     _COLLECT                 = ' '
   EXCEPTIONS
     foreign_lock             = 1
     system_failure           = 2
     OTHERS                   = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " enqueue

*&---------------------------------------------------------------------*
*&      Form  CONF_REPLACE_DATA
*&---------------------------------------------------------------------*
*       Replacing data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conf_replace_data.
*  IF p_conf <> 'X'.
*    MESSAGE e056.
*  ENDIF.
  DELETE FROM ztco_abispost
                      WHERE kokrs  = p_kokrs
                        AND gjahr  = p_gjahr
                        AND period = p_perio
                        AND versn  = p_versn
                        AND MATNR  IN S_MATNR.
* Do not check subrc.

ENDFORM.                    " CONF_REPLACE_DATA

*&---------------------------------------------------------------------*
*&      Form  CONFIRM_MESSAGE
*&---------------------------------------------------------------------*
*       Show Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm_message.
  IF p_conf = 'X'.
*    DATA : lv_answer.
*    CLEAR  lv_answer.
*    CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*      EXPORTING
*        textline1           = text-030
*        textline2           = text-031
*        titel               = text-032
**       START_COLUMN        = 25
**       START_ROW           = 6
**       DEFAULTOPTION       = 'N'
*      IMPORTING
*        answer              = lv_answer.
*
*    IF lv_answer <> 'J'.
*    MESSAGE w000(zz) WITH text-030.
*    ENDIF.
**    MESSAGE S060 .
  ENDIF.
ENDFORM.                    " CONFIRM_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       Post data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_data.
  CASE 'X'.
    WHEN p_report.
* Do not Post
    WHEN p_frpost.
* Read data from  ZTCO_ABISPOST
      PERFORM read_data_fr_ztco_abispost.
* Posting FM
*      PERFORM post_with_fm.
      PERFORM post_sap.
* Update Doc. No.
      PERFORM update_doc_no.
  ENDCASE.
ENDFORM.                    " POST_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_ZTCO_ABISPOST
*&---------------------------------------------------------------------*
*       Read data from ZTCO_ABISPOST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_fr_ztco_abispost.
* Use IT_POST
  CLEAR : it_post, it_post[].
*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE   it_post
           FROM ztco_abispost
          WHERE kokrs  = p_kokrs
            AND gjahr  = p_gjahr
            AND period = p_perio
            AND versn  = p_versn
            AND matnr  IN s_matnr
            AND KSTAR = '0000540300'.

* Collect data
  CLEAR : it_post_fin, it_post_fin[].
  LOOP AT it_post.
    MOVE-CORRESPONDING it_post TO it_post_fin.
    CLEAR it_post_fin-belnr.
    COLLECT it_post_fin.
    CLEAR   it_post.
  ENDLOOP.

ENDFORM.                    " READ_DATA_FR_ZTCO_ABISPOST

*&---------------------------------------------------------------------*
*&      Form  POST_WITH_FM
*&---------------------------------------------------------------------*
*       Posting FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_with_fm.
* The number of Item Records should not be greater than 200
  DATA : it_post_idx LIKE sy-tabix.
  DATA : lv_mod      LIKE sy-tabix.
  DATA : lv_div      LIKE sy-tabix.
  DATA : lv_from_idx LIKE sy-tabix,
         lv_to_idx   LIKE sy-tabix,
         lv_skip.

* Make one document per 200 records
  SORT it_post_fin BY io_aufnr pcc_aufnr kstar.

* Read the number of Index of "IT_POST"
  DESCRIBE TABLE it_post_fin LINES it_post_idx.
  lv_div = it_post_idx  DIV  200.
  lv_mod = it_post_idx  MOD  200.
  IF lv_mod > 0.
    lv_div = lv_div + 1.
  ENDIF.

* CALL POST FM
  DO lv_div TIMES.
* Check Index of IT_POST
* Cal. MOD. DIV.
    lv_to_idx   =  sy-index * 200 .
    lv_from_idx =  lv_to_idx - 199.
* From
    CLEAR lv_skip.
    CLEAR it_post_fin. READ TABLE it_post_fin INDEX lv_from_idx.
    IF sy-subrc <> 0. lv_skip = 'X'. ENDIF.
* TO
    CLEAR it_post_fin. READ TABLE it_post_fin INDEX lv_to_idx.
    IF sy-subrc <> 0. lv_to_idx = lv_from_idx + lv_mod - 1 . ENDIF.

    IF lv_skip <> 'X'.
* Run Post FM
      PERFORM call_post_fm_res USING  lv_from_idx lv_to_idx .
    ENDIF.
  ENDDO.

ENDFORM.                    " POST_WITH_FM

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM_RES
*&---------------------------------------------------------------------*
*       Run Post FM
*----------------------------------------------------------------------*
*      -->P_LV_FR  From-Index
*      -->P_LV_TO  To-Index
*----------------------------------------------------------------------*
FORM call_post_fm_res  USING    p_lv_fr
                                p_lv_to.
* Local Data definition
  DATA : wa_l_doc_header	LIKE	bapidochdru12p.
  DATA : it_l_doc_items	      LIKE  STANDARD TABLE OF 	bapircitm
                              WITH  HEADER LINE .
  DATA : it_return	      LIKE	STANDARD TABLE OF 	bapiret2
                              WITH  HEADER LINE.
  DATA : lv_doc_no            LIKE	bapidochdru12p-doc_no.

** Header data
  CLEAR wa_l_doc_header.
  wa_l_doc_header-co_area  = p_kokrs.
  wa_l_doc_header-period   = p_perio.
* Posting date - Last date in period
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_perio
       IMPORTING
            e_date  = wa_l_doc_header-postgdate.
  wa_l_doc_header-variant  = 'SAP02'.
  CONCATENATE text-010 sy-repid
            sy-uname sy-datum sy-uzeit
       INTO wa_l_doc_header-doc_hdr_tx
       SEPARATED BY space.
  wa_l_doc_header-username = sy-uname.

* Currency
* All currency in data is controlling area currency = 'USD'
* -> transaction Currency
  wa_l_doc_header-trans_curr = tka01-waers.

** Item DATA
  CLEAR : it_l_doc_items, it_l_doc_items[].
  LOOP AT it_post_fin  FROM p_lv_fr TO  p_lv_to.
    it_l_doc_items-sen_order  = it_post_fin-io_aufnr.
    it_l_doc_items-rec_order  = it_post_fin-pcc_aufnr.
    it_l_doc_items-cost_elem  = it_post_fin-kstar.
    it_l_doc_items-value_tcur = it_post_fin-chg_wkgbtr.
    it_l_doc_items-quantity   = it_post_fin-mbgbtr.
    it_l_doc_items-postquun   = it_post_fin-meinb.

    CONCATENATE it_post_fin-matnr ';' it_post_fin-stype
           INTO it_l_doc_items-seg_text.

    APPEND it_l_doc_items.
    CLEAR  it_l_doc_items.
    CLEAR it_post_fin.
  ENDLOOP.

** BAPI
  CLEAR : it_return, it_return[].
  CLEAR : lv_doc_no .
  CALL FUNCTION 'BAPI_ACC_PRIMARY_COSTS_POST'
    EXPORTING
      doc_header            = wa_l_doc_header
*     IGNORE_WARNINGS       = ' '
    IMPORTING
      doc_no                = lv_doc_no
    TABLES
      doc_items             = it_l_doc_items
      return                = it_return.
* CHECK ERROR
  CLEAR  it_return.
  LOOP AT it_return.
    MESSAGE ID     it_return-id
            TYPE   it_return-type
            NUMBER it_return-number
            WITH   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    CLEAR it_return.
  ENDLOOP.

* Save Doc. No.
  LOOP AT it_post_fin  FROM p_lv_fr TO  p_lv_to.
    it_post_fin-belnr =   lv_doc_no.
    MODIFY it_post_fin.
    CLEAR  it_post_fin.
  ENDLOOP.

ENDFORM.                    " CALL_POST_FM_RES

*&---------------------------------------------------------------------*
*&      Form  UPDATE_DOC_NO
*&---------------------------------------------------------------------*
*       Save Doc No.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_doc_no.
  CLEAR it_post_fin.
  LOOP AT it_post.
    CLEAR it_post_fin.
    READ TABLE it_post_fin WITH KEY   kstar      = it_post-kstar
                                      io_aufnr   = it_post-io_aufnr
                                      pcc_aufnr  = it_post-pcc_aufnr.
    it_post-belnr =  it_post_fin-belnr.
* Log
    it_post-aedat = sy-datum.
    it_post-aezet = sy-uzeit.
    it_post-aenam = sy-uname.
* Update
    CLEAR ztco_abispost.
    MOVE-CORRESPONDING it_post TO ztco_abispost.
    UPDATE ztco_abispost.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e030 WITH 'ZTCO_ABISPOST'.
    ENDIF.
  ENDLOOP.
* Commit Work.
  COMMIT WORK AND WAIT.
  MESSAGE s000 WITH 'The process for posting was Completed'.
ENDFORM.                    " UPDATE_DOC_NO

*&---------------------------------------------------------------------*
*&      Form  ALPHA_KSTAR
*&---------------------------------------------------------------------*
*       Kstar - > Alpha Numeric
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alpha_kstar.
  LOOP AT s_kstar.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              input  = s_kstar-low
         IMPORTING
              output = s_kstar-low.

    MODIFY s_kstar.
    CLEAR  s_kstar.
  ENDLOOP.
ENDFORM.                    " ALPHA_KSTAR

*&---------------------------------------------------------------------*
*&      Form  ADJUSTMENT_DATA
*&---------------------------------------------------------------------*
*       Adjustment
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM adjustment_data.
* Progress Ind.
  PERFORM progress_ind USING '80'
                             text-024.

* Local data definition
  DATA :  lv_sum_wkgbtr LIKE coep-wkgbtr.
  DATA :  lv_modify.

  SORT it_io_coep  BY  matnr stype werks kstar objnr.
  SORT it_post     BY  matnr stype werks kstar  ASCENDING
                       stype io_aufnr
                       chg_wkgbtr         DESCENDING .

  LOOP AT it_io_coep .
    LOOP AT it_post WHERE matnr    = it_io_coep-matnr
                      AND werks    = it_io_coep-werks
                      AND kstar    = it_io_coep-kstar
                      AND stype    = it_io_coep-stype
                      AND io_aufnr = it_io_coep-objnr+2(5).
      CLEAR  lv_modify.
      CLEAR  lv_sum_wkgbtr.
* Check different amount
      AT END OF io_aufnr.
        SUM .
        lv_sum_wkgbtr = it_post-chg_wkgbtr.
        IF lv_sum_wkgbtr <> it_io_coep-wkgbtr .
          lv_modify = 'X'.
        ENDIF.
      ENDAT.
      AT END OF werks.
      ENDAT.
      AT END OF kstar.
      ENDAT.
* modify
      IF lv_modify = 'X'.
        it_post-chg_wkgbtr =
        it_post-chg_wkgbtr + ( it_io_coep-wkgbtr - lv_sum_wkgbtr ).
        MODIFY it_post.
      ENDIF.
      CLEAR it_post.
    ENDLOOP.
    CLEAR it_io_coep.
  ENDLOOP.

  DELETE it_post WHERE chg_wkgbtr EQ 0
                    OR mbgbtr     EQ 0.
ENDFORM.                    " ADJUSTMENT_DATA

*&---------------------------------------------------------------------*
*&      Form  IND_POST_N_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ind_post_n_report.
*P_CONF
  LOOP AT SCREEN.
    CHECK screen-group1 = 'ZPA'.
    IF p_conf = 'X'.
      screen-invisible = ' '.
      screen-input     = '1'.
    ELSE.
      screen-invisible = '1'.
      screen-input     = ' '.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " IND_POST_N_REPORT

*&---------------------------------------------------------------------*
*&      Form  CAL_COST_RATIO_02
*&---------------------------------------------------------------------*
* Cal. Total material cost and the ratio for parents materials,
*      The cost ratio of child materials
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_cost_ratio_02.

* Local Data Definition
  DATA : BEGIN OF it_l_chm_rate OCCURS 0.
  DATA :  kstar  LIKE coep-kstar,
          matnr  LIKE coep-matnr,
          werks  LIKE coep-werks,
          fevor  LIKE marc-fevor,
          pcc_mbgbtr LIKE coep-mbgbtr.
  DATA : END OF  it_l_chm_rate.

  DATA : BEGIN OF it_l_tot_rate OCCURS 0.
  DATA :  werks  LIKE coep-werks,
          objnr  LIKE coep-objnr,
          fevor  LIKE marc-fevor,
          tot_wkgbtr LIKE coep-wkgbtr,
*          tot_mbgbtr LIKE coep-mbgbtr,
          kstar_rate LIKE ztco_abispost-kstar_rate.
  DATA : END OF  it_l_tot_rate.

  DATA : BEGIN OF it_l_pccsum OCCURS 0,
            werks  LIKE coep-werks,
            fevor  LIKE marc-fevor,
            wk_sum LIKE coep-wkgbtr,
         END OF   it_l_pccsum.

* Calculation the cost ratio
  CLEAR : it_l_chm_rate, it_l_chm_rate[].
  CLEAR : it_l_tot_rate, it_l_tot_rate[].
  CLEAR : it_l_pccsum  , it_l_pccsum[]  .

  SORT it_pcc_coep BY kstar matnr werks objnr.

  LOOP AT  it_pcc_coep.
    CLEAR it_pcc_mat.
    READ TABLE it_pcc_mat WITH KEY objnr  = it_pcc_coep-objnr.

* Calculation the total cost by child materials
    MOVE-CORRESPONDING  it_pcc_coep TO  it_l_chm_rate.
    it_l_chm_rate-pcc_mbgbtr = it_pcc_coep-mbgbtr.
    COLLECT it_l_chm_rate.
    CLEAR   it_l_chm_rate.
* Calculation the total cost by PCC (Parents materials)
    MOVE-CORRESPONDING  it_pcc_coep TO  it_l_tot_rate.
    it_l_tot_rate-tot_wkgbtr = it_pcc_coep-wkgbtr.
    it_l_tot_rate-fevor      = it_pcc_mat-fevor.
    COLLECT it_l_tot_rate.
    CLEAR   it_l_tot_rate.
* PCC SUM
    it_l_pccsum-werks  = it_pcc_coep-werks.
    it_l_pccsum-fevor  = it_pcc_mat-fevor.
    it_l_pccsum-wk_sum = it_pcc_coep-wkgbtr.
    COLLECT it_l_pccsum.
    CLEAR   it_l_pccsum.
    CLEAR it_pcc_coep.
  ENDLOOP.

* Copy to Global
  CLEAR : it_tot_rate, it_tot_rate[].
  LOOP AT it_l_tot_rate.
    CLEAR it_l_pccsum.
    READ TABLE it_l_pccsum WITH KEY werks = it_l_tot_rate-werks
                                    fevor = it_l_tot_rate-fevor.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    it_l_tot_rate-kstar_rate
      = it_l_tot_rate-tot_wkgbtr  / it_l_pccsum-wk_sum.
    MODIFY it_l_tot_rate.
* Copy to global Itab
    MOVE-CORRESPONDING  it_l_tot_rate TO it_tot_rate.
    APPEND it_tot_rate.
    CLEAR  it_tot_rate.
    CLEAR it_l_tot_rate.
  ENDLOOP.

* Sort
  SORT it_l_chm_rate BY kstar matnr werks.
  CLEAR   it_l_chm_rate.
*
*  RATE_CHILD(16)    LIKE ZTCO_ABISPOST-RATE_CHILD.
  LOOP AT it_pcc_coep.
* ratio (Child)
    CLEAR   it_l_chm_rate.
    READ TABLE it_l_chm_rate WITH KEY kstar = it_pcc_coep-kstar
                                      matnr = it_pcc_coep-matnr
                                      werks = it_pcc_coep-werks.
    IF   sy-subrc = 0
     AND it_l_chm_rate-pcc_mbgbtr NE space.
      it_pcc_coep-rate_child
       = it_pcc_coep-mbgbtr / it_l_chm_rate-pcc_mbgbtr.
    ENDIF.
    MODIFY it_pcc_coep.
    CLEAR  it_pcc_coep.
  ENDLOOP.

  CLEAR  it_pcc_coep.
  CLEAR  it_tot_rate.

ENDFORM.                    " CAL_COST_RATIO_02

*&---------------------------------------------------------------------*
*&      Form  CAL_QTY_RATIO_02
*&---------------------------------------------------------------------*
*       Ratio By qty
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_qty_ratio_02.

  CLEAR  it_pcc_coep.
  CLEAR  it_tot_rate.
  CLEAR  it_io_coep.

* Progress Ind.
  PERFORM progress_ind USING '60'
                             text-023.

ENDFORM.                    " CAL_QTY_RATIO_02

*&---------------------------------------------------------------------*
*&      Form  READ_VERID_FROM_PCC
*&---------------------------------------------------------------------*
*       Read Production version from PCC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_verid_from_pcc.

  TABLES : pkosa, ckmlmv001.

  CLEAR pkosa.
  pkosa-aufnr = ztco_abispost-pcc_aufnr.
  CALL FUNCTION 'KK_F_PKOSA_FILL'
       EXPORTING
            pkosanr_imp  = pkosa-aufnr
       IMPORTING
            pkosa_exp    = pkosa
       EXCEPTIONS
            aufnr_not_ok = 1
            OTHERS       = 2.
  IF sy-subrc <> 0.
  ENDIF.

  CLEAR ckmlmv001.
  CALL FUNCTION 'CKML_MGV_PROCESS_READ'
    EXPORTING
        i_kalnr           = pkosa-procnr
*       I_BUFFER          =
    IMPORTING
*       E_PROCESS         =
        e_ckmlmv001       = ckmlmv001
    EXCEPTIONS
        not_found         = 1
        OTHERS            = 2.
  IF sy-subrc <> 0.
  ENDIF.

  ztco_abispost-verid = ckmlmv001-verid_nd.
  ztco_abispost-fsc_matnr = pkosa-matnr.
ENDFORM.                    " READ_VERID_FROM_PCC

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_IND
*&---------------------------------------------------------------------*
*       Progress IND.
*----------------------------------------------------------------------*
*      -->P_%         %
*      -->P_TEXT      TEXT
*----------------------------------------------------------------------*
FORM progress_ind USING    p_%
                           p_text.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
    EXPORTING
      percentage          = p_%
      text                = p_text
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' '
*     MESSAGEPAR4         = ' '
            .
ENDFORM.                    " PROGRESS_IND
*&---------------------------------------------------------------------*
*&      Form  GET_PARENT_PRODUCTION_SCHEDULE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_FEVOR  text
*----------------------------------------------------------------------*
FORM get_parent_production_schedule CHANGING pw_fevor.
  CASE it_mard-werks.
    WHEN 'P001'.
      CASE it_mard-fevor.
        WHEN c_blank.
          MOVE: c_press TO pw_fevor.
        WHEN c_coil.
          MOVE: c_blank TO pw_fevor.
        WHEN OTHERS.
          MOVE: space   TO pw_fevor.
      ENDCASE.
    WHEN 'E001'.
      CASE it_mard-fevor.
        WHEN c_3c.
          MOVE: c_engine TO pw_fevor.
        WHEN OTHERS.
          MOVE: space    TO pw_fevor.
      ENDCASE.
    WHEN OTHERS.
      MOVE: space TO pw_fevor.
  ENDCASE.
ENDFORM.                    " GET_PARENT_PRODUCTION_SCHEDULE
*&---------------------------------------------------------------------*
*&      Form  make_ppc_post_table_for_no_bf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_ppc_post_table_for_no_bf USING pw_fevor.

  IF it_mard-werks EQ 'E001' OR
     pw_fevor      EQ  c_3c.
    LOOP AT it_tot_rate WHERE werks = it_mard-werks.
      PERFORM move_mard_to_it_post.
    ENDLOOP.
  ELSE.
    LOOP AT it_tot_rate WHERE werks = it_mard-werks
                          AND fevor = pw_fevor.
      PERFORM move_mard_to_it_post.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " make_ppc_post_table_for_no_bf
*&---------------------------------------------------------------------*
*&      Form  rounding_uom_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rounding_uom_amount USING pw_netpr.
*  DATA: lw_wkgbtr     LIKE it_io_coep-mbgbtr,
*        lw_netpr_half TYPE p DECIMALS 3,
*        lw_sum        LIKE it_io_coep-wkgbtr,
*        lw_gap        LIKE it_io_coep-wkgbtr,
*        lw_mod        TYPE p DECIMALS 3.
*
*
*  lw_netpr_half = pw_netpr / 2.
*
*  LOOP AT it_post_tmp.
*    lw_mod = it_post_tmp-chg_wkgbtr MOD pw_netpr.
*    IF lw_mod EQ 0.
*      CONTINUE.
*    ENDIF.
*
*    IF lw_mod >= lw_netpr_half.
*      lw_wkgbtr = it_post_tmp-chg_wkgbtr - lw_mod + pw_netpr.
*    ELSE.
*      lw_wkgbtr = it_post_tmp-chg_wkgbtr - lw_mod.
*    ENDIF.
*
*    it_post_tmp-chg_wkgbtr = lw_wkgbtr.
*    lw_sum = lw_sum + lw_wkgbtr.
*    MODIFY it_post_tmp.
*  ENDLOOP.
*
*  lw_gap = it_io_coep-wkgbtr - lw_sum.
*  IF lw_gap NE 0.
*    SORT it_post_tmp BY chg_wkgbtr DESCENDING rate_child DESCENDING.
*
*    READ TABLE it_post_tmp INDEX 1.
*    it_post_tmp-chg_wkgbtr = it_post_tmp-chg_wkgbtr + lw_gap.
*    MODIFY it_post_tmp INDEX 1.
*  ENDIF.
ENDFORM.                    " rounding_uom_amount
