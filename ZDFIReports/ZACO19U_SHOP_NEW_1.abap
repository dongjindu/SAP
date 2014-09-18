************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZACO19U_SHOP_SUMMARY_NEW                                   *
*& Type   : Report                                                     *
*& Spec   : Andy Choi                                                  *
*& Author : Andy
*& Title  :                                                            *
*&---------------------------------------------------------------------*
* Don't change logic without confirmation from Andy!!!
* Date         Developer    Request      Description
* 03/01/2007   Manju        UD1K930929   Program Bug fix
************************************************************************
REPORT zaco19u_shop_new  "LINE-COUNT 65
                         NO STANDARD PAGE HEADING MESSAGE-ID zmco_shop.

INCLUDE zaco19u_shop_new_1_top.

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
* General Info.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY ID cac ." OBLIGATORY.

* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj." OBLIGATORY.
* periods
*selection-screen begin of line.
*selection-screen comment  1(30) text-021. "From
*selection-screen position 33.
PARAMETERS: p_perab LIKE covja-perab MEMORY ID vpe
            MODIF ID per." OBLIGATORY.
*selection-screen end of line.

PARAMETERS: p_ccs    AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_update AS CHECKBOX DEFAULT 'X'.         "Update run
PARAMETERS: p_ksgru TYPE ksgru  DEFAULT 'HMMA-SHOP',
            p_elehk LIKE tckh4-elehk DEFAULT 'H1'.

SELECTION-SCREEN END OF BLOCK bl1.

* Option
SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-004.
SELECT-OPTIONS : s_matnr FOR keko-matnr MEMORY ID mat,
                 s_mtart FOR mara-mtart,
                 s_werks FOR marc-werks,
                 s_aufnr FOR aufk-aufnr MEMORY ID anr.
SELECTION-SCREEN SKIP 1.
PARAMETERS:     p_fsc   NO-DISPLAY. "AS CHECKBOX.

*selection-screen skip 1.
SELECTION-SCREEN END OF BLOCK bl4.

* Reporting Options
PARAMETERS: p_disp   AS CHECKBOX DEFAULT ''.
PARAMETERS: p_debug  AS CHECKBOX DEFAULT ''.
PARAMETERS: p_resou  LIKE ztco_shop_sum_1-resou.
*selection-screen skip 1.

PARAMETERS: p_sql(1) TYPE c DEFAULT 'X' NO-DISPLAY.
PARAMETERS: cstest LIKE bapicostes-cosestimat DEFAULT '01' NO-DISPLAY.

*//Modify..03/31/2011..T00020..
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK frame1 WITH FRAME TITLE text-200.
" Backgr : Disable due to the performance issue
PARAMETERS: backgr     TYPE batchflg NO-DISPLAY,
            protdr     LIKE kala-paradr4,
            p_count(5) TYPE n NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK frame1.
*//

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  CLEAR r_kstar[].
  r_kstar-option = 'EQ'.
  r_kstar-sign   = 'I'.

  p_versn = '000'.
  p_tvers  = '04'.


*SAP error; get next period STD (now fixed)
*past incorrect data...until 03/2006
***  select single noday into gv_sap_err from ztfi_ctl
***    where bukrs = p_kokrs and categ = 'DI_WIP' and flag = 'X'.
  IF p_kokrs = 'H201'.
    IF p_bdatj = '2006' AND p_perab <= '003'.
      gv_sap_err = 1.
    ELSEIF p_bdatj < '2006'.
      gv_sap_err = 1.
    ENDIF.
  ENDIF.

*  CLEAR: it_mat[], it_fsc_mat[].
*  IMPORT it_mat     = it_mat     FROM MEMORY ID 'SHOPCC'.
*  IMPORT it_fsc_mat = it_fsc_mat FROM MEMORY ID 'SHOPCC2'.
*  MOVE: p_perab TO w_perbi.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Set Record Type / Routing Usage
  PERFORM set_date.
  PERFORM set_rec_type_r_usage.

AT SELECTION-SCREEN OUTPUT.
* Modify Screen Att.
  PERFORM mod_screen_att.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* imported from parallel program
  IMPORT  p_kokrs  FROM MEMORY ID 'SHOPCC_KOKRS'.
  IMPORT  p_bdatj  FROM MEMORY ID 'SHOPCC_BDATJ'.
  IMPORT  p_perab  FROM MEMORY ID 'SHOPCC_PERAB'.

  CLEAR: it_mat[], it_fsc_mat[].
  IMPORT it_mat     = it_mat     FROM MEMORY ID 'SHOPCC'.
  IMPORT it_fsc_mat = it_fsc_mat FROM MEMORY ID 'SHOPCC2'.

*//Modify..03/31/2011..T00020..
*  IF NOT backgr  IS INITIAL AND
*      sy-batch   IS INITIAL.
*    PERFORM exec_in_bg USING sy-repid
*                             y_selection.
*    EXIT.
*  ENDIF.
*//

  PERFORM check_obligatory_field.

* Controlling Area Information
  PERFORM read_tka01.

* Read CCTRs in SHOP
  PERFORM read_cctr_in_shop.
* Read Default Unit of ATs
  PERFORM read_de_unit_of_csla.

* Read Tracking Point for Supply Area ( KMMG Only )
*  PERFORM read_tracking_point.

* -  Progress Ind.
  PERFORM progress_ind USING '05' text-210.

*  IF p_fsc = 'X'.
*    PERFORM read_ztcou100.
*  ENDIF.

* Read Base Information
* If IT_MAT is exist => It is imported from parallel program
  IF it_mat[] IS INITIAL.
    PERFORM read_materials.
  ELSE.
    g_batch  = 'X'.
    p_update = 'X'.
    p_disp   = ''.
    PERFORM read_materials_parallel.
    LOOP AT it_fsc_mat.
      PERFORM make_batch_log USING it_fsc_mat-matnr '2'.
    ENDLOOP.
  ENDIF.

* Commented out by Han Moon
*  PERFORM read_cokey.

* Read Object Key for PCC order .
  PERFORM progress_ind USING '10' text-211.
  PERFORM read_obj_for_pcc.

  PERFORM get_activity_types.

* CALCULATE % (KSBT)
**  PERFORM cal_percent_using_ksbt.

*Read OS&D,Key-In Alloc detail
*  PERFORM read_osd_alloc.

  "Commented out by Han Moon
*  PERFORM read_cosp_coss.

*Reading CO Object..
* cobk/coep -> COLLECT IT_FSC_GR.
*              COLLECT IT_OBJ_GR.
*              COLLECT IT_GI_MISC.
*              COLLECT IT_COEP_SELF.
  REFRESH it_mat_tmp.
  PERFORM progress_ind USING '15' text-231.
  PERFORM read_co_docs.

* Commented out by Han Moon
*  IF it_mat_tmp[] IS NOT INITIAL.
*    DELETE ADJACENT DUPLICATES FROM it_mat_tmp.
*    APPEND LINES OF it_mat_tmp TO it_mat.
*    REFRESH it_mat_tmp.
*  ENDIF.

* Read M/H
* Vehicle-> CPZP table...
* Other -> AFRU,COVP
  PERFORM progress_ind USING '20' text-250.
  PERFORM read_mh_data.                    " cobk/afru/coep -> it_mhsum


* Get Multi Level Price
  PERFORM progress_ind USING '25' text-225.
*//Modify..03/24/2011..T00020..
*  PERFORM GET_MULTILEVEL_PRICE_T00020.
  PERFORM get_multilevel_price.
*//

*read confirmation  ( GET SCRAP : AFRU & ckmlmv013 -> it_mto_scrap )
  PERFORM progress_ind USING '30' text-312.
  PERFORM read_confirmation.

* Get WIP
* -- refer ZACO15 report program for DI-WIP
*  REFRESH it_mat_tmp.
  PERFORM progress_ind USING '35' text-230.

  PERFORM read_wips.
*  IF it_mat_tmp[] IS NOT INITIAL.
*    DELETE ADJACENT DUPLICATES FROM it_mat_tmp.
*    APPEND LINES OF it_mat_tmp TO it_mat.
*    REFRESH it_mat_tmp.
*  ENDIF.

*//Modify..03/24/2011..T00020..
*  PERFORM read_scrap_from_pcc .  (Scrap : COSB -> it_obj_scrap )


  PERFORM progress_ind USING '40' text-313.
  PERFORM select_wip_materials.

*Calculate INPUT GR Qty / ITEMIZATION
  PERFORM progress_ind USING '45' text-260.
  PERFORM get_cost_itemization.            " it_fsc_gr -> lt_ck13n

* -  Progress Ind.
  PERFORM progress_ind USING '50' text-235.
*  IF it_mat_tmp[] IS NOT INITIAL.
*    DELETE ADJACENT DUPLICATES FROM it_mat_tmp.
*    APPEND LINES OF it_mat_tmp TO it_mat.
*  ENDIF.
  PERFORM get_material_prices.             " CKMLCR/CKMLPP -> IT_MAT

* FSC/Component material..
  PERFORM progress_ind USING '55' text-314.
  PERFORM get_ckmlkeph_all.                " ckmlmv001 -> it_ckmlkeph


* WIP valuation
  PERFORM progress_ind USING '60' text-240.
  PERFORM wip_valuation_di. " gs_quantities -> lt_wipvalue / it_mhsum
  PERFORM wip_valuation_sap.               " it_wip_sap -> lt_wipvalue
**MSG-Reading Cost Documents
*  perform progress_ind using '60' text-260.
** Calculate INPUT GR Qty / ITEMIZATION
*  perform get_cost_itemization.


*MSG: Populating Final internal table
  PERFORM progress_ind USING '65' text-270.
*FIXME; UoM Change, ...

*  SORT it_mat BY matnr bwkey bwtar.
  PERFORM making_main_tab.

* Material Data
  PERFORM put_shop_mat_info.  "it_shop_sum -> it_obj_sum_wip
* Read Scrap/Wip
*  PERFORM read_scrap_wip_qty.

* read Additional Issue from CBO
**  PERFORM read_abispost.
* SHOP information By Item Category. (Actual)
**  PERFORM set_shop_by_item_cate.

* Disposal of Fractional Amount
*  IF p_woadj = space.
*    PERFORM disp_fraction_amt.
*    PERFORM disp_fraction_by_cate.
*    PERFORM disp_fraction_by_mlcc.
*  ENDIF.

  IF p_ccs = 'X'.               "Cost component view
    PERFORM progress_ind USING '70' text-290.

    MESSAGE s000 WITH 'Cost component view 1 ...Overall & Fixed value'.
    PERFORM create_it_shop_cc.     "it_shop_sum  -> it_shop_cc


    MESSAGE s000 WITH 'Cost component view 2 ...Modify.. it_shop_cc'.
    PERFORM calc_shop_cc.          "it_shop_cc   -> it_res_ccsum
    PERFORM scale_cc_to_sum.       "it_res_ccsum -> it_shop_cc


    MESSAGE s000 WITH 'Cost component view 3 ...Adjust to total value'.
*-adjust to total value
    PERFORM get_fsc_std_cc. "it_fsc_gr  -> fsc_std_cc/ fsc_std_ccsum


    MESSAGE s000 WITH 'Cost component view 4 ...it_ml_scale'.
    PERFORM get_ml_gr_cc.
    "it_fsc_mat(it_obj_gr/it_ckmlkeph) -> it_ml_scale
*    PERFORM get_ml_single_cc.     "Modify..03/29/2011..T00020..


    MESSAGE s000 WITH 'Cost component view 5 ...it_shop_cc_adj'.
    PERFORM get_it_cc_wip_scale.   "it_shop_cc -> it_cc_wip_scale
    PERFORM scale_wip.             " W Creation
    "it_obj_gr(it_cc_wip_scale/fsc_std_cc) -> it_shop_cc_adj

    PERFORM scale_to_ml.           " U creation
    "it_fsc_mat(it_ml_scale / it_cc_round) -> it_shop_cc_adj

*    APPEND LINES OF it_shop_cc_adj TO it_shop_cc.
*    INSERT LINES OF it_shop_cc_adj INTO TABLE it_shop_cc.

  ENDIF.

  MESSAGE s000 WITH 'Data Processing Complete...'.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF p_update = 'X'.                ""Update run
* Delete DB for New Records
    PERFORM del_data_fr_ztco_shop_sum.
* Update/Insert
    PERFORM update_ztco_shop_sum.
    COMMIT WORK.
    PERFORM progress_ind USING '99' text-303.

* for batch program -> log
    PERFORM update_job_log.
  ENDIF.

* show detail list
  IF g_batch = space AND p_disp = 'X'.
    SORT it_shop_sum BY typps kstar resou.
    PERFORM display_out_itemization.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  display_out_itemization
*&---------------------------------------------------------------------*
FORM display_out_itemization.
* ==> build field category
  PERFORM field_setting TABLES gt_fieldcat USING :
 'TYPPS'          'TYPPS'        '01' 'X' 'L' ' ' ' ' ' ' ' ' ' ',
 'KSTAR'          'KSTAR'        '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'RESOU'          'Res Field'    '20' 'X' 'L' ' ' ' ' ' ' ' ' ' ',

 'MANU_AMT'       'Manuf $'      '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'WIP_AMT'        'WIP$'         '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'GR_AMT'         'GR $'         '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'SINGLE_AMT'     'ML_Single$'   '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'MULTI_AMT'      'ML_Multi $'   '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'MULTI_SAMT'     'ML_M_S $'     '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'MULTI_MAMT'     'ML_M_M $'     '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',

 'MBGBTR'         'CurrentQ'     '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'MBGBTR2'        'CurVarQ'      '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'ADD_MBGBTR'     'Add QTY'      '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'WIP_PQTY'       'WIP OpenQ'    '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'WIP_QTY'        'WIP_QTY'      '13' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'SCRAP_QTY'      'SCRAP Qty'    '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
* 'ACT_QTY'        'ACT QTY'      '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'GR_QTY'         'GR QTY'       '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'APO_VAR_QTY'    'APO VAR Q'    '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'APO_INPUT_QTY'  'APO INPUT Q'  '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'APO_OUTPUT_QTY' 'APO OUTPUT Q' '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'SINGLE_QTY'     'ML_SingleQ'   '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'MULTI_QTY'      'ML Multi Q'   '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',
 'MANU_QTY'       'Manuf QTY'    '15' ' ' 'R' ' ' ' ' ' ' ' ' ' ',

 'STPRS'          'STPRS'        '12' ' ' 'R' ' ' ' ' ' ' ' ' '',
 'VERPR'          'VERPR'        '12' ' ' 'R' ' ' ' ' ' ' ' ' '',
 'PEINH'          'PEINH'        '05' ' ' 'R' ' ' ' ' ' ' ' ' '',

 'WKGBTR'         'Current$'     '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'WKGBTR2'        'CurVar$'      '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'ADD_WKGBTR'     'Add $'        '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'SCRAP_AMT'      'Scrap $'      '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
 'WIP_PAMT'       'WIP Open$'    '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',

* 'MULTI_PRE_AMT'  'ML_PrevM$'    '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',


 'VMSTC'          'VMSTC'        '12' ' ' 'R' ' ' ' ' ' ' ' ' '',
 'VMPEC'          'VMPEC'        '05' ' ' 'R' ' ' ' ' ' ' ' ' '',

 'OBJNR'          'OBJNR'        '18' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'ARTNR'          'PRODUCT'      '18' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'VERID'          'VER'          '02' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'AUFNR'          'AUFNR'        '12' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'LLV_MATNR'      'LLV_MATNR'    '18' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'BWKEY'          'BWKEY'        '04' ' ' 'L' ' ' ' ' ' ' ' ' 'X',
 'KOSTL'          'KOSTL'        '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'LSTAR'          'LSTAR'        '06' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
 'MEEHT'          'MEEHT'        '03' ' ' 'L' ' ' ' ' ' ' ' ' ' '.

* 'PREIS'          'UNIT$'      '11' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
* 'APO_MEINS'      'APO Unit'      '03' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'SHOP'           'SHOP'       '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'KALNR'          'KALNR'      '12' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'ELEMT'          'ELEMT'      '03' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'proc_kalnr'      'proc_kalnr'  '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'PAR_KADKY'      'PAR_KADKY'  '12' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'KOKRS'          'KOKRS'      '04' 'X' 'L' ' ' ' ' ' ' ' ' ' ',
* 'BDATJ'          'BDATJ'      '04' 'X' 'L' ' ' ' ' ' ' ' ' ' ',
* 'POPER'          'POPER'      '03' 'X' 'L' ' ' ' ' ' ' ' ' ' ',
* 'VERSN'          'VERSN'      '03' 'X' 'L' ' ' ' ' ' ' ' ' ' ',
* 'RECORD_TYPE'    'RT'         '01' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'BWTAR'          'BWTAR'      '04' ' ' 'L' ' ' ' ' ' ' ' ' 'X',
* 'VSPVB'          'VSPVB'      '10' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'MTART'          'MTART'      '04' ' ' 'L' ' ' ' ' ' ' ' ' ' '.
*'OBJNR'          'OBJNR'      '18' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'CHD_KALNR'      'CHD_KALNR'  '12' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'proc_kalnr' 'proc_kalnr'  '12' ' ' 'L' ' ' ' ' ' ' ' ' ' ',
* 'CHD_PROC_KALNR' 'CHD_KALNR'  '12' ' ' 'L' ' ' ' ' ' ' ' ' ' '.

  w_program = sy-repid.

*  delete adjacent duplicates from IT_SHOP_SUM.
*  DELETE  IT_SHOP_SUM WHERE aufnr EQ ''.
*  DELETE  IT_SHOP_SUM WHERE typps EQ ''.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = w_program
      it_fieldcat              = gt_fieldcat
      i_save                   = 'A'
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
    TABLES
      t_outtab                 = it_shop_sum
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.


ENDFORM.                    " display_out_itemization
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
*----------------------------------------------------------------------*
*   INCLUDE ZACO19L_F001                                               *
*----------------------------------------------------------------------*
*
*&---------------------------------------------------------------------*
*&      Form  MOD_SCREEN_ATT
*&---------------------------------------------------------------------*
*       Modfiy Screen Attribute
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mod_screen_att.
* Modify Screen
  LOOP AT SCREEN.
    CHECK screen-group1 = 'DIV'.
    screen-input = '0'.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MOD_SCREEN_ATT

*&---------------------------------------------------------------------*
*&      Form  SET_REC_TYPE_R_USAGE
*&---------------------------------------------------------------------*
*       Set Record Type / Routing Usage
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_rec_type_r_usage.

* Period
  IF p_perab > w_perbi.
    MESSAGE e031.
  ENDIF.


  IF p_perab <> '' AND ( p_perab > 12 OR p_perab < 1 ) .
    MESSAGE e007.
  ENDIF.

  IF w_perbi <> '' AND ( w_perbi > 12 OR w_perbi < 1 ) .
    MESSAGE e007.
  ENDIF.

ENDFORM.                    " SET_REC_TYPE_R_USAGE

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

* Consumption account
  SELECT * INTO TABLE i_t030
      FROM t030
        WHERE ktopl = tka01-ktopl
          AND ktosl = 'GBB'
          AND komok = 'VBR'.


* Set Validity Date (Start)
  DATA : lv_datum LIKE sy-datum.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_bdatj
*     I_MONMIT       = 00
      i_periv        = tka01-lmona
      i_poper        = p_perab
    IMPORTING
      e_date         = lv_datum
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  g_todat = lv_datum.

* Get First Date (From-Period)
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_bdatj
*     I_MONMIT       = 00
      i_periv        = tka01-lmona
      i_poper        = p_perab
    IMPORTING
      e_date         = lv_datum
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  g_frdat = lv_datum.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'CKML_F_GET_PREVIOUS_PERIOD'
    EXPORTING
      input_period    = p_perab
      input_year      = p_bdatj
      input_periv     = tka01-lmona
    IMPORTING
      previous_period = g_pr_lfmon
      previous_year   = g_pr_lfgja.

  SELECT kstar katyp
    INTO CORRESPONDING FIELDS OF TABLE it_kstar
    FROM cskb
   WHERE kokrs = p_kokrs
     AND katyp IN ('01', '21', '22', '41', '42', '43').

  DATA: lx       LIKE sy-tabix,
        l_tckh3  TYPE  tckh3.

  LOOP AT it_kstar.
    lx = sy-tabix.

*   CALL FUNCTION 'KKEK_COST_COMPONENT_ELEMENT'  "ERROR msg
*      EXPORTING
*        elehk_imp         = p_elehk
*        ktopl_imp         = p_kokrs
*        kstar_imp         = it_kstar-kstar
*        message_on_screen = ' '
*      IMPORTING
*        elemt_exp         = it_kstar-elemt
*      EXCEPTIONS
*        calling_error     = 1
*        OTHERS            = 2.
    CALL FUNCTION 'CK_F_TCKH4_HIERARCHY_READING'
      EXPORTING
        p_bukrs = p_kokrs
        p_elehk = p_elehk
*       P_KTOPL =
        p_kstar = it_kstar-kstar
      IMPORTING
        f_tckh3 = l_tckh3.

    IF sy-subrc = 0.
      it_kstar-elemt = l_tckh3-elemt.
*      MODIFY it_kstar INDEX lx TRANSPORTING elemt.
      MODIFY it_kstar TRANSPORTING elemt
                             WHERE kstar = it_kstar-kstar.
    ENDIF.
  ENDLOOP.
*  SORT it_kstar BY kstar.


* Select object no. by Cost center & activity type
  DATA : BEGIN OF lt_onrkl OCCURS 0,
          kostl LIKE onrkl-kostl,
          lstar LIKE onrkl-lstar,
          objnr LIKE onrkl-objnr,
         END OF lt_onrkl.


*//Modify..03/29/2011..T00020..
  REFRESH lt_onrkl.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  lt_onrkl
     FROM onrkl
    WHERE kokrs = p_kokrs.
  LOOP AT lt_onrkl .
    it_act-kostl = lt_onrkl-kostl.
    it_act-lstar = lt_onrkl-lstar.
    it_act-objnr = lt_onrkl-objnr.
    APPEND it_act. CLEAR it_act.
  ENDLOOP.
  SORT it_act.
  DELETE ADJACENT DUPLICATES FROM it_act.

*  Costing no for cctr+activity
  DATA : l_tabix LIKE sy-tabix.
  DATA : l_kalnr LIKE ckmllahd-kalnr.
  LOOP AT it_act.
    l_tabix = sy-tabix.

    CALL FUNCTION 'CKML_LA_HEADER_READ'
      EXPORTING
        i_objnr                = it_act-objnr
      IMPORTING
        ef_ckmllahd            = ef_ckmllahd
      TABLES
        et_ckmllahd            = it_ckmllahd
      EXCEPTIONS
        no_key_specified       = 1
        header_not_found_kalnr = 2
        no_data_found          = 3
        no_header_created      = 4
        OTHERS                 = 5.

    IF sy-subrc <> 0.
    ELSE.
      READ TABLE it_ckmllahd INDEX 1.
      IF sy-subrc = 0 .
        l_kalnr = it_ckmllahd-kalnr.
        it_act-kalnr = l_kalnr.
        MODIFY it_act INDEX l_tabix. CLEAR it_act.
      ELSE.
        l_kalnr = ef_ckmllahd-kalnr.
        it_act-kalnr = l_kalnr.
        MODIFY it_act INDEX l_tabix. CLEAR it_act.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  read_materials
*&---------------------------------------------------------------------*
FORM read_materials.
  DATA: $it_fsc_mat LIKE it_fsc_mat OCCURS 0 WITH HEADER LINE.
  DATA: tper_kalnr TYPE ckmv0_matobj_str OCCURS 0 WITH HEADER LINE.

  RANGES: r_bwkey FOR ckmlhd-bwkey.
  TABLES: t001k.

* Select Materials

* Read Active Cost Component Structure
  CLEAR tckh4.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF tckh4
                  FROM tckh4
                 WHERE elehk = p_elehk
                   AND aktiv = 'X'.
  IF tckh4 IS INITIAL.
*Check Active Cost Components Structure (IMG)
    MESSAGE e000 WITH text-311.
  ENDIF.


* select materials
  r_bwkey-sign = 'I'. r_bwkey-option = 'EQ'.
  SELECT * FROM t001k WHERE bukrs = p_kokrs.
    IF t001k-bwkey IN s_werks.
      r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
    ENDIF.
  ENDSELECT.

* MARA + MBEW(current period) = MACKU, MARC
  DATA: l_abrechdat LIKE ckmlhd-abrechdat.

* MARC-LGPRO was added by Han Moon
  SELECT c~matnr o~bwkey o~bklas
         o~kalst o~error_status
         c~werks c~profil c~sauft c~beskz c~sobsl
         c~vspvb c~fevor  c~prctr c~lgpro
         a~meins a~raube  a~mtart
         h~kalnr AS kaln1
         h~abrechdat
    INTO CORRESPONDING FIELDS OF TABLE  it_mat
    FROM ( ckmlmv011 AS o JOIN marc AS c
                            ON o~matnr = c~matnr
                           AND o~werks = c~werks
                          JOIN mbew AS w
                            ON o~matnr = w~matnr
                           AND o~bwkey = w~bwkey
                          JOIN mara AS a
                            ON o~matnr = a~matnr
                          JOIN ckmlhd AS h
                            ON o~kalnr = h~kalnr )
         BYPASSING BUFFER
   WHERE EXISTS ( SELECT *
                    FROM ckmlrunperiod AS p
                   WHERE p~gjahr  = p_bdatj
                     AND p~poper  = p_perab
                     AND p~run_id = o~laufid )
     AND o~bwkey     IN r_bwkey
     AND o~werks     IN s_werks
     AND h~abrechdat <> l_abrechdat.
*     AND a~mtart     IN s_mtart
*     AND c~matnr     IN s_matnr.

*NO RESTRICT!!!   AND ckmlmv011~matnr IN s_matnr.

* select FSC/HALB Mat. from ML

  CLEAR : it_fsc_mat, it_fsc_mat[].

*  DATA lf_where TYPE string.
*  CONCATENATE 'R~SME' p_perab ' > 0' INTO lf_where.
*  SELECT DISTINCT c~matnr w~bwkey  w~bklas
*         c~werks c~profil c~sauft c~beskz c~sobsl
*         c~vspvb c~fevor  c~prctr c~lgpro
*         m~meins m~raube  m~mtart
*         w~kaln1
*    INTO CORRESPONDING FIELDS OF TABLE it_mat
*    FROM cosr AS r JOIN aufk AS k
*                     ON r~objnr = k~objnr
*                   JOIN afko AS o
*                     ON k~aufnr = o~aufnr
*                   JOIN ckmlmv013 AS p
*                     ON k~aufnr = p~aufnr
*                   JOIN marc AS c
*                     ON p~pmatn = c~matnr
*                    AND p~prwrk = c~werks
*                   JOIN mbew AS w
*                     ON c~matnr = w~matnr
*                    AND c~werks = w~bwkey
*                   JOIN mara AS m
*                     ON c~matnr = m~matnr
*   WHERE r~lednr EQ '00'
*     AND r~gjahr EQ p_bdatj
*     AND r~wrttp EQ '04'
*     AND r~versn EQ '000'
*     AND k~aufnr IN s_aufnr
*     AND c~matnr IN s_matnr
*     AND c~werks IN s_werks
*     AND m~mtart IN s_mtart
*     AND (lf_where).

* Replaced by Han Moon
  LOOP AT it_mat WHERE matnr IN s_matnr
                   AND mtart IN s_mtart
                   AND bwkey IN s_werks
                   AND beskz EQ 'E'
                   AND sobsl EQ space.
*FIXME; is it OK???
*   check it_mat-kalst > 0.

*//Modify..04/05/2011..T00020...
*    CHECK it_mat-fevor <> space.
*//

** Engine -> Only Plant 'E001'  FIXME
*    IF   it_mat-beskz = 'F'     "Procurement Type
*     AND it_mat-sobsl = '40'    "Special procurement type
*     AND it_mat-mtart = c_halb. "HALB.
*      CONTINUE.
*    ENDIF.

    CLEAR $it_fsc_mat.
    MOVE-CORRESPONDING it_mat  TO $it_fsc_mat .
    APPEND $it_fsc_mat.

*//Modify..04/05/2011..T00020...
*  ENDLOOP.
*
*
*  LOOP AT $it_fsc_mat.
*//
    tper_kalnr-kalnr   = $it_fsc_mat-kaln1.
    tper_kalnr-bwkey   = $it_fsc_mat-bwkey.
    APPEND tper_kalnr.

  ENDLOOP.


  PERFORM read_mlperiods TABLES tper_kalnr
          USING p_bdatj p_perab.


  LOOP AT $it_fsc_mat.
    MOVE-CORRESPONDING $it_fsc_mat  TO it_fsc_mat .
    READ TABLE t_ckmlpp WITH KEY kalnr = $it_fsc_mat-kaln1
                                 bdatj = p_bdatj
                                 poper = p_perab
                                 untper = '00'
                                 BINARY SEARCH.
* Commented out by Han Moon
*    PERFORM get_ml_status USING sy-subrc.
    IF sy-subrc = 0.
      APPEND it_fsc_mat.
      CLEAR  it_fsc_mat.
    ENDIF.
  ENDLOOP.


  CLEAR:  it_fsc_mat, it_mat.

  IF it_fsc_mat[] IS INITIAL.
    MESSAGE e076 WITH c_fsc c_fsc_plant.
  ENDIF.

*sort for binary search
*  SORT it_mat BY matnr bwkey bwtar.


*Maintenance Status
*User department Maintenance status
*Work scheduling A
*Accounting B
*Classification C
*MRP D
*Purchasing E
*Production resources/tools F
*Costing G
*Basic data K
*Storage L
*Forecasting P
*Quality management Q
*Warehouse management S
*Sales V
*Plant stocks X
*Storage location stocks Z



ENDFORM.                    " read_materials

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
      percentage    = p_%
      text          = p_text
*     MESSAGECLASS  = ' '
*     MESSAGENUMBER = ' '
*     MESSAGEPAR1   = ' '
*     MESSAGEPAR2   = ' '
*     MESSAGEPAR3   = ' '
*     MESSAGEPAR4   = ' '
    .
ENDFORM.                    " PROGRESS_IND
*&---------------------------------------------------------------------*
*&      Form  SET_FROM_TO_PERIOD
*&---------------------------------------------------------------------*
*       Period (From To) FSC
*----------------------------------------------------------------------*
FORM set_from_to_period USING p_date
                              p_kokrs
                              p_bdatj
                              p_per.
* period (From/To)
  CALL FUNCTION 'K_DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date             = p_date
      i_kokrs            = p_kokrs
    IMPORTING
      e_gjahr            = p_bdatj
      e_perio            = p_per
    EXCEPTIONS
      no_period_determin = 1
      t009b_notfound     = 2
      t009_notfound      = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " SET_FROM_TO_PERIOD

*&---------------------------------------------------------------------*
*&      Form  READ_OBJ_FOR_PCC
*&---------------------------------------------------------------------*
*       Read Object Key for PCC order
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
          kalnr       LIKE ckmlmv001-kalnr,
        END OF lt_pcc.

  DATA : it_tmp_fsc_mat   LIKE STANDARD TABLE OF it_fsc_mat
                          WITH HEADER LINE .
  DATA : ef_ok TYPE  c.
  DATA lf_cc_guid TYPE qrp002-cc_guid.

* Create index of CKMLMV013!!!  (MANDT/PRWRK/PMATN)
  SELECT k~objnr k~aufnr
         p~pkosa p~kalnr_proc
         p~prwrk p~pmatn p~verid
         o~klvarp
         a~kalnr
    INTO CORRESPONDING FIELDS OF TABLE lt_pcc
    FROM ckmlmv013 AS p JOIN aufk AS k
                          ON k~aufnr   = p~pkosa
                        JOIN afko AS o
                          ON o~aufnr   = k~aufnr
             LEFT OUTER JOIN ckmlmv001 AS a
                          ON p~prwrk = a~werks
                         AND p~pmatn = a~matnr
                         AND p~verid = a~verid_nd
* Commented out by Han Moon
*        INNER JOIN mkal
*           ON mkal~matnr   = ckmlmv013~pmatn
     BYPASSING BUFFER
     FOR ALL ENTRIES IN it_fsc_mat
   WHERE p~prwrk =  it_fsc_mat-werks
     AND p~pmatn =  it_fsc_mat-matnr
     AND p~pkosa IN s_aufnr
     AND p~loekz =  space       "deletion
     AND p~autyp =  '05'.       "PCC (Product Cost Collector)

  it_tmp_fsc_mat[] = it_fsc_mat[].

  REFRESH it_fsc_mat.
  CLEAR : it_ckmlmv001. REFRESH it_ckmlmv001.

  SORT it_tmp_fsc_mat BY matnr werks.

  LOOP AT lt_pcc .

    READ TABLE it_tmp_fsc_mat WITH KEY matnr = lt_pcc-pmatn
                                       werks = lt_pcc-prwrk
                              BINARY SEARCH.
    CHECK sy-subrc = 0.

    CLEAR: it_fsc_mat.

    MOVE-CORRESPONDING it_tmp_fsc_mat TO it_fsc_mat.

    it_fsc_mat-aufnr      = lt_pcc-aufnr.
    it_fsc_mat-objnr      = lt_pcc-objnr.
    it_fsc_mat-verid      = lt_pcc-verid.
    it_fsc_mat-klvarp     = lt_pcc-klvarp.
    it_fsc_mat-proc_kalnr = lt_pcc-kalnr_proc.
    it_fsc_mat-kalnr      = lt_pcc-kalnr.

*  Replaced by Han Moon
*    CALL FUNCTION 'QRP_IS_APO_ORDER'
*      EXPORTING
*        if_objnr = it_fsc_mat-objnr
*      IMPORTING
*        ef_ok    = ef_ok.
*
*    IF ef_ok = 'X'.
*      it_fsc_mat-categ = 'DI'.
*    ELSE.
*      IF it_fsc_mat-sauft = 'X'.       "Ind.: Repetitive mfg allowed
*        it_fsc_mat-categ = 'REM'.
*      ELSE.
*        it_fsc_mat-categ = 'MTO'.
*      ENDIF.
*    ENDIF.
    SELECT SINGLE cc_guid
      INTO lf_cc_guid
      FROM qrp002
     WHERE aufnr EQ it_fsc_mat-aufnr.
    IF sy-subrc EQ 0.
      it_fsc_mat-categ = 'DI'.
    ELSE.
      IF it_fsc_mat-sauft = 'X'.       "Ind.: Repetitive mfg allowed
        it_fsc_mat-categ = 'REM'.
      ELSE.
        it_fsc_mat-categ = 'MTO'.
      ENDIF.
    ENDIF.

    APPEND   it_fsc_mat.

* Added by Han Moon
    CLEAR  it_ckmlmv001.
    it_ckmlmv001-kaln1    = it_fsc_mat-kaln1.
    it_ckmlmv001-kalnr    = it_fsc_mat-kalnr.
    it_ckmlmv001-pmatn_nd = it_fsc_mat-matnr.
    it_ckmlmv001-verid_nd = it_fsc_mat-verid.
    APPEND it_ckmlmv001.

  ENDLOOP.


  IF it_fsc_mat[] IS INITIAL.
    MESSAGE e026.
  ENDIF.


ENDFORM.                    " READ_OBJ_FOR_PCC

*&---------------------------------------------------------------------*
*&      Form  READ_CO_DOCS
*&---------------------------------------------------------------------*
*       Read Cost Document
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_co_docs.

  DATA: l_mat(20)     TYPE c,
        l_matnr       LIKE coep-matnr,
        l_zbwart(10) , "      LIKE ztco0025-zbwart,
        l_budat(6),
        l_505905(5)   TYPE c,
        l_idx         LIKE sy-index.

  CLEAR : it_coep, it_coep[],
          it_covp, it_covp[]..


  REFRESH it_fsc_gr.
  REFRESH it_covp.

*//Modify..03/28/2011..T00020..
  PERFORM get_covp_summary_t00020.
*
*  LOOP AT it_fsc_mat.
*    PERFORM get_covp_summary.
*  ENDLOOP.
*//
* vrgng, beknz, parob -> transaction

*Original HRKFT
*L Quantity of goods manufactured
*P Primary costs
*S Secondary costs
*B Variances and

*  SORT it_mat     BY matnr bwkey.
  SORT it_fsc_mat BY objnr.
  SORT it_csla    BY lstar.
  SORT it_categ   BY objnr kstar vrgng.

* Local Data Definition
  DATA : BEGIN OF it_lt_cospa OCCURS 100, "LIKE STANDARD TABLE OF cospa
           objnr LIKE covp-objnr,
           kstar LIKE covp-kstar,
           vrgng LIKE covp-vrgng,
           beweg LIKE kkbcs_out-beweg,

           pargb LIKE cosp-pargb,
           beknz LIKE cosp-beknz,
          END OF it_lt_cospa.

  DATA : it_lt_cossa LIKE it_lt_cospa OCCURS 100 WITH HEADER LINE.

*  "STANDARD TABLE OF cossa
  DATA: lv_cospa LIKE cospa,
        lv_cossa LIKE cossa.

*TEXT split
  LOOP AT it_covp.
    CLEAR it_categ.
    READ TABLE it_categ WITH TABLE KEY objnr = it_covp-objnr
                                       kstar = it_covp-kstar
                                       vrgng = it_covp-vrgng.
    IF sy-subrc <> 0.
      CLEAR lv_cospa.
      MOVE-CORRESPONDING it_covp TO lv_cospa.
      MOVE-CORRESPONDING it_covp TO it_categ.

      CALL FUNCTION 'Z_K_KKB_BEWEG_SET'
        EXPORTING
          i_cospa  = lv_cospa
        IMPORTING
          e_beweg  = it_categ-beweg    "Business Transaction
        EXCEPTIONS
          no_input = 1
          OTHERS   = 2.

      INSERT it_categ INTO TABLE it_categ.
    ENDIF.


    CLEAR: it_coep.
    MOVE-CORRESPONDING it_covp TO it_coep.

*Item Category (C,F,L->M,E,V) GR-O,Settle-X
*KMMG: C,L,P,X
* RKIU = assessment
* RKL = activity
* KAMV = manual alloc

* Commented out by Han Moon
*    READ TABLE it_categ WITH TABLE KEY objnr = it_covp-objnr
*                                       kstar = it_covp-kstar
*                                       vrgng = it_covp-vrgng.
*    IF sy-subrc = 0.
    PERFORM conv_beweg_to_typps  USING it_categ-beweg
                                       it_coep-typps.
*    ENDIF.

*Split object key (CC+ACT)
    IF it_covp-parob(2) = 'KL'.
      it_coep-kostl = it_covp-parob+6(10).
      it_coep-lstar = it_covp-parob+16(6).
      it_coep-typps = 'E'.                   "Added by Han Moon
*FIXME
*Plant blank - Revaluation
      CLEAR: it_coep-werks, it_coep-vrgng, it_coep-hrkft.

    ENDIF.


*//Modify..03/28/2011..T00020..
*---manual allocation (material cost)
    CLEAR it_kstar.
    READ TABLE it_kstar WITH TABLE KEY kstar = it_covp-kstar.
    IF it_covp-vrgng = 'RKU1' AND
       it_kstar-elemt BETWEEN '010' AND '050'.
      it_coep-typps = 'V'.
      SPLIT it_covp-sgtxt AT ';' INTO l_matnr l_zbwart l_budat.
      IF l_matnr <> space.
        PERFORM check_it_mat USING    l_matnr it_covp-werks
                             CHANGING sy-subrc.
        IF sy-subrc = 0.
          it_covp-matnr = l_matnr.
          it_coep-matnr = l_matnr.
*        ELSE.
*          PERFORM add_it_mat_tmp USING l_matnr it_covp-werks
*                                  CHANGING sy-subrc.
*          IF sy-subrc = 0.
*            it_covp-matnr = l_matnr.
*            it_coep-matnr = l_matnr.
*          ENDIF.
        ENDIF.

        PERFORM add_it_mat_tmp USING l_matnr it_covp-werks
                                 CHANGING sy-subrc.

        CASE l_zbwart.
          WHEN 'OS&D'.
            it_coep-osnd_amt = it_coep-wkgbtr.
            it_coep-osnd_qty = it_coep-mbgbtr.
          WHEN OTHERS.
            it_coep-keyin_amt = it_coep-wkgbtr.
            it_coep-keyin_qty = it_coep-mbgbtr.
        ENDCASE.
*        IF l_505905(2) = 'OS'.
*          it_coep-scrap_amt = it_coep-wkgbtr.
*          it_coep-scrap_qty = it_coep-mbgbtr.
*        ENDIF.
      ENDIF.
    ENDIF.
*//

*GET material from Z-TABLE
    IF it_coep-typps = 'V' AND it_coep-matnr = ''.
      "MESSAGE s000 WITH 'Warning-material blank'  it_coep-objnr.

    ELSEIF it_coep-typps = 'M' AND it_coep-matnr = ''.
      it_coep-typps = 'F'.   "Finished Goods??
*     message s000 with 'Error-Material blank'    it_coep-objnr.
*     break-point.  "FIXME - ANDY

    ELSEIF it_coep-typps = 'O'.

      CLEAR: it_obj_gr.
      it_obj_gr-objnr = it_covp-objnr.
      it_obj_gr-werks = it_covp-werks.
      it_obj_gr-matnr = it_covp-matnr.
      it_obj_gr-grqty = - it_covp-mbgbtr.
      COLLECT it_obj_gr.

      CLEAR it_fsc_gr.
      it_fsc_gr-werks  = it_obj_gr-werks.
      it_fsc_gr-matnr  = it_obj_gr-matnr.
      it_fsc_gr-grqty  = it_obj_gr-grqty.
      COLLECT it_fsc_gr.

    ENDIF.


    IF it_coep-matnr <> space.
      PERFORM check_it_mat USING    it_coep-matnr it_coep-werks
                           CHANGING sy-subrc.
      IF sy-subrc = 0.
        IF it_mat-meins <> it_coep-meinb.
*//Modify..03/29/2011..T00020..   WHY NOT??? ANDY
          PERFORM unit_converion_new USING it_coep-mbgbtr it_coep-meinb
                                       it_mat-meins
                              CHANGING it_coep-mbgbtr.
          it_coep-meinb = it_mat-meins.
        ENDIF.
      ENDIF.
*      READ TABLE it_mat WITH KEY  matnr = it_coep-matnr
*                                  bwkey = it_coep-werks
*                        BINARY SEARCH.
*      IF sy-subrc = 0.
*        IF it_mat-meins <> it_coep-meinb.
**//Modify..03/29/2011..T00020..   WHY NOT??? ANDY
*          PERFORM unit_converion USING it_coep-mbgbtr it_coep-meinb
*                                       it_mat-meins
*                              CHANGING it_coep-mbgbtr.
*          it_coep-meinb = it_mat-meins.
*        ENDIF.
*      ELSE.
*        PERFORM add_it_mat_tmp USING it_coep-matnr it_coep-werks
*                                CHANGING sy-subrc.
*      ENDIF.

*-----capture self-gi
      IF it_coep-typps = 'M'.
        READ TABLE it_fsc_mat WITH KEY objnr = it_coep-objnr
                                       BINARY SEARCH.
        IF it_coep-matnr = it_fsc_mat-matnr.
          MOVE-CORRESPONDING it_coep TO it_coep_self.
          COLLECT it_coep_self.
          CLEAR   it_coep_self.
        ENDIF.
      ENDIF.

* Change unit :  'STD' => Activity master unit
*    elseif it_coep-lstar <> space and it_coep-meinb <> 'STD'.
    ELSEIF it_coep-lstar <> space .

      CLEAR it_csla.
      READ TABLE it_csla WITH KEY lstar = it_coep-lstar
                                  BINARY SEARCH.
      IF sy-subrc = 0 AND it_csla-leinh <> it_coep-meinb.
*//Modify..03/29/2011..T00020..   WHY NOT??? ANDY
        PERFORM unit_converion_new  USING it_coep-mbgbtr it_coep-meinb
                                      it_csla-leinh
                             CHANGING it_coep-mbgbtr.
        it_coep-meinb = it_csla-leinh.
      ENDIF.

    ENDIF.


*---clear unwanted fields for collect
    CLEAR: it_coep-parob, it_coep-vrgng.
    IF it_covp-parob(2) = 'KL'.
      CLEAR: it_coep-werks, it_coep-vrgng, it_coep-hrkft.
    ENDIF.


*---collect
    COLLECT it_coep.


*Manual Repost RKU1 / Lineitem report RKU3?
    IF  it_coep-vrgng(3) = 'RKU'.
      it_gi_misc-objnr      = it_covp-objnr.
      it_gi_misc-matnr      = it_covp-matnr.
      it_gi_misc-kstar      = it_covp-kstar.
      it_gi_misc-add_wkgbtr = it_covp-wkgbtr.
      it_gi_misc-add_mbgbtr = it_covp-mbgbtr.
      COLLECT it_gi_misc.
      CLEAR   it_gi_misc.
    ENDIF.                                                  "UD1K920985

*//Modify..03/28/2011..T00020..
**MTO,MTS addtional Man/Mch Hour
*    IF  it_covp-awtyp = 'AFRU' AND it_coep-vrgng = 'RKL'.
**                              AND ( it_gcovp-kstar = '0000836001' or '0000836002' )
*      CLEAR: it_mts_mh.
*      MOVE-CORRESPONDING it_covp TO it_mts_mh.
*      COLLECT it_mts_mh.
*    ENDIF.
*//

  ENDLOOP.


  SORT it_gi_misc BY objnr kstar matnr.

*//Modify..03/25/2011..T00020..
**Summary of GR
*  REFRESH: it_fsc_gr.
*  LOOP AT it_obj_gr.
*    CLEAR it_fsc_gr.
*    it_fsc_gr-werks  = it_obj_gr-werks.
*    it_fsc_gr-matnr  = it_obj_gr-matnr.
*    it_fsc_gr-grqty  = it_obj_gr-grqty.
*    COLLECT it_fsc_gr.
*  ENDLOOP.

**Convert UoM of COEP
*  LOOP AT it_coep.
*
*    l_idx = sy-tabix.
*
*    IF it_coep-matnr <> space.
*
*      READ TABLE  it_mat WITH KEY  matnr = it_coep-matnr
*                                   bwkey = it_coep-werks
*                         BINARY SEARCH.
*      IF sy-subrc = 0 AND it_mat-meins <> it_coep-meinb.
*        PERFORM unit_converion USING it_coep-mbgbtr it_coep-meinb
*                                     it_mat-meins
*                            CHANGING it_coep-mbgbtr.
*        it_coep-meinb = it_mat-meins.
*        MODIFY it_coep INDEX l_idx.
*      ENDIF.
*
**-----capture self-gi
*      IF it_coep-typps = 'M'.
*        READ TABLE it_fsc_mat WITH KEY objnr = it_coep-objnr
*                                       BINARY SEARCH.
*        IF it_coep-matnr = it_fsc_mat-matnr.
*          MOVE-CORRESPONDING it_coep TO it_coep_self.
*          COLLECT it_coep_self.
*          CLEAR   it_coep_self.
*        ENDIF.
*      ENDIF.
*
** Change unit :  'STD' => Activity master unit
**    elseif it_coep-lstar <> space and it_coep-meinb <> 'STD'.
*    ELSEIF it_coep-lstar <> space .
*      CLEAR it_csla.
*      READ TABLE it_csla WITH KEY lstar = it_coep-lstar.
*      IF it_csla-leinh <> it_coep-meinb.
*        PERFORM unit_converion  USING it_coep-mbgbtr it_coep-meinb
*                                      it_csla-leinh
*                             CHANGING it_coep-mbgbtr.
*        it_coep-meinb = it_csla-leinh.
*        MODIFY it_coep INDEX l_idx.
*      ENDIF.
*    ENDIF.
*
*    CLEAR it_coep.
*
*  ENDLOOP.
*//

ENDFORM.                    " READ_CO_DOCS

*&---------------------------------------------------------------------*
*&      Form  MAKING_MAIN_TAB
*&---------------------------------------------------------------------*
*       Making MAIN Itab
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM making_main_tab.
* Local Data Definition
  DATA: l_matnr     LIKE mara-matnr.

  DATA: wa_l_ionra  LIKE ionra,
        w_shopcost  LIKE it_shop_sum.

* Clear
  CLEAR : it_shop_sum, it_shop_sum[].
  CLEAR : it_coep.
  CLEAR : it_fsc_mat, it_mat .

*1. COLLECT COEP
*  IF p_coep = 'X'.
  PERFORM move_coep.
*  ENDIF.
* SORT IT_SHOP_SUM BY objnr llv_matnr kostl lstar.


**//Modify..03/24/2011..T00020..
**1.1 Scrap
*  PERFORM move_osnd_data.
**//


*2. COLLECT GR-item
*  IF p_gr   = 'X'.
  PERFORM move_gr_items.
*  ENDIF.

*3. COLLECTING WIP
*  IF p_wips = 'X'.
  PERFORM move_wip.
*  ENDIF.

*4. ML
*  IF p_ml = 'X'.
  PERFORM move_ml_data.

*NO NEED... Only Current Period
  IF 1 = 2.
    PERFORM get_prev_multi.
    PERFORM move_pre_ml_data.
  ENDIF.
*  ENDIF.

*  PERFORM RE_CREATE_WIP.

*5. Calc.
  PERFORM calc_shopcst.


ENDFORM.                    " MAKING_MAIN_TAB

*&---------------------------------------------------------------------*
*&      Form  CONV_BEWEG_TO_TYPPS
*&---------------------------------------------------------------------*
*       Convert Btr to Item Category
*----------------------------------------------------------------------*
*      -->P_BEWEG   Business Transaction on Manufacturing Orders
*      -->P_TYPPS   Item category
*----------------------------------------------------------------------*
FORM conv_beweg_to_typps USING    p_beweg LIKE kkbcs_out-beweg
                                  p_typps TYPE typps.
*C  Goods issues
*F  Confirmations
*I  Overhead
*L  Miscellaneous
*P  Goods receipt
*X  Settlement
*O  Distribution
  CASE p_beweg.
    WHEN 'C'.     p_typps = 'M'.
    WHEN 'F'.     p_typps = 'E'.
    WHEN 'L'.     p_typps = 'V'.
    WHEN 'P'.     p_typps = 'O'.

    WHEN 'X'.     p_typps = 'X'.
    WHEN 'I'.     p_typps = 'I'.
  ENDCASE.

ENDFORM.                    " CONV_BEWEG_TO_TYPPS

*&---------------------------------------------------------------------*
*&      Form  PUT_SHOP_MAT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM put_shop_mat_info.

  PERFORM progress_ind USING '80' text-280.

  DATA: l_idx LIKE sy-tabix.

* RP Master Data
*  CLEAR : it_zvco_rp1, it_zvco_rp1[].
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zvco_rp1
*           FROM zvco_rp1
*          WHERE plnnr = c_rp_plnnr.


  SORT it_shop_sum BY artnr aufnr resou.
  SORT it_fsc_mat  BY objnr matnr.
  SORT it_fsc_gr   BY werks matnr.
*  SORT it_mat      BY matnr bwkey bwtar.
  SORT it_act      BY kostl lstar.

  LOOP AT it_shop_sum.
*    it_shop_sum-waers = tka01-waers.
    l_idx = sy-tabix.

    PERFORM fill_shopcost_header.  "Not used..

    CLEAR it_fsc_mat.
    READ TABLE it_fsc_mat WITH KEY objnr = it_shop_sum-objnr
                                   BINARY SEARCH.

    it_shop_sum-aufnr      = it_fsc_mat-aufnr  .
    it_shop_sum-artnr      = it_fsc_mat-matnr  .
    it_shop_sum-verid      = it_fsc_mat-verid  .
    it_shop_sum-par_kalnr  = it_fsc_mat-kaln1  .
    it_shop_sum-par_werks  = it_fsc_mat-werks  .
    it_shop_sum-kalst      = it_fsc_mat-kalst  .

    it_shop_sum-categ      = it_fsc_mat-categ.  "Modify..03/29/2011..T00020

    CLEAR it_mat.

    CASE it_shop_sum-typps.

      WHEN 'M'.
        PERFORM check_it_mat USING    it_shop_sum-llv_matnr
                                      it_shop_sum-bwkey
                             CHANGING sy-subrc.
*        READ TABLE it_mat WITH KEY matnr = it_shop_sum-llv_matnr
*                                   bwkey = it_shop_sum-bwkey
*                                   bwtar = it_shop_sum-bwtar
*                                   BINARY SEARCH.
*        IF sy-subrc <> 0.
*          PERFORM read_material_add USING it_shop_sum-llv_matnr
*                                          it_shop_sum-bwkey
*                                    CHANGING it_mat.
*        ENDIF.

        it_shop_sum-fevor      = it_mat-fevor.
        it_shop_sum-chd_kalnr  = it_mat-kaln1.
        it_shop_sum-peinh      = it_mat-peinh.
        it_shop_sum-stprs      = it_mat-stprs.
        it_shop_sum-verpr      = it_mat-verpr.

        it_shop_sum-vmpec      = it_mat-vmpec.
        it_shop_sum-vmstc      = it_mat-vmstc.
        it_shop_sum-vmpep      = it_mat-vmpep.
        it_shop_sum-vmstp      = it_mat-vmstp.

*        PERFORM read_shop_name.

*    IT_SHOP_SUM-beskz =  it_mat-beskz. "Procurement Type
*    IT_SHOP_SUM-sobsl     =  it_mat-sobsl. "Special procurement type
*    IT_SHOP_SUM-vspvb     =  it_mat-vspvb. "Proposed Supply Area

      WHEN 'E'.
        READ TABLE it_act WITH KEY kostl = it_shop_sum-kostl
                                   lstar = it_shop_sum-lstar
                               BINARY SEARCH.
        it_shop_sum-chd_kalnr  = it_act-kalnr.

*Current period of WIP
        READ TABLE it_unitpc WITH KEY costcenter = it_shop_sum-kostl
                                      acttype    = it_shop_sum-lstar
                                      BINARY SEARCH.
        IF sy-subrc <> 0 AND p_debug = 'X'. BREAK-POINT. ENDIF.
*Standard price
        it_shop_sum-stprs = it_unitpc-price_ocurr_fix +
                           it_unitpc-price_ocurr_var.
*Price Unit
        it_shop_sum-peinh = it_unitpc-price_ocurr_unit.

        READ TABLE it_ccr1t WITH KEY kostl = it_shop_sum-kostl
                                     lstar = it_shop_sum-lstar
                                     BINARY SEARCH.
        IF  it_ccr1t-tkexxx <> 0 .
*Moving average price
          it_shop_sum-verpr = it_ccr1t-tkgxxx *
                     it_shop_sum-peinh / it_ccr1t-tkexxx.
        ENDIF.

        it_shop_sum-vmpec = it_shop_sum-peinh.
        it_shop_sum-vmstc = it_shop_sum-stprs.

*previous period of WIP
        READ TABLE it_unitpp WITH KEY costcenter = it_shop_sum-kostl
                                      acttype    = it_shop_sum-lstar
                                      BINARY SEARCH.
        IF sy-subrc = 0.
          it_shop_sum-vmstp = it_unitpp-price_ocurr_fix +
                              it_unitpp-price_ocurr_var.
          it_shop_sum-vmpep = it_unitpp-price_ocurr_unit.

        ENDIF.

*        PERFORM read_shop_name.

      WHEN 'V'.
*        PERFORM read_shop_name.

    ENDCASE.

    PERFORM read_shop_name.

*//Modify..03/29/2011..T00020
*    CLEAR it_fsc_mat.
*    READ TABLE it_fsc_mat WITH KEY matnr = it_shop_sum-artnr
*                          BINARY SEARCH.
** Costing category; DI or NOT
*    it_shop_sum-categ = it_fsc_mat-categ.
*//

* Target Qty Amt
    IF it_fsc_mat-categ =  'DI'.
      it_shop_sum-target_qty  = it_shop_sum-apo_output_qty.
      it_shop_sum-target_amt  = it_shop_sum-apo_output_qty *
                         it_shop_sum-stprs / it_shop_sum-peinh.
    ELSE.
      it_shop_sum-target_qty  = it_shop_sum-gr_qty.
      it_shop_sum-target_amt  = it_shop_sum-gr_qty *
                         it_shop_sum-stprs / it_shop_sum-peinh.
    ENDIF.

* Control amt ?= manuf. amount
    it_shop_sum-control_amt = it_shop_sum-manu_qty *
                         it_shop_sum-verpr / it_shop_sum-peinh.



*MPRICE = Costing Lot size * Manufacturing Cost / GR Qty
*MPUNIT = Costing Lot size
    CLEAR it_fsc_gr.
    READ TABLE it_fsc_gr WITH KEY werks = it_shop_sum-par_werks
                                  matnr = it_shop_sum-artnr
                                  BINARY SEARCH.
    IF it_fsc_gr-grqty <> 0 .
      it_shop_sum-mprice =  it_shop_sum-manu_qty * it_fsc_gr-lotsize /
                            it_fsc_gr-grqty.
    ENDIF.
    it_shop_sum-mpunit =  it_fsc_gr-lotsize.

*-- Put into Main table!!!
    MODIFY it_shop_sum INDEX l_idx.

* collect to WIP sum table
    MOVE-CORRESPONDING it_shop_sum TO it_obj_sum_wip.

    COLLECT it_obj_sum_wip. CLEAR it_obj_sum_wip.

  ENDLOOP.

*  delete it_shop_sum where kstar = 540400.

ENDFORM.                    " PUT_SHOP_MAT_INFO

*&---------------------------------------------------------------------*
*&      Form  READ_COST_COMP
*&---------------------------------------------------------------------*
*       Read Cost Comp. Number
*----------------------------------------------------------------------*
*      -->P_KSTAR  Cost Element
*      -->P_ELEMT  Cost Components
*----------------------------------------------------------------------*
*FORM read_cost_comp USING    p_kstar
*                             p_elemt.
*  CLEAR it_cskb.
*  READ TABLE it_cskb WITH KEY kstar = p_kstar.
*  p_elemt = it_cskb-elemt.
*
*ENDFORM.                    " READ_COST_COMP

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_FR_ROUTING_PRV
*&---------------------------------------------------------------------*
*       Read SHOP Info. from Routing/Production version
* refer: ZACO09U_SHOP_NEW
*----------------------------------------------------------------------*
*      -->P_MATNR  Material
*      -->P_SHOP   Shop
*      -->P_WERKS  Plant
*      -->P_BDATJ  Year
*      -->P_POPER  Period
*      -->P_KOKRS  Controlling Area
*----------------------------------------------------------------------*
FORM read_shop_fr_routing_prv USING    f_matnr
                                       f_shop
                                       f_werks
                                       f_bdatj
                                       f_poper
                                       f_kokrs.


  CLEAR : crhd, plpo, plko.
* Read Shop From Routing
  DATA : lv_arbid LIKE plpo-arbid.
  DATA : lv_fdper LIKE sy-datum.
  DATA : lv_ldper LIKE sy-datum.

  gv_verwe  = '1'.

  CALL FUNCTION 'K_PERIODS_GET'
    EXPORTING
      par_igjahr    = f_bdatj
      par_ipoper    = f_poper
      par_kokrs     = f_kokrs
*     PAR_PREVP     = ' '
*     PAR_SPEOK     = ' '
*     PAR_NEXTP     = ' '
    IMPORTING
*     PAR_ANZBP     =
*     PAR_ANZSP     =
*     PAR_EGJAHR    =
*     PAR_EPOPER    =
      par_fdper     = lv_fdper
      par_ldper     = lv_ldper
    EXCEPTIONS
      kokrs_invalid = 1
      poper_invalid = 2
      OTHERS        = 3.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  CLEAR mkal.
  SELECT SINGLE *  FROM mkal
                  WHERE matnr = f_matnr
                    AND werks = f_werks
                    AND bdatu >= lv_ldper
                    AND adatu <= lv_fdper.
*                   AND BDATU >= IT_SHOP_SUM-BIDAT
*                   AND ADATU <= IT_SHOP_SUM-KADAT.

  CHECK sy-subrc = 0.

  SELECT SINGLE  plpo~arbid  INTO lv_arbid
                  FROM plko INNER JOIN plpo
                    ON plko~plnty = plpo~plnty
                   AND plko~plnnr = plpo~plnnr
                 WHERE
                    (     plko~plnty = mkal-pltyg
                     AND  plko~plnnr = mkal-plnng
                     AND  plko~plnal = mkal-alnag
                     AND  plko~verwe = gv_verwe   )
                  OR
                    (     plko~plnty = mkal-pltym
                     AND  plko~plnnr = mkal-plnnm
                     AND  plko~plnal = mkal-alnam
                     AND  plko~verwe = gv_verwe   )
                  OR
                    (     plko~plnty = mkal-plnty
                     AND  plko~plnnr = mkal-plnnr
                     AND  plko~plnal = mkal-alnal
                     AND  plko~verwe = gv_verwe   ).


  CHECK sy-subrc = 0.

  CLEAR crhd.
  SELECT SINGLE *  FROM crhd
                  WHERE objid =  lv_arbid.

  CHECK sy-subrc = 0.

* Work Center = Cost center (1:1)
  CLEAR it_cctr.
  READ TABLE it_cctr WITH KEY kostl = crhd-arbpl.

  IF sy-subrc = 0.
    f_shop = it_cctr-shop.
  ENDIF.

ENDFORM.                    " READ_SHOP_FR_ROUTING_PRV

*&---------------------------------------------------------------------*
*&      Form  UNIT_CONVERION
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
FORM unit_converion_new USING    p_input
                             p_unit_in
                             p_unit_out
                    CHANGING p_output.

  IF p_unit_in = p_unit_out.
    p_output = p_input.
  ELSE.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = p_input
        unit_in              = p_unit_in
        unit_out             = p_unit_out
      IMPORTING
        output               = p_output
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.
*    IF sy-subrc <> 0.
*      IF p_debug = 'X'.
*        BREAK-POINT.
*      ENDIF.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    IF sy-subrc <> 0.
*           ALT UoM
      DATA : l_umrez_f TYPE umrez,
             l_umrez_t TYPE umrez.

      CLEAR : l_umrez_f,l_umrez_t.

      SELECT SINGLE umrez umren INTO :
                (l_umrez_f, l_umrez_t) FROM marm
               WHERE matnr = it_coep-matnr
               AND meinh = p_unit_in.

      IF l_umrez_f <> 0 AND  l_umrez_t <> 0.
        p_output = p_input * ( l_umrez_f / l_umrez_t ).
      ELSE.
        p_output = 1.
      ENDIF.
    ENDIF.


  ENDIF.
ENDFORM.                    " UNIT_CONVERION

*&---------------------------------------------------------------------*
*&      Form  DEL_DATA_FR_ztco_shopcost_a2
*&---------------------------------------------------------------------*
*       - > Always deletion -> Refresh data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM del_data_fr_ztco_shop_sum.

  MESSAGE s000 WITH 'Delete DB ...ztco_shop_sum_1'.

* Analyzed Data : Cost By Shop (Actual) - Summary
  DELETE FROM ztco_shop_sum_1
     WHERE
              kokrs        =  p_kokrs
          AND bdatj        =  p_bdatj
          AND poper        =  p_perab
*         and bwkey        in s_werks
          AND artnr        IN s_matnr
          AND aufnr        IN s_aufnr.
  IF sy-subrc = 0.
  ENDIF.
* No- Error Check In Deletion Phase

  IF p_ccs = 'X'.               "Cost component view

    MESSAGE s000 WITH 'Delete DB ...ZTCO_SHOP_CC'.

* Analyzed Data : Cost By Shop (Actual)
    DELETE FROM ztco_shop_cc_1
       WHERE
                kokrs        =  p_kokrs
            AND bdatj        =  p_bdatj
            AND poper        =  p_perab
*          AND bwkey        IN s_werks
            AND artnr        IN s_matnr
            AND aufnr        IN s_aufnr.
    IF sy-subrc = 0.
    ENDIF.
* No- Error Check In Deletion Phase
  ENDIF.

ENDFORM.                    " DEL_DATA_FR_ztco_shopcost_a2

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ztco_shopcost_a2
*&---------------------------------------------------------------------*
*       Update/Insert
*----------------------------------------------------------------------*
FORM update_ztco_shop_sum.

  DATA wa_zsco_shop_sum LIKE zsco_shop_sum.
  DATA wa_zsco_shop_cc  LIKE zsco_shop_cc.
  DATA $flag.
  DATA l_tabix          TYPE sy-tabix.

  FIELD-SYMBOLS <fs> TYPE any.

  PERFORM progress_ind USING '93' text-300.

*//Modify..03/29/2011..T00020..
*  LOOP AT it_shop_sum.
*
*    it_shop_sum-manu_qty =
*            it_shop_sum-mbgbtr  + it_shop_sum-add_mbgbtr
*          + it_shop_sum-mbgbtr2                          "PPCVAR
*          - it_shop_sum-wip_qty + it_shop_sum-wip_pqty.
*
*    it_shop_sum-manu_amt =
*          + it_shop_sum-gr_amt
*          + it_shop_sum-single_amt
*          + it_shop_sum-multi_amt.
**          - IT_SHOP_SUM-multi_pre_amt.
*
*    MODIFY it_shop_sum . CLEAR it_shop_sum.
*
*  ENDLOOP.
*//

  SORT it_shop_sum BY artnr aufnr kstar resou.

  MESSAGE s000 WITH 'Insert DB ...ztco_shop_sum_1'.

  LOOP AT it_shop_sum.

*//Modify..03/29/2011..T00020..
    l_tabix = sy-tabix.

    it_shop_sum-manu_qty =
            it_shop_sum-mbgbtr  + it_shop_sum-add_mbgbtr
          + it_shop_sum-mbgbtr2                          "PPCVAR
          - it_shop_sum-wip_qty + it_shop_sum-wip_pqty.

    it_shop_sum-manu_amt =
          + it_shop_sum-gr_amt
          + it_shop_sum-single_amt
          + it_shop_sum-multi_amt.
*          - IT_SHOP_SUM-multi_pre_amt.
*//

* by ig.moon 12/04/2008 {
    CLEAR wa_zsco_shop_sum.
    MOVE-CORRESPONDING it_shop_sum TO wa_zsco_shop_sum .

    CLEAR $flag.

    DO 41 TIMES.
      ASSIGN COMPONENT sy-index OF
             STRUCTURE wa_zsco_shop_sum TO <fs>.
      IF NOT <fs> IS INITIAL.
        $flag = 'X'.
      ENDIF.
    ENDDO.

    IF $flag IS INITIAL.
      CONTINUE.
    ENDIF.
* }

* LOG
    it_shop_sum-erdat = sy-datum.
    it_shop_sum-erzet = sy-uzeit.
    it_shop_sum-ernam = sy-uname.
* CURKY
    CLEAR ztco_shop_sum_1.
    MOVE-CORRESPONDING  it_shop_sum TO ztco_shop_sum_1.

    ztco_shop_sum_1-kokrs = p_kokrs.
    ztco_shop_sum_1-bdatj = p_bdatj.
    ztco_shop_sum_1-poper = p_perab.

* Analyzed Data : Cost By Shop (Actual) - Summary
    INSERT ztco_shop_sum_1 .
    IF sy-subrc <> 0.
      MESSAGE s000 WITH '**Insert error-SUM'
                        ztco_shop_sum_1-artnr
                        ztco_shop_sum_1-resou
                        ztco_shop_sum_1-aufnr.
      IF p_debug = 'X'.
        BREAK-POINT.
      ENDIF.
    ENDIF.


    MODIFY it_shop_sum INDEX l_tabix.
    CLEAR it_shop_sum.

  ENDLOOP.


  IF p_ccs = 'X'.               "Cost component view

    PERFORM progress_ind USING '95' text-301.


    MESSAGE s000 WITH 'Insert DB ...ZTCO_SHOP_CC'.

    LOOP AT it_shop_cc.

* by ig.moon 12/04/2008 {
      CLEAR wa_zsco_shop_cc.
      MOVE-CORRESPONDING it_shop_cc TO wa_zsco_shop_cc .

      CLEAR $flag.
      DO 30 TIMES.
        ASSIGN COMPONENT sy-index OF
               STRUCTURE wa_zsco_shop_cc TO <fs>.
        IF NOT <fs> IS INITIAL.
          $flag = 'X'.
        ENDIF.
      ENDDO.

      IF $flag IS INITIAL.
        CONTINUE.
      ENDIF.
* }

* CURKY
      CLEAR ztco_shop_cc_1.
      MOVE-CORRESPONDING  it_shop_cc TO ztco_shop_cc_1.

      ztco_shop_cc_1-kokrs = p_kokrs.
      ztco_shop_cc_1-bdatj = p_bdatj.
      ztco_shop_cc_1-poper = p_perab.
* LOG
      ztco_shop_cc_1-erdat = sy-datum.
      ztco_shop_cc_1-erzet = sy-uzeit.
      ztco_shop_cc_1-ernam = sy-uname.

* Analyzed Data : Cost By Shop (Actual)
      INSERT ztco_shop_cc_1 .
      IF sy-subrc <> 0.
        MESSAGE s000 WITH '**Insert error-CC '
                          ztco_shop_cc_1-artnr
                          ztco_shop_cc_1-resou
                          ztco_shop_cc_1-aufnr.
        IF p_debug = 'X'.
          BREAK-POINT.
        ENDIF.
      ENDIF.

      CLEAR it_shop_cc.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " UPDATE_ztco_shopcost_a2
*&---------------------------------------------------------------------*
*&      Form  READ_DE_UNIT_OF_CSLA
*&---------------------------------------------------------------------*
*       Read default Unit of CSLA
*----------------------------------------------------------------------*
FORM read_de_unit_of_csla.

* Local Data Definition
  DATA : lv_datum LIKE sy-datum.
  DATA : lv_input TYPE i VALUE '1'.

* Read CSLA : Activity master
  CLEAR : it_csla, it_csla[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_csla
           FROM csla INNER JOIN t006
             ON csla~leinh = t006~msehi
            AND t006~dimid = 'TIME'
           WHERE csla~kokrs = p_kokrs.

  IF  it_csla[] IS INITIAL.
    MESSAGE e000 WITH text-103.
  ENDIF.

  DATA l_tabix TYPE sy-tabix.

* Read Deb. Num.
  LOOP AT it_csla.
    l_tabix = sy-tabix.
* To STD
    it_csla-leinh_out = 'STD'.

* get Numerator / Denominator
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = lv_input
*       NO_TYPE_CHECK        = 'X'
*       ROUND_SIGN           = ' '
        unit_in              = it_csla-leinh
        unit_out             = it_csla-leinh_out
      IMPORTING
*       ADD_CONST            =
*       DECIMALS             =
        denominator          = it_csla-denominator
        numerator            = it_csla-numerator
*       OUTPUT               =
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.

    IF sy-subrc <> 0.
      IF p_debug = 'X'.
        BREAK-POINT.
      ENDIF.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Modify
    MODIFY it_csla INDEX l_tabix.
    CLEAR  it_csla.

  ENDLOOP.

ENDFORM.                    " READ_DE_UNIT_OF_CSLA

*&---------------------------------------------------------------------*
*&      Form  set_date
*&---------------------------------------------------------------------*
FORM set_date.
  MOVE: p_perab TO w_perbi.
ENDFORM.                    " set_date
*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_IN_SHOP
*&---------------------------------------------------------------------*
*       Read CCtrs linked to SHOP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr_in_shop.
*REFER ZACO09U_SHOP_NEW
* Read CCtrs
  CLEAR : it_costcenterlist, it_costcenterlist[].
  CLEAR : it_return, it_return[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
    EXPORTING
      controllingarea = p_kokrs
      date_from       = g_frdat
      costcentergroup = p_ksgru  "c_gp_kostl_e
    TABLES
      costcenterlist  = it_costcenterlist
      return          = it_return.

  IF it_costcenterlist[] IS INITIAL.
    MESSAGE e080 WITH p_ksgru p_kokrs g_frdat.
  ENDIF.

* Read SHOP (Linkage bwtween CCtrs and Shops)
* Read Hierarchy From Object ID, Read CCtr from CCgrp 'HMMA-SHOP'
  PERFORM read_hi_fr_setid(saplzgco_global_form)
                            TABLES it_nodes
                                   it_values
                            USING  p_kokrs
                                   '0101'
                                   p_ksgru. "c_gp_kostl_e.
  CLEAR : it_cctr , it_cctr[].
  SORT it_nodes BY setid.

  LOOP AT it_costcenterlist.

    CLEAR it_values.
    LOOP AT it_values WHERE vfrom =< it_costcenterlist-costcenter
                        AND vto   => it_costcenterlist-costcenter.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      READ TABLE it_nodes WITH KEY setid = it_values-setid
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT it_nodes FROM sy-tabix.
          IF it_nodes-setid <> it_values-setid.
            EXIT.
          ENDIF.
          it_cctr-shop  =  it_nodes-shortname.
          it_cctr-kostl =  it_costcenterlist-costcenter.
          APPEND  it_cctr.
          CLEAR   it_cctr.
        ENDLOOP.
      ENDIF.
    ENDIF.

    CLEAR it_costcenterlist.
  ENDLOOP.


* get plan amt (no actual)
  CALL FUNCTION 'BAPI_CTR_GETACTIVITYPRICES'
    EXPORTING
      coarea         = p_kokrs
      fiscyear       = p_bdatj
      version        = '000'
      costcenterfrom = '0'
      costcenterto   = 'ZZZZZZZZZZ'
      costcentergrp  = p_ksgru "'DIRECT'
      acttypefrom    = '0'
      acttypeto      = 'ZZZZZZ'
      periodfrom     = p_perab
    TABLES
      actprices      = it_unitpc
      return         = it_bapiret2.

  CALL FUNCTION 'CKML_F_GET_PREVIOUS_PERIOD'
    EXPORTING
      input_period    = p_perab
      input_year      = p_bdatj
      input_periv     = tka01-lmona
    IMPORTING
      previous_period = l_prev_perd
      previous_year   = l_prev_year.

  CALL FUNCTION 'BAPI_CTR_GETACTIVITYPRICES'
    EXPORTING
      coarea         = p_kokrs
      fiscyear       = l_prev_year
      version        = '000'
      costcenterfrom = '0'
      costcenterto   = 'ZZZZZZZZZZ'
      costcentergrp  = p_ksgru "'DIRECT'
      acttypefrom    = '0'
      acttypeto      = 'ZZZZZZ'
      periodfrom     = l_prev_perd
    TABLES
      actprices      = it_unitpp
      return         = it_bapiret2.


ENDFORM.                    " READ_CCTR_IN_SHOP
*&---------------------------------------------------------------------*
*&      Form  READ_MH_DATA
*&---------------------------------------------------------------------*
*       Read M/H table - ZTCO_MHPCPOST
* read additional activity...
*----------------------------------------------------------------------*
FORM read_mh_data.

* Local Data Definition
  DATA : it_tmp_mhpcpost LIKE STANDARD TABLE OF ztco_mhpcpost
                         WITH HEADER LINE .

*//Modify..04/07/2011..T00020..
  DATA : BEGIN OF lt_cobk OCCURS 0,
           mandt  LIKE cobk-mandt,
           kokrs  LIKE cobk-kokrs,
           belnr  LIKE cobk-belnr,
           awtyp  LIKE cobk-awtyp,
           vrgng  LIKE cobk-vrgng,
         END OF lt_cobk.

  DATA : lt_afru LIKE afru    OCCURS 0 WITH HEADER LINE.
*//

  DATA: pf_gjper TYPE cpzp-gjper,
        w_lmnga  LIKE afru-lmnga.

  DATA: BEGIN OF %g00 OCCURS 0,
              objnr   LIKE mdks-objnr,
              f_objnr LIKE cpzp-f_objnr,
              meinh   LIKE cpzp-meinh,
*              matnr   LIKE mdks-matnr,
*              aufnr   LIKE mdks-aufnr,
              varmn   LIKE cpzp-varmn,   "Variance
              varamt 	 TYPE wkgxxx,       "Amt
              xmper   LIKE cpzp-xmper,   "Scrap
        END OF %g00.

* Clear
  CLEAR : it_mhsum, it_mhsum[].

**DI: vehicle... amount missing (FIXME)
*  pf_gjper = ( 1000 * p_bdatj ) + p_perab.
*
*  SELECT mdks~matnr mdks~objnr mdks~werks cpzp~f_objnr
*         cpzp~meinh cpzp~varmn cpzp~xmper mdks~aufnr
*  INTO CORRESPONDING FIELDS OF TABLE %g00
*  FROM ( cpzp INNER JOIN mdks
*         ON mdks~objnr = cpzp~objnr )
*         FOR ALL entries IN it_fsc_mat
*         WHERE cpzp~objnr = it_fsc_mat-objnr
*           AND cpzp~gjper = pf_gjper
*           AND mdks~matnr IN s_matnr
*           AND cpzp~f_objnr LIKE 'KL%'   "activity only
*           AND cpzp~varmn <> 0.

  REFRESH: it_mts_mh, lt_afru, lt_cobk. ", it_covp.
  CLEAR  : it_mts_mh, lt_afru, lt_cobk. ", it_covp.

  CHECK it_fsc_mat[] IS NOT INITIAL.

  DELETE it_covp WHERE NOT ( vrgng EQ 'RKL' AND awtyp EQ 'AFRU' ).
* Commented by Han Moon
**//Modify..04/18/2011..T00020..
*  LOOP AT it_fsc_mat.
*
*    SELECT   objnr kstar werks matnr
*             parob hrkft vrgng beknz meinb
*             SUM( wkgbtr ) SUM( mbgbtr )
*             sgtxt awtyp  refbn  aworg
*        APPENDING TABLE it_covp
*        FROM covp CLIENT SPECIFIED BYPASSING BUFFER
*        WHERE mandt =  sy-mandt
*          AND kokrs =  p_kokrs
*          AND objnr =  it_fsc_mat-objnr
*          AND gjahr =  p_bdatj
*          AND perio =  p_perab
*          AND wrttp =  c_gp_wrttp
*          AND versn =  p_versn
*          AND vrgng = 'RKL'
*          AND awtyp = 'AFRU'
*        GROUP BY
*                 objnr kstar werks matnr
*                 parob hrkft vrgng beknz meinb sgtxt
*                 awtyp  refbn  aworg
*        %_HINTS ORACLE 'LEADING "T_00"'.
*
*  ENDLOOP.
*<- by Han Moon


*  SELECT A~MANDT KOKRS BELNR AWTYP VRGNG
*    INTO CORRESPONDING FIELDS OF TABLE LT_COBK
*  FROM COBK AS A
*                 INNER JOIN AFRU AS B
*     ON A~REFBN =  B~RUECK
*    AND A~AWORG =  B~RMZHL
*  CLIENT SPECIFIED BYPASSING BUFFER
*  WHERE A~MANDT =  SY-MANDT
*  AND   A~KOKRS =  P_KOKRS
*  AND   A~GJAHR =  P_BDATJ
*  AND   A~BUDAT IN R_BUDAT
*  AND   B~LMNGA = 0
*  %_hints ORACLE 'leading(T_00) use_nl(T_00 T_01)'.
*
*  DELETE LT_COBK WHERE AWTYP  <> 'AFRU'
*                 AND   VRGNG  <> 'RKL'.
*
*  CHECK LT_COBK[] IS NOT INITIAL.
*
*  SELECT OBJNR PAROB MEINB MBGBTR WKGBTR
*    INTO CORRESPONDING FIELDS OF TABLE IT_MTS_MH
*    FROM COEP CLIENT SPECIFIED BYPASSING BUFFER
*    FOR ALL ENTRIES IN LT_COBK
*    WHERE MANDT =  LT_COBK-MANDT
*     AND  KOKRS =  LT_COBK-KOKRS
*     AND  BELNR =  LT_COBK-BELNR
*     AND  GJAHR =  P_BDATJ
*     AND  PERIO =  P_PERAB
*     AND  WRTTP =  C_GP_WRTTP
*     AND  VERSN =  P_VERSN.

*
**  SELECT objnr parob meinb mbgbtr wkgbtr
**    INTO CORRESPONDING FIELDS OF TABLE it_mts_mh
**    FROM afru AS a
**                   INNER JOIN cobk AS b
**      ON a~rueck = b~refbn
**     AND a~rmzhl = b~aworg
**                   INNER JOIN coep AS c
**      ON b~kokrs = c~kokrs
**     AND b~belnr = c~belnr
**
**     FOR ALL ENTRIES IN it_fsc_mat
**
**    WHERE a~lmnga  = 0
**      AND b~gjahr  = p_bdatj
**      AND b~awtyp  = 'AFRU'
**      AND b~vrgng  = 'RKL'
**      AND c~mandt  = sy-mandt
**      AND c~objnr  =  it_fsc_mat-objnr
**      AND c~gjahr  =  p_bdatj
**      AND c~perio  =  p_perab
**      AND c~wrttp  =  c_gp_wrttp
**      AND c~versn  =  p_versn.

  DATA : l_rueck LIKE lt_afru-rueck,
         l_rmzhl LIKE lt_afru-rmzhl.

*  SORT lt_afru BY rueck rmzhl.
  LOOP AT it_covp.
    l_rueck = it_covp-refbn.
    l_rmzhl = it_covp-aworg.
* Replaced by Han Moon
*    SELECT SINGLE lmnga INTO w_lmnga
*      FROM ( afru INNER JOIN afpo
*        ON afru~aufnr = afpo~aufnr )
*      WHERE afru~rueck = l_rueck
*        AND afru~rmzhl = l_rmzhl.
    SELECT SINGLE lmnga INTO w_lmnga
      FROM afru
     WHERE rueck = l_rueck
       AND rmzhl = l_rmzhl
       AND lmnga = 0.   "Added by Han Moon
* confirmed w/o qty
    IF sy-subrc = 0 AND w_lmnga = 0.
      CLEAR: %g00.
      %g00-objnr   = it_covp-objnr.
      %g00-f_objnr = it_covp-parob.
      %g00-meinh   = it_covp-meinb.
      %g00-varmn   = it_covp-mbgbtr.
      %g00-varamt  = it_covp-wkgbtr.
      COLLECT %g00.
    ENDIF.

  ENDLOOP.

* MTO,MTS : COVP, AFRU
*  SORT IT_FSC_MAT BY OBJNR.
*
*  LOOP AT IT_MTS_MH.
*    CLEAR: %G00.
*    READ TABLE IT_FSC_MAT WITH KEY OBJNR = IT_MTS_MH-OBJNR
*                                   BINARY SEARCH.
*    CHECK SY-SUBRC = 0.
** Check Order completion confirmations
**    SELECT SINGLE lmnga INTO w_lmnga
**      FROM ( afru INNER JOIN afpo
**        ON afru~aufnr = afpo~aufnr )
**      WHERE afru~rueck = it_mts_mh-refbn
**        AND afru~rmzhl = it_mts_mh-aworg.
**
*** confirmed w/o qty
**    IF sy-subrc = 0 AND w_lmnga = 0.
*    %G00-OBJNR   = IT_MTS_MH-OBJNR.
*    %G00-F_OBJNR = IT_MTS_MH-PAROB.
*    %G00-MEINH   = IT_MTS_MH-MEINB.
*    %G00-VARMN   = IT_MTS_MH-MBGBTR.
*    %G00-VARAMT  = IT_MTS_MH-WKGBTR.
***      %g00-matnr   = g_it_coep-matnr.  "blank fixme
***      %g00-aufnr   = afru-aufnr.
*    COLLECT %G00.
**     ENDIF.
*
*  ENDLOOP.
*//


  SORT it_csla BY lstar.

  LOOP AT %g00.

    MOVE-CORRESPONDING  %g00 TO it_mhsum.

    CALL FUNCTION 'OBJECT_KEY_GET_KL'
      EXPORTING
        objnr  = %g00-f_objnr
      IMPORTING
        kostl  = it_mhsum-kostl
        lstar  = it_mhsum-lstar
      EXCEPTIONS
        OTHERS = 1.

* Change unit :  'STD' => Activity master unit
    CLEAR it_csla.
    READ TABLE it_csla WITH KEY lstar = it_mhsum-lstar
                                BINARY SEARCH.

    IF sy-subrc = 0 AND it_csla-leinh <> it_mhsum-meinh.
      PERFORM unit_converion
        USING    it_mhsum-varmn it_mhsum-meinh it_csla-leinh
        CHANGING it_mhsum-varmn.
      it_mhsum-meinh = it_csla-leinh.
    ENDIF.

*    perform unit_convert_to_std using    it_mhsum-varmn
*                                changing it_mhsum-meinh
*                                         it_mhsum-varmn.

    COLLECT it_mhsum.

  ENDLOOP.

  SORT  it_mhsum    BY  objnr kostl lstar.

ENDFORM.                    " READ_MH_DATA
*&---------------------------------------------------------------------*
*&      Form  CAL_ADD_QTY_AMT_FROM_MH
*&---------------------------------------------------------------------*
*       CAL. Additional Qty / AMT from M/H Table
*----------------------------------------------------------------------*
*FORM cal_add_qty_amt_from_mh.
*
*  CLEAR : it_mhsum.
*  CLEAR : it_sc_e.
*
*  SORT  it_mhsum    BY  objnr kostl lstar.
*
** Set Add. Qty/AMT
*  LOOP AT it_sc_e.
*    CLEAR it_mhsum.
*    READ TABLE it_mhsum
*            WITH KEY objnr = it_sc_e-objnr
*                     kostl = it_sc_e-kostl
*                     lstar = it_sc_e-lstar.
*    IF sy-subrc = 0.
*      MOVE : it_mhsum-varmn TO it_sc_e-add_mbgbtr.
** APPEND
*      MODIFY it_sc_e.
*    ENDIF.
*    CLEAR it_sc_e.
*  ENDLOOP.
*
*ENDFORM.                    " CAL_ADD_QTY_AMT_FROM_MH
*&---------------------------------------------------------------------*
*&      Form  READ_UNIT_PRICE_FR_KSBT
*&---------------------------------------------------------------------*
*       Read Unit Price of  AT / From KSBT
*----------------------------------------------------------------------*
*  -->  P_WRTTP        Value Type
*  <--  P_TARKZ        Price Ind.
*----------------------------------------------------------------------*
FORM read_unit_price_fr_ksbt
                       USING p_wrttp
                             p_tarkz.

* Local Data Definition
  DATA : it_l_cost LIKE STANDARD TABLE OF cost
                   WITH HEADER LINE .
  DATA : lv_cnt    LIKE p_perab.
  FIELD-SYMBOLS : <fs_tkg> TYPE any, <fs_tke> TYPE any.
  DATA : lv_tkg(30) VALUE 'IT_L_COST-TKGxxx',
         lv_tke(30) VALUE 'IT_L_COST-TKExxx'.

* Select
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_l_cost
           FROM cost
          WHERE lednr  = '00'
            AND objnr  LIKE 'KL%'
            AND gjahr  = p_bdatj
            AND wrttp  = p_wrttp                  "Actual 04 / Plan 01
            AND versn  = p_versn
            AND tarkz  = p_tarkz.                 "Price Ind. : 5,1

* Get data by Period (From P_PERAB to w_perbi)
* Clear
  CLEAR : it_ccr1t, it_ccr1t[].

  LOOP AT it_l_cost.
    CLEAR lv_cnt.
    lv_cnt  = p_perab.
    WHILE lv_cnt <= w_perbi.
* Trans. Data
      MOVE-CORRESPONDING it_l_cost TO it_ccr1t.
      MOVE lv_cnt TO: lv_tkg+13, lv_tke+13.
      ASSIGN: (lv_tkg) TO <fs_tkg>,
              (lv_tke) TO <fs_tke>.
      it_ccr1t-tkgxxx = <fs_tkg> / <fs_tke>.
* Set Period.
*     it_ccr1t-periode = lv_cnt.
* Kostl/Lstar
      CALL FUNCTION 'OBJECT_KEY_GET_KL'
        EXPORTING
          objnr       = it_ccr1t-objnr
        IMPORTING
          kokrs       = p_kokrs
          kostl       = it_ccr1t-kostl
          lstar       = it_ccr1t-lstar
        EXCEPTIONS
          not_found   = 1
          wrong_obart = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
      ENDIF.
* Append
      APPEND it_ccr1t.
      CLEAR  it_ccr1t.
* Period Counter.
      lv_cnt = lv_cnt + 1.
    ENDWHILE .
    CLEAR  it_l_cost.
  ENDLOOP.

  CLEAR  it_ccr1t.

ENDFORM.                    " READ_UNIT_PRICE_FR_KSBT
*&---------------------------------------------------------------------*
*&      Form  CAL_COSTCOMP_ML_ACT
*&---------------------------------------------------------------------*
*       CAL. CostComp / ML Actual Unit Price
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM cal_costcomp_ml_act.
*
** Read Unit Price of  AT / From KSBT
*  PERFORM read_unit_price_fr_ksbt
*                            USING '04'
*                                  '005'.
** Read Unit Price By CostComp
**  cal_koat 5.
*  DATA : lv_datum LIKE sy-datum.
*  CLEAR it_koat_p_5.
*  SORT it_ccr1t BY gjahr periode objnr.
*
*  SORT it_csla BY
*            lstar
*            leinh
*            datab.
*  CLEAR it_csla.
*
*  LOOP AT it_ccr1t.
*    LOOP AT it_koat_p_5
*                  WHERE gjahr = it_ccr1t-gjahr
*                    AND poper = it_ccr1t-periode
*                    AND kostl = it_ccr1t-kostl
*                    AND lstar = it_ccr1t-lstar.
** Get the Last Day of Period
*      CLEAR lv_datum.
*      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
*        EXPORTING
*          i_gjahr              = it_koat_p_5-gjahr
**         I_MONMIT             = 00
*          i_periv              = tka01-lmona
*          i_poper              = it_koat_p_5-poper
*        IMPORTING
*          e_date               = lv_datum
*        EXCEPTIONS
*          input_false          = 1
*          t009_notfound        = 2
*          t009b_notfound       = 3
*          OTHERS               = 4.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      CLEAR it_csla.
*      LOOP AT it_csla WHERE  lstar = it_koat_p_5-lstar
*                        AND  datab =< lv_datum.
*        IF sy-subrc = 0.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*
** Cal.
*      it_koat_p_5-tkgxxx
*        = (
*          it_ccr1t-tkgxxx * ( it_csla-denominator / it_csla-numerator )
*          )
*        * it_koat_p_5-cp_%.
** Modify
*      MODIFY it_koat_p_5.
*      CLEAR  it_koat_p_5.
*    ENDLOOP.
*    CLEAR  it_ccr1t.
*  ENDLOOP.
*
*
** Set ML Actual Unit Price
*  PERFORM set_act_un_price_e.
*
*ENDFORM.                    " CAL_COSTCOMP_ML_ACT

*&---------------------------------------------------------------------*
*&      Form  KS_N_KSTAR_C_E
*&---------------------------------------------------------------------*
*       KSTAR = '836001', '836002'
*       ( 'E' ).
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM ks_n_kstar_c_e.
*
** Clear Container
*  PERFORM clear_container_ic.
*
*  CLEAR IT_SHOP_SUM.
*  CLEAR it_koat_p_1.
*  CLEAR it_koat_p_5.
*
** Read SHOP Info. and Convert the UOM -> HR
*  PERFORM read_shop_uom_for_e.
*
** Read M/H table - ZTCO_MHPCPOST
** Vehicle-> CPZP table...
** Other -> AFRU,COVP
*  PERFORM read_mh_data.
*
** CAL. Additional Qty / AMT from M/H Table
*  PERFORM cal_add_qty_amt_from_mh.
*
** CAL. CostComp / ML Actual Unit Price
*  PERFORM cal_costcomp_ml_act.
*
** Cal. Aditional Amt/PCC Current Amt/WIP/SCRAP
*  PERFORM cal_add_cur_amt.
*
** Cal. ML_ACT_PREIS.  IT_SC_E.
*  PERFORM cal_ml_act_preis_e.
*
*  IF NOT it_sc_e[] IS INITIAL.
** Append
*    APPEND LINES OF it_sc_e TO IT_SHOP_SUM.
*    CLEAR IT_SHOP_SUM.
*  ENDIF.
*
*ENDFORM.                    " KS_N_KSTAR_C_E
*&---------------------------------------------------------------------*
*&      Form  unit_convert_to_std
*&---------------------------------------------------------------------*
FORM unit_convert_to_std USING    f_in_qty
                         CHANGING f_in_unit
                                  f_out_qty.

* Unit Conversion - MEEHT
* MBGBTR - current qty,
  IF f_in_unit = 'STD'.
    f_out_qty = f_in_qty.
  ELSE.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input    = f_in_qty
        unit_in  = f_in_unit
        unit_out = 'STD'  "Hour
      IMPORTING
        output   = f_out_qty.

    IF sy-subrc <> 0.
      BREAK-POINT.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    f_in_unit = 'STD'.

  ENDIF.

ENDFORM.                    " unit_convert_to_std
*&---------------------------------------------------------------------*
*&      Form  READ_COSL
*&---------------------------------------------------------------------*
*FORM read_cosl.
** Local Data Definition
*  DATA : it_lt_cosla LIKE STANDARD TABLE OF cosla
*                     WITH HEADER LINE .
*
** Read Qty Activity Type Totals
*  SORT it_fsc_mat BY objnr.
*  CLEAR : it_lt_cosla, it_lt_cosla[].
*  SELECT * FROM cosl
*           INTO CORRESPONDING FIELDS OF TABLE it_lt_cosla
*           FOR ALL ENTRIES IN it_fsc_mat
*           WHERE lednr = '00'
*             AND objnr = it_fsc_mat-objnr
*             AND gjahr = p_bdatj
*             AND wrttp = c_gp_wrttp
*             AND versn = p_versn.
*
*  SORT  it_lt_cosla BY objnr.
*  CLEAR it_lt_cosla.
*
** transfer data to Global Itabs
** Only for Miscellaneous/ Goods Issue / Confirmation
*  DATA : lv_kkb_beweg TYPE kkb_beweg.
** COSL
*  CLEAR : it_cosla, it_cosla[].
*  LOOP AT  it_lt_cosla.
*    MOVE-CORRESPONDING it_lt_cosla TO it_cosla.
*    COLLECT it_cosla.
*    CLEAR it_cosla.
*    CLEAR it_lt_cosla.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_COSS_COSP
*&---------------------------------------------------------------------*
FORM read_cosp_coss.
* Local Data Definition
  DATA : BEGIN OF it_lt_cospa OCCURS 100, "LIKE STANDARD TABLE OF cospa
           objnr LIKE covp-objnr,
           kstar LIKE covp-kstar,
           vrgng LIKE covp-vrgng,
           beweg LIKE kkbcs_out-beweg,

           pargb LIKE cosp-pargb,
           beknz LIKE cosp-beknz,
*//modify..t00020..
*           wkg001  TYPE wkgxxx,
*           wkg002  TYPE wkgxxx,
*           wkg003  TYPE wkgxxx,
*           wkg004  TYPE wkgxxx,
*           wkg005  TYPE wkgxxx,
*           wkg006  TYPE wkgxxx,
*           wkg007  TYPE wkgxxx,
*           wkg008  TYPE wkgxxx,
*           wkg009  TYPE wkgxxx,
*           wkg010  TYPE wkgxxx,
*           wkg011  TYPE wkgxxx,
*           wkg012  TYPE wkgxxx,
*//
          END OF it_lt_cospa.

  DATA : it_lt_cossa LIKE it_lt_cospa OCCURS 100 WITH HEADER LINE.

*  "STANDARD TABLE OF cossa
  DATA: lv_cospa LIKE cospa,
        lv_cossa LIKE cossa.

* Read COSP ( Only Debit data / BEKNZ = 'S' ) Fct Out
  CLEAR : it_lt_cospa, it_lt_cospa[].
  CLEAR : it_lt_cossa, it_lt_cossa[].

  CHECK it_fsc_mat[] IS NOT INITIAL.

*  LOOP AT it_fsc_mat.

  SELECT objnr kstar vrgng pargb beknz
*//modify..t00020..
*           SUM( wkg001 ) SUM( wkg002 ) SUM( wkg003 ) SUM( wkg004 )
*           SUM( wkg005 ) SUM( wkg006 ) SUM( wkg007 ) SUM( wkg008 )
*           SUM( wkg009 ) SUM( wkg010 ) SUM( wkg011 ) SUM( wkg012 )
        APPENDING CORRESPONDING FIELDS OF TABLE it_lt_cospa
            FROM cosp CLIENT SPECIFIED BYPASSING BUFFER
          FOR ALL ENTRIES IN it_fsc_mat
           WHERE objnr = it_fsc_mat-objnr
             AND mandt = sy-mandt
             AND lednr = '00'
             AND versn = p_versn
             AND wrttp = c_gp_wrttp
             AND gjahr = p_bdatj
             %_HINTS ORACLE 'index("cosp" "cosp~1")'.
*           GROUP BY objnr kstar vrgng pargb beknz.

* Read COSS ( Only Debit data / BEKNZ = 'S' )
  SELECT   objnr kstar vrgng parob beknz
*//modify..t00020..
*             SUM( wkg001 ) SUM( wkg002 ) SUM( wkg003 ) SUM( wkg004 )
*             SUM( wkg005 ) SUM( wkg006 ) SUM( wkg007 ) SUM( wkg008 )
*             SUM( wkg009 ) SUM( wkg010 ) SUM( wkg011 ) SUM( wkg012 )
         APPENDING CORRESPONDING FIELDS OF TABLE it_lt_cossa
            FROM coss CLIENT SPECIFIED BYPASSING BUFFER
          FOR ALL ENTRIES IN it_fsc_mat
            WHERE objnr = it_fsc_mat-objnr
              AND mandt = sy-mandt
              AND lednr = '00'
              AND versn = p_versn
              AND wrttp = c_gp_wrttp
              AND gjahr = p_bdatj
             %_HINTS ORACLE 'index("coss" "coss~1")'.
*             GROUP BY objnr kstar vrgng parob beknz.
*  ENDLOOP.

*C  Goods issues
*F  Confirmations
*I  Overhead
*L  Miscellaneous
*P  Goods receipt
*X  Settlement
*O  Distribution

*//Modify..03/29/2011..T00020..
  DELETE ADJACENT DUPLICATES FROM it_lt_cospa.
*//

  LOOP AT  it_lt_cospa.

    MOVE-CORRESPONDING it_lt_cospa TO lv_cospa.
    MOVE-CORRESPONDING it_lt_cospa TO it_categ.

    CALL FUNCTION 'Z_K_KKB_BEWEG_SET'
      EXPORTING
        i_cospa  = lv_cospa
      IMPORTING
        e_beweg  = it_categ-beweg    "Business Transaction on Manufacturing Orders
      EXCEPTIONS
        no_input = 1
        OTHERS   = 2.

*    COLLECT it_categ.
*    CLEAR   it_categ.

  ENDLOOP.


*//Modify..03/29/2011..T00020..
  DELETE ADJACENT DUPLICATES FROM it_lt_cossa.
*//

  LOOP AT  it_lt_cossa.

    MOVE-CORRESPONDING it_lt_cossa TO lv_cossa.
    MOVE-CORRESPONDING it_lt_cossa TO it_categ.

    CALL FUNCTION 'Z_K_KKB_BEWEG_SET'
      EXPORTING
        i_cossa  = lv_cossa
      IMPORTING
        e_beweg  = it_categ-beweg    "Business Transaction on Manufacturing Orders
      EXCEPTIONS
        no_input = 1
        OTHERS   = 2.

*    COLLECT it_categ.
*    CLEAR   it_categ.

  ENDLOOP.


*  SORT  it_categ BY objnr kstar vrgng.
*  CLEAR it_categ.


ENDFORM.                    "read_cosp_coss
*&---------------------------------------------------------------------*
*&      Form  get_unit_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATNR  text
*      -->P_P_BWKEY  text
*      -->P_P_BWTAR  text
*      -->P_L_UNITP  text
*      -->P_L_PEINH  text
*----------------------------------------------------------------------*
*FORM get_unit_price USING    f_matnr
*                             f_bwkey
*                             f_bwtar
*                    CHANGING f_unitp
*                             f_peinh.
*
**      select single * from mbew
**             where matnr = p_llv_matnr
**               and bwkey = p_bwkey
**               and bwtar = p_bwtar.
** if i_mbew-lfgja = p_bdatj and i_mbew-lfmon = p_poper.
*
*  READ TABLE i_mbew WITH KEY matnr = f_matnr
*                             bwkey = f_bwkey
*                             bwtar = f_bwtar.
*
*  IF sy-subrc = 0.
*    f_unitp = i_mbew-stprs.
*    f_peinh = i_mbew-peinh.
*  ELSE.
*    SELECT * FROM mbewh
*         WHERE matnr = f_matnr
*           AND bwkey = f_bwkey
*           AND bwtar = f_bwtar
*           AND lfgja =  p_bdatj
*           AND lfmon <= p_perab
*         ORDER BY lfmon  DESCENDING.
*      f_unitp = mbewh-stprs / mbewh-peinh.
*      f_peinh = mbewh-peinh.
*      EXIT.
*    ENDSELECT.
*  ENDIF.
*
*ENDFORM.                    " get_unit_price


*---------------------------------------------------------------------*
*       FORM set_status                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.

* RT_EXTAB CONTAIN THE FUNCTION CODES WHCH ARE HIDDEN
* IN THE STANDARD INTERFACE
*  SET PF-STATUS 'MYSTATUS' EXCLUDING rt_extab.
  SET PF-STATUS 'MYSTATUS' .
ENDFORM.                    "set_status


*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
     rs_selfield TYPE slis_selfield.
*  READ TABLE it_log INDEX selfield-tabindex.
  CASE ucomm.
    WHEN '&ITEM'.
      PERFORM show_cost_itemiziation.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  show_cost_itemiziation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_cost_itemiziation.

  RANGES:
         t_bzobj FOR keko-bzobj,
         t_kalnr FOR keko-kalnr,
         t_kalka FOR keko-kalka,
         t_kadky FOR keko-kadky,
         t_tvers FOR keko-tvers,
         t_bwvar FOR keko-bwvar,
         t_kkzma FOR keko-kkzma,
         t_patnr FOR keph-patnr,
         t_keart FOR keph-keart,
         t_losfx FOR keph-losfx,
         t_kkzst FOR keph-kkzst,
         t_kkzmm FOR keph-kkzmm,
         t_dipa  FOR keph-dipa,
         t_bidat FOR keko-bidat,
         t_kadat FOR keko-kadat,
         t_matnr FOR keko-matnr,
         t_werks FOR keko-werks,
       t_kalaid  FOR kala-kalaid,                     "INS46C note430408
       t_kaladat FOR kala-kaladat,                    "INS46C note430408
         t_klvar FOR keko-klvar,
         t_mkalk FOR keko-mkalk,
         t_baltkz FOR keko-baltkz,
         t_feh_sta FOR keko-feh_sta,
         t_cuobjid FOR keko-cuobjid,
         t_bwtar FOR keko-bwtar.
* Item structure
  TYPES: BEGIN OF kkb04_items,
          posnr LIKE kis1-posnr,                   "Kalkulationsposition
           typps LIKE kis1-typps,                   "Positionstyp
           kstar LIKE kis1-kstar,                   "Kostenart
           resou LIKE rk70eq-ressource,             "Ressource
           elemt LIKE tckh1-elemt,                  "Kostenelement
           pwerk LIKE kis1-werks,                   "Einsatzwerk
           pmatn LIKE kis1-matnr,                   "Material
           pmtar LIKE mara-mtart,                   "Materialart
           kostl LIKE kis1-kostl,                   "Kostenstelle
           lstar LIKE kis1-lstar,                   "Leistungsart
           arbpl LIKE kkbu-parpl,                   "Arbeitsplatz
           lifnr LIKE kis1-lifnr,                   "Lieferant
           pmawr LIKE kkbu-pmawr,                   "Material/Werk
           pksla LIKE kkbu-pksla,                   "Kostenstelle/LArt
           prznr LIKE kis1-prznr,                   "Prozeß
           vornr LIKE kis1-vornr,                   "Arbeitsvorgang
           infnr LIKE kis1-infnr,                   "Infosatz
          ekorg LIKE kis1-ekorg,                   "Einkaufsorganisation
           pextn LIKE kis1-extnr,                   "Bauteil
           stpos LIKE kis1-sposn,                   "St?cklistenposition
           fehlk(4),                                "Kennzeichen Fehler
         baugr LIKE kis1-baugr,                   "Kennzeichen Baugruppe
           strat LIKE kis1-strat,                   "Bew.Strategie
           ltext LIKE kis1-ltext,                   "Langtext
           " Kennzahlen
           menge LIKE ckis-menge,                   "Basismenge
           ameng LIKE ckis-menge,                   "Ausgabemenge
           " Werte ( 'O' Buchungskreisw?hrung
           "       ( 'K' Kostenrechnungskreisw?hrung
           "       ( 'T' Transaktionsw?hrung )
           valog LIKE ckis-wertn,                   "Wert gesamt
           valkg LIKE ckis-wertn,
           valtg LIKE ckis-wertn,
           valof LIKE ckis-wertn,                   "Fixwert
           valkf LIKE ckis-wertn,
           valtf LIKE ckis-wertn,
           valov LIKE ckis-wertn,                   "Variabler Wert
           valkv LIKE ckis-wertn,
           valtv LIKE ckis-wertn,
           preog LIKE ckis-gpreis,                  "Preis
           prekg LIKE ckis-gpreis,
           pretg LIKE ckis-gpreis,
           preof LIKE ckis-gpreis,                  "Preis fix
           prekf LIKE ckis-gpreis,
           pretf LIKE ckis-gpreis,
           preov LIKE ckis-gpreis,                  "Preis variabel
           prekv LIKE ckis-gpreis,
           pretv LIKE ckis-gpreis,
           peino LIKE ckis-peinh,                   "Preiseinheit
           peink LIKE ckis-peinh,
           peint LIKE ckis-peinh,
           ausmg LIKE ckis-ausmg,                   "Ausschußmenge
           ausmgko LIKE ckis-ausmgko,               "Komponentenausschu
           scrap LIKE kkbu-scrap,                   "Ausschußwert
           scrap_o LIKE kkbu-scrap_o,
           scrap_t LIKE kkbu-scrap,
         scrak LIKE kkbu-scrak,                   "Komponenentenausschu
           scrak_o  LIKE kkbu-scrak_o,
           scrak_t  LIKE kkbu-scrak,
           "Technische Merkmale
           twaer LIKE kkbu-kwaer,                   "Transaktionsw?hrung
         kwaer LIKE kkbu-kwaer,                   "Kostenrechnungskreisw
          owaer LIKE kkbu-kwaer,                   "Buchungskreisw?hrung
           meeht LIKE ckis-meeht,                   "Basismengeneinheit
          ausme LIKE marc-ausme,                   "Ausgabemengeneinheit
           pmeht LIKE ckis-pmeht,                   "Preismengeneinheit
         END OF kkb04_items.

  DATA : it_t_items TYPE kkb04_items OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_keko OCCURS 0.            "Interna1 KEKO
          INCLUDE STRUCTURE keko.
  DATA: END OF t_keko.

  DATA: BEGIN OF t_keph OCCURS 0.            "Internal KEPH
          INCLUDE STRUCTURE keph.
  DATA: END OF t_keph.

  DATA : lines LIKE sy-tfill.
* Read KEKO with a Date Range
  DATA:  BEGIN OF kkb0.
          INCLUDE STRUCTURE kkb0.
  DATA:  END OF kkb0.

  DATA : BEGIN OF kkb_aufloesung.
          INCLUDE STRUCTURE kkb_aufloesung.
  DATA:  END OF kkb_aufloesung.

  DATA BEGIN OF fsp_cki64a.
          INCLUDE STRUCTURE cki64a.
  DATA END OF fsp_cki64a.
  DATA : last_day LIKE sy-datum,
          erzka_given,
          erzka LIKE keko-erzka.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_bdatj
*     I_MONMIT       = 00
      i_periv        = 'K4'
      i_poper        = p_perab
    IMPORTING
      e_date         = last_day
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  MOVE:          s_matnr-low   TO   kkb0-matnr,   " Matnr
                  'KVA1'       TO   kkb0-werks,   " WERKS
                  'K201'       TO   kkb0-kokrs,
                     'PPC1'    TO   kkb0-klvar,   " Costing variant
                 last_day      TO   kkb0-kadky,   " Costing Date
                     '01'      TO   kkb0-tvers,   " Costing Version
                     '01'      TO   kkb0-sicht,   " Cost Component view
                       ''      TO   kkb0-aufl.    " Explode Group Yes/No
  t_bzobj-sign   = 'I'.
  t_bzobj-option = 'EQ'.
  t_bzobj-low    = '0'.
  t_bzobj-high   = '0'.
  APPEND t_bzobj.

  t_matnr-sign   = 'I'.                " Material Objekt1
  t_matnr-option = 'EQ'.
  t_matnr-low    = kkb0-matnr.
  t_matnr-high   = kkb0-matnr.
  APPEND t_matnr.

  t_werks-sign   = 'I'.                " Material Objekt1
  t_werks-option = 'EQ'.
  t_werks-low    = kkb0-werks.
  t_werks-high   = kkb0-werks.
  APPEND t_werks.


  IF NOT kkb0-kalnr_ba IS INITIAL.
    t_kalnr-sign   = 'I'.
    t_kalnr-option = 'EQ'.
    t_kalnr-low    = kkb0-kalnr_ba.
    t_kalnr-high   = kkb0-kalnr_ba.
    APPEND t_kalnr.
  ELSE.
    t_baltkz-sign = 'I'.
    t_baltkz-option = 'EQ'.
    t_baltkz-low = 'X'.
    t_baltkz-high = 'X'.
    APPEND t_baltkz.
  ENDIF.

  CALL FUNCTION 'CK_F_KEKO_KEPH_READING'        "Lesen KEKO
         EXPORTING
              read_keko = 'X'
              read_keph = space
         TABLES
              t_bzobj = t_bzobj
              t_bwvar = t_bwvar
              t_kadky = t_kadky
              t_kalka = t_kalka
              t_kalnr = t_kalnr
              t_keko  = t_keko
              t_keph  = t_keph
              t_kkzma = t_kkzma
              t_tvers = t_tvers
              t_keart = t_keart
              t_kkzst = t_kkzst
              t_kkzmm = t_kkzmm
              t_matnr = t_matnr
              t_werks = t_werks
              t_klvar = t_klvar
              t_bidat = t_bidat
              t_kadat = t_kadat
              t_losfx = t_losfx
              t_patnr = t_patnr
         EXCEPTIONS
              no_data_found  = 1
              no_data_wanted = 2.


  LOOP AT t_keko.
    MOVE sy-tabix TO lines.
    CHECK t_keko-klvar <> kkb0-klvar
    OR    t_keko-kkzma <> kkb0-aufl
    OR    t_keko-tvers <> kkb0-tvers.
    DELETE t_keko INDEX lines.
  ENDLOOP.

  SORT t_keko BY kadky.

  CALL FUNCTION 'K_KKB_ITEMIZATION'
     EXPORTING
          i_kdauf       = kkb0-kdauf
          i_kdpos       = kkb0-kdpos
          i_kadky       = kkb0-kadky
          i_kalnr       = t_keko-kalnr
          i_kalnr_ba    = kkb0-kalnr_ba
          i_klvar       = kkb0-klvar
          i_kokrs       = kkb0-kokrs
          i_matnr       = kkb0-matnr
          i_werks       = kkb0-werks
          i_tvers       = kkb0-tvers
          i_sicht       = '01'
*         I_VARIA       = G_VARIA-VARIANT
          i_explosion   = kkb_aufloesung-disso
          i_auflo       = kkb_aufloesung-auflo
          i_maton       = kkb_aufloesung-maton
          i_bz_losgr    = fsp_cki64a-kosmng
          i_keko        = t_keko
          i_erzka_given = erzka_given
          i_erzka       = erzka
*          I_NO_OUTPUT   = 'X'
*       IMPORTING
*           E_HEAD       = e_header
      TABLES
**          T_XHEADER     =  t_e_HEAD
            t_out_items   =  it_t_items
      EXCEPTIONS
           OTHERS        = 1.


ENDFORM.                    " show_cost_itemiziation
*&---------------------------------------------------------------------*
*&      Form  get_cost_itemization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cost_itemization.
* Itemiziation
  DATA :
  bapi_colist        LIKE bapicolist OCCURS 0 WITH HEADER LINE,
  itemization_header LIKE bapiitemizationheader,
  returnitemization  LIKE bapireturn,
  itemization_list   LIKE bapiitemizationlist OCCURS 0 WITH HEADER LINE,
  wa_item            LIKE bapiitemizationlist,
  wa_itemization     LIKE bapiitemizationlist OCCURS 0 WITH HEADER LINE,
  returnlist         LIKE bapireturn,
  rxm0_material      LIKE bapimateri OCCURS 0,
  l_grqty            LIKE coep-mbgbtr,
  l_idx              LIKE sy-tabix,
  w_keko             LIKE keko.

  bapi_cstgva-sign   = 'I'.
  bapi_cstgva-option = 'EQ'.
  bapi_cstgva-low    = 'PPC1'.
  APPEND bapi_cstgva.

  bapi_cstgstat-sign   = 'I'.
  bapi_cstgstat-option = 'EQ'.
  bapi_cstgstat-low    = 'FR'.  "Released
  APPEND bapi_cstgstat.

  TYPE-POOLS: bzobj.


  LOOP AT it_fsc_gr.

    l_idx = sy-tabix.

*    READ TABLE it_coep WITH KEY objnr = it_fsc_mat-objnr
*                                typps = 'O'.
**                                beknz = 'O'.
*    IF sy-subrc EQ 0.
*      CLEAR: lt_ck13n, l_grqty.
*      lt_ck13n-objnr     =  it_coep-objnr.
*      lt_ck13n-material  =  it_coep-matnr.
*      lt_ck13n-kstar     =  it_coep-kstar.
*      lt_ck13n-meeht     =  it_coep-meinb.
*      lt_ck13n-gr_amt    =  it_coep-wkgbtr.
*      lt_ck13n-gr_qty    =  it_coep-mbgbtr.
*      APPEND lt_ck13n.
*      l_grqty = - it_coep-mbgbtr.  "Positive sign

    REFRESH: bapi_matnr, bapi_plant, rxm0_material, bapi_valid.

    bapi_matnr-sign = 'I'.  bapi_matnr-option = 'EQ'.
    bapi_matnr-low  = it_fsc_gr-matnr.
    APPEND bapi_matnr.

    rxm0_material[] = bapi_matnr[].

    bapi_plant-sign = 'I'.  bapi_plant-option = 'EQ'.
    bapi_plant-low  = it_fsc_gr-werks.
    APPEND bapi_plant.

* Get List of Valid Cost Estimate
    REFRESH: bapi_colist.

    CALL FUNCTION 'BAPI_COSTESTIMATE_GETLIST'
      EXPORTING
        cost_estimate      = cstest                         "default 01
      IMPORTING
        return             = returnlist
      TABLES
        costing_variant    = bapi_cstgva
        material           = rxm0_material
        plant              = bapi_plant
        validity_from      = bapi_valid
        costing_status     = bapi_cstgstat
        costing_version    = bapi_cstgvrsn
        cost_estimate_list = bapi_colist
      EXCEPTIONS
        OTHERS             = 1.

    IF sy-subrc <> 0.
      IF p_debug = 'X'.
        BREAK-POINT.
      ENDIF.
      CONTINUE.
    ENDIF.

**select released cost est.
*    SELECT * INTO w_keko FROM keko
*       WHERE bzobj = bzobj_material
*         AND matnr = it_fsc_gr-matnr
*         AND kadat <= g_todat
*         AND bidat >= g_todat
*         AND klvar   IN bapi_cstgva      "PPC1
*         AND FEH_STA IN bapi_cstgstat    "FR
*         AND KKZMA   <> 'X'              "No additive
*       ORDER BY KADAT descending.
*       EXIT.
*    ENDSELECT.
*
*    bapi_valid-sign   = 'I'.
*    bapi_valid-option = 'EQ'.
*    bapi_valid-low    = w_keko-kadat.
*    bapi_valid-high   = w_keko-bidat.
*    APPEND bapi_valid.


*Select proper record using valid fr/to
    SORT bapi_colist  BY cstg_num ce_status
                               valid_from DESCENDING.

    LOOP AT bapi_colist WHERE valid_from   <= g_todat
                          AND valid_to     >= g_todat
*//Modify..03/25/2011..T00020..
                          AND ce_status(1) = 'F'.     "released.

*      IF bapi_colist-ce_status(1) = 'F'. "released
      EXIT.
*      ENDIF.
*//
    ENDLOOP.

    IF sy-subrc <> 0.
      IF p_debug = 'X'.
        BREAK-POINT.
      ENDIF.
      CONTINUE.
    ENDIF.

* Get Itemization Details
    REFRESH: itemization_list.
    CALL FUNCTION 'BAPI_COSTESTIMATE_ITEMIZATION'
      EXPORTING
        referenceobject    = bapi_colist-ref_object
        costingnumber      = bapi_colist-cstg_num
        costingtype        = bapi_colist-cstg_type
        costingdate        = bapi_colist-cstg_date
        costingversion     = bapi_colist-version
        valuationvariant   = bapi_colist-vltn_vrnt
        enteredmanually    = bapi_colist-enter_man
      IMPORTING
        itemization_header = itemization_header
        return             = returnitemization
      TABLES
        itemization_list   = itemization_list.

*Lot size
    it_fsc_gr-lotsize = itemization_header-lotsize.
    MODIFY it_fsc_gr INDEX l_idx.

*    REFRESH wa_itemization.
*    LOOP AT itemization_list INTO wa_item.
*      wa_item-operation = ''.
*      wa_item-sub_operation = ''.
*      wa_item-bom_item_number = ''.
*      CLEAR: wa_item-origin_group.
*      COLLECT wa_item INTO wa_itemization.
*    ENDLOOP.
*    REFRESH itemization_list. CLEAR itemization_list.
*    APPEND LINES OF wa_itemization TO itemization_list.


*.... collect to ITEMIZATION SUMMARY
    LOOP AT  itemization_list.

      CLEAR lt_ck13n.

      lt_ck13n-werks  = it_fsc_gr-werks.
      lt_ck13n-matnr  = it_fsc_gr-matnr.
      lt_ck13n-kstar  = itemization_list-cost_element.

      lt_ck13n-meeht  = itemization_list-base_unit_of_measure.
      lt_ck13n-gr_qty = itemization_list-quantity.

      IF  NOT itemization_list-material IS INITIAL.
*UoM Problem - FIXME
*     if lt_ck13n-material  = 'GO60520041203A001'. break-point. endif.

        lt_ck13n-material  = itemization_list-material.
        lt_ck13n-plant     = itemization_list-plant.

        PERFORM make_resou USING 'M' lt_ck13n-plant  lt_ck13n-material
                           CHANGING lt_ck13n-resou.

        PERFORM check_it_mat USING lt_ck13n-material lt_ck13n-plant
                             CHANGING sy-subrc.
*        IF sy-subrc <> 0.
        PERFORM add_it_mat_tmp USING lt_ck13n-material lt_ck13n-plant
                                     CHANGING sy-subrc.
*        ENDIF.

      ELSE.
* GR Qty for Activity
        lt_ck13n-kostl    = itemization_list-cost_center.
        lt_ck13n-lstar    = itemization_list-activity_type.

        PERFORM make_resou USING 'E' lt_ck13n-kostl  lt_ck13n-lstar
                           CHANGING lt_ck13n-resou .

*        PERFORM unit_convert_to_std USING    itemization_list-quantity
*                                    CHANGING lt_ck13n-meeht
*                                             lt_ck13n-gr_qty.
      ENDIF.

      lt_ck13n-peinh  =
                    itemization_list-co_area_currency_price_unit.
      lt_ck13n-gr_amt = itemization_list-co_area_currency_total_value.

      COLLECT lt_ck13n.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " get_cost_itemization
*&---------------------------------------------------------------------*
*&      Form  get_multilevel_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_multilevel_price.

  DATA: h_kalnr     LIKE ckmlhd-kalnr,
        lt_docs     TYPE ckmd_t_document_report WITH HEADER LINE,
        s_refresh   TYPE boole_d,
        it_ckmllahd LIKE ckmllahd,
        y_curtp_lc  LIKE mlcr-curtp    VALUE '10',
        l_idx       LIKE sy-index.

  DATA: t_mlreport  TYPE TABLE OF mlreport WITH HEADER LINE,
        t_mlhd      LIKE mlhd   OCCURS 0 WITH HEADER LINE,
        t_mlmst     LIKE mlmst  OCCURS 0 WITH HEADER LINE,
        t_mlit      TYPE TABLE OF mlit WITH HEADER LINE,

** Furong Performace tuning 07/24/2014
*        t_mlpp      TYPE SORTED TABLE OF mlpp WITH HEADER LINE
*                    WITH NON-UNIQUE KEY belnr kjahr posnr,
*        t_mlppf     TYPE SORTED TABLE OF mlppf WITH HEADER LINE
*                    WITH NON-UNIQUE KEY belnr kjahr posnr,
*        t_mlcr      TYPE SORTED TABLE OF mlcr  WITH HEADER LINE
*                    WITH NON-UNIQUE KEY belnr kjahr posnr,
*        t_mlcrf     TYPE SORTED TABLE OF mlcrf WITH HEADER LINE
*                    WITH NON-UNIQUE KEY belnr kjahr posnr,
        t_mlpp      TYPE SORTED TABLE OF mlpp WITH HEADER LINE
                    WITH UNIQUE KEY belnr kjahr posnr bdatj poper,
        t_mlppf     TYPE SORTED TABLE OF mlppf WITH HEADER LINE
                    WITH UNIQUE KEY belnr kjahr posnr bdatj poper feldg,
        t_mlcr      TYPE SORTED TABLE OF mlcr  WITH HEADER LINE
                    WITH UNIQUE KEY belnr kjahr posnr bdatj poper curtp,
        t_mlcrf     TYPE SORTED TABLE OF mlcrf WITH HEADER LINE
                    WITH UNIQUE KEY belnr kjahr posnr
                                    bdatj poper curtp feldg,
** End Tuning
        t_mlcrp     LIKE mlcrp  OCCURS 0 WITH HEADER LINE,
        t_mlitmb    LIKE mlitmb OCCURS 0 WITH HEADER LINE,
        t_mlco      LIKE mlco   OCCURS 0 WITH HEADER LINE.

  DATA : l_tabix TYPE sy-tabix.
  DATA: lh_counter LIKE sy-tabix.

*Get Multi Level Price  / Qty  for Activity
*MATERIAL LEVEL... NOT ORDER
  SORT it_fsc_mat BY matnr bwkey bwtar .
  SORT it_act BY kalnr.

** Furong Performace tuning 07/24/2014
  DATA : lt_fsc_temp      LIKE it_fsc_mat OCCURS 0 WITH HEADER LINE,
         lt_mlreport_temp LIKE t_mlreport OCCURS 0 WITH HEADER LINE.
  CLEAR : lt_fsc_temp,      lt_fsc_temp[],
          lt_mlreport_temp, lt_mlreport_temp[].
** End tuning

  IF it_fsc_mat[] IS NOT INITIAL.
** Furong Performace tuning 07/24/2014
*    SELECT *
*      INTO TABLE t_mlreport
*      FROM  mlreport
*       FOR ALL ENTRIES IN it_fsc_mat
*     WHERE kalnr  = it_fsc_mat-kaln1
*       AND bdatj  = p_bdatj
*       AND poper  = p_perab
*       AND untper = '000'
*       AND psart  = 'MS'.
    MOVE : it_fsc_mat[] TO lt_fsc_temp[].
    SORT lt_fsc_temp BY kaln1.
    DELETE ADJACENT DUPLICATES FROM lt_fsc_temp COMPARING kaln1.

    SELECT *
      INTO TABLE t_mlreport
      FROM  mlreport
       FOR ALL ENTRIES IN lt_fsc_temp
     WHERE kalnr  = lt_fsc_temp-kaln1
       AND bdatj  = p_bdatj
       AND poper  = p_perab
       AND untper = '000'
       AND psart  = 'MS'.
    SORT t_mlreport BY kalnr belnr kjahr.
    REFRESH lt_fsc_temp.
  ENDIF.
** End tuning

  IF t_mlreport[] IS NOT INITIAL.
** Furong Performace tuning 07/24/2014
*    SELECT *
*      INTO TABLE t_mlhd
*      FROM mlhd
*       FOR ALL ENTRIES IN t_mlreport
*     WHERE belnr = t_mlreport-belnr
*       AND kjahr = t_mlreport-kjahr.
    MOVE : t_mlreport[] TO lt_mlreport_temp[].
    SORT lt_mlreport_temp BY belnr kjahr.
    DELETE ADJACENT DUPLICATES FROM lt_mlreport_temp
           COMPARING belnr kjahr.
    SELECT *
      INTO TABLE t_mlhd
      FROM mlhd
       FOR ALL ENTRIES IN lt_mlreport_temp
     WHERE belnr = lt_mlreport_temp-belnr
       AND kjahr = lt_mlreport_temp-kjahr.
*    SORT t_mlhd DESCENDING BY cpudt cputm.
    SORT t_mlhd BY belnr ASCENDING
                   kjahr ASCENDING
                   cpudt DESCENDING
                   cputm DESCENDING.
    SELECT *
      INTO TABLE t_mlit
      FROM mlit
       FOR ALL ENTRIES IN lt_mlreport_temp
     WHERE belnr = lt_mlreport_temp-belnr
       AND kjahr = lt_mlreport_temp-kjahr.
    REFRESH : lt_mlreport_temp.
** End tuing
  ENDIF.

  SORT t_mlit BY belnr kjahr posnr.

** Fuorng on 07/22/14 performance tuning

*  LOOP AT it_fsc_mat.
*    CLEAR: lh_counter.
*    LOOP AT t_mlreport WHERE kalnr = it_fsc_mat-kaln1.
*      LOOP AT t_mlhd WHERE belnr = t_mlreport-belnr.
*        READ TABLE t_mlit WITH KEY belnr = t_mlhd-belnr
*                                   kjahr = t_mlhd-kjahr
*                          BINARY SEARCH.
*        IF sy-subrc = 0.
*          ADD 1 TO lh_counter.
*          IF lh_counter GT 1.
*            DELETE t_mlreport
*             WHERE belnr = t_mlhd-belnr
*               AND kjahr = t_mlhd-kjahr.
*            DELETE t_mlit
*             WHERE belnr = t_mlhd-belnr
*               AND kjahr = t_mlhd-kjahr.
*            DELETE t_mlhd.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*  ENDLOOP.

  LOOP AT it_fsc_mat.
    CLEAR: lh_counter.
    READ TABLE t_mlreport WITH KEY kalnr = it_fsc_mat-kaln1
                          BINARY SEARCH.
    CHECK sy-subrc = 0.
    LOOP AT t_mlreport FROM sy-tabix.
      IF t_mlreport-kalnr <> it_fsc_mat-kaln1.
        EXIT.
      ENDIF.
      READ TABLE t_mlhd WITH KEY belnr = t_mlreport-belnr
                        BINARY SEARCH.
      CHECK sy-subrc = 0.
      LOOP AT t_mlhd FROM sy-tabix.
        IF t_mlhd-belnr <> t_mlreport-belnr.
          EXIT.
        ENDIF.
        READ TABLE t_mlit WITH KEY belnr = t_mlhd-belnr
                                   kjahr = t_mlhd-kjahr
                          BINARY SEARCH.
        IF sy-subrc = 0.
          ADD 1 TO lh_counter.
          IF lh_counter GT 1.
            DELETE t_mlreport
             WHERE belnr = t_mlhd-belnr
               AND kjahr = t_mlhd-kjahr.
            DELETE t_mlit
             WHERE belnr = t_mlhd-belnr
               AND kjahr = t_mlhd-kjahr.
            DELETE t_mlhd.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
** End tuning

  DELETE t_mlit WHERE NOT ( psart = 'MI' OR psart = 'AC' ).

  IF t_mlit[] IS NOT INITIAL.
    SELECT *
      INTO TABLE t_mlpp
      FROM mlpp
       FOR ALL ENTRIES IN t_mlit
     WHERE belnr = t_mlit-belnr
       AND kjahr = t_mlit-kjahr
       AND posnr = t_mlit-posnr.
*    SORT t_mlpp BY posnr.

    SELECT *
      INTO TABLE t_mlppf
      FROM mlppf
       FOR ALL ENTRIES IN t_mlit
     WHERE belnr = t_mlit-belnr
       AND kjahr = t_mlit-kjahr
       AND posnr = t_mlit-posnr
       AND feldg = 'VNOS'.
*    SORT t_mlppf BY posnr.

    SELECT *
      FROM mlcr
      INTO TABLE t_mlcr
       FOR ALL ENTRIES IN t_mlit
     WHERE belnr = t_mlit-belnr
       AND kjahr = t_mlit-kjahr
       AND posnr = t_mlit-posnr
       AND curtp = y_curtp_lc.
*    SORT t_mlcr BY posnr.

    SELECT *
      FROM mlcrf
      INTO TABLE t_mlcrf
       FOR ALL ENTRIES IN t_mlit
     WHERE belnr = t_mlit-belnr
       AND kjahr = t_mlit-kjahr
       AND posnr = t_mlit-posnr
       AND curtp = y_curtp_lc.
  ENDIF.

** Furong 07/24/2014 Tuning
  SORT t_mlit BY belnr kjahr process psart.
*.. End Tuning

  LOOP AT it_fsc_mat.

    l_idx = sy-tabix.

    h_kalnr = it_fsc_mat-kaln1.

*-> Commented out by Han Moon
*    CALL FUNCTION 'CKMD_DOCUMENT_REPORT'
*      EXPORTING
*        i_kalnr              = h_kalnr
*        i_bdatj              = p_bdatj
*        i_poper              = p_perab
*        i_only_not_mlcd_docs = 'X'
*        i_refresh_buffer     = s_refresh
*        i_online             = space
*      TABLES
*        ot_docs              = lt_docs
*      EXCEPTIONS
*        no_document_found    = 1
*        no_data_found        = 2
*        OTHERS               = 3.
*
*    IF sy-subrc <> 0.
*      IF p_debug = 'X'.
*        BREAK-POINT.
*      ENDIF.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    DELETE lt_docs WHERE bdatj <> p_bdatj.
*
*    SORT lt_docs BY vgart.
*
*    READ TABLE lt_docs WITH KEY vgart = 'MS'    "Multilevel Price Determination
*                                BINARY SEARCH.
*    IF sy-subrc <> 0.
**     DELETE it_fsc_mat INDEX l_idx.
*      CONTINUE.
*    ENDIF.
*
*    REFRESH t_mlit.
**-----option 1: direct SQL
*    SELECT * FROM mlit INTO TABLE t_mlit
*      WHERE  belnr   = lt_docs-belnr
*        AND  kjahr   = lt_docs-kjahr            "p_bdatj ??? FIXME ANDY
*        AND  process = it_fsc_mat-proc_kalnr    "production version
*        AND  ( psart = 'MI' OR psart = 'AC' ).
*
*    IF sy-subrc NE 0.
**     DELETE it_fsc_mat INDEX l_idx.
*      CONTINUE.
*    ENDIF.
*
*
*    SELECT *
*      INTO TABLE t_mlpp
*      FROM mlpp
*       FOR ALL ENTRIES IN t_mlit
*     WHERE belnr = t_mlit-belnr
*       AND kjahr = t_mlit-kjahr
*       AND posnr = t_mlit-posnr.
**    SORT t_mlpp BY posnr.
*
*    SELECT *
*      INTO TABLE t_mlppf
*      FROM mlppf
*       FOR ALL ENTRIES IN t_mlit
*     WHERE belnr = t_mlit-belnr
*       AND kjahr = t_mlit-kjahr
*       AND posnr = t_mlit-posnr
*       AND feldg = 'VNOS'.
**    SORT t_mlppf BY posnr.
*
*    SELECT *
*      FROM mlcr
*      INTO TABLE t_mlcr
*       FOR ALL ENTRIES IN t_mlit
*     WHERE belnr = t_mlit-belnr
*       AND kjahr = t_mlit-kjahr
*       AND posnr = t_mlit-posnr
*       AND curtp = y_curtp_lc.
**    SORT t_mlcr BY posnr.
*
*    SELECT *
*      FROM mlcrf
*      INTO TABLE t_mlcrf
*       FOR ALL ENTRIES IN t_mlit
*     WHERE belnr = t_mlit-belnr
*       AND kjahr = t_mlit-kjahr
*       AND posnr = t_mlit-posnr
*       AND curtp = y_curtp_lc.
**    SORT t_mlcrf BY posnr.
*
***-----option 2: use SAP Function
**    IF 1 = 2.
**      CALL FUNCTION 'CKMD_DOCUMENT_READ_MLXX'
**        EXPORTING
**          I_BELNR     = LT_DOCS-BELNR
**          I_KJAHR     = LT_DOCS-KJAHR
**        TABLES
**          T_MLHD      = T_MLHD
**          T_MLMST     = T_MLMST
**          T_MLIT      = T_MLIT
**          T_MLPP      = T_MLPP
**          T_MLPPF     = T_MLPPF
**          T_MLCR      = T_MLCR
**          T_MLCRF     = T_MLCRF
**          T_MLCRP     = T_MLCRP
**          T_MLITMB    = T_MLITMB
**          T_MLCO      = T_MLCO
**        EXCEPTIONS
**          NO_DOCUMENT = 1
**          OTHERS      = 2.
**    ENDIF.
*
** refer CKMLMV013 tables..... PCC order - Material - production version
**ANDY FIX...
** MB = begin -> Only consider current Period.
** MI = input, AC = activity?
*<- Commented out by Han Moon

** Furong on 07/24/14 Performance Tuning

*    LOOP AT t_mlreport WHERE kalnr = it_fsc_mat-kaln1.
    READ TABLE t_mlreport WITH KEY kalnr = it_fsc_mat-kaln1
                           BINARY SEARCH.
    CHECK sy-subrc = 0.
    LOOP AT t_mlreport FROM sy-tabix.
      IF t_mlreport-kalnr <> it_fsc_mat-kaln1.
        EXIT.
      ENDIF.
*      LOOP AT t_mlit WHERE belnr   = t_mlreport-belnr
*                       AND kjahr   = t_mlreport-kjahr
*                       AND process = it_fsc_mat-proc_kalnr
*                       AND ( psart = 'MI' OR psart = 'AC' ).
      READ TABLE t_mlit WITH KEY belnr   = t_mlreport-belnr
                                 kjahr   = t_mlreport-kjahr
                                 process = it_fsc_mat-proc_kalnr
                        BINARY SEARCH.
      CHECK sy-subrc = 0.
      LOOP AT t_mlit FROM sy-tabix.
        IF t_mlit-belnr <> t_mlreport-belnr OR
           t_mlit-kjahr <> t_mlreport-kjahr OR
           t_mlit-process <> it_fsc_mat-proc_kalnr.
          EXIT.
        ENDIF.
        IF t_mlit-psart <> 'MI' AND t_mlit-psart <> 'AC'.
          CONTINUE.
        ENDIF.
** end tuning

        CLEAR t_mlite.
        MOVE-CORRESPONDING t_mlit TO t_mlite.
        MOVE it_fsc_mat-objnr     TO t_mlite-paobjnr.

        READ TABLE t_mlpp WITH KEY belnr = t_mlit-belnr
                                   kjahr = t_mlit-kjahr
                                   posnr = t_mlit-posnr
** Furong on 07/22/14 Performance Tuning
                          BINARY SEARCH.
** End tuing
        IF sy-subrc = 0.
          t_mlite-bdatj = t_mlpp-bdatj.
          t_mlite-poper = t_mlpp-poper.
          t_mlite-lbkum = t_mlpp-lbkum.
        ENDIF.

** Furong on 07/22/14 Performance Tuning
*            READ TABLE t_mlppf WITH TABLE KEY belnr = t_mlit-belnr
*                                          kjahr = t_mlit-kjahr
*                                          posnr = t_mlit-posnr.
        READ TABLE t_mlppf WITH KEY belnr = t_mlit-belnr
                              kjahr = t_mlit-kjahr
                              posnr = t_mlit-posnr
                     BINARY SEARCH.
** End tuing
        IF sy-subrc = 0.
          t_mlite-menge = t_mlppf-menge.
        ENDIF.

** Furong on 07/22/14 Performance Tuning
*       READ TABLE t_mlcr WITH TABLE KEY belnr = t_mlit-belnr
*                                         kjahr = t_mlit-kjahr
*                                         posnr = t_mlit-posnr
        READ TABLE t_mlcr WITH KEY belnr = t_mlit-belnr
                                          kjahr = t_mlit-kjahr
                                          posnr = t_mlit-posnr
                           BINARY SEARCH.
** End tuing
        IF sy-subrc = 0.
          t_mlite-curtp = t_mlcr-curtp.
          t_mlite-salk3 = t_mlcr-salk3.
          t_mlite-waers = t_mlcr-waers.
          t_mlite-exbwr = t_mlcr-exbwr.
          t_mlite-bnbtr = t_mlcr-bnbtr.
          t_mlite-vkwrt = t_mlcr-vkwrt.
          t_mlite-vkwra = t_mlcr-vkwra.
          t_mlite-exvkw = t_mlcr-exvkw.
          t_mlite-vksal_old = t_mlcr-vksal_old.
        ENDIF.

** Performance Tuning by Furong 07/24/2014
*        LOOP AT t_mlcrf WHERE belnr = t_mlit-belnr
*                          AND kjahr = t_mlit-kjahr
*                          AND posnr = t_mlit-posnr
*                          AND bdatj = t_mlpp-bdatj
*                          AND poper = t_mlpp-poper
*                          AND curtp = t_mlcr-curtp.
        READ TABLE t_mlcrf WITH KEY belnr = t_mlit-belnr
                               kjahr = t_mlit-kjahr
                               posnr = t_mlit-posnr
                               bdatj = t_mlpp-bdatj
                               poper = t_mlpp-poper
                               curtp = t_mlcr-curtp
                      BINARY SEARCH.
        CHECK sy-subrc = 0.
        LOOP AT t_mlcrf FROM sy-tabix.
          IF t_mlcrf-belnr <> t_mlit-belnr OR
             t_mlcrf-kjahr <> t_mlit-kjahr OR
             t_mlcrf-posnr <> t_mlit-posnr OR
             t_mlcrf-bdatj <> t_mlpp-bdatj OR
             t_mlcrf-poper <> t_mlpp-poper OR
             t_mlcrf-curtp <> t_mlcr-curtp.
            EXIT.
          ENDIF.
*.. End Tuning
          CASE t_mlcrf-feldg.
            WHEN 'ABMO'.   "Opening Period
              t_mlite-vnprd_ea = t_mlcrf-prdif.
              t_mlite-vnkdm_ea = t_mlcrf-krdif.
            WHEN 'VNEA'.
              t_mlite-vnprd_ea = t_mlcrf-prdif.
              t_mlite-vnkdm_ea = t_mlcrf-krdif.
            WHEN 'VNMA'.
              t_mlite-vnprd_ma = t_mlcrf-prdif.
              t_mlite-vnkdm_ma = t_mlcrf-krdif.
            WHEN 'EBEA'.
              t_mlite-ebprd_ea = t_mlcrf-prdif.
              t_mlite-ebkdm_ea = t_mlcrf-krdif.
            WHEN 'EBMA'.
              t_mlite-ebprd_ma = t_mlcrf-prdif.
              t_mlite-ebkdm_ma = t_mlcrf-krdif.
          ENDCASE.
        ENDLOOP.

        IF t_mlite-psart EQ 'AC'.
          READ TABLE it_act WITH KEY kalnr = t_mlite-kalnr
                                     BINARY SEARCH.
          IF sy-subrc = 0.
            t_mlite-act_objnr = it_act-objnr.
          ENDIF.
        ENDIF.
        APPEND t_mlite. CLEAR  t_mlite.

      ENDLOOP.

    ENDLOOP.
  ENDLOOP.
  SORT t_mlite BY paobjnr bwtar bwkey matnr act_objnr.

ENDFORM.                    " get_multilevel_price
*&---------------------------------------------------------------------*
*&      Form  get_activity_types
*&---------------------------------------------------------------------*
FORM get_activity_types.
* List of Cost Centers / Activity Types with Control Information
*  RANGES: r_obj FOR cost-objnr.
  DATA:  lt_info    TYPE TABLE OF bapi0012_actctrldata,
         wa_info    TYPE bapi0012_actctrldata.


*  CALL FUNCTION 'BAPI_CTR_GETACTIVITYTYPES'
*    EXPORTING
*      coarea      = p_kokrs
*      fiscyear    = p_bdatj
*      version     = '000'
*    TABLES
*      return      = return
*      controldata = lt_info.
*
** get activity actual price
*  REFRESH r_obj.
*  r_obj-option = 'EQ'. r_obj-sign = 'I'.
*
*  LOOP AT lt_info INTO wa_info.
*    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
*      EXPORTING
*        kokrs = p_kokrs
*        kostl = wa_info-costcenter
*        lstar = wa_info-acttype
*      IMPORTING
*        objnr = r_obj-low.
*    APPEND r_obj.
*  ENDLOOP.


* Local Data Definition
  DATA : it_l_cost LIKE STANDARD TABLE OF cost WITH HEADER LINE.
  DATA : lv_cnt    LIKE p_perab.

  FIELD-SYMBOLS : <fs_tkg> TYPE any, <fs_tke> TYPE any.

  DATA : lv_tkg(30) VALUE 'IT_L_COST-TKGxxx',
         lv_tke(30) VALUE 'IT_L_COST-TKExxx'.

* Select
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_l_cost
    FROM cost CLIENT SPECIFIED
   WHERE mandt  = sy-mandt
     AND lednr  = '00'
*     AND objnr  IN r_obj
     AND gjahr  =  p_bdatj
     AND wrttp  = '04'
     AND versn  = '000'
     AND tarkz  = '005'.

* Get data by Period (From P_PERAB to w_perbi)
* Clear
  CLEAR : it_ccr1t, it_ccr1t[].

  LOOP AT it_l_cost.

    CLEAR lv_cnt.
    lv_cnt  = p_perab.

    WHILE lv_cnt <= w_perbi.
* Trans. Data
      MOVE-CORRESPONDING it_l_cost TO it_ccr1t.

      MOVE lv_cnt TO: lv_tkg+13, lv_tke+13.
      ASSIGN: (lv_tkg) TO <fs_tkg>,
              (lv_tke) TO <fs_tke>.

      it_ccr1t-tkgxxx = <fs_tkg>.  "value
      it_ccr1t-tkexxx = <fs_tke>.  "price unit
* Set Period.
*     it_ccr1t-periode = lv_cnt.
* Kostl/Lstar
      CALL FUNCTION 'OBJECT_KEY_GET_KL'
        EXPORTING
          objnr       = it_ccr1t-objnr
        IMPORTING
          kokrs       = p_kokrs
          kostl       = it_ccr1t-kostl
          lstar       = it_ccr1t-lstar
        EXCEPTIONS
          not_found   = 1
          wrong_obart = 2
          OTHERS      = 3.

* Append
      APPEND it_ccr1t.
      CLEAR  it_ccr1t.
* Period Counter.
      lv_cnt = lv_cnt + 1.

    ENDWHILE .

    CLEAR  it_l_cost.

  ENDLOOP.

  CLEAR  it_ccr1t.

ENDFORM.                    " get_activity_types
*&---------------------------------------------------------------------*
*&      Form  read_wips
*&---------------------------------------------------------------------*
FORM read_wips.

* Consider COSB ; result analysis for MTO
  PERFORM read_wip_from_cosb.  "cosb -> it_obj_wip

* Compute WIP Qty & Amount...
* Fill same structure = gs_quantities (summarize)
  SORT it_obj_wip BY objnr.

  LOOP AT it_fsc_mat.
*FIXME
    READ TABLE it_obj_wip WITH KEY objnr = it_fsc_mat-objnr
                               BINARY SEARCH.
    CHECK sy-subrc = 0.
*    IF sy-subrc <> 0
*    OR ( it_obj_wip-amount = 0 AND it_obj_wip-amount2 = 0 ).
**      MESSAGE s000 WITH '...skipping WIP: '  it_fsc_mat-matnr.
*      CONTINUE.
*    ENDIF.
**    MESSAGE s000 WITH '...reading WIP: '  it_fsc_mat-matnr.

*//Modify..03/25/2011..T00020..
    CASE it_fsc_mat-categ.
      WHEN 'DI'.    "APO
        PERFORM read_wips_di.

      WHEN 'MTO'.
        PERFORM read_wips_mto.

      WHEN 'REM'.
        PERFORM read_wips_rem.

      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

*    IF it_fsc_mat-categ = 'DI'.    "APO
*      PERFORM read_wips_di.
*
*    ELSEIF it_fsc_mat-categ = 'MTO'.
*      PERFORM read_wips_mto.
*
**REM: WIP...
*    ELSEIF it_fsc_mat-categ = 'REM'.
*      PERFORM read_wips_rem.
*
*    ELSE.
*
*    ENDIF.
*//

  ENDLOOP.


ENDFORM.                    " read_wips
*&---------------------------------------------------------------------*
*&      Form  read_osd_alloc
*&---------------------------------------------------------------------*
*FORM read_osd_alloc.
*
*  SELECT kokrs  gjahr      period    versn    kstar    matnr
*           werks  io_aufnr    pcc_aufnr
*           chg_wkgbtr    waers
*           mbgbtr        meinb
*       FROM ztco_abispost
*       INTO  CORRESPONDING FIELDS OF TABLE it_ztco_abispost
*      WHERE kokrs  = p_kokrs
*        AND gjahr  = p_bdatj
*        AND period =  p_perab
*        AND versn  = p_versn
*        AND werks  = it_fsc_mat-werks
*        AND pcc_aufnr = it_fsc_mat-aufnr
*        AND fsc_matnr = it_fsc_mat-matnr .
*
*ENDFORM.                    " read_osd_alloc
*&---------------------------------------------------------------------*
*&      Form  fill_shopcost_header
*&---------------------------------------------------------------------*
FORM fill_shopcost_header.
*FIXME LATER
  EXIT.

  it_shop_sum-kokrs = p_kokrs.
  it_shop_sum-bdatj = p_bdatj.
  it_shop_sum-poper = p_perab.
*  IT_SHOP_SUM-record_type = gv_record_type.  "Record Type: A-Actual
*   IT_SHOP_SUM-versn  = it_gcovp-versn.

ENDFORM.                    " fill_shopcost_header
*&---------------------------------------------------------------------*
*&      Form  move_coep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM move_coep.

  SORT it_mto_scrap BY objnr.
  SORT it_mhsum     BY objnr kostl lstar.

  LOOP AT it_coep.
*    IF p_gr = space AND it_coep-typps = 'O'. CONTINUE. ENDIF.

    CLEAR it_shop_sum.
    IF it_coep-typps CA 'OX'.
      CLEAR: it_coep-matnr, it_coep-werks, it_coep-kstar, it_coep-meinb.
    ENDIF.

    it_shop_sum-objnr      = it_coep-objnr     .
    it_shop_sum-kstar      = it_coep-kstar     .

    it_shop_sum-bwkey      = it_coep-werks     .
    it_shop_sum-llv_matnr  = it_coep-matnr     .
    it_shop_sum-kostl      = it_coep-kostl     .
    it_shop_sum-lstar      = it_coep-lstar     .

    IF it_coep-typps = 'E'.
      CLEAR it_csla.
      READ TABLE it_csla WITH KEY lstar = it_coep-lstar
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        it_shop_sum-meeht    = it_csla-leinh     .
      ENDIF.
    ELSE.
      it_shop_sum-meeht      = it_coep-meinb     .
    ENDIF.

    CASE it_coep-typps.
*     Current
*     M: Material
      WHEN 'M' .
        it_shop_sum-wkgbtr     = it_coep-wkgbtr.
        it_shop_sum-mbgbtr     = it_coep-mbgbtr.
*     E:Activity
      WHEN 'E'.
        it_shop_sum-wkgbtr     = it_coep-wkgbtr.
        it_shop_sum-mbgbtr     = it_coep-mbgbtr.
      WHEN 'V'.
        it_shop_sum-add_wkgbtr = it_coep-wkgbtr.
        it_shop_sum-add_mbgbtr = it_coep-mbgbtr.
*scrap
        it_shop_sum-scrap_amt  = it_coep-scrap_amt.
        it_shop_sum-scrap_qty  = it_coep-scrap_qty.

*//Modify..03/28/2011..T00020..
        it_shop_sum-key_amt   = it_coep-keyin_amt.
        it_shop_sum-key_qty   = it_coep-keyin_qty.
        it_shop_sum-osnd_amt  = it_coep-osnd_amt.
        it_shop_sum-osnd_qty  = it_coep-osnd_qty.
*//

      WHEN 'O'.
        it_shop_sum-typps = 'O'.
        it_shop_sum-gr_amt     = it_coep-wkgbtr.
        it_shop_sum-gr_qty     = it_coep-mbgbtr.

* NEED further logic for scrap - MTS, DI
        READ TABLE it_mto_scrap WITH KEY objnr = it_coep-objnr
                                BINARY SEARCH.
        IF sy-subrc = 0 AND it_mto_scrap-xmnga <> 0.
          it_shop_sum-scrap_qty = - it_mto_scrap-xmnga.  "Neg.Sign
        ENDIF.

      WHEN 'X'.
        it_shop_sum-typps = 'O'.
        it_shop_sum-single_amt = it_coep-wkgbtr.

      WHEN 'F'.  "Misc Posting
        it_shop_sum-typps = 'V'.
        it_shop_sum-wkgbtr = it_coep-wkgbtr.
*->     Added by Han Moon
        it_shop_sum-mbgbtr = it_coep-mbgbtr.
*<-
      WHEN OTHERS.
        it_shop_sum-typps  = it_coep-typps.
        it_shop_sum-wkgbtr = it_coep-wkgbtr.
*->     Added by Han Moon
        it_shop_sum-mbgbtr = it_coep-mbgbtr.
*<-
    ENDCASE.

    IF it_shop_sum-typps <> 'O'.
      IF  it_shop_sum-llv_matnr <> ''.
        it_shop_sum-typps = 'M'.
        PERFORM make_resou USING 'M' it_shop_sum-bwkey
                                     it_shop_sum-llv_matnr
                           CHANGING it_shop_sum-resou.
      ELSE.
        CLEAR it_shop_sum-bwkey.
        IF it_shop_sum-lstar <> ''.
          it_shop_sum-typps = 'E'.
          PERFORM make_resou USING 'E' it_shop_sum-kostl
                                       it_shop_sum-lstar
                             CHANGING  it_shop_sum-resou.

*Adjust Variance Posting
          READ TABLE it_mhsum WITH KEY objnr = it_shop_sum-objnr
                                       kostl = it_shop_sum-kostl
                                       lstar = it_shop_sum-lstar
                                   BINARY SEARCH.
          IF sy-subrc = 0.
*---------- Amount; MTO,MTS only,  DI->use WIP table...
            it_shop_sum-wkgbtr     =
                                  it_shop_sum-wkgbtr - it_mhsum-varamt.
            it_shop_sum-mbgbtr     =
                                  it_shop_sum-mbgbtr - it_mhsum-varmn.
            it_shop_sum-add_wkgbtr = it_shop_sum-add_wkgbtr +
                                     it_mhsum-varamt.
            it_shop_sum-add_mbgbtr = it_shop_sum-add_mbgbtr  +
                                     it_mhsum-varmn.
          ENDIF.
        ELSE.
          it_shop_sum-typps = 'V'.
          PERFORM make_resou USING 'V' it_shop_sum-kostl
                                       it_shop_sum-lstar
                             CHANGING  it_shop_sum-resou.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_shop_sum-resou = p_resou AND p_debug = 'X'.
      BREAK-POINT.
    ENDIF.

    COLLECT it_shop_sum. CLEAR it_shop_sum.

  ENDLOOP.

*Old Logic
*     READ TABLE it_ztco_abispost WITH KEY pcc_aufnr =  it_shopcst-aufnr
*                                              matnr = it_coep-matnr
*          IT_SHOP_SUM-add_wkgbtr = it_ztco_abispost-chg_wkgbtr.
*          IT_SHOP_SUM-add_mbgbtr = it_ztco_abispost-mbgbtr .

ENDFORM.                    " move_coep
*&---------------------------------------------------------------------*
*&      Form  MOVE_WIP
*&---------------------------------------------------------------------*
FORM move_wip.

  LOOP AT lt_wipvalue .

    CLEAR it_shop_sum.

    it_shop_sum-objnr      = lt_wipvalue-objnr.
    it_shop_sum-kstar      = lt_wipvalue-kstar.

    it_shop_sum-bwkey      = lt_wipvalue-bwkey.
    it_shop_sum-llv_matnr  = lt_wipvalue-material.
    it_shop_sum-kostl      = lt_wipvalue-kostl.
    it_shop_sum-lstar      = lt_wipvalue-lstar.

    IF lt_wipvalue-material NE ''.
      it_shop_sum-typps      = 'M'.
      PERFORM make_resou USING 'M' it_shop_sum-bwkey
                                   it_shop_sum-llv_matnr
                         CHANGING it_shop_sum-resou.
    ELSE.
      CLEAR it_shop_sum-bwkey.
      it_shop_sum-typps = 'E'.
      PERFORM make_resou USING 'E' it_shop_sum-kostl
                                   it_shop_sum-lstar
                         CHANGING  it_shop_sum-resou.
    ENDIF.

    it_shop_sum-meeht        = lt_wipvalue-unit.

*WIP-------------------------
    it_shop_sum-wip_amt      = lt_wipvalue-wip_amt.
    it_shop_sum-wip_qty      = lt_wipvalue-wip_quantity.


* Prev WIP
    it_shop_sum-wip_pamt     = lt_wipvalue-wip_pamt.
    it_shop_sum-wip_pqty     = lt_wipvalue-prev_wip.

*//Modify..04/27/2011..T00020..
    it_shop_sum-wip_f        = lt_wipvalue-wip_f.
    it_shop_sum-wip_qty_f    = lt_wipvalue-wip_quantity_f.

    it_shop_sum-wip_pf       = lt_wipvalue-wip_pf.
    it_shop_sum-wip_pqty_f   = lt_wipvalue-prev_wip_f.
*//

    it_shop_sum-scrap_qty    = lt_wipvalue-actual_scrap.
    it_shop_sum-scrap_amt    = lt_wipvalue-scrap_amt.

* APO Information
    it_shop_sum-apo_var_qty    = lt_wipvalue-variance_qty.
    it_shop_sum-apo_input_qty  = lt_wipvalue-apo_input_qty .
    it_shop_sum-apo_output_qty = lt_wipvalue-apo_output_qty.
*   IT_SHOP_SUM-apo_meins      = lt_wipvalue-apo_meins.

*Variance-------------------- Normal Posting from ML view
*FIXME --- ANDY
    it_shop_sum-wkgbtr2  =   lt_wipvalue-var_amt.
    it_shop_sum-mbgbtr2  =   lt_wipvalue-variance_qty   .
    it_shop_sum-wkgbtr   = - lt_wipvalue-var_amt.
    it_shop_sum-mbgbtr   = - lt_wipvalue-variance_qty.

**   Material
*    IF IT_SHOP_SUM-typps = 'M' .
**      WIP - Begining
*       perform making_shop_cc_wip using  'B'
*                                         IT_SHOP_SUM-wip_qty "Qty
*                                         IT_SHOP_SUM-wip_amt."Amt
**      WIP - Ending
*       perform making_shop_cc_wip using  'E'
*                                         IT_SHOP_SUM-wip_pqty "Qty
*                                         IT_SHOP_SUM-wip_pamt."Amt
*
**      Current Var
*       perform making_shop_cc_material using  'V'
*                                               it_shopcst-mbgbtr2. "Qty
*
*       perform making_shop_cc_material using  'C'
*                                               it_shopcst-mbgbtr.  "Qty
**      Scrap
*       perform making_shop_cc_material using  'S'
*                                               it_shopcst-scrap_qty."Qt
** Activity
*    ELSE.
**      WIP - Begining
*       perform making_shop_cc_wip_Act using  'B'
*                                         IT_SHOP_SUM-wip_qty "Qty
*                                         IT_SHOP_SUM-wip_amt."Amt
**      WIP - Ending
*       perform making_shop_cc_wip_Act using  'E'
*                                         IT_SHOP_SUM-wip_pqty "Qty
*                                         IT_SHOP_SUM-wip_pamt."Amt
**      Current Var
*       perform making_shop_cc_activity using  'V'
*                                               it_shopcst-mbgbtr2. "Qty
*
*       perform making_shop_cc_activity using  'C'
*                                               it_shopcst-mbgbtr.  "Qty
**      Scrap
*       perform making_shop_cc_activity using  'S'
*                                               IT_SHOP_SUM-scrap_qty.
*   ENDIF.

    IF it_shop_sum-resou = p_resou AND p_debug = 'X'.
      BREAK-POINT.
    ENDIF.

    COLLECT it_shop_sum. CLEAR it_shop_sum.

  ENDLOOP.

ENDFORM.                    " MOVE_WIP
*&---------------------------------------------------------------------*
*&      Form  move_gr_items
*&---------------------------------------------------------------------*
FORM move_gr_items.

  SORT it_fsc_gr BY werks matnr.
  SORT lt_ck13n  BY werks matnr.
*  SORT it_mat    BY matnr bwkey bwtar.

  LOOP AT it_obj_gr.

    READ TABLE it_fsc_gr WITH KEY werks = it_obj_gr-werks
                                  matnr = it_obj_gr-matnr
                         BINARY SEARCH.

    READ TABLE lt_ck13n WITH KEY werks = it_obj_gr-werks
                                 matnr = it_obj_gr-matnr
                                 BINARY SEARCH.
    CHECK sy-subrc = 0.

    LOOP AT lt_ck13n FROM sy-tabix.
      IF lt_ck13n-werks <> it_obj_gr-werks OR
         lt_ck13n-matnr <> it_obj_gr-matnr.
        EXIT.
      ENDIF.

      it_shop_sum-objnr      = it_obj_gr-objnr.

      it_shop_sum-kstar      = lt_ck13n-kstar.
      it_shop_sum-bwkey      = lt_ck13n-plant.
      it_shop_sum-llv_matnr  = lt_ck13n-material.
      it_shop_sum-kostl      = lt_ck13n-kostl.
      it_shop_sum-lstar      = lt_ck13n-lstar.

      it_shop_sum-meeht      = lt_ck13n-meeht.
      it_shop_sum-gr_qty     = lt_ck13n-gr_qty *
                       it_obj_gr-grqty / it_fsc_gr-lotsize.


      IF lt_ck13n-material NE ''.

        it_shop_sum-typps = 'M'.
        PERFORM make_resou USING 'M' it_shop_sum-bwkey
                                     it_shop_sum-llv_matnr
                           CHANGING it_shop_sum-resou.

        PERFORM check_it_mat USING it_shop_sum-llv_matnr
                                   it_shop_sum-bwkey
                             CHANGING sy-subrc.
*        CLEAR it_mat.
*        READ TABLE  it_mat WITH KEY matnr = it_shop_sum-llv_matnr
*                                    bwkey = it_shop_sum-bwkey
*                                    bwtar = it_shop_sum-bwtar
*                                    BINARY SEARCH.
        IF sy-subrc = 0 AND it_mat-meins <> it_shop_sum-meeht.
          PERFORM unit_converion
            USING    it_shop_sum-gr_qty it_shop_sum-meeht it_mat-meins
            CHANGING it_shop_sum-gr_qty.
          it_shop_sum-meeht = it_mat-meins.

        ENDIF.


      ELSEIF NOT lt_ck13n-lstar IS INITIAL.  "Activity

        CLEAR it_shop_sum-bwkey.
        it_shop_sum-typps = 'E'.
        PERFORM make_resou USING 'E' it_shop_sum-kostl
                                     it_shop_sum-lstar
                           CHANGING  it_shop_sum-resou.
* Change unit :  'STD' => Activity master unit
        CLEAR it_csla.
        READ TABLE it_csla WITH KEY lstar = it_shop_sum-lstar
                                    BINARY SEARCH.
        IF it_csla-leinh <> it_shop_sum-meeht.
          PERFORM unit_converion
            USING    it_shop_sum-gr_qty it_shop_sum-meeht it_csla-leinh
            CHANGING it_shop_sum-gr_qty.
          it_shop_sum-meeht = it_csla-leinh.
        ENDIF.


      ELSE.

        CLEAR: it_shop_sum-bwkey, it_shop_sum-meeht.
        it_shop_sum-typps = 'V'.

      ENDIF.


      it_shop_sum-gr_amt     = lt_ck13n-gr_amt *
                       it_obj_gr-grqty / it_fsc_gr-lotsize.

      IF it_shop_sum-resou = p_resou AND p_debug = 'X'.
        BREAK-POINT.
      ENDIF.

      COLLECT it_shop_sum. CLEAR it_shop_sum.

    ENDLOOP.


  ENDLOOP.

ENDFORM.                    " move_gr_items
*&---------------------------------------------------------------------*
*&      Form  move_ml_data
*&---------------------------------------------------------------------*
FORM move_ml_data.
  DATA: wa_l_ionra  LIKE ionra,
        l_aufnr     LIKE ionra-aufnr,
        l_objnr     LIKE coep-objnr.


* Multi-Level Price Determination for Material.
  SORT t_mlite BY paobjnr.
  SORT it_act  BY kalnr.

  SORT it_fsc_mat BY aufnr.

  LOOP AT t_mlite.

    AT NEW paobjnr.
      l_aufnr = t_mlite-paobjnr.
      UNPACK l_aufnr TO l_aufnr.
*      CALL FUNCTION 'K_AUFNR_OBJECT_KEY_GET'
*        EXPORTING
*          aufnr = l_aufnr
*          kokrs = p_kokrs
*        IMPORTING
*          objnr = l_objnr.
      READ TABLE it_fsc_mat WITH KEY aufnr = l_aufnr
                            BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_objnr = it_fsc_mat-objnr.
      ENDIF.
    ENDAT.

    CLEAR it_shop_sum.

    it_shop_sum-objnr          =  l_objnr.
    it_shop_sum-meeht          =  t_mlite-meins.

    IF t_mlite-act_objnr(2) = 'KL'.

      it_shop_sum-typps          = 'E'.
      READ TABLE it_act WITH KEY kalnr = t_mlite-kalnr
                             BINARY SEARCH.
      it_shop_sum-kostl     = it_act-kostl.
      it_shop_sum-lstar     = it_act-lstar.
*      it_shop_sum-chd_kalnr = it_act-kalnr.
*      CALL FUNCTION 'OBJECT_KEY_GET_KL'
*        EXPORTING
*          objnr  = t_mlite-act_objnr
*        IMPORTING
*          kostl  = it_shop_sum-kostl
*          lstar  = it_shop_sum-lstar
*        EXCEPTIONS
*          OTHERS = 1.

      READ TABLE it_csla WITH KEY lstar = it_shop_sum-lstar
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        it_shop_sum-kstar = it_csla-vksta.
      ENDIF.

      PERFORM make_resou USING 'E' it_shop_sum-kostl
                                   it_shop_sum-lstar
                         CHANGING  it_shop_sum-resou.
* Change unit :  'STD' => Activity master unit
      CLEAR it_csla.
      READ TABLE it_csla WITH KEY lstar = it_shop_sum-lstar
                                  BINARY SEARCH.

      IF it_csla-leinh <> it_shop_sum-meeht.
        PERFORM unit_converion
          USING    t_mlite-menge it_shop_sum-meeht it_csla-leinh
          CHANGING it_shop_sum-multi_qty.
        it_shop_sum-meeht = it_csla-leinh.
      ELSE.
        it_shop_sum-multi_qty   = t_mlite-menge.
      ENDIF.

*      perform unit_convert_to_std using    t_mlite-menge
*                                  changing it_shop_sum-meeht
*                                           it_shop_sum-multi_qty.

    ELSE.

      it_shop_sum-typps          = 'M'.
      it_shop_sum-llv_matnr      =  t_mlite-matnr.
      it_shop_sum-bwkey          =  t_mlite-bwkey.
      it_shop_sum-bwtar          =  t_mlite-bwtar.

      PERFORM check_it_mat USING t_mlite-matnr
                                 t_mlite-bwkey
                           CHANGING sy-subrc.
*      READ TABLE it_mat WITH KEY matnr = t_mlite-matnr
*                                 bwkey = t_mlite-bwkey
*                                 bwtar = t_mlite-bwtar
*                                 BINARY SEARCH.
*      IF sy-subrc <> 0.
*        PERFORM read_material_add USING it_shop_sum-llv_matnr
*                                        it_shop_sum-bwkey
*                                  CHANGING it_mat.
*      ENDIF.


      it_shop_sum-kstar = it_mat-kstar.

*      READ TABLE it_mat_tmp WITH KEY matnr = t_mlite-matnr
*                                  bwkey = t_mlite-bwkey
*                                  bwtar = t_mlite-bwtar.
*      IF sy-subrc = 0.
*        it_shop_sum-kstar = it_mat-kstar.
*      ENDIF.


      PERFORM make_resou USING 'M' it_shop_sum-bwkey
                                   it_shop_sum-llv_matnr
                         CHANGING  it_shop_sum-resou.

      it_shop_sum-multi_qty  =  t_mlite-menge.

    ENDIF.

*
    it_shop_sum-multi_amt  =  t_mlite-vnprd_ea + t_mlite-vnprd_ma.
    it_shop_sum-multi_samt =  t_mlite-vnprd_ea.
    it_shop_sum-multi_mamt =  t_mlite-vnprd_ma.

* Diffrence about Exchange Rate
    it_shop_sum-ml_fd_amt  =  t_mlite-vnkdm_ea + t_mlite-vnkdm_ma.
    it_shop_sum-ml_fd_samt =  t_mlite-vnkdm_ea.
    it_shop_sum-ml_fd_mamt =  t_mlite-vnkdm_ma.


*   Cost component
*    perform making_shop_cc_ml_multi.

    IF it_shop_sum-resou = p_resou AND p_debug = 'X'.
      BREAK-POINT.
    ENDIF.

    COLLECT it_shop_sum.  CLEAR it_shop_sum.

  ENDLOOP.

ENDFORM.                    " move_ml_data
*&---------------------------------------------------------------------*
*&      Form  WIP_VALUATION_DI
*&---------------------------------------------------------------------*
FORM wip_valuation_di.
* previous unit price ( depending price control ? V,S ? FIXME )
  DATA: l_p_unitp    TYPE base_price_scale,
        l_p_peinh    LIKE mbew-peinh.

  SORT gs_quantities BY act_objnr objnr kaln1.
  SORT it_csla       BY lstar.
  SORT it_unitpc     BY costcenter acttype.
  SORT it_unitpp     BY costcenter acttype.
*  SORT it_mat        BY matnr bwkey bwtar.
  SORT i_t030        BY bklas.

  LOOP AT gs_quantities .

    MOVE-CORRESPONDING gs_quantities TO ls_wip_qty.

    IF gs_quantities-act_objnr IS INITIAL.
*.. Change by sgcho 12/17/2013 Request by Hong, Youngki
*      l_objnr(2) = 'VS'.
*      l_objnr+2 = gs_quantities-kzbws.
*      l_objnr+3 = gs_quantities-sobkz.
*      l_objnr+4 = gs_quantities-kaln1.
      l_objnr(2) = 'MK'.
      l_objnr+2  = gs_quantities-kaln1.
*.. End Change.

      CALL FUNCTION 'QRP_APO_COMP_OBJNR_DECODE'
        EXPORTING
          if_f_objnr      = l_objnr
          if_complete_key = 'X'
        IMPORTING
          ef_kaln1        = ls_wip_qty-kaln1
          ef_kzbws        = ls_wip_qty-kzbws
          ef_sobkz        = ls_wip_qty-sobkz
          ef_matnr        = ls_wip_qty-material
          ef_bwkey        = ls_wip_qty-bwkey
          ef_bwtar        = ls_wip_qty-bwtar
          ef_vbeln        = ls_wip_qty-vbeln
          ef_posnr        = ls_wip_qty-posnr.

    ELSE.

      CALL FUNCTION 'OBJECT_KEY_GET_KL'
        EXPORTING
          objnr  = gs_quantities-act_objnr
        IMPORTING
          kostl  = ls_wip_qty-kostl
          lstar  = ls_wip_qty-lstar
        EXCEPTIONS
          OTHERS = 1.

      IF NOT sy-subrc IS INITIAL AND p_debug = 'X'.
        BREAK-POINT.
      ELSE.
      ENDIF.

* Change unit :  'STD' => Activity master unit
      CLEAR it_csla.
      READ TABLE it_csla WITH KEY lstar = ls_wip_qty-lstar
                                  BINARY SEARCH.

      IF it_csla-leinh <> ls_wip_qty-unit.
        PERFORM convert_unit_wip_std USING ls_wip_qty-unit
                                           it_csla-leinh.
        ls_wip_qty-unit = it_csla-leinh.
      ENDIF.
*      if gs_quantities-unit <> 'STD'.
*        perform convert_unit_wip_std.
*      endif.
    ENDIF.

*    MOVE gs_quantities-unit TO ls_wip_qty-apo_meins.
    MOVE-CORRESPONDING ls_wip_qty TO lt_wipvalue.
*   MOVE ls_wip_qty-objnr TO lt_wipvalue-objnr.
* Determine Price.


*---activity
    IF ls_wip_qty-material = space.

      READ TABLE it_csla WITH KEY lstar = lt_wipvalue-lstar
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        lt_wipvalue-kstar = it_csla-vksta.
      ENDIF.

      READ TABLE it_unitpc WITH KEY costcenter = ls_wip_qty-kostl
                                    acttype    = ls_wip_qty-lstar
                                    BINARY SEARCH.

      IF sy-subrc <> 0 AND p_debug = 'X'. BREAK-POINT. ENDIF.

      lt_wipvalue-unitp = it_unitpc-price_ocurr_fix +
                          it_unitpc-price_ocurr_var.
      lt_wipvalue-peinh = it_unitpc-price_ocurr_unit.


      READ TABLE it_unitpp WITH KEY costcenter = ls_wip_qty-kostl
                                    acttype    = ls_wip_qty-lstar
                                    BINARY SEARCH.

      l_p_unitp = it_unitpp-price_ocurr_fix + it_unitpp-price_ocurr_var.
      l_p_peinh = it_unitpp-price_ocurr_unit.


    ELSE.
      PERFORM check_it_mat USING lt_wipvalue-material
                                 ls_wip_qty-bwkey
                           CHANGING sy-subrc.
*      READ TABLE it_mat WITH KEY matnr = lt_wipvalue-material
*                                 bwkey = ls_wip_qty-bwkey
*                                 bwtar = ls_wip_qty-bwtar
*                                 BINARY SEARCH.
*      IF sy-subrc <> 0.
*        PERFORM read_material_add USING lt_wipvalue-material
*                                        ls_wip_qty-bwkey
*                                  CHANGING it_mat.
*      ENDIF.

      lt_wipvalue-unitp = it_mat-vmstc.
      lt_wipvalue-peinh = it_mat-vmpec.

      READ TABLE i_t030 WITH KEY bklas = it_mat-bklas
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        lt_wipvalue-kstar = i_t030-konts.
      ENDIF.

      l_p_unitp = it_mat-vmstp.
      l_p_peinh = it_mat-vmpep.

    ENDIF.


    lt_wipvalue-var_amt = lt_wipvalue-variance_qty *
                          lt_wipvalue-unitp / lt_wipvalue-peinh.
    lt_wipvalue-wip_amt = lt_wipvalue-wip_quantity *
                          lt_wipvalue-unitp / lt_wipvalue-peinh.

    lt_wipvalue-wip_pamt = lt_wipvalue-prev_wip * l_p_unitp / l_p_peinh.

    lt_wipvalue-scrap_amt = lt_wipvalue-actual_scrap *
                            lt_wipvalue-unitp / lt_wipvalue-peinh.

    APPEND lt_wipvalue.

*M/H
    IF ls_wip_qty-material = space.
      MOVE-CORRESPONDING lt_wipvalue TO it_mhsum.
      it_mhsum-meinh   = lt_wipvalue-unit.
      it_mhsum-varamt  = lt_wipvalue-var_amt.
      it_mhsum-varmn   = lt_wipvalue-variance_qty.
      COLLECT it_mhsum.
    ENDIF.

    CLEAR : lt_wipvalue, ls_wip_qty.

  ENDLOOP.

ENDFORM.                    " WIP_VALUATION_DI
*&---------------------------------------------------------------------*
*&      Form  get_material_prices
*&---------------------------------------------------------------------*
FORM get_material_prices.
  DATA:
        l_lfgja  LIKE ckmlpp-bdatj,
        l_lfmon  LIKE ckmlpp-poper,
        l_lfgja2 LIKE ckmlpp-bdatj,
        l_lfmon2 LIKE ckmlpp-poper,
        l_idx    LIKE sy-tabix.
  DATA: lt_kalnr  TYPE ckmv0_matobj_tbl WITH HEADER LINE,
        lt_ckmlpp TYPE TABLE OF ckmlpp WITH HEADER LINE,
        lt_ckmlcr TYPE TABLE OF ckmlcr WITH HEADER LINE.

*  DATA: lt_ckmlcr TYPE HASHED TABLE OF tt_ckmlcr WITH HEADER LINE
*                  WITH UNIQUE KEY bdatj poper kalnr.
*
**  DATA: lt_ckmlcr2 TYPE tt_ckmlcr OCCURS 0 WITH HEADER LINE.
*  TYPES:
*    BEGIN OF ty_ckmlcr,
*      kalnr TYPE ckmlcr-kalnr,
*      peinh TYPE ckmlcr-peinh,
*      stprs TYPE ckmlcr-stprs,
*    END OF ty_ckmlcr.
*
*  DATA lt_ckmlcr2 TYPE HASHED TABLE OF ty_ckmlcr WITH HEADER LINE
*                  WITH UNIQUE KEY kalnr.

*  REFRESH: lt_ckmlcr, lt_ckmlcr2.

*Current period prices
  l_lfgja = p_bdatj.
  l_lfmon = p_perab.

**  LOOP AT it_mat.
**    l_idx = sy-tabix.
**    IF it_mat-lfgja = l_lfgja AND it_mat-lfmon = l_lfmon.
**    ELSE.
**      CLEAR : it_mbewh, it_mbewh[].
**      SELECT matnr bwkey bwtar peinh stprs verpr bklas lfgja lfmon
**          INTO CORRESPONDING FIELDS OF TABLE it_mbewh
**          FROM mbewh
**              WHERE matnr =  it_mat-matnr
**                AND bwkey =  it_mat-bwkey
**                AND bwtar =  it_mat-bwtar
**                AND lfgja <= l_lfgja.
**      SORT it_mbewh BY lfgja DESCENDING
**                       lfmon DESCENDING.
**
**      READ TABLE it_mbewh INDEX 1.
**
**      it_mat-peinh = it_mbewh-peinh.
**      it_mat-stprs = it_mbewh-stprs.
**      it_mat-verpr = it_mbewh-verpr.
**      it_mat-bklas = it_mbewh-bklas.
**      it_mat-lfgja = it_mbewh-lfgja.
**      it_mat-lfmon = it_mbewh-lfmon.
**    ENDIF.
**    READ TABLE i_t030 WITH KEY bklas = it_mat-bklas.
**    it_mat-kstar = i_t030-konts.
**
**    MODIFY it_mat INDEX l_idx.
**  ENDLOOP.

*GET ACT.UNIT PRICE (digit;5)
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ckmlcr
*    FROM ckmlpp INNER JOIN ckmlcr
*                   ON ckmlpp~kalnr  = ckmlcr~kalnr
*                  AND ckmlpp~bdatj  = ckmlcr~bdatj
*                  AND ckmlpp~poper  = ckmlcr~poper
*                  AND ckmlpp~untper = ckmlcr~untper
**   CLIENT specified BYPASSING BUFFER
**    FOR ALL entries IN it_mat
*    WHERE
**      ckmlpp~mandt  = sy-mandt AND
**          ckmlpp~kalnr  = it_mat-kaln1
*          ckmlpp~bdatj  = l_lfgja
*      AND ckmlpp~poper  = l_lfmon
*      AND ckmlpp~untper = space
*      AND ckmlcr~curtp  = '10'
*  %_HINTS ORACLE 'INDEX("&TABLE&" "CKMLPP~Z01")'.

*  SORT lt_ckmlcr BY bdatj poper.

*PREV PERIOD
  CALL FUNCTION 'CKML_F_GET_PREVIOUS_PERIOD'
    EXPORTING
      input_period    = l_lfmon
      input_year      = l_lfgja
      input_periv     = tka01-lmona
    IMPORTING
      previous_period = l_lfmon2
      previous_year   = l_lfgja2.

*Material Ledger: Period Totals Records Values
* Replaced by Han Moon
*  SELECT r~kalnr r~peinh r~stprs
*    INTO CORRESPONDING FIELDS OF TABLE lt_ckmlcr2
*    FROM ckmlcr AS r
**     FOR ALL ENTRIES IN it_mat
*   WHERE "r~kalnr  = it_mat-kaln1
*         r~bdatj  = l_lfgja2
*     AND r~poper  = l_lfmon2
*     AND r~untper = space
*     AND r~curtp  = '10'.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ckmlcr2
*    FROM ckmlpp INNER JOIN ckmlcr
*                   ON ckmlpp~kalnr  = ckmlcr~kalnr
*                  AND ckmlpp~bdatj  = ckmlcr~bdatj
*                  AND ckmlpp~poper  = ckmlcr~poper
*                  AND ckmlpp~untper = ckmlcr~untper
**    FOR ALL entries IN it_mat
*    WHERE "ckmlpp~kalnr  = it_mat-kaln1
*          ckmlpp~bdatj  = l_lfgja
*      AND ckmlpp~poper  = l_lfmon
*      AND ckmlpp~untper = space
*      AND ckmlcr~curtp  = '10'
*  %_HINTS ORACLE 'INDEX("&TABLE&" "CKMLPP~Z01")'.

*  SORT lt_ckmlcr2 BY kalnr.
  SORT lt_ckmlcr  BY kalnr.


*-get MIP information
*  DATA: BEGIN OF lt_marc OCCURS 0,
*          matnr LIKE marc-matnr,
*          fevor LIKE marc-fevor,
*        END OF lt_marc.

*  SELECT matnr fevor INTO TABLE lt_marc
*     FROM marc
*     FOR ALL ENTRIES IN it_mat
*     WHERE matnr = it_mat-matnr
*       AND werks = 'KVA1'.
**       AND FEVOR <> SPACE.
*  DELETE lt_marc WHERE fevor = space.
*
*  SORT lt_marc BY matnr.

  SORT i_t030  BY bklas.

  DATA: lw_qty LIKE ckmlpp-abkumo.


*-update material info
  LOOP AT it_mat.
    l_idx = sy-tabix.

    CLEAR : lt_ckmlcr, lw_qty, it_mat-verpr.

    REFRESH: lt_kalnr, lt_ckmlpp, lt_ckmlcr.

    CLEAR lt_kalnr.
    lt_kalnr-kalnr = it_mat-kaln1.
    lt_kalnr-bwkey = it_mat-werks.
    APPEND lt_kalnr.

    CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
      EXPORTING
*       I_REFRESH_BUFFER          =
*       I_READ_ONLY_BUFFER        = ' '
*       I_USE_BUFFER              = 'X'
*       I_BUILD_SMBEW             =
        i_bdatj_1                 = l_lfgja
        i_poper_1                 = l_lfmon
        i_bdatj_2                 = l_lfgja2
        i_poper_2                 = l_lfmon2
*       I_BDATJ_3                 =
*       I_POPER_3                 =
*       I_BETWEEN_1_AND_2         =
*       i_untper                  = lh_untper
*       I_CALL_BY_REPORTING       = ' '
*       I_NO_CHK_PERIODS_COMPLETE = ' '
      TABLES
        t_kalnr                   = lt_kalnr
        t_ckmlpp                  = lt_ckmlpp
        t_ckmlcr                  = lt_ckmlcr
*       T_MISS_CKMLPP             =
*       T_MISS_CKMLCR             =
      EXCEPTIONS
        no_data_found             = 1
        input_data_inconsistent   = 2
        buffer_inconsistent       = 3
        OTHERS                    = 4.

    READ TABLE lt_ckmlcr WITH KEY bdatj = l_lfgja
                                  poper = l_lfmon
                                  kalnr = it_mat-kaln1
                                  curtp = '10'
                         BINARY SEARCH.
    READ TABLE lt_ckmlpp WITH KEY bdatj = l_lfgja
                                  poper = l_lfmon
                                  kalnr = it_mat-kaln1
                         BINARY SEARCH.
    IF sy-subrc = 0.
      it_mat-peinh = lt_ckmlcr-peinh.
      it_mat-stprs = lt_ckmlcr-stprs.
      it_mat-verpr = lt_ckmlcr-pvprs.
*    it_mat-bklas = it_mbewh-bklas.

      lw_qty = lt_ckmlpp-abkumo + lt_ckmlpp-zukumo.

      IF lw_qty > 0.
        it_mat-verpr = lt_ckmlcr-stprs +
                     ( lt_ckmlcr-abprd_o + lt_ckmlcr-abkdm_o
                     + lt_ckmlcr-zuprd_o + lt_ckmlcr-zukdm_o )
                     / lw_qty.
      ENDIF.
    ENDIF.


    it_mat-lfgja = l_lfgja.
    it_mat-lfmon = l_lfmon.

    CLEAR i_t030.
    READ TABLE i_t030 WITH KEY bklas = it_mat-bklas
                               BINARY SEARCH.
    it_mat-kstar = i_t030-konts.

*    IF lv_sap_err = 1.   "SAP ERROR SITUATION (OLD)
*      READ TABLE lt_ckmlcr2 WITH KEY kalnr = it_mat-kaln1 BINARY SEARCH.
*      IF sy-subrc = 0.
*        it_mat-vmpec = lt_ckmlcr2-peinh.
*        it_mat-vmstc = lt_ckmlcr2-stprs.
*      ENDIF.
*
*      it_mat-vmpep = it_mat-peinh.
*      it_mat-vmstp = it_mat-stprs.
*    ELSE.
    it_mat-vmpec = it_mat-peinh.
    it_mat-vmstc = it_mat-stprs.

    READ TABLE lt_ckmlcr WITH KEY bdatj = l_lfgja2
                                  poper = l_lfmon2
                                  kalnr = it_mat-kaln1
                                  curtp = '10'
                         BINARY SEARCH.
*    CLEAR lt_ckmlcr2.
*    READ TABLE lt_ckmlcr2 WITH TABLE KEY kalnr = it_mat-kaln1.
    IF sy-subrc = 0.
      it_mat-vmpep = lt_ckmlcr-peinh.
      it_mat-vmstp = lt_ckmlcr-stprs.
    ENDIF.
*    ENDIF.


*Price digit; 5
*    if it_mat-kaln1 = '000100057110'.
*      break-point.
*    endif.

**check MIP material
*    READ TABLE lt_marc WITH KEY matnr = it_mat-matnr
*                                BINARY SEARCH.
*    IF sy-subrc = 0.
*      it_mat-fevor = lt_marc-fevor.
*    ELSE.
*      CLEAR it_mat-fevor.
*    ENDIF.

*    MODIFY it_mat INDEX l_idx.
    MODIFY it_mat TRANSPORTING peinh stprs verpr
                               lfgja lfmon kstar
                               vmpec vmstc vmpep vmstp
     WHERE matnr = it_mat-matnr
       AND werks = it_mat-werks.
  ENDLOOP.

ENDFORM.                    " get_material_prices
*&---------------------------------------------------------------------*
*FORM get_material_prices2.
*  DATA:
*        l_lfgja  LIKE ckmlpp-bdatj,
*        l_lfmon  LIKE ckmlpp-poper,
*        l_lfgja2 LIKE ckmlpp-bdatj,
*        l_lfmon2 LIKE ckmlpp-poper,
*        l_idx    LIKE sy-tabix.
*
*  DATA: lt_ckmlcr  TYPE tt_ckmlcr OCCURS 0 WITH HEADER LINE.
*  DATA: lt_ckmlcr2 TYPE tt_ckmlcr OCCURS 0 WITH HEADER LINE.
*
*  REFRESH: lt_ckmlcr, lt_ckmlcr2.
*
**Current period prices
*  l_lfgja = p_bdatj.
*  l_lfmon = p_perab.
*
**//Modify..04/07/2011..T00020..
*  CALL FUNCTION 'CKML_F_GET_PREVIOUS_PERIOD'
*    EXPORTING
*      input_period    = l_lfmon
*      input_year      = l_lfgja
*      input_periv     = tka01-lmona
*    IMPORTING
*      previous_period = l_lfmon2
*      previous_year   = l_lfgja2.
**//
*
**  LOOP AT it_mat.
**    l_idx = sy-tabix.
**    IF it_mat-lfgja = l_lfgja AND it_mat-lfmon = l_lfmon.
**    ELSE.
**      CLEAR : it_mbewh, it_mbewh[].
**      SELECT matnr bwkey bwtar peinh stprs verpr bklas lfgja lfmon
**          INTO CORRESPONDING FIELDS OF TABLE it_mbewh
**          FROM mbewh
**              WHERE matnr =  it_mat-matnr
**                AND bwkey =  it_mat-bwkey
**                AND bwtar =  it_mat-bwtar
**                AND lfgja <= l_lfgja.
**      SORT it_mbewh BY lfgja DESCENDING
**                       lfmon DESCENDING.
**
**      READ TABLE it_mbewh INDEX 1.
**
**      it_mat-peinh = it_mbewh-peinh.
**      it_mat-stprs = it_mbewh-stprs.
**      it_mat-verpr = it_mbewh-verpr.
**      it_mat-bklas = it_mbewh-bklas.
**      it_mat-lfgja = it_mbewh-lfgja.
**      it_mat-lfmon = it_mbewh-lfmon.
**    ENDIF.
**    READ TABLE i_t030 WITH KEY bklas = it_mat-bklas.
**    it_mat-kstar = i_t030-konts.
**
**    MODIFY it_mat INDEX l_idx.
**  ENDLOOP.
*
**GET ACT.UNIT PRICE (digit;5)
**NO GOOD SQL since 4 records... yyyy/mm
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ckmlcr
*    FROM ckmlcr INNER JOIN ckmlpp
*                   ON ckmlcr~kalnr  = ckmlpp~kalnr
*                  AND ckmlcr~bdatj  = ckmlpp~bdatj
*                  AND ckmlcr~poper  = ckmlpp~poper
*                  AND ckmlcr~untper = ckmlpp~untper
*    CLIENT specified BYPASSING BUFFER
*    FOR ALL ENTRIES IN it_mat
*    WHERE ckmlcr~mandt  = sy-mandt
*      AND ckmlcr~kalnr  = it_mat-kaln1
**//Modify..04/07/2011..T00020..
*      AND ckmlcr~bdatj  IN (l_lfgja, l_lfgja2)
*      AND ckmlcr~poper  IN (l_lfmon, l_lfmon2)
**      AND CKMLCR~BDATJ  = L_LFGJA
**      AND CKMLCR~POPER  = L_LFMON
**//
*      AND ckmlcr~untper = space
*      AND ckmlcr~curtp  = '10'.
*
*
*  SORT lt_ckmlcr BY bdatj poper.
*
***WIP previous
**
**//Modify..04/07/2011..T00020..
*  DATA l_tabix TYPE sy-tabix.
*  LOOP AT lt_ckmlcr WHERE bdatj = l_lfgja2 AND
*                          poper = l_lfmon2.
*    l_tabix = sy-tabix.
*    APPEND lt_ckmlcr TO lt_ckmlcr2.
*    DELETE lt_ckmlcr INDEX l_tabix.
*    CLEAR  lt_ckmlcr.
*  ENDLOOP.
**
**  IF GV_SAP_ERR = 1.
***    l_lfmon = p_perab + 1.
***    l_lfgja = p_bdatj.
***    if l_lfmon > 12.
***      l_lfmon = 1.
***      l_lfgja = p_bdatj + 1.
***    endif.
**  ELSE.
**    CALL FUNCTION 'CKML_F_GET_PREVIOUS_PERIOD'
**      EXPORTING
**        INPUT_PERIOD    = L_LFMON
**        INPUT_YEAR      = L_LFGJA
**        INPUT_PERIV     = TKA01-LMONA
**      IMPORTING
**        PREVIOUS_PERIOD = L_LFMON
**        PREVIOUS_YEAR   = L_LFGJA.
**  ENDIF.
**
***Material Ledger: Period Totals Records Values
**  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_CKMLCR2
**    FROM CKMLCR INNER JOIN CKMLPP
**                   ON CKMLCR~KALNR  = CKMLPP~KALNR
**                  AND CKMLCR~BDATJ  = CKMLPP~BDATJ
**                  AND CKMLCR~POPER  = CKMLPP~POPER
**                  AND CKMLCR~UNTPER = CKMLPP~UNTPER
**    CLIENT SPECIFIED BYPASSING BUFFER
**    FOR ALL ENTRIES IN IT_MAT
**    WHERE CKMLCR~MANDT  = SY-MANDT
**      AND CKMLCR~KALNR  = IT_MAT-KALN1
**      AND CKMLCR~BDATJ  = L_LFGJA
**      AND CKMLCR~POPER  = L_LFMON
**      AND CKMLCR~UNTPER = SPACE
**      AND CKMLCR~CURTP  = '10'.
**//
*
*  SORT lt_ckmlcr2 BY kalnr.
*  SORT lt_ckmlcr  BY kalnr.
*
*
**-get MIP information
*  DATA: BEGIN OF lt_marc OCCURS 0,
*          matnr LIKE marc-matnr,
*          fevor LIKE marc-fevor,
*        END OF lt_marc.
*
*  SELECT matnr fevor INTO TABLE lt_marc
*     FROM marc
*     FOR ALL ENTRIES IN it_mat
*     WHERE matnr = it_mat-matnr
*       AND werks = 'KVA1'.
**       AND FEVOR <> SPACE.
*  DELETE lt_marc WHERE fevor = space.
*
*  SORT lt_marc BY matnr.
*
*
**-update material info
*  DATA: lw_ckmlcr  TYPE tt_ckmlcr,
*        lw_ckmlcr2 TYPE tt_ckmlcr,
*        lw_mat     TYPE tt_mat.
*
*  LOOP AT it_mat INTO lw_mat.
*    l_idx = sy-tabix.
*
*    CLEAR: lw_ckmlcr, lw_ckmlcr2.
*    READ TABLE lt_ckmlcr  WITH KEY kalnr = lw_mat-kaln1 BINARY SEARCH
*         INTO  lw_ckmlcr.
*    READ TABLE lt_ckmlcr2 WITH KEY kalnr = lw_mat-kaln1 BINARY SEARCH
*         INTO  lw_ckmlcr2.
*
*    PERFORM update_it_mat  USING lw_ckmlcr lw_ckmlcr2
*                        CHANGING lw_mat.
*
*
**check MIP material
*    READ TABLE lt_marc WITH KEY matnr = lw_mat-matnr BINARY SEARCH.
*
*    IF sy-subrc = 0.
*      lw_mat-fevor = lt_marc-fevor.
*    ELSE.
*      CLEAR lw_mat-fevor.
*    ENDIF.
*
*    MODIFY it_mat INDEX l_idx FROM lw_mat.
*    CLEAR  lw_mat.
*
*  ENDLOOP.
*
*ENDFORM.                    " get_material_prices2
*&---------------------------------------------------------------------*
*&      Form  select_wip_materials
*&---------------------------------------------------------------------*
FORM select_wip_materials.
*  ranges lr_matnr for marc-matnr.
  RANGES lr_kalnr FOR macku-kalnr.

  REFRESH lr_kalnr.
  lr_kalnr-option = 'EQ'.  lr_kalnr-sign = 'I'.

*  SORT it_mat BY kaln1.

* Transfer Structure for APO Reporting Point Quantities..
  LOOP AT gs_quantities WHERE kaln1 <> space.
    LOOP AT it_mat WHERE kaln1 = gs_quantities-kaln1.
      EXIT.
    ENDLOOP.
*    READ TABLE it_mat WITH KEY kaln1 = gs_quantities-kaln1
*                               BINARY SEARCH.
    IF sy-subrc <> 0.
      lr_kalnr-low = gs_quantities-kaln1. APPEND lr_kalnr.
    ENDIF.

  ENDLOOP.


  CHECK NOT lr_kalnr[] IS INITIAL.

* by ig.moon 3/11/2009 {

*    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_mat
*             FROM ( mbew  INNER JOIN marc
*                             ON  mbew~matnr = marc~matnr
*                             AND mbew~bwkey = marc~werks
*                          INNER JOIN mara
*                             ON mara~matnr   = marc~matnr )
*             WHERE mbew~kaln1 IN lr_kalnr.

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_mat
           FROM ( mbew
                        INNER JOIN marc
             ON  mbew~matnr = marc~matnr
            AND  mbew~bwkey = marc~werks

                        INNER JOIN mara
             ON mara~matnr  = marc~matnr )

           FOR ALL ENTRIES IN lr_kalnr
           WHERE mbew~mandt EQ sy-mandt
           AND   mbew~kaln1 EQ lr_kalnr-low
            %_HINTS ORACLE 'index("mbew" "mbew~ML1")'.
* }

ENDFORM.                    " select_wip_materials
*&---------------------------------------------------------------------*
*&      Form  get_prev_multi
*&---------------------------------------------------------------------*
FORM get_prev_multi.

  REFRESH it_prevshop.
  LOOP AT it_fsc_mat.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_prevshop
      FROM ztco_shop_sum_1
      WHERE kokrs = p_kokrs
        AND bdatj = l_prev_year
        AND poper = l_prev_perd
        AND objnr = it_fsc_mat-objnr
        AND multi_amt <> 0.
  ENDLOOP.

ENDFORM.                    " get_prev_multi
*&---------------------------------------------------------------------*
*&      Form  move_pre_ml_data
*&---------------------------------------------------------------------*
FORM move_pre_ml_data.
  EXIT.

  LOOP AT it_prevshop.
    CLEAR it_shop_sum.
    it_shop_sum-objnr     = it_prevshop-objnr.
    it_shop_sum-typps     = it_prevshop-typps.
    it_shop_sum-kstar     = it_prevshop-kstar.
    it_shop_sum-resou     = it_prevshop-resou.

    it_shop_sum-bwkey     = it_prevshop-bwkey.
    it_shop_sum-llv_matnr = it_prevshop-llv_matnr.
    it_shop_sum-kostl     = it_prevshop-kostl.
    it_shop_sum-lstar     = it_prevshop-lstar.
    it_shop_sum-meeht     = it_prevshop-meeht.

    it_shop_sum-ml_pd_amt =  it_prevshop-multi_amt.

    IF it_shop_sum-resou = p_resou AND p_debug = 'X'.
      BREAK-POINT.
    ENDIF.

    COLLECT it_shop_sum. CLEAR it_shop_sum.
  ENDLOOP.
ENDFORM.                    " move_pre_ml_data
*&---------------------------------------------------------------------*
*&      Form  read_confirmation
*&---------------------------------------------------------------------*
FORM read_confirmation.

  REFRESH it_mto_scrap.

  SORT it_fsc_mat BY sauft.
  READ TABLE it_fsc_mat WITH KEY sauft = space
                                 BINARY SEARCH.
  CHECK sy-subrc = 0.

  LOOP AT it_fsc_mat FROM sy-tabix.
    IF it_fsc_mat-sauft <> space.
      EXIT.
    ENDIF.

    CLEAR it_mto_scrap.

*GET SCRAP
    SELECT AVG( afru~smeng )
           SUM( afru~lmnga ) SUM( afru~xmnga ) SUM( afru~rmnga )
      INTO (it_mto_scrap-smeng,   it_mto_scrap-lmnga,
            it_mto_scrap-xmnga,   it_mto_scrap-rmnga)

      FROM ( afru INNER JOIN ckmlmv013
        ON afru~aufnr = ckmlmv013~aufnr )

      WHERE ckmlmv013~autyp  = '10'
        AND ckmlmv013~pkosa  = it_fsc_mat-aufnr
        AND afru~budat BETWEEN g_frdat AND g_todat

      GROUP BY afru~aufnr.

      it_mto_scrap-objnr = it_fsc_mat-objnr.
      it_mto_scrap-aufnr = it_fsc_mat-aufnr.

      COLLECT it_mto_scrap.
      CLEAR   it_mto_scrap.

    ENDSELECT.

  ENDLOOP.

*Order header data PP orders; AFKO (rework qty)

*REWORK ->
* update to MCKALKW table; Versions: Cost Itemization



ENDFORM.                    " read_confirmation
*&---------------------------------------------------------------------*
*&      Form  get_covp_summary
*&---------------------------------------------------------------------*
FORM get_covp_summary.

  IF p_sql = 'X'.

    SELECT
*        belnr buzei
           objnr kstar werks matnr
           parob hrkft vrgng beknz meinb
           SUM( wkgbtr ) SUM( mbgbtr )
           sgtxt
*        awtyp  refbn  aworg
      APPENDING TABLE it_covp
      FROM covp
      WHERE
*        lednr = '00'
            objnr = it_fsc_mat-objnr
        AND gjahr = p_bdatj
        AND perio = p_perab
        AND wrttp = c_gp_wrttp
        AND versn = p_versn
*        AND beknz IN r_beknz
*        AND kstar IN r_kstar
        AND ( awtyp <> 'AFRU' )  "vrgng <> 'RKL' )  "Reference Transaction
      GROUP BY
*            belnr buzei
               objnr kstar werks matnr
               parob hrkft vrgng beknz meinb sgtxt.
*            awtyp  refbn  aworg  .

    SELECT
*        belnr buzei
           objnr kstar werks matnr
           parob hrkft vrgng beknz meinb
           SUM( wkgbtr ) SUM( mbgbtr )
           sgtxt
           awtyp  refbn  aworg
      APPENDING TABLE it_covp
      FROM covp
      WHERE
*        lednr = '00'
        objnr = it_fsc_mat-objnr
        AND gjahr = p_bdatj
        AND perio = p_perab
        AND wrttp = c_gp_wrttp
        AND versn = p_versn
*        AND beknz IN r_beknz
*        AND kstar IN r_kstar
        AND ( awtyp = 'AFRU' ) " AND vrgng = 'RKL' )
      GROUP BY
*            belnr buzei
               objnr kstar werks matnr
               parob hrkft vrgng beknz meinb sgtxt
               awtyp  refbn  aworg.

  ELSE.
    SELECT
*            belnr buzei
           objnr kstar werks matnr
           parob hrkft vrgng beknz meinb
           SUM( wkgbtr ) SUM( mbgbtr )
           sgtxt
           awtyp  refbn  aworg
      APPENDING TABLE it_covp
      FROM covp
*   FOR ALL ENTRIES IN it_fsc_mat
      WHERE
*        lednr = '00'
        objnr = it_fsc_mat-objnr
        AND gjahr = p_bdatj
        AND perio = p_perab
        AND wrttp = c_gp_wrttp
        AND versn = p_versn
        AND beknz IN r_beknz
*       AND stokz = space    "no reverse
*       AND stflg = space    "no reverse
        AND kstar IN r_kstar
      GROUP BY
*                BELNR buzei
               objnr kstar werks matnr
               parob hrkft vrgng beknz meinb sgtxt
               awtyp  refbn  aworg.
  ENDIF.

ENDFORM.                    " get_covp_summary
*&---------------------------------------------------------------------*
*&      Form  read_wips_di
*&---------------------------------------------------------------------*
FORM read_wips_di.

  CONSTANTS: con_x         TYPE c        VALUE 'X',
             con_obj_or(2) TYPE c        VALUE 'OR',
             con_obj_vs(2) TYPE c        VALUE 'VS',
*.. Add by sgcho 12/17/2013 Requestor by Hong, Youngki
             con_obj_mk(2) TYPE c        VALUE 'MK',
*.. End Add
             con_key_var   TYPE count_zp VALUE '00000000',  "DI46C2
             con_key_fix   TYPE count_zp VALUE '00000001'.  "DI46C2

  DATA: if_select_wip      TYPE c        VALUE 'X',
        if_select_scrap    TYPE c        VALUE 'X',
        if_prefetch_mat    TYPE c        VALUE 'X',

        lf_gjper           TYPE cpzp-gjper,
        lf_gjper_read      TYPE cpzp-gjper,
        lt_cpzp            LIKE cpzp OCCURS 0 WITH HEADER LINE,
        lt_kaln1           TYPE ckml_t_inkalnr,
        l_write_quantities TYPE c,

        lt_objnr_list      TYPE qrp_t_objnr_list,
        ls_objnr_list      TYPE qrp_objnr_list,
*       lt_quantities      type qrp_t_quantities,
        lt_quantities      TYPE z_qrp_t_quantities,
        wa_l_quantities    TYPE qrp_quantities,
        it_l_output        TYPE qrp_t_wip_scrap.


  REFRESH: lt_objnr_list, lt_quantities.

*     CALL FUNCTION 'QRP_APO_PKOSA_AUFNR_TO_OBJNR'
  ls_objnr_list-objnr = it_fsc_mat-objnr.
  ls_objnr_list-plant = it_fsc_mat-werks.
  APPEND ls_objnr_list TO lt_objnr_list.


* refer: QRP_APO_REPORTINGPOINT_READ

* lzgco_generalf01:
  lf_gjper = ( 1000 * p_bdatj ) + p_perab.


  LOOP AT lt_objnr_list INTO ls_objnr_list.

* read table CPZP for PCC
    CALL FUNCTION 'QRP_APO_CPZP_READ'
      EXPORTING
        if_objnr = ls_objnr_list-objnr
        if_gjper = lf_gjper
        if_werks = ls_objnr_list-plant
      IMPORTING
        ef_gjper = lf_gjper_read
      TABLES
        et_cpzp  = lt_cpzp
      EXCEPTIONS
        OTHERS   = 1.

    IF sy-subrc IS INITIAL.

      IF lf_gjper_read LE lf_gjper.

* if component data are in future period, they are ignored
        LOOP AT lt_cpzp.

          CLEAR gs_quantities.                              "DI46C2
*.. Change by sgcho 12/17/2013 Requestor by Hong, Youngki
*          IF lt_cpzp-f_objnr(2) EQ con_obj_vs.              "DI46C2
          IF lt_cpzp-f_objnr(2) EQ con_obj_mk.              "DI46C2
*.. End
*------------------------------- Component -----------------------------
* decode component's object key
            CALL FUNCTION 'QRP_APO_COMP_OBJNR_DECODE'
              EXPORTING
                if_f_objnr = lt_cpzp-f_objnr
              IMPORTING
                ef_kaln1   = gs_quantities-kaln1
                ef_kzbws   = gs_quantities-kzbws
                ef_sobkz   = gs_quantities-sobkz
              EXCEPTIONS
                OTHERS     = 1.

            IF sy-subrc IS INITIAL.
              l_write_quantities = con_x.
              APPEND gs_quantities-kaln1 TO lt_kaln1.
            ENDIF.

          ELSE.                                             "DI46C2
*-------------------------------- Activity -----------------------------
            l_write_quantities = con_x.                     "DI46C2
            gs_quantities-act_objnr = lt_cpzp-f_objnr.      "DI46C2

          ENDIF.                                            "DI46C2


* need all information for APO in/out
*          if not l_write_quantities is initial.             "DI46C2
          gs_quantities-objnr = lt_cpzp-objnr.
          gs_quantities-unit  = lt_cpzp-meinh.               "ANDY

          IF lf_gjper_read EQ lf_gjper.
*---------- The following quantities are only relevant in current period
            gs_quantities-prev_wip     = lt_cpzp-istmn - lt_cpzp-gmper.
            gs_quantities-wip_quantity = lt_cpzp-istmn - lt_cpzp-gmsum.
            gs_quantities-curr_wip     = lt_cpzp-istmn - lt_cpzp-gmsum.
            gs_quantities-actual_scrap    = lt_cpzp-xmper.
            gs_quantities-planned_scrap   = lt_cpzp-xmsum.
            gs_quantities-variance_qty    = lt_cpzp-varmn.
            gs_quantities-actual_qty_stpc = lt_cpzp-gmper.
            gs_quantities-target_qty = lt_cpzp-gmsum - lt_cpzp-xmper.
            gs_quantities-apo_input_qty   = lt_cpzp-gmper.
            gs_quantities-apo_output_qty  = lt_cpzp-gmsum.

            IF lt_cpzp-zaehl EQ con_key_fix.
              gs_quantities-flg_lsi = con_x.
            ELSE.
              CLEAR gs_quantities-flg_lsi.
            ENDIF.


          ELSE.
*------------ no wip change...
            gs_quantities-prev_wip     = lt_cpzp-istmn - lt_cpzp-gmsum.
            gs_quantities-wip_quantity = gs_quantities-prev_wip.
            gs_quantities-curr_wip     = gs_quantities-prev_wip.

          ENDIF.

*ANDY---need all information...for APO in/out
*            if gs_quantities-prev_wip = 0
*            and gs_quantities-wip_quantity = 0
*            and gs_quantities-variance_qty = 0. "
*              continue.
*            endif.
*
*            gs_quantities-unit = lt_cpzp-meinh.
*            if ( not if_select_wip is initial
*             and not gs_quantities-wip_quantity is initial )
*            or
*            ( not if_select_scrap is initial
*              and ( not gs_quantities-actual_scrap is initial
*                   or  not gs_quantities-planned_scrap is initial
*                   or  not gs_quantities-variance_qty is initial
*                   or  not gs_quantities-actual_qty_stpc is initial
*                   or  not gs_quantities-target_qty is initial )
*            ).
*              append gs_quantities to gs_quantities.
*            endif.
          APPEND gs_quantities TO gs_quantities.
          CLEAR gs_quantities.

*          endif.

        ENDLOOP.


        IF NOT if_prefetch_mat IS INITIAL.
* prefetch on material tables for valuation
          CALL METHOD cl_wrap_material_ck=>prefetch_with_kaln1
            EXPORTING
              costing_numbers = lt_kaln1.
        ENDIF.

      ENDIF.

    ENDIF.


  ENDLOOP.

*  call function 'Z_CO_GET_WIP_DETAILS'
*        exporting
*               if_period       = p_perab
*               if_gjahr        = p_bdatj
*               it_objnr_list   = lt_objnr_list
*               if_select_wip   = wip
*               if_select_scrap = scrap
**                  IF_PREFETCH_MAT         = 'X'
*          importing
*                et_quantity_table = lt_quantities
*           exceptions
*                wrong_input       = 1
*                others            = 2 .
*  append lines of lt_quantities to gs_quantities.

ENDFORM.                    " read_wips_di
*&---------------------------------------------------------------------*
*&      Form  read_wips_mto
*&---------------------------------------------------------------------*
FORM read_wips_mto.

*HOW??????
  DATA t_auftlst LIKE kkapara_ta OCCURS 0 WITH HEADER LINE.
  DATA: kv014 LIKE kv014.
  DATA  BEGIN OF t_kv012 OCCURS 1.
          INCLUDE STRUCTURE kv012.
  DATA: END OF t_kv012.
  DATA  BEGIN OF t_kpzp1 OCCURS 1.
          INCLUDE STRUCTURE kpzp1.
  DATA: END OF t_kpzp1.

  DATA: f_wip_wrt TYPE wkgxxx.


  IF it_obj_wip-amount <> 0.

    PERFORM get_kka_wip USING p_kokrs
                              it_fsc_mat-objnr
                              p_bdatj
                              p_perab
                         CHANGING f_wip_wrt.

*  CHECK f_wip_wrt <> 0.
**---- check WIP calculation exist
*    read table it_cosbb with key objnr = it_fsc_mat-objnr
*                                 binary search.
*    check sy-subrc = 0.

*ISSUE
*  KV 159
*  Reporting point/operation 0010: WIP cannot be negative

    CALL FUNCTION 'K_WIP_OBJECT_CALC'
      EXPORTING
        par_kokrs              = p_kokrs
        par_gjahr              = p_bdatj
        par_poper              = p_perab
        par_objnr              = it_fsc_mat-objnr
        par_auflo              = space
        par_plako              = 'X'  "Component explain
        par_knach              = 'X'  "No message
        par_mmess              = ' '  "Add.message
        par_safnr              = ' '  "Order No.
        par_awbva              = '001'  "Eval.Variant
        par_awvsb              = '000'  "version
      IMPORTING
        par_kv014              = kv014
      TABLES
        pta_kv012              = t_kv012
        pta_kpzp1              = t_kpzp1
      EXCEPTIONS
        targetcosts_impossible = 1  "46A
        system_error           = 2  "46A
        OTHERS                 = 3. "46A

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    LOOP AT t_kv012 WHERE awbvk = 'P'.  "Individual Order
      MOVE-CORRESPONDING t_kv012 TO it_wip_sap.
      it_wip_sap-objnr = it_fsc_mat-objnr.

      IF p_debug = 'X' AND it_wip_sap-ckmatnr = p_resou+5(18).
        BREAK-POINT.
      ENDIF.

      COLLECT it_wip_sap.
    ENDLOOP.

  ENDIF.


  IF it_obj_wip-amount2 <> 0.

*FIXME beginning WIP.... ANDY!!!
    PERFORM get_kka_wip USING p_kokrs
                              it_fsc_mat-objnr
                              g_pr_lfgja
                              g_pr_lfmon
                         CHANGING f_wip_wrt.

*  CHECK f_wip_wrt <> 0.

    REFRESH t_kv012.
    CALL FUNCTION 'K_WIP_OBJECT_CALC'
      EXPORTING
        par_kokrs              = p_kokrs
        par_gjahr              = g_pr_lfgja
        par_poper              = g_pr_lfmon
        par_objnr              = it_fsc_mat-objnr
        par_knach              = 'X'  "No message
        par_safnr              = ' '  "Order No.
        par_auflo              = space
        par_plako              = 'X'  "Component explain
        par_awbva              = '001'  "Eval.Variant
        par_awvsb              = '000'  "version
      IMPORTING
        par_kv014              = kv014
      TABLES
        pta_kv012              = t_kv012
        pta_kpzp1              = t_kpzp1
      EXCEPTIONS
        targetcosts_impossible = 1  "46A
        system_error           = 2  "46A
        OTHERS                 = 3. "46A

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT t_kv012 WHERE awbvk = 'P'.  "Individual Order

      MOVE-CORRESPONDING t_kv012 TO it_wip_sap.

      it_wip_sap-objnr = it_fsc_mat-objnr.
      CLEAR: it_wip_sap-megbtr,
             it_wip_sap-wkgbtr.

      it_wip_sap-pegbtr  = t_kv012-megbtr.
      it_wip_sap-pkgbtr  = t_kv012-wkgbtr.

      IF p_debug = 'X' AND it_wip_sap-ckmatnr = p_resou+5(18).
        BREAK-POINT.
      ENDIF.

      COLLECT it_wip_sap.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " read_wips_mto
*&---------------------------------------------------------------------*
*&      Form  WIP_VALUATION_SAP
*&---------------------------------------------------------------------*
FORM wip_valuation_sap.

  DATA: l_p_unitp    TYPE base_price_scale,
        l_p_peinh    LIKE mbew-peinh.

  LOOP AT it_wip_sap.

    CLEAR lt_wipvalue.

    IF p_debug = 'X' AND it_wip_sap-ckmatnr = p_resou+5(18).
      BREAK-POINT.
    ENDIF.

    lt_wipvalue-objnr = it_wip_sap-objnr.
    lt_wipvalue-kstar = it_wip_sap-kstar.
    lt_wipvalue-unit  = it_wip_sap-meinh.

    IF it_wip_sap-ckmatnr = space.

      CALL FUNCTION 'OBJECT_KEY_GET_KL'
        EXPORTING
          objnr  = it_wip_sap-parob
        IMPORTING
          kostl  = lt_wipvalue-kostl
          lstar  = lt_wipvalue-lstar
        EXCEPTIONS
          OTHERS = 1.

*      if gs_quantities-unit <> 'STD'.
* Change unit :  'STD' => Activity master unit
      CLEAR it_csla.
      READ TABLE it_csla WITH KEY lstar = lt_wipvalue-lstar
                                  BINARY SEARCH.

      IF it_csla-leinh <> lt_wipvalue-unit.

        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
          EXPORTING
            input    = it_wip_sap-megbtr
            unit_in  = it_wip_sap-meinh
            unit_out = it_csla-leinh  "Master
          IMPORTING
            output   = it_wip_sap-megbtr.

        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
          EXPORTING
            input    = it_wip_sap-pegbtr
            unit_in  = it_wip_sap-meinh
            unit_out = it_csla-leinh  "Master
          IMPORTING
            output   = it_wip_sap-pegbtr.

        lt_wipvalue-unit = it_csla-leinh.

      ENDIF.

      READ TABLE it_unitpc WITH KEY costcenter = lt_wipvalue-kostl
                                    acttype    = lt_wipvalue-lstar
                                    BINARY SEARCH.

      IF sy-subrc <> 0 AND p_debug = 'X'. BREAK-POINT. ENDIF.

      lt_wipvalue-unitp = it_unitpc-price_ocurr_fix +
                          it_unitpc-price_ocurr_var.
      lt_wipvalue-peinh = it_unitpc-price_ocurr_unit.


    ELSE.
      lt_wipvalue-material = it_wip_sap-ckmatnr.
      lt_wipvalue-bwkey    = it_wip_sap-ckwerks.

      PERFORM check_it_mat USING it_wip_sap-ckmatnr
                                 it_wip_sap-ckwerks
                           CHANGING sy-subrc.
*      READ TABLE it_mat WITH KEY matnr = it_wip_sap-ckmatnr
*                                 bwkey = it_wip_sap-ckwerks
*                                 BINARY SEARCH.
*      IF sy-subrc <> 0.
*        PERFORM read_material_add USING it_wip_sap-ckmatnr
*                                        it_wip_sap-ckwerks
*                                  CHANGING it_mat.
*      ENDIF.

      lt_wipvalue-unitp = it_mat-vmstc.
      lt_wipvalue-peinh = it_mat-vmpec.

    ENDIF.

    lt_wipvalue-wip_quantity = it_wip_sap-megbtr.
    lt_wipvalue-wip_amt      = it_wip_sap-wkgbtr.

    lt_wipvalue-prev_wip     = it_wip_sap-pegbtr.
    lt_wipvalue-wip_pamt     = it_wip_sap-pkgbtr.

*    READ TABLE it_wip_pre WITH KEY objnr = lt_wipvalue-objnr
*                                   kstar = lt_wipvalue-kstar
*                                   matnr = lt_wipvalue-material
*                                   kostl = lt_wipvalue-kostl
*                                   lstar = lt_wipvalue-lstar.
*    IF sy-subrc = 0.
*      lt_wipvalue-prev_wip = it_wip_pre-pegbtr.
*      lt_wipvalue-wip_pamt = it_wip_pre-pkgbtr.
*    ENDIF.

    lt_wipvalue-wip_quantity_f = it_wip_sap-mefbtr.
    lt_wipvalue-wip_f          = it_wip_sap-wkfbtr.

    lt_wipvalue-prev_wip_f     = it_wip_sap-pefbtr.
    lt_wipvalue-wip_pf         = it_wip_sap-pkfbtr.

    APPEND lt_wipvalue.

  ENDLOOP.

ENDFORM.                    " WIP_VALUATION_SAP
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8293   text
*----------------------------------------------------------------------*
FORM making_shop_cc_material.

  CLEAR : ld_wa_db_keko,ld_wa_db_keph.
  CLEAR it_mat.

  PERFORM check_it_mat USING it_shop_sum-llv_matnr
                             it_shop_sum-par_werks
                       CHANGING sy-subrc.
*  READ TABLE it_mat WITH KEY matnr = it_shop_sum-llv_matnr
*                             werks = it_shop_sum-par_werks
*                             BINARY SEARCH.

*  READ TABLE it_mat_tmp WITH KEY matnr = it_shop_sum-llv_matnr
*                             werks = it_shop_sum-par_werks
*                             BINARY SEARCH.

  CHECK sy-subrc = 0 .

  CLEAR it_prkeko.
  READ TABLE it_prkeko WITH TABLE KEY kalnr = it_shop_sum-chd_kalnr
                                      bdatj = p_bdatj
                                      poper = p_perab.
  IF sy-subrc <> 0 .
*FIXME!!! some material has no ccs information!!!
*determine cc using cost element.!
    PERFORM collect_cc_missing.

  ELSE.

    ld_wa_db_price-togbtr = it_mat-stprs.  "total
    ld_wa_db_price-tofbtr = it_mat-stprs.  "fixed
    MOVE-CORRESPONDING it_prkeko TO ld_wa_db_keko .

* read header
    CLEAR it_prkeph.
    READ TABLE it_prkeph WITH TABLE KEY kalnr = it_shop_sum-chd_kalnr
                                        bdatj = p_bdatj
                                        poper = p_perab
                                        kkzst = ''.
    CHECK sy-subrc = 0 .
    MOVE-CORRESPONDING it_prkeph TO ld_wa_db_keph .

*//Modify..04/04/2011..T00020..
    PERFORM extract_cc_current USING it_shop_sum-chd_kalnr 'PRKEPH'.


*    PERFORM extract_cc_amt TABLES lt_cc_amt
*                           USING  ld_wa_db_keph.
*
*    LOOP AT lt_cc_amt.
*      it_shop_cc-elemt        = lt_cc_amt-elemt.
*
*      PERFORM collect_shop_cc USING lt_cc_amt-dmbtr    'A'.
*
*      it_shop_cc-elemt        = lt_cc_amt-elemt.
*      PERFORM collect_shop_cc USING lt_cc_amt-dmbtr_f  'F'.
*    ENDLOOP.
*//

  ENDIF.

*---Previous WIP
*  if it_mat-matnr = 'AU62'. break-point. endif.

* Replaced by Han Moon
*  READ TABLE it_prkeko_p WITH KEY kalnr = it_shop_sum-chd_kalnr
*                              BINARY SEARCH.
  READ TABLE it_prkeko WITH TABLE KEY kalnr = it_shop_sum-chd_kalnr
                                      bdatj = g_pr_lfgja
                                      poper = g_pr_lfmon.
  IF sy-subrc <> 0 .
    PERFORM collect_cc_missing_p.

  ELSE.
    ld_wa_db_price-togbtr = it_mat-vmstp.  "previous STD
*   ld_wa_db_price-tofbtr = it_mat-stprs.  "fixed
*   Replaced by Han Moon
*    MOVE-CORRESPONDING it_prkeko_p TO ld_wa_db_keko .
*    READ TABLE it_prkeph_p WITH KEY kalnr = it_mat-kaln1
*                                    kkzst = ''
*                                BINARY SEARCH.
    MOVE-CORRESPONDING it_prkeko TO ld_wa_db_keko.
    READ TABLE it_prkeph WITH TABLE KEY kalnr = it_mat-kaln1
                                        bdatj = g_pr_lfgja
                                        poper = g_pr_lfmon
                                        kkzst = ''.
    CHECK sy-subrc = 0 .
* Replaced by Han Moon
*    MOVE-CORRESPONDING it_prkeph_p TO ld_wa_db_keph .
    MOVE-CORRESPONDING it_prkeph TO ld_wa_db_keph .

*//Modify..04/04/2011..T00020..
    PERFORM extract_cc_previous USING it_shop_sum-chd_kalnr 'PRKEPH_P'.

*    PERFORM extract_cc_amt TABLES lt_cc_amt
*                           USING  ld_wa_db_keph.
*    LOOP AT lt_cc_amt.
*      it_shop_cc-elemt        = lt_cc_amt-elemt.
*
*      PERFORM collect_shop_cc_p USING lt_cc_amt-dmbtr    'A'.
*
*      it_shop_cc-elemt        = lt_cc_amt-elemt.
*      PERFORM collect_shop_cc_p USING lt_cc_amt-dmbtr_f  'F'.
*    ENDLOOP.
*//

  ENDIF.

*   perform make_element_value.

*      WIP
*  PERFORM making_shop_cc_wip .

ENDFORM.                    " making_shop_cc
*&---------------------------------------------------------------------*
*&      Form  make_element_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PRKEKO  text
*      -->P_IT_PRKEPH  text
*      -->P_IT_SHOP_CC  text
*----------------------------------------------------------------------*
*form make_element_value .
*
*  data : l_cnt(3) type n,
*         l_field(25),
*         l_amt type gt_amt,
*         l_amt_type(1).
*
*  clear : l_cnt, l_field, l_amt, l_amt_type.
*
*  do 40 times.
*    clear : l_amt.
*    l_cnt = l_cnt + 1.
** Overall value
*    clear it_tckh3.
*    read table it_tckh3 with key el_hv = l_cnt.
*    if sy-subrc   =  0 .
*      l_amt_type = 'A'.  "A : ALL
*    else.
** Fixed value
*      clear it_tckh3.
*      read table it_tckh3 with key el_hf = l_cnt.
*      if sy-subrc   = 0 .
*        l_amt_type = 'F'. "F : FIXED
*      endif.
*    endif.
*    check sy-subrc = 0.
*
*    concatenate 'LD_WA_DB_KEPH-KST' l_cnt into l_field.
*    assign  (l_field)    to   <f_field> .
*    clear l_amt.
*    l_amt = <f_field>.
*    check not l_amt is initial.
*
*    it_shop_cc-elemt = it_tckh3-elemt.
*
*    perform collect_shop_cc using l_amt
*                                  l_amt_type .
*
*  enddo.
*
*endform.                    " make_element_value
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_activity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM making_shop_cc_activity .

  DATA: lt_cc_amt LIKE gt_cc_amt OCCURS 0 WITH HEADER LINE.

  CLEAR : ld_wa_db_keko,ld_wa_db_keph.

*  CLEAR it_act.
*  READ TABLE it_act WITH KEY kostl = it_shop_sum-kostl
*                             lstar = it_shop_sum-lstar
*                             BINARY SEARCH.
*  CHECK sy-subrc = 0.

*current
  READ TABLE it_ckmllacr WITH KEY kalnr = it_shop_sum-chd_kalnr
                              BINARY SEARCH.
  ld_wa_db_price-togbtr = it_ckmllacr-togbtr.  "total
  ld_wa_db_price-tofbtr = it_ckmllacr-tofbtr.  "fixed

  CLEAR it_ckmlprkeko.
  READ TABLE it_ckmlprkeko WITH KEY kalnr = it_shop_sum-chd_kalnr
                           BINARY SEARCH.
  CHECK sy-subrc = 0 .
  MOVE-CORRESPONDING it_ckmlprkeko TO ld_wa_db_keko .

  CLEAR it_ckmlprkeph.
  READ TABLE it_ckmlprkeph WITH KEY kalnr = it_shop_sum-chd_kalnr
                                    kkzst = ''
                             BINARY SEARCH.
  CHECK sy-subrc = 0 .
  MOVE-CORRESPONDING it_ckmlprkeph TO ld_wa_db_keph .

*//Modify..04/01/2011..T00020..
  PERFORM extract_cc_current USING it_shop_sum-chd_kalnr 'CKMLPRKEPH'.


*  PERFORM extract_cc_amt TABLES lt_cc_amt
*                         USING  ld_wa_db_keph.
*  LOOP AT lt_cc_amt.
*    it_shop_cc-elemt        = lt_cc_amt-elemt.
*    PERFORM collect_shop_cc USING lt_cc_amt-dmbtr    'A'.
*
*    it_shop_cc-elemt        = lt_cc_amt-elemt.
*    PERFORM collect_shop_cc USING lt_cc_amt-dmbtr_f  'F'.
*  ENDLOOP.
*//

*previous
  READ TABLE it_ckmllacr_p WITH KEY kalnr = it_shop_sum-chd_kalnr
                                BINARY SEARCH.
  ld_wa_db_price-togbtr = it_ckmllacr_p-togbtr.  "total
  ld_wa_db_price-tofbtr = it_ckmllacr_p-tofbtr.  "fixed

  CLEAR it_ckmlprkeko.
  READ TABLE it_ckmlprkeko_p WITH KEY kalnr = it_shop_sum-chd_kalnr
                             BINARY SEARCH.
  CHECK sy-subrc = 0 .
  MOVE-CORRESPONDING it_ckmlprkeko_p TO ld_wa_db_keko .

  CLEAR it_ckmlprkeph.
  READ TABLE it_ckmlprkeph_p WITH KEY kalnr = it_shop_sum-chd_kalnr
                                      kkzst = ''
                             BINARY SEARCH.
  CHECK sy-subrc = 0 .
  MOVE-CORRESPONDING it_ckmlprkeph_p TO ld_wa_db_keph .

*//Modify..04/01/2011..T00020..
  PERFORM extract_cc_previous USING it_shop_sum-chd_kalnr 'CKMLPRKEPH_P'.


*  PERFORM extract_cc_amt TABLES lt_cc_amt
*                         USING  ld_wa_db_keph.
*  LOOP AT lt_cc_amt.
*    it_shop_cc-elemt        = lt_cc_amt-elemt.
*    PERFORM collect_shop_cc_p USING lt_cc_amt-dmbtr    'A'.
*
*    it_shop_cc-elemt        = lt_cc_amt-elemt.
*    PERFORM collect_shop_cc_p USING lt_cc_amt-dmbtr_f  'F'.
*  ENDLOOP.
*//

*  perform make_element_value.
*
*      WIP
*  PERFORM making_shop_cc_wip_act .


ENDFORM.                    " making_shop_cc_activity
*&---------------------------------------------------------------------*
*&      Form  make_element_value_activity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CKMLPRKEKO  text
*      -->P_IT_CKMLPRKEPH  text
*      -->P_IT_SHOP_CC  text
*----------------------------------------------------------------------*
FORM make_element_value_activity USING  p_type
                                        p_qty.

  DATA : l_cnt(3) TYPE n,
         l_field(25),
         l_amt TYPE ckmlprkeph-kst001,
         l_amt_type(1),
         l_elemt LIKE it_tckh3-elemt.

  CLEAR : l_cnt, l_field, l_amt, l_amt_type, l_elemt.


  DO 40 TIMES VARYING l_amt FROM ld_wa_db_keph-kst001
                            NEXT ld_wa_db_keph-kst002.
    l_cnt = l_cnt + 1.
    CHECK NOT l_amt IS INITIAL.

* Overall value
    CLEAR it_elemt.
    READ TABLE it_elemt WITH TABLE KEY elnum = l_cnt.
    IF sy-subrc EQ 0.
      IF it_elemt-eltyp = 'V'.
        l_amt_type = 'A'.  "A : ALL
      ELSE.
        l_amt_type = 'F'. "F : FIXED
      ENDIF.
    ENDIF.

*    READ TABLE it_tckh3 WITH KEY el_hv = l_cnt.
*    IF sy-subrc =  0 .
*      l_amt_type = 'A'.  "A : ALL
*    ELSE.
*      CLEAR it_tckh3.
*      READ TABLE it_tckh3 WITH KEY el_hf = l_cnt.
*      IF sy-subrc = 0 .
*        l_amt_type = 'F'. "F : FIXED
*      ENDIF.
*    ENDIF.

*    CONCATENATE 'LD_WA_DB_KEPH-KST' l_cnt INTO l_field.
*    ASSIGN  (l_field)    TO   <f_field> .
*    CLEAR l_amt.
*    l_amt = <f_field>.
*    CHECK NOT l_amt IS INITIAL.
* Change unit :  'STD' => Activity master unit
    CLEAR it_csla.
    READ TABLE it_csla WITH KEY lstar = it_shop_sum-lstar.
    IF it_csla-leinh <> ld_wa_db_keko-meins.
      PERFORM unit_converion2  USING  l_amt
                                      ld_wa_db_keko-meins
                                      it_csla-leinh
                             CHANGING l_amt.
    ENDIF.

    PERFORM collect_shop_cc USING l_amt
                                  l_amt_type .


  ENDDO.

ENDFORM.                    " make_element_value_activity
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_v
*&---------------------------------------------------------------------*
FORM making_shop_cc_v.

  PERFORM move_sum_header_to_cc.

  CLEAR it_kstar .
  READ TABLE it_kstar WITH TABLE KEY kstar = it_shop_sum-kstar.

  it_shop_cc-add_wkgbtr  = it_shop_sum-add_wkgbtr.
  it_shop_cc-manu_amt    = it_shop_sum-manu_amt.
  it_shop_cc-single_amt  = it_shop_sum-single_amt.
  it_shop_cc-elemt       = it_kstar-elemt.

*//Modify..04/28/2011..T00020..
  it_shop_cc-key_amt     = it_shop_sum-key_amt.
  it_shop_cc-osnd_amt    = it_shop_sum-osnd_amt.
*//

  COLLECT it_shop_cc. CLEAR it_shop_cc.

ENDFORM.                    " making_shop_cc_v.
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_add
*&---------------------------------------------------------------------*
FORM making_shop_cc_add.

  CHECK it_shop_sum-add_wkgbtr <> 0.

  PERFORM move_sum_header_to_cc.

  CLEAR it_kstar .
  READ TABLE it_kstar WITH TABLE KEY kstar = it_shop_sum-kstar.

  it_shop_cc-add_wkgbtr = it_shop_sum-add_wkgbtr.
  it_shop_cc-elemt      = it_kstar-elemt.

*//Modify..04/28/2011..T00020..
  it_shop_cc-key_amt    = it_shop_sum-key_amt.
  it_shop_cc-osnd_amt   = it_shop_sum-osnd_amt.
*//

  COLLECT it_shop_cc. CLEAR it_shop_cc.


ENDFORM.                    " making_shop_cc_cost_add
*&---------------------------------------------------------------------*
*&      Form  MAKE_L_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TYPE  text
*      <--P_L_FIELD_NAME  text
*----------------------------------------------------------------------*
FORM make_l_field USING    p_type
                           p_amt_type
                  CHANGING p_name .
  DATA : l_table(12).

  CLEAR : p_name .

  IF g_std  = 'X'.

    IF p_type = 'B'.
      p_name   = 'FSC_STD_CC-WIP_PAMT'.
    ELSEIF p_type = 'E'.
      p_name   = 'FSC_STD_CC-WIP_AMT'.
    ENDIF.

  ELSE.

    IF p_type = 'C'.
      p_name   = 'IT_SHOP_CC-WKGBTR'.
    ELSEIF p_type = 'G'.
      p_name   = 'IT_SHOP_CC-GR_AMT'.
    ELSEIF p_type = 'V'.
      p_name   = 'IT_SHOP_CC-WKGBTR2'.
    ELSEIF p_type = 'S'.
      p_name   = 'IT_SHOP_CC-SCRAP_AMT'.
    ELSEIF p_type = 'B'.
      p_name   = 'IT_SHOP_CC-WIP_PAMT'.
    ELSEIF p_type = 'E'.
      p_name   = 'IT_SHOP_CC-WIP_AMT'.
    ELSEIF p_type = 'T'.
      p_name   = 'IT_SHOP_CC-TARGET_AMT'.
    ENDIF.

  ENDIF.

  IF p_amt_type = 'F'.
    CONCATENATE p_name '_F' INTO p_name.
  ENDIF.

ENDFORM.                    " MAKE_L_FIELD
*&---------------------------------------------------------------------*
*&      Form  select_mlkeph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_mlkeph.
  DATA : l_cnt(3) TYPE n,
         l_amt    TYPE gt_amt,
         l_field(20).

  FIELD-SYMBOLS: <f_field> .

  CHECK NOT t_mlite[] IS INITIAL.

  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE it_mlkeph
     FROM mlkeph
     FOR ALL ENTRIES IN t_mlite
     WHERE belnr  = t_mlite-belnr
       AND kjahr  = t_mlite-kjahr
       AND posnr  = t_mlite-posnr
      AND  bdatj  = p_bdatj
      AND  poper  = p_perab.

  SORT it_mlkeph BY kjahr belnr posnr.

*  SORT IT_MLKEPH.
*
*  REFRESH GT_MULTI. CLEAR GT_MULTI.
*
*  LOOP AT IT_MLKEPH.
*
*    CLEAR : L_CNT.
*
*    DO 40 TIMES.
*
*      CLEAR : L_AMT.
*      L_CNT = L_CNT + 1.
*
** Overall value
*      CLEAR IT_TCKH3.
*      READ TABLE IT_TCKH3 WITH KEY EL_HV = L_CNT.
*
*      IF SY-SUBRC =  0 .
*        CONCATENATE 'IT_MLKEPH-KST' L_CNT INTO L_FIELD.
*        ASSIGN  (L_FIELD)    TO   <F_FIELD> .
*        CLEAR L_AMT.
*        L_AMT = <F_FIELD>.
*
*      ELSE.
** Fixed value
*        CLEAR IT_TCKH3.
*        READ TABLE IT_TCKH3 WITH KEY EL_HF = L_CNT.
*
*        IF SY-SUBRC = 0 .
*          CONCATENATE 'IT_MLKEPH-KST' L_CNT INTO L_FIELD.
*          ASSIGN  (L_FIELD)    TO   <F_FIELD> .
*          CLEAR L_AMT.
*          L_AMT = <F_FIELD>.
*        ENDIF.
*
*      ENDIF.
*
*      CHECK NOT L_AMT IS INITIAL.
*
*      GT_MULTI-ELEMT = IT_TCKH3-ELEMT.
*      GT_MULTI-AMT_F = L_AMT.
*
*      IF IT_MLKEPH-MLCCT = 'M'.
*        GT_MULTI-MAMT_F = L_AMT.
*      ELSE.
*        GT_MULTI-SAMT_F = L_AMT.
*      ENDIF.
*
*      GT_MULTI-KJAHR = IT_MLKEPH-KJAHR.
*      GT_MULTI-BELNR = IT_MLKEPH-BELNR.
*      GT_MULTI-POSNR = IT_MLKEPH-POSNR.
*
*      COLLECT GT_MULTI . CLEAR GT_MULTI.
*
*    ENDDO.
*
*  ENDLOOP.
*
*  SORT GT_MULTI BY KJAHR BELNR POSNR.

ENDFORM.                    " select_mlkeph
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_ml_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM making_shop_cc_ml_multi.
  DATA : l_cnt(3) TYPE n,
         l_amt TYPE mlkeph-kst001.

  FIELD-SYMBOLS: <f_field> .

  DATA : temp_mlkeph LIKE mlkeph OCCURS 0 WITH HEADER LINE.

*-VC change -> multiple cost element possible...
  CHECK NOT it_shop_sum-multi_qty IS INITIAL.

  CLEAR : temp_mlkeph, temp_mlkeph[].

** Performance tuning by Furong 07/24/2014
*  SORT t_mlite BY paobjnr bwtar bwkey matnr act_objnr.
*  SORT it_mlkeph BY kjahr belnr posnr.
** End tuning

  CASE it_shop_sum-typps.
    WHEN 'M'.
      CLEAR t_mlite.
      READ TABLE t_mlite WITH KEY paobjnr = it_shop_sum-aufnr
                                  bwtar   = it_shop_sum-bwtar
                                  bwkey   = it_shop_sum-bwkey
                                  matnr   = it_shop_sum-llv_matnr
                         BINARY SEARCH.

*    WHEN OTHERS.
    WHEN 'E'.
      CLEAR it_coep.
      READ TABLE it_coep WITH KEY objnr = it_shop_sum-objnr
                                  kstar = it_shop_sum-kstar
                                  kostl = it_shop_sum-kostl
                                  lstar = it_shop_sum-lstar
                         BINARY SEARCH.
      CLEAR t_mlite.
      READ TABLE t_mlite WITH KEY paobjnr   = it_shop_sum-aufnr
                                  bwtar     = space
                                  bwkey     = space
                                  matnr     = space
                                  act_objnr = it_coep-parob
                         BINARY SEARCH.
  ENDCASE.


*  LOOP AT it_mlkeph WHERE kjahr = t_mlite-kjahr
*                      AND belnr = t_mlite-belnr
*                      AND posnr = t_mlite-posnr.
*    MOVE it_mlkeph TO temp_mlkeph.
*    APPEND temp_mlkeph. CLEAR temp_mlkeph.
*  ENDLOOP.
*
*  LOOP AT temp_mlkeph.
  READ TABLE it_mlkeph WITH KEY kjahr = t_mlite-kjahr
                                belnr = t_mlite-belnr
                                posnr = t_mlite-posnr
                       BINARY SEARCH.
  CHECK sy-subrc = 0.

  LOOP AT it_mlkeph FROM sy-tabix.

    IF it_mlkeph-kjahr <> t_mlite-kjahr OR
       it_mlkeph-belnr <> t_mlite-belnr OR
       it_mlkeph-posnr <> t_mlite-posnr.
      EXIT.
    ENDIF.

    CLEAR : l_cnt.

    DO 40 TIMES VARYING l_amt FROM it_mlkeph-kst001
                              NEXT it_mlkeph-kst002.
      l_cnt = l_cnt + 1.
      CHECK NOT l_amt IS INITIAL.
* Overall value
      CLEAR it_elemt.
      READ TABLE it_elemt WITH TABLE KEY elnum = l_cnt.
      IF sy-subrc EQ 0.
        IF it_elemt-eltyp = 'V'.
          it_shop_cc-elemt = it_elemt-elemt.
          IF it_mlkeph-mlcct = 'M'.
            it_shop_cc-multi_mamt = l_amt.
          ELSE.
            it_shop_cc-multi_samt = l_amt.
          ENDIF.
          it_shop_cc-multi_amt = l_amt.
          PERFORM move_sum_header_to_cc.
          COLLECT it_shop_cc . CLEAR it_shop_cc.
        ELSE.
          it_shop_cc-elemt = it_elemt-elemt.
          IF it_mlkeph-mlcct = 'M'.
            it_shop_cc-multi_mamt_f = l_amt.
          ELSE.
            it_shop_cc-multi_samt_f = l_amt.
          ENDIF.
          it_shop_cc-multi_amt_f = l_amt.
          PERFORM move_sum_header_to_cc.
          COLLECT it_shop_cc . CLEAR it_shop_cc.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

**//Modify..04/04/2011..T00020..
*  READ TABLE GT_MULTI WITH KEY
*                          KJAHR = T_MLITE-KJAHR
*                          BELNR = T_MLITE-BELNR
*                          POSNR = T_MLITE-POSNR
*                          BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    LOOP AT GT_MULTI FROM SY-TABIX.
*      IF GT_MULTI-KJAHR <> T_MLITE-KJAHR OR
*         GT_MULTI-BELNR <> T_MLITE-BELNR OR
*         GT_MULTI-POSNR <> T_MLITE-POSNR.
*        EXIT.
*      ENDIF.
*
*      PERFORM MOVE_SUM_HEADER_TO_CC.
*
*      IT_SHOP_CC-ELEMT        = GT_MULTI-ELEMT.
*      IT_SHOP_CC-MULTI_AMT_F  = GT_MULTI-AMT_F.
*      IT_SHOP_CC-MULTI_MAMT_F = GT_MULTI-MAMT_F.
*      IT_SHOP_CC-MULTI_SAMT_F = GT_MULTI-SAMT_F.
*
*      COLLECT IT_SHOP_CC . CLEAR IT_SHOP_CC.
*
*    ENDLOOP.
*  ENDIF.
**//

ENDFORM.                    " making_shop_cc_ml_material
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8597   text
*      -->P_IT_SHOP_SUM_WIP_QTY  text
*----------------------------------------------------------------------*
FORM making_shop_cc_wip .


** WIP Begining
*  IF NOT it_shop_sum-wip_pqty IS INITIAL.
*    PERFORM make_element_value USING    'B'
*                                        it_shop_sum-wip_pqty.
*  ENDIF.
** WIP Ending
*  IF NOT it_shop_sum-wip_qty IS INITIAL.
*    PERFORM make_element_value USING    'E'
*                                        it_shop_sum-wip_qty.
*  ENDIF.
*
*  CLEAR it_fsc_mat.
*  READ TABLE it_fsc_mat WITH KEY objnr = it_shop_sum-objnr.
*
*  CLEAR lt_ck13n.
*  READ TABLE lt_ck13n WITH KEY matnr    = it_fsc_mat-matnr
*                               resou    = it_shop_sum-resou.
*
** Standard ( * Itemize Qty)
*  CHECK NOT lt_ck13n-gr_qty IS INITIAL.
*  g_std = 'X'.
*  PERFORM make_element_value_std USING    'B'
*                                          lt_ck13n-gr_qty.
*
*  PERFORM make_element_value_std USING    'E'
*                                          lt_ck13n-gr_qty.
*
*  CLEAR g_std.


ENDFORM.                    " making_shop_cc_wip
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_wip_Act
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8637   text
*      -->P_IT_SHOP_SUM_WIP_QTY  text
*      -->P_IT_SHOP_SUM_WIP_AMT  text
*----------------------------------------------------------------------*
FORM making_shop_cc_wip_act .


* WIP - begining
  IF NOT it_shop_sum-wip_pqty IS INITIAL.
    PERFORM make_element_value_activity USING    'B'
                                                 it_shop_sum-wip_pqty.
  ENDIF.

* WIP - ending
  IF NOT it_shop_sum-wip_qty IS INITIAL.
    PERFORM make_element_value_activity USING    'E'
                                                 it_shop_sum-wip_qty.
  ENDIF.


  CLEAR it_fsc_mat.
  READ TABLE it_fsc_mat WITH KEY objnr = it_shop_sum-objnr.

  CLEAR lt_ck13n.
  READ TABLE lt_ck13n WITH KEY matnr    = it_fsc_mat-matnr
                               resou    = it_shop_sum-resou.
* Standard ( * Itemize Qty)
  CHECK NOT lt_ck13n-gr_qty IS INITIAL.
  g_std = 'X'.
  PERFORM make_element_value_act_std USING   'B'
                                             lt_ck13n-gr_qty.

  PERFORM make_element_value_act_std USING   'E'
                                             lt_ck13n-gr_qty.

  CLEAR g_std.


ENDFORM.                    " making_shop_cc_wip_Act
*&---------------------------------------------------------------------*
*&      Form  make_element_value_std
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FSC_GR_LOTSIZE  text
*      -->P_LT_CK13N_GR_QTY  text
*----------------------------------------------------------------------*
*FORM make_element_value_std .
*
*
*  DATA : l_cnt(3) TYPE n,
*         l_amt TYPE p DECIMALS 4.
*
*
*  CLEAR : l_cnt.
*  DO 40 TIMES.
*    CLEAR : l_amt.
*    l_cnt = l_cnt + 1.
** Overall value
*    CLEAR it_tckh3.
*    READ TABLE it_tckh3 WITH KEY el_hv = l_cnt.
*    IF sy-subrc =  0 .
*      FSC_STD_CC-elemt = it_tckh3-elemt.
*    else.
*      CLEAR it_tckh3.
*      READ TABLE it_tckh3 WITH KEY el_hf = l_cnt.
*      IF sy-subrc = 0 .
*        FSC_STD_CC-elemt = it_tckh3-elemt.
*
*
*      CONCATENATE 'LD_WA_DB_KEPH-KST' l_cnt INTO l_field.
*      ASSIGN  (l_field)    TO   <f_field> .
*      CLEAR l_amt.
*      l_amt = <f_field>.
*      CHECK NOT l_amt IS INITIAL.
*      PERFORM collect_FSC_STD_CC USING l_amt.
*    ELSE.
** Fixed value
*      CLEAR it_tckh3.
*      READ TABLE it_tckh3 WITH KEY el_hf = l_cnt.
*      IF sy-subrc = 0 .
*        FSC_STD_CC-elemt = it_tckh3-elemt.
*        CONCATENATE 'LD_WA_DB_KEPH-KST' l_cnt INTO l_field.
*        ASSIGN  (l_field)    TO   <f_field> .
*        CLEAR l_amt.
*        l_amt = <f_field>.
*        CHECK NOT l_amt IS INITIAL.
*        ASSIGN (l_field_name_f)  TO   <f_field2> .
*        <f_field2> = l_amt * p_qty / ld_wa_db_keko-losgr.
*        PERFORM move_sum_header_to_cc.
*        COLLECT FSC_STD_CC . CLEAR FSC_STD_CC.
*      ENDIF.
*    ENDIF.
*
*  ENDDO.
*
*
*ENDFORM.                    " make_element_value_std
*&---------------------------------------------------------------------*
*&      Form  SCALE_WIP
*&---------------------------------------------------------------------*
FORM scale_wip.
  DATA : l_scale_p   TYPE gt_amt, "p DECIMALS 5,
         l_scale     TYPE gt_amt. "p DECIMALS 5.

  DATA : l_scale_pf   TYPE gt_amt, "p DECIMALS 5,
         l_scalef     TYPE gt_amt. "p DECIMALS 5.

  SORT it_obj_sum_wip BY objnr.

* UD1K941236 - by IG.MOON 8/8/2007 {
*  sort it_cc_wip_scale by artnr elemt.
  SORT it_cc_wip_scale BY artnr aufnr elemt.
* }
  SORT fsc_std_cc      BY artnr elemt.
  SORT it_fsc_mat      BY objnr.
  SORT fsc_std_ccsum   BY artnr.

*FIXME for prev WIP cc value
*FIXME overall / fixed.
  LOOP AT it_obj_gr.

* UD1K941236 - by IG.MOON 8/8/2007 {
    CLEAR it_fsc_mat.
* }
    READ TABLE it_fsc_mat  WITH KEY objnr = it_obj_gr-objnr
                                    BINARY SEARCH.

    READ TABLE it_obj_sum_wip  WITH KEY objnr = it_obj_gr-objnr
                                    BINARY SEARCH.

    CLEAR : l_scale, l_scale_p.
    READ TABLE fsc_std_ccsum   WITH KEY artnr = it_obj_gr-matnr
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      l_scale_p   = it_obj_sum_wip-wip_pamt / fsc_std_ccsum-dmbtr.
      l_scale     = it_obj_sum_wip-wip_amt  / fsc_std_ccsum-dmbtr.

      l_scale_pf   = it_obj_sum_wip-wip_pf / fsc_std_ccsum-dmbtr_f.
      l_scalef     = it_obj_sum_wip-wip_f  / fsc_std_ccsum-dmbtr_f.
    ENDIF.

    CHECK l_scale_p <> 0 OR l_scale <> 0.

    LOOP AT it_tckh3.
      CLEAR it_cc_wip_scale.

* UD1K941236 - by IG.MOON 8/8/2007 {
*      read table it_cc_wip_scale with key artnr = it_obj_gr-matnr
*                                          elemt = it_tckh3-elemt
*                                 binary search.
      READ TABLE it_cc_wip_scale WITH KEY artnr = it_obj_gr-matnr
                                          aufnr = it_fsc_mat-aufnr
                                          elemt = it_tckh3-elemt
                                 BINARY SEARCH.
* }
      CLEAR fsc_std_cc.
      READ TABLE fsc_std_cc WITH KEY artnr = it_obj_gr-matnr
                                     elemt = it_tckh3-elemt
                                 BINARY SEARCH.
*-----if any entry exist
      CHECK it_cc_wip_scale-elemt <> space OR fsc_std_cc-elemt <> space.


      it_shop_cc_adj-wip_amt     = ( fsc_std_cc-dmbtr * l_scale )
                                       - it_cc_wip_scale-wip_amt.

      it_shop_cc_adj-wip_amt_f   = ( fsc_std_cc-dmbtr_f * l_scale )
                                       - it_cc_wip_scale-wip_amt_f.

      it_shop_cc_adj-wip_pamt    = ( fsc_std_cc-dmbtr * l_scale_p )
                                       - it_cc_wip_scale-wip_pamt.

      it_shop_cc_adj-wip_pamt_f  = ( fsc_std_cc-dmbtr_f * l_scale_p )
                                       - it_cc_wip_scale-wip_pamt_f.


      IF  it_shop_cc_adj-wip_amt    = 0
      AND it_shop_cc_adj-wip_pamt   = 0
      AND it_shop_cc_adj-wip_amt_f  = 0
      AND it_shop_cc_adj-wip_pamt_f = 0 .
        CONTINUE.
      ELSE.
* Single amount
        it_shop_cc_adj-single_amt =
              + it_shop_cc_adj-wip_pamt - it_shop_cc_adj-wip_amt.
        it_shop_cc_adj-manu_amt = it_shop_cc_adj-single_amt.

        it_shop_cc_adj-single_amt_f  =
              + it_shop_cc_adj-wip_pamt_f - it_shop_cc_adj-wip_amt_f.
        it_shop_cc_adj-manu_amt_f = it_shop_cc_adj-single_amt_f.

* control amt
        it_shop_cc_adj-control_amt = it_shop_cc_adj-manu_amt.

        it_shop_cc_adj-aufnr = it_fsc_mat-aufnr.
        it_shop_cc_adj-artnr = it_fsc_mat-matnr.

        it_shop_cc_adj-typps = c_typ_wip.
        it_shop_cc_adj-elemt = it_tckh3-elemt.

        COLLECT it_shop_cc_adj.
        CLEAR   it_shop_cc_adj.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " SCALE_WIP
*&---------------------------------------------------------------------*
*&      Form  make_element_value_act_std
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CKMLPRKEKO  text
*      -->P_LT_CKMLPRKEPH  text
*      -->P_P_TYPE  text
*      -->P_LT_CK13N_GR_QTY  text
*----------------------------------------------------------------------*
FORM make_element_value_act_std   USING  p_type
                                         p_qty.

  DATA : l_cnt(3) TYPE n,
         l_amt TYPE ckmlprkeph-kst001,
          l_field(25),
          l_field_name(25),
          l_field_name_f(25).

  FIELD-SYMBOLS: <f_field> ,<f_field2> .


  PERFORM make_l_field USING p_type
                       CHANGING l_field_name
                                l_field_name_f.

  CLEAR l_cnt.

  DO 40 TIMES VARYING l_amt FROM ld_wa_db_keph-kst001
                            NEXT ld_wa_db_keph-kst002.

    l_cnt = l_cnt + 1.
    CHECK NOT l_amt IS INITIAL.
* Overall value
    CLEAR it_elemt.
    READ TABLE it_elemt WITH TABLE KEY elnum = l_cnt.
    IF sy-subrc EQ 0.
      IF it_elemt-eltyp = 'V'.
        fsc_std_cc-elemt = it_elemt-elemt.
        ASSIGN (l_field_name)  TO   <f_field2> .
        <f_field2> = l_amt * p_qty / ld_wa_db_keko-losgr.

        PERFORM move_sum_header_to_cc.

        COLLECT fsc_std_cc . CLEAR fsc_std_cc.
      ELSE.
        fsc_std_cc-elemt = it_elemt-elemt.
        ASSIGN (l_field_name_f)  TO   <f_field2> .
        <f_field2> = l_amt * p_qty / ld_wa_db_keko-losgr.
        PERFORM move_sum_header_to_cc.

        COLLECT fsc_std_cc . CLEAR fsc_std_cc.
      ENDIF.
    ENDIF.
  ENDDO.
ENDFORM.                    " make_element_value_act_std
*&---------------------------------------------------------------------*
*&      Form  calc_shopcst
*&---------------------------------------------------------------------*
FORM calc_shopcst.

  DATA : l_idx       LIKE sy-tabix.

  DELETE it_shop_sum WHERE typps = 'O'.

*'V' with blank material
  SORT it_gi_misc BY matnr.
  READ TABLE it_gi_misc WITH KEY matnr = ''
                                 BINARY SEARCH.
  IF sy-subrc = 0.

    LOOP AT it_gi_misc FROM sy-tabix.
      IF it_gi_misc-matnr <> space.
        EXIT.
      ENDIF.

      CLEAR it_shop_sum.
      it_shop_sum-objnr      = it_gi_misc-objnr.
      it_shop_sum-kstar      = it_gi_misc-kstar.
      it_shop_sum-typps      = 'V'.
      it_shop_sum-add_wkgbtr = it_gi_misc-add_wkgbtr.

      IF it_shop_sum-resou = p_resou AND p_debug = 'X'.
        BREAK-POINT.
      ENDIF.

      COLLECT it_shop_sum.

    ENDLOOP.

  ENDIF.

  SORT it_coep_self BY objnr matnr.
  SORT it_mto_scrap BY objnr.
  SORT it_gi_misc   BY objnr kstar matnr.
  SORT it_prevshop  BY objnr resou.

  LOOP AT it_shop_sum .

    l_idx = sy-tabix.

*Scrap
*---if self GI, scrap with STD valuation.
    READ TABLE it_coep_self WITH KEY objnr = it_shop_sum-objnr
                                     BINARY SEARCH.

    IF sy-subrc = 0 AND it_coep_self-matnr = it_shop_sum-llv_matnr.
      it_shop_sum-scrap_amt = it_shop_sum-wkgbtr.
      it_shop_sum-scrap_qty = it_shop_sum-mbgbtr.

*---% of scrap for M/E type
    ELSE.
      IF it_shop_sum-typps = 'V'.

      ELSE.  "
        READ TABLE it_mto_scrap WITH KEY objnr = it_shop_sum-objnr
                                         BINARY SEARCH.
        IF sy-subrc = 0 AND it_mto_scrap-xmnga <> 0.
          it_shop_sum-scrap_amt = it_shop_sum-wkgbtr *
             ( it_mto_scrap-xmnga /
                   ( it_mto_scrap-lmnga + it_mto_scrap-xmnga ) ).
          it_shop_sum-scrap_qty = it_shop_sum-mbgbtr *
             ( it_mto_scrap-xmnga /
                   ( it_mto_scrap-lmnga + it_mto_scrap-xmnga ) ).
        ENDIF.
      ENDIF.
    ENDIF.

* MISC GI Qty / value
    CASE it_shop_sum-typps.
      WHEN 'M' .
        READ TABLE it_gi_misc WITH KEY objnr = lt_wipvalue-objnr
                                       kstar = lt_wipvalue-kstar
                                       matnr = lt_wipvalue-material
                                       BINARY SEARCH.
        IF sy-subrc = 0.
          it_shop_sum-add_wkgbtr = it_gi_misc-add_wkgbtr.
          it_shop_sum-add_mbgbtr = it_gi_misc-add_mbgbtr.
        ENDIF.

* if  Activity type = 'E'.
* ADD value => Var value
      WHEN 'E' .
        it_shop_sum-wkgbtr2  = it_shop_sum-wkgbtr2 +
                               it_shop_sum-add_wkgbtr.
        it_shop_sum-mbgbtr2  = it_shop_sum-mbgbtr2 +
                               it_shop_sum-add_mbgbtr.
        CLEAR : it_shop_sum-add_wkgbtr, it_shop_sum-add_mbgbtr.

    ENDCASE.

* SINGLE LEVEL Qty and Single level AMT
    IF it_shop_sum-typps CA 'ME' .
      it_shop_sum-single_qty =
              it_shop_sum-mbgbtr + it_shop_sum-add_mbgbtr
            + it_shop_sum-mbgbtr2                          "PPCVAR
*         + IT_SHOP_SUM-scrap_qty
            - it_shop_sum-gr_qty
            + it_shop_sum-wip_pqty - it_shop_sum-wip_qty.
    ENDIF.

*FIXME - Price diff. allocation
    it_shop_sum-single_amt =
          it_shop_sum-wkgbtr + it_shop_sum-add_wkgbtr
          + it_shop_sum-wkgbtr2                            "PPCVAR
*         + IT_SHOP_SUM-scrap_amt
          - it_shop_sum-gr_amt
          + it_shop_sum-wip_pamt - it_shop_sum-wip_amt.

* Manuf Qty / Manuf Amount.
    CLEAR it_prevshop.
    READ TABLE it_prevshop WITH KEY objnr = it_shop_sum-objnr
                                    resou = it_shop_sum-resou
                                    BINARY SEARCH.

    IF it_shop_sum-typps CA 'ME' .
      it_shop_sum-manu_qty =
              it_shop_sum-mbgbtr  + it_shop_sum-add_mbgbtr
            + it_shop_sum-mbgbtr2                          "PPCVAR
            - it_shop_sum-wip_qty + it_shop_sum-wip_pqty.
    ENDIF.

    it_shop_sum-manu_amt =
*         + IT_SHOP_SUM-wkgbtr + IT_SHOP_SUM-add_wkgbtr
          + it_shop_sum-gr_amt
          + it_shop_sum-single_amt
          + it_shop_sum-multi_amt.
*          - IT_SHOP_SUM-multi_pre_amt.

    MODIFY it_shop_sum INDEX l_idx.

  ENDLOOP.


ENDFORM.                    " calc_shopcst
*&---------------------------------------------------------------------*
*&      Form  calc_shop_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_shop_cc.

  LOOP AT it_shop_cc .
    CHECK it_shop_cc-typps EQ 'E' OR
          it_shop_cc-typps EQ 'M'.
* Over all
* Material ledger single amt
    it_shop_cc-single_amt =
          it_shop_cc-wkgbtr + it_shop_cc-add_wkgbtr
          + it_shop_cc-wkgbtr2                            "PPCVAR
          - it_shop_cc-gr_amt
          + it_shop_cc-wip_pamt - it_shop_cc-wip_amt.
* Material ledger multi amt
    it_shop_cc-multi_amt =
          + it_shop_cc-multi_samt
          + it_shop_cc-multi_mamt.

* Manuf Amount.
    it_shop_cc-manu_amt =
          + it_shop_cc-gr_amt
          + it_shop_cc-single_amt
          + it_shop_cc-multi_amt.
* control amt
    it_shop_cc-control_amt = it_shop_cc-manu_amt.


* Fixed
* Material ledger single amt
    it_shop_cc-single_amt_f =
          it_shop_cc-wkgbtr_f + it_shop_cc-add_wkgbtr_f
          + it_shop_cc-wkgbtr2_f                            "PPCVAR
          - it_shop_cc-gr_amt_f
          + it_shop_cc-wip_pamt_f - it_shop_cc-wip_amt_f.
* Material ledger multi amt
    it_shop_cc-multi_amt_f =
          + it_shop_cc-multi_samt_f
          + it_shop_cc-multi_mamt_f.

* Manuf Amount.
    it_shop_cc-manu_amt_f =
          + it_shop_cc-gr_amt_f
          + it_shop_cc-single_amt_f
          + it_shop_cc-multi_amt_f.
* control amt
    it_shop_cc-control_amt_f = it_shop_cc-manu_amt_f.

    MODIFY it_shop_cc.
*    MODIFY it_shop_cc. "INDEX sy-tabix.

* collect cc to resouce level
    IF  NOT it_shop_cc-resou IS INITIAL.
      MOVE-CORRESPONDING it_shop_cc TO it_res_ccsum.
      COLLECT it_res_ccsum.  CLEAR it_res_ccsum.

*      CLEAR it_kstar .
*      READ TABLE it_kstar WITH TABLE KEY kstar = it_shop_cc-kstar.
*
*      READ TABLE it_shop_sum WITH KEY typps   = it_shop_cc-typps
*                                      kstar   = it_shop_cc-kstar
*                                      resou   = it_shop_cc-resou
*                                      aufnr   = it_shop_cc-aufnr
*                             BINARY SEARCH.
*      CHECK sy-subrc = 0.
*
*      PERFORM calculate_rounding_diff2.

*      it_shop_cc-artnr      =   it_shop_sum-artnr.
*      it_shop_cc-aufnr      =   it_shop_sum-aufnr.
*      it_shop_cc-typps      =   it_shop_sum-typps.
*      it_shop_cc-kstar      =   it_shop_sum-kstar.
*      it_shop_cc-resou      =   it_shop_sum-resou.
**-----use cc from CE, use same resource type
*      it_shop_cc-elemt      =   it_kstar-elemt.
**         it_shop_cc-typps      =   c_typ_round.

*      COLLECT it_shop_cc. CLEAR it_shop_cc.
*      MOVE-CORRESPONDING it_shop_cc TO it_res_ccsum.
*      COLLECT it_res_ccsum.  CLEAR it_res_ccsum.
    ENDIF.

  ENDLOOP.
  CLEAR it_shop_cc.
ENDFORM.                    " calc_shop_cc
*&---------------------------------------------------------------------*
*&      Form  create_it_shop_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_it_shop_cc.
  DATA: lt_cc_amt LIKE gt_cc_amt_new OCCURS 0 WITH HEADER LINE.
  DATA : l_mbgbtr2 TYPE gt_amt.


  CHECK NOT it_shop_sum[] IS INITIAL.


  MESSAGE s000 WITH 'CC... get_ml_customizing'.
  PERFORM get_ml_customizing.  "read tckh3..

  MESSAGE s000 WITH 'CC... prepare_ml_cc_read_items'.
  PERFORM prepare_ml_cc_read_items.

  MESSAGE s000 WITH 'CC... call_mlccs_read_pr'.
* Material (table:CKMLPRKEPH)
  PERFORM call_mlccs_read_pr.

  MESSAGE s000 WITH 'CC... call_mlccs_read_pr_fsc'.
  PERFORM call_mlccs_read_pr_fsc.

* Activity (table:CKMLPRKEPH)
  MESSAGE s000 WITH 'CC... call_ckml_la_header_read'.
  PERFORM call_ckml_la_header_read.

** Cost component
*  MESSAGE s000 WITH 'CC... call_kkek_cost_component'.
*  PERFORM call_kkek_cost_component.       "<-- Not used..


  SORT it_prkeko     BY kalnr bdatj poper.
  SORT it_prkeph     BY kalnr bdatj poper kkzst.
  SORT it_ckmllacr   BY kalnr.
  SORT it_ckmlprkeko BY kalnr.
  SORT it_ckmlprkeph BY kalnr kkzst.
  SORT it_fsc_mat    BY matnr.
  SORT it_ckmlkeph   BY kalnr categ ptyp bvalt.

  SORT it_coep       BY objnr kstar kostl lstar.

  MESSAGE s000 WITH 'CC... processing_new_t00020'.
  PERFORM processing_new_t00020.

  MESSAGE s000 WITH 'CC... select_mlkeph'.
  PERFORM select_mlkeph .


*-master loop for cc break-down
  MESSAGE s000 WITH 'CC... looping it_shop_sum'.
  DATA: lv_shop_sum LIKE it_shop_sum.

  SORT it_shop_sum BY typps resou objnr.
  LOOP AT it_shop_sum .
    gv_shop_sum_idx = sy-tabix.
*    IF it_shop_sum-resou = p_resou . BREAK-POINT. ENDIF.

    IF it_shop_sum-resou <> lv_shop_sum-resou.
    ENDIF.

* Current/Current var./GR/Scrap
    CASE it_shop_sum-typps.
      WHEN 'M' .    "Material
        PERFORM making_shop_cc_material.
        PERFORM making_shop_cc_add.

      WHEN 'E'.     "Activity
        PERFORM making_shop_cc_activity .
        PERFORM making_shop_cc_add.

      WHEN 'V'.
        PERFORM making_shop_cc_v.

*---- problem in cost component break-down
      WHEN OTHERS.
        CONCATENATE '***Problem: :' it_shop_sum-resou
               INTO gt_msg.
        PERFORM progress_ind USING '88' gt_msg.
        CONTINUE.
    ENDCASE.


* ML-multi
*   MESSAGE s000 WITH 'CC... looping it_shop_sum - making_shop_cc_ml_multi'.
    PERFORM making_shop_cc_ml_multi.

** collect sum from cc data
*    move-corresponding it_shop_sum to it_obj_sum_wip.
*    collect it_obj_sum_wip. clear it_obj_sum_wip.

    lv_shop_sum = it_shop_sum.
  ENDLOOP.

ENDFORM.                    " create_it_shop_cc
*&---------------------------------------------------------------------*
*&      Form  move_sum_header_to_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM move_sum_header_to_cc .

  IF g_std = 'X'.
    fsc_std_cc-elemt           =   it_elemt-elemt.
*   FSC_STD_CC-aufnr           =   it_shop_sum-aufnr.
*    FSC_STD_CC-kokrs           =   it_shop_sum-kokrs.
*    FSC_STD_CC-bdatj           =   it_shop_sum-bdatj.
*    FSC_STD_CC-poper           =   it_shop_sum-poper.
*    FSC_STD_CC-typps           =   it_shop_sum-typps.
*    FSC_STD_CC-kstar           =   it_shop_sum-kstar.
*    FSC_STD_CC-resou           =   it_shop_sum-resou.
*    FSC_STD_CC-artnr           =   it_shop_sum-artnr.


*  ELSEIF g_round = 'X'.
*    it_ml_scale-elemt         =   it_tckh3-elemt.
*    it_ml_scale-kokrs         =   it_shop_sum-kokrs.
*    it_ml_scale-bdatj         =   it_shop_sum-bdatj.
*    it_ml_scale-poper         =   it_shop_sum-poper.
*    it_ml_scale-aufnr         =   it_shop_sum-aufnr.
*    it_ml_scale-artnr         =   it_shop_sum-artnr.

  ELSE.
*   it_shop_cc-elemt            =   it_tckh3-elemt.
*    it_shop_cc-kokrs            =   it_shop_sum-kokrs.
*    it_shop_cc-bdatj            =   it_shop_sum-bdatj.
*    it_shop_cc-poper            =   it_shop_sum-poper.

    it_shop_cc-artnr            =   it_shop_sum-artnr.
    it_shop_cc-aufnr            =   it_shop_sum-aufnr.
    it_shop_cc-typps            =   it_shop_sum-typps.
    it_shop_cc-kstar            =   it_shop_sum-kstar.
    it_shop_cc-resou            =   it_shop_sum-resou.

  ENDIF.

ENDFORM.                    " move_sum_header_to_cc
*&---------------------------------------------------------------------*
*&      Form  unit_converion2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM unit_converion2 USING    f_in_qty
                              f_in_unit
                              f_out_unit
                     CHANGING f_out_qty.
* caution  " unit in <= out_unit
*          " unit out<= in_unit
* Unit Conversion - MEEHT
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = f_in_qty
      unit_in  = f_out_unit  "Hour
      unit_out = f_in_unit
    IMPORTING
      output   = f_out_qty.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " unit_converion2
*&---------------------------------------------------------------------*
*&      Form  read_shop_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_shop_name.
* ANDY
* shop... need to be derived from current master...
* delete below logics...
*  CALL FUNCTION 'Z_CO_SHOP_DETERMINE'
*    EXPORTING
*      f_typps = it_shop_sum-typps
*      f_kostl = it_shop_sum-kostl
*      f_prctr = it_mat-prctr
*      f_fevor = it_mat-fevor
*      f_werks = it_mat-werks
*      f_raube = it_mat-raube
*    IMPORTING
*      e_shop  = it_shop_sum-shop.

** by ig.moon 11/19/2009 {
*  IF it_shop_sum-shop IS INITIAL OR it_mat-prctr IS INITIAL.
*    SELECT SINGLE prctr INTO it_shop_sum-shop
*      FROM marc WHERE matnr EQ it_mat-matnr
*                AND   werks EQ it_mat-werks.
*  ENDIF.
* }

*  data : l_mtart type mtart.
*  data : l_fevor like marc-fevor.
*  data : l_vspvb like it_mat-vspvb.
*
*  if it_shop_sum-par_werks = 'E001'.
*    it_shop_sum-shop = 'MXEX'.
*
*  else.
*    if f_typps = 'E' .
*      if it_shop_sum-kostl <> ''.
*        it_shop_sum-shop = it_shop_sum-kostl(4).
*      endif.
*
*    elseif f_typps = 'V'.
*      it_shop_sum-shop = 'MXTX'.
*
*    else.  "Material
*      case it_mat-fevor.
*        when 'SPB' or 'SPD' or 'SPP'.
*          it_shop_sum-shop = 'MXSX'.
*        when 'SEA' or 'SEC'.
*          it_shop_sum-shop = 'MXEX'.
*
*        when others.
*          case it_mat-raube.
*            when 10.
*              it_shop_sum-shop = 'MXSX'.
*            when 11.
*              it_shop_sum-shop = 'MXBX'.
*            when 12.
*              it_shop_sum-shop = 'MXPX'.
*            when 13.
*              it_shop_sum-shop = 'MXTX'.
*            when 14.
*              it_shop_sum-shop = 'MXEX'.
*            when others.
*              it_shop_sum-shop = space. "'MXTX'.
*          endcase.
*      endcase.
*
*    endif. "Material
*  endif.                                                    "P001

* KMMG Rule by Han Moon

*?????
*  CASE it_shop_sum-typps.
*    WHEN 'M'.
*      IF it_fsc_mat-mtart EQ 'SEMI'.
*        IF it_fsc_mat-matnr+10(1) EQ 'A'.   "Panel for A/S
*          IF it_mat-lgpro EQ 'L230'.  "Panel to Stamping
*            it_shop_sum-shop = 'S'.
*          ELSE.
*            READ TABLE it_tp WITH TABLE KEY prvbe = it_mat-vspvb.
*            IF sy-subrc EQ 0.
*              it_shop_sum-shop = it_tp-tpoint(1).
*            ELSE.
*              CASE it_mat-lgpro(1).
*                WHEN 'S'.
*                  it_shop_sum-shop = 'S'.
*                WHEN 'R'.
*                  CASE it_mat-lgpro+1(1).
*                    WHEN '1'.
*                      it_shop_sum-shop = 'S'.
*                    WHEN OTHERS.
*                      it_shop_sum-shop = 'B'.
*                  ENDCASE.
*                WHEN OTHERS.
*                  it_shop_sum-shop = 'B'.
*              ENDCASE.
*            ENDIF.
*          ENDIF.
**------ Overwrite Shop
*          IF it_shop_sum-shop = 'T' OR
*             it_shop_sum-shop = 'P'.
*            it_shop_sum-shop = 'B'.
*          ENDIF.
*        ELSE.  "Blank/Panel
*          it_shop_sum-shop = 'S'.
*        ENDIF.
*      ELSE.    "FSC / KMAT
*        IF it_mat-lgpro EQ 'L230'.  "Panel to Stamping
*          it_shop_sum-shop = 'S'.
*        ELSE.
*          READ TABLE it_tp WITH TABLE KEY prvbe = it_mat-vspvb.
*          IF sy-subrc EQ 0.
*            it_shop_sum-shop = it_tp-tpoint(1).
*          ELSE.
*            CASE it_mat-lgpro(1).
*              WHEN 'S'.
*                it_shop_sum-shop = 'S'.
*              WHEN 'R'.
*                CASE it_mat-lgpro+1(1).
*                  WHEN '1'.
*                    it_shop_sum-shop = 'S'.
*                  WHEN '2'.
*                    it_shop_sum-shop = 'B'.
*                  WHEN '3'.
*                    it_shop_sum-shop = 'P'.
*                  WHEN OTHERS.
*                    it_shop_sum-shop = 'T'.
*                ENDCASE.
*              WHEN OTHERS.
*                it_shop_sum-shop = 'T'.
*            ENDCASE.
*          ENDIF.
*        ENDIF.
*
**------ Overwrite Shop for BIW/BIP
*        CASE it_fsc_mat-matnr+4(1).
*          WHEN 'P'.
*            IF it_shop_sum-shop = 'T'.
*              it_shop_sum-shop = 'P'.
*            ENDIF.
*          WHEN 'W'.
*            IF it_shop_sum-shop = 'P' OR
*               it_shop_sum-shop = 'T'.
*              it_shop_sum-shop = 'B'.
*            ENDIF.
*          WHEN OTHERS.
*            "No changes.
*        ENDCASE.
*
*      ENDIF.
*    WHEN 'E'.
*      CASE it_shop_sum-kostl+1(2).   "K5210
*        WHEN '51'.
*          it_shop_sum-shop = 'S'.
*        WHEN '52'.
*          it_shop_sum-shop = 'B'.
*        WHEN '53'.
*          it_shop_sum-shop = 'P'.
*        WHEN '54'.
*          it_shop_sum-shop = 'T'.
*      ENDCASE.
*    WHEN 'V'.
*      IF it_fsc_mat-mtart EQ 'SEMI'.
*        IF it_fsc_mat-matnr+10(1) EQ 'A'.
*          it_shop_sum-shop = 'B'.
*        ELSE.
*          it_shop_sum-shop = 'S'.
*        ENDIF.
*      ELSE.      "BIW/BIP - FSC / KMAT
*        CASE it_fsc_mat-matnr+4(1).
*          WHEN 'P'.
*            it_shop_sum-shop = 'P'.
*          WHEN 'W'.
*            it_shop_sum-shop = 'B'.
*          WHEN OTHERS.
*            it_shop_sum-shop = 'T'.
*        ENDCASE.
*      ENDIF.
*  ENDCASE.

  CALL FUNCTION 'Z_CO_SHOP_DETERMINE'
    EXPORTING
      f_typps = it_shop_sum-typps
      f_kostl = it_shop_sum-kostl
      f_prctr = it_mat-prctr
      f_fevor = it_mat-fevor
      f_werks = it_mat-werks
      f_raube = it_mat-raube
    IMPORTING
      e_shop  = it_shop_sum-shop.

* by ig.moon 11/19/2009 {
  IF it_shop_sum-shop IS INITIAL OR it_mat-prctr IS INITIAL.
    SELECT SINGLE prctr INTO it_shop_sum-shop
    FROM marc WHERE matnr EQ it_mat-matnr
                AND werks EQ it_mat-werks.
  ENDIF.
* }
ENDFORM.                    " read_shop_name
*&---------------------------------------------------------------------*
*&      Form  convert_unit_wip_std
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM convert_unit_wip_std USING p_unit_in
                                p_unit_out .

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-wip_quantity
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-wip_quantity.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-actual_scrap
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-actual_scrap.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-planned_scrap
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-planned_scrap.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-variance_qty
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-variance_qty.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-actual_qty_stpc
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-actual_qty_stpc.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-target_qty
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-target_qty.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-prev_wip
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-prev_wip.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-curr_wip
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-curr_wip.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-apo_var_qty
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-apo_var_qty.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-apo_input_qty
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-apo_input_qty.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input    = ls_wip_qty-apo_output_qty
      unit_in  = p_unit_in
      unit_out = p_unit_out  "Activity master unit
    IMPORTING
      output   = ls_wip_qty-apo_output_qty.


ENDFORM.                    " convert_unit_wip_std
*&---------------------------------------------------------------------*
*&      Form  CALL_MLCCS_READ_PR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_mlccs_read_pr.
  RANGES :ir_prtyp FOR mlprkeph-prtyp,
          ir_curtp FOR tkel-curtp.

*//Modify..03/29/2011..T00020..
*  SORT it_mat BY matnr werks.
**component
*  LOOP AT it_shop_sum WHERE llv_matnr <> ''.
*    CLEAR it_mat.
*    READ TABLE it_mat WITH KEY matnr = it_shop_sum-llv_matnr
*                               werks = it_shop_sum-par_werks
*                               BINARY SEARCH.
*    CHECK sy-subrc = 0 .
*    lt_kalnr-kalnr  = it_mat-kaln1 .
*    COLLECT lt_kalnr.  CLEAR lt_kalnr.
*  ENDLOOP.
*
**product
*  LOOP AT it_fsc_gr.
*    READ TABLE it_mat WITH KEY matnr = it_fsc_gr-matnr
*                               werks = it_fsc_gr-werks
*                               BINARY SEARCH.
*    CHECK sy-subrc = 0.
*    lt_kalnr-kalnr  = it_mat-kaln1 .
*    COLLECT lt_kalnr.  CLEAR lt_kalnr.
*  ENDLOOP.
*
*  SORT lt_kalnr.
*  DELETE ADJACENT DUPLICATES FROM lt_kalnr.
*//

  ir_prtyp = 'IEQS'.
  APPEND ir_prtyp.
  ir_curtp = 'IEQ10'.
  APPEND ir_curtp.

*CURRENT PERIOD
  CALL FUNCTION 'MLCCS_READ_PR'
    EXPORTING
      i_use_buffer            = space
      i_bdatj_1               = p_bdatj
      i_poper_1               = p_perab    "Current
      i_bdatj_2               = g_pr_lfgja
      i_poper_2               = g_pr_lfmon "Previous
    IMPORTING
      et_prkeko               = it_prkeko_temp
      et_prkeph               = it_prkeph_temp
    TABLES
      it_kalnr                = lt_kalnr
      ir_prtyp                = ir_prtyp
      ir_curtp                = ir_curtp
    EXCEPTIONS
      no_data_found           = 1
      input_data_inconsistent = 2
      OTHERS                  = 3.

*  it_prkeko[] = it_prkeko_temp[].
*  it_prkeph[] = it_prkeph_temp[].
  INSERT LINES OF it_prkeko_temp INTO TABLE it_prkeko.
  INSERT LINES OF it_prkeph_temp INTO TABLE it_prkeph.

* Commented out by Han Moon
**PREVIOUS PERIOD
*  CALL FUNCTION 'MLCCS_READ_PR'
*    EXPORTING
*      i_use_buffer            = space
*      i_bdatj_1               = g_pr_lfgja
*      i_poper_1               = g_pr_lfmon
*    IMPORTING
*      et_prkeko               = it_prkeko_temp
*      et_prkeph               = it_prkeph_temp
*    TABLES
*      it_kalnr                = lt_kalnr
*      ir_prtyp                = ir_prtyp
*      ir_curtp                = ir_curtp
*    EXCEPTIONS
*      no_data_found           = 1
*      input_data_inconsistent = 2
*      OTHERS                  = 3.
*
*  it_prkeko_p[] = it_prkeko_temp[].
*  it_prkeph_p[] = it_prkeph_temp[].


ENDFORM.                    " CALL_MLCCS_READ_PR
*&---------------------------------------------------------------------*
*&      Form  read_materials_PARALLEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_materials_parallel.

  IF it_fsc_mat[] IS INITIAL OR s_aufnr[] IS INITIAL.
    MESSAGE e076 WITH c_fsc c_fsc_plant.
  ENDIF.

*  LOOP AT IT_FSC_MAT.
*    S_MATNR-SIGN       = 'I' .
*    S_MATNR-OPTION     = 'EQ'.
*    S_MATNR-LOW        = IT_FSC_MAT-MATNR.
*    APPEND S_MATNR.
**//Modify..04/05/2011..T00020..
*    S_AUFNR-SIGN       = 'I' .
*    S_AUFNR-OPTION     = 'EQ'.
*    S_AUFNR-LOW        = IT_FSC_MAT-AUFNR.
*    APPEND S_AUFNR.
**//
*    S_WERKS-SIGN       = 'I' .
*    S_WERKS-OPTION     = 'EQ'.
*    S_WERKS-LOW        = IT_FSC_MAT-WERKS.
*    COLLECT S_WERKS.
*  ENDLOOP.



ENDFORM.                    " read_materials_PARALLEL
*&---------------------------------------------------------------------*
*&      Form  call_CKML_LA_HEADER_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_ckml_la_header_read.

  DATA : l_kalnr LIKE ckmllahd-kalnr.


**//Modify..03/29/2011..T00020..
* select object no. by Cost center & activity type
*  DATA : BEGIN OF lt_onrkl OCCURS 0,
*          kostl LIKE onrkl-kostl,
*          lstar LIKE onrkl-lstar,
*          objnr LIKE onrkl-objnr,
*         END OF lt_onrkl.
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE  lt_onrkl
*     FROM onrkl
*    WHERE kokrs = p_kokrs.
*
*
*  LOOP AT it_shop_sum WHERE typps = 'E' .
*    CLEAR lt_onrkl .
*    READ TABLE lt_onrkl WITH KEY kostl = it_shop_sum-kostl
*                                 lstar = it_shop_sum-lstar  .
*    IF sy-subrc = 0.
*      it_act-kostl = lt_onrkl-kostl.
*      it_act-lstar = lt_onrkl-lstar.
*      it_act-objnr = lt_onrkl-objnr.
*      APPEND it_act. CLEAR it_act.
*    ENDIF.
*  ENDLOOP.
*
*  SORT it_act.
*  DELETE ADJACENT DUPLICATES FROM it_act.
*//
  DATA : lt_act LIKE it_act OCCURS 0 WITH HEADER LINE.
  REFRESH lt_act. CLEAR lt_act.

  SORT it_act BY kostl lstar.
  LOOP AT it_shop_sum WHERE typps = 'E' .

    READ TABLE it_act WITH KEY kostl = it_shop_sum-kostl
                               lstar = it_shop_sum-lstar
                      BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_act TO lt_act.
      APPEND lt_act. CLEAR lt_act.
    ENDIF.
  ENDLOOP.

  SORT lt_act.
  DELETE ADJACENT DUPLICATES FROM lt_act.

  DATA l_tabix TYPE sy-tabix.

  LOOP AT lt_act.  "it_act.
    l_tabix = sy-tabix.

    l_kalnr = lt_act-kalnr. "it_act-kalnr.

*CURRENT PERIOD
    CALL FUNCTION 'CKML_LA_PERIOD_READ'
      EXPORTING
        i_kalnr              = l_kalnr
        i_bdatj              = p_bdatj
        i_poper              = p_perab
        i_components         = 'X'
      IMPORTING
        et_prkeko            = lt_ckmlprkeko_temp
        et_prkeph            = lt_ckmlprkeph_temp
      TABLES
        et_ckmllacr          = lt_ckmllacr
      EXCEPTIONS
        interface_error      = 1
        no_period_data_found = 2
        OTHERS               = 3.
* Replaced by Han Moon
*    LOOP AT lt_ckmlprkeko_temp INTO ld_wa_db_keko .
*      it_ckmlprkeko =  ld_wa_db_keko .
*      APPEND it_ckmlprkeko.
*      CLEAR  ld_wa_db_keko.
*    ENDLOOP.
*
*    LOOP AT lt_ckmlprkeph_temp INTO ld_wa_db_keph .
*      it_ckmlprkeph =  ld_wa_db_keph .
*      APPEND it_ckmlprkeph.
*      CLEAR  ld_wa_db_keph.
*    ENDLOOP.

    APPEND LINES OF lt_ckmlprkeko_temp TO it_ckmlprkeko.
    APPEND LINES OF lt_ckmlprkeph_temp TO it_ckmlprkeph.

    APPEND LINES OF lt_ckmllacr TO it_ckmllacr.


*PREVIOUS PERIOD
    CALL FUNCTION 'CKML_LA_PERIOD_READ'
      EXPORTING
        i_kalnr              = l_kalnr
        i_bdatj              = g_pr_lfgja
        i_poper              = g_pr_lfmon
        i_components         = 'X'
      IMPORTING
        et_prkeko            = lt_ckmlprkeko_temp
        et_prkeph            = lt_ckmlprkeph_temp
      TABLES
        et_ckmllacr          = lt_ckmllacr
      EXCEPTIONS
        interface_error      = 1
        no_period_data_found = 2
        OTHERS               = 3.

* Replaced by Han Moon
*    LOOP AT lt_ckmlprkeko_temp INTO ld_wa_db_keko .
*      it_ckmlprkeko_p =  ld_wa_db_keko .
*      APPEND it_ckmlprkeko_p.
*      CLEAR: ld_wa_db_keko.
*    ENDLOOP.
*
*    LOOP AT lt_ckmlprkeph_temp INTO ld_wa_db_keph .
*      it_ckmlprkeph_p =  ld_wa_db_keph .
*      APPEND it_ckmlprkeph_p.
*      CLEAR: ld_wa_db_keph.
*    ENDLOOP.

    APPEND LINES OF lt_ckmlprkeko_temp TO it_ckmlprkeko_p.
    APPEND LINES OF lt_ckmlprkeph_temp TO it_ckmlprkeph_p.

    APPEND LINES OF lt_ckmllacr TO it_ckmllacr_p.

  ENDLOOP.

ENDFORM.                    " call_CKML_LA_HEADER_READ
*&---------------------------------------------------------------------*
*&      Form  collect_shop_cc
*&---------------------------------------------------------------------*
* p_amt = CC amt
* p_amt_type = Overall or Fixed
*----------------------------------------------------------------------*
FORM collect_shop_cc USING p_amt
                           p_amt_type .

  DATA: l_source_value     TYPE ckmlcr-salk3,
        l_destin_value     TYPE ckmlcr-salk3,
        l_value            TYPE zcoamt4.


*//Modify..04/04/2011..T00020..
  CASE p_amt_type.
    WHEN 'A'.
      l_source_value = ld_wa_db_price-togbtr.
    WHEN OTHERS.   "Fixed
      l_source_value = ld_wa_db_price-tofbtr.
  ENDCASE.

* current
  IF NOT it_shop_sum-mbgbtr IS INITIAL.

    l_destin_value = l_source_value * it_shop_sum-mbgbtr
                                    / ld_wa_db_keko-losgr.
    l_value  =  p_amt   * l_destin_value / l_source_value.

    CASE p_amt_type.
      WHEN 'A'.
        it_shop_cc-wkgbtr   = l_value.
      WHEN OTHERS.
        it_shop_cc-wkgbtr_f = l_value.
    ENDCASE.
*    PERFORM make_value USING    'C'
*                                p_amt
*                                p_amt_type
*                                it_shop_sum-mbgbtr.
  ENDIF.

* Current additional GI
  IF NOT it_shop_sum-mbgbtr2 IS INITIAL.

    l_destin_value      = l_source_value * it_shop_sum-mbgbtr2
                                         / ld_wa_db_keko-losgr.
    l_value  =  p_amt   * l_destin_value / l_source_value.

    CASE p_amt_type.
      WHEN 'A'.
        it_shop_cc-wkgbtr2   = l_value.
      WHEN OTHERS.
        it_shop_cc-wkgbtr2_f = l_value.
    ENDCASE.
*    PERFORM make_value  USING   'V'
*                                p_amt
*                                p_amt_type
*                                it_shop_sum-mbgbtr2.
  ENDIF.

* GR
  IF NOT it_shop_sum-gr_qty IS INITIAL.

    l_destin_value      = l_source_value * it_shop_sum-gr_qty
                                        / ld_wa_db_keko-losgr.
    l_value  =  p_amt   * l_destin_value / l_source_value.

    CASE p_amt_type.
      WHEN 'A'.
        it_shop_cc-gr_amt   = l_value.
      WHEN OTHERS.
        it_shop_cc-gr_amt_f = l_value.
    ENDCASE.
*    PERFORM make_value USING    'G'
*                                p_amt
*                                p_amt_type
*                                it_shop_sum-gr_qty.
  ENDIF.

* Scrap
  IF NOT it_shop_sum-scrap_qty IS INITIAL.

    l_destin_value      = l_source_value * it_shop_sum-scrap_qty
                                         / ld_wa_db_keko-losgr.
    l_value  =  p_amt   * l_destin_value / l_source_value.

    CASE p_amt_type.
      WHEN 'A'.
        it_shop_cc-scrap_amt   = l_value.
      WHEN OTHERS.
        it_shop_cc-scrap_amt_f = l_value.
    ENDCASE.
*    PERFORM make_value USING    'S'
*                                p_amt
*                                p_amt_type
*                                it_shop_sum-scrap_qty.

  ENDIF.


* WIP Begining
*FIXME LATER - CC should be previous period... - ANDY
*  if not it_shop_sum-wip_pqty is initial.
*    perform make_value using    'B'
*                                p_amt
*                                p_amt_type
*                                it_shop_sum-wip_pqty.
*  endif.

* WIP Ending
  IF NOT it_shop_sum-wip_qty IS INITIAL.

    l_destin_value      = l_source_value * it_shop_sum-wip_qty
                                         / ld_wa_db_keko-losgr.
    l_value  =  p_amt   * l_destin_value / l_source_value.

    CASE p_amt_type.
      WHEN 'A'.
        it_shop_cc-wip_amt   = l_value.
      WHEN OTHERS.
        it_shop_cc-wip_amt_f = l_value.
    ENDCASE.
*    PERFORM make_value USING   'E'
*                                p_amt
*                                p_amt_type
*                                it_shop_sum-wip_qty.
  ENDIF.

* Target Qty
  CLEAR it_fsc_mat.
  READ TABLE it_fsc_mat WITH KEY matnr = it_shop_sum-artnr
                                 BINARY SEARCH.
  IF sy-subrc = 0.

    CASE it_fsc_mat-categ.

      WHEN 'DI'.

        IF NOT it_shop_sum-apo_output_qty IS INITIAL.

          l_destin_value = l_source_value * it_shop_sum-apo_output_qty
                                          / ld_wa_db_keko-losgr.
          l_value = p_amt * l_destin_value / l_source_value.

          CASE p_amt_type.
            WHEN 'A'.
              it_shop_cc-target_amt   = l_value.
            WHEN OTHERS.
              it_shop_cc-target_amt_f = l_value.
          ENDCASE.
*      PERFORM make_value USING   'T'
*                                  p_amt
*                                  p_amt_type
*                                  it_shop_sum-apo_output_qty.
        ENDIF.

      WHEN OTHERS.

        IF NOT it_shop_sum-gr_qty IS INITIAL.

          l_destin_value = l_source_value * it_shop_sum-gr_qty
                                          / ld_wa_db_keko-losgr.
          l_value = p_amt * l_destin_value / l_source_value.

          CASE p_amt_type.
            WHEN 'A'.
              it_shop_cc-target_amt   = l_value.
            WHEN OTHERS.
              it_shop_cc-target_amt_f = l_value.
          ENDCASE.
*      PERFORM make_value USING   'T'
*                                  p_amt
*                                  p_amt_type
*                                  it_shop_sum-gr_qty.
        ENDIF.

    ENDCASE.
  ENDIF.


  PERFORM move_sum_header_to_cc.
  COLLECT it_shop_cc .   CLEAR it_shop_cc.


*  exit.
*
*  check g_round = ''.
** Standard ( * Itemize Qty)
*
*  clear it_fsc_mat.
*  read table it_fsc_mat with key objnr = it_shop_sum-objnr.
*
*  clear lt_ck13n.
*  read table lt_ck13n with key matnr    = it_fsc_mat-matnr
*                               resou    = it_shop_sum-resou.
*
*
*  check not lt_ck13n-gr_qty is initial.
*  g_std = 'X'.
*  perform make_value using   'B'
*                              p_amt
*                              p_amt_type
*                              lt_ck13n-gr_qty.
*
*  perform make_value using   'E'
*                              p_amt
*                              p_amt_type
*                              lt_ck13n-gr_qty.
*
*  perform move_sum_header_to_cc .
*  collect fsc_std_cc . clear fsc_std_cc.
*  clear g_std.
*

* Standard ( * Itemize Qty)
*  PERFORM standard_make_value.


ENDFORM.                    " collect_shop_cc
*&---------------------------------------------------------------------*
*&      Form  collect_shop_cc_p
*&---------------------------------------------------------------------*
* p_amt = CC amt
* p_amt_type = Overall or Fixed
*----------------------------------------------------------------------*
FORM collect_shop_cc_p USING p_amt
                             p_amt_type .

  DATA: l_source_value     TYPE ckmlcr-salk3,
        l_destin_value     TYPE ckmlcr-salk3,
        l_value            TYPE zcoamt4.

  IF NOT it_shop_sum-wip_pqty IS INITIAL.

*//Modify..04/04/2011..T00020..
    CASE p_amt_type.
      WHEN 'A'.
        l_source_value = ld_wa_db_price-togbtr.
      WHEN OTHERS.  "fixed
        l_source_value = ld_wa_db_price-tofbtr.
    ENDCASE.

    l_destin_value = l_source_value * it_shop_sum-wip_pqty
                                     / ld_wa_db_keko-losgr.
    l_value = p_amt * l_destin_value / l_source_value.

    CASE p_amt_type.
      WHEN 'A'.
        it_shop_cc-wip_pamt   = l_value.
      WHEN OTHERS.
        it_shop_cc-wip_pamt_f = l_value.
    ENDCASE.
*    PERFORM make_value USING    'B'
*                                p_amt
*                                p_amt_type
*                                it_shop_sum-wip_pqty.
*//
    PERFORM move_sum_header_to_cc.

    COLLECT it_shop_cc . CLEAR it_shop_cc.

  ENDIF.

ENDFORM.                    " collect_shop_cc_p
*&---------------------------------------------------------------------*
*&      Form  make_value
*&---------------------------------------------------------------------*
FORM make_value USING  f_type
                       f_amt
                       f_amt_type
                       f_qty.
  DATA: l_source_value     TYPE ckmlcr-salk3,
        l_destin_value     TYPE ckmlcr-salk3.

  DATA : l_field_name(25).

  PERFORM make_l_field USING f_type
                             f_amt_type
                       CHANGING l_field_name.

*Scale up/down - andy
* refer: function 'MLCCS_SCALE_UPDOWN'
*  - destin_value = price * qty / (peinh or losgr)
*  - new cc = cc value * destin value / source value
*  - still rounding problem ; GR itemization (activity)

  IF f_amt_type = 'F'.  "fixed
    l_source_value = ld_wa_db_price-tofbtr.
    l_destin_value = l_source_value * f_qty / ld_wa_db_keko-losgr.
  ELSE.
    l_source_value = ld_wa_db_price-togbtr.
    l_destin_value = l_source_value * f_qty / ld_wa_db_keko-losgr.
  ENDIF.

  ASSIGN (l_field_name)  TO   <f_field2> .
* <f_field2> = f_amt * f_qty / ld_wa_db_keko-losgr.
  <f_field2> = f_amt * l_destin_value / l_source_value.

ENDFORM.                    " make_value
*&---------------------------------------------------------------------*
*&      Form  standard_make_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM standard_make_value.
*  CLEAR it_fsc_mat.
*  READ TABLE it_fsc_mat WITH KEY objnr = it_shop_sum-objnr.
*
*  CLEAR lt_ck13n.
*  READ TABLE lt_ck13n WITH KEY matnr    = it_fsc_mat-matnr
*                               resou    = it_shop_sum-resou.
*
*
*  CHECK NOT lt_ck13n-gr_qty IS INITIAL.
*  g_std = 'X'.
*  PERFORM make_element_value_std .
*
*  CLEAR g_std.
*
*ENDFORM.                    " standard_make_value
**&---------------------------------------------------------------------
*
**&      Form  collect_FSC_STD_CC
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_L_AMT  text
**----------------------------------------------------------------------
*
*FORM collect_FSC_STD_CC USING    p_amt.
*
*
*  PERFORM make_value_std USING    'B'
*                                  p_amt
*                                  lt_ck13n-gr_qty.
*
*  PERFORM make_value_std USING    'E'
*                                  p_amt
*                                  lt_ck13n-gr_qty.
*
*  PERFORM move_sum_header_to_cc.
*  COLLECT FSC_STD_CC . CLEAR FSC_STD_CC.
*
*ENDFORM.                    " collect_FSC_STD_CC
**&---------------------------------------------------------------------
*
**&      Form  make_value_std
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_1250   text
**      -->P_LT_CK13N_GR_QTY  text
**----------------------------------------------------------------------
*
*FORM make_value_std USING   p_type
*                       p_amt
*                       p_qty.
*
*  PERFORM make_l_field USING p_type
*                       CHANGING l_field_name
*                                l_field_name_f.
*
*  ASSIGN (l_field_name)  TO   <f_field2> .
*  <f_field2> = p_amt * p_qty / ld_wa_db_keko-losgr.
*ENDFORM.                    " make_value_std
*&---------------------------------------------------------------------*
*&      Form  collect_shop_cc_Activity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_AMT  text
*      -->P_L_AMT_TYPE  text
*----------------------------------------------------------------------*
FORM collect_shop_cc_activity USING p_amt
                                    p_amt_type .


*.
** Current
*  IF NOT it_shop_sum-mbgbtr IS INITIAL.
*    PERFORM make_value USING  'C'
*                              it_shop_sum-mbgbtr.
*  ENDIF.
*
** Current - var
*  IF NOT it_shop_sum-mbgbtr2 IS INITIAL.
*    PERFORM make_value USING  'V'
*                              it_shop_sum-mbgbtr2.
*  ENDIF.
*
** GR
*  IF NOT it_shop_sum-gr_qty IS INITIAL.
*    PERFORM make_value USING  'G'
*                              it_shop_sum-gr_qty.
*  ENDIF.
*
** Scrap
*  IF NOT it_shop_sum-scrap_qty IS INITIAL.
*    PERFORM make_value USING  'S'
*                              it_shop_sum-scrap_qty.
*  ENDIF.
*
** WIP - begining
*  IF NOT it_shop_sum-wip_pqty IS INITIAL.
*    PERFORM make_value USING    'B'
*                                it_shop_sum-wip_pqty.
*  ENDIF.
*
** WIP - ending
*  IF NOT it_shop_sum-wip_qty IS INITIAL.
*    PERFORM make_value USING    'E'
*                                it_shop_sum-wip_qty.
*  ENDIF.
*
*  PERFORM move_sum_header_to_cc .
*  COLLECT it_shop_cc . CLEAR it_shop_cc.
*

ENDFORM.                    " collect_shop_cc_Activity
*&---------------------------------------------------------------------*
*&      Form  CALL_KKEK_COST_COMPONENT_ELEME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_kkek_cost_component.

  DATA l_tabix TYPE sy-tabix.

*//Modify..03/29/2011..T00020..
*  LOOP AT it_shop_sum.
*    it_kstar-kstar = it_shop_sum-kstar.
*    COLLECT it_kstar. CLEAR it_kstar.
*  ENDLOOP.
*
*  SORT it_kstar BY kstar.
*  DELETE ADJACENT DUPLICATES FROM it_kstar.
*//

*  LOOP AT it_kstar.
*    l_tabix = sy-tabix.
*
*    CALL FUNCTION 'KKEK_COST_COMPONENT_ELEMENT'
*      EXPORTING
*        elehk_imp         = 'K1'
*        ktopl_imp         = 'K201'
*        kstar_imp         = it_kstar-kstar
*        message_on_screen = space
*      IMPORTING
*        elemt_exp         = it_kstar-elemt
*      EXCEPTIONS
*        calling_error     = 1
*        OTHERS            = 2.
*
*    MODIFY it_kstar INDEX l_tabix.
*    CLEAR  it_kstar.
*
*  ENDLOOP.

ENDFORM.                    " CALL_KKEK_COST_COMPONENT_ELEME
*&---------------------------------------------------------------------*
*&      Form  scale_cc_to_sum
*&---------------------------------------------------------------------*
FORM scale_cc_to_sum.


  SORT it_shop_sum  BY typps kstar resou aufnr .
*  SORT it_res_ccsum BY typps kstar resou aufnr .

  REFRESH lt_shop_cc.
  INSERT LINES OF it_shop_cc INTO TABLE lt_shop_cc.
  REFRESH it_shop_cc.

  LOOP AT it_res_ccsum.
*    MOVE-CORRESPONDING it_res_ccsum TO it_shop_cc.


    IF it_res_ccsum-resou = p_resou AND p_debug = 'X'.
      BREAK-POINT.
    ENDIF.

    CLEAR it_kstar .
    READ TABLE it_kstar WITH TABLE KEY kstar = it_res_ccsum-kstar.

    CLEAR it_shop_sum.
    READ TABLE it_shop_sum WITH KEY typps   = it_res_ccsum-typps
                                    kstar   = it_res_ccsum-kstar
                                    resou   = it_res_ccsum-resou
                                    aufnr   = it_res_ccsum-aufnr
                           BINARY SEARCH.
    CHECK sy-subrc = 0.
    READ TABLE lt_shop_cc WITH KEY typps      =   it_shop_sum-typps
                                   kstar      =   it_shop_sum-kstar
                                   resou      =   it_shop_sum-resou
                                   elemt      =   it_kstar-elemt
                                   aufnr      =   it_shop_sum-aufnr
                                   artnr      =   it_shop_sum-artnr
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      PERFORM calculate_rounding_diff.
      MODIFY lt_shop_cc INDEX sy-tabix.
    ELSE.
      CLEAR lt_shop_cc.
      lt_shop_cc-artnr      =   it_shop_sum-artnr.
      lt_shop_cc-aufnr      =   it_shop_sum-aufnr.
      lt_shop_cc-typps      =   it_shop_sum-typps.
      lt_shop_cc-kstar      =   it_shop_sum-kstar.
      lt_shop_cc-resou      =   it_shop_sum-resou.
      lt_shop_cc-elemt      =   it_kstar-elemt.
      PERFORM calculate_rounding_diff.

      INSERT lt_shop_cc INTO TABLE lt_shop_cc.

*      it_shop_cc-artnr      =   it_shop_sum-artnr.
*      it_shop_cc-aufnr      =   it_shop_sum-aufnr.
*      it_shop_cc-typps      =   it_shop_sum-typps.
*      it_shop_cc-kstar      =   it_shop_sum-kstar.
*      it_shop_cc-resou      =   it_shop_sum-resou.
**-----use cc from CE, use same resource type
*      it_shop_cc-elemt      =   it_kstar-elemt.
**         it_shop_cc-typps      =   c_typ_round.
*
*      COLLECT it_shop_cc. CLEAR it_shop_cc.
    ENDIF.

    DELETE it_res_ccsum.
  ENDLOOP.
  INSERT LINES OF lt_shop_cc INTO TABLE it_shop_cc.
  REFRESH lt_shop_cc.
ENDFORM.                    " scale_cc_to_sum
*&---------------------------------------------------------------------*
*&      Form  calculate_rounding_diff
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_rounding_diff.

*  it_shop_cc-wkgbtr      =  it_shop_sum-wkgbtr
*                            - it_res_ccsum-wkgbtr.
*  it_shop_cc-wkgbtr2     =  it_shop_sum-wkgbtr2
*                            - it_res_ccsum-wkgbtr2.
*  it_shop_cc-add_wkgbtr  =  it_shop_sum-add_wkgbtr
*                            - it_res_ccsum-add_wkgbtr.
*  it_shop_cc-wip_amt     =  it_shop_sum-wip_amt
*                            - it_res_ccsum-wip_amt.
*  it_shop_cc-wip_pamt    =  it_shop_sum-wip_pamt
*                            - it_res_ccsum-wip_pamt.
*  it_shop_cc-scrap_amt   =  it_shop_sum-scrap_amt
*                            - it_res_ccsum-scrap_amt.
*  it_shop_cc-manu_amt    =  it_shop_sum-manu_amt
*                            - it_res_ccsum-manu_amt.
*  it_shop_cc-gr_amt      =  it_shop_sum-gr_amt
*                            - it_res_ccsum-gr_amt.
*  it_shop_cc-single_amt  =  it_shop_sum-single_amt
*                            - it_res_ccsum-single_amt.
*  it_shop_cc-multi_amt   =  it_shop_sum-multi_amt
*                            - it_res_ccsum-multi_amt.
*  it_shop_cc-multi_samt  =  it_shop_sum-multi_samt -
*                            it_res_ccsum-multi_samt.
*  it_shop_cc-multi_mamt  =  it_shop_sum-multi_mamt -
*                            it_res_ccsum-multi_mamt.

  ADD      it_shop_sum-wkgbtr  TO   lt_shop_cc-wkgbtr.
  SUBTRACT it_res_ccsum-wkgbtr FROM lt_shop_cc-wkgbtr.

  ADD      it_shop_sum-wkgbtr2  TO   lt_shop_cc-wkgbtr2.
  SUBTRACT it_res_ccsum-wkgbtr2 FROM lt_shop_cc-wkgbtr2.

  ADD      it_shop_sum-add_wkgbtr  TO   lt_shop_cc-add_wkgbtr.
  SUBTRACT it_res_ccsum-add_wkgbtr FROM lt_shop_cc-add_wkgbtr.

  ADD      it_shop_sum-wip_amt  TO   lt_shop_cc-wip_amt.
  SUBTRACT it_res_ccsum-wip_amt FROM lt_shop_cc-wip_amt.

  ADD      it_shop_sum-wip_pamt  TO   lt_shop_cc-wip_pamt.
  SUBTRACT it_res_ccsum-wip_pamt FROM lt_shop_cc-wip_pamt.

  ADD      it_shop_sum-scrap_amt  TO   lt_shop_cc-scrap_amt.
  SUBTRACT it_res_ccsum-scrap_amt FROM lt_shop_cc-scrap_amt.

  ADD      it_shop_sum-manu_amt  TO   lt_shop_cc-manu_amt.
  SUBTRACT it_res_ccsum-manu_amt FROM lt_shop_cc-manu_amt.

  ADD      it_shop_sum-gr_amt  TO   lt_shop_cc-gr_amt.
  SUBTRACT it_res_ccsum-gr_amt FROM lt_shop_cc-gr_amt.

  ADD      it_shop_sum-single_amt  TO   lt_shop_cc-single_amt.
  SUBTRACT it_res_ccsum-single_amt FROM lt_shop_cc-single_amt.

  ADD      it_shop_sum-multi_amt  TO   lt_shop_cc-multi_amt.
  SUBTRACT it_res_ccsum-multi_amt FROM lt_shop_cc-multi_amt.

  ADD      it_shop_sum-multi_samt  TO   lt_shop_cc-multi_samt.
  SUBTRACT it_res_ccsum-multi_samt FROM lt_shop_cc-multi_samt.

  ADD      it_shop_sum-multi_mamt  TO   lt_shop_cc-multi_mamt.
  SUBTRACT it_res_ccsum-multi_mamt FROM lt_shop_cc-multi_mamt.

ENDFORM.                    " calculate_rounding_diff
*&---------------------------------------------------------------------*
*&      Form  calculate_rounding_diff2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_rounding_diff2.

  it_shop_cc-wkgbtr      =  it_shop_sum-wkgbtr
                            - it_shop_cc-wkgbtr.
  it_shop_cc-wkgbtr2     =  it_shop_sum-wkgbtr2
                            - it_shop_cc-wkgbtr2.
  it_shop_cc-add_wkgbtr  =  it_shop_sum-add_wkgbtr
                            - it_shop_cc-add_wkgbtr.
  it_shop_cc-wip_amt     =  it_shop_sum-wip_amt
                            - it_shop_cc-wip_amt.
  it_shop_cc-wip_pamt    =  it_shop_sum-wip_pamt
                            - it_shop_cc-wip_pamt.
  it_shop_cc-scrap_amt   =  it_shop_sum-scrap_amt
                            - it_shop_cc-scrap_amt.
  it_shop_cc-manu_amt    =  it_shop_sum-manu_amt
                            - it_shop_cc-manu_amt.
  it_shop_cc-gr_amt      =  it_shop_sum-gr_amt
                            - it_shop_cc-gr_amt.
  it_shop_cc-single_amt  =  it_shop_sum-single_amt
                            - it_shop_cc-single_amt.
  it_shop_cc-multi_amt   =  it_shop_sum-multi_amt
                            - it_shop_cc-multi_amt.
  it_shop_cc-multi_samt  =  it_shop_sum-multi_samt -
                            it_shop_cc-multi_samt.
  it_shop_cc-multi_mamt  =  it_shop_sum-multi_mamt -
                            it_shop_cc-multi_mamt.

ENDFORM.                    " calculate_rounding_diff2
*&---------------------------------------------------------------------*
*&      Form  call_mlccs_read_pr_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mlccs_read_pr_fsc.
  RANGES :ir_prtyp FOR mlprkeph-prtyp,
          ir_curtp FOR tkel-curtp.
* Commented out by Han Moon

**  SORT it_mat BY matnr werks.
*
**  loop at it_shop_sum.
**    temp_shop-artnr = it_shop_sum-artnr.
**    temp_shop-aufnr = it_shop_sum-aufnr.
**    temp_shop-werks = it_shop_sum-par_werks.
**    temp_shop-verid = it_shop_sum-verid.
**    temp_shop-objnr = it_shop_sum-objnr.
**    collect temp_shop. clear temp_shop.
**  endloop.
**  sort temp_shop.
**  loop at temp_shop .
**    clear it_mat.
**    read table it_mat with key matnr = temp_shop-artnr
**                               werks = temp_shop-werks
**                              binary search.
**    check sy-subrc = 0 .
**    lt_kalnr2-kalnr  = it_mat-kaln1 .
**    collect lt_kalnr2.  clear lt_kalnr2.
**  endloop.
*
*
**//Modify..03/29/2011..T00020..
**  LOOP AT it_fsc_mat.
**    lt_kalnr2-kalnr  = it_fsc_mat-kaln1.
**    APPEND lt_kalnr2.
**  ENDLOOP.
**//
*
*  ir_prtyp = 'IEQS'.
*  APPEND ir_prtyp.
*  ir_curtp = 'IEQ10'.
*  APPEND ir_curtp.
*
*  CALL FUNCTION 'MLCCS_READ_PR'
*    EXPORTING
*      i_use_buffer            = space
*      i_bdatj_1               = p_bdatj
*      i_poper_1               = p_perab
*    IMPORTING
*      et_prkeko               = it_prkeko_fsc_temp
*      et_prkeph               = it_prkeph_fsc_temp
*    TABLES
*      it_kalnr                = lt_kalnr2
*      ir_prtyp                = ir_prtyp
*      ir_curtp                = ir_curtp
*    EXCEPTIONS
*      no_data_found           = 1
*      input_data_inconsistent = 2
*      OTHERS                  = 3.
*
*
*  it_prkeko_fsc[] = it_prkeko_fsc_temp[].
*  it_prkeph_fsc[] = it_prkeph_fsc_temp[].
*
**//Modify..04/27/2011..T00020..
** Previous period..
*  CALL FUNCTION 'MLCCS_READ_PR'
*    EXPORTING
*      i_use_buffer            = space
*      i_bdatj_1               = g_pr_lfgja
*      i_poper_1               = g_pr_lfmon
*    IMPORTING
*      et_prkeko               = it_prkeko_fsc_temp
*      et_prkeph               = it_prkeph_fsc_temp
*    TABLES
*      it_kalnr                = lt_kalnr2
*      ir_prtyp                = ir_prtyp
*      ir_curtp                = ir_curtp
*    EXCEPTIONS
*      no_data_found           = 1
*      input_data_inconsistent = 2
*      OTHERS                  = 3.
*
*
*  it_prkeko_fsc_p[] = it_prkeko_fsc_temp[].
*  it_prkeph_fsc_p[] = it_prkeph_fsc_temp[].
**//

ENDFORM.                    " call_mlccs_read_pr_fsc
*&---------------------------------------------------------------------*
*&      Form  get_sum_prkeko_prkeph
*&---------------------------------------------------------------------*
*form get_sum_prkeko_prkeph.
*
*  clear it_mat.
*  read table it_mat with key matnr = it_shop_sum-artnr
*                             werks = it_shop_sum-par_werks
*                             binary search.
*
*  check sy-subrc = 0 .
*  ld_wa_db_price-togbtr = it_mat-stprs.  "total
*  ld_wa_db_price-tofbtr = it_mat-stprs.  "fixed
*
*  clear it_prkeko.
*  read table it_prkeko_fsc with key kalnr = it_mat-kaln1
*                                binary search.
*  check sy-subrc = 0 .
*  move-corresponding it_prkeko_fsc to ld_wa_db_keko .
*
*
*  clear it_prkeph_fsc.
*  read table it_prkeph_fsc with key kalnr = it_mat-kaln1
*                                    kkzst = ''
*                                  binary search.
*  check sy-subrc = 0 .
*  move-corresponding it_prkeph_fsc to ld_wa_db_keph .
*
*  perform make_element_value.
*
*endform.                    " get_sum_prkeko_prkeph
*&---------------------------------------------------------------------*
*&      Form  make_batch_log
*&---------------------------------------------------------------------*
FORM make_batch_log USING f_matnr p_flag.
* CHECK g_batch = 'X'.
  IF p_flag = 'D'.
    DELETE FROM ztco_batch_log WHERE repid = 'SHOP_ACT'
                                 AND kokrs = p_kokrs
                                 AND bdatj = p_bdatj
                                 AND poper = p_perab
                                 AND matnr = f_matnr.
  ELSE.
    CLEAR : it_log, it_log[].
    it_log-kokrs = p_kokrs.
    it_log-bdatj = p_bdatj.
    it_log-poper = p_perab.
    it_log-matnr = f_matnr.
    it_log-repid = 'SHOP_ACT'.
    it_log-flag  = p_flag.

    it_log-erdat = sy-datum.
    it_log-erzet = sy-uzeit.
    it_log-ernam = sy-uname.

    APPEND it_log. CLEAR it_log.
    MODIFY ztco_batch_log FROM TABLE it_log.
    REFRESH it_log.
  ENDIF.
  COMMIT WORK.

ENDFORM.                    " make_batch_log
*&---------------------------------------------------------------------*
*&      Form  check_obligatory_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_obligatory_field.
  IF p_kokrs IS INITIAL.
    MESSAGE s000 WITH 'Please input Controlling area. '.
  ENDIF.

  IF p_bdatj IS INITIAL.
    MESSAGE s000 WITH 'Please input Year. '.
  ENDIF.

  IF p_perab IS INITIAL.
    MESSAGE s000 WITH 'Please input Period. '.
  ENDIF.

*//Modify..04/07/2011..T00020..
  REFRESH : r_budat. CLEAR : r_budat.
  r_budat = 'IBT'.
  CONCATENATE p_bdatj p_perab+1(2) '01' INTO r_budat-low.
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_bdatj
      i_periv        = 'K4'
      i_poper        = p_perab
    IMPORTING
      e_date         = r_budat-high
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  APPEND r_budat.
*//

ENDFORM.                    " check_obligatory_field
*&---------------------------------------------------------------------*
*&      Form  get_ml_single_cc
*&---------------------------------------------------------------------*
FORM get_ml_single_cc.
  DATA: lt_cc_amt LIKE gt_cc_amt OCCURS 0 WITH HEADER LINE.

  LOOP AT it_fsc_mat.

    CLEAR it_ckmlmv001.
    READ TABLE it_ckmlmv001 WITH KEY pmatn_nd = it_fsc_mat-matnr
                                     verid_nd = it_fsc_mat-verid.

    CLEAR it_ckmlkeph.
    READ TABLE it_ckmlkeph  WITH KEY
               kalnr = it_fsc_mat-kaln1
               categ = 'ZU'
               mlcct = 'E'
               bvalt = it_ckmlmv001-kalnr
         BINARY SEARCH.

    CHECK sy-subrc = 0 .
    MOVE-CORRESPONDING it_ckmlkeph TO ld_wa_db_keph .

*//Modify..04/04/2011..T00020..
    READ TABLE gt_cc_amt_cur WITH KEY
*                                gubun = 'CKMLKEPH'
                                kalnr = ld_wa_db_keph-kalnr
                                bdatj = p_bdatj
                                poper = p_perab
                                kkzst = ld_wa_db_keph-kkzst
                                BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_cc_amt_cur FROM sy-tabix.
        IF "gt_cc_amt_new-gubun <> 'CKMLKEPH'          OR
           gt_cc_amt_cur-kalnr <> ld_wa_db_keph-kalnr OR
           gt_cc_amt_cur-bdatj <> p_bdatj             OR
           gt_cc_amt_cur-poper <> p_perab             OR
           gt_cc_amt_cur-kkzst <> ld_wa_db_keph-kkzst.
          EXIT.
        ENDIF.

        it_ml_scale-aufnr      = it_fsc_mat-aufnr.
        it_ml_scale-artnr      = it_fsc_mat-matnr.
        it_ml_scale-elemt      = gt_cc_amt_cur-elemt.
        it_ml_scale-diff_amt   = gt_cc_amt_cur-dmbtr.
        it_ml_scale-diff_amt_f = gt_cc_amt_cur-dmbtr_f.

        COLLECT it_ml_scale. CLEAR it_ml_scale.
      ENDLOOP.
    ENDIF.

*    PERFORM extract_cc_amt TABLES lt_cc_amt
*                           USING  ld_wa_db_keph.
*
*    LOOP AT lt_cc_amt.
*
*      it_ml_scale-aufnr        = it_fsc_mat-aufnr.
*      it_ml_scale-artnr        = it_fsc_mat-matnr.
*      it_ml_scale-elemt        = lt_cc_amt-elemt.
*      it_ml_scale-single_amt   = lt_cc_amt-dmbtr.
*      it_ml_scale-single_amt_f = lt_cc_amt-dmbtr_f.
*
*      COLLECT it_ml_scale. CLEAR it_ml_scale.
*    ENDLOOP.
*//

  ENDLOOP.

ENDFORM.                    " get_ml_single_cc
*&---------------------------------------------------------------------*
*&      Form  make_resource
*&---------------------------------------------------------------------*
FORM make_resou USING    f_m_e
                         f_one
                         f_two
                CHANGING f_result.

  CLEAR f_result.
  IF f_m_e = 'M'.
    f_result(4)    = f_one.
    f_result+5(18) = f_two.
  ELSE.
    f_result(10)   = f_one.
    f_result+11(6) = f_two.
  ENDIF.
ENDFORM.                    " make_resource
*&---------------------------------------------------------------------*
*&      Form  add_it_mat_tmp
*&---------------------------------------------------------------------*
FORM add_it_mat_tmp USING    f_matnr TYPE matnr
                             f_bwkey TYPE bwkey
                    CHANGING f_subrc LIKE sy-subrc.

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_mat_tmp
           FROM ( mbew  INNER JOIN marc
                           ON  mbew~matnr = marc~matnr
                           AND mbew~bwkey = marc~werks
                        INNER JOIN mara
                           ON mara~matnr   = marc~matnr )
           WHERE mbew~matnr = f_matnr
             AND mbew~bwkey = f_bwkey.

  IF sy-subrc = 0.
    SORT it_mat_tmp BY matnr bwkey bwtar.
    f_subrc = 0.
  ELSE.
    f_subrc = 4.
  ENDIF.

ENDFORM.                    " add_it_mat_tmp
*&---------------------------------------------------------------------*
*&      Form  get_fsc_std_cc
*&---------------------------------------------------------------------*
FORM get_fsc_std_cc.

  DATA: lt_cc_amt LIKE gt_cc_amt OCCURS 0 WITH HEADER LINE.

*  SORT it_mat BY matnr.

  LOOP AT it_fsc_gr.
    PERFORM check_it_mat USING it_fsc_gr-matnr
                               it_fsc_gr-werks
                         CHANGING sy-subrc.
*    READ TABLE it_mat WITH KEY matnr = it_fsc_gr-matnr
*                               BINARY SEARCH.

    CHECK sy-subrc = 0 .
    ld_wa_db_price-togbtr = it_mat-stprs.  "total
    ld_wa_db_price-tofbtr = it_mat-stprs.  "fixed

    CLEAR it_prkeko.
* Replaced by Han Moon
*    READ TABLE it_prkeko_fsc WITH KEY kalnr = it_mat-kaln1
*                             BINARY SEARCH.
*    CHECK sy-subrc = 0 .
*    CLEAR : ld_wa_db_keko,ld_wa_db_keph.
*    MOVE-CORRESPONDING it_prkeko_fsc TO ld_wa_db_keko .
    READ TABLE it_prkeko WITH TABLE KEY kalnr = it_mat-kaln1
                                        bdatj = p_bdatj
                                        poper = p_perab.
    CHECK sy-subrc = 0 .
    CLEAR : ld_wa_db_keko,ld_wa_db_keph.
    MOVE-CORRESPONDING it_prkeko TO ld_wa_db_keko .
    CLEAR it_prkeph.
* Replaced by Han Moon
*    READ TABLE it_prkeph_fsc WITH KEY kalnr = it_mat-kaln1
*                                      kkzst = ''
*                             BINARY SEARCH.
*    CHECK sy-subrc = 0 .
*    MOVE-CORRESPONDING it_prkeph_fsc TO ld_wa_db_keph .
    READ TABLE it_prkeph WITH TABLE KEY kalnr = it_mat-kaln1
                                        bdatj = p_bdatj
                                        poper = p_perab
                                        kkzst = ''.
    CHECK sy-subrc = 0 .
    MOVE-CORRESPONDING it_prkeph TO ld_wa_db_keph .
*//Modify..04/04/2011..T00020..
    READ TABLE gt_cc_amt_new WITH KEY
*                                gubun = 'PRKEPH_FSC'
                                kalnr = ld_wa_db_keph-kalnr
                                bdatj = p_bdatj
                                poper = p_perab
                                kkzst = ld_wa_db_keph-kkzst
                                BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_cc_amt_new FROM sy-tabix.
        IF "gt_cc_amt_new-gubun <> 'PRKEPH_FSC'        OR
           gt_cc_amt_new-kalnr <> ld_wa_db_keph-kalnr OR
           gt_cc_amt_new-bdatj <> p_bdatj             OR
           gt_cc_amt_new-poper <> p_perab             OR
           gt_cc_amt_new-kkzst <> ld_wa_db_keph-kkzst.
          EXIT.
        ENDIF.

        CLEAR fsc_std_cc.
        fsc_std_cc-artnr   = it_fsc_gr-matnr.
        fsc_std_cc-elemt   = gt_cc_amt_new-elemt.
        fsc_std_cc-dmbtr   = gt_cc_amt_new-dmbtr.
        fsc_std_cc-dmbtr_f = gt_cc_amt_new-dmbtr_f.
* Replaced by Han Moon
*        APPEND fsc_std_cc.
        COLLECT fsc_std_cc.

*      PERFORM extract_cc_amt TABLES lt_cc_amt
*                           USING  ld_wa_db_keph.
*      LOOP AT lt_cc_amt.
*        CLEAR fsc_std_cc.
*        fsc_std_cc-artnr   = it_fsc_gr-matnr.
*        fsc_std_cc-elemt   = lt_cc_amt-elemt.
*        fsc_std_cc-dmbtr   = lt_cc_amt-dmbtr.
*        fsc_std_cc-dmbtr_f = lt_cc_amt-dmbtr_f.
*        APPEND fsc_std_cc.

*//Modify..04/01/2011..T00020..
        MOVE-CORRESPONDING fsc_std_cc TO fsc_std_ccsum.
        COLLECT fsc_std_ccsum . CLEAR fsc_std_ccsum.

      ENDLOOP.

    ENDIF.
*//

*//Modify..04/27/2011..T00020..
    CLEAR it_prkeph.
* Replaced by Han Moon
*    READ TABLE it_prkeph_fsc_p WITH KEY kalnr = it_mat-kaln1
*                                        kkzst = ''
*                             BINARY SEARCH.
*    CHECK sy-subrc = 0 .
*    MOVE-CORRESPONDING it_prkeph_fsc_p TO ld_wa_db_keph .
    READ TABLE it_prkeph WITH TABLE KEY kalnr = it_mat-kaln1
                                        bdatj = g_pr_lfgja
                                        poper = g_pr_lfmon
                                        kkzst = ''.
    CHECK sy-subrc = 0 .
    MOVE-CORRESPONDING it_prkeph TO ld_wa_db_keph .
    READ TABLE gt_cc_amt_new WITH KEY
*                                gubun = 'PRKEPH_FSC_P'
                                kalnr = ld_wa_db_keph-kalnr
                                bdatj = g_pr_lfgja
                                poper = g_pr_lfmon
                                kkzst = ld_wa_db_keph-kkzst
                                BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_cc_amt_new FROM sy-tabix.
        IF "gt_cc_amt_new-gubun <> 'PRKEPH_FSC_P'      OR
           gt_cc_amt_new-kalnr <> ld_wa_db_keph-kalnr OR
           gt_cc_amt_new-bdatj <> g_pr_lfgja          OR
           gt_cc_amt_new-poper <> g_pr_lfmon          OR
           gt_cc_amt_new-kkzst <> ld_wa_db_keph-kkzst.
          EXIT.
        ENDIF.

* Commented out by Han Moon
*        CLEAR fsc_std_cp.
*        fsc_std_cp-artnr   = it_fsc_gr-matnr.
*        fsc_std_cp-elemt   = gt_cc_amt_new-elemt.
*        fsc_std_cp-dmbtr   = gt_cc_amt_new-dmbtr.
*        fsc_std_cp-dmbtr_f = gt_cc_amt_new-dmbtr_f.
*        APPEND fsc_std_cp.

        CLEAR fsc_std_ccsum.
        fsc_std_ccsum-artnr    = it_fsc_gr-matnr.
* Replaced by Han Moon
*        fsc_std_ccsum-dmbtr_p  = fsc_std_cp-dmbtr.
*        fsc_std_ccsum-dmbtr_pf = fsc_std_cp-dmbtr_f.
        fsc_std_ccsum-dmbtr_p  = gt_cc_amt_new-dmbtr.
        fsc_std_ccsum-dmbtr_pf = gt_cc_amt_new-dmbtr_f.
        COLLECT fsc_std_ccsum .

      ENDLOOP.

    ENDIF.

  ENDLOOP.


*//Modify..04/01/2011..T00020..
** std component total
*  LOOP AT fsc_std_cc.
*    MOVE-CORRESPONDING fsc_std_cc TO fsc_std_ccsum.
*    COLLECT fsc_std_ccsum . CLEAR fsc_std_ccsum.
*  ENDLOOP.
*//

ENDFORM.                    " get_fsc_std_cc
*&---------------------------------------------------------------------*
*&      Form  get_ml_customizing
*&---------------------------------------------------------------------*
FORM get_ml_customizing.

*  call function 'MLCCS_READ_CUSTOMIZING'
*       exporting
*            i_bwkey              = mlkey-bwkey
*            i_call_by_startup    = ' '
*       importing
*            ef_mlccs_customizing = gs_mlccs_customizing
*       exceptions
*            mlccs_not_active     = 1
*            others               = 2.
* CALL FUNCTION 'MLCCS_GET_LAYOUT'
*      EXPORTING
*           i_bwkey    = i_bwkey
*           i_bdatj    = i_bdatj
*           i_poper    = i_poper
*           i_untper   = i_untper
*           i_curtp    = i_destin_curtp
*      IMPORTING
*           ef_ckmlkev = lf_layout.
*IF i_keart EQ ccs00_keart-haupt.
*  lh_layout = lf_layout-elehk.
*ELSE.
*  lh_layout = lf_layout-elehkns.
*ENDIF.
*lf_costelement_view-besbw = y_x.
*call function 'CK_F_TCKH4_HIERARCHY_READING'
*     exporting
*          p_elehk          = lh_elehk
*          f_tckh8_standard = ls_tckh8
*     tables
*          t_tckh3          = gt_tckh3
*          t_tckh1          = gt_tckh1
*          t_tckh6          = gt_tckh6.

  REFRESH it_tckh3.
  SELECT *
    INTO TABLE it_tckh3
    FROM tckh3
   WHERE elehk = 'H1'.

  SORT it_tckh3 BY elemt.

  LOOP AT it_tckh3.
    IF it_tckh3-el_hv IS NOT INITIAL.
      CLEAR it_elemt.
      it_elemt-elnum = it_tckh3-el_hv.
      it_elemt-elemt = it_tckh3-elemt.
      it_elemt-eltyp = 'V'.
      INSERT it_elemt INTO TABLE it_elemt.
    ENDIF.
    IF it_tckh3-el_hf IS NOT INITIAL.
      CLEAR it_elemt.
      it_elemt-elnum = it_tckh3-el_hf.
      it_elemt-elemt = it_tckh3-elemt.
      it_elemt-eltyp = 'F'.
      INSERT it_elemt INTO TABLE it_elemt.
    ENDIF.
  ENDLOOP.
  SORT it_elemt BY elnum.
ENDFORM.                    " get_ml_customizing
*&---------------------------------------------------------------------*
*&      Form  PD_SCALE
*&---------------------------------------------------------------------*
FORM pd_scale USING value(i_zielpreis) TYPE ckml_preis_neu
                    value(i_zielmenge) TYPE mlccs_d_menge.

  DATA: gt_ckmlprkeph     TYPE mlccs_t_prkeph,
        g_losgr       TYPE ck_losgr,
        g_peinh       TYPE peinh.



  DATA: ls_ckmlprkeph      TYPE ckmlprkeph,
        ls_cost_components TYPE mlccs_s_cost_components.

  LOOP AT gt_ckmlprkeph INTO ls_ckmlprkeph.
    IF  NOT i_zielpreis IS INITIAL
    AND NOT g_peinh     IS INITIAL
    AND NOT g_losgr     IS INITIAL.
      MOVE-CORRESPONDING ls_ckmlprkeph TO ls_cost_components.
      IF NOT ls_cost_components IS INITIAL.

        CALL FUNCTION 'MLCCS_SCALE_UPDOWN_QUANTITY'
          EXPORTING
            i_losgr                 = g_losgr  "lot size
            i_zielpreis             = i_zielpreis  "unit price
            i_peinh                 = g_peinh  "per
            i_zielmenge             = i_zielmenge  "qty
            i_kalnr                 = ls_ckmlprkeph-kalnr
            i_curtp                 = ls_ckmlprkeph-curtp
            i_bdatj                 = ls_ckmlprkeph-bdatj
            i_poper                 = ls_ckmlprkeph-poper
            i_keart                 = ls_ckmlprkeph-keart
            i_kkzst                 = ls_ckmlprkeph-kkzst
          CHANGING
            cf_keph                 = ls_ckmlprkeph
          EXCEPTIONS
            wrong_input_data        = 1
            no_ml_header_data_found = 2
            error_in_scale_updown   = 3
            OTHERS                  = 4.

        IF sy-subrc NE 0.
          PERFORM pd_zero CHANGING ls_ckmlprkeph.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM pd_zero CHANGING ls_ckmlprkeph.
    ENDIF.
    MODIFY gt_ckmlprkeph FROM ls_ckmlprkeph.
  ENDLOOP.

ENDFORM.                               " PD_SCALE
*&---------------------------------------------------------------------*
*&      Form  PD_ZERO
*&---------------------------------------------------------------------*
FORM pd_zero CHANGING cs_ckmlprkeph TYPE ckmlprkeph.

  DATA: BEGIN OF ls_name,
          text(3)   TYPE c VALUE 'KST',
          number(3) TYPE n VALUE 0,
        END OF ls_name.

  FIELD-SYMBOLS: <c_kst> TYPE mlccs_d_kstel.

  DO.
    ADD 1 TO ls_name-number.
    ASSIGN COMPONENT ls_name OF STRUCTURE cs_ckmlprkeph TO <c_kst>.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    CLEAR <c_kst>.
  ENDDO.

ENDFORM.                               " PD_ZERO
*&---------------------------------------------------------------------*
*&      Form  collect_cc_missing
*&---------------------------------------------------------------------*
FORM collect_cc_missing.
  PERFORM move_sum_header_to_cc.

  CLEAR it_kstar .
  READ TABLE it_kstar WITH TABLE KEY kstar = it_shop_sum-kstar.

  it_shop_cc-elemt      = it_kstar-elemt.
  it_shop_cc-wkgbtr     = it_shop_sum-wkgbtr.
  it_shop_cc-wkgbtr2    = it_shop_sum-wkgbtr2.
  it_shop_cc-gr_amt     = it_shop_sum-gr_amt.
  it_shop_cc-scrap_amt  = it_shop_sum-scrap_amt.
  it_shop_cc-wip_amt    = it_shop_sum-wip_amt.
*     it_shop_cc-wip_pamt   = it_shop_sum-wip_pamt.
  it_shop_cc-target_amt = it_shop_sum-target_amt.
  it_shop_cc-control_amt = it_shop_sum-control_amt.

  COLLECT it_shop_cc. CLEAR it_shop_cc.

ENDFORM.                    " collect_cc_missing
*&---------------------------------------------------------------------*
*&      Form  collect_cc_missing
*&---------------------------------------------------------------------*
FORM collect_cc_missing_p.

  PERFORM move_sum_header_to_cc .

  CLEAR it_kstar .
  READ TABLE it_kstar WITH TABLE KEY kstar = it_shop_sum-kstar.

  it_shop_cc-elemt    = it_kstar-elemt.
  it_shop_cc-wip_pamt = it_shop_sum-wip_pamt.

  COLLECT it_shop_cc. CLEAR it_shop_cc.

ENDFORM.                    " collect_cc_missing_p

*&---------------------------------------------------------------------*
*&      Form  scale_to_ml
*&---------------------------------------------------------------------*
FORM scale_to_ml.

  DATA : ls_shop_cc LIKE ztco_shop_cc_1.

  PERFORM get_it_cc_round.

  SORT it_cc_round BY aufnr elemt.
  SORT it_ml_scale BY aufnr elemt.

  LOOP AT it_fsc_mat.

    LOOP AT it_tckh3.

      CLEAR it_cc_round.
      READ TABLE it_cc_round WITH KEY aufnr = it_fsc_mat-aufnr
                                      elemt = it_tckh3-elemt
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        ls_shop_cc-aufnr = it_ml_scale-aufnr.
        ls_shop_cc-artnr = it_ml_scale-artnr.
      ENDIF.

      CLEAR it_ml_scale.
      READ TABLE it_ml_scale WITH KEY aufnr = it_fsc_mat-aufnr
                                      elemt = it_tckh3-elemt
                                 BINARY SEARCH.
      IF sy-subrc = 0 AND ls_shop_cc-aufnr IS INITIAL.
        ls_shop_cc-aufnr = it_ml_scale-aufnr.
        ls_shop_cc-artnr = it_ml_scale-artnr.
      ENDIF.

*-----if any entry exist
      CHECK  it_cc_round-elemt <> space
          OR it_ml_scale-elemt <> space.

      ls_shop_cc-gr_amt   = it_ml_scale-gr_amt
                          - it_cc_round-gr_amt.

      ls_shop_cc-gr_amt_f = it_ml_scale-gr_amt_f
                          - it_cc_round-gr_amt_f.

      ls_shop_cc-single_amt = it_ml_scale-diff_amt
                            - ( it_cc_round-single_amt +
                                it_cc_round-multi_amt ).

      ls_shop_cc-single_amt_f = it_ml_scale-diff_amt_f
                              - ( it_cc_round-single_amt_f +
                                  it_cc_round-multi_amt_f ).


      IF  ls_shop_cc-gr_amt       = 0
      AND ls_shop_cc-gr_amt_f     = 0
      AND ls_shop_cc-single_amt   = 0
      AND ls_shop_cc-single_amt_f = 0 .
        CONTINUE.
      ELSE.

* Manuf Amount.
        ls_shop_cc-manu_amt = ls_shop_cc-gr_amt
                            + ls_shop_cc-single_amt
                            + ls_shop_cc-multi_amt.
* control amt
        ls_shop_cc-control_amt = ls_shop_cc-manu_amt.

* Manuf Amount.
        ls_shop_cc-manu_amt_f = ls_shop_cc-gr_amt_f
                              + ls_shop_cc-single_amt_f
                              + ls_shop_cc-multi_amt_f.
* control amt
        ls_shop_cc-control_amt_f = ls_shop_cc-manu_amt_f.

        ls_shop_cc-typps = c_typ_round.
        ls_shop_cc-elemt = it_tckh3-elemt.

        COLLECT ls_shop_cc INTO it_shop_cc_adj.
        CLEAR   ls_shop_cc.

      ENDIF.

    ENDLOOP.

  ENDLOOP.


ENDFORM.                    " scale_to_ml
*&---------------------------------------------------------------------*
*&      Form  get_it_cc_wip_scale
*&---------------------------------------------------------------------*
FORM get_it_cc_wip_scale.

** Actual cost FOR ROUNDING error correction
  LOOP AT it_shop_cc.

    MOVE-CORRESPONDING it_shop_cc TO it_cc_wip_scale.
*    clear : it_cc_wip_scale-resou,
*            it_cc_wip_scale-typps, it_cc_wip_scale-kstar.
    COLLECT it_cc_wip_scale .
    CLEAR it_cc_wip_scale.

  ENDLOOP.

ENDFORM.                    " get_it_cc_wip_scale
*&---------------------------------------------------------------------*
*&      Form  get_ml_gr_cc
*&---------------------------------------------------------------------*
FORM get_ml_gr_cc.

  DATA: lt_cc_amt LIKE gt_cc_amt OCCURS 0 WITH HEADER LINE.

  SORT it_obj_gr    BY objnr werks matnr .
  SORT fsc_std_cc   BY artnr.
  SORT it_ckmlmv001 BY pmatn_nd verid_nd.
  SORT it_ckmlkeph  BY kalnr categ mlcct bvalt.

  LOOP AT it_fsc_mat.

    CLEAR : ld_wa_db_keko,ld_wa_db_keph .

    CLEAR it_obj_gr.
    READ TABLE it_obj_gr WITH KEY objnr = it_fsc_mat-objnr
                                  werks = it_fsc_mat-werks
                                  matnr = it_fsc_mat-matnr
                              BINARY SEARCH.
    IF it_obj_gr-grqty > 0.
*//Modify..04/05/2011..T00020..
      READ TABLE fsc_std_cc WITH KEY artnr = it_fsc_mat-matnr
                                     BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT fsc_std_cc FROM sy-tabix.
          IF fsc_std_cc-artnr <> it_fsc_mat-matnr.
            EXIT.
          ENDIF.
*//
          it_ml_scale-aufnr    = it_fsc_mat-aufnr.
          it_ml_scale-artnr    = it_fsc_mat-matnr.
          it_ml_scale-elemt    = fsc_std_cc-elemt.
          it_ml_scale-gr_amt   = fsc_std_cc-dmbtr   * it_obj_gr-grqty.
          it_ml_scale-gr_amt_f = fsc_std_cc-dmbtr_f * it_obj_gr-grqty.
          COLLECT it_ml_scale. CLEAR it_ml_scale.
        ENDLOOP.
      ENDIF.

    ENDIF.

*//Modify..03/31/2011..T00020..
    CLEAR it_ckmlmv001.
    READ TABLE it_ckmlmv001 WITH KEY pmatn_nd = it_fsc_mat-matnr
                                     verid_nd = it_fsc_mat-verid
                                     BINARY SEARCH.

    CLEAR it_ckmlkeph.
    READ TABLE it_ckmlkeph  WITH KEY kalnr = it_fsc_mat-kaln1
                                     categ = 'ZU'
                                     mlcct = 'E'
                                     bvalt = it_ckmlmv001-kalnr
                                     BINARY SEARCH.

    CHECK sy-subrc = 0 .
    MOVE-CORRESPONDING it_ckmlkeph TO ld_wa_db_keph .

*//Modify..04/04/2011..T00020..
    READ TABLE gt_cc_amt_cur WITH KEY
*                                gubun = 'CKMLKEPH'
                                kalnr = ld_wa_db_keph-kalnr
                                bdatj = p_bdatj
                                poper = p_perab
                                kkzst = ld_wa_db_keph-kkzst
                                BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_cc_amt_cur FROM sy-tabix.
        IF "gt_cc_amt_new-gubun <> 'CKMLKEPH'          OR
           gt_cc_amt_cur-kalnr <> ld_wa_db_keph-kalnr OR
           gt_cc_amt_cur-bdatj <> p_bdatj             OR
           gt_cc_amt_cur-poper <> p_perab             OR
           gt_cc_amt_cur-kkzst <> ld_wa_db_keph-kkzst.
          EXIT.
        ENDIF.

        it_ml_scale-aufnr        = it_fsc_mat-aufnr.
        it_ml_scale-artnr        = it_fsc_mat-matnr.
        it_ml_scale-elemt        = gt_cc_amt_cur-elemt.
        it_ml_scale-diff_amt     = gt_cc_amt_cur-dmbtr.
        it_ml_scale-diff_amt_f   = gt_cc_amt_cur-dmbtr_f.

        COLLECT it_ml_scale. CLEAR it_ml_scale.

      ENDLOOP.
    ENDIF.

*    PERFORM extract_cc_amt TABLES lt_cc_amt
*                           USING  ld_wa_db_keph.
*
*    LOOP AT lt_cc_amt.
*      it_ml_scale-aufnr        = it_fsc_mat-aufnr.
*      it_ml_scale-artnr        = it_fsc_mat-matnr.
*      it_ml_scale-elemt        = lt_cc_amt-elemt.
*      it_ml_scale-single_amt   = lt_cc_amt-dmbtr.
*      it_ml_scale-single_amt_f = lt_cc_amt-dmbtr_f.
*      COLLECT it_ml_scale. CLEAR it_ml_scale.
*    ENDLOOP.
*//

  ENDLOOP.

ENDFORM.                    " get_ml_gr_cc
*&---------------------------------------------------------------------*
*&      Form  extract_cc_amt
*&---------------------------------------------------------------------*
FORM extract_cc_amt TABLES   ft_cc_amt STRUCTURE gt_cc_amt
                    USING    f_keph    LIKE ckmlprkeph.

  REFRESH ft_cc_amt.
  CLEAR   ft_cc_amt.

  DATA : l_cnt(3) TYPE n,
         l_field(25),
         l_amt TYPE ckmlprkeph-kst001.
  CLEAR : l_cnt, l_field, l_amt.

  DO 40 TIMES VARYING l_amt FROM f_keph-kst001
                            NEXT f_keph-kst002.

    l_cnt = l_cnt + 1.
    CHECK NOT l_amt IS INITIAL.

* Overall value
    CLEAR it_elemt.
    READ TABLE it_elemt WITH TABLE KEY elnum = l_cnt.
    CHECK sy-subrc = 0.
    IF it_elemt-eltyp = 'V'.
      ft_cc_amt-dmbtr   = l_amt.
    ELSE.
      ft_cc_amt-dmbtr_f = l_amt.
    ENDIF.
    ft_cc_amt-elemt = it_elemt-elemt.
    COLLECT ft_cc_amt.  CLEAR ft_cc_amt.
  ENDDO.
  SORT ft_cc_amt BY elemt.
ENDFORM.                    " extract_cc_amt
*&---------------------------------------------------------------------*
*&      Form  check_it_mat
*&---------------------------------------------------------------------*
FORM check_it_mat USING    f_matnr
                           f_werks
                  CHANGING f_subrc.
  DATA lt_mat TYPE tt_mat.
  CLEAR it_mat.
  CHECK f_matnr IS NOT INITIAL.
  READ TABLE it_mat WITH TABLE KEY matnr = f_matnr
                                   werks = f_werks.
  IF sy-subrc EQ 0.
    f_subrc = sy-subrc.
  ELSE.
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF lt_mat
      FROM mbew AS w JOIN marc AS c
                       ON w~matnr = c~matnr
                      AND w~bwkey = c~werks
                     JOIN mara AS a
                       ON a~matnr = c~matnr
     WHERE w~matnr = f_matnr
       AND w~bwkey = f_werks.
    IF sy-subrc EQ 0.
      f_subrc = sy-subrc.
      it_mat = lt_mat.
      INSERT lt_mat INTO TABLE it_mat.
    ELSE.
      f_subrc = sy-subrc.
    ENDIF.
  ENDIF.
  f_subrc = sy-subrc.
ENDFORM.                    " check_it_mat
*&---------------------------------------------------------------------*
*&      Form  get_ckmlkeph_all
*&---------------------------------------------------------------------*
FORM get_ckmlkeph_all.

*  DATA:   lt_ckmlmv001 LIKE it_ckmlmv001 OCCURS 0 WITH HEADER LINE.

*  CLEAR : it_ckmlmv001. REFRESH it_ckmlmv001.
*
** Replaced by Han Moon => Moved to READ_OBJ_FOR_PCC
*  LOOP AT it_fsc_mat.
*    CLEAR  it_ckmlmv001.
*    it_ckmlmv001-kaln1    = it_fsc_mat-kaln1.
*    it_ckmlmv001-kalnr    = it_fsc_mat-kalnr.
*    it_ckmlmv001-pmatn_nd = it_fsc_mat-matnr.
*    it_ckmlmv001-verid_nd = it_fsc_mat-verid.
*    APPEND it_ckmlmv001.
*  ENDLOOP.


**//Modify..04/05/2011..T00020..
**-FSC material
*  REFRESH lt_ckmlmv001.
*  SELECT kalnr pmatn_nd verid_nd
*    INTO CORRESPONDING FIELDS OF TABLE lt_ckmlmv001
*    FROM ckmlmv001 CLIENT SPECIFIED BYPASSING BUFFER
*     FOR ALL ENTRIES IN it_fsc_mat
*   WHERE mandt    = sy-mandt
*     AND pmatn_nd = it_fsc_mat-matnr
*     AND verid_nd = it_fsc_mat-verid.
*
*  SORT it_fsc_mat BY matnr verid.
*
*  LOOP AT lt_ckmlmv001 INTO it_ckmlmv001.
*
*    READ TABLE it_fsc_mat WITH KEY matnr = it_ckmlmv001-pmatn_nd
*                                   verid = it_ckmlmv001-verid_nd
*                                   BINARY SEARCH.
*    IF sy-subrc = 0.
*      it_ckmlmv001-kaln1 = it_fsc_mat-kaln1.
*    ENDIF.
*
*    APPEND it_ckmlmv001.
*    CLEAR  it_ckmlmv001.
*
*  ENDLOOP.

**-FSC material
*  LOOP AT it_fsc_mat.
*
*    REFRESH lt_ckmlmv001.
*    SELECT kalnr pmatn_nd verid_nd
*      INTO CORRESPONDING FIELDS OF TABLE  lt_ckmlmv001
*      FROM ckmlmv001
**      for all entries in it_fsc_mat
*     WHERE pmatn_nd = it_fsc_mat-matnr
*       AND verid_nd = it_fsc_mat-verid.
*
*    LOOP AT lt_ckmlmv001 INTO it_ckmlmv001.
*      it_ckmlmv001-kaln1 = it_fsc_mat-kaln1.
*      APPEND it_ckmlmv001.
*    ENDLOOP.
*
*  ENDLOOP.
*//

  SORT it_ckmlmv001 BY kaln1.


  CHECK it_ckmlmv001[] IS NOT INITIAL.

  CLEAR : it_ckmlkeph. REFRESH it_ckmlkeph.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_ckmlkeph
    FROM ckmlkeph CLIENT SPECIFIED BYPASSING BUFFER
     FOR ALL ENTRIES IN it_ckmlmv001
   WHERE mandt  = sy-mandt
     AND kalnr  = it_ckmlmv001-kaln1
     AND bdatj  = p_bdatj
     AND poper  = p_perab
     AND untper = ''
     AND categ  = 'ZU'    "receipt
     AND ptyp   = 'BF'    "production
     AND bvalt  = it_ckmlmv001-kalnr
     AND kkzst  = ''.     "sum
*     AND mlcct  = 'E'.    "Added by Han Moon

**-Component material
*  select * appending corresponding fields of table  it_ckmlkeph
*    from ckmlkeph
*     for all entries in it_mat
*   where kalnr = it_mat-kaln1
*     and bdatj = p_bdatj
*     and poper = p_perab
*     and untper = ''
*     and categ = 'VN'    "consumption
*     and ptyp  = 'VF'    "production
*     and kkzst = '' .    "sum


  SORT it_ckmlkeph BY kalnr categ mlcct bvalt .

ENDFORM.                    " get_ckmlkeph_all
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_ml_single
*&---------------------------------------------------------------------*
FORM making_shop_cc_ml_single.

  DATA: lt_cc_amt LIKE gt_cc_amt OCCURS 0 WITH HEADER LINE.

  READ TABLE it_ckmlmv001 WITH KEY kaln1 = it_shop_sum-par_kalnr.

  READ TABLE it_ckmlkeph WITH KEY
                                   kalnr = it_shop_sum-chd_kalnr
                                   categ = 'VF'
                                   mlcct = 'E'
                                   bvalt = it_ckmlmv001-kalnr
                                   BINARY SEARCH.

  IF sy-subrc = 0.

    MOVE-CORRESPONDING it_ckmlkeph TO ld_wa_db_keph .

*//Modify..04/04/2011..T00020..
    READ TABLE gt_cc_amt_cur WITH KEY
*                                gubun = 'CKMLKEPH'
                                kalnr = ld_wa_db_keph-kalnr
                                bdatj = p_bdatj
                                poper = p_perab
                                kkzst = ld_wa_db_keph-kkzst
                                BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT gt_cc_amt_cur FROM sy-tabix.
        IF "gt_cc_amt_new-gubun <> 'CKMLKEPH'          OR
           gt_cc_amt_cur-kalnr <> ld_wa_db_keph-kalnr OR
           gt_cc_amt_cur-bdatj <> p_bdatj             OR
           gt_cc_amt_cur-poper <> p_perab             OR
           gt_cc_amt_cur-kkzst <> ld_wa_db_keph-kkzst.
          EXIT.
        ENDIF.

        it_shop_sum-single_amt   = gt_cc_amt_cur-dmbtr +
                                   it_shop_sum-single_amt.

        PERFORM move_sum_header_to_cc.

        it_shop_cc-elemt        = gt_cc_amt_cur-elemt.
        it_shop_cc-single_amt   = gt_cc_amt_cur-dmbtr.
        it_shop_cc-single_amt_f = gt_cc_amt_cur-dmbtr_f.

        COLLECT it_shop_cc. CLEAR it_shop_cc.

      ENDLOOP.
    ENDIF.

*    PERFORM extract_cc_amt TABLES lt_cc_amt
*                           USING  ld_wa_db_keph.
*
*    CLEAR: ztco_shop_sum_1-single_amt.
*
*    LOOP AT lt_cc_amt.
*
*      it_shop_sum-single_amt   = lt_cc_amt-dmbtr +
*                                 it_shop_sum-single_amt.
*
*      PERFORM move_sum_header_to_cc.
*
*      it_shop_cc-elemt        = lt_cc_amt-elemt.
*      it_shop_cc-single_amt   = lt_cc_amt-dmbtr.
*      it_shop_cc-single_amt_f = lt_cc_amt-dmbtr_f.
*
*      COLLECT it_shop_cc. CLEAR it_shop_cc.
*
*    ENDLOOP.
*//
    MODIFY it_shop_sum INDEX gv_shop_sum_idx
                             TRANSPORTING single_amt.

  ENDIF.

ENDFORM.                    " making_shop_cc_ml_single
*&---------------------------------------------------------------------*
*&      Form  get_it_cc_round
*&---------------------------------------------------------------------*
FORM get_it_cc_round.

* Actual cost FOR ROUNDING error correction
  LOOP AT it_cc_wip_scale.

    MOVE-CORRESPONDING it_cc_wip_scale TO it_cc_round.
    COLLECT it_cc_round.
    CLEAR it_cc_round.

  ENDLOOP.

  LOOP AT it_shop_cc_adj.
    MOVE-CORRESPONDING it_shop_cc_adj TO it_cc_round.
    COLLECT it_cc_round.
    CLEAR it_cc_round.
  ENDLOOP.

ENDFORM.                    " get_it_cc_round
*&---------------------------------------------------------------------*
*&      Form  update_job_log
*&---------------------------------------------------------------------*
FORM update_job_log.
  DATA : l_artnr(50),
         l_temp_artnr TYPE matnr.
  DATA : wa_artnr LIKE ztco_shop_sum_1-artnr.

  CLEAR l_artnr.

  MESSAGE s000 WITH 'Update log ...ZTCO_BATCH_LOG'.

  LOOP AT it_fsc_mat.
    SELECT  SINGLE artnr INTO wa_artnr
          FROM ztco_shop_sum_1
         WHERE kokrs = p_kokrs
           AND artnr = it_fsc_mat-matnr
           AND bdatj = p_bdatj
           AND poper = p_perab.

*      perform make_batch_log using 'D'.
    IF sy-subrc = 0 .
      PERFORM make_batch_log USING it_fsc_mat-matnr 'F'.
    ELSE.
      PERFORM make_batch_log USING it_fsc_mat-matnr 'X'.
      CONCATENATE it_fsc_mat-matnr l_artnr INTO l_artnr
      SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  IF NOT l_artnr IS INITIAL.
    EXPORT  l_artnr TO  MEMORY ID 'SHOPCC_ARTNR'.
  ENDIF.

ENDFORM.                    " update_job_log
*&---------------------------------------------------------------------*
*&      Form  get_kka_wip
*&---------------------------------------------------------------------*
FORM get_kka_wip USING    f_kokrs
                          f_objnr
                          f_gjahr
                          f_perio
                  CHANGING  f_wip_wrt.

  DATA: l_versa TYPE versn_abgr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = '0'
    IMPORTING
      output = l_versa.


* refer SAPKKA07
  DATA BEGIN OF tkkaabgr OCCURS 1.
          INCLUDE STRUCTURE kkaabgr.
  DATA END OF tkkaabgr.

  DATA: wip_wrt LIKE tkkaabgr-wrt.

  CONSTANTS: con_gruen(1) TYPE c VALUE '3'. "gruene Ampel

  DATA: abkat_0           LIKE cosba-abkat VALUE '50',
        abkat_bst_kos_ap  LIKE cosba-abkat VALUE '51',
        abkat_bst_kos_aw  LIKE cosba-abkat VALUE '52',
        abkat_bst_kos_nf  LIKE cosba-abkat VALUE '53',
        abkat_rue_kos_ap  LIKE cosba-abkat VALUE '54',
        abkat_rue_kos_aw  LIKE cosba-abkat VALUE '55',
        abkat_rue_kos_nf  LIKE cosba-abkat VALUE '56',
        abkat_rue_pro_ap  LIKE cosba-abkat VALUE '57',
        abkat_rue_pro_aw  LIKE cosba-abkat VALUE '58',
        abkat_rue_pro_nf  LIKE cosba-abkat VALUE '59'.


*Cost Obj Controlling MTO Miscellaneous
  CALL FUNCTION 'KKA_ABGR_LESEN_SUMMENSAETZE'
    EXPORTING
      kokrs    = f_kokrs
      objnr    = f_objnr
      versn    = l_versa
      gjahr    = f_gjahr
      periode  = f_perio
    TABLES
      tkkaabgr = tkkaabgr.


  wip_wrt = 0.

*//Modify..03/25/2011..T00020..
  SORT tkkaabgr BY abkat.

  LOOP AT tkkaabgr WHERE abkat EQ abkat_bst_kos_ap OR
                         abkat EQ abkat_bst_kos_aw OR
                         abkat EQ abkat_bst_kos_nf OR
                         abkat EQ abkat_rue_kos_ap OR
                         abkat EQ abkat_rue_kos_aw OR
                         abkat EQ abkat_rue_kos_nf.

    SUBTRACT tkkaabgr-wrt FROM wip_wrt.

  ENDLOOP.

*  LOOP AT tkkaabgr.
*    IF tkkaabgr-abkat EQ abkat_bst_kos_ap OR
*       tkkaabgr-abkat EQ abkat_bst_kos_aw OR
*       tkkaabgr-abkat EQ abkat_bst_kos_nf OR
*       tkkaabgr-abkat EQ abkat_rue_kos_ap OR
*       tkkaabgr-abkat EQ abkat_rue_kos_aw OR
*       tkkaabgr-abkat EQ abkat_rue_kos_nf.
*
*      SUBTRACT:
*      tkkaabgr-wrt FROM wip_wrt.
*    ENDIF.
*  ENDLOOP.
*//

  f_wip_wrt = wip_wrt.

ENDFORM.                    " get_kka_wip
*&---------------------------------------------------------------------*
*&      Form  get_ml_status
*&---------------------------------------------------------------------*
FORM get_ml_status  USING r_rc LIKE sy-subrc.

  DATA: ls_material  TYPE  ckml_s_matstatus_single_in.
  DATA: ls_matstatus TYPE  ckml_s_matstatus_single_out.
  CLEAR: ls_material.

  ls_material-kalnr        = it_fsc_mat-kaln1. "ckmlmv011-kalnr.
  ls_material-error_status = it_fsc_mat-error_status.
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
FORM read_mlperiods TABLES ut_kalnr STRUCTURE tper_kalnr
                    USING  ud_gjahr LIKE ckmlpp-bdatj
                           ud_poper LIKE ckmlpp-poper.

  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
      i_refresh_buffer          = 'X'
      i_use_buffer              = ' '
      i_build_smbew             = ' '
      i_bdatj_1                 = ud_gjahr
      i_poper_1                 = ud_poper
      i_untper                  = '00'    "lf_runperiod-untper
      i_call_by_reporting       = 'X'
      i_no_chk_periods_complete = 'X'
    TABLES
      t_kalnr                   = ut_kalnr
      t_ckmlpp                  = t_ckmlpp
      t_ckmlcr                  = t_ckmlcr
    EXCEPTIONS
      no_data_found             = 1
      input_data_inconsistent   = 2
      buffer_inconsistent       = 3
      OTHERS                    = 4.

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
*&---------------------------------------------------------------------*
*&      Form  READ_WIPS_REM
*&---------------------------------------------------------------------*
FORM read_wips_rem .

  DATA: soll_wip_not_ok LIKE kv042-xfeld.

*HOW??????
  DATA  t_auftlst LIKE kkapara_ta OCCURS 0 WITH HEADER LINE.
  DATA: kv014     LIKE kv014.

  DATA  BEGIN OF t_kv012 OCCURS 1.
          INCLUDE STRUCTURE kv012.
  DATA: END OF t_kv012.

  DATA  BEGIN OF t_kpzp1 OCCURS 1.
          INCLUDE STRUCTURE kpzp1.
  DATA: END OF t_kpzp1.

  DATA: f_wip_wrt TYPE wkgxxx.


  IF it_obj_wip-amount <> 0.

*//Modify..03/28/2011..T00020..
*    PERFORM get_kka_wip     USING p_kokrs
*                                  it_fsc_mat-objnr
*                                  p_bdatj
*                                  p_perab
*                         CHANGING f_wip_wrt.
*//

*  CHECK f_wip_wrt <> 0.
**---- check WIP calculation exist; not working at REM

*    read table it_cosbb with key objnr = it_fsc_mat-objnr
*                                 binary search.
*    check sy-subrc = 0.

*ISSUE
*  KV 159
*  Reporting point/operation 0010: WIP cannot be negative

    CALL FUNCTION 'K_WIP_OBJECT_CALC'
      EXPORTING
        par_gjahr              = p_bdatj
        par_kokrs              = p_kokrs
        par_objnr              = it_fsc_mat-objnr
        par_poper              = p_perab
        par_safnr              = ' '        "Order No.
        par_awbva              = '001'      "Eval.Variant
        par_awvsb              = '000'      "version
        par_auflo              = space
        par_plako              = 'X'        "Component explain
        par_knach              = 'X'        "No message
        par_mmess              = ' '        "Add.message
      IMPORTING
        flg_notok              = soll_wip_not_ok
        par_kv014              = kv014
      TABLES
        pta_kv012              = t_kv012
        pta_kpzp1              = t_kpzp1
      EXCEPTIONS
        targetcosts_impossible = 1  "46A
        system_error           = 2  "46A
        OTHERS                 = 3. "46A

*//Modify..03/25/2011..T00020..
    SORT t_kv012 BY awbvk.
    READ TABLE t_kv012 WITH KEY awbvk = 'P' BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT t_kv012 FROM sy-tabix.
        IF t_kv012-awbvk <> 'P'.  "Individual Order
          EXIT.
        ENDIF.
*//
        MOVE-CORRESPONDING t_kv012 TO it_wip_sap.
        it_wip_sap-objnr = it_fsc_mat-objnr.

* unit conversion... when needed
        IF p_debug = 'X' AND it_wip_sap-ckmatnr = p_resou+5(18).
          BREAK-POINT.
        ENDIF.

        PERFORM check_it_mat USING it_wip_sap-ckmatnr
                                   it_wip_sap-ckwerks
                             CHANGING sy-subrc.
*        READ TABLE  it_mat WITH KEY  matnr = it_wip_sap-ckmatnr
*                                     bwkey = it_wip_sap-ckwerks
*                           BINARY SEARCH.

        IF sy-subrc = 0.
          IF it_mat-meins <> it_wip_sap-meinh.
            PERFORM unit_converion
              USING    it_wip_sap-megbtr it_wip_sap-meinh it_mat-meins
              CHANGING it_wip_sap-megbtr.
            it_wip_sap-meinh = it_mat-meins.
          ENDIF.
*        ELSE.
*          PERFORM add_it_mat_tmp USING it_wip_sap-ckmatnr
*                                       it_wip_sap-ckwerks
*                              CHANGING sy-subrc.
        ENDIF.

        COLLECT it_wip_sap.
        CLEAR   it_wip_sap.

      ENDLOOP.
    ENDIF.

  ENDIF.


  CHECK it_obj_wip-amount2 <> 0.

*//Modify..03/28/2011..T00020..
*FIXME beginning WIP.... ANDY!!!
*  PERFORM get_kka_wip     USING p_kokrs
*                                it_fsc_mat-objnr
*                                g_pr_lfgja
*                                g_pr_lfmon
*                       CHANGING f_wip_wrt.
*//

*  CHECK f_wip_wrt <> 0.

  REFRESH t_kv012.
  CALL FUNCTION 'K_WIP_OBJECT_CALC'
    EXPORTING
      par_gjahr              = g_pr_lfgja
      par_knach              = 'X'          "No message
      par_kokrs              = p_kokrs
      par_objnr              = it_fsc_mat-objnr
      par_poper              = g_pr_lfmon
      par_safnr              = ' '          "Order No.
      par_awbva              = '001'        "Eval.Variant
      par_awvsb              = '000'        "version
      par_auflo              = space
      par_plako              = 'X'          "Component explain
      par_mmess              = ' '
    IMPORTING
      flg_notok              = soll_wip_not_ok
      par_kv014              = kv014
    TABLES
      pta_kv012              = t_kv012
      pta_kpzp1              = t_kpzp1
    EXCEPTIONS
      targetcosts_impossible = 1  "46A
      system_error           = 2  "46A
      OTHERS                 = 3. "46A

*//Modify..03/25/2011..T00020..
  SORT t_kv012 BY awbvk.
  READ TABLE t_kv012 WITH KEY awbvk = 'P' BINARY SEARCH.

  CHECK sy-subrc = 0.

  LOOP AT t_kv012 FROM sy-tabix.

    IF t_kv012-awbvk <> 'P'.  "Individual Order
      EXIT.
    ENDIF.
*//
    MOVE-CORRESPONDING t_kv012 TO it_wip_sap.

    it_wip_sap-objnr = it_fsc_mat-objnr.
    CLEAR: it_wip_sap-megbtr,
           it_wip_sap-wkgbtr.

    it_wip_sap-pegbtr  = t_kv012-megbtr.
    it_wip_sap-pkgbtr  = t_kv012-wkgbtr.

*//Modify..04/27/2011..T00020..
    CLEAR: it_wip_sap-mefbtr,
           it_wip_sap-wkfbtr.

    it_wip_sap-pefbtr  = t_kv012-mefbtr.
    it_wip_sap-pkfbtr  = t_kv012-wkfbtr.
*//

* unit conversion... when needed
    IF p_debug = 'X' AND it_wip_sap-ckmatnr = p_resou+5(18).
      BREAK-POINT.
    ENDIF.

    PERFORM check_it_mat USING it_wip_sap-ckmatnr
                               it_wip_sap-ckwerks
                         CHANGING sy-subrc.
*    READ TABLE  it_mat WITH KEY  matnr = it_wip_sap-ckmatnr
*                                 bwkey = it_wip_sap-ckwerks
*                       BINARY SEARCH.

    IF sy-subrc = 0.
      IF it_mat-meins <> it_wip_sap-meinh.
        PERFORM unit_converion
          USING    it_wip_sap-pegbtr it_wip_sap-meinh it_mat-meins
          CHANGING it_wip_sap-pegbtr.
        it_wip_sap-meinh = it_mat-meins.
      ENDIF.
*    ELSE.
*      PERFORM add_it_mat_tmp USING it_wip_sap-ckmatnr
*                                   it_wip_sap-ckwerks
*                          CHANGING sy-subrc.
    ENDIF.

    COLLECT it_wip_sap.
    CLEAR   it_wip_sap.

  ENDLOOP.


ENDFORM.                    " READ_WIPS_REM
*&---------------------------------------------------------------------*
*&      Form  READ_MATERIAL_ADD
*&---------------------------------------------------------------------*
FORM read_material_add USING l_matnr TYPE matnr
                             l_bwkey TYPE bwkey
                       CHANGING lw_mat TYPE tt_mat.

  DATA: l_lfgja  LIKE ckmlpp-bdatj,
        l_lfmon  LIKE ckmlpp-poper.
  DATA: lw_ckmlcr  TYPE tt_ckmlcr,
        lw_ckmlcr2 TYPE tt_ckmlcr.

  SELECT SINGLE
         ckmlhd~matnr   ckmlhd~bwkey
         marc~werks marc~profil marc~sauft marc~beskz marc~sobsl
         marc~vspvb marc~fevor  marc~prctr marc~lgpro
         mara~meins mara~raube mara~mtart
         ckmlhd~kalnr AS kaln1
         ckmlhd~abrechdat
         mbew~bklas
   INTO CORRESPONDING FIELDS OF lw_mat
           FROM ( ckmlhd
                       INNER JOIN mara
                          ON ckmlhd~matnr = mara~matnr
                       INNER JOIN marc
                          ON ckmlhd~matnr = marc~matnr
                         AND ckmlhd~bwkey = marc~werks
                       INNER JOIN mbew
                          ON ckmlhd~kalnr = mbew~kaln1 )
            WHERE ckmlhd~matnr = l_matnr
              AND ckmlhd~bwkey = l_bwkey.


  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF lw_ckmlcr
    FROM ckmlcr INNER JOIN ckmlpp
                   ON ckmlcr~kalnr  = ckmlpp~kalnr
                  AND ckmlcr~bdatj  = ckmlpp~bdatj
                  AND ckmlcr~poper  = ckmlpp~poper
                  AND ckmlcr~untper = ckmlpp~untper
    WHERE ckmlcr~kalnr  = lw_mat-kaln1
      AND ckmlcr~bdatj  = p_bdatj
      AND ckmlcr~poper  = p_perab
      AND ckmlcr~untper = space
      AND ckmlcr~curtp  = '10'.

  IF gv_sap_err = 1.
*    l_lfmon = p_perab + 1.
*    l_lfgja = p_bdatj.
*    if l_lfmon > 12.
*      l_lfmon = 1.
*      l_lfgja = p_bdatj + 1.
*    endif.
  ELSE.
    CALL FUNCTION 'CKML_F_GET_PREVIOUS_PERIOD'
      EXPORTING
        input_period    = l_lfmon
        input_year      = l_lfgja
        input_periv     = tka01-lmona
      IMPORTING
        previous_period = l_lfmon
        previous_year   = l_lfgja.
  ENDIF.
*Material Ledger: Period Totals Records Values
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF lw_ckmlcr2
    FROM ckmlcr INNER JOIN ckmlpp
                   ON ckmlcr~kalnr  = ckmlpp~kalnr
                  AND ckmlcr~bdatj  = ckmlpp~bdatj
                  AND ckmlcr~poper  = ckmlpp~poper
                  AND ckmlcr~untper = ckmlpp~untper
    WHERE ckmlcr~kalnr  = lw_mat-kaln1
      AND ckmlcr~bdatj  = l_lfgja
      AND ckmlcr~poper  = l_lfmon
      AND ckmlcr~untper = space
      AND ckmlcr~curtp  = '10'.


  PERFORM update_it_mat USING lw_ckmlcr lw_ckmlcr2
                        CHANGING lw_mat.

*  APPEND lw_mat TO it_mat.
  MODIFY it_mat FROM lw_mat
                TRANSPORTING peinh stprs verpr
    lfgja lfmon kstar vmpec vmstc vmpep vmstp
                  WHERE matnr = lw_mat-matnr
                              AND werks = lw_mat-werks.
*  SORT it_mat BY matnr bwkey bwtar.

* Commented out by Han Moon

** get CO key of material
*  SELECT hrkft werks matnr hkgrp subkey2
*    APPENDING TABLE it_cokey
*    FROM cokey
*    WHERE cokey~matnr = lw_mat-matnr
*      AND cokey~werks = lw_mat-werks.
*
*  SORT it_cokey BY hrkft.

ENDFORM.                    " READ_MATERIAL_ADD
*&---------------------------------------------------------------------*
*&      Form  UPDATE_IT_MAT
*&---------------------------------------------------------------------*
FORM update_it_mat  USING    lw_ckmlcr  TYPE tt_ckmlcr
                             lw_ckmlcr2 TYPE tt_ckmlcr
                    CHANGING lw_mat     TYPE tt_mat.


  lw_mat-peinh = lw_ckmlcr-peinh.
  lw_mat-stprs = lw_ckmlcr-stprs.
  lw_mat-verpr = lw_ckmlcr-pvprs.

*    lw_mat-lfgja = l_lfgja.
*    lw_mat-lfmon = l_lfmon.

  CLEAR i_t030.
  READ TABLE i_t030 WITH KEY bklas = lw_mat-bklas.
  lw_mat-kstar = i_t030-konts.

  IF gv_sap_err = 1.   "SAP ERROR SITUATION (OLD)
*     READ TABLE lw_ckmlcr2 WITH KEY kalnr = lw_mat-kaln1 BINARY SEARCH.
    IF sy-subrc = 0.
      lw_mat-vmpec = lw_ckmlcr2-peinh.
      lw_mat-vmstc = lw_ckmlcr2-stprs.
    ENDIF.

    lw_mat-vmpep = lw_mat-peinh.
    lw_mat-vmstp = lw_mat-stprs.
  ELSE.
    lw_mat-vmpec = lw_mat-peinh.
    lw_mat-vmstc = lw_mat-stprs.

*     READ TABLE lw_ckmlcr2 WITH KEY kalnr = lw_mat-kaln1 BINARY SEARCH.
    IF sy-subrc = 0.
      lw_mat-vmpep = lw_ckmlcr2-peinh.
      lw_mat-vmstp = lw_ckmlcr2-stprs.
    ENDIF.
  ENDIF.


  DATA: lw_qty LIKE ckmlpp-abkumo.
  lw_qty = lw_ckmlcr-abkumo + lw_ckmlcr-zukumo.
  IF sy-subrc = 0 AND lw_qty > 0.
    lw_mat-verpr = lw_ckmlcr-stprs +
       ( lw_ckmlcr-abprd_o + lw_ckmlcr-abkdm_o
       + lw_ckmlcr-zuprd_o + lw_ckmlcr-zukdm_o )
       / lw_qty.
  ENDIF.

* valuation class
  READ TABLE i_t030 WITH KEY bklas = lw_mat-bklas.
  IF sy-subrc = 0.
    lw_mat-kstar = i_t030-konts.
  ENDIF.

ENDFORM.                    " UPDATE_IT_MAT
*&---------------------------------------------------------------------
*&      Form  read_wip_from_cosb
*&---------------------------------------------------------------------
FORM read_wip_from_cosb.
*WIP pre,curr, Scrap
  DATA : BEGIN OF lt_cosba OCCURS 0.
          INCLUDE STRUCTURE cosba.
  DATA : END OF lt_cosba.

  DATA : BEGIN OF lt_cosbb OCCURS 0.
          INCLUDE STRUCTURE cosba.
  DATA : END OF lt_cosbb.

  DATA : lw_fname(30).
  FIELD-SYMBOLS : <amt>,<qty>.


  REFRESH: lt_cosba, lt_cosbb.

  CHECK it_fsc_mat[] IS NOT INITIAL.

  SORT it_fsc_mat BY objnr.

*//Modify..04/07/2011..T00020..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_cosba
    FROM cosb AS b LEFT OUTER JOIN cokey AS k
                                ON b~hrkft = k~hrkft
     CLIENT SPECIFIED BYPASSING BUFFER
     FOR ALL ENTRIES IN it_fsc_mat
   WHERE b~objnr = it_fsc_mat-objnr
     AND b~mandt = sy-mandt
     AND b~lednr = '00'
     AND b~versn = p_versn
     AND b~wrttp = '32'       "WIP only
     AND b~gjahr IN (g_pr_lfgja, p_bdatj)
     AND b~vrgng = 'KABG'
     %_HINTS ORACLE 'index("cosb" "cosb~1")'.

*previous WIP
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_COSBA
*            FROM COSB CLIENT SPECIFIED
*           FOR ALL ENTRIES IN IT_FSC_MAT
*           WHERE OBJNR = IT_FSC_MAT-OBJNR
*             AND MANDT = SY-MANDT
*             AND LEDNR = '00'
*             AND VERSN = P_VERSN
*             AND WRTTP = '32'         "WIP only
*             AND GJAHR = G_PR_LFGJA
*             AND VRGNG = 'KABG'
*              %_HINTS ORACLE 'index("cosb" "cosb~1")'.
**             AND beknz = c_gp_beknz.  "'S'
*//

  DELETE lt_cosba WHERE beknz <> c_gp_beknz.

  PERFORM extract_cosb_wip TABLES   lt_cosba
                           USING    ' '.

*current WIP
*//Modify..04/07/2011..T00020..
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_COSBB
*           FROM COSB CLIENT SPECIFIED
*           FOR ALL ENTRIES IN IT_FSC_MAT
*           WHERE OBJNR = IT_FSC_MAT-OBJNR
*             AND MANDT = SY-MANDT
*             AND LEDNR = '00'
*             AND VERSN = P_VERSN
*             AND WRTTP = '32'         "WIP only
*             AND GJAHR = P_BDATJ
*             AND VRGNG = 'KABG'
*              %_HINTS ORACLE 'index("cosb" "cosb~1")'.
**             AND beknz = c_gp_beknz.  "'S'
*
*  DELETE LT_COSBB WHERE BEKNZ <> C_GP_BEKNZ.
*
**  SORT  lt_cosbb BY objnr.
*
*  PERFORM EXTRACT_COSB_WIP TABLES LT_COSBB
*                           USING  'X'.
*
*//

ENDFORM.                    " read_wip_from_cosb
*&---------------------------------------------------------------------*
*&      Form  READ_SCRAP_FROM_PCC
*&---------------------------------------------------------------------*
FORM read_scrap_from_pcc .
  DATA : BEGIN OF lt_cosbs OCCURS 0.
          INCLUDE STRUCTURE cosba.
  DATA : END OF lt_cosbs.
  REFRESH lt_cosbs.

*scrap
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_cosbs
    FROM cosb AS b LEFT OUTER JOIN cokey AS k
                                ON b~hrkft = k~hrkft
     FOR ALL ENTRIES IN it_fsc_mat
   WHERE b~lednr = '00'
     AND b~objnr = it_fsc_mat-objnr
     AND b~gjahr = g_pr_lfgja
     AND b~wrttp = '30'         "WIP only
     AND b~versn = p_versn
     AND b~beknz = c_gp_beknz.


  PERFORM extract_cosb_scrap TABLES lt_cosbs.


  SORT it_obj_scrap BY objnr.

ENDFORM.                    " READ_SCRAP_FROM_PCC
*&---------------------------------------------------------------------*
*&      Form  READ_COKEY
*&---------------------------------------------------------------------*
FORM read_cokey .

  CHECK it_mat[] IS NOT INITIAL.

* get CO key of material
  SELECT hrkft werks matnr hkgrp subkey2 INTO TABLE it_cokey
    FROM cokey
    FOR ALL ENTRIES IN it_mat
    WHERE cokey~matnr = it_mat-matnr
      AND cokey~werks = it_mat-werks.

  SORT it_cokey BY hrkft.

ENDFORM.                    " READ_COKEY
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_COSB_WIP
*&---------------------------------------------------------------------*
FORM extract_cosb_wip  TABLES lt_cosbs STRUCTURE cosba
                       USING  l_first  TYPE char1.

  DATA : lw_fname(30).
  DATA : l_amt    TYPE wkgxxx,
         l_period LIKE ckmlpp-poper.

  FIELD-SYMBOLS : <amt>,<qty>.

*//Modify..04/07/2011..T00020..
*  IF L_FIRST = 'X'.
*    L_PERIOD = P_PERAB.
*  ELSE.
*    L_PERIOD = G_PR_LFMON.
*  ENDIF.

  SORT lt_cosbs BY objnr gjahr.
*//

  LOOP AT  lt_cosbs.

*//Modify..04/07/2011..T00020..
    READ TABLE it_fsc_mat WITH KEY objnr = lt_cosbs-objnr
                                   BINARY SEARCH.
    CHECK sy-subrc = 0.

    CLEAR it_obj_wip.
    IF g_pr_lfgja <> p_bdatj.  "different year
      IF g_pr_lfgja = lt_cosbs-gjahr.
        CONCATENATE 'LT_COSBS-WKG' g_pr_lfmon INTO lw_fname.
        ASSIGN (lw_fname) TO <amt>.
        it_obj_wip-amount2 = <amt>.
      ELSE.
        CONCATENATE 'LT_COSBS-WKG' p_perab INTO lw_fname.
        ASSIGN (lw_fname) TO <amt>.
        it_obj_wip-amount = <amt>.
      ENDIF.
    ELSE.
      CONCATENATE 'LT_COSBS-WKG' g_pr_lfmon INTO lw_fname.
      ASSIGN (lw_fname) TO <amt>.
      it_obj_wip-amount2 = <amt>.
      CONCATENATE 'LT_COSBS-WKG' p_perab INTO lw_fname.
      ASSIGN (lw_fname) TO <amt>.
      it_obj_wip-amount  = <amt>.
    ENDIF.
*//

    CHECK it_obj_wip-amount <> 0 OR it_obj_wip-amount2 <> 0.

    it_obj_wip-objnr = lt_cosbs-objnr.
    it_obj_wip-kstar = lt_cosbs-kstar.


    CASE lt_cosbs-parob(2).

      WHEN 'KL'. "CC+ACT

        it_obj_wip-typps = 'E'.
        it_obj_wip-kostl = lt_cosbs-parob+6(10).
        it_obj_wip-lstar = lt_cosbs-parob+10(6).
        PERFORM make_resou USING 'E' it_obj_wip-kostl it_obj_wip-lstar
                           CHANGING  it_obj_wip-resou .

      WHEN OTHERS.
        IF lt_cosbs-matnr IS INITIAL.
          it_obj_wip-typps = 'V'.
        ELSE.
          it_obj_wip-typps = 'M'.
          it_obj_wip-matnr = lt_cosbs-matnr.
          it_obj_wip-werks = lt_cosbs-werks.
          PERFORM make_resou USING 'M' it_obj_wip-werks it_obj_wip-matnr
                             CHANGING  it_obj_wip-resou.
        ENDIF.
*        READ TABLE it_cokey WITH KEY hrkft = lt_cosbs-hrkft
*                                     BINARY SEARCH.
*        IF sy-subrc = 0.
*          it_obj_wip-typps = 'M'.
*          it_obj_wip-matnr = it_cokey-matnr.
*          it_obj_wip-werks = it_cokey-werks.
*          PERFORM make_resou USING 'M' it_obj_wip-werks it_obj_wip-matnr
*                             CHANGING  it_obj_wip-resou.
*        ELSE.
*          it_obj_wip-typps = 'V'.
*        ENDIF.

    ENDCASE.

    COLLECT it_obj_wip.

  ENDLOOP.


ENDFORM.                    " EXTRACT_COSB_WIP
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_COSB_SCRAP
*&---------------------------------------------------------------------*
FORM extract_cosb_scrap  TABLES lt_cosbs STRUCTURE cosba.

  DATA : lw_fname(30).
  FIELD-SYMBOLS : <amt>,<qty>.
  DATA: l_amt    TYPE wkgxxx,
        l_period LIKE ckmlpp-poper.

  l_period = p_perab.

  LOOP AT  lt_cosbs.
    CLEAR : lw_fname.

    CONCATENATE 'LT_COSBS-WKG' l_period INTO lw_fname.
    ASSIGN (lw_fname) TO <amt>.
    l_amt = <amt>.

    CHECK l_amt <> 0.

    CLEAR it_obj_scrap.
    it_obj_scrap-amount  = l_amt.

    it_obj_scrap-objnr = lt_cosbs-objnr.
    it_obj_scrap-kstar = lt_cosbs-kstar.

    IF lt_cosbs-parob(2) = 'KL'. "CC+ACT
      it_obj_scrap-typps = 'E'.
      it_obj_scrap-kostl = lt_cosbs-parob+6(10).
      it_obj_scrap-lstar = lt_cosbs-parob+10(6).
      PERFORM make_resou USING 'E' it_obj_scrap-kostl  it_obj_scrap-lstar
                         CHANGING  it_obj_scrap-resou .
    ELSE.
      IF lt_cosbs-matnr IS INITIAL.
        it_obj_scrap-typps = 'V'.
      ELSE.
        it_obj_scrap-typps = 'M'.
        it_obj_scrap-matnr = lt_cosbs-matnr.
        it_obj_scrap-werks = lt_cosbs-werks.
        PERFORM make_resou USING 'M' it_obj_scrap-werks it_obj_scrap-matnr
                           CHANGING  it_obj_scrap-resou.
      ENDIF.

*      READ TABLE it_cokey WITH KEY hrkft = lt_cosbs-hrkft BINARY SEARCH.
*      IF sy-subrc = 0.
*        it_obj_scrap-typps = 'M'.
*        it_obj_scrap-matnr = it_cokey-matnr.
*        it_obj_scrap-werks = it_cokey-werks.
*        PERFORM make_resou USING 'M' it_obj_scrap-werks it_obj_scrap-matnr
*                           CHANGING  it_obj_scrap-resou.
*      ELSE.
*        it_obj_scrap-typps = 'V'.
*      ENDIF.
    ENDIF.


    COLLECT it_obj_scrap.
  ENDLOOP.


ENDFORM.                    " EXTRACT_COSB_SCRAP
*&---------------------------------------------------------------------*
*&      Form  GET_COVP_SUMMARY_T00020
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_covp_summary_t00020 .

*  DATA : BEGIN OF lt_covp OCCURS 0.
*          INCLUDE STRUCTURE it_covp.
*  DATA : END OF lt_covp.

  CHECK p_sql = 'X'.

*  REFRESH : lt_covp. CLEAR : lt_covp.

  LOOP AT it_fsc_mat.
*    select   objnr kstar werks matnr
*             parob hrkft vrgng beknz meinb
*             sum( wkgbtr ) sum( mbgbtr )
*             sgtxt awtyp  refbn  aworg
*        appending table it_covp
*        from covp client specified bypassing buffer
*        where mandt =  sy-mandt
*          and kokrs =  p_kokrs
*          and objnr =  it_fsc_mat-objnr
*          and gjahr =  p_bdatj
*          and perio =  p_perab
*          and wrttp =  c_gp_wrttp
*          and versn =  p_versn
*          and vrgng = 'RKL'
*          and awtyp = 'AFRU'
*        group by
*                 objnr kstar werks matnr
*                 parob hrkft vrgng beknz meinb sgtxt
*                 awtyp  refbn  aworg
*        %_hints oracle 'LEADING "T_00"'.



    SELECT v~objnr v~kstar v~werks v~matnr
           v~parob v~hrkft v~vrgng v~beknz v~meinb
           SUM( v~wkgbtr )
           SUM( v~mbgbtr )
           v~sgtxt v~awtyp  v~refbn  v~aworg
      APPENDING TABLE it_covp
      FROM covp AS v CLIENT SPECIFIED BYPASSING BUFFER
     WHERE v~mandt = sy-mandt
       AND v~objnr = it_fsc_mat-objnr
       AND v~gjahr = p_bdatj
       AND v~perio = p_perab
       AND v~wrttp = c_gp_wrttp
       AND v~versn = p_versn
     GROUP BY v~objnr v~kstar v~werks v~matnr
              v~parob v~hrkft v~vrgng v~beknz v~meinb v~sgtxt
              v~awtyp  v~refbn  v~aworg.

  ENDLOOP.

*  SELECT OBJNR KSTAR WERKS MATNR
*         PAROB HRKFT VRGNG BEKNZ MEINB
*         WKGBTR MBGBTR SGTXT
*    INTO CORRESPONDING FIELDS OF TABLE LT_COVP
*      FROM COVP CLIENT SPECIFIED BYPASSING BUFFER
*      FOR ALL ENTRIES IN IT_FSC_MAT
*      WHERE MANDT =  SY-MANDT
*        AND KOKRS =  P_KOKRS
*        AND BUDAT IN R_BUDAT
*        AND OBJNR =  IT_FSC_MAT-OBJNR
*        AND GJAHR =  P_BDATJ
*        AND PERIO =  P_PERAB
*        AND WRTTP =  C_GP_WRTTP
*        AND VERSN =  P_VERSN.
*
*  LOOP AT LT_COVP.
*    COLLECT LT_COVP INTO IT_COVP.
*    CLEAR   LT_COVP.
*  ENDLOOP.

ENDFORM.                    " GET_COVP_SUMMARY_T00020
*&---------------------------------------------------------------------*
*&      Form  EXEC_IN_BG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exec_in_bg  USING    value(p_program) TYPE sy-repid
                          p_process.

*//Modify..03/31/2011..T00020..
  CALL FUNCTION 'Z_SHOP_CC_BATCHJOB'
    EXPORTING
      i_program = p_program
      i_variant = sy-slset
      i_process = p_process
      i_count   = p_count.
*//

ENDFORM.                    " EXEC_IN_BG
*&---------------------------------------------------------------------*
*&      Form  PROCESSING_NEW_T00020
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processing_new_t00020 .

  REFRESH gt_cc_amt_new.  CLEAR gt_cc_amt_new.


  LOOP AT it_ckmlkeph.
    PERFORM extract_cc_amt_cur USING it_ckmlkeph "'CKMLKEPH'
                                      p_bdatj p_perab.
  ENDLOOP.

  LOOP AT it_ckmlprkeph_p.
    PERFORM extract_cc_amt_new USING it_ckmlprkeph_p 'CKMLPRKEPH_P'
                                     g_pr_lfgja g_pr_lfmon.
  ENDLOOP.

  LOOP AT it_ckmlprkeph.
    PERFORM extract_cc_amt_new USING it_ckmlprkeph 'CKMLPRKEPH'
                                     p_bdatj p_perab.
  ENDLOOP.

* Commented out by Han Moon
*  LOOP AT it_prkeph_p.
*    PERFORM extract_cc_amt_new USING it_prkeph_p 'PRKEPH_P'
*                                     g_pr_lfgja g_pr_lfmon.
*  ENDLOOP.

* Replaced by Han Moon
*  LOOP AT it_prkeph.
*    PERFORM extract_cc_amt_new USING it_prkeph 'PRKEPH'
*                                     p_bdatj p_perab.
*  ENDLOOP.
  DATA: lf_gubun(15),
        lf_beskz TYPE marc-beskz,
        lf_sobsl TYPE marc-sobsl.

  LOOP AT it_prkeph.
    READ TABLE it_ckobject WITH TABLE KEY kalnr = it_prkeph-kalnr.
    CHECK sy-subrc EQ 0.
    lf_beskz = it_ckobject-beskz.
    lf_sobsl = it_ckobject-sobsl.

    IF lf_beskz EQ 'E' AND lf_sobsl EQ space.
      IF it_prkeph-bdatj = p_bdatj AND
         it_prkeph-poper = p_perab.
        lf_gubun = 'PRKEPH_FSC'.
      ELSE.
        lf_gubun = 'PRKEPH_FSC_P'.
      ENDIF.
    ELSE.
      IF it_prkeph-bdatj = p_bdatj AND
         it_prkeph-poper = p_perab.
        lf_gubun = 'PRKEPH'.
      ELSE.
        lf_gubun = 'PRKEPH_P'.
      ENDIF.
    ENDIF.
    PERFORM extract_cc_amt_new USING it_prkeph
                                     lf_gubun
                                     it_prkeph-bdatj
                                     it_prkeph-poper.
  ENDLOOP.

* Commented out by Han Moon
*  LOOP AT it_prkeph_fsc.
*    PERFORM extract_cc_amt_new USING it_prkeph_fsc 'PRKEPH_FSC'
*                                     p_bdatj p_perab.
*  ENDLOOP.
*
**//Modify..04/27/2011..T00020..
*  LOOP AT it_prkeph_fsc_p.
*    PERFORM extract_cc_amt_new USING it_prkeph_fsc_p 'PRKEPH_FSC_P'
*                                     g_pr_lfgja g_pr_lfmon.
*  ENDLOOP.
**//

*  SORT gt_cc_amt_new BY gubun kalnr bdatj poper kkzst.

ENDFORM.                    " PROCESSING_NEW_T00020
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_CC_AMT_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM extract_cc_amt_new  USING   f_keph LIKE ckmlprkeph
                                 p_gubun
                                 pp_bdatj
                                 pp_poper.

  DATA : l_cnt(3) TYPE n,
*         l_field(25),
         l_amt     TYPE ckmlprkeph-kst001,
         l_dmbtr   TYPE ckmlprkeph-kst001,
         l_dmbtr_f TYPE ckmlprkeph-kst001.

  CLEAR : l_cnt, l_amt.

*-take overall value only (no low level data)
  CHECK f_keph-kkzst = ''.

  DO 40 TIMES VARYING l_amt FROM f_keph-kst001
                            NEXT f_keph-kst002.

    l_cnt = l_cnt + 1.

*    CONCATENATE 'F_KEPH-KST' l_cnt INTO l_field.
*    ASSIGN  (l_field)    TO   <f_field> .
*    CLEAR l_amt.
*    l_amt = <f_field>.
    CHECK NOT l_amt IS INITIAL.

* Overall value
    CLEAR it_elemt. " it_tckh3.
    CLEAR: l_dmbtr, l_dmbtr_f.
    READ TABLE it_elemt WITH TABLE KEY elnum = l_cnt.
    IF sy-subrc EQ 0.
      IF it_elemt-eltyp = 'V'.
        l_dmbtr   = l_amt.
      ELSE.
        l_dmbtr_f = l_amt.
      ENDIF.
    ENDIF.
*    READ TABLE it_tckh3 WITH KEY el_hv = l_cnt.
*    IF sy-subrc   =  0 .
*      l_dmbtr   = l_amt.
*    ELSE.
** Fixed value
*      READ TABLE it_tckh3 WITH KEY el_hf = l_cnt.
*      IF sy-subrc   = 0 .
*        l_dmbtr_f = l_amt.
*      ENDIF.
*    ENDIF.
    CLEAR gt_cc_amt_new.
    READ TABLE gt_cc_amt_new WITH TABLE KEY "gubun = p_gubun
                                            kalnr = f_keph-kalnr
                                            bdatj = pp_bdatj
                                            poper = pp_poper
                                            kkzst = f_keph-kkzst
                                            elemt = it_elemt-elemt.
    IF sy-subrc EQ 0.
      ADD l_dmbtr   TO gt_cc_amt_new-dmbtr.
      ADD l_dmbtr_f TO gt_cc_amt_new-dmbtr_f.
      MODIFY gt_cc_amt_new INDEX sy-tabix.
    ELSE.
*      gt_cc_amt_new-gubun   = p_gubun.
      gt_cc_amt_new-kalnr   = f_keph-kalnr.
      gt_cc_amt_new-bdatj   = pp_bdatj.
      gt_cc_amt_new-poper   = pp_poper.
      gt_cc_amt_new-kkzst   = f_keph-kkzst .
      gt_cc_amt_new-elemt   = it_elemt-elemt.
      gt_cc_amt_new-dmbtr   = l_dmbtr.
      gt_cc_amt_new-dmbtr_f = l_dmbtr_f.
      INSERT gt_cc_amt_new INTO TABLE gt_cc_amt_new.
    ENDIF.
*    COLLECT gt_cc_amt_new.
*    CLEAR   gt_cc_amt_new.

  ENDDO.

ENDFORM.                    " EXTRACT_CC_AMT_NEW
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_CC_AMT_CUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM extract_cc_amt_cur  USING   f_keph LIKE ckmlkeph
                                 pp_bdatj
                                 pp_poper.

  DATA : l_cnt(3) TYPE n,
*         l_field(25),
         l_amt     TYPE ckmlkeph-kst001,
         l_dmbtr   TYPE ckmlkeph-kst001,
         l_dmbtr_f TYPE ckmlkeph-kst001.

  CLEAR : l_cnt, l_amt.

*-take overall value only (no low level data)
  CHECK f_keph-kkzst = ''.

  DO 40 TIMES VARYING l_amt FROM f_keph-kst001
                            NEXT f_keph-kst002.

    l_cnt = l_cnt + 1.
*    CONCATENATE 'F_KEPH-KST' l_cnt INTO l_field.
*    ASSIGN  (l_field)    TO   <f_field> .
*    CLEAR l_amt.
*    l_amt = <f_field>.
    CHECK NOT l_amt IS INITIAL.

* Overall value
    CLEAR it_elemt.
    CLEAR: l_dmbtr, l_dmbtr_f.
    READ TABLE it_elemt WITH TABLE KEY elnum = l_cnt.
    IF sy-subrc EQ 0.
      IF it_elemt-eltyp = 'V'.
        l_dmbtr   = l_amt.
      ELSE.
        l_dmbtr_f = l_amt.
      ENDIF.
    ENDIF.

    CLEAR gt_cc_amt_cur.
    READ TABLE gt_cc_amt_cur WITH TABLE KEY "gubun = p_gubun
                                            kalnr = f_keph-kalnr
                                            bdatj = pp_bdatj
                                            poper = pp_poper
                                            kkzst = f_keph-kkzst
                                            elemt = it_elemt-elemt.
    IF sy-subrc EQ 0.
      ADD l_dmbtr   TO gt_cc_amt_cur-dmbtr.
      ADD l_dmbtr_f TO gt_cc_amt_cur-dmbtr_f.
      MODIFY gt_cc_amt_cur INDEX sy-tabix.
    ELSE.
*      gt_cc_amt_new-gubun   = p_gubun.
      gt_cc_amt_cur-kalnr   = f_keph-kalnr.
      gt_cc_amt_cur-bdatj   = pp_bdatj.
      gt_cc_amt_cur-poper   = pp_poper.
      gt_cc_amt_cur-kkzst   = f_keph-kkzst .
      gt_cc_amt_cur-elemt   = it_elemt-elemt.
      gt_cc_amt_cur-dmbtr   = l_dmbtr.
      gt_cc_amt_cur-dmbtr_f = l_dmbtr_f.
      INSERT gt_cc_amt_cur INTO TABLE gt_cc_amt_cur.
    ENDIF.

*    gt_cc_amt_new-gubun = p_gubun.
*    gt_cc_amt_new-kalnr = f_keph-kalnr.
*    gt_cc_amt_new-bdatj = pp_bdatj.
*    gt_cc_amt_new-poper = pp_poper.
*    gt_cc_amt_new-kkzst = f_keph-kkzst .
*
*    gt_cc_amt_new-elemt = it_tckh3-elemt.
*
*    COLLECT gt_cc_amt_new.
*    CLEAR   gt_cc_amt_new.

  ENDDO.

ENDFORM.                    " EXTRACT_CC_AMT_CUR
*&---------------------------------------------------------------------*
*&      Form  PREPARE_ML_CC_READ_ITEMS
*&---------------------------------------------------------------------*
FORM prepare_ml_cc_read_items .

*component
  REFRESH : lt_kalnr. " it_act, it_kstar.

*  SORT it_mat BY matnr werks.
  LOOP AT it_mat.
    lt_kalnr-kalnr  = it_mat-kaln1 .

* Replaced by Han Moon
*    APPEND lt_kalnr.
    COLLECT lt_kalnr.
* Added by Han Moon
    CLEAR it_ckobject.
    MOVE-CORRESPONDING it_mat TO it_ckobject.
    it_ckobject-kalnr  = it_mat-kaln1.
    INSERT it_ckobject INTO TABLE it_ckobject.
  ENDLOOP.

  LOOP AT it_fsc_mat.
* Commented out by Han Moon
*    lt_kalnr2-kalnr  = it_fsc_mat-kaln1.
*    APPEND lt_kalnr2.
    lt_kalnr-kalnr  = it_mat-kaln1 .
    COLLECT lt_kalnr.
* Added by Han Moon
    READ TABLE it_ckobject WITH TABLE KEY kalnr = it_fsc_mat-kaln1.
    IF sy-subrc <> 0.
      CLEAR it_ckobject.
      MOVE-CORRESPONDING it_fsc_mat TO it_ckobject.
      it_ckobject-kalnr  = it_fsc_mat-kaln1.
      INSERT it_ckobject INTO TABLE it_ckobject.
    ENDIF.
  ENDLOOP.

*product
  LOOP AT it_fsc_gr.
    PERFORM check_it_mat USING it_fsc_gr-matnr
                               it_fsc_gr-werks
                         CHANGING sy-subrc.
*    READ TABLE it_mat WITH KEY matnr = it_fsc_gr-matnr
*                               werks = it_fsc_gr-werks
*                               BINARY SEARCH.
    CHECK sy-subrc = 0.
    lt_kalnr-kalnr  = it_mat-kaln1 .
    COLLECT lt_kalnr.  CLEAR lt_kalnr.
  ENDLOOP.

* Commented out by Han Moon
*  SORT lt_kalnr.
*  DELETE ADJACENT DUPLICATES FROM lt_kalnr.

ENDFORM.                    " PREPARE_ML_CC_READ_ITEMS
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_CC_CURRENT
*&---------------------------------------------------------------------*
FORM extract_cc_current  USING    lf_kalnr LIKE ckmlhd-kalnr
                                  lf_gubun TYPE char20.

  READ TABLE gt_cc_amt_new WITH KEY
*                            gubun = lf_gubun
                            kalnr = lf_kalnr
                            bdatj = p_bdatj
                            poper = p_perab
                            kkzst = ''
                            BINARY SEARCH.
  IF sy-subrc = 0.
    LOOP AT gt_cc_amt_new FROM sy-tabix.
      IF "gt_cc_amt_new-gubun <> lf_gubun OR
         gt_cc_amt_new-kalnr <> lf_kalnr OR
         gt_cc_amt_new-bdatj <> p_bdatj  OR
         gt_cc_amt_new-poper <> p_perab  OR
         gt_cc_amt_new-kkzst <> ''.
        EXIT.
      ENDIF.

      it_shop_cc-elemt        = gt_cc_amt_new-elemt.
      PERFORM collect_shop_cc USING gt_cc_amt_new-dmbtr    'A'.

      it_shop_cc-elemt        = gt_cc_amt_new-elemt.
      PERFORM collect_shop_cc USING gt_cc_amt_new-dmbtr_f  'F'.

    ENDLOOP.

*FIXME LATER
* Need logic to scale based on sum table
* SUM = total of CC (+/-) to largest amount item

  ENDIF.

ENDFORM.                    " EXTRACT_CC_CURRENT
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_CC_PREVIOUS
*&---------------------------------------------------------------------*
FORM extract_cc_previous  USING   lf_kalnr LIKE ckmlhd-kalnr
                                  lf_gubun TYPE char20.

  READ TABLE gt_cc_amt_new WITH KEY
*                            gubun = lf_gubun
                            kalnr = lf_kalnr
                            bdatj = g_pr_lfgja
                            poper = g_pr_lfmon
                            kkzst = ''
                            BINARY SEARCH.
  IF sy-subrc = 0.
    LOOP AT gt_cc_amt_new FROM sy-tabix.
      IF "gt_cc_amt_new-gubun <> lf_gubun            OR
         gt_cc_amt_new-kalnr <> lf_kalnr            OR
         gt_cc_amt_new-bdatj <> g_pr_lfgja          OR
         gt_cc_amt_new-poper <> g_pr_lfmon          OR
         gt_cc_amt_new-kkzst <> ''.
        EXIT.
      ENDIF.
      it_shop_cc-elemt        = gt_cc_amt_new-elemt.
      PERFORM collect_shop_cc_p USING gt_cc_amt_new-dmbtr    'A'.

      it_shop_cc-elemt        = gt_cc_amt_new-elemt.
      PERFORM collect_shop_cc_p USING gt_cc_amt_new-dmbtr_f  'F'.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " EXTRACT_CC_PREVIOUS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTCOU100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM read_ztcou100 .
*
*  DATA: lt_ztcou100 LIKE ztcou100 OCCURS 0 WITH HEADER LINE.
*
*  REFRESH lt_ztcou100. CLEAR lt_ztcou100.
*
*  SELECT *
*    INTO CORRESPONDING FIELDS OF TABLE lt_ztcou100
*    FROM ztcou100
*   WHERE kokrs =  p_kokrs
*     AND kalka =  'U1'
*     AND bdatj =  p_bdatj
*     AND poper =  p_perab
*     AND matnr IN s_matnr
*     AND werks IN s_werks
*     AND mtart IN s_mtart.
*
*  IF sy-subrc <> 0.
*    MESSAGE s000 WITH 'This period is not Data in Unit Cost.'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
*  REFRESH s_matnr. CLEAR s_matnr.
*
*  LOOP AT lt_ztcou100.
*    s_matnr = 'IEQ'.
*    s_matnr-low = lt_ztcou100-matnr.
*    APPEND s_matnr.
*    CLEAR  s_matnr.
*  ENDLOOP.
*
*ENDFORM.                    " READ_ZTCOU100
*&---------------------------------------------------------------------*
*&      Form  READ_TRACKING_POINT
*&---------------------------------------------------------------------*
*       Read Tracking Point
*----------------------------------------------------------------------*
*FORM read_tracking_point .
*  SELECT key1 AS prvbe
*         key2 AS tpoint
*    INTO CORRESPONDING FIELDS OF TABLE it_tp
*    FROM ztpp0001
*   WHERE code EQ '07'.
*ENDFORM.                    " READ_TRACKING_POINT
*&---------------------------------------------------------------------*
*&      Form  UNIT_CONVERION_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COEP_MBGBTR  text
*      -->P_IT_COEP_MEINB  text
*      -->P_IT_MAT_MEINS  text
*      <--P_IT_COEP_MBGBTR  text
*----------------------------------------------------------------------*
FORM unit_converion USING    p_input
                             p_unit_in
                             p_unit_out
                    CHANGING p_output.

  IF p_unit_in = p_unit_out.
    p_output = p_input.
  ELSE.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = p_input
        unit_in              = p_unit_in
        unit_out             = p_unit_out
      IMPORTING
        output               = p_output
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.
    IF sy-subrc <> 0.
      IF p_debug = 'X'.
        BREAK-POINT.
      ENDIF.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " UNIT_CONVERION_NEW
