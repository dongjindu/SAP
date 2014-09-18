************************************************************************
* Program Name      : ZACO09U_SHOP
* Author            : Hyung Jin Youn
* Creation Date     : 19/02/2004
* Specifications By : Hae Sung Cho
* Pattern           : Report 1-1
* Development Request No:UD1K908459
* Add documentation :
* Description       : Making Cost data which analysed by SHOP and Cost
*                     Component
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
* 01/11/2005 wskim issue #20041201-001 standard cost error :
************************************************************************
REPORT zaco09u_shop MESSAGE-ID zmco.

*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
INCLUDE zlzgco_global_formto1.

* For Global Value in CO For SHOP
INCLUDE zlzgco_global_formto2.

* TYPE-POOL
TYPE-POOLS:
  ccs00,
  ccs01,
  ckmv0.

FIELD-SYMBOLS: <f_field>.
*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
TABLES : keko, keph,   ckhs,  ckis,  tckh3, tckh2, cskb ,tckh4, coomco,
         khsk,mara.
TABLES : cki64a, covja, rkpln.
TABLES : tck03,  tck05.
TABLES : t001w,  macku, marc.
TABLES : zvco_rp1, tka01,
         ztco_shop_plh,

         ztco_shop_pln,
         ztco_shop_pln_cc.

*        zsco_shopcost_001, zsco_shopcost_key.
TABLES : crhd, plpo, plko, mkal.

** Internal Tables
*  For Valuation Info. of Materials
DATA : BEGIN OF it_mat OCCURS 100,
        matnr LIKE macku-matnr,
        werks LIKE marc-werks,

        peinh TYPE ck_kpeinh,
        pmeht TYPE pmeht,

        bwkey LIKE macku-bwkey,
        bwtar LIKE macku-bwtar,
        kalnr LIKE ckmlhd-kalnr,

        raube LIKE mara-raube,   "shop
        fevor LIKE marc-fevor,   "Production scheduler
        vspvb LIKE marc-vspvb,   "Proposed Supply Area

        mtart LIKE macku-mtart,
        beskz LIKE marc-beskz,   "Procurement Type
        sobsl LIKE marc-sobsl,   "Special procurement type
        prctr LIKE marc-prctr,   "PCT
* by ig.moon 6/11/2009 {
        matkl TYPE matkl,
* }
       END OF it_mat.

* by ig.moon 6/11/2009 {
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

* }


RANGES: gr_ccact FOR coomco-objnr.  "KL+H201+CC/ACT


DATA: it_prkeko     LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
DATA: it_prkeph     LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.
DATA: it_prkeko_sum LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
DATA: it_prkeph_sum LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.
DATA: it_ckmlprkeko LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
DATA: it_ckmlprkeph LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE,
      it_ckmllahd         LIKE ckmllahd OCCURS 0 WITH HEADER LINE.
DATA : it_tckh3      LIKE tckh3         OCCURS 0 WITH HEADER LINE.
DATA : it_mlkeph     LIKE mlkeph        OCCURS 0 WITH HEADER LINE.
DATA: ld_wa_db_keph  LIKE ckmlprkeph,
      ld_wa_db_keko  LIKE ckmlprkeko.

DATA : BEGIN OF it_kstar OCCURS 0,
        kstar LIKE ztco_shop_cc-kstar,
        elemt LIKE ztco_shop_cc-elemt,
       END OF it_kstar.

DATA : it_csla TYPE TABLE OF csla WITH HEADER LINE.

DATA : BEGIN OF it_onrkl OCCURS 0,
        kostl LIKE onrkl-kostl,
        lstar LIKE onrkl-lstar,
        objnr LIKE onrkl-objnr,
       END OF it_onrkl.

*DATA : BEGIN OF it_cosl OCCURS 0 ,
*        objnr    LIKE cosl-objnr,
*        kostl    TYPE kostl,
*        lstar    TYPE kstar,
*        vrgng    LIKE cosl-vrgng,
*        meinh    TYPE meinh,
*        menge    LIKE cosl-lst001,
*       END OF it_cosl .

* Main ITAB
DATA : BEGIN OF ty_ztco_shop_cc OCCURS 5000.
        INCLUDE STRUCTURE ztco_shop_pln.
DATA :  elemt  LIKE ztco_shop_pln_cc-elemt.
DATA : $peinh(15).
DATA : $lot_size(15).
DATA : END OF  ty_ztco_shop_cc.

* Temp. Table for Main ITAB
DATA : BEGIN OF gt_shop OCCURS 5000.
        INCLUDE STRUCTURE ty_ztco_shop_cc.
DATA : END OF  gt_shop.
DATA : gt_shop_cc     LIKE gt_shop    OCCURS 0 WITH HEADER LINE.

DATA : it_shop_pln    LIKE ztco_shop_pln    OCCURS 0 WITH HEADER LINE.
DATA : it_shop_pln_cc LIKE ztco_shop_pln_cc OCCURS 0 WITH HEADER LINE.

RANGES : r_matnr FOR plaf-matnr.

DATA : BEGIN OF it_tmp_pe OCCURS 0,
        llv_matnr LIKE mara-matnr ,
        werks     LIKE t001w-werks,
        bwkey     LIKE mbew-bwkey ,
        bwtar     LIKE mbew-bwtar .
DATA : END OF   it_tmp_pe .
* KEKO
DATA: it_ckikekokey   TYPE TABLE OF ckiuser  WITH HEADER LINE.

*DATA : BEGIN OF it_ckikekokey OCCURS 0.
*        INCLUDE STRUCTURE ckikekokey.
*DATA : kadat LIKE keko-kadat,
*       bidat LIKE keko-bidat,
*       matnr LIKE keko-matnr,
*       werks LIKE keko-werks,
*       bwkey LIKE keko-bwkey,
*       bwtar LIKE keko-bwtar,
*       kalst LIKE keko-kalst,
*       losgr LIKE keko-losgr.
*DATA : END OF it_ckikekokey.

* Itemization
DATA : BEGIN OF it_kis1 OCCURS 0.
        INCLUDE STRUCTURE kis1.
DATA : losgr LIKE keko-losgr.
DATA : upgvc TYPE zupgvc.
DATA : indx TYPE zindex.
DATA : chk.
DATA : END OF it_kis1.

* by ig.moon 6/11/2009 {
DATA: stb         TYPE TABLE OF stpox    WITH HEADER LINE.

DATA: BEGIN OF gt_bom OCCURS 0,
         index  LIKE stpox-index,     " Index
         stufe  LIKE stpox-stufe,     " Level
         disst  LIKE stpox-disst,     " low-level code
         idnrk  LIKE stpox-idnrk,     " Object(Mat)
         posnr  LIKE stpox-posnr,     " BOM item number
         hdnfo  LIKE stpox-hdnfo,     " Indicator: header info record
         mtart  LIKE stpox-mtart,     " mat type
         xchar  LIKE stpox-xchar,     " batch-mgt
         dumps  LIKE stpox-dumps,     " Phantom.
         stkkz  LIKE stpox-stkkz,     " assemble ind
         schgt  LIKE stpox-schgt,     " ind-bulk material
         mstae  LIKE stpox-mstae,     " mat-status
         mmsta  LIKE stpox-mmsta,     " mat-status(plant)
         menge  LIKE stpox-menge,     " Qty
         ausss  LIKE stpox-ausss,     " assembly scrap
         kausf  LIKE stpox-kausf,     " component scrap
         bausf  LIKE stpox-bausf,     " assembly scrap%
         meins  LIKE stpox-meins,     " UoM
         sobsl  LIKE stpox-sobsl,     " Special.Proc.
         rgekz  LIKE stpox-rgekz,     " b/f ind.
         lgpro  LIKE stpox-lgpro,     " S.Loc
         matmk  LIKE stpox-matmk,     " Mat.Group
         postp  LIKE stpox-postp,     " Type
         sortf  LIKE stpox-sortf,     " SortString
         stawn  LIKE stpox-stawn,     " Duty Code
         xtlty  LIKE stpox-xtlty,     " BOM category (next level)
         xtlnr  LIKE stpox-xtlnr,     " BOM no.
         eitm   LIKE stpox-eitm,      " EndItem
         stgb   LIKE stpox-stgb,      " Str.Type
         ojtxb  LIKE stpox-ojtxb,     " description
         upgn   LIKE stpox-upgn,      " user field - upg
         chk,
      END OF gt_bom.

* }
DATA : it_khs1  LIKE STANDARD TABLE OF khs1
                WITH HEADER LINE .
* ZVCO_RP1 (Report Point Linkage)
DATA : it_zvco_rp1       LIKE STANDARD TABLE OF zvco_rp1
                         WITH HEADER LINE .
* Plant Info.
DATA : it_t001w LIKE STANDARD TABLE OF t001w
                WITH HEADER LINE .
* For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : BEGIN  OF it_cctr  OCCURS 0,
        shop  LIKE ty_ztco_shop_cc-shop,
        kostl LIKE csks-kostl.
DATA : END    OF it_cctr.

* KSBT Costing Rate BY KOSTL LSTAR
DATA : BEGIN OF it_koat_p OCCURS 0.
DATA :  gjahr    LIKE ccss-gjahr,
        poper    LIKE ccss-buper,
        kostl    LIKE csks-kostl,
        lstar    LIKE csla-lstar,
        elemt    LIKE kkb_split-elemt,
        w000     LIKE kkb_split-w000 ,
        waers    LIKE kkb_split-waers,
        total    LIKE kkb_split-w000 ,
        cp_%(16) TYPE p DECIMALS 7.
DATA : END OF it_koat_p.

DATA : BEGIN OF it_kostl_lstar_pct OCCURS 0.
DATA :  from_per LIKE cobk-perab,
        to_per   LIKE cobk-perab,
        objnr    LIKE coomco-objnr,
        kadky    LIKE sy-datum ,
        bidat    LIKE sy-datum .
        INCLUDE STRUCTURE it_koat_p.
DATA : END OF it_kostl_lstar_pct.

DATA : BEGIN OF it_coomco OCCURS 0.
        INCLUDE STRUCTURE coomco.
DATA :  kostl    LIKE csks-kostl,
        lstar    LIKE csla-lstar.
DATA : END OF it_coomco.

DATA : record_num TYPE i.
** Global Vriables
DATA : gv_verwe LIKE plko-verwe.
DATA : gv_tarkz LIKE cokl-tarkz.
DATA : gv_freig LIKE keko-freig.
DATA: gv_kalka    TYPE ck_kalka.

DATA: g_frdat  LIKE sy-datum,
      g_todat  LIKE sy-datum.
DATA: g_yrdtf LIKE sy-datum,
      g_yrdtt LIKE sy-datum.

RANGES: gr_poper FOR keko-poper.
DATA: gv_poper  LIKE keko-poper.


*----------------------------------------------------------------------*
*   DEFINITION                                                *
*----------------------------------------------------------------------*
DEFINE screen_period_d.
* Period Check No Longer than To_period
* P_PERBI
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*  DELETE   &1
*   WHERE   BDATJ =  P_BDATJ
*     AND   (   POPER <  P_PERAB
*          OR   POPER >  P_PERBI ).
*----End
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(13)  text-032. "Business Plan
SELECTION-SCREEN POSITION 15.
PARAMETERS : p_std RADIOBUTTON GROUP ra01
             USER-COMMAND  cty.
SELECTION-SCREEN COMMENT  25(13) text-031. "Standard Plan
SELECTION-SCREEN POSITION 39.
PARAMETERS : p_bpl RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_freig LIKE keko-freig DEFAULT 'X'.

* General Info.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY ID cac  OBLIGATORY.

PARAMETERS : p_versn LIKE rkpln-versn  MEMORY ID kvs  MODIF ID div.
PARAMETERS : p_klvar LIKE cki64a-klvar MEMORY ID krt  MODIF ID div.
PARAMETERS : p_tvers LIKE keko-tvers   DEFAULT '01'   MODIF ID div.
PARAMETERS : p_elehk LIKE tckh4-elehk  DEFAULT 'H1'   MODIF ID div.

SELECTION-SCREEN SKIP 1.

* Costing Type
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.

PARAMETERS : p_step1 RADIOBUTTON GROUP ra02,
             p_step2 RADIOBUTTON GROUP ra02.

PARAMETERS: p_plscn  LIKE plaf-plscn DEFAULT '901'.
SELECTION-SCREEN END OF BLOCK bl2.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY.
* periods
*selection-screen begin of line.
*selection-screen comment  1(30) text-021. "From
*selection-screen position 33.
PARAMETERS: p_perab LIKE covja-perab MEMORY ID vpe
            MODIF ID per.
*SELECTION-SCREEN COMMENT  52(3) TEXT-022. "To
*SELECTION-SCREEN POSITION 58.
*PARAMETERS: P_PERBI LIKE COVJA-PERBI MEMORY ID BPE
*            MODIF ID PER OBLIGATORY.
*selection-screen end of line.
SELECTION-SCREEN END OF BLOCK bl3.

* Option
SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-004.
SELECT-OPTIONS : s_matnr FOR keko-matnr MEMORY ID mat,
                 s_mtart FOR mara-mtart.
*PARAMETERS : P_DEL AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl4.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Default
  p_std   = 'X'.
  p_bpl   = ' '.
  p_step2 = 'X'.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Read Version / Costing Variant / Routing Usage
  PERFORM read_set_ver_cver.
* Check Input Values
  PERFORM chk_input_values.

AT SELECTION-SCREEN OUTPUT.
* Read Version / Costing Variant / Routing Usage
  PERFORM read_set_ver_cver.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP ra01.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Controlling Area Information
  PERFORM read_tka01.
  PERFORM determine_period.


  IF p_step1 = 'X'.
    PERFORM check_existing_data.
    PERFORM set_dates.
    PERFORM read_plaf_ltp_product.

  ELSE.
*   Read CCTRs in SHOP
    PERFORM read_cctr_in_shop.
*   Read KEKO
    PERFORM get_products.
    DESCRIBE TABLE it_ckikekokey LINES sy-index.
    CHECK sy-index > 0.

*   Read Base Information
    PERFORM read_base_info.
*   Read Cost Estimated Data
    PERFORM read_itemization.
    PERFORM read_ml_header.

*   Put data to distinguish Shop, Material Data
    PERFORM collect_itemization.

    PERFORM put_shop_mat_info.
    PERFORM calc_cost_by_cc.

    PERFORM create_final_itab.
*   Delete DB for New Records
    PERFORM del_date_fr_ztco_shop_pln_cc.
**  Update/Insert
    PERFORM update_ztco_sop_pln_cc.
  ENDIF.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Insert/Update Log.
  PERFORM up_ins_log.



*&---------------------------------------------------------------------*
*&      Form  READ_SET_VER_CVER
*&---------------------------------------------------------------------*
*       Read Version / Costing Variant
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_set_ver_cver.
* Set Cost Variant
  CASE 'X'.
    WHEN p_bpl.
      p_versn = 311.
      p_klvar   = c_bpl.
      gv_verwe  = '10'.
      gv_tarkz  = '001'.
      CLEAR gv_freig .
      gv_kalka = '11'.  "ABP
    WHEN p_std.
      p_versn = 0.
      p_klvar   = c_std.
      gv_verwe  = '1'.
      gv_tarkz  = '001'.
      gv_freig = p_freig. "'X'.
      gv_kalka = '01'.  "STD
  ENDCASE.

* Set Cost Variant
* p_klvar = c_bpl.
  gv_verwe = '10'.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = '001'
       IMPORTING
            output = gv_tarkz.

* Modify Screen
  LOOP AT SCREEN.
    CHECK screen-group1 = 'DIV'.
    screen-input = '0'.
    MODIFY SCREEN.
  ENDLOOP.

* Read Version
  CLEAR tck03.
  SELECT SINGLE * FROM tck03
                 WHERE klvar = p_klvar.
  IF sy-subrc <> 0.
    MESSAGE w000 WITH text-101 p_klvar.
  ENDIF.

  CLEAR tck05.
  SELECT SINGLE * FROM tck05
                 WHERE bwvar = tck03-bwvar.
  IF sy-subrc <> 0.
    MESSAGE w000 WITH text-102 tck03-bwvar.
  ENDIF.

*  p_versn = tck05-versn.

ENDFORM.                    " READ_SET_VER_CVER

*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUES
*&---------------------------------------------------------------------*
*       Check Input Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_input_values.
* Check Version / Costing Variant
  CLEAR tck03.
  SELECT SINGLE * FROM tck03
                 WHERE klvar = p_klvar.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-101 p_klvar.
  ENDIF.

  CLEAR tck05.
  SELECT SINGLE * FROM tck05
                 WHERE bwvar = tck03-bwvar.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-102 tck03-bwvar.
  ENDIF.

*  if   p_versn <> tck05-versn.
*    message e000 with text-103 p_versn.
*  endif.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*  IF p_perab > p_perbi.
*    MESSAGE e031.
*  ENDIF.
*----End
  IF p_perab > 12 OR p_perab < 1.
    MESSAGE e007.
  ENDIF.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start

*  IF p_perbi > 12 OR p_perbi < 1.
*    MESSAGE e007.
*  ENDIF.
*-----End
ENDFORM.                    " CHK_INPUT_VALUES

*&---------------------------------------------------------------------*
*&      Form  READ_BASE_INFO
*&---------------------------------------------------------------------*
*       Read Base Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_base_info.

* -  Progress Ind.
  PERFORM progress_ind USING '10'
                             text-210.

* RP Master Data
  CLEAR : it_zvco_rp1, it_zvco_rp1[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zvco_rp1
           FROM zvco_rp1
          WHERE plnnr = c_rp_plnnr.
  SORT it_zvco_rp1 BY usr00.
  IF  it_zvco_rp1[] IS INITIAL.
    MESSAGE e077.
  ENDIF.

* Plant Information.
  CLEAR t001w.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_t001w
           FROM t001w.

* Read Active Cost Component Structure
  CLEAR tckh4.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF tckh4
                  FROM tckh4
                 WHERE elehk = p_elehk
                   AND aktiv = 'X'.
  IF tckh4 IS INITIAL.
    MESSAGE e000 WITH text-250.
  ENDIF.

ENDFORM.                    " READ_BASE_INFO
*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
FORM read_tka01.
  CLEAR tka01.
  SELECT SINGLE * FROM tka01
                 WHERE kokrs = p_kokrs.
  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.

* Set Validity Date (Start)
  DATA : lv_datum LIKE sy-datum.
* Get First Date (From-Period)
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr              = p_bdatj
*     I_MONMIT             = 00
      i_periv              = tka01-lmona
      i_poper              = p_perab
    IMPORTING
      e_date               = lv_datum
    EXCEPTIONS
      input_false          = 1
      t009_notfound        = 2
      t009b_notfound       = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SELECT * INTO TABLE it_csla
    FROM csla
    WHERE kokrs = p_kokrs
      AND datab <= lv_datum
      AND datbi >= lv_datum.


ENDFORM.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  READ_BEGIN_ENDING_DATE
*&---------------------------------------------------------------------*
*       Get Period dates
*----------------------------------------------------------------------*
*      -->P_PECNT  Period
*      -->P_LD     Latest Date
*      -->P_ED     Eariest Date
*----------------------------------------------------------------------*
FORM read_begin_ending_date USING    p_pecnt
                                     p_ld
                                     p_ed.
* last Date
  CLEAR p_ld.
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr              = p_bdatj
*     I_MONMIT             = 00
      i_periv              = tka01-lmona
      i_poper              = p_pecnt
    IMPORTING
      e_date               = p_ld
    EXCEPTIONS
      input_false          = 1
      t009_notfound        = 2
      t009b_notfound       = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* First Date
  CLEAR p_ed.
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr              = p_bdatj
*     I_MONMIT             = 00
      i_periv              = tka01-lmona
      i_poper              = p_pecnt
    IMPORTING
      e_date               = p_ed
    EXCEPTIONS
      input_false          = 1
      t009_notfound        = 2
      t009b_notfound       = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " READ_BEGIN_ENDING_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_ITEMIZATION
*&---------------------------------------------------------------------*
*       Read Cost Estimated Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_itemization.

  DATA : it_t_kis1  LIKE STANDARD TABLE OF kis1
                    WITH HEADER LINE .
  DATA : it_t_khs1  LIKE STANDARD TABLE OF khs1
                    WITH HEADER LINE .
  DATA : wa_l_ckikekokey LIKE ckikekokey.

* Clear
  CLEAR : it_kis1, it_kis1[].
  CLEAR : it_khs1, it_khs1[].

* -  Progress Ind.
  DATA :  lv_pros_t   TYPE i.
  DATA :  lv_pros_cnt TYPE i.
  DATA :  lv_pros_%   TYPE i.
  DESCRIBE TABLE it_ckikekokey LINES lv_pros_t.
  lv_pros_cnt = 50 / lv_pros_t.
  lv_pros_% = '20'.

* Read Itemization Data (Using  Standard FM)
  LOOP AT it_ckikekokey .
* Key Data
    CLEAR : wa_l_ckikekokey.
    MOVE-CORRESPONDING it_ckikekokey TO  wa_l_ckikekokey.

* -  Progress Ind.
    lv_pros_% = lv_pros_% + lv_pros_cnt.
    PERFORM progress_ind USING lv_pros_%
                               text-230.

    CLEAR : it_t_kis1, it_t_kis1[].
    CLEAR : it_t_khs1, it_t_khs1[].

    CALL FUNCTION 'KKP4_READ_ITEMIZATION'
      EXPORTING
        i_kekokey                    = wa_l_ckikekokey
        i_no_zero_vpos               = ' '
*     IMPORTING
*       I_NUMBER_OF_CKIS_LINES       =
      TABLES
        i_t_kis1                     = it_t_kis1
        et_khs1                      = it_t_khs1
      EXCEPTIONS
        no_entry_found               = 1
        no_head_entry_found          = 2
        OTHERS                       = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Appending
    LOOP AT it_t_kis1.
      MOVE-CORRESPONDING it_t_kis1 TO it_kis1.
      it_kis1-losgr = it_ckikekokey-losgr.
      APPEND it_kis1. CLEAR it_kis1.

      IF it_t_kis1-typps = 'E'.
        gr_ccact-sign = 'I'.         gr_ccact-option = 'EQ'.
        gr_ccact-low  = it_t_kis1-parob.
        APPEND gr_ccact.
      ELSEIF it_t_kis1-typps = 'M'.
        it_mat-werks = it_t_kis1-werks.
        it_mat-matnr = it_t_kis1-matnr.
        it_mat-peinh = it_t_kis1-peinh.
        it_mat-pmeht = it_t_kis1-pmeht.
        APPEND it_mat.
      ENDIF.

      it_kstar-kstar =  it_t_kis1-kstar.
      APPEND it_kstar.
    ENDLOOP.
    APPEND LINES OF it_t_khs1 TO it_khs1.

    CLEAR it_ckikekokey.
  ENDLOOP.

  SORT it_kstar BY kstar.
  DELETE ADJACENT DUPLICATES FROM it_kstar COMPARING kstar.
  SORT gr_ccact BY low.
  DELETE ADJACENT DUPLICATES FROM gr_ccact.
  SORT it_mat   BY werks matnr.
  DELETE ADJACENT DUPLICATES FROM it_mat   COMPARING werks matnr.

  CLEAR it_kis1.
  CLEAR it_khs1.

ENDFORM.                    " READ_ITEMIZATION

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
*&      Form  PUT_SHOP_MAT_INFO
*&---------------------------------------------------------------------*
*       Put Shop Data and Material Data
*----------------------------------------------------------------------*
FORM put_shop_mat_info.
  DATA: l_idx LIKE sy-tabix.

  LOOP AT gt_shop WHERE typps = 'M'.
    l_idx = sy-tabix.

    CLEAR it_mat.
    READ TABLE  it_mat
      WITH KEY  matnr = gt_shop-llv_matnr
                werks = gt_shop-werks
                bwkey = gt_shop-bwkey
                bwtar = gt_shop-bwtar
      BINARY SEARCH.
    IF sy-subrc <> 0.
      MESSAGE w000 WITH 'Component material not found'
                        gt_shop-llv_matnr.
    ELSE.
      gt_shop-peinh = it_mat-peinh.
      gt_shop-meeht = it_mat-pmeht.
      gt_shop-fevor = it_mat-fevor.
      gt_shop-mtart = it_mat-mtart.
      gt_shop-kalnr = it_mat-kalnr.
      gt_shop-beskz = it_mat-beskz. "Procurement Type
      gt_shop-sobsl = it_mat-sobsl. "Special procurement type
      gt_shop-vspvb = it_mat-vspvb. "Proposed Supply Area
      gt_shop-matkl = it_mat-matkl.

      CALL FUNCTION 'Z_CO_SHOP_DETERMINE'
           EXPORTING
                f_typps = 'M'
                f_prctr = it_mat-prctr
                f_fevor = it_mat-fevor
                f_werks = it_mat-werks
                f_raube = it_mat-raube
           IMPORTING
                e_shop  = gt_shop-shop.

* by ig.moon 11/19/2009 {
   if gt_shop-shop is initial or it_mat-prctr is initial.
     select single prctr into gt_shop-shop
     from marc where MATNR eq it_mat-matnr
                 and WERKS eq it_mat-werks.
   endif.
* }

*      if gt_shop-werks = 'E001'.
*        gt_shop-shop = 'MXEX'.
*      else.
*        case it_mat-fevor .
*          when 'SPB' or 'SPD' or 'SPP'.
*            gt_shop-shop = 'MXSX'.
*          when 'SEA' or 'SEC'.
*            gt_shop-shop = 'MXEX'.
*          when others.
*            case it_mat-raube.
*              when 10.
*                gt_shop-shop = 'MXSX'.
*              when 11.
*                gt_shop-shop = 'MXBX'.
*              when 12.
*                gt_shop-shop = 'MXPX'.
*              when 13.
*                gt_shop-shop = 'MXTX'.
*              when 14.
*                gt_shop-shop = 'MXEX'.
*              when others.
*                gt_shop-shop = space.  "'MXTX'.
*            endcase.
*        endcase.
*      endif.

    ENDIF.

    MODIFY gt_shop INDEX l_idx
           TRANSPORTING fevor mtart kalnr beskz sobsl vspvb shop peinh
matkl.

  ENDLOOP.

ENDFORM.                    " PUT_SHOP_MAT_INFO

*&---------------------------------------------------------------------*
*&      Form  SET_FROM_TO_PERIOD
*&---------------------------------------------------------------------*
*       Period (From To) FSC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
*&      Form  ATT_SUBDATA_FOR_M_TO_PAR
*&---------------------------------------------------------------------*
*       Attach Sub Exploded data to the parents records.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM att_subdata_for_m_to_par.
*
*  CLEAR   it_tmp_pe.
*  CLEAR   it_tmp_pren.
*  CLEAR   it_add_tmp_scost.
*
*  DATA :  it_l_sub_mip LIKE STANDARD TABLE OF gt_shop
*                       WITH HEADER LINE .
*  CLEAR : it_l_sub_mip, it_l_sub_mip[].
*
** Multiply with Period key
** Multiply data( X the number of period )
*  PERFORM mult_x_temp_itab_by_period.
*
** Making IT_L_SUB_MIP (With the Consideration of period)
*  LOOP AT it_add_tmp_scost.
*    LOOP AT it_tmp_pren
*                   WHERE llv_matnr = it_add_tmp_scost-llv_matnr
*                     AND bdatj = it_add_tmp_scost-bdatj
*                     AND poper = it_add_tmp_scost-poper.
** Copy   From IT_ADD_TMP_SCOST   to IT_L_SUB_MIP
*      MOVE-CORRESPONDING it_add_tmp_scost  TO it_l_sub_mip.
** Copy   From IT_TMP_PREN        to IT_L_SUB_MIP
*      MOVE-CORRESPONDING it_tmp_pren-sub   TO it_l_sub_mip-sub.
** Comp.
*      it_l_sub_mip-par_kalnr = it_add_tmp_scost-par_kalnr.
*      it_l_sub_mip-par_kadky = it_add_tmp_scost-par_kadky.
*      it_l_sub_mip-kalnr = it_tmp_pren-kalnr.
*      it_l_sub_mip-elemt = it_tmp_pren-elemt.
** Costs
*      it_l_sub_mip-wertn = it_tmp_pren-wertn * it_l_sub_mip-menge.
*      it_l_sub_mip-wrtfx = it_tmp_pren-wrtfx * it_l_sub_mip-menge.
*      it_l_sub_mip-wdiff = it_tmp_pren-wdiff * it_l_sub_mip-menge.
** Append
*      APPEND it_l_sub_mip.
*      CLEAR  it_l_sub_mip.
*
*      CLEAR it_tmp_pren.
*    ENDLOOP.
*    CLEAR it_add_tmp_scost.
*  ENDLOOP.
*
*  CLEAR  it_l_sub_mip.
*
*
** Mtart
*  LOOP AT  it_l_sub_mip WHERE mtart EQ space.
*    SELECT SINGLE mtart FROM mara
*                   INTO it_l_sub_mip-mtart
*                  WHERE matnr = it_l_sub_mip-artnr.
*    MODIFY it_l_sub_mip.
*    CLEAR  it_l_sub_mip.
*  ENDLOOP.
*
** Append
*  APPEND LINES OF it_l_sub_mip TO gt_shop.
*  CLEAR gt_shop.
*
*ENDFORM.                    " ATT_SUBDATA_FOR_M_TO_PAR

*&---------------------------------------------------------------------*
*&      Form  MULT_X_TEMP_ITAB_BY_PERIOD
*&---------------------------------------------------------------------*
*       Multiply with Period key (Temp. Itab)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM mult_x_temp_itab_by_period.
** Multiply with Period key
** Multiply data( X the number of period )
*  DATA : it_dup_tmp_scost LIKE STANDARD TABLE OF it_add_tmp_scost
*                          WITH HEADER LINE .
*  DATA : lv_per_cnt LIKE it_dup_tmp_scost-from_per.
*  it_dup_tmp_scost[] = it_add_tmp_scost[].
*
*  CLEAR : it_add_tmp_scost, it_add_tmp_scost[].
*
*  LOOP AT it_dup_tmp_scost.
*    CLEAR lv_per_cnt.
*    lv_per_cnt = it_dup_tmp_scost-from_per.
*
*    WHILE lv_per_cnt <= it_dup_tmp_scost-to_per  .
*      MOVE-CORRESPONDING it_dup_tmp_scost TO  it_add_tmp_scost.
*      it_add_tmp_scost-poper = lv_per_cnt.
** Append
*      APPEND it_add_tmp_scost.
*      CLEAR  it_add_tmp_scost.
** Period Counter
*      lv_per_cnt = lv_per_cnt + 1.
*    ENDWHILE.
*  ENDLOOP.
*
** Check Period Range
**Issue # 20041201-001 requested by HS CHO
**Changed by wskim,on 01112005
**-----Start
**   screen_period_d  it_add_tmp_scost.
**-----End
*  SORT it_add_tmp_scost BY bdatj poper llv_matnr .
*
*ENDFORM.                    " MULT_X_TEMP_ITAB_BY_PERIOD


*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_IN_SHOP
*&---------------------------------------------------------------------*
*       Read CCtrs linked to SHOP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr_in_shop.
*REFER: ZACO19U_SHOP_SUMMARY_NEW
* Read CCtrs
  CLEAR : it_costcenterlist, it_costcenterlist[].
  CLEAR : it_return, it_return[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
       EXPORTING
            controllingarea = p_kokrs
            date_from       = g_frdat
            costcentergroup = c_gp_kostl_e
       TABLES
            costcenterlist  = it_costcenterlist
            return          = it_return.

  IF it_costcenterlist[] IS INITIAL.
    MESSAGE e080 WITH c_gp_kostl_e p_kokrs g_frdat.
  ENDIF.

* Read SHOP (Linkage bwtween CCtrs and Shops)
* Read Hierarchy From Object ID, Read CCtr from CCgrp 'HMMA-SHOP'
  PERFORM read_hi_fr_setid(saplzgco_global_form)
                            TABLES it_nodes
                                   it_values
                            USING  p_kokrs
                                   '0101'
                                   c_gp_kostl_e.
  CLEAR : it_cctr , it_cctr[].

  LOOP AT it_costcenterlist.
    CLEAR it_values.
    LOOP AT it_values WHERE vfrom =< it_costcenterlist-costcenter
                        AND vto   => it_costcenterlist-costcenter.
    ENDLOOP.
    IF sy-subrc = 0.
      LOOP AT it_nodes WHERE setid  = it_values-setid.
        it_cctr-shop  =  it_nodes-shortname.
        it_cctr-kostl =  it_costcenterlist-costcenter.
        APPEND  it_cctr.
        CLEAR   it_cctr.
      ENDLOOP.
    ENDIF.
    CLEAR it_costcenterlist.
  ENDLOOP.

ENDFORM.                    " READ_CCTR_IN_SHOP

*&---------------------------------------------------------------------*
*&      Form  CAL_PERCENT_USING_KSBT
*&---------------------------------------------------------------------*
*       CALCULATE % (KSBT)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_percent_using_ksbt.


* Read Coponent Values - KSBT
  PERFORM read_comp_value_ksbt.

* Multiply with Period key
* Multiply data( X the number of period )
*  PERFORM multi_record_with_period_ksbt.

* Cal. Rate
  PERFORM cal_percent_ksbt.

ENDFORM.                    " CAL_PERCENT_USING_KSBT

*&---------------------------------------------------------------------*
*&      Form  get_shop_fr_routing
*&---------------------------------------------------------------------*
*       Read SHOP Info. from Routing/Production version
* refer: ZACO19U_SHOP_SUMMARY_NEW
*----------------------------------------------------------------------*
*      -->P_MATNR  Material
*      -->P_SHOP   Shop
*      -->P_WERKS  Plant
*----------------------------------------------------------------------*
FORM get_shop_fr_routing USING    f_matnr
                                       f_shop
                                       f_werks.

  CLEAR : crhd, plpo, plko.
* Read Shop From Routing
  DATA : lv_arbid LIKE plpo-arbid.
  DATA : lv_fdper LIKE sy-datum.
  DATA : lv_ldper LIKE sy-datum.

  gv_verwe  = '1'.

  CALL FUNCTION 'K_PERIODS_GET'
    EXPORTING
      par_igjahr          = p_bdatj
      par_ipoper          = p_perab
      par_kokrs           = p_kokrs
*     PAR_PREVP           = ' '
*     PAR_SPEOK           = ' '
*     PAR_NEXTP           = ' '
    IMPORTING
*     PAR_ANZBP           =
*     PAR_ANZSP           =
*     PAR_EGJAHR          =
*     PAR_EPOPER          =
      par_fdper           = lv_fdper
      par_ldper           = lv_ldper
    EXCEPTIONS
      kokrs_invalid       = 1
      poper_invalid       = 2
      OTHERS              = 3.

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
ENDFORM.                    " get_shop_fr_routing

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FROM_COOMCO
*&---------------------------------------------------------------------*
*       Read DATA From COOMCO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_from_coomco.
  DATA: l_kadky  LIKE coomco-kadky,
        l_versn  LIKE coomco-versn,
        l_lednr  LIKE coomco-lednr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = '00'
       IMPORTING
            output = l_lednr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = p_versn
       IMPORTING
            output = l_versn.

* KSBT dose not have data by period in case of (BP/Qarter) planning
* Select data with date range

* From period - To period
  READ TABLE it_ckikekokey INDEX 1.
  l_kadky = it_ckikekokey-kadat.

* Clear
  REFRESH it_coomco.

* KEPH -> 'C' COKL/KEKO/KEPH
* K_KKB_SPLITTING_CONVERT

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_coomco
           FROM coomco
          WHERE lednr = l_lednr
            AND objnr IN gr_ccact
            AND gjahr = p_bdatj
            AND versn = l_versn
            AND tvers = p_tvers     "PLAN = 01, ACTUAL = 04
            AND ( kadky <= l_kadky AND bidat >= l_kadky )
            AND tarkz = gv_tarkz    "Price indicator;
            AND kkzma = space.      "Additive - > X

  SORT it_coomco BY kalnr kkzma kkzmm kkzst.

  DELETE ADJACENT DUPLICATES FROM it_coomco.
  CLEAR : it_coomco.

  IF it_coomco[] IS INITIAL .
    MESSAGE e081.
  ENDIF.


** Activity qty total
*  DATA : it_cosl_temp LIKE cosl OCCURS 0 WITH HEADER LINE.
*  DATA : l_cnt(3) TYPE n,
*         l_field(20).
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cosl_temp
*           FROM cosl
*           FOR ALL ENTRIES IN it_coomco
*           WHERE objnr = it_coomco-objnr
*             AND lednr = it_coomco-lednr
*             AND gjahr = it_coomco-gjahr
*             AND versn = it_coomco-versn.
**            and vrgng = 'RKP2'
*
*  LOOP AT it_cosl_temp.
*    CLEAR it_cosl.
*
*    READ TABLE it_onrkl WITH KEY objnr = it_cosl_temp-objnr.
*    it_cosl-kostl = it_onrkl-kostl.
*    it_cosl-lstar = it_onrkl-lstar.
*    it_cosl-objnr = it_cosl_temp-objnr.
*    it_cosl-meinh = it_cosl_temp-meinh.
*
*    CLEAR : l_cnt.
*    DO 12 TIMES.
*      l_cnt = l_cnt + 1.
*      IF l_cnt = p_perab.
**-------plan qty
*        CONCATENATE 'IT_COSL_TEMP-LST' l_cnt INTO l_field.
*        ASSIGN  (l_field)    TO   <f_field> .
*        it_cosl-menge = <f_field>.
*      ENDIF.
*    ENDDO.
*    COLLECT  it_cosl.
*  ENDLOOP.
*
*  SORT it_cosl BY objnr.

ENDFORM.                    " READ_DATA_FROM_COOMCO

*&---------------------------------------------------------------------*
*&      Form  READ_COMP_VALUE_KSBT
*&---------------------------------------------------------------------*
*       Read Coponent Values - KSBT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_comp_value_ksbt.

  DATA : it_l_keph    LIKE STANDARD TABLE OF keph
                      WITH HEADER LINE .
  DATA : it_l_kkb_split
                      LIKE STANDARD TABLE OF  kkb_split
                      WITH HEADER LINE .
  DATA : wa_l_ckikekokey LIKE ckikekokey .


* Clear
  CLEAR : it_kostl_lstar_pct, it_kostl_lstar_pct[].

  LOOP AT it_coomco.
* Move Keys
    MOVE-CORRESPONDING it_coomco TO wa_l_ckikekokey  .
* Read KEPH
    CLEAR : it_l_keph, it_l_keph[].
    CALL FUNCTION 'CK_F_KEKO_KEPH_DIRECT_READ'
      EXPORTING
        f_kekokey              = wa_l_ckikekokey
        read_keko              = space
        read_keph              = 'X'
*       READ_ONLY_BUFFER       = ' '
*       READ_ONLY_DB           = ' '
*       CACHED_READ            = ' '
*       KEPH_MANDATORY         = 'X'
*     IMPORTING
*       F_KEKO                 =
      TABLES
        i_keph                 = it_l_keph
      EXCEPTIONS
        data_not_found         = 1
        wrong_call             = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      MESSAGE e079 WITH it_tmp_pe-llv_matnr
                        it_tmp_pe-werks.
    ENDIF.

* Read Keph (1 line)
    CLEAR it_l_keph.
    CLEAR keph.
    READ TABLE it_l_keph INTO keph INDEX 1.
* Read Costs by Cost Comp.
    CLEAR : it_l_kkb_split, it_l_kkb_split[].
    CALL FUNCTION 'K_KKB_SPLITTING_CONVERT'
         EXPORTING
              i_elehk     = tckh4-elehk
              i_sicht     = '01'
              i_keart     = keph-keart
              i_losfx     = keph-losfx
              i_waers     = tka01-waers
         TABLES
              t_keph      = it_l_keph
              t_kkb_split = it_l_kkb_split.
*ELEMT      -> Cost Component Number
*ELEMT_TEXT
*W000       -> PLAN
    LOOP AT it_l_kkb_split.
      MOVE-CORRESPONDING it_coomco      TO it_kostl_lstar_pct.
      MOVE-CORRESPONDING it_l_kkb_split TO it_kostl_lstar_pct.
      APPEND it_kostl_lstar_pct.
      CLEAR  it_kostl_lstar_pct.
    ENDLOOP.
  ENDLOOP.

* Get CCTR / AT
  LOOP AT it_kostl_lstar_pct.
*   Object -> Kostl Key
    CALL FUNCTION 'OBJECT_KEY_GET_KL'
         EXPORTING
              objnr       = it_kostl_lstar_pct-objnr
         IMPORTING
              kokrs       = p_kokrs
              kostl       = it_kostl_lstar_pct-kostl
              lstar       = it_kostl_lstar_pct-lstar
         EXCEPTIONS
              not_found   = 1
              wrong_obart = 2
              OTHERS      = 3.
    IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*   From Period
    PERFORM set_from_to_period
      USING it_kostl_lstar_pct-kadky
            p_kokrs
            it_kostl_lstar_pct-gjahr
            it_kostl_lstar_pct-from_per.
*   TO Period
    PERFORM set_from_to_period
      USING it_kostl_lstar_pct-bidat
            p_kokrs
            it_kostl_lstar_pct-gjahr
            it_kostl_lstar_pct-to_per.

    MODIFY it_kostl_lstar_pct.
    CLEAR it_kostl_lstar_pct.
  ENDLOOP.

  CLEAR it_kostl_lstar_pct.

ENDFORM.                    " READ_COMP_VALUE_KSBT

*&---------------------------------------------------------------------*
*&      Form  CAL_PERCENT_KSBT
*&---------------------------------------------------------------------*
*       Cal. Rate KSBT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_percent_ksbt.

  CLEAR : it_kostl_lstar_pct.
  CLEAR : it_koat_p, it_koat_p[].

  DATA :  it_tmp_% LIKE STANDARD TABLE OF it_koat_p
                   WITH HEADER LINE .

  LOOP AT it_kostl_lstar_pct.
    MOVE-CORRESPONDING it_kostl_lstar_pct TO it_koat_p.
    MOVE-CORRESPONDING it_kostl_lstar_pct TO it_tmp_%.
    COLLECT it_koat_p.
    CLEAR   it_koat_p.
* Cal SUM
    CLEAR it_tmp_%-elemt.
    COLLECT it_tmp_%.
    CLEAR   it_tmp_%.
    CLEAR   it_kostl_lstar_pct.
  ENDLOOP.

  SORT it_koat_p BY gjahr poper kostl lstar elemt.
  SORT it_tmp_%  BY gjahr poper kostl lstar elemt.

  LOOP AT it_koat_p.
    CLEAR it_tmp_%.
    READ TABLE  it_tmp_% WITH KEY  gjahr = it_koat_p-gjahr
                                   poper = it_koat_p-poper
                                   kostl = it_koat_p-kostl
                                   lstar = it_koat_p-lstar.
* Total
    it_koat_p-total = it_tmp_%-w000.
* %
    IF it_koat_p-total <> 0.
      it_koat_p-cp_% = it_koat_p-w000  /  it_koat_p-total.
    ENDIF.
* Modify
    MODIFY it_koat_p.
    CLEAR it_koat_p.
  ENDLOOP.

  SORT it_koat_p BY kostl lstar elemt.

ENDFORM.                    " CAL_PERCENT_KSBT

*&---------------------------------------------------------------------*
*&      Form  UP_INS_LOG
*&---------------------------------------------------------------------*
*       Update / Insert Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM up_ins_log.
* LOG
*  DESCRIBE TABLE gt_shop LINES sy-tfill.
  WRITE : / 'No. of Created Records : ' , record_num.
  WRITE : / 'Created date           : ' , sy-datum.
  WRITE : / 'Created By             : ' , sy-uname.
  SKIP 1.
  WRITE : / text-190.

* Success
  MESSAGE s009 WITH 'Data Creation'.

ENDFORM.                    " UP_INS_LOG

*&---------------------------------------------------------------------*
*&      Form  CREATE_FINAL_ITAB
*&---------------------------------------------------------------------*
FORM create_final_itab.

  DELETE  gt_shop_cc
   WHERE wertn IS initial
     AND wrtfx IS initial.

  SORT gt_shop     BY typps kstar resou.
  SORT gt_shop_cc  BY typps kstar resou.

  LOOP AT gt_shop.
    MOVE-CORRESPONDING gt_shop TO it_shop_pln.
    APPEND it_shop_pln.
  ENDLOOP.

  LOOP AT gt_shop_cc.
    MOVE-CORRESPONDING gt_shop_cc TO it_shop_pln_cc.
    APPEND it_shop_pln_cc.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DEL_DATE_FR_ZTCO_SHOP_PLN_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM del_date_fr_ztco_shop_pln_cc.

  DELETE FROM ztco_shop_pln
        WHERE
              kokrs        =  p_kokrs
          AND bdatj        =  p_bdatj
          AND poper        =  p_perab "_poper
          AND klvar        =  p_klvar
          AND artnr    IN r_matnr.

  DELETE FROM ztco_shop_pln_cc
        WHERE
              kokrs        =  p_kokrs
          AND bdatj        =  p_bdatj
          AND poper        =  p_perab "_poper
          AND klvar        =  p_klvar
          AND artnr    IN r_matnr.

  COMMIT WORK AND WAIT.

ENDFORM.                    " DEL_DATE_FR_ZTCO_SHOP_PLN_CC
*&---------------------------------------------------------------------*
*&      Form  update_ztco_SOP_PLN_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_sop_pln_cc.
  DATA: l_err(80) TYPE c.

  CLEAR : record_num.
  LOOP AT it_shop_pln.
* LOG
    it_shop_pln-erdat = sy-datum.
    it_shop_pln-erzet = sy-uzeit.
    it_shop_pln-ernam = sy-uname.
    it_shop_pln-bdatj = p_bdatj.
    it_shop_pln-poper = p_perab. "gv_poper.

* CURKY
*    IF it_shop_pln-hwaer EQ space .
*      it_shop_pln-hwaer = tka01-waers.
*    ENDIF.

    MOVE-CORRESPONDING  it_shop_pln TO ztco_shop_pln.
    INSERT ztco_shop_pln .

    record_num = record_num + 1.
    IF sy-subrc <> 0.
      CONCATENATE '***INSERT ERR ' ztco_shop_pln-artnr
                      ztco_shop_pln-resou  INTO l_err.
      PERFORM progress_ind USING '90' l_err.
    ENDIF.
    CLEAR it_shop_pln.
  ENDLOOP.

*  BREAK-POINT.
  CLEAR : record_num.
  LOOP AT it_shop_pln_cc.
    it_shop_pln_cc-bdatj = p_bdatj.
    it_shop_pln_cc-poper = p_perab. "gv_poper.
* LOG
*    it_shop_pln_cc-erdat = sy-datum.
*    it_shop_pln_cc-erzet = sy-uzeit.
*    it_shop_pln_cc-ernam = sy-uname.
* CURKY
*    if it_shop_pln_Cc-hwaer eq space .
*      it_shop_pln_Cc-hwaer = tka01-waers.
*    endif.

    MOVE-CORRESPONDING  it_shop_pln_cc TO ztco_shop_pln_cc.
    INSERT ztco_shop_pln_cc.

    record_num = record_num + 1.
    IF sy-subrc <> 0.
      CONCATENATE '***INSERT ERR '
                  ztco_shop_pln_cc-artnr ztco_shop_pln_cc-resou
                  INTO l_err.
      PERFORM progress_ind USING '90' l_err.
    ENDIF.
    CLEAR it_shop_pln_cc.
  ENDLOOP.


ENDFORM.                    " update_ztco_SOP_PLN_CC
*&---------------------------------------------------------------------*
*&      Form  get_products
*&---------------------------------------------------------------------*
FORM get_products.
  DATA lt_keko TYPE TABLE OF ckiuser WITH HEADER LINE.

  CLEAR: it_ckikekokey.
  REFRESH: it_ckikekokey, r_matnr.

  r_matnr-option = 'EQ'.
  r_matnr-sign   = 'I'.
  SELECT * FROM ztco_shop_plh
     WHERE kokrs = p_kokrs
       AND bdatj = p_bdatj
       AND poper = p_perab
       AND klvar = p_klvar
       AND artnr IN s_matnr.
    r_matnr-low = ztco_shop_plh-artnr.
    APPEND r_matnr.
  ENDSELECT.
  DESCRIBE TABLE r_matnr LINES sy-index.
  CHECK sy-index > 0.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_keko
    FROM keko
   WHERE matnr IN r_matnr
     AND kalka = gv_kalka
     AND tvers = p_tvers
     AND bdatj = p_bdatj
     AND poper IN gr_poper      "STD=Quarterly...ABP=Monthly
     AND kokrs = p_kokrs
     AND stnum <> space        "BoM Exist
     AND tvers = '01'
     AND freig = gv_freig.
* FREIG = 'X' Only Mark Released - Standard Plan
* FREIG = ' ' No   Mark Released - Business Plan

  IF sy-subrc = 0.
* Stock Trf.
    LOOP AT lt_keko.
      IF lt_keko-sobes = '7'.
        READ TABLE lt_keko WITH KEY matnr = lt_keko-matnr
                                    werks = lt_keko-sowrk
                                    klvar = lt_keko-klvar
                                    kadky = lt_keko-kadky.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING lt_keko TO it_ckikekokey.

          APPEND it_ckikekokey.
          CLEAR it_ckikekokey.
        ENDIF.

      ELSE.
        MOVE-CORRESPONDING lt_keko TO it_ckikekokey.

        APPEND it_ckikekokey.
        CLEAR it_ckikekokey.
      ENDIF.

    ENDLOOP.
  ENDIF.

  SORT it_ckikekokey BY kalnr
                        kadky DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_ckikekokey COMPARING kalnr.

  SORT it_ckikekokey BY disst DESCENDING
                        kalst ASCENDING .    " Sort by level

ENDFORM.                    " get_products
*&---------------------------------------------------------------------*
*&      Form  read_ml_header
*&---------------------------------------------------------------------*
FORM read_ml_header.

  DATA: lt_ckmlhd LIKE it_mat OCCURS 0 WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ckmlhd
    FROM ckmlhd
         INNER JOIN mara
                 ON ckmlhd~matnr = mara~matnr
         INNER JOIN marc
                 ON marc~werks = ckmlhd~bwkey
                AND marc~matnr = ckmlhd~matnr
    FOR ALL entries IN it_mat
    WHERE ckmlhd~bwkey = it_mat-werks
      AND ckmlhd~matnr = it_mat-matnr.

  SORT lt_ckmlhd BY matnr bwkey.

*-get MIP information
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr LIKE marc-matnr,
          fevor LIKE marc-fevor,
        END OF lt_marc.
  SELECT matnr fevor INTO TABLE lt_marc
     FROM marc
     FOR ALL ENTRIES IN it_mat
     WHERE matnr = it_mat-matnr
       AND fevor <> space.
  SORT lt_marc BY matnr.

  DATA: l_idx LIKE sy-tabix.
  LOOP AT it_mat.
    l_idx = sy-tabix.
    READ TABLE lt_ckmlhd WITH KEY matnr = it_mat-matnr
                                  bwkey = it_mat-werks
                              BINARY SEARCH.
    it_mat-kalnr = lt_ckmlhd-kalnr.
    it_mat-bwkey = lt_ckmlhd-bwkey.
    it_mat-bwtar = lt_ckmlhd-bwtar.
    it_mat-raube = lt_ckmlhd-raube.  "SHOP code

    READ TABLE lt_marc WITH KEY matnr = it_mat-matnr.
    IF sy-subrc = 0.
      it_mat-fevor = lt_marc-fevor.  "production scheduler
    ELSE.
      CLEAR it_mat-fevor.
    ENDIF.

    it_mat-vspvb = lt_ckmlhd-vspvb.
    it_mat-beskz = lt_ckmlhd-beskz.
    it_mat-mtart = lt_ckmlhd-mtart.
    it_mat-sobsl = lt_ckmlhd-sobsl.
    it_mat-prctr = lt_ckmlhd-prctr.
    it_mat-matkl = lt_ckmlhd-matkl.

    MODIFY it_mat INDEX l_idx.
  ENDLOOP.

  SORT it_mat BY matnr werks bwkey bwtar.

ENDFORM.                    " read_ml_header
*&---------------------------------------------------------------------*
*&      Form  read_plaf_ltp_product
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_plaf_ltp_product.
* if p_ltp = 'X' => only plaf exist product
* else           => All product
  RANGES: lr_pedtr FOR plaf-pedtr.
  DATA : it_plaf_temp   LIKE plaf OCCURS 0 WITH HEADER LINE.
  DATA : it_plaf LIKE ztco_shop_plh OCCURS 0 WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_plaf_temp
   FROM plaf AS a
   INNER JOIN mara AS b
      ON a~matnr = b~matnr
   WHERE a~plscn = p_plscn              "Scenario
     AND a~sobes = 'E'                  "Special procurement
     AND a~stlan = '6'                  "BOM usage
     AND a~pedtr BETWEEN g_yrdtf AND g_yrdtt
     AND a~matnr IN s_matnr
     AND b~mtart IN s_mtart.

  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'No plan found in LTP'.
  ELSE.
    LOOP AT it_plaf_temp.
      it_plaf-mandt = sy-mandt.
      it_plaf-kokrs = p_kokrs.
      it_plaf-klvar = p_klvar.
      it_plaf-bdatj = it_plaf_temp-pedtr(4).
      it_plaf-poper = it_plaf_temp-pedtr+4(2).
      it_plaf-artnr = it_plaf_temp-matnr.
      it_plaf-pwwrk = it_plaf_temp-pwwrk.
      it_plaf-meins = it_plaf_temp-meins.
      it_plaf-gsmng = it_plaf_temp-gsmng.
      COLLECT it_plaf.
    ENDLOOP.
  ENDIF.

  DELETE FROM ztco_shop_plh
     WHERE kokrs = p_kokrs
       AND bdatj = p_bdatj
       AND poper = p_perab
       AND klvar = p_klvar
       AND artnr IN s_matnr.

  INSERT ztco_shop_plh FROM TABLE it_plaf.
  record_num = sy-dbcnt.

ENDFORM.                    " read_plaf_ltp_product
*&---------------------------------------------------------------------*
*&      Form  call_mlccs_read_pr
*&---------------------------------------------------------------------*
FORM call_mlccs_read_pr.

  READ TABLE it_ckikekokey INDEX 1.
* Read KEPH
*    CALL FUNCTION 'CK_F_KEKO_KEPH_DIRECT_READ'
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_prkeph
    FROM keph
    FOR ALL ENTRIES IN it_mat
    WHERE bzobj = '0'
      AND kalnr = it_mat-kalnr
      AND kalka = it_ckikekokey-kalka
      AND kadky = it_ckikekokey-kadky
      AND tvers = it_ckikekokey-tvers
      AND bwvar = it_ckikekokey-bwvar
      AND kkzma = space
      AND kkzmm = space.


*  RANGES :ir_prtyp FOR mlprkeph-prtyp,
*          ir_curtp FOR tkel-curtp.
*  DATA:   lt_kalnr            TYPE ckmv0_laobj_tbl WITH HEADER LINE,
*          it_prkeko_temp      TYPE mlccs_t_prkeko,
*          it_prkeph_temp      TYPE mlccs_t_prkeph.
*
*  DATA : it_l_keph    LIKE STANDARD TABLE OF keph
*                      WITH HEADER LINE .
*  DATA : it_l_kkb_split
*                      LIKE STANDARD TABLE OF  kkb_split
*                      WITH HEADER LINE .
*
** Read Costs by Cost Comp.
*  LOOP AT it_l_keph.
*    CLEAR : it_l_kkb_split, it_l_kkb_split[].
*    CALL FUNCTION 'K_KKB_SPLITTING_CONVERT'
*         EXPORTING
*              i_elehk     = tckh4-elehk
*              i_sicht     = '01'
*              i_keart     = it_l_keph-keart
*              i_losfx     = it_l_keph-losfx
*              i_waers     = tka01-waers
*         TABLES
*              t_keph      = it_l_keph
*              t_kkb_split = it_l_kkb_split.
*
*    MOVE TO it_prkeph. APPEND.
*
*  ENDLOOP.

* STD/ACT only
*  SORT it_mat BY matnr werks.
*  LOOP AT it_mat.
*    lt_kalnr-kalnr  = it_mat-kalnr.
*    APPEND lt_kalnr.
*  ENDLOOP.
*  SORT lt_kalnr BY kalnr.
*  DELETE ADJACENT DUPLICATES FROM lt_kalnr.
*
*  ir_prtyp = 'IEQS'.
*  APPEND ir_prtyp.
*  ir_curtp = 'IEQ10'.
*  APPEND ir_curtp.
*  CALL FUNCTION 'MLCCS_READ_PR'
*       EXPORTING
*            i_use_buffer            = space
*            i_bdatj_1               = p_bdatj
*            i_poper_1               = p_perab
*       IMPORTING
*            et_prkeko               = it_prkeko_temp
*            et_prkeph               = it_prkeph_temp
*       TABLES
*            it_kalnr                = lt_kalnr
*            ir_prtyp                = ir_prtyp
*            ir_curtp                = ir_curtp
*       EXCEPTIONS
*            no_data_found           = 1
*            input_data_inconsistent = 2
*            OTHERS                  = 3.

*  it_prkeko[] = it_prkeko_temp[].
*  it_prkeph[] = it_prkeph_temp[].

ENDFORM.                    " call_mlccs_read_pr
*&---------------------------------------------------------------------*
*&      Form  call_ckml_la_header_read
*&---------------------------------------------------------------------*
FORM call_ckml_la_header_read.
  DATA: l_kostl LIKE onrkl-kostl,
        l_lstar LIKE onrkl-lstar.
  DATA : l_kalnr LIKE ckmllahd-kalnr,
         ef_ckmllahd         TYPE ckmllahd.

  DATA : it_l_keph    LIKE STANDARD TABLE OF keph
                      WITH HEADER LINE .
  DATA : wa_l_ckikekokey LIKE ckikekokey .

* select object no. by Cost center & activity type
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_onrkl
     FROM onrkl
    WHERE kokrs = p_kokrs.


* Read DATA From COOMCO / COSL
  PERFORM read_data_from_coomco.

* CALCULATE % (KSBT)
*  PERFORM cal_percent_using_ksbt.

* by ig.moon 6/11/2009 {
  DATA $ix TYPE i.
  SORT it_onrkl BY objnr.
* }
  LOOP AT it_coomco.
    $ix = sy-tabix.
    CLEAR it_onrkl .
    READ TABLE it_onrkl WITH KEY objnr = it_coomco-objnr BINARY SEARCH.
    it_coomco-kostl = it_onrkl-kostl.
    it_coomco-lstar = it_onrkl-lstar.

    MODIFY it_coomco INDEX $ix TRANSPORTING kostl lstar.

** Move Keys
*    MOVE-CORRESPONDING it_coomco TO wa_l_ckikekokey  .
** Read KEPH
*    refresh it_l_keph.
*    CALL FUNCTION 'CK_F_KEKO_KEPH_DIRECT_READ'
*      EXPORTING
*        f_kekokey              = wa_l_ckikekokey
*        read_keko              = space
*        read_keph              = 'X'
**       READ_ONLY_BUFFER       = ' '
**       READ_ONLY_DB           = ' '
**       CACHED_READ            = ' '
**       KEPH_MANDATORY         = 'X'
**     IMPORTING
**       F_KEKO                 =
*      TABLES
*        i_keph                 = it_l_keph
*      EXCEPTIONS
*        data_not_found         = 1
*        wrong_call             = 2
*        OTHERS                 = 3.
* Read Keph (1 line)

    MOVE-CORRESPONDING it_coomco TO it_ckmlprkeph.
    APPEND it_ckmlprkeph.

  ENDLOOP.

*  LOOP AT it_act.
*    CLEAR l_kalnr .
**  Costing no
*    CALL FUNCTION 'CKML_LA_HEADER_READ'
*         EXPORTING
*              i_objnr                = it_act-objnr
*         IMPORTING
*              ef_ckmllahd            = ef_ckmllahd
*         TABLES
*              et_ckmllahd            = it_ckmllahd
*         EXCEPTIONS
*              no_key_specified       = 1
*              header_not_found_kalnr = 2
*              no_data_found          = 3
*              no_header_created      = 4
*              OTHERS                 = 5.
*    IF sy-subrc <> 0.
*    ELSE.
*      READ TABLE it_ckmllahd INDEX 1.
*      IF sy-subrc = 0 .
*        l_kalnr = it_ckmllahd-kalnr.
*        it_act-kalnr = l_kalnr.
*        MODIFY it_act. CLEAR it_act.
*      ELSE.
*        l_kalnr = ef_ckmllahd-kalnr.
*        it_act-kalnr = l_kalnr.
*        MODIFY it_act. CLEAR it_act.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.


*
*  DATA : l_kalnr LIKE ckmllahd-kalnr,
*         ef_ckmllahd         TYPE ckmllahd,
*         lt_ckmlprkeko_temp  TYPE mlccs_t_prkeko,
*         lt_ckmlprkeph_temp  TYPE mlccs_t_prkeph.
*
*
*    CALL FUNCTION 'CKML_LA_PERIOD_READ'
*         EXPORTING
*              i_kalnr              = l_kalnr
*              i_bdatj              = p_bdatj
*              i_poper              = p_perab
*              i_components         = 'X'
*         IMPORTING
*              et_prkeko            = lt_ckmlprkeko_temp
*              et_prkeph            = lt_ckmlprkeph_temp
*         EXCEPTIONS
*              interface_error      = 1
*              no_period_data_found = 2
*              OTHERS               = 3.
*
*
*    LOOP AT lt_ckmlprkeko_temp INTO ld_wa_db_keko .
*      it_ckmlprkeko =  ld_wa_db_keko .
*      APPEND  it_ckmlprkeko. CLEAR  it_ckmlprkeko.
*    ENDLOOP.
*
*    LOOP AT lt_ckmlprkeph_temp INTO ld_wa_db_keph .
*      it_ckmlprkeph =  ld_wa_db_keph .
*      APPEND  it_ckmlprkeph. CLEAR  it_ckmlprkeph.
*    ENDLOOP.
*  ENDLOOP.

ENDFORM.                    " call_ckml_la_header_read
*&---------------------------------------------------------------------*
*&      Form  call_kkek_cost_component
*&---------------------------------------------------------------------*
FORM call_kkek_cost_component.
  LOOP AT it_kstar.

    CALL FUNCTION 'KKEK_COST_COMPONENT_ELEMENT'
         EXPORTING
              elehk_imp         = 'H1'
              ktopl_imp         = 'HNA1'
              kstar_imp         = it_kstar-kstar
              message_on_screen = space
         IMPORTING
              elemt_exp         = it_kstar-elemt
         EXCEPTIONS
              calling_error     = 1
              OTHERS            = 2.

    MODIFY it_kstar. CLEAR it_kstar.
  ENDLOOP.
ENDFORM.                    " call_kkek_cost_component
*&---------------------------------------------------------------------*
*&      Form  calc_cost_by_cc
*&---------------------------------------------------------------------*
FORM calc_cost_by_cc.

  SELECT * INTO TABLE it_tckh3
      FROM tckh3
        WHERE elehk = 'H1'.


* CCS INFO
  PERFORM call_mlccs_read_pr.
  PERFORM call_ckml_la_header_read.
  PERFORM call_kkek_cost_component.
  SORT it_prkeko BY kalnr.
  SORT it_prkeph BY kalnr kkzst.
  SORT it_ckmlprkeko BY kalnr.
  SORT it_ckmlprkeph BY kalnr kkzst.

  CLEAR gt_shop_cc.
  REFRESH gt_shop_cc.

  LOOP AT gt_shop.
    gt_shop_cc = gt_shop.

    CASE gt_shop-typps.
      WHEN 'M' .    "Material
*      Current/Current var./GR/Scrap
        PERFORM making_shop_cc_material.
      WHEN 'E'.     "Activity
*      Current/Current var./GR/Scrap
        PERFORM making_shop_cc_activity .
      WHEN 'V'.
        CLEAR it_kstar .
        READ TABLE it_kstar WITH KEY kstar = gt_shop-kstar.
        gt_shop_cc-wertn = gt_shop-wertn.
        gt_shop_cc-elemt = it_kstar-elemt.
        COLLECT gt_shop_cc.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    " calc_cost_by_cc
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8293   text
*----------------------------------------------------------------------*
FORM making_shop_cc_material .
  DATA: l_output(16) TYPE p DECIMALS 6.

  CLEAR : ld_wa_db_keko,ld_wa_db_keph.
  CLEAR it_mat.
  READ TABLE it_mat WITH KEY matnr = gt_shop-llv_matnr
                             werks = gt_shop-werks
                             BINARY SEARCH.

  CHECK sy-subrc = 0 .
  CLEAR it_prkeko.
  READ TABLE it_prkeko WITH KEY kalnr = it_mat-kalnr
                                BINARY SEARCH.
  MOVE-CORRESPONDING it_prkeko TO ld_wa_db_keko .


  CLEAR it_prkeph.
  READ TABLE it_prkeph WITH KEY kalnr = it_mat-kalnr
                                kkzst = ''
                                BINARY SEARCH.


* by ig.moon {

  IF sy-subrc NE 0.
    PERFORM collect_cc_missing.
  ELSE.
    MOVE-CORRESPONDING it_prkeph TO ld_wa_db_keph .
* }

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
         EXPORTING
              input                = it_mat-peinh
              unit_in              = it_mat-pmeht
              unit_out             = gt_shop-meeht
         IMPORTING
              output               = l_output
         EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9.

    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    PERFORM make_element_value USING gt_shop-menge
                                     l_output.

  ENDIF.


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
FORM make_element_value USING f_menge
                              f_baseq.

  FIELD-SYMBOLS: <f_field> ,<f_field2> .
  TYPES: gt_amt TYPE p DECIMALS 6.

  DATA : l_cnt(3) TYPE n,
         l_field(25),
         l_amt TYPE gt_amt,
         l_amt_type(1).

  CLEAR : l_cnt, l_field, l_amt, l_amt_type.

  DO 40 TIMES.
    CLEAR : l_amt, gt_shop_cc-wertn, gt_shop_cc-wrtfx.

    l_cnt = l_cnt + 1.
* Overall value
    CLEAR it_tckh3.
    READ TABLE it_tckh3 WITH KEY el_hv = l_cnt.
    IF sy-subrc   =  0 .
      l_amt_type = 'A'.  "A : ALL
    ELSE.
* Fixed value
      CLEAR it_tckh3.
      READ TABLE it_tckh3 WITH KEY el_hf = l_cnt.
      IF sy-subrc   = 0 .
        l_amt_type = 'F'. "F : FIXED
      ENDIF.
    ENDIF.

    CONCATENATE 'LD_WA_DB_KEPH-KST' l_cnt INTO l_field.
    ASSIGN  (l_field)    TO   <f_field> .
    CLEAR l_amt.

* by ig.moon {
    IF f_baseq EQ 0.
      f_baseq = 1.
    ENDIF.
* }

    l_amt = f_menge * <f_field> / f_baseq.
    CHECK NOT l_amt IS INITIAL.

    IF l_amt_type = 'A'.
      gt_shop_cc-wertn = l_amt.
    ELSE.
      gt_shop_cc-wrtfx = l_amt.
    ENDIF.

    gt_shop_cc-elemt   = it_tckh3-elemt.

    CLEAR: gt_shop_cc-menge, gt_shop_cc-peinh.
    COLLECT gt_shop_cc.
  ENDDO.

ENDFORM.                    " make_element_value
*&---------------------------------------------------------------------*
*&      Form  making_shop_cc_activity
*&---------------------------------------------------------------------*
FORM making_shop_cc_activity.
  DATA: l_output(16) TYPE p DECIMALS 6.

  CLEAR : ld_wa_db_keko,ld_wa_db_keph.

  READ TABLE it_coomco WITH KEY kostl = gt_shop-kostl
                                lstar = gt_shop-lstar .
  READ TABLE it_csla WITH KEY lstar = gt_shop-lstar.

  CLEAR it_ckmlprkeko.
  READ TABLE it_ckmlprkeko WITH KEY kalnr = it_coomco-kalnr
                           BINARY SEARCH.
* CHECK sy-subrc = 0 .
  MOVE-CORRESPONDING it_ckmlprkeko TO ld_wa_db_keko .

  CLEAR it_ckmlprkeph.
  READ TABLE it_ckmlprkeph WITH KEY kalnr = it_coomco-kalnr
                                    kkzst = ''
                             BINARY SEARCH.
  CHECK sy-subrc = 0 .
  MOVE-CORRESPONDING it_ckmlprkeph TO ld_wa_db_keph .

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
       EXPORTING
            input                = it_coomco-losgr
            unit_in              = it_csla-leinh
            unit_out             = gt_shop-meeht
       IMPORTING
            output               = l_output
       EXCEPTIONS
            conversion_not_found = 1
            division_by_zero     = 2
            input_invalid        = 3
            output_invalid       = 4
            overflow             = 5
            type_invalid         = 6
            units_missing        = 7
            unit_in_not_found    = 8
            unit_out_not_found   = 9.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.


  PERFORM make_element_value USING gt_shop-menge
                                   l_output.

ENDFORM.                    " making_shop_cc_activity
*&---------------------------------------------------------------------*
*&      Form  set_dates
*&---------------------------------------------------------------------*
FORM set_dates.

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr              = p_bdatj
*     I_MONMIT             = 00
      i_periv              = tka01-lmona
      i_poper              = p_perab
    IMPORTING
      e_date               = g_yrdtf
    EXCEPTIONS
      input_false          = 1
      t009_notfound        = 2
      t009b_notfound       = 3
      OTHERS               = 4.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
        EXPORTING
          i_gjahr              = p_bdatj
*         I_MONMIT             = 00
          i_periv              = tka01-lmona
          i_poper              = p_perab
        IMPORTING
          e_date               = g_yrdtt
        EXCEPTIONS
          input_false          = 1
          t009_notfound        = 2
          t009b_notfound       = 3
          OTHERS               = 4.

ENDFORM.                    " set_dates
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
FORM pop_up USING    p_text
            CHANGING p_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            titel          = 'Check!'
            cancel_display = 'X'
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  check_existing_data
*&---------------------------------------------------------------------*
FORM check_existing_data.

  DATA: l_cnt TYPE i,
        l_text(100),
        l_answer.

  SELECT COUNT( * ) INTO l_cnt FROM ztco_shop_plh
     WHERE kokrs = p_kokrs
       AND bdatj = p_bdatj
       AND poper = p_perab
       AND klvar = p_klvar.
  IF l_cnt > 0.
    l_text =
'LTP planning data is already in the system. Are you sure refresh?'.

    PERFORM pop_up USING l_text
                   CHANGING l_answer.

    IF l_answer <> 'J'.
      MESSAGE e000 WITH 'Job canceled'.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_existing_data
*&---------------------------------------------------------------------*
*&      Form  determine_period
*&---------------------------------------------------------------------*
FORM determine_period.

*ABP
  IF p_bpl = 'X'.
    gr_poper-option = 'EQ'.
    gr_poper-sign   = 'I'.
    gr_poper-low = p_perab.
    APPEND gr_poper.

    gv_poper = p_perab.

*STD
  ELSE.

    gr_poper-option = 'BT'.
    gr_poper-sign   = 'I'.
    IF p_perab < 4.
      gr_poper-low = 1.  gr_poper-high = 3.  APPEND gr_poper.
      gv_poper = 1.
    ELSEIF p_perab < 7.
      gr_poper-low = 4.  gr_poper-high = 6.  APPEND gr_poper.
      gv_poper = 4.
    ELSEIF p_perab < 10.
      gr_poper-low = 7.  gr_poper-high = 9.  APPEND gr_poper.
      gv_poper = 7.
    ELSE.
      gr_poper-low = 10. gr_poper-high = 12. APPEND gr_poper.
      gv_poper = 10.
    ENDIF.

  ENDIF.

ENDFORM.                    " determine_period
*&---------------------------------------------------------------------*
*&      Form  collect_itemization
*&---------------------------------------------------------------------*
FORM collect_itemization.

  DATA : l_raube LIKE mara-raube,
         l_mtart LIKE mara-mtart.

  REFRESH: gt_shop.
  CLEAR: gt_shop.

*+ by ig.moon {
  SORT it_ckikekokey BY kalnr.
* }

*+ by ig.moon 6/11/2009 {
  DATA $ix TYPE i.

  it_kis1-indx = '0'.
  MODIFY  it_kis1 TRANSPORTING indx WHERE indx NE '0'.
  LOOP AT it_ckikekokey.
    PERFORM get_bom TABLES it_ckikekokey.
    SORT it_kis1 BY matnr typps .

    LOOP AT gt_bom WHERE dumps = space. " excluding Phantom
      READ TABLE it_kis1 WITH KEY matnr = gt_bom-idnrk
                                   upgvc = space
                                   indx = '0'
                                   chk = space.
      IF sy-subrc = 0.
        $ix = sy-tabix.
        it_kis1-indx  = gt_bom-index.
        it_kis1-chk = 'X'.
        MODIFY it_kis1 INDEX $ix TRANSPORTING indx chk.
      ENDIF.
    ENDLOOP.

    PERFORM get_upg TABLES it_ckikekokey.
  ENDLOOP.

* }

  LOOP AT it_kis1.
* Parents Material (FSC)
    READ TABLE it_ckikekokey
      WITH KEY kalnr = it_kis1-kalnr
      BINARY SEARCH.
    CHECK sy-subrc = 0.

* Controlling Area
    gt_shop-kokrs       = p_kokrs.
*   gt_shop-BDATJ
*   gt_shop-POPER

* Base Info.
    gt_shop-klvar       = p_klvar.

* SAVE Pareants KALNR
    gt_shop-par_kalnr = it_kis1-kalnr.
    gt_shop-par_werks = it_kis1-werks.
    gt_shop-lot_size  = it_kis1-losgr.
    gt_shop-artnr     = it_ckikekokey-matnr .
    gt_shop-ck_kalst  = it_ckikekokey-kalst.
    gt_shop-par_kadky = it_ckikekokey-kadky.

* by ig.moon 6/11/2009 {
    gt_shop-upgvc = it_kis1-upgvc.
* }

    MOVE-CORRESPONDING it_kis1 TO gt_shop.

    IF it_kis1-typps = 'M'.
      gt_shop-bwkey      = it_kis1-werks.
      gt_shop-llv_matnr  = it_kis1-matnr.
      PERFORM make_resou(zaco19u_shop_new)
                         USING 'M' it_kis1-werks it_kis1-matnr
                         CHANGING  gt_shop-resou.

    ELSEIF it_kis1-typps = 'E'.
      CALL FUNCTION 'Z_CO_SHOP_DETERMINE'
           EXPORTING
                f_typps = 'E'
                f_kostl = it_kis1-kostl
           IMPORTING
                e_shop  = gt_shop-shop.
*      gt_shop-shop = it_kis1-kostl(4).

      PERFORM make_resou(zaco19u_shop_new)
                         USING 'E' it_kis1-kostl it_kis1-lstar
                         CHANGING  gt_shop-resou.
      CLEAR gt_shop-kalnr.

    ELSEIF it_kis1-typps = 'V'.
      CLEAR gt_shop-kalnr.

      CALL FUNCTION 'Z_CO_SHOP_DETERMINE'
           EXPORTING
                f_typps = 'V'
                f_werks = gt_shop-par_werks
           IMPORTING
                e_shop  = gt_shop-shop.
*      if gt_shop-par_werks = 'E001'.
*        gt_shop-shop = 'MXEX'.
*      else.
*        gt_shop-shop = 'MXTX'.
*      endif.

    ENDIF.

    gt_shop-$peinh = gt_shop-peinh.
    gt_shop-$lot_size = gt_shop-lot_size.

    COLLECT  gt_shop.
    CLEAR    gt_shop.
  ENDLOOP.

  LOOP AT gt_shop.
    MOVE :
    gt_shop-$peinh TO gt_shop-peinh,
    gt_shop-$lot_size  TO gt_shop-lot_size.
    MODIFY gt_shop.
  ENDLOOP.

ENDFORM.                    " collect_itemization
*&---------------------------------------------------------------------*
*&      Form  collect_cc_missing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_cc_missing.

  gt_shop_cc-artnr            =   gt_shop-artnr.
  gt_shop_cc-typps            =   gt_shop-typps.
  gt_shop_cc-kstar            =   gt_shop-kstar.
  gt_shop_cc-resou            =   gt_shop-resou.

  CLEAR it_kstar .
  READ TABLE it_kstar WITH KEY kstar = gt_shop-kstar.
  gt_shop_cc-elemt = it_kstar-elemt.

  gt_shop_cc-wertn           = gt_shop-wertn.

  COLLECT gt_shop_cc. CLEAR gt_shop_cc.

ENDFORM.                    " collect_cc_missing
*&---------------------------------------------------------------------*
*&      Form  get_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bom TABLES f_ckiuser STRUCTURE ckiuser .

  DATA  l_capid TYPE capid.
  DATA  last_day LIKE f_ckiuser-aldat.

  SELECT SINGLE a~capid INTO l_capid
    FROM tck19a AS a
    JOIN tck03 AS b
      ON b~aufkz = a~aufkz
   WHERE b~klvar = f_ckiuser-klvar
     AND b~kalka = f_ckiuser-kalka
     AND b~bwvar = f_ckiuser-bwvar.

  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = f_ckiuser-aldat
       IMPORTING
            last_day_of_month = last_day
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  IF sy-subrc <> 0. ENDIF.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            capid                 = l_capid
            datuv                 = last_day
            mtnrv                 = f_ckiuser-matnr
            werks                 = f_ckiuser-werks
            stlan                 = f_ckiuser-stlan
            stlal                 = f_ckiuser-stalt
            mmory                 = '1'  "Memory use On(1)
            sanka =
               ' ' "Only Costing Relevency(inc.Phantom)
            ftrel =
               ' ' "stop explosion not relevant to production
            aumgb                 = ' '  "calculate scrap
            mdmps =
               'X' "Limited multi-lvl- explode phantom at leas
            mehrs =
               'X' "Multi-level explosion "Notes 729663 !!!
            rndkz =
               ' ' "Round off: ' '=always, '1'=never,
            emeng                 = 1  "Required quantity
       TABLES
            stb                   = stb
       EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            OTHERS                = 9.

  IF sy-subrc NE 0. ENDIF.

  __cls gt_bom.

  LOOP AT stb.
    MOVE-CORRESPONDING stb TO gt_bom.
    APPEND gt_bom.
  ENDLOOP.

  SORT gt_bom BY idnrk.

ENDFORM.                    " get_bom
*&---------------------------------------------------------------------*
*&      Form  get_upg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_upg TABLES f_ckiuser STRUCTURE ckiuser .

  CHECK f_ckiuser-stlan <> space.
  DATA  w_upg LIKE gt_bom.

  DATA : l_key(60),
         ln  TYPE i,
         $ix TYPE i.

* get UPG
  SORT : gt_bom BY index stufe ASCENDING,
         it_kis1 BY indx.

  LOOP AT gt_bom.

    IF gt_bom-stufe = 1.
      w_upg = gt_bom.
    ENDIF.

    READ TABLE it_kis1 WITH KEY indx = gt_bom-index BINARY SEARCH.

    IF sy-subrc = 0 AND it_kis1-typps = 'M'.
      $ix = sy-tabix.
      ln = strlen( w_upg-idnrk ).
      IF ln = 9.
        CONCATENATE w_upg-idnrk(7) '0' w_upg-idnrk+7(2)
            INTO it_kis1-upgvc.
      ELSE.
        it_kis1-upgvc = w_upg-idnrk.
      ENDIF.
      MODIFY it_kis1 INDEX $ix TRANSPORTING upgvc.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_upg
