*----------------------------------------------------------------------*
*   INCLUDE ZACO09L_F001                                               *
*----------------------------------------------------------------------*

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
      p_klvar = c_bpl.
      gv_record_type = 'B'. gv_verwe = '10'.
      gv_tarkz = '001'.     CLEAR gv_freig .
    WHEN p_std.
      p_klvar = c_std.
      gv_record_type = 'S'. gv_verwe = '1'.
      gv_tarkz       = '001'. gv_freig = p_freig. "'X'.
*    WHEN P_ACT.
*      P_KLVAR = C_ACT.
*      GV_RECORD_TYPE = 'A'.
  ENDCASE.

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

  p_versn = tck05-versn.

ENDFORM.                    " READ_SET_VER_CVER

*&---------------------------------------------------------------------*
*&      Form  SET_DEAFULT
*&---------------------------------------------------------------------*
*       Default Value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_deafult.
*  p_bpl = 'X'.
  p_std = 'X'.
ENDFORM.                    " SET_DEAFULT

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

  IF   p_versn <> tck05-versn.
    MESSAGE e000 WITH text-103 p_versn.
  ENDIF.
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

* Read Material Information (ALL)
  CLEAR : it_mat, it_mat[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_mat
           FROM ( macku INNER JOIN marc
             ON macku~matnr = marc~matnr )
                        INNER JOIN t001w
             ON marc~werks  = t001w~werks
            AND macku~bwkey = t001w~bwkey
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
         WHERE marc~ncost  = ' '.
*-----End
  CLEAR it_mat.
  SORT it_mat BY werks mtart.

* Read FSC/HALB Mat.
  CLEAR : it_fsc_mat, it_fsc_mat[].
  LOOP AT it_mat WHERE matnr IN s_matnr
*                  AND WERKS = C_FSC_PLANT
                   AND ( mtart = c_fsc OR mtart = c_halb ).

* Engine -> Only Plant 'E001'
    IF   it_mat-beskz = 'F'     "Procurement Type
     AND it_mat-sobsl = '40'    "Special procurement type
     AND it_mat-mtart = c_halb. "HALB.
      CONTINUE.
    ENDIF.

    CLEAR it_fsc_mat.
    MOVE-CORRESPONDING it_mat  TO it_fsc_mat .
    APPEND it_fsc_mat.
    CLEAR  it_fsc_mat.
    CLEAR  it_mat.
  ENDLOOP.

  IF it_fsc_mat[] IS INITIAL.
    MESSAGE e076 WITH c_fsc c_fsc_plant.
  ENDIF.

* RP Master Data
  CLEAR : it_zvco_rp1, it_zvco_rp1[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zvco_rp1
           FROM zvco_rp1
          WHERE plnnr = c_rp_plnnr.
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
*&      Form  READ_KEKO
*&---------------------------------------------------------------------*
*       Read KEKO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_keko.

* -  Progress Ind.
  PERFORM progress_ind USING '20'
                             text-220.

  CLEAR : it_ckikekokey, it_ckikekokey[].

  DATA : lv_pecnt(3) TYPE n.
  DATA : lv_ld       LIKE sy-datum.
  DATA : lv_ed       LIKE sy-datum.

* Comments by Author
* For Single Value of Valid Date
* Use "CK_F_MAT_COST_SELECT_SCREEN"

* Set First period
  lv_pecnt = p_perab.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*   WHILE lv_pecnt =< p_perbi.
  WHILE lv_pecnt =< p_perab.
*-----End
* Get Period dates
    PERFORM read_begin_ending_date
      USING lv_pecnt
            lv_ld
            lv_ed.

* Read KEKO with a Date Range

    SELECT * FROM keko
             APPENDING CORRESPONDING FIELDS OF TABLE  it_ckikekokey
             FOR ALL ENTRIES IN it_fsc_mat
             WHERE tvers = p_tvers
               AND bwvar = tck03-bwvar
               AND kalka = tck03-kalka
               AND kkzma = space         "ALL Data
               AND matnr = it_fsc_mat-matnr
               AND werks = it_fsc_mat-werks
               AND bwkey = it_fsc_mat-bwkey
               AND bwtar = it_fsc_mat-bwtar
               AND kokrs = p_kokrs
               AND ( kadat =< lv_ld AND bidat => lv_ed )
               AND freig = gv_freig.
*               AND FREIG = 'X'.
* FREIG = 'X' Only Mark Released - Standard Plan
* FREIG = ' ' No   Mark Released - Business Plan

* Period Counter
    lv_pecnt = lv_pecnt + 1.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
    SORT it_ckikekokey
      BY  bzobj  kalnr kalka  kadky DESCENDING.

    DELETE ADJACENT DUPLICATES FROM it_ckikekokey COMPARING kalnr.
*-----End
  ENDWHILE.

  CLEAR it_ckikekokey.
* Make Unique Key
  SORT it_ckikekokey
    BY
      bzobj
      kalnr
      kalka
      kadky
      tvers
      bwvar
      kkzma.
  DELETE ADJACENT DUPLICATES FROM it_ckikekokey.

* Check Valid Data
  IF it_ckikekokey[] IS INITIAL .
    MESSAGE e026.
  ENDIF.
ENDFORM.                    " READ_KEKO

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
    APPEND LINES OF it_t_kis1 TO it_kis1.
    APPEND LINES OF it_t_khs1 TO it_khs1.

    CLEAR it_ckikekokey.
  ENDLOOP.

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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM put_shop_mat_info.

  CLEAR it_mat.
  CLEAR it_fsc_mat.
  CLEAR it_kis1.
  CLEAR it_khs1.
  CLEAR it_ckikekokey.

* Clear
  CLEAR : it_tmp_shopcost, it_tmp_shopcost[].

* Sort (Material - ALL)
  SORT it_mat
    BY  matnr
        werks
        bwkey
        bwtar.

  LOOP AT it_kis1.
    CLEAR it_tmp_shopcost.
    MOVE-CORRESPONDING it_kis1 TO it_tmp_shopcost.
* Controlling Area
    it_tmp_shopcost-kokrs       = p_kokrs.
*   IT_TMP_SHOPCOST-BDATJ
*   IT_TMP_SHOPCOST-POPER
* Base Info.
    it_tmp_shopcost-klvar       = p_klvar.
    it_tmp_shopcost-versn       = p_versn.
    it_tmp_shopcost-record_type = gv_record_type.
* SAVE Pareants KALNR
    it_tmp_shopcost-par_kalnr   = it_tmp_shopcost-kalnr.
* Parents Material (FSC)
    CLEAR it_ckikekokey.
    READ TABLE it_ckikekokey
      WITH KEY bzobj = it_tmp_shopcost-bzobj
               kalnr = it_tmp_shopcost-kalnr
               kalka = it_tmp_shopcost-kalka
               kadky = it_tmp_shopcost-kadky
               tvers = it_tmp_shopcost-tvers
               bwvar = it_tmp_shopcost-bwvar
               kkzma = it_tmp_shopcost-kkzma.
    it_tmp_shopcost-fsc_matnr = it_ckikekokey-matnr .
* Material Type Of parents Material
    CLEAR it_t001w.
    READ TABLE  it_t001w WITH KEY werks = it_tmp_shopcost-werks.
    it_tmp_shopcost-bwkey = it_t001w-bwkey.
    CLEAR it_mat.
    READ TABLE  it_mat
      WITH KEY  matnr = it_tmp_shopcost-fsc_matnr
                werks = it_tmp_shopcost-werks
                bwkey = it_tmp_shopcost-bwkey
                bwtar = it_tmp_shopcost-bwtar
      BINARY SEARCH.
    it_tmp_shopcost-mtart = it_mat-mtart.
* Costing Dates/Plant/Valuation Level
    MOVE-CORRESPONDING it_ckikekokey TO it_tmp_shopcost.
* Child Material
    it_tmp_shopcost-llv_matnr  = it_kis1-matnr.
* Key Date (Parents)
    it_tmp_shopcost-par_kadky  = it_tmp_shopcost-kadky.
* Collect
    COLLECT  it_tmp_shopcost.
    CLEAR    it_tmp_shopcost.
    CLEAR it_kis1.
  ENDLOOP.

  CLEAR    it_tmp_shopcost.

* Sort
  SORT it_tmp_shopcost BY llv_matnr
                          werks
                          bwkey
                          bwtar.

  LOOP AT it_tmp_shopcost.
* Child Material Info
    CLEAR it_mat.
    READ TABLE  it_mat
      WITH KEY  matnr = it_tmp_shopcost-llv_matnr
                werks = it_tmp_shopcost-werks
                bwkey = it_tmp_shopcost-bwkey
                bwtar = it_tmp_shopcost-bwtar
      BINARY SEARCH.
    it_tmp_shopcost-beskz =  it_mat-beskz. "Procurement Type
    it_tmp_shopcost-sobsl =  it_mat-sobsl. "Special procurement type
    it_tmp_shopcost-vspvb =  it_mat-vspvb. "Proposed Supply Area
* Period (From / To)
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
    it_tmp_shopcost-bdatj    = it_tmp_shopcost-kadat(4).
    it_tmp_shopcost-from_per = it_tmp_shopcost-kadat+4(2).
    it_tmp_shopcost-bdatj    = it_tmp_shopcost-kadat(4).
    it_tmp_shopcost-to_per   =  p_perab.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = it_tmp_shopcost-kadat
         IMPORTING
              last_day_of_month = it_tmp_shopcost-bidat
         EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.

*    PERFORM set_from_to_period
*       USING it_tmp_shopcost-kadat
*             p_kokrs
*             it_tmp_shopcost-bdatj
*             it_tmp_shopcost-from_per.
*    PERFORM set_from_to_period
*       USING it_tmp_shopcost-bidat
*             p_kokrs
*             it_tmp_shopcost-bdatj
*             it_tmp_shopcost-to_per.
*-----End

* Modify
    MODIFY it_tmp_shopcost.
    CLEAR it_tmp_shopcost.
  ENDLOOP.

ENDFORM.                    " PUT_SHOP_MAT_INFO

*&---------------------------------------------------------------------*
*&      Form  SET_SHOP_BY_ITEM_CATE
*&---------------------------------------------------------------------*
*       Shop information by Item Category
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_shop_by_item_cate.

*  CLEAR IT_MAT.
*  CLEAR IT_FSC_MAT.
*  CLEAR IT_KIS1.
*  CLEAR IT_KHS1.
*  CLEAR IT_CKIKEKOKEY.
*  CLEAR IT_TMP_SHOPCOST.

* -  Progress Ind.
  PERFORM progress_ind USING '80'
                             text-240.

* Sort  By Item Category
  PERFORM sort_by_it_cate.

* Making Assingment of Cost element / Cost Components
  PERFORM assingment_ce_n_ccomp.

* Only 'M', 'E', and 'V' will be considered
* (Request By Functional Team Memeber 2004.03.19)

*   Category M  - Material Cost
  PERFORM for_category_m.

*   Category V  - Additive Cost
  PERFORM for_category_v.

*   Category E  - Manufacturing Cost
  PERFORM for_category_e.

ENDFORM.                    " SET_SHOP_BY_ITEM_CATE

*&---------------------------------------------------------------------*
*&      Form  SORT_BY_IT_CATE
*&---------------------------------------------------------------------*
*       Sort ITAB BY Item Category
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_by_it_cate.
  SORT it_tmp_shopcost BY typps kstar .
ENDFORM.                    " SORT_BY_IT_CATE

*&---------------------------------------------------------------------*
*&      Form  FOR_CATEGORY_M
*&---------------------------------------------------------------------*
*       Category 'M' - Material
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM for_category_m.

* Clear
  CLEAR : it_add_tmp_scost, it_add_tmp_scost[].
  CLEAR   it_tmp_shopcost.

  LOOP AT it_tmp_shopcost WHERE typps = c_cat_m.
*   C_KSTAR_M -> 540300
    IF it_tmp_shopcost-kstar = c_kstar_m.
*     Only For KSTAR C_KSTAR_M. -> New Sub Itab
      PERFORM for_cat_m_for_kstar.
    ELSE.
*     Except   KSTAR C_KSTAR_M. -> Modify
      PERFORM for_cat_m_but_kstar.
    ENDIF.
    CLEAR  it_tmp_shopcost.
  ENDLOOP.

  CLEAR  it_tmp_shopcost.
  CLEAR  it_add_tmp_scost.

* Sub Extract for KSTAR = 540300. -> IT_ADD_TMP_SCOST
* Read Summed Costs by Cost components
* Do not Change Origianl Costelement and other informations .
* Change only the values to Cost components and the Costs
* Cost element (Sub-extracted) : Cost component = 1 : 1
* The data for engine materials should be deriven using Plant "E001"
* -> CK13N, Press using Plant 'P001'
* (Itemization View, Cost Component View)
* All Costs should be mulitiplied by the qunatity which was used by
* the parents material.

*   C_KSTAR_M = 540300
  PERFORM cal_sub_ext_item.


*   C_KSTAR_M <> 540300
  PERFORM set_period_ne_c_kstar_m.


* Sort  By Item Category
  PERFORM sort_by_it_cate.


ENDFORM.                    " FOR_CATEGORY_M

*&---------------------------------------------------------------------*
*&      Form  FOR_CAT_M_BUT_KSTAR
*&---------------------------------------------------------------------*
*       Except   KSTAR C_KSTAR_M. - LP/KD
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM for_cat_m_but_kstar.
  CASE  it_tmp_shopcost-mtart.
    WHEN c_halb.  "HALB
* Read Shop Info. Form Routing. (Parents Material)
* Read SHOP Info. from Routing/Production version
      PERFORM read_shop_fr_routing_prv
                                 USING it_tmp_shopcost-fsc_matnr
                                       it_tmp_shopcost-shop
                                       it_tmp_shopcost-werks.
    WHEN OTHERS.
* Read Shop Info. Form PP Side.
      CLEAR it_zvco_rp1.
      READ TABLE it_zvco_rp1 WITH KEY usr00 = it_tmp_shopcost-vspvb.
      it_tmp_shopcost-shop = it_zvco_rp1-usr02.
  ENDCASE.
* Read Cost Comp. Number
  PERFORM read_cost_comp USING it_tmp_shopcost-kstar
                               it_tmp_shopcost-elemt.
* Variables
  it_tmp_shopcost-wdiff = it_tmp_shopcost-wertn - it_tmp_shopcost-wrtfx.
*   Modify
  MODIFY it_tmp_shopcost.
ENDFORM.                    " FOR_CAT_M_BUT_KSTAR

*&---------------------------------------------------------------------*
*&      Form  FOR_CAT_M_FOR_KSTAR
*&---------------------------------------------------------------------*
*       Only For KSTAR C_KSTAR_M. - Engine/Press (MIP)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM for_cat_m_for_kstar.

* Local Data Definition
  DATA : lv_shop  LIKE  it_tmp_shopcost-shop.
  DATA : lv_werks LIKE  t001w-werks.
  CLEAR lv_shop.
* Check Procurement Type / Spc Proc. Type
* -> Engine In Plant 'P001' -> 'E001'
  IF   it_tmp_shopcost-beskz = 'F'
   AND it_tmp_shopcost-sobsl = '40'.
*    LV_SHOP  = C_SHP_M_E001.
    lv_werks = 'E001'.
  ELSE.
*    LV_SHOP  = C_SHP_M_P001.
    lv_werks = it_tmp_shopcost-werks.
  ENDIF.

* Read SHOP Info. from Routing/Production version
  PERFORM read_shop_fr_routing_prv
                             USING it_tmp_shopcost-llv_matnr
                                   lv_shop
                                   lv_werks.

* Check Master Error
*  CHECK LV_SHOP NE SPACE .

* Store DATA which should be exploded later .
  MOVE-CORRESPONDING it_tmp_shopcost TO it_add_tmp_scost.
* Change Plan/Shop
  it_add_tmp_scost-shop  = lv_shop.
  it_add_tmp_scost-werks = lv_werks.
* Bwkey
  CLEAR it_t001w.
  READ TABLE it_t001w WITH KEY werks = it_add_tmp_scost-werks .
  it_add_tmp_scost-bwkey = it_t001w-bwkey.
* Leave BWTAR
  APPEND it_add_tmp_scost.
  CLEAR it_add_tmp_scost.

* Delete Original Data
  DELETE  it_tmp_shopcost.

ENDFORM.                    " FOR_CAT_M_FOR_KSTAR

*&---------------------------------------------------------------------*
*&      Form  ASSINGMENT_CE_N_CCOMP
*&---------------------------------------------------------------------*
*       Making Assingment of Cost element / Cost Components
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assingment_ce_n_ccomp.
* Read Cost Components
  CLEAR : it_cskb, it_cskb[].
  LOOP AT it_tmp_shopcost.
    ON CHANGE OF it_tmp_shopcost-kstar.
      it_cskb-kstar = it_tmp_shopcost-kstar.
      CALL FUNCTION 'KKEK_COST_COMPONENT_ELEMENT'
        EXPORTING
          elehk_imp                 = p_elehk
          ktopl_imp                 = tka01-ktopl
          kstar_imp                 = it_cskb-kstar
*         HRKFT_IMP                 =
*         ELEMT_IMP                 =
*         ELEHKNS_IMP               =
*         SCHKZ_IMP                 =
*         SCHKZNS_IMP               =
          message_on_screen         = space
        IMPORTING
*         KSTAR_EXP                 =
*         HRKFT_EXP                 =
          elemt_exp                 = it_cskb-elemt
*         ELEMTNS_EXP               =
*         FLG_FIX_ALLOWED_EXP       =
*         RC                        =
        EXCEPTIONS
          calling_error             = 1
          OTHERS                    = 2.
      IF sy-subrc = 0.
        COLLECT it_cskb.
      ENDIF.
    ENDON.
  ENDLOOP.

  CLEAR it_cskb.

ENDFORM.                    " ASSINGMENT_CE_N_CCOMP

*&---------------------------------------------------------------------*
*&      Form  READ_COST_COMP
*&---------------------------------------------------------------------*
*       Read Cost Comp. Number
*----------------------------------------------------------------------*
*      -->P_KSTAR  Cost Element
*      -->P_ELEMT  Cost Components
*----------------------------------------------------------------------*
FORM read_cost_comp USING    p_kstar
                             p_elemt.
  CLEAR it_cskb.
  READ TABLE it_cskb WITH KEY kstar = p_kstar.
  p_elemt = it_cskb-elemt.

ENDFORM.                    " READ_COST_COMP

*&---------------------------------------------------------------------*
*&      Form  CAL_SUB_EXT_ITEM
*&---------------------------------------------------------------------*
*       Sub Extraction for KSTAR = 540300
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_sub_ext_item.

  CLEAR  it_tmp_shopcost.
  CLEAR  it_add_tmp_scost.

* Local Definition
  DATA : it_l_ck_key  LIKE STANDARD TABLE OF it_ckikekokey
                      WITH HEADER LINE .

  SORT it_add_tmp_scost BY  llv_matnr werks.

* Clear
  CLEAR : it_tmp_pe, it_tmp_pe[].

  LOOP AT it_add_tmp_scost.
    MOVE-CORRESPONDING it_add_tmp_scost TO it_tmp_pe.
    COLLECT it_tmp_pe.
    CLEAR   it_tmp_pe.
  ENDLOOP.

* Clear
  CLEAR : it_tmp_pren , it_tmp_pren[].
  LOOP AT it_tmp_pe.
* Clear
    CLEAR : it_l_ck_key, it_l_ck_key[].
* Read KEKO Key Part
    PERFORM  get_engine_press_key_part
      TABLES it_l_ck_key.

* Check Valid Data
    IF it_l_ck_key[] IS INITIAL .
*    MESSAGE e026.
*This is error... master error, etc.. use CE for CCS.
*TEMP... FIXIT LATER
      PERFORM determine_ccs_fr_ce.
    ELSE.
* Read Cost Components
      PERFORM  read_cost_comp_sub
        TABLES it_l_ck_key.
    ENDIF.

    CLEAR it_tmp_pe.
  ENDLOOP.

* Reorginzing data - Child . period
  PERFORM re_sub_data_eng_pres.

* Attach Sub Exploded data to the parents records.
  PERFORM att_subdata_for_m_to_par.

ENDFORM.                    " CAL_SUB_EXT_ITEM

*&---------------------------------------------------------------------*
*&      Form  GET_ENGINE_PRESS_KEY_PART
*&---------------------------------------------------------------------*
*       Read KEKO Key Part (Engine, Press)
*----------------------------------------------------------------------*
*      -->IT_PL_CK_KEY  ITAB For Key Part
*----------------------------------------------------------------------*
FORM get_engine_press_key_part
 TABLES   it_pl_ck_key STRUCTURE it_ckikekokey.

  CLEAR : it_pl_ck_key, it_pl_ck_key[].

  DATA : lv_pecnt(3) TYPE n.
  DATA : lv_ld       LIKE sy-datum.
  DATA : lv_ed       LIKE sy-datum.

* Set First period
  lv_pecnt = p_perab.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*  WHILE lv_pecnt =< p_perbi.
  WHILE lv_pecnt =< p_perab.
*-----End
* Get Period dates
    PERFORM read_begin_ending_date
      USING lv_pecnt
            lv_ld
            lv_ed.

* IT_ADD_TMP_SCOST
* Read KEKO with a Date Range
    SELECT * FROM keko
             APPENDING CORRESPONDING FIELDS OF TABLE  it_pl_ck_key
             WHERE tvers = p_tvers
               AND bwvar = tck03-bwvar
               AND kalka = tck03-kalka
               AND kkzma = space         "ALL Data
               AND matnr = it_tmp_pe-llv_matnr
               AND werks = it_tmp_pe-werks
               AND bwkey = it_tmp_pe-bwkey
               AND bwtar = it_tmp_pe-bwtar
               AND kokrs = p_kokrs
               AND ( kadat =< lv_ld AND bidat => lv_ed )
               AND freig = gv_freig.
* FREIG = 'X' Only Mark Released - Standard Plan
* FREIG = ' ' No   Mark Released - Business Plan

* Period Counter
    lv_pecnt = lv_pecnt + 1.

  ENDWHILE.
*
  CLEAR it_pl_ck_key.

* Make Unique Key
  SORT it_pl_ck_key
    BY
      bzobj
      kalnr
      kalka
      kadky
      tvers
      bwvar
      kkzma.
  DELETE ADJACENT DUPLICATES FROM it_pl_ck_key.


ENDFORM.                    " GET_ENGINE_PRESS_KEY_PART

*&---------------------------------------------------------------------*
*&      Form  READ_COST_COMP_SUB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_L_CK_KEY  text
*----------------------------------------------------------------------*
FORM read_cost_comp_sub
                 TABLES   p_it_l_ck_key STRUCTURE it_ckikekokey.

* Only COGM data should be considered
* -> view : '01'

*select * from tckh8 up to 1 rows.
*   move tckh8-sicht to i_sicht.
*endselect.

  DATA : it_l_keph    LIKE STANDARD TABLE OF keph
                      WITH HEADER LINE .
  DATA : it_l_kkb_split
                      LIKE STANDARD TABLE OF  kkb_split
                      WITH HEADER LINE .
  DATA : wa_l_ckikekokey LIKE ckikekokey .
  DATA : begin_date LIKE sy-datum.
  CLEAR begin_date.
  LOOP AT p_it_l_ck_key.
* Move Keys
    MOVE-CORRESPONDING p_it_l_ck_key TO wa_l_ckikekokey  .
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
*W000       -> Total
*W001       -> Fixed
*W002       -> Variable
    LOOP AT it_l_kkb_split.
      MOVE-CORRESPONDING  it_tmp_pe      TO   it_tmp_pren.
      MOVE-CORRESPONDING  p_it_l_ck_key  TO   it_tmp_pren.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
      begin_date(4) = p_bdatj.
      begin_date+4(2) = p_perab.
      begin_date+6(2) = '01'.
*-----End

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
*-----Start
*                day_in            = it_tmp_pren-kadat
                  day_in           = begin_date
*-----End
           IMPORTING
                last_day_of_month = it_tmp_pren-bidat
           EXCEPTIONS
                day_in_no_date    = 1
                OTHERS            = 2.

      it_tmp_pren-elemt = it_l_kkb_split-elemt.
      it_tmp_pren-wertn = it_l_kkb_split-w000.
      it_tmp_pren-wrtfx = it_l_kkb_split-w001.
      it_tmp_pren-wdiff = it_l_kkb_split-w002.
      APPEND  it_tmp_pren.
      CLEAR    it_tmp_pren.
    ENDLOOP.
  ENDLOOP.

  CLEAR   it_tmp_pren.

ENDFORM.                    " READ_COST_COMP_SUB

*&---------------------------------------------------------------------*
*&      Form  RE_SUB_DATA_ENG_PRES
*&---------------------------------------------------------------------*
*       Period (From To) MIP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_sub_data_eng_pres.

  SORT it_tmp_pren BY llv_matnr kadky.

* Dis. Period
  LOOP AT it_tmp_pren.
* period (From)
    CALL FUNCTION 'K_DATE_TO_PERIOD_CONVERT'
         EXPORTING
              i_date             = it_tmp_pren-kadat
              i_kokrs            = p_kokrs
         IMPORTING
              e_gjahr            = it_tmp_pren-bdatj
              e_perio            = it_tmp_pren-from_per
         EXCEPTIONS
              no_period_determin = 1
              t009b_notfound     = 2
              t009_notfound      = 3
              OTHERS             = 4.
    IF sy-subrc <> 0.
    ENDIF.

* period (To)
    CALL FUNCTION 'K_DATE_TO_PERIOD_CONVERT'
         EXPORTING
              i_date             = it_tmp_pren-bidat
              i_kokrs            = p_kokrs
         IMPORTING
              e_gjahr            = it_tmp_pren-bdatj
              e_perio            = it_tmp_pren-to_per
         EXCEPTIONS
              no_period_determin = 1
              t009b_notfound     = 2
              t009_notfound      = 3
              OTHERS             = 4.
    IF sy-subrc <> 0.
    ENDIF.

* Modify
    MODIFY it_tmp_pren.
    CLEAR it_tmp_pren.
  ENDLOOP.

  SORT it_tmp_pren BY bdatj poper llv_matnr kadky.

* Multiply data( X the number of period )
  DATA : it_dup_tmp_pren LIKE STANDARD TABLE OF it_tmp_pren
                         WITH HEADER LINE .
  DATA : lv_per_cnt LIKE it_dup_tmp_pren-from_per.

  it_dup_tmp_pren[] = it_tmp_pren[].

  CLEAR : it_tmp_pren, it_tmp_pren[].

  LOOP AT it_dup_tmp_pren.
    CLEAR lv_per_cnt.
    lv_per_cnt = it_dup_tmp_pren-from_per.
    WHILE lv_per_cnt <= it_dup_tmp_pren-to_per  .
      MOVE-CORRESPONDING it_dup_tmp_pren TO  it_tmp_pren.
      it_tmp_pren-poper = lv_per_cnt.
* Append
      APPEND it_tmp_pren.
      CLEAR  it_tmp_pren.
* Period Counter
      lv_per_cnt = lv_per_cnt + 1.
    ENDWHILE.
  ENDLOOP.

* Check Period Range
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*    screen_period_d  it_tmp_pren.
*-----End
  SORT it_tmp_pren BY bdatj poper llv_matnr .

ENDFORM.  "  RE_SUB_DATA_ENG_PRES

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
FORM att_subdata_for_m_to_par.

  CLEAR   it_tmp_pe.
  CLEAR   it_tmp_pren.
  CLEAR   it_add_tmp_scost.

  DATA :  it_l_sub_mip LIKE STANDARD TABLE OF it_tmp_shopcost
                       WITH HEADER LINE .
  CLEAR : it_l_sub_mip, it_l_sub_mip[].

* Multiply with Period key
* Multiply data( X the number of period )
  PERFORM mult_x_temp_itab_by_period.

* Making IT_L_SUB_MIP (With the Consideration of period)
  LOOP AT it_add_tmp_scost.
    LOOP AT it_tmp_pren
                   WHERE llv_matnr = it_add_tmp_scost-llv_matnr
                     AND bdatj = it_add_tmp_scost-bdatj
                     AND poper = it_add_tmp_scost-poper.
* Copy   From IT_ADD_TMP_SCOST   to IT_L_SUB_MIP
      MOVE-CORRESPONDING it_add_tmp_scost TO it_l_sub_mip.
* Copy   From IT_TMP_PREN        to IT_L_SUB_MIP
      MOVE-CORRESPONDING  it_tmp_pren-sub   TO it_l_sub_mip-sub.
* Comp.
      it_l_sub_mip-par_kalnr = it_add_tmp_scost-par_kalnr.
      it_l_sub_mip-par_kadky = it_add_tmp_scost-par_kadky.
      it_l_sub_mip-kalnr = it_tmp_pren-kalnr.
      it_l_sub_mip-elemt = it_tmp_pren-elemt.
* Costs
      it_l_sub_mip-wertn = it_tmp_pren-wertn * it_l_sub_mip-menge.
      it_l_sub_mip-wrtfx = it_tmp_pren-wrtfx * it_l_sub_mip-menge.
      it_l_sub_mip-wdiff = it_tmp_pren-wdiff * it_l_sub_mip-menge.
* Append
      APPEND it_l_sub_mip.
      CLEAR  it_l_sub_mip.

      CLEAR it_tmp_pren.
    ENDLOOP.
    CLEAR it_add_tmp_scost.
  ENDLOOP.

  CLEAR  it_l_sub_mip.


* Mtart
  LOOP AT  it_l_sub_mip WHERE mtart EQ space.
    SELECT SINGLE mtart FROM mara
                   INTO it_l_sub_mip-mtart
                  WHERE matnr = it_l_sub_mip-fsc_matnr.
    MODIFY it_l_sub_mip.
    CLEAR  it_l_sub_mip.
  ENDLOOP.

* Append
  APPEND LINES OF it_l_sub_mip TO it_tmp_shopcost.
  CLEAR it_tmp_shopcost.

ENDFORM.                    " ATT_SUBDATA_FOR_M_TO_PAR

*&---------------------------------------------------------------------*
*&      Form  FOR_CATEGORY_V
*&---------------------------------------------------------------------*
*       Category V  - Material Cost
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM for_category_v.
* Clear
  CLEAR : it_add_tmp_scost, it_add_tmp_scost[].
  CLEAR   it_tmp_shopcost.

  LOOP AT it_tmp_shopcost WHERE typps = c_cat_v.
    CLEAR it_cskb.
    READ TABLE it_cskb WITH KEY kstar = it_tmp_shopcost-kstar.
* SET SHOP = 'MXTX'
*-----Start wskim 02/03/2005
    IF  it_tmp_shopcost-mtart EQ 'FERT'.
      it_tmp_shopcost-shop  = 'MXTX'.
    ELSE.
* Read SHOP Info. from Routing/Production version
      PERFORM read_shop_fr_routing_prv
                                 USING it_tmp_shopcost-fsc_matnr
                                       it_tmp_shopcost-shop
                                       it_tmp_shopcost-werks.

    ENDIF.
*-----End
    it_tmp_shopcost-elemt = it_cskb-elemt.
*    MODIFY IT_TMP_SHOPCOST.
** Period (From / To)
*    PERFORM SET_FROM_TO_PERIOD
*       USING IT_TMP_SHOPCOST-KADAT
*             P_KOKRS
*             IT_TMP_SHOPCOST-BDATJ
*             IT_TMP_SHOPCOST-FROM_PER.
*    PERFORM SET_FROM_TO_PERIOD
*       USING IT_TMP_SHOPCOST-BIDAT
*             P_KOKRS
*             IT_TMP_SHOPCOST-BDATJ
*             IT_TMP_SHOPCOST-TO_PER.
* Variables
    it_tmp_shopcost-wdiff
                   = it_tmp_shopcost-wertn - it_tmp_shopcost-wrtfx.
* Making Temporary itab.
    MOVE-CORRESPONDING it_tmp_shopcost TO it_add_tmp_scost .
    APPEND it_add_tmp_scost .
    CLEAR  it_add_tmp_scost .
* Delete Original data
    DELETE it_tmp_shopcost.
    CLEAR  it_tmp_shopcost.
  ENDLOOP.

  CLEAR  it_tmp_shopcost.

* Multiply with Period key
* Multiply data( X the number of period )
  PERFORM mult_x_temp_itab_by_period.

* Append to Main Itab
  APPEND LINES OF it_add_tmp_scost TO it_tmp_shopcost.
  CLEAR it_tmp_shopcost.

ENDFORM.                    " FOR_CATEGORY_V

*&---------------------------------------------------------------------*
*&      Form  MULT_X_TEMP_ITAB_BY_PERIOD
*&---------------------------------------------------------------------*
*       Multiply with Period key (Temp. Itab)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mult_x_temp_itab_by_period.
* Multiply with Period key
* Multiply data( X the number of period )
  DATA : it_dup_tmp_scost LIKE STANDARD TABLE OF it_add_tmp_scost
                          WITH HEADER LINE .
  DATA : lv_per_cnt LIKE it_dup_tmp_scost-from_per.
  it_dup_tmp_scost[] = it_add_tmp_scost[].

  CLEAR : it_add_tmp_scost, it_add_tmp_scost[].

  LOOP AT it_dup_tmp_scost.
    CLEAR lv_per_cnt.
    lv_per_cnt = it_dup_tmp_scost-from_per.

    WHILE lv_per_cnt <= it_dup_tmp_scost-to_per  .
      MOVE-CORRESPONDING it_dup_tmp_scost TO  it_add_tmp_scost.
      it_add_tmp_scost-poper = lv_per_cnt.
* Append
      APPEND it_add_tmp_scost.
      CLEAR  it_add_tmp_scost.
* Period Counter
      lv_per_cnt = lv_per_cnt + 1.
    ENDWHILE.
  ENDLOOP.

* Check Period Range
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*   screen_period_d  it_add_tmp_scost.
*-----End
  SORT it_add_tmp_scost BY bdatj poper llv_matnr .

ENDFORM.                    " MULT_X_TEMP_ITAB_BY_PERIOD

*&---------------------------------------------------------------------*
*&      Form  FOR_CATEGORY_E
*&---------------------------------------------------------------------*
*       Category E  - Material Cost
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM for_category_e.

* Clear
  CLEAR : it_add_tmp_scost, it_add_tmp_scost[].
  CLEAR   it_tmp_shopcost.
  CLEAR it_cctr.

  LOOP AT it_tmp_shopcost WHERE typps = c_cat_e.
* SET SHOP
    CLEAR it_cctr.
    READ TABLE it_cctr WITH KEY kostl = it_tmp_shopcost-kostl.
    it_tmp_shopcost-shop  = it_cctr-shop.
* Making Temporary itab.
    MOVE-CORRESPONDING it_tmp_shopcost TO it_add_tmp_scost .
    APPEND it_add_tmp_scost .
    CLEAR  it_add_tmp_scost .
* Delete Original data
    DELETE it_tmp_shopcost.
*    MODIFY IT_TMP_SHOPCOST.
    CLEAR  it_tmp_shopcost.
  ENDLOOP.

  CLEAR  it_tmp_shopcost.

* Multiply with Period key
* Multiply data( X the number of period )
  PERFORM mult_x_temp_itab_by_period.

* Calculation %/Cost Comp. for Category 'E'
  PERFORM mult_x_by_cost_comp_e.

* Append to Main Itab
  APPEND LINES OF it_add_tmp_scost TO it_tmp_shopcost.
  CLEAR it_tmp_shopcost.

ENDFORM.                    " FOR_CATEGORY_E

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_IN_SHOP
*&---------------------------------------------------------------------*
*       Read CCtrs linked to SHOP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr_in_shop.

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

* Read CCtrs
  CLEAR : it_costcenterlist, it_costcenterlist[].
  CLEAR : it_return, it_return[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
       EXPORTING
            controllingarea = p_kokrs
            date_from       = lv_datum
            costcentergroup = c_gp_kostl_e
       TABLES
            costcenterlist  = it_costcenterlist
            return          = it_return.

  IF it_costcenterlist[] IS INITIAL.
    MESSAGE e080 WITH c_gp_kostl_e p_kokrs lv_datum.
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

* Read DATA From COOMCO
  PERFORM read_data_from_coomco.

* Read Coponent Values - KSBT
  PERFORM read_comp_value_ksbt.

* Multiply with Period key
* Multiply data( X the number of period )
  PERFORM multi_record_with_period_ksbt.

* Cal. Rate
  PERFORM cal_percent_ksbt.

ENDFORM.                    " CAL_PERCENT_USING_KSBT

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_FR_ROUTING_PRV
*&---------------------------------------------------------------------*
*       Read SHOP Info. from Routing/Production version
*----------------------------------------------------------------------*
*      -->P_MATNR  Material
*      -->P_SHOP   Shop
*      -->P_WERKS  Plant
*----------------------------------------------------------------------*
FORM read_shop_fr_routing_prv USING    p_matnr
                                       p_shop
                                       p_werks.

  CLEAR : crhd, plpo, plko.
* Read Shop From Routing
  DATA : lv_arbid LIKE plpo-arbid.

*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
  DATA: BEGIN OF it_routing OCCURS 0,
          matnr    TYPE mara-matnr,
          ro_plnnr TYPE plko-plnnr,
          ro_plnal TYPE plko-plnal,
          ro_zaehl TYPE plko-zaehl,
          ro_verwe TYPE plko-verwe,
          ro_vornr TYPE plpo-vornr,
          ro_arbpl TYPE crhd-arbpl,
        END OF it_routing.
  DATA : from_date(8).
  REFRESH it_routing.CLEAR from_date.

  IF p_bpl EQ 'X'.
    CLEAR : mara,mkal.

*    SELECT SINGLE mtart AS mara  plnty AS mkal
*           INTO (mara-mtart,mkal-plnty)
*         FROM mara AS mara INNER JOIN mkal AS mkal
*           ON mara~matnr = mkal~matnr
*          WHERE mara~matnr EQ p_matnr
*            AND mkal~werks EQ p_werks
*            AND mkal~bdatu >= it_tmp_shopcost-bidat
*            AND mkal~adatu <= it_tmp_shopcost-kadat.
    SELECT SINGLE * FROM mara
          WHERE matnr EQ p_matnr.

    CHECK sy-subrc = 0.

    CONCATENATE p_bdatj p_perab+1(2) '01' INTO from_date.
    SELECT ma~matnr pk~plnnr pk~plnal pk~zaehl pk~verwe
           pp~vornr ch~arbpl
        INTO TABLE it_routing
          FROM  ( ( ( ( ( ( mara AS ma
               INNER JOIN marc AS mr ON ma~matnr = mr~matnr )
               INNER JOIN mapl AS mp ON mr~matnr = mp~matnr )
               INNER JOIN plko AS pk ON mp~plnnr = pk~plnnr AND
                                        mp~plnal = pk~plnal )
               INNER JOIN plas AS pa ON mp~plnty = pa~plnty AND
                                        mp~plnnr = pa~plnnr AND
                                        mp~plnal = pa~plnal )
               INNER JOIN plpo AS pp ON pk~plnnr = pp~plnnr AND
                                        pa~plnty = pp~plnty AND
                                        pa~plnnr = pp~plnnr AND
                                        pa~plnkn = pp~plnkn AND
                                        pa~zaehl = pp~zaehl )
               INNER JOIN crhd AS ch ON pp~arbid = ch~objid )
            WHERE
*                  mp~plnty = mkal-plnty   AND
                  mp~loekz = ' ' AND
                  pa~loekz = ' ' AND
                  ma~matnr EQ p_matnr     AND
                  ma~mtart EQ mara-mtart  AND
                  mr~werks EQ p_werks     AND
                  pk~verwe EQ gv_verwe    AND
                  pk~loekz NE 'X' AND
                  pk~datuv <= from_date AND
                  ch~objty = 'A' .

    SORT it_routing BY matnr ro_plnnr ro_zaehl DESCENDING.

    DELETE ADJACENT DUPLICATES FROM it_routing
           COMPARING matnr ro_plnnr ro_zaehl .
    READ TABLE it_routing INDEX 1.
* Work Center = Cost center (1:1)
    CLEAR it_cctr.
    READ TABLE it_cctr WITH KEY kostl = it_routing-ro_arbpl.
    IF sy-subrc = 0.
      p_shop = it_cctr-shop.
    ENDIF.
  ELSE.
    CLEAR mkal.
    SELECT SINGLE *  FROM mkal
                    WHERE matnr = p_matnr
                      AND werks = p_werks
                      AND bdatu >= it_tmp_shopcost-bidat
                      AND adatu <= it_tmp_shopcost-kadat.

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
      p_shop = it_cctr-shop.
    ENDIF.
*-----Start
  ENDIF.
*-----End
ENDFORM.                    " READ_SHOP_FR_ROUTING_PRV

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FROM_COOMCO
*&---------------------------------------------------------------------*
*       Read DATA From COOMCO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_from_coomco.


**// Begin of MOD. By Hyung Jin Youn 2004.07.06
* KSBT dose not have data by period in case of (BP/Qarter) planning
* Select data with date range

* Local Data Definition
  DATA : lv_percount LIKE cosp-perbl. "Period Counter
  DATA : lv_cnt  LIKE  cosp-perbl.
  DATA : lv_kadky LIKE coomco-kadky.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
** Cal. the Counter
*  lv_percount = p_perbi - p_perab + 1.
  lv_percount = 1.
*-----End

* Period Counter : Set From-Period .
  CLEAR lv_cnt.
  lv_cnt = p_perab .

* Clear
  CLEAR : it_coomco, it_coomco[].

* From period - To period
  DO lv_percount TIMES.

* KEPH -> 'C' COKL/KEKO/KEPH
* K_KKB_SPLITTING_CONVERT

* First Date of From Period
    CLEAR lv_kadky.
    CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_gjahr              = p_bdatj
*       I_MONMIT             = 00
        i_periv              = tka01-lmona
        i_poper              = lv_cnt
      IMPORTING
        e_date               = lv_kadky
      EXCEPTIONS
        input_false          = 1
        t009_notfound        = 2
        t009b_notfound       = 3
        OTHERS               = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Comment : Functional Team could NOT find the reason
* why the Costing type and Costing Variant for specific
* version are not matched to the values on IMG
*  Cost Components for Cost of Goods Mfd is now created
*  only on PPC1/01 .
*   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COOMCO
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_coomco
            FROM coomco
           WHERE gjahr = p_bdatj
             AND versn = p_versn
*            AND KLVAR = P_KLVAR
             AND tarkz = gv_tarkz
             AND tvers = p_tvers
*            AND KALKA = TCK03-KALKA
*            AND BWVAR = TCK03-BWVAR
*            AND LEDNR = '00'
             AND (     kadky =< lv_kadky
                   AND bidat => lv_kadky )
             AND kkzma = space. "Additive - > X

    CLEAR : it_coomco.

* Period Counter
    lv_cnt = lv_cnt + 1.
  ENDDO.

  SORT it_coomco BY
                    lednr
                    objnr
                    gjahr
                    versn
                    bzobj
                    tvers
                    kadky
                    kalnr
                    kalka
                    kkzma
                    dipa
                    bwvar
                    keart
                    kkzmm
                    kkzst
                    losfx
                    patnr.

  DELETE ADJACENT DUPLICATES FROM it_coomco.

  CLEAR : it_coomco.

  IF it_coomco[] IS INITIAL .
    MESSAGE e081.
  ENDIF.


** Obsoleted
*
** Local Data Definition
*  DATA : LV_KADKY LIKE COOMCO-KADKY.
*
** KEPH -> 'C' COKL/KEKO/KEPH
** K_KKB_SPLITTING_CONVERT
*
** First Date of From Period
*  CLEAR LV_KADKY.
*  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
*    EXPORTING
*      I_GJAHR              = P_BDATJ
**     I_MONMIT             = 00
*      I_PERIV              = TKA01-LMONA
*      I_POPER              = P_PERAB
*    IMPORTING
*      E_DATE               = LV_KADKY
*    EXCEPTIONS
*      INPUT_FALSE          = 1
*      T009_NOTFOUND        = 2
*      T009B_NOTFOUND       = 3
*      OTHERS               = 4.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
** Comment : Functional Team could NOT find the reason
** why the Costing type and Costing Variant for specific
** version are not matched to the values on IMG
**  Cost Components for Cost of Goods Mfd is now created
**  only on PPC1/01 .
*  CLEAR : IT_COOMCO, IT_COOMCO[].
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COOMCO
*           FROM COOMCO
*          WHERE GJAHR = P_BDATJ
*            AND VERSN = P_VERSN
**           AND KLVAR = P_KLVAR
*            AND TARKZ = GV_TARKZ
*            AND TVERS = P_TVERS
**           AND KALKA = TCK03-KALKA
**           AND BWVAR = TCK03-BWVAR
**           AND LEDNR = '00'
*            AND KADKY => LV_KADKY
*            AND KKZMA = SPACE. "Additive - > X
*
*  CLEAR : IT_COOMCO.
*
*  IF IT_COOMCO[] IS INITIAL .
*    MESSAGE E081.
*  ENDIF.
*

**// End of Mod.

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
*&      Form  MULTI_RECORD_WITH_PERIOD_KSBT
*&---------------------------------------------------------------------*
*       Multiply with Period key KSBT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM multi_record_with_period_ksbt.

  CLEAR it_kostl_lstar_pct.


  DATA : it_l_tmp_coomco  LIKE STANDARD TABLE OF it_kostl_lstar_pct
                          WITH HEADER LINE .
  DATA : lv_per_cnt       LIKE it_kostl_lstar_pct-poper.

  it_l_tmp_coomco[] = it_kostl_lstar_pct[].

  CLEAR : it_kostl_lstar_pct, it_kostl_lstar_pct[].

  LOOP AT it_l_tmp_coomco.
    CLEAR lv_per_cnt.
    lv_per_cnt = it_l_tmp_coomco-from_per.
    WHILE lv_per_cnt <= it_l_tmp_coomco-to_per  .
      MOVE-CORRESPONDING it_l_tmp_coomco TO  it_kostl_lstar_pct.
      it_kostl_lstar_pct-poper = lv_per_cnt.
* Append
      APPEND it_kostl_lstar_pct.
      CLEAR  it_kostl_lstar_pct.
* Period Counter
      lv_per_cnt = lv_per_cnt + 1.
    ENDWHILE.
  ENDLOOP.

* Check Period Range
*  SCREEN_PERIOD_D  IT_ADD_TMP_SCOST.

  SORT it_kostl_lstar_pct BY gjahr poper .

  IF it_kostl_lstar_pct[] IS INITIAL .
    MESSAGE e081.
  ENDIF.

ENDFORM.                    " MULTI_RECORD_WITH_PERIOD_KSBT

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

  SORT it_koat_p BY gjahr poper kostl lstar elemt.

ENDFORM.                    " CAL_PERCENT_KSBT

*&---------------------------------------------------------------------*
*&      Form  MULT_X_BY_COST_COMP_E
*&---------------------------------------------------------------------*
*       Calculation %/Cost Comp. for Category 'E'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mult_x_by_cost_comp_e.
* Local Data Definition
  DATA : it_dup_tmp_scost LIKE STANDARD TABLE OF it_add_tmp_scost
                          WITH HEADER LINE .
  DATA : lv_per_cnt LIKE it_dup_tmp_scost-from_per.

  it_dup_tmp_scost[] = it_add_tmp_scost[].

  CLEAR : it_add_tmp_scost, it_add_tmp_scost[].

* Clear
  CLEAR  it_koat_p .
  LOOP AT it_dup_tmp_scost.
    LOOP AT  it_koat_p WHERE gjahr =  it_dup_tmp_scost-bdatj
                         AND poper =  it_dup_tmp_scost-poper
                         AND kostl =  it_dup_tmp_scost-kostl
                         AND lstar =  it_dup_tmp_scost-lstar.
* Transfer Data
      MOVE-CORRESPONDING  it_dup_tmp_scost TO it_add_tmp_scost .
* Cost Comp.
      it_add_tmp_scost-elemt = it_koat_p-elemt.
* Values
      it_add_tmp_scost-wertn = it_dup_tmp_scost-wertn * it_koat_p-cp_%.
      it_add_tmp_scost-wrtfx = it_dup_tmp_scost-wrtfx * it_koat_p-cp_%.
      it_add_tmp_scost-wdiff
         = it_add_tmp_scost-wertn - it_add_tmp_scost-wrtfx .
* Collect
      COLLECT  it_add_tmp_scost.
      CLEAR    it_add_tmp_scost.
      CLEAR it_koat_p.
    ENDLOOP.
    CLEAR it_dup_tmp_scost.
  ENDLOOP.

  CLEAR   it_add_tmp_scost.

ENDFORM.                    " MULT_X_BY_COST_COMP_V

*&---------------------------------------------------------------------*
*&      Form  TRE_FRACTION_TO_V
*&---------------------------------------------------------------------*
*       Fractions to 'V' w/o SHOP, CE, CCTR
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tre_fraction_to_v.

  CLEAR it_khs1.
  CLEAR it_tmp_shopcost.

  DATA : BEGIN OF it_tmp_fraction OCCURS 0.
          INCLUDE STRUCTURE khsk.
  DATA :  par_kalnr LIKE  khsk-kalnr,
          par_kadky LIKE  khsk-kadky,
          mtart     LIKE  mara-mtart.
  DATA :  wertn LIKE it_tmp_shopcost-wertn,
          wrtfx LIKE it_tmp_shopcost-wrtfx,
          wdiff LIKE it_tmp_shopcost-wdiff.
  DATA :  kokrs         LIKE it_tmp_shopcost-kokrs,
          bdatj         LIKE it_tmp_shopcost-bdatj,
          poper         LIKE it_tmp_shopcost-poper,
          klvar         LIKE it_tmp_shopcost-klvar,
          versn         LIKE it_tmp_shopcost-versn,
          record_type   LIKE it_tmp_shopcost-record_type,
          fsc_matnr     LIKE it_tmp_shopcost-fsc_matnr.
  DATA : END OF   it_tmp_fraction.

  DATA : it_frac_addon LIKE STANDARD TABLE OF it_tmp_shopcost
                       WITH HEADER LINE .

*Summation By Material
  LOOP AT it_tmp_shopcost.
    CLEAR it_tmp_fraction.
    MOVE-CORRESPONDING it_tmp_shopcost TO it_tmp_fraction.
    CLEAR   it_tmp_fraction-kalnr.
    CLEAR   it_tmp_fraction-kadky.
* Collect
    COLLECT it_tmp_fraction.
    CLEAR   it_tmp_fraction.
  ENDLOOP.

* Clear
  CLEAR : it_frac_addon, it_frac_addon[].
  LOOP AT it_tmp_fraction.
    LOOP AT it_khs1
                    WHERE
                          bzobj = it_tmp_fraction-bzobj
                      AND kalnr = it_tmp_fraction-par_kalnr
                      AND kalka = it_tmp_fraction-kalka
                      AND kadky = it_tmp_fraction-par_kadky
                      AND tvers = it_tmp_fraction-tvers
                      AND bwvar = it_tmp_fraction-bwvar
                      AND kkzma = it_tmp_fraction-kkzma.
      IF    it_khs1-hwges <> it_tmp_fraction-wertn
         OR it_khs1-hwgfx <> it_tmp_fraction-wrtfx.
        MOVE-CORRESPONDING it_tmp_fraction  TO it_frac_addon.
* Fraction to Cate. 'V'
        it_frac_addon-kalnr = it_tmp_fraction-par_kalnr.
        it_frac_addon-kadky = it_tmp_fraction-par_kadky.
        it_frac_addon-typps = c_cat_v.
* MTART
        it_frac_addon-mtart = it_tmp_fraction-mtart.
* Value
        it_frac_addon-wertn = it_khs1-hwges - it_tmp_fraction-wertn.
        it_frac_addon-wrtfx = it_khs1-hwgfx - it_tmp_fraction-wrtfx.
        it_frac_addon-wdiff = it_frac_addon-wertn - it_frac_addon-wrtfx.
        COLLECT it_frac_addon.
        CLEAR   it_frac_addon.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

* Append to Main Itab
  IF NOT it_frac_addon[] IS INITIAL.
    APPEND LINES OF it_frac_addon  TO it_tmp_shopcost.
    CLEAR it_tmp_shopcost.
  ENDIF.

ENDFORM.                    " TRE_FRACTION_TO_V

*&---------------------------------------------------------------------*
*&      Form  DEL_DATA_FR_ZTCO_SHOPCOST
*&---------------------------------------------------------------------*
*       - > Always deletion -> Refresh data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM del_data_fr_ztco_shopcost.
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*operating error : Delete wrong data :One time.
*-----Start
  DATA : it_temp LIKE ztco_shopcost OCCURS 0 WITH HEADER LINE,
         w_int TYPE i.
  REFRESH it_temp.CLEAR w_int.
  SELECT * INTO TABLE it_temp
         FROM ztco_shopcost
          WHERE kokrs =  p_kokrs
           AND bdatj = '9999'.
  DESCRIBE TABLE it_temp LINES w_int.
  IF w_int <> 0.
    DELETE ztco_shopcost FROM TABLE it_temp.
  ENDIF.
*-----End
  DELETE FROM ztco_shopcost
        WHERE
              kokrs        =  p_kokrs
          AND bdatj        =  p_bdatj
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*          AND poper   BETWEEN p_perab and p_perbi
          AND poper   = p_perab
*-----End
          AND klvar        =  p_klvar
          AND versn        =  p_versn
          AND record_type  =  gv_record_type
          AND fsc_matnr    IN s_matnr.
  IF sy-subrc = 0.
  ENDIF.
* No- Error Check In Deletion Phase
  COMMIT WORK AND WAIT.

ENDFORM.                    " DEL_DATA_FR_ZTCO_SHOPCOST

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_SHOPCOST
*&---------------------------------------------------------------------*
*       Update/Insert
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_shopcost.
* Always Insertion
  CLEAR it_tmp_shopcost.

* NO Display - Zero Cost
  DELETE  it_tmp_shopcost
   WHERE wertn IS initial
     AND wrtfx IS initial
     AND wdiff IS initial.



**// Begin of Mod. By Hyung Jin Youn  2004.07.06
*    If data with Period "000", the data should have period.
*    This case can be occured when wrong data were generated in
*    standard system (No SHOP information)
* Local Data Definition
  DATA : lv_percount LIKE cosp-perbl. "Period Counter
  DATA : lv_cnt  LIKE  cosp-perbl.
  DATA : it_l_per000 LIKE  STANDARD TABLE OF it_tmp_shopcost
                     WITH HEADER LINE .
  DATA : it_l_perxxx LIKE  STANDARD TABLE OF it_tmp_shopcost
                     WITH HEADER LINE .

  LOOP AT it_tmp_shopcost WHERE poper = space.
    MOVE-CORRESPONDING it_tmp_shopcost TO it_l_per000.
    APPEND it_l_per000.
    CLEAR  it_l_per000.
* Delete
    DELETE it_tmp_shopcost.
    CLEAR  it_tmp_shopcost.
  ENDLOOP.

* Clear
  CLEAR : it_l_perxxx,  it_l_perxxx[].
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
** Cal. the Counter
*  lv_percount = p_perbi - p_perab + 1.
  lv_percount =  1.
*-----End
* Period Counter : Set From-Period .
  CLEAR lv_cnt.
  lv_cnt = p_perab .

* From period - To period
  DO lv_percount TIMES.
    LOOP AT it_l_per000.
      MOVE-CORRESPONDING it_l_per000 TO it_l_perxxx.
      it_l_perxxx-poper = lv_cnt.
      APPEND it_l_perxxx.
      CLEAR  it_l_perxxx.
      CLEAR  it_l_per000.
    ENDLOOP.
* Period Counter
    lv_cnt = lv_cnt + 1.
  ENDDO.

* Appending
  APPEND LINES OF  it_l_perxxx  TO it_tmp_shopcost.
  CLEAR it_tmp_shopcost.

**// End of Mod.


*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
  CLEAR : record_num.
*  LOOP AT it_tmp_shopcost.
  LOOP AT it_tmp_shopcost WHERE bdatj EQ  p_bdatj
                            AND poper EQ p_perab.
*-----End
* LOG
    it_tmp_shopcost-erdat = sy-datum.
    it_tmp_shopcost-erzet = sy-uzeit.
    it_tmp_shopcost-ernam = sy-uname.
* CURKY
    IF it_tmp_shopcost-hwaer EQ space .
      it_tmp_shopcost-hwaer = tka01-waers.
    ENDIF.
    CLEAR ztco_shopcost.
    MOVE-CORRESPONDING  it_tmp_shopcost TO ztco_shopcost.
    INSERT ztco_shopcost .
*----Start
    record_num = record_num + 1.
*----End
    IF sy-subrc <> 0.
*      WRITE : / ZTCO_SHOPCOST .
      MESSAGE e044.
    ENDIF.
    CLEAR it_tmp_shopcost.
  ENDLOOP.

ENDFORM.                    " UPDATE_ZTCO_SHOPCOST

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
*  DESCRIBE TABLE it_tmp_shopcost LINES sy-tfill.
  WRITE : / 'No. of Created Records : ' , record_num.
  WRITE : / 'Created date           : ' , sy-datum.
  WRITE : / 'Created By             : ' , sy-uname.
  SKIP 1.
  WRITE : / text-190.

* Success
  MESSAGE s009 WITH 'Data Creation'.

ENDFORM.                    " UP_INS_LOG

*&---------------------------------------------------------------------*
*&      Form  SET_PERIOD_NE_C_KSTAR_M
*&---------------------------------------------------------------------*
*         C_KSTAR_M <> 540300
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_period_ne_c_kstar_m.

  CLEAR : it_add_tmp_scost, it_add_tmp_scost[].

  LOOP AT it_tmp_shopcost WHERE typps = c_cat_m
                            AND kstar <> c_kstar_m
                            AND poper EQ space.
    MOVE-CORRESPONDING  it_tmp_shopcost TO it_add_tmp_scost.
    APPEND it_add_tmp_scost.
    CLEAR  it_add_tmp_scost.
    DELETE it_tmp_shopcost.
    CLEAR  it_tmp_shopcost.
  ENDLOOP.

* Multiply with Period key
* Multiply data( X the number of period )
  PERFORM mult_x_temp_itab_by_period.

* Append
  APPEND LINES OF it_add_tmp_scost TO it_tmp_shopcost.
  CLEAR it_tmp_shopcost.

ENDFORM.                    " SET_PERIOD_NE_C_KSTAR_M
*
*&---------------------------------------------------------------------*
*&      Form  determine_ccs_fr_ce
*&---------------------------------------------------------------------*
FORM determine_ccs_fr_ce.
*FIXIT LATER - ANDY
*  DATA: l_ccs TYPE  ck_element
*
*  call function 'KKEK_COST_COMPONENT_ELEMENT'
*       exporting
*            elehk_imp = 'H1'
*            ktopl_imp = 'HNA1'
*            kstar_imp = '540300'
*       importing
*            elemt_exp = l_ccs.
*
*  MOVE-CORRESPONDING  it_tmp_pe      TO   it_tmp_pren.
*  MOVE-CORRESPONDING  p_it_l_ck_key  TO   it_tmp_pren.
*  it_tmp_pren-elemt = it_l_kkb_split-elemt.
*  it_tmp_pren-wertn = it_l_kkb_split-w000.
*  it_tmp_pren-wrtfx = it_l_kkb_split-w001.
*  it_tmp_pren-wdiff = it_l_kkb_split-w002.
*  APPEND  it_tmp_pren.
ENDFORM.                    " determine_ccs_fr_ce
