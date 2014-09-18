REPORT zaco16u_abis_rep LINE-COUNT 65
                        NO STANDARD PAGE HEADING MESSAGE-ID zmco .



* Top Include
INCLUDE zaco16u_abis_rep_top.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY.
PARAMETERS : p_perab LIKE covja-perab MEMORY ID vpe
             MODIF ID per OBLIGATORY.

SELECT-OPTIONS : s_budat FOR mkpf-budat,
                 s_mtart FOR mara-mtart DEFAULT 'HALB',
                 s_werks FOR mseg-werks,
                 s_matnr FOR mara-matnr."obligatory.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_aufnr FOR aufk-aufnr,
                 s_artnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK bl2.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA : l_fdate TYPE datum.

  CONCATENATE p_bdatj p_perab+1(2) '01' INTO l_fdate.
* MONTH END DATE
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_fdate
       IMPORTING
            last_day_of_month = g_last_date.

** 1)  GI Data by pcc order
* 1-1) Get Cost est number by materia
*      Make material's object no & Get Product object no.
  PERFORM select_ckmlhd .
* 1-2) Get Variance GI
  PERFORM select_cpzp.
* 1-3) Get PCC data using Product Object no.
  PERFORM select_pcc_order.
* 1-4) Get Normal GI
  PERFORM select_covp.
* 1-5) Additional GI
  PERFORM select_ztco_abispost.


** 2) Goods movement data by llv_matnr
  PERFORM select_mseg.


** 3) Data merge for display.
  PERFORM make_it_pcc.
  PERFORM make_it_goodsmv.

  PERFORM make_it_display.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM call_alv_list.

*&---------------------------------------------------------------------*
*&      Form  select_pcc_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_pcc_order.


  SELECT aufk~objnr aufk~aufnr
         ckmlmv013~pmatn  ckmlmv013~verid ckmlmv013~prwrk
    INTO CORRESPONDING FIELDS OF TABLE lt_pcc
    FROM ckmlmv013
   INNER JOIN aufk
      ON aufk~aufnr   = ckmlmv013~pkosa
   INNER JOIN afko
      ON afko~aufnr   = aufk~aufnr
   INNER JOIN mkal
      ON mkal~matnr   = ckmlmv013~pmatn
     FOR ALL entries IN it_cpzp
   WHERE aufk~objnr = it_cpzp-objnr
     AND ckmlmv013~prwrk IN s_werks
     AND ckmlmv013~pmatn IN s_artnr
     AND ckmlmv013~pkosa IN s_aufnr
     AND ckmlmv013~loekz = space       "deletion
     AND ckmlmv013~autyp = '05'.       "PCC


  LOOP AT lt_pcc .
    it_fsc_mat-aufnr      = lt_pcc-aufnr.
    it_fsc_mat-objnr      = lt_pcc-objnr.
    it_fsc_mat-verid      = lt_pcc-verid.
    it_fsc_mat-werks      = lt_pcc-prwrk.
    it_fsc_mat-matnr      = lt_pcc-pmatn.
    APPEND   it_fsc_mat.  CLEAR it_fsc_mat.
  ENDLOOP.


  IF it_fsc_mat[] IS INITIAL.
    MESSAGE e026.
  ENDIF.


ENDFORM.                    " select_pcc_order
*&---------------------------------------------------------------------*
*&      Form  select_covp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_covp.

  LOOP AT it_fsc_mat.
    PERFORM get_covp_summary.
  ENDLOOP.

  LOOP AT it_covp.
    CLEAR: it_coep.
    MOVE-CORRESPONDING it_covp TO it_coep.

    COLLECT it_coep. CLEAR it_coep.
  ENDLOOP.


* Get item category for define COEP typps
  PERFORM read_cosp_coss.

ENDFORM.                    " select_covp
*&---------------------------------------------------------------------*
*&      Form  get_covp_summary
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_covp_summary.

  g_versn = '000'.
  SELECT  objnr kstar werks matnr budat
          vrgng meinb SUM( mbgbtr )
          sgtxt
     APPENDING TABLE it_covp
     FROM covp
     WHERE lednr = '00'
       AND objnr = it_fsc_mat-objnr
       AND gjahr = p_bdatj
       AND perio = p_perab
       AND wrttp = '04'
       AND versn = g_versn
       AND budat IN s_budat
       AND matnr IN s_matnr
       and werks in s_werks
     GROUP BY OBJNR kstar WERKS matnr budat
              vrgng meinb sgtxt.

ENDFORM.                    " get_covp_summary
*&---------------------------------------------------------------------*
*&      Form  make_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_pcc.
  DATA : l_matnr(20),
         l_os_key(20),
         l_typps(1).

  LOOP AT it_coep.
*  1) PCC data
    CLEAR it_fsc_mat.
    READ TABLE it_fsc_mat WITH KEY objnr = it_coep-objnr.
    it_pcc-aufnr     = it_fsc_mat-aufnr.
    it_pcc-artnr     = it_fsc_mat-matnr.
    it_pcc-verid     = it_fsc_mat-verid.


*  2) COEP data
    it_pcc-llv_matnr = it_coep-matnr.
    it_pcc-budat     = it_coep-budat.
**   Check if user id show or not
*    IF p_user = 'X'.
*      it_pcc-usnam     = it_coep-usnam.
*    ELSE.
*      CLEAR it_pcc-usnam.
*    ENDIF.


*   get item_category
    PERFORM get_item_category CHANGING l_typps.

*  3) Normal GI
    CASE l_typps.
      WHEN 'M'  OR 'E'.
        it_pcc-mbgbtr     = it_coep-mbgbtr.
    ENDCASE.

*  5) Additioanl GI
    CLEAR it_abispost.
    READ TABLE it_abispost WITH KEY " werks     = it_pcc-werks
                                    matnr     = it_pcc-llv_matnr
                                    fsc_matnr = it_pcc-artnr
                                    budat     = it_pcc-budat.
    IF it_abispost-stype = 'OS&D'.
      it_pcc-osnd = it_coep-mbgbtr.
    ELSEIF  it_abispost-stype = 'KEYIN'.
      it_pcc-keyin = it_coep-mbgbtr.
    ENDIF.


    it_pcc-mbgbtr2 = it_cpzp-varmn.
*  5) Variance GI
    CLEAR it_ckmlhd .
    READ TABLE it_ckmlhd WITH KEY matnr = it_pcc-llv_matnr.

    CLEAR it_cpzp.
    READ TABLE it_cpzp WITH KEY objnr   = it_coep-objnr
                                f_objnr = it_ckmlhd-f_objnr
                                budat   = it_pcc-budat.

    it_pcc-mbgbtr2 = it_cpzp-varmn.
    it_mara-matnr = it_pcc-llv_matnr.
    COLLECT it_mara. CLEAR it_mara.
    COLLECT it_pcc. CLEAR it_pcc.
  ENDLOOP.



ENDFORM.                    " make_itab
*&---------------------------------------------------------------------*
*&      Form  select_mseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_mseg.

  SELECT b~matnr b~werks b~lgort
         b~bwart a~usnam b~menge a~budat
      INTO CORRESPONDING FIELDS OF TABLE it_mseg
      FROM mkpf AS a
     INNER JOIN mseg AS b
        ON a~mblnr  = b~mblnr
       AND a~mjahr  = b~mjahr
     INNER JOIN mara AS c
        ON b~matnr  = c~matnr
     WHERE a~mjahr = p_bdatj
       AND a~budat IN s_budat
       AND b~werks IN s_werks
       AND b~matnr IN s_matnr
       AND b~bwart IN ('301', '302', '311', '312', '343', '344')
       AND c~mtart IN s_mtart.

*  LOOP AT it_mseg.
*    CASE it_mseg-bwart.
*      WHEN '311' OR '312'.
*        IF it_mseg-lgort = 'X551' OR it_mseg-lgort = 'X905'.
*        ELSE.
*          DELETE it_mseg.
*          CONTINUE.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.

ENDFORM.                    " select_mseg
*&---------------------------------------------------------------------*
*&      Form  select_CKMLHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_ckmlhd.

  DATA : l_kalnr TYPE string,
         l_temp(1).
  SELECT a~kalnr a~matnr a~bwkey
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlhd
    FROM ckmlhd AS a
   INNER JOIN mara AS b
     ON  a~matnr = b~matnr
   WHERE a~matnr IN s_matnr
     AND a~bwkey IN s_werks
     AND b~mtart IN s_mtart.


  LOOP AT it_ckmlhd.
*   f_objnr will use for selection cpzp
    l_kalnr = it_ckmlhd-kalnr .
    SHIFT l_kalnr RIGHT.
*   VS & kalnr : 2 digit space
    CONCATENATE 'VS' l_kalnr INTO it_ckmlhd-f_objnr
    SEPARATED BY space.
    MODIFY it_ckmlhd. CLEAR it_ckmlhd.
  ENDLOOP.


ENDFORM.                    " select_CKMLHD
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_GOODMOV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_goodsmv.
  LOOP AT it_mseg.
    MOVE-CORRESPONDING it_mseg TO it_goodsmv.
    it_goodsmv-llv_matnr = it_mseg-matnr.
    CASE it_mseg-bwart.
      WHEN '301' .
        it_goodsmv-trf = it_mseg-menge.
      WHEN '302' .
        it_goodsmv-trf = it_mseg-menge * - 1.
      WHEN '311'.
        IF it_mseg-lgort =  'P230' .                        "'X551'.
          it_goodsmv-x551 = it_mseg-menge.
        ELSEIF it_mseg-lgort = '9999' .                     " 'X905'.
          it_goodsmv-x905 = it_mseg-menge.
        ENDIF.
      WHEN '312'.
        IF it_mseg-lgort = 'X551'.
          it_goodsmv-x551 = it_mseg-menge * -1.
        ELSEIF it_mseg-lgort = 'X905'.
          it_goodsmv-x905 = it_mseg-menge * -1.
        ENDIF.
      WHEN '343'.
        it_goodsmv-scrap = it_mseg-menge.
      WHEN '344'.
        it_goodsmv-scrap = it_mseg-menge * - 1.
    ENDCASE.
*   Check if user id show or not
*    IF p_user = 'X'.
*      it_goodsmv-usnam  = it_mseg-usnam.
*    ELSE.
*      CLEAR it_goodsmv-usnam  .
*    ENDIF.
    it_mara-matnr = it_goodsmv-llv_matnr.
    COLLECT it_mara. CLEAR it_mara.
    COLLECT it_goodsmv. CLEAR it_goodsmv.
  ENDLOOP.


ENDFORM.                    " MAKE_IT_GOODMOV
*&---------------------------------------------------------------------*
*&      Form  read_cosp_coss
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cosp_coss.
* Local Data Definition
  DATA : it_lt_cospa LIKE STANDARD TABLE OF cospa
                     WITH HEADER LINE .
  DATA : it_lt_cossa LIKE STANDARD TABLE OF cossa
                     WITH HEADER LINE .

* Read COSP ( Only Debit data / BEKNZ = 'S' ) Fct Out
  CLEAR : it_lt_cospa, it_lt_cospa[].
  SELECT * FROM cosp
          INTO CORRESPONDING FIELDS OF TABLE it_lt_cospa
           FOR ALL ENTRIES IN it_fsc_mat
           WHERE lednr = '00'
             AND objnr = it_fsc_mat-objnr
             AND gjahr = p_bdatj
             AND wrttp = '04'
             AND versn = g_versn.

* Read COSS ( Only Debit data / BEKNZ = 'S' )
  CLEAR : it_lt_cossa, it_lt_cossa[].
  SELECT * FROM coss
          INTO CORRESPONDING FIELDS OF TABLE it_lt_cossa
           FOR ALL ENTRIES IN it_fsc_mat
           WHERE lednr = '00'
             AND objnr = it_fsc_mat-objnr
             AND gjahr = p_bdatj
             AND wrttp = '04'
             AND versn = g_versn.

*C	Goods issues
*F	Confirmations
*I	Overhead
*L	Miscellaneous
*P	Goods receipt
*X	Settlement
*O	Distribution
  LOOP AT it_lt_cospa.
    MOVE-CORRESPONDING it_lt_cospa TO it_categ.
    CALL FUNCTION 'K_KKB_BEWEG_SET'
         EXPORTING
              i_cospa  = it_lt_cospa
         IMPORTING
              e_beweg  = it_categ-beweg
         EXCEPTIONS
              no_input = 1
              OTHERS   = 2.
    COLLECT it_categ.
  ENDLOOP.

  LOOP AT  it_lt_cossa.
    MOVE-CORRESPONDING it_lt_cossa TO it_categ.
    CALL FUNCTION 'K_KKB_BEWEG_SET'
         EXPORTING
              i_cossa  = it_lt_cossa
         IMPORTING
              e_beweg  = it_categ-beweg
         EXCEPTIONS
              no_input = 1
              OTHERS   = 2.
    COLLECT it_categ.
  ENDLOOP.

  SORT  it_categ BY objnr kstar vrgng.
  CLEAR it_categ.

ENDFORM.                    " read_cosp_coss
*&---------------------------------------------------------------------*
*&      Form  SELECT_CPZP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_cpzp.

  DATA : l_gjper LIKE cpzp-gjper.

  CHECK NOT it_ckmlhd[] IS INITIAL.
  CONCATENATE p_bdatj p_perab INTO l_gjper.

  SELECT objnr f_objnr varmn
  INTO CORRESPONDING FIELDS OF TABLE it_cpzp
  FROM cpzp
   FOR ALL ENTRIES IN it_ckmlhd
  WHERE gjper = l_gjper
    AND objnr LIKE 'OR%'
    AND f_objnr = it_ckmlhd-f_objnr.

  LOOP AT it_cpzp.
    it_cpzp-budat = g_last_date.
    MODIFY it_cpzp. CLEAR it_cpzp.
  ENDLOOP.

ENDFORM.                    " SELECT_CPZP
*&---------------------------------------------------------------------*
*&      Form  conv_beweg_to_typps
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CATEG_BEWEG  text
*      -->P_IT_COEP_TYPPS  text
*----------------------------------------------------------------------*
FORM conv_beweg_to_typps USING    p_beweg LIKE kkbcs_out-beweg
                         CHANGING pp_typps TYPE typps.
  CASE p_beweg.
    WHEN 'C'.     pp_typps = 'M'.
    WHEN 'F'.     pp_typps = 'E'.
    WHEN 'L'.     pp_typps = 'V'.
    WHEN 'P'.     pp_typps = 'O'.

    WHEN 'X'.     pp_typps = 'X'.
    WHEN 'I'.     pp_typps = 'I'.
  ENDCASE.

ENDFORM.                    " conv_beweg_to_typps
*&---------------------------------------------------------------------*
*&      Form  get_item_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_COEP_TYPPS  text
*----------------------------------------------------------------------*
FORM get_item_category CHANGING p_typps.

*Item Category (C,F,L->M,E,V) GR-O,Settle-X
  CLEAR it_categ.
  READ TABLE it_categ WITH KEY objnr = it_covp-objnr
                               kstar = it_covp-kstar
                               vrgng = it_covp-vrgng.
  IF sy-subrc = 0.
    PERFORM conv_beweg_to_typps  USING    it_categ-beweg
                                 CHANGING p_typps.
  ENDIF.

ENDFORM.                    " get_item_category
*&---------------------------------------------------------------------*
*&      Form  select_ZTCO_ABISPOST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_ztco_abispost.

  SELECT werks matnr stype mbgbtr meinb fsc_matnr
    INTO CORRESPONDING FIELDS OF TABLE it_abispost
    FROM ztco_abispost
   WHERE gjahr  = p_bdatj
     AND period = p_perab
     AND versn  = g_versn
     AND matnr  IN s_matnr
     AND werks  IN s_werks.

  LOOP AT it_abispost.
    it_abispost-budat = g_last_date.
    MODIFY it_abispost. CLEAR it_abispost.
  ENDLOOP.



ENDFORM.                    " select_ZTCO_ABISPOST
*&---------------------------------------------------------------------*
*&      Form  call_alv_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.

  PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
   'LLV_MATNR'  'Material'      '18' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
   'MAKTX'      'Description'   '30' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
   'BUDAT'      'PstDate'       '10' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
   'AUFNR'      'PCC'           '10' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
   'ARTNR'      'Product'       '18' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
   'VERID'      'VerID'         '5'  'X' ' ' ' ' ' ' ' ' ' ' ' ',
   'MBGBTR'     'Normal GI'     '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
   'MBGBTR2'    'Variance GI'   '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
   'OSND'       'OS&D'          '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
   'KEYIN'      'KEYIN'         '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
   'SCRAP'      'Scrap'         '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
   'X551'       'X551 Stock'    '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
   'X905'       'X905 Stcok'    '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X',
   'TRF'        'Plant TRF'     '15' ' ' 'R' ' ' ' ' ' ' ' ' 'X'.


  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = it_display
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.



ENDFORM.                    " call_alv_list
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_display.
  DATA : BEGIN OF it_makt OCCURS 0,
           matnr LIKE makt-matnr,
           maktx LIKE makt-maktx,
         END OF it_makt.

  CHECK  NOT it_mara[] IS INITIAL.
  SELECT matnr maktx
    INTO CORRESPONDING FIELDS OF TABLE it_makt
     FROM makt
       FOR ALL ENTRIES IN it_mara
    WHERE matnr = it_mara-matnr
      AND spras = sy-langu.


  LOOP AT it_pcc WHERE aufnr IN s_aufnr
                   AND artnr IN s_artnr.
    MOVE-CORRESPONDING it_pcc TO it_display.
    it_display-type = 'P'.
    CLEAR it_makt.
    READ TABLE it_makt WITH KEY matnr = it_display-llv_matnr.
    it_display-maktx = it_makt-maktx.
    COLLECT it_display. CLEAR it_display.
  ENDLOOP.

  CHECK s_artnr[] IS INITIAL.
  CHECK s_aufnr[] IS INITIAL.
  LOOP AT it_goodsmv .
    MOVE-CORRESPONDING it_goodsmv TO it_display.
    it_display-type = 'G'.
    CLEAR it_makt.
    READ TABLE it_makt WITH KEY matnr = it_display-llv_matnr.
    it_display-maktx = it_makt-maktx.

    COLLECT it_display. CLEAR it_display.
  ENDLOOP.

  SORT it_display.

ENDFORM.                    " MAKE_IT_DISPLAY
