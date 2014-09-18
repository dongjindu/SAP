*----------------------------------------------------------------------*
*   INCLUDE ZIPP303L_MAT_CRE_BDC_01_F01                                *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
  SELECT *
       FROM ztbm_abxmmcdt
       INTO TABLE it_ammc
       WHERE zedat EQ p_zedat
       AND   zbtim EQ p_zbtim.
  IF sy-subrc NE 0.
    WRITE: / text-001 COLOR 6 ON.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
  PERFORM check_material_same_data.
  PERFORM collect_lt_matnr_read_mara.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM write_process.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-002, wa_line_idx.
  WRITE: / text-003, wa_erro_idx.
  FORMAT COLOR OFF.
  IF wa_erro_idx GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / text-004.
    FORMAT COLOR OFF.
    WRITE: /(10)  text-005,
            (20)  text-006,
            (20)  text-007,
            (40)  text-008,
            (10)  text-009,
            (03)  text-010,
            (06)  text-011,
            (12)  text-012,
            (18)  text-013,
            (10)  text-014,
            (07)  text-015,
            (07)  text-016,
            (07)  text-017,
            (07)  text-018,
            (10)  text-019,
            (10)  text-020,
            (18)  text-021,
            (10)  text-022,
            (10)  text-023,
            (11)  text-024,
                  text-025.

    FORMAT COLOR OFF.
    LOOP AT it_ammc WHERE zresult EQ 'E'.
      WRITE: /(10) it_ammc-plnt,
              (20) it_ammc-mtyp,
              (20) it_ammc-mtno,
              (40) it_ammc-zdesc,
              (10) it_ammc-indu,
              (03) it_ammc-unit,
              (06) it_ammc-mgrp,
              (12) it_ammc-gica,
              (18) it_ammc-sour,
              (10) it_ammc-mtcn,
              (07) it_ammc-mrpy,
              (07) it_ammc-mrpc,
              (07) it_ammc-lots,
              (07) it_ammc-prty,
              (10) it_ammc-smky,
              (10) it_ammc-avck,
              (18) it_ammc-cnmt,
              (10) it_ammc-slmd,
              (10) it_ammc-inco,
              (11) it_ammc-vesn,
                   it_ammc-zmsg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  COLLECT_LT_MATNR_READ_MARA
*&---------------------------------------------------------------------*
FORM collect_lt_matnr_read_mara.
  DATA l_tabix TYPE sy-tabix.
  DATA lt_ammc TYPE ztbm_abxmmcdt OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF lt_matnr OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
        END   OF lt_matnr.
  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
          mtart TYPE mara-mtart,
        END   OF lt_marc.
* DATA TOTAL LINES
  DESCRIBE TABLE it_ammc LINES wa_line_idx.

  LOOP AT it_ammc.
    lt_matnr-matnr = it_ammc-mtno.
    lt_matnr-werks = it_ammc-plnt.
    COLLECT lt_matnr.
    CLEAR: lt_matnr, it_ammc.
  ENDLOOP.

  IF NOT lt_matnr[] IS INITIAL.
    SELECT a~matnr
           a~werks
           b~mtart
         FROM marc AS a INNER JOIN mara AS b
                        ON a~matnr EQ b~matnr
         INTO TABLE lt_marc
         FOR ALL ENTRIES IN lt_matnr
         WHERE a~matnr EQ lt_matnr-matnr
         AND   a~werks EQ lt_matnr-werks.
    IF sy-subrc EQ 0.
*     SORTING
      SORT lt_marc BY matnr werks mtart.
*     ERROR CHECK
      LOOP AT it_ammc.
        l_tabix = sy-tabix.
        READ TABLE lt_marc WITH KEY matnr = it_ammc-mtno
                                    werks = it_ammc-plnt
*                                    MTART = IT_AMMC-MTYP
                           BINARY SEARCH
                           TRANSPORTING mtart.
        IF sy-subrc EQ 0.
*          P_CHECK = 'X'.
          lt_ammc = it_ammc.
          lt_ammc-zresult = 'L'.
*         MATERIAL TYPE CHECKING
          IF lt_marc-mtart EQ it_ammc-mtyp.
            lt_ammc-zmsg = 'MATERIAL does exist'.
          ELSE.
            lt_ammc-zmsg =
                      'MATERIAL does exist OR MATERIAL TYPE unequal '.
          ENDIF.
          PERFORM zsbm_if_time_change USING     'E'
                                                it_ammc-zedat
                                                it_ammc-zetim
                                       CHANGING lt_ammc-zbdat
                                                lt_ammc-zbtim
                                                lt_ammc-zbnam
                                                lt_ammc-zmode.
          APPEND lt_ammc.
          DELETE it_ammc INDEX l_tabix.
        ENDIF.
        CLEAR: it_ammc, lt_marc, lt_ammc.
      ENDLOOP.
    ENDIF.
  ENDIF.
  DESCRIBE TABLE lt_ammc LINES wa_erro_idx.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-026, wa_line_idx.
  WRITE: / text-027, wa_erro_idx.
  FORMAT COLOR OFF.
  PERFORM write_error TABLES   lt_ammc
                      USING    'L'.
  PERFORM update_ztbm_abxmmcdt TABLES   lt_ammc.
ENDFORM.                    " COLLECT_LT_MATNR_READ_MARA
*&---------------------------------------------------------------------*
*&      Form  ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
FORM zsbm_if_time_change USING     p_chk
                                   p_zedat
                                   p_zetim
                          CHANGING p_zbdat
                                   p_zbtim
                                   p_zbnam
                                   p_zmode.
* BDC EXECUTE DATE
  p_zbdat = sy-datum.
  p_zbnam = sy-uname.
  p_zmode = 'C'.
* If error becomes, do not input time.
  IF p_chk EQ 'S'.
    p_zbtim = sy-uzeit.
  ENDIF.
ENDFORM.                    " ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
*&      Form  ZTBM_ABXMMCDT_MARA_UPDATE
*&---------------------------------------------------------------------*
FORM ztbm_abxmmcdt_mara_update.
* ZTBM_ABXMMCDT UPDATE
  UPDATE ztbm_abxmmcdt FROM TABLE it_ammc.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.


ENDFORM.                    " ZTBM_ABXMMCDT_MARA_UPDATE
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
  p_zedat = sy-datum.
  CLEAR: p_zbtim.
* BDC MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR
*&---------------------------------------------------------------------*
FORM write_error TABLES    pt_ammc STRUCTURE it_ammc
                 USING     zresult.
  IF NOT pt_ammc[] IS INITIAL.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: /(10)  text-005,
            (20)  text-006,
            (20)  text-007,
            (40)  text-008,
            (10)  text-009,
            (03)  text-010,
            (06)  text-011,
            (12)  text-012,
            (18)  text-013,
            (10)  text-014,
            (07)  text-015,
            (07)  text-016,
            (07)  text-017,
            (07)  text-018,
            (10)  text-019,
            (10)  text-020,
            (18)  text-021,
            (10)  text-022,
            (10)  text-023,
            (11)  text-024,
            (10)  text-028,
                  text-025.
    FORMAT COLOR OFF.
    LOOP AT pt_ammc WHERE zresult EQ zresult.
      WRITE: /(10) pt_ammc-plnt,
              (20) pt_ammc-mtyp,
              (20) pt_ammc-mtno,
              (40) pt_ammc-zdesc,
              (10) pt_ammc-indu,
              (03) pt_ammc-unit,
              (06) pt_ammc-mgrp,
              (12) pt_ammc-gica,
              (18) pt_ammc-sour,
              (10) pt_ammc-mtcn,
              (07) pt_ammc-mrpy,
              (07) pt_ammc-mrpc,
              (07) pt_ammc-lots,
              (07) pt_ammc-prty,
              (10) pt_ammc-smky,
              (10) pt_ammc-avck,
              (18) pt_ammc-cnmt,
              (10) pt_ammc-slmd,
              (10) pt_ammc-inco,
              (11) pt_ammc-vesn,
              (10) pt_ammc-zresult,
                   pt_ammc-zmsg.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_ERROR
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXMMCDT
*&---------------------------------------------------------------------*
FORM update_ztbm_abxmmcdt TABLES   pt_ammc STRUCTURE it_ammc.
* ERROR DATA UPDATE
  UPDATE ztbm_abxmmcdt FROM TABLE pt_ammc.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXMMCDT
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM bdc_process.
  DATA: lt_ammc LIKE it_ammc OCCURS 0 WITH HEADER LINE.
* DATA TOTAL LINES
  DESCRIBE TABLE it_ammc LINES wa_line_idx.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-026, wa_line_idx.
  FORMAT COLOR OFF.

  PERFORM bdc_execution.

  REFRESH: it_bdc, it_mess.
* CONFIGURABLE 'X'
  PERFORM mm02_bdc_exection.

  CLEAR: wa_line_idx, wa_erro_idx.
* ERROR LINES
  LOOP AT it_ammc.
    CASE it_ammc-zresult.
      WHEN 'E'.
*        WA_ERRO_IDX = WA_ERRO_IDX + 1.
        lt_ammc = it_ammc.
        APPEND lt_ammc.
      WHEN OTHERS.
        wa_line_idx = wa_line_idx + 1.
    ENDCASE.
    CLEAR: it_ammc, lt_ammc.
  ENDLOOP.
  DESCRIBE TABLE lt_ammc LINES wa_erro_idx.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / text-002, wa_line_idx.
  WRITE: / text-003, wa_erro_idx.
  FORMAT COLOR OFF.
  PERFORM write_error TABLES   lt_ammc
                      USING    'E'.
* ZTBM_ABXMMCDT UPDATE
  PERFORM update_ztbm_abxmmcdt TABLES   it_ammc.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_SAME_DATA
*&---------------------------------------------------------------------*
FORM check_material_same_data.
  DATA: lt_ammc LIKE it_ammc OCCURS 0 WITH HEADER LINE,
        mt_ammc LIKE it_ammc OCCURS 0 WITH HEADER LINE,
        l_tabix TYPE sy-tabix,
        l_count TYPE i.
* DATA TOTAL LINES
  DESCRIBE TABLE it_ammc LINES wa_line_idx.
  lt_ammc[] = it_ammc[].
  CLEAR: lt_ammc, it_ammc.

  LOOP AT it_ammc.
    l_tabix = sy-tabix.
    LOOP AT lt_ammc WHERE mtno EQ it_ammc-mtno.
      l_count = l_count + 1.
    ENDLOOP.
    IF l_count GT 2.
*      P_CHECK = 'X'.
      mt_ammc = it_ammc.
      mt_ammc-zresult = 'L'.
      mt_ammc-zmsg = 'MATERIAL does exist that PLANT is unequal'.
      PERFORM zsbm_if_time_change USING     'E'
                                             mt_ammc-zedat
                                             mt_ammc-zetim
                                    CHANGING mt_ammc-zbdat
                                             mt_ammc-zbtim
                                             mt_ammc-zbnam
                                             mt_ammc-zmode.
      APPEND mt_ammc.
      DELETE it_ammc INDEX l_tabix.
    ENDIF.
    CLEAR: l_count, it_ammc, lt_ammc, mt_ammc.
  ENDLOOP.
  IF NOT mt_ammc[] IS INITIAL.
    DESCRIBE TABLE mt_ammc LINES wa_erro_idx.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    WRITE: / text-026, wa_line_idx.
   WRITE: / 'MATERIAL does exist that PLANT is unequal : ', wa_erro_idx.
    FORMAT COLOR OFF.
    PERFORM write_error TABLES   mt_ammc
                        USING    'L'.
    PERFORM update_ztbm_abxmmcdt TABLES   mt_ammc.
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL_SAME_DATA
*&---------------------------------------------------------------------*
*&      Form  FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM fert_selection_view.
  DATA: l_vkorg TYPE rmmg1-vkorg,
        l_prodh TYPE t179t-prodh,  "Product hierarchy
** Added By Tonkey on 05/21/2004.
        l_mtpos      TYPE mvke-mtpos,   "Item category group from M/M
        l_natn_c     TYPE ztbm_abxtrtdt-natn_c,
        l_delr_c     TYPE ztbm_abxtrtdt-delr_c,
        l_blnk_cloc4 TYPE ztbm_abxtrtdt-blnk_cloc4,
*    2004.03.15 CHANGE
        l_lgort TYPE rmmg1-lgort,
        l_vtweg TYPE rmmg1-vtweg,
        l_spart TYPE mara-spart,
        l_dwerk TYPE mvke-dwerk,
        l_ktgrm TYPE mvke-ktgrm,
        l_tragr TYPE mara-tragr,
        l_ladgr TYPE marc-ladgr,
        l_disgr TYPE marc-disgr,
        l_strgr TYPE marc-strgr,
        l_vrmod TYPE marc-vrmod,
        l_sauft TYPE marc-sauft,       "Repetitive Mfg Indicator
        l_sfepr TYPE marc-sfepr,       "REM profile
        l_profil TYPE marc-profil,     "Backfl.Profile
        l_fevor TYPE marc-fevor,     "Production scheduler
        l_uneto TYPE marc-uneto,     "Underdely tol.
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(06),    "Price unitPrice unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1),                  "Costing lot size
*-----start wskim 02/11/2005
        l_brgew(16),
        l_ntgew(16),
        l_gewei(3),
        z_org(5),
        z_spec(3),
        z_item(18).

  DATA : it_inf LIKE ztbm_fsc_cre_inf OCCURS 0 WITH HEADER LINE.
*-----end
************************
** Changed By Tonkey 05/21/2004.
  MOVE: it_ammc-mtno+01(03) TO l_natn_c,
        it_ammc-mtno+04(02) TO l_delr_c.
*-----Start wskim 02/11/2005
*  SELECT SINGLE blnk_cloc4
*    INTO l_blnk_cloc4
*    FROM ztbm_abxtrtdt
*    WHERE natn_c = l_natn_c AND  "Nation
*          delr_c = l_delr_c   .  "Dealer
*   CASE l_blnk_cloc4.
*    WHEN 'D'.
*      l_vkorg = 'D100'.
*      it_ammc-gica = '0002'.  "Item category group from M/M
*    WHEN 'E'.
*      l_vkorg = 'E100'.
*      it_ammc-gica = 'Z002'.
*  ENDCASE.
  REFRESH it_inf. CLEAR :z_org,z_item.

  SELECT * INTO TABLE it_inf
          FROM ztbm_fsc_cre_inf.
*Sales org.
  MOVE : it_ammc-mtno+1(5)  TO z_org.

  CONCATENATE 'SA_' z_org INTO z_item.

  READ TABLE it_inf WITH KEY item = z_item.
  IF sy-subrc = 0.
    MOVE : it_inf-valu2 TO l_vkorg,
           it_inf-valu3 TO l_vtweg.
  ELSE.
    CASE it_ammc-mtno+1(3).
      WHEN 'B28'.
        l_vkorg = 'D100'.  "Sales organization
      WHEN 'B06'.
        l_vkorg = 'E100'.  "Sales organization
      WHEN OTHERS.
        l_vkorg = 'E100'.  "Sales organization
    ENDCASE.
    l_vtweg   = '10'   .  "Distribution level

    MESSAGE i001 WITH 'Sales org Data missing'
                      'Check : T-code ZBM_FINF'.
  ENDIF.
*Division
  CLEAR : z_spec, z_item.
  MOVE : it_ammc-mtno+6(2)  TO z_spec.

  CONCATENATE 'DI_' z_spec INTO z_item.

  READ TABLE it_inf WITH KEY item = z_item.
  IF sy-subrc = 0.
    MOVE : it_inf-valu1 TO l_spart.
  ELSE.
    l_spart   = '10'   .  "Division
    MESSAGE i001 WITH 'Division Data missing' 'Check : T-code ZBM_FINF'.
  ENDIF.
*Weight
  CLEAR :z_spec,z_item.

  MOVE : it_ammc-mtno+6(2)  TO z_spec,
         it_ammc-mtno+10(1) TO z_spec+2(1).

  CONCATENATE 'WE_' z_spec INTO z_item.

  READ TABLE it_inf WITH KEY item = z_item.
  IF sy-subrc = 0.
    MOVE : it_inf-valu1 TO l_brgew,
           it_inf-valu2 TO l_ntgew,
           it_inf-valu3 TO l_gewei.
  ELSE.
    MESSAGE i001 WITH 'Weight Data missing' 'Check : T-code ZBM_FINF'.
  ENDIF.
*-----end

*   =
*----end
*  case it_ammc-mtno+1(3).
*    when 'B28'.
*      l_vkorg = 'D100'.  "Sales organization
*    when 'B06'.
*      l_vkorg = 'E100'.  "Sales organization
*  endcase.
**

*    2004.03.15 CHANGE
  l_lgort   = 'F001' .  "Storage location
*----start wskim 02/11/2005
*  l_vtweg   = '10'   .  "Distribution level
*  l_spart   = '10'   .  "Division
*----end
  l_dwerk   = 'P001' .  "Delivering plant
  l_ktgrm   = '10'   .  "Acct assignment grp
  l_tragr   = '0001' .  "Trans. grp
  l_ladgr   = 'P100' .  "LoadingGrp
  l_disgr   = '0001' .  "MRP group
  l_strgr   = '56'   .  "Strategy group
  l_vrmod   = '1'    .  "Consumption mode
  l_sauft   = 'X'    .  "Repetitive Mfg
  l_sfepr   = 'VEHI' .  "REM profile
  l_profil  = 'SAP2' .  "Backfl.Profile
  l_fevor   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  l_bklas   = '7920' .  "Valuation class
  l_peinh_1 = '1'    .  "Price unit
  l_ekalr   = 'X'    .  "with qty structure
  l_losgr   = '1'    .  "Costing lot size
*
** Unactivated By Tonkey on 05/21/2004.
*  it_ammc-gica = '0002'.  "Item category group
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-mrpy = 'PD'.    "MRP type
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = 'V01'.   "MRP controller
**end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-prty = 'E'.     "Procurement type
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-slmd = '2'.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.
**************************
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
     ' ' 'BDC_OKCODE'  '/00'.
* ORGANIZATIONAL LEVELS

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,  "PLANT
*    2004.03.15 CHANGE
     ' ' 'RMMG1-LGORT' l_lgort,       "Storage location
     ' ' 'RMMG1-VTWEG' l_vtweg,       "Distribution level
*******************************
     ' ' 'RMMG1-VKORG' l_vkorg,       "Sales organization
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  PERFORM read_t179t USING    it_ammc-mtyp
                              it_ammc-mtno+6(2)
                     CHANGING l_prodh.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX'      it_ammc-zdesc,  "Description
     ' ' 'MARA-MEINS'      it_ammc-unit,   "Unit
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,   "GenItemCatGroup
     ' ' 'MARA-PRDHA'      l_prodh,        "Prod.hierarchy
*    2004.03.15 CHANGE
     ' ' 'MARA-SPART'      l_spart,        "Division
*-----start wskim 02/11/2005
     ' ' 'MARA-BRGEW'      l_brgew,        "Gross weight
     ' ' 'MARA-NTGEW'      l_ntgew,        "net weight
     ' ' 'MARA-GEWEI'      l_gewei,        "unit
*-----end
**********************************
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
*-----start wskim 02/11/2005 " default 'M' Choi
*     ' ' 'MARA-PROFL' it_ammc-sour,        "LP/KD/MIP
     ' ' 'MARA-PROFL' 'M',        "LP/KD/MIP
*----end
     ' ' 'BDC_OKCODE'  '=SP04'.
* Classification
* N/A
* Sales: Sales Org. Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*-----start 02/16/05 by wskim
*     ' ' 'MARA-SPART' ' ',                 "Division
      ' ' 'MARA-SPART' l_spart,
*-----end
*    2004.03.15 CHANGE
     ' ' 'MVKE-DWERK' l_dwerk,              "Delivering plant
*******************************
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
*-----start wskim 02/11/2005  change from 1 to 0
* 03/15/2005  change From 1 to 0
     ' ' 'MG03STEUER-TAXKM(01)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(02)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(03)' '0',       "Tax classification
*     ' ' 'MG03STEUER-TAXKM(01)' '1',       "Tax classification
*     ' ' 'MG03STEUER-TAXKM(02)' '1',       "Tax classification
*     ' ' 'MG03STEUER-TAXKM(03)' '1',       "Tax classification
*-----END
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MVKE-KTGRM'  l_ktgrm,   "Acct assignment grp
*******************************
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,         "Gen. item cat. grp
     ' ' 'MVKE-PRODH'      l_prodh,              "Product hierarchy
     ' ' 'MVKE-MVGR3'      it_ammc-mtno+12(01),  "Material group 3
     ' ' 'MVKE-MVGR4'      it_ammc-mtno+10(01),  "Material group 4
     ' ' 'MVKE-MVGR5'      it_ammc-mtno+09(01),  "Material group 5
     ' ' 'MVKE-MTPOS'      it_ammc-gica,         "Item category group
     ' ' 'BDC_OKCODE'      '=SP06'.
* Sales: General/Plant Data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MARC-MTVFP'  it_ammc-avck,   "Availability check
*******************************
     ' ' 'MARA-TRAGR'  l_tragr,  "Trans. grp
*    2004.03.15 CHANGE
     ' ' 'MARC-LADGR'  l_ladgr,  "LoadingGrp
*******************************
     ' ' 'BDC_OKCODE'  '=SP12'.

* Foreign Trade: Export Data

* Sales Text

* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISGR'  l_disgr,       "MRP group
     ' ' 'MARC-DISMM'  it_ammc-mrpy,  "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,  "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-LGPRO'  l_lgort,        "Issue stor. location
     ' ' 'MARC-BESKZ'  it_ammc-prty,  "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky,  "SchedMargin key
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,    "Availability check
     ' ' 'MARC-STRGR'  l_strgr,         "Strategy group
*    2004.03.15 CHANGE
     ' ' 'MARC-VRMOD'  l_vrmod,         "Consumption mode
*******************************

     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'   it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'   it_ammc-inco,  "Individual/coll.
     ' ' 'MARC-SAUFT'   l_sauft,       "Repetitive Mfg Indicator
     ' ' 'MARC-SFEPR'   l_sfepr,       "REM profile
     ' ' 'MARC-PROFIL'  l_profil,      "Backfl.Profile
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting

* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  l_fevor,   " Production scheduler
*     ' ' 'MARC-UNETO'  L_UNETO,   "Underdely tol.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.

* Warehouse Management 1

* Warehouse Management 2

* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  l_bklas,          "Valuation class
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.

ENDFORM.                    " FERT_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM halb_selection_view.
  DATA: l_prodh LIKE t179t-prodh,    "Product hierarchy
        l_rgekz LIKE marc-rgekz,     "Backflush
        l_fevor TYPE marc-fevor,     "Production scheduler
*---start wskim 03/15/2005
*        l_uneto TYPE marc-uneto,     "Underdely tol.
        l_uneto(5) TYPE C,           "Underdely tol.
*---end
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(06),    "Price unitPrice unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1).                  "Costing lot size

* 2004.03.25 CHANGE
  l_rgekz = '1'. "Backflush
  l_fevor   = '001'  .  "Production scheduler
  l_uneto   = '10.0' .  "Underdely tol
  l_bklas   = '7900' .  "Valuation class
  l_peinh_1 = '1'    .  "Price unit
  l_ekalr   = 'X'    .  "with qty structure
  l_losgr   = '1'    .  "Costing lot size
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-prty = 'X'.     "Procurement type
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-slmd = '2'.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.
*******************
* START BDC
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt, "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1

* 2004.03.25 CHANGE
*  PERFORM READ_T179T USING    IT_AMMC-MTYP
*                              IT_AMMC-MTNO+6(2)
*                     CHANGING L_PRODH.
  l_prodh = '00002'.   "Product hierarchy
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'MARA-PRDHA'      l_prodh,        "Prod.hierarchy
     ' ' 'MAKT-MAKTX'      it_ammc-zdesc,  "Description
     ' ' 'MARA-MEINS'      it_ammc-unit,   "Unit
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,   "GenItemCatGroup
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PROFL'  it_ammc-sour,   "LP/KD/MIP
     ' ' 'BDC_OKCODE'  '=SP12'.
** Classification

* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,   "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,   "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty, "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky, "SchedMargin key
     ' ' 'MARC-RGEKZ'  l_rgekz,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'  it_ammc-inco,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  l_fevor,   "Production scheduler
     ' ' 'MARC-UNETO'  l_uneto,   "Underdely tol.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'                '5000',
    ' ' 'MBEW-BKLAS'               l_bklas,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'              '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM halb_phan_selection_view.
  DATA: l_prodh LIKE t179t-prodh,      "Product hierarchy
        l_rgekz LIKE marc-rgekz,     "Backflush
        l_fevor TYPE marc-fevor,     "Production scheduler
        l_uneto TYPE marc-uneto,     "Underdely tol.
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(06),    "Price unitPrice unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1).                  "Costing lot size

* 2004.03.25 CHANGE
  l_rgekz = '1'. "Backflush
  l_fevor   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  l_bklas   = '7900' .  "Valuation class
  l_peinh_1 = '1'    .  "Price unit
  l_ekalr   = 'X'    .  "with qty structure
  l_losgr   = '1'    .  "Costing lot size
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-prty = 'X'.     "Procurement type
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-slmd = '2'.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.
*******************
* START BDC
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt, "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1

* 2004.03.25 CHANGE
*  PERFORM READ_T179T USING    IT_AMMC-MTYP
*                              IT_AMMC-MTNO+6(2)
*                     CHANGING L_PRODH.
  l_prodh = '00002'.   "Product hierarchy
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'        '5004',
     ' ' 'MARA-PRDHA'      l_prodh,        "Prod.hierarchy
     ' ' 'MAKT-MAKTX'      it_ammc-zdesc,  "Description
     ' ' 'MARA-MEINS'      it_ammc-unit,   "Unit
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,   "GenItemCatGroup
     ' ' 'BDC_OKCODE'      '=SP02'.
* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PROFL'  it_ammc-sour,   "LP/KD/MIP
     ' ' 'BDC_OKCODE'  '=SP12'.
** Classification

* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  'ND',           "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,   "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  'X',          "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky, "SchedMargin key
     ' ' 'MARC-RGEKZ'  l_rgekz,      "Backflush
     ' ' 'MARC-SOBSL'  '50',         "Special procurement
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'  it_ammc-inco,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* Work Scheduling

* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
    ' ' 'MBEW-BKLAS'  l_bklas,          "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'MARC-NCOST'  'X',     "No costing
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_PHAN_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
FORM roh_selection_view.
  DATA: l_matkl TYPE mara-matkl,
        l_mstae LIKE mara-mstae,     "X-plant matl status
        l_rgekz LIKE marc-rgekz,     "Backflush
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(5),                "Price unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1).                  "Costing lot size

*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI
*  CASE it_ammc-sour.
*    WHEN 'K'.
*      l_bklas = '3000'.  "Valuation class
*      it_ammc-lots = 'WB'.    "Lot size(Default : EX)
*    WHEN 'V'.
*      l_bklas = '3001'.  "Valuation class
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*    WHEN OTHERS.
*      l_bklas = '3000'.  "Valuation class
*      CLEAR: it_ammc-sour.
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  ENDCASE.

  CLEAR: it_ammc-sour.
  l_bklas      = '3000'.
  it_ammc-lots = 'EX'.

  l_matkl      = 'INIT'.  "Material Group
  l_mstae      = '11'.    "X-plant matl status
  l_mstae      = '11'.    "X-plant matl status
  l_rgekz      = '1'.     "Backflush
  l_peinh_1    = '1'.     "Price unit
  l_ekalr      = 'X'.     "with qty structure
  l_losgr      = '1'.     "Costing lot size
  it_ammc-unit = 'EA'.    "Unit of measure
  it_ammc-gica = ' '.     "GenItemCatGroup
  it_ammc-mrpy = 'PD'.    "MRP type
  it_ammc-prty = 'F'.     "Procurement type
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = 'C01'.   "MRP controller
** end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-slmd = ' '.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.

* 2004.03.25 CHANGE
*  l_mstae = '11'      .  "X-plant matl status
*  l_rgekz = '1'      .  "Backflush
*  l_peinh_1 = '1'    .  "Price unit
*  clear l_matkl.
*  perform read_t023t changing l_matkl.  "Material  Group
*
*  clear l_bklas.
*  case it_ammc-sour.
*    when 'K'.
*      l_bklas = '3000'.  "Valuation class
*    when others.
*      l_bklas = '3001'.  "Valuation class
*  endcase.
*
*  l_ekalr   = 'X'    .  "with qty structure
*  l_losgr   = '1'    .  "Costing lot size

*  it_ammc-gica = ' '.     "GenItemCatGroup
*  it_ammc-mrpc = 'C01'.   "MRP controller
*  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  it_ammc-smky = '000'.   "SchedMargin key
*  it_ammc-avck = 'KP'.    "Availability check
*  it_ammc-slmd = ' '.     "Selection method
*  it_ammc-inco = '2'.     "Individual/coll.
*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,  "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Basic Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MAKT-MAKTX' it_ammc-zdesc,  "Description
     ' ' 'MARA-MEINS' it_ammc-unit,   "Unit
     ' ' 'MARA-MATKL' l_matkl,        "Material  Group
     ' ' 'MARA-MSTAE' l_mstae,        "X-plant matl status
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica, "GenItemCatGroup
     ' ' 'BDC_OKCODE'  '=SP02'.
* Basic Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5004',
     ' ' 'MARA-PROFL' it_ammc-sour,  "MIP/LP/KD
     ' ' 'BDC_OKCODE'  '=SP09'.
** Classification

* Purchasing View
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MMSTA'  l_mstae,
     ' ' 'MARC-KORDB'  'X',
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,  "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,  "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty, "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky, "SchedMargin key
     ' ' 'MARC-RGEKZ'  l_rgekz,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'  it_ammc-inco,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'              l_bklas,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " ROH_SELECTION_VIEW
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM rkc_msg_string CHANGING p_msg.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = p_msg
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  BDC_EXECUTION
*&---------------------------------------------------------------------*
FORM bdc_execution.
  DATA: l_tabix TYPE sy-tabix,
        l_mtart TYPE mara-mtart,
        l_messg LIKE cfgnl-msglin.

  LOOP AT it_ammc.
    l_tabix = sy-tabix.
    SELECT SINGLE mtart
            FROM mara
            INTO l_mtart
            WHERE matnr EQ it_ammc-mtno.
    IF sy-subrc EQ 0.
      SELECT SINGLE *
                  FROM marc
                  WHERE matnr EQ it_ammc-mtno
                  AND   werks EQ it_ammc-plnt.
      IF sy-subrc EQ 0.
*       Material already maintained for this transaction/event
        PERFORM error_log_modify USING l_tabix.
      ELSE.
*   Create material that there does not exist to 'MARC' being to 'MARA'.
        IF l_mtart EQ it_ammc-mtyp.
          PERFORM marc_does_not_exist_material.
          PERFORM call_transaction_mm01 USING l_tabix.
        ELSE.
          PERFORM error_log_mat_type USING l_tabix.
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM mara_does_not_exist_material.
      PERFORM call_transaction_mm01 USING l_tabix.
    ENDIF.
    REFRESH: it_bdc, it_mess.
    CLEAR: it_ammc.
  ENDLOOP.
ENDFORM.                    " BDC_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  MM02_BDC_EXECTION
*&---------------------------------------------------------------------*
FORM mm02_bdc_exection.
  DATA: l_messg LIKE cfgnl-msglin,
        l_tabix LIKE sy-tabix.
  LOOP AT it_ammc WHERE zresult NE 'E'
                  AND   mtcn    EQ 'X'.
    l_tabix = sy-tabix.
    PERFORM dynpro USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR'  it_ammc-mtno,
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)'  'X',
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '5004',
       ' ' 'MARA-KZKFG'  it_ammc-mtcn,
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'   USING it_bdc
                     OPTIONS  FROM wa_opt
                     MESSAGES INTO it_mess.
    it_ammc-zresult = sy-msgty.
    PERFORM rkc_msg_string CHANGING l_messg.
    IF it_ammc-zresult EQ 'E'.
      it_ammc-zmsg = l_messg.
      MODIFY it_ammc INDEX l_tabix TRANSPORTING zresult zmsg.
    ENDIF.
    CLEAR: l_messg, it_ammc.
    REFRESH: it_bdc, it_mess.
  ENDLOOP.
ENDFORM.                    " MM02_BDC_EXECTION
*&---------------------------------------------------------------------*
*&      Form  READ_T179T
*&---------------------------------------------------------------------*
FORM read_t179t USING    p_mtart
                         p_carty
                CHANGING p_prodh.
  DATA: l_prodh TYPE t179t-prodh,
        l_vtext TYPE t179t-vtext.
  CASE p_mtart.
    WHEN 'FERT'.
      CONCATENATE p_mtart '%' INTO l_vtext.
      SELECT SINGLE prodh
                  FROM t179t
                  INTO l_prodh
                  WHERE vtext LIKE l_vtext.
      CLEAR l_vtext.

      CONCATENATE l_prodh '%' INTO l_prodh.
      CONCATENATE p_carty '%' INTO l_vtext.
      SELECT SINGLE prodh
           FROM t179t
           INTO p_prodh
           WHERE prodh LIKE l_prodh
           AND   vtext LIKE l_vtext.
    WHEN 'HALB'.
      CONCATENATE p_mtart '%' INTO l_vtext.
      SELECT SINGLE prodh
                  FROM t179t
                  INTO p_prodh
                  WHERE vtext LIKE l_vtext.
      CLEAR l_vtext.
  ENDCASE.
ENDFORM.                    " READ_T179T
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
FORM error_log_modify USING    p_tabix.
  it_ammc-zresult = 'E'.
  it_ammc-zbdat = sy-datum.
  it_ammc-zbnam = sy-uname.
  it_ammc-zmode = 'C'.
  it_ammc-zmsg =
  'Material already maintained for this transaction/event'.
* MODIFY IT_AMMC
  MODIFY it_ammc INDEX p_tabix TRANSPORTING zresult
                                            zbdat
                                            zbnam
                                            zmode
                                            zmsg.
ENDFORM.                    " ERROR_LOG_MODIFY
*&---------------------------------------------------------------------*
*&      Form  MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
FORM marc_does_not_exist_material.
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0060',
     ' ' 'RMMG1-MATNR' it_ammc-mtno,    "
     ' ' 'RMMG1-MBRSH' it_ammc-indu,    "
     ' ' 'RMMG1-MTART' it_ammc-mtyp,    "
     ' ' 'BDC_OKCODE'  '/00'.
* MATERIAL TYPE
  CASE it_ammc-mtyp.
    WHEN 'FERT'.
*     Industry Standard Description
      PERFORM fert_selection_view1.
    WHEN 'HALB'.
      CASE it_ammc-sour.
*       MIP
        WHEN 'M'.
*         SELECTION VIEW( HALB(MIP) : BDC TAB )
          PERFORM halb_selection_view1.
*       PHANTOM
        WHEN OTHERS.
*         SELECTION VIEW( HALB(PHANTOM) : BDC TAB )
          PERFORM halb_phan_selection_view1.
      ENDCASE.

    WHEN 'ROH'.
*     SELECTION VIEW( ROH : BDC TAB )
      PERFORM roh_selection_view1.
  ENDCASE.
ENDFORM.                    " MARC_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM fert_selection_view1.
  DATA: l_vkorg TYPE rmmg1-vkorg,
        l_prodh TYPE t179t-prodh,  "Product hierarchy
** Added By Tonkey on 05/21/2004.
        l_natn_c     TYPE ztbm_abxtrtdt-natn_c,
        l_delr_c     TYPE ztbm_abxtrtdt-delr_c,
        l_blnk_cloc4 TYPE ztbm_abxtrtdt-blnk_cloc4,
**
*    2004.03.15 CHANGE
        l_lgort TYPE rmmg1-lgort,
        l_vtweg TYPE rmmg1-vtweg,
        l_dwerk TYPE mvke-dwerk,
        l_ktgrm TYPE mvke-ktgrm,
        l_tragr TYPE mara-tragr,
        l_ladgr TYPE marc-ladgr,
        l_disgr TYPE marc-disgr,
        l_strgr TYPE marc-strgr,
        l_vrmod TYPE marc-vrmod,
        l_sauft TYPE marc-sauft,       "Repetitive Mfg Indicator
        l_sfepr TYPE marc-sfepr,       "REM profile
        l_profil TYPE marc-profil,     "Backfl.Profile
        l_fevor TYPE marc-fevor,     "Production scheduler
        l_uneto TYPE marc-uneto,     "Underdely tol.
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(06),    "Price unitPrice unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1).                  "Costing lot size
************************
** Changed By Tonkey on 05/21/2004.
  MOVE: it_ammc-mtno+01(03) TO l_natn_c,
        it_ammc-mtno+04(02) TO l_delr_c.
*
  SELECT SINGLE blnk_cloc4
    INTO l_blnk_cloc4
    FROM ztbm_abxtrtdt
    WHERE natn_c = l_natn_c AND  "Nation
          delr_c = l_delr_c   .  "Dealer
*
  CASE l_blnk_cloc4.
    WHEN 'D'.
      l_vkorg      = 'D100'.
      it_ammc-gica = '0002'.
    WHEN 'E'.
      l_vkorg      = 'E100'.
      it_ammc-gica = 'Z002'.
  ENDCASE.
*  case it_ammc-mtno+1(3).
*    when 'B28'.
*      l_vkorg = 'D100'.  "Sales organization
*    when 'B06'.
*      l_vkorg = 'E100'.  "Sales organization
*  endcase.
**

*    2004.03.15 CHANGE
  PERFORM read_t179t USING    it_ammc-mtyp
                              it_ammc-mtno+6(2)
                     CHANGING l_prodh.

  l_lgort   = 'F001' .  "Storage location
  l_vtweg   = '10'   .  "Distribution level
  l_dwerk   = 'P001' .  "Delivering plant
  l_ktgrm   = '10'   .  "Acct assignment grp
  l_tragr   = '0001' .  "Trans. grp
  l_ladgr   = 'P100' .  "LoadingGrp
  l_disgr   = '0001' .  "MRP group
  l_strgr   = '56'   .  "Strategy group
  l_vrmod   = '1'    .  "Consumption mode
  l_sauft   = 'X'    .  "Repetitive Mfg
  l_sfepr   = 'VEHI' .  "REM profile
  l_profil  = 'SAP2' .  "Backfl.Profile
  l_fevor   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  l_bklas   = '7920' .  "Valuation class
  l_peinh_1 = '1'    .  "Price unit
  l_ekalr   = 'X'    .  "with qty structure
  l_losgr   = '1'    .  "Costing lot size
*
** Inactivated By Tonkey on 05/21/2004.
*  it_ammc-gica = '0002'.  "Item category group
**
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-mrpy = 'PD'.    "MRP type
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = 'V01'.   "MRP controller
** end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-prty = 'E'.     "Procurement type
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-slmd = '2'.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.
**************************

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'BDC_OKCODE'  '=SELA',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(02)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(03)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' ' ',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' ' ',   "
     ' ' 'BDC_OKCODE'  '/00'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,  "PLANT
*    2004.03.15 CHANGE
     ' ' 'RMMG1-LGORT' l_lgort,       "Storage location
     ' ' 'RMMG1-VTWEG' l_vtweg,       "Distribution level
*******************************
     ' ' 'RMMG1-VKORG' l_vkorg,       "Sales organization
     ' ' 'BDC_OKCODE'  '=ENTR'.
* Sales: Sales Org. Data 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' ' ',                 "Division
*    2004.03.15 CHANGE
     ' ' 'MVKE-DWERK' l_dwerk,              "Delivering plant
*******************************
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(02)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(03)' '0',       "Tax classification
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MVKE-KTGRM'  l_ktgrm,   "Acct assignment grp
*******************************
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,         "Gen. item cat. grp
     ' ' 'MVKE-PRODH'      l_prodh,              "Product hierarchy
     ' ' 'MVKE-MVGR3'      it_ammc-mtno+12(01),  "Material group 3
     ' ' 'MVKE-MVGR4'      it_ammc-mtno+10(01),  "Material group 4
     ' ' 'MVKE-MVGR5'      it_ammc-mtno+09(01),  "Material group 5
     ' ' 'MVKE-MTPOS'      it_ammc-gica,         "Item category group
     ' ' 'BDC_OKCODE'      '=SP06'.
* Sales: General/Plant Data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARA-SPART' ' ',                 "Division
*    2004.03.15 CHANGE
     ' ' 'MVKE-DWERK' l_dwerk,              "Delivering plant
*******************************
     ' ' 'BDC_OKCODE'  '=SP05',

     'X' 'SAPLMGMM'    '4200',
     ' ' 'MG03STEUER-TAXKM(01)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(02)' '0',       "Tax classification
     ' ' 'MG03STEUER-TAXKM(03)' '0',       "Tax classification
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Sales: Sales Org. Data 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MVKE-KTGRM'  l_ktgrm,   "Acct assignment grp
*******************************
     ' ' 'MARA-MTPOS_MARA' it_ammc-gica,         "Gen. item cat. grp
     ' ' 'MVKE-PRODH'      l_prodh,              "Product hierarchy
     ' ' 'MVKE-MVGR3'      it_ammc-mtno+12(01),  "Material group 3
     ' ' 'MVKE-MVGR4'      it_ammc-mtno+10(01),  "Material group 4
     ' ' 'MVKE-MVGR5'      it_ammc-mtno+09(01),  "Material group 5
     ' ' 'MVKE-MTPOS'      it_ammc-gica,         "Item category group
     ' ' 'BDC_OKCODE'      '=SP06'.
* Sales: General/Plant Data
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*    2004.03.15 CHANGE
     ' ' 'MARC-MTVFP'  it_ammc-avck,   "Availability check
*******************************
     ' ' 'MARA-TRAGR'  l_tragr,  "Trans. grp
*    2004.03.15 CHANGE
     ' ' 'MARC-LADGR'  l_ladgr,  "LoadingGrp
*******************************
     ' ' 'BDC_OKCODE'  '=SP12'.

* Foreign Trade: Export Data

* Sales Text

* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISGR'  l_disgr,       "MRP group
     ' ' 'MARC-DISMM'  it_ammc-mrpy,  "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,  "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-LGPRO'  l_lgort,        "Issue stor. location
     ' ' 'MARC-BESKZ'  it_ammc-prty,  "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky,  "SchedMargin key
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,    "Availability check
     ' ' 'MARC-STRGR'  l_strgr,         "Strategy group
*    2004.03.15 CHANGE
     ' ' 'MARC-VRMOD'  l_vrmod,         "Consumption mode
*******************************

     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'   it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'   it_ammc-inco,  "Individual/coll.
     ' ' 'MARC-SAUFT'   l_sauft,       "Repetitive Mfg Indicator
     ' ' 'MARC-SFEPR'   l_sfepr,       "REM profile
     ' ' 'MARC-PROFIL'  l_profil,      "Backfl.Profile
     ' ' 'BDC_OKCODE'  '=SP17'.
* Forecasting

* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  l_fevor,   " Production scheduler
     ' ' 'MARC-UNETO'  l_uneto,   "Underdely tol.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.

* Warehouse Management 1

* Warehouse Management 2

* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'  l_bklas,          "Valuation class
*     ' ' 'CKMMAT_DISPLAY-VPRSV_1'  'S',
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.


ENDFORM.                    " FERT_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM halb_selection_view1.
  DATA: l_prodh LIKE t179t-prodh,      "Product hierarchy
        l_rgekz LIKE marc-rgekz,     "Backflush
        l_fevor TYPE marc-fevor,     "Production scheduler
        l_uneto TYPE marc-uneto,     "Underdely tol.
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(06),    "Price unitPrice unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1).                  "Costing lot size

* 2004.03.25 CHANGE
  l_rgekz = '1'. "Backflush
  l_fevor   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  l_bklas   = '7900' .  "Valuation class
  l_peinh_1 = '1'    .  "Price unit
  l_ekalr   = 'X'    .  "with qty structure
  l_losgr   = '1'    .  "Costing lot size
  l_prodh   = '00002'.  "Product hierarchy
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-prty = 'X'.     "Procurement type
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-slmd = '2'.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.
*******************
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,
     ' ' 'BDC_OKCODE'  '=ENTR'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,   "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,   "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  it_ammc-prty, "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky, "SchedMargin key
     ' ' 'MARC-RGEKZ'  l_rgekz,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'  it_ammc-inco,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP17'.
* Work Scheduling
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-FEVOR'  l_fevor,   "Production scheduler
     ' ' 'MARC-UNETO'  l_uneto,   "Underdely tol.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'                '5000',
    ' ' 'MBEW-BKLAS'               l_bklas,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'              '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM halb_phan_selection_view1.
  DATA: l_prodh LIKE t179t-prodh,      "Product hierarchy
        l_rgekz LIKE marc-rgekz,     "Backflush
        l_fevor TYPE marc-fevor,     "Production scheduler
        l_uneto TYPE marc-uneto,     "Underdely tol.
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(06),    "Price unitPrice unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1).                  "Costing lot size

* 2004.03.25 CHANGE
  l_rgekz = '1'. "Backflush
  l_fevor   = '001'  .  "Production scheduler
*  L_UNETO   = '10.0' .  "Underdely tol
  l_bklas   = '7900' .  "Valuation class
  l_peinh_1 = '1'    .  "Price unit
  l_ekalr   = 'X'    .  "with qty structure
  l_losgr   = '1'    .  "Costing lot size
  l_prodh   = '00002'.  "Product hierarchy
** changed by Furong on 12/19/2005
*  it_ammc-mrpc = '001'.   "MRP controller
** end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-prty = 'X'.     "Procurement type
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-slmd = '2'.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.
*******************

*******************
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(08)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.
* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt, "PLANT
     ' ' 'BDC_OKCODE'  '=ENTR'.

* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  'ND',           "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,   "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,   "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-BESKZ'  'X',          "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky, "SchedMargin key
     ' ' 'MARC-RGEKZ'  l_rgekz,      "Backflush
     ' ' 'MARC-SOBSL'  '50',         "Special procurement
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'  it_ammc-inco,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* Work Scheduling

* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
    ' ' 'MBEW-BKLAS'  l_bklas,          "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.

* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'MARC-NCOST'  'X',     "No costing
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " HALB_PHAN_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
FORM roh_selection_view1.
  DATA: l_matkl TYPE mara-matkl,
        l_mstae LIKE mara-mstae,     "X-plant matl status
        l_rgekz LIKE marc-rgekz,     "Backflush
        l_bklas TYPE mbew-bklas,     "Valuation class
        l_peinh_1(5),                "Price unit
        l_ekalr TYPE mbew-ekalr,     "with qty structure
        l_losgr(1).                  "Costing lot size

*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI
*  CASE it_ammc-sour.
*    WHEN 'K'.
*      l_bklas = '3000'.  "Valuation class
*      it_ammc-lots = 'WB'.    "Lot size(Default : EX)
*    WHEN 'V'.
*      l_bklas = '3001'.  "Valuation class
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*    WHEN OTHERS.
*      l_bklas = '3000'.  "Valuation class
*      CLEAR: it_ammc-sour.
*      it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  ENDCASE.

  CLEAR: it_ammc-sour.
  l_bklas = '3000'.
  it_ammc-lots = 'EX'.

  l_matkl      = 'INIT'.  "Material Group
  l_mstae      = '11'.    "X-plant matl status
  l_rgekz      = '1'.     "Backflush
  l_peinh_1    = '1'.     "Price unit
  l_ekalr      = 'X'.     "with qty structure
  l_losgr      = '1'.     "Costing lot size
  it_ammc-unit = 'EA'.    "Unit of measure
  it_ammc-gica = ' '.     "GenItemCatGroup
  it_ammc-mrpy = 'PD'.    "MRP type
  it_ammc-prty = 'F'.     "Procurement type
** change by Furong on 12/19/2005
*  it_ammc-mrpc = 'C01'.   "MRP controller
** end of change
  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
  it_ammc-smky = '000'.   "SchedMargin key
  it_ammc-avck = 'KP'.    "Availability check
  it_ammc-slmd = ' '.     "Selection method
  it_ammc-inco = '2'.     "Individual/coll.

** 2004.03.25 CHANGE
*  l_rgekz = '1'      .  "Backflush
*  l_peinh_1 = '1'    .  "Price unit
*  CLEAR l_matkl.
*  PERFORM read_t023t CHANGING l_matkl.  "Material  Group
*
*  CLEAR l_bklas.
*  CASE it_ammc-sour.
*    WHEN 'K'.
*      l_bklas = '3000'.  "Valuation class
*    WHEN OTHERS.
*      l_bklas = '3001'.  "Valuation class
*  ENDCASE.
*
*  l_ekalr   = 'X'    .  "with qty structure
*  l_losgr   = '1'    .  "Costing lot size
*
*  it_ammc-gica = ' '.     "GenItemCatGroup
*  it_ammc-mrpc = 'C01'.   "MRP controller
*  it_ammc-lots = 'EX'.    "Lot size(Default : EX)
*  it_ammc-smky = '000'.   "SchedMargin key
*  it_ammc-avck = 'KP'.    "Availability check
*  it_ammc-slmd = ' '.     "Selection method
*  it_ammc-inco = '2'.     "Individual/coll.
*----- Issue PP-20040812-001 Changed by BSBAE, Changed on 2004.09.21
*----- Requested by IYCHOI

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(09)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(13)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(14)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(15)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(17)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(18)' 'X',   "
     ' ' 'BDC_OKCODE'  '=P+',
     'X' 'SAPLMGMM'    '0070',
     ' ' 'MSICHTAUSW-KZSEL(03)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(04)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(05)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(06)' 'X',   "
     ' ' 'MSICHTAUSW-KZSEL(07)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR'.

* ORGANIZATIONAL LEVELS
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '0080',
     ' ' 'RMMG1-WERKS' it_ammc-plnt,  "Plant
     ' ' 'BDC_OKCODE'  '=ENTR'.

* Purchasing View
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MMSTA'  l_mstae,
     ' ' 'MARC-KORDB'  'X',
     ' ' 'BDC_OKCODE'  '=SP12'.
* MRP 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-DISMM'  it_ammc-mrpy,  "MRP type
     ' ' 'MARC-DISPO'  it_ammc-mrpc,  "MRP controller
     ' ' 'MARC-DISLS'  it_ammc-lots,  "Lot size(Default : EX)
     ' ' 'BDC_OKCODE'  '=SP13'.
* MRP 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
*     ' ' 'MARC-BESKZ'  IT_AMMC-PRTY, "Procurement type
     ' ' 'MARC-FHORI'  it_ammc-smky, "SchedMargin key
     ' ' 'MARC-RGEKZ'  l_rgekz,      "Backflush
     ' ' 'BDC_OKCODE'  '=SP14'.
* MRP 3
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MARC-MTVFP'  it_ammc-avck,  "Availability check
     ' ' 'BDC_OKCODE'  '=SP15'.
* MRP 4
  PERFORM dynpro USING:
    'X' 'SAPLMGMM'    '5004',
     ' ' 'MARC-ALTSL'  it_ammc-slmd,  "Selection method
     ' ' 'MARC-SBDKZ'  it_ammc-inco,  "Individual/coll.
     ' ' 'BDC_OKCODE'  '=SP19'.
* General Plant Data / Storage 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP20'.
* General Plant Data / Storage 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP23'.
* Quality Management
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP24'.
* Accounting 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-BKLAS'              l_bklas,    "Valuation class
     ' ' 'CKMMAT_DISPLAY-PEINH_1'  l_peinh_1,  "Price unit
     ' ' 'BDC_OKCODE'  '=SP25',

     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '/00'.
* Accounting 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=SP26'.
* Costing 1
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'MBEW-EKALR'  l_ekalr, "with qty structure
     ' ' 'MARC-LOSGR'  l_losgr, "Costing lot size
     ' ' 'BDC_OKCODE'  '=SP27'.
* Costing 2
  PERFORM dynpro USING:
     'X' 'SAPLMGMM'    '5000',
     ' ' 'BDC_OKCODE'  '=BU'.
ENDFORM.                    " ROH_SELECTION_VIEW1
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
FORM call_transaction_mm01 USING    p_tabix.
  DATA l_messg LIKE cfgnl-msglin.

  CALL TRANSACTION 'MM01'  USING it_bdc
                  OPTIONS FROM wa_opt
                  MESSAGES INTO it_mess.
  it_ammc-zresult = sy-msgty.

  PERFORM rkc_msg_string CHANGING l_messg.

  it_ammc-zmsg = l_messg.
  PERFORM zsbm_if_time_change USING     it_ammc-zresult
                                        it_ammc-zedat
                                        it_ammc-zetim
                               CHANGING it_ammc-zbdat
                                        it_ammc-zbtim
                                        it_ammc-zbnam
                                        it_ammc-zmode.
*   MODIFY IT_AMMC
  MODIFY it_ammc INDEX p_tabix TRANSPORTING zbdat
                                            zbtim
                                            zbnam
                                            zmode
                                            zresult
                                            zmsg.
  REFRESH: it_bdc, it_mess.
  CLEAR: it_ammc.
ENDFORM.                    " CALL_TRANSACTION_MM01
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
FORM error_log_mat_type USING    p_tabix.
  it_ammc-zresult = 'E'.
  it_ammc-zbdat = sy-datum.
  it_ammc-zbnam = sy-uname.
  it_ammc-zmode = 'C'.
  it_ammc-zmsg = 'MATERIAL TYPE is Mismatch'.
* MODIFY IT_AMMC
  MODIFY it_ammc INDEX p_tabix TRANSPORTING zresult
                                            zbdat
                                            zbnam
                                            zmode
                                            zmsg.
ENDFORM.                    " ERROR_LOG_MAT_TYPE
*&---------------------------------------------------------------------*
*&      Form  MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
FORM mara_does_not_exist_material.
  PERFORM dynpro USING:
      'X' 'SAPLMGMM'    '0060',
      ' ' 'RMMG1-MATNR' it_ammc-mtno,   "
      ' ' 'RMMG1-MBRSH' it_ammc-indu,   "
      ' ' 'RMMG1-MTART' it_ammc-mtyp,   "
      ' ' 'BDC_OKCODE'  '/00'.
*   MATERIAL TYPE
  CASE it_ammc-mtyp.
    WHEN 'FERT'.
*     Industry Standard Description
      PERFORM fert_selection_view.
    WHEN 'HALB'.
      CASE it_ammc-sour.
*       MIP
        WHEN 'M'.
          PERFORM halb_selection_view.
*       PHANTOM
        WHEN OTHERS.
*         SELECTION VIEW( HALB(PHANTOM) : BDC TAB )
          PERFORM halb_phan_selection_view.
      ENDCASE.

    WHEN 'ROH'.
      PERFORM roh_selection_view.
  ENDCASE.
ENDFORM.                    " MARA_DOES_NOT_EXIST_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  READ_T023T
*&---------------------------------------------------------------------*
FORM read_t023t CHANGING p_matkl.
  SELECT SINGLE matkl
              FROM t023t
              INTO p_matkl.
ENDFORM.                    " READ_T023T
