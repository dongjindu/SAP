************************************************************************
* Program Name      : ZMMIFTZC_ENGINE
* Author            : Furong Wang
* Creation Date     : 06/24/09
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Engine
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zmmiftzc_engine.
INCLUDE zmmfztop.
INCLUDE zmmcontp.
TABLES: ztpperm.
DATA: l_inbdt LIKE equi-inbdt,
      l_equnr LIKE equi-equnr.
DATA : wa_mast TYPE mast.

PARAMETERS: p_date   LIKE sy-datum OBLIGATORY,
            p_rp LIKE ztpperm-erpid OBLIGATORY,
*            P_WERKS LIKE ZTPPERM-PLANT_CD DEFAULT 'E001' OBLIGATORY,
            p_capid LIKE tc04-capid DEFAULT 'PP01' OBLIGATORY,
            p_srvgrp LIKE rzllitab-classname OBLIGATORY
                     DEFAULT 'PG_FTZ'.
PARAMETERS: p_time TYPE i DEFAULT 10.
** only for test
SELECT-OPTIONS :  s_werks FOR ztpperm-plant_cd DEFAULT 'E001' OBLIGATORY,
             s_matnr FOR it_ztpperm-eitem,
             s_eassy FOR it_ztpperm-eassyid             .

CONCATENATE p_date+4(2) '/' p_date+6(2) '/' p_date(4) INTO w_key_date
.

SELECT matnr mtart meins FROM mara
                         INTO TABLE it_mat_type
                        WHERE ( mtart = 'HALB' AND lvorm = space )
                        OR    ( mtart = 'ROH'  AND lvorm = space )
                        OR    ( mtart = 'FERT' AND lvorm = space ).

SELECT * INTO TABLE it_ztpperm
  FROM ztpperm
  WHERE erpid = p_rp
    AND rdate = p_date
    AND eitem IN s_matnr
** Changed on 10/12/12
    AND eassyid IN s_eassy.
** End

IF it_ztpperm[] IS INITIAL.
  MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH text-001.
  EXIT.
ENDIF.

** Furong on 09/07/12 for delete dupliacte Engine
SORT it_ztpperm BY eassyid.
DELETE ADJACENT DUPLICATES FROM it_ztpperm COMPARING eassyid.
** End

LOOP AT it_ztpperm.
  wa_halb-matnr = it_ztpperm-eitem.
  wa_halb-pmatnr = it_ztpperm-eitem.

** Changed on 02/01/12
  wa_halb-werks = it_ztpperm-en_reserve_02.
*  WA_HALB-WERKS = P_WERKS.
** end on 02/01/12

  wa_halb-menge = it_ztpperm-eqty.
*  WA_HALB-STLAL = WA_FSC_INFO-VERSN+1(2).
** On 07/17/13 Furong
*  l_equnr = it_ztpperm-eassyid.
*  SELECT SINGLE inbdt INTO l_inbdt
*    FROM equi
*    WHERE equnr = l_equnr.
*  IF sy-subrc = 0.
*    wa_halb-datuv = l_inbdt.
*  ELSE.
*
*  ENDIF.
  wa_halb-datuv = it_ztpperm-rdate.
** En don 07/17/13
  APPEND  wa_halb TO it_halb.
ENDLOOP.

*LOOP AT IT_HALB INTO WA_HALB.
**  WA_PARENT_MAT-PLNUM = WA_HALB-PLNUM.
**  WA_PARENT_MAT-VBELN = WA_HALB-VBELN.
*  WA_PARENT_MAT-MATNR = WA_HALB-MATNR.
*  WA_PARENT_MAT-PLANT = WA_HALB-WERKS.
*  WA_PARENT_MAT-MENGE = WA_HALB-MENGE.
**    WA_PARENT_MAT-DATUV = WA_HALB-DATUV.
*  READ TABLE IT_MAT_TYPE INTO WA_MAT_TYPE
*                         WITH TABLE KEY MATNR = WA_HALB-MATNR
*                                        MTART = 'FERT'
*                                       TRANSPORTING MEINS.
*  IF SY-SUBRC NE 0.
*    WA_PARENT_MAT-MEINS = SPACE.
*  ELSE.
*    WA_PARENT_MAT-MEINS = WA_MAT_TYPE-MEINS.
*  ENDIF.
*  COLLECT WA_PARENT_MAT INTO IT_PARENT_MAT.
*ENDLOOP.
CLEAR: wa_parent_mat, wa_halb,wa_mat_type.

LOOP AT it_halb INTO wa_halb.
  wa_halb_exp-matnr = wa_halb-matnr.
  wa_halb_exp-werks = wa_halb-werks.
  wa_halb_exp-stlal = wa_halb-stlal.
  wa_halb_exp-datuv = wa_halb-datuv.
  COLLECT wa_halb_exp INTO it_halb_exp.
ENDLOOP.

CLEAR wa_halb_exp.
wa_halb_exp-menge = 1.
MODIFY it_halb_exp FROM wa_halb_exp TRANSPORTING menge
                         WHERE menge IS INITIAL.

LOOP AT it_halb_exp INTO wa_halb_exp.
  SELECT  * INTO wa_mast
  FROM mast
    UP TO 1 ROWS
  WHERE matnr = wa_halb_exp-matnr
    AND stlan = '1'
  ORDER BY  andat DESCENDING.
  ENDSELECT.
  IF sy-subrc = 0.
    IF wa_halb_exp-werks <> wa_mast-werks.
      wa_halb_exp-werks = wa_mast-werks.
      MODIFY it_halb_exp FROM wa_halb_exp TRANSPORTING werks
                               WHERE matnr  = wa_halb_exp-matnr.
    ENDIF.
  ENDIF.
ENDLOOP.

* Explode BOM
PERFORM get_bom_info.

DATA: lw_likp LIKE likp.

CLEAR wa_planord_info.

LOOP AT it_halb INTO wa_halb.
  wa_planord_info-plnum  = wa_halb-datuv.
*  WA_PLANORD_INFO-VBELN  = WA_PARENT_MAT-VBELN.
  wa_planord_info-pmatnr = wa_halb-matnr.
  wa_planord_info-cmatnr = wa_halb-matnr.
  wa_planord_info-plant  = wa_halb-werks.
  wa_planord_info-menge  = wa_halb-menge.
*  READ TABLE IT_MAT_TYPE INTO WA_MAT_TYPE
*                       WITH TABLE KEY MATNR = WA_HALB-MATNR
*                                      MTART = 'FERT'
*                                     TRANSPORTING MEINS.
*  IF SY-SUBRC NE 0.
*    WA_PLANORD_INFO-MEINS = SPACE.
*  ELSE.
*    WA_PLANORD_INFO-MEINS = WA_MAT_TYPE-MEINS.
*  ENDIF.

  SELECT SINGLE meins INTO wa_planord_info-meins
     FROM mara
      WHERE matnr =  wa_halb-matnr.

  APPEND wa_planord_info TO it_planord_info.

*Sales order into to get country Shipped to.
  wa_vbak-vbeln = wa_parent_mat-vbeln.
  COLLECT wa_vbak INTO it_vbak.
  CLEAR: wa_planord_info.
ENDLOOP.

PERFORM get_material_info.
PERFORM get_salepart_ccode USING 'C'.

SORT it_planord_info BY pmatnr cmatnr plant.
LOOP AT it_planord_info ASSIGNING <fs_planord>.
  wa_ztmm_6026_01-partnerid = '100300'.
  w_date = sy-datum.
  w_time = sy-uzeit.
  CONCATENATE w_date 'T' w_time  INTO wa_ztmm_6026_01-effdate.
  wa_ztmm_6026_01-txncode = 'SPNM'.
  wa_ztmm_6026_01-ordernumreceipt = space.
  wa_ztmm_6026_01-transportid     = space.
  CONCATENATE <fs_planord>-pmatnr p_date <fs_planord>-plnum
                                  INTO wa_ztmm_6026_01-ordernumwork.

  CONCATENATE p_date 'T' '000000' INTO wa_ztmm_6026_01-txndate.

  wa_ztmm_6026_01-matnr = <fs_planord>-cmatnr.
  IF <fs_planord>-pmatnr EQ <fs_planord>-cmatnr.
    wa_ztmm_6026_01-ptc = 'NM'.
  ELSE.
    wa_ztmm_6026_01-ptc = 'PC'.
  ENDIF.
  wa_ztmm_6026_01-ptcsrc = c_erp_source.
  wa_ztmm_6026_01-maktxsrc = c_erp_source.
  wa_ztmm_6026_01-naftacertified = space.
  wa_ztmm_6026_01-naftacertifiedsc = c_ftzlink_source.
  wa_ztmm_6026_01-menge = <fs_planord>-menge.
  wa_ztmm_6026_01-qtyperlm = <fs_planord>-reqqty.
  wa_ztmm_6026_01-meinssrc = c_erp_source.
  wa_ztmm_6026_01-countryshipto = 'US'.
  CONCATENATE 'SHIPENG' p_date INTO wa_ztmm_6026_01-ordernumship.

  wa_ztmm_6026_01-adjproductnum = space.
  wa_ztmm_6026_01-modeoftransport = 'L'.
  wa_ztmm_6026_01-statuscode    = 'F'.
  wa_ztmm_6026_01-statuscodesrc = ''.
  wa_ztmm_6026_01-validflag        = 'N'.
  wa_ztmm_6026_01-assignmentflag   = 'N'.
  wa_ztmm_6026_01-fifoflag         = 'N'.
  wa_ztmm_6026_01-deletedflag      = 'N'.
  wa_ztmm_6026_01-keepduringrollba = 'N'.

*&----------FIFO HTS value
  READ TABLE it_mat_info1 INTO wa_mat_info1
                         WITH TABLE KEY matnr = <fs_planord>-cmatnr
                                        werks = <fs_planord>-plant.
  IF sy-subrc NE 0.
*    CONCATENATE <FS_PLANORD>-CMATNR '*' INTO W_C_MATNR.
*    LOOP AT IT_COLOR_PARTS INTO WA_COLOR_PARTS
*                            WHERE MATNR CP W_C_MATNR.
*      W_C_MATNR = WA_COLOR_PARTS-MATNR.
*      EXIT.
*    ENDLOOP.
*    READ TABLE IT_MAT_INFO1 INTO WA_MAT_INFO1
*                           WITH TABLE KEY MATNR = W_C_MATNR
*                                          WERKS = <FS_PLANORD>-PLANT.
*    IF SY-SUBRC NE 0.
    wa_ztmm_6026_01-stawn    = space.
    wa_ztmm_6026_01-stawnsrc = 'H'.
    wa_ztmm_6026_01-ntgew  = space.
    wa_ztmm_6026_01-ntgewsrc = c_ftzlink_source.
    wa_ztmm_6026_01-gewei = space.
    wa_ztmm_6026_01-geweisrc = c_ftzlink_source.
*    ELSE.
*      IF WA_MAT_INFO1-STAWN IS INITIAL.
*        WA_ZTMM_6026_01-STAWN    = SPACE.
*        WA_ZTMM_6026_01-STAWNSRC = 'H'.
*      ELSE.
*        WA_ZTMM_6026_01-STAWN    = WA_MAT_INFO1-STAWN.
*        WA_ZTMM_6026_01-STAWNSRC = C_ERP_SOURCE.
*      ENDIF.
*      WA_ZTMM_6026_01-MAKTX = WA_MAT_INFO1-MAKTX. "Material
*      WA_ZTMM_6026_01-MEINS = <FS_PLANORD>-MEINS. "Material
*      IF WA_MAT_INFO1-NTGEW IS INITIAL.
*        WA_ZTMM_6026_01-NTGEWSRC = C_FTZLINK_SOURCE.
*        WA_ZTMM_6026_01-GEWEISRC = C_FTZLINK_SOURCE.
*      ELSE.
*        WA_ZTMM_6026_01-NTGEW  = WA_MAT_INFO1-NTGEW. "Material
*        WA_ZTMM_6026_01-NTGEWSRC = C_ERP_SOURCE.
*        WA_ZTMM_6026_01-GEWEI = WA_MAT_INFO1-GEWEI. "Material
*        WA_ZTMM_6026_01-GEWEISRC = C_ERP_SOURCE.
*      ENDIF.
*    ENDIF.
  ELSE.
    IF wa_mat_info1-stawn IS INITIAL.
      wa_ztmm_6026_01-stawn    = space.
      wa_ztmm_6026_01-stawnsrc = 'H'.
    ELSE.
      wa_ztmm_6026_01-stawn    = wa_mat_info1-stawn.
      wa_ztmm_6026_01-stawnsrc = c_erp_source.
    ENDIF.
    wa_ztmm_6026_01-maktx = wa_mat_info1-maktx. "Material
    wa_ztmm_6026_01-meins = <fs_planord>-meins. "Material
    IF wa_mat_info1-ntgew IS INITIAL.
      wa_ztmm_6026_01-ntgewsrc = c_ftzlink_source.
      wa_ztmm_6026_01-geweisrc = c_ftzlink_source.
    ELSE.
      wa_ztmm_6026_01-ntgew  = wa_mat_info1-ntgew. "Material
      wa_ztmm_6026_01-ntgewsrc = c_erp_source.
      wa_ztmm_6026_01-gewei = wa_mat_info1-gewei. "Material
      wa_ztmm_6026_01-geweisrc = c_erp_source.
    ENDIF.
  ENDIF.
  wa_ztmm_6026_01-spicode1src = 'H'.
  wa_ztmm_6026_01-spicode2src = 'H'.
  wa_ztmm_6026_01-lifnrsrc = c_ftzlink_source.
  wa_ztmm_6026_01-relflagsrc = 'M'.
  wa_ztmm_6026_01-htsindexsrc = c_ftzlink_source.
  wa_ztmm_6026_01-htsdescsrc = 'H'.
  wa_ztmm_6026_01-htsnum2src = c_ftzlink_source.
  READ TABLE it_eina INTO wa_eina WITH KEY matnr = <fs_planord>-cmatnr
                                                       BINARY SEARCH.
  IF sy-subrc NE 0.
    wa_ztmm_6026_01-netpr = 0.
    wa_ztmm_6026_01-effpr = 0.
    wa_ztmm_6026_01-netpruom = 0.
    wa_ztmm_6026_01-effpruom = 0.
  ELSE.
    wa_ztmm_6026_01-netpr = wa_eina-kbetr.
    wa_ztmm_6026_01-effpr = wa_eina-effpr.
    IF NOT wa_eina-bpumn IS INITIAL.
      wa_ztmm_6026_01-netpruom = ( wa_ztmm_6026_01-netpr * wa_eina-bpumz ) /
                                                               wa_eina-bpumn.
      wa_ztmm_6026_01-effpruom = ( wa_ztmm_6026_01-effpr * wa_eina-bpumz ) /
                                                               wa_eina-bpumn.
      IF NOT wa_eina-peinh IS INITIAL.
        wa_ztmm_6026_01-netpruom = wa_ztmm_6026_01-netpruom / wa_eina-peinh.
        wa_ztmm_6026_01-effpruom = wa_ztmm_6026_01-effpruom / wa_eina-peinh.
      ENDIF.
    ENDIF.
  ENDIF.
  wa_ztmm_6026_01-land1 = wa_eina-land1.
  wa_ztmm_6026_01-value2src = c_ftzlink_source.
  IF wa_eina-waers IS INITIAL.
    wa_ztmm_6026_01-waerssrc = c_ftzlink_source.
  ELSE.
    wa_ztmm_6026_01-waers    = wa_eina-waers.
    wa_ztmm_6026_01-waerssrc = c_erp_source.
  ENDIF.
  wa_ztmm_6026_01-altvaluesrc = 'I'.
  wa_ztmm_6026_01-advaloremratesrc = 'H'.
  wa_ztmm_6026_01-specificratesrc = 'H'.
  wa_ztmm_6026_01-uomconvfactorsrc = 'I'.
  wa_ztmm_6026_01-adduomconvfacsrc = 'I'.
  wa_ztmm_6026_01-rptqtyuomsrc = 'H'.
  wa_ztmm_6026_01-addrptqtyuomsrc = 'H'.
  wa_ztmm_6026_01-dotindicator     = 'N'.
  wa_ztmm_6026_01-fccindicator     = 'N'.
  wa_ztmm_6026_01-fdaindicator     = 'N'.
  APPEND wa_ztmm_6026_01 TO it_ztmm_6026_01.
  CLEAR: wa_ztmm_6026_01,wa_mat_info1,wa_vbpa,wa_eina.
ENDLOOP.

*REFRESH: IT_EQUI_PLAN_INFO.

PERFORM ftz_sum_new.

PERFORM process_data_by_section.
IF sy-batch IS INITIAL.
  PERFORM dsp_log.
ENDIF.
INCLUDE zimmgm29i_6026cla.
INCLUDE zimmgm29i_6026o01.   "PBO Part
INCLUDE zimmgm29i_6026i01.   "PAI Part
INCLUDE zmmiftzrf01.

*&---------------------------------------------------------------------*
*&      Form  get_material_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_info.

  DATA: BEGIN OF wa_a018,
           matnr LIKE a018-matnr,
           lifnr LIKE a018-lifnr,
           knumh LIKE a018-knumh,
        END OF wa_a018.
  DATA: BEGIN OF wa_konp,
          knumh LIKE konp-knumh,
          kbetr LIKE konp-kbetr,
        END OF wa_konp.
  DATA:it_a018 LIKE TABLE OF wa_a018,
       it_konp LIKE HASHED TABLE OF wa_konp WITH UNIQUE KEY knumh.
  FIELD-SYMBOLS:  <fs_eina> LIKE LINE OF it_eina.

  w_date = sy-datum.
  SELECT mara~matnr werks profl ntgew gewei stawn maktx
                          INTO TABLE it_mat_info1
                          FROM mara
                          INNER JOIN marc
                          ON marc~matnr = mara~matnr
                          INNER JOIN makt
                          ON makt~matnr = mara~matnr
                          WHERE ( ( mtart = 'ROH' AND spras = sy-langu )
                          OR  ( mtart = 'ROH1' AND  spras = sy-langu )
                          OR  ( mtart = 'FERT' AND  spras = sy-langu ) )
** Changed on 02/1/12
*                           AND WERKS = P_WERKS.
                             AND werks IN s_werks
** End on 02/1/12
** Furong on 05/23/12 for sap tuning
%_HINTS ORACLE 'ORDERED USE_NL(T_00 T_01 T_02) INDEX (T_00 "MARA~T")'.
** End on 05/23/12

  IF sy-subrc NE 0.
  ENDIF.
  SELECT matnr eina~lifnr eine~werks ekorg waers peinh
               bpumz bpumn effpr land1
                     INTO TABLE it_eina
                     FROM eina
                     INNER JOIN eine
                     ON eine~infnr = eina~infnr
                     INNER JOIN lfa1
                     ON lfa1~lifnr = eina~lifnr
                     FOR ALL ENTRIES IN it_planord_info
                     WHERE matnr = it_planord_info-cmatnr.
*                     and   eina~lifnr = it_planord_info-lifnr.
  DESCRIBE TABLE it_eina LINES w_lines.
  IF w_lines = 0.
  ELSE.
    SELECT matnr lifnr knumh INTO TABLE it_a018
                             FROM a018
                             FOR ALL ENTRIES IN it_eina
                             WHERE matnr = it_eina-matnr
                             AND   lifnr = it_eina-lifnr
                             AND   ekorg = it_eina-ekorg
                             AND   datab <= w_date
                             AND   datbi >= w_date
                             AND   kschl = 'PB00'.
  ENDIF.
  DESCRIBE TABLE it_a018 LINES w_lines.
  IF w_lines = 0.
  ELSE.
    SORT it_a018 BY knumh.
    SELECT knumh kbetr FROM konp
                       INTO TABLE it_konp
                       FOR ALL ENTRIES IN it_a018
                       WHERE knumh = it_a018-knumh
                       AND   kschl = 'PB00'.
  ENDIF.
  CLEAR w_lines.
  SORT: it_eina BY matnr lifnr,
        it_a018 BY matnr lifnr.
  w_lines = 1.
  LOOP AT it_eina ASSIGNING <fs_eina>.
    READ TABLE it_a018 INTO wa_a018 WITH KEY matnr = <fs_eina>-matnr
                                             lifnr = <fs_eina>-lifnr
                                             BINARY SEARCH
                                             TRANSPORTING knumh.
    IF sy-subrc NE 0.
      <fs_eina>-kbetr = 0.
      CONTINUE.
    ENDIF.
    READ TABLE it_konp INTO wa_konp WITH TABLE KEY knumh = wa_a018-knumh.
    IF sy-subrc NE 0.
      <fs_eina>-kbetr = 0.
    ELSE.
      <fs_eina>-kbetr = wa_konp-kbetr.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_material_info
*&---------------------------------------------------------------------*
*&      Form  get_bom_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bom_info.

  DATA: wa_fsc_stpox LIKE stpox.
  DATA: it_fsc_stpox LIKE TABLE OF wa_fsc_stpox.
*  DATA: BEGIN OF WA_T460A,
*          MATNR LIKE MARC-MATNR,
*          WERKS LIKE MARC-WERKS,
*          WRK02 LIKE T460A-WRK02,
*        END OF WA_T460A.
*  DATA: IT_T460A LIKE TABLE OF WA_T460A.

  RANGES: r_bom_topmat FOR mara-matnr.

  FIELD-SYMBOLS: <fs_halb> LIKE LINE OF it_halb,
                  <fs_halb_exp> LIKE LINE OF it_halb_exp,
                 <fs_com> LIKE LINE OF it_fsc_stpox1,
                 <fs_halb_eng> LIKE LINE OF it_halb_eng.

  CLEAR: w_matnr_nc, w_color,
         w_taskname, w_rcv_jobs, w_snd_jobs, w_excep_flag,
         w_i, w_no_times, w_rem, w_frm, w_to, w_lines.

  r_mtart-sign = 'E'.
  r_mtart-option = 'EQ'.
  r_mtart-low = 'ROH'.
  APPEND r_mtart.
  r_mtart-low = 'ROH1'.
  APPEND r_mtart.

  DO.
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = p_srvgrp
      IMPORTING
        max_pbt_wps                    = w_max
        free_pbt_wps                   = w_free
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.
    IF sy-subrc <> 0 OR w_free = 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.


  DESCRIBE TABLE it_halb_exp LINES w_lines.
  IF w_lines > w_free.
    w_rem = w_lines MOD w_free.
    w_no_times = w_lines / w_free.
    IF w_rem EQ 0.
    ELSE.
      w_no_times = w_no_times + 1.
    ENDIF.
  ELSE.
    w_no_times = 1.
  ENDIF.
  w_i = 1.
  WHILE w_i <= w_no_times.
    IF w_i = 1.
      w_frm = w_i.
    ELSE.
      w_frm = w_to + 1.
    ENDIF.
    IF w_lines > w_free.
      w_to =  w_i * w_free .
    ELSE.
      w_to = w_lines.
    ENDIF.
    LOOP AT it_halb_exp ASSIGNING <fs_halb_exp> FROM w_frm TO w_to.
      r_bom_topmat-sign = 'I'.
      r_bom_topmat-option = 'EQ'.
      r_bom_topmat-low = <fs_halb_exp>-matnr.
      COLLECT r_bom_topmat.
      CLEAR: r_bom_topmat.

      DO.
        CALL FUNCTION 'Z_FFTZ_EXP_BOM'
          STARTING NEW TASK w_taskname
          DESTINATION IN GROUP p_srvgrp
          PERFORMING fsc_bom_exp ON END OF TASK
          EXPORTING
            p_capid               = p_capid
            p_datuv               = <fs_halb_exp>-datuv
            p_emeng               = <fs_halb_exp>-menge
            p_mehrs               = 'X'
            p_mmory               = '1'
            p_mtnrv               = <fs_halb_exp>-matnr
*           P_STLAL               = <FS_HALB_EXP>-STLAL
            p_stlan               = '1'
            p_werks               = <fs_halb_exp>-werks
            p_pamatnr             = <fs_halb_exp>-matnr
          TABLES
            p_stpox               = it_fsc_stpox
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            resource_failure      = 3.
        CASE sy-subrc.
          WHEN 0.
            w_taskname = w_taskname + 1.
            w_snd_jobs = w_snd_jobs + 1.
            EXIT.
          WHEN 1 OR 2.
            w_excep_flag = 'X'.
          WHEN 3.
            IF w_excep_flag = space.
              w_excep_flag = 'X'.
              WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.01' SECONDS.
            ELSE.
              WAIT UNTIL w_rcv_jobs >= w_snd_jobs UP TO '0.1' SECONDS.
            ENDIF.
            IF sy-subrc EQ 0.
              CLEAR w_excep_flag.
            ELSE.
*              EXIT.
            ENDIF.
        ENDCASE.
      ENDDO.

    ENDLOOP.

    DO.
      WAIT UNTIL w_rcv_jobs >= w_snd_jobs.
      IF w_rcv_jobs >= w_snd_jobs.
        EXIT.
      ENDIF.
    ENDDO.


    w_i = w_i + 1.
  ENDWHILE.

  CLEAR: r_bom_topmat.
  CHECK NOT it_fsc_stpox1[] IS INITIAL.
  LOOP AT it_fsc_stpox1 ASSIGNING <fs_com>.
*    READ TABLE IT_MAT_TYPE WITH TABLE KEY MATNR = <FS_COM>-IDNRK
*                                          MTART = 'ROH'
*                                                 TRANSPORTING NO FIELDS
*.
**&----Get rid of the color from material number.
*    IF SY-SUBRC EQ 0.
*      PERFORM SEPARATE_COLOR USING   <FS_COM>-IDNRK 'ROH'
*                            CHANGING W_MATNR_NC
*                                     W_COLOR.
*      WA_COLOR_PARTS-MATNR = <FS_COM>-IDNRK.
*      COLLECT WA_COLOR_PARTS INTO IT_COLOR_PARTS.
*    ELSE.
    w_matnr_nc = <fs_com>-idnrk.
    w_color    = space.
*    ENDIF.
    wa_bom_com-plnum = <fs_com>-plnum.
    wa_bom_com-vbeln = <fs_com>-vbeln.
    wa_bom_com-matnr = <fs_com>-pmatnr.
    wa_bom_com-stlal = <fs_com>-stlal1.
    wa_bom_com-datuv = <fs_com>-datuv1.
    wa_bom_com-idnrk = w_matnr_nc.
    wa_bom_com-werks = <fs_com>-werks.
    wa_bom_com-menge = <fs_com>-menge.      "perlm
    wa_bom_com-mngko = <fs_com>-mngko.
    wa_bom_com-meins = <fs_com>-meins.
    COLLECT wa_bom_com INTO it_bom_com.
    CLEAR: wa_bom_com, wa_color_parts, w_matnr_nc, w_color.
  ENDLOOP.

  SORT: it_halb BY matnr pmatnr werks stlal datuv.

  LOOP AT it_halb ASSIGNING <fs_halb>.
    LOOP AT it_bom_com INTO wa_bom_com
                       WHERE matnr = <fs_halb>-matnr
                       AND   stlal = <fs_halb>-stlal
                       AND   datuv = <fs_halb>-datuv.
      wa_planord_info-plnum = <fs_halb>-datuv.
*      WA_PLANORD_INFO-VBELN = <FS_HALB>-VBELN.
      wa_planord_info-pmatnr = <fs_halb>-pmatnr.
      wa_planord_info-cmatnr = wa_bom_com-idnrk.
      wa_planord_info-plant  = wa_bom_com-werks.
      wa_planord_info-reqqty = wa_bom_com-menge.
      wa_planord_info-menge  =  <fs_halb>-menge * wa_bom_com-mngko.
      wa_planord_info-meins = wa_bom_com-meins.
      APPEND wa_planord_info TO it_planord_info.
      CLEAR wa_planord_info.
    ENDLOOP.
  ENDLOOP.

  CLEAR: w_matnr_nc, w_color,
         w_taskname, w_rcv_jobs, w_snd_jobs, w_excep_flag,
         w_i, w_no_times, w_max, w_free, w_frm, w_to.

ENDFORM.                    " get_bom_info
*&---------------------------------------------------------------------*
*&      Form  ftz_sum_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ftz_sum_new.

  DATA: BEGIN OF wa_spnm_sum,
          matnr  LIKE mara-matnr,
          ordnum LIKE ztmm_6026_01-ordernumwork,
          ordernumship LIKE  wa_ztmm_6026_01-ordernumship,
          menge LIKE mseg-menge,
        END OF wa_spnm_sum.
  DATA: it_spnm_sum LIKE TABLE OF wa_spnm_sum.

  LOOP AT it_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    wa_spnm_sum-matnr  = <fs_ztmm_6026_01>-matnr.
    wa_spnm_sum-ordnum = <fs_ztmm_6026_01>-ordernumwork.
    wa_spnm_sum-ordernumship = <fs_ztmm_6026_01>-ordernumship.
    wa_spnm_sum-menge  = <fs_ztmm_6026_01>-menge.
    COLLECT wa_spnm_sum INTO it_spnm_sum.
    CLEAR wa_spnm_sum.
  ENDLOOP.

  REFRESH it_ztmm_6026_01_tmp.

  SORT: it_spnm_sum BY matnr ordnum,
        it_ztmm_6026_01 BY matnr ordernumwork ordernumship.

  LOOP AT it_spnm_sum INTO wa_spnm_sum.
    READ TABLE it_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>
                        WITH KEY matnr = wa_spnm_sum-matnr
                                 ordernumwork = wa_spnm_sum-ordnum
                                 ordernumship = wa_spnm_sum-ordernumship
                                      BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR: wa_ztmm_6026_01.
      CONTINUE.
    ELSE.
      wa_ztmm_6026_01 = <fs_ztmm_6026_01>.
      wa_ztmm_6026_01-menge = wa_spnm_sum-menge.
      APPEND wa_ztmm_6026_01 TO it_ztmm_6026_01_tmp.
      CLEAR: wa_ztmm_6026_01.
    ENDIF.
  ENDLOOP.
  it_ztmm_6026_01[] = it_ztmm_6026_01_tmp[].
  FREE: it_ztmm_6026_01_tmp.

ENDFORM.                    " ftz_sum_new
