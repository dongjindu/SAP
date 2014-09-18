REPORT ZMMIFTZC_NEW.
INCLUDE zmmfztop.
INCLUDE zmmcontp.
*&--------------------------------------------------------------------&*
*&    Program: ZMMIFTZC.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Send FTZ related consumption information.        &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 03/08/2005  Shiva    UD1K915611      initial program.
*& 08/26/2005  Shiva    UD1K917075      FTZ Consumption info for FIFO.
*&                      UD1K917581      add oredr num for grouping.
*& 10/11/2005  Shiva    UD1K917926      New structure for select stmt
*&                                      to get material info.
*& 10/14/2005  Shiva    UD1K917970      Fixed: STAWNSRC value.
*&                                        Removed hard coding material.
*& 10/18/2005  Shiva    UD1K918009      Fixes based on BCV client test.
*& 10/19/2005  Shiva    UD1K918027      Fixes based on BCV client test.
*& 10/20/2005  Shiva    UD1K918058      Fixes based on BCV client test.
*& 10/21/2005  Shiva    UD1K918078      Fixes based on BCV client test.
*&--------------------------------------------------------------------&*

PARAMETERS: p_date   LIKE sy-datum OBLIGATORY,
            p_plnum  LIKE plaf-plnum,
            p_srvgrp LIKE rzllitab-classname OBLIGATORY
                     DEFAULT 'PG_FTZ'.
parameters: p_time type i default 10.

r_dt-sign = 'I'.
r_dt-option = 'BT'.
CONCATENATE p_date '000000' INTO w_date_time.
r_dt-low = w_date_time.
CLEAR w_date_time.
CONCATENATE p_date '235959' INTO w_date_time.
r_dt-high = w_date_time.
APPEND r_dt.
CLEAR: r_dt, w_date_time.

CONCATENATE p_date+4(2) '/' p_date+6(2) '/' p_date(4) INTO w_key_date.

SELECT matnr mtart meins FROM mara
                         INTO TABLE it_mat_type
                        WHERE ( mtart = 'HALB' AND lvorm = space )
                        OR    ( mtart = 'ROH'  AND lvorm = space )
                        OR    ( mtart = 'FERT' AND lvorm = space ).

SELECT plnum FROM plaf
             INTO TABLE it_plaf.

SELECT atinn adzhl atnam FROM cabn
                         INTO TABLE it_cabn
                         WHERE atnam = 'P_DESTINATION_CODE'
                         OR    atnam = 'P_MI'
                         OR    atnam = 'P_MODEL_YEAR'
                         OR    atnam = 'P_OCN'
                         OR    atnam = 'P_PLAN_ORDER'
                         OR    atnam = 'P_RP25_ACTUAL_DATE'
                         OR    atnam = 'P_RP27_ACTUAL_DATE'
                         OR    atnam = 'P_RP_STATUS'
                         OR    atnam = 'P_SALES_ORDER'
                         OR    atnam = 'P_SEQUENCE_DATE'
                         OR    atnam = 'P_USAGE_CAR'
                         OR    atnam = 'P_VERSION'.
LOOP AT it_cabn INTO wa_cabn.
  CASE wa_cabn-atnam.
    WHEN 'P_DESTINATION_CODE'.
      w_atinn1 = wa_cabn-atinn.
      w_adzhl1 = wa_cabn-adzhl.
    WHEN 'P_MI'.
      w_atinn2 = wa_cabn-atinn.
      w_adzhl2 = wa_cabn-adzhl.
    WHEN 'P_MODEL_YEAR'.
      w_atinn3 = wa_cabn-atinn.
      w_adzhl3 = wa_cabn-adzhl.
    WHEN 'P_OCN'.
      w_atinn4 = wa_cabn-atinn.
      w_adzhl4 = wa_cabn-adzhl.
    WHEN 'P_PLAN_ORDER'.
      w_atinn5 = wa_cabn-atinn.
      w_adzhl5 = wa_cabn-adzhl.
    WHEN 'P_RP25_ACTUAL_DATE'.
      w_atinn6 = wa_cabn-atinn.
      w_adzhl6 = wa_cabn-adzhl.
    WHEN 'P_RP27_ACTUAL_DATE'.
      w_atinn7 = wa_cabn-atinn.
      w_adzhl7 = wa_cabn-adzhl.
    WHEN 'P_RP_STATUS'.
      w_atinn8 = wa_cabn-atinn.
      w_adzhl8 = wa_cabn-adzhl.
    WHEN 'P_SALES_ORDER'.
      w_atinn9 = wa_cabn-atinn.
      w_adzhl9 = wa_cabn-adzhl.
    WHEN 'P_SEQUENCE_DATE'.
      w_atinn10 = wa_cabn-atinn.
      w_adzhl10 = wa_cabn-adzhl.
    WHEN 'P_USAGE_CAR'.
      w_atinn11 = wa_cabn-atinn.
      w_adzhl11 = wa_cabn-adzhl.
    WHEN 'P_VERSION'.
      w_atinn12 = wa_cabn-atinn.
      w_adzhl12 = wa_cabn-adzhl.
  ENDCASE.
ENDLOOP.

IF sy-uname = '101457'. " OR sy-uname ='100698'.
  SELECT objek atinn atwrt
              INTO TABLE it_ausp
              FROM ausp
              WHERE objek = any ( SELECT objek FROM ausp
                                   WHERE  klart = '002' AND
                                          atinn = w_atinn5 AND
                                          adzhl = w_adzhl5 AND
                                          atwrt = p_plnum ).
else.
SELECT objek atinn atwrt atflv
             INTO TABLE it_ausp
             FROM ausp
             WHERE objek = any ( SELECT objek FROM ausp
                                  WHERE ( ( klart = '002' AND
                                          atinn = w_atinn8 AND
                                          adzhl = w_adzhl8 AND
                                          atwrt = '25' )
                                    OR  ( klart = '002' AND
                                          atinn = w_atinn8 AND
                                          adzhl = w_adzhl8 AND
                                          atwrt = '27' ) )
                               AND objek = ANY ( SELECT objek FROM ausp
                                            WHERE ( klart = '002' AND
                                                 atinn = w_atinn6 AND
                                                 adzhl = w_adzhl6 AND
                                                 atwrt IN r_dt )
                                             OR ( klart = '002'   AND
                                                  atinn = w_atinn7 AND
                                                  adzhl = w_adzhl7 AND
                                                  atwrt IN r_dt ) ) )
  AND ( ( klart = '002' AND atinn = w_atinn1 AND adzhl = w_adzhl1 )
         OR ( klart = '002' AND atinn = w_atinn2 AND adzhl = w_adzhl2 )
         OR ( klart = '002' AND atinn = w_atinn3 AND adzhl = w_adzhl3 )
         OR ( klart = '002' AND atinn = w_atinn4 AND adzhl = w_adzhl4 )
         OR ( klart = '002' AND atinn = w_atinn5 AND adzhl = w_adzhl5 )
         OR ( klart = '002' AND atinn = w_atinn9 AND adzhl = w_adzhl9 )
        OR ( klart = '002' AND atinn = w_atinn10 AND adzhl = w_adzhl10 )
        OR ( klart = '002' AND atinn = w_atinn11 AND adzhl = w_adzhl11 )
   OR ( klart = '002' AND atinn = w_atinn12 AND adzhl = w_adzhl12 ) ).
endif.
IF sy-subrc NE 0.
  MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH text-001.
  EXIT.
ENDIF.

LOOP AT it_ausp ASSIGNING <fs_ausp>
                WHERE atinn EQ w_atinn11
                AND   atwrt NE 'P'.
  r_scrap-sign = 'I'.
  r_scrap-option = 'EQ'.
  r_scrap-low = <fs_ausp>-objek.
  APPEND r_scrap.
ENDLOOP.

IF NOT r_scrap[] IS INITIAL.
  DELETE it_ausp WHERE objek IN r_scrap.
ENDIF.
SORT: it_plaf BY plnum,
      it_ausp BY objek.
*&--------------------------------------------------------------------&*
*     FSC = model yr. + Destn.code + model idx. + space + OCN         &*
*&--------------------------------------------------------------------&*
LOOP AT it_ausp ASSIGNING <fs_ausp>.
  CASE <fs_ausp>-atinn.
    WHEN w_atinn1.
      w_destn = <fs_ausp>-atwrt.
    WHEN w_atinn2.
      w_modidx = <fs_ausp>-atwrt.
    WHEN w_atinn3.
      w_modyr = <fs_ausp>-atwrt.
    WHEN w_atinn4.
      w_ocn = <fs_ausp>-atwrt.
    WHEN w_atinn5.
      w_plnum = <fs_ausp>-atwrt.
    WHEN w_atinn9.
      w_salord = <fs_ausp>-atwrt.
    WHEN w_atinn10.
      w_pack = <fs_ausp>-atflv.
      WRITE w_pack TO w_date.
    WHEN w_atinn12.
      w_versn = <fs_ausp>-atwrt.
  ENDCASE.
  AT END OF objek.
    CONCATENATE w_modyr w_destn w_modidx INTO w_fsc.
    CONCATENATE w_fsc w_ocn INTO w_fsc SEPARATED BY space.
    wa_equi_info-equnr = <fs_ausp>-objek.
    wa_equi_info-fscod = w_fsc.
    wa_equi_info-plnum = w_plnum.
    wa_equi_info-codes = w_destn.
    wa_equi_info-sdate = w_date.
    wa_equi_info-versn = w_versn.
    wa_equi_info-salod = w_salord.
    APPEND wa_equi_info TO it_equi_info.
    CLEAR: wa_equi_info, w_modyr, w_destn, w_modidx, w_ocn, w_fsc.
  ENDAT.
ENDLOOP.

CLEAR w_lines.

IF NOT p_plnum IS INITIAL.
  DELETE it_equi_info WHERE plnum NE p_plnum.
  DESCRIBE TABLE it_equi_info LINES w_lines.
  IF w_lines EQ 0.
    MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH text-002.
    EXIT.
  ENDIF.
ENDIF.
LOOP AT it_equi_info ASSIGNING  <fs_equi>.
  READ TABLE it_plaf WITH KEY plnum = <fs_equi>-plnum
                                      TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    wa_fsc_info-plnum = <fs_equi>-plnum.
    wa_fsc_info-matnr = <fs_equi>-fscod.
    wa_fsc_info-sdate = <fs_equi>-sdate.
    wa_fsc_info-versn = <fs_equi>-versn.
    wa_fsc_info-salod = <fs_equi>-salod.
    APPEND wa_fsc_info TO it_fsc_info.
    <fs_equi>-mark = 'X'.
  ENDIF.
ENDLOOP.
*&---Get plant info for FSC.
DESCRIBE TABLE it_fsc_info LINES w_lines.
IF w_lines NE 0.
  SELECT matnr werks INTO TABLE it_marc
                     FROM marc
                     FOR ALL ENTRIES IN it_fsc_info
                     WHERE matnr = it_fsc_info-matnr.
  IF sy-subrc NE 0.
  ELSE.
    SORT it_marc BY matnr.
  ENDIF.
ENDIF.
LOOP AT it_fsc_info INTO wa_fsc_info.
  wa_halb-plnum = wa_fsc_info-plnum.
  wa_halb-vbeln = wa_fsc_info-salod.
  wa_halb-matnr = wa_fsc_info-matnr.
  wa_halb-pmatnr = wa_fsc_info-matnr.
  READ TABLE it_marc INTO wa_marc WITH KEY matnr = wa_fsc_info-matnr
                                                   BINARY SEARCH.
  IF sy-subrc NE 0.
  ELSE.
    wa_halb-werks = wa_marc-werks.
  ENDIF.
  wa_halb-menge = 1.
  wa_halb-stlal = wa_fsc_info-versn+1(2).
  wa_halb-datuv = wa_fsc_info-sdate.
  APPEND  wa_halb TO it_halb.
ENDLOOP.

DELETE it_equi_info WHERE mark = 'X'.

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
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
DESCRIBE TABLE it_equi_info LINES w_lines.
if p_time > 1.
   w_free = w_free * p_time.
endif.
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
    w_to = w_i * w_free.
  ELSE.
    w_to = w_lines.
  ENDIF.
  LOOP AT it_equi_info ASSIGNING  <fs_equi> FROM w_frm TO w_to.
  DO.
    CALL FUNCTION 'Z_FFTZ_READ_PLANORDER'
         STARTING NEW TASK    w_taskname
         DESTINATION IN GROUP p_srvgrp
         PERFORMING bom_info ON END OF TASK
         EXPORTING
           p_plnum        = <fs_equi>-plnum
         TABLES
           p_itcomponents = it_components
         EXCEPTIONS
           communication_failure = 1
           system_failure        = 2
           RESOURCE_FAILURE      = 3.

    CASE sy-subrc.
      WHEN 0.
        w_taskname = w_taskname + 1.
        w_snd_jobs = w_snd_jobs + 1.
        exit.
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
*        ELSE.
*          EXIT.
        ENDIF.
    ENDCASE.
   enddo.
 ENDLOOP.

* Replace WAIT statement for loop

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

DO.
       WAIT UNTIL w_rcv_jobs >= w_snd_jobs.
       IF w_rcv_jobs >= w_snd_jobs.
          EXIT.
       ENDIF.
ENDDO.




  w_i = w_i + 1.
ENDWHILE.
*&----Add all Parent material( Existing Plan ord + Deleted Plan ord)
LOOP AT it_halb INTO wa_halb.
  wa_parent_mat-plnum = wa_halb-plnum.
  wa_parent_mat-vbeln = wa_halb-vbeln.
  wa_parent_mat-matnr = wa_halb-matnr.
  wa_parent_mat-plant = wa_halb-werks.
  wa_parent_mat-menge = wa_halb-menge.
  READ TABLE it_mat_type INTO wa_mat_type
                         WITH TABLE KEY matnr = wa_halb-matnr
                                        mtart = 'FERT'
                                       TRANSPORTING meins.
  IF sy-subrc NE 0.
    wa_parent_mat-meins = space.
  ELSE.
    wa_parent_mat-meins = wa_mat_type-meins.
  ENDIF.
  COLLECT wa_parent_mat INTO it_parent_mat.
ENDLOOP.
CLEAR: wa_parent_mat, wa_halb,wa_mat_type.
SORT it_temp BY matcom-material.
LOOP AT it_temp ASSIGNING <fs_comp>.
READ TABLE it_mat_type WITH TABLE KEY matnr = <fs_comp>-matcom-material
                                                          mtart = 'ROH'
                                                 TRANSPORTING NO FIELDS.
*&----Get rid of the color from material number.
  IF sy-subrc EQ 0.
    PERFORM separate_color USING <fs_comp>-matcom-material
                                 'ROH'
                        CHANGING w_matnr_nc
                                 w_color.
    wa_color_parts-matnr = <fs_comp>-matcom-material.
    COLLECT wa_color_parts INTO it_color_parts.
    CLEAR wa_color_parts.
  ELSE.
    w_matnr_nc = <fs_comp>-matcom-material.
    w_color = space.
  ENDIF.
  wa_planord_info-plnum = <fs_comp>-plnum.
  wa_planord_info-vbeln = <fs_comp>-vbeln.
  wa_planord_info-pmatnr = <fs_comp>-pmatnr.
  wa_planord_info-cmatnr = w_matnr_nc.
  wa_planord_info-plant  = <fs_comp>-matcom-plant.
  wa_planord_info-reqqty = <fs_comp>-matcom-req_quan.
  wa_planord_info-menge  = <fs_comp>-matcom-entry_qty.
  wa_planord_info-meins  = <fs_comp>-matcom-base_uom.
  COLLECT wa_planord_info INTO it_planord_info.
*&----Get HALB material for BOM explosion.
READ TABLE it_mat_type WITH TABLE KEY matnr = <fs_comp>-matcom-material
                                                         mtart = 'HALB'
                                                 TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
  ELSE.
    wa_halb-plnum = <fs_comp>-plnum.
    wa_halb-vbeln = <fs_comp>-vbeln.
    wa_halb-matnr = <fs_comp>-matcom-material.
    wa_halb-pmatnr = <fs_comp>-pmatnr.
    wa_halb-werks = <fs_comp>-matcom-plant.
    wa_halb-menge = <fs_comp>-matcom-entry_qty.
    wa_halb-stlal = space.
    wa_halb-datuv = p_date.
    COLLECT wa_halb INTO it_halb.
  ENDIF.
  CLEAR: wa_halb, wa_planord_info, w_matnr_nc, w_color.
ENDLOOP.
FREE it_temp.
IF <fs_comp> IS ASSIGNED.
  UNASSIGN <fs_comp>.
ENDIF.
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
                         WHERE menge IS initial.
*&---------------Explode BOM for HALB & FSC---------------------------&*
PERFORM get_bom_info.
*&--------------------------------------------------------------------&*
CLEAR wa_planord_info.
LOOP AT it_parent_mat INTO wa_parent_mat.
  wa_planord_info-plnum  = wa_parent_mat-plnum.
  wa_planord_info-vbeln  = wa_parent_mat-vbeln.
  wa_planord_info-pmatnr = wa_parent_mat-matnr.
  wa_planord_info-cmatnr = wa_parent_mat-matnr.
  wa_planord_info-plant  = wa_parent_mat-plant.
  wa_planord_info-menge  = wa_parent_mat-menge.
  wa_planord_info-meins  = wa_parent_mat-meins.
  APPEND wa_planord_info TO it_planord_info.
*Sales order into to get country Shipped to.
  wa_vbak-vbeln = wa_parent_mat-vbeln.
  COLLECT wa_vbak INTO it_vbak.
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
  CONCATENATE <fs_planord>-pmatnr p_date
                                  INTO wa_ztmm_6026_01-ordernumwork.
*  wa_ztmm_6026_01-billoflading.
  CONCATENATE p_date 'T' '000000' INTO wa_ztmm_6026_01-txndate.
  CONCATENATE 'SHIPCAR' p_date INTO wa_ztmm_6026_01-ordernumship.
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
  READ TABLE it_vbpa INTO wa_vbpa WITH KEY vbeln = <fs_planord>-vbeln
                                                   BINARY SEARCH.
  IF sy-subrc NE 0.
    wa_ztmm_6026_01-countryshipto = space.
  ELSE.
    wa_ztmm_6026_01-countryshipto = wa_vbpa-land1.
  ENDIF.
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
    CONCATENATE <fs_planord>-cmatnr '*' INTO w_c_matnr.
    LOOP AT it_color_parts INTO wa_color_parts
                            WHERE matnr CP w_c_matnr.
      w_c_matnr = wa_color_parts-matnr.
      EXIT.
    ENDLOOP.
    READ TABLE it_mat_info1 INTO wa_mat_info1
                           WITH TABLE KEY matnr = w_c_matnr
                                          werks = <fs_planord>-plant.
    IF sy-subrc NE 0.
      wa_ztmm_6026_01-stawn    = space.
      wa_ztmm_6026_01-stawnsrc = 'H'.
      wa_ztmm_6026_01-ntgew  = space.
      wa_ztmm_6026_01-ntgewsrc = c_ftzlink_source.
      wa_ztmm_6026_01-gewei = space.
      wa_ztmm_6026_01-geweisrc = c_ftzlink_source.
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

PERFORM ftz_sum_new.

PERFORM process_data_by_section.
IF sy-batch IS INITIAL.
  PERFORM dsp_log.
ENDIF.
INCLUDE zimmgm29i_6026cla.
INCLUDE zimmgm29i_6026o01.   "PBO Part
INCLUDE zimmgm29i_6026i01.   "PAI Part
INCLUDE zmmiftzrf01.

*---------------------------------------------------------------------*
*       FORM bom_info                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  W_TASKNAME                                                    *
*---------------------------------------------------------------------*
FORM bom_info USING w_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FFTZ_READ_PLANORDER'
          IMPORTING ps_result = wa_return
                    ps_header = wa_header
          TABLES    pit_components = it_components
          EXCEPTIONS
                    communication_failure = 1
                    system_failure        = 2.
  IF sy-subrc NE 0.
    w_excep_flag = 'X'.
    EXIT.
  ENDIF.
  w_rcv_jobs = w_rcv_jobs + 1.

  wa_parent_mat-plnum = wa_header-plannedorder_num.
  wa_parent_mat-matnr = wa_header-material.
  wa_parent_mat-plant = wa_header-prod_plant.
  wa_parent_mat-menge = wa_header-total_plord_qty.
  wa_parent_mat-meins = wa_header-base_uom.
  wa_parent_mat-vbeln = wa_header-sales_ord.
  APPEND wa_parent_mat TO it_parent_mat.

  DELETE it_components WHERE phant_item = 'X'.
  APPEND LINES OF it_components TO it_temp.
  wa_temp-plnum  = wa_header-plannedorder_num.
  wa_temp-pmatnr = wa_header-material.
  wa_temp-vbeln  = wa_header-sales_ord.
  MODIFY it_temp FROM wa_temp TRANSPORTING plnum pmatnr vbeln
                       WHERE pmatnr IS initial.
  REFRESH: it_components.
  CLEAR: wa_return, wa_header, wa_temp, wa_parent_mat.

ENDFORM.
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
  SELECT mara~matnr werks profl ntgew gewei stawn maktx
                          INTO TABLE it_mat_info1
                          FROM mara
                          INNER JOIN marc
                          ON marc~matnr = mara~matnr
                          INNER JOIN makt
                          ON makt~matnr = mara~matnr
*                          for all entries in it_planord_info
*                          where mara~matnr = it_planord_info-cmatnr
                           WHERE ( mtart = 'ROH' AND spras = sy-langu )
                           OR  ( mtart = 'ROH1' AND  spras = sy-langu )
                           OR  ( mtart = 'FERT' AND  spras = sy-langu ).
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
                     FOR ALL entries IN it_planord_info
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
  DATA: BEGIN OF wa_t460a,
          matnr LIKE marc-matnr,
          werks LIKE marc-werks,
          wrk02 LIKE t460a-wrk02,
        END OF wa_t460a.
  DATA: it_t460a LIKE TABLE OF wa_t460a.

  RANGES: r_bom_topmat FOR mara-matnr.

  FIELD-SYMBOLS: <fs_halb> LIKE LINE OF it_halb,
                  <fs_halb_exp> LIKE LINE OF it_halb_exp,
                 <fs_com> LIKE LINE OF it_fsc_stpox1.

  CLEAR: w_matnr_nc, w_color,
         w_taskname, w_rcv_jobs, w_snd_jobs, w_excep_flag,
         w_i, w_no_times, w_rem, w_frm, w_to, w_lines.

  r_mtart-sign = 'E'.
  r_mtart-option = 'EQ'.
  r_mtart-low = 'ROH'.
  APPEND r_mtart.
  r_mtart-low = 'ROH1'.
  APPEND r_mtart.

  SELECT matnr marc~werks wrk02 INTO TABLE it_t460a
                                FROM marc
                                INNER JOIN t460a
                                ON t460a~werks = marc~werks
                                AND t460a~sobsl = marc~sobsl
                               FOR ALL entries IN it_halb_exp
                                WHERE matnr EQ it_halb_exp-matnr
                                AND  marc~werks EQ it_halb_exp-werks.
  IF sy-subrc NE 0.
  ELSE.
    SORT it_t460a BY matnr werks.
    LOOP AT it_halb_exp ASSIGNING <fs_halb_exp>.
      READ TABLE it_t460a INTO wa_t460a
                          WITH KEY matnr = <fs_halb_exp>-matnr
                                   werks = <fs_halb_exp>-werks
                                   BINARY SEARCH.
      IF sy-subrc NE 0.
      ELSE.
        IF wa_t460a-wrk02 IS INITIAL.
        ELSE.
          <fs_halb_exp>-werks = wa_t460a-wrk02.
          wa_halb-werks       = wa_t460a-wrk02.
          MODIFY it_halb FROM wa_halb TRANSPORTING werks
                          WHERE matnr = <fs_halb_exp>-matnr.
          CLEAR: wa_halb, wa_t460a.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
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
** change by Furong on 05/04/2006
      DO.
        CALL FUNCTION 'Z_FFTZ_EXP_BOM'
          STARTING NEW TASK w_taskname
          DESTINATION IN GROUP p_srvgrp
          PERFORMING fsc_bom_exp ON END OF TASK
          EXPORTING
            p_capid  = 'PP01'
            p_datuv  = <fs_halb_exp>-datuv
            p_emeng  = <fs_halb_exp>-menge
            p_mehrs  = 'X'
            p_mmory  = '1'
            p_mtnrv  = <fs_halb_exp>-matnr
            p_stlal  = <fs_halb_exp>-stlal
            p_stlan  = '1'
            p_werks  = <fs_halb_exp>-werks
          TABLES
            p_stpox  = it_fsc_stpox
          EXCEPTIONS
             communication_failure = 1
             system_failure        = 2
             RESOURCE_FAILURE      = 3.
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
** end of change
    ENDLOOP.

* Replace WAIT statement for loop

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

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
*&-----------Take the HALB material out of the main list.
  IF r_bom_topmat[] IS INITIAL.
  ELSE.
    DELETE it_planord_info WHERE cmatnr IN r_bom_topmat.
  ENDIF.
  LOOP AT it_fsc_stpox1 ASSIGNING <fs_com>.
    READ TABLE it_mat_type WITH TABLE KEY matnr = <fs_com>-idnrk
                                          mtart = 'ROH'
                                                 TRANSPORTING NO FIELDS.
*&----Get rid of the color from material number.
    IF sy-subrc EQ 0.
      PERFORM separate_color USING   <fs_com>-idnrk 'ROH'
                            CHANGING w_matnr_nc
                                     w_color.
      wa_color_parts-matnr = <fs_com>-idnrk.
      COLLECT wa_color_parts INTO it_color_parts.
    ELSE.
      w_matnr_nc = <fs_com>-idnrk.
      w_color    = space.
    ENDIF.
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
      wa_planord_info-plnum = <fs_halb>-plnum.
      wa_planord_info-vbeln = <fs_halb>-vbeln.
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
form ftz_sum_new.

  data: begin of wa_spnm_sum,
          matnr  like mara-matnr,
          ordnum like ztmm_6026_01-ordernumwork,
          menge like mseg-menge,
        end of wa_spnm_sum.
  data: it_spnm_sum like table of wa_spnm_sum.

  loop at it_ztmm_6026_01 assigning <fs_ztmm_6026_01>.
    wa_spnm_sum-matnr  = <fs_ztmm_6026_01>-matnr.
    wa_spnm_sum-ordnum = <fs_ztmm_6026_01>-ordernumwork.
    wa_spnm_sum-menge  = <fs_ztmm_6026_01>-menge.
    collect wa_spnm_sum into it_spnm_sum.
    clear wa_spnm_sum.
  endloop.

  refresh it_ztmm_6026_01_tmp.

*  it_ztmm_6026_01_tmp[] = it_ztmm_6026_01.
*  refresh it_ztmm_6026_01.

  sort: it_spnm_sum by matnr ordnum,
        it_ztmm_6026_01 by matnr ordernumwork.

  loop at it_spnm_sum into wa_spnm_sum.
*      read table it_ztmm_6026_01_tmp assigning <fs_ztmm_6026_01>
*                                   with key matnr = wa_spnm_sum-matnr
*                                     ordernumwork = wa_spnm_sum-ordnum
*                                     binary search.
   read table it_ztmm_6026_01 assigning <fs_ztmm_6026_01>
                                   with key matnr = wa_spnm_sum-matnr
                                     ordernumwork = wa_spnm_sum-ordnum
                                     binary search.
    if sy-subrc ne 0.
      clear: wa_ztmm_6026_01.
      continue.
    else.
      wa_ztmm_6026_01 = <fs_ztmm_6026_01>.
      wa_ztmm_6026_01-menge = wa_spnm_sum-menge.
      append wa_ztmm_6026_01 to it_ztmm_6026_01_TMP.
      clear: wa_ztmm_6026_01.
    endif.
  endloop.
  it_ztmm_6026_01[] = it_ztmm_6026_01_TMP[].
  free: it_ztmm_6026_01_tmp.

endform.                    " ftz_sum_new
