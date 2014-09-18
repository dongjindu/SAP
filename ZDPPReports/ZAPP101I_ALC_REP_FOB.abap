*----------------------------------------------------------------------*
*   INCLUDE YAPP101L_ALC_REP_FOB                                       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_field
*&---------------------------------------------------------------------*
*       Setting Data For a Dropdown listbox - Work Order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field.
  DATA: l_wo_ser TYPE ztpp_alc_rerun-wo_ser,
        l_nation TYPE ztpp_alc_rerun-nation,
        l_dealer TYPE ztpp_alc_rerun-dealer.

  SELECT DISTINCT wo_ser nation dealer
    INTO (l_wo_ser , l_nation , l_dealer)
    FROM ztpp_alc_rerun.

    CLEAR workorder_value.

    CONCATENATE l_wo_ser l_nation l_dealer
      INTO workorder_value-key.

    SELECT SINGLE maktx
      INTO workorder_value-text
      FROM makt
      WHERE spras = 'EN' AND
            matnr = workorder_value-key.

    APPEND workorder_value TO workorder_list.

  ENDSELECT.

ENDFORM.                    " set_field
*&---------------------------------------------------------------------*
*&      Form  call_func_vrm
*&---------------------------------------------------------------------*
*       Call Func. For Creating Dropdown Listbox.
*----------------------------------------------------------------------*
*      -->P_WORKORDER_LIST  text
*----------------------------------------------------------------------*
FORM call_func_vrm USING    p_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = p_list.

ENDFORM.                    " call_func_vrm
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       Selection of Initial Data.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  DATA: l_wo_ser(10),
        l_nation(04),
        l_dealer(03),
        l_extc(04),
        l_intc(04).

  CLEAR: l_wo_ser(10),
         l_nation(04),
         l_dealer(03),
         l_extc(04),
         l_intc(04).
* w/o serial, nation, dealer
  IF p_workorder <> space.
    MOVE p_workorder+00(09) TO l_wo_ser.
    CONCATENATE l_wo_ser '%' INTO l_wo_ser.
    MOVE p_workorder+09(03) TO l_nation.
    CONCATENATE l_nation '%' INTO l_nation.
    MOVE p_workorder+12 TO l_dealer.
    CONCATENATE l_dealer '%' INTO l_dealer.
  ELSE.
    l_wo_ser = '%'.
    l_nation = '%'.
    l_dealer = '%'.
  ENDIF.
* external color
  IF p_extc <> space.
    CONCATENATE p_extc '%' INTO l_extc.
  ELSE.
    l_extc = '%'.
  ENDIF.
* internal color
  IF p_intc <> space.
    CONCATENATE p_intc '%' INTO l_intc.
  ELSE.
    l_intc = '%'.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_app101
    FROM  ztpp_alc_rerun
    WHERE wo_ser LIKE l_wo_ser AND
          nation LIKE l_nation AND
          dealer LIKE l_dealer AND
          extc   LIKE l_extc   AND
          intc   LIKE l_intc   AND
          req_date BETWEEN p_reqdate_st and p_reqdate_en .

  LOOP AT it_app101.
    CONCATENATE it_app101-wo_ser it_app101-nation it_app101-dealer
      INTO it_app101-workorder.
    MODIFY it_app101 INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  process_download
*&---------------------------------------------------------------------*
*       Process of downloading
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_download.
  CLEAR: it_excel, it_excel[].
  MOVE 'WORK ORDER SERIAL' TO it_excel-col01.
  MOVE 'NATION' TO it_excel-col02.
  MOVE 'DEALER' TO it_excel-col03.
  MOVE 'EXTERNAL COLOR' TO it_excel-col04.
  MOVE 'INTERNAL COLOR' TO it_excel-col05.
  MOVE 'MODEL YEAR' TO it_excel-col06.
  MOVE 'MODEL INDEX' TO it_excel-col07.
  MOVE 'OCN' TO it_excel-col08.
  MOVE 'VERSION' TO it_excel-col09.
  MOVE 'INITIAL ORDER QUANTITY' TO it_excel-col10.
  MOVE 'MODIFICATED ORDER QUANTITY' TO it_excel-col11.
  MOVE 'REQUESTED DATE' TO it_excel-col12.
  MOVE 'CREATED DATE' TO it_excel-col13.
  MOVE 'CHANGED DATE' TO it_excel-col14.
  MOVE 'DESTINATION COUNTRY' TO it_excel-col15.
  MOVE 'L/C COUNT' TO it_excel-col16.
  MOVE 'L/C No' TO it_excel-col17.
  MOVE 'REGION PORT' TO it_excel-col18.
  MOVE 'ORDER ZONE' TO it_excel-col19.
  MOVE '219 CODE' TO it_excel-col20.
  APPEND it_excel.
*
  LOOP AT it_app101.
    CLEAR it_excel.
    MOVE it_app101-wo_ser TO it_excel-col01    .
    MOVE it_app101-nation TO it_excel-col02    .
    MOVE it_app101-dealer TO it_excel-col03    .
    MOVE it_app101-extc TO it_excel-col04    .
    MOVE it_app101-intc TO it_excel-col05    .
    MOVE it_app101-moye TO it_excel-col06    .
    MOVE it_app101-bmdl TO it_excel-col07    .
    MOVE it_app101-ocnn TO it_excel-col08    .
    MOVE it_app101-vers TO it_excel-col09    .
    MOVE it_app101-initqty TO it_excel-col10    .
    MOVE it_app101-modqty TO it_excel-col11    .
    MOVE it_app101-req_date TO it_excel-col12    .
    MOVE it_app101-crt_date TO it_excel-col13    .
    MOVE it_app101-chg_date TO it_excel-col14    .
    MOVE it_app101-dest TO it_excel-col15    .
    MOVE it_app101-lcnt TO it_excel-col16    .
    MOVE it_app101-lcno TO it_excel-col17    .
    MOVE it_app101-regn TO it_excel-col18    .
    MOVE it_app101-orzn TO it_excel-col19    .
    MOVE it_app101-s219 TO it_excel-col20    .
    APPEND it_excel.

  ENDLOOP.
  PERFORM call_func_download.

ENDFORM.                    " process_download
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       Deletion of ZTPP_ALC_RERUN'S Data.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  LOOP AT it_app101 WHERE mark = 'X'.
    DELETE ztpp_alc_rerun FROM it_app101.
    DELETE it_app101.
  ENDLOOP.
ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_ascending.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP101'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app101 ASCENDING BY (field_name01).
  ENDIF.
*
ENDFORM.                    " sort_ascending
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_descending.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP101'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app101 DESCENDING BY (field_name01).
  ENDIF.
*
ENDFORM.                    " sort_descending
*&---------------------------------------------------------------------*
*&      Form  release_data
*&---------------------------------------------------------------------*
*       Running The Reprocess of Work Order Creation.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM release_data.
*
  DATA: l_modsum             LIKE ztpp_ksbohmm-modqty,
        l_initsum            LIKE ztpp_ksbohmm-initqty,
        l_material           LIKE mara-matnr           ,
        l_color              TYPE c       ,
        l_cm(15)             TYPE c       ,
        l_exist              LIKE sy-subrc.
  CLEAR: it_sys_message, it_sys_message[].
  PERFORM start_time_checking.
  LOOP AT it_app101 WHERE mark = 'X' .
*    PERFORM conversion_space_to_hypen  USING it_app101-s219.
*    WRITE: / it_app101 .
    CLEAR: l_exist, it_bdcdata, it_bdcdata[], wa_error.
    PERFORM check_color      USING l_color.
    CASE  l_color .
      WHEN 'Y'    .         " WORK ORDER HEADER.
        CONCATENATE it_app101-wo_ser it_app101-nation it_app101-dealer
               INTO l_material .
        it_app101-wo = 'H' .
*>>>>>> THE FIRST MISSION.
*       CREATE A CLASSIFICATION.
        PERFORM create_classification_mm   USING l_material
                                                 it_app101-wo.
*>>>>>> CHECK IF THERE IS A MATERIAL NUMBER IN MARA TABLE.
        PERFORM check_material USING l_exist  l_material .

*>>>>>> I THINK THAT L_EXIST VLAUE MUST BE '0'.
*>>>>>> WHAT DO YOU THINK OF MY OPINION ?
        CASE l_exist.
          WHEN 0    .         "  EXIST
*>>>>>>>>>> THE SECOND MISSION.
*           UPDATE BASIC 2 FOR 'P_PERF_YN' FIELD. <--- T_CODE : MM02
            PERFORM bdc_material_update USING l_material 'WOHD' .
            PERFORM check_bdc_result USING 'UPDATE' 'WOHD' l_material.
*>>>>>>>>>> THE THIRD MISSION.
*           UPDATE THE CLASSIFICATION FOR ALC U 200 - P_MOD_DATE.
            PERFORM update_classification_mm USING l_material.
*>>>>>>>> THERE SHOULD BE THE PROCESS FOR ERROR .
*>>>>>>>> USUALLY, IT MAY NOT WORK.
          WHEN OTHERS.        "  NO DATA
*>>>>>>>>>> CREATE BASIC 2. <--- T_CODE : MM01
            CLEAR: l_cm.
            CONCATENATE it_app101-bmdl(3) '_' 'WOHD' INTO l_cm .
            PERFORM bdc_material_create USING l_material 'WOHD' l_cm .
            PERFORM check_bdc_result USING 'CREATE' 'WOHD' l_material.
        ENDCASE.
      WHEN 'N'    .         " WORK ORDER COLOR .
        CONCATENATE it_app101-wo_ser it_app101-nation it_app101-dealer
                    it_app101-extc   it_app101-intc   INTO l_material .
        it_app101-wo = 'C' .
*>>>>>> THE FIRST MISSION.
*       CREATE A CLASSIFICATION.
        PERFORM create_classification_mm   USING l_material
                                                 it_app101-wo.
*>>>>>> CHECK IF THERE IS A MATERIAL NUMBER IN MARA TABLE.
        PERFORM check_material USING l_exist  l_material.

*>>>>>> I THINK THAT L_EXIST VLAUE MUST BE '0'.
        CASE l_exist.
          WHEN 0    .         "  EXIST
            WRITE AT: /001(018)  l_material,
                       020(050) '(WOCL) is now UPDATE.....' .
*>>>>>>>>>> THE SECOND MISSION.
*           UPDATE BASIC 2 FOR 'P_PERF_YN' FIELD. <--- T_CODE : MM02
            PERFORM bdc_material_update USING l_material 'WOCL' .
            PERFORM check_bdc_result USING 'UPDATE' 'WOCL' l_material.
*>>>>>>>>>> THE THIRD MISSION.
*           UPDATE THE CLASSIFICATION FOR ALC U 200 - P_MOD_DATE.
            PERFORM update_classification_mm USING l_material.
*>>>>>>>>>> UPDATE ztpp_wosum TABLE.
            PERFORM update_wosum        USING l_material .

*>>>>>>>> THERE SHOULD BE THE PROCESS FOR ERROR .
*>>>>>>>> USUALLY, IT MAY NOT WORK.
          WHEN OTHERS.        "  NO DATA
            WRITE AT: /001(018)  l_material,
                       020(050) '(WOCL) is now CREATE.....' .
*>>>>>>>>>> CREATE BASIC 2.                <--- T_CODE : MM01
            CLEAR: l_cm.
            CONCATENATE it_app101-bmdl(3) '_' 'WOCL' INTO l_cm .
            PERFORM bdc_material_create USING l_material 'WOCL' l_cm .
            PERFORM check_bdc_result USING 'CREATE' 'WOCL' l_material.
*>>>>>>>>>> INSERT ztpp_wosum TABLE.
            PERFORM insert_wosum        USING l_material        .
        ENDCASE.
    ENDCASE.
    MODIFY it_app101.

  ENDLOOP.

**  COMMIT WORK AND WAIT .
**  WAIT UP TO 3 SECONDS .

* THE FORTH MISSION.
* CHECK TABLE ztpp_wosum FOR SD CALL.
  LOOP AT it_app101.
    SELECT SINGLE *
      FROM ztpp_wosum
      WHERE wo_ser = it_app101-wo_ser AND
            nation = it_app101-nation AND
            dealer = it_app101-dealer AND
            extc   = it_app101-extc   AND
            intc   = it_app101-intc .
    IF sy-subrc = 0.
      IF ztpp_wosum-sales = space.
*       RUN SD CALL.
        PERFORM run_sd_call.
        EXIT.
      ENDIF.
    ELSE.
*     RUN SD CALL.
      PERFORM run_sd_call.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " release_data
*&---------------------------------------------------------------------*
*&      Form  conversion_space_to_hypen
*&---------------------------------------------------------------------*
*       Modification of 219 Code from Legacy Sys.
*----------------------------------------------------------------------*
*      -->P_IT_APP101_S219  text
*----------------------------------------------------------------------*
FORM conversion_space_to_hypen USING    pa_s219.
  DO 219 TIMES.
    REPLACE ' '  WITH '-' INTO pa_s219 .
    IF sy-subrc = 4 .
      EXIT .
    ENDIF.
  ENDDO.
ENDFORM.                    " CONVERSION_SPACE_TO_HYPEN

*&---------------------------------------------------------------------*
*&      Form  check_color
*&---------------------------------------------------------------------*
*       The Data Definition(Work Order Color or Header)
*----------------------------------------------------------------------*
*      -->P_L_COLOR  text
*----------------------------------------------------------------------*
FORM check_color USING    pa_color.
  pa_color = 'N'  .
  IF it_app101-extc = '***' AND it_app101-intc = '***'.
    pa_color = 'Y' .
  ENDIF.
ENDFORM.                    " check_color

*&---------------------------------------------------------------------*
*&      Form  check_material
*&---------------------------------------------------------------------*
*       Checking a Material by MARA
*----------------------------------------------------------------------*
*      -->P_L_EXIST  text
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM check_material USING    pa_exist  pa_material .
  SELECT SINGLE matnr INTO pa_material
    FROM mara
   WHERE matnr = pa_material .

  pa_exist = sy-subrc .
ENDFORM.                    " CHECK_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  bdc_material_update
*&---------------------------------------------------------------------*
*       Running BDC For Updating Material Master
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_0369   text
*----------------------------------------------------------------------*
FORM bdc_material_update  USING  pa_material  pa_cm   .
*
  PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '0060',
                         ' '  'BDC_OKCODE'           '=AUSW' ,
                         ' '  'RMMG1-MATNR'           pa_material,

                         'X'  'SAPLMGMM'             '0070',
                         ' '  'BDC_OKCODE'           '=RESA' ,

                         'X'  'SAPLMGMM'             '0070',
                         ' '  'BDC_OKCODE'           '=ENTR' ,
                         ' '  'MSICHTAUSW-KZSEL(02)' 'X'     ,

                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=PB21' ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'      'P_PERF_YN',
                         ' '  'RCTMS-MWERT(01)'      ' '  ,
*                         ' '  'RCTMS-MNAME(02)'      'P_MOD_QTY',
*                         ' '  'RCTMS-MWERT(02)'    IT_APP101-MODQTY,
                         ' '  'RCTMS-MNAME(03)'      'P_MOD_DATE',
                         ' '  'RCTMS-MWERT(03)'       sy-datum  .
*                         ' '  'RCTMS-MNAME(04)'      'P_WO_MODI_DATE',
*                         ' '  'RCTMS-MWERT(04)'    IT_APP101-chg_date.

*  IF pa_cm = 'WOCL' .
*    PERFORM bdc_dynpro_processing USING :
*                          ' '  'RCTMS-MNAME(05)'      'P_ORDER_ZONE',
*                          ' '  'RCTMS-MWERT(05)'       IT_APP101-orzn .
*  ELSE.
*    PERFORM bdc_dynpro_processing USING :
*                          ' '  'RCTMS-MNAME(05)'      'P_LC_COUNT'  ,
*                          ' '  'RCTMS-MWERT(05)'       IT_APP101-lcnt ,
*                          ' '  'RCTMS-MNAME(06)'      'P_REGION_PORT',
*                          ' '  'RCTMS-MWERT(06)'       IT_APP101-regn .
*  ENDIF.

  PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=BU' .

  CALL TRANSACTION 'MM02'  USING it_bdcdata           MODE wa_mode
                           MESSAGES INTO              it_msg .
ENDFORM.                    " bdc_material_update

*&---------------------------------------------------------------------*
*&      Form  bdc_material_create
*&---------------------------------------------------------------------*
*       Running BDC For Creating Material Master
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_0392   text
*      -->P_L_CM  text
*----------------------------------------------------------------------*
FORM bdc_material_create   USING  pa_material  pa_type  pa_cm .
  DATA: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20),
        l_no(3)              TYPE n  VALUE 9 ,
        l_matnr              LIKE mara-matnr ,
        l_class(20)          TYPE c,
        l_qty                LIKE ztpp_wosum-seqqty .

  l_qty = it_app101-modqty.

  SELECT SINGLE matnr INTO l_matnr
    FROM mara
   WHERE matnr = pa_cm .

  IF sy-subrc NE 0.
*   PERFORM create_error_log USING 'MARA'.
    WRITE AT: /001(029) 'Master Data does not exist.. ',
               031(020) pa_cm .
    wa_error = 'X'.
    EXIT .
  ENDIF.

  CHECK wa_error IS INITIAL .

  PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '0060',
                         ' '  'BDC_OKCODE'           '=AUSW' ,
                         ' '  'RMMG1-MATNR'           pa_material,
                         ' '  'RMMG1-MBRSH'          'A'         ,
                         ' '  'RMMG1-MTART'           pa_type    ,

                         'X'  'SAPLMGMM'             '0070',
                         ' '  'BDC_OKCODE'           '=RESA' ,

                         'X'  'SAPLMGMM'             '0070',
                         ' '  'BDC_OKCODE'           '=ENTR' ,
                         ' '  'MSICHTAUSW-KZSEL(01)' 'X'     ,
                         ' '  'MSICHTAUSW-KZSEL(02)' 'X'     ,
                         ' '  'MSICHTAUSW-KZSEL(03)' 'X'     ,


                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=SP02' ,
                         ' '  'MAKT-MAKTX'            pa_material ,
                         ' '  'MARA-MEINS'           'EA'         ,

                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=PB21' ,
                         ' '  'MARA-SATNR'            pa_cm  ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
*                        ' '  'RCTMS-MNAME(01)'      'P_MOD_QTY',
*                         ' '  'RCTMS-MWERT(01)'       l_qty     ,
*                         ' '  'RCTMS-MNAME(02)'  'P_WO_MODI_DATE',
*                         ' '  'RCTMS-MWERT(02)'   IT_APP101-chg_date,
*                         ' '  'RCTMS-MNAME(03)'  'P_INIT_QTY',
*                         ' '  'RCTMS-MWERT(03)'   l_qty     ,
*                         ' '  'RCTMS-MNAME(04)'  'P_WO_CREATE_DATE',
*                         ' '  'RCTMS-MWERT(04)'   IT_APP101-chg_date,
                         ' '  'RCTMS-MNAME(05)'    'P_WO_SER',
                         ' '  'RCTMS-MWERT(05)'    it_app101-wo_ser   ,
                         ' '  'RCTMS-MNAME(06)'    'P_NATION',
                         ' '  'RCTMS-MWERT(06)'    it_app101-nation   ,
                         ' '  'RCTMS-MNAME(07)'    'P_DEALER',
                         ' '  'RCTMS-MWERT(07)'    it_app101-dealer   ,
                         ' '  'RCTMS-MNAME(08)'    'P_MODEL_YEAR',
                         ' '  'RCTMS-MWERT(08)'    it_app101-moye     ,

                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=PB21' ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'      'P_MI'     ,
                         ' '  'RCTMS-MWERT(01)'    it_app101-bmdl,
                         ' '  'RCTMS-MNAME(02)'      'P_OCN'         ,
                         ' '  'RCTMS-MWERT(02)'    it_app101-ocnn   ,
                         ' '  'RCTMS-MNAME(03)'      'P_VERSION' ,
                         ' '  'RCTMS-MWERT(03)'    it_app101-vers   ,
                         ' '  'RCTMS-MNAME(04)'      'P_MODEL'       ,
                         ' '  'RCTMS-MWERT(04)'    it_app101-bmdl(3),
*                        ' '  'RCTMS-MNAME(05)'    'P_DESTINATION_CODE',
*                        ' '  'RCTMS-MWERT(05)'    IT_APP101-dest     ,
                         ' '  'RCTMS-MNAME(06)'      'P_GEN_DATE'      ,
                         ' '  'RCTMS-MWERT(06)'       sy-datum         .

  IF pa_type = 'WOCL' .
    PERFORM bdc_dynpro_processing USING :
*                         ' '  'RCTMS-MNAME(07)'      'P_ORDER_ZONE',
*                         ' '  'RCTMS-MWERT(07)'       IT_APP101-orzn ,
                          ' '  'RCTMS-MNAME(08)'      'COLOREXT',
                          ' '  'RCTMS-MWERT(08)'    it_app101-extc   ,
                          ' '  'RCTMS-MNAME(09)'      'COLORINT'    ,
                          ' '  'RCTMS-MWERT(09)'    it_app101-intc  .
*  ELSE.
*    PERFORM bdc_dynpro_processing USING :
*                          ' '  'RCTMS-MNAME(07)'      'P_LC_COUNT'  ,
*                          ' '  'RCTMS-MWERT(07)'       IT_APP101-lcnt ,
*                          ' '  'RCTMS-MNAME(08)'      'P_REGION_PORT',
*                          ' '  'RCTMS-MWERT(08)'       IT_APP101-regn .
  ENDIF.

  PERFORM bdc_dynpro_processing USING :
                        'X'  'SAPLMGMM'             '4004',
                        ' '  'BDC_OKCODE'           '=PB21' ,

                        'X'  'SAPLCEI0'             '0109',
                        ' '  'BDC_OKCODE'           '=BACK' ,
                        ' '  'RCTMS-MNAME(01)'      'P_219_1',
                        ' '  'RCTMS-MWERT(01)'    it_app101-s219+0(1),
                        ' '  'RCTMS-MNAME(02)'      'P_219_2',
                        ' '  'RCTMS-MWERT(02)'    it_app101-s219+1(1),
                        ' '  'RCTMS-MNAME(03)'      'P_219_3',
                        ' '  'RCTMS-MWERT(03)'    it_app101-s219+2(1),
                        ' '  'RCTMS-MNAME(04)'      'P_219_4',
                        ' '  'RCTMS-MWERT(04)'    it_app101-s219+3(1),
                        ' '  'RCTMS-MNAME(05)'      'P_219_5',
                        ' '  'RCTMS-MWERT(05)'    it_app101-s219+4(1),
                        ' '  'RCTMS-MNAME(06)'      'P_219_6',
                        ' '  'RCTMS-MWERT(06)'    it_app101-s219+5(1),
                        ' '  'RCTMS-MNAME(07)'      'P_219_7',
                        ' '  'RCTMS-MWERT(07)'    it_app101-s219+6(1),
                        ' '  'RCTMS-MNAME(08)'      'P_219_8',
                        ' '  'RCTMS-MWERT(08)'    it_app101-s219+7(1),
                        ' '  'RCTMS-MNAME(09)'      'P_219_9',
                        ' '  'RCTMS-MWERT(09)'    it_app101-s219+8(1).

  DO  9 TIMES.
    l_vals1 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name1 .
    l_vals2 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name2 .
    l_vals3 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name3 .
    l_vals4 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name4 .
    l_vals5 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name5 .
    l_vals6 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name6 .
    l_vals7 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name7 .
    l_vals8 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name8 .
    l_vals9 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name9 .
    l_vals0 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name0 .

    PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=PB21' ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'       l_name1,
                         ' '  'RCTMS-MWERT(01)'       l_vals1,
                         ' '  'RCTMS-MNAME(02)'       l_name2,
                         ' '  'RCTMS-MWERT(02)'       l_vals2,
                         ' '  'RCTMS-MNAME(03)'       l_name3,
                         ' '  'RCTMS-MWERT(03)'       l_vals3,
                         ' '  'RCTMS-MNAME(04)'       l_name4,
                         ' '  'RCTMS-MWERT(04)'       l_vals4,
                         ' '  'RCTMS-MNAME(05)'       l_name5,
                         ' '  'RCTMS-MWERT(05)'       l_vals5,
                         ' '  'RCTMS-MNAME(06)'       l_name6,
                         ' '  'RCTMS-MWERT(06)'       l_vals6,
                         ' '  'RCTMS-MNAME(07)'       l_name7,
                         ' '  'RCTMS-MWERT(07)'       l_vals7,
                         ' '  'RCTMS-MNAME(08)'       l_name8,
                         ' '  'RCTMS-MWERT(08)'       l_vals8,
                         ' '  'RCTMS-MNAME(09)'       l_name9,
                         ' '  'RCTMS-MWERT(09)'       l_vals9,
                         ' '  'RCTMS-MNAME(10)'       l_name0,
                         ' '  'RCTMS-MWERT(10)'       l_vals0.
  ENDDO.

  DO 12 TIMES.
    l_vals1 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name1 .
    l_vals2 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name2 .
    l_vals3 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name3 .
    l_vals4 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name4 .
    l_vals5 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name5 .
    l_vals6 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name6 .
    l_vals7 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name7 .
    l_vals8 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name8 .
    l_vals9 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name9 .
    l_vals0 = it_app101-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name0 .

    PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=PB21' ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'       l_name1,
                         ' '  'RCTMS-MWERT(01)'       l_vals1,
                         ' '  'RCTMS-MNAME(02)'       l_name2,
                         ' '  'RCTMS-MWERT(02)'       l_vals2,
                         ' '  'RCTMS-MNAME(03)'       l_name3,
                         ' '  'RCTMS-MWERT(03)'       l_vals3,
                         ' '  'RCTMS-MNAME(04)'       l_name4,
                         ' '  'RCTMS-MWERT(04)'       l_vals4,
                         ' '  'RCTMS-MNAME(05)'       l_name5,
                         ' '  'RCTMS-MWERT(05)'       l_vals5,
                         ' '  'RCTMS-MNAME(06)'       l_name6,
                         ' '  'RCTMS-MWERT(06)'       l_vals6,
                         ' '  'RCTMS-MNAME(07)'       l_name7,
                         ' '  'RCTMS-MWERT(07)'       l_vals7,
                         ' '  'RCTMS-MNAME(08)'       l_name8,
                         ' '  'RCTMS-MWERT(08)'       l_vals8,
                         ' '  'RCTMS-MNAME(09)'       l_name9,
                         ' '  'RCTMS-MWERT(09)'       l_vals9,
                         ' '  'RCTMS-MNAME(10)'       l_name0,
                         ' '  'RCTMS-MWERT(10)'       l_vals0.
  ENDDO.

  CONCATENATE  'P_'  pa_type  '_001'                  INTO  l_class.

  PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '4004',
                         ' '  'BDC_OKCODE'           '=SP03' ,

                         'X'  'SAPLCLCA'             '0602',
                         ' '  'BDC_OKCODE'           '=ENTE' ,
                         ' '  'RMCLF-KLART'          '001'   ,

                         'X'  'SAPLCLFM'             '0500',
                         ' '  'BDC_OKCODE'           '=AUSW' ,
                         ' '  'RMCLF-KREUZ(01)'      'X'     ,
                         ' '  'RMCLF-CLASS(01)'       l_class,

                         'X'  'SAPLCTMS'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,

                         'X'  'SAPLCLFM'             '0500',
                         ' '  'BDC_OKCODE'           '=SAVE'.

  CALL TRANSACTION 'MM01'  USING it_bdcdata           MODE wa_mode
                           MESSAGES INTO              it_msg .
ENDFORM.                    " bdc_material_create
*&---------------------------------------------------------------------*
*&      Form  check_bdc_result
*&---------------------------------------------------------------------*
*       Checking The Result of BDC Processes.
*----------------------------------------------------------------------*
*      -->P_0397   text
*      -->P_0398   text
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM check_bdc_result USING    pa_mode  pa_type  pa_material.
  DATA: l_msg(100)           TYPE c.

  IF wa_error NE ' '     .
    CLEAR: it_msg, it_msg[].
    EXIT .
  ENDIF.

  it_app101-p_flag = 'X' .

  READ TABLE it_msg WITH KEY msgtyp = 'E' .

  CHECK sy-subrc = 0.
* WA_ERROR IS THE CHECK FLAG
* BY WHICH WE CAN FIND IF THERE IS A JOB IN TABLE ztpp_common_vals .
*  wa_error = 'E'    .
  CLEAR: it_app101-p_flag.
*  write at: /005(012) 'Error.. =>' ,
*             017(020) pa_material  ,
*             037(007) 'Mode = '    ,
*             044(010) pa_mode      ,
*             054(020) '  Work Order type is ',
*             074(010) pa_type      .
  CONCATENATE 'ERROR :'
               pa_material
              ' Mode = '
               pa_mode
              ' Work Order type is '
               pa_type
    INTO it_sys_message-text.
  APPEND it_sys_message.
  MESSAGE s000 WITH it_sys_message-text.

  LOOP AT it_msg  WHERE msgtyp = 'E' .
    PERFORM create_message USING l_msg  it_msg-msgid it_msg-msgnr
              it_msg-msgv1 it_msg-msgv2 it_msg-msgv3 it_msg-msgv4.
*    write at: /020(080) l_msg .
    MOVE l_msg TO it_sys_message-text.
    APPEND it_sys_message.
    MESSAGE s000 WITH it_sys_message-text.

  ENDLOOP.
  CLEAR: it_msg, it_msg[].
ENDFORM.                    " CHECK_BDC_RESULT
*&---------------------------------------------------------------------*
*&      Form  update_wosum
*&---------------------------------------------------------------------*
*       Updating ZTPP_WOSUM Table.
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM update_wosum  USING pa_material .
  DATA: l_udate               LIKE sy-datum.

  CHECK wa_error IS INITIAL  .
  l_udate = wa_date + 1      .
  " AEDAT Field is changed for the SD Module for the Testing...
  UPDATE ztpp_wosum  SET: womoddate = sy-datum
                          modqty    = it_app101-modqty
                          aedat     = sy-datum
                          aezet     = sy-uzeit
                          aenam     = sy-uname
                    WHERE wo_ser = pa_material(9)
                      AND nation = pa_material+9(3)
                      AND dealer = pa_material+12(2)
                      AND extc   = pa_material+14(2)
                      AND intc   = pa_material+16(2) .
*  write at: /001(50) ' Table ZTPP_WOSUM Data Updated.. ',
*            /001(30) '     The Update Result is ..',
*             031(10) sy-subrc.
  MESSAGE s001 WITH 'Table ZTPP_WOSUM Data Updated.. '.
ENDFORM.                    " UPDATE_WOSUM
*&---------------------------------------------------------------------*
*&      Form  insert_wosum
*&---------------------------------------------------------------------*
*       Inserting New Data into ZTPP_WOSUM.
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM insert_wosum  USING pa_material .
  DATA: l_fsc                 LIKE ztpp_wosum-fsc,
        l_idate               LIKE sy-datum.

  CHECK wa_error IS INITIAL  .
  l_idate = wa_date + 1      .

  PERFORM concate_fsc     USING l_fsc.
  CLEAR: ztpp_wosum.
  ztpp_wosum-wo_ser    = pa_material(9) .
  ztpp_wosum-nation    = pa_material+9(3).
  ztpp_wosum-dealer    = pa_material+12(2).
  ztpp_wosum-extc      = pa_material+14(2).
  ztpp_wosum-intc      = pa_material+16(2).
  ztpp_wosum-initqty   = it_app101-initqty  .
  ztpp_wosum-modqty    = it_app101-modqty   .
  ztpp_wosum-wocredate = wa_wkdate        .
  ztpp_wosum-womoddate = wa_wkdate        .
  ztpp_wosum-version   = it_app101-vers     .
  ztpp_wosum-fsc       = l_fsc            .
  ztpp_wosum-erdat     = sy-datum         .
  ztpp_wosum-erzet     = sy-uzeit         .
  ztpp_wosum-ernam     = sy-uname         .
  ztpp_wosum-aedat     = sy-datum         .
  ztpp_wosum-aezet     = sy-uzeit         .
  ztpp_wosum-aenam     = sy-uname         .
  INSERT ztpp_wosum .

*  WRITE AT: /001(50) ' Table ZTPP_WOSUM Data Created.. ',
*            /001(30) '     The Create Result is ..',
*             031(10) sy-subrc.
  DATA: l_subrc(04).
  WRITE sy-subrc TO l_subrc .
  CONCATENATE 'DATA OF TABLE ZTPP_WOSUM is created ...'
              ' '
              l_subrc
    INTO it_sys_message-text.
  APPEND it_sys_message.
  MESSAGE s000 WITH it_sys_message-text.

ENDFORM.                    " INSERT_WOSUM
*&---------------------------------------------------------------------*
*&      Form  bdc_wohd
*&---------------------------------------------------------------------*
*       Creating Work Order's Inf
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM bdc_wohd USING    pa_material.
  DATA: l_tables          LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_vin(17)         TYPE c ,
        l_return(3)       TYPE c ,
        l_material        LIKE mara-matnr,
        l_flag            LIKE ztpp_pmt07jb-zresult.

  CALL FUNCTION 'Z_FPP_HANDLE_CLASSIFICATION'
       EXPORTING
            matnr       = pa_material
            mtype       = 'H'
       IMPORTING
            return_flag = l_flag
       TABLES
            val_table   = l_tables.

  IF l_flag   = 'S' .
    WRITE AT: /001(50) ' Work Order Information Generated.. ',
              /001(30) '     The Result is ..',
               031(50) ' ** Success!!! **' .

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_123'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      l_vin = '---'.
*     PERFORM create_error_log USING 'P_VIN_123' .
    ELSE.
      l_vin = l_return.
    ENDIF.

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_4'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      CONCATENATE l_vin  '-'               INTO l_vin.
*     PERFORM create_error_log USING 'P_VIN_4' .
    ELSE.
      CONCATENATE l_vin  l_return          INTO l_vin.
    ENDIF.

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_5'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      CONCATENATE l_vin  '-'               INTO l_vin.
*     PERFORM create_error_log USING 'P_VIN_5'   .
    ELSE.
      CONCATENATE l_vin  l_return          INTO l_vin.
    ENDIF.

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_6'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      CONCATENATE l_vin  '-'               INTO l_vin.
*     PERFORM create_error_log USING 'P_VIN_6'   .
    ELSE.
      CONCATENATE l_vin  l_return          INTO l_vin.
    ENDIF.

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_7'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      CONCATENATE l_vin  '-'               INTO l_vin.
*     PERFORM create_error_log USING 'P_VIN_7'   .
    ELSE.
      CONCATENATE l_vin  l_return          INTO l_vin.
    ENDIF.

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_8'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      CONCATENATE l_vin  '--'              INTO l_vin.
*     PERFORM create_error_log USING 'P_VIN_8'   .
    ELSE.
      CONCATENATE l_vin  l_return  '-'     INTO l_vin.
    ENDIF.

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_10'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      CONCATENATE l_vin  '-'               INTO l_vin.
*     PERFORM create_error_log USING 'P_VIN_10'  .
    ELSE.
      CONCATENATE l_vin  l_return          INTO l_vin.
    ENDIF.

    CLEAR: l_return.
    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
         EXPORTING
              vtnam   = 'EMF_VIN_11'
              s219    = it_app101-s219
         IMPORTING
              return  = l_return
         EXCEPTIONS
              no_data = 1
              OTHERS  = 2.

    IF l_return IS INITIAL.
      CONCATENATE l_vin  '-'               INTO l_vin.
*     PERFORM create_error_log USING 'P_VIN_11'  .
    ELSE.
      CONCATENATE l_vin  l_return          INTO l_vin.
    ENDIF.

    CONCATENATE it_app101-wo_ser it_app101-nation it_app101-dealer
           INTO l_material .
    PERFORM call_bdc_wohd_vin  USING l_material  l_vin.
*   PERFORM check_bdc_result USING 'VIN CHANGE' 'WOHD' l_material.
  ELSE.
    CASE l_flag.
      WHEN 'L'.     " Instance not found...
        WRITE AT: /001(50) ' Work Order Information Generated.. ',
                  /001(30) '     The Result is ..',
                   031(50) ' ** Instance Not found..' .
      WHEN 'E'.     " Failure of the Classification Change...
        WRITE AT: /001(50) ' Work Order Information Generated.. ',
                  /001(30) '     The Result is ..',
                   031(90) ' ** Failure of the Classification Change..'.
    ENDCASE.
  ENDIF.
ENDFORM.                    " BDC_WOHD
*&---------------------------------------------------------------------*
*&      Form  bdc_wocl
*&---------------------------------------------------------------------*
*       Calling the Func. For Work Order's Classifications.
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM bdc_wocl USING    pa_material.
  DATA: l_flag                LIKE ztpp_pmt07jb-zresult.

  CALL FUNCTION 'Z_FPP_HANDLE_CLASSIFICATION'
       EXPORTING
            matnr       = pa_material
            mtype       = 'C'
       IMPORTING
            return_flag = l_flag.

  WRITE AT: /001(50) ' Work Order Information Generated.. ',
            /001(30) '     The Update Result is ..',
             031(10) l_flag  .
ENDFORM.                    " BDC_WOCL
*&---------------------------------------------------------------------*
*&      Form  create_classification_mm
*&---------------------------------------------------------------------*
*       Creation of W/O's Classifications.
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_IT_APP101_WO  text
*----------------------------------------------------------------------*
FORM create_classification_mm USING    pa_material  pa_wo.
  DATA: l_variable           LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE,
        l_conf               LIKE TABLE OF conf_out    WITH HEADER LINE,
        l_instance           LIKE mara-cuobf,
        l_no(3)              TYPE n,
        l_of(3)              TYPE n,
        l_name(30)           TYPE c.

  CLEAR: l_variable, l_variable[], l_conf, l_conf[].
  SELECT SINGLE cuobf INTO l_instance
    FROM mara
   WHERE matnr = pa_material.

  CALL FUNCTION 'VC_I_GET_CONFIGURATION_IBASE'
       EXPORTING
            instance           = l_instance
       TABLES
            configuration      = l_conf
       EXCEPTIONS
            instance_not_found = 1
            OTHERS             = 2.

  IF sy-subrc <> 0 .
    CONCATENATE 'Function Calling Error ... ' pa_material
      INTO it_sys_message-text .
    APPEND it_sys_message.
    MESSAGE s000 WITH it_sys_message-text .
    EXIT.

  ENDIF.

  " ALC Code ------> Classification.
  IF pa_wo = 'H'.
    CLEAR: l_no.
    DO 9 TIMES .
      l_no = l_no + 1.     l_of = l_no - 1 .
      CONCATENATE 'P_ALC_U_' l_no+2(1)  INTO  l_name  .
      l_variable-atnam = l_name.
      READ TABLE l_conf WITH KEY atnam = l_name .
      IF sy-subrc = 0.
        l_variable-atwrt = l_conf-atwrt .          APPEND l_variable .
      ENDIF.
    ENDDO.

    DO 90 TIMES.
      l_no = l_no + 1.     l_of = l_no - 1 .
      CONCATENATE 'P_ALC_U_' l_no+1(2)  INTO  l_name  .
      l_variable-atnam = l_name.
      READ TABLE l_conf WITH KEY atnam = l_name .
      IF sy-subrc = 0.
        l_variable-atwrt = l_conf-atwrt .          APPEND l_variable .
      ENDIF.
    ENDDO.

    DO 101 TIMES.
      l_no = l_no + 1.     l_of = l_no - 1 .
      CONCATENATE 'P_ALC_U_' l_no    INTO  l_name  .
      l_variable-atnam = l_name.
      READ TABLE l_conf WITH KEY atnam = l_name .
      IF sy-subrc = 0.
        l_variable-atwrt = l_conf-atwrt .          APPEND l_variable .
      ENDIF.
    ENDDO.
  ELSE.
    CLEAR: l_no.
    DO 9 TIMES .
      l_no = l_no + 1.     l_of = l_no - 1 .
      CONCATENATE 'P_ALC_C_' l_no    INTO  l_name  .
      l_variable-atnam = l_name.
      READ TABLE l_conf WITH KEY atnam = l_name .
      IF sy-subrc = 0.
        l_variable-atwrt = l_conf-atwrt .          APPEND l_variable .
      ENDIF.
    ENDDO.

    DO 41 TIMES.
      l_no = l_no + 1.     l_of = l_no - 1 .
      CONCATENATE 'P_ALC_C_' l_no    INTO  l_name  .
      l_variable-atnam = l_name.
      READ TABLE l_conf WITH KEY atnam = l_name .
      IF sy-subrc = 0.
        l_variable-atwrt = l_conf-atwrt .          APPEND l_variable .
      ENDIF.
    ENDDO.
  ENDIF.

  l_variable-atnam = 'P_INIT_QTY'       .
  l_variable-atwrt =  it_app101-initqty   .   APPEND l_variable .
  l_variable-atnam = 'P_MOD_QTY'        .
  l_variable-atwrt =  it_app101-modqty    .   APPEND l_variable .
  l_variable-atnam = 'P_WO_SER'         .
  l_variable-atwrt =  it_app101-wo_ser    .   APPEND l_variable .
  l_variable-atnam = 'P_NATION'         .
  l_variable-atwrt =  it_app101-nation    .   APPEND l_variable .
  l_variable-atnam = 'P_DEALER'         .
  l_variable-atwrt =  it_app101-dealer    .   APPEND l_variable .

  IF it_app101-wo = 'C' .
    l_variable-atnam = 'COLOREXT'       .
    l_variable-atwrt =  it_app101-extc    .     APPEND l_variable .
    l_variable-atnam = 'COLORINT'       .
    l_variable-atwrt =  it_app101-intc    .     APPEND l_variable .
    l_variable-atnam = 'P_ORDER_ZONE'   .
    l_variable-atwrt =  it_app101-orzn    .     APPEND l_variable .
  ELSE.
    l_variable-atnam = 'P_LC_NO'        .
    l_variable-atwrt =  it_app101-lcno    .     APPEND l_variable .
    l_variable-atnam = 'P_LC_COUNT'     .
    l_variable-atwrt =  it_app101-lcnt    .     APPEND l_variable .
    l_variable-atnam = 'P_REGION_PORT'  .
    l_variable-atwrt =  it_app101-regn    .     APPEND l_variable .
  ENDIF.

  l_variable-atnam = 'P_MODEL_YEAR'   .
  l_variable-atwrt =  it_app101-moye    .     APPEND l_variable .
  l_variable-atnam = 'P_MI'           .
  l_variable-atwrt =  it_app101-bmdl    .     APPEND l_variable .
  l_variable-atnam = 'P_OCN'          .
  l_variable-atwrt =  it_app101-ocnn    .     APPEND l_variable .
  l_variable-atnam = 'P_VERSION'      .
  l_variable-atwrt =  it_app101-vers    .     APPEND l_variable .
  l_variable-atnam = 'P_MODEL'        .
  l_variable-atwrt =  it_app101-bmdl(3) .     APPEND l_variable .
  l_variable-atnam = 'P_DESTINATION_CODE'.
  l_variable-atwrt =  it_app101-dest    .     APPEND l_variable .
*   l_variable-atnam = 'P_GEN_DATE'     .
*   l_variable-atwrt =  IT_APP101-BMDL    .     APPEND l_variable .
  l_variable-atnam = 'P_MI'           .
  l_variable-atwrt =  it_app101-bmdl    .     APPEND l_variable .
  l_variable-atnam = 'P_WO_MODI_DATE' .
  l_variable-atwrt =  it_app101-chg_date.     APPEND l_variable .
  l_variable-atnam = 'P_WO_CREATE_DATE'.
  l_variable-atwrt =  it_app101-chg_date.     APPEND l_variable .

  CLEAR: l_no.
  DO 9 TIMES .
    l_no = l_no + 1.     l_of = l_no - 1 .
    CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name  .
    l_variable-atnam = l_name.
    l_variable-atwrt = it_app101-s219+l_of(1) .  APPEND l_variable .
  ENDDO.

  DO 90 TIMES.
    l_no = l_no + 1.     l_of = l_no - 1 .
    CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name  .
    l_variable-atnam = l_name.
    l_variable-atwrt = it_app101-s219+l_of(1) .  APPEND l_variable .
  ENDDO.

  DO 120 TIMES.
    l_no = l_no + 1.     l_of = l_no - 1 .
    CONCATENATE 'P_219_' l_no       INTO  l_name  .
    l_variable-atnam = l_name.
    l_variable-atwrt = it_app101-s219+l_of(1) .  APPEND l_variable .
  ENDDO.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = l_variable
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

  IF sy-subrc = 0.
    LOOP AT l_variable WHERE zflag = 'E' .
*      WRITE AT: /005(034) 'Charac. Value UPDATE Error => '     ,
*                 039(020)  pa_material                         ,
*                 059(030)  l_variable-atnam                    .
*      wa_error = 'X'.
      CONCATENATE 'Charac. Value UPDATE ERROR '
                   pa_material
                  ':'
                   l_variable-atnam
        INTO it_sys_message-text.
      APPEND it_sys_message .
      MESSAGE s000 WITH it_sys_message-text.

    ENDLOOP.
  ELSE.
*    WRITE AT: /005(034) 'Function Execution Error ==>  '     ,
*               039(022) 'Z_FPP_HANDLING_MASTER'              ,
*               063(030)  sy-subrc                            .
*    wa_error = 'X' .
    DATA l_subrc(04).
    WRITE sy-subrc TO l_subrc.
    CONCATENATE 'Function Execution ERROR '
                ':'
                'Z_FPP_HANDLING_MASTER'
                 l_subrc
      INTO it_sys_message-text.
    APPEND it_sys_message.
    MESSAGE s000 WITH it_sys_message-text.

  ENDIF.
ENDFORM.                    " create_classification_mm
*&---------------------------------------------------------------------*
*&      Form  create_message
*&---------------------------------------------------------------------*
*       Calling a Func. for Messages.
*----------------------------------------------------------------------*
*      -->P_L_MSG  text
*      -->P_IT_MSG_MSGID  text
*      -->P_IT_MSG_MSGNR  text
*      -->P_IT_MSG_MSGV1  text
*      -->P_IT_MSG_MSGV2  text
*      -->P_IT_MSG_MSGV3  text
*      -->P_IT_MSG_MSGV4  text
*----------------------------------------------------------------------*
FORM create_message  USING p_text  pid  pnr  p01  p02  p03  p04 .
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = pid
            msgnr               = pnr
            msgv1               = p01
            msgv2               = p02
            msgv3               = p03
            msgv4               = p04
       IMPORTING
            message_text_output = p_text.
ENDFORM.                    " CREATE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  concate_fsc
*&---------------------------------------------------------------------*
*       Concatenation of FSC.
*----------------------------------------------------------------------*
*      -->P_L_FSC  text
*----------------------------------------------------------------------*
FORM concate_fsc USING    pa_fsc.
  CLEAR: pa_fsc .
  CONCATENATE it_app101-moye
              it_app101-nation
              it_app101-dealer
              it_app101-bmdl
    INTO pa_fsc .
  CONCATENATE pa_fsc it_app101-ocnn
    INTO pa_fsc SEPARATED BY space .

ENDFORM.                    " concate_fsc
*&---------------------------------------------------------------------*
*&      Form  call_bdc_wohd_vin
*&---------------------------------------------------------------------*
*       Calling a Func For Creating Vin Spec.
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_VIN  text
*----------------------------------------------------------------------*
FORM call_bdc_wohd_vin USING    pa_material   pa_vin.
  DATA: l_variable       LIKE TABLE OF zspp_vin_value  WITH HEADER LINE.

  CLEAR: l_variable, l_variable[],
         it_bdcdata, it_bdcdata[], it_msg,  it_msg[].

  l_variable-atnam = 'P_VIN_SPEC'     .
  l_variable-atwrt =  pa_vin          .     APPEND l_variable .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = l_variable
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

  IF sy-subrc = 0.
    LOOP AT l_variable WHERE zflag = 'E' .
      WRITE AT: /005(034) 'Charac. Value UPDATE Error => '     ,
                 039(020)  pa_material                         ,
                 059(030)  l_variable-atnam                    .
      wa_error = 'X' .
    ENDLOOP.
  ELSE.
    WRITE AT: /005(034) 'Function Execution Error ==>  '     ,
               039(022) 'Z_FPP_HANDLING_MASTER'              ,
               063(030)  sy-subrc                            .
    wa_error = 'X' .
  ENDIF.

*  PERFORM bdc_dynpro_processing USING :
*                         'X'  'SAPLMGMM'             '0060',
*                         ' '  'BDC_OKCODE'           '=AUSW' ,
*                         ' '  'RMMG1-MATNR'           pa_material,
*
*                         'X'  'SAPLMGMM'             '0070',
*                         ' '  'BDC_OKCODE'           '=RESA' ,
*
*                         'X'  'SAPLMGMM'             '0070',
*                         ' '  'BDC_OKCODE'           '=ENTR' ,
*                         ' '  'MSICHTAUSW-KZSEL(02)' 'X'     ,
*
*                         'X'  'SAPLMGMM'             '4004',
*                         ' '  'BDC_OKCODE'           '=PB21' ,
*
*                         'X'  'SAPLCEI0'             '0109',
*                         ' '  'BDC_OKCODE'           '=BACK' ,
*                         ' '  'RCTMS-MNAME(01)'      'P_VIN_SPEC',
*                         ' '  'RCTMS-MWERT(01)'       pa_vin    ,
*
*                         'X'  'SAPLMGMM'             '4004',
*                         ' '  'BDC_OKCODE'           '=BU' .
*
*  CALL TRANSACTION 'MM02'  USING it_bdcdata           MODE wa_mode
*                           MESSAGES INTO              it_msg .
ENDFORM.                    " CALL_BDC_WOHD_VIN
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro_processing
*&---------------------------------------------------------------------*
*       Setting Data For BDC
*----------------------------------------------------------------------*
*      -->P_0945   text
*      -->P_0946   text
*      -->P_0947   text
*----------------------------------------------------------------------*
FORM bdc_dynpro_processing USING    dy_begin  pg_name   sc_no.
  IF dy_begin = 'X'.
    CLEAR it_bdcdata.
    MOVE  pg_name  TO it_bdcdata-program.
    MOVE  sc_no    TO it_bdcdata-dynpro.
    MOVE  'X'      TO it_bdcdata-dynbegin.
    APPEND it_bdcdata.
  ELSE.
    CLEAR it_bdcdata.
    MOVE  pg_name  TO it_bdcdata-fnam.
    MOVE  sc_no    TO it_bdcdata-fval.
    APPEND it_bdcdata.
  ENDIF.
ENDFORM.                    " BDC_DYNPRO_PROCESSING.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CLASSIFICATION_MM
*&---------------------------------------------------------------------*
*       Calling Func. For Updating W/O's Classifications.
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_IT_APP101_W  text
*----------------------------------------------------------------------*
FORM update_classification_mm USING    pa_material.
  DATA: l_variable           LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE,
        l_conf               LIKE TABLE OF conf_out    WITH HEADER LINE,
        l_instance           LIKE mara-cuobf,
        l_no(3)              TYPE n,
        l_of(3)              TYPE n,
        l_name(30)           TYPE c.

  CLEAR: l_variable, l_variable[], l_conf, l_conf[].
  SELECT SINGLE cuobf INTO l_instance
    FROM mara
   WHERE matnr = pa_material.

  CALL FUNCTION 'VC_I_GET_CONFIGURATION_IBASE'
       EXPORTING
            instance           = l_instance
       TABLES
            configuration      = l_conf
       EXCEPTIONS
            instance_not_found = 1
            OTHERS             = 2.

  IF sy-subrc <> 0 .
*    write at: /005(040) 'Function Calling Error....              ',
*               045(020) pa_material                               ,
*               065(020) sy-subrc                                  .
*    exit .
    DATA l_subrc(04).
    WRITE sy-subrc TO l_subrc.
    CONCATENATE 'Function Calling ERROR ... '
                'VC_I_GET_CONFIGURATION_IBASE'
                ':'
                 pa_material
                ' '
                 l_subrc
      INTO it_sys_message-text.
    APPEND it_sys_message.
    MESSAGE i000 WITH it_sys_message-text.

  ENDIF.

  CLEAR: l_variable, l_variable[].
  MOVE 'P_MOD_DATE' TO l_variable-atnam.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = l_variable-atnam
       IMPORTING
            output = l_variable-atinn.
  MOVE  sy-datum    TO l_variable-atwrt.
  APPEND l_variable.

  " ALC Code ------> Classification.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = l_variable
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

  IF sy-subrc = 0.
    LOOP AT l_variable WHERE zflag = 'E' .
      CONCATENATE 'Charac. Value UPDATE ERROR : '
                   pa_material
                  ':'
                   l_variable-atnam
        INTO it_sys_message-text.
      APPEND it_sys_message.
      MESSAGE s000 WITH it_sys_message-text.
    ENDLOOP.
  ELSE.
    WRITE sy-subrc TO l_subrc.
    CONCATENATE 'Function Execution ERROR : '
                'Z_FPP_HANDLING_MASTER'
                ' '
                 l_subrc
      INTO it_sys_message-text.
    APPEND it_sys_message.
    MESSAGE s000 WITH it_sys_message-text.
  ENDIF.
ENDFORM.                    " UPDATE_CLASSIFICATION_MM

*&---------------------------------------------------------------------*
*&      Form  RUN_SD_CALL
*&---------------------------------------------------------------------*
*       Tossing Data to SD.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM run_sd_call.
  DATA: variant              LIKE indx-srtfd VALUE 'ZISD03_01' ,
        eventid              LIKE tbtcjob-eventid.
  DATA: l_datum LIKE sy-datum.
  l_datum = sy-datum - 1 .
  EXPORT p_date = l_datum TO DATABASE indx(zz) ID variant.
  eventid = 'ZISD03_01' .

  CALL FUNCTION 'BP_EVENT_RAISE'
       EXPORTING
            eventid                = eventid
       EXCEPTIONS
            bad_eventid            = 1
            eventid_does_not_exist = 2
            eventid_missing        = 3
            raise_failed           = 4
            OTHERS                 = 5.

  IF sy-subrc <> 0.
**   PERFORM create_error_log  USING 'SD' .
*    write at: /001(50) ' Event Call Process was failed.. ',
*              /001(50) '    *** Event ID: ZISD03_01 ***  '.
    CONCATENATE 'Event Call Process failed ...'
                'Event ID : ZISD03_01 '
      INTO it_sys_message-text.
    APPEND it_sys_message.
    MESSAGE s000 WITH it_sys_message-text.

  ENDIF.

  GET TIME .
*  write at: /001(030) 'Processing Time...(End)' ,
*             031(010) sy-datum                    ,
*             042(010) sy-uzeit                    .
  CONCATENATE 'Process Ending Time is ... '
               sy-datum
              ' '
               sy-uzeit
    INTO it_sys_message-text.
  APPEND it_sys_message.
  MESSAGE s000 WITH it_sys_message-text.

ENDFORM.                    " RUN_SD_CALL
*&---------------------------------------------------------------------*
*&      Form  START_TIME_CHECKING
*&---------------------------------------------------------------------*
*       Checking Starting Time.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_time_checking.
  CONCATENATE 'Process Starting Time is ... '
               sy-datum
              ' '
               sy-uzeit
    INTO it_sys_message-text.
  APPEND it_sys_message.

ENDFORM.                    " START_TIME_CHECKING
*&---------------------------------------------------------------------*
*&      Form  TOGGLE_FOR_CHECK_BOX
*&---------------------------------------------------------------------*
*       Setting a toggle for check box.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM toggle_for_check_box.
  IF wa_tog_flg <> 'X'.
    wa_tog_flg = 'X'.
  ELSE.
    CLEAR wa_tog_flg.
  ENDIF.
  LOOP AT it_app101.
    IF wa_tog_flg = 'X'.
      it_app101-mark = 'X'.
    ELSE.
      it_app101-mark = ' '.
    ENDIF.
    MODIFY it_app101 INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " TOGGLE_FOR_CHECK_BOX
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNC_DOWNLOAD
*&---------------------------------------------------------------------*
*       Downloading data into Local Text File .
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_func_download.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'WORK ORDER REPROCESS.XLS'
            filetype                = 'DAT'
            item                    = ' '
            filetype_no_change      = 'X'
            filetype_no_show        = 'X'
       TABLES
            data_tab                = it_excel
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " CALL_FUNC_DOWNLOAD
