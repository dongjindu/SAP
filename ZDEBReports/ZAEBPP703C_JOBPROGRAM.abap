************************************************************************
* Program Name      : ZAEBPP703C_JOBPROGRAM
* Author            : Manjunath
* Creation Date     : 2007.02.27.
* Specifications By : Daniel KIM
* Development Request No : UD1K930891
* Addl Documentation: Copy of ZAPP703C_JOBPROGRAM
* Description       : Interface Work Order from Legacy System

* Modification Logs
* Date        Developer    RequestNo    Description
* 02/27/2007  Manju        UD1K930891   E-BoM Changes
*
************************************************************************
REPORT  ZAEBPP703C_JOBPROGRAM   LINE-SIZE  700 MESSAGE-ID zmpp.

TABLES: ztpp_pp_log_head,       " Table of the Interface Log(Header)
        ztpp_pp_log_deta,       " Table of the Interface Log(Detail)
        ztpp_common_vals,       " Table of the last Working Information
        ztpp_wosum    ,         " Table of the WorkOrder Summary
        ztpp_ksbohmm  .

DATA: BEGIN OF it_data       OCCURS 0.
        INCLUDE STRUCTURE    ztpp_ksbohmm .
DATA:   p_flag               TYPE c,
        wo                   TYPE c,
        h_c                  TYPE c,
        exist                TYPE c,
        material             LIKE mara-matnr,
      END OF it_data.

DATA: BEGIN OF it_alc         OCCURS 0.
        INCLUDE STRUCTURE     cukb    .
DATA:   knktx                 LIKE cukbt-knktx,
        code(3)               TYPE c  ,
        rp(2)                 TYPE n  ,
        type_alc              TYPE c  ,
        char_alc              LIKE cabn-atnam,
      END OF it_alc .

DATA: it_msg                 LIKE TABLE OF bdcmsgcoll  WITH HEADER LINE,
      it_bdcdata             LIKE TABLE OF bdcdata     WITH HEADER LINE,
      it_error               LIKE TABLE OF it_data     WITH HEADER LINE,
      wa_mode                TYPE c VALUE 'N',
      wa_date                LIKE sy-datum   ,
      wa_cnt                 TYPE i       ,
      wa_flg                 TYPE c       ,
      wa_error               TYPE c       ,
      wa_number              LIKE ztpp_pp_log_head-logkey,
      wa_data                LIKE it_data ,
      wa_seq                 LIKE ztpp_pp_log_deta-sequence,
      wa_wkdate              LIKE sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_woser          LIKE ztpp_wosum-wo_ser,
            p_log            LIKE ztpp_pp_log_head-logkey.
SELECTION-SCREEN END OF BLOCK b1.

*************** DO NOT USE!!!! *****************************************
DATA: it_rec                  LIKE TABLE OF mara ,
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      p_pmode                 TYPE  c   VALUE   'N'           ,
      wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname.          " APQI-GROUPID
************************************************************************

INITIALIZATION.

LOAD-OF-PROGRAM.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM bdc_processing .

  INCLUDE zcpp103_common_routine .

*&---------------------------------------------------------------------*
*&      Form  SET_WORKDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_workdate.
  DATA: l_date                TYPE d,
        l_gaps                TYPE i.

  SELECT SINGLE dates  INTO wa_wkdate
    FROM ztpp_common_vals
   WHERE jobs = sy-repid   .

  IF sy-subrc <> 0.
    " Step 1: Table's Entry Error for the ZTPP_COMMON_VALS.......
    PERFORM create_log USING 'E' 1 text-001  space .
    wa_error = 'E' .     EXIT.
  ENDIF.

  l_date = wa_wkdate.

  IF wa_date IS INITIAL.
    wa_date = wa_wkdate.
  ENDIF.

  SELECT MAX( chg_date ) INTO l_date
    FROM ztpp_ksbohmm  .

  l_gaps = l_date - wa_wkdate.
  l_date = wa_wkdate.

  IF l_gaps > 0 .
    DO l_gaps TIMES.
      l_date    = l_date    + 1 .
      SELECT SINGLE *
        FROM ztpp_ksbohmm
       WHERE chg_date = l_date .

      IF sy-subrc = 0.
        IF wa_flg IS INITIAL.
          wa_cnt = wa_cnt + 1 .
        ELSE.
          wa_wkdate = l_date  .
          EXIT .
        ENDIF.
      ENDIF.
    ENDDO.
  ELSE.
    CHECK wa_flg IS INITIAL .
    " Step 2: Not found entry for the processig.....
    PERFORM create_log USING 'S' 2 text-002  space .
    wa_error = 'E' .     EXIT.
  ENDIF.
ENDFORM.                    " SET_WORKDATE

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_ksbohmm
    WHERE wo_ser EQ p_woser.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_processing.
  DATA: l_modsum             LIKE ztpp_ksbohmm-modqty,
        l_initsum            LIKE ztpp_ksbohmm-initqty,
        l_material           LIKE mara-matnr           ,
        l_color              TYPE c       ,
        l_cm(15)             TYPE c       ,
        l_exist              TYPE c     ,
        l_dealer(2) type c,
        l_deal(1) type c.

  SORT it_data BY wo_ser nation dealer extc intc .
  LOOP AT it_data.
    PERFORM conversion_space_to_hypen  USING it_data-s219.
    CLEAR: l_exist, it_bdcdata, it_bdcdata[], wa_error,L_dealer.
* Dealer Conversion FROM OLD to New -
* Begin of changes -  UD1K930891
     clear l_deal.
       l_deal = it_data-dealer.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
                          EXPORTING
                              new_DEALER       = l_deal
                          IMPORTING
                              old_DEALER       = L_dealer.
         if not l_dealer is initial.
           it_data-dealer = L_dealer.
         else.
         message e001 with 'Dealer conversion error'.
         endif.

* End of changes -  UD1K930891

    PERFORM check_color      USING l_color.
    CASE   l_color .
      WHEN 'Y'    .         " WORK ORDER HEADER.
        CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
               INTO l_material .
        it_data-wo = 'H' .
        PERFORM check_material USING l_exist  l_material .
        CASE l_exist.
          WHEN 'Y'    .         "  EXIST
** added by Furong
**            PERFORM bdc_material_update USING l_material 'WOHD' .
** end of addition
          WHEN 'N'    .         "  NO DATA
            CLEAR: l_cm.
            IF it_data-bmdl(2) = 'EM'.
              CONCATENATE 'EMF_' 'WOHD' INTO l_cm .
            ELSEIF it_data-bmdl(2) = 'CR'.
              CONCATENATE 'CRA_' 'WOHD' INTO l_cm .
            ELSE.
              WRITE: 'Error in Model Code:' , l_material, it_data-bmdl.
            ENDIF.
            PERFORM bdc_material_create USING l_material 'WOHD' l_cm .
            PERFORM check_bdc_result USING 'CREATE' 'WOHD' l_material.
        ENDCASE.
      WHEN 'N'    .         " WORK ORDER COLOR .
        CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
                    it_data-extc   it_data-intc   INTO l_material .
        it_data-wo = 'C' .
        PERFORM check_material USING l_exist  l_material.
        CASE l_exist.
          WHEN 'Y'    .         "  EXIST
** added by Furong
*            PERFORM bdc_material_update USING l_material 'WOCL' .
*            PERFORM update_wosum        USING l_material        .
** end of add
          WHEN 'N'    .         "  NO DATA
            CLEAR: l_cm.
            IF it_data-bmdl(2) = 'EM'.
              CONCATENATE 'EMF_' 'WOCL' INTO l_cm .
            ELSEIF it_data-bmdl(2) = 'CR'.
              CONCATENATE 'CRA_' 'WOCL' INTO l_cm .
            ELSE.
              WRITE: 'Error in Model Code:' , l_material, it_data-bmdl.
              CONTINUE.
            ENDIF.
            PERFORM bdc_material_create USING l_material 'WOCL' l_cm .
            PERFORM check_bdc_result USING 'CREATE' 'WOCL' l_material.
            PERFORM insert_wosum        USING l_material        .
        ENDCASE.
    ENDCASE.
    MODIFY it_data.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.

  DATA: w_model(3).
  LOOP AT it_data  WHERE  p_flag = 'X'.
    CLEAR: l_material, w_model.
    CASE it_data-wo.
      WHEN 'H'.
        CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
                                                  INTO l_material .
        IF it_data-bmdl(2) = 'EM'.
          w_model = 'EMF'.
        ELSEIF it_data-bmdl(2) = 'CR'.
          w_model = 'CRA'.
        ELSE.
          WRITE: 'Error in Model Code:' , l_material, it_data-bmdl.
          CONTINUE.
        ENDIF.
        PERFORM bdc_wohd USING l_material w_model.
      WHEN 'C'.
        CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
                    it_data-extc   it_data-intc   INTO l_material .
        PERFORM bdc_wocl USING l_material .
    ENDCASE.

    CLEAR: w_model.
    IF it_data-bmdl(2) = 'EM'.
      w_model = 'EMF'.
    ELSEIF it_data-bmdl(2) = 'CR'.
      w_model = 'CRA'.
    ELSE.
      WRITE: 'Error in Model Code:' , l_material, it_data-bmdl.
      CONTINUE.
    ENDIF.
    PERFORM create_classification_mm   USING l_material  it_data-wo
                                             w_model.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  check_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COLOR  text
*----------------------------------------------------------------------*
FORM check_color USING    pa_color.
  pa_color = 'N'  .
  IF it_data-extc = '***' AND it_data-intc = '***'.
    pa_color = 'Y' .
  ENDIF.
ENDFORM.                    " check_color

*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EXIST  text
*----------------------------------------------------------------------*
FORM check_material USING    pa_exist  pa_material .
  SELECT SINGLE matnr INTO pa_material
    FROM mara
   WHERE matnr = pa_material .

  IF sy-subrc = 0.
    pa_exist = 'Y' .
  ELSE.
    pa_exist = 'N' .
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  bdc_material_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_material_create   USING  pa_material  pa_type  pa_cm .
  DATA: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20),
        l_wo_create_date(10) TYPE c          ,
        l_gen_date(10)       TYPE c          ,
        l_no(3)              TYPE n  VALUE 9 ,
        l_matnr              LIKE mara-matnr ,
        l_maktx              LIKE makt-maktx ,
        l_class(20)          TYPE c,
        l_model(3),
        l_qty                LIKE ztpp_wosum-seqqty .

  l_qty = it_data-modqty.

  SELECT SINGLE matnr INTO l_matnr
    FROM mara
   WHERE matnr = pa_cm .

  IF sy-subrc NE 0.
    " Step 5: Master Data does not exist..
    PERFORM create_log USING 'E' 5 text-004 pa_cm  .
    PERFORM create_log USING 'R' 5 text-004 it_data  .
    PERFORM create_log USING 'R' 5 text-004 it_data-s219  .
    wa_error = 'X'.
    EXIT .
  ENDIF.

  CHECK wa_error IS INITIAL .
  WRITE sy-datum TO l_gen_date       .
  CONCATENATE it_data-moye it_data-nation it_data-dealer it_data-bmdl
         INTO l_maktx .
  CONCATENATE l_maktx it_data-ocnn it_data-vers INTO l_maktx
         SEPARATED BY space .

  IF it_data-bmdl(2) = 'EM'.
    l_model = 'EMF'.
  ELSEIF it_data-bmdl(2) = 'CR'.
    l_model = 'CRA'.
  ELSE.
    WRITE: 'Error in Model Code:' , l_maktx, it_data-bmdl.
    EXIT.
  ENDIF.

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


                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=SP02' ,
                         ' '  'MAKT-MAKTX'            l_maktx    ,
                         ' '  'MARA-MEINS'           'EA'         ,

                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=PB21' ,
                         ' '  'MARA-SATNR'            pa_cm  ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,
                         ' '  'RCTMS-MNAME(01)'      'P_MI'          ,
                         ' '  'RCTMS-MWERT(01)'       it_data-bmdl   ,
                         ' '  'RCTMS-MNAME(02)'      'P_OCN'         ,
                         ' '  'RCTMS-MWERT(02)'       it_data-ocnn   ,
                         ' '  'RCTMS-MNAME(03)'      'P_VERSION'     ,
                         ' '  'RCTMS-MWERT(03)'       it_data-vers   ,
                         ' '  'RCTMS-MNAME(04)'      'P_MODEL'       ,
                         ' '  'RCTMS-MWERT(04)'       l_model        ,
*                         ' '  'RCTMS-MWERT(04)'       it_data-bmdl(3),
                         ' '  'RCTMS-MNAME(05)'      'P_WO_SER',
                         ' '  'RCTMS-MWERT(05)'       it_data-wo_ser ,
                         ' '  'RCTMS-MNAME(06)'      'P_NATION',
                         ' '  'RCTMS-MWERT(06)'       it_data-nation ,
                         ' '  'RCTMS-MNAME(07)'      'P_DEALER',
                         ' '  'RCTMS-MWERT(07)'       it_data-dealer ,
                         ' '  'RCTMS-MNAME(08)'      'P_MODEL_YEAR'  ,
                         ' '  'RCTMS-MWERT(08)'       it_data-moye   ,
                         ' '  'RCTMS-MNAME(09)'      'P_ORDER_PACK'  ,
                         ' '  'RCTMS-MWERT(09)'       pa_material(5) ,

                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=PB21' .

  IF pa_type = 'WOCL' .
    PERFORM bdc_dynpro_processing USING :
                          'X'  'SAPLCEI0'             '0109',
                          ' '  'BDC_OKCODE'           '=BACK' ,
                          ' '  'RCTMS-MNAME(01)'      'COLOREXT',
                          ' '  'RCTMS-MWERT(01)'       it_data-extc   ,
                          ' '  'RCTMS-MNAME(02)'      'COLORINT'    ,
                          ' '  'RCTMS-MWERT(02)'       it_data-intc ,

                          'X'  'SAPLMGMM'             '5004',
                          ' '  'BDC_OKCODE'           '=PB21' .
  ENDIF.

  PERFORM bdc_dynpro_processing USING :
                        'X'  'SAPLCEI0'             '0109',
                        ' '  'BDC_OKCODE'           '=BACK' ,
                        ' '  'RCTMS-MNAME(01)'      'P_219_1',
                        ' '  'RCTMS-MWERT(01)'       it_data-s219+0(1),
                        ' '  'RCTMS-MNAME(02)'      'P_219_2',
                        ' '  'RCTMS-MWERT(02)'       it_data-s219+1(1),
                        ' '  'RCTMS-MNAME(03)'      'P_219_3',
                        ' '  'RCTMS-MWERT(03)'       it_data-s219+2(1),
                        ' '  'RCTMS-MNAME(04)'      'P_219_4',
                        ' '  'RCTMS-MWERT(04)'       it_data-s219+3(1),
                        ' '  'RCTMS-MNAME(05)'      'P_219_5',
                        ' '  'RCTMS-MWERT(05)'       it_data-s219+4(1),
                        ' '  'RCTMS-MNAME(06)'      'P_219_6',
                        ' '  'RCTMS-MWERT(06)'       it_data-s219+5(1),
                        ' '  'RCTMS-MNAME(07)'      'P_219_7',
                        ' '  'RCTMS-MWERT(07)'       it_data-s219+6(1),
                        ' '  'RCTMS-MNAME(08)'      'P_219_8',
                        ' '  'RCTMS-MWERT(08)'       it_data-s219+7(1),
                        ' '  'RCTMS-MNAME(09)'      'P_219_9',
                        ' '  'RCTMS-MWERT(09)'       it_data-s219+8(1).

  DO  9 TIMES.
    l_vals1 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name1 .
    l_vals2 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name2 .
    l_vals3 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name3 .
    l_vals4 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name4 .
    l_vals5 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name5 .
    l_vals6 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name6 .
    l_vals7 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name7 .
    l_vals8 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name8 .
    l_vals9 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name9 .
    l_vals0 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name0 .

    PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '5004',
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
    l_vals1 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name1 .
    l_vals2 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name2 .
    l_vals3 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name3 .
    l_vals4 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name4 .
    l_vals5 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name5 .
    l_vals6 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name6 .
    l_vals7 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name7 .
    l_vals8 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name8 .
    l_vals9 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name9 .
    l_vals0 = it_data-s219+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name0 .

    PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '5004',
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

  " Classificaition View : SP04
  PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPLMGMM'             '5004',
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
*&      Form  bdc_material_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_material_update  USING  pa_material  pa_cm   .
  DATA: l_conf               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE,
        l_variable           LIKE TABLE OF l_conf      WITH HEADER LINE,
        l_basic              LIKE TABLE OF l_conf      WITH HEADER LINE,
        l_instance           LIKE mara-cuobf,
        l_qty                TYPE i.

*  l_qty = it_data-modqty.
*
*  " Check the MODQTY Values..  if Old MODQTY is bigger.. Error..
*  L_CONF-ATNAM = 'P_SEQ_QTY' . APPEND L_CONF.
*  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*       EXPORTING
*            object    = pa_material
*            ctype     = '001'
*       TABLES
*            val_table = l_conf.
*
*  IF sy-subrc = 0.
**    READ TABLE l_conf  WITH KEY  atnam = 'P_SEQ_QTY' .
**    IF sy-subrc = 0.
*    READ TABLE L_CONF  INDEX 1.
*      IF l_conf-atwrt > l_qty .
*        " Step 3: Sequence Quantity is Over!!!!!
*        PERFORM create_log USING 'E' 3 text-003 it_data.
*        PERFORM create_log USING 'R' 3 text-003 it_data.
*        PERFORM create_log USING 'R' 3 text-003 it_data-S219.
*        wa_error = 'X' .
*        EXIT .
*      ENDIF.
**    ENDIF.
*  ENDIF.
*
*  CHECK wa_error IS INITIAL.

* l_variable-atnam = 'P_SEQ_QTY'        .
* l_variable-atwrt =  l_conf-atwrt      .   APPEND l_variable .
  l_variable-atnam = 'P_MOD_QTY'        .
  l_variable-atwrt =  it_data-modqty    .   APPEND l_variable .
*  l_variable-atnam = 'P_MOD_DATE'       .
*  l_variable-atwrt =  sy-datum          .   APPEND l_variable .
  l_variable-atnam = 'P_WO_MODI_DATE'   .
  l_variable-atwrt =  it_data-chg_date  .   APPEND l_variable .
*  IF pa_cm = 'WOCL' .
*    l_variable-atnam = 'P_ORDER_ZONE'   .
*    l_variable-atwrt =  it_data-orzn    .     APPEND l_variable .
*  ELSE.
*    l_variable-atnam = 'P_LC_COUNT'     .
*    l_variable-atwrt =  it_data-lcnt    .     APPEND l_variable .
*    l_variable-atnam = 'P_REGION_PORT'  .
*    l_variable-atwrt =  it_data-regn    .     APPEND l_variable .
*  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = l_variable
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

  IF sy-subrc = 0.
    LOOP AT l_variable WHERE zflag = 'E' .
      " Step 4: Material's Characteristic Value UPDATE Error...
      PERFORM create_log USING 'E' 4 text-010 it_data.
      PERFORM create_log USING 'E' 4 text-010 l_variable.
      wa_error = 'X' .
    ENDLOOP.
  ENDIF.
ENDFORM.                    " bdc_material_update

*&---------------------------------------------------------------------*
*&      Form  UPDATE_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_wosum  USING pa_material .
  DATA: l_udate               LIKE sy-datum.

  CHECK wa_error IS INITIAL  .
  l_udate = wa_date + 1      .
  " AEDAT Field is changed for the SD Module for the Testing...
  UPDATE ztpp_wosum  SET: womoddate = it_data-chg_date
                          modqty    = it_data-modqty
                          aedat     = wa_date
                          aezet     = sy-uzeit
                          aenam     = sy-uname
                    WHERE wo_ser = pa_material(9)
                      AND nation = pa_material+9(3)
                      AND dealer = pa_material+12(2)
                      AND extc   = pa_material+14(2)
                      AND intc   = pa_material+16(2) .

  IF sy-subrc NE 0.
    " Step 7: Summary Table Update Error..
    PERFORM create_log USING 'E' 7 text-006 it_data.
  ENDIF.
ENDFORM.                    " UPDATE_WOSUM

*&---------------------------------------------------------------------*
*&      Form  INSERT_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
  ztpp_wosum-initqty   = it_data-initqty  .
  ztpp_wosum-modqty    = it_data-modqty   .
  ztpp_wosum-wocredate = it_data-crt_date .
  ztpp_wosum-womoddate = it_data-chg_date .
  ztpp_wosum-version   = it_data-vers     .
  ztpp_wosum-fsc       = l_fsc            .
  ztpp_wosum-erdat     = sy-datum         .
  ztpp_wosum-erzet     = sy-uzeit         .
  ztpp_wosum-ernam     = sy-uname         .
  ztpp_wosum-aedat     = sy-datum         .
  ztpp_wosum-aezet     = sy-uzeit         .
  ztpp_wosum-aenam     = sy-uname         .
  INSERT ztpp_wosum .

  IF sy-subrc NE 0.
    " Step 8: Summary Table Insert Error..
    PERFORM create_log USING 'E' 8 text-005 ztpp_wosum.
    PERFORM create_log USING 'E' 8 text-005 it_data   .
  ENDIF.
ENDFORM.                    " INSERT_WOSUM

*&---------------------------------------------------------------------*
*&      Form  BDC_WOHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM bdc_wohd USING    pa_material pa_model.
  DATA: l_tables          LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_vin(17)         TYPE c ,
        l_return(3)       TYPE c ,
        l_material        LIKE mara-matnr,
        l_atnam           LIKE ausp-atwrt,
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
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_123'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      l_vin = '---'.
*    ELSE.
*      l_vin = l_return.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_4'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_5'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_6'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_7'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_8'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '--'              INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return  '-'     INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_10'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.
*
*    CLEAR: l_return.
*    CONCATENATE pa_model '_VIN_11'  INTO l_atnam.
*    CALL FUNCTION 'Z_FPP_VIN_DEFINE'
*         EXPORTING
*              vtnam   = l_atnam
*              s219    = it_data-s219
*         IMPORTING
*              return  = l_return
*         EXCEPTIONS
*              no_data = 1
*              OTHERS  = 2.
*
*    IF l_return IS INITIAL.
*      CONCATENATE l_vin  '-'               INTO l_vin.
*    ELSE.
*      CONCATENATE l_vin  l_return          INTO l_vin.
*    ENDIF.

    CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
           INTO l_material .
*    PERFORM call_bdc_wohd_vin  USING l_material  l_vin.
  ELSE.
    CASE l_flag.
      WHEN 'L'.     " Instance not found...
        " Step 9: ALC Code --> Work Order Instance Not found!!
        PERFORM create_log USING 'E' 9  text-008 it_data   .
      WHEN 'E'.     " Failure of the Classification Change...
        " Step 10: ALC Code --> Classification Table Update Error..
        PERFORM create_log USING 'E' 10 text-009 it_data   .
      WHEN 'V'.     " Failure of the generation of vin spec
        PERFORM create_log USING 'V' 10 text-015 it_data   .

    ENDCASE.
  ENDIF.
ENDFORM.                    " BDC_WOHD

*&---------------------------------------------------------------------*
*&      Form  BDC_WOCL
*&---------------------------------------------------------------------*
*       text
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

  CASE l_flag.
    WHEN 'L'.     " Instance not found...
      " Step 9: ALC Code --> Work Order Instance Not found!!
      PERFORM create_log USING 'E' 9  text-008 it_data   .
    WHEN 'E'.     " Failure of the Classification Change...
      " Step 10: ALC Code --> Classification Table Update Error..
      PERFORM create_log USING 'E' 10 text-009 it_data   .
  ENDCASE.
ENDFORM.                    " BDC_WOCL

*&---------------------------------------------------------------------*
*&      Form  UPDATE_LASTDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_lastdate.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = sy-repid
     AND key2 = '******************'
     AND key3 = '******************' .

  ztpp_common_vals-dates = wa_wkdate .
  ztpp_common_vals-description = 'VIN Last Serial-No.' .
  ztpp_common_vals-udate = sy-datum  .
  ztpp_common_vals-utime = sy-uzeit  .
  MODIFY ztpp_common_vals .
ENDFORM.                    " UPDATE_LASTDATE

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_WOHD_VIN
*&---------------------------------------------------------------------*
*       text
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
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

  IF sy-subrc = 0.
    LOOP AT l_variable WHERE zflag = 'E' .
      wa_error = 'X' .
    ENDLOOP.
  ELSE.
    wa_error = 'X' .
  ENDIF.
ENDFORM.                    " CALL_BDC_WOHD_VIN

*&---------------------------------------------------------------------*
*&      Form  concate_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FSC  text
*----------------------------------------------------------------------*
FORM concate_fsc USING    pa_fsc.
  CLEAR: pa_fsc .
  CONCATENATE it_data-moye it_data-nation it_data-dealer it_data-bmdl
         INTO pa_fsc .
  CONCATENATE pa_fsc it_data-ocnn INTO pa_fsc SEPARATED BY space .
ENDFORM.                    " concate_fsc

*&---------------------------------------------------------------------*
*&      Form  CHECK_BDC_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0420   text
*      -->P_0421   text
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM check_bdc_result USING    pa_mode  pa_type  pa_material.
  DATA: l_msg(100)           TYPE c.

  IF wa_error NE ' '     .
    CLEAR: it_msg, it_msg[].
    EXIT .
  ENDIF.

  it_data-p_flag = 'X' .

  READ TABLE it_msg WITH KEY msgtyp = 'E' .

  CHECK sy-subrc = 0.
  wa_error = 'E'    .
  CLEAR: it_data-p_flag.
  LOOP AT it_msg WHERE msgtyp = 'E' .
    PERFORM create_message USING l_msg  it_msg-msgid it_msg-msgnr
                    it_msg-msgv1 it_msg-msgv2 it_msg-msgv3 it_msg-msgv4.
    " Step 6: Material Master Creation Error for the BDC
    PERFORM create_log USING 'E' 6 l_msg    it_data.
    PERFORM create_log USING 'R' 6 l_msg    it_data.
    PERFORM create_log USING 'R' 6 l_msg    it_data-s219.
  ENDLOOP.
  CLEAR: it_msg, it_msg[].
ENDFORM.                    " CHECK_BDC_RESULT

*&---------------------------------------------------------------------*
*&      Form  create_classification_mm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
FORM create_classification_mm USING    pa_material  pa_wo  pa_model.
  DATA: l_variable           LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE,
        l_perf               TYPE c,
        l_no(3)              TYPE n,
        l_of(3)              TYPE n,
        l_name(30)           TYPE c.

  PERFORM check_perf_alc           USING pa_material  l_perf  pa_model.

  l_variable-atnam = 'P_INIT_QTY'       .
  l_variable-atwrt =  it_data-initqty   .   APPEND l_variable .
  l_variable-atnam = 'P_MOD_QTY'        .
  l_variable-atwrt =  it_data-modqty    .   APPEND l_variable .
  l_variable-atnam = 'P_WO_SER'         .
  l_variable-atwrt =  it_data-wo_ser    .   APPEND l_variable .
  l_variable-atnam = 'P_NATION'         .
  l_variable-atwrt =  it_data-nation    .   APPEND l_variable .
  l_variable-atnam = 'P_DEALER'         .
  l_variable-atwrt =  it_data-dealer    .   APPEND l_variable .
  l_variable-atnam = 'P_GEN_DATE'       .
  l_variable-atwrt =  sy-datum          .   APPEND l_variable .
  l_variable-atnam = 'P_MOD_DATE'       .
  l_variable-atwrt =  sy-datum          .   APPEND l_variable .
  l_variable-atnam = 'P_DESTINATION_CODE'.
  l_variable-atwrt =  it_data-dest      .   APPEND l_variable .

  IF it_data-wo = 'C' .
    l_variable-atnam = 'COLOREXT'       .
    l_variable-atwrt =  it_data-extc    .     APPEND l_variable .
    l_variable-atnam = 'COLORINT'       .
    l_variable-atwrt =  it_data-intc    .     APPEND l_variable .
    l_variable-atnam = 'P_ORDER_ZONE'   .
    l_variable-atwrt =  it_data-orzn    .     APPEND l_variable .
    l_variable-atnam = 'P_COLOR_SER'    .
    l_variable-atwrt =  it_data-clsr    .     APPEND l_variable .
    l_variable-atnam = 'P_REGION_PORT'  .
    l_variable-atwrt =  it_data-regn    .     APPEND l_variable .
    l_variable-atnam = 'P_FLEET'        .
    l_variable-atwrt =  it_data-flet    .     APPEND l_variable .
    l_variable-atnam = 'P_MANUAL_ORDER' .
    l_variable-atwrt =  it_data-maor    .     APPEND l_variable .
  ELSE.
    l_variable-atnam = 'P_LC_NO'        .
    l_variable-atwrt =  it_data-lcno    .     APPEND l_variable .
    l_variable-atnam = 'P_LC_COUNT'     .
    l_variable-atwrt =  it_data-lcnt    .     APPEND l_variable .
    l_variable-atnam = 'P_REGION_PORT'  .
    l_variable-atwrt =  it_data-regn    .     APPEND l_variable .
  ENDIF.

  l_variable-atnam = 'P_MODEL_YEAR'   .
  l_variable-atwrt =  it_data-moye    .     APPEND l_variable .
  l_variable-atnam = 'P_MI'           .
  l_variable-atwrt =  it_data-bmdl    .     APPEND l_variable .
  l_variable-atnam = 'P_OCN'          .
  l_variable-atwrt =  it_data-ocnn    .     APPEND l_variable .
  l_variable-atnam = 'P_VERSION'      .
  l_variable-atwrt =  it_data-vers    .     APPEND l_variable .
  l_variable-atnam = 'P_MODEL'        .
  l_variable-atwrt =  pa_model        .     APPEND l_variable .
*  l_variable-atwrt =  it_data-bmdl(3) .     APPEND l_variable .
*  l_variable-atnam = 'P_MI'           .
*  l_variable-atwrt =  it_data-bmdl    .     APPEND l_variable .
  l_variable-atnam = 'P_WO_MODI_DATE' .
  l_variable-atwrt =  it_data-chg_date .     APPEND l_variable .
  l_variable-atnam = 'P_WO_CREATE_DATE'.
  l_variable-atwrt =  it_data-crt_date .     APPEND l_variable .
  l_variable-atnam = 'P_PLAN_QTY'    .
  l_variable-atwrt =  '0'            .       APPEND l_variable .
  l_variable-atnam = 'P_FORECAST_QTY'.
  l_variable-atwrt =  '0'            .       APPEND l_variable .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = l_variable
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

  IF sy-subrc = 0.
    LOOP AT l_variable WHERE zflag = 'E' .
      " Step 4: Material's Characteristic Value UPDATE Error...
      PERFORM create_log USING 'E' 4 text-010 it_data.
      PERFORM create_log USING 'E' 4 text-010 l_variable.
      wa_error = 'X' .
    ENDLOOP.
  ENDIF.
ENDFORM.                    " create_classification_mm

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_SPACE_TO_HYPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_S219  text
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
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
FORM create_log USING    pa_type  pa_step  pa_text  pa_key .
  DATA: l_seq                LIKE ztpp_pp_log_deta-sequence.
  SELECT MAX( sequence ) INTO l_seq
    FROM ztpp_pp_log_deta
   WHERE logkey = p_log   .

  l_seq = l_seq + 1.
  " Log Detail Creation
  ztpp_pp_log_deta-logkey   = p_log        .
  ztpp_pp_log_deta-sequence = l_seq      .
  ztpp_pp_log_deta-logtype  = pa_type     .
  ztpp_pp_log_deta-logstep  = pa_step     .
  ztpp_pp_log_deta-keydata  = pa_key      .
  INSERT INTO ztpp_pp_log_deta VALUES ztpp_pp_log_deta .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  GET_LOGSERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_logserial.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZLOG'
       IMPORTING
            number                  = wa_number
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
ENDFORM.                    " GET_LOGSERIAL

*&---------------------------------------------------------------------*
*&      Form  create_steplog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_steplog.
  IF wa_error = space.
    " Step 11: Successfully creation...
    PERFORM create_log USING 'S' 11 text-011  it_data.
  ELSE.
    " Step 11: Error creation...
    PERFORM create_log USING 'E' 11 text-012  it_data.
  ENDIF.
ENDFORM.                    " create_steplog

*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  DATA: l_modsum             LIKE ztpp_ksbohmm-modqty,
        l_initsum            LIKE ztpp_ksbohmm-initqty,
        l_hqty               LIKE ztpp_ksbohmm-initqty,  " Head Qty
        l_cqty               LIKE ztpp_ksbohmm-initqty,  " Color Qty
        l_rqty               LIKE ztpp_ksbohmm-initqty,  " Remain Qty
        l_data               LIKE TABLE OF it_data WITH HEADER LINE,
        l_material           LIKE mara-matnr          ,
        l_error              TYPE c       ,
        l_cm(15)             TYPE c       ,
        l_exist              LIKE sy-subrc.

  " Check the Sequence Quantity for the Work Order....
  SORT it_data BY wo_ser nation dealer extc intc .
  l_data[] = it_data[].
*  DELETE l_data WHERE extc = '***' .

  LOOP AT it_data.
    PERFORM check_color      USING it_data-h_c.
    CASE  it_data-h_c .
      WHEN 'Y'    .         " WORK ORDER HEADER.
        " Check the Result about the Previous Work order....
        IF l_hqty = 0 .
          " Normal!!
        ELSE.
          " Error --> Unmatched the Sequence qty...
          PERFORM create_log USING 'E' 13 text-014  l_material.
          LOOP AT l_data WHERE wo_ser = l_material(9)    AND
                               nation = l_material+9(3)  AND
                               dealer = l_material+12(2) AND
                               extc   NE '***'           .
            PERFORM create_log USING 'R' 13 text-014  l_data.
            DELETE l_data.
          ENDLOOP.
          DELETE it_data WHERE wo_ser = l_material(9)   AND
                               nation = l_material+9(3) AND
                               dealer = l_material+12(2) .
        ENDIF.
        CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
               INTO it_data-material .
        it_data-wo = 'H' .
        PERFORM check_material USING it_data-exist  it_data-material .
        CASE it_data-exist.
          WHEN 'Y'    .         "  EXIST
            " Check between the Old-Data(Header/Color) and the New-Data.
            CLEAR: l_error.
            PERFORM check_workorder  USING l_error.
            IF l_error = 'X'.
              PERFORM create_log USING 'E' 13 text-014 it_data-material.
              LOOP AT l_data WHERE wo_ser = it_data-material(9)    AND
                                   nation = it_data-material+9(3)  AND
                                   dealer = it_data-material+12(2) .
                MOVE-CORRESPONDING l_data   TO  it_error           .
                APPEND it_error    .
              ENDLOOP.
              LOOP AT l_data WHERE wo_ser = it_data-material(9)    AND
                                   nation = it_data-material+9(3)  AND
                                   dealer = it_data-material+12(2) AND
                                   extc   NE '***'           .
                PERFORM create_log USING 'R' 13 text-014  l_data.
                DELETE l_data.
              ENDLOOP.
              DELETE it_data WHERE wo_ser = it_data-material(9)   AND
                                   nation = it_data-material+9(3) AND
                                   dealer = it_data-material+12(2) .
            ENDIF.
*            l_hqty = it_data-modqty.
          WHEN 'N'    .         "  NO DATA
            l_hqty = it_data-modqty.
        ENDCASE.
      WHEN 'N'    .         " WORK ORDER COLOR .
        CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
                    it_data-extc   it_data-intc   INTO it_data-material.
        it_data-wo = 'C' .
        PERFORM check_material USING it_data-exist  it_data-material .
        CASE it_data-exist.
*          WHEN 'Y'    .         "  EXIST
          WHEN 'N'    .         "  NO DATA
            l_hqty = l_hqty - it_data-modqty .
        ENDCASE.
    ENDCASE.
    CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
           INTO l_material .
    CHECK l_error = space  .
    MODIFY it_data.
  ENDLOOP.
ENDFORM.                    " check_data

*&---------------------------------------------------------------------*
*&      Form  check_workorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_workorder   USING pa_flag.
  DATA: l_worder                 LIKE TABLE OF it_data WITH HEADER LINE,
        l_cqty                   LIKE ztpp_ksbohmm-initqty,  " Color Qty
        l_conf                   LIKE TABLE OF zspp_vin_value
                                                 WITH HEADER LINE.

  l_worder[] = it_data[].
  CLEAR: l_conf, l_conf[].
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_MOD_QTY'
       IMPORTING
            output = l_conf-atinn.
  APPEND l_conf.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_SEQ_QTY'
       IMPORTING
            output = l_conf-atinn.
  APPEND l_conf.

  LOOP AT l_worder WHERE wo_ser = it_data-wo_ser  AND
                         nation = it_data-nation  AND
                         dealer = it_data-dealer  AND
                         extc   NE '***'          .
    CONCATENATE it_data-wo_ser it_data-nation it_data-dealer
                it_data-extc   it_data-intc   INTO l_worder-material.


    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_worder-material
              ctype        = '001'
         TABLES
              val_table    = l_conf
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    " Compare the Qty....
    READ TABLE l_conf WITH KEY atnam = 'P_SEQ_QTY' .
    l_cqty = l_conf-atwrt.
    IF l_cqty > l_worder-modqty.
      " Error.....
      pa_flag = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_workorder

*&---------------------------------------------------------------------*
*&      Form  check_perf_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_PERF  text
*----------------------------------------------------------------------*
FORM check_perf_alc USING    pa_material  pa_perf  pa_model .
  DATA: l_str(10)            TYPE c         ,
        l_knobj              LIKE cuco-knobj,
        l_knnum              LIKE cuob-knobj,
        l_knnam              LIKE cukb-knnam.

  CLEAR: it_alc, it_alc[].

  CONCATENATE pa_model '_WOHD'           INTO  l_knnam.
  PERFORM get_knobj                      USING l_knnam  l_knobj.
  PERFORM get_knnum USING l_knobj.
  CONCATENATE pa_model '_WOCL'           INTO  l_knnam.
  PERFORM get_knobj                      USING l_knnam  l_knobj.
  PERFORM get_knnum                      USING l_knobj.

  LOOP AT it_alc.
    SELECT SINGLE b~knnam t~knktx
      INTO CORRESPONDING FIELDS OF it_alc
      FROM cukb AS b INNER JOIN cukbt AS t
        ON b~knnum = t~knnum
     WHERE b~knnum = it_alc-knnum
       AND t~spras = sy-langu   .

    CONCATENATE 'D_' pa_model '_ALC_'   INTO l_str.
    IF it_alc-knnam(10) NE l_str        .
      DELETE it_alc .
      CONTINUE .
    ENDIF.
    it_alc-code     = it_alc-knnam+12(3) .
    it_alc-type_alc = it_alc-knnam+10(1) .
    it_alc-rp       = it_alc-knktx(2)    .
    CONCATENATE 'P' it_alc-knnam+5(10)  INTO it_alc-char_alc .
    MODIFY it_alc .
  ENDLOOP.
  SORT it_alc BY knnum rp code .
ENDFORM.                    " check_perf_alc

*&---------------------------------------------------------------------*
*&      Form  get_knobj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM get_knobj USING    pa_knnam  pa_knobj.
  SELECT SINGLE knobj INTO pa_knobj
    FROM cuco
   WHERE obtab = 'MARA'
     AND objek = pa_knnam .
ENDFORM.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM get_knnum USING    pa_knobj.
  SELECT knnum APPENDING CORRESPONDING FIELDS OF TABLE it_alc
    FROM cuob
   WHERE knobj = pa_knobj.
ENDFORM.                    " get_KNNUM
