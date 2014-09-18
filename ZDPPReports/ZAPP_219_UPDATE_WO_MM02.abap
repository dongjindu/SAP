************************************************************************
* Program Name      : ZAPP_219_UPDATE_WO
* Author            : Furong, Wang
* Creation Date     : 10/2005
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Module Cost Update
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT zapp_219_update_wo_mm02
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID zmbm.

TABLES: ztpp_wosum.
DATA: it_wosum LIKE TABLE OF ztpp_wosum WITH HEADER LINE,
      it_wosum_hd LIKE TABLE OF ztpp_wosum WITH HEADER LINE,

      it_abxalcdt LIKE TABLE OF ztbm_abxalcdt WITH HEADER LINE .


DATA: wa_material             LIKE mara-matnr                 ,
      wa_material_cl          LIKE mara-matnr                 ,
      wa_error                TYPE c                          ,
      wa_date                 TYPE d                          .

DATA: session(1).

*DATA: wa_tline TYPE n.

INCLUDE zbdcrecx1_pp.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-101.
SELECT-OPTIONS: s_wo FOR ztpp_wosum-wo_ser.
SELECT-OPTIONS: s_date FOR ztpp_wosum-wocredate.
SELECTION-SCREEN END   OF BLOCK b2.

START-OF-SELECTION.
  PERFORM read_data.
  PERFORM data_process.
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.

  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_vartable_300    LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_name(30)        TYPE c           ,
        l_no(03)          TYPE n,
        l_recno(4)           TYPE n,
        l_fsc             LIKE ztbm_abxalcdt-mtno,
        l_alcd             LIKE ztbm_abxalcdt-alcd,
        l_material_300   LIKE mara-matnr,
        l_mess(40).

  DATA: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20),
        l_vals(01).

  it_wosum_hd[] = it_wosum[].

  SORT it_wosum_hd BY wo_ser nation dealer.
  DELETE ADJACENT DUPLICATES FROM it_wosum_hd
       COMPARING wo_ser nation dealer.

** WO HEADER UPDATE
  LOOP AT it_wosum_hd.
    CLEAR: bdcdata, bdcdata[].
    CLEAR: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20).

    CLEAR: l_fsc, wa_material, wa_material_cl, l_material_300,
           l_vartable,l_vartable[],l_vartable_300,l_vartable_300[],l_no.

    CONCATENATE it_wosum_hd-wo_ser it_wosum_hd-nation
                it_wosum_hd-dealer INTO wa_material.

    CLEAR : l_name, l_vals.
    l_fsc = it_wosum_hd-fsc.
    SELECT SINGLE alcd INTO l_alcd FROM ztbm_abxalcdt
          WHERE mtno = l_fsc.

    l_vals1 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name1.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals2 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name2.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals3 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name3 .
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .
*
    l_vals4 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name4.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .
*
    l_vals5 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name5.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals6 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name6.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals7 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name7.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals8 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name8.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals9 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name9.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    PERFORM bdc_dynpro      USING 'SAPLMGMM' '0060'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RMMG1-MATNR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=AUSW'.
    PERFORM bdc_field       USING 'RMMG1-MATNR'
*                              record-MATNR_001.
                                   wa_material.
    PERFORM bdc_dynpro      USING 'SAPLMGMM' '0070'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MSICHTAUSW-DYTXT(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(02)'
                                   'X'.
*                              record-KZSEL_02_002.
    PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PB21'.

    PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RCTMS-MWERT(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM bdc_field       USING 'RCTMS-MNAME(01)'
                                  l_name1.
    PERFORM bdc_field       USING 'RCTMS-MWERT(01)'
                                  l_vals1.
    PERFORM bdc_field       USING 'RCTMS-MNAME(02)'
                                  l_name2.
    PERFORM bdc_field       USING 'RCTMS-MWERT(02)'
                                  l_vals2.
    PERFORM bdc_field       USING 'RCTMS-MNAME(03)'
                                  l_name3.
    PERFORM bdc_field       USING 'RCTMS-MWERT(03)'
                                  l_vals3.
    PERFORM bdc_field       USING 'RCTMS-MNAME(04)'
                                  l_name4.
    PERFORM bdc_field       USING 'RCTMS-MWERT(04)'
                                  l_vals4.
    PERFORM bdc_field       USING 'RCTMS-MNAME(05)'
                                  l_name5.
    PERFORM bdc_field       USING 'RCTMS-MWERT(05)'
                                  l_vals5.
    PERFORM bdc_field       USING 'RCTMS-MNAME(06)'
                                  l_name6.
    PERFORM bdc_field       USING 'RCTMS-MWERT(06)'
                                  l_vals6.
    PERFORM bdc_field       USING 'RCTMS-MNAME(07)'
                                  l_name7.
    PERFORM bdc_field       USING 'RCTMS-MWERT(07)'
                                  l_vals7.
    PERFORM bdc_field       USING 'RCTMS-MNAME(08)'
                                  l_name8.
    PERFORM bdc_field       USING 'RCTMS-MWERT(08)'
                                  l_vals8.
    PERFORM bdc_field       USING 'RCTMS-MNAME(09)'
                                  l_name9.
    PERFORM bdc_field       USING 'RCTMS-MWERT(09)'
                                  l_vals9.

    DO  9 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name1 .
*      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
*      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name2 .
*      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
*      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name3 .
*      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
*      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name4 .
*      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
*      APPEND l_vartable        .
*
      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name5 .
*      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
*      APPEND l_vartable        .
*
      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name6 .
*      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
*      APPEND l_vartable        .

      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name7 .
*      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
*      APPEND l_vartable        .
*
      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name8 .
*      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
*      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name9 .
*      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
*      APPEND l_vartable.

      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name0 .
*      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
*      APPEND l_vartable.

      PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=PB21'.

      PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RCTMS-MWERT(02)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.
      PERFORM bdc_field       USING 'RCTMS-MNAME(01)'
                                    l_name1.
      PERFORM bdc_field       USING 'RCTMS-MWERT(01)'
                                    l_vals1.
      PERFORM bdc_field       USING 'RCTMS-MNAME(02)'
                                    l_name2.
      PERFORM bdc_field       USING 'RCTMS-MWERT(02)'
                                    l_vals2.
      PERFORM bdc_field       USING 'RCTMS-MNAME(03)'
                                    l_name3.
      PERFORM bdc_field       USING 'RCTMS-MWERT(03)'
                                    l_vals3.
      PERFORM bdc_field       USING 'RCTMS-MNAME(04)'
                                    l_name4.
      PERFORM bdc_field       USING 'RCTMS-MWERT(04)'
                                    l_vals4.
      PERFORM bdc_field       USING 'RCTMS-MNAME(05)'
                                    l_name5.
      PERFORM bdc_field       USING 'RCTMS-MWERT(05)'
                                    l_vals5.
      PERFORM bdc_field       USING 'RCTMS-MNAME(06)'
                                    l_name6.
      PERFORM bdc_field       USING 'RCTMS-MWERT(06)'
                                    l_vals6.
      PERFORM bdc_field       USING 'RCTMS-MNAME(07)'
                                    l_name7.
      PERFORM bdc_field       USING 'RCTMS-MWERT(07)'
                                    l_vals7.
      PERFORM bdc_field       USING 'RCTMS-MNAME(08)'
                                    l_name8.
      PERFORM bdc_field       USING 'RCTMS-MWERT(08)'
                                    l_vals8.
      PERFORM bdc_field       USING 'RCTMS-MNAME(09)'
                                    l_name9.
      PERFORM bdc_field       USING 'RCTMS-MWERT(09)'
                                    l_vals9.
      PERFORM bdc_field       USING 'RCTMS-MNAME(10)'
                                    l_name0.
      PERFORM bdc_field       USING 'RCTMS-MWERT(10)'
                                    l_vals0.

    ENDDO.

    DO 12 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name1 .
*      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
*      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name2 .
*      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
*      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name3 .
*      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
*      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name4 .
*      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
*      APPEND l_vartable        .
*
      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name5 .
*      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
*      APPEND l_vartable        .

      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name6 .
*      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
*      APPEND l_vartable        .
*
      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name7 .
*      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
*      APPEND l_vartable        .
*
      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name8 .
*      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
*      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name9 .
*      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
*      APPEND l_vartable        .
*
      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name0 .
*      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
*      APPEND l_vartable        .

      PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=PB21'.

      PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RCTMS-MWERT(02)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.
      PERFORM bdc_field       USING 'RCTMS-MNAME(01)'
                                    l_name1.
      PERFORM bdc_field       USING 'RCTMS-MWERT(01)'
                                    l_vals1.
      PERFORM bdc_field       USING 'RCTMS-MNAME(02)'
                                    l_name2.
      PERFORM bdc_field       USING 'RCTMS-MWERT(02)'
                                    l_vals2.
      PERFORM bdc_field       USING 'RCTMS-MNAME(03)'
                                    l_name3.
      PERFORM bdc_field       USING 'RCTMS-MWERT(03)'
                                    l_vals3.
      PERFORM bdc_field       USING 'RCTMS-MNAME(04)'
                                    l_name4.
      PERFORM bdc_field       USING 'RCTMS-MWERT(04)'
                                    l_vals4.
      PERFORM bdc_field       USING 'RCTMS-MNAME(05)'
                                    l_name5.
      PERFORM bdc_field       USING 'RCTMS-MWERT(05)'
                                    l_vals5.
      PERFORM bdc_field       USING 'RCTMS-MNAME(06)'
                                    l_name6.
      PERFORM bdc_field       USING 'RCTMS-MWERT(06)'
                                    l_vals6.
      PERFORM bdc_field       USING 'RCTMS-MNAME(07)'
                                    l_name7.
      PERFORM bdc_field       USING 'RCTMS-MWERT(07)'
                                    l_vals7.
      PERFORM bdc_field       USING 'RCTMS-MNAME(08)'
                                    l_name8.
      PERFORM bdc_field       USING 'RCTMS-MWERT(08)'
                                    l_vals8.
      PERFORM bdc_field       USING 'RCTMS-MNAME(09)'
                                    l_name9.
      PERFORM bdc_field       USING 'RCTMS-MWERT(09)'
                                    l_vals9.
      PERFORM bdc_field       USING 'RCTMS-MNAME(10)'
                                    l_name0.
      PERFORM bdc_field       USING 'RCTMS-MWERT(10)'
                                    l_vals0.

    ENDDO.


    PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PB21'.


    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MAKT-MAKTX'.
    PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RCTMS-MWERT(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MAKT-MAKTX'.
    PERFORM bdc_transaction USING 'MM02'.

    IF sy-subrc <> 0.
      WRITE:/ wa_material, 'Error'.      wa_error = 'X'      .
    ELSE.
      WRITE:/ wa_material, 'updated'.
*      COMMIT WORK.
    ENDIF.
    CLEAR it_wosum_hd.
    l_recno = l_recno + 1.
*    if l_recno = 50 or l_recno = 100 or l_recno = 150.
    CONCATENATE 'processed rec.' l_recno wa_material INTO l_mess.
    PERFORM display_progress_bar USING l_mess.
*    endif.
  ENDLOOP.
  COMMIT WORK.
  CLEAR: l_recno.

** WOCL UPDATE

  LOOP AT it_wosum.
    CLEAR: bdcdata, bdcdata[].
    CLEAR: l_name0(20), l_name1(20), l_name2(20), l_name3(20),
        l_name4(20), l_name5(20), l_name6(20), l_name7(20),
        l_name8(20), l_name9(20), l_vals0(01), l_vals1(01),
        l_vals2(20), l_vals3(20), l_vals4(20), l_vals5(20),
        l_vals6(20), l_vals7(20), l_vals8(20), l_vals9(20).

    CLEAR: l_fsc, wa_material, wa_material_cl, l_material_300,
           l_vartable,l_vartable[],l_vartable_300,l_vartable_300[],l_no.

    CONCATENATE it_wosum-wo_ser it_wosum-nation
                 it_wosum-dealer it_wosum-extc it_wosum-intc
                 INTO wa_material.

    CLEAR : l_name, l_vals.
    l_fsc = it_wosum-fsc.
    SELECT SINGLE alcd INTO l_alcd FROM ztbm_abxalcdt
          WHERE mtno = l_fsc.

    l_vals1 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name1.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals2 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name2.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals3 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name3 .
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .
*
    l_vals4 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name4.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .
*
    l_vals5 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name5.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals6 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name6.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals7 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name7.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals8 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name8.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    l_vals9 = l_alcd+l_no(1) .
    l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+2(1)  INTO  l_name9.
*    l_vartable-atnam = l_name.       l_vartable-atwrt = l_vals.
*    APPEND l_vartable        .

    PERFORM bdc_dynpro      USING 'SAPLMGMM' '0060'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RMMG1-MATNR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=AUSW'.
    PERFORM bdc_field       USING 'RMMG1-MATNR'
*                              record-MATNR_001.
                                   wa_material.
    PERFORM bdc_dynpro      USING 'SAPLMGMM' '0070'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MSICHTAUSW-DYTXT(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(02)'
                                   'X'.
*                              record-KZSEL_02_002.
    PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PB21'.

    PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RCTMS-MWERT(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM bdc_field       USING 'RCTMS-MNAME(01)'
                                  l_name1.
    PERFORM bdc_field       USING 'RCTMS-MWERT(01)'
                                  l_vals1.
    PERFORM bdc_field       USING 'RCTMS-MNAME(02)'
                                  l_name2.
    PERFORM bdc_field       USING 'RCTMS-MWERT(02)'
                                  l_vals2.
    PERFORM bdc_field       USING 'RCTMS-MNAME(03)'
                                  l_name3.
    PERFORM bdc_field       USING 'RCTMS-MWERT(03)'
                                  l_vals3.
    PERFORM bdc_field       USING 'RCTMS-MNAME(04)'
                                  l_name4.
    PERFORM bdc_field       USING 'RCTMS-MWERT(04)'
                                  l_vals4.
    PERFORM bdc_field       USING 'RCTMS-MNAME(05)'
                                  l_name5.
    PERFORM bdc_field       USING 'RCTMS-MWERT(05)'
                                  l_vals5.
    PERFORM bdc_field       USING 'RCTMS-MNAME(06)'
                                  l_name6.
    PERFORM bdc_field       USING 'RCTMS-MWERT(06)'
                                  l_vals6.
    PERFORM bdc_field       USING 'RCTMS-MNAME(07)'
                                  l_name7.
    PERFORM bdc_field       USING 'RCTMS-MWERT(07)'
                                  l_vals7.
    PERFORM bdc_field       USING 'RCTMS-MNAME(08)'
                                  l_name8.
    PERFORM bdc_field       USING 'RCTMS-MWERT(08)'
                                  l_vals8.
    PERFORM bdc_field       USING 'RCTMS-MNAME(09)'
                                  l_name9.
    PERFORM bdc_field       USING 'RCTMS-MWERT(09)'
                                  l_vals9.

    DO  9 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name1 .
*      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
*      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name2 .
*      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
*      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name3 .
*      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
*      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name4 .
*      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
*      APPEND l_vartable        .
*
      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name5 .
*      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
*      APPEND l_vartable        .
*
      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name6 .
*      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
*      APPEND l_vartable        .

      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name7 .
*      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
*      APPEND l_vartable        .
*
      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name8 .
*      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
*      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name9 .
*      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
*      APPEND l_vartable.

      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no+1(2)  INTO  l_name0 .
*      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
*      APPEND l_vartable.

      PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=PB21'.

      PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RCTMS-MWERT(02)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.
      PERFORM bdc_field       USING 'RCTMS-MNAME(01)'
                                    l_name1.
      PERFORM bdc_field       USING 'RCTMS-MWERT(01)'
                                    l_vals1.
      PERFORM bdc_field       USING 'RCTMS-MNAME(02)'
                                    l_name2.
      PERFORM bdc_field       USING 'RCTMS-MWERT(02)'
                                    l_vals2.
      PERFORM bdc_field       USING 'RCTMS-MNAME(03)'
                                    l_name3.
      PERFORM bdc_field       USING 'RCTMS-MWERT(03)'
                                    l_vals3.
      PERFORM bdc_field       USING 'RCTMS-MNAME(04)'
                                    l_name4.
      PERFORM bdc_field       USING 'RCTMS-MWERT(04)'
                                    l_vals4.
      PERFORM bdc_field       USING 'RCTMS-MNAME(05)'
                                    l_name5.
      PERFORM bdc_field       USING 'RCTMS-MWERT(05)'
                                    l_vals5.
      PERFORM bdc_field       USING 'RCTMS-MNAME(06)'
                                    l_name6.
      PERFORM bdc_field       USING 'RCTMS-MWERT(06)'
                                    l_vals6.
      PERFORM bdc_field       USING 'RCTMS-MNAME(07)'
                                    l_name7.
      PERFORM bdc_field       USING 'RCTMS-MWERT(07)'
                                    l_vals7.
      PERFORM bdc_field       USING 'RCTMS-MNAME(08)'
                                    l_name8.
      PERFORM bdc_field       USING 'RCTMS-MWERT(08)'
                                    l_vals8.
      PERFORM bdc_field       USING 'RCTMS-MNAME(09)'
                                    l_name9.
      PERFORM bdc_field       USING 'RCTMS-MWERT(09)'
                                    l_vals9.
      PERFORM bdc_field       USING 'RCTMS-MNAME(10)'
                                    l_name0.
      PERFORM bdc_field       USING 'RCTMS-MWERT(10)'
                                    l_vals0.

    ENDDO.

    DO 12 TIMES.
      l_vals1 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name1 .
*      l_vartable-atnam = l_name1.       l_vartable-atwrt = l_vals1.
*      APPEND l_vartable        .

      l_vals2 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name2 .
*      l_vartable-atnam = l_name2.       l_vartable-atwrt = l_vals2.
*      APPEND l_vartable        .

      l_vals3 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name3 .
*      l_vartable-atnam = l_name3.       l_vartable-atwrt = l_vals3.
*      APPEND l_vartable        .

      l_vals4 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name4 .
*      l_vartable-atnam = l_name4.       l_vartable-atwrt = l_vals4.
*      APPEND l_vartable        .
*
      l_vals5 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name5 .
*      l_vartable-atnam = l_name5.       l_vartable-atwrt = l_vals5.
*      APPEND l_vartable        .

      l_vals6 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name6 .
*      l_vartable-atnam = l_name6.       l_vartable-atwrt = l_vals6.
*      APPEND l_vartable        .
*
      l_vals7 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name7 .
*      l_vartable-atnam = l_name7.       l_vartable-atwrt = l_vals7.
*      APPEND l_vartable        .
*
      l_vals8 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name8 .
*      l_vartable-atnam = l_name8.       l_vartable-atwrt = l_vals8.
*      APPEND l_vartable        .

      l_vals9 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name9 .
*      l_vartable-atnam = l_name9.       l_vartable-atwrt = l_vals9.
*      APPEND l_vartable        .
*
      l_vals0 = l_alcd+l_no(1) .
      l_no = l_no + 1.   CONCATENATE 'P_219_' l_no       INTO  l_name0 .
*      l_vartable-atnam = l_name0.       l_vartable-atwrt = l_vals0.
*      APPEND l_vartable        .

      PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=PB21'.

      PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RCTMS-MWERT(02)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.
      PERFORM bdc_field       USING 'RCTMS-MNAME(01)'
                                    l_name1.
      PERFORM bdc_field       USING 'RCTMS-MWERT(01)'
                                    l_vals1.
      PERFORM bdc_field       USING 'RCTMS-MNAME(02)'
                                    l_name2.
      PERFORM bdc_field       USING 'RCTMS-MWERT(02)'
                                    l_vals2.
      PERFORM bdc_field       USING 'RCTMS-MNAME(03)'
                                    l_name3.
      PERFORM bdc_field       USING 'RCTMS-MWERT(03)'
                                    l_vals3.
      PERFORM bdc_field       USING 'RCTMS-MNAME(04)'
                                    l_name4.
      PERFORM bdc_field       USING 'RCTMS-MWERT(04)'
                                    l_vals4.
      PERFORM bdc_field       USING 'RCTMS-MNAME(05)'
                                    l_name5.
      PERFORM bdc_field       USING 'RCTMS-MWERT(05)'
                                    l_vals5.
      PERFORM bdc_field       USING 'RCTMS-MNAME(06)'
                                    l_name6.
      PERFORM bdc_field       USING 'RCTMS-MWERT(06)'
                                    l_vals6.
      PERFORM bdc_field       USING 'RCTMS-MNAME(07)'
                                    l_name7.
      PERFORM bdc_field       USING 'RCTMS-MWERT(07)'
                                    l_vals7.
      PERFORM bdc_field       USING 'RCTMS-MNAME(08)'
                                    l_name8.
      PERFORM bdc_field       USING 'RCTMS-MWERT(08)'
                                    l_vals8.
      PERFORM bdc_field       USING 'RCTMS-MNAME(09)'
                                    l_name9.
      PERFORM bdc_field       USING 'RCTMS-MWERT(09)'
                                    l_vals9.
      PERFORM bdc_field       USING 'RCTMS-MNAME(10)'
                                    l_name0.
      PERFORM bdc_field       USING 'RCTMS-MWERT(10)'
                                    l_vals0.

    ENDDO.


    PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PB21'.


    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MAKT-MAKTX'.
    PERFORM bdc_dynpro      USING 'SAPLCEI0' '0109'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RCTMS-MWERT(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM bdc_dynpro      USING 'SAPLMGMM' '5004'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'MAKT-MAKTX'.
    PERFORM bdc_transaction USING 'MM02'.

    IF sy-subrc <> 0.
      WRITE:/ wa_material, 'Error'.      wa_error = 'X'      .
    ELSE.
      WRITE:/ wa_material, 'updated'.
*      COMMIT WORK.
    ENDIF.
    CLEAR it_wosum.
    l_recno = l_recno + 1.
*    if l_recno = 50 or l_recno = 100 or l_recno = 150.
    CONCATENATE 'processed rec.' l_recno wa_material INTO l_mess.
    PERFORM display_progress_bar USING l_mess.
*    endif.

  ENDLOOP.
  COMMIT WORK.

ENDFORM.                    " DATA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MESS  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING    p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.

ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  SELECT * FROM ztpp_wosum INTO TABLE it_wosum
     WHERE wo_ser IN s_wo
      AND wocredate IN s_date.

  IF sy-subrc NE 0.
    WRITE: ' No data in WOSUM table'.
    EXIT.
  ENDIF.
ENDFORM.                    " read_data

*FORM BDC_DYNPRO USING PROGRAM DYNPRO.
*  CLEAR BDCDATA.
*  BDCDATA-PROGRAM  = PROGRAM.
*  BDCDATA-DYNPRO   = DYNPRO.
*  BDCDATA-DYNBEGIN = 'X'.
*  APPEND BDCDATA.
*ENDFORM.
*FORM BDC_FIELD USING FNAM FVAL.
*  IF FVAL <> NODATA.
*    CLEAR BDCDATA.
*    BDCDATA-FNAM = FNAM.
*    BDCDATA-FVAL = FVAL.
*    APPEND BDCDATA.
*  ENDIF.
*ENDFORM.
