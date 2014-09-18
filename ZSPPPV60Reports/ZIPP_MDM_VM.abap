************************************************************************
* Program Name      : ZIPP_MDM_VM
* Author            : Furong Wang
* Creation Date     : 12/10/2009
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Send VM to HMC
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************

REPORT zipp_mdm_vm NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.

TABLES: ztppvr.

CONSTANTS: c_cg_date TYPE d VALUE '20100731'.
DATA : l_msgtxt(100),
       l_result(1).

DATA: w_dest(20).   "Interface Destination.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztpp_mdm_hmc.
DATA: END OF it_data.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

PARAMETERS: p_opt1 RADIOBUTTON GROUP grp1,
            p_opt2 RADIOBUTTON GROUP grp1.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_bukrs LIKE t001-bukrs DEFAULT 'H201' OBLIGATORY MODIF ID
r1.
*PARAMETERS: P_DATUM LIKE SY-DATUM OBLIGATORY.
SELECT-OPTIONS: s_datum FOR sy-datum,   "OBLIGATORY
                s_status FOR ztppvr-p_status MODIF ID g11,
                s_dest FOR ztppvr-p_dest_code MODIF ID g11.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_send AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

START-OF-SELECTION.
  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE i000 WITH 'No Data'.
  ELSE.
    PERFORM save_send_data.
  ENDIF.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_data.
  DATA: l_subrc    TYPE sy-subrc ,
        l_atnam    TYPE cabn-atnam,
        l_atwrt    TYPE ausp-atwrt,
        l_name     TYPE cabn-atnam,
        l_name1    TYPE cabn-atnam,
        l_atflv    TYPE ausp-atflv,  "Prod. Date
        l_atflv_temp TYPE ausp-atflv,
        l_temp(18),
        l_datum    TYPE sy-datum,
        l_num(08)  TYPE n,
        l_exrp(02) TYPE n,
        l_wono LIKE ausp-atwrt,
        l_wono_zwosum LIKE ztpp_wosum-wo_ser,
        l_nation LIKE ztpp_wosum-nation,
        l_dealer LIKE ztpp_wosum-dealer,
        l_mi(9),
        l_altn LIKE ztbm_abyalcdt-altn,
        l_mtno LIKE ztbm_abyalcdt-mtno,
        l_lenth TYPE i,
        l_dealer1(1),
        l_excl LIKE ztpp_wosum-extc,
        l_incl LIKE ztpp_wosum-intc,
        l_seq_date LIKE sy-datum,
        l_days TYPE i,
        l_old_dealer(2),
        l_new_dealer(1),
        l_country(3),
        l_old_wo_no LIKE it_data-wo_no,
        l_fsc_29(32).

  DATA: w_fsc LIKE ztpp_mdm_hmc-fsc_no,
        w_219 TYPE zs219.

  DATA: BEGIN OF lt_objek OCCURS 0,
          objek    LIKE ausp-objek,
          atwrt    LIKE ausp-atwrt,
        END OF lt_objek.
  DATA: lt_objek_temp LIKE TABLE OF lt_objek WITH HEADER LINE.

  DATA: BEGIN OF lt_ztppvr OCCURS 0,
        p_model LIKE ztppvr-p_model,
        p_body_serial LIKE ztppvr-p_body_serial,
        k04pdat LIKE ztppvr-k04pdat,
        END OF lt_ztppvr.
  DATA: BEGIN OF lt_scrap OCCURS 0,
         model LIKE ztpp_scrap_car-model,
         body_ser LIKE ztpp_scrap_car-body_ser,
         scr_date LIKE ztpp_scrap_car-scr_date,
         END OF lt_scrap.

  DATA : BEGIN OF lt_ztppvm OCCURS 0,
         p_model LIKE ztppvm-p_model,
         p_body_serial LIKE ztppvm-p_body_serial,
      END OF lt_ztppvm.

  DATA : BEGIN OF lt_engine OCCURS 0,
          eassyid LIKE ausp-atwrt,
        END OF lt_engine.

  DATA : it_ausp_engine LIKE ausp OCCURS 0 WITH HEADER LINE.
  DATA : it_ausp_plan   LIKE ausp OCCURS 0 WITH HEADER LINE.
  DATA : it_ausp_engine_cd LIKE ausp OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF it_porder OCCURS 0,
          bodyno(10),
          plnum LIKE resb-plnum,
        END   OF it_porder.
  DATA: BEGIN OF it_eng OCCURS 0,
          plnum LIKE resb-plnum,
          matnr LIKE mara-matnr,
        END   OF it_eng.

  DATA : BEGIN OF it_engine_tmp OCCURS 0,
          objek LIKE ausp-objek,
          atwrt LIKE ausp-objek,
        END OF it_engine_tmp.

  DATA : lv_atinn      LIKE ausp-atinn.

  RANGES: r_atflv FOR ausp-atflv.

  IF s_datum[] IS INITIAL.
    MESSAGE i000 WITH 'Please Input Date'.
    EXIT.
  ENDIF.

  IF p_opt1 = 'X'.
    SELECT DISTINCT p_model p_body_serial k04pdat
       INTO TABLE lt_ztppvr
       FROM ztppvr
       WHERE flag = 'LT'
*         AND K04PDAT = P_DATUM
*         AND K04PDAT IN S_DATUM
         AND zbdat IN s_datum
         AND p_status IN s_status
         AND p_dest_code IN s_dest.
    IF sy-subrc = 0.
      LOOP AT lt_ztppvr.
        CONCATENATE lt_ztppvr-p_model lt_ztppvr-p_body_serial
                  INTO lt_objek-objek.
        COLLECT lt_objek.
        CLEAR: lt_objek.
      ENDLOOP.
*    ELSE.
*      EXIT.
    ENDIF.



*-< 08.16.2013
    SELECT p_model p_body_serial
      INTO CORRESPONDING FIELDS OF TABLE lt_ztppvm
    FROM ztppvm
    WHERE zedat IN s_datum
      GROUP BY p_model p_body_serial.

    LOOP AT lt_ztppvm.
      CLEAR : lt_objek.
      CONCATENATE lt_ztppvm-p_model lt_ztppvm-p_body_serial
                        INTO lt_objek-objek.
      APPEND lt_objek.
    ENDLOOP.

    SELECT eassyid
      INTO TABLE lt_engine
    FROM ztpperm
    WHERE zedat IN s_datum
      GROUP BY eassyid.

    IF lt_engine[] IS NOT INITIAL.
      SELECT objek
        APPENDING CORRESPONDING FIELDS OF TABLE lt_objek
      FROM ausp AS a INNER JOIN cabn AS b
                      ON b~atinn  = a~atinn
                     AND b~atnam  = 'P_ENGINE_NO'
        FOR ALL ENTRIES IN lt_engine
      WHERE a~klart EQ '002'
        AND a~atwrt = lt_engine-eassyid.
    ENDIF.
*->

** Added scrap car on 02/05/10
    CLEAR: lt_objek.
    SELECT DISTINCT model body_ser scr_date
      INTO TABLE lt_scrap
      FROM ztpp_scrap_car
      WHERE scr_date IN s_datum.

    IF sy-subrc = 0.
      LOOP AT lt_scrap.
        CONCATENATE lt_scrap-model lt_scrap-body_ser
                  INTO lt_objek-objek.
        COLLECT lt_objek.
        CLEAR: lt_objek.
      ENDLOOP.
    ENDIF.
** End of addition

  ELSE.
    REFRESH: r_atflv.
    r_atflv-option = s_datum-option.
    r_atflv-sign = s_datum-sign.
    r_atflv-low = l_num = s_datum-low.
    r_atflv-high = l_num = s_datum-high.
    APPEND r_atflv.

    REFRESH lt_objek.
*    L_NAME = 'P_STATUS'.

*    SELECT DISTINCT OBJEK
*       INTO TABLE LT_OBJEK
*       FROM AUSP AS AU
*         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*       WHERE KLART = '002'
*             AND ( AU~ATWRT <> 'V05' AND AU~ATWRT <> 'V07' )
*             AND CA~ATNAM = L_NAME.

*    L_NAME = 'P_RP25_SHOP_DATE'.
*    L_NAME1 = 'P_RP27_SHOP_DATE'.
*
*    SELECT DISTINCT OBJEK
*        INTO TABLE LT_OBJEK
**      APPENDING TABLE LT_OBJEK
*      FROM AUSP AS AU
*        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
*      WHERE KLART = '002' AND
*            AU~ATFLV IN R_ATFLV AND
*            ( CA~ATNAM = L_NAME OR CA~ATNAM = L_NAME1 ).

    l_name = 'P_RP18_SHOP_DATE'.

    SELECT DISTINCT objek
        INTO TABLE lt_objek
*      APPENDING TABLE LT_OBJEK
      FROM ausp AS au
        INNER JOIN cabn AS ca ON au~atinn = ca~atinn
      WHERE klart = '002' AND
            au~atflv IN r_atflv AND
            ca~atnam = l_name.

  ENDIF.

  SORT lt_objek BY objek.
  DELETE ADJACENT DUPLICATES FROM lt_objek. "08.16.2013

*-Read Engine code from Body # 08.16.2013 Victor
  IF lt_objek[] IS NOT INITIAL.

*-  Get Engine Serial
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'P_ENGINE_NO'
      IMPORTING
        output = lv_atinn.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp_engine
    FROM ausp
      FOR ALL ENTRIES IN  lt_objek
    WHERE objek =   lt_objek-objek
      AND atinn =   lv_atinn
      AND klart =   '002'.
    SORT it_ausp_engine BY objek.

    IF it_ausp_engine[] IS NOT INITIAL.
      CLEAR : it_engine_tmp[], it_engine_tmp.
      LOOP AT it_ausp_engine.
        it_engine_tmp-objek = it_ausp_engine-objek.
        it_engine_tmp-atwrt = it_ausp_engine-atwrt.
        APPEND  it_engine_tmp.
      ENDLOOP.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'EN_ITEM_CODE'
        IMPORTING
          output = lv_atinn.
      SELECT * INTO TABLE it_ausp_engine_cd
      FROM ausp
        FOR ALL ENTRIES IN it_engine_tmp
      WHERE objek = it_engine_tmp-atwrt
        AND atinn = lv_atinn
        AND klart =   '002'.

      SORT it_ausp_engine_cd BY objek.
    ENDIF.

*-  Get Planned Order Engine code
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'P_PLAN_ORDER'
      IMPORTING
        output = lv_atinn.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp_plan
    FROM ausp
      FOR ALL ENTRIES IN  lt_objek
    WHERE objek =   lt_objek-objek
      AND atinn =   lv_atinn
      AND klart =   '002'.
    SORT it_ausp_plan BY objek.

    LOOP AT it_ausp_plan.
      MOVE: it_ausp_plan-objek TO it_porder-bodyno,
            it_ausp_plan-atwrt TO it_porder-plnum.
      APPEND it_porder.
    ENDLOOP.

    SELECT DISTINCT a~plnum a~matnr
      INTO CORRESPONDING FIELDS OF TABLE it_eng
      FROM resb AS a INNER JOIN stpo AS b
                        ON b~idnrk = a~matnr
                       AND b~lkenz = ''
                       AND b~stlty = 'M'
                       AND b~posnr = '8000'
                       AND b~zinfo = 'ENG'
       FOR ALL ENTRIES IN it_porder
     WHERE a~plnum = it_porder-plnum.

    SORT it_porder BY bodyno.
    SORT it_eng BY plnum.

  ENDIF.


  LOOP AT lt_objek.
    CLEAR: l_atwrt, it_ausp_engine, it_ausp_engine_cd.

    PERFORM read_normal_class USING lt_objek-objek 'P_DIST_CODE'
                                CHANGING l_atwrt.

    IF l_atwrt = 'XX' OR l_atwrt = 'XY'.
      it_data-car_usf_cd = 'U'.
    ELSEIF l_atwrt = 'XA'.
      it_data-car_usf_cd = 'T'.
    ELSEIF  l_atwrt+0(1) = 'A'.
      it_data-car_usf_cd = 'M'.
    ENDIF.

    it_data-comp = p_bukrs.
    CLEAR: l_atwrt.
    PERFORM read_normal_class USING lt_objek-objek 'P_USAGE_CAR'
                                CHANGING l_atwrt.

    IF l_atwrt = 'S' OR l_atwrt = 'D'.
      it_data-prdn_car_st_cd = 'S'.
    ELSEIF l_atwrt = 'T'.
      it_data-prdn_car_st_cd = 'P'.
** Changed by Furong on 02/05/10
    ELSE.
      CLEAR: it_data-prdn_car_st_cd.
** end of change
    ENDIF.

    CLEAR: l_atwrt.
    PERFORM read_normal_class USING lt_objek-objek 'P_VIN'
                               CHANGING l_atwrt.
    it_data-vin = l_atwrt.
    CLEAR: l_atwrt.

** Changed by Furong on 01/08/10
*    IT_DATA-BODY_NO = LT_OBJEK-OBJEK+3(6).
    it_data-body_no = lt_objek-objek+0(9).
** End of change
    it_data-vin_mdl_cd = lt_objek-objek+0(3).

    PERFORM read_normal_class USING lt_objek-objek 'P_WORK_ORDER'
                                CHANGING l_atwrt.
    l_wono = l_atwrt.

** Changed by Furong on 03/24/10
*    IT_DATA-WO_NO = L_ATWRT.

    l_old_dealer = l_atwrt+12(2).

    CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
      EXPORTING
        old_dealer = l_old_dealer
      IMPORTING
        new_dealer = l_new_dealer.

    CONCATENATE l_atwrt+0(12) l_new_dealer INTO it_data-wo_no.

** Changed by Furong on 07/16/10
    CONCATENATE l_atwrt+0(12) l_old_dealer INTO l_old_wo_no.
** end of Change on 07/16/10

** End of change on 03/24/10


*    CLEAR: L_ATWRT.
*    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_AIRBAG_NO6'
*                               CHANGING L_ATWRT.
*    IT_DATA-ACU_CD = L_ATWRT.

    CLEAR: l_atwrt.
    PERFORM read_normal_class USING lt_objek-objek 'P_AIRBAG_NO10'
                               CHANGING l_atwrt.
    it_data-ado_no = l_atwrt.

    CLEAR: l_atwrt.
    PERFORM read_normal_class USING lt_objek-objek 'P_KEY_NO'
                                CHANGING l_atwrt.
    it_data-key_no = l_atwrt.
    CLEAR: l_atwrt.

    SELECT SINGLE au~atwrt
    INTO it_data-tm_code
    FROM ausp AS au
    INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = l_old_wo_no
      AND klart = '001'
* by Daniel on 08/20/10 {
*      AND CA~ATNAM = 'P_ALC_U_1'.
      AND ca~atnam = 'P_ALC_U_2'.

    CLEAR: l_atwrt.

    PERFORM read_normal_class USING lt_objek-objek 'P_ENGINE_NO'
                                  CHANGING l_atwrt.
    it_data-eng_no = l_atwrt.
    CLEAR: l_atwrt.

*--< 8/16/2013 by Victor. Engine code should be from V/M
    IF it_data-car_usf_cd <> 'U'.
      READ TABLE it_ausp_engine WITH KEY objek  =  it_data-body_no
                                         BINARY SEARCH.

      IF sy-subrc = 0 AND it_ausp_engine-atwrt IS NOT INITIAL.
*-    Read Engine code
        CLEAR: it_ausp_engine_cd.
        READ TABLE it_ausp_engine_cd WITH KEY
                                        objek = it_ausp_engine-atwrt
                                        BINARY SEARCH.

        IF sy-subrc EQ 0 AND
           it_ausp_engine_cd-atwrt IS NOT INITIAL.
          it_data-eng_code = it_ausp_engine_cd-atwrt.
        ELSE.
*-      When Engine code is empty
          CLEAR : it_porder, it_eng.
          READ TABLE it_porder WITH KEY bodyno  = it_data-body_no
                                         BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE it_eng WITH KEY plnum = it_porder-plnum
                              BINARY SEARCH.
            IF sy-subrc = 0.
              it_data-eng_code  = it_eng-matnr.
            ELSE.
              IF  it_data-prdn_car_st_cd <> 'S'.
                MESSAGE e000 WITH 'No Engine code:' it_data-body_no
                                  it_ausp_engine-atwrt.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
*-    When Engine Serial is emtpy
        CLEAR : it_porder, it_eng.
        READ TABLE it_porder WITH KEY bodyno  = it_data-body_no
                                       BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_eng WITH KEY plnum = it_porder-plnum
                            BINARY SEARCH.
          IF sy-subrc = 0.
            it_data-eng_code  = it_eng-matnr.
          ELSE.
            IF  it_data-prdn_car_st_cd <> 'S'.
              MESSAGE e000 WITH 'No Engine code:' it_data-body_no.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*-->

** Changed by Furong on 01/08/10
    SELECT SINGLE au~atwrt
      INTO it_data-acu_cd
      FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
      WHERE objek = l_old_wo_no
        AND klart = '001'
     AND ca~atnam = 'P_ALC_U_42'.
** End of change

    PERFORM read_normal_class USING lt_objek-objek 'P_TM_NO'
                                   CHANGING l_atwrt.
    it_data-tm_no = l_atwrt.
    CLEAR: l_atwrt.

    PERFORM read_normal_class USING lt_objek-objek 'P_AIRBAG_NO16'
                                    CHANGING l_atwrt.
    it_data-pin_no = l_atwrt.
    CLEAR: l_atwrt.


** Added by Furong on 03/24/10
    PERFORM read_normal_class USING lt_objek-objek 'P_DESTINATION_CODE'
                              CHANGING l_atwrt.
    it_data-elmn_gas_ctfn_no = l_atwrt.
    CLEAR: l_atwrt.
** end of addition

    PERFORM read_normal_class USING lt_objek-objek 'P_MODEL_YEAR'
                                CHANGING l_atwrt.
    it_data-fsc_year_cd = l_atwrt.
    CLEAR: l_atwrt.

    CLEAR: w_fsc, w_219.

    CALL FUNCTION 'Z_FPP_GET_FSC_219'
      EXPORTING
        i_objek  = lt_objek-objek
        i_219    = 'X'
        i_fsc_29 = 'X'
      IMPORTING
        o_fsc    = w_fsc
        o_219    = w_219.
    IF sy-subrc  = 0.
      it_data-fsc_no = w_fsc.
      it_data-o219_opt_cd = w_219.
    ENDIF.

    PERFORM read_normal_class USING lt_objek-objek 'P_EXT_COLOR'
                                CHANGING l_atwrt.
** Changed by Furong on 03/24/10

    SELECT SINGLE ctrn_key_colr INTO it_data-xrcl_cd
       FROM ztbm_abycoldt
       WHERE ctrn_cars_c = lt_objek-objek+0(2)
         AND ctrn_gubn_c = 'EXT'
         AND ctrn_conf_colr = l_atwrt
         AND ctrn_year_c2 = it_data-fsc_year_cd.
    IF sy-subrc <> 0.
      it_data-xrcl_cd = l_atwrt.
    ENDIF.
*    IT_DATA-XRCL_CD = L_ATWRT.
** End of change
    CLEAR: l_atwrt.

    PERFORM read_normal_class USING lt_objek-objek 'P_INT_COLOR'
                                CHANGING l_atwrt.
** Changed by Furong on 03/24/10
    SELECT SINGLE ctrn_key_colr INTO it_data-iecl_cd
       FROM ztbm_abycoldt
       WHERE ctrn_cars_c = lt_objek-objek+0(2)
         AND ctrn_gubn_c = 'INT'
         AND ctrn_conf_colr = l_atwrt
         AND ctrn_year_c2 = it_data-fsc_year_cd.
    IF sy-subrc <> 0.
      it_data-iecl_cd = l_atwrt.
    ENDIF.
*    IT_DATA-IECL_CD = L_ATWRT.
** End of change
    CLEAR: l_atwrt.

    it_data-prdn_corp_cd = p_bukrs.
    IF p_bukrs = 'H201'.
      it_data-body_plnt_cd = 'HVA1'.
      it_data-pntg_plnt_cd = 'HVA1'.
      it_data-trim_plnt_cd = 'HVA1'.
    ELSE.
** For KMMG
    ENDIF.

    it_data-body_ln_cd = '1'.
    it_data-pntg_ln_cd = '1'.
    it_data-trim_ln_cd = '1'.

    CLEAR: l_temp.
    PERFORM read_normal_class USING lt_objek-objek
                              'P_RP01_ACTUAL_DATE'
                              CHANGING l_temp.
    it_data-body_trwi_dt = l_temp.

    CLEAR: l_atflv_temp.
    PERFORM read_normal_class_atflv USING lt_objek-objek
                                     'P_RP01_SHOP_DATE'
                                  CHANGING l_atflv_temp.
    it_data-body_trwi_std_dt = l_num = l_atflv_temp.

    CLEAR: l_temp.
    PERFORM read_normal_class USING lt_objek-objek
                              'P_RP02_ACTUAL_DATE'
                              CHANGING l_temp.
    it_data-pntg_trwi_dt = l_temp.

    CLEAR: l_atflv_temp.
    PERFORM read_normal_class_atflv USING lt_objek-objek
                                     'P_RP02_SHOP_DATE'
                                  CHANGING l_atflv_temp.
    it_data-pntg_trwi_std_dt = l_num = l_atflv_temp.


    CLEAR: l_temp.
    PERFORM read_normal_class USING lt_objek-objek
                              'P_RP07_ACTUAL_DATE'
                              CHANGING l_temp.
    it_data-trim_trwi_dt = l_temp.

    CLEAR: l_atflv_temp.
    PERFORM read_normal_class_atflv USING lt_objek-objek
                                     'P_RP07_SHOP_DATE'
                                  CHANGING l_atflv_temp.
    it_data-trim_trwi_std_dt = l_num = l_atflv_temp.

    CLEAR: l_temp.
    PERFORM read_normal_class USING lt_objek-objek
                              'P_RP18_ACTUAL_DATE'
                              CHANGING l_temp.
    it_data-sgff_dt = l_temp.

    CLEAR: l_atflv_temp.
    PERFORM read_normal_class_atflv USING lt_objek-objek
                                     'P_RP18_SHOP_DATE'
                                  CHANGING l_atflv_temp.
    it_data-sgff_std_dt = l_num = l_atflv_temp.


*      CLEAR: L_ATFLV_TEMP.
*      PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
*                                        'P_RP02_SHOP_DATE'
*                                     CHANGING L_ATFLV_TEMP.
*      IT_DATA-PNTG_TRWI_DT = L_NUM = L_ATFLV_TEMP.
*      CLEAR: L_ATFLV_TEMP.


*      PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
*                                        'P_RP07_SHOP_DATE'
*                                     CHANGING L_ATFLV_TEMP.
*      IT_DATA-TRIM_TRWI_DT = L_NUM = L_ATFLV_TEMP.
*      CLEAR: L_ATFLV_TEMP.
*
*      PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
*                                        'P_RP18_SHOP_DATE'
*                                     CHANGING L_ATFLV_TEMP.
*      IT_DATA-SGFF_DT = L_NUM = L_ATFLV_TEMP.

** changed by furong on 03/29/10
    l_country = l_wono+9(3).
    IF l_country = 'B28'.
      CLEAR: l_atflv_temp.
      PERFORM read_normal_class_atflv USING lt_objek-objek
                                     'P_RP19_SHOP_DATE'
                                  CHANGING l_atflv_temp.
      it_data-shpg_dt = l_num = l_atflv_temp.
      l_datum = l_num = l_atflv_temp.
      IF l_datum > c_cg_date.
        CLEAR: l_atflv_temp.
        PERFORM read_normal_class_atflv USING lt_objek-objek
                                       'P_RP23_SHOP_DATE'
                                    CHANGING l_atflv_temp.
        it_data-shpg_dt = l_num = l_atflv_temp.

      ENDIF.
    ELSE.
      CLEAR: l_atflv_temp.
      PERFORM read_normal_class_atflv USING lt_objek-objek
                                     'P_RP25_SHOP_DATE'
                                  CHANGING l_atflv_temp.
      IF l_atflv_temp IS INITIAL.
        PERFORM read_normal_class_atflv USING lt_objek-objek
                                  'P_RP27_SHOP_DATE'
                               CHANGING l_atflv_temp.
        it_data-shpg_dt = l_num = l_atflv_temp.
      ELSE.
        it_data-shpg_dt = l_num = l_atflv_temp.
      ENDIF.
    ENDIF.
** changed by furong on 02/09/10
*    CLEAR: L_ATFLV_TEMP.
*    PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
*                                     'P_RP25_SHOP_DATE'
*                                  CHANGING L_ATFLV_TEMP.
*    IF L_ATFLV_TEMP IS INITIAL.
*      PERFORM READ_NORMAL_CLASS_ATFLV USING LT_OBJEK-OBJEK
*                                   'P_RP27_SHOP_DATE'
*                                CHANGING L_ATFLV_TEMP.
*      IT_DATA-SHPG_DT = L_NUM = L_ATFLV_TEMP.
*    ELSE.
*      IT_DATA-SHPG_DT = L_NUM = L_ATFLV_TEMP.
*    ENDIF.
*    CLEAR: IT_DATA-SHPG_DT.
** End of change on 02/09/10
** End of change on 03/29/10

    CLEAR: l_atflv_temp.

** changed by furong on 02/05/10
*    IF P_OPT1 = 'X'.
*      READ TABLE LT_ZTPPVR WITH KEY P_MODEL = IT_DATA-VIN_MDL_CD
*                                    P_BODY_SERIAL = IT_DATA-BODY_NO.
*      IF SY-SUBRC = 0.
*        IT_DATA-PRDN_CAR_ST_DT = L_NUM = LT_ZTPPVR-K04PDAT.
*      ENDIF.
*    ELSE.
** Changed by Furong on 02/04/10
*      IT_DATA-PRDN_CAR_ST_DT = IT_DATA-SHPG_DT.
    PERFORM read_normal_class_atflv USING lt_objek-objek
                                     'P_SCRAP_DATE'
                                  CHANGING l_atflv_temp.
    IF l_atflv_temp IS INITIAL.
      CLEAR:  it_data-prdn_car_st_dt.
    ELSE.
      it_data-prdn_car_st_dt =  l_num = l_atflv_temp.
    ENDIF.
** End of change 02/04/10
**    ENDIF.
** End of change 02/05/10

*    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_VESL_CODE'
*                                CHANGING L_ATWRT.
*    IT_DATA-SHIP_CD = L_ATWRT.
*    CLEAR: L_ATWRT.
    CLEAR: it_data-ship_cd.

*    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_VESL_NO'
*                                  CHANGING L_ATWRT.
*    IT_DATA-SHIP_NM = L_ATWRT.
*    CLEAR: L_ATWRT.
    CLEAR: it_data-ship_nm.

*    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK 'P_REGION_PORT'
*                                  CHANGING L_ATWRT.
*    IT_DATA-ARV_HABR_NM = L_ATWRT.
*    CLEAR: L_ATWRT.
    CLEAR: it_data-arv_habr_nm, it_data-arv_habr_cd.

** Changed by Furong on 01/13/11
    MOVE it_data-fsc_no+0(23) TO l_fsc_29+0(23).
    MOVE it_data-xrcl_cd TO l_fsc_29+23(3).
    MOVE it_data-iecl_cd TO l_fsc_29+26(3).
    it_data-fsc_no = l_fsc_29.
** End of change

    it_data-zuser = sy-uname.
    it_data-zsdat = sy-datum.
    it_data-zstim = sy-uzeit.
    APPEND it_data.
    CLEAR: it_data.
  ENDLOOP.
ENDFORM.                    "GET_DATA

*---------------------------------------------------------------------*
*       FORM SAVE_SEND_DATA                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM save_send_data.
  DATA: it_save LIKE TABLE OF ztpp_mdm_hmc WITH HEADER LINE,
        lt_save_log LIKE TABLE OF ztpp_mdm_hmc_log WITH HEADER LINE,
        l_flag(1),
        l_num(08) TYPE n,
        l_datum TYPE sy-datum.

  IF it_data[] IS INITIAL.
    EXIT.
  ENDIF.

  IF p_bukrs = 'H201'.
    w_dest = 'WMHR01'.
  ELSEIF p_bukrs = 'K201'.
** For KMMG
*    SELECT SINGLE DEST INTO (W_DEST)
*      FROM ZDEST
*     WHERE SY_SYSID = SY-SYSID
*       AND SY_MANDT = SY-MANDT.
  ENDIF.

  IF p_send IS INITIAL.
    LOOP AT it_data.
      MOVE-CORRESPONDING it_data TO it_save.
      APPEND it_save.
      CLEAR: it_save.
    ENDLOOP.

    DELETE FROM ztpp_mdm_hmc CLIENT SPECIFIED
              WHERE mandt = sy-mandt.
    INSERT ztpp_mdm_hmc FROM TABLE it_save.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Table saving error'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'Z_FPP_PRDTN_STATUS'
      DESTINATION w_dest
      IMPORTING
        flag                  = l_result
      TABLES
        i_prdtn_status        = it_data
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.
    CALL FUNCTION 'Z_FPP_MDM_VM'
      DESTINATION w_dest
      IMPORTING
        flag                  = l_flag
      TABLES
        i_mdm_hmc             = it_data
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.

    IF sy-subrc = 0.
      l_flag = 'S'.
      MESSAGE i001 WITH 'Data was sent successfully'.
    ELSE.
      l_flag = 'E'.
      MESSAGE i001 WITH l_msgtxt.
    ENDIF.

    LOOP AT it_data.
      MOVE-CORRESPONDING it_data TO it_save.
      it_save-zresult = l_flag.
      it_save-zmsg = l_msgtxt.
      it_save-zedat = sy-datum.
      it_save-zetim = sy-uzeit.
      APPEND it_save.
      CLEAR: it_save.
    ENDLOOP.

    DELETE FROM ztpp_mdm_hmc CLIENT SPECIFIED
             WHERE mandt = sy-mandt.
    INSERT ztpp_mdm_hmc FROM TABLE it_save.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Error: Table Updating'.
    ENDIF.
  ENDIF.
** Added by Furong on 02/02/10
  LOOP AT it_save.
    MOVE-CORRESPONDING it_save TO lt_save_log.
    lt_save_log-erdat =  sy-datum.
    lt_save_log-erzet =  sy-uzeit.
    lt_save_log-prod_date = l_num = it_save-sgff_std_dt.
    APPEND lt_save_log.
  ENDLOOP.

  l_datum = sy-datum - 90.
  DELETE FROM ztpp_mdm_hmc_log
           WHERE zedat  < l_datum.

  INSERT ztpp_mdm_hmc_log FROM TABLE lt_save_log.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE i000 WITH 'Error: Log Table Updating'.
  ENDIF.
** End of change

ENDFORM.                    "SAVE_SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
FORM read_normal_class USING p_vmno p_char
                             CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '002'       AND
          ca~atnam = p_char  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM read_normal_class_atflv USING p_vmno p_char
                             CHANGING p_value.
  SELECT SINGLE au~atflv
      INTO p_value
      FROM ausp AS au
        INNER JOIN cabn AS ca ON au~atinn = ca~atinn
      WHERE objek = p_vmno      AND
            klart = '002'       AND
            ca~atnam = p_char  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  control_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.
  LOOP AT SCREEN.
    IF p_opt2 = 'X'.
      IF screen-group1 = 'G11'.
        screen-invisible = 1.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

*      IF SCREEN-NAME = 'S_STATUS-LOW' OR
*         SCREEN-NAME = 'S_STATUS-HIGH' OR
*         SCREEN-NAME = 'S_DEST-LOW' OR
*         SCREEN-NAME = 'S_DEST-HIGH'.
*        SCREEN-INVISIBLE = 1.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*      IF SCREEN-GROUP1 = 'G11'.
*        SCREEN-INVISIBLE = 0.
*        SCREEN-INPUT = 1.
*        MODIFY SCREEN.
*      ENDIF.
*      IF SCREEN-NAME = 'S_STATUS-LOW' OR
*         SCREEN-NAME = 'S_STATUS-HIGH' OR
*         SCREEN-NAME = 'S_DEST-LOW' OR
*         SCREEN-NAME = 'S_DEST-HIGH'.
*        SCREEN-INVISIBLE = 0.
*        SCREEN-INPUT = 1.
*        MODIFY SCREEN.
*      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " control_screen
