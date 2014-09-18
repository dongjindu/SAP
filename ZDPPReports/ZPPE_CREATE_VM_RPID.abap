************************************************************************
* Program Name      : ZPPE_CREATE_VM_RPID
* Author            :
* Creation Date     : 09/14/10
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 02/22/2011 SJLee                      About OpenPo Support
************************************************************************
REPORT zppe_create_vm_rpid  NO STANDARD PAGE HEADING
                          MESSAGE-ID zmpp.

TYPE-POOLS: slis.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztppvp,
         ztpp_vm,
         ztpp_rpid,
         ztpp_scrap_car,
         likp,
         equi.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_vm  LIKE TABLE OF ztpp_vm WITH HEADER LINE,
       it_rpid LIKE TABLE OF ztpp_rpid WITH HEADER LINE.

DATA: w_datum LIKE sy-datum,
      w_uzeit LIKE sy-uzeit.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
*DATA : WA_FNAME_TX(40),
*       WA_SAVELINE_IX     LIKE  SY-INDEX.
*DATA : WA_IR         LIKE  SY-TABIX,   "Count IR
*       WA_RP         LIKE  SY-TABIX,   "Count RP
*       WA_DL         LIKE  SY-TABIX,   "Count DL
*       WA_BA         LIKE  SY-TABIX.   "Count BA
*
*DATA : WA_ATINN      TYPE  CABN-ATINN,
*       WA_OBJEK      TYPE  AUSP-OBJEK,
*       WA_ATWRT_S    TYPE  AUSP-ATWRT,
*       WA_ATWRT_E    TYPE  AUSP-ATWRT,
*       WA_ATFLV_S    TYPE  AUSP-ATFLV,
*       WA_ATFLV_E    TYPE  AUSP-ATFLV.
*
*RANGES: R_ATINN      FOR  CABN-ATINN,
*        R_ATNAM      FOR  CABN-ATNAM.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : ok_code       LIKE  sy-ucomm,
       save_ok_code  LIKE  sy-ucomm.
DATA : alv_grid               TYPE REF TO cl_gui_alv_grid,
       gs_custom_container    TYPE REF TO cl_gui_custom_container,
       wa_container           TYPE scrfname VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : gs_variant        TYPE disvariant ,  "Display Variant
       gs_layout         TYPE lvc_s_layo ,  "Layout
       gs_print          TYPE lvc_s_prnt ,  "Print control
       gt_special_groups TYPE lvc_t_sgrp ,  "Field groups
       gt_toolbar_excluding TYPE ui_functions , "Exclu Toolbar Std FUNC
       gt_header         TYPE TABLE OF slis_listheader WITH HEADER LINE,
       gt_fieldcat       TYPE lvc_t_fcat ,  "Field Catalog
       gt_sort           TYPE lvc_t_sort ,  "Sort criteria
       gt_filter         TYPE lvc_t_filt .  "Filter criteria
DATA : wa_fieldcat     TYPE lvc_s_fcat.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_mark     VALUE  'X'.


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_new RADIOBUTTON GROUP r DEFAULT 'X' USER-COMMAND new,
            p_rp RADIOBUTTON  GROUP r,
            p_sc RADIOBUTTON  GROUP r,
            p_mn RADIOBUTTON  GROUP r,
            p_bd RADIOBUTTON  GROUP r.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.

SELECT-OPTIONS : s_bodyno FOR likp-vbeln.

SELECT-OPTIONS: s_date FOR sy-datum DEFAULT sy-datum OBLIGATORY
                        MODIF ID new,
                s_time FOR sy-uzeit MODIF ID new.

SELECT-OPTIONS: s_erdat FOR ztpp_scrap_car-erdat DEFAULT sy-datum
                          MODIF ID scr,
                s_erzet FOR ztpp_scrap_car-erzet MODIF ID scr.
SELECT-OPTIONS: s_aedat FOR ztpp_scrap_car-aedat DEFAULT sy-datum
                         MODIF ID scr,
                s_aezet FOR ztpp_scrap_car-aezet MODIF ID scr.

SELECT-OPTIONS: s_mnedat FOR ztpp_scrap_car-aedat DEFAULT sy-datum
                         NO-EXTENSION
                         MODIF ID mnl,
                s_mncdat FOR equi-erdat DEFAULT sy-datum NO-EXTENSION
                         MODIF ID mnl.


SELECTION-SCREEN END OF BLOCK block2.


************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_output.
************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  PERFORM get_data.

  IF it_vm[] IS INITIAL.
    MESSAGE i001 WITH 'No valid Data exist for selected condition'.
  ELSE.
    PERFORM save_data.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: l_objek LIKE mara-matnr,
        l_date LIKE sy-datum,
        l_numc(6) TYPE n,
        l_bodyno LIKE it_rpid-body_no,
        l_dtae_c(10).
  DATA: l_vals LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  DATA: BEGIN OF lt_temp OCCURS 0,
        model_no LIKE ztppvp-modl,
        body_no LIKE ztppvp-vhno,
        END OF lt_temp.

  DATA: BEGIN OF lt_equi OCCURS 0,
         equnr LIKE equi-equnr,
         END OF lt_equi.
  CASE 'X'.
    WHEN p_new.
      SELECT modl AS model_no vhno AS body_no
        INTO TABLE lt_temp
        FROM ztppvp
          WHERE zedat IN s_date
          AND zetim IN s_time.

    WHEN p_rp.

      SELECT p_model AS model_no
             p_body_serial AS body_no
           INTO TABLE lt_temp
           FROM ztppvr
           WHERE zbdat IN s_date
             AND zetim IN s_time
             AND zresult = 'S'
   %_HINTS ORACLE 'index("ZTPPVR","ZTPPVR~Z02")'.

** Furong on 07/12/12 adding distinct in query
      SORT lt_temp BY model_no body_no.
      DELETE ADJACENT DUPLICATES FROM lt_temp
             COMPARING model_no body_no.
** End on 07/12/12

    WHEN p_sc.
      SELECT model AS model_no body_ser AS body_no
           INTO TABLE lt_temp
           FROM ztpp_scrap_car
           WHERE erdat IN s_erdat
             AND erzet IN s_erzet.

      SELECT model AS model_no body_ser AS body_no
            APPENDING TABLE lt_temp
            FROM ztpp_scrap_car
            WHERE aedat IN s_aedat
              AND aezet IN s_aezet.
    WHEN p_mn.
      SELECT equnr INTO TABLE lt_equi
           FROM  equi
           WHERE eqtyp = 'V'
             AND eqart = '1000'
             AND aedat IN s_mnedat
             AND erdat IN s_mncdat.
*           AND AEDAT IN S_AEDAT .
*     WHERE AEDAT IN S_AEDAT.


      REFRESH lt_temp.
      LOOP AT lt_equi.
        lt_temp-model_no = lt_equi+0(3).
        lt_temp-body_no = lt_equi+3(6).
        APPEND lt_temp.
      ENDLOOP.

    WHEN p_bd.
      IF s_bodyno[] IS INITIAL.
        MESSAGE s000 WITH text-w01.
        EXIT.
      ENDIF.
      REFRESH lt_temp.

      DATA: BEGIN OF lt_vbeln OCCURS 1000,
              vbeln LIKE likp-vbeln,
            END OF lt_vbeln.
      REFRESH lt_vbeln.
      SELECT vbeln INTO TABLE lt_vbeln
         FROM likp
         WHERE vbeln IN s_bodyno
         ORDER BY vbeln.
      IF sy-subrc = 0.
        LOOP AT lt_vbeln.
          PERFORM conversion_output USING lt_vbeln-vbeln.
          lt_temp-model_no = lt_vbeln-vbeln+0(3).
          lt_temp-body_no  = lt_vbeln-vbeln+3(6).
          APPEND lt_temp.
        ENDLOOP.
      ELSE.
        LOOP AT s_bodyno.
          PERFORM conversion_output USING s_bodyno-low.
          lt_temp-model_no = s_bodyno-low+0(3).
          lt_temp-body_no  = s_bodyno-low+3(6).
          APPEND lt_temp.
        ENDLOOP.
      ENDIF.
  ENDCASE.

  w_datum  = sy-datum.
  w_uzeit = sy-uzeit.

  CLEAR: l_vals, l_vals[].
  PERFORM append_char TABLES l_vals .

  LOOP AT lt_temp.
    CONCATENATE lt_temp-model_no lt_temp-body_no INTO l_objek.
*    message s001 with L_OBJEK ' ...processing'.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER_READ'
      EXPORTING
        object       = l_objek
      TABLES
        val_table    = l_vals
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        OTHERS       = 4.

    LOOP AT l_vals.
      CASE l_vals-atnam.
        WHEN 'P_USAGE_CAR'.
          it_vm-usg_car = l_vals-atwrt.
        WHEN 'P_USAGE_TYPE'.
          it_vm-usage_type = l_vals-atwrt.
        WHEN 'P_USAGE_DEPT'.
          it_vm-usg_dept = l_vals-atwrt.
        WHEN 'P_USAGE_TEXT'.
          it_vm-usg_text = l_vals-atwrt.

        WHEN 'P_SCRAP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_VM-SCRAP_DATE = L_DATE.
          it_vm-scrap_date = l_vals-atwrt.
        WHEN 'P_SCRAP_RP'.
          it_vm-scrap_rpid = l_vals-atwrt.
        WHEN 'P_SCRAP_ACCT'.
          it_vm-scrap_acc_no = l_vals-atwrt.
        WHEN 'P_SCRAP_NO'.
          it_vm-scrap_no  = l_vals-atwrt.
        WHEN 'P_SCRAP_APP_DOC'.
          it_vm-scrap_app_doc = l_vals-atwrt.
        WHEN 'P_ENGINE_NO'.
          it_vm-eng_no = l_vals-atwrt.
        WHEN 'P_TM_NO'.
          it_vm-tm_no = l_vals-atwrt.
        WHEN 'P_KEY_NO'.
          it_vm-key_no = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO1'.
          it_vm-airbag_no01 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO2'.
          it_vm-airbag_no02 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO3'.
          it_vm-airbag_no03 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO4'.
          it_vm-airbag_no04 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO5'.
          it_vm-airbag_no05 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO6'.
          it_vm-airbag_no06 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO7'.
          it_vm-airbag_no07 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO8'.
          it_vm-airbag_no08 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO9'.
          it_vm-airbag_no09 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO10'.
          it_vm-airbag_no10 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO11'.
          it_vm-airbag_no11 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO12'.
          it_vm-airbag_no12 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO13'.
          it_vm-airbag_no13 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO14'.
          it_vm-airbag_no14 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO15'.
          it_vm-airbag_no15 = l_vals-atwrt.
        WHEN 'P_AIRBAG_NO16'.
          it_vm-airbag_no16 = l_vals-atwrt.
        WHEN 'P_MODEL'.
          it_vm-model_code = l_vals-atwrt.
          it_rpid-model_code =  l_vals-atwrt.
        WHEN 'P_BODY_SERIAL'.
          it_vm-body_no = l_vals-atwrt.
          it_rpid-body_no = l_vals-atwrt.
*              l_bodyno = l_numc = L_VALS-ATWRT.
*             IT_RPID-BODY_NO = l_bodyno.
        WHEN 'P_WORK_ORDER'.
          it_vm-wo_serial = l_vals-atwrt+0(9).
          it_vm-wo_nation = l_vals-atwrt+9(3).
          it_vm-wo_dealer = l_vals-atwrt+12(2).
        WHEN 'P_EXT_COLOR'.
          it_vm-extc = l_vals-atwrt.
        WHEN 'P_INT_COLOR'.
          it_vm-intc = l_vals-atwrt.
        WHEN 'P_BC_RP'.
          it_vm-pre_rpid = l_vals-atwrt.
        WHEN 'P_FIXED_WEEK'.
          it_vm-fix_week = l_vals-atwrt.
        WHEN 'P_MITU'.
          it_vm-mitu = l_vals-atwrt.
        WHEN 'P_MITU_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          	IT_VM-MITU_DATE = L_DATE.
          it_vm-mitu_date = l_vals-atwrt.
        WHEN 'P_BODY_PLANT_NO'.
          it_vm-body_plant = l_vals-atwrt.
        WHEN 'P_BODY_LINE_NO'.
          it_vm-body_line = l_vals-atwrt.
        WHEN 'P_PAINT_PLANT_NO'.
          it_vm-paint_plant = l_vals-atwrt.
        WHEN 'P_PAINT_LINE_NO'.
          it_vm-paint_line = l_vals-atwrt.
        WHEN 'P_TRIM_PLANT_NO'.
          it_vm-trim_plant = l_vals-atwrt.
        WHEN 'P_TRIM_LINE_NO'.
          it_vm-trim_line = l_vals-atwrt.
        WHEN 'P_VIN'.
          it_vm-vin = l_vals-atwrt.
        WHEN 'P_STATUS'.
          it_vm-tp_cstatus = l_vals-atwrt.
        WHEN 'P_COATING'.
          it_vm-coat = l_vals-atwrt.
        WHEN 'P_FINAL_BF_DATE'.
          it_vm-bf_date = l_vals-atwrt.
        WHEN 'P_VM_DATE'.
          l_date = l_dtae_c = l_vals-atwrt+0(8).
          it_vm-vmc_date = l_date.
*        WHEN 'P_DIST_CODE'.
*          	IT_VM-DEST_CODE = L_VALS-ATWRT.
        WHEN 'P_SEQUENCE_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          	IT_VM-SEQ_DATE = L_DATE.
          it_vm-seq_date = l_vals-atwrt.
        WHEN 'P_SEQUENCE_SERIAL'.
          it_vm-seq_ser = l_vals-atwrt.
        WHEN 'P_SEQUENCE_CODE'.
          it_vm-seq_code = l_vals-atwrt.
        WHEN 'P_PROBLEM'.
          it_vm-problem = l_vals-atwrt.

        WHEN 'P_MODEL_YEAR'.
          it_vm-model_year = l_vals-atwrt.
          it_vm-pre_fsc(1) = l_vals-atwrt(1).
        WHEN 'P_MI'.
          it_vm-mi         = l_vals-atwrt.
        WHEN 'P_OCN'.
          it_vm-ocn        = l_vals-atwrt.

        WHEN 'P_DESTINATION_CODE'.
          it_vm-dest_code = l_vals-atwrt.
          it_vm-pre_fsc+1(5) = l_vals-atwrt(5).
        WHEN 'P_BC_MI'.
          it_vm-pre_fsc+6(9) = l_vals-atwrt(9).
        WHEN 'P_BC_OCN'.
          it_vm-pre_fsc+15(4) = l_vals-atwrt(4).
        WHEN 'P_BC_VERSION'.
          it_vm-pre_fsc+19(3) = l_vals-atwrt+1(3).

        WHEN 'P_BC_WORK_ORDER'.
          it_vm-pre_wo_no+0(9)  = l_vals-atwrt(9).
        WHEN 'P_BC_EXT_COLOR'.
          it_vm-pre_wo_no+9(3)  = l_vals-atwrt(3).
        WHEN 'P_BC_INT_COLOR'.
          it_vm-pre_wo_no+12(3) = l_vals-atwrt(3).


        WHEN 'P_SALES_ORDER'.
          it_vm-sales_order = l_vals-atwrt.
        WHEN 'P_SALES_ORDER_NEW'.
          it_vm-sales_order_new = l_vals-atwrt.
        WHEN 'P_PLAN_ORDER'.
          it_vm-pldord = l_vals-atwrt.
        WHEN 'P_DEALER_NO'.
          it_vm-dealer_no = l_vals-atwrt.

        WHEN 'P_VESL_CODE'.
          it_vm-vessel_code = l_vals-atwrt.
        WHEN 'P_VESL_NO'.
          it_vm-vessel_no = l_vals-atwrt.
        WHEN 'P_VESL_DEST'.
          it_vm-vessel_dest = l_vals-atwrt.
        WHEN 'P_VERSION'.
          it_vm-ver  = l_vals-atwrt+1(3).
        WHEN 'P_RP_STATUS'.
          it_vm-rp_cstatus = l_vals-atwrt.
        WHEN 'P_COLOR_SER'.
          it_vm-color_ser = l_vals-atwrt.
        WHEN 'P_ORDER_ZONE'.
          it_vm-order_zone = l_vals-atwrt.
        WHEN 'P_REGION_PORT'.
          it_vm-region_port = l_vals-atwrt.
        WHEN 'P_FLEET'.
          it_vm-fleet = l_vals-atwrt.

        WHEN 'P_EMISSION'.
          it_vm-emission = l_vals-atwrt.
        WHEN 'P_EMISSION_DATE'.
          CONCATENATE l_vals-atwrt+6(4) l_vals-atwrt+0(2)
          l_vals-atwrt+3(2) INTO l_dtae_c.
          WRITE l_dtae_c TO l_date MM/DD/YYYY .
          it_vm-emission_date = l_date.
        WHEN 'P_RETURN_NO'.
          it_vm-return_no = l_vals-atwrt.
        WHEN 'P_RETURN_REASON'.
          it_vm-return_reason = l_vals-atwrt.
        WHEN 'P_RETURN_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_VM-RETURN_DATE = L_DATE.
          it_vm-return_date = l_vals-atwrt.
        WHEN 'P_RETURN_REWORK_DATE'.
          CONCATENATE l_vals-atwrt+6(4) l_vals-atwrt+0(2)
          l_vals-atwrt+3(2) INTO l_dtae_c.
          WRITE l_dtae_c TO l_date MM/DD/YYYY .
          it_vm-return_rework_da = l_date.
        WHEN 'P_BC_DVRS'.
          it_vm-pre_dvrs  = l_vals-atwrt.
        WHEN 'P_BC_CHANGE_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*               L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          	IT_VM-PRE_CHANGE_DATE = L_DATE.
          it_vm-pre_change_date =  l_vals-atwrt.
*-----<< 6/19/2013 BS Bae.
*-----// Fill chg_date and chg_time.
          it_vm-chg_date = l_vals-atwrt(8).
          it_vm-chg_time = l_vals-atwrt+8(6).
*----->> 6/19/2013 BS Bae.

        WHEN 'P_BC_PLAN_ORDER'.
          it_vm-pre_plan_order  = l_vals-atwrt.
        WHEN 'P_TENDER_NO'.
          it_vm-tender_no  = l_vals-atwrt.
        WHEN 'P_EPI_CODE'.
          it_vm-epi_code  = l_vals-atwrt.
        WHEN 'P_MANUAL_ORDER'.
          it_vm-manual_order  = l_vals-atwrt.
        WHEN 'P_BL_NO'.
          it_vm-bl_no  = l_vals-atwrt.
        WHEN 'P_LC_NO'.
          it_vm-lc_no  = l_vals-atwrt.

        WHEN 'P_RP01_ACTUAL_DATE'.
          it_rpid-rp01_adate = l_vals-atwrt.
        WHEN 'P_RP01_SERIAL'.
          it_rpid-rp01_serial = l_vals-atwrt.
        WHEN 'P_RP01_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP01_SDATE = L_DATE.
          it_rpid-rp01_sdate = l_vals-atwrt.
        WHEN 'P_RP02_ACTUAL_DATE'.
          it_rpid-rp02_adate = l_vals-atwrt.
        WHEN 'P_RP02_SERIAL'.
          it_rpid-rp02_serial = l_vals-atwrt.
        WHEN 'P_RP02_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP02_SDATE = L_DATE.
          it_rpid-rp02_sdate = l_vals-atwrt.
        WHEN 'P_RP03_ACTUAL_DATE'.
          it_rpid-rp03_adate = l_vals-atwrt.
        WHEN 'P_RP03_SERIAL'.
          it_rpid-rp03_serial = l_vals-atwrt.
        WHEN 'P_RP03_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP03_SDATE = L_DATE.
          it_rpid-rp03_sdate = l_vals-atwrt.
        WHEN 'P_RP04_ACTUAL_DATE'.
          it_rpid-rp04_adate = l_vals-atwrt.
        WHEN 'P_RP04_SERIAL'.
          it_rpid-rp04_serial = l_vals-atwrt.
        WHEN 'P_RP04_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP04_SDATE = L_DATE.
          it_rpid-rp04_sdate = l_vals-atwrt.
        WHEN 'P_RP05_ACTUAL_DATE'.
          it_rpid-rp05_adate = l_vals-atwrt.
        WHEN 'P_RP05_SERIAL'.
          it_rpid-rp05_serial = l_vals-atwrt.
        WHEN 'P_RP05_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP05_SDATE = L_DATE.
          it_rpid-rp05_sdate = l_vals-atwrt.
        WHEN 'P_RP06_ACTUAL_DATE'.
          it_rpid-rp06_adate = l_vals-atwrt.
        WHEN 'P_RP06_SERIAL'.
          it_rpid-rp06_serial = l_vals-atwrt.
        WHEN 'P_RP06_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP06_SDATE = L_DATE.
          it_rpid-rp06_sdate = l_vals-atwrt.

        WHEN 'P_RP07_ACTUAL_DATE'.
          it_rpid-rp07_adate = l_vals-atwrt.
        WHEN 'P_RP07_SERIAL'.
          it_rpid-rp07_serial = l_vals-atwrt.
        WHEN 'P_RP07_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP07_SDATE = L_DATE.
          it_rpid-rp07_sdate = l_vals-atwrt.

        WHEN 'P_RP08_ACTUAL_DATE'.
          it_rpid-rp08_adate = l_vals-atwrt.
        WHEN 'P_RP08_SERIAL'.
          it_rpid-rp08_serial = l_vals-atwrt.
        WHEN 'P_RP08_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP08_SDATE = L_DATE.
          it_rpid-rp08_sdate = l_vals-atwrt.

        WHEN 'P_RP09_ACTUAL_DATE'.
          it_rpid-rp09_adate = l_vals-atwrt.
        WHEN 'P_RP09_SERIAL'.
          it_rpid-rp09_serial = l_vals-atwrt.
        WHEN 'P_RP09_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP09_SDATE = L_DATE.
          it_rpid-rp09_sdate = l_vals-atwrt.

        WHEN 'P_RP10_ACTUAL_DATE'.
          it_rpid-rp10_adate = l_vals-atwrt.
        WHEN 'P_RP10_SERIAL'.
          it_rpid-rp10_serial = l_vals-atwrt.
        WHEN 'P_RP10_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP10_SDATE = L_DATE.
          it_rpid-rp10_sdate = l_vals-atwrt.

        WHEN 'P_RP11_ACTUAL_DATE'.
          it_rpid-rp11_adate = l_vals-atwrt.
        WHEN 'P_RP11_SERIAL'.
          it_rpid-rp11_serial = l_vals-atwrt.
        WHEN 'P_RP11_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP11_SDATE = L_DATE.
          it_rpid-rp11_sdate = l_vals-atwrt.

        WHEN 'P_RP12_ACTUAL_DATE'.
          it_rpid-rp12_adate = l_vals-atwrt.
        WHEN 'P_RP12_SERIAL'.
          it_rpid-rp12_serial = l_vals-atwrt.
        WHEN 'P_RP12_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP12_SDATE = L_DATE.
          it_rpid-rp12_sdate = l_vals-atwrt.

        WHEN 'P_RP13_ACTUAL_DATE'.
          it_rpid-rp13_adate = l_vals-atwrt.
        WHEN 'P_RP13_SERIAL'.
          it_rpid-rp13_serial = l_vals-atwrt.
        WHEN 'P_RP13_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP13_SDATE = L_DATE.
          it_rpid-rp13_sdate = l_vals-atwrt.

        WHEN 'P_RP14_ACTUAL_DATE'.
          it_rpid-rp14_adate = l_vals-atwrt.
        WHEN 'P_RP14_SERIAL'.
          it_rpid-rp14_serial = l_vals-atwrt.
        WHEN 'P_RP14_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP14_SDATE = L_DATE.
          it_rpid-rp14_sdate = l_vals-atwrt.

        WHEN 'P_RP15_ACTUAL_DATE'.
          it_rpid-rp15_adate = l_vals-atwrt.
        WHEN 'P_RP15_SERIAL'.
          it_rpid-rp15_serial = l_vals-atwrt.
        WHEN 'P_RP15_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP15_SDATE = L_DATE.
          it_rpid-rp15_sdate = l_vals-atwrt.

        WHEN 'P_RP16_ACTUAL_DATE'.
          it_rpid-rp16_adate = l_vals-atwrt.
        WHEN 'P_RP16_SERIAL'.
          it_rpid-rp16_serial = l_vals-atwrt.
        WHEN 'P_RP16_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP16_SDATE = L_DATE.
          it_rpid-rp16_sdate = l_vals-atwrt.

        WHEN 'P_RP17_ACTUAL_DATE'.
          it_rpid-rp17_adate = l_vals-atwrt.
        WHEN 'P_RP17_SERIAL'.
          it_rpid-rp17_serial = l_vals-atwrt.
        WHEN 'P_RP17_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP17_SDATE = L_DATE.
          it_rpid-rp17_sdate = l_vals-atwrt.

        WHEN 'P_RP18_ACTUAL_DATE'.
          it_rpid-rp18_adate = l_vals-atwrt.
        WHEN 'P_RP18_SERIAL'.
          it_rpid-rp18_serial = l_vals-atwrt.
        WHEN 'P_RP18_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP18_SDATE = L_DATE.
          it_rpid-rp18_sdate = l_vals-atwrt.

        WHEN 'P_RP19_ACTUAL_DATE'.
          it_rpid-rp19_adate = l_vals-atwrt.
        WHEN 'P_RP19_SERIAL'.
          it_rpid-rp19_serial = l_vals-atwrt.
        WHEN 'P_RP19_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP19_SDATE = L_DATE.
          it_rpid-rp19_sdate = l_vals-atwrt.

        WHEN 'P_RP20_ACTUAL_DATE'.
          it_rpid-rp20_adate = l_vals-atwrt.
        WHEN 'P_RP20_SERIAL'.
          it_rpid-rp20_serial = l_vals-atwrt.
        WHEN 'P_RP20_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP20_SDATE = L_DATE.
          it_rpid-rp20_sdate = l_vals-atwrt.

        WHEN 'P_RP21_ACTUAL_DATE'.
          it_rpid-rp21_adate = l_vals-atwrt.
        WHEN 'P_RP21_SERIAL'.
          it_rpid-rp21_serial = l_vals-atwrt.
        WHEN 'P_RP21_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP21_SDATE = L_DATE.
          it_rpid-rp21_sdate = l_vals-atwrt.

        WHEN 'P_RP22_ACTUAL_DATE'.
          it_rpid-rp22_adate = l_vals-atwrt.
        WHEN 'P_RP22_SERIAL'.
          it_rpid-rp22_serial = l_vals-atwrt.
        WHEN 'P_RP22_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP22_SDATE = L_DATE.
          it_rpid-rp22_sdate = l_vals-atwrt.

        WHEN 'P_RP23_ACTUAL_DATE'.
          it_rpid-rp23_adate = l_vals-atwrt.
        WHEN 'P_RP23_SERIAL'.
          it_rpid-rp23_serial = l_vals-atwrt.
        WHEN 'P_RP23_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP23_SDATE = L_DATE.
          it_rpid-rp23_sdate = l_vals-atwrt.

        WHEN 'P_RP24_ACTUAL_DATE'.
          it_rpid-rp24_adate = l_vals-atwrt.
        WHEN 'P_RP24_SERIAL'.
          it_rpid-rp24_serial = l_vals-atwrt.
        WHEN 'P_RP24_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP24_SDATE = L_DATE.
          it_rpid-rp24_sdate = l_vals-atwrt.

        WHEN 'P_RP25_ACTUAL_DATE'.
          it_rpid-rp25_adate = l_vals-atwrt.
        WHEN 'P_RP25_SERIAL'.
          it_rpid-rp25_serial = l_vals-atwrt.
        WHEN 'P_RP25_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP25_SDATE = L_DATE.
          it_rpid-rp25_sdate = l_vals-atwrt.

        WHEN 'P_RP26_ACTUAL_DATE'.
          it_rpid-rp26_adate = l_vals-atwrt.
        WHEN 'P_RP26_SERIAL'.
          it_rpid-rp26_serial = l_vals-atwrt.
        WHEN 'P_RP26_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP26_SDATE = L_DATE.
          it_rpid-rp26_sdate = l_vals-atwrt.

        WHEN 'P_RP27_ACTUAL_DATE'.
          it_rpid-rp27_adate = l_vals-atwrt.
        WHEN 'P_RP27_SERIAL'.
          it_rpid-rp27_serial = l_vals-atwrt.
        WHEN 'P_RP27_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY .
*          IT_RPID-RP27_SDATE = L_DATE.
          it_rpid-rp27_sdate = l_vals-atwrt.

        WHEN 'P_RP28_ACTUAL_DATE'.
          it_rpid-rp28_adate = l_vals-atwrt.
        WHEN 'P_RP28_SERIAL'.
          it_rpid-rp28_serial = l_vals-atwrt.
        WHEN 'P_RP28_SHOP_DATE'.
*          CONCATENATE L_VALS-ATWRT+6(4) L_VALS-ATWRT+0(2)
*          L_VALS-ATWRT+3(2) INTO L_DTAE_C.
*          WRITE L_DTAE_C TO L_DATE MM/DD/YYYY.
*          IT_RPID-RP28_SDATE = L_DATE.
          it_rpid-rp28_sdate = l_vals-atwrt.

      ENDCASE.
    ENDLOOP.

*    IF P_NEW = 'X'.
*      IT_VM-ERDAT = W_DATUM.
*      IT_VM-ERNAM = SY-UNAME.
*      IT_VM-ERZET = W_UZEIT.
*      IT_RPID-ERDAT = W_DATUM.
*      IT_RPID-ERNAM = SY-UNAME.
*      IT_RPID-ERZET = W_UZEIT.
*    ENDIF.
*
*    IT_VM-AEDAT = W_DATUM.
*    IT_VM-AENAM = SY-UNAME.
*    IT_VM-AEZET = W_UZEIT.
*
*    IT_RPID-AEDAT = W_DATUM.
*    IT_RPID-AENAM = SY-UNAME.
*    IT_RPID-AEZET = W_UZEIT.

    APPEND it_vm.
    APPEND it_rpid.
    CLEAR: it_vm, it_rpid.

  ENDLOOP.
ENDFORM.                    " GET_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  APPEND_CHAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VALS  text
*----------------------------------------------------------------------*
FORM append_char TABLES   l_vals STRUCTURE zspp_vin_value .
  l_vals-atnam = 'P_SCRAP_NO'.         APPEND l_vals.

  l_vals-atnam = 'P_USAGE_CAR'.              APPEND l_vals.
  l_vals-atnam = 'P_USAGE_TYPE'.              APPEND l_vals.
  l_vals-atnam = 'P_USAGE_DEPT'.              APPEND l_vals.
  l_vals-atnam = 'P_USAGE_TEXT'.              APPEND l_vals.
  l_vals-atnam = 'P_SCRAP_DATE'.              APPEND l_vals.
  l_vals-atnam = 'P_SCRAP_RP'.              APPEND l_vals.
  l_vals-atnam = 'P_SCRAP_ACCT'.              APPEND l_vals.
  l_vals-atnam = 'P_SCRAP_APP_DOC'.            APPEND l_vals.
  l_vals-atnam = 'P_ENGINE_NO'.              APPEND l_vals.
  l_vals-atnam = 'P_TM_NO'.              APPEND l_vals.

  l_vals-atnam = 'P_KEY_NO'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO1'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO2'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO3'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO4'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO5'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO6'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO7'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO8'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO9'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO10'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO11'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO12'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO13'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO14'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO15'.              APPEND l_vals.
  l_vals-atnam = 'P_AIRBAG_NO16'.              APPEND l_vals.

  l_vals-atnam = 'P_MODEL'.         APPEND l_vals.
  l_vals-atnam = 'P_BODY_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_WORK_ORDER'.         APPEND l_vals.
  l_vals-atnam = 'P_EXT_COLOR'.         APPEND l_vals.
  l_vals-atnam = 'P_INT_COLOR'.         APPEND l_vals.
  l_vals-atnam = 'P_MODEL_YEAR'.        APPEND l_vals.
  l_vals-atnam = 'P_MI'.          APPEND l_vals.
  l_vals-atnam = 'P_OCN'.         APPEND l_vals.
  l_vals-atnam = 'P_VERSION'.         APPEND l_vals.

  l_vals-atnam = 'P_BC_WORK_ORDER'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_EXT_COLOR'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_INT_COLOR'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_MI'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_OCN'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_VERSION'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_RP'.         APPEND l_vals.

  l_vals-atnam = 'P_DESTINATION_CODE'.   APPEND l_vals.
  l_vals-atnam = 'P_FIXED_WEEK'.         APPEND l_vals.
  l_vals-atnam = 'P_MITU'.         APPEND l_vals.
  l_vals-atnam = 'P_MITU_DATE'.         APPEND l_vals.


  l_vals-atnam = 'P_BODY_PLANT_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_BODY_LINE_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_PAINT_PLANT_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_PAINT_LINE_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_TRIM_PLANT_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_TRIM_LINE_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_VIN'.         APPEND l_vals.
  l_vals-atnam = 'P_STATUS'.         APPEND l_vals.
  l_vals-atnam = 'P_COATING'.         APPEND l_vals.
  l_vals-atnam = 'P_FINAL_BF_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_VM_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_LC_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_NATN_CODE'.         APPEND l_vals.
  l_vals-atnam = 'P_DIST_CODE'.         APPEND l_vals.
  l_vals-atnam = 'P_SEQUENCE_DATE'.         APPEND l_vals.

  l_vals-atnam = 'P_SEQUENCE_SERIAL'.         APPEND l_vals.

  l_vals-atnam = 'P_SEQUENCE_CODE'.         APPEND l_vals.
  l_vals-atnam = 'P_PROBLEM'.         APPEND l_vals.



  l_vals-atnam = 'P_SALES_ORDER'.         APPEND l_vals.
  l_vals-atnam = 'P_SALES_ORDER_NEW'.         APPEND l_vals.
  l_vals-atnam = 'P_PLAN_ORDER'.         APPEND l_vals.
  l_vals-atnam = 'P_DEALER_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_VESL_CODE'.         APPEND l_vals.
  l_vals-atnam = 'P_VESL_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_VESL_DEST'.         APPEND l_vals.
  l_vals-atnam = 'P_RP_STATUS'.         APPEND l_vals.
  l_vals-atnam = 'P_COLOR_SER'.         APPEND l_vals.
  l_vals-atnam = 'P_ORDER_ZONE'.         APPEND l_vals.
  l_vals-atnam = 'P_REGION_PORT'.         APPEND l_vals.
  l_vals-atnam = 'P_FLEET'.         APPEND l_vals.

  l_vals-atnam = 'P_EMISSION'.         APPEND l_vals.
  l_vals-atnam = 'P_EMISSION_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RETURN_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_RETURN_REASON'.         APPEND l_vals.
  l_vals-atnam = 'P_RETURN_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RETURN_REWORK_DATE'.         APPEND l_vals.

  l_vals-atnam = 'P_BC_DVRS'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_CHANGE_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_BC_PLAN_ORDER'.         APPEND l_vals.
  l_vals-atnam = 'P_TENDER_NO'.         APPEND l_vals.
  l_vals-atnam = 'P_EPI_CODE'.         APPEND l_vals.
  l_vals-atnam = 'P_MANUAL_ORDER'.         APPEND l_vals.
  l_vals-atnam = 'P_BL_NO'.        APPEND l_vals.

** RPID
  l_vals-atnam = 'P_RP01_ACTUAL_DATE'.   APPEND l_vals.
  l_vals-atnam = 'P_RP01_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP01_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP02_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP02_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP02_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP03_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP03_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP03_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP04_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP04_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP04_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP05_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP05_SERIAL'.         APPEND l_vals.

  l_vals-atnam = 'P_RP05_SHOP_DATE'.         APPEND l_vals.

  l_vals-atnam = 'P_RP06_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP06_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP06_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP07_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP07_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP07_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP08_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP08_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP08_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP09_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP09_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP09_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP10_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP10_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP10_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP11_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP11_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP11_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP12_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP12_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP12_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP13_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP13_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP13_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP14_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP14_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP14_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP15_ACTUAL_DATE'.         APPEND l_vals.

  l_vals-atnam = 'P_RP15_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP15_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP16_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP16_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP16_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP17_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP17_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP17_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP18_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP18_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP18_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP19_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP19_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP19_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP20_ACTUAL_DATE'.         APPEND l_vals.

  l_vals-atnam = 'P_RP20_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP20_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP21_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP21_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP21_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP22_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP22_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP22_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP23_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP23_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP23_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP24_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP24_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP24_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP25_ACTUAL_DATE'.         APPEND l_vals.

  l_vals-atnam = 'P_RP25_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP25_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP26_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP26_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP26_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP27_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP27_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP27_SHOP_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP28_ACTUAL_DATE'.         APPEND l_vals.
  l_vals-atnam = 'P_RP28_SERIAL'.         APPEND l_vals.
  l_vals-atnam = 'P_RP28_SHOP_DATE'.         APPEND l_vals.

ENDFORM.                    " APPEND_CHAR
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  DATA: lt_vm_bf   LIKE ztpp_vm.   " WITH HEADER LINE.
*  DATA: LT_VMi     like ZTPP_VM   WITH HEADER LINE.
*  DATA: LT_VMu     like ZTPP_VM   WITH HEADER LINE.

  DATA: lt_rpid_bf LIKE ztpp_rpid. " WITH HEADER LINE.
  DATA: l_index LIKE sy-tabix.

  LOOP AT it_vm.
    l_index = sy-tabix.

    IF p_new = 'X'.
      it_vm-erdat = w_datum.
      it_vm-erzet = w_uzeit.
      it_vm-ernam = sy-uname.
      MODIFY it_vm INDEX l_index TRANSPORTING erdat erzet ernam.
    ELSE.
      SELECT SINGLE * INTO lt_vm_bf
        FROM ztpp_vm
        WHERE model_code = it_vm-model_code
          AND body_no    = it_vm-body_no.
      IF sy-subrc <> 0.  "NEW
        it_vm-erdat = w_datum.
        it_vm-erzet = w_uzeit.
        it_vm-ernam = sy-uname.

        MODIFY it_vm INDEX l_index TRANSPORTING erdat erzet ernam.
      ELSE.             "Exisitng
        CLEAR: lt_vm_bf-erdat, lt_vm_bf-erzet, lt_vm_bf-ernam,
               lt_vm_bf-aedat, lt_vm_bf-aezet, lt_vm_bf-aenam.

        IF lt_vm_bf = it_vm.  "SAME record
          DELETE it_vm INDEX l_index.
        ELSE.
          it_vm-aedat = w_datum.
          it_vm-aezet = w_uzeit.
          it_vm-aenam = sy-uname.
          MODIFY it_vm INDEX l_index TRANSPORTING aedat aezet aenam.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
*-save to DB
  MODIFY ztpp_vm FROM TABLE it_vm.

  LOOP AT it_rpid.
    l_index = sy-tabix.

    IF p_new = 'X'.
      it_rpid-erdat = w_datum.
      it_rpid-erzet = w_uzeit.
      it_rpid-ernam = sy-uname.
      MODIFY it_rpid INDEX l_index TRANSPORTING erdat erzet ernam.
    ELSE.
      SELECT SINGLE * INTO lt_rpid_bf
        FROM ztpp_rpid
        WHERE model_code = it_rpid-model_code
          AND body_no    = it_rpid-body_no.
      IF sy-subrc <> 0.  "NEW
        it_rpid-erdat = w_datum.
        it_rpid-erzet = w_uzeit.
        it_rpid-ernam = sy-uname.
        MODIFY it_rpid INDEX l_index TRANSPORTING erdat erzet ernam.
      ELSE.             "Exisitng
        CLEAR: lt_rpid_bf-erdat, lt_rpid_bf-erzet, lt_rpid_bf-ernam,
               lt_rpid_bf-aedat, lt_rpid_bf-aezet, lt_rpid_bf-aenam.

        IF lt_rpid_bf = it_rpid.  "SAME record
          DELETE it_rpid INDEX l_index.
        ELSE.
          it_rpid-aedat = w_datum.
          it_rpid-aezet = w_uzeit.
          it_rpid-aenam = sy-uname.
          MODIFY it_rpid INDEX l_index TRANSPORTING aedat aezet aenam.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  MODIFY ztpp_rpid FROM TABLE it_rpid.

  IF sy-subrc = 0.
    COMMIT WORK.
    DESCRIBE TABLE it_vm LINES sy-index.
    MESSAGE s001 WITH 'Successfully updated... ' sy-index.
  ELSE.
    ROLLBACK WORK.
    MESSAGE w000 WITH 'Unsuccessfully updated'.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_output.
  CASE 'X'.
    WHEN p_new OR p_rp.
      PERFORM change_screen USING 'NEW' "GROUP ID
                                 ''
                                 "'*S_MNEDAT*'
                                 'BODMNLSCR' ." SELOPTION ID .

      LOOP AT SCREEN .
        IF screen-group1 EQ 'NEW'.
          screen-input = '1'.
          IF screen-name = '%_S_DATE_%_APP_%-OPTI_PUSH' OR
              screen-name =  '%_S_TIME_%_APP_%-OPTI_PUSH'.
            screen-input = '0'.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN p_sc  .
      PERFORM change_screen USING 'SCR' "GROUP ID
                                  ''
                                  'NEWBODMNL' ." SELOPTION ID .


      LOOP AT SCREEN.
        IF screen-group1 = 'SCR'.
          screen-input = '1'.
          IF screen-name = '%_S_ERDAT_%_APP_%-OPTI_PUSH' OR
          screen-name = '%_S_ERZET_%_APP_%-OPTI_PUSH' OR
          screen-name = '%_S_AEDAT_%_APP_%-OPTI_PUSH' OR
          screen-name = '%_S_AEZET_%_APP_%-OPTI_PUSH'.
            screen-input = '0'.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN p_mn  .
      PERFORM change_screen USING 'MNL' "GROUP ID
                                  '%_S_MNEDAT_%_APP_%-OPTI_PUSH'
                                  "'*S_MNEDAT*'
                                  'NEWBODSCR' ." SELOPTION ID .
      LOOP AT SCREEN.
        IF screen-group1 = 'MNL'.
          screen-input = '1'.

          IF screen-name CP '%_S_MNCDAT_%_APP_%-OPTI_PUSH' OR
             screen-name CP '%_S_MNEDAT_%_APP_%-OPTI_PUSH' .
            screen-input = '0'.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN p_bd  .
      PERFORM change_screen USING 'BOD' "GROUP ID
                                  '%_S_BODYNO_%_APP_%-OPTI_PUSH'
                                  'NEWSCRMNL' ." SELOPTION ID .

  ENDCASE.
*
*  IF  P_SC = 'X'.
*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 = 'SCR'.
*        SCREEN-INPUT = '1'.
*        IF SCREEN-NAME = '%_S_ERDAT_%_APP_%-OPTI_PUSH' OR
*        SCREEN-NAME = '%_S_ERZET_%_APP_%-OPTI_PUSH' OR
*        SCREEN-NAME = '%_S_AEDAT_%_APP_%-OPTI_PUSH' OR
*        SCREEN-NAME = '%_S_AEZET_%_APP_%-OPTI_PUSH' OR
*        SCREEN-NAME CP '*S_BODYNO*'.
*          SCREEN-INPUT = '0'.
*        ENDIF.
*        MODIFY SCREEN.
*      ELSE.
*        IF SCREEN-GROUP1 = 'NEW' OR
*           SCREEN-GROUP1 = 'MNL' OR
*           SCREEN-GROUP1 = 'BOD'.
*          SCREEN-INPUT = '0'.
*          SCREEN-INVISIBLE = '1'.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ELSE.
*    IF P_NEW = 'X' OR P_RP = 'X' OR P_RP EQ 'X'.
*      LOOP AT SCREEN.
*        IF SCREEN-GROUP1 = 'NEW'.
*          SCREEN-INPUT = '1'.
*          IF SCREEN-NAME = '%_S_DATE_%_APP_%-OPTI_PUSH' OR
*         SCREEN-NAME = '%_S_TIME_%_APP_%-OPTI_PUSH'.
*            SCREEN-INPUT = '0'.
*          ENDIF.
*          MODIFY SCREEN.
*        ELSE.
*          IF SCREEN-GROUP1 = 'SCR' OR
*          SCREEN-GROUP1 = 'MNL'.
*            SCREEN-INPUT = '0'.
*            SCREEN-INVISIBLE = '1'.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ELSE.
*      LOOP AT SCREEN.
*        IF SCREEN-GROUP1 = 'MNL'.
*          SCREEN-INPUT = '1'.
*          IF SCREEN-NAME = '%_S_MNEDAT_%_APP_%-OPTI_PUSH'.
*            SCREEN-INPUT = '0'.
*          ENDIF.
*          MODIFY SCREEN.
*        ELSE.
*          IF SCREEN-GROUP1 = 'SCR' OR
*          SCREEN-GROUP1 = 'NEW' OR
*          SCREEN-GROUP1 = 'BOD'.
*            SCREEN-INPUT = '0'.
*            SCREEN-INVISIBLE = '1'.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
*    ENDIF.
*  ENDIF.

*  LOOP AT SCREEN.
*    IF P_NEW = 'X'.
*      IF SCREEN-GROUP1 = 'NEW'.
*        SCREEN-ACTIVE = '0'.
*      ELSE.
*        SCREEN-ACTIVE = '1'.
*      ENDIF.
*      MODIFY SCREEN.
*    ELSE.
*      IF SCREEN-GROUP1 = 'NEW'.
*        SCREEN-ACTIVE = '1'.
*      ELSE.
*        SCREEN-ACTIVE = '0'.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_screen USING p_grpnm LIKE screen-group1
                         p_selnm LIKE screen-name
                         p_viwnm.
  LOOP AT SCREEN.
    IF screen-group1 = p_grpnm.
      screen-input = '1'.
      IF screen-name CP p_selnm.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ELSE.
      IF screen-group1 EQ p_viwnm+0(3) OR
         screen-group1 EQ p_viwnm+3(3) OR
         screen-group1 EQ p_viwnm+6(3) .
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHANGE_SCREEN

*---------------------------------------------------------------------*
*       FORM CONVERSION_OUTPUT                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM conversion_output USING p_value .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_value.
ENDFORM.                    "CONVERSION_OUTPUT
