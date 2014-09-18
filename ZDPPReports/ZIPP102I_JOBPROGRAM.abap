************************************************************************
* Program Name      : ZIPP102I_JOBPROGRAM
* Author            : Bobby
* Creation Date     : 2003.09.04.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No :
* Addl Documentation:
* Description       : Vehicle Order(Planned Order) Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  ZIPP102I_JOBPROGRAM  MESSAGE-ID zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_pp_log_head,       " Table of the Interface Log(Header)
        ztpp_pp_log_deta,       " Table of the Interface Log(Detail)
        equi ,
        ausp .

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: wa_material             LIKE mara-matnr                 ,
      wa_7jb                  LIKE ztpp_pmt07jb_b             ,
      wa_vin                  LIKE MARA-MATNR                 ,
      wa_plnum                LIKE plaf-plnum                 ,
*     wa_equnr                LIKE equi-equnr                 ,
      wa_number               LIKE ztpp_pp_log_head-logkey,
      l_evcode(5)             TYPE c                          ,
      wa_lines                TYPE i                          ,
      wa_msg(70)              TYPE c                          ,
      wa_mng                  TYPE i                          ,
      wa_tabix                LIKE sy-tabix                   ,
      wa_seq                  LIKE ztpp_pp_log_deta-sequence,
      wa_flag                 TYPE c                          ,
      wa_error                TYPE c                          ,
      wa_mode                 TYPE c   VALUE   'N'            .

*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <field1>       TYPE ANY                        .

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
DATA: it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

DATA: BEGIN OF WA_vehicle    OCCURS 0       .
        INCLUDE STRUCTURE    ztpp_pmt07jb_b .
DATA:   workorder            LIKE mara-matnr,
*       instance             LIKE mara-cuobf,
        sorder(10)           TYPE c         ,
        porder               like plaf-plnum,
        equnr                LIKE equi-equnr,
        matnr                LIKE mara-matnr,
        b_serial(6)          TYPE n         ,
        vin                  LIKE mara-matnr,
        e_flag               TYPE c         ,
      END OF WA_vehicle                     .

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: P_7JB            LIKE ZTPP_PMT07JB_B,
*           p_vin            LIKE MARA-MATNR    ,
*           P_EQUNR          LIKE EQUI-EQUNR    ,
            p_log            like ztpp_pp_log_head-logkey.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM SET_VARIABLES    .
  PERFORM record_processing.

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_processing.
  DATA: l_date(10)           TYPE c          ,
        l_salesorder         like vbak-vbeln ,
        l_fsc                like mara-matnr ,
        l_vin                LIKE mara-matnr .

  PERFORM VIN_CODE_GEN      .
  move-corresponding wa_7jb  to     WA_vehicle .
  wa_vehicle-matnr = wa_material .

  SELECT SINGLE fsc sales INTO (l_fsc, l_salesorder)
    FROM ztpp_wosum
   WHERE wo_ser = wa_7jb-ordr
     AND nation = wa_7jb-dist(3)
     AND dealer = wa_7jb-dist+3(2)
     AND extc   = wa_7jb-extc
     AND intc   = wa_7jb-intc     .

  " Processing of the Transaction MD11
  WRITE wa_7jb-sqdt TO l_date .
  wa_vehicle-sorder = l_salesorder .

  CONCATENATE wa_7jb-moye wa_7jb-dist  wa_7jb-bmdl   INTO wa_material.
  CONCATENATE wa_material wa_7jb-ocnn INTO wa_material SEPARATED BY ' '.

  PERFORM call_bdc_planned_order USING  wa_material  wa_7jb-pver
                                        l_date       l_salesorder.

  " Processing the VINGeneration & Vehicla Master Creation
  PERFORM vin_vm_creation USING wa_plnum l_salesorder  l_fsc .

  PERFORM class_assign .

  PERFORM generate_characterisitc_vm .
*    PERFORM create_log USING 'S' 5 text-011 wa_7jb  .
ENDFORM.                    " RECORD_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_PLANNED_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_planned_order  USING pa_matnr pa_verid pa_date pa_sorder.
  DATA: l_header              LIKE bapiplaf_i1 ,
        l_return              LIKE  bapireturn1 .

  " Create the Planned Order

  CLEAR: wa_plnum.

  l_header-material         = pa_matnr   .
  l_header-plan_plant       = 'P001'     .
  l_header-prod_plant       = 'P001'     .
  l_header-total_plord_qty  = 1          .
  l_header-order_start_date = wa_7jb-sqdt.
  l_header-order_fin_date   = wa_7jb-sqdt.
  l_header-plan_open_date   = wa_7jb-sqdt.
  l_header-firming_ind      = 'X'        .
  l_header-sales_ord        = pa_sorder  .
  l_header-s_ord_item       = 10         .
  l_header-version          = pa_verid   .
  l_header-mrp_area         = 'P001'     .
  l_header-pldord_profile   = 'KD'       .
  l_header-acctasscat       = 'M'        .

  CALL FUNCTION 'BAPI_PLANNEDORDER_CREATE'
       EXPORTING
            headerdata   = l_header
       IMPORTING
            return       = l_return
            plannedorder = wa_plnum.

  wa_vehicle-porder = wa_plnum .
ENDFORM.                    " CALL_BDC_PLANNED_ORDER

*&---------------------------------------------------------------------*
*&      Form  VIN_VM_CREATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0588   text
*      -->P_0589   text
*----------------------------------------------------------------------*
FORM vin_vm_creation USING    pa_plnum   pa_sorder  pa_fsc.
  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_conf            LIKE TABLE OF conf_out       WITH HEADER LINE,
        l_name(30)        TYPE c           ,
        l_no(03)          TYPE n           ,
        l_seq(6)          TYPE n           ,
        l_workorder       LIKE mara-matnr  ,
        l_eqfnr           LIKE itob-eqfnr  ,
        l_equnr           LIKE equi-equnr  ,
        l_rsnum           like resb-rsnum  ,
        l_general         LIKE bapi_itob ,
        l_specific        LIKE bapi_itob_eq_only,
        l_return          LIKE bapiret2.

  wa_vehicle-b_serial = l_seq  = WA_VEHICLE-vin+11(6) .
* CONCATENATE 'EMF'           l_seq INTO wa_equnr .
* CONCATENATE wa_7jb-bmdl(3)  l_seq INTO l_equnr .

  SELECT SINGLE *
    FROM equi
   WHERE equnr = WA_VEHICLE-EQUNR.

  IF sy-subrc = 0.
    " Create Error Log for the COMMON_VALS incorrect!!!
    wa_error = 'X' .
    EXIT.
  ENDIF.

  " Check the result-code of the BDC .
  " if it is successful, Saveing the Planned Order Number to the table
  " or not, Logging the Result into the table
  UPDATE ztpp_pmt07jb_a   SET: plnum = wa_plnum
                               vinn  = WA_VEHICLE-vin
                        WHERE sqdt  = wa_7jb-sqdt
                          AND modl  = wa_7jb-modl
                          AND mtgu  = wa_7jb-mtgu
                          AND ssr1  = wa_7jb-ssr1 .

  " Create the Vehicle Master
  CONCATENATE  wa_7jb-ordr  wa_7jb-dist  wa_7jb-EXTC  wa_7jb-INTC
         INTO l_workorder.
  CONCATENATE pa_FSC WA_7JB-VERS+1(2) L_WORKORDER
         INTO l_general-descript SEPARATED BY SPACE     .

  PERFORM get_workcenter      USING l_general-pp_wkctr  .
*  PERFORM get_eqfnr           USING l_eqfnr             .
  perform get_rsnum           using pa_plnum  l_rsnum   .

  l_general-objecttype       = '1000' .
  l_general-manfacture       = 'HMMA' .
  l_general-mancountry       = 'US'   .
  l_general-countr_iso       = 'US'   .
  l_general-manserno         =  l_seq .
  l_general-manmodel         =  wa_7jb-bmdl(3)          .
  l_general-constyear        =  sy-datum(4)             .
  l_general-constmonth       =  sy-datum+4(2)           .
  l_general-start_from       =  sy-datum                .
  l_general-planplant        = 'P001'                   .
  l_general-manparno         =  WA_VEHICLE-vin                  .
  l_general-descript         =  WA_VEHICLE-EQUNR         .
  l_general-sortfield        =  l_rsnum                 .
  l_general-maintplant       = 'P001'                   .
*  l_general-pp_wkctr         =  l_workcenter            .
  l_general-read_crdat       = sy-datum.
  l_general-read_crnam       = sy-uname.

  l_specific-equicatgry      = 'V' .

  CALL FUNCTION 'BAPI_EQUI_CREATE'
       EXPORTING
            external_number = WA_VEHICLE-EQUNR
            data_general    = l_general
            data_specific   = l_specific
            valid_date      = sy-datum
       IMPORTING
            return          = l_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
  COMMIT WORK AND WAIT .
  WAIT UP TO 10 SECONDS.
*  pa_equnr = l_equnr .
ENDFORM.                    " VIN_VM_CREATION

*&---------------------------------------------------------------------*
*&      Form  GET_EQFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_EQFNR  text
*----------------------------------------------------------------------*
FORM get_eqfnr USING    pa_eqfnr.
  pa_eqfnr = 'A' .
ENDFORM.                    " GET_EQFNR

*&---------------------------------------------------------------------*
*&      Form  GET_WORKCENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORKCENTER  text
*----------------------------------------------------------------------*
FORM get_workcenter USING    pa_workcenter.
  SELECT SINGLE objid
               INTO pa_workcenter
               FROM crhd
               WHERE arbpl EQ 'T'.
ENDFORM.                    " GET_WORKCENTER

*&---------------------------------------------------------------------*
*&      Form  GENERATE_CHARACTERISITC_VM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_characterisitc_vm.
  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_conf            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_data            LIKE TABLE OF conf_out       WITH HEADER LINE,
        l_name(30)        TYPE c           ,
        l_no(03)          TYPE n           ,
        l_seq(6)          TYPE n           ,
        l_instance        LIKE inob-cuobj  ,
        l_workcenter      LIKE crhd-arbpl  ,
        l_workorder       LIKE mara-matnr  ,
        l_eqfnr           LIKE itob-eqfnr  ,
        l_mode            LIKE ztpp_common_vals-key2,
        l_equnr           LIKE equi-equnr  .

  COMMIT WORK .
  CHECK wa_flag NE 'E' .
  IF WA_vehicle-mtgu = 'M' .
    l_vartable-atnam = 'P_MITU' .
    l_vartable-atwrt = 'Y'              .       APPEND l_vartable.
    l_vartable-atnam = 'P_MITU_DATE'.
    l_vartable-atwrt =  wa_vehicle-sqdt .       APPEND l_vartable.
  ENDIF.
  l_vartable-atnam = 'P_OCN'.
  l_vartable-atwrt =  wa_vehicle-ocnn .         APPEND l_vartable.
  l_vartable-atnam = 'P_VERSION'.
  l_vartable-atwrt =  wa_vehicle-vers .         APPEND l_vartable.
  l_vartable-atnam = 'P_DESTINATION_CODE'.
  l_vartable-atwrt =  wa_vehicle-dist .         APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_DATE'.
  l_vartable-atwrt =  wa_vehicle-sqdt .         APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_SERIAL'.
  l_vartable-atwrt =  wa_vehicle-ssr1 .         APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_CODE'.
  l_vartable-atwrt =  wa_vehicle-sqcd .         APPEND l_vartable.
  CONCATENATE wa_vehicle-evl1  wa_vehicle-evl2  wa_vehicle-evl3
              wa_vehicle-evl4  wa_vehicle-evl5  INTO l_evcode    .
  l_vartable-atnam = 'P_EPI_CODE'     .
  l_vartable-atwrt =  l_evcode        .         APPEND l_vartable.
  l_vartable-atnam = 'P_SALES_ORDER'   .
  l_vartable-atwrt =  wa_vehicle-sorder.        APPEND l_vartable.
  l_vartable-atnam = 'P_PLAN_ORDER'   .
  l_vartable-atwrt =  wa_vehicle-porder.        APPEND l_vartable.
  l_vartable-atnam = 'P_STATUS'       .
  l_vartable-atwrt = 'B00'             .        APPEND l_vartable.
  l_vartable-atnam = 'P_RP_STATUS'    .
  l_vartable-atwrt = '00'              .        APPEND l_vartable.
  l_vartable-atnam = 'P_VM_DATE'      .
  CONCATENATE SY-DATUM SY-UZEIT           INTO  l_vartable-atwrt .
                                                APPEND l_vartable.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object             = WA_vehicle-matnr     " wa_material
      ctype              = '001'
    TABLES
      val_table          = l_conf .

  CLEAR: l_conf-atwrt .
  READ TABLE l_conf WITH KEY atnam = 'P_LC_NO'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_LC_NO'         .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MODEL'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MODEL'         .        APPEND l_vartable.
  l_vartable-atwrt = WA_vehicle-b_serial.       CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_BODY_SERIAL'   .        APPEND l_vartable.
  CONCATENATE WA_vehicle-ordr  WA_vehicle-dist  INTO   l_vartable-atwrt.
  CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_WORK_ORDER'    .        APPEND l_vartable.
  l_vartable-atwrt = WA_vehicle-extc  .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_EXT_COLOR'     .        APPEND l_vartable.
  l_vartable-atwrt = WA_vehicle-intc  .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_INT_COLOR'     .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MODEL_YEAR'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MODEL_YEAR'    .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MI'   .
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MI'            .        APPEND l_vartable.
  l_vartable-atwrt = WA_VEHICLE-vin+11(6).      CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_VIN'           .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_FLEET'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_FLEET'         .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_REGION_PORT'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_REGION_PORT'  .         APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_ORDER_ZONE'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_ORDER_ZONE'   .         APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_COLOR_SER'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_COLOR_SER'    .         APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MANUAL_ORDER'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MANUAL_ORDER' .         APPEND l_vartable.

  DO  9 TIMES.
    l_no = l_no + 1.
    CONCATENATE 'P_219_' l_no+2(1)   INTO l_name .  CLEAR: l_conf-atwrt.
    READ TABLE l_conf    WITH KEY    atnam = l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_conf-atwrt .
    APPEND l_vartable        .
  ENDDO.

  DO 90 TIMES.
    l_no = l_no + 1.
    CONCATENATE 'P_219_' l_no+1(2)   INTO l_name .  CLEAR: l_conf-atwrt.
    READ TABLE l_conf    WITH KEY    atnam = l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_conf-atwrt .
    APPEND l_vartable        .
  ENDDO.

  DO 120 TIMES.
    l_no = l_no + 1.
    CONCATENATE 'P_219_' l_no        INTO l_name .  CLEAR: l_conf-atwrt.
    READ TABLE l_conf    WITH KEY    atnam = l_name.
    l_vartable-atnam = l_name.       l_vartable-atwrt = l_conf-atwrt .
    APPEND l_vartable        .
  ENDDO.

  l_equnr = WA_vehicle-equnr.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object     = l_equnr
            mode       = 'W'
       TABLES
            val_table  = l_vartable
       EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.

  IF sy-subrc <> 0.
    WRITE AT: /001(50) 'Error of the VIN Master Characteristic Generat'.
  ENDIF.
ENDFORM.                    " GENERATE_CHARACTERISITC_VM

*&---------------------------------------------------------------------*
*&      Form  CLASS_ASSIGN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLASS_ASSIGN.
  DATA: l_equi            LIKE  v_equi  .

  l_equi-equnr = WA_VEHICLE-EQUNR.
  COMMIT WORK.

  CALL FUNCTION 'EQUIPMENT_CLASS_ALLOCATE'
       EXPORTING
            eq_class      = 'P_VEHICLE_MASTER'
            eq_class_type = '002'
            is_standard   = 'X'
            init_new      = 'X'
            lock_new      = 'X'
            update_new    = 'X'
            commit_new    = 'X'
       CHANGING
            s_equi        = l_equi.
ENDFORM.                    " CLASS_ASSIGN

*&---------------------------------------------------------------------*
*&      Form  get_rsnum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_PLNUM  text
*      -->P_L_RSNUM  text
*----------------------------------------------------------------------*
FORM get_rsnum USING    pa_plnum  pa_rsnum.
  select single rsnum into pa_rsnum
    from plaf
   where plnum = pa_plnum.
ENDFORM.                    " get_rsnum


*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
FORM create_log USING    pa_type  pa_step  pa_text  pa_key .
  wa_seq = wa_seq + 1 .

  " Log Detail Creation
  ztpp_pp_log_deta-logkey   = wa_number    .
  ztpp_pp_log_deta-sequence = wa_seq      .
  ztpp_pp_log_deta-logtype  = pa_type     .
  ztpp_pp_log_deta-keydata  = pa_key      .
  INSERT INTO ztpp_pp_log_deta VALUES ztpp_pp_log_deta .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  VIN_CODE_GEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VIN_CODE_GEN.
 DATA: L_VIN                LIKE MARA-MATNR ,
       L_MODE               LIKE ZTPP_COMMON_VALS-KEY2.

  " Call the function for the Last Data...
  l_mode = 'EMF'.      " wa_7jb-modl .
  L_VIN  = wa_VIN.

  CALL FUNCTION 'Z_FPP_VIN_GENERATION'
       EXPORTING
            w_order  = L_vin
            mode     = l_mode
       IMPORTING
            p_lastid = L_VIN .

  WA_VEHICLE-VIN = L_VIN     .
  CONCATENATE L_MODE L_VIN+11(7) INTO WA_VEHICLE-EQUNR.
ENDFORM.                    " VIN_CODE_GEN

*&---------------------------------------------------------------------*
*&      Form  SET_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_VARIABLES.
  data: l_atinn             like cabn-atinn,
        l_material          like mara-matnr.

  WA_7JB = P_7JB .
  CONCATENATE wa_7jb-ordr wa_7jb-dist INTO wa_material .

  " Finding a VIN Value..
  CONCATENATE wa_7jb-ordr      wa_7jb-dist      INTO l_material .
  select single atinn into l_atinn
    from cabn
   where atnam = 'P_VIN_SPEC' .

  select single atwrt into wa_vin
    from ausp
   where objek = l_material
     and atinn = l_atinn
     and klart = '001'     .

ENDFORM.                    " SET_VARIABLES
