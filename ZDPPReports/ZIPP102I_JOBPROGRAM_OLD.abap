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

TABLES: ztpp_pp_log_head,       " Table of the Interface Log(Header)
        ztpp_pp_log_deta,       " Table of the Interface Log(Detail)
        equi ,
        ausp .

DATA: wa_material             LIKE mara-matnr                 ,
      wa_instance             LIKE mara-cuobf                 ,
      wa_plnum                LIKE plaf-plnum                 ,
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

FIELD-SYMBOLS: <field1>       TYPE ANY                        .

DATA: BEGIN OF it_vin         OCCURS 0                        .
        INCLUDE STRUCTURE     ztpp_pmt07jb_b .
DATA:   vin(17)               TYPE c         ,
      END OF it_vin                          ,
      it_7jb              LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE,
      it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

DATA: BEGIN OF it_vehicle    OCCURS 0       .
        INCLUDE STRUCTURE    ztpp_pmt07jb_b .
DATA:   workorder            LIKE mara-matnr,
        instance             LIKE mara-cuobf,
        sorder(10)           TYPE c         ,
        porder               like plaf-plnum,
        equnr                LIKE equi-equnr,
        matnr                LIKE mara-matnr,
        b_serial(6)          TYPE n         ,
        vin                  LIKE mara-matnr,
        e_flag               TYPE c         ,
      END OF it_vehicle                     .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: P_7JB            LIKE ZTPP_PMT07JB_B,
            p_log            like ztpp_pp_log_head-logkey.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
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
  data: l_cont               TYPE i        ,
        l_max                TYPE i        VALUE 1,
        condition(6)                       .

  perform process_parallel   .

  IF wa_tabix = l_cont.
    PERFORM create_log USING 'S' 5 text-011 it_7jb  .
  ELSE.
    PERFORM create_log USING 'E' 5 text-004 it_7jb  .
    EXIT.
  ENDIF.

  get time.
  WRITE AT: /001(40) 'Class Assign & Charateristics Assign...',
             041(10)  sy-datum ,
             051(02) '**'      ,
             053(10)  sy-uzeit .

  LOOP AT IT_VEHICLE.
    PERFORM CLASS_ASSIGN .
  ENDLOOP.

  LOOP AT it_vehicle.
    PERFORM generate_characterisitc_vm .
  ENDLOOP.
ENDFORM.                    " RECORD_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  process_mitu_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_mitu_value.
  DATA: l_equnr              LIKE equi-equnr ,
        l_atinn              LIKE ausp-atinn ,
        l_porder             LIKE plaf-plnum ,
        l_vin                LIKE mara-matnr ,
        l_mituqty            LIKE ztpp_wosum-mituqty.

  CLEAR: wa_material.
  CONCATENATE it_7jb-ordr it_7jb-dist INTO wa_material .

  " Work Order Summary Table Update.....
  SELECT SINGLE mituqty INTO l_mituqty
    FROM ztpp_wosum
   WHERE wo_ser = it_7jb-ordr
     AND nation = it_7jb-dist(3)
     AND dealer = it_7jb-dist+3(2)
     AND extc   = it_7jb-extc
     AND intc   = it_7jb-intc     .

  l_mituqty = l_mituqty - it_7jb-pqty  .
  UPDATE ztpp_wosum   SET mituqty = l_mituqty
                    WHERE wo_ser = it_7jb-ordr
                      AND nation = it_7jb-dist(3)
                      AND dealer = it_7jb-dist+3(2)
                      AND extc   = it_7jb-extc
                      AND intc   = it_7jb-intc     .

  " Vehicla Master Update..
  CONCATENATE  it_7jb-bmdl(3)  it_7jb-vhno  INTO  l_equnr .
  CLEAR: it_vmaster, it_vmaster[] .
  it_vmaster-atnam = 'P_SEQUENCE_DATE' .
  it_vmaster-atwrt = it_7jb-sqdt       .
  APPEND it_vmaster.
  it_vmaster-atnam = 'P_MITU'          .
  it_vmaster-atwrt = ' '               .
  APPEND it_vmaster.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object     = l_equnr
            mode       = 'W'
       TABLES
            val_table  = it_vmaster
       EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.

  " Plan Order Number Search and Saving...
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_PLAN_ORDER'
       IMPORTING
            output = l_atinn.
  .
  SELECT SINGLE atwrt INTO l_porder
    FROM ausp
   WHERE objek  = l_equnr
     AND atinn  = l_atinn .

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_VIN'
       IMPORTING
            output = l_atinn.
  .
  SELECT SINGLE atwrt INTO l_vin
    FROM ausp
   WHERE objek  = l_equnr
     AND atinn  = l_atinn .

  IF sy-subrc = 0.
    UPDATE ztpp_pmt07jb_a    SET: plnum = l_porder
                                  vinn  = l_vin
                                  aedat = sy-datum
                                  aezet = sy-uzeit
                                  aenam = sy-uname
                           WHERE sqdt  = it_7jb-sqdt
                             AND plnt  = it_7jb-plnt
                             AND line  = it_7jb-line
                             AND modl  = it_7jb-modl
                             AND mtgu  = it_7jb-mtgu
                             AND ssr1  = it_7jb-ssr1
                             AND ssr2  = it_7jb-ssr2  .
  ENDIF.
ENDFORM.                    " process_mitu_value

*&---------------------------------------------------------------------*
*&      Form  process_mitu_space
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_mitu_space.
  DATA: l_date(10)           TYPE c          ,
        l_salesorder(10)     TYPE c          ,
        l_vin                LIKE mara-matnr .

  CLEAR: wa_material.
  CONCATENATE it_7jb-ordr it_7jb-dist INTO wa_material .

  SELECT SINGLE sales INTO l_salesorder
    FROM ztpp_wosum
   WHERE wo_ser = it_7jb-ordr
     AND nation = it_7jb-dist(3)
     AND dealer = it_7jb-dist+3(2)
     AND extc   = it_7jb-extc
     AND intc   = it_7jb-intc     .

  READ TABLE it_vin WITH KEY ordr = it_7jb-ordr
                             dist = it_7jb-dist .
  IF sy-subrc = 0.
    l_vin = it_vin-vin .
  ENDIF.

  " Processing of the Transaction MD11
  WRITE it_7jb-sqdt TO l_date .
  CONCATENATE it_7jb-moye it_7jb-dist  it_7jb-bmdl   INTO wa_material.
  CONCATENATE wa_material it_7jb-ocnn INTO wa_material SEPARATED BY ' '.

  PERFORM call_bdc_planned_order USING  wa_material  it_7jb-pver
                                        l_date       l_salesorder.

  " Processing the VINGeneration & Vehicla Master Creation
  PERFORM vin_vm_creation USING wa_plnum l_salesorder l_vin           .
ENDFORM.                    " process_mitu_space

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
  l_header-order_start_date = it_7jb-sqdt.
  l_header-order_fin_date   = it_7jb-sqdt.
  l_header-plan_open_date   = it_7jb-sqdt.
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

  WRITE: /, 'Return Message for the Creation Plan Order: ',
             pa_matnr, wa_plnum, l_return-MESSAGE.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
ENDFORM.                    " CALL_BDC_PLANNED_ORDER

*&---------------------------------------------------------------------*
*&      Form  VIN_VM_CREATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0588   text
*      -->P_0589   text
*----------------------------------------------------------------------*
FORM vin_vm_creation USING    pa_plnum   pa_sorder   pa_vin.
  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_conf            LIKE TABLE OF conf_out       WITH HEADER LINE,
        l_name(30)        TYPE c           ,
        l_no(03)          TYPE n           ,
        l_seq(6)          TYPE n           ,
        l_instance        LIKE inob-cuobj  ,
        l_workcenter      LIKE crhd-objid  ,
        l_workorder       LIKE mara-matnr  ,
        l_eqfnr           LIKE itob-eqfnr  ,
        l_mode            LIKE ztpp_common_vals-key2,
        l_equnr           LIKE equi-equnr  ,
        l_rsnum           like resb-rsnum  ,
        l_general         LIKE bapi_itob ,
        l_specific        LIKE bapi_itob_eq_only,
        l_return          LIKE bapiret2.

  " Call the function for the Last Data...
  l_mode = 'EMF'.      " it_7jb-modl .

  WRITE AT: /001(40) 'Create the Vehicle Master with..' ,
             041(15) 'Sales Order is ',
             056(15)  pa_sorder       ,
             060(15) 'Plan Order is  ',
             075(10)  pa_plnum        ,
             086(17) 'VIN Spec COde is',
             105(20)  pa_vin.

  CALL FUNCTION 'Z_FPP_VIN_GENERATION'
       EXPORTING
            w_order  = pa_vin
            mode     = l_mode
       IMPORTING
            p_lastid = pa_vin.

  l_seq  = pa_vin+11(6) .
  CONCATENATE 'EMF'           l_seq INTO l_equnr .
* CONCATENATE it_7jb-bmdl(3)  l_seq INTO l_equnr .

  SELECT SINGLE *
    FROM equi
   WHERE equnr = l_equnr.

  IF sy-subrc = 0.
    WRITE AT: /001(35) 'Already created Vehicle Master..' ,
               036(20)  l_equnr .
    wa_error = 'X' .
    EXIT.
  ENDIF.
  " Check the result-code of the BDC .
  " if it is successful, Saveing the Planned Order Number to the table
  " or not, Logging the Result into the table
  UPDATE ztpp_pmt07jb_a   SET: plnum = wa_plnum
                               vinn  = pa_vin
                        WHERE sqdt  = it_7jb-sqdt
                          AND modl  = it_7jb-modl
                          AND mtgu  = it_7jb-mtgu
                          AND ssr1  = it_7jb-ssr1 .

  WRITE AT: /001(40) 'Update ZTPP_PMT07JB_A Table ....' ,
             041(20)  sy-subrc.

  " Create the Vehicle Master
  CONCATENATE it_7jb-ordr  it_7jb-dist               INTO l_workorder.

  PERFORM get_workcenter      USING l_workcenter        .
  PERFORM get_eqfnr           USING l_eqfnr             .
  perform get_rsnum           using pa_plnum  l_rsnum   .

  WRITE AT: /001(40) 'Create the Vehicle Master.......' ,
             041(20)  l_equnr .

  l_general-objecttype       = '1000' .
  l_general-manfacture       = 'HMMA' .
  l_general-mancountry       = 'US'   .
  l_general-countr_iso       = 'US'   .
  l_general-manserno         =  l_seq .
  l_general-manmodel         =  it_7jb-bmdl(3)          .
  l_general-constyear        =  sy-datum(4)             .
  l_general-constmonth       =  sy-datum+4(2)           .
  l_general-start_from       =  sy-datum                .
  l_general-planplant        = 'P001'                   .
  l_general-manparno         =  pa_vin                  .
  l_general-descript         =  l_equnr                 .
  l_general-sortfield        =  l_rsnum                 .
  l_general-maintplant       = 'P001'                   .
  l_general-pp_wkctr         =  l_workcenter            .
  l_general-read_crdat       = sy-datum.
  l_general-read_crnam       = sy-uname.

  l_specific-equicatgry      = 'V' .

  CALL FUNCTION 'BAPI_EQUI_CREATE'
       EXPORTING
            external_number = l_equnr
            data_general    = l_general
            data_specific   = l_specific
            valid_date      = sy-datum
       IMPORTING
            return          = l_return.

  WRITE: /, 'Return Message for the Creation Equipment Heaa: ',
             l_equnr, l_return-MESSAGE.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
  WAIT UP TO 1 SECONDS.

* l_equi-equnr = l_equnr.

* CALL FUNCTION 'EQUIPMENT_CLASS_ALLOCATE'
*      EXPORTING
*           eq_class      = 'P_VEHICLE_MASTER'
*           eq_class_type = '002'
*           is_standard   = 'X'
*           init_new      = 'X'
*           lock_new      = 'X'
*           update_new    = 'X'
*           commit_new    = 'X'
*      CHANGING
*           s_equi        = l_equi.

  CLEAR: it_vehicle .

  SELECT SINGLE cuobf INTO l_instance
    FROM mara
   WHERE matnr = l_workorder.

  MOVE-CORRESPONDING      it_7jb                 TO     it_vehicle.
  it_vehicle-e_flag     = wa_flag   .
  CONCATENATE it_7jb-ordr it_7jb-dist INTO it_vehicle-matnr .
  it_vehicle-instance   = l_instance.
  it_vehicle-workorder  = l_instance.
  it_vehicle-sorder     = pa_sorder .
  it_vehicle-porder     = pa_plnum  .
  it_vehicle-b_serial   = l_seq     .
  it_vehicle-vin        = pa_vin    .
  it_vehicle-equnr      = l_equnr   .            APPEND it_vehicle.
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
  pa_workcenter = '10000064' .
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
  IF it_vehicle-mtgu = 'M' .
    l_vartable-atnam = 'P_MITU' .
    l_vartable-atwrt = 'Y'              .       APPEND l_vartable.
    l_vartable-atnam = 'P_MITU_DATE'.
    l_vartable-atwrt =  it_vehicle-sqdt .       APPEND l_vartable.
  ENDIF.
  l_vartable-atnam = 'P_OCN'.
  l_vartable-atwrt =  it_vehicle-ocnn .         APPEND l_vartable.
  l_vartable-atnam = 'P_VERSION'.
  l_vartable-atwrt =  it_vehicle-vers .         APPEND l_vartable.
  l_vartable-atnam = 'P_DESTINATION_CODE'.
  l_vartable-atwrt =  it_vehicle-dist .         APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_DATE'.
  l_vartable-atwrt =  it_vehicle-sqdt .         APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_SERIAL'.
  l_vartable-atwrt =  it_vehicle-ssr1 .         APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_CODE'.
  l_vartable-atwrt =  it_vehicle-sqcd .         APPEND l_vartable.
  CONCATENATE it_vehicle-evl1  it_vehicle-evl2  it_vehicle-evl3
              it_vehicle-evl4  it_vehicle-evl5  INTO l_evcode    .
  l_vartable-atnam = 'P_EPI_CODE'     .
  l_vartable-atwrt =  l_evcode        .         APPEND l_vartable.
  l_vartable-atnam = 'P_SALES_ORDER'   .
  l_vartable-atwrt =  it_vehicle-sorder.        APPEND l_vartable.
  l_vartable-atnam = 'P_PLAN_ORDER'   .
  l_vartable-atwrt =  it_vehicle-porder.        APPEND l_vartable.
  l_vartable-atnam = 'P_STATUS'       .
  l_vartable-atwrt = '00'              .        APPEND l_vartable.

  l_instance = it_vehicle-instance .
*
*  CALL FUNCTION 'VC_I_GET_CONFIGURATION_IBASE'
*    EXPORTING
*      instance                  = l_instance
*    TABLES
*      configuration             = l_conf
**     ET_CONF_WITH_AUTHOR       =
*    EXCEPTIONS
*      instance_not_found        = 1
*      OTHERS                    = 2 .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object             = it_vehicle-matnr     " wa_material
*     MODE               = 'R'
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
  l_vartable-atwrt = it_vehicle-b_serial.       CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_BODY_SERIAL'   .        APPEND l_vartable.
  CONCATENATE it_vehicle-ordr  it_vehicle-dist  INTO   l_vartable-atwrt.
  CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_WORK_ORDER'    .        APPEND l_vartable.
  l_vartable-atwrt = it_vehicle-extc  .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_EXT_COLOR'     .        APPEND l_vartable.
  l_vartable-atwrt = it_vehicle-intc  .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_INT_COLOR'     .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MODEL_YEAR'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MODEL_YEAR'    .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MI'   .
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MI'            .        APPEND l_vartable.
  l_vartable-atwrt = it_vehicle-vin    .        CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_VIN'           .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_LC_NO'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_LC_NO'         .        APPEND l_vartable.

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

  l_equnr = it_vehicle-equnr.

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

  l_equi-equnr = IT_VEHICLE-EQUNR.    " l_equnr.

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
*&      Form  process_parallel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_parallel.
  PERFORM process_mitu_space .
  PERFORM CLASS_ASSIGN .
  PERFORM generate_characterisitc_vm .
ENDFORM.                    " process_parallel


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
