************************************************************************
* Program Name      : YIPP102I_002_TEMPORARY2
* Author            : Bobby
* Creation Date     : 2003.09.04.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No :
* Addl Documentation:
* Description       : Create Plan Order using VINN
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  yipp102i_002_temporary2  MESSAGE-ID zmpp  .

TABLES: equi ,
        ausp .
************* DO NOT USE !!!! *****************************************
DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname,
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      it_rec                  LIKE TABLE OF mara       WITH HEADER LINE.
********************************************************************

DATA: wa_material             LIKE mara-matnr                 ,
      wa_instance             LIKE mara-cuobf                 ,
      wa_plnum                LIKE plaf-plnum                 ,
      wa_maxday               LIKE sy-datum                   ,
      l_decg(8)               TYPE n                          ,
      l_evcode(5)             TYPE c                          ,
      wa_lines                TYPE i                          ,
      wa_msg(70)              TYPE c                          ,
      wa_mng                  TYPE i                          ,
      wa_flag                 TYPE c                          ,
      wa_check                TYPE c                          ,
      wa_error                TYPE c                          ,
      wa_mode                 TYPE c   VALUE   'N'            .

FIELD-SYMBOLS: <field1>       TYPE ANY                        .

DATA: BEGIN OF it_vin         OCCURS 0                        .
        INCLUDE STRUCTURE     ztpp_pmt07jb_b .
DATA:   vin(17)               TYPE c         ,
      END OF it_vin                          ,
      it_7jb              LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE,
      it_bdcdata          LIKE TABLE OF bdcdata        WITH HEADER LINE,
      it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

DATA: BEGIN OF it_vehicle    OCCURS 0       .
        INCLUDE STRUCTURE    ztpp_pmt07jb_b .
DATA:   workorder            LIKE mara-matnr,
        instance             LIKE mara-cuobf,
        sorder(10)           TYPE c         ,
        porder               LIKE plaf-plnum,
        equnr                LIKE equi-equnr,
        matnr                LIKE mara-matnr,
        b_serial(6)          TYPE n         ,
        vin                  LIKE mara-matnr,
        e_flag               TYPE c         ,
      END OF it_vehicle                     .

DATA: it_ausp                LIKE TABLE OF ausp  WITH HEADER LINE.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS   p_run      RADIOBUTTON GROUP ra  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-102 FOR FIELD p_run  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS   p_rerun    RADIOBUTTON GROUP ra.
SELECTION-SCREEN COMMENT  (55) text-101 FOR FIELD p_rerun.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS: s_vin    FOR equi-equnr         .
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  IF p_run = 'X'.
    " Normal Processing...
    PERFORM get_data.
*    PERFORM check_data .
*    CHECK wa_flag IS INITIAL.
*    PERFORM record_processing.
*    PERFORM update_commonvlas    .
  ELSE.
    " Reprocessing..
  ENDIF.

  INCLUDE zcpp103_common_routine .

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_vals            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_equnr           LIKE mara-matnr,
        l_matnr           LIKE mara-matnr,
        l_salesorder      LIKE vbak-vbeln ,
        l_date            TYPE d         ,
        l_ver(3)          TYPE c         ,
        l_fsc             LIKE mara-matnr ,
        l_rsnum           LIKE resb-rsnum  ,
        l_atinn           LIKE cabn-atinn.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_VIN' .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp
    FROM ausp
   WHERE atinn = l_atinn
     AND atwrt IN s_vin  .

  LOOP AT it_ausp.
    CLEAR: l_vals, l_vals[].
    l_vals-atnam = 'P_WORK_ORDER'.    APPEND l_vals.
    l_vals-atnam = 'P_EXT_COLOR' .    APPEND l_vals.
    l_vals-atnam = 'P_INT_COLOR' .    APPEND l_vals.
    l_vals-atnam = 'P_VERSION'   .    APPEND l_vals.
    l_equnr = it_ausp-objek .

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_equnr
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              OTHERS       = 4.

    READ TABLE l_vals INDEX 1.
    l_matnr = l_vals-atwrt.            CLEAR: l_vals.
    READ TABLE l_vals INDEX 2.
    CONCATENATE l_matnr l_vals-atwrt INTO l_matnr.  CLEAR: l_vals.
    READ TABLE l_vals INDEX 3.
    CONCATENATE l_matnr l_vals-atwrt INTO l_matnr.  CLEAR: l_vals.

    SELECT SINGLE  wocredate fsc sales version
      INTO (l_date, l_fsc, l_salesorder, l_ver)
      FROM ztpp_wosum
     WHERE wo_ser = l_matnr(9)
       AND nation = l_matnr+9(3)
       AND dealer = l_matnr+12(2)
       AND extc   = l_matnr+14(2)
       AND intc   = l_matnr+16(2)   .

    PERFORM call_bdc_planned_order USING  l_fsc        l_ver+1(2)
                                          l_date       l_salesorder.

    PERFORM get_rsnum           USING WA_plnum  l_rsnum   .
    PERFORM CHANGE_EQUI_HEADER  USING l_equnr   L_RSNUM   .

    CLEAR: l_vals, l_vals[].
    l_vals-atnam = 'P_PLAN_ORDER'.    APPEND l_vals.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_equnr
              mode         = 'W'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              OTHERS       = 4.
  ENDLOOP.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_processing.
  GET TIME.
  WRITE AT: /001(40) 'Make the P/O & VM Header...............',
             041(10)  sy-datum ,
             051(02) '**'      ,
             053(10)  sy-uzeit .

*  SORT it_7jb BY ordr dist extc intc SSR1.
  SORT it_7jb BY sqdt ssr1.
  LOOP AT it_7jb .
*   CLEAR: WA_CHECK.
*   PERFORM CHECK_7JB_A .
*   CHECK WA_CHECK IS INITIAL.
    CASE it_7jb-mtgu .
      WHEN 'M'      .
        PERFORM process_mitu_value .
      WHEN OTHERS.
        PERFORM process_mitu_space .
    ENDCASE.
  ENDLOOP.

  GET TIME.
  WRITE AT: /001(40) 'Class Assign & Charateristics Assign...',
             041(10)  sy-datum ,
             051(02) '**'      ,
             053(10)  sy-uzeit .

  LOOP AT it_vehicle.
    PERFORM class_assign .
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
*&      Form  CALL_BDC_WORKORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_PLNMG  text
*----------------------------------------------------------------------*
FORM call_bdc_workorder USING    pa_material  pa_plnmg  pa_char  pa_z .
  DATA: l_vars                LIKE TABLE OF zspp_vin_value
                                                  WITH HEADER LINE,
        l_dec                 TYPE i,
        l_dec1(6)             TYPE c,
        l_dec2(6)             TYPE c.

  l_dec = pa_plnmg.
  WRITE l_dec    TO l_dec1.
*
*  WRITE AT: /001(50) 'Update the Work Order...........' ,
*             045(20)  pa_material                       .

  l_vars-atnam = pa_char .  l_vars-atwrt = l_dec1.  APPEND l_vars.
  IF pa_z = 'Z'.
    l_dec = wa_mng  .
    WRITE l_dec   TO l_dec2.
    l_vars-atnam = 'P_MITU_QTY' .  l_vars-atwrt =  l_dec2.
    APPEND l_vars.
  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object    = pa_material
            mode      = 'W'
            ctype     = '001'
       TABLES
            val_table = l_vars.

  LOOP AT l_vars.
    CHECK l_vars-zflag = 'E' .
    WRITE AT: /001(025) 'Function - Error Result : ' ,
               026(020)  pa_material  ,
               046(020) 'Characteristics is '        ,
               066(020)  l_vars-atwrt                .
  ENDLOOP.
ENDFORM.                    " CALL_BDC_WORKORDER

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
        l_return              LIKE bapireturn1 .

  " Create the Planned Order

  CLEAR: wa_plnum.

  l_header-material         = pa_matnr   .
  l_header-plan_plant       = 'P001'     .
  l_header-prod_plant       = 'P001'     .
  l_header-total_plord_qty  = 1          .
  l_header-order_start_date = pa_date    .
  l_header-order_fin_date   = pa_date    .
  l_header-plan_open_date   = pa_date    .
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

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
ENDFORM.                    " CALL_BDC_PLANNED_ORDER

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_SALES_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_sales_order   USING pa_sorder pa_val2  pa_val10  pa_val20.
  DATA: l_val10(6)          TYPE n           ,
        l_val20(6)          TYPE n           ,
        l_kwmeng            LIKE vbap-kwmeng .

  " Change the Sales Order
  l_val10 =  pa_val10 .
  l_val20 =  pa_val20 .
  l_val20 =  l_val20 - l_val10 .
*
*  WRITE AT: /001(50) 'Update the Sales Order..........' ,
*             045(20)  pa_sorder                         .

  PERFORM bdc_dynpro_processing USING :
                         'X'  'SAPMV45A'             '0102',
                         ' '  'BDC_OKCODE'           '=UER2' ,
                         ' '  'VBAK-VBELN'            pa_sorder,

                         'X'  'SAPMV45A'             '4001',
                         ' '  'BDC_OKCODE'           '=SICH' ,
                         ' '  'RV45A-KWMENG(01)'      l_val10,
                         ' '  'RV45A-KWMENG(02)'      l_val20.

  CALL TRANSACTION 'VA02'  USING it_bdcdata MODE wa_mode
                           MESSAGES INTO    it_msg    .

  LOOP AT it_msg.
    CHECK it_msg-msgid = 'E' .
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = it_msg-msgid
              msgnr               = it_msg-msgnr
              msgv1               = it_msg-msgv1
              msgv2               = it_msg-msgv2
              msgv3               = it_msg-msgv3
              msgv4               = it_msg-msgv4
         IMPORTING
              message_text_output = wa_msg.
    WRITE: /'Sales Order -- ' , wa_msg .
  ENDLOOP.

  CLEAR: it_bdcdata, it_bdcdata[], it_msg, it_msg[].
ENDFORM.                    " CALL_BDC_SALES_ORDER

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
        l_rsnum           LIKE resb-rsnum  ,
        l_general         LIKE bapi_itob ,
        l_specific        LIKE bapi_itob_eq_only,
        l_return          LIKE bapiret2.

  " Call the function for the Last Data...
  l_mode = 'EMF'.      " it_7jb-modl .
*
*  WRITE AT: /001(40) 'Create the Vehicle Master with..' ,
*             041(15) 'Sales Order is ',
*             056(15)  pa_sorder       ,
*             060(15) 'Plan Order is  ',
*             075(10)  pa_plnum        ,
*             086(17) 'VIN Spec COde is',
*             105(20)  pa_vin.

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
*
*  WRITE AT: /001(40) 'Update ZTPP_PMT07JB_A Table ....' ,
*             041(20)  sy-subrc.

  " Create the Vehicle Master
  CONCATENATE it_7jb-ordr  it_7jb-dist               INTO l_workorder.

  PERFORM get_workcenter      USING l_workcenter        .
  PERFORM get_eqfnr           USING l_eqfnr             .
  PERFORM get_rsnum           USING pa_plnum  l_rsnum   .
*  WRITE AT: /001(40) 'Create the Vehicle Master.......' ,
*             041(20)  l_equnr .

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

*  WRITE: /, 'Return Message for the Creation Equipment Heaa: ',
*             l_equnr, l_return-message.

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
*  pa_workcenter = '10000064' .
  SELECT SINGLE objid
               INTO pa_workcenter
               FROM crhd
               WHERE arbpl EQ 'T'.
ENDFORM.                    " GET_WORKCENTER

*&---------------------------------------------------------------------*
*&      Form  BDC_WOHD_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_wohd_summary.
  " Sum the MITU Qty and SEQ Qty ...  (Each Others..)
  DATA: l_plnmg(4)           TYPE n          ,
        l_ordr               LIKE it_7jb-ordr,
        l_dist               LIKE it_7jb-dist,
        l_seqqty             LIKE ztpp_wosum-seqqty ,
        l_mituqty            LIKE ztpp_wosum-mituqty,
        l_data               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE.

  SORT it_7jb BY ordr dist extc intc ssr1.
  CLEAR: l_seqqty, l_mituqty.
  READ TABLE it_7jb INDEX 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  MOVE-CORRESPONDING it_7jb  TO   it_vin     .

  LOOP AT it_7jb .
    IF it_7jb-ordr = l_ordr  AND  it_7jb-dist = l_dist .
      IF it_7jb-mtgu = space .
        l_seqqty    = l_seqqty + 1.
      ELSE.
        l_mituqty   = l_mituqty + 1 .
      ENDIF.
    ELSE.
      " Process.
      CLEAR: wa_material,  l_data, l_data[].
      CONCATENATE l_ordr      l_dist      INTO wa_material .

      " Work Order Header's SEQ  Qty Change......
      SELECT SINGLE cuobf INTO wa_instance
        FROM mara
       WHERE matnr = wa_material.

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object             = wa_material
*     MODE               = 'R'
          ctype              = '001'
        TABLES
          val_table          = l_data .

      CLEAR: l_plnmg, wa_mng, ausp.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
           EXPORTING
                input  = 'P_SEQ_QTY'
           IMPORTING
                output = ausp-atinn.

      SELECT SINGLE *
        FROM ausp
       WHERE objek = wa_material
         AND atinn = ausp-atinn    .

      IF sy-subrc = 0 .
        l_plnmg = ausp-atflv.
      ENDIF.
      l_plnmg = l_plnmg + l_seqqty     .
      CLEAR: l_data-atwrt, ausp        .
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
           EXPORTING
                input  = 'P_MITU_QTY'
           IMPORTING
                output = ausp-atinn.

      SELECT SINGLE *
        FROM ausp
       WHERE objek = wa_material
         AND atinn = ausp-atinn    .

      IF sy-subrc = 0 .
        wa_mng  = ausp-atflv.
      ENDIF.
      wa_mng  = wa_mng  - l_mituqty    .

      CLEAR: l_data-atwrt .
      READ TABLE l_data     WITH KEY atnam = 'P_VIN_SPEC'    .
      IF sy-subrc = 0.
        it_vin-vin  = l_data-atwrt .
      ENDIF.

      PERFORM call_bdc_workorder
                          USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

      APPEND it_vin .     CLEAR: it_vin.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      it_vin = it_7jb     .
      IF it_7jb-mtgu = space .
        l_seqqty    = 1.
      ELSE.
        l_mituqty   = 1 .
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF wa_lines > 0 .
    CLEAR: wa_material,  l_data, l_data[].
    CONCATENATE l_ordr      l_dist      INTO wa_material .

    " Work Order Header's SEQ  Qty Change......
    SELECT SINGLE cuobf INTO wa_instance
      FROM mara
     WHERE matnr = wa_material.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object    = wa_material
              ctype     = '001'
         TABLES
              val_table = l_data.

    CLEAR: l_plnmg, wa_mng, l_data-atwrt, ausp.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_SEQ_QTY'
         IMPORTING
              output = ausp-atinn.

    SELECT SINGLE *
      FROM ausp
     WHERE objek = wa_material
       AND atinn = ausp-atinn    .

    IF sy-subrc = 0 .
      l_plnmg = ausp-atflv.
    ENDIF.
    l_plnmg = l_plnmg + l_seqqty     .
    CLEAR: l_data-atwrt, ausp        .
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_MITU_QTY'
         IMPORTING
              output = ausp-atinn.

    SELECT SINGLE *
      FROM ausp
     WHERE objek = wa_material
       AND atinn = ausp-atinn    .

    IF sy-subrc = 0 .
      wa_mng  = ausp-atflv.
    ENDIF.
    wa_mng  = wa_mng  - l_mituqty    .

    CLEAR: l_data-atwrt .
    READ TABLE l_data     WITH KEY atnam = 'P_VIN_SPEC'    .
    IF sy-subrc = 0.
      it_vin-vin  = l_data-atwrt .
    ENDIF.

    PERFORM call_bdc_workorder
                        USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

    APPEND it_vin .
  ENDIF.
ENDFORM.                    " BDC_WOHD_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  bdc_wocl_summary
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_wocl_summary.
  DATA: l_plnmg              LIKE ztpp_wosum-seqqty ,
        l_modqty             LIKE ztpp_wosum-modqty ,
        l_salesorder         LIKE ztpp_wosum-sales,
        l_ordr               LIKE it_7jb-ordr,
        l_dist               LIKE it_7jb-dist,
        l_extc               LIKE it_7jb-extc,
        l_intc               LIKE it_7jb-intc,
        l_seq                LIKE ztpp_wosum-seqqty ,
        l_seqqty             LIKE ztpp_wosum-seqqty ,
        l_tseq               LIKE ztpp_wosum-seqqty ,
        l_mituqty            LIKE ztpp_wosum-mituqty,
        l_data               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE.

  SORT it_7jb BY ordr dist extc intc ssr1.
  CLEAR: l_seqqty, l_mituqty, l_seq .
  READ TABLE it_7jb INDEX 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  l_extc = it_7jb-extc.  l_intc = it_7jb-intc.

  LOOP AT it_7jb .
    IF it_7jb-ordr = l_ordr  AND  it_7jb-dist = l_dist AND
       it_7jb-extc = l_extc  AND  it_7jb-intc = l_intc .
      IF it_7jb-mtgu = space .
        l_seq       = l_seq    + 1.
      ELSE.
        l_mituqty   = l_mituqty + 1 .
      ENDIF.
    ELSE.
      " Work Order Summary Table Update.....
      SELECT SINGLE modqty seqqty sales
               INTO (l_modqty, l_tseq, l_salesorder)
        FROM ztpp_wosum
       WHERE wo_ser = l_ordr
         AND nation = l_dist(3)
         AND dealer = l_dist+3(2)
         AND extc   = l_extc
         AND intc   = l_intc     .

      l_seqqty  = l_seq     + l_tseq  .
      UPDATE ztpp_wosum   SET seqqty  = l_seqqty
                        WHERE wo_ser = l_ordr
                          AND nation = l_dist(3)
                          AND dealer = l_dist+3(2)
                          AND extc   = l_extc
                          AND intc   = l_intc     .

      IF sy-subrc NE 0.
        " Step 1: Update Fail - Reason: Data not found!!!
        " Save the Working data.
        wa_flag = 'X'.
        EXIT.
      ENDIF.
*
*      WRITE AT: /001(50) 'Update WorkOrder Summary...' ,
*                 050(05)  sy-subrc,
*                /001(09)  l_ordr ,
*                 011(05)  l_dist ,
*                 017(03)  l_extc ,
*                 021(03)  l_intc .
      " Process.
      CLEAR: wa_material,  l_data, l_data[].
      CONCATENATE l_ordr       l_dist         INTO wa_material .
      CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

      SELECT SINGLE cuobf INTO wa_instance
        FROM mara
       WHERE matnr = wa_material.

      CLEAR: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
           EXPORTING
                input  = 'P_SEQ_QTY'
           IMPORTING
                output = ausp-atinn.

      SELECT SINGLE *
        FROM ausp
       WHERE objek = wa_material
         AND atinn = ausp-atinn    .

      IF sy-subrc = 0 .
        l_plnmg = ausp-atflv.
      ENDIF.
      l_plnmg = l_plnmg + l_seq        .
      CLEAR: l_data-atwrt, ausp        .
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
           EXPORTING
                input  = 'P_MITU_QTY'
           IMPORTING
                output = ausp-atinn.

      SELECT SINGLE *
        FROM ausp
       WHERE objek = wa_material
         AND atinn = ausp-atinn    .

      IF sy-subrc = 0 .
        wa_mng  = ausp-atflv.
      ENDIF.
      wa_mng  = wa_mng  - l_mituqty    .
      PERFORM call_bdc_workorder
                           USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

      " Sales Order Master Change...
      PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
                                         l_seqqty     l_modqty  .
      CLEAR: l_seq, l_mituqty.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      l_extc = it_7jb-extc.  l_intc = it_7jb-intc.
      IF it_7jb-mtgu = space .
        l_seq       = 1.
      ELSE.
        l_mituqty   = 1 .
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF wa_lines > 0 .
    " Work Order Summary Table Update.....
    SELECT SINGLE modqty seqqty sales
            INTO (l_modqty, l_tseq, l_salesorder)
      FROM ztpp_wosum
     WHERE wo_ser = it_7jb-ordr
       AND nation = it_7jb-dist(3)
       AND dealer = it_7jb-dist+3(2)
       AND extc   = it_7jb-extc
       AND intc   = it_7jb-intc     .

    l_seqqty  = l_seq     + l_tseq  .
    UPDATE ztpp_wosum   SET seqqty  = l_seqqty
                      WHERE wo_ser = it_7jb-ordr
                        AND nation = it_7jb-dist(3)
                        AND dealer = it_7jb-dist+3(2)
                        AND extc   = it_7jb-extc
                        AND intc   = it_7jb-intc     .
*
*    WRITE AT: /001(50) 'Update WorkOrder Summary...' ,
*               050(05)  sy-subrc,
*              /001(09)  l_ordr ,
*               011(05)  l_dist ,
*               017(03)  l_extc ,
*               021(03)  l_intc .

    CLEAR: wa_material,  l_data, l_data[].
    CONCATENATE l_ordr       l_dist         INTO wa_material .
    CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

    SELECT SINGLE cuobf INTO wa_instance
      FROM mara
     WHERE matnr = wa_material.

    CLEAR: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_SEQ_QTY'
         IMPORTING
              output = ausp-atinn.

    SELECT SINGLE *
      FROM ausp
     WHERE objek = wa_material
       AND atinn = ausp-atinn    .

*     READ TABLE l_data WITH KEY atnam = 'P_SEQ_QTY'  .
*     l_plnmg = l_data-atwrt .
*     l_plnmg = AUSP-atwrt + AUSP-ATFLV.
    IF sy-subrc = 0 .
      l_plnmg = ausp-atflv.
    ENDIF.
    l_plnmg = l_plnmg + l_seq        .
    CLEAR: l_data-atwrt, ausp        .
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_MITU_QTY'
         IMPORTING
              output = ausp-atinn.

    SELECT SINGLE *
      FROM ausp
     WHERE objek = wa_material
       AND atinn = ausp-atinn    .

*     READ TABLE l_data WITH KEY atnam = 'P_MITU_QTY' .
*     wa_mng  = l_data-atwrt .
*     wa_mng  = AUSP-atwrt + AUSP-ATFLV.
    IF sy-subrc = 0 .
      wa_mng  = ausp-atflv.
    ENDIF.
    wa_mng  = wa_mng  - l_mituqty    .
    PERFORM call_bdc_workorder
                         USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

    " Sales Order Master Change...
    PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
                                       l_seqqty     l_modqty  .
  ENDIF.
ENDFORM.                    " bdc_wocl_summary

*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  DATA: l_plnmg(4)           TYPE n          ,
        l_salesorder         LIKE ztpp_wosum-sales,
        l_ordr               LIKE it_7jb-ordr,
        l_dist               LIKE it_7jb-dist,
        l_extc               LIKE it_7jb-extc,
        l_intc               LIKE it_7jb-intc,
        l_seqqty             LIKE ztpp_wosum-seqqty ,
        l_modqty             LIKE ztpp_wosum-modqty ,
        l_chkqty             TYPE i                 ,
        l_count              TYPE i                 ,
        l_data               LIKE TABLE OF conf_out    WITH HEADER LINE.

*  WRITE AT: /001(050) 'Check the Data: for the SEQ Qty.... '.

  SORT it_7jb BY ordr dist extc intc ssr1.
  CLEAR: l_seqqty.
  READ TABLE it_7jb INDEX 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  l_extc = it_7jb-extc.  l_intc = it_7jb-intc.

  LOOP AT it_7jb .
    IF it_7jb-ordr = l_ordr  AND  it_7jb-dist = l_dist AND
       it_7jb-extc = l_extc  AND  it_7jb-intc = l_intc .
      IF it_7jb-mtgu = space .
        l_count      = l_count  + 1.
      ENDIF.
    ELSE.
      " Processing Data Check with ZTPP_WOSUM Table...
      SELECT SINGLE modqty seqqty INTO (l_modqty, l_seqqty)
        FROM ztpp_wosum
       WHERE wo_ser = l_ordr
         AND nation = l_dist(3)
         AND dealer = l_dist+3(2)
         AND extc   = l_extc
         AND intc   = l_intc     .

      l_chkqty  = l_modqty - l_seqqty .
      IF l_chkqty < l_count .
        wa_flag  = 'E'     .
        SKIP 1 .
        WRITE AT: /001(50) 'Error of the Sequencial Data OVER!' ,
                  /001(09)  l_ordr ,
                   011(05)  l_dist ,
                   017(03)  l_extc ,
                   021(03)  l_intc .
      ENDIF.
      IF it_7jb-mtgu = space .
        l_count = 1 .
      ELSE.
        CLEAR: l_count.
      ENDIF.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      l_extc = it_7jb-extc.  l_intc = it_7jb-intc.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_DATA

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
  l_vartable-atnam = 'P_VM_DATE'         .
  CONCATENATE sy-datum sy-uzeit           INTO  l_vartable-atwrt .
  APPEND l_vartable.
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
  l_vartable-atnam = 'P_RP_STATUS'     .
  l_vartable-atwrt = '00'              .        APPEND l_vartable.
  l_vartable-atnam = 'P_STATUS'        .
  l_vartable-atwrt = 'B00'             .        APPEND l_vartable.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object    = it_vehicle-matnr  " wa_material
            ctype     = '001'
       TABLES
            val_table = l_conf.

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
*&      Form  CHECK_7JB_A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_7jb_a.
  TABLES: ztpp_pmt07jb_a.

  SELECT SINGLE *
    FROM ztpp_pmt07jb_a
   WHERE sqdt = it_7jb-sqdt
     AND plnt = it_7jb-plnt
     AND line = it_7jb-line
     AND modl = it_7jb-modl
     AND mtgu = it_7jb-mtgu
     AND ssr1 = it_7jb-ssr1
     AND ssr2 = it_7jb-ssr2 .

  IF sy-subrc = 0 AND ztpp_pmt07jb_a-vinn = space .
    wa_check = 'X' .
  ENDIF.

*  MOVE-CORRESPONDING      it_7jb                 TO     it_vehicle.
*  it_vehicle-e_flag     = wa_flag   .
*  CONCATENATE it_7jb-ordr it_7jb-dist INTO it_vehicle-matnr .
*  it_vehicle-instance   = l_instance.
*  it_vehicle-workorder  = l_instance.
*  it_vehicle-sorder     = pa_sorder .
*  it_vehicle-porder     = pa_plnum  .
*  it_vehicle-b_serial   = l_seq     .
*  it_vehicle-vin        = pa_vin    .
*  it_vehicle-equnr      = l_equnr   .            APPEND it_vehicle.
ENDFORM.                    " CHECK_7JB_A

*&---------------------------------------------------------------------*
*&      Form  CLASS_ASSIGN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM class_assign.
  DATA: l_equi            LIKE  v_equi  .

  l_equi-equnr = it_vehicle-equnr.    " l_equnr.

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
  SELECT SINGLE rsnum INTO pa_rsnum
    FROM plaf
   WHERE plnum = pa_plnum.
ENDFORM.                    " get_rsnum

*&---------------------------------------------------------------------*
*&      Form  update_commonvlas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_commonvlas.
  DATA: c_prog                LIKE ztpp_common_vals-jobs.

  c_prog = 'ZIPP101U_PMT07JB_A'.
  SELECT MAX( sqdt ) INTO wa_maxday
    FROM ztpp_pmt07jb_b .
  UPDATE ztpp_common_vals SET dates = wa_maxday
                        WHERE jobs  = c_prog   .
ENDFORM.                    " update_commonvlas

*&---------------------------------------------------------------------*
*&      Form  CHANGE_EQUI_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_RSNUM  text
*----------------------------------------------------------------------*
FORM CHANGE_EQUI_HEADER USING    pA_EQUNR  PA_rsnum.
  DATA: l_general         LIKE BAPI_ITOB ,
        l_generalX        LIKE BAPI_ITOBX,
        l_specific        LIKE BAPI_ITOB_EQ_ONLY,
        l_specificX       LIKE BAPI_ITOB_EQ_ONLYX,
        l_return          LIKE bapiret2.

  l_general-sortfield        =  PA_rsnum                .
  l_generalx-sortfield       =  'X'                     .
  clear: l_specific, l_specificx.

  CALL FUNCTION 'BAPI_EQUI_CHANGE'
       EXPORTING
            EQUIPMENT       = PA_equnr
            data_general    = l_general
            data_generalx   = l_generalx
            data_specific   = l_specific
            DATA_SPECIFICX  = l_specificX
       IMPORTING
            return          = l_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
ENDFORM.                    " CHANGE_EQUI_HEADER
