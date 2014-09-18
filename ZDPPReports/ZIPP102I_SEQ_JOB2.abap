************************************************************************
* Program Name      : ZIPP102I_SEQ_JOB2
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
************************************************************************
REPORT  zipp102i_seq_job2    MESSAGE-ID zmpp  .

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
      wa_fsc                  LIKE mara-matnr                 ,
      wa_7jb                  LIKE ztpp_pmt07jb_b             ,
      wa_sorder               LIKE vbak-vbeln                 ,
      wa_plnum                LIKE plaf-plnum                 ,
      wa_equnr                LIKE equi-equnr                 ,
      wa_b_serial(6)          TYPE n                          ,
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
*DATA: BEGIN OF wa_vehicle    OCCURS 0       .
*        INCLUDE STRUCTURE    ztpp_pmt07jb_b .
*DATA:   workorder            LIKE mara-matnr,
**       instance             LIKE mara-cuobf,
*        sorder(10)           TYPE c         ,
*        porder               LIKE plaf-plnum,
*        equnr                LIKE BAPI_ITOB_PARMS-EQUIPMENT,
*        matnr                LIKE mara-matnr,
*        b_serial(6)          TYPE n         ,
*        vin                  LIKE mara-matnr,
*        e_flag               TYPE c         ,
*      END OF wa_vehicle                     .

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_7jb            LIKE ztpp_pmt07jb_b,
            p_log            LIKE ztpp_pp_log_head-logkey,
            p_date           TYPE d .
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM write_test       .
  PERFORM set_variables    .
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
        l_fsc                LIKE mara-matnr ,
        l_vin                LIKE mara-matnr ,
        l_mode               LIKE ztpp_common_vals-key2.

  l_mode = 'EMF'.      " wa_7jb-modl .
*  MOVE-CORRESPONDING wa_7jb  TO     wa_vehicle .
*  wa_vehicle-matnr = wa_material .
  CONCATENATE l_mode wa_7jb-vinn+11(7) INTO wa_equnr.

  SELECT SINGLE fsc sales INTO (l_fsc, wa_sorder)
    FROM ztpp_wosum
   WHERE wo_ser = wa_7jb-ordr
     AND nation = wa_7jb-dist(3)
     AND dealer = wa_7jb-dist+3(2)
     AND extc   = wa_7jb-extc
     AND intc   = wa_7jb-intc     .

  " Processing of the Transaction MD11
  WRITE wa_7jb-sqdt TO l_date .

  CONCATENATE wa_7jb-moye wa_7jb-dist  wa_7jb-bmdl   INTO wa_fsc     .
  CONCATENATE wa_fsc      wa_7jb-ocnn INTO wa_fsc    SEPARATED BY ' '.

  PERFORM call_bdc_planned_order USING  wa_fsc       wa_7jb-pver
                                        l_date       wa_sorder   .

* IF wa_error = 'X'.  EXIT.  ENDIF.

  " Vehicle Master Update for Plan Order & Reservation Number..
  PERFORM vin_vm_creation USING wa_plnum wa_sorder    wa_fsc .

  if wa_error = 'X'.  exit.  endif.

  PERFORM generate_characterisitc_vm .
ENDFORM.                    " RECORD_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_PLANNED_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_planned_order  USING pa_fsc   pa_verid pa_date pa_sorder.
  DATA: l_sloc                TYPE lgort_d     ,
        l_header              LIKE bapiplaf_i1 ,
        l_return              LIKE bapireturn1 .

  " Create the Planned Order

  CLEAR: wa_plnum.
  PERFORM get_sloc          USING pa_fsc  l_sloc.

  l_header-material         = pa_fsc     .
  l_header-plan_plant       = 'P001'     .
  l_header-prod_plant       = 'P001'     .
  l_header-total_plord_qty  = 1          .
  l_header-order_start_date = wa_7jb-sqdt.
  l_header-order_fin_date   = wa_7jb-sqdt.
  l_header-plan_open_date   = wa_7jb-sqdt.
  l_header-firming_ind      = 'X'        .
*  L_HEADER-BOM_EXP_FIX_IND  = 'X'        .
  l_header-sales_ord        = pa_sorder  .
  l_header-s_ord_item       = 10         .
  l_header-version          = pa_verid   .
  l_header-stge_loc         = l_sloc     .
  l_header-mrp_area         = 'P001'     .
  l_header-pldord_profile   = 'KD'       .
  l_header-acctasscat       = 'M'        .

  CALL FUNCTION 'BAPI_PLANNEDORDER_CREATE'
       EXPORTING
            headerdata   = l_header
       IMPORTING
            return       = l_return
            plannedorder = wa_plnum.

  IF l_return-type = 'E' OR l_return-type = 'A'.
    PERFORM create_log  using '5' l_return-message .
*   wa_error = 'X'     .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
           WAIT    = 'X'.
  ENDIF.
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
        l_rsnum           LIKE resb-rsnum  ,
        l_general         LIKE bapi_itob ,
        l_generalx        LIKE bapi_itobx,
        l_specific        LIKE bapi_itob_eq_only,
        l_specificx       LIKE bapi_itob_eq_onlyx,
        l_return          LIKE bapiret2.

  wa_b_serial = l_seq  = wa_7jb-vinn+11(6) .
* CONCATENATE 'EMF'           l_seq INTO wa_equnr .
* CONCATENATE wa_7jb-bmdl(3)  l_seq INTO l_equnr .

  " Check the result-code of the BDC .
  " if it is successful, Saveing the Planned Order Number to the table
  " or not, Logging the Result into the table
  UPDATE ztpp_pmt07jb_b  SET  plnum = wa_plnum
                        WHERE sqdt  = wa_7jb-sqdt
                          AND modl  = wa_7jb-modl
                          AND mtgu  = wa_7jb-mtgu
                          AND ssr1  = wa_7jb-ssr1 .

  " Create the Vehicle Master
  CONCATENATE  wa_7jb-ordr  wa_7jb-dist  wa_7jb-extc  wa_7jb-intc
         INTO l_workorder.
* CONCATENATE pa_fsc wa_7jb-vers+1(2) l_workorder
*        INTO l_general-descript SEPARATED BY space     .

  PERFORM get_rsnum           USING pa_plnum  l_rsnum   .

  l_general-sortfield        =  l_rsnum                 .
  l_generalx-sortfield       =  'X'                     .
  CLEAR: l_specific, l_specificx.

  WRITE: / 'BAPI-HEAD Update ==>', wa_equnr .

  CALL FUNCTION 'BAPI_EQUI_CHANGE'
       EXPORTING
            equipment      = wa_equnr
            data_general   = l_general
            data_generalx  = l_generalx
            data_specific  = l_specific
            data_specificx = l_specificx
       IMPORTING
            return         = l_return.

  IF l_return-type = 'A' OR l_return-type = 'E'.
    PERFORM create_log  using '6' l_return-message.
    wa_error = 'X'          .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
           WAIT    = 'X'  .
*   WAIT UP TO 1  SECONDS.
  ENDIF.
* PERFORM write_return_bapi   USING l_return  wa_equnr.
ENDFORM.                    " VIN_VM_CREATION

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
  CONCATENATE P_7jb-ordr P_7jb-dist INTO wa_material .
  CHECK wa_flag NE 'E' .
  IF wa_7jb-mtgu = 'M' .
    l_vartable-atnam = 'P_MITU' .
    l_vartable-atwrt = 'Y'              .       APPEND l_vartable.
    l_vartable-atnam = 'P_MITU_DATE'.
    l_vartable-atwrt =  wa_7jb-sqdt .           APPEND l_vartable.
  ENDIF.
  l_vartable-atnam = 'P_OCN'.
  l_vartable-atwrt =  wa_7jb-ocnn .             APPEND l_vartable.
  l_vartable-atnam = 'P_VERSION'.
  l_vartable-atwrt =  wa_7jb-vers .             APPEND l_vartable.
  l_vartable-atnam = 'P_DESTINATION_CODE'.
  l_vartable-atwrt =  wa_7jb-dist .             APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_DATE'.
  l_vartable-atwrt =  wa_7jb-sqdt .             APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_SERIAL'.
  l_vartable-atwrt =  wa_7jb-ssr1 .             APPEND l_vartable.
  l_vartable-atnam = 'P_SEQUENCE_CODE'.
  l_vartable-atwrt =  wa_7jb-sqcd .             APPEND l_vartable.
  CONCATENATE wa_7jb-evl1      wa_7jb-evl2      wa_7jb-evl3
              wa_7jb-evl4      wa_7jb-evl5      INTO l_evcode    .
  l_vartable-atnam = 'P_EPI_CODE'     .
  l_vartable-atwrt =  l_evcode        .         APPEND l_vartable.
  l_vartable-atnam = 'P_SALES_ORDER'  .
  l_vartable-atwrt =  wa_sorder       .         APPEND l_vartable.
  l_vartable-atnam = 'P_PLAN_ORDER'   .
  l_vartable-atwrt =  wa_plnum        .         APPEND l_vartable.
  l_vartable-atnam = 'P_STATUS'       .
  l_vartable-atwrt = 'B00'            .         APPEND l_vartable.
  l_vartable-atnam = 'P_RP_STATUS'    .
  l_vartable-atwrt = '00'             .         APPEND l_vartable.
  l_vartable-atnam = 'P_VM_DATE'      .
  CONCATENATE sy-datum sy-uzeit           INTO  l_vartable-atwrt .
  APPEND l_vartable.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object     = wa_material
            ctype      = '001'
       TABLES
            val_table  = l_conf
       EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.

  CLEAR: l_conf-atwrt .
  READ TABLE l_conf WITH KEY atnam = 'P_LC_NO'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_LC_NO'         .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MODEL'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MODEL'         .        APPEND l_vartable.
  l_vartable-atwrt = wa_b_serial       .        CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_BODY_SERIAL'   .        APPEND l_vartable.
  CONCATENATE wa_7jb-ordr      wa_7jb-dist      INTO   l_vartable-atwrt.
  CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_WORK_ORDER'    .        APPEND l_vartable.
  l_vartable-atwrt = wa_7jb-extc       .        CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_EXT_COLOR'     .        APPEND l_vartable.
  l_vartable-atwrt = wa_7jb-intc       .        CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_INT_COLOR'     .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MODEL_YEAR'.
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MODEL_YEAR'    .        APPEND l_vartable.
  READ TABLE l_conf WITH KEY atnam = 'P_MI'   .
  l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
  l_vartable-atnam = 'P_MI'            .        APPEND l_vartable.
  l_vartable-atwrt = wa_7jb-vinn       .        CLEAR: l_conf-atwrt.
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

  l_equnr = wa_equnr.

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
    perform create_log using '7' text-001 .
    wa_error = 'X'      .
  ENDIF.
ENDFORM.                    " GENERATE_CHARACTERISITC_VM

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
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
FORM create_log  using  pa_step pa_msg .
  DATA: l_log                LIKE ztpp_rep_seq .

  CLEAR: l_log.
  SELECT MAX( sequence ) INTO l_log-sequence
    FROM ztpp_rep_seq
   WHERE wk_date  = p_date  .

  l_log-wk_date   = p_date             .
  l_log-sequence  = l_log-sequence + 1 .
  l_log-step      = pa_step            .
  l_log-msg       = pa_msg             .
  l_log-status    = 'E'                .
  l_log-logtype   = 'E'                .
  MOVE-CORRESPONDING p_7jb    TO l_log .
  INSERT INTO ztpp_rep_seq VALUES l_log.
  wa_error = 'X'                       .
*  wa_seq = wa_seq + 1 .
*
*  " Log Detail Creation
*  ztpp_pp_log_deta-logkey   = wa_number    .
*  ztpp_pp_log_deta-sequence = wa_seq      .
*  ztpp_pp_log_deta-logtype  = pa_type     .
*  ztpp_pp_log_deta-keydata  = pa_key      .
*  INSERT INTO ztpp_pp_log_deta VALUES ztpp_pp_log_deta .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  SET_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_variables.
  DATA: l_atinn             LIKE cabn-atinn,
        l_material          LIKE mara-matnr.

  wa_7jb = p_7jb .
  CONCATENATE wa_7jb-ordr wa_7jb-dist INTO wa_material .
ENDFORM.                    " SET_VARIABLES

*&---------------------------------------------------------------------*
*&      Form  WRITE_TEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_test.
  WRITE: / 'Called Program...'          .
  WRITE: / p_7jb-vinn, p_log,
         / p_7jb-dist, p_7jb-ordr       ,
         / p_7jb      .
ENDFORM.                    " WRITE_TEST

*&---------------------------------------------------------------------*
*&      Form  write_return_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_RETURN  text
*----------------------------------------------------------------------*
FORM write_return_bapi USING    pa_return STRUCTURE bapiret2 pa_equnr.
  WRITE AT: /001(020)    pa_equnr,
             021(001)    pa_return-type,
             023(020)    pa_return-id  ,
             044(003)    pa_return-number,
             048(030)    pa_return-message.
ENDFORM.                    " write_return_bapi

*&---------------------------------------------------------------------*
*&      Form  get_sloc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_FSC  text
*----------------------------------------------------------------------*
FORM get_sloc USING    pa_fsc  pa_lgort.
  SELECT SINGLE lgpro INTO pa_lgort
    FROM marc
   WHERE matnr = pa_fsc
     AND werks = 'P001' .
ENDFORM.                    " get_sloc
