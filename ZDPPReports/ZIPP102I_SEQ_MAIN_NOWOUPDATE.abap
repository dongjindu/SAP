************************************************************************
* Program Name      : ZIPP102I_SEQ_MAIN
* Author            : Bobby
* Creation Date     : 2004.02.08.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No :
* Addl Documentation:
* Description       : Vehicle Order(Planned Order) Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/11/05   Furong       UD1K917863   continue to run if error occurs
*                         UD1K917912   in characteristic creation;
*                                      create issue log ZTPP_SEQ_LOG
*                                      which will be reprocessed by
*                                      other program
************************************************************************
REPORT  zipp102i_seq_main   MESSAGE-ID zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_seq_backup ,    " Sequence Table's Information for the Reco
        ztpp_common_vals,    " PP: Common Values
        equi ,               " Vehicle Master(Header)
        ausp .               " Vehicle Master(Detail-Characteristic)

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF it_vin         OCCURS 0                        .
        INCLUDE STRUCTURE     ztpp_pmt07jb_b .
DATA:   matnr                 LIKE mara-matnr,
      END OF it_vin                          ,
      it_7jb              LIKE TABLE OF it_vin         WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE,
      it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: wa_material             LIKE mara-matnr                 ,
      wa_7jb                  LIKE it_vin                     ,
      wa_7jb_log              LIKE it_vin                     ,
      wa_maxday               LIKE sy-datum                   ,
      wa_minday               LIKE sy-datum                   ,
      wa_lines                TYPE i                          ,
      wa_msg(70)              TYPE c                          ,
      wa_mng                  TYPE i                          ,
      wa_snd_jobs             TYPE i                          ,
      wa_rcv_jobs             TYPE i                          ,
      wa_taskname(4)          TYPE n VALUE '0001'             ,
      wa_excp_flag            TYPE c                          ,
      wa_error                TYPE c                          ,
      wa_flag                 TYPE c                          ,
      wa_date                 TYPE d                          ,
      wa_err_hd               TYPE c                          ,
      wa_subrc                LIKE sy-subrc                   ,
      c_prog                  LIKE sy-repid                   .

*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS   p_run                            DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-102 FOR FIELD p_run  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
TOP-OF-PAGE.
*----------------------------------------------------------------------
  IF wa_err_hd = 'E'     .
    WRITE AT: /001(50)  text-001.
    SKIP 1 .
    ULINE AT: /(50)             .
    WRITE AT: /001(09)  text-201,     " l_ordr ,
               011(05)  text-202,     " l_dist ,
               017(03)  text-203,     " l_extc ,
               021(03)  text-204.     " l_intc .
    ULINE AT: /(50)             .
  ENDIF.

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  GET TIME.
  wa_date = sy-datum.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  DELETE FROM ztpp_rep_seq CLIENT SPECIFIED WHERE mandt = sy-mandt.
  DELETE FROM ztpp_seq_log CLIENT SPECIFIED WHERE mandt = sy-mandt.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.

  IF p_run = 'X'.
    " Normal Processing...
    IF sy-batch = ' '.
** for testing only
*      PERFORM exit_message  .
*      EXIT.
    ENDIF.
    PERFORM get_data        .
    CHECK wa_flag IS INITIAL.
    PERFORM check_data .
    CHECK wa_flag IS INITIAL.
    PERFORM back_up_data1   .         " BACK-UP ROUTINE FOR THE RECOVERY
*workorder color update
    PERFORM bdc_wocl_summary .        " Step 1: Work Order Color
    IF wa_error = 'X'        .
      MESSAGE i001 WITH text-905 '#7 Rollback STEP 1-1 W.Color'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .

      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      PERFORM create_log USING '1' wa_7jb_log. " Log Create in STEP 1 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.
*workorder header update
    PERFORM bdc_wohd_summary .
    IF wa_error = 'X'        .
      MESSAGE i001 WITH text-905 '#10 Rollback STEP 1-2'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      PERFORM create_log USING '2' wa_7jb_log. " Log Create in STEP 2 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.
*vehicle
    PERFORM clear_vinn       .
    PERFORM record_processing.           " Log Create in Step 3 & 4 .
  ELSE.
    " Reprocessing..
  ENDIF.

  CHECK wa_error = space   .
  PERFORM check_result     .
  PERFORM write_result     .
  PERFORM write_timestamp  USING  text-011 .

END-OF-SELECTION.
  PERFORM check_pcc_routine.
  PERFORM call_plan_save   .


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: com_date             TYPE d,
        chk_date             TYPE d.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_b .

  DESCRIBE TABLE it_7jb LINES wa_lines .
  READ TABLE it_7jb INDEX 1.
  chk_date = it_7jb-sqdt   .

  " Data Check for Re-Running..
*  c_prog = 'ZIPP101U_PMT07JB_A'.
*  SELECT SINGLE *
*    FROM ztpp_common_vals
*   WHERE jobs  = c_prog
*     AND key2  = it_7jb-modl .
*
*  com_date = ztpp_common_vals-item1.   " Previous Max Sequenced Date..
*  IF chk_date <= com_date .
*    wa_flag  =  'X'      .            " Already Sequenced Data..
*    MESSAGE i001 WITH text-012 .
*  ENDIF.
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
  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  SORT it_7jb BY sqdt ssr1.
  LOOP AT it_7jb WHERE mtgu NE 'M'.
    PERFORM check_vin         .
    PERFORM vin_code_gen      .
    IF wa_error = 'X'.
      MESSAGE i001 WITH text-905 '#14 Error VIN_CODE_GEN'.
      EXIT.
    ENDIF.
    MODIFY it_7jb.
  ENDLOOP.

  IF wa_error = 'X'.
    MESSAGE i001 WITH text-905 '#15 Error Rollback in STEP 3-1'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    " VIN Code Generation Error...
    PERFORM create_log USING '3' it_7jb.   " Log Create in STEP 3    .
    EXIT .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
  ENDIF.

  CLEAR: wa_rcv_jobs, wa_snd_jobs.
  LOOP AT it_7jb .
    CONCATENATE it_7jb-moye  it_7jb-dist it_7jb-bmdl INTO it_7jb-matnr.
    CONCATENATE it_7jb-matnr it_7jb-ocnn             INTO it_7jb-matnr
      SEPARATED BY space.

    wa_7jb = it_7jb .
    " Error Log Step 4 : Call the Job-Processor.....
    CASE it_7jb-mtgu .
      WHEN 'M'      .
        PERFORM process_mitu_value .
      WHEN OTHERS.
        PERFORM select_vin        .
        PERFORM job_create  USING  '1'           .
    ENDCASE.
    MODIFY it_7jb.
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.


  CLEAR: wa_rcv_jobs, wa_snd_jobs, wa_taskname.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.

  IF sy-subrc = 0  AND  wa_excp_flag = space.
  ELSE.
*    MESSAGE i001 WITH text-905 '#16 Error Rollback  Vehicle Master'.
    MESSAGE w001 WITH text-300 .
** added by furong on 11/16/05
    WRITE: 'SY:', sy-subrc,sy-msgid,sy-msgty,sy-msgv1,sy-msgv2,sy-msgv3.
    WRITE: / 'wa_excp_flag =', wa_excp_flag.
    WRITE: / 'Received jobs =', wa_rcv_jobs.
    WRITE: / 'Sent jobs =', wa_snd_jobs.
** end of change

*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .


    WRITE AT: /001(010) 'RT-Code: '   ,
               012(003) sy-subrc      ,
               016(013) 'Send Count: ',
               030(003) wa_snd_jobs   ,
               035(013) 'Recv Count: ',
               069(003) wa_rcv_jobs   .
    ULINE.                                " TO-BE DELETE LINE.

    PERFORM create_log USING '6' it_7jb.   " Log Create in STEP 4    .
* changed by furong on 11/15/05
*   EXIT.
* end of change
  ENDIF.

  CLEAR: wa_rcv_jobs, wa_snd_jobs.
  LOOP AT it_7jb WHERE mtgu NE 'M' .
    " Error Log Step 5 : Call the Job-Processor.....
    PERFORM select_vin        .
    PERFORM job_create USING '2'         .
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.
  IF sy-subrc = 0  AND  wa_excp_flag = space.

  ELSE.
*    MESSAGE i001 WITH text-905 '#17 Error Rollback Planned Order'.
** andded by furong on 11/16/2005
    MESSAGE w001 WITH text-400 .

    WRITE: 'sy:', sy-subrc,sy-msgid,sy-msgty,sy-msgv1,sy-msgv2,sy-msgv3.
    WRITE: / 'wa_excp_flag =', wa_excp_flag.
    WRITE: / 'Received jobs =', wa_rcv_jobs.
    WRITE: / 'Sent jobs =', wa_snd_jobs.
** end of change

*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    WRITE AT: /001(010) 'RT-Code: '   ,
               012(003) sy-subrc      ,
               016(013) 'Send Count: ',
               030(003) wa_snd_jobs   ,
               035(013) 'Recv Count: ',
               069(003) wa_rcv_jobs   .
    ULINE.                                " TO-BE DELETE LINE.
*    MESSAGE w001 WITH text-400 .
    PERFORM create_log USING '9' it_7jb.   " Log Create in STEP 5    .
* changed by furong on 11/15/05
*   EXIT.
* end of change
  ENDIF.

  PERFORM update_commonvlas    .
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
        l_objek              LIKE ausp-objek ,
        l_ausp               LIKE ausp       ,
        l_mituqty            LIKE ztpp_wosum-mituqty,
        l_data               LIKE TABLE OF zspp_vin_value
                                         WITH HEADER LINE,
        l_plnmg              LIKE ztpp_wosum-seqqty .


  CLEAR: wa_material.
  CONCATENATE it_7jb-ordr it_7jb-dist INTO wa_material ."workorder h
  CONCATENATE it_7jb-modl it_7jb-vhno INTO l_objek     ."vehicle num

  " Check the Vehicle for the MITU...
  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_MITU'      .

  SELECT SINGLE *  INTO l_ausp
    FROM ausp
   WHERE objek = l_objek
     AND klart = '002'
     AND atinn = l_atinn .
*requested by hur,changed by wskim,on 2004.11.05
*When Mitu vehicle is entered in BODY-IN Point without resequencing
*-----start
*  CHECK sy-subrc = 0 AND l_ausp-atwrt = 'Y' .
  IF sy-subrc = 0 AND l_ausp-atwrt = 'Y' .
*-----end
    " Work Order Summary Table Update.....
    SELECT SINGLE mituqty INTO l_mituqty
      FROM ztpp_wosum
     WHERE wo_ser = it_7jb-ordr
       AND nation = it_7jb-dist(3)
       AND dealer = it_7jb-dist+3(2)
       AND extc   = it_7jb-extc
       AND intc   = it_7jb-intc     .

    l_mituqty = l_mituqty - it_7jb-pqty  .
*    UPDATE ztpp_wosum   SET mituqty = l_mituqty
*                      WHERE wo_ser = it_7jb-ordr
*                        AND nation = it_7jb-dist(3)
*                        AND dealer = it_7jb-dist+3(2)
*                        AND extc   = it_7jb-extc
*                        AND intc   = it_7jb-intc     .

    " Vehicle Master Update..
    CONCATENATE  it_7jb-bmdl(3)  it_7jb-vhno  INTO  l_equnr .
    CLEAR: it_vmaster, it_vmaster[] .
    it_vmaster-atnam = 'P_SEQUENCE_DATE' .
    it_vmaster-atwrt = it_7jb-sqdt       .
    APPEND it_vmaster.
*add sequence serial & vm_date
*requested by hur,changed by wskim,on 2004.11.05
*-----start
    it_vmaster-atnam = 'P_SEQUENCE_SERIAL' .
    it_vmaster-atwrt = it_7jb-ssr1 .
    APPEND it_vmaster.
    DATA :lt_atwrt(10) TYPE n.
    CLEAR lt_atwrt.
    it_vmaster-atnam = 'P_VM_DATE' .
    CONCATENATE sy-datum sy-uzeit   INTO  lt_atwrt .
    it_vmaster-atwrt =  lt_atwrt.
    APPEND it_vmaster.
*-----end
    it_vmaster-atnam = 'P_MITU'          .
    it_vmaster-atwrt = ' '               .
    APPEND it_vmaster.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_equnr
              mode         = 'W'
         TABLES
              val_table    = it_vmaster
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    " Plan Order Number Search and Saving...
    SELECT SINGLE atinn INTO l_atinn
      FROM cabn
     WHERE atnam = 'P_PLAN_ORDER'.

    SELECT SINGLE atwrt INTO l_porder
      FROM ausp
     WHERE objek  = l_equnr
       AND atinn  = l_atinn .

    SELECT SINGLE atinn INTO l_atinn
      FROM cabn
     WHERE atnam = 'P_VIN'       .

    SELECT SINGLE atwrt INTO l_vin
      FROM ausp
     WHERE objek  = l_equnr
       AND atinn  = l_atinn .

    IF sy-subrc = 0.
*      UPDATE ztpp_pmt07jb_b    SET: plnum = l_porder
*                                    vinn  = l_vin
*                                    aedat = sy-datum
*                                    aezet = sy-uzeit
*                                    aenam = sy-uname
*                             WHERE sqdt  = it_7jb-sqdt
*                               AND plnt  = it_7jb-plnt
*                               AND line  = it_7jb-line
*                               AND modl  = it_7jb-modl
*                               AND mtgu  = it_7jb-mtgu
*                               AND ssr1  = it_7jb-ssr1
*                               AND ssr2  = it_7jb-ssr2  .
*
    ENDIF.
*requested by hur,changed by wskim,on 2004.11.05
*When Mitu vehicle is entered in BODY-IN Point
*-----start
*update planned order
*change  order_start_date & order_fin_date & plan_open_date for MRP
    PERFORM change_plannedorder USING it_7jb l_porder.
*Check : WORKORDER, EXT_COLOR,INT_COLOR
    PERFORM check_vehicle_info USING it_7jb l_equnr.
  ELSE.
*Workorder color mitu quty should be restorated
    CLEAR: wa_material,l_data,l_data[].
    CONCATENATE it_7jb-ordr it_7jb-dist   INTO wa_material .
    CONCATENATE wa_material it_7jb-extc  it_7jb-intc INTO wa_material.

    CLEAR: l_plnmg, l_data-atwrt, ausp.
    l_data-atnam = 'P_MITU_QTY'.    APPEND l_data.
*     l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    " MITU Qauntity is add  : Work color
    READ TABLE l_data INDEX 1.
    l_plnmg = l_data-atwrt   .
    l_plnmg =  l_plnmg + 1.
    PERFORM workorder_mituquty_restoration USING wa_material
                                                 l_plnmg.
*Workorder header mitu quty should be restorated
    CLEAR: wa_material,l_data,l_data[].
    CONCATENATE it_7jb-ordr it_7jb-dist   INTO wa_material .

    CLEAR: l_plnmg, l_data-atwrt, ausp.
    l_data-atnam = 'P_MITU_QTY'.    APPEND l_data.
*     l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    " MITU Qauntity is add  : Workorder header
    READ TABLE l_data INDEX 1.
    l_plnmg = l_data-atwrt   .
    l_plnmg =  l_plnmg + 1.
    PERFORM workorder_mituquty_restoration USING wa_material
                                                 l_plnmg.
  ENDIF.
*-----end
ENDFORM.                    " process_mitu_value

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_WORKORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_PLNMG  text
*----------------------------------------------------------------------*
FORM call_bdc_workorder USING    pa_material  pa_plnmg  pa_pqty
                                 pa_fqty      pa_mitu .
  DATA: l_vars                LIKE TABLE OF zspp_vin_value
                                                  WITH HEADER LINE,
        l_dec                 TYPE i,
        l_decp                TYPE p DECIMALS 0,
        l_dec1(6)             TYPE c,
        l_dec2(6)             TYPE c.

  l_dec1 = pa_plnmg.
  l_vars-atnam = 'P_SEQ_QTY' .     l_vars-atwrt = l_dec1. APPEND l_vars.
  CLEAR: l_vars              .     l_dec1 = pa_pqty     .
  l_vars-atnam = 'P_PLAN_QTY'.     l_vars-atwrt = l_dec1. APPEND l_vars.
  CLEAR: l_vars              .     l_dec1 = pa_fqty     .
  l_vars-atnam = 'P_FORECAST_QTY'. l_vars-atwrt = l_dec1. APPEND l_vars.
  CLEAR: l_vars              .     l_dec1 = pa_mitu     .
  l_vars-atnam = 'P_MITU_QTY' .    l_vars-atwrt = l_dec1. APPEND l_vars.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = l_vars
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

  IF sy-subrc NE 0.
    wa_error = 'X'.
  ENDIF.
ENDFORM.                    " CALL_BDC_WORKORDER

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
        l_flag              TYPE c           ,   " Roll-Back Flag..
        l_kwmeng            LIKE vbap-kwmeng .

  " Change the Sales Order
  l_val10 =  pa_val10 .
  l_val20 =  pa_val20 .
  l_val20 = l_val20 - l_val10.

  " Change the Coding for the Re-Processing (BDC --> BAPI)
  PERFORM call_bapi_salesorder USING pa_sorder l_val10 l_val20 .
ENDFORM.                    " CALL_BDC_SALES_ORDER

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
        l_mqty               LIKE ztpp_wosum-seqqty ,
        l_seqqty             LIKE ztpp_wosum-seqqty ,
        l_tseq               LIKE ztpp_wosum-seqqty ,
        l_sqty               LIKE ztpp_wosum-seqqty ,
        l_pqty               LIKE ztpp_wosum-seqqty ,
        l_fqty               LIKE ztpp_wosum-seqqty ,
        l_mituqty            LIKE ztpp_wosum-mituqty,
        l_mitu               LIKE ztpp_wosum-mituqty,
        l_data               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE.

  SORT it_7jb BY ordr dist extc intc ssr1.
  CLEAR: l_sqty  , l_mituqty.
  READ TABLE it_7jb INDEX 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  MOVE-CORRESPONDING it_7jb  TO   it_vin     .

  LOOP AT it_7jb .
    IF it_7jb-ordr = l_ordr  AND  it_7jb-dist = l_dist .
*      IF it_7jb-mtgu = space .
*        l_sqty      = l_sqty   + 1.
*      ELSE.
*        l_mituqty   = l_mituqty + 1 .
*      ENDIF.
*requested by hur,changed by wskim,on 2004.11.05
*logic add : Mitu quty
*-----Start
      IF it_7jb-mtgu <> space.
        l_mituqty   = l_mituqty + 1 .
      ENDIF.
*-----End
    ELSE.
      " Process.
      CLEAR: wa_material,  l_data, l_data[], l_plnmg, ausp.
      CONCATENATE l_ordr      l_dist      INTO wa_material .

      SELECT SUM( modqty )  SUM( seqqty ) SUM( mituqty )
        INTO (l_mqty, l_tseq, l_mitu)
        FROM ztpp_wosum
       WHERE wo_ser = l_ordr
         AND nation = l_dist(3)
         AND dealer = l_dist+3(2) .

      l_seqqty  = l_sqty    + l_tseq  .      " Total SEQ-Qty..

      " Work Order Header's SEQ  Qty Change......
      l_data-atnam = 'P_VIN_SPEC'.        APPEND l_data.
      l_data-atnam = 'P_MITU_QTY'.        APPEND l_data.
*      l_data-atnam = 'P_SEQ_QTY'.         APPEND l_data.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_material
                ctype        = '001'
           TABLES
                val_table    = l_data
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                error_value  = 4
                OTHERS       = 5.

*      READ TABLE l_data INDEX 3.
*      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
*      l_plnmg = l_sqty .                  " L_plnmg + l_seqqty.
      READ TABLE l_data INDEX 1.
      it_vin-vinn = l_data-atwrt.        CLEAR: l_data.
*     it_7JB-vinn = l_data-atwrt.        CLEAR: l_data.
      READ TABLE l_data INDEX 2.
      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
      l_plnmg  = l_plnmg - l_mituqty.

      PERFORM plan_quantity USING l_ordr   l_dist   ' '    ' '
                                  l_mqty   l_seqqty l_pqty l_fqty 'H' .

*      PERFORM call_bdc_workorder USING wa_material l_seqqty
*                                       l_pqty      l_fqty    l_plnmg .
*
      IF wa_error = 'X'.
        MESSAGE i003 WITH text-905 '#8 Workorder Header Update error'
                     wa_material.
** Changed on 07/19/2006 Requested by Hur
        CLEAR: wa_error.
*        EXIT.
** end of change
      ENDIF.
      APPEND it_vin .     CLEAR: it_vin.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      it_vin = it_7jb     .
*      IF it_7jb-mtgu = space .
*        l_sqty      = 1.
*      ELSE.
*        l_mituqty   = 1 .
*      ENDIF.
*requested by hur,changed by wskim,on 2004.11.05
*logic add : Mitu quty
*-----Start
      IF it_7jb-mtgu <> space.
        l_mituqty   = 1 .
      ENDIF.
*-----End

    ENDIF.
  ENDLOOP.

  IF wa_error = 'X'.  EXIT.  ENDIF.

  CHECK wa_error = space.

  IF wa_lines > 0 .
    CLEAR: wa_material, l_data, l_data[], l_plnmg, l_mqty, l_tseq.
    CONCATENATE l_ordr      l_dist      INTO wa_material .

    SELECT SUM( modqty )  SUM( seqqty ) SUM( mituqty )
      INTO (l_mqty, l_tseq, l_mitu)
      FROM ztpp_wosum
     WHERE wo_ser = l_ordr
       AND nation = l_dist(3)
       AND dealer = l_dist+3(2) .

    l_seqqty  = l_sqty    + l_tseq  .      " Total SEQ-Qty..

    " Work Order Header's SEQ  Qty Change......
    l_data-atnam = 'P_VIN_SPEC'.        APPEND l_data.
    l_data-atnam = 'P_MITU_QTY'.        APPEND l_data.
*      l_data-atnam = 'P_SEQ_QTY'.         APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

*      READ TABLE l_data INDEX 3.
*      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
*      l_plnmg = l_sqty .                  " L_plnmg + l_seqqty.
    READ TABLE l_data INDEX 1.
    it_vin-vinn = l_data-atwrt.        CLEAR: l_data.
    READ TABLE l_data INDEX 2.
    l_plnmg = l_data-atwrt   .         CLEAR: l_data.
    l_plnmg  = l_plnmg - l_mituqty.

    PERFORM plan_quantity USING l_ordr   l_dist   ' '    ' '
                                l_mqty   l_seqqty l_pqty l_fqty 'H' .

*    PERFORM call_bdc_workorder USING wa_material l_seqqty
*                                     l_pqty      l_fqty    l_plnmg .

    IF wa_error = 'X'.
      MESSAGE i002 WITH text-905 '#9 Workorder Header Update error'
wa_material.
      EXIT.
    ENDIF.
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
        l_wosum(20),
        l_seq                LIKE ztpp_wosum-seqqty ,
        l_seqqty             LIKE ztpp_wosum-seqqty ,
        l_tseq               LIKE ztpp_wosum-seqqty ,
        l_pqty               LIKE ztpp_wosum-seqqty ,
        l_fqty               LIKE ztpp_wosum-seqqty ,
        l_mituqty            LIKE ztpp_wosum-mituqty,
        l_data               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE.

  DATA: l_count              TYPE i.

  PERFORM write_timestamp  USING  text-010 .
  PERFORM write_head               .         " TO-BE DELETE LINE...

  SORT it_7jb BY ordr dist extc intc ssr1 ssr2.
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

      l_seqqty  = l_seq     + l_tseq  .      " Total SEQ-Qty..
      PERFORM plan_quantity USING l_ordr   l_dist  l_extc  l_intc
                         l_modqty l_seqqty l_pqty  l_fqty  'C' .
*      UPDATE ztpp_wosum  SET: seqqty  = l_seqqty
*                              planqty = l_pqty
*                          forecastqty = l_fqty
*                        WHERE wo_ser = l_ordr
*                          AND nation = l_dist(3)
*                          AND dealer = l_dist+3(2)
*                          AND extc   = l_extc
*                          AND intc   = l_intc     .

      CONCATENATE l_ordr l_dist l_extc l_intc INTO l_wosum.
      IF sy-subrc NE 0.
        wa_error = 'X'.
        MESSAGE i002 WITH text-905 '#1 Update error ZTPP_WOSUM' l_wosum.

        EXIT.
      ELSE.                                " TO-BE DELETE LINE.
        WRITE AT: /001(010) l_ordr ,       " TO-BE DELETE LINE.
                   012(005) l_dist ,       " TO-BE DELETE LINE.
                   018(003) l_extc ,       " TO-BE DELETE LINE.
                   022(003) l_intc ,       " TO-BE DELETE LINE.
                   026(004) sy-subrc,      " TO-BE DELETE LINE.
                   031(020) l_seqqty .     " TO-BE DELETE LINE.
      ENDIF.

      CLEAR: wa_material,  l_data, l_data[].
      CONCATENATE l_ordr       l_dist         INTO wa_material .
      CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

      CLEAR: l_plnmg, l_tseq, l_data-atwrt, ausp.
      l_data-atnam = 'P_MITU_QTY'.    APPEND l_data.
*     l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_material
                ctype        = '001'
           TABLES
                val_table    = l_data
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                error_value  = 4
                OTHERS       = 5.

*     READ TABLE l_data INDEX 2.
*     l_plnmg = l_data-atwrt   .
*     l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
      " MITU Qauntity is MUNIS.. => MITU Sequence reduced the MITU-Qty..
      READ TABLE l_data INDEX 1.
      l_plnmg = l_data-atwrt   .
      l_plnmg = l_plnmg - l_mituqty.

*      PERFORM call_bdc_workorder USING wa_material l_seqqty
*                                       l_pqty      l_fqty    l_plnmg.

      IF wa_error = 'X'.
        MESSAGE i002 WITH text-905  '#2 Workorder Color Update error'
wa_material.
        EXIT.
      ENDIF.
      WRITE AT: /001(018)  wa_material,     " TO-BE DELETE LINE.
                 020(005) 'WO-CL'     ,     " TO-BE DELETE LINE.
                 025(001) sy-vline    ,     " TO-BE DELETE LINE.
                 026(004) wa_error    ,     " TO-BE DELETE LINE.
                 030(001) sy-vline    ,     " TO-BE DELETE LINE.
                 031(020) l_seqqty    .     " TO-BE DELETE LINE.

      " Sales Order Master Change...
*      PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
*                                         l_seqqty     l_modqty  .
      IF wa_error = 'X'.
        MESSAGE i003 WITH text-905 '#3 Sales Order Update error'
                          l_salesorder wa_material.
        EXIT.
      ENDIF.
      WRITE AT: /001(017) l_salesorder,     " TO-BE DELETE LINE.
                 018(007) 'S-ORDER'   ,     " TO-BE DELETE LINE.
                 025(001) sy-vline    ,     " TO-BE DELETE LINE.
                 026(004) wa_error    ,     " TO-BE DELETE LINE.
                 030(001) sy-vline    ,     " TO-BE DELETE LINE.
                 031(020) l_seqqty    .     " TO-BE DELETE LINE.
      ULINE.                                " TO-BE DELETE LINE.
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

  IF wa_error = 'X'.   EXIT.  ENDIF.

  IF wa_lines > 0 .
    " Work Order Summary Table Update.....

    SELECT SINGLE modqty seqqty sales
            INTO (l_modqty, l_tseq, l_salesorder)
      FROM ztpp_wosum
     WHERE wo_ser = l_ordr            " it_7jb-ordr
       AND nation = l_dist(3)         " it_7jb-dist(3)
       AND dealer = l_dist+3(2)       " it_7jb-dist+3(2)
       AND extc   = l_extc            " it_7jb-extc
       AND intc   = l_intc     .      " it_7jb-intc     .

    l_seqqty  = l_seq     + l_tseq  .
    PERFORM plan_quantity USING l_ordr   l_dist   l_extc l_intc
                                l_modqty l_seqqty l_pqty l_fqty 'C' .
*    UPDATE ztpp_wosum  SET: seqqty  = l_seqqty
*                            planqty = l_pqty
*                        forecastqty = l_fqty
*                      WHERE wo_ser  = l_ordr          " it_7jb-ordr
*                        AND nation  = l_dist(3)       " it_7jb-dist(3)
*                        AND dealer  = l_dist+3(2)     "
*it_7jb-dist+3(2)
*                        AND extc    = l_extc          " it_7jb-extc
*                        AND intc    = l_intc     .    " it_7jb-intc  .

    CLEAR: wa_material,  l_data, l_data[].
    CONCATENATE l_ordr       l_dist         INTO wa_material .
    CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

    IF sy-subrc NE 0.

      " Step 1: Update Fail - Reason: Data not found!!!
      "         Check the Working data.
      wa_error = 'X'.
      MESSAGE i002 WITH text-905  '#4 Update error ZTPP_WOSUM'
wa_material.
      EXIT.
    ELSE.                                  " TO-BE DELETE LINE.

      WRITE AT: /001(010) l_ordr ,       " TO-BE DELETE LINE.
                 012(005) l_dist ,       " TO-BE DELETE LINE.
                 018(003) l_extc ,       " TO-BE DELETE LINE.
                 022(003) l_intc ,       " TO-BE DELETE LINE.
                 026(004) sy-subrc,      " TO-BE DELETE LINE.
                 031(020) l_seqqty .     " TO-BE DELETE LINE.
    ENDIF.


    CLEAR: l_plnmg, l_tseq, l_data-atwrt, ausp.
    l_data-atnam = 'P_MITU_QTY'.    APPEND l_data.
*   l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

*   READ TABLE l_data INDEX 2.
*   l_plnmg = l_data-atwrt   .
*   l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
    READ TABLE l_data INDEX 1.
    l_plnmg = l_data-atwrt   .
    l_plnmg = l_plnmg - l_mituqty.    CLEAR: l_data.

*    PERFORM call_bdc_workorder USING wa_material l_seqqty
*                                     l_pqty      l_fqty    l_plnmg .

    IF wa_error = 'X'.
      MESSAGE i002 WITH text-905  '#5 Workorder Color Update error'
       wa_material.
      EXIT.
    ENDIF.
    WRITE AT: /001(018)  wa_material,     " TO-BE DELETE LINE.
               020(005) 'WO-CL'     ,     " TO-BE DELETE LINE.
               025(001) sy-vline    ,     " TO-BE DELETE LINE.
               026(004) wa_error    ,     " TO-BE DELETE LINE.
               030(001) sy-vline    ,     " TO-BE DELETE LINE.
               031(020) l_seqqty    .     " TO-BE DELETE LINE.

    " Sales Order Master Change...
*    PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
*                                       l_seqqty     l_modqty  .
    IF wa_error = 'X'.
      MESSAGE i002 WITH text-905 '#6 Sales Order Update error'
              l_salesorder.
      EXIT.
    ENDIF.
    WRITE AT: /001(017) l_salesorder,     " TO-BE DELETE LINE.
                   018(007) 'S-ORDER'   ,     " TO-BE DELETE LINE.
                   025(001) sy-vline    ,     " TO-BE DELETE LINE.
                   026(004) wa_error    ,     " TO-BE DELETE LINE.
                   030(001) sy-vline    ,     " TO-BE DELETE LINE.
                   031(020) l_seqqty    .     " TO-BE DELETE LINE.
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
        l_plnqty             LIKE ztpp_wosum-planqty,
        l_chkqty             TYPE i,
        l_count              TYPE i,
        l_chkplnqty          TYPE i,
        l_wosum(20),
        l_data               LIKE TABLE OF conf_out    WITH HEADER LINE.

  SORT it_7jb BY ordr dist extc intc ssr1.
  CLEAR: l_seqqty, l_chkplnqty .
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
*      IF l_chkqty < l_count .
*        wa_err_hd = wa_flag  = 'E'    .
*        WRITE AT: /001(20)  text-902,
*                   021(09)  l_ordr ,
*                   031(05)  l_dist ,
*                   037(03)  l_extc ,
*                   041(03)  l_intc .
*        ULINE AT: /(60)            .
*      ENDIF.
      " Check Data for the Plan Quantity...
      SELECT SUM( pqty ) INTO l_plnqty
        FROM ztpp_pmt07jb_a
       WHERE ordr = l_ordr
         AND dist = l_dist
         AND extc = l_extc
         AND intc = l_intc
         AND gubb = 'A'    .

      l_chkplnqty  = l_count + l_plnqty.

*      IF l_chkqty < l_chkplnqty ."l_plnqty.
*        wa_err_hd = wa_flag  = 'E'    .
*        CONCATENATE l_ordr l_dist l_extc l_intc INTO l_wosum.
*        MESSAGE i001 WITH text-904 l_wosum.
*        WRITE AT: /001(20)  text-904,
*                   021(09)  l_ordr ,
*                   031(05)  l_dist ,
*                   037(03)  l_extc ,
*                   041(03)  l_intc ,
*                   045(10)  l_chkqty,
*                   056(3)   '< (',
*                   060(10)  l_count ,
*                   071(1)   '+',
*                   073(10)  l_plnqty,
*                   084(1)   ')'.
*        ULINE AT: /(85)            .
*      ENDIF.
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
*&      Form  update_commonvlas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_commonvlas.
  c_prog = 'ZIPP101U_PMT07JB_A'.
  SELECT MAX( sqdt ) MIN( sqdt ) INTO (wa_maxday, wa_minday)
    FROM ztpp_pmt07jb_b .

  UPDATE ztpp_common_vals SET: dates = wa_minday
                               item1 = wa_maxday
                        WHERE jobs  = c_prog
                          AND key2  = it_7jb-modl.
ENDFORM.                    " update_commonvlas

*&---------------------------------------------------------------------*
*&      Form  job_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_create   USING pa_type.
  DATA: l_7jb     LIKE ztpp_pmt07jb_b   .

  CLEAR: l_7jb.
  MOVE-CORRESPONDING wa_7jb TO l_7jb .

  DO .
    CASE pa_type.
      WHEN '1'  .
        CALL FUNCTION 'Z_FPP_VEHICLE_CREATION'
            STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
             PERFORMING return_step1 ON END OF TASK
             EXPORTING
               p_7jb                       = l_7jb
               p_date                      = sy-datum
             EXCEPTIONS
               communication_failure       = 1
               system_failure              = 2
               RESOURCE_FAILURE            = 3.
*         OTHERS                      = 4.
      WHEN '2' .
        CALL FUNCTION 'Z_FPP_PLANORDER_CREATION'
            STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
             PERFORMING return_step2 ON END OF TASK
             EXPORTING
               p_7jb                       = l_7jb
               p_date                      = sy-datum
             EXCEPTIONS
               communication_failure       = 1
               system_failure              = 2
               RESOURCE_FAILURE            = 3.
*         OTHERS                      = 4.
    ENDCASE.

    CASE sy-subrc.
      WHEN 0.
        wa_taskname = wa_taskname  + 1.
        wa_snd_jobs = wa_snd_jobs  + 1.
        CLEAR: wa_excp_flag .
        EXIT.
      WHEN 1 OR 2.
        wa_excp_flag = 'X'.
      WHEN 3.
*Receive reply to asynchronous RFC calls
        IF wa_excp_flag = space.
          wa_excp_flag = 'X'.
*First attempt for RESOURCE_Failure handling
          WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs UP TO '0.01' SECONDS.
        ELSE.
*Second attempt for RESOURCE_Failure handling
          WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs UP TO '0.1' SECONDS.
        ENDIF.
        IF sy-subrc = 0.
          CLEAR wa_excp_flag. " Reset flag
*        ELSE.
*          EXIT.
        ENDIF.
    ENDCASE.
  ENDDO.
ENDFORM.                    " job_create

*&---------------------------------------------------------------------*
*&      Form  check_vin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vin.
  CLEAR: it_vin.
  wa_7jb = it_7jb.
  READ TABLE it_vin WITH KEY ordr = wa_7jb-ordr
                             dist = wa_7jb-dist .
ENDFORM.                    " check_vin

*&---------------------------------------------------------------------*
*&      Form  WRITE_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_011  text
*----------------------------------------------------------------------*
FORM write_timestamp USING    pa_text.
  GET TIME.
  WRITE AT: /001(030)  pa_text,
             031(015)  sy-datum,
             047(015)  sy-uzeit.
ENDFORM.                    " WRITE_TIMESTAMP

*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_result.
  DATA: l_lines             TYPE i.

  DESCRIBE TABLE it_7jb     LINES l_lines.
  WRITE AT:/001(020) text-015 ,
            022(010) l_lines  .
ENDFORM.                    " WRITE_RESULT

*&---------------------------------------------------------------------*
*&      Form  VIN_CODE_GEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vin_code_gen.
  DATA: l_vin                LIKE mara-matnr ,
        l_mode               LIKE ztpp_common_vals-key2.

  " Call the function for the Last Data...
* l_mode = 'EMF'.      " wa_7jb-modl .
  l_mode = it_7jb-modl .
  l_vin = it_vin-vinn  .

  CALL FUNCTION 'Z_FPP_VIN_GENERATION'
       EXPORTING
            w_order   = l_vin
            mode      = l_mode
       IMPORTING
            p_lastid  = l_vin
       EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

  IF sy-subrc <> 0.
    wa_error = 'X'.
    MESSAGE i002 WITH text-905 '#12 Error at VIN GENERATION ' l_vin.

    WRITE AT: /001(025) 'Error of VIN-Generation: ',
               027(003) sy-subrc                   ,
               031(004) l_mode                     .
    EXIT.
  ENDIF.

  it_7jb-vinn = wa_7jb-vinn = l_vin     .

  " Update the ZTPP_PMT07JB_B..
  UPDATE ztpp_pmt07jb_b   SET  vinn  = l_vin
                        WHERE sqdt  = wa_7jb-sqdt
                          AND modl  = wa_7jb-modl
                          AND mtgu  = wa_7jb-mtgu
                          AND ssr1  = wa_7jb-ssr1
                          AND ssr2  = wa_7jb-ssr2 .

  IF sy-subrc NE 0  OR l_vin = space .
    "Error Data
    wa_error = 'X' .
    MESSAGE i002 WITH text-905 '#13 Error Update ZTPP_PMT07JB_B' l_vin.
  ENDIF.
ENDFORM.                    " VIN_CODE_GEN

*&---------------------------------------------------------------------*
*&      Form  select_vin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_vin.
  SELECT SINGLE * INTO wa_7jb
    FROM ztpp_pmt07jb_b
   WHERE sqdt  = it_7jb-sqdt
     AND modl  = it_7jb-modl
     AND mtgu  = it_7jb-mtgu
     AND ssr1  = it_7jb-ssr1 .
ENDFORM.                    " select_vin

*&---------------------------------------------------------------------*
*&      Form  clear_vinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_vinn.
  DATA: l_porder            LIKE ztpp_pmt07jb_b-plnum,
        l_vinn              LIKE ztpp_pmt07jb_b-vinn.

  CLEAR: l_vinn, l_porder.
  UPDATE ztpp_pmt07jb_b   SET vinn  = l_vinn
                        WHERE vinn NE l_vinn .
  COMMIT WORK.

  UPDATE ztpp_pmt07jb_b   SET plnum = l_porder
                        WHERE plnum NE l_porder.
  COMMIT WORK.

  MESSAGE i001 WITH text-905 '#11 Update ZTPP_WOSUM In clear vinn'.

ENDFORM.                    " clear_vinn

*&---------------------------------------------------------------------*
*&      Form  check_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_result.
  " Check the result of the Plan Order & Vehicle Master.
ENDFORM.                    " check_result

*&---------------------------------------------------------------------*
*&      Form  call_bapi_salesorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_SORDER  text
*      -->P_L_VAL10  text
*      -->P_L_VAL20  text
*----------------------------------------------------------------------*
FORM call_bapi_salesorder USING    pa_order  pa_val10  pa_val20.
  DATA : l_it_ord_header_inx LIKE TABLE OF bapisdh1x  WITH HEADER LINE,
         l_bapisdls          LIKE bapisdls ,
         l_it_return         LIKE TABLE OF bapiret2   WITH HEADER LINE,
         l_it_itm            LIKE TABLE OF bapisditm  WITH HEADER LINE,
         l_it_itmx           LIKE TABLE OF bapisditmx WITH HEADER LINE,
         l_it_lines          LIKE TABLE OF bapischdl  WITH HEADER LINE,
         l_it_linesx         LIKE TABLE OF bapischdlx WITH HEADER LINE.

  DATA : p_item10_org         LIKE vbap-kwmeng,
         p_item20_org         LIKE vbap-kwmeng,
         p_item20_qty         LIKE ztpp_wosum-modqty,
         p_item10_qty         LIKE ztpp_wosum-seqqty,
         l_item10_flg(01),
         l_item20_flg(01),
         l_item10_qty_flg(01),
         l_item20_qty_flg(01).

  p_item10_qty = pa_val10 .
  p_item20_qty = pa_val20 .

  SELECT SINGLE kwmeng INTO p_item10_org
    FROM vbap
   WHERE vbeln = pa_order
     AND posnr = '000010' .

  SELECT SINGLE kwmeng INTO p_item20_org
    FROM vbap
   WHERE vbeln = pa_order
     AND posnr = '000020' .

  " Check Logic..
  "   1) p_item10_org =  0,     " Seq. Qty before sequence..
  "      l_item10_flg --> 'I'   l_item10_qty_flg --> 'I'
  "      l_item20_flg --> 'I'   l_item20_qty_flg --> 'I'
  "   2) p_item10_QTY =  0,     " Seq. Qty after sequence..
  "      l_item10_flg --> 'D'   l_item10_qty_flg --> 'D'
  "      l_item20_flg --> 'D'   l_item20_qty_flg --> 'D'
  "   3) OTHERS.
  "      l_item10_flg --> 'U'   l_item10_qty_flg --> 'U'
  "      l_item20_flg --> 'U'   l_item20_qty_flg --> 'U'

  IF p_item10_org =  0 AND p_item10_qty =  0.
    l_it_return-type = 'A'   .
    wa_error = 'X'           .
    EXIT.
  ELSE.
    IF p_item10_org =  0     .
      l_item10_flg     = 'I'   .
      l_item10_qty_flg = 'I'   .
      IF p_item20_qty = 0   .
        l_item20_flg     = 'U'   .
        l_item20_qty_flg = 'D'   .
      ELSE.
        l_item20_flg     = 'U'   .
        l_item20_qty_flg = 'U'   .
      ENDIF.
    ELSE.
      l_item10_flg     = 'U'   .
      l_item10_qty_flg = 'U'   .
      IF p_item20_qty = 0   .
        l_item20_flg     = 'U'   .
        l_item20_qty_flg = 'D'   .
      ELSE.
        l_item20_flg     = 'U'   .
        l_item20_qty_flg = 'U'   .
      ENDIF.
    ENDIF.
  ENDIF.

  l_bapisdls-scheduling = 'X'.

  l_it_ord_header_inx-updateflag = 'U'.
  APPEND l_it_ord_header_inx.

  l_it_itm-itm_number = '000010'.
  APPEND l_it_itm.
  l_it_itm-itm_number = '000020'.
  APPEND l_it_itm.

  l_it_itmx-updateflag = l_item10_flg.
  l_it_itmx-itm_number = '000010'.
  APPEND l_it_itmx.
  l_it_itmx-updateflag = l_item20_flg.
  l_it_itmx-itm_number = '000020'.
  APPEND l_it_itmx.

  p_item10_org = pa_val10 .
  p_item20_org = pa_val20 .

  l_it_lines-itm_number = '000010'.
  IF l_item10_qty_flg = 'I'       .
    l_it_lines-sched_line = '0001'.
  ELSE.
    SELECT SINGLE etenr INTO l_it_lines-sched_line
      FROM vbep
     WHERE vbeln = pa_order
       AND posnr = l_it_lines-itm_number .
  ENDIF.
  l_it_lines-req_qty = p_item10_org.
  APPEND l_it_lines.

  l_it_linesx-updateflag = l_item10_qty_flg.
  l_it_linesx-itm_number = '000010'.
  l_it_linesx-sched_line = l_it_lines-sched_line .
  l_it_linesx-req_qty = 'X'.
  APPEND l_it_linesx.

  l_it_lines-itm_number = '000020'.
  IF l_item20_qty_flg = 'I'       .
    l_it_lines-sched_line = '0001'.
  ELSE.
    SELECT SINGLE etenr INTO l_it_lines-sched_line
      FROM vbep
     WHERE vbeln = pa_order
       AND posnr = l_it_lines-itm_number .
  ENDIF.
  l_it_lines-req_qty = p_item20_org.
  APPEND l_it_lines.

  l_it_linesx-updateflag = l_item20_qty_flg.
  l_it_linesx-itm_number = '000020'.
  l_it_linesx-sched_line = l_it_lines-sched_line .
  l_it_linesx-req_qty = 'X'.
  APPEND l_it_linesx.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
       EXPORTING
            salesdocument    = pa_order
            order_header_inx = l_it_ord_header_inx
            logic_switch     = l_bapisdls
       TABLES
            return           = l_it_return
            order_item_in    = l_it_itm
            order_item_inx   = l_it_itmx
            schedule_lines   = l_it_lines
            schedule_linesx  = l_it_linesx.

  LOOP AT l_it_return .   " WHERE type = 'E' OR type = 'A' .
    WRITE: / pa_order, ':', l_it_return-type, l_it_return-message .
    IF l_it_return-type = 'E' OR
       l_it_return-type = 'A'   .
      wa_error = 'X'           .
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " call_bapi_salesorder

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0346   text
*----------------------------------------------------------------------*
FORM create_log USING    pa_step  pa_data LIKE it_7jb .
  DATA: l_log                LIKE ztpp_rep_seq .

  CLEAR: l_log.
  SELECT MAX( sequence ) INTO l_log-sequence
    FROM ztpp_rep_seq
   WHERE wk_date  = wa_date .

  l_log-wk_date   = wa_date            .
  l_log-sequence  = l_log-sequence + 1 .
  l_log-step      = pa_step            .
  l_log-status    = 'E'                .
  l_log-logtype   = 'E'                .

  CASE pa_step.
    WHEN '1'  .
    WHEN '2'  .
    WHEN '3'  .
      MOVE-CORRESPONDING pa_data  TO l_log .
      l_log-msg = it_7jb-vinn              .
    WHEN '4'  .
      MOVE-CORRESPONDING pa_data  TO l_log .
    WHEN '5'  .
      MOVE-CORRESPONDING pa_data  TO l_log .
  ENDCASE.
  INSERT INTO ztpp_rep_seq VALUES l_log    .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  return_STEP1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_step1  USING p_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_VEHICLE_CREATION'
         EXCEPTIONS
         communication_failure       = 1
         system_failure              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.
ENDFORM.                    " return_STEP1

*&---------------------------------------------------------------------*
*&      Form  return_STEP2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_step2  USING p_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_CREATION'
         EXCEPTIONS
         communication_failure       = 1
         system_failure              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.
ENDFORM.                    " return_STEP2

*&---------------------------------------------------------------------*
*&      Form  CREATE_PCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_pcc USING pa_matnr  pa_werks  pa_text  pa_verid  pa_flag .
  DATA: l_matnr  LIKE bdcdata-fval,
        l_werks  LIKE bdcdata-fval,
        l_ktext  LIKE bdcdata-fval,
        l_verid  LIKE bdcdata-fval,
        l_flag   LIKE bdcdata-fval.

  l_matnr = pa_matnr.   l_werks = pa_werks.
  l_ktext = pa_text .   l_verid = pa_verid.

  CLEAR: wa_subrc, it_msg, it_msg[].
  CALL FUNCTION 'Z_FCO_PCC_ORDER_CRE_WITH_PDV'
       EXPORTING
            matnr_001 = l_matnr
            werks_002 = l_werks
            ktext_004 = l_ktext
            verid_007 = l_verid
            p_first   = pa_flag
       IMPORTING
            subrc     = wa_subrc
       TABLES
            messtab   = it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0.
    LOOP AT it_msg WHERE msgtyp = 'E'.
      MESSAGE i002 WITH text-905 '#18 PCC UPDATE ERROR' l_matnr.
      WRITE: / it_msg                .
      EXIT.
    ENDLOOP.
  ELSE.
    WRITE: / 'Success: ', l_ktext .
  ENDIF.
ENDFORM.                    " CREATE_PCC

*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_ROUTINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_pcc_routine.
  DATA: lt_7jb               LIKE TABLE OF it_7jb      WITH HEADER LINE,
        l_matnr              LIKE mara-matnr,
        l_verid              LIKE mkal-verid ,
        l_text               LIKE makt-maktx ,
        l_flag               TYPE c          .

  lt_7jb[] = it_7jb[]       .
  LOOP AT lt_7jb.
    CONCATENATE lt_7jb-moye  lt_7jb-dist lt_7jb-bmdl INTO lt_7jb-matnr .
    CONCATENATE lt_7jb-matnr lt_7jb-ocnn             INTO lt_7jb-matnr
      SEPARATED BY space.
    MODIFY lt_7jb .
  ENDLOOP.

  SORT it_7jb BY matnr vers .
  DELETE ADJACENT DUPLICATES FROM lt_7jb COMPARING matnr vers .

  LOOP AT lt_7jb            .
    " Function Call for the PCC check...
    CLEAR: l_flag.
    l_verid = lt_7jb-vers+1(2) .
    PERFORM check_pcc_function   USING  lt_7jb-matnr   l_verid  l_flag .
    CHECK l_flag = space OR l_flag = 'X' .
    CLEAR: l_text .
    CONCATENATE lt_7jb-matnr l_verid                 INTO l_text .
    PERFORM create_pcc USING lt_7jb-matnr   'P001'
                             l_text   l_verid  l_flag .
  ENDLOOP.
ENDFORM.                    " CHECK_PCC_ROUTINE

*&---------------------------------------------------------------------*
*&      Form  CHECK_PCC_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB_MATNR  text
*----------------------------------------------------------------------*
FORM check_pcc_function USING    pa_matnr  pa_verid  pa_flag .
  DATA: lp_procnr        LIKE aufk-procnr,
        lp_verid         LIKE afpo-verid ,
        lp_stlan         LIKE mkal-stlan ,
        lp_stlal         LIKE mkal-stlal ,
        lp_plnty         LIKE plko-plnty ,
        lp_plnnr         LIKE plko-plnnr ,
        lp_plnal         LIKE plko-plnal ,
        lp_aufnr         LIKE aufk-aufnr ,
        lw_keko          LIKE keko       ,
        lt_afko          LIKE TABLE OF afko            WITH HEADER LINE,
        lt_vkks0         LIKE TABLE OF vkks0           WITH HEADER LINE,
        lt_pkosa         LIKE TABLE OF pkosa           WITH HEADER LINE.

  pa_flag = 'X'.
  CALL FUNCTION 'KK_F_PKOSA_FIND'
       EXPORTING
            i_matnr               = pa_matnr
            i_werks               = 'P001'
            i_pwerk               = 'P001'
            i_verid               = pa_verid
       IMPORTING
            e_procnr              = lp_procnr
            e_verid               = lp_verid
            e_stlan               = lp_stlan
            e_stlal               = lp_stlal
            e_plnty               = lp_plnty
            e_plnnr               = lp_plnnr
            e_plnal               = lp_plnal
            e_aufnr               = lp_aufnr
       TABLES
            e_vkks0               = lt_vkks0
            e_pkosa               = lt_pkosa
       EXCEPTIONS
            none_found            = 1
            wrong_input           = 2
            none_picked           = 3
            wrong_rule            = 4
            rsh_not_valid         = 5
            wrong_characteristics = 6
            no_rule               = 7
            version_not_valid     = 8
            OTHERS                = 9.

  CASE sy-subrc  .
    WHEN 0.
      pa_flag = 'S' .     " Can not call the PCC Function..
    WHEN 1.
      " Check the Standard Costing Estimate REsult...
      " If the Data is make without error... Continue...
      " Else Error...
      SELECT SINGLE * INTO lw_keko
        FROM keko
       WHERE matnr = pa_matnr
         AND tvers = '01'
         AND werks = 'P001'
         AND kokrs = 'H201'
         AND feh_sta = 'FR'
         AND klvar = 'PPC1'
         AND kadat <= sy-datum
         AND bidat >= sy-datum .

      IF sy-subrc NE 0.
        " Display the Message...
        WRITE: / pa_matnr, '(', pa_verid, ')',  text-016.
        pa_flag = 'S'.  EXIT.
      ENDIF.
    WHEN 8.
      pa_flag = 'E' .     " Error - Un-Respected Scenario.
    WHEN OTHERS.
      pa_flag = 'E' .     " Error - Un-Respected Scenario.
  ENDCASE.
ENDFORM.                    " CHECK_PCC_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  PLAN_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ORDR  text
*      -->P_L_DIST  text
*      -->P_L_EXTC  text
*      -->P_L_INTC  text
*      -->P_L_SEQQTY  text
*      -->P_L_PQTY  text
*      -->P_L_FQTY  text
*      -->P_1528   text
*----------------------------------------------------------------------*
FORM plan_quantity USING    pa_ordr    pa_dist  pa_extc  pa_intc
                   pa_mqty  pa_sqty    pa_pqty  pa_fqty  pa_type .
  DATA: l_qty      TYPE i .

  IF pa_type = 'C' .
    SELECT SUM( pqty ) INTO  l_qty
      FROM ztpp_pmt07jb_a
     WHERE ordr = pa_ordr
       AND dist = pa_dist
       AND extc = pa_extc
       AND intc = pa_intc
       AND gubb = 'A'    .
*      AND gub1 = '1'    .

    pa_pqty = l_qty      .
    pa_fqty = pa_mqty - pa_sqty - l_qty .
    IF pa_fqty LT 0.
      pa_fqty = 0.
    ENDIF.
  ELSE.
    SELECT SUM( pqty ) INTO  l_qty
      FROM ztpp_pmt07jb_a
     WHERE ordr = pa_ordr
       AND dist = pa_dist
       AND gubb = 'A'    .
*      AND gub1 = '1'    .

    pa_pqty = l_qty      .
    pa_fqty = pa_mqty - pa_sqty - l_qty .
  ENDIF.
ENDFORM.                    " PLAN_QUANTITY

*&---------------------------------------------------------------------*
*&      Form  WRITE_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_head.                          " TO-BE DELETE LINE.
  WRITE AT: /001(010) 'WO_SER' ,          " TO-BE DELETE LINE.
             011(001) sy-vline ,          " TO-BE DELETE LINE.
             012(005) 'DEST'   ,          " TO-BE DELETE LINE.
             017(001) sy-vline ,          " TO-BE DELETE LINE.
             018(003) 'EXT'    ,          " TO-BE DELETE LINE.
             021(001) sy-vline ,          " TO-BE DELETE LINE.
             022(003) 'INT'    ,          " TO-BE DELETE LINE.
             025(001) sy-vline ,          " TO-BE DELETE LINE.
             026(004) 'RTC'    ,          " TO-BE DELETE LINE.
             030(001) sy-vline ,          " TO-BE DELETE LINE.
             031(020) 'SEQ_QTY'.          " TO-BE DELETE LINE.
  ULINE .                                 " TO-BE DELETE LINE.
  SKIP 2.                                 " TO-BE DELETE LINE.
  ULINE .                                 " TO-BE DELETE LINE.
ENDFORM.                    " WRITE_HEAD

*&---------------------------------------------------------------------*
*&      Form  CALL_PLAN_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_plan_save.
  DATA: lt_7jb               LIKE TABLE OF it_7jb      WITH HEADER LINE,
        l_matnr              LIKE mara-matnr,
        l_verid              LIKE mkal-verid ,
        l_text               LIKE makt-maktx ,
        l_flag               TYPE c          .

  " Save the Daily Plan..
  lt_7jb[] = it_7jb[]  .
  DELETE ADJACENT DUPLICATES FROM lt_7jb COMPARING sqdt.

  LOOP AT lt_7jb       .
    DELETE FROM ztpp_day_plan WHERE sqdt = lt_7jb-sqdt .
  ENDLOOP.

  MODIFY ztpp_day_plan FROM TABLE it_7jb .

  MESSAGE i001 WITH text-905 '#19 Successful Processing'.

ENDFORM.                    " CALL_PLAN_SAVE

*&---------------------------------------------------------------------*
*&      Form  BACK_UP_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM back_up_data1 .
  DATA: l_modqty             LIKE ztpp_wosum-modqty ,
        l_salesorder         LIKE ztpp_wosum-sales,
        l_ordr               LIKE it_7jb-ordr,
        l_dist               LIKE it_7jb-dist,
        l_extc               LIKE it_7jb-extc,
        l_intc               LIKE it_7jb-intc,
        l_qty                TYPE p DECIMALS 0,
        l_seq                LIKE ztpp_wosum-seqqty ,
        l_seqqty             LIKE ztpp_wosum-seqqty ,
        l_tseq               LIKE ztpp_wosum-seqqty ,
        l_pqty               LIKE ztpp_wosum-seqqty ,
        l_fqty               LIKE ztpp_wosum-seqqty ,
        l_mituqty            LIKE ztpp_wosum-mituqty,
        l_data               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE.

  " Processing Process:  1. Sales Order's Quantity
  "                      2. Work Order Header's SEQ/PLAN/FORECASR/MITU
  "                      3. Work Order Color's SEQ/PLAN/FORECASR/MITU
  "                      4. Work Order Summary's SEQ/PLAN/FORECASR/MITU
  DELETE FROM ztpp_seq_backup CLIENT SPECIFIED WHERE mandt = sy-mandt.
  SORT it_7jb BY ordr dist extc intc ssr1.
  CLEAR: l_seqqty, l_mituqty, l_seq .
  READ TABLE it_7jb INDEX 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  l_extc = it_7jb-extc.  l_intc = it_7jb-intc.

  LOOP AT it_7jb .
    IF it_7jb-ordr = l_ordr  AND  it_7jb-dist = l_dist AND
       it_7jb-extc = l_extc  AND  it_7jb-intc = l_intc .
      CONTINUE.
    ELSE.
      CLEAR: ztpp_seq_backup.
** changed by furong on 10/31/05
      SELECT SINGLE modqty seqqty planqty forecastqty sales mituqty
        INTO (l_modqty, l_seqqty, l_pqty, l_fqty, l_salesorder,
              l_mituqty)
        FROM ztpp_wosum
       WHERE wo_ser = l_ordr
         AND nation = l_dist(3)
         AND dealer = l_dist+3(2)
         AND extc   = l_extc
         AND intc   = l_intc     .

*      SELECT SINGLE modqty seqqty planqty forecastqty sales mituqty
*        INTO (l_modqty, l_tseq, l_pqty, l_fqty, l_salesorder,
*              l_mituqty)
*        FROM ztpp_wosum
*       WHERE wo_ser = l_ordr
*         AND nation = l_dist(3)
*         AND dealer = l_dist+3(2)
*         AND extc   = l_extc
*         AND intc   = l_intc     .
*
** end of change
      CLEAR: wa_material,  l_data, l_data[].
      CONCATENATE l_ordr       l_dist         INTO wa_material .
      CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

      ztpp_seq_backup-tables = 'ZTPP_WOSUM'      .
      ztpp_seq_backup-worder = wa_material       .
      ztpp_seq_backup-fname  = 'SEQQTY'          .
      ztpp_seq_backup-vals   = l_qty = l_seqqty  .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'PLANQTY'         .
      ztpp_seq_backup-vals   = l_qty = l_pqty    .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'FORECASTQTY'     .
      ztpp_seq_backup-vals   = l_qty = l_fqty    .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'MITUQTY'         .
      ztpp_seq_backup-vals   = l_qty = l_mituqty .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.

      CLEAR: l_tseq, l_data-atwrt, ausp, ztpp_seq_backup.
      l_data-atnam = 'P_SEQ_QTY'.        APPEND l_data.
      l_data-atnam = 'P_PLAN_QTY'.       APPEND l_data.
      l_data-atnam = 'P_FORECAST_QTY'.   APPEND l_data.
      l_data-atnam = 'P_MITU_QTY'.       APPEND l_data.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_material
                ctype        = '001'
           TABLES
                val_table    = l_data
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                error_value  = 4
                OTHERS       = 5.

      READ TABLE l_data INDEX 1.
      l_seqqty   = l_data-atwrt   .        CLEAR: l_data.
      READ TABLE l_data INDEX 2.
      l_pqty     = l_data-atwrt   .        CLEAR: l_data.
      READ TABLE l_data INDEX 3.
      l_fqty     = l_data-atwrt   .        CLEAR: l_data.
      READ TABLE l_data INDEX 4.
      l_mituqty  = l_data-atwrt   .        CLEAR: l_data.

      ztpp_seq_backup-tables = 'WOCL'          .
      ztpp_seq_backup-worder = wa_material     .
      ztpp_seq_backup-fname  = 'P_SEQ_QTY'       .
      ztpp_seq_backup-vals   = l_qty = l_seqqty  .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'P_PLAN_QTY'      .
      ztpp_seq_backup-vals   = l_qty = l_pqty    .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'P_FORECAST_QTY'  .
      ztpp_seq_backup-vals   = l_qty = l_fqty    .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'P_MITU_QTY'      .
      ztpp_seq_backup-vals   = l_qty = l_mituqty .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.

      " Sales Order Master Change...
      CLEAR: ztpp_seq_backup, l_qty.
      ztpp_seq_backup-tables = 'VBAP'          .
      ztpp_seq_backup-worder = l_salesorder    .
      SELECT SINGLE kwmeng INTO l_qty
        FROM vbap
       WHERE vbeln = l_salesorder
         AND posnr = '000010' .

      ztpp_seq_backup-fname  = '000010'        .
      ztpp_seq_backup-vals   = l_qty           .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.   CLEAR: l_qty.

      SELECT SINGLE kwmeng INTO l_qty
        FROM vbap
       WHERE vbeln = l_salesorder
         AND posnr = '000020' .

      ztpp_seq_backup-fname  = '000020'        .
      ztpp_seq_backup-vals   = l_qty           .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.   CLEAR: l_qty.

      CLEAR: l_seq, l_mituqty.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      l_extc = it_7jb-extc.  l_intc = it_7jb-intc.
    ENDIF.
  ENDLOOP.

  IF wa_lines > 0 .
    CLEAR: ztpp_seq_backup.
    SELECT SINGLE modqty seqqty planqty forecastqty sales mituqty
      INTO (l_modqty, l_seqqty, l_pqty, l_fqty, l_salesorder, l_mituqty)
      FROM ztpp_wosum
     WHERE wo_ser = l_ordr
       AND nation = l_dist(3)
       AND dealer = l_dist+3(2)
       AND extc   = l_extc
       AND intc   = l_intc     .

    CLEAR: wa_material,  l_data, l_data[].
    CONCATENATE l_ordr       l_dist         INTO wa_material .
    CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

    ztpp_seq_backup-tables = 'ZTPP_WOSUM'      .
    ztpp_seq_backup-worder = wa_material       .
    ztpp_seq_backup-fname  = 'SEQQTY'          .
    ztpp_seq_backup-vals   = l_qty = l_seqqty  .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'PLANQTY'         .
    ztpp_seq_backup-vals   = l_qty = l_pqty    .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'FORECASTQTY'     .
    ztpp_seq_backup-vals   = l_qty = l_fqty    .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.

    CLEAR: l_tseq, l_data-atwrt, ausp, ztpp_seq_backup.
    l_data-atnam = 'P_SEQ_QTY'.        APPEND l_data.
    l_data-atnam = 'P_PLAN_QTY'.       APPEND l_data.
    l_data-atnam = 'P_FORECAST_QTY'.   APPEND l_data.
    l_data-atnam = 'P_MITU_QTY'.       APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    READ TABLE l_data INDEX 1.
    l_seqqty   = l_data-atwrt   .        CLEAR: l_data.
    READ TABLE l_data INDEX 2.
    l_pqty     = l_data-atwrt   .        CLEAR: l_data.
    READ TABLE l_data INDEX 3.
    l_fqty     = l_data-atwrt   .        CLEAR: l_data.
    READ TABLE l_data INDEX 4.
    l_mituqty  = l_data-atwrt   .        CLEAR: l_data.

    ztpp_seq_backup-tables = 'WOCL'          .
    ztpp_seq_backup-worder = wa_material     .
    ztpp_seq_backup-fname  = 'P_SEQ_QTY'       .
    ztpp_seq_backup-vals   = l_qty = l_seqqty  .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'P_PLAN_QTY'      .
    ztpp_seq_backup-vals   = l_qty = l_pqty    .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'P_FORECAST_QTY'  .
    ztpp_seq_backup-vals   = l_qty = l_fqty    .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'P_MITU_QTY'      .
    ztpp_seq_backup-vals   = l_qty = l_mituqty .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.

    " Sales Order Master Change...
    CLEAR: ztpp_seq_backup, l_qty.
    ztpp_seq_backup-tables = 'VBAP'          .
    ztpp_seq_backup-worder = l_salesorder    .
    SELECT SINGLE kwmeng INTO l_qty
      FROM vbap
     WHERE vbeln = l_salesorder
       AND posnr = '000010' .

    ztpp_seq_backup-fname  = '000010'        .
    ztpp_seq_backup-vals   = l_qty           .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.   CLEAR: l_qty.

    SELECT SINGLE kwmeng INTO l_qty
      FROM vbap
     WHERE vbeln = l_salesorder
       AND posnr = '000020' .

    ztpp_seq_backup-fname  = '000020'        .
    ztpp_seq_backup-vals   = l_qty           .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.   CLEAR: l_qty.
  ENDIF.

  DATA: lt_7jb              LIKE TABLE OF it_7jb  WITH HEADER LINE,
        l_modl              LIKE it_7jb-modl,
        l_cnt               TYPE i.

  lt_7jb[] = it_7jb[].
  SORT lt_7jb BY ordr dist extc intc ssr1.
  DELETE ADJACENT DUPLICATES FROM lt_7jb COMPARING  ordr dist .

  READ TABLE lt_7jb INDEX 1.
  l_ordr = lt_7jb-ordr.  l_dist = lt_7jb-dist.

  LOOP AT lt_7jb .
    IF lt_7jb-ordr = l_ordr  AND  lt_7jb-dist = l_dist .
      CONTINUE.
    ELSE.
      CLEAR: ztpp_seq_backup, wa_material,  l_data, l_data[].

      CONCATENATE l_ordr       l_dist         INTO wa_material .

      l_data-atnam = 'P_SEQ_QTY'.        APPEND l_data.
      l_data-atnam = 'P_PLAN_QTY'.       APPEND l_data.
      l_data-atnam = 'P_FORECAST_QTY'.   APPEND l_data.
      l_data-atnam = 'P_MITU_QTY'.       APPEND l_data.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_material
                ctype        = '001'
           TABLES
                val_table    = l_data
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                error_value  = 4
                OTHERS       = 5.

      READ TABLE l_data INDEX 1.
      l_seqqty   = l_data-atwrt   .        CLEAR: l_data.
      READ TABLE l_data INDEX 2.
      l_pqty     = l_data-atwrt   .        CLEAR: l_data.
      READ TABLE l_data INDEX 3.
      l_fqty     = l_data-atwrt   .        CLEAR: l_data.
      READ TABLE l_data INDEX 4.
      l_mituqty  = l_data-atwrt   .        CLEAR: l_data.

      ztpp_seq_backup-tables = 'WOHD'          .
      ztpp_seq_backup-worder = wa_material     .
      ztpp_seq_backup-fname  = 'P_SEQ_QTY'       .
      ztpp_seq_backup-vals   = l_qty = l_seqqty  .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'P_PLAN_QTY'      .
      ztpp_seq_backup-vals   = l_qty = l_pqty    .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'P_FORECAST_QTY'  .
      ztpp_seq_backup-vals   = l_qty = l_fqty    .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
      ztpp_seq_backup-fname  = 'P_MITU_QTY'      .
      ztpp_seq_backup-vals   = l_qty = l_mituqty .
      MODIFY ztpp_seq_backup FROM ztpp_seq_backup.

      l_ordr = lt_7jb-ordr.  l_dist = lt_7jb-dist.
    ENDIF.
  ENDLOOP.

  IF wa_lines > 0 .
    CLEAR: ztpp_seq_backup, wa_material,  l_data, l_data[].

    CONCATENATE l_ordr       l_dist         INTO wa_material .

    l_data-atnam = 'P_SEQ_QTY'.        APPEND l_data.
    l_data-atnam = 'P_PLAN_QTY'.       APPEND l_data.
    l_data-atnam = 'P_FORECAST_QTY'.   APPEND l_data.
    l_data-atnam = 'P_MITU_QTY'.       APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    READ TABLE l_data INDEX 1.
    l_seqqty   = l_data-atwrt   .        CLEAR: l_data.
    READ TABLE l_data INDEX 2.
    l_pqty     = l_data-atwrt   .        CLEAR: l_data.
    READ TABLE l_data INDEX 3.
    l_fqty     = l_data-atwrt   .        CLEAR: l_data.
    READ TABLE l_data INDEX 4.
    l_mituqty  = l_data-atwrt   .        CLEAR: l_data.

    ztpp_seq_backup-tables = 'WOHD'          .
    ztpp_seq_backup-worder = wa_material     .
    ztpp_seq_backup-fname  = 'P_SEQ_QTY'       .
    ztpp_seq_backup-vals   = l_qty = l_seqqty  .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'P_PLAN_QTY'      .
    ztpp_seq_backup-vals   = l_qty = l_pqty    .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'P_FORECAST_QTY'  .
    ztpp_seq_backup-vals   = l_qty = l_fqty    .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
    ztpp_seq_backup-fname  = 'P_MITU_QTY'      .
    ztpp_seq_backup-vals   = l_qty = l_mituqty .
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
  ENDIF.

  lt_7jb[] = it_7jb[].
  SORT lt_7jb BY modl .
  DELETE ADJACENT DUPLICATES FROM lt_7jb COMPARING modl .
  c_prog = 'Z_FPP_VIN_GENERATION'.

  LOOP AT lt_7jb .
    SELECT SINGLE *
      FROM ztpp_common_vals
     WHERE jobs = c_prog
       AND key2 = lt_7jb-modl .
*      AND key3 = '******************'.

    ztpp_seq_backup-tables = 'VIN_GEN'         .
    ztpp_seq_backup-worder = lt_7jb-modl       .
    ztpp_seq_backup-fname  = 'ITEM4'           .
    ztpp_seq_backup-vals   = ztpp_common_vals-item4+14(6).
    MODIFY ztpp_seq_backup FROM ztpp_seq_backup.
  ENDLOOP.
ENDFORM.                    " BACK_UP_DATA1

*&---------------------------------------------------------------------*
*&      Form  EXIT_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_message.
  MESSAGE i001 WITH text-900 text-901." TEXT-902 TEXT-903 .
ENDFORM.                    " EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  workorder_mituquty_restoration
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MATERIAL  text
*      -->P_L_PLNMG  text
*----------------------------------------------------------------------*
FORM workorder_mituquty_restoration USING    pa_material
                                             pa_mitu .
  DATA: l_vars                LIKE TABLE OF zspp_vin_value
                                                  WITH HEADER LINE,
        l_dec1(6)             TYPE c.
  REFRESH l_vars.
  CLEAR: l_vars              .     l_dec1 = pa_mitu     .
  l_vars-atnam = 'P_MITU_QTY' .    l_vars-atwrt = l_dec1. APPEND l_vars.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_material
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = l_vars
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

  IF sy-subrc NE 0.
    wa_error = 'X'.
  ENDIF.
ENDFORM.                    " workorder_mituquty_restoration
*&---------------------------------------------------------------------*
*&      Form  check_vehicle_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB  text
*----------------------------------------------------------------------*
FORM check_vehicle_info USING    p_7jb LIKE it_7jb
                                 p_equnr.

  CLEAR: it_vmaster, it_vmaster[] .
  it_vmaster-atnam = 'P_WORK_ORDER' .
  APPEND it_vmaster.
  it_vmaster-atnam = 'P_EXT_COLOR' .
  APPEND it_vmaster.
  it_vmaster-atnam = 'P_INT_COLOR' .
  APPEND it_vmaster.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = p_equnr
       TABLES
            val_table    = it_vmaster
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

  CLEAR wa_material.
  CONCATENATE it_7jb-ordr it_7jb-dist INTO wa_material ."workorder h

  READ TABLE it_vmaster WITH KEY atnam = 'P_WORK_ORDER'
                                 atwrt =  wa_material.
  IF sy-subrc <> 0.
    MESSAGE i001 WITH text-906 wa_material.
  ENDIF.

  READ TABLE it_vmaster WITH KEY atnam = 'P_EXT_COLOR'
                                 atwrt =  p_7jb-extc.
  IF sy-subrc <> 0.
    MESSAGE i001 WITH text-906 p_7jb-extc.
  ENDIF.

  READ TABLE it_vmaster WITH KEY atnam = 'P_INT_COLOR'
                                 atwrt =  p_7jb-intc.
  IF sy-subrc <> 0.
    MESSAGE i001 WITH text-906 p_7jb-intc.
  ENDIF.

ENDFORM.                    " check_vehicle_info
*&---------------------------------------------------------------------*
*&      Form  change_plannedorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB  text
*      -->P_L_PORDER  text
*----------------------------------------------------------------------*
FORM change_plannedorder USING    p_7jb LIKE it_7jb
                                  p_porder.
  DATA :ls_headerdata    LIKE  bapiplaf_i2,
        ls_headerdatax   LIKE  bapiplaf_i2x.


  ls_headerdata-order_start_date  = p_7jb-sqdt.
  ls_headerdata-order_fin_date    = p_7jb-sqdt.
  ls_headerdata-plan_open_date    = p_7jb-sqdt.
  ls_headerdatax-order_start_date = 'X'.
  ls_headerdatax-order_fin_date   = 'X'.
  ls_headerdatax-plan_open_date   = 'X'.

  CALL FUNCTION 'BAPI_PLANNEDORDER_CHANGE'
       EXPORTING
            plannedorder = p_porder
            headerdata   = ls_headerdata
            headerdatax  = ls_headerdatax.


ENDFORM.                    " change_plannedorder
