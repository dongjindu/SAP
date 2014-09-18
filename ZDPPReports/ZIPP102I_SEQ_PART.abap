************************************************************************
* Program Name      : ZIPP102I_SEQ_PART
* Author            : Bobby
* Creation Date     : 2004.02.08.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No :
* Addl Documentation:
* Description       : Vehicle Order(Planned Order) Creation
*                     --> Processing Sequence Change for recoverary..
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zipp102i_seq_part   MESSAGE-ID zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_common_vals,
        equi ,
        ausp .

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
*DATA: BEGIN OF it_vin         OCCURS 0                        .
*        INCLUDE STRUCTURE     ztpp_pmt07jb_b .
*DATA:   matnr                 LIKE mara-matnr,
*      END OF it_vin                          ,
DATA: it_7jb              LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE,
      it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: wa_material             LIKE mara-matnr                 ,
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
PARAMETERS   p_run      RADIOBUTTON GROUP ra  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-102 FOR FIELD p_run  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS   p_rerun    RADIOBUTTON GROUP ra.
SELECTION-SCREEN COMMENT  (55) text-101 FOR FIELD p_rerun.
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
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.

  IF p_run = 'X'.                        " Normal Processing...
    PERFORM get_data        .
    CHECK wa_flag IS INITIAL.
    PERFORM check_data .
    CHECK wa_flag IS INITIAL.


    PERFORM write_timestamp  USING  text-010 .

    PERFORM clear_vinn           .
    PERFORM record_process_step1 .       " Step 1: VIN SPEC GENERATOT.
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " VIN Code Generation Error...
      PERFORM create_log USING '1' it_7jb    . " Log Create in STEP 1 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM record_process_step2 .       " Step 2: Vehicle Master Create
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " Vehicle Master Creation Error....
      PERFORM create_log USING '2' it_7jb    . " Log Create in STEP 2 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM record_process_step3 .       " Step 3: Class Assign
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " Vehicle Master Class Assign Error....
      PERFORM create_log USING '3' it_7jb    . " Log Create in STEP 3 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM record_process_step4 .       " Step 4: Plan Order Creation
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " Plan Order Creation Error....
      PERFORM create_log USING '4' it_7jb    . " Log Create in STEP 4 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM record_process_step5 .       " Step 5: Change Vehicle Master
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " Vehicle Master Information Change Error...
      PERFORM create_log USING '5' it_7jb    . " Log Create in STEP 5 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM record_process_step6 .       " Step 6: Characteristic Assign
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " Vehicle Master Characteristic Assign Assign Error
      PERFORM create_log USING '6' it_7jb    . " Log Create in STEP 6 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM bdc_wocl_summary     .       " Step 7: W/O Color  Summary
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " W/O Color  Summary Change Error...(Including Sales Order)
      PERFORM create_log USING '7' it_7jb    . " Log Create in STEP 7 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM bdc_wohd_summary     .       " Step 8: W/O Header Summary
    IF wa_error = 'X'            .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      " W/O Header Summary Change Error...
      PERFORM create_log USING '8' it_7jb    . " Log Create in STEP 8 .
      EXIT .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    CHECK wa_error = space   .
    PERFORM check_result     .
    PERFORM write_result     .
    PERFORM write_timestamp  USING  text-011 .
  ELSE.
    " Call the Re-Processing Program.
    SUBMIT ZIPP102I_SEQ_PART_REPROCESS
      with p_run   = 'X'
      WITH p_dgmul = 'X'
      WITH p_bgmul = ' '     AND RETURN .
  ENDIF.

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
  c_prog = 'ZIPP101U_PMT07JB_A'.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs  = c_prog
     AND key2  = it_7jb-modl .

  com_date = ztpp_common_vals-item1.   " Previous Max Sequenced Date..
  IF chk_date <= com_date .
    wa_flag  =  'X'      .            " Already Sequenced Data..
  ENDIF.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESS_STEP1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_process_step1 .
  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  SORT it_7jb BY sqdt ssr1 SSR2.
  LOOP AT it_7jb WHERE mtgu NE 'M'.
    PERFORM vin_code_gen      .
    IF wa_error = 'X'.  EXIT.  ENDIF.
    MODIFY it_7jb.
  ENDLOOP.

  CHECK WA_ERROR = SPACE.
  PERFORM SAVE_7JB.
ENDFORM.                    " RECORD_PROCESS_STEP1

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESS_STEP2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_process_step2 .
  DATA: l_matnr              LIKE mara-matnr.

  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  SORT it_7jb BY sqdt ssr1.

  CLEAR: wa_rcv_jobs, wa_snd_jobs.
  LOOP AT it_7jb  WHERE  mtgu NE 'M' .
    CONCATENATE it_7jb-moye  it_7jb-dist it_7jb-bmdl INTO l_matnr.
    CONCATENATE l_matnr it_7jb-ocnn INTO l_matnr SEPARATED BY space  .

    PERFORM job_create1        .    " Vehicle Master Creation..
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
  IF sy-subrc = 0  .
    CLEAR: wa_rcv_jobs, wa_snd_jobs, wa_taskname.
  ELSE.
    wa_error = 'X' .
    EXIT.
  ENDIF.

  LOOP AT it_7jb  WHERE  mtgu = 'M' .
    PERFORM process_mitu_value .
    PERFORM create_log USING 'L' it_7jb    . " Log Create in STEP 2 .
  ENDLOOP.

  CHECK WA_ERROR = SPACE.
  PERFORM SAVE_7JB.
ENDFORM.                    " RECORD_PROCESS_STEP2

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESS_STEP3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_process_step3 .
  DATA: l_matnr         LIKE mara-matnr.

  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  SORT it_7jb BY sqdt ssr1.

  CLEAR: wa_rcv_jobs, wa_snd_jobs.
  LOOP AT it_7jb WHERE mtgu NE 'M' .
    CONCATENATE it_7jb-moye  it_7jb-dist it_7jb-bmdl INTO l_matnr     .
    CONCATENATE l_matnr it_7jb-ocnn INTO l_matnr SEPARATED BY space  .

    PERFORM job_create2 .
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
  IF sy-subrc = 0  .
    CLEAR: wa_rcv_jobs, wa_snd_jobs, wa_taskname.
  ELSE.
    wa_error = 'X'.
    EXIT.
  ENDIF.

  CHECK WA_ERROR = SPACE.
  PERFORM SAVE_7JB.
ENDFORM.                    " RECORD_PROCESS_STEP3

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESS_STEP4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_process_step4 .
  DATA: l_matnr         LIKE mara-matnr.
  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  SORT it_7jb BY sqdt ssr1.

  CLEAR: wa_rcv_jobs, wa_snd_jobs.
  LOOP AT it_7jb WHERE mtgu NE 'M' .
    CONCATENATE it_7jb-moye  it_7jb-dist it_7jb-bmdl INTO l_matnr     .
    CONCATENATE l_matnr it_7jb-ocnn INTO l_matnr SEPARATED BY space  .

    PERFORM job_create3                       .
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
  IF sy-subrc = 0  .
    CLEAR: wa_rcv_jobs, wa_snd_jobs, wa_taskname.
  ELSE.
    wa_error = 'X' .
    EXIT.
  ENDIF.

  CHECK WA_ERROR = SPACE.
  PERFORM SAVE_7JB.
ENDFORM.                    " RECORD_PROCESS_STEP4

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESS_STEP5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_process_step5.
  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  SORT it_7jb BY sqdt ssr1.

  CLEAR: wa_rcv_jobs, wa_snd_jobs.

  LOOP AT it_7jb WHERE mtgu NE 'M' .
    " Error Log Step 5 : Call the Job-Processor.....
    PERFORM job_create4                  .
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
  IF sy-subrc = 0  .
    CLEAR: wa_rcv_jobs, wa_snd_jobs, wa_taskname.
  ELSE.
    wa_error = 'X' .
    EXIT.
  ENDIF.

  CHECK WA_ERROR = SPACE.
  PERFORM SAVE_7JB.
ENDFORM.                    " RECORD_PROCESS_STEP5

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESS_STEP6
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_process_step6.
  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  SORT it_7jb BY sqdt ssr1.
  CLEAR: wa_rcv_jobs, wa_snd_jobs.

  LOOP AT it_7jb WHERE mtgu NE 'M' .
    " Error Log Step 5 : Call the Job-Processor.....
    PERFORM job_create5                  .
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
  IF sy-subrc = 0  .
    CLEAR: wa_rcv_jobs, wa_snd_jobs, wa_taskname.
  ELSE.
    WA_ERROR = 'X'.
    EXIT.
  ENDIF.

  CHECK WA_ERROR = SPACE.
  PERFORM SAVE_7JB.
ENDFORM.                    " RECORD_PROCESS_STEP6

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

  IF sy-subrc NE 0.
    wa_error = 'X'.
    EXIT.
  ENDIF.

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
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

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
    UPDATE ztpp_pmt07jb_b    SET: plnum = l_porder
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

    IF sy-subrc NE 0.
      wa_error = 'X'.
      EXIT.
    ENDIF.
  ENDIF.
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
            object     = pa_material
            mode       = 'W'
            ctype      = '001'
       TABLES
            val_table  = l_vars
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

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

  LOOP AT it_7jb .
    IF it_7jb-ordr = l_ordr  AND  it_7jb-dist = l_dist .
       CONTINUE.
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
      l_data-atnam = 'P_MITU_QTY'.        APPEND l_data.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = wa_material
                ctype        = '001'
           TABLES
                val_table    = l_data
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

      READ TABLE l_data INDEX 1.
      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
      l_plnmg  = l_plnmg - l_mituqty.

      PERFORM plan_quantity USING l_ordr   l_dist   ' '    ' '
                                  l_mqty   l_seqqty l_pqty l_fqty 'H' .

      PERFORM call_bdc_workorder USING wa_material l_seqqty
                                       l_pqty      l_fqty    l_plnmg .

      IF wa_error = 'X'.  EXIT.  ENDIF.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
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
    l_data-atnam = 'P_MITU_QTY'.        APPEND l_data.
*      l_data-atnam = 'P_SEQ_QTY'.         APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

*      READ TABLE l_data INDEX 3.
*      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
*      l_plnmg = l_sqty .                  " L_plnmg + l_seqqty.
    READ TABLE l_data INDEX 1.
    l_plnmg = l_data-atwrt   .         CLEAR: l_data.
    l_plnmg  = l_plnmg - l_mituqty.

    PERFORM plan_quantity USING l_ordr   l_dist   ' '    ' '
                                l_mqty   l_seqqty l_pqty l_fqty 'H' .

    PERFORM call_bdc_workorder USING wa_material l_seqqty
                                     l_pqty      l_fqty    l_plnmg .

    IF wa_error = 'X'.  EXIT.  ENDIF.
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
        l_pqty               LIKE ztpp_wosum-seqqty ,
        l_fqty               LIKE ztpp_wosum-seqqty ,
        l_mituqty            LIKE ztpp_wosum-mituqty,
        l_data               LIKE TABLE OF zspp_vin_value
                                                       WITH HEADER LINE.

  DATA: l_count              TYPE i.

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

      l_seqqty  = l_seq     + l_tseq  .      " Total SEQ-Qty..
      PERFORM plan_quantity USING l_ordr   l_dist  l_extc  l_intc
                         l_modqty l_seqqty l_pqty  l_fqty  'C' .
      UPDATE ztpp_wosum  SET: seqqty  = l_seqqty
                              planqty = l_pqty
                          forecastqty = l_fqty
                        WHERE wo_ser = l_ordr
                          AND nation = l_dist(3)
                          AND dealer = l_dist+3(2)
                          AND extc   = l_extc
                          AND intc   = l_intc     .

      " Process.
      IF sy-subrc NE 0.
        wa_error = 'X'.
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
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

*     READ TABLE l_data INDEX 2.
*     l_plnmg = l_data-atwrt   .
*     l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
      " MITU Qauntity is MUNIS.. => MITU Sequence reduced the MITU-Qty..
      READ TABLE l_data INDEX 1.
      l_plnmg = l_data-atwrt   .
      l_plnmg = l_plnmg - l_mituqty.    CLEAR: l_data.

      PERFORM call_bdc_workorder USING wa_material l_seqqty
                                       l_pqty      l_fqty    l_plnmg.

      IF wa_error = 'X'.   EXIT.  ENDIF.
      WRITE AT: /001(018)  wa_material,     " TO-BE DELETE LINE.
                 020(005) 'WO-CL'     ,     " TO-BE DELETE LINE.
                 025(001) sy-vline    ,     " TO-BE DELETE LINE.
                 026(004) wa_error    ,     " TO-BE DELETE LINE.
                 030(001) sy-vline    ,     " TO-BE DELETE LINE.
                 031(020) l_seqqty    .     " TO-BE DELETE LINE.

      " Sales Order Master Change...
      PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
                                         l_seqqty     l_modqty  .
      IF wa_error = 'X'.   EXIT.  ENDIF.
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
    UPDATE ztpp_wosum  SET: seqqty  = l_seqqty
                            planqty = l_pqty
                        forecastqty = l_fqty
                      WHERE wo_ser  = l_ordr          " it_7jb-ordr
                        AND nation  = l_dist(3)       " it_7jb-dist(3)
                        AND dealer  = l_dist+3(2)     " it_7jb-dist+3(2)
                        AND extc    = l_extc          " it_7jb-extc
                        AND intc    = l_intc     .    " it_7jb-intc  .

    IF sy-subrc NE 0.

      " Step 1: Update Fail - Reason: Data not found!!!
      "         Check the Working data.
      wa_error = 'X'.
      EXIT.
    ELSE.                                  " TO-BE DELETE LINE.

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
*   l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_data
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

*   READ TABLE l_data INDEX 2.
*   l_plnmg = l_data-atwrt   .
*   l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
    READ TABLE l_data INDEX 1.
    l_plnmg = l_data-atwrt   .
    l_plnmg = l_plnmg - l_mituqty.    CLEAR: l_data.

    PERFORM call_bdc_workorder USING wa_material l_seqqty
                                     l_pqty      l_fqty    l_plnmg .

    IF wa_error = 'X'.   EXIT.  ENDIF.
    WRITE AT: /001(018)  wa_material,     " TO-BE DELETE LINE.
               020(005) 'WO-CL'     ,     " TO-BE DELETE LINE.
               025(001) sy-vline    ,     " TO-BE DELETE LINE.
               026(004) wa_error    ,     " TO-BE DELETE LINE.
               030(001) sy-vline    ,     " TO-BE DELETE LINE.
               031(020) l_seqqty    .     " TO-BE DELETE LINE.

    " Sales Order Master Change...
    PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
                                       l_seqqty     l_modqty  .
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
        l_chkqty             TYPE i                 ,
        l_count              TYPE i                 ,
        l_data               LIKE TABLE OF conf_out    WITH HEADER LINE.

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
        wa_err_hd = wa_flag  = 'E'     .
        WRITE AT: /001(09)  l_ordr ,
                   011(05)  l_dist ,
                   017(03)  l_extc ,
                   021(03)  l_intc .
        ULINE AT: /(60)            .
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
*&      Form  job_create1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_create1  .
  DO .
    CALL FUNCTION 'Z_FPP_VEHICLE_CREATION_PART1'
        STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
         PERFORMING return_step1 ON END OF TASK
         EXPORTING
           p_7jb                       = it_7jb
           p_date                      = sy-datum
         EXCEPTIONS
           communication_failure       = 1
           system_failure              = 2
           RESOURCE_FAILURE            = 3
           OTHERS                      = 4.

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
ENDFORM.                    " job_create1

*&---------------------------------------------------------------------*
*&      Form  job_create2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_create2  .
  DO .
    CALL FUNCTION 'Z_FPP_VEHICLE_CREATION_PART2'
        STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
         PERFORMING return_step2 ON END OF TASK
         EXPORTING
           p_7jb                       = it_7jb
           p_date                      = sy-datum
         EXCEPTIONS
           communication_failure       = 1
           system_failure              = 2
           RESOURCE_FAILURE            = 3
           OTHERS                      = 4.

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
ENDFORM.                    " job_create2

*&---------------------------------------------------------------------*
*&      Form  job_create3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_create3  .
  DO .
    CALL FUNCTION 'Z_FPP_PLANORDER_CREATION_PART1'
        STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
         PERFORMING return_step3 ON END OF TASK
         EXPORTING
           p_7jb                       = it_7jb
           p_date                      = sy-datum
         EXCEPTIONS
           communication_failure       = 1
           system_failure              = 2
           RESOURCE_FAILURE            = 3
           OTHERS                      = 4.

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
ENDFORM.                    " job_create3

*&---------------------------------------------------------------------*
*&      Form  job_create4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_create4   .
  DO .
    CALL FUNCTION 'Z_FPP_PLANORDER_CREATION_PART2'
        STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
         PERFORMING return_step4 ON END OF TASK
         EXPORTING
           p_7jb                       = it_7jb
           p_date                      = sy-datum
         EXCEPTIONS
           communication_failure       = 1
           system_failure              = 2
           RESOURCE_FAILURE            = 3
           OTHERS                      = 4.

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
ENDFORM.                    " job_create4

*&---------------------------------------------------------------------*
*&      Form  job_create5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job_create5  .
  DO .
    CALL FUNCTION 'Z_FPP_PLANORDER_CREATION_PART3'
        STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
         PERFORMING return_step5 ON END OF TASK
         EXPORTING
           p_7jb                       = it_7jb
           p_date                      = sy-datum
         EXCEPTIONS
           communication_failure       = 1
           system_failure              = 2
           RESOURCE_FAILURE            = 3
           OTHERS                      = 4.

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
ENDFORM.                    " job_create5

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
  DATA: l_matnr              LIKE mara-matnr ,
        l_vin                LIKE mara-matnr ,
        l_mode               LIKE ztpp_common_vals-key2.

  " Call the function for the Last Data...
  l_mode = it_7jb-modl .     CLEAR: l_vin.
  CONCATENATE  it_7jb-ordr   it_7jb-dist      INTO  l_matnr .
  PERFORM get_vinn  USING l_vin  l_matnr                    .

  CALL FUNCTION 'Z_FPP_VIN_GENERATION'
       EXPORTING
            w_order   = l_vin
            mode      = l_mode
       IMPORTING
            p_lastid  = l_vin
       EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

  IF sy-subrc <> 0  OR  l_vin = space .
    wa_error = 'X'.
    MESSAGE w003  WITH text-110 l_matnr  '::'  sy-subrc.
    EXIT.
  ENDIF.

  it_7jb-vinn = l_vin     .

  " Update the ZTPP_PMT07JB_B..
  UPDATE ztpp_pmt07jb_b   SET  vinn  = l_vin
                        WHERE sqdt  = it_7jb-sqdt
                          AND modl  = it_7jb-modl
                          AND mtgu  = it_7jb-mtgu
                          AND ssr1  = it_7jb-ssr1
                          AND ssr2  = it_7jb-ssr2 .

  IF sy-subrc NE 0  .
    "Error Data
    wa_error = 'X' .
    MESSAGE w003  WITH text-120  it_7jb-ssr1  '::'  sy-subrc.
  ENDIF.
ENDFORM.                    " VIN_CODE_GEN

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
  UPDATE ztpp_pmt07jb_b  CLIENT SPECIFIED
                          SET: vinn  = l_vinn
                               plnum = l_porder
                        WHERE  mandt = sy-mandt .
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
  IF l_item10_qty_flg = 'I'       .
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
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_VEHIVLE_CREATION_PART1'
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
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_VEHIVLE_CREATION_PART2'
         EXCEPTIONS
         communication_failure       = 1
         system_failure              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.
ENDFORM.                    " return_STEP2

*&---------------------------------------------------------------------*
*&      Form  return_STEP3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_step3  USING p_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_CREATION_PART1'
         EXCEPTIONS
         communication_failure       = 1
         system_failure              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.
ENDFORM.                    " return_STEP3

*&---------------------------------------------------------------------*
*&      Form  return_STEP4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_step4  USING p_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_CREATION_PART2'
         EXCEPTIONS
         communication_failure       = 1
         system_failure              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.
ENDFORM.                    " return_STEP4

*&---------------------------------------------------------------------*
*&      Form  return_STEP5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM return_step5  USING p_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_CREATION_PART3'
         EXCEPTIONS
         communication_failure       = 1
         system_failure              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.
ENDFORM.                    " return_STEP5

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

  SORT it_7jb BY moye dist bmdl ocnn vers .
  DELETE ADJACENT DUPLICATES FROM lt_7jb
                        COMPARING moye dist bmdl ocnn vers .

  LOOP AT lt_7jb            .
    " Function Call for the PCC check...
    CONCATENATE lt_7jb-moye  lt_7jb-dist  lt_7jb-bmdl  lt_7jb-ocnn
           INTO l_matnr .
    CONCATENATE l_matnr lt_7jb-vers INTO l_matnr SEPARATED BY space.
    CLEAR: l_flag.
    l_verid = lt_7jb-vers+1(2) .
    PERFORM check_pcc_function   USING  l_matnr        l_verid  l_flag .
    CHECK l_flag = space OR l_flag = 'X' .
    CLEAR: l_text .
    CONCATENATE l_matnr      l_verid                 INTO l_text .
    PERFORM create_pcc USING l_matnr  'P001'  l_text   l_verid  l_flag .
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
      SELECT SINGLE aufnr INTO lp_aufnr
        FROM afko
       WHERE plnbez = pa_matnr .

      IF sy-subrc = 0.
        SELECT SINGLE aufnr INTO lp_aufnr
          FROM aufk
         WHERE aufnr = lp_aufnr
           AND auart = 'RM01'
           AND werks = 'P001'  .
        IF sy-subrc  = 0       .
          CLEAR: pa_flag.
        ELSE.
          CLEAR: pa_flag.
        ENDIF                  .
      ELSE.
        pa_flag = 'X'.
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
       AND gubb = 'A'
       AND gub1 = '1'    .

    pa_pqty = l_qty      .
    pa_fqty = pa_mqty - pa_sqty - l_qty .
  ELSE.
    SELECT SUM( pqty ) INTO  l_qty
      FROM ztpp_pmt07jb_a
     WHERE ordr = pa_ordr
       AND dist = pa_dist
       AND gubb = 'A'
       AND gub1 = '1'    .

    pa_pqty = l_qty      .
    pa_fqty = pa_mqty - pa_sqty - l_qty .
  ENDIF.
ENDFORM.                    " PLAN_QUANTITY

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
ENDFORM.                    " CALL_PLAN_SAVE

*&---------------------------------------------------------------------*
*&      Form  GET_VINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VIN  text
*      -->P_IT_7JB_ORDR  text
*      -->P_IT_7JB_DIST  text
*----------------------------------------------------------------------*
FORM get_vinn USING    pa_vin  pa_matnr .
  DATA: l_data         LIKE TABLE OF zspp_vin_value    WITH HEADER LINE.

  l_data-atnam = 'P_VIN_SPEC'.         APPEND l_data.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pa_matnr
            ctype        = '001'
       TABLES
            val_table    = l_data
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

  CHECK sy-subrc = 0       .

  READ TABLE l_data INDEX 1.
  IF l_data-zflag = space .
    pa_vin       = l_data-atwrt.
  ENDIF.
ENDFORM.                    " GET_VINN

*&---------------------------------------------------------------------*
*&      Form  SAVE_7JB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_7JB.
  DATA: LT_7JB           LIKE TABLE OF ZTPP_PMT07JB_B  WITH HEADER LINE.

  CLEAR: LT_7JB, LT_7JB[].

  LOOP AT IT_7JB.
    MOVE-CORRESPONDING IT_7JB  TO LT_7JB .
    APPEND LT_7JB .
  ENDLOOP.
  MODIFY ZTPP_PMT07JB_B FROM TABLE LT_7JB .
ENDFORM.                    " SAVE_7JB
