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
************************************************************************
REPORT  zipp102i_seq_main   MESSAGE-ID zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_common_vals,
        equi ,
        ausp .

************* DO NOT USE!!!!! *****************************************
DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname,
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      it_rec                  LIKE TABLE OF mara       WITH HEADER LINE.
********************************************************************

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: wa_material             LIKE mara-matnr                 ,
      wa_number               LIKE ztpp_pp_log_head-logkey    ,
      wa_7jb                  LIKE ztpp_pmt07jb_b             ,
      wa_7jb_log              LIKE ztpp_pmt07jb_b             ,
      wa_maxday               LIKE sy-datum                   ,
      wa_minday               LIKE sy-datum                   ,
      wa_vin                  LIKE mara-matnr                 ,
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
      wa_mode                 TYPE c   VALUE   'N'            ,
      sv_log_color            LIKE mara-matnr                 ,
      jobc                    LIKE tbtcjob-jobcount           ,
      jobn                    LIKE  tbtcjob-jobname           ,
      immediate               LIKE btch0000-char1  VALUE  'X' ,
      wa_count(4)             TYPE n                          ,
      c_prog                  LIKE sy-repid                   .

RANGES: s_jobnam FOR tbtcp-jobname,
        s_pronam FOR tbtcp-progname,
        s_date FOR tbtcp-sdldate,
        s_time FOR tbtcp-sdltime.

*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF it_vin         OCCURS 0                        .
        INCLUDE STRUCTURE     ztpp_pmt07jb_b .
DATA:   vin                   LIKE mara-matnr,
      END OF it_vin                          ,
      it_7jb              LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE,
      it_bdcdata          LIKE TABLE OF bdcdata        WITH HEADER LINE,
      it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

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
  IF p_run = 'X'.
    " Normal Processing...
    PERFORM get_data.
    CHECK wa_flag IS INITIAL.
*   PERFORM check_data .
    CHECK wa_flag IS INITIAL.
    PERFORM bdc_wocl_summary .           " Step 1: Work Order Color
    IF wa_error = 'X'        .
      ROLLBACK WORK          .
      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      PERFORM create_log USING '1' wa_7jb_log. " Log Create in STEP 1 .
      EXIT .
    ELSE.
      COMMIT WORK            .
    ENDIF.

    PERFORM bdc_wohd_summary .
    IF wa_error = 'X'        .
      ROLLBACK WORK          .
      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      PERFORM create_log USING '2' wa_7jb_log. " Log Create in STEP 2 .
      EXIT .
    ELSE.
      COMMIT WORK            .
    ENDIF.

    PERFORM clear_vinn       .
    PERFORM record_processing.           " Log Create in Step 3 & 4 .
  ELSE.
    " Reprocessing..
  ENDIF.

  CHECK wa_error = space   .
  PERFORM check_result     .
  PERFORM write_result     .
  PERFORM write_timestamp  USING  text-011 .

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
   WHERE jobs  = c_prog  .

  com_date = ztpp_common_vals-item1.   " Previous Max Sequenced Date..
  IF chk_date <= com_date .
    wa_flag  =  'X'      .            " Already Sequenced Data..
  ENDIF.
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
    " Error Log Step 3 : VIN Code Generation Error...
    PERFORM check_vin         .
    PERFORM vin_code_gen      .
    IF wa_error = 'X'.  EXIT.  ENDIF.
    MODIFY it_7jb.
  ENDLOOP.

  IF wa_error = 'X'.  EXIT.  ENDIF.

* COMMIT WORK AND WAIT.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.
  IF sy-subrc NE 0.
    PERFORM create_log USING '3' it_7jb.   " Log Create in STEP 3    .
    EXIT.
  ENDIF.

  CLEAR: wa_rcv_jobs, wa_snd_jobs.
  LOOP AT it_7jb .
    wa_7jb = it_7jb .
    " Error Log Step 4 : Call the Job-Processor.....
    CASE it_7jb-mtgu .
      WHEN 'M'      .
        PERFORM process_mitu_value .
      WHEN OTHERS.
        PERFORM select_vin        .
        PERFORM job_create  USING  '1'           .
    ENDCASE.
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
  IF sy-subrc = 0  AND  wa_excp_flag = space.
    CLEAR: wa_rcv_jobs, wa_snd_jobs, wa_taskname.
  ELSE.
    MESSAGE w001 WITH text-300 .
    EXIT.
  ENDIF.

  LOOP AT it_7jb WHERE mtgu NE 'M' .
    " Error Log Step 5 : Call the Job-Processor.....
    PERFORM select_vin        .
    PERFORM job_create USING '2'         .
  ENDLOOP.

  WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
  IF sy-subrc = 0  AND  wa_excp_flag = space.
  ELSE.
    MESSAGE w001 WITH text-400 .
    EXIT.
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
FORM call_bdc_workorder USING    pa_material  pa_plnmg  pa_char  pa_z .
  DATA: l_vars                LIKE TABLE OF zspp_vin_value
                                                  WITH HEADER LINE,
        l_dec                 TYPE i,
        l_dec1(6)             TYPE c,
        l_dec2(6)             TYPE c.

  l_dec = pa_plnmg.
  WRITE l_dec    TO l_dec1.

  l_vars-atnam = pa_char .  l_vars-atwrt = l_dec1.  APPEND l_vars.
  IF pa_z = 'Z'.
    l_dec = wa_mng  .
    WRITE l_dec   TO l_dec2.
    l_vars-atnam = 'P_MITU_QTY' .  l_vars-atwrt =  l_dec2.
    APPEND l_vars.
  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object     = pa_material
            mode       = 'W'
            ctype      = '001'
       TABLES
            val_table  = l_vars
       EXCEPTIONS
            no_data    = 1
            error_mode = 2
            OTHERS     = 3.

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
*
*  PERFORM bdc_dynpro_processing USING :
*                         'X'  'SAPMV45A'             '0102',
*                         ' '  'BDC_OKCODE'           '=UER2' ,
*                         ' '  'VBAK-VBELN'            pa_sorder,
*
*                         'X'  'SAPMV45A'             '4001',
*                         ' '  'BDC_OKCODE'           '=SICH' ,
*                         ' '  'RV45A-KWMENG(01)'      l_val10,
*                         ' '  'RV45A-KWMENG(02)'      l_val20.
*
*  CALL TRANSACTION 'VA02'  USING it_bdcdata MODE wa_mode
*                           MESSAGES INTO    it_msg    .
*
*  LOOP AT it_msg WHERE msgid = 'E' .
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*         EXPORTING
*              msgid               = it_msg-msgid
*              msgnr               = it_msg-msgnr
*              msgv1               = it_msg-msgv1
*              msgv2               = it_msg-msgv2
*              msgv3               = it_msg-msgv3
*              msgv4               = it_msg-msgv4
*         IMPORTING
*              message_text_output = wa_msg.
*    l_flag = 'X'.
*    WRITE: /'Sales Order -- ' , wa_msg .
*  ENDLOOP.
*
*  IF l_flag = 'X'.
*    ROLLBACK WORK.
*    " Log-Creation for the Re-Working...
*  ELSE.
*    " Saving the Success Material for the Re-Working Point....
*    sv_log_color = wa_material.
*    COMMIT WORK.
*  ENDIF.
*  CLEAR: it_bdcdata, it_bdcdata[], it_msg, it_msg[].
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
      CLEAR: wa_material,  l_data, l_data[], l_plnmg, wa_mng, ausp.
      CONCATENATE l_ordr      l_dist      INTO wa_material .

      " Work Order Header's SEQ  Qty Change......
      l_data-atnam = 'P_SEQ_QTY'.         APPEND l_data.
      l_data-atnam = 'P_MITU_QTY'.        APPEND l_data.
      l_data-atnam = 'P_VIN_SPEC'.        APPEND l_data.
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
                OTHERS       = 4.

      READ TABLE l_data INDEX 1.
      l_plnmg = l_data-atwrt   .         CLEAR: l_data.
      l_plnmg = l_plnmg + l_seqqty.
      READ TABLE l_data INDEX 2.
      wa_mng  = l_data-atwrt   .         CLEAR: l_data.
      wa_mng  = wa_mng  - l_mituqty.
      READ TABLE l_data INDEX 3.
      it_vin-vin = l_data-atwrt.         CLEAR: l_data.

*      PERFORM call_bdc_workorder
*                          USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

      IF wa_error = 'X'.  EXIT.  ENDIF.
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

  IF wa_error = 'X'.  EXIT.  ENDIF.

  CHECK wa_error = space.

  IF wa_lines > 0 .
    CLEAR: wa_material,  l_data, l_data[], l_plnmg, wa_mng, ausp.
    CONCATENATE l_ordr      l_dist      INTO wa_material .
    " Work Order Header's SEQ  Qty Change......
    l_data-atnam = 'P_SEQ_QTY'.         APPEND l_data.
    l_data-atnam = 'P_MITU_QTY'.        APPEND l_data.
    l_data-atnam = 'P_VIN_SPEC'.        APPEND l_data.

    " Work Order Header's SEQ  Qty Change......
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
              OTHERS       = 4.


    READ TABLE l_data INDEX 1.
    l_plnmg = l_data-atwrt   .         CLEAR: l_data.
    l_plnmg = l_plnmg + l_seqqty.
    READ TABLE l_data INDEX 2.
    wa_mng  = l_data-atwrt   .         CLEAR: l_data.
    wa_mng  = wa_mng  - l_mituqty.
    READ TABLE l_data INDEX 3.
    it_vin-vin = l_data-atwrt.         CLEAR: l_data.

*    PERFORM call_bdc_workorder
*                        USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.
    IF wa_error = 'X'.  EXIT.  ENDIF.
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

  DATA: l_count              TYPE i.

  PERFORM write_timestamp  USING  text-010 .

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
*      UPDATE ztpp_wosum   SET seqqty  = l_seqqty
*                        WHERE wo_ser = l_ordr
*                          AND nation = l_dist(3)
*                          AND dealer = l_dist+3(2)
*                          AND extc   = l_extc
*                          AND intc   = l_intc     .

      " Process.
      IF sy-subrc NE 0.
        wa_error = 'X'.
        EXIT.
      ENDIF.
      CLEAR: wa_material,  l_data, l_data[].
      CONCATENATE l_ordr       l_dist         INTO wa_material .
      CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

      CLEAR: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
      l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
      l_data-atnam = 'P_MITU_QTY'.    APPEND l_data.
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
                OTHERS       = 4.

      READ TABLE l_data INDEX 1.
      l_plnmg = l_data-atwrt   .
      l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
      " MITU Qauntity is MUNIS.. => MITU Sequence reduced the MITU-Qty..
      READ TABLE l_data INDEX 2.
      wa_mng  = l_data-atwrt   .
      wa_mng  = wa_mng  - l_mituqty.    CLEAR: l_data.

*      PERFORM call_bdc_workorder
*                           USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.
      IF wa_error = 'X'.   EXIT.  ENDIF.

      " Sales Order Master Change...
*      PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
*                                         l_seqqty     l_modqty  .
      IF wa_error = 'X'.   EXIT.  ENDIF.

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
     WHERE wo_ser = it_7jb-ordr
       AND nation = it_7jb-dist(3)
       AND dealer = it_7jb-dist+3(2)
       AND extc   = it_7jb-extc
       AND intc   = it_7jb-intc     .

    l_seqqty  = l_seq     + l_tseq  .
*    UPDATE ztpp_wosum   SET seqqty  = l_seqqty
*                      WHERE wo_ser = it_7jb-ordr
*                        AND nation = it_7jb-dist(3)
*                        AND dealer = it_7jb-dist+3(2)
*                        AND extc   = it_7jb-extc
*                        AND intc   = it_7jb-intc     .

    IF sy-subrc NE 0.
      " Step 1: Update Fail - Reason: Data not found!!!
      "         Check the Working data.
      wa_error = 'X'.
      EXIT.
    ENDIF.

    CLEAR: wa_material,  l_data, l_data[].
    CONCATENATE l_ordr       l_dist         INTO wa_material .
    CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

    CLEAR: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
    l_data-atnam = 'P_SEQ_QTY'.     APPEND l_data.
    l_data-atnam = 'P_MITU_QTY'.    APPEND l_data.
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
              OTHERS       = 4.

    READ TABLE l_data INDEX 1.
    l_plnmg = l_data-atwrt   .
    l_plnmg = l_plnmg + l_seq.        CLEAR: l_data.
    READ TABLE l_data INDEX 2.
    wa_mng  = l_data-atwrt   .
    wa_mng  = wa_mng  - l_mituqty.    CLEAR: l_data.

*    PERFORM call_bdc_workorder
*                         USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

    IF wa_error = 'X'.   EXIT.  ENDIF.

    " Sales Order Master Change...
*    PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
*                                       l_seqqty     l_modqty  .
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
                        WHERE jobs  = c_prog   .
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
  WRITE: / 'Calling Process Type...', pa_type.
  WRITE AT: /001(010) wa_7jb-ordr ,
             011(006) wa_7jb-dist ,
             017(020) wa_7jb-vinn ,
             037(200) wa_7jb      .
  DO .
    CASE pa_type.
      WHEN '1'  .
        CALL FUNCTION 'Z_FPP_VEHICLE_CREATION'
            STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
             PERFORMING return_step1 ON END OF TASK
             EXPORTING
               p_7jb                       = wa_7jb
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
               p_7jb                       = wa_7jb
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
        clear: wa_excp_flag .
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
  l_mode = 'EMF'.      " wa_7jb-modl .
  l_vin = it_vin-vin.

  CALL FUNCTION 'Z_FPP_VIN_GENERATION'
       EXPORTING
            w_order  = l_vin
            mode     = l_mode
       IMPORTING
            p_lastid = l_vin.

  it_7jb-vinn = wa_7jb-vinn = l_vin     .

  " Update the ZTPP_PMT07JB_B..
  UPDATE ztpp_pmt07jb_b   SET  vinn  = l_vin
                        WHERE sqdt  = wa_7jb-sqdt
                          AND modl  = wa_7jb-modl
                          AND mtgu  = wa_7jb-mtgu
                          AND ssr1  = wa_7jb-ssr1 .

  IF sy-subrc NE 0  OR l_vin = space .
    "Error Data
    wa_error = 'X' .
    PERFORM create_log USING '3' it_7jb.   " Log Create in STEP 3    .
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

  IF p_item10_qty = 0 AND p_item10_org > 0.
    l_item10_flg = 'U'.             " l_item10_flg = 'U'.
    l_item10_qty_flg = 'D'.
  ELSEIF p_item10_qty > 0 AND p_item10_org = 0.
    l_item10_flg = 'U'.
    l_item10_qty_flg = 'I'.
  ELSE.
    l_item10_flg = 'U'.
    l_item10_qty_flg = 'U'.
  ENDIF.

  IF p_item20_qty = 0 AND p_item20_org > 0.
    l_item20_flg = 'U'.
    l_item20_qty_flg = 'D'.
  ELSEIF p_item20_qty > 0 AND p_item20_org = 0.
    l_item20_flg = 'U'.
    l_item20_qty_flg = 'I'.
  ELSE.
    l_item20_flg = 'U'.
    l_item20_qty_flg = 'U'.
  ENDIF.

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
  l_it_lines-sched_line = '0001'.
  l_it_lines-req_qty = p_item10_org.
  APPEND l_it_lines.
  l_it_lines-itm_number = '000020'.
  l_it_lines-sched_line = '0001'.
  l_it_lines-req_qty = p_item20_org.
  APPEND l_it_lines.

  l_it_linesx-updateflag = l_item10_qty_flg.
  l_it_linesx-itm_number = '000010'.
  l_it_linesx-sched_line = '0001'.
  l_it_linesx-req_qty = 'X'.
  APPEND l_it_linesx.
  l_it_linesx-updateflag = l_item20_qty_flg.
  l_it_linesx-itm_number = '000020'.
  l_it_linesx-sched_line = '0001'.
  l_it_linesx-req_qty = 'X'.
  APPEND l_it_linesx.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
       EXPORTING
            salesdocument    = pa_order
            order_header_inx = l_it_ord_header_inx
       TABLES
            return           = l_it_return
            order_item_in    = l_it_itm
            order_item_inx   = l_it_itmx
            schedule_lines   = l_it_lines
            schedule_linesx  = l_it_linesx.

  LOOP AT l_it_return.
    IF l_it_return-type = 'E' OR
       l_it_return-type = 'A'   .
      wa_error = 'X'           .
*    ELSE.
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
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_VEHIVLE_CREATION'
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
