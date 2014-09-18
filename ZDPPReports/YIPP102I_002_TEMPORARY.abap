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
REPORT  ZIPP102I_SEQ_MAIN   MESSAGE-ID zmpp  .

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
      wa_maxday               LIKE sy-datum                   ,
      wa_minday               LIKE sy-datum                   ,
      wa_vin                  LIKE mara-matnr                 ,
      wa_lines                TYPE i                          ,
      wa_msg(70)              TYPE c                          ,
      wa_mng                  TYPE i                          ,
      wa_flag                 TYPE c                          ,
      wa_err_hd               TYPE c                          ,
      wa_mode                 TYPE c   VALUE   'N'            ,
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

DATA: global_job          LIKE TABLE OF tbtcjob        WITH HEADER LINE,
      it_joblist          LIKE TABLE OF tbtcjob        WITH HEADER LINE,
      global_start_date   LIKE TABLE OF tbtcstrt       WITH HEADER LINE,
      global_step_tbl     LIKE TABLE OF tbtcstep       WITH HEADER LINE.

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
START-OF-SELECTION.
*----------------------------------------------------------------------
  IF p_run = 'X'.
    " Normal Processing...
    PERFORM get_data.
    CHECK wa_flag IS INITIAL.
*    PERFORM check_data .
*    CHECK wa_flag IS INITIAL.
*    PERFORM bdc_wocl_summary .           " Step 1: Work Order Color
*    PERFORM bdc_wohd_summary .
    PERFORM record_processing.
  ELSE.
    " Reprocessing..
  ENDIF.
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
  data: com_date             type d,
        chk_date             type d.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_b            " a
   where sqdt = '20040217'
     and ssr1 < '0009'     .


*  DESCRIBE TABLE it_7jb LINES wa_lines .
*  read table it_7jb index 1.
*  chk_date = it_7jb-sqdt   .
*
*  " Data Check for Re-Running..
*  c_prog = 'ZIPP101U_PMT07JB_A'.
*  SELECT SINGLE *
*    FROM ztpp_common_vals
*   WHERE jobs  = c_prog  .
*
*  com_date = ztpp_common_vals-ITEM1.   " Previous Max Sequenced Date..
*  if chk_date <= com_date .
*     wa_flag  =  'X'      .            " Already Sequenced Data..
*  endif.
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
  LOOP AT it_7jb .
    CASE it_7jb-mtgu .
      WHEN 'M'      .
        PERFORM process_mitu_value .
      WHEN OTHERS.
*        PERFORM check_vin         .
*        PERFORM vin_code_gen      .
        c_prog = 'ZIPP102I_SEQ_JOB1'.
        PERFORM job_create  USING  '_VM' .
        WAIT UP TO 1  SECONDS.
    ENDCASE.
    MODIFY it_7jb.
  ENDLOOP.
*
*  COMMIT WORK.
*
*  LOOP AT it_7jb WHERE mtgu NE 'M' .
*    PERFORM check_vin         .
*    c_prog = 'ZIPP102I_SEQ_JOB2'.
*    PERFORM job_create USING '_PLANORDER'.
*  ENDLOOP.
*  PERFORM update_commonvlas    .
  PERFORM delete_joblist                .
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
  l_val20 = l_val20 - l_val10.
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
    CLEAR: wa_material, l_plnmg, wa_mng, ausp.
    CONCATENATE l_ordr      l_dist      INTO wa_material .

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

      " Process.
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

    CLEAR: wa_material.
    CONCATENATE l_ordr       l_dist         INTO wa_material .
    CONCATENATE wa_material  l_extc  l_intc INTO wa_material.

    CLEAR: l_plnmg, wa_mng, l_tseq, ausp.
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
  SELECT MAX( sqdt ) MIN( SQDT ) INTO (wa_maxday, wa_minday)
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
  REFRESH: global_job, global_step_tbl.
  CLEAR:   global_job, global_step_tbl.

  wa_count = wa_count + 1.
  CONCATENATE sy-mandt '_' wa_count  pa_type INTO jobn.

  global_job-jobname = jobn.             "S_PROG.
  global_job-jobclass = 'A'.             "
  global_job-newflag = 'O'.
  global_step_tbl-program = c_prog.      "dummy step
  global_step_tbl-typ = 'A'.             "
  global_step_tbl-status = 'P'.          "scheduled
  global_step_tbl-authcknam = sy-uname.
  APPEND global_step_tbl.
  APPEND global_job.

  CALL FUNCTION 'BP_JOB_CREATE'
       EXPORTING
            job_cr_dialog       = 'N'
            job_cr_head_inp     = global_job
       IMPORTING
            job_cr_head_out     = global_job
       TABLES
            job_cr_steplist     = global_step_tbl
       EXCEPTIONS
            cant_create_job     = 1
            invalid_dialog_type = 2
            invalid_job_data    = 3
            job_create_canceled = 4
            OTHERS              = 5.

  jobc = global_job-jobcount.
  jobn = global_job-jobname.
  wa_number = wa_count.

  SUBMIT (c_prog)     AND RETURN
         WITH p_7jb    EQ it_7jb
         WITH p_log    EQ wa_number
         VIA JOB jobn NUMBER jobc .

  CONCATENATE 'IEQ' jobn     INTO s_jobnam.  APPEND s_jobnam.
  CALL FUNCTION 'JOB_CLOSE'
       EXPORTING
            jobcount  = jobc
            jobname   = jobn
            strtimmed = immediate
       EXCEPTIONS
            OTHERS    = 4.
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
*&      Form  DELETE_JOBLIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_joblist.

ENDFORM.                    " DELETE_JOBLIST

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

  " Update the ZTPP_PMT07JB_A..
  UPDATE ztpp_pmt07jb_b   SET: vinn  = l_vin
                        WHERE sqdt  = wa_7jb-sqdt
                          AND modl  = wa_7jb-modl
                          AND mtgu  = wa_7jb-mtgu
                          AND ssr1  = wa_7jb-ssr1 .
ENDFORM.                    " VIN_CODE_GEN
