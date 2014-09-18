************************************************************************
* Program Name      : ZRPP801R_CHECK_SEQUENCE
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Cehck the Sequence Quantity and Modify Quantity
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zrpp801r_check_sequence    NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: mara         ,
        zspp_sequence,
        ztpp_wosum.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_list OCCURS 0.
        INCLUDE STRUCTURE      zspp_sequence .
DATA: END OF it_list.

DATA: BEGIN OF it_collect OCCURS 0,
        worder            LIKE mara-matnr,
        sorder            LIKE ztpp_wosum-sales ,
        modqty            LIKE ztpp_wosum-modqty,
        seqqty            LIKE ztpp_wosum-seqqty,
        hd_qty            LIKE ztpp_wosum-modqty,
        hd_seq            LIKE ztpp_wosum-seqqty,
      END OF it_collect,
      it_ausp             LIKE TABLE OF ausp           WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE,
      it_bdcdata          LIKE TABLE OF bdcdata        WITH HEADER LINE,
      it_vals             LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

*----------------------------------------------------------------------*
* Working Variables AREA
*----------------------------------------------------------------------*
DATA: wa_record               TYPE i                          ,
      wa_create               TYPE i                          ,
      wa_update               TYPE i                          ,
      wa_error                TYPE i                          ,
      wa_msg(70)              TYPE c                          .

*----------------------------------------------------------------------*
* Constants AREA
*----------------------------------------------------------------------*
CONSTANTS: c_mark   VALUE 'X'.

************* NOT USED!!!! *****************************************
DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname,
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c    VALUE  'N'           ,
      it_rec                  LIKE TABLE OF mara       WITH HEADER LINE.

*DATA: package(5).
data: p_f_wo LIKE ztpp_wosum-wo_ser,
      p_t_wo LIKE ztpp_wosum-wo_ser.

********************************************************************

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b8 WITH FRAME TITLE text-801.
SELECT-OPTIONS: s_wo for ztpp_wosum-wo_ser NO-EXTENSION .
SELECTION-SCREEN END   OF BLOCK b8.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_list        RADIOBUTTON  GROUP rl   DEFAULT 'X' .
SELECTION-SCREEN COMMENT  (10) text-306 FOR FIELD p_list      .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: p_wc          RADIOBUTTON  GROUP rg               .
*SELECTION-SCREEN COMMENT  (40) text-103 FOR FIELD p_wc        .
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_all         RADIOBUTTON  GROUP rg   DEFAULT 'X' .
SELECTION-SCREEN COMMENT  (40) text-101 FOR FIELD p_all       .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_7jb         RADIOBUTTON  GROUP rg               .
SELECTION-SCREEN COMMENT  (40) text-102 FOR FIELD p_7jb       .
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: p_vehic       RADIOBUTTON  GROUP rg               .
*SELECTION-SCREEN COMMENT  (40) text-303 FOR FIELD p_vehic     .
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run         RADIOBUTTON  GROUP rl               .
SELECTION-SCREEN COMMENT  (30) text-307 FOR FIELD p_run       .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_vm          RADIOBUTTON GROUP ra                .
SELECTION-SCREEN COMMENT  (40) text-303 FOR FIELD p_vm        .
PARAMETERS: p_wo          RADIOBUTTON GROUP ra                .
SELECTION-SCREEN COMMENT  (20) text-308 FOR FIELD p_wo        .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_wosum       RADIOBUTTON GROUP ra   DEFAULT 'X'  .
SELECTION-SCREEN COMMENT  (40) text-305 FOR FIELD p_wosum     .
PARAMETERS: p_sorder      RADIOBUTTON GROUP ra                .
SELECTION-SCREEN COMMENT  (20) text-304 FOR FIELD p_sorder    .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: pt_vm         RADIOBUTTON GROUP rt               .
SELECTION-SCREEN COMMENT  (40) text-303 FOR FIELD pt_vm      .
PARAMETERS: pt_wo         RADIOBUTTON GROUP rt               .
SELECTION-SCREEN COMMENT  (20) text-308 FOR FIELD pt_wo      .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: pt_wosum      RADIOBUTTON GROUP rt               .
SELECTION-SCREEN COMMENT  (40) text-309 FOR FIELD pt_wosum   .
PARAMETERS: pt_sd         RADIOBUTTON GROUP rt   DEFAULT 'X' .
SELECTION-SCREEN COMMENT  (20) text-304 FOR FIELD pt_sd      .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b4.
SELECTION-SCREEN END   OF BLOCK b1.


INCLUDE zcpp103_common_routine .

AT SELECTION-SCREEN OUTPUT.
*S_PACK-LOW = 'E0   '.
*S_PACK-HIGH = 'E0   '.
*S_PACK-SIGN = 'I'.
*S_PACK-OPTION = 'EQ'.
*APPEND S_PACK.

START-OF-SELECTION.
  PERFORM select_wo_range      .
  PERFORM list_processing      .
  PERFORM error_collect        .
  PERFORM collect_list         .

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
FORM call_function .
  CALL FUNCTION 'Z_FPP_CHECK_WO_SEQUENCE'
       EXPORTING
            check      = p_all
            f_wo       = p_f_wo
            t_wo       = p_t_wo
       TABLES
            t_sequence = it_list.

  DESCRIBE TABLE it_list LINES wa_record.
ENDFORM.                               " CALL_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
FORM write_result         .
  DATA: l_cnt      TYPE  i       ,
        l_line     LIKE  sy-tabix.

  CLEAR: l_line.
  DESCRIBE TABLE it_list LINES l_cnt.
  IF l_cnt > 0.
    PERFORM header_format             .
    LOOP AT it_list WHERE flag = 'X'  .
      l_line = l_line + 1  .
      l_line = l_line MOD 2.
      IF l_line EQ 0.
        FORMAT INTENSIFIED ON.
      ELSE.
        FORMAT INTENSIFIED OFF.
      ENDIF.
      PERFORM body_format     .
    ENDLOOP.
    ULINE AT: /(132).
  ELSE.
    WRITE AT: /001(100) text-010 .
  ENDIF.
ENDFORM.                    " WRITE_RESULT

*&---------------------------------------------------------------------*
*&      Form  HEADER_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header_format.
  SKIP 2          .
  WRITE AT: /001(132) text-001.
  WRITE AT: /001(023) text-006,
             032(010) wa_record.

  IF pt_wo  = 'X' AND p_run = 'X'.
    WRITE AT: /001(031) text-007,
               032(010) wa_create.
    WRITE AT: /001(031) text-017,
               032(010) wa_update.
    WRITE AT: /001(031) text-041,
               032(010) wa_error .
  ENDIF.
  IF pt_wosum = 'X' AND p_run = 'X'.
    WRITE AT: /001(031) text-008,
               032(010) wa_create.
    WRITE AT: /001(031) text-018,
               032(010) wa_update.
    WRITE AT: /001(031) text-041,
               032(010) wa_error .
  ENDIF.
  IF pt_sd   = 'X' AND p_run = 'X'.
    WRITE AT: /001(031) text-009,
               032(010) wa_create.
    WRITE AT: /001(031) text-019,
               032(010) wa_update.
    WRITE AT: /001(031) text-041,
               032(010) wa_error .
  ENDIF.
  IF pt_vm  = 'X' AND p_run = 'X'.
    WRITE AT: /001(031) text-011,
               032(010) wa_create.
    WRITE AT: /001(031) text-021,
               032(010) wa_update.
    WRITE AT: /001(031) text-041,
               032(010) wa_error .
  ENDIF.

  SKIP 1          .
  ULINE AT: /(132).
  WRITE AT: /001(018) text-201 ,
             019(001) sy-vline                        ,
             020(010) text-202 ,
             030(001) sy-vline                        ,
             031(016) text-204 ,
             047(001) sy-vline                        ,
             048(016) text-206 ,
             064(001) sy-vline                        ,
             065(016) text-208 ,
             081(001) sy-vline                        ,
             082(016) text-205 ,
             098(001) sy-vline                        ,
             099(016) text-207 ,
             115(001) sy-vline                        ,
             116(016) text-209 ,
             132(001) sy-vline                        .
  ULINE AT: /(132).
ENDFORM.                    " HEADER_FORMAT

*&---------------------------------------------------------------------*
*&      Form  BODY_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM body_format.
  WRITE AT: /001(018) it_list-worder  COLOR COL_KEY   ,
             019(001) sy-vline                        ,
             020(010) it_list-sorder  COLOR COL_KEY   ,
             030(001) sy-vline                        ,
             031(016) it_list-cl_cnt  COLOR COL_NORMAL,
             047(001) sy-vline                        ,
             048(016) it_list-wo_cnt  COLOR COL_NORMAL,
             064(001) sy-vline                        ,
             065(016) it_list-so_cnt  COLOR COL_NORMAL,
             081(001) sy-vline                        ,
             082(016) it_list-cl_seq  COLOR COL_TOTAL ,
             098(001) sy-vline                        ,
             099(016) it_list-wo_seq  COLOR COL_TOTAL ,
             115(001) sy-vline                        ,
             116(016) it_list-so_seq  COLOR COL_TOTAL ,
             132(001) sy-vline                        .
ENDFORM.                    " BODY_FORMAT

*&---------------------------------------------------------------------*
*&      Form  error_collect
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_collect.
  CHECK p_run = 'X'      .

  PERFORM call_function        .
  PERFORM check_exist          .

  " Check the Source data.
  CLEAR: it_collect, it_collect[].

  IF p_vm     = 'X'.
*   PERFORM gather_vehicle                .
  ENDIF.
  IF p_wo     = 'X'.
    LOOP AT it_list.
      it_collect-worder = it_list-worder .
      it_collect-sorder = it_list-sorder .
      it_collect-modqty = it_list-cl_cnt .
      it_collect-seqqty = it_list-cl_seq .
      it_collect-hd_qty = it_list-hd_cnt .
      it_collect-hd_seq = it_list-hd_seq .
      APPEND it_collect.
    ENDLOOP.
  ENDIF.
  IF p_wosum  = 'X'.
    LOOP AT it_list.
      it_collect-worder = it_list-worder .
      it_collect-sorder = it_list-sorder .
      it_collect-modqty = it_list-wo_cnt .
      it_collect-seqqty = it_list-wo_seq .
      SELECT SUM( modqty ) SUM( seqqty )
        INTO (it_collect-hd_qty, it_collect-hd_seq)
        FROM ztpp_wosum
       WHERE wo_ser = it_list-worder(9)
         AND nation = it_list-worder+9(3)
         AND dealer = it_list-worder+12(2) .
      APPEND it_collect.
    ENDLOOP.
  ENDIF.
  IF p_sorder = 'X' .
    LOOP AT it_list.
      it_collect-worder = it_list-worder .
      it_collect-sorder = it_list-sorder .
      it_collect-modqty = it_list-so_cnt .
      it_collect-seqqty = it_list-so_seq .
      APPEND it_collect.
    ENDLOOP.
  ENDIF.

  " Check the Target data.
  IF pt_vm     = 'X'.
    CHECK p_vm NE 'X' .
*   PERFORM collect_vehicle               .
  ENDIF.
  IF pt_wo     = 'X'.
    CHECK p_wo    NE 'X' .
    PERFORM collect_wocolor               .
    wa_update = wa_create                 .
  ENDIF.
  IF pt_wosum  = 'X'.
    CHECK p_wosum NE 'X' .
    PERFORM collect_wosum                 .
  ENDIF.
  IF pt_sd    = 'X' .
    CHECK p_sorder NE 'X' .
    DELETE it_collect WHERE sorder = space   .
    CLEAR: wa_update.
    PERFORM collect_sorder                .
  ENDIF.
ENDFORM.                    " error_collect

*&---------------------------------------------------------------------*
*&      Form  gather_vehicle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gather_vehicle.

ENDFORM.                    " gather_vehicle

*&---------------------------------------------------------------------*
*&      Form  collect_vehicle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_vehicle.
  LOOP AT it_collect.
  ENDLOOP.
ENDFORM.                    " collect_vehicle

*&---------------------------------------------------------------------*
*&      Form  collect_wosum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_wosum.
  data: l_mara  LIKE mara-matnr,
        l_modqty like ztpp_wosum-modqty,
        l_seqqty like ztpp_wosum-seqqty.

  LOOP AT it_collect.
    l_mara = it_collect-worder.

*    *l_val10  = it_collect-seqqty .
*    *l_val20  = it_collect-modqty - it_collect-seqqty .

    select single modqty seqqty into (l_modqty, l_seqqty)
               from ztpp_wosum
               WHERE  wo_ser = l_mara(9)
                      AND nation = l_mara+9(3)
                      AND dealer = l_mara+12(2)
                      AND extc   = l_mara+14(2)
                      AND intc   = l_mara+16(2) .

    if sy-subrc eq 0.
       if l_modqty <> it_collect-modqty
       or l_seqqty <> it_collect-seqqty.
          UPDATE ztpp_wosum  SET: modqty    = it_collect-modqty
                        seqqty    = it_collect-seqqty
                    WHERE wo_ser = l_mara(9)
                      AND nation = l_mara+9(3)
                      AND dealer = l_mara+12(2)
                      AND extc   = l_mara+14(2)
                      AND intc   = l_mara+16(2) .
       endif.
    endif.

  ENDLOOP.
  commit work.
ENDFORM.                    " collect_wosum

*&---------------------------------------------------------------------*
*&      Form  collect_sorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_sorder.
  DATA: l_cnt(5)              TYPE n           ,
        l_error(5)            TYPE n           ,
        l_val10               TYPE i           ,
        l_val20               TYPE i           ,
        l_sorder              LIKE vbap-vbeln  ,
        l_kwmeng10            LIKE vbap-kwmeng ,
        l_kwmeng20             LIKE vbap-kwmeng .

  DATA: it_message LIKE TABLE OF bapiret2 WITH HEADER LINE.

  DESCRIBE TABLE it_collect LINES l_cnt.
  LOOP AT it_collect.
    l_sorder = it_collect-sorder .
    l_val10  = it_collect-seqqty .
    l_val20  = it_collect-modqty - it_collect-seqqty .

    if l_val20 < 0.
       WRITE: /'Work order --' ,it_collect-worder,' Mod Qty < Seq Qty'.
       continue.
    endif.
    " Check the Sales Order Quantity
    CLEAR: l_kwmeng10, l_kwmeng20.
    SELECT SINGLE kwmeng INTO l_kwmeng20
      FROM vbap
     WHERE vbeln = it_collect-sorder
       AND posnr = '000020'         .
*    IF l_kwmeng20 = l_val20           .
      SELECT SINGLE kwmeng INTO l_kwmeng10
        FROM vbap
       WHERE vbeln = it_collect-sorder
         AND posnr = '000010'         .
      IF l_kwmeng10 = l_val10 and l_kwmeng20 = l_val20.
        CONTINUE .
      ENDIF.
*    ENDIF.

    " Change the Sales Order
    PERFORM change_sorder_bapi TABLES it_message
                               USING l_sorder l_kwmeng10 l_kwmeng20
                                     l_val10 l_val20.

*    PERFORM bdc_dynpro_processing USING :
*                           'X'  'SAPMV45A'             '0102',
*                           ' '  'BDC_OKCODE'           '=UER2' ,
*                           ' '  'VBAK-VBELN'            l_sorder.
*
*    IF l_val10 = 0 AND l_kwmeng > 0 or
*       l_val20 = 0 AND l_kwmeng1 > 0.
*      PERFORM bdc_dynpro_processing USING :
*                           'X'  'SAPMV45A'             '4001',
*                           ' '  'BDC_OKCODE'           '=ITEM' ,
*                           ' '  'BDC_CURSOR'           'VBAP-POSNR(01)'
*    ,
*                           ' '  'RV45A-VBAP_SELKZ(01)' 'X'     ,
*
*                           'X'  'SAPMV45A'             '4003',
*                           ' '  'BDC_OKCODE'           '=T\07' ,
*
*                           'X'  'SAPMV45A'             '4003',
*                           ' '  'BDC_OKCODE'           '=MKAP' ,
*
*                           'X'  'SAPMV45A'             '4003',
*                           ' '  'BDC_OKCODE'           '=EILO' ,
*
*                           'X'  'SAPMV45A'             '4003',
*                           ' '  'BDC_OKCODE'           '/EBACK',
*
*                           'X'  'SAPMV45A'             '4001',
*                           ' '  'BDC_OKCODE'           '=SICH' ,
*                           ' '  'RV45A-KWMENG(01)'      l_val10,
*                           ' '  'RV45A-KWMENG(02)'      l_val20.
*    ELSE.
*      PERFORM bdc_dynpro_processing USING :
*                           'X'  'SAPMV45A'             '4001',
*                           ' '  'BDC_OKCODE'           '=SICH' ,
*                           ' '  'RV45A-KWMENG(01)'      l_val10,
*                           ' '  'RV45A-KWMENG(02)'      l_val20.
*    ENDIF.
*
*    CALL TRANSACTION 'VA02'  USING it_bdcdata MODE p_cmode
*                             MESSAGES INTO    it_msg    .
*
    LOOP AT it_message.
      IF it_message-type = 'E' OR
         it_message-type = 'A'.
        wa_update = wa_update + 1        .
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  msgid               = it_message-id
                  msgnr               = it_message-number
                  msgv1               = it_message-message_v1
                  msgv2               = it_message-message_v2
                  msgv3               = it_message-message_v3
                  msgv4               = it_message-message_v4
             IMPORTING
                  message_text_output = wa_msg.
        WRITE: /'Sales Order -- ' , wa_msg .
      ENDIF.
    ENDLOOP.
*
    CLEAR: it_bdcdata, it_bdcdata[], it_message, it_message[].
  ENDLOOP.


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.
* IMPORTING
*   RETURN        =

ENDFORM.                    " collect_sorder

*&---------------------------------------------------------------------*
*&      Form  collect_wocolor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_wocolor.
  DATA: l_matnr               LIKE mara-matnr.

  LOOP AT it_collect.
    " Color Collection...
    CLEAR: l_matnr, it_vals, it_vals[].
    l_matnr = it_collect-worder .

    SELECT SINGLE *
      FROM mara
     WHERE matnr = l_matnr
       AND mtart = 'WOCL'  .

    IF sy-subrc =  0 .
      it_vals-atnam = 'P_SEQ_QTY'.  it_vals-atwrt = it_collect-seqqty .
      APPEND it_vals.
      it_vals-atnam = 'P_MOD_QTY'.  it_vals-atwrt = it_collect-modqty .
      APPEND it_vals.

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object             = l_matnr
          mode               = 'W'
          ctype              = '001'
*       DISPLAY            = 'D'
        TABLES
          val_table          = it_vals
        EXCEPTIONS
          no_data            = 1
          error_mode         = 2
          error_object       = 3
          OTHERS             = 4 .
    ENDIF.

    " Head Collection...
    CLEAR: l_matnr, it_vals, it_vals[].
    l_matnr = it_collect-worder(14) .

    SELECT SINGLE *
      FROM mara
     WHERE matnr = l_matnr
** changed by Furong on 09/24/05
*       AND mtart = 'WOCL'  .
       AND mtart = 'WOHD'.
** end of change
    IF sy-subrc =  0 .
      it_vals-atnam = 'P_SEQ_QTY'.
** changed by Furong on 09/24/05
*      it_vals-atwrt = it_collect-seqqty .
      it_vals-atwrt = it_collect-hd_seq .
** end of change
      APPEND it_vals.
      it_vals-atnam = 'P_MOD_QTY'.
** changed by Furong on 09/24/05
*      it_vals-atwrt = it_collect-modqty .
      it_vals-atwrt = it_collect-hd_qty .
** end of change
      APPEND it_vals.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object             = l_matnr
          mode               = 'W'
          ctype              = '001'
*       DISPLAY            = 'D'
        TABLES
          val_table          = it_vals
        EXCEPTIONS
          no_data            = 1
          error_mode         = 2
          error_object       = 3
          OTHERS             = 4 .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " collect_wocolor

*&---------------------------------------------------------------------*
*&      Form  list_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_processing.
  CHECK p_list = 'X'.
  PERFORM call_function        .
  PERFORM write_result         .
ENDFORM.                    " list_processing

*&---------------------------------------------------------------------*
*&      Form  collect_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_list.
  CHECK p_run = 'X' .
  PERFORM call_function        .
  PERFORM write_result_run     .
ENDFORM.                    " collect_list

*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT_RUN
*&---------------------------------------------------------------------*
FORM write_result_run     .
  DATA: l_line     TYPE  i       ,
        l_cnt      TYPE  i       .

  DESCRIBE TABLE it_list LINES l_cnt.
  IF l_cnt > 0.
    PERFORM header_format             .
    LOOP AT it_list WHERE flag = 'X'  .
      l_line = l_line + 1  .
      l_line = l_line MOD 2.
      IF l_line EQ 0.
        FORMAT INTENSIFIED ON.
      ELSE.
        FORMAT INTENSIFIED OFF.
      ENDIF.
      PERFORM body_format     .
    ENDLOOP.
    ULINE AT: /(132).
  ELSE.
    WRITE AT: /001(100) text-010 .
  ENDIF.
ENDFORM.                    " WRITE_RESULT_RUN

*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_exist.
  CLEAR: wa_create.
  IF pt_vm = 'X'.
    LOOP AT it_list WHERE vm_exist NE 'X'.
      wa_create = wa_create + 1.
    ENDLOOP.
  ENDIF.
  IF pt_wo = 'X'.
    LOOP AT it_list WHERE cl_exist NE 'X'.
      wa_create = wa_create + 1.
    ENDLOOP.
  ENDIF.
  IF pt_wosum = 'X'.
    LOOP AT it_list WHERE wo_exist NE 'X'.
      wa_create = wa_create + 1.
    ENDLOOP.
  ENDIF.
  IF pt_sd = 'X'.
    LOOP AT it_list WHERE so_exist NE 'X'.
      wa_create = wa_create + 1.
    ENDLOOP.
  ENDIF.
  wa_create = wa_record - wa_create .
ENDFORM.                    " CHECK_EXIST
*&---------------------------------------------------------------------*
*&      Form  select_wo_range
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_wo_range.
*  IF s_pack-low IS INITIAL AND s_pack-high IS INITIAL.
*    p_f_wo = 'E00000000'.
*    p_t_wo = 'EZZZZZZZZ'.
*  ELSE.
*    IF s_pack-low IS INITIAL.
*      p_f_wo = 'E00000000'.
*      CONCATENATE s_pack-high 'ZZZZ' INTO p_t_wo.
*    ELSEIF s_pack-high IS INITIAL.
*      CONCATENATE s_pack-low '0000' INTO p_f_wo.
*      CONCATENATE s_pack-low 'ZZZZ' INTO p_t_wo.
*    ELSE.
*      CONCATENATE s_pack-low '0000' INTO p_f_wo.
*      CONCATENATE s_pack-high 'ZZZZ' INTO p_t_wo.
*    ENDIF.
*  ENDIF.
 IF s_wo-low IS INITIAL AND s_wo-high IS INITIAL.
    p_f_wo = 'E00000000'.
    p_t_wo = 'EZZZZZZZZ'.
  ELSE.
    IF s_wo-low IS INITIAL.
      p_f_wo = 'E00000000'.
      p_t_wo = s_wo-high.
    ELSEIF s_wo-high IS INITIAL.
      p_f_wo = s_wo-low.
      p_t_wo = s_wo-low.
    ELSE.
      p_f_wo = s_wo-low.
      p_t_wo = s_wo-high.
    ENDIF.
  ENDIF.

ENDFORM.                    " select_wo_range
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SORDER_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KWMENG1  text
*      -->P_L_KWMENG  text
*      -->P_L_VAL10  text
*      -->P_L_VAL20  text
*      -->P_TABLE  text
*      -->P_IT_MESSAGE  text
*----------------------------------------------------------------------*
FORM change_sorder_bapi TABLES   l_it_return STRUCTURE bapiret2
                        USING    pa_order
                                 p_item10_org
                                 p_item20_org
                                 p_item10_qty
                                 p_item20_qty.

  DATA : l_it_ord_header_inx LIKE TABLE OF bapisdh1x  WITH HEADER LINE,
           l_bapisdls          LIKE bapisdls ,
*         l_it_return         LIKE TABLE OF bapiret2   WITH HEADER LINE,
          l_it_itm            LIKE TABLE OF bapisditm  WITH HEADER LINE,
          l_it_itmx           LIKE TABLE OF bapisditmx WITH HEADER LINE,
          l_it_lines          LIKE TABLE OF bapischdl  WITH HEADER LINE,
          l_it_linesx         LIKE TABLE OF bapischdlx WITH HEADER LINE.

*  DATA : p_item10_org         LIKE vbap-kwmeng,
*         p_item20_org         LIKE vbap-kwmeng,
*         p_item20_qty         LIKE ztpp_wosum-modqty,
*         p_item10_qty         LIKE ztpp_wosum-seqqty.

  DATA:   l_stot               LIKE vbap-kwmeng,
          l_wtot               LIKE ztpp_wosum-seqqty,
          l_qty                TYPE i                ,
          l_item10_flg(01),
          l_item20_flg(01),
          l_item10_qty_flg(01),
          l_item20_qty_flg(01),
          pa_flag.

*  pa_order LIKE vbep-vbeln .
*  p_item10_org = 0.
*  p_item20_org = 1.
*  pa_order = '2050100318'.
*  p_item10_qty = 2.
*  p_item20_qty = 0.

  IF p_item10_org =  0     .
    l_item10_flg     = 'I'   .
    l_item10_qty_flg = 'I'   .
    IF p_item20_org = 0    .
      l_item20_flg     = 'I'   .    " 'U' ??
      l_item20_qty_flg = 'I'   .
    ELSEIF p_item20_qty = 0    .
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'D'   .
    ELSE.
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'U'   .
    ENDIF.
  ELSEIF p_item10_qty = 0    .
    l_item10_flg     = 'U'   .
    l_item10_qty_flg = 'D'   .
    IF p_item20_org = 0    .
      l_item20_flg     = 'I'   .    " 'U' ??
      l_item20_qty_flg = 'I'   .
    ELSEIF p_item20_qty = 0    .
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'D'   .
    ELSE.
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'U'   .
    ENDIF.
  ELSE.
    l_item10_flg     = 'U'   .
    l_item10_qty_flg = 'U'   .
    IF p_item20_org = 0    .
      l_item20_flg     = 'I'   .    " 'U' ??
      l_item20_qty_flg = 'I'   .
    ELSEIF p_item20_qty = 0    .
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'D'   .
    ELSE.
      l_item20_flg     = 'U'   .
      l_item20_qty_flg = 'U'   .
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

  p_item10_org = p_item10_qty.
  p_item20_org = p_item20_qty.

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

ENDFORM.                    " CHANGE_SORDER_BAPI
