************************************************************************
* Program Name      : ZIPP102I_SEQ_REPROCESSING
* Author            : Bobby
* Creation Date     : 2004.02.08.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No :
* Addl Documentation:
* Description       : Re-Processing of Sequencing(All Recovery)
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zipp102i_seq_reprocessing    MESSAGE-ID zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_common_vals,
        equi ,
        ausp .

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
      wa_error                TYPE c                          ,
      wa_flag                 TYPE c                          ,
      wa_date                 TYPE d                          ,
      wa_err_hd               TYPE c                          ,
      wa_mode                 TYPE c   VALUE   'N'            ,
      wa_subrc                LIKE sy-subrc                   ,
      wa_plnum                LIKE plaf-plnum                 ,
      wa_equnr                LIKE equi-equnr                 ,
      wa_matnr                LIKE mara-matnr                 ,
      wa_sorder               LIKE vbak-vbeln                 ,
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
DATA:   matnr                 LIKE mara-matnr,
      END OF it_vin                          ,
      it_char            LIKE TABLE OF zspp_vin_value  WITH HEADER LINE,
      it_msg             LIKE TABLE OF bdcmsgcoll      WITH HEADER LINE,
      it_data            LIKE TABLE OF ztpp_seq_backup WITH HEADER LINE.


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_run    TYPE c  DEFAULT ' ' .
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  IF p_run = 'X'.
    PERFORM get_wkdata.

    SORT it_data BY tables worder fname vals.
    PERFORM rerun_process_1.         " Sales Order Recovery
    IF wa_error = 'X'      .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      EXIT .
    ELSE.
*      DELETE FROM ztpp_seq_backup WHERE tables = 'VBAP' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM rerun_process_2.         " Plan Order Recovery
    IF wa_error = 'X'      .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      EXIT .
    ELSE.
*      DELETE FROM ztpp_seq_backup WHERE tables = 'PLAF' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM rerun_process_3.         " ZTPP_COMMON_VALS Recovery
    IF wa_error = 'X'        .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      EXIT .
    ELSE.
*      DELETE FROM ztpp_seq_backup WHERE tables = 'ZTPP_COMMON_VALS' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM rerun_process_4.           " ZTPP_WOSUM Recovery
    IF wa_error = 'X'        .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      EXIT .
    ELSE.
*      DELETE FROM ztpp_seq_backup WHERE tables = 'ZTPP_WOSUM' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM rerun_process_5.         " Work Order Header Recovery
    IF wa_error = 'X'        .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      EXIT .
    ELSE.
*      DELETE FROM ztpp_seq_backup WHERE tables = 'WOHD' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM rerun_process_6.         " Work Order Color Recovery
    IF wa_error = 'X'        .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      EXIT .
    ELSE.
*      DELETE FROM ztpp_seq_backup WHERE tables = 'WOCL' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM rerun_process_7.         " VIN Generation...
    IF wa_error = 'X'        .
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      EXIT .
    ELSE.
*      DELETE FROM ztpp_seq_backup WHERE tables = 'PLAF' .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
    ENDIF.

    PERFORM write_result     .
  ENDIF.


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

  DESCRIBE TABLE it_data    LINES l_lines.
  WRITE AT:/001(020) text-015 ,
            022(010) l_lines  .
ENDFORM.                    " WRITE_RESULT


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
  DATA : l_bapisdls          LIKE bapisdls ,
         l_it_ord_header_inx LIKE TABLE OF bapisdh1x  WITH HEADER LINE,
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

  LOOP AT l_it_return.
    IF l_it_return-type = 'E' OR
       l_it_return-type = 'A'   .
      wa_error = 'X'           .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " call_bapi_salesorder

*&---------------------------------------------------------------------*
*&      Form  get_wkdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wkdata.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_seq_backup .
ENDFORM.                    " get_wkdata

*&---------------------------------------------------------------------*
*&      Form  RERUN_PROCESS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rerun_process_1.
  DATA: l_idx            TYPE i           ,
        l_qty            TYPE p DECIMALS 3,
        l_val10(8)       TYPE n           ,
        l_val20(8)       TYPE n           ,
        l_sorder         LIKE vbap-vbeln.

  " Sales Order Recovery
  LOOP AT it_data WHERE tables = 'VBAP' .
    l_idx = l_idx + 1  .
    IF l_sorder IS INITIAL.
      l_sorder = it_data-worder(10) .
      CLEAR: l_val10, l_val20       .
    ENDIF.
    IF l_sorder = it_data-worder(10) .
      CASE it_data-fname .
        WHEN '000010' .
          l_val10 = l_qty = it_data-vals.
        WHEN '000020' .
          l_val20 = l_qty = it_data-vals.
      ENDCASE.
      CONTINUE.
    ELSE.
      PERFORM call_bapi_salesorder USING l_sorder l_val10 l_val20 .
      CLEAR: l_val10, l_val20 .
      CASE it_data-fname .
        WHEN '000010' .
          l_val10 = l_qty = it_data-vals.
        WHEN '000020' .
          l_val20 = l_qty = it_data-vals.
      ENDCASE.
      l_sorder = it_data-worder(10) .
    ENDIF.
  ENDLOOP.

  IF l_idx > 0 .
    PERFORM call_bapi_salesorder USING l_sorder l_val10 l_val20 .
  ENDIF.
ENDFORM.                    " RERUN_PROCESS_1

*&---------------------------------------------------------------------*
*&      Form  RERUN_PROCESS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rerun_process_2.
  DATA: lw_plaf            LIKE plaf      ,
        l_rtcode           TYPE c         ,
        l_porder           LIKE plaf-plnum.

  " Plan Order Recovery
  LOOP AT it_data WHERE tables = 'PLAF' .
    l_porder = it_data-vals .

    SELECT SINGLE * INTO lw_plaf
      FROM plaf
     WHERE plnum = l_porder.

    IF sy-subrc = 0 .
      CLEAR: l_rtcode .
      PERFORM call_bapi_planorder USING l_porder l_rtcode .
      IF l_rtcode = 'E' .
        wa_error = 'X'  .
      ELSE.
        DELETE it_data .
      ENDIF.
    ELSE.
      DELETE it_data .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " RERUN_PROCESS_2

*&---------------------------------------------------------------------*
*&      Form  RERUN_PROCESS_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rerun_process_3.
  DATA: lw_common        LIKE ztpp_common_vals,
        l_idx            TYPE i               ,
        l_name(30)       TYPE c               ,
        l_worder         LIKE it_data-worder  .
  FIELD-SYMBOLS: <name>  TYPE ANY.

  " ZTPP_COMMON_VALS Recovery
  READ TABLE it_data WITH KEY tables = 'ZTPP_COMMON_VALS' .
  l_worder = it_data-worder.

  LOOP AT it_data WHERE tables = 'ZTPP_COMMON_VALS' .
    l_idx = l_idx + 1  .
    IF <name> IS ASSIGNED.
      UNASSIGN <name>   .
    ENDIF.
    IF l_worder = it_data-worder.
      CONCATENATE 'LW_COMMON-' it_data-fname INTO l_name.
      IF it_data-fname NE space .
        ASSIGN (l_name)  TO <name>.
        <name> = it_data-vals   .
      ENDIF.
      CONTINUE.
    ENDIF.
    lw_common-jobs   = 'ZIPP101U_PMT07JB_A'.
    MODIFY ztpp_common_vals FROM lw_common .
    CLEAR: lw_common .
    l_worder = it_data-worder.
    CONCATENATE 'LW_COMMON-' it_data-fname INTO l_name.
    IF it_data-fname NE space .
      ASSIGN (l_name)  TO <name>.
      <name> = it_data-vals   .
    ENDIF.
  ENDLOOP.

  IF l_idx > 0 .
    lw_common-jobs   = 'ZIPP101U_PMT07JB_A'.
    MODIFY ztpp_common_vals FROM lw_common .
  ENDIF.
ENDFORM.                    " RERUN_PROCESS_3

*&---------------------------------------------------------------------*
*&      Form  RERUN_PROCESS_4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rerun_process_4.
  DATA: lw_wosum         LIKE ztpp_wosum      ,
        l_wosum          LIKE TABLE OF ztpp_wosum WITH HEADER LINE,
        l_idx            TYPE i               ,
        l_name(30)       TYPE c               ,
        l_worder         LIKE it_data-worder  .

  " ZTPP_WOSUM Recovery
  READ TABLE it_data WITH KEY tables = 'ZTPP_WOSUM' .
  l_worder = it_data-worder.
** furong changed on 10/31/05
*  LOOP AT it_data WHERE tables = 'ZTPP_COMMON_VALS' .
  LOOP AT it_data WHERE tables = 'ZTPP_WOSUM' .
** end of change
    l_idx = l_idx + 1 .
    IF l_worder = it_data-worder.
      CASE it_data-fname  .
        WHEN 'SEQQTY'     .
          lw_wosum-seqqty      = it_data-vals.
        WHEN 'PLANQTY'    .
          lw_wosum-planqty     = it_data-vals.
        WHEN 'FORECASTQTY'.
          lw_wosum-forecastqty = it_data-vals.
        WHEN 'MITUQTY'    .
          lw_wosum-mituqty     = it_data-vals.
      ENDCASE.
      CONTINUE.
    ENDIF.
** changed by furong on 11/16/05
*    lw_wosum-wo_ser = it_data-worder(09)   .
*    lw_wosum-nation = it_data-worder+9(3)  .
*    lw_wosum-dealer = it_data-worder+12(2) .
*    lw_wosum-extc   = it_data-worder+14(2) .
*    lw_wosum-intc   = it_data-worder+16(2) .
    lw_wosum-wo_ser = l_worder(09)   .
    lw_wosum-nation = l_worder+9(3)  .
    lw_wosum-dealer = l_worder+12(2) .
    lw_wosum-extc   = l_worder+14(2) .
    lw_wosum-intc   = l_worder+16(2) .

*    MODIFY ztpp_wosum   FROM lw_wosum      .
    SELECT * INTO TABLE l_wosum FROM ztpp_wosum
                      WHERE wo_ser = lw_wosum-wo_ser
                        AND nation = lw_wosum-nation
                        AND dealer = lw_wosum-dealer
                        AND extc = lw_wosum-extc
                        AND intc = lw_wosum-intc.
    READ TABLE l_wosum INDEX 1.
    l_wosum-seqqty = lw_wosum-seqqty.
    l_wosum-planqty = lw_wosum-planqty.
    l_wosum-forecastqty = lw_wosum-forecastqty.
    l_wosum-mituqty = lw_wosum-mituqty.

    MODIFY ztpp_wosum  FROM l_wosum.

    CLEAR: lw_wosum, l_wosum .
    l_worder = it_data-worder.
    CASE it_data-fname  .
      WHEN 'SEQQTY'     .
        lw_wosum-seqqty      = it_data-vals.
      WHEN 'PLANQTY'    .
        lw_wosum-planqty     = it_data-vals.
      WHEN 'FORECASTQTY'.
        lw_wosum-forecastqty = it_data-vals.
      WHEN 'MITUQTY'    .
        lw_wosum-mituqty     = it_data-vals.
    ENDCASE.
    CLEAR l_idx.
  ENDLOOP.

  IF l_idx > 0 .
    lw_wosum-wo_ser = it_data-worder(09)   .
    lw_wosum-nation = it_data-worder+9(3)  .
    lw_wosum-dealer = it_data-worder+12(2) .
    lw_wosum-extc   = it_data-worder+14(2) .
    lw_wosum-intc   = it_data-worder+16(2) .

    SELECT * INTO TABLE l_wosum FROM ztpp_wosum
                  WHERE wo_ser = lw_wosum-wo_ser
                    AND nation = lw_wosum-nation
                    AND dealer = lw_wosum-dealer
                    AND extc = lw_wosum-extc
                    AND intc = lw_wosum-intc.

    READ TABLE l_wosum INDEX 1.
    l_wosum-seqqty = lw_wosum-seqqty.
    l_wosum-planqty = lw_wosum-planqty.
    l_wosum-forecastqty = lw_wosum-forecastqty.
    l_wosum-mituqty = lw_wosum-mituqty.

    MODIFY ztpp_wosum  FROM l_wosum.

  ENDIF.
ENDFORM.                    " RERUN_PROCESS_4

*&---------------------------------------------------------------------*
*&      Form  RERUN_PROCESS_5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rerun_process_5.
  DATA: l_idx            TYPE i         ,
        l_worder         LIKE mara-matnr,
        l_vals           LIKE TABLE OF zspp_vin_value  WITH HEADER LINE.

  READ TABLE it_data WITH KEY tables = 'WOHD' .
  l_worder = it_data-worder.
  CLEAR: l_vals, l_vals[].

  LOOP AT it_data WHERE tables = 'WOHD'.
    l_idx = l_idx + 1           .
    IF l_worder = it_data-worder.
      l_vals-atnam = it_data-fname  .
      l_vals-atwrt = it_data-vals   .
      APPEND l_vals                 .
      CONTINUE.
    ENDIF.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_worder
              mode         = 'W'
              ctype        = '001'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    l_worder = it_data-worder.
    CLEAR: l_vals, l_vals[].
    l_vals-atnam = it_data-fname  .
    l_vals-atwrt = it_data-vals   .
    APPEND l_vals                 .
  ENDLOOP.

  IF l_idx > 0 .
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_worder
              mode         = 'W'
              ctype        = '001'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.
  ENDIF.
ENDFORM.                    " RERUN_PROCESS_5

*&---------------------------------------------------------------------*
*&      Form  RERUN_PROCESS_6
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rerun_process_6.
  DATA: l_idx            TYPE i         ,
        l_worder         LIKE mara-matnr,
        l_vals           LIKE TABLE OF zspp_vin_value  WITH HEADER LINE.

  READ TABLE it_data WITH KEY tables = 'WOCL' .
  l_worder = it_data-worder.
  CLEAR: l_vals, l_vals[].

  LOOP AT it_data WHERE tables = 'WOCL'.
    l_idx = l_idx + 1           .
    IF l_worder = it_data-worder.
      l_vals-atnam = it_data-fname  .
      l_vals-atwrt = it_data-vals   .
      APPEND l_vals                 .
      CONTINUE.
    ENDIF.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_worder
              mode         = 'W'
              ctype        = '001'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    l_worder = it_data-worder.
    CLEAR: l_vals, l_vals[].
    l_vals-atnam = it_data-fname  .
    l_vals-atwrt = it_data-vals   .
    APPEND l_vals                 .
  ENDLOOP.

  IF l_idx > 0 .
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_worder
              mode         = 'W'
              ctype        = '001'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.
  ENDIF.
ENDFORM.                    " RERUN_PROCESS_6

*&---------------------------------------------------------------------*
*&      Form  RERUN_PROCESS_7
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rerun_process_7.
  DATA: lw_common        LIKE ztpp_common_vals,
        l_idx            TYPE i               ,
        l_name(30)       TYPE c               ,
        l_worder         LIKE it_data-worder  .
  FIELD-SYMBOLS: <name>  TYPE ANY.

  " ZTPP_COMMON_VALS Recovery
  READ TABLE it_data WITH KEY tables = 'VIN_GEN'.
  l_worder = it_data-worder.

  LOOP AT it_data WHERE tables = 'VIN_GEN'      .
    l_idx = l_idx + 1    .
    IF <name> IS ASSIGNED.
      UNASSIGN <name>    .
    ENDIF.
    IF l_worder = it_data-worder.
      lw_common-item4  = it_data-vals .
      CONTINUE.
    ENDIF.
    lw_common-jobs   = 'Z_FPP_VIN_GENERATION'.
    lw_common-key2   = l_worder              .
    lw_common-key3   = '******************'  .
    MODIFY ztpp_common_vals FROM lw_common .
    CLEAR: lw_common .
    l_worder = it_data-worder.
    lw_common-item4  = it_data-vals .
  ENDLOOP.

  IF l_idx > 0 .
    lw_common-jobs   = 'Z_FPP_VIN_GENERATION'.
    lw_common-key2   = l_worder              .
    lw_common-key3   = '******************'  .
    MODIFY ztpp_common_vals FROM lw_common   .
  ENDIF.
ENDFORM.                    " RERUN_PROCESS_7

*&---------------------------------------------------------------------*
*&      Form  call_bapi_PLANORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PORDER  text
*      -->P_L_RTCODE  text
*----------------------------------------------------------------------*
FORM call_bapi_planorder USING    pa_plnum  pa_code.
  DATA: l_ret         LIKE TABLE OF bapireturn1        WITH HEADER LINE.

  CALL FUNCTION 'BAPI_PLANNEDORDER_DELETE'
       EXPORTING
            plannedorder = pa_plnum
       IMPORTING
            return       = l_ret.

  IF l_ret-type = 'A' OR l_ret-type = 'E' .
    pa_code = 'E'.
  ENDIF.
ENDFORM.                    " call_bapi_PLANORDER
