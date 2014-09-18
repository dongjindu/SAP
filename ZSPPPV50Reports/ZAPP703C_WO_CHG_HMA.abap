************************************************************************
* Program Name      : ZAPP703C_WO_CHG_HMA
* Author            : Furong
* Creation Date     : 10/21/05
* Specifications By : MY Hur
* Pattern           :
* Development Request No : UD1K918082
* Addl Documentation:
* Description       : Interface of Work Order change from HMA
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  ZAPP703C_WO_CHG_HMA   LINE-SIZE  700 MESSAGE-ID ZMPP.

DATA: BEGIN OF IT_DATA       OCCURS 0.
        INCLUDE STRUCTURE    ZTSD_SODATA.
DATA:   END OF IT_DATA.

DATA: IT_MSG                 LIKE TABLE OF BDCMSGCOLL  WITH HEADER LINE,
      IT_BDCDATA             LIKE TABLE OF BDCDATA     WITH HEADER LINE,
      IT_ERROR               LIKE TABLE OF IT_DATA     WITH HEADER LINE,
      WA_MODE                TYPE C VALUE 'N',
      WA_DATE                LIKE SY-DATUM,
      WA_CNT                 TYPE I,
      WA_FLG                 TYPE C,
      WA_ERROR               TYPE C,
      WA_NUMBER              LIKE ZTPP_PP_LOG_HEAD-LOGKEY,
      WA_MSG(70)              TYPE C.

DATA: IT_REC                  LIKE TABLE OF MARA,
      P_TCODE                 LIKE TSTC-TCODE,
      P_CMODE                 TYPE C,
      P_PMODE                 TYPE C VALUE 'N',
      WA_FILENAME             LIKE RLGRAP-FILENAME,
      WA_FILETYPE             LIKE RLGRAP-FILETYPE VALUE 'DAT',
      WA_BDCGROUP             LIKE SY-UNAME.          " APQI-GROUPID

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
PARAMETERS: P_FLAG RADIOBUTTON GROUP RAD1 DEFAULT 'X',
            REP_FLAG RADIOBUTTON GROUP RAD1,
            P_LOG            LIKE ZTPP_PP_LOG_HEAD-LOGKEY NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.

LOAD-OF-PROGRAM.

START-OF-SELECTION.
*  PERFORM write_start.
  PERFORM GET_DATA.
  PERFORM DATA_PROCESSING .
*  PERFORM write_end.
  PERFORM UPDATE_ZTSD_SODATA.
  INCLUDE ZCPP103_COMMON_ROUTINE .


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  IF P_FLAG = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
      FROM ZTSD_SODATA
      WHERE P_FLAG = ' '.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
     FROM ZTSD_SODATA
     WHERE P_FLAG = 'E'.
  ENDIF.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_PROCESSING.
  DATA: L_MODSUM LIKE ZTPP_KSBOHMM-MODQTY,
        L_NEWSUM LIKE ZTPP_KSBOHMM-INITQTY,
        L_MATERIAL_CL LIKE MARA-MATNR,
        L_MATERIAL_HD LIKE MARA-MATNR,
        L_OLD_MODQTY LIKE ZTPP_KSBOHMM-MODQTY,
        L_WO_MODQTY LIKE ZTPP_KSBOHMM-MODQTY,
        WA_OLD_MODQTY LIKE ZTPP_KSBOHMM-MODQTY,
        L_WOHD_INIQTY LIKE ZTPP_KSBOHMM-MODQTY,
        L_LAST(1),
        L_NEW_WO_SER(1),
        L_SORDER LIKE VBAP-VBELN,
        L_KWMENG10 LIKE VBAP-KWMENG,
        L_KWMENG20 LIKE VBAP-KWMENG,
        P_ITEM20_QTY LIKE VBAP-KWMENG,
        L_ITEM20_FLG(1),
        L_ITEM20_QTY_FLG(1).

  DATA: L_SEQ_QTY LIKE ZTPP_KSBOHMM-MODQTY,
        L_PLAN_QTY LIKE ZTPP_KSBOHMM-MODQTY,
        L_FORECATEQTY  LIKE ZTPP_KSBOHMM-MODQTY.

  DATA: L_VIN_VALUE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  DATA: L_IT_ORD_HEADER_INX LIKE TABLE OF BAPISDH1X  WITH HEADER LINE,
        L_BAPISDLS          LIKE BAPISDLS ,
        L_IT_RETURN         LIKE TABLE OF BAPIRET2   WITH HEADER LINE,
        L_IT_ITM            LIKE TABLE OF BAPISDITM  WITH HEADER LINE,
        L_IT_ITMX           LIKE TABLE OF BAPISDITMX WITH HEADER LINE,
        L_IT_LINES          LIKE TABLE OF BAPISCHDL  WITH HEADER LINE,
        L_IT_LINESX         LIKE TABLE OF BAPISCHDLX WITH HEADER LINE.

  DESCRIBE TABLE IT_DATA LINES WA_CNT.
  IF WA_CNT = 0.
    MESSAGE S001 WITH TEXT-018.
    EXIT.
  ENDIF.
  SORT IT_DATA BY WO_SER NATION DEALER EXTC INTC .

  LOOP AT IT_DATA.

    CLEAR: WA_ERROR, L_SORDER, L_MATERIAL_CL,
           L_MATERIAL_CL, L_OLD_MODQTY, L_WOHD_INIQTY.
    CLEAR: L_IT_ORD_HEADER_INX,L_BAPISDLS, L_IT_RETURN,
           L_IT_ITM, L_IT_ITMX, L_IT_LINES, L_IT_LINESX.
    REFRESH: L_IT_ORD_HEADER_INX, L_IT_RETURN,
             L_IT_ITM, L_IT_ITMX, L_IT_LINES, L_IT_LINESX,
             L_VIN_VALUE.

    AT NEW WO_SER.
      L_NEW_WO_SER = 'X'.
    ENDAT.
    IF L_NEW_WO_SER = 'X'.
      CLEAR: L_NEW_WO_SER, L_WO_MODQTY.
** wohd modqty
      SELECT SUM( MODQTY ) INTO L_WO_MODQTY
             FROM ZTPP_WOSUM
             WHERE WO_SER = IT_DATA-WO_SER
               AND NATION = IT_DATA-NATION
               AND DEALER = IT_DATA-DEALER.
    ENDIF.

    CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                IT_DATA-EXTC IT_DATA-INTC INTO L_MATERIAL_CL.

** Changed by Furong on 08/24/10
*    PERFORM check_and_get_modqty USING l_material_cl
*                       CHANGING l_old_modqty.
    PERFORM CHECK_AND_GET_MODQTY USING L_MATERIAL_CL
                        CHANGING L_OLD_MODQTY
                                 L_SEQ_QTY
                                 L_PLAN_QTY.
** End of change on 08/24/10
    IF NOT WA_ERROR IS INITIAL.
      IT_DATA-P_FLAG = 'E'.
      MODIFY IT_DATA.
      CONTINUE.
    ENDIF.

    WA_OLD_MODQTY = WA_OLD_MODQTY + L_OLD_MODQTY.

** check it_data-ordqty = l_old_modqty
    IF IT_DATA-ORDQTY <> L_OLD_MODQTY.
** update wo color
      PERFORM MATERIAL_UPDATE USING L_MATERIAL_CL 'WOCL'
                                  IT_DATA-ORDQTY L_OLD_MODQTY
                                  CHANGING L_SORDER.
      IF WA_ERROR IS INITIAL.
** update wosum table
** Changed by Furong on 08/24/10
*        PERFORM update_wosum USING l_material_cl l_old_modqty.
        L_FORECATEQTY = IT_DATA-ORDQTY - L_SEQ_QTY -
                        L_PLAN_QTY.

        PERFORM UPDATE_WOSUM USING L_MATERIAL_CL L_OLD_MODQTY
                                   L_FORECATEQTY.
** End of change on 08/24/10
        IF WA_ERROR IS INITIAL.
        ELSE.
          WRITE:/ TEXT-016, L_MATERIAL_CL.
          IT_DATA-P_FLAG = 'E'.
          MODIFY IT_DATA.
          CONTINUE.
        ENDIF.
      ELSE.
        IT_DATA-P_FLAG = 'E'.
        MODIFY IT_DATA.
        CONTINUE.
      ENDIF.

** UPDATE SALES ORDER
      IF L_SORDER IS INITIAL.
        IT_DATA-P_FLAG = 'P'.
        MODIFY IT_DATA.
      ELSE.
        SELECT SINGLE KWMENG INTO L_KWMENG10
            FROM VBAP
           WHERE VBELN = L_SORDER
             AND POSNR = '000010'.
        SELECT SINGLE KWMENG INTO L_KWMENG20
          FROM VBAP
         WHERE VBELN = L_SORDER
           AND POSNR = '000020'.

        P_ITEM20_QTY = IT_DATA-ORDQTY - L_KWMENG10.
        IF P_ITEM20_QTY <> L_KWMENG20.
          IF L_KWMENG20 = 0.
            L_ITEM20_FLG = 'I'.
            L_ITEM20_QTY_FLG = 'I'.
          ELSEIF P_ITEM20_QTY = 0 .
            L_ITEM20_FLG = 'U'.
            L_ITEM20_QTY_FLG = 'D'.
          ELSE.
            L_ITEM20_FLG     = 'U'.
            L_ITEM20_QTY_FLG = 'U'.
          ENDIF.

          L_BAPISDLS-SCHEDULING = 'X'.

          L_IT_ORD_HEADER_INX-UPDATEFLAG = 'U'.
          APPEND L_IT_ORD_HEADER_INX.

          L_IT_ITM-ITM_NUMBER = '000020'.
          APPEND L_IT_ITM.
          L_IT_ITMX-UPDATEFLAG = L_ITEM20_FLG.
          L_IT_ITMX-ITM_NUMBER = '000020'.
          APPEND L_IT_ITMX.

          L_IT_LINES-ITM_NUMBER = '000020'.
          IF L_ITEM20_QTY_FLG = 'I'.
            L_IT_LINES-SCHED_LINE = '0001'.
          ELSE.
            SELECT SINGLE ETENR INTO L_IT_LINES-SCHED_LINE
              FROM VBEP
             WHERE VBELN = L_SORDER
               AND POSNR = L_IT_LINES-ITM_NUMBER .
          ENDIF.
          L_IT_LINES-REQ_QTY = P_ITEM20_QTY.
          APPEND L_IT_LINES.

          L_IT_LINESX-UPDATEFLAG = L_ITEM20_QTY_FLG.
          L_IT_LINESX-ITM_NUMBER = '000020'.
          L_IT_LINESX-SCHED_LINE = L_IT_LINES-SCHED_LINE .
          L_IT_LINESX-REQ_QTY = 'X'.
          APPEND L_IT_LINESX.

          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
               EXPORTING
                    SALESDOCUMENT    = L_SORDER
                    ORDER_HEADER_INX = L_IT_ORD_HEADER_INX
                    LOGIC_SWITCH     = L_BAPISDLS
               TABLES
                    RETURN           = L_IT_RETURN
                    ORDER_ITEM_IN    = L_IT_ITM
                    ORDER_ITEM_INX   = L_IT_ITMX
                    SCHEDULE_LINES   = L_IT_LINES
                    SCHEDULE_LINESX  = L_IT_LINESX.

          LOOP AT L_IT_RETURN.
            IF L_IT_RETURN-TYPE = 'E' OR L_IT_RETURN-TYPE = 'A'.
              IT_DATA-P_FLAG = 'E'.
              WA_ERROR = 'X'.
              MODIFY IT_DATA.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF WA_ERROR IS INITIAL.
            WRITE: / 'Successfully updated: ',  L_MATERIAL_CL.
            IT_DATA-P_FLAG = 'S'.
            MODIFY IT_DATA.
          ENDIF.

*      LOOP AT l_it_return.
*        IF l_it_return-type = 'E' OR
*           l_it_return-type = 'A'.
*          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*               EXPORTING
*                    msgid               = l_it_return-id
*                    msgnr               = l_it_return-number
*                    msgv1               = l_it_return-message_v1
*                    msgv2               = l_it_return-message_v2
*                    msgv3               = l_it_return-message_v3
*                    msgv4               = l_it_return-message_v4
*               IMPORTING
*                    message_text_output = wa_msg.
*          WRITE: /'Sales Order -- ' , wa_msg .
*        ENDIF.
*      ENDLOOP.
        ELSE.
          WRITE: / 'Successfully updated: ',  L_MATERIAL_CL.
          IT_DATA-P_FLAG = 'S'.
          MODIFY IT_DATA.
        ENDIF.
      ENDIF.
    ELSE.
      IT_DATA-P_FLAG = 'P'.
      MODIFY IT_DATA.
    ENDIF.

** Update wo header
    AT END OF WO_SER.
      L_LAST = 'X'.
      SUM.
      L_MODSUM = IT_DATA-ORDQTY.
*     l_newsum = it_data-newqty.
    ENDAT.

    IF L_LAST ='X'.
*      CLEAR: l_vin_value.
*      REFRESH: l_vin_value.
      CLEAR: L_MATERIAL_HD, L_LAST.

      CONCATENATE IT_DATA-WO_SER IT_DATA-NATION IT_DATA-DEALER
                  INTO L_MATERIAL_HD.

      L_MODSUM = L_WO_MODQTY - WA_OLD_MODQTY + L_MODSUM.

** wohd initial qty
*      l_vin_value-atnam = 'P_MOD_QTY'.
*      APPEND l_vin_value.
*      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*           EXPORTING
*                object    = l_material_hd
*                ctype     = '001'
*           TABLES
*                val_table = l_vin_value.
*
*      IF sy-subrc = 0.
*        READ TABLE l_vin_value INDEX 1.
      L_WOHD_INIQTY = L_WO_MODQTY.

      PERFORM MATERIAL_UPDATE USING L_MATERIAL_HD 'WOHD'
                                          L_MODSUM
                                          L_WOHD_INIQTY
                   CHANGING L_SORDER.
      IF WA_ERROR IS INITIAL.
        WRITE: / 'Successfully updated(HD): ',  L_MATERIAL_HD.
        IT_DATA-P_FLAG = 'S'.
      ELSE.
        IT_DATA-P_FLAG = 'E'.
      ENDIF.
      MODIFY IT_DATA.
*      ENDIF.
      CLEAR: WA_OLD_MODQTY, L_MODSUM, L_NEWSUM.
    ENDIF.
    CLEAR: IT_DATA.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT = 'X'.

ENDFORM.                    " BDC_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  material_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MATERIAL_UPDATE  USING  PA_MATERIAL PA_CM
                                 PA_ORDQTY PA_INITQTY
                          CHANGING PA_SORDER LIKE VBAP-VBELN.

  DATA: L_CONF               LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE,
        L_VARIABLE           LIKE TABLE OF L_CONF      WITH HEADER LINE,
        L_QTY                TYPE I,
        L_NEWQTY             TYPE I.

  DATA: L_SEQQTY LIKE IT_DATA-ORDQTY,
        L_PLANQTY LIKE IT_DATA-ORDQTY,
        L_FCQTY LIKE IT_DATA-ORDQTY.

** Changed by Furong on 08/24/10
*  IF pa_cm = 'WOCL'.
*    l_conf-atnam = 'P_SALES_ORDER'.
*    APPEND l_conf.
*    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*         EXPORTING
*              object    = pa_material
*              ctype     = '001'
*         TABLES
*              val_table = l_conf.
*
*    IF sy-subrc = 0.
*      READ TABLE l_conf INDEX 1.
*      pa_sorder = l_conf-atwrt.
*    ENDIF.
*  ENDIF.

  L_CONF-ATNAM = 'P_SALES_ORDER'.
  APPEND L_CONF.
  L_CONF-ATNAM = 'P_SEQ_QTY'.
  APPEND L_CONF.
  L_CONF-ATNAM = 'P_PLAN_QTY'.
  APPEND L_CONF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT    = PA_MATERIAL
            CTYPE     = '001'
       TABLES
            VAL_TABLE = L_CONF.

  IF SY-SUBRC = 0.
    READ TABLE L_CONF INDEX 1.
    PA_SORDER = L_CONF-ATWRT.
    READ TABLE L_CONF INDEX 2.
    L_SEQQTY = L_CONF-ATWRT.
    READ TABLE L_CONF INDEX 3.
    L_PLANQTY = L_CONF-ATWRT.
    L_FCQTY = PA_ORDQTY - L_SEQQTY - L_PLANQTY.
  ENDIF.

  L_VARIABLE-ATNAM = 'P_FORECAST_QTY'.
  L_VARIABLE-ATWRT =  L_FCQTY.
  APPEND L_VARIABLE.

** end of changed on 08/24/10

  L_VARIABLE-ATNAM = 'P_MOD_QTY'.
  L_VARIABLE-ATWRT =  PA_ORDQTY.
  APPEND L_VARIABLE.
  L_VARIABLE-ATNAM =  'P_INIT_QTY'.
  L_VARIABLE-ATWRT =  PA_INITQTY.
  APPEND L_VARIABLE.
  L_VARIABLE-ATNAM = 'P_MOD_DATE'.
  L_VARIABLE-ATWRT =  SY-DATUM.
  APPEND L_VARIABLE.
  L_VARIABLE-ATNAM = 'P_WO_MODI_DATE'.
  L_VARIABLE-ATWRT =  SY-DATUM.
  APPEND L_VARIABLE.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PA_MATERIAL
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VARIABLE
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC = 0.
  ELSE.
    WRITE:/ TEXT-015, PA_MATERIAL, PA_CM.
    WA_ERROR = 'X' .
*    LOOP AT l_variable WHERE zflag = 'E' .
*      " Step 4: Material's Characteristic Value UPDATE Error...
*      PERFORM create_log USING 'E' 4 text-010 it_data.
*      PERFORM create_log USING 'E' 4 text-010 l_variable.
*    ENDLOOP.
  ENDIF.
ENDFORM.                    " bdc_material_update

*&---------------------------------------------------------------------*
*&      Form  UPDATE_WOSUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_WOSUM  USING PA_MATERIAL PA_INITQTY PA_FCQTY.
  DATA:L_INITQTY LIKE ZTPP_KSBOHMM-MODQTY.
  DATA:L_FCQTY LIKE ZTPP_KSBOHMM-MODQTY.

  L_INITQTY = PA_INITQTY.
  L_FCQTY = PA_FCQTY.
  UPDATE ZTPP_WOSUM  SET: WOMODDATE = SY-DATUM
                        MODQTY    = IT_DATA-ORDQTY
                        INITQTY   = L_INITQTY
                        FORECASTQTY = L_FCQTY
                        AEDAT     = SY-DATUM
                        AEZET     = SY-UZEIT
                        AENAM     = SY-UNAME
                  WHERE WO_SER = PA_MATERIAL(9)
                    AND NATION = PA_MATERIAL+9(3)
                    AND DEALER = PA_MATERIAL+12(2)
                    AND EXTC   = PA_MATERIAL+14(2)
                    AND INTC   = PA_MATERIAL+16(2).

  IF SY-SUBRC NE 0.
    WA_ERROR ='X'.
*    " Step 7: Summary Table Update Error..
*    PERFORM create_log USING 'E' 7 text-006 it_data.
  ENDIF.
ENDFORM.                    " UPDATE_WOSUM

*&---------------------------------------------------------------------*
*&      Form  CHECK_BDC_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0420   text
*      -->P_0421   text
*      -->P_L_MATERIAL  text
*----------------------------------------------------------------------*
*FORM check_bdc_result USING    pa_mode  pa_type  pa_material.
*  DATA: l_msg(100)           TYPE c.
*
*  IF wa_error NE ' '     .
*    CLEAR: it_msg, it_msg[].
*    EXIT .
*  ENDIF.
*
*  it_data-p_flag = 'X' .
*
*  READ TABLE it_msg WITH KEY msgtyp = 'E' .
*
*  CHECK sy-subrc = 0.
*  wa_error = 'E'    .
*  CLEAR: it_data-p_flag.
*  LOOP AT it_msg WHERE msgtyp = 'E' .
*    PERFORM create_message USING l_msg  it_msg-msgid it_msg-msgnr
*                    it_msg-msgv1 it_msg-msgv2 it_msg-msgv3 it_msg-msgv4
.
*    " Step 6: Material Master Creation Error for the BDC
*    PERFORM create_log USING 'E' 6 l_msg    it_data.
*    PERFORM create_log USING 'R' 6 l_msg    it_data.
*    PERFORM create_log USING 'R' 6 l_msg    it_data-s219.
*  ENDLOOP.
*  CLEAR: it_msg, it_msg[].
*ENDFORM.                    " CHECK_BDC_RESULT

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_TXT_001  text
*----------------------------------------------------------------------*
*FORM create_log USING    pa_type  pa_step  pa_text  pa_key .
*  DATA: l_seq                LIKE ztpp_pp_log_deta-sequence.
*  SELECT MAX( sequence ) INTO l_seq
*    FROM ztpp_pp_log_deta
*   WHERE logkey = p_log   .
*
*  l_seq = l_seq + 1.
*  " Log Detail Creation
*  ztpp_pp_log_deta-logkey   = p_log        .
*  ztpp_pp_log_deta-sequence = l_seq      .
*  ztpp_pp_log_deta-logtype  = pa_type     .
*  ztpp_pp_log_deta-logstep  = pa_step     .
*  ztpp_pp_log_deta-keydata  = pa_key      .
*  INSERT INTO ztpp_pp_log_deta VALUES ztpp_pp_log_deta .
*ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  GET_LOGSERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LOGSERIAL.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZLOG'
       IMPORTING
            NUMBER                  = WA_NUMBER
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.
ENDFORM.                    " GET_LOGSERIAL

*&---------------------------------------------------------------------*
*&      Form  write_start
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_START.
  WRITE :/ TEXT-201 .
  WRITE AT: /001(030) TEXT-202,
             031(010) SY-DATUM,
             042(010) SY-UZEIT.
  WRITE :/ TEXT-201.
  SKIP 1.
ENDFORM.                    " write_start

*&---------------------------------------------------------------------*
*&      Form  WRITE_END
*&---------------------------------------------------------------------*
FORM WRITE_END.
  GET TIME.
  SKIP 2.
  WRITE :/ TEXT-203 .
  WRITE AT: /001(030) TEXT-204,
             031(010) SY-DATUM,
             042(010) SY-UZEIT.
  WRITE :/ TEXT-203.
ENDFORM.                    " WRITE_END
*&---------------------------------------------------------------------*
*&      Form  update_ztsd_sodata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_ZTSD_SODATA.
  LOOP AT IT_DATA.
    UPDATE ZTSD_SODATA  SET: P_FLAG = IT_DATA-P_FLAG
                           ZBDAT     = SY-DATUM
                           ZBTIM     = SY-UZEIT
                       WHERE WO_SER = IT_DATA-WO_SER
                       AND NATION = IT_DATA-NATION
                       AND DEALER = IT_DATA-DEALER
                       AND EXTC   = IT_DATA-EXTC
                       AND INTC   = IT_DATA-INTC
                       AND ZSDAT  = IT_DATA-ZSDAT
                       AND ZSTIM  = IT_DATA-ZSTIM.

    IF SY-SUBRC NE 0.
      SKIP.
      WRITE:/ 'Error for updating ztsd_sodata,'. " it_data-wo_ser.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.                    " update_ztsd_sodata
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AND_GET_MODQTY USING PA_MATERIAL
                CHANGING PA_MODQTY
                         PA_SEQQTY
                         PA_PLANQTY.
  DATA: L_MODQTY LIKE ZTPP_WOSUM-MODQTY,
        L_SEQQTY LIKE ZTPP_WOSUM-SEQQTY,
        L_PLANQTY LIKE ZTPP_WOSUM-SEQQTY,
        L_NEWQTY TYPE I.

  SELECT SINGLE MODQTY SEQQTY PLANQTY INTO
               (L_MODQTY, L_SEQQTY, L_PLANQTY)
               FROM ZTPP_WOSUM
               WHERE WO_SER = IT_DATA-WO_SER
                 AND NATION = IT_DATA-NATION
                 AND DEALER = IT_DATA-DEALER
                 AND EXTC = IT_DATA-EXTC
                 AND INTC = IT_DATA-INTC.

  IF SY-SUBRC = 0.
** check sequence qty
    IF L_SEQQTY > IT_DATA-ORDQTY.
      WRITE:/ TEXT-003, PA_MATERIAL.
      WA_ERROR = 'X'.
      EXIT .
    ENDIF.
** check neq qty
*    l_newqty = it_data-ordqty - l_modqty.
*    IF l_newqty <> it_data-newqty.
*      WRITE:/ text-021, pa_material, 'QTY in ZTPP_WOSUM:', l_modqty.
*      wa_error = 'X'.
*      EXIT.
*    ENDIF.
    PA_MODQTY = L_MODQTY.
    PA_SEQQTY = L_SEQQTY.
    PA_PLANQTY = L_PLANQTY.
  ELSE.
    WRITE:/ TEXT-022, PA_MATERIAL.
    WA_ERROR = 'X'.
    EXIT .
  ENDIF.
ENDFORM.                    " check_data
