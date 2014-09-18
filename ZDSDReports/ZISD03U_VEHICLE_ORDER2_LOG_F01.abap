*----------------------------------------------------------------------*
***INCLUDE ZISD03U_VEHICLE_ORDER2_LOG_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE G_TC_9000_ITAB
         FROM ZTSD_VEH_OR
        WHERE PDATE  EQ P_PDATE
        AND   WO_SER IN S_WO_SER
        AND   NATION IN S_NATION
        AND   DEALER IN S_DEALER.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  DESCRIBE TABLE G_TC_9000_ITAB LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ENDIF.

  CALL SCREEN 9000.
ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  RESTARTING
*&---------------------------------------------------------------------*
FORM RESTARTING.
  CLEAR W_CNT.
  LOOP AT   G_TC_9000_ITAB
       INTO G_TC_9000_WA
       WHERE FLAG = 'X'.
    W_CNT = W_CNT + 1.
    EXIT.
  ENDLOOP.
  IF W_CNT <> 1.
    MESSAGE I000 WITH TEXT-M02.
    EXIT.
  ENDIF.

  LOOP AT   G_TC_9000_ITAB
       INTO G_TC_9000_WA
       WHERE FLAG = 'X'.
    IF G_TC_9000_WA-GUBUN_S <> 'S'.
      PERFORM SO_WS_WC.
    ELSE.
      PERFORM WS_WC.
    ENDIF.
  ENDLOOP.

  PERFORM READ_DATA.
ENDFORM.                    " RESTARTING
*&---------------------------------------------------------------------*
*&      Form  SO_WS_WC
*&---------------------------------------------------------------------*
FORM SO_WS_WC.
  PERFORM BDC_VA01.

  CALL TRANSACTION 'VA01' USING BDC_TAB MODE 'N'
                                UPDATE 'S'
                                MESSAGES INTO MESS_TAB.
  PERFORM MESSAGE_ADJUST_1 USING G_TC_9000_WA-GUBUN_S
                                 G_TC_9000_WA-MESSAGE.
**
  CHECK G_TC_9000_WA-GUBUN_S = 'S'.
  G_TC_9000_WA-SALES = SY-MSGV2.
  PERFORM UPDATE_WOSUM USING G_TC_9000_WA-GUBUN_W G_TC_9000_WA-MESSAGE.
**
  PERFORM FUNC_MM02 USING G_TC_9000_WA-GUBUN_M G_TC_9000_WA-MESSAGE.

  MODIFY G_TC_9000_ITAB FROM G_TC_9000_WA INDEX 1.

  UPDATE ZTSD_VEH_OR SET : SALES   = SY-MSGV2
                           GUBUN_S = G_TC_9000_WA-GUBUN_S
                           GUBUN_W = G_TC_9000_WA-GUBUN_W
                           GUBUN_M = G_TC_9000_WA-GUBUN_M
                           MESSAGE = G_TC_9000_WA-MESSAGE
                     WHERE PDATE = G_TC_9000_WA-PDATE
                     AND   WO_SER = G_TC_9000_WA-WO_SER
                     AND   NATION = G_TC_9000_WA-NATION
                     AND   DEALER = G_TC_9000_WA-DEALER
                     AND   EXTC = G_TC_9000_WA-EXTC
                     AND   INTC = G_TC_9000_WA-INTC.
ENDFORM.                    " SO_WS_WC
*&---------------------------------------------------------------------*
*&      Form  WS_WC
*&---------------------------------------------------------------------*
FORM WS_WC.
  PERFORM CHECK_CHANGE.
  IF SY-SUBRC = 0.
    IF G_TC_9000_WA-MODQTY = G_TC_9000_WA-SEQQTY OR
       G_TC_9000_WA-SEQQTY = 0.
      PERFORM BDC_VA02.

      CALL TRANSACTION 'VA02' USING BDC_TAB MODE 'N'
                                    UPDATE 'S'
                                    MESSAGES INTO MESS_TAB.
      PERFORM MESSAGE_ADJUST_1 USING G_TC_9000_WA-GUBUN_S
                                     G_TC_9000_WA-MESSAGE .
    ELSE.
      PERFORM BAPI_VA02.
      PERFORM MESSAGE_ADJUST_2 USING G_TC_9000_WA-GUBUN_S
                                     G_TC_9000_WA-MESSAGE.
    ENDIF.
  ELSEIF SY-SUBRC = 4. "NOT CHANGE
*    CONTINUE.
  ELSEIF SY-SUBRC = 1. "Quantity decrease is not allowed
    G_TC_9000_WA-GUBUN_S = 'E'.
    G_TC_9000_WA-MESSAGE = 'Quantity decrease is not allowed'.
    EXIT.
  ENDIF.
**
  PERFORM UPDATE_WOSUM USING G_TC_9000_WA-GUBUN_W G_TC_9000_WA-MESSAGE.
**
  PERFORM FUNC_MM02 USING G_TC_9000_WA-GUBUN_M G_TC_9000_WA-MESSAGE.

  MODIFY G_TC_9000_ITAB FROM G_TC_9000_WA INDEX 1.

  UPDATE ZTSD_VEH_OR SET : GUBUN_S = G_TC_9000_WA-GUBUN_S
                           GUBUN_W = G_TC_9000_WA-GUBUN_W
                           GUBUN_M = G_TC_9000_WA-GUBUN_M
                           MESSAGE = G_TC_9000_WA-MESSAGE
                     WHERE PDATE = G_TC_9000_WA-PDATE
                     AND   WO_SER = G_TC_9000_WA-WO_SER
                     AND   NATION = G_TC_9000_WA-NATION
                     AND   DEALER = G_TC_9000_WA-DEALER
                     AND   EXTC = G_TC_9000_WA-EXTC
                     AND   INTC = G_TC_9000_WA-INTC.
ENDFORM.                    " WS_WC
*&---------------------------------------------------------------------*
*&      Form  BDC_VA01
*&---------------------------------------------------------------------*
FORM BDC_VA01.
  REFRESH : BDC_TAB, MESS_TAB.
  CLEAR   : BDC_TAB, MESS_TAB.

  DATA W_AUART LIKE VBAK-AUART.

  SELECT SINGLE *
         FROM USR01
        WHERE BNAME = SY-UNAME.
  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      W_DATE+4(2) = G_TC_9000_WA-WO_SER+1(2).
      W_DATE+2(2) = G_TC_9000_WA-WO_SER+3(2).
      W_DATE+0(2) = '01'.
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      W_DATE+4(2) = G_TC_9000_WA-WO_SER+1(2).
      W_DATE+0(2) = G_TC_9000_WA-WO_SER+3(2).
      W_DATE+2(2) = '01'.
  ENDCASE.

  SELECT SINGLE *
         FROM MARA
        WHERE MATNR = G_TC_9000_WA-FSC.

* will be customer code 3 digit later !!
* so we don't need the delaer code
  CONCATENATE G_TC_9000_WA-NATION G_TC_9000_WA-DEALER
              INTO W_CHAR_5.

  SELECT SINGLE *
         FROM KNVV
        WHERE KUNNR = W_CHAR_5
        AND   VTWEG = '10'
        AND   LOEVM = ''
        AND   AUFSD = ''.

  CONCATENATE G_TC_9000_WA-WO_SER G_TC_9000_WA-NATION
              G_TC_9000_WA-DEALER
              G_TC_9000_WA-EXTC+0(2) G_TC_9000_WA-INTC+0(2)
              INTO W_CHAR_20.

  WRITE G_TC_9000_WA-MODQTY TO W_CHAR_MOD.

  PERFORM GET_VBELN USING W_VBELN.

  IF G_TC_9000_WA-DEALER+0(1) = 'X'.
    W_AUART = 'ZVTO'.
  ELSE.
    W_AUART = 'ZVSO'.
  ENDIF.

  PERFORM BDC_FILL USING :
          'X' 'SAPMV45A'             '0101',
          ' ' 'VBAK-AUART'           W_AUART,
          ' ' 'VBAK-VKORG'           KNVV-VKORG,
          ' ' 'VBAK-VTWEG'           KNVV-VTWEG,
          ' ' 'VBAK-SPART'           MARA-SPART,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
          ' ' 'VBAK-VBELN'           W_VBELN,
          ' ' 'KUAGV-KUNNR'          W_CHAR_5,
          ' ' 'VBKD-BSTKD'           W_CHAR_20,
          ' ' 'RV45A-KPRGBZ'         'D',
          ' ' 'RV45A-KETDAT'         W_DATE,
          ' ' 'VBKD-PRSDT'           W_DATE,
          ' ' 'VBAP-POSNR(01)'       '10',
          ' ' 'VBAP-POSNR(02)'       '20',
          ' ' 'RV45A-MABNR(01)'      G_TC_9000_WA-FSC,
          ' ' 'RV45A-MABNR(02)'      G_TC_9000_WA-FSC,
          ' ' 'RV45A-KWMENG(01)'     '0',
          ' ' 'RV45A-KWMENG(02)'     W_CHAR_MOD,
          ' ' 'VBAP-UEPOS(02)'       '10',
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPLCEI0'             '0109',     " ITEM 10
          ' ' 'RCTMS-MNAME(01)'      'COLOREXT',
          ' ' 'RCTMS-MNAME(02)'      'COLORINT',
          ' ' 'RCTMS-MWERT(01)'      G_TC_9000_WA-EXTC,
          ' ' 'RCTMS-MWERT(02)'      G_TC_9000_WA-INTC,
          ' ' 'BDC_OKCODE'           '=BACK',
          'X' 'SAPLCEI0'             '0109',     " ITEM 20
          ' ' 'RCTMS-MNAME(01)'      'COLOREXT',
          ' ' 'RCTMS-MNAME(02)'      'COLORINT',
          ' ' 'RCTMS-MWERT(01)'      G_TC_9000_WA-EXTC,
          ' ' 'RCTMS-MWERT(02)'      G_TC_9000_WA-INTC,
          ' ' 'BDC_OKCODE'           '=BACK',
          'X' 'SAPMV45A'             '4001',
          ' ' 'BDC_OKCODE'           '=SICH'.
ENDFORM.                    " BDC_VA01
*&---------------------------------------------------------------------*
*&      Form  GET_VBELN
*&---------------------------------------------------------------------*
FORM GET_VBELN USING VBELN.
  CLEAR VBELN.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = 'ZY'
            OBJECT                  = 'RV_BELEG'
       IMPORTING
            NUMBER                  = VBELN
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.

  CONCATENATE '2' G_TC_9000_WA-WO_SER+1(4) VBELN+5(5) INTO VBELN.
ENDFORM.                    " GET_VBELN
*&---------------------------------------------------------------------*
*&      Form  CHECK_CHANGE
*&---------------------------------------------------------------------*
FORM CHECK_CHANGE.
  TABLES : VBAP.

  DATA : W_10 LIKE G_TC_9000_WA-SEQQTY,
         W_20 LIKE G_TC_9000_WA-MODQTY.

  CLEAR : W_10, W_20.

  SELECT *
         FROM VBAP
        WHERE VBELN EQ G_TC_9000_WA-SALES.
    CASE VBAP-POSNR.
    WHEN '000010'.
      W_10 = VBAP-KWMENG.
    WHEN '000020'.
      W_20 = VBAP-KWMENG.
    ENDCASE.
  ENDSELECT.

  W_20 = W_20 + W_10.

  IF W_10 EQ G_TC_9000_WA-SEQQTY AND
     W_20 EQ G_TC_9000_WA-MODQTY.
     SY-SUBRC = 4.
     EXIT.
  ENDIF.

* Quantity decrease is not allowed
  IF G_TC_9000_WA-WO_SER+1(4) NE SY-DATUM+2(4) AND
     W_20 < G_TC_9000_WA-MODQTY.
     SY-SUBRC = 1.
     EXIT.
  ENDIF.
ENDFORM.                    " CHECK_CHANGE
*&---------------------------------------------------------------------*
*&      Form  BDC_VA02
*&---------------------------------------------------------------------*
FORM BDC_VA02.
  REFRESH : BDC_TAB, MESS_TAB.
  CLEAR   : BDC_TAB, MESS_TAB.

  G_TC_9000_WA-MODQTY = G_TC_9000_WA-MODQTY - G_TC_9000_WA-SEQQTY.
  WRITE G_TC_9000_WA-MODQTY TO W_CHAR_MOD.
  WRITE G_TC_9000_WA-SEQQTY TO W_CHAR_SEQ.

  PERFORM BDC_FILL USING :
          'X' 'SAPMV45A'             '0102',
          ' ' 'VBAK-VBELN'           G_TC_9000_WA-SALES,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
          ' ' 'RV45A-KWMENG(01)'     W_CHAR_SEQ,
          ' ' 'RV45A-KWMENG(02)'     W_CHAR_MOD,
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
          ' ' 'BDC_OKCODE'           '=SICH'.
ENDFORM.                    " BDC_VA02
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_1
*&---------------------------------------------------------------------*
FORM MESSAGE_ADJUST_1 USING GUBUN MESSAGE.
  READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                               MSGID  = 'V1'
                               MSGNR  = '311'.
  IF SY-SUBRC = 0.
    GUBUN = 'S'.
    MESSAGE = ''.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
             MSGID               = MESS_TAB-MSGID
             MSGNR               = MESS_TAB-MSGNR
             MSGV1               = MESS_TAB-MSGV1
             MSGV2               = MESS_TAB-MSGV2
             MSGV3               = MESS_TAB-MSGV3
             MSGV4               = MESS_TAB-MSGV4
           IMPORTING
             MESSAGE_TEXT_OUTPUT = MESSAGE.
      GUBUN = 'E'.
    ELSE.
      READ TABLE MESS_TAB INDEX 1.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
             MSGID               = MESS_TAB-MSGID
             MSGNR               = MESS_TAB-MSGNR
             MSGV1               = MESS_TAB-MSGV1
             MSGV2               = MESS_TAB-MSGV2
             MSGV3               = MESS_TAB-MSGV3
             MSGV4               = MESS_TAB-MSGV4
           IMPORTING
             MESSAGE_TEXT_OUTPUT = MESSAGE.
      GUBUN = 'W'.
    ENDIF.
  ENDIF.
ENDFORM.                    " MESSAGE_ADJUST_1
*&---------------------------------------------------------------------*
*&      Form  BAPI_VA02
*&---------------------------------------------------------------------*
FORM BAPI_VA02.
  G_TC_9000_WA-MODQTY = G_TC_9000_WA-MODQTY - G_TC_9000_WA-SEQQTY.
  WRITE G_TC_9000_WA-MODQTY TO W_CHAR_MOD.
  WRITE G_TC_9000_WA-SEQQTY TO W_CHAR_SEQ.

  CLEAR   : ORDER_HEADER_INX.
  REFRESH : RETURN, ORDER_ITEM_IN, ORDER_ITEM_INX,
            SCHEDULE_LINES, SCHEDULE_LINESX.
  CLEAR   : RETURN, ORDER_ITEM_IN, ORDER_ITEM_INX,
            SCHEDULE_LINES, SCHEDULE_LINESX.

  ORDER_HEADER_INX-UPDATEFLAG = 'U'.

  ORDER_ITEM_IN-ITM_NUMBER = '000010'. APPEND ORDER_ITEM_IN.
  ORDER_ITEM_IN-ITM_NUMBER = '000020'. APPEND ORDER_ITEM_IN.

  ORDER_ITEM_INX-UPDATEFLAG = 'I'.
  ORDER_ITEM_INX-ITM_NUMBER = '000010'. APPEND ORDER_ITEM_INX.
  ORDER_ITEM_INX-UPDATEFLAG = 'I'.
  ORDER_ITEM_INX-ITM_NUMBER = '000020'. APPEND ORDER_ITEM_INX.

  SCHEDULE_LINES-ITM_NUMBER = '000010'.
  SCHEDULE_LINES-SCHED_LINE = '0001'.
  SCHEDULE_LINES-REQ_QTY = W_CHAR_SEQ. APPEND SCHEDULE_LINES.
  SCHEDULE_LINES-ITM_NUMBER = '000020'.
  SCHEDULE_LINES-SCHED_LINE = '0001'.
  SCHEDULE_LINES-REQ_QTY = W_CHAR_MOD. APPEND SCHEDULE_LINES.

  SCHEDULE_LINESX-UPDATEFLAG = 'U'.
  SCHEDULE_LINESX-ITM_NUMBER = '000010'.
  SCHEDULE_LINESX-SCHED_LINE = '0001'.
  SCHEDULE_LINESX-REQ_QTY = 'X'. APPEND SCHEDULE_LINESX.
  SCHEDULE_LINESX-UPDATEFLAG = 'U'.
  SCHEDULE_LINESX-ITM_NUMBER = '000020'.
  SCHEDULE_LINESX-SCHED_LINE = '0001'.
  SCHEDULE_LINESX-REQ_QTY = 'X'. APPEND SCHEDULE_LINESX.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      SALESDOCUMENT               = G_TC_9000_WA-SALES
*     ORDER_HEADER_IN             =
      ORDER_HEADER_INX            = ORDER_HEADER_INX
*     SIMULATION                  = ' '
*     BEHAVE_WHEN_ERROR           = ' '
*     INT_NUMBER_ASSIGNMENT       = ' '
*     LOGIC_SWITCH                =
    TABLES
      RETURN                      = RETURN
      ORDER_ITEM_IN               = ORDER_ITEM_IN
      ORDER_ITEM_INX              = ORDER_ITEM_INX
*     PARTNERS                    =
*     PARTNERCHANGES              =
*     PARTNERADDRESSES            =
*     ORDER_CFGS_REF              =
*     ORDER_CFGS_INST             =
*     ORDER_CFGS_PART_OF          =
*     ORDER_CFGS_VALUE            =
*     ORDER_CFGS_BLOB             =
*     ORDER_CFGS_VK               =
*     ORDER_CFGS_REFINST          =
      SCHEDULE_LINES              = SCHEDULE_LINES
      SCHEDULE_LINESX             = SCHEDULE_LINESX
*     ORDER_TEXT                  =
*     ORDER_KEYS                  =
*     CONDITIONS_IN               =
*     CONDITIONS_INX              =
*     EXTENSIONIN                 =
          .
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT          = 'X'
    IMPORTING
      RETURN        = RETURN.
ENDFORM.                    " BAPI_VA02
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_2
*&---------------------------------------------------------------------*
FORM MESSAGE_ADJUST_2 USING GUBUN MESSAGE.
  READ TABLE RETURN WITH KEY TYPE    = 'S'
                             ID      = 'V1'
                             NUMBER  = '311'.
  IF SY-SUBRC = 0.
    GUBUN = 'S'.
    MESSAGE = ''.
  ELSE.
    READ TABLE RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      MESSAGE = RETURN-MESSAGE.
      GUBUN = 'E'.
    ELSE.
      READ TABLE RETURN INDEX 1.
      MESSAGE = RETURN-MESSAGE.
      GUBUN = 'W'.
    ENDIF.
  ENDIF.
ENDFORM.                    " MESSAGE_ADJUST_2
*&---------------------------------------------------------------------*
*&      Form  UPDATE_WOSUM
*&---------------------------------------------------------------------*
FORM UPDATE_WOSUM USING GUBUN MESSAGE.
  UPDATE ZTPP_WOSUM SET : SALES = G_TC_9000_WA-SALES
                    WHERE WO_SER = G_TC_9000_WA-WO_SER
                    AND   NATION = G_TC_9000_WA-NATION
                    AND   DEALER = G_TC_9000_WA-DEALER
                    AND   EXTC   = G_TC_9000_WA-EXTC
                    AND   INTC   = G_TC_9000_WA-INTC.
  IF SY-SUBRC = 0.
    COMMIT WORK.
    GUBUN = 'S'.
    MESSAGE = ''.
  ELSE.
    GUBUN = 'E'.
    MESSAGE = 'Work Order summary does not updated'.
  ENDIF.
ENDFORM.                    " UPDATE_WOSUM
*&---------------------------------------------------------------------*
*&      Form  FUNC_MM02
*&---------------------------------------------------------------------*
FORM FUNC_MM02 USING GUBUN MESSAGE.
  DATA : BEGIN OF VAL_TABLE OCCURS 0.
         INCLUDE STRUCTURE ZSPP_VIN_VALUE.
  DATA : END OF VAL_TABLE.

  CONCATENATE G_TC_9000_WA-WO_SER G_TC_9000_WA-NATION
              G_TC_9000_WA-DEALER
              G_TC_9000_WA-EXTC+0(2) G_TC_9000_WA-INTC+0(2)
              INTO W_CHAR_20.

  REFRESH VAL_TABLE. CLEAR VAL_TABLE.

  VAL_TABLE-ATNAM = 'P_SALES_ORDER'.
  VAL_TABLE-ATWRT = G_TC_9000_WA-SALES.
  APPEND VAL_TABLE. CLEAR VAL_TABLE.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = W_CHAR_20+0(18)
      MODE               = 'W'
      CTYPE              = '001'
*     DISPLAY            = 'D'
    TABLES
      VAL_TABLE          = VAL_TABLE
    EXCEPTIONS
      NO_DATA            = 1
      ERROR_MODE         = 2
      ERROR_OBJECT       = 3
      OTHERS             = 4.

  IF SY-SUBRC <> 0.
    GUBUN = 'E'.
    MESSAGE = 'Work Order color does not updated'.
  ELSE.
    READ TABLE VAL_TABLE INDEX 1.
    IF VAL_TABLE-ZFLAG = ''. "SUCCESS
      GUBUN = 'S'.
      MESSAGE = ''.
    ELSE.
      GUBUN = 'E'.
      MESSAGE = 'Work Order color does not updated'.
    ENDIF.
  ENDIF.
ENDFORM.                    " FUNC_MM02
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
FORM BDC_FILL USING    P1 P2 P3.
  CLEAR BDC_TAB.
  IF P1 = 'X'.
     BDC_TAB-DYNBEGIN = P1.
     BDC_TAB-PROGRAM  = P2.
     BDC_TAB-DYNPRO   = P3.
  ELSE.
     BDC_TAB-DYNBEGIN = P1.
     BDC_TAB-FNAM     = P2.
     BDC_TAB-FVAL     = P3.
  ENDIF.
  APPEND BDC_TAB.
ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  SHOW_CLICKED_SO_DOC
*&---------------------------------------------------------------------*
FORM SHOW_CLICKED_SO_DOC.
  DATA : W_FIELD(20),
         W_VBELN LIKE VBAK-VBELN.
  GET CURSOR FIELD W_FIELD VALUE W_VBELN.
  CHECK NOT W_VBELN IS INITIAL.
  SET PARAMETER ID 'AUN' FIELD W_VBELN.
  CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
ENDFORM.                    " SHOW_CLICKED_SO_DOC
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_INPUT USING PPPPP.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = PPPPP
       IMPORTING
            OUTPUT = PPPPP.
ENDFORM.                    " CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_OUTPUT USING PPPPP.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = PPPPP
       IMPORTING
            OUTPUT = PPPPP.
ENDFORM.                    " CONVERSION_EXIT_ALPHA_OUTPUT












************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME
                          P_MARK_NAME
                 CHANGING P_OK      LIKE SY-UCOMM.

*-BEGIN OF LOCAL DATA--------------------------------------------------*
   DATA: L_OK              TYPE SY-UCOMM,
         L_OFFSET          TYPE I.
*-END OF LOCAL DATA----------------------------------------------------*

* Table control specific operations                                    *
*   evaluate TC name and operations                                    *
   SEARCH P_OK FOR P_TC_NAME.
   IF SY-SUBRC <> 0.
     EXIT.
   ENDIF.
   L_OFFSET = STRLEN( P_TC_NAME ) + 1.
   L_OK = P_OK+L_OFFSET.
* execute general and TC specific operations                           *
   CASE L_OK.
     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                             L_OK.
       CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME   .
       CLEAR P_OK.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                           P_TABLE_NAME
                                           P_MARK_NAME .
       CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                       P_OK.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
   DATA L_TC_NEW_TOP_LINE     TYPE I.
   DATA L_TC_NAME             LIKE FELD-NAME.
   DATA L_TC_LINES_NAME       LIKE FELD-NAME.
   DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <LINES>      TYPE I.
*-END OF LOCAL DATA----------------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.
* get looplines of TableControl
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


* is no line filled?                                                   *
   IF <TC>-LINES = 0.
*   yes, ...                                                           *
     L_TC_NEW_TOP_LINE = 1.
   ELSE.
*   no, ...                                                            *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
          EXPORTING
               ENTRY_ACT             = <TC>-TOP_LINE
               ENTRY_FROM            = 1
               ENTRY_TO              = <TC>-LINES
               LAST_PAGE_FULL        = 'X'
               LOOPS                 = <LINES>
               OK_CODE               = P_OK
               OVERLAPPING           = 'X'
          IMPORTING
               ENTRY_NEW             = L_TC_NEW_TOP_LINE
          EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               OTHERS                = 0.
   ENDIF.

* get actual tc and column                                             *
   GET CURSOR FIELD L_TC_FIELD_NAME
              AREA  L_TC_NAME.

   IF SYST-SUBRC = 0.
     IF L_TC_NAME = P_TC_NAME.
*     set actual column                                                *
       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
     ENDIF.
   ENDIF.

* set the new top line                                                 *
   <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

* get the table, which belongs to the tc                               *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

* mark all filled lines                                                *
  LOOP AT <TABLE> ASSIGNING <WA>.

*   access to the component 'FLAG' of the table header                 *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

* get the table, which belongs to the tc                               *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

* demark all filled lines                                              *
  LOOP AT <TABLE> ASSIGNING <WA>.

*   access to the component 'FLAG' of the table header                 *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
