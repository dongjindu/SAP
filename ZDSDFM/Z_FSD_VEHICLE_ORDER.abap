FUNCTION Z_FSD_VEHICLE_ORDER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_VEH_OR STRUCTURE  ZTSD_VEH_OR
*"----------------------------------------------------------------------
  READ TABLE IT_VEH_OR INDEX 1.

  IF IT_VEH_OR-SALES IS INITIAL.
*&* PERFORM BDC_VA01.
    REFRESH : BDC_TAB, MESS_TAB.
    CLEAR   : BDC_TAB, MESS_TAB.

    DATA W_AUART LIKE VBAK-AUART.

    SELECT SINGLE *
           FROM USR01
          WHERE BNAME = SY-UNAME.
    CASE USR01-DATFM.
      WHEN '1'. "DD.MM.YYYY
        W_DATE+4(2) = IT_VEH_OR-WO_SER+1(2).
        W_DATE+2(2) = IT_VEH_OR-WO_SER+3(2).
        W_DATE+0(2) = '01'.
      WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
        W_DATE+4(2) = IT_VEH_OR-WO_SER+1(2).
        W_DATE+0(2) = IT_VEH_OR-WO_SER+3(2).
        W_DATE+2(2) = '01'.
    ENDCASE.

    SELECT SINGLE *
           FROM MARA
          WHERE MATNR = IT_VEH_OR-FSC.

* will be customer code 3 digit later !!
* so we don't need the delaer code
    CONCATENATE IT_VEH_OR-NATION IT_VEH_OR-DEALER
                INTO W_CHAR_5.

    SELECT SINGLE *
           FROM KNVV
          WHERE KUNNR = W_CHAR_5
          AND   VTWEG = '10'
          AND   LOEVM = ''
          AND   AUFSD = ''.

    CONCATENATE IT_VEH_OR-WO_SER IT_VEH_OR-NATION IT_VEH_OR-DEALER
                IT_VEH_OR-EXTC+0(2) IT_VEH_OR-INTC+0(2)
                INTO W_CHAR_20.

    WRITE IT_VEH_OR-MODQTY TO W_CHAR_MOD.

*&* PERFORM GET_VBELN USING W_VBELN.
    CLEAR W_VBELN.

    CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
              NR_RANGE_NR             = 'ZY'
              OBJECT                  = 'RV_BELEG'
         IMPORTING
              NUMBER                  = W_VBELN
         EXCEPTIONS
              INTERVAL_NOT_FOUND      = 1
              NUMBER_RANGE_NOT_INTERN = 2
              OBJECT_NOT_FOUND        = 3
              QUANTITY_IS_0           = 4
              QUANTITY_IS_NOT_1       = 5
              INTERVAL_OVERFLOW       = 6
              BUFFER_OVERFLOW         = 7
              OTHERS                  = 8.

    CONCATENATE '2' IT_VEH_OR-WO_SER+1(4) W_VBELN+5(5) INTO W_VBELN.
*&* PERFORM GET_VBELN USING W_VBELN.

    IF IT_VEH_OR-DEALER+0(1) = 'X'.
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
            ' ' 'RV45A-MABNR(01)'      IT_VEH_OR-FSC,
            ' ' 'RV45A-MABNR(02)'      IT_VEH_OR-FSC,
            ' ' 'RV45A-KWMENG(01)'     '0',
            ' ' 'RV45A-KWMENG(02)'     W_CHAR_MOD,
            ' ' 'VBAP-UEPOS(02)'       '10',
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPLCEI0'             '0109',              " ITEM 10
            ' ' 'RCTMS-MNAME(01)'      'COLOREXT',
            ' ' 'RCTMS-MNAME(02)'      'COLORINT',
            ' ' 'RCTMS-MWERT(01)'      IT_VEH_OR-EXTC,
            ' ' 'RCTMS-MWERT(02)'      IT_VEH_OR-INTC,
            ' ' 'BDC_OKCODE'           '=BACK',
            'X' 'SAPLCEI0'             '0109',              " ITEM 20
            ' ' 'RCTMS-MNAME(01)'      'COLOREXT',
            ' ' 'RCTMS-MNAME(02)'      'COLORINT',
            ' ' 'RCTMS-MWERT(01)'      IT_VEH_OR-EXTC,
            ' ' 'RCTMS-MWERT(02)'      IT_VEH_OR-INTC,
            ' ' 'BDC_OKCODE'           '=BACK',
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '=SICH'.
*&* PERFORM BDC_VA01.

    CALL TRANSACTION 'VA01' USING BDC_TAB MODE 'N'
                                  UPDATE 'S'
                                  MESSAGES INTO MESS_TAB.
*&* PERFORM MESSAGE_ADJUST_1 USING 1 BDC_LIST-GUBUN_S.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'V1'
                                 MSGNR  = '311'.
    IF SY-SUBRC = 0.
      IT_VEH_OR-GUBUN_S = 'S'.
      IT_VEH_OR-MESSAGE = ''.
      IT_VEH_OR-SALES   = SY-MSGV2.
*&* PERFORM UPDATE_WOSUM USING BDC_LIST-GUBUN_W.
      UPDATE ZTPP_WOSUM SET : SALES = SY-MSGV2
                        WHERE WO_SER = IT_VEH_OR-WO_SER
                        AND   NATION = IT_VEH_OR-NATION
                        AND   DEALER = IT_VEH_OR-DEALER
                        AND   EXTC   = IT_VEH_OR-EXTC
                        AND   INTC   = IT_VEH_OR-INTC.
      IF SY-SUBRC = 0.
        IT_VEH_OR-GUBUN_W = 'S'.
      ELSE.
        IT_VEH_OR-GUBUN_W = 'E'.
        IT_VEH_OR-MESSAGE = 'Work Order summary does not updated'.
      ENDIF.
*&* PERFORM UPDATE_WOSUM USING BDC_LIST-GUBUN_W.
*&* PERFORM FUNC_MM02.
      DATA : BEGIN OF VAL_TABLE OCCURS 0.
              INCLUDE STRUCTURE ZSPP_VIN_VALUE.
      DATA : END OF VAL_TABLE.

      CONCATENATE IT_VEH_OR-WO_SER IT_VEH_OR-NATION IT_VEH_OR-DEALER
                  IT_VEH_OR-EXTC+0(2) IT_VEH_OR-INTC+0(2)
                  INTO W_CHAR_20.

      REFRESH VAL_TABLE. CLEAR VAL_TABLE.

      VAL_TABLE-ATNAM = 'P_SALES_ORDER'.
      VAL_TABLE-ATWRT = IT_VEH_OR-SALES.
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
        ERROR_VALUE        = 4
        OTHERS             = 5 .

      IF SY-SUBRC <> 0.
        IT_VEH_OR-GUBUN_M = 'E'.
        IT_VEH_OR-MESSAGE = 'Work Order does not updated'.
      ELSE.
        READ TABLE VAL_TABLE INDEX 1.
        IF VAL_TABLE-ZFLAG = ''. "SUCCESS
          IT_VEH_OR-GUBUN_M = 'S'.
          IT_VEH_OR-MESSAGE = ''.
        ELSE.
          IT_VEH_OR-GUBUN_M = 'E'.
          IT_VEH_OR-MESSAGE = 'Work Order does not updated'.
        ENDIF.
      ENDIF.
*&* PERFORM FUNC_MM02.
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
                  MESSAGE_TEXT_OUTPUT = IT_VEH_OR-MESSAGE.
        IT_VEH_OR-GUBUN_S = 'E'.
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
                  MESSAGE_TEXT_OUTPUT = IT_VEH_OR-MESSAGE.
        IT_VEH_OR-GUBUN_S = 'W'.
      ENDIF.
    ENDIF.
*&* PERFORM MESSAGE_ADJUST_1 USING 1 BDC_LIST-GUBUN_S.
  ELSE.
*&* PERFORM CHECK_CHANGE.
**    DATA : W_10 LIKE IT_VEH_OR-SEQQTY,
**           W_20 LIKE IT_VEH_OR-MODQTY.
**
**    CLEAR : W_10, W_20.
**
**    SELECT *
**           FROM VBAP
**          WHERE VBELN EQ IT_VEH_OR-SALES.
**      CASE VBAP-POSNR.
**        WHEN '000010'.
**          W_10 = VBAP-KWMENG.
**        WHEN '000020'.
**          W_20 = VBAP-KWMENG.
**      ENDCASE.
**    ENDSELECT.
**
**    W_20 = W_20 + W_10.
**
      SY-SUBRC = 0.
**    IF W_10 EQ IT_VEH_OR-SEQQTY AND
**       W_20 EQ IT_VEH_OR-MODQTY.
**      SY-SUBRC = 4.
**    ELSE.
***   Quantity decrease is not allowed
**      IF IT_VEH_OR-WO_SER+1(4) NE SY-DATUM+2(4) AND
**         W_20 < IT_VEH_OR-MODQTY.
**        SY-SUBRC = 1.
**      ENDIF.
**    ENDIF.
*&* PERFORM CHECK_CHANGE.
    IF SY-SUBRC = 0.
      IF IT_VEH_OR-MODQTY = IT_VEH_OR-SEQQTY OR
         IT_VEH_OR-SEQQTY = 0.
*&* PERFORM BDC_VA02.
        REFRESH : BDC_TAB, MESS_TAB.
        CLEAR   : BDC_TAB, MESS_TAB.

        IT_VEH_OR-MODQTY = IT_VEH_OR-MODQTY - IT_VEH_OR-SEQQTY.
        WRITE IT_VEH_OR-MODQTY TO W_CHAR_MOD.
        WRITE IT_VEH_OR-SEQQTY TO W_CHAR_SEQ.

        PERFORM BDC_FILL USING :
                'X' 'SAPMV45A'             '0102',
                ' ' 'VBAK-VBELN'           IT_VEH_OR-SALES,
                ' ' 'BDC_OKCODE'           '/00',
                'X' 'SAPMV45A'             '4001',
                ' ' 'RV45A-KWMENG(01)'     W_CHAR_SEQ,
                ' ' 'RV45A-KWMENG(02)'     W_CHAR_MOD,
                ' ' 'BDC_OKCODE'           '/00',
                'X' 'SAPMV45A'             '4001',
                ' ' 'BDC_OKCODE'           '=SICH'.
*&* PERFORM BDC_VA02.

        CALL TRANSACTION 'VA02' USING BDC_TAB MODE 'N'
                                      UPDATE 'S'
                                      MESSAGES INTO MESS_TAB.
*&* PERFORM MESSAGE_ADJUST_1 USING 1 BDC_LIST-GUBUN_S.
        READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                                     MSGID  = 'V1'
                                     MSGNR  = '311'.
        IF SY-SUBRC = 0.
          IT_VEH_OR-GUBUN_S = 'S'.
          IT_VEH_OR-MESSAGE = ''.
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
                      MESSAGE_TEXT_OUTPUT = IT_VEH_OR-MESSAGE.
            IT_VEH_OR-GUBUN_S = 'E'.
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
                      MESSAGE_TEXT_OUTPUT = IT_VEH_OR-MESSAGE.
            IT_VEH_OR-GUBUN_S = 'W'.
          ENDIF.
        ENDIF.
*&* PERFORM MESSAGE_ADJUST_1 USING 1 BDC_LIST-GUBUN_S.
      ELSE.
*&* PERFORM BAPI_VA02.
        IT_VEH_OR-MODQTY = IT_VEH_OR-MODQTY - IT_VEH_OR-SEQQTY.
        WRITE IT_VEH_OR-MODQTY TO W_CHAR_MOD.
        WRITE IT_VEH_OR-SEQQTY TO W_CHAR_SEQ.

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
            SALESDOCUMENT               = IT_VEH_OR-SALES
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
                  WAIT   = 'X'
             IMPORTING
                  RETURN = RETURN.
*&* PERFORM BAPI_VA02.
*&* PERFORM MESSAGE_ADJUST_2 USING BDC_LIST-GUBUN_S.
        READ TABLE RETURN WITH KEY TYPE    = 'S'
                                   ID      = 'V1'
                                   NUMBER  = '311'.
        IF SY-SUBRC = 0.
          IT_VEH_OR-GUBUN_S = 'S'.
          IT_VEH_OR-MESSAGE = ''.
        ELSE.
          READ TABLE RETURN WITH KEY TYPE = 'E'.
          IF SY-SUBRC = 0.
            IT_VEH_OR-MESSAGE = RETURN-MESSAGE.
            IT_VEH_OR-GUBUN_S = 'E'.
          ELSE.
            READ TABLE RETURN INDEX 1.
            IT_VEH_OR-MESSAGE = RETURN-MESSAGE.
            IT_VEH_OR-GUBUN_S = 'W'.
          ENDIF.
        ENDIF.
*&* PERFORM MESSAGE_ADJUST_2 USING BDC_LIST-GUBUN_S.
      ENDIF.

    ELSEIF SY-SUBRC = 4. "NOT CHANGE
      EXIT..
    ELSEIF SY-SUBRC = 1. "Quantity decrease is not allowed
      IT_VEH_OR-GUBUN_S = 'E'.
      IT_VEH_OR-MESSAGE = 'Quantity decrease is not allowed'.
    ENDIF.
  ENDIF.

  MODIFY IT_VEH_OR INDEX 1.
  MODIFY ZTSD_VEH_OR FROM TABLE IT_VEH_OR.

ENDFUNCTION.

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
