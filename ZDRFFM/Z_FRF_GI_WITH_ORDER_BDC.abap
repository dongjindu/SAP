FUNCTION Z_FRF_GI_WITH_ORDER_BDC .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_ORDER) TYPE  ZSRF_OD_DISP OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(E_AUFNR) LIKE  ZSPM_BAKO-AUFNR
*"     VALUE(E_MBLNR) LIKE  MSEG-MBLNR
*"     VALUE(E_MBLNR1) LIKE  MSEG-MBLNR
*"  TABLES
*"      T_ORDER STRUCTURE  ZSRF_GI_WITH_ORDER
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"      T_ERROR STRUCTURE  ZSRF_AMOUNT_CHECK
*"----------------------------------------------------------------------
*  DATA: I_CHANGED_LOG(15) VALUE 'NOT-CHANGED'.
  DATA: L_LINES TYPE SY-TABIX,
        L_LINE1 TYPE SY-TABIX,
        L_TABIX TYPE SY-TABIX,
        L_BDMNG(18),
        L_BLDAT(10),
        L_E_MESS  TYPE BAPI_MSG,
        L_ZRESULT TYPE ZRESULT.
  DATA: LA_ORDER LIKE T_ORDER,
        LT_ORDER LIKE T_ORDER OCCURS 0 WITH HEADER LINE,
        MT_ORDER LIKE T_ORDER OCCURS 0 WITH HEADER LINE,
        LT_ORDER_CHAN LIKE T_ORDER OCCURS 0 WITH HEADER LINE,
        LT_OR_CHECK LIKE ZSRF_GI_WITH_ORDER OCCURS 0 WITH HEADER LINE,
        LT_ITEM_CHECK LIKE ZSRF_AMOUNT_CHECK OCCURS 0 WITH HEADER LINE.
* BDC Messages
  DATA: WA_TEXT(200).
  DATA: WA_MSGNR LIKE SY-MSGNO,
        WA_MSGID LIKE SY-MSGID,
        WA_MSGV1 LIKE SY-MSGV1,
        WA_MSGV2 LIKE SY-MSGV2,
        WA_MSGV3 LIKE SY-MSGV3,
        WA_MSGV4 LIKE SY-MSGV4.
* Refresh T_MESSAGE
  CLEAR: T_MESSAGE[], T_MESSAGE.
  CLEAR: LT_ORDER_CHAN, LT_ORDER.
  REFRESH: LT_ORDER_CHAN, LT_ORDER.
  PERFORM WA_OPT_OPTION.
* BDC STRUCTURE REFRESH
  PERFORM REFRESH_BDC_STRUCTURE.
  REFRESH T_ERROR. CLEAR T_ERROR.
  CLEAR L_ZRESULT.

*  CALL FUNCTION 'Y_YTEST_KBS_TEST'
*       TABLES
*            T_OSORDER = T_ORDER.


  LOOP AT T_ORDER.
    MOVE-CORRESPONDING T_ORDER TO LT_ITEM_CHECK.
    COLLECT LT_ITEM_CHECK . CLEAR LT_ITEM_CHECK .
  ENDLOOP.

  CALL FUNCTION 'Z_FRF_MARD_CHEDK'
       IMPORTING
            ZRESULT = L_ZRESULT
       TABLES
            T_CHECK = LT_ITEM_CHECK
            T_ERROR = T_ERROR.
  REFRESH LT_ITEM_CHECK. CLEAR LT_ITEM_CHECK.

  LOOP AT T_ORDER.
    L_TABIX = SY-TABIX.
    IF T_ORDER-ZCHAN EQ 'S'.                                "TEXT-T03.
      LT_ORDER_CHAN = T_ORDER.
      APPEND LT_ORDER_CHAN.
    ELSEIF T_ORDER-ZCHAN EQ 'O'.                            "TEXT-T02.
      LT_ORDER = T_ORDER.
      APPEND LT_ORDER.
    ENDIF.
    CLEAR: LT_ORDER, LT_ORDER_CHAN, T_ORDER.
  ENDLOOP.
* REFRESH & CLEAR
  REFRESH T_ORDER. CLEAR T_ORDER.
* ORDER LINES COUNT CHECKING
  CALL FUNCTION 'Z_FRF_GI_WITH_ORDER_ITEM'
       EXPORTING
            I_ORDER = I_ORDER
       IMPORTING
            E_MESS  = L_E_MESS
            ZRESULT = L_ZRESULT
       TABLES
            T_ORDER = LT_OR_CHECK.

  DESCRIBE TABLE LT_OR_CHECK LINES L_LINE1.

  DESCRIBE TABLE LT_ORDER LINES L_LINES.

  IF L_LINE1 NE L_LINES.

    REFRESH MT_ORDER. CLEAR MT_ORDER.
    MT_ORDER[] = LT_ORDER[].
    REFRESH LT_ORDER. CLEAR LT_ORDER.

    LOOP AT LT_OR_CHECK.
      READ TABLE MT_ORDER WITH KEY AUFNR = LT_OR_CHECK-AUFNR
                                   RSPOS = LT_OR_CHECK-RSPOS.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING MT_ORDER TO LT_ORDER.
        APPEND LT_ORDER. CLEAR LT_ORDER.
      ELSE.
        MOVE-CORRESPONDING LT_OR_CHECK TO LT_ORDER.
        LT_ORDER-BDMNG = 0.
        APPEND LT_ORDER. CLEAR LT_ORDER.
      ENDIF.

    ENDLOOP.
  ELSE.

  ENDIF.
*
*  CHECK ZRESULT EQ TEXT-M03.
  IF NOT LT_ORDER[] IS INITIAL.
    CLEAR: L_LINE1, L_LINES.
    DESCRIBE TABLE LT_ORDER LINES L_LINES.
    LOOP AT LT_ORDER WHERE BDMNG EQ 0.
      L_LINE1 = L_LINE1 + 1.
    ENDLOOP.
    IF L_LINES NE L_LINE1.

      SORT LT_ORDER BY RSPOS.

      LOOP AT LT_ORDER.
        L_TABIX = SY-TABIX.
        LA_ORDER = LT_ORDER.
        CLEAR L_BDMNG.
        WRITE: LA_ORDER-BDMNG TO L_BDMNG LEFT-JUSTIFIED.
        WRITE: LA_ORDER-BUDAT TO L_BLDAT.
        AT FIRST.
          PERFORM DYNPRO USING:
                  'X' 'SAPMM07M'        '0400',
                  ' ' 'MKPF-BLDAT'      L_BLDAT,
                  ' ' 'MKPF-BUDAT'      L_BLDAT,
                  ' ' 'XFULL'           ' ',
                  ' ' 'BDC_OKCODE'      '=NFAL', "TO ORDER

                  'X' 'SAPMM07M'        '1405',
                  ' ' 'RM07M-AUFNR(01)' LA_ORDER-AUFNR,
                  ' ' 'BDC_OKCODE'      '=OK',

                  'X' 'SAPMM07M'        '0421',
                  ' ' 'MSEGK-WEMPF'     LA_ORDER-WEMPF,
                  ' ' 'BDC_CURSOR'      'RM07M-XSELK(01)',
                  ' ' 'BDC_OKCODE'      '=SP'.
        ENDAT.
        PERFORM DYNPRO USING:
                'X' 'SAPMM07M'        '0410',
                ' ' 'MSEG-ERFMG'      L_BDMNG,
                ' ' 'MSEG-LGORT'      LA_ORDER-LGORT,
                ' ' 'BDC_OKCODE'      '/00'.
*      IF LA_ORDER-BDMNG EQ 0.
*        PERFORM DYNPRO USING:
*                'X' 'SAPMM07M'        '0410',
*                ' ' 'BDC_OKCODE'      '/00'.
*      ENDIF.
        AT LAST.
          PERFORM DYNPRO USING:
                  'X' 'SAPMM07M'        '0420',
                  ' ' 'BDC_OKCODE'      '=BU'.

*       CALL TRANSACTION
          CALL TRANSACTION 'MB11'  USING IT_BDC
                                   OPTIONS FROM WA_OPT
                                   MESSAGES INTO IT_MESS.
        ENDAT.

      ENDLOOP.

      LOOP AT IT_MESS.
        CLEAR: WA_TEXT.
        MOVE:   IT_MESS-MSGNR TO WA_MSGNR,
                IT_MESS-MSGID TO WA_MSGID,
                IT_MESS-MSGV1 TO WA_MSGV1,
                IT_MESS-MSGV2 TO WA_MSGV2,
                IT_MESS-MSGV3 TO WA_MSGV3,
                IT_MESS-MSGV4 TO WA_MSGV4.

        CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
             EXPORTING
                  LANGU = SY-LANGU
                  MSGID = WA_MSGID
                  MSGNO = WA_MSGNR
                  MSGV1 = WA_MSGV1
                  MSGV2 = WA_MSGV2
                  MSGV3 = WA_MSGV3
                  MSGV4 = WA_MSGV4
             IMPORTING
                  TEXT  = WA_TEXT.
        CLEAR:  T_MESSAGE.
        MOVE : WA_TEXT TO T_MESSAGE-MESSAGE,
               IT_MESS-MSGTYP TO T_MESSAGE-TYPE,
               IT_MESS-MSGID  TO T_MESSAGE-ID,
               IT_MESS-MSGNR  TO T_MESSAGE-NUMBERS.
        APPEND T_MESSAGE.
      ENDLOOP.
      IF NOT IT_MESS[] IS INITIAL.
        READ TABLE IT_MESS WITH KEY MSGTYP = 'S'
                                    MSGID  = 'M7'
                                    MSGNR  = '060'.
*    P_SUBRC = SY-SUBRC.
        IF SY-SUBRC EQ 0.
*          COMMIT WORK.
          E_MBLNR = IT_MESS-MSGV1(10).
          ZRESULT = TEXT-M03.
*      E_MESS  = TEXT-M10.
          CONCATENATE E_MBLNR TEXT-M10 INTO E_MESS SEPARATED BY SPACE.
          LOOP AT LT_ORDER.
            MOVE-CORRESPONDING LT_ORDER TO T_ORDER.
            T_ORDER-EXECUTED_FLG = 'X'.
            APPEND T_ORDER. CLEAR T_ORDER.
          ENDLOOP.
        ELSE.
          PERFORM PM_LOG_TABLE_INSERT USING 'GI WITH ORDER'
                                            LA_ORDER-AUFNR
                                            WA_TEXT.

          ZRESULT = TEXT-M04.
          E_MESS  = TEXT-M11.
          LOOP AT LT_ORDER.
            MOVE-CORRESPONDING LT_ORDER TO T_ORDER.
            T_ORDER-EXECUTED_FLG = ' '.
            APPEND T_ORDER. CLEAR T_ORDER.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* BDC STRUCTURE REFRESH
  PERFORM REFRESH_BDC_STRUCTURE.

  IF NOT LT_ORDER_CHAN[] IS INITIAL.
    DESCRIBE TABLE LT_ORDER_CHAN LINES L_LINES.
    LOOP AT LT_ORDER_CHAN.
      L_TABIX = SY-TABIX.
      LA_ORDER = LT_ORDER_CHAN.
      CLEAR L_BDMNG.
      WRITE: LA_ORDER-BDMNG TO L_BDMNG LEFT-JUSTIFIED.
      WRITE: LA_ORDER-BUDAT TO L_BLDAT.

      AT FIRST.
        PERFORM DYNPRO USING:
                'X' 'SAPMM07M'        '0400',
                ' ' 'RM07M-BWARTWA'   LA_ORDER-BWART, "Movement type
                ' ' 'RM07M-WERKS'     LA_ORDER-WERKS, "Plant
                ' ' 'MKPF-BLDAT'      L_BLDAT, "Document date
                ' ' 'MKPF-BUDAT'      L_BLDAT, "Posting date
                ' ' 'XFULL'           ' ',
                ' ' 'BDC_OKCODE'      '=NPE'. "TO ORDER
      ENDAT.

      IF L_TABIX NE L_LINES.
        PERFORM DYNPRO USING:
                'X' 'SAPMM07M'        '0410',
                ' ' 'MSEG-MATNR'      LA_ORDER-MATNR,
                ' ' 'MSEG-ERFMG'      L_BDMNG,
                ' ' 'MSEG-LGORT'      LA_ORDER-LGORT,
                ' ' 'MSEG-WEMPF'      LA_ORDER-WEMPF,
                ' ' 'COBL-AUFNR'      LA_ORDER-AUFNR,
                ' ' 'BDC_OKCODE'      '=NPE'.

      ELSE.
        PERFORM DYNPRO USING:
                'X' 'SAPMM07M'        '0410',
                ' ' 'MSEG-MATNR'      LA_ORDER-MATNR,
                ' ' 'MSEG-ERFMG'      L_BDMNG,
                ' ' 'MSEG-LGORT'      LA_ORDER-LGORT,
                ' ' 'MSEG-WEMPF'      LA_ORDER-WEMPF,
                ' ' 'COBL-AUFNR'      LA_ORDER-AUFNR,
                ' ' 'BDC_OKCODE'      '=BU'.

*       CALL TRANSACTION
        CALL TRANSACTION 'MB11'  USING IT_BDC
                                 OPTIONS FROM WA_OPT
                                 MESSAGES INTO IT_MESS.
      ENDIF.

    ENDLOOP.

    LOOP AT IT_MESS.
      CLEAR: WA_TEXT.
      MOVE:   IT_MESS-MSGNR TO WA_MSGNR,
              IT_MESS-MSGID TO WA_MSGID,
              IT_MESS-MSGV1 TO WA_MSGV1,
              IT_MESS-MSGV2 TO WA_MSGV2,
              IT_MESS-MSGV3 TO WA_MSGV3,
              IT_MESS-MSGV4 TO WA_MSGV4.

      CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
           EXPORTING
                LANGU = SY-LANGU
                MSGID = WA_MSGID
                MSGNO = WA_MSGNR
                MSGV1 = WA_MSGV1
                MSGV2 = WA_MSGV2
                MSGV3 = WA_MSGV3
                MSGV4 = WA_MSGV4
           IMPORTING
                TEXT  = WA_TEXT.
      CLEAR:  T_MESSAGE.
      MOVE : WA_TEXT TO T_MESSAGE-MESSAGE,
             IT_MESS-MSGTYP TO T_MESSAGE-TYPE,
             IT_MESS-MSGID  TO T_MESSAGE-ID,
             IT_MESS-MSGNR  TO T_MESSAGE-NUMBERS.
      APPEND T_MESSAGE.
    ENDLOOP.
    IF NOT IT_MESS[] IS INITIAL.
      READ TABLE IT_MESS WITH KEY MSGTYP = 'S'
                                  MSGID  = 'M7'
                                  MSGNR  = '060'.
*    P_SUBRC = SY-SUBRC.
      IF SY-SUBRC EQ 0.
*        COMMIT WORK.
        E_MBLNR1 = IT_MESS-MSGV1(10).
        ZRESULT = TEXT-M03.
        CONCATENATE E_MESS E_MBLNR1 TEXT-M10
                               INTO E_MESS SEPARATED BY SPACE.

        LOOP AT LT_ORDER_CHAN.
          MOVE-CORRESPONDING LT_ORDER_CHAN TO T_ORDER.
          T_ORDER-EXECUTED_FLG = 'X'.
          APPEND T_ORDER. CLEAR T_ORDER.
        ENDLOOP.
      ELSE.
        PERFORM PM_LOG_TABLE_INSERT USING 'GI WITH ORDER'
                                          LA_ORDER-AUFNR
                                          WA_TEXT.
        ZRESULT = TEXT-M04.
        CONCATENATE E_MESS TEXT-M11
                           INTO E_MESS SEPARATED BY SPACE.
        LOOP AT LT_ORDER_CHAN.
          MOVE-CORRESPONDING LT_ORDER_CHAN TO T_ORDER.
          T_ORDER-EXECUTED_FLG = ' '.
          APPEND T_ORDER. CLEAR T_ORDER.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
