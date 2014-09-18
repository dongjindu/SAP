FUNCTION Z_FRF_GI_PLANT_TO_PLANT .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_AUFNR) LIKE  ZSPM_BAKO-AUFNR
*"     VALUE(E_MBLNR) LIKE  MSEG-MBLNR
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_PLTOPL STRUCTURE  ZSRF_TRANSFER_PL_TO_PL
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"      T_ERROR STRUCTURE  ZSRF_AMOUNT_CHECK
*"----------------------------------------------------------------------
  DATA: L_LINES TYPE SY-TABIX,
        L_TABIX TYPE SY-TABIX,
        L_BDMNG(18),
        L_BLDAT(10).
  DATA: LA_PLTOPL LIKE T_PLTOPL,
        LT_ITEM_CHECK LIKE ZSRF_AMOUNT_CHECK OCCURS 0 WITH HEADER LINE.

* BDC Messages
  DATA: WA_TEXT(200).
  DATA: WA_MSGNR LIKE SY-MSGNO,
        WA_MSGID LIKE SY-MSGID,
        WA_MSGV1 LIKE SY-MSGV1,
        WA_MSGV2 LIKE SY-MSGV2,
        WA_MSGV3 LIKE SY-MSGV3,
        WA_MSGV4 LIKE SY-MSGV4,
        L_ZRESULT TYPE ZRESULT.
  PERFORM WA_OPT_OPTION.
* BDC STRUCTURE REFRESH
  PERFORM REFRESH_BDC_STRUCTURE.

  LOOP AT T_PLTOPL.
    MOVE-CORRESPONDING T_PLTOPL TO LT_ITEM_CHECK.
    LT_ITEM_CHECK-BDMNG = T_PLTOPL-MENGE.
    LT_ITEM_CHECK-ERFME = T_PLTOPL-MEINS.
    COLLECT LT_ITEM_CHECK . CLEAR LT_ITEM_CHECK .
  ENDLOOP.

  CALL FUNCTION 'Z_FRF_MARD_CHEDK'
       IMPORTING
            ZRESULT = L_ZRESULT
       TABLES
            T_CHECK = LT_ITEM_CHECK
            T_ERROR = T_ERROR.
  REFRESH LT_ITEM_CHECK. CLEAR LT_ITEM_CHECK.

  CHECK L_ZRESULT EQ '0'.


* Refresh T_MESSAGE
  CLEAR: T_MESSAGE[], T_MESSAGE.
  DESCRIBE TABLE T_PLTOPL LINES L_LINES.
  LOOP AT T_PLTOPL.
    L_TABIX = SY-TABIX.
    LA_PLTOPL = T_PLTOPL.
    WRITE: LA_PLTOPL-BUDAT TO L_BLDAT.
    AT FIRST.
      PERFORM DYNPRO USING:
              'X' 'SAPMM07M'        '0400',
              ' ' 'RM07M-BWARTWA'   LA_PLTOPL-BWART, "Movement type
              ' ' 'RM07M-WERKS'     LA_PLTOPL-WERKS, "Plant
              ' ' 'MKPF-BLDAT'      L_BLDAT, "Document date
              ' ' 'MKPF-BUDAT'      L_BLDAT, "Posting date
              ' ' 'XFULL'           ' ',
              ' ' 'BDC_OKCODE'      '=NPE'. "TO ORDER
    ENDAT.
    IF L_TABIX NE L_LINES.
      PERFORM DYNPRO USING:
              'X' 'SAPMM07M'        '0410',
              ' ' 'MSEG-MATNR'      LA_PLTOPL-MATNR,
              ' ' 'MSEG-ERFMG'      LA_PLTOPL-MENGE,
              ' ' 'MSEG-WERKS'      LA_PLTOPL-WERKS,
              ' ' 'MSEG-LGORT'      LA_PLTOPL-LGORT,
              ' ' 'MSEG-UMWRK'      LA_PLTOPL-UMWRK,
              ' ' 'MSEG-UMLGO'      LA_PLTOPL-UMLGO,
              ' ' 'MSEG-WEMPF'      LA_PLTOPL-WEMPF,
              ' ' 'BDC_OKCODE'      '=NPE'.

*              'X' 'SAPLKACB'        '0002',
*              ' ' 'BDC_OKCODE'      '=ENTE'.
    ELSE.
      PERFORM DYNPRO USING:
              'X' 'SAPMM07M'        '0410',
              ' ' 'MSEG-MATNR'      LA_PLTOPL-MATNR,
              ' ' 'MSEG-ERFMG'      LA_PLTOPL-MENGE,
              ' ' 'MSEG-LGORT'      LA_PLTOPL-LGORT,
              ' ' 'MSEG-UMWRK'      LA_PLTOPL-UMWRK,
              ' ' 'MSEG-UMLGO'      LA_PLTOPL-UMLGO,
              ' ' 'MSEG-WERKS'      LA_PLTOPL-WERKS,
              ' ' 'MSEG-WEMPF'      LA_PLTOPL-WEMPF,
              ' ' 'BDC_OKCODE'      '=BU'.

*              'X' 'SAPLKACB'        '0002',
*              ' ' 'BDC_OKCODE'      '=ENTE'.
*     CALL TRANSACTION
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
  READ TABLE IT_MESS WITH KEY MSGTYP = 'S'
                              MSGID  = 'M7'
                              MSGNR  = '060'.
*    P_SUBRC = SY-SUBRC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    E_MBLNR = IT_MESS-MSGV1(10).
    E_MESS  = IT_MESS-MSGV1.
    ZRESULT = TEXT-M03.
  ELSE.
    PERFORM PM_LOG_TABLE_INSERT USING 'PLANT TO PLANT'
                                      ' '
                                      WA_TEXT.
    E_MESS  = WA_TEXT.
    ZRESULT = TEXT-M04.
  ENDIF.





ENDFUNCTION.
