FUNCTION Z_FRF_MM_LIST_BDC_LT01.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BXCNT) TYPE  ZRF_NSOLM OPTIONAL
*"     VALUE(I_KTOTE) TYPE  CHAR1 OPTIONAL
*"     VALUE(I_FLEXTO) LIKE  SY-UZEIT
*"     VALUE(I_PERNR) LIKE  LTAK-PERNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(ORDERNO) LIKE  LTAP-TANUM
*"  TABLES
*"      T_LIST STRUCTURE  ZSRF_PICKER_LIST
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"      ZTMESSAGE STRUCTURE  ZSMESSAGE
*"----------------------------------------------------------------------
  DATA: L_TABIX LIKE SY-TABIX,
        L_INDEX LIKE SY-INDEX,
        L_COUNT TYPE I.
  DATA: WA_LIST LIKE ZSRF_PICKER_LIST,  " +UD1K942396
        T_LIST1 LIKE ZSRF_PICKER_LIST OCCURS 0 WITH HEADER LINE.
  " +UD1K942396

  DATA: W_BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA: $RDMNG TYPE MLGT_RDMNG.
  DATA NUM(12) VALUE ' 0123456789.'.

*-Start of +UD1K942396
  IF T_LIST[] IS INITIAL.
    E_MESS  = TEXT-M16.
    ZRESULT = TEXT-M02.
    EXIT.
  ENDIF.

* by ig.moon 02/06/2008 {
** Changed by Furong on 09/05/08
*  IF BXCNT CN NUM. BXCNT = SPACE. ENDIF.
** End of change
* }

  SELECT * FROM ZTWM_REFNR_MAP INTO TABLE IT_ZTWM_REFNR_MAP.

  LOOP AT T_LIST INTO WA_LIST.

** Changed by Furong on 09/05/08
    BXCNT  = WA_LIST-NSOLA.
    IF BXCNT CN NUM. BXCNT = SPACE. ENDIF.
** End of change

* by ig.moon 02/06/2008 + {
    IF NOT BXCNT IS INITIAL.

      SELECT SINGLE RDMNG INTO $RDMNG
          FROM MLGT
          WHERE MATNR EQ WA_LIST-MATNR
          AND LVORM EQ SPACE
          AND RDMNG NE 0.

      IF SY-SUBRC EQ 0.
        WA_LIST-NSOLM = BXCNT * $RDMNG.
      ELSE.
        WA_LIST-NSOLM = BXCNT.
      ENDIF.

    ENDIF.
* }
** Changed by Furong on 09/05/08
    CLEAR: WA_LIST-NSOLA.
** End of change

    REFRESH IT_BDCMSGCOLL.
    CLEAR: W_ZDOCNO,
           L_INDEX.
*-End of +UD1K942396

*****************
* PERFORM SAVE_TLIST TABLES T_LIST.
* COMMIT WORK.
**************************************************

    PERFORM NUMBER_GET_NEXT USING    NRO_NR_09     "NRO Interval
                                     NRO_OBJECT    "NRO Object
                            CHANGING W_ZDOCNO.     "App. Doc. No.
    COMMIT WORK.

    CLEAR: L_COUNT, ORDERNO, W_SUBRC.

*-Start of -UD1K942396
*    IF t_list[] IS INITIAL.
*      e_mess  = text-m16.
*      zresult = text-m02.
*      EXIT.
*    ENDIF.
*-End of -UD1K942396

*    PERFORM bdc_processing_rf_lt01 TABLES   t_list        " -UD1K942396
*                                            it_bdcmsgcoll " -UD1K942396
    PERFORM BDC_PROCESSING_RF_LT01 TABLES    IT_BDCMSGCOLL " +UD1K942396
                                               USING:     W_ZDOCNO
                                                          I_KTOTE
                                                          I_FLEXTO
                                                          I_PERNR
                                               CHANGING: W_SUBRC
                                             WA_LIST       " +UD1K942396
                                                         ORDERNO.
    IF W_SUBRC NE 0.
      L_INDEX = L_INDEX + 1.
    ENDIF.
*-Start of changes +UD1K942396
    IF W_SUBRC = 3.
      E_MESS  = TEXT-M25.
      ZRESULT = TEXT-M02.
      ZTMESSAGE-MATNR = WA_LIST-MATNR.
      ZTMESSAGE-ORDERNO = WA_LIST-TANUM.
      ZTMESSAGE-ZRESULT = TEXT-M02.
      ZTMESSAGE-E_MESS = TEXT-M25.
      ZTMESSAGE-REFNR = WA_LIST-REFNR.
      ZTMESSAGE-VLTYP = WA_LIST-VLTYP.
      ZTMESSAGE-NLTYP = WA_LIST-NLTYP.
      APPEND ZTMESSAGE.
      MODIFY T_LIST FROM WA_LIST.
** Changed by Furong on 09/15/08
*      IF WA_LIST-TANUM IS INITIAL.
*        EXIT.
*      ELSE.
*        CONTINUE.
*      ENDIF.
      CONTINUE.
    ENDIF.

** End of change
*-End of changes +UD1K942396
    IF IT_BDCMSGCOLL[] IS INITIAL.
      IF W_SUBRC <> 3.        " +UD1K942396
        E_MESS  = TEXT-W11.
        ZRESULT = TEXT-M02.
*-Start of changes +UD1K942396
        ZTMESSAGE-MATNR = WA_LIST-MATNR.
        ZTMESSAGE-ORDERNO = WA_LIST-TANUM.
        ZTMESSAGE-ZRESULT = TEXT-M02.
        ZTMESSAGE-E_MESS = TEXT-W11.
        ZTMESSAGE-REFNR = WA_LIST-REFNR.
        ZTMESSAGE-VLTYP = WA_LIST-VLTYP.
        ZTMESSAGE-NLTYP = WA_LIST-NLTYP.
        APPEND ZTMESSAGE.
        MODIFY T_LIST FROM WA_LIST.
** Changed by Furong on 09/15/08
*        IF WA_LIST-TANUM IS INITIAL.
*          EXIT.
*        ELSE.
*          CONTINUE.
*        ENDIF.
        CONTINUE.
      ENDIF.
** End of change
*-End of changes +UD1K942396
    ELSE.
      PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                   T_MESSAGE.
      CASE L_INDEX.
        WHEN 0.
          E_MESS  = TEXT-M22.
          ZRESULT = TEXT-M04.
*-Start of changes +UD1K942396
          ZTMESSAGE-MATNR = WA_LIST-MATNR.
          ZTMESSAGE-ORDERNO = WA_LIST-TANUM.
          ZTMESSAGE-ZRESULT = TEXT-M04.
          ZTMESSAGE-E_MESS = TEXT-M22.
          ZTMESSAGE-REFNR = WA_LIST-REFNR.
          ZTMESSAGE-VLTYP = WA_LIST-VLTYP.
          ZTMESSAGE-NLTYP = WA_LIST-NLTYP.
          APPEND ZTMESSAGE.
          REFRESH T_LIST1.
          MOVE WA_LIST TO T_LIST1.
          APPEND T_LIST1.
          CLEAR  T_LIST1.
*-End of changes +UD1K942396
*          PERFORM middleware_update TABLES t_list.  " -UD1K942396
          PERFORM MIDDLEWARE_UPDATE TABLES T_LIST1.  " +UD1K942396
*       WHEN 1.
*          E_MESS  = T_MESSAGE.
*          ZRESULT =  TEXT-M02.
        WHEN OTHERS.
          IF W_SUBRC = '2'.
            E_MESS  = T_MESSAGE.
            ZRESULT =  TEXT-M23.
*-Start of changes +UD1K942396
            ZTMESSAGE-MATNR = WA_LIST-MATNR.
            ZTMESSAGE-ORDERNO = WA_LIST-TANUM.
            ZTMESSAGE-ZRESULT = TEXT-M23.
            ZTMESSAGE-E_MESS = T_MESSAGE.
            ZTMESSAGE-REFNR = WA_LIST-REFNR.
            ZTMESSAGE-VLTYP = WA_LIST-VLTYP.
            ZTMESSAGE-NLTYP = WA_LIST-NLTYP.
            APPEND ZTMESSAGE.
*-End of changes +UD1K942396
          ELSE.
            E_MESS  = T_MESSAGE.
            ZRESULT =  TEXT-M02.
*-Start of changes +UD1K942396
            ZTMESSAGE-MATNR = WA_LIST-MATNR.
            ZTMESSAGE-ORDERNO = WA_LIST-TANUM.
            ZTMESSAGE-ZRESULT = TEXT-M02.
            ZTMESSAGE-E_MESS = T_MESSAGE.
            ZTMESSAGE-REFNR = WA_LIST-REFNR.
            ZTMESSAGE-VLTYP = WA_LIST-VLTYP.
            ZTMESSAGE-NLTYP = WA_LIST-NLTYP.
            APPEND ZTMESSAGE.
*-End of changes +UD1K942396
          ENDIF.
      ENDCASE.
    ENDIF.
    MODIFY T_LIST FROM WA_LIST.        " +UD1K942396
** Changed by Furong on 09/15/08
*    IF WA_LIST-TANUM IS INITIAL.
*      EXIT.
*    ELSE.
*      CONTINUE.
*    ENDIF.
    CONTINUE.
** End of change
  ENDLOOP.  " LOOP AT t_list INTO wa_list. +UD1K942396
ENDFUNCTION.
