FUNCTION Z_FRF_ONE_STEP_WORK_ORDER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_AUFNR) LIKE  ZSPM_BAKO-AUFNR
*"     VALUE(E_MBLNR) LIKE  MSEG-MBLNR
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_OSORDER STRUCTURE  ZSRF_ONE_STEP_ORDER
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"      T_ERROR STRUCTURE  ZSRF_AMOUNT_CHECK
*"----------------------------------------------------------------------
  DATA: IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
  DATA: WA_TEXT(200).
  DATA: WA_MSGNR LIKE SY-MSGNO,
        WA_MSGID LIKE SY-MSGID,
        WA_MSGV1 LIKE SY-MSGV1,
        WA_MSGV2 LIKE SY-MSGV2,
        WA_MSGV3 LIKE SY-MSGV3,
        WA_MSGV4 LIKE SY-MSGV4.
*
  DATA : WA_ORDERID TYPE AUFNR.
  DATA : WA_SUBRC LIKE SY-SUBRC.
  DATA : WA_MODE VALUE 'N'.
  DATA: IT_ZSPM_RESBD LIKE ZSPM_RESBD OCCURS 0 WITH HEADER LINE,
        LT_ITEM_CHECK LIKE ZSRF_AMOUNT_CHECK OCCURS 0 WITH HEADER LINE.
  DATA: L_TPLNR LIKE ZSPM_BAKO-TPLNR,
        L_EQUNR LIKE ZSPM_BAKO-EQUNR,
        L_KTEXT LIKE ZSPM_BAKO-KTEXT,
        L_WEMPF LIKE ZSPM_BAKO-WEMPF,
        L_GSTRP LIKE ZSPM_BAKO-BUDAT,
        L_ZRESULT TYPE ZRESULT.

  LOOP AT T_OSORDER .
    MOVE-CORRESPONDING T_OSORDER TO LT_ITEM_CHECK.
    LT_ITEM_CHECK-BDMNG = T_OSORDER-MENGE.
    COLLECT LT_ITEM_CHECK . CLEAR LT_ITEM_CHECK .
  ENDLOOP.

*
  CALL FUNCTION 'Z_FRF_MARD_CHEDK'
       IMPORTING
            ZRESULT = L_ZRESULT
       TABLES
            T_CHECK = LT_ITEM_CHECK
            T_ERROR = T_ERROR.

  REFRESH LT_ITEM_CHECK. CLEAR LT_ITEM_CHECK.

  CHECK L_ZRESULT EQ '0'.

  CLEAR: IT_ZSPM_RESBD, IT_ZSPM_RESBD[], WA_ORDERID.

  LOOP AT T_OSORDER.
    MOVE-CORRESPONDING T_OSORDER TO IT_ZSPM_RESBD.
    APPEND IT_ZSPM_RESBD.
    IF SY-TABIX EQ '1'.
      L_EQUNR = T_OSORDER-EQUNR.
      L_KTEXT = T_OSORDER-KTEXT.
      L_GSTRP = T_OSORDER-BUDAT.
      L_WEMPF = T_OSORDER-WEMPF.
    ENDIF.
    CLEAR: IT_ZSPM_RESBD.
  ENDLOOP.

  CALL FUNCTION 'Z_FPM_CREATE_EMERGENCCY_ORDER'
       EXPORTING
            CTU       = 'X'
            MODE      = WA_MODE
            UPDATE    = 'L'
            PM_AUFART = 'PM02'
            PRIOK     = '1'
*            TPLNR     = L_TPLNR
            EQUNR     = L_EQUNR
            KTEXT     = L_KTEXT
            GSTRP     = L_GSTRP
            ATP_CHECK = ' '
       IMPORTING
            SUBRC     = WA_SUBRC
       TABLES
            RESBD     = IT_ZSPM_RESBD
            MESSTAB   = IT_MESSTAB.

  READ TABLE IT_MESSTAB   WITH KEY MSGTYP = 'S'
                                  MSGID  = '00'
                                  MSGNR  = '344'.
  IF SY-SUBRC EQ 0.
    CLEAR : IT_MESSTAB, IT_MESSTAB[].
    CALL FUNCTION 'Z_FPM_CREATE_EMERGENCCY_ORDER'
         EXPORTING
              CTU       = 'X'
              MODE      = WA_MODE
              UPDATE    = 'L'
              PM_AUFART = 'PM02'
              PRIOK     = '1'
*              TPLNR     = L_TPLNR
              EQUNR     = L_EQUNR
              KTEXT     = L_KTEXT
              GSTRP     = L_GSTRP
              ATP_CHECK = 'X'
         IMPORTING
              SUBRC     = WA_SUBRC
         TABLES
              RESBD     = IT_ZSPM_RESBD
              MESSTAB   = IT_MESSTAB.

  ENDIF.
  LOOP AT IT_MESSTAB.
    CLEAR: WA_TEXT.
    MOVE:   IT_MESSTAB-MSGNR TO WA_MSGNR,
            IT_MESSTAB-MSGID TO WA_MSGID,
            IT_MESSTAB-MSGV1 TO WA_MSGV1,
            IT_MESSTAB-MSGV2 TO WA_MSGV2,
            IT_MESSTAB-MSGV3 TO WA_MSGV3,
            IT_MESSTAB-MSGV4 TO WA_MSGV4.

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
           IT_MESSTAB-MSGTYP TO T_MESSAGE-TYPE,
           IT_MESSTAB-MSGID  TO T_MESSAGE-ID,
           IT_MESSTAB-MSGNR  TO T_MESSAGE-NUMBERS.
    APPEND T_MESSAGE.
  ENDLOOP.
  READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'S'
                                 MSGID  = 'IW'
                                 MSGNR  = '085'.
  WA_SUBRC = SY-SUBRC.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    E_AUFNR = IT_MESSTAB-MSGV1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = E_AUFNR
         IMPORTING
              OUTPUT = E_AUFNR.

*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*         EXPORTING
*              PERCENTAGE = 100
*              TEXT       = TEXT-M05.
    CONCATENATE E_AUFNR TEXT-M16 INTO E_MESS SEPARATED BY SPACE.
*    E_MESS  = TEXT-M16.
    ZRESULT = TEXT-M03.
  ELSE.
    PERFORM PM_LOG_TABLE_INSERT USING 'ONE STEP WORK ORDER'
                                      E_AUFNR
                                      WA_TEXT.
    E_MESS  = TEXT-M17.
    ZRESULT = TEXT-M04.
  ENDIF.

  IF WA_SUBRC EQ 0.
*** Goods movement ( MB11 )
    PERFORM GOODS_MOVEMENT2 TABLES   T_MESSAGE
                            USING    L_WEMPF
                                     L_GSTRP
                                     WA_MODE
                                     E_AUFNR
                            CHANGING WA_SUBRC
                                     E_MBLNR.
    IF WA_SUBRC NE 0.
***   Delete new Order

      PERFORM COMPLETE_ORDER TABLES T_MESSAGE
                             USING  E_AUFNR
                                    WA_MODE
                                    WA_SUBRC.
      PERFORM DELETE_ORDER TABLES T_MESSAGE
                           USING  E_AUFNR
                                  WA_MODE
                                  WA_SUBRC.
      CLEAR E_AUFNR.
      CLEAR E_MBLNR.
    ELSE.
      E_AUFNR = E_AUFNR+2(10).
      CONCATENATE E_MESS E_MBLNR TEXT-M10 INTO E_MESS
                                           SEPARATED BY SPACE.
      ZRESULT = TEXT-M03.
*      WA_SAVE = 'X'.
*      ZSPM_BAKO-AUFNR = WA_ORDERID.
    ENDIF.
  ENDIF.


ENDFUNCTION.
