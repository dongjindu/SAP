FUNCTION Z_FRF_MM_LIST_BDC_LT12.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_TCODE) TYPE  SY-TCODE
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_LIST STRUCTURE  ZSRF_PICKER_LIST
*"      T_LT12 STRUCTURE  ZSRF_LT12_CHECK OPTIONAL
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"----------------------------------------------------------------------
  DATA: L_TABIX LIKE SY-TABIX,
        L_INDEX LIKE SY-INDEX,
        L_COUNT TYPE I.
  DATA: W1_TO_QTY LIKE LTAP-VSOLA.
  DATA: W1_PICK_QTY LIKE LTAP-VSOLA.


  data: w_bdcmsgcoll like bdcmsgcoll OCCURS 0 WITH HEADER LINE.

** FOR TEST ONLY
*  DATA: LT_TEST_RF LIKE TABLE OF ZTMM_TEST_RF WITH HEADER LINE.
*  LOOP AT T_LIST.
*  MOVE-CORRESPONDING T_LIST TO LT_TEST_RF.
*  APPEND LT_TEST_RF.
*  ENDLOOP.
*  MODIFY ZTMM_TEST_RF FROM TABLE LT_TEST_RF.
** END OF CHANGE
  CLEAR W_CONFIRMATION.
*  CASE I_TCODE.
*    WHEN 'ZMME88'.
*      W_CONFIRMATION = 'PICK'.
*    WHEN 'ZMME89'.
*      W_CONFIRMATION = 'TRANSFER'.
*    WHEN 'ZMME88S'.
*      W_CONFIRMATION = 'PICK'.
*  ENDCASE.
*/ Begin of Added by Hakchin (20040119)
*/ Begin of BDC Processing
* Application Doc No.

**

*****************  perform save_tlist tables t_list.
* PERFORM SAVE_TLIST TABLES T_LIST.
* COMMIT WORK.
**************************************************

  PERFORM NUMBER_GET_NEXT USING    NRO_NR_09     "NRO Interval
                                   NRO_OBJECT    "NRO Object
                          CHANGING W_ZDOCNO.     "App. Doc. No.
  COMMIT WORK.
  CLEAR L_COUNT.

*  LOOP AT T_LIST INTO WA_LTXX WHERE CHK          EQ 'X'
*                              AND   EXECUTED_FLG NE 'X'.
  LOOP AT T_LIST INTO WA_LTXX WHERE CHK          NE '4'.

    L_TABIX = SY-TABIX.
    L_COUNT = L_COUNT + 1.
    CASE WA_LTXX-CHK.
      WHEN '1'.
        W_CONFIRMATION = 'PICK'.
      WHEN '2'.
        W_CONFIRMATION = 'TRANSFER'.
      WHEN '3'.
        W_CONFIRMATION = 'PK_TR'.
    ENDCASE.

*   Confirm Transfer Order
    PERFORM BDC_PROCESSING_LT12 TABLES   IT_BDCMSGCOLL
                                USING    W_ZDOCNO
                                         W_CONFIRMATION
                                CHANGING W_SUBRC.
    IF W_SUBRC = 0.
*    CLEAR: IO_SQTY.  "Scanned Qty.
*      WA_LTXX-EXECUTED_FLG = 'C'.

*      IF I_TCODE EQ 'ZMME88' OR
*         I_TCODE EQ 'ZMME88S'.
*        WA_LTXX-PVQUI = 'X'.
*      ELSEIF I_TCODE EQ 'ZMME89'.
*        WA_LTXX-PQUIT = 'X'.
*      ENDIF.
      CASE WA_LTXX-CHK.
        WHEN '1'.  "PICK
          WA_LTXX-PVQUI = 'X'.
        WHEN '2'.  "TRANSFER
          WA_LTXX-PQUIT = 'X'.
          WA_LTXX-CHK   = '4'.
        WHEN '3'.  "PICK + TRANSFER
          WA_LTXX-PVQUI = 'X'.
          WA_LTXX-PQUIT = 'X'.
          WA_LTXX-CHK   = '4'.
      ENDCASE.

      MODIFY T_LIST  FROM WA_LTXX
                      INDEX L_TABIX
                      TRANSPORTING PVQUI
                                   PQUIT
*                                   EXECUTED_FLG
                                   CHK.

*** INSERTED BY FURONG ??????????????
*  clear W_bdcmsgcoll.
*  MOVE 'LA12' TO w_bdcmsgcoll-TCODE.
*  CONCATENATE  WA_LTXX-CHK WA_LTXX-NSOLM WA_LTXX-NISTA
*                         INTO w_bdcmsgcoll-MSGv1 SEPARATED BY SPACE.
*  APPEND w_bdcmsgcoll.
*  PERFORM ADD_TO_TEMP_LOG TABLES w_bdcmsgcoll.
*  clear W_BDcmsgcoll.
*  REFRESH W_BDcmsgcoll.
*  COMMIT WORK.
*******************


      IF WA_LTXX-CHK = '1'.  "PICK
         W1_TO_QTY = WA_LTXX-NSOLM.
         W1_PICK_QTY = WA_LTXX-NISTA.
*         IF W1_TO_QTY > W1_PICK_QTY.
*            PERFORM CREATE_LT01_1A_SHORT TABLES T_LIST
*                                                T_MESSAGE
*                                         USING  W_ZDOCNO.
*         ENDIF.
      ENDIF.
    ELSE.
*      WA_LTXX-EXECUTED_FLG = 'N'.
*      IF I_TCODE EQ 'ZMME88' OR
*         I_TCODE EQ 'ZMME88S'.
*        WA_LTXX-PVQUI = ' '.
*      ELSEIF I_TCODE EQ 'ZMME89'.
*        WA_LTXX-PQUIT = ' '.
*      ENDIF.
      CASE WA_LTXX-CHK.
        WHEN '1'.  "PICK
          WA_LTXX-PVQUI = ' '.
        WHEN '2'.  "TRANSFER
          WA_LTXX-PQUIT = ' '.
        WHEN '3'.  "PICK + TRANSFER
          IF WA_LTXX-PVQUI EQ 'X'.
            WA_LTXX-PQUIT = ' '.
          ELSE.
            WA_LTXX-PVQUI = ' '.
            WA_LTXX-PQUIT = ' '.
          ENDIF.
      ENDCASE.
      MODIFY T_LIST  FROM WA_LTXX
                      INDEX L_TABIX
                      TRANSPORTING PVQUI
                                   PQUIT
                                   EXECUTED_FLG.
      L_INDEX = L_INDEX + 1.
    ENDIF.

    PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                  T_MESSAGE.
    MOVE-CORRESPONDING WA_LTXX TO T_LT12.
    T_LT12-MESSAGE = T_MESSAGE-MESSAGE.
    APPEND T_LT12.
    CLEAR WA_LTXX.
  ENDLOOP.

  IF NOT L_COUNT IS INITIAL.
    CASE L_INDEX.
      WHEN 0.
        E_MESS  = TEXT-M22.
        ZRESULT = TEXT-M04.
        PERFORM MIDDLEWARE_UPDATE TABLES T_LIST.

      WHEN 1.
        E_MESS  = T_LT12-MESSAGE.
        ZRESULT =  TEXT-M02.
      WHEN OTHERS.
        E_MESS  = T_LT12-MESSAGE.
        ZRESULT =  TEXT-M02.
    ENDCASE.

*    IF NOT T_LT12[] IS INITIAL.
*      DELETE T_LT12 WHERE EXECUTED_FLG NE 'N'.
*    ELSE.
*      E_MESS  = TEXT-M16.
*      ZRESULT = TEXT-M02.
*    ENDIF.
  ELSE.
    E_MESS  = TEXT-M16.
    ZRESULT = TEXT-M02.
  ENDIF.
ENDFUNCTION.
