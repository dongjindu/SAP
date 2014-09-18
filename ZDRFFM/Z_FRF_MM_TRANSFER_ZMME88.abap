FUNCTION Z_FRF_MM_TRANSFER_ZMME88 .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_REFNR) LIKE  LTAK-REFNR
*"     VALUE(I_TCODE) TYPE  SY-TCODE
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_LIST STRUCTURE  ZSRF_PICKER_LIST
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"----------------------------------------------------------------------
  DATA: L_TABIX TYPE SY-TABIX,
        L_SUBRC TYPE SY-SUBRC.
*/End of Commented by Hakchin(20040209)
*  PERFORM CHECK_REFNR USING I_REFNR. "Check Group
  CALL FUNCTION 'Z_FRF_MM_CHECK_REFNR'
       EXPORTING
            I_REFNR = I_REFNR
       IMPORTING
            E_SUBRC = L_SUBRC.

  IF L_SUBRC <> 0.
    E_MESS  = TEXT-M06.
    ZRESULT = TEXT-M02.
*    MESSAGE S172(L3).
    EXIT.
  ENDIF.

* Get IT_LTXX
*  PERFORM GET_T_LIST TABLES T_LIST
*                     USING  I_REFNR.

  CALL FUNCTION 'Z_FRF_SEL_LTAK_LTAP_LIST'
    EXPORTING
      I_REFNR       = I_REFNR
*   I_STDAT       =
*   I_STUZT       =
      I_TCODE       = I_TCODE
   IMPORTING
     E_MESS        = E_MESS
     ZRESULT       = ZRESULT
    TABLES
      T_LIST        = T_LIST.


**/ Begin of Added by Hakchin (20040119)
**/ Begin of BDC Processing
** Application Doc No.
*  PERFORM NUMBER_GET_NEXT USING    NRO_NR_09     "NRO Interval
*                                   NRO_OBJECT    "NRO Object
*                          CHANGING W_ZDOCNO.     "App. Doc. No.
*  COMMIT WORK.


  CALL FUNCTION 'Z_FRF_MM_LIST_BDC_LT12'
       EXPORTING
            I_TCODE         = I_TCODE
       IMPORTING
            E_MESS    = E_MESS
            ZRESULT   = ZRESULT
       TABLES
            T_LIST    = T_LIST
            T_MESSAGE = T_MESSAGE.

*  LOOP AT T_LIST INTO WA_LTXX WHERE CHK EQ 'X'.
*    L_TABIX = SY-TABIX.
*
**   Confirm Transfer Order
*
*    PERFORM BDC_PROCESSING_LT12 TABLES   IT_BDCMSGCOLL
*                                USING    W_ZDOCNO
*                                         W_CONFIRMATION
*                                CHANGING W_SUBRC.
*    IF W_SUBRC = 0.
**    CLEAR: IO_SQTY.  "Scanned Qty.
*      WA_LTXX-EXECUTED_FLG = 'X'.
*      MODIFY T_LIST FROM WA_LTXX
*                     INDEX L_TABIX
*                     TRANSPORTING EXECUTED_FLG.
*
*    ELSE.
*      READ TABLE IT_BDCMSGCOLL INTO WA_BDCMSGCOLL
*                        WITH KEY MSGTYP = 'E'.
*
*      E_MESS  = TEXT-M05.
*      ZRESULT = TEXT-M02.
*      PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
*                                    T_MESSAGE.
*    ENDIF.
*
*    CLEAR WA_LTXX.
*  ENDLOOP.
ENDFUNCTION.
