FUNCTION Z_FRF_MM_UNIT_LOAD_NO.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_UNIT_NO STRUCTURE  ZSRF_UNIT_NO
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"----------------------------------------------------------------------
  DATA: L_CHECK.
  CHECK NOT T_UNIT_NO[] IS INITIAL.  "Vendor mat. no.

* Button Click Date & Time
  W_BUTTON_CLICK_DATE = SY-DATUM.
  W_BUTTON_CLICK_TIME = SY-UZEIT.

  CLEAR: IT_IDITEMS_WITHOUT_POST.
  PERFORM IDITEMS_WITHOUT_POST
                 TABLES   IT_IDITEMS_WITHOUT_POST
                          T_UNIT_NO
                 CHANGING L_CHECK.

** Here we call transaction /nLT03 and BAPI of /nMIGO_GR
** (Create Transfer Order for Delivery Note & GR) and
** get BDC log messages in the internal table IT_bdcmsgcoll.
* App Doc No
  PERFORM NUMBER_GET_NEXT USING    NRO_NR_09     "NRO Interval
                                   NRO_OBJECT    "NRO Object
                          CHANGING W_ZDOCNO.     "App Doc No
  COMMIT WORK.

  SORT IT_IDITEMS_WITHOUT_POST BY VBELN POSNR.
  "Sorted by Inbound Delivery, Item no.

*/Begin of Added by Hakchin(20030127)
  DELETE ADJACENT DUPLICATES FROM IT_IDITEMS_WITHOUT_POST
                             COMPARING VBELN.
*/End of Added by Hakchin(20030127)

  LOOP AT IT_IDITEMS_WITHOUT_POST
                   INTO WA_IDITEMS_WITHOUT_POST.

    IF WA_IDITEMS_WITHOUT_POST-KOSTA = 'A'.
      "until this time, Transfer Order not processed.
      "Picking status/Putaway status = 'A'.
* Generate TO (/nLT03)
      PERFORM BDC_PROCESSING_LT03_HEADER1
                                 TABLES   IT_BDCMSGCOLL
                                 USING    W_ZDOCNO
                                 CHANGING W_SUBRC.
      PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                    T_MESSAGE.
      IF W_SUBRC = 0.
* Post GR (/nVL32N)
* We use BDC instead of BAPI because of BAPI's no Doc-Flow Supporting.
* Reference BAPI: BAPI_GOODSMVT_CREATE

*/Begin of Changed by Hakchin(20040416)
* We use bdc_processing_mb01 instead of bdc_processing_vl32n
* because GR Posting Date Adjusting
        PERFORM BDC_PROCESSING_VL32N_1
                             TABLES   IT_BDCMSGCOLL
                             USING    W_ZDOCNO
                             CHANGING W_SUBRC.
        PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                      T_MESSAGE.
      ELSE.
        L_CHECK = 'X'.  " ERROR CHECK
        WA_MESSA = WA_TEXT(73).
        WA_EPOSITION = 'TO CREATION ERROR'.
        CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
             EXPORTING
                  I_VBELN     = WA_IDITEMS_WITHOUT_POST-VBELN
                  I_MESSA     = WA_MESSA
                  I_EPOSITION = WA_EPOSITION.
      ENDIF.
    ELSE.
* Post GR (/nVL32N)
*/Begin of Changed by Hakchin(20040416)
* We use bdc_processing_mb01 instead of bdc_processing_vl32n
* because GR Posting Date Adjusting
      PERFORM BDC_PROCESSING_VL32N_1
                           TABLES   IT_BDCMSGCOLL
                           USING    W_ZDOCNO
                           CHANGING W_SUBRC.
      PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                    T_MESSAGE.
*/End of Changed by Hakchin(20040416)
      IF W_SUBRC NE 0.
        L_CHECK = 'X'.  " ERROR CHECK
        WA_MESSA = WA_TEXT(73).
        WA_EPOSITION = 'GR CREATION ERROR'.
        CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
             EXPORTING
                  I_VBELN     = WA_IDITEMS_WITHOUT_POST-VBELN
                  I_MESSA     = WA_MESSA
                  I_EPOSITION = WA_EPOSITION.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF L_CHECK EQ 'X'.
    E_MESS  = TEXT-M05.
    ZRESULT = TEXT-M02.
*    EXIT.
  ELSE.
    E_MESS  = TEXT-M03.
    ZRESULT = TEXT-M04.
*    EXIT.
  ENDIF.
*  CLEAR: I_KDMAT.   "Vendor mat. no.
ENDFUNCTION.
