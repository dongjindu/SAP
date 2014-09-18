FUNCTION Z_FRF_MM_VENDOR_TRUCK_DELIVERY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_TRUCK STRUCTURE  ZSRF_TRUCK
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE
*"      T_VENDOR STRUCTURE  ZSRF_VENDOR_SE
*"----------------------------------------------------------------------
  DATA L_CHECK. "ERROR CHECK
  CHECK NOT T_VENDOR[] IS INITIAL.   "External ID CHECK
*  TRANSLATE I_TRUCK-LIFEX TO UPPER CASE. "To Uppercase
  DATA: L_LIFNR LIKE LIKP-LIFNR.
* Button Click Date & Time
  W_BUTTON_CLICK_DATE = SY-DATUM.
  W_BUTTON_CLICK_TIME = SY-UZEIT.

* Making Inbound Deliveries list which are not posted
  REFRESH T_TRUCK. CLEAR: T_TRUCK.
  PERFORM T_TRUCK_VENDOR TABLES T_TRUCK
                                T_VENDOR.
*                         CHANGING L_LIFNR.
* { Here we call transaction /nLM74(Select Delivery by Others) and
*  get BDC log messages in the internal table IT_bdcmsgcoll. }
* -> Useless, Now we use /nLT03 and /nVL32N

* Application Doc No.
  PERFORM NUMBER_GET_NEXT USING    NRO_NR_09     "NRO Interval
                                   NRO_OBJECT    "NRO Object
                          CHANGING W_ZDOCNO.     "App. Doc. No.
  COMMIT WORK.

  SORT T_TRUCK BY VBELN.

  LOOP AT T_TRUCK INTO WA_TRUCK.
    IF WA_TRUCK-KOSTK = 'A'. "until this time,
      "Transfer Order not processed.
      "Total goods movement status
* Generate TO (/nLT03)
      PERFORM BDC_PROCESSING_LT03_HEADER
                                 TABLES   IT_BDCMSGCOLL
                                 USING    W_ZDOCNO
                                 CHANGING W_SUBRC.
      PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                    T_MESSAGE.
      IF W_SUBRC = 0.
*       Post GR
        PERFORM BDC_PROCESSING_VL32N
                             TABLES   IT_BDCMSGCOLL
                             USING    W_ZDOCNO
                             CHANGING W_SUBRC.
        PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                      T_MESSAGE.
        IF W_SUBRC NE 0.
          L_CHECK = 'X'.  "ERROR CHECK
          WA_MESSA = WA_TEXT(73).
          WA_EPOSITION = 'GR CREATION ERROR'.
          CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
            EXPORTING
              I_VBELN           =  WA_TRUCK-VBELN
              I_MESSA           =  WA_MESSA
              I_EPOSITION       =  WA_EPOSITION .
        ENDIF.
      ELSE.
          L_CHECK = 'X'.  "ERROR CHECK
          WA_MESSA = WA_TEXT(73).
          WA_EPOSITION = 'TO CREATION ERROR'.
          CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
            EXPORTING
              I_VBELN           =  WA_TRUCK-VBELN
              I_MESSA           =  WA_MESSA
              I_EPOSITION       =  WA_EPOSITION .
      ENDIF.
    ELSE.
* Post GR
      PERFORM BDC_PROCESSING_VL32N
                           TABLES   IT_BDCMSGCOLL
                           USING    W_ZDOCNO
                           CHANGING W_SUBRC.

      PERFORM MESSAGE_TEXT TABLES   IT_BDCMSGCOLL
                                    T_MESSAGE.
      IF W_SUBRC NE 0.
          L_CHECK = 'X'.  "ERROR CHECK
          WA_MESSA = WA_TEXT(73).
          WA_EPOSITION = 'GR CREATION ERROR'.
          CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
            EXPORTING
              I_VBELN           =  WA_TRUCK-VBELN
              I_MESSA           =  WA_MESSA
              I_EPOSITION       =  WA_EPOSITION .
      ENDIF.
    ENDIF.
  ENDLOOP.



*/ Log Message Processing
  CLEAR: WA_LIPS, W_QTY_MESSAGE.
  IF T_TRUCK[] IS INITIAL.
* If there is no related inbound delivery.
    E_MESS  = TEXT-M01.
    ZRESULT = TEXT-M02.
  ELSE.
    IF L_CHECK NE 'X'.
      E_MESS  = TEXT-M03.
      ZRESULT = TEXT-M04.

    ELSE.
      ZRESULT = TEXT-M02.
      E_MESS  = TEXT-M05.
    ENDIF.
  ENDIF.
  CLEAR: LIKP-LIFEX.


ENDFUNCTION.
