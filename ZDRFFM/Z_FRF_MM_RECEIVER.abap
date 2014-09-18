FUNCTION Z_FRF_MM_RECEIVER .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_TYPES) TYPE  ZRF_TYPES
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_TRUCK STRUCTURE  ZSRF_TRUCK OPTIONAL
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE OPTIONAL
*"      T_RECEIVER STRUCTURE  ZSRF_RECEIVER
*"----------------------------------------------------------------------
  DATA: L_CHECK, " ERROR CHECK
        L_TABIX TYPE SY-TABIX.
  DATA: BEGIN OF LT_TRUCK OCCURS 0,
          RECEIVER TYPE ZSRF_TRUCK-RECEIVER,
          VBELN TYPE ZSRF_TRUCK-VBELN,
          WBSTK TYPE ZSRF_TRUCK-WBSTK,
          KOSTK TYPE ZSRF_TRUCK-KOSTK,
          EXECUTED_FLG TYPE ZSRF_TRUCK-EXECUTED_FLG,
        END OF LT_TRUCK.
  DATA: LT_RECE TYPE ZSRF_RECE_DATA OCCURS 0 WITH HEADER LINE.

  CHECK NOT I_TYPES IS INITIAL.   "External ID CHECK
  CHECK NOT T_RECEIVER[] IS INITIAL.   "External ID CHECK
*  TRANSLATE I_TRUCK-LIFEX TO UPPER CASE. "To Uppercase
* Button Click Date & Time
  W_BUTTON_CLICK_DATE = SY-DATUM.
  W_BUTTON_CLICK_TIME = SY-UZEIT.

* Making Inbound Deliveries list which are not posted
  REFRESH T_TRUCK. CLEAR: T_TRUCK. CLEAR L_CHECK.
  PERFORM RECEIVER_ITEM_SEARCH TABLES T_RECEIVER
                                      T_TRUCK
*                  USING  I_TRUCK-LIFEX  ""Ext.delivery
*                  2004.08.03 CHANGING addition
                               USING  I_TYPES
                                      L_CHECK.
* { Here we call transaction /nLM74(Select Delivery by Others) and
*  get BDC log messages in the internal table IT_bdcmsgcoll. }
* -> Useless, Now we use /nLT03 and /nVL32N
  IF NOT T_TRUCK[] IS INITIAL.
* Application Doc No.
    PERFORM NUMBER_GET_NEXT USING    NRO_NR_09     "NRO Interval
                                     NRO_OBJECT    "NRO Object
                            CHANGING W_ZDOCNO.     "App. Doc. No.
    COMMIT WORK.

    SORT T_TRUCK BY VBELN.

    LOOP AT T_TRUCK INTO WA_TRUCK WHERE EXECUTED_FLG NE 'X'.
      L_TABIX = SY-TABIX.
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
            L_CHECK  = 'X'.  "ERROR CHECK
            WA_MESSA = WA_TEXT(73).
            WA_EPOSITION = 'GR CREATION ERROR'.
            CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
                 EXPORTING
                      I_VBELN     = WA_TRUCK-VBELN
                      I_MESSA     = WA_MESSA
                      I_EPOSITION = WA_EPOSITION.
            WA_TRUCK-EXECUTED_FLG = 'X'.
          ELSE.
            WA_TRUCK-EXECUTED_FLG = 'S'.
          ENDIF.
        ELSE.
          L_CHECK  = 'X'.  "ERROR CHECK
          WA_MESSA = WA_TEXT(73).
          WA_EPOSITION = 'TO CREATION ERROR'.
          CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
               EXPORTING
                    I_VBELN     = WA_TRUCK-VBELN
                    I_MESSA     = WA_MESSA
                    I_EPOSITION = WA_EPOSITION.
          WA_TRUCK-EXECUTED_FLG = 'X'.
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
          L_CHECK  = 'X'.  "ERROR CHECK
          WA_MESSA = WA_TEXT(73).
          WA_EPOSITION = 'GR CREATION ERROR'.
          CALL FUNCTION 'Z_FRF_MM_DELIVERY_ERROR'
               EXPORTING
                    I_VBELN     = WA_TRUCK-VBELN
                    I_MESSA     = WA_MESSA
                    I_EPOSITION = WA_EPOSITION.
          WA_TRUCK-EXECUTED_FLG = 'X'.
        ELSE.
          WA_TRUCK-EXECUTED_FLG = 'S'.

        ENDIF.
      ENDIF.
      MODIFY T_TRUCK FROM WA_TRUCK INDEX L_TABIX
                                   TRANSPORTING EXECUTED_FLG.
*     M/W MODIFY DATA
      MOVE-CORRESPONDING WA_TRUCK TO LT_RECE.
      CASE WA_TRUCK-EXECUTED_FLG.
        WHEN 'S'.
          LT_RECE-EXECUTED_FLG = 'X'.
          APPEND LT_RECE. CLEAR LT_RECE.
        WHEN 'X'.
          CLEAR LT_RECE.
      ENDCASE.
      CLEAR WA_TRUCK.
    ENDLOOP.

    CASE I_TYPES.
      WHEN 'TRUCK'.

      WHEN 'UNIT'.
        LOOP AT T_TRUCK.
          MOVE-CORRESPONDING T_TRUCK TO LT_TRUCK.
          APPEND LT_TRUCK. CLEAR LT_TRUCK.
        ENDLOOP.
        REFRESH T_TRUCK. CLEAR T_TRUCK.
        SORT LT_TRUCK BY RECEIVER
                         EXECUTED_FLG DESCENDING.
        DELETE ADJACENT DUPLICATES FROM LT_TRUCK COMPARING RECEIVER.
        LOOP AT LT_TRUCK.
          MOVE-CORRESPONDING LT_TRUCK TO T_TRUCK.
          APPEND T_TRUCK. CLEAR T_TRUCK.
        ENDLOOP.
      WHEN 'VENDOR'.

    ENDCASE.

*/ Log Message Processing
    CLEAR: WA_LIPS, W_QTY_MESSAGE.
* If there is no related inbound delivery.
*    PERFORM CALL_MESSAGE_SCREEN_NODELIV.
*                  "No delivery found for selection criteria

    IF L_CHECK NE 'X'.
      E_MESS  = TEXT-M22.
      ZRESULT = TEXT-M04.
    ELSE.
      ZRESULT = TEXT-M02.
*      E_MESS  = TEXT-M05.
      E_MESS  = WA_TEXT.
    ENDIF.
  ELSE.
    IF L_CHECK EQ 'X'.
      ZRESULT = TEXT-M02.
      E_MESS  = TEXT-M21.

    ELSE.
      ZRESULT = TEXT-M02.
      E_MESS  = TEXT-M01.
    ENDIF.
  ENDIF.
  PERFORM MIDDLEWARE_UPLODA_RECEIVER TABLES  LT_RECE.
ENDFUNCTION.
