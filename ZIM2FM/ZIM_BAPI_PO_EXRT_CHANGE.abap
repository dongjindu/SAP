FUNCTION ZIM_BAPI_PO_EXRT_CHANGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(MODE) TYPE  C DEFAULT 'Y'
*"  TABLES
*"      IT_EBELN STRUCTURE  EKKO
*"      RETURN STRUCTURE  BAPIRET2
*"  EXCEPTIONS
*"      POST_ERROR
*"----------------------------------------------------------------------
   CLEAR : RETURN.
   REFRESH : RETURN.

   LOOP AT IT_EBELN.
      CLEAR : BAPIMEPOHEADER,
              BAPIMEPOHEADERX.
      MOVE : 'X'            TO      BAPIMEPOHEADERX-EXCH_RATE,
             'X'            TO      BAPIMEPOHEADERX-EX_RATE_FX,
             IT_EBELN-EBELN TO      BAPIMEPOHEADER-PO_NUMBER,
             IT_EBELN-WKURS TO      BAPIMEPOHEADER-EXCH_RATE,
             IT_EBELN-KUFIX TO      BAPIMEPOHEADER-EX_RATE_FX.

      CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
             EXPORTING
                PURCHASEORDER = IT_EBELN-EBELN
                POHEADER      = BAPIMEPOHEADER
                POHEADERX     = BAPIMEPOHEADERX
             TABLES
                RETURN        = XRETURN.

      REFRESH : RETURN.
      LOOP AT XRETURN WHERE TYPE EQ 'E'.
         MOVE-CORRESPONDING XRETURN TO RETURN.
         APPEND RETURN.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
         RAISE   POST_ERROR.
      ENDIF.
   ENDLOOP.
*-----------------------------------------------------------------------

ENDFUNCTION.
