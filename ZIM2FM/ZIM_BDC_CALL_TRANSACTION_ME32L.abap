FUNCTION ZIM_BDC_CALL_TRANSACTION_ME32L.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(PURCHASEORDER) LIKE  BAPIMEPOHEADER-PO_NUMBER
*"     VALUE(POHEADER) LIKE  BAPIMEPOHEADER STRUCTURE  BAPIMEPOHEADER
*"     VALUE(TCODE) LIKE  SYST-TCODE
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
DATA : L_TEXT(12).


   REFRESH : BDCDATA.
   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMM06E'        '0205',
          ' ' 'RM06E-EVRTN'     PURCHASEORDER,
          ' ' 'BDC_OKCODE'      '=KOPF'.

   WRITE : POHEADER-EXCH_RATE TO L_TEXT.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPMM06E'        '0201',
          ' ' 'EKKO-WKURS'      L_TEXT,
          ' ' 'EKKO-KUFIX'      POHEADER-EX_RATE_FX,
          ' ' 'BDC_OKCODE'       '=BU'.

   PERFORM P2000_DYNPRO USING :
          'X' 'SAPLSPO1'        '0300',
          ' ' 'BDC_OKCODE'       '=YES'.

*>> BDC CALL.
   REFRESH:MESSTAB.
   CALL TRANSACTION TCODE    USING       BDCDATA
*                             MODE        'N'
                             MODE        MODE
*                             UPDATE      'V'
                             UPDATE      'S'
                             MESSAGES    INTO   MESSTAB.

  LOOP AT MESSTAB.
    MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
           MESSTAB-MSGID   TO     RETURN-ID,
           MESSTAB-MSGNR   TO     RETURN-NUMBER,
           MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
           MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
           MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
           MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                     MSGID     = RETURN-ID
                     MSGNR     = RETURN-NUMBER
                     MSGV1     = RETURN-MESSAGE_V1
                     MSGV2     = RETURN-MESSAGE_V2
                     MSGV3     = RETURN-MESSAGE_V3
                     MSGV4     = RETURN-MESSAGE_V4
             IMPORTING
                     MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
     APPEND  RETURN.
  ENDLOOP.

ENDFUNCTION.
