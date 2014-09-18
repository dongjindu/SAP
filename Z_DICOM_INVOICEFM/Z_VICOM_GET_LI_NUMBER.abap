FUNCTION Z_VICOM_GET_LI_NUMBER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(ARTICLENUMBER) TYPE  CHAR18 OPTIONAL
*"       EXPORTING
*"             VALUE(POLINEITEM) TYPE  I
*"       TABLES
*"              PONUMBER STRUCTURE  ZPONUMBER OPTIONAL
*"       EXCEPTIONS
*"              NOT_FOUND
*"              MORE_THAN_ONE_ITEMNO
*"----------------------------------------------------------------------

DATA:    COUNTER TYPE I,
         SUM TYPE I,
         C TYPE I,
         TBL_ALL TYPE ZPONUMBER OCCURS 1 WITH HEADER LINE,
         TBL_LINE_ITEMS TYPE BAPIEKPO OCCURS 1 WITH HEADER LINE.


  CLEAR TBL_ALL.

*** Loop at each PO number in tables to get it's line items
  LOOP AT PONUMBER.
      CLEAR TBL_LINE_ITEMS.
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          PURCHASEORDER                    = PONUMBER-PO_NUMBER
          ITEMS                            = 'X'
        TABLES
          PO_ITEMS                         = TBL_LINE_ITEMS.

***   Append line item into the overall list
      LOOP AT TBL_LINE_ITEMS.
        TBL_ALL-PO_NUMBER = TBL_LINE_ITEMS-MATERIAL.
        APPEND TBL_ALL.
      ENDLOOP.
  ENDLOOP.

* Validation - Material number cannot appear more than one
  SUM = 0.
  LOOP AT TBL_ALL.
    IF TBL_ALL-PO_NUMBER = ARTICLENUMBER.
           SUM = SUM + 1.
    ENDIF.
  ENDLOOP.

*  IF SUM > 1 Then error
IF SUM > 1.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   RAISE MORE_THAN_ONE_ITEMNO.
ENDIF.


* Find the position of material number
  COUNTER = 0.

  LOOP AT TBL_ALL.
   COUNTER = COUNTER + 1.

    IF TBL_ALL-PO_NUMBER = ARTICLENUMBER.
           C = COUNTER.
    ENDIF.

    ENDLOOP.

  POLINEITEM = C.


ENDFUNCTION.
