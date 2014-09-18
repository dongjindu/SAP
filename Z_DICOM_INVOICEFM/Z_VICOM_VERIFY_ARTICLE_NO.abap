FUNCTION Z_VICOM_VERIFY_ARTICLE_NO.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       EXPORTING
*"             REFERENCE(STATUS) TYPE  I
*"       TABLES
*"              ITEM_DATA STRUCTURE  ZMIRO_ITEM
*"              PO_NUMBERS STRUCTURE  ZPONUMBER
*"----------------------------------------------------------------------

DATA:  COUNTER TYPE I,
       RESULT TYPE I.

* Initialise
  COUNTER = 0.

  LOOP AT ITEM_DATA.
*   Reset counters
    RESULT = 0.
    COUNTER = COUNTER + 1.

*   Check line item position
    CALL FUNCTION 'Z_DICOM_GET_LI_NUMBER'
     EXPORTING
       ARTICLENUMBER              = ITEM_DATA-ITEM_MATERIAL_NO
     IMPORTING
       POLINEITEM                 = RESULT
     TABLES
       PONUMBER                   = PO_NUMBERS.

    IF SY-SUBRC <> 0.
*     If error, exit directly
      STATUS = 0.
      EXIT.
    ELSE.
      IF COUNTER = RESULT.
        STATUS = 1.
      ELSE.
        STATUS = 0.
        EXIT.
      ENDIF.
    ENDIF.


  ENDLOOP.



ENDFUNCTION.
