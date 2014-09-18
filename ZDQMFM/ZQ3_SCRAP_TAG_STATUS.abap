FUNCTION ZQ3_SCRAP_TAG_STATUS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_QMNUM) TYPE  QMNUM
*"  EXPORTING
*"     VALUE(O_MESS) TYPE  ZMESS
*"----------------------------------------------------------------------

  DATA: LT_JEST LIKE TABLE OF JEST WITH HEADER LINE.
  DATA: LV_QMNUM LIKE QMEL-QMNUM.
  LV_QMNUM = I_QMNUM.

  SELECT A~MANDT A~OBJNR STAT INACT INTO TABLE LT_JEST
    FROM QMEL AS A
    INNER JOIN JEST AS B
    ON A~OBJNR = B~OBJNR
   WHERE A~QMNUM = LV_QMNUM.

  IF SY-SUBRC IS INITIAL.
    READ TABLE LT_JEST WITH KEY STAT = 'I0076'
                                INACT = ' '.
    IF SY-SUBRC = 0.
      O_MESS = 'Scrap Tag is Void'.
      EXIT.
    ENDIF.

    READ TABLE LT_JEST WITH KEY STAT = 'E0003'
                                INACT = ' '.
    IF SY-SUBRC = 0.
      O_MESS = 'Scrap Tag is Void'.
      EXIT.
    ENDIF.

    READ TABLE LT_JEST WITH KEY STAT = 'I0072'
                                 INACT = ' '.
    IF SY-SUBRC = 0.
      O_MESS = 'Scrap Tag is complete'.
    ELSE.
      O_MESS = 'Scrap Tag is incomplete'.
    ENDIF.
  ELSE.
    O_MESS = 'The notification is not found'.
  ENDIF.

ENDFUNCTION.
