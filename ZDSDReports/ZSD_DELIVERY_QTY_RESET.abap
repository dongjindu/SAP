REPORT ZSD_DELIVERY_QTY_RESET.

INCLUDE: ZSD_DELIVERY_QTY_RESET_TOP,
         ZSD_DELIVERY_QTY_RESET_SEL,
         ZSD_DELIVERY_QTY_RESET_F01.

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION.

*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM P1000_START_PROGRESSBAR USING 5.
  PERFORM P2000_GET_DATA.
  PERFORM P1000_START_PROGRESSBAR USING 90.
END-OF-SELECTION.
