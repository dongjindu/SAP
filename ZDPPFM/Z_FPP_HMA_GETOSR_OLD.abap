FUNCTION Z_FPP_HMA_GETOSR_OLD.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ZVIN) LIKE  ZOSRSEG-ZVIN
*"  EXPORTING
*"     VALUE(OUTPUT) LIKE  ZOSRSEG STRUCTURE  ZOSRSEG
*"--------------------------------------------------------------------

*--->> 01.17.2013 by BSBAE. Nobody use this function module
*--->> Below logic is wrong.

*DATA : LT_ZOSR LIKE TABLE OF ZTSD_OSR_LOG WITH HEADER LINE,
*       LV_ZVIN LIKE ZVIN.
*CLEAR : LT_ZOSR[] , LT_ZOSR.
*
*MOVE ZVIN TO LV_ZVIN.
*
*SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ZOSR
*  FROM ZTSD_OSR
*  WHERE ZVIN  = LV_ZVIN.
*
*
*  SORT LT_ZOSR BY ZDATE DESCENDING
*                  ZSEQ  DESCENDING.
*
*
* READ TABLE LT_ZOSR INDEX 1.
* MOVE-CORRESPONDING LT_ZOSR TO OUTPUT.



ENDFUNCTION.
