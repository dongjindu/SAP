*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM P2000_GET_DATA.
  DATA : LT_UM LIKE TABLE OF ZTSD_UM WITH HEADER LINE.
  PERFORM P1000_START_PROGRESSBAR USING 10.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_UM
  FROM ZTSD_UM
  WHERE URGENCY NE ''
    AND URGENCY IN S_URGNC
    AND BODY_NO EQ ''
    AND STATUS  NE 'D'.

  PERFORM P1000_START_PROGRESSBAR USING 50.
  LOOP AT LT_UM.
    MOVE-CORRESPONDING LT_UM TO GT_DATA.
    GT_DATA-SUMQTY = 1.
    COLLECT GT_DATA.
  ENDLOOP.
  PERFORM P1000_START_PROGRESSBAR USING 80.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_START_PROGRESSBAR USING PERCENT.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = PERCENT
              TEXT       = TEXT-001
         EXCEPTIONS
              OTHERS     = 1.

ENDFORM.                    " P1000_START_PROGRESSBAR
