*----------------------------------------------------------------------*
*   INCLUDE ZSD_DELIVERY_QTY_RESET_F01                                 *
*----------------------------------------------------------------------*


FORM P1000_START_PROGRESSBAR USING PERCENT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PERCENT
            TEXT       = TEXT-001
       EXCEPTIONS
            OTHERS     = 1.
ENDFORM.                    " P1000_START_PROGRESSBAR
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_DATA.
  DATA L_CURR_GI LIKE VBLB-ABEFZ.

  DATA: $CURR_YEAR(4) TYPE N,
        $PREV_YEAR(4) TYPE N.

  DATA: BEGIN OF LT_VBLB OCCURS 0,
          VBELN TYPE VBLB-VBELN,
          POSNR TYPE VBLB-POSNR,
        END OF LT_VBLB.

  DATA IT_VBLB LIKE VBLB OCCURS 0 WITH HEADER LINE.

  $CURR_YEAR = SY-DATUM+0(4).
  $PREV_YEAR = $CURR_YEAR - 1.

  SELECT * INTO TABLE IT_VBLB
    FROM VBLB
   WHERE VBLB~VBELN = S_VBELN-LOW AND
         VBLB~ABRLI = '' AND
         VBLB~ABART = '2'.

  IF IT_VBLB[] IS INITIAL.
    WRITE 'No Data'.
    EXIT.
  ENDIF.

  SORT IT_VBLB BY VBELN POSNR.

  LOOP AT IT_VBLB.
    CALL FUNCTION 'SD_DELIVERY_CUMULATIVE_DETERM'
    EXPORTING
      CURRENT_YEAR              = $CURR_YEAR
      I_POSNR                   = IT_VBLB-POSNR
      I_UMVKN                   = '1'
      I_UMVKZ                   = '1'
      I_VBELN                   = IT_VBLB-VBELN
      PREVIOUS_YEAR             = $PREV_YEAR
      NO_BUFFER                 = 'X'
   IMPORTING
      CUMULATIVE_GOOD_ISSUE        = IT_VBLB-ABEFZ.

    WRITE: / IT_VBLB-POSNR, ' ', IT_VBLB-ABEFZ.
    MODIFY IT_VBLB.
  ENDLOOP.

  MODIFY VBLB FROM TABLE IT_VBLB.

ENDFORM.                    " P2000_GET_DATA
