*----------------------------------------------------------------------*
*   INCLUDE ZRSD01R_WO_STATUS_GENERAL_F01                              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

  DATA: BEGIN OF LTAB OCCURS 0.
          INCLUDE STRUCTURE ZSSD_WO_GENERAL.
  DATA: END OF LTAB.

  DATA: L_TABIX LIKE SY-TABIX.

  SELECT WO_SER  NATION  DEALER  EXTC
         INTC    MODQTY  FSC     SALES
         RP01TQ  RP08TQ  RP09TQ  RP11TQ
         RP15TQ  RP16TQ
  INTO CORRESPONDING FIELDS OF TABLE LTAB
  FROM ZTPP_WOSUM
  WHERE WO_SER IN S_WO_SER
  AND   NATION IN S_NATION
  AND   DEALER IN S_DEALER
  AND   EXTC   IN S_EXTC
  AND   INTC   IN S_INTC
  AND   SEQQTY NE 0.

*  IF NOT S_MON IS INITIAL.
*    CLEAR: LTAB.
*    LOOP AT LTAB.
*      L_TABIX = SY-TABIX.
*      READ TABLE S_MON WITH KEY LOW = LTAB-WO_SER+1(4).
*      IF SY-SUBRC NE 0.
*        DELETE LTAB INDEX L_TABIX.
*      ENDIF.
*      CLEAR: LTAB.
*    ENDLOOP.
*  ENDIF.

  LOOP AT LTAB INTO T_ITAB1.

    APPEND T_ITAB1 TO ITAB1.
    CLEAR: T_ITAB1.

  ENDLOOP.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_SCREEN.

  SORT ITAB1.
  DESCRIBE TABLE ITAB1 LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.

ENDFORM.                    " CALL_SCREEN
