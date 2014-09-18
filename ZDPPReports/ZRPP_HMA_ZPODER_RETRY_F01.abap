*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_F01                                        *
*----------------------------------------------------------------------*

FORM P2000_GET_DATA.

  DATA : LT_KSBOHMM LIKE TABLE OF ZTPP_KSBOHMM_IF WITH HEADER LINE,
         LT_WOSUM   LIKE TABLE OF ZTPP_WOSUM      WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_KSBOHMM
    FROM ZTPP_KSBOHMM_IF
    WHERE WO_SER IN S_WOSER
      AND ZSDAT  IN S_DATUM.

    CHECK NOT LT_KSBOHMM[] IS INITIAL.

  PERFORM P1000_START_PROGRESSBAR USING '10'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_WOSUM
      FROM ZTPP_WOSUM
      FOR ALL ENTRIES IN LT_KSBOHMM
      WHERE WO_SER = LT_KSBOHMM-WO_SER
        AND NATION = LT_KSBOHMM-NATION
        AND DEALER = LT_KSBOHMM-DEALER
        AND EXTC   = LT_KSBOHMM-EXTC
        AND INTC   = LT_KSBOHMM-INTC.
    SORT LT_WOSUM BY WO_SER NATION DEALER EXTC INTC .

  PERFORM P1000_START_PROGRESSBAR USING '20'.
    LOOP AT LT_KSBOHMM WHERE EXTC NE '***'.
      READ TABLE LT_WOSUM WITH KEY
          WO_SER   = LT_KSBOHMM-WO_SER
          NATION   = LT_KSBOHMM-NATION
          DEALER   = LT_KSBOHMM-DEALER
          EXTC   = LT_KSBOHMM-EXTC
          INTC   = LT_KSBOHMM-INTC BINARY SEARCH.
      IF SY-SUBRC <> 0 .
        APPEND LT_KSBOHMM TO GT_KSBOHMM.
      ENDIF.
    ENDLOOP.
  PERFORM P1000_START_PROGRESSBAR USING '50'.
    GT_DATA[] = GT_KSBOHMM[].

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
