*----------------------------------------------------------------------*
***INCLUDE ZRSD10R_ORDER_BALANCE_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  SORT ITAB.
  DESCRIBE TABLE ITAB LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_DATA.

  DATA: L_INDEX ,
        L_TABIX LIKE SY-TABIX,
        L_MODQT LIKE VBAP-KWMENG,
        L_SO    LIKE VBAP-VBELN,
        L_MATNR LIKE MARA-MATNR,
        L_MATNR2 LIKE MARA-MATNR.

  CLEAR: LTAB.
  LOOP AT LTAB.

    L_TABIX = SY-TABIX.
    L_MATNR = LTAB-BSTNK+0(14).
    L_MATNR2 = LTAB-BSTNK.

    CALL FUNCTION 'Z_FSD_WO_INFO1'
         EXPORTING
              P_MATNR = L_MATNR
              P_MTART = 'WOHD'
         IMPORTING
              P_MODQT = L_MODQT
              P_SO    = L_SO
              P_INDEX = L_INDEX
         EXCEPTIONS
              1       = 1
              2       = 2
              OTHERS  = 3.


    IF SY-SUBRC = 0.
      IF L_INDEX NE '1'.
        LTAB-ZFLAG1 = 'E'.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'Z_FSD_WO_INFO1'
         EXPORTING
              P_MATNR = L_MATNR2
              P_MTART = 'WOCL'
         IMPORTING
              P_MODQT = LTAB-ZWOQTY1
              P_SO    = L_SO
              P_INDEX = L_INDEX
         EXCEPTIONS
              1       = 1
              2       = 2
              OTHERS  = 3.

    IF SY-SUBRC = 0.
      IF L_INDEX EQ 'X'.
        LTAB-ZFLAG2 = 'E'.
      ENDIF.
    ENDIF.

    SELECT SINGLE MODQTY SEQQTY
    INTO  (LTAB-ZWOQTY2, LTAB-ZWOQTY3)
    FROM  ZTPP_WOSUM
    WHERE WO_SER EQ LTAB-BSTNK+0(9)
    AND   NATION EQ LTAB-BSTNK+9(3)
    AND   DEALER EQ LTAB-BSTNK+12(2)
    AND   EXTC   EQ LTAB-BSTNK+14(2)
    AND   INTC   EQ LTAB-BSTNK+16(2)
    AND   SALES  EQ LTAB-VBELN.

    IF SY-SUBRC NE 0.
      LTAB-ZFLAG3 = 'E'.
    ENDIF.

    IF LTAB-ZORQTY NE LTAB-ZWOQTY1
    OR LTAB-ZORQTY NE LTAB-ZWOQTY2.
      LTAB-ZFLAG5 = 'E'.
    ENDIF.

    IF LTAB-ZFLAG1 = 'E'
    OR LTAB-ZFLAG2 = 'E'
    OR LTAB-ZFLAG3 = 'E'.
      LTAB-ZFLAG4 = 'E'.
    ENDIF.

    MODIFY LTAB INDEX L_TABIX.
    CLEAR: L_TABIX, LTAB, L_INDEX.

  ENDLOOP.

  DELETE LTAB WHERE ZFLAG1 = ''
              AND   ZFLAG2 = ''
              AND   ZFLAG3 = ''
              AND   ZFLAG4 = ''
              AND   ZFLAG5 = ''.


  DATA: L_NUM1 TYPE I,
        L_NUM2 TYPE I,
        L_NUM3 TYPE I,
        L_NUM4 TYPE I,
        L_NUM5 TYPE I.

  CLEAR: LTAB, L_NUM1, L_NUM2, L_NUM3, L_NUM4, L_NUM5.
  LOOP AT LTAB.

    L_NUM1 = L_NUM1 + 1.
    IF LTAB-ZFLAG1 = 'E'.
      L_NUM2 = L_NUM2 + 1.
    ENDIF.

    IF LTAB-ZFLAG2 = 'E'.
      L_NUM3 = L_NUM3 + 1.
    ENDIF.

    IF LTAB-ZFLAG3 = 'E'.
      L_NUM4 = L_NUM4 + 1.
    ENDIF.

    IF LTAB-ZFLAG5 = 'E'.
      L_NUM5 = L_NUM5 + 1.
    ENDIF.

    MOVE-CORRESPONDING LTAB TO T_DEV.
    APPEND T_DEV TO ITAB.
    CLEAR: T_DEV.

  ENDLOOP.

  T_CNT-ZSOC1 = L_NUM1.
  T_CNT-ZWOHD = L_NUM2.
  T_CNT-ZWOCL = L_NUM3.
  T_CNT-ZWOSUM = L_NUM4.
  T_CNT-ZQT_DIFF = L_NUM5.

  APPEND T_CNT TO ITAB2.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

  DATA: BEGIN OF LTAB2 OCCURS 0,
          BSTNK LIKE VBAK-BSTNK,
          KUNNR LIKE VBAK-KUNNR,
          VBELN LIKE VBAK-VBELN,
          POSNR LIKE VBAP-POSNR,
          KWMENG LIKE VBAP-KWMENG,
          PSTYV  LIKE VBAP-PSTYV,
        END OF LTAB2.

  SELECT A~BSTNK A~KUNNR B~VBELN B~POSNR B~KWMENG B~PSTYV
  INTO   CORRESPONDING FIELDS OF TABLE LTAB2
  FROM   VBAK AS A
  INNER  JOIN VBAP AS B
  ON     A~VBELN EQ B~VBELN

  WHERE  A~AUART EQ 'ZVSO'
  AND    A~AUART EQ 'ZVTO'
  AND    A~ERDAT IN S_DATE.

  REFRESH: LTAB.
  CLEAR: LTAB2, LTAB.
  LOOP AT LTAB2.

*    IF  LTAB2-PSTYV EQ 'ZTAC' "ZTCE??
*    AND LTAB2-POSNR EQ 10.
    IF  LTAB2-POSNR EQ 10. "BY JUNHO , REQUESTED SW KIM 2004.1.23
      LTAB-ZSQQTY = LTAB2-KWMENG.
    ENDIF.

    MOVE:   LTAB2-KWMENG TO LTAB-ZORQTY,
            LTAB2-BSTNK  TO LTAB-BSTNK,
            LTAB2-KUNNR  TO LTAB-KUNNR,
            LTAB2-VBELN  TO LTAB-VBELN.

    COLLECT LTAB.
    CLEAR: LTAB, LTAB2.
  ENDLOOP.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_HEADER.
  DATA: L_HEADER(70).

  CONCATENATE S_DATE-LOW '~' S_DATE-HIGH INTO L_HEADER.

  GS_LAYOUT-SMALLTITLE = 'S'.
  GS_LAYOUT-GRID_TITLE = L_HEADER.
*  APPEND GS_LAYOUT.

ENDFORM.                    " MAKE_HEADER
