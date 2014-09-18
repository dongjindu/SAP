*----------------------------------------------------------------------*
***INCLUDE ZRSD10R_ORDER_BALANCE_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  SELECT *
         INTO TABLE IT_VBAK
         FROM VBAK
        WHERE VBELN IN S_VBELN
        AND   VTWEG EQ P_VTWEG
        AND   KUNNR IN S_KUNNR.
*
  IF NOT S_MATNR-LOW IS INITIAL.
    S_MATNR-SIGN = 'I'.
    S_MATNR-OPTION = 'BT'.
    S_MATNR-HIGH = 'ZZZZZZZZZZZZZZZZZZ'.
    APPEND S_MATNR.
  ENDIF.
*
  SELECT *
         INTO TABLE IT_VBAP
         FROM VBAP
              FOR ALL ENTRIES IN IT_VBAK
        WHERE VBELN EQ IT_VBAK-VBELN
        AND   MATNR IN S_MATNR.

  SELECT *
         INTO TABLE IT_VBEP
         FROM VBEP
              FOR ALL ENTRIES IN IT_VBAP
        WHERE VBELN EQ IT_VBAP-VBELN
        AND   POSNR EQ IT_VBAP-POSNR
        AND   BMENG NE 0.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA.
  LOOP AT IT_VBEP.
    IF W_VBELN NE IT_VBEP-VBELN OR
       W_POSNR NE IT_VBEP-POSNR.
      PERFORM GET_GI_IV_DATE.
      W_VBELN = IT_VBEP-VBELN.
      W_POSNR = IT_VBEP-POSNR.
    ENDIF.

    PERFORM GET_MATERIAL_INFO.
    CHECK SY-SUBRC = 0.
    PERFORM GET_SOLD_TO_PARTY.
*    PERFORM GET_GI_IV_DATE.
    PERFORM GET_BACK_ORDER_QTY.
    PERFORM GET_PREPAYMENT_QTY.
    PERFORM GET_CONFIRMED_QTY.
    PERFORM GET_GOODS_ISSUE_QTY.
    PERFORM GET_INVOICE_QTY.

    T_OR_BAL-VRKME = IT_VBEP-VRKME.

    COLLECT T_OR_BAL INTO IT_OR_BAL. CLEAR T_OR_BAL.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_INFO
*&---------------------------------------------------------------------*
FORM GET_MATERIAL_INFO.
  CLEAR : T_OR_BAL-MATNR, T_OR_BAL-ARKTX, T_OR_BAL-SUBPR.
  READ TABLE IT_VBAP WITH KEY VBELN = IT_VBEP-VBELN
                              POSNR = IT_VBEP-POSNR.
  IF SY-SUBRC = 0.
    T_OR_BAL-MATNR = IT_VBAP-MATNR.
    T_OR_BAL-ARKTX = IT_VBAP-ARKTX.
    SELECT SINGLE *
           FROM MARC
          WHERE MATNR EQ IT_VBAP-MATNR.
    IF SY-SUBRC = 0 AND MARC-SOBSL = '30'.
      T_OR_BAL-SUBPR = 'X'.
    ENDIF.
  ENDIF.

  IF S_SUBPR-LOW = 'X'.
    IF T_OR_BAL-SUBPR = 'X'.
      SY-SUBRC = 0.
    ELSE.
      SY-SUBRC = 4.
    ENDIF.
  ELSE.
    SY-SUBRC = 0.
  ENDIF.
ENDFORM.                    " GET_MATERIAL_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_SOLD_TO_PARTY
*&---------------------------------------------------------------------*
FORM GET_SOLD_TO_PARTY.
  CLEAR T_OR_BAL-KUNNM.
  READ TABLE IT_VBAK WITH KEY VBELN = IT_VBEP-VBELN.
  IF SY-SUBRC = 0.
    SELECT SINGLE *
           FROM KNA1
          WHERE KUNNR EQ IT_VBAK-KUNNR.
    IF SY-SUBRC = 0.
      T_OR_BAL-KUNNM = KNA1-MCOD1.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_SOLD_TO_PARTY
*&---------------------------------------------------------------------*
*&      Form  GET_GI_IV_DATE
*&---------------------------------------------------------------------*
FORM GET_GI_IV_DATE.
  CLEAR  : W_GI_DATE, W_IV_DATE,
           W_GI_QTY,  W_IV_QTY.
  SELECT *
         FROM VBFA
        WHERE VBELV EQ IT_VBEP-VBELN
        AND   POSNV EQ IT_VBEP-POSNR
        AND   ( VBTYP_N EQ 'R' OR
                VBTYP_N EQ 'h' ).
    CHECK VBFA-ERDAT+0(6) EQ P_SPMON+0(6).
    W_GI_DATE = VBFA-ERDAT.
    IF VBFA-VBTYP_N = 'R'.
      W_GI_QTY = W_GI_QTY + VBFA-RFMNG.
    ELSE.
      W_GI_QTY = W_GI_QTY - VBFA-RFMNG.
    ENDIF.
  ENDSELECT.
  IF W_GI_QTY < 0.
    W_GI_QTY = 0.
  ENDIF.

  SELECT *
         FROM VBFA
        WHERE VBELV EQ IT_VBEP-VBELN
        AND   POSNV EQ IT_VBEP-POSNR
        AND   ( VBTYP_N EQ 'M' OR
                VBTYP_N EQ 'N' ).
    CHECK VBFA-ERDAT+0(6) EQ P_SPMON+0(6).
    W_IV_DATE = VBFA-ERDAT.
    IF VBFA-VBTYP_N = 'M'.
      W_IV_QTY = W_IV_QTY + VBFA-RFMNG.
    ELSE.
      W_IV_QTY = W_IV_QTY - VBFA-RFMNG.
    ENDIF.
  ENDSELECT.
  IF W_IV_QTY < 0.
    W_IV_QTY = 0.
  ENDIF.
ENDFORM.                    " GET_GI_IV_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_BACK_ORDER_QTY
*&---------------------------------------------------------------------*
FORM GET_BACK_ORDER_QTY.
  CLEAR T_OR_BAL-BOQTY.
  IF P_SPMON+0(6) > IT_VBEP-EDATU+0(6).
    T_OR_BAL-BOQTY = IT_VBEP-BMENG - W_GI_QTY.
  ENDIF.
ENDFORM.                    " GET_BACK_ORDER_QTY
*&---------------------------------------------------------------------*
*&      Form  GET_PREPAYMENT_QTY
*&---------------------------------------------------------------------*
FORM GET_PREPAYMENT_QTY.
  CLEAR T_OR_BAL-PPQTY.
  IF P_SPMON+0(6) = IT_VBEP-EDATU+0(6) AND
     P_SPMON+0(6) > W_GI_DATE+0(6)     AND
     NOT W_GI_DATE IS INITIAL.
    IF IT_VBEP-BMENG LE W_GI_QTY.
      T_OR_BAL-PPQTY = IT_VBEP-BMENG.
    ELSE.
      T_OR_BAL-PPQTY = W_GI_QTY.
    ENDIF.
    W_GI_QTY = W_GI_QTY - T_OR_BAL-PPQTY.
  ENDIF.
ENDFORM.                    " GET_PREPAYMENT_QTY
*&---------------------------------------------------------------------*
*&      Form  GET_CONFIRMED_QTY
*&---------------------------------------------------------------------*
FORM GET_CONFIRMED_QTY.
  CLEAR T_OR_BAL-CFQTY.
  IF P_SPMON+0(6) = IT_VBEP-TDDAT+0(6).
    T_OR_BAL-CFQTY = IT_VBEP-BMENG.
  ENDIF.
ENDFORM.                    " GET_CONFIRMED_QTY
*&---------------------------------------------------------------------*
*&      Form  GET_GOODS_ISSUE_QTY
*&---------------------------------------------------------------------*
FORM GET_GOODS_ISSUE_QTY.
  CLEAR T_OR_BAL-GIQTY.
  IF P_SPMON+0(6) = W_GI_DATE+0(6).
    IF IT_VBEP-BMENG LE W_GI_QTY.
      T_OR_BAL-GIQTY = IT_VBEP-BMENG.
    ELSE.
      T_OR_BAL-GIQTY = W_GI_QTY.
    ENDIF.
    W_GI_QTY = W_GI_QTY - T_OR_BAL-GIQTY.
  ENDIF.
ENDFORM.                    " GET_GOODS_ISSUE_QTY
*&---------------------------------------------------------------------*
*&      Form  GET_INVOICE_QTY
*&---------------------------------------------------------------------*
FORM GET_INVOICE_QTY.
  CLEAR T_OR_BAL-IVQTY.
  IF P_SPMON+0(6) = W_IV_DATE+0(6).
    IF IT_VBEP-BMENG LE W_GI_QTY.
      T_OR_BAL-IVQTY = IT_VBEP-BMENG.
    ELSE.
      T_OR_BAL-IVQTY = W_IV_QTY.
    ENDIF.
    W_IV_QTY = W_IV_QTY - T_OR_BAL-IVQTY.
  ENDIF.
ENDFORM.                    " GET_INVOICE_QTY
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  SORT IT_OR_BAL.
  DESCRIBE TABLE IT_OR_BAL LINES W_CNT.
  IF W_CNT = 0.
     MESSAGE I000 WITH TEXT-M01.
  ELSE.
     CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " CALL_SCREEN
