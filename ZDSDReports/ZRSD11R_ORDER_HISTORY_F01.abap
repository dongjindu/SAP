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
      PERFORM GET_GI_DATE.
      W_VBELN = IT_VBEP-VBELN.
      W_POSNR = IT_VBEP-POSNR.
    ENDIF.

    PERFORM GET_MATERIAL_INFO.
    PERFORM GET_SOLD_TO_PARTY.
*    PERFORM GET_GI_DATE.
    PERFORM GET_MONTHLY_RESULTS.
    PERFORM GET_DAILY_RESULTS.

    T_OR_HIS-VRKME = IT_VBEP-VRKME.

    COLLECT T_OR_HIS INTO IT_OR_HIS. CLEAR T_OR_HIS.
  ENDLOOP.

  LOOP AT IT_OR_HIS INTO T_OR_HIS.
    IF T_OR_HIS-MPQTY = 0.
      T_OR_HIS-MACCM = 0.
    ELSE.
      T_OR_HIS-MACCM = T_OR_HIS-MRQTY / T_OR_HIS-MPQTY * 100.
    ENDIF.

    MODIFY IT_OR_HIS FROM T_OR_HIS INDEX SY-TABIX.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_INFO
*&---------------------------------------------------------------------*
FORM GET_MATERIAL_INFO.
  CLEAR : T_OR_HIS-MATNR, T_OR_HIS-ARKTX.
  READ TABLE IT_VBAP WITH KEY VBELN = IT_VBEP-VBELN
                              POSNR = IT_VBEP-POSNR.
  IF SY-SUBRC = 0.
    T_OR_HIS-MATNR = IT_VBAP-MATNR.
    T_OR_HIS-ARKTX = IT_VBAP-ARKTX.
  ENDIF.
ENDFORM.                    " GET_MATERIAL_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_SOLD_TO_PARTY
*&---------------------------------------------------------------------*
FORM GET_SOLD_TO_PARTY.
  CLEAR T_OR_HIS-KUNNM.
  READ TABLE IT_VBAK WITH KEY VBELN = IT_VBEP-VBELN.
  IF SY-SUBRC = 0.
    SELECT SINGLE *
           FROM KNA1
          WHERE KUNNR EQ IT_VBAK-KUNNR.
    IF SY-SUBRC = 0.
      T_OR_HIS-KUNNM = KNA1-MCOD1.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_SOLD_TO_PARTY
*&---------------------------------------------------------------------*
*&      Form  GET_GI_DATE
*&---------------------------------------------------------------------*
FORM GET_GI_DATE.
  CLEAR  : W_GI_DATE, W_GI_QTYM, W_GI_QTY.
  SELECT *
         FROM VBFA
        WHERE VBELV EQ IT_VBEP-VBELN
        AND   POSNV EQ IT_VBEP-POSNR
        AND   ( VBTYP_N EQ 'R' OR
                VBTYP_N EQ 'h' ).
  ENDSELECT.
  IF SY-SUBRC = 0 AND VBFA-VBTYP_N = 'R'.
    W_GI_DATE = VBFA-ERDAT.
    W_GI_QTYM = VBFA-RFMNG.
    W_GI_QTY  = VBFA-RFMNG.
  ENDIF.
ENDFORM.                    " GET_GI_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_MONTHLY_RESULTS
*&---------------------------------------------------------------------*
FORM GET_MONTHLY_RESULTS.
  CLEAR : T_OR_HIS-MPQTY, T_OR_HIS-MRQTY.
  IF P_EXEDT+0(6) = IT_VBEP-TDDAT+0(6).
    T_OR_HIS-MPQTY = IT_VBEP-BMENG.
  ENDIF.

  IF P_EXEDT+0(6) = W_GI_DATE+0(6).
    T_OR_HIS-MRQTY = IT_VBEP-BMENG.
    IF IT_VBEP-BMENG LE W_GI_QTYM.
      T_OR_HIS-MRQTY = IT_VBEP-BMENG.
    ELSE.
      T_OR_HIS-MRQTY = W_GI_QTYM.
    ENDIF.
    W_GI_QTYM = W_GI_QTYM - T_OR_HIS-MRQTY.
  ENDIF.
ENDFORM.                    " GET_MONTHLY_RESULTS
*&---------------------------------------------------------------------*
*&      Form  GET_DAILY_RESULTS
*&---------------------------------------------------------------------*
FORM GET_DAILY_RESULTS.
  CLEAR : T_OR_HIS-DPQTY, T_OR_HIS-DRQTY.
  IF P_EXEDT = IT_VBEP-TDDAT.
    T_OR_HIS-DPQTY = IT_VBEP-BMENG.
  ENDIF.

  IF P_EXEDT = W_GI_DATE.
    T_OR_HIS-DRQTY = IT_VBEP-BMENG.
    IF IT_VBEP-BMENG LE W_GI_QTY.
      T_OR_HIS-DRQTY = IT_VBEP-BMENG.
    ELSE.
      T_OR_HIS-DRQTY = W_GI_QTY.
    ENDIF.
    W_GI_QTY = W_GI_QTY - T_OR_HIS-DRQTY.
  ENDIF.
ENDFORM.                    " GET_DAILY_RESULTS
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  SORT IT_OR_HIS.
  DESCRIBE TABLE IT_OR_HIS LINES W_CNT.
  IF W_CNT = 0.
     MESSAGE I000 WITH TEXT-M01.
  ELSE.
     CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " CALL_SCREEN
