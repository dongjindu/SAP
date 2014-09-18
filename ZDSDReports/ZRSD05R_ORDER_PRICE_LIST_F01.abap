*----------------------------------------------------------------------*
***INCLUDE ZRSD05R_ORDER_PRICE_LIST_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_INIT
*&---------------------------------------------------------------------*
FORM CHECK_INIT.
  REFRESH : S_KUNNR, S_BSTNK.
  CLEAR   : S_KUNNR, S_BSTNK.

  IF S_YYMM-OPTION = 'EQ'.
    S_BSTNK-SIGN = 'I'. S_BSTNK-OPTION = 'BT'.
    CONCATENATE 'E' S_YYMM-LOW  '000000000000000' INTO S_BSTNK-LOW.
    CONCATENATE 'E' S_YYMM-LOW  'ZZZZZZZZZZZZZZZ' INTO S_BSTNK-HIGH.
    APPEND S_BSTNK.
  ELSE. "BT
    S_BSTNK-SIGN = 'I'. S_BSTNK-OPTION = 'BT'.
    CONCATENATE 'E' S_YYMM-LOW  '000000000000000' INTO S_BSTNK-LOW.
    CONCATENATE 'E' S_YYMM-HIGH 'ZZZZZZZZZZZZZZZ' INTO S_BSTNK-HIGH.
    APPEND S_BSTNK.
  ENDIF.

  IF S_KUNN-OPTION = 'EQ'.
    S_KUNNR-SIGN = 'I'. S_KUNNR-OPTION = 'BT'.
    CONCATENATE S_KUNN-LOW  'AA' INTO S_KUNNR-LOW.
    CONCATENATE S_KUNN-LOW  'ZZ' INTO S_KUNNR-HIGH.
    APPEND S_KUNNR.
  ELSE. "BT
    S_KUNNR-SIGN = 'I'. S_KUNNR-OPTION = 'BT'.
    CONCATENATE S_KUNN-LOW  'AA' INTO S_KUNNR-LOW.
    CONCATENATE S_KUNN-HIGH 'ZZ' INTO S_KUNNR-HIGH.
    APPEND S_KUNNR.
  ENDIF.
ENDFORM.                    " CHECK_INIT
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  SELECT *
         INTO TABLE IT_VBAK
         FROM VBAK
        WHERE BSTNK IN S_BSTNK
        AND   KUNNR IN S_KUNNR.

  SELECT *
         INTO TABLE IT_VBAP
         FROM VBAP
              FOR ALL ENTRIES IN IT_VBAK
        WHERE VBELN EQ IT_VBAK-VBELN.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA.
  LOOP AT IT_VBAK.
    T_OR_PRL-KUNNR  = IT_VBAK-KUNNR.
    T_OR_PRL-BSTNK  = IT_VBAK-BSTNK+0(9).
    T_OR_PRL-COLOR  = IT_VBAK-BSTNK+14(4).
    T_OR_PRL-VBELN  = IT_VBAK-VBELN.
    T_OR_PRL-WAERK  = IT_VBAK-WAERK.

    LOOP AT IT_VBAP WHERE VBELN = IT_VBAK-VBELN.
      IF IT_VBAP-POSNR = '000010'.
        T_OR_PRL-MATNR  = IT_VBAP-MATNR.
        T_OR_PRL-ARKTX  = IT_VBAP-ARKTX.
        T_OR_PRL-KWMENG = IT_VBAP-KWMENG.
        T_OR_PRL-VRKME  = IT_VBAP-VRKME.
      ELSE. " '000020'
        T_OR_PRL-KWMENG = T_OR_PRL-KWMENG + IT_VBAP-KWMENG.
      ENDIF.
    ENDLOOP.

    SELECT * FROM KONV WHERE KNUMV = IT_VBAK-KNUMV.
      CASE KONV-KSCHL.
      WHEN 'ZV00'.
      T_OR_PRL-KWERTZ = T_OR_PRL-KWERTZ + KONV-KWERT.
      WHEN 'VA00'.
      T_OR_PRL-KWERTV = T_OR_PRL-KWERTV + KONV-KWERT.
      ENDCASE.
    ENDSELECT.

**    IF T_OR_PRL-KWERTV = 0.
**      T_OR_PRL-COLOR = ''.
**    ENDIF.

    T_OR_PRL-KWERT  = T_OR_PRL-KWERTZ + T_OR_PRL-KWERTV.

    APPEND T_OR_PRL TO IT_OR_PRL. CLEAR T_OR_PRL.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  SORT IT_OR_PRL.
  DESCRIBE TABLE IT_OR_PRL LINES W_CNT.
  IF W_CNT = 0.
     MESSAGE I000 WITH TEXT-M01.
  ELSE.
     CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " CALL_SCREEN
