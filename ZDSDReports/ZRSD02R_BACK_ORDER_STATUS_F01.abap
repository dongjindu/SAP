*----------------------------------------------------------------------*
***INCLUDE ZRSD02R_BACK_ORDER_STATUS_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATE
*&---------------------------------------------------------------------*
FORM CHECK_DATE.
  DATA : W_DATE LIKE P_DATE.

  IF P_DATE > SY-DATUM+2(4).
    MESSAGE E000 WITH 'Input date(YYMM) is bigger than system date'.
  ENDIF.

  IF P_DATE = SY-DATUM+2(4).
    IF P_DATE+2(2) = '01'.
      W_DATE+0(2) = P_DATE+0(2) - 1.
      W_DATE+2(2) = '12'.
    ELSE.
      W_DATE+0(2) = P_DATE+0(2).
      W_DATE+2(2) = P_DATE+2(2) - 1.
    ENDIF.

    S_WO_SER-SIGN = 'I'.
    S_WO_SER-OPTION = 'BT'.
    CONCATENATE 'E' '0000' '000' INTO S_WO_SER-LOW.
    CONCATENATE 'E' W_DATE 'ZZZ' INTO S_WO_SER-HIGH.
    APPEND S_WO_SER.

    CONCATENATE 'D' '0000' '000' INTO S_WO_SER-LOW.
    CONCATENATE 'D' W_DATE 'ZZZ' INTO S_WO_SER-HIGH.
    APPEND S_WO_SER.
  ELSE.
    S_WO_SER-SIGN = 'I'.
    S_WO_SER-OPTION = 'BT'.
    CONCATENATE 'E' '0000' '000' INTO S_WO_SER-LOW.
    CONCATENATE 'E' P_DATE 'ZZZ' INTO S_WO_SER-HIGH.
    APPEND S_WO_SER.

    CONCATENATE 'D' '0000' '000' INTO S_WO_SER-LOW.
    CONCATENATE 'D' P_DATE 'ZZZ' INTO S_WO_SER-HIGH.
    APPEND S_WO_SER.
  ENDIF.
ENDFORM.                    " CHECK_DATE
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  SELECT *
         INTO TABLE IT_WOSUM
         FROM ZTPP_WOSUM
        WHERE WO_SER IN S_WO_SER.

  LOOP AT IT_WOSUM.
    IF IT_WOSUM-MODQTY = IT_WOSUM-SEQQTY.
       DELETE IT_BACK_OR INDEX SY-TABIX.
    ELSE.
       MOVE-CORRESPONDING IT_WOSUM TO T_BACK_OR.
       T_BACK_OR-FSC    = IT_WOSUM-FSC+6(12).
       T_BACK_OR-DIFQTY = IT_WOSUM-MODQTY - IT_WOSUM-SEQQTY.
       APPEND T_BACK_OR TO IT_BACK_OR. CLEAR T_BACK_OR.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  SORT IT_BACK_OR BY NATION DEALER WO_SER EXTC INTC.
  DESCRIBE TABLE IT_BACK_OR LINES W_CNT.
  IF W_CNT = 0.
     MESSAGE I000 WITH TEXT-M01.
  ELSE.
     CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " CALL_SCREEN
