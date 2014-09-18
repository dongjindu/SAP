*----------------------------------------------------------------------*
*   INCLUDE MZAHR0001F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
  IF NOT DEL_WTMNG[] IS INITIAL.
    CLEAR ZTHR_WTMNG.
    DELETE ZTHR_WTMNG FROM TABLE DEL_WTMNG.
  ENDIF.
*
  MODIFY ZTHR_WTMNG FROM TABLE IT_WTMNG.
  IF SY-SUBRC = 0.
    CLEAR TMP_WTMNG. REFRESH TMP_WTMNG.
    TMP_WTMNG[] = IT_WTMNG[].
    MESSAGE S001 WITH 'DATA SAVED'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_DATA
*&---------------------------------------------------------------------*
FORM APPEND_DATA.
  DATA L_ZCOLN LIKE ZTHR_WTMNG-ZCOLN.
*
  SORT IT_WTMNG BY ZFORM ZCOLN.
  IF W_ZPRNT = SPACE.
    L_ZCOLN = W_ZCOLN + 1.
    LOOP AT IT_WTMNG WHERE ZFORM = W_ZFORM
                       AND ZCOLN >= W_ZCOLN
                       AND ZMARK = SPACE.
      IT_WTMNG-ZCOLN = L_ZCOLN.
      MODIFY IT_WTMNG. CLEAR IT_WTMNG.
      L_ZCOLN = L_ZCOLN + 1.
    ENDLOOP.
  ENDIF.
*
  CLEAR IT_WTMNG.
  IT_WTMNG-ZFORM = W_ZFORM.
  IT_WTMNG-LGART = W_LGART.
  IT_WTMNG-LGTXT = W_LGTXT.
  IT_WTMNG-ZCOLN = W_ZCOLN.
  IT_WTMNG-ZPRNT = W_ZPRNT.
  IF W_ZPRNT = SPACE.
    IT_WTMNG-ZMARK = SPACE.
  ELSE.
    IT_WTMNG-ZMARK = 'X'.
    IT_WTMNG-ZCOLN = T_ZCOLN.
  ENDIF.
  IT_WTMNG-ERDAT = SY-DATUM.
  IT_WTMNG-ERNAM = SY-UNAME.
  IT_WTMNG-ERZET = SY-UZEIT.
  IT_WTMNG-STATUS = SPACE.
  APPEND IT_WTMNG. CLEAR IT_WTMNG.
*
  CLEAR CMP_WTMNG. REFRESH CMP_WTMNG.
  CMP_WTMNG[] = IT_WTMNG[].
  LOOP AT IT_WTMNG WHERE ZPRNT NE SPACE.
    CLEAR CMP_WTMNG.
    READ TABLE CMP_WTMNG WITH KEY ZFORM = IT_WTMNG-ZFORM
                                  LGART = IT_WTMNG-ZPRNT.
    IT_WTMNG-ZCOLN = CMP_WTMNG-ZCOLN.
    MODIFY IT_WTMNG. CLEAR IT_WTMNG.
  ENDLOOP.
*
  SORT IT_WTMNG BY ZFORM ZCOLN LGART.
* REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_WTMNG LINES TC9000-LINES.

ENDFORM.                    " APPEND_DATA
