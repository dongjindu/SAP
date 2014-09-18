*----------------------------------------------------------------------*
*   INCLUDE MZAHR0003F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM SELECT_DATA.
  DATA: L_BEGDA LIKE SY-DATUM,
        L_YEARS TYPE I,
        L_DOMVA LIKE DD07V-DOMVALUE_L,
        L_DTEXT LIKE DD07V-DDTEXT,
        L_ENTRY LIKE HIDA OCCURS 1 WITH HEADER LINE.
*
  CLEAR IT_S9000. REFRESH IT_S9000.
*
  SELECT PERNR WERKS PERSG PERSK KOSTL STELL
    INTO (PA0001-PERNR, PA0001-WERKS, PA0001-PERSG,
          PA0001-PERSK, PA0001-KOSTL, PA0001-STELL)
    FROM PA0001 WHERE ENDDA = '99991231'.
    IF PA0001-PERSG <> '2'.
      IT_S9000-ZVERS = W_ZVERS.
      IT_S9000-ZYEAR = SY-DATUM+(4).
      IT_S9000-ZMONS = SY-DATUM+4(2).
      IT_S9000-ZPERA = PA0001-WERKS.
      IT_S9000-ZPERG = PA0001-PERSG.
      IT_S9000-ZSUBG = PA0001-PERSK.
      IT_S9000-ZCOST = PA0001-KOSTL.
      IT_S9000-ZJOBK = PA0001-STELL.
      IT_S9000-ZHEDC = 1.
*.. get entry date
      CLEAR: L_ENTRY, L_ENTRY[].
      CALL FUNCTION 'HR_ENTRY_DATE'
           EXPORTING PERSNR      = PA0001-PERNR
           IMPORTING ENTRYDATE   = L_BEGDA
           TABLES    ENTRY_DATES = L_ENTRY.
*.. get working years
      CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
           EXPORTING BEG_DA     = L_BEGDA
                     END_DA     = SY-DATUM
           IMPORTING NO_YEAR    = L_YEARS.
      IF L_YEARS = 0. L_YEARS = 1. ENDIF.
*
      IT_S9000-ZSENR = L_YEARS.
      COLLECT IT_S9000. CLEAR IT_S9000.
    ENDIF.
  ENDSELECT.
*... get job description
  LOOP AT IT_S9000.
    CLEAR HRP1000.
    SELECT SINGLE SHORT INTO HRP1000-SHORT
      FROM HRP1000 WHERE PLVAR = '01'
                     AND OTYPE = 'C'
                     AND OBJID = IT_S9000-ZJOBK
                     AND ISTAT = '1'
                     AND ENDDA = '99991231'
                     AND LANGU = SY-LANGU.
    IT_S9000-ZJOBK = HRP1000-SHORT.

    CLEAR T500P.
    SELECT SINGLE NAME1 INTO T500P-NAME1
      FROM T500P WHERE PERSA = IT_S9000-ZPERA
                   AND MOLGA = '10'
                   AND BUKRS = 'H201'.
    IT_S9000-NAME1 = T500P-NAME1.
    MODIFY IT_S9000. CLEAR IT_S9000.
  ENDLOOP.
*
  L_DOMVA = SY-DATUM+4(2).
  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      I_DOMNAME        = 'MONTH'
      I_DOMVALUE       = L_DOMVA
    IMPORTING
      E_DDTEXT         = L_DTEXT
    EXCEPTIONS
      NOT_EXIST        = 1
      OTHERS           = 2.
*
  IT_S9000-MONTH = L_DTEXT.
  MODIFY IT_S9000 TRANSPORTING MONTH WHERE MONTH = SPACE.
  SORT IT_S9000 BY ZCOST ZJOBK ZPERG.
*
  REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
  DESCRIBE TABLE IT_S9000 LINES TC9000-LINES.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM SAVE_DATA.
  CLEAR IT_AHC01. REFRESH IT_AHC01.
*
  LOOP AT IT_S9000.
    MOVE-CORRESPONDING IT_S9000 TO IT_AHC01.
    APPEND IT_AHC01. CLEAR IT_AHC01.
  ENDLOOP.
*
  MODIFY ZTHR_AHC01 FROM TABLE IT_AHC01.
  IF SY-SUBRC = 0.
    MESSAGE S001 WITH 'DATA SAVED'.
  ENDIF.
ENDFORM.                    " SAVE_DATA
