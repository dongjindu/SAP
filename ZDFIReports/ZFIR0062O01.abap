*----------------------------------------------------------------------*
***INCLUDE ZFIR0062F01 .
*----------------------------------------------------------------------*
FORM modify_zfit0090 USING p_rcdat.

  CLEAR   it_90.
  REFRESH it_90.

  LOOP AT itab WHERE check = 'X'
               AND   deakt IS INITIAL.      "Deactivation date
    it_90-bukrs = p_bukrs.
    it_90-pernr = zfit0090-pernr.
    it_90-rcdat = zfit0090-rcdat.
    it_90-anln1 = itab-anln1.
    it_90-statu = 'L'.
    APPEND it_90.
    CLEAR  it_90.
  ENDLOOP.
  IF sy-subrc = 0.
    DELETE FROM zfit0090 WHERE bukrs = p_bukrs
                         AND   pernr = zfit0090-pernr
                         AND   rcdat = p_rcdat
                         AND   statu IN ('L', 'D').
    MODIFY zfit0090 FROM TABLE it_90.
    IF sy-subrc = 0.
      MESSAGE s178 WITH zfit0090-pernr zfit0090-rcdat.
    ELSE.
      MESSAGE e061 WITH 'ZFIT0090'.
    ENDIF.
  ENDIF.

ENDFORM.                    "modify_zfit0090
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZFIT0091
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_zfit0091 USING p_isdat.

  CLEAR   it_91.
  REFRESH it_91.

  LOOP AT itab WHERE check = 'X'
               AND   deakt IS INITIAL       "Deactivation date
               AND   NOT e_acq IS INITIAL.  "Acquistion cost
    it_91-bukrs = p_bukrs.
    it_91-pernr = zfit0091-pernr.
    it_91-isdat = zfit0091-isdat.
    it_91-anln1 = itab-anln1.
    it_91-statu = 'L'.
    APPEND it_91.
    CLEAR  it_91.
  ENDLOOP.
  IF sy-subrc = 0.
    DELETE FROM zfit0091 WHERE bukrs = p_bukrs
                         AND   pernr = zfit0091-pernr
                         AND   isdat = p_isdat
                         AND   statu IN ('L', 'D').
    MODIFY zfit0091 FROM TABLE it_91.
    IF sy-subrc = 0.
      MESSAGE s178 WITH zfit0091-pernr zfit0091-isdat.
    ELSE.
      MESSAGE e061 WITH 'ZFIT0091'.
    ENDIF.
  ENDIF.

ENDFORM.                    " MODIFY_ZFIT0091
