*----------------------------------------------------------------------*
*                                                                      *
*       Subroutines for infotype 9882                                  *
*                                                                      *
*----------------------------------------------------------------------*

FORM get_org_name.

  CLEAR gv_stext.
  IF NOT p9882-orgeh IS INITIAL.
    SELECT SINGLE stext FROM hrp1000 INTO gv_stext
      WHERE plvar = '01'
      AND   otype = 'O'
      AND   objid = p9882-orgeh
      AND   istat = '1'
      AND   begda <= sy-datum
      AND   endda >= sy-datum
      AND   langu = sy-langu.
  ENDIF.

ENDFORM.                    "get_org_name
