*&---------------------------------------------------------------------*
*&  Include           ZMMSFDRVF04
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
**Subroutines.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMER_ADDRESS
*&---------------------------------------------------------------------*
FORM GET_CUSTOMER_ADDRESS USING    P_KUNNR LIKE EKPO-KUNNR
                          CHANGING P_ADRNR.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  DATA: L_ADRNR LIKE KNA1-ADRNR.

  CHECK NOT P_KUNNR IS INITIAL.
  SELECT SINGLE ADRNR FROM  KNA1 INTO (L_ADRNR)
         WHERE  KUNNR  = P_KUNNR.
  IF SY-SUBRC EQ 0.
    P_ADRNR = L_ADRNR.
  ELSE.
    CLEAR P_ADRNR.
  ENDIF.

ENDFORM.                    " GET_CUSTOMER_ADDRESS
