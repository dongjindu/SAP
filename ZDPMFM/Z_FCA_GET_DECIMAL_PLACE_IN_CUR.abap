FUNCTION Z_FCA_GET_DECIMAL_PLACE_IN_CUR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_CURRKEY) TYPE  TCURX-CURRKEY
*"  EXPORTING
*"     REFERENCE(E_POINT) TYPE  CFFSD-RVALF
*"----------------------------------------------------------------------
  TABLES: TCURX.

  MOVE  1          TO  E_POINT.

  SELECT SINGLE * FROM TCURX WHERE CURRKEY = I_CURRKEY.
  IF SY-SUBRC <> 0.
    TCURX-CURRDEC = 2.
  ENDIF.
  IF TCURX-CURRDEC > 2.
    TCURX-CURRDEC = TCURX-CURRDEC - 2.
    DO TCURX-CURRDEC TIMES.
      E_POINT = E_POINT / 10.
    ENDDO.
  ELSE.
    TCURX-CURRDEC = 2 - TCURX-CURRDEC.
    DO TCURX-CURRDEC TIMES.
      E_POINT = E_POINT * 10.
    ENDDO.
  ENDIF.
ENDFUNCTION.
