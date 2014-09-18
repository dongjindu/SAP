FUNCTION ZGIM_EMPLOYEE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DATUM) TYPE  SY-DATUM OPTIONAL
*"     VALUE(IV_CHANGE) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      ET_PERNR STRUCTURE  ZGIMT0001
*"  CHANGING
*"     VALUE(CV_SUBRC) TYPE  SY-SUBRC
*"----------------------------------------------------------------------

  IF iv_change IS INITIAL.
*   All Data Transfer
    SELECT * FROM zgimt0001 INTO CORRESPONDING FIELDS OF TABLE et_pernr.
  ELSE.
*   Only Change data Transfer
    SELECT * FROM zgimt0001 INTO CORRESPONDING FIELDS OF TABLE et_pernr
            WHERE aedtm = iv_datum.
  ENDIF.

  cv_subrc = sy-subrc.

ENDFUNCTION.
