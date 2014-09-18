FUNCTION Z_FRF_MM_READ_QUANTS_PER_SU.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(LENUM) LIKE  LEIN-LENUM
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"  TABLES
*"      ET_LQUA STRUCTURE  ZSRF_MAT_SU
*"----------------------------------------------------------------------
* Date      Developer      Request              Description
* 11/20/06  Manju          UD1K923145           Initial Coding
*-----------------------------------------------------------------------
  data: ls_lein type lein.
  if not LENUM is initial.

    CALL FUNCTION 'L_READ_SU'
         EXPORTING
              I_LENUM      = lenum
         IMPORTING
              E_LEIN       = ls_lein
         EXCEPTIONS
              SU_NOT_FOUND = 1
              OTHERS       = 2.
    IF SY-SUBRC <> 0.
      E_MESS = 'Case not found'.
    else.
      SELECT *  INTO  CORRESPONDING fields of TABLE ET_LQUA
                          from LQUA
                           WHERE LGNUM = ls_lein-LGNUM
                             AND LGTYP = ls_lein-LGTYP
                             AND LGPLA = ls_lein-LGPLA
                             and lenum = ls_lein-lenum.
      if sy-subrc ne 0.
        E_MESS = 'No data found in LQUA'.
      endif.
    ENDIF.

  endif.
ENDFUNCTION.
