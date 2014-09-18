FUNCTION Z_FPP_NATION_CODE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(DIST) TYPE  ZTPP_PMT07AB-DIST
*"  EXPORTING
*"     VALUE(N_CODE) TYPE  ZTPP_NATION_DEF-N_CODE
*"----------------------------------------------------------------------
  DATA: Z_CODE LIKE DIST.

  Z_CODE =  DIST.

  SELECT SINGLE * FROM ZTPP_NATION_DEF
                WHERE NATION EQ Z_CODE .

  IF SY-SUBRC NE 0.
    MESSAGE E002 WITH 'Nation Code Not Found' z_code.
  ELSE.
    N_CODE = ZTPP_NATION_DEF-N_CODE.
  ENDIF.
ENDFUNCTION.
