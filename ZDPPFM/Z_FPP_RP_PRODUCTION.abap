FUNCTION Z_FPP_RP_PRODUCTION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(WDATE) LIKE  SY-DATUM
*"     REFERENCE(I_INPUT_PLAN_CHK) TYPE  CHAR1
*"  TABLES
*"      T_WORDER STRUCTURE  ZSPP_DAY_WORDER OPTIONAL
*"      T_VMASTER STRUCTURE  ZSPP_DAY_VEHICLE OPTIONAL
*"  EXCEPTIONS
*"      INPUT_ERROR
*"----------------------------------------------------------------------
 DATA: L_DATE             LIKE SY-DATUM.

 L_DATE = WDATE.
 clear:  it_day_worder,   it_day_vmaster,
         it_day_worder[], it_day_vmaster[].

 perform GET_VEHICLE  USING L_DATE I_INPUT_PLAN_CHK.

 T_WORDER[]  = IT_DAY_WORDER[].
 T_VMASTER[] = IT_DAY_VMASTER[].
ENDFUNCTION.
