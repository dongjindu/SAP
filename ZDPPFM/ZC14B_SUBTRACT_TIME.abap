FUNCTION ZC14B_SUBTRACT_TIME.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_STARTTIME) TYPE  T
*"     VALUE(I_STARTDATE) TYPE  D
*"     VALUE(I_TIME) TYPE  T
*"     VALUE(L_MODE) TYPE  C DEFAULT '-'
*"  EXPORTING
*"     VALUE(E_ENDTIME) TYPE  T
*"     VALUE(E_ENDDATE) TYPE  D
*"----------------------------------------------------------------------
 DATA : DIFFERENZ TYPE I.

 E_ENDTIME = I_STARTTIME - I_TIME.
 DIFFERENZ = E_ENDTIME - I_STARTTIME.

 IF DIFFERENZ > 0.
   E_ENDDATE = I_STARTDATE - 1.
 ELSE.
   E_ENDDATE = I_STARTDATE.
 ENDIF.

ENDFUNCTION.
