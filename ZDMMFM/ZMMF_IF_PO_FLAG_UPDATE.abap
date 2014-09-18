FUNCTION ZMMF_IF_PO_FLAG_UPDATE.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_EBELN) TYPE  EBELN
*"     VALUE(I_EBELP) TYPE  EBELP
*"----------------------------------------------------------------------

 UPDATE EKPO SET ZZTYPE = SPACE
           WHERE EBELN  = I_EBELN
             AND EBELP  = I_EBELP.


ENDFUNCTION.
