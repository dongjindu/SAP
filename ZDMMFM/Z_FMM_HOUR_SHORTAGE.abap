FUNCTION Z_FMM_HOUR_SHORTAGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      PT_SHORT STRUCTURE  ZMMS_SHORT
*"----------------------------------------------------------------------

perform GET_VIN_DATA(ZMMR_PARTS_SHORTAGE)
    TABLES PT_SHORT.

ENDFUNCTION.
