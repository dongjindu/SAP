FUNCTION Z_FCA_GET_TABLE_CONTENT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(FLAG) TYPE  ZFLAG
*"  TABLES
*"      IT_DATA STRUCTURE  ZTPPAFFW
*"----------------------------------------------------------------------
Select * into table it_data
  from ztppaffw.
if sy-subrc = 0.
   flag = 'S'.
ELSE.
  FLAG = 'E'.
ENDIF.

ENDFUNCTION.
