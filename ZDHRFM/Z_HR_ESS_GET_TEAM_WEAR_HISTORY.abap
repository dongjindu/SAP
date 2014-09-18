function z_hr_ess_get_team_wear_history.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_TEAMWEAR_HISTORY STRUCTURE  ZESS_TEAMWEAR_HISTORY
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  __cls : zess_teamwear_history.

  select bvmrk termn mndat into
  (zess_teamwear_history-bvmrk,zess_teamwear_history-termn,
zess_teamwear_history-mndat)
  from pa0019 where pernr eq employee_number
   and tmart eq 'ZT'.
*   and bvmrk ne space.
    append zess_teamwear_history.
  endselect.

  sort zess_teamwear_history by mndat DESCENDING.

  return-type = 'S'.
  return-message = 'Success!'.
  append return.

endfunction.
