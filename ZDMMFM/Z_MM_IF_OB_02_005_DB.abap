FUNCTION Z_MM_IF_OB_02_005_DB.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0048
*"----------------------------------------------------------------------
 CLEAR : it_m048, it_m048[].

  it_m048[] = it_body[].

  CHECK NOT it_m048[] IS INITIAL.

  MODIFY zmmt0048 FROM TABLE it_m048.

ENDFUNCTION.
