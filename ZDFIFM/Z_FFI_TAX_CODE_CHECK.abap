FUNCTION Z_FFI_TAX_CODE_CHECK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(LV_VALUE) TYPE  LVC_VALUE
*"  EXPORTING
*"     REFERENCE(FLAG) TYPE  C
*"----------------------------------------------------------------------

* Function Module to check Tax Code.

*Tax code  Is nothing but SSN number or Employer ID
  clear flag.

  data : l_str type i,
         l_str1 type i,
         l_str2 type i,
         l_str3 type i,
         str1(15) type c  ,
         str2(15) type c,
         str3(15) type c.

* Check Prefix
  condense lv_value no-gaps.
  l_str = strlen( lv_value ).

  if l_str > 11.
    flag = 'X'.
  elseif   lv_value  CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' .
       flag = 'X'.
  else.
    split LV_VALUE at '-' into str1 str2 str3.
    l_str1  = strlen( str1 ).
    l_str2 = strlen( str2 ).
    l_str3 = strlen( str3 ).

    if str3 is initial.
      if not ( l_str1 eq '2' and l_str2 eq '7' ).
        flag = 'X'.
      endif.
    elseif str2 is initial and str3 is initial.
      flag = 'X'.
    elseif not ( l_str1 eq 3  and l_str2 eq 2 and l_str3  eq 4 ).
      flag = 'X'.
    endif.

  endif.
ENDFUNCTION.
