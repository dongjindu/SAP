function z_hr_ess_search_bank .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BANKS) TYPE  BANKS OPTIONAL
*"     VALUE(BANKL) TYPE  BANKK OPTIONAL
*"     VALUE(BANKA) TYPE  BANKA OPTIONAL
*"     VALUE(ORT01) TYPE  ORT01_GP OPTIONAL
*"  TABLES
*"      ZESS_EMP_BANK_LIST STRUCTURE  ZESS_EMP_BANK_LIST
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  data:   wa_tab(72) type c,
          atab like standard table of wa_tab with non-unique
                    default key initial size 5.

  data i_bnka like bnka occurs 0 with header line.

  if banks eq space.
    banks = 'US'.
  endif.

  __cls atab.

  concatenate 'BANKS' '^eq^' '!' banks '!' into wa_tab.
  replace '^' with ' ' into : wa_tab , wa_tab.
  replace '!' with '''' into : wa_tab , wa_tab.
  replace '!' with '''' into : wa_tab , wa_tab.
  append wa_tab to atab.

  if bankl ne space.
    concatenate 'AND BANKL' '^eq^' '!' bankl '!' into wa_tab.
    replace '^' with ' ' into : wa_tab , wa_tab.
    replace '!' with '''' into : wa_tab , wa_tab.
    replace '!' with '''' into : wa_tab , wa_tab.
    append wa_tab to atab.
  endif.

  select * into table i_bnka
  from bnka where (atab).

*  09/04/2013 - T00306 Start
  delete i_bnka where loevm = 'X'.
*  09/04/2013 - T00306 End

  data app_ok.

  loop at i_bnka.

    app_ok = false.
    move-corresponding i_bnka to zess_emp_bank_list.

    translate banka to upper case.
    translate i_bnka-banka to upper case.

    translate ort01 to upper case.
    translate i_bnka-ort01 to upper case.

    if banka eq space and ort01 eq space.
      app_ok = true.
    else.

      if banka eq space.
      else.
        if i_bnka-banka cp banka.
          app_ok = true.
        else.
          app_ok = false.
          continue.
        endif.
      endif.

      if ort01 eq space.
      else.
        if i_bnka-ort01 cp ort01.
          app_ok = true.
        else.
          app_ok = false.
        endif.
      endif.
    endif.

    if app_ok eq true.
      append zess_emp_bank_list. clear zess_emp_bank_list.
    endif.

  endloop.

  return-type = 'S'.
  return-message = 'Success!'.
  append return.

endfunction.
