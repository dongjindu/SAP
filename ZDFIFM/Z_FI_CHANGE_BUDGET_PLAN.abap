function z_fi_change_budget_plan.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PROGRAM) LIKE  BAPIPROGPOSID-PROGRAM
*"     VALUE(APPROVALYEAR) LIKE  BAPIPROGPOSID-APPROVALYEAR
*"     VALUE(POSITION) LIKE  BAPIPROGPOSID-POSITION
*"     VALUE(LINE_ITEM_TEXT) LIKE  BAPIPROGAUX-LINE_ITEM_TEXT DEFAULT
*"       ' '
*"     VALUE(ROLLUP) LIKE  BAPIPROGAUX-ROLLUP DEFAULT ' '
*"     VALUE(TEST_RUN) LIKE  BAPIPROGAUX-TEST_RUN DEFAULT ' '
*"     REFERENCE(WRTTP) TYPE  CO_WRTTP
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      PROGTREEVALUES STRUCTURE  BAPIPROGVAL
*"  EXCEPTIONS
*"      NO_VALUE_CHANGE
*"----------------------------------------------------------------------

  data: ls_objnr type im_objnr,
        ls_bpja type bpja,
        l_vorga type bp_vorgang.

  data: lt_treevalues	like table of bapiprogval with header line.

  select single objnr
    into ls_objnr
    from impr
   where gjahr = approvalyear
     and posid = position.

  if wrttp eq '47'.
    l_vorga = 'KBUD'.
  else.
    l_vorga = 'KSTP'.
  endif.

  select single *
    into ls_bpja
    from bpja
   where lednr = '0001'
     and objnr = ls_objnr
     and trgkz = 'N'
     and wrttp = wrttp
     and vorga = l_vorga
     and gjahr = approvalyear.

*  data: l_value type bapicurr_d.

  loop at progtreevalues.
    move-corresponding progtreevalues to lt_treevalues.
    lt_treevalues-value = progtreevalues-value - ls_bpja-wtjhr.
    append lt_treevalues.
    if lt_treevalues-value eq 0.
      raise no_value_change.
    endif.
  endloop.


  call function 'BAPI_EXPENDITUREPROGTREE_CHVAL'
    exporting
      program        = program
      approvalyear   = approvalyear
      position       = position
      line_item_text = line_item_text
      rollup         = rollup
      test_run       = test_run
    tables
      return         = return
      progtreevalues = lt_treevalues.

endfunction.
