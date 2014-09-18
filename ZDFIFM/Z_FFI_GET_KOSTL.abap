FUNCTION Z_FFI_GET_KOSTL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_KOKRS) TYPE  KOKRS DEFAULT 'H201'
*"     REFERENCE(I_KOSTL) TYPE  KOSTL
*"  EXPORTING
*"     VALUE(E_KTEXT) TYPE  KTEXT
*"     VALUE(E_FUNC_AREA) TYPE  FKBER
*"----------------------------------------------------------------------

select single func_area ktext into (e_func_area, e_ktext)
  from csks as a inner join cskt as b on
                          b~kokrs = a~kokrs and
                          b~kostl = a~kostl and
                          b~datbi = a~datbi
  where a~kokrs = i_kokrs and
        a~kostl = i_kostl and
        a~datbi >= sy-datum and
        b~spras = sy-langu.
*  order by a~datbi.
*


ENDFUNCTION.
