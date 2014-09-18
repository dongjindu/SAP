FUNCTION z_fmm_create_class.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_OBJECTKEYNEW) LIKE  BAPI1003_KEY-OBJECT
*"     VALUE(I_CLASSNUMNEW) LIKE  BAPI1003_KEY-CLASSNUM
*"  TABLES
*"      IT_ZSMM_CHARACTER STRUCTURE  ZSMM_CHARACTER
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
*** existence check
  CLEAR : et_return[], et_return.
  PERFORM existence_check_calss   TABLES et_return
                                  USING  i_objectkeynew
                                         i_classnumnew.

  READ TABLE et_return WITH KEY type = 'S'.    "exist
***>>> change
  IF sy-subrc EQ 0.
* set parameters for bapi
    PERFORM set_parameter_for_class TABLES  it_zsmm_character
                                            it_allocvalueschar
                                    USING   i_classnumnew.
* exec bapi
    PERFORM update_characteristic   TABLES  it_allocvalueschar
                                            et_return
                                    USING   i_objectkeynew
                                            i_classnumnew.
***>>> create
  ELSE.
* set parameters for bapi
    PERFORM set_parameter_for_class TABLES  it_zsmm_character
                                            it_allocvalueschar
                                    USING   i_classnumnew.
* exec bapi
    PERFORM create_characteristic   TABLES  it_allocvalueschar
                                            et_return
                                    USING   i_objectkeynew
                                            i_classnumnew.
  ENDIF.



ENDFUNCTION.
