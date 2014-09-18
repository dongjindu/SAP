FUNCTION Z_ELM_GET_CONTRACTS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BEGDA) TYPE  DATUM OPTIONAL
*"     VALUE(ENDDA) TYPE  DATUM OPTIONAL
*"  TABLES
*"      ZELM_CONTRACT STRUCTURE  ZELM_CONTRACT
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  data $sum like ekpo-BRTWR.

  if BEGDA is initial.
    begda = sy-datum.
  endif.

  if enddA is initial.
    endda = begda.
  endif.

  select * from ekko where BUKRS = 'H201'
                       and ( BSTYP eq 'F' or BSTYP eq 'K' )
                       and ( BEDAT >= BEGDA and BEDAT <= ENDDA )
                       and BSART eq 'ZB'
                       and LOEKZ eq space.
*                       and AUTLF eq 'X'.

    ZELM_CONTRACT-FIELD1 = ekko-ebeln.
    concatenate 'P/O#:' ekko-ebeln into ZELM_CONTRACT-FIELD2.
    ZELM_CONTRACT-FIELD3 = '999'.

    select single * from lfa1 where lifnr = ekko-LIFNR.

    if sy-subrc eq 0.
      ZELM_CONTRACT-FIELD5 = lfa1-NAME1.
    endif.

    clear $sum.
    select * from ekpo where EBELN eq ekko-ebeln
                         and LOEKZ eq space.
*                         and ELIKZ eq 'X'.

      add ekpo-BRTWR to $sum.

    endselect.
    if sy-subrc ne 0.
      continue.
    endif.

    select single * from ekkn where ebeln = ekko-ebeln.
    if sy-subrc eq 0.
      select single ORGEH into ZELM_CONTRACT-FIELD4
      from pa0001 where kostl eq ekkn-kostl
                    and endda eq '99991231'.
    endif.

    ZELM_CONTRACT-FIELD6 = $sum.
    ZELM_CONTRACT-FIELD7 = 'USD'.

    ZELM_CONTRACT-FIELD8 = ekko-BEDAT.
    ZELM_CONTRACT-FIELD10 = 'SAP I/F'.
    ZELM_CONTRACT-FIELD11 = ZELM_CONTRACT-FIELD4.

    append ZELM_CONTRACT.

  endselect.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.

ENDFUNCTION.
