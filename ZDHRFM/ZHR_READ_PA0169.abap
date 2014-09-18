FUNCTION ZHR_READ_PA0169.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(PERNR) TYPE  PA0169-PERNR
*"     REFERENCE(SUBTY) TYPE  PA0169-SUBTY
*"     REFERENCE(BEGDA) TYPE  PA0169-BEGDA
*"     REFERENCE(ENDDA) TYPE  PA0169-ENDDA
*"  EXPORTING
*"     VALUE(EEPCT) TYPE  PA0169-EEPCT
*"----------------------------------------------------------------------

  data : begin of it_Pa0169 occurs 0,
          pernr like pa0169-pernr,
          EEPCT like pa0169-EEPCT,
          BEGDA like pa0169-BEGDA,
          endda like pa0169-endda,
         end of it_pa0169.

  data : flag type c,
         l_cnt type  i.

*  select count( * ) into l_cnt from PA0169 where PERNR =  PERNR and
*                                     SUBTY =  '401K' and
*                                 begda between   BEGDA and  endda.

  .

*  if sy-subrc eq 0.
*    EEPCT  =  PA0169-EEPCT.
*    exit.
*  else.
  select pernr EEPCT BEGDA endda
  into table it_Pa0169
                         from PA0169
                         where PERNR =  PERNR and
                               SUBTY =  '401K' and
                               BAREA = '10' and
                               endda >= BEGDA.
  sort   it_Pa0169  by pernr  BEGDA   .
*  endif.


  loop at it_pa0169.
    clear flag.
    if BEGDA  between it_pa0169-begda and it_pa0169-endda.
* if 2 employee contributions records are maintained in
* the same pay period then pick the last record
*      if it_pa0169-endda < endda .
*      else.
      EEPCT  =  it_pa0169-EEPCT.
*      flag = 'X'.
*      exit.

    elseif ENDDA  between it_pa0169-begda and it_pa0169-endda.
      EEPCT  =  it_pa0169-EEPCT.
*      flag = 'X'.
      exit.
    endif.

  endloop.

* Begin of  UD1K940849
* if 2 employee contributions records are maintained in
* the same pay period then pick the last record
*if flag eq 'X'.
*
*  loop at it_pa0169 where ENDDA between it_pa0169-begda and
*                                        it_pa0169-endda.
*
*      EEPCT  =  it_pa0169-EEPCT.
*      exit.
*
*  endloop.
*
*endif.


ENDFUNCTION.
