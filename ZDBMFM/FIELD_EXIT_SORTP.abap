function field_exit_sortp.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
  check sy-tcode eq 'CS01' or sy-tcode eq 'CS02'.

* UD1K941058 - by mOOn 07/19/2007 {
   check sy-ucomm ne 'CLWI'.
*  message e001(zmco) with sy-ucomm . " for debug
* }

  refresh : it_stpo.
  clear : it_plpo,lw_matnr,lw_werks,lw_stlan,
          ew_sortf.

  free memory id 'ZEOR'.

  ew_sortf = input.

  import lw_matnr from memory id 'ZMAT'.
  import lw_werks from memory id 'ZWRK'.
  import lw_idnrk from memory id 'ZIDN'.
  import lw_stlan from memory id 'ZCSV'.
*  IMPORT ew_sortf FROM MEMORY ID 'ZEOR'.
  import lw_sortf from memory id 'ZSOR'.
  export ew_sortf   to memory id 'ZEOR'.

  check lw_stlan eq '1' and lw_werks eq 'P001'.

  select single  *  from mara
          where matnr eq lw_idnrk
            and ( mtart eq 'ROH' or mtart eq 'ROH1'
                 or mtart eq 'HALB' ).

  if sy-subrc = 0 .

    select single * from marc
         where matnr eq lw_idnrk
           and werks eq lw_werks.

*    IF ( mara-mtart EQ 'ROH' OR mara-mtart EQ 'ROH1' )
*      AND marc-disgr EQ 'P010'.
*      lw_sortf  = '  '.
*      output = lw_sortf.
*      CLEAR :lw_itsob,lw_sortf.
*    ELSEIF  mara-mtart EQ 'HALB' AND lw_idnrk(1) EQ 'B'.
*      lw_sortf  = '  '.
*      output = lw_sortf.
*      CLEAR :lw_itsob,lw_sortf.
*    ELSEIF marc-sobsl = '50' .
*      lw_sortf  = ' '.
*      output = lw_sortf.
*      CLEAR :lw_itsob,lw_sortf.
*    ELSE.
    if marc-vspvb ne space.
*reference rate routing
      select single plnkn usr01
           into corresponding fields of it_plpo
                                 from plpo
                                  where  plnty eq 'M'
                                     and plnnr eq 'RP'
                                     and werks eq lw_werks
                                     and usr00 eq marc-vspvb.

* UD1K941050 by IG.MOON 7/18/2007 {
      if sy-subrc eq 0.
        lw_sortf  = it_plpo-usr01.
        output = lw_sortf.
      else.
        lw_sortf  = '17'.
        output = lw_sortf.
      endif.
* }

      clear :lw_itsob,lw_sortf.
    else.
*        lw_sortf  = '17'.
      lw_sortf  = '17'.
      output = lw_sortf.
      clear :lw_itsob,lw_sortf.
    endif.
*    ENDIF.
  else.
    lw_sortf  = ' '.
    output = lw_sortf.
    clear :lw_itsob,lw_sortf.
  endif.

endfunction.
