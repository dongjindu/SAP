*----------------------------------------------------------------------*
*   INCLUDE ZXM02U08                                                   *
*----------------------------------------------------------------------*
* Date      Developer    Request        Description
* 11/14/06  Manju        UD1K923009     Vaatz PR user exit changes
*
*----------------------------------------------------------------------*

*data : begin of it_EBaN occurs 0,
*        BANFN like EBKN-BANFN,
*        BNFPO like ebkn-BNFPO,
*        zztype1 like eban-zztype1,
*       end of it_eban.
**
*data: wa_eban type MEREQ_T_UEBAN with header line,
*      flag.
*
*clear flag.
*** EBKN changes
**wa_ebkn = IM_EBKN_CHANGES.
**
*** EBAN changes
*wa_eban[] = IM_EBAN_CHANGES.
**
*if not wa_eban[]  is initial.
*  select  BANFN
*          BNFPO
*          zztype1
*          into table it_eban
*          from eban for all entries in wa_eban
*          where BANFN = wa_eban-banfn  and
*                BNFPO = wa_eban-bnfpo.
** New line added
*  if sy-subrc ne 0.
*    select  BANFN
*            BNFPO
*            zztype1
*            into table it_eban
*            from eban for all entries in wa_eban
*            where BANFN = wa_eban-banfn .
*
*
** if one of the line as 'S' Status and don't allow to add
** new line to PR
*    loop at it_eban.
*      check it_eban-ZZTYPE1 eq 'S' .
*      flag = 'X' .
*      exit.
*    endloop.
*    if flag = 'X'.
* message e000(009) with 'Cant add new line to PR which is interface to
*VAATZ'.
*    endif .
*  endif.
*  endif.
