function zppc1tp_update_resb.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IF_RSNUM) TYPE  RSNUM
*"     VALUE(IF_SORTF) TYPE  SORTP
*"     VALUE(IF_REVERSAL) TYPE  PPC_FLG_REV
*"----------------------------------------------------------------------

  if if_reversal is initial.

*   direct confirmation
    update ppc1tp_resb
            set enmng = ppc1tp_resb~bdmng
            where rsnum = if_rsnum
              and sortf = if_sortf.

  else.

*   reversal, reset quantity
    update ppc1tp_resb
            set enmng = 0
            where rsnum = if_rsnum
              and sortf = if_sortf.

  endif.


endfunction.
