FUNCTION ZFEB_GET_OLD_DEALER_CODE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(NEW_DEALER) TYPE  ZDEALER1
*"       EXPORTING
*"             REFERENCE(OLD_DEALER) TYPE  ZDEALER
*"----------------------------------------------------------------------
  if not new_DEALER is initial.
    select single old_DEALER   into old_DEALER
                   from ZTEBPP_DEAL_CONV
                   where new_DEALER eq new_DEALER.

  endif.
ENDFUNCTION.
