FUNCTION ZFEB_GET_NEW_DEALER_CODE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(OLD_DEALER) TYPE  ZDEALER
*"  EXPORTING
*"     REFERENCE(NEW_DEALER) TYPE  ZDEALER1
*"----------------------------------------------------------------------
  if not OLD_DEALER is initial.
    select single NEW_DEALER   into NEW_DEALER
                   from ZTEBPP_DEAL_CONV
                   where OLD_DEALER eq OLD_DEALER.

    IF SY-SUBRC <> 0 .
      NEW_DEALER = OLD_DEALER.
    ENDIF.
  endif.

ENDFUNCTION.
