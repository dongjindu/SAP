*----------------------------------------------------------------------*
*   INCLUDE ZXMRERU03                                                  *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             REFERENCE(IS_LFM1) TYPE  LFM1
*"             REFERENCE(IS_HEAD) TYPE  ERS_HEAD
*"             REFERENCE(IS_ITEM) TYPE  MRER_ITEM
*"       CHANGING
*"             REFERENCE(CF_AUFWR) TYPE  ERS_ITEM-AUFWR
*"       EXCEPTIONS
*"              ERROR_MESSAGE_RECEIVED

*if IS_HEAD-lifnr eq 'Dagobert Duck'.
*  Cf_AUFWR = Cf_AUFWR + is_lfm1-hscabs.
*endif.
