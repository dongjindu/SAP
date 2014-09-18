*----------------------------------------------------------------------*
*   INCLUDE ZXMRERU01                                                  *
*----------------------------------------------------------------------*
* Header

*"  TABLES
*"      CT_HEAD TYPE  MRER_HEAD_TABTYPE
*"      CT_ITEM TYPE  MRER_ITEM_TABTYPE
*"  EXCEPTIONS
*"      ERROR_MESSAGE_RECEIVED

*data: lf_tabix like  sy-tabix,
*      ls_head  type  mrer_head,
*      ls_item  TYPE  MRER_ITEM.
*
*loop at CT_ITEM into ls_item.
*  lf_tabix = sy-tabix.
*
*  ls_ITEM-XBLNR = 'andy...'.
*  MODIFY CT_ITEM  from ls_item INDEX lf_tabix.
*
*
**  if ls_ITEM-shkzg eq c_signs. ".    S: Rechnungen/Gutschriftsanzeigen
**
**  elseif ls_ITEM-shkzg eq c_signh. " H:
**  Gutschriften/Belastungssanzeigen
*** Kopfdaten nachlesen
**    READ TABLE CT_HEAD WITH KEY EBELN = ls_ITEM-EBELN
**                                ebelp = ls_ITEM-ebelp into Ls_head.
**    if  ls_HEAD-LIFNR ne ls_HEAD-LIFRE.
**      check   not     ls_ITEM-AUFWR is initial.
**      ls_ITEM-AUFWR = ls_ITEM-AUFWR * 2.
**      MODIFY CT_ITEM  from ls_item INDEX lf_tabix.
**    endif.
**  endif.
*endloop.
