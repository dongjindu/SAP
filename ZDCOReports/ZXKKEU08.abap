*----------------------------------------------------------------------*
*   INCLUDE ZXKKEU08                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_ERKRS)
*"       TABLES
*"              T_ACCHD STRUCTURE  ACCHD
*"              T_ACCIT STRUCTURE  ACCIT
*"              T_ACCCR STRUCTURE  ACCCR
*"              T_ACCIT_PA STRUCTURE  ACCIT_PA
*"              T_ACCCR_PA STRUCTURE  ACCCR_PA
*"              T_ITEM

*----------------------------------------------------------------------*
*   by Andy
*----------------------------------------------------------------------*

CHECK i_erkrs = 'H201'.

DATA: ce_h201 LIKE ce1h201.

LOOP AT t_item INTO ce_h201.
* warranty/campaign claim  ; SD credit(-), FI(debit), PA(-)
* warranty/campaign reclaim; SD debit (+), FI(credit),PA(+)
* change PA sign
  IF ce_h201-fkart(2) = 'ZW'.
    CLEAR ce_h201-absmg.
    ce_h201-vv495 = - ce_h201-vv495.
    ce_h201-vv500 = - ce_h201-vv500.
    t_item = ce_h201.
    MODIFY t_item INDEX sy-tabix.
* credit/debit amount for vehicle invoice
* reset qty (IMG), reset valuation
  ELSEIF ce_h201-fkart = 'ZVG2' OR ce_h201-fkart = 'ZVL2'.
    CLEAR: ce_h201-vv320, ce_h201-vv330, ce_h201-vv340,
           ce_h201-vv350, ce_h201-vv360, ce_h201-vv200,
           ce_h201-vv201, ce_h201-vv210, ce_h201-vv211,
           ce_h201-vv220, ce_h201-vv221, ce_h201-vv231,
           ce_h201-vv230, ce_h201-vv240, ce_h201-vv241,
           ce_h201-vv250, ce_h201-vv251, ce_h201-vv260,
           ce_h201-vv261, ce_h201-vv270, ce_h201-vv271,
           ce_h201-vv280, ce_h201-vv281, ce_h201-vv290,
           ce_h201-vv291, ce_h201-vv300, ce_h201-vv301,
           ce_h201-vv310.
    t_item = ce_h201.
    MODIFY t_item INDEX sy-tabix.
  ENDIF.

ENDLOOP.
