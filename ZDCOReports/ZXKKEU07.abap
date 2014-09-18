*----------------------------------------------------------------------*
*   INCLUDE ZXKKEU07                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_ERKRS) LIKE  TKEB-ERKRS
*"             VALUE(I_ITEM)
*"       EXPORTING
*"             VALUE(E_ITEM)
*"             VALUE(E_INIT)
*"       TABLES
*"              T_ACCHD STRUCTURE  ACCHD
*"              T_ACCIT STRUCTURE  ACCIT
*"              T_ACCCR STRUCTURE  ACCCR
*"              T_ACCIT_PA STRUCTURE  ACCIT_PA
*"              T_ACCCR_PA STRUCTURE  ACCCR_PA
*
*CHECK i_erkrs = 'H201'.
*
*E_ITEM = I_ITEM.
*
*DATA: ce_h201 LIKE ce1h201.
*
*ce_h201 = i_item.
** warranty/campaign & reclaim
*IF ce_h201-fkart(2) = 'ZW'.
**  ce0_h201-vv420 = - ce0_h201-vv420.
*
* E_INIT = 'X'.
*ENDIF.
