*----------------------------------------------------------------------*
*   INCLUDE ZXM08U09                                                   *
*----------------------------------------------------------------------*
* For blanket order

*ANDY
*"  IMPORTING
*"     VALUE(I_DRSEG) TYPE  MMCR_DRSEG
*"  TABLES
*"      T_DRSEG_CO TYPE  MMCR_TDRSEG_CO

*This customer-defined function module is used to determine a default
*account assignment when entering an invoice for a blanket purchase
*order. This enables the system to determine a proposed account
*assignment, depending on the data in the invoice.

*by Andy Choi.
TABLES: ekpo.

SELECT COUNT( * ) INTO sy-dbcnt FROM ztfi_ctl
   WHERE bukrs = i_drseg-bukrs
     AND categ = 'ZXM08U09' AND flag = 'X'.

IF sy-subrc = 0.
  LOOP AT t_drseg_co.
    SELECT SINGLE mwskz INTO t_drseg_co-mwskz FROM ekpo
      WHERE ebeln = i_drseg-ebeln
        AND ebelp = i_drseg-ebelp.

    MODIFY t_drseg_co.
  ENDLOOP.
ENDIF.
