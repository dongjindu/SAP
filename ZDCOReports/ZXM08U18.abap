*----------------------------------------------------------------------*
*   INCLUDE ZXM08U18                                                   *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(I_WRXMOD) LIKE  WRXMOD STRUCTURE  WRXMOD
*"  EXPORTING
*"     VALUE(E_KONTO_MODIF) LIKE  T030-KOMOK

* BUKRS
tables: konv, ekko, t001.
data: l_LAND1  like lfa1-LAND1.

select single * from ekko into corresponding fields of ekko
  where ebeln = i_wrxmod-ebeln.

select single * from t001 where bukrs = ekko-bukrs.
check t001-KTOPL = 'HNA1'.

case ekko-BSART.
  when 'KD'.   E_KONTO_MODIF = 'KD'.
  when 'JIS'.  E_KONTO_MODIF = 'JIS'.
  when others.
* TEMPORARY
*     select single LAND1 into l_LAND1
*        from lfa1 where LIFNR = ekko-LIFNR.
*     if l_LAND1 <> 'US'.
*        E_KONTO_MODIF = 'KD'.
*     endif.
endcase.


*Field Description
*EKKO-LIFNR Vendor
*EKKO-LIFRE Invoicing party
*EKKO-KNUMV Document condition number
*The partner, using function module 'MM_READ_PARTNER'
*The lines in table KONV contain all conditions that were entered for
*this purchase order item. The field KONV-LIFNR contains freight
*vendors* for the relevant condition.
