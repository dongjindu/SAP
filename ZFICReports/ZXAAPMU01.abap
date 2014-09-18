*----------------------------------------------------------------------*
*   INCLUDE ZXAAPMU01                                                  *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(I_ITOB_REC) LIKE  ITOB STRUCTURE  ITOB
*"     VALUE(I_ANKAZ_TAB) TYPE  AAPMT_ANKAZ_TAB
*"     VALUE(I_CALLTIME) TYPE  AAPMT_CALLTIME
*"  CHANGING
*"     VALUE(C_ANKAZ_REC) LIKE  ANKAZ STRUCTURE  ANKAZ

tables: ztfiaa1, anka.
data :  wa_EQART like ztfiaa1-EQART.

clear c_ankaz_rec-ANLKL.

select single * from ztfiaa1
   where EQTYP = i_itob_rec-eqtyp
     and eqart = i_itob_rec-eqart.

if sy-subrc = 0.
  c_ankaz_rec-ANLKL = ztfiaa1-ANLKL.
  select single * from anka
  where ANLKL = ztfiaa1-ANLKL.
  if sy-subrc <> 0.
      message e006(zfi).
  endif.
else.
  concatenate i_itob_rec-eqart+0(2) i_itob_rec-eqtyp
              i_itob_rec-eqart+2(5) into wa_eqart.
  select single * from anka
  where ANLKL = wa_eqart.
  if sy-subrc = 0.
      message e006(zfi).
  endif.

endif.
