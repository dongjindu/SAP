*----------------------------------------------------------------------*
*   INCLUDE ZXTXWU01                                                   *
*----------------------------------------------------------------------*

** Function exit interface:
** import: EXPORT_STRUCTURE - name of export structure
** changing: EXPORT_RECORD - data record
** in: all standard fields are filled and those customer
** fields that have the same name as a source table field
** out: in addition, customer fields are filled
* Table declarations (only list tables where fields are needed below).
* - Note that table work areas are global; the source table work areas
* are always filled from the calling program and can be used in
* this exit function.
* (declare here or, better, in global data; see CMOD help)
tables: bkpf, "source table for TXW_FI_HD
bseg, "source table for TXW_HD_POS
anep. "source table for TXW_AS_POS
* work areas
data: wa_txw_fi_hd like txw_fi_hd, "FI document header
wa_txw_fi_pos like txw_fi_pos, "FI document position
wa_txw_as_pos like txw_as_pos. "AM Asset document position

data: currency_unit like tcurc-waers.

case export_structure.
* add WHEN block for each structure where customer specific fields
* need to filled (to use FORM routines, see CMOD help)
  when 'TXW_FI_POS'. "FI document line
    WA_TXW_FI_POS = export_record. "copy record to local structure

*    WA_TXW_FI_POS-FISTL = bseg-fistl. "FC
*    WA_TXW_FI_POS-GEBER = bseg-GEBER. "fund
*    WA_TXW_FI_POS-FIPOS = bseg-fipos. "CI

    if WA_TXW_FI_POS-fistl = 'BS'. WA_TXW_FI_POS-fistl = 'HMMA'. endif.

*    if wa_txw_fi_pos-kostl is initial and wa_txw_fi_pos-aufnr <> space.
*       select single AKSTL into wa_txw_fi_pos-kostl
*          from aufk where aufnr = wa_txw_fi_pos-aufnr.
*    endif.

    export_record = WA_TXW_FI_POS. "copy structure back to record


endcase.
