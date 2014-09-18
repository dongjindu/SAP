function z_dicom_ponumbers_fill_item.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      PO_NUMBERS STRUCTURE  ZPONUMBER OPTIONAL
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
  data:
    counter_po type i,
    error_msg  like sy-msgv1,
    item_temp  like zmiro_item.

* Raise error is multiple POs with line item without PO number specified
  describe table po_numbers lines counter_po.
  if counter_po > 1.
    error_msg = 'Multiple POs, but line item without PO number'.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = error_msg.
    raise general_error.
  endif.

  if counter_po = 1.
*   Fill line item without PO, with PO number from POTable
    item_temp-item_po = po_numbers-po_number.
    modify item_data from item_temp transporting item_po
           where ( item_po eq space ).
  endif.

endfunction.
