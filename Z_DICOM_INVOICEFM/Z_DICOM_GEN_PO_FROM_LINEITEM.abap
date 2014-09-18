function z_dicom_gen_po_from_lineitem.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      PO_NUMBERS STRUCTURE  ZPONUMBER OPTIONAL
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    item_found type i.

  loop at item_data.
    if item_data-item_po ne space.
      item_found = 0.
      loop at po_numbers.
        if item_data-item_po = po_numbers-po_number.
          item_found = 1.
          exit.
        endif.
      endloop.
      if item_found = 0.
        clear po_numbers.
        po_numbers-po_number = item_data-item_po.
        append po_numbers.
      endif.
    endif.
  endloop.

endfunction.
