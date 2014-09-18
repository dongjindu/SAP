function z_dicom_ponumbers_replace.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      PO_NUMBERS_HEADER STRUCTURE  ZPONUMBER OPTIONAL
*"      PO_NUMBERS_LINE STRUCTURE  ZPONUMBER OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

* Delete table content
  free po_numbers_header.

* Copy PO number(s) from line item table to header table
  loop at po_numbers_line.
    clear po_numbers_header.
    po_numbers_header-po_number = po_numbers_line-po_number.
    append po_numbers_header.
  endloop.

endfunction.
