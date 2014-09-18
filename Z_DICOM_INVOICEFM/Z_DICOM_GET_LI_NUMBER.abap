function z_dicom_get_li_number.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ARTICLENUMBER) TYPE  CHAR18 OPTIONAL
*"  EXPORTING
*"     VALUE(POLINEITEM) TYPE  I
*"  TABLES
*"      PONUMBER STRUCTURE  ZPONUMBER OPTIONAL
*"  EXCEPTIONS
*"      MORE_THAN_ONE_ITEMNO
*"----------------------------------------------------------------------

  data:    counter type i,
           total_line_items type i,
           counter_index type i,
           tbl_all type zponumber occurs 1 with header line,
           tbl_line_items type bapiekpo occurs 1 with header line.

  clear tbl_all.

* Loop at each PO number in tables to get it's line items
  loop at ponumber.
    clear tbl_line_items.
    call function 'BAPI_PO_GETDETAIL'
      exporting
        purchaseorder = ponumber-po_number
        items         = 'X'
      tables
        po_items      = tbl_line_items.

*     Append line item into the overall list
    loop at tbl_line_items.
      tbl_all-po_number = tbl_line_items-material.
      append tbl_all.
    endloop.
  endloop.

* Validation - Material number cannot appear more than one
  total_line_items = 0.
  loop at tbl_all.
    if tbl_all-po_number = articlenumber.
      total_line_items = total_line_items + 1.
    endif.
  endloop.

*  IF total_line_items > 1 Then error
  if total_line_items > 1.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = 'More than one material number found'.
    raise more_than_one_itemno.
  endif.

* Find the position of material number
  counter = 0.

  loop at tbl_all.
    counter = counter + 1.
    if tbl_all-po_number = articlenumber.
      counter_index = counter.
    endif.
  endloop.

  polineitem = counter_index.

endfunction.
