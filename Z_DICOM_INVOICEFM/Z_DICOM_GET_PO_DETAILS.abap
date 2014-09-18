function z_dicom_get_po_details.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PONUMBER) TYPE  CHAR10
*"  EXPORTING
*"     VALUE(VENDOR) TYPE  CHAR10
*"     VALUE(SUPPL_VEND) TYPE  CHAR10
*"     VALUE(DOC_DATE) TYPE  CHAR8
*"     VALUE(CURRENCY) TYPE  CHAR5
*"     VALUE(STATUS) TYPE  CHAR1
*"  TABLES
*"      PO_LINEITEMS STRUCTURE  ZPO_LINEINFO OPTIONAL
*"  EXCEPTIONS
*"      PO_NOT_FOUND
*"--------------------------------------------------------------------
*{   INSERT         TS2K900062                                        1

  data  :
          error_msg   like sy-msgv1,
          return      type bapireturn occurs 0 with header line,
          tbl_items   type bapiekpo occurs 1 with header line,
          tbl_header  type bapiekkol occurs 1 with header line.

  call function 'BAPI_PO_GETDETAIL'
    exporting
      purchaseorder = ponumber
    importing
      po_header     = tbl_header
    tables
      po_items      = tbl_items
      return        = return.

  if not return is initial.
    loop at return.
      error_msg = return-message.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = error_msg.
      raise po_not_found.
    endloop.
  endif.

  move tbl_header-vendor to vendor.
  move tbl_header-suppl_vend to suppl_vend.
  move tbl_header-doc_date to doc_date.
  move tbl_header-currency to currency.
  move tbl_header-status to status.

  clear po_lineitems.
  loop at tbl_items.
    po_lineitems-po_item = tbl_items-po_item.
    po_lineitems-quantity = tbl_items-quantity.
    po_lineitems-material = tbl_items-material.
    po_lineitems-short_text = tbl_items-short_text.
    po_lineitems-net_price = tbl_items-net_price.
    append po_lineitems.
  endloop.

*}   INSERT





endfunction.
