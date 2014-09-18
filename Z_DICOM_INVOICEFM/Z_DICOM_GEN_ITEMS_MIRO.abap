function z_dicom_gen_items_miro.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_LI_MATCHING) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(STATUS) LIKE  SY-MSGV1
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM
*"      ITEM_NEW STRUCTURE  ZMIRO_ITEM
*"      PO_NUMBERS STRUCTURE  ZPONUMBER
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------

  data:
    counter_sap type i,
    counter_ascent type i,
    tbl_line_items type bapiekpo occurs 1 with header line.

* Initialise
  counter_sap = 0.
  counter_ascent = 0.
  clear item_new.

* Loop at each PO number in tables to get it's line items
  loop at po_numbers.
    clear tbl_line_items.
    call function 'BAPI_PO_GETDETAIL'
      exporting
        purchaseorder = po_numbers-po_number
        items         = 'X'
      tables
        po_items      = tbl_line_items.

*     Append line item into the new list
    loop at tbl_line_items.
      counter_sap = counter_sap + 1.
      call function 'Z_DICOM_GEN_ITEMS_WRITE_MIRO'
        exporting
          articlenumber    = tbl_line_items-material
          conf_li_matching = conf_li_matching
          po_item_no       = tbl_line_items-po_item
          purchaseorder    = po_numbers-po_number
        tables
          item_data        = item_data
          item_new         = item_new.
    endloop.
  endloop.

* Get total line items in Ascent
  describe table item_data lines counter_ascent.

* Validation - Line items from Ascent cannot more than records in SAP
  if counter_ascent > counter_sap.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = 'Line items in Ascent more than in SAP'.
      raise general_error.
  endif.

endfunction.
