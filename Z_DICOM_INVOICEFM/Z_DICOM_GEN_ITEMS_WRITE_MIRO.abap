function z_dicom_gen_items_write_miro.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ARTICLENUMBER) TYPE  CHAR18 OPTIONAL
*"     VALUE(CONF_LI_MATCHING) TYPE  CHAR1 OPTIONAL
*"     VALUE(PO_ITEM_NO) TYPE  NUMC5 OPTIONAL
*"     VALUE(PURCHASEORDER) LIKE  BAPIEKKO-PO_NUMBER OPTIONAL
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM
*"      ITEM_NEW STRUCTURE  ZMIRO_ITEM
*"--------------------------------------------------------------------

  data:
      str_item_no type char5,
      temp_table   type zmiro_item occurs 0 with header line.

  str_item_no = po_item_no.
  shift str_item_no left deleting leading '0'.

  if conf_li_matching = '1'.
*       Check by line item article number
    read table item_data into temp_table with key
         item_po = purchaseorder item_material_no = articlenumber.
  else.
*       Check by line item number
    read table item_data into temp_table with key
         item_po = purchaseorder item_po_item_no = str_item_no.
  endif.

*   Append into list
  clear item_new.
  item_new-item_po = purchaseorder.
  item_new-item_material_no = articlenumber.
  item_new-item_amount = temp_table-item_amount.
  item_new-item_quantity = temp_table-item_quantity.
  item_new-item_po_item_no = po_item_no.
  append item_new.

endfunction.
