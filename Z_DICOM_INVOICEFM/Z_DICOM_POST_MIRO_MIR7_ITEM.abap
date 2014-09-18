function z_dicom_post_miro_mir7_item.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BSC_CURRENCY) TYPE  CHAR5 OPTIONAL
*"  TABLES
*"      AC_ITEM_DATA STRUCTURE  ZMIRO_ITEM OPTIONAL
*"      BAPI_ITEM_DATA STRUCTURE  BAPI_INCINV_CREATE_ITEM OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
*{   INSERT         TS2K900130                                        1

  data:
    counter type i,
    decimal_temp type bapicurr-bapicurr,
    internal_amount type char13.

  counter = 0.

  if not ac_item_data[] is initial.
    loop at ac_item_data.
*     Initialise
      counter = counter + 1.
      clear bapi_item_data.

*     Property without formatting
      bapi_item_data-invoice_doc_item = counter.
      bapi_item_data-po_number = ac_item_data-item_po.
      bapi_item_data-po_item = ac_item_data-item_po_item_no.
      bapi_item_data-tax_code = ac_item_data-item_tax_code.
      bapi_item_data-po_unit = ac_item_data-item_measure_unit.

*     Item Amount - translate to internal amount format
      translate ac_item_data-item_amount using ',.'.
      decimal_temp = ac_item_data-item_amount .
      call function 'BAPI_CURRENCY_CONV_TO_INTERNAL'
        exporting
          currency             = bsc_currency
          amount_external      = decimal_temp
          max_number_of_digits = 13
        importing
          amount_internal      = internal_amount.
      bapi_item_data-item_amount = internal_amount.

*     Item Amount - translate to internal amount format
      translate ac_item_data-item_quantity using ',.'.
      decimal_temp = ac_item_data-item_quantity .
      call function 'BAPI_CURRENCY_CONV_TO_INTERNAL'
        exporting
          currency             = bsc_currency
          amount_external      = decimal_temp
          max_number_of_digits = 13
        importing
          amount_internal      = internal_amount.
      bapi_item_data-quantity = internal_amount.

*     Append line item
      append bapi_item_data.
    endloop.
  endif.

*}   INSERT





endfunction.
