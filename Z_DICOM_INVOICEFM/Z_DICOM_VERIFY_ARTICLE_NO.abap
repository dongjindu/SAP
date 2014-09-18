function z_dicom_verify_article_no.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(STATUS) TYPE  I
*"  TABLES
*"      ITEM_DATA STRUCTURE  ZMIRO_ITEM
*"      PO_NUMBERS STRUCTURE  ZPONUMBER
*"----------------------------------------------------------------------

  data:  counter type i,
         result type i.

* Initialise
  counter = 0.

  loop at item_data.
*   Reset counters
    result = 0.
    counter = counter + 1.

*   Check line item position
    call function 'Z_DICOM_GET_LI_NUMBER'
      exporting
        articlenumber        = item_data-item_material_no
      importing
        polineitem           = result
      tables
        ponumber             = po_numbers
      exceptions
        more_than_one_itemno = 1
        others               = 2.

    if sy-subrc <> 0.
*     If error, exit directly
      status = 0.
      exit.
    else.
      if counter = result.
        status = 1.
      else.
        status = 0.
        exit.
      endif.
    endif.
  endloop.

endfunction.
