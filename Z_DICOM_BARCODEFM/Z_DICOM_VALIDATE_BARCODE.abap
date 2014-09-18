function z_dicom_validate_barcode.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(P_BARCODEIN) LIKE  BDS_BAR_IN-BARCODE
*"     REFERENCE(P_SKIP_BARCODE_CHECK) TYPE  CHAR1
*"  CHANGING
*"     REFERENCE(P_RETURN) TYPE  BAPIRETURN1
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"--------------------------------------------------------------------
  data:
        lv_bds_bar_in      type bds_bar_in,
        error_msg          like sy-msgv1.

  if p_barcodein is initial.

    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = 'Barcode is empty'.
    raise general_error.

  else.


    if p_skip_barcode_check <> 'X'.

      select single * from bds_bar_in into  lv_bds_bar_in where
             barcode = p_barcodein.

      if sy-subrc ne 0.
        concatenate 'Barcode' p_barcodein 'not found' into error_msg
                    separated by space.
        call function 'Z_DICOM_MSG_POPULATE'
          exporting
            error_msg = error_msg.
        raise general_error.
      endif.

    endif.


  endif.





endfunction.
