function z_dicom_doc2bdcdata.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      DOCDATA STRUCTURE  OARFCDATA OPTIONAL
*"      BDCDATA STRUCTURE  ZBDCDATA OPTIONAL
*"--------------------------------------------------------------------
* Initialise
  clear bdcdata.

* Create entry in BDCData table
  loop at docdata.
*     Populate field name
    if docdata-name = zcontrol_element.
      bdcdata-zprogram = ''.
    else.
      bdcdata-zprogram = docdata-name.
    endif.
*     Split field value
    split docdata-wert at '|' into bdcdata-zdynpro bdcdata-zdynbegin
bdcdata-zfnam bdcdata-zfval.
*     Append new row
    append bdcdata.
  endloop.





endfunction.
