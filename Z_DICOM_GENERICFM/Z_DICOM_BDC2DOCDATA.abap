function z_dicom_bdc2docdata.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      BDCDATA STRUCTURE  ZBDCDATA OPTIONAL
*"      DOCDATA STRUCTURE  OARFCDATA OPTIONAL
*"--------------------------------------------------------------------
* Initialise
  clear docdata.

* Create entry in DocumentData table
  loop at bdcdata.
*     Populate field name
    if bdcdata-zprogram = ' '.
      docdata-name = zcontrol_element.
    else.
      docdata-name = bdcdata-zprogram.
    endif.
*     Populate field value
    concatenate bdcdata-zdynpro bdcdata-zdynbegin bdcdata-zfnam
    bdcdata-zfval into docdata-wert separated by '|'.

*     Append new row
    append docdata.
  endloop.





endfunction.
