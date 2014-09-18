function z_dicom_execute_bdc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CONF_ARCHIV_ID) TYPE  CHAR2 OPTIONAL
*"     VALUE(CONF_DOC_TYPE) TYPE  CHAR20 OPTIONAL
*"     VALUE(CONF_AR_OBJECT) TYPE  CHAR10
*"     VALUE(CONF_DOC_AVAILABLE) TYPE  CHAR1 OPTIONAL
*"     VALUE(ARCHIV_DOC_ID) TYPE  CHAR40 OPTIONAL
*"     VALUE(TRANSACTION_CODE) TYPE  CHAR4
*"     VALUE(SAP_OBJECTID_CONSTRUCTOR) TYPE  CHAR255
*"  TABLES
*"      BDCDATA STRUCTURE  ZBDCDATA OPTIONAL
*"      Z_KEYVALUE STRUCTURE  ZDICOM_KV OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"      TRANSACTION_EXECUTE_ERROR
*"      ARCHIVELINK_ERROR
*"----------------------------------------------------------------------

  if conf_doc_available = 'X'.

*  With image, check archive link settings
    data lv_sap_object like toaom-sap_object.
    call function 'Z_DICOM_VD_ARCHIVELINK_INFO'
      exporting
        conf_archiv_id = conf_archiv_id
        conf_ar_object = conf_ar_object
        conf_doc_type  = conf_doc_type
      importing
        sap_object     = lv_sap_object
      exceptions
        general_error  = 1
        others         = 2.

    if sy-subrc <> 0.
      call function 'Z_DICOM_MSG_POPULATE'
        exporting
          error_msg = ''.
      raise general_error.
    endif.
  endif.

* set the parameters (SPA/GPA parameters)
  data parameter_id type char20.
  loop at z_keyvalue.
    parameter_id = z_keyvalue-key.
    set parameter id parameter_id field z_keyvalue-value.
  endloop.


*  Call Transaction.
   data it_msg type table of bdcmsgcoll with header line.
   call transaction transaction_code using bdcdata messages into it_msg
        mode 'N'.

   if sy-subrc <> 0.
     call function 'Z_DICOM_MSG_POPULATE'
       exporting
         error_msg = ''.
     raise transaction_execute_error.
   else.

     if conf_doc_available = 'X'.
*       generate the sap object id for archivelink linking.
        data: begin of parts, part type char20, end of parts.
        data it_parts like parts occurs 0 with header line.
        data object_id_part type char50 value ''.
        data object_id like sapb-sapobjid value ''.

       split sap_objectid_constructor at '+' into table it_parts.
       loop at it_parts.
         parameter_id = it_parts-part.
         get parameter id parameter_id field object_id_part.
         concatenate object_id object_id_part into object_id.
       endloop.

       if archiv_doc_id ne space.
         call function 'ARCHIV_CONNECTION_INSERT'
           exporting
             archiv_id             = conf_archiv_id
             arc_doc_id            = archiv_doc_id
             ar_object             = conf_ar_object
             object_id             = object_id
             sap_object            = lv_sap_object
             doc_type              = conf_doc_type
           exceptions
             error_connectiontable = 1
             others                = 2.

          if sy-subrc <> 0.
            call function 'Z_DICOM_MSG_POPULATE'
              exporting
                error_msg = 'ERROR_CONNECTIONTABLE'.
            raise archivelink_error.
          endif.
      endif.
    endif.
  endif.

endfunction.
