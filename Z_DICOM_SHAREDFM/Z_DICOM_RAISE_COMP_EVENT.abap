function z_dicom_raise_comp_event.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SAP_OBJECT) TYPE  SAEANWDID
*"     VALUE(SAP_OBJECTID) TYPE  SAEOBJID
*"     VALUE(COMP_EVENT) LIKE  SWETYPECOU-EVENT
*"  EXCEPTIONS
*"      OBJTYPE_NOT_FOUND
*"--------------------------------------------------------------------
  include <cntain>.
  swc_container container_instanz.

  data: wf_objkey like sweinstcou-objkey.

  swc_clear_container container_instanz.

  wf_objkey   = sap_objectid.

  call function 'SWE_EVENT_CREATE_FOR_UPD_TASK'
    exporting
      event             = comp_event
      objtype           = sap_object
      objkey            = wf_objkey
    tables
      event_container   = container_instanz
    exceptions
      objtype_not_found = 1.

  if sy-subrc ne 0.
    rollback work.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise objtype_not_found.
  else.
    commit work.
  endif.





endfunction.
