function z_dicom_workitem_noimage_gen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TASK_ID) LIKE  SWWWIHEAD-WI_RH_TASK
*"     VALUE(TRANSACTIONCODE) TYPE  CHAR4
*"  TABLES
*"      DOCUMENT_DATA STRUCTURE  OARFCDATA OPTIONAL
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"----------------------------------------------------------------------
  data:
      tmp_elem_tab   type standard   table of char255,
      it_swr_cont    type swcont     occurs 0 with header line,
      it_swwcompevt  type swwcompevt occurs 0 with header line,
      it_hri1212     type hri1212    occurs 0 with header line,
      lv_workitem_id like swwwihead-wi_id,
      lv_creator     like swwwihead-wi_creator.

  include <cntn01>.

  lv_creator = sy-uname.

* Preparing data
  swc_set_element it_swr_cont 'TransactionCode' transactioncode.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

* Convert document data into Container.
  clear document_data.
  loop at document_data.
    append document_data-name to tmp_elem_tab.
    append document_data-wert to tmp_elem_tab.
  endloop.

  swc_set_table it_swr_cont zcontain_name tmp_elem_tab.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.


  clear it_hri1212.
  refresh it_hri1212.
  call function 'RH_GET_TASK_TERM_EVENTS'
    exporting
      act_task        = task_id
    tables
      act_events      = it_hri1212
    exceptions
      no_active_plvar = 1
      no_events_found = 2
      others          = 3.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.

  loop at it_hri1212.
*   Set the Terminating Event.
    clear it_swwcompevt.
    it_swwcompevt-objtype = it_hri1212-objtyp.
    it_swwcompevt-event = it_hri1212-event.
    it_swwcompevt-contobjnam = it_hri1212-ev_element.
    append it_swwcompevt.
  endloop.

* Create Work Item.
  call function 'SWW_WI_CREATE'
    exporting
      creator        = lv_creator
      task           = task_id
      workitem_type  = 'W'
    importing
      wi_id          = lv_workitem_id
    tables
      wi_container   = it_swr_cont
      comp_events    = it_swwcompevt
    exceptions
      id_not_created = 1
      read_failed    = 2
      others         = 3.

  if sy-subrc <> 0.
    call function 'Z_DICOM_MSG_POPULATE'
      exporting
        error_msg = ''.
    raise general_error.
  endif.





endfunction.
