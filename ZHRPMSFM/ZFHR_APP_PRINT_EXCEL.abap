FUNCTION zfhr_app_print_excel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_DOCUMENT) TYPE  HAP_S_DOCUMENT OPTIONAL
*"     REFERENCE(I_PTYPE) TYPE  CHAR01 OPTIONAL
*"----------------------------------------------------------------------


  CHECK is_document IS NOT INITIAL.

  PERFORM excel_print.
*  g_object = 'YHRPMSTEST01'.
*  g_title  = 'PMS Test'.
*  PERFORM init_factory.
*  PERFORM open_doc.
*
*  IF i_ptype IS NOT INITIAL.
*    g_flag = '1'.
*  ELSE.
*    g_flag = '2'.
*  ENDIF.
*  PERFORM macro USING g_flag. "immediately print(1), preview print(2)
*
*  PERFORM close_doc.
*  PERFORM close_factory.

ENDFUNCTION.
