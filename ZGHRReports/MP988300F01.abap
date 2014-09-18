*&---------------------------------------------------------------------*
*&  Include           MP988300F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  OPEN_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_FILE .


  DATA:
          ls_wwwdata_item   LIKE wwwdatatab,
          l_temp_file             LIKE w3file-name VALUE '~wwwtmp',
          l_filename              LIKE w3file-name,
          l_document            TYPE string.



  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_wwwdata_item
    FROM wwwdata
   WHERE objid = 'ZGHRIS_GRADE'.

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key  = ls_wwwdata_item
    CHANGING
      temp = l_temp_file.

  l_filename = l_temp_file.
  SHIFT l_temp_file RIGHT.
  l_temp_file(1) = '"'.
  CONCATENATE l_temp_file '"' INTO l_temp_file.

  l_document = l_temp_file.

  cl_gui_frontend_services=>execute(
  EXPORTING
    document = l_document
  ).



ENDFORM.                    " OPEN_FILE
