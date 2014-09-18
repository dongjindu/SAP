function ZDOC_QA_LVC_FCAT_MERGE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_STRUCTURE_NAME) LIKE  DD02L-TABNAME OPTIONAL
*"  CHANGING
*"     VALUE(CT_FIELDCAT) TYPE  LVC_T_FCAT
*"--------------------------------------------------------------------


  data : ls_fieldcat type lvc_s_fcat.
  data : l_tabix     type sy-tabix.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = i_structure_name
    changing
      ct_fieldcat            = ct_fieldcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  loop at  ct_fieldcat    into  ls_fieldcat.
    l_tabix = sy-tabix.


    if  ls_fieldcat-scrtext_l  is initial.
      select single  ddtext
        into ls_fieldcat-scrtext_l
        from dd03t
       where  tabname    =  i_structure_name
         and  ddlanguage = 'E'
         and  as4local   = 'A'
         and  fieldname  = ls_fieldcat-fieldname.

      ls_fieldcat-scrtext_s = ls_fieldcat-scrtext_m =
            ls_fieldcat-scrtext_l.

      modify ct_fieldcat from ls_fieldcat index l_tabix.
    endif.
  endloop.




endfunction.
