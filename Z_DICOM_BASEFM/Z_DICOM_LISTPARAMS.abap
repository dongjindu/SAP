function z_dicom_listparams.
*"----------------------------------------------------------------------
*"*"Global Interface:
*"  IMPORTING
*"     VALUE(FUNCNAME) LIKE  TFDIR-FUNCNAME
*"  TABLES
*"      PARAMS STRUCTURE  ZDCM_RFC_PARAM
*"      TABLE_PARAMS STRUCTURE  ZDCM_RFC_TABLE
*"  EXCEPTIONS
*"      FU_NOT_FOUND
*"      GENERAL_ERROR
*"----------------------------------------------------------------------
  refresh: if_import, if_export, if_tables, if_except, params,
             if_change.

  clear:   if_docu, if_import, if_export, if_tables, if_except, params,
           if_change.

  data: begin of params_tab occurs 1.
          include structure zdcm_rfc_param.
  data: end of params_tab.

  data: temp type char41.

* Get function details
  call function 'FUNCTION_IMPORT_DOKU'
    exporting
      funcname           = funcname
      language           = 'E'
    tables
      dokumentation      = if_docu
      exception_list     = if_except
      export_parameter   = if_export
      import_parameter   = if_import
      changing_parameter = if_change
      tables_parameter   = if_tables
    exceptions
      error_message      = 01
      invalid_name       = 03.
  if sy-subrc <> 0.
    raise fu_not_found.
  endif.

* Move funcname to func_name.
  perform move_if_import.
  perform move_if_export.
  perform move_if_change.
  perform move_if_tables.

* Perform move_if_except.
  loop at params where ztabname = 'SY  '.
    params-ztabname = 'SYST'.
    modify params.
  endloop.

* Get ddic-information on each field or structure
  perform ddic_info_get.

  loop at params.
    move-corresponding params to params_tab.
    append params_tab.
  endloop.

  clear table_params.
  refresh table_params.

  loop at if_tables.
*   Set the prefix to 'TABLE-'.
    clear table_params.
    concatenate 'TABLE-' if_tables-parameter '-' if_tables-dbstruct
      into temp.
    move temp to table_params.
    append table_params.
    perform get_columns tables table_params.
  endloop.

endfunction.


*---------------------------------------------------------------------*
*       FORM GET_COLUMNS                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TABLE_PARAMS                                                  *
*---------------------------------------------------------------------*

form get_columns tables table_params structure ztable_structure.

  data:
    lv_tbname  like rfc_funint-tabname,
    lv_dfies   type dfies,
    it_dfies   type dfies occurs 0 with header line.

  if not if_tables-dbstruct is initial.
    lv_tbname = if_tables-dbstruct.
  else.
    if not if_tables-typ is initial.
      lv_tbname = if_tables-typ.
    endif.
  endif.

  if not lv_tbname is initial.

    call function 'DDIF_FIELDINFO_GET'
      exporting
        tabname        = lv_tbname
        all_types      = 'X'
      importing
        dfies_wa       = lv_dfies
      tables
        dfies_tab      = it_dfies
      exceptions
        not_found      = 1
        internal_error = 2
        others         = 3.

    if sy-subrc eq 0.
      if it_dfies is initial.
        call function 'DDIF_FIELDINFO_GET'
          exporting
            tabname        = lv_dfies-rollname
            all_types      = 'X'
          importing
            dfies_wa       = lv_dfies
          tables
            dfies_tab      = it_dfies
          exceptions
            not_found      = 1
            internal_error = 2
            others         = 3.

        if sy-subrc eq 0.
          loop at it_dfies.
            clear table_params.
            table_params-zfieldname = it_dfies-fieldname.
            table_params-zexid      = it_dfies-inttype.
            table_params-zintlength = it_dfies-leng.
            append table_params.
          endloop.
        endif.

      else.
        loop at it_dfies.
          clear table_params.
          table_params-zfieldname = it_dfies-fieldname.
          table_params-zexid      = it_dfies-inttype.
          table_params-zintlength = it_dfies-leng.
          append table_params.
        endloop.
      endif.
    endif.

  endif.


endform.                    "GET_COLUMNS


**-- internal forms----------------------------------------------------*
**---------------------------------------------------------------------*
**       FORM MOVE_IF_IMPORT                                           *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
form move_if_import.

  data: l_struc type fupararef-structure.
  loop at if_import.
*    clear params.
    params-zparamclass = import.
    params-zparameter = if_import-parameter.
    perform docu_get using    params-zparameter params-zparamclass
                     changing params-zparamtext.
    params-zdefault = if_import-default.
    params-zoptional = if_import-optional.
    if if_import-dbfield is initial.
      l_struc = if_import-typ.
    else.
      l_struc = if_import-dbfield.
    endif.
    if l_struc ca '-'.
      assign l_struc(sy-fdpos) to <f>.
      params-ztabname   = <f>.
      position = sy-fdpos + 1.
      assign l_struc+position(*) to <f>.
      params-zfieldname = <f>.
      append params.
    else.
      params-ztabname = l_struc.
      params-zfieldname = ''.
      append params.
    endif.
  endloop.

endform.                    "MOVE_IF_IMPORT

**---------------------------------------------------------------------*
**       FORM MOVE_IF_CHANGE                                          *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
form move_if_change.

  data: l_struc type fupararef-structure.
  loop at if_change.
    clear params.
    params-zparamclass = change.
    params-zparameter = if_change-parameter.
    perform docu_get using    params-zparameter params-zparamclass
                     changing params-zparamtext.
    params-zdefault = if_change-default.
    params-zoptional = if_change-optional.
    if if_change-dbfield is initial.
      l_struc = if_change-typ.
    else.
      l_struc = if_change-dbfield.
    endif.
    if l_struc ca '-'.
      assign l_struc(sy-fdpos) to <f>.
      params-ztabname   = <f>.
      position = sy-fdpos + 1.
      assign l_struc+position(*) to <f>.
      params-zfieldname = <f>.
      append params.
    else.
      params-ztabname = l_struc.
      append params.
    endif.
  endloop.

endform.                    "MOVE_IF_CHANGE

**--------------------------------------------------------------------*
**       FORM MOVE_IF_EXPORT                                           *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
form move_if_export.
  data: l_struc type fupararef-structure.
  loop at if_export.
    clear params.
    params-zparamclass = export.
    params-zparameter = if_export-parameter.
    perform docu_get using    params-zparameter params-zparamclass
                     changing params-zparamtext.
    if if_export-dbfield is initial.
      l_struc = if_export-typ.
    else.
      l_struc = if_export-dbfield.
    endif.
    if l_struc ca '-'.
      assign l_struc(sy-fdpos) to <f>.
      params-ztabname   = <f>.
      position = sy-fdpos + 1.
      assign l_struc+position(*) to <f>.
      params-zfieldname = <f>.
      append params.
    else.
      params-ztabname = l_struc.
      append params.
    endif.
  endloop.

endform.                    "MOVE_IF_EXPORT

**---------------------------------------------------------------------*
**       FORM MOVE_IF_TABLES                                           *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
form move_if_tables.
  loop at if_tables.
    clear params.
    params-zparamclass = itab.
    params-zparameter = if_tables-parameter.
    params-zoptional = if_tables-optional.
    perform docu_get using    params-zparameter params-zparamclass
                     changing params-zparamtext.
    if if_tables-dbstruct is initial.
      params-ztabname = if_tables-typ.
    else.
      params-ztabname = if_tables-dbstruct.
    endif.
  endloop.
endform.                    "MOVE_IF_TABLES

**---------------------------------------------------------------------*
**       FORM MOVE_IF_EXCEPT                                           *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*form move_if_except.
*  loop at if_except.
*    clear params.
*    params-zparamclass = except.
*    params-zparameter = if_except-exception.
*    append params.
*  endloop.
*endform.                    "MOVE_IF_EXCEPT

**---------------------------------------------------------------------*
**       FORM DOCU_GET                                                 *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
**  -->  PARAMETER                                                     *
**  -->  PARAMCLASS                                                    *
**  -->  PARAMTEXT                                                     *
**---------------------------------------------------------------------*
form docu_get using    parameter  like funct-parameter
                       paramclass like funct-kind
              changing paramtext  like funct-stext.
  data kind.
  if paramclass = except.
    kind    = 'X'.
  else.
    kind    = 'P'.
  endif.
  read table if_docu with key
    spras     = 'E'
    funcname  = funcname
    parameter = parameter
    kind      = kind.
  paramtext = if_docu-stext.
endform.                    "DOCU_GET

**---------------------------------------------------------------------*
**       FORM DDIC_INFO_GET                                            *
**---------------------------------------------------------------------*
**       get ddic-info on each each im/export and itab field           *
**---------------------------------------------------------------------*
form ddic_info_get.
  data: tabname_save like params-ztabname.

  sort params by ztabname.
  loop at params where ztabname <> space.
    if params-ztabname <> tabname_save.
      tabname_save = params-ztabname.
      perform nametab_get using params-ztabname.
    endif.

    if params-zfieldname = space.
      perform move_header.
    else.
      perform move_nametab.
    endif.

*    PERFORM MOVE_HEADER.
    modify params.
  endloop.
  sort params by zparamclass zparameter.

endform.                    "DDIC_INFO_GET

**---------------------------------------------------------------------*
**       FORM NAMETAB_GET                                              *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
form nametab_get using tabname like rfc_funint-tabname.

  data: l_dfies type dfies occurs 0 with header line.

*  DATA: L_DFIES TYPE DFIES OCCURS 0 WITH HEADER LINE,
*        TYPE_LIST_TAB TYPE SANA_TYPE_LIST,
*        TYPE_LIST TYPE SANA_TYPE_STRUCT.
  constants: deep_struc type x value '02',
             hier_struc type x value '01',
             flat_struc type x value '00'.
  call function 'DDIF_FIELDINFO_GET'
    exporting
      tabname        = tabname
      all_types      = 'X'
    importing
      x030l_wa       = header
      dfies_wa       = g_dfies
    tables
      dfies_tab      = l_dfies
    exceptions
      not_found      = 1
      internal_error = 2
      others         = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    raise general_error.
  endif.

  loop at l_dfies.
    move-corresponding l_dfies to g_nametab.
    g_nametab-leng      = l_dfies-leng.
    g_nametab-outputlen = l_dfies-outputlen.
    g_nametab-inttype   = l_dfies-inttype.
    g_nametab-offset    = l_dfies-offset.
    g_nametab-decimals    = l_dfies-decimals.
    append g_nametab.
  endloop.

  clear inttype.

  if not header is initial.
    if header-flag3 o deep_struc
       or header-flag3 o hier_struc.
      inttype = 'v'.
    elseif header-flag3 o flat_struc.
      inttype = 'u'.
      if header-tabtype = 'L'.
        inttype = 'h'.
*       In Case of TABLE-TYPEs, both may be filled
        g_dfies-inttype = inttype.
      endif.
    endif.
  endif.
endform.                    "NAMETAB_GET

**---------------------------------------------------------------------*
**       FORM MOVE_HEADER                                              *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
form move_header.

  if g_dfies is initial.
*   Komplexer Typ
    params-zposition  = 0.
    params-zintlength = header-tablen.
    params-zoffset    = 0.
    params-zexid      = inttype.
    params-zdecimals  = 0.
  else.
*   Elementarer Typ
    params-zposition  = 0.
    params-zintlength = g_dfies-leng.
    params-zoffset    = 0.
    if g_dfies-inttype is initial.
      params-zexid    = inttype.
    else.
      params-zexid    = g_dfies-inttype.
    endif.
    params-zdecimals  = g_dfies-decimals.
  endif.

endform.                    "MOVE_HEADER


**---------------------------------------------------------------------*
**       FORM MOVE_NAMETAB                                             *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
form move_nametab.

  loop at g_nametab where fieldname = params-zfieldname.
    params-zexid  = g_nametab-inttype.
    params-zintlength = g_nametab-leng.
    params-zdecimals = g_nametab-decimals.
    params-zposition  = 0.
    params-zoffset    = 0.
  endloop.

endform.                    "MOVE_NAMETAB
