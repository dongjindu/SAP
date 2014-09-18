FUNCTION ZH_HR_STRUC_GET.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ACT_OTYPE) LIKE  OBJEC-OTYPE
*"     VALUE(ACT_OBJID) TYPE  HROBJID
*"     VALUE(ACT_WEGID) LIKE  GDSTR-WEGID
*"     VALUE(ACT_INT_FLAG) LIKE  HRRHAS-77AW_INT OPTIONAL
*"     VALUE(ACT_PLVAR) LIKE  OBJEC-PLVAR DEFAULT SPACE
*"     VALUE(ACT_BEGDA) LIKE  OBJEC-BEGDA DEFAULT SY-DATUM
*"     VALUE(ACT_ENDDA) LIKE  OBJEC-ENDDA DEFAULT SY-DATUM
*"     VALUE(ACT_TDEPTH) LIKE  HRRHAS-TDEPTH DEFAULT 0
*"     VALUE(ACT_TFLAG) LIKE  HRRHAS-TFLAG DEFAULT 'X'
*"     VALUE(ACT_VFLAG) LIKE  HRRHAS-VFLAG DEFAULT 'X'
*"     VALUE(AUTHORITY_CHECK) LIKE  HRRHAS-AUTHY DEFAULT 'X'
*"     VALUE(TEXT_BUFFER_FILL) LIKE  HRPP0C-TEST OPTIONAL
*"     VALUE(BUFFER_MODE) TYPE  FLAG OPTIONAL
*"  TABLES
*"      RESULT_TAB STRUCTURE  SWHACTOR OPTIONAL
*"      RESULT_OBJEC STRUCTURE  OBJEC OPTIONAL
*"      RESULT_STRUC STRUCTURE  STRUC OPTIONAL
*"  EXCEPTIONS
*"      NO_PLVAR_FOUND
*"      NO_ENTRY_FOUND
*"----------------------------------------------------------------------

  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype        = act_otype
      act_objid        = act_objid
      act_wegid        = act_wegid
      act_int_flag     = act_int_flag
      act_plvar        = act_plvar
      act_begda        = act_begda
      act_endda        = act_endda
      act_tdepth       = act_tdepth
      act_tflag        = act_tflag
      act_vflag        = act_vflag
      authority_check  = authority_check
      text_buffer_fill = text_buffer_fill
      buffer_mode      = buffer_mode
    IMPORTING
      act_plvar        = act_plvar
    TABLES
      result_tab       = result_tab
      result_objec     = result_objec
      result_struc     = result_struc
    EXCEPTIONS
      no_plvar_found   = 1
      no_entry_found   = 2
      OTHERS           = 3.

  CASE sy-subrc.
    WHEN 0.
    WHEN 1. RAISE no_plvar_found.
    WHEN 2. RAISE no_entry_found.
    WHEN 3.
  ENDCASE.


ENDFUNCTION.
