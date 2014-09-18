FUNCTION Z_FPP_CONDITION.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_COND) LIKE  ZSPP_CONDITION-VALS
*"     REFERENCE(I_CHECK) TYPE  ZFLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(O_SUCCESS) TYPE  ZFLAG
*"     REFERENCE(O_FAIL) TYPE  ZFLAG
*"  TABLES
*"      T_WORDER STRUCTURE  ZSPP_MASTER_ASUP
*"----------------------------------------------------------------------
  data: lt_cond             like table of it_cals      with header line.

  CALL FUNCTION 'Z_FPP_COMPILE_CONDITION'
    EXPORTING
      i_cond                = i_cond
      I_CHECK               = I_CHECK
    IMPORTING
      O_SUCCESS             = O_SUCCESS
      O_FAIL                = O_FAIL
    tables
      t_worder              = T_WORDER
      t_cond                = lt_cond
    EXCEPTIONS
      CONDITION_ERROR       = 1
      ERR_PAREN             = 2
      ERR_OPERATION         = 3
      ERR_RELATION          = 4
      ERR_VALUES            = 5
      ERR_FIELDS            = 6
      OTHERS                = 7 .

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFUNCTION.
