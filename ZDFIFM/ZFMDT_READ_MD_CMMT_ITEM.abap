FUNCTION ZFMDT_READ_MD_CMMT_ITEM .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  FMDT_FMCI_KEY
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  FMCI
*"  EXCEPTIONS
*"      NOT_FOUND
*"      DO_NOT_STOP
*"--------------------------------------------------------------------

  DATA: l_year         LIKE t009b-bdatj.

*--- Assign Fiscal year
  l_year = input-fisc_year.

*--- Check source fields are not initial.
  IF   input-fm_area   IS INITIAL
    OR input-CMMT_ITEM IS INITIAL.
     RAISE DO_NOT_STOP.
  ENDIF.

*--- Get Year from Posting Date
  IF NOT input-PSTNG_DATE IS INITIAL
     AND input-fisc_year  is initial.
    CALL FUNCTION 'FM_GET_YEAR_FROM_DATE'                   "#EC *
      EXPORTING
        i_farea             = input-fm_area
        i_date              = input-PSTNG_DATE
      IMPORTING
        e_year              = l_year
      EXCEPTIONS
        applc_not_supported = 1
        no_periv            = 2.
  ENDIF.

*--- No Date/Year Provided.
  IF     input-PSTNG_DATE IS INITIAL
     AND input-fisc_year  IS INITIAL.
    l_year = '0000'.
  ENDIF.

*--- Get CI master Data
  CALL FUNCTION 'FM_COM_ITEM_READ'                          "#EC *
    EXPORTING
      i_fikrs               = input-fm_area
      i_gjahr               = l_year
      i_fipex               = input-CMMT_ITEM
    IMPORTING
      e_f_fmci              = output
    EXCEPTIONS
      master_data_not_found = 1
      input_error           = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
    CLEAR output.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
    RAISING NOT_FOUND.
  ENDIF.

ENDFUNCTION.
