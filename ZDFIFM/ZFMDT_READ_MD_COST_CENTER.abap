FUNCTION ZFMDT_READ_MD_COST_CENTER.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  FMDT_CSKS_KEY
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CSKSV
*"  EXCEPTIONS
*"      NOT_FOUND
*"      DO_NOT_STOP
*"--------------------------------------------------------------------

*--- Skip rule when source fields are initial.
  IF   input-co_area    IS INITIAL
    OR input-costcenter IS INITIAL.
     RAISE DO_NOT_STOP.
  ENDIF.

*--- Read Cost Center Master Data
  CALL FUNCTION 'RK_KOSTL_READ'   "#EC *
    EXPORTING
      datum              = input-pstng_date
      kokrs              = input-co_area
      kostl              = input-costcenter
    IMPORTING
      xcsksv             = output
    EXCEPTIONS
      kostl_not_complete = 1
      kostl_not_found    = 2
      text_not_found     = 3
      kokrs_missing      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    CLEAR output.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
    RAISING NOT_FOUND.
  ENDIF.

ENDFUNCTION.
