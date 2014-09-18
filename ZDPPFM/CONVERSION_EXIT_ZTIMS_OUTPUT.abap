FUNCTION conversion_exit_ztims_output.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  CHAR23
*"  EXCEPTIONS
*"      ILLEGAL_SIZE
*"----------------------------------------------------------------------

  DATA: l_char              TYPE c,
        l_len               TYPE i,
        l_date(10)          TYPE c,
        l_sdat              TYPE d,
        l_time(08)          TYPE c,
        l_stim              TYPE t.

  l_len = strlen( input ) .
  CASE l_len.
    WHEN 14 .
      l_sdat = input(8) .            l_stim = input+8(6)     .
      WRITE l_sdat  TO  l_date .     WRITE l_stim  TO  l_time.
      CONCATENATE l_date l_time      INTO  output  SEPARATED BY space.
    WHEN 15 .
      l_sdat = input(8) .            l_stim = input+8(6)     .
      WRITE l_sdat  TO  l_date .     WRITE l_stim  TO  l_time.
      CONCATENATE l_date l_time      INTO  output  SEPARATED BY space.
      l_char = input+14(1) .
      TRANSLATE l_char TO UPPER CASE.
      CASE l_char.
        WHEN 'A' .
          CONCATENATE output 'ACT'   INTO output  SEPARATED BY space.
        WHEN 'P' .
          CONCATENATE output 'PLN'   INTO output  SEPARATED BY space.
      ENDCASE.
    WHEN OTHERS.
*     RAISE illegal_size .
  ENDCASE.
ENDFUNCTION.
