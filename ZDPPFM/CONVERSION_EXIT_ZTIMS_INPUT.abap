FUNCTION CONVERSION_EXIT_ZTIMS_INPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  CHAR15
*"  EXCEPTIONS
*"      ILLEGAL_SIZE
*"----------------------------------------------------------------------

  DATA: l_cinput(23)        type c,
        l_char(3)           TYPE c,
        l_len               TYPE i,
        l_date(10)          TYPE c,
        l_sdat              TYPE d,
        l_time(08)          TYPE c,
        l_stim              TYPE t.

  l_cinput = input .
  condense l_cinput no-gaps .
  l_len = strlen( l_cinput ).
  CASE l_len.
    WHEN 21 OR 19 .
      l_date = l_cinput(10).         l_time = l_cinput+10(8)     .
      CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
        EXPORTING
          input         = L_DATE
        IMPORTING
          OUTPUT        = L_SDAT .

      CALL FUNCTION 'CONVERSION_EXIT_TIMLO_INPUT'
        EXPORTING
          input         = L_TIME
        IMPORTING
          OUTPUT        = L_STIM .

      L_CHAR = l_cinput+18(3) .
      TRANSLATE L_CHAR TO UPPER CASE .
      CASE l_char.
        WHEN 'A'  OR 'ACT' .
          CONCATENATE L_SDAT L_STIM 'A'  INTO output  .
        WHEN 'P'  OR 'PLN' .
          CONCATENATE L_SDAT L_STIM 'P'  INTO output  .
      ENDCASE.
    WHEN 18 .
      l_date = l_cinput(10).         l_time = l_cinput+10(8)     .
      CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
        EXPORTING
          input         = L_DATE
        IMPORTING
          OUTPUT        = L_SDAT .

      CALL FUNCTION 'CONVERSION_EXIT_STIME_INPUT'
        EXPORTING
          input         = L_TIME
        IMPORTING
          OUTPUT        = L_STIM .

      CONCATENATE L_SDAT L_STIM      INTO  output  .
    WHEN OTHERS.
*     RAISE illegal_size .
  ENDCASE.
ENDFUNCTION.
