FUNCTION Z_FPP_REALTIME_CHECK .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(WORD) LIKE  ZTPP_KSBOHMM-WO_SER
*"     VALUE(DIST) LIKE  ZTPP_KSBOHMM-DEST
*"     VALUE(EXTC) LIKE  ZTPP_KSBOHMM-EXTC
*"     VALUE(INTC) LIKE  ZTPP_KSBOHMM-INTC
*"     VALUE(MULTIPLE) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_ZSPP_WORKORDER STRUCTURE  ZSPP_WORKORDER
*"----------------------------------------------------------------------
  DATA: l_qty(5)           TYPE n         ,
        l_header           LIKE mara-matnr,
        l_color            LIKE mara-matnr,
       l_vals             LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  IF multiple = space.
    CONCATENATE word dist extc intc INTO l_color .
    CLEAR: t_zspp_workorder.
    t_zspp_workorder-word   = word.
    t_zspp_workorder-dist   = dist.
    t_zspp_workorder-extc   = extc.
    t_zspp_workorder-intc   = intc.
    CLEAR: l_vals, l_vals[].
    l_vals-atnam = 'P_PERF_YN' .    APPEND l_vals.
    l_vals-atnam = 'P_MOD_QTY' .    APPEND l_vals.
    l_vals-atnam = 'P_SEQ_QTY' .    APPEND l_vals.
    l_vals-atnam = 'P_MITU_QTY'.    APPEND l_vals.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object         = l_color
              ctype          = '001'
         TABLES
              val_table      = l_vals
         EXCEPTIONS
           NO_DATA           = 1
           ERROR_MODE        = 2
           ERROR_OBJECT      = 3
           ERROR_VALUE       = 4
           OTHERS            = 5 .

    IF SY-SUBRC NE 0.
      t_zspp_workorder-perf = 'N'.
      t_zspp_workorder-prod = 'N'.
      t_zspp_workorder-FLAG = 'E'.
      APPEND t_zspp_workorder .
      EXIT.
    ENDIF.

    CLEAR: l_vals. READ TABLE l_vals WITH KEY atnam = 'P_PERF_YN'.
    IF l_vals-atwrt = 'Y'.
      t_zspp_workorder-perf = 'Y'.
    ELSE.
      t_zspp_workorder-perf = 'N'.
    ENDIF.
    CLEAR: l_vals. READ TABLE l_vals WITH KEY atnam = 'P_MOD_QTY'.
    t_zspp_workorder-cqty   = l_vals-atwrt .
    CLEAR: l_vals. READ TABLE l_vals WITH KEY atnam = 'P_SEQ_QTY'.
    t_zspp_workorder-sqty   = l_vals-atwrt .
    CLEAR: l_vals. READ TABLE l_vals WITH KEY atnam = 'P_MITU_QTY'.
    t_zspp_workorder-mqty   = l_vals-atwrt .

    CONCATENATE word dist           INTO l_header.
    CLEAR: l_vals, l_vals[].
    l_vals-atnam = 'P_PROD_FLAG'.   APPEND l_vals.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object             = l_header
        CTYPE              = '001'
      tables
        val_table          = l_vals
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

    CLEAR: l_vals. READ TABLE l_vals WITH KEY atnam = 'P_PROD_FLAG'.
    IF l_vals-atwrt = 'Y'.
      t_zspp_workorder-prod = 'Y'.
    ELSE.
      t_zspp_workorder-prod = 'N'.
    ENDIF.
    APPEND t_zspp_workorder .
  ELSE.
  ENDIF.
ENDFUNCTION.
