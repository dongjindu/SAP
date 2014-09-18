FUNCTION z_fca_get_exchange_rate.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(CLIENT) LIKE  ZSCA_EXCHANGE_RATE-MANDT DEFAULT
*"       SY-MANDT
*"     REFERENCE(DATE) LIKE  ZSCA_EXCHANGE_RATE-DATUM
*"     REFERENCE(SOURCE_CURRENCY) LIKE  ZSCA_EXCHANGE_RATE-FCURR
*"     REFERENCE(TARGET_CURRENCY) LIKE  ZSCA_EXCHANGE_RATE-FCURR
*"     REFERENCE(COMPANY_CURRENCY) LIKE  ZSCA_EXCHANGE_RATE-FCURR
*"     REFERENCE(TYPE_OF_RATE) LIKE  ZSCA_EXCHANGE_RATE-KURST
*"     REFERENCE(SOURCE_AMOUNT) LIKE  ZSCA_EXCHANGE_RATE-WRBTR OPTIONAL
*"  EXPORTING
*"     REFERENCE(EXCHANGE_RATE) TYPE  F
*"     REFERENCE(VALID_FROM_DATE) LIKE  ZSCA_EXCHANGE_RATE-DATUM
*"     REFERENCE(DERIVED_RATE_TYPE) LIKE  ZSCA_EXCHANGE_RATE-KURST
*"     REFERENCE(FIXED_RATE) LIKE  ZSCA_EXCHANGE_RATE-WKURS
*"     REFERENCE(TARGET_AMOUNT) LIKE  ZSCA_EXCHANGE_RATE-DMBTR
*"  EXCEPTIONS
*"      TARGET_LOCAL_RATE_ERROR
*"      SOURCE_LOCAL_RATE_ERROR
*"----------------------------------------------------------------------
  DATA: lv_exchange_rate_01  LIKE zsca_exchange_rate-wkurs,
        lv_exchange_rate_02  LIKE zsca_exchange_rate-wkurs,
        lv_foreign_factor_01 TYPE f,
        lv_foreign_factor_02 TYPE f,
        lv_local_factor_01   TYPE f,
        lv_local_factor_02   TYPE f,
        lv_source_point  LIKE cffsd-rvalf,
        lv_target_point  LIKE cffsd-rvalf.

  IF target_currency EQ company_currency.
    CALL FUNCTION 'READ_EXCHANGE_RATE'
         EXPORTING
              client            = client
              date              = date
              foreign_currency  = source_currency
              local_currency    = target_currency
              type_of_rate      = type_of_rate
         IMPORTING
              exchange_rate     = lv_exchange_rate_01
              foreign_factor    = lv_foreign_factor_01
              local_factor      = lv_local_factor_01
              valid_from_date   = valid_from_date
              derived_rate_type = derived_rate_type
              fixed_rate        = fixed_rate
         EXCEPTIONS
              no_rate_found     = 1
              no_factors_found  = 2
              no_spread_found   = 3
              derived_2_times   = 4
              overflow          = 5
              OTHERS            = 6.
    IF sy-subrc NE 0.
      RAISE target_local_rate_error.
    ENDIF.

    target_amount = source_amount      * lv_exchange_rate_01 *
                    lv_local_factor_01 / lv_foreign_factor_01.
    exchange_rate = lv_exchange_rate_01 *
                    lv_local_factor_01  / lv_foreign_factor_01.
  ELSE.
    CALL FUNCTION 'READ_EXCHANGE_RATE'
         EXPORTING
              client            = client
              date              = date
              foreign_currency  = source_currency
              local_currency    = company_currency
              type_of_rate      = type_of_rate
         IMPORTING
              exchange_rate     = lv_exchange_rate_01
              foreign_factor    = lv_foreign_factor_01
              local_factor      = lv_local_factor_01
              valid_from_date   = valid_from_date
              derived_rate_type = derived_rate_type
              fixed_rate        = fixed_rate
         EXCEPTIONS
              no_rate_found     = 1
              no_factors_found  = 2
              no_spread_found   = 3
              derived_2_times   = 4
              overflow          = 5
              OTHERS            = 6.
    IF sy-subrc NE 0.
      RAISE source_local_rate_error.
    ENDIF.

    CALL FUNCTION 'READ_EXCHANGE_RATE'
         EXPORTING
              client            = client
              date              = date
              foreign_currency  = target_currency
              local_currency    = company_currency
              type_of_rate      = type_of_rate
         IMPORTING
              exchange_rate     = lv_exchange_rate_02
              foreign_factor    = lv_foreign_factor_02
              local_factor      = lv_local_factor_02
              valid_from_date   = valid_from_date
              derived_rate_type = derived_rate_type
              fixed_rate        = fixed_rate
         EXCEPTIONS
              no_rate_found     = 1
              no_factors_found  = 2
              no_spread_found   = 3
              derived_2_times   = 4
              overflow          = 5
              OTHERS            = 6.
    IF sy-subrc NE 0.
      RAISE target_local_rate_error.
    ENDIF.

    target_amount = source_amount       *
                    lv_exchange_rate_01 *
                    lv_local_factor_01  / lv_foreign_factor_01 /
                    ( lv_exchange_rate_02 *
                    lv_local_factor_02  / lv_foreign_factor_02 ).
    exchange_rate = lv_exchange_rate_01 *
                    lv_local_factor_01  / lv_foreign_factor_01 /
                    ( lv_exchange_rate_02 *
                    lv_local_factor_02  / lv_foreign_factor_02 ).
  ENDIF.

*----- Read currency decimal point & calculate amount
  CALL FUNCTION 'Z_FCA_GET_DECIMAL_PLACE_IN_CUR'
    EXPORTING
      i_currkey       = source_currency
    IMPORTING
      E_POINT         = lv_source_point.

  CALL FUNCTION 'Z_FCA_GET_DECIMAL_PLACE_IN_CUR'
       EXPORTING
            i_currkey = target_currency
       IMPORTING
            e_point   = lv_target_point.

  target_amount = target_amount * lv_source_point / lv_target_point.
ENDFUNCTION.
