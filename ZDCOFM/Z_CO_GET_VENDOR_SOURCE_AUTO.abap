FUNCTION z_co_get_vendor_source_auto.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(MATNR) TYPE  MATNR
*"     REFERENCE(WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(AVAILABLE_DATE) TYPE  DATUM
*"  EXPORTING
*"     REFERENCE(LIFNR) TYPE  LIFNR
*"     REFERENCE(USED_SOURCE) TYPE  TABNAME16
*"     REFERENCE(EKORG) TYPE  EKORG
*"     REFERENCE(INFNR) TYPE  INFNR
*"  EXCEPTIONS
*"      NO_SOURCE_FOUND
*"      INVALID_WERKS
*"----------------------------------------------------------------------

  DATA: l_lifnr TYPE  lifnr,
        l_ekorg TYPE  ekorg,
        l_infnr TYPE  infnr,
        l_text  TYPE  tabname16.

  IF available_date >= sy-datum. " current or future

    CALL FUNCTION 'Z_CO_GET_VENDOR_SOURCE'
         EXPORTING
              matnr           = matnr
              werks           = werks
              available_date  = available_date
              sub_part        = space
              use_source_list = 'X'
         IMPORTING
              lifnr           = lifnr
              used_source     = used_source
              ekorg           = ekorg
              infnr           = infnr
         EXCEPTIONS
              no_source_found = 1
              invalid_werks   = 2
              OTHERS          = 3.

    IF sy-subrc NE 0.

      CALL FUNCTION 'Z_CO_GET_VENDOR_SOURCE'
           EXPORTING
                matnr           = matnr
                werks           = werks
                available_date  = available_date
                sub_part        = space
                use_source_list = ' '
           IMPORTING
                lifnr           = lifnr
                used_source     = used_source
                ekorg           = ekorg
                infnr           = infnr
           EXCEPTIONS
                no_source_found = 1
                invalid_werks   = 2
                OTHERS          = 3.

      IF sy-subrc <> 0.
          RAISE NO_SOURCE_FOUND.
      ENDIF.

    ENDIF.

  ELSE. " past

    CALL FUNCTION 'Z_CO_GET_VENDOR_SOURCE'
         EXPORTING
              matnr           = matnr
              werks           = werks
              available_date  = available_date
         IMPORTING
              lifnr           = lifnr
              used_source     = used_source
              ekorg           = ekorg
              infnr           = infnr
         EXCEPTIONS
              no_source_found = 1
              invalid_werks   = 2
              OTHERS          = 3.

    IF sy-subrc NE 0.
      CALL FUNCTION 'Z_CO_GET_VENDOR_SOURCE'
           EXPORTING
                matnr           = matnr
                werks           = werks
                available_date  = available_date
                sub_part        = space
                use_source_list = 'X'
           IMPORTING
                lifnr           = lifnr
                used_source     = used_source
                ekorg           = ekorg
                infnr           = infnr
           EXCEPTIONS
                no_source_found = 1
                invalid_werks   = 2
                OTHERS          = 3.

      IF sy-subrc NE 0.

        CALL FUNCTION 'Z_CO_GET_VENDOR_SOURCE'
             EXPORTING
                  matnr           = matnr
                  werks           = werks
                  available_date  = available_date
                  sub_part        = 'X'
             IMPORTING
                  lifnr           = lifnr
                  used_source     = used_source
                  ekorg           = ekorg
                  infnr           = infnr
             EXCEPTIONS
                  no_source_found = 1
                  invalid_werks   = 2
                  OTHERS          = 3.

        IF sy-subrc <> 0.
          RAISE NO_SOURCE_FOUND.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
