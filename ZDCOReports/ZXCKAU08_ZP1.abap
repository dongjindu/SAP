*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU08_ZP1                                               *
*----------------------------------------------------------------------*
*Try again
*F_MATBW-MENGE, F_MATBW-MEINS

CALL FUNCTION 'Z_CO_GET_VENDOR_SOURCE_AUTO'
     EXPORTING
          matnr           = f_matbw-matnr
          werks           = f_matbw-werks
          available_date  = f_matbw-bwdat
     IMPORTING
          lifnr           = l_lifnr
          used_source     = l_text
          ekorg           = l_ekorg
          infnr           = l_infnr
     EXCEPTIONS
          no_source_found = 1
          invalid_werks   = 2
          OTHERS          = 3.

IF sy-subrc = 0.
  CALL FUNCTION 'Z_CO_GET_INFO_AMT'
       EXPORTING
            ekorg      = l_ekorg
            matnr      = f_matbw-matnr
            lifnr      = l_lifnr
            infnr      = l_infnr
            valid_date = f_matbw-bwdat
*            menge      = f_matbw-menge
*            meins      = f_matbw-meins
       IMPORTING
            exp_preis  = exp_preis
            exp_peinh  = exp_peinh
            exp_waers  = exp_waers
            exp_kmein  = l_kmein
       EXCEPTIONS
            not_found  = 1
            OTHERS     = 2.

ENDIF.

IF SY-SUBRC <> 0.
*DEFAULT DUMMY PRICE
*Change message type to '*' - no error
*CK 361 - Value of costing item 00001 in itemization is 0
*CK 240 - Cost component split costed with value of zero

  exp_peinh = '9999'.
  exp_preis = '0.01'.
ENDIF.

exp_waers = f_matbw-waers.
