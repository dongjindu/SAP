FUNCTION z_fi_pick_one_barcode.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(AR_OBJECT) LIKE  TOAOM-AR_OBJECT DEFAULT SPACE
*"     VALUE(SAP_OBJECT) LIKE  TOAOM-SAP_OBJECT
*"     VALUE(BUKRS) TYPE  BUKRS
*"     VALUE(GJAHR) TYPE  GJAHR
*"     VALUE(BELNR) TYPE  BELNR_D
*"     VALUE(I_BARCODE) TYPE  CHAR40 OPTIONAL
*"  EXPORTING
*"     VALUE(BARCODE) TYPE  CHAR40
*"  EXCEPTIONS
*"      ERROR_CONNECTIONTABLE
*"      ERROR_PARAMETER
*"----------------------------------------------------------------------

  DATA : $barcode LIKE toav0-arc_doc_id,
         $objectid LIKE sapb-sapobjid.

  CLEAR : barcode.

  CONCATENATE bukrs belnr gjahr
   INTO $barcode.

*  SELECT SINGLE arc_doc_id INTO barcode FROM toa01 WHERE
*          object_id EQ $barcode.
*
*  IF sy-subrc EQ 0.
*    EXIT.
*  ENDIF.

  SELECT SINGLE barcode INTO barcode FROM bds_bar_in WHERE
          object_key EQ $barcode.

  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

  $objectid = $barcode.

  IF NOT i_barcode IS INITIAL.
    CALL FUNCTION 'ARCHIV_BARCODE_INSERT'
         EXPORTING
              ar_object             = ar_object
              barcode_id            = i_barcode
              object_id             = $objectid
              sap_object            = sap_object
         EXCEPTIONS
              error_connectiontable = 1
              error_parameter       = 2
              OTHERS                = 3.
  ELSE.
    CALL FUNCTION 'ARCHIV_BARCODE_INSERT'
         EXPORTING
              ar_object             = ar_object
              barcode_id            = $barcode
              object_id             = $objectid
              sap_object            = sap_object
         EXCEPTIONS
              error_connectiontable = 1
              error_parameter       = 2
              OTHERS                = 3.
  ENDIF.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        RAISE error_connectiontable.
      WHEN 2.
        RAISE error_parameter.
      WHEN OTHERS.
        RAISE other_error.
    ENDCASE.
  ELSE.
    barcode = $barcode.
  ENDIF.

ENDFUNCTION.
