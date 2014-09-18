************************************************************************
* Program Name      : Z_FSD_VEHICLE_DELIVERY_BAPI
* Author            : IG Moon
* Creation Date     : 2012.4.5.
************************************************************************
FUNCTION Z_FSD_VEHICLE_DELIVERY_BAPI.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EQUNO)
*"  EXPORTING
*"     VALUE(DELIVERY_NO) LIKE  LIKP-VBELN
*"  EXCEPTIONS
*"      NOT_FOUND_VIN
*"      NOT_FOUND_SALES
*"      NOT_FOUND_VSTEL
*"      NOT_CREATE_DELIVERY
*"      NOT_UPDATE_DELIVERY
*"----------------------------------------------------------------------

  PERFORM get_sales_no_new USING equno.
  IF sy-subrc NE 0.
    RAISE not_found_sales.
  ENDIF.

  PERFORM call_bapi    USING delivery_no.

ENDFUNCTION.
