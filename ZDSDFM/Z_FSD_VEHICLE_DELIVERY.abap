************************************************************************
* Program Name      : Z_FSD_VEHICLE_DELIVERY
* Author            : jun ho choi
* Creation Date     : 2003.09.03.
* Specifications By : jun ho choi
* Pattern           : 7-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Creation of deliveries for vehicles that
*                          reach reporting point 08.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
FUNCTION Z_FSD_VEHICLE_DELIVERY.
*"----------------------------------------------------------------------
*"*"Local interface:
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

  PERFORM GET_SALES_NO USING EQUNO.
  CHECK SY-SUBRC = 0.
  PERFORM BDC_VL01N    USING DELIVERY_NO.
ENDFUNCTION.
