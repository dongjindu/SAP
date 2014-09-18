************************************************************************
* Program Name      : Z_FSD_VEHICLE_GOODISSUE
* Author            : jun ho choi
* Creation Date     : 2004.01.26.
* Specifications By : jun ho choi
* Pattern           : 7-2
* Development Request No : UD1K906426
* Addl Documentation:
* Description       : Creation of good issue for vehicles (CANADA)
*
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
FUNCTION Z_FSD_VEHICLE_GOODISSUE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EQUNO)
*"  EXPORTING
*"     VALUE(RETURN1) LIKE  SY-SUBRC
*"     VALUE(RETURN2) LIKE  SY-SUBRC
*"----------------------------------------------------------------------

  PERFORM BDC_VL02N_GI    USING EQUNO RETURN1 RETURN2.
ENDFUNCTION.
