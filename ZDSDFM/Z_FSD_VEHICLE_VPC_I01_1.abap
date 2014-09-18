************************************************************************
* Program Name      : Z_FSD_VEHICLE_VPC_I01_1
* Author            : HONG KI KIM
* Creation Date     : 2003.09.16.
* Specifications By : HONG KI KIM
* Pattern           : I/F 7-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Z_FSD_VEHICLE_VPC_I01_1
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
FUNCTION Z_FSD_VEHICLE_VPC_I01_1.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      ITAB STRUCTURE  ZTSD_GL_INF_I01A
*"----------------------------------------------------------------------
*DATA : DSN(50).
*  CONCATENATE '/sapmnt' SY-SYSID 'EDI/VPC_I01_1.txt'
*                                        INTO DSN
*                                        SEPARATED BY '/'.
*
*  OPEN DATASET DSN IN TEXT MODE FOR OUTPUT.
*  LOOP AT ITAB.
*      OPEN DATASET DSN IN TEXT MODE FOR APPENDING.
*      TRANSFER ITAB TO DSN.
*  ENDLOOP.
*
*  CLOSE DATASET DSN.

ENDFUNCTION.
