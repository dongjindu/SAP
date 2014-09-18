************************************************************************
* Program Name      : ZIPP303U_MAT_CRE_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003-08-12
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K901912
* Addl Documentation:
* Description       : Material Master Interface
*
* Modification Logs
* Date       Developer    Request ID Description
* 2003.10.01 UD1K901912              SAP Material Master Interface
* 2003.12.21 UD1K905279              SAP Material Master Interface
* 2004.02.20 UD1K907658              SAP Material Master Interface
************************************************************************
REPORT ZIPP303U_MAT_CRE_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP303L_MAT_CRE_BDC_T.
INCLUDE ZIPP303L_MAT_CRE_BDC_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.
START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
*  PERFORM BAPI_PROCESS.
*  PERFORM WRITE_PROCESS.
END-OF-SELECTION.
