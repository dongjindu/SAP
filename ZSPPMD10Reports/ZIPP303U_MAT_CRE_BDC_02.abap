************************************************************************
* Program Name      : ZIPP303U_MAT_CRE_BDC_01
* Author            : Bongsoo, Kim
* Creation Date     : 2004-03-25
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K907658
* Addl Documentation:
* Description       : Material Master Create BDC Version 1
*
* Modification Logs
************************************************************************
REPORT ZIPP303U_MAT_CRE_BDC_01
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP303L_MAT_CRE_BDC_02_T.
INCLUDE ZIPP303L_MAT_CRE_BDC_02_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.
START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
END-OF-SELECTION.
