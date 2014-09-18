************************************************************************
* Program Name      : ZIPP302U_BOM_MAT_CONFI_CHK
* Author            : Bongsoo, Kim
* Creation Date     : 2003.09.15.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K902160
* Addl Documentation:
* Description       : Material configurable check
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP302U_BOM_MAT_CONFI_CHK
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.

*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP302L_BOM_MAT_CONFI_CHK_T.
INCLUDE ZIPP302L_BOM_MAT_CONFI_CHK_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
  PERFORM WRITE_PROCESS.
