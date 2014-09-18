************************************************************************
* Program Name      : ZIPP301U_ECM_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003.08.10.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K901912
* Addl Documentation:
* Description       : Engineering Change Master
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZIPP301U_ECM_BDC
       NO STANDARD PAGE HEADING
       LINE-SIZE  255
       LINE-COUNT 65
       MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP301L_ECM_BDC_T.
INCLUDE ZIPP301L_ECM_BDC_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM SCREEN_MODIFY.
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
*  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

START-OF-SELECTION.
*  PERFORM UPLOAD_PROCESS.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
  PERFORM TABLE_UPDATE.
  PERFORM WRITE_PROCESS.
END-OF-SELECTION.
