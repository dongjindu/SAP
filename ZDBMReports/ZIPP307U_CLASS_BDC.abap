************************************************************************
* Program Name      : ZIPP307U_CLASS_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003-08-18
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K901982
* Addl Documentation:
* Description       : Class Definition
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP307U_CLASS_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  400
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP307L_CLASS_BDC_T.
INCLUDE ZIPP307L_CLASS_BDC_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

START-OF-SELECTION.
  CASE 'X'.
*   INTERFACE TABLE
    WHEN P_RDO1.
      PERFORM READ_PROCESS.
      PERFORM DATA_PROCESS.
      PERFORM BDC_PROCESS.
      PERFORM ERROR_CHECK.
      PERFORM UPDATE_PROCESS.
      PERFORM WRITE_PROCESS.
*   EXCEL UPLOAD
    WHEN P_RDO2.
      PERFORM UPLOAD_PROCESS.
      PERFORM BDC_PROCESS_01.
      PERFORM ERROR_CHECK1.
      PERFORM WRITE_PROCESS_01.
  ENDCASE.
END-OF-SELECTION.
