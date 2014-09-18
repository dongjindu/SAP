************************************************************************
* Program Name      : ZIPP308U_OBJ_DEP_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003.08.22.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K901999
* Addl Documentation:
* Description       : OBJECT DEPENDENCY
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP308U_OBJ_DEP_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP308L_OBJ_DEP_BDC_T.
INCLUDE ZIPP308L_OBJ_DEP_BDC_F01.
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
    WHEN P_RDO1.  "INTERFACE TABLE
      PERFORM READ_PROCESS.
      PERFORM DATA_PROCESS.
      PERFORM BDC_PROCESS.
      PERFORM UPDATE_PROCESS.
      PERFORM WRITE_PROCESS.
    WHEN P_RDO2.  "EXCEL
      PERFORM UPLOAD_PROCESS.
      PERFORM BDC_PROCESS_01.
      PERFORM WRITE_PROCESS_01.
  ENDCASE.
END-OF-SELECTION.
