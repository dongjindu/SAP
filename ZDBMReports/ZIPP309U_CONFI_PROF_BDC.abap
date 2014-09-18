************************************************************************
* Program Name      : ZIPP309U_CONFI_PROF_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003.09.01.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K902103
* Addl Documentation:
* Description       : CONFIGURATION PROFILE
* Modification Logs
* Date       Developer    RequestNo    Description
* 2003.12.21 UD1K905279              CONFIGURATION PROFILE
*
*
************************************************************************
REPORT ZIPP309U_CONFI_PROF_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP309L_CONFI_PROF_BDC_T.
INCLUDE ZIPP309L_CONFI_PROF_BDC_F01.
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
      PERFORM DATA_MAST_CHECK.
      PERFORM DATA_PROCESS.
      PERFORM BDC_PROCESS.
      PERFORM UPDATE_PROCESS.
      PERFORM WRITE_PROCESS.
    WHEN P_RDO2.  "EXCEL
      REFRESH IT_EXCL. CLEAR IT_EXCL.
      PERFORM UPLOAD_PROCESS.
      PERFORM DATA_PROCESS1.
      PERFORM BDC_PROCESS1.
      PERFORM WRITE_PROCESS1.
  ENDCASE.

END-OF-SELECTION.
