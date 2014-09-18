************************************************************************
* Program Name      : ZIPP305U_FSC_219_DATA_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003.08.30.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K902101
* Addl Documentation:
* Description       : FSC 219 DATA
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP305U_FSC_219_DATA_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP305L_FSC_219_DATA_BDC_01_T.
INCLUDE ZIPP305L_FSC_219_DATA_B_01_F01.
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
      REFRESH IT_EXCL. CLEAR IT_EXCL.
      PERFORM UPLOAD_PROCESS.
      PERFORM DATA_PROCESS1.
      PERFORM BDC_PROCESS1.
      PERFORM WRITE_PROCESS1.
  ENDCASE.

END-OF-SELECTION.
