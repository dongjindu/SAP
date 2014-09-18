************************************************************************
* Program Name      : ZEPP317U_MAT_INIT_BDC_01
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.29.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K902045
* Addl Documentation:
* Description       : Material Master Initial Upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZEPP317U_MAT_INIT_BDC_01
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZEPP317L_MAT_INIT_BDC_01_T.
INCLUDE ZEPP317L_MAT_INIT_BDC_01_F01.
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


    WHEN P_RDO2.  "EXCEL
      REFRESH IT_EXCL. CLEAR IT_EXCL.
      PERFORM UPLOAD_PROCESS.
      PERFORM BDC_PROCESS.
      PERFORM WRITE_PROCESS.
  ENDCASE.

END-OF-SELECTION.
