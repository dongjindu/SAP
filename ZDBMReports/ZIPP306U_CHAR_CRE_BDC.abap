************************************************************************
* Author                 : bong soo kim
* Creation Date          : 2003-08-18
* Specifications By      :
* Development Request No : UD1K901954
* Addl documentation     :
* Description            : Interface Characteristics Definition
*                          from Legacy to SAP.
*
*
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************
REPORT ZIPP306U_CHAR_CRE_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  255
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP306L_CHAR_CRE_BDC_T.
INCLUDE ZIPP306L_CHAR_CRE_BDC_F01.
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
