************************************************************************
* Program Name      : ZIPP304U_FSC_PRODU_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003.08.29.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K902095
* Addl Documentation:
* Description       : FSC PRODUCTION VERSION
* Modification Logs
* Date       Developer    RequestNo    Description
* 2003.12.21              UD1K905279   FSC PRODUCTION VERSION
* 2004.01.06              UD1K905477   Reference Rate Routing Assignment
* 2011.10.24 KDM          UP1K930009   BDC Logic change(KDM01)
************************************************************************
REPORT ZIPP304U_FSC_PRODU_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP304L_FSC_PRODU_BDC_01_T.
INCLUDE ZIPP304L_FSC_PRODU_BDC_01_F01.
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
      PERFORM TITLE.
      PERFORM READ_PROCESS.
      PERFORM DATA_PROCESS.
      PERFORM BDC_PROCESS.
      PERFORM UPDATE_PROCESS.
      PERFORM WRITE_PROCESS.
*      PERFORM WRITE_ROUTING_PROCESS.
    WHEN P_RDO2.  "EXCEL
      PERFORM TITLE.
      REFRESH IT_EXCL. CLEAR IT_EXCL.
      PERFORM UPLOAD_PROCESS.
      PERFORM DATA_PROCESS1.
      PERFORM BDC_PROCESS1.
      PERFORM WRITE_PROCESS1.
  ENDCASE.

END-OF-SELECTION.
