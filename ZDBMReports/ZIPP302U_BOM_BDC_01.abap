************************************************************************
* Program Name      : ZIPP302U_BOM_BDC_01
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.23.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K902160
* Addl Documentation:
* Description       : BOM Structure VERSION 1
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP302U_BOM_BDC_01
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP302L_BOM_BDC_01_T.
INCLUDE ZIPP302L_BOM_BDC_01_F01.
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
  IF P_RDO1 EQ 'X'.
*   TABLE SELECTION DATA BDC
    PERFORM TABLE_SELECTION_DATA_BDC.
  ELSEIF P_RDO2 EQ 'X'.
*   EXECL DATA BDC
    PERFORM EXCEL_DATA_BDC_PROCESS.

  ENDIF.
