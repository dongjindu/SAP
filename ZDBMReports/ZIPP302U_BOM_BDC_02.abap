************************************************************************
* Program Name      : ZIPP302U_BOM_BDC_02
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.13.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K902160
* Addl Documentation:
* Description       : BOM Structure VERSION 2
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2003.12.17  Bongsoo,Kim  UD1K905094   BOM Structure VERSION 2
*
*
************************************************************************
REPORT ZIPP302U_BOM_BDC_02
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP302L_BOM_BDC_02_T.
INCLUDE ZIPP302L_BOM_BDC_02_F01.
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
