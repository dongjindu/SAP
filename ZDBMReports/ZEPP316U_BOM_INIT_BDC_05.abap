************************************************************************
* Program Name      : ZEPP316U_BOM_INIT_BDC_05
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.21.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K901990
* Addl Documentation:
* Description       : BOM EXECL BDC version 5
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZEPP316U_BOM_INIT_BDC_05
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZEPP316L_BOM_INIT_BDC_05_T.
INCLUDE ZEPP316L_BOM_INIT_BDC_05_F01.
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
  PERFORM UPLOAD_PROCESS.
  PERFORM DEPENDENCY_ECM_CHECK CHANGING WA_CHECK.
  IF WA_CHECK NE 'X'.
    PERFORM DATA_PROCESS CHANGING WA_CHECK.
    IF WA_CHECK NE 'X'.
*     COLOR PART & NON COLOR PART PARTITION
      PERFORM COLOR_PART_PARTITION.
**    BOM ITEM CREATE
      PERFORM BDC_PROCESS.
**     BOM EXPLODED.
      PERFORM BOM_EXPLODED TABLES IT_BOM_EXPLODED.
**     MARA CONFIGURABLE MATERIAL CHECK
      IF NOT IT_BOM_EXPLODED[] IS INITIAL.
        PERFORM MARA_CONFIGURABLE_MATERIAL TABLES IT_BOM_EXPLODED.
        PERFORM MM02_CONFIGURABLE_MATERIAL_BDC.
      ENDIF.
      PERFORM OBJECT_DEPENDENCY_APPENDING.
    ENDIF.
  ENDIF.
