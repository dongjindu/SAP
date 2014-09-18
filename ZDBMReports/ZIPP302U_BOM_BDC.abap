************************************************************************
* Program Name      : ZIPP302U_BOM_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003.09.04.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K902160
* Addl Documentation:
* Description       : BOM STRUCTURE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2003.10.01  UD1K902160                BOM STRUCTURE
*
*
************************************************************************
REPORT ZIPP302U_BOM_BDC
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZIPP302L_BOM_BDC_T.
INCLUDE ZIPP302L_BOM_BDC_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS USING WA_CHECK.
*  IF WA_CHECK NE 'X'.
*    PERFORM HEADER_BOM_PROCESS CHANGING WA_CHECK.
    IF WA_CHECK NE 'X'.
      PERFORM BDC_PROCESS.
**     BOM EXPLODED.
      PERFORM BOM_EXPLODED TABLES IT_BOM_EXPLODED.
**     MARA CONFIGURABLE MATERIAL CHECK
      IF NOT IT_BOM_EXPLODED[] IS INITIAL.
        PERFORM MARA_CONFIGURABLE_MATERIAL TABLES IT_BOM_EXPLODED.
        PERFORM MM02_CONFIGURABLE_MATERIAL_BDC.
        PERFORM MAST_STPO_SELECT_STLKN_CS02BDC.
      ENDIF.

*      PERFORM WRITE_PROCESS.
*    ELSE.
*      PERFORM HEDAER_BOM_PROCESS_ERROR_WRITE.
    ENDIF.
*  ELSE.
*    PERFORM DATA_PROCESS_ERROR_WRITE.
*  ENDIF.
