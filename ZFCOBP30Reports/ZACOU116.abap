*----------------------------------------------------------------------
* Program ID        : ZACOU116
* Title             : [CO] ABP LDC Rate
* Created on        : 10/24/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Maintain FSC/MIP/Module.
*----------------------------------------------------------------------
REPORT ZACOU116 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU116_TOP.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_KOKRS LIKE ZTCOU102-KOKRS OBLIGATORY
                                    MEMORY ID CAC
                                    MATCHCODE OBJECT FC_KOKRS,
            P_YEAR  LIKE ZTCOU102-BDATJ OBLIGATORY MEMORY ID BDTJ,
            P_VER   LIKE ZTCOU102-VER.
SELECTION-SCREEN END OF BLOCK B0.

parameters: p_ref as checkbox.

INCLUDE ZACOU116_F01.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_CONTROL OUTPUT.
  IF G_CUSTOM_CONTAINER IS INITIAL.
*   Create object
    PERFORM CREATE_OBJECT.

*   Exclude toolbar
    PERFORM EXCLUDE_FUNCTIONS USING 'GT_EXCLUDE'.

*   Create field category
    PERFORM CREATE_FIELD_CATEGORY.

*   Setting for layout
*    GS_LAYO-EDIT       = 'X'.
    GS_LAYO-ZEBRA      = 'X'.
    GS_LAYO-SEL_MODE   = 'A'.       " Column and row selection
    GS_LAYO-STYLEFNAME = 'CELLTAB'.

*   Define editable field
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
         EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*   Setting for event
    CREATE OBJECT G_EVENT_RECEIVER.
    SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR G_GRID.

*   Define variant
    GS_VARIANT-REPORT = SY-REPID.

*   Display alv grid
    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING IS_LAYOUT            = GS_LAYO
                   IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
                   I_SAVE               = GC_VAR_SAVE
                   IS_VARIANT           = GS_VARIANT
         CHANGING  IT_OUTTAB            = GT_OUT[]
                   IT_FIELDCATALOG      = GT_FCAT[].

  ENDIF.

ENDMODULE.                 " CREATE_ALV_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      PERFORM SAVE_DATA.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
