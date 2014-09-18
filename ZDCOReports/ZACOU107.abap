*----------------------------------------------------------------------
* Program ID        : ZACOU107
* Title             : [CO] Change Unit Cost Variance
* Created on        : 09/29/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Program for change unit cost variance.
*                     Get data from table ZTCOU106.
*                     Modify reason & reason amount.
*----------------------------------------------------------------------
REPORT ZACOU107 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOUI00.
INCLUDE ZACOU107_TOP.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME.
PARAMETERS: P_KOKRS LIKE ZTCOU106-KOKRS OBLIGATORY MEMORY ID CAC
                                        MATCHCODE OBJECT FC_KOKRS,
            P_YEAR  LIKE ZTCOU106-BDATJ OBLIGATORY MEMORY ID BDTJ,
            P_KALKA LIKE ZTCOU106-KALKA OBLIGATORY MEMORY ID KKA,
            P_POPER LIKE ZTCOU102-POPER OBLIGATORY MEMORY ID POPR.
SELECT-OPTIONS: S_ID  FOR ZTCOU104-ID MATCHCODE OBJECT ZID,
                S_LIFNR FOR ZTCOU106-LIFNR,
                S_UPGVC FOR ZTCOU106-UPGVC,
                S_COMPN FOR ZTCOU106-COMPN,
                S_RSN FOR ZTCOU106-KZUST,
* UD1K941594 by IG.MOON 9/18/07
* {
                S_ZRCLSS FOR ZTCOU106-ZRCLSS,
                S_EKGRP FOR ZTCOU106-EKGRP.
* }

SELECTION-SCREEN END OF BLOCK B0.

INCLUDE ZACOU107_F01.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
* Possible entries for Costing type
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_KALKA.
  PERFORM POPUP_KALKA USING P_KALKA 'P_KALKA'.

* Possible entries for Reason
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_RSN-LOW.
  PERFORM POPUP_RSN USING S_RSN-LOW 'S_RSN-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_RSN-HIGH.
  PERFORM POPUP_RSN USING S_RSN-HIGH 'S_RSN-HIGH'.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get data of Calculate Variances from table ZTCOU106
  PERFORM GET_DATA.

  IF GV_CNT = 0.
    MESSAGE S000 WITH 'No Data Found.'.
    EXIT.
  ENDIF.

  CLEAR FLAG_DATA_CHANGED.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

  CLEAR GV_SEL.
  PERFORM CREATE_ALV_CONTROL.

ENDMODULE.                 " STATUS_0100  OUTPUT
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

      DATA ANSWER.
      IF FLAG_DATA_CHANGED NE TRUE.

        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
             EXPORTING
                  DEFAULTOPTION  = 'Y'
                  TEXTLINE1      = 'Data has not been changed at all.'
                  TEXTLINE2      = 'Do you want to save anyway?'
                  TITEL          = 'Save'
                  CANCEL_DISPLAY = 'X'
             IMPORTING
                  ANSWER         = ANSWER
             EXCEPTIONS
                  OTHERS         = 1.

        CHECK ANSWER EQ 'J'.
      ENDIF.

      PERFORM SAVE_DATA.
      PERFORM REFRESH_FIELD.
      CLEAR FLAG_DATA_CHANGED.

    WHEN 'SWITCH'.
      PERFORM SWITCH_EDIT_MODE.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SWITCH_EDIT_MODE.

  DATA ANSWER.
  IF G_GRID->IS_READY_FOR_INPUT( ) EQ 0.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 1.
    SET PF-STATUS '100'.
    PERFORM INFO_TEXT_SET USING TRUE.
  ELSE.
    IF FLAG_DATA_CHANGED EQ TRUE.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
           EXPORTING
                TEXTLINE1     = 'Data has not been saved yet.'
                TEXTLINE2     = 'Do you want to continue anyway? '
                TITEL         = 'Confirmation'
                DEFAULTOPTION = 'N'
           IMPORTING
                ANSWER        = ANSWER.
      CHECK ANSWER EQ 'J'.
    ENDIF.
    CLEAR FLAG_DATA_CHANGED.
    CALL METHOD G_GRID->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 0.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
    PERFORM INFO_TEXT_SET USING FALSE.
  ENDIF.


ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM INFO_TEXT_SET USING    P_TRUE.
*  IF P_TRUE EQ TRUE.
*    INFO = TEXT-015.
*  ELSE.
*    INFO = TEXT-016.
*  ENDIF.
ENDFORM.                    " INFO_TEXT_SET
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING FT_SORT TYPE LVC_T_SORT.

  DEFINE SORT_TAB.
    CLEAR GS_SORT.
    GS_SORT-FIELDNAME = &1.
    GS_SORT-SPOS      = &2.
    GS_SORT-UP        = &3.
    GS_SORT-GROUP     = &4.
    GS_SORT-SUBTOT    = &5.
    GS_SORT-COMP      = &6.
    APPEND GS_SORT TO FT_SORT.
  END-OF-DEFINITION.


  SORT_TAB : 'ID'       '1' 'X' 'X' '' 'X',
             'UPGVC'    '2' 'X' 'X' '' 'X',
             'UPGVC_T'  '3' 'X' 'X' '' 'X',
             'MAKTX'    '4' 'X' 'X' '' 'X',
             'COMPN'    '5' 'X' 'X' '' 'X',
             'KSTAR'    '6' 'X' 'X' '' 'X',
             'LIFNR'    '7' 'X' 'X' '' 'X',
             'EKGRP'    '8' 'X' 'X' '' 'X',
             'INFRSN'   '9' 'X' 'X' '' 'X',
             'ZRCLSS'   '10' 'X' 'X' '' 'X'.

ENDFORM.                    " SORT_BUILD
