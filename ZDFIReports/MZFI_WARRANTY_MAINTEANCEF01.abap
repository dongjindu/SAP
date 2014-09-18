*----------------------------------------------------------------------*
***INCLUDE MZFI_WARRANTY_MAINTEANCE1F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  confirm_step
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM confirm_step CHANGING p_answer.

  CALL FUNCTION 'LC_POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1 = 'Data is Changed'
            textline2 = 'Do you want to continue ?'
            titel     = 'Confirm Step'
       IMPORTING
            answer    = p_answer.

ENDFORM.                    " confirm_step
