************************************************************************
* Program Name      : ZAPP920R_BACKFLUSH_PLANORDER
* Author            : BOBBY
* Creation Date     : 2003.09.03.
* Specifications By : JongOh, Kim
* Pattern           : 1.1
* Development Request No : UD1K901950
* Addl Documentation:
* Description       : Call the BACKFLUSH Function for PlanOrder Create
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP920R_BACKFLUSH_PLANORDER NO STANDARD PAGE HEADING
                                    LINE-SIZE 200
                                    MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS : p_PORDER LIKE  PLAF-PLNUM .
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM call_BF_function.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM list_process.

*&---------------------------------------------------------------------*
*&      Form  CALL_BF_FUNCTION
*&---------------------------------------------------------------------*
FORM call_bf_function.
  CALL FUNCTION 'Z_FPP_BAPI_PLAN_ORDER_CHANGE'
    EXPORTING
      l_plnum       = P_PORDER .
ENDFORM.                    " CALL_BF_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.

ENDFORM.                    " LIST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.

ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.

ENDFORM.                    " INITIALIZATION
