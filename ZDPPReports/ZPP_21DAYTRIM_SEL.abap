*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST001_SEL                                            *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST029_SEL                                            *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS S_RSNUM FOR ZTPP_INPUT_PLAN-RSNUM ."NO INTERVALS.
PARAMETERS: P_LPLAN AS CHECKBOX,
            P_DISP  AS CHECKBOX,
            P_SEND  AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B1.


INITIALIZATION.
  S_RSNUM-LOW = SY-DATUM.
  S_RSNUM-HIGH = SY-DATUM + 21.
  S_RSNUM-SIGN = 'I'.
  S_RSNUM-OPTION = 'BT'.
  APPEND S_RSNUM.
