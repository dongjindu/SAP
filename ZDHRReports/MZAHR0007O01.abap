*----------------------------------------------------------------------*
*   INCLUDE MZAHR0007O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.
*
  W_CHOS1 = 'Head Count Plan by Cost Center'.
  W_CHOS2 = 'Monthly Total Time Sheet by Cost Center'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS 'PS9100'.
  SET TITLEBAR '910'.

ENDMODULE.                 " STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9100 OUTPUT.
  IF W_FLAGS = SPACE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

    IF W_MASTER = ''.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR4'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9200 OUTPUT.
  SET PF-STATUS 'PS9200'.
  SET TITLEBAR '920'.
ENDMODULE.                 " STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  READ_IT_PCP05  OUTPUT
*&---------------------------------------------------------------------*
MODULE READ_IT_PCP05 OUTPUT.
  W_LOPLN = SY-LOOPC.
  W_INDEX = W_TOPLN + SY-STEPL - 1.
  READ TABLE IT_PCP05 INDEX W_INDEX.
ENDMODULE.                 " READ_IT_PCP05  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9110  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9110 OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
*
  SET PF-STATUS SPACE.
  SET TITLEBAR '911'.
*
  PERFORM SET_MONTH_STATUS.
ENDMODULE.                 " STATUS_9110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_NEW_LINE  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_NEW_LINE OUTPUT.
  IF IT_AHC01-STATS = 'N'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP2 = 'GR2'.
        SCREEN-INPUT = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " MODIFY_NEW_LINE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9210  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9210 OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
*
  SET PF-STATUS SPACE.
  SET TITLEBAR '911'.
*
  PERFORM SET_MONTH_STATUS_XX.
ENDMODULE.                 " STATUS_9210  OUTPUT
             " SCREEN_ATR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9200 OUTPUT.

ENDMODULE.                 " MODIFY_SCREEN_9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE OUTPUT.
  IF W_ZYEAR = '' OR W_ZVERS = ''.
    CLEAR ZTHR_PCP02.

    SELECT SINGLE * FROM ZTHR_PCP02
        WHERE ZMODL = '02'
          AND ZGRUP = '1030'
          AND ZVAL4 <> ''.
    MOVE : ZTHR_PCP02-ZVAL2 TO W_ZYEAR,
           ZTHR_PCP02-ZVAL1 TO W_ZVERS.
  ENDIF.

ENDMODULE.                 " INITIAL_VALUE  OUTPUT
