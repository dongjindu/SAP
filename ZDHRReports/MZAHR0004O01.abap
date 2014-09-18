*----------------------------------------------------------------------*
*   INCLUDE MZAHR0004O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.
*
  W_OTYPE = 'O'.
  W_WEGID = 'ORGEH'.
  W_PLVAR = '01'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'GR1'.
      CASE W_MODES.
        WHEN 'D'. SCREEN-INPUT = 0.
        WHEN 'M'. SCREEN-INPUT = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS 'PS9100'.
  SET TITLEBAR '910'.
ENDMODULE.                 " STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE INIT_SCREEN OUTPUT.
  IF W_ZINIT = ' '.
    CLEAR: IT_H1001. REFRESH: IT_H1001.
    CLEAR: IT_D1001. REFRESH: IT_D1001.
    PERFORM SELECT_CBO_DATA.

    REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
    DESCRIBE TABLE IT_H1001 LINES TC9000-LINES.

    W_ZINIT = 'X'.
  ENDIF.
ENDMODULE.                 " INIT_SCREEN  OUTPUT
