*----------------------------------------------------------------------*
*   INCLUDE MZAHR0001O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  CASE W_MODES.
    WHEN 'D'. SET PF-STATUS 'PS9000' EXCLUDING 'SAVE'.
    WHEN 'M'. SET PF-STATUS 'PS9000'.
  ENDCASE.
  SET TITLEBAR '900'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_SCREEN OUTPUT.
  IF W_FLAGS = ' '.
    CLEAR IT_WTMNG. REFRESH IT_WTMNG.
    CLEAR TMP_WTMNG. REFRESH TMP_WTMNG.
    CLEAR DEL_WTMNG. REFRESH DEL_WTMNG.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_WTMNG
      FROM ZTHR_WTMNG WHERE ZFORM NE SPACE.
    IT_WTMNG-STATUS = 'M'.
    MODIFY IT_WTMNG TRANSPORTING STATUS WHERE STATUS = SPACE.
    TMP_WTMNG[] = IT_WTMNG[].
    SORT: IT_WTMNG BY ZFORM ZCOLN LGART,
          TMP_WTMNG BY ZFORM ZCOLN LGART.

    REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
    DESCRIBE TABLE IT_WTMNG LINES TC9000-LINES.
    W_FLAGS = 'X'.
  ENDIF.
ENDMODULE.                 " INIT_SCREEN  OUTPUT
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
*&      Module  MODIFY_SCREEN_FOR_PARENT_WT  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_FOR_PARENT_WT OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'GR1'.
      CASE W_MODES.
        WHEN 'D'. SCREEN-INPUT = 0.
        WHEN 'M'. SCREEN-INPUT = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*
  IF IT_WTMNG-ZPRNT NE SPACE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP2 = 'GR2'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN_FOR_PARENT_WT  OUTPUT
