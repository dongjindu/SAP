*----------------------------------------------------------------------*
*   INCLUDE MZAHR0007O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.
*
  w_chos1 = 'Input Screen for Personal'.
  w_chos2 = 'Screen for download'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9100 OUTPUT.
  SET PF-STATUS 'PS9100'.
  SET TITLEBAR '910'.

ENDMODULE.                 " STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE modify_screen_9100 OUTPUT.
  IF w_status = 'I'." OR w_status = space.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
*      screen-input = 1.
*      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF w_status = 'U'.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF w_status = space.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9200 OUTPUT.
  SET PF-STATUS 'PS9200'.
  SET TITLEBAR '920'.
ENDMODULE.                 " STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9110  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9110 OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  SET PF-STATUS space.
  SET TITLEBAR '911'.
ENDMODULE.                 " STATUS_9110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_NEW_LINE  OUTPUT
*&---------------------------------------------------------------------*
MODULE modify_new_line OUTPUT.

ENDMODULE.                 " MODIFY_NEW_LINE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_value OUTPUT.
  IF w_zyear = '' .
    MOVE : sy-datum(4) TO w_zyear.
  ENDIF.
  IF w_zmons = '' .
    MOVE : sy-datum+4(2) TO w_zmons.
  ENDIF.
*  IF w_status IS INITIAL.
*    w_status = 'I'.
*    REFRESH it_et03.
*  ENDIF.
ENDMODULE.                 " INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initial_value_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_value_9200 OUTPUT.
  IF w_zyear = '' .
    MOVE : sy-datum(4) TO w_zyear.
  ENDIF.
  IF w_zmons = '' .
    MOVE : sy-datum+4(2) TO w_zmons.
  ENDIF.
  IF w_status IS INITIAL.
    w_status = 'I'.
  ENDIF.

ENDMODULE.                 " initial_value_9200  OUTPUT
