*----------------------------------------------------------------------*
*   INCLUDE YAPP223L_PBO                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters
*----------------------------------------------------------------------*
MODULE initialization_1205 OUTPUT.
  IF WA_INIT_1205 <> 'X'.
*   Company
    p_company = 'HMMA'.

*   Model(Car Type)
    CLEAR: model_list, model_value.
    name = 'P_MODEL'.
    PERFORM set_field_model.
    PERFORM call_function_vrm USING model_list.

*   Part(U or C)
    CLEAR: part_list, part_value.
    name = 'P_PART'.
    PERFORM set_field_PART.
    PERFORM call_function_VRM USING part_list.

*   KEY(1, ... , 200)
    CLEAR: key_list, key_value.
    name = 'P_KEY'.
    PERFORM set_field_KEY.
    PERFORM call_function_VRM USING key_list.
    WA_INIT_1205 = 'X'.
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Internal Table's total Line Amount
*----------------------------------------------------------------------*
MODULE modify_lines_1205 OUTPUT.
  DATA: l_line TYPE i.
  DESCRIBE TABLE IT_APP223 LINES l_line.
  TC_APP223-lines = l_line.

ENDMODULE.                 " modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Srceen's Attributes.
*----------------------------------------------------------------------*
MODULE modify_screen_1205 OUTPUT.
  LOOP AT SCREEN.
    IF ( wa_upd_1205 = 'X'       AND
         IT_APP223-mark = 'X'   AND
         screen-group1 <> 'GRP' ) OR
       screen-name = 'IT_APP223-MARK' .
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1206  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status
*----------------------------------------------------------------------*
MODULE status_1206 OUTPUT.
*  SET PF-STATUS 'STATUS111'.
  SET TITLEBAR '1206'.

ENDMODULE.                 " STATUS_1206  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  initialization02  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameter - Directory & Init-Flag.
*----------------------------------------------------------------------*
MODULE initialization_1206 OUTPUT.
  IF WA_INIT_1206 IS INITIAL.
    CLEAR IT_APP223_NEW.
    REFRESH IT_APP223_NEW.
    p_f_name = 'C:\SAPworkdir\alc.txt' .
    p_f_type = 'DAT' .
    WA_INIT_1206 = 'X'.
  ENDIF.
ENDMODULE.                 " initialization02  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  setting_TC_APP223_NEW  OUTPUT
*&---------------------------------------------------------------------*
*       Setting The Total Line Amount.
*----------------------------------------------------------------------*
MODULE setting_TC_APP223_NEW OUTPUT.
  DATA wa_line TYPE i.
*
  DESCRIBE TABLE IT_APP223_NEW LINES wa_line.
  TC_APP223_NEW-lines = wa_line.

ENDMODULE.                 " setting_TC_APP223_NEW  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_111  OUTPUT
*&---------------------------------------------------------------------*
*       Modification of Screen
*----------------------------------------------------------------------*
MODULE modify_screen_1206 OUTPUT.

ENDMODULE.                 " modify_screen_111  OUTPUT
