*----------------------------------------------------------------------*
***INCLUDE ZAPP221M_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  status_1203  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Commands with toggles.
*----------------------------------------------------------------------*
MODULE status_1203 OUTPUT.
  REFRESH: it_func_1203.

  IF g_toggle_1203 NE 'C' AND  g_toggle_1203  NE  'A'.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'SAVE'.
          screen-invisible = '1'.
        WHEN 'TOGGLE'.
          screen-invisible = '0'.
        WHEN 'ISRT'.
          screen-invisible = '0'.
        WHEN 'TRANS'.
          screen-invisible = '0'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
*    move  'SAVE'   to   IT_FUNC_1203-fcode.
*    append IT_FUNC_1203.
  ENDIF.
  IF g_toggle_1203 EQ  'A'.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'SAVE'.
          screen-invisible = '0'.
        WHEN 'TOGGLE'.
          screen-invisible = '1'.
        WHEN 'ISRT'.
          screen-invisible = '1'.
        WHEN 'TRANS'.
          screen-invisible = '1'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
*    move  'ISRT'     to   IT_FUNC_1203-fcode.
*    append IT_FUNC_1203.
*    move  'TOGGLE'   to   IT_FUNC_1203-fcode.
*    append IT_FUNC_1203.
*    move  'TRANS'    to   IT_FUNC_1203-fcode.
*    append IT_FUNC_1203.
  ENDIF.
  IF g_toggle_1203 EQ  'C'.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'SAVE'.
          screen-invisible = '0'.
        WHEN 'TOGGLE'.
          screen-invisible = '0'.
        WHEN 'ISRT'.
          screen-invisible = '1'.
        WHEN 'TRANS'.
          screen-invisible = '1'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
*    move  'ISRT'     to   IT_FUNC_1203-fcode.
*    append IT_FUNC_1203.
*    move  'TRANS'    to   IT_FUNC_1203-fcode.
*    append IT_FUNC_1203.
  ENDIF.

  set  pf-status  '1203'  excluding  IT_FUNC_1203.
  SET  TITLEBAR   '100'.
ENDMODULE.                 " status_1203  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_ATTRIBUTE_SET  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Screen's Field-Attributes with toggles.
*----------------------------------------------------------------------*
MODULE field_attribute_set_1203 OUTPUT.
  LOOP AT SCREEN.
    IF  g_toggle_1203  EQ  'C'.
      IF screen-group1 EQ 'UPD'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 EQ 'KEY'.
        screen-input = 0.
      ENDIF.
    ELSEIF  g_toggle_1203 EQ 'A'.
      IF screen-group1 EQ 'UPD' OR screen-group1 = 'KEY'.
        screen-input = 1.
      ENDIF.
    ELSE.
      IF screen-group1 EQ 'UPD'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    MODIFY  SCREEN.
  ENDLOOP.

ENDMODULE.                 " FIELD_ATTRIBUTE_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  intial_data_load  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters & Searching Initial Data
*----------------------------------------------------------------------*
MODULE intial_data_load_1203 OUTPUT.
  MOVE   'HMMA'    TO   g_company_1203.
  CHECK is_app221-nation IS  INITIAL.
  CHECK g_toggle_1203 IS INITIAL.

  SELECT * FROM  ztpp_nation_def
    INTO CORRESPONDING FIELDS OF TABLE it_app221.
  IF sy-subrc NE 0.
    MESSAGE w000 WITH 'Nation data Not found'.
    EXIT.
  ENDIF.
  LOOP AT  it_app221.
    MOVE  sy-tabix   TO  it_app221-seq.
    MODIFY it_app221.
  ENDLOOP.
  DESCRIBE TABLE it_app221  LINES  g_seq_1203.
  READ TABLE it_app221  INDEX  1.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING   it_app221   TO   is_app221.
  ENDIF.
ENDMODULE.                 " intial_data_load  OUTPUT
