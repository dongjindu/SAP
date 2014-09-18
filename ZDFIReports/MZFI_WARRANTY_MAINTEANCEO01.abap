*----------------------------------------------------------------------*
***INCLUDE MZFI_WARRANTY_MAINTEANCEO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'MX'.
  SET TITLEBAR 'MX'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_initial_values  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_initial_values OUTPUT.
*refresh it_source.
*clear it_source.
  CLEAR :l_index. "c_flag .
* IF  sy-ucomm IS INITIAL.                                  "HIS20094
  IF  v_first IS INITIAL.                                   "HIS20094
    v_first = 'X'.                                          "HIS20094
    SELECT MAX( versn ) INTO ztfi_warranty-versn
       FROM ztfi_warranty.
    IF  ztfi_warranty-versn EQ '000'.
      ztfi_warranty-versn = '001'.
    ENDIF.
    PERFORM execute_query.                                  "HIS20094
  ENDIF.


*  if  ZTFI_WARRANTY-GJAHR is initial.
*    ZTFI_WARRANTY-GJAHR = sy-datum+2(2).
*  ENDIF.

*  name = 'ZTFI_WARRANTY-LAND1'.
*  free list.
*
*  MOVE: 'US'      TO value-key,
*        'America'  TO value-text.
*  APPEND value     TO list.
*
*  MOVE: 'CA'    TO value-key,
*        'Canada' TO value-text.
*  APPEND value   TO list.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*       EXPORTING
*            id     = name
*            values = list.

* BEGIN HIS20094
  DESCRIBE TABLE it_source LINES tc1-lines.
*  tc1-lines = 15.
* END HIS20094
  tc1-v_scroll = 'X'.


ENDMODULE.                 " set_initial_values  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  move_data_to_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_data_to_screen OUTPUT.
  READ TABLE it_source INDEX tc1-current_line.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_source TO ztfi_warranty_str.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.
ENDMODULE.                 " move_data_to_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_screen_display  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen_display OUTPUT.

* During Execute Make all fields display only.
*  IF   sy-ucomm EQ 'EXEC' OR sy-ucomm EQ 'DEL'.            "HIS20094
  IF v_change IS INITIAL.                                   "HIS20094
    LOOP AT SCREEN.
      IF screen-group1 = 'N1'.
        screen-input = 0.
        screen-output = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'N2'.                              "HIS20094
        screen-input = 1.                                   "HIS20094
        screen-output = 1.                                  "HIS20094
        MODIFY SCREEN.                                      "HIS20094
      ENDIF.                                                "HIS20094
    ENDLOOP.
  ELSE.                                                     "HIS20094
*  ENDIF.                                                   "HIS20094
* During change - Allow for values to be changed
*  IF   sy-ucomm EQ 'CHG'.                                  "HIS20094
    LOOP AT SCREEN.
      IF screen-group1 = 'N1'.
        screen-input = 1.
        screen-output = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'N2'.                              "HIS20094
        screen-input = 0.                                   "HIS20094
        screen-output = 1.                                  "HIS20094
        MODIFY SCREEN.                                      "HIS20094
      ENDIF.                                                "HIS20094
    ENDLOOP.
  ENDIF.

* BEGIN HIS20094
  CLEAR: v_emsg, sy-ucomm.
* END HIS20094

ENDMODULE.                 " set_screen_display  OUTPUT
