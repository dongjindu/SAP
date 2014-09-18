*----------------------------------------------------------------------*
*                                                                      *
*       Output-modules for infotype 9881                               *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       MODULE  P9881 OUTPUT                                           *
*----------------------------------------------------------------------*
*       Default values, Texts                                          *
*----------------------------------------------------------------------*
MODULE p9881 OUTPUT.

  DATA: lv_cnt     TYPE i.

  IF psyst-nselc EQ yes.
* read text fields etc.; do this whenever the screen is show for the
*  first time:
*   PERFORM RExxxx.

    CLEAR: gt_history, gt_history[].
    IF p9881-thist NE ''.
      SELECT * FROM zghrlt0002
        INTO CORRESPONDING FIELDS OF TABLE gt_history
        WHERE tabnr = p9881-thist.
    ENDIF.

    CLEAR: gt_plan, gt_plan[].
    IF p9881-tplan NE ''.
      SELECT * FROM zghrlt0003
        INTO CORRESPONDING FIELDS OF TABLE gt_plan
        WHERE tabnr = p9881-tplan.
    ENDIF.

    IF psyst-iinit = yes AND psyst-ioper = insert.
* generate default values; do this the very first time on insert only:
*     PERFORM GET_DEFAULT.
    ENDIF.

    IF psyst-iinit = yes AND psyst-ioper = copy.
      IF NOT p9881-thist IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
             IMPORTING
                  ev_guid_32 = p9881-thist.
        gt_history-tabnr = p9881-thist.
        MODIFY gt_history TRANSPORTING tabnr WHERE tabnr <> ''.
      ENDIF.
      IF NOT p9881-tplan IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
             IMPORTING
                  ev_guid_32 = p9881-tplan.
        gt_plan-tabnr = p9881-tplan.
        MODIFY gt_plan TRANSPORTING tabnr WHERE tabnr <> ''.
      ENDIF.
    ENDIF.

  ENDIF.

  IF psyst-iinit = yes AND
  ( psyst-ioper = 'INS' OR psyst-ioper = 'MOD' ).
    CLEAR lv_cnt.
    DESCRIBE TABLE gt_history LINES lv_cnt.
    IF lv_cnt = 0.
      APPEND INITIAL LINE TO gt_history.
    ENDIF.
    CLEAR lv_cnt.
    DESCRIBE TABLE gt_plan LINES lv_cnt.
    IF lv_cnt = 0.
      APPEND INITIAL LINE TO gt_plan.
    ENDIF.
  ENDIF.

  PERFORM get_master.

  DESCRIBE TABLE gt_history LINES tc_history-lines.
  DESCRIBE TABLE gt_plan LINES tc_plan-lines.
*  tc_history-lines = LINES( gt_history ).
*  tc_plan-lines = LINES( gt_plan ).


ENDMODULE.                    "P9881 OUTPUT
*----------------------------------------------------------------------*
*       MODULE  P9881L OUTPUT                                          *
*----------------------------------------------------------------------*
*       read texts for listscreen
*----------------------------------------------------------------------*
MODULE p9881l OUTPUT.
* PERFORM RExxxx.
ENDMODULE.                    "P9881L OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_HISTORY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_history OUTPUT.
  READ TABLE gt_history INDEX tc_history-current_line.

  IF NOT gt_history-objid IS INITIAL.
    SELECT SINGLE stext FROM zghrlt0004 INTO gt_history-stext
      WHERE objid = gt_history-objid
      AND   endda >= sy-datum
      AND   begda <= sy-datum.
  ENDIF.

ENDMODULE.                 " DISPLAY_HISTORY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_PLAN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_plan OUTPUT.
  READ TABLE gt_plan INDEX tc_plan-current_line.

  IF NOT gt_plan-objid IS INITIAL.
    SELECT SINGLE stext FROM zghrlt0004 INTO gt_plan-stext
      WHERE objid = gt_plan-objid
      AND   endda >= sy-datum
      AND   begda <= sy-datum.
  ENDIF.

ENDMODULE.                 " DISPLAY_PLAN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_LAYOUT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_layout OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = '006'.
      CASE psyst-ioper.
        WHEN 'INS' OR 'MOD' OR copy.
          screen-input = 1.
        WHEN OTHERS.
          screen-input = 0.
      ENDCASE.

    ENDIF.

*    find '-MARK' in screen-name.
    SEARCH screen-name FOR '-MARK'.
    IF sy-subrc = 0.
      IF psyst-ioper = 'INS' OR psyst-ioper = 'MOD' OR psyst-ioper =
copy.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " SET_LAYOUT  OUTPUT
**&---------------------------------------------------------------------
**
**&      Module  GET_HISTORY  OUTPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE get_history OUTPUT.
*  SELECT a~objid b~stext a~begda a~endda
*    FROM zghrlt0002 AS a INNER JOIN zghrlt0004 AS b ON a~objid =
*b~objid
*    INTO TABLE gt_history.
*
*ENDMODULE.                 " GET_HISTORY  OUTPUT
**&---------------------------------------------------------------------
**
**&      Module  GET_PLAN  OUTPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE get_plan OUTPUT.
*  SELECT a~objid b~stext a~tyear
*    FROM zghrlt0003 AS a INNER JOIN zghrlt0004 AS b ON a~objid =
*b~objid
*    INTO TABLE gt_plan.
*
*ENDMODULE.                 " GET_PLAN  OUTPUT
