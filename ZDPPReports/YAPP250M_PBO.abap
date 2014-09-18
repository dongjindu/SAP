*----------------------------------------------------------------------*
***INCLUDE YAPP250M_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PROGRESS_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE progress_init OUTPUT.

  CHECK  g_init_skip  NE  'Y'.

  REFRESH: list, list1.

  SELECT status progress vmrp
    INTO CORRESPONDING FIELDS OF TABLE it_prog
    FROM ztpp_process
    WHERE vmrp <> ''.

  LOOP AT it_prog.
    value-key  = it_prog-status.
    value-text = it_prog-progress.
    APPEND value TO list.
  ENDLOOP.

  name = 'KEY-STATS'.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_model
    FROM ztpp_veh_model.

  LOOP AT it_model.
    value1-key  = it_model-model.
    value1-text = it_model-name.
    APPEND value1 TO list1.
  ENDLOOP.

  name1 = 'KEY-MODEL'.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name1
            values = list1.

  key-company = 'HMMA'.
  g_init_skip = 'Y'.

ENDMODULE.                 " PROGRESS_INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
