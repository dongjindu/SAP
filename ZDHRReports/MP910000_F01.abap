*----------------------------------------------------------------------*
***INCLUDE MP910000_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  INIT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_data OUTPUT.
  PERFORM set_droplist_status.
ENDMODULE.                 " INIT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_droplist_status .
  DATA: gt_values             TYPE vrm_values,
      gs_value                LIKE LINE OF gt_values,
      g_fieldname             TYPE vrm_id.


  SELECT ELIGI AS key ZTEXT AS text
    INTO TABLE gt_values
    FROM ZTHR_ELIGI_CHECK.

  g_fieldname = 'P9100-ELIGI'.

*  CLEAR gt_values[].
*  gs_value-key = 'E'.
*  gs_value-text = 'Enrolled - Eligible'.
*  APPEND gs_value TO gt_values .
*
*  gs_value-key = 'S1'.
*  gs_value-text = 'Enrolled - Suspended 1'.
*  APPEND gs_value TO gt_values .
*
*  gs_value-key = 'S2'.
*  gs_value-text = 'Enrolled - Suspended 2'.
*  APPEND gs_value TO gt_values .
*
*  gs_value-key = 'SE'.
*  gs_value-text = 'Enrolled - Suspended EMER'.
*  APPEND gs_value TO gt_values .

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.

ENDFORM.                    " SET_DROPLIST_STATUS
