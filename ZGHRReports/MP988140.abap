*----------------------------------------------------------------------*
*                                                                      *
*       Subroutines for infotype 9881                                  *
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_master .
  SELECT objid stext FROM zghrlt0004
    INTO CORRESPONDING FIELDS OF TABLE gt_master
    WHERE begda <= sy-datum
    AND   endda >= sy-datum.
ENDFORM.                    " GET_MASTER
*&---------------------------------------------------------------------*
*&      Form  SAVE_TRAINING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_training .

*  IF p9881-begda = sy-datum OR p9881-endda ='9999-12-31'.
*    MESSAGE 'Date check' TYPE 'E'.
*  ENDIF.
  IF p9881-thist IS INITIAL AND NOT gt_history-objid IS INITIAL.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = p9881-thist.
  ENDIF.

  DATA: ls_zghrlt0002   TYPE zghrlt0002.

  DELETE FROM zghrlt0002 WHERE tabnr = p9881-thist.

  LOOP AT gt_history WHERE NOT objid IS INITIAL.
    CLEAR: ls_zghrlt0002.
    ls_zghrlt0002-tabnr = p9881-thist.
    ls_zghrlt0002-objid = gt_history-objid.
    ls_zghrlt0002-begda = gt_history-begda.
    ls_zghrlt0002-endda = gt_history-endda.

    ls_zghrlt0002-aedtm = sy-datum.
    ls_zghrlt0002-uname = sy-uname.

    INSERT zghrlt0002 FROM ls_zghrlt0002.

  ENDLOOP.

  IF p9881-tplan IS INITIAL AND NOT gt_plan-objid IS INITIAL.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = p9881-tplan.
  ENDIF.

  DATA: ls_zghrlt0003   TYPE zghrlt0003.

  DELETE FROM zghrlt0003 WHERE tabnr = p9881-tplan.

  LOOP AT gt_plan WHERE NOT objid IS INITIAL.
    CLEAR: ls_zghrlt0003.
    ls_zghrlt0003-tabnr = p9881-tplan.
    ls_zghrlt0003-objid = gt_plan-objid.
    ls_zghrlt0003-tyear = gt_plan-tyear.

    ls_zghrlt0003-aedtm = sy-datum.
    ls_zghrlt0003-uname = sy-uname.

    INSERT zghrlt0003 FROM ls_zghrlt0003.
  ENDLOOP.

ENDFORM.                    " SAVE_TRAINING
*&---------------------------------------------------------------------*
*&      Form  DELE_TRAINING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dele_training .
  IF NOT p9881-thist IS INITIAL.
    DELETE FROM zghrlt0002 WHERE tabnr = p9881-thist.
  ENDIF.

  IF NOT p9881-tplan IS INITIAL.
    DELETE FROM zghrlt0003 WHERE tabnr = p9881-tplan.
  ENDIF.

ENDFORM.                    " DELE_TRAINING
*&---------------------------------------------------------------------*
*&      Form  CHECK_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_status .
  DATA: ls_pa0000       TYPE p0000.

  CLEAR ls_pa0000.
  SELECT SINGLE * FROM pa0000
    INTO ls_pa0000
    WHERE pernr = p9881-pernr
    AND   endda => p9881-begda
    AND   begda <= p9881-endda.

  IF sy-subrc = 0.
    IF ls_pa0000-stat2 CA '02'.
      MESSAGE e007(zghrm).
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_STATUS
