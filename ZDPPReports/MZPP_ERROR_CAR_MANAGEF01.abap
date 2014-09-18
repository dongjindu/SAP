*----------------------------------------------------------------------*
*   INCLUDE MZPP_APP601F01                                             *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_exit.
  IF wa_change = 'X'.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
         EXPORTING
              textline1 = 'Data has been changed.'
              textline2 = 'Save data?'
              titel     = 'Save Confirm'
         IMPORTING
              answer    = wa_answer.

    CASE  wa_answer    .
      WHEN  'J'.
        PERFORM  save_data .
        wa_end   = 'X'     .
      WHEN  'N'.
        wa_end   = 'X'     .
      WHEN  'A'.
    ENDCASE.
  ELSE.
    wa_end = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_EXIT

*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  READ TABLE it_car WITH KEY mark = 'X'.
  it_del = it_car.
  APPEND it_del.
  DELETE it_ins WHERE err_veh = it_del-err_veh .
  DELETE it_car WHERE err_veh = it_del-err_veh .
ENDFORM.                    " DELETE_DATA

*&---------------------------------------------------------------------*
*&      Form  INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_data.
  it_ins = it_car.
  APPEND it_ins.
  DELETE it_del WHERE err_veh = it_ins-err_veh .
ENDFORM.                    " INSERT_DATA

*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  CASE sy-dynnr.
    WHEN '1000'.
      APPEND it_car .
      it_ins = it_car.  APPEND it_ins .
      DELETE it_del WHERE err_veh = it_car-err_veh.
    WHEN OTHERS.
      DELETE ztpp_error_car FROM TABLE it_del.
      MODIFY ztpp_error_car FROM TABLE it_car.
      IF SY-SUBRC EQ 0.
         COMMIT WORK.
      ENDIF.
  ENDCASE.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_data.
  CLEAR: it_car, it_car[].
  SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_car
   FROM ztpp_error_car  .
ENDFORM.                    " REFRESH_DATA
