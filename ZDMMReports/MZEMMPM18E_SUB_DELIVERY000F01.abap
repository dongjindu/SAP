************************************************************************
* Program name : SAPMZEMMPM18E_SUB_DELIVERY000
* Created by   : Min-su Park
* Created on   : 2003.08.29.
* Pattern      :
* Description  : Sub-Daily Delivery Schedule z-Table
*
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.08.29.     Min-su Park      UD1K901873     Initial Coding       *
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM18E_SUB_DELIVERY000F01                              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MATNR_EXISTENCE_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM matnr_existence_chk USING subrc.
  SELECT SINGLE *
           FROM ztmm_delisch
          WHERE matnr = it_ztmm_delisch-matnr
            AND lifnr = it_ztmm_delisch-lifnr.
  IF sy-subrc = 0.
    subrc = 5.
  ENDIF.
ENDFORM.                    " MATNR_EXISTENCE_CHK
*&---------------------------------------------------------------------*
*&      Form  GET_DESCRIPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_description.
  SELECT SINGLE maktx
           INTO it_ztmm_delisch-maktx
           FROM makt
          WHERE matnr = it_ztmm_delisch-matnr.
  SELECT SINGLE name1
           INTO it_ztmm_delisch-name1
           FROM lfa1
          WHERE lifnr = it_ztmm_delisch-lifnr.
ENDFORM.                    " GET_DESCRIPTION
*&---------------------------------------------------------------------*
*&      Form  CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create.
  CLEAR :  w_firstchk_flg .
*Set Creation mode
  w_status = 'N'.
ENDFORM.                    " CREATE
*&---------------------------------------------------------------------*
*&      Form  CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change.
  SELECT * FROM ztmm_delisch
           INTO CORRESPONDING FIELDS OF TABLE it_ztmm_delisch.
  DESCRIBE TABLE it_ztmm_delisch LINES tc_ztmm_delisch-lines.
*Set Change mode
  w_status = 'C'.
ENDFORM.                    " CHANGE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display.
  SELECT * FROM ztmm_delisch
           INTO CORRESPONDING FIELDS OF TABLE it_ztmm_delisch.
  DESCRIBE TABLE it_ztmm_delisch LINES tc_ztmm_delisch-lines.
*Set Display mode
  w_status = 'D'.
ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.
  DATA : tmp LIKE ztmm_delisch.

  LOOP AT it_ztmm_delisch.
    CLEAR tmp.
    MOVE-CORRESPONDING it_ztmm_delisch TO tmp.
    IF tmp-time1  = space . CLEAR tmp-time1 . ENDIF.
    IF tmp-time2  = space . CLEAR tmp-time2 . ENDIF.
    IF tmp-time3  = space . CLEAR tmp-time3 . ENDIF.
    IF tmp-time4  = space . CLEAR tmp-time4 . ENDIF.
    IF tmp-time5  = space . CLEAR tmp-time5 . ENDIF.
    IF tmp-time6  = space . CLEAR tmp-time6 . ENDIF.
    IF tmp-time7  = space . CLEAR tmp-time7 . ENDIF.
    IF tmp-time8  = space . CLEAR tmp-time8 . ENDIF.
    IF tmp-time9  = space . CLEAR tmp-time9 . ENDIF.
    IF tmp-time10 = space . CLEAR tmp-time10. ENDIF.

    UPDATE ztmm_delisch FROM tmp.
    IF sy-subrc <> 0.
      INSERT ztmm_delisch FROM tmp.
    ENDIF.
  ENDLOOP.
*  ENDIF.
ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete.
  READ TABLE it_ztmm_delisch WITH KEY mark = 'X'.
  IF sy-subrc <> 0.
    MESSAGE e001.
  ELSE.
    LOOP AT it_ztmm_delisch WHERE mark = 'X'.
      DELETE FROM ztmm_delisch WHERE matnr = it_ztmm_delisch-matnr
                                 AND lifnr = it_ztmm_delisch-lifnr.
    ENDLOOP.
    DELETE it_ztmm_delisch WHERE mark = 'X'.
  ENDIF.
ENDFORM.                    " DELETE
*&---------------------------------------------------------------------*
*&      Form  SELALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selall.
  it_ztmm_delisch-mark = 'X'.
  MODIFY it_ztmm_delisch TRANSPORTING mark WHERE mark <> 'X'.
ENDFORM.                    " SELALL
*&---------------------------------------------------------------------*
*&      Form  DESELALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deselall.
  CLEAR it_ztmm_delisch-mark.
  MODIFY it_ztmm_delisch TRANSPORTING mark WHERE mark = 'X'.
ENDFORM.                    " DESELALL
*&---------------------------------------------------------------------*
*&      Form  PAGE_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_control.
  CALL FUNCTION 'SCROLLING_IN_TABLE'
       EXPORTING
            entry_act             = tc_ztmm_delisch-top_line
            entry_to              = tc_ztmm_delisch-lines
            last_page_full        = ' '
            loops                 = w_loopc
            ok_code               = w_fcode
            overlapping           = 'X'
       IMPORTING
            entry_new             = tc_ztmm_delisch-top_line
       EXCEPTIONS
            no_entry_or_page_act  = 1
            no_entry_to           = 2
            no_ok_code_or_page_go = 3
            OTHERS                = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " PAGE_CONTROL
*&---------------------------------------------------------------------*
*&      Form  CLEAR_0_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_0_time.
  FIELD-SYMBOLS : <fs>.
  DATA          : field_name(22), idx, idx10(2).
*Clear Delivery Times.
  LOOP AT it_ztmm_delisch.
    DO 24 TIMES.
      IF sy-index GE 10.
        idx10 = sy-index.
        CONCATENATE 'IT_ZTMM_DELISCH-TIME' idx10 INTO field_name.
        ASSIGN (field_name) TO <fs>.
        IF <fs> IS INITIAL.
          <fs> = '        '.
        ENDIF.
      ELSE.
        idx = sy-index.
        CONCATENATE 'IT_ZTMM_DELISCH-TIME' idx INTO field_name.
        ASSIGN (field_name) TO <fs>.
        IF <fs> IS INITIAL.
          <fs> = '        '.
        ENDIF.
      ENDIF.
    ENDDO.
    MODIFY it_ztmm_delisch.
  ENDLOOP.
ENDFORM.                    " CLEAR_0_TIME

*&---------------------------------------------------------------------*
*&      Form  copy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy.
*---
  CLEAR : wa_ztmm_delisch, it_ztmm_delisch.

  READ TABLE it_ztmm_delisch WITH KEY mark = 'X'.

  MOVE-CORRESPONDING it_ztmm_delisch TO wa_ztmm_delisch.

  CALL SCREEN 900 STARTING AT  5  5
                  ENDING   AT 65 20.
ENDFORM.                    " copy

*&---------------------------------------------------------------------*
*&      Form  adopt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM adopt.
*---
  MOVE-CORRESPONDING wa_ztmm_delisch TO it_ztmm_delisch.

  MODIFY it_ztmm_delisch TRANSPORTING
                                 time1  time2  time3  time4  time5
                                 time6  time7  time8  time9  time10
                                 time11 time12 time13 time14 time15
                                 time16 time17 time18 time19 time20
                                 time21 time22 time23 time24
                          WHERE lifnr EQ wa_ztmm_delisch-lifnr.
ENDFORM.                    " adopt
