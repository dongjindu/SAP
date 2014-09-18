*----------------------------------------------------------------------*
***INCLUDE MZAPP903M_INPUT_PLANF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data  using  pa_flag.
  data: l_answer              type c.

  " POP-UP Window Execution.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      DEFAULTOPTION        = 'Y'
      textline1            = text-003
      TEXTLINE2            = text-004
      titel                = text-005
      START_COLUMN         = 25
      START_ROW            = 6
      CANCEL_DISPLAY       = 'X'
    IMPORTING
      ANSWER               = l_answer.

  clear: wa_change.
  case l_answer.
     when 'J' or 'Y'.   " Save data to Z-Table & Change Vehicle Master.
     when 'N'       .   " No
     when 'A'       .   " Cancel
       pa_flag   = 'X'.
       wa_change = 'X'.
  endcase.
ENDFORM.                    " SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form  RECALC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalc_data.
  Check wa_change = 'X'.
  " Change the Table ZTPP_INPUT_PLAN
  perform save_data    using  l_flag .
  " Change the Value in the Vehicle Master..
  " Run the Report ZAPP903R_INPUT_PLAN
  PERFORM run_zapp903r .
  "
ENDFORM.                    " RECALC_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_BLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_block.
* Check the Block Start point...
  IF wa_block  = space .           " Start point Setting.
    READ TABLE it_data WITH KEY mark = 'X'.
    wa_sblock = sy-tabix.
    wa_block  = 'X'     .
  ELSE.
    READ TABLE it_data WITH KEY mark = 'X'.
    wa_sblock = sy-tabix.
    LOOP AT it_data WHERE mark = 'X'.
      wa_eblock = sy-tabix          .
      IF wa_eblock < wa_sblock.
        wa_eblock = wa_sblock.
        wa_sblock = sy-tabix .
      ENDIF.
    ENDLOOP.
    LOOP AT it_data FROM wa_sblock TO wa_eblock.
      it_data-mark = 'X'.
      MODIFY it_data.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SET_BLOCK

*&---------------------------------------------------------------------*
*&      Form  move_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_data.
* Check the Rule...
* 1st : Unmatch the status information on the Z-Table & Vehicle Master
* 2st : Unmatch the information for the APS Transportation..
* 3st : Unmatch the Vehicle Masters Information..

  DATA: l_error.

  IF wa_block = space.
    PERFORM check_moving  USING l_error.
    IF l_error = 'X'.
      MESSAGE i001(zmpp) WITH text-002.
      EXIT.
    ENDIF.
  ENDIF.
  wa_change = 'X'.
  CALL SCREEN 9001 STARTING AT 10 10 ENDING AT 60 15 .
  CLEAR: wa_block, wa_sblock, wa_eblock, it_data-mark.
  MODIFY it_data TRANSPORTING mark WHERE mark = 'X'.
ENDFORM.                    " move_data

*&---------------------------------------------------------------------*
*&      Form  check_point
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ERROR  text
*----------------------------------------------------------------------*
FORM check_point USING    pa_error.
  DATA: l_max                    LIKE sy-tabix.

  DESCRIBE TABLE it_data  LINES  l_max.
  IF wa_point = 0 OR wa_point > l_max OR
   ( wa_point >= wa_sblock AND wa_point <= wa_eblock ) .
    pa_error = 'X'.
  ENDIF.
ENDFORM.                    " check_point

*&---------------------------------------------------------------------*
*&      Form  check_moving
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ERROR  text
*----------------------------------------------------------------------*
FORM check_moving USING    pa_error.
  READ TABLE it_data WITH KEY mark = 'X'.
  IF sy-subrc = 0. wa_sblock = sy-tabix. ENDIF.
  LOOP AT it_data WHERE mark = 'X'.
    wa_block = 'X'.
    wa_eblock = sy-tabix.
  ENDLOOP.
  IF wa_block = space.  pa_error = 'X'. ENDIF.
ENDFORM.                    " check_moving

*&---------------------------------------------------------------------*
*&      Form  run_zapp903r
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM run_zapp903r.
  ranges: s_arbpls         for crhd-arbpl.

  s_arbpls = 'IBTB' .  s_arbpls-high = 'T'.  APPEND s_arbpls.
  submit ZAPP903R_INPUT_PLAN and return
    with p_sctype = '2'
    with p_plnnr  = 'RP'
    with p_arbpll = '1'
    with s_arbpls in s_arbpls
    with p_ptype  = 'N'  .
ENDFORM.                    " run_zapp903r

*&---------------------------------------------------------------------*
*&      Form  check_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_save.
  data: l_flag.

  check wa_change = 'X'.
  " POP-UP Window Execution.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      DEFAULTOPTION        = 'Y'
      textline1            = text-003
      TEXTLINE2            = text-004
      titel                = text-005
      START_COLUMN         = 25
      START_ROW            = 6
      CANCEL_DISPLAY       = 'X'
    IMPORTING
      ANSWER               = l_flag.

  clear: wa_change.
  case l_flag.
     when 'J' or 'Y'.   " Save data to Z-Table & Change Vehicle Master.
     when 'N'       .   " No
     when 'A'       .   " Cancel
       wa_change = 'X'.
  endcase.
ENDFORM.                    " check_save

*&---------------------------------------------------------------------*
*&      Form  save_index
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_index.
  loop at it_data.
    it_data-tabix = sy-tabix.
    modify it_data.
  endloop.
ENDFORM.                    " save_index
