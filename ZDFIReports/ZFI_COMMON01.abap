*&---------------------------------------------------------------------*
*&  Include           ZFI_COMMON01
*&---------------------------------------------------------------------*
TYPE-POOLS:
  icon,
  vrm.

CONSTANTS:
  c_plvar TYPE hrp1000-plvar  VALUE '01',
  c_endda TYPE datum  VALUE '99991231',

  c_on    VALUE 1,
  c_off   VALUE 0,

  c_display VALUE '3',
  c_change  VALUE '2',
  c_create  VALUE '1'.


* etc
DATA: g_init    VALUE 'X',
      g_status,
      g_okcode  TYPE syucomm,
      okcode    TYPE syucomm,
      g_changed,
      g_answer,
      g_subrc   TYPE sy-subrc.

*&---------------------------------------------------------------------*
*&      Form  CONFIRM_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_G_ANSWER  text
*----------------------------------------------------------------------*
FORM confirm_exit_message  CHANGING p_answer.
* A: ???
* J: ?

  DATA: l_text1 TYPE string,
        l_text2 TYPE string,
        l_title TYPE string.

  MESSAGE s252(zcm01) INTO l_text1.   "??? ???? ?????.
  MESSAGE s253(zcm01) INTO l_text2.   "?????????
  MESSAGE s254(zcm01) INTO l_title.   "??

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
*   DEFAULTOPTION        = 'Y'
      textline1            = l_text1
      textline2            = l_text2
      titel                = l_title
*   START_COLUMN         = 25
*   START_ROW            = 6
*   CANCEL_DISPLAY       = 'X'
    IMPORTING
      answer              = p_answer
            .

ENDFORM.                    " CONFIRM_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_SAVE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_ANSWER  text
*----------------------------------------------------------------------*
FORM confirm_save_message  USING    p_answer.
* A: ???
* J: ?

  DATA: l_text1 TYPE string,
        l_title TYPE string.

  MESSAGE s253(zcm01) INTO l_text1. "?????????
  MESSAGE s254(zcm01) INTO l_title. "??

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
*   DEFAULTOPTION        = 'Y'
      textline1            = l_text1
*      TEXTLINE2            =
      titel                = l_title
*   START_COLUMN         = 25
*   START_ROW            = 6
*   CANCEL_DISPLAY       = 'X'
    IMPORTING
      answer              = p_answer
            .
ENDFORM.                    " CONFIRM_SAVE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  F4_OBJID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_SELOP_SM  text
*----------------------------------------------------------------------*
FORM f4_objid  CHANGING ps_hrs_objec  TYPE hrs_objec.
  DATA: ls_objec      LIKE objec.

  CALL FUNCTION 'RH_OBJID_REQUEST'
    EXPORTING
      plvar             = c_plvar
      otype             = ps_hrs_objec-otype
*      dynpro_repid      = sy-repid
*      dynpro_dynnr      = sy-dynnr
*      dynpro_plvarfield = 'PPHDR-PLVAR'
*      dynpro_otypefield = 'PPHDR-OTYPE'
*      dynpro_searkfield = p_fldnm
    IMPORTING
      sel_object        = ls_objec
    EXCEPTIONS
      cancelled         = 1
      wrong_condition   = 2
      nothing_found     = 3
      internal_error    = 4
      illegal_mode      = 5
      OTHERS            = 6.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING ls_objec TO ps_hrs_objec.
  ENDIF.

* ?? fcode ??
  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = 'ENTER'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.                                                    " F4_OBJID
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       ????? ?? ??/??
*----------------------------------------------------------------------*
FORM modify_selection_screen  USING    p_group_name.
  DATA: l_input.

  IF g_status IS INITIAL.
    l_input = c_on.
  ELSE.
    l_input = c_off.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 = p_group_name.
      screen-input  = l_input.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " MODIFY_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PROGRESS_IN_LOOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_progress_in_loop  USING    p_min
                                        p_max
                                        p_total
                                        p_tabix
                                        p_add
                                        p_text.
  DATA: l_rate  TYPE i,
        l_text  TYPE string,
        l_range TYPE i,
        l_total(5),
        l_tabix(5).

  l_range = p_max - p_min.
  l_rate  = l_range * p_tabix / p_total.
  l_rate  = p_min + l_rate.

  IF p_add  IS INITIAL.
    l_text = p_text.
  ELSE.
    l_total = p_total.
    l_tabix = p_tabix.
    CONCATENATE p_text  '(' l_tabix '/' l_total ')' INTO l_text.
  ENDIF.

  PERFORM display_progress_by_rate  USING l_rate  l_text.

ENDFORM.                    " DISPLAY_PROGRESS_IN_LOOP
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PROGRESS_BY_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_progress_by_rate   USING p_rate p_text.
  DATA: l_percentage  TYPE i,
        l_text        TYPE string.

  l_percentage  = p_rate.
  l_text        = p_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = l_percentage
      text       = l_text.

ENDFORM.                    " DISPLAY_PROGRESS_BY_RATE
