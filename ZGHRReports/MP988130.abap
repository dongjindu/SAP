*----------------------------------------------------------------------*
*                                                                      *
*       Input-modules for infotype 9881                                *
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  CASE psyst-ioper.
    WHEN 'INS' OR 'MOD' or copy.
      CASE fcode.
        WHEN 'UPD'.
          PERFORM check_status.
          PERFORM save_training.
        WHEN 'BTN_INS001'.
          APPEND INITIAL LINE TO gt_history.
        WHEN 'BTN_DEL001'.
          DELETE gt_history WHERE mark = 'X'.
        WHEN 'BTN_INS002'.
          APPEND INITIAL LINE TO gt_plan.
        WHEN 'BTN_DEL002'.
          DELETE gt_plan WHERE mark = 'X'.
      ENDCASE.
    WHEN 'DEL'.
      CASE fcode.
        WHEN 'UPD'.
          PERFORM dele_training.
      ENDCASE.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_HISTORY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_history INPUT.

  MODIFY gt_history INDEX tc_history-current_line.

ENDMODULE.                 " MODIFY_HISTORY  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_PLAN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_plan INPUT.

  MODIFY gt_plan INDEX tc_plan-current_line.

ENDMODULE.                 " MODIFY_PLAN  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_HIST_OBJ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_hist_obj INPUT.

  DATA: lv_line     TYPE systepl,
        lv_field    TYPE help_info-dynprofld.

  DATA: lt_rtab_f4  TYPE TABLE OF ddshretval WITH HEADER LINE.

  DATA: lt_mapping  TYPE TABLE OF dselc WITH HEADER LINE.

*&--- Table Control?? ??? ??? ???? ??? ??? ????
  CLEAR: lv_field, lv_line.
  GET CURSOR FIELD lv_field LINE lv_line.

  PERFORM get_master.

*&--- Search Help?? ? ???? ?? ??? ????.
  CLEAR: lt_mapping, lt_mapping[].
  lt_mapping-fldname   = 'F0002'.
  lt_mapping-dyfldname = 'GT_HISTORY-STEXT'.
  APPEND lt_mapping.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'OBJID'
            dynpprog        = sy-cprog
            dynpnr          = sy-dynnr
            dynprofield     = lv_field  "?? ????
            stepl           = lv_line  "?? ?? ????
            window_title    = 'Training Master Data'
            value_org       = 'S'
       TABLES
            value_tab       = gt_master[]
            return_tab      = lt_rtab_f4[]
            dynpfld_mapping = lt_mapping[].


ENDMODULE.                 " F4_HIST_OBJ  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_PLAN_OBJ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_plan_obj INPUT.
*&--- Table Control?? ??? ??? ???? ??? ??? ????
  CLEAR: lv_field, lv_line.
  GET CURSOR FIELD lv_field LINE lv_line.

  PERFORM get_master.

*&--- Search Help?? ? ???? ?? ??? ????.
  CLEAR: lt_mapping, lt_mapping[].
  lt_mapping-fldname   = 'F0002'.
  lt_mapping-dyfldname = 'GT_PLAN-STEXT'.
  APPEND lt_mapping.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'OBJID'
            dynpprog        = sy-cprog
            dynpnr          = sy-dynnr
            dynprofield     = lv_field  "?? ????
            stepl           = lv_line  "?? ?? ????
            window_title    = 'Training Master Data'
            value_org       = 'S'
       TABLES
            value_tab       = gt_master[]
            return_tab      = lt_rtab_f4[]
            dynpfld_mapping = lt_mapping[].

ENDMODULE.                 " F4_PLAN_OBJ  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_beg_date INPUT.

  IF NOT gt_history-objid IS INITIAL.
    IF gt_history-begda IS INITIAL.
*      MESSAGE 'Check Start Date' TYPE 'E'.
      MESSAGE e005(zghrm).
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_end_date INPUT.

  IF NOT gt_history-objid IS INITIAL.
    IF gt_history-endda IS INITIAL.
*      MESSAGE 'Check End Date' TYPE 'E'.
      MESSAGE e001(zghrm).
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_date INPUT.

 IF NOT gt_history-begda IS INITIAL AND NOT gt_history-endda IS INITIAL
.
    IF gt_history-begda > gt_history-endda.
*      MESSAGE 'Start Date can not be greater than End Date' type 'E'.
      MESSAGE e002(zghrm).
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_YEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_year INPUT.

  IF NOT gt_plan-objid IS INITIAL.
    IF gt_plan-tyear IS INITIAL.
*      MESSAGE 'Check Year' type 'E'.
      MESSAGE e002(zghrm).
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_YEAR  INPUT
