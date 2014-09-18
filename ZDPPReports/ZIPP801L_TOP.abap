*&---------------------------------------------------------------------*
*& Include ZIPP801L_TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zipp801r_trans_log_check MESSAGE-ID zmpp     .

*----------------------------------------------------------------------
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------

TABLES: ztpp_pp_log_head,  "PP Module - Processing Log Header Table
        ztpp_pp_log_deta.  "PP Module - Processing Log Detail Table

CONTROLS: tc_ipp801    TYPE TABLEVIEW USING SCREEN 0100,
          tc_ipp801_02 TYPE TABLEVIEW USING SCREEN 0200.

INCLUDE <icon>.
INCLUDE <list>.

*----------------------------------------------------------------------
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------

DATA: it_log_deta LIKE ztpp_pp_log_deta  OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_log_head OCCURS 0.
        INCLUDE STRUCTURE ztpp_pp_log_head.
DATA:   mark,
        text TYPE trdirt-text,  "Report title
      END OF it_log_head.

DATA: BEGIN OF it_ipp801 OCCURS 0,
        mark,                                    "Check Box
        programm TYPE ztpp_pp_log_head-programm, "Program Name
        text TYPE trdirt-text,                   "Report Title
        rcrd TYPE i,                             "Records
        succ TYPE i,                             "Success
        fail TYPE i,                             "Fail
*        msg   TYPE ztpp_pp_log_head-msg,    "Message text
*        ldate TYPE ztpp_pp_log_head-ldate,  "Date
*        ltime TYPE ztpp_pp_log_head-ltime,  "Time
*        luser TYPE ztpp_pp_log_head-luser,  "User
      END OF it_ipp801.

DATA: it_ipp801_02 LIKE TABLE OF it_log_head WITH HEADER LINE,
      it_data_t    LIKE TABLE OF it_ipp801 WITH HEADER LINE.

DATA: BEGIN OF it_detail OCCURS 0,
        mark,                    "Check Box
        name  TYPE trdirt-name,  "ABAP program name
        text  TYPE trdirt-text,  "Report title
        msg   TYPE bapi_msg,     "Message text
        ldate TYPE ztpp_pp_log_head-ldate,  "Date
        ltime TYPE ztpp_pp_log_head-ltime,  "Time
        luser TYPE ztpp_pp_log_head-luser,  "User
      END OF it_detail .

*----------------------------------------------------------------------
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------

DATA :     p_date          LIKE sy-datum,
           wa_init_flg ,
           ok_code         TYPE sy-ucomm .
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initialization OUTPUT.
  IF wa_init_flg IS INITIAL.
    wa_init_flg = 'X'.
    p_date = sy-datum.

  ENDIF.
ENDMODULE.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '1000'.
  SET TITLEBAR 'TITLE1000'.
ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_lines OUTPUT.
  IF sy-dynnr = '0100'.
    DESCRIBE TABLE it_ipp801    LINES tc_ipp801-lines.
  ELSE.
    DESCRIBE TABLE it_ipp801_02 LINES tc_ipp801_02-lines.
  ENDIF.
ENDMODULE.                 " modify_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  MODIFY SCREEN.
ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  IF sy-dynnr = '0100'.
    MODIFY it_ipp801    INDEX tc_ipp801-current_line.
  ELSE.
    MODIFY it_ipp801_02 INDEX tc_ipp801_02-current_line.
  ENDIF.
ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'SEL'.
      CLEAR ok_code.
      PERFORM search_data.

    WHEN 'INCL'.
      CLEAR ok_code.
      it_ipp801-mark = 'X'.
      MODIFY it_ipp801 TRANSPORTING mark WHERE mark <> 'X' .

    WHEN 'EXCL'.
      CLEAR ok_code.
      CLEAR it_ipp801-mark .
      MODIFY it_ipp801 TRANSPORTING mark WHERE mark = 'X' .

    WHEN 'DETAIL'.
      CLEAR ok_code.
      PERFORM write_marked_records.
      CALL SCREEN 0200
        STARTING AT 10 01  ENDING AT 115 20 .
    WHEN 'BACK'.
      LEAVE .

    WHEN 'CS'.
*      perform double_click_event .

  ENDCASE.
ENDMODULE.                 " user_command_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Form  Search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_data.
  CLEAR: it_log_head, it_log_head[],
         it_data_t, it_data_t[],
         it_ipp801, it_ipp801[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_log_head
    FROM ztpp_pp_log_head AS hd INNER JOIN
         trdirt           AS tr
         ON hd~programm = tr~name
    WHERE hd~ldate = p_date AND
          tr~sprsl = 'EN'.
  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'There is no data!!!'.
  ENDIF.

  CHECK sy-subrc = 0.

  SORT it_log_head BY programm.
  LOOP AT it_log_head.
    CLEAR it_data_t.
    MOVE-CORRESPONDING it_log_head TO it_data_t.
    IF it_log_head-logtype = 'E'.  "For Error
      it_data_t-fail = 1.
    ELSE.  "Success Or Warning
      it_data_t-succ = 1.
    ENDIF.
    it_data_t-rcrd = 1.
    APPEND it_data_t.
  ENDLOOP.

  SORT it_data_t BY text.
  LOOP AT it_data_t.
    CLEAR it_ipp801.
    MOVE-CORRESPONDING it_data_t TO it_ipp801.
    COLLECT it_ipp801.
  ENDLOOP.
ENDFORM.                    " Search_data
*&---------------------------------------------------------------------*
*&      Form  write_marked_records
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_marked_records.
  CLEAR: it_ipp801_02, it_ipp801_02[].
  LOOP AT it_ipp801 WHERE mark = 'X'.
    PERFORM write_detail_data USING it_ipp801-programm.
  ENDLOOP.
ENDFORM.                    " write_marked_records
*&---------------------------------------------------------------------*
*&      Form  write_detail_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IPP801_PROGRAMM  text
*----------------------------------------------------------------------*
FORM write_detail_data USING    p_prog.
  LOOP AT it_log_head WHERE programm = p_prog AND
                            logtype  = 'E' .
    MOVE-CORRESPONDING it_log_head TO it_ipp801_02.
    APPEND it_ipp801_02.
  ENDLOOP.
ENDFORM.                    " write_detail_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR 'TITLE0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0200 INPUT.
  SET SCREEN '0100'.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_date  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_date INPUT.
  IF p_date is initial .
    MESSAGE e000 WITH 'Set the parameter - Basic Date!!!'.
    EXIT.
  ENDIF.
ENDMODULE.                 " check_date  INPUT
