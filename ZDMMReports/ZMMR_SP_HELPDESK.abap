*&---------------------------------------------------------------------*
*& Report  ZDMM_SP_HELPDESK_
*& *
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zmmr_sp_helpdesk MESSAGE-ID zmmm..

TABLES: ztmm_sp_help_it.

DATA: it_data TYPE ztmm_sp_help_it OCCURS 0.
DATA: ws_data LIKE LINE OF it_data.
DATA: fcode TYPE TABLE OF sy-ucomm.

*DATA: w_rver LIKE somlreci1-receiver
**       VALUE 'furongwang@hmmausa.com'.
*        VALUE 'ingeolmoon@hmmausa.com'.

DATA: w_rver LIKE somlreci1-receiver
        VALUE 'supplierservices@hmmausa.com',
      w_rver1 LIKE somlreci1-receiver
        VALUE 'ingeolmoon@hmmausa.com',
      w_rver2 LIKE somlreci1-receiver
        VALUE 'furongwang@hmmausa.com'.

DATA: ok_code LIKE sy-ucomm,
      w_suppname(25).

DATA:  w_st_desc(25),
       w_pri_desc(25),
       w_sev_desc(25),
       w_type_desc(25).

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  REFRESH: fcode.
  IF sy-dynnr = '9000'.
    APPEND 'PROC'  TO fcode.
  ENDIF.
  SET PF-STATUS 'ST900' EXCLUDING fcode.
  SET TITLEBAR 'T9000'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'PROC'.
      PERFORM create_ticket.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_TICKET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_ticket .
  DATA: l_number(10) TYPE n.

  SELECT SINGLE zdesc INTO w_st_desc
    FROM ztmm_sp_help_st
    WHERE item_status = ztmm_sp_help_it-item_status.

  SELECT SINGLE zdesc INTO w_pri_desc
     FROM ztmm_sp_help_pri
     WHERE item_priority = ztmm_sp_help_it-item_priority.

  SELECT SINGLE zdesc INTO w_type_desc
  FROM ztmm_sp_help_typ
  WHERE item_calltype = ztmm_sp_help_it-item_calltype.

*  SELECT SINGLE zdesc INTO w_sev_desc
*    FROM ztmm_sp_help_sev
*    WHERE item_severity = ztmm_sp_help_it-item_severity.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZMM_SP_HD'
*     QUANTITY                = '1'
*     SUBOBJECT               = ' '
*     TOYEAR                  = '0000'
*     IGNORE_BUFFER           = ' '
    IMPORTING
      number                  = l_number
*     QUANTITY                =
*     RETURNCODE              =
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.
    ztmm_sp_help_it-item_id = l_number.
  ELSE.
    CASE sy-subrc.
      WHEN '1'.
        MESSAGE e001 WITH text-m02.
      WHEN '2'.
        MESSAGE e001 WITH text-m03.
      WHEN '3'.
        MESSAGE e001 WITH text-m04.
      WHEN '4'.
        MESSAGE e001 WITH text-m05.
      WHEN '5'.
        MESSAGE e001 WITH text-m06.
      WHEN '6'.
        MESSAGE e001 WITH text-m07.
      WHEN '7'.
        MESSAGE e001 WITH text-m08.
      WHEN '8'.
        MESSAGE e001 WITH text-m09.
    ENDCASE.
  ENDIF.
  ztmm_sp_help_it-item_created_d = sy-datum.
  ztmm_sp_help_it-item_created_t = sy-uzeit.
  INSERT ztmm_sp_help_it .

  IF sy-subrc = 0.
    PERFORM send_email.
    COMMIT WORK.
    MESSAGE s000 WITH 'Data was saved'.
    CLEAR: ztmm_sp_help_it.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " CREATE_TICKET
*&---------------------------------------------------------------------*
*&      Module  INIT_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_9000 OUTPUT.
  IF ztmm_sp_help_it-item_requestor IS INITIAL.
    CLEAR: ztmm_sp_help_it.
    ztmm_sp_help_it-item_status = '002'.
    w_st_desc = 'New'.
    ztmm_sp_help_it-item_supplier = sy-uname.
    SELECT SINGLE name1 INTO w_suppname
      FROM lfa1
      WHERE lifnr = ztmm_sp_help_it-item_supplier.
  ENDIF.
*  CHECK w_init = 'X'.
*  IF sy-ucomm = 'CREATE'.
*    LOOP AT SCREEN.
*      IF screen-name = 'ZTMM_SP_HELP_IT-ITEM_ID'.
*        screen-input  = 0.
*      ELSE.
*        screen-input  = 1.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ELSEIF sy-ucomm = 'EDIT' .
*    LOOP AT SCREEN.
*      screen-input  = 1.
*      MODIFY SCREEN.
*    ENDLOOP.
*
*  ELSE.
*    LOOP AT SCREEN.
*      screen-input  = 0.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.

ENDMODULE.                 " INIT_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'CREATE'.
      CALL SCREEN '9100'.
    WHEN 'EDIT'.
      CALL TRANSACTION 'ZMMR022_E'.
    WHEN 'DISP'.
      CALL TRANSACTION 'ZMMR022_D'.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR ok_code.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  LEAVE_SCR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE leave_scr INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR ok_code.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " LEAVE_SCR  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.

  MOVE 'Following request was created:' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '=============================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Supplier: ' TO lt_body+0(15),
         ztmm_sp_help_it-item_supplier TO  lt_body+15(80).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE:  w_suppname TO lt_body+15(80).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Requestor:' TO  lt_body+0(15),
        ztmm_sp_help_it-item_requestor TO  lt_body+15(80).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Email:' TO  lt_body+0(15),
       ztmm_sp_help_it-item_requestor_e TO  lt_body+15(80).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Phone:' TO  lt_body+0(15),
        ztmm_sp_help_it-item_requestor_p TO  lt_body+15(80).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Service Area:' TO  lt_body+0(15),
        ztmm_sp_help_it-item_calltype TO  lt_body+15(5),
        w_type_desc TO  lt_body+20(20).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Priority:' TO  lt_body+0(15),
        ztmm_sp_help_it-item_priority TO  lt_body+15(5),
                w_pri_desc TO  lt_body+20(20).
  APPEND lt_body.
  CLEAR: lt_body.

*  MOVE: 'Severity:' TO  lt_body+0(15),
*         ztmm_sp_help_it-item_severity TO  lt_body+15(5),
*                 w_sev_desc TO  lt_body+20(20).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Status:' TO  lt_body+0(15),
         ztmm_sp_help_it-item_status TO  lt_body+15(5),
                 w_st_desc TO  lt_body+20(20).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Request:' TO  lt_body+0(15),
        ztmm_sp_help_it-item_summary TO  lt_body+15(100).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '' TO  lt_body+0(15),
        ztmm_sp_help_it-item_summary1 TO  lt_body+15(100).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: ' ' TO  lt_body+0(15),
          ztmm_sp_help_it-item_summary2 TO  lt_body+15(100).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: ' ' TO  lt_body+0(15),
          ztmm_sp_help_it-item_summary3 TO  lt_body+15(100).
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: ' ' TO  lt_body+0(15),
          ztmm_sp_help_it-item_summary4 TO  lt_body+15(100).
  APPEND lt_body.
  CLEAR: lt_body.

  IF sy-sysid NE 'UD1'.
    CALL FUNCTION 'ZCAF_SEND_EMAIL'
      EXPORTING
        p_subject  = 'HAEA Services Request'
        p_rec_type = 'U'
        p_receiver = w_rver
      TABLES
        pt_body    = lt_body.
  ENDIF.
  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'HAEA Services Request'
      p_rec_type = 'U'
      p_receiver = w_rver1
    TABLES
      pt_body    = lt_body.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'HAEA Services Request'
      p_rec_type = 'U'
      p_receiver = w_rver2
    TABLES
      pt_body    = lt_body.

ENDFORM.                    " SEND_EMAIL
