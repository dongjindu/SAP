************************************************************************
* Program Name      : ZAPP809M_VMCHANGE_MNTNC
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Delete Data Table: ZTPP_VM_CHANGE
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP809M_VMCHANGE_MNTNC    NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* TRANSPARENT TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTPP_VM_CHANGE.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_data OCCURS 0,
        mark                   .
        INCLUDE STRUCTURE     ZTPP_VM_CHANGE.
DATA: END OF it_data.
DATA: it_del  LIKE TABLE OF ztpp_vm_change  WITH HEADER LINE.
data: w_count type i.
data: w_total(10).
DATA: W_ERROR.
*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA: ok_code                  LIKE sy-ucomm  ,
      sv_code                  LIKE sy-ucomm  ,
      wa_flag                  TYPE C         ,
      wa_date                  LIKE sy-datum  ,
      wa_date_to               like sy-datum  .
*----------------------------------------------------------------------*
* Special Variables Definition
*----------------------------------------------------------------------*
CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

*----------------------------------------------------------------------*
* selection screen
*----------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-t01.
select-options: p_date  for sy-datum no-extension.
selection-screen skip.
selection-screen begin of line.
parameters:     r_dis   radiobutton group rg.
selection-screen comment 10(30) text-c01 for field r_dis.
parameters:     r_del   radiobutton group rg.
selection-screen comment 45(30) text-c02 for field r_del.
selection-screen end of line.
selection-screen end of block b1.

*----------------------------------------------------------------------*
at selection-screen.
  perform check_input.


*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  perform GET_COUNT.
  CALL SCREEN 9000.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'STATS9000'.
  SET TITLEBAR '9000'.
ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  data: l_yn.
  CASE ok_code.
    WHEN 'BACK' .
      perform check_delete using l_yn.
      IF L_YN = 'N'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'EXIT' OR 'CANC'.
      perform check_delete using l_yn.
      if l_yn = 'N'.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data INPUT.

  MODIFY it_data INDEX tc_9000-current_line.
ENDMODULE.                 " modify_data  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  IF W_ERROR = 'X'.
    CLEAR: W_ERROR.
    EXIT.
  ENDIF.
  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code.
    WHEN 'DELT'.
      PERFORM delete_data.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'SELT'.
      PERFORM read_data.
    WHEN 'CUNT'.
      PERFORM GET_COUNT.
    WHEN 'SELA'.
      PERFORM SELECT_ALL.
    WHEN 'DSEL'.
      PERFORM DESELECT_ALL.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  data: l_count type i.

* Checking Authority.
* perform check_authority  using wa_flag.

  check wa_flag = space .
* check delete by work date or from displayed data.

* Reading The Number of The Internal Table's Data
  describe table it_DATA   lines l_count.
  if l_count < 1 and
     p_date is initial.
    message s000 with  TEXT-001 .
    exit.
  endif.
  if l_count eq 0.
    perform delete_records.
    exit.
  endif.

* Getting The Table Control's Current Line
  get cursor line l_count.
  if sy-subrc <> 0.
    message s000 with 'Set Cursor Correctly!!'.
  endif.
* Copying Data To be Deleted into IT_DEL.
  loop at it_DATA   where  mark = 'X'.
    move-corresponding it_DATA   to it_del .
    append it_del.
*   Deleting Data to be deleted from IT_APP803.
    delete it_DATA  .
  endloop.
ENDFORM.                    " DELETE_DATA

*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  data: l_text(20).
  DATA: L_LINE TYPE I.
* Checking Authority.
* perform check_authority  using wa_flag.

  check wa_flag = space .

  DESCRIBE TABLE IT_DEL LINES L_LINE.
  IF L_LINE EQ 0.
    MESSAGE s000 WITH 'No deleted data!'.
    exit.
  ENDIF.
* Data Deletion
  delete ZTPP_VM_CHANGE from table it_del .
  if sy-subrc <> 0.
    rollback work.
    message s000 with  TEXT-004 .
  else.
    message s000 with  TEXT-003 .
    commit work.
    REFRESH IT_del.
  endif.
ENDFORM.                    " SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  IF P_DATE IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ZTPP_VM_CHANGE .
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ZTPP_VM_CHANGE
     WHERE CHG_DATE in p_date.
  ENDIF.
  DESCRIBE TABLE it_data LINES tc_9000-lines.
  W_TOTAL = TC_9000-LINES.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  check_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_input.
   if r_del = 'X' AND
      P_DATE[] IS INITIAL.
      MESSAGE E000 WITH TEXT-M01.
   ENDIF.
   if r_del = 'X'.
     PERfORM DELETE_records.
   ENDIF.
endform.                    " check_input
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_records.
  data: answer(1) .
  data: l_text(80).

  concatenate 'Data from' p_date-low 'to' p_date-high
              'will be deleted. Are you sure?'
              into l_text
              separated by space.
* POPUP FOR COMFIRM
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR                    = 'Please Confirm'
*     DIAGNOSE_OBJECT             = ' '
      text_question               = l_text
      TEXT_BUTTON_1               = 'Yes'
*     ICON_BUTTON_1               = ' '
      TEXT_BUTTON_2               = 'No'
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
    IMPORTING
      ANSWER                      = answer.
*   TABLES
*     PARAMETER                   =
*   EXCEPTIONS
*     TEXT_NOT_FOUND              = 1
*     OTHERS                      = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  if answer = '2' or
     answer = 'A'.
     stop.
  endif.
  IF ANSWER = '1'.
     delete from ztpp_vm_change where CHG_DATE in p_date.
     message s000 with text-m02.
     stop.
  ENDIF.


endform.                    " delete_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GET_COUNT.

* read the total records
  select count( * ) into w_count
    from ztpp_vm_change
    where chg_date in p_date.
  WRITE W_COUNT TO W_TOTAL USING EDIT MASK 'RR__________' .
  REFRESH IT_DATA.
endform.                    " GET_COUNT
*&---------------------------------------------------------------------*
*&      Form  DATE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DATE_CHECK.

   DELETE P_DATE INDEX 1.
   IF P_DATE-LOW IS INITIAL AND
      P_DATE-HIGH IS INITIAL.
      EXIT.
   ENDIF.
   IF P_DATE-LOW GT P_DATE-HIGH.
     MESSAGE S000 WITH 'Date from is great than date to'.
     W_ERROR = 'X'.
     STOP.
   ENDIF.
   if not p_date-high is initial.
     p_date-sign = 'I'.
     P_DATE-OPTION = 'BT'.
     APPEND P_DATE.
   ELSE.
     p_date-sign = 'I'.
     P_DATE-OPTION = 'EQ'.
     APPEND P_DATE.
   endif.
endform.                    " DATE_CHECK
*&---------------------------------------------------------------------*
*&      Module  CHECH_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module CHECH_DATE input.
  PERFORM DATE_CHECK.
endmodule.                 " CHECH_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SELECT_ALL.
  IT_DATA-MARK = 'X'.
  MODIFY IT_DATA TRANSPORTING MARK
     WHERE MARK NE 'X'.
endform.                    " SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DESELECT_ALL.
  CLEAR: IT_DATA-MARK .
  MODIFY IT_DATA TRANSPORTING MARK
     WHERE MARK EQ 'X'.

endform.                    " DESELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  check_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_delete USING P_YN.
   data: l_line type i.
   DATA: L_TEXT(100).
   DATA: ANSWER.

   describe table it_del lines l_line.
   L_TEXT = 'You have put some records for delete.'.
   concatenate l_text 'Do you want to save the deletion?' into l_text..
   if l_line = 0.
     p_yn = 'N'.
   else.
     CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR                    = 'Please Confirm'
*     DIAGNOSE_OBJECT             = ' '
      text_question               = l_text
      TEXT_BUTTON_1               = 'Yes'
*     ICON_BUTTON_1               = ' '
      TEXT_BUTTON_2               = 'No'
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
    IMPORTING
      ANSWER                      = answer.
*   TABLES
*     PARAMETER                   =
*   EXCEPTIONS
*     TEXT_NOT_FOUND              = 1
*     OTHERS                      = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  if answer = '2'.
     answer = 'A'.
     P_YN = 'N'.
  ELSEIF ANSWER = 'A'.
     P_YN = 'Y'.
  endif.
  IF ANSWER = '1'.
     P_YN = 'Y'.
  ENDIF.

   endif.
endform.                    " check_delete
