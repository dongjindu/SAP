************************************************************************
* Program Name      : ZAPP807M_REPSEQ_MNTNC
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Delete Data Table: ZTPP_REP_SEQ
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zapp807m_repseq_mntnc      NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* TRANSPARENT TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ztpp_rep_seq.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_data OCCURS 0,
        mark                   .
        INCLUDE STRUCTURE     ztpp_rep_seq.
DATA: END OF it_data.
DATA: it_del                  LIKE TABLE OF IT_DATA    WITH HEADER LINE.

*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA: ok_code                  LIKE sy-ucomm  ,
      sv_code                  LIKE sy-ucomm  ,
      wa_flag                  TYPE C         ,
      wa_date                  LIKE sy-datum  .

*----------------------------------------------------------------------*
* Special Variables Definition
*----------------------------------------------------------------------*
CONTROLS: tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
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
  CASE ok_code.
    WHEN 'BACK' .
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
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
  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code.
    WHEN 'DELT'.
      PERFORM delete_data.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'SELT'.
      PERFORM read_data.
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

* Reading The Number of The Internal Table's Data
  describe table it_DATA   lines l_count.
  if l_count < 1.
    message s000 with  TEXT-001 .
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

* Checking Authority.
* perform check_authority  using wa_flag.

  check wa_flag = space .
  commit work.
* Data Deletion
  delete ztpp_REP_SEQ from table it_del .
  if sy-subrc <> 0.
    rollback work.
    message s000 with  TEXT-004 .
  else.
    message s000 with  TEXT-003 .
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
  IF wa_date IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ztpp_rep_seq     .
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
      FROM ztpp_rep_seq
     WHERE wk_date = wa_date.
  ENDIF.
  DESCRIBE TABLE it_data LINES tc_9000-lines.
ENDFORM.                    " READ_DATA
