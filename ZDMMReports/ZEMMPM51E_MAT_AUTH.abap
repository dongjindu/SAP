************************************************************************
* Program Name      : ZEMMPM51E_MAT_AUTH
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.08.26.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K912027
* Addl Documentation:
* Description       : Authority Management for Material Master
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.08.26.     Sung-Tae Lim     UD1K912027    Initial Coding
*
* 5/4/2012        t-code is deleted by APM Monitoring
************************************************************************

REPORT zemmpm51e_mat_auth NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

TABLES : ztmm_mara,
         ztmm_auth.


**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE ztmm_auth.
DATA :   w_selected(1),
         w_exist(1),
         w_change(1),
       END OF it_itab.

DATA : it_dele LIKE ztmm_auth OCCURS 0 WITH HEADER LINE.


**--- Variables
DATA : w_okcode LIKE sy-ucomm,
       w_save_okcode LIKE sy-ucomm,
       w_loopc LIKE sy-loopc,
       w_tot_lines TYPE i,
       w_selected(1).

DATA : w_position TYPE i,
       w_found(1),
       w_find_pos TYPE i,
       w_loop_first TYPE i.

DATA : wa_tc9000 TYPE cxtab_column.

DATA : wa_ztmm_auth LIKE ztmm_auth.


**--- Table Control
CONTROLS : tc_9000 TYPE TABLEVIEW USING SCREEN 9000.


**--- Constants


**--- Macro


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_uname FOR ztmm_auth-uname MATCHCODE OBJECT user_comp,
                 s_tcode FOR ztmm_auth-tcode,
                 s_erdat FOR ztmm_auth-erdat,
                 s_aedat FOR ztmm_auth-aedat.
SELECTION-SCREEN END OF BLOCK block1.


**---
INITIALIZATION.


**---
TOP-OF-PAGE.


**---
START-OF-SELECTION.
  PERFORM get_data.
  CALL SCREEN 9000.




**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_itab, it_itab[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
           FROM ztmm_auth
          WHERE uname IN s_uname
            AND tcode IN s_tcode
            AND erdat IN s_erdat
            AND aedat IN s_aedat.

  LOOP AT it_itab.
    MOVE : 'X' TO it_itab-w_exist.
    MODIFY it_itab.
  ENDLOOP.

  CLEAR : it_itab.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Module  status_scrcom  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_scrcom OUTPUT.
**---
  CASE sy-dynnr.
    WHEN '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " status_scrcom  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
**---
  CASE sy-dynnr.
    WHEN '9000'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          READ TABLE it_itab WITH KEY w_change = 'X'.
          IF sy-subrc EQ 0.
            PERFORM confirm_step.
          ELSE.
            IF NOT it_dele[] IS INITIAL.
              PERFORM confirm_step.
            ENDIF.
            LEAVE TO SCREEN 0.
          ENDIF.
*          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_scrcom  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_scrcom INPUT.
**---
  MOVE : w_okcode TO w_save_okcode.

  CLEAR : w_okcode.

  CASE sy-dynnr.
    WHEN '9000'.
      CASE w_save_okcode.
        WHEN 'SAVE'.
          CLEAR : w_save_okcode.
          PERFORM save_routine.
          PERFORM get_data.
          LEAVE TO SCREEN 9000.
*          LEAVE TO SCREEN 0.
        WHEN 'DELE'.
          CLEAR : w_save_okcode.
          PERFORM delete_line.
        WHEN 'SALL'.
          CLEAR : w_save_okcode.
          PERFORM select_deselect_all_9000 USING 'X'.
        WHEN 'DALL'.
          CLEAR : w_save_okcode.
          PERFORM select_deselect_all_9000 USING ' '.
        WHEN 'ASCE'.
          CLEAR : w_save_okcode.
          PERFORM ascending_sort.
        WHEN 'DESC'.
          CLEAR : w_save_okcode.
          PERFORM descending_sort.
        WHEN 'FIND' OR 'FIND+'.
          PERFORM find_string.
          CLEAR : w_save_okcode.
        WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
          PERFORM table_control_page_scrol USING w_save_okcode
                                                 tc_9000-top_line
                                                 w_tot_lines
                                                 w_loopc.
          CLEAR : w_save_okcode.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " user_command_scrcom  INPUT

*&---------------------------------------------------------------------*
*&      Form  find_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_string.
**---
  IF w_save_okcode EQ 'FIND'.
    MOVE : 1 TO w_position.
  ELSEIF w_save_okcode EQ 'FIND+'.
    w_position = w_loop_first + 1.
  ENDIF.

**---
  IF w_save_okcode EQ 'FIND'.
    PERFORM popup_get_value(sapfsfxx) USING    'FSTR' ' '
                                      CHANGING rsdxx-findstr.
  ENDIF.

**---
  IF sy-ucomm NE 'CANC'.
*---
    CLEAR : w_found.
    IF sy-dynnr EQ '9000'.
      LOOP AT it_itab FROM w_position.
        IF it_itab CS rsdxx-findstr OR it_itab CP rsdxx-findstr.
          MOVE : 'X' TO w_found.
          MOVE : sy-tabix TO w_find_pos.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF w_found NE space.
        MOVE : 1          TO tc_9000-current_line,
               w_find_pos TO tc_9000-top_line,
               w_find_pos TO w_loop_first.
      ELSE.
        MESSAGE s042(e2) WITH rsdxx-findstr.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " find_string

*&---------------------------------------------------------------------*
*&      Module  display_data_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_data_scr9000 OUTPUT.
**---
  IF sy-stepl EQ 1.
    CALL FUNCTION 'ME_GET_TC_LINES'
         EXPORTING
              im_lines_total    = w_tot_lines
              im_lines_per_page = sy-loopc
              im_top_line       = tc_9000-top_line
         IMPORTING
              ex_tc_lines       = tc_9000-lines
         EXCEPTIONS
              OTHERS            = 1.
  ENDIF.

**---
  CLEAR : it_itab.

  READ TABLE it_itab INDEX tc_9000-current_line.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_itab TO ztmm_auth.
    MOVE : it_itab-w_selected  TO w_selected.
    IF ztmm_auth-uname NE space AND ztmm_auth-tcode NE space AND
       ztmm_auth-func1 NE space.
      LOOP AT SCREEN.
        IF screen-group1 EQ 'GR1'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

**---
  MOVE : sy-loopc TO w_loopc.
ENDMODULE.                 " display_data_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  get_control_lines_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_control_lines_9000 OUTPUT.
**---
  DESCRIBE TABLE it_itab LINES w_tot_lines.
ENDMODULE.                 " get_control_lines_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  input_data_modify_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE input_data_modify_scr9000 INPUT.
**---
  CLEAR : it_itab.

  READ TABLE it_itab INDEX tc_9000-current_line.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING ztmm_auth TO it_itab.
    MOVE : w_selected            TO it_itab-w_selected,
           'X'                   TO it_itab-w_change.
    MOVE : sy-datum              TO it_itab-aedat,
           sy-uzeit              TO it_itab-aezet,
           sy-uname              TO it_itab-aenam.
    IF it_itab-w_exist EQ space.
      MOVE : sy-datum            TO it_itab-erdat,
             sy-uzeit            TO it_itab-erzet,
             sy-uname            TO it_itab-ernam.
    ENDIF.
    IF it_itab-uname NE space.
      MODIFY it_itab INDEX tc_9000-current_line.
    ELSE.
      DELETE it_itab INDEX tc_9000-current_line.
    ENDIF.
  ELSE.
    MOVE-CORRESPONDING ztmm_auth TO it_itab.
    MOVE : w_selected            TO it_itab-w_selected,
           'X'                   TO it_itab-w_change.
    MOVE : sy-datum              TO it_itab-aedat,
           sy-uzeit              TO it_itab-aezet,
           sy-uname              TO it_itab-aenam.
    IF it_itab-w_exist EQ space.
      MOVE : sy-datum            TO it_itab-erdat,
             sy-uzeit            TO it_itab-erzet,
             sy-uname            TO it_itab-ernam.
    ENDIF.
    IF it_itab-uname NE space.
      APPEND it_itab.
    ENDIF.
  ENDIF.

  CLEAR : it_itab.

*---
  CHECK ztmm_auth-uname NE space.

  IF ztmm_auth-tcode EQ space.
    SET CURSOR FIELD 'ZTMM_AUTH-TCODE' LINE sy-stepl.
    MESSAGE e999 WITH text-m03.
  ENDIF.

  IF ztmm_auth-func1 EQ space.
    SET CURSOR FIELD 'ZTMM_AUTH-FUNC1' LINE sy-stepl.
    MESSAGE e999 WITH text-m04.
  ENDIF.
ENDMODULE.                 " input_data_modify_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  ascending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ascending_sort.
*---
  LOOP AT tc_9000-cols INTO wa_tc9000.
    IF wa_tc9000-selected = 'X'.
      SORT it_itab BY (wa_tc9000-screen-name+10) ASCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ascending_sort

*&---------------------------------------------------------------------*
*&      Form  descending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descending_sort.
*---
  LOOP AT tc_9000-cols INTO wa_tc9000.
    IF wa_tc9000-selected = 'X'.
      SORT it_itab BY (wa_tc9000-screen-name+10) DESCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " descending_sort

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0211   text
*----------------------------------------------------------------------*
FORM select_deselect_all_9000 USING    p_value.
**---
  MOVE : p_value TO it_itab-w_selected.

  MODIFY it_itab TRANSPORTING w_selected WHERE uname NE space.
ENDFORM.                    " select_deselect_all_9000

*&---------------------------------------------------------------------*
*&      Module  check_input_data_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_input_data_scr9000 INPUT.
*---
  CLEAR : wa_ztmm_auth.

  SELECT SINGLE * INTO wa_ztmm_auth
                  FROM ztmm_auth
                 WHERE uname EQ ztmm_auth-uname
                   AND tcode EQ ztmm_auth-tcode
                   AND func1 EQ ztmm_auth-func1.

  IF sy-subrc EQ 0.
    MESSAGE e999 WITH text-m01.
  ENDIF.

  READ TABLE it_itab WITH KEY uname = ztmm_auth-uname
                              tcode = ztmm_auth-tcode
                              func1 = ztmm_auth-func1.

  IF sy-subrc EQ 0.
    MESSAGE e999 WITH text-m01.
  ENDIF.
ENDMODULE.                 " check_input_data_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  save_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_routine.
*---
  DELETE ztmm_auth FROM TABLE it_dele.

  LOOP AT it_itab.
    CLEAR : ztmm_auth.
    MOVE-CORRESPONDING it_itab TO ztmm_auth.
    MODIFY ztmm_auth.
    CLEAR : it_itab, ztmm_auth.
  ENDLOOP.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    MESSAGE s999 WITH text-m02.
  ENDIF.
ENDFORM.                    " save_routine

*&---------------------------------------------------------------------*
*&      Form  confirm_step
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm_step.
*---
  DATA : l_answer(1).

  CLEAR : l_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption        = 'Y'
      textline1            = text-003
*     TEXTLINE2            = ' '
      titel                = text-004
      start_column         = 25
      start_row            = 6
      cancel_display       = 'X'
    IMPORTING
      answer               = l_answer.

  CASE l_answer.
    WHEN 'J'.
      PERFORM save_routine.
      LEAVE TO SCREEN 0.
    WHEN 'N'.
      LEAVE TO SCREEN 0.
    WHEN 'A'.
  ENDCASE.
ENDFORM.                    " confirm_step

*&---------------------------------------------------------------------*
*&      Form  delete_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_line.
*---
  CLEAR : it_dele, it_dele[].

  LOOP AT it_itab WHERE w_selected NE space.
    MOVE-CORRESPONDING it_itab TO it_dele.
    APPEND it_dele.
    CLEAR : it_itab, it_dele.
  ENDLOOP.

  DELETE it_itab WHERE w_selected NE space.
ENDFORM.                    " delete_line
