************************************************************************
* Program Name      : ZRMMPM24R_DOCK_STATUS
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.09.04.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : DMS Dock Status Report
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.09.04.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT zrmmpm24r_dock_status NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID zmmm.

INCLUDE : zrmmpmxxr_incl.

**--- Tables, Views & Structures

**--- Internal Tables
DATA : BEGIN OF it_temp OCCURS 0.
        INCLUDE STRUCTURE ztmm_dock.
DATA : END OF it_temp.

DATA : BEGIN OF it_itab OCCURS 0,
         zdock LIKE ztmm_dock-zdock,
         occup(3) TYPE n,
         waitg(3) TYPE n,
       END OF it_itab.

**--- Variables

**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_zdock FOR ztmm_dock-zdock.
SELECTION-SCREEN ULINE.
PARAMETERS : p_occu AS CHECKBOX DEFAULT 'X',
             p_wait AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK block1.

**--- Initialization
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.
  PERFORM get_data.

**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM comment_build.
    PERFORM make_alv_grid.
  ENDIF.




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
**---
  FIELD-SYMBOLS : <f1> TYPE ANY,
                  <f2> TYPE ANY.

  DATA : l_count(3) TYPE n,
         l_index LIKE sy-index.

  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_temp
           FROM ztmm_dock
          WHERE zdock IN s_zdock.

*---
  ASSIGN it_temp TO <f1>.

  LOOP AT it_temp.
    MOVE : it_temp-zdock TO it_itab-zdock.
    DO 10 TIMES.
      l_index = sy-index + 3.
      ASSIGN COMPONENT l_index OF STRUCTURE <f1> TO <f2>.
      CHECK NOT <f2> IS INITIAL.
      l_count = l_count + 1.
    ENDDO.
    IF l_count GE 1.
      it_itab-occup = 1.
      it_itab-waitg = l_count - 1.
    ENDIF.
    APPEND it_itab.
    CLEAR : it_temp, it_itab, l_count, l_index.
  ENDLOOP.

*---
  IF p_occu EQ space AND p_wait EQ space.
    DELETE it_itab WHERE NOT ( occup IS initial
                           AND waitg IS initial ).
  ELSEIF p_occu NE space AND p_wait EQ space.
    DELETE it_itab WHERE occup IS initial.
  ELSEIF p_occu EQ space AND p_wait NE space.
    DELETE it_itab WHERE waitg IS initial.
  ELSEIF p_occu NE space AND p_wait NE space.
    DELETE it_itab WHERE occup IS initial
                     AND waitg IS initial.
  ENDIF.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
**---
  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = w_program
*            i_callback_user_command = 'USER_COMMAND'
            it_fieldcat             = w_fieldcat[]
            it_events               = w_eventcat[]
            it_sort                 = w_sortcat[]
            i_save                  = 'A'
       TABLES
            t_outtab                = it_itab
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position     &2 : field name     &3 : field length
**--- &4 : description  &5 : field type     &6 : key

  append_fieldcat :
    w_col_pos 'ZDOCK' 10 'Dock'         'CHAR' 'X',
    w_col_pos 'OCCUP'  8 'Occupied'     'NUMC' ' ',
    w_col_pos 'WAITG'  8 'Waiting'      'NUMC' ' '.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.

ENDFORM.                    " build_sortcat
