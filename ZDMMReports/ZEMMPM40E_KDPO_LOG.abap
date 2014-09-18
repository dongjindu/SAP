************************************************************************
* Program Name      : ZEMMPM40E_KDPO_LOG
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.07.20.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K911477
* Addl Documentation:
* Description       : KD PO Creation Log Program
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.07.20.     Sung-Tae Lim     UD1K911477     Initial Coding
*
*
************************************************************************

REPORT zemmpm40e_kdpo_log NO STANDARD PAGE HEADING
                        LINE-SIZE 400
                        MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

TABLES : ztmm_kdpo_log,
         t024.


**---
DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE ztmm_kdpo_log.
DATA :   linecolor(4),
         mark(1),
       END OF it_itab.

* Log Date
DATA : fr_logdate  LIKE sy-datum.
DATA : to_logdate  LIKE sy-datum.

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
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
SELECTION-SCREEN COMMENT (12) text-010.
SELECTION-SCREEN POSITION 33.
PARAMETERS p_succe AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) text-011 FOR FIELD p_succe.
SELECTION-SCREEN POSITION 46.
PARAMETERS p_error AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) text-012 FOR FIELD p_error.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

SELECT-OPTIONS : s_lifnr   FOR ztmm_kdpo_log-lifnr,
                 s_matkl   FOR ztmm_kdpo_log-matkl,
                 s_matnr   FOR ztmm_kdpo_log-matnr,
                 s_ebeln   FOR ztmm_kdpo_log-ebeln,
                 s_pwwrk   FOR ztmm_kdpo_log-pwwrk,
                 s_ekgrp   FOR t024-ekgrp,
                 s_pedtr   FOR ztmm_kdpo_log-pedtr,
                 s_ernam   FOR ztmm_kdpo_log-ernam,
                 s_erdat   FOR ztmm_kdpo_log-erdat,
                 s_erzet   FOR ztmm_kdpo_log-erzet.
SELECTION-SCREEN END OF BLOCK block1.


**---
INITIALIZATION.
  PERFORM init.
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





*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init.
*--- For User
  MOVE: 'I'      TO s_ernam-sign,
        'EQ'     TO s_ernam-option,
        sy-uname TO s_ernam-low.
  APPEND s_ernam.

*--- For Log Date
  fr_logdate = sy-datum - 1.
  to_logdate = sy-datum.
  MOVE: 'I'        TO s_erdat-sign,
        'BT'       TO s_erdat-option,
        fr_logdate TO s_erdat-low,
        to_logdate TO s_erdat-high.
  APPEND s_erdat.
ENDFORM.                    " init

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

  IF p_succe NE space.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_itab
             FROM ztmm_kdpo_log
            WHERE lifnr IN s_lifnr
              AND matkl IN s_matkl
              AND matnr IN s_matnr
              AND ebeln IN s_ebeln
              AND pwwrk IN s_pwwrk
              AND ekgrp IN s_ekgrp
              AND pedtr IN s_pedtr
              AND ernam IN s_ernam
              AND erdat IN s_erdat
              AND erzet IN s_erzet
              AND ebeln NE space.
  ENDIF.

  IF p_error NE space.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_itab
             FROM ztmm_kdpo_log
            WHERE lifnr IN s_lifnr
              AND matkl IN s_matkl
              AND matnr IN s_matnr
              AND ebeln IN s_ebeln
              AND pwwrk IN s_pwwrk
              AND ekgrp IN s_ekgrp
              AND pedtr IN s_pedtr
              AND ernam IN s_ernam
              AND erdat IN s_erdat
              AND erzet IN s_erzet
              AND ebeln EQ space.
  ENDIF.

*---
  LOOP AT it_itab.
    MOVE : it_itab-color TO it_itab-linecolor.
    MODIFY it_itab.
    CLEAR : it_itab.
  ENDLOOP.
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
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.
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
*---
  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize,
         'MARK'      TO w_layout-box_fieldname.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = w_program
*            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = w_layout
            it_fieldcat              = w_fieldcat[]
            it_events                = w_eventcat[]
            it_sort                  = w_sortcat[]
            i_save                   = 'A'
       TABLES
            t_outtab                 = it_itab
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
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
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'LOGNO_H'   10 'Log Number'     'CHAR' 'X' ''      '',
    w_col_pos 'LIFNR'     10 'Vendor'         'CHAR' 'X' ''      '',
    w_col_pos 'MATKL'     09 'Material Group' 'CHAR' 'X' ''      '',
    w_col_pos 'MATNR'     18 'Material'       'CHAR' ''  ''      '',
    w_col_pos 'EBELN'     10 'PO Number'      'CHAR' ''  ''      '',
    w_col_pos 'PWWRK'     04 'Plant'          'CHAR' ''  ''      '',
    w_col_pos 'LGORT'     04 'Storage Loc'    'CHAR' ''  ''      '',
    w_col_pos 'GSMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'     03 'UoM'            'UNIT' ''  ''      '',
    w_col_pos 'EKGRP'     03 'Pur Group'      'CHAR' ''  ''      '',
    w_col_pos 'PEDTR'     10 'Delivery Date'  'DATS' ''  ''      '',
    w_col_pos 'MESSA'     80 'Message'        'CHAR' ''  ''      ''.
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

*&---------------------------------------------------------------------*
*&      Form  make_message_type_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_message_type_ranges.

ENDFORM.                    " make_message_type_ranges
