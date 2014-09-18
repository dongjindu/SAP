************************************************************************
* Program Name      : ZEMMPM49E_LDC_LOG
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.08.24.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K911878
* Addl Documentation:
* Description       : LDC Creation Log Program
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.08.24.     Sung-Tae Lim     UD1K911878     Initial Coding
*
*
************************************************************************

REPORT zemmpm49e_ldc_log NO STANDARD PAGE HEADING
                         LINE-SIZE 132
                         LINE-COUNT 64(1)
                         MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

TABLES : ztmm_ldc_log.


**--- Internal Tables
DATA : it_itab LIKE ztmm_ldc_log OCCURS 0 WITH HEADER LINE.

RANGES : r_msgty FOR ztmm_nstl_log-msgty.


**--- Variables


**--- Constants


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
SELECTION-SCREEN POSITION 58.
PARAMETERS p_error AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) text-012 FOR FIELD p_error.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

PARAMETERS : p_ekorg LIKE t024e-ekorg DEFAULT 'PU01'.
SELECT-OPTIONS : s_lifnr FOR lfa1-lifnr,
                 s_matnr FOR mara-matnr,
                 s_kzust FOR konh-kzust,
                 s_mtart FOR mara-mtart,
                 s_profl FOR mara-profl,
                 s_erdat FOR ztmm_ldc_log-erdat,
                 s_ernam FOR ztmm_ldc_log-ernam DEFAULT sy-uname.

SELECTION-SCREEN END OF BLOCK block1.


**---
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
*---
  PERFORM make_message_type_ranges.

  CLEAR : it_itab, it_itab[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
           FROM ztmm_ldc_log
          WHERE lifnr IN s_lifnr
            AND matnr IN s_matnr
            AND kzust IN s_kzust
            AND erdat IN s_erdat
            AND ernam IN s_ernam
            AND msgty IN r_msgty.

*---
  DATA : l_tabix LIKE sy-tabix.

  LOOP AT it_itab.
    CLEAR : mara, l_tabix.
    MOVE : sy-tabix TO l_tabix.
    SELECT SINGLE profl mtart INTO (it_itab-profl, it_itab-mtart)
                              FROM mara
                             WHERE matnr EQ it_itab-matnr
                               AND profl IN s_profl
                               AND mtart IN s_mtart.
    IF sy-subrc NE 0.
      DELETE it_itab.
    ELSE.
      PERFORM get_vendor_desc USING it_itab-lifnr.
      PERFORM get_material_desc USING it_itab-matnr.
      MOVE : lfa1-name1 TO it_itab-name1,
             makt-maktx TO it_itab-maktx.
      MODIFY it_itab INDEX l_tabix.
    ENDIF.
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
  w_line-info = text-000.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-002 p_ekorg ' ',
      'S' text-003 s_lifnr-low s_lifnr-high,
      'S' text-004 s_matnr-low s_matnr-high,
      'S' text-005 s_kzust-low s_kzust-high,
      'S' text-006 s_mtart-low s_mtart-high,
      'S' text-007 s_profl-low s_profl-high,
      'S' text-008 s_erdat-low s_erdat-high,
      'S' text-009 s_ernam-low s_ernam-high.
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
  MOVE : 'LINEC' TO w_layout-info_fieldname,
         'X'     TO w_layout-colwidth_optimize.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
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
    w_col_pos 'LIFNR'     10 'Vendor'         'NUMC' 'X' ''      '',
    w_col_pos 'NAME1'     25 'Vendor Name'    'CHAR' 'X' ''      '',
    w_col_pos 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'MAKTX'     30 'Material Desc'  'CHAR' 'X' ''      '',
    w_col_pos 'DATAB'     10 'Valid From'     'DATS' ''  ''      '',
    w_col_pos 'DATBI'     10 'Valid To'       'DATS' ''  ''      '',
    w_col_pos 'KBETR'     12 'Condition'      'CURR' ''  'WAERS' '',
    w_col_pos 'WAERS'     05 'Cuky'           'CUKY' ''  ''      '',
    w_col_pos 'ZFRA1'     08 'FRA1 Rate'      'CURR' ''  'KONWA' '',
    w_col_pos 'ZZOTH'     08 'ZOTH Rate'      'CURR' ''  'KONWA' '',
    w_col_pos 'ZZOTI'     08 'ZOTI Rate'      'CURR' ''  'KONWA' '',
    w_col_pos 'KONWA'     05 'Cuky'           'CUKY' ''  ''      '',
    w_col_pos 'ERDAT'     10 'Change Date'    'DATS' ''  ''      '',
    w_col_pos 'KZUST'     03 'Reason Code'    'CHAR' ''  ''      '',
    w_col_pos 'PROFL'     03 'Source'         'CHAR' ''  ''      '',
    w_col_pos 'MTART'     04 'Material Type'  'CHAR' ''  ''      '',
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
*---
  CLEAR : r_msgty, r_msgty[].

  IF p_succe NE space.
    MOVE : 'I'     TO r_msgty-sign,
           'EQ'    TO r_msgty-option,
           'S'     TO r_msgty-low.
    APPEND r_msgty.
  ENDIF.

  IF p_error NE space.
    MOVE : 'I'     TO r_msgty-sign,
           'EQ'    TO r_msgty-option,
           'E'     TO r_msgty-low.
    APPEND r_msgty.
  ENDIF.
ENDFORM.                    " make_message_type_ranges
