*&---------------------------------------------------------------------*
*&  Include           ZRHR_APP_DOC_CREATE_BDCTOP
*&---------------------------------------------------------------------*

TABLES sscrfields.

DATA: ok_code       TYPE sy-ucomm.

* alv variable definition
DATA: gr_cont       TYPE REF TO cl_gui_custom_container,
      gr_grid       TYPE REF TO cl_gui_alv_grid.
DATA: st_layo       TYPE lvc_s_layo.
DATA: it_fcat       TYPE lvc_t_fcat WITH HEADER LINE.

* header title
DATA: BEGIN OF gs_title,
        col_01      TYPE string,  " duration(from)
        col_02      TYPE string,  " duration(to)
        col_03      TYPE string,  " appraisee id
        col_04      TYPE string,  " 1st evaluator id(appraiser id)
        col_05      TYPE string,  " 1st coordinator id
        col_06      TYPE string,  " 2nd evaluator id
        col_07      TYPE string,  " 2nd coordinator id
        col_08      TYPE string,  " 3rd evaluator id
        col_09      TYPE string,  " 3rd coordinator id
        col_10      TYPE string,  " approver id
        col_11      TYPE string,  " 4th coordinator id
        col_12      TYPE string,  " HR Team id
        col_13      TYPE string,  " Error flag
        col_14      TYPE string,  " Error message
      END OF gs_title,
      gt_title      LIKE TABLE OF gs_title.

* excel upload file form
DATA: BEGIN OF record OCCURS 0,
        begda(10),  " duration(from)
        endda(10),  " duration(to)
        appee(8),   " appraisee id
        apper(8),   " 1st evaluator id(appraiser id)
        cori1(8),   " 1st coordinator id
        evai2(8),   " 2nd evaluator id
        cori2(8),   " 2nd coordinator id
        evai3(8),   " 3rd evaluator id
        cori3(8),   " 3rd coordinator id
        appri(8),   " approver id
        cori4(8),   " 4th coordinator id
        hrtmi(8),   " HR Team id
      END OF record.

DATA: gs_record     TYPE zshr_adc_bdc,
      gt_record     TYPE ztyhr_adc_bdc.

DATA: g_down_path   TYPE string.

* OLE Object variable
DATA: application   TYPE ole2_object,
      workbook      TYPE ole2_object,
      sheet         TYPE ole2_object,
      cells         TYPE ole2_object,
      color         TYPE ole2_object,
      border        TYPE ole2_object,
      bold          TYPE ole2_object,
      size          TYPE ole2_object.

DATA: icon_name(20),
      icon_text(20),
      info_text     TYPE icont-quickinfo.

CONSTANTS: c_row    TYPE i VALUE 65536,
           c_col    TYPE i VALUE 256,
           c_massn  TYPE massn VALUE 'X3', "Action type
           c_massg  TYPE massg VALUE '  '. "Action reason

*&********************************************************************
*    Selection Screen
**&********************************************************************
SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_fname LIKE rlgrap-filename DEFAULT 'Assign Data Sources File !',
            p_ftype LIKE rlgrap-filetype DEFAULT 'DAT'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) text-c01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(60) text-c03.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(60) text-c02.
SELECTION-SCREEN END OF LINE.

*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sscrfields-functxt_01  = 'Layout Download'.

  icon_name = 'ICON_EXPORT'.
  icon_text = 'LayOutDownload'.
  info_text = 'LayOutDownload'.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_name
      text                  = icon_text
      info                  = info_text
    IMPORTING
      result                = sscrfields-functxt_01
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.
