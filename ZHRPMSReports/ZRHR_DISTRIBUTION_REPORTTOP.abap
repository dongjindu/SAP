*&---------------------------------------------------------------------*
*&  Include           ZRHR_DISTRIBUTION_REPORTTOP
*&---------------------------------------------------------------------*

DATA: ok_code       TYPE sy-ucomm.

* alv variable definition
DATA: gr_cont       TYPE REF TO cl_gui_custom_container,
      gr_grid       TYPE REF TO cl_gui_alv_grid.
DATA: gs_layo       TYPE lvc_s_layo.
DATA: gt_fcat       TYPE lvc_t_fcat WITH HEADER LINE.

DATA: gs_dpool      TYPE zthr_dpool,    " Selected value at Distribution search help
      gs_addps      TYPE zthr_addps.    " Selected value at Department search help

DATA: BEGIN OF gt_result OCCURS 0,
        title       TYPE text50,
        rat01       TYPE text50,
        rat02       TYPE text50,
        rat03       TYPE text50,
        rat04       TYPE text50,
        rat05       TYPE text50,
        remark      TYPE text100,
      END OF gt_result.


*&********************************************************************
*    Selection Screen
*&********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t80.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(20) text-t81 FOR FIELD p_year.
PARAMETERS: p_year  TYPE zdhr_year AS LISTBOX VISIBLE LENGTH 10.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 2.
PARAMETERS: p_type1 TYPE char1 RADIOBUTTON GROUP gr1 USER-COMMAND r_type.  " Distribution Pool
SELECTION-SCREEN COMMENT (25) text-t82 FOR FIELD p_type1.
SELECTION-SCREEN POSITION 32.
PARAMETERS: p_type2 TYPE char1 RADIOBUTTON GROUP gr1.                      " Approver Distribution
SELECTION-SCREEN COMMENT (25) text-t83 FOR FIELD p_type2.
SELECTION-SCREEN POSITION 62.
PARAMETERS: p_type3 TYPE char1 RADIOBUTTON GROUP gr1.                      " Department Distribution
SELECTION-SCREEN COMMENT (25) text-t84 FOR FIELD p_type3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(20) text-t85 FOR FIELD p_disp MODIF ID tp1.
PARAMETERS: p_disp  TYPE persno MODIF ID tp1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(20) text-t86 FOR FIELD p_adis MODIF ID tp2.
PARAMETERS: p_adis  TYPE persno MODIF ID tp2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(20) text-t87 FOR FIELD p_ddis MODIF ID tp3.
PARAMETERS: p_ddis  TYPE persno MODIF ID tp3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 200 .
