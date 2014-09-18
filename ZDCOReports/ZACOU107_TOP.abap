*----------------------------------------------------------------------*
*   INCLUDE ZACOU107_TOP                                               *
*----------------------------------------------------------------------*
TABLES: ztcou102,    " [CO] Costing Result
        ztcou104,    " [CO] Variance Analysis Code
        ztcou106.    " [CO] Calculate Variances

* Internal Table for Display
TYPES: BEGIN OF ty_ztcou106,
         kokrs  TYPE kokrs,
         bdatj  TYPE bdatj,
         kalka  TYPE ck_kalka,
         id	    TYPE zid1,
         poper  TYPE zpoper,
         upgvc  TYPE zupgvc,
         compn  TYPE idnrk,
         kstar  TYPE kstar,
         lifnr  TYPE lifnr,
         kzust1 TYPE zkzust1,
         pmenge TYPE zpmenge,
         pwertn TYPE zpwertn,
         menge  TYPE menge_pos,
         wertn  TYPE zwertn,
         kpein  TYPE kpein,
         meeht  TYPE meins,
         dmenge TYPE zdmenge,
         dwertn TYPE zdwertn,
         wertn1 TYPE zwertn1_1,
         kzust2 TYPE zkzust2,
         wertn2 TYPE zwertn2_1,
         kzust3 TYPE zkzust3,
         wertn3 TYPE zwertn3,
         maktx  TYPE maktx,
         waers  TYPE waers,
* UD1K941594 by IG.MOON 9/18/07
* {
         zrclss TYPE zrclss,
         infrsn TYPE zkzust1,
         ekgrp  TYPE ekgrp,
* }
       END OF ty_ztcou106.

TYPES: BEGIN OF ty_out.
INCLUDE   TYPE ty_ztcou106.
TYPES:   wchk,
         chk,
         celltab   TYPE lvc_t_styl,
         tabcolor  TYPE slis_t_specialcol_alv,
         upgvc_t   TYPE maktx,
* by ig.moon 5/22/2008 {
        lock,
        lock_ico(4),  " TYPE icon,
* }
       END OF ty_out.

RANGES r_kzust FOR ztcou106-kzust.

DATA: gv_cnt      TYPE i,              " Total count
      gv_index    TYPE sytabix,        " Row Index
      gv_sel      TYPE i,              " Count of Change data
      bezei       TYPE bezei,
      gt_ztcou106 TYPE TABLE OF ty_ztcou106 WITH HEADER LINE,
      gt_out      TYPE TABLE OF ty_out      WITH HEADER LINE.

TYPES ddshretval_table TYPE TABLE OF ddshretval.

*Icon constants
constants: gc_red_icon(4)    value '@0A@'.  "ICON_LED_RED
constants: gc_green_icon(4)  value '@08@'.  "ICON_LED_GREEN
constants: gc_yellow_icon(4) value '@09@'.  "ICON_LED_YELLOW


*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
               IMPORTING er_data_changed,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
                          IMPORTING e_object e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
                          IMPORTING e_ucomm,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
                IMPORTING sender
                          e_fieldname
                          e_fieldvalue
                          es_row_no
                          er_event_data
                          et_bad_cells
                          e_display,

      my_f4 IMPORTING sender        TYPE REF TO cl_gui_alv_grid
                      et_bad_cells  TYPE lvc_t_modi
                      es_row_no     TYPE lvc_s_roid
                      er_event_data TYPE REF TO cl_alv_event_data
                      e_display     TYPE c
                      e_fieldname   TYPE lvc_fname
            EXPORTING lt_f4         TYPE ddshretval_table.


ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
* Setting for Change data
  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD handle_toolbar.
    PERFORM toolbar USING e_object e_interactive.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM user_command USING e_ucomm.
  ENDMETHOD.

* Get values of possible entries
  METHOD on_f4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                                                " on_f4

  METHOD my_f4.
    PERFORM my_f4 TABLES lt_f4
                  USING  sender
                         et_bad_cells
                         es_row_no
                         er_event_data
                         e_display
                         e_fieldname
                         'GT_OUT'.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

DATA  flag_data_changed.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
