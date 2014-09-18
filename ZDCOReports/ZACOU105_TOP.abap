*----------------------------------------------------------------------*
*   INCLUDE ZACOU105_TOP                                               *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  INCLUDE ZACOU105_TOP                                                *
*----------------------------------------------------------------------*
*  Define Variant & tables & local class
*----------------------------------------------------------------------*
TABLES ZTCOU105.       " [CO] Following Part Information

* Internal Table for Display
TYPES: BEGIN OF TY_OUT.
        INCLUDE STRUCTURE ZTCOU105.
TYPES:  CELLTAB TYPE LVC_T_STYL,
       END OF TY_OUT.

DATA: BEZEI       TYPE BEZEI,
      GV_CNT      TYPE I,
      GT_ZTCOU105 TYPE TABLE OF ZTCOU105 WITH HEADER LINE,
      GT_OUT      TYPE TABLE OF TY_OUT   WITH HEADER LINE.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DATA_CHANGED
                FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
                IMPORTING ER_DATA_CHANGED.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
* Setting for Change data
  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    " handle_data_changed

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.
