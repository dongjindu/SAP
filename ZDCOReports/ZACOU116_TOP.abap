*----------------------------------------------------------------------*
*   INCLUDE ZACOU116_TOP                                               *
*----------------------------------------------------------------------*

TABLES: ZTCOU102.

* Internal Table for Display
TYPES: BEGIN OF TY_OUT.
        INCLUDE STRUCTURE ZTCOU116.
TYPES:  CHK,
        CELLTAB TYPE LVC_T_STYL,
       END OF TY_OUT.

TYPES: BEGIN OF TY_102,
         KOKRS TYPE KOKRS,
         BDATJ TYPE BDATJ,
         POPER TYPE POPER,
         KALKA TYPE CK_KALKA,
         VER   TYPE ZVER1,
         MATNR TYPE MATNR,
         WERKS TYPE WERKS_D,
         LIFNR TYPE LIFNR,
         WERTN TYPE ZWERTN1,
         FRG   TYPE ZFRG1,
         OTH   TYPE ZOTH1,
       END OF TY_102.

DATA: GT_116 TYPE TABLE OF ZTCOU116 WITH HEADER LINE,
      GT_OUT TYPE TABLE OF TY_OUT   WITH HEADER LINE,
      GT_102 TYPE TABLE OF TY_102   WITH HEADER LINE.

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
* Change data
  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    " handle_data_changed

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.
