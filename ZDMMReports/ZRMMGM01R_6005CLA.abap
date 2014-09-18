*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM01R_6005CLA                                          *
*----------------------------------------------------------------------*
TYPES: ty_func TYPE TABLE OF rsmpe-func.
TYPES: ty_ps(20).
TYPES: ty_tb(60).
TYPES: ty_title(80).
*---------------------------------------------------------------------*
*       CLASS lcl_ps DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_ps DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
                    IMPORTING im_ps      TYPE ty_ps
                              im_it_func TYPE ty_func
                              im_tb      TYPE ty_tb
                              im_title   TYPE ty_title..
  PRIVATE SECTION.
    DATA: ps    TYPE ty_ps,
          tb    TYPE ty_tb,
          title TYPE ty_title.
    DATA: it_func TYPE ty_func.  "Internal table
ENDCLASS.                    "lcl_ps DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_ps IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_ps IMPLEMENTATION.
  METHOD constructor.
    ps      = im_ps.
    tb      = im_tb.
    it_func = im_it_func.
    title   = im_title.
    SET PF-STATUS ps EXCLUDING it_func.
    SET TITLEBAR  tb WITH title.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_ps IMPLEMENTATION
