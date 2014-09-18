*----------------------------------------------------------------------*
*   INCLUDE ZRIMMIDUPCLS                                               *
*----------------------------------------------------------------------*
TYPES: TY_FUNC TYPE TABLE OF RSMPE-FUNC.
TYPES: TY_PS(20).
TYPES: TY_TB(60).
TYPES: TY_TITLE(80).
*---------------------------------------------------------------------*
*       CLASS lcl_ps DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_PS DEFINITION.
  PUBLIC SECTION.
    METHODS CONSTRUCTOR
                    IMPORTING IM_PS      TYPE TY_PS
                              IM_IT_FUNC TYPE TY_FUNC
                              IM_TB      TYPE TY_TB
                              IM_TITLE   TYPE TY_TITLE..
  PRIVATE SECTION.
    DATA: PS    TYPE TY_PS,
          TB    TYPE TY_TB,
          TITLE TYPE TY_TITLE.
    DATA: IT_FUNC TYPE TY_FUNC.  "Internal table
ENDCLASS.                    "lcl_ps DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_ps IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_PS IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    PS      = IM_PS.
    TB      = IM_TB.
    IT_FUNC = IM_IT_FUNC.
    TITLE   = IM_TITLE.
    SET PF-STATUS PS EXCLUDING IT_FUNC.
    SET TITLEBAR  TB WITH TITLE.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_ps IMPLEMENTATION
