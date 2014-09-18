*----------------------------------------------------------------------*
*   INCLUDE MZPP_APP202O01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_100 OUTPUT.
  SET PF-STATUS 'MAIN'.

  IF G_APPLICATION IS INITIAL.
     CREATE OBJECT g_application.
  ENDIF.

  IF G_TREE IS INITIAL.
    " The Tree Control has not been created yet.
    " Create a Tree Control and insert nodes into it.
    PERFORM CREATE_AND_INIT_TREE.
    SV_PROG = SY-REPID .
    SV_SCNO = '0001'   .
  ENDIF.
ENDMODULE.                 " PBO_0100  OUTPUT
*** INCLUDE simple_tree_control_demoO01

*&---------------------------------------------------------------------*
*&      Module  STATUS_1101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1101 OUTPUT.
* SET TITLEBAR 'TITLE_XXX' WITH 'TEST - 1101' .
ENDMODULE.                 " STATUS_1101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1102 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_1102  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1103 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_1103  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1104 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_1104  OUTPUT
