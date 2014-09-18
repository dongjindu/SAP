*&---------------------------------------------------------------------*
*& Include MZAHR0005TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzahr0005 MESSAGE-ID zmhr.

*... tables
TABLES: zthr_et01,   "module, group
        zthr_et02.   "code

CONTROLS: tc9000 TYPE TABLEVIEW USING SCREEN 9000,
tc9100 TYPE TABLEVIEW USING SCREEN 9100.

*... internal tables
DATA: BEGIN OF it_et01 OCCURS 0.
        INCLUDE STRUCTURE zthr_et01.
DATA: chkbx,
      tabix    LIKE sy-tabix,
      END OF it_et01.

DATA: del_et01 LIKE zthr_et01 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_et02 OCCURS 0.
        INCLUDE STRUCTURE zthr_et02.
DATA: chkbx,
      tabix    LIKE sy-tabix,
      END OF it_et02.

DATA: del_et02 LIKE zthr_et02 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_stats OCCURS 1,
      fcode    LIKE rsmpe-func,
      END OF it_stats.

DATA: BEGIN OF it_moval OCCURS 1,
      zmodl    LIKE zthr_et01-zmodl,
      zmtxt    LIKE zthr_et01-zmtxt.
DATA: END OF it_moval.

DATA: BEGIN OF it_grval OCCURS 1,
      zgrup    LIKE zthr_et01-zgrup,
      zgtxt    LIKE zthr_et01-zgtxt.
DATA: END OF it_grval.

DATA: it_field     LIKE help_value OCCURS 1 WITH HEADER LINE,
      dynpfields   LIKE STANDARD TABLE OF dynpread WITH HEADER LINE.

*... variants
DATA: w_text1(50),
      w_text2(50),
      w_zmodl      LIKE zthr_et01-zmodl,
      w_zmtxt      LIKE zthr_et01-zmtxt,
      w_zgrup      LIKE zthr_et01-zgrup,
      w_zgtxt      LIKE zthr_et01-zgtxt.

DATA: w_count      LIKE sy-tabix,
      w_modes(1)   TYPE n.

DATA: w_fname      LIKE  help_info-fieldname,
      w_tabix      LIKE  sy-tabix,
      w_fldvl      LIKE  help_info-fldvalue.

DATA: w_field(20).
DATA: w_tc_index TYPE i.
