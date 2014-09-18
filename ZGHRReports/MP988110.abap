*----------------------------------------------------------------------*
*                                                                      *
*       Data definition for infotype 9881                              *
*                                                                      *
*----------------------------------------------------------------------*
PROGRAM mp988100 MESSAGE-ID rp.

TABLES: p9881.
* the following tables are filled globally:
* T001P, T500P
* they can be made available with a TABLES-statement

FIELD-SYMBOLS: <pnnnn> STRUCTURE p9881
                       DEFAULT p9881.

DATA: psave LIKE p9881.

CONTROLS: tc_history TYPE TABLEVIEW USING SCREEN 2000,
          tc_plan    TYPE TABLEVIEW USING SCREEN 2000.

DATA: BEGIN OF gt_master OCCURS 0,
        objid           TYPE zghrlt0004-objid,
        stext           TYPE zghrlt0004-stext,
      END OF gt_master.
DATA: gt_history TYPE TABLE OF zghrls0002 WITH HEADER LINE.
*DATA: BEGIN OF gt_history OCCURS 0.
*.INCLUDE TYPE zghrls0002.
*DATA: mark    TYPE  char1,
*      END OF gt_history.

DATA: BEGIN OF gt_plan OCCURS 0.
.
INCLUDE TYPE zghrls0003.
DATA: mark    TYPE char1,
      END OF gt_plan.
