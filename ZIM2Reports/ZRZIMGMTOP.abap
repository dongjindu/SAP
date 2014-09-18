*&---------------------------------------------------------------------*
*& Include ZRZIMGMTOP                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 IMG 통합 관리 Data Define Inculde          *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
REPORT SAPMZIMGM MESSAGE-ID DW.

INCLUDE <ctldef>.
INCLUDE cnt4defs.
TYPE-POOLS: cntl, stree.
TABLES: dwtree, rseu0, demotree, demotreet, seucomm, snodetext.
TABLES: trdir, tstc, demo_rb, tstct.

TABLES: ZTIMIMG08,
        ZTIMGTXT.

DATA: REPID LIKE SY-REPID,
      DYNNR LIKE SY-DYNNR.

DATA: return_code type i.

DATA: answer.
  DATA: nodekey TYPE tv_nodekey,
          node TYPE snodetext.
  data: nodekeytab type treev_nks.
  data: itemname type tv_itmname.
  clear: node, nodekey, itemname.

  DATA: G_EVENT(30),
        G_ZFCD         LIKE ZTIMGTXT-ZFCD,
        G_NODE_KEY     TYPE TV_NODEKEY,
        G_NODE_KEY_OLD TYPE TV_NODEKEY,
        G_NODE_KEY_TXT TYPE TV_NODEKEY,
        G_ITEM_NAME    TYPE TV_ITMNAME,
        G_HEADER_NAME  TYPE TV_HDRNAME.



* Define Table Type For Data Exchange.
TYPES: BEGIN OF MYTABLE_LINE,
       TDLINE LIKE ZTIMGTXT-TDLINE,
       END OF MYTABLE_LINE.

* Table To Exchange Text.
DATA SRC     TYPE TABLE OF MYTABLE_LINE.
DATA SRC_TMP TYPE TABLE OF ZTIMGTXT WITH HEADER LINE.
*DATA : BEGIN OF SRC_TMP OCCURS 0.
*       INCLUDE STRUCTURE ZTIMGTXT.
*DATA : END OF SRC_TMP.
*DATA: BEGIN OF src OCCURS 500,
*              line(72),
*      END   OF src.
DATA: ctrl_node_tab TYPE STANDARD TABLE OF treev_node,
      ctrl_node_tab2 TYPE STANDARD TABLE OF treev_node,
      ctrl_item_tab TYPE STANDARD TABLE OF demo_item,
      ctrl_node TYPE treev_node,
      ctrl_item TYPE demo_item,
      header_tab TYPE STANDARD TABLE OF treev_hdr,
      column_tab TYPE STANDARD TABLE OF treev_col,
      header TYPE treev_hdr,
      hierarchy_header TYPE treev_hhdr,
      column TYPE treev_col.
DATA  fcode LIKE sy-ucomm.
DATA editor TYPE REF TO cl_gui_textedit.
DATA:  level TYPE i VALUE -1.

DATA: BEGIN OF tree OCCURS 100.
        INCLUDE STRUCTURE streenode.
DATA: END OF tree.
DATA: BEGIN OF nodetab OCCURS 100.
        INCLUDE STRUCTURE snodetext.
DATA: END OF nodetab.

DATA: customer_id(10) VALUE 'DEMO'.
DATA: BEGIN OF layout_id,
       'DEMO_LAYOUT_',
       user LIKE sy-uname,
      END OF layout_id.

DATA: BEGIN OF marktab OCCURS 10.
        INCLUDE STRUCTURE snodetext.
DATA: END OF marktab.

DATA: bool.
DATA: true  LIKE bool VALUE 'T'.
DATA: false LIKE bool VALUE 'F'.

DATA  value_request  LIKE bool.
DATA  save_necessary LIKE bool.

DATA: ok_code(4).

DATA: BEGIN OF layout OCCURS 1.
        INCLUDE STRUCTURE seutexpand.
DATA: END OF layout.

DATA: cucol TYPE i,
      culin TYPE i,
      licol TYPE i,
      lilin TYPE i.
DATA: first_node LIKE streenode-id.

DATA: tlength(2) TYPE n VALUE '40',
      col_begin  TYPE i,
      col_end    TYPE i.


DATA: BEGIN OF deltab OCCURS 10,
        name    LIKE aptree-name,
      END OF deltab.

DATA: BEGIN OF add_nodes OCCURS 10.
        INCLUDE STRUCTURE streenode.
DATA: END OF add_nodes.
DATA: BEGIN OF mod_nodes OCCURS 10.
        INCLUDE STRUCTURE streenode.
DATA: END OF mod_nodes.
DATA: BEGIN OF del_nodes OCCURS 10.
        INCLUDE STRUCTURE streenode.
DATA: END OF del_nodes.


DATA: act_edit(2) VALUE '02',
      act_show(2) VALUE '03'.
DATA  repname LIKE sy-repid.
DATA  BEGIN OF texttab OCCURS 50.
        INCLUDE STRUCTURE textpool.
DATA  END   OF texttab.
DATA  demo_id(10) VALUE 'DEMO'.
DATA  systemtype(10).

DATA: TEXT_ID(3)  TYPE N,
      FIELDNM(8)  TYPE C,
      W_STATUS    TYPE C.

FIELD-SYMBOLS <F>.
