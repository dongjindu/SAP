*&---------------------------------------------------------------------*
*& Include MZAPM10_INFOTOP                                             *
*&                                                                     *
*&---------------------------------------------------------------------*

* For OK code
DATA: OK_CODE      LIKE SY-UCOMM,
      SAVE_OK_CODE LIKE OK_CODE.

* For PF-STATUS and Titlebar
CLASS LCL_PS DEFINITION DEFERRED.
DATA: CRV_PS TYPE REF TO LCL_PS.
DATA: TITLE(80).   "Title
****(BEGIN) Itab & WA For FOR Dynamic Menu
DATA:  IT_FUNC TYPE STANDARD TABLE OF RSMPE-FUNC.
*
*DATA: BEGIN OF itabfunc OCCURS 0,
*         fcode LIKE rsmpe-func,
*       END OF itabfunc.
DATA:  WA_FUNC LIKE LINE OF IT_FUNC.

DATA: DYNTB(50). " VALUE 'LOGI'.  "For dynamic titlebar

* Constants for Dynamic Function Code Text
CONSTANTS: CO_TREE_ON(30)   VALUE 'Tree On',
           CO_TREE_OFF(30)  VALUE 'Tree Off'.

DATA: DYNFTEXT(50). " VALUE co_tree_off. "For dynamic function text

****(END) Itab & WA For FOR Dynamic Menu

**** Begin of Tree
* Info Variable for Tree Event
DATA: IO_EVENT(30),
      IO_NODE_KEY(30) TYPE C,
      IO_ITEM_NAME    TYPE TV_ITMNAME,                      "Char 12
      IO_HEADER_NAME  TYPE TV_HDRNAME.                      "Char 72

CLASS LCL_H_TREE DEFINITION DEFERRED.
*If the first access to a global class in a program is to its static
*components or in the definition of an event handler method, you must
*load it explicitly using the statement CLASS class DEFINITION LOAD.
CLASS CL_GUI_CFW DEFINITION LOAD.

DATA: CRV_H_TREE    TYPE REF TO LCL_H_TREE.
* Docking Container
DATA: CRV_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA: CRV_TREE_MODEL        TYPE REF TO CL_COLUMN_TREE_MODEL.
*
DATA: EVENT            TYPE CNTL_SIMPLE_EVENT,
      EVENTS           TYPE CNTL_SIMPLE_EVENTS,
      HIERARCHY_HEADER LIKE TREEMHHDR.
"Tree Model: Attributes of the Hierarchy Heading

* For tree node
DATA: ITEM_TABLE TYPE TREEMCITAB,    "Column Tree Model: Item Table
      ITEM       TYPE TREEMCITEM,    "Column Tree Model: Item
      NODE_KEY   TYPE TM_NODEKEY,    "Tree Model: Key of a node
      NODE_DESC(50).                 "Description
**** End of Tree

* Constants for Visible, Invisible
CONSTANTS: CO_VISIBLE_TRUE  VALUE '1',
           CO_VISIBLE_FALSE VALUE '0'.

**** Begin of Program, Screen
DATA  WA_PGM     LIKE SY-CPROG.   " LIKE trdirt-name.
DATA  WA_SCR     TYPE SY-DYNNR.
DATA: WA_0110    LIKE SY-DYNNR.   "For Subscreen of Screen 0110
**** End of Program, Screen
