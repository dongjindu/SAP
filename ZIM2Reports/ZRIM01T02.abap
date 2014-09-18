*&---------------------------------------------------------------------*
*&  INCLUDE ZRIM01T02                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : COLUMN TREE CONTROL용 데이타 선언                     *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2002.02.12                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
DATA : G_ZFSEQ  LIKE   ZSBKPF-ZFSEQ.
data: return_code type i.

  CLASS LCL_APPLICATION DEFINITION DEFERRED.
  CLASS CL_GUI_CFW DEFINITION LOAD.

* CAUTION: MTREEITM is the name of the item structure which must
* be defined by the programmer. DO NOT USE MTREEITM!
  TYPES: ITEM_TABLE_TYPE LIKE STANDARD TABLE OF MTREEITM
         WITH DEFAULT KEY.

  DATA: G_APPLICATION TYPE REF TO LCL_APPLICATION,
        G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
        G_TREE TYPE REF TO CL_GUI_COLUMN_TREE,
        G_OK_CODE TYPE SY-UCOMM.

* Fields on Dynpro 100
  DATA: G_EVENT(30),
        G_NODE_KEY     TYPE TV_NODEKEY,
        G_NODE_KEY_OLD TYPE TV_NODEKEY,
        G_ITEM_NAME    TYPE TV_ITMNAME,
        G_HEADER_NAME  TYPE TV_HDRNAME.



*------------------------------------------------------------
*> Class 선언부.
*------------------------------------------------------------
CLASS LCL_APPLICATION DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_NODE_DOUBLE_CLICK
        FOR EVENT NODE_DOUBLE_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY,
      HANDLE_HEADER_CLICK
        FOR EVENT HEADER_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING HEADER_NAME,
      HANDLE_EXPAND_NO_CHILDREN
        FOR EVENT EXPAND_NO_CHILDREN
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY,
      HANDLE_ITEM_DOUBLE_CLICK
        FOR EVENT ITEM_DOUBLE_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_BUTTON_CLICK
        FOR EVENT BUTTON_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_LINK_CLICK
        FOR EVENT LINK_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_CHECKBOX_CHANGE
        FOR EVENT CHECKBOX_CHANGE
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME CHECKED.
ENDCLASS.

CLASS LCL_APPLICATION IMPLEMENTATION.

  METHOD HANDLE_NODE_DOUBLE_CLICK.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
    G_EVENT = 'NODE_DOUBLE_CLICK'.
    G_NODE_KEY = NODE_KEY.
    CLEAR: G_ITEM_NAME, G_HEADER_NAME.
  ENDMETHOD.

  METHOD HANDLE_HEADER_CLICK.
    " this method handles header click event of the tree
                                       " control instance

    " show the name of the clicked header in a dynpro field
    G_EVENT = 'HEADER_CLICK'.
    G_HEADER_NAME = HEADER_NAME.
    CLEAR: G_NODE_KEY, G_ITEM_NAME.
  ENDMETHOD.

  METHOD  HANDLE_ITEM_DOUBLE_CLICK.
    " this method handles the item double click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    G_EVENT = 'ITEM_DOUBLE_CLICK'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.


    CLEAR G_HEADER_NAME.
  ENDMETHOD.

  METHOD  HANDLE_LINK_CLICK.
    " this method handles the link click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
    G_EVENT = 'LINK_CLICK'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.
    CLEAR G_HEADER_NAME.
  ENDMETHOD.

  METHOD  HANDLE_BUTTON_CLICK.
    " this method handles the button click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
    G_EVENT = 'BUTTON_CLICK'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.
    CLEAR G_HEADER_NAME.
  ENDMETHOD.

  METHOD  HANDLE_CHECKBOX_CHANGE.
    " this method handles the checkbox_change event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
    G_EVENT = 'CHECKBOX_CHANGE'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.
    CLEAR  G_HEADER_NAME.
  ENDMETHOD.


  METHOD HANDLE_EXPAND_NO_CHILDREN.
    DATA: NODE_TABLE TYPE TREEV_NTAB,
          NODE TYPE TREEV_NODE,
          ITEM_TABLE TYPE ITEM_TABLE_TYPE,
          ITEM TYPE MTREEITM.

* show the key of the expanded node in a dynpro field
    G_EVENT = 'EXPAND_NO_CHILDREN'.
    G_NODE_KEY = NODE_KEY.
    CLEAR: G_ITEM_NAME, G_HEADER_NAME.

*   if node_key = 'Child1'. "#EC NOTEXT
** add two nodes
*
** Node with key 'New1'
*      CLEAR NODE.
*      NODE-NODE_KEY = 'New1'.          "#EC NOTEXT
*      NODE-RELATKEY = 'Child1'.
*      NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
*      APPEND NODE TO NODE_TABLE.
*
** Node with key 'New2'
*      CLEAR NODE.
*      NODE-NODE_KEY = 'New2'.          "#EC NOTEXT
*      NODE-RELATKEY = 'Child1'.
*      NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
*      NODE-N_IMAGE = '@10@'.
*      APPEND NODE TO NODE_TABLE.
*
** Items of node with key 'New1'
*      CLEAR ITEM.
*      ITEM-NODE_KEY = 'New1'.
*      ITEM-ITEM_NAME = 'Column1'.      "#EC NOTEXT
*      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*      ITEM-TEXT = 'New1 Col. 1'.       "#EC NOTEXT
*      APPEND ITEM TO ITEM_TABLE.
*
*      CLEAR ITEM.
*      ITEM-NODE_KEY = 'New1'.
*      ITEM-ITEM_NAME = 'Column2'.      "#EC NOTEXT
*      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
*      item-t_image = '@3C@'.
*      APPEND ITEM TO ITEM_TABLE.
*
*      CLEAR ITEM.
*      ITEM-NODE_KEY = 'New1'.
*      ITEM-ITEM_NAME = 'Column3'.      "#EC NOTEXT
*      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*      ITEM-TEXT = 'New1 Col. 3'.       "#EC NOTEXT
*      APPEND ITEM TO ITEM_TABLE.
*
** Items of node with key 'New2'
*      CLEAR ITEM.
*      ITEM-NODE_KEY = 'New2'.
*      ITEM-ITEM_NAME = 'Column1'.      "#EC NOTEXT
*      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*      ITEM-TEXT = 'New2 Col. 1'.       "#EC NOTEXT
*      APPEND ITEM TO ITEM_TABLE.
*
*      CLEAR ITEM.
*      ITEM-NODE_KEY = 'New2'.
*      ITEM-ITEM_NAME = 'Column2'.      "#EC NOTEXT
*      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*      ITEM-TEXT = 'New2 Col. 2'.       "#EC NOTEXT
*      APPEND ITEM TO ITEM_TABLE.
*
*      CLEAR ITEM.
*      ITEM-NODE_KEY = 'New2'.
*      ITEM-ITEM_NAME = 'Column3'.      "#EC NOTEXT
*      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
*      ITEM-TEXT = 'New2 Col. 3'.       "#EC NOTEXT
*      APPEND ITEM TO ITEM_TABLE.
*
*
*      CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
*        EXPORTING
*          NODE_TABLE = NODE_TABLE
*          ITEM_TABLE = ITEM_TABLE
*          ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
*        EXCEPTIONS
*          FAILED = 1
*          CNTL_SYSTEM_ERROR = 3
*          ERROR_IN_TABLES = 4
*          DP_ERROR = 5
*          TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
*      IF SY-SUBRC <> 0.
*        MESSAGE A000.
*      ENDIF.
*    endif.
  ENDMETHOD.

ENDCLASS.
