*&---------------------------------------------------------------------*
*& INCLUDE ZRZIMGTOP                                                   *
*&---------------------------------------------------------------------*
*&  Program Name  : Data Define Include                                *
*&  Created By    : INFOLINK Ltd.                                      *
*&  Created On    : 2000.02.12                                         *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* Title Text Define
*-----------------------------------------------------------------------
DATA : W_CREATE(6)        TYPE     C     VALUE   'Create',
       W_CHANGE(6)        TYPE     C     VALUE   'Change',
       W_DISPLAY(7)       TYPE     C     VALUE   'Display'.

*> For Password
DATA: MI_KEY          TYPE I VALUE 26101957,
      MI_PWD_LEN      TYPE I,
      MI_HANDLE       TYPE I.
DATA: W_ZTIMIMG11     LIKE ZTIMIMG11,
      W_ZTIMIMGTX     LIKE ZTIMIMGTX.

*-----------------------------------------------------------------------
* Table Control & Screen Assign.
*-----------------------------------------------------------------------
CONTROLS: TC_0020 TYPE TABLEVIEW USING SCREEN 0020,
          TC_0210 TYPE TABLEVIEW USING SCREEN 0210,
          TC_0220 TYPE TABLEVIEW USING SCREEN 0220,
          TC_0080 TYPE TABLEVIEW USING SCREEN 0080,
          TC_0800 TYPE TABLEVIEW USING SCREEN 0800,
          TC_1000 TYPE TABLEVIEW USING SCREEN 1000,
          TC_1100 TYPE TABLEVIEW USING SCREEN 1100,
          TC_1200 TYPE TABLEVIEW USING SCREEN 1200,
          TC_1400 TYPE TABLEVIEW USING SCREEN 1400,
          TC_1500 TYPE TABLEVIEW USING SCREEN 1500,
          TC_1600 TYPE TABLEVIEW USING SCREEN 1600,
          TC_1700 TYPE TABLEVIEW USING SCREEN 1700,
          TC_2100 TYPE TABLEVIEW USING SCREEN 2100,
          TC_2200 TYPE TABLEVIEW USING SCREEN 2200,
          TC_2300 TYPE TABLEVIEW USING SCREEN 2300,
          TC_2400 TYPE TABLEVIEW USING SCREEN 2400,
          TC_2500 TYPE TABLEVIEW USING SCREEN 2500,
          TC_9100 TYPE TABLEVIEW USING SCREEN 9100,
          TC_9200 TYPE TABLEVIEW USING SCREEN 9200,
          TC_9301 TYPE TABLEVIEW USING SCREEN 9301,
          TC_9401 TYPE TABLEVIEW USING SCREEN 9401,
          TC_0600 TYPE TABLEVIEW USING SCREEN 0600.

DATA : WA_COLS LIKE LINE OF TC_9301-COLS.

DATA : W_ACCPW(4),
       W_APPPW          LIKE ZTIMIMGTX-PASSW,   ">Password.
       W_NEWPW          LIKE ZTIMIMGTX-PASSW,   ">New Passwerd.
       W_CFMPW          LIKE ZTIMIMGTX-PASSW,   ">Password Confirm.
       W_PASSW          LIKE ZTIMIMGTX-PASSW.

*---------------------------------------------------------------------*
*     Tree Control                                                    *
*---------------------------------------------------------------------*
TABLES : MTREESNODE_FT.

CONSTANTS: LINE_LENGTH TYPE I VALUE 256.

* Define Table Type For Data Exchange.
TYPES: BEGIN OF MYTABLE_LINE,
          LINE LIKE ZTIMGTXT-TDLINE,
       END OF MYTABLE_LINE.

TYPES: BEGIN OF LINE_TEMP,
              LINE    TYPE MYTABLE_LINE,
              TDID(4) TYPE C.
TYPES: END OF LINE_TEMP.

DATA: IT_LINE_TEMP TYPE STANDARD TABLE OF LINE_TEMP WITH DEFAULT KEY
                                          WITH HEADER LINE.
DATA : IT_LINE      LIKE IT_LINE_TEMP OCCURS 0 WITH HEADER LINE.

DATA : W_ITTR_LINE LIKE SY-TABIX.
DATA : WA_ZSIMIMG20 LIKE ZTIMIMG20.

DATA: IT_IMG20 LIKE TABLE OF ZTIMIMG20 WITH HEADER LINE.

DATA: BEGIN OF IT_TRPHD OCCURS 0,
      ZFMARK    TYPE   C,
      BUKRS    LIKE ZTIMIMG20-BUKRS,
      ZFAPLDT  LIKE ZTIMIMG20-ZFAPLDT.
DATA: END OF IT_TRPHD.
*-------------------------------

*TREE MODE  IT_TAB.
DATA : BEGIN OF IT_V_TVKN OCCURS 0,
       NODE LIKE ZTIMIMG08-ZFCDTY,
       TEXT LIKE DD07T-DDTEXT,
       END   OF IT_V_TVKN.

DATA : GV_XTHEAD_UPDKZ TYPE I.
CLASS LCL_APPLICATION DEFINITION DEFERRED.
CLASS CL_GUI_CFW DEFINITION LOAD.
CLASS CL_GUI_COLUMN_TREE DEFINITION LOAD.

TYPES: ITEM_TABLE_TYPE LIKE STANDARD TABLE OF MTREEITM
         WITH DEFAULT KEY.

DATA: NODE TYPE TREEV_NODE,
        ITEM TYPE MTREEITM.
DATA: NODE_TABLE TYPE TREEV_NTAB,
      ITEM_TABLE TYPE ITEM_TABLE_TYPE.

DATA: G_APPLICATION  TYPE REF TO LCL_APPLICATION,
*      G_APPLICATION2 TYPE REF TO LCL_APPLICATION," DEFINITION DEFERRED,
      G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
*      G_CUSTOM_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_TREE  TYPE REF TO CL_GUI_LIST_TREE,
*      G_TREE2 TYPE REF TO CL_GUI_LIST_TREE,
      G_OK_CODE TYPE SY-UCOMM.

* Fields on Dynpro 100
DATA:   G_EVENT(30),
        G_NODE_KEY TYPE TV_NODEKEY,
*        G_NODE_KEY2 TYPE TV_NODEKEY,
        G_NODE_KEY_OLD TYPE TV_NODEKEY,
        G_NODE_KEY_NEW TYPE TV_NODEKEY,
        G_NODE_KEY_ALL TYPE TV_NODEKEY,
        G_ITEM_NAME TYPE TV_ITMNAME,
        G_HEADER_NAME TYPE TV_HDRNAME.

************************************************************************
*CLASS DEFINE.
CLASS LCL_APPLICATION DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_NODE_DOUBLE_CLICK
        FOR EVENT NODE_DOUBLE_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY,
*      HANDLE_NODE_DOUBLE_CLICK2
*        FOR EVENT NODE_DOUBLE_CLICK
*        OF CL_GUI_COLUMN_TREE
*        IMPORTING NODE_KEY,

      HANDLE_HEADER_CLICK
        FOR EVENT HEADER_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING HEADER_NAME,
*      HANDLE_HEADER_CLICK2
*        FOR EVENT HEADER_CLICK
*        OF CL_GUI_COLUMN_TREE
*        IMPORTING HEADER_NAME,

      HANDLE_EXPAND_NO_CHILDREN
        FOR EVENT EXPAND_NO_CHILDREN
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY,

      HANDLE_ITEM_DOUBLE_CLICK
        FOR EVENT ITEM_DOUBLE_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,
*      HANDLE_ITEM_DOUBLE_CLICK2
*        FOR EVENT ITEM_DOUBLE_CLICK
*        OF CL_GUI_COLUMN_TREE
*        IMPORTING NODE_KEY ITEM_NAME,

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

*---------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_APPLICATION IMPLEMENTATION.

  METHOD HANDLE_NODE_DOUBLE_CLICK.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
    G_EVENT = 'NODE_DOUBLE_CLICK'.
    G_NODE_KEY = NODE_KEY.
    CLEAR: G_ITEM_NAME, G_HEADER_NAME.
  ENDMETHOD.

*METHOD HANDLE_NODE_DOUBLE_CLICK2.
*    " this method handles the node double click event of the tree
*    " control instance
*
*    " show the key of the double clicked node in a dynpro field
*    G_EVENT = 'NODE_DOUBLE_CLICK'.
*    G_NODE_KEY2 = NODE_KEY.
*    CLEAR: G_ITEM_NAME, G_HEADER_NAME.
*  ENDMETHOD.

  METHOD HANDLE_HEADER_CLICK.
    " this method handles header click event of the tree
    " control instance

    " show the name of the clicked header in a dynpro field
    G_EVENT = 'HEADER_CLICK'.
    G_HEADER_NAME = HEADER_NAME.
    CLEAR: G_NODE_KEY, G_ITEM_NAME.
  ENDMETHOD.

*  METHOD HANDLE_HEADER_CLICK2.
*    " this method handles header click event of the tree
*                                       " control instance
*
*    " show the name of the clicked header in a dynpro field
*    G_EVENT = 'HEADER_CLICK'.
*    G_HEADER_NAME = HEADER_NAME.
*    CLEAR: G_NODE_KEY2, G_ITEM_NAME.
*  ENDMETHOD.

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

*  METHOD  HANDLE_ITEM_DOUBLE_CLICK2.
*    " this method handles the item double click event of the tree
*    " control instance
*
*    " show the key of the node and the name of the item
*    " of the double clicked item in a dynpro field
*    G_EVENT = 'ITEM_DOUBLE_CLICK'.
*    G_NODE_KEY2 = NODE_KEY.
*    G_ITEM_NAME = ITEM_NAME.
*    CLEAR G_HEADER_NAME.
*  ENDMETHOD.

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

    IF NODE_KEY = 'Child1'.                                 "#EC NOTEXT
* add two nodes

* Node with key 'New1'
      CLEAR NODE.
      NODE-NODE_KEY = 'New1'.                               "#EC NOTEXT
      NODE-RELATKEY = 'Child1'.
      NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
      APPEND NODE TO NODE_TABLE.

** Node with key 'New2'
*      CLEAR NODE.
*      NODE-NODE_KEY = 'New2'.          "#EC NOTEXT
*      NODE-RELATKEY = 'Child1'.
*      NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
*      NODE-N_IMAGE = '@10@'.
*      APPEND NODE TO NODE_TABLE.

* Items of node with key 'New1'
      CLEAR ITEM.
      ITEM-NODE_KEY = 'New1'.
      ITEM-ITEM_NAME = 'Column1'.                           "#EC NOTEXT
      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      ITEM-TEXT = 'New1 Col. 1'.                            "#EC NOTEXT
      APPEND ITEM TO ITEM_TABLE.

      CLEAR ITEM.
      ITEM-NODE_KEY = 'New1'.
      ITEM-ITEM_NAME = 'Column2'.                           "#EC NOTEXT
      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
      ITEM-T_IMAGE = '@3C@'.
      APPEND ITEM TO ITEM_TABLE.

      CLEAR ITEM.
      ITEM-NODE_KEY = 'New1'.
      ITEM-ITEM_NAME = 'Column3'.                           "#EC NOTEXT
      ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      ITEM-TEXT = 'New1 Col. 3'.                            "#EC NOTEXT
      APPEND ITEM TO ITEM_TABLE.

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


      CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
        EXPORTING
          NODE_TABLE = NODE_TABLE
          ITEM_TABLE = ITEM_TABLE
          ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
        EXCEPTIONS
          FAILED = 1
          CNTL_SYSTEM_ERROR = 3
          ERROR_IN_TABLES = 4
          DP_ERROR = 5
          TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
      IF SY-SUBRC <> 0.
        MESSAGE A000(TREE_CONTROL_MSG).
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------
*> 2002.02.20 KSB INSERT.
*> DOCKING CONTAINER Data Declare
*----------------------------------------------------------------------
CLASS LCL_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      HANDLE_ITEM_DOUBLE_CLICK
        FOR EVENT ITEM_DOUBLE_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,

      HANDLE_ITEM_CONTEXT_MENU_REQ
        FOR EVENT ITEM_CONTEXT_MENU_REQUEST
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME MENU,

      HANDLE_ITEM_CONTEXT_MENU_SEL
        FOR EVENT ITEM_CONTEXT_MENU_SELECT
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME FCODE,

      HANDLE_ITEM_KEYPRESS
        FOR EVENT ITEM_KEYPRESS
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME KEY.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_HANDLER IMPLEMENTATION.
  METHOD  HANDLE_ITEM_DOUBLE_CLICK.
    " this method handles the item double click event of the tree
    " control instance
*  DATA: L_STRUC_TAB like vcl_struc_tab.
*
*    READ TABLE VCL_STRUC_TAB INTO L_STRUC_TAB
*                             WITH KEY OBJPOS = NODE_KEY.
*    IF L_STRUC_TAB-OBJECT = VCL_AKT_VIEW.
*      NAVI_EVENT_OCCURED = 'Y'.
*      MESSAGE S517(SV).
*    ELSE.
*      VCL_NEXT_VIEW = L_STRUC_TAB-OBJECT.
*      NAVI_EVENT_OCCURED = 'X'.
*    ENDIF.
*  clear function.   * in VIEWCLUSTER_NEXT_ACTION
  ENDMETHOD.

  METHOD HANDLE_ITEM_CONTEXT_MENU_REQ.
    " this method handles the node context menu request event of the
    " tree control instance
    CALL METHOD MENU->LOAD_GUI_STATUS
        EXPORTING PROGRAM = 'SAPLSVCM'
                  STATUS  = 'NAVITREE'
                  MENU    = MENU.
  ENDMETHOD.

  METHOD HANDLE_ITEM_CONTEXT_MENU_SEL.
    " this method handles the context menu select event of the tree
    " control instance
*  DATA: L_STRUC_TAB like vcl_struc_tab.
*    CHECK FCODE = 'NAVI'.   " nur Navi im Kontextmen?enthalten
*    READ TABLE VCL_STRUC_TAB INTO L_STRUC_TAB
*                             WITH KEY OBJPOS = NODE_KEY.
*    IF L_STRUC_TAB-OBJECT = VCL_AKT_VIEW.
*      NAVI_EVENT_OCCURED = 'Y'.
*      MESSAGE S517(SV).
*    ELSE.
*      VCL_NEXT_VIEW = L_STRUC_TAB-OBJECT.
*      NAVI_EVENT_OCCURED = 'X'.
*    ENDIF.
  ENDMETHOD.

  METHOD HANDLE_ITEM_KEYPRESS.
*     CALL FUNCTION 'POPUP_DISPLAY_TEXT'
*        EXPORTING
**            LANGUAGE       = SY-LANGU
*             POPUP_TITLE    = TEXT-011
**            START_COLUMN   = 10
**            START_ROW      = 3
*             TEXT_OBJECT    = 'SM34_NAVI_F1'.
**            HELP_MODAL     = 'X'
  ENDMETHOD.

ENDCLASS.


* Docking Container
DATA CL_GUI_DOCKING_CONTAINER  TYPE REF TO CL_GUI_DOCKING_CONTAINER.
* Picture Control
DATA PICTURE TYPE REF TO CL_GUI_PICTURE.
* Definition of Control Framework
TYPE-POOLS CNDP.

DATA REPID TYPE SY-REPID.

DATA :
      EVENT_HANDLER     TYPE REF TO LCL_HANDLER,
      NAVI_TREE         TYPE REF TO CL_GUI_COLUMN_TREE,
      DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      TREE_CONTROL_CREATED(1) TYPE C,
      NAVI_EVENT_OCCURED(1) TYPE C,
      TREE_IS_DISABLED(1) TYPE C,         "Knoten des TreeCtrl disabled
      LAST_VIM_DYNNR LIKE SY-DYNNR,       "zuletzt aktives Pflegedynpro
      LAST_VIM_REPID LIKE SY-REPID.       "zuletzt aktive Pflegefugr


DATA: SUBSCR_TITLE(50) TYPE C,    " 'Navigation' (?erschrift Subscreen)
      SUBSCR_POSITEXT(19) TYPE C, " 'Stufe' (Position-Info Subscreen)
      DEACTIVATE_SUBSCR(1) TYPE C,   "Auswahl-Button im Subscr. deaktiv.
      INTENSIFIED_VIEW LIKE VCLSTRUC-OBJECT,
      LAST_INTENSIFIED_VIEW LIKE VCLSTRUC-OBJECT, "nur Ctrls
      POS                 TYPE I,                " aktuelle Position
      AKT_LINES           LIKE SY-TABIX VALUE 1,  " aktuelle Zeile
      MAX_LINES           LIKE SY-TABIX,          " maximale Zeilen
      READ_INDEX          LIKE SY-STEPL,          " Index ausgew. Eintr.
      LOOP_LINES          LIKE SY-LOOPC,          " aktuelle Seite
      USED_LOOP_LINES     LIKE SY-STEPL,         " Anz. gef?lter Zeilen
      PAGE                TYPE I,                " gesamte Seitenanzahl
      H_PAGE              TYPE I,                " Hilfsfeld Seitenanz.
      STEPL               LIKE SY-STEPL,
      AN                  TYPE C VALUE '1',
      AUS                 TYPE C VALUE '0',
      VIEWCLUSTER_COMING_FROM_IMG,       "'Y': coming from IMG, 'N': not
*
      LOOP_POSITION_INFO(36)      TYPE C,
      LOOP_POSITION_INFO_MASK(36) TYPE C,
      LOOP_POSITION_INFO_LEN      TYPE I VALUE '36'.

*--------------------------------------------------------------------
*> EDIT CONTAINER DATA DEFINE.
*--------------------------------------------------------------------
DATA: EDITCONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: BEGIN OF SRC OCCURS 500,
              LINE(72),
      END   OF SRC.

DATA EDITOR TYPE REF TO CL_GUI_TEXTEDIT.

DATA: BEHAVIOUR_LEFT TYPE REF TO CL_DRAGDROP,
      BEHAVIOUR_RIGHT TYPE REF TO CL_DRAGDROP.





DATA: W_FIRST_FLAG_0020 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0210 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0220 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0080 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0100 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0200 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0300 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0500 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0800 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_1100 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_1000 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_1200 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_1400 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_1500 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_1600 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_1700 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_2000 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_2100 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_2300 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_2400 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_2500 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_9100 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_9200 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_0600 TYPE     C     VALUE   'Y',
      W_FIRST_FLAG_9500 TYPE     C     VALUE   'Y',
      W_ZFPVEN1_NM(20)  TYPE     C,
      W_ZFPVEN2_NM(20)  TYPE     C,
      W_ZFPVEN3_NM(20)  TYPE     C,
      W_ZFPVEN4_NM(20)  TYPE     C,
      W_ZFPVEN5_NM(20)  TYPE     C,
      W_ZFPVEN6_NM(20)  TYPE     C,
      W_ZFPVEN7_NM(20)  TYPE     C,
      W_INSCOMP_NM(25)  TYPE     C,
      W_MEINS           LIKE     ZTIMIMG21-MEINS,
      W_WAERS           LIKE     ZTIMIMG06-WAERS,
      W_ZFMATGB         LIKE     ZTIMIMG04-ZFMATGB,
      W_ZFCDTY          LIKE     ZTIMIMG08-ZFCDTY,
      W_ZFCDTY_NM(50)   TYPE     C,
      W_ZFCD            LIKE     ZTIMIMG08-ZFCD,
      W_ZFCD5           LIKE     ZTIMIMG08-ZFCD5,
      W_ZFCOTM          LIKE     ZTIMIMG02-ZFCOTM,
      W_ZFBNAR          LIKE     ZTIMIMG03-ZFBNAR.

CONSTANTS:
* Total Menu Maintenance
   TEXT-000-1(80) TYPE C VALUE 'Use Yes/No(X/Space)',
   TEXT-000-2(80) TYPE C VALUE 'Tree Level',
   TEXT-000-3(80) TYPE C VALUE 'Transaction Code(ZIMG + ??)',
   TEXT-000-4(80) TYPE C VALUE 'Lowest Level(LAST/Space)',
   TEXT-000-5(80) TYPE C VALUE 'None',
*  Import Type
   TEXT-001-1(80) TYPE C VALUE 'Monetary Transaction Yes/No',
   TEXT-001-2(80) TYPE C VALUE 'None',
   TEXT-001-3(80) TYPE C VALUE 'None',
   TEXT-001-4(80) TYPE C VALUE 'None',
   TEXT-001-5(80) TYPE C VALUE 'None',
* Shipping Port/Arrival Port
   TEXT-002-1(80) TYPE C VALUE 'None',
   TEXT-002-2(80) TYPE C VALUE 'Country Code(Search Condition)',
   TEXT-002-3(80) TYPE C VALUE 'None',
   TEXT-002-4(80) TYPE C VALUE 'Description(Loading/Arrival Port Text)',
   TEXT-002-5(80) TYPE C VALUE 'None',
*  L/C Expense Code.
   TEXT-003-1(80) TYPE C VALUE 'Delivery Cost Yes/No',
   TEXT-003-2(80) TYPE C VALUE 'Short Text',
   TEXT-003-3(80) TYPE C VALUE 'Group',
   TEXT-003-4(80) TYPE C VALUE 'Group Description',
   TEXT-003-5(80) TYPE C VALUE 'Tax Code',
*  B/L Expense Code
   TEXT-004-1(80) TYPE C VALUE 'Planed Cost Yes/No',
   TEXT-004-2(80) TYPE C VALUE 'Screen Diplay Sequence(Number)',
   TEXT-004-3(80) TYPE C VALUE 'Match EDI Code(Other Chg.)',
   TEXT-004-4(80) TYPE C VALUE 'Ocean / Air',
   TEXT-004-5(80) TYPE C VALUE 'Tax Code',
*  B/L Bonded Expense Code
   TEXT-005-1(80) TYPE C VALUE 'Planed Cost Yes/No',
   TEXT-005-2(80) TYPE C VALUE 'Screen Diplay Sequence(Number)',
   TEXT-005-3(80) TYPE C VALUE 'None',
   TEXT-005-4(80) TYPE C VALUE 'Ocean / Air',
   TEXT-005-5(80) TYPE C VALUE 'Tax Code',
* C/C Expense Code
   TEXT-006-1(80) TYPE C VALUE 'Planed Cost Yes/No',
   TEXT-006-2(80) TYPE C VALUE 'None',
   TEXT-006-3(80) TYPE C VALUE 'None',
   TEXT-006-4(80) TYPE C VALUE 'None',
   TEXT-006-5(80) TYPE C VALUE 'Tax Code',
* Cargo Expense Code
   TEXT-007-1(80) TYPE C VALUE 'Planed Cost Yes/No',
   TEXT-007-2(80) TYPE C VALUE 'None',
   TEXT-007-3(80) TYPE C VALUE 'None',
   TEXT-007-4(80) TYPE C VALUE 'None',
   TEXT-007-5(80) TYPE C VALUE 'Tax Code',
* Duty Expense Code
   TEXT-008-1(80) TYPE C VALUE 'Planed Cost Yes/No',
   TEXT-008-2(80) TYPE C VALUE 'None',
   TEXT-008-3(80) TYPE C VALUE 'None',
   TEXT-008-4(80) TYPE C VALUE 'None',
   TEXT-008-5(80) TYPE C VALUE 'Tax Code',
* Transfer Expense Code
   TEXT-009-1(80) TYPE C VALUE 'Planed Cost Yes/No',
   TEXT-009-2(80) TYPE C VALUE 'None',
   TEXT-009-3(80) TYPE C VALUE 'None',
   TEXT-009-4(80) TYPE C VALUE 'None',
   TEXT-009-5(80) TYPE C VALUE 'Tax Code',
* Insurance Code & Identification
   TEXT-010-1(80) TYPE C VALUE 'None',
   TEXT-010-2(80) TYPE C VALUE 'None',
   TEXT-010-3(80) TYPE C VALUE 'None',
   TEXT-010-4(80) TYPE C VALUE 'EDI Indicatior(Option)',
   TEXT-010-5(80) TYPE C VALUE 'Vendor Code(Option)',
* Bonded Transport Location
   TEXT-011-1(80) TYPE C VALUE 'None',
   TEXT-011-2(80) TYPE C VALUE 'Bonded Area Code(Required Column)',
   TEXT-011-3(80) TYPE C VALUE 'None',
   TEXT-011-4(80) TYPE C VALUE 'Storage Location(Required Column)',
   TEXT-011-5(80) TYPE C VALUE 'None',
* Shipping Document.
   TEXT-012-1(80) TYPE C VALUE 'None',
   TEXT-012-2(80) TYPE C VALUE 'None',
   TEXT-012-3(80) TYPE C VALUE 'None',
   TEXT-012-4(80) TYPE C VALUE 'None',
   TEXT-012-5(80) TYPE C VALUE 'None',
* EDI Document Management Code.
   TEXT-013-1(80) TYPE C VALUE 'None',
   TEXT-013-2(80) TYPE C VALUE 'I:Import, E:Export',
   TEXT-013-3(80) TYPE C VALUE 'None',
   TEXT-013-4(80) TYPE C VALUE 'electronic Document',
   TEXT-013-5(80) TYPE C VALUE 'None',
* Ocean Transportation Shipment Area.
   TEXT-014-1(80) TYPE C VALUE 'None',
   TEXT-014-2(80) TYPE C VALUE 'None',
   TEXT-014-3(80) TYPE C VALUE 'None',
   TEXT-014-4(80) TYPE C VALUE 'None',
   TEXT-014-5(80) TYPE C VALUE 'None',
* Air Transportation Shipment Area..
   TEXT-015-1(80) TYPE C VALUE 'None',
   TEXT-015-2(80) TYPE C VALUE 'None',
   TEXT-015-3(80) TYPE C VALUE 'None',
   TEXT-015-4(80) TYPE C VALUE 'None',
   TEXT-015-5(80) TYPE C VALUE 'None',
* Ocean Freight Basic Rate
   TEXT-016-1(80) TYPE C VALUE 'None',
   TEXT-016-2(80) TYPE C VALUE 'None',
   TEXT-016-3(80) TYPE C VALUE 'None',
   TEXT-016-4(80) TYPE C VALUE 'None',
   TEXT-016-5(80) TYPE C VALUE 'None',
* Air Freight Basic Rate
   TEXT-017-1(80) TYPE C VALUE 'None',
   TEXT-017-2(80) TYPE C VALUE 'None',
   TEXT-017-3(80) TYPE C VALUE 'None',
   TEXT-017-4(80) TYPE C VALUE 'None',
   TEXT-017-5(80) TYPE C VALUE 'None',
* MASTER L/C Added Conditions
   TEXT-997-1(80) TYPE C VALUE 'None',
   TEXT-997-2(80) TYPE C VALUE 'None',
   TEXT-997-3(80) TYPE C VALUE 'None',
   TEXT-997-4(80) TYPE C VALUE 'None',
* Clearance Exchange Rate
   TEXT-998-1(80) TYPE C VALUE 'None',
   TEXT-998-2(80) TYPE C VALUE 'None',
   TEXT-998-3(80) TYPE C VALUE 'Sequence(Initial character->Zero)',
   TEXT-998-4(80) TYPE C VALUE 'None',
   TEXT-998-5(80) TYPE C VALUE 'None',
* MASTER L/C Additional Condition
   TEXT-999-1(80) TYPE C VALUE 'None',
   TEXT-999-2(80) TYPE C VALUE 'None',
   TEXT-999-3(80) TYPE C VALUE 'None',
   TEXT-999-4(80) TYPE C VALUE 'None'.
* 국가별 Port 유지 보수 Table control의 현재 위치.
DATA : VIM_POSITION_INFO(40)       TYPE C,
       VIM_POSITION_INFO_MASK(40)  TYPE C,
       POS_TEXT1(20)               TYPE C  VALUE 'Entry',
       POS_TEXT2(20)               TYPE C  VALUE 'of'.
