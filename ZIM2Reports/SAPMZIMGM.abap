*&---------------------------------------------------------------------*
*& MODULE POOL       SAPMZIMGM                                         *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 IMG 통합관리 MODULE POOL PROGRAM           *
*&      작성자 : 강석봉 INFOLINK LTD.                                  *
*&      작성일 : 2002.02.10                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*&                                                                     *
*&---------------------------------------------------------------------*


INCLUDE ZRZIMGMTOP.                  " GLOBAL DATA

*---> class 정의.
CLASS c_event_handler DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_item_double_click
        FOR EVENT item_double_click
        OF cl_gui_column_tree
        IMPORTING node_key item_name,
      handle_node_double_click
        for event node_double_click
        of cl_gui_column_tree
        importing node_key.
    class-methods:
      ignore_cntl_error
        for event flush_error of cl_gui_cfw.
ENDCLASS.

CLASS c_rmouse_click DEFINITION.
  PUBLIC SECTION.
    METHODS handle_node_rmouse_click FOR EVENT
    node_context_menu_request OF cl_gui_column_tree
      IMPORTING node_key
                menu.
    METHODS handle_item_rmouse_click FOR EVENT
         item_context_menu_request OF cl_gui_column_tree
           IMPORTING node_key
                     item_name
                     menu.
    METHODS handle_node_select FOR EVENT node_context_menu_select
            OF cl_gui_column_tree
               IMPORTING node_key fcode.
    METHODS handle_item_select FOR EVENT item_context_menu_select
            OF cl_gui_column_tree
               IMPORTING node_key item_name fcode.
ENDCLASS.

CLASS lcl_drag_object DEFINITION.
  PUBLIC SECTION.
    DATA text TYPE demo_item-text.
ENDCLASS.

CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
       flavor_select FOR EVENT on_get_flavor OF cl_gui_textedit
                      IMPORTING index line pos flavors dragdrop_object,
       left_drag FOR EVENT on_drag OF cl_gui_column_tree
                      IMPORTING node_key drag_drop_object,
       right_drop FOR EVENT ON_DROP OF cl_gui_textedit
                      IMPORTING index line pos dragdrop_object,
       drop_complete FOR EVENT on_drop_complete OF cl_gui_column_tree
                      IMPORTING node_key drag_drop_object.

ENDCLASS.

DATA: node_handler type ref to c_event_handler,
      item_handler type ref to c_event_handler.
DATA: H_SPLITTER TYPE CNTL_HANDLE,
      H_TREE TYPE CNTL_HANDLE.
DATA: PARENT LIKE NODETAB-PARENT.
DATA: ROOTID LIKE NODETAB-ID.
DATA: EDITCONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      piccontainer type ref to cl_gui_custom_container.
data dockcontainer type ref to cl_gui_docking_container.
DATA TREEv TYPE REF TO CL_GUI_COLUMN_TREE.
DATA EVENTS TYPE CNTL_SIMPLE_EVENTS.
Data evt type cntl_simple_event.
data pic type ref to cl_gui_picture.
FIELD-SYMBOLS: <FS> LIKE NODETAB, <GS> LIKE NODETAB.


DATA: activexactive.
DATA PIC_DATA LIKE W3MIME OCCURS 0.
DATA PIC_SIZE TYPE I.
  data url(255) type c.                " URL-field in screen 200
  data url2(255) type c.               " URL-field in screen 200
DATA: node_rclick_handler TYPE REF TO c_rmouse_click,
      item_rclick_handler TYPE REF TO c_rmouse_click,
      node_select_handler TYPE REF TO c_rmouse_click,
      item_select_handler TYPE REF TO c_rmouse_click.
DATA: p_menu TYPE REF TO cl_ctmenu.
DATA: gnode TYPE snodetext.
DATA: grelship TYPE stree_relation_type.
DATA: behaviour_left type ref to cl_dragdrop,
      behaviour_right type ref to cl_dragdrop.
DATA: handle_tree type i.
DATA: dragdrop TYPE REF TO lcl_dragdrop_receiver.



START-OF-SELECTION.
  LAYOUT_ID-USER = SY-UNAME.
  REPID = SY-REPID.
  DYNNR = '0200'.

  CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
           RETURN  = ACTIVEXACTIVE.

  FIRST_NODE = 1.
  PERFORM GET_TREE_DATA USING 'X'.
  CHECK TREE-ID > 0.

  DESCRIBE TABLE TREE LINES SY-TABIX.
  IF SY-TABIX = 0.
*     PERFORM AUTHORITY_CHECK USING ACT_EDIT.
     MESSAGE S900(ZIM1).
     EXIT.
  ELSE.
*     PERFORM AUTHORITY_CHECK USING ACT_SHOW.
     SET PF-STATUS 'DMSHOW'.
     SET TITLEBAR  'DMS'.
  ENDIF.

  IF NOT ACTIVEXACTIVE IS INITIAL.
*     CREATE OBJECT PICCONTAINER
*            EXPORTING CONTAINER_NAME = 'PIC'
*               REPID  = 'SAPMZIMGM'
*               DYNNR  = '0200'.
*
*     CREATE OBJECT PIC EXPORTING PARENT = PICCONTAINER.
*
*     CALL METHOD PIC->SET_ALIGNMENT EXPORTING
*          ALIGNMENT = 15.
*
*     CALL METHOD PIC->SET_DISPLAY_MODE
*          EXPORTING DISPLAY_MODE = CL_GUI_PICTURE=>DISPLAY_MODE_FIT.
*
*     PERFORM LOAD_PIC_FROM_DB
*             TABLES PIC_DATA
**             USING 'ENJOYSAP_LOGO'
*              USING 'KKENJOY'
*              CHANGING PIC_SIZE.
*
** REQUEST AN URL FROM THE DATA PROVIDER BY EXPORTING THE PIC_DATA.
*      CLEAR URL.
*      CALL FUNCTION 'DP_CREATE_URL'
*           EXPORTING
*              TYPE     = 'IMAGE'
*              SUBTYPE  = CNDP_SAP_TAB_UNKNOWN
*              SIZE     = PIC_SIZE
*              LIFETIME = CNDP_LIFETIME_TRANSACTION
*           TABLES
*              DATA     = PIC_DATA
*           CHANGING
*              URL      = URL
*           EXCEPTIONS
*              OTHERS   = 1.
** LOAD THE PICTURE BY USING THE URL GENERATED BY THE DATA PROVIDER.
*      IF SY-SUBRC = 0.
*         CALL METHOD PIC->LOAD_PICTURE_FROM_URL
*              EXPORTING URL = URL.
*      ENDIF.

      CREATE OBJECT NODE_HANDLER.
      CREATE OBJECT ITEM_HANDLER.
      CREATE OBJECT NODE_RCLICK_HANDLER.
      CREATE OBJECT ITEM_RCLICK_HANDLER.
      CREATE OBJECT P_MENU.
      CREATE OBJECT NODE_SELECT_HANDLER.
      CREATE OBJECT ITEM_SELECT_HANDLER.

**> TEXT EDIT BOX.
*      CREATE OBJECT EDITCONTAINER
*             EXPORTING
*                CONTAINER_NAME = 'SRC'
*                REPID          = REPID
*                DYNNR          = DYNNR.

      CREATE OBJECT DOCKCONTAINER
        EXPORTING
            REPID  = REPID
            DYNNR  = DYNNR
*         NO_FLUSH     = 'X'
*         DOCK_AT      = DOCK_AT_LEFT
            EXTENSION        = 290
*         HEIGHT       = 42
*       EXCEPTIONS
*         CREATE_ERROR = 1
*         OTHERS       = 2
          .
      IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*      CREATE OBJECT DRAGDROP.
*      CREATE OBJECT BEHAVIOUR_LEFT.
*      CALL METHOD BEHAVIOUR_LEFT->ADD
*           EXPORTING
*              FLAVOR = 'TREE_MOVE_TO_EDIT'
*              DRAGSRC = 'X'
*              DROPTARGET = ' '
*              EFFECT = CL_DRAGDROP=>COPY.
*
*      CALL METHOD BEHAVIOUR_LEFT->ADD
*           EXPORTING
*              FLAVOR = 'TREE_COPY_TO_EDIT'
*              DRAGSRC = 'X'
*              DROPTARGET = ' '
*              EFFECT = CL_DRAGDROP=>COPY.
*
*      CALL METHOD BEHAVIOUR_LEFT->GET_HANDLE
*           IMPORTING HANDLE = HANDLE_TREE.
*      CREATE OBJECT BEHAVIOUR_RIGHT.
*      CALL METHOD BEHAVIOUR_RIGHT->ADD
*           EXPORTING
*              FLAVOR = 'TREE_MOVE_TO_EDIT'
*              DRAGSRC = ' '
*              DROPTARGET = 'X'
*              EFFECT = CL_DRAGDROP=>COPY.
*
*      CALL METHOD BEHAVIOUR_RIGHT->ADD
*           EXPORTING
*              FLAVOR = 'TREE_COPY_TO_EDIT'
*              DRAGSRC = ' '
*              DROPTARGET = 'X'
*              EFFECT = CL_DRAGDROP=>COPY.

      REFRESH CTRL_NODE_TAB.

      LOOP AT NODETAB.
         CLEAR CTRL_NODE.
         CLEAR CTRL_ITEM.
         CTRL_NODE-NODE_KEY = NODETAB-ID.
         PARENT = NODETAB-PARENT.
         IF NODETAB-TYPE IS INITIAL OR
            NODETAB-TEXT1(8) = 'KEN_HELP'.
            CTRL_NODE-ISFOLDER = 'X'.
         ELSE.
            CTRL_NODE-DRAGDROPID = HANDLE_TREE.
         ENDIF.

         IF PARENT IS INITIAL.                 "WURZEL
            CTRL_NODE-ISFOLDER = 'X'.
            CLEAR CTRL_NODE-DRAGDROPID.
            APPEND CTRL_NODE TO CTRL_NODE_TAB.
            ROOTID = CTRL_NODE-NODE_KEY.
         ELSE.
            READ TABLE NODETAB ASSIGNING <FS> WITH KEY ID = PARENT.
            IF <FS>-CHILD = NODETAB-ID.
               CTRL_NODE-RELATSHIP = TREEV_RELAT_FIRST_CHILD.
               CTRL_NODE-RELATKEY = <FS>-ID.
               APPEND CTRL_NODE TO CTRL_NODE_TAB.
            ELSE.
               READ TABLE NODETAB ASSIGNING <GS>
                                  WITH KEY ID = <FS>-CHILD.
               WHILE <GS>-NEXT <> NODETAB-ID.
                  READ TABLE NODETAB ASSIGNING <GS>
                                     WITH KEY ID = <GS>-NEXT.
               ENDWHILE.
               CTRL_NODE-RELATSHIP = TREEV_RELAT_NEXT_SIBLING.
               CTRL_NODE-RELATKEY = <GS>-ID.
               APPEND CTRL_NODE TO CTRL_NODE_TAB.
            ENDIF.

            IF NODETAB-NEXT IS INITIAL.
               CTRL_NODE-RELATSHIP = TREEV_RELAT_LAST_CHILD.
               CTRL_NODE-RELATKEY = <FS>-ID.
               APPEND CTRL_NODE TO CTRL_NODE_TAB.
            ENDIF.
         ENDIF.

         IF NODETAB-NEXT IS INITIAL.
         ELSE.
            CTRL_NODE-RELATSHIP = TREEV_RELAT_PREV_SIBLING.
            CTRL_NODE-RELATKEY = NODETAB-NEXT.
            APPEND CTRL_NODE TO CTRL_NODE_TAB.
         ENDIF.

         CTRL_ITEM-ITEM_NAME = 'REPTITLE'.
         CTRL_ITEM-NODE_KEY = CTRL_NODE-NODE_KEY. "EINGEF?Tt
         CTRL_ITEM-CLASS = TREEV_ITEM_CLASS_TEXT.
         CTRL_ITEM-TEXT = NODETAB-TEXT.
         APPEND CTRL_ITEM TO CTRL_ITEM_TAB.

         CTRL_ITEM-ITEM_NAME = 'REPNAME'.
         CTRL_ITEM-NODE_KEY = CTRL_NODE-NODE_KEY. "EINGEF?Tt
         CTRL_ITEM-CLASS = TREEV_ITEM_CLASS_TEXT.
         CTRL_ITEM-TEXT = NODETAB-TEXT1.
         APPEND CTRL_ITEM TO CTRL_ITEM_TAB.
      ENDLOOP.

      CTRL_NODE_TAB2 = CTRL_NODE_TAB.
      REFRESH CTRL_NODE_TAB.
      PERFORM PRINT_NODE_TAB USING ROOTID LEVEL.
      PERFORM TRANSFORM_NODE_TAB.


      CLEAR HIERARCHY_HEADER.
      HIERARCHY_HEADER-HEADING = TEXT-001.
      HIERARCHY_HEADER-WIDTH = 25.
*WIDTH
      CLEAR HEADER.
      HEADER-NAME = 'HDRREPNAME'.
      HEADER-END_COL = 'REPNAME'.
      HEADER-HEADING = TEXT-002.
      HEADER-WIDTH = 10.
*WIDTH
      HEADER-ALIGNMENT = TREEV_ALIGN_LEFT.
      HEADER-TYPE = TREEV_HEADER_VARIABLE.
      APPEND HEADER TO HEADER_TAB.

      CLEAR COLUMN.
      COLUMN-SELECTABLE = 'X'.
      COLUMN-NAME = 'REPTITLE'.
      COLUMN-ALIGNMENT = TREEV_ALIGN_LEFT.
      COLUMN-WIDTH = 25.
      COLUMN-WIDTH_CHAR = 'X'.
      APPEND COLUMN TO COLUMN_TAB.

      COLUMN-NAME = 'REPNAME'.
      COLUMN-ALIGNMENT = TREEV_ALIGN_LEFT.
      APPEND COLUMN TO COLUMN_TAB.

      CREATE OBJECT TREEV
          EXPORTING
             PARENT = DOCKCONTAINER
             NODE_SELECTION_MODE =
                       CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
             ITEM_SELECTION = 'X'
             HIERARCHY_COLUMN_NAME = 'REPTITLE'
             HIERARCHY_HEADER = HIERARCHY_HEADER
          EXCEPTIONS
             CNTL_SYSTEM_ERROR = 1
             CREATE_ERROR = 2
             FAILED = 3
             ILLEGAL_NODE_SELECTION_MODE = 4
             ILLEGAL_COLUMN_NAME = 5.

*******************************
      DATA ALIGN TYPE I.
      ALIGN = ALIGN_AT_TOP + ALIGN_AT_BOTTOM + ALIGN_AT_RIGHT +
              ALIGN_AT_LEFT.
      CALL METHOD TREEV->SET_ALIGNMENT EXPORTING
      ALIGNMENT = ALIGN.
********************************
*CALL FUNCTION 'TREEV_CREATE_COLUMN_TREE'
*     EXPORTING
*          OWNER_REPID                    = 'SAPMSDM1'
*          DYNNR                          = '0200'
**         CONTAINER                      =
*          LEFT                           = 0
*          TOP                            = 0
*          WIDTH                          = 300
*          HEIGHT                         = 400
**         ALIGNMENT                      =
**         METRIC                         = CNTL_METRIC_DYNPRO
**         NO_FLUSH                       =
**         LINK_REPID                     =
**         SHELLSTYLE                     =
*         PARENTID                       = H_SPLITTER-SHELLID
**         REG_EVENT_EXPAND_NO_CHILDREN   =
*         REGISTER_EVENT_ITEM_DBL_CLICK  = 'X'
*         REGISTER_EVENT_NODE_DBL_CLICK  = 'X'
**         REGISTER_EVENT_HEADER_CLICK    =
**         REGISTER_EVENT_NODE_CONTEXT_M  =
**         REGISTER_EVENT_ITEM_CONTEXT_M  =
**         REGISTER_EVENT_LINK_CLICK      =
**         REGISTER_EVENT_BUTTON_CLICK    =
**         REGISTER_EVENT_CHECKBOX_CHANGE =
**         REGISTER_EVENT_SEL_CHANGE      =
*          ITEM_SELECTION                 = 'X'
*          NODE_SELECTION_MODE            = TREEV_NODE_SEL_MODE_SINGLE
**         GROUP_HEADERS                  =
**         HIDE_SELECTION                 =
*          HIERARCHY_HEADER               = HIERARCHY_HEADER
*         ITEM_TABLE_STRUCTURE_NAME      = 'DEMO_ITEM'
*     TABLES
*          COLUMNS                        = COLUMN_TAB
*         HEADERS                        = HEADER_TAB
*         NODE_TABLE                     = CTRL_NODE_TAB
*         ITEM_TABLE                     = CTRL_ITEM_TAB
*     CHANGING
*          HANDLE                         = H_TREE.

      CALL METHOD TREEV->ADD_COLUMN
           EXPORTING
              NAME = 'REPNAME'
              WIDTH = 11
              ALIGNMENT = CL_GUI_COLUMN_TREE=>ALIGN_LEFT
              HEADER_TEXT = TEXT-002.

      CALL METHOD TREEV->ADD_NODES_AND_ITEMS
           EXPORTING
              NODE_TABLE = CTRL_NODE_TAB
              ITEM_TABLE = CTRL_ITEM_TAB
              ITEM_TABLE_STRUCTURE_NAME = 'demo_item'
           EXCEPTIONS
              FAILED = 1
              CNTL_SYSTEM_ERROR = 3
              ERROR_IN_TABLES = 4
              DP_ERROR = 5
              TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
*     IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*     ENDIF.
*     CALL METHOD CL_GUI_CFW=>FLUSH.
*     IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*     ENDIF.
      EVT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
      EVT-APPL_EVENT = 'X'.
      APPEND EVT TO EVENTS.
      EVT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
      EVT-APPL_EVENT = 'X'.
      APPEND EVT TO EVENTS.
      EVT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_CONTEXT_MENU_REQ.
      EVT-APPL_EVENT = 'X'.
      APPEND EVT TO EVENTS.
      EVT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_CONTEXT_MENU_REQ.
      EVT-APPL_EVENT = 'X'.
      APPEND EVT TO EVENTS.

      CALL METHOD TREEV->SET_REGISTERED_EVENTS
           EXPORTING
              EVENTS = EVENTS
           EXCEPTIONS
              CNTL_ERROR                = 1
              CNTL_SYSTEM_ERROR         = 2
              ILLEGAL_EVENT_COMBINATION = 3.

      SET HANDLER NODE_HANDLER->HANDLE_NODE_DOUBLE_CLICK FOR TREEV.
      SET HANDLER C_EVENT_HANDLER=>IGNORE_CNTL_ERROR.
      SET HANDLER ITEM_HANDLER->HANDLE_ITEM_DOUBLE_CLICK FOR TREEV.
      SET HANDLER NODE_RCLICK_HANDLER->HANDLE_NODE_RMOUSE_CLICK
                                                            FOR TREEV.
      SET HANDLER ITEM_RCLICK_HANDLER->HANDLE_ITEM_RMOUSE_CLICK
                                                            FOR TREEV.
      SET HANDLER NODE_SELECT_HANDLER->HANDLE_NODE_SELECT FOR TREEV.
      SET HANDLER ITEM_SELECT_HANDLER->HANDLE_ITEM_SELECT FOR TREEV.
*      SET HANDLER DRAGDROP->LEFT_DRAG FOR TREEV.
*      SET HANDLER DRAGDROP->DROP_COMPLETE FOR TREEV.

      CALL METHOD TREEV->EXPAND_ROOT_NODES
           EXPORTING
              LEVEL_COUNT = 0
              EXPAND_SUBTREE = 'X'.
*      CALL FUNCTION 'TREEV_EXPAND_ROOT_NODES'
*           EXPORTING
*                HANDLE              = H_TREE
*               LEVEL_COUNT         = 0
*                EXPAND_ALL_CHILDREN = 'X'
**               NO_FLUSH            =
**          EXCEPTIONS
**               FAILED              = 1
**               ILLEGAL_LEVEL_COUNT = 2
**               CNTL_SYSTEM_ERROR   = 3
**               OTHERS              = 4
*          .
*      IF SY-SUBRC <> 0.
**       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.

*      CALL FUNCTION 'TREEV_EVENT_CB_ITEM_DBL_CLK'
*           EXPORTING
*                CALLBACK_FORM               = 'ITEM_DBL_CLK'
**          IMPORTING
**               CALLBACK_ID                 =
*           CHANGING
*                HANDLE                      = H_TREE
**          EXCEPTIONS
**               CB_NOT_FOUND                = 1
**               FAILED                      = 2
**               INSTANCE_NOT_FOUND          = 3
**               NOT_ALLOWED_FOR_SIMPLE_TREE = 4
**               INV_CALLBACK_DEFINITION     = 5
**               NO_ITEM_SELECTION           = 6
**               OTHERS                      = 7
*          .
*      IF SY-SUBRC <> 0.
**       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.

**          EXCEPTIONS
**               CREATE_ERROR                   = 1
**               TREE_CONTROL_NOT_EXISTING      = 2
**               CNTL_SYSTEM_ERROR              = 3
**               FAILED                         = 4
**               ILLEGAL_NODE_SELECTION_MODE    = 5
**               ILLEGAL_COLUMN_NAME            = 6
**               ILLEGAL_COLUMN_ALIGNMENT       = 7
**               ILLEGAL_HEADER_ALIGNMENT       = 8
**               ILLEGAL_HEADER_TYPE            = 9
**               ILLEGAL_HEADER_NAME            = 10
**               DUPLICATED_COLUMN_NAME         = 11
**               DUPLICATED_HEADER_NAME         = 12
**               ILLEGAL_END_COL                = 13
**               END_COL_NOT_FOUND              = 14
**               MISSING_ITEM_STRUCTURE_NAME    = 15
**               EMPTY_NODE_TABLE               = 16
**               EMPTY_ITEM_TABLE               = 17
**               ERROR_IN_TABLES                = 18
**               DP_ERROR                       = 19
**               ILLEGAL_OWNER_REPID            = 20
**               TOO_MANY_COLUMNS               = 21
**               TABLE_STRUCTURE_NAME_NOT_FOUND = 22
**               OTHERS                         = 23
*          .
*      IF SY-SUBRC <> 0.
**       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*      CALL FUNCTION 'TREEV_EVENT_CB_NODE_DBL_CLK'
*           EXPORTING
*                CALLBACK_FORM           = 'NODE_DBL_CLK'
**          IMPORTING
**               CALLBACK_ID             =
*           CHANGING
*                HANDLE                  = H_TREE
**          EXCEPTIONS
**               CB_NOT_FOUND            = 1
**               FAILED                  = 2
**               INV_CALLBACK_DEFINITION = 3
**               OTHERS                  = 4
*          .
*      IF SY-SUBRC <> 0.
**        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.

       W_STATUS = 'D'.

       CALL SCREEN 200.
   ENDIF.

   SET PF-STATUS 'DMSHOW'.
   SET TITLEBAR 'DMS'.

   CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
        EXPORTING
          CALLBACK_PROGRAM      = REPID
          CALLBACK_USER_COMMAND = 'USER_COMMAND'
          CALLBACK_TEXT_DISPLAY = 'TEXT_DISPLAY'
          CHECK_DUPLICATE_NAME  = ' '
          LOWER_CASE_SENSITIVE  = 'X'
          NODE_LENGTH           = 40
          TEXT_LENGTH           = TLENGTH
          TEXT_LENGTH1          = 40
          MODIFICATION_LOG      = 'X'
          RETURN_MARKED_SUBTREE = 'X'.


INCLUDE ZRZIMGMF01.                  " FORM-ROUTINES                 *
INCLUDE ZRZIMGMO01.
INCLUDE ZRZIMGMI01.
