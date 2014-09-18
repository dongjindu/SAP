*----------------------------------------------------------------------*
***INCLUDE MZAPM10_INFOF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_AND_INIT_TREE.
** Column1 of Tree
* setup the hierarchy header
  HIERARCHY_HEADER-HEADING =  'Hierarchy Header'.           "#EC NOTEXT
  HIERARCHY_HEADER-WIDTH = 45.  " width: 30 characters
* create a simple tree model instance
  CREATE OBJECT CRV_TREE_MODEL
    EXPORTING
      NODE_SELECTION_MODE = CL_COLUMN_TREE_MODEL=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION = 'X'
      HIERARCHY_COLUMN_NAME = 'Column1' "EC NOTEXT
      HIERARCHY_HEADER = HIERARCHY_HEADER
    EXCEPTIONS
      ILLEGAL_NODE_SELECTION_MODE = 1
      ILLEGAL_COLUMN_NAME = 2.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

** Column2 of Tree
  CALL METHOD CRV_TREE_MODEL->ADD_COLUMN
    EXPORTING
      NAME                = 'Column2'                       "#EC NOTEXT
      WIDTH               = 50
      HEADER_TEXT         = 'Description'                   "#EC NOTEXT
    EXCEPTIONS
      COLUMN_EXISTS       = 1
      ILLEGAL_COLUMN_NAME = 2
      TOO_MANY_COLUMNS    = 3
      ILLEGAL_ALIGNMENT   = 4.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.
**

* create the view (control) of the tree model
  CALL METHOD CRV_TREE_MODEL->CREATE_TREE_CONTROL
    EXPORTING
      PARENT                       = CRV_DOCKING_CONTAINER
    EXCEPTIONS
      LIFETIME_ERROR               = 1
      CNTL_SYSTEM_ERROR            = 2
      CREATE_ERROR                 = 3
      FAILED                       = 4
      TREE_CONTROL_ALREADY_CREATED = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

* define the events which will be passed to the backend

* node double click
  EVENT-EVENTID = CL_COLUMN_TREE_MODEL=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.              " process PAI if event occurs
  APPEND EVENT TO EVENTS.
* item double click
  EVENT-EVENTID = CL_COLUMN_TREE_MODEL=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.
* link click
  EVENT-EVENTID = CL_COLUMN_TREE_MODEL=>EVENTID_LINK_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.
* button click
  EVENT-EVENTID = CL_COLUMN_TREE_MODEL=>EVENTID_BUTTON_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.
* checkbox change
  EVENT-EVENTID = CL_COLUMN_TREE_MODEL=>EVENTID_CHECKBOX_CHANGE.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

* header click
  EVENT-EVENTID = CL_COLUMN_TREE_MODEL=>EVENTID_HEADER_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

* Register Events
  CALL METHOD CRV_TREE_MODEL->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = EVENTS
    EXCEPTIONS
      ILLEGAL_EVENT_COMBINATION = 1
      UNKNOWN_EVENT             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER CRV_H_TREE->HANDLE_NODE_DOUBLE_CLICK
      FOR     CRV_TREE_MODEL.
  SET HANDLER CRV_H_TREE->HANDLE_ITEM_DOUBLE_CLICK
      FOR     CRV_TREE_MODEL.
  SET HANDLER CRV_H_TREE->HANDLE_LINK_CLICK
      FOR     CRV_TREE_MODEL.
  SET HANDLER CRV_H_TREE->HANDLE_BUTTON_CLICK
      FOR     CRV_TREE_MODEL.
  SET HANDLER CRV_H_TREE->HANDLE_CHECKBOX_CHANGE
      FOR     CRV_TREE_MODEL.
  SET HANDLER CRV_H_TREE->HANDLE_HEADER_CLICK
      FOR     CRV_TREE_MODEL.

* add nodes to the tree model
  PERFORM ADD_NODES.

* expand the root node
  CALL METHOD CRV_TREE_MODEL->EXPAND_NODE
    EXPORTING
      NODE_KEY       = 'Root'                               "#EC NOTEXT
    EXCEPTIONS
      NODE_NOT_FOUND = 1.
  IF SY-SUBRC <> 0.
    MESSAGE A001(ZMMM) WITH 'Error!'.
  ENDIF.

* expand the level 2 node
  CALL METHOD CRV_TREE_MODEL->EXPAND_NODE
    EXPORTING
      NODE_KEY       = 'Child1'
    EXCEPTIONS
      NODE_NOT_FOUND = 1.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

ENDFORM.                    " CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*&      Form  ADD_NODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_NODES.
**** Node with key 'Root'
* Column1
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.    "#EC NOTEXT " Item of Column 'Column1'
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
*  item-text = 'Root Col. 1'.                                "#EC NOTEXT
  ITEM-TEXT = 'PMIS'.                                       "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = 'PM Information Management'.                  "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Root'                      "#EC NOTEXT
      ISFOLDER                = 'X'
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A001(ZM) WITH 'Error'.
  ENDIF.

*************
* Column1
  CLEAR ITEM.
  CLEAR ITEM_TABLE.

  ITEM-ITEM_NAME = 'Column1'.    "#EC NOTEXT " Item of Column 'Column1'
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
*  item-text = 'Root Col. 1'.                                "#EC NOTEXT
  ITEM-TEXT = 'Custom'.                                     "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = 'Custom Input Program'.                       "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.


  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Root1'                     "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root'                      "#EC NOTEXT
*      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = 'X'
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.
*************

*************
* Column1
  CLEAR ITEM.
  CLEAR ITEM_TABLE.

  ITEM-ITEM_NAME = 'Column1'.    "#EC NOTEXT " Item of Column 'Column1'
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
*  item-text = 'Root Col. 1'.                                "#EC NOTEXT
  ITEM-TEXT = 'Report'.                                     "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = 'Report Program'.                             "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.


  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Root2'                     "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root'                      "#EC NOTEXT
*      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = 'X'
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.
*************



**** Node with key 'Child1'
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                     "#EC NOTEXT
  ITEM-TEXT = text-t01.
  "'Input Actual Production Operation Time'.    "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.


  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child1'                    "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root1'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.


************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  ITEM-TEXT = text-t02.
  "'Input MTTR/MTBF Plan'.                    "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child2'                    "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root1'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  ITEM-TEXT = text-t03.
  "'Input Breakdown Rate Plan'.                "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child3'                    "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root1'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

*************
** Column1
*  CLEAR ITEM_TABLE.
*  CLEAR ITEM.
*  ITEM-ITEM_NAME = 'Column1'.
*  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
**  item-text = 'Child1 Col. 1'.                  "#EC NOTEXT
*  ITEM-TEXT = text-t04.
*  "'Input Preventive Maintenance Plan'.        "#EC NOTEXT
*  APPEND ITEM TO ITEM_TABLE.
*
** Column2
*  CLEAR ITEM.
*  ITEM-ITEM_NAME = 'Column2'.     "
*  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
*  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
**  item-t_image = '@0B@'.
*  APPEND ITEM TO ITEM_TABLE.
*
*  CALL METHOD CRV_TREE_MODEL->ADD_NODE
*    EXPORTING
*      NODE_KEY                = 'Child4'                    "#EC NOTEXT
*      RELATIVE_NODE_KEY       = 'Root1'                     "#EC NOTEXT
*      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
*      ISFOLDER                = SPACE
*      ITEM_TABLE              = ITEM_TABLE
*    EXCEPTIONS
*      NODE_KEY_EXISTS         = 1
*      NODE_KEY_EMPTY          = 2
*      ILLEGAL_RELATIONSHIP    = 3
*      RELATIVE_NODE_NOT_FOUND = 4
*      ERROR_IN_ITEM_TABLE     = 5.
*  IF SY-SUBRC <> 0.
*    MESSAGE A999(ZMMM) WITH 'Error!'.
*  ENDIF.

************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                   "#EC NOTEXT
  ITEM-TEXT = text-t05.
  "'Input Maintenance Cost Plan'.             "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child5'                    "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root1'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                             "#EC NOTEXT
  ITEM-TEXT = text-t06.
  "'Input Actual No. of Production Units'.       "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child6'                    "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root1'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.



************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  ITEM-TEXT = 'Shop Breakdown Trend'.                       "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child7'                    "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root2'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.


************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  ITEM-TEXT = 'Monthly Breakdown Status & TOP-5'.           "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child8'                    "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root2'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

*-commented on 4.25.2014 by Victor :Not used any more
*************
** Column1
*  CLEAR ITEM_TABLE.
*  CLEAR ITEM.
*  ITEM-ITEM_NAME = 'Column1'.
*  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
**  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
*  ITEM-TEXT = 'MTTR / MTBF Satus Report'.                   "#EC NOTEXT
*  APPEND ITEM TO ITEM_TABLE.
*
** Column2
*  CLEAR ITEM.
*  ITEM-ITEM_NAME = 'Column2'.     "
*  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
*  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
**  item-t_image = '@0B@'.
*  APPEND ITEM TO ITEM_TABLE.
*
*  CALL METHOD CRV_TREE_MODEL->ADD_NODE
*    EXPORTING
*      NODE_KEY                = 'Child9'                    "#EC NOTEXT
*      RELATIVE_NODE_KEY       = 'Root2'                     "#EC NOTEXT
*      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
*      ISFOLDER                = SPACE
*      ITEM_TABLE              = ITEM_TABLE
*    EXCEPTIONS
*      NODE_KEY_EXISTS         = 1
*      NODE_KEY_EMPTY          = 2
*      ILLEGAL_RELATIONSHIP    = 3
*      RELATIVE_NODE_NOT_FOUND = 4
*      ERROR_IN_ITEM_TABLE     = 5.
*  IF SY-SUBRC <> 0.
*    MESSAGE A999(ZMMM) WITH 'Error!'.
*  ENDIF.

************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  ITEM-TEXT = 'Preventive / Post Maintenance Ratio'.        "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child10'                   "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root2'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  ITEM-TEXT = 'Maintenance Cost Trend'.                     "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child11'                   "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root2'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

************
* Column1
  CLEAR ITEM_TABLE.
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column1'.
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_LINK.
*  item-text = 'Child1 Col. 1'.                              "#EC NOTEXT
  ITEM-TEXT = 'Each Shop Operation Status'.                  "#EC NOTEXT
  APPEND ITEM TO ITEM_TABLE.

* Column2
  CLEAR ITEM.
  ITEM-ITEM_NAME = 'Column2'.     "
  ITEM-CLASS = CL_COLUMN_TREE_MODEL=>ITEM_CLASS_TEXT.
  ITEM-TEXT = SPACE.                                        "#EC NOTEXT
*  item-t_image = '@0B@'.
  APPEND ITEM TO ITEM_TABLE.

  CALL METHOD CRV_TREE_MODEL->ADD_NODE
    EXPORTING
      NODE_KEY                = 'Child12'                   "#EC NOTEXT
      RELATIVE_NODE_KEY       = 'Root2'                     "#EC NOTEXT
      RELATIONSHIP            = CL_TREE_MODEL=>RELAT_LAST_CHILD
      ISFOLDER                = SPACE
      ITEM_TABLE              = ITEM_TABLE
    EXCEPTIONS
      NODE_KEY_EXISTS         = 1
      NODE_KEY_EMPTY          = 2
      ILLEGAL_RELATIONSHIP    = 3
      RELATIVE_NODE_NOT_FOUND = 4
      ERROR_IN_ITEM_TABLE     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A999(ZMMM) WITH 'Error!'.
  ENDIF.

** add 1000 children to node Child1
*  PERFORM node_3.

ENDFORM.                    " ADD_NODES
*&---------------------------------------------------------------------*
*&      Form  CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR.

ENDFORM.                    " CLEAR
*&---------------------------------------------------------------------*
*&      Form  TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0110  text
*      <--P_TITLE  text
*----------------------------------------------------------------------*
FORM TITLE USING    VALUE(P_DYNNR)
           CHANGING VALUE(P_TITLE).
  CASE P_DYNNR.
    WHEN 9001.
      P_TITLE = text-t01.
    WHEN 9002.
      P_TITLE = text-t02. "'Planned MTTR / MTBF'.
    WHEN 9003.
      P_TITLE = text-t03. "'Planned Breakdown Ratio'.
    WHEN 9004.
      P_TITLE = text-t04. "'Number of Planned Mtnc'.
    WHEN 9005.
      P_TITLE = text-t05. "'Planned Maintenance Cost'.
    WHEN 9006.
      P_TITLE = text-t06. "'Number of Production Unit'.
    WHEN 0101.

  ENDCASE.
ENDFORM.                    " TITLE
*&---------------------------------------------------------------------*
*&      Form  GET_SUBSCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SUBSCREEN USING IM_NODE_KEY
                         EX_DYNNR.
  EX_DYNNR = '9000'.

  DATA: NODE_KEY TYPE  TM_NODEKEY.

  CLEAR: NODE_KEY.
  NODE_KEY = IM_NODE_KEY.
  TRANSLATE NODE_KEY TO UPPER CASE.

  IF NODE_KEY = 'CHILD1'.
    EX_DYNNR = '9001'.
  ELSEIF NODE_KEY = 'CHILD2'.
    EX_DYNNR = '9002'.
  ELSEIF NODE_KEY = 'CHILD3'.
    EX_DYNNR = '9003'.
  ELSEIF NODE_KEY = 'CHILD4'.
    EX_DYNNR = '9004'.
  ELSEIF NODE_KEY = 'CHILD5'.
    EX_DYNNR = '9005'.
  ELSEIF NODE_KEY = 'CHILD6'.
    EX_DYNNR = '9006'.
  ELSEIF NODE_KEY = 'CHILD7'.
    WA_PGM = 'ZRPM04_BDCH'.
    EX_DYNNR = '0101'.
  ELSEIF NODE_KEY = 'CHILD8'.
    WA_PGM = 'ZRPM05_TOP5'.
    EX_DYNNR = '0101'.
  ELSEIF NODE_KEY = 'CHILD9'.
    WA_PGM = 'ZRPM06_MTBT'.
    EX_DYNNR = '0101'.
  ELSEIF NODE_KEY = 'CHILD10'.
    WA_PGM = 'ZRPM07_PMRO'.
    EX_DYNNR = '0101'.
  ELSEIF NODE_KEY = 'CHILD11'.
    WA_PGM = 'ZRPM08_PMCO'.
    EX_DYNNR = '0101'.
  ELSEIF NODE_KEY = 'CHILD12'.
    WA_PGM = 'ZRPM03_STATUS'.
    EX_DYNNR = '0101'.
  ENDIF.
ENDFORM.                    " GET_SUBSCREEN
