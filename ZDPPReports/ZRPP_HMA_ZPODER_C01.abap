*----------------------------------------------------------------------*
*   INCLUDE ZSJ_TEST007_C01                                            *
*----------------------------------------------------------------------*
INCLUDE ZRPP_COMMON_TREE.
INCLUDE <ICON>.

*----------------------------------------------------------------------*
* ### DEFINITION.
*----------------------------------------------------------------------*
*#SCREEN
CLASS SCREEN_INIT DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-DATA : LIST_VIEWER TYPE REF TO CL_GUI_ALV_GRID.
* Method
    CLASS-METHODS :
       INIT_SCREEN.

    METHODS :
       CONSTRUCTOR.

  PRIVATE SECTION.
* Class varient
    DATA:
      SPLITTER_H TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      SPLITTER_V TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      PICTURE TYPE REF TO CL_GUI_PICTURE,
      TREE TYPE REF TO CL_GUI_SIMPLE_TREE.
* Method
    METHODS :
      FILL_TREE. " Custom Define
ENDCLASS.

*#HANDLER
CLASS SCREEN_HANDLER DEFINITION.

  PUBLIC SECTION.
    DATA :  LIST_VIEWER TYPE REF TO CL_GUI_ALV_GRID.

    METHODS:
      CONSTRUCTOR
             IMPORTING CONTAINER TYPE REF TO CL_GUI_CONTAINER
                       GRID      TYPE REF TO CL_GUI_ALV_GRID,

      HANDLE_NODE_DOUBLE_CLICK
             FOR EVENT NODE_DOUBLE_CLICK
             OF CL_GUI_SIMPLE_TREE
             IMPORTING NODE_KEY .


  PRIVATE SECTION.
    DATA:
      HTML_VIEWER TYPE REF TO CL_GUI_HTML_VIEWER.
    METHODS:
      FILL_LIST IMPORTING UPDDAT LIKE EDIDC-UPDDAT
                          NO     LIKE GT_DATA-NO.
ENDCLASS.

*----------------------------------------------------------------------*
*# End DEFINITION
*----------------------------------------------------------------------*


CLASS SCREEN_INIT IMPLEMENTATION.

  METHOD INIT_SCREEN.
    DATA SCREEN TYPE REF TO SCREEN_INIT.
    CREATE OBJECT SCREEN.
  ENDMETHOD.

  METHOD CONSTRUCTOR.
    DATA: EVENTS TYPE CNTL_SIMPLE_EVENTS,
          EVENT LIKE LINE OF EVENTS,
          EVENT_HANDLER TYPE REF TO SCREEN_HANDLER,
          CONTAINER_LEFT TYPE REF TO CL_GUI_CONTAINER,
          CONTAINER_RIGHT TYPE REF TO CL_GUI_CONTAINER,

          CONTAINER_TOP TYPE REF TO CL_GUI_CONTAINER,
          CONTAINER_BOTTOM TYPE REF TO CL_GUI_CONTAINER.

    CREATE OBJECT SPLITTER_H
           EXPORTING
           PARENT = CL_GUI_CONTAINER=>SCREEN0
           ROWS = 1
           COLUMNS = 2.

    CALL METHOD SPLITTER_H->SET_BORDER
         EXPORTING BORDER = CL_GUI_CFW=>FALSE.

    CALL METHOD SPLITTER_H->SET_COLUMN_MODE
         EXPORTING MODE = SPLITTER_H->MODE_ABSOLUTE.

    CALL METHOD SPLITTER_H->SET_COLUMN_WIDTH
         EXPORTING ID = 1
         WIDTH = 170.

    CONTAINER_LEFT  = SPLITTER_H->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
    CONTAINER_RIGHT = SPLITTER_H->GET_CONTAINER( ROW = 1 COLUMN = 2 ).
    G_GUI_CONTAINER1 =  CONTAINER_RIGHT.

    CREATE OBJECT SPLITTER_V
           EXPORTING
           PARENT = CONTAINER_LEFT
           ROWS = 2
           COLUMNS = 1.

    CALL METHOD SPLITTER_V->SET_BORDER
         EXPORTING BORDER = CL_GUI_CFW=>FALSE.

    CALL METHOD SPLITTER_V->SET_ROW_MODE
         EXPORTING MODE = SPLITTER_V->MODE_ABSOLUTE.

    CALL METHOD SPLITTER_V->SET_ROW_HEIGHT
         EXPORTING ID = 1
         HEIGHT = 800.

*    CONTAINER_TOP    = SPLITTER_V->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
    CONTAINER_BOTTOM = SPLITTER_V->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

*    CREATE OBJECT PICTURE
*           EXPORTING PARENT = CONTAINER_TOP.
    CREATE OBJECT: LIST_VIEWER EXPORTING I_PARENT = CONTAINER_RIGHT.

    CREATE OBJECT TREE
           EXPORTING
           PARENT = CONTAINER_BOTTOM
           NODE_SELECTION_MODE =
                   CL_GUI_SIMPLE_TREE=>NODE_SEL_MODE_SINGLE.

    CREATE OBJECT  EVENT_HANDLER
           EXPORTING CONTAINER = CONTAINER_RIGHT
                     GRID  = LIST_VIEWER.

    EVENT-EVENTID = CL_GUI_SIMPLE_TREE=>EVENTID_NODE_DOUBLE_CLICK.

    EVENT-APPL_EVENT = ' '.   "system event, does not trigger PAI

    APPEND EVENT TO EVENTS.

    CALL METHOD TREE->SET_REGISTERED_EVENTS
         EXPORTING EVENTS = EVENTS.

    SET HANDLER EVENT_HANDLER->HANDLE_NODE_DOUBLE_CLICK FOR TREE.

    CALL METHOD: ME->FILL_TREE.
  ENDMETHOD.



  METHOD FILL_TREE.

    DATA: NODE_TABLE TYPE TABLE OF ABDEMONODE,
          NODE TYPE ABDEMONODE,
          LS_DATA LIKE GT_DATA.

    NODE-HIDDEN = ' '.                 " All nodes are visible,
    NODE-DISABLED = ' '.               " selectable,
    NODE-ISFOLDER = 'X'.                                    " a folder,
    NODE-EXPANDER = ' '.               " have no '+' sign for expansion.


    LOOP AT GT_DATA INTO LS_DATA.
      GT_DATA = LS_DATA .
      AT NEW UPDDAT.
        NODE-NODE_KEY = GT_DATA-UPDDAT.
        CLEAR : NODE-RELATKEY,
                NODE-RELATSHIP.

        WRITE LS_DATA-UPDDAT TO NODE-TEXT MM/DD/YYYY.
        NODE-N_IMAGE   =   ' '.
        NODE-EXP_IMAGE =   ' '.

        APPEND NODE TO NODE_TABLE.
      ENDAT.

      AT NEW DOCNUM.
        NODE-NODE_KEY = GT_DATA-NO.
        NODE-RELATKEY = GT_DATA-UPDDAT.
        NODE-RELATSHIP = CL_GUI_SIMPLE_TREE=>RELAT_LAST_CHILD.
        CONCATENATE GT_DATA-UPDTIM+0(2)
                    GT_DATA-UPDTIM+2(2)
                    GT_DATA-UPDTIM+4(2)
                    INTO NODE-TEXT SEPARATED BY ':'.
        NODE-N_IMAGE =   '@AW@'.       "AV is the internal code
        NODE-EXP_IMAGE = '@AW@'.       "for an airplane icon
        APPEND NODE TO NODE_TABLE.
      ENDAT.

    ENDLOOP.


    DELETE ADJACENT DUPLICATES FROM NODE_TABLE.
    CALL METHOD TREE->ADD_NODES
         EXPORTING TABLE_STRUCTURE_NAME = 'ABDEMONODE'
                   NODE_TABLE = NODE_TABLE.
  ENDMETHOD.

ENDCLASS.

*

CLASS SCREEN_HANDLER IMPLEMENTATION.
*
  METHOD CONSTRUCTOR.
    LIST_VIEWER = GRID.
  ENDMETHOD.

  METHOD HANDLE_NODE_DOUBLE_CLICK.
    DATA: UPDDAT LIKE EDIDC-UPDDAT,
          NO     LIKE GT_DATA-NO.

    NO     = NODE_KEY.
    UPDDAT = NODE_KEY.

    CALL METHOD: FILL_LIST EXPORTING  UPDDAT = UPDDAT
                                         NO     = NO  ,
     LIST_VIEWER->SET_VISIBLE EXPORTING VISIBLE = 'X'.
    PERFORM P1010_SET_GRID_EVENTS
            USING LIST_VIEWER GS_TOOLBAR .

    CALL METHOD CL_GUI_CFW=>FLUSH.

  ENDMETHOD.


  METHOD FILL_LIST.
    PERFORM P1000_CREATE_GRID USING LIST_VIEWER NO UPDDAT ''.
  ENDMETHOD.





ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
FORM LVC_SET_SORT_C TABLES  P_SORT
                    USING   P_SPOS
                            P_FIELD
                            P_UP
                            P_DOWN
                            P_GROUP
                            P_SUBTOT
                            P_COMP
                            P_EXPA  .

  DATA: LS_SORT TYPE LVC_S_SORT.
*
  LS_SORT-SPOS      = P_SPOS .
  LS_SORT-FIELDNAME = P_FIELD.
  LS_SORT-UP        = P_UP.
  LS_SORT-DOWN      = P_DOWN.
  LS_SORT-GROUP     = P_GROUP.
  LS_SORT-SUBTOT    = P_SUBTOT.
  LS_SORT-COMP      = P_COMP.
  LS_SORT-EXPA      = P_EXPA.
*
  INSERT LS_SORT INTO TABLE P_SORT.

ENDFORM.                    " SET_SORT

*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCAT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM GET_FILEDCAT_ALV  USING GT_FIELDCAT TYPE LVC_T_FCAT P_TNAME.


  DATA : LS_FIELDCAT TYPE LVC_S_FCAT.
  DATA : LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
         L_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.

  CLEAR : LT_FIELDCAT ,LT_FIELDCAT[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = GV_REPID "'ZRPP_HMA_ZPODER'
      I_INTERNAL_TABNAME     = P_TNAME
*      I_BYPASSING_BUFFER     = 'X'
      I_INCLNAME             = GV_REPID "'ZRPP_HMA_ZPODER'
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  CLEAR : GT_FIELDCAT , GT_FIELDCAT[].

  LOOP AT LT_FIELDCAT INTO L_FIELDCAT.
    CLEAR : LS_FIELDCAT.
    MOVE-CORRESPONDING L_FIELDCAT TO LS_FIELDCAT.

    LS_FIELDCAT-REPTEXT   = L_FIELDCAT-SELTEXT_S.
    LS_FIELDCAT-REF_TABLE = L_FIELDCAT-REF_TABNAME.
    LS_FIELDCAT-KEY       = SPACE.

    IF  R_NEW EQ 'X'.
      CASE LS_FIELDCAT-FIELDNAME.
        WHEN 'POYEAR'   . LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'POMONTH'  . LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'ORDQTY'   .
         LS_FIELDCAT-COL_POS = 16.
        "LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'NEWQTY'   .
         LS_FIELDCAT-COL_POS = 17.
        "LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'ZSDAT'    . LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'ZSTIM'	   . LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'ZUSER'	   . LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'NO' . LS_FIELDCAT-NO_OUT = 'X'.

        WHEN 'INITQTY'. LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'MODQTY '. LS_FIELDCAT-NO_OUT = 'X'.
* by Daniel {
        WHEN 'WOUPS'.
             LS_FIELDCAT-SCRTEXT_S = 'Spec W/O'.
* }


      ENDCASE.
    ELSE.
      CASE LS_FIELDCAT-FIELDNAME.
        WHEN 'DEST'.    LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'MOYE'.    LS_FIELDCAT-NO_OUT = 'X'.

        WHEN 'VERS'.    LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'INITQTY'. LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'MODQTY '. LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'LCNO'.    LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'LCNT'.    LS_FIELDCAT-NO_OUT = 'X'.

        WHEN 'REQ_DATE'. LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'CRT_DATE'. LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'CHG_DATE'. LS_FIELDCAT-NO_OUT = 'X'.
        WHEN 'NO' . LS_FIELDCAT-NO_OUT = 'X'.
* by Daniel {
        WHEN 'WOUPS'.
             LS_FIELDCAT-SCRTEXT_S = 'Spec W/O'.
* }



      ENDCASE.
    ENDIF.

    CASE LS_FIELDCAT-FIELDNAME.

      WHEN 'STATUS'.
        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.

        LS_FIELDCAT-COL_POS = 0.
      WHEN 'DOCNUM'.
        LS_FIELDCAT-KEY  = 'X'.
      WHEN 'UPDDAT'.
        LS_FIELDCAT-KEY  = 'X'.
      WHEN 'UPDTIM'.
        LS_FIELDCAT-KEY  = 'X'.
      WHEN 'WO_SER'.
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'NATION'.
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'DEALER'.
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'EXTC'  .
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.
      WHEN 'INTC'  .
        LS_FIELDCAT-EMPHASIZE = 'C210'.
*        LS_FIELDCAT-KEY  = 'X'.
        LS_FIELDCAT-FIX_COLUMN = 'X'.

      WHEN 'ORDQTY'   .
        LS_FIELDCAT-DO_SUM = 'X'.
      WHEN 'NEWQTY'   .
        LS_FIELDCAT-DO_SUM = 'X'.

      WHEN 'INITQTY'.
        LS_FIELDCAT-DO_SUM   = 'X'.
        LS_FIELDCAT-REPTEXT = 'Init.Qty'.

      WHEN 'MODQTY'.
        LS_FIELDCAT-DO_SUM   = 'X'.
        LS_FIELDCAT-REPTEXT = 'Mod.Qty'.
    ENDCASE.

    APPEND LS_FIELDCAT TO GT_FIELDCAT.

  ENDLOOP.

ENDFORM.                    " GET_FILEDCAT_ALV

*******************************************************
*Set Grid Setting

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID  text
*      -->P_0097   text
*----------------------------------------------------------------------*
FORM P1010_SET_GRID_EVENTS
  USING P_GRID TYPE REF TO CL_GUI_ALV_GRID
        P_TOOLBAR.

  DATA : P_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET,
         P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL,
         PS_ROW_NO     TYPE LVC_S_ROID,
         PR_EVENT_DATA TYPE REF TO CL_ALV_EVENT_DATA,
         PT_BAD_CELLS  TYPE LVC_T_MODI.

  CREATE OBJECT G_EVENTS.

*****  이벤트 핸들러 등록
*_DOUBLE CLICK
  SET HANDLER G_EVENTS->DOUBLE_CLICK FOR P_GRID.
* HOTSPOT
  SET HANDLER G_EVENTS->HOTSPOT_CLICK FOR P_GRID.

*_DATA CHANGED
  SET HANDLER G_EVENTS->DATA_CHANGED FOR P_GRID.
  PERFORM EVENT_DATA_CHANGED
          USING P_ER_DATA_CHANGED '' '' '' ''.

*_DATA CHANGED FINISHED
  SET HANDLER G_EVENTS->DATA_CHANGED_FINISHED FOR P_GRID.
  PERFORM EVENT_DATA_CHANGED_FINIS
          USING ''.
  SET HANDLER G_EVENTS->PRINT_TOP_OF_PAGE FOR P_GRID.

  SET HANDLER G_EVENTS->USER_COMMAND FOR P_GRID.
  PERFORM  EVENT_UCOMM
          USING ''
                '' .
  SET HANDLER G_EVENTS->MENU_BUTTON FOR P_GRID.
*  CHECK NOT P_TOOLBAR IS INITIAL.
  SET HANDLER G_EVENTS->TOOLBAR FOR P_GRID.
  PERFORM  EVENT_TOOLBAR
          USING P_OBJECT
                'X' ''.

ENDFORM.                    " P1010_SET_GRID_EVENTS

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ER_DATA_CHANGED  text
*      -->P_0213   text
*      -->P_0214   text
*      -->P_0215   text
*      -->P_0216   text
*----------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED  USING    P_DATA_CHANGED
                                        VALUE(P_0213)
                                        VALUE(P_0214)
                                        VALUE(P_0215)
                                        VALUE(P_0216).

ENDFORM.                    " P1050_EVENT_DATA_CHANGED

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED_FINIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED_FINIS  USING    VALUE(P_0225).

ENDFORM.                    " P1060_EVENT_DATA_CHANGED_FINIS

*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0243   text
*      -->P_0244   text
*----------------------------------------------------------------------*
FORM EVENT_UCOMM   USING   E_UCOMM LIKE SY-UCOMM
                                                  P_CHECK.
**---------------------------------------------------------------
*  CHECK P_CHECK EQ 'X'.
**
**---------------------------------------------------------------
*  CASE E_UCOMM.
**___재전송
*    WHEN '&CS03'.
*      PERFORM P3000_CALL_CS03.
*  ENDCASE.

ENDFORM.                    " P1020_EVENT_UCOMM

*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OBJECT  text
*      -->P_0254   text
*      -->P_0255   text
*----------------------------------------------------------------------*
FORM EVENT_TOOLBAR
   USING  E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
               E_INTERACTIVE TYPE C
               P_CHECK.

*---------------------------------------------------------------
  CHECK P_CHECK EQ 'X' .

*---------------------------------------------------------------
  DATA : LS_TOOLBAR  TYPE STB_BUTTON.

*_SET : BUTTON TYPE - SEPARATOR
  CLEAR : LS_TOOLBAR.
  LS_TOOLBAR-BUTN_TYPE = 3.
  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.

ENDFORM.                    " P1030_EVENT_TOOLBAR

*&--------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_0201   text
*      -->P_0202   text
*      -->P_0203   text
*---------------------------------------------------------------------*
FORM EVENT_DOUBLE_CLICK  USING    VALUE(P_0201)
                                             VALUE(P_0202)
                                             VALUE(P_0203).

ENDFORM.                    " P1040_EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  P1000_CREATE_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIST_VIEWER  text
*----------------------------------------------------------------------*
FORM P1000_CREATE_GRID USING    LIST_VIEWER TYPE REF TO CL_GUI_ALV_GRID
                                NO
                                UPDDAT
                                VALUE(P_DOCNUM).

  DATA : VIEW_TAB  LIKE TABLE OF GT_DATA ,
         VIEW_LINE LIKE LINE  OF GT_DATA ,
         VIEW_TITLE(200) ,

        LIST_LAYOUT TYPE LVC_S_LAYO,
        LIST_SORT   TYPE LVC_T_SORT.

  DATA : LS_ZSEG1 LIKE GT_ZPOSEG1.

  CLEAR : LS_ZSEG1, VIEW_TAB, VIEW_LINE, VIEW_TITLE, GV_DOCNUM.

  IF P_DOCNUM NE ''.
    CALL METHOD LIST_VIEWER->FREE.

    CREATE OBJECT: LIST_VIEWER
      EXPORTING I_PARENT =  G_GUI_CONTAINER1.

    LOOP AT GT_DATA INTO  VIEW_LINE
                WHERE DOCNUM = P_DOCNUM.
      APPEND VIEW_LINE TO VIEW_TAB.
    ENDLOOP.

    SORT VIEW_TAB BY WO_SER .
    GV_DOCNUM = VIEW_LINE-DOCNUM.

  ELSE.

    LOOP AT GT_DATA INTO  VIEW_LINE
                    WHERE NO = NO.

      APPEND VIEW_LINE TO VIEW_TAB.
    ENDLOOP.

    SORT VIEW_TAB BY WO_SER .
    GV_DOCNUM = VIEW_LINE-DOCNUM.


    IF VIEW_LINE IS INITIAL.
      CLEAR : VIEW_LINE, GV_DOCNUM.
      LOOP AT GT_DATA INTO  VIEW_LINE
                WHERE UPDDAT = UPDDAT.

        APPEND VIEW_LINE TO VIEW_TAB.
      ENDLOOP.

      CLEAR :  VIEW_LINE-DOCNUM.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = VIEW_LINE-DOCNUM
       IMPORTING
            OUTPUT = VIEW_LINE-DOCNUM.

  DATA : TAB_LINE(3) TYPE N.
  DESCRIBE TABLE VIEW_TAB LINES TAB_LINE .

  DATA : LV_TXT(20) .
  READ TABLE GT_IDOC WITH KEY DOCNUM = GV_DOCNUM .
  IF GT_IDOC-TYPE EQ 'X' .
    LV_TXT = 'Create New PO List '.
  ELSE.
    LV_TXT = 'Modify PO List ' .
  ENDIF.
  CONCATENATE LV_TXT '['
              VIEW_LINE-UPDDAT+4(2)
              VIEW_LINE-UPDDAT+6(2)
              VIEW_LINE-UPDDAT+0(4)']  '
              VIEW_LINE-DOCNUM
              TAB_LINE ' Counts'
   INTO VIEW_TITLE SEPARATED BY SPACE.



*# Set Grid
  GS_O_LAYOUT-REPORT = G_REPID_C = GV_REPID .
  PERFORM LVC_SET_SORT_C
    TABLES LIST_SORT USING '1' 'UPDDAT'  'X' '' '' '' '' '' .
  PERFORM LVC_SET_SORT_C
    TABLES LIST_SORT USING '2' 'DOCNUM'  'X' '' '' '' '' '' .
  PERFORM LVC_SET_SORT_C
    TABLES LIST_SORT USING '3' 'WO_SER'  'X' '' '' '' '' '' .
  PERFORM LVC_SET_SORT_C
    TABLES LIST_SORT USING '4' 'NATION'  'X' '' ''  '' '' '' .
  PERFORM LVC_SET_SORT_C
    TABLES LIST_SORT USING '5' 'DEALER'  'X' '' ''  '' '' '' .
  PERFORM LVC_SET_SORT_C
    TABLES LIST_SORT USING '6' 'EXTC'  'X' '' ''  '' '' '' .
  PERFORM LVC_SET_SORT_C
    TABLES LIST_SORT USING '7' 'INTC'  'X' '' ''  '' '' '' .


  LIST_LAYOUT-GRID_TITLE = VIEW_TITLE.
  LIST_LAYOUT-SMALLTITLE = 'X'.      "The list title has small fonts,
  LIST_LAYOUT-CWIDTH_OPT = 'X'.      "the column width is adjusted,
  LIST_LAYOUT-NO_TOOLBAR = 'X'.      "the toolbar is suppressed.
  LIST_LAYOUT-STYLEFNAME = 'H_STYLE'.
  LIST_LAYOUT-SEL_MODE   = 'B'.


  GS_LAYOUT = LIST_LAYOUT.
  GT_DATA_DETAIL[] = VIEW_TAB[].

  PERFORM GET_FILEDCAT_ALV  USING GT_FIELDCAT[] 'GT_DATA'.


  CALL METHOD LIST_VIEWER->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_SAVE            = 'A'
                 I_DEFAULT         = 'X'
                 IS_LAYOUT         = LIST_LAYOUT
                 IS_VARIANT        = GS_O_LAYOUT
*                 IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
       CHANGING  IT_OUTTAB         = GT_DATA_DETAIL[]
                 IT_FIELDCATALOG   = GT_FIELDCAT[]
                 IT_SORT           = LIST_SORT
       EXCEPTIONS
                  INVALID_PARAMETER_COMBINATION = 1
                  PROGRAM_ERROR                 = 2
                  TOO_MANY_LINES                = 3
                  OTHERS                        = 4 .
  G_GRID = LIST_VIEWER.
ENDFORM.                    " P1000_CREATE_GRID
