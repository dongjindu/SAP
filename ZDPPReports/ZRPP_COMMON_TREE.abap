*----------------------------------------------------------------------*
*   INCLUDE ZRSJ_COMMON_ALVC                                           *
*----------------------------------------------------------------------*

DATA: G_TEMP_OK_CODE TYPE SY-UCOMM.
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_COMMON_ALVC                                           *
*----------------------------------------------------------------------*
**S> 08/05/11 Paul : ECC60
*CLASS LCL_APPLICATION01    DEFINITION DEFERRED.        " TREE
**E<
*----------------------------------------------------------------------*
*   INCLUDE TREE_EVENT                                                 *
*----------------------------------------------------------------------*
CLASS LCL_APPLICATION01 DEFINITION.

  PUBLIC SECTION.
*   double click item
    METHODS HANDLE_DOUBLE_CLICK
      FOR EVENT NODE_DOUBLE_CLICK OF CL_GUI_ALV_TREE
      IMPORTING NODE_KEY.
*   Drag

ENDCLASS.                    "LCL_APPLICATION01 DEFINITION
*---------------------------------------------------------------------*
*       CLASS CL_TREE_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_APPLICATION01 IMPLEMENTATION.
* handle double_click

  METHOD HANDLE_DOUBLE_CLICK.
    CHECK NOT NODE_KEY IS INITIAL.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK



ENDCLASS.                    "LCL_APPLICATION01 IMPLEMENTATION


*DATA: TREE_EVENT_RECEIVER TYPE REF TO CL_TREE_EVENT_RECEIVER.

CLASS CL_GUI_CFW         DEFINITION LOAD.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENTS DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENTS DEFINITION.
  PUBLIC SECTION.
    DATA: MR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
    METHODS:
    USER_COMMAND         FOR EVENT USER_COMMAND
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_UCOMM SENDER,
    BEFORE_USER_COMMAND  FOR EVENT BEFORE_USER_COMMAND
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_UCOMM,
    AFTER_USER_COMMAND   FOR EVENT AFTER_USER_COMMAND
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_UCOMM,
    DOUBLE_CLICK         FOR EVENT DOUBLE_CLICK
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO,
    HOTSPOT_CLICK        FOR EVENT HOTSPOT_CLICK
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW_ID
                                   E_COLUMN_ID
                                   ES_ROW_NO,
    MENU_BUTTON          FOR EVENT MENU_BUTTON
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_OBJECT
                                   E_UCOMM,
    TOOLBAR              FOR EVENT TOOLBAR
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_OBJECT
                                   E_INTERACTIVE,
    CONTEXT_MENU_REQUEST FOR EVENT CONTEXT_MENU_REQUEST
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_OBJECT,
    TOP_OF_PAGE          FOR EVENT TOP_OF_PAGE
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_DYNDOC_ID,
    END_OF_LIST          FOR EVENT END_OF_LIST
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_DYNDOC_ID,
    PRINT_TOP_OF_PAGE    FOR EVENT PRINT_TOP_OF_PAGE
                         OF CL_GUI_ALV_GRID,
    PRINT_END_OF_PAGE    FOR EVENT PRINT_END_OF_PAGE
                         OF CL_GUI_ALV_GRID,
    PRINT_TOP_OF_LIST    FOR EVENT PRINT_TOP_OF_LIST
                         OF CL_GUI_ALV_GRID,
    PRINT_END_OF_LIST    FOR EVENT PRINT_END_OF_LIST
                         OF CL_GUI_ALV_GRID,
    AFTER_REFRESH        FOR EVENT AFTER_REFRESH
                         OF CL_GUI_ALV_GRID,
    DELAYED_CALLBACK     FOR EVENT DELAYED_CALLBACK
                         OF CL_GUI_ALV_GRID,
    DELAYED_CHANGED_SEL_CALLBACK
                         FOR EVENT DELAYED_CHANGED_SEL_CALLBACK
                         OF CL_GUI_ALV_GRID,
    SUBTOTAL_TEXT        FOR EVENT SUBTOTAL_TEXT
                         OF CL_GUI_ALV_GRID
                         IMPORTING ES_SUBTOTTXT_INFO
                                   EP_SUBTOT_LINE
                                   E_EVENT_DATA,
    ONDRAG               FOR EVENT ONDRAG
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ,
    ONDROP               FOR EVENT ONDROP
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ,
    ONDROPCOMPLETE       FOR EVENT ONDROPCOMPLETE
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ,
    ONDROPGETFLAVOR      FOR EVENT ONDROPGETFLAVOR
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_ROW
                                   E_COLUMN
                                   ES_ROW_NO
                                   E_DRAGDROPOBJ
                                   E_FLAVORS,
    DATA_CHANGED         FOR EVENT DATA_CHANGED
                         OF CL_GUI_ALV_GRID
                         IMPORTING ER_DATA_CHANGED
                                   E_ONF4
                                   E_ONF4_BEFORE
                                   E_ONF4_AFTER,
    DATA_CHANGED_FINISHED
                         FOR EVENT DATA_CHANGED_FINISHED
                         OF CL_GUI_ALV_GRID,
    BUTTON_CLICK         FOR EVENT BUTTON_CLICK
                         OF CL_GUI_ALV_GRID
                         IMPORTING ES_COL_ID
                                   ES_ROW_NO,
    ONF1                 FOR EVENT ONF1
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_FIELDNAME
                                   ES_ROW_NO
                                   ER_EVENT_DATA,
    ONF4                 FOR EVENT ONF4
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_FIELDNAME
                                   E_FIELDVALUE
                                   ES_ROW_NO
                                   ER_EVENT_DATA
                                   ET_BAD_CELLS
                                   E_DISPLAY.

    METHODS : ON_F4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
      IMPORTING SENDER
                E_FIELDNAME
                E_FIELDVALUE
                ES_ROW_NO
                ER_EVENT_DATA
                ET_BAD_CELLS
                E_DISPLAY.

    TYPES : DDSHRETVAL_TABLE TYPE TABLE OF DDSHRETVAL.

    METHODS : MY_F4
          IMPORTING SENDER         TYPE REF TO CL_GUI_ALV_GRID
                    ET_BAD_CELLS   TYPE LVC_T_MODI
                    ES_ROW_NO      TYPE LVC_S_ROID
                    ER_EVENT_DATA  TYPE REF TO CL_ALV_EVENT_DATA
                    E_DISPLAY      TYPE C
                    E_FIELDNAME    TYPE LVC_FNAME
          EXPORTING LT_F4          TYPE DDSHRETVAL_TABLE.

ENDCLASS.                    "lcl_events DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_events_d0100 IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENTS IMPLEMENTATION.

*---------------------------------------------------------------
*_METHOD user_command
  METHOD USER_COMMAND.

    PERFORM EVENT_UCOMM IN PROGRAM (SY-CPROG)
            USING E_UCOMM 'X' .

  ENDMETHOD.                    "user_command

*---------------------------------------------------------------
*_METHOD before_user_command
  METHOD BEFORE_USER_COMMAND.
*    PERFORM D0100_EVENT_BEFORE_UCOMM
*            USING E_UCOMM.
  ENDMETHOD.                    "before_user_command

*---------------------------------------------------------------
*_METHOD after_user_command
  METHOD AFTER_USER_COMMAND.
*    PERFORM D0100_EVENT_AFTER_UCOMM
*            USING E_UCOMM.
  ENDMETHOD.                    "after_user_command

*---------------------------------------------------------------
*_METHOD double_click
  METHOD DOUBLE_CLICK.
*    PERFORM D0100_EVENT_DOUBLE_CLICK
*            USING E_ROW
*            E_COLUMN.
    PERFORM EVENT_DOUBLE_CLICK
            IN PROGRAM (SY-CPROG)
            USING E_ROW
                  E_COLUMN
                  ES_ROW_NO.                   .
  ENDMETHOD.                    "double_click

*---------------------------------------------------------------
*_METHOD hotspot_click
  METHOD HOTSPOT_CLICK.
*    PERFORM EVENT_HOTSPOT_CLICK
*            USING E_ROW_ID
*                  E_COLUMN_ID.
  ENDMETHOD.                    "hotspot_click

*---------------------------------------------------------------
*_METHOD menu_button
  METHOD MENU_BUTTON.
*    PERFORM D0100_EVENT_MENU_BUTTON
*            USING E_OBJECT
*                  E_UCOMM.
  ENDMETHOD.                    "menu_button

*---------------------------------------------------------------
*_METHOD toolbar
  METHOD TOOLBAR.
    PERFORM EVENT_TOOLBAR
            IN PROGRAM (SY-CPROG)
            USING E_OBJECT
                  E_INTERACTIVE
                  'X'.
  ENDMETHOD.                    "toolbar

*---------------------------------------------------------------
*_METHOD context_menu_request
  METHOD CONTEXT_MENU_REQUEST.
*    PERFORM D0100_EVENT_CONTEXT_MENU_REQST
*            USING E_OBJECT.
  ENDMETHOD.                    "context_menu_request

*---------------------------------------------------------------
*_METHOD top_of_page
  METHOD TOP_OF_PAGE.
*    PERFORM D0100_EVENT_TOP_OF_PAGE
*            USING E_DYNDOC_ID.
  ENDMETHOD.                    "top_of_page

*---------------------------------------------------------------
*_METHOD end_of_list
  METHOD END_OF_LIST.
*    PERFORM D0100_EVENT_END_OF_LIST
*            USING E_DYNDOC_ID.
  ENDMETHOD.                    "end_of_list

*---------------------------------------------------------------
*_METHOD print_top_of_page
  METHOD PRINT_TOP_OF_PAGE.
*    PERFORM D0100_EVENT_PRINT_TOP_OF_PAGE.
  ENDMETHOD.                    "print_top_of_page

*---------------------------------------------------------------
*_METHOD print_end_of_page
  METHOD PRINT_END_OF_PAGE.
*    PERFORM D0100_EVENT_PRINT_END_OF_PAGE.
  ENDMETHOD.                    "print_end_of_page

*---------------------------------------------------------------
*_METHOD print_top_of_list
  METHOD PRINT_TOP_OF_LIST.
*    PERFORM D0100_EVENT_PRINT_TOP_OF_LIST.
  ENDMETHOD.                    "print_top_of_list

*---------------------------------------------------------------
*_METHOD print_end_of_list
  METHOD PRINT_END_OF_LIST.
*    PERFORM D0100_EVENT_PRINT_END_OF_LIST.
  ENDMETHOD.                    "print_end_of_list

*---------------------------------------------------------------
*_METHOD after_refresh
  METHOD AFTER_REFRESH.
    PERFORM EVENT_AFTER_REFRESH
            IN PROGRAM (SY-CPROG)
            USING 'X'.
  ENDMETHOD.                    "after_refresh

*---------------------------------------------------------------
*_METHOD delayed_callback
  METHOD DELAYED_CALLBACK.
*    PERFORM D0100_EVENT_DELAYED_CALLBACK.
  ENDMETHOD.                    "delayed_callback

*---------------------------------------------------------------
*_METHOD delayed_changed_sel_callba
  METHOD DELAYED_CHANGED_SEL_CALLBACK.
*    PERFORM D0100_EVENT_CHANGED_SEL_CALLBA.
  ENDMETHOD.                    "delayed_changed_sel_callback

*---------------------------------------------------------------
*_METHOD subtotal_text
  METHOD SUBTOTAL_TEXT.
*    PERFORM D0100_EVENT_SUBTOTAL_TEXT
*            USING ES_SUBTOTTXT_INFO
*                  EP_SUBTOT_LINE
*                  E_EVENT_DATA.
  ENDMETHOD.                    "subtotal_text

*---------------------------------------------------------------
*_METHOD ondrag
  METHOD ONDRAG.
*    PERFORM D0100_EVENT_ONDRAG
*            USING E_ROW
*                  E_COLUMN
*                  E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondrag

*---------------------------------------------------------------
*_METHOD ondrop
  METHOD ONDROP.
*    PERFORM D0100_EVENT_ONDROP
*            USING E_ROW
*            E_COLUMN
*            E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondrop

*---------------------------------------------------------------
*_METHOD ondropcomplete
  METHOD ONDROPCOMPLETE.
*    PERFORM D0100_EVENT_ONDROPCOMPLETE
*            USING E_ROW
*                  E_COLUMN
*                  E_DRAGDROPOBJ.
  ENDMETHOD.                    "ondropcomplete

*---------------------------------------------------------------
*_METHOD ondropgetflavor
  METHOD ONDROPGETFLAVOR.

  ENDMETHOD.                    "ondropgetflavor

*---------------------------------------------------------------
*_METHOD data_changed
  METHOD DATA_CHANGED.
    PERFORM EVENT_DATA_CHANGED
            IN PROGRAM (SY-CPROG)
             USING ER_DATA_CHANGED
                   E_ONF4
                   E_ONF4_BEFORE
                   E_ONF4_AFTER
                   'X' .
  ENDMETHOD.                    "data_changed

*---------------------------------------------------------------
*       METHOD data_changed_finished
  METHOD DATA_CHANGED_FINISHED.
    PERFORM EVENT_DATA_CHANGED_FINIS
            IN PROGRAM (SY-CPROG)
            USING 'X'.
  ENDMETHOD.                    "data_changed_finished

*---------------------------------------------------------------
*_METHOD button_click
  METHOD BUTTON_CLICK.
*    PERFORM D0100_EVENT_BUTTON_CLICK
*            USING ES_COL_ID
*                  ES_ROW_NO.
  ENDMETHOD.                    "button_click

*---------------------------------------------------------------
*_METHOD onf1
  METHOD ONF1.
*    PERFORM D0100_EVENT_ONF1
*            USING E_FIELDNAME
*                  ES_ROW_NO
*                  ER_EVENT_DATA.
  ENDMETHOD.                                                "ONF1

*---------------------------------------------------------------
*_METHOD onf4
  METHOD ONF4.
    PERFORM EVENT_ONF4
            IN PROGRAM (SY-CPROG)
            USING E_FIELDNAME
                  E_FIELDVALUE
                  ES_ROW_NO
                  ER_EVENT_DATA
                  ET_BAD_CELLS
                  E_DISPLAY
                  'X'      .
  ENDMETHOD.                                                "ONF4

*---------------------------------------------------------------
  METHOD ON_F4.
    PERFORM EVENT_ON_F4
            IN PROGRAM (SY-CPROG)
            USING SENDER
                 E_FIELDNAME
                 E_FIELDVALUE
                 ES_ROW_NO
                 ER_EVENT_DATA
                 ET_BAD_CELLS
                 E_DISPLAY
                 'X'.
  ENDMETHOD.                                                "ON_F4

*---------------------------------------------------------------
  METHOD MY_F4.
    PERFORM EVENT_MY_F4
            IN PROGRAM (SY-CPROG)
            TABLES LT_F4
            USING SENDER
                  ET_BAD_CELLS
                  ES_ROW_NO
                  ER_EVENT_DATA
                  E_DISPLAY
                  E_FIELDNAME.
  ENDMETHOD.                                                "MY_F4

ENDCLASS.                    "lcl_events_d0100 IMPLEMENTATION







*&**********************************************************************
*  CLASS DATA                                                          *
*&**********************************************************************
DATA : G_APPLICATION01 TYPE REF TO LCL_APPLICATION01.      " Tree

DATA : G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_ALV_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

       G_DOCKING_CONTAINER     TYPE REF TO CL_GUI_DOCKING_CONTAINER,
       G_DOCKING_CONTAINER_TOP TYPE REF TO CL_GUI_DOCKING_CONTAINER,

       G_SPLIT_CONTAINER  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,

       G_GUI_CONTAINER1   TYPE REF TO CL_GUI_CONTAINER ,
       G_GUI_CONTAINER2   TYPE REF TO CL_GUI_CONTAINER ,



       G_CONTAINER        TYPE SCRFNAME VALUE 'CONTAINER',
       G_GRID             TYPE REF TO CL_GUI_ALV_GRID,
       G_GRID1            TYPE REF TO CL_GUI_ALV_GRID,
       G_GRID2            TYPE REF TO CL_GUI_ALV_GRID,
       G_EVENTS           TYPE REF TO LCL_EVENTS,
       G_DOCUMENT    TYPE REF TO CL_DD_DOCUMENT,

       G_HIERARCHY_HEADER TYPE TREEV_HHDR,
       GT_HEADER
           TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE.


* TABLE TYPE.
DATA :  GT_FIELDCAT    TYPE LVC_T_FCAT,
        GT_FIELDCAT2    TYPE LVC_T_FCAT,
        GT_SORT        TYPE LVC_T_SORT,
        GT_STYL        TYPE LVC_T_STYL,
        GT_TABCOLOR    TYPE LVC_T_SCOL,
        GT_EXCL_FUNC   TYPE UI_FUNCTIONS.
DATA :  G_FIELD_CT      TYPE SLIS_T_FIELDCAT_ALV.
* STRUCTURE TYPE
DATA :  GS_LAYOUT   TYPE LVC_S_LAYO,
        GS_SORT     TYPE LVC_S_SORT,
        GS_STYL     TYPE LVC_S_STYL,
        GS_TABCOLOR TYPE LVC_S_SCOL,
        GS_TOOLBAR  TYPE STB_BUTTON,
        GS_CURR_COL       TYPE LVC_S_COL,
        GS_CURR_ROW       TYPE LVC_S_ROID.

DATA :  GS_O_LAYOUT TYPE DISVARIANT.      "for parameter IS_VARIANT


DATA : OK_CODE_C LIKE SY-UCOMM ,
       G_CHANGE_DATA,
       G_ERROR ,
       G_SAVE_C  ,
       G_INPUT TYPE I VALUE 0,
       G_ANSWER,
       G_TOT_CNT TYPE I,
       G_PERCENT TYPE P,
       G_MESSAGE(20)   .

DATA : G_REPID_C TYPE SY-REPID.

*_F4 ????
DATA : GS_F4        TYPE LVC_S_F4,
       GT_F4        TYPE LVC_T_F4.


* message popup.
*DATA : GT_MESSAGE_C TYPE BAPIRETTAB.
*DATA : GS_MESSAGE_C LIKE BAPIRET2.

* ????
DATA : G_ERDAT LIKE SY-DATUM,
       G_ERZET LIKE SY-UZEIT,
       G_ERNAM LIKE SY-UNAME.




*---------------------------------------------------------------
FIELD-SYMBOLS : <F4TAB> TYPE LVC_T_MODI.

*---------------------------------------------------------------
*------Possible Entry
DATA : LT_VALUES TYPE TABLE OF SEAHLPRES,
       LT_FIELDS TYPE TABLE OF DFIES,
       LS_VALUE  TYPE SEAHLPRES,
       LS_FIELD  TYPE DFIES,

       LS_F4     TYPE DDSHRETVAL,
       LS_MODI   TYPE LVC_S_MODI.

************************************************************************
* DEFINE...
************************************************************************
DEFINE D_TOOLBAR.
  CLEAR GS_TOOLBAR.
  GS_TOOLBAR-FUNCTION  = &1.
  GS_TOOLBAR-ICON      = &2.
  GS_TOOLBAR-BUTN_TYPE = &3.
  GS_TOOLBAR-DISABLED  = &4.
  GS_TOOLBAR-TEXT      = &5.
  GS_TOOLBAR-QUICKINFO = &6.
  GS_TOOLBAR-CHECKED   = &7.
  APPEND GS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
END-OF-DEFINITION.



*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
FORM SET_SORT_C  USING  P_SPOS
                      P_FIELD
                      P_UP
                      P_DOWN
                      P_GROUP
                      P_SUBTOT
                      P_COMP
                      P_EXPA.

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
  INSERT LS_SORT INTO TABLE GT_SORT.

ENDFORM.                    " SET_SORT
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_C .
*  INPUT.
*  GS_LAYOUT-EDIT = 'X' .
  GS_LAYOUT-STYLEFNAME = 'H_STYLE'.
*  GS_LAYOUT-ZEBRA      = 'X'.
* B:single C:multi D:cell A:rowcol
  GS_LAYOUT-SEL_MODE   = 'B'.
* ROW COLOR
  GS_LAYOUT-INFO_FNAME = 'COLOR'.
* CELL COLOR
  GS_LAYOUT-CTAB_FNAME = 'TABCOLOR'.
** BOX
*  GS_LAYOUT-BOX_FNAME  = 'MARK'.
* OPTIMAZE
  GS_LAYOUT-CWIDTH_OPT = 'X'.
* Title
  GS_LAYOUT-GRID_TITLE = TEXT-T01 .
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CALL_FIRST_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_FIRST_DISPLAY  TABLES   P_TABLE .
*---------------------------------------------------------------
  CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_O_LAYOUT "&see below
*      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            = P_TABLE[].

ENDFORM.                    " CALL_FIRST_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  CALL_FIRST_DISPLAY_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_FIRST_DISPLAY_1  TABLES   P_TABLE .
*---------------------------------------------------------------
  CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_O_LAYOUT "&see below
*      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            = P_TABLE[].

ENDFORM.                    " CALL_FIRST_DISPLAY_1

*&---------------------------------------------------------------------*
*&      Form  SET_STYLE
*&---------------------------------------------------------------------*
*       CELL EDIT.
*----------------------------------------------------------------------*
FORM SET_STYLE USING PT_STYL TYPE LVC_T_STYL
                     P_FIELD
                     P_MAXLEN.
  CLEAR : GS_STYL.
  GS_STYL-FIELDNAME  = P_FIELD.
  GS_STYL-STYLE      = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
  GS_STYL-MAXLEN     = P_MAXLEN.
  INSERT GS_STYL INTO TABLE PT_STYL .
*
ENDFORM.                    " SET_STYLE
*&---------------------------------------------------------------------*
*&      Form  SET_CEL_COLOR
*&---------------------------------------------------------------------*
*       CELL COLOR
*----------------------------------------------------------------------*
FORM SET_CEL_COLOR  USING    PT_TABLCOLOR TYPE LVC_T_SCOL
                             P_FIELD
                             P_COLOR.
*
  GS_TABCOLOR-FNAME     = P_FIELD.
  GS_TABCOLOR-COLOR-COL = P_COLOR+0(1).
  GS_TABCOLOR-COLOR-INT = P_COLOR+1(1).
  GS_TABCOLOR-COLOR-INV = P_COLOR+2(1).
  GS_TABCOLOR-NOKEYCOL  = 'X'.
  INSERT GS_TABCOLOR INTO TABLE GT_TABCOLOR.
ENDFORM.                    " SET_CEL_COLOR

*&---------------------------------------------------------------------*
*&      Form  SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM SET_GRID_TOOLBAR CHANGING CT_EXCL_FUNC
                                              TYPE UI_FUNCTIONS.
  DATA: LS_FUNC TYPE LVC_S_EXCL.

  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL TO CT_EXCL_FUNC.
*
*  LOOP AT GS_GRID_OPT-TOOLBAR-TOOLBAR_EXCL_FUNC INTO LS_FUNC.
*    APPEND LS_FUNC TO CT_EXCL_FUNC.
*  ENDLOOP.
ENDFORM.                    " SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM SET_GRID_TOOLBAR_CON1 CHANGING CT_EXCL_FUNC
                                              TYPE UI_FUNCTIONS.
  DATA: LS_FUNC TYPE LVC_S_EXCL.

  LOOP AT GT_EXCL_FUNC INTO LS_FUNC.
    APPEND LS_FUNC TO CT_EXCL_FUNC.
  ENDLOOP.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL TO CT_EXCL_FUNC.

ENDFORM.                    " SET_GRID_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISPLAY
*&---------------------------------------------------------------------*
FORM REFRESH_DISPLAY .
*  SCROLL ????? ??.
  CALL METHOD G_GRID->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_CURR_COL
      ES_ROW_NO   = GS_CURR_ROW.
* Layout
  CALL METHOD G_GRID->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = GS_LAYOUT.
*  REFRESH DISPLAY
  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
*REFRESH ? SCROLL ??? ??.
  CALL METHOD G_GRID->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = GS_CURR_COL
      IS_ROW_NO   = GS_CURR_ROW.

ENDFORM.

" REFRESH_DISPLAY


*---------------------------------------------------------------------*
*       FORM REFRESH_DISPLAY_200                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM REFRESH_DISPLAY_200 .
*  SCROLL ????? ??.
  CALL METHOD G_GRID1->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_CURR_COL
      ES_ROW_NO   = GS_CURR_ROW.
* Layout
  CALL METHOD G_GRID1->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = GS_LAYOUT.
*  REFRESH DISPLAY
  CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY.
*REFRESH ? SCROLL ??? ??.
  CALL METHOD G_GRID1->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = GS_CURR_COL
      IS_ROW_NO   = GS_CURR_ROW.

ENDFORM.                    " REFRESH_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_1
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM_1 USING P_DEFAULT
                            P_TITLE
                            P_TEXT1
                            P_TEXT2
                            P_DISPLAY
                      CHANGING P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            DEFAULTOPTION  = P_DEFAULT
            TEXTLINE1      = P_TEXT1
            TEXTLINE2      = P_TEXT2
            TITEL          = P_TITLE
            CANCEL_DISPLAY = P_DISPLAY
       IMPORTING
            ANSWER         = P_ANSWER
       EXCEPTIONS
            TEXT_NOT_FOUND.

ENDFORM.                    " POPUP_TO_CONFIRM_1


*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_CON
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_CON USING P_STYLE
                          P_EDIT
                          P_SEL
                          P_STYLEFNAME
                          P_COLOR
                          P_TABC
                          P_BOX
                          P_OPT.
*_Lights
  IF P_STYLE = 'LIGHT'.
    GS_LAYOUT-EXCP_FNAME = P_STYLE.
  ELSE.
    GS_LAYOUT-STYLEFNAME = P_STYLE.
  ENDIF.

*_INPUT.
  GS_LAYOUT-EDIT = P_EDIT .

*  GS_LAYOUT-ZEBRA      = 'X'.

  GS_LAYOUT-SEL_MODE   = P_SEL.

*_
  GS_LAYOUT-STYLEFNAME = P_STYLEFNAME.

*_ROW COLOR
  GS_LAYOUT-INFO_FNAME = P_COLOR.

*_CELL COLOR
  GS_LAYOUT-CTAB_FNAME = P_TABC .

*_BOX
  GS_LAYOUT-BOX_FNAME  = P_BOX.

*_OPTIMAZE
  GS_LAYOUT-CWIDTH_OPT = P_OPT.

*_Title
  GS_LAYOUT-GRID_TITLE = TEXT-T01 .

ENDFORM.                    " SET_LAYOUT_CON
*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_MET_CON
*&---------------------------------------------------------------------*
FORM SET_INPUT_MET_CON
     USING  P_GB
            P_IN.

  IF P_GB EQ 'X'.
*___ENTER EVENT
    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
  ELSE.
*___DATA MODIFY EVENT
    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  ENDIF.

*_INPUT EVENT
  CALL METHOD G_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = P_IN.

ENDFORM.                    " SET_INPUT_MET_CON
**&---------------------------------------------------------------------
*
**&      Form  SET_GRID_TOOLBAR_CON
**&---------------------------------------------------------------------
*
FORM SET_GRID_TOOLBAR_CON
     CHANGING CT_EXCL_FUNC TYPE UI_FUNCTIONS.


  PERFORM LOCAL_TOOL_CON IN PROGRAM (SY-CPROG)
          CHANGING CT_EXCL_FUNC.


ENDFORM.                    " SET_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  FREE_RTN
*&---------------------------------------------------------------------*
FORM FREE_RTN .
  CALL METHOD G_GRID->FREE.
  CALL METHOD G_CUSTOM_CONTAINER->FREE.
  CALL METHOD CL_GUI_CFW=>FLUSH.
  CLEAR : G_CUSTOM_CONTAINER.
ENDFORM.                    " FREE_RTN
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
FORM PROGRESS_BAR USING P_TOT_CNT .

  G_PERCENT = ( SY-TABIX / G_TOT_CNT ) * 100.
  MOVE : G_PERCENT TO G_MESSAGE+0(3).
  G_MESSAGE+3(17) = '% ?? ???...' .
*
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = G_PERCENT
            TEXT       = G_MESSAGE
       EXCEPTIONS
            OTHERS     = 1.
ENDFORM.                    " PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_CON
*&---------------------------------------------------------------------*
FORM SET_INPUT_CON USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID
                            P_GB
                            P_IN.

  IF P_GB EQ 'X'.
*___ENTER EVENT
    CALL METHOD P_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
  ELSE.
*___DATA MODIFY EVENT
    CALL METHOD P_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  ENDIF.

*_INPUT EVENT
  CALL METHOD P_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = P_IN.


ENDFORM.                    " SET_INPUT_CON
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY_1
*&---------------------------------------------------------------------*
FORM CALL_GRID_DISPLAY    TABLES   P_TABLE
                          USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID
                          .

  CALL METHOD P_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_O_LAYOUT
*      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            = P_TABLE[].

ENDFORM.                    " CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY_1
*&---------------------------------------------------------------------*
FORM CALL_GRID_DISPLAY_FCAT    TABLES   P_TABLE
                          USING    P_GRID TYPE REF TO CL_GUI_ALV_GRID
                                   P_FIELDCAT
                          .

  CALL METHOD P_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE               = 'A'
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_O_LAYOUT
*      IT_TOOLBAR_EXCLUDING = GT_EXCL_FUNC
    CHANGING
      IT_FIELDCATALOG      = P_FIELDCAT
      IT_SORT              = GT_SORT
      IT_OUTTAB            = P_TABLE[].

ENDFORM.                    " CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_MODIFY_CELL
*&---------------------------------------------------------------------*
FORM ALV_MODIFY_CELL_C  USING RR_DATA_CHANGED  TYPE REF TO
                                      CL_ALV_CHANGED_DATA_PROTOCOL
                            P_INDEX TYPE SY-TABIX P_FIELD P_VALUE.

  CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = P_INDEX
      I_FIELDNAME = P_FIELD
      I_VALUE     = P_VALUE.

ENDFORM.                    " ALV_MODIFY_CELL
*&---------------------------------------------------------------------*
*&      Form  MULTI_MSG_POPUP_CLASS
*&---------------------------------------------------------------------*
FORM MULTI_MSG_POPUP_CLASS  USING PT_MESSAGE ."TYPE BAPIRETTAB.
  CALL FUNCTION 'OXT_MESSAGE_TO_POPUP'
       EXPORTING
            IT_MESSAGE = PT_MESSAGE
       EXCEPTIONS
            BAL_ERROR  = 1
            OTHERS     = 2.
ENDFORM.                    " MULTI_MSG_POPUP_CLASS
*&---------------------------------------------------------------------*
*&      Form  MOVE_MSG_CLASS
*&---------------------------------------------------------------------*
FORM MOVE_MSG_CLASS  USING    P_TYPE
                              P_ID
                              P_NUM
                              P_V1
                              P_V2
                              P_V3
                              P_V4.

*  MOVE: P_TYPE TO GS_MESSAGE_C-TYPE ,
*        P_ID   TO GS_MESSAGE_C-ID ,
*        P_NUM  TO GS_MESSAGE_C-NUMBER ,
*        P_V1   TO GS_MESSAGE_C-MESSAGE_V1 ,
*        P_V2   TO GS_MESSAGE_C-MESSAGE_V2 ,
*        P_V3   TO GS_MESSAGE_C-MESSAGE_V3 ,
*        P_V4   TO GS_MESSAGE_C-MESSAGE_V4 .
*  INSERT GS_MESSAGE_C INTO TABLE GT_MESSAGE_C.

ENDFORM.                    " MOVE_MSG_CLASS
