*----------------------------------------------------------------------*
*   INCLUDE ZMMR10000C_C01                                             *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       CLASS cl_event_tree DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS CL_EVENT_TREE DEFINITION.
  PUBLIC SECTION.
    METHODS :  H_NMENU_REQ
                 FOR EVENT NODE_CONTEXT_MENU_REQUEST  OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           MENU
                           NODE_KEY
                           .
    METHODS :  H_IMENU_REQ
                 FOR EVENT ITEM_CONTEXT_MENU_REQUEST  OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           MENU
                           NODE_KEY
                           FIELDNAME
                           .
    METHODS :  H_NMENU_SEL
                 FOR EVENT NODE_CONTEXT_MENU_SELECTED  OF
                 CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           FCODE
                           NODE_KEY
                           .
    METHODS :  H_IMENU_SEL
                 FOR EVENT ITEM_CONTEXT_MENU_SELECTED OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           FCODE
                           NODE_KEY
                           FIELDNAME
                           .
    METHODS :  H_NDOUBLE_CLICK  " Node Double Click
                 FOR EVENT NODE_DOUBLE_CLICK  OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           NODE_KEY
                           .
    METHODS : H_IDOUBLE_CLICK  " Item Double Click
                 FOR EVENT ITEM_DOUBLE_CLICK  OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           NODE_KEY
                           FIELDNAME
                           .
    METHODS : H_LINK_CLICK
                 FOR EVENT LINK_CLICK  OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           NODE_KEY
                           FIELDNAME
                           .
    METHODS : H_CHECKBOX_CLICK
                 FOR EVENT CHECKBOX_CHANGE   OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           NODE_KEY
                           FIELDNAME
                           CHECKED
                           .
    METHODS : H_BUTTON_CLICK
                 FOR EVENT BUTTON_CLICK      OF CL_GUI_ALV_TREE
                 IMPORTING SENDER
                           NODE_KEY
                           FIELDNAME
                           .
    METHODS : H_FUNC_CLICK
                 FOR EVENT FUNCTION_SELECTED OF CL_GUI_TOOLBAR
                 IMPORTING SENDER
                           FCODE
                           .

    METHODS : H_DROP_CLICK
                 FOR EVENT DROPDOWN_CLICKED  OF CL_GUI_TOOLBAR
                 IMPORTING SENDER
                           FCODE
                           POSX
                           POSY
                           .

ENDCLASS.                    "CL_EVENT_TREE DEFINITION

*---------------------------------------------------------------------*
*       CLASS cl_event_tree IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS CL_EVENT_TREE IMPLEMENTATION.

  METHOD H_NMENU_REQ.
    PERFORM TREE_NMENU_REQ  IN PROGRAM (SY-REPID)  IF FOUND
                                  USING SENDER
                                        MENU
                                        NODE_KEY
                                       .
  ENDMETHOD.                    "H_NMENU_REQ

  METHOD H_IMENU_REQ.
    PERFORM TREE_IMENU_REQ  IN PROGRAM (SY-REPID)  IF FOUND
                                  USING SENDER
                                        MENU
                                        NODE_KEY
                                        FIELDNAME
                                       .
  ENDMETHOD.                    "H_IMENU_REQ

  METHOD H_NMENU_SEL.
    PERFORM TREE_NMENU_SEL  IN PROGRAM (SY-REPID)  IF FOUND
                                  USING SENDER
                                        FCODE
                                        NODE_KEY
                                       .
  ENDMETHOD.                    "H_NMENU_SEL

  METHOD H_IMENU_SEL.
    PERFORM TREE_IMENU_SEL  IN PROGRAM (SY-REPID)  IF FOUND
                                  USING SENDER
                                        FCODE
                                        NODE_KEY
                                        FIELDNAME
                                       .
  ENDMETHOD.                    "H_IMENU_SEL

  METHOD H_NDOUBLE_CLICK.
    PERFORM TREE_NDOUBLE_CLICK IN PROGRAM (SY-REPID)  IF FOUND
                                  USING SENDER
                                        NODE_KEY
                                       .
  ENDMETHOD.                    "H_NDOUBLE_CLICK

  METHOD H_IDOUBLE_CLICK.
    PERFORM TREE_IDOUBLE_CLICK IN PROGRAM (SY-REPID)  IF FOUND
                                  USING SENDER
                                        NODE_KEY
                                        FIELDNAME
                                       .
  ENDMETHOD.                    "H_IDOUBLE_CLICK

  METHOD H_LINK_CLICK.
    PERFORM TREE_LINK_CLICK IN PROGRAM (SY-REPID)  IF FOUND
                                  USING SENDER
                                        NODE_KEY
                                        FIELDNAME
                                       .
  ENDMETHOD.                    "H_LINK_CLICK

  METHOD H_CHECKBOX_CLICK.
    PERFORM TREE_CHECKBOX_CLICK IN PROGRAM (SY-REPID)  IF FOUND
                                   USING SENDER
                                         NODE_KEY
                                         FIELDNAME
                                         CHECKED
                                         .
  ENDMETHOD.                    "H_CHECKBOX_CLICK

  METHOD H_BUTTON_CLICK.
    PERFORM TREE_BUTTON_CLICK IN PROGRAM (SY-REPID)  IF FOUND
                                 USING SENDER
                                       NODE_KEY
                                       FIELDNAME
                                       .
  ENDMETHOD.                    "H_BUTTON_CLICK

  METHOD H_FUNC_CLICK.
    PERFORM TREE_FUNCTION_CLICK IN PROGRAM (SY-REPID)  IF FOUND
                                 USING SENDER
                                       FCODE
                                       .
  ENDMETHOD.                    "H_FUNC_CLICK
  METHOD H_DROP_CLICK.
    PERFORM TREE_DROPDOWN_CLICK IN PROGRAM (SY-REPID)  IF FOUND
                                 USING SENDER
                                       FCODE
                                       POSX
                                       POSY
                                       .
  ENDMETHOD.                    "H_DROP_CLICK
ENDCLASS.                    "CL_EVENT_TREE IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS cl_event_splt DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS CL_EVENT_SPLT DEFINITION.
  PUBLIC SECTION.
    METHODS : CONSTRUCTOR.
    METHODS : H_SPLITTER_REMOVE
                FOR EVENT MOVE_CONTROL OF  CL_GUI_SPLITTER_CONTAINER
                IMPORTING SENDER.

    METHODS : H_SPLITTER_RESIZE
                FOR EVENT SIZE_CONTROL OF  CL_GUI_SPLITTER_CONTAINER
                IMPORTING SENDER.
ENDCLASS.                    "CL_EVENT_SPLT DEFINITION

*---------------------------------------------------------------------*
*       CLASS cl_event_splt IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS CL_EVENT_SPLT IMPLEMENTATION.
  METHOD CONSTRUCTOR.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD H_SPLITTER_REMOVE.
    CHECK NOT SENDER IS INITIAL.
    PERFORM SPLITTER_RESIZE IN PROGRAM (SY-REPID)  IF FOUND
                                 USING SENDER
                                       .
  ENDMETHOD.                    "H_SPLITTER_REMOVE
  METHOD H_SPLITTER_RESIZE.
    CHECK NOT SENDER IS INITIAL.
    PERFORM SPLITTER_RESIZE IN PROGRAM (SY-REPID)  IF FOUND
                                 USING SENDER
                                       .
  ENDMETHOD.                    "H_SPLITTER_RESIZE
ENDCLASS.                    "CL_EVENT_SPLT IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS cl_event_grid DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS CL_EVENT_GRID DEFINITION.
  PUBLIC SECTION.
    METHODS :  H_TOP_OF_PAGE
                 FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
                 IMPORTING  SENDER
                            E_DYNDOC_ID
                            .
    METHODS : H_TOOLBAR
                  FOR EVENT TOOLBAR        OF CL_GUI_ALV_GRID
                  IMPORTING SENDER
                            E_OBJECT
                            E_INTERACTIVE.
    METHODS : h_user_command
                  FOR EVENT user_command   OF cl_gui_alv_grid
                  IMPORTING sender
                            e_ucomm.
*
**---
*    METHODS : h_user_ok_code
*                  FOR EVENT user_event     OF cl_event_user
*                  IMPORTING sender
*                            new_ok_code.

ENDCLASS.                    "CL_EVENT_GRID DEFINITION
*-----------------------------------------------------------------------
CLASS CL_EVENT_GRID IMPLEMENTATION.

  METHOD H_TOP_OF_PAGE.
    PERFORM GRID_TOP_OF_PAGE IN PROGRAM (SY-REPID)  IF FOUND
                                 USING SENDER
                                       E_DYNDOC_ID
                                       .
  ENDMETHOD.                    "handle_button_click
  METHOD H_TOOLBAR.
    PERFORM GRID_APPEND_TOOLBAR IN PROGRAM (SY-REPID)  IF FOUND
                                 USING SENDER
                                       E_OBJECT
                                       E_INTERACTIVE
                                       .
  ENDMETHOD.                    "H_TOOLBAR
*
  METHOD h_user_command.
    PERFORM grid_user_command  IN PROGRAM (sy-repid)  IF FOUND
                                 USING sender
                                       e_ucomm
                                       .
  ENDMETHOD.                    "H_USER_COMMAND
*
**----
**--
*  METHOD h_user_ok_code.
*    PERFORM grid_user_ok_code IN PROGRAM (sy-repid)  IF FOUND
*                                 USING sender
*                                       new_ok_code
*                                       .
*  ENDMETHOD.                    "H_USER_OK_CODE
ENDCLASS.                    "CL_EVENT_GRID IMPLEMENTATION

********************************************************************
**ALV TREE
TYPES     : BEGIN          OF GY_ALVGRID      " ALV GRID
          , GRID         TYPE REF TO CL_GUI_ALV_GRID
          , CTRL         TYPE REF TO CL_GUI_CONTROL
          , FCAT         TYPE LVC_T_FCAT
          , LAYO         TYPE LVC_S_LAYO
          , SORT         TYPE LVC_T_SORT
          , EXCF         TYPE UI_FUNCTIONS
          , VARI         TYPE DISVARIANT
          , DROP         TYPE LVC_T_DROP
*          , dral         TYPE lvc_t_dral
          , END            OF GY_ALVGRID
          .

TYPES     : BEGIN              OF GY_MGRID.  " Message Grid
INCLUDE     STRUCTURE ZMMS0105.
TYPES     : STATE            TYPE SY-UCOMM
          , STTAB            TYPE LVC_T_STYL
          , COTAB            TYPE LVC_T_SCOL
          , END                OF GY_MGRID.

TYPES     : BEGIN              OF GY_IGRID.  " Inbound Grid
INCLUDE     STRUCTURE ZMMS0104.
TYPES     : CHECK            TYPE C
          , STATE            TYPE SY-UCOMM
          , STTAB            TYPE LVC_T_STYL
          , COTAB            TYPE LVC_T_SCOL
          , END                OF GY_IGRID.

TYPES : BEGIN OF GY_ALVTREE,
         TREE TYPE REF TO CL_GUI_ALV_TREE,
         CTRL TYPE REF TO CL_GUI_CONTROL,
         TBAR TYPE REF TO CL_GUI_TOOLBAR,
         FCAT TYPE LVC_T_FCAT,
         LAYO TYPE LVC_T_LAYI,
         LAYN TYPE LVC_S_LAYN,
         VARI TYPE DISVARIANT,
        END OF GY_ALVTREE.

TYPES : BEGIN OF GY_GTREE,   " Tree Group
         MODLE TYPE ZE_MODLE,
         MNODE TYPE LVC_NKEY,
        END OF GY_GTREE.

TYPES : BEGIN OF GY_ITREE.
        INCLUDE STRUCTURE ZMMS0102.
TYPES : CHECK TYPE C,
        CNODE TYPE LVC_NKEY,
        LEVEL TYPE CHAR10,
        FCODE TYPE SY-UCOMM,
        END OF GY_ITREE.

TYPES  : TE_SLINE     TYPE EDPLINE,
         BEGIN        OF   T_EDIT,
         TEXT         TYPE TE_SLINE,
         END          OF   T_EDIT.

DATA : BEGIN OF GT_USRPG OCCURS 0,
         TEXT         TYPE TE_SLINE,
       END OF GT_USRPG.

DATA : BEGIN OF GT_ABAPG OCCURS 0,
         TEXT         TYPE TE_SLINE,
       END OF GT_ABAPG.

DATA : SV_CODE LIKE SY-UCOMM.

DATA : GR_INB_TREE   TYPE REF TO CL_GUI_ALV_TREE,"GY_ALVTREE,
       GR_TRE_DOCK   TYPE REF TO CL_GUI_DOCKING_CONTAINER,
       GT_IMGRD      TYPE GY_MGRID   OCCURS 0 WITH HEADER LINE,
       GT_ITREE      TYPE GY_ITREE OCCURS 0 WITH HEADER LINE,
       G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_CUSTOM_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GR_TR1_CONT   TYPE REF TO CL_GUI_CONTAINER,
       GR_TR2_CONT   TYPE REF TO CL_GUI_CONTAINER,
       GR_TREE_CLASS TYPE REF TO CL_EVENT_TREE,
       GR_GR1_CUST   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       GR_GR1_SPLT   TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
       GR_G11_CONT   TYPE REF TO CL_GUI_CONTAINER,
       GR_SPLT_CLASS TYPE REF TO CL_EVENT_SPLT,
       GR_INB_GRID   TYPE GY_ALVGRID,
       GV_BTN_STAT   TYPE LVC_T_STYL,
       GT_IGRID      TYPE GY_IGRID    OCCURS 0 WITH HEADER LINE,
       GR_GRID_CLASS TYPE REF TO CL_EVENT_GRID,
       GT_INB_GROP   TYPE LVC_T_NKEY,
       GT_ITGRP      TYPE GY_GTREE    OCCURS 0 WITH HEADER LINE,
       GR_INF_CLAS   TYPE REF TO ZMMC_CL_IF ,  " Interface Class,
       GV_INB_NODE   TYPE LVC_NKEY  ,          " inbound Header Key
       GV_TOP_NODE   TYPE LVC_NKEY,
       GT_FIELDCAT_LVC TYPE LVC_T_FCAT WITH HEADER LINE,
       GR_OTB_EDIT   TYPE REF TO CL_GUI_TEXTEDIT,
       GV_ACTIV         TYPE SY-UCOMM









       .
