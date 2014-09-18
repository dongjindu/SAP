*&---------------------------------------------------------------------*
*& Include ZRZIMGMF01                                                  *
*&---------------------------------------------------------------------*
*&  ÇÁ·Î±×·¥¸í : ¼öÀÔ½Ã½ºÅÛ IMG ÅëÇÕ °ü¸® Sub Module Inculde           *
*&      ÀÛ¼ºÀÚ : °­¼®ºÀ INFOLINK Ltd.                                  *
*&      ÀÛ¼ºÀÏ : 2000.01.25                                            *
*&  Àû¿ëÈ¸»çPJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS c_rmouse_click IMPLEMENTATION.
  METHOD handle_node_rmouse_click.
    DATA: fcodes TYPE ui_functions.
    data: code type ui_func.
    REFRESH fcodes.
    clear code.
    code = 'DMCREAEQ'.
    APPEND code TO fcodes.
    code = 'DMCREALT'.
    APPEND code TO fcodes.
    code = 'DMUPDA'.
    APPEND code TO fcodes.
    code = 'DMDELE'.


    APPEND code TO fcodes.
    CALL METHOD p_menu->load_gui_status
              EXPORTING program = 'SAPMZIMGM'
                          status  = 'TREECONT'
                          menu    = menu.
    IF sy-pfkey = 'DMSHOWC'.
      CALL METHOD menu->disable_functions
          EXPORTING fcodes = fcodes.
    ELSE.
      CALL METHOD menu->enable_functions
          EXPORTING fcodes = fcodes.
    ENDIF.
    call method cl_gui_cfw=>flush.
  ENDMETHOD.
  METHOD handle_item_rmouse_click.
    DATA: fcodes TYPE ui_functions.
    data: code type ui_func.
    REFRESH fcodes.
    code = 'DMCREAEQ'.
    APPEND code TO fcodes.
    code = 'DMCREALT'.
    APPEND code TO fcodes.
    code = 'DMUPDA'.
    APPEND code TO fcodes.
    code = 'DMDELE'.
    APPEND code TO fcodes.
    CALL METHOD p_menu->load_gui_status
              EXPORTING program = 'SAPMZIMGM'
                          status  = 'TREECONT'
                          menu    = menu.
    IF sy-pfkey = 'DMSHOWC'.
      CALL METHOD menu->disable_functions
          EXPORTING fcodes = fcodes.
    ELSE.
      CALL METHOD menu->enable_functions
          EXPORTING fcodes = fcodes.
    ENDIF.
    call method cl_gui_cfw=>flush.
  ENDMETHOD.
  METHOD handle_node_select.
    READ TABLE nodetab WITH KEY id = node_key INTO gnode.
    CASE fcode.
      WHEN 'DMCREAEQ'.
        grelship = stree_reltype_next.
*       CALL SCREEN 150 STARTING AT -5 5.
      WHEN 'DMCREALT'.
        grelship = stree_reltype_child.
*       CALL SCREEN 150 STARTING AT -5 5.
      WHEN 'DMUPDA'.
      WHEN 'DMDELE'.
    ENDCASE.
  ENDMETHOD.
  METHOD handle_item_select.
    CASE fcode.
      WHEN 'DMCREAEQ'.
        grelship = stree_reltype_next.
*       CALL SCREEN 150 STARTING AT -5 5.
      WHEN 'DMCREALT'.
        grelship = stree_reltype_child.
*       CALL SCREEN 150 STARTING AT -5 5.
      WHEN 'DMUPDA'.
      WHEN 'DMDELE'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

class c_event_handler implementation.

Method ignore_cntl_error.

endmethod.

method handle_node_double_click.
  data: nodekey type tv_nodekey,
        node type snodetext.
  call method treev->get_selected_node importing
       node_key = nodekey.
  call method cl_gui_cfw=>flush.
  READ TABLE NODETAB WITH KEY ID = NODEKEY INTO NODE.
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  G_EVENT = 'NODE_DOUBLE_CLICK'.
  G_NODE_KEY = NODEKEY.

*  IF NODE-TYPE IS INITIAL.
*  ELSE.
*    CALL FUNCTION 'RS_TOOL_ACCESS'
*         EXPORTING
*              OPERATION           = 'TEST'
*              OBJECT_NAME         = NODE-TEXT1
*              OBJECT_TYPE         = NODE-TYPE
**              ENCLOSING_OBJECT    =
**              POSITION            = ' '
**              DEVCLASS            =
**              INCLUDE             =
**              MONITOR_ACTIVATION  = 'X'
**         IMPORTING
**              NEW_NAME            =
**         TABLES
**              OBJLIST             =
*         EXCEPTIONS
*              NOT_EXECUTED        = 1
*              INVALID_OBJECT_TYPE = 2
*              OTHERS              = 3.
*
*  ENDIF.
endmethod.

method handle_item_double_click.
data: nodekey type tv_nodekey,
      itemname type tv_itmname,
      node type snodetext.

call method treev->get_selected_item importing
     node_key = Nodekey
     item_name = itemname.
call method cl_gui_cfw=>flush.
READ TABLE NODETAB WITH KEY ID = NODEKEY INTO NODE.

  G_EVENT = 'ITEM_DOUBLE_CLICK'.
  G_NODE_KEY = NODEKEY.

IF  NODE-TYPE IS INITIAL.
ELSE.
  IF ITEMNAME = 'REPTITLE'.
     CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
               OPERATION           = 'TEST'
              OBJECT_NAME         = NODE-TEXT1
              OBJECT_TYPE         = NODE-TYPE
*              ENCLOSING_OBJECT    =
*              POSITION            = ' '
*              DEVCLASS            =
*              INCLUDE             =
*              MONITOR_ACTIVATION  = 'X'
*         IMPORTING
*              NEW_NAME            =
*         TABLES
*              OBJLIST             =
         EXCEPTIONS
              NOT_EXECUTED        = 1
              INVALID_OBJECT_TYPE = 2
              OTHERS              = 3
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
ELSE.
  IF NODE-TYPE EQ 'TRAN'.
    SELECT PGMNA INTO (NODE-TEXT1)
    FROM TSTC WHERE TCODE = NODE-TEXT1.
    ENDSELECT.
  ENDIF.
READ REPORT NODE-TEXT1 INTO SRC.
CALL METHOD EDITOR->DELETE_TEXT.
CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE EXPORTING TABLE = SRC[].
CALL METHOD cl_gui_cfw=>FLUSH.
ENDIF.
ENDIF.
endmethod.

endclass.

CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD flavor_select.
    IF line > 5.
      SEARCH flavors FOR 'Tree_move_to_Edit'.
      IF sy-subrc = 0.
        CALL METHOD dragDROP_OBJECT->SET_FLAVOR
             EXPORTING newflavor = 'Tree_move_to_Edit'.
      ELSE.
        CALL METHOD dragdrop_object->abort.
      ENDIF.
    ELSE.
      SEARCH flavors FOR 'Tree_copy_to_Edit'.
      IF sy-subrc = 0.
        CALL METHOD dragdrop_object->set_flavor
             EXPORTING newflavor = 'Tree_copy_to_Edit'.
      ELSE.
        CALL METHOD dragdrop_object->abort.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD left_drag.
  data: node type snodetext.
    DATA drag_object TYPE REF TO lcl_drag_object.
      READ TABLE nodetab WITH KEY id = node_key INTO node.
      IF node-type EQ 'TRAN'.
          SELECT pgmna INTO (node-text1)
          FROM tstc WHERE tcode = node-text1.
          ENDSELECT.
        ENDIF.

    CREATE OBJECT drag_object.
    drag_object->text = node-text1.
    drag_drop_object->object = drag_object.
ENDMETHOD.

  METHOD right_drop.
    DATA textline(256).
    DATA text_table LIKE STANDARD TABLE OF textline.
    DATA drag_object TYPE REF TO lcl_drag_object.
    CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.
      drag_object ?= dragdrop_object->object.
    ENDCATCH.
    IF sy-subrc = 1.
      " data object has unexpected class
                                       " => cancel Drag & Drop operation
      CALL METHOD dragdrop_object->abort.
      EXIT.
    ENDIF.

    CALL METHOD editor->get_text_as_stream
         IMPORTING text        = SRC[].
    CALL METHOD cl_gui_cfw=>flush.

*    SELECT * INTO  CORRESPONDING FIELDS OF TABLE SRC
*             FROM  ZTIMGTXT
*             WHERE ZFCD = node_key.
*        READ REPORT drag_object->text INTO src.
*        CALL METHOD editor->delete_text.
*       CALL METHOD editor->set_text_as_r3table EXPORTING table = src[].
*       CALL METHOD cl_gui_cfw=>flush.


  ENDMETHOD.
  METHOD drop_complete.
*    IF drag_drop_object->flavor = 'Tree_move_to_Edit'.
*      CALL METHOD tree->delete_node
*           EXPORTING node_key = node_key.
*      delete node_itab where node_key = node_key.
*    ENDIF.
    ENDMETHOD.
ENDCLASS.


*CALLBACK NODE_DBL_CLK.
*DATA: NODEKEY LIKE TREEV_NODE-NODE_KEY,
*      NODE TYPE SNODETEXT.
*
*CALL FUNCTION 'TREEV_GET_SELECTED_NODE'
*     EXPORTING
*          HANDLE                     = H_TREE
**         NO_FLUSH                   =
*    IMPORTING
*         NODE_KEY                   = NODEKEY
**    EXCEPTIONS
**         FAILED                     = 1
**         INSTANCE_NOT_FOUND         = 2
**         SINGLE_NODE_SELECTION_ONLY = 3
**         CNTL_SYSTEM_ERROR          = 4
**         OTHERS                     = 5
*          .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*READ TABLE NODETAB WITH KEY ID = NODEKEY INTO NODE.
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*IF NODE-TYPE IS INITIAL.
*ELSE.
*CALL FUNCTION 'RS_TOOL_ACCESS'
*     EXPORTING
*          OPERATION           = 'TEST'
*         OBJECT_NAME         = NODE-TEXT1
*         OBJECT_TYPE         = NODE-TYPE
**         ENCLOSING_OBJECT    =
**         POSITION            = ' '
**         DEVCLASS            =
**         INCLUDE             =
**         MONITOR_ACTIVATION  = 'X'
**    IMPORTING
**         NEW_NAME            =
**    TABLES
**         OBJLIST             =
*    EXCEPTIONS
*         NOT_EXECUTED        = 1
*         INVALID_OBJECT_TYPE = 2
*         OTHERS              = 3
*          .
*ENDIF.
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*ENDCALLBACK.

CALLBACK ITEM_DBL_CLK.
DATA: NODEKEY LIKE  TREEV_ITEM-NODE_KEY,
      ITEMNAME LIKE  TREEV_ITEM-ITEM_NAME,
      NODE TYPE SNODETEXT.
CALL FUNCTION 'TREEV_GET_SELECTED_ITEM'
     EXPORTING
          HANDLE                      = H_TREE
*         NO_FLUSH                    =
     IMPORTING
         NODE_KEY                    = NODEKEY
         ITEM_NAME                   = ITEMNAME
*    EXCEPTIONS
*         FAILED                      = 1
*         INSTANCE_NOT_FOUND          = 2
*         NOT_ALLOWED_FOR_SIMPLE_TREE = 3
*         CNTL_SYSTEM_ERROR           = 4
*         NO_ITEM_SELECTION           = 5
*         OTHERS                      = 6
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
READ TABLE NODETAB WITH KEY ID = NODEKEY INTO NODE.
IF  NODE-TYPE IS INITIAL.
ELSE.
  IF ITEMNAME = 'REPTITLE'.
     CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
               OPERATION           = 'TEST'
              OBJECT_NAME         = NODE-TEXT1
              OBJECT_TYPE         = NODE-TYPE
*              ENCLOSING_OBJECT    =
*              POSITION            = ' '
*              DEVCLASS            =
*              INCLUDE             =
*              MONITOR_ACTIVATION  = 'X'
*         IMPORTING
*              NEW_NAME            =
*         TABLES
*              OBJLIST             =
         EXCEPTIONS
              NOT_EXECUTED        = 1
              INVALID_OBJECT_TYPE = 2
              OTHERS              = 3
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
ELSE.
  IF NODE-TYPE EQ 'TRAN'.
    SELECT PGMNA INTO (NODE-TEXT1)
    FROM TSTC WHERE TCODE = NODE-TEXT1.
    ENDSELECT.
  ENDIF.
READ REPORT NODE-TEXT1 INTO SRC.
CALL METHOD EDITOR->DELETE_TEXT.
CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE EXPORTING TABLE = SRC[].
CALL METHOD cl_gui_cfw=>FLUSH.
ENDIF.
ENDIF.
ENDCALLBACK.

FORM USER_COMMAND  TABLES NODE STRUCTURE SEUCOMM
                   USING  COMMAND
                   CHANGING VALUE(EXIT)
                            VALUE(LIST_REFRESH).

  DATA: BEGIN OF NODELIST OCCURS 100.
          INCLUDE STRUCTURE STREENODE.
  DATA: END OF NODELIST.

  DATA: ANSWER.

  LOOP AT NODE.
    CASE COMMAND.
      WHEN 'REME'.
        CALL FUNCTION 'RS_TOOL_ACCESS'
             EXPORTING
                  OPERATION           = 'DOCS'
                  OBJECT_NAME         = 'SAPMZIMGM'
                  OBJECT_TYPE         = 'PROG'
             EXCEPTIONS
                  NOT_EXECUTED        = 1
                  INVALID_OBJECT_TYPE = 2
                  OTHERS              = 3.
        EXIT.
      WHEN 'INFO'.
        CALL FUNCTION 'RS_TOOL_ACCESS'
             EXPORTING
                  OPERATION           = 'DOCS'
                  OBJECT_NAME         = NODE-TEXT1
                  OBJECT_TYPE         = NODE-TYPE
             EXCEPTIONS
                  NOT_EXECUTED        = 1
                  INVALID_OBJECT_TYPE = 2
                  OTHERS              = 3.
      WHEN 'SRC'.
        if node-type is initial or node-text1 is initial.
        else.
        CALL FUNCTION 'RS_TOOL_ACCESS'
             EXPORTING
                  OPERATION           = 'SHOW'
                  OBJECT_NAME         = NODE-TEXT1
                  OBJECT_TYPE         = NODE-TYPE
             EXCEPTIONS
                  NOT_EXECUTED        = 1
                  INVALID_OBJECT_TYPE = 2
                  OTHERS              = 3.
        endif.
      WHEN 'TRPI'.
        IF NODE-SELFIELD = 'TEXT1' OR NODE-SELFIELD = 'TEXT'.
          IF NODE-TEXT1 = SPACE.
            MESSAGE E174.
*           Der Demo ist noch kein Report zugeordnet
          ELSE.
            IF NODE-TEXT1 = 'SAPMSDM1'.
              MESSAGE I169.
            ELSE.
              CALL FUNCTION 'RS_TOOL_ACCESS'
                   EXPORTING
                        OPERATION           = 'TEST'
                        OBJECT_NAME         = NODE-TEXT1
                        OBJECT_TYPE         = NODE-TYPE
                   EXCEPTIONS
                        NOT_EXECUTED        = 1
                        INVALID_OBJECT_TYPE = 2
                        OTHERS              = 3.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'TRAD'.
        PERFORM ADD_NODE USING NODE.
        SAVE_NECESSARY = TRUE.
        NODE-NEWNAME = NODE-TEXT.
        NODE-NEWTYPE = NODE-TYPE.
      WHEN 'TRDL'.
        SAVE_NECESSARY = TRUE.
      WHEN 'TRMV'.
        SAVE_NECESSARY = TRUE.
      WHEN 'TRRN'.
        SAVE_NECESSARY = TRUE.
        NODE-NEWNAME = NODE-TEXT.
      WHEN 'TRRT'.
        PERFORM EXIT USING EXIT.
      WHEN 'TREX'.
        PERFORM EXIT USING EXIT.
      WHEN 'ENJOY'.
        PERFORM EXIT USING EXIT.
      WHEN 'SAVE'.
        PERFORM SAVE_CHANGES.
        SET PF-STATUS 'DMSHOW'.
        SET TITLEBAR 'DMS'.
*     WHEN 'SWIT'.
*        IF SY-PFKEY = 'DMSHOW'.
**         Berechtigungsprüfung
*          PERFORM AUTHORITY_CHECK USING ACT_EDIT.
**         Sperren
*          PERFORM ENQUEUE_DEMO.
**         Hierarchie neu lesen
*          PERFORM GET_TREE_DATA USING SPACE.
**         SET PF-STATUS 'DMEDIT'.
**         SET TITLEBAR 'DMU'.
*        ELSE.
*          IF SAVE_NECESSARY = TRUE.
*            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*                 EXPORTING
*                      TEXTLINE1 = TEXT-005
*                      TEXTLINE2 = TEXT-010
*                      TITEL     = 'Demo Center'(020)
*                 IMPORTING
*                      ANSWER    = ANSWER.
*            CASE ANSWER.
*              WHEN 'J'.
*                PERFORM SAVE_CHANGES.
*              WHEN 'A'.
*                CONTINUE.
*              WHEN 'N'.
**               Hierarchie neu lesen
*                PERFORM GET_TREE_DATA USING SPACE.
*            ENDCASE.
*          ENDIF.
*          PERFORM DEQUEUE_DEMO.
*          SET PF-STATUS 'DMSHOW'.
*          SET TITLEBAR 'DMS'.
*        ENDIF.
*      WHEN 'REPO'.
*        PERFORM AUTHORITY_CHECK USING ACT_EDIT.
*        MOVE-CORRESPONDING NODE TO SEUCOMM.
*        CLEAR DEMOTREE.
*        COL_BEGIN = 2.
*        COL_END  = COL_BEGIN + TLENGTH + 22.
*        CALL SCREEN 100 STARTING AT COL_BEGIN  2
*                        ENDING   AT COL_END    5.
*      WHEN 'HOME'.
*        IF SAVE_NECESSARY = TRUE.
*          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*               EXPORTING
*                    TEXTLINE1 = TEXT-005
*                    TEXTLINE2 = TEXT-010
*                    TITEL     = TEXT-020
*               IMPORTING
*                    ANSWER    = ANSWER.
*          CASE ANSWER.
*            WHEN 'J'.
*              PERFORM SAVE_CHANGES.
*            WHEN 'A'.
*              CONTINUE.
*            WHEN 'N'.
*              CONTINUE.
*          ENDCASE.
*        ENDIF.
*        CALL FUNCTION 'RS_TREE_GET_CURRENT_LAYOUT'
*             IMPORTING
*                  FIRST_NODE = FIRST_NODE
*             TABLES
*                  LAYOUT     = LAYOUT.
*
*        EXPORT LAYOUT FIRST_NODE TO DATABASE DWTREE(DM) ID LAYOUT_ID.
*        MESSAGE S194.
**       Die Darstellung wurde gesichert.
      WHEN 'WTRP'.
        PERFORM TRANSPORT_DEMO.
    ENDCASE.
    MODIFY NODE.
  ENDLOOP.


ENDFORM.                               " USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  SAVE_CHANGES
*&---------------------------------------------------------------------*
FORM SAVE_CHANGES.


  SAVE_NECESSARY = FALSE.

* Die Hierarchie wird zurückgeschrieben
  CALL FUNCTION 'RS_TREE_GET_CURRENT_TREE'
       TABLES
            NODELIST = TREE.
  DESCRIBE TABLE TREE LINES SY-TABIX.
  IF SY-TABIX > 0.
    EXPORT TREE TO DATABASE DWTREE(DM) ID DEMO_ID.
  ELSE.
    DELETE FROM DATABASE DWTREE(DM) ID DEMO_ID.
    DELETE FROM DATABASE DWTREE(DM) ID LAYOUT_ID.
  ENDIF.

* Änderungsprotokoll
  CALL FUNCTION 'RS_TREE_GET_MODIFICATION_LOG'
       TABLES
            ADD_NODES = ADD_NODES
            DEL_NODES = DEL_NODES
            MOD_NODES = MOD_NODES.

* Löschen Knoten
  LOOP AT DEL_NODES.
    DELETE FROM DEMOTREE  WHERE ID = DEL_NODES-ID.
    DELETE FROM DEMOTREET WHERE ID = DEL_NODES-ID.
  ENDLOOP.

* Umbenennen Knoten
  LOOP AT MOD_NODES.
    CALL FUNCTION 'RS_TREE_GET_NODE'
         EXPORTING
              NODE_ID      = MOD_NODES-ID
         IMPORTING
              NODE_INFO    = SNODETEXT
         EXCEPTIONS
              ID_NOT_FOUND = 01.
    CHECK SY-SUBRC = 0.
    UPDATE DEMOTREET SET STEXT = SNODETEXT-TEXT
                    WHERE SPRAS = SY-LANGU
                    AND   ID    = MOD_NODES-ID.
  ENDLOOP.

* Neu angelegte Knoten
  LOOP AT ADD_NODES.
    CLEAR: DEMOTREE, DEMOTREET.
    CALL FUNCTION 'RS_TREE_GET_NODE'
         EXPORTING
              NODE_ID      = ADD_NODES-ID
         IMPORTING
              NODE_INFO    = SNODETEXT
         EXCEPTIONS
              ID_NOT_FOUND = 01.
    DEMOTREE-ID = ADD_NODES-ID.
    DEMOTREE-REPNAME = SNODETEXT-TEXT1.
    MODIFY DEMOTREE.
    MOVE-CORRESPONDING DEMOTREE TO DEMOTREET.
    DEMOTREET-SPRAS = SY-LANGU.
    DEMOTREET-STEXT = SNODETEXT-TEXT.
    MODIFY DEMOTREET.
  ENDLOOP.

  PERFORM DEQUEUE_DEMO.

ENDFORM.
                                       " SAVE_CHANGES
*&---------------------------------------------------------------------*
*&      Form  TEXT_DISPLAY
*&---------------------------------------------------------------------*
FORM TEXT_DISPLAY TABLES NODES STRUCTURE SNODETEXT.

  LOOP AT NODES.
    NODES-NLENGTH  = 0.
    NODES-TLENGTH  = TLENGTH.
    NODES-TLENGTH1 = 40.
    NODES-TCOLOR   = 4.
    MODIFY NODES.
  ENDLOOP.

ENDFORM.                               " TEXT_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  EXIT
*&---------------------------------------------------------------------*
FORM EXIT USING EXIT.

  DATA: ANSWER.

  EXIT = 'X'.
* IF SAVE_NECESSARY = TRUE.
*   CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*             TEXTLINE1 = TEXT-005
*             TEXTLINE2 = TEXT-010
*             TITEL     = 'Beenden Demo Center'(025)
*        IMPORTING
*             ANSWER    = ANSWER.
*   CASE ANSWER.
*     WHEN 'J'.
*       PERFORM SAVE_CHANGES.
*     WHEN 'A'.
*       CLEAR EXIT.
*       EXIT.
*   ENDCASE.
* ENDIF.
  PERFORM DEQUEUE_DEMO.

ENDFORM.                               " EXIT

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_DEMO
*&---------------------------------------------------------------------*
FORM ENQUEUE_DEMO.

  CALL FUNCTION 'ENQUEUE_EDEMOTREE'
       EXCEPTIONS
            FOREIGN_LOCK = 01
            OTHERS       = 02.
  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE E171 WITH SY-MSGV1.
*     Das Demo Center ist im Moment gesperrt
    WHEN 2.
      MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDCASE.

ENDFORM.                               " ENQUEUE_APPL

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_APPL
*&---------------------------------------------------------------------*
FORM DEQUEUE_DEMO.

  CALL FUNCTION 'DEQUEUE_EDEMOTREE'.

ENDFORM.                               " ENQUEUE_APPL
*&---------------------------------------------------------------------*
*&      Form  GET_APPLTREE
*&---------------------------------------------------------------------*
FORM GET_TREE_DATA USING STANDARD_LAYOUT.

DATA: TMP_ID(3)   TYPE  N.

  CUCOL = 3.
  CULIN = 2.
  LICOL = 1.
  LILIN = 1.

*  IMPORT TREE FROM DATABASE DWTREE(DM) ID DEMO_ID.

*  DESCRIBE TABLE TREE LINES SY-TABIX.
*  IF SY-SUBRC NE 0 OR SY-TABIX = 0.
*    CALL SCREEN 500 STARTING AT  5  5
*                    ENDING   AT 55  11.
*    IF TREE-ID IS INITIAL.
*      EXIT.
*    ELSE.
*      SAVE_NECESSARY = TRUE.
*    ENDIF.
*  ELSE.
*    IF STANDARD_LAYOUT = 'X'.
*      IMPORT LAYOUT FIRST_NODE FROM DATABASE DWTREE(DM) ID LAYOUT_ID.
*    ELSE.
*      CALL FUNCTION 'RS_TREE_GET_CURRENT_LAYOUT'
*           IMPORTING
*                CURSOR_COLUMN = CUCOL
*                CURSOR_LINE   = CULIN
*                FIRST_NODE    = FIRST_NODE
*                LIST_COLUMN   = LICOL
*                LIST_LINE     = LILIN
*           TABLES
*                LAYOUT        = LAYOUT.
*    ENDIF.
  DATA : BEGIN OF IT_IMG08 OCCURS 0.
         INCLUDE STRUCTURE ZTIMIMG08.
  DATA : END   OF IT_IMG08.


   REFRESH: NODETAB, TREE.

   SELECT * INTO TABLE IT_IMG08
            FROM  ZTIMIMG08
            WHERE ZFCDTY EQ '000'
*            AND   ZFCD1  IN ('X', 'x')
            AND   ZFCD1  EQ 'X'.
*            ORDER BY ZFCD.
   SORT IT_IMG08 BY ZFCD.

   LOOP AT IT_IMG08.
      CLEAR : NODETAB.
      MOVE : IT_IMG08-ZFCD     TO  NODETAB-ID+3(3),
             IT_IMG08-ZFCDNM   TO  NODETAB-TEXT,
             IT_IMG08-ZFCDNM   TO  NODETAB-NAME,
             IT_IMG08-ZFCD2    TO  NODETAB-TLEVEL.
*------------------------------------------------------------------
*> ÇÏÀ§ ·¹º§ Á¤ÀÇ..
*------------------------------------------------------------------
*      IF NOT IT_IMG08-ZFCD4 IS INITIAL.
*         NODETAB-CHILD+3(3) = IT_IMG08-ZFCD4.
*      ENDIF.
      IF NOT IT_IMG08-ZFCD3 IS INITIAL.
         NODETAB-CHILD+3(3) = '000'.
      ELSE.
         TMP_ID = IT_IMG08-ZFCD.
         CASE IT_IMG08-ZFCD2.
            WHEN '01'.
               TMP_ID = TMP_ID + 100.
               NODETAB-CHILD+3(3) = TMP_ID.
            WHEN '02'.
               TMP_ID = TMP_ID + 10.
               NODETAB-CHILD+3(3) = TMP_ID.
         ENDCASE.
      ENDIF.

*      IF NOT IT_IMG08-ZFCD5 IS INITIAL.
*         NODETAB-NEXT+3(3) = IT_IMG08-ZFCD5.
*      ENDIF.
*------------------------------------------------------------------
*. ´ÙÀ½ ·¹º§ Á¤ÀÇ.
*------------------------------------------------------------------
      TMP_ID = IT_IMG08-ZFCD.
      CASE IT_IMG08-ZFCD2.
         WHEN '01'.
            TMP_ID = '000'.
         WHEN '02'.
            IF IT_IMG08-ZFCD4 EQ 'LAST'.
               TMP_ID = '000'.
            ELSE.
               TMP_ID = TMP_ID + 100.
            ENDIF.
         WHEN '03'.
            IF IT_IMG08-ZFCD4 EQ 'LAST'.
               TMP_ID = '000'.
            ELSE.
               TMP_ID = TMP_ID + 1.
            ENDIF.
      ENDCASE.
      NODETAB-NEXT+3(3) = TMP_ID.

*------------------------------------------------------------------
*> T-code Á¤ÀÇ.
*------------------------------------------------------------------
      IF NOT IT_IMG08-ZFCD3 IS INITIAL.
         CONCATENATE 'ZIMG' IT_IMG08-ZFCD3 INTO NODETAB-HIDE.
         MOVE : NODETAB-HIDE TO NODETAB-TEXT1.
      ENDIF.
      CASE NODETAB-TLEVEL.
         WHEN '01'.
            NODETAB-PARENT = '000000'.
         WHEN '02'.
            NODETAB-PARENT = '000001'.
         WHEN '03'.
            MOVE : NODETAB-ID TO NODETAB-PARENT,
                   '01'       TO NODETAB-PARENT+4(2).
      ENDCASE.

      IF NOT NODETAB-HIDE IS INITIAL.
         NODETAB-TYPE = 'TRAN'.
      ENDIF.
      APPEND NODETAB.
      MOVE-CORRESPONDING NODETAB TO TREE.
      APPEND TREE.
   ENDLOOP.

*    REFRESH NODETAB.
*    LOOP AT TREE.
*      CLEAR NODETAB.
*      MOVE-CORRESPONDING TREE TO NODETAB.
*      SELECT SINGLE * FROM DEMOTREE WHERE ID   =  TREE-ID.
*      IF SY-SUBRC = 0.
*        NODETAB-TEXT1 = DEMOTREE-REPNAME.
*      ENDIF.
*      SELECT SINGLE * FROM DEMOTREET WHERE SPRAS = SY-LANGU
*                                    AND   ID    = TREE-ID.
*      IF SY-SUBRC = 0.
*        NODETAB-TEXT = DEMOTREET-STEXT.
*      ELSE.
*        NODETAB-TEXT = TREE-NAME.
*      ENDIF.
*      APPEND NODETAB.
*    ENDLOOP.
    CALL FUNCTION 'RS_TREE_SET_CURRENT_TREE'
         TABLES
              NODEINFO = NODETAB.

    CALL FUNCTION 'RS_TREE_SET_CURRENT_LAYOUT'
         EXPORTING
              CURSOR_COLUMN = CUCOL
              CURSOR_LINE   = CULIN
              FIRST_NODE    = FIRST_NODE
              LIST_COLUMN   = LICOL
              LIST_LINE     = LILIN
         TABLES
              LAYOUT        = LAYOUT.

*  ENDIF.

ENDFORM.                               " GET_DEMOTREE

*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK USING ACTION.

*  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
*           ID 'DEVCLASS' DUMMY
*           ID 'OBJTYPE'  FIELD 'DEMOTREE'
*           ID 'OBJNAME'  DUMMY
*           ID 'P_GROUP'  DUMMY
*           ID 'ACTVT'    FIELD ACTION.
  SY-SUBRC = 0.
  IF ACTION = ACT_EDIT.
    CALL 'C_SAPGPARAM'
       ID 'NAME' FIELD 'transport/systemtype'
       ID 'VALUE' FIELD SYSTEMTYPE.
    IF SYSTEMTYPE <> 'SAP'.
      SY-SUBRC = 1.
    ELSE.
      IF SY-SYSID <> 'B20'.
        SY-SUBRC = 1.
      ENDIF.
    ENDIF.
  ENDIF.

  IF SY-SUBRC NE 0.
    IF ACTION = ACT_EDIT.
      MESSAGE E195.
*       Sie besitzen keine Änderungsberechtigung
    ELSEIF ACTION = ACT_SHOW.
      MESSAGE E196.
*       Sie besitzen keine Anzeigeberechtigung
    ENDIF.
  ENDIF.
ENDFORM.                               " AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  TRANSPORT_DEMO
*&---------------------------------------------------------------------*
FORM TRANSPORT_DEMO.

  DATA: BEGIN OF TE071 OCCURS 2.
          INCLUDE STRUCTURE KO200.
  DATA: END OF TE071.
  DATA: BEGIN OF TE071K OCCURS 2.
          INCLUDE STRUCTURE E071K.
  DATA: END OF TE071K.

* Darstellung sichern
  CALL FUNCTION 'RS_TREE_GET_CURRENT_TREE'
       TABLES
            NODEINFO = NODETAB.
  CALL FUNCTION 'RS_TREE_GET_CURRENT_LAYOUT'
       IMPORTING
            CURSOR_COLUMN = CUCOL
            CURSOR_LINE   = CULIN
            FIRST_NODE    = FIRST_NODE
            LIST_COLUMN   = LICOL
            LIST_LINE     = LILIN
       TABLES
            LAYOUT        = LAYOUT.


  TE071-PGMID    = 'R3TR'.
  TE071-OBJECT   = 'TABU'.
  TE071-OBJ_NAME = 'DEMOTREE'.
  TE071-OBJFUNC  = 'K'.
  APPEND TE071.
  TE071-OBJ_NAME = 'DEMOTREET'.
  APPEND TE071.
  TE071-OBJ_NAME = 'DWTREE'.
  APPEND TE071.

  TE071K-PGMID   = 'R3TR'.
  TE071K-OBJECT  = 'TABU'.
  TE071K-OBJNAME = 'DEMOTREE'.
  TE071K-TABKEY  = '*'.
  TE071K-MASTERTYPE = 'TABU'.
  TE071K-MASTERNAME = 'DEMOTREE'.
  APPEND TE071K.
  TE071K-OBJNAME    = 'DEMOTREET'.
  TE071K-MASTERNAME = 'DEMOTREET'.
  APPEND TE071K.
  CONCATENATE 'DM' DEMO_ID '*' INTO TE071K-TABKEY.
  TE071K-OBJNAME    = 'DWTREE'.
  TE071K-MASTERNAME = 'DWTREE'.
  APPEND TE071K.


  CALL FUNCTION 'TR_EDIT_CHECK_OBJECTS_KEYS'
       EXPORTING
            WI_WITH_DIALOG = 'R'
       TABLES
            WT_E071        = TE071
            WT_E071K       = TE071K
       EXCEPTIONS
            OTHERS.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'TR_EDIT_CHECK_OBJECTS_KEYS'
       EXPORTING
            WI_WITH_DIALOG = 'X'
       TABLES
            WT_E071        = TE071
            WT_E071K       = TE071K
       EXCEPTIONS
            OTHERS.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    MESSAGE S112.
*     Die gewünschten Transporteinträge wurden erstellt
  ENDIF.

* Darstellung wieder setzen
  CALL FUNCTION 'RS_TREE_SET_CURRENT_TREE'
       TABLES
            NODEINFO = NODETAB.

  CALL FUNCTION 'RS_TREE_SET_CURRENT_LAYOUT'
       EXPORTING
            CURSOR_COLUMN = CUCOL
            CURSOR_LINE   = CULIN
            FIRST_NODE    = FIRST_NODE
            LIST_COLUMN   = LICOL
            LIST_LINE     = LILIN
       TABLES
            LAYOUT        = LAYOUT.

ENDFORM.                               " TRANSPORT_APPL

*&---------------------------------------------------------------------*
*&      Form  ADD_NODE
*&---------------------------------------------------------------------*
FORM ADD_NODE USING NODE LIKE SEUCOMM.

  DATA: LEN TYPE I.

  CLEAR NODE-TEXT1.
  CLEAR NODE-TYPE.
* Prüfen, ob angegebener Name ein Report ist
  REPNAME = NODE-TEXT.
  TRANSLATE REPNAME TO UPPER CASE.           "#EC SYNTCHAR
  SELECT SINGLE * FROM TRDIR WHERE NAME = REPNAME.
  IF SY-SUBRC = 0.
    READ TEXTPOOL REPNAME INTO TEXTTAB LANGUAGE SY-LANGU.
    CLEAR TEXTTAB.
    TEXTTAB-ID = 'R'.
    READ TABLE TEXTTAB.
    IF SY-SUBRC = 0.
      NODE-TEXT  = TEXTTAB-ENTRY.
      NODE-TEXT1 = REPNAME.
      NODE-TYPE = 'PROG'.
      EXIT.
    ENDIF.
  EXIT.
  ENDIF.
  TSTCT-TCODE = NODE-TEXT.
  TRANSLATE TSTCT-TCODE TO UPPER CASE.        "#EC SYNTCHAR
  SELECT SINGLE * FROM TSTC WHERE TCODE = TSTCT-TCODE.
  IF SY-SUBRC = 0.
    SELECT SINGLE * FROM TSTCT
           WHERE SPRSL = SY-LANGU AND TCODE = TSTCT-TCODE.
    IF SY-SUBRC = 0.
      NODE-TEXT = TSTCT-TTEXT.
      NODE-TEXT1 = TSTCT-TCODE.
      NODE-TYPE = 'TRAN'.
    ENDIF.
  ENDIF.
ENDFORM.                               " ADD_NODE
*&---------------------------------------------------------------------*
*&      Form  PRINT_NODE_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROOTID  text
*----------------------------------------------------------------------*
FORM PRINT_NODE_TAB USING    P_ID LEVEL.
FIELD-SYMBOLS: <F> TYPE TREEV_NODE,
               <H> TYPE TREEV_NODE,
               <P> TYPE TREEV_NODE.
IF P_ID IS INITIAL OR LEVEL = 0.
ELSE.
  LEVEL = LEVEL - 1.
  READ TABLE CTRL_NODE_TAB2 ASSIGNING <F> WITH KEY NODE_KEY = P_ID.
  APPEND <F> TO CTRL_NODE_TAB.
  LOOP AT CTRL_NODE_TAB2 ASSIGNING <P>.
    IF <P>-RELATSHIP = TREEV_RELAT_FIRST_CHILD AND
       <P>-RELATKEY = <F>-NODE_KEY.
       PERFORM PRINT_NODE_TAB USING <P>-NODE_KEY LEVEL.
       EXIT.
    ENDIF.
  ENDLOOP.
  LOOP AT CTRL_NODE_TAB2 ASSIGNING <H>.
    IF <H>-RELATSHIP = TREEV_RELAT_PREV_SIBLING AND
       <H>-NODE_KEY = <F>-NODE_KEY.
    PERFORM PRINT_NODE_TAB USING <H>-RELATKEY LEVEL.
    EXIT.
    ENDIF.
  ENDLOOP.
ENDIF.
ENDFORM.                    " PRINT_NODE_TAB
*&---------------------------------------------------------------------*
*&      Form  TRANSFORM_NODE_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TRANSFORM_NODE_TAB.
FIELD-SYMBOLS: <F> TYPE TREEV_NODE.
LOOP AT CTRL_NODE_TAB ASSIGNING <F>.
IF <F>-RELATSHIP = TREEV_RELAT_FIRST_CHILD.
   <F>-RELATSHIP = TREEV_RELAT_LAST_CHILD.
ENDIF.
IF <F>-RELATSHIP = TREEV_RELAT_NEXT_SIBLING.
   READ TABLE NODETAB WITH KEY ID = <F>-NODE_KEY.
   <F>-RELATSHIP = TREEV_RELAT_LAST_CHILD.
   <F>-RELATKEY = NODETAB-PARENT.
ENDIF.
ENDLOOP.
ENDFORM.                    " TRANSFORM_NODE_TAB
*&---------------------------------------------------------------------*
*&      Form  LOAD_PIC_FROM_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PIC_DATA  text
*      -->P_0166   text
*      <--P_PIC_SIZE  text
*----------------------------------------------------------------------*
FORM LOAD_PIC_FROM_DB TABLES   PIC_DATA
                      USING    pic_name
                      CHANGING PIC_SIZE.
  DATA QUERY_TABLE LIKE W3QUERY OCCURS 1 WITH HEADER LINE.
  DATA HTML_TABLE LIKE W3HTML OCCURS 1.
  DATA RETURN_CODE LIKE  W3PARAM-RET_CODE.
  DATA CONTENT_TYPE LIKE  W3PARAM-CONT_TYPE.
  DATA CONTENT_LENGTH LIKE  W3PARAM-CONT_LEN.

  REFRESH QUERY_TABLE.
  QUERY_TABLE-NAME = '_OBJECT_ID'.
  QUERY_TABLE-VALUE = PIC_NAME.
  APPEND QUERY_TABLE.

  CALL FUNCTION 'WWW_GET_MIME_OBJECT'
       TABLES
            QUERY_STRING        = QUERY_TABLE
            HTML                = HTML_TABLE
            MIME                = PIC_DATA
       CHANGING
            RETURN_CODE         = RETURN_CODE
            CONTENT_TYPE        = CONTENT_TYPE
            CONTENT_LENGTH      = CONTENT_LENGTH
       EXCEPTIONS
            OBJECT_NOT_FOUND       = 1
            PARAMETER_NOT_FOUND = 2
            OTHERS              = 3.
  if sy-subrc = 0.
    PIC_SIZE = CONTENT_LENGTH.
  endif.

ENDFORM.                    " LOAD_PIC_FROM_DB
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DESCRIPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_DESCRIPTION.

   READ TABLE nodetab WITH KEY id = G_NODE_KEY INTO node.
   G_NODE_KEY_OLD = G_NODE_KEY.

   IF sy-subrc <> 0.
      MESSAGE E902(ZIM1).
   ENDIF.

   IF node-type IS INITIAL or node-text1 is initial.
      MESSAGE S901(ZIM1).
   ELSE.
      IF W_STATUS EQ 'C' AND
         FCODE    IS INITIAL.
         CALL METHOD editor->get_text_as_stream
              IMPORTING text        = SRC[].

         REFRESH : SRC_TMP.
         LOOP AT SRC  INTO SRC_TMP-TDLINE.
            SRC_TMP-ZFCD  = G_NODE_KEY_TXT+3(3).
            SRC_TMP-MANDT = SY-MANDT.
            SRC_TMP-ZFSEQ = SY-TABIX.
*            SRC_TMP-TDLINE = SRC-LINE.
            APPEND SRC_TMP.
         ENDLOOP.
         SET UPDATE TASK LOCAL.

         DELETE FROM ZTIMGTXT
                WHERE ZFCD EQ G_NODE_KEY_TXT+3(3).
         MODIFY ZTIMGTXT FROM TABLE SRC_TMP.
         IF SY-SUBRC EQ 0.
            COMMIT WORK.
            W_STATUS = 'D'.
         ELSE.
            ROLLBACK WORK.
            MESSAGE S906(ZIM1).
            EXIT.
         ENDIF.
      ENDIF.

      G_ZFCD = G_NODE_KEY+3(3).
      REFRESH : SRC.
      SELECT TDLINE INTO CORRESPONDING FIELDS OF TABLE SRC
               FROM  ZTIMGTXT
               WHERE ZFCD = G_ZFCD.
*      SRC_TMP
      CALL METHOD editor->set_text_as_stream
           EXPORTING  text = SRC
           EXCEPTIONS error_dp        = 1
                      error_dp_create = 2.
*     CALL METHOD EDITOR->SET_TEXT_AS_R3TABLE
*                 EXPORTING
*                    TABLE = SRC[].

*     CALL METHOD editor->set_text_as_stream
*          EXPORTING  text = SRC[]
*          EXCEPTIONS error_dp        = 1
*                     error_dp_create = 2.

*     CALL METHOD cl_gui_cfw=>FLUSH.
   ENDIF.
   CLEAR : G_NODE_KEY_TXT.


ENDFORM.                    " P1000_GET_DESCRIPTION
