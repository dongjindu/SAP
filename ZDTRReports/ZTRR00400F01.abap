*&---------------------------------------------------------------------*
*&  Include           ZTRR00400F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form INIT_SCREEN .

  GET PARAMETER ID 'BUK' FIELD P_BUKRS.
  IF P_BUKRS IS INITIAL.
    P_BUKRS = 'H201'.
  ENDIF.

  loop at screen.
    if screen-name = 'P_BUTXT'.
      screen-input  = 0.
      screen-intensified = '0'.
      screen-display_3d  = '0'.
      modify screen.
    endif.
    if screen-name = 'P_BUKRS'.
      screen-input = ' '.
      modify screen.
    endif.
  endloop.


* & find text.
  perform fi_wt_read_t001 using    p_bukrs
                          changing p_butxt.

endform.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  FI_WT_READ_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      <--P_P_BUTXT  text
*----------------------------------------------------------------------*
form FI_WT_READ_T001  using    pa_bukrs
                      changing pa_butxt.

  data : it_t001 like t001.

  call function 'FI_WT_READ_T001'
    exporting
      i_bukrs   = pa_bukrs
    importing
      t_t001    = it_t001
    exceptions
      not_found = 1.

  case sy-subrc.
    when 0.
      pa_butxt = it_t001-butxt.
    when 1.
      message s101(f5).
    when others.
  endcase.

endform.                    " FI_WT_READ_T001

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM SELECT_DATA.

*// Get Plan Date
  CALL FUNCTION 'ZTR_GET_PLAN_DATE'
    EXPORTING
      GJAHR        = P_GJAHR
      MONTH        = P_MONTH
      FIRST_MONTH  = FIRST_MONTH
      SECOND_MONTH = SECOND_MONTH
      THIRD_MONTH  = THIRD_MONTH
      LATER        = LATER
      ALL_DAY      = ALL_DAY
    TABLES
      DATE         = DATE.

*// Get Plan Monthly Data
  CALL FUNCTION 'ZTR_GET_PLAN_MONTHLY'
    EXPORTING
      BUKRS    = P_BUKRS
      GJAHR    = P_GJAHR
      MONTH    = P_MONTH
      PLANDATA = P_R1         " Plan
      ZTYPE    = C_MONTHLY    " MONTHLY 2.
      SEQNO    = P_SEQNO
    TABLES
      DATE     = DATE
      PLAN     = PLAN.

*// Get Balance
  CALL FUNCTION 'ZTR_GET_BALANCE'
    EXPORTING
      BUKRS     = P_BUKRS
      GJAHR     = P_GJAHR
      MONTH     = P_MONTH
    TABLES
      DATE      = DATE
      DATA      = PLAN
      BEGINNING = BEGINNING
      ENDING    = ENDING.

  ICLEAR GT_DATE.

  SORT DATE.

  LOOP AT DATE.
    GT_DATE-INDEX = GT_DATE-INDEX + 1.
    GT_DATE-DATUM = DATE.
    APPEND GT_DATE.
  ENDLOOP.

  SELECT SINGLE WAERS INTO G_WAERS
    FROM T001
   WHERE BUKRS = P_BUKRS.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_TREE_DATA
*&---------------------------------------------------------------------*
FORM MAKE_TREE_DATA .

  TABLES : V_TKCHH.

  SELECT SINGLE * FROM V_TKCHH WHERE ID1 = 'FDGRP'
                                 AND ID2 = P_HIER.

  CALL FUNCTION 'RKC_GET_HIERARCHY_WITH_TEXT'
    EXPORTING
      APPLCLASS         = 'TRM'
      I_HIERARCHY       = V_TKCHH
    IMPORTING
      O_HIERARCHY       = V_TKCHH
    TABLES
      HIERARCHY_POINTER = HIER_DB
      HIERARCHY_TABLE   = HIER_TB
    EXCEPTIONS
      BAD_HIERARCHY     = 01
      BAD_KEYID         = 02
      DB_ERROR          = 03.

*// = HIERARCHY add to data_itab.
  DATA : L_NODID LIKE HIER_DB-NODID,
         LT_PLAN LIKE PLAN OCCURS 0 WITH HEADER LINE.

  LOOP AT PLAN.
    READ TABLE HIER_DB WITH KEY WERT1 = PLAN-GRUPP.
    IF SY-SUBRC = 0.
      L_NODID = HIER_DB-PARNT.
      LT_PLAN = PLAN.
      DO.
        READ TABLE HIER_DB WITH KEY NODID = L_NODID.
        IF SY-SUBRC NE 0.
          EXIT.
        ENDIF.

        LT_PLAN-GRUPP = HIER_DB-WERT1.
        COLLECT LT_PLAN.
        L_NODID = HIER_DB-PARNT.
      ENDDO.
    ENDIF.
  ENDLOOP.

  LOOP AT LT_PLAN.
    PLAN = LT_PLAN.
    ICOLLECT PLAN.
  ENDLOOP.

  ICLEAR : GT_NODE_TABLE, GT_ITEM_TABLE.

* Beginning Balance
  PERFORM MAKE_BEGINNING_BALANCE.

* Tree
  PERFORM MAKE_PLAN.

* Ending Balance
  PERFORM MAKE_ENDING_BALANCE.

ENDFORM.                    " MAKE_TREE_DATA
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_TO_ZTTR0008
*&---------------------------------------------------------------------*
FORM SAVE_TO_ZTTR0008 .
  DATA : L_PDAT1     LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_MONTH '01' INTO L_PDAT1.

  CALL FUNCTION 'ZTR_SAVE_PLAN'
    EXPORTING
      BUKRS       = P_BUKRS
      GJAHR       = P_GJAHR
      PDAT1       = L_PDAT1
      ZTYPE       = C_MONTHLY
    EXCEPTIONS
      SAVE_ERROR  = 1
      SAVE_CANCEL = 2
      OTHERS      = 3.

ENDFORM.                    " SAVE_TO_ZTTR0008
*&---------------------------------------------------------------------*
*&      Form  GET_SEQ_MAX
*&---------------------------------------------------------------------*
FORM GET_SEQ_MAX  USING    P_SEQNO
                           P_BUKRS
                           P_PDAT1
                           P_GJAHR
                           P_ZTYPE.
  CLEAR P_SEQNO.

  SELECT MAX( SEQNO ) INTO P_SEQNO
    FROM ZTTR0008
   WHERE BUKRS = P_BUKRS
     AND PDAT1 = P_PDAT1
     AND GJAHR = P_GJAHR
     AND ZTYPE = P_ZTYPE.

  ADD 1 TO P_SEQNO.

ENDFORM.                    " GET_SEQ_MAX
*&---------------------------------------------------------------------*
*&      Module  STATUS_8000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_8000 OUTPUT.

  SET PF-STATUS 'S8000'.

ENDMODULE.                 " STATUS_8000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXPAND_NODE
*&---------------------------------------------------------------------*
FORM EXPAND_NODE .
  CLEAR   GT_NODE_TABLE-NODE_KEY.
  CALL METHOD G_GRID->GET_SELECTED_ITEM
    IMPORTING
      NODE_KEY          = GT_NODE_TABLE-NODE_KEY
    EXCEPTIONS
      FAILED            = 1
      CNTL_SYSTEM_ERROR = 2
      NO_ITEM_SELECTION = 3.

  IF GT_NODE_TABLE-NODE_KEY IS INITIAL.
    CALL METHOD G_GRID->GET_SELECTED_NODE
      IMPORTING
        NODE_KEY                   = GT_NODE_TABLE-NODE_KEY
      EXCEPTIONS
        FAILED                     = 1
        SINGLE_NODE_SELECTION_ONLY = 2
        CNTL_SYSTEM_ERROR          = 3.
  ENDIF.

  IF NOT GT_NODE_TABLE-NODE_KEY IS INITIAL.
    CALL METHOD G_GRID->EXPAND_NODE
      EXPORTING
        NODE_KEY            = GT_NODE_TABLE-NODE_KEY
        EXPAND_SUBTREE      = 'X'
      EXCEPTIONS
        FAILED              = 1
        ILLEGAL_LEVEL_COUNT = 2
        CNTL_SYSTEM_ERROR   = 3
        NODE_NOT_FOUND      = 4
        CANNOT_EXPAND_LEAF  = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.
  ENDIF.

ENDFORM.                    " EXPAND_NODE
*&---------------------------------------------------------------------*
*&      Form  COLLAPSE_NODE
*&---------------------------------------------------------------------*
FORM COLLAPSE_NODE .
  CLEAR   GT_NODE_TABLE-NODE_KEY.

  CALL METHOD G_GRID->GET_SELECTED_ITEM
    IMPORTING
      NODE_KEY          = GT_NODE_TABLE-NODE_KEY
    EXCEPTIONS
      FAILED            = 1
      CNTL_SYSTEM_ERROR = 2
      NO_ITEM_SELECTION = 3.

  IF GT_NODE_TABLE-NODE_KEY IS INITIAL.
    CALL METHOD G_GRID->GET_SELECTED_NODE
      IMPORTING
        NODE_KEY                   = GT_NODE_TABLE-NODE_KEY
      EXCEPTIONS
        FAILED                     = 1
        SINGLE_NODE_SELECTION_ONLY = 2
        CNTL_SYSTEM_ERROR          = 3.
  ENDIF.

  IF NOT GT_NODE_TABLE-NODE_KEY IS INITIAL.
    CALL METHOD G_GRID->COLLAPSE_SUBTREE
      EXPORTING
        NODE_KEY          = GT_NODE_TABLE-NODE_KEY
      EXCEPTIONS
        FAILED            = 1
        NODE_NOT_FOUND    = 2
        CNTL_SYSTEM_ERROR = 3.
  ENDIF.

ENDFORM.                    " COLLAPSE_NODE
*&---------------------------------------------------------------------*
*&      Form  DATA_PRINT
*&---------------------------------------------------------------------*
FORM DATA_PRINT .

  DATA : TITLE(30) OCCURS 0 WITH HEADER LINE.
  ICLEAR TITLE.

  LOOP AT DATE.
    IF DATE = '99999999'.
      WRITE : 'Later' TO TITLE.
    ELSE.
      WRITE : DATE TO TITLE.
    ENDIF.
    IAPPEND TITLE.
  ENDLOOP.

  CALL FUNCTION 'ZTR_PRINT_PLAN'
    EXPORTING
      HIER      = P_HIER
      SKALV     = P_SKALV
      DECIM     = P_DECIM
    TABLES
      TITLE     = TITLE
      DATE      = DATE
      BEGINNING = BEGINNING
      PLAN      = PLAN
      ENDING    = ENDING.

ENDFORM.                    " DATA_PRINT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS OUTPUT.

  DATA FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH FCODE.

  IF P_R2 = 'X'.
    APPEND 'SAVE'    TO FCODE.
    APPEND 'SINGLE'  TO FCODE.
  ENDIF.

  CASE FIRST_MONTH.
    WHEN 'D'.  APPEND 'FIRST_D'    TO FCODE.
    WHEN 'M'.  APPEND 'FIRST_M'    TO FCODE.
  ENDCASE.

  CASE SECOND_MONTH.
    WHEN 'D'.  APPEND 'SECOND_D'    TO FCODE.
    WHEN 'M'.  APPEND 'SECOND_M'    TO FCODE.
  ENDCASE.

  CASE THIRD_MONTH.
    WHEN 'D'.  APPEND 'THIRD_D'    TO FCODE.
    WHEN 'M'.  APPEND 'THIRD_M'    TO FCODE.
  ENDCASE.

  SET PF-STATUS 'S9000'  EXCLUDING FCODE.
  CASE 'X'.
    WHEN P_R1.
      SET TITLEBAR  'T9000' WITH 'Planning'.
    WHEN P_R2.
      SET TITLEBAR  'T9000' WITH 'Display -' P_SEQNO.
  ENDCASE.

ENDMODULE.                 " SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_SCREEN OUTPUT.

  PERFORM CREATE_SCREEN.

ENDMODULE.                 " CREATE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
FORM CREATE_CONTAINER .

  CREATE OBJECT G_CUSTOM_CONTAINER
    EXPORTING
      CONTAINER_NAME              = 'CUSTOM_CONTAINER'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

ENDFORM.                    " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  CREATE_COLUMN
*&---------------------------------------------------------------------*
FORM CREATE_COLUMN .
  DATA : COLNAME(12),
         HEADER(132).
* Tree Column
  GS_HIERARCHY_HEADER-HEADING = 'Code'.
  GS_HIERARCHY_HEADER-WIDTH   = 50.

  CREATE OBJECT G_GRID
    EXPORTING
      PARENT                      = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION              = 'X'
      HIERARCHY_COLUMN_NAME       = 'C01'
      HIERARCHY_HEADER            = GS_HIERARCHY_HEADER
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      ILLEGAL_COLUMN_NAME         = 5
      LIFETIME_ERROR              = 6.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* ?? Column
  PERFORM ADD_COLUMN USING 'C02' '10' 'L' 'Name'.
  LOOP AT GT_DATE.
    CONCATENATE 'F' GT_DATE-INDEX INTO COLNAME.
    IF GT_DATE-DATUM = '99999999'.
      HEADER = 'Later'.
    ELSE.
      WRITE : GT_DATE-DATUM TO HEADER.
    ENDIF.
    PERFORM ADD_COLUMN USING COLNAME '21' 'R' HEADER.
  ENDLOOP.

ENDFORM.                    " CREATE_COLUMN
*&---------------------------------------------------------------------*
*&      Form  ADD_COLUMN
*&---------------------------------------------------------------------*
FORM ADD_COLUMN  USING    P_COLNAME
                          P_WIDTH
                          P_ALIGNMENT
                          P_TEXT.

  DATA : L_ALIGNMENT TYPE I.

  CASE P_ALIGNMENT.
    WHEN 'L'.
      L_ALIGNMENT = CL_GUI_COLUMN_TREE=>ALIGN_LEFT.
    WHEN 'C'.
      L_ALIGNMENT = CL_GUI_COLUMN_TREE=>ALIGN_CENTER.
    WHEN 'R'.
      L_ALIGNMENT = CL_GUI_COLUMN_TREE=>ALIGN_RIGHT.
  ENDCASE.


  CALL METHOD G_GRID->ADD_COLUMN
    EXPORTING
      NAME                         = P_COLNAME
      WIDTH                        = P_WIDTH
      ALIGNMENT                    = L_ALIGNMENT
      HEADER_TEXT                  = P_TEXT
    EXCEPTIONS
      COLUMN_EXISTS                = 1
      ILLEGAL_COLUMN_NAME          = 2
      TOO_MANY_COLUMNS             = 3
      ILLEGAL_ALIGNMENT            = 4
      DIFFERENT_COLUMN_TYPES       = 5
      CNTL_SYSTEM_ERROR            = 6
      FAILED                       = 7
      PREDECESSOR_COLUMN_NOT_FOUND = 8.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

ENDFORM.                    " ADD_COLUMN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TREE_ALV
*&---------------------------------------------------------------------*
FORM DISPLAY_TREE_ALV .

  CALL METHOD G_GRID->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE                     = GT_NODE_TABLE[]
      ITEM_TABLE                     = GT_ITEM_TABLE[]
      ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
    EXCEPTIONS
      FAILED                         = 1
      CNTL_SYSTEM_ERROR              = 3
      ERROR_IN_TABLES                = 4
      DP_ERROR                       = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

* Top Node ? Expand ??.
  LOOP AT GT_NODE_TABLE WHERE RELATKEY = SPACE.
    CALL METHOD G_GRID->EXPAND_NODE
      EXPORTING
        NODE_KEY            = GT_NODE_TABLE-NODE_KEY
        LEVEL_COUNT         = 2
        EXPAND_SUBTREE      = SPACE
      EXCEPTIONS
        FAILED              = 1
        ILLEGAL_LEVEL_COUNT = 2
        CNTL_SYSTEM_ERROR   = 3
        NODE_NOT_FOUND      = 4
        CANNOT_EXPAND_LEAF  = 5.
  ENDLOOP.
*  IF SY-SUBRC <> 0.
*    MESSAGE A000(TREE_CONTROL_MSG).
*  ENDIF.

* ITEM double click
*  G_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
*  G_EVENT-APPL_EVENT = 'X'.
*  APPEND G_EVENT TO G_EVENTS.
*
*  CREATE OBJECT G_APPLICATION.
*  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_GRID.
*
*  CALL METHOD G_GRID->SET_REGISTERED_EVENTS
*    EXPORTING
*      EVENTS                    = G_EVENTS
*    EXCEPTIONS
*      CNTL_ERROR                = 1
*      CNTL_SYSTEM_ERROR         = 2
*      ILLEGAL_EVENT_COMBINATION = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE A000.
*  ENDIF.

ENDFORM.                    " DISPLAY_TREE_ALV
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
FORM REFRESH_DATA .

  PERFORM GET_EXPANDED_NODES.


  IF G_GRID IS NOT INITIAL.
    CALL METHOD G_GRID->DELETE_ALL_NODES
      EXCEPTIONS
        FAILED            = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3.

    CALL METHOD G_GRID->FREE.

    PERFORM SELECT_DATA.
    PERFORM MAKE_TREE_DATA.
    PERFORM CREATE_COLUMN.
    PERFORM DISPLAY_TREE_ALV.
  ENDIF.

  LOOP AT G_NODE_KEY_TABLE.
    CALL METHOD G_GRID->EXPAND_NODE
      EXPORTING
        NODE_KEY = G_NODE_KEY_TABLE.
  ENDLOOP.
ENDFORM.                    " REFRESH_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_EXPANDED_NODES
*&---------------------------------------------------------------------*
FORM GET_EXPANDED_NODES .

  ICLEAR G_NODE_KEY_TABLE.

  CALL METHOD G_GRID->GET_EXPANDED_NODES
    CHANGING
      NODE_KEY_TABLE    = G_NODE_KEY_TABLE[]
    EXCEPTIONS
      CNTL_SYSTEM_ERROR = 1
      DP_ERROR          = 2
      FAILED            = 3
      OTHERS            = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " GET_EXPANDED_NODES
*&---------------------------------------------------------------------*
*&      Form  CREATE_SCREEN
*&---------------------------------------------------------------------*
FORM CREATE_SCREEN .

  IF G_CUSTOM_CONTAINER IS INITIAL.
    PERFORM CREATE_CONTAINER.
    PERFORM CREATE_COLUMN.
    PERFORM DISPLAY_TREE_ALV.
  ENDIF.

ENDFORM.                    " CREATE_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MAKE_BEGINNING_BALANCE
*&---------------------------------------------------------------------*
FORM MAKE_BEGINNING_BALANCE .

  GT_NODE_TABLE-NODE_KEY  = C_BEGIN.
  GT_NODE_TABLE-ISFOLDER  = 'X'.
  GT_NODE_TABLE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  IAPPEND GT_NODE_TABLE.

*NAME
  GT_ITEM_TABLE-NODE_KEY  = C_BEGIN.
  GT_ITEM_TABLE-ITEM_NAME = 'C01'.
  GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  GT_ITEM_TABLE-TEXT      = 'Beginning Balance'.
  IAPPEND GT_ITEM_TABLE.

*CODE
  GT_ITEM_TABLE-NODE_KEY  = C_BEGIN.
  GT_ITEM_TABLE-ITEM_NAME = 'C02'.
  GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  GT_ITEM_TABLE-TEXT      = C_BEGIN.
  IAPPEND GT_ITEM_TABLE.

  LOOP AT GT_DATE.
    LOOP AT BEGINNING WHERE DATUM = GT_DATE-DATUM.
      GT_ITEM_TABLE-NODE_KEY  = C_BEGIN.
      CONCATENATE 'F' GT_DATE-INDEX INTO FIELDNAME.
      GT_ITEM_TABLE-ITEM_NAME = FIELDNAME.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING BEGINNING-DMSHB G_WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.
    ENDLOOP.
    IF SY-SUBRC <> 0.
      GT_ITEM_TABLE-NODE_KEY  = C_BEGIN.
      CONCATENATE 'F' GT_DATE-INDEX INTO FIELDNAME.
      GT_ITEM_TABLE-ITEM_NAME = FIELDNAME.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      CLEAR BEGINNING-DMSHB.
      PERFORM WRITE_AMOUNT USING BEGINNING-DMSHB G_WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MAKE_BEGINNING_BALANCE
*&---------------------------------------------------------------------*
*&      Form  MAKE_PLAN
*&---------------------------------------------------------------------*
FORM MAKE_PLAN.

  LOOP AT HIER_DB.

    GT_NODE_TABLE-NODE_KEY = HIER_DB-NODID.

    IF HIER_DB-PARNT IS NOT INITIAL.
      GT_NODE_TABLE-RELATKEY = HIER_DB-PARNT.
    ENDIF.

    GT_NODE_TABLE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.

    GT_ITEM_TABLE-NODE_KEY  = HIER_DB-NODID.
    GT_ITEM_TABLE-ITEM_NAME = 'C01'.
    GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.

    READ TABLE HIER_TB WITH KEY KEYID = HIER_DB-WERT1.

    SELECT SINGLE TXT40 INTO GT_ITEM_TABLE-TEXT
      FROM TKCHNT
     WHERE LANGU = SY-LANGU
       AND APPLC = 'KC'
       AND KEYID = HIER_TB-KEYID.

    IF SY-SUBRC = 0.
      GT_NODE_TABLE-ISFOLDER  = 'X'.
    ELSE.
      SELECT SINGLE TEXTL INTO GT_ITEM_TABLE-TEXT
        FROM T035T
       WHERE SPRAS = SY-LANGU
         AND GRUPP = HIER_DB-WERT1.
    ENDIF.

    IAPPEND GT_ITEM_TABLE.
    IAPPEND GT_NODE_TABLE.

    GT_ITEM_TABLE-NODE_KEY  = HIER_DB-NODID.
    GT_ITEM_TABLE-ITEM_NAME = 'C02'.
    GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    GT_ITEM_TABLE-TEXT      = HIER_DB-WERT1.
    IAPPEND GT_ITEM_TABLE.

    LOOP AT GT_DATE.
      LOOP AT PLAN WHERE GRUPP = HIER_DB-WERT1
                     AND DATUM = GT_DATE-DATUM.
        GT_ITEM_TABLE-NODE_KEY  = HIER_DB-NODID.
        CONCATENATE 'F' GT_DATE-INDEX INTO FIELDNAME.
        GT_ITEM_TABLE-ITEM_NAME = FIELDNAME.
        GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.

        PERFORM WRITE_AMOUNT USING PLAN-DMSHB G_WAERS
                                   GT_ITEM_TABLE-TEXT.
        IAPPEND GT_ITEM_TABLE.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        GT_ITEM_TABLE-NODE_KEY  = HIER_DB-NODID.
        CONCATENATE 'F' GT_DATE-INDEX INTO FIELDNAME.
        GT_ITEM_TABLE-ITEM_NAME = FIELDNAME.
        GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
        CLEAR PLAN-DMSHB.
        PERFORM WRITE_AMOUNT USING PLAN-DMSHB G_WAERS
                                   GT_ITEM_TABLE-TEXT.
        IAPPEND GT_ITEM_TABLE.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " MAKE_PLAN
*&---------------------------------------------------------------------*
*&      Form  MAKE_ENDING_BALANCE
*&---------------------------------------------------------------------*
FORM MAKE_ENDING_BALANCE .

  GT_NODE_TABLE-NODE_KEY  = C_END.
  GT_NODE_TABLE-ISFOLDER  = 'X'.
  GT_NODE_TABLE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  IAPPEND GT_NODE_TABLE.

*NAME
  GT_ITEM_TABLE-NODE_KEY  = C_END.
  GT_ITEM_TABLE-ITEM_NAME = 'C01'.
  GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  GT_ITEM_TABLE-TEXT      = 'Ending Balance'.
  IAPPEND GT_ITEM_TABLE.

*CODE
  GT_ITEM_TABLE-NODE_KEY  = C_END.
  GT_ITEM_TABLE-ITEM_NAME = 'C02'.
  GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  GT_ITEM_TABLE-TEXT      = C_END.
  IAPPEND GT_ITEM_TABLE.

  LOOP AT GT_DATE.
    LOOP AT ENDING WHERE DATUM = GT_DATE-DATUM.
      GT_ITEM_TABLE-NODE_KEY  = C_END.
      CONCATENATE 'F' GT_DATE-INDEX INTO FIELDNAME.
      GT_ITEM_TABLE-ITEM_NAME = FIELDNAME.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING ENDING-DMSHB G_WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      GT_ITEM_TABLE-NODE_KEY  = C_END.
      CONCATENATE 'F' GT_DATE-INDEX INTO FIELDNAME.
      GT_ITEM_TABLE-ITEM_NAME = FIELDNAME.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      CLEAR ENDING-DMSHB.
      PERFORM WRITE_AMOUNT USING ENDING-DMSHB G_WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MAKE_ENDING_BALANCE
*&---------------------------------------------------------------------*
*&      Form  WRITE_AMOUNT
*&---------------------------------------------------------------------*
FORM WRITE_AMOUNT  USING    P_AMOUNT
                            P_WAERS
                            P_TEXT.
  DATA : L_DMSHB LIKE FDSR-DMSHB,
         TEXT(20).

  L_DMSHB = P_AMOUNT / ( 10 ** P_SKALV ).

  WRITE L_DMSHB TO TEXT CURRENCY P_WAERS
                        DECIMALS P_DECIM.

* (-)?? ??? ???
  SEARCH TEXT FOR '-'.

  IF SY-SUBRC = 0.
    CALL FUNCTION 'STRING_REPLACE'
      EXPORTING
        PATTERN             = '-'
        SUBSTITUTE          = ''
      CHANGING
        TEXT                = TEXT
      EXCEPTIONS
        WRONG_STRING_LENGTH = 1
        OTHERS              = 2.
    CONDENSE TEXT.
    CONCATENATE '-' TEXT INTO TEXT.
    CALL FUNCTION 'STRING_MOVE_RIGHT'
      EXPORTING
        STRING    = TEXT
      IMPORTING
        RSTRING   = TEXT
      EXCEPTIONS
        TOO_SMALL = 1
        OTHERS    = 2.
  ENDIF.

  P_TEXT = TEXT.

ENDFORM.                    " WRITE_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWN
*&---------------------------------------------------------------------*
FORM EXCEL_DOWN .

  DATA : TITLE(30) OCCURS 0 WITH HEADER LINE.
  ICLEAR TITLE.
  LOOP AT DATE.
    IF DATE = '99999999'.
      WRITE : 'Later' TO TITLE.
    ELSE.
      WRITE : DATE TO TITLE.
    ENDIF.
    IAPPEND TITLE.
  ENDLOOP.

  CALL FUNCTION 'ZTR_EXCEL_DOWN'
    EXPORTING
      HIER      = P_HIER
      SKALV     = P_SKALV
      DECIM     = P_DECIM
    TABLES
      TITLE     = TITLE
      DATE      = DATE
      BEGINNING = BEGINNING
      PLAN      = PLAN
      ENDING    = ENDING.

ENDFORM.                    " EXCEL_DOWN
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND INPUT.

  CASE OK_CODE.
    WHEN 'EXPAND'.
      PERFORM EXPAND_NODE.

    WHEN 'COLLAPSE'.
      PERFORM COLLAPSE_NODE.

    WHEN 'SAVE'.
      PERFORM SAVE_TO_ZTTR0008.

    WHEN 'SINGLE'.
      CALL TRANSACTION 'FF63'.
      PERFORM REFRESH_DATA.

    WHEN 'NEWDISPLAY'.
      CALL SCREEN 8000 STARTING AT 20   5
                       ENDING   AT 80   8.
    WHEN 'OK'.
      PERFORM REFRESH_DATA.
      LEAVE TO SCREEN 0.

    WHEN 'FIRST_D'.
      FIRST_MONTH = 'D'.
      PERFORM REFRESH_DATA.

    WHEN 'FIRST_M'.
      FIRST_MONTH = 'M'.
      PERFORM REFRESH_DATA.

    WHEN 'SECOND_D'.
      SECOND_MONTH = 'D'.
      PERFORM REFRESH_DATA.

    WHEN 'SECOND_M'.
      SECOND_MONTH = 'M'.
      PERFORM REFRESH_DATA.

    WHEN 'THIRD_D'.
      THIRD_MONTH = 'D'.
      PERFORM REFRESH_DATA.

    WHEN 'THIRD_M'.
      THIRD_MONTH = 'M'.
      PERFORM REFRESH_DATA.

    WHEN 'REFRESH'.
      PERFORM REFRESH_DATA.

    WHEN 'PRINT'.
      PERFORM DATA_PRINT.

    WHEN 'EXCEL'.
      PERFORM EXCEL_DOWN.

  ENDCASE.

  CLEAR OK_CODE.

ENDMODULE.                 " USER_COMMAND  INPUT
