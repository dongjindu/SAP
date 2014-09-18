*&---------------------------------------------------------------------*
*&  Include           ZTRR00600F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MAKE_TREE_DATA
*&---------------------------------------------------------------------*
FORM MAKE_TREE_DATA .

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

  DATA : GT_ZTTR0008 LIKE ZTTR0008 OCCURS 0 WITH HEADER LINE,
         GT_FDSR     LIKE FDSR     OCCURS 0 WITH HEADER LINE,
         L_PDAT1     LIKE SY-DATUM,
         L_SEQNO     LIKE ZTTR0008-SEQNO,
         ANSWER.

  ICLEAR : GT_ZTTR0008, GT_FDSR .
  CLEAR: ANSWER.

  CONCATENATE P_GJAHR '0101' INTO L_PDAT1.

  CASE 'X'.
    WHEN P_R1.
      PERFORM GET_SEQ_MAX USING L_SEQNO
                                P_BUKRS
                                L_PDAT1
                                P_GJAHR
                                C_YEARLY.
      IF L_SEQNO > 1.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            TEXTLINE1      = 'Already exist'
            TEXTLINE2      = 'Are you save again?'
            TITEL          = 'Save'
            CANCEL_DISPLAY = ' '
          IMPORTING
            ANSWER         = ANSWER.
        IF ANSWER NE 'J'.
          MESSAGE S030.
          REJECT.
        ENDIF.
      ENDIF.
      P_SEQNO = L_SEQNO.

    WHEN P_R2.

      CALL FUNCTION 'POPUP_TO_DECIDE'
        EXPORTING
          DEFAULTOPTION  = '2'
          TEXTLINE1      = 'Determine save option !'
          TEXT_OPTION1   = 'New Version'
          TEXT_OPTION2   = 'Overwrite'
          TITEL          = 'Save'
          CANCEL_DISPLAY = 'X'
        IMPORTING
          ANSWER         = ANSWER.

      CASE ANSWER.
        WHEN '1'.
          PERFORM GET_SEQ_MAX USING L_SEQNO
                                    P_BUKRS
                                    L_PDAT1
                                    P_GJAHR
                                    C_YEARLY.
          P_SEQNO = L_SEQNO.
        WHEN '2'.
          L_SEQNO = P_SEQNO.
        WHEN OTHERS.
          MESSAGE S030.
          REJECT.
      ENDCASE.

  ENDCASE.

  LOOP AT PLAN.
    SELECT SINGLE TEXTL INTO GT_ITEM_TABLE-TEXT
         FROM T035T
        WHERE SPRAS = SY-LANGU
          AND GRUPP = PLAN-GRUPP.
    CHECK SY-SUBRC = 0.

    MOVE-CORRESPONDING PLAN TO GT_ZTTR0008.
    GT_ZTTR0008-BUKRS = P_BUKRS.
    GT_ZTTR0008-PDAT1 = L_PDAT1.
    GT_ZTTR0008-GJAHR = P_GJAHR.
    GT_ZTTR0008-ZTYPE = C_YEARLY.
    GT_ZTTR0008-SEQNO = L_SEQNO.
    GT_ZTTR0008-ERNAM = SY-UNAME.
    GT_ZTTR0008-ERDAT = SY-DATUM.
    GT_ZTTR0008-ERZET = SY-UZEIT.
    IAPPEND  GT_ZTTR0008.

  ENDLOOP.

  MODIFY ZTTR0008 FROM TABLE GT_ZTTR0008.

  IF SY-SUBRC = 0.
    COMMIT WORK.
    MESSAGE S007.
  ELSE.
    ROLLBACK WORK.
    MESSAGE W011.
  ENDIF.

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
      WRITE : 'Sum' TO TITLE.
    ELSE.
      CONCATENATE DATE+4(2) '.' DATE(4) INTO TITLE.
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
*&      Form  EXCEL_DOWN
*&---------------------------------------------------------------------*
FORM EXCEL_DOWN .

  DATA : TITLE(30) OCCURS 0 WITH HEADER LINE.
  ICLEAR TITLE.
  LOOP AT DATE.
    IF DATE = '99999999'.
      WRITE : 'Sum' TO TITLE.
    ELSE.
      CONCATENATE DATE+4(2) '.' DATE(4) INTO TITLE.
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
*&      Module  SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS OUTPUT.

  DATA FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH FCODE.

  IF P_R2 = 'X'.
*    APPEND 'SAVE'    TO FCODE.
    APPEND 'SINGLE'  TO FCODE.
    APPEND 'PA'      TO FCODE.
    APPEND 'CCA'     TO FCODE.
    APPEND 'IM'      TO FCODE.
*    APPEND 'MA'      TO FCODE.
  ENDIF.

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
      HEADER = 'Sum'.
    ELSE.
      CONCATENATE GT_DATE-DATUM+4(2) '.' GT_DATE-DATUM(4) INTO HEADER.
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


* ITEM double click
  G_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  G_EVENT-APPL_EVENT = 'X'.
  APPEND G_EVENT TO G_EVENTS.

  CREATE OBJECT G_APPLICATION.
  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_GRID.

  CALL METHOD G_GRID->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = G_EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.
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
FORM MAKE_PLAN .
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
*&      Form  SELECT_CO_DATA
*&---------------------------------------------------------------------*
FORM SELECT_CO_DATA .

  ICLEAR : GT_PA, GT_CCA.

*2011.08.23 imsi delete
***  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_PA
***    FROM CE2HK00         AS A
***    LEFT OUTER JOIN KNB1 AS B
***      ON A~KUNWE = B~KUNNR
***     AND A~BUKRS = B~BUKRS
***   WHERE A~PALEDGER = '01'
***     AND A~VERSI    = P_VERSN
***     AND A~GJAHR    = P_GJAHR.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_CCA
    FROM COSP AS A
    JOIN SKB1 AS B
      ON A~KSTAR = B~SAKNR
*   WHERE A~OBJNR LIKE 'KS%'
   WHERE A~OBJNR LIKE 'K%'
     AND A~LEDNR = '00'
     AND A~GJAHR = P_GJAHR
     AND A~WRTTP = '01'
     AND A~VERSN = P_VERSN
     AND B~BUKRS = 'H201'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE GT_IM
    FROM AUFK AS A
    JOIN COSP AS B
      ON A~OBJNR = B~OBJNR
    JOIN ZTTR0009 AS C
      ON A~AUFNR = C~AUFNR
   WHERE A~AUART = 'H101'
     AND B~LEDNR = '00'
     AND B~GJAHR = P_GJAHR
     AND B~WRTTP = '01'
     AND B~VERSN = '000'.

ENDFORM.                    " SELECT_CO_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAN_DATA
*&---------------------------------------------------------------------*
FORM SELECT_PLAN_DATA.

  CALL FUNCTION 'ZTR_GET_PLAN_MONTHLY'
    EXPORTING
      BUKRS    = P_BUKRS
      GJAHR    = P_GJAHR
      MONTH    = '01'
      PLANDATA = P_R1
      ZTYPE    = C_YEARLY
      SEQNO    = P_SEQNO
    TABLES
      DATE     = DATE
      PLAN     = PLAN.

* Revenue Data Delete
  IF P_R1 = 'X'.
    PERFORM DELETE_PLAN USING 'H11'.

  ENDIF.

  LOOP AT PLAN.
    READ TABLE HIER_DB WITH KEY WERT1 = PLAN-GRUPP.
    IF SY-SUBRC NE 0.
      DELETE PLAN.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SELECT_PLAN_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_TREE_DATA
*&---------------------------------------------------------------------*
FORM GET_TREE_DATA .

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

ENDFORM.                    " GET_TREE_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_PA_DATA
*&---------------------------------------------------------------------*
FORM APPEND_PA_DATA .

  DATA : LT_HIER_DB LIKE HIER_DB       OCCURS 0 WITH HEADER LINE,
         LT_WERT1   LIKE HIER_DB-WERT1 OCCURS 0 WITH HEADER LINE.

* Revenue
*  PERFORM DELETE_PLAN USING 'H111'.

  APPEND 'H111' TO LT_WERT1.
  LOOP AT LT_WERT1.
    READ TABLE HIER_DB INTO LT_HIER_DB WITH KEY WERT1 = LT_WERT1.

    LOOP AT HIER_DB WHERE PARNT = LT_HIER_DB-NODID.
      APPEND HIER_DB TO LT_HIER_DB.
      APPEND HIER_DB-WERT1 TO LT_WERT1.
    ENDLOOP.
  ENDLOOP.

*imsi
***  LOOP AT LT_WERT1.
***    LOOP AT GT_PA WHERE FDGRV = LT_WERT1.
***      CONCATENATE GT_PA-PERBL(4) GT_PA-PERBL+5(2) '01'  INTO PLAN-DATUM.
***      PERFORM GET_LAST_DAY_OF_MONTHS USING PLAN-DATUM PLAN-DATUM.
***
***      PLAN-GRUPP = GT_PA-FDGRV.
***      PLAN-WRSHB =
***      PLAN-DMSHB = GT_PA-VV002001
***                 + GT_PA-VV003001
***                 + GT_PA-VV004001
***                 + GT_PA-VV005001
***                 + GT_PA-VV006001
***                 + GT_PA-VV007001.
***      ICOLLECT PLAN.
***    ENDLOOP.
***  ENDLOOP.


* Expenditure Data Delete
*  PERFORM DELETE_PLAN USING 'H112'.

  LOOP AT GT_PA.
***    CONCATENATE GT_PA-PERBL(4) GT_PA-PERBL+5(2) '01'  INTO PLAN-DATUM.
***    PERFORM GET_LAST_DAY_OF_MONTHS USING PLAN-DATUM PLAN-DATUM.

*   Part(For)
    PLAN-GRUPP = 'C1520'.
***    PLAN-WRSHB = PLAN-DMSHB = GT_PA-VV140001.
    COLLECT PLAN.

*   Part(Dom)
    PLAN-GRUPP = 'C1510'.
***    PLAN-WRSHB = PLAN-DMSHB = GT_PA-VV141001.
    COLLECT PLAN.

*   Coil
    PLAN-GRUPP = 'C1530'.
***    PLAN-WRSHB = PLAN-DMSHB = GT_PA-VV142001 + GT_PA-VV143001.
    COLLECT PLAN.

*   Bulk
    PLAN-GRUPP = 'C1540'.
***    PLAN-WRSHB = PLAN-DMSHB = GT_PA-VV144001 + GT_PA-VV145001.
    COLLECT PLAN.

*   others
    PLAN-GRUPP = 'C1590'.
***    PLAN-WRSHB = PLAN-DMSHB = GT_PA-VV146001 + GT_PA-VV147001.
    COLLECT PLAN.
  ENDLOOP.

  G_PA = 'X'.
ENDFORM.                    " APPEND_PA_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_CCA_DATA
*&---------------------------------------------------------------------*
FORM APPEND_CCA_DATA .

  LOOP AT GT_CCA.
    LOOP AT DATE.
      CONCATENATE 'GT_CCA-WKG0' DATE+4(2) INTO FIELDNAME.
      ASSIGN (FIELDNAME) TO <FS>.
      PLAN-DATUM = DATE.
      IF GT_CCA-FDGRV IS INITIAL.
        PLAN-GRUPP = 'C3900'.
      ELSE.
        PLAN-GRUPP = GT_CCA-FDGRV.
      ENDIF.
      PLAN-WRSHB = PLAN-DMSHB = <FS>.
      ICOLLECT PLAN.
    ENDLOOP.
  ENDLOOP.

  G_CCA = 'X'.
ENDFORM.                    " APPEND_CCA_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_IM_DATA
*&---------------------------------------------------------------------*
FORM APPEND_IM_DATA .

  LOOP AT GT_IM.
    LOOP AT DATE.
      CONCATENATE 'GT_IM-WKG0' DATE+4(2) INTO FIELDNAME.
      ASSIGN (FIELDNAME) TO <FS>.
      PLAN-DATUM = DATE.
      IF GT_IM-FDGRV IS INITIAL.
        PLAN-GRUPP = 'C5900'.
      ELSE.
        PLAN-GRUPP = GT_IM-FDGRV.
      ENDIF.
      PLAN-WRSHB = PLAN-DMSHB = <FS>.
      ICOLLECT PLAN.
    ENDLOOP.
  ENDLOOP.

  G_IM = 'X'.
ENDFORM.                    " APPEND_IM_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_PLAN
*&---------------------------------------------------------------------*
FORM DELETE_PLAN  USING  P_GRUPP.

  DATA : LT_HIER_DB LIKE HIER_DB       OCCURS 0 WITH HEADER LINE,
         LT_WERT1   LIKE HIER_DB-WERT1 OCCURS 0 WITH HEADER LINE,
         L_DATE     LIKE SY-DATUM.

  CONCATENATE P_GJAHR '0101' INTO L_DATE.
  PERFORM GET_LAST_DAY_OF_MONTHS USING L_DATE L_DATE.

  APPEND  P_GRUPP TO LT_WERT1.

  LOOP AT LT_WERT1.
    READ TABLE HIER_DB INTO LT_HIER_DB WITH KEY WERT1 = LT_WERT1.

    LOOP AT HIER_DB WHERE PARNT = LT_HIER_DB-NODID.
      APPEND HIER_DB TO LT_HIER_DB.
      APPEND HIER_DB-WERT1 TO LT_WERT1.
    ENDLOOP.
  ENDLOOP.

  LOOP AT LT_WERT1.
    DELETE PLAN WHERE GRUPP = LT_WERT1
                  AND DATUM >= L_DATE.
  ENDLOOP.

ENDFORM.                    " DELETE_PLAN
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_DAY_OF_MONTHS
*&---------------------------------------------------------------------*
FORM GET_LAST_DAY_OF_MONTHS  USING    F_DATE
                                      T_DATE.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = F_DATE
    IMPORTING
      LAST_DAY_OF_MONTH = T_DATE.

ENDFORM.                    " GET_LAST_DAY_OF_MONTHS
*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
FORM SET_DATE .

  ICLEAR : DATE, GT_DATE.

  CONCATENATE P_GJAHR '0101' INTO DATE.

  DO 12 TIMES.
    DATE = DATE + 1.
    PERFORM GET_LAST_DAY_OF_MONTHS USING DATE DATE.
    APPEND DATE.
  ENDDO.

  SORT DATE.

  LOOP AT DATE.
    GT_DATE-INDEX = GT_DATE-INDEX + 1.
    GT_DATE-DATUM = DATE.
    APPEND GT_DATE.
  ENDLOOP.

  SELECT SINGLE WAERS INTO G_WAERS
    FROM T001
   WHERE BUKRS = P_BUKRS.

ENDFORM.                    " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_BALANCE_DATA
*&---------------------------------------------------------------------*
FORM GET_BALANCE_DATA .

  CALL FUNCTION 'ZTR_GET_BALANCE'
    EXPORTING
      BUKRS     = P_BUKRS
      GJAHR     = P_GJAHR
      MONTH     = '01'
    TABLES
      DATE      = DATE
      DATA      = PLAN
      BEGINNING = BEGINNING
      ENDING    = ENDING.

ENDFORM.                    " GET_BALANCE_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_MANUAL
*&---------------------------------------------------------------------*
FORM APPEND_MANUAL.

  LOOP AT GT_MANUAL.
*    DELETE PLAN WHERE GRUPP = GT_MANUAL-GRUPP.

    LOOP AT DATE.
      CONCATENATE 'GT_MANUAL-DMSHB' DATE+4(2) INTO FIELDNAME.
      ASSIGN (FIELDNAME) TO <FS>.
      PLAN-DATUM = DATE.
      PLAN-GRUPP = GT_MANUAL-GRUPP.
      PLAN-WRSHB = PLAN-DMSHB = <FS>.
      ICOLLECT PLAN.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " APPEND_MANUAL
