*&---------------------------------------------------------------------*
*&  Include           ZFMR0030F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_COMMITMENT_ITEM_GROUP
*&---------------------------------------------------------------------*
FORM GET_COMMITMENT_ITEM_GROUP .

  DATA : LT_NODE LIKE SETNODE OCCURS 0 WITH HEADER LINE.

  ICLEAR : GT_NODE, LT_NODE, GT_LEAF, GT_FMCI.

  SELECT * INTO TABLE LT_NODE
    FROM SETNODE
   WHERE SETCLASS = C_CLASS
     AND SUBCLASS = P_FIKRS
     AND SETNAME  = P_GRPID.

  CHECK SY-SUBRC = 0.
  APPEND LINES OF LT_NODE TO GT_NODE.

  DO.
    SELECT * INTO TABLE LT_NODE
    FROM SETNODE FOR ALL ENTRIES IN LT_NODE
   WHERE SETCLASS = LT_NODE-SUBSETCLS
     AND SUBCLASS = LT_NODE-SUBSETSCLS
     AND SETNAME  = LT_NODE-SUBSETNAME.

    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    APPEND LINES OF LT_NODE TO GT_NODE.
  ENDDO.

  SELECT * INTO TABLE GT_LEAF
    FROM SETLEAF FOR ALL ENTRIES IN GT_NODE
   WHERE SETCLASS = GT_NODE-SUBSETCLS
     AND SUBCLASS = GT_NODE-SUBSETSCLS
     AND SETNAME  = GT_NODE-SUBSETNAME.

  SELECT * INTO TABLE GT_SETHEADERT
    FROM SETHEADERT
   WHERE SETCLASS = C_CLASS
     AND SUBCLASS = P_FIKRS
     AND LANGU = SY-LANGU.

  SELECT FIPEX BEZEI TEXT1 INTO CORRESPONDING FIELDS OF TABLE GT_FMCI
    FROM FMCIT
   WHERE SPRAS = SY-LANGU
     AND FIKRS = P_FIKRS.

ENDFORM.                    " GET_COMMITMENT_ITEM_GROUP
*&---------------------------------------------------------------------*
*&      Form  GET_BUDGET
*&---------------------------------------------------------------------*
FORM GET_BUDGET .

  ICLEAR : GT_9000.

  CALL FUNCTION 'Z_FM_GET_MONTHLY_BUDGET_OTH'
    EXPORTING
      I_FIKRS            = P_FIKRS
      I_GJAHR            = P_GJAHR
    TABLES
      T_GEBER            = S_GEBER
      T_FICTR            = S_FICTR
      T_FIPEX            = S_FIPEX
      T_PROFIL           = S_PROFIL
      T_POTYP            = S_POTYP
      T_ITAB             = GT_9000
    EXCEPTIONS
      NO_FM_AREA         = 1
      NO_FUND            = 2
      NO_FUNDS_CENTER    = 3
      NO_COMMITMENT_ITEM = 4
      NO_PROFILE         = 5
      NO_CATEGORY        = 6
      NO_ORIGINAL        = 7
      OTHERS             = 8.

  CASE SY-SUBRC.
    WHEN 1.
      MESSAGE S000 WITH 'FM Area does not exist.' DISPLAY LIKE 'E'.
      STOP.
    WHEN 2.
      MESSAGE S000 WITH 'Fund does not exist' DISPLAY LIKE 'E'.
      STOP.
    WHEN 3.
      MESSAGE S000 WITH 'Funds Center does not exist' DISPLAY LIKE 'E'.
      STOP.
    WHEN 4.
      MESSAGE S000 WITH 'Commitment Item does not exist' DISPLAY LIKE 'E'.
      STOP.
    WHEN 5.
      MESSAGE S000 WITH 'Profile does not exist for FM' DISPLAY LIKE 'E'.
      STOP.
    WHEN 6.
      MESSAGE S000 WITH 'Item Category does not exist' DISPLAY LIKE 'E'.
      STOP.
    WHEN 7.
      MESSAGE S000 WITH 'Initial budget does not exist' DISPLAY LIKE 'E'.
      STOP.
    WHEN 8.
      MESSAGE S000 WITH 'No data founded' DISPLAY LIKE 'E'.
      STOP.
  ENDCASE.

ENDFORM.                    " GET_BUDGET
*&---------------------------------------------------------------------*
*&      Form  MAKE_ITAB
*&---------------------------------------------------------------------*
FORM MAKE_ITAB.

  ICLEAR : GT_ITAB.

*  PERFORM CLEAR_OVER_MONTH_DATA.
  PERFORM COLLECT_PLAN_ACTUAL.

  LOOP AT GT_LEAF.

    ICLEAR R_FIPEX.

    R_FIPEX-SIGN   = GT_LEAF-VALSIGN.
    R_FIPEX-OPTION = GT_LEAF-VALOPTION.
    R_FIPEX-LOW    = GT_LEAF-VALFROM.
    R_FIPEX-HIGH   = GT_LEAF-VALTO.
    IAPPEND R_FIPEX.

    LOOP AT GT_ITAB WHERE FIPEX IN R_FIPEX.
      GT_ITAB-FIPEX = GT_LEAF-SETNAME.
      PERFORM GET_DESCRIPTION USING GT_ITAB-FIPEX GT_ITAB-BEZEI.
      COLLECT GT_ITAB.
    ENDLOOP.

  ENDLOOP.

  RANGES : R_SETNAME1 FOR  SETNODE-SETNAME,
           R_SETNAME2 FOR  SETNODE-SETNAME.

  LOOP AT GT_LEAF.
    AT NEW SETNAME.
      ICLEAR : R_SETNAME1, R_SETNAME2.

      R_SETNAME1-SIGN   = 'I'.
      R_SETNAME1-OPTION = 'EQ'.
      R_SETNAME1-LOW    = GT_LEAF-SETNAME.
      IAPPEND R_SETNAME1.

      DO.
        LOOP AT GT_NODE WHERE SUBSETNAME IN R_SETNAME1.
          R_SETNAME2-SIGN   = 'I'.
          R_SETNAME2-OPTION = 'EQ'.
          R_SETNAME2-LOW    = GT_NODE-SETNAME.
          IAPPEND R_SETNAME2.

          LOOP AT GT_ITAB WHERE FIPEX = GT_LEAF-SETNAME.
            GT_ITAB-FIPEX = GT_NODE-SETNAME.
            PERFORM GET_DESCRIPTION USING GT_ITAB-FIPEX GT_ITAB-BEZEI.
            COLLECT GT_ITAB.
          ENDLOOP.

        ENDLOOP.

        IF SY-SUBRC NE 0.
          EXIT.
        ENDIF.

        ICLEAR : R_SETNAME1.
        R_SETNAME1[] = R_SETNAME2[].
        ICLEAR : R_SETNAME2.
      ENDDO.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "MAKE_ITAB

*&---------------------------------------------------------------------*
*&      Form  CLEAR_OVER_MONTH_DATA
*&---------------------------------------------------------------------*
FORM CLEAR_OVER_MONTH_DATA .

  DATA : FIELDNAME(20),
         INDEX(2) TYPE N.

  INDEX = '13'.
* Monthly WTP Clear
  DO.
    INDEX = INDEX - 1.
    IF INDEX = P_PERIOD.
      EXIT.
    ENDIF.

    CONCATENATE 'GT_9000-WTP' INDEX INTO FIELDNAME.
    ASSIGN (FIELDNAME) TO <FS>.
    LOOP AT GT_9000.
      <FS> = 0.
      MODIFY GT_9000.
    ENDLOOP.
  ENDDO.

ENDFORM.                    " CLEAR_OVER_MONTH_DATA

*&---------------------------------------------------------------------*
*&      Form  COLLECT_PLAN_ACTUAL
*&---------------------------------------------------------------------*
FORM COLLECT_PLAN_ACTUAL.

  DATA : FIELDNAME(20),
         INDEX(2) TYPE N.

  LOOP AT GT_LEAF.

    ICLEAR R_FIPEX.

    R_FIPEX-SIGN   = GT_LEAF-VALSIGN.
    R_FIPEX-OPTION = GT_LEAF-VALOPTION.
    R_FIPEX-LOW    = GT_LEAF-VALFROM.
    R_FIPEX-HIGH   = GT_LEAF-VALTO.
    IAPPEND R_FIPEX.

    LOOP AT GT_9000 WHERE FIPEX IN R_FIPEX.
      GT_ITAB-FIPEX = GT_9000-FIPEX.
      GT_ITAB-BEZEI = GT_9000-BEZEI.
      GT_ITAB-WAERS = GT_9000-WAERS.

      CASE GT_9000-CTGRY.
        WHEN '05'. "Plan

          CONCATENATE 'GT_9000-WTP' P_PERIOD INTO FIELDNAME.
          ASSIGN (FIELDNAME) TO <FS>.
          GT_ITAB-C_PLAN   = <FS>.
          CLEAR INDEX.
          DO P_PERIOD TIMES.
            ADD 1 TO INDEX .
            CONCATENATE 'GT_9000-WTP' INDEX INTO FIELDNAME.
            ASSIGN (FIELDNAME) TO <FS>.
            GT_ITAB-T_PLAN = GT_ITAB-T_PLAN + <FS>.
          ENDDO.

*          GT_ITAB-T_PLAN   = GT_9000-WTP01
*                           + GT_9000-WTP02
*                           + GT_9000-WTP03
*                           + GT_9000-WTP04
*                           + GT_9000-WTP05
*                           + GT_9000-WTP06
*                           + GT_9000-WTP07
*                           + GT_9000-WTP08
*                           + GT_9000-WTP09
*                           + GT_9000-WTP10
*                           + GT_9000-WTP11
*                           + GT_9000-WTP12.

          GT_ITAB-C_DIFF   = GT_ITAB-C_PLAN.
          GT_ITAB-T_DIFF   = GT_ITAB-T_PLAN.

        WHEN '11' OR '12' OR '13'. "Actual
          CONCATENATE 'GT_9000-WTP' P_PERIOD INTO FIELDNAME.
          ASSIGN (FIELDNAME) TO <FS>.
          GT_ITAB-C_ACTUAL = <FS>.

          CLEAR INDEX.
          DO P_PERIOD TIMES.
            ADD 1 TO INDEX .
            CONCATENATE 'GT_9000-WTP' INDEX INTO FIELDNAME.
            ASSIGN (FIELDNAME) TO <FS>.
            GT_ITAB-T_ACTUAL = GT_ITAB-T_ACTUAL + <FS>.
          ENDDO.

*          GT_ITAB-T_ACTUAL = GT_9000-WTP01
*                           + GT_9000-WTP02
*                           + GT_9000-WTP03
*                           + GT_9000-WTP04
*                           + GT_9000-WTP05
*                           + GT_9000-WTP06
*                           + GT_9000-WTP07
*                           + GT_9000-WTP08
*                           + GT_9000-WTP09
*                           + GT_9000-WTP10
*                           + GT_9000-WTP11
*                           + GT_9000-WTP12.
          GT_ITAB-C_ACTUAL = - GT_ITAB-C_ACTUAL.
          GT_ITAB-T_ACTUAL = - GT_ITAB-T_ACTUAL.
          GT_ITAB-C_DIFF   = - GT_ITAB-C_ACTUAL.
          GT_ITAB-T_DIFF   = - GT_ITAB-T_ACTUAL.
      ENDCASE.
      ICOLLECT GT_ITAB.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      LOOP AT GT_FMCI WHERE FIPEX IN R_FIPEX.
        GT_ITAB-FIPEX = GT_FMCI-FIPEX.
        IF GT_FMCI-TEXT1 IS NOT INITIAL.
          GT_ITAB-BEZEI = GT_FMCI-TEXT1.
        ELSE.
          GT_ITAB-BEZEI = GT_FMCI-BEZEI.
        ENDIF.
        GT_ITAB-WAERS = 'USD'.
        ICOLLECT GT_ITAB.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " COLLECT_PLAN_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  MAKE_TREE_DATA
*&---------------------------------------------------------------------*
FORM MAKE_TREE_DATA .

  ICLEAR : GT_NODE_TABLE, GT_ITEM_TABLE.

  GT_NODE_TABLE-NODE_KEY  = P_GRPID.
  GT_NODE_TABLE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  GT_NODE_TABLE-ISFOLDER  = 'X'.
  IAPPEND GT_NODE_TABLE.

*NAME
  GT_ITEM_TABLE-NODE_KEY  = P_GRPID.
  GT_ITEM_TABLE-ITEM_NAME = C_COL-C01.
  GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  PERFORM GET_DESCRIPTION USING GT_ITEM_TABLE-NODE_KEY GT_ITEM_TABLE-TEXT.
  IAPPEND GT_ITEM_TABLE.

*CODE
  GT_ITEM_TABLE-NODE_KEY  = P_GRPID.
  GT_ITEM_TABLE-ITEM_NAME = C_COL-C02.
  GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  GT_ITEM_TABLE-TEXT      = P_GRPID.
  IAPPEND GT_ITEM_TABLE.

  LOOP AT GT_NODE.

    GT_NODE_TABLE-NODE_KEY  = GT_NODE-SUBSETNAME.
    GT_NODE_TABLE-RELATKEY  = GT_NODE-SETNAME. "PARENT ID
    GT_NODE_TABLE-ISFOLDER  = 'X'.

    GT_NODE_TABLE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
    IAPPEND GT_NODE_TABLE.

*NAME
    GT_ITEM_TABLE-NODE_KEY  = GT_NODE-SUBSETNAME.
    GT_ITEM_TABLE-ITEM_NAME = C_COL-C01.
    GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    PERFORM GET_DESCRIPTION USING GT_ITEM_TABLE-NODE_KEY GT_ITEM_TABLE-TEXT.
    IAPPEND GT_ITEM_TABLE.

*CODE
    GT_ITEM_TABLE-NODE_KEY  = GT_NODE-SUBSETNAME.
    GT_ITEM_TABLE-ITEM_NAME = C_COL-C02.
    GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    GT_ITEM_TABLE-TEXT      = GT_NODE-SUBSETNAME.
    IAPPEND GT_ITEM_TABLE.
  ENDLOOP.

  LOOP AT GT_LEAF.

    ICLEAR R_FIPEX.

    R_FIPEX-SIGN   = GT_LEAF-VALSIGN.
    R_FIPEX-OPTION = GT_LEAF-VALOPTION.
    R_FIPEX-LOW    = GT_LEAF-VALFROM.
    R_FIPEX-HIGH   = GT_LEAF-VALTO.
    IAPPEND R_FIPEX.

    LOOP AT GT_FMCI WHERE FIPEX IN R_FIPEX.

      GT_NODE_TABLE-NODE_KEY  = GT_FMCI-FIPEX.
      GT_NODE_TABLE-RELATKEY  = GT_LEAF-SETNAME. "PARENT ID
      GT_NODE_TABLE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
      IAPPEND GT_NODE_TABLE.

*NAME
      GT_ITEM_TABLE-NODE_KEY  = GT_FMCI-FIPEX.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C01.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.

      IF GT_FMCI-TEXT1 IS NOT INITIAL.
        GT_ITEM_TABLE-TEXT      = GT_FMCI-TEXT1.
      ELSE.
        GT_ITEM_TABLE-TEXT      = GT_FMCI-BEZEI.
      ENDIF.
      IAPPEND GT_ITEM_TABLE.

*CODE
      GT_ITEM_TABLE-NODE_KEY  = GT_FMCI-FIPEX.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C02.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      GT_ITEM_TABLE-TEXT      = GT_FMCI-FIPEX.
      IAPPEND GT_ITEM_TABLE.

    ENDLOOP.
  ENDLOOP.


  LOOP AT GT_NODE_TABLE.
    LOOP AT GT_ITAB WHERE FIPEX = GT_NODE_TABLE-NODE_KEY.

*Current ACTUAL
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C03.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING GT_ITAB-C_ACTUAL GT_ITAB-WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Current PLAN
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C04.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING GT_ITAB-C_PLAN GT_ITAB-WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Current DIFF
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C05.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING GT_ITAB-C_DIFF GT_ITAB-WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Total ACTUAL
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C06.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING GT_ITAB-T_ACTUAL GT_ITAB-WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Total PLAN
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C07.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING GT_ITAB-T_PLAN GT_ITAB-WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Total DIFF
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C08.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING GT_ITAB-T_DIFF GT_ITAB-WAERS
                                 GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.
    ENDLOOP.

    IF SY-SUBRC NE 0.
*Current PLAN
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C03.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING '0.00' 'USD' GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Current ACTUAL
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C04.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING '0.00' 'USD' GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Current DIFF
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C05.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING '0.00' 'USD' GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Total PLAN
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C06.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING '0.00' 'USD' GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Total ACTUAL
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C07.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING '0.00' 'USD' GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.

*Total DIFF
      GT_ITEM_TABLE-NODE_KEY  = GT_NODE_TABLE-NODE_KEY.
      GT_ITEM_TABLE-ITEM_NAME = C_COL-C08.
      GT_ITEM_TABLE-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
      PERFORM WRITE_AMOUNT USING '0.00' 'USD' GT_ITEM_TABLE-TEXT.
      IAPPEND GT_ITEM_TABLE.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MAKE_TREE_DATA
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS OUTPUT.

  SET PF-STATUS 'S9000'.
  SET TITLEBAR  'T9000'." WITH P_PERIOD P_GJAHR P_GRPID.

ENDMODULE.                 " SET_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_SCREEN OUTPUT.

  IF G_CUSTOM_CONTAINER IS INITIAL.
    PERFORM CREATE_CONTAINER.
    PERFORM CREATE_COLUMN.
    PERFORM DISPLAY_TREE_ALV.
  ENDIF.

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

* Tree Column
  GS_HIERARCHY_HEADER-HEADING = 'Code'.
  GS_HIERARCHY_HEADER-WIDTH   = 50.

  CREATE OBJECT G_GRID
    EXPORTING
      PARENT                      = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE         = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION              = 'X'
      HIERARCHY_COLUMN_NAME       = C_COL-C01
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
  PERFORM ADD_COLUMN USING C_COL-C02 '20' 'L' 'Name'.
  PERFORM ADD_COLUMN USING C_COL-C03 '20' 'R' 'Current Actual'.
  PERFORM ADD_COLUMN USING C_COL-C04 '20' 'R' 'Current Plan'.
  PERFORM ADD_COLUMN USING C_COL-C05 '20' 'R' 'Current Difference'.
  PERFORM ADD_COLUMN USING C_COL-C06 '20' 'R' 'YTD Actual'.
  PERFORM ADD_COLUMN USING C_COL-C07 '20' 'R' 'YTD Plan'.
  PERFORM ADD_COLUMN USING C_COL-C08 '20' 'R' 'YTD Difference'.

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
*
* Top Node ? Expand ??.
  READ TABLE GT_NODE_TABLE WITH KEY NODE_KEY = P_GRPID.
  CALL METHOD G_GRID->EXPAND_NODE
    EXPORTING
      NODE_KEY            = GT_NODE_TABLE-NODE_KEY
      LEVEL_COUNT         = 1
      EXPAND_SUBTREE      = SPACE
    EXCEPTIONS
      FAILED              = 1
      ILLEGAL_LEVEL_COUNT = 2
      CNTL_SYSTEM_ERROR   = 3
      NODE_NOT_FOUND      = 4
      CANNOT_EXPAND_LEAF  = 5.

  IF SY-SUBRC <> 0.
    MESSAGE A000(TREE_CONTROL_MSG).
  ENDIF.

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
*&      Form  EXPAND_NODE
*&---------------------------------------------------------------------*
FORM EXPAND_NODE .

  CLEAR   GT_NODE_TABLE-NODE_KEY.
  PERFORM GET_SELECTED_ITEM.
  PERFORM GET_SELECTED_NODE.
  PERFORM SET_EXPAND_NODE.

ENDFORM.                    " EXPAND_NODE
*&---------------------------------------------------------------------*
*&      Form  COLLAPSE_NODE
*&---------------------------------------------------------------------*
FORM COLLAPSE_NODE .

  CLEAR   GT_NODE_TABLE-NODE_KEY.
  PERFORM GET_SELECTED_ITEM.
  PERFORM GET_SELECTED_NODE.
  PERFORM COLLAPSE_SUBTREE.

ENDFORM.                    " COLLAPSE_NODE
*&---------------------------------------------------------------------*
*&      Form  get_selected_item
*&---------------------------------------------------------------------*
FORM GET_SELECTED_ITEM .

  CALL METHOD G_GRID->GET_SELECTED_ITEM
    IMPORTING
      NODE_KEY          = GT_NODE_TABLE-NODE_KEY
    EXCEPTIONS
      FAILED            = 1
      CNTL_SYSTEM_ERROR = 2
      NO_ITEM_SELECTION = 3.

ENDFORM.                    " get_selected_item
*&---------------------------------------------------------------------*
*&      Form  get_selected_node
*&---------------------------------------------------------------------*
FORM GET_SELECTED_NODE .

  IF GT_NODE_TABLE-NODE_KEY IS INITIAL.
    CALL METHOD G_GRID->GET_SELECTED_NODE
      IMPORTING
        NODE_KEY                   = GT_NODE_TABLE-NODE_KEY
      EXCEPTIONS
        FAILED                     = 1
        SINGLE_NODE_SELECTION_ONLY = 2
        CNTL_SYSTEM_ERROR          = 3.
  ENDIF.

ENDFORM.                    " get_selected_node
*&---------------------------------------------------------------------*
*&      Form  set_expand_node
*&---------------------------------------------------------------------*
FORM SET_EXPAND_NODE .

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

ENDFORM.                    " set_expand_node
*&---------------------------------------------------------------------*
*&      Form  collapse_subtree
*&---------------------------------------------------------------------*
FORM COLLAPSE_SUBTREE .

  IF NOT GT_NODE_TABLE-NODE_KEY IS INITIAL.
    CALL METHOD G_GRID->COLLAPSE_SUBTREE
      EXPORTING
        NODE_KEY          = GT_NODE_TABLE-NODE_KEY
      EXCEPTIONS
        FAILED            = 1
        NODE_NOT_FOUND    = 2
        CNTL_SYSTEM_ERROR = 3.
  ENDIF.

ENDFORM.                    " collapse_subtree
*&---------------------------------------------------------------------*
*&      Form  GET_DESCRIPTION
*&---------------------------------------------------------------------*
FORM GET_DESCRIPTION  USING    P_NODE_KEY
                               P_TEXT.
  CLEAR GT_SETHEADERT.
  READ TABLE GT_SETHEADERT WITH KEY SETNAME = P_NODE_KEY.
  P_TEXT = GT_SETHEADERT-DESCRIPT.

ENDFORM.                    " GET_DESCRIPTION
*&---------------------------------------------------------------------*
*&      Form  WRITE_AMOUNT
*&---------------------------------------------------------------------*
FORM WRITE_AMOUNT  USING    P_AMOUNT
                            P_WAERS
                            P_TEXT.
  DATA : TEXT(20).

  WRITE P_AMOUNT TO TEXT CURRENCY P_WAERS.

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
*&      Form  DATA_DOWN
*&---------------------------------------------------------------------*
FORM DATA_DOWN.

  ICLEAR   : GT_DOWN.

  PERFORM GET_EXPANDED_NODES.
  PERFORM APPEND_DOWN USING P_GRPID 0.
  PERFORM MAKE_DOWN_DATA.
  PERFORM SAVE_FILE.

ENDFORM.                    "DATA_DOWN
*&---------------------------------------------------------------------*
*&      Form  DATA_PRINT
*&---------------------------------------------------------------------*
FORM DATA_PRINT.

  DATA : MLINE TYPE I,
         FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH : GT_DOWN.
  CLEAR   : GT_DOWN.

  PERFORM GET_EXPANDED_NODES.
  PERFORM APPEND_DOWN USING P_GRPID 0.

  DATA: PRINT_PARAMETERS TYPE PRI_PARAMS,
        VALID_FLAG       TYPE C LENGTH 1.

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      DESTINATION          = 'LOCL'
      LAYOUT               = 'ZL_60_180'
    IMPORTING
      OUT_PARAMETERS       = PRINT_PARAMETERS
      VALID                = VALID_FLAG
    EXCEPTIONS
      INVALID_PRINT_PARAMS = 2
      OTHERS               = 4.

  CHECK VALID_FLAG = 'X'.

  NEW-PAGE NO-HEADING NO-TITLE LINE-SIZE 180 LINE-COUNT 60
                               PRINT ON NO DIALOG.

  LOOP AT GT_DOWN.
    IF SY-LINNO > 58.
      WRITE :/(C_SIZE) SY-ULINE.
      CLEAR MLINE.
      NEW-PAGE.
    ENDIF.
    FORMAT COLOR = 4.
    WRITE :/ SY-VLINE NO-GAP,
             (40) GT_DOWN-BEZEI    NO-GAP,
             SY-VLINE NO-GAP, (11) GT_DOWN-FIPEX   NO-GAP.
    FORMAT COLOR OFF.
    WRITE : SY-VLINE NO-GAP, (18) GT_DOWN-C_ACTUAL NO-GAP RIGHT-JUSTIFIED,
            SY-VLINE NO-GAP, (18) GT_DOWN-C_PLAN   NO-GAP RIGHT-JUSTIFIED,
            SY-VLINE NO-GAP, (18) GT_DOWN-C_DIFF   NO-GAP RIGHT-JUSTIFIED,
            SY-VLINE NO-GAP, (18) GT_DOWN-T_ACTUAL NO-GAP RIGHT-JUSTIFIED,
            SY-VLINE NO-GAP, (18) GT_DOWN-T_PLAN   NO-GAP RIGHT-JUSTIFIED,
            SY-VLINE NO-GAP, (18) GT_DOWN-T_DIFF   NO-GAP RIGHT-JUSTIFIED,
            SY-VLINE NO-GAP.
    ADD 1 TO MLINE.
    IF MLINE = 5.
      WRITE :/(C_SIZE) SY-ULINE.
      CLEAR MLINE.
    ENDIF.
  ENDLOOP.
  IF MLINE NE 0.
    WRITE :/(C_SIZE) SY-ULINE.
  ENDIF.

  NEW-PAGE PRINT OFF.
ENDFORM.                    "DATA_PRINT
*&---------------------------------------------------------------------*
*&      Form  STRING_REPLACE
*&---------------------------------------------------------------------*
FORM STRING_REPLACE  USING    F_AMT
                              T_AMT.
  T_AMT = F_AMT.
* (-)?? ?? ???
  SEARCH T_AMT FOR '-'.

  IF SY-SUBRC = 0.
    CALL FUNCTION 'STRING_REPLACE'
      EXPORTING
        PATTERN             = '-'
        SUBSTITUTE          = ''
      CHANGING
        TEXT                = T_AMT
      EXCEPTIONS
        WRONG_STRING_LENGTH = 1
        OTHERS              = 2.

    SHIFT T_AMT LEFT DELETING LEADING SPACE.
    CONCATENATE '-' T_AMT INTO T_AMT.
  ENDIF.

ENDFORM.                    " STRING_REPLACE
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
*&      Form  APPEND_DOWN
*&---------------------------------------------------------------------*
FORM APPEND_DOWN  USING    P_NODE P_LEVEL.

  DATA : LT_NODE LIKE GT_NODE OCCURS 0 WITH HEADER LINE,
         L_LEVEL TYPE I.

  LT_NODE[] = GT_NODE[].
  L_LEVEL = P_LEVEL.

  READ TABLE GT_ITAB WITH KEY FIPEX = P_NODE.
  CHECK SY-SUBRC = 0.
  GT_DOWN-BEZEI    = GT_ITAB-BEZEI.
  GT_DOWN-FIPEX    = GT_ITAB-FIPEX.
  GT_DOWN-C_PLAN   = GT_ITAB-C_PLAN.
  GT_DOWN-C_ACTUAL = GT_ITAB-C_ACTUAL.
  GT_DOWN-C_DIFF   = GT_ITAB-C_DIFF.
  GT_DOWN-T_PLAN   = GT_ITAB-T_PLAN.
  GT_DOWN-T_ACTUAL = GT_ITAB-T_ACTUAL.
  GT_DOWN-T_DIFF   = GT_ITAB-T_DIFF.

  READ TABLE LT_NODE WITH KEY SETNAME = P_NODE.
  IF SY-SUBRC = 0.
    DO L_LEVEL TIMES.
      SHIFT GT_DOWN-BEZEI RIGHT BY 2 PLACES .
    ENDDO.
  ELSE.
    READ TABLE GT_LEAF WITH KEY SETNAME = P_NODE.
    IF GT_DOWN-BEZEI IS INITIAL.
      PERFORM GET_DESCRIPTION USING P_NODE GT_DOWN-BEZEI.
    ENDIF.
    DO L_LEVEL TIMES.
      SHIFT GT_DOWN-BEZEI RIGHT BY 2 PLACES .
    ENDDO.
  ENDIF.

  APPEND GT_DOWN .
  CLEAR  GT_DOWN .

  IF G_NODE_KEY_TABLE[] IS NOT INITIAL.
    READ TABLE G_NODE_KEY_TABLE INTO G_NODE_KEY_TABLE
      WITH KEY TABLE_LINE = P_NODE.
    CHECK SY-SUBRC = 0.
  ENDIF.

  ADD 1 TO L_LEVEL.
  LOOP AT LT_NODE WHERE SETNAME = P_NODE.
    PERFORM APPEND_DOWN USING LT_NODE-SUBSETNAME L_LEVEL.
  ENDLOOP.

  IF SY-SUBRC NE 0.

    LOOP AT GT_LEAF WHERE SETNAME = P_NODE.
      ICLEAR R_FIPEX.

      R_FIPEX-SIGN   = GT_LEAF-VALSIGN.
      R_FIPEX-OPTION = GT_LEAF-VALOPTION.
      R_FIPEX-LOW    = GT_LEAF-VALFROM.
      R_FIPEX-HIGH   = GT_LEAF-VALTO.
      IAPPEND R_FIPEX.

      LOOP AT GT_ITAB WHERE FIPEX IN R_FIPEX.
        PERFORM APPEND_DOWN USING GT_ITAB-FIPEX L_LEVEL.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " APPEND_DOWN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DOWN_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DOWN_DATA .

  DATA :  GT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_INTERNAL_TABNAME     = 'GT_DOWN'
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  LOOP AT GT_FIELDCAT.
    CASE GT_FIELDCAT-FIELDNAME.
      WHEN 'BEZEI'.
        GT_FIELDCAT-SELTEXT_L = 'Name'.
        GT_FIELDCAT-OUTPUTLEN = '38'.
        GT_FIELDCAT-KEY       = 'X'.
      WHEN 'FIPEX'.
        GT_FIELDCAT-SELTEXT_L = 'Code'.
        GT_FIELDCAT-OUTPUTLEN = '12'.
        GT_FIELDCAT-KEY       = 'X'.
      WHEN 'C_PLAN'.
        GT_FIELDCAT-SELTEXT_L = 'Current Plan'.
        GT_FIELDCAT-OUTPUTLEN = '18'.
      WHEN 'C_ACTUAL'.
        GT_FIELDCAT-SELTEXT_L = 'Current Actual'.
        GT_FIELDCAT-OUTPUTLEN = '18'.
      WHEN 'C_DIFF'.
        GT_FIELDCAT-SELTEXT_L = 'Current Difference'.
        GT_FIELDCAT-OUTPUTLEN = '18'.
      WHEN 'T_PLAN'.
        GT_FIELDCAT-SELTEXT_L = 'YTD Plan'.
        GT_FIELDCAT-OUTPUTLEN = '18'.
      WHEN 'T_ACTUAL'.
        GT_FIELDCAT-SELTEXT_L = 'YTD Actual'.
        GT_FIELDCAT-OUTPUTLEN = '18'.
      WHEN 'T_DIFF'.
        GT_FIELDCAT-SELTEXT_L = 'YTD Difference'.
        GT_FIELDCAT-OUTPUTLEN = '18'.
    ENDCASE.

    GT_FIELDCAT-SELTEXT_S =
    GT_FIELDCAT-SELTEXT_M =
    GT_FIELDCAT-SELTEXT_L.
    MODIFY GT_FIELDCAT.
  ENDLOOP.

*  DATA IS_LAYOUT TYPE  SLIS_LAYOUT_ALV.
*  IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  DATA : IS_PRINT      TYPE SLIS_PRINT_ALV,
         LS_PRINT_CTRL TYPE ALV_S_PCTL,
         LS_PRI_PARAMS TYPE PRI_PARAMS.
*printer
  LS_PRI_PARAMS-PDEST      = 'LOCL'.
  LS_PRI_PARAMS-PAART      = 'ZL_60_180'.
  LS_PRINT_CTRL-PRI_PARAMS = LS_PRI_PARAMS.
  IS_PRINT-PRINT_CTRL = LS_PRINT_CTRL.


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IS_PRINT           = IS_PRINT
*      IS_LAYOUT          = IS_LAYOUT
      IT_FIELDCAT        = GT_FIELDCAT[]
    TABLES
      T_OUTTAB           = GT_DOWN.

ENDFORM.                    " DISPLAY_DOWN_DATA
*&---------------------------------------------------------------------*
*&      Form  user_auth_check_geber
*&---------------------------------------------------------------------*
FORM USER_AUTH_CHECK_GEBER  TABLES   P_S_FICTR
                            USING    P_P_FIKRS.
  DATA : LT_FICTR LIKE FMFCTR OCCURS 0 WITH HEADER LINE.
  CLEAR : G_AUTH_CHECK.
  SELECT * FROM FMFCTR
           INTO TABLE LT_FICTR
           WHERE FIKRS = P_P_FIKRS
             AND FICTR IN P_S_FICTR.
  LOOP AT LT_FICTR .
    IF LT_FICTR-FICTR = 'G9990' OR
       LT_FICTR-FICTR = 'G9999'.
      CONTINUE.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'F_FICA_CTR'
             ID 'FM_AUTHACT' FIELD '03'
             ID 'FM_FIKRS' FIELD P_P_FIKRS
             ID 'FM_FICTR' FIELD LT_FICTR-FICTR.

    IF SY-SUBRC NE 0.
      MESSAGE S019(ZMFI) WITH LT_FICTR-FICTR .
      G_AUTH_CHECK = 'E'.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF LINES( LT_FICTR ) = 1.
    LOOP AT LT_FICTR.
      SELECT SINGLE FICTR BEZEICH
        INTO (P_FICTR, P_BEZEICH)
        FROM FMFCTRT
       WHERE SPRAS = SY-LANGU
         AND FIKRS = P_FIKRS
         AND FICTR = LT_FICTR-FICTR
         AND DATBIS >= SY-DATUM.
    ENDLOOP.
  ELSE.
    P_FICTR   = 'Multi'.
  ENDIF.

ENDFORM.                    " user_auth_check_geber
*&---------------------------------------------------------------------*
*&      Form  CALL_ZRFFMEPGAX
*&---------------------------------------------------------------------*
FORM CALL_ZRFFMEPGAX USING P_NODE_KEY
                           P_ITEM_NAME
                           P_FR
                           P_TO.

  PERFORM GET_COMMITMENT_ITEM_RANGE USING  P_NODE_KEY
                                           P_ITEM_NAME
                                           P_FR
                                           P_TO.


  DATA : P_PER_FR(2), P_PER_TO(2).

  ICLEAR : R_WRTTP.
  CLEAR  : GT_ITAB.

  READ TABLE GT_NODE_TABLE INTO GT_NODE_TABLE
  WITH KEY NODE_KEY = P_NODE_KEY.

  P_PER_FR = P_FR.
  P_PER_TO = P_TO.

  CASE P_ITEM_NAME.
    WHEN C_COL-C03 OR C_COL-C06. "Actual
      R_WRTTP-SIGN   = 'I'.
      R_WRTTP-OPTION = 'EQ'.
      R_WRTTP-LOW    = '54'.
      INSERT R_WRTTP INTO TABLE R_WRTTP.
      R_WRTTP-LOW    = '65'.
      INSERT R_WRTTP INTO TABLE R_WRTTP.
      R_WRTTP-LOW    = '66'.
      INSERT R_WRTTP INTO TABLE R_WRTTP.
      R_WRTTP-LOW    = '57'.
      INSERT R_WRTTP INTO TABLE R_WRTTP.
      R_WRTTP-LOW    = '64'.
      INSERT R_WRTTP INTO TABLE R_WRTTP.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

*  IF GT_NODE_TABLE-ISFOLDER  = 'X'.
*    SUBMIT ZRFFMEPGAX
*       WITH S_FIKRS-LOW = P_FIKRS
*       WITH P_FYR_FR    = P_GJAHR
*       WITH P_PER_FR    = P_PER_FR
*       WITH P_FYR_TO    = P_GJAHR
*       WITH P_PER_TO    = P_PER_TO
*       WITH P_CI_GRP    = P_NODE_KEY
*       WITH P_MAXSEL    = ' '
*       WITH S_FICTR    IN S_FICTR
*       WITH S_WRTTP    IN R_WRTTP
*       AND RETURN.
*  ELSE.
*    R_FIPEX-SIGN   = 'I'.
*    R_FIPEX-OPTION = 'EQ'.
*    R_FIPEX-LOW    = P_NODE_KEY.
*    INSERT R_FIPEX INTO TABLE R_FIPEX.

  SUBMIT ZRFFMEPGAX
    WITH S_FIKRS-LOW = P_FIKRS
    WITH P_FYR_FR    = P_GJAHR
    WITH P_PER_FR    = P_PER_FR
    WITH P_FYR_TO    = P_GJAHR
    WITH P_PER_TO    = P_PER_TO
    WITH P_MAXSEL    = ' '
    WITH S_FIPEX    IN R_FIPEX
    WITH S_FICTR    IN S_FICTR
    WITH S_WRTTP    IN R_WRTTP
    AND RETURN.
*  ENDIF.

ENDFORM.                    " CALL_ZRFFMEPGAX
*&---------------------------------------------------------------------*
*&      Form  CALL_PLAN_LIST
*&---------------------------------------------------------------------*
FORM CALL_PLAN_LIST  USING    P_NODE_KEY
                              P_ITEM_NAME
                              P_FR
                              P_TO.

  PERFORM GET_COMMITMENT_ITEM_RANGE USING  P_NODE_KEY
                                           P_ITEM_NAME
                                           P_FR
                                           P_TO.

  DATA : LT_9000  LIKE GT_9000    OCCURS 0 WITH HEADER LINE,
         FIELDNAME(20),
         INDEX(2) TYPE N.

  LOOP AT GT_9000 WHERE FIPEX IN R_FIPEX
                    AND CTGRY <= '06'.

    LT_9000 = GT_9000.

    CLEAR : LT_9000-GEBER,
            LT_9000-FICTR,
            LT_9000-FIPEX,
            LT_9000-BEZEI,
            LT_9000-KTEXT,
            LT_9000-PROFIL,
            LT_9000-POTYP,
            LT_9000-TOTAL.

    IF P_FR = P_TO.
      CONCATENATE 'GT_9000-WTP' P_PERIOD INTO FIELDNAME.
      ASSIGN (FIELDNAME) TO <FS>.
      LT_9000-TOTAL  = <FS>.
    ELSE.
      CLEAR INDEX.
      DO P_PERIOD TIMES.
        ADD 1 TO INDEX .
        CONCATENATE 'GT_9000-WTP' INDEX INTO FIELDNAME.
        ASSIGN (FIELDNAME) TO <FS>.
        LT_9000-TOTAL = LT_9000-TOTAL + <FS>.
      ENDDO.
    ENDIF.
    ICOLLECT LT_9000.
  ENDLOOP.

  IF SY-SUBRC NE 0.
    MESSAGE S001.
    EXIT.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_STRUCTURE_NAME       = 'ZSFM0008'
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  LOOP AT GT_FIELDCAT.
    CASE GT_FIELDCAT-FIELDNAME.
      WHEN 'CTTXT'.
        GT_FIELDCAT-SELTEXT_L = 'Category'.
        GT_FIELDCAT-KEY       = 'X'.
      WHEN 'TOTAL'.
        GT_FIELDCAT-SELTEXT_L = 'Total'.
        GT_FIELDCAT-KEY       = 'X'.
      WHEN 'WTP01'.
        GT_FIELDCAT-SELTEXT_L = 'Jan'.
      WHEN 'WTP02'.
        GT_FIELDCAT-SELTEXT_L = 'Feb'.
      WHEN 'WTP03'.
        GT_FIELDCAT-SELTEXT_L = 'Mar'.
      WHEN 'WTP04'.
        GT_FIELDCAT-SELTEXT_L = 'Apl'.
      WHEN 'WTP05'.
        GT_FIELDCAT-SELTEXT_L = 'May'.
      WHEN 'WTP06'.
        GT_FIELDCAT-SELTEXT_L = 'Jun'.
      WHEN 'WTP07'.
        GT_FIELDCAT-SELTEXT_L = 'Jul'.
      WHEN 'WTP08'.
        GT_FIELDCAT-SELTEXT_L = 'Aug'.
      WHEN 'WTP09'.
        GT_FIELDCAT-SELTEXT_L = 'Sep'.
      WHEN 'WTP10'.
        GT_FIELDCAT-SELTEXT_L = 'Oct'.
      WHEN 'WTP11'.
        GT_FIELDCAT-SELTEXT_L = 'Nov'.
      WHEN 'WTP12'.
        GT_FIELDCAT-SELTEXT_L = 'Dec'.
      WHEN OTHERS.
        GT_FIELDCAT-NO_OUT = 'X'.
    ENDCASE.

    GT_FIELDCAT-SELTEXT_S =
    GT_FIELDCAT-SELTEXT_M =
    GT_FIELDCAT-SELTEXT_L.
    MODIFY GT_FIELDCAT.
  ENDLOOP.
  PERFORM LAYOUT_BUILD       USING  GS_LAYOUT.
  PERFORM LISTHEADER_BUILD.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      IS_LAYOUT              = GS_LAYOUT
      I_CALLBACK_TOP_OF_PAGE = 'TOP_OF_PAGE'
      IT_FIELDCAT            = GT_FIELDCAT[]
    TABLES
      T_OUTTAB               = LT_9000.

ENDFORM.                    " CALL_PLAN_LIST
*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = GT_LIST_TOP.

ENDFORM.                    " TOP-OF-PAGE

*&---------------------------------------------------------------------*
*&      Form  listheader_build
*&---------------------------------------------------------------------*
FORM LISTHEADER_BUILD.
  DATA : L_LINE_S  TYPE SLIS_LISTHEADER.

  REFRESH   GT_LIST_TOP.
  L_LINE_S-TYP  = 'S'.
  L_LINE_S-KEY  = 'Name : '.
  L_LINE_S-INFO = GT_ITAB-BEZEI.
  APPEND L_LINE_S  TO GT_LIST_TOP.

  L_LINE_S-TYP  = 'S'.
  L_LINE_S-KEY  = 'Code : '.
  L_LINE_S-INFO = GT_ITAB-FIPEX.
  APPEND L_LINE_S  TO GT_LIST_TOP.

ENDFORM.                    " listheader_build
*&---------------------------------------------------------------------*
*&      Form  layout_build
*&---------------------------------------------------------------------*
FORM LAYOUT_BUILD USING P_LAYOUT TYPE SLIS_LAYOUT_ALV.

*___Display options
  P_LAYOUT-ZEBRA             = 'X'.
  P_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  P_LAYOUT-CELL_MERGE        = 'X'.
*  P_LAYOUT-BOX_FIELDNAME     = 'CHK'.
*___Detailed screen
  P_LAYOUT-DETAIL_POPUP      = 'X'.
  P_LAYOUT-DETAIL_TITLEBAR   = SY-TITLE.

ENDFORM.                    " layout_build
*&---------------------------------------------------------------------*
*&      Form  GET_COMMITMENT_ITEM_RANGE
*&---------------------------------------------------------------------*
FORM GET_COMMITMENT_ITEM_RANGE  USING    P_NODE_KEY
                                         P_ITEM_NAME
                                         P_FR
                                         P_TO.

  ICLEAR R_FIPEX.

  DATA : L_SETID      LIKE SETHIER-SETID,
         L_INFO       LIKE GRPHINFO,
         L_OVERWRITE  LIKE SY-DATAR,
         LT_NODES     LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE,
         LT_VALUES    LIKE GRPVALUES  OCCURS 0 WITH HEADER LINE.

  READ TABLE GT_ITAB WITH KEY FIPEX = P_NODE_KEY.

  CHECK SY-SUBRC = 0.

  CONCATENATE '0311' P_FIKRS P_NODE_KEY  INTO  L_SETID.
  CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
    EXPORTING
      E_CLASS                     = '0311'
      E_SETID                     = L_SETID
      E_KOKRS                     = P_FIKRS
    TABLES
      T_NODES                     = LT_NODES
      T_VALUES                    = LT_VALUES
    CHANGING
      C_INFO                      = L_INFO
      C_OVERWRITE                 = L_OVERWRITE
    EXCEPTIONS
      NO_CONTROLLING_AREA         = 1
      NO_CHART_OF_ACCOUNT         = 2
      DIFFERENT_CONTROLLING_AREAS = 3
      DIFFERENT_CHART_OF_ACCOUNTS = 4
      SET_NOT_FOUND               = 5
      ILLEGAL_FIELD_REPLACEMENT   = 6
      ILLEGAL_TABLE_REPLACEMENT   = 7
      FM_RAISE                    = 8
      CONVERT_ERROR               = 9
      NO_OVERWRITE_STANDARD_HIER  = 10
      NO_BUKRS_FOR_KOKRS          = 11
      OTHERS                      = 12.


  LOOP AT LT_VALUES.
    R_FIPEX-SIGN   = 'I'.
    R_FIPEX-OPTION = 'BT'.
    R_FIPEX-LOW    = LT_VALUES-VFROM.
    R_FIPEX-HIGH   = LT_VALUES-VTO.
    IAPPEND R_FIPEX.
  ENDLOOP.

  IF SY-SUBRC NE 0.
    R_FIPEX-SIGN   = 'I'.
    R_FIPEX-OPTION = 'EQ'.
    R_FIPEX-LOW    = P_NODE_KEY.
    IAPPEND R_FIPEX.
  ENDIF.



  SELECT FIPEX AS LOW
    INTO CORRESPONDING FIELDS OF TABLE R_FIPEX
    FROM FMCI
   WHERE FIKRS  = P_FIKRS
     AND FIPEX IN S_FIPEX
     AND FIPEX IN R_FIPEX.

  R_FIPEX-SIGN   = 'I'.
  R_FIPEX-OPTION = 'EQ'.
  MODIFY R_FIPEX TRANSPORTING SIGN OPTION WHERE SIGN = SPACE.

ENDFORM.                    " GET_COMMITMENT_ITEM_RANGE
*&---------------------------------------------------------------------*
*&      Form  WRITE_TITLE
*&---------------------------------------------------------------------*
FORM WRITE_TITLE .

  WRITE :/60     SY-TITLE,
         /60(48) SY-ULINE,
         /2      'Date :', SY-DATUM,
         /2      'User :', SY-UNAME,
          156    'Page :', SY-PAGNO,
         /(C_SIZE)  SY-ULINE.
  FORMAT  COLOR = 1.
  WRITE :/ SY-VLINE NO-GAP, (40) 'Name'                NO-GAP CENTERED,
           SY-VLINE NO-GAP, (11) 'Code'                NO-GAP CENTERED,
           SY-VLINE NO-GAP, (18) 'Current Actual'      NO-GAP CENTERED,
           SY-VLINE NO-GAP, (18) 'Current Plan'        NO-GAP CENTERED,
           SY-VLINE NO-GAP, (18) 'Current Difference'  NO-GAP CENTERED,
           SY-VLINE NO-GAP, (18) 'YTD Actual'          NO-GAP CENTERED,
           SY-VLINE NO-GAP, (18) 'YTD Plan'            NO-GAP CENTERED,
           SY-VLINE NO-GAP, (18) 'YTD Difference'      NO-GAP CENTERED,
           SY-VLINE NO-GAP,
         /(C_SIZE)  SY-ULINE.

ENDFORM.                    " WRITE_TITLE
*&---------------------------------------------------------------------*
*&      Form  MAKE_DOWN_DATA
*&---------------------------------------------------------------------*
FORM MAKE_DOWN_DATA .
  ICLEAR LT_DOWN.
* Make Header
  WRITE : 'Name'                TO LT_DOWN-BEZEI,
          'Code'                TO LT_DOWN-FIPEX,
          'Current Actual'      TO LT_DOWN-C_ACTUAL,
          'Current Plan'        TO LT_DOWN-C_PLAN,
          'Current Difference'  TO LT_DOWN-C_DIFF,
          'YTD Actual'          TO LT_DOWN-T_ACTUAL,
          'YTD Plan'            TO LT_DOWN-T_PLAN,
          'YTD Difference'      TO LT_DOWN-T_DIFF.
  IAPPEND LT_DOWN.

* Make item
  LOOP AT GT_DOWN.
    WRITE : GT_DOWN-BEZEI    TO LT_DOWN-BEZEI,
            GT_DOWN-FIPEX    TO LT_DOWN-FIPEX.

    PERFORM STRING_REPLACE USING :
            GT_DOWN-C_ACTUAL LT_DOWN-C_ACTUAL,
            GT_DOWN-C_PLAN   LT_DOWN-C_PLAN,
            GT_DOWN-C_DIFF   LT_DOWN-C_DIFF,
            GT_DOWN-T_ACTUAL LT_DOWN-T_ACTUAL,
            GT_DOWN-T_PLAN   LT_DOWN-T_PLAN,
            GT_DOWN-T_DIFF   LT_DOWN-T_DIFF.
    IAPPEND LT_DOWN.
  ENDLOOP.

ENDFORM.                    " MAKE_DOWN_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_FILE
*&---------------------------------------------------------------------*
FORM SAVE_FILE .

  DATA : L_FILE_NAME  LIKE  IBIPPARMS-PATH.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      MASK             = '*.xls.'
      MODE             = 'S'
    IMPORTING
      FILENAME         = L_FILE_NAME
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.

  IF SY-SUBRC <> 0 OR L_FILE_NAME IS INITIAL.
    EXIT.
  ENDIF.

  DATA: LEN TYPE I.

  LEN =  STRLEN( L_FILE_NAME ).
  IF LEN > 4.
    LEN = LEN - 4.
    IF L_FILE_NAME+LEN(4) = '.xls'
    OR L_FILE_NAME+LEN(4) = '.XLS'.
      L_FILE_NAME = L_FILE_NAME(LEN).
    ENDIF.
  ENDIF.

  CALL FUNCTION 'EXCEL_OLE_STANDARD_DAT'
    EXPORTING
      FILE_NAME                 = L_FILE_NAME
    TABLES
      DATA_TAB                  = LT_DOWN
    EXCEPTIONS
      FILE_NOT_EXIST            = 1
      FILENAME_EXPECTED         = 2
      COMMUNICATION_ERROR       = 3
      OLE_OBJECT_METHOD_ERROR   = 4
      OLE_OBJECT_PROPERTY_ERROR = 5
      INVALID_PIVOT_FIELDS      = 6
      DOWNLOAD_PROBLEM          = 7
      OTHERS                    = 8.


ENDFORM.                    " SAVE_FILE
