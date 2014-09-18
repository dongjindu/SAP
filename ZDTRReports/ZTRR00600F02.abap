*&---------------------------------------------------------------------*
*&  Include           ZTRR00600F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_7000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_7000 OUTPUT.
  SET PF-STATUS 'S7000'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_7000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_SCREEN_7000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_SCREEN_7000 OUTPUT.

  IF G_CUSTOM_7000 IS INITIAL.

    CREATE OBJECT G_CUSTOM_7000
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER_7000'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    CREATE OBJECT G_GRID_7000
      EXPORTING
        I_PARENT      = G_CUSTOM_7000
        I_APPL_EVENTS = 'X'.

    PERFORM SET_GS_LAYOUT.
    PERFORM FIELDCAT_INIT  USING  GT_FIELDCAT1 'GT_7000'.
    PERFORM EXCLUDE_OF_TOOLBAR_BUTTON USING 'GT_EXCLUDE'.

    CALL METHOD G_GRID_7000->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT_7000
        IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = GT_FIELDCAT1
        IT_OUTTAB            = GT_7000[].

    CALL METHOD G_GRID_7000->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD G_GRID_7000->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER. "MODIFIED.

    CREATE OBJECT G_EVENT_RECEIVER.
    SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR G_GRID_7000.

  ELSE.
    CALL METHOD G_GRID_7000->CHECK_CHANGED_DATA.
    CALL METHOD G_GRID_7000->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = GS_STABLE
      EXCEPTIONS
        FINISHED  = 1
        OTHERS    = 2.

  ENDIF.

ENDMODULE.                 " CREATE_SCREEN_7000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_GS_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_GS_LAYOUT.
  CLEAR GS_LAYOUT_7000.
  GS_LAYOUT_7000-SEL_MODE = 'A'.
*  GS_LAYOUT_7000-BOX_FNAME = 'SEL'.
  GS_LAYOUT_7000-GRID_TITLE = ' '.
  GS_LAYOUT_7000-NO_MERGING = ' '.
  GS_LAYOUT_7000-ZEBRA = 'X'.
*  GS_LAYOUT_7000-CWIDTH_OPT = 'X'.
*  GS_LAYOUT_7000-STYLEFNAME = 'CELLTAB'.
ENDFORM.                    " SET_GS_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
FORM FIELDCAT_INIT USING P_FIELDCAT  TYPE LVC_T_FCAT
                         P_TNAME  LIKE DD02L-TABNAME.

  ICLEAR   : P_FIELDCAT, GT_FIELDCAT.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_INTERNAL_TABNAME     = P_TNAME
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  CHECK SY-SUBRC EQ 0.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    MOVE-CORRESPONDING GS_FIELDCAT TO GS_FIELDCAT_LVC.

    GS_FIELDCAT_LVC-SCRTEXT_M = GS_FIELDCAT-SELTEXT_M.
    GS_FIELDCAT_LVC-REF_FIELD = GS_FIELDCAT-REF_FIELDNAME.
    GS_FIELDCAT_LVC-REF_TABLE = GS_FIELDCAT-REF_TABNAME.
    CASE GS_FIELDCAT_LVC-FIELDNAME(5).
      WHEN 'GRUPP'.
        GS_FIELDCAT_LVC-EDIT = 'X'.
        GS_FIELDCAT_LVC-F4AVAILABL     = 'X'.
      WHEN 'DMSHB'.
        GS_FIELDCAT_LVC-EDIT = 'X'.
        CONCATENATE GS_FIELDCAT_LVC-FIELDNAME+5(2) '.' P_GJAHR
               INTO GS_FIELDCAT_LVC-SCRTEXT_M.

    ENDCASE.

    GS_FIELDCAT_LVC-SCRTEXT_S    =
    GS_FIELDCAT_LVC-SCRTEXT_L    =
    GS_FIELDCAT_LVC-REPTEXT      =
    GS_FIELDCAT_LVC-SCRTEXT_M.
    APPEND : GS_FIELDCAT_LVC TO P_FIELDCAT.
    CLEAR  : GS_FIELDCAT, GS_FIELDCAT_LVC.
  ENDLOOP.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  exclude_of_toolbar_button_1
*&---------------------------------------------------------------------*
FORM EXCLUDE_OF_TOOLBAR_BUTTON   USING  L_TABNAME.
  DATA : L_TAB_NAME LIKE FELD-NAME.

  FIELD-SYMBOLS : <TABLE> TYPE UI_FUNCTIONS.

  CONCATENATE L_TABNAME '[]' INTO  L_TAB_NAME.
  ASSIGN     (L_TAB_NAME)    TO <TABLE>.

  REFRESH <TABLE>.


  PERFORM ADD_EXCLUDE_TOOLBAR_BUTTON
         TABLES <TABLE>
         USING : CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
ENDFORM.                    " exclude_of_toolbar_button_1
*&---------------------------------------------------------------------*
*&      Form  add_exclude_toolbar_button
*&---------------------------------------------------------------------*
FORM ADD_EXCLUDE_TOOLBAR_BUTTON  TABLES   LT_TABLE
                                 USING    L_VALUE.

  DATA: L_EXCLUDE TYPE UI_FUNC.

  L_EXCLUDE = L_VALUE.
  APPEND L_EXCLUDE TO LT_TABLE.

ENDFORM.                    " add_exclude_toolbar_button
*&---------------------------------------------------------------------*
*&      Form  ROW_CONTROL
*&---------------------------------------------------------------------*
FORM ROW_CONTROL  USING    P_CODE.

  DATA :  LT_INDEX TYPE LVC_T_ROW WITH HEADER LINE.
  ICLEAR LT_INDEX.

  CALL METHOD G_GRID_7000->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_INDEX[].

  SORT LT_INDEX DESCENDING.

  CASE P_CODE.
    WHEN 'INSERT'.
      CLEAR GT_7000.
      LOOP AT LT_INDEX.
        INSERT GT_7000 INDEX LT_INDEX.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        APPEND GT_7000.
      ENDIF.

    WHEN 'DELETE'.
      LOOP AT LT_INDEX.
        DELETE GT_7000 INDEX LT_INDEX.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " ROW_CONTROL
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_7000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_7000 INPUT.

  CASE OK_CODE.
    WHEN 'ADD'.
      DATA : L_VALID(1).
      CALL METHOD G_GRID_7000->CHECK_CHANGED_DATA
        IMPORTING
          E_VALID = L_VALID.

      ICLEAR GT_MANUAL.
      DELETE GT_7000 WHERE GRUPP = SPACE.
      GT_MANUAL[] = GT_7000[].

      PERFORM SELECT_PLAN_DATA.
      IF G_PA = 'X'.
        PERFORM APPEND_PA_DATA.
      ENDIF.
      IF G_CCA = 'X'.
        PERFORM APPEND_CCA_DATA.
      ENDIF.
      IF G_IM = 'X'.
        PERFORM APPEND_IM_DATA.
      ENDIF.

      PERFORM APPEND_MANUAL.
      PERFORM GET_BALANCE_DATA.
      PERFORM REFRESH_DATA.

      LEAVE TO SCREEN 0.

    WHEN 'INSERT' OR 'DELETE'.
      PERFORM ROW_CONTROL USING OK_CODE.

  ENDCASE.

  CLEAR OK_CODE.
ENDMODULE.                 " USER_COMMAND_7000  INPUT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
FORM DATA_CHANGED  USING RR_DATA_CHANGED
                         TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
  DATA : LS_MOD_CELLS TYPE LVC_S_MODI.


  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'GRUPP'.
        CALL METHOD RR_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
          IMPORTING
            E_VALUE     = GT_7000-GRUPP.

        IF GT_7000-GRUPP NE SPACE.
          SELECT SINGLE TEXTL INTO GT_7000-TEXTL
            FROM T035T
           WHERE SPRAS = SY-LANGU
             AND GRUPP = GT_7000-GRUPP.

          CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
            EXPORTING
              I_ROW_ID    = LS_MOD_CELLS-ROW_ID
              I_FIELDNAME = 'TEXTL'
              I_VALUE     = GT_7000-TEXTL.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  CALL METHOD CL_GUI_CFW=>FLUSH.
ENDFORM.                    " DATA_CHANGED
