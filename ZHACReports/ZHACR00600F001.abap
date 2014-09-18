*&---------------------------------------------------------------------*
*&  Include           ZHARC00600F001
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM START_OF_SELECTION.
  PERFORM DATA_PROCESSING.
ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESSING
*&---------------------------------------------------------------------*
FORM DATA_PROCESSING.
  DATA LT_ZACTASTB1 LIKE ZACRS002 OCCURS 0 WITH HEADER LINE.

  PERFORM SELECT_ZACTASTB1
                      TABLES LT_ZACTASTB1.

  PERFORM MOVE_DATA_TO_LIST
                      TABLES LT_ZACTASTB1.

ENDFORM.                    " DATA_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM END_OF_SELECTION.
  CALL SCREEN 100.
ENDFORM.                    " END_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CONTROL1
*&---------------------------------------------------------------------*
FORM ALV_CL_CONTROL1.

  PERFORM ALV_CL_CREATE_OBJ_DOCK_CONT
              USING    1500
                       CL_GUI_DOCKING_CONTAINER=>DOCK_AT_TOP
              CHANGING G_DOCKING_CONTAINER1.
  PERFORM ALV_CL_CREATE_OBJ_ALV_GRID
              USING    G_DOCKING_CONTAINER1
              CHANGING G_ALV_GRID1.
  PERFORM ALV_CL_VARIANT1.
  PERFORM ALV_CL_LAYOUT1.
  PERFORM ALV_CL_REGISTER_EDIT_EVENT USING G_ALV_GRID1.
  PERFORM ALV_CL_EXCLUDE_FCODE1.
  PERFORM ALV_CL_CREATE_OBJ_EVENT_RECVR1.
  PERFORM ALV_CL_SET_FOR_INPUT USING G_ALV_GRID1.
* ###### #### ### ## ####.
  PERFORM ALV_CL_CELL_CONTROL1.
  PERFORM ALV_CL_SORT1.
  PERFORM ALV_CL_FIELDCATALOG_MERGE1.
  PERFORM ALV_CL_SET_FOR_FIRST_DISPLAY1.

ENDFORM.                    " ALV_CL_CONTROL1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_DOCK_CONT
*&---------------------------------------------------------------------*
FORM ALV_CL_CREATE_OBJ_DOCK_CONT
     USING    P_EXTENSION
              P_SIDE
     CHANGING P_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER.

  CLEAR P_DOCKING_CONTAINER.

* Container for Custom Controls in the Screen Area
  DATA: L_REPID TYPE SY-REPID,
        L_DYNNR TYPE SY-DYNNR.

  L_REPID = SY-REPID.
  L_DYNNR = SY-DYNNR.

  CREATE OBJECT P_DOCKING_CONTAINER
      EXPORTING
        REPID     = L_REPID
        DYNNR     = L_DYNNR
        EXTENSION = P_EXTENSION
        SIDE  =     P_SIDE.

ENDFORM.                    " ALV_CL_CREATE_OBJ_DOCK_CONT
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_ALV_GRID
*&---------------------------------------------------------------------*
FORM ALV_CL_CREATE_OBJ_ALV_GRID
       USING    P_CONTAINER
                CHANGING P_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

  CLEAR P_ALV_GRID.

* Create an instance of alv control
  CREATE OBJECT P_ALV_GRID
       EXPORTING I_PARENT = P_CONTAINER.

ENDFORM.                    " ALV_CL_CREATE_OBJ_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_VARIANT1
*&---------------------------------------------------------------------*
FORM ALV_CL_VARIANT1.

  CLEAR GS_VARIANT1.
* ABAP ######
  GS_VARIANT1-REPORT   = SY-REPID.
  GS_VARIANT1-HANDLE   = 'LIS1'.
  GS_VARIANT1-USERNAME = SY-UNAME.

ENDFORM.                    " ALV_CL_VARIANT1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_LAYOUT1
*&---------------------------------------------------------------------*
FORM ALV_CL_LAYOUT1.
  DATA: L_TITLE TYPE LVC_TITLE,
        C_MONEY(18) TYPE C.

  CLEAR GS_LAYOUT1.

* Set a layout for the grid control
  GS_LAYOUT1-CWIDTH_OPT = 'X'.              "### ###
  GS_LAYOUT1-SEL_MODE   = 'D'.
*  GS_LAYOUT1-TOTALS_BEF  = 'X'.
*  GS_LAYOUT1-NO_MERGING = 'X'.
*  GS_LAYOUT1-DETAILINIT = 'X'. "DISPLAY INITIAL VALUES ON DETAIL SCREEN
*  GS_LAYOUT1-NO_KEYFIX  = ' '.
*  GS_LAYOUT1-BOX_FNAME  = 'MARK'.
  GS_LAYOUT1-NO_ROWMARK = 'X'.
  GS_LAYOUT1-SMALLTITLE = 'X'.
  GS_LAYOUT1-STYLEFNAME = 'CELLSTYL'.         "# ###
*  GS_LAYOUT1-INFO_FNAME = 'ROWSCOL'.         "# #
  GS_LAYOUT1-CTAB_FNAME = 'CELLSCOL'.         "# #
  GS_LAYOUT1-ZEBRA = 'X'.
  GS_LAYOUT1-DETAILTITL = 'TEST'.
*  CONCATENATE '('
*              G_SMR_FILTEREDLINE
*              '/'
*              G_SMR_TOTALLINE
*              ')'
*              INTO GS_ALV_LAYOUT1-GRID_TITLE.
*  READ TABLE GT_LIST INDEX 1.
*
*  CONCATENATE '#####:'
*              GT_LIST-WDATE+0(4)
*              '#'
*              GT_LIST-WDATE+4(2)
*              '#'
*              GT_LIST-WDATE+6(2)
*              '#'
*         INTO L_TITLE SEPARATED BY SPACE.
*
*  WRITE G_REFUND_AMT TO C_MONEY CURRENCY 'KRW'.
*  CONCATENATE L_TITLE
*             '#####:'
*             C_MONEY
*             '#'
*        INTO GS_LAYOUT1-GRID_TITLE
*             SEPARATED BY SPACE.
*

ENDFORM.                    " ALV_CL_LAYOUT1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_REGISTER_EDIT_EVENT
*&---------------------------------------------------------------------*
FORM ALV_CL_REGISTER_EDIT_EVENT
                    USING P_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.
* Edit  ### ##
  IF SY-BATCH IS INITIAL.
    CALL METHOD P_ALV_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
  ENDIF.
ENDFORM.                    " ALV_CL_REGISTER_EDIT_EVENT

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_EXCLUDE_FCODE1
*&---------------------------------------------------------------------*
FORM ALV_CL_EXCLUDE_FCODE1.

* ### ### #### ## ### ####.
  REFRESH GT_FCODE1.
* FIELD-SYMBOLS : <TABLE> TYPE UI_FUNCTIONS.

*  DATA : LS_EXCLUDE   TYPE UI_FUNC.
*  DATA : L_TABLE_NAME LIKE FELD-NAME.

*  CONCATENATE 'PT_FCODE' '[]' INTO  L_TABLE_NAME.
*  ASSIGN     (L_TABLE_NAME)    TO <TABLE>.

  PERFORM ALV_CL_APPEND_EXCLUDE_FUNCTION
        TABLES GT_FCODE1 "<TABLE>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** ## #### **
        USING :
                CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO, " ####&LOCAL&UNDO
*                CL_GUI_ALV_GRID=>MC_FC_AUF,      " #### &AUF
*                CL_GUI_ALV_GRID=>MC_FC_AVERAGE,  " &AVERAGE
*                cl_gui_alv_grid=>mc_fc_back_classic,
*                CL_GUI_ALV_GRID=>MC_FC_CALL_ABC, " &ABC
*                cl_gui_alv_grid=>mc_fc_call_chain,
*                cl_gui_alv_grid=>mc_fc_call_crbatch,
*                cl_gui_alv_grid=>mc_fc_call_crweb,
*                cl_gui_alv_grid=>mc_fc_call_lineitems,
*                cl_gui_alv_grid=>mc_fc_call_master_data,
*                cl_gui_alv_grid=>mc_fc_call_more,
*                cl_gui_alv_grid=>mc_fc_call_report,
*                cl_gui_alv_grid=>mc_fc_call_xint,
*                cl_gui_alv_grid=>mc_fc_call_xxl,
*                cl_gui_alv_grid=>mc_fc_col_invisible,
*                cl_gui_alv_grid=>mc_fc_col_optimize,
*                cl_gui_alv_grid=>mc_fc_current_variant,
*                cl_gui_alv_grid=>mc_fc_data_save,
*                CL_GUI_ALV_GRID=>MC_FC_DELETE_FILTER,
*                CL_GUI_ALV_GRID=>MC_FC_DESELECT_ALL,
*                cl_gui_alv_grid=>mc_fc_detail,
*                CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
*                CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
*                CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
*                cl_gui_alv_grid=>mc_fc_expmdb,
*                cl_gui_alv_grid=>mc_fc_extend,
**                cl_gui_alv_grid=>mc_fc_f4,
*                CL_GUI_ALV_GRID=>MC_FC_FILTER,
*                CL_GUI_ALV_GRID=>MC_FC_FIND,
*                cl_gui_alv_grid=>mc_fc_fix_columns,
                CL_GUI_ALV_GRID=>MC_FC_GRAPH,
*                CL_GUI_ALV_GRID=>MC_FC_HELP,
                CL_GUI_ALV_GRID=>MC_FC_INFO,
*                CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
*                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " # ##
*                cl_gui_alv_grid=>mc_fc_html,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      " # ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           " ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW, " ####.
*                cl_gui_alv_grid=>mc_fc_maintain_variant,
*                cl_gui_alv_grid=>mc_fc_maximum,
*                cl_gui_alv_grid=>mc_fc_minimum,
*                cl_gui_alv_grid=>mc_fc_pc_file,
*                CL_GUI_ALV_GRID=>MC_FC_PRINT,
*                CL_GUI_ALV_GRID=>MC_FC_PRINT_BACK,
*                CL_GUI_ALV_GRID=>MC_FC_PRINT_PREV,
                CL_GUI_ALV_GRID=>MC_FC_REFRESH,
*                cl_gui_alv_grid=>mc_fc_reprep,
*                cl_gui_alv_grid=>mc_fc_save_variant,
*                CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL.
*                cl_gui_alv_grid=>mc_fc_send,
*                cl_gui_alv_grid=>mc_fc_separator,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                CL_GUI_ALV_GRID=>MC_FC_SUBTOT,
*                CL_GUI_ALV_GRID=>MC_MB_SUM,
*                CL_GUI_ALV_GRID=>MC_FC_SUM,
*                cl_gui_alv_grid=>mc_fc_to_office,
*                cl_gui_alv_grid=>mc_fc_to_rep_tree,
*                cl_gui_alv_grid=>mc_fc_unfix_columns,
*                CL_GUI_ALV_GRID=>MC_FC_VIEWS,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL.
*                CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
*                CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID.
*                cl_gui_alv_grid=>mc_fc_word_processor.

ENDFORM.                    " ALV_CL_EXCLUDE_FCODE1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_APPEND_EXCLUDE_FUNCTION
*&---------------------------------------------------------------------*
FORM ALV_CL_APPEND_EXCLUDE_FUNCTION
                              TABLES PT_FCODE
                              USING  P_VALUE.
  DATA : LS_EXCLUDE TYPE UI_FUNC.
  LS_EXCLUDE = P_VALUE.
  APPEND LS_EXCLUDE TO PT_FCODE.
ENDFORM.                    " ALV_CL_APPEND_EXCLUDE_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_EVENT_RECVR1
*&---------------------------------------------------------------------*
FORM ALV_CL_CREATE_OBJ_EVENT_RECVR1.

  CLEAR G_EVENT_RECEIVER1.

  CREATE OBJECT G_EVENT_RECEIVER1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_TOOLBAR1       FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_DOUBLE_CLICK1 FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_USER_COMMAND1  FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_DATA_CHANGED1 FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_AFTER_USER_COMMAND1
*                                                      FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_BUTTON_CLICK1 FOR G_ALV_GRID1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SET_FOR_INPUT
*&---------------------------------------------------------------------*
FORM ALV_CL_SET_FOR_INPUT
            USING P_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

* ### ## ## DISPLAY
* INPUT ## 1## ## ### ####
*            0## ## ####.
  CALL METHOD P_ALV_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

ENDFORM.                    " ALV_CL_SET_FOR_INPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_CONTROL1
*&---------------------------------------------------------------------*
FORM ALV_CL_CELL_CONTROL1.
  DATA: LT_CELLSTYL TYPE LVC_S_STYL OCCURS 0 WITH HEADER LINE,
        LT_CELLSCOL TYPE LVC_S_SCOL OCCURS 0 WITH HEADER LINE,
        L_TABIX    TYPE SY-TABIX.
* ## # ##
* ## ## ### ### ### ####
* ### ### #### ##.
*  CHECK SCREEN_MODE EQ C_CHANGE.

  LOOP AT GT_LIST.
    L_TABIX = SY-TABIX.

    CLEAR: GT_LIST-CELLSTYL,
           GT_LIST-CELLSCOL.

*    IF GT_LIST-FBRST EQ C_FBRST_S OR
*       GT_LIST-FBRST EQ C_FBRST_X.
*      GT_LIST-MARK = SPACE.
*      PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*         USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ELSE.
*    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*       USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*    ENDIF.
    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST-CELLSTYL.
    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_LIST-CELLSCOL.
    MODIFY GT_LIST INDEX L_TABIX TRANSPORTING
                                      CELLSTYL
                                      CELLSCOL.
    REFRESH: LT_CELLSTYL,
             LT_CELLSCOL.
    CLEAR  : LT_CELLSTYL,
             LT_CELLSCOL.
    CLEAR: GT_LIST.
  ENDLOOP.
ENDFORM.                    " ALV_CL_CELL_CONTROL1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SORT1
*&---------------------------------------------------------------------*
FORM ALV_CL_SORT1.
  DATA: LS_SORT LIKE LVC_S_SORT.

*  REFRESH GT_SORT1.

*  LS_SORT-FIELDNAME = 'BELNR'.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-GROUP     = ' '.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = ' '.
*  APPEND LS_SORT TO GT_SORT1.
*  CLEAR LS_SORT.

ENDFORM.                    " ALV_CL_SORT1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_FIELDCATALOG_MERGE1
*&---------------------------------------------------------------------*
FORM ALV_CL_FIELDCATALOG_MERGE1.

  DATA: L_TABIX TYPE SY-TABIX,
        LS_FDCAT LIKE LVC_S_FCAT.

  REFRESH GT_FDCAT1.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE    = 'X'
            I_BYPASSING_BUFFER = 'X'
            I_STRUCTURE_NAME   = 'ZACRS002'
       CHANGING
            CT_FIELDCAT        = GT_FDCAT1[].

  LOOP AT GT_FDCAT1
          INTO LS_FDCAT.
    L_TABIX = SY-TABIX.
    LS_FDCAT-KEY        = SPACE.
    LS_FDCAT-FIX_COLUMN = SPACE.

*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'EXMON'.
        LS_FDCAT-FIX_COLUMN = 'X'.
    ENDCASE.
*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'EXMON' OR
           'WDATE'.
        LS_FDCAT-NO_OUT = 'X'.
    ENDCASE.
*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'TOTAX'.
        LS_FDCAT-DO_SUM = 'X'.
    ENDCASE.
**   ########
*    CASE LS_FDCAT-FIELDNAME.
*      WHEN 'MARK'.
*        LS_FDCAT-CHECKBOX = 'X'.
*    ENDCASE.
**   KEY####
*    CASE LS_FDCAT-FIELDNAME.
*      WHEN 'MARK'.
*        LS_FDCAT-EMPHASIZE = 'C000'.
*      WHEN 'IMCOD' OR
*           'IMRAN'.
*        LS_FDCAT-EMPHASIZE = 'C410'.
*    ENDCASE.

*   #####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'AREA'.
        LS_FDCAT-SCRTEXT_L    = 'TEXT'.
        LS_FDCAT-SCRTEXT_M    = 'TEXT'.
        LS_FDCAT-SCRTEXT_S    = 'TEXT'.
        LS_FDCAT-COLTEXT      = 'TEXT'.
      WHEN 'CUSCD'.
        LS_FDCAT-SCRTEXT_L    = '#####'.
        LS_FDCAT-SCRTEXT_M    = '#####'.
        LS_FDCAT-SCRTEXT_S    = '#####'.
        LS_FDCAT-COLTEXT      = '#####'.
      WHEN 'IMPHS'.
        LS_FDCAT-SCRTEXT_L    = 'HS##'.
        LS_FDCAT-SCRTEXT_M    = 'HS##'.
        LS_FDCAT-SCRTEXT_S    = 'HS##'.
        LS_FDCAT-COLTEXT      = 'HS##'.
      WHEN 'DRQTY'.
        LS_FDCAT-SCRTEXT_L    = '##'.
        LS_FDCAT-SCRTEXT_M    = '##'.
        LS_FDCAT-SCRTEXT_S    = '##'.
        LS_FDCAT-COLTEXT      = '##'.
      WHEN 'UPRICE'.
        LS_FDCAT-SCRTEXT_L    = '##'.
        LS_FDCAT-SCRTEXT_M    = '##'.
        LS_FDCAT-SCRTEXT_S    = '##'.
        LS_FDCAT-COLTEXT      = '##'.
      WHEN 'TOTAX'.
        LS_FDCAT-SCRTEXT_L    = '####'.
        LS_FDCAT-SCRTEXT_M    = '####'.
        LS_FDCAT-SCRTEXT_S    = '####'.
        LS_FDCAT-COLTEXT      = '####'.
      WHEN 'VBELN'.
        LS_FDCAT-SCRTEXT_L    = '######'.
        LS_FDCAT-SCRTEXT_M    = '######'.
        LS_FDCAT-SCRTEXT_S    = '######'.
        LS_FDCAT-COLTEXT      = '######'.
    ENDCASE.


    MODIFY GT_FDCAT1 FROM LS_FDCAT INDEX L_TABIX.
    CLEAR LS_FDCAT.

  ENDLOOP.
ENDFORM.                    " ALV_CL_FIELDCATALOG_MERGE1
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SET_FOR_FIRST_DISPLAY1
*&---------------------------------------------------------------------*
FORM ALV_CL_SET_FOR_FIRST_DISPLAY1.

  CALL METHOD G_ALV_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*      I_STRUCTURE_NAME     = ' '
      IS_LAYOUT            = GS_LAYOUT1
      IS_VARIANT           = GS_VARIANT1
      I_SAVE               = 'A'
      IT_TOOLBAR_EXCLUDING = GT_FCODE1
    CHANGING
      IT_FIELDCATALOG      = GT_FDCAT1[]
      IT_SORT              = GT_SORT1
      IT_OUTTAB            = GT_LIST[].

ENDFORM.                    " ALV_CL_SET_FOR_FIRST_DISPLAY1

*&---------------------------------------------------------------------*
*&      Form  ALV_CL_REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
FORM ALV_CL_REFRESH_TABLE_DISPLAY
        USING P_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID
              P_REC_STABLE TYPE LVC_S_STBL.

  P_REC_STABLE-ROW = 'X'.
  P_REC_STABLE-COL = 'X'.
  CALL METHOD P_ALV_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = P_REC_STABLE.
ENDFORM.                    " ALV_CL_REFRESH_TABLE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_DOUBLE_CLICK1
*&---------------------------------------------------------------------*
FORM ALV_CL_CELL_DOUBLE_CLICK1
                           USING
                              P_ROW TYPE LVC_S_ROW
                              P_COLUMN TYPE LVC_S_COL.

*  DATA LT_LINE TYPE REF TO DATA.
*  FIELD-SYMBOLS: <GA_LIST> TYPE ANY,
*                 <GA_LIST_TMP> TYPE ANY.
*
**  CREATE DATA LT_LINE LIKE LINE OF <GT_LIST>.
**  ASSIGN LT_LINE->* TO <GA_LIST>.
*
*
*  CASE P_COLUMN-FIELDNAME.
*
*    WHEN 'BELNR_RLFE'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
**        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
*        IF GT_LIST-BELNR_RLFE IS INITIAL.
*
*          MESSAGE S000 WITH
*            '####### ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_RLFE.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-BAS_DT(4).
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*    WHEN 'BELNR_AR'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
*        IF GT_LIST-BELNR_AR IS INITIAL.
*
*          MESSAGE S000 WITH
*            '######### ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_AR.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-BUDAT.
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*    WHEN 'CNRT_NO'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF GT_LIST-RNT_TYP EQ 1.
*
*        SET PARAMETER ID 'CNRT1' FIELD GT_LIST-CNRT_NO.
*        CALL TRANSACTION 'ZRFIAM0142' .
*      ELSE.
*        SET PARAMETER ID 'CNRT2' FIELD GT_LIST-CNRT_NO.
*        CALL TRANSACTION 'ZRFIAM0072' .
*      ENDIF.
**    WHEN 'ANLN1'.
**
**      READ TABLE <GT_LIST> INTO <GA_LIST> INDEX P_ROW-INDEX.
**
**      IF SY-SUBRC EQ 0.
**        ASSIGN COMPONENT 'ANLN1' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
**
**        IF <GA_LIST_TMP> IS INITIAL.
**
**          MESSAGE S000 WITH
**            '##### ####.'.
**
**        ELSE.
**
**          SET PARAMETER ID 'AN1' FIELD <GA_LIST_TMP>.
**          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
**          CALL TRANSACTION C_TCODE_AS03
**               AND SKIP FIRST SCREEN.
**
**        ENDIF.
**      ENDIF.
*
*    WHEN 'BELNR_GRAMT'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
**          ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
*        IF GT_LIST-BELNR_GRAMT IS INITIAL.
*
*          MESSAGE S000 WITH
*            '######## ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_GRAMT.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-BUDAT(4).
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*    WHEN 'BELNR_R'.
*
*      READ TABLE GT_LIST INDEX P_ROW-INDEX.
*
*      IF SY-SUBRC EQ 0.
*
**          ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO <GA_LIST_TMP>.
*        IF GT_LIST-BELNR_R IS INITIAL.
*
*          MESSAGE S000 WITH
*            '########### ####.'.
*
*        ELSE.
*
*          SET PARAMETER ID 'BLN' FIELD GT_LIST-BELNR_R.
*          SET PARAMETER ID 'BUK' FIELD C_BUKRS_1000.
*          SET PARAMETER ID 'GJR' FIELD GT_LIST-GJAHR_R.
*
*          CALL TRANSACTION C_TCODE_FB03
*               AND SKIP FIRST SCREEN.
*
*        ENDIF.
*      ENDIF.
*
*  ENDCASE.

ENDFORM.                    " ALV_CL_CELL_DOUBLE_CLICK1
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZACTASTB1
*&---------------------------------------------------------------------*
FORM SELECT_ZACTASTB1
                  TABLES PT_ZACTASTB1 STRUCTURE ZACRS002.

  SELECT
    A~PROGNAME
    A~TABNAME
    A~AREA
    A~FIELDNAME
    FROM ZACTASTB1 AS A
      INNER JOIN ZACTTATB AS B
        ON A~TABNAME = B~TABNAME
    INTO CORRESPONDING FIELDS OF TABLE PT_ZACTASTB1.

  SORT PT_ZACTASTB1 BY PROGNAME TABNAME FIELDNAME.
ENDFORM.                    " SELECT_ZACTASTB1
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA_TO_LIST
*&---------------------------------------------------------------------*
FORM MOVE_DATA_TO_LIST
                    TABLES PT_ZACTASTB1 STRUCTURE ZACRS002.

  LOOP AT PT_ZACTASTB1.
    MOVE-CORRESPONDING PT_ZACTASTB1 TO GT_LIST.

    SELECT SINGLE
      KEYFLAG
      FROM DD03L
      INTO GT_LIST-KEYFLAG
     WHERE TABNAME EQ GT_LIST-TABNAME
       AND FIELDNAME EQ GT_LIST-FIELDNAME.

    APPEND GT_LIST.
    CLEAR GT_LIST.
  ENDLOOP.

ENDFORM.                    " MOVE_DATA_TO_LIST
