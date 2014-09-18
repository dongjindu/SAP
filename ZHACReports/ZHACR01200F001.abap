*&---------------------------------------------------------------------*
*&  Include           ZHACR01000F001
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

  DATA LT_AIND_STR LIKE GA_LIST OCCURS 0 WITH HEADER LINE.

  PERFORM SELECT_AIND_STR
                      TABLES LT_AIND_STR.

  PERFORM MOVE_DATA_TO_LIST
                      TABLES LT_AIND_STR.

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

  PERFORM ALV_CL_CREATE_OBJ_DOCK_CONT.
  PERFORM ALV_CL_CREATE_SPLITTER.
  PERFORM ALV_CL_CREATE_OBJ_ALV_GRID
              USING    G_CONTAIN_LEFT
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
FORM ALV_CL_CREATE_OBJ_DOCK_CONT.

* Container for Custom Controls in the Screen Area
  DATA: L_REPID TYPE SY-REPID,
        L_DYNNR TYPE SY-DYNNR.

  L_REPID = SY-REPID.
  L_DYNNR = SY-DYNNR.

  CREATE OBJECT G_CUSTOM_CONTAINER1
    EXPORTING
      CONTAINER_NAME = 'G_CUSTOM_CONTAINER1'.

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
    EXPORTING
      I_PARENT = P_CONTAINER.

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
  GS_LAYOUT1-NO_ROWMARK = ' '.
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
                CL_GUI_ALV_GRID=>MC_FC_HELP,
                CL_GUI_ALV_GRID=>MC_FC_INFO,
                CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " # ##
                CL_GUI_ALV_GRID=>MC_FC_HTML,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      " # ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           " ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW, " ####.
                CL_GUI_ALV_GRID=>MC_FC_MAINTAIN_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_MAXIMUM,
                CL_GUI_ALV_GRID=>MC_FC_MINIMUM,
                CL_GUI_ALV_GRID=>MC_FC_PC_FILE,
                CL_GUI_ALV_GRID=>MC_FC_PRINT,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_BACK,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_PREV,
                CL_GUI_ALV_GRID=>MC_FC_REFRESH,
                CL_GUI_ALV_GRID=>MC_FC_REPREP,
                CL_GUI_ALV_GRID=>MC_FC_SAVE_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL,
                CL_GUI_ALV_GRID=>MC_FC_SEND,
                CL_GUI_ALV_GRID=>MC_FC_SEPARATOR,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                CL_GUI_ALV_GRID=>MC_FC_SUBTOT,
*                CL_GUI_ALV_GRID=>MC_MB_SUM,
*                CL_GUI_ALV_GRID=>MC_FC_SUM,
                CL_GUI_ALV_GRID=>MC_FC_TO_OFFICE,
                CL_GUI_ALV_GRID=>MC_FC_TO_REP_TREE,
                CL_GUI_ALV_GRID=>MC_FC_UNFIX_COLUMNS,
                CL_GUI_ALV_GRID=>MC_FC_VIEWS,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
                CL_GUI_ALV_GRID=>MC_FC_WORD_PROCESSOR.

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
  SET HANDLER G_EVENT_RECEIVER1->HANDLE_DOUBLE_CLICK1 FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_USER_COMMAND1  FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_DATA_CHANGED1 FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_AFTER_USER_COMMAND1
*                                                      FOR G_ALV_GRID1.
*  SET HANDLER G_EVENT_RECEIVER1->HANDLE_BUTTON_CLICK1 FOR G_ALV_GRID1.

ENDFORM.                    "ALV_CL_CREATE_OBJ_EVENT_RECVR1
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

  LOOP AT GT_LIST_LEFT.
    L_TABIX = SY-TABIX.

    CLEAR: GT_LIST_LEFT-CELLSTYL,
           GT_LIST_LEFT-CELLSCOL.

*    IF GT_LIST_LEFT-FBRST EQ C_FBRST_S OR
*       GT_LIST_LEFT-FBRST EQ C_FBRST_X.
*      GT_LIST_LEFT-MARK = SPACE.
*      PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*         USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ELSE.
*    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*       USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*    ENDIF.
    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST_LEFT-CELLSTYL.
    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_LIST_LEFT-CELLSCOL.
    MODIFY GT_LIST_LEFT INDEX L_TABIX TRANSPORTING
                                      CELLSTYL
                                      CELLSCOL.
    REFRESH: LT_CELLSTYL,
             LT_CELLSCOL.
    CLEAR  : LT_CELLSTYL,
             LT_CELLSCOL.
    CLEAR: GT_LIST_LEFT.
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
      I_STRUCTURE_NAME   = 'ZHACS0040'
    CHANGING
      CT_FIELDCAT        = GT_FDCAT1[].

  LOOP AT GT_FDCAT1
          INTO LS_FDCAT.
    L_TABIX = SY-TABIX.
    LS_FDCAT-KEY        = SPACE.
    LS_FDCAT-FIX_COLUMN = SPACE.

*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'DOCUMENT' OR
           'WDATE'.
        LS_FDCAT-NO_OUT = 'X'.
    ENDCASE.
*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'FILE_SIZE1' OR
           'FILE_SIZE2' OR
           'FILE_SIZE3'.
        LS_FDCAT-DECIMALS_O = '3'.
        LS_FDCAT-EXPONENT = '0'.
    ENDCASE.
*   KEY####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'ARCHINDEX' OR 'TEXT'.
        LS_FDCAT-EMPHASIZE = 'C410'.
      WHEN 'GENTAB'.
        LS_FDCAT-EMPHASIZE = 'C510'.
    ENDCASE.

*   #####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'APPLIC'.
        LS_FDCAT-SCRTEXT_L    = 'Module'.
        LS_FDCAT-SCRTEXT_M    = 'Module'.
        LS_FDCAT-SCRTEXT_S    = 'Module'.
        LS_FDCAT-COLTEXT      = 'Module'.
      WHEN 'OBJECT'.
        LS_FDCAT-SCRTEXT_L    = 'Object Name'.
        LS_FDCAT-SCRTEXT_M    = 'Object Name'.
        LS_FDCAT-SCRTEXT_S    = 'Object Name'.
        LS_FDCAT-COLTEXT      = 'Object Name'.
      WHEN 'OBJTEXT'.
        LS_FDCAT-SCRTEXT_L    = 'Object Description'.
        LS_FDCAT-SCRTEXT_M    = 'Object Description'.
        LS_FDCAT-SCRTEXT_S    = 'Object Description'.
        LS_FDCAT-COLTEXT      = 'Object Description'.
      WHEN 'ARCHINDEX'.
        LS_FDCAT-SCRTEXT_L    = 'AS'.
        LS_FDCAT-SCRTEXT_M    = 'AS'.
        LS_FDCAT-SCRTEXT_S    = 'AS'.
        LS_FDCAT-COLTEXT      = 'AS'.
      WHEN 'TEXT'.
        LS_FDCAT-SCRTEXT_L    = 'AS Description'.
        LS_FDCAT-SCRTEXT_M    = 'AS Description'.
        LS_FDCAT-SCRTEXT_S    = 'AS Description'.
        LS_FDCAT-COLTEXT      = 'AS Description'.
      WHEN 'GENTAB'.
        LS_FDCAT-SCRTEXT_L    = 'Temp Table'.
        LS_FDCAT-SCRTEXT_M    = 'Temp Table'.
        LS_FDCAT-SCRTEXT_S    = 'Temp Table'.
        LS_FDCAT-COLTEXT      = 'Temp Table'.
      WHEN 'ACTIVE'.
        LS_FDCAT-SCRTEXT_L    = 'Status'.
        LS_FDCAT-SCRTEXT_M    = 'Status'.
        LS_FDCAT-SCRTEXT_S    = 'Status'.
        LS_FDCAT-COLTEXT      = 'Status'.
      WHEN 'SAVEMONTHS'.
        LS_FDCAT-SCRTEXT_L    = 'Residence Time'.
        LS_FDCAT-SCRTEXT_M    = 'Residence Time'.
        LS_FDCAT-SCRTEXT_S    = 'Residence Time'.
        LS_FDCAT-COLTEXT      = 'Residence Time'.
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
      IT_OUTTAB            = GT_LIST_LEFT[].

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
  READ TABLE GT_LIST_LEFT INDEX P_ROW-INDEX.

  CASE P_COLUMN-FIELDNAME.

    WHEN 'ARCHINDEX' OR
         'TEXT'.

      IF GT_LIST_LEFT-OBJECT IS NOT INITIAL.
        SET PARAMETER ID 'OBT' FIELD GT_LIST_LEFT-OBJECT.
        CALL TRANSACTION 'SARI'.
      ENDIF.

    WHEN 'GENTAB'.

***      IF GT_LIST_LEFT-GENTAB IS NOT INITIAL.
***        SET PARAMETER ID 'DTB' FIELD GT_LIST_LEFT-GENTAB.
***        CALL TRANSACTION 'SE11'.
***      ENDIF.

    WHEN OTHERS.

      PERFORM SELECT_ADMI_VARIA.

  ENDCASE.
*
**        ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO
*<GA_LIST_TMP>.
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
**        ASSIGN COMPONENT 'ANLN1' OF STRUCTURE <GA_LIST> TO
**<GA_LIST_TMP>.
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
**          ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO
**<GA_LIST_TMP>.
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
**          ASSIGN COMPONENT 'BELNR' OF STRUCTURE <GA_LIST> TO
**<GA_LIST_TMP>.
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
*&      Form  SELECT_ADMI_RUN
*&---------------------------------------------------------------------*
FORM SELECT_AIND_STR
                  TABLES PT_AIND_STR STRUCTURE GA_LIST.

  DATA LT_LIST LIKE GA_LIST OCCURS 0 WITH HEADER LINE.

  SELECT DISTINCT
    A~OBJECT
    C~OBJTEXT
    E~ZAPPLI AS APPLIC
    FROM AIND_STR1 AS A
     LEFT OUTER JOIN AIND_STR1T AS B
        ON A~ARCHINDEX EQ B~ARCHINDEX
       AND A~ITYPE EQ B~ITYPE
       AND B~SPRAS EQ SY-LANGU
     LEFT OUTER JOIN ARCH_TXT AS C
       ON A~OBJECT EQ C~OBJECT
      AND C~LANGU EQ SY-LANGU
     LEFT OUTER JOIN AIND_STR2 AS D
       ON A~ARCHINDEX EQ D~ARCHINDEX
     INNER JOIN ZHACT0060 AS E
       ON A~OBJECT = E~OBJECT
    INTO CORRESPONDING FIELDS OF TABLE LT_LIST
    WHERE A~ITYPE EQ 'I'
      AND E~ZAPPLI IN S_APPLIC
      AND A~OBJECT IN S_OBJECT.

  IF SY-SUBRC EQ 0.
    PT_AIND_STR[] = LT_LIST[].
  ENDIF.
ENDFORM.                    " SELECT_ADMI_RUN
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA_TO_LIST
*&---------------------------------------------------------------------*
FORM MOVE_DATA_TO_LIST
                    TABLES PT_AIND_STR STRUCTURE GA_LIST.

  REFRESH: GT_LIST_LEFT,
           GT_LIST_RIGHT,
           GT_LIST_TOP,
           GT_LIST_BOTTOM.

  LOOP AT PT_AIND_STR.

    MOVE-CORRESPONDING PT_AIND_STR TO GT_LIST_LEFT.
    APPEND GT_LIST_LEFT.
    CLEAR GT_LIST_LEFT.

  ENDLOOP.

  SORT GT_LIST_LEFT BY APPLIC OBJECT.

ENDFORM.                    " MOVE_DATA_TO_LIST
*---------------------------------------------------------------------*
*       FORM APPEND_BDC_DATA
*---------------------------------------------------------------------*
FORM APPEND_BDC_DATA
                  TABLES
                    PT_BDCDATA STRUCTURE BDCDATA
                  USING
                    P_DYNBEGIN LIKE BDCDATA-DYNBEGIN
                    P_NAME
                    P_VALUE.
  IF P_DYNBEGIN = 'X'.
    PT_BDCDATA-PROGRAM  = P_NAME.
    PT_BDCDATA-DYNPRO   = P_VALUE.
    PT_BDCDATA-DYNBEGIN = P_DYNBEGIN.

    APPEND PT_BDCDATA.
    CLEAR PT_BDCDATA.
  ELSE.
    PT_BDCDATA-FNAM = P_NAME.
    PT_BDCDATA-FVAL = P_VALUE.

    APPEND PT_BDCDATA.
    CLEAR PT_BDCDATA.
  ENDIF.
ENDFORM.                    " APPEND_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_FOR_BDC
                          TABLES
                            PT_BDCDATA
                            PT_MSGDATA
                          USING
                            P_TCODE TYPE SY-TCODE.
  DATA: LS_OPTIONS LIKE CTU_PARAMS.
* OPTION
  LS_OPTIONS-DISMODE = G_BDCMODE.
  LS_OPTIONS-UPDMODE = 'S'.
  LS_OPTIONS-DEFSIZE = 'X'.
  LS_OPTIONS-RACOMMIT = 'X'.

  CALL TRANSACTION P_TCODE USING    PT_BDCDATA
                           OPTIONS  FROM LS_OPTIONS
                           MESSAGES INTO PT_MSGDATA.

ENDFORM.                    " CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
FORM RESULT_TRANSACTION_FOR_ACR
                            TABLES
                              PT_MSGDATA STRUCTURE BDCMSGCOLL
                            USING
                              P_TABNAME.

***  DATA L_RETURN LIKE ZBDCTABLE-RETMSG.
***  DATA L_RETMSG LIKE ZBDCTABLE-MESSAGE.
***
**** ##### ##
***  LOOP AT PT_MSGDATA
***    WHERE
***      MSGTYP  EQ C_MSGTY_S AND
***      MSGSPRA EQ SY-LANGU  AND
***      MSGID   EQ 'SV'      AND
***      ( MSGNR EQ '004' OR
***        MSGNR EQ '018' ).
***
***    L_RETURN = C_MSGTY_S.  "##
***    L_RETMSG = 'Succeed'.
***    CLEAR PT_MSGDATA.
***    EXIT.
***  ENDLOOP.
***
**** ###### ###
***  IF SY-SUBRC NE 0.
***
****   ##### ##
***    LOOP AT PT_MSGDATA
***      WHERE
***        MSGTYP  EQ C_MSGTY_E.  "####
***
***      L_RETURN = C_MSGTY_E.  "##
***
***      PERFORM MESSAGE_TEXT_BUILD
***                              USING
***                                PT_MSGDATA-MSGID
***                                PT_MSGDATA-MSGNR
***                                PT_MSGDATA-MSGTYP
***                                PT_MSGDATA-MSGV1
***                                PT_MSGDATA-MSGV2
***                                PT_MSGDATA-MSGV3
***                                PT_MSGDATA-MSGV4
***                              CHANGING
***                                L_RETMSG.
***      CLEAR PT_MSGDATA.
***      EXIT.
***    ENDLOOP.
***
******    IF SY-SUBRC NE 0.
*******    ###### ##
******
******      LOOP AT PT_MSGDATA
******        WHERE
******          MSGSPRA EQ SY-LANGU  AND
******          MSGID   EQ '00'      AND
******          ( MSGNR EQ '344' OR
******            MSGNR EQ '346' OR
******            MSGNR EQ '347' OR
******            MSGNR EQ '348' OR
******            MSGNR EQ '349' ).
******
******        PS_RETURN-RETCD = C_RETCD_E.
******
******        PERFORM MESSAGE_TEXT_BUILD
******                                USING
******                                  PT_MSGDATA-MSGID
******                                  PT_MSGDATA-MSGNR
******                                  PT_MSGDATA-MSGTYP
******                                  PT_MSGDATA-MSGV1
******                                  PT_MSGDATA-MSGV2
******                                  PT_MSGDATA-MSGV3
******                                  PT_MSGDATA-MSGV4
******                                CHANGING
******                                  PS_RETURN-RETMG.
******        CLEAR PT_MSGDATA.
******        EXIT.
******      ENDLOOP.
******
******      IF SY-SUBRC NE 0.
*******     ########## ###
******
******        PS_RETURN-RETCD = C_RETCD_E.  "##
******        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
******
******      ENDIF.
******    ENDIF.
***  ENDIF.
***
***  IF L_RETURN IS NOT INITIAL.
***
***    UPDATE ZBDCTABLE
***       SET RETMSG = L_RETURN
***           MESSAGE = L_RETMSG
***     WHERE TABNAME = P_TABNAME.
***
***    IF SY-SUBRC EQ 0.
***      COMMIT WORK.
***      GT_LIST-RETMSG = L_RETURN.
***      GT_LIST-MESSAGE = L_RETMSG.
***      MODIFY GT_LIST TRANSPORTING RETMSG MESSAGE WHERE TABNAME =
***P_TABNAME .
***    ENDIF.
***    CLEAR L_RETURN.
***  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
FORM MESSAGE_TEXT_BUILD
                    USING
                      P_MSGID
                      P_MSGNO
                      P_MSGTY
                      P_MSGV1
                      P_MSGV2
                      P_MSGV3
                      P_MSGV4
                    CHANGING
                      P_MESSAGE.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = P_MSGID
      MSGNR               = P_MSGNO
      MSGV1               = P_MSGV1
      MSGV2               = P_MSGV2
      MSGV3               = P_MSGV3
      MSGV4               = P_MSGV4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = P_MESSAGE.

ENDFORM.                    " MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONCATENATE
*&---------------------------------------------------------------------*
FORM CREATE_CONCATENATE
                    USING P_TABNAME
                          P_VALUE
                    CHANGING P_RET.

  CONCATENATE
        P_TABNAME
        P_VALUE INTO P_RET.

ENDFORM.                    " CREATE_CONCATENATE
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
FORM RESULT_TRANSACTION_FOR_CRE
                            TABLES
                              PT_MSGDATA STRUCTURE BDCMSGCOLL
                            USING
                              P_TABNAME.

  DATA L_RETURN LIKE ZBDCTABLE-RETMSG.
  DATA L_RETMSG LIKE ZBDCTABLE-MESSAGE.

* ##### ##
  LOOP AT PT_MSGDATA
    WHERE
      MSGTYP  EQ C_MSGTY_S AND
      MSGSPRA EQ SY-LANGU  AND
      MSGID   EQ 'EU'  AND
      MSGNR EQ '000'.

    L_RETURN = C_MSGTY_S.  "##
    L_RETMSG = 'Succeed'.
    CLEAR PT_MSGDATA.
    EXIT.
  ENDLOOP.

* ###### ###
  IF SY-SUBRC NE 0.

*   ##### ##
    LOOP AT PT_MSGDATA
      WHERE
        MSGTYP  EQ C_MSGTY_E.  "####

      L_RETURN = C_MSGTY_E.  "##

      PERFORM MESSAGE_TEXT_BUILD
                              USING
                                PT_MSGDATA-MSGID
                                PT_MSGDATA-MSGNR
                                PT_MSGDATA-MSGTYP
                                PT_MSGDATA-MSGV1
                                PT_MSGDATA-MSGV2
                                PT_MSGDATA-MSGV3
                                PT_MSGDATA-MSGV4
                              CHANGING
                                L_RETMSG.
      CLEAR PT_MSGDATA.
      EXIT.
    ENDLOOP.

***    IF SY-SUBRC NE 0.
****    ###### ##
***
***      LOOP AT PT_MSGDATA
***        WHERE
***          MSGSPRA EQ SY-LANGU  AND
***          MSGID   EQ '00'      AND
***          ( MSGNR EQ '344' OR
***            MSGNR EQ '346' OR
***            MSGNR EQ '347' OR
***            MSGNR EQ '348' OR
***            MSGNR EQ '349' ).
***
***        PS_RETURN-RETCD = C_RETCD_E.
***
***        PERFORM MESSAGE_TEXT_BUILD
***                                USING
***                                  PT_MSGDATA-MSGID
***                                  PT_MSGDATA-MSGNR
***                                  PT_MSGDATA-MSGTYP
***                                  PT_MSGDATA-MSGV1
***                                  PT_MSGDATA-MSGV2
***                                  PT_MSGDATA-MSGV3
***                                  PT_MSGDATA-MSGV4
***                                CHANGING
***                                  PS_RETURN-RETMG.
***        CLEAR PT_MSGDATA.
***        EXIT.
***      ENDLOOP.
***
***      IF SY-SUBRC NE 0.
****     ########## ###
***
***        PS_RETURN-RETCD = C_RETCD_E.  "##
***        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
***
***      ENDIF.
***    ENDIF.
  ENDIF.

  IF L_RETURN IS NOT INITIAL.

    UPDATE ZBDCTABLE
       SET CRETYP = L_RETURN
           CREMES = L_RETMSG
     WHERE TABNAME = P_TABNAME.

    IF SY-SUBRC EQ 0.
      COMMIT WORK.

***      GT_LIST-CRETYP = L_RETURN.
***      GT_LIST-CREMES = L_RETMSG.
***
***      MODIFY GT_LIST TRANSPORTING CRETYP CREMES WHERE TABNAME =
***P_TABNAME .
    ENDIF.
    CLEAR L_RETURN.
  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_CRE
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_FOR_BDC_E
                            TABLES
                              PT_BDCDATA
                              PT_MSGDATA
                            USING
                              P_TCODE TYPE SY-TCODE.
  DATA: LS_OPTIONS LIKE CTU_PARAMS.
* OPTION
  LS_OPTIONS-DISMODE = 'E'.
  LS_OPTIONS-UPDMODE = 'S'.
  LS_OPTIONS-DEFSIZE = 'X'.
  LS_OPTIONS-RACOMMIT = 'X'.

  CALL TRANSACTION P_TCODE USING    PT_BDCDATA
                           OPTIONS  FROM LS_OPTIONS
                           MESSAGES INTO PT_MSGDATA.

ENDFORM.                    " CALL_TRANSACTION_FOR_BDC
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_ACR
*&---------------------------------------------------------------------*
FORM RESULT_TRANSACTION_FOR_PAR
                            TABLES
                              PT_MSGDATA STRUCTURE BDCMSGCOLL
                            USING
                              P_TABNAME.

  DATA L_RETURN LIKE ZBDCTABLE-RETMSG.
  DATA L_RETMSG LIKE ZBDCTABLE-MESSAGE.

* ##### ##
  LOOP AT PT_MSGDATA
    WHERE
      MSGTYP  EQ C_MSGTY_S AND
      MSGSPRA EQ SY-LANGU  AND
      MSGID   EQ 'SV'  AND
      MSGNR EQ '018'.

    L_RETURN = C_MSGTY_S.  "##
    L_RETMSG = 'Succeed'.
    CLEAR PT_MSGDATA.
    EXIT.
  ENDLOOP.

* ###### ###
  IF SY-SUBRC NE 0.

*   ##### ##
    LOOP AT PT_MSGDATA
      WHERE
        ( MSGTYP  EQ C_MSGTY_E OR   "####
          MSGTYP  EQ C_MSGTY_I ).
      L_RETURN = C_MSGTY_E.  "##

      PERFORM MESSAGE_TEXT_BUILD
                              USING
                                PT_MSGDATA-MSGID
                                PT_MSGDATA-MSGNR
                                PT_MSGDATA-MSGTYP
                                PT_MSGDATA-MSGV1
                                PT_MSGDATA-MSGV2
                                PT_MSGDATA-MSGV3
                                PT_MSGDATA-MSGV4
                              CHANGING
                                L_RETMSG.
      CLEAR PT_MSGDATA.
      EXIT.
    ENDLOOP.

***    IF SY-SUBRC NE 0.
****    ###### ##
***
***      LOOP AT PT_MSGDATA
***        WHERE
***          MSGSPRA EQ SY-LANGU  AND
***          MSGID   EQ '00'      AND
***          ( MSGNR EQ '344' OR
***            MSGNR EQ '346' OR
***            MSGNR EQ '347' OR
***            MSGNR EQ '348' OR
***            MSGNR EQ '349' ).
***
***        PS_RETURN-RETCD = C_RETCD_E.
***
***        PERFORM MESSAGE_TEXT_BUILD
***                                USING
***                                  PT_MSGDATA-MSGID
***                                  PT_MSGDATA-MSGNR
***                                  PT_MSGDATA-MSGTYP
***                                  PT_MSGDATA-MSGV1
***                                  PT_MSGDATA-MSGV2
***                                  PT_MSGDATA-MSGV3
***                                  PT_MSGDATA-MSGV4
***                                CHANGING
***                                  PS_RETURN-RETMG.
***        CLEAR PT_MSGDATA.
***        EXIT.
***      ENDLOOP.
***
***      IF SY-SUBRC NE 0.
****     ########## ###
***
***        PS_RETURN-RETCD = C_RETCD_E.  "##
***        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
***
***      ENDIF.
***    ENDIF.
  ENDIF.

  IF L_RETURN IS NOT INITIAL.

    UPDATE ZBDCTABLE
       SET PARTYP = L_RETURN
           PARMES = L_RETMSG
     WHERE TABNAME = P_TABNAME.

    IF SY-SUBRC EQ 0.
      COMMIT WORK.

***      GT_LIST-PARTYP = L_RETURN.
***      GT_LIST-PARMES = L_RETMSG.
***
***      MODIFY GT_LIST TRANSPORTING PARTYP PARMES WHERE TABNAME =
***P_TABNAME .
    ENDIF.
    CLEAR L_RETURN.
  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_PAR
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZBDCTABLE
*&---------------------------------------------------------------------*
FORM UPDATE_ZBDCTABLE
                  USING P_TABNAME.

  UPDATE ZBDCTABLE
     SET PARTYP = C_MSGTY_S
   WHERE TABNAME = P_TABNAME.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.

***    GT_LIST-PARTYP = C_MSGTY_S.
***
***    MODIFY GT_LIST TRANSPORTING PARTYP WHERE TABNAME = P_TABNAME .
  ENDIF.

ENDFORM.                    " UPDATE_ZBDCTABLE
*&---------------------------------------------------------------------*
*&      Form  SELECT_DD02T
*&---------------------------------------------------------------------*
FORM SELECT_DD02T  USING    P_TABNAME
                   CHANGING P_DDTEXT.

  DATA: L_DDTEXT  LIKE DD02T-DDTEXT.

  SELECT SINGLE
    DDTEXT
    FROM DD02T
    INTO L_DDTEXT
   WHERE TABNAME EQ P_TABNAME
     AND DDLANGUAGE EQ SY-LANGU.

  IF SY-SUBRC EQ 0.
    P_DDTEXT = L_DDTEXT.
  ENDIF.

ENDFORM.                    " SELECT_DD02T
*&---------------------------------------------------------------------*
*&      Form  RESULT_TRANSACTION_FOR_FICA
*&---------------------------------------------------------------------*
FORM RESULT_TRANSACTION_FOR_FICA
                            TABLES
                              PT_MSGDATA STRUCTURE BDCMSGCOLL
                            USING
                              P_TABNAME.

  DATA L_RETURN LIKE ZBDCTABLE-RETMSG.
  DATA L_RETMSG LIKE ZBDCTABLE-MESSAGE.

* ##### ##
  LOOP AT PT_MSGDATA
    WHERE
      MSGTYP  EQ C_MSGTY_S AND
      MSGSPRA EQ SY-LANGU  AND
      MSGID   EQ 'SV'      AND
      ( MSGNR EQ '004' OR
        MSGNR EQ '018' ).

    L_RETURN = C_MSGTY_S.  "##
    L_RETMSG = 'Succeed'.
    CLEAR PT_MSGDATA.
    EXIT.
  ENDLOOP.

* ###### ###
  IF SY-SUBRC NE 0.

*   ##### ##
    LOOP AT PT_MSGDATA
      WHERE
        MSGTYP  EQ C_MSGTY_E.  "####

      L_RETURN = C_MSGTY_E.  "##

      PERFORM MESSAGE_TEXT_BUILD
                              USING
                                PT_MSGDATA-MSGID
                                PT_MSGDATA-MSGNR
                                PT_MSGDATA-MSGTYP
                                PT_MSGDATA-MSGV1
                                PT_MSGDATA-MSGV2
                                PT_MSGDATA-MSGV3
                                PT_MSGDATA-MSGV4
                              CHANGING
                                L_RETMSG.
      CLEAR PT_MSGDATA.
      EXIT.
    ENDLOOP.

***    IF SY-SUBRC NE 0.
****    ###### ##
***
***      LOOP AT PT_MSGDATA
***        WHERE
***          MSGSPRA EQ SY-LANGU  AND
***          MSGID   EQ '00'      AND
***          ( MSGNR EQ '344' OR
***            MSGNR EQ '346' OR
***            MSGNR EQ '347' OR
***            MSGNR EQ '348' OR
***            MSGNR EQ '349' ).
***
***        PS_RETURN-RETCD = C_RETCD_E.
***
***        PERFORM MESSAGE_TEXT_BUILD
***                                USING
***                                  PT_MSGDATA-MSGID
***                                  PT_MSGDATA-MSGNR
***                                  PT_MSGDATA-MSGTYP
***                                  PT_MSGDATA-MSGV1
***                                  PT_MSGDATA-MSGV2
***                                  PT_MSGDATA-MSGV3
***                                  PT_MSGDATA-MSGV4
***                                CHANGING
***                                  PS_RETURN-RETMG.
***        CLEAR PT_MSGDATA.
***        EXIT.
***      ENDLOOP.
***
***      IF SY-SUBRC NE 0.
****     ########## ###
***
***        PS_RETURN-RETCD = C_RETCD_E.  "##
***        PS_RETURN-RETMG = C_UNKNOWN_ERROR.
***
***      ENDIF.
***    ENDIF.
  ENDIF.

  IF L_RETURN IS NOT INITIAL.

    UPDATE ZBDCTABLE
       SET FILTYP = L_RETURN
           FILMES = L_RETMSG
     WHERE TABNAME = P_TABNAME.

    IF SY-SUBRC EQ 0.
      COMMIT WORK.
***      GT_LIST-FILTYP = L_RETURN.
***      GT_LIST-FILMES = L_RETMSG.
***      MODIFY GT_LIST TRANSPORTING FILTYP FILMES WHERE TABNAME =
***P_TABNAME .
    ENDIF.
    CLEAR L_RETURN.
  ENDIF.

ENDFORM.                    " RESULT_TRANSACTION_FOR_FICA
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZHACT0020
*&---------------------------------------------------------------------*
FORM SELECT_ZHACT0020
                TABLES PT_ZHACT0020
                USING P_TABNAME.

  SELECT
    TABNAME
    FIELDNAME
    SERIAL
    KEYFLAG
    FROM ZHACT0020
    INTO CORRESPONDING FIELDS OF TABLE PT_ZHACT0020
   WHERE TABNAME EQ P_TABNAME.
***     AND KEYFLAG EQ C_FLAG_X
***     AND POSITION NE '0001'.

ENDFORM.                    " SELECT_ZHACT0020
*&---------------------------------------------------------------------*
*&      Form  MODIFY_BDC_INFOSTRUCTURE
*&---------------------------------------------------------------------*
FORM MODIFY_BDC_INFOSTRUCTURE
                          USING P_TABNAME.

  DATA:
    LT_BDCDATA LIKE BDCDATA OCCURS 10,
    LT_MSGDATA LIKE BDCMSGCOLL OCCURS 1.
  DATA L_FIELDCAT TYPE TABNAME.
  DATA L_OBJECT TYPE TABNAME.
  DATA L_DDTEXT  LIKE DD02T-DDTEXT.
  DATA L_FLDNR TYPE AIND_FLDNR.
  DATA L_LAST_FLAG.
  DATA L_ARCH_INFO TYPE AIND_DESC.
  DATA : BEGIN OF LT_DD03L OCCURS 0,
           TABNAME LIKE ZHACT0020-TABNAME,
           FIELDNAME LIKE ZHACT0020-FIELDNAME,
           POSITION LIKE ZHACT0020-SERIAL,
           KEYFLAG LIKE ZHACT0020-KEYFLAG,
         END OF LT_DD03L.

  PERFORM SELECT_DD02T
                    USING P_TABNAME
                    CHANGING L_DDTEXT.

***  PERFORM SELECT_DD03L
***                   TABLES LT_DD03L
***                   USING P_TABNAME.

  PERFORM SELECT_ZHACT0020
                       TABLES LT_DD03L
                       USING P_TABNAME.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*   Archive Information System: Central management
    'X'   'SAPLAINA'       '0100',
    ' '   'BDC_OKCODE'     '=CUST'.

  IF P_TABNAME(1) EQ 'Z'.
    L_OBJECT = P_TABNAME(10).
    CONCATENATE L_OBJECT '_001' INTO L_FIELDCAT.
  ELSE.
    L_OBJECT = P_TABNAME(10).
    CONCATENATE 'Z' L_OBJECT '_001' INTO L_FIELDCAT.
  ENDIF.
  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'         '1000',
    ' '   'AIND_STR1-ARCHINDEX' L_FIELDCAT,
    ' '   'BDC_OKCODE'         '=DELE'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*   Archive Retrieval Configurator
    'X'   'SAPLSPO1'           '0500',
***    ' '   'BDC_SUBSCR'         'SAPLSPO1
***   0501SUBSCREEN',
    ' '   'BDC_OKCODE'         '=OPT1'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPMSSY0'         '0120',
    ' '   'BDC_OKCODE'       '=&ONT'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLAS_ARC'         '1000',
    ' '   'AIND_STR1-ARCHINDEX' L_FIELDCAT,
    ' '   'BDC_OKCODE'         '=FCAT'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLARIC'           '0150',
    ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
    ' '   'BDC_OKCODE'         '=POSI'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLSPO4'           '0300',
    ' '   'BDC_CURSOR'         'SVALD-VALUE(01)',
    ' '   'SVALD-VALUE(01)'    L_FIELDCAT,
    ' '   'BDC_OKCODE'         '=FURT'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLARIC'           '0150',
    ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
    ' '   'VIM_MARKED(01)'     C_FLAG_X,
    ' '   'BDC_OKCODE'         '=DELE'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLARIC'           '0150',
    ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
    ' '   'BDC_OKCODE'         '=SAVE'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*   Change View "Field Catalogs": Overview
     'X'   'SAPLARIC'           '0150',
     ' '   'BDC_CURSOR'         'AIND_SV1S-ARCHINDEX(01)',
     ' '   'BDC_OKCODE'         '=NEWL'.

  IF P_TABNAME(1) EQ 'Z'.
    L_OBJECT = P_TABNAME(10).
    CONCATENATE L_OBJECT '_001' INTO L_FIELDCAT.
  ELSE.
    L_OBJECT = P_TABNAME(10).
    CONCATENATE 'Z' L_OBJECT '_001' INTO L_FIELDCAT.
  ENDIF.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*   New Entries: Overview of Added Entries
    'X'   'SAPLARIC'                 '0150',
    ' '   'AIND_SV1S-ARCHINDEX(01)'  L_FIELDCAT,
    ' '   'AIND_SV1S-TEXT(01)'       L_DDTEXT,
    ' '   'AIND_SV1S-OBJECT(01)'     L_OBJECT,
    ' '   'AIND_SV1S-FILEFIELD(01)'  'K',
    ' '   'AIND_SV1S-OFFSFIELD(01)'  'D',
    ' '   'BDC_OKCODE'               '=ATAB'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLSVCM'          '0100',
    ' '   'BDC_CURSOR'        'VIMDYNFLDS-DYN_LINE(02)',
    ' '   'BDC_OKCODE'        '=DETA'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLARIC'           '0350',
    ' '   'BDC_CURSOR'         'VIMDYNFLDS-DYN_LINE(01)',
    ' '   'BDC_OKCODE'         '=NEWL'.

  LOOP AT LT_DD03L.

    AT LAST.
      L_LAST_FLAG = C_FLAG_X.
    ENDAT.

    ADD 1 TO L_FLDNR.

    IF LT_DD03L-KEYFLAG EQ C_FLAG_X.

      PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*       Change View "Field Selection":Overview
        'X'   'SAPLARIC'                 '0350',
        ' '   'AIND_SV3ES-FLDNR(01)'     L_FLDNR,
        ' '   'AIND_SV3ES-FIELDNAME(01)' LT_DD03L-FIELDNAME,
        ' '   'AIND_SV3ES-REFTAB(01)'    LT_DD03L-TABNAME,
        ' '   'AIND_SV3ES-REFFIELD(01)'  LT_DD03L-FIELDNAME,
        ' '   'AIND_SV3ES-KEYFLAG(01)'   C_FLAG_X,
        ' '   'BDC_OKCODE'               '=NEXT'.

    ELSE.

      IF L_LAST_FLAG EQ C_FLAG_X.

        PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*         Change View "Field Selection":Overview
          'X'   'SAPLARIC'                 '0350',
          ' '   'AIND_SV3ES-FLDNR(01)'     L_FLDNR,
          ' '   'AIND_SV3ES-FIELDNAME(01)' LT_DD03L-FIELDNAME,
          ' '   'AIND_SV3ES-REFTAB(01)'    LT_DD03L-TABNAME,
          ' '   'AIND_SV3ES-REFFIELD(01)'  LT_DD03L-FIELDNAME,
          ' '   'BDC_OKCODE'               '=SAVE'.

        CLEAR L_LAST_FLAG.

      ELSE.

        PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*         Change View "Field Selection":Overview
          'X'   'SAPLARIC'                 '0350',
          ' '   'AIND_SV3ES-FLDNR(01)'     L_FLDNR,
          ' '   'AIND_SV3ES-FIELDNAME(01)' LT_DD03L-FIELDNAME,
          ' '   'AIND_SV3ES-REFTAB(01)'    LT_DD03L-TABNAME,
          ' '   'AIND_SV3ES-REFFIELD(01)'  LT_DD03L-FIELDNAME,
          ' '   'BDC_OKCODE'               '=NEXT'.

      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:

    'X'   'SAPLARIC'                 '0350',
    ' '   'BDC_OKCODE'               '=ENDE'.


  L_OBJECT = P_TABNAME(10).
  CONCATENATE L_OBJECT '_001' INTO L_ARCH_INFO.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'               '1000',
    ' '   'AIND_STR1-ARCHINDEX'      L_ARCH_INFO,
    ' '   'BDC_OKCODE'               '=NEWL'.

  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
*   Archive Retrieval Configurator
    'X'   'SAPLAS_ARC'               '1010',
***    ' '   'AIND_STR1-ARCHINDEX'      L_ARCH_INFO,
    ' '   'AIND_STR1T-TEXT'          L_DDTEXT,
    ' '   'AIND_STR1-OBJECT'         L_OBJECT,
    ' '   'AIND_STR1-SKEY'           L_FIELDCAT,
    ' '   'BDC_OKCODE'               '=NEWL'.


  G_BDCMODE = 'A'.
* CALL TRANSACTION
  PERFORM CALL_TRANSACTION_FOR_BDC
                                TABLES
                                  LT_BDCDATA
                                  LT_MSGDATA
                                USING
                                  C_TCODE_SARI.

ENDFORM.                    " MODIFY_BDC_INFOSTRUCTURE
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CONTROL2
*&---------------------------------------------------------------------*
FORM ALV_CL_CONTROL2.

  PERFORM ALV_CL_CREATE_OBJ_ALV_GRID
              USING    G_CONTAIN_RIGHT
              CHANGING G_ALV_GRID2.
  PERFORM ALV_CL_VARIANT2.
  PERFORM ALV_CL_LAYOUT2.
  PERFORM ALV_CL_REGISTER_EDIT_EVENT USING G_ALV_GRID2.
  PERFORM ALV_CL_EXCLUDE_FCODE2.
  PERFORM ALV_CL_CREATE_OBJ_EVENT_RECVR2.
  PERFORM ALV_CL_SET_FOR_INPUT USING G_ALV_GRID2.
* ###### #### ### ## ####.
  PERFORM ALV_CL_CELL_CONTROL2.
  PERFORM ALV_CL_SORT2.
  PERFORM ALV_CL_FIELDCATALOG_MERGE2.
  PERFORM ALV_CL_SET_FOR_FIRST_DISPLAY2.

ENDFORM.                    " ALV_CL_CONTROL2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_VARIANT2
*&---------------------------------------------------------------------*
FORM ALV_CL_VARIANT2.
  CLEAR GS_ALV_VARIANT2.
* ABAP ######
  GS_ALV_VARIANT2-REPORT   = SY-REPID.
  GS_ALV_VARIANT2-HANDLE   = 'LIS2'.
  GS_ALV_VARIANT2-USERNAME = SY-UNAME.

ENDFORM.                    " ALV_CL_VARIANT2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_LAYOUT2
*&---------------------------------------------------------------------*
FORM ALV_CL_LAYOUT2.
  DATA: L_TITLE TYPE LVC_TITLE.
  CLEAR GS_ALV_LAYOUT2.

* Set a layout for the grid control
  GS_ALV_LAYOUT2-CWIDTH_OPT = ' '.              "### ###
  GS_ALV_LAYOUT2-SEL_MODE   = 'D'.
  GS_ALV_LAYOUT2-TOTALS_BEF  = 'X'.
*  GS_ALV_LAYOUT2-NO_MERGING = 'X'.
*  GS_ALV_LAYOUT2-DETAILINIT = 'X'. "DISPLAY INITIAL VALUES ON DETAIL
*  GS_ALV_LAYOUT2-NO_KEYFIX  = ' '.
*  GS_ALV_LAYOUT2-BOX_FNAME  = 'MARK'.
  GS_ALV_LAYOUT2-SMALLTITLE = 'X'.
  GS_ALV_LAYOUT2-NO_ROWMARK = 'X'.
  GS_ALV_LAYOUT2-STYLEFNAME = 'CELLSTYL'.         "# ###
*  GS_ALV_LAYOUT2-INFO_FNAME = 'ROWSCOL'.         "# #
  GS_ALV_LAYOUT2-CTAB_FNAME = 'CELLSCOL'.         "# #
  GS_ALV_LAYOUT2-ZEBRA = ' '.

**** #### ##### #### ####
***  READ TABLE GT_SUM INDEX 1.
***  PERFORM GET_DETAIL_DATA USING GT_SUM-GUBUN.
***
***  CONCATENATE  '#'
***              GT_SUM-GTEXT
***              '('
***              G_LST_FILTEREDLINE
***              '/'
***              G_LST_TOTALLINE
***              ')'
***         INTO GS_ALV_LAYOUT2-GRID_TITLE SEPARATED BY SPACE.

ENDFORM.                    " ALV_CL_LAYOUT2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_EVENT_RECVR2
*&---------------------------------------------------------------------*
FORM ALV_CL_CREATE_OBJ_EVENT_RECVR2.
  CLEAR G_EVENT_RECEIVER2.

  CREATE OBJECT G_EVENT_RECEIVER2.
  SET HANDLER G_EVENT_RECEIVER2->HANDLE_TOOLBAR2       FOR G_ALV_GRID2.
  SET HANDLER G_EVENT_RECEIVER2->HANDLE_DOUBLE_CLICK2 FOR G_ALV_GRID2.
  SET HANDLER G_EVENT_RECEIVER2->HANDLE_USER_COMMAND2  FOR G_ALV_GRID2.
*  SET HANDLER G_EVENT_RECEIVER2->HANDLE_DATA_CHANGED2 FOR G_ALV_GRID2.
*  SET HANDLER G_EVENT_RECEIVER2->HANDLE_AFTER_USER_COMMAND2
*                                                      FOR G_ALV_GRID2.*
*  SET HANDLER G_EVENT_RECEIVER2->HANDLE_BUTTON_CLICK2 FOR G_ALV_GRID2.

ENDFORM.                    " ALV_CL_CREATE_OBJ_EVENT_RECVR2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_CONTROL2
*&---------------------------------------------------------------------*
FORM ALV_CL_CELL_CONTROL2.
*  DATA: LT_CELLSTYL TYPE LVC_S_STYL OCCURS 0 WITH HEADER LINE,
*        LT_CELLSCOL TYPE LVC_S_SCOL OCCURS 0 WITH HEADER LINE,
*        L_TABIX    TYPE SY-TABIX.
* ## # ##
* ## ## ### ### ### ####
* ### ### #### ##.
*  CHECK SCREEN_MODE EQ C_CHANGE.


*  LOOP AT GT_LIST.
*    L_TABIX = SY-TABIX.
*
*    CLEAR: GT_LIST-CELLSTYL,
*           GT_LIST-CELLSCOL.
*
**    IF GT_LIST-FBRST EQ C_FBRST_S OR
**       GT_LIST-FBRST EQ C_FBRST_X.
**      GT_LIST-MARK = SPACE.
**      PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
**         USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
**    ELSE.
*    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*       USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**    ENDIF.
*    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST-CELLSTYL.
*    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_LIST-CELLSCOL.
*    MODIFY GT_LIST INDEX L_TABIX TRANSPORTING
*                                      CELLSTYL
*                                      CELLSCOL.
*    REFRESH: LT_CELLSTYL,
*             LT_CELLSCOL.
*    CLEAR  : LT_CELLSTYL,
*             LT_CELLSCOL.
*    CLEAR: GT_LIST.
*  ENDLOOP.

  DATA: LT_CELLSTYL TYPE LVC_S_STYL OCCURS 0 WITH HEADER LINE,
         LT_CELLSCOL TYPE LVC_S_SCOL OCCURS 0 WITH HEADER LINE,
         L_TABIX TYPE SY-TABIX.

***  LOOP AT GT_LIST.
****    WHERE SEQNO EQ '0000'.
***    L_TABIX = SY-TABIX.
***
***    CLEAR: GT_LIST-CELLSTYL.
****    CLEAR: GT_DETL-CELLSCOL.
***
***    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
***       USING : 'BUTN' CL_GUI_ALV_GRID=>MC_STYLE_BUTTON,
***               'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
***
***    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST-CELLSTYL.
****    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_DETL-CELLSCOL.
***    MODIFY GT_LIST INDEX L_TABIX TRANSPORTING
***                                      CELLSTYL.
****                                      CELLSCOL.
***    REFRESH: LT_CELLSTYL.
****             LT_CELLSCOL.
***    CLEAR  : LT_CELLSTYL.
****             LT_CELLSCOL.
******    CLEAR: GT_DETL.
***  ENDLOOP.

ENDFORM.                    " ALV_CL_CELL_CONTROL2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SORT2
*&---------------------------------------------------------------------*
FORM ALV_CL_SORT2.
*  DATA: LS_SORT LIKE LVC_S_SORT.
*
*  LS_SORT-FIELDNAME = 'GUBUN'.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-GROUP     = ' '.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = ' '.
*  APPEND LS_SORT TO GT_SORT1.
*  CLEAR LS_SORT.
ENDFORM.                    " ALV_CL_SORT2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_FIELDCATALOG_MERGE2
*&---------------------------------------------------------------------*
FORM ALV_CL_FIELDCATALOG_MERGE2.
  DATA: L_TABIX TYPE SY-TABIX,
         LS_FDCAT LIKE LVC_S_FCAT.

  REFRESH GT_FDCAT2.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_BUFFER_ACTIVE    = 'X'
      I_BYPASSING_BUFFER = 'X'
      I_STRUCTURE_NAME   = 'ZHACS0050'
    CHANGING
      CT_FIELDCAT        = GT_FDCAT2[].

  LOOP AT GT_FDCAT2
          INTO LS_FDCAT.
    L_TABIX = SY-TABIX.
    LS_FDCAT-KEY        = SPACE.
    LS_FDCAT-FIX_COLUMN = SPACE.

*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'GUBUN'  OR
           'GTEXT' OR
           'BUTN' OR
           'EXMON'.
        LS_FDCAT-FIX_COLUMN = 'X'.
    ENDCASE.

    CASE LS_FDCAT-FIELDNAME.
      WHEN 'GUBUN' OR
           'EXMON' OR
           'WDATE' OR
           'GTEXT' OR
           'MARK' OR
           'BUTN'.
        LS_FDCAT-NO_OUT = 'X'.
    ENDCASE.

*   #####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'NAME'.
        LS_FDCAT-SCRTEXT_L    = 'Program'.
        LS_FDCAT-SCRTEXT_M    = 'Program'.
        LS_FDCAT-SCRTEXT_S    = 'Program'.
        LS_FDCAT-COLTEXT      = 'Program'.
        LS_FDCAT-OUTPUTLEN    = '12'.
      WHEN 'TEXT'.
        LS_FDCAT-SCRTEXT_L    = 'Program Description'.
        LS_FDCAT-SCRTEXT_M    = 'Program Description'.
        LS_FDCAT-SCRTEXT_S    = 'Program Description'.
        LS_FDCAT-COLTEXT      = 'Program Description'.
        LS_FDCAT-OUTPUTLEN    = '50'.
    ENDCASE.

    MODIFY GT_FDCAT2 FROM LS_FDCAT INDEX L_TABIX.
    CLEAR LS_FDCAT.

  ENDLOOP.
ENDFORM.                    " ALV_CL_FIELDCATALOG_MERGE2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SET_FOR_FIRST_DISPLAY2
*&---------------------------------------------------------------------*
FORM ALV_CL_SET_FOR_FIRST_DISPLAY2.

  CALL METHOD G_ALV_GRID2->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING
*      I_STRUCTURE_NAME     = ' '
       IS_LAYOUT            = GS_ALV_LAYOUT2
       IS_VARIANT           = GS_ALV_VARIANT2
       I_SAVE               = 'A'
       IT_TOOLBAR_EXCLUDING = GT_FCODE2
     CHANGING
       IT_FIELDCATALOG      = GT_FDCAT2[]
       IT_SORT              = GT_SORT2
       IT_OUTTAB            = GT_LIST_RIGHT[].

ENDFORM.                    " ALV_CL_SET_FOR_FIRST_DISPLAY2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_EXCLUDE_FCODE2
*&---------------------------------------------------------------------*
FORM ALV_CL_EXCLUDE_FCODE2.
* ### ### #### ## ### ####.
  REFRESH GT_FCODE2.
* FIELD-SYMBOLS : <TABLE> TYPE UI_FUNCTIONS.

*  DATA : LS_EXCLUDE   TYPE UI_FUNC.
*  DATA : L_TABLE_NAME LIKE FELD-NAME.

*  CONCATENATE 'PT_FCODE' '[]' INTO  L_TABLE_NAME.
*  ASSIGN     (L_TABLE_NAME)    TO <TABLE>.

  PERFORM ALV_CL_APPEND_EXCLUDE_FUNCTION
        TABLES GT_FCODE2 "<TABLE>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** ## #### **
        USING :
                CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO, " ####&LOCAL&UNDO
                CL_GUI_ALV_GRID=>MC_FC_AUF,      " #### &AUF
                CL_GUI_ALV_GRID=>MC_FC_AVERAGE,  " &AVERAGE
*                cl_gui_alv_grid=>mc_fc_back_classic,
                CL_GUI_ALV_GRID=>MC_FC_CALL_ABC, " &ABC
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
                CL_GUI_ALV_GRID=>MC_FC_DETAIL,
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
                CL_GUI_ALV_GRID=>MC_FC_HELP,
                CL_GUI_ALV_GRID=>MC_FC_INFO,
                CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " # ##
                CL_GUI_ALV_GRID=>MC_FC_HTML,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      " # ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           " ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW, " ####.
                CL_GUI_ALV_GRID=>MC_FC_MAINTAIN_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_MAXIMUM,
                CL_GUI_ALV_GRID=>MC_FC_MINIMUM,
                CL_GUI_ALV_GRID=>MC_FC_PC_FILE,
                CL_GUI_ALV_GRID=>MC_FC_PRINT,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_BACK,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_PREV,
                CL_GUI_ALV_GRID=>MC_FC_REFRESH,
                CL_GUI_ALV_GRID=>MC_FC_REPREP,
                CL_GUI_ALV_GRID=>MC_FC_SAVE_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL,
                CL_GUI_ALV_GRID=>MC_FC_SEND,
                CL_GUI_ALV_GRID=>MC_FC_SEPARATOR,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                CL_GUI_ALV_GRID=>MC_FC_SUBTOT,
*                CL_GUI_ALV_GRID=>MC_MB_SUM,
*                CL_GUI_ALV_GRID=>MC_FC_SUM,
                CL_GUI_ALV_GRID=>MC_FC_TO_OFFICE,
                CL_GUI_ALV_GRID=>MC_FC_TO_REP_TREE,
                CL_GUI_ALV_GRID=>MC_FC_UNFIX_COLUMNS,
                CL_GUI_ALV_GRID=>MC_FC_VIEWS,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
                CL_GUI_ALV_GRID=>MC_FC_WORD_PROCESSOR.

ENDFORM.                    " ALV_CL_EXCLUDE_FCODE2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_FILL_CELLSTYL
*&---------------------------------------------------------------------*
FORM ALV_CL_FILL_CELLSTYL TABLES PT_CELLSTYL
                   USING  P_FNAME
                          P_STYPE.

  DATA : LS_CELLSTYL TYPE LVC_S_STYL.
  LS_CELLSTYL-FIELDNAME = P_FNAME.
  LS_CELLSTYL-STYLE = P_STYPE.
*  LS_CELLSTYL-STYLE = CL_GUI_ALV_GRID=> MC_STYLE_ENABLED.
  INSERT LS_CELLSTYL INTO TABLE PT_CELLSTYL.
ENDFORM.                    " ALV_CL_FILL_CELLSTYL
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_SPLITTER
*&---------------------------------------------------------------------*
FORM ALV_CL_CREATE_SPLITTER.

  CREATE OBJECT G_SPLITTER1
    EXPORTING
      PARENT  = G_CUSTOM_CONTAINER1
      ROWS    = 1
      COLUMNS = 3.

  CALL METHOD G_SPLITTER1->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 1
      WIDTH = 30.

  CALL METHOD G_SPLITTER1->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 2
      WIDTH = 20.

  CALL METHOD G_SPLITTER1->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 3
      WIDTH = 50.

  G_CONTAIN_LEFT  = G_SPLITTER1->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
  G_CONTAIN_RIGHT = G_SPLITTER1->GET_CONTAINER( ROW = 1 COLUMN = 2 ).
  G_CONTAIN_RIGHT1 = G_SPLITTER1->GET_CONTAINER( ROW = 1 COLUMN = 3 ).

 CREATE OBJECT G_SPLITTER2
    EXPORTING
      PARENT  = G_CONTAIN_RIGHT1
      ROWS    = 2
      COLUMNS = 1.

  CALL METHOD G_SPLITTER2->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 1
      WIDTH = 50.

  G_CONTAIN_TOP   = G_SPLITTER2->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
  G_CONTAIN_BOTTOM = G_SPLITTER2->GET_CONTAINER( ROW = 2 COLUMN = 1 )
.

ENDFORM.                    " ALV_CL_CREATE_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  SELECT_ADMI_VARIA
*&---------------------------------------------------------------------*
FORM SELECT_ADMI_VARIA .

  DATA LT_ZHACS0050 LIKE ZHACS0050 OCCURS 0 WITH HEADER LINE.
  DATA: LT_ARCHTAB TYPE ADK_CCMS_TABLES WITH HEADER LINE.
  RANGES LR_ARCHTAB FOR DD02V-TABNAME.

  CALL FUNCTION 'ADK_CCMS_GET_TABLES'
    EXPORTING
      OBJECT                   = GT_LIST_LEFT-OBJECT
*     DELETE_TABLES_ONLY       = 'X'
    TABLES
      OBJECT_TABLES            = LT_ARCHTAB
   EXCEPTIONS
     OBJECT_NOT_FOUND         = 1
     OTHERS                   = 2.

  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_ARCHTAB.
    SET_RANGE LR_ARCHTAB 'I' 'EQ' LT_ARCHTAB-TABNAME ''.
  ENDLOOP.

  SELECT DISTINCT
    A~PROGNAME AS NAME
    A~TEXT
    FROM ZACTFITB4 AS A
      INNER JOIN ZACTTATB AS B
         ON A~TABNAME = B~TABNAME
      INNER JOIN ZHACT0100 AS C
         ON A~PROGNAME = C~PROGNAME
    INTO CORRESPONDING FIELDS OF TABLE LT_ZHACS0050
    WHERE B~TABNAME IN LR_ARCHTAB.

  IF SY-SUBRC EQ 0.
    REFRESH GT_LIST_RIGHT.
    REFRESH GT_LIST_TOP.
    REFRESH GT_LIST_BOTTOM.
    LOOP AT LT_ZHACS0050.
      MOVE-CORRESPONDING LT_ZHACS0050 TO GT_LIST_RIGHT.
      GT_LIST_RIGHT-NAME = LT_ZHACS0050-NAME.
      APPEND GT_LIST_RIGHT.
      CLEAR GT_LIST_RIGHT.
    ENDLOOP.
  ELSE.
    REFRESH GT_LIST_RIGHT.
    REFRESH GT_LIST_TOP.
    REFRESH GT_LIST_BOTTOM.
  ENDIF.

* ALV Refresh
  PERFORM ALV_CL_REFRESH_TABLE_DISPLAY
                          USING: G_ALV_GRID2
                                 G_REC_STABLE2,
                                 G_ALV_GRID3
                                 G_REC_STABLE3,
                                 G_ALV_GRID4
                                 G_REC_STABLE4.

ENDFORM.                    " SELECT_ADMI_VARIA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_ZHACR01000
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_ZHACR01000 .

  LEAVE TO TRANSACTION 'ZHACR01000'.

ENDFORM.                    " CALL_TRANSACTION_ZHACR01000
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CONTROL3
*&---------------------------------------------------------------------*
FORM ALV_CL_CONTROL3 .

  PERFORM ALV_CL_CREATE_OBJ_ALV_GRID
              USING    G_CONTAIN_TOP
              CHANGING G_ALV_GRID3.
  PERFORM ALV_CL_VARIANT3.
  PERFORM ALV_CL_LAYOUT3.
  PERFORM ALV_CL_REGISTER_EDIT_EVENT USING G_ALV_GRID3.
  PERFORM ALV_CL_EXCLUDE_FCODE3.
  PERFORM ALV_CL_CREATE_OBJ_EVENT_RECVR3.
  PERFORM ALV_CL_SET_FOR_INPUT USING G_ALV_GRID3.
* ###### #### ### ## ####.
  PERFORM ALV_CL_CELL_CONTROL3.
  PERFORM ALV_CL_SORT3.
  PERFORM ALV_CL_FIELDCATALOG_MERGE3.
  PERFORM ALV_CL_SET_FOR_FIRST_DISPLAY3.

ENDFORM.                    " ALV_CL_CONTROL3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CONTROL4
*&---------------------------------------------------------------------*
FORM ALV_CL_CONTROL4.

  PERFORM ALV_CL_CREATE_OBJ_ALV_GRID
              USING    G_CONTAIN_BOTTOM
              CHANGING G_ALV_GRID4.
  PERFORM ALV_CL_VARIANT4.
  PERFORM ALV_CL_LAYOUT4.
  PERFORM ALV_CL_REGISTER_EDIT_EVENT USING G_ALV_GRID4.
  PERFORM ALV_CL_EXCLUDE_FCODE4.
  PERFORM ALV_CL_CREATE_OBJ_EVENT_RECVR4.
  PERFORM ALV_CL_SET_FOR_INPUT USING G_ALV_GRID4.
* ###### #### ### ## ####.
  PERFORM ALV_CL_CELL_CONTROL4.
  PERFORM ALV_CL_SORT4.
  PERFORM ALV_CL_FIELDCATALOG_MERGE4.
  PERFORM ALV_CL_SET_FOR_FIRST_DISPLAY4.

ENDFORM.                    " ALV_CL_CONTROL4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_VARIANT3
*&---------------------------------------------------------------------*
FORM ALV_CL_VARIANT3.
  CLEAR GS_ALV_VARIANT3.
* ABAP ######
  GS_ALV_VARIANT3-REPORT   = SY-REPID.
  GS_ALV_VARIANT3-HANDLE   = 'LIS2'.
  GS_ALV_VARIANT3-USERNAME = SY-UNAME.

ENDFORM.                    " ALV_CL_VARIANT3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_LAYOUT3
*&---------------------------------------------------------------------*
FORM ALV_CL_LAYOUT3.
  DATA: L_TITLE TYPE LVC_TITLE.
  CLEAR GS_ALV_LAYOUT3.

* Set a layout for the grid control
*  GS_ALV_LAYOUT3-CWIDTH_OPT = 'X'.              "### ###
  GS_ALV_LAYOUT3-SEL_MODE   = 'D'.
  GS_ALV_LAYOUT3-TOTALS_BEF  = 'X'.
*  GS_ALV_LAYOUT3-NO_MERGING = 'X'.
*  GS_ALV_LAYOUT3-DETAILINIT = 'X'. "DISPLAY INITIAL VALUES ON DETAIL
*  GS_ALV_LAYOUT3-NO_KEYFIX  = ' '.
*  GS_ALV_LAYOUT3-BOX_FNAME  = 'MARK'.
  GS_ALV_LAYOUT3-SMALLTITLE = 'X'.
  GS_ALV_LAYOUT3-NO_ROWMARK = 'X'.
  GS_ALV_LAYOUT3-STYLEFNAME = 'CELLSTYL'.         "# ###
*  GS_ALV_LAYOUT3-INFO_FNAME = 'ROWSCOL'.         "# #
  GS_ALV_LAYOUT3-CTAB_FNAME = 'CELLSCOL'.         "# #
  GS_ALV_LAYOUT3-ZEBRA = ' '.

**** #### ##### #### ####
***  READ TABLE GT_SUM INDEX 1.
***  PERFORM GET_DETAIL_DATA USING GT_SUM-GUBUN.
***
***  CONCATENATE  '#'
***              GT_SUM-GTEXT
***              '('
***              G_LST_FILTEREDLINE
***              '/'
***              G_LST_TOTALLINE
***              ')'
***         INTO GS_ALV_LAYOUT3-GRID_TITLE SEPARATED BY SPACE.

ENDFORM.                    " ALV_CL_LAYOUT3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_EVENT_RECVR3
*&---------------------------------------------------------------------*
FORM ALV_CL_CREATE_OBJ_EVENT_RECVR3.
  CLEAR G_EVENT_RECEIVER3.

  CREATE OBJECT G_EVENT_RECEIVER3.
  SET HANDLER G_EVENT_RECEIVER3->HANDLE_TOOLBAR3       FOR G_ALV_GRID3.
  SET HANDLER G_EVENT_RECEIVER3->HANDLE_DOUBLE_CLICK3 FOR G_ALV_GRID3.
  SET HANDLER G_EVENT_RECEIVER3->HANDLE_USER_COMMAND3  FOR G_ALV_GRID3.
*  SET HANDLER G_EVENT_RECEIVER3->HANDLE_DATA_CHANGED3 FOR G_ALV_GRID3.
*  SET HANDLER G_EVENT_RECEIVER3->HANDLE_AFTER_USER_COMMAND3
*                                                      FOR G_ALV_GRID3.*
*  SET HANDLER G_EVENT_RECEIVER3->HANDLE_BUTTON_CLICK3 FOR G_ALV_GRID3.

ENDFORM.                    " ALV_CL_CREATE_OBJ_EVENT_RECVR3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_CONTROL3
*&---------------------------------------------------------------------*
FORM ALV_CL_CELL_CONTROL3.
*  DATA: LT_CELLSTYL TYPE LVC_S_STYL OCCURS 0 WITH HEADER LINE,
*        LT_CELLSCOL TYPE LVC_S_SCOL OCCURS 0 WITH HEADER LINE,
*        L_TABIX    TYPE SY-TABIX.
* ## # ##
* ## ## ### ### ### ####
* ### ### #### ##.
*  CHECK SCREEN_MODE EQ C_CHANGE.


*  LOOP AT GT_LIST.
*    L_TABIX = SY-TABIX.
*
*    CLEAR: GT_LIST-CELLSTYL,
*           GT_LIST-CELLSCOL.
*
**    IF GT_LIST-FBRST EQ C_FBRST_S OR
**       GT_LIST-FBRST EQ C_FBRST_X.
**      GT_LIST-MARK = SPACE.
**      PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
**         USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
**    ELSE.
*    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*       USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**    ENDIF.
*    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST-CELLSTYL.
*    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_LIST-CELLSCOL.
*    MODIFY GT_LIST INDEX L_TABIX TRANSPORTING
*                                      CELLSTYL
*                                      CELLSCOL.
*    REFRESH: LT_CELLSTYL,
*             LT_CELLSCOL.
*    CLEAR  : LT_CELLSTYL,
*             LT_CELLSCOL.
*    CLEAR: GT_LIST.
*  ENDLOOP.

  DATA: LT_CELLSTYL TYPE LVC_S_STYL OCCURS 0 WITH HEADER LINE,
         LT_CELLSCOL TYPE LVC_S_SCOL OCCURS 0 WITH HEADER LINE,
         L_TABIX TYPE SY-TABIX.

***  LOOP AT GT_LIST.
****    WHERE SEQNO EQ '0000'.
***    L_TABIX = SY-TABIX.
***
***    CLEAR: GT_LIST-CELLSTYL.
****    CLEAR: GT_DETL-CELLSCOL.
***
***    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
***       USING : 'BUTN' CL_GUI_ALV_GRID=>MC_STYLE_BUTTON,
***               'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
***
***    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST-CELLSTYL.
****    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_DETL-CELLSCOL.
***    MODIFY GT_LIST INDEX L_TABIX TRANSPORTING
***                                      CELLSTYL.
****                                      CELLSCOL.
***    REFRESH: LT_CELLSTYL.
****             LT_CELLSCOL.
***    CLEAR  : LT_CELLSTYL.
****             LT_CELLSCOL.
******    CLEAR: GT_DETL.
***  ENDLOOP.

ENDFORM.                    " ALV_CL_CELL_CONTROL3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SORT3
*&---------------------------------------------------------------------*
FORM ALV_CL_SORT3.
*  DATA: LS_SORT LIKE LVC_S_SORT.
*
*  LS_SORT-FIELDNAME = 'GUBUN'.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-GROUP     = ' '.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = ' '.
*  APPEND LS_SORT TO GT_SORT1.
*  CLEAR LS_SORT.
ENDFORM.                    " ALV_CL_SORT3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_FIELDCATALOG_MERGE3
*&---------------------------------------------------------------------*
FORM ALV_CL_FIELDCATALOG_MERGE3.
  DATA: L_TABIX TYPE SY-TABIX,
         LS_FDCAT LIKE LVC_S_FCAT.

  REFRESH GT_FDCAT3.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_BUFFER_ACTIVE    = 'X'
      I_BYPASSING_BUFFER = 'X'
      I_STRUCTURE_NAME   = 'ZHACS0060'
    CHANGING
      CT_FIELDCAT        = GT_FDCAT3[].

  LOOP AT GT_FDCAT3
          INTO LS_FDCAT.
    L_TABIX = SY-TABIX.
    LS_FDCAT-KEY        = SPACE.
    LS_FDCAT-FIX_COLUMN = SPACE.

    CASE LS_FDCAT-FIELDNAME.
      WHEN 'ACTIVE'.
        LS_FDCAT-NO_OUT = 'X'.
    ENDCASE.

*   #####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'ARCH_INDEX'.
        LS_FDCAT-SCRTEXT_L    = 'Info Structure'.
        LS_FDCAT-SCRTEXT_M    = 'Info Structure'.
        LS_FDCAT-SCRTEXT_S    = 'Info Structure'.
        LS_FDCAT-COLTEXT      = 'Info Structure'.
        LS_FDCAT-OUTPUTLEN    = '12'.
      WHEN 'TEXT'.
        LS_FDCAT-SCRTEXT_L    = 'Program Description'.
        LS_FDCAT-SCRTEXT_M    = 'Program Description'.
        LS_FDCAT-SCRTEXT_S    = 'Program Description'.
        LS_FDCAT-COLTEXT      = 'Program Description'.
        LS_FDCAT-OUTPUTLEN    = '35'.
      WHEN 'GENTAB'.
        LS_FDCAT-SCRTEXT_L    = 'Temp'.
        LS_FDCAT-SCRTEXT_M    = 'Temp'.
        LS_FDCAT-SCRTEXT_S    = 'Temp'.
        LS_FDCAT-COLTEXT      = 'Temp'.
        LS_FDCAT-OUTPUTLEN    = '10'.
      WHEN 'KBYTES'.
        LS_FDCAT-SCRTEXT_L    = 'KBytes'.
        LS_FDCAT-SCRTEXT_M    = 'KBytes'.
        LS_FDCAT-SCRTEXT_S    = 'KBytes'.
        LS_FDCAT-COLTEXT      = 'KBytes'.
      WHEN 'ACTIVE'.
        LS_FDCAT-SCRTEXT_L    = 'Status'.
        LS_FDCAT-SCRTEXT_M    = 'Status'.
        LS_FDCAT-SCRTEXT_S    = 'Status'.
        LS_FDCAT-COLTEXT      = 'Status'.
      WHEN 'ZICON'.
        LS_FDCAT-SCRTEXT_L    = 'Status'.
        LS_FDCAT-SCRTEXT_M    = 'Status'.
        LS_FDCAT-SCRTEXT_S    = 'Status'.
        LS_FDCAT-COLTEXT      = 'Status'.
        LS_FDCAT-OUTPUTLEN    = '10'.
    ENDCASE.

    MODIFY GT_FDCAT3 FROM LS_FDCAT INDEX L_TABIX.
    CLEAR LS_FDCAT.

  ENDLOOP.
ENDFORM.                    " ALV_CL_FIELDCATALOG_MERGE3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SET_FOR_FIRST_DISPLAY3
*&---------------------------------------------------------------------*
FORM ALV_CL_SET_FOR_FIRST_DISPLAY3.

  CALL METHOD G_ALV_GRID3->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING
*      I_STRUCTURE_NAME     = ' '
       IS_LAYOUT            = GS_ALV_LAYOUT3
       IS_VARIANT           = GS_ALV_VARIANT3
       I_SAVE               = 'A'
       IT_TOOLBAR_EXCLUDING = GT_FCODE3
     CHANGING
       IT_FIELDCATALOG      = GT_FDCAT3[]
       IT_SORT              = GT_SORT3
       IT_OUTTAB            = GT_LIST_TOP[].

ENDFORM.                    " ALV_CL_SET_FOR_FIRST_DISPLAY3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_EXCLUDE_FCODE3
*&---------------------------------------------------------------------*
FORM ALV_CL_EXCLUDE_FCODE3.
* ### ### #### ## ### ####.
  REFRESH GT_FCODE3.
* FIELD-SYMBOLS : <TABLE> TYPE UI_FUNCTIONS.

*  DATA : LS_EXCLUDE   TYPE UI_FUNC.
*  DATA : L_TABLE_NAME LIKE FELD-NAME.

*  CONCATENATE 'PT_FCODE' '[]' INTO  L_TABLE_NAME.
*  ASSIGN     (L_TABLE_NAME)    TO <TABLE>.

  PERFORM ALV_CL_APPEND_EXCLUDE_FUNCTION
        TABLES GT_FCODE3 "<TABLE>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** ## #### **
        USING :
                CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO, " ####&LOCAL&UNDO
                CL_GUI_ALV_GRID=>MC_FC_AUF,      " #### &AUF
                CL_GUI_ALV_GRID=>MC_FC_AVERAGE,  " &AVERAGE
*                cl_gui_alv_grid=>mc_fc_back_classic,
                CL_GUI_ALV_GRID=>MC_FC_CALL_ABC, " &ABC
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
                CL_GUI_ALV_GRID=>MC_FC_DETAIL,
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
                CL_GUI_ALV_GRID=>MC_FC_HELP,
                CL_GUI_ALV_GRID=>MC_FC_INFO,
                CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " # ##
                CL_GUI_ALV_GRID=>MC_FC_HTML,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      " # ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           " ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW, " ####.
                CL_GUI_ALV_GRID=>MC_FC_MAINTAIN_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_MAXIMUM,
                CL_GUI_ALV_GRID=>MC_FC_MINIMUM,
                CL_GUI_ALV_GRID=>MC_FC_PC_FILE,
                CL_GUI_ALV_GRID=>MC_FC_PRINT,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_BACK,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_PREV,
                CL_GUI_ALV_GRID=>MC_FC_REFRESH,
                CL_GUI_ALV_GRID=>MC_FC_REPREP,
                CL_GUI_ALV_GRID=>MC_FC_SAVE_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL,
                CL_GUI_ALV_GRID=>MC_FC_SEND,
                CL_GUI_ALV_GRID=>MC_FC_SEPARATOR,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                CL_GUI_ALV_GRID=>MC_FC_SUBTOT,
*                CL_GUI_ALV_GRID=>MC_MB_SUM,
*                CL_GUI_ALV_GRID=>MC_FC_SUM,
                CL_GUI_ALV_GRID=>MC_FC_TO_OFFICE,
                CL_GUI_ALV_GRID=>MC_FC_TO_REP_TREE,
                CL_GUI_ALV_GRID=>MC_FC_UNFIX_COLUMNS,
                CL_GUI_ALV_GRID=>MC_FC_VIEWS,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
                CL_GUI_ALV_GRID=>MC_FC_WORD_PROCESSOR.

ENDFORM.                    " ALV_CL_EXCLUDE_FCODE3
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_VARIANT4
*&---------------------------------------------------------------------*
FORM ALV_CL_VARIANT4.
  CLEAR GS_ALV_VARIANT4.
* ABAP ######
  GS_ALV_VARIANT4-REPORT   = SY-REPID.
  GS_ALV_VARIANT4-HANDLE   = 'LIS2'.
  GS_ALV_VARIANT4-USERNAME = SY-UNAME.

ENDFORM.                    " ALV_CL_VARIANT4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_LAYOUT4
*&---------------------------------------------------------------------*
FORM ALV_CL_LAYOUT4.
  DATA: L_TITLE TYPE LVC_TITLE.
  CLEAR GS_ALV_LAYOUT4.

* Set a layout for the grid control
  GS_ALV_LAYOUT4-CWIDTH_OPT = 'X'.              "### ###
  GS_ALV_LAYOUT4-SEL_MODE   = 'D'.
  GS_ALV_LAYOUT4-TOTALS_BEF  = 'X'.
*  GS_ALV_LAYOUT4-NO_MERGING = 'X'.
*  GS_ALV_LAYOUT4-DETAILINIT = 'X'. "DISPLAY INITIAL VALUES ON DETAIL
*  GS_ALV_LAYOUT4-NO_KEYFIX  = ' '.
*  GS_ALV_LAYOUT4-BOX_FNAME  = 'MARK'.
  GS_ALV_LAYOUT4-SMALLTITLE = 'X'.
  GS_ALV_LAYOUT4-NO_ROWMARK = 'X'.
  GS_ALV_LAYOUT4-STYLEFNAME = 'CELLSTYL'.         "# ###
*  GS_ALV_LAYOUT4-INFO_FNAME = 'ROWSCOL'.         "# #
  GS_ALV_LAYOUT4-CTAB_FNAME = 'CELLSCOL'.         "# #
  GS_ALV_LAYOUT4-ZEBRA = ' '.

**** #### ##### #### ####
***  READ TABLE GT_SUM INDEX 1.
***  PERFORM GET_DETAIL_DATA USING GT_SUM-GUBUN.
***
***  CONCATENATE  '#'
***              GT_SUM-GTEXT
***              '('
***              G_LST_FILTEREDLINE
***              '/'
***              G_LST_TOTALLINE
***              ')'
***         INTO GS_ALV_LAYOUT4-GRID_TITLE SEPARATED BY SPACE.

ENDFORM.                    " ALV_CL_LAYOUT4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CREATE_OBJ_EVENT_RECVR4
*&---------------------------------------------------------------------*
FORM ALV_CL_CREATE_OBJ_EVENT_RECVR4.
  CLEAR G_EVENT_RECEIVER4.

  CREATE OBJECT G_EVENT_RECEIVER4.
  SET HANDLER G_EVENT_RECEIVER4->HANDLE_TOOLBAR4       FOR G_ALV_GRID4.
*  SET HANDLER G_EVENT_RECEIVER4->HANDLE_DOUBLE_CLICK4 FOR G_ALV_GRID4.
  SET HANDLER G_EVENT_RECEIVER4->HANDLE_USER_COMMAND4  FOR G_ALV_GRID4.
*  SET HANDLER G_EVENT_RECEIVER4->HANDLE_DATA_CHANGED4 FOR G_ALV_GRID4.
*  SET HANDLER G_EVENT_RECEIVER4->HANDLE_AFTER_USER_COMMAND4
*                                                      FOR G_ALV_GRID4.*
*  SET HANDLER G_EVENT_RECEIVER4->HANDLE_BUTTON_CLICK4 FOR G_ALV_GRID4.

ENDFORM.                    " ALV_CL_CREATE_OBJ_EVENT_RECVR4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_CONTROL4
*&---------------------------------------------------------------------*
FORM ALV_CL_CELL_CONTROL4.
*  DATA: LT_CELLSTYL TYPE LVC_S_STYL OCCURS 0 WITH HEADER LINE,
*        LT_CELLSCOL TYPE LVC_S_SCOL OCCURS 0 WITH HEADER LINE,
*        L_TABIX    TYPE SY-TABIX.
* ## # ##
* ## ## ### ### ### ####
* ### ### #### ##.
*  CHECK SCREEN_MODE EQ C_CHANGE.


*  LOOP AT GT_LIST.
*    L_TABIX = SY-TABIX.
*
*    CLEAR: GT_LIST-CELLSTYL,
*           GT_LIST-CELLSCOL.
*
**    IF GT_LIST-FBRST EQ C_FBRST_S OR
**       GT_LIST-FBRST EQ C_FBRST_X.
**      GT_LIST-MARK = SPACE.
**      PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
**         USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
**    ELSE.
*    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
*       USING : 'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**    ENDIF.
*    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST-CELLSTYL.
*    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_LIST-CELLSCOL.
*    MODIFY GT_LIST INDEX L_TABIX TRANSPORTING
*                                      CELLSTYL
*                                      CELLSCOL.
*    REFRESH: LT_CELLSTYL,
*             LT_CELLSCOL.
*    CLEAR  : LT_CELLSTYL,
*             LT_CELLSCOL.
*    CLEAR: GT_LIST.
*  ENDLOOP.

  DATA: LT_CELLSTYL TYPE LVC_S_STYL OCCURS 0 WITH HEADER LINE,
         LT_CELLSCOL TYPE LVC_S_SCOL OCCURS 0 WITH HEADER LINE,
         L_TABIX TYPE SY-TABIX.

***  LOOP AT GT_LIST.
****    WHERE SEQNO EQ '0000'.
***    L_TABIX = SY-TABIX.
***
***    CLEAR: GT_LIST-CELLSTYL.
****    CLEAR: GT_DETL-CELLSCOL.
***
***    PERFORM ALV_CL_FILL_CELLSTYL TABLES LT_CELLSTYL
***       USING : 'BUTN' CL_GUI_ALV_GRID=>MC_STYLE_BUTTON,
***               'MARK' CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
***
***    INSERT LINES OF LT_CELLSTYL INTO TABLE GT_LIST-CELLSTYL.
****    INSERT LINES OF LT_CELLSCOL INTO TABLE GT_DETL-CELLSCOL.
***    MODIFY GT_LIST INDEX L_TABIX TRANSPORTING
***                                      CELLSTYL.
****                                      CELLSCOL.
***    REFRESH: LT_CELLSTYL.
****             LT_CELLSCOL.
***    CLEAR  : LT_CELLSTYL.
****             LT_CELLSCOL.
******    CLEAR: GT_DETL.
***  ENDLOOP.

ENDFORM.                    " ALV_CL_CELL_CONTROL4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SORT4
*&---------------------------------------------------------------------*
FORM ALV_CL_SORT4.
*  DATA: LS_SORT LIKE LVC_S_SORT.
*
*  LS_SORT-FIELDNAME = 'GUBUN'.
*  LS_SORT-SPOS      = 1.
*  LS_SORT-GROUP     = ' '.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = ' '.
*  APPEND LS_SORT TO GT_SORT1.
*  CLEAR LS_SORT.
ENDFORM.                    " ALV_CL_SORT4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_FIELDCATALOG_MERGE4
*&---------------------------------------------------------------------*
FORM ALV_CL_FIELDCATALOG_MERGE4.
  DATA: L_TABIX TYPE SY-TABIX,
         LS_FDCAT LIKE LVC_S_FCAT.

  REFRESH GT_FDCAT4.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_BUFFER_ACTIVE    = 'X'
      I_BYPASSING_BUFFER = 'X'
      I_STRUCTURE_NAME   = 'ZHACS0070'
    CHANGING
      CT_FIELDCAT        = GT_FDCAT4[].

  LOOP AT GT_FDCAT4
          INTO LS_FDCAT.
    L_TABIX = SY-TABIX.
    LS_FDCAT-KEY        = SPACE.
    LS_FDCAT-FIX_COLUMN = SPACE.

*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'GUBUN'  OR
           'GTEXT' OR
           'BUTN' OR
           'EXMON'.
        LS_FDCAT-FIX_COLUMN = 'X'.
    ENDCASE.

    CASE LS_FDCAT-FIELDNAME.
      WHEN 'STATUS' OR
           'EXMON' OR
           'WDATE' OR
           'GTEXT' OR
           'MARK' OR
           'BUTN'.
        LS_FDCAT-NO_OUT = 'X'.
    ENDCASE.

*   #####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'ZICON'.
        LS_FDCAT-SCRTEXT_L    = 'Fill Status'.
        LS_FDCAT-SCRTEXT_M    = 'Fill Status'.
        LS_FDCAT-SCRTEXT_S    = 'Fill Status'.
        LS_FDCAT-COLTEXT      = 'Fill Status'.
        LS_FDCAT-OUTPUTLEN    = '12'.
      WHEN 'DOCUMENT'.
        LS_FDCAT-SCRTEXT_L    = 'Runnumber'.
        LS_FDCAT-SCRTEXT_M    = 'Runnumber'.
        LS_FDCAT-SCRTEXT_S    = 'Runnumber'.
        LS_FDCAT-COLTEXT      = 'Runnumber'.
        LS_FDCAT-OUTPUTLEN    = '50'.
    ENDCASE.

    MODIFY GT_FDCAT4 FROM LS_FDCAT INDEX L_TABIX.
    CLEAR LS_FDCAT.

  ENDLOOP.
ENDFORM.                    " ALV_CL_FIELDCATALOG_MERGE4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_SET_FOR_FIRST_DISPLAY4
*&---------------------------------------------------------------------*
FORM ALV_CL_SET_FOR_FIRST_DISPLAY4.

  CALL METHOD G_ALV_GRID4->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING
*      I_STRUCTURE_NAME     = ' '
       IS_LAYOUT            = GS_ALV_LAYOUT4
       IS_VARIANT           = GS_ALV_VARIANT4
       I_SAVE               = 'A'
       IT_TOOLBAR_EXCLUDING = GT_FCODE4
     CHANGING
       IT_FIELDCATALOG      = GT_FDCAT4[]
       IT_SORT              = GT_SORT4
       IT_OUTTAB            = GT_LIST_BOTTOM[].

ENDFORM.                    " ALV_CL_SET_FOR_FIRST_DISPLAY4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_EXCLUDE_FCODE4
*&---------------------------------------------------------------------*
FORM ALV_CL_EXCLUDE_FCODE4.
* ### ### #### ## ### ####.
  REFRESH GT_FCODE4.
* FIELD-SYMBOLS : <TABLE> TYPE UI_FUNCTIONS.

*  DATA : LS_EXCLUDE   TYPE UI_FUNC.
*  DATA : L_TABLE_NAME LIKE FELD-NAME.

*  CONCATENATE 'PT_FCODE' '[]' INTO  L_TABLE_NAME.
*  ASSIGN     (L_TABLE_NAME)    TO <TABLE>.

  PERFORM ALV_CL_APPEND_EXCLUDE_FUNCTION
        TABLES GT_FCODE4 "<TABLE>
*        USING : cl_gui_alv_grid=>mc_fc_excl_all. " ** ## #### **
        USING :
                CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO, " ####&LOCAL&UNDO
                CL_GUI_ALV_GRID=>MC_FC_AUF,      " #### &AUF
                CL_GUI_ALV_GRID=>MC_FC_AVERAGE,  " &AVERAGE
*                cl_gui_alv_grid=>mc_fc_back_classic,
                CL_GUI_ALV_GRID=>MC_FC_CALL_ABC, " &ABC
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
                CL_GUI_ALV_GRID=>MC_FC_DETAIL,
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
                CL_GUI_ALV_GRID=>MC_FC_HELP,
                CL_GUI_ALV_GRID=>MC_FC_INFO,
                CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " # ##
                CL_GUI_ALV_GRID=>MC_FC_HTML,
                CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      " # ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           " ##.
                CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    " ###.
                CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
                CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         " ####.
                CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW, " ####.
                CL_GUI_ALV_GRID=>MC_FC_MAINTAIN_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_MAXIMUM,
                CL_GUI_ALV_GRID=>MC_FC_MINIMUM,
                CL_GUI_ALV_GRID=>MC_FC_PC_FILE,
                CL_GUI_ALV_GRID=>MC_FC_PRINT,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_BACK,
                CL_GUI_ALV_GRID=>MC_FC_PRINT_PREV,
                CL_GUI_ALV_GRID=>MC_FC_REFRESH,
                CL_GUI_ALV_GRID=>MC_FC_REPREP,
                CL_GUI_ALV_GRID=>MC_FC_SAVE_VARIANT,
                CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL,
                CL_GUI_ALV_GRID=>MC_FC_SEND,
                CL_GUI_ALV_GRID=>MC_FC_SEPARATOR,
*                cl_gui_alv_grid=>mc_fc_sort,
*                cl_gui_alv_grid=>mc_fc_sort_asc,
*                cl_gui_alv_grid=>mc_fc_sort_dsc,
*                CL_GUI_ALV_GRID=>MC_FC_SUBTOT,
*                CL_GUI_ALV_GRID=>MC_MB_SUM,
*                CL_GUI_ALV_GRID=>MC_FC_SUM,
                CL_GUI_ALV_GRID=>MC_FC_TO_OFFICE,
                CL_GUI_ALV_GRID=>MC_FC_TO_REP_TREE,
                CL_GUI_ALV_GRID=>MC_FC_UNFIX_COLUMNS,
                CL_GUI_ALV_GRID=>MC_FC_VIEWS,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
                CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
                CL_GUI_ALV_GRID=>MC_FC_WORD_PROCESSOR.

ENDFORM.                    " ALV_CL_EXCLUDE_FCODE4
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_DOUBLE_CLICK2
*&---------------------------------------------------------------------*
FORM ALV_CL_CELL_DOUBLE_CLICK2  USING
                              P_ROW TYPE LVC_S_ROW
                              P_COLUMN TYPE LVC_S_COL.

  DATA: BEGIN OF LT_LIST OCCURS 0,
          APPLIC LIKE ZHACT0100-APPLIC,
          PROGNAME LIKE ZHACT0100-PROGNAME,
*          ZSEQ LIKE ZHACT0100-ZSEQ,
          ARCHINDEX LIKE ZHACT0100-ARCHINDEX,
          GENTAB LIKE AIND_STR2-GENTAB,
          ACTIVE LIKE AIND_STR2-ACTIVE,
          TEXT LIKE AIND_STR1T-TEXT,
        END OF LT_LIST.
  DATA LT_DBA_SEGMENTS LIKE SEGMENTS_F OCCURS 0 WITH HEADER LINE.
  DATA L_SEG_NAME TYPE SEGMENTS_F-SN.
  READ TABLE GT_LIST_RIGHT INDEX P_ROW-INDEX.

  SELECT DISTINCT
    A~APPLIC
    A~PROGNAME
*    ZSEQ
    A~ARCHINDEX
    B~GENTAB
    B~ACTIVE
    C~TEXT
    FROM ZHACT0100 AS A
      LEFT OUTER JOIN AIND_STR2 AS B
        ON A~ARCHINDEX EQ B~ARCHINDEX
      LEFT OUTER JOIN AIND_STR1T AS C
        ON A~ARCHINDEX EQ C~ARCHINDEX
       AND C~SPRAS EQ SY-LANGU
       AND C~ITYPE EQ 'I'
    INTO CORRESPONDING FIELDS OF TABLE LT_LIST
   WHERE PROGNAME EQ GT_LIST_RIGHT-NAME.

  IF SY-SUBRC EQ 0.
  REFRESH GT_LIST_TOP.

  LOOP AT LT_LIST.
    MOVE-CORRESPONDING LT_LIST TO GT_LIST_TOP.

    L_SEG_NAME = LT_LIST-GENTAB.
    CALL FUNCTION 'DB02_ORA_SELECT_SEGMENTS'
      EXPORTING
        SEG_NAME               = L_SEG_NAME
*       TB_SPACE               = '*'
*       SEG_TYPE               = '*'
*       KBYTES                 = 0
*       NR_EXTENTS             = 0
*       WITH_BITMAP            = ' '
*       WITH_COMPRESSION       = ' '
      TABLES
        DBA_SEGMENTS           = LT_DBA_SEGMENTS.


      READ TABLE LT_DBA_SEGMENTS INDEX 1.
      GT_LIST_TOP-KBYTES = LT_DBA_SEGMENTS-KBYTES.

    IF LT_LIST-ACTIVE EQ C_FLAG_X.
      GT_LIST_TOP-ZICON = '@08@'.
    ENDIF.
    APPEND GT_LIST_TOP.
    CLEAR GT_LIST_TOP.
  ENDLOOP.
  ELSE.
    REFRESH: GT_LIST_TOP,
             GT_LIST_BOTTOM.
  ENDIF.
* ALV Refresh
  PERFORM ALV_CL_REFRESH_TABLE_DISPLAY
                          USING  G_ALV_GRID3
                                 G_REC_STABLE3.

ENDFORM.                    " ALV_CL_CELL_DOUBLE_CLICK2
*&---------------------------------------------------------------------*
*&      Form  ALV_CL_CELL_DOUBLE_CLICK3
*&---------------------------------------------------------------------*
FORM ALV_CL_CELL_DOUBLE_CLICK3   USING
                              P_ROW TYPE LVC_S_ROW
                              P_COLUMN TYPE LVC_S_COL.

  DATA L_OBJECT LIKE AIND_STR1-OBJECT.
  DATA LT_LIST LIKE ZHACS0070 OCCURS 0 WITH HEADER LINE.
  DATA L_AIND_STATU.

  READ TABLE GT_LIST_TOP INDEX P_ROW-INDEX.

  SELECT SINGLE
    OBJECT
    FROM AIND_STR1
    INTO L_OBJECT
   WHERE ARCHINDEX EQ GT_LIST_TOP-ARCHINDEX
     AND ITYPE EQ 'I'.

  SELECT DISTINCT
***    A~STATUS
    A~DOCUMENT
    A~CREAT_DATE
    A~COMMENTS
    C~AIND_STATU AS STATUS
    FROM ADMI_RUN AS A
      INNER JOIN ADMI_FILES AS B
         ON A~DOCUMENT EQ B~DOCUMENT
      LEFT OUTER JOIN AIND_STATU AS C
         ON B~ARCHIV_KEY EQ C~ARCHIVEKEY
        AND C~ARCHINDEX EQ GT_LIST_TOP-ARCHINDEX
    INTO CORRESPONDING FIELDS OF TABLE LT_LIST
   WHERE OBJECT EQ L_OBJECT
     AND DELETE_FLG EQ SPACE.

   IF SY-SUBRC EQ 0.
     REFRESH GT_LIST_BOTTOM.

     SORT LT_LIST BY DOCUMENT.

     LOOP AT LT_LIST.
       MOVE-CORRESPONDING LT_LIST TO GT_LIST_BOTTOM.

       IF LT_LIST-STATUS EQ C_FLAG_X.
         GT_LIST_BOTTOM-STATUS = '1'.
         GT_LIST_BOTTOM-ZICON = '@08@'.
       ELSE.
         GT_LIST_BOTTOM-STATUS = '0'.
         GT_LIST_BOTTOM-ZICON = '@0A@'.
       ENDIF.

       APPEND GT_LIST_BOTTOM.
       CLEAR GT_LIST_BOTTOM.
     ENDLOOP.
   ELSE.
     REFRESH GT_LIST_BOTTOM.
   ENDIF.

  SORT GT_LIST_BOTTOM BY DOCUMENT DESCENDING.
* ALV Refresh
  PERFORM ALV_CL_REFRESH_TABLE_DISPLAY
                          USING  G_ALV_GRID4
                                 G_REC_STABLE4.

ENDFORM.                    " ALV_CL_CELL_DOUBLE_CLICK3
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_SARI
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_SARI .

  LEAVE TO TRANSACTION 'SARI'.

ENDFORM.                    " CALL_TRANSACTION_SARI
