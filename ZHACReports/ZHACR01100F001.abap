*&---------------------------------------------------------------------*
*&  Include           ZHACR01000F001
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM START_OF_SELECTION.

  DATA L_APPLIC TYPE APPLI_TR01.
  DATA L_OBJECT TYPE AIND_OBJ.

  GET PARAMETER ID 'API' FIELD L_APPLIC.
  GET PARAMETER ID 'OBT' FIELD L_OBJECT.

  IF L_APPLIC IS NOT INITIAL.
    SET_RANGE S_APPLIC 'I' 'EQ' L_APPLIC ''.
    SET_RANGE S_OBJECT 'I' 'EQ' L_OBJECT ''.
    SET PARAMETER ID 'API' FIELD SPACE.
    SET PARAMETER ID 'OBT' FIELD SPACE.

    PERFORM DATA_PROCESSING.

  ENDIF.
ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESSING
*&---------------------------------------------------------------------*
FORM DATA_PROCESSING.

  DATA LT_ADMI_RUN LIKE GA_LIST OCCURS 0 WITH HEADER LINE.

  PERFORM SELECT_ADMI_RUN
                      TABLES LT_ADMI_RUN.

  PERFORM MOVE_DATA_TO_LIST
                      TABLES LT_ADMI_RUN.

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
*  GS_LAYOUT1-CWIDTH_OPT = 'X'.              "### ###
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
      I_STRUCTURE_NAME   = 'ZHACS0020'
    CHANGING
      CT_FIELDCAT        = GT_FDCAT1[].

  LOOP AT GT_FDCAT1
          INTO LS_FDCAT.
    L_TABIX = SY-TABIX.
    LS_FDCAT-KEY        = SPACE.
    LS_FDCAT-FIX_COLUMN = SPACE.

*   ######
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'ARCSTATUS' OR
           'ZFORMAT_TYP' OR
           'STATUS' OR
           'STATUS_FIL' OR
           'STATUS_OPT' OR
           'FILE_SIZE3'.
        LS_FDCAT-NO_OUT = 'X'.
    ENDCASE.

    CASE LS_FDCAT-FIELDNAME.
      WHEN 'FILE_SIZE1' OR
           'FILE_SIZE2' OR
           'FILE_SIZE3'.
        LS_FDCAT-DECIMALS_O = '3'.
        LS_FDCAT-EXPONENT = '0'.
    ENDCASE.
*   KEY####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'OBJECT'.
        LS_FDCAT-EMPHASIZE = 'C410'.
      WHEN 'FIELDNAME' OR
           'COMMENTS' OR
           'VARIANTWRI'.
        LS_FDCAT-EMPHASIZE = 'C510'.
    ENDCASE.

*   #####
    CASE LS_FDCAT-FIELDNAME.
      WHEN 'APPLIC'.
        LS_FDCAT-SCRTEXT_L    = 'Module'.
        LS_FDCAT-SCRTEXT_M    = 'Module'.
        LS_FDCAT-SCRTEXT_S    = 'Module'.
        LS_FDCAT-COLTEXT      = 'Module'.
        LS_FDCAT-OUTPUTLEN    = '4'.
      WHEN 'OBJECT'.
        LS_FDCAT-SCRTEXT_L    = 'Object Name'.
        LS_FDCAT-SCRTEXT_M    = 'Object Name'.
        LS_FDCAT-SCRTEXT_S    = 'Object Name'.
        LS_FDCAT-COLTEXT      = 'Object Name'.
        LS_FDCAT-OUTPUTLEN    = '11'.
      WHEN 'OBJTEXT'.
        LS_FDCAT-SCRTEXT_L    = 'Object Description'.
        LS_FDCAT-SCRTEXT_M    = 'Object Description'.
        LS_FDCAT-SCRTEXT_S    = 'Object Description'.
        LS_FDCAT-COLTEXT      = 'Object Description'.
        LS_FDCAT-OUTPUTLEN    = '10'.
      WHEN 'DOCUMENT'.
        LS_FDCAT-SCRTEXT_L    = 'Run Number'.
        LS_FDCAT-SCRTEXT_M    = 'Run Number'.
        LS_FDCAT-SCRTEXT_S    = 'Run Number'.
        LS_FDCAT-COLTEXT      = 'Run Number'.
        LS_FDCAT-OUTPUTLEN    = '4'.
     WHEN 'CREAT_DATE'.
        LS_FDCAT-SCRTEXT_L    = 'Date'.
        LS_FDCAT-SCRTEXT_M    = 'Date'.
        LS_FDCAT-SCRTEXT_S    = 'Date'.
        LS_FDCAT-COLTEXT      = 'Date'.
        LS_FDCAT-OUTPUTLEN    = '6'.
     WHEN 'STATUS'.
        LS_FDCAT-SCRTEXT_L    = 'Write Status'.
        LS_FDCAT-SCRTEXT_M    = 'Write Status'.
        LS_FDCAT-SCRTEXT_S    = 'Write Status'.
        LS_FDCAT-COLTEXT      = 'Write Status'.
        LS_FDCAT-OUTPUTLEN    = '4'.
      WHEN 'STATUS_FIL'.
        LS_FDCAT-SCRTEXT_L    = 'Delete Status'.
        LS_FDCAT-SCRTEXT_M    = 'Delete Status'.
        LS_FDCAT-SCRTEXT_S    = 'Delete Status'.
        LS_FDCAT-COLTEXT      = 'Delete Status'.
        LS_FDCAT-OUTPUTLEN    = '1'.
      WHEN 'STATUS_OPT'.
        LS_FDCAT-SCRTEXT_L    = 'Storage Status'.
        LS_FDCAT-SCRTEXT_M    = 'Storage Status'.
        LS_FDCAT-SCRTEXT_S    = 'Storage Status'.
        LS_FDCAT-COLTEXT      = 'Storage Status'.
        LS_FDCAT-OUTPUTLEN    = '4'.
      WHEN 'ZICON1'.
        LS_FDCAT-SCRTEXT_L    = 'Write Status'.
        LS_FDCAT-SCRTEXT_M    = 'Write Status'.
        LS_FDCAT-SCRTEXT_S    = 'Write Status'.
        LS_FDCAT-COLTEXT      = 'Write Status'.
        LS_FDCAT-OUTPUTLEN    = '4'.
      WHEN 'ZICON2'.
        LS_FDCAT-SCRTEXT_L    = 'Delete Status'.
        LS_FDCAT-SCRTEXT_M    = 'Delete Status'.
        LS_FDCAT-SCRTEXT_S    = 'Delete Status'.
        LS_FDCAT-COLTEXT      = 'Delete Status'.
        LS_FDCAT-OUTPUTLEN    = '1'.
      WHEN 'ZICON3'.
        LS_FDCAT-SCRTEXT_L    = 'Storage Status'.
        LS_FDCAT-SCRTEXT_M    = 'Storage Status'.
        LS_FDCAT-SCRTEXT_S    = 'Storage Status'.
        LS_FDCAT-COLTEXT      = 'Storage Status'.
        LS_FDCAT-OUTPUTLEN    = '4'.
      WHEN 'FILE_SIZE1'.
        LS_FDCAT-SCRTEXT_L    = 'DB space(Write)in MB'.
        LS_FDCAT-SCRTEXT_M    = 'DB space(Write)in MB'.
        LS_FDCAT-SCRTEXT_S    = 'DB space(Write)in MB'.
        LS_FDCAT-COLTEXT      = 'DB space(Write)in MB'.
        LS_FDCAT-OUTPUTLEN    = '7'.
      WHEN 'FILE_SIZE2'.
        LS_FDCAT-SCRTEXT_L    = 'Disk Space in MB'.
        LS_FDCAT-SCRTEXT_M    = 'Disk Space in MB'.
        LS_FDCAT-SCRTEXT_S    = 'Disk Space in MB'.
        LS_FDCAT-COLTEXT      = 'Disk Space in MB'.
        LS_FDCAT-OUTPUTLEN    = '7'.
      WHEN 'FILE_SIZE3'.
        LS_FDCAT-SCRTEXT_L    = 'File Size in MB'.
        LS_FDCAT-SCRTEXT_M    = 'File Size in MB'.
        LS_FDCAT-SCRTEXT_S    = 'File Size in MB'.
        LS_FDCAT-COLTEXT      = 'File Size in MB'.
        LS_FDCAT-OUTPUTLEN    = '10'.
      WHEN 'SAVEMONTHS'.
        LS_FDCAT-SCRTEXT_L    = 'Residence Time'.
        LS_FDCAT-SCRTEXT_M    = 'Residence Time'.
        LS_FDCAT-SCRTEXT_S    = 'Residence Time'.
        LS_FDCAT-COLTEXT      = 'Residence Time'.
      WHEN 'USER_NAME'.
        LS_FDCAT-SCRTEXT_L    = 'Job Owner'.
        LS_FDCAT-SCRTEXT_M    = 'Job Owner'.
        LS_FDCAT-SCRTEXT_S    = 'Job Owner'.
        LS_FDCAT-COLTEXT      = 'Job Owner'.
        LS_FDCAT-OUTPUTLEN    = '7'.
      WHEN 'CREATE_DATE'.
        LS_FDCAT-SCRTEXT_L    = 'Job Date'.
        LS_FDCAT-SCRTEXT_M    = 'Job Date'.
        LS_FDCAT-SCRTEXT_S    = 'Job Date'.
        LS_FDCAT-COLTEXT      = 'Job Date'.
        LS_FDCAT-OUTPUTLEN    = '3'.
      WHEN 'CREATE_TIME'.
        LS_FDCAT-SCRTEXT_L    = 'Job Time'.
        LS_FDCAT-SCRTEXT_M    = 'Job Time'.
        LS_FDCAT-SCRTEXT_S    = 'Job Time'.
        LS_FDCAT-COLTEXT      = 'Job Time'.
        LS_FDCAT-OUTPUTLEN    = '3'.
      WHEN 'FIELDNAME'.
        LS_FDCAT-SCRTEXT_L    = 'Basic Variant Column'.
        LS_FDCAT-SCRTEXT_M    = 'Basic Variant Column'.
        LS_FDCAT-SCRTEXT_S    = 'Basic Variant Column'.
        LS_FDCAT-COLTEXT      = 'Basic Variant Column'.
        LS_FDCAT-OUTPUTLEN    = '7'.
      WHEN 'COMMENTS'.
        LS_FDCAT-SCRTEXT_L    = 'Note'.
        LS_FDCAT-SCRTEXT_M    = 'Note'.
        LS_FDCAT-SCRTEXT_S    = 'Note'.
        LS_FDCAT-COLTEXT      = 'Note'.
        LS_FDCAT-OUTPUTLEN    = '10'.
      WHEN 'VARIANTWRI'.
        LS_FDCAT-SCRTEXT_L    = 'Variant'.
        LS_FDCAT-SCRTEXT_M    = 'Variant'.
        LS_FDCAT-SCRTEXT_S    = 'Variant'.
        LS_FDCAT-COLTEXT      = 'Variant'.
        LS_FDCAT-OUTPUTLEN    = '7'.
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
***  CASE P_COLUMN-FIELDNAME.
***
***    WHEN 'OBJECT' OR
***         'OBJTEXT'.
***
***      READ TABLE GT_LIST_LEFT INDEX P_ROW-INDEX.
***
***      IF SY-SUBRC EQ 0.
***        SET PARAMETER ID 'OBT' FIELD GT_LIST_LEFT-OBJECT.
***        CALL TRANSACTION C_TCODE_SARA
***          AND SKIP FIRST SCREEN.
***      ENDIF.
***
***    WHEN 'FIELDNAME' OR
***         'COMMENTS' OR
***         'VARIANTWRI'.
***
***      READ TABLE GT_LIST_LEFT INDEX P_ROW-INDEX.
***
***      IF SY-SUBRC EQ 0.
***        PERFORM SELECT_ADMI_VARIA.
***      ENDIF.
***    ENDCASE.

      READ TABLE GT_LIST_LEFT INDEX P_ROW-INDEX.

      IF SY-SUBRC EQ 0.
        IF GT_LIST_LEFT-OBJECT EQ 'ZHMMACCT_1' OR
           GT_LIST_LEFT-OBJECT EQ 'ZHPPPART_1'.
          PERFORM SELECT_ZHACT0090.
        ELSE.
          PERFORM SELECT_ADMI_VARIA.
        ENDIF.
      ENDIF.



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
FORM SELECT_ADMI_RUN
                  TABLES PT_ADMI_RUN STRUCTURE GA_LIST.

  DATA L_END_STATUS.
  DATA LT_LIST LIKE GA_LIST OCCURS 0 WITH HEADER LINE.
  DATA L_DB_SPACE_W TYPE ADMI_STATS-DB_SPACE_W.
  DATA L_DB_INDEX_W TYPE ADMI_STATS-DB_INDEX_W.
  DATA L_DB_CLUST_W TYPE ADMI_STATS-DB_CLUST_W.
  DATA L_DB_STRUC_W TYPE ADMI_STATS-DB_STRUC_W.
  DATA L_FILE_HEAD TYPE ADMI_STATS-FILE_HEAD.
  DATA L_FILE_DATA TYPE ADMI_STATS-FILE_DATA.
  DATA L_APPLIC TYPE APPLI_TR01.

  GET PARAMETER ID 'API' FIELD L_APPLIC.

    IF L_APPLIC IS NOT INITIAL.
      SET_RANGE S_APPLIC 'I' 'EQ' L_APPLIC ''.
      SET PARAMETER ID 'API' FIELD SPACE.
    ENDIF.

    SELECT
      A~OBJECT
      A~DOCUMENT
      A~STATUS
      A~USER_NAME
      A~CREAT_DATE
      A~CREAT_TIME
      A~COMMENTS
      A~VARIANTWRI
      B~OBJTEXT
      C~DB_SPACE_W
      C~DB_INDEX_W
      C~DB_CLUST_W
      C~DB_STRUC_W
      C~FILE_HEAD
      C~FILE_DATA
      D~FILE_SIZE
      E~ZAPPLI AS APPLIC
      E~SAVEMONTHS
      FROM ADMI_RUN AS A
        LEFT OUTER JOIN ARCH_TXT AS B
          ON A~OBJECT = B~OBJECT
         AND B~LANGU = SY-LANGU
        LEFT OUTER JOIN ADMI_STATS AS C
          ON A~DOCUMENT = C~DOCUMENT
        LEFT OUTER JOIN ADMI_FILES AS D
          ON A~DOCUMENT = D~DOCUMENT
        INNER JOIN ZHACT0060 AS E
          ON A~OBJECT = E~OBJECT
      INTO CORRESPONDING FIELDS OF TABLE LT_LIST
     WHERE STATUS EQ 1
       AND DELETE_FLG EQ SPACE
       AND NOTGUILTY EQ SPACE
       AND WRITING EQ SPACE
       AND E~ZAPPLI IN S_APPLIC
       AND A~OBJECT IN S_OBJECT
       AND A~USER_NAME IN S_NAME
       AND A~CREAT_DATE IN S_DATE
       AND A~CLIENT EQ SY-MANDT.

***  ENDIF.


* admi_varia

  IF SY-SUBRC EQ 0.
    SORT LT_LIST BY OBJECT DOCUMENT.

    LOOP AT LT_LIST.

      MOVE-CORRESPONDING LT_LIST TO PT_ADMI_RUN.
      APPEND PT_ADMI_RUN.
      CLEAR PT_ADMI_RUN.
      CLEAR L_END_STATUS.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " SELECT_ADMI_RUN
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA_TO_LIST
*&---------------------------------------------------------------------*
FORM MOVE_DATA_TO_LIST
                    TABLES PT_ADMI_RUN STRUCTURE GA_LIST.

  DATA L_END_STATUS.

  REFRESH GT_LIST_LEFT.
  LOOP AT PT_ADMI_RUN.

    AT END OF DOCUMENT.
      L_END_STATUS = C_FLAG_X.
    ENDAT.

    IF L_END_STATUS EQ C_FLAG_X.

    MOVE-CORRESPONDING PT_ADMI_RUN TO GT_LIST_LEFT.


*   DB space (Write) in MB
    GT_LIST_LEFT-FILE_SIZE1 = ( PT_ADMI_RUN-DB_SPACE_W +
                              PT_ADMI_RUN-DB_INDEX_W +
                              PT_ADMI_RUN-DB_CLUST_W +
                              PT_ADMI_RUN-DB_STRUC_W ) / 1024 / 1024.
*   Disk Space in MB
    GT_LIST_LEFT-FILE_SIZE2 = ( PT_ADMI_RUN-FILE_HEAD +
                                PT_ADMI_RUN-FILE_DATA ) / 1024 / 1024.
*   File Size in MB
    GT_LIST_LEFT-FILE_SIZE3 = PT_ADMI_RUN-FILE_SIZE.
*   Basic Variant Column
    PERFORM SELECT_ZHACT0070
                        USING PT_ADMI_RUN-APPLIC
                              PT_ADMI_RUN-OBJECT
                              '2'
                              '10'
                        CHANGING GT_LIST_LEFT-FIELDNAME.
*   Write Status
    IF PT_ADMI_RUN-STATUS EQ 0.
      GT_LIST_LEFT-STATUS = 2.
      GT_LIST_LEFT-ZICON1 = '@0A@'.
    ELSE.
      GT_LIST_LEFT-ZICON1 = '@08@'.
    ENDIF.
*   Delete Status, Stroage Status
    PERFORM SELECT_ADMI_FILES
                          USING PT_ADMI_RUN-DOCUMENT
                          CHANGING GT_LIST_LEFT-STATUS_FIL
                                   GT_LIST_LEFT-ZICON2
                                   GT_LIST_LEFT-STATUS_OPT
                                   GT_LIST_LEFT-ZICON3.
    APPEND GT_LIST_LEFT.
    CLEAR: GT_LIST_LEFT,
           L_END_STATUS.

    ENDIF.
  ENDLOOP.

  CLEAR: GT_LIST_LEFT-STATUS,
         GT_LIST_LEFT-STATUS_FIL,
         GT_LIST_LEFT-STATUS_OPT.

  PERFORM SELECT_ZHACT0080
                      USING GT_LIST_LEFT-APPLIC
                            GT_LIST_LEFT-OBJECT
                      CHANGING GT_LIST_LEFT-USER_NAME
                               GT_LIST_LEFT-CREAT_DATE
                               GT_LIST_LEFT-CREAT_TIME.

  SORT GT_LIST_LEFT BY APPLIC OBJECT DOCUMENT.

ENDFORM.                    " MOVE_DATA_TO_LIST
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION .

  DATA  : LT_ROWS TYPE LVC_T_ROW.
  DATA  : LS_ROWS TYPE LVC_S_ROW.

  CALL METHOD G_ALV_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.

  SORT LT_ROWS DESCENDING BY INDEX.

  IF LT_ROWS[] IS INITIAL.
    MESSAGE S032.
    EXIT.
  ENDIF.

  LOOP AT LT_ROWS INTO LS_ROWS .

    READ TABLE GT_LIST_LEFT INDEX LS_ROWS-INDEX.

***    PERFORM CREATE_AOBJ
***                    USING GT_LIST_LEFT-TABNAME
***                          LS_ROWS-INDEX.

  ENDLOOP.
ENDFORM.                    " CALL_TRANSACTION
****&-------------------------------------------------------------------
*-
**-
****
****&      Form  CREATE_AOBJ
****&-------------------------------------------------------------------
*-
**-
****
***FORM CREATE_AOBJ
***              USING P_TABNAME
***                    P_INDEX.
***
***  DATA:
***    LT_BDCDATA LIKE BDCDATA OCCURS 10,
***    LT_MSGDATA LIKE BDCMSGCOLL OCCURS 1.
***  DATA: L_TABNAME TYPE TABNAME.
***  DATA: L_TABNAME_R TYPE TABNAME.
***  DATA: L_DDTEXT  LIKE DD02T-DDTEXT.
******  DATA: L_LEN TYPE I.
***
***  PERFORM SELECT_DD02T
***                  USING P_TABNAME
***                  CHANGING L_DDTEXT.
******  L_LEN = CHARLEN( P_TABNAME ).
***  L_TABNAME = P_TABNAME(10).
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****   Archiving Object Create
***    'X'   'SAPL0ARC'       '0100',
***    ' '   'BDC_CURSOR'     'V_ARC_OBJ-OBJECT(01)',
***    ' '   'BDC_OKCODE'     '=NEWL'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****   View Cluster Maintenance: Initial Screen
***    'X'   'SAPL0ARC'           '0101',
***    ' '   'V_ARC_OBJ-OBJECT'   L_TABNAME,  "Object Name
***    ' '   'V_ARC_OBJ-OBJTEXT'  L_DDTEXT(50),   "Text
***    ' '   'V_ARC_OBJ-APPLIC'   'CA',       "Aplication Area
***    ' '   'BDC_OKCODE'         '=ATAB'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****   Other View
***    'X'   'SAPLSVCM'           '0100',
***    ' '   'BDC_CURSOR'         'VIMDYNFLDS-DYN_LINE(02)',
***    ' '   'BDC_OKCODE'         '=DETA'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'           '0400',
***    ' '   'BDC_CURSOR'         'V_ARC_DEF-OBJECT',  "Object Name
***    ' '   'BDC_OKCODE'         '=NEWL'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'               '0400',
***    ' '   'V_ARC_DEF-SON(01)'      L_TABNAME,  "Object Name
***    ' '   'BDC_OKCODE'             '/00'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'               '0400',
***    ' '   'BDC_OKCODE'             '=ATAB'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****   Other View
***    'X'   'SAPLSVCM'               '0100',
***    ' '   'BDC_CURSOR'              'VIMDYNFLDS-DYN_LINE(05)',
***    ' '   'BDC_OKCODE'             '=DETA'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'           '0500',
***    ' '   'BDC_CURSOR'         'VIM_POSITION_INFO',  "Object Name
***    ' '   'BDC_OKCODE'         '=NEWL'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'               '0501',
***    ' '   'BDC_CURSOR'             'V_ARC_USR-OBJECT',
***    ' '   'V_ARC_USR-OBJECT'       L_TABNAME,  "Object Name
***    ' '   'BDC_OKCODE'             '/00'.
***
**** Read Program
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***     'X'   'SAPL0ARC'               '0501',
***     ' '   'BDC_OKCODE'             '=ATAB'.
***
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****   Other View
***    'X'   'SAPLSVCM'               '0100',
***    ' '   'BDC_CURSOR'              'VIMDYNFLDS-DYN_LINE(07)',
***    ' '   'BDC_OKCODE'             '=DETA'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'           '0850',
***    ' '   'BDC_CURSOR'         'V_ARC_RPRG-OBJECT',  "Object Name
***    ' '   'BDC_OKCODE'         '=NEWL'.
***
**** Read Program Name
***  CONCATENATE P_TABNAME '_R' INTO L_TABNAME_R.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'               '0850',
***    ' '   'BDC_CURSOR'             'V_ARC_RPRG-READ_PRG(01)',
***    ' '   'V_ARC_RPRG-READ_PRG(01)' L_TABNAME_R,  "Readd Progjram
*Name
***    ' '   'BDC_OKCODE'             '/00'.
***
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPL0ARC'               '0850',
******    ' '   'BDC_CURSOR'             'V_ARC_USR-OBJECT',
******    ' '   'V_ARC_USR-OBJECT'       P_TABNAME,  "Object Name
***    ' '   'BDC_OKCODE'             '=SAVE'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***    'X'   'SAPLSTRD'               '0100',
***    ' '   'BDC_CURSOR'             'KO007-L_DEVCLASS',
***    ' '   'KO007-L_DEVCLASS'       'ZHAC',  "
***    ' '   'BDC_OKCODE'             '=ADD'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***   'X'   'SAPLSTRD'               '0300',
***   ' '   'BDC_OKCODE'             '=LOCK'.
***
***  PERFORM APPEND_BDC_DATA TABLES LT_BDCDATA USING:
****
***   'X'   'SAPL0ARC'               '0850',
***   ' '   'BDC_OKCODE'             '=ENDE'.
***
***  G_BDCMODE = 'E'.
**** CALL TRANSACTION
***  PERFORM CALL_TRANSACTION_FOR_BDC
***                                TABLES
***                                  LT_BDCDATA
***                                  LT_MSGDATA
***                                USING
***                                  C_TCODE_AOBJ.
***
**** TRANSACTION RESULT
***  PERFORM RESULT_TRANSACTION_FOR_ACR
***                                 TABLES
***                                   LT_MSGDATA
***                                 USING
***                                   P_TABNAME.
***
***ENDFORM.                    " CREATE_AOBJ
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
*  SET HANDLER G_EVENT_RECEIVER2->HANDLE_DOUBLE_CLICK2 FOR G_ALV_GRID2.
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
      I_STRUCTURE_NAME   = 'ZHACS0030'
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
      WHEN 'TEXT'.
        LS_FDCAT-SCRTEXT_L    = 'User Input'.
        LS_FDCAT-SCRTEXT_M    = 'User Input'.
        LS_FDCAT-SCRTEXT_S    = 'User Input'.
        LS_FDCAT-COLTEXT      = 'User Input'.
        LS_FDCAT-OUTPUTLEN    = '12'.
      WHEN 'LOW'.
        LS_FDCAT-SCRTEXT_L    = 'Low'.
        LS_FDCAT-SCRTEXT_M    = 'Low'.
        LS_FDCAT-SCRTEXT_S    = 'Low'.
        LS_FDCAT-COLTEXT      = 'Low'.
        LS_FDCAT-OUTPUTLEN    = '15'.
      WHEN 'HIGH'.
        LS_FDCAT-SCRTEXT_L    = 'High'.
        LS_FDCAT-SCRTEXT_M    = 'High'.
        LS_FDCAT-SCRTEXT_S    = 'High'.
        LS_FDCAT-COLTEXT      = 'High'.
        LS_FDCAT-OUTPUTLEN    = '15'.
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

  CREATE OBJECT G_SPLITTER
    EXPORTING
      PARENT  = G_CUSTOM_CONTAINER1
      ROWS    = 1
      COLUMNS = 2.

  CALL METHOD G_SPLITTER->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 1
      WIDTH = 75.

  G_CONTAIN_LEFT  = G_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).
  G_CONTAIN_RIGHT = G_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 2 ).

ENDFORM.                    " ALV_CL_CREATE_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  SELECT_ADMI_VARIA
*&---------------------------------------------------------------------*
FORM SELECT_ADMI_VARIA .

  DATA LT_VALUTAB LIKE RSPARAMS OCCURS 0 WITH HEADER LINE.
  DATA LT_OBJECT LIKE VANZ OCCURS 0 WITH HEADER LINE.
  DATA L_REPORT LIKE ARCH_OBJ-REORGA_PRG.

  DATA: BEGIN OF LT_LIST OCCURS 0,
          SELNAME LIKE ADMI_VARIA-SELNAME,
          LOW LIKE ADMI_VARIA-LOW,
        END OF LT_LIST.

 SELECT SINGLE
    REORGA_PRG
    FROM ARCH_OBJ
    INTO L_REPORT
   WHERE OBJECT EQ GT_LIST_LEFT-OBJECT.

  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      REPORT                      = L_REPORT
      VARIANT                     = GT_LIST_LEFT-VARIANTWRI
*     MOVE_OR_WRITE               = 'W'
*     NO_IMPORT                   = ' '
*     EXECUTE_DIRECT              = ' '
*   IMPORTING
*     SP                          =
    TABLES
*     L_PARAMS                    =
*     L_PARAMS_NONV               =
*     L_SELOP                     =
*     L_SELOP_NONV                =
      VALUTAB                     = LT_VALUTAB
      OBJECTS                     = LT_OBJECT
*     FREE_SELECTIONS_DESC        =
*     FREE_SELECTIONS_VALUE       =
   EXCEPTIONS
     VARIANT_NON_EXISTENT        = 1
     VARIANT_OBSOLETE            = 2
     OTHERS                      = 3
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  REFRESH GT_LIST_RIGHT.
  LOOP AT LT_VALUTAB.

    READ TABLE LT_OBJECT
      WITH KEY NAME = LT_VALUTAB-SELNAME.

    GT_LIST_RIGHT-TEXT = LT_OBJECT-TEXT.
    GT_LIST_RIGHT-LOW = LT_VALUTAB-LOW.
    IF LT_VALUTAB-HIGH EQ '000' OR
       LT_VALUTAB-HIGH EQ '00000000' OR
       LT_VALUTAB-HIGH EQ '00.00.0000'.
      CLEAR GT_LIST_RIGHT-HIGH.
    ELSE.
      GT_LIST_RIGHT-HIGH = LT_VALUTAB-HIGH.
    ENDIF.

    APPEND GT_LIST_RIGHT.
    CLEAR GT_LIST_RIGHT.
  ENDLOOP.


***  SELECT
***    SELNAME
***    LOW
***    FROM ADMI_VARIA
***    INTO TABLE LT_LIST
***    WHERE DOCUMENT = GT_LIST_LEFT-DOCUMENT.
***
***  DATA L_PARTEXT LIKE TPARA-PARTEXT.
***  IF SY-SUBRC EQ 0.
***    REFRESH GT_LIST_RIGHT.
***    LOOP AT LT_LIST.
******      SELECT SINGLE
******        PARTEXT
******        FROM TPARA
******        INTO L_PARTEXT
******       WHERE PARAMID = LT_LIST-SELNAME.
***
***      GT_LIST_RIGHT-TEXT = LT_LIST-SELNAME.
***      GT_LIST_RIGHT-VALUE = LT_LIST-LOW.
***      APPEND GT_LIST_RIGHT.
***      CLEAR GT_LIST_RIGHT.
***    ENDLOOP.
***    REFRESH LT_LIST.
***  ENDIF.

*   ALV Refresh
    PERFORM ALV_CL_REFRESH_TABLE_DISPLAY
                            USING  G_ALV_GRID2
                                   G_REC_STABLE2.

ENDFORM.                    " SELECT_ADMI_VARIA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_ZHACR01200
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_ZHACR01200 .

  LEAVE TO TRANSACTION 'ZHACR01200'.

ENDFORM.                    " CALL_TRANSACTION_ZHACR01200
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_ZHACR01000
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_ZHACR01000 .

  LEAVE TO TRANSACTION 'ZHACR01000'.

ENDFORM.                    " CALL_TRANSACTION_ZHACR01000
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZHACT0070
*&---------------------------------------------------------------------*
FORM SELECT_ZHACT0070
                  USING P_ZAPPLI
                        P_OBJECT
                        P_ZGUBUN
                        P_ZFORMAT_TYP
                  CHANGING P_FIELDNAME.

  DATA: BEGIN OF LT_LIST OCCURS 0,
          ZAPPLI LIKE ZHACT0070-ZAPPLI,
          OBJECT LIKE ZHACT0070-OBJECT,
          FIELDNAME LIKE ZHACT0070-FIELDNAME,
          ZGUBUN LIKE ZHACT0070-ZGUBUN,
        END OF LT_LIST.

  DATA: L_FIELDNAME LIKE ZHACT0070-FIELDNAME.
  DATA L_END_OBJECT.

  SELECT
    ZAPPLI
    OBJECT
    FIELDNAME
    ZGUBUN
    FROM ZHACT0070
    INTO CORRESPONDING FIELDS OF TABLE LT_LIST
   WHERE ZAPPLI EQ P_ZAPPLI
     AND OBJECT EQ P_OBJECT
     AND ZGUBUN EQ P_ZGUBUN.

  LOOP AT LT_LIST.

    AT END OF OBJECT.
      L_END_OBJECT = C_FLAG_X.
    ENDAT.

    IF L_END_OBJECT EQ C_FLAG_X.
      CONCATENATE L_FIELDNAME LT_LIST-FIELDNAME INTO L_FIELDNAME.
      CLEAR L_END_OBJECT.
    ELSE.
      CONCATENATE L_FIELDNAME LT_LIST-FIELDNAME ',' INTO L_FIELDNAME.
    ENDIF.
  ENDLOOP.

  P_FIELDNAME = L_FIELDNAME.

ENDFORM.                    " SELECT_ZHACT0070
*&---------------------------------------------------------------------*
*&      Form  SELECT_ADMI_FILES
*&---------------------------------------------------------------------*
FORM SELECT_ADMI_FILES
                  USING P_DOCUMENT
                  CHANGING P_STATUS_FIL
                           P_ZICON2
                           P_STATUS_OPT
                           P_ZICON3.

  DATA L_STATUS_FIL_R.
  DATA L_STATUS_FIL_Y.
  DATA L_STATUS_FIL_G.
  DATA L_STATUS_FIL.
  DATA L_STATUS_OPT_R.
  DATA L_STATUS_OPT_Y.
  DATA L_STATUS_OPT_G.
  DATA L_STATUS_OPT.
  DATA: BEGIN OF LT_LIST OCCURS 0,
            STATUS_FIL LIKE ADMI_FILES-STATUS_FIL,
            STATUS_OPT LIKE ADMI_FILES-STATUS_OPT,
        END OF LT_LIST.

  SELECT
      STATUS_FIL
      STATUS_OPT
      FROM ADMI_FILES
      INTO CORRESPONDING FIELDS OF TABLE LT_LIST
     WHERE DOCUMENT EQ P_DOCUMENT.

  IF SY-SUBRC EQ 0.

    LOOP AT LT_LIST.

      CASE LT_LIST-STATUS_FIL.
        WHEN 0.
          L_STATUS_FIL_R = C_FLAG_X.
        WHEN 1 OR 2.
          L_STATUS_FIL_Y = C_FLAG_X.
        WHEN 3.
          L_STATUS_FIL_G = C_FLAG_X.
        WHEN 4.
          P_ZICON2 = '@2O@'.
        WHEN 5.
          P_ZICON2 = '@17@'.
        WHEN 6.
          P_ZICON2 = '@3H@'.
        WHEN 7.
          P_ZICON2 = '@06@'.
*    **          L_STATUS_FIL = LT_LIST-STATUS_FIL.
      ENDCASE.

      CASE LT_LIST-STATUS_OPT.
        WHEN 0.
          L_STATUS_OPT_R = C_FLAG_X.
        WHEN 1.
          L_STATUS_OPT_Y = C_FLAG_X.
        WHEN 2.
          L_STATUS_OPT_G = C_FLAG_X.
        WHEN 3.
          P_ZICON3 = '@0A@'.
        WHEN 4.
          P_ZICON3 = '@08@'.
        WHEN 5.
          P_ZICON3 = '@08@'.
        WHEN 6.
          P_ZICON3 = '@09@'.
***          L_STATUS_OPT = LT_LIST-STATUS_OPT.
      ENDCASE.
    ENDLOOP.

    IF L_STATUS_FIL_R IS NOT INITIAL.
      P_STATUS_FIL = 0.
      P_ZICON2 = '@2O@'.
    ELSE.
      IF L_STATUS_FIL_Y IS  NOT INITIAL.
        P_STATUS_FIL = 2.
        P_ZICON2 = '@5D@'.
      ELSE.
        IF L_STATUS_FIL_G IS NOT INITIAL.
          P_STATUS_FIL = 3.
          P_ZICON2 = '@5B@'.
        ELSE.
          P_STATUS_FIL = L_STATUS_FIL.
        ENDIF.
      ENDIF.
    ENDIF.

    IF L_STATUS_OPT_R IS NOT INITIAL.
      P_STATUS_OPT = 0.
      P_ZICON3 = '@0A@'.
    ELSE.
      IF L_STATUS_OPT_Y IS  NOT INITIAL.
        P_STATUS_OPT = 1.
        P_ZICON3 = '@09@'.
      ELSE.
        IF L_STATUS_OPT_G IS NOT INITIAL.
          P_STATUS_OPT = 2.
           P_ZICON3 = '@08@'.
        ELSE.
          P_STATUS_OPT = L_STATUS_OPT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " SELECT_ADMI_FILES
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZHACT0090
*&---------------------------------------------------------------------*
FORM SELECT_ZHACT0090 .

  DATA: BEGIN OF LT_ZHACT0090 OCCURS 0,
              FIELD_SEQ LIKE ZHACT0090-FIELD_SEQ,
              FIELDNAME LIKE ZHACT0090-FIELDNAME,
              FROM_VALUE LIKE ZHACT0090-FROM_VALUE,
              TO_VALUE LIKE ZHACT0090-TO_VALUE,
            END OF LT_ZHACT0090.

  SELECT
    FIELD_SEQ
    FIELDNAME
    FROM_VALUE
    TO_VALUE
    FROM ZHACT0090
    INTO CORRESPONDING FIELDS OF TABLE LT_ZHACT0090
   WHERE ZAPPLI EQ GT_LIST_LEFT-APPLIC
     AND OBJECT EQ GT_LIST_LEFT-OBJECT
     AND ZSEQ EQ GT_LIST_LEFT-ZSEQ
     AND FIELD_SEQ EQ '1'.

  REFRESH GT_LIST_RIGHT.

  LOOP AT LT_ZHACT0090.

    IF GT_LIST_LEFT-OBJECT EQ 'ZHMMACCT_1'.
      GT_LIST_RIGHT-TEXT = 'To Posting Date'.
    ELSEIF GT_LIST_LEFT-OBJECT EQ 'ZHPPPART_1'.
      GT_LIST_RIGHT-TEXT = 'Before Month ago (Posting Date)'.
    ENDIF.

    GT_LIST_RIGHT-LOW = LT_ZHACT0090-FROM_VALUE.
    GT_LIST_RIGHT-HIGH = LT_ZHACT0090-TO_VALUE.

    APPEND GT_LIST_RIGHT.
    CLEAR GT_LIST_RIGHT.

  ENDLOOP.

*   ALV Refresh
  PERFORM ALV_CL_REFRESH_TABLE_DISPLAY
                          USING  G_ALV_GRID2
                                 G_REC_STABLE2.

ENDFORM.                    " SELECT_ZHACT0090
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_SARA
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_SARA .

  DATA  : LT_ROWS TYPE LVC_T_ROW.
  DATA  : LS_ROWS TYPE LVC_S_ROW.

  CALL METHOD G_ALV_GRID1->GET_SELECTED_ROWS
       IMPORTING
         ET_INDEX_ROWS = LT_ROWS.

  SORT LT_ROWS DESCENDING BY INDEX.

  IF LT_ROWS[] IS INITIAL.
    MESSAGE S003 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT LT_ROWS INTO LS_ROWS .
    READ TABLE GT_LIST_LEFT INDEX LS_ROWS-INDEX.

    IF SY-SUBRC EQ 0.
      IF GT_LIST_LEFT-OBJECT EQ 'ZHMMACCT_1'.
      ELSE.
        SET PARAMETER ID 'OBT' FIELD GT_LIST_LEFT-OBJECT.

        LEAVE TO TRANSACTION C_TCODE_SARA
          AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " CALL_TRANSACTION_SARA
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZHACT0080
*&---------------------------------------------------------------------*
FORM SELECT_ZHACT0080
                 USING P_ZAPPLI
                       P_OBJECT
***                     P_ZSEQ
                 CHANGING P_USER_NAME
                          P_CREAT_DATE
                          P_CREAT_TIME.
  DATA L_END_OBJECT.
  DATA: BEGIN OF LT_ZHACT0080 OCCURS 0,
          ZAPPLI LIKE ZHACT0080-ZAPPLI,
          OBJECT LIKE ZHACT0080-OBJECT,
          ZSEQ LIKE ZHACT0080-ZSEQ,
          USER_NAME LIKE ZHACT0080-USER_NAME,
          CREAT_DATE LIKE ZHACT0080-CREAT_DATE,
          CREAT_TIME LIKE ZHACT0080-CREAT_TIME,
          OBJTEXT LIKE ARCH_TXT-OBJTEXT,
        END OF LT_ZHACT0080.

  SELECT
    A~ZAPPLI
    A~OBJECT
    A~ZSEQ
    A~USER_NAME
    A~CREAT_DATE
    A~CREAT_TIME
    B~OBJTEXT
    FROM ZHACT0080 AS A
      INNER JOIN ARCH_TXT AS B
        ON A~OBJECT EQ B~OBJECT
       AND B~LANGU EQ SY-LANGU
    INTO CORRESPONDING FIELDS OF TABLE LT_ZHACT0080
   WHERE A~ZAPPLI IN S_APPLIC
     AND A~OBJECT IN S_OBJECT
     AND A~USER_NAME IN S_NAME
     AND A~CREAT_DATE IN S_DATE.


  IF SY-SUBRC EQ 0.
    SORT LT_ZHACT0080 BY ZAPPLI OBJECT ZSEQ.

    LOOP AT LT_ZHACT0080.

***      AT END OF OBJECT.
***        L_END_OBJECT = C_FLAG_X.
***      ENDAT.

***      IF L_END_OBJECT EQ C_FLAG_X.
        MOVE-CORRESPONDING LT_ZHACT0080 TO GT_LIST_LEFT.
        GT_LIST_LEFT-APPLIC = LT_ZHACT0080-ZAPPLI.

*       Basic Variant Column
        PERFORM SELECT_ZHACT0070
                            USING LT_ZHACT0080-ZAPPLI
                                  LT_ZHACT0080-OBJECT
                                  '2'
                                  '10'
                            CHANGING GT_LIST_LEFT-FIELDNAME.

        PERFORM GET_SAVEMONTHS
                           USING GT_LIST_LEFT-APPLIC
                                 GT_LIST_LEFT-OBJECT
                           CHANGING GT_LIST_LEFT-SAVEMONTHS.

        GT_LIST_LEFT-STATUS = '7'.
        APPEND GT_LIST_LEFT.
        CLEAR: GT_LIST_LEFT,
               L_END_OBJECT.
***      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " SELECT_ZHACT0080
*&---------------------------------------------------------------------*
*&      Form  GET_SAVEMONTHS
*&---------------------------------------------------------------------*
FORM GET_SAVEMONTHS USING P_APPLIC
                          P_OBJECT
                    CHANGING P_SAVEMONTHS.

  DATA L_SAVEMONTHS LIKE ZHACT0060-SAVEMONTHS.

  SELECT SINGLE
    SAVEMONTHS
    FROM ZHACT0060
    INTO L_SAVEMONTHS
   WHERE ZAPPLI EQ P_APPLIC
     AND OBJECT EQ P_OBJECT.

  IF SY-SUBRC EQ 0.
    P_SAVEMONTHS = L_SAVEMONTHS.
  ENDIF.

ENDFORM.                    " GET_SAVEMONTHS
