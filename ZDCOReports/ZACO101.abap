*
* Spec: ANDY CHOI
* developed by Andy Choi
*

REPORT ZACO101  NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

TABLES: ZTCO_SHOP_PLH.

DATA: OK_CODE LIKE SY-UCOMM,
      SAVE_OK LIKE SY-UCOMM,
      G_CONTAINER TYPE SCRFNAME VALUE 'BCALV_GRID_DEMO_0100_CONT1',
      GRID1  TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAYOUT TYPE LVC_S_LAYO.


*§1.Extend your output table for a field, e.g., CELLTAB, that holds
*   information about the edit status of each cell for the
*   corresponding row (the table type is SORTED!).
DATA: BEGIN OF GT_OUTTAB OCCURS 0.  "with header line
        INCLUDE STRUCTURE ZTCO_SHOP_PLH.
DATA: CELLTAB TYPE LVC_T_STYL.
DATA: END OF GT_OUTTAB.

DATA: GT_ROW   TYPE LVC_T_ROW,
      GS_ROW   TYPE LVC_S_ROW,
      GT_ROID  TYPE LVC_T_ROID.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    TYPES: ZTCO_SHOP_KEYS TYPE STANDARD TABLE OF ZTCO_SHOP_PLH,
           ZTCO_SHOP_TABLE TYPE STANDARD TABLE OF ZTCO_SHOP_PLH.

    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED,
                       GET_DELETED_ROWS
             EXPORTING
                       DELETED_ROWS TYPE ZTCO_SHOP_TABLE,

      REFRESH_DELTA_TABLES.

  PRIVATE SECTION.
    DATA DELETED_ROWS TYPE STANDARD TABLE OF ZTCO_SHOP_PLH.

* This flag is set if any error occured in one of the
* following methods:
    DATA: ERROR_IN_DATA TYPE C.
    METHODS:
      UPDATE_DELTA_TABLES
         IMPORTING
            PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

* Setting for Change data
  METHOD HANDLE_DATA_CHANGED.

* remember deleted lines for saving
    CALL METHOD UPDATE_DELTA_TABLES( ER_DATA_CHANGED ).

    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    " handle_data_changed

  METHOD GET_DELETED_ROWS.
    DELETED_ROWS = ME->DELETED_ROWS.
  ENDMETHOD.

  METHOD REFRESH_DELTA_TABLES.
    CLEAR ME->DELETED_ROWS[].
  ENDMETHOD.

  METHOD UPDATE_DELTA_TABLES.
    DATA: L_DEL_ROW TYPE LVC_S_MOCE,
          LS_KEY TYPE ZTCO_SHOP_PLH,
          LS_ZTCO_SHOP TYPE ZTCO_SHOP_PLH,
          LS_OUTTAB LIKE LINE OF GT_OUTTAB.

    LOOP AT PR_DATA_CHANGED->MT_DELETED_ROWS INTO L_DEL_ROW.
      READ TABLE GT_OUTTAB INTO LS_OUTTAB INDEX L_DEL_ROW-ROW_ID.
      IF SY-SUBRC NE 0.
        MESSAGE I000(0K) WITH TEXT-E01. "Internal error
      ELSE.
        MOVE-CORRESPONDING LS_OUTTAB TO LS_ZTCO_SHOP.
        APPEND LS_ZTCO_SHOP TO DELETED_ROWS.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.

*////////////////
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS    :
  P_KOKRS LIKE ZTCO_SHOP_PLH-KOKRS DEFAULT 'H201',
  P_BDATJ LIKE ZTCO_SHOP_PLH-BDATJ MEMORY ID GJR,
  P_POPER LIKE ZTCO_SHOP_PLH-POPER MEMORY ID POPR,
  P_KLVAR LIKE ZTCO_SHOP_PLH-KLVAR MEMORY ID KRT.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  S_ARTNR FOR ZTCO_SHOP_PLH-ARTNR.
SELECTION-SCREEN END OF BLOCK B2.


* Refer: BCALV_EDIT_02.
* - prepare STRUCTURE (se11)
* - copy screen, pf-status

*-----------------------------------------------------------------
* Essential steps (search for '§')
* ~~~~~~~~~~~~~~~
* 1.Extend your output table for a field, e.g., CELLTAB, that holds
*   information about the edit status of each cell for the
*   corresponding row (the table type is SORTED!).
* 2.After selecting data, set edit status for each row in a loop
*   according to field SEATSMAX.
* 2a.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_ENABLED to set a cell
*    to status "editable".
* 2b.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_DISABLED to set a cell
*    to status "non-editable".
* 2c.Copy your celltab to the celltab of the current row of gt_outtab.
* 3.Provide the fieldname of the celltab field by using field
*   STYLEFNAME of the layout structure.
*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

*data: gv_level(5),                       " Authorization Level
*      gv_past(1).
*
*get parameter id 'ZCOLV1' field gv_level.
*clear gv_past.


START-OF-SELECTION.
  PERFORM SELECT_DATA.
  CALL SCREEN 100.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'MAIN100'.
*  SET TITLEBAR 'MAIN100'.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
           EXPORTING CONTAINER_NAME = G_CONTAINER.
    CREATE OBJECT GRID1
           EXPORTING I_PARENT = G_CUSTOM_CONTAINER.


*  Create Object to verify input values.
    CREATE OBJECT G_EVENT_RECEIVER.
    SET HANDLER : G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR GRID1.

    CALL METHOD GRID1->REGISTER_EDIT_EVENT
         EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    PERFORM INIT_STYLE.

*§3.Provide the fieldname of the celltab field by using field
*   STYLEFNAME of the layout structure.
    GS_LAYOUT-STYLEFNAME = 'CELLTAB'.

* set substate of editable cells to deactivated
    CALL METHOD GRID1->SET_READY_FOR_INPUT
          EXPORTING I_READY_FOR_INPUT = 0.

    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZTCO_SHOP_PLH'
                   IS_LAYOUT        = GS_LAYOUT
         CHANGING  IT_OUTTAB        = GT_OUTTAB[].

  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE PAI INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN 'EXIT'.
      PERFORM EXIT_PROGRAM.
    WHEN 'SWITCH'.
*      if gv_past = space or gv_level = 'ADM'.
      PERFORM SWITCH_EDIT_MODE.
*      endif.
    WHEN 'SAVE'.
      IF NOT GRID1->IS_READY_FOR_INPUT( ) EQ 0.
        PERFORM DATA_SAVE.
      ENDIF.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
ENDMODULE.
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM EXIT_PROGRAM.
  LEAVE TO SCREEN 0.
*  leave PROGRAM.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
FORM INIT_STYLE.
  DATA: LT_CELLTAB TYPE LVC_T_STYL,
        L_INDEX TYPE I.


*§2.After selecting data, set edit status for each row in a loop
*   according to field SEATSMAX.
  LOOP AT GT_OUTTAB.
    L_INDEX = SY-TABIX.
    REFRESH LT_CELLTAB.
*    if gt_outtab-seatsmax ge 300.
    PERFORM FILL_CELLTAB USING 'RW'
                         CHANGING LT_CELLTAB.
*    else.
*        perform fill_celltab using 'RO'
*                             changing lt_celltab.
*    endif.

*§2c.Copy your celltab to the celltab of the current row of gt_outtab.
    INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUTTAB-CELLTAB.
    MODIFY GT_OUTTAB INDEX L_INDEX.
  ENDLOOP.
ENDFORM.                               " SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
FORM FILL_CELLTAB USING VALUE(P_MODE)
                  CHANGING PT_CELLTAB TYPE LVC_T_STYL.
  data: ls_celltab type lvc_s_styl.
  data: l_mode1 type raw4,
        l_mode2 type raw4.

* This forms sets the style of column 'PRICE' editable
* according to 'p_mode' and the rest to read only either way.

  if p_mode eq 'RW'.
    l_mode1 = cl_gui_alv_grid=>mc_style_enabled.
  else. "p_mode eq 'RO'
    l_mode1 = cl_gui_alv_grid=>mc_style_disabled.
  endif.

  l_mode2 = cl_gui_alv_grid=>mc_style_disabled.

  ls_celltab-fieldname = 'ARTNR'.
  ls_celltab-style = l_mode1.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'GSMNG'.
  ls_celltab-style = l_mode1.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'MEINS'.
  ls_celltab-style = l_mode1.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'PWWRK'.
  ls_celltab-style = l_mode1.
  insert ls_celltab into table pt_celltab.

ENDFORM.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
FORM SWITCH_EDIT_MODE.

  IF GRID1->IS_READY_FOR_INPUT( ) EQ 0.
* set edit enabled cells ready for input
    CALL METHOD GRID1->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 1.

  ELSE.
* lock edit enabled cells against input
    CALL METHOD GRID1->SET_READY_FOR_INPUT
                    EXPORTING I_READY_FOR_INPUT = 0.
  ENDIF.
ENDFORM.                               " SWITCH_EDIT_MODE


*CUST
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
FORM SELECT_DATA.
  DATA: LT_DATA TYPE TABLE OF ZTCO_SHOP_PLH WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    FROM ZTCO_SHOP_PLH
    WHERE KOKRS = P_KOKRS
      AND BDATJ = P_BDATJ
      AND POPER = P_POPER
      AND KLVAR = P_KLVAR
      AND ARTNR   IN S_ARTNR.

  LOOP AT LT_DATA.
    MOVE-CORRESPONDING LT_DATA TO GT_OUTTAB.
    APPEND GT_OUTTAB.
  ENDLOOP.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  data_save
*&---------------------------------------------------------------------*
FORM DATA_SAVE.
  DATA: LS_ITAB LIKE ZTCO_SHOP_PLH,
        LT_ITAB TYPE TABLE OF ZTCO_SHOP_PLH WITH HEADER LINE.
  DATA  LT_DEL_ROWS TYPE TABLE OF ZTCO_SHOP_PLH.

*  call method grid1->get_selected_rows
*              importing et_index_rows = gt_row
*                        et_row_no = gt_roid.

* Delete Lines
  CALL METHOD G_EVENT_RECEIVER->GET_DELETED_ROWS
            IMPORTING DELETED_ROWS = LT_DEL_ROWS.

  DELETE ZTCO_SHOP_PLH FROM TABLE LT_DEL_ROWS.
  COMMIT WORK.

  CALL METHOD G_EVENT_RECEIVER->REFRESH_DELTA_TABLES.

  LOOP AT GT_OUTTAB.
*    read table gt_outtab index gs_row-index.
*    if sy-subrc = 0.
    MOVE-CORRESPONDING GT_OUTTAB TO LT_ITAB.

    LT_ITAB-MANDT = SY-MANDT.
    LT_ITAB-KOKRS = P_KOKRS.
    LT_ITAB-BDATJ = P_BDATJ.
    LT_ITAB-POPER = P_POPER.

    APPEND LT_ITAB.
*    endif.
  ENDLOOP.

  MODIFY ZTCO_SHOP_PLH FROM TABLE LT_ITAB.

  MESSAGE S000 WITH 'Data saved'.
ENDFORM.                    " data_save
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM DATA_CHANGED USING RR_DATA_CHANGED
                        TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

ENDFORM.                    " DATA_CHANGED
