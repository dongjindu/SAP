************************************************************************
* Program Name      : ZRMM_BDP_01
* Creation Date     : 12/2009
* Development Request No :
* Addl Documentation: ZMMR91100T
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZRMM_BDP_01 NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS, VRM.
TABLES: MARA, MARC, MARD, LAGP, LFA1, ZTMM_BDP.

*DATA: IT_ITAB LIKE TABLE OF ZTMM_BDP WITH HEADER LINE.
DATA: BEGIN OF IT_ITAB OCCURS 0,
      MATNR LIKE ZTMM_BDP-MATNR,
      WERKS LIKE ZTMM_BDP-WERKS,
      LGORT LIKE ZTMM_BDP-LGORT,
      LGPLA LIKE ZTMM_BDP-LGPLA,
      RDMNG LIKE ZTMM_BDP-RDMNG,
      FEEDER LIKE ZTMM_BDP-FEEDER,
      SCRAP_WS LIKE ZTMM_BDP-SCRAP_WS,
      DEL_METHOD LIKE ZTMM_BDP-DEL_METHOD,
      ZOPTION LIKE ZTMM_BDP-ZOPTION,
      ZVARIANT LIKE ZTMM_BDP-ZVARIANT,
      APPS LIKE ZTMM_BDP-APPS,
      ZUSAGE LIKE ZTMM_BDP-ZUSAGE,
      ROUTE_CYCLE LIKE ZTMM_BDP-ROUTE_CYCLE,
      FLAG LIKE ZTMM_BDP-FLAG,
      MESSAGE LIKE ZTMM_BDP-MESSAGE,
     END OF IT_ITAB.

DATA: BEGIN OF IT_DATA OCCURS 0,
      MATNR(18),
      WERKS(4),
      LGORT(4),
      LGPLA(10),
      RDMNG(13),
      FEEDER(5),
      SCRAP_WS(5),
      DEL_METHOD(3),
      ZOPTION(3),
      ZVARIANT(3),
      APPS(12),
      ZUSAGE(2),
      ROUTE_CYCLE(1),
      END OF IT_DATA.

DATA: IT_BDP LIKE TABLE OF ZTMM_BDP WITH HEADER LINE.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT TYPE I,
      W_ANSWER(1).

DATA:  WA_STBL  TYPE LVC_S_STBL.
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_EXCLUDE      TYPE UI_FUNCTIONS.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME  LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA: G_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_DATA_CHANGED
         FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED.

    DATA: ERROR_IN_DATA TYPE C.

ENDCLASS.
DATA :IT_LVC  LIKE LVC_S_ROW.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_DATA_CHANGED.

    DATA: LS_GOOD TYPE LVC_S_MODI,
          LV_VALUE TYPE LVC_VALUE,
          W_QTY(13),
          LVC_T_ROW TYPE LVC_T_ROW.

    ERROR_IN_DATA = SPACE.
    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.
      CASE LS_GOOD-FIELDNAME.
* check if column Name1 of this row was changed
        WHEN 'FEEDER' OR 'SCRAP_WS' OR 'DEL_METHOD' OR 'ZOPTION' OR
             'ZVARIANT' OR 'APPS' OR 'ZUSAGE' OR 'ROUTE_CYCLE'.
          CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
                     EXPORTING
                        I_ROW_ID  = LS_GOOD-ROW_ID
                        I_FIELDNAME = LS_GOOD-FIELDNAME
                     IMPORTING
                        E_VALUE =   LV_VALUE.
          CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
                  EXPORTING
                       I_ROW_ID = LS_GOOD-ROW_ID
                       I_FIELDNAME = LS_GOOD-FIELDNAME
                       I_VALUE     = LV_VALUE.

      ENDCASE.
    ENDLOOP.
*§7.Display application log if an error has occured.
    IF ERROR_IN_DATA EQ 'X'.
      CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK MAIN WITH FRAME TITLE TEXT-001.
PARAMETERS: FNAME LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK MAIN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FNAME.

  PERFORM SELECTION_SCREEN USING FNAME.

START-OF-SELECTION.
  PERFORM UPLOAD_FILE TABLES IT_DATA.
  PERFORM UPDATE_TABLE.
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    CALL SCREEN 200.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SCREEN.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EXCEL'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen



*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_200 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_ITAB'.
    PERFORM ASSIGN_ITAB_TO_ALV.
  ELSE.
    WA_STBL-ROW = 'X'.
    WA_STBL-COL = 'X'.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING IS_STABLE = WA_STBL.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure

  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable

  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = ' '.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.
*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).
  FIELD-SYMBOLS <FS>.

  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.
    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.
    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
*               i_default        = space
               IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_ITAB[].
*               it_sort          = it_sort[].

** ENTER
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
** Cursor----
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*
*
*  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.
*

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS.
  DATA LS_EXCLUDE TYPE UI_FUNC.

* Row manipulation
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.

*  Sort buttons
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.


  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :


                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '15',

                                  'S' 'WERKS'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'PLNT',
                                   'E' 'OUTPUTLEN'   '4',

                                  'S' 'LGORT'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'SLOC',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'LGPLA'   ' ',
                                     ' ' 'COLTEXT'     'BIN',
                                     'E' 'OUTPUTLEN'   '10',


                                    'S' 'RDMNG'       ' ',
                                    ' ' 'KEY'         '',
                                    ' ' 'COLTEXT'     'Quantity',
*                                    ' ' 'DECIMALS_O'  '0',
                                    'E' 'OUTPUTLEN'   '13',


                                    'S' 'FEEDER'   ' ',
                                    ' ' 'KEY'         '',
                                    ' ' 'COLTEXT'     'FDR',
                                    'E' 'OUTPUTLEN'   '4',

                                   'S' 'SCRAP_WS'   ' ',
                                    ' ' 'KEY'         '',
                                    ' ' 'COLTEXT'     'Scrap',
                                    'E' 'OUTPUTLEN'   '5',

                                     'S' 'DEL_METHOD'   ' ',
                                     ' ' 'COLTEXT'     'DLV',
                                     'E' 'OUTPUTLEN'   '2',

                                     'S' 'ZOPTION'   ' ',
                                     ' ' 'COLTEXT'     'Option',
                                     ' ' 'LZERO'       'X',
                                     'E' 'OUTPUTLEN'   '7',

                                    'S' 'ZVARIANT'   ' ',
                                     ' ' 'KEY'         '',
                                     ' ' 'COLTEXT'     'Var',
                                     ' ' 'LZERO'       'X',
                                     'E' 'OUTPUTLEN'   '5',

                                     'S' 'APPS'   ' ',
                                     ' ' 'COLTEXT'     'Apps',
                                     'E' 'OUTPUTLEN'   '12',

                                     'S' 'ZUSAGE'   ' ',
                                     ' ' 'COLTEXT'     'Usage',
                                     'E' 'OUTPUTLEN'   '5',

                                     'S' 'ROUTE_CYCLE'   ' ',
                                     ' ' 'COLTEXT'     'Cyc',
                                     'E' 'OUTPUTLEN'   '3',

                                     'S' 'FLAG'   ' ',
                                     ' ' 'COLTEXT'     'Flag',
                                     'E' 'OUTPUTLEN'   '4',

                                     'S' 'MESSAGE'   ' ',
                                     ' ' 'COLTEXT'     'Message',
                                     'E' 'OUTPUTLEN'   '40'.


ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.

  DATA: LT_SAVE LIKE TABLE OF ZTMM_BDP WITH HEADER LINE.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I,
        L_COUNT TYPE N,
        L_INDEX LIKE SY-TABIX.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  REFRESH: IT_BDP.
  LOOP AT LT_ROWS.
    READ TABLE IT_ITAB INDEX LT_ROWS-INDEX.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING IT_ITAB TO IT_BDP.
      APPEND IT_BDP.
    ENDIF.
  ENDLOOP.

  PERFORM CHECK_DATA TABLES IT_BDP
                      CHANGING W_ANSWER.
** Changed by Furong on 12/21/10
  LOOP AT IT_BDP WHERE FLAG = ' '.
    LT_SAVE = IT_BDP.
    LT_SAVE-ERDAT = SY-DATUM.
    LT_SAVE-ERZET = SY-UZEIT.
    LT_SAVE-ERNAM = SY-UNAME.
    APPEND LT_SAVE.
  ENDLOOP.

  DESCRIBE TABLE LT_SAVE LINES L_COUNT.
  IF L_COUNT = 0.
    MESSAGE S000 WITH 'No Data was saved successfully'.
  ELSE.

    INSERT ZTMM_BDP FROM TABLE LT_SAVE.

    IF SY-SUBRC = 0.
      COMMIT WORK.
     MESSAGE S000 WITH 'Data was updated successfully, total records: '
    L_COUNT.
      LOOP AT IT_BDP WHERE FLAG = ' '.
        IT_BDP-ERDAT = SY-DATUM.
        IT_BDP-ERZET = SY-UZEIT.
        IT_BDP-ERNAM = SY-UNAME.
        IT_BDP-FLAG = 'S'.
        IT_BDP-MESSAGE = 'Data was updated successfully'.
        MODIFY IT_BDP.
      ENDLOOP.
    ELSE.
      ROLLBACK WORK.
      MESSAGE S000 WITH 'Data Updating Error (SAP error)'.
      LOOP AT IT_BDP  WHERE FLAG = ' '.
        IT_BDP-ERDAT = SY-DATUM.
        IT_BDP-ERZET = SY-UZEIT.
        IT_BDP-ERNAM = SY-UNAME.
        IT_BDP-FLAG = 'E'.
        IT_BDP-MESSAGE = 'Data Saving Error (SAP error)'.
        MODIFY IT_BDP.
      ENDLOOP.

    ENDIF.
  ENDIF.
  LOOP AT IT_BDP.
    READ TABLE IT_ITAB WITH KEY MATNR = IT_BDP-MATNR
                                 WERKS = IT_BDP-WERKS
                                 LGORT = IT_BDP-LGORT
                                 LGPLA = IT_BDP-LGPLA.
    IF SY-SUBRC = 0.
      L_INDEX = SY-TABIX.
      IT_ITAB-FLAG = IT_BDP-FLAG.
      IT_ITAB-MESSAGE = IT_BDP-MESSAGE.
      MODIFY IT_ITAB INDEX L_INDEX.
    ENDIF.
  ENDLOOP.


*  IF W_ANSWER IS INITIAL.
*    LOOP AT LT_BDP.
*      LT_BDP-ERDAT = SY-DATUM.
*      LT_BDP-ERZET = SY-UZEIT.
*      LT_BDP-ERNAM = SY-UNAME.
*      MODIFY LT_BDP.
*    ENDLOOP.
*
*    INSERT ZTMM_BDP FROM TABLE LT_BDP.
*    IF SY-SUBRC = 0.
*      COMMIT WORK.
*      MESSAGE S000 WITH 'Data was updated successfully'.
*    ELSE.
*      ROLLBACK WORK.
*      MESSAGE S000 WITH 'Data update error'.
*    ENDIF.
*  ELSE.
*    MESSAGE I000 WITH 'No data was updated'.
*  ENDIF.

** End of change
ENDFORM.                    " SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form  selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FNAME  text
*----------------------------------------------------------------------*
FORM SELECTION_SCREEN  USING  FNAME.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.*,*.*.'
            MODE             = 'O'
       IMPORTING
            FILENAME         = FNAME
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

ENDFORM.                    " selection_screen
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_FILE TABLES P_TABLE.

  CHECK FNAME NE ' '.


  DATA : LT_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : LW_START_COL TYPE I VALUE '1',
         LW_START_ROW TYPE I VALUE '2',
         LW_END_COL   TYPE I VALUE '20',
         LW_END_ROW   TYPE I VALUE '3000'.
  FIELD-SYMBOLS : <LW_FS>.
  DATA : LW_FIELD_TYPE.

  CALL FUNCTION 'ZFMM_EXCEL_UPLOAD'
       EXPORTING
            FILENAME    = FNAME
            I_BEGIN_COL = LW_START_COL
            I_BEGIN_ROW = LW_START_ROW
            I_END_COL   = LW_END_COL
            I_END_ROW   = LW_END_ROW
       TABLES
            OUTAB       = P_TABLE.

*  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*       EXPORTING
*            FILENAME                = FNAME
*            I_BEGIN_COL             = LW_START_COL
*            I_BEGIN_ROW             = LW_START_ROW
*            I_END_COL               = LW_END_COL
*            I_END_ROW               = LW_END_ROW
*       TABLES
*            INTERN                  = LT_ITAB
*       EXCEPTIONS
*            INCONSISTENT_PARAMETERS = 1
*            UPLOAD_OLE              = 2
*            OTHERS                  = 3.
*
*
*  IF SY-SUBRC NE 0.
*    MESSAGE E000 WITH 'File Upload Failed !'.
*  ENDIF.
*
*
*  CHECK NOT LT_ITAB[] IS INITIAL.
*
*-- Delete Header line: row from 1 to 1.
*
*  DELETE LT_ITAB WHERE ROW LE 1.
*
*  SORT LT_ITAB BY ROW COL.
*  REFRESH P_TABLE.
*
*  LOOP AT LT_ITAB.
*    MOVE : LT_ITAB-COL TO LW_INDEX.
*    ASSIGN COMPONENT LW_INDEX OF STRUCTURE P_TABLE TO <LW_FS>.
*
*    DESCRIBE FIELD <LW_FS> TYPE LW_FIELD_TYPE.
*
*    IF LW_FIELD_TYPE = 'D'.  "'MM/DD/YYYY"
*      CONCATENATE LT_ITAB-VALUE+6(4)    "YEAR  (YYYY)
*                  LT_ITAB-VALUE+0(2)    "MONTH (MM)
*                  LT_ITAB-VALUE+3(2)    "DAY   (DD)
*                              INTO <LW_FS>.
*    ELSE.
*      MOVE : LT_ITAB-VALUE TO <LW_FS>.
*    ENDIF.
*
*    AT END OF ROW.
*      APPEND P_TABLE.
*      CLEAR P_TABLE.
*    ENDAT.
*  ENDLOOP.

ENDFORM.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_ANSWER  text
*----------------------------------------------------------------------*
FORM CHECK_DATA TABLES PT_DATA STRUCTURE ZTMM_BDP
                       CHANGING P_ANSWER.
  DATA: L_LINE(3),
        L_TEXT(50),
        L_DISPLAY(1),
        L_INDEX LIKE SY-TABIX..

  LOOP AT PT_DATA.
    CLEAR: L_LINE, L_TEXT, L_DISPLAY.
    L_LINE = SY-TABIX.
    L_INDEX  = SY-TABIX.

    IF PT_DATA-MATNR IS INITIAL.
*      CONCATENATE 'Material number is blank in line' L_LINE
*             INTO L_TEXT
*        SEPARATED BY SPACE.
      L_TEXT = 'Material number is blank'.
      P_ANSWER = 'X'.
      L_DISPLAY = 'X'.
    ELSE.
      SELECT * FROM MARA UP TO 1 ROWS
      WHERE MATNR = PT_DATA-MATNR.
      ENDSELECT.
      IF SY-SUBRC <> 0.
        CONCATENATE 'Material in record'    "L_LINE
            'is not found in SAP'
            INTO L_TEXT SEPARATED BY SPACE.
        P_ANSWER = 'X'.
        L_DISPLAY = 'X'.
      ENDIF.
    ENDIF.

** Changed by Furong on 12/21/10
    IF L_DISPLAY = 'X'.
      PT_DATA-FLAG = 'E'.
      PT_DATA-MESSAGE = L_TEXT.
      MODIFY PT_DATA INDEX L_INDEX.
      CONTINUE.
    ENDIF.
*    IF L_DISPLAY = 'X'.
*      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
*      EXPORTING
*        TITEL              = 'Error '
*        TEXTLINE1          = L_TEXT
*    TEXTLINE2          = 'Please check the data and re-upload again '
**      TEXTLINE3          = ' '
*        START_COLUMN       = 15
*        START_ROW          = 6.
*
*      CONTINUE.
*    ENDIF.
** End of change

    IF PT_DATA-WERKS IS INITIAL.
*      CONCATENATE 'Plant is blank in line' L_LINE INTO L_TEXT
*        SEPARATED BY SPACE.
      L_TEXT = 'Material number is blank'.
      P_ANSWER = 'X'.
      L_DISPLAY = 'X'.
    ELSE.
      SELECT * FROM MARC UP TO 1 ROWS
      WHERE MATNR = PT_DATA-MATNR
        AND WERKS = PT_DATA-WERKS.
      ENDSELECT.
      IF SY-SUBRC <> 0.
        CONCATENATE 'Material in record'    "L_LINE
           'is not found in plant'
           INTO L_TEXT SEPARATED BY SPACE.
        P_ANSWER = 'X'.
        L_DISPLAY = 'X'.
      ENDIF.
    ENDIF.

** Changed by Furong on 12/21/10
    IF L_DISPLAY = 'X'.
      PT_DATA-FLAG = 'E'.
      PT_DATA-MESSAGE = L_TEXT.
      MODIFY PT_DATA INDEX L_INDEX.
      CONTINUE.
    ENDIF.
*    IF L_DISPLAY = 'X'.
*      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
*      EXPORTING
*        TITEL              = 'Error '
*        TEXTLINE1          = L_TEXT
*    TEXTLINE2          = 'Please check the data and re-upload again '
**      TEXTLINE3          = ' '
*        START_COLUMN       = 15
*        START_ROW          = 6.
*
*      CONTINUE.
*    ENDIF.
** End of change

    IF PT_DATA-LGORT IS INITIAL.
      CONCATENATE 'Storage location in record' " L_LINE
       'is blank'
       INTO L_TEXT SEPARATED BY SPACE.
      P_ANSWER = 'X'.
      L_DISPLAY = 'X'.
    ELSE.
      SELECT * FROM MARD UP TO 1 ROWS
      WHERE MATNR = PT_DATA-MATNR
        AND WERKS = PT_DATA-WERKS
        AND LGORT = PT_DATA-LGORT.
      ENDSELECT.
      IF SY-SUBRC <> 0.
        CONCATENATE 'Storage loca in record'     " L_LINE
          'is not found in MARD' INTO L_TEXT SEPARATED BY SPACE.
        P_ANSWER = 'X'.
        L_DISPLAY = 'X'.
      ENDIF.
    ENDIF.

** Changed by Furong on 12/21/10
    IF L_DISPLAY = 'X'.
      PT_DATA-FLAG = 'E'.
      PT_DATA-MESSAGE = L_TEXT.
      MODIFY PT_DATA INDEX L_INDEX.
      CONTINUE.
    ENDIF.

*    IF L_DISPLAY = 'X'.
*      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
*      EXPORTING
*        TITEL              = 'Error '
*        TEXTLINE1          = L_TEXT
*    TEXTLINE2          = 'Please check the data and re-upload again '
**      TEXTLINE3          = ' '
*        START_COLUMN       = 15
*        START_ROW          = 6.
*
*      CONTINUE.
*    ENDIF.
** End of change

    IF PT_DATA-LGPLA IS INITIAL.
*      CONCATENATE 'Storage bin is blank in line' l_line INTO L_TEXT
*        SEPARATED BY SPACE.
      L_TEXT = 'Storage bin is blank in line'.

      P_ANSWER = 'X'.
      L_DISPLAY = 'X'.
    ENDIF.

** Changed by Furong on 12/21/10
    IF L_DISPLAY = 'X'.
      PT_DATA-FLAG = 'E'.
      PT_DATA-MESSAGE = L_TEXT.
      MODIFY PT_DATA INDEX L_INDEX.
    ENDIF.

*    IF L_DISPLAY = 'X'.
*      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
*      EXPORTING
*        TITEL              = 'Error '
*        TEXTLINE1          = L_TEXT
*    TEXTLINE2          = 'Please check the data and re-upload again '
**      TEXTLINE3          = ' '
*        START_COLUMN       = 15
*        START_ROW          = 6.
*
*    ENDIF.
** End of change

  ENDLOOP.

** Changed by Furong on 12/21/10

  LOOP AT PT_DATA WHERE FLAG = ' '.
    L_INDEX = SY-TABIX.
    SELECT * FROM ZTMM_BDP UP TO 1 ROWS
    WHERE MATNR = PT_DATA-MATNR
     AND WERKS = PT_DATA-WERKS
     AND LGORT = PT_DATA-LGORT
     AND LGPLA = PT_DATA-LGPLA.
    ENDSELECT.
    IF SY-SUBRC = 0.
      CONCATENATE PT_DATA-MATNR 'was existed in plant' PT_DATA-WERKS
           INTO L_TEXT
           SEPARATED BY SPACE.
      P_ANSWER = 'X'.
      PT_DATA-FLAG = 'E'.
      PT_DATA-MESSAGE = L_TEXT.
      MODIFY PT_DATA INDEX L_INDEX.
    ENDIF.
    CLEAR: L_TEXT.
  ENDLOOP.


*  IF P_ANSWER IS INITIAL.
*    LOOP AT PT_DATA.
*      SELECT * FROM ZTMM_BDP UP TO 1 ROWS
*      WHERE MATNR = PT_DATA-MATNR
*       AND WERKS = PT_DATA-WERKS
*       AND LGORT = PT_DATA-LGORT
*       AND LGPLA = PT_DATA-LGPLA.
*      ENDSELECT.
*      IF SY-SUBRC = 0.
*        CONCATENATE PT_DATA-MATNR 'was existed in plant' PT_DATA-WERKS
*             INTO L_TEXT
*             SEPARATED BY SPACE.
*        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
*             EXPORTING
*                  TITEL        = 'Error '
*                  TEXTLINE1    = L_TEXT
*                  TEXTLINE2    = PT_DATA-LGORT
*                  TEXTLINE3    = PT_DATA-LGPLA
*                  START_COLUMN = 15
*                  START_ROW    = 6.
*        P_ANSWER = 'X'.
*      ENDIF.
*      CLEAR: L_TEXT.
*    ENDLOOP.
*  ENDIF.
** End of change
ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.
  REFRESH IT_ITAB.
  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_ITAB.
    APPEND IT_ITAB.
  ENDLOOP.
ENDFORM.                    " UPDATE_TABLE
