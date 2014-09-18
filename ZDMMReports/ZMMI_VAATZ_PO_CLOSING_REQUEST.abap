*&----------------------------------------------------------------------
*&*
*& TASK-ID : MM508 &*
*& TASK-TITLE : Create P/O closing request &*
*& TASK-TYPE : ON-LINE &*
*& PROCESS-NO :  51.03.01.04 &*
*& PROCESS-NAME : GR for General Material &*
*& PROGRAM-ID : ZHINMMO90600 &*
*& T-CODE : ZHINMMO90600 &*
*& PACKAGE : ZHINMM &*
*& DATE : 2011.06.14 &*
*& DEVELOPER : Bae, Byungsung &*
*& GENERAL-INFORMATION :
*   This is a online program to maintain P/O closing indicator
*   for general material. &*
*& PROCESS-FLOW :
*   1. Selection-Screen: Select-options & parameters entry.
*    - Common options : Plant, Storage location, Purchase order,
*                       Material, vendor, document date
*    - Display  options : closing user, closing date
*   2. Perform related job according to the button click
*      on selection-screen.
*      - button : Create, Change, Display
*   3. Display List screen(ALV screen).
*      - Initial screen or Change screen or Display screen.
*   4. Save the P/O closing data created or changed if the user clicks
*      the save button. &*
*& OTHER-CONSIDERATIONS :
*    &*
*&----------------------------------------------------------------------
*-&*
*& VERSION-NO : #1 &*
*& REQUEST :   &*
*& DEVELOPMENT :  &*
*& REQ.-DATE :  &*
*& FINISH-DATE :  &*
*& DESCRIPTION-OF-CHANGE :  &*
*&-------------------------------------------------------------------

*-&* copy from HMI  zhinmmo90600

REPORT  ZMMI_VAATZ_PO_CLOSING_REQUEST MESSAGE-ID ZMMM.
INCLUDE: <ICON>.
TABLES: USR01, T001W, T001L, EKKO, MARA, LFA1, SSCRFIELDS,
        ZTMM_VAZ_VZ028.

*---// Internal tables
DATA: BEGIN OF ST_0100.
        INCLUDE STRUCTURE ZTMM_VAZ_VZ028.
DATA:   MAKTX      LIKE MAKT-MAKTX,
        NAME1      LIKE LFA1-NAME1,
        ELIKZ      LIKE EKPO-ELIKZ,
        REQ,                               "P/O Closing Request
        IFRESULT   LIKE  ZTMM_VAZ_028_LOG-IFRESULT,
        ZMSG(255),
        SUCCESS,
        CHANGED,
        CELLTAB    TYPE LVC_T_STYL,        " Cell type
      END   OF ST_0100.

DATA: IT_0100 LIKE ST_0100 OCCURS 0 WITH HEADER LINE.

DATA: IT_9055 LIKE ZTMM_VAZ_VZ028 OCCURS 0 WITH HEADER LINE.

DATA: IT_EXCLUDE TYPE TABLE OF RSEXFCODE WITH HEADER LINE.

DATA: BEGIN OF IT_FCODE OCCURS 0,
        UCOMM   LIKE   SY-UCOMM,
      END   OF IT_FCODE.

*---// Global variable
DATA: G_MODE.

*---// Constants
CONSTANTS: C_CHECKED                VALUE 'X',
           C_CREATE                 VALUE 'C',
           C_CHANGE                 VALUE 'M',
           C_DISPLAY                VALUE 'D'.


*-----/// Grid Control : START
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

* Splitter Declaration
DATA: G_SPLITTER_0100     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      G_SP_CONTAINER_0100 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CONTROL_0100      TYPE        SCRFNAME VALUE 'CC_0100'.

* Declare for Grid
DATA: G_CONTAINER_0100 TYPE REF TO CL_GUI_CONTAINER,
      G_GRID_0100      TYPE REF TO CL_GUI_ALV_GRID,
      G_LAYOUT_0100     TYPE LVC_S_LAYO,
      G_VARIANT_0100    TYPE DISVARIANT,
      IT_SORT_0100      TYPE LVC_T_SORT   WITH HEADER LINE,
      IT_FILTER_0100    TYPE LVC_T_FILT   WITH HEADER LINE,
      IT_EXCLUDE_0100   TYPE UI_FUNCTIONS WITH HEADER LINE.

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Interal tables for ALV GRID
DATA : IT_ROWS         TYPE LVC_T_ROW  WITH HEADER LINE,
       IT_ROW_NO       TYPE LVC_T_ROID WITH HEADER LINE,
       IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

* Global variable for ALV GRID
DATA : G_FIELDNAME   TYPE LINE OF SLIS_T_FIELDCAT_ALV,
       G_REPID       LIKE SY-REPID,
       G_CNT         TYPE I,                   "Field count
       G_SCROLL      TYPE LVC_S_STBL,
       G_SAVE        TYPE C   VALUE 'A'.       "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: G_CONTAINER(100),
      G_CONTROL(100),
      G_SPLITTER(100),
      G_GRID(100),
      G_ITAB(100),
      G_STRUCTURE LIKE DD02L-TABNAME.

FIELD-SYMBOLS: <FS_CONTAINER> TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
               <FS_CONTROL>   TYPE        SCRFNAME,
               <FS_SPLITTER>  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
               <FS_GRID>      TYPE REF TO CL_GUI_ALV_GRID,
               <FS_ITAB>      TYPE STANDARD TABLE,
               <FS_ITAB_OLD>  TYPE STANDARD TABLE.

CONSTANTS: C_STRUCTURE(100) VALUE 'ST_'.

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS: DATA_CHANGED
        FOR  EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED  E_UCOMM.

    METHODS: TOOLBAR
        FOR  EVENT TOOLBAR OF CL_GUI_ALV_GRID
             IMPORTING E_OBJECT E_INTERACTIVE.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED
                                      E_UCOMM.
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = 'DUMMY'.
  ENDMETHOD.                    "handle_data_changed

  METHOD TOOLBAR.
*    PERFORM toolbar USING e_object e_interactive.
  ENDMETHOD.                    "handle_toolbar "handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-T01.
PARAMETERS: P_WERKS LIKE ZTMM_VAZ_VZ028-WERKS OBLIGATORY,
            P_LGORT LIKE ZTMM_VAZ_VZ028-LGORT  " OBLIGATORY
                         MATCHCODE OBJECT ZMM_LGORT.
SELECT-OPTIONS: S_EBELN FOR ZTMM_VAZ_VZ028-EBELN,
                S_MATNR FOR ZTMM_VAZ_VZ028-MATNR,
                S_LIFNR FOR ZTMM_VAZ_VZ028-LIFNR,
                S_BEDAT FOR EKKO-BEDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-T02.
PARAMETERS: P_ERNAM LIKE ZTMM_VAZ_VZ028-ERNAM.
SELECT-OPTIONS: S_ERDAT FOR ZTMM_VAZ_VZ028-ERDAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK2.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_SELECTION_SCREEN_ATTR.

AT SELECTION-SCREEN.

  PERFORM CHECK_RTN.
  PERFORM USER_COMMAND.
  PERFORM READ_DATA.
  IF NOT IT_0100[] IS INITIAL.
    CALL SCREEN 0100.
  ENDIF.

START-OF-SELECTION.


END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  SET PF-STATUS 'G0100' EXCLUDING IT_FCODE.
  CASE G_MODE.
    WHEN C_CREATE.
      SET TITLEBAR  'T0100' WITH '(Create)'.
    WHEN C_DISPLAY.
      SET TITLEBAR  'T0100' WITH '(Display)'.
    WHEN C_CHANGE.
      SET TITLEBAR  'T0100' WITH '(Change)'.
  ENDCASE.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: SY-UCOMM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_CONTROL OUTPUT.
  PERFORM CREATE_SPLITTER USING '0100'.
  PERFORM CREATE_GRID USING '0100'  '1' '1'.
ENDMODULE.                 " CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_SPLITTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0333   text
*----------------------------------------------------------------------*
FORM CREATE_SPLITTER USING P_DYNNR.
  CONCATENATE: 'G_SP_CONTAINER_' P_DYNNR INTO G_CONTAINER.
  ASSIGN:      (G_CONTAINER)               TO   <FS_CONTAINER>.

  CONCATENATE: 'G_CONTROL_' P_DYNNR INTO G_CONTROL.
  ASSIGN:      (G_CONTROL)            TO   <FS_CONTROL>.

  CONCATENATE: 'G_SPLITTER_' P_DYNNR INTO G_SPLITTER.
  ASSIGN:      (G_SPLITTER)            TO   <FS_SPLITTER>.


  IF <FS_CONTAINER> IS INITIAL.
    CREATE OBJECT <FS_CONTAINER>
      EXPORTING
        CONTAINER_NAME              = <FS_CONTROL>
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT <FS_SPLITTER>
      EXPORTING
        PARENT            = <FS_CONTAINER>
        ROWS              = 1
        COLUMNS           = 1
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0337   text
*      -->P_0338   text
*      -->P_0339   text
*----------------------------------------------------------------------*
FORM CREATE_GRID USING P_GRID P_ROW P_COLUMN.
  DATA: L_CONTAINER(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO CL_GUI_CONTAINER.

  CONCATENATE: 'G_CONTAINER_' P_GRID INTO L_CONTAINER.
  ASSIGN:      (L_CONTAINER)          TO   <LFS_CONTAINER>.

  IF <LFS_CONTAINER> IS INITIAL.
    PERFORM CREATE_GRID_CONTAINER USING P_GRID P_ROW P_COLUMN.
    PERFORM BUILD_FIELD_CATALOG   USING P_GRID.
    PERFORM SET_ATTRIBUTE         USING P_GRID.
    PERFORM SET_CELL_TYPE USING P_GRID.
    PERFORM ASSIGN_ITAB_TO_ALV    USING P_GRID.
    PERFORM ASSIGN_EVENT          USING P_GRID.
  ELSE.
    PERFORM SET_CELL_TYPE USING P_GRID.
    PERFORM REFRESH_GRID USING P_GRID.
  ENDIF.
ENDFORM.                    " CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*      -->P_PV_ROW  text
*      -->P_PV_COLUMN  text
*----------------------------------------------------------------------*
FORM CREATE_GRID_CONTAINER USING P_GRID P_ROW P_COLUMN.
  DATA: L_CONTAINER(100),
        L_GRID(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO   CL_GUI_CONTAINER,
                 <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'G_SPLITTER_' P_GRID  INTO G_SPLITTER.
  ASSIGN:      (G_SPLITTER)            TO   <FS_SPLITTER>.

  CONCATENATE: 'G_CONTAINER_' P_GRID INTO L_CONTAINER.
  ASSIGN:      (L_CONTAINER)          TO   <LFS_CONTAINER>.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CALL METHOD <FS_SPLITTER>->GET_CONTAINER
    EXPORTING
      ROW       = P_ROW
      COLUMN    = P_COLUMN
    RECEIVING
      CONTAINER = <LFS_CONTAINER>.

  CREATE OBJECT <LFS_GRID>
    EXPORTING
      I_PARENT          = <LFS_CONTAINER>
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CASE P_ROW.
    WHEN 1.
      CALL METHOD <FS_SPLITTER>->SET_ROW_HEIGHT
        EXPORTING
          ID                = P_ROW
          HEIGHT            = 20
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    WHEN 2.
      CALL METHOD <FS_SPLITTER>->SET_ROW_HEIGHT
        EXPORTING
          ID                = P_ROW
          HEIGHT            = 36
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
  ENDCASE.
ENDFORM.                    " CREATE_GRID_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_GRID.
*--- adjust field catalog to suppress the output of already
*  displayed key fields of structure

  DATA: L_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

*  CALL METHOD <alv>->get_frontend_fieldcatalog
*    IMPORTING
*      et_fieldcatalog = it_fieldcat[].

  PERFORM SET_FIELDNAME USING P_GRID.
  PERFORM SET_SCREEN_FIELDS USING P_GRID.

  CALL METHOD <LFS_GRID>->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = IT_FIELDCAT[].
ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_FIELDNAME  USING P_GRID.

  DATA: L_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  MOVE: SY-REPID TO G_REPID.
  CONCATENATE C_STRUCTURE P_GRID INTO L_ITAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = G_REPID
            I_INTERNAL_TABNAME = L_ITAB
            I_INCLNAME         = G_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME[].
ENDFORM.                    " SET_FIELDNAME
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS USING P_GRID.
  CASE P_GRID.
    WHEN '0100'.
      PERFORM SET_SCREEN_FIELDS_0100.
  ENDCASE.
ENDFORM.                    " SET_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS_WO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_0100.
  CASE G_MODE.
    WHEN C_CREATE.
      PERFORM SET_SCREEN_FIELDS_0100_CRE.
    WHEN C_DISPLAY.
      PERFORM SET_SCREEN_FIELDS_0100_DISP.
    WHEN C_CHANGE.
      PERFORM SET_SCREEN_FIELDS_0100_CRE.
  ENDCASE.
ENDFORM.                    " SET_SCREEN_FIELDS_WO
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0817   text
*      -->P_0818   text
*      -->P_0819   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   PT_FIELDCAT  STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.

  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: PT_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO G_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E999(ZZ) WITH 'Check field catalog:' P_FIELD.
    ENDIF.

    MOVE: G_FIELDNAME-FIELDNAME TO PT_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'PT_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE  TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    IF PT_FIELDCAT-COL_POS IS INITIAL.
      ADD 1 TO G_CNT.
      PT_FIELDCAT-COL_POS = G_CNT.
    ENDIF.
    APPEND PT_FIELDCAT.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTE USING P_GRID.
  PERFORM SET_LAYOUT                USING P_GRID.
  PERFORM SET_VARIANT               USING P_GRID.
*  PERFORM set_sort_total_field      USING p_grid.
  PERFORM SET_FILTER                USING P_GRID.
ENDFORM.                    " SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT USING P_GRID.
  DATA: L_GRID(100),
        L_LAYOUT(100).

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_LAYOUT> TYPE LVC_S_LAYO.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'G_LAYOUT_' P_GRID INTO L_LAYOUT.
  ASSIGN:      (L_LAYOUT)         TO   <LFS_LAYOUT>.

  CALL METHOD <LFS_GRID>->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = <LFS_LAYOUT>.

  IF <LFS_LAYOUT> IS INITIAL.

    CASE P_GRID.
      WHEN '0100'.
        <LFS_LAYOUT>-EDIT       = ' '.     " Edit Mode Enable
        <LFS_LAYOUT>-SEL_MODE   = 'A'.    " mode for select col and row
        <LFS_LAYOUT>-LANGUAGE   = SY-LANGU." Language Key
*        <lfs_layout>-totals_bef = 'X'.     " Upper Total Line
        <LFS_LAYOUT>-ZEBRA      = 'X'.         " Emphasize C250
*        <lfs_layout>-totals_bef = 'X'.        " Upper Total Line
*        <lfs_layout>-no_totline = ' '.        " Disable Total Line
        <LFS_LAYOUT>-INFO_FNAME = 'ROW_COLOR'. " Line color field
        <LFS_LAYOUT>-STYLEFNAME = 'CELLTAB'.
    ENDCASE.
  ENDIF.

  CALL METHOD <LFS_GRID>->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = <LFS_LAYOUT>.
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_VARIANT USING P_GRID.
  DATA: L_GRID(100),
        L_VARIANT(100).

  FIELD-SYMBOLS: <LFS_GRID>    TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_VARIANT> TYPE DISVARIANT.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'G_VARIANT_' P_GRID INTO L_VARIANT.
  ASSIGN:      (L_VARIANT)         TO   <LFS_VARIANT>.


  CALL METHOD <LFS_GRID>->GET_VARIANT
    IMPORTING
      ES_VARIANT = <LFS_VARIANT>.

  IF <LFS_VARIANT> IS INITIAL.
    <LFS_VARIANT>-REPORT      = SY-REPID.
    <LFS_VARIANT>-HANDLE      = SPACE.
    <LFS_VARIANT>-LOG_GROUP   = SPACE.
    <LFS_VARIANT>-USERNAME    = SPACE.
    <LFS_VARIANT>-VARIANT     = SPACE.
    <LFS_VARIANT>-TEXT        = SPACE.
*    <lfs_variant>-dependvars  = space.
  ENDIF.
ENDFORM.                    " SET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_SORT_TOTAL_FIELD USING P_GRID.
  DATA: L_GRID(100),
        L_SORT(100),
        LST_SORT LIKE IT_SORT_0100.

  FIELD-SYMBOLS: <LFS_GRID> TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_SORT> TYPE LVC_T_SORT.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'IT_SORT_' P_GRID '[]' INTO L_SORT.
  ASSIGN:      (L_SORT)               TO   <LFS_SORT>.

  CALL METHOD <LFS_GRID>->GET_SORT_CRITERIA
    IMPORTING
      ET_SORT = <LFS_SORT>.

  REFRESH <LFS_SORT>.

  CHECK G_MODE EQ C_DISPLAY.

  CASE P_GRID.
    WHEN '0100'.

      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'MATNR'.
      LST_SORT-UP        = 'X'.
      APPEND LST_SORT TO <LFS_SORT>.

      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'MAKTX'.
      LST_SORT-UP        = 'X'.
      APPEND LST_SORT TO <LFS_SORT>.

      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'LIFNR'.
      LST_SORT-UP        = 'X'.
      APPEND LST_SORT TO <LFS_SORT>.

      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'NAME1'.
      LST_SORT-UP        = 'X'.
      APPEND LST_SORT TO <LFS_SORT>.

*      CLEAR : lst_sort.
*      lst_sort-fieldname = 'ZNPRCNO'.
*      APPEND lst_sort TO <lfs_sort>.
  ENDCASE.

  CALL METHOD <LFS_GRID>->SET_SORT_CRITERIA
    EXPORTING
      IT_SORT = <LFS_SORT>.
ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_FILTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_FILTER  USING P_GRID.
  DATA: L_GRID(100),
        L_FILTER(100).

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_FILTER> TYPE LVC_T_FILT.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'IT_FILTER_' P_GRID '[]' INTO L_FILTER.
  ASSIGN:      (L_FILTER)               TO   <LFS_FILTER>.

  CALL METHOD <LFS_GRID>->GET_FILTER_CRITERIA
    IMPORTING
      ET_FILTER = <LFS_FILTER>.

  IF <LFS_FILTER> IS INITIAL.
*    CASE pv_grid.
*      WHEN 'KEY_SUM'.
*        CLEAR : it_filter_key_sum. REFRESH it_filter_key_sum.
*    ENDCASE.
  ENDIF.
ENDFORM.                    " SET_FILTER
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV USING P_GRID.
  DATA: L_GRID(100),
        L_ITAB(100),
        L_LAYOUT(100),
        L_VARIANT(100),
        L_SORT(100),
        L_FILTER(100),
        L_EXCLUDE(100).

  FIELD-SYMBOLS: <LFS_GRID>    TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_ITAB>    TYPE STANDARD TABLE,
                 <LFS_LAYOUT>  TYPE LVC_S_LAYO,
                 <LFS_VARIANT> TYPE DISVARIANT,
                 <LFS_SORT>    TYPE LVC_T_SORT,
                 <LFS_FILTER>  TYPE LVC_T_FILT,
                 <LFS_EXCLUDE> TYPE UI_FUNCTIONS.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'G_LAYOUT_' P_GRID INTO L_LAYOUT.
  ASSIGN:      (L_LAYOUT)         TO   <LFS_LAYOUT>.

  CONCATENATE: 'G_VARIANT_' P_GRID INTO L_VARIANT.
  ASSIGN:      (L_VARIANT)         TO   <LFS_VARIANT>.

  CONCATENATE: 'IT_'      P_GRID '[]' INTO L_ITAB.
  ASSIGN:      (L_ITAB)               TO   <LFS_ITAB>.

  CONCATENATE: 'IT_SORT_' P_GRID '[]' INTO L_SORT.
  ASSIGN:      (L_SORT)               TO   <LFS_SORT>.

  CONCATENATE: 'IT_FILTER_' P_GRID '[]' INTO L_FILTER.
  ASSIGN:      (L_FILTER)               TO   <LFS_FILTER>.

  CONCATENATE: 'IT_EXCLUDE_' P_GRID '[]' INTO L_EXCLUDE.
  ASSIGN:      (L_EXCLUDE)               TO   <LFS_EXCLUDE>.

  CONCATENATE: C_STRUCTURE P_GRID INTO G_STRUCTURE.

  CALL METHOD <LFS_GRID>->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_STRUCTURE_NAME              = G_STRUCTURE
      IS_LAYOUT                     = <LFS_LAYOUT>
      IT_TOOLBAR_EXCLUDING          = <LFS_EXCLUDE>
      I_SAVE                        = G_SAVE
      IS_VARIANT                    = <LFS_VARIANT>
      I_DEFAULT                     = SPACE
    CHANGING
      IT_FIELDCATALOG               = IT_FIELDCAT[]
      IT_FILTER                     = <LFS_FILTER>
      IT_SORT                       = <LFS_SORT>
      IT_OUTTAB                     = <LFS_ITAB>
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM ASSIGN_EVENT USING P_GRID.
  DATA: L_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

*--  Regist event for Edit
  CALL METHOD <LFS_GRID>->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.

  CASE P_GRID.
    WHEN '0100'.
      SET HANDLER EVENT_RECEIVER->DATA_CHANGED        FOR <LFS_GRID>.
      SET HANDLER EVENT_RECEIVER->TOOLBAR             FOR <LFS_GRID>.
  ENDCASE.

  CALL METHOD <LFS_GRID>->SET_TOOLBAR_INTERACTIVE.
ENDFORM.                    " ASSIGN_EVENT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM REFRESH_GRID USING P_GRID.
  DATA: L_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  G_SCROLL-ROW = 'X'.
  G_SCROLL-COL = 'X'.

  CALL METHOD <LFS_GRID>->REFRESH_TABLE_DISPLAY
    EXPORTING
*      i_soft_refresh = 'X'
      IS_STABLE      = G_SCROLL.     "## ### ### refresh
ENDFORM.                    " REFRESH_GRID
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR SY-UCOMM.
*      PERFORM check_changed_data_0100.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      CLEAR: SY-UCOMM.
      PERFORM SAVE_0100_RTN.
    WHEN 'CHANGE'.
      CLEAR: SY-UCOMM.
      PERFORM CHANGE_0100_RTN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION .
  MOVE: 'I'      TO S_BEDAT-SIGN,
        'EQ'     TO S_BEDAT-OPTION,
        SY-DATUM TO S_BEDAT-HIGH.
  S_BEDAT-LOW = SY-DATUM - 31.

  APPEND S_BEDAT.

  MOVE: 'I'      TO S_ERDAT-SIGN,
        'EQ'     TO S_ERDAT-OPTION,
        SY-DATUM TO S_ERDAT-HIGH.
  S_ERDAT-LOW = SY-DATUM - 6.

  APPEND S_ERDAT.

  SSCRFIELDS-FUNCTXT_01 = 'Create'.
  SSCRFIELDS-FUNCTXT_02 = 'Display'.

  APPEND: 'ONLI' TO IT_EXCLUDE.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RTN .
  " Check plant
  SELECT SINGLE * FROM T001W WHERE WERKS = P_WERKS.
  IF SY-SUBRC NE 0.
    MESSAGE E009 WITH TEXT-M04.
  ENDIF.

  " Check storage location
*  SELECT SINGLE * FROM T001L WHERE WERKS = P_WERKS
*                               AND LGORT = P_LGORT.
*  IF SY-SUBRC NE 0.
*    MESSAGE E000 WITH TEXT-M05 P_WERKS.
*  ENDIF.

  " Check PO
  SELECT SINGLE * FROM EKKO WHERE EBELN IN S_EBELN.
  IF SY-SUBRC NE 0.
    MESSAGE E999 WITH TEXT-M06.
  ENDIF.

  " Check material
  SELECT SINGLE * FROM MARA WHERE MATNR IN S_MATNR.
  IF SY-SUBRC NE 0.
    MESSAGE E999 WITH TEXT-M07.
  ENDIF.

  " Check vendor
  SELECT SINGLE * FROM LFA1 WHERE LIFNR IN S_LIFNR.
  IF SY-SUBRC NE 0.
    MESSAGE E999 WITH TEXT-M08.
  ENDIF.
ENDFORM.                    " CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_SELECTION_SCREEN_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SELECTION_SCREEN_ATTR .
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
       EXPORTING
            P_STATUS  = '%_00'
            P_PROGRAM = 'RSSYSTDB'
       TABLES
            P_EXCLUDE = IT_EXCLUDE.
ENDFORM.                    " SET_SELECTION_SCREEN_ATTR
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA .

  CASE G_MODE.
    WHEN C_CREATE.
      PERFORM READ_DATA_FOR_CREATE.
    WHEN C_DISPLAY.
      PERFORM READ_DATA_FOR_DISPLAY.
  ENDCASE.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_CELL_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GRID  text
*----------------------------------------------------------------------*
FORM SET_CELL_TYPE USING P_GRID.
  DATA: LS_S_FCAT TYPE LVC_S_FCAT.

  DATA: LT_CELL   TYPE LVC_T_STYL,
        LS_CELL   TYPE LVC_S_STYL,
        LT_FCAT   TYPE LVC_T_FCAT.

  DATA: L_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'G_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  IT_0100-CELLTAB = LT_CELL.
  MODIFY IT_0100 TRANSPORTING CELLTAB WHERE EBELN >= SPACE.

  LT_FCAT[] = IT_FIELDCAT[].

  SORT LT_FCAT BY FIELDNAME.
  LOOP AT  LT_FCAT  INTO LS_S_FCAT
                    WHERE EDIT = 'X'.

    LS_CELL-FIELDNAME = LS_S_FCAT-FIELDNAME.
    LS_CELL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

    INSERT LS_CELL INTO TABLE LT_CELL.
  ENDLOOP.

  CASE G_MODE.
    WHEN C_CREATE.
      IT_0100-CELLTAB = LT_CELL.
      MODIFY IT_0100 TRANSPORTING CELLTAB WHERE SUCCESS = 'X'.
    WHEN C_CHANGE.
      IT_0100-CELLTAB = LT_CELL.
*      MODIFY IT_0100 TRANSPORTING CELLTAB WHERE IFRESULT = 'Z'.
      MODIFY IT_0100 TRANSPORTING CELLTAB WHERE IFRESULT = 'S'.
  ENDCASE.
ENDFORM.                    " SET_CELL_TYPE
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS_0100_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_0100_DISP .
  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                  'S' 'EBELN'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'EBELN',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'EBELP',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'MATNR',
                                  ' ' 'OUTPUTLEN'    '18',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'REF_TABLE'   'MAKT',
                                  ' ' 'REF_FIELD'   'MAKTX',
                                  ' ' 'OUTPUTLEN'    '20',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'LIFNR',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'NAME1'       ' ',
                                  ' ' 'REF_TABLE'   'LFA1',
                                  ' ' 'REF_FIELD'   'NAME1',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'MENGE'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'MENGE',
                                  ' ' 'COLTEXT'     'P/O Qty',
                                  ' ' 'QFIELDNAME'  'MEINS',
                                  ' ' 'OUTPUTLEN'    '8',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'MEINS'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'MEINS',
                                  ' ' 'OUTPUTLEN'    '4',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'WEMNG'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'WEMNG',
                                  ' ' 'COLTEXT'     'G/R Qty',
                                  ' ' 'QFIELDNAME'  'MEINS',
                                  ' ' 'OUTPUTLEN'    '8',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'REQ'         ' ',
                                  ' ' 'COLTEXT'     'P/O Closing Req',
                                  ' ' 'CHECKBOX'    'X',
                                  ' ' 'OUTPUTLEN'    '13',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZREASON'     ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'ZREASON',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'IFRESULT'    ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'IFRESULT',
                                  ' ' 'OUTPUTLEN'    '10',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ELIKZ'       ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'ELIKZ',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ERNAM'       ' ',
                                  ' ' 'REF_TABLE'   'EKKO',
                                  ' ' 'REF_FIELD'   'ERNAM',
                                ' ' 'COLTEXT'     'Closing Req.User',
                                  ' ' 'OUTPUTLEN'    '15',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ERDAT'       ' ',
                                  ' ' 'REF_TABLE'   'EKKO',
                                  ' ' 'REF_FIELD'   'ERDAT',
                                ' ' 'COLTEXT'     'Closing Req.Date',
                                  ' ' 'OUTPUTLEN'    '15',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZMSG'  ' ',
                                  ' ' 'COLTEXT'      'Mssage',
                                  ' ' 'OUTPUTLEN'    '100',
                                  'E' 'EMPHASIZE'    'C250'.

ENDFORM.                    " SET_SCREEN_FIELDS_0100_DISP
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM USER_COMMAND .
  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.       "CREATE
      MOVE: C_CREATE TO G_MODE.
      REFRESH IT_FCODE.
      APPEND 'CHANGE' TO IT_FCODE.
    WHEN 'FC02'.       "DISPLAY
      IF P_ERNAM IS INITIAL.
        MESSAGE E999 WITH TEXT-M02.
      ENDIF.

      SELECT SINGLE * FROM USR01 WHERE BNAME = P_ERNAM.
      IF SY-SUBRC NE 0.
        MESSAGE E999 WITH TEXT-M03.
      ENDIF.

      MOVE: C_DISPLAY TO G_MODE.
      REFRESH IT_FCODE.
      APPEND 'SAVE' TO IT_FCODE.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FOR_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_FOR_CREATE .
  DATA: BEGIN OF LT_GRQTY OCCURS 0,
          SHKZG   LIKE   EKBE-SHKZG,
          WEMNG   LIKE   ZTMM_VAZ_VZ028-WEMNG,
        END   OF LT_GRQTY.

  DATA: BEGIN OF LT_0100 OCCURS 0.
          INCLUDE STRUCTURE ZTMM_VAZ_VZ028.
  DATA:   MAKTX      LIKE MAKT-MAKTX,
          NAME1      LIKE LFA1-NAME1,
          ELIKZ      LIKE EKPO-ELIKZ,
          REQ,                               "P/O Closing Request
          IFRESULT   LIKE  ZTMM_VAZ_028_LOG-IFRESULT,
          ZMSG(255),
          SUCCESS,
          CHANGED,
        END   OF LT_0100.


  SELECT A~EBELN B~EBELP B~MATNR D~MAKTX A~LIFNR C~NAME1 B~MENGE
         B~MEINS B~ELIKZ B~WERKS B~LGORT
    INTO CORRESPONDING FIELDS OF TABLE LT_0100
    FROM EKKO AS A INNER JOIN EKPO AS B
                      ON B~EBELN = A~EBELN
              LEFT OUTER JOIN LFA1 AS C
                      ON C~LIFNR = A~LIFNR
              LEFT OUTER JOIN MAKT AS D
                      ON D~MATNR = B~MATNR
                     AND D~SPRAS = 'E'
  WHERE A~EBELN IN S_EBELN
    AND A~BEDAT IN S_BEDAT
    AND A~LIFNR IN S_LIFNR
*    AND A~BSART IN ('NB','ZB')
    AND A~BSART ='ZB'
    AND B~WERKS EQ P_WERKS
    AND B~LGORT EQ P_LGORT
    AND B~MATNR IN S_MATNR
    AND NOT EXISTS ( SELECT * FROM ZTMM_VAZ_VZ028
                             WHERE EBELN = B~EBELN
                               AND EBELP = B~EBELP ).

  LOOP AT LT_0100.
    MOVE-CORRESPONDING LT_0100 TO IT_0100.
    APPEND IT_0100.
  ENDLOOP.

  " Read GR quantity
  LOOP AT IT_0100.
    REFRESH LT_GRQTY.

    SELECT SHKZG SUM( BPMNG )
      INTO TABLE LT_GRQTY
      FROM EKBE
     WHERE EBELN = IT_0100-EBELN
       AND EBELP = IT_0100-EBELP
       AND VGABE = '1'
     GROUP BY SHKZG.

    LOOP AT LT_GRQTY.
      CASE LT_GRQTY-SHKZG.
        WHEN 'S'.
          IT_0100-WEMNG = IT_0100-WEMNG + LT_GRQTY-WEMNG.
        WHEN 'H'.
          IT_0100-WEMNG = IT_0100-WEMNG - LT_GRQTY-WEMNG.
      ENDCASE.
    ENDLOOP.

    MODIFY IT_0100.
  ENDLOOP.
ENDFORM.                    " READ_DATA_FOR_CREATE
*&---------------------------------------------------------------------*
*&      Form  SAVE_0100_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_0100_RTN .
  PERFORM EVENT_TRIGGER USING 'DATA_CHANGED' '0100'.

  CASE G_MODE.
    WHEN C_CREATE.
      PERFORM SAVE_0100_CREATE.
    WHEN C_CHANGE.
      PERFORM SAVE_0100_CHANGE.
  ENDCASE.
ENDFORM.                    " SAVE_0100_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS_0100_CRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_0100_CRE .
  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                  'S' 'EBELN'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'EBELN',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'EBELP',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'MATNR',
                                  ' ' 'OUTPUTLEN'    '18',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'REF_TABLE'   'MAKT',
                                  ' ' 'REF_FIELD'   'MAKTX',
                                  ' ' 'OUTPUTLEN'    '20',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'LIFNR',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'NAME1'       ' ',
                                  ' ' 'REF_TABLE'   'LFA1',
                                  ' ' 'REF_FIELD'   'NAME1',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'MENGE'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'MENGE',
                                  ' ' 'COLTEXT'     'P/O Qty',
                                  ' ' 'QFIELDNAME'  'MEINS',
                                  ' ' 'OUTPUTLEN'    '8',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'MEINS'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'MEINS',
                                  ' ' 'OUTPUTLEN'    '4',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'WEMNG'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'WEMNG',
                                  ' ' 'COLTEXT'     'G/R Qty',
                                  ' ' 'QFIELDNAME'  'MEINS',
                                  ' ' 'OUTPUTLEN'    '8',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'REQ'         ' ',
                                  ' ' 'COLTEXT'     'P/O Closing Req',
                                  ' ' 'CHECKBOX'    'X',
                                  ' ' 'OUTPUTLEN'    '13',
                                  ' ' 'EDIT'         'X',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZREASON'     ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'ZREASON',
                                  ' ' 'EDIT'         'X',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'IFRESULT'    ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_VZ028',
                                  ' ' 'REF_FIELD'   'IFRESULT',
                                  ' ' 'OUTPUTLEN'    '10',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ELIKZ'       ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'ELIKZ',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ERNAM'       ' ',
                                  ' ' 'REF_TABLE'   'EKKO',
                                  ' ' 'REF_FIELD'   'ERNAM',
                                ' ' 'COLTEXT'     'Closing Req.User',
                                  ' ' 'OUTPUTLEN'    '15',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ERDAT'       ' ',
                                  ' ' 'REF_TABLE'   'EKKO',
                                  ' ' 'REF_FIELD'   'ERDAT',
                                ' ' 'COLTEXT'     'Closing Req.Date',
                                  ' ' 'OUTPUTLEN'    '15',
                                  'E' 'EMPHASIZE'    'C250',

                                  'S' 'ZMSG'  ' ',
                                  ' ' 'COLTEXT'      'Mssage',
                                  ' ' 'OUTPUTLEN'    '100',
                                  'E' 'EMPHASIZE'    'C250'.
ENDFORM.                    " SET_SCREEN_FIELDS_0100_CRE
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM DATA_CHANGED USING PR_DATA_CHANGED TYPE REF TO
                                        CL_ALV_CHANGED_DATA_PROTOCOL
                        P_UCOMM.
  DATA: LS_MOD_CELLS        TYPE LVC_S_MODI,
        LT_MOD_CELLS        TYPE LVC_T_MODI.

  DATA: L_LINES TYPE I.

  DATA: L_FIELDNAME TYPE STRING.
  FIELD-SYMBOLS: <LFS_FIELDNAME>.

  CLEAR: LS_MOD_CELLS, LT_MOD_CELLS.

  LT_MOD_CELLS = PR_DATA_CHANGED->MT_GOOD_CELLS.

  DESCRIBE TABLE LT_MOD_CELLS LINES L_LINES.
  CASE L_LINES.
    WHEN 0.
      EXIT.
    WHEN 1.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  SORT LT_MOD_CELLS BY ROW_ID.

  LOOP AT LT_MOD_CELLS INTO LS_MOD_CELLS.
    IF SY-TABIX EQ 1.
      READ TABLE IT_0100 INDEX LS_MOD_CELLS-ROW_ID.
      IF SY-SUBRC NE 0.
        EXIT.
      ENDIF.
    ENDIF.

    CONCATENATE 'IT_0100-' LS_MOD_CELLS-FIELDNAME
           INTO L_FIELDNAME.
    ASSIGN (L_FIELDNAME) TO <LFS_FIELDNAME>.

    MOVE: LS_MOD_CELLS-VALUE TO <LFS_FIELDNAME>.

*    CASE ls_mod_cells-fieldname.
*      WHEN 'CONF_CIF'.
*        PERFORM RESET_AMOUNT_FIELDS.
*      WHEN 'ZQUOIBAPR'.
*        PERFORM read_shop_name.
*      WHEN 'ZQUOLACO'.
*        PERFORM get_vendor.
*      WHEN 'ZQUOIMNGCH'.
*        PERFORM read_material_text.
*      WHEN 'STIND'.
*        PERFORM read_vendor_text.
*    ENDCASE.
  ENDLOOP.

  MOVE: 'X' TO IT_0100-CHANGED.

  MODIFY IT_0100 INDEX LS_MOD_CELLS-ROW_ID.
ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  EVENT_TRIGGER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2985   text
*      -->P_2986   text
*----------------------------------------------------------------------*
FORM EVENT_TRIGGER USING PV_EVENT PV_GRID.
  DATA : L_VALID(1),
         L_REFRESH(1) TYPE C VALUE 'X',
         L_GRID       TYPE STRING.

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'G_GRID_' PV_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CASE PV_EVENT.
    WHEN 'DATA_CHANGED'.
      CALL METHOD <LFS_GRID>->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

      CALL METHOD <LFS_GRID>->CHECK_CHANGED_DATA
        IMPORTING
          E_VALID   = L_VALID
        CHANGING
          C_REFRESH = L_REFRESH.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " EVENT_TRIGGER
*&---------------------------------------------------------------------*
*&      Form  CHANGE_0100_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_0100_RTN .
  REFRESH IT_FCODE.
  APPEND 'CHANGE' TO IT_FCODE.

  MOVE: C_CHANGE TO G_MODE.

  PERFORM BUILD_FIELD_CATALOG   USING '0100'.
  PERFORM ASSIGN_ITAB_TO_ALV    USING '0100'.

ENDFORM.                    " CHANGE_0100_RTN
*&---------------------------------------------------------------------*
*&      Form  SAVE_0100_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_0100_CREATE .
  REFRESH IT_9055.
  READ TABLE IT_0100 WITH KEY REQ     = 'X'
                              ZREASON = SPACE.
  IF SY-SUBRC EQ 0.
    MESSAGE S999 WITH TEXT-M09 IT_0100-EBELN IT_0100-EBELP.
    LEAVE TO SCREEN SY-DYNNR.
  ENDIF.

  LOOP AT IT_0100 WHERE REQ EQ 'X'.
    CLEAR: IT_9055.
    MOVE: IT_0100-EBELN   TO IT_9055-EBELN,
          IT_0100-EBELP   TO IT_9055-EBELP,
          IT_0100-WERKS   TO IT_9055-WERKS,
          IT_0100-LGORT   TO IT_9055-LGORT,
          IT_0100-MATNR   TO IT_9055-MATNR,
          IT_0100-LIFNR   TO IT_9055-LIFNR,
          IT_0100-MENGE   TO IT_9055-MENGE,
          IT_0100-MEINS   TO IT_9055-MEINS,
          IT_0100-WEMNG   TO IT_9055-WEMNG,
          IT_0100-ZREASON TO IT_9055-ZREASON,
          SY-UNAME        TO IT_9055-ERNAM,
          SY-DATUM        TO IT_9055-ERDAT,
          SY-UZEIT        TO IT_9055-ERZET,
          SY-UNAME        TO IT_9055-AENAM,
          SY-DATUM        TO IT_9055-AEDAT,
          SY-UZEIT        TO IT_9055-AEZET.
    APPEND IT_9055.
  ENDLOOP.

  INSERT ZTMM_VAZ_VZ028 FROM TABLE IT_9055 ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC NE 0.
    MESSAGE E009 WITH TEXT-M10.
  ENDIF.

  MESSAGE S009 WITH TEXT-M11.

  COMMIT WORK AND WAIT.

  MOVE: 'X'      TO IT_0100-SUCCESS,
        SY-UNAME TO IT_9055-ERNAM,
        SY-DATUM TO IT_9055-ERDAT,
        SY-UZEIT TO IT_9055-ERZET,
        SY-UNAME TO IT_9055-AENAM,
        SY-DATUM TO IT_9055-AEDAT,
        SY-UZEIT TO IT_9055-AEZET.

  MODIFY IT_0100 TRANSPORTING SUCCESS ERNAM ERDAT ERZET
                                      AENAM AEDAT AEZET
                        WHERE REQ EQ 'X'.
ENDFORM.                    " SAVE_0100_CREATE
*&---------------------------------------------------------------------*
*&      Form  SAVE_0100_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_0100_CHANGE .
  DATA: BEGIN OF LT_0100_TMP OCCURS 0.
          INCLUDE STRUCTURE IT_0100.
  DATA:   DELETED,
        END   OF LT_0100_TMP.

  REFRESH IT_9055.
  READ TABLE IT_0100 WITH KEY REQ     = 'X'
                              ZREASON = SPACE.
  IF SY-SUBRC EQ 0.
    MESSAGE S999 WITH TEXT-M09 IT_0100-EBELN IT_0100-EBELP.
    LEAVE TO SCREEN SY-DYNNR.
  ENDIF.

*  LOOP AT IT_0100 WHERE IFRESULT NE 'Z'
  LOOP AT IT_0100 WHERE IFRESULT NE 'S'
                    AND CHANGED  EQ 'X'.
    CASE IT_0100-REQ.
      WHEN SPACE.
        SELECT SINGLE * FROM ZTMM_VAZ_VZ028 WHERE EBELN   =
IT_0100-EBELN
                                           AND EBELP   = IT_0100-EBELP
*                                           AND ZIF_IND = 'Z'.
                                           AND ZIF_IND = 'S'.
        IF SY-SUBRC EQ 0.
          ROLLBACK WORK.
          MESSAGE S999 WITH TEXT-M12 IT_0100-EBELN IT_0100-EBELP.
          LEAVE TO SCREEN SY-DYNNR.
        ENDIF.

        DELETE FROM ZTMM_VAZ_VZ028 WHERE EBELN = IT_0100-EBELN
                                  AND EBELP = IT_0100-EBELP.

        MOVE-CORRESPONDING IT_0100 TO LT_0100_TMP.
        MOVE: 'X'     TO LT_0100_TMP-DELETED.
        APPEND LT_0100_TMP.
      WHEN 'X'.
        UPDATE ZTMM_VAZ_VZ028
        SET: ZREASON = IT_0100-ZREASON
               AENAM   = SY-UNAME
               AEDAT   = SY-DATUM
               AEZET   = SY-UZEIT
         WHERE EBELN = IT_0100-EBELN
           AND EBELP = IT_0100-EBELP.
        IF SY-SUBRC NE 0.
          MESSAGE S999 WITH TEXT-M13 IT_0100-EBELN IT_0100-EBELP.
          LEAVE TO SCREEN SY-DYNNR.
        ENDIF.

        CLEAR: IT_9055.
        MOVE-CORRESPONDING IT_0100 TO LT_0100_TMP.
        MOVE: SPACE           TO LT_0100_TMP-CHANGED,
              SY-UNAME        TO LT_0100_TMP-AENAM,
              SY-DATUM        TO LT_0100_TMP-AEDAT,
              SY-UZEIT        TO LT_0100_TMP-AEZET.
        APPEND LT_0100_TMP.
    ENDCASE.
  ENDLOOP.

  COMMIT WORK AND WAIT.

  MESSAGE S009 WITH TEXT-M11.

  LOOP AT LT_0100_TMP.
    CASE LT_0100_TMP-DELETED.
      WHEN 'X'.
        DELETE IT_0100 WHERE EBELN = LT_0100_TMP-EBELN
                         AND EBELP = LT_0100_TMP-EBELP.
      WHEN SPACE.
        MOVE: SPACE TO IT_0100-CHANGED.
        MODIFY IT_0100 TRANSPORTING CHANGED
                              WHERE EBELN = LT_0100_TMP-EBELN
                                AND EBELP = LT_0100_TMP-EBELP.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " SAVE_0100_CHANGE
*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FOR_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_FOR_DISPLAY .
  DATA: BEGIN OF LT_0100 OCCURS 0.
          INCLUDE STRUCTURE ZTMM_VAZ_VZ028.
  DATA:   MAKTX      LIKE MAKT-MAKTX,
          NAME1      LIKE LFA1-NAME1,
          ELIKZ      LIKE EKPO-ELIKZ,
          REQ,                               "P/O Closing Request
          IFRESULT   LIKE  ZTMM_VAZ_028_LOG-IFRESULT,
          ZMSG(255),
          SUCCESS,
          CHANGED,
        END   OF LT_0100.



  REFRESH: IT_0100.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_0100
    FROM ZTMM_VAZ_VZ028 AS A INNER JOIN EKKO AS B
                             ON B~EBELN = A~EBELN
                          INNER JOIN EKPO AS C
                             ON C~EBELN = A~EBELN
                            AND C~EBELP = A~EBELP
   WHERE A~WERKS EQ P_WERKS
     AND A~LGORT EQ P_LGORT
     AND A~ERNAM EQ P_ERNAM
     AND A~ERDAT IN S_ERDAT
     AND A~EBELN IN S_EBELN
     AND A~LIFNR IN S_LIFNR
     AND A~MATNR IN S_MATNR
     AND B~BEDAT IN S_BEDAT.

  LOOP AT LT_0100.
    MOVE-CORRESPONDING LT_0100 TO IT_0100.
    APPEND IT_0100.
  ENDLOOP.


  MOVE: 'X' TO IT_0100-REQ.
  MODIFY IT_0100 TRANSPORTING REQ WHERE EBELN >= SPACE.
ENDFORM.                    " READ_DATA_FOR_DISPLAY
