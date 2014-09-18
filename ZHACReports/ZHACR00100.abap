*&--------------------------------------------------------------------------&*
*& ####  NO    : AC-M001                                                 &*
*& ####     : PGM ID# ### ### ## ## - ZACTFITB4            &*
*& ####       : On-Line                                                 &*
*& ####  NO    : 24.12.03.01                                             &*
*& #####     : PGM ID# ### ### ## ##(##)                  &*
*& ####  ID    : ZHARC00100                                              &*
*& T-CODE         : ZHARC00100                                              &*
*& PACKAGE        : ZHAC                                                    &*
*& ###       : 2013.02.25                                              &*
*& ###         : HMCDVM3                                                 &*
*& #### ##   : #### #### ### #### #####.           &*
*& ####### : 1. ##### ### #### ### ####.
*            2. # ### ## # ### # ###, ## ## ##&*
*&--------------------------------------------------------------------------&*
*& ## SEQ : #                                                             &*
*& # # # :                                                               &*
*& #### :                                                               &*
*& #### :                                                                &*
*&--------------------------------------------------------------------------&*
REPORT ZHACR00100 MESSAGE-ID EU.

TYPE-POOLS:
  SLIS,
  SSCR.

TABLES:
  SEOCLASSTX,
  TADIR,
  TLIBT,
  D020S,
  D010INC,
  TRDIR.

CLASS:
  LCL_SOURCE_SCAN DEFINITION DEFERRED.

DATA:
  LO_SSCAN   TYPE REF TO LCL_SOURCE_SCAN,
  LV_SSTRING TYPE TEXT255,
  LV_APPL    TYPE TAPLT-APPL.

DATA: BEGIN OF GS_STR.
DATA: FLD01 TYPE STRING,
      FLD02 TYPE STRING,
      FLD03 TYPE STRING,
      FLD04 TYPE STRING,
      FLD05 TYPE STRING,
      FLD06 TYPE STRING,
      FLD07 TYPE STRING,
      FLD08 TYPE STRING,
      FLD09 TYPE STRING,
      FLD10 TYPE STRING,
      FLD11 TYPE STRING,
      FLD12 TYPE STRING,
      FLD13 TYPE STRING,
      FLD14 TYPE STRING,
      FLD15 TYPE STRING,
      FLD16 TYPE STRING,
      FLD17 TYPE STRING,
      FLD18 TYPE STRING,
      FLD19 TYPE STRING,
      FLD20 TYPE STRING,
      FLD21 TYPE STRING,
      FLD22 TYPE STRING,
      FLD23 TYPE STRING,
      FLD24 TYPE STRING,
      FLD25 TYPE STRING,
      FLD26 TYPE STRING,
      FLD27 TYPE STRING,
      FLD28 TYPE STRING,
      FLD29 TYPE STRING,
      FLD30 TYPE STRING,
      FLD31 TYPE STRING,
      FLD32 TYPE STRING,
      FLD33 TYPE STRING,
      FLD34 TYPE STRING,
      FLD35 TYPE STRING,
      FLD36 TYPE STRING,
      FLD37 TYPE STRING,
      FLD38 TYPE STRING,
      FLD39 TYPE STRING,
      FLD40 TYPE STRING,
      FLD41 TYPE STRING,
      FLD42 TYPE STRING,
      FLD43 TYPE STRING,
      FLD44 TYPE STRING,
      FLD45 TYPE STRING,
      FLD46 TYPE STRING,
      FLD47 TYPE STRING,
      FLD48 TYPE STRING,
      FLD49 TYPE STRING,
      FLD50 TYPE STRING,
      FLD51 TYPE STRING,
      FLD52 TYPE STRING,
      FLD53 TYPE STRING,
      FLD54 TYPE STRING,
      FLD55 TYPE STRING,
      FLD56 TYPE STRING,
      FLD57 TYPE STRING,
      FLD58 TYPE STRING,
      FLD59 TYPE STRING,
      FLD60 TYPE STRING,
      FLD61 TYPE STRING,
      FLD62 TYPE STRING,
      FLD63 TYPE STRING,
      FLD64 TYPE STRING,
      FLD65 TYPE STRING,
      FLD66 TYPE STRING,
      FLD67 TYPE STRING,
      FLD68 TYPE STRING,
      FLD69 TYPE STRING,
      FLD70 TYPE STRING,
      FLD71 TYPE STRING,
      FLD72 TYPE STRING,
      FLD73 TYPE STRING,
      FLD74 TYPE STRING,
      FLD75 TYPE STRING.
DATA END OF GS_STR.

DATA: G_NUMC2   TYPE NUMC2.
DATA: G_FIELD   TYPE CHAR30.
DATA: G_TABNAME TYPE TABNAME.
DATA: G_SW1.

FIELD-SYMBOLS:  <FS_FIELD1>  TYPE ANY.

DATA: GT_PGML LIKE ZACTPGML3 OCCURS 0 WITH HEADER LINE.
DATA: GT_STR2 LIKE AIND_STR2 OCCURS 0 WITH HEADER LINE.
DATA: GT_FITB LIKE ZACTFITB4 OCCURS 0 WITH HEADER LINE.
DATA: GS_FITB LIKE ZACTFITB4.
DATA: GT_D010INC LIKE D010INC OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN  BEGIN OF BLOCK: A05 WITH FRAME TITLE A05.
SELECT-OPTIONS    SSTRING     FOR LV_SSTRING NO INTERVALS.
PARAMETERS        P_REGEX     TYPE XFELD NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK A05,

                  BEGIN OF BLOCK A10 WITH FRAME TITLE A10.
SELECT-OPTIONS:   REPNAME  FOR TRDIR-NAME MEMORY ID RS_SCAN_REPID,
                  DYNNR    FOR D020S-DNUM,
                  SUBC     FOR TRDIR-SUBC,
                  APPL     FOR LV_APPL,
                  CNAM     FOR TRDIR-CNAM MATCHCODE OBJECT USER_ADDR,
                  UNAM     FOR TRDIR-UNAM MATCHCODE OBJECT USER_ADDR.
SELECTION-SCREEN: END OF BLOCK A10,

                  BEGIN OF BLOCK A11 WITH FRAME TITLE A11.
SELECT-OPTIONS    DEVCLASS FOR TADIR-DEVCLASS.
SELECTION-SCREEN: END OF BLOCK A11,

                  BEGIN OF BLOCK A12 WITH FRAME TITLE A12.
SELECT-OPTIONS:   FUNCGRP  FOR TLIBT-AREA NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK A12,

                  BEGIN OF BLOCK A13 WITH FRAME TITLE A13.
SELECT-OPTIONS:   P_CLASS  FOR SEOCLASSTX-CLSNAME  NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK A13,

                  BEGIN OF BLOCK A20 WITH FRAME TITLE A20.
PARAMETERS:       PLUSMINU(2) TYPE N DEFAULT 2,
                  INCLU       TYPE XFELD AS CHECKBOX DEFAULT 'X',
                  MODIASS     TYPE XFELD AS CHECKBOX,
                  COMMENT     TYPE XFELD AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK A20,

                  BEGIN OF BLOCK A30 WITH FRAME TITLE A30.
PARAMETERS:       RB_CODE  RADIOBUTTON GROUP R10,
                  RB_DYN   RADIOBUTTON GROUP R10,
                  RB_ALL   RADIOBUTTON GROUP R10,
                  P_VERS   TYPE XFELD AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK A30.

*----------------------------------------------------------------------*
*       CLASS lcx_scan_exceptions DEFINITION
*----------------------------------------------------------------------*
*       Exceptions for source scanning
*----------------------------------------------------------------------*
CLASS LCX_SCAN_EXCEPTIONS DEFINITION INHERITING FROM CX_STATIC_CHECK.
ENDCLASS.                    "lcx_scan_exceptions DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_source_scan DEFINITION
*----------------------------------------------------------------------*
*       ABAP source scanner
*----------------------------------------------------------------------*
CLASS LCL_SOURCE_SCAN DEFINITION.
  PUBLIC SECTION.
    METHODS:
      CONSTRUCTOR,

      F4_CLASS
        CHANGING
          CV_CLASS_NAME TYPE CLIKE,

      F4_FUNCTION_GROUP
        IMPORTING
          IV_GROUP_NAME TYPE CLIKE,

      F4_REPNAME
        CHANGING
          CV_REPNAME TYPE CLIKE,

      START.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF TY_DYNPRO,
        REPNAME LIKE D020S-PROG,
        DYNNR  LIKE D020S-DNUM,
      END OF TY_DYNPRO.

    TYPES:
      BEGIN OF TY_LS_OBJNAME,
        REPORT TYPE SY-REPID,
        DYNNR  TYPE SY-DYNNR,
      END OF TY_LS_OBJNAME.

    DATA:
      GO_ALV        TYPE REF TO CL_SALV_HIERSEQ_TABLE,
      GV_HIT_COUNT  TYPE I,
      GV_SSTRING    TYPE STRING,
      GV_DYNP_FOUND TYPE XFELD,
      GV_VERS_FOUND TYPE XFELD,
      GT_DYNPRO     TYPE STANDARD TABLE OF TY_DYNPRO,
      GT_OBJECT     TYPE STANDARD TABLE OF TADIR-OBJ_NAME,
      GT_VRSD       TYPE HASHED TABLE OF VRSD
                      WITH UNIQUE KEY OBJNAME VERSNO,

    BEGIN OF GS_ALV_HEADER,
      REPNAME TYPE TADIR-OBJ_NAME,
      DYNNR   TYPE SY-DYNNR,
      EXPAND  TYPE XFELD,
      VERSNO  TYPE VRSD-VERSNO,
    END OF GS_ALV_HEADER,

    GT_ALV_HEADER LIKE STANDARD TABLE OF GS_ALV_HEADER,

    BEGIN OF GS_ALV_ITEM,
        REPNAME    TYPE SY-REPID,
        DYNNR      TYPE SY-DYNNR,
        VERSNO     TYPE VRSD-VERSNO,
        LINE_NO    TYPE RSROW,
        TEXT       TYPE TEXT255,
        HIT        TYPE XFELD,
        CELL_COLOR TYPE LVC_T_SCOL,
     END OF GS_ALV_ITEM,

     GT_ALV_ITEM LIKE STANDARD TABLE OF GS_ALV_ITEM.

    CONSTANTS:
      GC_X TYPE XFELD VALUE 'X'.

    METHODS:
      ADD_TO_HITLIST
        IMPORTING
          IV_REPORT      TYPE CLIKE
          IV_DYNPRO      TYPE CLIKE OPTIONAL
          IV_SOURCE_LINE TYPE CLIKE
          IV_TABIX       TYPE SY-TABIX
          IV_HIT         TYPE XFELD
          IV_VERSNO      TYPE VRSD-VERSNO,

      CALL_ABAP_EDITOR
        IMPORTING
          IS_ALV_ITEM LIKE GS_ALV_ITEM,

      CALL_SCREEN_PAINTER
        IMPORTING
          IS_ALV_ITEM LIKE GS_ALV_ITEM,

      DISPLAY,

      DISPLAY_ABAP_VERSION
        IMPORTING
          IS_ALV_ITEM LIKE GS_ALV_ITEM,

      DISPLAY_SCREEN_PAINTER_VERSION
        IMPORTING
          IS_ALV_ITEM LIKE GS_ALV_ITEM,

      DISPLAY_VERSION_MANAGEMENT
        IMPORTING
          IS_ALV_HEADER LIKE GS_ALV_HEADER,

      GET_ALV_INSTANCE,
      GET_DYNPRO_FLOW_LOGIC
        IMPORTING
          IV_REPORT TYPE CLIKE
          IV_DYNPRO TYPE CLIKE
        RETURNING VALUE(RT_DFLOW) TYPE ABAPTXT255_TAB,

      GET_HIT_SET
        IMPORTING
          IV_REPORT      TYPE CLIKE
          IV_DYNPRO      TYPE CLIKE OPTIONAL
          IT_ABAP        TYPE ABAPTXT255_TAB
          IV_TABIX       TYPE SY-TABIX
          IV_VERSNO      TYPE VRSD-VERSNO,

      GET_VERSION_NUMBERS
        IMPORTING
          IV_REPORT TYPE CLIKE
          IV_DYNPRO TYPE CLIKE OPTIONAL
        RETURNING VALUE(RT_VRSD) LIKE GT_VRSD,

      GET_DYNPROS,
      GET_SOURCE_NAMES,

      GET_SOURCE_BY_VERSION
        IMPORTING
          IV_REPORT TYPE CLIKE
          IV_DYNPRO TYPE CLIKE OPTIONAL
          IV_VERSNO TYPE VRSD-VERSNO
        RETURNING VALUE(RT_ABAP) TYPE ABAPTXT255_TAB,

      GET_REPORT_NAMES,
      GET_FUNCTION_NAMES,
      GET_CLASS_NAMES,
      GET_INCLUDES,

      GET_METHOD_INCLUDES
        IMPORTING
          IV_CLASS_NAME TYPE CLIKE,

      SEARCH_ABAP_SOURCE   RAISING LCX_SCAN_EXCEPTIONS,
      SEARCH_DYNPRO_SOURCE RAISING LCX_SCAN_EXCEPTIONS,

      SEARCH_SOURCE
        IMPORTING
          IT_SOURCE TYPE ABAPTXT255_TAB
          IV_REPORT TYPE CLIKE
          IV_DYNPRO TYPE CLIKE OPTIONAL
        RAISING LCX_SCAN_EXCEPTIONS,

      SET_ALV_ATTRIBUTES,

      ON_LINK_CLICK
          FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_HIERSEQ
            IMPORTING
                SENDER
                LEVEL
                ROW
                COLUMN.

ENDCLASS.                    "lcl_source_scan DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_source_scan IMPLEMENTATION
*----------------------------------------------------------------------*
*       ABAP source scanner
*----------------------------------------------------------------------*
CLASS LCL_SOURCE_SCAN IMPLEMENTATION.
  METHOD DISPLAY_SCREEN_PAINTER_VERSION.
    DATA:
      LV_OBJECT_NAME TYPE VERSOBJNAM,
      LS_INFOLNA     TYPE VRSINFOLNA,
      LS_INFOLNB     TYPE VRSINFOLNB,
      LS_VRSD        LIKE LINE OF GT_VRSD,
      LS_OBJECT_NAME TYPE TY_LS_OBJNAME.

    LS_OBJECT_NAME-REPORT = IS_ALV_ITEM-REPNAME.
    LS_OBJECT_NAME-DYNNR  = IS_ALV_ITEM-DYNNR.
    LV_OBJECT_NAME        = LS_OBJECT_NAME.

    READ TABLE GT_VRSD WITH TABLE KEY OBJNAME = LV_OBJECT_NAME
                                      VERSNO  = IS_ALV_ITEM-VERSNO
                                      INTO LS_VRSD.

    CHECK SY-SUBRC IS INITIAL.

    LS_INFOLNA = LV_OBJECT_NAME.
    MOVE-CORRESPONDING LS_VRSD TO LS_INFOLNB.

    CALL FUNCTION 'RS_SCRP_SHOW_VERS'
      EXPORTING
        INFOLNA    = LS_INFOLNA
        INFOLNB    = LS_INFOLNB
        OBJNAME    = LV_OBJECT_NAME
        VERSNO     = IS_ALV_ITEM-VERSNO
      EXCEPTIONS
        NO_VERSION = 1
        CANCELLED  = 2
        OTHERS     = 3.

  ENDMETHOD.                    "display_screen_painter_version

  METHOD DISPLAY_ABAP_VERSION.
    DATA:
      LT_TRDIR       TYPE STANDARD TABLE OF TRDIR,
      LV_OBJECT_NAME TYPE VERSOBJNAM,
      LV_TITLE       TYPE SY-TITLE,
      LT_ABAP        TYPE ABAPTXT255_TAB.

    LV_OBJECT_NAME = IS_ALV_ITEM-REPNAME.

*   Display report version
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        OBJECT_NAME                  = LV_OBJECT_NAME
        OBJECT_TYPE                  = 'REPS'
        VERSNO                       = IS_ALV_ITEM-VERSNO
        IV_NO_RELEASE_TRANSFORMATION = GC_X
      TABLES
        REPOS_TAB                    = LT_ABAP
        TRDIR_TAB                    = LT_TRDIR
      EXCEPTIONS
        NO_VERSION                   = 1
        OTHERS                       = 2.

    CHECK SY-SUBRC IS INITIAL.

    CONCATENATE 'Programm:'(004)
                IS_ALV_ITEM-REPNAME
                'Version'(005)
                 IS_ALV_ITEM-VERSNO
                 INTO LV_TITLE SEPARATED BY SPACE.

    EDITOR-CALL FOR LT_ABAP TITLE LV_TITLE DISPLAY-MODE.

  ENDMETHOD.                    "display_abap_version

  METHOD CALL_SCREEN_PAINTER.
    CALL FUNCTION 'RS_SCRP'
      EXPORTING
        ABL_LINE       = IS_ALV_ITEM-LINE_NO
        DYNNR          = IS_ALV_ITEM-DYNNR
        PROGNAME       = IS_ALV_ITEM-REPNAME
        WANTED_MODE    = 'SHOW'
      EXCEPTIONS
        ALREADY_EXISTS = 1
        NOT_FOUND      = 2
        NOT_EXECUTED   = 3
        OTHERS         = 4.

  ENDMETHOD.                    "call_screen_painter

  METHOD CALL_ABAP_EDITOR.
    CALL FUNCTION 'EDITOR_PROGRAM'
      EXPORTING
        APPID   = 'PG'
        DISPLAY = GC_X
        PROGRAM = IS_ALV_ITEM-REPNAME
        LINE    = IS_ALV_ITEM-LINE_NO
        TOPLINE = IS_ALV_ITEM-LINE_NO
      EXCEPTIONS
        OTHERS  = 1.

  ENDMETHOD.                    "call_abap_editor

  METHOD DISPLAY_VERSION_MANAGEMENT.
    IF IS_ALV_HEADER-DYNNR IS INITIAL.
*     call version management for programs
      CALL FUNCTION 'RS_PROGRAM_VERSIONS'
        EXPORTING
          PROGNAME         = IS_ALV_HEADER-REPNAME
        EXCEPTIONS
          FUNCTION_INCLUDE = 1
          OTHERS           = 2.
    ELSE.
      CALL FUNCTION 'RS_SCRP_VERSION'
        EXPORTING
          DYNNR     = IS_ALV_HEADER-DYNNR
          PROGNAME  = IS_ALV_HEADER-REPNAME
          NO_UPDATE = GC_X.
    ENDIF.
  ENDMETHOD.                    "display_version_management

  METHOD CONSTRUCTOR.
    DATA:
      LS_RESTRICT    TYPE SSCR_RESTRICT,
      LS_OPT_LIST    TYPE SSCR_OPT_LIST,
      LS_ASSOCIATION TYPE SSCR_ASS.

    LS_OPT_LIST-NAME       = 'RESTRICT'.
    LS_OPT_LIST-OPTIONS-CP = GC_X.
    LS_OPT_LIST-OPTIONS-EQ = GC_X.

    APPEND LS_OPT_LIST TO LS_RESTRICT-OPT_LIST_TAB.

    LS_ASSOCIATION-KIND    = 'S'.
    LS_ASSOCIATION-NAME    = 'SSTRING'.
    LS_ASSOCIATION-SG_MAIN = 'I'.
    LS_ASSOCIATION-OP_MAIN = LS_ASSOCIATION-OP_ADDY = 'RESTRICT'.

    APPEND LS_ASSOCIATION TO LS_RESTRICT-ASS_TAB.

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        PROGRAM                = SY-REPID
        RESTRICTION            = LS_RESTRICT
      EXCEPTIONS
        TOO_LATE               = 1
        REPEATED               = 2
        SELOPT_WITHOUT_OPTIONS = 3
        SELOPT_WITHOUT_SIGNS   = 4
        INVALID_SIGN           = 5
        EMPTY_OPTION_LIST      = 6
        INVALID_KIND           = 7
        REPEATED_KIND_A        = 8
        OTHERS                 = 9.

  ENDMETHOD.                    "constructor

  METHOD GET_DYNPRO_FLOW_LOGIC.
    DATA: LS_DHEAD  TYPE D020S,
          LT_DFIELD TYPE STANDARD TABLE OF D021S,
          LT_DFLOW  TYPE STANDARD TABLE OF D022S,
          LT_DMATCH TYPE STANDARD TABLE OF D023S,

          BEGIN OF LS_DYNP_ID,
            PROG TYPE D020S-PROG,
            DNUM TYPE D020S-DNUM,
         END OF LS_DYNP_ID.

    LS_DYNP_ID-PROG = IV_REPORT.
    LS_DYNP_ID-DNUM = IV_DYNPRO.

    IMPORT DYNPRO LS_DHEAD LT_DFIELD LT_DFLOW LT_DMATCH ID LS_DYNP_ID.

    RT_DFLOW = LT_DFLOW.
  ENDMETHOD.                    "get_dynpro_flow_logic

  METHOD ON_LINK_CLICK.
    DATA:
      LS_ALV_HEADER  LIKE LINE OF GT_ALV_HEADER,
      LS_ALV_ITEM    LIKE LINE OF GT_ALV_ITEM.

    CASE LEVEL.
      WHEN '1'.
        READ TABLE GT_ALV_HEADER INDEX ROW INTO LS_ALV_HEADER.
        CHECK SY-SUBRC IS INITIAL.

        DISPLAY_VERSION_MANAGEMENT( LS_ALV_HEADER ).

      WHEN '2'.
        READ TABLE GT_ALV_ITEM INDEX ROW INTO LS_ALV_ITEM.
        CHECK SY-SUBRC IS INITIAL.

        IF LS_ALV_ITEM-DYNNR IS INITIAL.
          IF LS_ALV_ITEM-VERSNO IS INITIAL.
            CALL_ABAP_EDITOR( LS_ALV_ITEM ).
          ELSE.
            DISPLAY_ABAP_VERSION( LS_ALV_ITEM ).
          ENDIF.

          SET PARAMETER ID 'RID' FIELD SY-REPID.
        ELSE.
*         Call screen painter
          IF LS_ALV_ITEM-VERSNO IS INITIAL.
            CALL_SCREEN_PAINTER( LS_ALV_ITEM ).
          ELSE.
            DISPLAY_SCREEN_PAINTER_VERSION( LS_ALV_ITEM ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_link_click

  METHOD SET_ALV_ATTRIBUTES.
    DATA:
      LO_LAYOUT    TYPE REF TO CL_SALV_LAYOUT,
      LO_EVENTS    TYPE REF TO CL_SALV_EVENTS_HIERSEQ,
      LO_FUNCTIONS TYPE REF TO CL_SALV_FUNCTIONS_LIST,
      LO_LEVEL     TYPE REF TO CL_SALV_HIERSEQ_LEVEL,
      LO_COLUMN    TYPE REF TO CL_SALV_COLUMN_HIERSEQ,
      LO_COLUMNS   TYPE REF TO CL_SALV_COLUMNS_HIERSEQ,
      LT_COLUMNS   TYPE SALV_T_COLUMN_REF,
      LS_COLUMNS   LIKE LINE OF LT_COLUMNS,
      LO_SETTINGS  TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      LV_TITLE     TYPE LVC_TITLE,
      LV_HITS      TYPE LVC_TITLE,
      LS_COLOR     TYPE LVC_S_COLO,
      LS_LAYOUT    TYPE SALV_S_LAYOUT_KEY,
      LT_FUNCTIONS TYPE SALV_T_UI_FUNC.

*   Layout
    LS_LAYOUT-REPORT = SY-REPID.
    LS_LAYOUT-HANDLE = 'SCAN'.

    LO_LAYOUT = GO_ALV->GET_LAYOUT( ).
    LO_LAYOUT->SET_KEY( LS_LAYOUT ).
    LO_LAYOUT->SET_SAVE_RESTRICTION( ).

*   Function keys/buttons
    LO_FUNCTIONS = GO_ALV->GET_FUNCTIONS( ).
    LO_FUNCTIONS->SET_ALL( GC_X ).

*   exclude the following functions (column paging buttons)
    LT_FUNCTIONS = LO_FUNCTIONS->GET_FUNCTIONS( ).

    TRY.
*    lo_functions->remove_function( '&CRB' ).
*    lo_functions->remove_function( '&CRL' ).
*    lo_functions->remove_function( '&CRR' ).
*    lo_functions->remove_function( '&CRE' ).
      CATCH CX_SALV_WRONG_CALL
            CX_SALV_NOT_FOUND.
    ENDTRY.
*   Display settings
    LO_SETTINGS = GO_ALV->GET_DISPLAY_SETTINGS( ).

*   Title
    LV_HITS = GV_HIT_COUNT.
    SHIFT LV_HITS LEFT DELETING LEADING SPACE.

    CONCATENATE LV_HITS
                'Treffer'(001)
                INTO LV_HITS SEPARATED BY SPACE.

    LV_TITLE = 'Source Scan für String:'(002).

    CONCATENATE LV_TITLE
                GV_SSTRING
                INTO LV_TITLE SEPARATED BY SPACE.

    CONCATENATE LV_TITLE
                LV_HITS
                INTO LV_TITLE SEPARATED BY ' - '.

    LO_SETTINGS->SET_LIST_HEADER( LV_TITLE ).

*   Event handling
    LO_EVENTS = GO_ALV->GET_EVENT( ).
    SET HANDLER ON_LINK_CLICK FOR LO_EVENTS.

*   Field catalog
    TRY.
*       Field catalog/columns - header table
        LO_COLUMNS  = GO_ALV->GET_COLUMNS( '1' ).
        LT_COLUMNS = LO_COLUMNS->GET( ).

        TRY.
            LO_COLUMNS->SET_EXPAND_COLUMN( 'EXPAND' ).

            LO_LEVEL = GO_ALV->GET_LEVEL( '1' ).
            LO_LEVEL->SET_ITEMS_EXPANDED( GC_X ).

          CATCH CX_SALV_DATA_ERROR.
        ENDTRY.

        LOOP AT LT_COLUMNS INTO LS_COLUMNS.
          CASE LS_COLUMNS-COLUMNNAME.
            WHEN 'EXPAND'.
              LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).

            WHEN 'DYNNR'.
              IF GV_DYNP_FOUND IS INITIAL.
                LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).
              ELSE.
                LS_COLUMNS-R_COLUMN->SET_OUTPUT_LENGTH( '15' ).
              ENDIF.

            WHEN 'VERSNO'.
              IF GV_VERS_FOUND IS INITIAL.
                LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).
              ELSE.
                LS_COLUMNS-R_COLUMN->SET_LEADING_ZERO( GC_X ).
                LS_COLUMNS-R_COLUMN->SET_OUTPUT_LENGTH( '15' ).
                TRY.
                    LO_COLUMN ?= LS_COLUMNS-R_COLUMN.
                    LO_COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
                  CATCH CX_SY_MOVE_CAST_ERROR.
                ENDTRY.
              ENDIF.
          ENDCASE.
        ENDLOOP.

*       Field catalog/columns - item table
        LO_COLUMNS = GO_ALV->GET_COLUMNS( '2' ).

        TRY.
            LO_COLUMNS->SET_COLOR_COLUMN( 'CELL_COLOR' ).
          CATCH CX_SALV_DATA_ERROR.
        ENDTRY.

        LT_COLUMNS = LO_COLUMNS->GET( ).

        LOOP AT LT_COLUMNS INTO LS_COLUMNS.
          CASE LS_COLUMNS-COLUMNNAME.
            WHEN 'REPNAME'.
              LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).

            WHEN 'DYNNR'.
              LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).

            WHEN 'VERSNO'.
              LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).

            WHEN 'CELL_COLOR'.
              LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).

            WHEN 'HIT'.
              LS_COLUMNS-R_COLUMN->SET_TECHNICAL( ).

            WHEN 'LINE_NO'.
              LS_COLOR-COL = '4'.
              TRY.
                  LO_COLUMN ?= LS_COLUMNS-R_COLUMN.
                  LO_COLUMN->SET_COLOR( LS_COLOR ).
                  LO_COLUMN->SET_LEADING_ZERO( GC_X ).
                CATCH CX_SY_MOVE_CAST_ERROR.
              ENDTRY.

            WHEN 'TEXT'.
              TRY.
                  LO_COLUMN ?= LS_COLUMNS-R_COLUMN.
                  LO_COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
                CATCH CX_SY_MOVE_CAST_ERROR.
              ENDTRY.

          ENDCASE.
        ENDLOOP.
      CATCH CX_SALV_NOT_FOUND.
    ENDTRY.

  ENDMETHOD.                    "set_alv_attributes

  METHOD GET_ALV_INSTANCE.
    DATA:
      LT_ALV_BIND TYPE SALV_T_HIERSEQ_BINDING,
      LS_ALV_BIND LIKE LINE OF LT_ALV_BIND.

    LS_ALV_BIND-MASTER = LS_ALV_BIND-SLAVE = 'REPNAME'.
    APPEND LS_ALV_BIND TO LT_ALV_BIND.

    LS_ALV_BIND-MASTER = LS_ALV_BIND-SLAVE = 'DYNNR'.
    APPEND LS_ALV_BIND TO LT_ALV_BIND.

    LS_ALV_BIND-MASTER = LS_ALV_BIND-SLAVE = 'VERSNO'.
    APPEND LS_ALV_BIND TO LT_ALV_BIND.

    TRY.
        CALL METHOD CL_SALV_HIERSEQ_TABLE=>FACTORY
          EXPORTING
            T_BINDING_LEVEL1_LEVEL2 = LT_ALV_BIND
          IMPORTING
            R_HIERSEQ               = GO_ALV
          CHANGING
            T_TABLE_LEVEL1          = GT_ALV_HEADER
            T_TABLE_LEVEL2          = GT_ALV_ITEM.

      CATCH CX_SALV_DATA_ERROR.
      CATCH CX_SALV_NOT_FOUND.
    ENDTRY.

  ENDMETHOD.                    "get_alv_instance

  METHOD F4_REPNAME.
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        OBJECT_TYPE          = 'PROG'
        OBJECT_NAME          = CV_REPNAME
        SUPPRESS_SELECTION   = 'X'
      IMPORTING
        OBJECT_NAME_SELECTED = CV_REPNAME
      EXCEPTIONS
        CANCEL               = 1.
  ENDMETHOD.                                                "f4_repname

  METHOD F4_FUNCTION_GROUP.
    DATA:
      LV_FNAME TYPE DYNFNAM.

    LV_FNAME = IV_GROUP_NAME.

    CALL FUNCTION 'RS_HELP_HANDLING'
      EXPORTING
        DYNPFIELD                 = LV_FNAME
        DYNPNAME                  = SY-DYNNR
        OBJECT                    = 'FG  '
        PROGNAME                  = SY-REPID
        SUPPRESS_SELECTION_SCREEN = 'X'.

  ENDMETHOD.                    "f4_function_group

  METHOD F4_CLASS.
    CALL FUNCTION 'F4_DD_ALLTYPES'
      EXPORTING
        OBJECT               = CV_CLASS_NAME
        SUPPRESS_SELECTION   = GC_X
        DISPLAY_ONLY         = SPACE
        ONLY_TYPES_FOR_CLIFS = GC_X
      IMPORTING
        RESULT               = CV_CLASS_NAME.
  ENDMETHOD.                                                "f4_class

  METHOD DISPLAY.
    FIELD-SYMBOLS:
      <LV_OBJ> LIKE LINE OF GT_OBJECT.

    IF GV_HIT_COUNT IS INITIAL.
      MESSAGE S326 WITH GV_SSTRING.
      RETURN.
    ENDIF.

    SORT GT_ALV_ITEM BY REPNAME DYNNR VERSNO LINE_NO HIT DESCENDING.
    DELETE ADJACENT DUPLICATES FROM GT_ALV_ITEM COMPARING REPNAME DYNNR VERSNO LINE_NO.


    REFRESH: GT_FITB.
    CLEAR:  GT_FITB, GS_FITB.

    LOOP AT GT_ALV_ITEM INTO GS_ALV_ITEM.
      CLEAR: GS_STR.

*     ##### ### ##
      CHECK GS_ALV_ITEM-TEXT(1) NE '*'.

      TRANSLATE GS_ALV_ITEM-TEXT TO UPPER CASE .

      SPLIT GS_ALV_ITEM-TEXT AT SPACE INTO: GS_STR-FLD01
                                            GS_STR-FLD02
                                            GS_STR-FLD03
                                            GS_STR-FLD04
                                            GS_STR-FLD05
                                            GS_STR-FLD06
                                            GS_STR-FLD07
                                            GS_STR-FLD08
                                            GS_STR-FLD09
                                            GS_STR-FLD10
                                            GS_STR-FLD11
                                            GS_STR-FLD12
                                            GS_STR-FLD13
                                            GS_STR-FLD14
                                            GS_STR-FLD15
                                            GS_STR-FLD16
                                            GS_STR-FLD17
                                            GS_STR-FLD18
                                            GS_STR-FLD19
                                            GS_STR-FLD20
                                            GS_STR-FLD21
                                            GS_STR-FLD22
                                            GS_STR-FLD23
                                            GS_STR-FLD24
                                            GS_STR-FLD25
                                            GS_STR-FLD26
                                            GS_STR-FLD27
                                            GS_STR-FLD28
                                            GS_STR-FLD29
                                            GS_STR-FLD30
                                            GS_STR-FLD31
                                            GS_STR-FLD32
                                            GS_STR-FLD33
                                            GS_STR-FLD34
                                            GS_STR-FLD35
                                            GS_STR-FLD36
                                            GS_STR-FLD37
                                            GS_STR-FLD38
                                            GS_STR-FLD39
                                            GS_STR-FLD40
                                            GS_STR-FLD41
                                            GS_STR-FLD42
                                            GS_STR-FLD43
                                            GS_STR-FLD44
                                            GS_STR-FLD45
                                            GS_STR-FLD46
                                            GS_STR-FLD47
                                            GS_STR-FLD48
                                            GS_STR-FLD49
                                            GS_STR-FLD50
                                            GS_STR-FLD51
                                            GS_STR-FLD52
                                            GS_STR-FLD53
                                            GS_STR-FLD54
                                            GS_STR-FLD55
                                            GS_STR-FLD56
                                            GS_STR-FLD57
                                            GS_STR-FLD58
                                            GS_STR-FLD59
                                            GS_STR-FLD60
                                            GS_STR-FLD61
                                            GS_STR-FLD62
                                            GS_STR-FLD63
                                            GS_STR-FLD64
                                            GS_STR-FLD65
                                            GS_STR-FLD66
                                            GS_STR-FLD67
                                            GS_STR-FLD68
                                            GS_STR-FLD69
                                            GS_STR-FLD70
                                            GS_STR-FLD71
                                            GS_STR-FLD72
                                            GS_STR-FLD73
                                            GS_STR-FLD74
                                            GS_STR-FLD75.
      CLEAR: G_SW1.
      DO 75 TIMES.
        MOVE SY-INDEX TO G_NUMC2.

        CONCATENATE 'GS_STR-FLD' G_NUMC2 INTO G_FIELD.
        ASSIGN (G_FIELD) TO  <FS_FIELD1>.

*       ## ## ### ## ## ### ## ### ### ### ## ### ## ###.
        IF G_SW1 IS NOT INITIAL.
          CLEAR: G_SW1.

          SELECT SINGLE TABNAME DDTEXT TABCLASS
            INTO (GS_FITB-TABNAME, GS_FITB-DDTEXT, GS_FITB-TABCLASS)
            FROM DD02V
           WHERE TABNAME EQ <FS_FIELD1>
             AND DDLANGUAGE EQ SY-LANGU. "##### KO# ## EN# ## ## ### ##.2013.02.18 BY NSI

          IF SY-SUBRC EQ 0.

*           ######## ##
            CLEAR: D010INC.
            SELECT SINGLE MASTER
              INTO GS_ALV_ITEM-REPNAME
              FROM D010INC
             WHERE MASTER EQ GS_ALV_ITEM-REPNAME.

*           Include PGM## ### ##.
            IF SY-SUBRC NE 0.
              REFRESH: GT_D010INC.
              CLEAR:   GT_D010INC.

              SELECT *
                INTO TABLE GT_D010INC
                FROM D010INC
               WHERE INCLUDE EQ GS_ALV_ITEM-REPNAME.

*             ### 2# ##### ## ## include## ### # ## ### ###.
*             ### ### ### ##### ##PGM# ## #.
              IF LINES( GT_D010INC[] ) EQ 1.
                READ TABLE GT_D010INC INTO D010INC INDEX 1.
                MOVE D010INC-MASTER TO GS_ALV_ITEM-REPNAME.
              ENDIF.
            ENDIF.

            MOVE GS_ALV_ITEM-REPNAME  TO GS_FITB-PROGNAME.
*
*            CLEAR: GS_FITB-MULGUBUN, GS_FITB-STAT_TYPE.
*            SELECT SINGLE MULGUBUN STAT_TYPE
*              INTO (GS_FITB-MULGUBUN, GS_FITB-STAT_TYPE)
*              FROM ZACTPGML3
*             WHERE PROGNAME EQ GS_ALV_ITEM-REPNAME.

*            MOVE-CORRESPONDING GT_PGML TO GS_FITB.

            CLEAR: GS_FITB-TEXT.
            SELECT SINGLE TEXT
              INTO GS_FITB-TEXT
              FROM TRDIRT
             WHERE NAME  EQ GS_ALV_ITEM-REPNAME
               AND TEXT  NE SPACE
               AND SPRSL EQ SY-LANGU.  "##### KO# ## EN# ## ## ### ##..###.

            COLLECT GS_FITB INTO GT_FITB.
            CLEAR:  GT_FITB, GS_FITB.

          ENDIF.

        ENDIF.

*       ## ### ### ### ## ### ### #### #### ####.
        IF <FS_FIELD1> IN SSTRING.
          MOVE 'X'          TO  G_SW1.
        ENDIF.

      ENDDO.
    ENDLOOP.

    MODIFY ZACTFITB4 FROM TABLE GT_FITB.
    COMMIT WORK.

    DATA : L_LINES  LIKE  SY-TABIX .
    DESCRIBE TABLE GT_FITB LINES L_LINES .
    MESSAGE S011(ZAC01) WITH L_LINES .

  ENDMETHOD.                    "display

  METHOD ADD_TO_HITLIST.
    DATA:
      LS_COL LIKE LINE OF GS_ALV_ITEM-CELL_COLOR.

    GS_ALV_ITEM-REPNAME = IV_REPORT.
    GS_ALV_ITEM-DYNNR   = IV_DYNPRO.
    GS_ALV_ITEM-LINE_NO = IV_TABIX.
    GS_ALV_ITEM-VERSNO  = IV_VERSNO.
    GS_ALV_ITEM-TEXT    = IV_SOURCE_LINE.

    IF IV_HIT IS NOT INITIAL.
      GS_ALV_ITEM-HIT = GC_X.
      ADD 1 TO GV_HIT_COUNT.
      LS_COL-FNAME     = 'TEXT'.
      LS_COL-COLOR-COL = '5'.
      APPEND LS_COL TO GS_ALV_ITEM-CELL_COLOR.
    ENDIF.

    APPEND GS_ALV_ITEM TO GT_ALV_ITEM.

    CLEAR GS_ALV_ITEM.
  ENDMETHOD.                    "add_to_hitlist

  METHOD GET_HIT_SET.
    DATA: LV_END     TYPE I,
          LV_START   TYPE I,
          LV_XTABIX  TYPE SY-TABIX,
          LV_HITLINE TYPE XFELD.

    FIELD-SYMBOLS:
      <LV_ABAP> TYPE ANY.

    LV_START = IV_TABIX - PLUSMINU .
    LV_END   = IV_TABIX + PLUSMINU.

    IF LV_START < 1.
      LV_START = 1.
    ENDIF.

    WHILE LV_START <= LV_END.
      READ TABLE IT_ABAP ASSIGNING <LV_ABAP> INDEX LV_START.
      IF SY-SUBRC IS NOT INITIAL.
        EXIT.
      ENDIF.

      LV_XTABIX = SY-TABIX.

      IF LV_START = IV_TABIX.
        LV_HITLINE = GC_X.
      ELSE.
        CLEAR LV_HITLINE.
      ENDIF.

      ADD 1 TO LV_START.

      IF COMMENT IS NOT INITIAL.
        IF MODIASS IS INITIAL.
          IF <LV_ABAP>(1) = '*'
          OR <LV_ABAP>(1) = '"'.
            CONTINUE.
          ENDIF.
        ELSE.
          IF <LV_ABAP>(1) = '*'
          OR <LV_ABAP>(1) = '"'
          AND NOT ( <LV_ABAP>(2) = '*{' OR <LV_ABAP>(2) = '*}' ).
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL METHOD ADD_TO_HITLIST
        EXPORTING
          IV_REPORT      = IV_REPORT
          IV_DYNPRO      = IV_DYNPRO
          IV_SOURCE_LINE = <LV_ABAP>
          IV_TABIX       = LV_XTABIX
          IV_HIT         = LV_HITLINE
          IV_VERSNO      = IV_VERSNO.

    ENDWHILE.

  ENDMETHOD.                    "get_hit_set

  METHOD GET_SOURCE_BY_VERSION.
    DATA:
      LV_OBJECT_NAME TYPE VERSOBJNAM,
      LS_OBJECT_NAME TYPE TY_LS_OBJNAME,
      LT_TRDIR       TYPE STANDARD TABLE OF TRDIR,
      LT_D022S       TYPE STANDARD TABLE OF D022S.

    IF IV_DYNPRO IS INITIAL.
      LV_OBJECT_NAME = IV_REPORT.

      CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
        EXPORTING
          OBJECT_NAME                  = LV_OBJECT_NAME
          OBJECT_TYPE                  = 'REPS'
          VERSNO                       = IV_VERSNO
          IV_NO_RELEASE_TRANSFORMATION = 'X'
        TABLES
          REPOS_TAB                    = RT_ABAP
          TRDIR_TAB                    = LT_TRDIR
        EXCEPTIONS
          NO_VERSION                   = 1
          OTHERS                       = 2.
    ELSE.
      LS_OBJECT_NAME-REPORT = IV_REPORT.
      LS_OBJECT_NAME-DYNNR  = IV_DYNPRO.

      LV_OBJECT_NAME = LS_OBJECT_NAME.

      CALL FUNCTION 'SVRS_GET_VERSION_DYNP_40'
        EXPORTING
          OBJECT_NAME = LV_OBJECT_NAME
          VERSNO      = IV_VERSNO
        TABLES
          D022S_TAB   = LT_D022S
        EXCEPTIONS
          NO_VERSION  = 01
          OTHERS      = 02.

      CHECK SY-SUBRC IS INITIAL AND LT_D022S IS NOT INITIAL.

      APPEND LINES OF LT_D022S TO RT_ABAP.

    ENDIF.
  ENDMETHOD.                    "get_source_by_version

  METHOD GET_VERSION_NUMBERS.
    DATA:
      LS_OBJNAME TYPE TY_LS_OBJNAME,
      LV_OBJTYPE TYPE VRSD-OBJTYPE,
      LV_OBJNAME TYPE VERSOBJNAM,
      LV_VERSNO  TYPE VERSNO,
      LT_VRSN    TYPE STANDARD TABLE OF VRSN,
      LT_VRSD    TYPE STANDARD TABLE OF VRSD.

    LS_OBJNAME-REPORT = IV_REPORT.
    LS_OBJNAME-DYNNR  = IV_DYNPRO.
    LV_OBJNAME        = LS_OBJNAME.

    IF IV_DYNPRO IS INITIAL.
      LV_OBJTYPE = 'REPS'.
    ELSE.
      LV_OBJTYPE = 'DYNP'.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
      EXPORTING
        OBJNAME                = LV_OBJNAME
        OBJTYPE                = LV_OBJTYPE
      TABLES
        LVERSNO_LIST           = LT_VRSN
        VERSION_LIST           = LT_VRSD
      EXCEPTIONS
        NO_ENTRY               = 1
        COMMUNICATION_FAILURE_ = 2
        SYSTEM_FAILURE         = 3
        OTHERS                 = 4.

    CHECK SY-SUBRC IS INITIAL .

    SORT LT_VRSD BY OBJNAME VERSNO.
    DELETE ADJACENT DUPLICATES FROM LT_VRSD COMPARING OBJNAME VERSNO.

    RT_VRSD = LT_VRSD.

    DELETE TABLE RT_VRSD WITH TABLE KEY OBJNAME = LV_OBJNAME
                                        VERSNO  = LV_VERSNO.

    SORT RT_VRSD.

    CHECK IV_DYNPRO IS NOT INITIAL.
*   For dynpros we need to save the version information for the version display
*   this is not required for source code
    INSERT LINES OF RT_VRSD INTO TABLE GT_VRSD.

  ENDMETHOD.                    "get_version_Numbers

  METHOD SEARCH_ABAP_SOURCE.
    DATA:
      LT_ABAP TYPE ABAPTXT255_TAB.

    FIELD-SYMBOLS:
     <LV_OBJ> TYPE TADIR-OBJ_NAME.

    LOOP AT GT_OBJECT ASSIGNING <LV_OBJ>.
      READ REPORT <LV_OBJ> INTO LT_ABAP.
      IF SY-SUBRC IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      SEARCH_SOURCE( IT_SOURCE = LT_ABAP
                     IV_REPORT = <LV_OBJ> ).

    ENDLOOP.

    FREE GT_OBJECT.

  ENDMETHOD.                    "search_abap_source

  METHOD SEARCH_SOURCE.
    DATA:
      LT_SOURCE       TYPE ABAPTXT255_TAB,
      LT_SOURCE_VERS  TYPE ABAPTXT255_TAB,
      LV_STRING_FOUND TYPE XFELD,
      LT_VRSD         TYPE STANDARD TABLE OF VRSD,
      LS_VRSD         LIKE LINE OF LT_VRSD,
      LV_NUMBER       TYPE I,
      LV_INDEX        TYPE I,
      LT_RESULTS      TYPE MATCH_RESULT_TAB,
      LS_RESULT       LIKE LINE OF LT_RESULTS,
      LS_SSTRING      LIKE LINE OF SSTRING.

    LT_SOURCE = IT_SOURCE.

    IF P_VERS IS INITIAL.
      LV_NUMBER = 1.
    ELSE.
      LT_VRSD = GET_VERSION_NUMBERS( IV_REPORT = IV_REPORT
                                     IV_DYNPRO = IV_DYNPRO ).

      LV_NUMBER = LINES( LT_VRSD ) + 1.
    ENDIF.

    DO LV_NUMBER TIMES.
      CLEAR LV_STRING_FOUND.

      IF SY-INDEX = 1.
        CLEAR LS_VRSD.
      ELSE.
        LV_INDEX = SY-INDEX - 1.
        READ TABLE LT_VRSD INDEX LV_INDEX INTO LS_VRSD.
        CHECK SY-SUBRC IS INITIAL.

        LT_SOURCE_VERS = GET_SOURCE_BY_VERSION( IV_REPORT = IV_REPORT
                                                IV_DYNPRO = IV_DYNPRO
                                                IV_VERSNO = LS_VRSD-VERSNO ).

        IF LT_SOURCE_VERS IS NOT INITIAL.
          LT_SOURCE = LT_SOURCE_VERS.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      LOOP AT SSTRING INTO LS_SSTRING.
        REFRESH LT_RESULTS.

        IF P_REGEX IS INITIAL.
          FIND ALL OCCURRENCES OF LS_SSTRING-LOW IN TABLE LT_SOURCE
            IN CHARACTER MODE
            IGNORING CASE
            RESULTS LT_RESULTS.
        ELSE.
          TRY.
              FIND ALL OCCURRENCES OF REGEX LS_SSTRING-LOW IN TABLE LT_SOURCE
                IN CHARACTER MODE
                IGNORING CASE
                RESULTS LT_RESULTS.
            CATCH CX_SY_REGEX.
*             invalid regex -> stop processing
              MESSAGE S384 WITH LS_SSTRING-LOW DISPLAY LIKE 'E'.
              RAISE EXCEPTION TYPE LCX_SCAN_EXCEPTIONS.
          ENDTRY.
        ENDIF.

        CHECK LT_RESULTS IS NOT INITIAL.

        LV_STRING_FOUND = GC_X.

        SORT LT_RESULTS BY LINE.
        DELETE ADJACENT DUPLICATES FROM LT_RESULTS COMPARING LINE.

        LOOP AT LT_RESULTS INTO LS_RESULT.
          CALL METHOD GET_HIT_SET
            EXPORTING
              IV_REPORT = IV_REPORT
              IV_DYNPRO = IV_DYNPRO
              IT_ABAP   = LT_SOURCE
              IV_TABIX  = LS_RESULT-LINE
              IV_VERSNO = LS_VRSD-VERSNO.
        ENDLOOP.

      ENDLOOP.
      IF LV_STRING_FOUND IS NOT INITIAL.
*       Add ALV header entry
        CLEAR GS_ALV_HEADER.

        GS_ALV_HEADER-REPNAME = IV_REPORT.
        GS_ALV_HEADER-DYNNR   = IV_DYNPRO.
        GS_ALV_HEADER-VERSNO  = LS_VRSD-VERSNO.
        APPEND GS_ALV_HEADER TO GT_ALV_HEADER.

        IF IV_DYNPRO IS NOT INITIAL.
          GV_DYNP_FOUND = GC_X.
        ENDIF.

        IF LS_VRSD-VERSNO IS NOT INITIAL.
          GV_VERS_FOUND = GC_X.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "search_source

  METHOD SEARCH_DYNPRO_SOURCE.
    DATA:
      LT_DFLOW TYPE ABAPTXT255_TAB.

    FIELD-SYMBOLS:
      <LS_DYNPRO>     LIKE LINE OF GT_DYNPRO.

    LOOP AT GT_DYNPRO ASSIGNING <LS_DYNPRO>.
      REFRESH LT_DFLOW.

      LT_DFLOW = GET_DYNPRO_FLOW_LOGIC( IV_REPORT =  <LS_DYNPRO>-REPNAME
                                        IV_DYNPRO  = <LS_DYNPRO>-DYNNR ).

      CHECK LT_DFLOW IS NOT INITIAL.

      SEARCH_SOURCE( IT_SOURCE = LT_DFLOW
                     IV_REPORT = <LS_DYNPRO>-REPNAME
                     IV_DYNPRO = <LS_DYNPRO>-DYNNR ).

    ENDLOOP.

  ENDMETHOD.                    "search_dynpro_source

  METHOD GET_DYNPROS.
    CHECK GT_OBJECT IS NOT INITIAL.

    SELECT PROG DNUM INTO TABLE GT_DYNPRO
      FROM D020S FOR ALL ENTRIES IN GT_OBJECT
      WHERE PROG = GT_OBJECT-TABLE_LINE
      AND   DNUM IN DYNNR.
  ENDMETHOD.                    "get_dynpros

  METHOD GET_INCLUDES.
    DATA:
     LT_INC     TYPE STANDARD TABLE OF TADIR-OBJ_NAME,
     LT_INC_TMP LIKE LT_INC,
     LV_PROGRAM TYPE SY-REPID,
     LV_OLD     TYPE XFELD.

    FIELD-SYMBOLS:
      <LV_OBJ> TYPE TADIR-OBJ_NAME.

    CHECK INCLU IS NOT INITIAL.

    LOOP AT GT_OBJECT ASSIGNING <LV_OBJ>
      WHERE TABLE_LINE(2) <> 'CL'.    "for classes we already have the includes

      REFRESH LT_INC_TMP.

      IF LV_OLD IS NOT INITIAL.
        CALL FUNCTION 'GET_INCLUDES'
          EXPORTING
            PROGNAME = <LV_OBJ>
          TABLES
            INCLTAB  = LT_INC_TMP.

      ELSE.
        LV_PROGRAM = <LV_OBJ>.

        CALL FUNCTION 'RS_GET_ALL_INCLUDES'
          EXPORTING
            PROGRAM      = LV_PROGRAM
          TABLES
            INCLUDETAB   = LT_INC_TMP
          EXCEPTIONS
            NOT_EXISTENT = 1
            NO_PROGRAM   = 2
            OTHERS       = 3.

        CHECK SY-SUBRC IS INITIAL.
      ENDIF.

      APPEND LINES OF LT_INC_TMP TO LT_INC.
    ENDLOOP.

    SORT LT_INC.
    DELETE ADJACENT DUPLICATES FROM LT_INC.

    APPEND LINES OF LT_INC TO GT_OBJECT.

  ENDMETHOD.                    "get_includes

  METHOD GET_METHOD_INCLUDES.
    DATA: LO_NAME     TYPE REF TO CL_OO_INCLUDE_NAMING,
          LO_NAME_TMP TYPE REF TO IF_OO_CLIF_INCL_NAMING,
          LT_METHOD   TYPE SEOP_METHODS_W_INCLUDE,
          LV_OBJ      TYPE TADIR-OBJ_NAME.

    FIELD-SYMBOLS:
     <LS_METHOD> LIKE LINE OF LT_METHOD.

    CALL METHOD CL_OO_INCLUDE_NAMING=>GET_INSTANCE_BY_NAME
      EXPORTING
        NAME           = IV_CLASS_NAME
      RECEIVING
        CIFREF         = LO_NAME_TMP
      EXCEPTIONS
        NO_OBJECTTYPE  = 1
        INTERNAL_ERROR = 2
        OTHERS         = 3.

    CHECK SY-SUBRC IS INITIAL.

    LO_NAME ?= LO_NAME_TMP.

    CALL METHOD LO_NAME->IF_OO_CLASS_INCL_NAMING~GET_ALL_METHOD_INCLUDES
      RECEIVING
        METHODS_W_INCLUDE           = LT_METHOD
      EXCEPTIONS
        INTERNAL_CLASS_NOT_EXISTING = 1
        OTHERS                      = 2.

    LOOP AT LT_METHOD ASSIGNING <LS_METHOD>.
      LV_OBJ = <LS_METHOD>-INCNAME.
      APPEND LV_OBJ TO GT_OBJECT.
    ENDLOOP.
  ENDMETHOD.                    "get_method_includes

  METHOD GET_REPORT_NAMES.
    SELECT OBJ_NAME INTO TABLE GT_OBJECT
      FROM TADIR
      WHERE PGMID  = 'R3TR'
      AND   OBJECT = 'PROG'
      AND   DEVCLASS IN DEVCLASS.                       "#EC CI_GENBUFF
  ENDMETHOD.                    "get_report_names

  METHOD GET_FUNCTION_NAMES.
    DATA:
      LT_OBJ     TYPE STANDARD TABLE OF TADIR-OBJ_NAME,
      LV_OBJ     TYPE TADIR-OBJ_NAME,
      LV_FGROUP  TYPE RS38L-AREA,
      LV_PROGRAM TYPE PROGNAME.

    FIELD-SYMBOLS:
      <LV_OBJ> LIKE LINE OF LT_OBJ.

    SELECT OBJ_NAME INTO TABLE LT_OBJ
      FROM TADIR
      WHERE PGMID  = 'R3TR'
      AND   OBJECT = 'FUGR'
      AND   DEVCLASS IN DEVCLASS
      AND   OBJ_NAME IN FUNCGRP.                        "#EC CI_GENBUFF

    LOOP AT LT_OBJ ASSIGNING <LV_OBJ>.
      LV_FGROUP = <LV_OBJ>.
      CLEAR LV_PROGRAM.

      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        CHANGING
          PROGRAM                  = LV_PROGRAM
          COMPLETE_AREA            = LV_FGROUP
        EXCEPTIONS
          NOT_ENOUGH_INPUT         = 1
          NO_FUNCTION_POOL         = 2
          DELIMITER_WRONG_POSITION = 3
          OTHERS                   = 4.

      CHECK SY-SUBRC IS INITIAL AND LV_PROGRAM IS NOT INITIAL.

      LV_OBJ = LV_PROGRAM.
      APPEND LV_OBJ TO GT_OBJECT.
    ENDLOOP.
  ENDMETHOD.                    "get_function_names

  METHOD GET_CLASS_NAMES.
    DATA:
      LT_OBJ TYPE STANDARD TABLE OF TADIR-OBJ_NAME.

    FIELD-SYMBOLS:
      <LS_OBJ> LIKE LINE OF LT_OBJ.

    SELECT OBJ_NAME INTO TABLE LT_OBJ
      FROM TADIR
      WHERE PGMID  = 'R3TR'
      AND   OBJECT = 'CLAS'
      AND   DEVCLASS IN DEVCLASS
      AND   OBJ_NAME IN P_CLASS.                        "#EC CI_GENBUFF

    LOOP AT LT_OBJ ASSIGNING <LS_OBJ>.
      GET_METHOD_INCLUDES( <LS_OBJ> ).
    ENDLOOP.
  ENDMETHOD.                    "get_class_names

  METHOD GET_SOURCE_NAMES.

    REFRESH: GT_OBJECT.

    IF REPNAME[] IS NOT INITIAL OR
       CNAM[]    IS NOT INITIAL OR
       UNAM[]    IS NOT INITIAL OR
       SUBC[]    IS NOT INITIAL OR
       APPL[]    IS NOT INITIAL.

      SELECT NAME APPENDING TABLE GT_OBJECT
        FROM TRDIR
        WHERE NAME IN REPNAME
        AND   CNAM IN CNAM
        AND   UNAM IN UNAM
        AND   SUBC IN SUBC
        AND   APPL IN APPL.
    ENDIF.

    IF DEVCLASS[] IS NOT INITIAL.
      GET_REPORT_NAMES( ).
      GET_FUNCTION_NAMES( ).
      GET_CLASS_NAMES( ).
    ENDIF.

    IF FUNCGRP[] IS NOT INITIAL.
      GET_FUNCTION_NAMES( ).
    ENDIF.

    IF P_CLASS[] IS NOT INITIAL.
      GET_CLASS_NAMES( ).
    ENDIF.

    IF RB_CODE IS INITIAL.
      GET_DYNPROS( ).
    ENDIF.

  ENDMETHOD.                    "get_source_names

  METHOD START.
    DATA:
     LS_SSTRING LIKE LINE OF SSTRING[].


    REFRESH: GT_ALV_HEADER.
    REFRESH:  GT_ALV_ITEM.


    IF SSTRING[] IS INITIAL AND MODIASS IS INITIAL.
*     Please specifiy a search term
      MESSAGE S385 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF NOT MODIASS IS INITIAL.
      REFRESH SSTRING.
      LS_SSTRING-SIGN    = 'I'.
      LS_SSTRING-OPTION  = 'EQ'.
      LS_SSTRING-LOW     = '^\*\{'.
      APPEND LS_SSTRING TO SSTRING.
      LS_SSTRING-LOW     = '^\*\}'.
      APPEND LS_SSTRING TO SSTRING.

      P_REGEX = GC_X.
    ENDIF.

    READ TABLE SSTRING[] INTO LS_SSTRING INDEX 1.
    IF LINES( SSTRING[] ) = 1.
      GV_SSTRING = LS_SSTRING-LOW.
    ELSE.
      CONCATENATE LS_SSTRING-LOW
                  '...'
                  INTO GV_SSTRING.
    ENDIF.

    GET_SOURCE_NAMES( ).
    GET_INCLUDES( ).

    IF RB_DYN IS INITIAL.
      TRY.
          SEARCH_ABAP_SOURCE( ).
        CATCH LCX_SCAN_EXCEPTIONS.
          RETURN.
      ENDTRY.
    ENDIF.

    IF RB_CODE IS INITIAL.
      TRY.
          SEARCH_DYNPRO_SOURCE( ).
        CATCH LCX_SCAN_EXCEPTIONS.
          RETURN.
      ENDTRY.
    ENDIF.

    DISPLAY( ).
  ENDMETHOD.                    "start
ENDCLASS.                    "lcl_source_scan IMPLEMENTATION

INITIALIZATION.
* #### ## ###
  REFRESH: SSTRING.
  SSTRING = 'IEQ'.
  SSTRING-LOW = 'FROM'.
  APPEND SSTRING.
  SSTRING-LOW = 'INSERT'.
  APPEND SSTRING.
  SSTRING-LOW = 'UPDATE'.
  APPEND SSTRING.
  SSTRING-LOW = 'DELETE'.
  APPEND SSTRING.
  SSTRING-LOW = 'MODIFY'.
  APPEND SSTRING.
  SSTRING-LOW = 'JOIN'.
  APPEND SSTRING.

  CREATE OBJECT LO_SSCAN.

  A05 = 'Suchbegriff'(A05).
  A10 = 'Report/Dynpro Selektion'(A10).
  A11 = 'Paket Selektion'(A11).
  A12 = 'Funktionsgruppen Selektion'(A12).
  A13 = 'Klassen Selektion'(A13).
  A20 = 'Suchkriterien'(A20).
  A30 = 'Suchbereich'(A30).

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CLASS-LOW.
*  LO_SSCAN->F4_CLASS( CHANGING CV_CLASS_NAME = P_CLASS-LOW ).

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CLASS-HIGH.
*  LO_SSCAN->F4_CLASS( CHANGING CV_CLASS_NAME = P_CLASS-HIGH ).
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR FUNCGRP-LOW.
*  LO_SSCAN->F4_FUNCTION_GROUP( 'FUNCGRP-LOW' ).

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR FUNCGRP-HIGH.
*  LO_SSCAN->F4_FUNCTION_GROUP( 'FUNCGRP-HIGH' ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR REPNAME-LOW.
  LO_SSCAN->F4_REPNAME( CHANGING CV_REPNAME = REPNAME-LOW ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR REPNAME-HIGH.
  LO_SSCAN->F4_REPNAME( CHANGING CV_REPNAME = REPNAME-HIGH ).

START-OF-SELECTION.

  SELECT DISTINCT PGMNA AS PROGNAME
    INTO CORRESPONDING FIELDS OF TABLE  GT_PGML
    FROM TSTC
   WHERE PGMNA IN REPNAME.
***   TCODE IN REPNAME.

  IF GT_PGML[] IS INITIAL.
    MESSAGE S003.
    EXIT.
  ENDIF.

  LOOP AT GT_PGML.
    REFRESH REPNAME.
    CLEAR:  REPNAME.

    REPNAME-SIGN = 'I'.
    REPNAME-OPTION = 'EQ'.
    REPNAME-LOW = GT_PGML-PROGNAME.
    APPEND REPNAME.
    CLEAR: REPNAME.

    LO_SSCAN->START( ).

  ENDLOOP.
