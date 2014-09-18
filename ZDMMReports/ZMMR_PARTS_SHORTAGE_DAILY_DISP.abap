************************************************************************
* Program Name      : ZMMR_PARTS_SHORTAGE_DAILY_DISP\
* Author            : Furong Wang
* Creation Date     : 05/21//2010
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZMMR_PARTS_SHORTAGE_DAILY_DISP MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TABLES: MARA, ZTMM_DAILY_SHORT.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF IT_SHORT OCCURS 0.
        INCLUDE STRUCTURE ZTMM_DAILY_SHORT.
DATA: LIGHTS,
      COLOR TYPE SLIS_T_SPECIALCOL_ALV,
      VEN_NAME LIKE LFA1-NAME1,
      END OF IT_SHORT.

DATA: BEGIN OF IT_SHORT2 OCCURS 0.
        INCLUDE STRUCTURE ZTMM_DAILY_SHORT.
data: END OF IT_SHORT2.

** ALV
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
      IT_EXCLUDE TYPE UI_FUNCTIONS.

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL_800 TYPE SCRFNAME VALUE 'ALV_CONTAINER_800',
      ALV_GRID_800          TYPE REF TO CL_GUI_ALV_GRID,
*      GRID_CONTAINER_800    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
     GRID_dock_CONTAINER_800 TYPE REF TO CL_GUI_docking_CONTAINER.

DATA: G_LIGHTS_FIELDNAME  TYPE SLIS_FIELDNAME VALUE 'LIGHTS'.

DATA: BEGIN OF IT_DAY OCCURS 21,
        SEQ(2)  TYPE N,
        DATUM   LIKE   SY-DATUM,
      END   OF IT_DAY.

DATA: BEGIN OF IT_WEEK OCCURS 21,
        SEQ(2)  TYPE N,
        DATUM   LIKE   SY-DATUM,
      END   OF IT_WEEK.


DATA: OK_CODE LIKE SY-UCOMM,
      W_CODE LIKE SY-UCOMM,
      W_OLD_CODE LIKE SY-UCOMM,
      W_CNT   TYPE   I,
      W_BASE_DATE LIKE SY-DATUM,
      W_BASE_TIME LIKE SY-UZEIT,
      W_REPID LIKE SY-REPID,
      W_DYNNR LIKE SY-DYNNR,
            W_MAX_DATE LIKE SY-DATUM..

DATA:  W_KALID LIKE KAKO-KALID.
*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER DEFINITION INHERITING FROM CL_GUI_CONTROL.

  PUBLIC SECTION.

    CONSTANTS:  EVENTID_FINISHED TYPE I VALUE 1 .

    CLASS-DATA: INTERVAL TYPE I VALUE '0'.

    EVENTS:     FINISHED .

    METHODS:
*             show_alv,
             CANCEL
                  EXCEPTIONS
                     ERROR,
             CONSTRUCTOR
                 IMPORTING
                     LIFETIME TYPE I OPTIONAL
                     VALUE(SHELLSTYLE) TYPE I OPTIONAL
                     VALUE(PARENT) TYPE REF TO CL_GUI_CONTAINER OPTIONAL
                 EXCEPTIONS
                     ERROR,
             RUN
                 EXCEPTIONS
                     ERROR,
             DISPATCH REDEFINITION.


ENDCLASS.                    "lcl_gui_timer DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
                ON_FINISHED
                       FOR EVENT FINISHED OF LCL_GUI_TIMER.

ENDCLASS.                    "lcl_event_handler DEFINITION


DATA: TIMER_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GUI_TIMER TYPE REF TO LCL_GUI_TIMER,
      EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER,
      TIMEOUT_INTERVAL TYPE I.
*      L_ALV TYPE REF TO CL_GUI_ALV_GRID,
*      FIRST_CALL(1) TYPE C,
**      ok_code     TYPE syucomm,
*      L_IS_STABLE TYPE LVC_S_STBL.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_FINISHED.

* Start Timer again

    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.

* cause PAI
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = 'REFR'.

  ENDMETHOD.                    "on_finished

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    TYPE-POOLS: SFES.

    DATA CLSID(80).
    DATA EVENT_TAB TYPE CNTL_SIMPLE_EVENTS.
    DATA EVENT_TAB_LINE TYPE CNTL_SIMPLE_EVENT.

    IF CLSID IS INITIAL.
      DATA: RETURN,
            GUITYPE TYPE I.

      GUITYPE = 0.
      CALL FUNCTION 'GUI_HAS_OBJECTS'
           EXPORTING
                OBJECT_MODEL = SFES_OBJ_ACTIVEX
           IMPORTING
                RETURN       = RETURN
           EXCEPTIONS
                OTHERS       = 1.
      IF SY-SUBRC NE 0.
        RAISE ERROR.
      ENDIF.

      IF RETURN = 'X'.
        GUITYPE = 1.
      ENDIF.
      IF GUITYPE = 0.
        CALL FUNCTION 'GUI_HAS_OBJECTS'
             EXPORTING
                  OBJECT_MODEL = SFES_OBJ_JAVABEANS
             IMPORTING
                  RETURN       = RETURN
             EXCEPTIONS
                  OTHERS       = 1.
        IF SY-SUBRC NE 0.
          RAISE ERROR.
        ENDIF.

        IF RETURN = 'X'.
          GUITYPE = 2.
        ENDIF.
      ENDIF.

      CASE GUITYPE.
        WHEN 1.
          CLSID = 'Sapgui.InfoCtrl.1'.
        WHEN 2.
          CLSID = 'com.sap.components.controls.sapImage.SapImage'.
      ENDCASE.
    ENDIF.

    CALL METHOD SUPER->CONSTRUCTOR
      EXPORTING
        CLSID      = CLSID
        SHELLSTYLE = 0
        PARENT     = CL_GUI_CONTAINER=>DEFAULT_SCREEN
        AUTOALIGN  = SPACE
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

    CALL METHOD CL_GUI_CFW=>SUBSCRIBE
      EXPORTING
        SHELLID = H_CONTROL-SHELLID
        REF     = ME
      EXCEPTIONS
        OTHERS  = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

* Register the events
    EVENT_TAB_LINE-EVENTID = LCL_GUI_TIMER=>EVENTID_FINISHED.
    APPEND EVENT_TAB_LINE TO EVENT_TAB.

    CALL METHOD SET_REGISTERED_EVENTS
      EXPORTING
        EVENTS = EVENT_TAB.

  ENDMETHOD.                    "constructor

  METHOD CANCEL.

    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = -1
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.
  ENDMETHOD.                    "cancel

  METHOD RUN.

    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = INTERVAL
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.


  ENDMETHOD.                    "run

  METHOD DISPATCH .

    CASE EVENTID.
      WHEN EVENTID_FINISHED.
        RAISE EVENT FINISHED.
    ENDCASE.

    CLEAR TIMER_CONTAINER.

  ENDMETHOD.                    "dispatch

ENDCLASS.                    "lcl_gui_timer IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS:   P_WERKS   TYPE MARC-WERKS MEMORY ID WRK OBLIGATORY
*DEFAULT
* 'P001'.
SELECT-OPTIONS :
  S_MATNR   FOR ZTMM_DAILY_SHORT-MATNR MODIF ID REQ,
  S_WERKS   FOR ZTMM_DAILY_SHORT-WERKS MODIF ID REQ,
  S_MTART   FOR ZTMM_DAILY_SHORT-MTART MODIF ID REQ,
  S_MATKL   FOR MARA-MATKL MODIF ID REQ,
  S_DISPO   FOR ZTMM_DAILY_SHORT-DISPO MODIF ID REQ,   "MRP controller
  S_LGPRO   FOR ZTMM_DAILY_SHORT-LGPRO MODIF ID REQ,
  S_PRVBE   FOR ZTMM_DAILY_SHORT-PRVBE MODIF ID REQ,
  S_SORTF   FOR ZTMM_DAILY_SHORT-SORTF MODIF ID REQ,
  S_LIFNR   FOR ZTMM_DAILY_SHORT-LIFNR MODIF ID REQ.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 25(5) TEXT-ALL FOR FIELD PC_ALL .
SELECTION-SCREEN POSITION 18.
PARAMETERS  PC_ALL  RADIOBUTTON GROUP ICON DEFAULT 'X'.
SELECTION-SCREEN COMMENT 32(5) TEXT-RED FOR FIELD PC_RED .
PARAMETERS  PC_RED  RADIOBUTTON GROUP ICON .
SELECTION-SCREEN COMMENT 46(5) TEXT-YEL FOR FIELD PC_YEL .
PARAMETERS  PC_YEL  RADIOBUTTON GROUP ICON .
SELECTION-SCREEN COMMENT 60(5) TEXT-GRE FOR FIELD PC_GRE .
PARAMETERS  PC_GRE  RADIOBUTTON GROUP ICON .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) TEXT-011  .
SELECTION-SCREEN COMMENT 31(2) TEXT-012 FOR FIELD P_RED1 .
PARAMETERS :  P_RED1 TYPE MDKP-BERW1 DEFAULT '10'.
SELECTION-SCREEN COMMENT 45(2) TEXT-012 FOR FIELD P_YEL1 .
PARAMETERS :  P_YEL1 TYPE MDKP-BERW1 DEFAULT '20'.
SELECTION-SCREEN COMMENT 59(2) TEXT-012 FOR FIELD P_GRE1 .
PARAMETERS :  P_GRE1 TYPE MDKP-BERW1 DEFAULT '999'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLOCK2.


SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN : BEGIN OF LINE.

PARAMETERS : P_TOPB AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(10) TEXT-TOP FOR FIELD P_TOPB.
PARAMETERS: P_TOPV TYPE  I DEFAULT '20'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN : BEGIN OF LINE.
PARAMETERS : P_LESB AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(10) TEXT-LES FOR FIELD P_LESB.
PARAMETERS: P_LESV TYPE  I DEFAULT '20'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : BEGIN OF LINE.
PARAMETERS : P_GRTB AS CHECKBOX.
SELECTION-SCREEN COMMENT 8(10) TEXT-GRT FOR FIELD P_GRTB.
PARAMETERS: P_GRTV TYPE  I DEFAULT '4'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK6 WITH FRAME TITLE TEXT-006.
PARAMETERS :
    P_INTRV TYPE I DEFAULT '10'.
SELECTION-SCREEN END OF BLOCK BLOCK6.


*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM INIT_DATA.

* Set F4 values for MRP Controller
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-LOW.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         DISPO  LIKE MARC-DISPO,
         DESC LIKE T024D-DSNAM,
         END OF VALUE_TAB.

  SELECT DISTINCT A~DISPO DSNAM
     INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T024D AS B
     ON A~DISPO = B~DISPO
     WHERE A~WERKS = 'P001'
     AND A~DISPO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'DISPO'
            DYNPPROG        = W_REPID
            DYNPNR          = W_DYNNR
            DYNPROFIELD     = 'DISPO'
            WINDOW_TITLE    = 'MRP Controller'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-HIGH.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         DISPO  LIKE MARC-DISPO,
         DESC LIKE T024D-DSNAM,
         END OF VALUE_TAB.

  SELECT DISTINCT A~DISPO DSNAM
     INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T024D AS B
     ON A~DISPO = B~DISPO
     WHERE A~WERKS = 'P001'
     AND A~DISPO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'DISPO'
            DYNPPROG        = W_REPID
            DYNPNR          = W_DYNNR
            DYNPROFIELD     = 'DISPO'
            WINDOW_TITLE    = 'MRP Controller'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

* Set F4 values for Prod Storage Location
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LGPRO-LOW.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         LGPRO  LIKE MARC-LGPRO,
         DESC LIKE T001L-LGOBE,
         END OF VALUE_TAB.

  SELECT DISTINCT A~LGPRO LGOBE INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T001L AS B
     ON A~LGPRO = B~LGORT
     WHERE A~WERKS = 'P001'
       AND LGPRO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'LGPRO'
            DYNPPROG        = W_REPID
            DYNPNR          = W_DYNNR
            DYNPROFIELD     = 'LGPRO'
            WINDOW_TITLE    = 'Prod Storage Loca'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LGPRO-HIGH.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         LGPRO  LIKE MARC-LGPRO,
         DESC LIKE T001L-LGOBE,
         END OF VALUE_TAB.

  SELECT DISTINCT A~LGPRO LGOBE INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T001L AS B
     ON A~LGPRO = B~LGORT
     WHERE A~WERKS = 'P001'
       AND LGPRO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'LGPRO'
            DYNPPROG        = W_REPID
            DYNPNR          = W_DYNNR
            DYNPROFIELD     = 'LGPRO'
            WINDOW_TITLE    = 'Prod Storage Loca'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM GET_DATA.
  IF IT_SHORT[] IS INITIAL.
    MESSAGE I000(ZZ) WITH 'No Data Found'.
  ELSE.
    IF SY-BATCH = ' '.
      CALL SCREEN 0800.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
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
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object
**&---------------------------------------------------------------------
**
**&      Form  setting_fieldcat
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_IT_FIELDCAT  text
**      -->P_0584   text
**      -->P_0585   text
**      -->P_0586   text
**----------------------------------------------------------------------
**
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.
* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog' P_GUBUN P_FIELD.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME .
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
*
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS.
*  DATA LS_EXCLUDE TYPE UI_FUNC.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*FORM ASSIGN_ITAB_TO_ALV_210.
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_0800 OUTPUT.
  IF GRID_dock_CONTAINER_800 IS INITIAL.
    PERFORM CREATE_CONTAINER_OBJECT_800.
    PERFORM SET_ATTRIBUTES_ALV_GRID_800.
*    PERFORM BUILD_SORTCAT_DISPLAY.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG_800 USING 'IT_SHORT'.
    PERFORM ASSIGN_ITAB_TO_ALV_800.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID_800->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_OBJECT_800.
** Changed on 08/27/13
*  CLEAR: W_REPID.
*  CREATE OBJECT GRID_CONTAINER_800
*          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL_800
*          EXCEPTIONS
*           CNTL_ERROR = 1
*           CNTL_SYSTEM_ERROR = 2
*           CREATE_ERROR = 3
*           LIFETIME_ERROR = 4
*           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*  W_REPID = SY-REPID.
    w_repid = sy-repid.
    w_dynnr = sy-dynnr.
    CREATE OBJECT grid_dock_container_800
    EXPORTING
      repid     = w_repid
      dynnr     = w_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.
** End on 08/27/13

  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID_800
         EXPORTING I_PARENT = grid_dock_container_800
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_800.
  DATA: L_DATE_C(10),
        L_TIME(8).

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
 CONCATENATE W_BASE_DATE+4(2) '/' W_BASE_DATE+6(2) '/' W_BASE_DATE+0(4)
                                                          INTO L_DATE_C.
 CONCATENATE W_BASE_TIME+0(2) ':' W_BASE_TIME+2(2) ':' W_BASE_TIME+4(2)
                                                          INTO L_TIME.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-CTAB_FNAME  = 'COLOR'.
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.

  CONCATENATE 'As of' L_DATE_C L_TIME INTO WA_IS_LAYOUT-GRID_TITLE
  SEPARATED BY SPACE.

*  CONCATENATE WA_IS_LAYOUT-GRID_TITLE W_UPDATING INTO
*  WA_IS_LAYOUT-GRID_TITLE
*  SEPARATED BY SPACE.

*  L_UPH_C =  L_UPH.
*  WA_IS_LAYOUT-GRID_TITLE+50(5) = 'UPH :'.
*  MOVE: L_UPH_C TO WA_IS_LAYOUT-GRID_TITLE+56(10).

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

  WA_IS_LAYOUT-ZEBRA             = 'X'.


ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3194   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_800 USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,
  DATA: L_CN(2) TYPE N,
        L_RP(30),
        L_DAY(10),
        L_DATUM(8).

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

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

                                  'S' 'MATNR'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'WERKS'    ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '4',

                                 'S' 'MAKTX'    ' ',
                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'HR_OH'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DECIMALS_O'  '1',
                                  ' ' 'COLTEXT'     'On Hand',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'VEN_NAME'    ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'SORTF'    ' ',
                                  ' ' 'COLTEXT'     'Sort String',
                                  'E' 'OUTPUTLEN'   '3',

                                 'S' 'PRVBE'    ' ',
                                 ' ' 'COLTEXT'     'Supp Area',
                                  'E' 'OUTPUTLEN'   '2',

                                 'S' 'DSNAM'    ' ',
                                  ' ' 'COLTEXT'     'MRP Controller',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'MATKL'    ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '3',

                                 'S' 'OHQTY'    ' ',
                                 ' ' 'COLTEXT'     'Stock',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '8'.

*  L_CN = '00'.
*  DO 21 TIMES.
*    L_CN = L_CN + 1.
*
**    READ TABLE it_day WITH KEY seq = l_cn.
*
*
*    CONCATENATE 'D' L_CN INTO L_RP.
*    CONCATENATE  L_CN 'Day' INTO L_DAY SEPARATED BY SPACE.
*
*    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
*
*                                   'S' L_RP        ' ',
*                                   ' ' 'COLTEXT'     L_DAY,
*                                   ' ' 'DECIMALS_O'  '0',
*                                   'E' 'OUTPUTLEN'   '10'.
*    CLEAR: L_RP.
*  ENDDO.

*  L_CN = '00'.
*  DO 18 TIMES.
*    L_CN = L_CN + 1.
*
**    READ TABLE it_day WITH KEY seq = l_cn.
*
*
*    CONCATENATE 'W' L_CN INTO L_RP.
*    CONCATENATE  L_CN 'Week' INTO L_DAY SEPARATED BY SPACE.
*
*    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
*
*                                   'S' L_RP        ' ',
*                                   ' ' 'COLTEXT'     L_DAY,
*                                   ' ' 'DECIMALS_O'  '0',
*                                   'E' 'OUTPUTLEN'   '10'.
*    CLEAR: L_RP, L_DAY.
*  ENDDO.

*  L_CN = '00'.
*  DO 21 TIMES.
*    L_CN = L_CN + 1.
*
*    READ TABLE IT_DAY WITH KEY SEQ = L_CN.
*
*    CONCATENATE 'D' L_CN INTO L_RP.
*
*    WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.
*
*    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
*
*                                   'S' L_RP        ' ',
*                                   ' ' 'COLTEXT'     L_DATUM,
*                                   ' ' 'DECIMALS_O'  '0',
*                                   'E' 'OUTPUTLEN'   '6'.
*    CLEAR: L_RP, L_DATUM.
*  ENDDO.

  L_CN = '00'.
  LOOP AT IT_DAY.
*  DO 21 TIMES.
    L_CN = L_CN + 1.

*    READ TABLE it_day WITH KEY seq = l_cn.
*    CONCATENATE 'D' L_CN INTO L_RP.
*    CONCATENATE  L_CN 'Day' INTO L_DAY SEPARATED BY SPACE.

    CONCATENATE 'D' IT_DAY-SEQ INTO L_RP.
    WRITE IT_DAY-DATUM TO L_DAY DD/MM/YYYY .

    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                    'S' L_RP        ' ',
                                    ' ' 'COLTEXT'     L_DAY,
                                    ' ' 'DECIMALS_O'  '0',
                                    'E' 'OUTPUTLEN'   '8'.
    CLEAR: L_RP.
*  ENDDO.
  ENDLOOP.

  L_CN = '00'.
  DO 18 TIMES.
    L_CN = L_CN + 1.

    READ TABLE IT_WEEK WITH KEY SEQ = L_CN.
    IF SY-SUBRC = 0.
      CONCATENATE 'W' L_CN INTO L_RP.

      WRITE IT_WEEK-DATUM TO L_DATUM MM/DD/YY.

      PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                     'S' L_RP        ' ',
                                     ' ' 'COLTEXT'     L_DATUM,
                                     ' ' 'DECIMALS_O'  '0',
                                     'E' 'OUTPUTLEN'   '7'.
      CLEAR: L_RP, L_DATUM.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.


  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                    'S' 'TOTAL'      ' ',
                                    ' ' 'COLTEXT'     'Total',
                                    ' ' 'DECIMALS_O'  '0',
                                    'E' 'OUTPUTLEN'   '9'.

ENDFORM.                    " BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_800.
  CALL METHOD ALV_GRID_800->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
*               I_DEFAULT        = SPACE
*               it_toolbar_excluding = IT_EXCLUDE[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_SHORT[]
               IT_SORT          = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID_800->REGISTER_EDIT_EVENT
                EXPORTING
                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID_800->REGISTER_EDIT_EVENT
                EXPORTING
                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID_800.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0800 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REFR'.
      SUBMIT ZMMR_PARTS_SHORTAGE_DAILY_DISP
          WITH S_MATNR IN S_MATNR
                WITH S_WERKS IN S_WERKS
       WITH S_MTART IN S_MTART
       WITH S_MATKL IN S_MATKL
       WITH S_DISPO IN S_DISPO
       WITH S_LGPRO IN S_LGPRO
       WITH S_PRVBE IN S_PRVBE
       WITH S_SORTF IN S_SORTF
       WITH S_LIFNR IN S_LIFNR
       WITH PC_ALL = PC_ALL
       WITH PC_RED = PC_RED
       WITH PC_YEL = PC_YEL
       WITH PC_GRE = PC_GRE
       WITH P_RED1 = P_RED1
       WITH P_YEL1 = P_YEL1
       WITH P_GRE1 = P_GRE1
       WITH P_TOPB = P_TOPB
       WITH P_TOPV = P_TOPV
       WITH P_LESB = P_LESB
       WITH P_LESV = P_LESV
       WITH P_GRTB = P_GRTB
       WITH P_GRTV = P_GRTV
       WITH P_INTRV = P_INTRV.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_cell_color
*&---------------------------------------------------------------------*
*       Set Cell Color
*----------------------------------------------------------------------*
*FORM SET_CELL_COLOR  USING    U_COL
*                              U_INT
*                              U_FIELD
*                     CHANGING COLOR_TAB
*                              TYPE SLIS_T_SPECIALCOL_ALV.
**----------------------------------------------------------------------
**
** No  Colour
**  0  COL_BACKGROUND
**  1  COL_HEADING
**  2  COL_NORMAL
**  3  COL_TOTAL
**  4  COL_KEY
**  5  COL_POSITIVE
**  6  COL_NEGATIVE
**  7  COL_GROUP
**----------------------------------------------------------------------
**
*  DATA : L_COLOR TYPE SLIS_SPECIALCOL_ALV.
*  L_COLOR-FIELDNAME = U_FIELD.
*  L_COLOR-COLOR-COL = U_COL.
*  L_COLOR-COLOR-INT = U_INT.
*  APPEND L_COLOR TO COLOR_TAB.
*ENDFORM.                    " set_cell_color
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  W_REPID = SY-REPID.
  W_DYNNR = SY-DYNNR.
ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  save_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TO_TABLE.

*// === 2011.08.04 insert by Kim_yn.     for ECC6 upgrade === //*
  clear: IT_SHORT2, IT_SHORT2[].

  loop at IT_SHORT.
    move-corresponding IT_SHORT to IT_SHORT2.
    append IT_SHORT2.
    clear: IT_SHORT2.
  endloop.

  check it_short2[] is not initial.
*// ========================= end ========================== //*

  DELETE FROM ZTMM_DAILY_SHORT CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
*//  INSERT ZTMM_DAILY_SHORT FROM TABLE IT_SHORT.
  INSERT ZTMM_DAILY_SHORT FROM TABLE IT_SHORT2.
*//
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    MESSAGE E999(PP) WITH 'Error: saving ZTMM_DAILY_SHORT'.
  ENDIF.
ENDFORM.                    " save_to_table
*&---------------------------------------------------------------------*
*&      Module  SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TIMER OUTPUT.
  IF TIMER_CONTAINER IS INITIAL.
    CREATE OBJECT:

       TIMER_CONTAINER
             EXPORTING
                  CONTAINER_NAME = 'TI_CONTAINER',
       GUI_TIMER
             EXPORTING
                  PARENT = TIMER_CONTAINER.

    SET HANDLER EVENT_HANDLER->ON_FINISHED FOR GUI_TIMER.

    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.
  ENDIF.
ENDMODULE.                 " SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  checking_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM CHECKING_BATCH_JOB.
  DATA: L_BACKJOB LIKE  SY-REPID,
        LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.

  L_BACKJOB = SY-REPID.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
       EXPORTING
            ABAP_PROGRAM_NAME             = L_BACKJOB
            DIALOG                        = 'N'
            STATUS                        = 'R'
       TABLES
            JOBLIST                       = LT_JOBLIST
       EXCEPTIONS
            NO_JOBS_FOUND                 = 1
            PROGRAM_SPECIFICATION_MISSING = 2
            INVALID_DIALOG_TYPE           = 3
            JOB_FIND_CANCELED             = 4
            OTHERS                        = 5.

  IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S999(PP) WITH TEXT-M01.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE LT_JOBLIST INDEX 1.
    IF SY-SUBRC EQ 0.
      MESSAGE E999(PP) WITH TEXT-M01.
    ENDIF.
  ENDIF.

ENDFORM.                    " checking_batch_job
*&---------------------------------------------------------------------*
*&      Form  GET_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.


  DATA: LT_7JB_DATE LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE.

  DATA: LT_TEMP LIKE TABLE OF ZTMM_DAILY_SHORT WITH HEADER LINE.

  DATA:  L_INDEX LIKE SY-TABID,
         L_CN(2) TYPE N,
         L_CN_C(10),
         L_DEC(3),
         L_QTY_C(13),
         L_TEXT(20),
         L_REM_REC_DAY TYPE I,
         L_REC_DAY TYPE I.

  REFRESH: IT_SHORT.

  TIMEOUT_INTERVAL = P_INTRV * 60.

  SELECT * INTO TABLE LT_TEMP
     FROM ZTMM_DAILY_SHORT
     WHERE MATNR IN S_MATNR
       AND WERKS IN S_WERKS
       AND MTART IN S_MTART
       AND MATKL IN S_MATKL
       AND DISPO IN S_DISPO
       AND LGPRO IN S_LGPRO
       AND PRVBE IN S_PRVBE
       AND SORTF IN S_SORTF
       AND LIFNR IN S_LIFNR.
* by Daniel on 11/12/10 {
*  IF SY-SUBRC = 0.
  IF SY-SUBRC = 4.
    EXIT.
  ENDIF.
* }

  READ TABLE LT_TEMP INDEX 1.
*  W_BASE_DATE = LT_TEMP-DATUM.
  W_BASE_DATE = LT_TEMP-ZEDAT.
  W_BASE_TIME = LT_TEMP-ZETIM.

  PERFORM GET_DAY_WEEK.

  LOOP AT LT_TEMP.

    CLEAR: IT_SHORT, L_CN.

    MOVE-CORRESPONDING LT_TEMP TO IT_SHORT.
    SELECT SINGLE NAME1 INTO IT_SHORT-VEN_NAME
      FROM LFA1
      WHERE LIFNR = IT_SHORT-LIFNR.

    SELECT SINGLE DSNAM INTO IT_SHORT-DSNAM
       FROM T024D
      WHERE WERKS = 'P001'
      AND DISPO = IT_SHORT-DISPO.

    SELECT SINGLE MAKTX INTO IT_SHORT-MAKTX
      FROM MAKT
        WHERE MATNR = IT_SHORT-MATNR
          AND SPRAS = 'EN'.

    DESCRIBE TABLE IT_DAY LINES L_REC_DAY.

    IF IT_SHORT-HR_OH <= 0.
      IT_SHORT-LIGHTS = '1'.  "red
      L_QTY_C = IT_SHORT-HR_OH.
      SPLIT L_QTY_C AT '.' INTO L_CN_C L_DEC.

      IF L_CN_C <= L_REC_DAY.
        L_CN = L_CN_C + 1.
        WHILE L_CN <= L_REC_DAY.
          CONCATENATE 'D' L_CN INTO L_TEXT.
          PERFORM SET_CELL_COLOR USING '6'
                                       '1'
                                   L_TEXT
                                CHANGING IT_SHORT-COLOR[].
          L_CN = L_CN + 1.
          CLEAR: L_TEXT.

        ENDWHILE.
        L_CN = '01'.
        WHILE L_CN <= 18.
          CONCATENATE 'W' L_CN INTO L_TEXT.
          PERFORM SET_CELL_COLOR USING '6'
                                       '1'
                                   L_TEXT
                                CHANGING IT_SHORT-COLOR[].
          L_CN = L_CN + 1.
          CLEAR: L_TEXT.

        ENDWHILE.
      ELSE.
        L_REM_REC_DAY = L_CN_C - L_REC_DAY.
        L_REM_REC_DAY = L_REM_REC_DAY DIV 5.
        L_CN = L_REM_REC_DAY + 1.

        WHILE L_CN <= 18.
          CONCATENATE 'W' L_CN INTO L_TEXT.
          PERFORM SET_CELL_COLOR USING '6'
                                       '1'
                                   L_TEXT
                                CHANGING IT_SHORT-COLOR[].
          L_CN = L_CN + 1.
          CLEAR: L_TEXT.
        ENDWHILE.
      ENDIF.

      APPEND IT_SHORT.
      CONTINUE.
    ENDIF.

    CASE 'X'.
      WHEN PC_ALL.
        IF IT_SHORT-HR_OH <= P_RED1.
          IT_SHORT-LIGHTS = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '6'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          L_QTY_C = IT_SHORT-HR_OH.
          SPLIT L_QTY_C AT '.' INTO L_CN_C L_DEC.


*          L_CN = L_CN_C + 1.
*          WHILE L_CN <= 40.
*            CONCATENATE 'RP' L_CN INTO L_TEXT.
*            PERFORM SET_CELL_COLOR USING '6'
*                                         '1'
*                                     L_TEXT
*                                  CHANGING IT_SHORT-COLOR[].
*            L_CN = L_CN + 1.
*            CLEAR: L_TEXT.
*
*          ENDWHILE.
          IF L_CN_C <= L_REC_DAY.
            L_CN = L_CN_C + 1.
            WHILE L_CN <= L_REC_DAY.
              CONCATENATE 'D' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
            L_CN = '01'.
            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
          ELSE.
            L_REM_REC_DAY = L_CN_C - L_REC_DAY.
            L_REM_REC_DAY = L_REM_REC_DAY DIV 5.
            L_CN = L_REM_REC_DAY + 1.

            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.
            ENDWHILE.
          ENDIF.


          APPEND IT_SHORT.
        ENDIF.
        IF IT_SHORT-HR_OH <=  P_YEL1 AND IT_SHORT-HR_OH > P_RED1.
          IT_SHORT-LIGHTS = '2'.  "yellow
*          PERFORM SET_CELL_COLOR USING    '3'
*                                            '1'
*                                        'HR_OH'
*                                     CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          L_QTY_C = IT_SHORT-HR_OH.
          SPLIT L_QTY_C AT '.' INTO L_CN_C L_DEC.
*          L_CN = L_CN_C + 1.
*          WHILE L_CN <= 40.
*            CONCATENATE 'RP' L_CN INTO L_TEXT.
*            PERFORM SET_CELL_COLOR USING '6'
*                                         '1'
*                                     L_TEXT
*                                  CHANGING IT_SHORT-COLOR[].
*            L_CN = L_CN + 1.
*            CLEAR: L_TEXT.
*
*          ENDWHILE.
          IF L_CN_C <= L_REC_DAY.
            L_CN = L_CN_C + 1.
            WHILE L_CN <= L_REC_DAY.
              CONCATENATE 'D' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
            L_CN = '01'.
            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
          ELSE.
            L_REM_REC_DAY = L_CN_C - L_REC_DAY.
            L_REM_REC_DAY = L_REM_REC_DAY DIV 5.
            L_CN = L_REM_REC_DAY + 1.

            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.
            ENDWHILE.
          ENDIF.

          APPEND IT_SHORT.
        ENDIF.

        IF IT_SHORT-HR_OH <= P_GRE1 AND IT_SHORT-HR_OH > P_YEL1 .
          IT_SHORT-LIGHTS = '3'.  "green
*          PERFORM SET_CELL_COLOR USING    '5'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          L_QTY_C = IT_SHORT-HR_OH.
          SPLIT L_QTY_C AT '.' INTO L_CN_C L_DEC.
*          L_CN = L_CN_C + 1.
*          WHILE L_CN <= 40.
*            CONCATENATE 'RP' L_CN INTO L_TEXT.
*            PERFORM SET_CELL_COLOR USING '6'
*                                         '1'
*                                     L_TEXT
*                                  CHANGING IT_SHORT-COLOR[].
*            L_CN = L_CN + 1.
*            CLEAR: L_TEXT.
*
*          ENDWHILE.
          IF L_CN_C <= L_REC_DAY.
            L_CN = L_CN_C + 1.
            WHILE L_CN <= L_REC_DAY.
              CONCATENATE 'D' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
            L_CN = '01'.
            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
          ELSE.
            L_REM_REC_DAY = L_CN_C - L_REC_DAY.
            L_REM_REC_DAY = L_REM_REC_DAY DIV 5.
            L_CN = L_REM_REC_DAY + 1.

            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.
            ENDWHILE.
          ENDIF.

          APPEND IT_SHORT.
        ENDIF.

      WHEN PC_RED.
        IF IT_SHORT-HR_OH <= P_RED1.
          IT_SHORT-LIGHTS = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '6'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          L_QTY_C = IT_SHORT-HR_OH.
          SPLIT L_QTY_C AT '.' INTO L_CN_C L_DEC.
*          L_CN = L_CN_C + 1.
*          WHILE L_CN <= 40.
*            CONCATENATE 'RP' L_CN INTO L_TEXT.
*            PERFORM SET_CELL_COLOR USING '6'
*                                         '1'
*                                     L_TEXT
*                                  CHANGING IT_SHORT-COLOR[].
*            L_CN = L_CN + 1.
*            CLEAR: L_TEXT.
*          ENDWHILE.
          IF L_CN_C <= L_REC_DAY.
            L_CN = L_CN_C + 1.
            WHILE L_CN <= L_REC_DAY.
              CONCATENATE 'D' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
            L_CN = '01'.
            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
          ELSE.
            L_REM_REC_DAY = L_CN_C - L_REC_DAY.
            L_REM_REC_DAY = L_REM_REC_DAY DIV 5.
            L_CN = L_REM_REC_DAY + 1.

            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.
            ENDWHILE.
          ENDIF.

          APPEND IT_SHORT.
        ENDIF.
      WHEN PC_YEL.
        IF IT_SHORT-HR_OH <= P_YEL1.
          IT_SHORT-LIGHTS = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '3'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          L_QTY_C = IT_SHORT-HR_OH.
          SPLIT L_QTY_C AT '.' INTO L_CN_C L_DEC.

*          L_CN = L_CN_C + 1.
*          WHILE L_CN <= 40.
*            CONCATENATE 'RP' L_CN INTO L_TEXT.
*            PERFORM SET_CELL_COLOR USING '6'
*                                         '1'
*                                     L_TEXT
*                                  CHANGING IT_SHORT-COLOR[].
*            L_CN = L_CN + 1.
*            CLEAR: L_TEXT.
*          ENDWHILE.
          IF L_CN_C <= L_REC_DAY.
            L_CN = L_CN_C + 1.
            WHILE L_CN <= L_REC_DAY.
              CONCATENATE 'D' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
            L_CN = '01'.
            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
          ELSE.
            L_REM_REC_DAY = L_CN_C - L_REC_DAY.
            L_REM_REC_DAY = L_REM_REC_DAY DIV 5.
            L_CN = L_REM_REC_DAY + 1.

            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.
            ENDWHILE.
          ENDIF.

          APPEND IT_SHORT.
        ENDIF.

      WHEN PC_GRE.
        IF IT_SHORT-HR_OH <= P_GRE1.
          IT_SHORT-LIGHTS = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '5'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          L_QTY_C = IT_SHORT-HR_OH.
          SPLIT L_QTY_C AT '.' INTO L_CN_C L_DEC.
*          L_CN = L_CN_C + 1.
*          WHILE L_CN <= 40.
*            CONCATENATE 'RP' L_CN INTO L_TEXT.
*            PERFORM SET_CELL_COLOR USING '6'
*                                         '1'
*                                     L_TEXT
*                                  CHANGING IT_SHORT-COLOR[].
*            L_CN = L_CN + 1.
*            CLEAR: L_TEXT.
*          ENDWHILE.
          IF L_CN_C <= L_REC_DAY.
            L_CN = L_CN_C + 1.
            WHILE L_CN <= L_REC_DAY.
              CONCATENATE 'D' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
            L_CN = '01'.
            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.

            ENDWHILE.
          ELSE.
            L_REM_REC_DAY = L_CN_C - L_REC_DAY.
            L_REM_REC_DAY = L_REM_REC_DAY DIV 5.
            L_CN = L_REM_REC_DAY + 1.

            WHILE L_CN <= 18.
              CONCATENATE 'W' L_CN INTO L_TEXT.
              PERFORM SET_CELL_COLOR USING '6'
                                           '1'
                                       L_TEXT
                                    CHANGING IT_SHORT-COLOR[].
              L_CN = L_CN + 1.
              CLEAR: L_TEXT.
            ENDWHILE.
          ENDIF.

          APPEND IT_SHORT.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.

*  SORT IT_SHORT BY HR_OH MATNR D01.
  IF P_TOPB = 'X' AND P_TOPV > 0.
    L_INDEX = P_TOPV + 1.
    DELETE IT_SHORT FROM L_INDEX.
  ENDIF.

  IF P_LESB = 'X' AND P_GRTB = 'X'.
    DELETE IT_SHORT WHERE HR_OH >= P_LESV
                      OR HR_OH <= P_GRTV.
  ELSE.
    IF  P_LESB = 'X'.
      DELETE IT_SHORT WHERE HR_OH >= P_LESV.
    ENDIF.
    IF P_GRTB = 'X' .
      DELETE IT_SHORT WHERE HR_OH <= P_GRTV.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_data
*&---------------------------------------------------------------------*
*&      Form  set_cell_color
*&---------------------------------------------------------------------*
*       Set Cell Color
*----------------------------------------------------------------------*
FORM SET_CELL_COLOR  USING    U_COL
                              U_INT
                              U_FIELD
                     CHANGING COLOR_TAB
                              TYPE SLIS_T_SPECIALCOL_ALV.
*----------------------------------------------------------------------*
* No  Colour
*  0  COL_BACKGROUND
*  1  COL_HEADING
*  2  COL_NORMAL
*  3  COL_TOTAL
*  4  COL_KEY
*  5  COL_POSITIVE
*  6  COL_NEGATIVE
*  7  COL_GROUP
*----------------------------------------------------------------------*
  DATA : L_COLOR TYPE SLIS_SPECIALCOL_ALV.
  L_COLOR-FIELDNAME = U_FIELD.
  L_COLOR-COLOR-COL = U_COL.
  L_COLOR-COLOR-INT = U_INT.
  APPEND L_COLOR TO COLOR_TAB.
ENDFORM.                    " set_cell_color

*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'ST800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_day_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DAY_WEEK.

  DATA: L_COUNT TYPE I,
        L_CN(2) TYPE N.
  DATA: L_DATE LIKE SY-DATUM.

  DATA: LT_7JB_DATE LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE.

  SELECT SINGLE KALID INTO W_KALID
  FROM ZVPP_CAPACITY
   WHERE ARBPL = 'T'.

  IT_DAY-SEQ = 1.
** Changed by Furong on 03/17/10
*   IT_DAY-DATUM = SY-DATUM.
  IT_DAY-DATUM = W_BASE_DATE.
** End of change

  APPEND IT_DAY.
  L_COUNT = '01'.
** Changed by Furong on 03/17/10
*  L_DATE = SY-DATUM.
  L_DATE = W_BASE_DATE.
** End of change

  CLEAR: W_MAX_DATE.
  SELECT MAX( SQDT ) INTO W_MAX_DATE
    FROM ZTPP_PMT07JB_A
     WHERE GUBB = 'A'.


  WHILE L_DATE < W_MAX_DATE.
*  DO 20 TIMES.
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+' W_KALID  L_DATE.
    IT_DAY-SEQ     = L_COUNT.
    IT_DAY-DATUM   = L_DATE .
    APPEND IT_DAY.  CLEAR: IT_DAY.
*  ENDDO.
  ENDWHILE.

*  DO 20 TIMES.
*    L_COUNT  = L_COUNT + 1.
*    L_DATE   = L_DATE  + 1.
*    PERFORM READ_WORKING_DATE USING '+' W_KALID  L_DATE.
*    IT_DAY-SEQ     = L_COUNT.
*    IT_DAY-DATUM   = L_DATE .
*    APPEND IT_DAY.  CLEAR: IT_DAY.
*  ENDDO.

  SELECT * INTO TABLE LT_7JB_DATE
    FROM ZTPP_PMT07JB_A
   WHERE GUBB EQ 'B'.

  SORT LT_7JB_DATE BY SQDT.
  DELETE ADJACENT DUPLICATES FROM LT_7JB_DATE COMPARING SQDT.

  L_CN = '00'.
  LOOP AT LT_7JB_DATE.
    L_CN = L_CN + 1.
    IT_WEEK-DATUM = LT_7JB_DATE-SQDT.
    IT_WEEK-SEQ = L_CN.
    APPEND IT_WEEK.
  ENDLOOP.

ENDFORM.                    " get_day_week
*---------------------------------------------------------------------*
*       FORM read_working_date                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_TYPE                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WDATE                                                      *
*---------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = PA_TYPE
            DATE                         = PA_WDATE
            FACTORY_CALENDAR_ID          = PA_KALID
       IMPORTING
            DATE                         = PA_WDATE
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
