************************************************************************
* Program Name      : ZMMR_PARTS_SHORTAGE_DAILY
* Author            : Furong Wang
* Creation Date     : 05/20//2010
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZMMR_PARTS_SHORTAGE_DAILY MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TABLES: MARA, ZTMM_DAILY_SHORT.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF IT_SHORT OCCURS 0.
        INCLUDE STRUCTURE ZTMM_DAILY_SHORT.
DATA: END OF IT_SHORT.

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
      GRID_CONTAINER_800    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

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
      W_MAX_DATE LIKE SY-DATUM.

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

*SELECTION-SCREEN BEGIN OF BLOCK BLOCK7 WITH FRAME TITLE TEXT-007.
*PARAMETERS :
*    P_HRLY AS CHECKBOX DEFAULT 'X',
*    P_STK AS CHECKBOX,
*    P_ALL  AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK BLOCK7.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS:   P_WERKS   TYPE MARC-WERKS MEMORY ID WRK OBLIGATORY
*DEFAULT
* 'P001'.
SELECT-OPTIONS :
    S_MATNR   FOR ZTMM_DAILY_SHORT-MATNR MODIF ID REQ,
    S_MTART   FOR ZTMM_DAILY_SHORT-MTART MODIF ID REQ,
    S_MATKL   FOR MARA-MATKL MODIF ID REQ,
    S_DISPO   FOR ZTMM_DAILY_SHORT-DISPO MODIF ID REQ,   "MRP controller
    S_LGPRO   FOR ZTMM_DAILY_SHORT-LGPRO MODIF ID REQ,
    S_PRVBE   FOR ZTMM_DAILY_SHORT-PRVBE MODIF ID REQ,
    S_SORTF   FOR ZTMM_DAILY_SHORT-SORTF MODIF ID REQ,
    S_LIFNR   FOR ZTMM_DAILY_SHORT-LIFNR MODIF ID REQ.
SELECTION-SCREEN END OF BLOCK B1.

*SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 3.
*PARAMETERS:  P_NON RADIOBUTTON GROUP GRP MODIF ID REQ.
*SELECTION-SCREEN COMMENT 5(5)  TEXT-M15  FOR FIELD P_NON.
*PARAMETERS:  P_VST RADIOBUTTON GROUP GRP MODIF ID REQ.
*SELECTION-SCREEN COMMENT 15(10)  TEXT-M11  FOR FIELD P_VST.
*PARAMETERS:  P_VSCH RADIOBUTTON GROUP GRP MODIF ID REQ.  "  AS CHECKBOX
*.
*SELECTION-SCREEN COMMENT 35(12) TEXT-M12  FOR FIELD P_VSCH.
*PARAMETERS   P_REQ RADIOBUTTON GROUP GRP MODIF ID REQ. "  AS CHECKBOX.
*SELECTION-SCREEN COMMENT 50(10) TEXT-M13  FOR FIELD P_REQ.
*PARAMETERS   P_REP RADIOBUTTON GROUP GRP MODIF ID REQ.   "AS CHECKBOX.
*SELECTION-SCREEN COMMENT 62(10) TEXT-M14  FOR FIELD P_REP.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : P_SAVE AS CHECKBOX.
SELECTION-SCREEN COMMENT 8(12) TEXT-M16 FOR FIELD P_SAVE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B5.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK6 WITH FRAME TITLE TEXT-006.
PARAMETERS :
    P_INTRV TYPE I DEFAULT '10' MODIF ID REQ.
*    p_clock TYPE rlmon-clock.
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

  PERFORM CHECKING_BATCH_JOB.

  PERFORM GET_DATA.
  IF P_SAVE = 'X'.
    PERFORM SAVE_TO_TABLE.
  ENDIF.

  IF SY-BATCH = ' '.
    CALL SCREEN 0800.
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
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE USER_COMMAND_0200 INPUT.
*  W_CODE = OK_CODE.
*  CASE OK_CODE.
*    WHEN 'BACK'.
*      LEAVE TO SCREEN 0.
**      LEAVE PROGRAM.
*    WHEN 'EXIT'.
*      LEAVE PROGRAM.
*
*  ENDCASE.
*ENDMODULE.                 " USER_COMMAND_0200  INPUT
**---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*FORM ASSIGN_ITAB_TO_ALV.
*
*  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*
*   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
*               I_SAVE           = WA_SAVE
*               IS_VARIANT       = WA_VARIANT
*               I_DEFAULT        = SPACE
**               it_toolbar_excluding = IT_EXCLUDE[]
*     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*               IT_OUTTAB        = IT_INPUT_PLAN[]
*               IT_SORT          = IT_SORT[].
*
*** ENTER
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
** Cursor----
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
**  CREATE OBJECT G_EVENT_RECEIVER.
**  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
**  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.
*
*  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.
*
*ENDFORM.                    " assign_itab1_to_alv
*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*FORM SET_ATTRIBUTES_ALV_GRID.
*  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
*
**//-- Set Layout Structure
**  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
*  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
*  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
**  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
**  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
**  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*
**  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
**  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*  CONCATENATE 'Screen ' SY-DYNNR INTO WA_IS_LAYOUT-GRID_TITLE
*  SEPARATED BY SPACE.
*
**//-- Set Variant Structure
*  WA_VARIANT-REPORT       = SY-REPID.
*  WA_VARIANT-USERNAME     = SY-UNAME.
*ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*FORM BUILD_SORTCAT_DISPLAY.
*
*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'STATUS'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.
*
*ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
*FORM BUILD_FIELD_CATALOG USING P_ITAB.

*  DATA: LW_ITAB TYPE SLIS_TABNAME.
**        lw_waers LIKE t001-waers,
*
*  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
*         IT_FIELDNAME, IT_FIELDNAME[].
*  CLEAR: W_REPID.
*
*  LW_ITAB = P_ITAB.
*
*  W_REPID = SY-REPID.
*
**
** CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
**    EXPORTING
***      I_INTERNAL_TABNAME     = LW_ITAB
**      I_STRUCTURE_NAME       = 'ZTPP_INPUTPLAN_H'
**    CHANGING
**      CT_FIELDCAT            = IT_FIELDCAT[]
**    EXCEPTIONS
**      INCONSISTENT_INTERFACE = 1
**      PROGRAM_ERROR          = 2
**      OTHERS                 = 3.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*       EXPORTING
*            I_PROGRAM_NAME     = W_REPID
*            I_INTERNAL_TABNAME = LW_ITAB
*            I_INCLNAME         = W_REPID
*       CHANGING
*            CT_FIELDCAT        = IT_FIELDNAME.
*
*  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
*
*                                  'S' 'STATUS'    ' ',
*                                  ' ' 'KEY'         'X',
*                                  ' ' 'COLTEXT'     'RP',
*                                  'E' 'OUTPUTLEN'   '4',
*
*
*                                  'S' 'MODL'    ' ',
*                                  ' ' 'KEY'         'X',
*                                  ' ' 'COLTEXT'     'Model',
*                                  'E' 'OUTPUTLEN'   '5',
*
*
*                                  'S' 'BODY_SER'     ' ',
*                                  ' ' 'COLTEXT'     'Body No',
*                                  'E' 'OUTPUTLEN'   '12',
**
**                                  'S' 'RPCUR'     ' ',
**                                  ' ' 'COLTEXT'     'Time Stamp',
**                                  'E' 'OUTPUTLEN'   '18',
*
*                                  'S' 'SERIAL'    ' ',
**                                 ' ' 'KEY'         'X',
*                                  ' ' 'COLTEXT'     'Serial No',
*                                  'E' 'OUTPUTLEN'   '10'.
*
*
**                                  'S' 'QTY'    ' ',
**                                  ' ' 'COLTEXT'     'Quantity',
**                                  'E' 'OUTPUTLEN'   '10',
**
**
**                                  'S' 'MESS'       ' ',
**                                  ' ' 'COLTEXT'     'Error Message',
**                                  'E' 'OUTPUTLEN'   '255'.
***
*
*ENDFORM.
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
  IF GRID_CONTAINER_800 IS INITIAL.
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
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER_800
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL_800
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
  CREATE OBJECT ALV_GRID_800
         EXPORTING I_PARENT = GRID_CONTAINER_800
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
        L_TIME(8),
        L_UPH_C(6).
  .

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
 CONCATENATE W_BASE_DATE+4(2) '/' W_BASE_DATE+6(2) '/' W_BASE_DATE+0(4)
                                                          INTO L_DATE_C.
* CONCATENATE W_BASE_TIME+0(2) ':' W_BASE_TIME+2(2) ':' W_BASE_TIME+4(2)
*                                                            INTO L_TIME
*.
*
*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-CTAB_FNAME  = 'COLOR'.
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.
  CONCATENATE 'As of' L_DATE_C L_TIME INTO WA_IS_LAYOUT-GRID_TITLE
 SEPARATED BY SPACE.
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
                                  ' ' 'COLTEXT'     'Part',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'WERKS'    ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '4',

                                 'S' 'MAKTX'    ' ',
                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                   'S' 'HR_OH'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'COLTEXT'     'On Hand',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'LIFNR'    ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '18',

                                'S' 'SORTF'    ' ',
                                  ' ' 'COLTEXT'     'RP',
                                  'E' 'OUTPUTLEN'   '3',

                                 'S' 'PRVBE'    ' ',
                                 ' ' 'COLTEXT'     'Supp Area',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'DSNAM'    ' ',
                                  ' ' 'COLTEXT'     'MRP Controller',
                                  'E' 'OUTPUTLEN'   '18',

                                'S' 'MATKL'    ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'OHQTY'    ' ',
                                 ' ' 'COLTEXT'     'Stock',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '13'.

  LOOP AT IT_DAY.
    CONCATENATE 'D' IT_DAY-SEQ INTO L_RP.
    WRITE IT_DAY-DATUM TO L_DAY DD/MM/YYYY .

    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                    'S' L_RP        ' ',
                                    ' ' 'COLTEXT'     L_DAY,
                                    ' ' 'DECIMALS_O'  '0',
                                    'E' 'OUTPUTLEN'   '10'.
    CLEAR: L_RP, L_DAY .

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


* L_CN = '00'.
*  DO 18 TIMES.
*    L_CN = L_CN + 1.
*
*    READ TABLE IT_WEEK WITH KEY SEQ = L_CN.
*
*    CONCATENATE 'W' L_CN INTO L_RP.
*
*    WRITE IT_WEEK-DATUM TO L_DATUM MM/DD/YY.
*
*    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
*
*                                   'S' L_RP        ' ',
*                                   ' ' 'COLTEXT'     L_DATUM,
*                                   ' ' 'DECIMALS_O'  '0',
*                                   'E' 'OUTPUTLEN'   '7'.
*    CLEAR: L_RP, L_DATUM.
*  ENDDO.



  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                    'S' 'TOTAL'      ' ',
                                    ' ' 'COLTEXT'     'Total',
                                    ' ' 'DECIMALS_O'  '0',
                                    'E' 'OUTPUTLEN'   '10'.

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
               I_DEFAULT        = SPACE
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
      SUBMIT ZMMR_PARTS_SHORTAGE
       WITH S_MATNR IN S_MATNR
       WITH S_MTART IN S_MTART
       WITH S_MATKL IN S_MATKL
       WITH S_DISPO IN S_DISPO
       WITH S_LGPRO IN S_LGPRO
       WITH S_PRVBE IN S_PRVBE
       WITH S_SORTF IN S_SORTF
       WITH S_LIFNR IN S_LIFNR.
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
  DATA: L_SEQ(5) TYPE N.

  L_SEQ = '00001'.
  LOOP AT IT_SHORT.
    IT_SHORT-SEQ = L_SEQ.
    MODIFY IT_SHORT.
    L_SEQ = L_SEQ + 1.
  ENDLOOP.

  DELETE FROM ZTMM_DAILY_SHORT CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  INSERT ZTMM_DAILY_SHORT FROM TABLE IT_SHORT.
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
  DATA: L_CN(2) TYPE N,
        L_IN_CN(2) TYPE N,
         L_OHQTY LIKE ZMMS_SHORT-RP01,
         L_QTY LIKE ZMMS_SHORT-RP01,
         L_DAY_QTY LIKE ZMMS_SHORT-RP01,
         L_TOTAL LIKE ZMMS_SHORT-RP01,
         L_DAYS LIKE ZMMS_SHORT-RP01,
         L_FLAG(1),
         L_TEXT(40),
         L_TEXT_SHORT(40),
         L_INDEX LIKE SY-TABIX,
         L_LGPRO LIKE MARC-LGPRO,
         L_FIRST_WEEK  LIKE SY-DATUM,
         L_FDAY_WEEK LIKE SY-DATUM,
         L_LDAY_WEEK LIKE SY-DATUM,
         L_QTY_PER_DAY_INT TYPE I,
         L_QTY_REM TYPE I,
         L_LGORT LIKE MARD-LGORT,
         L_LABST_P001 LIKE MARD-LABST,
         L_LABST_9999 LIKE MARD-LABST,
         L_LABST_E001 LIKE MARD-LABST,
* by ig.moon - Engine Plant Split {
         L_LABST_E002 LIKE MARD-LABST,
* }
         L_MAX_TIMES TYPE I,
         L_ACT_WEEKS TYPE I.

  DATA: BEGIN OF LT_MARD OCCURS 0,
        MATNR LIKE MARD-MATNR,
        WERKS LIKE MARD-WERKS,
        LGORT LIKE MARD-LGORT,
        LABST LIKE MARD-LABST,
        END OF LT_MARD.
  FIELD-SYMBOLS: <FS_SHORT>,
                 <FS_DATA>.

  DATA: BEGIN OF LT_21DAY OCCURS 0.
          INCLUDE STRUCTURE ZTMM_PARTS_21DAY.
  DATA: MTART LIKE MARA-MTART,
        MATKL LIKE MARA-MATKL,
        END OF LT_21DAY.

  DATA: LT_7JB_DATE LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE.
*  DATA: BEGIN OF LT_7JB_DATE OCCURS 0,
**      SEQ(2)  TYPE N,
*        SQDT,
*        END OF LT_7JB_DATE.

  DATA:  BEGIN OF LT_MDSM OCCURS 0,
           WERKS LIKE MARC-WERKS,
           MATNR LIKE MARA-MATNR,
           BDTER LIKE MDSM-BDTER,
           BDMNG LIKE MDSM-BDMNG,
           SBNUM LIKE MDSM-SBNUM,
           SBPOS LIKE MDSM-SBPOS,
         END OF LT_MDSM.

  DATA: LT_MDSM_TEMP LIKE TABLE OF LT_MDSM WITH HEADER LINE.
** Get 21 day
  REFRESH: IT_SHORT.
  SELECT WERKS A~MATNR DATUM DISPO A~PROFL LGORT LIFNR SHOP PRVBE
         SORTF A~MEINS D01 D02 D03 D04 D05 D06 D07 D08 D09 D10
         D11 D12 D13 D14 D15 D16 D17 D18 D19 D20 D21 MTART MATKL
         ERDAT ERZET
         INTO CORRESPONDING FIELDS OF TABLE LT_21DAY
    FROM ZTMM_PARTS_21DAY AS A
    INNER JOIN MARA AS B
    ON A~MATNR = B~MATNR
     WHERE A~MATNR IN S_MATNR
       AND B~MTART IN S_MTART
       AND B~MATKL IN S_MATKL
       AND DISPO IN S_DISPO
       AND LGORT IN S_LGPRO
       AND PRVBE IN S_PRVBE
       AND SORTF IN S_SORTF
       AND LIFNR IN S_LIFNR
       AND LGORT <> 'P500'.

  READ TABLE LT_21DAY INDEX 1.
  W_BASE_DATE = LT_21DAY-DATUM.
*  W_BASE_TIME = LT_21DAY-ERZET.

  CLEAR: W_MAX_DATE.
  SELECT MAX( SQDT ) INTO W_MAX_DATE
    FROM ZTPP_PMT07JB_A
     WHERE GUBB = 'A'.

  PERFORM GET_DAY_WEEK.

** get 18 week

  REFRESH LT_7JB_DATE.

  SELECT * INTO TABLE LT_7JB_DATE
    FROM ZTPP_PMT07JB_A
   WHERE GUBB EQ 'B'.

  SORT LT_7JB_DATE BY SQDT.
  DELETE ADJACENT DUPLICATES FROM LT_7JB_DATE COMPARING SQDT.
  READ TABLE LT_7JB_DATE INDEX 1.
  L_FIRST_WEEK  = LT_7JB_DATE-SQDT.

  L_CN = '00'.
  LOOP AT LT_7JB_DATE.
    L_CN = L_CN + 1.
    IT_WEEK-DATUM = LT_7JB_DATE-SQDT.
    IT_WEEK-SEQ = L_CN.
    APPEND IT_WEEK.
  ENDLOOP.

  L_ACT_WEEKS = L_CN.

  IF LT_21DAY[] IS INITIAL.
  ELSE.
    SELECT  MATNR WERKS BDTER BDMNG SBNUM SBPOS
           INTO CORRESPONDING FIELDS OF TABLE LT_MDSM_TEMP
         FROM MDSM
         FOR ALL ENTRIES IN LT_21DAY
         WHERE MATNR = LT_21DAY-MATNR
            AND WERKS = LT_21DAY-WERKS
            AND PLSCN = '900'
            AND BDTER >= L_FIRST_WEEK.
  ENDIF.

  LOOP AT LT_MDSM_TEMP.
    LT_MDSM = LT_MDSM_TEMP.
    LT_MDSM-SBNUM = ''.
    LT_MDSM-SBPOS = ''.
    COLLECT LT_MDSM.
    CLEAR: LT_MDSM, LT_MDSM_TEMP.
  ENDLOOP.
  SORT LT_MDSM BY WERKS MATNR BDTER.

*  CONCATENATE '%' '999' INTO L_LGORT.

  LOOP AT LT_21DAY.
    MOVE-CORRESPONDING LT_21DAY TO IT_SHORT.
    IT_SHORT-LGPRO = LT_21DAY-LGORT.


** Changed on 07/13/11

    SELECT SINGLE SUM( LABST ) INTO L_LABST_P001
     FROM MARD
        WHERE MATNR =  IT_SHORT-MATNR
           AND WERKS = 'P001'
           AND ( LGORT <> '9999' AND
                 LGORT <> 'P999' AND
                 LGORT <> 'P998' AND
                 LGORT <> 'G999' AND
                 LGORT <> 'G998' )
           GROUP BY MATNR WERKS.

    SELECT SINGLE SUM( LABST ) INTO L_LABST_E001
          FROM MARD
          WHERE MATNR =  IT_SHORT-MATNR
             AND WERKS = 'E001'
             AND ( LGORT <> 'E999' AND
                  LGORT <> 'E998' )
*          GROUP BY MATNR WERKS LGORT.
             GROUP BY MATNR WERKS.

* by ig.moon - Engine Plant Split {
*    IT_SHORT-OHQTY = L_LABST_P001 + L_LABST_E001.

    SELECT SINGLE SUM( LABST ) INTO L_LABST_E002
          FROM MARD
          WHERE MATNR =  IT_SHORT-MATNR
             AND WERKS = 'E002'
             AND ( LGORT <> 'N999' AND
                  LGORT <> 'N998' )
             GROUP BY MATNR WERKS.

    IT_SHORT-OHQTY = L_LABST_P001 + L_LABST_E001 + L_LABST_E002.
* }

*    SELECT SINGLE SUM( LABST ) INTO L_LABST_P001
*    FROM MARD
*    WHERE MATNR =  IT_SHORT-MATNR
*       AND WERKS = 'P001'
**       AND LGORT = 'P400'
*       GROUP BY MATNR WERKS.
*
*    SELECT SINGLE SUM( LABST ) INTO L_LABST_9999
*      FROM MARD
*      WHERE MATNR =  IT_SHORT-MATNR
*         AND WERKS = 'P001'
*         AND ( LGORT = '9999' OR LGORT = 'P999' )
*         GROUP BY MATNR WERKS.
*
*    SELECT SINGLE SUM( LABST ) INTO L_LABST_E001
*        FROM MARD
*        WHERE MATNR =  IT_SHORT-MATNR
*           AND WERKS = 'E001'
*           AND LGORT <> 'E999'
**           GROUP BY MATNR WERKS LGORT.
*           GROUP BY MATNR WERKS.
*
*    IT_SHORT-OHQTY = L_LABST_P001 - L_LABST_9999 + L_LABST_E001.

** END ON 07/13/11

*    SELECT SINGLE SUM( LABST ) INTO IT_SHORT-OHQTY
*       FROM MARD
*       WHERE MATNR =  IT_SHORT-MATNR
**         AND WERKS = IT_SHORT-WERKS
**         AND LGORT = IT_SHORT-LGPRO
*         AND not LGORT LIKE L_LGORT
**         AND LGORT <> '9999'
**       GROUP BY MATNR WERKS LGORT.
*        group by matnr.

    L_OHQTY = IT_SHORT-OHQTY.
    L_DAY_QTY = IT_SHORT-OHQTY.

    CLEAR: L_TOTAL.
    L_CN = '00'.
    CLEAR: L_FLAG, L_DAYS.
*    DO 21 TIMES.
*      L_CN = L_CN + 1.
*      CONCATENATE 'IT_SHORT-D' L_CN INTO L_TEXT_SHORT.
*      ASSIGN (L_TEXT_SHORT) TO <FS_SHORT>.
*      CONCATENATE 'LT_21DAY-D' L_CN INTO L_TEXT.
*      ASSIGN (L_TEXT) TO <FS_DATA>.
*
*      <FS_SHORT> = <FS_DATA>.
*      MOVE <FS_SHORT> TO L_QTY.
*      L_TOTAL = L_TOTAL + L_QTY.
*
*      L_OHQTY = L_OHQTY - L_QTY.
*
*      IF L_FLAG IS INITIAL AND L_DAY_QTY > 0.
*        IF L_OHQTY >= 0.
*          L_DAYS = L_DAYS + 1.
*        ELSEIF  L_QTY <> 0.
*          L_FLAG = 'X'.
*          L_OHQTY = L_OHQTY + L_QTY.
*          L_DAYS = L_DAYS + L_OHQTY / L_QTY.
*        ENDIF.
*      ENDIF.
*    ENDDO.

    LOOP AT IT_DAY.
*      L_CN = L_CN + 1.
      CONCATENATE 'IT_SHORT-D' IT_DAY-SEQ INTO L_TEXT_SHORT.
      ASSIGN (L_TEXT_SHORT) TO <FS_SHORT>.
      CONCATENATE 'LT_21DAY-D' IT_DAY-SEQ INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS_DATA>.

      <FS_SHORT> = <FS_DATA>.
      MOVE <FS_SHORT> TO L_QTY.
      L_TOTAL = L_TOTAL + L_QTY.

      L_OHQTY = L_OHQTY - L_QTY.

      IF L_FLAG IS INITIAL.  " AND L_DAY_QTY > 0.
        IF L_OHQTY >= 0.
          L_DAYS = L_DAYS + 1.
        ELSEIF  L_QTY <> 0.
          L_FLAG = 'X'.
          L_OHQTY = L_OHQTY + L_QTY.
          L_DAYS = L_DAYS + L_OHQTY / L_QTY.
        ENDIF.
      ENDIF.
    ENDLOOP.

    L_CN = '00'.

** Changed by Furong on 07/20/10
    L_MAX_TIMES = L_DAYS + L_ACT_WEEKS * 5.
    DO L_MAX_TIMES TIMES.
*    DO 18 TIMES.
** End of change
      L_CN = L_CN + 1.
      READ TABLE IT_WEEK WITH KEY SEQ = L_CN.
      L_FDAY_WEEK = IT_WEEK-DATUM.
*      IF L_CN = 21.
*        L_LDAY_WEEK = L_FDAY_WEEK + 6.
*      ELSE.
*        L_IN_CN = L_CN + 1.
*        READ TABLE IT_WEEK WITH KEY SEQ = L_IN_CN.
*        L_LDAY_WEEK = IT_WEEK-DATUM - 1.
*      ENDIF.
      CONCATENATE 'IT_SHORT-W' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS_DATA>.
      IF SY-SUBRC = 0.
        LOOP AT LT_MDSM WHERE WERKS = IT_SHORT-WERKS
                  AND MATNR = IT_SHORT-MATNR
*                  AND ( BDTER BETWEEN L_FDAY_WEEK AND L_LDAY_WEEK ).
                  AND BDTER = L_FDAY_WEEK.
          <FS_DATA> = <FS_DATA> + LT_MDSM-BDMNG.
        ENDLOOP.
        MOVE <FS_DATA> TO L_QTY.

        L_TOTAL = L_TOTAL + L_QTY.

        L_OHQTY = L_OHQTY - L_QTY.

*        IF L_FLAG IS INITIAL AND L_DAY_QTY > 0.
        IF L_FLAG IS INITIAL.  " AND L_DAY_QTY > 0.
          IF L_OHQTY >= 0.
            L_DAYS = L_DAYS + 5.
            IF  L_DAYS > L_MAX_TIMES.
              L_DAYS = L_MAX_TIMES.
            ENDIF.
*            L_FLAG = 'X'.
*            L_OHQTY = L_OHQTY + L_QTY.
*            L_QTY_PER_DAY_INT  = L_QTY DIV 5.
*            L_QTY_REM = L_QTY MOD 5.
*
*            L_QTY = L_QTY_PER_DAY_INT + L_QTY_REM.
*            DO 5 TIMES.
*              L_OHQTY = L_OHQTY - L_QTY.
*              IF L_OHQTY >= 0.
*                L_DAYS = L_DAYS + 1.
*              ELSE.
*                L_DAYS = L_DAYS + L_OHQTY / L_QTY.
*                EXIT.
*              ENDIF.
*              L_QTY = L_QTY_PER_DAY_INT.
*            ENDDO.
*
*          ENDIF.
          ELSEIF  L_QTY <> 0.
            L_FLAG = 'X'.
            L_OHQTY = L_OHQTY + L_QTY.
            L_DAYS = L_DAYS + L_OHQTY / L_QTY * 5.
            IF  L_DAYS > L_MAX_TIMES.
              L_DAYS = L_MAX_TIMES.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
    IF L_DAYS < 0.
      CLEAR: L_DAYS.
    ENDIF.

    IT_SHORT-HR_OH = L_DAYS.

    IT_SHORT-TOTAL = L_TOTAL.

    IT_SHORT-ZSDAT = W_BASE_DATE.
    IT_SHORT-ZSTIM = W_BASE_TIME.
    IT_SHORT-ZEDAT = SY-DATUM.
    IT_SHORT-ZETIM = SY-UZEIT.

    APPEND IT_SHORT.

* by ig.moon - Engine Plant Split {
*    CLEAR: IT_SHORT, L_LABST_P001, L_LABST_E001, L_LABST_9999.
    CLEAR: IT_SHORT, L_LABST_P001, L_LABST_E001, L_LABST_E002,
           L_LABST_9999.
* }

  ENDLOOP.

  SORT IT_SHORT BY HR_OH ASCENDING
                TOTAL DESCENDING
                D01 DESCENDING
                MATNR.

ENDFORM.                    " GET_data
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

  DATA: L_COUNT TYPE I.
  DATA: L_DATE LIKE SY-DATUM.

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

  WHILE L_DATE < W_MAX_DATE AND L_COUNT < 21.
*  DO 20 TIMES.
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+' W_KALID  L_DATE.
    IT_DAY-SEQ     = L_COUNT.
    IT_DAY-DATUM   = L_DATE .
    APPEND IT_DAY.  CLEAR: IT_DAY.
*  ENDDO.
  ENDWHILE.

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
