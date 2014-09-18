************************************************************************
* Program Name      : SAPMZRQM01R_NOTI_STATUS
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.21.
* Specifications By : SeungLyong, Lee
* Pattern           : 1.2.4 Call Screen + 3.1 General
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Notification progress report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/19/2004 SLLEE        UD1K907648   Add Coding Level(QM-20040318-001)
*
*
************************************************************************

REPORT  SAPMZRQM01R_NOTI_STATUS    .

*&&& Data Declaration.  &&&*
TYPE-POOLS : VRM.     "//Value Request Manager: Types & Constants

*TYPE-POOLS CXTAB .  "//Table_control Object type pool
*TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.

INCLUDE ZQM_INCLUDE_POOL02. "/Notification - Constants and etc

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : IHPA,   "/Plant Maintenance: Partners
         JEST,   "/Object status
         QMEL,   "/Quality Notification
         QMSM.   "/Quality notification - tasks

TABLES : TPAR,
         TQ80_T.
*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_NOTI_SEL,     "/Noti Status -Selection screen Structure
         ZSQM_NOTI_STATUS,  "/Notification(Q1) Status Report - Structure
         ZSQM_NOTI_STATUS2, "/Notification(Q2/Q3) Status  - Structure
         ZSQM_NOTI_ST_LIST, "/Notification Status list
         ZSQM_NOTI_ST_LIST2. "/Notification Task Status list for Q1

*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
*CONTROLS: TC_9100  TYPE TABLEVIEW USING SCREEN 9000.

*//Type (Table Structure);(TY_ )- Table or Structure

*-- PF-Status : Excluding Function Code table
TYPES: BEGIN OF TY_FCODE,
        FCODE LIKE RSMPE-FUNC,
      END OF TY_FCODE.

DATA: IT_EX_FUNC TYPE STANDARD TABLE OF TY_FCODE WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      WA_EX_FUNC TYPE TY_FCODE.


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.

*-- Screen Control Mode
CONSTANTS : C_CODING(15)   TYPE C VALUE 'CODING',
            C_EXTWG(15)    TYPE C VALUE 'EXTWG',
            C_MATERIAL(15) TYPE C VALUE 'MATERIAL'.

**-- Process Status

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

DATA : WA_LEVEL(15) TYPE C.
DATA :  WA_RENEWAL_FLG.
*-- Screnn field cursor control
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*-- Work area Variables in Program.
DATA : WA_EXTWG TYPE EXTWG. "/Selected Ext. Mat. Group for Detail
DATA : WA_QMGRP TYPE QMGRP, "/Selected Coding group
       WA_QMCOD TYPE QMCOD. "/Selected Coding Code.
DATA : WA_KURZTEXT_COD TYPE QTXT_CODE. "/Coding code Text.

DATA : WA_UCOMM LIKE SY-UCOMM.

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Selection Criterion Text work area for Screen 9000.
DATA : BEGIN OF WA_SCR_TXT,
         TYPTX LIKE T370U-TYPTX, "/Equipment category Text
         NAME1 LIKE T001W-NAME1, "/Plant Text
         KTEXT LIKE CRTX-KTEXT,  "/Work center Text(view: 'M_CRAMN')
         CLINT LIKE KSML-CLINT,  "/Class Number
         ATNAM LIKE CABN-ATNAM,  "/Characteristic name
       END OF WA_SCR_TXT.


**-- List box variables
DATA: WA_NAME  TYPE VRM_ID,
      IT_LIST  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST.

*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT

*/-- Internale Tables with structure as sama as DB
DATA : IT_MAT_ST_LIST LIKE ZSQM_NOTI_STATUS OCCURS 10
                                             WITH HEADER LINE.
DATA : IT_ZSQM_NOTI_STATUS LIKE ZSQM_NOTI_STATUS OCCURS 10  "/Ext.Group
                                             WITH HEADER LINE.
DATA : IT_ZSQM_NOTI_STATUS2 LIKE ZSQM_NOTI_STATUS2 OCCURS 10
                                             WITH HEADER LINE.
DATA : IT_ZSQM_NOTI_STATUS_D LIKE ZSQM_NOTI_STATUS OCCURS 10  "Material
                                             WITH HEADER LINE.
DATA : IT_ZSQM_NOTI_STATUS_C LIKE ZSQM_NOTI_STATUS OCCURS 10  "Coding
                                             WITH HEADER LINE.

DATA : IT_ZSQM_NOTI_ST_LIST LIKE ZSQM_NOTI_ST_LIST  OCCURS 0
                                         WITH HEADER LINE.
DATA : IT_ZSQM_NOTI_ST_LIST2 LIKE ZSQM_NOTI_ST_LIST2  OCCURS 0
                                         WITH HEADER LINE.

*//Ranges; (R_)
*-- Selection variables
RANGES : R_MAWERK     FOR T001W-WERKS, "/Plant
         R_QMART      FOR QMEL-QMART,  "/Notification type
         R_QMGRP      FOR QMEL-QMGRP,  "/Code Group - Coding
         R_QMCOD      FOR QMEL-QMCOD,  "/Code - Coding
         R_PARNR_MAG  FOR IHPA-PARNR,  "/Manager
         R_LIFNUM     FOR QMEL-LIFNUM,  "/Vendor
         R_PARNR_VERA FOR IHPA-PARNR,   "/Resp. Department
         R_CODEGRP_VH FOR QMEL-CODEGRP_VH, "/Vehicle/Engine code. GROUP
         R_CODE_VH    FOR QMEL-CODE_VH,    "/Vehicle/Engine code
         R_ERDAT      FOR QMEL-ERDAT,      "/Created date
         R_EXTWG      FOR MARA-EXTWG.      "/External mat. group
*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)
*FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL. "table control
*"                              Table_control Object(CXTAB)

*//Field Group;

* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

*// Declare reference variables, the container and internal table
DATA: WA_CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Global variables for attributes or etc of ALV GRID
DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO. "/The Layout Structure
DATA : IT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_SORT     TYPE LVC_T_SORT WITH HEADER LINE.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

***//Macro Definitions
*-- macro : RANGE_SET &1 &2 &3
*--           &1 - Range Variable
*--           &2 - Low Variable
*--           &3 - high Variable
DEFINE RANGE_SET.
  MOVE : 'I' TO &1-SIGN.
  IF NOT &2 IS INITIAL AND
         &3 IS INITIAL.
    MOVE :'EQ' TO &1-OPTION.
    MOVE : &2  TO &1-LOW.
  ELSEIF NOT &2 IS INITIAL AND
         NOT &3 IS INITIAL.
    MOVE : 'BT' TO &1-OPTION.
    MOVE : &2   TO &1-LOW,
           &3   TO &1-HIGH.
  ELSEIF  &2 IS INITIAL AND
         NOT &3 IS INITIAL.
    MOVE : 'EQ' TO &1-OPTION.
    MOVE : &3   TO &1-LOW.
  ENDIF.
  APPEND &1.

END-OF-DEFINITION.


****//& Selection Screen Definition(Parameters Select-Option)
**-- Paramerters : (P_), Select-Options : (S_)
*SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
*AT SELECTION-SCREEN OUTPUT.
*  SET TITLEBAR '1000'.

*AT SELECTION-SCREEN.
*  CHECK SY-UCOMM = 'ONLI'.


*-- Selection for Selection Screen
*START-OF-SELECTION.
**-- End of Selection.
*END-OF-SELECTION.



*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.
*  MOVE : SY-REPID TO WA_VARIANT-REPORT. "/Set variant-report field

*-- Set listbox for notification type(ZSQM_NOTI_SEL-QMART) in scr. 9000
  PERFORM SET_LISTBOX_QMART.

*-- Set default value for Vehicle/Engine catalog type
  ZSQM_NOTI_SEL-KATART_VH = C_VH_ENG_CATEGORY.

*-- Set default value for Coding catalog type
  ZSQM_NOTI_SEL-QMKAT = C_CODING_CATEGORY.

*-- Set default value for created on first day of month and SY-DATUM
  ZSQM_NOTI_SEL-ERDAT_L = SY-DATUM. ZSQM_NOTI_SEL-ERDAT_L+6(2) = '01'.
  ZSQM_NOTI_SEL-ERDAT_H = SY-DATUM.

*INITIALIZATION.



****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
* class lcl_event_receiver: local class to handle events
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS:

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN,

    HANDLE_TOOLBAR
        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,

     HANDLE_MENU_BUTTON
        FOR EVENT MENU_BUTTON OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_UCOMM,

    HANDLE_USER_COMMAND
        FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING E_UCOMM.

  PRIVATE SECTION.

ENDCLASS.

* lcl_event_receiver (Definition)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

*-- / Double Click
  METHOD HANDLE_DOUBLE_CLICK.
* .The event DOUBLE_CLICK provides parameters for row and column
*   of the click. Use row parameter to select a line of the
*   corresponding internal table.

*- read selected row from internal table
*  READ TABLE IT_ZSQM_NOTI_STATUS INDEX E_ROW-INDEX .

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

    CASE WA_LEVEL.
      WHEN C_CODING.
*         append a separator('3') to normal toolbar
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to show detail List of selected item.
        CLEAR LS_TOOLBAR.
        MOVE 2 TO LS_TOOLBAR-BUTN_TYPE. "/ Context Menu Button Type
        MOVE 'DETAIL_ME'             TO LS_TOOLBAR-FUNCTION.
        MOVE  ICON_DETAIL            TO LS_TOOLBAR-ICON.
        MOVE 'Detail(Ext.mat.Group or Material)'(T14)
                                     TO LS_TOOLBAR-QUICKINFO.
        MOVE 'Detail'(T15)           TO LS_TOOLBAR-TEXT.
        MOVE ' '             TO LS_TOOLBAR-DISABLED.   "/For Submenu

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.


      WHEN C_EXTWG.
*         append a separator('3') to normal toolbar
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to show detail List of selected item.
        CLEAR LS_TOOLBAR.
        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
        MOVE 'DETAIL'           TO LS_TOOLBAR-FUNCTION.
        MOVE ICON_DETAIL        TO LS_TOOLBAR-ICON.
        MOVE 'Show detail'(T12) TO LS_TOOLBAR-QUICKINFO.
        MOVE 'Detail'(T13)      TO LS_TOOLBAR-TEXT.
        MOVE ' '                TO LS_TOOLBAR-DISABLED.

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.
*-------------------------------------------------------------------

  METHOD HANDLE_MENU_BUTTON.
*   At event MENU_BUTTON query your function code and define a
*     menu in the same way as a context menu.
*          Evaluate 'e_ucomm' to see which menu button of the toolbar
*          has been clicked on.
*          Define then the corresponding menu.
*          The menu contains function codes that are evaluated
*          in 'handle_user_command'.

* handle own menubuttons
    IF E_UCOMM = 'DETAIL_ME'.
      CALL METHOD E_OBJECT->ADD_FUNCTION
                  EXPORTING FCODE   = 'TO_EXTWG'
                            TEXT    = 'Ext.mat. group'(T17). "
      CALL METHOD E_OBJECT->ADD_FUNCTION
                  EXPORTING FCODE   = 'TO_MATNR'
                            TEXT    = 'Material'(T18). "

    ENDIF.

  ENDMETHOD.


*-- / Handling User defined commands for Toolbar
  METHOD HANDLE_USER_COMMAND.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.

    DATA : LT_ROWS   TYPE LVC_T_ROW,
           LW_LINE_ROW LIKE LINE OF LT_ROWS.
    DATA : LW_LINES TYPE I.

    WA_RENEWAL_FLG = C_MARK.

    WA_UCOMM = E_UCOMM.

    CASE E_UCOMM.
      WHEN 'TO_EXTWG'.
        CALL METHOD ALV_GRID->GET_SELECTED_ROWS
                 IMPORTING ET_INDEX_ROWS = LT_ROWS.

        CALL METHOD CL_GUI_CFW=>FLUSH.

        IF SY-SUBRC NE 0.
          WA_REPID = SY-REPID.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    TITEL = WA_REPID
                    TXT2  = SY-SUBRC
                    TXT1  = TEXT-E01.
        ELSE.
          DESCRIBE TABLE LT_ROWS LINES LW_LINES.
          CHECK LW_LINES = 1.     "/Check single line Selected

          READ TABLE LT_ROWS  INDEX 1 INTO LW_LINE_ROW.

          CHECK NOT LW_LINE_ROW-INDEX IS INITIAL.

          PERFORM RETRIEV_DETAIL_DATA_EXT USING LW_LINE_ROW-INDEX.

        ENDIF.


      WHEN 'DETAIL' OR 'TO_MATNR'.
        CALL METHOD ALV_GRID->GET_SELECTED_ROWS
                 IMPORTING ET_INDEX_ROWS = LT_ROWS.

        CALL METHOD CL_GUI_CFW=>FLUSH.

        IF SY-SUBRC NE 0.
          WA_REPID = SY-REPID.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    TITEL = WA_REPID
                    TXT2  = SY-SUBRC
                    TXT1  = TEXT-E01.
        ELSE.
          DESCRIBE TABLE LT_ROWS LINES LW_LINES.
          CHECK LW_LINES = 1.     "/Check single line Selected

          READ TABLE LT_ROWS  INDEX 1 INTO LW_LINE_ROW.

          CHECK NOT LW_LINE_ROW-INDEX IS INITIAL.

          PERFORM RETRIEV_DETAIL_DATA USING LW_LINE_ROW-INDEX.

        ENDIF.

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.                           "handle_user_command

ENDCLASS.
*
* lcl_event_receiver (Implementation)
*===================================================================



**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&------------------------------------------------------------------*
*&      Form  GET_TEXT_FOR_SELECT_CRITERION
*&------------------------------------------------------------------*
FORM GET_TEXT_FOR_SELECT_CRITERION.

ENDFORM.                    " GET_TEXT_FOR_SELECT_CRITERION
*&------------------------------------------------------------------*
*&      Form  RETRIEVE_SELECT_CRITERION
*&------------------------------------------------------------------*
FORM RETRIEVE_SELECT_CRITERION.

*-- Select All equipment List by Select criterion From ZVQM_EQ_CL_STAT

ENDFORM.                    " RETRIEVE_SELECT_CRITERION
*&------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.

  IF GRID_CONTAINER IS INITIAL. "/Not Created Container for ALV GRID
    CLEAR WA_RENEWAL_FLG.
*- Create Container('GRID_CONTAINER') with Custom Contro on screen
    CREATE OBJECT GRID_CONTAINER
           EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
           EXCEPTIONS
            CNTL_ERROR = 1
            CNTL_SYSTEM_ERROR = 2
            CREATE_ERROR = 3
            LIFETIME_ERROR = 4
            LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC NE 0.
      WA_REPID = SY-REPID.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                TITEL = WA_REPID
                TXT2  = SY-SUBRC
                TXT1  = 'The control can not be created'(E02).
    ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
    CREATE OBJECT ALV_GRID
           EXPORTING I_PARENT = GRID_CONTAINER
                     I_APPL_EVENTS = 'X'.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

**-- adjust field sort and subtotal to display total of column
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
*                                        IT_FIELDCAT.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_NEW_TABLE_DATA.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
*-   toolbar control event
    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON   FOR ALV_GRID.

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD ALV_GRID->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.


  ENDIF.

*-- Refresh ALV

  IF NOT GRID_CONTAINER IS INITIAL AND
          WA_RENEWAL_FLG = C_MARK.

    CLEAR WA_RENEWAL_FLG.
*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of sflight
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

*-- Display data on ALV GRID Control using method

    PERFORM SET_NEW_TABLE_DATA.
    PERFORM REFRESH_ALV_GRID_DATA_DISP.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.

  ENDIF.


ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
  IF WA_LEVEL = C_EXTWG.
    SET TITLEBAR  '9100'.
  ELSE.
    SET TITLEBAR  '9100' WITH ':' WA_LEVEL.
  ENDIF.
ENDMODULE.                 " STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Form  REFRESH_ALV_GRID_DATA_DISP
*&------------------------------------------------------------------*
FORM REFRESH_ALV_GRID_DATA_DISP.
  CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
*         EXPORTING
*           IS_STABLE      =
*           I_SOFT_REFRESH =
*         EXCEPTIONS
*           FINISHED       = 1
*           others         = 2
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " REFRESH_ALV_GRID_DATA_DISP
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      CASE WA_LEVEL.
        WHEN C_CODING.
          LEAVE TO SCREEN 0.
          PERFORM SET_LIST_LEVEL_CONTROL.
        WHEN C_EXTWG.
*          PERFORM FREE_ALV_GRID.
          PERFORM SET_LIST_LEVEL_CONTROL.
        WHEN OTHERS.
          PERFORM SET_LIST_LEVEL_CONTROL.
      ENDCASE.

    WHEN 'REFRESH'.

      PERFORM GET_DATA_AND_PROC_BASIC.

      CHECK NOT IT_ZSQM_NOTI_ST_LIST[] IS INITIAL.

      CASE WA_LEVEL.
        WHEN C_CODING.
          PERFORM COUNT_NOTI_ST_CODING_LEVEL.
*         - FILL Coding Text
          PERFORM FILL_CODING_TEXT.

        WHEN C_EXTWG.

*          PERFORM COUNT_NOTI_ST_BASIC_LEVEL.
          PERFORM COUNT_NOTI_ST_EXTWG_LEVEL  USING WA_QMGRP
                                                   WA_QMCOD.

        WHEN C_MATERIAL.

          PERFORM GET_COUNT_DETAIL_MAT_ST  USING WA_UCOMM
                                                 WA_QMGRP
                                                 WA_QMCOD
                                                 WA_EXTWG.

        WHEN OTHERS.
      ENDCASE.

      WA_RENEWAL_FLG = C_MARK.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.
      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'RW'.
      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9100  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.

      CASE WA_LEVEL.
        WHEN C_CODING.
          LEAVE TO SCREEN 0.
        WHEN C_EXTWG.
        WHEN OTHERS.
      ENDCASE.
      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN 'RW'.

      CASE WA_LEVEL.
        WHEN C_CODING.
          LEAVE TO SCREEN 0.
        WHEN C_EXTWG.
        WHEN OTHERS.
      ENDCASE..
      PERFORM SET_LIST_LEVEL_CONTROL.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = C_MARK.  "/Optimize column width
*  WA_IS_LAYOUT-DETAILTITL                "/Title bar of detail screen
*                        = 'Detail Title'(T10).
*  WA_IS_LAYOUT-GRID_TITLE                "/ Title bar text
*                    = 'Equipment List Report'(T11).
*  WA_IS_LAYOUT-KEYHOT    = C_MARK.       "/ Key columns as hotspot
*  WA_IS_LAYOUT-NO_HEADERS  = C_MARK.     "/Hide column headings
*  WA_IS_LAYOUT-NO_HGRIDLN  = C_MARK.     "/Hide horizontal grid lines
*  WA_IS_LAYOUT-NO_VGRIDLN  = C_MARK.     "/Hide vertical grid lines
*  WA_IS_LAYOUT-NO_MERGING  = C_MARK.     "/Disable cell merging
*  WA_IS_LAYOUT-NO_ROWMARK  = C_MARK.     "/Disable row selections
*  WA_IS_LAYOUT-NO_TOOLBAR  = C_MARK.     "/Hide toolbar
  WA_IS_LAYOUT-NUMC_TOTAL  = C_MARK. "/Allow totals for NUMC
*  WA_IS_LAYOUT-S_DRAGDROP  = LW_S_DRAGDROP. "/Drag & Drop control

  WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row
*  WA_IS_LAYOUT-SGL_CLK_HD = C_MARK. "/sorts the list whe column clicked

*//-- Set Variant Structure
  WA_VARIANT-REPORT = SY-REPID.
  WA_VARIANT-USERNAME = SY-UNAME.


ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9100 OUTPUT.

  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'Q23'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN '2' OR '3' OR '4'.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'Q1'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

  CASE WA_LEVEL.
    WHEN C_CODING.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'DET' OR
           SCREEN-GROUP1 = 'COD'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN C_EXTWG.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'DET'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA : LW_STRUCTURE_NAME LIKE DD02L-TABNAME.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.


  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.

      LW_STRUCTURE_NAME = 'ZSQM_NOTI_STATUS'.

    WHEN OTHERS.
      LW_STRUCTURE_NAME = 'ZSQM_NOTI_STATUS'.
*      LW_STRUCTURE_NAME = 'ZSQM_NOTI_STATUS2'.
  ENDCASE.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = LW_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

  CASE WA_LEVEL.

    WHEN C_CODING.

* Set field attribute
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_COD'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          PT_FIELDCAT-COLTEXT = 'Coding'(T05).
          MODIFY PT_FIELDCAT.
* set left alignment for the two key fields left...
        ELSEIF PT_FIELDCAT-FIELDNAME = 'EXTWG' OR
               PT_FIELDCAT-FIELDNAME = 'MATNR' OR
               PT_FIELDCAT-FIELDNAME = 'QMGRP' OR
               PT_FIELDCAT-FIELDNAME = 'QMCOD'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          MODIFY PT_FIELDCAT.

        ELSEIF PT_FIELDCAT-FIELDNAME = 'QMCOD'.
          PT_FIELDCAT-COLTEXT = TEXT-T05.
          MODIFY PT_FIELDCAT.
        ELSE.
          PERFORM SET_COUNT_FIELD_NAME  USING PT_FIELDCAT-FIELDNAME
                                              PT_FIELDCAT-COLTEXT.
          PT_FIELDCAT-JUST = 'R'.
*          PT_FIELDCAT-TOTAL = C_MARK.
          PT_FIELDCAT-DO_SUM = C_MARK.
          IF PT_FIELDCAT-FIELDNAME+0(2) = 'T_'.
            PT_FIELDCAT-EMPHASIZE = 'C711'.
          ENDIF.
          MODIFY PT_FIELDCAT.
        ENDIF.
      ENDLOOP.

    WHEN C_EXTWG.
* Set field attribute
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'EXTWG'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          MODIFY PT_FIELDCAT.
* set left alignment for the two key fields left...
        ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR' OR
               PT_FIELDCAT-FIELDNAME = 'QMGRP' OR
               PT_FIELDCAT-FIELDNAME = 'QMCOD' OR
               PT_FIELDCAT-FIELDNAME = 'KURZTEXT_COD'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          MODIFY PT_FIELDCAT.
        ELSE.
          PERFORM SET_COUNT_FIELD_NAME  USING PT_FIELDCAT-FIELDNAME
                                              PT_FIELDCAT-COLTEXT.
          PT_FIELDCAT-JUST = 'R'.
*          PT_FIELDCAT-TOTAL = C_MARK.
          PT_FIELDCAT-DO_SUM = C_MARK.
          IF PT_FIELDCAT-FIELDNAME+0(2) = 'T_'.
            PT_FIELDCAT-EMPHASIZE = 'C711'.
          ENDIF.
          MODIFY PT_FIELDCAT.
        ENDIF.
      ENDLOOP.

    WHEN C_MATERIAL.

* Set field attribute
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'MATNR'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-OUTPUTLEN = 18.
          PT_FIELDCAT-KEY      = C_MARK.
          MODIFY PT_FIELDCAT.
* set left alignment for the two key fields left...
        ELSEIF PT_FIELDCAT-FIELDNAME = 'EXTWG' OR
               PT_FIELDCAT-FIELDNAME = 'QMGRP' OR
               PT_FIELDCAT-FIELDNAME = 'QMCOD' OR
               PT_FIELDCAT-FIELDNAME = 'KURZTEXT_COD'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          MODIFY PT_FIELDCAT.
        ELSE.
          PERFORM SET_COUNT_FIELD_NAME  USING PT_FIELDCAT-FIELDNAME
                                             PT_FIELDCAT-COLTEXT.
          PT_FIELDCAT-JUST = 'R'.
*          PT_FIELDCAT-TOTAL = C_MARK.
          PT_FIELDCAT-DO_SUM = C_MARK.

          IF PT_FIELDCAT-FIELDNAME+0(2) = 'T_'.
            PT_FIELDCAT-EMPHASIZE = 'C711'.
          ENDIF.

          MODIFY PT_FIELDCAT.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&------------------------------------------------------------------*
*&      Form  FREE_ALV_GRID
*&------------------------------------------------------------------*
FORM FREE_ALV_GRID.
  CHECK NOT ALV_GRID IS INITIAL.
  CALL METHOD ALV_GRID->FREE.
  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    WA_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = WA_REPID
              TXT2  = SY-SUBRC
              TXT1  = TEXT-E01.
  ENDIF.

ENDFORM.                    " FREE_ALV_GRID
*&------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&------------------------------------------------------------------*
FORM SET_NEW_TABLE_DATA.
  DATA : LT_UI_FUNCTIONS TYPE UI_FUNCTIONS WITH HEADER LINE.

  CASE WA_LEVEL.
    WHEN C_CODING.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_NOTI_STATUS'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_NOTI_STATUS_C[].
    WHEN C_EXTWG.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_NOTI_STATUS'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_NOTI_STATUS[].
    WHEN C_MATERIAL.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_NOTI_STATUS'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_NOTI_STATUS_D[].
  ENDCASE.

ENDFORM.                    " SET_NEW_TABLE_DATA
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA USING    P_INDEX.
  DATA : LW_SEL_INDEX   LIKE SY-TABIX.
  DATA : LW_TABLE_LINES LIKE SY-TABIX.

  DATA : LW_ZSQM_NOTI_STATUS  LIKE ZSQM_NOTI_STATUS,
         LW_ZSQM_NOTI_STATUS2 LIKE ZSQM_NOTI_STATUS2.

  LW_SEL_INDEX = P_INDEX.

  CLEAR LW_ZSQM_NOTI_STATUS.

  CASE WA_UCOMM.
    WHEN 'TO_MATNR'.
      READ TABLE IT_ZSQM_NOTI_STATUS_C INDEX LW_SEL_INDEX
                                     INTO LW_ZSQM_NOTI_STATUS.

      WA_QMGRP = LW_ZSQM_NOTI_STATUS-QMGRP.
      WA_QMCOD = LW_ZSQM_NOTI_STATUS-QMCOD.
      WA_KURZTEXT_COD = LW_ZSQM_NOTI_STATUS-KURZTEXT_COD.

    WHEN 'DETAIL'.
      READ TABLE IT_ZSQM_NOTI_STATUS INDEX LW_SEL_INDEX
                                   INTO LW_ZSQM_NOTI_STATUS.

      WA_EXTWG = LW_ZSQM_NOTI_STATUS-EXTWG.
  ENDCASE.

*  PERFORM GET_COUNT_DETAIL_MATERIAL  USING WA_EXTWG.

  PERFORM GET_COUNT_DETAIL_MAT_ST  USING WA_UCOMM
                                         WA_QMGRP
                                         WA_QMCOD
                                         WA_EXTWG.

  WA_LEVEL = C_MATERIAL.

ENDFORM.                    " RETRIEV_DETAIL_DATA
*&-----------------------------------------------------------------*
*&      Module  F4_HELP_MANAGER  INPUT
*&------------------------------------------------------------------*
MODULE F4_HELP_MANAGER INPUT.

  PERFORM GET_PARTNER_VALUE   USING 'Z2'  "/Partner Function:Manager
                                    'ZSQM_NOTI_SEL-PARNR_MAG'
                                    'ZSQM_NOTI_SEL-NAME_LIST'.

ENDMODULE.                 " F4_HELP_MANAGER  INPUT
*&-------------------------------------------------------------------*
*&      Module  F4_HELP_VENDOR  INPUT
*&-------------------------------------------------------------------*
MODULE F4_HELP_VENDOR INPUT.
  PERFORM GET_PARTNER_VALUE   USING 'Z5'  "/Partner Function:Manager
                                    'ZSQM_NOTI_SEL-LIFNUM'
                                    'ZSQM_NOTI_SEL-NAME_LIF'.
ENDMODULE.                 " F4_HELP_VENDOR  INPUT
*&------------------------------------------------------------------*
*&      Module  F4_HELP_PARNR_VERA  INPUT
*&------------------------------------------------------------------*
MODULE F4_HELP_PARNR_VERA INPUT.
  PERFORM GET_PARTNER_VALUE   USING 'Z3'  "/Partner Function
                                    'ZSQM_NOTI_SEL-PARNR_VERA'
                                    'ZSQM_NOTI_SEL-NAME_VERA'.
ENDMODULE.                 " F4_HELP_PARNR_VERA  INPUT

*&-----------------------------------------------------------------*
*&      Form  GET_PARTNER_VALUE
*&-----------------------------------------------------------------*
FORM GET_PARTNER_VALUE USING    VALUE(P_PARVW)
                                VALUE(P_PARNR_FIELD)
                                VALUE(P_NAME_FIELD).
*// TPAR : Business Partner: Functions Table
  DATA : LW_NRART LIKE TPAR-NRART. "/Type of partner number
  DATA : LW_PARNR LIKE IHPA-PARNR. "/Selected Partner
  DATA : LW_NAME_LIST LIKE USER_ADDR-NAME_TEXTC. "User name


  DATA: BEGIN OF LW_DYNPFIELDS OCCURS 100.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF LW_DYNPFIELDS.
  DATA: LW_DYNAME LIKE D020S-PROG.
  DATA: LW_DYNUMB LIKE D020S-DNUM.

  CHECK NOT P_PARVW IS INITIAL.

*-- get partner type of partner number(partner function)
  SELECT SINGLE NRART INTO LW_NRART
    FROM TPAR
      WHERE PARVW = P_PARVW.

  CHECK SY-SUBRC = 0.
*--Search Help using partner type(LW_NRART)-HR organization or SAP User
  CALL FUNCTION 'SEARCH_OM_PARTNER'
       EXPORTING
            ACT_NRART         = LW_NRART
*            ACTIVE_PLVAR      = ' '
            SEARCH_STRING     = ' '
*            RESTRICTION_OTYPE = ' '
*            RESTRICTION_OBJID = '00000000'
       IMPORTING
            SEL_PARNR         = LW_PARNR
       EXCEPTIONS
            NO_ACTIVE_PLVAR   = 1
            NOT_SELECTED      = 2
            NO_OM_OTYPE       = 3
            OTHERS            = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK NOT LW_PARNR IS INITIAL.


  CASE P_PARVW.
    WHEN 'Z2'.                            "/Manager
      SELECT SINGLE NAME_TEXTC     INTO LW_NAME_LIST
        FROM USER_ADDR
           WHERE BNAME = LW_PARNR.
    WHEN 'Z3'.                           "/Resp. Department
      SELECT SINGLE ORGTX    INTO LW_NAME_LIST
        FROM T527X
           WHERE ORGEH = LW_PARNR
             AND ENDDA > SY-DATUM
             AND SPRSL = SY-LANGU.
  ENDCASE.

*-- Update screen field data using FUNCTION 'DYNP_VALUES_UPDATE'

  LW_DYNAME = SY-REPID.
  LW_DYNUMB = SY-DYNNR.

  LW_DYNPFIELDS-FIELDNAME  = P_PARNR_FIELD.
  LW_DYNPFIELDS-FIELDVALUE = LW_PARNR.
  APPEND LW_DYNPFIELDS.
  LW_DYNPFIELDS-FIELDNAME  = P_NAME_FIELD.
  LW_DYNPFIELDS-FIELDVALUE = LW_NAME_LIST.
  APPEND LW_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = LW_DYNAME
            DYNUMB     = LW_DYNUMB
       TABLES
            DYNPFIELDS = LW_DYNPFIELDS.



ENDFORM.                    " GET_PARTNER_VALUE
*&-----------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.

      IF WA_LEVEL = C_EXTWG.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN 'ONLI'.

      PERFORM SET_RANGE_VAR_FOR_SEARCH.

      PERFORM GET_DATA_AND_PROC_BASIC.

      CHECK NOT IT_ZSQM_NOTI_ST_LIST[] IS INITIAL.

      PERFORM COUNT_NOTI_ST_CODING_LEVEL.

*-- FILL Coding Text
      PERFORM FILL_CODING_TEXT.

      WA_LEVEL = C_CODING.

*-- Remarked by Function change request by Mr.Moon 03/18/2004
*      PERFORM COUNT_NOTI_ST_BASIC_LEVEL.
*      WA_LEVEL = C_EXTWG.

      WA_RENEWAL_FLG = C_MARK.
      CALL SCREEN 9100.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  SET_RANGE_VAR_FOR_SEARCH
*&------------------------------------------------------------------*
FORM SET_RANGE_VAR_FOR_SEARCH.
  DATA : LW_PARNR LIKE IHPA-PARNR.

  CLEAR :  R_MAWERK,     R_PARNR_MAG, R_LIFNUM, R_PARNR_VERA,
           R_CODEGRP_VH, R_CODE_VH,   R_ERDAT,  R_QMART, R_EXTWG,
           R_QMGRP,      R_QMCOD.
  REFRESH :  R_MAWERK,     R_PARNR_MAG, R_LIFNUM, R_PARNR_VERA,
             R_CODEGRP_VH, R_CODE_VH,   R_ERDAT,  R_QMART, R_EXTWG,
             R_QMGRP,      R_QMCOD.


  IF NOT ZSQM_NOTI_SEL-MAWERK IS INITIAL.
    RANGE_SET R_MAWERK  ZSQM_NOTI_SEL-MAWERK ''.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-PARNR_MAG IS INITIAL.
    LW_PARNR = ZSQM_NOTI_SEL-PARNR_MAG.
    SHIFT LW_PARNR LEFT  DELETING LEADING  '0'.

    RANGE_SET R_PARNR_MAG  LW_PARNR ''.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-LIFNUM IS INITIAL.
    RANGE_SET R_LIFNUM     ZSQM_NOTI_SEL-LIFNUM ''.
  ENDIF.


*-- Resp. Department

  IF NOT ZSQM_NOTI_SEL-PARNR_VERA IS INITIAL.
    LW_PARNR = ZSQM_NOTI_SEL-PARNR_VERA.
    SHIFT LW_PARNR LEFT  DELETING LEADING  '0'.

    RANGE_SET R_PARNR_VERA     LW_PARNR ''.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-CODEGRP_VH IS INITIAL.
    RANGE_SET R_CODEGRP_VH     ZSQM_NOTI_SEL-CODEGRP_VH ''.
    IF NOT ZSQM_NOTI_SEL-CODE_VH IS INITIAL.
      RANGE_SET R_CODE_VH     ZSQM_NOTI_SEL-CODE_VH ''.
    ENDIF.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-EXTWG_L IS INITIAL.
    RANGE_SET R_EXTWG     ZSQM_NOTI_SEL-EXTWG_L  ZSQM_NOTI_SEL-EXTWG_H.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-ERDAT_L IS INITIAL.
    RANGE_SET R_ERDAT    ZSQM_NOTI_SEL-ERDAT_L ZSQM_NOTI_SEL-ERDAT_H.
  ENDIF.

  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
      RANGE_SET  R_QMART  C_QMART_INT_DEF ''.
    WHEN '2'.
      RANGE_SET  R_QMART  C_QMART_LINE_EXT ''.
    WHEN '3'.
      RANGE_SET  R_QMART  C_QMART_INSP_EXT ''.
    WHEN '4'.
      RANGE_SET  R_QMART  C_QMART_LINE_EXT  C_QMART_INSP_EXT.
  ENDCASE.

  IF NOT ZSQM_NOTI_SEL-QMCOD IS INITIAL.
    RANGE_SET : R_QMGRP    ZSQM_NOTI_SEL-QMGRP '',
                R_QMCOD    ZSQM_NOTI_SEL-QMCOD ''.
  ENDIF.

ENDFORM.                    " SET_RANGE_VAR_FOR_SEARCH
*&------------------------------------------------------------------*
*&      Module  CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
MODULE CHECK_VH_ENG_INPUT INPUT.


  CHECK     ZSQM_NOTI_SEL-CODEGRP_VH IS INITIAL AND
        NOT ZSQM_NOTI_SEL-CODE_VH    IS INITIAL.

  MESSAGE E000(ZMQM)
      WITH 'Please Input Vehicle/Engine Code Group.'(E20).

ENDMODULE.                 " CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_SCREEN_9000 INPUT.
  CLEAR : ZSQM_NOTI_SEL-NAME1,
          ZSQM_NOTI_SEL-QMARTX,
          ZSQM_NOTI_SEL-NAME_LIST,
          ZSQM_NOTI_SEL-NAME_LIF,
          ZSQM_NOTI_SEL-NAME_VERA,
          ZSQM_NOTI_SEL-KURZTEXT_VH,
          ZSQM_NOTI_SEL-KURZTEXT_G,
          ZSQM_NOTI_SEL-EWBEZ_L,
          ZSQM_NOTI_SEL-KURZTEXT_COD.

  IF NOT ZSQM_NOTI_SEL-MAWERK IS INITIAL.
    SELECT SINGLE NAME1 INTO ZSQM_NOTI_SEL-NAME1
        FROM T001W
          WHERE WERKS = ZSQM_NOTI_SEL-MAWERK.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-MAWERK
                              'is not exist!'(E10).
    ENDIF.

  ENDIF.

  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
      SELECT SINGLE QMARTX INTO ZSQM_NOTI_SEL-QMARTX
         FROM TQ80_T
           WHERE QMART = C_QMART_INT_DEF
             AND SPRAS = SY-LANGU.
    WHEN OTHERS.

      SELECT SINGLE QMARTX INTO ZSQM_NOTI_SEL-QMARTX
         FROM TQ80_T
           WHERE QMART = C_QMART_LINE_EXT
             AND SPRAS = SY-LANGU.

      CLEAR TQ80_T.

      SELECT SINGLE *   FROM TQ80_T
           WHERE QMART = C_QMART_INSP_EXT
             AND SPRAS = SY-LANGU.

      IF ZSQM_NOTI_SEL-QMART = '2'.

      ELSEIF ZSQM_NOTI_SEL-QMART = '3' .
        MOVE : TQ80_T-QMARTX TO ZSQM_NOTI_SEL-QMARTX.
      ELSE .
        CONCATENATE ZSQM_NOTI_SEL-QMARTX  '+' TQ80_T-QMARTX
                 INTO ZSQM_NOTI_SEL-QMARTX SEPARATED BY SPACE.
      ENDIF.

  ENDCASE.

  IF NOT ZSQM_NOTI_SEL-PARNR_MAG IS INITIAL.
    DATA : LW_BNAME LIKE USER_ADDR-BNAME.

    LW_BNAME = ZSQM_NOTI_SEL-PARNR_MAG.
    SHIFT LW_BNAME LEFT  DELETING LEADING  '0'.

    SELECT SINGLE NAME_TEXTC     INTO ZSQM_NOTI_SEL-NAME_LIST
        FROM USER_ADDR
           WHERE BNAME = LW_BNAME.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-MAWERK
                              TEXT-E10.
    ENDIF.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-LIFNUM IS INITIAL.
    SELECT SINGLE NAME1  INTO ZSQM_NOTI_SEL-NAME_LIF
      FROM LFA1
        WHERE LIFNR = ZSQM_NOTI_SEL-LIFNUM.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-LIFNUM
                              TEXT-E10.
    ENDIF.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-PARNR_VERA IS INITIAL.
    SELECT SINGLE ORGTX    INTO ZSQM_NOTI_SEL-NAME_VERA
      FROM T527X
         WHERE ORGEH = ZSQM_NOTI_SEL-PARNR_VERA
           AND ENDDA > SY-DATUM
           AND SPRSL = SY-LANGU.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-PARNR_VERA
                              TEXT-E10.
    ENDIF.
  ENDIF.


  IF NOT ZSQM_NOTI_SEL-CODEGRP_VH IS INITIAL.
    SELECT SINGLE KURZTEXT INTO ZSQM_NOTI_SEL-KURZTEXT_G
      FROM ZVQM_QPGRT01
         WHERE KATALOGART = ZSQM_NOTI_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_NOTI_SEL-CODEGRP_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-CODEGRP_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_NOTI_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_NOTI_SEL-CODE_VH    IS INITIAL.
    SELECT SINGLE KURZTEXT_C INTO ZSQM_NOTI_SEL-KURZTEXT_VH
      FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_NOTI_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_NOTI_SEL-CODEGRP_VH
           AND CODE       = ZSQM_NOTI_SEL-CODE_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-CODEGRP_VH
                              ','
                              ZSQM_NOTI_SEL-CODE_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_NOTI_SEL-EXTWG_L IS INITIAL.
    SELECT SINGLE EWBEZ INTO ZSQM_NOTI_SEL-EWBEZ_L
       FROM TWEWT
         WHERE EXTWG = ZSQM_NOTI_SEL-EXTWG_L
           AND SPRAS = SY-LANGU.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-EXTWG_L
                              ','
                              ''
                              TEXT-E10.
    ENDIF.
  ENDIF.

  IF NOT ZSQM_NOTI_SEL-QMCOD IS INITIAL.
    SELECT SINGLE  KURZTEXT_C INTO ZSQM_NOTI_SEL-KURZTEXT_COD
       FROM ZVQM_QPCT_EN
         WHERE KATALOGART  = C_CODING_CATEGORY
           AND CODEGRUPPE  = ZSQM_NOTI_SEL-QMGRP
           AND CODE        = ZSQM_NOTI_SEL-QMCOD
           AND SPRAS       = SY-LANGU
           AND SPRAS_CD    = SY-LANGU.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_NOTI_SEL-QMCOD
                              ','
                              ''
                              TEXT-E10.
    ENDIF.

  ENDIF.

ENDMODULE.                 " GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  SET_LISTBOX_QMART
*&------------------------------------------------------------------*
FORM SET_LISTBOX_QMART.
  WA_NAME = 'ZSQM_NOTI_SEL-QMART'.
  WA_VALUE-KEY = '1'. WA_VALUE-TEXT = C_QMART_INT_DEF.
  APPEND WA_VALUE TO IT_LIST.
  WA_VALUE-KEY = '2'. WA_VALUE-TEXT = C_QMART_LINE_EXT.
  APPEND WA_VALUE TO IT_LIST.
*-- Deleted by request(03/18/2004 : QM-20040318-001) - start
*  WA_VALUE-KEY = '3'. WA_VALUE-TEXT = C_QMART_INSP_EXT.
*  APPEND WA_VALUE TO IT_LIST.
*  WA_VALUE-KEY = '4'. WA_VALUE-TEXT = 'Q2+Q3'.
*  APPEND WA_VALUE TO IT_LIST.
*-- Deleted by request(03/18/2004 : QM-20040318-001) - end

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = WA_NAME
            VALUES = IT_LIST.

ENDFORM.                    " SET_LISTBOX_QMART
*&-----------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Form  GET_DATA_USING_SEL_CRITERION
*&------------------------------------------------------------------*
FORM GET_DATA_USING_SEL_CRITERION.

  REFRESH : IT_ZSQM_NOTI_ST_LIST, IT_ZSQM_NOTI_ST_LIST2.


  CASE ZSQM_NOTI_SEL-QMART.

    WHEN '1'.                                               " = 'Q1'

*     - Notification status list for Q1
      SELECT A~QMNUM A~OBJNR A~QMGRP A~QMCOD  A~MAWERK A~MATNR  A~EXTWG
             A~PLANDAT A~LTEXT_IMPR A~LTEXT_CONF
             F~STAT
          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_NOTI_ST_LIST
        FROM ( (  QMEL AS A      INNER JOIN IHPA AS C
           ON   A~OBJNR = C~OBJNR ) INNER JOIN IHPA AS E
           ON   A~OBJNR = E~OBJNR ) INNER JOIN JEST AS F
           ON   A~OBJNR = F~OBJNR
        WHERE  A~MAWERK     IN R_MAWERK
          AND  A~QMART      IN R_QMART
          AND  A~KATART_VH  = ZSQM_NOTI_SEL-KATART_VH
          AND  A~CODEGRP_VH IN R_CODEGRP_VH
          AND  A~CODE_VH    IN R_CODE_VH
          AND  A~ERDAT      IN R_ERDAT
          AND  A~EXTWG      IN R_EXTWG
*          AND  A~QMKAT      = C_CODING_CATEGORY
          AND  A~QMGRP      IN R_QMGRP
*          AND  G~STATUS     = '2'    "/Released - Request by FCR(03/19)
*          and  G~INAKTIV    = ' '
          AND  A~QMCOD      IN R_QMCOD
          AND  A~KZLOESCH   = ' '           "/Not Deleted 'X'-Deleted
          AND  C~PARNR      IN R_PARNR_MAG
          AND  C~PARVW      =  C_PARVW_MANAGER
          AND  E~PARNR      IN R_PARNR_VERA
          AND  E~PARVW      =  C_PARVW_RESP_DEP
          AND  F~INACT      = ' '
          AND  F~STAT       IN (C_STAT_NOTI_CREATION,
                                C_STAT_NOTI_RELEASE,
                                C_STAT_NOTI_COMPLETION).

      CHECK SY-SUBRC = 0.

*     - Notification Task status list for only Q1
      SELECT A~QMNUM B~MANUM A~QMGRP A~QMCOD
             B~OBJNR AS OBJNR_TASK
             A~EXTWG  A~MATNR F~STAT
          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_NOTI_ST_LIST2
        FROM  ( ( ( QMEL AS A INNER JOIN QMSM AS B
           ON A~QMNUM = B~QMNUM ) INNER JOIN IHPA AS C
           ON A~OBJNR = C~OBJNR ) INNER JOIN IHPA AS E
           ON A~OBJNR = E~OBJNR ) INNER JOIN JEST AS F
           ON B~OBJNR = F~OBJNR
        WHERE  A~MAWERK     IN R_MAWERK
          AND  A~QMART      IN R_QMART
          AND  A~KATART_VH  = ZSQM_NOTI_SEL-KATART_VH
          AND  A~CODEGRP_VH IN R_CODEGRP_VH
          AND  A~CODE_VH    IN R_CODE_VH
          AND  A~ERDAT      IN R_ERDAT
*          AND  A~QMKAT      = C_CODING_CATEGORY
          AND  A~QMGRP      IN R_QMGRP
          AND  A~QMCOD      IN R_QMCOD
          AND  A~KZLOESCH   = ' '           "/Not Deleted 'X'-Deleted
          AND  C~PARNR      IN R_PARNR_MAG
          AND  C~PARVW      =  C_PARVW_MANAGER
          AND  E~PARNR      IN R_PARNR_VERA
          AND  E~PARVW      =  C_PARVW_RESP_DEP
          AND  F~INACT      = ' '
          AND  F~STAT       IN (C_STAT_TASK_CREATION,
                                C_STAT_TASK_RELEASE,
                                C_STAT_TASK_COMPLETION,
                                C_STAT_TASK_SUCCESSFUL)
          AND  B~MANUM = ( SELECT MAX( MANUM ) FROM QMSM
                               WHERE QMNUM = A~QMNUM ).

    WHEN OTHERS.

*     - Notification status list for Q2, Q3 AND Q2 + Q3
      SELECT A~QMNUM A~OBJNR A~QMGRP A~QMCOD A~MAWERK A~MATNR  A~EXTWG
             A~PLANDAT A~LTEXT_IMPR A~LTEXT_CONF
             A~COMPLETED  A~SUCCESS
             F~STAT
          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_NOTI_ST_LIST
        FROM  ( (  QMEL AS A  INNER JOIN IHPA AS C
           ON A~OBJNR = C~OBJNR ) INNER JOIN IHPA AS E
           ON A~OBJNR = E~OBJNR ) INNER JOIN JEST AS F
           ON A~OBJNR = F~OBJNR
        WHERE  A~MAWERK     IN R_MAWERK
          AND  A~QMART      IN R_QMART
          AND  A~KATART_VH  = ZSQM_NOTI_SEL-KATART_VH
          AND  A~CODEGRP_VH IN R_CODEGRP_VH
          AND  A~CODE_VH    IN R_CODE_VH
          AND  A~ERDAT      IN R_ERDAT
          AND  A~EXTWG      IN R_EXTWG
*          AND  A~QMKAT      = C_CODING_CATEGORY
          AND  A~QMGRP      IN R_QMGRP
          AND  A~QMCOD      IN R_QMCOD
          AND  A~KZLOESCH   = ' '           "/Not Deleted 'X'-Deleted
          AND  C~PARNR      IN R_PARNR_MAG
          AND  C~PARVW      =  C_PARVW_MANAGER
          AND  E~PARNR      IN R_LIFNUM
          AND  E~PARVW      =  C_PARVW_VENDOR
          AND  F~INACT      = ' '
          AND  F~STAT       IN (C_STAT_NOTI_CREATION,
                                C_STAT_NOTI_RELEASE,
                                C_STAT_NOTI_COMPLETION).

  ENDCASE.

**// Remove not available task for only 'Q1' Notification
**    - if there are several task for notification,
*  CHECK ZSQM_NOTI_SEL-QMART = '1'.                          "// 'Q1'



ENDFORM.                    " GET_DATA_USING_SEL_CRITERION
*&------------------------------------------------------------------*
*&      Form  COUNT_NOTI_STATUS
*&------------------------------------------------------------------*
FORM COUNT_NOTI_STATUS.
  REFRESH : IT_ZSQM_NOTI_STATUS, IT_ZSQM_NOTI_STATUS2.

  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
*-    count Notifcation status
      LOOP AT IT_ZSQM_NOTI_ST_LIST.
        CLEAR IT_ZSQM_NOTI_STATUS.

        MOVE : IT_ZSQM_NOTI_ST_LIST-EXTWG TO IT_ZSQM_NOTI_STATUS-EXTWG.

        CASE IT_ZSQM_NOTI_ST_LIST-STAT.
          WHEN C_STAT_NOTI_CREATION.                 "/Noti Creation.
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-CREATION.

          WHEN C_STAT_NOTI_RELEASE.                   "/Noti. Release
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-RELEASE.

          WHEN C_STAT_NOTI_COMPLETION.                "/Noti. Complete
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-COMPLETE.
        ENDCASE.
        COLLECT IT_ZSQM_NOTI_STATUS.
      ENDLOOP.

*-     count Notification Task status
      LOOP AT IT_ZSQM_NOTI_ST_LIST2.
        CLEAR IT_ZSQM_NOTI_STATUS.

        MOVE : IT_ZSQM_NOTI_ST_LIST2-EXTWG TO IT_ZSQM_NOTI_STATUS-EXTWG.
        CASE IT_ZSQM_NOTI_ST_LIST2-STAT.
          WHEN C_STAT_TASK_CREATION.                 "/Task Creation
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_CREATION.

          WHEN C_STAT_TASK_RELEASE.                   "/Task Release
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_RELEASE.

          WHEN C_STAT_TASK_COMPLETION.                "/Task completion
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_COMPLETE.

          WHEN C_STAT_TASK_SUCCESSFUL.                "/Task successful
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_SUCCESS.
        ENDCASE.

*-     Subtract successful from completion
        IF NOT IT_ZSQM_NOTI_STATUS-T_SUCCESS IS INITIAL.    "= 1
          IT_ZSQM_NOTI_STATUS-T_COMPLETE = -1.
        ENDIF.

        COLLECT IT_ZSQM_NOTI_STATUS.
      ENDLOOP.

    WHEN OTHERS.
*-    count Notifcation status and Vendor response. for Q2, Q3 and Q2+Q3

*-- 02/02/2004 Modifid by SLLEE. - Start
*--   - Modified. because ZQMEX_02 was changed. Status control process
*--     was changed. add Completed/Success fields are added (Q2/Q3)
*
      LOOP AT IT_ZSQM_NOTI_ST_LIST.
        CLEAR IT_ZSQM_NOTI_STATUS.

        MOVE : IT_ZSQM_NOTI_ST_LIST-EXTWG TO IT_ZSQM_NOTI_STATUS-EXTWG.

        CASE IT_ZSQM_NOTI_ST_LIST-STAT.
          WHEN C_STAT_NOTI_CREATION.                 "/Noti Creation.
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-CREATION.

          WHEN C_STAT_NOTI_RELEASE.                   "/Noti. Release
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-RELEASE.

          WHEN C_STAT_NOTI_COMPLETION.                "/Noti. Complete
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS-COMPLETE.
        ENDCASE.

        IF IT_ZSQM_NOTI_ST_LIST-SUCCESS = C_MARK.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_SUCCESS.
        ELSEIF IT_ZSQM_NOTI_ST_LIST-COMPLETED = C_MARK.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_COMPLETE.
        ELSEIF NOT IT_ZSQM_NOTI_ST_LIST-PLANDAT IS INITIAL AND
                   IT_ZSQM_NOTI_ST_LIST-COMPLETED IS INITIAL.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_RELEASE.
        ELSEIF  IT_ZSQM_NOTI_ST_LIST-PLANDAT IS INITIAL AND
                IT_ZSQM_NOTI_ST_LIST-COMPLETED  IS INITIAL.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS-T_CREATION.
        ENDIF.

**----- 01/21/2004 Appended by BSBAE
*        IF NOT IT_ZSQM_NOTI_ST_LIST-LTEXT_CONF IS INITIAL.
*          MOVE : 0 TO IT_ZSQM_NOTI_STATUS2-PLANNED,
*                 0 TO IT_ZSQM_NOTI_STATUS2-LT_IMPR,
*                 1 TO IT_ZSQM_NOTI_STATUS2-LT_CONF.
*        ELSEIF NOT IT_ZSQM_NOTI_ST_LIST-LTEXT_IMPR IS INITIAL AND
*                   IT_ZSQM_NOTI_ST_LIST-LTEXT_CONF IS INITIAL.
*          MOVE : 0 TO IT_ZSQM_NOTI_STATUS2-PLANNED,
*                 1 TO IT_ZSQM_NOTI_STATUS2-LT_IMPR,
*                 0 TO IT_ZSQM_NOTI_STATUS2-LT_CONF.
*        ELSEIF NOT IT_ZSQM_NOTI_ST_LIST-PLANDAT    IS INITIAL AND
*                   IT_ZSQM_NOTI_ST_LIST-LTEXT_IMPR IS INITIAL AND
*                   IT_ZSQM_NOTI_ST_LIST-LTEXT_CONF IS INITIAL.
*          MOVE : 1 TO IT_ZSQM_NOTI_STATUS2-PLANNED,
*                 0 TO IT_ZSQM_NOTI_STATUS2-LT_IMPR,
*                 0 TO IT_ZSQM_NOTI_STATUS2-LT_CONF.
*        ELSEIF IT_ZSQM_NOTI_ST_LIST-PLANDAT    IS INITIAL AND
*               IT_ZSQM_NOTI_ST_LIST-LTEXT_IMPR IS INITIAL AND
*                   IT_ZSQM_NOTI_ST_LIST-LTEXT_CONF IS INITIAL.
*          MOVE : 0 TO IT_ZSQM_NOTI_STATUS2-PLANNED,
*                 0 TO IT_ZSQM_NOTI_STATUS2-LT_IMPR,
*                 0 TO IT_ZSQM_NOTI_STATUS2-LT_CONF.
*        ENDIF.
*
**        IF NOT IT_ZSQM_NOTI_ST_LIST-PLANDAT IS INITIAL.
**          MOVE : 1 TO IT_ZSQM_NOTI_STATUS2-PLANNED.
**        ENDIF.
**
**        IF NOT IT_ZSQM_NOTI_ST_LIST-LTEXT_IMPR IS INITIAL.
**          MOVE : 1 TO IT_ZSQM_NOTI_STATUS2-LT_IMPR.
**          IT_ZSQM_NOTI_STATUS2-PLANNED = -1.
**        ENDIF.
**
**        IF NOT IT_ZSQM_NOTI_ST_LIST-LTEXT_CONF IS INITIAL.
**          MOVE : 1 TO IT_ZSQM_NOTI_STATUS2-LT_CONF.
**          IT_ZSQM_NOTI_STATUS2-LT_IMPR = -1.
**        ENDIF.
**----- 01/21/2004 Appended by BSBAE
*
*        COLLECT IT_ZSQM_NOTI_STATUS2.
*-- 02/02/2004 Modifid by SLLEE. - End

        COLLECT IT_ZSQM_NOTI_STATUS.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    " COUNT_NOTI_STATUS
*&------------------------------------------------------------------*
*&      Form  SET_LIST_LEVEL_CONTROL
*&------------------------------------------------------------------*
FORM SET_LIST_LEVEL_CONTROL.
  CASE WA_LEVEL.
    WHEN C_CODING.
    WHEN C_EXTWG.
      WA_LEVEL = C_CODING.
      WA_RENEWAL_FLG = C_MARK.
    WHEN C_MATERIAL.
      CASE WA_UCOMM.
        WHEN 'TO_MATNR'.
          WA_LEVEL = C_CODING.
          WA_RENEWAL_FLG = C_MARK.
        WHEN 'DETAIL'.
          WA_LEVEL = C_EXTWG.
          WA_RENEWAL_FLG = C_MARK.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " SET_LIST_LEVEL_CONTROL
*&---------------------------------------------------------------------*
*&      Form  GET_COUNT_DETAIL_MATERIAL
*&---------------------------------------------------------------------*
FORM GET_COUNT_DETAIL_MATERIAL.
  REFRESH : IT_ZSQM_NOTI_STATUS_D.

  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
*-    count Notifcation status
      LOOP AT IT_ZSQM_NOTI_ST_LIST WHERE EXTWG = WA_EXTWG.
        CLEAR IT_ZSQM_NOTI_STATUS_D.

        MOVE: IT_ZSQM_NOTI_ST_LIST-MATNR TO IT_ZSQM_NOTI_STATUS_D-MATNR
  .

        CASE IT_ZSQM_NOTI_ST_LIST-STAT.
          WHEN C_STAT_NOTI_CREATION.                 "/Noti Creation.
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-CREATION.

          WHEN C_STAT_NOTI_RELEASE.                   "/Noti. Release
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-RELEASE.

          WHEN C_STAT_NOTI_COMPLETION.                "/Noti. Complete
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-COMPLETE.
        ENDCASE.
        COLLECT IT_ZSQM_NOTI_STATUS_D.
      ENDLOOP.

*-     count Notification Task status
      LOOP AT IT_ZSQM_NOTI_ST_LIST2  WHERE EXTWG = WA_EXTWG.
        CLEAR IT_ZSQM_NOTI_STATUS_D.

        MOVE:IT_ZSQM_NOTI_ST_LIST2-MATNR TO IT_ZSQM_NOTI_STATUS_D-MATNR
  .
        CASE IT_ZSQM_NOTI_ST_LIST2-STAT.
          WHEN C_STAT_TASK_CREATION.                 "/Task Creation
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_CREATION.

          WHEN C_STAT_TASK_RELEASE.                   "/Task Release
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_RELEASE.

          WHEN C_STAT_TASK_COMPLETION.                "/Task completion
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_COMPLETE.

          WHEN C_STAT_TASK_SUCCESSFUL.                "/Task successful
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_SUCCESS.
        ENDCASE.

*-     Subtract successful from completion
        IF NOT IT_ZSQM_NOTI_STATUS_D-T_SUCCESS IS INITIAL.    "= 1
          IT_ZSQM_NOTI_STATUS_D-T_COMPLETE = -1.
        ENDIF.

        COLLECT IT_ZSQM_NOTI_STATUS_D.
      ENDLOOP.

    WHEN OTHERS.
*-   count Notifcation status and Vendor response. for Q2, Q3 and Q2+Q3
*-- 02/02/2004 Modifid by SLLEE. - Start
*--   - Modified. because ZQMEX_02 was changed. Status control process
*--     was changed. add Completed/Success fields are added (Q2/Q3)
*
      LOOP AT IT_ZSQM_NOTI_ST_LIST WHERE EXTWG = WA_EXTWG.
        CLEAR IT_ZSQM_NOTI_STATUS.

       MOVE : IT_ZSQM_NOTI_ST_LIST-MATNR TO IT_ZSQM_NOTI_STATUS_D-MATNR.

        CASE IT_ZSQM_NOTI_ST_LIST-STAT.
          WHEN C_STAT_NOTI_CREATION.                 "/Noti Creation.
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-CREATION.

          WHEN C_STAT_NOTI_RELEASE.                   "/Noti. Release
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-RELEASE.

          WHEN C_STAT_NOTI_COMPLETION.                "/Noti. Complete
            MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-COMPLETE.
        ENDCASE.



        IF IT_ZSQM_NOTI_ST_LIST-SUCCESS = C_MARK.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_SUCCESS.
        ELSEIF IT_ZSQM_NOTI_ST_LIST-COMPLETED = C_MARK.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_COMPLETE.
        ELSEIF NOT IT_ZSQM_NOTI_ST_LIST-PLANDAT IS INITIAL AND
                   IT_ZSQM_NOTI_ST_LIST-COMPLETED IS INITIAL.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_RELEASE.
        ELSEIF  IT_ZSQM_NOTI_ST_LIST-PLANDAT IS INITIAL AND
                IT_ZSQM_NOTI_ST_LIST-COMPLETED  IS INITIAL.
          MOVE : 1 TO IT_ZSQM_NOTI_STATUS_D-T_CREATION.
        ENDIF.

*----- 01/21/2004 Appended by BSBAE
        COLLECT IT_ZSQM_NOTI_STATUS_D.
      ENDLOOP.
  ENDCASE.


ENDFORM.                    " GET_COUNT_DETAIL_MATERIAL
*&---------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  SET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
*&----------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&----------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FLDTXT, WA_CUR_LINE.
  GET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
*&------------------------------------------------------------------*
*&      Module  CONTROL_BY_QMART  INPUT
*&------------------------------------------------------------------*
MODULE CONTROL_BY_QMART INPUT.
  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
      CLEAR : ZSQM_NOTI_SEL-LIFNUM, ZSQM_NOTI_SEL-NAME_LIF.
    WHEN '2'. "/ OR '3' OR '4'.
      CLEAR : ZSQM_NOTI_SEL-PARNR_VERA, ZSQM_NOTI_SEL-NAME_VERA.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " CONTROL_BY_QMART  INPUT
*&-----------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.
  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'Q23'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN '2' OR '3' OR '4'.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'Q1'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Form  SET_COUNT_FIELD_NAME
*&------------------------------------------------------------------*
FORM SET_COUNT_FIELD_NAME USING    P_FIELDNAME
                                   P_REPTEXT.

  CASE P_FIELDNAME.
    WHEN 'CREATION'.
      P_REPTEXT = 'OSNO'.
    WHEN 'RELEASE'.
      P_REPTEXT = 'NOPR'.
    WHEN 'COMPLETE'.
      P_REPTEXT = 'NOCO'.
    WHEN 'T_CREATION'.
      P_REPTEXT = 'TSOS'.
    WHEN 'T_RELEASE'.
      P_REPTEXT = 'TSRL'.
    WHEN 'T_COMPLETE'.
      P_REPTEXT = 'TSCO'.
    WHEN 'T_SUCCESS'.
      P_REPTEXT = 'TSSC'.
*    WHEN 'PLANNED'.
*      P_REPTEXT = 'TSRL'.
*    WHEN 'LT_IMPR'.
*      P_REPTEXT = 'TSCO'.
*    WHEN 'LT_CONF'.
*      P_REPTEXT = 'TSSC'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " SET_COUNT_FIELD_NAME
*&---------------------------------------------------------------------*
*&      Form  GET_MAT_NOTI_ST_LIST
*&---------------------------------------------------------------------*
FORM GET_MAT_NOTI_ST_LIST.
  REFRESH IT_MAT_ST_LIST.

  CASE ZSQM_NOTI_SEL-QMART.
    WHEN '1'.
*-    count Notifcation status
      LOOP AT IT_ZSQM_NOTI_ST_LIST.
        CLEAR IT_MAT_ST_LIST.

        MOVE :
               IT_ZSQM_NOTI_ST_LIST-QMGRP TO IT_MAT_ST_LIST-QMGRP,
               IT_ZSQM_NOTI_ST_LIST-QMCOD TO IT_MAT_ST_LIST-QMCOD,
               IT_ZSQM_NOTI_ST_LIST-EXTWG TO IT_MAT_ST_LIST-EXTWG,
               IT_ZSQM_NOTI_ST_LIST-MATNR TO IT_MAT_ST_LIST-MATNR.

        CASE IT_ZSQM_NOTI_ST_LIST-STAT.
          WHEN C_STAT_NOTI_CREATION.                 "/Noti Creation.
            MOVE : 1 TO IT_MAT_ST_LIST-CREATION.

          WHEN C_STAT_NOTI_RELEASE.                   "/Noti. Release
            MOVE : 1 TO IT_MAT_ST_LIST-RELEASE.

          WHEN C_STAT_NOTI_COMPLETION.                "/Noti. Complete
            MOVE : 1 TO IT_MAT_ST_LIST-COMPLETE.

          WHEN C_STAT_NOTI_DELETION.

        ENDCASE.
        COLLECT IT_MAT_ST_LIST.
      ENDLOOP.

*-     count Notification Task status
      LOOP AT IT_ZSQM_NOTI_ST_LIST2.
        CLEAR IT_MAT_ST_LIST.

        MOVE :
               IT_ZSQM_NOTI_ST_LIST2-QMGRP TO IT_MAT_ST_LIST-QMGRP,
               IT_ZSQM_NOTI_ST_LIST2-QMCOD TO IT_MAT_ST_LIST-QMCOD,
               IT_ZSQM_NOTI_ST_LIST2-EXTWG TO IT_MAT_ST_LIST-EXTWG,
               IT_ZSQM_NOTI_ST_LIST2-MATNR TO IT_MAT_ST_LIST-MATNR.

        CASE IT_ZSQM_NOTI_ST_LIST2-STAT.
          WHEN C_STAT_TASK_CREATION.                 "/Task Creation
            MOVE : 1 TO IT_MAT_ST_LIST-T_CREATION.

          WHEN C_STAT_TASK_RELEASE.                   "/Task Release
            MOVE : 1 TO IT_MAT_ST_LIST-T_RELEASE.

          WHEN C_STAT_TASK_COMPLETION.                "/Task completion
            MOVE : 1 TO IT_MAT_ST_LIST-T_COMPLETE.

          WHEN C_STAT_TASK_SUCCESSFUL.                "/Task successful
            MOVE : 1 TO IT_MAT_ST_LIST-T_SUCCESS.
*             - Subtract successful from completion
            MOVE : -1 TO  IT_MAT_ST_LIST-T_COMPLETE.
        ENDCASE.

        COLLECT IT_MAT_ST_LIST.
      ENDLOOP.

    WHEN OTHERS.
*-    count Notifcation status and Vendor response. for Q2, Q3 and Q2+Q3

*-- 02/02/2004 Modifid by SLLEE. - Start
*--   - Modified. because ZQMEX_02 was changed. Status control process
*--     was changed. add Completed/Success fields are added (Q2/Q3)

      LOOP AT IT_ZSQM_NOTI_ST_LIST.
        CLEAR IT_MAT_ST_LIST.

        MOVE :
               IT_ZSQM_NOTI_ST_LIST-QMGRP TO IT_MAT_ST_LIST-QMGRP,
               IT_ZSQM_NOTI_ST_LIST-QMCOD TO IT_MAT_ST_LIST-QMCOD,
               IT_ZSQM_NOTI_ST_LIST-EXTWG TO IT_MAT_ST_LIST-EXTWG,
               IT_ZSQM_NOTI_ST_LIST-MATNR TO IT_MAT_ST_LIST-MATNR.

        CASE IT_ZSQM_NOTI_ST_LIST-STAT.
          WHEN C_STAT_NOTI_CREATION.                 "/Noti Creation.
            MOVE : 1 TO IT_MAT_ST_LIST-CREATION.

          WHEN C_STAT_NOTI_RELEASE.                   "/Noti. Release
            MOVE : 1 TO IT_MAT_ST_LIST-RELEASE.

          WHEN C_STAT_NOTI_COMPLETION.                "/Noti. Complete
            MOVE : 1 TO IT_MAT_ST_LIST-COMPLETE.
        ENDCASE.

        IF IT_ZSQM_NOTI_ST_LIST-SUCCESS = C_MARK.
          MOVE : 1 TO IT_MAT_ST_LIST-T_SUCCESS.
        ELSEIF IT_ZSQM_NOTI_ST_LIST-COMPLETED = C_MARK.
          MOVE : 1 TO IT_MAT_ST_LIST-T_COMPLETE.
        ELSEIF NOT IT_ZSQM_NOTI_ST_LIST-PLANDAT IS INITIAL AND
                   IT_ZSQM_NOTI_ST_LIST-COMPLETED IS INITIAL.
          MOVE : 1 TO IT_MAT_ST_LIST-T_RELEASE.
        ELSEIF  IT_ZSQM_NOTI_ST_LIST-PLANDAT IS INITIAL AND
                IT_ZSQM_NOTI_ST_LIST-COMPLETED  IS INITIAL.
          MOVE : 1 TO IT_MAT_ST_LIST-T_CREATION.
        ENDIF.

        COLLECT IT_MAT_ST_LIST.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    " GET_MAT_NOTI_ST_LIST
*&------------------------------------------------------------------*
*&      Form  COUNT_NOTI_ST_BASIC_LEVEL
*&------------------------------------------------------------------*
FORM COUNT_NOTI_ST_BASIC_LEVEL.
  REFRESH IT_ZSQM_NOTI_STATUS.

  LOOP AT IT_MAT_ST_LIST.
    CLEAR IT_ZSQM_NOTI_STATUS.
    MOVE-CORRESPONDING IT_MAT_ST_LIST TO IT_ZSQM_NOTI_STATUS.
    CLEAR : IT_ZSQM_NOTI_STATUS-MATNR.
    COLLECT IT_ZSQM_NOTI_STATUS.
  ENDLOOP.

ENDFORM.                    " COUNT_NOTI_ST_BASIC_LEVEL
*&-----------------------------------------------------------------*
*&      Form  GET_COUNT_DETAIL_MAT_ST
*&-----------------------------------------------------------------*
FORM GET_COUNT_DETAIL_MAT_ST USING    P_UCOMM
                                      P_QMGRP
                                      P_QMCOD
                                      P_EXTWG.
  REFRESH IT_ZSQM_NOTI_STATUS_D.

  CASE P_UCOMM.
    WHEN 'TO_MATNR'.
      LOOP AT IT_MAT_ST_LIST WHERE QMGRP = P_QMGRP
                               AND QMCOD = P_QMCOD.
        CLEAR IT_ZSQM_NOTI_STATUS_D.
        MOVE-CORRESPONDING IT_MAT_ST_LIST TO IT_ZSQM_NOTI_STATUS_D.
        CLEAR : IT_ZSQM_NOTI_STATUS_D-EXTWG,
                IT_ZSQM_NOTI_STATUS_D-QMCOD,
                IT_ZSQM_NOTI_STATUS_D-QMGRP.
        COLLECT IT_ZSQM_NOTI_STATUS_D.
      ENDLOOP.

    WHEN 'DETAIL'.
      LOOP AT IT_MAT_ST_LIST WHERE QMGRP = P_QMGRP
                               AND QMCOD = P_QMCOD
                               AND EXTWG = P_EXTWG.
        CLEAR IT_ZSQM_NOTI_STATUS_D.
        MOVE-CORRESPONDING IT_MAT_ST_LIST TO IT_ZSQM_NOTI_STATUS_D.
        CLEAR : IT_ZSQM_NOTI_STATUS_D-EXTWG,
                IT_ZSQM_NOTI_STATUS_D-QMCOD,
                IT_ZSQM_NOTI_STATUS_D-QMGRP.
        COLLECT IT_ZSQM_NOTI_STATUS_D.
      ENDLOOP.


  ENDCASE.

ENDFORM.                    " GET_COUNT_DETAIL_MAT_ST
*&------------------------------------------------------------------*
*&      Form  GET_DATA_AND_PROC_BASIC
*&-----------------------------------------------------------------*
FORM GET_DATA_AND_PROC_BASIC.

  PERFORM GET_DATA_USING_SEL_CRITERION.

  IF IT_ZSQM_NOTI_ST_LIST[] IS INITIAL.
    MESSAGE W000(ZMQM) WITH 'Data not found!'(W01).
    EXIT.
  ENDIF.

  PERFORM GET_MAT_NOTI_ST_LIST.

ENDFORM.                    " GET_DATA_AND_PROC_BASIC
*&------------------------------------------------------------------*
*&      Form  COUNT_NOTI_ST_CODING_LEVEL
*&------------------------------------------------------------------*
FORM COUNT_NOTI_ST_CODING_LEVEL.
  DATA : LW_INDEX LIKE SY-TABIX.

  REFRESH IT_ZSQM_NOTI_STATUS_C.

  LOOP AT IT_MAT_ST_LIST.
    CLEAR IT_ZSQM_NOTI_STATUS_C.
    MOVE-CORRESPONDING IT_MAT_ST_LIST TO IT_ZSQM_NOTI_STATUS_C.
    CLEAR : IT_ZSQM_NOTI_STATUS_C-MATNR,
            IT_ZSQM_NOTI_STATUS_C-EXTWG.
*            IT_ZSQM_NOTI_STATUS_C-QMGRP.
    COLLECT IT_ZSQM_NOTI_STATUS_C.
  ENDLOOP.



ENDFORM.                    " COUNT_NOTI_ST_CODING_LEVEL
*&------------------------------------------------------------------*
*&      Form  COUNT_NOTI_ST_EXTWG_LEVEL
*&------------------------------------------------------------------*
FORM COUNT_NOTI_ST_EXTWG_LEVEL USING  P_QMGRP
                                          P_QMCOD.

  REFRESH IT_ZSQM_NOTI_STATUS.

  LOOP AT IT_MAT_ST_LIST WHERE QMGRP = P_QMGRP
                           AND QMCOD = P_QMCOD.
    CLEAR IT_ZSQM_NOTI_STATUS.
    MOVE-CORRESPONDING IT_MAT_ST_LIST TO IT_ZSQM_NOTI_STATUS.
    CLEAR : IT_ZSQM_NOTI_STATUS-MATNR,
            IT_ZSQM_NOTI_STATUS-QMGRP,
            IT_ZSQM_NOTI_STATUS-QMCOD.
    COLLECT IT_ZSQM_NOTI_STATUS.
  ENDLOOP.

ENDFORM.                    " COUNT_NOTI_ST_EXTWG_LEVEL
*&-----------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA_EXT
*&-----------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA_EXT USING    P_INDEX.
  DATA : LW_SEL_INDEX   LIKE SY-TABIX.
  DATA : LW_TABLE_LINES LIKE SY-TABIX.

  DATA : LW_ZSQM_NOTI_STATUS  LIKE ZSQM_NOTI_STATUS,
         LW_ZSQM_NOTI_STATUS2 LIKE ZSQM_NOTI_STATUS2.

  LW_SEL_INDEX = P_INDEX.

  CLEAR LW_ZSQM_NOTI_STATUS.
  READ TABLE IT_ZSQM_NOTI_STATUS_C INDEX LW_SEL_INDEX
                                   INTO LW_ZSQM_NOTI_STATUS.

  WA_QMGRP = LW_ZSQM_NOTI_STATUS-QMGRP.
  WA_QMCOD = LW_ZSQM_NOTI_STATUS-QMCOD.
  WA_KURZTEXT_COD = LW_ZSQM_NOTI_STATUS-KURZTEXT_COD.

  PERFORM COUNT_NOTI_ST_EXTWG_LEVEL  USING WA_QMGRP
                                           WA_QMCOD.

  WA_LEVEL = C_EXTWG.

ENDFORM.                    " RETRIEV_DETAIL_DATA_EXT
*&------------------------------------------------------------------*
*&      Form  FILL_CODING_TEXT
*&------------------------------------------------------------------*
FORM FILL_CODING_TEXT.
  DATA LW_INDEX LIKE SY-TABIX.
*-- Get Coding Text
  LOOP AT IT_ZSQM_NOTI_STATUS_C.
    LW_INDEX = SY-TABIX.

    SELECT SINGLE  KURZTEXT_C INTO IT_ZSQM_NOTI_STATUS_C-KURZTEXT_COD
        FROM ZVQM_QPCT_EN
          WHERE KATALOGART  = C_CODING_CATEGORY
            AND CODEGRUPPE  = IT_ZSQM_NOTI_STATUS_C-QMGRP
            AND CODE        = IT_ZSQM_NOTI_STATUS_C-QMCOD
            AND SPRAS       = SY-LANGU
            AND SPRAS_CD    = SY-LANGU.

    MODIFY IT_ZSQM_NOTI_STATUS_C INDEX LW_INDEX.

  ENDLOOP.
ENDFORM.                    " FILL_CODING_TEXT
