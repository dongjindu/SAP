************************************************************************
* Program Name      : SAPMZRQM25R_NOTI_SUMMARY
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.27.
* Specifications By : SeungLyong, Lee
* Pattern           : 1.2.4 Call Screen + 3.1 General
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Notification Summary report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/19/2004 SLLEE        UD1K907648   Add Coding field(QM-20040318-002)
*
*
************************************************************************

REPORT  SAPMZRQM25R_NOTI_SUMMARY    .

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

TABLES :  TJ02T, "/System status texts
          TQ80_T.
TABLES : TPAR.
*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_NOTI_SEL,  "/Noti Status -Selection screen Structure
         ZSQM_NOTI_SUM,  "/Notification Summary list  - Structure
         ZSQM_NOTI_SUM_R. "/Notification Summary list for ALV Report
*<notes>:ZSQM_NOTI_SUM_R is copied from ZSQM_NOTI_SUM and delete some
*        fields for display Structure. IT_ZSQM_NOTI_SUM_R is a
*        internal table which line structure is sama as ZSQM_NOTI_SUM_R

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
CONSTANTS : C_FIRST(15)  TYPE C VALUE 'FIRST',
            C_DETAIL(15) TYPE C VALUE 'DETAIL'.

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

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Selection Criterion Text work area for Screen 9000.
DATA : BEGIN OF WA_SCR_TXT,
         TYPTX LIKE T370U-TYPTX, "/Equipment category Text
         NAME1 LIKE T001W-NAME1, "/Plant Text
         KTEXT LIKE CRTX-KTEXT,  "/Work center Text(view: 'M_CRAMN')
         CLINT LIKE KSML-CLINT,  "/Class Number
         ATNAM LIKE CABN-ATNAM,  "/Characteristic name
       END OF WA_SCR_TXT.

*-- Check Box Variables for Notification Status
DATA : BEGIN OF WA_ST,
         CRT  TYPE C   VALUE 'X',     "/Creation
         REL  TYPE C,     "/Release
         CMP  TYPE C,     "/Completion
       END OF WA_ST.


**-- List box variables
DATA: WA_NAME  TYPE VRM_ID,
      IT_LIST  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST.

*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT

*/-- Internale Tables with structure as sama as DB
DATA : IT_ZSQM_NOTI_SUM LIKE ZSQM_NOTI_SUM OCCURS 0
                                             WITH HEADER LINE,
       IT_ZSQM_NOTI_SUM_R LIKE ZSQM_NOTI_SUM_R OCCURS 0
                                             WITH HEADER LINE.


*//--- Internal tables for code Master
*-- Coding Code Text
DATA : BEGIN OF IT_QMCOD OCCURS 10,
        QMKAT    TYPE QMKAT,
        QMGRP    TYPE QMGRP,
        QMCOD    TYPE QMCOD,
        KURZTEXT TYPE QTXT_CODE,
       END OF IT_QMCOD.

*-- Defect type code
DATA : BEGIN OF IT_DEFECT OCCURS 10,
         FEKAT        TYPE FEKAT,
         FEGRP        TYPE FEGRP,
         FECOD        TYPE FECOD,
         KURZTEXT	TYPE QTXT_CODE,
       END OF IT_DEFECT.
*-- Cause code
DATA : BEGIN OF IT_CAUSE OCCURS 10,
         URKAT        TYPE URKAT,
         URGRP        TYPE URGRP,
         URCOD        TYPE URCOD,
         KURZTEXT	TYPE QTXT_CODE,
       END OF IT_CAUSE.
*-- Defect location code
DATA : BEGIN OF IT_DEFLOC OCCURS 10,
         OTKAT      TYPE  OTKAT,
         OTGRP      TYPE  OTGRP,
         OTEIL      TYPE  OTEIL,
         KURZTEXT	TYPE QTXT_CODE,
       END OF IT_DEFLOC.
*-- Activity type code
DATA : BEGIN OF IT_ACTIVITY OCCURS 10,
         KATART_AT  TYPE ZQKATART_AT,
         CODEGRP_AT TYPE ZQCODEGRP_AT,
         CODE_AT    TYPE ZQCODE_AT,
         KURZTEXT_AT	TYPE QTXT_CODE,
       END OF IT_ACTIVITY .

*-- System Status : TJ02T
DATA : IT_TJ02T  LIKE TJ02T OCCURS 10 WITH HEADER LINE.


*//Ranges; (R_)
*-- Selection variables
RANGES : R_MAWERK     FOR T001W-WERKS, "/Plant
         R_QMART      FOR QMEL-QMART,  "/Notification type
         R_PARNR_MAG  FOR IHPA-PARNR,  "/Manager
         R_LIFNUM     FOR QMEL-LIFNUM,  "/Vendor
         R_PARNR_VERA FOR IHPA-PARNR,   "/Resp. Department
         R_CODEGRP_VH FOR QMEL-CODEGRP_VH, "/Vehicle/Engine code. GROUP
         R_CODE_VH    FOR QMEL-CODE_VH,    "/Vehicle/Engine code
         R_ERDAT      FOR QMEL-ERDAT.      "/Created date

RANGES : R_QMGRP      FOR QMEL-QMGRP,  "/Code Group - Coding
         R_QMCOD      FOR QMEL-QMCOD.  "/Code - Coding


RANGES : R_STAT      FOR JEST-STAT, "/Notification Status.
         R_TASK_STAT FOR JEST-STAT. "/Noti. Task Status.
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

*-- Get Master Code table for Code Text(Defect type, Cause, Defect
*    location)
  PERFORM GET_CODE_MASTER.

*-- Set listbox for notification type(ZSQM_NOTI_SEL-QMART) in scr. 9000
  PERFORM SET_LISTBOX_QMART.

*-- Set default value for Vehicle/Engine catalog type
  ZSQM_NOTI_SEL-KATART_VH = C_VH_ENG_CATEGORY.

*-- Set default value for Coding catalog type
  ZSQM_NOTI_SEL-QMKAT = C_CODING_CATEGORY.

*INITIALIZATION.


***//Macro Definitions
*-- macro : RANGE_SET &1 &2 &3
*--           &1 - Range Variable
*--           &2 - Low Variable
*--           &3 - high Variable


****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS:

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN,

    HANDLE_TOOLBAR
        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,

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
*   READ TABLE IT_ZSQM_EQ_CLASS INDEX E_ROW-INDEX INTO LW_ZSQM_EQ_CLASS.

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

  ENDMETHOD.
*-------------------------------------------------------------------

*-- / Handling User defined commands for Toolbar
  METHOD HANDLE_USER_COMMAND.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.

    DATA : LT_ROWS   TYPE LVC_T_ROW,
           LW_LINE_ROW LIKE LINE OF LT_ROWS.
    DATA : LW_LINES TYPE I.

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
    PERFORM SET_TABLE_TO_ALV.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
*-   toolbar control event
    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD ALV_GRID->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.


  ENDIF.


  IF NOT GRID_CONTAINER IS INITIAL AND "/Not Created  for ALV GRID
     WA_RENEWAL_FLG = C_MARK.
    CLEAR WA_RENEWAL_FLG.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_TABLE_TO_ALV.


    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.
  ENDIF.

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
  IF WA_LEVEL = C_FIRST.
    SET TITLEBAR  '9100'.
  ELSE.
    SET TITLEBAR  '9100'.
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
*          PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
*-- Get data from DB AND Process
      PERFORM GET_DATA_AND_PROCESS.

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

      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      LEAVE TO SCREEN 0.

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
*  WA_IS_LAYOUT-DETAILTITL = ''.        "/Title bar of detail screen
*  WA_IS_LAYOUT-GRID_TITLE = ''.        "/ Title bar text
*  WA_IS_LAYOUT-KEYHOT      = C_MARK.    "/ Key columns as hotspot
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

ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZSQM_NOTI_SUM_R'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].


* Set field attribute

  LOOP AT PT_FIELDCAT.
    IF PT_FIELDCAT-FIELDNAME = 'QMNUM'.
      PT_FIELDCAT-KEY_SEL = C_MARK.
      PT_FIELDCAT-KEY      = C_MARK.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_COD'.
      PT_FIELDCAT-COLTEXT = 'Coding'(T49).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'CODEGRP_VH'.
*      PT_FIELDCAT-KEY_SEL = C_MARK.
      PT_FIELDCAT-COLTEXT = 'V/E Group'(T50).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'CODE_VH'.
      PT_FIELDCAT-COLTEXT = 'V/E'(T51).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'PARNR'.
      PT_FIELDCAT-COLTEXT = 'Vendor/Resp.Dept.'(T52).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR'.
      PT_FIELDCAT-OUTPUTLEN = 18.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'DEVICEID'.
      PT_FIELDCAT-COLTEXT = 'Device data'.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'MAKTX'.

    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_FE'.
      PT_FIELDCAT-COLTEXT = 'Defect type'(T53).
      PT_FIELDCAT-OUTPUTLEN = 20.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_UR'.
      PT_FIELDCAT-COLTEXT = 'Cause'(T54).
      PT_FIELDCAT-OUTPUTLEN = 20.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_OT'.
      PT_FIELDCAT-COLTEXT = 'Defect Location'(T55).
      PT_FIELDCAT-OUTPUTLEN = 20.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_AT'.
      PT_FIELDCAT-COLTEXT = 'Activity'(T56).
      PT_FIELDCAT-OUTPUTLEN = 20.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'TXT04'.
      PT_FIELDCAT-COLTEXT = 'Noti.Status'(T57).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'TASK_TXT04'.
      PT_FIELDCAT-COLTEXT = 'Task Status'(T58).
    ENDIF.
    MODIFY PT_FIELDCAT.
  ENDLOOP.

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
  PERFORM GET_PARTNER_VALUE   USING 'Z3'  "/Partner Function:Manager
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

      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'ONLI'.
*-- Set range variables for SQL
      PERFORM SET_RANGE_VAR_FOR_SEARCH.

*-- Get data from DB AND Process
      PERFORM GET_DATA_AND_PROCESS.

      CHECK NOT IT_ZSQM_NOTI_SUM[] IS INITIAL.

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
           R_CODEGRP_VH, R_CODE_VH,   R_ERDAT,  R_QMART,
           R_STAT, R_TASK_STAT,      R_QMGRP,      R_QMCOD.
  REFRESH :  R_MAWERK,     R_PARNR_MAG, R_LIFNUM, R_PARNR_VERA,
             R_CODEGRP_VH, R_CODE_VH,   R_ERDAT,  R_QMART,
             R_STAT, R_TASK_STAT,     R_QMGRP,      R_QMCOD.


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

*- Notification Status.
  IF WA_ST-CRT = C_MARK.
    RANGE_SET  R_STAT C_STAT_NOTI_CREATION ''.
  ENDIF.
  IF WA_ST-REL = C_MARK.
    RANGE_SET  R_STAT C_STAT_NOTI_RELEASE ''.
  ENDIF.
  IF WA_ST-CMP = C_MARK.
    RANGE_SET  R_STAT C_STAT_NOTI_COMPLETION ''.
  ENDIF.
  IF R_STAT[] IS INITIAL.
    MESSAGE W000(ZMQM) WITH
      'At least, one of Notification status must be selected.'(W02).
    STOP.
  ENDIF.

*-  Coding
  IF NOT ZSQM_NOTI_SEL-QMCOD IS INITIAL.
    RANGE_SET : R_QMGRP    ZSQM_NOTI_SEL-QMGRP '',
                R_QMCOD    ZSQM_NOTI_SEL-QMCOD ''.
  ENDIF.

*-- Notification Task Status
  CHECK ZSQM_NOTI_SEL-QMART = '1'.                          "/'Q1'.
  RANGE_SET  R_TASK_STAT   C_STAT_TASK_CREATION    ''.
  RANGE_SET  R_TASK_STAT   C_STAT_TASK_RELEASE     ''.
  RANGE_SET  R_TASK_STAT   C_STAT_TASK_COMPLETION  ''.
  RANGE_SET  R_TASK_STAT   C_STAT_TASK_SUCCESSFUL  ''.

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
           WHERE QMART = 'Q1'
             AND SPRAS = SY-LANGU.
    WHEN OTHERS.

      SELECT SINGLE QMARTX INTO ZSQM_NOTI_SEL-QMARTX
         FROM TQ80_T
           WHERE QMART = 'Q2'
             AND SPRAS = SY-LANGU.

      CLEAR TQ80_T.

      SELECT SINGLE *   FROM TQ80_T
           WHERE QMART = 'Q3'
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
    SELECT SINGLE NAME_TEXTC     INTO ZSQM_NOTI_SEL-NAME_LIST
        FROM USER_ADDR
           WHERE BNAME = ZSQM_NOTI_SEL-PARNR_MAG.
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
  WA_VALUE-KEY = '1'. WA_VALUE-TEXT = 'Q1'.
  APPEND WA_VALUE TO IT_LIST.
  WA_VALUE-KEY = '2'. WA_VALUE-TEXT = 'Q2'.
  APPEND WA_VALUE TO IT_LIST.
*  WA_VALUE-KEY = '3'. WA_VALUE-TEXT = 'Q3'.
*  APPEND WA_VALUE TO IT_LIST.
*  WA_VALUE-KEY = '4'. WA_VALUE-TEXT = 'Q2+Q3'.
*  APPEND WA_VALUE TO IT_LIST.

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

  DATA : LT_NOTI_SUM LIKE IT_ZSQM_NOTI_SUM OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF LT_NOTI_TASK OCCURS 0, "/task list of noti.
              QMNUM      TYPE QMNUM,
              MANUM      TYPE MANUM,
              TASK_STAT  TYPE J_STATUS,
              TASK_TXT04 TYPE J_TXT04,
         END OF LT_NOTI_TASK.

  DATA : LT_TASK_T LIKE LT_NOTI_TASK OCCURS 0 WITH HEADER LINE.

  DATA : LW_NOTI_INDEX LIKE SY-TABIX,
         LW_INDEX      LIKE SY-TABIX.

  REFRESH : IT_ZSQM_NOTI_SUM.


  CASE ZSQM_NOTI_SEL-QMART.

    WHEN '1'.

*     - Notification  list for Q1
      SELECT A~QMNUM A~QMART A~KATART_VH A~CODEGRP_VH A~CODE_VH
             A~MAWERK A~EXTWG A~KATART_AT A~CODEGRP_AT A~CODE_AT
             E~PARNR A~MATNR  I~MAKTX A~DEVICEID B~FEKAT B~FEGRP B~FECOD
             C~URKAT C~URGRP C~URCOD B~OTKAT  B~OTGRP B~OTEIL
             F~STAT
             A~QMGRP A~QMCOD
          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_NOTI_SUM
        FROM ( ( ( (  QMEL AS A  INNER JOIN QMFE AS B
           ON   A~QMNUM = B~QMNUM ) INNER JOIN QMUR AS C
           ON   A~QMNUM = C~QMNUM
            AND B~FENUM = C~FENUM ) INNER JOIN IHPA AS E
           ON   A~OBJNR = E~OBJNR ) INNER JOIN JEST AS F
           ON   A~OBJNR = F~OBJNR ) INNER JOIN MAKT AS I
           ON   A~MATNR = I~MATNR
        WHERE  A~MAWERK     IN R_MAWERK
          AND  A~QMART      IN R_QMART
          AND  A~KATART_VH  = ZSQM_NOTI_SEL-KATART_VH
          AND  A~CODEGRP_VH IN R_CODEGRP_VH
          AND  A~CODE_VH    IN R_CODE_VH
          AND  A~ERDAT      IN R_ERDAT
*          AND  A~QMKAT      = C_CODING_CATEGORY
          AND  A~QMGRP      IN R_QMGRP
          AND  A~QMCOD      IN R_QMCOD
          AND  B~FEGRP      IN (C_DEFECT_FEGRP1, C_DEFECT_FEGRP2)
          AND  E~PARNR      IN R_PARNR_VERA
          AND  E~PARVW      =  C_PARVW_RESP_DEP
          AND  F~INACT      = ' '
          AND  F~STAT       IN R_STAT
          AND  I~SPRAS      = SY-LANGU.



*-- Get task information of Notification  list for Q1

      SELECT A~QMNUM  G~MANUM
             H~STAT AS TASK_STAT
          INTO CORRESPONDING FIELDS OF TABLE LT_NOTI_TASK
        FROM  (  QMEL AS A  INNER JOIN QMSM AS G
           ON   A~QMNUM = G~QMNUM ) INNER JOIN JEST AS H
           ON   G~OBJNR = H~OBJNR
         FOR ALL ENTRIES IN  IT_ZSQM_NOTI_SUM
        WHERE  A~QMNUM      = IT_ZSQM_NOTI_SUM-QMNUM
          AND  H~INACT      = ' '
          AND  H~STAT       IN R_TASK_STAT
          AND  G~MANUM      = ( SELECT MAX( MANUM ) FROM QMSM
                                  WHERE QMNUM = A~QMNUM ).

*--   copy LT_NOTI_TASK to LT_TASK_T for Deleting not available task
      LT_TASK_T[] = LT_NOTI_TASK[].
*--    if successful status of notification task is exist,
*      delete completion task
      LOOP AT LT_TASK_T WHERE TASK_STAT = C_STAT_TASK_SUCCESSFUL .

        READ TABLE LT_NOTI_TASK
                       WITH KEY QMNUM     = LT_TASK_T-QMNUM
                                TASK_STAT = C_STAT_TASK_COMPLETION.
        IF SY-SUBRC = 0.
          LW_NOTI_INDEX = SY-TABIX.
          DELETE LT_NOTI_TASK INDEX LW_NOTI_INDEX.
        ENDIF.
      ENDLOOP.


*-- merge data
      LOOP AT LT_NOTI_TASK.
        CLEAR IT_ZSQM_NOTI_SUM.
        READ TABLE IT_ZSQM_NOTI_SUM WITH KEY QMNUM = LT_NOTI_TASK-QMNUM.
        IF SY-SUBRC = 0.
          LW_INDEX = SY-TABIX.
          MOVE-CORRESPONDING LT_NOTI_TASK TO IT_ZSQM_NOTI_SUM.
          MODIFY IT_ZSQM_NOTI_SUM INDEX LW_INDEX.
        ENDIF.
      ENDLOOP.


    WHEN OTHERS.
*     - Notification list for Q2, Q3 AND Q2 + Q3
      SELECT A~QMNUM A~QMART A~KATART_VH A~CODEGRP_VH A~CODE_VH
             A~MAWERK A~EXTWG A~KATART_AT A~CODEGRP_AT A~CODE_AT
             E~PARNR A~MATNR I~MAKTX A~DEVICEID F~STAT B~FEKAT
             B~FEGRP B~FECOD
             C~URKAT C~URGRP C~URCOD B~OTKAT  B~OTGRP B~OTEIL
             A~PLANDAT A~COMPLETED  A~SUCCESS
             A~QMGRP A~QMCOD
          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_NOTI_SUM
        FROM  ( ( ( (  QMEL AS A  INNER JOIN QMFE AS B
           ON   A~QMNUM = B~QMNUM ) INNER JOIN QMUR AS C
           ON   A~QMNUM = C~QMNUM
            AND B~FENUM = C~FENUM ) INNER JOIN IHPA AS E
           ON   A~OBJNR = E~OBJNR ) INNER JOIN JEST AS F
           ON   A~OBJNR = F~OBJNR ) INNER JOIN MAKT AS I
           ON   A~MATNR = I~MATNR
        WHERE  A~MAWERK     IN R_MAWERK
          AND  A~QMART      IN R_QMART
          AND  A~KATART_VH  = ZSQM_NOTI_SEL-KATART_VH
          AND  A~CODEGRP_VH IN R_CODEGRP_VH
          AND  A~CODE_VH    IN R_CODE_VH
          AND  A~ERDAT      IN R_ERDAT
*          AND  A~QMKAT      = C_CODING_CATEGORY
          AND  A~QMGRP      IN R_QMGRP
          AND  A~QMCOD      IN R_QMCOD
          AND  B~FEGRP      IN (C_DEFECT_FEGRP1, C_DEFECT_FEGRP2)
          AND  E~PARNR      IN R_LIFNUM
          AND  E~PARVW      =  C_PARVW_VENDOR
          AND  F~INACT      = ' '
          AND  F~STAT       IN R_STAT
          AND  I~SPRAS      = SY-LANGU.

      CHECK SY-SUBRC = 0.
*-- Fill Noti Task Status for Q2/Q3
      LOOP AT IT_ZSQM_NOTI_SUM.
        LW_NOTI_INDEX = SY-TABIX.

        IF IT_ZSQM_NOTI_SUM-SUCCESS = C_MARK.

          MOVE : C_STAT_TASK_SUCCESSFUL
                       TO  IT_ZSQM_NOTI_SUM-TASK_STAT.

        ELSEIF IT_ZSQM_NOTI_SUM-COMPLETED = C_MARK.

          MOVE : C_STAT_TASK_COMPLETION
                       TO IT_ZSQM_NOTI_SUM-TASK_STAT.

        ELSEIF NOT IT_ZSQM_NOTI_SUM-PLANDAT IS INITIAL AND
                   IT_ZSQM_NOTI_SUM-COMPLETED IS INITIAL.

          MOVE : C_STAT_TASK_RELEASE
                       TO IT_ZSQM_NOTI_SUM-TASK_STAT.

        ELSEIF  IT_ZSQM_NOTI_SUM-PLANDAT IS INITIAL AND
                IT_ZSQM_NOTI_SUM-COMPLETED  IS INITIAL.

          MOVE : C_STAT_TASK_CREATION
                       TO IT_ZSQM_NOTI_SUM-TASK_STAT.

        ENDIF.

        MODIFY IT_ZSQM_NOTI_SUM INDEX LW_NOTI_INDEX.
      ENDLOOP.


  ENDCASE.

ENDFORM.                    " GET_DATA_USING_SEL_CRITERION
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
    WHEN '2' OR '3' OR '4'.
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
*&-----------------------------------------------------------------*
*&      Form  GET_CODE_TEXT_AND_MODIFY
*&------------------------------------------------------------------*
FORM GET_CODE_TEXT_AND_MODIFY.

***- Coding Code
  LOOP AT IT_QMCOD.
    CLEAR IT_ZSQM_NOTI_SUM.
    MOVE : IT_QMCOD-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_COD.
    MODIFY IT_ZSQM_NOTI_SUM   TRANSPORTING KURZTEXT_COD
             WHERE QMNUM NE ''
               AND QMGRP = IT_QMCOD-QMGRP
               AND QMCOD = IT_QMCOD-QMCOD.
  ENDLOOP.

***-- Defect type code
  LOOP AT IT_DEFECT.
    CLEAR IT_ZSQM_NOTI_SUM.
*    MOVE : IT_DEFECT-FEKAT    TO IT_ZSQM_NOTI_SUM-FEKAT,
*           IT_DEFECT-FEGRP    TO IT_ZSQM_NOTI_SUM-FEGRP,
*           IT_DEFECT-FECOD    TO IT_ZSQM_NOTI_SUM-FECOD,
    MOVE : IT_DEFECT-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_FE.

    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_FE
                 WHERE QMNUM NE ''
                   AND FEKAT = IT_DEFECT-FEKAT
                   AND FEGRP = IT_DEFECT-FEGRP
                   AND FECOD = IT_DEFECT-FECOD.

  ENDLOOP.

***-- Cause code

  LOOP AT IT_CAUSE.
    CLEAR IT_ZSQM_NOTI_SUM.
*    MOVE : IT_CAUSE-URKAT    TO IT_ZSQM_NOTI_SUM-URKAT,
*           IT_CAUSE-URGRP    TO IT_ZSQM_NOTI_SUM-URGRP,
*           IT_CAUSE-URCOD    TO IT_ZSQM_NOTI_SUM-URCOD,
    MOVE : IT_CAUSE-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_UR.

    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_UR
                 WHERE QMNUM NE ''
                   AND URKAT = IT_CAUSE-URKAT
                   AND URGRP = IT_CAUSE-URGRP
                   AND URCOD = IT_CAUSE-URCOD.

  ENDLOOP.

***-- Defect location code
  LOOP AT IT_DEFLOC.
    CLEAR IT_ZSQM_NOTI_SUM.
*    MOVE : IT_DEFLOC-OTKAT    TO IT_ZSQM_NOTI_SUM-OTKAT,
*           IT_DEFLOC-OTGRP    TO IT_ZSQM_NOTI_SUM-OTGRP,
*           IT_DEFLOC-OTEIL    TO IT_ZSQM_NOTI_SUM-OTEIL,
    MOVE : IT_DEFLOC-KURZTEXT TO IT_ZSQM_NOTI_SUM-KURZTEXT_OT.

    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_OT
                 WHERE QMNUM NE ''
                   AND OTKAT = IT_DEFLOC-OTKAT
                   AND OTGRP = IT_DEFLOC-OTGRP
                   AND OTEIL = IT_DEFLOC-OTEIL.

  ENDLOOP.

**-- Activity type code

  LOOP AT IT_ACTIVITY.
    CLEAR IT_ZSQM_NOTI_SUM.
*    MOVE : IT_ACTIVITY-KATART_AT    TO IT_ZSQM_NOTI_SUM-KATART_AT,
*           IT_ACTIVITY-CODEGRP_AT   TO IT_ZSQM_NOTI_SUM-CODEGRP_AT,
*           IT_ACTIVITY-CODE_AT      TO IT_ZSQM_NOTI_SUM-CODE_AT,
    MOVE : IT_ACTIVITY-KURZTEXT_AT  TO IT_ZSQM_NOTI_SUM-KURZTEXT_AT.

    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING KURZTEXT_AT
                 WHERE QMNUM NE ''
                   AND KATART_AT  = IT_ACTIVITY-KATART_AT
                   AND CODEGRP_AT = IT_ACTIVITY-CODEGRP_AT
                   AND CODE_AT    = IT_ACTIVITY-CODE_AT .

  ENDLOOP.

*/-- System Status Text
*- Notification status text
  LOOP AT IT_TJ02T.
    CLEAR IT_ZSQM_NOTI_SUM.
    MOVE IT_TJ02T-TXT04 TO IT_ZSQM_NOTI_SUM-TXT04.
    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING TXT04
                 WHERE  STAT = IT_TJ02T-ISTAT.
  ENDLOOP.

*- Notification Task status text
*  IF ZSQM_NOTI_SEL-QMART = '1'.
  LOOP AT IT_TJ02T.
    CLEAR IT_ZSQM_NOTI_SUM.
    MOVE IT_TJ02T-TXT04 TO IT_ZSQM_NOTI_SUM-TASK_TXT04.
    MODIFY IT_ZSQM_NOTI_SUM  TRANSPORTING TASK_TXT04
                 WHERE TASK_STAT = IT_TJ02T-ISTAT.
  ENDLOOP.
*  ENDIF.

ENDFORM.                    " GET_CODE_TEXT_AND_MODIFY
*&------------------------------------------------------------------*
*&      Form  GET_CODE_MASTER
*&------------------------------------------------------------------*
FORM GET_CODE_MASTER.

**-- Coding Code Text
  SELECT KATALOGART AS QMKAT
         CODEGRUPPE AS QMGRP
         CODE       AS QMCOD
         KURZTEXT_C AS KURZTEXT
     INTO CORRESPONDING FIELDS OF TABLE IT_QMCOD
    FROM ZVQM_QPCT
      WHERE KATALOGART = C_CODING_CATEGORY
        AND SPRAS = SY-LANGU.

**-- Defect type code
  SELECT KATALOGART AS FEKAT
         CODEGRUPPE AS FEGRP
         CODE       AS FECOD
         KURZTEXT_C AS KURZTEXT
     INTO CORRESPONDING FIELDS OF TABLE IT_DEFECT
    FROM ZVQM_QPCT
      WHERE KATALOGART = '9'
        AND CODEGRUPPE IN (C_DEFECT_FEGRP1, C_DEFECT_FEGRP2)
        AND SPRAS = SY-LANGU.

**-- Cause code
  SELECT KATALOGART AS URKAT
         CODEGRUPPE AS URGRP
         CODE       AS URCOD
         KURZTEXT_C AS KURZTEXT
     INTO CORRESPONDING FIELDS OF TABLE IT_CAUSE
    FROM ZVQM_QPCT
      WHERE KATALOGART = '5'
        AND CODEGRUPPE LIKE C_CAUSE_URGRP
        AND SPRAS = SY-LANGU.

**-- Defect location code
  SELECT KATALOGART AS OTKAT
         CODEGRUPPE AS OTGRP
         CODE       AS OTEIL
         KURZTEXT_C AS KURZTEXT
     INTO CORRESPONDING FIELDS OF TABLE IT_DEFLOC
    FROM ZVQM_QPCT
      WHERE KATALOGART = 'E'
        AND CODEGRUPPE = C_DEF_LOC_OTGRP
        AND SPRAS = SY-LANGU.

*-- Activity type code
  SELECT KATALOGART AS KATART_AT
         CODEGRUPPE AS CODEGRP_AT
         CODE       AS CODE_AT
         KURZTEXT_C AS KURZTEXT_AT
     INTO CORRESPONDING FIELDS OF TABLE IT_ACTIVITY
    FROM ZVQM_QPCT
      WHERE KATALOGART = 'R'
*        AND CODEGRUPPE =
        AND SPRAS = SY-LANGU.

*-- System status code text
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TJ02T
    FROM TJ02T
      WHERE SPRAS = SY-LANGU
        AND ISTAT IN (C_STAT_NOTI_CREATION,
                     C_STAT_NOTI_RELEASE,
                     C_STAT_NOTI_COMPLETION,
                     C_STAT_TASK_CREATION,
                     C_STAT_TASK_RELEASE,
                     C_STAT_TASK_COMPLETION,
                     C_STAT_TASK_SUCCESSFUL).

ENDFORM.                    " GET_CODE_MASTER
*&------------------------------------------------------------------*
*&      Form  MOVE_DATA_FOR_DISPLAY
*&------------------------------------------------------------------*
FORM MOVE_DATA_FOR_DISPLAY.
  DATA : LW_NOTI_INDEX LIKE SY-TABIX.

  REFRESH IT_ZSQM_NOTI_SUM_R.

  LOOP AT IT_ZSQM_NOTI_SUM.
    CLEAR IT_ZSQM_NOTI_SUM_R.
    MOVE-CORRESPONDING IT_ZSQM_NOTI_SUM TO IT_ZSQM_NOTI_SUM_R.
    APPEND IT_ZSQM_NOTI_SUM_R.
  ENDLOOP.



ENDFORM.                    " MOVE_DATA_FOR_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SET_TABLE_TO_ALV
*&---------------------------------------------------------------------*
FORM SET_TABLE_TO_ALV.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'ZSQM_NOTI_SUM_R'
                 IS_LAYOUT        = WA_IS_LAYOUT
                 I_SAVE           = WA_SAVE
                 IS_VARIANT       = WA_VARIANT
                 I_DEFAULT        = C_MARK
*            IT_TOOLBAR_EXCLUDING  = <internal table of type
*                                                          UI_FUNCTIONS>
*            IT_HYPERLINK          = <internal table of type LVC_T_HYPE>
*            IT_ALV_GRAPHICS       = <internal table of type DTC_T_TC>
       CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*            IT_SORT               = <internal table of type LVC_T_SORT>
                 IT_OUTTAB        = IT_ZSQM_NOTI_SUM_R[].

*            IT_FILTER             = <internal table of type LVC_T_FILT>
ENDFORM.                    " SET_TABLE_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_INITIAL_VALUE OUTPUT.
*----- 01/20/2004 Appended by BSBAE

  CHECK ZSQM_NOTI_SEL-ERDAT_L IS INITIAL.

  CONCATENATE SY-DATUM(6) '01' INTO ZSQM_NOTI_SEL-ERDAT_L.
  MOVE: SY-DATUM TO ZSQM_NOTI_SEL-ERDAT_H.

ENDMODULE.                 " set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_AND_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_AND_PROCESS.

*-- Get data from DB
  PERFORM GET_DATA_USING_SEL_CRITERION.

  IF IT_ZSQM_NOTI_SUM[] IS INITIAL.
    MESSAGE W000(ZMQM) WITH 'Data not found!'(W01).
    EXIT.
  ENDIF.

*--  Get code Text and modify data
  PERFORM GET_CODE_TEXT_AND_MODIFY.

  PERFORM MOVE_DATA_FOR_DISPLAY.

ENDFORM.                    " GET_DATA_AND_PROCESS
