************************************************************************
* Program Name      : SAPMZRQM21R_EQUIP_SUMM
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.09.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Equipment Summary Reports
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZRQM21R_EQUIP_SUMM     .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
*TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.
INCLUDE ZQM_INCLUDE_POOL03. "/Equipment Include Module
*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : EQUI,   "/Equipment master data
         EQUZ,   "/Equipment time segment
         ILOA,   "/PM Object Location and Account Assignment
         CRHD,   "/Work Center Header
         AUSP,   "/Characteristic Values
         KSML,   "/Characteristics of a Class
         KLAH.   "/Class Header Data
TABLES : AFKO,
         AFIH,
         AUFK,
         JEST,
         CABN,
         T370T,
         TJ30T,
         EQKT,
         IHPA,
         T527X,
         QALS.

TABLES : T001W.
TABLES : ZVQM_EQ_SUMMARY. "/Equipment Summary List

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_EQ_SUMM.  "/Equipment List for Select criterion


*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
*CONTROLS: TC_9000  TYPE TABLEVIEW USING SCREEN 9000.

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
*CONSTANTS : C_CREATE(7)  TYPE C VALUE 'CREATE',
*            C_CHANGE(7)  TYPE C VALUE 'CHANGE',
*            C_DISPLAY(7) TYPE C VALUE 'DISPLAY'.


**-- Process Status
*CONSTANTS : " C_UPLOADED(8)  TYPE C VALUE 'UPLOADED',
*            C_SAVED(8)     TYPE C VALUE 'SAVED'.


**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

DATA : WA_LEVEL(15) TYPE C.
DATA : WA_RENEWAL_FLG.

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*-- Work area Variables in Program.
DATA : WA_CLASS TYPE KLASSE_D,  "/Selected Class for Detail
       WA_IMERK TYPE ATINN.     "/Selected Characteristic for Detail

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Selection Criterion Text work area for Screen 9000.
DATA : BEGIN OF WA_SCR_TXT,
         TYPTX LIKE T370U-TYPTX, "/Equipment category Text
*         NAME1 LIKE T001W-NAME1, "/Plant Text
*         KTEXT LIKE CRTX-KTEXT,  "/Work center Text(view: 'M_CRAMN')
*         CLINT LIKE KSML-CLINT,  "/Class Number
*         ATNAM LIKE CABN-ATNAM,  "/Characteristic name
       END OF WA_SCR_TXT.


*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT

*/-- Internale Tables with structure as sama as DB
DATA : IT_ZSQM_EQ_SUMM LIKE ZSQM_EQ_SUMM  OCCURS 0
                                        WITH HEADER LINE.
DATA : IT_ZSQM_EQ_SUMM_T LIKE ZSQM_EQ_SUMM OCCURS 0 WITH HEADER LINE.

*//Ranges; (R_)
*RANGES :

*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)
*FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL. "table control
*"                              Table_control Object(CXTAB)

*//Field Group;


* BDC Tables
DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.
DATA   BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA   END OF IT_MSG.

DATA : WA_BDC_MODE    TYPE TB_BDCMODE VALUE 'N',"/BDC Mode(A/E/N)
       WA_BDC_UPMODE  TYPE BDCUPMODE  VALUE 'S'."/BDC Update mode (A/S)



*// Class Definition
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

*// Declare reference variables, the container and internal table
DATA: CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.


* Global variables for attributes or etc of ALV GRID
DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO. "/The Layout Structure
DATA : IT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT


***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*- Equipment Category
SELECT-OPTIONS : S_EQTYP  FOR EQUI-EQTYP  NO-EXTENSION NO INTERVALS
                    DEFAULT 'Z' OBLIGATORY   MATCHCODE OBJECT  H_T370T
                    MODIF ID EQT.
*- Basic Start date
SELECT-OPTIONS : S_GSTRP FOR AFKO-GSTRP.
*- Lot create date
SELECT-OPTIONS : S_ERSTD FOR QALS-ERSTELDAT.
*- Equipment
SELECT-OPTIONS : S_EQUNR  FOR EQUI-EQUNR.
*- User status
SELECT-OPTIONS : S_STAT  FOR JEST-STAT.
*- Class
SELECT-OPTIONS : S_CLASS FOR KLAH-CLASS.
*- Characteristic
SELECT-OPTIONS : S_IMERK FOR KSML-IMERK.
*- Value
SELECT-OPTIONS : S_ATWRT FOR AUSP-ATWRT.
*- maintenance plant
SELECT-OPTIONS : S_SWERK FOR ILOA-SWERK NO-EXTENSION NO INTERVALS.
*- Work Center
SELECT-OPTIONS : S_ARBPL  FOR CRHD-ARBPL NO-EXTENSION
                                            MATCHCODE OBJECT CRAM.
*- Department
SELECT-OPTIONS : S_PARNR FOR IHPA-PARNR.
*- Maintenance plan No.
SELECT-OPTIONS : S_WARPL FOR EQUI-WARPL.
*- Order type
SELECT-OPTIONS : S_AUART FOR AUFK-AUART.
*- Order no.
SELECT-OPTIONS : S_AUFNR FOR AUFK-AUFNR.

SELECTION-SCREEN : SKIP 1,
                   ULINE,
                   SKIP 1.
SELECTION-SCREEN COMMENT 1(15) TEXT-001. "/Comfirm

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS : P_C_WWO TYPE C RADIOBUTTON GROUP CONF  DEFAULT 'X'.
SELECTION-SCREEN  COMMENT  3(15) TEXT-SS1 FOR FIELD P_C_WWO.

PARAMETERS : P_C_WIT TYPE C RADIOBUTTON GROUP CONF.
SELECTION-SCREEN  COMMENT  20(15) TEXT-SS2 FOR FIELD P_C_WIT.

PARAMETERS : P_C_WO TYPE C RADIOBUTTON GROUP CONF.
SELECTION-SCREEN  COMMENT  40(15) TEXT-SS3 FOR FIELD P_C_WO.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(15) TEXT-002. "/Usage Decision

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS : P_U_WWO TYPE C RADIOBUTTON GROUP UD  DEFAULT 'X'.
SELECTION-SCREEN  COMMENT  3(15) TEXT-SS1 FOR FIELD P_U_WWO.

PARAMETERS : P_U_WIT TYPE C RADIOBUTTON GROUP UD.
SELECTION-SCREEN  COMMENT  20(15) TEXT-SS2 FOR FIELD P_U_WIT.

PARAMETERS : P_U_WO TYPE C RADIOBUTTON GROUP UD.
SELECTION-SCREEN  COMMENT  40(15) TEXT-SS3 FOR FIELD P_U_WO.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN COMMENT 1(15) TEXT-003. "/UD long text

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS : P_L_WWO TYPE C RADIOBUTTON GROUP LONG  DEFAULT 'X'.
SELECTION-SCREEN  COMMENT  3(15) TEXT-SS1 FOR FIELD P_L_WWO.

PARAMETERS : P_L_WIT TYPE C RADIOBUTTON GROUP LONG.
SELECTION-SCREEN  COMMENT  20(15) TEXT-SS2 FOR FIELD P_L_WIT.

PARAMETERS : P_L_WO TYPE C RADIOBUTTON GROUP LONG.
SELECTION-SCREEN  COMMENT  40(15) TEXT-SS3 FOR FIELD P_L_WO.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK .

*//-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'EQT'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PARNR-LOW.
  PERFORM GET_PARTNER_VALUE   USING 'Z3'  "/Partner Function:Manager
                                   'S_PARNR-LOW'.
*                                    'ZSQM_NOTI_SEL-NAME_VERA'.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PARNR-HIGH.
  PERFORM GET_PARTNER_VALUE   USING 'Z3'  "/Partner Function:Manager
                                   'S_PARNR-HIGH'.
*                                    'ZSQM_NOTI_SEL-NAME_VERA'.

AT SELECTION-SCREEN.
  CHECK SY-UCOMM = 'ONLI'.
*--  Get Equipment Summary list
  IF S_EQTYP-LOW = 'Z'.
    PERFORM RET_EQUIP_SUMMARY_LIST.
  ELSEIF S_EQTYP-LOW = 'Y'.
    PERFORM RET_EQUIP_SUM_Y.
  ELSE.
    STOP.
  ENDIF.

  IF IT_ZSQM_EQ_SUMM[] IS INITIAL.
    MESSAGE E000(ZMQM) WITH 'No entries!'(E05).
    STOP.
  ENDIF.

  PERFORM GET_TEXT_FOR_SELECT_CRITERION.

  PERFORM ADD_MAINTENANCE_PLAN_DATA.


*-- Selection for Selection Screen
START-OF-SELECTION.


**-- End of Selection.
END-OF-SELECTION.
  CHECK NOT IT_ZSQM_EQ_SUMM[] IS INITIAL.
  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
*LOAD-OF-PROGRAM.


*INITIALIZATION.


***//Macro Definitions
*-- macro : macro_name &1 &2
*--           &1 -
*--           &2 -
*  DEFINE macro_name.
*  END-OF-DEFINITION.


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
    DATA: LW_ZSQM_EQ_SUMM LIKE LINE OF IT_ZSQM_EQ_SUMM.

* .The event DOUBLE_CLICK provides parameters for row and column
*   of the click. Use row parameter to select a line of the
*   corresponding internal table.

*- read selected row from internal table IT_ZSQM_EQ_SUMM
    READ TABLE IT_ZSQM_EQ_SUMM INDEX E_ROW-INDEX INTO LW_ZSQM_EQ_SUMM.

    PERFORM CALL_EQUIP_DISPLAY  USING LW_ZSQM_EQ_SUMM-EQUNR.

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method
    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

*         append a separator('3') to normal toolbar
    CLEAR LS_TOOLBAR.
    MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to show detail List of selected item.
    CLEAR LS_TOOLBAR.
    MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
    MOVE 'CONFIRM'                    TO LS_TOOLBAR-FUNCTION.
    MOVE ICON_WS_CONFIRM_WHSE_PROC_BACK  TO LS_TOOLBAR-ICON.
    MOVE 'Order Confirmation'(T12)    TO LS_TOOLBAR-QUICKINFO.
    MOVE 'Confirm'(T13)               TO LS_TOOLBAR-TEXT.
    MOVE ' '                          TO LS_TOOLBAR-DISABLED.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append a separator('3') to normal toolbar
    CLEAR LS_TOOLBAR.
    MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to show detail List of selected item.
    CLEAR LS_TOOLBAR.
    MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
    MOVE 'UD'                    TO LS_TOOLBAR-FUNCTION.
    MOVE ICON_ALLOW              TO LS_TOOLBAR-ICON.
    MOVE 'Usage Decision'(T14)   TO LS_TOOLBAR-QUICKINFO.
    MOVE 'Usage Decision'(T15)   TO LS_TOOLBAR-TEXT.
    MOVE ' '                     TO LS_TOOLBAR-DISABLED.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  ENDMETHOD.
*-------------------------------------------------------------------

*-- / Handling User defined commands for Toolbar
  METHOD HANDLE_USER_COMMAND.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.

    DATA : LT_ROWS   TYPE LVC_T_ROW,
           LW_LINE_ROW LIKE LINE OF LT_ROWS.
    DATA : LW_LINES TYPE I.

    WA_RENEWAL_FLG = C_MARK.

    CASE E_UCOMM.
*-- Confirm order Using order no.  It will be executed with background
*-- processing.  Transaction IW44
      WHEN 'CONFIRM'.
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

          PERFORM CONFIRM_ORDER_IW44  USING LW_LINE_ROW-INDEX.

        ENDIF.

      WHEN 'UD'.
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

          PERFORM RECORD_UD_QA11 USING LW_LINE_ROW-INDEX.

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

  CLEAR WA_SCR_TXT.
*-- Equipment category text
  IF NOT S_EQTYP-LOW IS INITIAL.
    SELECT SINGLE TYPTX INTO WA_SCR_TXT-TYPTX
       FROM T370U
         WHERE EQTYP = S_EQTYP-LOW
           AND SPRAS = SY-LANGU.
  ENDIF.

ENDFORM.                    " GET_TEXT_FOR_SELECT_CRITERION
*&------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.


  IF GRID_CONTAINER IS INITIAL. "/Not Created Container for ALV GRID

*- Create Container('GRID_CONTAINER') with Custom Contro on screen
    CREATE OBJECT GRID_CONTAINER
           EXPORTING CONTAINER_NAME = CUSTOM_CONTROL
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
                TXT1  = 'The control could not be created'(E02).
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
*   displayed key fields of IT_FIELDCAT
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_SUMM'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = C_MARK
*            IT_TOOLBAR_EXCLUDING  = <internal table of type
*                                                          UI_FUNCTIONS>
*            IT_HYPERLINK          = <internal table of type LVC_T_HYPE>
*            IT_ALV_GRAPHICS       = <internal table of type DTC_T_TC>
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_EQ_SUMM[].
*            IT_SORT               = <internal table of type LVC_T_SORT>
*            IT_FILTER             = <internal table of type LVC_T_FILT>


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
*&      Module  STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000' .

ENDMODULE.                 " STATUS_9000  OUTPUT
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
*&      Module  USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.

      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
      REFRESH IT_ZSQM_EQ_SUMM.
*--  Get Equipment Summary list
      PERFORM RET_EQUIP_SUMMARY_LIST.

      IF IT_ZSQM_EQ_SUMM[] IS INITIAL.
        MESSAGE W000(ZMQM) WITH 'No entries!'(E05).
      ENDIF.

      PERFORM ADD_MAINTENANCE_PLAN_DATA.

      WA_RENEWAL_FLG = C_MARK.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT INPUT.
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

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT.
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


ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.

*  LOOP AT SCREEN.
*    IF SCREEN-GROUP1 = 'CLA'.
*      SCREEN-ACTIVE = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.


ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.


* Build the fieldcat according to DDIC structure ZSQM_EQ_SUMM:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZSQM_EQ_SUMM'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

* Set field attribute
  LOOP AT PT_FIELDCAT.

    CASE PT_FIELDCAT-FIELDNAME.
      WHEN 'EQUNR'  .
        PT_FIELDCAT-KEY_SEL = C_MARK.
        PT_FIELDCAT-KEY      = C_MARK.
      WHEN 'EQKTX'.
        PT_FIELDCAT-EMPHASIZE = C_MARK.
      WHEN 'EQTYP' OR 'ATINN' OR 'ADZHL' OR 'CLINT' OR 'IMERK' OR
           'KLART' OR 'STATU' OR 'SPRAS' OR 'ILOAN' OR
           'AUTYP' OR 'STSMA' OR 'STAT' OR 'PARVW' OR 'RMZHL' OR
           'STAT_AUF'.
        PT_FIELDCAT-NO_OUT = C_MARK.

      WHEN OTHERS.

    ENDCASE.

    MODIFY PT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&------------------------------------------------------------------*
*&      Form  FREE_ALV_GRID
*&------------------------------------------------------------------*
FORM FREE_ALV_GRID.
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
*  DATA : LT_UI_FUNCTIONS TYPE UI_FUNCTIONS WITH HEADER LINE.
*
*      CLEAR LT_UI_FUNCTIONS. REFRESH LT_UI_FUNCTIONS.
*      MOVE : 'DETAIL' TO LT_UI_FUNCTIONS. APPEND LT_UI_FUNCTIONS.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_SUMM'
               IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = C_MARK
*                   IT_TOOLBAR_EXCLUDING  = LT_UI_FUNCTIONS[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_ZSQM_EQ_SUMM[].

ENDFORM.                    " SET_NEW_TABLE_DATA
*&------------------------------------------------------------------*
*&      Form  RET_EQUIP_SUMMARY_LIST
*&------------------------------------------------------------------*
FORM RET_EQUIP_SUMMARY_LIST.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : BEGIN OF LT_ZVQM_EQ_SUM_UD OCCURS 0.
          INCLUDE STRUCTURE ZVQM_EQ_SUM_UD .
  DATA :  VCODE  TYPE  QVCODE,
          VDATUM TYPE QVEDATUM,
          LTEXTKZ TYPE QLTEXTAVE,
        END OF  LT_ZVQM_EQ_SUM_UD.

  DATA : LW_FROMNUMBER LIKE NRIV-FROMNUMBER,
         LW_TONUMBER   LIKE NRIV-TONUMBER.

  REFRESH : IT_ZSQM_EQ_SUMM, IT_ZSQM_EQ_SUMM_T.


*- Retrieve Data.
  IF NOT S_EQUNR[] IS INITIAL.

    SELECT A~EQTYP A~EQUNR A~EQKTX A~EQART A~SWERK A~ILOAN A~ARBPL
          A~AUFNR A~AUTYP A~OBJ_AUF A~ANLNR A~KOSTL A~AUART A~GSTRP
        A~GLTRP A~RUECK A~STSMA A~STAT A~TXT04  A~ABCKZ A~SPRAS A~ATINN
          A~ADZHL A~ATWRT A~ATNAM A~CLINT A~IMERK A~KLART A~CLASS
          A~STATU A~BRGEW A~GEWEI A~GROES A~INBDT A~ANSWT A~WAERS
          A~HERST A~TYPBZ A~PARVW A~PARNR A~ORGTX
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM
        FROM ZVQM_EQ_SUMMARY AS A
            WHERE A~EQTYP =  S_EQTYP-LOW
*               AND A~EQART EQ 'QM_1000'
               AND A~EQART IN ('QM_1000', 'QM_2000')
               AND A~GSTRP IN S_GSTRP  AND A~EQUNR IN S_EQUNR
               AND A~STAT  IN S_STAT   AND A~CLASS IN S_CLASS
               AND A~IMERK IN S_IMERK  AND A~ATWRT IN S_ATWRT
               AND A~SWERK IN S_SWERK  AND A~ARBPL IN S_ARBPL
               AND A~PARNR IN S_PARNR  AND A~AUART IN S_AUART
               AND A~AUFNR IN S_AUFNR  AND A~WARPL IN S_WARPL
               AND A~SPRAS = SY-LANGU
               AND A~ENDDA = '99991231'.

  ELSE.
*-   If EQUNR wsa not inputed, Get Equipment Number ranges data
*-   and It will be used in SQL. (LW_FROMNUMBER, LW_TONUMBER)
    SELECT SINGLE B~FROMNUMBER B~TONUMBER
        INTO (LW_FROMNUMBER, LW_TONUMBER)
          FROM T370T AS A INNER JOIN NRIV AS B
            ON  A~NUMKI = B~NRRANGENR
            WHERE  A~EQTYP  = S_EQTYP-LOW
               AND B~OBJECT = 'EQUIP_NR'.

    SELECT A~EQTYP A~EQUNR A~EQKTX A~EQART A~SWERK A~ILOAN A~ARBPL
          A~AUFNR A~AUTYP A~OBJ_AUF A~ANLNR A~KOSTL A~AUART A~GSTRP
        A~GLTRP A~RUECK A~STSMA A~STAT A~TXT04  A~ABCKZ A~SPRAS A~ATINN
          A~ADZHL A~ATWRT A~ATNAM A~CLINT A~IMERK A~KLART A~CLASS
          A~STATU A~BRGEW A~GEWEI A~GROES A~INBDT A~ANSWT A~WAERS
          A~HERST A~TYPBZ A~PARVW A~PARNR A~ORGTX A~WARPL
      INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM
          FROM ZVQM_EQ_SUMMARY AS A
             WHERE A~EQTYP =  S_EQTYP-LOW
               AND A~GSTRP IN S_GSTRP
*               AND A~EQART EQ 'QM_1000'
                AND A~EQART IN ('QM_1000', 'QM_2000')
               AND A~EQUNR BETWEEN LW_FROMNUMBER AND LW_TONUMBER
               AND A~STAT  IN S_STAT   AND A~CLASS IN S_CLASS
               AND A~IMERK IN S_IMERK  AND A~ATWRT IN S_ATWRT
               AND A~SWERK IN S_SWERK  AND A~ARBPL IN S_ARBPL
               AND A~PARNR IN S_PARNR  AND A~AUART IN S_AUART
               AND A~AUFNR IN S_AUFNR  AND A~WARPL IN S_WARPL
               AND A~SPRAS = SY-LANGU
               AND A~ENDDA = '99991231'.

  ENDIF.

  CHECK SY-SUBRC = 0.

**-   Inspection Lot AND UD
**  SELECT A~EQTYP A~EQUNR A~AUFNR A~AUTYP A~AUART
**         A~GSTRP A~GLTRP A~PRUEFLOS A~ERSTELDAT
**         A~RUECK A~RMZHL A~BUDAT
**         B~VCODE B~LTEXTKZ B~VDATUM
**   INTO CORRESPONDING FIELDS OF TABLE LT_ZVQM_EQ_SUM_UD
**      FROM ZVQM_EQ_SUM_UD AS A LEFT OUTER JOIN QAVE AS B
**       ON A~PRUEFLOS = B~PRUEFLOS
**        FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
**          WHERE  A~EQTYP =  S_EQTYP-LOW
**             AND A~GSTRP IN S_GSTRP
**             AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
**             AND A~AUART =  IT_ZSQM_EQ_SUMM-AUART
**             AND A~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
**             AND A~ERSTELDAT IN S_ERSTD.

*  SELECT A~EQTYP A~EQUNR B~AUFNR C~AUTYP C~AUART
*         D~GSTRP D~GLTRP F~PRUEFLOS F~ERSTELDAT
*         E~RUECK E~RMZHL E~BUDAT
*         K~VCODE K~LTEXTKZ K~VDATUM
*   INTO CORRESPONDING FIELDS OF TABLE LT_ZVQM_EQ_SUM_UD
*      FROM ( ( ( ( ( ( EQUI  AS A INNER JOIN AFIH AS B
*         ON A~EQUNR  = B~EQUNR ) INNER JOIN AUFK AS C
*         ON B~AUFNR  = C~AUFNR ) INNER JOIN AFKO AS D
*         ON C~AUFNR  = D~AUFNR ) INNER JOIN AFRU AS E
*         ON C~AUFNR  = E~AUFNR ) INNER JOIN QALS AS F
*         ON C~AUFNR  = F~AUFNR ) INNER JOIN AUSP AS G
*         ON A~EQUNR  = G~OBJEK
*                               )  LEFT OUTER JOIN QAVE AS K
*         ON F~PRUEFLOS = K~PRUEFLOS
*        FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
*          WHERE  A~EQTYP =  S_EQTYP-LOW
*             AND D~GSTRP IN S_GSTRP
*             AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
*             AND C~AUART =  IT_ZSQM_EQ_SUMM-AUART
*             AND B~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
*             AND C~LOEKZ =  ' '
*             AND A~LVORM EQ ' '
*             AND A~EQART EQ 'QM_1000'
*             AND E~RMZHL EQ '00000001'
*             AND F~ERSTELDAT IN S_ERSTD.

  SELECT A~EQTYP A~EQUNR B~AUFNR C~AUTYP C~AUART
*         D~GSTRP D~GLTRP
         F~PRUEFLOS F~ERSTELDAT
*         E~RUECK E~RMZHL E~BUDAT
         K~VCODE K~LTEXTKZ K~VDATUM
   INTO CORRESPONDING FIELDS OF TABLE LT_ZVQM_EQ_SUM_UD
      FROM ( ( ( (  EQUI  AS A INNER JOIN AFIH AS B
         ON A~EQUNR  = B~EQUNR ) INNER JOIN AUFK AS C
         ON B~AUFNR  = C~AUFNR ) INNER JOIN QALS AS F
         ON C~AUFNR  = F~AUFNR ) INNER JOIN AUSP AS G
         ON A~EQUNR  = G~OBJEK
                               )  LEFT OUTER JOIN QAVE AS K
         ON F~PRUEFLOS = K~PRUEFLOS
        FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
          WHERE  A~EQTYP =  S_EQTYP-LOW
             AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
             AND C~AUART =  IT_ZSQM_EQ_SUMM-AUART
             AND B~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
             AND C~LOEKZ =  ' '
             AND A~LVORM EQ ' '
*             AND A~EQART EQ 'QM_1000'
             AND A~EQART IN ('QM_1000', 'QM_2000')
             AND F~ERSTELDAT IN S_ERSTD.


  DATA : BEGIN OF LT_AFKO OCCURS 0,
          AUFNR  TYPE AUFNR,
          RUECK  TYPE CO_RUECK,
          GSTRP  TYPE CO_GSTRP,
          GLTRP  TYPE CO_GLTRP,
          RMZHL  TYPE CO_RMZHL,
          BUDAT  TYPE BUDAT,
         END OF LT_AFKO.

  SELECT A~AUFNR E~RUECK A~GSTRP A~GLTRP E~RMZHL E~BUDAT
    INTO CORRESPONDING FIELDS OF TABLE LT_AFKO
     FROM AFKO AS A  INNER JOIN AFRU AS E
        ON A~AUFNR  = E~AUFNR
       FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
      WHERE A~AUFNR = IT_ZSQM_EQ_SUMM-AUFNR
        AND A~GSTRP IN S_GSTRP
        AND E~RMZHL EQ '00000001'.

  LOOP AT LT_AFKO.

    READ TABLE LT_ZVQM_EQ_SUM_UD WITH KEY AUFNR = LT_AFKO-AUFNR.

    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      MOVE-CORRESPONDING LT_AFKO TO LT_ZVQM_EQ_SUM_UD.
      MODIFY LT_ZVQM_EQ_SUM_UD INDEX LW_INDEX.
    ENDIF.

  ENDLOOP.



*-- Merge UD data
  LOOP AT LT_ZVQM_EQ_SUM_UD.
    READ TABLE IT_ZSQM_EQ_SUMM
              WITH KEY EQTYP = LT_ZVQM_EQ_SUM_UD-EQTYP
                       EQUNR = LT_ZVQM_EQ_SUM_UD-EQUNR
                       AUFNR = LT_ZVQM_EQ_SUM_UD-AUFNR.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      MOVE:LT_ZVQM_EQ_SUM_UD-VCODE   TO IT_ZSQM_EQ_SUMM-VCODE,
        LT_ZVQM_EQ_SUM_UD-LTEXTKZ TO IT_ZSQM_EQ_SUMM-LTEXTKZ,
        LT_ZVQM_EQ_SUM_UD-PRUEFLOS TO IT_ZSQM_EQ_SUMM-PRUEFLOS,
        LT_ZVQM_EQ_SUM_UD-ERSTELDAT TO IT_ZSQM_EQ_SUMM-ERSTELDAT,
        LT_ZVQM_EQ_SUM_UD-RUECK TO IT_ZSQM_EQ_SUMM-RUECK,
        LT_ZVQM_EQ_SUM_UD-RMZHL TO IT_ZSQM_EQ_SUMM-RMZHL,
        LT_ZVQM_EQ_SUM_UD-BUDAT TO IT_ZSQM_EQ_SUMM-BUDAT,
        LT_ZVQM_EQ_SUM_UD-VDATUM TO IT_ZSQM_EQ_SUMM-VDATUM.
      MODIFY IT_ZSQM_EQ_SUMM INDEX LW_INDEX.
    ENDIF.
  ENDLOOP.

  CHECK NOT IT_ZSQM_EQ_SUMM[] IS INITIAL.

*- 2. FILL Confirm Status Field of order(AUFNR)- STAT_AUF

*  SELECT A~EQTYP A~EQUNR  A~AUFNR
*         B~STAT AS STAT_AUF
*   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM_T
*      FROM ZVQM_EQ_SUM_UD AS A INNER JOIN JEST AS B
*        ON A~OBJ_AUF = B~OBJNR
*         FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
*          WHERE  A~EQTYP =  S_EQTYP-LOW
*               AND A~GSTRP IN S_GSTRP
*               AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
*               AND A~AUART =  IT_ZSQM_EQ_SUMM-AUART
*               AND A~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
*               AND B~STAT = C_ORDER_STATUS_CONFIRM. "/CONFIRM STATUS.

  SELECT A~EQTYP A~EQUNR  B~AUFNR
         F~STAT AS STAT_AUF
   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM_T
      FROM ( ( ( ( EQUI  AS A INNER JOIN AFIH AS B
         ON A~EQUNR  = B~EQUNR ) INNER JOIN AUFK AS C
         ON B~AUFNR  = C~AUFNR ) INNER JOIN AFKO AS D
         ON C~AUFNR  = D~AUFNR ) INNER JOIN QALS AS E
         ON C~AUFNR  = E~AUFNR ) INNER JOIN JEST AS F
         ON C~OBJNR  = F~OBJNR
         FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
          WHERE  A~EQTYP =  S_EQTYP-LOW
               AND D~GSTRP IN S_GSTRP
               AND A~EQUNR = IT_ZSQM_EQ_SUMM-EQUNR
               AND C~AUART = IT_ZSQM_EQ_SUMM-AUART
               AND C~AUFNR = IT_ZSQM_EQ_SUMM-AUFNR
               AND F~STAT  = C_ORDER_STATUS_CONFIRM "/CONFIRM STATUS.
               AND C~LOEKZ EQ ' '
               AND A~LVORM EQ ' '
*               AND A~EQART EQ 'QM_1000'
               AND A~EQART IN ('QM_1000', 'QM_2000').

*  CHECK SY-SUBRC = 0.

  LOOP AT IT_ZSQM_EQ_SUMM_T.
    CLEAR IT_ZSQM_EQ_SUMM.
    READ TABLE IT_ZSQM_EQ_SUMM
               WITH KEY EQTYP = IT_ZSQM_EQ_SUMM_T-EQTYP
                        EQUNR = IT_ZSQM_EQ_SUMM_T-EQUNR
                        AUFNR = IT_ZSQM_EQ_SUMM_T-AUFNR.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      CASE C_MARK.
        WHEN P_C_WWO OR P_C_WIT. "/ With or Without/ WITH
          MOVE : IT_ZSQM_EQ_SUMM_T-STAT_AUF
                             TO IT_ZSQM_EQ_SUMM-STAT_AUF.
          MODIFY IT_ZSQM_EQ_SUMM INDEX LW_INDEX.
        WHEN P_C_WO.  " Without.
          DELETE IT_ZSQM_EQ_SUMM INDEX LW_INDEX.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDLOOP.


  IF    P_C_WIT = C_MARK              AND
    NOT IT_ZSQM_EQ_SUMM[] IS INITIAL.
    DELETE IT_ZSQM_EQ_SUMM WHERE STAT_AUF = ' '.
  ENDIF.

*- Usage Decision
  CASE C_MARK.
    WHEN P_U_WWO.
    WHEN P_U_WIT.
      DELETE IT_ZSQM_EQ_SUMM WHERE VCODE = ''.
    WHEN P_U_WO.
      DELETE IT_ZSQM_EQ_SUMM WHERE VCODE NE ''.
  ENDCASE.

*- Long Text of Usage Decision
  CASE C_MARK.
    WHEN P_L_WWO.
    WHEN P_L_WIT.
      DELETE IT_ZSQM_EQ_SUMM WHERE LTEXTKZ = ' '.
    WHEN P_L_WO.
      DELETE IT_ZSQM_EQ_SUMM WHERE LTEXTKZ  = 'X'.
  ENDCASE.


ENDFORM.                    " RET_EQUIP_SUMMARY_LIST
*&------------------------------------------------------------------*
*&      Form  RECORD_UD_QA11
*&------------------------------------------------------------------*
FORM RECORD_UD_QA11 USING    P_INDEX.

  CLEAR IT_ZSQM_EQ_SUMM.
  READ TABLE IT_ZSQM_EQ_SUMM INDEX P_INDEX.

  CHECK     SY-SUBRC = 0                        AND
            IT_ZSQM_EQ_SUMM-VCODE    IS INITIAL AND
       NOT  IT_ZSQM_EQ_SUMM-PRUEFLOS IS INITIAL.

  SET PARAMETER   ID 'QLS' FIELD IT_ZSQM_EQ_SUMM-PRUEFLOS.

  CALL TRANSACTION 'QA11' AND SKIP FIRST SCREEN .

  SET PARAMETER   ID 'QLS' FIELD ''.

  SELECT SINGLE VCODE VDATUM LTEXTKZ
      INTO CORRESPONDING FIELDS OF IT_ZSQM_EQ_SUMM
        FROM QAVE
          WHERE PRUEFLOS = IT_ZSQM_EQ_SUMM-PRUEFLOS.

  CHECK SY-SUBRC = 0.
  MODIFY IT_ZSQM_EQ_SUMM INDEX P_INDEX.

  WA_RENEWAL_FLG = C_MARK.

ENDFORM.                    " RECORD_UD_QA11
*&-----------------------------------------------------------------*
*&      Form  CONFIRM_ORDER_IW44
*&-----------------------------------------------------------------*
FORM CONFIRM_ORDER_IW44 USING    P_INDEX.
  DATA : LW_INDEX LIKE SY-TABIX.
  CLEAR IT_ZSQM_EQ_SUMM.
  READ TABLE IT_ZSQM_EQ_SUMM INDEX P_INDEX.

  LW_INDEX = SY-TABIX.

  CHECK     SY-SUBRC = 0                        AND
            IT_ZSQM_EQ_SUMM-RUECK    IS INITIAL AND
       NOT  IT_ZSQM_EQ_SUMM-AUFNR    IS INITIAL AND
            IT_ZSQM_EQ_SUMM-STAT_AUF IS INITIAL.

  REFRESH BDC_TAB.

  PERFORM DYNPRO  USING:
       'X'   'SAPLCORU'          '3350',
       ' '   'BDC_OKCODE'        '/00',
       ' '   'AFRUD-AUFNR(01)'   IT_ZSQM_EQ_SUMM-AUFNR,
       ' '   'AFRUD-VORNR(01)'	 '0010'.

  PERFORM DYNPRO  USING:
       'X'   'SAPLCORU'          '3350',
       ' '   'BDC_OKCODE'        '=BU'.

*- TRANSACTION  IW44
  REFRESH IT_MSG.
  CALL TRANSACTION 'IW44'    USING  BDC_TAB
                             UPDATE WA_BDC_UPMODE
                             MODE   WA_BDC_MODE
                             MESSAGES INTO IT_MSG.

*  READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
*
*  IF SY-SUBRC = 0.
*    ROLLBACK WORK.
*    MESSAGE E000(ZMQM) WITH 'BDC Error Founded!'(E10).
*    EXIT.
*  ENDIF.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'S'
                             MSGNR  = '197'.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMQM)
         WITH 'Order Confirmation not be created, Please Check!'(E11).
    EXIT.
  ELSE.

*    - Fill confirmation Data
    PERFORM GET_CONFIRM_DATA_SEL.
    MODIFY IT_ZSQM_EQ_SUMM INDEX LW_INDEX.

    MESSAGE S000(ZMQM)
         WITH 'Order Confirmation created,'(S02) IT_ZSQM_EQ_SUMM-AUFNR.
  ENDIF.

ENDFORM.                    " CONFIRM_ORDER_IW44
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-PROGRAM,
           VALUE TO BDC_TAB-DYNPRO,
           'X'   TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-FNAM,
           VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.                    "DYNPRO
*&------------------------------------------------------------------*
*&      Form  GET_CONFIRM_DATA_SEL
*&------------------------------------------------------------------*
FORM GET_CONFIRM_DATA_SEL.

*-  FILL Confirm data and Status Field of order(AUFNR)- STAT_AUF
  SELECT SINGLE
         A~PRUEFLOS A~ERSTELDAT
         A~RUECK A~RMZHL A~BUDAT
         B~STAT AS STAT_AUF
   INTO CORRESPONDING FIELDS OF  IT_ZSQM_EQ_SUMM
      FROM ZVQM_EQ_SUM_UD AS A INNER JOIN JEST AS B
        ON A~OBJ_AUF = B~OBJNR
          WHERE  A~EQTYP =  IT_ZSQM_EQ_SUMM-EQTYP
             AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
             AND A~AUART =  IT_ZSQM_EQ_SUMM-AUART
             AND A~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
             AND B~STAT = 'I0009'. "/cONFIRM STATUS..


ENDFORM.                    " GET_CONFIRM_DATA_SEL
*&-----------------------------------------------------------------*
*&      Form  GET_PARTNER_VALUE
*&-----------------------------------------------------------------*
FORM GET_PARTNER_VALUE USING    VALUE(P_PARVW)
                                VALUE(P_PARNR_FIELD).
*                                VALUE(P_NAME_FIELD).
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
*  LW_DYNPFIELDS-FIELDNAME  = P_NAME_FIELD.
*  LW_DYNPFIELDS-FIELDVALUE = LW_NAME_LIST.
*  APPEND LW_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = LW_DYNAME
            DYNUMB     = LW_DYNUMB
       TABLES
            DYNPFIELDS = LW_DYNPFIELDS.

ENDFORM.                    " GET_PARTNER_VALUE
*&---------------------------------------------------------------------*
*&      Form  CALL_EQUIP_DISPLAY
*&---------------------------------------------------------------------*
FORM CALL_EQUIP_DISPLAY USING    P_EQUNR TYPE EQUNR.

  SET PARAMETER ID 'EQN' FIELD P_EQUNR.

  CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.

  SET PARAMETER ID 'EQN' FIELD ''.

ENDFORM.                    " CALL_EQUIP_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  add_maintenance_plan_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_MAINTENANCE_PLAN_DATA.
  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_WARPL LIKE MPLA-WARPL,
         LW_ZYKL1 LIKE MMPT-ZYKL1,
         LW_ZEIEH LIKE MMPT-ZEIEH.

  LOOP AT IT_ZSQM_EQ_SUMM.
    LW_INDEX = SY-TABIX.

    CLEAR : LW_WARPL, LW_ZYKL1, LW_ZEIEH.

    SELECT  SINGLE B~WARPL C~ZYKL1 C~ZEIEH
            INTO (LW_WARPL, LW_ZYKL1, LW_ZEIEH)
            FROM AFIH AS A
                 INNER JOIN MPLA AS B
                 ON A~WARPL = B~WARPL
                    INNER JOIN MMPT AS C
                    ON B~WARPL = C~WARPL
            WHERE A~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR.


    IF NOT LW_ZEIEH IS INITIAL.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
           EXPORTING
                CHAR_UNIT       = LW_ZEIEH
                DECIMALS        = 0
                EXPONENT        = 0
                FLTP_VALUE_SI   = LW_ZYKL1
                INDICATOR_VALUE = 'X'
                MASC_SYMBOL     = ' '
           IMPORTING
                CHAR_VALUE      = IT_ZSQM_EQ_SUMM-ZYKL1.
    ENDIF.
    MOVE LW_WARPL TO IT_ZSQM_EQ_SUMM-WARPL.
    MOVE LW_ZEIEH TO IT_ZSQM_EQ_SUMM-ZEIEH.

    MODIFY IT_ZSQM_EQ_SUMM INDEX LW_INDEX.
  ENDLOOP.
ENDFORM.                    " add_maintenance_plan_data
*&------------------------------------------------------------------*
*&      Form  RET_EQUIP_SUM_Y
*&------------------------------------------------------------------*
FORM RET_EQUIP_SUM_Y.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : BEGIN OF LT_ZVQM_EQ_SUM_UD OCCURS 0.
          INCLUDE STRUCTURE ZVQM_EQ_SUM_UD .
  DATA :  VCODE  TYPE  QVCODE,
          VDATUM TYPE QVEDATUM,
          LTEXTKZ TYPE QLTEXTAVE,
        END OF  LT_ZVQM_EQ_SUM_UD.

  DATA : LW_FROMNUMBER LIKE NRIV-FROMNUMBER,
         LW_TONUMBER   LIKE NRIV-TONUMBER.

  REFRESH : IT_ZSQM_EQ_SUMM, IT_ZSQM_EQ_SUMM_T.


*- Retrieve Data.
  IF NOT S_EQUNR[] IS INITIAL.

*    SELECT A~EQTYP A~EQUNR A~EQKTX A~EQART A~SWERK A~ILOAN A~ARBPL
*          A~AUFNR A~AUTYP A~OBJ_AUF A~ANLNR A~KOSTL A~AUART A~GSTRP
*        A~GLTRP A~RUECK A~STSMA A~STAT A~TXT04  A~ABCKZ A~SPRAS A~ATINN
*          A~ADZHL A~ATWRT A~ATNAM A~CLINT A~IMERK A~KLART A~CLASS
*          A~STATU A~BRGEW A~GEWEI A~GROES A~INBDT A~ANSWT A~WAERS
*          A~HERST A~TYPBZ A~PARVW A~PARNR A~ORGTX
*     INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM
*        FROM ZVQM_EQ_SUMMARY AS A
*            WHERE A~EQTYP =  S_EQTYP-LOW
*               AND A~EQART EQ 'QM_1000'
*               AND A~GSTRP IN S_GSTRP  AND A~EQUNR IN S_EQUNR
*               AND A~STAT  IN S_STAT   AND A~CLASS IN S_CLASS
*               AND A~IMERK IN S_IMERK  AND A~ATWRT IN S_ATWRT
*               AND A~SWERK IN S_SWERK  AND A~ARBPL IN S_ARBPL
*               AND A~PARNR IN S_PARNR  AND A~AUART IN S_AUART
*               AND A~AUFNR IN S_AUFNR  AND A~WARPL IN S_WARPL
*               AND A~SPRAS = SY-LANGU
*               AND A~ENDDA = '99991231'.

  ELSE.
*-   If EQUNR wsa not inputed, Get Equipment Number ranges data
*-   and It will be used in SQL. (LW_FROMNUMBER, LW_TONUMBER)
    SELECT SINGLE B~FROMNUMBER B~TONUMBER
        INTO (LW_FROMNUMBER, LW_TONUMBER)
          FROM T370T AS A INNER JOIN NRIV AS B
            ON  A~NUMKI = B~NRRANGENR
            WHERE  A~EQTYP  = S_EQTYP-LOW
               AND B~OBJECT = 'EQUIP_NR'.

    SELECT A~EQTYP A~EQUNR A~EQKTX A~EQART
          A~AUFNR A~AUTYP A~OBJ_AUF  A~AUART A~GSTRP
        A~GLTRP A~RUECK A~STSMA  A~SPRAS
          A~BRGEW A~GEWEI A~GROES A~INBDT A~ANSWT A~WAERS
          A~HERST A~TYPBZ  A~ORGTX A~WARPL
      INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM
          FROM ZVQM_EQ_SUM_2 AS A
             WHERE A~EQTYP =  S_EQTYP-LOW
               AND A~GSTRP IN S_GSTRP
               AND A~EQART EQ 'QM_3000'
               AND A~EQUNR BETWEEN LW_FROMNUMBER AND LW_TONUMBER
*               AND A~ARBPL IN S_ARBPL
               AND A~AUART IN S_AUART
               AND A~AUFNR IN S_AUFNR  AND A~WARPL IN S_WARPL
               AND A~SPRAS = SY-LANGU
               AND A~ENDDA = '99991231'.

  ENDIF.

  CHECK SY-SUBRC = 0.

**-   Inspection Lot AND UD
**  SELECT A~EQTYP A~EQUNR A~AUFNR A~AUTYP A~AUART
**         A~GSTRP A~GLTRP A~PRUEFLOS A~ERSTELDAT
**         A~RUECK A~RMZHL A~BUDAT
**         B~VCODE B~LTEXTKZ B~VDATUM
**   INTO CORRESPONDING FIELDS OF TABLE LT_ZVQM_EQ_SUM_UD
**      FROM ZVQM_EQ_SUM_UD AS A LEFT OUTER JOIN QAVE AS B
**       ON A~PRUEFLOS = B~PRUEFLOS
**        FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
**          WHERE  A~EQTYP =  S_EQTYP-LOW
**             AND A~GSTRP IN S_GSTRP
**             AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
**             AND A~AUART =  IT_ZSQM_EQ_SUMM-AUART
**             AND A~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
**             AND A~ERSTELDAT IN S_ERSTD.

*  SELECT A~EQTYP A~EQUNR B~AUFNR C~AUTYP C~AUART
*         D~GSTRP D~GLTRP F~PRUEFLOS F~ERSTELDAT
*         E~RUECK E~RMZHL E~BUDAT
*         K~VCODE K~LTEXTKZ K~VDATUM
*   INTO CORRESPONDING FIELDS OF TABLE LT_ZVQM_EQ_SUM_UD
*      FROM ( ( ( ( ( ( EQUI  AS A INNER JOIN AFIH AS B
*         ON A~EQUNR  = B~EQUNR ) INNER JOIN AUFK AS C
*         ON B~AUFNR  = C~AUFNR ) INNER JOIN AFKO AS D
*         ON C~AUFNR  = D~AUFNR ) INNER JOIN AFRU AS E
*         ON C~AUFNR  = E~AUFNR ) INNER JOIN QALS AS F
*         ON C~AUFNR  = F~AUFNR ) INNER JOIN AUSP AS G
*         ON A~EQUNR  = G~OBJEK
*                               )  LEFT OUTER JOIN QAVE AS K
*         ON F~PRUEFLOS = K~PRUEFLOS
*        FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
*          WHERE  A~EQTYP =  S_EQTYP-LOW
*             AND D~GSTRP IN S_GSTRP
*             AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
*             AND C~AUART =  IT_ZSQM_EQ_SUMM-AUART
*             AND B~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
*             AND C~LOEKZ =  ' '
*             AND A~LVORM EQ ' '
*             AND A~EQART EQ 'QM_1000'
*             AND E~RMZHL EQ '00000001'
*             AND F~ERSTELDAT IN S_ERSTD.

  SELECT A~EQTYP A~EQUNR B~AUFNR C~AUTYP C~AUART
*         D~GSTRP D~GLTRP
         F~PRUEFLOS F~ERSTELDAT
*         E~RUECK E~RMZHL E~BUDAT
         K~VCODE K~LTEXTKZ K~VDATUM
   INTO CORRESPONDING FIELDS OF TABLE LT_ZVQM_EQ_SUM_UD
      FROM ( ( ( (  EQUI  AS A INNER JOIN AFIH AS B
         ON A~EQUNR  = B~EQUNR ) INNER JOIN AUFK AS C
         ON B~AUFNR  = C~AUFNR ) INNER JOIN QALS AS F
         ON C~AUFNR  = F~AUFNR ) INNER JOIN AUSP AS G
         ON A~EQUNR  = G~OBJEK
                               )  LEFT OUTER JOIN QAVE AS K
         ON F~PRUEFLOS = K~PRUEFLOS
        FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
          WHERE  A~EQTYP =  S_EQTYP-LOW
             AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
             AND C~AUART =  IT_ZSQM_EQ_SUMM-AUART
             AND B~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
             AND C~LOEKZ =  ' '
             AND A~LVORM EQ ' '
             AND A~EQART EQ 'QM_3000'
             AND F~ERSTELDAT IN S_ERSTD.


  DATA : BEGIN OF LT_AFKO OCCURS 0,
          AUFNR  TYPE AUFNR,
          RUECK  TYPE CO_RUECK,
          GSTRP  TYPE CO_GSTRP,
          GLTRP  TYPE CO_GLTRP,
          RMZHL  TYPE CO_RMZHL,
          BUDAT  TYPE BUDAT,
         END OF LT_AFKO.

  SELECT A~AUFNR E~RUECK A~GSTRP A~GLTRP E~RMZHL E~BUDAT
    INTO CORRESPONDING FIELDS OF TABLE LT_AFKO
     FROM AFKO AS A  INNER JOIN AFRU AS E
        ON A~AUFNR  = E~AUFNR
       FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
      WHERE A~AUFNR = IT_ZSQM_EQ_SUMM-AUFNR
        AND A~GSTRP IN S_GSTRP
        AND E~RMZHL EQ '00000001'.

  LOOP AT LT_AFKO.

    READ TABLE LT_ZVQM_EQ_SUM_UD WITH KEY AUFNR = LT_AFKO-AUFNR.

    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      MOVE-CORRESPONDING LT_AFKO TO LT_ZVQM_EQ_SUM_UD.
      MODIFY LT_ZVQM_EQ_SUM_UD INDEX LW_INDEX.
    ENDIF.

  ENDLOOP.



*-- Merge UD data
  LOOP AT LT_ZVQM_EQ_SUM_UD.
    READ TABLE IT_ZSQM_EQ_SUMM
              WITH KEY EQTYP = LT_ZVQM_EQ_SUM_UD-EQTYP
                       EQUNR = LT_ZVQM_EQ_SUM_UD-EQUNR
                       AUFNR = LT_ZVQM_EQ_SUM_UD-AUFNR.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      MOVE:LT_ZVQM_EQ_SUM_UD-VCODE   TO IT_ZSQM_EQ_SUMM-VCODE,
        LT_ZVQM_EQ_SUM_UD-LTEXTKZ TO IT_ZSQM_EQ_SUMM-LTEXTKZ,
        LT_ZVQM_EQ_SUM_UD-PRUEFLOS TO IT_ZSQM_EQ_SUMM-PRUEFLOS,
        LT_ZVQM_EQ_SUM_UD-ERSTELDAT TO IT_ZSQM_EQ_SUMM-ERSTELDAT,
        LT_ZVQM_EQ_SUM_UD-RUECK TO IT_ZSQM_EQ_SUMM-RUECK,
        LT_ZVQM_EQ_SUM_UD-RMZHL TO IT_ZSQM_EQ_SUMM-RMZHL,
        LT_ZVQM_EQ_SUM_UD-BUDAT TO IT_ZSQM_EQ_SUMM-BUDAT,
        LT_ZVQM_EQ_SUM_UD-VDATUM TO IT_ZSQM_EQ_SUMM-VDATUM.
      MODIFY IT_ZSQM_EQ_SUMM INDEX LW_INDEX.
    ENDIF.
  ENDLOOP.

  CHECK NOT IT_ZSQM_EQ_SUMM[] IS INITIAL.

*- 2. FILL Confirm Status Field of order(AUFNR)- STAT_AUF

*  SELECT A~EQTYP A~EQUNR  A~AUFNR
*         B~STAT AS STAT_AUF
*   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM_T
*      FROM ZVQM_EQ_SUM_UD AS A INNER JOIN JEST AS B
*        ON A~OBJ_AUF = B~OBJNR
*         FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
*          WHERE  A~EQTYP =  S_EQTYP-LOW
*               AND A~GSTRP IN S_GSTRP
*               AND A~EQUNR =  IT_ZSQM_EQ_SUMM-EQUNR
*               AND A~AUART =  IT_ZSQM_EQ_SUMM-AUART
*               AND A~AUFNR =  IT_ZSQM_EQ_SUMM-AUFNR
*               AND B~STAT = C_ORDER_STATUS_CONFIRM. "/CONFIRM STATUS.

  SELECT A~EQTYP A~EQUNR  B~AUFNR
         F~STAT AS STAT_AUF
   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_SUMM_T
      FROM ( ( ( ( EQUI  AS A INNER JOIN AFIH AS B
         ON A~EQUNR  = B~EQUNR ) INNER JOIN AUFK AS C
         ON B~AUFNR  = C~AUFNR ) INNER JOIN AFKO AS D
         ON C~AUFNR  = D~AUFNR ) INNER JOIN QALS AS E
         ON C~AUFNR  = E~AUFNR ) INNER JOIN JEST AS F
         ON C~OBJNR  = F~OBJNR
         FOR ALL ENTRIES IN IT_ZSQM_EQ_SUMM
          WHERE  A~EQTYP =  S_EQTYP-LOW
               AND D~GSTRP IN S_GSTRP
               AND A~EQUNR = IT_ZSQM_EQ_SUMM-EQUNR
               AND C~AUART = IT_ZSQM_EQ_SUMM-AUART
               AND C~AUFNR = IT_ZSQM_EQ_SUMM-AUFNR
               AND F~STAT  = C_ORDER_STATUS_CONFIRM "/CONFIRM STATUS.
               AND C~LOEKZ EQ ' '
               AND A~LVORM EQ ' '
               AND A~EQART EQ 'QM_3000'.

*  CHECK SY-SUBRC = 0.

  LOOP AT IT_ZSQM_EQ_SUMM_T.
    CLEAR IT_ZSQM_EQ_SUMM.
    READ TABLE IT_ZSQM_EQ_SUMM
               WITH KEY EQTYP = IT_ZSQM_EQ_SUMM_T-EQTYP
                        EQUNR = IT_ZSQM_EQ_SUMM_T-EQUNR
                        AUFNR = IT_ZSQM_EQ_SUMM_T-AUFNR.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      CASE C_MARK.
        WHEN P_C_WWO OR P_C_WIT. "/ With or Without/ WITH
          MOVE : IT_ZSQM_EQ_SUMM_T-STAT_AUF
                             TO IT_ZSQM_EQ_SUMM-STAT_AUF.
          MODIFY IT_ZSQM_EQ_SUMM INDEX LW_INDEX.
        WHEN P_C_WO.  " Without.
          DELETE IT_ZSQM_EQ_SUMM INDEX LW_INDEX.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDLOOP.


  IF    P_C_WIT = C_MARK              AND
    NOT IT_ZSQM_EQ_SUMM[] IS INITIAL.
    DELETE IT_ZSQM_EQ_SUMM WHERE STAT_AUF = ' '.
  ENDIF.

*- Usage Decision
  CASE C_MARK.
    WHEN P_U_WWO.
    WHEN P_U_WIT.
      DELETE IT_ZSQM_EQ_SUMM WHERE VCODE = ''.
    WHEN P_U_WO.
      DELETE IT_ZSQM_EQ_SUMM WHERE VCODE NE ''.
  ENDCASE.

*- Long Text of Usage Decision
  CASE C_MARK.
    WHEN P_L_WWO.
    WHEN P_L_WIT.
      DELETE IT_ZSQM_EQ_SUMM WHERE LTEXTKZ = ' '.
    WHEN P_L_WO.
      DELETE IT_ZSQM_EQ_SUMM WHERE LTEXTKZ  = 'X'.
  ENDCASE.

ENDFORM.                    " RET_EQUIP_SUM_Y
