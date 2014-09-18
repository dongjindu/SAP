************************************************************************
* Program Name      : ZRQM22R_CALI_STAT_DEP
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.31.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Calibration Status Report by Department
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZRQM22R_CALI_STAT_DEP    NO STANDARD PAGE HEADING .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
TABLES : FELD.      "//Screen Object Structure
*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.
INCLUDE ZQM_INCLUDE_POOL03. "/Equipment Include Module

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : EQUI,   "/Equipment master data
         EQUZ,   "/Equipment time segment
         ILOA,   "/PM Object Location and Account Assignment
         KLAH.   "/Class Header Data
TABLES : T001W,
         IHPA.   "/Plant Maintenance: Partners.
TABLES : AUFK,   "/Order master data
         AFKO.   "/Order header data PP orders


*// Structures
TABLES : ZSQM_CALI_DEP_L. "/Calibration Status List
TABLES : ZSQM_CALI_DEP_B. "/Calibration Status : Basic List

*//Type (Table Structure);(TY_ )- Table or Structure

*-- PF-Status : Excluding Function Code table
TYPES: BEGIN OF TY_FCODE,
        FCODE LIKE RSMPE-FUNC,
      END OF TY_FCODE.
DATA: IT_EX_FUNC TYPE STANDARD TABLE OF TY_FCODE WITH
                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
      WA_EX_FUNC TYPE TY_FCODE.

DATA : IT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS WITH HEADER LINE.

*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.
*CONSTANTS : C_PARVW_RESP_DEPT   TYPE PARVW VALUE 'Z3'.

**-- Screen Control Mode
CONSTANTS : C_BASIC(10) TYPE C VALUE 'BASIC',
            C_DETAIL(10) TYPE C VALUE 'DETAIL'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.

*-- Data Level control
DATA : WA_LEVEL(8) TYPE C.
DATA : WA_RENEWAL_FLG.


*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*-- Work area Variables in Program.(WA_xxxx)


*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Screnn field cursor control
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT_


**/-- Internale Tables with structure as sama as DB
*- Calibration Status List
DATA : IT_ZSQM_CALI_DEP_L LIKE ZSQM_CALI_DEP_L OCCURS 0
                                            WITH HEADER LINE.
*- Calibration Status List for only Confirmed order list
DATA : IT_ZSQM_CALI_CNF   LIKE ZSQM_CALI_DEP_L OCCURS 0
                                            WITH HEADER LINE.
*- Calibration Status : Basic List
DATA : IT_ZSQM_CALI_DEP_B LIKE ZSQM_CALI_DEP_B OCCURS 0
                                            WITH HEADER LINE.

*//Ranges; (R_)
*RANGES :

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

*-- ALV LIST STRUCTURE
DATA : WA_STRUCTURE_NAME LIKE DD02L-TABNAME.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS : LCL_EVENT_RECEIVER   DEFINITION   DEFERRED.

DATA : EVENT_RECEIVER   TYPE REF TO    LCL_EVENT_RECEIVER.

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
*-- macro : macro_name &1 &2
*--           &1 -
*--           &2 -
*  DEFINE macro_name.
*  END-OF-DEFINITION.


***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*- Plant
SELECT-OPTIONS : S_WERKS  FOR T001W-WERKS  NO INTERVALS NO-EXTENSION .
*- Department(Partner Function :Resp Department).
SELECT-OPTIONS : S_PARNR  FOR IHPA-PARNR NO INTERVALS NO-EXTENSION.
*- Start Date
SELECT-OPTIONS : S_SDATE FOR AFKO-GSTRP.

SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

*--// Search Help for Department field using HR organization
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PARNR-LOW.
*-- Help for Department - Low : Partner function Resp. Department.
  PERFORM GET_SCREEN_FIELD_INFO.
  PERFORM GET_PARTNER_VALUE   USING C_EQUIP_PARVW_DEPT  "/parvw
                                    WA_FLDTXT.

AT SELECTION-SCREEN ON BLOCK BLK.
  CHECK SY-UCOMM = 'ONLI'.

*-- get Calibration Status data from DB.
  PERFORM GET_DATA_FROM_DB.

  IF IT_ZSQM_CALI_DEP_L[] IS INITIAL.
    MESSAGE E000(ZMQM) WITH 'No entries!'(E01).
    EXIT.
  ENDIF.


START-OF-SELECTION.
  CHECK NOT IT_ZSQM_CALI_DEP_L[] IS INITIAL.

*-  merge order list and confirmed order list
  PERFORM MERGE_ORDER_LIST_CNF.

*-  Basic List
  PERFORM RET_BASIC_LIST.

*-- Fill Department Text
  CHECK NOT IT_ZSQM_CALI_DEP_B[] IS INITIAL.
  PERFORM FILL_DEPARTMENT_TEXT.

**-- End of Selection.
END-OF-SELECTION.
  CHECK NOT IT_ZSQM_CALI_DEP_B[] IS INITIAL.
  WA_LEVEL = C_BASIC.
  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.

**-- Set class type field for Search help : '002'.
*  SET PARAMETER ID 'KAR' FIELD '002'.

INITIALIZATION.
*- Set default date for inspection start date
  CONCATENATE SY-DATUM+0(4)
              '0101'    INTO S_SDATE-LOW.
  MOVE : SY-DATUM TO S_SDATE-HIGH.
  APPEND S_SDATE.

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

* The event DOUBLE_CLICK provides parameters for row and column
*   of the click. Use row parameter to select a line of the
*   corresponding internal table.
* : E_ROW-INDEX.

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
*&---------------------------------------------------------------------*
*&      Form  GET_SCREEN_FIELD_INFO
*&---------------------------------------------------------------------*
FORM GET_SCREEN_FIELD_INFO.
  CLEAR: WA_FLDTXT, WA_CUR_LINE.
  GET CURSOR FIELD WA_FLDTXT LINE WA_CUR_LINE.
ENDFORM.                    " GET_SCREEN_FIELD_INFO
*&-----------------------------------------------------------------*
*&      Form  GET_PARTNER_VALUE
*&-----------------------------------------------------------------*
FORM GET_PARTNER_VALUE USING    VALUE(P_PARVW)
                                VALUE(P_PARNR_FIELD).
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

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = LW_DYNAME
            DYNUMB     = LW_DYNUMB
       TABLES
            DYNPFIELDS = LW_DYNPFIELDS.



ENDFORM.                    " GET_PARTNER_VALUE
*&------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&------------------------------------------------------------------*
FORM GET_DATA_FROM_DB.

  REFRESH : IT_ZSQM_CALI_DEP_L, IT_ZSQM_CALI_CNF.

*-- Get order List and equipment Status
  SELECT E~AUFNR A~EQUNR C~PARNR AS PARNR_DEP
         G~SWERK B~STAT G~ABCKZ
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_CALI_DEP_L
       FROM ( ( ( ( ( EQUI AS A INNER JOIN JEST AS B
          ON A~OBJNR = B~OBJNR ) INNER JOIN IHPA AS C
          ON A~OBJNR = C~OBJNR ) INNER JOIN AFIH AS D
          ON A~EQUNR = D~EQUNR ) INNER JOIN AUFK AS E
          ON D~AUFNR = E~AUFNR ) INNER JOIN AFKO AS F
          ON D~AUFNR = F~AUFNR ) INNER JOIN ILOA AS G
          ON D~ILOAN = G~ILOAN
         WHERE  A~EQTYP = C_EQTYP_CALIBRATION
           AND  B~STAT  = C_EQUIP_STATUS_ERIN
           AND  B~INACT = ' '
           AND  C~PARVW = C_EQUIP_PARVW_DEPT
           AND  C~PARNR IN S_PARNR
           AND  E~AUART = 'QM10'        "/Regular calibration Order
           AND  G~SWERK IN S_WERKS
           AND  F~GSTRP IN S_SDATE.

  CHECK SY-SUBRC = 0.

*- Get only confirmed order list
  SELECT E~AUFNR A~EQUNR C~PARNR AS PARNR_DEP
         G~SWERK B~STAT G~ABCKZ H~STAT AS STAT_AUF
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_CALI_CNF
       FROM ( ( ( ( ( ( EQUI AS A INNER JOIN JEST AS B
          ON A~OBJNR = B~OBJNR ) INNER JOIN IHPA AS C
          ON A~OBJNR = C~OBJNR ) INNER JOIN AFIH AS D
          ON A~EQUNR = D~EQUNR ) INNER JOIN AUFK AS E
          ON D~AUFNR = E~AUFNR ) INNER JOIN AFKO AS F
          ON D~AUFNR = F~AUFNR ) INNER JOIN ILOA AS G
          ON D~ILOAN = G~ILOAN ) INNER JOIN JEST AS H
          ON E~OBJNR = H~OBJNR
         WHERE  A~EQTYP = C_EQTYP_CALIBRATION
           AND  B~STAT  = C_EQUIP_STATUS_ERIN
           AND  B~INACT = ' '
           AND  C~PARVW = C_EQUIP_PARVW_DEPT
           AND  C~PARNR IN S_PARNR
           AND  E~AUART = 'QM10'        "/Regular calibration Order
           AND  G~SWERK IN S_WERKS
           AND  F~GSTRP IN S_SDATE
           AND  H~INACT = ' '
           AND  H~STAT  = C_ORDER_STATUS_CONFIRM.

ENDFORM.                    " GET_DATA_FROM_DB
*&------------------------------------------------------------------*
*&      Form  MERGE_ORDER_LIST_CNF
*&------------------------------------------------------------------*
FORM MERGE_ORDER_LIST_CNF.
  DATA : LW_ORDER_INDEX LIKE SY-TABIX.

*- Fill confirmed order status field of IT_ZSQM_CALI_DEP_L from
*- IT_ZSQM_CALI_CNF-STAT_AUF

  LOOP AT IT_ZSQM_CALI_CNF.

    READ TABLE IT_ZSQM_CALI_DEP_L WITH KEY
                                   AUFNR = IT_ZSQM_CALI_CNF-AUFNR.
    IF SY-SUBRC = 0.
      LW_ORDER_INDEX = SY-TABIX.
      MOVE: IT_ZSQM_CALI_CNF-STAT_AUF TO IT_ZSQM_CALI_DEP_L-STAT_AUF.
      MODIFY IT_ZSQM_CALI_DEP_L INDEX LW_ORDER_INDEX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MERGE_ORDER_LIST_CNF
*&------------------------------------------------------------------*
*&      Form  RET_BASIC_LIST
*&------------------------------------------------------------------*
FORM RET_BASIC_LIST.
  REFRESH IT_ZSQM_CALI_DEP_B.

  LOOP AT IT_ZSQM_CALI_DEP_L.

    CLEAR IT_ZSQM_CALI_DEP_B.

    MOVE : IT_ZSQM_CALI_DEP_L-PARNR_DEP TO IT_ZSQM_CALI_DEP_B-PARNR_DEP.

    CASE IT_ZSQM_CALI_DEP_L-ABCKZ.
      WHEN C_EQUIP_ABC_INTERNAL.
        MOVE : 1 TO IT_ZSQM_CALI_DEP_B-IN_SUM,
               1 TO IT_ZSQM_CALI_DEP_B-IN_NCNF.
        IF NOT IT_ZSQM_CALI_DEP_L-STAT_AUF IS INITIAL.
          MOVE : 1 TO IT_ZSQM_CALI_DEP_B-IN_CNF.
          CLEAR IT_ZSQM_CALI_DEP_B-IN_NCNF.
        ENDIF.
      WHEN C_EQUIP_ABC_EXTERNAL.
        MOVE : 1 TO IT_ZSQM_CALI_DEP_B-EX_SUM,
               1 TO IT_ZSQM_CALI_DEP_B-EX_NCNF.
        IF NOT IT_ZSQM_CALI_DEP_L-STAT_AUF IS INITIAL.
          MOVE : 1 TO IT_ZSQM_CALI_DEP_B-EX_CNF.
          CLEAR IT_ZSQM_CALI_DEP_B-EX_NCNF.
        ENDIF.
      WHEN C_EQUIP_ABC_PATROL.
        MOVE : 1 TO IT_ZSQM_CALI_DEP_B-PA_SUM,
               1 TO IT_ZSQM_CALI_DEP_B-PA_NCNF.
        IF NOT IT_ZSQM_CALI_DEP_L-STAT_AUF IS INITIAL.
          MOVE : 1 TO IT_ZSQM_CALI_DEP_B-PA_CNF.
          CLEAR IT_ZSQM_CALI_DEP_B-PA_NCNF.
        ENDIF.
    ENDCASE.

    COLLECT IT_ZSQM_CALI_DEP_B.

  ENDLOOP.

ENDFORM.                    " RET_BASIC_LIST
*&-----------------------------------------------------------------*
*&      Form  FILL_DEPARTMENT_TEXT
*&-----------------------------------------------------------------*
FORM FILL_DEPARTMENT_TEXT.
  DATA : LW_INDEX LIKE SY-TABIX.

  LOOP AT IT_ZSQM_CALI_DEP_B.
    LW_INDEX = SY-TABIX.

    SELECT SINGLE A~ORGTX
                   INTO IT_ZSQM_CALI_DEP_B-ORGTX
         FROM T527X AS A INNER JOIN IHPA AS B
              ON A~ORGEH = B~PARNR
           WHERE B~PARNR = IT_ZSQM_CALI_DEP_B-PARNR_DEP
             AND B~PARVW = C_EQUIP_PARVW_DEPT.

    MODIFY IT_ZSQM_CALI_DEP_B INDEX LW_INDEX.
  ENDLOOP.

ENDFORM.                    " FILL_DEPARTMENT_TEXT
*&------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.

ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
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
              TXT1  = TEXT-E05.
  ENDIF.

ENDFORM.                    " FREE_ALV_GRID
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      CASE WA_LEVEL.
        WHEN C_BASIC.
          PERFORM SET_LIST_LEVEL_CONTROL.
          LEAVE TO SCREEN 0.
        WHEN OTHERS.
          PERFORM SET_LIST_LEVEL_CONTROL.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  SET_LIST_LEVEL_CONTROL
*&------------------------------------------------------------------*
FORM SET_LIST_LEVEL_CONTROL.
  CASE WA_LEVEL.
    WHEN C_BASIC.
    WHEN C_DETAIL.
      WA_LEVEL = C_BASIC.
      WA_RENEWAL_FLG = C_MARK.
  ENDCASE.
ENDFORM.                    " SET_LIST_LEVEL_CONTROL
*&-----------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&-----------------------------------------------------------------*
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
                TXT1  = 'The control can not be created'(E06).
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
*        Set field attribute/Text by by Inspection type(ISIR, Regular)
    PERFORM SET_FIELD_CAT_ATTR  TABLES IT_FIELDCAT.

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
*    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON   FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.

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
*        Set field attribute/Text by by Inspection type(ISIR, Regular)
    PERFORM SET_FIELD_CAT_ATTR  TABLES IT_FIELDCAT.

*-- Display data on ALV GRID Control using method

    PERFORM SET_NEW_TABLE_DATA.

*    PERFORM REFRESH_ALV_GRID_DATA_DISP.
*
*    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.

  ENDIF.


ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = C_MARK.  "/Optimize column width
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.    "/Field name for cell color
*  WA_IS_LAYOUT-DETAILTITL                "/Title bar of detail screen
*                        = 'Detail information'(T10).
*  WA_IS_LAYOUT-GRID_TITLE                "/ Title bar text
*                    = 'Inspection '(T11).
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
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  CLEAR : WA_STRUCTURE_NAME.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

  CASE WA_LEVEL.
    WHEN C_BASIC.
      WA_STRUCTURE_NAME = 'ZSQM_CALI_DEP_B'.

    WHEN C_DETAIL.
      WA_STRUCTURE_NAME = 'ZSQM_CALI_DEP_L'.
  ENDCASE.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = WA_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].


ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&------------------------------------------------------------------*
*&      Form  SET_FIELD_CAT_ATTR
*&------------------------------------------------------------------*
FORM SET_FIELD_CAT_ATTR TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.
  DATA : LW_D_ABCKZ(2) TYPE C,
         LW_D_FLD(4) TYPE C.
  DATA : LW_COL_TEXT(20) TYPE C.

  CASE WA_LEVEL.
    WHEN C_BASIC.
* Set field attribute
      LOOP AT PT_FIELDCAT.
        CASE PT_FIELDCAT-FIELDNAME.
          WHEN 'PARNR_DEP' OR 'ORGTX'.
            PT_FIELDCAT-KEY_SEL = C_MARK.
            PT_FIELDCAT-KEY     = C_MARK.
            MOVE  TEXT-TT1 TO : PT_FIELDCAT-COLTEXT,
                                PT_FIELDCAT-SCRTEXT_L,
                                PT_FIELDCAT-SCRTEXT_M,
                                PT_FIELDCAT-SCRTEXT_S.
            IF PT_FIELDCAT-FIELDNAME = 'PARNR_DEP'.
              PT_FIELDCAT-NO_OUT = C_MARK.
            ENDIF.

          WHEN OTHERS.
            PT_FIELDCAT-DO_SUM    = C_MARK.

            SPLIT PT_FIELDCAT-FIELDNAME AT '_'
                                           INTO LW_D_ABCKZ  LW_D_FLD.
            CLEAR LW_COL_TEXT.

            MOVE  LW_D_ABCKZ TO LW_COL_TEXT."column Hdr Text using ABCKZ

            CASE LW_D_FLD.
              WHEN 'SUM'.
                CONCATENATE LW_COL_TEXT
                            'SUM'(TT2)
                            INTO LW_COL_TEXT SEPARATED BY SPACE.
              WHEN 'CNF'.
                CONCATENATE LW_COL_TEXT
                            'Confirm'(TT3)
                            INTO LW_COL_TEXT SEPARATED BY SPACE.
              WHEN 'NCNF'.
                CONCATENATE LW_COL_TEXT
                            'not Confirm'(TT4)
                            INTO LW_COL_TEXT SEPARATED BY SPACE.
            ENDCASE.

            MOVE   LW_COL_TEXT
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.

            CASE LW_D_ABCKZ.
              WHEN 'IN'.
              WHEN 'EX'.
                PT_FIELDCAT-EMPHASIZE = 'C710'.
              WHEN 'PA'.
                PT_FIELDCAT-EMPHASIZE = 'C510'.
            ENDCASE.

        ENDCASE.

        MODIFY PT_FIELDCAT.

      ENDLOOP.

    WHEN C_DETAIL.

  ENDCASE.

ENDFORM.                    " SET_FIELD_CAT_ATTR
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
FORM SET_NEW_TABLE_DATA.
  CASE WA_LEVEL.
    WHEN C_BASIC.

      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
                  IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_CALI_DEP_B[].


    WHEN C_DETAIL.
*      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*        EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
*                  IS_LAYOUT        = WA_IS_LAYOUT
*                  I_SAVE           = WA_SAVE
*                  IS_VARIANT       = WA_VARIANT
*                  I_DEFAULT        = SPACE
*                  IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
*        CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*                  IT_OUTTAB        = IT_ZSQM_IS_MIC_DLIST[].
  ENDCASE.

ENDFORM.                    " SET_NEW_TABLE_DATA
