************************************************************************
* Program Name      : ZRQM05R_EQUIP_LIST_V2
* Author            : SeungLyong, Lee
* Creation Date     : 2003.09.30.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Equipment registration List
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZRQM05R_EQUIP_LIST_V2     .

*&&& Data Declaration.  &&&*
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
*TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.

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
TABLES : T001W.
TABLES : ZVQM_EQ_CL_STAT. "/Equipment Class Status List

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_EQ_LIST.  "/Equipment List for Select criterion
TABLES : ZSQM_EQ_CLASS, "/Equipment List-Class Level Count
         ZSQM_EQ_CHAR,  "/Equipment List-Characteristic Level Count
         ZSQM_EQ_VALUE. "/Equipment List-Value Level Count
TABLES : ZSQM_EQ_VALUE_T. "/Equipment List - Temp : Value Count

TABLES : ZSQM_EQ_L_D. "/Equipment List for Equipment Level

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
CONSTANTS : C_CLASS(15)          TYPE C VALUE 'Class',
            C_CHARACTERISTIC(15) TYPE C VALUE 'Characteristic',
            C_VALUE(15)          TYPE C VALUE 'Value',
            C_EQUIP(15)          TYPE C VALUE 'Equipment'.


**-- Process Status
*CONSTANTS : " C_UPLOADED(8)  TYPE C VALUE 'UPLOADED',
*            C_SAVED(8)     TYPE C VALUE 'SAVED'.


**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

DATA : WA_LEVEL(15) TYPE C.
DATA :  WA_RENEWAL_FLG.

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*-- Work area Variables in Program.
DATA : WA_CLASS TYPE KLASSE_D,  "/Selected Class for Detail
       WA_IMERK TYPE ATINN,     "/Selected Characteristic for Detail
       WA_ATWRT TYPE ATWRT.     "/Selected Value for Detail

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Selection Criterion Text work area for Screen 9000.
DATA : BEGIN OF WA_SCR_TXT,
         TYPTX LIKE T370U-TYPTX, "/Equipment category Text
         NAME1 LIKE T001W-NAME1, "/Plant Text
         KTEXT LIKE CRTX-KTEXT,  "/Work center Text(view: 'M_CRAMN')
         CLINT LIKE KSML-CLINT,  "/Class Number
         ATNAM LIKE CABN-ATNAM,  "/Characteristic name
         ATWRT LIKE AUSP-ATWRT,  "/Value
         ATWTB LIKE CAWNT-ATWTB, "/Characteristic value description
       END OF WA_SCR_TXT.


*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT

*/-- Internale Tables with structure as sama as DB
DATA : IT_ZVQM_EQ_CL_STAT LIKE ZVQM_EQ_CL_STAT  OCCURS 0
                                                 WITH HEADER LINE.
DATA : IT_ZSQM_EQ_LIST LIKE ZSQM_EQ_LIST OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSQM_EQ_CLASS TYPE TABLE OF ZSQM_EQ_CLASS,
       IT_ZSQM_EQ_CHAR  TYPE TABLE OF ZSQM_EQ_CHAR,
       IT_ZSQM_EQ_VALUE TYPE TABLE OF ZSQM_EQ_VALUE.

*-   "/Equipment List for Equipment Level
DATA : IT_ZSQM_EQ_L_D TYPE TABLE OF ZSQM_EQ_L_D.

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
DATA: CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
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


***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
*- Equipment Category
SELECT-OPTIONS : S_EQTYP  FOR EQUI-EQTYP  NO-EXTENSION NO INTERVALS
                    DEFAULT 'Z' OBLIGATORY   MATCHCODE OBJECT  H_T370T
                     MODIF ID EQT.
*- Plant
SELECT-OPTIONS : S_WERKS  FOR T001W-WERKS NO-EXTENSION NO INTERVALS.
*- Work Center
SELECT-OPTIONS : S_ARBPL  FOR CRHD-ARBPL NO-EXTENSION NO INTERVALS
                     MATCHCODE OBJECT CRAM.
*- Equipment
SELECT-OPTIONS : S_EQUNR FOR EQUI-EQUNR.

SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
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

AT SELECTION-SCREEN.
  CHECK SY-UCOMM = 'ONLI'.

  PERFORM GET_TEXT_FOR_SELECT_CRITERION.

  PERFORM RETRIEVE_SELECT_CRITERION.

  IF IT_ZSQM_EQ_LIST[] IS INITIAL.
    MESSAGE E000(ZMQM) WITH 'No entries!'(E01).
  ENDIF.

*-- Selection for Selection Screen
START-OF-SELECTION.
*--  Retrieve Equipment list count : Class Level
  PERFORM RET_COUNT_EQUIP_CLASS_LEVEL.

**-- End of Selection.
END-OF-SELECTION.
  WA_LEVEL = C_CLASS.
  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.
*  MOVE : SY-REPID TO WA_VARIANT-REPORT. "/Set variant-report field

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
    DATA: LW_ZSQM_EQ_L_D LIKE LINE OF IT_ZSQM_EQ_L_D.

* .The event DOUBLE_CLICK provides parameters for row and column
*   of the click. Use row parameter to select a line of the
*   corresponding internal table.

*- read selected row from internal table
    CASE WA_LEVEL.
      WHEN C_CLASS.
      WHEN C_CHARACTERISTIC.
      WHEN C_VALUE.
      WHEN C_EQUIP.
        READ TABLE IT_ZSQM_EQ_L_D INDEX E_ROW-INDEX INTO LW_ZSQM_EQ_L_D.

        PERFORM CALL_EQUIP_DISPLAY  USING LW_ZSQM_EQ_L_D-EQUNR.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method
*-----------------------------------------------------------------
*   In event handler method for event TOOLBAR: Append own functions
*   by using event parameter E_OBJECT.
*....................................................................
* E_OBJECT of event TOOLBAR is of type REF TO CL_ALV_EVENT_TOOLBAR_SET.
* This class has got one attribute, namly MT_TOOLBAR, which
* is a table of type TTB_BUTTON. One line of this table is
* defined by the Structure STB_BUTTON (see data deklaration above).

* A remark to the flag E_INTERACTIVE:
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*         'e_interactive' is set, if this event is raised due to
*         the call of 'set_toolbar_interactive' by the user.
*         You can distinguish this way if the event was raised
*         by yourself or by ALV
*         (e.g. in method 'refresh_table_display').
*         An application of this feature is still unknown...
*-----------------------------------------------------------------

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
    CASE WA_LEVEL.
      WHEN C_CLASS OR C_CHARACTERISTIC OR C_VALUE.
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

*      WHEN C_VALUE.
      WHEN OTHERS.
    ENDCASE.
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
      WHEN 'DETAIL'.
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

*DATA : BEGIN OF WA_SCR_TXT,
*         TYPTX LIKE T370U-TYPTX, "/Equipment category Text
*         NAME1 LIKE T001W-NAME1, "/Plant Text
*         KTEXT LIKE CRTX-KTEXT,  "/Work center Text(view: 'M_CRAMN')
*       END OF WA_SCR_TXT.
  CLEAR WA_SCR_TXT.
*-- Equipment category text
  IF NOT S_EQTYP-LOW IS INITIAL.
    SELECT SINGLE TYPTX INTO WA_SCR_TXT-TYPTX
       FROM T370U
         WHERE EQTYP = S_EQTYP-LOW
           AND SPRAS = SY-LANGU.
  ENDIF.

*-- Plant Text
  IF NOT S_WERKS-LOW IS INITIAL.
    SELECT SINGLE NAME1 INTO WA_SCR_TXT-NAME1
      FROM T001W
        WHERE WERKS = S_WERKS-LOW.
    IF SY-SUBRC NE 0.
      MESSAGE E058(00).
    ENDIF.
  ENDIF.

*-- Work center Text(view: 'M_CRAMN')
  IF NOT S_ARBPL-LOW IS INITIAL.
    SELECT SINGLE KTEXT INTO  WA_SCR_TXT-KTEXT
      FROM M_CRAMN
        WHERE ARBPL = S_ARBPL-LOW
          AND SPRAS = SY-LANGU.
    IF SY-SUBRC NE 0.
      MESSAGE E058(00).
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_TEXT_FOR_SELECT_CRITERION
*&------------------------------------------------------------------*
*&      Form  RETRIEVE_SELECT_CRITERION
*&------------------------------------------------------------------*
FORM RETRIEVE_SELECT_CRITERION.
  DATA : LW_FROMNUMBER LIKE NRIV-FROMNUMBER,
         LW_TONUMBER   LIKE NRIV-TONUMBER.

*-- Select All equipment List by Select criterion From ZVQM_EQ_CL_STAT
  IF NOT S_EQUNR[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_LIST
      FROM ZVQM_EQ_CL_STAT AS A INNER JOIN EQKT AS B
         ON A~EQUNR = B~EQUNR
        WHERE EQTYP IN S_EQTYP
          AND A~WERKS IN S_WERKS
          AND A~ARBPL IN S_ARBPL
          AND A~EQUNR IN S_EQUNR
          AND A~SPRAS = SY-LANGU
          AND B~SPRAS = SY-LANGU.
  ELSE.

    SELECT SINGLE B~FROMNUMBER B~TONUMBER
        INTO (LW_FROMNUMBER, LW_TONUMBER)
          FROM T370T AS A INNER JOIN NRIV AS B
            ON  A~NUMKI = B~NRRANGENR
            WHERE  A~EQTYP  = S_EQTYP-LOW
               AND B~OBJECT = 'EQUIP_NR'.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_LIST
     FROM ZVQM_EQ_CL_STAT AS A INNER JOIN EQKT AS B
         ON A~EQUNR = B~EQUNR
       WHERE A~EQTYP IN S_EQTYP
         AND A~WERKS IN S_WERKS
         AND A~ARBPL IN S_ARBPL
         AND A~EQUNR BETWEEN LW_FROMNUMBER AND LW_TONUMBER
         AND A~SPRAS = SY-LANGU
         AND B~SPRAS = SY-LANGU.
  ENDIF.

ENDFORM.                    " RETRIEVE_SELECT_CRITERION
*&---------------------------------------------------------------------*
*&      Form  RET_COUNT_EQUIP_CLASS_LEVEL
*&---------------------------------------------------------------------*
FORM RET_COUNT_EQUIP_CLASS_LEVEL.
  DATA : LW_ZSQM_EQ_CLASS LIKE ZSQM_EQ_CLASS.

  REFRESH  IT_ZSQM_EQ_CLASS.

  LOOP AT IT_ZSQM_EQ_LIST.
    CLEAR LW_ZSQM_EQ_CLASS.
    MOVE-CORRESPONDING  IT_ZSQM_EQ_LIST TO LW_ZSQM_EQ_CLASS.

    CASE IT_ZSQM_EQ_LIST-TXT04. "/Equipment user status.
      WHEN 'ERIN'. "/status for inspection(internal, external,patrol)
        CASE IT_ZSQM_EQ_LIST-ABCKZ.
          WHEN '1'.              "-Internal
            LW_ZSQM_EQ_CLASS-INTER = 1.
            LW_ZSQM_EQ_CLASS-TOTAL = 1.
          WHEN '2'.              "-Patrol
            LW_ZSQM_EQ_CLASS-PATROL = 1.
            LW_ZSQM_EQ_CLASS-TOTAL = 1.
          WHEN '3'.              "-External
            LW_ZSQM_EQ_CLASS-EXTER = 1.
            LW_ZSQM_EQ_CLASS-TOTAL = 1.
        ENDCASE.
*// Start of remark - sllee : 10/14/2003
*--  requested by Mr. Han :
*      WHEN 'ESON'.              "-Internal
*        LW_ZSQM_EQ_CLASS-INTER = 1.
*        LW_ZSQM_EQ_CLASS-TOTAL = 1.
*      WHEN 'ESPA'.              "-Patrol
*        LW_ZSQM_EQ_CLASS-PATROL = 1.
*        LW_ZSQM_EQ_CLASS-TOTAL = 1.
*      WHEN 'ESOF'.              "-External
*        LW_ZSQM_EQ_CLASS-EXTER = 1.
*        LW_ZSQM_EQ_CLASS-TOTAL = 1.
*// End of remark - sllee : 10/14/2003
      WHEN 'ESIO'.              "-Self
        LW_ZSQM_EQ_CLASS-SELF = 1.
        LW_ZSQM_EQ_CLASS-TOTAL = 1.
      WHEN 'EOSI'.              "-Outscope
        LW_ZSQM_EQ_CLASS-OUTSCOPE = 1.
        LW_ZSQM_EQ_CLASS-TOTAL = 1.
      WHEN 'IDLE'.              "-Idle
        LW_ZSQM_EQ_CLASS-IDLE = 1.
        LW_ZSQM_EQ_CLASS-TOTAL = 1.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    COLLECT LW_ZSQM_EQ_CLASS INTO IT_ZSQM_EQ_CLASS.
  ENDLOOP.

ENDFORM.                    " RET_COUNT_EQUIP_CLASS_LEVEL
*&---------------------------------------------------------------------*
*&      Form  RET_COUNT_EQUIP_CHAR_LEVEL
*&---------------------------------------------------------------------*
FORM RET_COUNT_EQUIP_CHAR_LEVEL.
  DATA : LW_ZSQM_EQ_CHAR LIKE ZSQM_EQ_CHAR.

  REFRESH IT_ZSQM_EQ_CHAR.

  LOOP AT IT_ZSQM_EQ_LIST WHERE CLASS = WA_CLASS .
*                            AND EQTYP IN S_EQTYP
*                            AND WERKS IN S_WERKS
*                            AND ARBPL IN S_ARBPL
*                            AND EQUNR IN S_EQUNR.

    CLEAR LW_ZSQM_EQ_CHAR.
    MOVE-CORRESPONDING  IT_ZSQM_EQ_LIST TO LW_ZSQM_EQ_CHAR.

    CASE IT_ZSQM_EQ_LIST-TXT04. "/Equipment user status.
      WHEN 'ERIN'. "/status for inspection(internal, external,patrol)
        CASE IT_ZSQM_EQ_LIST-ABCKZ.
          WHEN '1'.              "-Internal
            LW_ZSQM_EQ_CHAR-INTER = 1.
            LW_ZSQM_EQ_CHAR-TOTAL = 1.
          WHEN '2'.              "-Patrol
            LW_ZSQM_EQ_CHAR-PATROL = 1.
            LW_ZSQM_EQ_CHAR-TOTAL = 1.
          WHEN '3'.              "-External
            LW_ZSQM_EQ_CHAR-EXTER = 1.
            LW_ZSQM_EQ_CHAR-TOTAL = 1.
        ENDCASE.
*// Start of remark - sllee : 10/14/2003
*--  requested by Mr. Han :
*      WHEN 'ESON'.              "-Internal
*        LW_ZSQM_EQ_CHAR-INTER = 1.
*        LW_ZSQM_EQ_CHAR-TOTAL = 1.
*      WHEN 'ESPA'.              "-Patrol
*        LW_ZSQM_EQ_CHAR-PATROL = 1.
*        LW_ZSQM_EQ_CHAR-TOTAL = 1.
*      WHEN 'ESOF'.              "-External
*        LW_ZSQM_EQ_CHAR-EXTER = 1.
*        LW_ZSQM_EQ_CHAR-TOTAL = 1.
*// End of remark - sllee : 10/14/2003
      WHEN 'ESIO'.              "-Self
        LW_ZSQM_EQ_CHAR-SELF = 1.
        LW_ZSQM_EQ_CHAR-TOTAL = 1.
      WHEN 'EOSI'.              "-Outscope
        LW_ZSQM_EQ_CHAR-OUTSCOPE = 1.
        LW_ZSQM_EQ_CHAR-TOTAL = 1.
      WHEN 'IDLE'.              "-Idle
        LW_ZSQM_EQ_CHAR-IDLE = 1.
        LW_ZSQM_EQ_CHAR-TOTAL = 1.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    COLLECT LW_ZSQM_EQ_CHAR INTO IT_ZSQM_EQ_CHAR.
  ENDLOOP.

ENDFORM.                    " RET_COUNT_EQUIP_CHAR_LEVEL
*&---------------------------------------------------------------------*
*&      Form  RET_COUNT_EQUIP_VALUE_LEVEL
*&---------------------------------------------------------------------*
FORM RET_COUNT_EQUIP_VALUE_LEVEL.
  DATA : LW_ZSQM_EQ_VALUE   LIKE ZSQM_EQ_VALUE.
  DATA : LT_ZSQM_EQ_VALUE_T LIKE ZSQM_EQ_VALUE_T OCCURS 0
                                           WITH HEADER LINE.

  DATA : BEGIN OF LT_CAWNT OCCURS 0,
           ATINN  TYPE ATINN,
           ATZHL  TYPE ATZHL,
           ADZHL  TYPE ADZHL,
           ATWRT  TYPE ATWRT,
           ATWTB  TYPE ATWTB,
         END OF LT_CAWNT.

  LOOP AT IT_ZSQM_EQ_LIST WHERE CLASS = WA_CLASS
                           AND IMERK = WA_IMERK.
    CLEAR LT_ZSQM_EQ_VALUE_T.
    MOVE-CORRESPONDING  IT_ZSQM_EQ_LIST TO LT_ZSQM_EQ_VALUE_T.

    CASE IT_ZSQM_EQ_LIST-TXT04. "/Equipment user status.
      WHEN 'ERIN'. "/status for inspection(internal, external,patrol)
        CASE IT_ZSQM_EQ_LIST-ABCKZ.
          WHEN '1'.              "-Internal
            LT_ZSQM_EQ_VALUE_T-INTER = 1.
            LT_ZSQM_EQ_VALUE_T-TOTAL = 1.
          WHEN '2'.              "-Patrol
            LT_ZSQM_EQ_VALUE_T-PATROL = 1.
            LT_ZSQM_EQ_VALUE_T-TOTAL = 1.
          WHEN '3'.              "-External
            LT_ZSQM_EQ_VALUE_T-EXTER = 1.
            LT_ZSQM_EQ_VALUE_T-TOTAL = 1.
        ENDCASE.
*// Start of remark - sllee : 10/14/2003
*--  requested by Mr. Han :
*      WHEN 'ESON'.              "-Internal
*        LW_ZSQM_EQ_VALUE-INTER = 1.
*        LW_ZSQM_EQ_VALUE-TOTAL = 1.
*      WHEN 'ESPA'.              "-Patrol
*        LW_ZSQM_EQ_VALUE-PATROL = 1.
*        LW_ZSQM_EQ_VALUE-TOTAL = 1.
*      WHEN 'ESOF'.              "-External
*        LW_ZSQM_EQ_VALUE-EXTER = 1.
*        LW_ZSQM_EQ_VALUE-TOTAL = 1.
*// End of remark - sllee : 10/14/2003
      WHEN 'ESIO'.              "-Self
        LT_ZSQM_EQ_VALUE_T-SELF = 1.
        LT_ZSQM_EQ_VALUE_T-TOTAL = 1.
      WHEN 'EOSI'.              "-Outscope
        LT_ZSQM_EQ_VALUE_T-OUTSCOPE = 1.
        LT_ZSQM_EQ_VALUE_T-TOTAL = 1.
      WHEN 'IDLE'.              "-Idle
        LT_ZSQM_EQ_VALUE_T-IDLE = 1.
        LT_ZSQM_EQ_VALUE_T-TOTAL = 1.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    COLLECT LT_ZSQM_EQ_VALUE_T.
  ENDLOOP.

  CHECK NOT LT_ZSQM_EQ_VALUE_T[] IS INITIAL.

  SELECT B~ATINN B~ATZHL
         B~ADZHL A~ATWRT B~ATWTB
   INTO CORRESPONDING FIELDS OF TABLE LT_CAWNT
     FROM CAWN AS A INNER JOIN CAWNT AS B
       ON  A~ATINN = B~ATINN
       AND A~ATZHL = B~ATZHL
       AND A~ADZHL = B~ADZHL
      FOR ALL ENTRIES IN LT_ZSQM_EQ_VALUE_T
       WHERE A~ATINN = LT_ZSQM_EQ_VALUE_T-ATINN
*         AND A~ATZHL = LT_ZSQM_EQ_VALUE_T-ATZHL
*         AND A~ADZHL = LT_ZSQM_EQ_VALUE_T-ADZHL
         AND A~ATWRT = LT_ZSQM_EQ_VALUE_T-ATWRT
         AND B~SPRAS = SY-LANGU
         AND B~LKENZ = ' '.

*--------------------------------
  REFRESH IT_ZSQM_EQ_VALUE.

  LOOP AT LT_ZSQM_EQ_VALUE_T.

    CLEAR LW_ZSQM_EQ_VALUE.
    MOVE-CORRESPONDING LT_ZSQM_EQ_VALUE_T TO LW_ZSQM_EQ_VALUE.

    READ TABLE LT_CAWNT WITH KEY ATINN = LT_ZSQM_EQ_VALUE_T-ATINN
                                 ATWRT = LT_ZSQM_EQ_VALUE_T-ATWRT.

    IF SY-SUBRC = 0.
      MOVE : LT_CAWNT-ATWTB TO LW_ZSQM_EQ_VALUE-ATWTB.
    ENDIF.

    APPEND LW_ZSQM_EQ_VALUE TO IT_ZSQM_EQ_VALUE.
  ENDLOOP.

ENDFORM.                    " RET_COUNT_EQUIP_VALUE_LEVEL

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
*   displayed key fields of structure
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

**-- adjust field sort and subtotal to display total of column
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
*                                        IT_FIELDCAT.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_NEW_TABLE_DATA.

**    WA_VARIANT-VARIANT = '/ZRQM21_CLAS'.
*    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*         EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_CLASS'
*                   IS_LAYOUT        = WA_IS_LAYOUT
*                   I_SAVE           = WA_SAVE
*                   IS_VARIANT       = WA_VARIANT
*                   I_DEFAULT        = SPACE
*         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*                   IT_OUTTAB        = IT_ZSQM_EQ_CLASS.


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


**/-- Create Object to receive events and link them to handler methods.
*    CREATE OBJECT EVENT_RECEIVER.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
**-   toolbar control event
*    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
*
**- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD ALV_GRID->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.

  ENDIF.

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000' WITH WA_LEVEL.

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
      IF WA_LEVEL = C_CLASS.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN 'REFRESH'.
      REFRESH : IT_ZSQM_EQ_LIST, IT_ZSQM_EQ_CLASS,
                IT_ZSQM_EQ_CHAR, IT_ZSQM_EQ_VALUE.

      PERFORM RETRIEVE_SELECT_CRITERION.

      IF IT_ZSQM_EQ_LIST[] IS INITIAL.
        MESSAGE E000(ZMQM) WITH 'No entries!'(E01).
      ENDIF.

*--    Retrieve Equipment list count : Class Level
      PERFORM RET_COUNT_EQUIP_CLASS_LEVEL.

      CASE WA_LEVEL.
        WHEN C_CLASS.
        WHEN C_CHARACTERISTIC.
          PERFORM RET_COUNT_EQUIP_CHAR_LEVEL.
        WHEN C_VALUE.
          PERFORM RET_COUNT_EQUIP_VALUE_LEVEL.
        WHEN C_EQUIP.
          PERFORM RET_COUNT_EQUIP_LEVEL.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.



*  CASE WA_LEVEL.
*    WHEN C_CLASS.
*      CLEAR LW_ZSQM_EQ_CLASS.
*      READ TABLE IT_ZSQM_EQ_CLASS INDEX LW_SEL_INDEX
*                                     INTO LW_ZSQM_EQ_CLASS.
*      WA_CLASS = LW_ZSQM_EQ_CLASS-CLASS.
*      WA_SCR_TXT-CLINT = LW_ZSQM_EQ_CLASS-CLINT.
*
*      PERFORM RET_COUNT_EQUIP_CHAR_LEVEL.
*
*      WA_LEVEL = C_CHARACTERISTIC.
*
*    WHEN C_CHARACTERISTIC.
*      CLEAR LW_ZSQM_EQ_CHAR.
*      READ TABLE IT_ZSQM_EQ_CHAR INDEX LW_SEL_INDEX
*                                     INTO LW_ZSQM_EQ_CHAR.
*      WA_IMERK = LW_ZSQM_EQ_CHAR-IMERK.
*      WA_SCR_TXT-ATNAM = LW_ZSQM_EQ_CHAR-ATNAM.
*
*      PERFORM RET_COUNT_EQUIP_VALUE_LEVEL.
*
*      WA_LEVEL = C_VALUE.
*
*    WHEN C_VALUE.
*    WHEN OTHERS.
*  ENDCASE.



ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.
      IF WA_LEVEL = C_CLASS.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.
      PERFORM SET_LIST_LEVEL_CONTROL.
    WHEN 'RW'.
      IF WA_LEVEL = C_CLASS.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.
      PERFORM SET_LIST_LEVEL_CONTROL.
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

  CASE WA_LEVEL.
    WHEN C_CLASS.
      WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
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

    WHEN C_CHARACTERISTIC.
      WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
      WA_IS_LAYOUT-NUMC_TOTAL  = C_MARK. "/Allow totals  for NUMC
      WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row

    WHEN C_VALUE.
      WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
      WA_IS_LAYOUT-NUMC_TOTAL  = C_MARK. "/Allow totals  for NUMC
      WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row

    WHEN C_EQUIP.
      WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
*      WA_IS_LAYOUT-NUMC_TOTAL  = C_MARK. "/Allow totals  for NUMC
      WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row

  ENDCASE.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.

  CASE WA_LEVEL.
    WHEN C_CLASS.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'CLA'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN C_CHARACTERISTIC.
      LOOP AT SCREEN.
        IF SCREEN-GROUP2 = 'CHA'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN C_VALUE.
      LOOP AT SCREEN.
        IF SCREEN-GROUP3 = 'VAL'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.

ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

  CASE WA_LEVEL.
    WHEN C_CLASS.
* Build the fieldcat according to DDIC structure SBOOK:
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
           EXPORTING
                I_STRUCTURE_NAME = 'ZSQM_EQ_CLASS'
           CHANGING
                CT_FIELDCAT      = PT_FIELDCAT[].

* Set field attribute
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'CLASS'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          MODIFY PT_FIELDCAT.
* set left alignment for the two key fields left...
        ELSEIF PT_FIELDCAT-FIELDNAME = 'CLINT'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          MODIFY PT_FIELDCAT.
        ELSE.
          PT_FIELDCAT-JUST = 'R'.
*          PT_FIELDCAT-TOTAL = C_MARK.
          PT_FIELDCAT-DO_SUM = C_MARK.
          MODIFY PT_FIELDCAT.
        ENDIF.
      ENDLOOP.

    WHEN C_CHARACTERISTIC.

* Build the fieldcat according to DDIC structure SBOOK:
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
           EXPORTING
                I_STRUCTURE_NAME = 'ZSQM_EQ_CHAR'
           CHANGING
                CT_FIELDCAT      = PT_FIELDCAT[].

* suppress key fields CARRID, CONNID and FLDATE
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'IMERK'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          PT_FIELDCAT-JUST = 'L'.
          MODIFY PT_FIELDCAT.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'ATNAM'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          PT_FIELDCAT-JUST = 'L'.
          MODIFY PT_FIELDCAT.
* set left alignment for the two key fields left...
        ELSE.
          PT_FIELDCAT-JUST = 'R'.
          PT_FIELDCAT-DO_SUM = C_MARK.
          MODIFY PT_FIELDCAT.
        ENDIF.
      ENDLOOP.

    WHEN C_VALUE.
* Build the fieldcat according to DDIC structure SBOOK:
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
           EXPORTING
                I_STRUCTURE_NAME = 'ZSQM_EQ_VALUE'
           CHANGING
                CT_FIELDCAT      = PT_FIELDCAT[].

* suppress key fields CARRID, CONNID and FLDATE
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'ATWRT' OR
           PT_FIELDCAT-FIELDNAME = 'ATWTB'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          PT_FIELDCAT-JUST = 'L'.
          MODIFY PT_FIELDCAT.
* set left alignment for the two key fields left...
        ELSE.
          PT_FIELDCAT-JUST = 'R'.
          PT_FIELDCAT-DO_SUM = C_MARK.
          MODIFY PT_FIELDCAT.
        ENDIF.
      ENDLOOP.

    WHEN C_EQUIP.
* Build the fieldcat according to DDIC structure SBOOK:
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
           EXPORTING
                I_STRUCTURE_NAME = 'ZSQM_EQ_L_D'
           CHANGING
                CT_FIELDCAT      = PT_FIELDCAT[].

* suppress key fields CARRID, CONNID and FLDATE
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'EQUNR' OR
           PT_FIELDCAT-FIELDNAME = 'EQKTX'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
*          PT_FIELDCAT-NO_OUT = C_MARK.
*          PT_FIELDCAT-EMPHASIZE = C_MARK.
          PT_FIELDCAT-KEY      = C_MARK.
          PT_FIELDCAT-JUST = 'L'.
          MODIFY PT_FIELDCAT.
* set left alignment for the two key fields left...
        ELSEIF PT_FIELDCAT-FIELDNAME = 'STAT'.
          PT_FIELDCAT-NO_OUT = C_MARK.
          MODIFY PT_FIELDCAT.
        ELSE.
          MODIFY PT_FIELDCAT.
        ENDIF.
      ENDLOOP.
  ENDCASE.
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
  DATA : LT_UI_FUNCTIONS TYPE UI_FUNCTIONS WITH HEADER LINE.

  CASE WA_LEVEL.
    WHEN C_CLASS.
*      WA_VARIANT-VARIANT = '/ZRQM21_CLAS'.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_CLASS'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_EQ_CLASS.

    WHEN C_CHARACTERISTIC.
*      WA_VARIANT-VARIANT = '/ZRQM21_CHAR'.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_CHAR'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_EQ_CHAR.
    WHEN C_VALUE.
*      CLEAR LT_UI_FUNCTIONS. REFRESH LT_UI_FUNCTIONS.
*      MOVE : 'DETAIL' TO LT_UI_FUNCTIONS. APPEND LT_UI_FUNCTIONS.
*      WA_VARIANT-VARIANT = '/ZRQM21_VAL'.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_VALUE'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
*                   IT_TOOLBAR_EXCLUDING  = LT_UI_FUNCTIONS[]
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_EQ_VALUE.

    WHEN C_EQUIP.

      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_L_D'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
*                   IT_TOOLBAR_EXCLUDING  = LT_UI_FUNCTIONS[]
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                   IT_OUTTAB        = IT_ZSQM_EQ_L_D.


    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " SET_NEW_TABLE_DATA
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA USING    P_INDEX.
  DATA : LW_SEL_INDEX   LIKE SY-TABIX.
  DATA : LW_TABLE_LINES LIKE SY-TABIX.
  LW_SEL_INDEX = P_INDEX.

  DATA : LW_ZSQM_EQ_CLASS LIKE ZSQM_EQ_CLASS,
         LW_ZSQM_EQ_CHAR  LIKE ZSQM_EQ_CHAR,
         LW_ZSQM_EQ_VALUE LIKE ZSQM_EQ_VALUE.

  CASE WA_LEVEL.
    WHEN C_CLASS.
      CLEAR LW_ZSQM_EQ_CLASS.
      READ TABLE IT_ZSQM_EQ_CLASS INDEX LW_SEL_INDEX
                                     INTO LW_ZSQM_EQ_CLASS.
      WA_CLASS = LW_ZSQM_EQ_CLASS-CLASS.
      WA_SCR_TXT-CLINT = LW_ZSQM_EQ_CLASS-CLINT.

      PERFORM RET_COUNT_EQUIP_CHAR_LEVEL.

      WA_LEVEL = C_CHARACTERISTIC.

    WHEN C_CHARACTERISTIC.
      CLEAR LW_ZSQM_EQ_CHAR.
      READ TABLE IT_ZSQM_EQ_CHAR INDEX LW_SEL_INDEX
                                     INTO LW_ZSQM_EQ_CHAR.
      WA_IMERK = LW_ZSQM_EQ_CHAR-IMERK.
      WA_SCR_TXT-ATNAM = LW_ZSQM_EQ_CHAR-ATNAM.

      PERFORM RET_COUNT_EQUIP_VALUE_LEVEL.

      WA_LEVEL = C_VALUE.

    WHEN C_VALUE.
      CLEAR LW_ZSQM_EQ_VALUE.
      READ TABLE IT_ZSQM_EQ_VALUE INDEX LW_SEL_INDEX
                                     INTO LW_ZSQM_EQ_VALUE.
      WA_ATWRT = LW_ZSQM_EQ_VALUE-ATWRT.
      WA_SCR_TXT-ATWRT = LW_ZSQM_EQ_VALUE-ATWRT.
      WA_SCR_TXT-ATWTB = LW_ZSQM_EQ_VALUE-ATWTB.

      PERFORM RET_COUNT_EQUIP_LEVEL.

      WA_LEVEL = C_EQUIP.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " RETRIEV_DETAIL_DATA
*&-------------------------------------------------------------------*
*&      Form  SET_LIST_LEVEL_CONTROL
*&-------------------------------------------------------------------*
FORM SET_LIST_LEVEL_CONTROL.
  CASE WA_LEVEL.
    WHEN C_CLASS.
    WHEN C_CHARACTERISTIC.
      WA_LEVEL = C_CLASS.
      WA_RENEWAL_FLG = C_MARK.
    WHEN C_VALUE.
      WA_LEVEL = C_CHARACTERISTIC.
      WA_RENEWAL_FLG = C_MARK.
    WHEN C_EQUIP.
      WA_LEVEL = C_VALUE.
      WA_RENEWAL_FLG = C_MARK.
  ENDCASE.
ENDFORM.                    " SET_LIST_LEVEL_CONTROL
*&----------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&----------------------------------------------------------------*
FORM SET_SORT_TOTAL_FIELD TABLES   PT_SORT     TYPE LVC_T_SORT
                                   PT_FIELDCAT TYPE LVC_T_FCAT.
  REFRESH PT_SORT.
  CASE WA_LEVEL.
    WHEN C_CLASS.
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'CLASS' OR
           PT_FIELDCAT-FIELDNAME = 'CLINT'.
          CLEAR PT_SORT.
          PT_SORT-FIELDNAME = PT_FIELDCAT-FIELDNAME.
          PT_SORT-UP        = 'X'.
*          PT_SORT-EXPA      = 'X'.
          PT_SORT-SUBTOT    = 'X'.
          APPEND PT_SORT.
        ENDIF.
      ENDLOOP.

    WHEN C_CHARACTERISTIC.

      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'IMERK' OR
           PT_FIELDCAT-FIELDNAME = 'ATNAM'.
          CLEAR PT_SORT.
          PT_SORT-FIELDNAME = PT_FIELDCAT-FIELDNAME.
          PT_SORT-UP        = 'X'.
*          PT_SORT-EXPA      = 'X'.
          PT_SORT-SUBTOT    = 'X'.
          APPEND PT_SORT.
        ENDIF.
      ENDLOOP.

    WHEN C_VALUE.

      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'ATWTB'.
          CLEAR PT_SORT.
          PT_SORT-FIELDNAME = PT_FIELDCAT-FIELDNAME.
          PT_SORT-UP        = 'X'.
*          PT_SORT-EXPA      = 'X'.
          PT_SORT-SUBTOT    = 'X'.
          APPEND PT_SORT.
        ENDIF.
      ENDLOOP.

  ENDCASE.


ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&-----------------------------------------------------------------*
*&      Form  RET_COUNT_EQUIP_LEVEL
*&-----------------------------------------------------------------*
FORM RET_COUNT_EQUIP_LEVEL.

  DATA : LW_ZSQM_EQ_L_D LIKE ZSQM_EQ_L_D.

  REFRESH IT_ZSQM_EQ_L_D.

  LOOP AT IT_ZSQM_EQ_LIST WHERE CLASS = WA_CLASS
                           AND IMERK = WA_IMERK
                           AND ATWRT = WA_ATWRT.
    CLEAR LW_ZSQM_EQ_L_D .
    MOVE-CORRESPONDING  IT_ZSQM_EQ_LIST  TO LW_ZSQM_EQ_L_D .
    APPEND LW_ZSQM_EQ_L_D TO IT_ZSQM_EQ_L_D.
  ENDLOOP.

ENDFORM.                    " RET_COUNT_EQUIP_LEVEL
*&---------------------------------------------------------------------*
*&      Form  CALL_EQUIP_DISPLAY
*&---------------------------------------------------------------------*
FORM CALL_EQUIP_DISPLAY USING    P_EQUNR TYPE EQUNR.

  SET PARAMETER ID 'EQN' FIELD P_EQUNR.

  CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.

  SET PARAMETER ID 'EQN' FIELD ''.

ENDFORM.                    " CALL_EQUIP_DISPLAY
