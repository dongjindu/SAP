************************************************************************
* Program Name      : ZRQM04R_INSP_RESULT
* Author            : SeungLyong, Lee
* Creation Date     : 2003.11.24.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No :
* Addl Documentation:
* Description       : Inspection Result reports
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZRQM04R_INSP_RESULT  NO STANDARD PAGE HEADING.

*&&& Data Declaration.  &&&*
TYPE-POOLS ZQMT1.  "/QM-Type group for inspection
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
**
TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE : ZQM_INCLUDE_POOL01. "/Inspection Constants and etc
INCLUDE <ICON>.

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES : T001W,
         QALS,
         QPGR,
         QPCD,
         MARA,
         MARC,
         JEST.

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_IS_RES_SEL. "/Inspection Result Status -Selection screen
TABLES : ZSQM_IS_R_ISE, "/Inspection Result Status : ISIR - E001
         ZSQM_IS_R_ISP, "/Inspection Result Status : ISIR - P001
         ZSQM_IS_R_REG. "/Inspection Result Status : Regular - P001/E001
TABLES : ZSQM_IS_R_LIST. "/Inspection Result Status : SQL - List

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

*-- Report Level control constants
CONSTANTS : C_BASIC(10)  TYPE C VALUE 'BASIC',
            C_DETAIL(10) TYPE C VALUE 'DETAIL'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.

DATA :  WA_RENEWAL_FLG.
*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID,
       WA_DYNNR LIKE SY-DYNNR.

*-- Table Control Field Variables
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Global Structure of Radio Button for Screen Control according
*-- Selection of ISIR/Regular
DATA : BEGIN OF ST_DIST,
         REGU   TYPE C,
         ISIR   TYPE C,
       END OF ST_DIST.
*-- Control flag for interface with other program
DATA : WA_IMPORT_EXIST.

*-- List Box Set Flag
DATA : WA_LISTBOX_SET.

*-- Error check flag
DATA : WA_ERROR_FOUNDED.

*-- Inspection date
DATA : WA_FIRSTDAY  TYPE QPRSTART, "/Inspection Start Date
       WA_LASTDAY   TYPE QPRENDE.  "/End Date of the Inspection
*-- field count variable for dynamic access
DATA : WA_FLD_ISRE_CNT  TYPE I, "/Count of ISIR or Regular
       WA_FLD_MS_CNT   TYPE I.  "/Count of MS

*- Selected Ext. material group and responsible person
DATA : WA_EXTWG  TYPE EXTWG,
       WA_RESPP  TYPE ZQM_RESP_P.

*-- Report Level control
DATA : WA_LEVEL(10) TYPE C.

*//Ranges; (R_)
RANGES :
         R_LIFNR       FOR ZSQM_IS_RES_SEL-LIFNR_L,
         R_CODEGRP_VH  FOR ZSQM_IS_RES_SEL-CODEGRP_VH,
         R_CODE_VH     FOR ZSQM_IS_RES_SEL-CODE_VH,
         R_EXTWG       FOR ZSQM_IS_RES_SEL-EXTWG_L,
         R_PASTR       FOR QALS-PASTRTERM,
         R_PAEND       FOR QALS-PAENDTERM.
*         R_STAT        FOR JEST-STAT.


*//Internal Tables and Index Fields;(IT_), (I_)
*- Inspection Result Status : ISIR - E001
DATA : IT_ZSQM_IS_R_ISE  LIKE ZSQM_IS_R_ISE OCCURS 0 WITH HEADER LINE.
*- Inspection Result Status : ISIR - P001
DATA : IT_ZSQM_IS_R_ISP  LIKE ZSQM_IS_R_ISP OCCURS 0 WITH HEADER LINE.
*- Inspection Result Status : Regular - P001/E001
DATA : IT_ZSQM_IS_R_REG  LIKE ZSQM_IS_R_REG OCCURS 0 WITH HEADER LINE.
*- Inspection Result Status : SQL - List
DATA : IT_ZSQM_IS_R_LIST LIKE ZSQM_IS_R_LIST OCCURS 0 WITH HEADER LINE.

*- Inspection scheduling - Detail List
DATA : IT_ZSQM_IS_R_LIST_D LIKE ZSQM_IS_R_LIST OCCURS 0
                                                   WITH HEADER LINE.

**-- Vehicle/Engine type master temp table for Text input

*--/ Work area structure same as DB structure
*DATA :

*/// ALV Control....: Start
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

*// Declare reference variables, the container and internal table
DATA: WA_CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*DATA : WA_CONTROL_OBJECT TYPE REF TO CL_GUI_CONTROL.

*-- ALV LIST STRUCTURE
DATA : WA_STRUCTURE_NAME LIKE DD02L-TABNAME.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED. "/ALV Event Handling

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Global variables for attributes or etc of ALV GRID
DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO. "/The Layout Structure
DATA : IT_FIELDCAT   TYPE LVC_T_FCAT WITH HEADER LINE, "for Basic
       IT_FIELDCAT_D TYPE LVC_T_FCAT WITH HEADER LINE, "for Detail
       IT_SORT       TYPE LVC_T_SORT WITH HEADER LINE.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

*/// ALV Control....: End


*****//& Selection Screen Definition(Parameters Select-Option)
**-- Paramerters : (P_), Select-Options : (S_)


*
**-- Seclection Screen Flow Logic Control Event Handling
**AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
** ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
** ON EXIT-COMMAND )
*AT SELECTION-SCREEN OUTPUT.
**  SET TITLEBAR '1000'.
*
*AT SELECTION-SCREEN ON BLOCK BLK.
*  CHECK SY-UCOMM = 'ONLI'.
**-- Start of Selection
*START-OF-SELECTION.
***-- End of Selection.
*END-OF-SELECTION.


SET SCREEN 9000.


****//Macro Definitions
DEFINE RANGE_MACRO.
  CLEAR &1.
  MOVE : 'I' TO &1-SIGN.
  IF NOT &2 IS INITIAL AND &3 IS INITIAL.
    MOVE : 'EQ' TO &1-OPTION,
            &2  TO &1-LOW.
    APPEND &1.
  ELSEIF &2 IS INITIAL AND NOT &3 IS INITIAL.
    MOVE : 'EQ' TO &1-OPTION,
            &3  TO &1-LOW.
    APPEND &1.
  ELSEIF NOT &2 IS INITIAL AND NOT &3 IS INITIAL.
    MOVE : 'BT' TO &1-OPTION,
            &2  TO &1-LOW,
            &3  TO &1-HIGH.
    APPEND &1.
  ENDIF.
END-OF-DEFINITION.

*// Event Handling(Except Selection Screen (Flow)event)
*Load of Program.
LOAD-OF-PROGRAM.
*- Set Vehicle/Engine type catagory
  ZSQM_IS_RES_SEL-KATART_VH = C_VE_ENG_CAT_TYPE.

*- Set default date for inspection start date
  CONCATENATE SY-DATUM+0(4)
              '0101'    INTO ZSQM_IS_RES_SEL-PASTR_L.
  MOVE : SY-DATUM TO ZSQM_IS_RES_SEL-PASTR_H.


*-- Get Selection data for Selection from Memory ID using IMPORT
  IMPORT ZSQM_IS_RES_SEL  FROM MEMORY ID 'ZSCHEDULE'.

  IF SY-SUBRC = 0.  "/Import data exist.
    WA_IMPORT_EXIST = C_MARK.

    FREE MEMORY ID 'ZSCHEDULE'.
    MOVE : ZSQM_IS_RES_SEL-EXTWG_L TO WA_EXTWG.

*-- Set Inspection type radio button selection by inspection type
    IF ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_ISIR.
      ST_DIST-ISIR = C_MARK.
    ELSEIF ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_REGULAR.
      ST_DIST-REGU = C_MARK.
    ENDIF.

  ENDIF.

*Initialization.


****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS:

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW
                      E_COLUMN
                      ES_ROW_NO,

    HANDLE_TOOLBAR
        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,

    HANDLE_USER_COMMAND
        FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING E_UCOMM,


    HANDLE_BEFORE_USER_COMMAND
        FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
          IMPORTING E_UCOMM.



*  PRIVATE SECTION.

ENDCLASS.

* lcl_event_receiver (Definition)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_BEFORE_USER_COMMAND.
    DATA : LW_UCOMM LIKE SY-UCOMM.
    LW_UCOMM = E_UCOMM.
  ENDMETHOD.


*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

    CASE WA_LEVEL.
      WHEN C_BASIC.
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

      WHEN C_DETAIL.

      WHEN OTHERS.

    ENDCASE.


  ENDMETHOD.
*-------------------------------------------------------------------

*-- / Double Click
  METHOD HANDLE_DOUBLE_CLICK.
*- read selected row from internal table using INDEX E_ROW-INDEX
*Parameters.
*  E_ROW	Type	LVC_S_ROW		Row ID
*  E_COLUMN	Type	LVC_S_COL		Column Name
*  ES_ROW_NO	Type	LVC_S_ROID		Numeric Row ID
    CASE WA_LEVEL.
      WHEN C_BASIC.

        CHECK NOT E_ROW-INDEX IS INITIAL.

        PERFORM RETRIEV_DETAIL_DATA USING E_ROW-INDEX.

*        CHECK NOT IT_ZSQM_INSP_REJ_D[] IS INITIAL.

        WA_LEVEL = C_DETAIL.

        WA_RENEWAL_FLG = C_MARK.

      WHEN C_DETAIL.
        CHECK NOT E_ROW-INDEX IS INITIAL.

        PERFORM RETRIEV_DETAIL_DATA USING E_ROW-INDEX.

        CHECK NOT IT_ZSQM_IS_R_LIST_D[] IS INITIAL.

        WA_LEVEL = C_DETAIL.

        WA_RENEWAL_FLG = C_MARK.
    ENDCASE.
  ENDMETHOD.                           "handle_double_click


*-- / Handling User defined commands for Toolbar
  METHOD HANDLE_USER_COMMAND.
*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.
    DATA : LT_ROWS   TYPE LVC_T_ROW,  "/Indexes of Selected Rows
           LT_ROW_NO TYPE LVC_T_ROID, "/Numeric IDs of Selected Rows

           LW_LINE_ROW LIKE LINE OF LT_ROWS,
           LW_ROW_NO   LIKE LINE OF LT_ROW_NO.

    DATA : LW_LINES LIKE SY-TABIX.
    DATA : LW_ERROR TYPE C.

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

          CHECK NOT IT_ZSQM_IS_R_LIST_D[] IS INITIAL.

          WA_LEVEL = C_DETAIL.

          WA_RENEWAL_FLG = C_MARK.
        ENDIF.

      WHEN 'UD_CHANGE'.
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

*          PERFORM PROC_CHAGE_USAGE_DECISION USING LW_LINE_ROW-INDEX.

          WA_LEVEL = C_DETAIL.

          WA_RENEWAL_FLG = C_MARK.
        ENDIF.

      WHEN OTHERS.
*      N/A
    ENDCASE.


  ENDMETHOD.

ENDCLASS.
* lcl_event_receiver (Implementation)
*===================================================================


**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
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
*&      Module  CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
MODULE CHECK_VH_ENG_INPUT INPUT.

  DATA : LW_ZVQM_VEHICLE TYPE ZVQM_VEHICLE.

  IF     ZSQM_IS_RES_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_IS_RES_SEL-CODE_VH    IS INITIAL.

    MESSAGE E000(ZMQM)
        WITH 'Please Input Vehicle/Engine Code Group.'(E01).

  ENDIF.

  CHECK NOT ZSQM_IS_RES_SEL-CODEGRP_VH IS INITIAL AND
        NOT ZSQM_IS_RES_SEL-CODE_VH    IS INITIAL.


  SELECT SINGLE * INTO LW_ZVQM_VEHICLE
       FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_IS_RES_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_RES_SEL-CODEGRP_VH
           AND CODE       = ZSQM_IS_RES_SEL-CODE_VH.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH
       'Vehicle/Engine type  code group and code are not matched'(E02).
  ENDIF.

ENDMODULE.                 " CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
*&      Module  SET_LISTBOX_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE SET_LISTBOX_9000 OUTPUT.
*  CHECK WA_LISTBOX_SET IS INITIAL.
*  WA_LISTBOX_SET = C_MARK.
*
**- Set List for Selection screen : Inspectio type
*  PERFORM SET_LISTBOX_FOR_QPART   USING 'ZSQM_IS_RES_SEL-ART'.
*
**- Set List for Selection screen : Inspectio UD Code
*  PERFORM SET_LISTBOX_FOR_UDCODE  USING 'ZSQM_IS_RES_SEL-UD_SEL'.

ENDMODULE.                 " SET_LISTBOX_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Form  SET_LISTBOX_FOR_QPART
*&------------------------------------------------------------------*
FORM SET_LISTBOX_FOR_QPART USING    VALUE(P_SCREEN_FLD_NAME).


  DATA: LT_LIST  TYPE VRM_VALUES,
        LW_VALUE LIKE LINE OF LT_LIST.

**-- Inspection Type Constants
*CONSTANTS : C_INSP_TYPE_ISIR    TYPE QPART VALUE '8910',
*            C_INSP_TYPE_REGULAR TYPE QPART VALUE '8920',
*            C_INSP_TYPE_MS      TYPE QPART VALUE '8930'.

  SELECT  ART  AS KEY
          KURZTEXT AS TEXT
        INTO CORRESPONDING FIELDS OF TABLE LT_LIST
     FROM TQ30T
       WHERE SPRACHE = SY-LANGU
         AND ART IN (C_INSP_TYPE_ISIR,
                     C_INSP_TYPE_REGULAR,
                     C_INSP_TYPE_MS).

  PERFORM EXECUTE_SET_VALUES   TABLES LT_LIST
                               USING  P_SCREEN_FLD_NAME.

ENDFORM.                    " SET_LISTBOX_FOR_QPART
*&-------------------------------------------------------------------*
*&      Form  SET_LISTBOX_FOR_UDCODE
*&-------------------------------------------------------------------*
FORM SET_LISTBOX_FOR_UDCODE USING    VALUE(P_SCREEN_FLD_NAME).
  DATA: LT_LIST  TYPE VRM_VALUES,
        LW_VALUE LIKE LINE OF LT_LIST.


  SELECT CODE     AS KEY
         KURZTEXT AS TEXT
       INTO CORRESPONDING FIELDS OF TABLE LT_LIST
         FROM QPCT
           WHERE KATALOGART = C_UD_CODE_KATART
             AND CODEGRUPPE = C_UD_CODE_GRUPPE
             AND CODE       IN (C_UD_CODE_MD,
                                C_UD_CODE_REJECT)
             AND SPRACHE    = SY-LANGU.

  MOVE : ' '   TO LW_VALUE-KEY,
         'All' TO LW_VALUE-TEXT.
  INSERT LW_VALUE INTO LT_LIST INDEX 1.

  PERFORM EXECUTE_SET_VALUES   TABLES LT_LIST
                               USING  P_SCREEN_FLD_NAME.

ENDFORM.                    " SET_LISTBOX_FOR_UDCODE
*&------------------------------------------------------------------*
*&      Form  EXECUTE_SET_VALUES
*&------------------------------------------------------------------*
FORM EXECUTE_SET_VALUES TABLES   PT_LIST TYPE VRM_VALUES
                        USING    P_FLD_NAME.
  DATA: LT_LIST  TYPE VRM_VALUES.
  DATA: LW_NAME  TYPE VRM_ID.

  LW_NAME = P_FLD_NAME.
  LT_LIST[] = PT_LIST[].

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = LW_NAME
            VALUES = LT_LIST.

ENDFORM.                    " EXECUTE_SET_VALUES
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_SCREEN_9000 INPUT.

  CLEAR : ZSQM_IS_RES_SEL-KURZTEXT_G,
          ZSQM_IS_RES_SEL-KURZTEXT_VH.


  IF NOT ZSQM_IS_RES_SEL-CODEGRP_VH IS INITIAL.
    SELECT SINGLE KURZTEXT INTO ZSQM_IS_RES_SEL-KURZTEXT_G
      FROM ZVQM_QPGRT01
         WHERE KATALOGART = ZSQM_IS_RES_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_RES_SEL-CODEGRP_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_RES_SEL-CODEGRP_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_IS_RES_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_IS_RES_SEL-CODE_VH    IS INITIAL.
    SELECT SINGLE KURZTEXT_C INTO ZSQM_IS_RES_SEL-KURZTEXT_VH
      FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_IS_RES_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_RES_SEL-CODEGRP_VH
           AND CODE       = ZSQM_IS_RES_SEL-CODE_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_RES_SEL-CODEGRP_VH
                              ','
                              ZSQM_IS_RES_SEL-CODE_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

ENDMODULE.                 " GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  IF WA_IMPORT_EXIST = C_MARK AND SY-UCOMM IS INITIAL.
    SY-UCOMM = 'ONLI'.
  ENDIF.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'ONLI'.

*    - Set inspection type by user selection of radio button
      PERFORM SET_INSPECTION_TYPE.
*    - Set plant by Vehicle/Engine type Code Group
      PERFORM SET_PLAT_DATA_BY_CODE_G.
*    - Set range data by user input for query to DB
      PERFORM SET_RANGE_VALUE.

*   - Get data and count for Basic list.
      PERFORM GET_DATA_AND_LIST_BASIC.

      CHECK NOT IT_ZSQM_IS_R_LIST[] IS INITIAL.

      WA_RENEWAL_FLG = C_MARK.

      IF WA_IMPORT_EXIST = ' '. "/Normal
        WA_LEVEL = C_BASIC.
        CALL SCREEN 9100.

      ELSE. "/Interface
        WA_LEVEL = C_DETAIL.
        PERFORM RETRIEV_DETAIL_D_FOR_INTERFACE.

        CHECK NOT IT_ZSQM_IS_R_LIST_D[] IS INITIAL.

        CALL SCREEN 9100.

      ENDIF.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.

  CASE ZSQM_IS_RES_SEL-ART.
    WHEN C_INSP_TYPE_ISIR.

    WHEN C_INSP_TYPE_REGULAR.

    WHEN  C_INSP_TYPE_MS.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT

*&-----------------------------------------------------------------*
*&      Form  SET_RANGE_VALUE
*&-----------------------------------------------------------------*
FORM SET_RANGE_VALUE.

  REFRESH : R_LIFNR, R_CODEGRP_VH,
            R_CODE_VH, R_EXTWG, R_PASTR, R_PAEND.
  CLEAR   : R_LIFNR, R_CODEGRP_VH,
            R_CODE_VH, R_EXTWG, R_PASTR, R_PAEND.

  RANGE_MACRO :
     R_LIFNR       ZSQM_IS_RES_SEL-LIFNR_L  ZSQM_IS_RES_SEL-LIFNR_H,
     R_CODEGRP_VH  ZSQM_IS_RES_SEL-CODEGRP_VH  '',
     R_CODE_VH     ZSQM_IS_RES_SEL-CODE_VH     '',
     R_EXTWG    ZSQM_IS_RES_SEL-EXTWG_L  ZSQM_IS_RES_SEL-EXTWG_H,
     R_PASTR    ZSQM_IS_RES_SEL-PASTR_L  ZSQM_IS_RES_SEL-PASTR_H,
     R_PAEND    ZSQM_IS_RES_SEL-PAEND_L  ZSQM_IS_RES_SEL-PAEND_H.

ENDFORM.                    " SET_RANGE_VALUE
*&------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&------------------------------------------------------------------*
FORM GET_DATA_FROM_DB.
  REFRESH IT_ZSQM_IS_R_LIST.

  CASE 'X'.

    WHEN ST_DIST-REGU.
      SELECT F~EXTWG D~RESPP A~MATNR  A~LIFNR G~MAKTX
               A~CODE_IP  A~PRUEFLOS B~PRUEFLOS_MS
               A~DATUV_0010 A~DATUB_0010 A~DATUV_0020 A~DATUB_0020
               A~DATUV_0030 A~DATUB_0030 A~DATUV_0040 A~DATUB_0040
               A~DATUV_0050 A~DATUB_0050 A~DATUV_0060 A~DATUB_0060
               A~DATUV_0070 A~DATUB_0070 A~DATUV_0080 A~DATUB_0080
               A~DATUV_0090 A~DATUB_0090 A~DATUV_0100 A~DATUB_0100
               A~DATUV_0110 A~DATUB_0110 A~DATUV_0120 A~DATUB_0120
               A~DATUV_0130 A~DATUB_0130 A~DATUV_0140 A~DATUB_0140
               A~DATUV_0150 A~DATUB_0150 A~DATUV_0160 A~DATUB_0160
               A~DATUV_0170 A~DATUB_0170 A~DATUV_0180 A~DATUB_0180
               A~DATUV_0190 A~DATUB_0190 A~DATUV_0200 A~DATUB_0200
               A~DATUV_0210 A~DATUB_0210 A~DATUV_0220 A~DATUB_0220
               B~DATUV_1010 B~DATUB_1010 B~DATUV_1020 B~DATUB_1020
               B~DATUV_1030 B~DATUB_1030
                   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_IS_R_LIST
        FROM  ( ( ( ( ZTQM_QNS_ITEM AS A INNER JOIN ZTQM_QNS_IT_MS AS B
                ON   A~IYEAR      = B~IYEAR
                 AND A~KATART_VH  = B~KATART_VH
                 AND A~CODEGRP_VH = B~CODEGRP_VH
                 AND A~CODE_VH    = B~CODE_VH
                 AND A~ART        = B~ART
                 AND A~MATNR      = B~MATNR
                 AND A~EONO       = B~EONO
             AND A~LIFNR      = B~LIFNR ) INNER JOIN ZTQM_QNS_IT_D AS C
                ON   A~IYEAR      = C~IYEAR
                 AND A~KATART_VH  = C~KATART_VH
                 AND A~CODEGRP_VH = C~CODEGRP_VH
                 AND A~CODE_VH    = C~CODE_VH
                 AND A~ART        = C~ART
                 AND A~MATNR      = C~MATNR
                 AND A~EONO       = C~EONO
                 AND A~LIFNR      = C~LIFNR
*                                        ) INNER JOIN QALS AS C
*          ON   A~PRUEFLOS   = C~PRUEFLOS
                                         ) INNER JOIN ZTQM_MAT_REG AS D
                ON  A~MATNR       = D~MATNR


*          AND A~EONO        = D~EONO
                AND A~LIFNR       = D~LIFNR
                                               ) INNER JOIN MARA AS F
                ON   A~MATNR      = F~MATNR    ) INNER JOIN MAKT AS G
                ON   F~MATNR      = G~MATNR


                 WHERE A~ART        = ZSQM_IS_RES_SEL-ART
                   AND A~KATART_VH  = ZSQM_IS_RES_SEL-KATART_VH
                   AND A~CODEGRP_VH = ZSQM_IS_RES_SEL-CODEGRP_VH
                   AND A~CODE_VH   IN R_CODE_VH
                   AND A~LIFNR     IN R_LIFNR
                   AND F~EXTWG     IN R_EXTWG
                   AND C~DATUV_FD  IN R_PASTR
                   AND C~DATUV_LD  IN R_PAEND
*             AND C~PASTRTERM IN R_PASTR
*             AND C~PAENDTERM IN R_PAEND
                   AND A~I_STAT     = C_RELEASE
                   AND G~SPRAS      = SY-LANGU.



    WHEN ST_DIST-ISIR.




      SELECT F~EXTWG D~RESPP A~MATNR A~EONO A~LIFNR G~MAKTX
             A~CODE_IP  A~PRUEFLOS B~PRUEFLOS_MS
             A~DATUV_0010 A~DATUB_0010 A~DATUV_0020 A~DATUB_0020
             A~DATUV_0030 A~DATUB_0030 A~DATUV_0040 A~DATUB_0040
             A~DATUV_0050 A~DATUB_0050 A~DATUV_0060 A~DATUB_0060
             A~DATUV_0070 A~DATUB_0070 A~DATUV_0080 A~DATUB_0080
             A~DATUV_0090 A~DATUB_0090 A~DATUV_0100 A~DATUB_0100
             A~DATUV_0110 A~DATUB_0110 A~DATUV_0120 A~DATUB_0120
             A~DATUV_0130 A~DATUB_0130 A~DATUV_0140 A~DATUB_0140
             A~DATUV_0150 A~DATUB_0150 A~DATUV_0160 A~DATUB_0160
             A~DATUV_0170 A~DATUB_0170 A~DATUV_0180 A~DATUB_0180
             A~DATUV_0190 A~DATUB_0190 A~DATUV_0200 A~DATUB_0200
             A~DATUV_0210 A~DATUB_0210 A~DATUV_0220 A~DATUB_0220
             B~DATUV_1010 B~DATUB_1010 B~DATUV_1020 B~DATUB_1020
             B~DATUV_1030 B~DATUB_1030
                   INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_IS_R_LIST
        FROM  ( ( ( ( ZTQM_QNS_ITEM AS A INNER JOIN ZTQM_QNS_IT_MS AS B
              ON   A~IYEAR      = B~IYEAR
               AND A~KATART_VH  = B~KATART_VH
               AND A~CODEGRP_VH = B~CODEGRP_VH
               AND A~CODE_VH    = B~CODE_VH
               AND A~ART        = B~ART
               AND A~MATNR      = B~MATNR
               AND A~EONO       = B~EONO
             AND A~LIFNR      = B~LIFNR ) INNER JOIN ZTQM_QNS_IT_D AS C
              ON   A~IYEAR      = C~IYEAR
               AND A~KATART_VH  = C~KATART_VH
               AND A~CODEGRP_VH = C~CODEGRP_VH
               AND A~CODE_VH    = C~CODE_VH
               AND A~ART        = C~ART
               AND A~MATNR      = C~MATNR
               AND A~EONO       = C~EONO
               AND A~LIFNR      = C~LIFNR
*                                        ) INNER JOIN QALS AS C
*          ON   A~PRUEFLOS   = C~PRUEFLOS
                                        ) INNER JOIN ZTQM_MAT_ISIR AS D
              ON  A~MATNR       = D~MATNR
              AND A~EONO        = D~EONO
              AND A~LIFNR       = D~LIFNR
                                             ) INNER JOIN MARA AS F
              ON   A~MATNR      = F~MATNR    ) INNER JOIN MAKT AS G
              ON   F~MATNR      = G~MATNR


               WHERE A~ART        = ZSQM_IS_RES_SEL-ART
                 AND A~KATART_VH  = ZSQM_IS_RES_SEL-KATART_VH
                 AND A~CODEGRP_VH = ZSQM_IS_RES_SEL-CODEGRP_VH
                 AND A~CODE_VH   IN R_CODE_VH
                 AND A~LIFNR     IN R_LIFNR
                 AND F~EXTWG     IN R_EXTWG
                 AND C~DATUV_FD  IN R_PASTR
                 AND C~DATUV_LD  IN R_PAEND
*             AND C~PASTRTERM IN R_PASTR
*             AND C~PAENDTERM IN R_PAEND
                 AND A~I_STAT     = C_RELEASE
                 AND G~SPRAS      = SY-LANGU.
  ENDCASE.

  CHECK SY-SUBRC NE 0.

  MESSAGE W000(ZMQM) WITH 'No entries.'(W01).

ENDFORM.                    " GET_DATA_FROM_DB
*&-----------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Form  COUNT_UP_DATA_FOR_BASIC
*&------------------------------------------------------------------*
FORM COUNT_UP_DATA_FOR_BASIC.
  REFRESH : IT_ZSQM_IS_R_ISE , IT_ZSQM_IS_R_ISP, IT_ZSQM_IS_R_REG.

  DATA : LW_FLD_N_IND(4) TYPE N.


  LOOP AT IT_ZSQM_IS_R_LIST.
    CLEAR : IT_ZSQM_IS_R_ISE , IT_ZSQM_IS_R_ISP, IT_ZSQM_IS_R_REG.

*-   Move ext. material group code to IT
    CASE ZSQM_IS_RES_SEL-ART.
      WHEN C_INSP_TYPE_ISIR.
        CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
          WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
            MOVE : IT_ZSQM_IS_R_LIST-EXTWG TO IT_ZSQM_IS_R_ISE-EXTWG,
                   IT_ZSQM_IS_R_LIST-RESPP TO IT_ZSQM_IS_R_ISE-RESPP,

                   1                       TO IT_ZSQM_IS_R_ISE-MATNR_C.
            IF   NOT IT_ZSQM_IS_R_LIST-PRUEFLOS IS INITIAL.
              MOVE : 1  TO IT_ZSQM_IS_R_ISE-PLOS_C.
            ENDIF.
            IF   NOT IT_ZSQM_IS_R_LIST-PRUEFLOS_MS IS INITIAL.
              MOVE : 1  TO IT_ZSQM_IS_R_ISE-PLOS_MS_C.
            ENDIF.

          WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
            MOVE : IT_ZSQM_IS_R_LIST-EXTWG TO IT_ZSQM_IS_R_ISP-EXTWG,
                   IT_ZSQM_IS_R_LIST-RESPP TO IT_ZSQM_IS_R_ISP-RESPP,
                   1                       TO IT_ZSQM_IS_R_ISP-MATNR_C.
            IF  NOT IT_ZSQM_IS_R_LIST-PRUEFLOS IS INITIAL.
              MOVE : 1  TO IT_ZSQM_IS_R_ISP-PLOS_C.
            ENDIF.
            IF   NOT IT_ZSQM_IS_R_LIST-PRUEFLOS_MS IS INITIAL.
              MOVE : 1  TO IT_ZSQM_IS_R_ISP-PLOS_MS_C.
            ENDIF.
        ENDCASE.
      WHEN C_INSP_TYPE_REGULAR.
        MOVE : IT_ZSQM_IS_R_LIST-EXTWG TO IT_ZSQM_IS_R_REG-EXTWG,
* Changed by 100565 request by Mr Moon 07/19/04 - Start
         IT_ZSQM_IS_R_LIST-Respp TO IT_ZSQM_IS_R_REG-Respp,
* Changed by 100565 request by Mr Moon 07/19/04 - End
               1                       TO IT_ZSQM_IS_R_REG-MATNR_C.
        IF   NOT IT_ZSQM_IS_R_LIST-PRUEFLOS IS INITIAL.
          MOVE : 1  TO IT_ZSQM_IS_R_REG-PLOS_C.
        ENDIF.
        IF   NOT IT_ZSQM_IS_R_LIST-PRUEFLOS_MS IS INITIAL.
          MOVE : 1  TO IT_ZSQM_IS_R_REG-PLOS_MS_C.
        ENDIF.
    ENDCASE.

*-- ISIR/REGULAR
    CLEAR LW_FLD_N_IND.
    DO WA_FLD_ISRE_CNT TIMES.
      LW_FLD_N_IND =  LW_FLD_N_IND + 10.
*      - count Start/End date of MIC
      PERFORM COUNT_DATS_FIELD  USING LW_FLD_N_IND.
    ENDDO.
*-- MS
    LW_FLD_N_IND = '1000'.

    DO WA_FLD_MS_CNT  TIMES.
      LW_FLD_N_IND =  LW_FLD_N_IND + 10.
*      - count Start/End date of MIC
      PERFORM COUNT_DATS_FIELD  USING LW_FLD_N_IND.
    ENDDO.

    CASE ZSQM_IS_RES_SEL-ART.
      WHEN C_INSP_TYPE_ISIR.
        CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
          WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
            COLLECT IT_ZSQM_IS_R_ISE.
          WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
            COLLECT IT_ZSQM_IS_R_ISP.
        ENDCASE.
      WHEN C_INSP_TYPE_REGULAR.
        COLLECT IT_ZSQM_IS_R_REG.
    ENDCASE.


  ENDLOOP.

*-- Sort
  CASE ZSQM_IS_RES_SEL-ART.
    WHEN C_INSP_TYPE_ISIR.
      CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
        WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
          SORT IT_ZSQM_IS_R_ISE BY  EXTWG     ASCENDING.
        WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
          SORT IT_ZSQM_IS_R_ISP BY  EXTWG     ASCENDING.
      ENDCASE.
    WHEN C_INSP_TYPE_REGULAR.
      SORT IT_ZSQM_IS_R_REG BY  EXTWG     ASCENDING.
  ENDCASE.


ENDFORM.                    " COUNT_UP_DATA_FOR_BASIC
*&-------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&-------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
  IF WA_IMPORT_EXIST = C_MARK.
    SET TITLEBAR  '9100' WITH C_DETAIL.
  ELSE.
    SET TITLEBAR  '9100' WITH WA_LEVEL.
  ENDIF.

ENDMODULE.                 " STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9100 OUTPUT.
  CASE WA_LEVEL.
    WHEN C_BASIC.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'DET'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN C_DETAIL.
  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9100  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  IF WA_IMPORT_EXIST = ' '.
    CASE OK_CODE.
      WHEN 'EXIT'.

        CASE WA_LEVEL.
          WHEN C_BASIC.
            LEAVE TO SCREEN 0.
          WHEN OTHERS.
        ENDCASE.
        PERFORM SET_LIST_LEVEL_CONTROL.
      WHEN 'RW'.

        CASE WA_LEVEL.
          WHEN C_BASIC.
            LEAVE TO SCREEN 0.
          WHEN OTHERS.
        ENDCASE.
        PERFORM SET_LIST_LEVEL_CONTROL.
      WHEN OTHERS.
    ENDCASE.
  ELSE.
    CASE OK_CODE.
      WHEN 'EXIT' OR 'RW'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " EXIT_9100  INPUT
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
*&      Module  USER_COMMAND_9100  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      IF WA_IMPORT_EXIST = ' '.
        CASE WA_LEVEL.
          WHEN C_BASIC.
            PERFORM SET_LIST_LEVEL_CONTROL.
            LEAVE TO SCREEN 0.
          WHEN OTHERS.
            PERFORM SET_LIST_LEVEL_CONTROL.
        ENDCASE.
      ELSE.
        LEAVE PROGRAM.
      ENDIF.

    WHEN 'REFRESH'.
*   - Get data and count for Basic list.
      PERFORM GET_DATA_AND_LIST_BASIC.

      IF WA_LEVEL = C_DETAIL.

        PERFORM RETRIEV_DETAIL_DATA_REFRESH.

      ENDIF.

      WA_RENEWAL_FLG = C_MARK.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9100  INPUT
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
    PERFORM SET_FIELD_ATTR_FOR_ISIR_P  TABLES IT_FIELDCAT.

*--if this progrma was called by other program and Import value existed.
*-- WA_IMPORT_EXIST = C_MARK.
*--  WA_LEVEL = C_BASIC
    IF WA_IMPORT_EXIST = C_MARK.

*-- ADJUST FIELD CATALOG TO SUPPRESS THE OUTPUT OF ALREADY
*   displayed key fields of structure
****//   for Detail of interface, Basic list fieldcat data read
      PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.
*        Set field attribute/Text by by Inspection type(ISIR, Regular)
      PERFORM SET_FIELD_ATTR_FOR_ISIR_P  TABLES IT_FIELDCAT.

****/// Detail
      PERFORM MASK_COLUMNS_FOR_INTERFACE TABLES IT_FIELDCAT_D.
*        Set field attribute/Text for Detail
      PERFORM SET_FIELD_ATTR_FOR_DETAIL  TABLES IT_FIELDCAT
                                                IT_FIELDCAT_D.
    ENDIF.


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
    CASE WA_LEVEL.
      WHEN C_BASIC.
        PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.
*        Set field attribute/Text by by Inspection type(ISIR, Regular)
        PERFORM SET_FIELD_ATTR_FOR_ISIR_P  TABLES IT_FIELDCAT.

      WHEN C_DETAIL.
        PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT_D.
*        Set field attribute/Text for Detail
        PERFORM SET_FIELD_ATTR_FOR_DETAIL  TABLES IT_FIELDCAT
                                                  IT_FIELDCAT_D.
    ENDCASE.

*--if this progrma was called by other program and Import value existed.
*-- WA_IMPORT_EXIST = C_MARK.
*--  WA_LEVEL = C_BASIC
    IF WA_IMPORT_EXIST = C_MARK.
      PERFORM MASK_COLUMNS_FOR_INTERFACE TABLES IT_FIELDCAT_D.
*        Set field attribute/Text for Detail
      PERFORM SET_FIELD_ATTR_FOR_DETAIL  TABLES IT_FIELDCAT
                                                IT_FIELDCAT_D.
    ENDIF.


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
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG.           "/ HMMA QM Engine type
              WA_STRUCTURE_NAME = 'ZSQM_IS_R_ISE'.
            WHEN C_VE_ENG_CG_VEH.           "/ HMMA QM Vehicle type
              WA_STRUCTURE_NAME = 'ZSQM_IS_R_ISP'.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          WA_STRUCTURE_NAME = 'ZSQM_IS_R_REG'.
      ENDCASE.
    WHEN C_DETAIL.
      WA_STRUCTURE_NAME = 'ZSQM_IS_R_LIST'.

  ENDCASE.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = WA_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].



ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
FORM SET_NEW_TABLE_DATA.

  IF WA_IMPORT_EXIST = ' '.
    CASE WA_LEVEL.
      WHEN C_BASIC.

        CASE ZSQM_IS_RES_SEL-ART.
          WHEN C_INSP_TYPE_ISIR.

            CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
              WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type

                CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
                   EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                             IS_LAYOUT        = WA_IS_LAYOUT
                             I_SAVE           = WA_SAVE
                             IS_VARIANT       = WA_VARIANT
                             I_DEFAULT        = SPACE
                          IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
                   CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                             IT_OUTTAB        = IT_ZSQM_IS_R_ISE[].
              WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
                CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
                   EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                             IS_LAYOUT        = WA_IS_LAYOUT
                             I_SAVE           = WA_SAVE
                             IS_VARIANT       = WA_VARIANT
                             I_DEFAULT        = SPACE
                          IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
                   CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                             IT_OUTTAB        = IT_ZSQM_IS_R_ISP[].
            ENDCASE.
          WHEN C_INSP_TYPE_REGULAR.
            CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
               EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                         IS_LAYOUT        = WA_IS_LAYOUT
                         I_SAVE           = WA_SAVE
                         IS_VARIANT       = WA_VARIANT
                         I_DEFAULT        = SPACE
                         IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
               CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                         IT_OUTTAB        = IT_ZSQM_IS_R_REG[].
        ENDCASE.


      WHEN C_DETAIL.
        CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                    IS_LAYOUT        = WA_IS_LAYOUT
                    I_SAVE           = WA_SAVE
                    IS_VARIANT       = WA_VARIANT
                    I_DEFAULT        = SPACE
                    IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
          CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT_D[]
                    IT_OUTTAB        = IT_ZSQM_IS_R_LIST_D[].
    ENDCASE.

  ELSE.
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                IS_LAYOUT        = WA_IS_LAYOUT
                I_SAVE           = WA_SAVE
                IS_VARIANT       = WA_VARIANT
                I_DEFAULT        = SPACE
                IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
      CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT_D[]
                IT_OUTTAB        = IT_ZSQM_IS_R_LIST_D[].

  ENDIF.

ENDFORM.                    " SET_NEW_TABLE_DATA
*&------------------------------------------------------------------*
*&      Form  REFRESH_ALV_GRID_DATA_DISP
*&------------------------------------------------------------------*
FORM REFRESH_ALV_GRID_DATA_DISP.
*-- Refresh field category
  CALL METHOD ALV_GRID->SET_FRONTEND_FIELDCATALOG
       EXPORTING
         IT_FIELDCATALOG = IT_FIELDCAT[].

*-- Refresh table display
*the position of the scroll bar for the rows or columns remains stable.
  DATA : LW_IS_STABLE LIKE  LVC_S_STBL.

  MOVE  C_MARK TO : LW_IS_STABLE-ROW,
                    LW_IS_STABLE-COL.

  CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
         EXPORTING
           IS_STABLE      = LW_IS_STABLE
*           I_SOFT_REFRESH = C_MARK
         EXCEPTIONS
           FINISHED       = 1
           OTHERS         = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " REFRESH_ALV_GRID_DATA_DISP
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA USING    P_INDEX.
  DATA : LW_SEL_INDEX   LIKE SY-TABIX.
  DATA : LW_TABLE_LINES LIKE SY-TABIX.

  LW_SEL_INDEX = P_INDEX.

  REFRESH IT_ZSQM_IS_R_LIST_D.

  CASE WA_LEVEL .
    WHEN C_BASIC.
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
              READ TABLE IT_ZSQM_IS_R_ISE  INDEX LW_SEL_INDEX.
              MOVE : IT_ZSQM_IS_R_ISE-EXTWG TO WA_EXTWG,
                     IT_ZSQM_IS_R_ISE-RESPP TO WA_RESPP.
            WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
              READ TABLE IT_ZSQM_IS_R_ISP  INDEX LW_SEL_INDEX.
              MOVE : IT_ZSQM_IS_R_ISP-EXTWG TO WA_EXTWG,
                     IT_ZSQM_IS_R_ISP-RESPP TO WA_RESPP.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          READ TABLE IT_ZSQM_IS_R_REG  INDEX LW_SEL_INDEX.
          MOVE : IT_ZSQM_IS_R_REG-EXTWG TO WA_EXTWG,
* changed by 100565 request by Mr Moon 7/19/04 - Start
                 IT_ZSQM_IS_R_REG-RESPP TO WA_RESPP.
* changed by 100565 request by Mr Moon 7/19/04 - End
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.


  LOOP AT IT_ZSQM_IS_R_LIST WHERE EXTWG = WA_EXTWG AND
                                  RESPP = WA_RESPP.
    CLEAR IT_ZSQM_IS_R_LIST_D.
    MOVE-CORRESPONDING IT_ZSQM_IS_R_LIST
                           TO IT_ZSQM_IS_R_LIST_D.
    APPEND IT_ZSQM_IS_R_LIST_D.
  ENDLOOP.

  SORT IT_ZSQM_IS_R_LIST_D BY MATNR ASCENDING.

ENDFORM.                    " RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
*&      Form  SET_INSPECTION_TYPE
*&------------------------------------------------------------------*
FORM SET_INSPECTION_TYPE.
  CASE C_MARK.
    WHEN ST_DIST-ISIR.
      ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_ISIR.
    WHEN ST_DIST-REGU.
      ZSQM_IS_RES_SEL-ART = C_INSP_TYPE_REGULAR.
  ENDCASE.

  CLEAR ZSQM_IS_RES_SEL-KURZTEXT.

  SELECT SINGLE KURZTEXT INTO ZSQM_IS_RES_SEL-KURZTEXT
     FROM TQ30T
       WHERE SPRACHE = SY-LANGU
         AND ART     = ZSQM_IS_RES_SEL-ART.

ENDFORM.                    " SET_INSPECTION_TYPE
*&---------------------------------------------------------------------*
*&      Form  SET_PLAT_DATA_BY_CODE_G
*&---------------------------------------------------------------------*
FORM SET_PLAT_DATA_BY_CODE_G.
  CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
    WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
      ZSQM_IS_RES_SEL-WERKS = C_PLANT_ENGINE.
    WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
      ZSQM_IS_RES_SEL-WERKS = C_PLANT_VEHICLE.
  ENDCASE.

  CLEAR ZSQM_IS_RES_SEL-NAME1.

  SELECT SINGLE NAME1 INTO ZSQM_IS_RES_SEL-NAME1
    FROM T001W
     WHERE WERKS = ZSQM_IS_RES_SEL-WERKS.

ENDFORM.                    " SET_PLAT_DATA_BY_CODE_G
*&-----------------------------------------------------------------*
*&      Form  COUNT_DATS_FIELD
*&------------------------------------------------------------------*
FORM COUNT_DATS_FIELD USING    PW_FLD_N_IND.
  DATA : LW_FLD_DAT_NAME(40) TYPE C,
         LW_FLD_CNT_NAME(40) TYPE C.

  FIELD-SYMBOLS : <LW_FS_DAT>,
                  <LW_FS_CNT>.
*      - count Start/end date of MIC
  CONCATENATE 'IT_ZSQM_IS_R_LIST-DATUV_'  PW_FLD_N_IND
                                            INTO LW_FLD_DAT_NAME.
  ASSIGN (LW_FLD_DAT_NAME) TO <LW_FS_DAT>.

  IF NOT <LW_FS_DAT> IS INITIAL.
    CASE ZSQM_IS_RES_SEL-ART.
      WHEN C_INSP_TYPE_ISIR.
        CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
          WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
            CONCATENATE 'IT_ZSQM_IS_R_ISE-DATUV_' PW_FLD_N_IND
                                              INTO LW_FLD_CNT_NAME.
          WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
            CONCATENATE 'IT_ZSQM_IS_R_ISP-DATUV_' PW_FLD_N_IND
                                              INTO LW_FLD_CNT_NAME.
        ENDCASE.
      WHEN C_INSP_TYPE_REGULAR.
        CONCATENATE 'IT_ZSQM_IS_R_REG-DATUV_' PW_FLD_N_IND
                                          INTO LW_FLD_CNT_NAME.
    ENDCASE.

    ASSIGN (LW_FLD_CNT_NAME) TO <LW_FS_CNT>.
    MOVE : 1 TO <LW_FS_CNT>.
  ENDIF.

*      - count End date of MIC
  CONCATENATE 'IT_ZSQM_IS_R_LIST-DATUB_'  PW_FLD_N_IND
                                            INTO LW_FLD_DAT_NAME.
  ASSIGN (LW_FLD_DAT_NAME) TO <LW_FS_DAT>.

  IF NOT <LW_FS_DAT> IS INITIAL.
    CASE ZSQM_IS_RES_SEL-ART.
      WHEN C_INSP_TYPE_ISIR.
        CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
          WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
            CONCATENATE 'IT_ZSQM_IS_R_ISE-DATUB_' PW_FLD_N_IND
                                              INTO LW_FLD_CNT_NAME.
          WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
            CONCATENATE 'IT_ZSQM_IS_R_ISP-DATUB_' PW_FLD_N_IND
                                              INTO LW_FLD_CNT_NAME.
        ENDCASE.
      WHEN C_INSP_TYPE_REGULAR.
        CONCATENATE 'IT_ZSQM_IS_R_REG-DATUB_' PW_FLD_N_IND
                                          INTO LW_FLD_CNT_NAME.
    ENDCASE.

    ASSIGN (LW_FLD_CNT_NAME) TO <LW_FS_CNT>.
    MOVE : 1 TO <LW_FS_CNT>.
  ENDIF.

ENDFORM.                    " COUNT_DATS_FIELD
*&------------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_FOR_ISIR_P
*&------------------------------------------------------------------*
FORM SET_FIELD_ATTR_FOR_ISIR_P TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  DATA : BEGIN OF LT_QPMK OCCURS 30,
          MKMNR      TYPE  QMERKNR,
          KURZTEXT   TYPE  QKURZTEXT,
         END OF LT_QPMK.
  	
    DATA : LW_MKMNR TYPE QMERKNR.

  DATA : LW_COL_TEXT TYPE LVC_TXTCOL. "/Temporary Column Text

*  DATA : LW_FLD_NAME(40) TYPEC.
*  FIELD-SYMBOLS : <LW_FS>.

  DATA : LW_FLD_N_IND(4) TYPE N,
         LW_FLD_N_DIV(3) TYPE N,
         LW_MIC_SER(2)   TYPE N.



*--  Get MIC Master text list
  SELECT A~MKMNR B~KURZTEXT  INTO CORRESPONDING FIELDS OF TABLE LT_QPMK
                        FROM QPMK AS  A INNER JOIN QPMT AS B
                           ON   A~ZAEHLER  = B~ZAEHLER
                            AND A~MKMNR    = B~MKMNR
                            AND A~VERSION  = B~VERSION
                          WHERE A~ZAEHLER = ZSQM_IS_RES_SEL-WERKS
*                            AND A~VERSION = '000001'
                            AND A~LOEKZ   = '2'
                            AND B~SPRACHE = SY-LANGU
                            AND B~GELOESCHT = ' '
                            AND A~VERSION =
                                    ( SELECT MAX( VERSION )
                                        FROM QPMK
                                         WHERE ZAEHLER = A~ZAEHLER
                                           AND MKMNR   = A~MKMNR ).

  CHECK SY-SUBRC = 0.

* Set field attribute
  LOOP AT PT_FIELDCAT.
    IF PT_FIELDCAT-FIELDNAME = 'EXTWG'.
      PT_FIELDCAT-KEY_SEL = C_MARK.
      PT_FIELDCAT-KEY     = C_MARK.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR_C'.
      MOVE  'Material'(TT4)  TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
      PT_FIELDCAT-EMPHASIZE = 'C410'.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'PLOS_C'.
      MOVE  'Inspection Lot'(TT5)  TO : PT_FIELDCAT-COLTEXT,
                                        PT_FIELDCAT-SCRTEXT_L,
                                        PT_FIELDCAT-SCRTEXT_M,
                                        PT_FIELDCAT-SCRTEXT_S.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'PLOS_MS_C'.
      MOVE  'Inspection Lot(MS)'(TT6)  TO : PT_FIELDCAT-COLTEXT,
                                            PT_FIELDCAT-SCRTEXT_L,
                                            PT_FIELDCAT-SCRTEXT_M,
                                            PT_FIELDCAT-SCRTEXT_S.
      PT_FIELDCAT-EMPHASIZE = C_MARK.

    ENDIF.

    IF PT_FIELDCAT-DATATYPE = 'INT4'.
      PT_FIELDCAT-DO_SUM = C_MARK.
    ENDIF.

*-- ISIR/REGULAR
    IF PT_FIELDCAT-FIELDNAME+0(4) = 'DATU' AND
       PT_FIELDCAT-FIELDNAME+6(1) = '0'.

      IF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT-EMPHASIZE = 'C300'.
      ELSEIF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT-EMPHASIZE = 'C311'.
      ENDIF.

      LW_FLD_N_IND =  PT_FIELDCAT-FIELDNAME+6(4). "/'DATUV/B_xxxx'
      LW_MIC_SER   =  LW_FLD_N_IND+1(2). "/'0xx0' "/MIC Serial
*      - Start/End date of MIC
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type

              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_ISIR+2(2)      "/Insp. type:'QPxx'
                      '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.

            WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_ISIR+2(2)      "/Insp. type:'QPxx'
                       '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                      C_CODEGRUPPE_REGU+2(2)    "/Insp. type:'QPxx'
                       '_'
                      LW_MIC_SER
                        INTO LW_MKMNR.
      ENDCASE.

*      select Column Text for MIC - 'XXX_nn'
      READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

      IF SY-SUBRC = 0.

*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        MOVE  LW_COL_TEXT TO :      PT_FIELDCAT-COLTEXT,
                                    PT_FIELDCAT-SCRTEXT_L,
                                    PT_FIELDCAT-SCRTEXT_M,
                                    PT_FIELDCAT-SCRTEXT_S.

      ELSE. "- 'XXXnn' "/ for Dev Test system - sllee
        REPLACE '_' WITH ' ' INTO  LW_MKMNR.
        CONDENSE LW_MKMNR NO-GAPS.
        READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        IF SY-SUBRC = 0.
          MOVE  LW_COL_TEXT TO :       PT_FIELDCAT-COLTEXT,
                                      PT_FIELDCAT-SCRTEXT_L,
                                      PT_FIELDCAT-SCRTEXT_M,
                                      PT_FIELDCAT-SCRTEXT_S.
        ENDIF.
      ENDIF.

    ENDIF.

**-- MS
    IF PT_FIELDCAT-FIELDNAME+0(4) = 'DATU' AND
       PT_FIELDCAT-FIELDNAME+6(1) = '1'.

      IF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT-EMPHASIZE = 'C400'.
      ELSEIF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT-EMPHASIZE = 'C411'.
      ENDIF.

      LW_FLD_N_IND =  PT_FIELDCAT-FIELDNAME+6(4). "/'DATUV/B_xxxx'
      LW_MIC_SER   =  LW_FLD_N_IND+1(2). "/'0xx0' "/MIC Serial
*      - Start/End date of MIC
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type

              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_MS+2(2)      "/Insp. type:'QPxx'
                       '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.

            WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
              CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_MS+2(2)      "/Insp. type:'QPxx'
                       '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          CONCATENATE ZSQM_IS_RES_SEL-WERKS+0(1)  "/Plant:'x001'
                      C_CODEGRUPPE_MS+2(2)    "/Insp. type:'QPxx'
                       '_'
                      LW_MIC_SER
                        INTO LW_MKMNR.
      ENDCASE.

*      select Column Text for MIC - 'XXX_nn'
      READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

      IF SY-SUBRC = 0.
*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        MOVE  LW_COL_TEXT  TO :     PT_FIELDCAT-COLTEXT,
                                    PT_FIELDCAT-SCRTEXT_L,
                                    PT_FIELDCAT-SCRTEXT_M,
                                    PT_FIELDCAT-SCRTEXT_S.
      ELSE. "- 'XXXnn' "/ for Dev Test system - sllee
        REPLACE '_' WITH ' ' INTO  LW_MKMNR.
        CONDENSE LW_MKMNR NO-GAPS.
        READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

*-- Make MIC Column Text
        CONCATENATE   LW_FLD_N_IND+1(3)
                      '-'
                      LT_QPMK-KURZTEXT
                                INTO LW_COL_TEXT.

        IF SY-SUBRC = 0.
          MOVE  LW_COL_TEXT TO :      PT_FIELDCAT-COLTEXT,
                                      PT_FIELDCAT-SCRTEXT_L,
                                      PT_FIELDCAT-SCRTEXT_M,
                                      PT_FIELDCAT-SCRTEXT_S.
        ENDIF.
      ENDIF.

    ENDIF.

    MODIFY PT_FIELDCAT.

  ENDLOOP.

ENDFORM.                    " SET_FIELD_ATTR_FOR_ISIR_P
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_COUNT_VAR
*&---------------------------------------------------------------------*
FORM SET_FIELD_COUNT_VAR.

**-- Count material by type and plant
  CASE ZSQM_IS_RES_SEL-ART.
    WHEN C_INSP_TYPE_ISIR.
      CASE ZSQM_IS_RES_SEL-CODEGRP_VH.
        WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
          WA_FLD_ISRE_CNT = C_INSP_ISE_CNT.
          WA_FLD_MS_CNT   = C_INSP_MS_CNT.
        WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
          WA_FLD_ISRE_CNT = C_INSP_ISP_CNT.
          WA_FLD_MS_CNT   = C_INSP_MS_CNT.
      ENDCASE.
    WHEN C_INSP_TYPE_REGULAR.
      WA_FLD_ISRE_CNT = C_INSP_REG_CNT.
      WA_FLD_MS_CNT   = C_INSP_MS_CNT.
  ENDCASE.

ENDFORM.                    " SET_FIELD_COUNT_VAR
*&-----------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_FOR_DETAIL
*&-----------------------------------------------------------------*
FORM SET_FIELD_ATTR_FOR_DETAIL TABLES  PT_FIELDCAT   TYPE LVC_T_FCAT
                                       PT_FIELDCAT_D TYPE LVC_T_FCAT.

  LOOP AT PT_FIELDCAT_D.
    IF PT_FIELDCAT_D-FIELDNAME = 'EXTWG'.
      PT_FIELDCAT_D-NO_OUT = C_MARK.
    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'MATNR' OR
           PT_FIELDCAT_D-FIELDNAME = 'MAKTX' OR
           PT_FIELDCAT_D-FIELDNAME = 'CODE_VH' OR
           PT_FIELDCAT_D-FIELDNAME = 'LIFNR'   OR
           PT_FIELDCAT_D-FIELDNAME = 'EONO'.
      PT_FIELDCAT_D-KEY_SEL = C_MARK.
      PT_FIELDCAT_D-KEY     = C_MARK.
      PT_FIELDCAT_D-EMPHASIZE = 'C410'.

      IF PT_FIELDCAT_D-FIELDNAME = 'EONO'.
        CASE ZSQM_IS_RES_SEL-ART.
          WHEN C_INSP_TYPE_ISIR.
*            PT_FIELDCAT_D-EMPHASIZE = C_MARK.
            PT_FIELDCAT_D-NO_OUT = C_MARK.
          WHEN C_INSP_TYPE_REGULAR.
            CLEAR PT_FIELDCAT_D-KEY_SEL.
            CLEAR PT_FIELDCAT_D-KEY.
            PT_FIELDCAT_D-NO_OUT = C_MARK.
        ENDCASE.
      ENDIF.

    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'RESPP'.
      CASE ZSQM_IS_RES_SEL-ART.
        WHEN C_INSP_TYPE_ISIR.
*            PT_FIELDCAT_D-EMPHASIZE = C_MARK.
*            PT_FIELDCAT_D-NO_OUT = C_MARK.
        WHEN C_INSP_TYPE_REGULAR.
* changed by 100565 request by Mr Moon 7/19/04 - Start
*Commented out the following code
*          PT_FIELDCAT_D-NO_OUT = C_MARK.
* changed by 100565 request by Mr Moon 7/19/04 - End
      ENDCASE.


    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'I_STAT'.
      PT_FIELDCAT_D-OUTPUTLEN = 3.
      PT_FIELDCAT_D-EMPHASIZE = 'C210'.

    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'PRUEFLOS'.
      MOVE  'Inspection Lot'(TT5)  TO : PT_FIELDCAT_D-COLTEXT,
                                        PT_FIELDCAT_D-SCRTEXT_L,
                                        PT_FIELDCAT_D-SCRTEXT_M,
                                        PT_FIELDCAT_D-SCRTEXT_S.
    ELSEIF PT_FIELDCAT_D-FIELDNAME = 'PRUEFLOS_MS'.
      MOVE  'Inspection Lot(MS)'(TT6)  TO : PT_FIELDCAT_D-COLTEXT,
                                            PT_FIELDCAT_D-SCRTEXT_L,
                                            PT_FIELDCAT_D-SCRTEXT_M,
                                            PT_FIELDCAT_D-SCRTEXT_S.
      PT_FIELDCAT_D-EMPHASIZE = C_MARK.

    ENDIF.

*-- ISIR/REGULAR
    IF PT_FIELDCAT_D-FIELDNAME+0(4) = 'DATU'.
      READ TABLE PT_FIELDCAT  WITH KEY
                                FIELDNAME = PT_FIELDCAT_D-FIELDNAME.
      IF SY-SUBRC NE 0.
        PT_FIELDCAT_D-NO_OUT = C_MARK.
      ELSE.
        MOVE : PT_FIELDCAT-COLTEXT    TO PT_FIELDCAT_D-COLTEXT,
               PT_FIELDCAT-SCRTEXT_L  TO PT_FIELDCAT_D-SCRTEXT_L,
               PT_FIELDCAT-SCRTEXT_M  TO PT_FIELDCAT_D-SCRTEXT_M,
               PT_FIELDCAT-SCRTEXT_S  TO PT_FIELDCAT_D-SCRTEXT_S.
      ENDIF.

      IF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT_D-EMPHASIZE = 'C300'.
      ELSEIF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT_D-EMPHASIZE = 'C311'.
      ENDIF.

      IF PT_FIELDCAT_D-FIELDNAME+6(1) = '1'.
        IF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUV'.
          PT_FIELDCAT_D-EMPHASIZE = 'C400'.
        ELSEIF PT_FIELDCAT_D-FIELDNAME+0(5) = 'DATUB'.
          PT_FIELDCAT_D-EMPHASIZE = 'C411'.
        ENDIF.
      ENDIF.

    ENDIF.

    MODIFY PT_FIELDCAT_D.

  ENDLOOP.

ENDFORM.                    " SET_FIELD_ATTR_FOR_DETAIL
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_D_FOR_INTERFACE
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_D_FOR_INTERFACE.

  REFRESH IT_ZSQM_IS_R_LIST_D.

  LOOP AT IT_ZSQM_IS_R_LIST.
    CLEAR IT_ZSQM_IS_R_LIST_D.
    MOVE-CORRESPONDING IT_ZSQM_IS_R_LIST
                           TO IT_ZSQM_IS_R_LIST_D.
    APPEND IT_ZSQM_IS_R_LIST_D.
  ENDLOOP.

  SORT IT_ZSQM_IS_R_LIST_D BY MATNR ASCENDING.

ENDFORM.                    " RETRIEV_DETAIL_D_FOR_INTERFACE
*&---------------------------------------------------------------------*
*&      Form  MASK_COLUMNS_FOR_INTERFACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_D  text
*----------------------------------------------------------------------*
FORM MASK_COLUMNS_FOR_INTERFACE TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.
  CLEAR : WA_STRUCTURE_NAME.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

  WA_STRUCTURE_NAME = 'ZSQM_IS_R_LIST'.



* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = WA_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

ENDFORM.                    " MASK_COLUMNS_FOR_INTERFACE
*&------------------------------------------------------------------*
*&      Form  GET_DATA_AND_LIST_BASIC
*&------------------------------------------------------------------*
FORM GET_DATA_AND_LIST_BASIC.
*    - Get data from DB
  PERFORM GET_DATA_FROM_DB.

  CHECK NOT IT_ZSQM_IS_R_LIST[] IS INITIAL.

*    - Set field count
  PERFORM SET_FIELD_COUNT_VAR.

  PERFORM COUNT_UP_DATA_FOR_BASIC.

ENDFORM.                    " GET_DATA_AND_LIST_BASIC
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA_REFRESH
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA_REFRESH.

  REFRESH IT_ZSQM_IS_R_LIST_D.

  LOOP AT IT_ZSQM_IS_R_LIST WHERE EXTWG = WA_EXTWG.
    CLEAR IT_ZSQM_IS_R_LIST_D.
    MOVE-CORRESPONDING IT_ZSQM_IS_R_LIST
                           TO IT_ZSQM_IS_R_LIST_D.
    APPEND IT_ZSQM_IS_R_LIST_D.
  ENDLOOP.

  CHECK SY-SUBRC = 0.

  SORT IT_ZSQM_IS_R_LIST_D BY MATNR ASCENDING.

ENDFORM.                    " RETRIEV_DETAIL_DATA_REFRESH
