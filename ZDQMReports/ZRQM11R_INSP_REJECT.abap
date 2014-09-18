************************************************************************
* Program Name      : ZRQM11R_INSP_REJECT
* Author            : SeungLyong, Lee
* Creation Date     : 2003.11.18.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No :
* Addl Documentation:
* Description       : Inspection Reject reports
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZRQM11R_INSP_REJECT  NO STANDARD PAGE HEADING.

*&&& Data Declaration.  &&&*
TYPE-POOLS ZQMT1.  "/QM-Type group for inspection
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool

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
         MARC.

*//Structures Declaration(TABLES : Structure Name."/Description)
*TABLES : ZSCA_TIME_STAMP.   "/Time Stamp Structre.
TABLES : ZSQM_INSP_SEL. "/Inspection Status -Selection screen Structure
TABLES : ZSQM_INSP_REJ, "/Inspection Reject data list str.
         ZSQM_INSP_REJ_B, "/Inspection Reject basic str.
         ZSQM_INSP_REJ_D. "/Inspection Reject Detail str.

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

*-- Control flag for interface with other program
DATA : WA_IMPORT_EXIST.

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
         REGU   TYPE C  VALUE C_MARK,
         ISIR   TYPE C,
       END OF ST_DIST.

*-- List Box Set Flag
DATA : WA_LISTBOX_SET.

*-- Error check flag
DATA : WA_ERROR_FOUNDED.

*-- Inspection date
DATA : WA_FIRSTDAY  TYPE QPRSTART, "/Inspection Start Date
       WA_LASTDAY   TYPE QPRENDE.  "/End Date of the Inspection

*- Selected Ext. material group
DATA : WA_EXTWG  TYPE EXTWG.

*-- Report Level control
DATA : WA_LEVEL(10) TYPE C.

*//Ranges; (R_)
RANGES : R_WERKS       FOR ZSQM_INSP_SEL-WERKS,
         R_ART         FOR ZSQM_INSP_SEL-ART,
         R_IYEAR       FOR ZSQM_INSP_SEL-IYEAR,
         R_LIFNR       FOR ZSQM_INSP_SEL-LIFNR,
         R_CODEGRP_VH  FOR ZSQM_INSP_SEL-CODEGRP_VH,
         R_CODE_VH     FOR ZSQM_INSP_SEL-CODE_VH,
         R_EXTWG       FOR ZSQM_INSP_SEL-EXTWG,
         R_UD_CODE     FOR QAVE-VCODE,
         R_PASTR       FOR QALS-PASTRTERM,
         R_PAEND       FOR QALS-PAENDTERM.


*//Internal Tables and Index Fields;(IT_), (I_)
DATA : IT_ZSQM_INSP_REJ  LIKE ZSQM_INSP_REJ OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSQM_INSP_REJ_B  LIKE ZSQM_INSP_REJ_B OCCURS 0
                                                     WITH HEADER LINE.
DATA : IT_ZSQM_INSP_REJ_D  LIKE ZSQM_INSP_REJ_D OCCURS 0
                                                     WITH HEADER LINE.

**-- Vehicle/Engine type master temp table for Text input
*DATA : begin of IT_VEHENG OCCURS 0,
*        CODEGRP_VH    TYPE ZQCODEGRP_VH,
*        CODE_VH       TYPE ZQCODE_VH,
*        KURZTEXT_VH   TYPE QTXT_CODE,
*       END OF IT_VEHENG.

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


* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED. "/ALV Event Handling

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

*/// ALV Control....: End


****//& Selection Screen Definition(Parameters Select-Option)
**-- Paramerters : (P_), Select-Options : (S_)
*SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.

*SELECTION-SCREEN END OF BLOCK BLK .
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
  ZSQM_INSP_SEL-KATART_VH = C_VE_ENG_CAT_TYPE.

*- Set default date for inspection start date
  CONCATENATE SY-DATUM+0(4)
              '0101'    INTO ZSQM_INSP_SEL-PASTR_L.
  MOVE : SY-DATUM TO ZSQM_INSP_SEL-PASTR_H.


*-- Get Selection data for Selection from Memory ID using IMPORT
  IMPORT ZSQM_INSP_SEL  FROM MEMORY ID 'ZREJECT'.

  IF SY-SUBRC = 0.  "/Import data exist.
    WA_IMPORT_EXIST = C_MARK.

    FREE MEMORY ID 'ZREJECT'.
    MOVE : ZSQM_INSP_SEL-EXTWG TO WA_EXTWG.
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
*         append a separator('3') to normal toolbar
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to show detail List of selected item.
        CLEAR LS_TOOLBAR.
        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
        MOVE 'UD_CHANGE'        TO LS_TOOLBAR-FUNCTION.
        MOVE ICON_CHANGE        TO LS_TOOLBAR-ICON.
        MOVE 'Usage decision'(T14) TO LS_TOOLBAR-QUICKINFO.
        MOVE TEXT-T14      TO LS_TOOLBAR-TEXT.
        MOVE ' '                TO LS_TOOLBAR-DISABLED.

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
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

        CHECK NOT IT_ZSQM_INSP_REJ_D[] IS INITIAL.

        WA_LEVEL = C_DETAIL.

        WA_RENEWAL_FLG = C_MARK.

      WHEN C_DETAIL.
        CHECK NOT E_ROW-INDEX IS INITIAL.

        PERFORM PROC_CHAGE_USAGE_DECISION USING E_ROW-INDEX.

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

          CHECK NOT IT_ZSQM_INSP_REJ_D[] IS INITIAL.

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

          PERFORM PROC_CHAGE_USAGE_DECISION USING LW_LINE_ROW-INDEX.

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

  IF     ZSQM_INSP_SEL-CODEGRP_VH IS INITIAL AND
        NOT ZSQM_INSP_SEL-CODE_VH    IS INITIAL.

    MESSAGE E000(ZMQM)
        WITH 'Please Input Vehicle/Engine Code Group.'(E01).

  ENDIF.

  CHECK NOT ZSQM_INSP_SEL-CODEGRP_VH IS INITIAL AND
        NOT ZSQM_INSP_SEL-CODE_VH    IS INITIAL.


  SELECT SINGLE * INTO LW_ZVQM_VEHICLE
       FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_INSP_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_INSP_SEL-CODEGRP_VH
           AND CODE       = ZSQM_INSP_SEL-CODE_VH.

  CHECK SY-SUBRC NE 0.

  MESSAGE E000(ZMQM) WITH
     'Vehicle/Engine type  code group and code are not matched'(E02).

ENDMODULE.                 " CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
*&      Module  SET_LISTBOX_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE SET_LISTBOX_9000 OUTPUT.
  CHECK WA_LISTBOX_SET IS INITIAL.
  WA_LISTBOX_SET = C_MARK.

*- Set List for Selection screen : Inspectio type
  PERFORM SET_LISTBOX_FOR_QPART   USING 'ZSQM_INSP_SEL-ART'.

*- Set List for Selection screen : Inspectio UD Code
  PERFORM SET_LISTBOX_FOR_UDCODE  USING 'ZSQM_INSP_SEL-UD_SEL'.

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

  CLEAR : ZSQM_INSP_SEL-NAME1,
          ZSQM_INSP_SEL-NAME_LF,
          ZSQM_INSP_SEL-KURZTEXT_G,
          ZSQM_INSP_SEL-KURZTEXT_VH,
          ZSQM_INSP_SEL-EWBEZ.

  IF NOT ZSQM_INSP_SEL-WERKS IS INITIAL.
    SELECT SINGLE NAME1 INTO ZSQM_INSP_SEL-NAME1
        FROM T001W
          WHERE WERKS = ZSQM_INSP_SEL-WERKS.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_INSP_SEL-WERKS
                              'is not exist!'(E10).
    ENDIF.

  ENDIF.


  IF NOT ZSQM_INSP_SEL-LIFNR IS INITIAL.
    SELECT SINGLE NAME1  INTO ZSQM_INSP_SEL-NAME_LF
      FROM LFA1
        WHERE LIFNR = ZSQM_INSP_SEL-LIFNR.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_INSP_SEL-LIFNR
                              TEXT-E10.
    ENDIF.
  ENDIF.

  IF NOT ZSQM_INSP_SEL-CODEGRP_VH IS INITIAL.
    SELECT SINGLE KURZTEXT INTO ZSQM_INSP_SEL-KURZTEXT_G
      FROM ZVQM_QPGRT01
         WHERE KATALOGART = ZSQM_INSP_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_INSP_SEL-CODEGRP_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_INSP_SEL-CODEGRP_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_INSP_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_INSP_SEL-CODE_VH    IS INITIAL.
    SELECT SINGLE KURZTEXT_C INTO ZSQM_INSP_SEL-KURZTEXT_VH
      FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_INSP_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_INSP_SEL-CODEGRP_VH
           AND CODE       = ZSQM_INSP_SEL-CODE_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_INSP_SEL-CODEGRP_VH
                              ','
                              ZSQM_INSP_SEL-CODE_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_INSP_SEL-EXTWG IS INITIAL.
    SELECT SINGLE EWBEZ   INTO ZSQM_INSP_SEL-EWBEZ
      FROM TWEWT
        WHERE SPRAS = SY-LANGU
          AND EXTWG = ZSQM_INSP_SEL-EXTWG.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_INSP_SEL-EXTWG
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
      PERFORM CHECK_REQUIRED_FIELD  USING WA_ERROR_FOUNDED.
      CHECK WA_ERROR_FOUNDED IS INITIAL.
      PERFORM SET_RANGE_VALUE.

*-     Get data and Count for Basic List.
      PERFORM GET_DATA_AND_COUNT_BASIC.

      CHECK NOT IT_ZSQM_INSP_REJ_B[] IS INITIAL.

      IF WA_IMPORT_EXIST = ' '. "/Normal
        WA_LEVEL = C_BASIC.
        CALL SCREEN 9100.

      ELSE. "/Interface
        WA_LEVEL = C_DETAIL.
        PERFORM RETRIEV_DETAIL_D_FOR_INTERFACE .

        CHECK NOT IT_ZSQM_INSP_REJ_D[] IS INITIAL.

        CALL SCREEN 9100.

      ENDIF.


    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
*&      Module  CONTROL_INSP_YEAR_BY_ART  INPUT
*&------------------------------------------------------------------*
MODULE CONTROL_INSP_YEAR_BY_ART INPUT.

  CASE ZSQM_INSP_SEL-ART.
    WHEN C_INSP_TYPE_ISIR.
      CLEAR ZSQM_INSP_SEL-IYEAR.
    WHEN C_INSP_TYPE_REGULAR.

    WHEN  C_INSP_TYPE_MS.
      CLEAR ZSQM_INSP_SEL-IYEAR.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " CONTROL_INSP_YEAR_BY_ART  INPUT
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.

  CASE ZSQM_INSP_SEL-ART.
    WHEN C_INSP_TYPE_ISIR.

    WHEN C_INSP_TYPE_REGULAR.

    WHEN  C_INSP_TYPE_MS.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&-----------------------------------------------------------------*
*&      Form  CHECK_REQUIRED_FIELD
*&-----------------------------------------------------------------*
FORM CHECK_REQUIRED_FIELD USING    P_ERROR_FOUNDED.
*----- 01.20.2004 Marked up by BSBAE
*  CLEAR P_ERROR_FOUNDED.
*  IF ZSQM_INSP_SEL-ART = C_INSP_TYPE_REGULAR AND
*     ZSQM_INSP_SEL-IYEAR IS INITIAL.
*    P_ERROR_FOUNDED = C_MARK.
*    MESSAGE E000(ZMQM) WITH 'Inspection year must be inputed.'(E11).
*    EXIT.
*  ENDIF.
*----- 01.20.2004 Marked up by BSBAE
ENDFORM.                    " CHECK_REQUIRED_FIELD
*&-----------------------------------------------------------------*
*&      Form  SET_RANGE_VALUE
*&-----------------------------------------------------------------*
FORM SET_RANGE_VALUE.

  REFRESH : R_WERKS, R_ART, R_IYEAR, R_LIFNR, R_CODEGRP_VH,
            R_CODE_VH, R_EXTWG, R_UD_CODE, R_PASTR, R_PAEND.
  CLEAR   : R_WERKS, R_ART, R_IYEAR, R_LIFNR, R_CODEGRP_VH,
            R_CODE_VH, R_EXTWG, R_UD_CODE, R_PASTR, R_PAEND.

  RANGE_MACRO : R_WERKS       ZSQM_INSP_SEL-WERKS       '',
*                R_ART         ZSQM_INSP_SEL-ART         '',
*                R_IYEAR       ZSQM_INSP_SEL-IYEAR       '',
                R_LIFNR       ZSQM_INSP_SEL-LIFNR       '',
                R_CODEGRP_VH  ZSQM_INSP_SEL-CODEGRP_VH  '',
                R_CODE_VH     ZSQM_INSP_SEL-CODE_VH     '',
                R_EXTWG       ZSQM_INSP_SEL-EXTWG       '',
                R_PASTR    ZSQM_INSP_SEL-PASTR_L  ZSQM_INSP_SEL-PASTR_H,
                R_PAEND    ZSQM_INSP_SEL-PAEND_L  ZSQM_INSP_SEL-PAEND_H.

  IF ZSQM_INSP_SEL-ART IS INITIAL.
    RANGE_MACRO : R_ART     C_INSP_TYPE_ISIR       '',
                  R_ART     C_INSP_TYPE_REGULAR    '',
                  R_ART     C_INSP_TYPE_MS         ''.
  ELSE.
    RANGE_MACRO : R_ART         ZSQM_INSP_SEL-ART         ''.
  ENDIF.

  IF ZSQM_INSP_SEL-UD_SEL IS INITIAL.
    RANGE_MACRO : R_UD_CODE    C_UD_CODE_REJECT    '',
                  R_UD_CODE    C_UD_CODE_MD        ''.
  ELSE.
    RANGE_MACRO : R_UD_CODE    ZSQM_INSP_SEL-UD_SEL      ''.
  ENDIF.

ENDFORM.                    " SET_RANGE_VALUE
*&------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&------------------------------------------------------------------*
FORM GET_DATA_FROM_DB.



  SELECT A~WERK AS WERKS   A~CODEGRP_VH A~CODE_VH A~ART
         C~EXTWG A~MATNR   A~PRUEFLOS B~VCODE
         D~KURZTEXT AS KURZTEXT_VH
         E~MAKTX
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_INSP_REJ
      FROM ( ( ( QALS AS A  INNER JOIN QAVE AS B
         ON   A~PRUEFLOS   = B~PRUEFLOS ) INNER JOIN MARA AS C
         ON   A~MATNR      = C~MATNR    ) INNER JOIN QPCT AS D
         ON   A~KATART_VH  = D~KATALOGART
          AND A~CODEGRP_VH = D~CODEGRUPPE
          AND A~CODE_VH    = D~CODE      ) INNER JOIN MAKT AS E
         ON   C~MATNR      = E~MATNR
       WHERE  A~ART        IN R_ART
          AND A~WERK       IN R_WERKS
          AND A~SELLIFNR   IN R_LIFNR
          AND A~KATART_VH  = C_VE_ENG_CAT_TYPE
          AND A~CODEGRP_VH IN R_CODEGRP_VH
          AND A~CODE_VH    IN R_CODE_VH
          AND C~EXTWG      IN R_EXTWG
          AND B~VKATART    = C_UD_CODE_KATART
          AND B~VCODEGRP   = C_UD_CODE_GRUPPE
          AND B~VCODE      IN R_UD_CODE
          AND A~PASTRTERM  IN R_PASTR
          AND A~PAENDTERM  IN R_PAEND
          AND D~SPRACHE    = SY-LANGU
          AND D~INAKTIV    = ' '
          AND D~GELOESCHT  = ' '
          AND E~SPRAS      = SY-LANGU.

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
  REFRESH IT_ZSQM_INSP_REJ_B.

*-- Count material by UD Code
  LOOP AT IT_ZSQM_INSP_REJ.
    CLEAR IT_ZSQM_INSP_REJ_B.
    MOVE-CORRESPONDING IT_ZSQM_INSP_REJ TO IT_ZSQM_INSP_REJ_B.

    CASE IT_ZSQM_INSP_REJ-VCODE.
      WHEN C_UD_CODE_REJECT.
        IT_ZSQM_INSP_REJ_B-REJEC_C = 1.
      WHEN C_UD_CODE_MD.
        IT_ZSQM_INSP_REJ_B-PR_MD_C = 1.
    ENDCASE.

    COLLECT IT_ZSQM_INSP_REJ_B.
  ENDLOOP.

  SORT IT_ZSQM_INSP_REJ_B BY  WERKS CODEGRP_VH CODE_VH ART
                              EXTWG                        ASCENDING.

ENDFORM.                    " COUNT_UP_DATA_FOR_BASIC
*&-------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&-------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
  SET TITLEBAR  '9100'.

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
            LEAVE TO SCREEN 0.
            PERFORM SET_LIST_LEVEL_CONTROL.
          WHEN OTHERS.
            PERFORM SET_LIST_LEVEL_CONTROL.
        ENDCASE.
      ELSE.
        LEAVE PROGRAM.
      ENDIF.

    WHEN 'REFRESH'.

*-     Get data and Count for Basic List.
      PERFORM GET_DATA_AND_COUNT_BASIC.

      IF  WA_LEVEL = C_DETAIL.
        PERFORM RETRIEV_DETAIL_D_FOR_INTERFACE .
      ENDIF.

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
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

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

  DATA : LW_STRUCTURE_NAME LIKE DD02L-TABNAME.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

  IF WA_IMPORT_EXIST = ' '.
    CASE WA_LEVEL.
      WHEN C_BASIC.
        LW_STRUCTURE_NAME = 'ZSQM_INSP_REJ_B'.
      WHEN C_DETAIL.
        LW_STRUCTURE_NAME = 'ZSQM_INSP_REJ_D'.
    ENDCASE.
  ELSE.
    LW_STRUCTURE_NAME = 'ZSQM_INSP_REJ_D'.
  ENDIF.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = LW_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

* Set field attribute
  IF WA_IMPORT_EXIST = ' '.
    CASE WA_LEVEL.
      WHEN C_BASIC.
        LOOP AT PT_FIELDCAT.
          IF PT_FIELDCAT-FIELDNAME = 'WERKS' OR
             PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH' OR
             PT_FIELDCAT-FIELDNAME = 'ART' OR
             PT_FIELDCAT-FIELDNAME = 'EXTWG'.
            PT_FIELDCAT-KEY_SEL = C_MARK.
            PT_FIELDCAT-KEY     = C_MARK.
            PT_FIELDCAT-OUTPUTLEN = 12.
            IF  PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH'.
              PT_FIELDCAT-COLTEXT = 'Vehicle/Engine type'(TT3).
              PT_FIELDCAT-OUTPUTLEN = 30.
            ELSEIF PT_FIELDCAT-FIELDNAME = 'WERKS'.
              PT_FIELDCAT-OUTPUTLEN = 7.
            ENDIF.
          ELSEIF PT_FIELDCAT-FIELDNAME = 'CODEGRP_VH' OR
                 PT_FIELDCAT-FIELDNAME = 'CODE_VH'.
            PT_FIELDCAT-NO_OUT = C_MARK.
          ELSEIF  PT_FIELDCAT-FIELDNAME = 'REJEC_C'.
            PT_FIELDCAT-COLTEXT = 'Reject'(TT1).
            PT_FIELDCAT-DO_SUM = C_MARK.
          ELSEIF  PT_FIELDCAT-FIELDNAME = 'PR_MD_C'.
            PT_FIELDCAT-COLTEXT = 'MD'(TT2).
            PT_FIELDCAT-DO_SUM = C_MARK.
          ENDIF.

          MODIFY PT_FIELDCAT.

        ENDLOOP.

      WHEN C_DETAIL.
        LOOP AT PT_FIELDCAT.
          IF PT_FIELDCAT-FIELDNAME = 'WERKS' OR
             PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH' OR
             PT_FIELDCAT-FIELDNAME = 'ART' OR
             PT_FIELDCAT-FIELDNAME = 'EXTWG' OR
             PT_FIELDCAT-FIELDNAME = 'MATNR'.
            PT_FIELDCAT-KEY_SEL = C_MARK.
            PT_FIELDCAT-KEY     = C_MARK.

            IF  PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH'.
              PT_FIELDCAT-COLTEXT = 'Vehicle/Engine type'(TT3).
              PT_FIELDCAT-OUTPUTLEN = 30.
            ELSEIF PT_FIELDCAT-FIELDNAME = 'WERKS'.
              PT_FIELDCAT-OUTPUTLEN = 7.
            ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR'.
              PT_FIELDCAT-OUTPUTLEN = 20.
            ENDIF.
          ELSEIF  PT_FIELDCAT-FIELDNAME = 'REJECT'.
            PT_FIELDCAT-COLTEXT = 'Reject'(TT1).
            PT_FIELDCAT-DO_SUM = C_MARK.
            PT_FIELDCAT-CHECKBOX = C_MARK.
            PT_FIELDCAT-OUTPUTLEN = 7.
          ELSEIF  PT_FIELDCAT-FIELDNAME = 'PR_MD'.
            PT_FIELDCAT-COLTEXT = 'In process MD'(TT2).
            PT_FIELDCAT-DO_SUM = C_MARK.
            PT_FIELDCAT-CHECKBOX = C_MARK.
            PT_FIELDCAT-OUTPUTLEN = 13.
          ENDIF.

          MODIFY PT_FIELDCAT.

        ENDLOOP.
    ENDCASE.

  ELSE.
    LOOP AT PT_FIELDCAT.
      IF PT_FIELDCAT-FIELDNAME = 'WERKS' OR
         PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH' OR
         PT_FIELDCAT-FIELDNAME = 'ART' OR
         PT_FIELDCAT-FIELDNAME = 'EXTWG' OR
         PT_FIELDCAT-FIELDNAME = 'MATNR'.
        PT_FIELDCAT-KEY_SEL = C_MARK.
        PT_FIELDCAT-KEY     = C_MARK.

        IF  PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH'.
          PT_FIELDCAT-COLTEXT = 'Vehicle/Engine type'(TT3).
          PT_FIELDCAT-OUTPUTLEN = 30.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'WERKS'.
          PT_FIELDCAT-OUTPUTLEN = 7.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR'.
          PT_FIELDCAT-OUTPUTLEN = 20.
        ENDIF.
      ELSEIF  PT_FIELDCAT-FIELDNAME = 'REJECT'.
        PT_FIELDCAT-COLTEXT = 'Reject'(TT1).
        PT_FIELDCAT-DO_SUM = C_MARK.
        PT_FIELDCAT-CHECKBOX = C_MARK.
        PT_FIELDCAT-OUTPUTLEN = 7.
      ELSEIF  PT_FIELDCAT-FIELDNAME = 'PR_MD'.
        PT_FIELDCAT-COLTEXT = 'In process MD'(TT2).
        PT_FIELDCAT-DO_SUM = C_MARK.
        PT_FIELDCAT-CHECKBOX = C_MARK.
        PT_FIELDCAT-OUTPUTLEN = 13.
      ENDIF.

      MODIFY PT_FIELDCAT.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_NEW_TABLE_DATA.
  IF WA_IMPORT_EXIST = ' '.
    CASE WA_LEVEL.
      WHEN C_BASIC.
        CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSQM_INSP_REJ_B'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = WA_SAVE
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
                     IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                     IT_OUTTAB        = IT_ZSQM_INSP_REJ_B[].
      WHEN C_DETAIL.
        CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING I_STRUCTURE_NAME = 'ZSQM_INSP_REJ_D'
                    IS_LAYOUT        = WA_IS_LAYOUT
                    I_SAVE           = WA_SAVE
                    IS_VARIANT       = WA_VARIANT
                    I_DEFAULT        = SPACE
                    IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
          CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                    IT_OUTTAB        = IT_ZSQM_INSP_REJ_D[].
    ENDCASE.

  ELSE.
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'ZSQM_INSP_REJ_D'
                 IS_LAYOUT        = WA_IS_LAYOUT
                 I_SAVE           = WA_SAVE
                 IS_VARIANT       = WA_VARIANT
                 I_DEFAULT        = SPACE
                 IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
       CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                 IT_OUTTAB        = IT_ZSQM_INSP_REJ_D[].
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


  CASE WA_LEVEL .
    WHEN C_BASIC.
      REFRESH  IT_ZSQM_INSP_REJ_D.

      READ TABLE IT_ZSQM_INSP_REJ_B INDEX LW_SEL_INDEX.

      CHECK SY-SUBRC = 0.
      MOVE : IT_ZSQM_INSP_REJ_B-EXTWG TO WA_EXTWG.

      LOOP AT IT_ZSQM_INSP_REJ
                    WHERE WERKS      = IT_ZSQM_INSP_REJ_B-WERKS
                      AND CODEGRP_VH = IT_ZSQM_INSP_REJ_B-CODEGRP_VH
                      AND CODE_VH    = IT_ZSQM_INSP_REJ_B-CODE_VH
                      AND ART        = IT_ZSQM_INSP_REJ_B-ART
                      AND EXTWG      = IT_ZSQM_INSP_REJ_B-EXTWG.

        CLEAR IT_ZSQM_INSP_REJ_D.
        MOVE-CORRESPONDING IT_ZSQM_INSP_REJ TO IT_ZSQM_INSP_REJ_D.

        IF IT_ZSQM_INSP_REJ-VCODE = C_UD_CODE_REJECT.

          IT_ZSQM_INSP_REJ_D-REJECT = C_MARK.

*-          MIC text list for Rejected  'R'
          PERFORM GET_MIC_TEXT_LIST_NOT_OK
                                  USING IT_ZSQM_INSP_REJ_D-PRUEFLOS
                                        IT_ZSQM_INSP_REJ_D-WERKS
                                        C_UD_CODE_REJECT
                               CHANGING IT_ZSQM_INSP_REJ_D-MIC_TEXT.


        ELSEIF IT_ZSQM_INSP_REJ-VCODE = C_UD_CODE_MD.
          IT_ZSQM_INSP_REJ_D-PR_MD = C_MARK.
*-          MIC text list for MD
          PERFORM GET_MIC_TEXT_LIST_NOT_OK
                                  USING IT_ZSQM_INSP_REJ_D-PRUEFLOS
                                        IT_ZSQM_INSP_REJ_D-WERKS
                                        C_UD_CODE_MD
                               CHANGING IT_ZSQM_INSP_REJ_D-MIC_TEXT.

        ENDIF.
        APPEND IT_ZSQM_INSP_REJ_D.
      ENDLOOP.

      SORT IT_ZSQM_INSP_REJ_D  BY WERKS KURZTEXT_VH ART MATNR
                                                       ASCENDING.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
*&      Form  PROC_CHAGE_USAGE_DECISION
*&------------------------------------------------------------------*
FORM PROC_CHAGE_USAGE_DECISION USING    P_ROW_INDEX.

  DATA : LW_VCODE TYPE QVCODE. "/UD CODE

  DATA : LW_INDEX LIKE SY-TABIX.
  LW_INDEX = P_ROW_INDEX.

  CHECK NOT LW_INDEX IS INITIAL.

  CLEAR IT_ZSQM_INSP_REJ_D.
  READ TABLE IT_ZSQM_INSP_REJ_D INDEX P_ROW_INDEX.

  CHECK NOT IT_ZSQM_INSP_REJ_D-PRUEFLOS IS INITIAL.

  SET PARAMETER ID 'QLS' FIELD IT_ZSQM_INSP_REJ_D-PRUEFLOS.

  CALL TRANSACTION 'QA12' AND SKIP FIRST SCREEN.

  CHECK SY-SUBRC = 0.

  COMMIT WORK AND WAIT.


  SELECT SINGLE B~VCODE  INTO LW_VCODE
      FROM  QALS AS A  INNER JOIN QAVE AS B
         ON   A~PRUEFLOS   = B~PRUEFLOS
       WHERE  A~PRUEFLOS   = IT_ZSQM_INSP_REJ_D-PRUEFLOS
          AND A~ART        = IT_ZSQM_INSP_REJ_D-ART
          AND A~WERK       = IT_ZSQM_INSP_REJ_D-WERKS
          AND B~VKATART    = C_UD_CODE_KATART
          AND B~VCODEGRP   = C_UD_CODE_GRUPPE
          AND A~MATNR      = IT_ZSQM_INSP_REJ_D-MATNR.

  CHECK SY-SUBRC = 0.

  CASE LW_VCODE.
    WHEN C_UD_CODE_REJECT.  "/ '09'. "/Reject
      IT_ZSQM_INSP_REJ_D-PR_MD  = ' '.
      IT_ZSQM_INSP_REJ_D-REJECT = C_MARK.

      PERFORM GET_MIC_TEXT_LIST_NOT_OK
                               USING IT_ZSQM_INSP_REJ_D-PRUEFLOS
                                     IT_ZSQM_INSP_REJ_D-WERKS
                                     C_UD_CODE_REJECT
                            CHANGING IT_ZSQM_INSP_REJ_D-MIC_TEXT.

    WHEN C_UD_CODE_MD.      "/ '02', "/MD
      IT_ZSQM_INSP_REJ_D-PR_MD  = C_MARK.
      IT_ZSQM_INSP_REJ_D-REJECT = ' '.
      CLEAR IT_ZSQM_INSP_REJ_D-MIC_TEXT.

      PERFORM GET_MIC_TEXT_LIST_NOT_OK
                               USING IT_ZSQM_INSP_REJ_D-PRUEFLOS
                                     IT_ZSQM_INSP_REJ_D-WERKS
                                     C_UD_CODE_MD
                            CHANGING IT_ZSQM_INSP_REJ_D-MIC_TEXT.

  ENDCASE.


  MODIFY IT_ZSQM_INSP_REJ_D INDEX LW_INDEX.

ENDFORM.                    " PROC_CHAGE_USAGE_DECISION
*&------------------------------------------------------------------*
*&      Form  GET_MIC_TEXT_LIST_NOT_OK
*&------------------------------------------------------------------*
FORM GET_MIC_TEXT_LIST_NOT_OK USING    P_PRUEFLOS
                                       P_WERKS
                                       P_UD_CODE
                              CHANGING P_MIC_TEXT.

  DATA : BEGIN OF LT_MIC OCCURS 4,  "/MIC CODE TEXT OF Not OK(Reject)
            MERKNR    TYPE 	QMERKNRP,
            KURZTEXT  TYPE	QMKKURZTXT,
         END OF LT_MIC.

  SELECT D~MERKNR  D~KURZTEXT
    INTO CORRESPONDING FIELDS OF TABLE LT_MIC
       FROM ( ( QALS AS A  INNER JOIN QAVE AS B
         ON   A~PRUEFLOS   = B~PRUEFLOS ) INNER JOIN QAMR AS C
         ON   A~PRUEFLOS   = C~PRUEFLOS ) INNER JOIN QAMV AS D
         ON   A~PRUEFLOS   = D~PRUEFLOS
          AND C~VORGLFNR   = D~VORGLFNR
          AND C~MERKNR     = D~MERKNR
       WHERE  A~PRUEFLOS   = P_PRUEFLOS
          AND B~VKATART    = C_UD_CODE_KATART
          AND B~VCODEGRP   = C_UD_CODE_GRUPPE
          AND B~VCODE      = P_UD_CODE
          AND C~MBEWERTG   = 'R'
          AND C~SATZSTATUS = '5'.

  IF SY-SUBRC NE 0.
    CLEAR IT_ZSQM_INSP_REJ_D-MIC_TEXT.
  ELSE.
    CLEAR IT_ZSQM_INSP_REJ_D-MIC_TEXT.
    LOOP AT LT_MIC.
      IF SY-TABIX = 1.
        P_MIC_TEXT = LT_MIC-KURZTEXT.
      ELSE.
        CONCATENATE P_MIC_TEXT
                    LT_MIC-KURZTEXT
                        INTO P_MIC_TEXT
                           SEPARATED BY '/'.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_MIC_TEXT_LIST_NOT_OK
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_D_FOR_INTERFACE
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_D_FOR_INTERFACE.

  REFRESH IT_ZSQM_INSP_REJ_D.

  LOOP AT IT_ZSQM_INSP_REJ.

    CLEAR IT_ZSQM_INSP_REJ_D.
    MOVE-CORRESPONDING IT_ZSQM_INSP_REJ TO IT_ZSQM_INSP_REJ_D.

    IF IT_ZSQM_INSP_REJ-VCODE = C_UD_CODE_REJECT.
      IT_ZSQM_INSP_REJ_D-REJECT = C_MARK.
*-          MIC text list for Rejected
      PERFORM GET_MIC_TEXT_LIST_NOT_OK
                              USING IT_ZSQM_INSP_REJ_D-PRUEFLOS
                                    IT_ZSQM_INSP_REJ_D-WERKS
                                    C_UD_CODE_REJECT
                           CHANGING IT_ZSQM_INSP_REJ_D-MIC_TEXT.


    ELSEIF IT_ZSQM_INSP_REJ-VCODE = C_UD_CODE_MD.
      IT_ZSQM_INSP_REJ_D-PR_MD = C_MARK.
*-          MIC text list for MD
      PERFORM GET_MIC_TEXT_LIST_NOT_OK
                              USING IT_ZSQM_INSP_REJ_D-PRUEFLOS
                                    IT_ZSQM_INSP_REJ_D-WERKS
                                    C_UD_CODE_MD
                           CHANGING IT_ZSQM_INSP_REJ_D-MIC_TEXT.
    ENDIF.
    APPEND IT_ZSQM_INSP_REJ_D.
  ENDLOOP.

  SORT IT_ZSQM_INSP_REJ_D  BY WERKS KURZTEXT_VH ART MATNR
                                                   ASCENDING.


ENDFORM.                    " RETRIEV_DETAIL_D_FOR_INTERFACE
*&------------------------------------------------------------------*
*&      Form  GET_DATA_AND_COUNT_BASIC
*&------------------------------------------------------------------*
FORM GET_DATA_AND_COUNT_BASIC.
  REFRESH IT_ZSQM_INSP_REJ_B.
  REFRESH IT_ZSQM_INSP_REJ.

  PERFORM GET_DATA_FROM_DB.

  CHECK NOT IT_ZSQM_INSP_REJ[] IS INITIAL.

  PERFORM COUNT_UP_DATA_FOR_BASIC.

ENDFORM.                    " GET_DATA_AND_COUNT_BASIC
