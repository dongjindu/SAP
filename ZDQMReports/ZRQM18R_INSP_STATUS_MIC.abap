************************************************************************
* Program Name      : ZRQM18R_INSP_STATUS_MIC
* Author            : SeungLyong, Lee
* Creation Date     : 2003.11.24.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Inspection status report by MIC
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZRQM18R_INSP_STATUS_MIC   NO STANDARD PAGE HEADING .

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
         MARC,
         JEST.

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_IS_MIC_SEL. "/Insp. Status(MIC) -Selection Structure
TABLES : ZSQM_IS_MIC_STL. "/Insp Status by MIC :SQL - List
TABLES : ZSQM_IS_MIC_DET_L. "/Inspection Status by MIC :  List
TABLES : ZSQM_IS_MIC_BLIST, "/Inspection Status by MIC : Basic - List
         ZSQM_IS_MIC_DLIST. "/Inspe Status by MIC:Detail(Selected)-List

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

*- Selected MIC / Vehicle/Engine type
DATA : WA_VERWMERKM      TYPE QMERKNR,
       WA_KURZTEXT       TYPE QMKKURZTXT,
       WA_KURZTEXT_VH    TYPE  QTXT_CODE.

*-- Report Level control
DATA : WA_LEVEL(10) TYPE C.

*//Ranges; (R_)
RANGES : R_WERKS       FOR ZSQM_IS_MIC_SEL-WERKS,
         R_ART         FOR ZSQM_IS_MIC_SEL-ART,
         R_LIFNR       FOR ZSQM_IS_MIC_SEL-LIFNR_L,
         R_CODEGRP_VH  FOR ZSQM_IS_MIC_SEL-CODEGRP_VH,
         R_CODE_VH     FOR ZSQM_IS_MIC_SEL-CODE_VH,
         R_EXTWG       FOR ZSQM_IS_MIC_SEL-EXTWG_L,
         R_PASTR       FOR QALS-PASTRTERM,
         R_PAEND       FOR QALS-PAENDTERM,
         R_STAT        FOR JEST-STAT.


*//Internal Tables and Index Fields;(IT_), (I_)
*- Insp Status by MIC:SQL - List
DATA : IT_ZSQM_IS_MIC_STL LIKE ZSQM_IS_MIC_STL OCCURS 0
                                    WITH HEADER LINE.
*-  Inspection Status by MIC : Detail - List
DATA : IT_ZSQM_IS_MIC_DET_L LIKE ZSQM_IS_MIC_DET_L OCCURS 0
                                    WITH HEADER LINE.
*-  Inspection Status by MIC : Basic - List
DATA : IT_ZSQM_IS_MIC_BLIST LIKE ZSQM_IS_MIC_BLIST OCCURS 0
                                    WITH HEADER LINE.
*- Inspe Status by MIC:Detail(Selected)-List
DATA : IT_ZSQM_IS_MIC_DLIST LIKE ZSQM_IS_MIC_DLIST OCCURS 0
                                    WITH HEADER LINE.

*-- Inspection result data by inspection lot with valuation
DATA : BEGIN OF IT_QAMRV  OCCURS 0,
         PRUEFLOS    TYPE QPLOS,
         VORGLFNR    TYPE QLFNKN,
         MERKNR      TYPE QMERKNRP,
         VERWMERKM   TYPE QMERKNR,
         KURZTEXT    TYPE QMKKURZTXT,
         SATZSTATUS  TYPE QBEASTATUS,
         MBEWERTG    TYPE QMBEWERTG,
       END OF IT_QAMRV.

**-- Vehicle/Engine type master temp table for Text input
DATA :  BEGIN OF IT_VEH_MAST OCCURS 10,
          CODEGRP_VH   TYPE  ZQCODEGRP_VH,
          CODE_VH      TYPE  ZQCODE_VH,
          KURZTEXT_VH  TYPE  QTXT_CODE,
        END OF IT_VEH_MAST.

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
  ZSQM_IS_MIC_SEL-KATART_VH = C_VE_ENG_CAT_TYPE.

*- Set default date for inspection start date
  CONCATENATE SY-DATUM+0(4)
              '0101'    INTO ZSQM_IS_MIC_SEL-PASTR_L.
  MOVE : SY-DATUM TO ZSQM_IS_MIC_SEL-PASTR_H.

*- Get Vehicle/Engine type master for text field of list
  PERFORM GET_VEH_ENG_TYP_MASTER.

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

        CHECK NOT IT_ZSQM_IS_MIC_DLIST[] IS INITIAL.

        WA_LEVEL = C_DETAIL.

        WA_RENEWAL_FLG = C_MARK.

      WHEN C_DETAIL.

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

          CHECK NOT IT_ZSQM_IS_MIC_DLIST[] IS INITIAL.

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
*&------------------------------------------------------------------*
*&      Module  SET_LISTBOX_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE SET_LISTBOX_9000 OUTPUT.
  CHECK WA_LISTBOX_SET IS INITIAL.
  WA_LISTBOX_SET = C_MARK.

*- Set Listbox for Selection screen : Inspectio type
  PERFORM SET_LISTBOX_FOR_QPART   USING 'ZSQM_IS_MIC_SEL-ART'.

*- Set Listbox for Selection screen : Syatem status
  PERFORM SET_LISTBOX_FOR_STATUS   USING 'ZSQM_IS_MIC_SEL-STAT'.

ENDMODULE.                 " SET_LISTBOX_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Form  SET_LISTBOX_FOR_QPART
*&------------------------------------------------------------------*
FORM SET_LISTBOX_FOR_QPART USING    VALUE(P_SCREEN_FLD_NAME).


  DATA: LT_LIST  TYPE VRM_VALUES,
        LW_VALUE LIKE LINE OF LT_LIST.

**-- Inspection Type Constants

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
*&      Form  SET_LISTBOX_FOR_STATUS
*&-----------------------------------------------------------------*
FORM SET_LISTBOX_FOR_STATUS USING  VALUE(P_SCREEN_FLD_NAME).

  DATA: LT_LIST  TYPE VRM_VALUES,
        LW_VALUE LIKE LINE OF LT_LIST.

**-- System status of Inspection lot
  CLEAR  LW_VALUE-KEY.
  MOVE : 'All' TO LW_VALUE-TEXT.
  APPEND LW_VALUE TO LT_LIST.

  SELECT  TXT04  AS KEY
          TXT30  AS TEXT
        APPENDING CORRESPONDING FIELDS OF TABLE LT_LIST
     FROM TJ02T
       WHERE SPRAS = SY-LANGU
         AND ISTAT IN (C_INSP_STATUS_REL,
                       C_INSP_STATUS_RREC,
                       C_INSP_STATUS_UD).



  PERFORM EXECUTE_SET_VALUES   TABLES LT_LIST
                               USING  P_SCREEN_FLD_NAME.

ENDFORM.                    " SET_LISTBOX_FOR_STATUS
*&-----------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
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
*&      Module  CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
MODULE CHECK_VH_ENG_INPUT INPUT.

  DATA : LW_ZVQM_VEHICLE TYPE ZVQM_VEHICLE.

  IF     ZSQM_IS_MIC_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_IS_MIC_SEL-CODE_VH    IS INITIAL.

    MESSAGE E000(ZMQM)
        WITH 'Please Input Vehicle/Engine Code Group.'(E01).

  ENDIF.

  CHECK NOT ZSQM_IS_MIC_SEL-CODEGRP_VH IS INITIAL AND
        NOT ZSQM_IS_MIC_SEL-CODE_VH    IS INITIAL.


  SELECT SINGLE * INTO LW_ZVQM_VEHICLE
       FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_IS_MIC_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_MIC_SEL-CODEGRP_VH
           AND CODE       = ZSQM_IS_MIC_SEL-CODE_VH.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH
       'Vehicle/Engine type  code group and code are not matched'(E02).
  ENDIF.

ENDMODULE.                 " CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_SCREEN_9000 INPUT.

  CLEAR : ZSQM_IS_MIC_SEL-KURZTEXT_G,
          ZSQM_IS_MIC_SEL-KURZTEXT_VH.


  IF NOT ZSQM_IS_MIC_SEL-CODEGRP_VH IS INITIAL.
    SELECT SINGLE KURZTEXT INTO ZSQM_IS_MIC_SEL-KURZTEXT_G
      FROM ZVQM_QPGRT01
         WHERE KATALOGART = ZSQM_IS_MIC_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_MIC_SEL-CODEGRP_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_MIC_SEL-CODEGRP_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_IS_MIC_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_IS_MIC_SEL-CODE_VH    IS INITIAL.
    SELECT SINGLE KURZTEXT_C INTO ZSQM_IS_MIC_SEL-KURZTEXT_VH
      FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_IS_MIC_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_MIC_SEL-CODEGRP_VH
           AND CODE       = ZSQM_IS_MIC_SEL-CODE_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_MIC_SEL-CODEGRP_VH
                              ','
                              ZSQM_IS_MIC_SEL-CODE_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

ENDMODULE.                 " GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_PLANT  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_PLANT INPUT.
  CLEAR ZSQM_IS_MIC_SEL-NAME1.

  CHECK NOT ZSQM_IS_MIC_SEL-WERKS IS INITIAL.

  CALL FUNCTION 'AIPH_GET_TEXT_PLANT'
       EXPORTING
            I_WERKS = ZSQM_IS_MIC_SEL-WERKS
       IMPORTING
            E_TEXT  = ZSQM_IS_MIC_SEL-NAME1.

ENDMODULE.                 " GET_TEXT_PLANT  INPUT
*&-------------------------------------------------------------------*
*&      Module  GET_TEXT_VENDOR  INPUT
*&-------------------------------------------------------------------*
MODULE GET_TEXT_VENDOR INPUT.
  CLEAR ZSQM_IS_MIC_SEL-LIFNR_N.
  CHECK NOT ZSQM_IS_MIC_SEL-LIFNR_L IS INITIAL.

  SELECT SINGLE NAME1 INTO ZSQM_IS_MIC_SEL-LIFNR_N
    FROM LFA1
      WHERE LIFNR = ZSQM_IS_MIC_SEL-LIFNR_L.

ENDMODULE.                 " GET_TEXT_VENDOR  INPUT
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_EXT_MAT_GRP  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_EXT_MAT_GRP INPUT.
  CLEAR ZSQM_IS_MIC_SEL-EWBEZ.
  CHECK NOT ZSQM_IS_MIC_SEL-EXTWG_L IS INITIAL.

  SELECT SINGLE EWBEZ  INTO ZSQM_IS_MIC_SEL-EWBEZ
    FROM TWEWT
      WHERE EXTWG = ZSQM_IS_MIC_SEL-EXTWG_L
        AND SPRAS = SY-LANGU.
ENDMODULE.                 " GET_TEXT_EXT_MAT_GRP  INPUT
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

    WHEN 'ONLI'.

*    - Check plant by Vehicle/Engine type Code Group
      PERFORM CHECK_PLANT_AND_CODE_G.
*    - Set range data by user input for query to DB
      PERFORM SET_RANGE_VALUE.

*--   Get data and make detail list for Basic list.
      PERFORM GET_D_AND_MAKE_LIST_BASIC.

      CHECK NOT IT_ZSQM_IS_MIC_BLIST[] IS INITIAL.

      WA_LEVEL = C_BASIC.
      WA_RENEWAL_FLG = C_MARK.

      CALL SCREEN 9100.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  CHECK_PLANT_AND_CODE_G
*&------------------------------------------------------------------*
FORM CHECK_PLANT_AND_CODE_G.

  CASE ZSQM_IS_MIC_SEL-CODEGRP_VH.
    WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
      IF ZSQM_IS_MIC_SEL-WERKS+0(1) = 'P'.
        MESSAGE E000(ZMQM) WITH
         'Plant and Code.G are not matched'(E50).
      ENDIF.
    WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
      IF ZSQM_IS_MIC_SEL-WERKS+0(1) = 'E' .
        MESSAGE E000(ZMQM) WITH TEXT-E50.
      ENDIF.
  ENDCASE.

*  CLEAR ZSQM_IS_MIC_SEL-NAME1.
*
*  SELECT SINGLE NAME1 INTO ZSQM_IS_ST_SEL-NAME1
*    FROM T001W
*      WHERE WERKS = ZSQM_IS_ST_SEL-WERKS.

ENDFORM.                    " CHECK_PLANT_AND_CODE_G
*&------------------------------------------------------------------*
*&      Form  SET_RANGE_VALUE
*&------------------------------------------------------------------*
FORM SET_RANGE_VALUE.

  REFRESH : R_LIFNR, R_CODEGRP_VH, R_WERKS, R_ART,
            R_CODE_VH, R_EXTWG, R_PASTR, R_PAEND, R_STAT.
  CLEAR   : R_LIFNR, R_CODEGRP_VH, R_WERKS, R_ART,
            R_CODE_VH, R_EXTWG, R_PASTR, R_PAEND, R_STAT.

  RANGE_MACRO :
     R_ART         ZSQM_IS_MIC_SEL-ART         '',
     R_LIFNR       ZSQM_IS_MIC_SEL-LIFNR_L     '',
     R_CODEGRP_VH  ZSQM_IS_MIC_SEL-CODEGRP_VH  '',
     R_CODE_VH     ZSQM_IS_MIC_SEL-CODE_VH     '',
     R_EXTWG       ZSQM_IS_MIC_SEL-EXTWG_L  ZSQM_IS_MIC_SEL-EXTWG_H,
     R_PASTR       ZSQM_IS_MIC_SEL-PASTR_L  ZSQM_IS_MIC_SEL-PASTR_H,
     R_PAEND       ZSQM_IS_MIC_SEL-PAEND_L  ZSQM_IS_MIC_SEL-PAEND_H.

*-- Inspection Lot status value
  IF ZSQM_IS_MIC_SEL-STAT IS INITIAL.  "/All
    RANGE_MACRO :
       R_STAT    C_INSP_STATUS_REL    '',
       R_STAT    C_INSP_STATUS_RREC   '',
       R_STAT    C_INSP_STATUS_UD     ''.
  ELSE.
    CASE ZSQM_IS_MIC_SEL-STAT.
      WHEN 'REL'.
        RANGE_MACRO : R_STAT    C_INSP_STATUS_REL    ''.
      WHEN 'RREC'.
        RANGE_MACRO : R_STAT    C_INSP_STATUS_RREC   ''.
      WHEN 'UD'.
        RANGE_MACRO : R_STAT    C_INSP_STATUS_UD     ''.
    ENDCASE.
  ENDIF.

**-- Usage Decision range
*  RANGE_MACRO :
*   R_VCODE    C_UD_CODE_REJECT    '',
*   R_VCODE    C_UD_CODE_MD        '',
*   R_VCODE    C_UD_CODE_ACCEPT    ''.

ENDFORM.                    " SET_RANGE_VALUE
*&------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&------------------------------------------------------------------*
FORM GET_DATA_FROM_DB.
  DATA : LR_PRUEFLOS TYPE RANGE OF QPLOS WITH HEADER LINE.

  DATA : LT_ZSQM_IS_MIC_STL LIKE IT_ZSQM_IS_MIC_STL OCCURS 0
                                                WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.

  REFRESH : IT_ZSQM_IS_MIC_STL.
  REFRESH : IT_QAMRV.

  CASE ZSQM_IS_MIC_SEL-ART.
    WHEN C_INSP_TYPE_ISIR  OR  C_INSP_TYPE_REGULAR.

      SELECT A~CODEGRP_VH A~CODE_VH C~EXTWG A~MATNR A~KTEXTMAT
             A~PRUEFLOS D~STAT  B~EONO  B~LIFNR
             B~DATUV_0010 B~DATUB_0010 B~DATUV_0020 B~DATUB_0020
             B~DATUV_0030 B~DATUB_0030 B~DATUV_0040 B~DATUB_0040
             B~DATUV_0050 B~DATUB_0050 B~DATUV_0060 B~DATUB_0060
             B~DATUV_0070 B~DATUB_0070 B~DATUV_0080 B~DATUB_0080
             B~DATUV_0090 B~DATUB_0090 B~DATUV_0100 B~DATUB_0100
             B~DATUV_0110 B~DATUB_0110 B~DATUV_0120 B~DATUB_0120
             B~DATUV_0130 B~DATUB_0130 B~DATUV_0140 B~DATUB_0140
             B~DATUV_0150 B~DATUB_0150 B~DATUV_0160 B~DATUB_0160
             B~DATUV_0170 B~DATUB_0170 B~DATUV_0180 B~DATUB_0180
             B~DATUV_0190 B~DATUB_0190 B~DATUV_0200 B~DATUB_0200
             B~DATUV_0210 B~DATUB_0210 B~DATUV_0220 B~DATUB_0220
         INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_IS_MIC_STL
            FROM ( ( QALS AS A INNER JOIN ZTQM_QNS_ITEM AS B
               ON A~PRUEFLOS = B~PRUEFLOS ) INNER JOIN MARA AS C
               ON A~MATNR    = C~MATNR    ) INNER JOIN JEST AS D
               ON A~OBJNR    = D~OBJNR
            WHERE A~WERK        = ZSQM_IS_MIC_SEL-WERKS
              AND A~ART         = ZSQM_IS_MIC_SEL-ART
              AND A~SELLIFNR    IN R_LIFNR
              AND A~KATART_VH   = ZSQM_IS_MIC_SEL-KATART_VH
              AND A~CODEGRP_VH  IN R_CODEGRP_VH
              AND A~CODE_VH     IN R_CODE_VH
              AND A~PASTRTERM   IN R_PASTR
              AND A~PAENDTERM   IN R_PAEND
              AND C~EXTWG       IN R_EXTWG
              AND D~INACT       = ' '
              AND D~STAT        IN R_STAT.

    WHEN C_INSP_TYPE_MS.

      SELECT A~CODEGRP_VH A~CODE_VH C~EXTWG A~MATNR A~KTEXTMAT
             A~PRUEFLOS D~STAT  B~EONO B~LIFNR
             B~DATUV_1010  AS DATUV_0010  B~DATUB_1010  AS DATUB_0010
             B~DATUV_1020  AS DATUV_0020  B~DATUB_1020  AS DATUB_0020
             B~DATUV_1030  AS DATUV_0030  B~DATUB_1030  AS DATUB_0030
         INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_IS_MIC_STL
            FROM ( ( QALS AS A INNER JOIN ZTQM_QNS_IT_MS AS B
               ON A~PRUEFLOS = B~PRUEFLOS_MS ) INNER JOIN MARA AS C
               ON A~MATNR    = C~MATNR    ) INNER JOIN JEST AS D
               ON A~OBJNR    = D~OBJNR
            WHERE A~WERK        = ZSQM_IS_MIC_SEL-WERKS
              AND A~ART         = ZSQM_IS_MIC_SEL-ART
              AND A~SELLIFNR    IN R_LIFNR
              AND A~KATART_VH   = ZSQM_IS_MIC_SEL-KATART_VH
              AND A~CODEGRP_VH  IN R_CODEGRP_VH
              AND A~CODE_VH     IN R_CODE_VH
              AND A~PASTRTERM   IN R_PASTR
              AND A~PAENDTERM   IN R_PAEND
              AND C~EXTWG       IN R_EXTWG
              AND D~INACT       = ' '
              AND D~STAT        IN R_STAT.

  ENDCASE.

  IF SY-SUBRC NE  0.
    MESSAGE W000(ZMQM) WITH 'No entries!'(E03).
    EXIT.
  ENDIF.

*-  Delete  record of lot have both Released and insp(Results confirmed
*-  status or both insp and UD.
  LT_ZSQM_IS_MIC_STL[] = IT_ZSQM_IS_MIC_STL[].

  LOOP AT LT_ZSQM_IS_MIC_STL WHERE STAT = C_INSP_STATUS_RREC.

    READ TABLE IT_ZSQM_IS_MIC_STL WITH KEY
                         PRUEFLOS = LT_ZSQM_IS_MIC_STL-PRUEFLOS
                         STAT     = C_INSP_STATUS_REL.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      DELETE IT_ZSQM_IS_MIC_STL INDEX LW_INDEX.
    ENDIF.

  ENDLOOP.

  LOOP AT LT_ZSQM_IS_MIC_STL WHERE STAT = C_INSP_STATUS_UD.

    READ TABLE IT_ZSQM_IS_MIC_STL WITH KEY
                          PRUEFLOS = LT_ZSQM_IS_MIC_STL-PRUEFLOS
                          STAT     = C_INSP_STATUS_RREC.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      DELETE IT_ZSQM_IS_MIC_STL INDEX LW_INDEX.
    ENDIF.

  ENDLOOP.


*- Get inspection lot No list for SQL of MIC result data
  LR_PRUEFLOS-SIGN = 'I'. LR_PRUEFLOS-OPTION = 'EQ'.
  LOOP AT IT_ZSQM_IS_MIC_STL.

    LR_PRUEFLOS-LOW = IT_ZSQM_IS_MIC_STL-PRUEFLOS.
    APPEND LR_PRUEFLOS.
  ENDLOOP.

*-- Get inspection processing and result(QAMV + QAMR) data
  SELECT B~PRUEFLOS B~VORGLFNR B~MERKNR A~MBEWERTG
         B~SATZSTATUS B~VERWMERKM B~KURZTEXT
     INTO CORRESPONDING FIELDS OF TABLE IT_QAMRV
       FROM QAMV AS B   LEFT OUTER JOIN QAMR AS A
          ON   A~PRUEFLOS  = B~PRUEFLOS
           AND A~VORGLFNR  = B~VORGLFNR
           AND A~MERKNR    = B~MERKNR
         WHERE  B~PRUEFLOS IN LR_PRUEFLOS.


ENDFORM.                    " GET_DATA_FROM_DB
*&------------------------------------------------------------------*
*&      Form  GET_VEH_ENG_TYP_MASTER
*&------------------------------------------------------------------*
FORM GET_VEH_ENG_TYP_MASTER.

  SELECT CODEGRUPPE AS CODEGRP_VH
         CODE       AS CODE_VH
         KURZTEXT   AS KURZTEXT_VH
     INTO CORRESPONDING FIELDS OF TABLE IT_VEH_MAST
       FROM QPCT
         WHERE KATALOGART = C_VE_ENG_CAT_TYPE
           AND SPRACHE    = SY-LANGU
           AND INAKTIV    = ' '.

ENDFORM.                    " GET_VEH_ENG_TYP_MASTER
*&-----------------------------------------------------------------*
*&      Form  MAKE_MIC_DETAIL_LIST
*&-----------------------------------------------------------------*
FORM MAKE_MIC_DETAIL_LIST.
  DATA : LW_DATU_FLD_NAME(40) TYPE C.

  FIELD-SYMBOLS : <LW_FS>.

  REFRESH : IT_ZSQM_IS_MIC_DET_L.

  LOOP AT IT_ZSQM_IS_MIC_STL.
    CLEAR IT_ZSQM_IS_MIC_DET_L.

    LOOP AT IT_QAMRV WHERE PRUEFLOS = IT_ZSQM_IS_MIC_STL-PRUEFLOS.
      CLEAR IT_ZSQM_IS_MIC_DET_L.
      MOVE-CORRESPONDING : IT_ZSQM_IS_MIC_STL TO IT_ZSQM_IS_MIC_DET_L,
                           IT_QAMRV           TO IT_ZSQM_IS_MIC_DET_L.
      CLEAR LW_DATU_FLD_NAME.

      CONCATENATE 'IT_ZSQM_IS_MIC_STL-DATUV_'    "/Planned date of MIC
                  IT_QAMRV-MERKNR
                        INTO LW_DATU_FLD_NAME.

      ASSIGN (LW_DATU_FLD_NAME) TO <LW_FS>.

*-- Skip below when Planned date is blank. It will be not counted
      IF <LW_FS> IS INITIAL.
        CONTINUE.
      ENDIF.

      MOVE : <LW_FS> TO IT_ZSQM_IS_MIC_DET_L-DATUV.

      CONCATENATE 'IT_ZSQM_IS_MIC_STL-DATUB_'    "/Actual date of MIC
                  IT_QAMRV-MERKNR
                        INTO LW_DATU_FLD_NAME.

      ASSIGN (LW_DATU_FLD_NAME) TO <LW_FS>.

      MOVE : <LW_FS> TO IT_ZSQM_IS_MIC_DET_L-DATUB.

      APPEND IT_ZSQM_IS_MIC_DET_L.

    ENDLOOP.

  ENDLOOP.

*-- Fill Vehicle/Engine type Text to Internal tables
  LOOP AT IT_VEH_MAST.
    CLEAR IT_ZSQM_IS_MIC_DET_L.
    MOVE : IT_VEH_MAST-KURZTEXT_VH TO IT_ZSQM_IS_MIC_DET_L-KURZTEXT_VH.

    MODIFY IT_ZSQM_IS_MIC_DET_L TRANSPORTING KURZTEXT_VH
                      WHERE CODEGRP_VH = IT_VEH_MAST-CODEGRP_VH
                        AND CODE_VH    = IT_VEH_MAST-CODE_VH.

  ENDLOOP.

ENDFORM.                    " MAKE_MIC_DETAIL_LIST
*&------------------------------------------------------------------*
*&      Form  COUNT_UP_DATA_FOR_BASIC
*&------------------------------------------------------------------*
FORM COUNT_UP_DATA_FOR_BASIC.

**-  Inspection Status by MIC : Basic - List
*TABLES : IT_ZSQM_IS_MIC_BLIST LIKE ZSQM_IS_MIC_BLIST OCCURS 0
*                                    WITH HEADER LINE.

  REFRESH IT_ZSQM_IS_MIC_BLIST.

  LOOP AT IT_ZSQM_IS_MIC_DET_L.
    CLEAR IT_ZSQM_IS_MIC_BLIST.
    MOVE-CORRESPONDING IT_ZSQM_IS_MIC_DET_L TO IT_ZSQM_IS_MIC_BLIST.

    IF NOT IT_ZSQM_IS_MIC_DET_L-DATUV IS INITIAL.
      IT_ZSQM_IS_MIC_BLIST-PLANNED_C = 1.
    ENDIF.
    IF NOT IT_ZSQM_IS_MIC_DET_L-DATUB IS INITIAL.
      IT_ZSQM_IS_MIC_BLIST-ACTUAL_C = 1.
    ENDIF.

    COLLECT IT_ZSQM_IS_MIC_BLIST.

  ENDLOOP.

*-- Calculate progress rate

  LOOP AT IT_ZSQM_IS_MIC_BLIST.
    IF NOT IT_ZSQM_IS_MIC_BLIST-PLANNED_C IS INITIAL.

      IT_ZSQM_IS_MIC_BLIST-PROGRESS =
         ( IT_ZSQM_IS_MIC_BLIST-ACTUAL_C /
                IT_ZSQM_IS_MIC_BLIST-PLANNED_C ) * 100.

    ENDIF.

    MODIFY IT_ZSQM_IS_MIC_BLIST.
  ENDLOOP.

*- Sort
  SORT IT_ZSQM_IS_MIC_BLIST BY   KURZTEXT_VH VERWMERKM ASCENDING.

ENDFORM.                    " COUNT_UP_DATA_FOR_BASIC
*&-------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&-------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS '9100'.
  SET TITLEBAR  '9100' WITH WA_LEVEL.

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

ENDMODULE.                 " EXIT_9100  INPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.

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

    WHEN 'REFRESH'.

*--   Get data and make detail list for Basic list.
      PERFORM GET_D_AND_MAKE_LIST_BASIC.

      IF  IT_ZSQM_IS_MIC_BLIST[] IS INITIAL.
        REFRESH IT_ZSQM_IS_MIC_DLIST.
      ENDIF.

      IF WA_LEVEL = C_DETAIL.
        REFRESH IT_ZSQM_IS_MIC_DLIST.

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
      WA_STRUCTURE_NAME = 'ZSQM_IS_MIC_BLIST'.

    WHEN C_DETAIL.
      WA_STRUCTURE_NAME = 'ZSQM_IS_MIC_DLIST'.
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

  CASE WA_LEVEL.
    WHEN C_BASIC.
* Set field attribute
      LOOP AT PT_FIELDCAT.
        CASE PT_FIELDCAT-FIELDNAME.
          WHEN 'WERKS' OR 'KURZTEXT_VH' OR 'VERWMERKM' OR 'KURZTEXT'.
            PT_FIELDCAT-KEY_SEL = C_MARK.
            PT_FIELDCAT-KEY     = C_MARK.

            IF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH'. "/Vehicle/Engine
              MOVE  TEXT-TT6 TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
            ELSEIF PT_FIELDCAT-FIELDNAME = 'KURZTEXT'. "/MIC Text
              MOVE  TEXT-TT7 TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
            ENDIF.

          WHEN OTHERS.
            PT_FIELDCAT-DO_SUM    = C_MARK.

            CASE PT_FIELDCAT-FIELDNAME.
              WHEN 'PLANNED_C'.
                MOVE  'Planned'(TC1)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
                PT_FIELDCAT-EMPHASIZE = 'C300'.
              WHEN 'ACTUAL_C'.
                MOVE  'Actual'(TC2)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
                PT_FIELDCAT-EMPHASIZE = 'C311'.
              WHEN 'PROGRESS'.
                MOVE  'Progress(%)'(TC3)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
                PT_FIELDCAT-DO_SUM    = ' '.
            ENDCASE.
*          -  Set Column Color
            IF PT_FIELDCAT-FIELDNAME = 'PROGRESS'.
              PT_FIELDCAT-EMPHASIZE = 'C710'.
            ENDIF.

        ENDCASE.

        MODIFY PT_FIELDCAT.

      ENDLOOP.

    WHEN C_DETAIL.

      LOOP AT PT_FIELDCAT.
        CASE PT_FIELDCAT-FIELDNAME.
          WHEN 'KURZTEXT_VH' OR 'EXTWG' OR 'MATNR' OR 'KTEXTMAT'.
            PT_FIELDCAT-KEY_SEL = C_MARK.
            PT_FIELDCAT-KEY     = C_MARK.

            IF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH'. "/Vehicle/Engine
              MOVE  TEXT-TT6 TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
            ELSEIF PT_FIELDCAT-FIELDNAME = 'KTEXTMAT'. "/Material
              MOVE  TEXT-TT8 TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
            ENDIF.

          WHEN OTHERS.

            CASE PT_FIELDCAT-FIELDNAME.
              WHEN 'DATUV'.
                MOVE  'Planned Date'(TC4)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
                PT_FIELDCAT-EMPHASIZE = 'C300'.
              WHEN 'DATUB'.
                MOVE  'Actual Date'(TC5)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
                PT_FIELDCAT-EMPHASIZE = 'C311'.
            ENDCASE.
*          -  Set Column Color
            IF PT_FIELDCAT-FIELDNAME = 'MBEWERTG'.
              PT_FIELDCAT-EMPHASIZE = 'C710'.
            ENDIF.
        ENDCASE.

        MODIFY PT_FIELDCAT.

      ENDLOOP.

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
                   IT_OUTTAB        = IT_ZSQM_IS_MIC_BLIST[].


    WHEN C_DETAIL.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                  IS_LAYOUT        = WA_IS_LAYOUT
                  I_SAVE           = WA_SAVE
                  IS_VARIANT       = WA_VARIANT
                  I_DEFAULT        = SPACE
                  IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
        CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                  IT_OUTTAB        = IT_ZSQM_IS_MIC_DLIST[].
  ENDCASE.

ENDFORM.                    " SET_NEW_TABLE_DATA
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA USING    P_INDEX.
  DATA : LW_SEL_INDEX   LIKE SY-TABIX.
  DATA : LW_TABLE_LINES LIKE SY-TABIX.

  LW_SEL_INDEX = P_INDEX.

  REFRESH IT_ZSQM_IS_MIC_DLIST.

  CASE WA_LEVEL .
    WHEN C_BASIC.

      READ TABLE IT_ZSQM_IS_MIC_BLIST  INDEX LW_SEL_INDEX.
      CHECK SY-SUBRC = 0.
      MOVE : IT_ZSQM_IS_MIC_BLIST-KURZTEXT_VH TO WA_KURZTEXT_VH,
             IT_ZSQM_IS_MIC_BLIST-VERWMERKM   TO WA_VERWMERKM,
             IT_ZSQM_IS_MIC_BLIST-KURZTEXT    TO WA_KURZTEXT.

    WHEN OTHERS.
      EXIT.
  ENDCASE.


  LOOP AT IT_ZSQM_IS_MIC_DET_L
                     WHERE KURZTEXT_VH = WA_KURZTEXT_VH
                       AND VERWMERKM   = WA_VERWMERKM.

    CLEAR IT_ZSQM_IS_MIC_DLIST.
    MOVE-CORRESPONDING IT_ZSQM_IS_MIC_DET_L
                           TO IT_ZSQM_IS_MIC_DLIST.
    APPEND IT_ZSQM_IS_MIC_DLIST.
  ENDLOOP.

  SORT IT_ZSQM_IS_MIC_DLIST BY KURZTEXT_VH EXTWG MATNR    ASCENDING.

ENDFORM.                    " RETRIEV_DETAIL_DATA
*&-----------------------------------------------------------------*
*&      Form  GET_D_AND_MAKE_LIST_BASIC
*&-----------------------------------------------------------------*
FORM GET_D_AND_MAKE_LIST_BASIC.

*    - Get data from DB
  PERFORM GET_DATA_FROM_DB.

  CHECK NOT IT_ZSQM_IS_MIC_STL[] IS INITIAL.

*    - make detail list of inspection lot by MIC
  PERFORM MAKE_MIC_DETAIL_LIST.

  PERFORM COUNT_UP_DATA_FOR_BASIC.

ENDFORM.                    " GET_D_AND_MAKE_LIST_BASIC
*&------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA_REFRESH
*&------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA_REFRESH.
  REFRESH IT_ZSQM_IS_MIC_DLIST.

  LOOP AT IT_ZSQM_IS_MIC_DET_L
                     WHERE KURZTEXT_VH = WA_KURZTEXT_VH
                       AND VERWMERKM   = WA_VERWMERKM.

    CLEAR IT_ZSQM_IS_MIC_DLIST.
    MOVE-CORRESPONDING IT_ZSQM_IS_MIC_DET_L
                           TO IT_ZSQM_IS_MIC_DLIST.
    APPEND IT_ZSQM_IS_MIC_DLIST.
  ENDLOOP.

  SORT IT_ZSQM_IS_MIC_DLIST BY KURZTEXT_VH EXTWG MATNR    ASCENDING.

ENDFORM.                    " RETRIEV_DETAIL_DATA_REFRESH
