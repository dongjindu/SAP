************************************************************************
* Program Name      : ZRQM03R_INSP_STATUS
* Author            : SeungLyong, Lee
* Creation Date     : 2003.11.24.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No :
* Addl Documentation:
* Description       : Inspection Status reports
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZRQM03R_INSP_STATUS  NO STANDARD PAGE HEADING.

*&&& Data Declaration.  &&&*
TYPE-POOLS ZQMT1.   "/QM-Type group for inspection
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
         JEST,
         QAVE.

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_IS_ST_SEL. "/Inspection Status -Selection screen Structure
TABLES : ZSQM_IS_ST_LIST, "/Inspection Status : SQL - List
         ZSQM_IS_ST_L_D,  "/Inspection Status : status(checkbox)
         ZSQM_IS_ST_L_CNT. "/Inspection Status Count : Basic List by key

*//Structure Declaration for Other Report transaction interface
TABLES : ZSQM_IS_RES_SEL, "/Inspection Result Status-ZRQM04R_INSP_RESULT
         ZSQM_INSP_SEL.   "/Inspection Status -ZRQM11R_INSP_REJECT

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

*-- List Box Set Flag
DATA : WA_LISTBOX_SET.

*-- Error check flag
DATA : WA_ERROR_FOUNDED.

*-- Inspection date
DATA : WA_FIRSTDAY  TYPE QPRSTART, "/Inspection Start Date
       WA_LASTDAY   TYPE QPRENDE.  "/End Date of the Inspection


*- Selected Ext. material group
DATA : WA_EXTWG  TYPE EXTWG.

*- Selected Basic list item data : Temporary workarea
DATA : BEGIN OF WA_ST_SEL,
         WERKS        TYPE WERKS_D,
         KURZTEXT_VH  LIKE ZSQM_IS_ST_L_CNT-KURZTEXT_VH,
         ART          LIKE ZSQM_IS_ST_L_CNT-ART,
         EXTWG        TYPE EXTWG,
       END OF WA_ST_SEL.

*-- Report Level control
DATA : WA_LEVEL(10) TYPE C.

*//Ranges; (R_)
RANGES : R_WERKS       FOR T001W-WERKS,
         R_ART         FOR QALS-ART,
         R_LIFNR       FOR ZSQM_IS_ST_SEL-LIFNR_L,
         R_CODEGRP_VH  FOR ZSQM_IS_ST_SEL-CODEGRP_VH,
         R_CODE_VH     FOR ZSQM_IS_ST_SEL-CODE_VH,
         R_EXTWG       FOR ZSQM_IS_ST_SEL-EXTWG_L,
         R_PASTR       FOR QALS-PASTRTERM,
         R_PAEND       FOR QALS-PAENDTERM,
         R_STAT        FOR JEST-STAT,
         R_VCODE       FOR QAVE-VCODE.


*//Internal Tables and Index Fields;(IT_), (I_)

**-- Vehicle/Engine type master temp table for Text input
DATA :  BEGIN OF IT_VEH_MAST OCCURS 10,
          CODEGRP_VH   TYPE  ZQCODEGRP_VH,
          CODE_VH      TYPE  ZQCODE_VH,
          KURZTEXT_VH  TYPE  QTXT_CODE,
        END OF IT_VEH_MAST.

*- Inspection Status : SQL - List
DATA : IT_ZSQM_IS_ST_LIST LIKE ZSQM_IS_ST_LIST OCCURS 0
                                                WITH HEADER LINE.
*- Inspection Status : status(checkbox)
DATA : IT_ZSQM_IS_ST_L_D LIKE ZSQM_IS_ST_L_D OCCURS 0
                                                WITH HEADER LINE.

*- Inspection Status : status(checkbox) : Detail by EXTWG
DATA : IT_ZSQM_IS_ST_L_D_D LIKE ZSQM_IS_ST_L_D OCCURS 0
                                                WITH HEADER LINE.

*- Inspection Status Count : Basic List by key
DATA : IT_ZSQM_IS_ST_L_CNT LIKE ZSQM_IS_ST_L_CNT OCCURS 0
                                                WITH HEADER LINE.

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
  ZSQM_IS_ST_SEL-KATART_VH = C_VE_ENG_CAT_TYPE.

*- Set default date for inspection start date
  CONCATENATE SY-DATUM+0(4)
              '0101'    INTO ZSQM_IS_ST_SEL-PASTR_L.
  MOVE : SY-DATUM TO ZSQM_IS_ST_SEL-PASTR_H.

**- Get Vehicle/Engine type master for text field of list
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

     HANDLE_MENU_BUTTON
        FOR EVENT MENU_BUTTON OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_UCOMM,

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
**//    Detail
*         append a separator('3') to normal toolbar
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to show detail List of selected item.
        CLEAR LS_TOOLBAR.
        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
        MOVE 'DETAIL'           TO LS_TOOLBAR-FUNCTION.
        MOVE ICON_DETAIL        TO LS_TOOLBAR-ICON.
        MOVE 'Show status detail'(T12) TO LS_TOOLBAR-QUICKINFO.
        MOVE 'Status Detail'(T13)      TO LS_TOOLBAR-TEXT.
        MOVE ' '                TO LS_TOOLBAR-DISABLED.

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

**//    Other Report Transaction
*         append a separator('3') to normal toolbar
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to show detail List of selected item.
        CLEAR LS_TOOLBAR.
        MOVE 2 TO LS_TOOLBAR-BUTN_TYPE. "/ Context Menu Button Type
        MOVE 'OTHER_REPORT'           TO LS_TOOLBAR-FUNCTION.
        MOVE ICON_OTHER_OBJECT        TO LS_TOOLBAR-ICON.
        MOVE 'Other Detail Reports'(T15) TO LS_TOOLBAR-QUICKINFO.
        MOVE 'Other Reports'(T16)      TO LS_TOOLBAR-TEXT.
        MOVE ' '             TO LS_TOOLBAR-DISABLED.   "/For Submenu

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

      WHEN C_DETAIL.

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
    IF E_UCOMM = 'OTHER_REPORT'.
      CALL METHOD E_OBJECT->ADD_FUNCTION
                  EXPORTING FCODE   = 'TO_SCHEDULE'
                            TEXT    = 'Schedule detail'(T17). "
      CALL METHOD E_OBJECT->ADD_FUNCTION
                  EXPORTING FCODE   = 'TO_REJECT'
                            TEXT    = 'Reject detail'(T18). "

    ENDIF.

  ENDMETHOD.


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

        CHECK NOT IT_ZSQM_IS_ST_L_D_D[] IS INITIAL.

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
    CASE WA_LEVEL.
      WHEN C_BASIC.
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
                        TXT1  = TEXT-E05.
            ELSE.
              CLEAR LW_LINES.
              DESCRIBE TABLE LT_ROWS LINES LW_LINES.

              IF LW_LINES IS INITIAL.
                MESSAGE W000(ZMQM) WITH TEXT-E01.
                EXIT.
              ENDIF.

              CHECK LW_LINES = 1.     "/Check single line Selected

              READ TABLE LT_ROWS  INDEX 1 INTO LW_LINE_ROW.

              CHECK NOT LW_LINE_ROW-INDEX IS INITIAL.

              PERFORM RETRIEV_DETAIL_DATA USING LW_LINE_ROW-INDEX.

              CHECK NOT IT_ZSQM_IS_ST_L_D_D[] IS INITIAL.

              WA_LEVEL = C_DETAIL.

              WA_RENEWAL_FLG = C_MARK.
            ENDIF.

          WHEN 'TO_SCHEDULE'.  "/ZRQM04R_INSP_RESULT

            CALL METHOD ALV_GRID->GET_SELECTED_ROWS
                     IMPORTING ET_INDEX_ROWS = LT_ROWS.

            CALL METHOD CL_GUI_CFW=>FLUSH.

            IF SY-SUBRC NE 0.
              WA_REPID = SY-REPID.
              CALL FUNCTION 'POPUP_TO_INFORM'
                   EXPORTING
                        TITEL = WA_REPID
                        TXT2  = SY-SUBRC
                        TXT1  = TEXT-E05.
            ELSE.
              CLEAR LW_LINES.
              DESCRIBE TABLE LT_ROWS LINES LW_LINES.

              IF LW_LINES IS INITIAL.
                MESSAGE W000(ZMQM) WITH TEXT-E01.
                EXIT.
              ENDIF.

              CHECK LW_LINES = 1.     "/Check single line Selected

              READ TABLE LT_ROWS  INDEX 1 INTO LW_LINE_ROW.

              CHECK NOT LW_LINE_ROW-INDEX IS INITIAL.

              PERFORM GET_SELECTED_DATA_FOR_SCH
                                           USING LW_LINE_ROW-INDEX.

              CHECK NOT ZSQM_IS_RES_SEL-ART IS INITIAL.
              IF      ZSQM_IS_ST_SEL-ART = C_INSP_TYPE_MS .
                MESSAGE W000(ZMQM) WITH ZSQM_IS_ST_SEL-ART   ','
                          'MS Inspection type is not supported'(WT1).
                EXIT.
              ENDIF.

              EXPORT ZSQM_IS_RES_SEL FROM ZSQM_IS_RES_SEL TO
              MEMORY ID 'ZSCHEDULE'.

              CALL DIALOG 'ZQM_RQM04' AND SKIP FIRST SCREEN.

              FREE MEMORY ID 'ZSCHEDULE'.

            ENDIF.

          WHEN 'TO_REJECT'.


            CALL METHOD ALV_GRID->GET_SELECTED_ROWS
                     IMPORTING ET_INDEX_ROWS = LT_ROWS.

            CALL METHOD CL_GUI_CFW=>FLUSH.

            IF SY-SUBRC NE 0.
              WA_REPID = SY-REPID.
              CALL FUNCTION 'POPUP_TO_INFORM'
                   EXPORTING
                        TITEL = WA_REPID
                        TXT2  = SY-SUBRC
                        TXT1  = TEXT-E05.
            ELSE.
              CLEAR LW_LINES.
              DESCRIBE TABLE LT_ROWS LINES LW_LINES.

              IF LW_LINES IS INITIAL.
                MESSAGE W000(ZMQM) WITH TEXT-E01.
                EXIT.
              ENDIF.

              CHECK LW_LINES = 1.     "/Check single line Selected

              READ TABLE LT_ROWS  INDEX 1 INTO LW_LINE_ROW.

              CHECK NOT LW_LINE_ROW-INDEX IS INITIAL.

              CLEAR ZSQM_INSP_SEL.
              FREE MEMORY ID 'ZREJECT'.

              PERFORM GET_SELECTED_DATA_FOR_REJ
                                           USING LW_LINE_ROW-INDEX.

*              CHECK NOT ZSQM_INSP_SEL-ART IS INITIAL.
*              IF      ZSQM_INSP_SEL-ART = C_INSP_TYPE_MS .
*                MESSAGE W000(ZMQM) WITH ZSQM_INSP_SEL-ART   ','
*                          'MS Inspection type is not supported'(WT1).
*                EXIT.
*              ENDIF.

              EXPORT ZSQM_INSP_SEL FROM ZSQM_INSP_SEL TO
                                              MEMORY ID 'ZREJECT'.

              CALL DIALOG 'ZQM_RQM11' AND SKIP FIRST SCREEN.

              FREE MEMORY ID 'ZREJECT'.

            ENDIF.

          WHEN OTHERS.
*      N/A
        ENDCASE.

      WHEN OTHERS.

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

  IF     ZSQM_IS_ST_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_IS_ST_SEL-CODE_VH    IS INITIAL.

    MESSAGE E000(ZMQM)
        WITH 'Please Input Vehicle/Engine Code Group.'(E01).

  ENDIF.

  CHECK NOT ZSQM_IS_ST_SEL-CODEGRP_VH IS INITIAL AND
        NOT ZSQM_IS_ST_SEL-CODE_VH    IS INITIAL.


  SELECT SINGLE * INTO LW_ZVQM_VEHICLE
       FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_IS_ST_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_ST_SEL-CODEGRP_VH
           AND CODE       = ZSQM_IS_ST_SEL-CODE_VH.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH
       'Vehicle/Engine type  code group and code are not matched'(E02).
  ENDIF.

ENDMODULE.                 " CHECK_VH_ENG_INPUT  INPUT
*&------------------------------------------------------------------*
*&      Module  SET_LISTBOX_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE SET_LISTBOX_9000 OUTPUT.
  CHECK WA_LISTBOX_SET IS INITIAL.
  WA_LISTBOX_SET = C_MARK.

*- Set List for Selection screen : Inspectio type
  PERFORM SET_LISTBOX_FOR_QPART   USING 'ZSQM_IS_ST_SEL-ART'.


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
*&      Module  GET_TEXT_OF_VEH_ENG_9000  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_OF_VEH_ENG_9000 INPUT.

  CLEAR : ZSQM_IS_ST_SEL-KURZTEXT_G,
          ZSQM_IS_ST_SEL-KURZTEXT_VH.


  IF NOT ZSQM_IS_ST_SEL-CODEGRP_VH IS INITIAL.
    SELECT SINGLE KURZTEXT INTO ZSQM_IS_ST_SEL-KURZTEXT_G
      FROM ZVQM_QPGRT01
         WHERE KATALOGART = ZSQM_IS_ST_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_ST_SEL-CODEGRP_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_ST_SEL-CODEGRP_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_IS_ST_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_IS_ST_SEL-CODE_VH    IS INITIAL.
    SELECT SINGLE KURZTEXT_C INTO ZSQM_IS_ST_SEL-KURZTEXT_VH
      FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_IS_ST_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_ST_SEL-CODEGRP_VH
           AND CODE       = ZSQM_IS_ST_SEL-CODE_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_ST_SEL-CODEGRP_VH
                              ','
                              ZSQM_IS_ST_SEL-CODE_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

ENDMODULE.                 " GET_TEXT_OF_VEH_ENG_9000  INPUT
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
*    - Get inspection type text by user selection
      PERFORM GET_INSPECTION_TYPE_TEXT.
*    - Check plant by Vehicle/Engine type Code Group
      PERFORM CHECK_PLANT_AND_CODE_G.
*    - Set range data by user input for query to DB
      PERFORM SET_RANGE_VALUE.

*--  Get data and set field for lot status of Detail
      PERFORM GET_AND_MAKE_STATUS_CNT.

      CHECK NOT IT_ZSQM_IS_ST_LIST[] IS INITIAL.

      WA_LEVEL = C_BASIC.
      WA_RENEWAL_FLG = C_MARK.

      CALL SCREEN 9100.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.

  CASE ZSQM_IS_ST_SEL-ART.
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

  REFRESH : R_LIFNR, R_CODEGRP_VH, R_WERKS, R_ART, R_VCODE,
            R_CODE_VH, R_EXTWG, R_PASTR, R_PAEND, R_STAT.
  CLEAR   : R_LIFNR, R_CODEGRP_VH, R_WERKS, R_ART, R_VCODE,
            R_CODE_VH, R_EXTWG, R_PASTR, R_PAEND, R_STAT.

  RANGE_MACRO :
     R_WERKS       ZSQM_IS_ST_SEL-WERKS       '',
     R_ART         ZSQM_IS_ST_SEL-ART         '',
     R_LIFNR       ZSQM_IS_ST_SEL-LIFNR_L  ZSQM_IS_ST_SEL-LIFNR_H,
     R_CODEGRP_VH  ZSQM_IS_ST_SEL-CODEGRP_VH  '',
     R_CODE_VH     ZSQM_IS_ST_SEL-CODE_VH     '',
     R_EXTWG       ZSQM_IS_ST_SEL-EXTWG_L  ZSQM_IS_ST_SEL-EXTWG_H,
     R_PASTR       ZSQM_IS_ST_SEL-PASTR_L  ZSQM_IS_ST_SEL-PASTR_H,
     R_PAEND       ZSQM_IS_ST_SEL-PAEND_L  ZSQM_IS_ST_SEL-PAEND_H.

*-- Inspection Lot status value
  RANGE_MACRO :
     R_STAT    C_INSP_STATUS_REL  '',
     R_STAT    C_INSP_STATUS_RREC   '',
     R_STAT    C_INSP_STATUS_UD  ''.

*-- Usage Decision range
  RANGE_MACRO :
   R_VCODE    C_UD_CODE_REJECT    '',
   R_VCODE    C_UD_CODE_MD        '',
   R_VCODE    C_UD_CODE_ACCEPT    ''.

ENDFORM.                    " SET_RANGE_VALUE
*&------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&------------------------------------------------------------------*
FORM GET_DATA_FROM_DB.
  RANGES : LR_PRUEFLOS  FOR QALS-PRUEFLOS.

  DATA : LT_ZSQM_IS_ST_LIST LIKE IT_ZSQM_IS_ST_LIST OCCURS 0
                                                  WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : BEGIN OF LT_QAVE OCCURS 0,
           PRUEFLOS  TYPE QPLOS,
           VCODE     TYPE QVCODE,                           "/01,09,02
         END OF LT_QAVE.

  REFRESH IT_ZSQM_IS_ST_LIST.

  SELECT A~WERK AS WERKS   A~CODEGRP_VH A~CODE_VH A~ART
         C~EXTWG A~MATNR  A~KTEXTMAT  A~PRUEFLOS
         D~KURZTEXT AS KURZTEXT_VH F~STAT
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_IS_ST_LIST
      FROM ( ( QALS AS A  INNER JOIN MARA AS C
         ON   A~MATNR      = C~MATNR    ) INNER JOIN QPCT AS D
         ON   A~KATART_VH  = D~KATALOGART
          AND A~CODEGRP_VH = D~CODEGRUPPE
          AND A~CODE_VH    = D~CODE     ) INNER JOIN JEST AS F
         ON   A~OBJNR      = F~OBJNR
       WHERE  A~ART        IN R_ART
          AND A~WERK       IN R_WERKS
          AND A~SELLIFNR   IN R_LIFNR
          AND A~KATART_VH  = C_VE_ENG_CAT_TYPE
          AND A~CODEGRP_VH IN R_CODEGRP_VH
          AND A~CODE_VH    IN R_CODE_VH
          AND C~EXTWG      IN R_EXTWG
          AND A~PASTRTERM  IN R_PASTR
          AND A~PAENDTERM  IN R_PAEND
          AND D~SPRACHE    = SY-LANGU
          AND D~INAKTIV    = ' '
          AND D~GELOESCHT  = ' '
          AND F~INACT      = ' '
          AND F~STAT      IN R_STAT.


  CHECK SY-SUBRC = 0.

*-  Delete one record of lot have both Released and
*   insp(Results confirmed status
  LT_ZSQM_IS_ST_LIST[] = IT_ZSQM_IS_ST_LIST[].

  LOOP AT LT_ZSQM_IS_ST_LIST WHERE STAT = C_INSP_STATUS_RREC.

    READ TABLE IT_ZSQM_IS_ST_LIST WITH KEY
                            PRUEFLOS = LT_ZSQM_IS_ST_LIST-PRUEFLOS
                            STAT     = C_INSP_STATUS_REL.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      DELETE IT_ZSQM_IS_ST_LIST INDEX LW_INDEX.
    ENDIF.

  ENDLOOP.

  LOOP AT LT_ZSQM_IS_ST_LIST WHERE STAT = C_INSP_STATUS_UD.

    READ TABLE IT_ZSQM_IS_ST_LIST WITH KEY
                            PRUEFLOS = LT_ZSQM_IS_ST_LIST-PRUEFLOS
                            STAT     = C_INSP_STATUS_RREC.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      DELETE IT_ZSQM_IS_ST_LIST INDEX LW_INDEX.
    ENDIF.

  ENDLOOP.


*- Get inspection lot No list for UD(Usage Decision)
  LR_PRUEFLOS-SIGN = 'I'. LR_PRUEFLOS-OPTION = 'EQ'.
  LOOP AT IT_ZSQM_IS_ST_LIST WHERE STAT = C_INSP_STATUS_UD.

    LR_PRUEFLOS-LOW = IT_ZSQM_IS_ST_LIST-PRUEFLOS.
    APPEND LR_PRUEFLOS.
  ENDLOOP.


*-- Get Usage Decision record for merging with Lot List

  SELECT PRUEFLOS  VCODE
     INTO CORRESPONDING FIELDS OF TABLE LT_QAVE
       FROM QAVE
         WHERE PRUEFLOS IN LR_PRUEFLOS
           AND VCODE    IN R_VCODE.

  LOOP AT LT_QAVE.
    CLEAR IT_ZSQM_IS_ST_LIST.
    READ TABLE IT_ZSQM_IS_ST_LIST WITH KEY PRUEFLOS = LT_QAVE-PRUEFLOS.
    IF SY-SUBRC = 0.
      LW_INDEX = SY-TABIX.
      MOVE : LT_QAVE-VCODE  TO IT_ZSQM_IS_ST_LIST-VCODE.
      MODIFY IT_ZSQM_IS_ST_LIST INDEX LW_INDEX.
    ENDIF.
  ENDLOOP.

  CHECK IT_ZSQM_IS_ST_LIST[] IS INITIAL.

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

  DATA : LT_ZSQM_CNT LIKE ZSQM_IS_ST_L_CNT OCCURS 0  WITH HEADER LINE.

  REFRESH IT_ZSQM_IS_ST_L_CNT.

  LOOP AT IT_ZSQM_IS_ST_L_D.
    CLEAR LT_ZSQM_CNT.

    MOVE-CORRESPONDING IT_ZSQM_IS_ST_L_D TO LT_ZSQM_CNT.

    CASE C_MARK.                          "/Status
      WHEN IT_ZSQM_IS_ST_L_D-ST_REL.
        LT_ZSQM_CNT-ST_REL_C = 1.
        LT_ZSQM_CNT-ALL_C    = 1.
      WHEN IT_ZSQM_IS_ST_L_D-ST_INSP.
        LT_ZSQM_CNT-ST_INSP_C = 1.
        LT_ZSQM_CNT-ALL_C     = 1.
      WHEN IT_ZSQM_IS_ST_L_D-ST_UD.
        LT_ZSQM_CNT-ST_UD_C = 1.
        LT_ZSQM_CNT-ALL_C   = 1.

        CASE C_MARK.                          "/Usage Decision
          WHEN IT_ZSQM_IS_ST_L_D-UD_ACPT.
            LT_ZSQM_CNT-UD_ACPT_C = 1.
          WHEN IT_ZSQM_IS_ST_L_D-UD_MD.
            LT_ZSQM_CNT-UD_MD_C   = 1.
          WHEN IT_ZSQM_IS_ST_L_D-UD_REJ.
            LT_ZSQM_CNT-UD_REJ_C  = 1.
        ENDCASE.

      WHEN OTHERS.
    ENDCASE.


    COLLECT LT_ZSQM_CNT.
  ENDLOOP.

*-- Copy LT_ZSQM_CNT to IT_ZSQM_IS_ST_L_CNT.
  LOOP AT LT_ZSQM_CNT.
    CLEAR IT_ZSQM_IS_ST_L_CNT.
    MOVE-CORRESPONDING LT_ZSQM_CNT TO IT_ZSQM_IS_ST_L_CNT.
    APPEND IT_ZSQM_IS_ST_L_CNT.
  ENDLOOP.


**-- Sort
  SORT IT_ZSQM_IS_ST_L_CNT BY WERKS KURZTEXT_VH ART EXTWG ASCENDING.

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
      CASE WA_LEVEL.
        WHEN C_BASIC.
          PERFORM SET_LIST_LEVEL_CONTROL.
          LEAVE TO SCREEN 0.
        WHEN OTHERS.
          PERFORM SET_LIST_LEVEL_CONTROL.
      ENDCASE.

    WHEN 'REFRESH'.

*--       Get data and set field for lot status of Basic
      REFRESH : IT_ZSQM_IS_ST_LIST, IT_ZSQM_IS_ST_L_CNT.

      PERFORM GET_AND_MAKE_STATUS_CNT.

      CHECK NOT IT_ZSQM_IS_ST_L_CNT[] IS INITIAL.

      IF WA_LEVEL = C_DETAIL.

        PERFORM RETRIEV_DETAIL_DATA_REFRESH.

        CHECK NOT IT_ZSQM_IS_ST_L_D_D[] IS INITIAL.

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
    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON   FOR ALV_GRID.
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
  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.    "/Field name for cell color
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
      WA_STRUCTURE_NAME = 'ZSQM_IS_ST_L_CNT'.

    WHEN C_DETAIL.
      WA_STRUCTURE_NAME = 'ZSQM_IS_ST_L_D'.
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
                   IT_OUTTAB        = IT_ZSQM_IS_ST_L_CNT[].


    WHEN C_DETAIL.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING I_STRUCTURE_NAME = WA_STRUCTURE_NAME
                  IS_LAYOUT        = WA_IS_LAYOUT
                  I_SAVE           = WA_SAVE
                  IS_VARIANT       = WA_VARIANT
                  I_DEFAULT        = SPACE
                  IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
        CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                  IT_OUTTAB        = IT_ZSQM_IS_ST_L_D_D[].
  ENDCASE.

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

  REFRESH IT_ZSQM_IS_ST_L_D_D.

  CASE WA_LEVEL .
    WHEN C_BASIC.

      READ TABLE IT_ZSQM_IS_ST_L_CNT  INDEX LW_SEL_INDEX.
      CHECK SY-SUBRC = 0.
      MOVE : IT_ZSQM_IS_ST_L_CNT-EXTWG TO WA_EXTWG.

*-     Backup selected item data for refresh
      MOVE-CORRESPONDING IT_ZSQM_IS_ST_L_CNT TO WA_ST_SEL.

    WHEN OTHERS.
      EXIT.
  ENDCASE.


  LOOP AT IT_ZSQM_IS_ST_L_D
                     WHERE WERKS       = IT_ZSQM_IS_ST_L_CNT-WERKS
                       AND KURZTEXT_VH = IT_ZSQM_IS_ST_L_CNT-KURZTEXT_VH
                       AND ART         = IT_ZSQM_IS_ST_L_CNT-ART
                       AND EXTWG       = WA_EXTWG.
    CLEAR IT_ZSQM_IS_ST_L_D_D.
    MOVE-CORRESPONDING IT_ZSQM_IS_ST_L_D
                           TO IT_ZSQM_IS_ST_L_D_D.
    APPEND IT_ZSQM_IS_ST_L_D_D.
  ENDLOOP.

  SORT IT_ZSQM_IS_ST_L_D_D BY WERKS KURZTEXT_VH ART MATNR
                              KTEXTMAT PRUEFLOS           ASCENDING.

ENDFORM.                    " RETRIEV_DETAIL_DATA
*&------------------------------------------------------------------*
*&      Form  GET_INSPECTION_TYPE_TEXT
*&------------------------------------------------------------------*
FORM GET_INSPECTION_TYPE_TEXT.

  CLEAR ZSQM_IS_ST_SEL-KURZTEXT.

  SELECT SINGLE KURZTEXT INTO ZSQM_IS_ST_SEL-KURZTEXT
     FROM TQ30T
       WHERE SPRACHE = SY-LANGU
         AND ART     = ZSQM_IS_ST_SEL-ART.

ENDFORM.                    " GET_INSPECTION_TYPE_TEXT
*&---------------------------------------------------------------------*
*&      Form  CHECK_PLANT_AND_CODE_G
*&---------------------------------------------------------------------*
FORM CHECK_PLANT_AND_CODE_G.
  CASE ZSQM_IS_ST_SEL-CODEGRP_VH.
    WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type
      IF ZSQM_IS_ST_SEL-WERKS+0(1) = 'P'.
        MESSAGE E000(ZMQM) WITH
         'Plant and Code.G are not matched'(E50).
      ENDIF.
    WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
      IF ZSQM_IS_ST_SEL-WERKS+0(1) = 'E' .
        MESSAGE E000(ZMQM) WITH TEXT-E50.
      ENDIF.
  ENDCASE.

  CLEAR ZSQM_IS_ST_SEL-NAME1.

  SELECT SINGLE NAME1 INTO ZSQM_IS_ST_SEL-NAME1
    FROM T001W
     WHERE WERKS = ZSQM_IS_ST_SEL-WERKS.

ENDFORM.                    " CHECK_PLANT_AND_CODE_G
*&-----------------------------------------------------------------*
*&      Form  COUNT_DATS_FIELD
*&------------------------------------------------------------------*
FORM COUNT_DATS_FIELD USING    PW_FLD_N_IND.
  DATA : LW_FLD_DAT_NAME(40) TYPE C,
         LW_FLD_CNT_NAME(40) TYPE C.

  FIELD-SYMBOLS : <LW_FS_DAT>,
                  <LW_FS_CNT>.
*      - count Start/end date of MIC
  CONCATENATE 'IT_ZSQM_IS_ST_LIST-DATUV_'  PW_FLD_N_IND
                                            INTO LW_FLD_DAT_NAME.
  ASSIGN (LW_FLD_DAT_NAME) TO <LW_FS_DAT>.

  IF NOT <LW_FS_DAT> IS INITIAL.
    CASE ZSQM_IS_ST_SEL-ART.
      WHEN C_INSP_TYPE_ISIR.
        CASE ZSQM_IS_ST_SEL-CODEGRP_VH.
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
  CONCATENATE 'IT_ZSQM_IS_ST_LIST-DATUB_'  PW_FLD_N_IND
                                            INTO LW_FLD_DAT_NAME.
  ASSIGN (LW_FLD_DAT_NAME) TO <LW_FS_DAT>.

  IF NOT <LW_FS_DAT> IS INITIAL.
    CASE ZSQM_IS_ST_SEL-ART.
      WHEN C_INSP_TYPE_ISIR.
        CASE ZSQM_IS_ST_SEL-CODEGRP_VH.
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
*&      Form  SET_FIELD_CAT_ATTR
*&------------------------------------------------------------------*
FORM SET_FIELD_CAT_ATTR TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  CASE WA_LEVEL.
    WHEN C_BASIC.
* Set field attribute
      LOOP AT PT_FIELDCAT.
        CASE PT_FIELDCAT-FIELDNAME.
          WHEN 'WERKS' OR 'KURZTEXT_VH' OR 'ART' OR 'EXTWG'.
            PT_FIELDCAT-KEY_SEL = C_MARK.
            PT_FIELDCAT-KEY     = C_MARK.

            IF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH'. "/Vehicle/Engine
              MOVE  TEXT-TT6 TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
            ENDIF.

          WHEN OTHERS.
            PT_FIELDCAT-DO_SUM    = C_MARK.

            CASE PT_FIELDCAT-FIELDNAME.
              WHEN 'ALL_C'.
                MOVE  'ALL'(TC1)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'ST_REL_C'.
                MOVE  'REL'(TC2)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'ST_INSP_C'.
                MOVE  'INSP'(TC3)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'ST_UD_C'.
                MOVE  'UD'(TC4)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'UD_ACPT_C'.
                MOVE  'Accept'(TC5)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'UD_MD_C'.
                MOVE  'MD'(TC6)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'UD_REJ_C'.
                MOVE  'Reject'(TC7)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
            ENDCASE.
*          -  Set Column Color
            IF PT_FIELDCAT-FIELDNAME+0(3) = 'ST_'.
              PT_FIELDCAT-EMPHASIZE = 'C710'.
            ELSEIF PT_FIELDCAT-FIELDNAME+0(3) = 'UD_'.
              PT_FIELDCAT-EMPHASIZE = 'C301'.
            ENDIF.

        ENDCASE.

        MODIFY PT_FIELDCAT.

      ENDLOOP.

    WHEN C_DETAIL.

      LOOP AT PT_FIELDCAT.
        CASE PT_FIELDCAT-FIELDNAME.
          WHEN 'WERKS' OR 'KURZTEXT_VH' OR 'ART' OR 'MATNR' OR
               'KTEXTMAT' OR 'PRUEFLOS'.
            PT_FIELDCAT-KEY_SEL = C_MARK.
            PT_FIELDCAT-KEY     = C_MARK.

            IF PT_FIELDCAT-FIELDNAME = 'KURZTEXT_VH'. "/Vehicle/Engine
              MOVE  TEXT-TT6 TO : PT_FIELDCAT-COLTEXT,
                                  PT_FIELDCAT-SCRTEXT_L,
                                  PT_FIELDCAT-SCRTEXT_M,
                                  PT_FIELDCAT-SCRTEXT_S.
            ENDIF.

          WHEN 'CODEGRP_VH' OR 'CODE_VH' OR 'EXTWG'.
            PT_FIELDCAT-NO_OUT    = C_MARK.

          WHEN OTHERS.
            PT_FIELDCAT-CHECKBOX = C_MARK.

            CASE PT_FIELDCAT-FIELDNAME.
              WHEN 'ST_REL'.
                MOVE  'REL'(TC2)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'ST_INSP'.
                MOVE  'INSP'(TC3)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'ST_UD'.
                MOVE  'UD'(TC4)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'UD_ACPT'.
                MOVE  'Accept'(TC5)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'UD_MD'.
                MOVE  'MD'(TC6)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
              WHEN 'UD_REJ'.
                MOVE  'Reject'(TC7)
                 TO : PT_FIELDCAT-COLTEXT,   PT_FIELDCAT-SCRTEXT_L,
                      PT_FIELDCAT-SCRTEXT_M, PT_FIELDCAT-SCRTEXT_S.
            ENDCASE.
*          -  Set Column Color
            IF PT_FIELDCAT-FIELDNAME+0(3) = 'ST_'.
              PT_FIELDCAT-EMPHASIZE = 'C710'.
            ELSEIF PT_FIELDCAT-FIELDNAME+0(3) = 'UD_'.
              PT_FIELDCAT-EMPHASIZE = 'C301'.
            ENDIF.
        ENDCASE.

        MODIFY PT_FIELDCAT.

      ENDLOOP.

  ENDCASE.

ENDFORM.                    " SET_FIELD_CAT_ATTR

*&------------------------------------------------------------------*
*&      Form  MAKE_STATUS_DETAIL_LIST
*&------------------------------------------------------------------*
FORM MAKE_STATUS_DETAIL_LIST.

  REFRESH IT_ZSQM_IS_ST_L_D.

  LOOP AT IT_ZSQM_IS_ST_LIST.

    CLEAR IT_ZSQM_IS_ST_L_D.
    MOVE-CORRESPONDING : IT_ZSQM_IS_ST_LIST TO IT_ZSQM_IS_ST_L_D.

    CASE IT_ZSQM_IS_ST_LIST-STAT.
      WHEN C_INSP_STATUS_REL.           "/Released
        IT_ZSQM_IS_ST_L_D-ST_REL = C_MARK.
      WHEN C_INSP_STATUS_RREC.          "/Results confirmed
        IT_ZSQM_IS_ST_L_D-ST_INSP = C_MARK.
      WHEN C_INSP_STATUS_UD.            "/UD has been made
        IT_ZSQM_IS_ST_L_D-ST_UD = C_MARK.

        CASE IT_ZSQM_IS_ST_LIST-VCODE.
          WHEN C_UD_CODE_ACCEPT.            "/Accept
            IT_ZSQM_IS_ST_L_D-UD_ACPT = C_MARK.
          WHEN C_UD_CODE_MD.                "/MD
            IT_ZSQM_IS_ST_L_D-UD_MD  = C_MARK.
          WHEN C_UD_CODE_REJECT.            "/Reject
            IT_ZSQM_IS_ST_L_D-UD_REJ = C_MARK.
        ENDCASE.
    ENDCASE.

    APPEND IT_ZSQM_IS_ST_L_D.

  ENDLOOP.

*-- Fill Vehicl/Engine type Text
  LOOP AT IT_VEH_MAST.
    CLEAR IT_ZSQM_IS_ST_L_D.
    MOVE : IT_VEH_MAST-KURZTEXT_VH TO IT_ZSQM_IS_ST_L_D-KURZTEXT_VH.
    MODIFY IT_ZSQM_IS_ST_L_D TRANSPORTING  KURZTEXT_VH
                            WHERE CODEGRP_VH = IT_VEH_MAST-CODEGRP_VH
                              AND CODE_VH    = IT_VEH_MAST-CODE_VH.
  ENDLOOP.

ENDFORM.                    " MAKE_STATUS_DETAIL_LIST
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
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_PLANT  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_PLANT INPUT.
  CLEAR ZSQM_IS_ST_SEL-NAME1.

  CHECK NOT ZSQM_IS_ST_SEL-WERKS IS INITIAL.

  CALL FUNCTION 'AIPH_GET_TEXT_PLANT'
       EXPORTING
            I_WERKS = ZSQM_IS_ST_SEL-WERKS
       IMPORTING
            E_TEXT  = ZSQM_IS_ST_SEL-NAME1.

ENDMODULE.                 " GET_TEXT_PLANT  INPUT
*&------------------------------------------------------------------*
*&      Form  SET_COLUMN_COLOR
*&------------------------------------------------------------------*
FORM SET_COLUMN_COLOR.
*DATA : LW_LVC_S_SCOL  TYPE LVC_S_SCOL.
*DATA : LW_INDEX LIKE SY-TABIX.
*
*  CASE WA_LEVEL.
*    WHEN C_BASIC.
*
*      LOOP AT IT_ZSQM_IS_ST_L_CNT.
*        LW_INDEX = SY-TABIX.
*
*        LOOP AT IT_FIELDCAT.
*          CLEAR LW_LVC_S_SCOL.
*          IF IT_FIELDCAT-FIELDNAME+0(3) = 'ST_'.
*            MOVE :
*          ENDIF.
*
*        ENDLOOP.
*
*        MODIFY IT_ZSQM_IS_ST_L_CNT INDEX LW_INDEX.
*      ENDLOOP.
*
*    WHEN C_DETAIL.
*
*  ENDCASE.

ENDFORM.                    " SET_COLUMN_COLOR
*&------------------------------------------------------------------*
*&      Module  GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
MODULE GET_TEXT_SCREEN_9000 INPUT.

  CLEAR : ZSQM_IS_ST_SEL-KURZTEXT_G,
          ZSQM_IS_ST_SEL-KURZTEXT_VH.


  IF NOT ZSQM_IS_ST_SEL-CODEGRP_VH IS INITIAL.
    SELECT SINGLE KURZTEXT INTO ZSQM_IS_ST_SEL-KURZTEXT_G
      FROM ZVQM_QPGRT01
         WHERE KATALOGART = ZSQM_IS_ST_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_ST_SEL-CODEGRP_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_ST_SEL-CODEGRP_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

  IF NOT ZSQM_IS_ST_SEL-CODEGRP_VH IS INITIAL AND
     NOT ZSQM_IS_ST_SEL-CODE_VH    IS INITIAL.
    SELECT SINGLE KURZTEXT_C INTO ZSQM_IS_ST_SEL-KURZTEXT_VH
      FROM ZVQM_VEHICLE
         WHERE KATALOGART = ZSQM_IS_ST_SEL-KATART_VH
           AND CODEGRUPPE = ZSQM_IS_ST_SEL-CODEGRP_VH
           AND CODE       = ZSQM_IS_ST_SEL-CODE_VH.

    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMQM) WITH ZSQM_IS_ST_SEL-CODEGRP_VH
                              ','
                              ZSQM_IS_ST_SEL-CODE_VH
                              TEXT-E10.
    ENDIF.

  ENDIF.

ENDMODULE.                 " GET_TEXT_SCREEN_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  GET_SELECTED_DATA_FOR_SCH
*&------------------------------------------------------------------*
FORM GET_SELECTED_DATA_FOR_SCH USING P_INDEX.
  DATA : LW_SEL_INDEX LIKE SY-TABIX.

  LW_SEL_INDEX = P_INDEX.

  CLEAR ZSQM_IS_RES_SEL.

  READ TABLE IT_ZSQM_IS_ST_L_CNT  INDEX LW_SEL_INDEX.

  CHECK SY-SUBRC = 0.

  READ TABLE IT_VEH_MAST WITH KEY
             KURZTEXT_VH = IT_ZSQM_IS_ST_L_CNT-KURZTEXT_VH.

  MOVE-CORRESPONDING IT_ZSQM_IS_ST_L_CNT
                                 TO ZSQM_IS_RES_SEL.
  MOVE-CORRESPONDING IT_VEH_MAST  TO ZSQM_IS_RES_SEL.
  MOVE : C_VE_ENG_CAT_TYPE  TO ZSQM_IS_RES_SEL-KATART_VH.

  FREE MEMORY ID 'ZSCHEDULE'.

  MOVE :
*    ZSQM_IS_ST_SEL-ART         TO ZSQM_IS_RES_SEL-ART,
    IT_ZSQM_IS_ST_L_CNT-EXTWG  TO ZSQM_IS_RES_SEL-EXTWG_L,
    ZSQM_IS_ST_SEL-PASTR_L  TO ZSQM_IS_RES_SEL-PASTR_L,
    ZSQM_IS_ST_SEL-PASTR_H  TO ZSQM_IS_RES_SEL-PASTR_H.

ENDFORM.                    " GET_SELECTED_DATA_FOR_SCH
*&------------------------------------------------------------------*
*&      Form  GET_SELECTED_DATA_FOR_REJ
*&------------------------------------------------------------------*
FORM GET_SELECTED_DATA_FOR_REJ  USING P_INDEX.
  DATA : LW_SEL_INDEX LIKE SY-TABIX.

  LW_SEL_INDEX = P_INDEX.

  CLEAR ZSQM_INSP_SEL.

  READ TABLE IT_ZSQM_IS_ST_L_CNT  INDEX LW_SEL_INDEX.

  CHECK SY-SUBRC = 0.

  READ TABLE IT_VEH_MAST WITH KEY
             KURZTEXT_VH = IT_ZSQM_IS_ST_L_CNT-KURZTEXT_VH.

  MOVE-CORRESPONDING IT_ZSQM_IS_ST_L_CNT TO ZSQM_INSP_SEL.
  MOVE-CORRESPONDING IT_VEH_MAST         TO ZSQM_INSP_SEL.

  MOVE :
    C_VE_ENG_CAT_TYPE         TO ZSQM_INSP_SEL-KATART_VH,
    ZSQM_IS_ST_SEL-LIFNR_L    TO ZSQM_INSP_SEL-LIFNR,
    ZSQM_IS_ST_SEL-PASTR_L    TO ZSQM_INSP_SEL-PASTR_L,
    ZSQM_IS_ST_SEL-PASTR_H    TO ZSQM_INSP_SEL-PASTR_H.

ENDFORM.                    " GET_SELECTED_DATA_FOR_REJ
*&-----------------------------------------------------------------*
*&      Form  GET_AND_MAKE_STATUS_CNT
*&-----------------------------------------------------------------*
FORM GET_AND_MAKE_STATUS_CNT.

*    - Get data from DB
  PERFORM GET_DATA_FROM_DB.

  CHECK NOT IT_ZSQM_IS_ST_LIST[] IS INITIAL.

*    - Set field for lot status of Detail
  PERFORM MAKE_STATUS_DETAIL_LIST.

  PERFORM COUNT_UP_DATA_FOR_BASIC.

ENDFORM.                    " GET_AND_MAKE_STATUS_CNT
*&-----------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA_REFRESH
*&-----------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA_REFRESH.

  REFRESH IT_ZSQM_IS_ST_L_D_D.

  LOOP AT IT_ZSQM_IS_ST_L_D
                     WHERE WERKS       = WA_ST_SEL-WERKS
                       AND KURZTEXT_VH = WA_ST_SEL-KURZTEXT_VH
                       AND ART         = WA_ST_SEL-ART
                       AND EXTWG       = WA_EXTWG.
    CLEAR IT_ZSQM_IS_ST_L_D_D.
    MOVE-CORRESPONDING IT_ZSQM_IS_ST_L_D
                           TO IT_ZSQM_IS_ST_L_D_D.
    APPEND IT_ZSQM_IS_ST_L_D_D.
  ENDLOOP.

  SORT IT_ZSQM_IS_ST_L_D_D BY WERKS KURZTEXT_VH ART MATNR
                              KTEXTMAT PRUEFLOS           ASCENDING.

ENDFORM.                    " RETRIEV_DETAIL_DATA_REFRESH
