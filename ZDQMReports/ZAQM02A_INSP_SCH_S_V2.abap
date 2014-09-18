************************************************************************
* Program Name      : ZAQM02A_INSP_SCH_S_V2
* Author            : SeungLyong, Lee
* Creation Date     : 2003.11.04.
* Specifications By : SeungLyong, Lee
* Pattern           :  3.3 Table Control
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Inspection Scheduling - Raw data Conversion
*  -  Upload Scheduling file(from Excel)
*  -  Manually creation of Inspection lot
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZAQM02A_INSP_SCH_S_V2        NO STANDARD PAGE HEADING  .

*&&& Data Declaration.  &&&*
TYPE-POOLS ZQMT1.  "/QM-Type group for inspection
TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool

TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE : ZQM_INCLUDE_POOL01. "/Inspection Constants and etc
INCLUDE <ICON>.

*//Tables;(TABLES : Table_Name  "//Table Description)
TABLES :
  ZTQM_QNS_HDR,   "/Insp. Scheduling Header : ISIR, Regualr,(MS)
  ZTQM_QNS_ITEM,  "/Insp. Scheduling Item(Material) : ISIR, Regular
  ZTQM_QNS_IT_MS. "/Insp. Scheduling Item(Material) : MS
TABLES :
  ZTQM_QNS_IT_D. "/Insp Scheduling Item(Mat.): earliest day of each item

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSCA_TIME_STAMP.   "/Time Stamp Structre.
TABLES : ZSQM_QNS_HDR.  "/Insp. Scheduling Header : ISIR, Regualr
TABLES : ZSQM_QNS_EX_ISP,  "/Insp. Scheduling (ISIR-P001): Excel Layout
         ZSQM_QNS_EX_ISE,  "/Insp. Scheduling (ISIR-E001): Excel Layout
         ZSQM_QNS_EX_REG. "/Insp. Scheduling (Regular): Excel Layout

TABLES : ZSQM_ALV_DET. "/Detail of ALV : QNS Planned Date

TABLES : ZSQM_QNS_ITEM. "/Insp.ITEM for User Interface and Screen Layout
TABLES : ZSQM_QNS_IT_D. "/for get earliest date of material item

TABLES : ZSQM_QNS_CH_ISP,  "/Insp. Scheduling (ISIR-P001):Change/Display
         ZSQM_QNS_CH_ISE,  "/Insp. Scheduling (ISIR-E001):Change/Display
         ZSQM_QNS_CH_REG. "/Insp. Scheduling (Regular): Change/Display

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
*-- Screen Control Mode
CONSTANTS : C_CREATE(7)  TYPE C VALUE 'CREATE',
            C_CHANGE(7)  TYPE C VALUE 'CHANGE',
            C_DISPLAY(7) TYPE C VALUE 'DISPLAY'.
*-- Process Status
CONSTANTS : C_UPLOADED(8)  TYPE C VALUE 'UPLOADED',
            C_SAVED(8)     TYPE C VALUE 'SAVED'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

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
         REGU   TYPE C  VALUE C_MARK,
         ISIR   TYPE C,
       END OF ST_DIST.

DATA : WA_FILENAME   LIKE RLGRAP-FILENAME .

DATA : WA_SELECTED_INDEX LIKE SY-TABIX. "/Index for selected item


*-- BDC Mode  control
DATA : WA_BDC_MODE TYPE TB_BDCMODE VALUE 'N'.

*//Ranges; (R_)
*RANGES :  "/

*//Internal Tables and Index Fields;(IT_), (I_)
*-- Inspection Scheduling Item(Flat Type) for Excel Upload and T/C
**-      Insp. Scheduling (ISIR-P001): Excel Layout
*DATA:
* IT_ZSQM_QNS_EX_ISP  LIKE ZSQM_QNS_EX_ISP  OCCURS 100 WITH HEADER LINE,
**-      Insp. Scheduling (ISIR-P001): Excel Layout
* IT_ZSQM_QNS_EX_ISE  LIKE ZSQM_QNS_EX_ISE  OCCURS 100 WITH HEADER LINE,
**-      Insp. Scheduling (Regular): Excel Layout
* IT_ZSQM_QNS_EX_REG LIKE ZSQM_QNS_EX_REG OCCURS 100 WITH HEADER LINE.

**-      Insp. Scheduling (ISIR-P001): Excel Layout
DATA : BEGIN OF  IT_ZSQM_QNS_EX_ISP OCCURS 0.
        INCLUDE STRUCTURE ZSQM_QNS_EX_ISP.
DATA  : CELLTAB TYPE LVC_T_STYL,            "
       END OF  IT_ZSQM_QNS_EX_ISP.
**-      Insp. Scheduling (ISIR-P001): Excel Layout
DATA : BEGIN OF  IT_ZSQM_QNS_EX_ISE OCCURS 0.
        INCLUDE STRUCTURE ZSQM_QNS_EX_ISE.
DATA  : CELLTAB TYPE LVC_T_STYL,            "
       END OF  IT_ZSQM_QNS_EX_ISE.
**-      Insp. Scheduling (Regular): Excel Layout
DATA : BEGIN OF  IT_ZSQM_QNS_EX_REG OCCURS 0.
        INCLUDE STRUCTURE ZSQM_QNS_EX_REG.
DATA  : CELLTAB TYPE LVC_T_STYL,            "
       END OF  IT_ZSQM_QNS_EX_REG.

**-      Insp. Scheduling (ISIR-P001): Change/Display
DATA : BEGIN OF  IT_ZSQM_QNS_CH_ISP OCCURS 0.
        INCLUDE STRUCTURE ZSQM_QNS_CH_ISP.
DATA  : CELLTAB TYPE LVC_T_STYL,            "
       END OF  IT_ZSQM_QNS_CH_ISP.
**-      Insp. Scheduling (ISIR-P001): Change/Display
DATA : BEGIN OF  IT_ZSQM_QNS_CH_ISE OCCURS 0.
        INCLUDE STRUCTURE ZSQM_QNS_CH_ISE.
DATA  : CELLTAB TYPE LVC_T_STYL,            "
       END OF  IT_ZSQM_QNS_CH_ISE.
**-      Insp. Scheduling (Regular): Change/Display
DATA : BEGIN OF  IT_ZSQM_QNS_CH_REG OCCURS 0.
        INCLUDE STRUCTURE ZSQM_QNS_CH_REG.
DATA  : CELLTAB TYPE LVC_T_STYL,            "
       END OF  IT_ZSQM_QNS_CH_REG.

**- internal table for Detail ALV
DATA : BEGIN OF  IT_ZSQM_ALV_DET OCCURS 30.
        INCLUDE STRUCTURE ZSQM_ALV_DET.
DATA  : CELLTAB TYPE LVC_T_STYL,            "
       END OF  IT_ZSQM_ALV_DET.


*-- earliest date for item : ZTQM_QNS_IT_D.
DATA : IT_ZTQM_QNS_IT_D LIKE ZTQM_QNS_IT_D OCCURS 0 WITH HEADER LINE.


DATA : IT_ZSQM_QNS_ITEM LIKE ZSQM_QNS_ITEM OCCURS 20 WITH HEADER LINE.

*--//// Internal tables for interface with DB as same as Structure
DATA :
* -   "/Insp. Scheduling Item(Material) : ISIR, Regular
  IT_ZTQM_QNS_ITEM  LIKE ZTQM_QNS_ITEM  OCCURS 100 WITH HEADER LINE,
* -    "/Insp. Scheduling Item(Material) : MS
  IT_ZTQM_QNS_IT_MS LIKE ZTQM_QNS_IT_MS OCCURS 100 WITH HEADER LINE.


*--//// Internal tables for Delete item
DATA :
* -   "/Insp. Scheduling Item(Material) : ISIR, Regular
  IT_ZTQM_DEL_ITEM  LIKE ZTQM_QNS_ITEM  OCCURS 100 WITH HEADER LINE,
* -    "/Insp. Scheduling Item(Material) : MS
  IT_ZTQM_DEL_IT_MS LIKE ZTQM_QNS_IT_MS OCCURS 100 WITH HEADER LINE,
* -  earliest date for item : ZTQM_QNS_IT_D.
  IT_ZTQM_DEL_IT_D LIKE ZTQM_QNS_IT_D OCCURS 0 WITH HEADER LINE.

*--// Message Control table
DATA : IT_MSG_CONTROL LIKE LVC_S_MSG1 OCCURS 10 WITH HEADER LINE.

*--/ Work area structure same as DB structure
* - "/Insp. Scheduling Header : ISIR, Regualr,(MS)
DATA : WA_ZTQM_QNS_HDR  LIKE ZTQM_QNS_HDR.

*/// ALV Control....: Start
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

*// Declare reference variables, the container and internal table
DATA: WA_CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

*DATA : WA_CONTROL_OBJECT TYPE REF TO CL_GUI_CONTROL.

*// Detail ALV Control Objects : 01/27/2004 - sllee
DATA: WA_CUSTOM_C_DET       TYPE   SCRFNAME VALUE 'ALV_DET_CC',
      ALV_GRID_DET          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER_DET    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.


* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED. "/ALV Event Handling

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Global variables for attributes or etc of ALV GRID
DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO. "/The Layout Structure
DATA : IT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_SORT     TYPE LVC_T_SORT WITH HEADER LINE,
       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

*/// ALV Control....: End

***//Macro Definitions
*DEFINE RANGE_MACRO.
*END-OF-DEFINITION.

*// Event Handling(Except Selection Screen (Flow)event)
*Load of Program.
LOAD-OF-PROGRAM.
*-- Set Vehicle/Engine catalog type : 'Q'.
  ZSQM_QNS_HDR-KATART_VH = C_VE_ENG_CAT_TYPE.

**-- Create Message control Class object
*  CREATE OBJECT WA_MSG_CTRL.

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

*    HANDLE_BUTTON_CLICK
*        FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
*               IMPORTING ES_COL_ID
*                         ES_ROW_NO,

*     HANDLE_HOTSPOT_CLICK
*                 FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*                 IMPORTING E_ROW_ID
*                           E_COLUMN_ID,

*    HANDLE_ONF4
*        FOR EVENT ONF4        OF CL_GUI_ALV_GRID
*          IMPORTING E_FIELDNAME
*                    E_FIELDVALUE
*                    ES_ROW_NO
*                    ER_EVENT_DATA
*                    ET_BAD_CELLS
*                    E_DISPLAY,

*-----------------------------------------------------------
*    HANDLE_MENU_BUTTON
*        FOR EVENT MENU_BUTTON OF CL_GUI_ALV_GRID
*           IMPORTING E_OBJECT
*                     E_UCOMM,
*     HANDLE_AFTER_USER_COMMAND
*        FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
*          IMPORTING E_UCOMM
*                    E_NOT_PROCESSED
*                    E_SAVED,

    HANDLE_BEFORE_USER_COMMAND
        FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
          IMPORTING E_UCOMM,
*--------------------------------------------------------
    HANDLE_DATA_CHANGED
        FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
            IMPORTING ER_DATA_CHANGED
                      E_ONF4
                      E_ONF4_BEFORE
                      E_ONF4_AFTER.


  PRIVATE SECTION.

* Methods to modularize event handler method HANDLE_DATA_CHANGED:
* Using Data Change Object CL_ALV_CHANGED_DATA_PROTOCOL
    METHODS: CHECK_MATNR_CHG
     IMPORTING
         PW_MATNR_CHG    TYPE LVC_S_MODI
         PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
         P_EXTWG         TYPE EXTWG
     EXPORTING
         P_ERROR_IN_DATA TYPE C.

ENDCLASS.

* lcl_event_receiver (Definition)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

*  METHOD HANDLE_MENU_BUTTON.
*
*    BREAK-POINT.
*  ENDMETHOD.
*---------------------------------------------------

  METHOD HANDLE_BEFORE_USER_COMMAND.
    DATA : LW_UCOMM LIKE SY-UCOMM.
    LW_UCOMM = E_UCOMM.
*    BREAK-POINT.
*    CALL METHOD ALV_GRID->SET_USER_COMMAND
*                           EXPORTING I_UCOMM = SPACE.

  ENDMETHOD.

*------------------------------------------------

*-- / Double Click
  METHOD HANDLE_DOUBLE_CLICK.
*- read selected row from internal table using INDEX E_ROW-INDEX
*Parameters.
*  E_ROW	Type	LVC_S_ROW		Row ID
*  E_COLUMN	Type	LVC_S_COL		Column Name
*  ES_ROW_NO	Type	LVC_S_ROID		Numeric Row ID
    CHECK WA_MODE NE C_CREATE.

    PERFORM PROC_DOUBLE_CLICK  USING E_COLUMN-FIELDNAME
                                     ES_ROW_NO-ROW_ID.

  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

    CASE WA_MODE.
      WHEN C_CREATE.
*     N/A
      WHEN C_CHANGE. "/Set user command for Create Inspectio Lot
*      -> Delete one row
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE. "/Separator
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to create Inspection Lot for selected item.
        CLEAR LS_TOOLBAR.
        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
        MOVE 'DEL_ROW'(UC3)      TO LS_TOOLBAR-FUNCTION.
        MOVE ICON_DELETE_ROW     TO LS_TOOLBAR-ICON.
        MOVE 'Delete material'(UT3)   TO LS_TOOLBAR-QUICKINFO.
*        MOVE 'Del'(UF3)         TO LS_TOOLBAR-TEXT.
        MOVE ' '                  TO LS_TOOLBAR-DISABLED.

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

*      -> Create Inspection Lot
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE. "/Separator
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to create Inspection Lot for selected item.
        CLEAR LS_TOOLBAR.
        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
        MOVE 'CRT_INSP'(UC1)      TO LS_TOOLBAR-FUNCTION.
        MOVE ICON_INSPECTION_LOT  TO LS_TOOLBAR-ICON.
        MOVE 'Create Inspection Lot'(UT1)   TO LS_TOOLBAR-QUICKINFO.
        MOVE 'Create Inspection'(UF1)       TO LS_TOOLBAR-TEXT.
        MOVE ' '                  TO LS_TOOLBAR-DISABLED.

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

**      -> Change planned date on and MIC
*        CLEAR LS_TOOLBAR.
*        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE. "/Separator
*        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
**         append an icon to create Inspection Lot for selected item.
*        CLEAR LS_TOOLBAR.
*        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
*        MOVE 'CHG_CONT'(UC4)      TO LS_TOOLBAR-FUNCTION.
*        MOVE ICON_CHANGE  TO LS_TOOLBAR-ICON.
*        MOVE 'Ch. Planned date'(UT4)    TO LS_TOOLBAR-QUICKINFO.
*        MOVE 'Change Planned date'(UF4) TO LS_TOOLBAR-TEXT.
*        MOVE ' '                  TO LS_TOOLBAR-DISABLED.
*
*        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

      WHEN C_DISPLAY.
        CLEAR LS_TOOLBAR.
        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE. "/Separator
        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*         append an icon to create Inspection Lot for selected item.
        CLEAR LS_TOOLBAR.
        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
        MOVE 'DISP_INSP'(UC2)      TO LS_TOOLBAR-FUNCTION.
        MOVE ICON_INSPECTION_LOT  TO LS_TOOLBAR-ICON.
        MOVE 'Display Inspection Lot'(UT2)   TO LS_TOOLBAR-QUICKINFO.
        MOVE 'Display Inspection'(UF2)       TO LS_TOOLBAR-TEXT.
        MOVE ' '                  TO LS_TOOLBAR-DISABLED.

        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.
*-------------------------------------------------------------------

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
    DATA : LW_MATERIAL TYPE MATNR.

    WA_RENEWAL_FLG = C_MARK.

    CASE E_UCOMM.
      WHEN TEXT-UC1. "/'CRT_INSP'.Create Inspection Lot

        CALL METHOD ALV_GRID->GET_SELECTED_ROWS
                 IMPORTING ET_INDEX_ROWS = LT_ROWS
                           ET_ROW_NO     = LT_ROW_NO.

        CALL METHOD CL_GUI_CFW=>FLUSH.

        IF SY-SUBRC NE 0.
          WA_REPID = SY-REPID.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    TITEL = WA_REPID
                    TXT2  = SY-SUBRC
                    TXT1  = TEXT-EZZ.
        ELSE.
          DESCRIBE TABLE LT_ROWS LINES LW_LINES.
          CHECK LW_LINES = 1.     "/Check single line Selected

          LOOP AT  LT_ROWS INTO LW_LINE_ROW.
            CLEAR LW_ERROR.
*            - Check whether selected item is newly uploaded item
            PERFORM CHECK_NEW_ITEM_SAVED_TO_DB USING  LW_LINE_ROW-INDEX
                                            CHANGING  LW_ERROR.

            CHECK LW_ERROR IS INITIAL.
            CLEAR LW_ERROR.
            PERFORM CREATE_INSPECTION_LOT  USING    LW_LINE_ROW-INDEX
                                           CHANGING LW_ERROR
                                                    LW_MATERIAL.

            IF NOT LW_ERROR IS INITIAL. "/when it cann't create.
*             Collect error message for disable item.
              PERFORM ADD_MSG_TO_DISPLAY_MSG
                              USING    'E'
                                       'ZMQM'
                                       '000'
                                       LW_MATERIAL
                          'It cannot create inspection lot'(ET7)
                                       'MATNR'
                                      LW_LINE_ROW-INDEX.
              CONTINUE.
            ENDIF.



          ENDLOOP.

          PERFORM CHECK_N_SET_HEADER_STAT.

        ENDIF.

      WHEN TEXT-UC2. "/'DISP_INSP'.Display Inspection Lot

        CALL METHOD ALV_GRID->GET_SELECTED_ROWS
                IMPORTING ET_INDEX_ROWS = LT_ROWS
                          ET_ROW_NO     = LT_ROW_NO.

        CALL METHOD CL_GUI_CFW=>FLUSH.
        IF SY-SUBRC NE 0.
          WA_REPID = SY-REPID.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    TITEL = WA_REPID
                    TXT2  = SY-SUBRC
                    TXT1  = TEXT-EZZ.
        ELSE.
          READ TABLE  LT_ROWS   INDEX 1 INTO LW_LINE_ROW.
          LW_LINES = LW_LINE_ROW-INDEX.

          CHECK NOT LW_LINES IS INITIAL.

          PERFORM PROC_DOUBLE_CLICK  USING 'PRUEFLOS'
                                           LW_LINES .

        ENDIF.

      WHEN TEXT-UC3. "/Delete row : when item status is creation.
        REFRESH IT_MSG_CONTROL.

        CALL METHOD ALV_GRID->GET_SELECTED_ROWS
                 IMPORTING ET_INDEX_ROWS = LT_ROWS
                           ET_ROW_NO     = LT_ROW_NO.

        CALL METHOD CL_GUI_CFW=>FLUSH.

        IF SY-SUBRC NE 0.
          WA_REPID = SY-REPID.
          CALL FUNCTION 'POPUP_TO_INFORM'
               EXPORTING
                    TITEL = WA_REPID
                    TXT2  = SY-SUBRC
                    TXT1  = TEXT-EZZ.
        ELSE.

          DESCRIBE TABLE LT_ROWS LINES LW_LINES.
          CHECK LW_LINES = 1.     "/Check single line Selected

          LOOP AT  LT_ROWS INTO LW_LINE_ROW.
            CLEAR LW_ERROR.

            PERFORM CHECK_DELETE_ROW_ENABLE  USING  LW_LINE_ROW-INDEX
                                          CHANGING LW_ERROR
                                                   LW_MATERIAL.

            IF NOT LW_ERROR IS INITIAL. "/when it cann't delete
*             Collect error message for disable item.
              PERFORM ADD_MSG_TO_DISPLAY_MSG
                              USING    'E'
                                       'ZMQM'
                                       '000'
                                       LW_MATERIAL
                     'It cannot delete. Inspection lot was created'(ET8)
                                       'MATNR'
                                      LW_LINE_ROW-INDEX.
              CONTINUE.

            ELSE.  "/Delete item
              WA_RENEWAL_FLG = C_MARK.
              PERFORM DELETE_ITEM   USING LW_LINE_ROW-INDEX.

            ENDIF.

          ENDLOOP.

        ENDIF.

        PERFORM CHECK_N_SET_HEADER_STAT.

*     WHEN TEXT-UC4. "Change Planned date of MIC,if status is creation.
*        REFRESH IT_MSG_CONTROL.
*
*        CALL METHOD ALV_GRID->GET_SELECTED_ROWS
*                 IMPORTING ET_INDEX_ROWS = LT_ROWS
*                           ET_ROW_NO     = LT_ROW_NO.
*
*        CALL METHOD CL_GUI_CFW=>FLUSH.
*
*        IF SY-SUBRC NE 0.
*          WA_REPID = SY-REPID.
*          CALL FUNCTION 'POPUP_TO_INFORM'
*               EXPORTING
*                    TITEL = WA_REPID
*                    TXT2  = SY-SUBRC
*                    TXT1  = TEXT-EZZ.
*        ELSE.
*          DESCRIBE TABLE LT_ROWS LINES LW_LINES.
*          CHECK LW_LINES = 1.     "/Check single line Selected
*
*          LOOP AT  LT_ROWS INTO LW_LINE_ROW.
*            CLEAR LW_ERROR.
*
*            PERFORM CHANGE_PLANNED_DATE_ITEM  USING  LW_LINE_ROW-INDEX
*                                            CHANGING LW_ERROR
*                                                     LW_MATERIAL.
*
*
*            IF NOT LW_ERROR IS INITIAL. "/when it cann't delete
**             Collect error message for disable item.
*              PERFORM ADD_MSG_TO_DISPLAY_MSG
*                              USING    'E'
*                                       'ZMQM'
*                                       '000'
*                                       LW_MATERIAL
*                 'It cannot be chaged. Inspection lot was created'(ET9)
*                                       'MATNR'
*                                      LW_LINE_ROW-INDEX.
*              CONTINUE.
*
*            ENDIF.
*
*          ENDLOOP.
*
*        ENDIF.

      WHEN OTHERS.
*      N/A
    ENDCASE.

  ENDMETHOD.

  METHOD  HANDLE_DATA_CHANGED.

    DATA: LW_GOOD_CELLS TYPE LVC_S_MODI.

    DATA: LW_ERROR_IN_DATA TYPE C VALUE  SPACE.

    CHECK  ( E_ONF4        = ' ' AND
             E_ONF4_BEFORE = ' ' AND
             E_ONF4_AFTER  = ' '    ) OR
             E_ONF4_AFTER  = C_MARK.

    CALL METHOD ER_DATA_CHANGED->REFRESH_PROTOCOL.

    CASE WA_MODE.
      WHEN C_CREATE.
        LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LW_GOOD_CELLS.
*         check if column MATNR_CHG of this row was changed
          CASE LW_GOOD_CELLS-FIELDNAME.
            WHEN 'MATNR'.
              CALL METHOD CHECK_MATNR_CHG
                     EXPORTING
                        PW_MATNR_CHG      = LW_GOOD_CELLS
                        PR_DATA_CHANGED   = ER_DATA_CHANGED
                        P_EXTWG           = ZSQM_QNS_HDR-EXTWG
                     IMPORTING
                        P_ERROR_IN_DATA   = LW_ERROR_IN_DATA.

              WA_RENEWAL_FLG = C_MARK.

            WHEN OTHERS.

          ENDCASE.
*        IF LW_ERROR_IN_DATA = C_MARK.
*          CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.
*        ENDIF.
        ENDLOOP.

      WHEN C_CHANGE.

        LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LW_GOOD_CELLS.
          CLEAR LW_ERROR_IN_DATA.
*         check if column of this row was changed
          CASE LW_GOOD_CELLS-FIELDNAME.
            WHEN 'MATNR'.
              CALL METHOD CHECK_MATNR_CHG
                     EXPORTING
                        PW_MATNR_CHG      = LW_GOOD_CELLS
                        PR_DATA_CHANGED   = ER_DATA_CHANGED
                        P_EXTWG           = ZSQM_QNS_HDR-EXTWG
                     IMPORTING
                        P_ERROR_IN_DATA   = LW_ERROR_IN_DATA.

              WA_RENEWAL_FLG = C_MARK.

              CHECK LW_ERROR_IN_DATA IS INITIAL.

              PERFORM SET_SCH_STATUS_AND_CHECK  USING LW_GOOD_CELLS.

            WHEN 'I_STAT'.

              PERFORM CHECK_ITEM_STATUS  USING LW_GOOD_CELLS
                                               LW_ERROR_IN_DATA.

              WA_RENEWAL_FLG = C_MARK.

              CHECK NOT LW_ERROR_IN_DATA IS INITIAL.

              PERFORM ADD_MESSAGE_ENTRY
                         USING ER_DATA_CHANGED
                               'E'
                               '000'
                               'ZMQM'
                               LW_GOOD_CELLS-FIELDNAME
                               'It cannot be changed status'(ET6)
                               ''
                               ''
                               LW_GOOD_CELLS-ROW_ID
                               LW_GOOD_CELLS-TABIX.

            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

*-- fill Chage timestamp data.
        IF NOT LW_GOOD_CELLS-ROW_ID IS INITIAL.
          PERFORM FILL_CHANGE_TIME_STAMP USING LW_GOOD_CELLS-ROW_ID.
        ENDIF.

      WHEN C_DISPLAY.
      WHEN OTHERS.
    ENDCASE.

    PERFORM CHECK_N_SET_HEADER_STAT.

*        Display application log if an error has occured.
    CHECK NOT ER_DATA_CHANGED->MT_PROTOCOL IS INITIAL.
    CALL METHOD ER_DATA_CHANGED->DISPLAY_PROTOCOL.

  ENDMETHOD.

*--- Implementation Method in Private Section

  METHOD CHECK_MATNR_CHG.

    DATA : LW_CHG_MATNR TYPE MATNR.
    DATA : LW_SUBRC TYPE C.

* Get new cell value to check it.
    CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING I_ROW_ID =    PW_MATNR_CHG-ROW_ID
                    I_FIELDNAME = PW_MATNR_CHG-FIELDNAME
          IMPORTING E_VALUE     = LW_CHG_MATNR.

    CHECK NOT  LW_CHG_MATNR IS INITIAL.

* Check Material Type('ROH'/'HALB'), ext. material group and Plant
    CLEAR : LW_SUBRC.

    PERFORM CHECK_MATNR    USING LW_CHG_MATNR
                                 ZSQM_QNS_HDR-EXTWG
                                 ZSQM_QNS_HDR-WERKS
                                 LW_SUBRC.

    IF NOT LW_SUBRC IS INITIAL.
      CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
       EXPORTING
          I_MSGID = 'ZMQM' I_MSGNO = '000'  I_MSGTY = LW_SUBRC
          I_MSGV1 = LW_CHG_MATNR
          I_MSGV2 =
    'is not avaiable for inspection or already exist Material'(EM8)
*          I_MSGV3 =
          I_FIELDNAME = PW_MATNR_CHG-FIELDNAME
          I_ROW_ID    = PW_MATNR_CHG-ROW_ID.
* Set Error Indicator "X"
      P_ERROR_IN_DATA = C_MARK.
    ENDIF.

    WA_RENEWAL_FLG = C_MARK.
  ENDMETHOD.

**-- Set listbox for status field
*  METHOD  HANDLE_ONF4.
*    CHECK E_FIELDNAME = 'I_STAT'.
*    PERFORM SET_LISTBOX_FOR_STATUS  TABLES   ET_BAD_CELLS
*                                    USING    "SENDER
*                                             E_FIELDNAME
*                                             E_FIELDVALUE
*                                             ES_ROW_NO
*                                             ER_EVENT_DATA
*                                             E_DISPLAY.
*
*  ENDMETHOD.

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
*&      Module  STATUS_9000  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9000  OUTPUT
*&-----------------------------------------------------------------*
MODULE MODIFY_SCREEN_9000 OUTPUT.
  CHECK ST_DIST-ISIR = C_MARK.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'REG'.
    SCREEN-ACTIVE = 0.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  REFRESH IT_MSG_CONTROL.

  PERFORM EXECUTE_USER_COMMAND_9000.


ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
*&      Form  EXECUTE_USER_COMMAND_9000
*&------------------------------------------------------------------*
FORM EXECUTE_USER_COMMAND_9000.

  DATA : LW_SUBRC LIKE SY-SUBRC.

  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'DUMMY'. "/when Select Radio Button
      CHECK ST_DIST-ISIR = C_MARK.
      CLEAR ZSQM_QNS_HDR-IYEAR.

    WHEN OTHERS.

*--      Assign Inspection type(ISIR, Regular) by user Selection
      CASE C_MARK.
        WHEN ST_DIST-ISIR.
          ZSQM_QNS_HDR-ART = C_INSP_TYPE_ISIR.
        WHEN ST_DIST-REGU.
          ZSQM_QNS_HDR-ART = C_INSP_TYPE_REGULAR.
        WHEN OTHERS.
      ENDCASE.

      IF ZSQM_QNS_HDR-CODEGRP_VH = 'QQVE'.
        ZSQM_QNS_HDR-WERKS = 'P001'.
      ELSEIF ZSQM_QNS_HDR-CODEGRP_VH = 'QQEN'.
        ZSQM_QNS_HDR-WERKS = 'E001'.
      ENDIF.

*-- Get Text for Plant
      PERFORM GET_PLANT_TEXT_AND_CHECK
                                  USING  ZSQM_QNS_HDR-WERKS
                               CHANGING  ZSQM_QNS_HDR-WERKS_NAME.

      WA_RENEWAL_FLG = C_MARK. "/Renewal ALV

      CASE OK_CODE.
        WHEN 'CREATE'.
          WA_MODE = C_CREATE. "/Command Status Set to WA_MODE
          ZSQM_QNS_HDR-H_STAT = C_CREATION. "/First set Status to Creat

*--    Check Exist Data if user click ICON_CREATE on Application toolbar
          PERFORM CHECK_ENABLE_CREATE.

*-     Refresh internal tables for Excel Load File
          REFRESH : IT_ZSQM_QNS_EX_ISP,
                    IT_ZSQM_QNS_EX_ISE,
                    IT_ZSQM_QNS_EX_REG.

          PERFORM SET_EXCLUDING_PF_STATUS.

*-- Call Screen by Inspection type
          CASE C_MARK.
            WHEN ST_DIST-ISIR.
              CALL SCREEN 9100.   "/ISIR Screen
            WHEN ST_DIST-REGU.
              CALL SCREEN 9100.   "/Regular Screen
            WHEN OTHERS.
          ENDCASE.

        WHEN 'CHANGE' OR 'DISPLAY'.
          IF OK_CODE = 'CHANGE'.
            WA_MODE = C_CHANGE.
          ELSE.
            WA_MODE = C_DISPLAY.
          ENDIF.

          PERFORM SET_EXCLUDING_PF_STATUS.

          WA_RENEWAL_FLG = C_MARK. "/Renewal ALV

*-     Refresh internal tables for Excel Load File
          REFRESH : IT_ZSQM_QNS_CH_ISP,
                    IT_ZSQM_QNS_CH_ISE,
                    IT_ZSQM_QNS_CH_REG.
*-     Refresh internal tables for Delete item
          REFRESH : IT_ZTQM_DEL_ITEM,
                    IT_ZTQM_DEL_IT_MS.

          PERFORM READ_N_CHECK_ENABLE_DATA USING LW_SUBRC.
          CHECK LW_SUBRC IS INITIAL.

*-- Remarked by sllee : 2004/01/09 : requested by Mr. Moon. : Start
*          PERFORM GET_EXT_MAT_GRP_TEXT   USING  ZSQM_QNS_HDR-EXTWG
*                                      CHANGING  ZSQM_QNS_HDR-EWBEZ.
*-- Remarked by sllee : 2004/01/09 : requested by Mr. Moon. : End

*-         Get item data by user selection criterion.
          PERFORM GET_ITEM_DATA_FOR_SELECT.

          CALL SCREEN 9100.


        WHEN OTHERS.

      ENDCASE.

  ENDCASE.


ENDFORM.                    " EXECUTE_USER_COMMAND_9000

*&------------------------------------------------------------------*
*&      Module  CHECK_ISIR_REGULAR_9000  INPUT
*&------------------------------------------------------------------*
MODULE CHECK_ISIR_REGULAR_9000 INPUT.

  CHECK NOT ( SY-UCOMM = 'DUMMY' OR SY-UCOMM IS INITIAL ).

  CASE C_MARK.
    WHEN ST_DIST-REGU.
      IF ZSQM_QNS_HDR-KATART_VH  IS INITIAL  OR
         ZSQM_QNS_HDR-CODEGRP_VH IS INITIAL  OR
         ZSQM_QNS_HDR-CODE_VH    IS INITIAL  OR
*         ZSQM_QNS_HDR-WERKS      IS INITIAL  OR
         ZSQM_QNS_HDR-IYEAR      IS INITIAL  .

        MESSAGE E000(ZMQM) WITH 'Please Required Values'(EM1).

      ENDIF.
    WHEN ST_DIST-ISIR.
      IF ZSQM_QNS_HDR-KATART_VH  IS INITIAL  OR
         ZSQM_QNS_HDR-CODEGRP_VH IS INITIAL  OR
         ZSQM_QNS_HDR-CODE_VH    IS INITIAL  .
*         ZSQM_QNS_HDR-WERKS      IS INITIAL .
        MESSAGE E000(ZMQM) WITH TEXT-EM1.
      ENDIF.
  ENDCASE.


ENDMODULE.                 " CHECK_ISIR_REGULAR_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  GET_TEXT_FIELD_VALUE  INPUT
*&-----------------------------------------------------------------*
MODULE GET_TEXT_FIELD_VALUE INPUT.

*-- Get Text for Vehicle/Engine Type
  PERFORM GET_VEHICLE_NAME_AND_CHECK
                             USING  ZSQM_QNS_HDR-KATART_VH
                                    ZSQM_QNS_HDR-CODEGRP_VH
                                    ZSQM_QNS_HDR-CODE_VH
                          CHANGING  ZSQM_QNS_HDR-KURZTEXT_VH.
**-- Get Text for Plant
*  PERFORM GET_PLANT_TEXT_AND_CHECK
*                              USING  ZSQM_QNS_HDR-WERKS
*                           CHANGING  ZSQM_QNS_HDR-WERKS_NAME.

ENDMODULE.                 " GET_TEXT_FIELD_VALUE  INPUT
*&------------------------------------------------------------------*
*&      Form  GET_VEHICLE_NAME_AND_CHECK
*&------------------------------------------------------------------*
FORM GET_VEHICLE_NAME_AND_CHECK USING    P_KATART_VH
                                         P_CODEGRP_VH
                                         P_CODE_VH
                              CHANGING   P_KURZTEXT_VH.
  CLEAR  P_KURZTEXT_VH.

  SELECT SINGLE KURZTEXT_C INTO P_KURZTEXT_VH
     FROM ZVQM_VEHICLE
       WHERE KATALOGART = P_KATART_VH
         AND CODEGRUPPE = P_CODEGRP_VH
         AND CODE       = P_CODE_VH.

  CHECK SY-SUBRC NE 0.
  CHECK NOT ( SY-UCOMM = 'DUMMY' OR SY-UCOMM IS INITIAL ).
  MESSAGE E000(ZMPM) WITH P_CODEGRP_VH
                          P_CODE_VH
                          ',Not Exist Vehicle/Engine Code.'(EM3).

ENDFORM.                    " GET_VEHICLE_NAME_AND_CHECK
*&------------------------------------------------------------------*
*&      Form  GET_PLANT_TEXT_AND_CHECK
*&------------------------------------------------------------------*
FORM GET_PLANT_TEXT_AND_CHECK USING    P_WERKS
                              CHANGING P_NAME.

  CLEAR P_NAME.

  CALL FUNCTION 'AIPH_GET_TEXT_PLANT'
       EXPORTING
            I_WERKS = P_WERKS
       IMPORTING
            E_TEXT  = P_NAME.


  CHECK SY-SUBRC NE 0 OR P_NAME IS INITIAL.
  CHECK NOT ( SY-UCOMM = 'DUMMY' OR SY-UCOMM IS INITIAL ).
  MESSAGE E000(ZMPM) WITH P_WERKS
                         ',Not Exist plant.'(EM4).

ENDFORM.                    " GET_PLANT_TEXT_AND_CHECK
*&------------------------------------------------------------------*
*&      Form  CHECK_ENABLE_CREATE
*&------------------------------------------------------------------*
FORM CHECK_ENABLE_CREATE.
*-- Check Existence of Inspection scheduling
  SELECT SINGLE *
     FROM ZTQM_QNS_HDR
       WHERE KATART_VH  = ZSQM_QNS_HDR-KATART_VH
         AND CODEGRP_VH = ZSQM_QNS_HDR-CODEGRP_VH
         AND CODE_VH    = ZSQM_QNS_HDR-CODE_VH
         AND ART        = ZSQM_QNS_HDR-ART
         AND IYEAR      = ZSQM_QNS_HDR-IYEAR .

  CHECK SY-SUBRC = 0.
  MESSAGE E000(ZMQM) WITH ZSQM_QNS_HDR-CODEGRP_VH
                          ZSQM_QNS_HDR-CODE_VH
                         'is exist in DB, Can''t Create'(EM2).

ENDFORM.                    " CHECK_ENABLE_CREATE
*&------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_9100  OUTPUT
*&------------------------------------------------------------------*
MODULE MODIFY_SCREEN_9100 OUTPUT.
  CHECK ST_DIST-ISIR = C_MARK.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'REG'.
    SCREEN-ACTIVE = 0.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_9100  OUTPUT
*&-----------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&-----------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'RW'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT_9000  INPUT
*&-----------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&-----------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.

  CHECK WA_RENEWAL_FLG = C_MARK.
  CLEAR WA_RENEWAL_FLG.

  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
*- Create Container('GRID_CONTAINER') with Custom Control on screen
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
                TXT1  = 'The control can not be created'(EZY).
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

*-- Set ALV to Ready to Input mode for create and change.
    IF WA_MODE NE C_DISPLAY.
      CALL METHOD ALV_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 1.
    ELSE.
      CALL METHOD ALV_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 0.
    ENDIF.

    IF WA_MODE NE C_DISPLAY. "/Mode = Change or Create
*-- Set data cell attributes using Cell table for create and change
      PERFORM INITIAL_SET_FIELD_STYLE.
    ENDIF.

*-   Set Excluding ALV toolbar Function code.
    PERFORM SET_EXCLUDING_F_CODE  TABLES IT_TOOLBAR_EXCLUDING.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_NEW_TABLE_DATA.

*--  Regist event for Edit
    IF SY-BATCH IS INITIAL.
      CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
          EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.


*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
    CREATE OBJECT EVENT_RECEIVER.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
*-   toolbar control event
    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_ONF4          FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_BUTTON_CLICK FOR ALV_GRID.
    SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR ALV_GRID.

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD ALV_GRID->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.

  ENDIF.

*-- Refresh ALV

  IF NOT GRID_CONTAINER IS INITIAL.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of sflight
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.


*-- Set data cell attributes using Cell table for create and change
    PERFORM INITIAL_SET_FIELD_STYLE.

    IF WA_MODE NE C_DISPLAY.
*-- Set ALV to Ready to Input mode for create and change.
      CALL METHOD ALV_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 1.
    ELSE.
      CALL METHOD ALV_GRID->SET_READY_FOR_INPUT
        EXPORTING
          I_READY_FOR_INPUT = 0.
    ENDIF.

*-   Set Excluding ALV toolbar Function code.
    PERFORM SET_EXCLUDING_F_CODE  TABLES IT_TOOLBAR_EXCLUDING.

*-- Display data on ALV GRID Control using method

    PERFORM SET_NEW_TABLE_DATA.

    PERFORM REFRESH_ALV_GRID_DATA_DISP.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.

  ENDIF.

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  CASE WA_MODE.
    WHEN C_CREATE.
      WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.    "/Style field name set
      WA_IS_LAYOUT-EDIT       = C_MARK.   "/Edit Mode Enable
      WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row
    WHEN C_CHANGE.
      WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.    "/Style field name set
      WA_IS_LAYOUT-EDIT       = C_MARK.   "/Edit Mode Enable
      WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row

      WA_IS_LAYOUT-BOX_FNAME = 'I_STAT'.

    WHEN C_DISPLAY.
*      WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.    "/Style field name set
      WA_IS_LAYOUT-EDIT       = ' '.   "/Edit Mode Enable
      WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row
  ENDCASE.

  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
*  WA_IS_LAYOUT-NO_ROWMARK  = C_MARK.     "/Disable row selections
*  WA_IS_LAYOUT-CWIDTH_OPT = C_MARK.      "/optimizes the column width
*  WA_IS_LAYOUT-DETAILTITL                "/Title bar of detail screen
*                        = 'Detail Title'(T10).
*  WA_IS_LAYOUT-GRID_TITLE                "/ Title bar text
*                    = ''(T11).
*  WA_IS_LAYOUT-KEYHOT    = C_MARK.       "/ Key columns as hotspot
*  WA_IS_LAYOUT-NO_HEADERS  = C_MARK.     "/Hide column headings
*  WA_IS_LAYOUT-NO_HGRIDLN  = C_MARK.     "/Hide horizontal grid lines
*  WA_IS_LAYOUT-NO_VGRIDLN  = C_MARK.     "/Hide vertical grid lines
*  WA_IS_LAYOUT-NO_MERGING  = C_MARK.     "/Disable cell merging
*  WA_IS_LAYOUT-NO_TOOLBAR  = C_MARK.     "/Hide toolbar
  WA_IS_LAYOUT-NUMC_TOTAL  = C_MARK. "/Allow totals for NUMC
*  WA_IS_LAYOUT-S_DRAGDROP  = LW_S_DRAGDROP. "/Drag & Drop control

*  WA_IS_LAYOUT-SGL_CLK_HD = C_MARK. "/sorts the list whe column clicked

*//-- Set Variant Structure
  WA_VARIANT-REPORT = SY-REPID.
  WA_VARIANT-USERNAME = SY-UNAME.


ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.
  DATA : LW_STRUCTURE_NAME LIKE DD02L-TABNAME.


*-- Choose Struvture by Inspection type(ISIR, Regular),
*--  control mode and Plant
  IF WA_MODE = C_CREATE.
    CASE ZSQM_QNS_HDR-ART.
      WHEN C_INSP_TYPE_ISIR.
        IF ZSQM_QNS_HDR-WERKS+0(1)     = 'P'.
          LW_STRUCTURE_NAME = 'ZSQM_QNS_EX_ISP'.
        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
          LW_STRUCTURE_NAME = 'ZSQM_QNS_EX_ISE'.
        ENDIF.
      WHEN C_INSP_TYPE_REGULAR.
        LW_STRUCTURE_NAME = 'ZSQM_QNS_EX_REG'.
      WHEN OTHERS.
    ENDCASE.

  ELSEIF WA_MODE = C_CHANGE OR
         WA_MODE = C_DISPLAY.

    CASE ZSQM_QNS_HDR-ART.
      WHEN C_INSP_TYPE_ISIR.
        IF ZSQM_QNS_HDR-WERKS+0(1)     = 'P'.
          LW_STRUCTURE_NAME = 'ZSQM_QNS_CH_ISP'.
        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
          LW_STRUCTURE_NAME = 'ZSQM_QNS_CH_ISE'.
        ENDIF.
      WHEN C_INSP_TYPE_REGULAR.
        LW_STRUCTURE_NAME = 'ZSQM_QNS_CH_REG'.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = LW_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

* Set field attribute/Text by by Inspection type(ISIR, Regular)
*  PERFORM SET_FIELD_ATTR_FOR_ISIR_P  TABLES PT_FIELDCAT.

  PERFORM SET_FIELD_ATTR_FOR_ALV  TABLES PT_FIELDCAT.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&------------------------------------------------------------------*
FORM SET_NEW_TABLE_DATA.

  IF WA_MODE = C_CREATE.
    CASE ZSQM_QNS_HDR-ART.

      WHEN C_INSP_TYPE_ISIR.
        IF ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
          CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
             EXPORTING I_STRUCTURE_NAME = 'ZSQM_QNS_EX_ISP'
                       IS_LAYOUT        = WA_IS_LAYOUT
                       I_SAVE           = WA_SAVE
                       IS_VARIANT       = WA_VARIANT
                       I_DEFAULT        = SPACE
                       IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
             CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                       IT_OUTTAB        = IT_ZSQM_QNS_EX_ISP[].
        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
          CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
            EXPORTING I_STRUCTURE_NAME = 'ZSQM_QNS_EX_ISE'
                      IS_LAYOUT        = WA_IS_LAYOUT
                      I_SAVE           = WA_SAVE
                      IS_VARIANT       = WA_VARIANT
                      I_DEFAULT        = SPACE
                      IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
            CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                      IT_OUTTAB        = IT_ZSQM_QNS_EX_ISE[].
        ENDIF.

      WHEN C_INSP_TYPE_REGULAR.

        CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSQM_QNS_EX_REG'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = WA_SAVE
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
                     IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                     IT_OUTTAB        = IT_ZSQM_QNS_EX_REG[].
      WHEN OTHERS.
    ENDCASE.

  ELSE.
    CASE ZSQM_QNS_HDR-ART.

      WHEN C_INSP_TYPE_ISIR.
        IF ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
          CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
             EXPORTING I_STRUCTURE_NAME = 'ZSQM_QNS_CH_ISP'
                       IS_LAYOUT        = WA_IS_LAYOUT
                       I_SAVE           = WA_SAVE
                       IS_VARIANT       = WA_VARIANT
                       I_DEFAULT        = SPACE
                       IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
             CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                       IT_OUTTAB        = IT_ZSQM_QNS_CH_ISP[].
        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
          CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
            EXPORTING I_STRUCTURE_NAME = 'ZSQM_QNS_CH_ISE'
                      IS_LAYOUT        = WA_IS_LAYOUT
                      I_SAVE           = WA_SAVE
                      IS_VARIANT       = WA_VARIANT
                      I_DEFAULT        = SPACE
                      IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
            CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                      IT_OUTTAB        = IT_ZSQM_QNS_CH_ISE[].
        ENDIF.

      WHEN C_INSP_TYPE_REGULAR.

        CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSQM_QNS_CH_REG'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = WA_SAVE
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
                     IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                     IT_OUTTAB        = IT_ZSQM_QNS_CH_REG[].
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.                    " SET_NEW_TABLE_DATA
*&----------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&----------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  IF WA_MODE = C_CREATE.
    SET PF-STATUS '9100'.
  ELSEIF WA_MODE = C_CHANGE.
    SET PF-STATUS '9100'." EXCLUDING IT_EX_FUNC.
  ELSE.
    SET PF-STATUS '9100' EXCLUDING IT_EX_FUNC.
  ENDIF.

  SET TITLEBAR  '9100' WITH WA_MODE.

ENDMODULE.                 " STATUS_9100  OUTPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.

  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      CLEAR : ZSQM_QNS_HDR-EXTWG,
              ZSQM_QNS_HDR-EWBEZ.
      REFRESH IT_MSG_CONTROL.
      LEAVE TO SCREEN 0.

    WHEN 'UPLOAD'.

      PERFORM UPLOAD_DATA_FROM_EXCEL.

*      PERFORM CHECK_MATNR_WITH_EXTWG_PLANT.

      PERFORM DISPLAY_PROGRESS_INDICATOR  USING 70
                                          'Converting Excel Data'.

*      PERFORM GET_EXT_MAT_GRP_TEXT   USING  ZSQM_QNS_HDR-EXTWG
*                                  CHANGING  ZSQM_QNS_HDR-EWBEZ.

*-- Check inspection setup of material uploaded using Excel file
      PERFORM CHECK_INSP_SETUP_MATERIAL.

*-- Append material insp. scheduling to itab : change Mode
      IF WA_MODE = C_CHANGE.
        PERFORM MOVE_EXCEL_D_TO_CH_MODE_IT.
      ENDIF.

      WA_STATUS = C_UPLOADED.
      WA_RENEWAL_FLG = C_MARK.

    WHEN 'SAVE'.

      IF    WA_MODE = C_CREATE AND
          ( NOT IT_ZSQM_QNS_EX_ISP[] IS INITIAL OR
            NOT IT_ZSQM_QNS_EX_ISE[] IS INITIAL OR
            NOT IT_ZSQM_QNS_EX_REG[] IS INITIAL ).
        PERFORM MOVE_EXCEL_TO_IT_FOR_DB.
        PERFORM FILL_ETC_FIELD_TO_ITAB.

        PERFORM GET_EARLIST_LATEST_DATE.

        PERFORM SAVE_DATA_TO_DB.

      ELSEIF WA_MODE = C_CHANGE AND      "/item exist
          ( NOT IT_ZSQM_QNS_CH_ISP[] IS INITIAL OR
            NOT IT_ZSQM_QNS_CH_ISE[] IS INITIAL OR
            NOT IT_ZSQM_QNS_CH_REG[] IS INITIAL ).
        PERFORM MOVE_CHDATA_TO_IT_FOR_DB_CH.
        PERFORM FILL_ETC_FIELD_TO_ITAB.

        PERFORM GET_EARLIST_LATEST_DATE.

        PERFORM FILL_IT_FOR_DELETE_ITEM.

        PERFORM SAVE_DATA_TO_DB.

      ELSEIF WA_MODE = C_CHANGE AND
          ( IT_ZSQM_QNS_CH_ISP[] IS INITIAL AND
            IT_ZSQM_QNS_CH_ISE[] IS INITIAL AND
            IT_ZSQM_QNS_CH_REG[] IS INITIAL )..
*          "/All items was deleted by user -> Delete all data from DB

        CLEAR WA_ANSWER.

        PERFORM POP_UP_TO_CONFIRM    USING WA_ANSWER
                                           TEXT-QT2    "/Pop_up Title
                                           TEXT-QT3.   "/Question

        CHECK WA_ANSWER = 'Y'.

        PERFORM DEL_ALL_ITEM_HDR_FROM_DB.

      ENDIF.

    WHEN 'REFRESH'.
      DATA : LW_SUBRC LIKE SY-SUBRC.
*-     Refresh internal tables for Excel Load File
      REFRESH : IT_ZSQM_QNS_CH_ISP,
                IT_ZSQM_QNS_CH_ISE,
                IT_ZSQM_QNS_CH_REG.
*-     Refresh internal tables for Delete item
      REFRESH : IT_ZTQM_DEL_ITEM,
                IT_ZTQM_DEL_IT_MS.

      PERFORM READ_N_CHECK_ENABLE_DATA USING LW_SUBRC.

      CHECK LW_SUBRC IS INITIAL.

*-         Get item data by user selection criterion.
      PERFORM GET_ITEM_DATA_FOR_SELECT.


      WA_RENEWAL_FLG = C_MARK.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9100  INPUT
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
*&-------------------------------------------------------------------*
*&      Form  UPLOAD_DATA_FROM_EXCEL
*&-------------------------------------------------------------------*
FORM UPLOAD_DATA_FROM_EXCEL.
  DATA : LW_Q_TEXT(132) TYPE C.

  DATA : LT_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : LW_START_COL TYPE I VALUE '1',
         LW_START_ROW TYPE I VALUE '1',
         LW_END_COL   TYPE I VALUE '256',
         LW_END_ROW   TYPE I VALUE '65536'.

  DATA : LW_ART TYPE QPART.

  CONCATENATE TEXT-Q01
              ZSQM_QNS_HDR-KURZTEXT_VH
              '?'
           INTO LW_Q_TEXT   SEPARATED BY SPACE.

  CLEAR WA_ANSWER.

  PERFORM POP_UP_TO_CONFIRM    USING WA_ANSWER
                                     TEXT-QT1    "/Pop_up Title
                                     LW_Q_TEXT.   "/Question

  CHECK WA_ANSWER = 'Y'.
  CLEAR WA_FILENAME.

  PERFORM GET_FILE_NAME   USING  WA_FILENAME.

  CHECK NOT WA_FILENAME IS INITIAL.




  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = WA_FILENAME
            I_BEGIN_COL             = LW_START_COL
            I_BEGIN_ROW             = LW_START_ROW
            I_END_COL               = LW_END_COL
            I_END_ROW               = LW_END_ROW
       TABLES
            INTERN                  = LT_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH  TEXT-EZ5. "'File Upload Failed !'.
    STOP.
  ENDIF.

  PERFORM DISPLAY_PROGRESS_INDICATOR  USING 50
                                            'Uploading'.

  CHECK NOT LT_ITAB[] IS INITIAL.

  SORT LT_ITAB BY ROW COL ASCENDING.

*/-- Get External material group from Excel Header field
*--   Row : 1  column : 5
*  READ TABLE LT_ITAB  WITH KEY ROW = 1
*                               COL = 5.
*
*  MOVE : LT_ITAB-VALUE  TO ZSQM_QNS_HDR-EXTWG.

*-- Check Inspection type and Sheet format using A1 Column Value
*--   02/03/2004 - sllee
  READ TABLE LT_ITAB WITH KEY ROW = 1
                              COL = 1.

  IF SY-SUBRC = 0.
    MOVE LT_ITAB-VALUE TO LW_ART.

    IF ZSQM_QNS_HDR-ART NE LW_ART. "/IF Not matched
      MESSAGE E000(ZMQM) WITH
       'Inspection type is not matched.'(E18).
      EXIT.
    ENDIF.

  ELSE.
    MESSAGE E000(ZMQM) WITH
     'Inspection type is required at uploading sheet.'(E19).
    EXIT.
  ENDIF.

*-- Transfer Data to Internal Table from Excel Input Tables
  DELETE LT_ITAB WHERE ROW <= 3. "/Delete Header line(1-3)


*-     Refresh internal tables for Excel Load File
  REFRESH : IT_ZSQM_QNS_EX_ISP,
            IT_ZSQM_QNS_EX_ISE,
            IT_ZSQM_QNS_EX_REG.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        PERFORM MOVE_EXCEL_TO_ITAB  TABLES   IT_ZSQM_QNS_EX_ISP
                                             LT_ITAB.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        PERFORM MOVE_EXCEL_TO_ITAB  TABLES   IT_ZSQM_QNS_EX_ISE
                                             LT_ITAB.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      PERFORM MOVE_EXCEL_TO_ITAB  TABLES   IT_ZSQM_QNS_EX_REG
                                            LT_ITAB.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " UPLOAD_DATA_FROM_EXCEL
*&-----------------------------------------------------------------*
*&      Form  GET_FILE_NAME
*&-----------------------------------------------------------------*
FORM GET_FILE_NAME USING    P_FILENAME.

  DATA : LW_REPID LIKE SY-REPID,
         LW_DYNNR LIKE SY-DYNNR.

  LW_REPID = SY-REPID.
  LW_DYNNR = SY-DYNNR.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            PROGRAM_NAME  = LW_REPID
            DYNPRO_NUMBER = LW_DYNNR
            FIELD_NAME    = ' '
            STATIC        = ' '
            MASK          = '*.xls'
       CHANGING
            FILE_NAME     = P_FILENAME
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " GET_FILE_NAME
*&------------------------------------------------------------------*
*&      Form  POP_UP_TO_CONFIRM
*&------------------------------------------------------------------*
FORM POP_UP_TO_CONFIRM USING    P_ANSWER
                                P_TITLE
                                P_Q_TEXT.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR              = P_TITLE
            TEXT_QUESTION         = P_Q_TEXT
            TEXT_BUTTON_1         = 'Yes'
            ICON_BUTTON_1         = ' '
            TEXT_BUTTON_2         = 'No'
            ICON_BUTTON_2         = ' '
            DEFAULT_BUTTON        = '2'
            DISPLAY_CANCEL_BUTTON = ' '
       IMPORTING
            ANSWER                = P_ANSWER
       EXCEPTIONS
            TEXT_NOT_FOUND        = 1
            OTHERS                = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF P_ANSWER = '1'.
    P_ANSWER = 'Y'.
  ELSEIF P_ANSWER = '2'.
    P_ANSWER = 'N'.
  ENDIF.

ENDFORM.                    " POP_UP_TO_CONFIRM
*&------------------------------------------------------------------*
*&      Form  MOVE_EXCEL_TO_ITAB
*&------------------------------------------------------------------*
FORM MOVE_EXCEL_TO_ITAB
                     TABLES  PT_TABLE
                             PT_ITAB  STRUCTURE ALSMEX_TABLINE.

  DATA : LW_INDEX LIKE SY-TABIX.
  FIELD-SYMBOLS : <LW_FS>.
  DATA : LW_FIELD_TYPE.

  LOOP AT PT_ITAB.
    AT NEW ROW.
      CLEAR PT_TABLE.
    ENDAT.

    MOVE : PT_ITAB-COL TO LW_INDEX.
    ASSIGN COMPONENT LW_INDEX OF STRUCTURE PT_TABLE TO <LW_FS>.

    DESCRIBE FIELD <LW_FS> TYPE LW_FIELD_TYPE.

    IF LW_FIELD_TYPE = 'D'.  "'MM/DD/YYYY"
      CONCATENATE PT_ITAB-VALUE+6(4)    "YEAR  (YYYY)
                  PT_ITAB-VALUE+0(2)    "MONTH (MM)
                  PT_ITAB-VALUE+3(2)    "DAY   (DD)
                              INTO <LW_FS>.
    ELSE.
      TRANSLATE PT_ITAB-VALUE TO UPPER CASE.
      MOVE : PT_ITAB-VALUE TO <LW_FS>.
    ENDIF.
*    ENDIF.
    AT END OF ROW.
      APPEND PT_TABLE.
*      CLEAR PT_TABLE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " MOVE_EXCEL_TO_ITAB
*&------------------------------------------------------------------*
*&      Form  DISPLAY_PROGRESS_INDICATOR
*&------------------------------------------------------------------*
FORM DISPLAY_PROGRESS_INDICATOR USING    VALUE(P_PERCENT)
                                         VALUE(P_TEXT).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = P_PERCENT
            TEXT       = P_TEXT.

ENDFORM.                    " DISPLAY_PROGRESS_INDICATOR
*&------------------------------------------------------------------*
*&      Form  GET_EXT_MAT_GRP_TEXT
*&------------------------------------------------------------------*
FORM GET_EXT_MAT_GRP_TEXT USING    P_EXTWG
                          CHANGING P_EWBEZ.

  CLEAR P_EWBEZ.
  SELECT SINGLE EWBEZ INTO P_EWBEZ
    FROM TWEWT
      WHERE SPRAS = SY-LANGU
        AND EXTWG = P_EXTWG.

  CHECK SY-SUBRC NE 0.
  MESSAGE E000(ZMQM) WITH P_EXTWG
                          ', External Material Group not exist!'(EM5).

ENDFORM.                    " GET_EXT_MAT_GRP_TEXT
*&------------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_FOR_ISIR_P
*&------------------------------------------------------------------*
FORM SET_FIELD_ATTR_FOR_ISIR_P TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.
*-- Set Field attributes of ALV :

  DATA : LW_DIV_FLD(2) TYPE N.
  DATA : LW_TEXT_SYMBOL_NAME(8) TYPE C. "/Text Symbols name var.
  DATA : LW_MIC_CODE(5) TYPE C.

  FIELD-SYMBOLS:<LW_FS>.

*-- Assign text-symbol name to ALV Field Text using field-symbols
*--  by Inspection type and Plant

  LOOP AT PT_FIELDCAT.
    IF PT_FIELDCAT-FIELDNAME = 'MATNR'.
      PT_FIELDCAT-KEY_SEL   = C_MARK.
      PT_FIELDCAT-EMPHASIZE = C_MARK.
      PT_FIELDCAT-KEY       = C_MARK.
      PT_FIELDCAT-OUTPUTLEN = 18.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'I_STAT'.
      PT_FIELDCAT-OUTPUTLEN = 3.
    ENDIF.

    IF PT_FIELDCAT-DATATYPE = 'DATS' AND  "/Date Field FOR MIC
       PT_FIELDCAT-SCRTEXT_L NE 'Created on'.
      LW_DIV_FLD = PT_FIELDCAT-FIELDNAME+7(2). "/DATUV(B)_xNNx

      IF PT_FIELDCAT-FIELDNAME+6(1) = '0'. "ISIR/Regular

*-    assign Text symbols to field symbols by Field name(DATUV(B)_xxxx)
        CONCATENATE 'TEXT-'
                    ZSQM_QNS_HDR-WERKS+0(1)
                    LW_DIV_FLD
                       INTO LW_TEXT_SYMBOL_NAME.
        ASSIGN (LW_TEXT_SYMBOL_NAME) TO <LW_FS>. "/Text Symbols

        CASE ZSQM_QNS_HDR-ART.
          WHEN C_INSP_TYPE_ISIR.
*-         make MIC Code for Field : ISIR
            CONCATENATE   ZSQM_QNS_HDR-WERKS+0(1)
                          'IS'
                          LW_DIV_FLD
                   INTO  LW_MIC_CODE. "/MIC Code : P(E)IS01~22(17).

          WHEN C_INSP_TYPE_REGULAR.
*-         make MIC Code for Field : Regular
            CONCATENATE   ZSQM_QNS_HDR-WERKS+0(1)
                          'RE'
                          LW_DIV_FLD
                   INTO  LW_MIC_CODE. "/MIC Code : P(E)RE01~22(17).
        ENDCASE.

      ELSEIF PT_FIELDCAT-FIELDNAME+6(1) = '1'..  "/MS
        CONCATENATE 'TEXT-'
                    'M'
                    LW_DIV_FLD
                       INTO LW_TEXT_SYMBOL_NAME.
        ASSIGN (LW_TEXT_SYMBOL_NAME) TO <LW_FS>. "/Text Symbols Assign
*-        make MIC Code for Field
        CONCATENATE   ZSQM_QNS_HDR-WERKS+0(1)
                      'MS'
                      LW_DIV_FLD
                        INTO  LW_MIC_CODE. "/MIC Code : PIS01~22.

        MOVE : C_MARK  TO PT_FIELDCAT-EMPHASIZE.

      ENDIF.


      MOVE  <LW_FS> TO : PT_FIELDCAT-COLTEXT,
                         PT_FIELDCAT-SCRTEXT_L.


      MOVE : LW_MIC_CODE  TO PT_FIELDCAT-SCRTEXT_M,
             LW_MIC_CODE  TO PT_FIELDCAT-SCRTEXT_S.

    ENDIF.

    IF PT_FIELDCAT-FIELDNAME = 'ERDAT' OR
       PT_FIELDCAT-FIELDNAME = 'ERNAM' OR
       PT_FIELDCAT-FIELDNAME = 'ERZET' OR
       PT_FIELDCAT-FIELDNAME = 'AEDAT' OR
       PT_FIELDCAT-FIELDNAME = 'AENAM' OR
       PT_FIELDCAT-FIELDNAME = 'AEZET' .
*     N/A
    ENDIF.

    MODIFY PT_FIELDCAT.

  ENDLOOP.

ENDFORM.                    " SET_FIELD_ATTR_FOR_ISIR_P
*&-----------------------------------------------------------------*
*&      Form  MOVE_EXCEL_TO_IT_FOR_DB
*&-----------------------------------------------------------------*
FORM MOVE_EXCEL_TO_IT_FOR_DB.
  CLEAR   : WA_ZTQM_QNS_HDR, IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
  REFRESH : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.

*-- move Header data  from structure to workarea like Header table
  MOVE-CORRESPONDING : ZSQM_QNS_HDR TO WA_ZTQM_QNS_HDR.

*-- move itema data from excel upload table to IT like item table
*-- by Inspection type and plant.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.

      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.

        LOOP AT IT_ZSQM_QNS_EX_ISP.
          CLEAR : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
          MOVE-CORRESPONDING IT_ZSQM_QNS_EX_ISP TO : IT_ZTQM_QNS_ITEM,
                                                     IT_ZTQM_QNS_IT_MS.
          APPEND : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
        ENDLOOP.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.

        LOOP AT IT_ZSQM_QNS_EX_ISE.
          CLEAR : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
          MOVE-CORRESPONDING IT_ZSQM_QNS_EX_ISE TO : IT_ZTQM_QNS_ITEM,
                                                     IT_ZTQM_QNS_IT_MS.
          APPEND : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
        ENDLOOP.
      ENDIF.

    WHEN C_INSP_TYPE_REGULAR.

      LOOP AT IT_ZSQM_QNS_EX_REG.
        CLEAR : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
        MOVE-CORRESPONDING IT_ZSQM_QNS_EX_REG TO : IT_ZTQM_QNS_ITEM,
                                                   IT_ZTQM_QNS_IT_MS.
        APPEND : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " MOVE_EXCEL_TO_IT_FOR_DB
*&-----------------------------------------------------------------*
*&      Form  FILL_ETC_FIELD_TO_ITAB
*&-----------------------------------------------------------------*
FORM FILL_ETC_FIELD_TO_ITAB.
  CLEAR : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.

*-- fill Key field data using header key field data
  MOVE-CORRESPONDING  WA_ZTQM_QNS_HDR TO : IT_ZTQM_QNS_ITEM,
                                           IT_ZTQM_QNS_IT_MS.

*-- fill item status value to I_STAT
*       when creation, C_CREATION
  IF WA_MODE = C_CREATE.
    MOVE : C_CREATION TO IT_ZTQM_QNS_ITEM-I_STAT.
  ENDIF.
*-- fill inspection purpose catalog type and code group
*-- Inspection Purpose Code Group and Code
*--  - ISIR    : KATALOGART = 'P',  CODEGRUPPE = 'QPIS', CODE= *
*--  - REGULAR : KATALOGART = 'P',  CODEGRUPPE = 'QPRE', CODE='01'
*CONSTANTS : C_KATALOGART      TYPE QKATART   VALUE 'P',
*            C_CODEGRUPPE_ISIR TYPE QCODEGRP  VALUE 'QPIS',
*            C_CODEGRUPPE_REGU TYPE QCODEGRP  VALUE 'QPRE',
*            C_CODEGRUPPE_MS   TYPE QCODEGRP  VALUE 'QPMS'.

*   Catalog type(Inspection Purpose) = 'P' fixed value
  MOVE : C_KATALOGART  TO IT_ZTQM_QNS_ITEM-KATART_IP.

*   Code Group(Inspection Purpose) : ISIR(MS), RE(MS)
  CASE ZSQM_QNS_HDR-ART.
    WHEN C_INSP_TYPE_ISIR.
      MOVE : C_CODEGRUPPE_ISIR  TO IT_ZTQM_QNS_ITEM-CODEGR_IP.
    WHEN C_INSP_TYPE_REGULAR.
      MOVE : C_CODEGRUPPE_REGU  TO IT_ZTQM_QNS_ITEM-CODEGR_IP.
  ENDCASE.

*-  fill Inspectio type of MS to MS item table
  MOVE : C_INSP_TYPE_MS  TO IT_ZTQM_QNS_IT_MS-ART_MS.

  IF WA_MODE = C_CREATE.
*-- fill created time stamp information
    MOVE : SY-DATUM TO WA_ZTQM_QNS_HDR-ERDAT,
           SY-DATUM TO IT_ZTQM_QNS_ITEM-ERDAT,
           SY-UZEIT TO WA_ZTQM_QNS_HDR-ERZET,
           SY-UZEIT TO IT_ZTQM_QNS_ITEM-ERZET,
           SY-UNAME TO WA_ZTQM_QNS_HDR-ERNAM,
           SY-UNAME TO IT_ZTQM_QNS_ITEM-ERNAM.
  ENDIF.
*-- Modify table using transporting command
  IF WA_MODE = C_CREATE.
    MODIFY IT_ZTQM_QNS_ITEM TRANSPORTING IYEAR    KATART_VH  CODEGRP_VH
                                         CODE_VH    ART        I_STAT
                                         KATART_IP  CODEGR_IP
                                         ERDAT      ERZET      ERNAM
                               WHERE MATNR NE ''.
  ELSEIF WA_MODE = C_CHANGE.
    MODIFY IT_ZTQM_QNS_ITEM TRANSPORTING IYEAR    KATART_VH  CODEGRP_VH
                                         CODE_VH  ART
                                         KATART_IP  CODEGR_IP
                               WHERE MATNR NE ''.
  ENDIF.

  MODIFY IT_ZTQM_QNS_IT_MS TRANSPORTING IYEAR     KATART_VH  CODEGRP_VH
                                       CODE_VH    ART        ART_MS
                           WHERE MATNR NE ''.

ENDFORM.                    " FILL_ETC_FIELD_TO_ITAB
*&------------------------------------------------------------------*
*&      Form  SAVE_DATA_TO_DB
*&------------------------------------------------------------------*
FORM SAVE_DATA_TO_DB.

  PERFORM ENQUEUE_DB.

  IF WA_MODE = C_CREATE.
    INSERT ZTQM_QNS_HDR FROM WA_ZTQM_QNS_HDR.
  ELSEIF WA_MODE = C_CHANGE.
    MODIFY ZTQM_QNS_HDR FROM WA_ZTQM_QNS_HDR.
  ENDIF.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH 'Error founded Access DB'(EE1) 'ZTQM_QNS_HDR'.
  ENDIF.

*-- Change Header Status for Adding item when Header Status is released
*--  Start : 01/09/2004
  IF WA_ZTQM_QNS_HDR-H_STAT = C_RELEASE.
*      - Check existence of Created Item
    READ TABLE IT_ZTQM_QNS_ITEM WITH KEY I_STAT = C_CREATION.
    IF SY-SUBRC = 0.
      UPDATE ZTQM_QNS_HDR SET H_STAT = C_CREATION
                      WHERE IYEAR      = WA_ZTQM_QNS_HDR-IYEAR
                        AND KATART_VH  = WA_ZTQM_QNS_HDR-KATART_VH
                        AND CODEGRP_VH = WA_ZTQM_QNS_HDR-CODEGRP_VH
                        AND CODE_VH    = WA_ZTQM_QNS_HDR-CODE_VH
                        AND ART        = WA_ZTQM_QNS_HDR-ART.

      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        PERFORM DEQUEUE_DB.
        MESSAGE E000(ZMQM)
              WITH 'Error founded Access DB'(EE1) 'ZTQM_QNS_HDR'.
      ENDIF.
    ENDIF.

  ENDIF.
*--  End


  IF WA_MODE = C_CREATE.
    INSERT ZTQM_QNS_ITEM FROM TABLE IT_ZTQM_QNS_ITEM.
  ELSEIF WA_MODE = C_CHANGE.
    IF NOT IT_ZTQM_DEL_ITEM[] IS INITIAL.
      DELETE ZTQM_QNS_ITEM FROM TABLE IT_ZTQM_DEL_ITEM.
      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        PERFORM DEQUEUE_DB.
        MESSAGE E000(ZMQM)
              WITH TEXT-EE1 'ZTQM_QNS_ITEM'.
      ENDIF.
    ENDIF.
    MODIFY ZTQM_QNS_ITEM FROM TABLE IT_ZTQM_QNS_ITEM.
  ENDIF.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH TEXT-EE1 'ZTQM_QNS_ITEM'.
  ENDIF.

  IF WA_MODE = C_CREATE.
    INSERT ZTQM_QNS_IT_MS FROM TABLE IT_ZTQM_QNS_IT_MS.
  ELSEIF WA_MODE = C_CHANGE.
    IF NOT IT_ZTQM_DEL_IT_MS[] IS INITIAL.
      DELETE ZTQM_QNS_IT_MS FROM TABLE IT_ZTQM_DEL_IT_MS.
      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        PERFORM DEQUEUE_DB.
        MESSAGE E000(ZMQM)
              WITH TEXT-EE1 'ZTQM_QNS_IT_MS'.
      ENDIF.
    ENDIF.
    MODIFY ZTQM_QNS_IT_MS FROM TABLE IT_ZTQM_QNS_IT_MS.
  ENDIF.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH TEXT-EE1 'ZTQM_QNS_IT_MS'.
  ENDIF.


  IF WA_MODE = C_CREATE.
    INSERT ZTQM_QNS_IT_D FROM TABLE IT_ZTQM_QNS_IT_D.
  ELSEIF WA_MODE = C_CHANGE.
    IF NOT IT_ZTQM_DEL_IT_D[] IS INITIAL.
      DELETE ZTQM_QNS_IT_D FROM TABLE IT_ZTQM_DEL_IT_D.
      IF SY-SUBRC NE 0.
        ROLLBACK WORK.
        PERFORM DEQUEUE_DB.
        MESSAGE E000(ZMQM)
              WITH TEXT-EE1 'ZTQM_QNS_IT_D'.
      ENDIF.
    ENDIF.

    MODIFY ZTQM_QNS_IT_D FROM TABLE IT_ZTQM_QNS_IT_D.
  ENDIF.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH TEXT-EE1 'ZTQM_QNS_IT_D'.
  ENDIF.


  CHECK SY-SUBRC = 0.
  COMMIT WORK AND WAIT.
  MESSAGE S000(ZMQM) WITH 'Successfully Saved!'(S01).
  PERFORM DEQUEUE_DB.

  WA_STATUS = C_SAVED.

  LEAVE TO SCREEN 0.

ENDFORM.                    " SAVE_DATA_TO_DB
*&------------------------------------------------------------------*
*&      Form  ENQUEUE_DB
*&------------------------------------------------------------------*
FORM ENQUEUE_DB.
*-- Lock Header table ZTQM_QNS_HDR.
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_QNS_HDR'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*-- Lock Item table ZTQM_QNS_ITEM
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_QNS_ITEM'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART
*            MATNR             =
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*-- Lock Item table ZTQM_QNS_IT_MS
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_QNS_ITMS'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART
*            MATNR             =
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*-- Lock earliest date table ZTQM_QNS_IT_D
  CALL FUNCTION 'ENQUEUE_EZ_ZTQM_QNS_IT_D'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART
*            MATNR             =
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " ENQUEUE_DB
*&------------------------------------------------------------------*
*&      Form  DEQUEUE_DB
*&------------------------------------------------------------------*
FORM DEQUEUE_DB.
*-- Unlock Header table ZTQM_QNS_HDR.
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_QNS_HDR'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART.

*-- Unlock Item table ZTQM_QNS_ITEM
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_QNS_ITEM'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART.
*            MATNR             =.

*-- Unlock Item table ZTQM_QNS_IT_MS
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_QNS_ITMS'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART.
*            MATNR             =.

*-- Unlock earliest date table ZTQM_QNS_IT_D
  CALL FUNCTION 'DEQUEUE_EZ_ZTQM_QNS_IT_D'
       EXPORTING
            MODE_ZTQM_QNS_HDR = 'X'
            MANDT             = SY-MANDT
            IYEAR             = ZSQM_QNS_HDR-IYEAR
            KATART_VH         = ZSQM_QNS_HDR-KATART_VH
            CODEGRP_VH        = ZSQM_QNS_HDR-CODEGRP_VH
            CODE_VH           = ZSQM_QNS_HDR-CODE_VH
            ART               = ZSQM_QNS_HDR-ART.
*            MATNR             =.


ENDFORM.                    " DEQUEUE_DB
*&-----------------------------------------------------------------*
*&      Module  EXIT_9100  INPUT
*&----------------------------------------------------------------*
MODULE EXIT_9100 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'RW'.
      CLEAR : ZSQM_QNS_HDR-EXTWG,
              ZSQM_QNS_HDR-EWBEZ.
      REFRESH IT_MSG_CONTROL.

      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " EXIT_9100  INPUT
*&-----------------------------------------------------------------*
*&      Form  INITIAL_SET_FIELD_STYLE
*&-----------------------------------------------------------------*
FORM INITIAL_SET_FIELD_STYLE.

  DATA : LT_CELL_STYLE_TAB TYPE LVC_T_STYL.
  DATA : LW_LVC_S_STYLE     LIKE LVC_S_STYL.
  DATA : LW_QNS_EX_INDEX LIKE SY-TABIX,
         LW_QNS_CH_INDEX LIKE SY-TABIX.

  DATA : LW_FLD_NAME(40) TYPE C.
  FIELD-SYMBOLS : <LW_FS>.

  IF WA_MODE = C_CREATE.  "//Create Mode

    CASE ZSQM_QNS_HDR-ART.

      WHEN C_INSP_TYPE_ISIR.

        IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.

          LOOP AT IT_ZSQM_QNS_EX_ISP.
            LW_QNS_EX_INDEX = SY-TABIX.
            REFRESH IT_ZSQM_QNS_EX_ISP-CELLTAB.

            LOOP AT IT_FIELDCAT.
              CLEAR LW_LVC_S_STYLE.
              MOVE : IT_FIELDCAT-FIELDNAME TO LW_LVC_S_STYLE-FIELDNAME.
              IF     IT_FIELDCAT-FIELDNAME = 'MATNR' AND
                 NOT IT_ZSQM_QNS_EX_ISP-MATNR IS INITIAL.
                LW_LVC_S_STYLE-STYLE
                                   = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
              ELSEIF IT_FIELDCAT-FIELDNAME = 'ERDAT' OR
                     IT_FIELDCAT-FIELDNAME = 'ERNAM' OR
                     IT_FIELDCAT-FIELDNAME = 'ERZET' OR
                     IT_FIELDCAT-FIELDNAME = 'AEDAT' OR
                     IT_FIELDCAT-FIELDNAME = 'AENAM' OR
                     IT_FIELDCAT-FIELDNAME = 'AEZET' .
                LW_LVC_S_STYLE-STYLE
                                   = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
              ELSE.
               LW_LVC_S_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
              ENDIF.
            INSERT LW_LVC_S_STYLE INTO TABLE IT_ZSQM_QNS_EX_ISP-CELLTAB.
            ENDLOOP.
            MODIFY IT_ZSQM_QNS_EX_ISP INDEX LW_QNS_EX_INDEX.
          ENDLOOP.

        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.

          LOOP AT IT_ZSQM_QNS_EX_ISE.
            LW_QNS_EX_INDEX = SY-TABIX.
            REFRESH IT_ZSQM_QNS_EX_ISE-CELLTAB.
            LOOP AT IT_FIELDCAT.
              CLEAR LW_LVC_S_STYLE.
              MOVE : IT_FIELDCAT-FIELDNAME TO LW_LVC_S_STYLE-FIELDNAME.
              IF     IT_FIELDCAT-FIELDNAME = 'MATNR' AND
                 NOT IT_ZSQM_QNS_EX_ISE-MATNR IS INITIAL.
              LW_LVC_S_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
              ELSEIF IT_FIELDCAT-FIELDNAME = 'ERDAT' OR
                     IT_FIELDCAT-FIELDNAME = 'ERNAM' OR
                     IT_FIELDCAT-FIELDNAME = 'ERZET' OR
                     IT_FIELDCAT-FIELDNAME = 'AEDAT' OR
                     IT_FIELDCAT-FIELDNAME = 'AENAM' OR
                     IT_FIELDCAT-FIELDNAME = 'AEZET' .
              LW_LVC_S_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
              ELSE.
               LW_LVC_S_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
              ENDIF.
            INSERT LW_LVC_S_STYLE INTO TABLE IT_ZSQM_QNS_EX_ISE-CELLTAB.
            ENDLOOP.
            MODIFY IT_ZSQM_QNS_EX_ISE INDEX LW_QNS_EX_INDEX.
          ENDLOOP.
        ENDIF.

      WHEN C_INSP_TYPE_REGULAR.

        LOOP AT IT_ZSQM_QNS_EX_REG.
          LW_QNS_EX_INDEX = SY-TABIX.
          REFRESH IT_ZSQM_QNS_EX_REG-CELLTAB.
          LOOP AT IT_FIELDCAT.
            CLEAR LW_LVC_S_STYLE.
            MOVE : IT_FIELDCAT-FIELDNAME TO LW_LVC_S_STYLE-FIELDNAME.
            IF     IT_FIELDCAT-FIELDNAME = 'MATNR' AND
               NOT IT_ZSQM_QNS_EX_REG-MATNR IS INITIAL.
              LW_LVC_S_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            ELSEIF IT_FIELDCAT-FIELDNAME = 'ERDAT' OR
                   IT_FIELDCAT-FIELDNAME = 'ERNAM' OR
                   IT_FIELDCAT-FIELDNAME = 'ERZET' OR
                   IT_FIELDCAT-FIELDNAME = 'AEDAT' OR
                   IT_FIELDCAT-FIELDNAME = 'AENAM' OR
                   IT_FIELDCAT-FIELDNAME = 'AEZET' .
              LW_LVC_S_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            ELSE.
              LW_LVC_S_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
            ENDIF.
            INSERT LW_LVC_S_STYLE INTO TABLE IT_ZSQM_QNS_EX_REG-CELLTAB.
          ENDLOOP.
          MODIFY IT_ZSQM_QNS_EX_REG INDEX LW_QNS_EX_INDEX.
        ENDLOOP.

    ENDCASE.

*   "//Change Mode
  ELSEIF WA_MODE = C_CHANGE.

    CASE ZSQM_QNS_HDR-ART.

      WHEN C_INSP_TYPE_ISIR.

        IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.

          LOOP AT IT_ZSQM_QNS_CH_ISP.
            LW_QNS_CH_INDEX = SY-TABIX.
            REFRESH IT_ZSQM_QNS_CH_ISP-CELLTAB.

            LOOP AT IT_FIELDCAT.
              CLEAR LW_LVC_S_STYLE.
              MOVE : IT_FIELDCAT-FIELDNAME TO LW_LVC_S_STYLE-FIELDNAME.

              CASE IT_ZSQM_QNS_CH_ISP-I_STAT.
                WHEN C_CREATION.
                  IF     IT_FIELDCAT-FIELDNAME = 'MATNR' AND
                     NOT IT_ZSQM_QNS_CH_ISP-MATNR IS INITIAL.
                    LW_LVC_S_STYLE-STYLE =
                                    CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                  ELSEIF IT_FIELDCAT-FIELDNAME+0(8) = 'PRUEFLOS'.
                    LW_LVC_S_STYLE-STYLE =
                                     CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                  "/End date
                  ELSEIF IT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
                    LW_LVC_S_STYLE-STYLE =
                                    CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                  "/Start date
                  ELSEIF IT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
                    CLEAR  LW_FLD_NAME.
*       If start date is empty, input disable :01/15/2004
                    CONCATENATE 'IT_ZSQM_QNS_CH_ISP-'
                                 IT_FIELDCAT-FIELDNAME
                                              INTO LW_FLD_NAME.
                    ASSIGN (LW_FLD_NAME) TO <LW_FS>.

                    IF <LW_FS> IS INITIAL.
                      LW_LVC_S_STYLE-STYLE =
                                     CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                    ELSE.
                      LW_LVC_S_STYLE-STYLE =
                                      CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
                    ENDIF.

                  ELSEIF IT_FIELDCAT-FIELDNAME = 'ERDAT' OR
                         IT_FIELDCAT-FIELDNAME = 'ERNAM' OR
                         IT_FIELDCAT-FIELDNAME = 'ERZET' OR
                         IT_FIELDCAT-FIELDNAME = 'AEDAT' OR
                         IT_FIELDCAT-FIELDNAME = 'AENAM' OR
                         IT_FIELDCAT-FIELDNAME = 'AEZET' .
                    LW_LVC_S_STYLE-STYLE =
                                     CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                  ENDIF.
                WHEN C_RELEASE.

                  LW_LVC_S_STYLE-STYLE =
                                 CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

                WHEN C_DONTUSE. "/ Don't use status
                  IF     IT_FIELDCAT-FIELDNAME NE 'I_STAT'.
                    LW_LVC_S_STYLE-STYLE =
                                   CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

                  ENDIF.
              ENDCASE.

              INSERT LW_LVC_S_STYLE
                         INTO TABLE IT_ZSQM_QNS_CH_ISP-CELLTAB.
            ENDLOOP.

            MODIFY IT_ZSQM_QNS_CH_ISP INDEX LW_QNS_CH_INDEX.
          ENDLOOP.

        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.

          LOOP AT IT_ZSQM_QNS_CH_ISE.
            LW_QNS_CH_INDEX = SY-TABIX.
            REFRESH IT_ZSQM_QNS_CH_ISE-CELLTAB.

            LOOP AT IT_FIELDCAT.
              CLEAR LW_LVC_S_STYLE.
              MOVE : IT_FIELDCAT-FIELDNAME TO LW_LVC_S_STYLE-FIELDNAME.

              CASE IT_ZSQM_QNS_CH_ISE-I_STAT.
                WHEN C_CREATION.
                  IF     IT_FIELDCAT-FIELDNAME = 'MATNR' AND
                     NOT IT_ZSQM_QNS_CH_ISE-MATNR IS INITIAL.
                    LW_LVC_S_STYLE-STYLE =
                                    CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                  ELSEIF IT_FIELDCAT-FIELDNAME+0(8) = 'PRUEFLOS'.
                    LW_LVC_S_STYLE-STYLE =
                                     CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                  "/End date
                  ELSEIF IT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
                    LW_LVC_S_STYLE-STYLE =
                                    CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                  "/Start date
                  ELSEIF IT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
                    LW_LVC_S_STYLE-STYLE =
                                    CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
                  ELSEIF IT_FIELDCAT-FIELDNAME = 'ERDAT' OR
                         IT_FIELDCAT-FIELDNAME = 'ERNAM' OR
                         IT_FIELDCAT-FIELDNAME = 'ERZET' OR
                         IT_FIELDCAT-FIELDNAME = 'AEDAT' OR
                         IT_FIELDCAT-FIELDNAME = 'AENAM' OR
                         IT_FIELDCAT-FIELDNAME = 'AEZET' .
                    LW_LVC_S_STYLE-STYLE =
                                     CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                  ENDIF.
                WHEN C_RELEASE.
                  LW_LVC_S_STYLE-STYLE =
                                 CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

                WHEN C_DONTUSE. "/ Don't use status
                  IF     IT_FIELDCAT-FIELDNAME NE 'I_STAT'.
                    LW_LVC_S_STYLE-STYLE =
                                   CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

                  ENDIF.
              ENDCASE.

              INSERT LW_LVC_S_STYLE
                         INTO TABLE IT_ZSQM_QNS_CH_ISE-CELLTAB.
            ENDLOOP.

            MODIFY IT_ZSQM_QNS_CH_ISE INDEX LW_QNS_CH_INDEX.
          ENDLOOP.

        ENDIF.

      WHEN C_INSP_TYPE_REGULAR.

        LOOP AT IT_ZSQM_QNS_CH_REG.
          LW_QNS_CH_INDEX = SY-TABIX.
          REFRESH IT_ZSQM_QNS_CH_REG-CELLTAB.

          LOOP AT IT_FIELDCAT.
            CLEAR LW_LVC_S_STYLE.
            MOVE : IT_FIELDCAT-FIELDNAME TO LW_LVC_S_STYLE-FIELDNAME.

            CASE IT_ZSQM_QNS_CH_REG-I_STAT.
              WHEN C_CREATION.
                IF     IT_FIELDCAT-FIELDNAME = 'MATNR' AND
                   NOT IT_ZSQM_QNS_CH_REG-MATNR IS INITIAL.
                  LW_LVC_S_STYLE-STYLE =
                                  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                ELSEIF IT_FIELDCAT-FIELDNAME+0(8) = 'PRUEFLOS'.
                  LW_LVC_S_STYLE-STYLE =
                                   CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                  "/End date
                ELSEIF IT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
                  LW_LVC_S_STYLE-STYLE =
                                  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                  "/Start date
                ELSEIF IT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
                  LW_LVC_S_STYLE-STYLE =
                                  CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
                ELSEIF IT_FIELDCAT-FIELDNAME = 'ERDAT' OR
                       IT_FIELDCAT-FIELDNAME = 'ERNAM' OR
                       IT_FIELDCAT-FIELDNAME = 'ERZET' OR
                       IT_FIELDCAT-FIELDNAME = 'AEDAT' OR
                       IT_FIELDCAT-FIELDNAME = 'AENAM' OR
                       IT_FIELDCAT-FIELDNAME = 'AEZET' .
                  LW_LVC_S_STYLE-STYLE =
                                   CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
                ENDIF.
              WHEN C_RELEASE.
                LW_LVC_S_STYLE-STYLE =
                               CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.

              WHEN C_DONTUSE. "/ Don't use status
                IF     IT_FIELDCAT-FIELDNAME NE 'I_STAT'.
                  LW_LVC_S_STYLE-STYLE =
                                 CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.

                ENDIF.
            ENDCASE.

            INSERT LW_LVC_S_STYLE
                       INTO TABLE IT_ZSQM_QNS_CH_REG-CELLTAB.
          ENDLOOP.

          MODIFY IT_ZSQM_QNS_CH_REG INDEX LW_QNS_CH_INDEX.
        ENDLOOP.

    ENDCASE.

  ELSE.
    CASE ZSQM_QNS_HDR-ART.
      WHEN C_INSP_TYPE_ISIR.
        IF ZSQM_QNS_HDR-WERKS+0(1)     = 'P'.
          LOOP AT IT_ZSQM_QNS_CH_ISP.
            REFRESH IT_ZSQM_QNS_CH_ISP-CELLTAB.
            MODIFY IT_ZSQM_QNS_CH_ISP.
          ENDLOOP.
        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
          LOOP AT IT_ZSQM_QNS_CH_ISE.
            REFRESH IT_ZSQM_QNS_CH_ISE-CELLTAB.
            MODIFY IT_ZSQM_QNS_CH_ISE.
          ENDLOOP.
        ENDIF.
      WHEN C_INSP_TYPE_REGULAR.
        LOOP AT IT_ZSQM_QNS_CH_REG.
          REFRESH IT_ZSQM_QNS_CH_REG-CELLTAB.
          MODIFY IT_ZSQM_QNS_CH_REG.
        ENDLOOP.
      WHEN OTHERS.
    ENDCASE.

  ENDIF.

ENDFORM.                    " INITIAL_SET_FIELD_STYLE
*&------------------------------------------------------------------*
*&      Form  CHECK_MATNR
*&------------------------------------------------------------------*
FORM CHECK_MATNR USING    P_MATNR
                          P_EXTWG
                          P_WERKS
                          P_SUBRC.
  DATA : BEGIN OF LW_MATERIAL,
          MATNR TYPE MATNR,
          MTART TYPE MTART,
          EXTWG TYPE EXTWG,
          WERKS TYPE WERKS_D,
         END OF LW_MATERIAL.

  CHECK NOT P_MATNR IS INITIAL.

*-- Check inspection available
  SELECT SINGLE A~MATNR A~MTART A~EXTWG B~WERKS
      INTO CORRESPONDING FIELDS OF LW_MATERIAL
        FROM ( MARA AS A INNER JOIN MARC AS B
          ON   A~MATNR = B~MATNR ) INNER JOIN QMAT AS C
          ON   A~MATNR = C~MATNR
           AND B~WERKS = C~WERKS
            WHERE A~MATNR = P_MATNR
              AND A~MTART IN ('ROH', 'HALB')
*              AND A~EXTWG = P_EXTWG
              AND B~WERKS = P_WERKS
              AND B~QMATV = C_MARK
              AND C~ART   IN (C_INSP_TYPE_ISIR,
                              C_INSP_TYPE_REGULAR,
                              C_INSP_TYPE_MS ).


  IF SY-SUBRC NE 0.  P_SUBRC = C_MARK. EXIT. ENDIF.


*-- Check material overlapping

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP WITH KEY MATNR = P_MATNR.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE WITH KEY MATNR = P_MATNR.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG WITH KEY MATNR = P_MATNR.
  ENDCASE.

  IF SY-SUBRC = 0.  P_SUBRC = C_MARK. EXIT. ENDIF.

ENDFORM.                    " CHECK_MATNR
*&------------------------------------------------------------------*
*&      Form  CHECK_MATNR_WITH_EXTWG_PLANT
*&------------------------------------------------------------------*
FORM CHECK_MATNR_WITH_EXTWG_PLANT.
  DATA : LW_SUBRC TYPE C.
  DATA : LW_QNS_INDEX LIKE SY-TABIX.

  REFRESH IT_MSG_CONTROL.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.

      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        LOOP AT IT_ZSQM_QNS_EX_ISP.
          LW_QNS_INDEX = SY-TABIX.
          CLEAR LW_SUBRC.
          PERFORM CHECK_MATNR   USING IT_ZSQM_QNS_EX_ISP-MATNR
                                      ZSQM_QNS_HDR-EXTWG
                                      ZSQM_QNS_HDR-WERKS
                                      LW_SUBRC.
          CHECK NOT LW_SUBRC IS INITIAL.

          PERFORM ADD_MSG_TO_DISPLAY_MSG
                      USING   'E'
                            'ZMQM'
                            '000'
                            IT_ZSQM_QNS_EX_ISP-MATNR
                            TEXT-EM8
                            'MATNR'
                            LW_QNS_INDEX.

        ENDLOOP.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        LOOP AT IT_ZSQM_QNS_EX_ISE.
          LW_QNS_INDEX = SY-TABIX.
          CLEAR LW_SUBRC.
          PERFORM CHECK_MATNR   USING IT_ZSQM_QNS_EX_ISE-MATNR
                                      ZSQM_QNS_HDR-EXTWG
                                      ZSQM_QNS_HDR-WERKS
                                      LW_SUBRC.
          CHECK NOT LW_SUBRC IS INITIAL.

          PERFORM ADD_MSG_TO_DISPLAY_MSG
                      USING   'E'
                            'ZMQM'
                            '000'
                            IT_ZSQM_QNS_EX_ISE-MATNR
                            TEXT-EM8
                            'MATNR'
                            LW_QNS_INDEX.
        ENDLOOP.
      ENDIF.

    WHEN C_INSP_TYPE_REGULAR.
      LOOP AT IT_ZSQM_QNS_EX_REG.
        LW_QNS_INDEX = SY-TABIX.
        CLEAR LW_SUBRC.
        PERFORM CHECK_MATNR   USING IT_ZSQM_QNS_EX_REG-MATNR
                                    ZSQM_QNS_HDR-EXTWG
                                    ZSQM_QNS_HDR-WERKS
                                    LW_SUBRC.
        CHECK NOT LW_SUBRC IS INITIAL.

        PERFORM ADD_MSG_TO_DISPLAY_MSG
                    USING   'E'
                          'ZMQM'
                          '000'
                          IT_ZSQM_QNS_EX_REG-MATNR
                          TEXT-EM8
                          'MATNR'
                          LW_QNS_INDEX.
      ENDLOOP.
  ENDCASE.


ENDFORM.                    " CHECK_MATNR_WITH_EXTWG_PLANT
*&---------------------------------------------------------------------*
*&      Form  ADD_MESSAGE_TO_PROTOCOL
*&---------------------------------------------------------------------*
FORM ADD_MESSAGE_TO_PROTOCOL
         USING
               PO_MSG_CONTROL
                              TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                  VALUE(P_MSGTY)
                  VALUE(P_MSGID)
                  VALUE(P_MSGNO)
                  P_DATA
                  P_MESSAGE
                  P_FIELDNAME
                  P_INDEX.

  CALL METHOD PO_MSG_CONTROL->ADD_PROTOCOL_ENTRY
     EXPORTING
        I_MSGID = P_MSGID I_MSGNO = P_MSGNO  I_MSGTY = P_MSGTY
        I_MSGV1 = P_DATA
        I_MSGV2 = P_MESSAGE
*         I_MSGV3 =
        I_FIELDNAME = P_FIELDNAME
        I_ROW_ID = P_INDEX.

ENDFORM.                    " ADD_MESSAGE_TO_PROTOCOL
*&------------------------------------------------------------------*
*&      Module  DISPLAY_MSG_OBJECT  OUTPUT
*&------------------------------------------------------------------*
MODULE DISPLAY_MSG_OBJECT OUTPUT.

  CHECK NOT IT_MSG_CONTROL[] IS INITIAL.

  PERFORM DISPLAY_MESSAGE_TABLE  TABLES IT_MSG_CONTROL.

  REFRESH IT_MSG_CONTROL.

ENDMODULE.                 " DISPLAY_MSG_OBJECT  OUTPUT
*&----------------------------------------------------------------*
*&      Module  DISPLAY_MSG_OBJECT_9110  OUTPUT
*&----------------------------------------------------------------*
MODULE DISPLAY_MSG_OBJECT_9110 OUTPUT.

*  IF MSG_CONTAINER  IS INITIAL. "/Not Created Control
*
*    CREATE OBJECT MSG_CONTAINER
*           EXPORTING CONTAINER_NAME = WA_MSG_CONTROL
*           EXCEPTIONS
*            CNTL_ERROR = 1
*            CNTL_SYSTEM_ERROR = 2
*            CREATE_ERROR = 3
*            LIFETIME_ERROR = 4
*            LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*  ENDIF.
*
*  CALL METHOD WA_MSG_CTRL->DISPLAY_PROTOCOL
*          EXPORTING
*            I_CONTAINER = MSG_CONTAINER
*            .

ENDMODULE.                 " DISPLAY_MSG_OBJECT_9110  OUTPUT
*&------------------------------------------------------------------*
*&      Module  STATUS_9110  OUTPUT
*&------------------------------------------------------------------*
MODULE STATUS_9110 OUTPUT.
  SET PF-STATUS '9110'.
  SET TITLEBAR '9110'.

ENDMODULE.                 " STATUS_9110  OUTPUT
*&------------------------------------------------------------------*
*&      Form  ADD_MSG_TO_DISPLAY_MSG
*&------------------------------------------------------------------*
FORM ADD_MSG_TO_DISPLAY_MSG USING    VALUE(P_MSGTY)
                                     VALUE(P_MSGID)
                                     VALUE(P_MSGNO)
                                     P_DATA
                                     P_MESSAGE
                                     P_FIELDNAME
                                     P_INDEX.

  CLEAR IT_MSG_CONTROL.
  CONCATENATE   P_DATA
                ':'
                P_MESSAGE
                  INTO IT_MSG_CONTROL-MSGV1.

  MOVE : P_MSGTY TO IT_MSG_CONTROL-MSGTY,
         'Material' TO IT_MSG_CONTROL-MSGID,
*         P_MSGID TO IT_MSG_CONTROL-MSGID,
         P_MSGNO TO IT_MSG_CONTROL-MSGNO,
*         P_DATA  TO IT_MSG_CONTROL-MSGV1,
*         P_MESSAGE TO IT_MSG_CONTROL-MSGV2,
*         P_ TO IT_MSG_CONTROL-MSGV3,
*         P_ TO IT_MSG_CONTROL-MSGV4,
         P_FIELDNAME TO IT_MSG_CONTROL-FIELDNAME,
         P_INDEX TO IT_MSG_CONTROL-ROW_ID.
  APPEND IT_MSG_CONTROL.

ENDFORM.                    " ADD_MSG_TO_DISPLAY_MSG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE_TABLE
*&---------------------------------------------------------------------*
FORM DISPLAY_MESSAGE_TABLE TABLES   PT_MSG_TABLE .

  CALL FUNCTION 'OG_POPUP_WITH_TABLE_DISPLAY'
       EXPORTING
            I_ENDPOS_COL   = 100
            I_ENDPOS_ROW   = 15
            I_STARTPOS_COL = 5
            I_STARTPOS_ROW = 5
            I_TITLETEXT    = 'Inconsistent Message Log'(ET5)
            I_SELECTION_ACTIVE  = ' '
            I_HEADERTEXT   = ''
*       IMPORTING
*            E_CHOISE       =
*            E_SY_SUBRC     =
       TABLES
            T_VALUETAB     = PT_MSG_TABLE.

ENDFORM.                    " DISPLAY_MESSAGE_TABLE
*&------------------------------------------------------------------*
*&      Form  READ_N_CHECK_ENABLE_DATA
*&------------------------------------------------------------------*
FORM READ_N_CHECK_ENABLE_DATA  USING P_SUBRC.
  CLEAR P_SUBRC.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ZSQM_QNS_HDR
     FROM ZTQM_QNS_HDR
       WHERE IYEAR      = ZSQM_QNS_HDR-IYEAR
         AND KATART_VH  = ZSQM_QNS_HDR-KATART_VH
         AND CODEGRP_VH = ZSQM_QNS_HDR-CODEGRP_VH
         AND CODE_VH    = ZSQM_QNS_HDR-CODE_VH
         AND ART        = ZSQM_QNS_HDR-ART.

  CHECK SY-SUBRC NE 0.
  P_SUBRC = SY-SUBRC.

  MESSAGE E000(ZMQM) WITH  'Data not exist!'(ET0).

ENDFORM.                    " READ_N_CHECK_ENABLE_DATA
*&-----------------------------------------------------------------*
*&      Form  GET_ITEM_DATA_FOR_SELECT
*&-----------------------------------------------------------------*
FORM GET_ITEM_DATA_FOR_SELECT.

*- Get item data into IT_ZSQM_QNS_ITEM  and copy to data to
*  IT_ZSQM_QNS_CH_ISP, IT_ZSQM_QNS_CH_ISE OR IT_ZSQM_QNS_CH_REG
*  by Inspection type. If you try to patch directly into them,
*   short dump will be happen.

  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QNS_ITEM
   FROM ( ZTQM_QNS_HDR AS A INNER JOIN ZTQM_QNS_ITEM AS B
      ON   A~IYEAR      = B~IYEAR
       AND A~KATART_VH  = B~KATART_VH
       AND A~CODEGRP_VH = B~CODEGRP_VH
       AND A~CODE_VH    = B~CODE_VH
       AND A~ART        = B~ART  ) INNER JOIN ZTQM_QNS_IT_MS AS C
      ON   B~IYEAR      = C~IYEAR
       AND B~KATART_VH  = C~KATART_VH
       AND B~CODEGRP_VH = C~CODEGRP_VH
       AND B~CODE_VH    = C~CODE_VH
       AND B~ART        = C~ART
       AND B~MATNR      = C~MATNR
     WHERE  A~IYEAR      = ZSQM_QNS_HDR-IYEAR
        AND A~KATART_VH  = ZSQM_QNS_HDR-KATART_VH
        AND A~CODEGRP_VH = ZSQM_QNS_HDR-CODEGRP_VH
        AND A~CODE_VH    = ZSQM_QNS_HDR-CODE_VH
        AND A~ART        = ZSQM_QNS_HDR-ART.

  CHECK SY-SUBRC = 0.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.

        LOOP AT IT_ZSQM_QNS_ITEM.
          CLEAR IT_ZSQM_QNS_CH_ISP.
          MOVE-CORRESPONDING IT_ZSQM_QNS_ITEM TO IT_ZSQM_QNS_CH_ISP.
          APPEND IT_ZSQM_QNS_CH_ISP.
        ENDLOOP.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        LOOP AT IT_ZSQM_QNS_ITEM.
          CLEAR IT_ZSQM_QNS_CH_ISE.
          MOVE-CORRESPONDING IT_ZSQM_QNS_ITEM TO IT_ZSQM_QNS_CH_ISE.
          APPEND IT_ZSQM_QNS_CH_ISE.
        ENDLOOP.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      LOOP AT IT_ZSQM_QNS_ITEM.
        CLEAR IT_ZSQM_QNS_CH_REG.
        MOVE-CORRESPONDING IT_ZSQM_QNS_ITEM TO IT_ZSQM_QNS_CH_REG.
        APPEND IT_ZSQM_QNS_CH_REG.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    " GET_ITEM_DATA_FOR_SELECT
*&------------------------------------------------------------------*
*&      Form  SET_LISTBOX_FOR_STATUS
*&------------------------------------------------------------------*
FORM SET_LISTBOX_FOR_STATUS
                TABLES PET_BAD_CELLS TYPE LVC_T_MODI
                USING  "P_SENDER TYPE REF TO cl_gui_alv_grid
                       PE_FIELDNAME    TYPE LVC_FNAME
                       PE_FIELDVALUE   TYPE LVC_VALUE
                       PES_ROW_NO      TYPE LVC_S_ROID
                       PER_EVENT_DATA  TYPE REF TO CL_ALV_EVENT_DATA
                       PE_DISPLAY      TYPE CHAR01.

ENDFORM.                    " SET_LISTBOX_FOR_STATUS
*&------------------------------------------------------------------*
*&      Form  CHECK_ITEM_STATUS
*&------------------------------------------------------------------*
FORM CHECK_ITEM_STATUS USING    PW_GOOD_CELLS TYPE LVC_S_MODI
                                PW_ERROR.

  DATA : LW_ZSQM_QNS_ITEM LIKE ZSQM_QNS_ITEM.
  DATA : LW_I_STAT         TYPE ZQINSPSTATUS.

  CHECK  PW_GOOD_CELLS-ERROR IS INITIAL."No error during changing value

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX PW_GOOD_CELLS-ROW_ID.
        MOVE-CORRESPONDING IT_ZSQM_QNS_CH_ISP  TO LW_ZSQM_QNS_ITEM.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX PW_GOOD_CELLS-ROW_ID.
        MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_ISE TO LW_ZSQM_QNS_ITEM.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX PW_GOOD_CELLS-ROW_ID.
      MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_REG TO LW_ZSQM_QNS_ITEM.
  ENDCASE.

  MOVE : PW_GOOD_CELLS-VALUE TO LW_I_STAT.

  IF LW_ZSQM_QNS_ITEM-I_STAT = C_CREATION.
    CHECK LW_I_STAT = C_RELEASE.
    PW_ERROR = C_MARK.
  ELSEIF LW_ZSQM_QNS_ITEM-I_STAT = C_RELEASE.
    PW_ERROR = C_MARK.
  ELSEIF LW_ZSQM_QNS_ITEM-I_STAT = C_DONTUSE.
    CHECK LW_I_STAT = C_RELEASE.
    PW_ERROR = C_MARK.
  ELSE.
  ENDIF.

  CHECK PW_ERROR IS INITIAL.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX PW_GOOD_CELLS-ROW_ID.
        MOVE-CORRESPONDING IT_ZSQM_QNS_CH_ISP  TO LW_ZSQM_QNS_ITEM.
        PERFORM CHANGE_LOG_INFO_SET   USING PW_GOOD_CELLS-ROW_ID
                                            IT_ZSQM_QNS_CH_ISP-MATNR.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX PW_GOOD_CELLS-ROW_ID.
        MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_ISE TO LW_ZSQM_QNS_ITEM.
        PERFORM CHANGE_LOG_INFO_SET   USING PW_GOOD_CELLS-ROW_ID
                                            IT_ZSQM_QNS_CH_ISE-MATNR.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX PW_GOOD_CELLS-ROW_ID.
      MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_REG TO LW_ZSQM_QNS_ITEM.
      PERFORM CHANGE_LOG_INFO_SET   USING PW_GOOD_CELLS-ROW_ID
                                          IT_ZSQM_QNS_CH_REG-MATNR.
  ENDCASE.


ENDFORM.                    " CHECK_ITEM_STATUS
*&------------------------------------------------------------------*
*&      Form  ADD_MESSAGE_ENTRY
*&------------------------------------------------------------------*
FORM ADD_MESSAGE_ENTRY
     USING   PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
            VALUE(P_MSGTY)
            VALUE(P_MSGNO)
            VALUE(P_MSGID)
            P_FIELDNAME
            VALUE(P_MSGV1)
            VALUE(P_MSGV2)
            VALUE(P_MSGV3)
            P_ROW_ID
            P_TABIX.

  CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
          EXPORTING
              I_MSGID     = P_MSGID
              I_MSGTY     = P_MSGTY
              I_MSGNO     = P_MSGNO
              I_MSGV1     = P_MSGV1
              I_MSGV2     = P_MSGV2
              I_MSGV3     = P_MSGV3
*              I_MSGV4     =
              I_FIELDNAME = P_FIELDNAME
              I_ROW_ID    = P_ROW_ID
              I_TABIX     = P_TABIX.


ENDFORM.                    " ADD_MESSAGE_ENTRY
*&------------------------------------------------------------------*
*&      Form  SET_EXCLUDING_F_CODE
*&------------------------------------------------------------------*
FORM SET_EXCLUDING_F_CODE TABLES   PT_EX_FCODE TYPE UI_FUNCTIONS.
  REFRESH PT_EX_FCODE.
  CASE WA_MODE.
    WHEN C_CREATE.
      APPEND : CL_GUI_ALV_GRID=>MC_FC_SUM     TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_INFO    TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_GRAPH   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_AVERAGE TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SUBTOT  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_SUM     TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_SUBTOT  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_REFRESH TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_PASTE TO PT_EX_FCODE.
    WHEN C_CHANGE.
      APPEND : CL_GUI_ALV_GRID=>MC_FC_INFO    TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_REFRESH TO PT_EX_FCODE,
*               CL_GUI_ALV_GRID=>MC_FC_LOC_COPY   TO PT_EX_FCODE,
*               CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_LOC_CUT   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW   TO PT_EX_FCODE,
*               CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_GRAPH   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SUM     TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_AVERAGE TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SUBTOT  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_SUM     TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_DETAIL  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_SUBTOT  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_PASTE TO PT_EX_FCODE.
    WHEN C_DISPLAY.
      APPEND : CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_LOC_COPY   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_LOC_CUT   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_GRAPH   TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SUM     TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SELECT_ALL  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_INFO    TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_AVERAGE TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_SUBTOT  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_SUM     TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_SUBTOT  TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_FC_REFRESH TO PT_EX_FCODE,
               CL_GUI_ALV_GRID=>MC_MB_PASTE TO PT_EX_FCODE.
  ENDCASE.

ENDFORM.                    " SET_EXCLUDING_F_CODE
*&-----------------------------------------------------------------*
*&      Form  CREATE_INSPECTION_LOT
*&-----------------------------------------------------------------*
FORM CREATE_INSPECTION_LOT USING     P_INDEX
                           CHANGING  P_ERROR
                                     P_MATNR TYPE MATNR.

  DATA : LW_ZSQM_QNS_ITEM LIKE ZSQM_QNS_ITEM.
  DATA : LW_I_STAT         TYPE ZQINSPSTATUS.
  DATA : LW_INDEX LIKE SY-TABIX.



  DATA : LW_PRUEFLOS    TYPE QPLOS,
         LW_PRUEFLOS_MS TYPE QPLOS.

  DATA : LW_RETURN     TYPE BAPIRET2,
         LW_RETURN_MS  TYPE BAPIRET2.

  DATA : LT_BDC_MSG    LIKE BDCMSGCOLL OCCURS 10 WITH HEADER LINE,
         LT_BDC_MSG_MS LIKE BDCMSGCOLL OCCURS 10 WITH HEADER LINE.


  LW_INDEX = P_INDEX.

  CHECK NOT LW_INDEX IS INITIAL.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
        MOVE-CORRESPONDING IT_ZSQM_QNS_CH_ISP  TO LW_ZSQM_QNS_ITEM.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
        MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_ISE TO LW_ZSQM_QNS_ITEM.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
      MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_REG TO LW_ZSQM_QNS_ITEM.
  ENDCASE.

  MOVE : LW_ZSQM_QNS_ITEM-MATNR TO P_MATNR.

*  execute Function for creating inspection lot by item status.
  IF LW_ZSQM_QNS_ITEM-I_STAT = C_CREATION.

  ELSEIF LW_ZSQM_QNS_ITEM-I_STAT = C_RELEASE OR
         LW_ZSQM_QNS_ITEM-I_STAT = C_DONTUSE.
    P_ERROR = C_MARK.
    EXIT.
  ELSE.
  ENDIF.


*  DATA : LW_ZTQM_QNS_HDR   LIKE ZTQM_QNS_ITEM,
*         LW_ZTQM_QNS_ITEM  LIKE ZTQM_QNS_ITEM,
*         LW_ZTQM_QNS_IT_MS LIKE ZTQM_QNS_IT_MS.
  DATA : LW_INSP_HDR	LIKE	ZTQM_QNS_HDR,
         LW_INSP_ITEM	LIKE	ZTQM_QNS_ITEM,
         LW_INSP_IT_MS	LIKE	ZTQM_QNS_IT_MS.

*//////-- Create Inspection lot./////////>>>>>>>>>.>%%%%%
  MOVE-CORRESPONDING  ZSQM_QNS_HDR      TO LW_INSP_HDR.
  MOVE-CORRESPONDING  LW_ZSQM_QNS_ITEM  TO :  LW_INSP_ITEM,
                                             LW_INSP_IT_MS.
*-  Create inspection lot using BDC function

  CALL FUNCTION 'Z_FQM_INSPECTION_LOT_CRT_V2'
       EXPORTING
            I_INSP_HDR              = LW_INSP_HDR
            I_INSP_ITEM             = LW_INSP_ITEM
            I_INSP_IT_MS            = LW_INSP_IT_MS
            I_BDC_MODE              = WA_BDC_MODE
       IMPORTING
            E_PRUEFLOS              = LW_PRUEFLOS
            E_PRUEFLOS_MS           = LW_PRUEFLOS_MS
            RETURN                  = LW_RETURN
            RETURN_MS               = LW_RETURN_MS
       TABLES
            T_BDC_MSG               = LT_BDC_MSG
            T_BDC_MSG_MS            = LT_BDC_MSG_MS
       EXCEPTIONS
            ERROR_DURING_CREATE_LOT = 1
            NO_SUPPORTED_MATERIAL   = 2
            OTHERS                  = 3.

  IF SY-SUBRC NE 0.
    P_ERROR = SY-SUBRC.
    EXIT.
  ENDIF.

  IF LW_RETURN-TYPE = 'E' OR LW_RETURN-TYPE = 'A'.
    P_ERROR = '8'.
    EXIT.
  ENDIF.
  IF LW_RETURN_MS-TYPE = 'E' OR LW_RETURN_MS-TYPE = 'A'.
    P_ERROR = '9'.
    EXIT.
  ENDIF.

*-- Check Inspection lot creation status : Added 02/05/2004 : sllee
  IF SY-SUBRC = 0 AND
      NOT LW_PRUEFLOS     IS INITIAL AND
          LW_RETURN-TYPE = 'S'.
  ELSE.
    MESSAGE W000(ZMQM) WITH TEXT-EEE."/'Can''t create inspetion Lot!'
    EXIT.
  ENDIF.

*-- Check Inspection lot creation status : end

*////-- Successfully created and update internal table and DB.
*-  Update internal table for ALV
  CASE ZSQM_QNS_HDR-ART.
    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        MOVE : LW_PRUEFLOS    TO IT_ZSQM_QNS_CH_ISP-PRUEFLOS,
               LW_PRUEFLOS_MS TO IT_ZSQM_QNS_CH_ISP-PRUEFLOS_MS,
               C_RELEASE      TO IT_ZSQM_QNS_CH_ISP-I_STAT.
        MOVE : SY-DATUM       TO IT_ZSQM_QNS_CH_ISP-AEDAT,
               SY-UNAME       TO IT_ZSQM_QNS_CH_ISP-AENAM,
               SY-UZEIT       TO IT_ZSQM_QNS_CH_ISP-AEZET.
        MODIFY IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        MOVE : LW_PRUEFLOS    TO IT_ZSQM_QNS_CH_ISE-PRUEFLOS,
               LW_PRUEFLOS_MS TO IT_ZSQM_QNS_CH_ISE-PRUEFLOS_MS,
               C_RELEASE      TO IT_ZSQM_QNS_CH_ISE-I_STAT.
        MOVE : SY-DATUM       TO IT_ZSQM_QNS_CH_ISE-AEDAT,
               SY-UNAME       TO IT_ZSQM_QNS_CH_ISE-AENAM,
               SY-UZEIT       TO IT_ZSQM_QNS_CH_ISE-AEZET.
        MODIFY IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      MOVE : LW_PRUEFLOS    TO IT_ZSQM_QNS_CH_REG-PRUEFLOS,
             LW_PRUEFLOS_MS TO IT_ZSQM_QNS_CH_REG-PRUEFLOS_MS,
             C_RELEASE      TO IT_ZSQM_QNS_CH_REG-I_STAT.
      MOVE : SY-DATUM       TO IT_ZSQM_QNS_CH_REG-AEDAT,
             SY-UNAME       TO IT_ZSQM_QNS_CH_REG-AENAM,
             SY-UZEIT       TO IT_ZSQM_QNS_CH_REG-AEZET.
      MODIFY IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
  ENDCASE.

*/-  Update DB table.
  PERFORM ENQUEUE_DB.
*- Inspection Scheduling Item(Material) : ISIR, Regular
  UPDATE  ZTQM_QNS_ITEM  SET I_STAT   = C_RELEASE
                             PRUEFLOS = LW_PRUEFLOS
                             AENAM    = SY-UNAME
                             AEDAT    = SY-DATUM
                             AEZET    = SY-UZEIT
                  WHERE IYEAR      = ZSQM_QNS_HDR-IYEAR
                    AND KATART_VH  = ZSQM_QNS_HDR-KATART_VH
                    AND CODEGRP_VH = ZSQM_QNS_HDR-CODEGRP_VH
                    AND CODE_VH    = ZSQM_QNS_HDR-CODE_VH
                    AND ART        = ZSQM_QNS_HDR-ART
                    AND MATNR      = LW_ZSQM_QNS_ITEM-MATNR.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    P_ERROR = 'U'.
    EXIT.
  ENDIF.
*- Insp. Scheduling Item(8910(20)-8930) : MS of ISIR/Regular
  UPDATE  ZTQM_QNS_IT_MS  SET PRUEFLOS_MS = LW_PRUEFLOS_MS
                  WHERE IYEAR      = ZSQM_QNS_HDR-IYEAR
                    AND KATART_VH  = ZSQM_QNS_HDR-KATART_VH
                    AND CODEGRP_VH = ZSQM_QNS_HDR-CODEGRP_VH
                    AND CODE_VH    = ZSQM_QNS_HDR-CODE_VH
                    AND ART        = ZSQM_QNS_HDR-ART
                    AND MATNR      = LW_ZSQM_QNS_ITEM-MATNR.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    P_ERROR = 'U'.
    EXIT.
  ENDIF.

  COMMIT WORK AND WAIT.

  PERFORM DEQUEUE_DB.

ENDFORM.                    " CREATE_INSPECTION_LOT
*&------------------------------------------------------------------*
*&      Form  MOVE_CHDATA_TO_IT_FOR_DB_CH
*&------------------------------------------------------------------*
FORM MOVE_CHDATA_TO_IT_FOR_DB_CH.
  CLEAR   : WA_ZTQM_QNS_HDR, IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
  REFRESH : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.

*-- move Header data  from structure to workarea like Header table
  MOVE-CORRESPONDING : ZSQM_QNS_HDR TO WA_ZTQM_QNS_HDR.

*-- move itema data from excel upload table to IT like item table
*-- by Inspection type and plant.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.

      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.

        LOOP AT IT_ZSQM_QNS_CH_ISP.
          CLEAR : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
          MOVE-CORRESPONDING IT_ZSQM_QNS_CH_ISP TO : IT_ZTQM_QNS_ITEM,
                                                     IT_ZTQM_QNS_IT_MS.
          APPEND : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
        ENDLOOP.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.

        LOOP AT IT_ZSQM_QNS_CH_ISE.
          CLEAR : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
          MOVE-CORRESPONDING IT_ZSQM_QNS_CH_ISE TO : IT_ZTQM_QNS_ITEM,
                                                     IT_ZTQM_QNS_IT_MS.
          APPEND : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
        ENDLOOP.
      ENDIF.

    WHEN C_INSP_TYPE_REGULAR.

      LOOP AT IT_ZSQM_QNS_CH_REG.
        CLEAR : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
        MOVE-CORRESPONDING IT_ZSQM_QNS_CH_REG TO : IT_ZTQM_QNS_ITEM,
                                                   IT_ZTQM_QNS_IT_MS.
        APPEND : IT_ZTQM_QNS_ITEM, IT_ZTQM_QNS_IT_MS.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " MOVE_CHDATA_TO_IT_FOR_DB_CH
*&----------------------------------------------------------------*
*&      Form  SET_SCH_STATUS_AND_CHECK
*&-----------------------------------------------------------------*
FORM SET_SCH_STATUS_AND_CHECK  USING PW_GOOD_CELLS TYPE LVC_S_MODI.

  DATA : LW_ZSQM_QNS_ITEM LIKE ZSQM_QNS_ITEM.
  DATA : LW_I_STAT         TYPE ZQINSPSTATUS.

  CHECK  PW_GOOD_CELLS-ERROR IS INITIAL."No error during changing value


  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX PW_GOOD_CELLS-ROW_ID.
        MOVE C_CREATION TO IT_ZSQM_QNS_CH_ISP-I_STAT.
        MODIFY IT_ZSQM_QNS_CH_ISP INDEX PW_GOOD_CELLS-ROW_ID.
        PERFORM CHANGE_LOG_INFO_SET   USING PW_GOOD_CELLS-ROW_ID
                                            IT_ZSQM_QNS_CH_ISP-MATNR.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX PW_GOOD_CELLS-ROW_ID.
        MOVE C_CREATION TO IT_ZSQM_QNS_CH_ISE-I_STAT.
        MODIFY IT_ZSQM_QNS_CH_ISE INDEX PW_GOOD_CELLS-ROW_ID.
        PERFORM CHANGE_LOG_INFO_SET   USING PW_GOOD_CELLS-ROW_ID
                                               IT_ZSQM_QNS_CH_ISE-MATNR.

      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX PW_GOOD_CELLS-ROW_ID.
      MOVE C_CREATION TO IT_ZSQM_QNS_CH_REG-I_STAT.
      MODIFY IT_ZSQM_QNS_CH_REG INDEX PW_GOOD_CELLS-ROW_ID.
      PERFORM CHANGE_LOG_INFO_SET   USING PW_GOOD_CELLS-ROW_ID
                                               IT_ZSQM_QNS_CH_REG-MATNR.

  ENDCASE.

ENDFORM.                    " SET_SCH_STATUS_AND_CHECK
*&------------------------------------------------------------------*
*&      Form  CHANGE_LOG_INFO_SET
*&------------------------------------------------------------------*
FORM CHANGE_LOG_INFO_SET USING    P_INDEX
                                  P_MATNR .

  DATA : LW_INDEX LIKE SY-TABIX.

  LW_INDEX = P_INDEX.


  READ TABLE IT_ZSQM_QNS_ITEM   WITH KEY MATNR = P_MATNR.

  IF SY-SUBRC = 0.  "/already existed material
    CASE ZSQM_QNS_HDR-ART.

      WHEN C_INSP_TYPE_ISIR.
        IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
          READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
          MOVE : SY-UNAME   TO IT_ZSQM_QNS_CH_ISP-AENAM,
                 SY-DATUM   TO IT_ZSQM_QNS_CH_ISP-AEDAT,
                 SY-UZEIT   TO IT_ZSQM_QNS_CH_ISP-AEZET.
          MODIFY IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
          READ TABLE IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
          MOVE : SY-UNAME   TO IT_ZSQM_QNS_CH_ISE-AENAM,
                 SY-DATUM   TO IT_ZSQM_QNS_CH_ISE-AEDAT,
                 SY-UZEIT   TO IT_ZSQM_QNS_CH_ISE-AEZET.
          MODIFY IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
        ENDIF.
      WHEN C_INSP_TYPE_REGULAR.
        READ TABLE IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
        MOVE : SY-UNAME   TO IT_ZSQM_QNS_CH_REG-AENAM,
               SY-DATUM   TO IT_ZSQM_QNS_CH_REG-AEDAT,
               SY-UZEIT   TO IT_ZSQM_QNS_CH_REG-AEZET.
        MODIFY IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
    ENDCASE.

  ELSE.           "/new created material
    CASE ZSQM_QNS_HDR-ART.

      WHEN C_INSP_TYPE_ISIR.
        IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
          READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
          MOVE : SY-UNAME   TO IT_ZSQM_QNS_CH_ISP-ERNAM,
                 SY-DATUM   TO IT_ZSQM_QNS_CH_ISP-ERDAT,
                 SY-UZEIT   TO IT_ZSQM_QNS_CH_ISP-ERZET.
          MODIFY IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
        ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
          READ TABLE IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
          MOVE : SY-UNAME   TO IT_ZSQM_QNS_CH_ISE-ERNAM,
                 SY-DATUM   TO IT_ZSQM_QNS_CH_ISE-ERDAT,
                 SY-UZEIT   TO IT_ZSQM_QNS_CH_ISE-ERZET.
          MODIFY IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
        ENDIF.
      WHEN C_INSP_TYPE_REGULAR.
        READ TABLE IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
        MOVE : SY-UNAME   TO IT_ZSQM_QNS_CH_REG-ERNAM,
               SY-DATUM   TO IT_ZSQM_QNS_CH_REG-ERDAT,
               SY-UZEIT   TO IT_ZSQM_QNS_CH_REG-ERZET.
        MODIFY IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
    ENDCASE.
  ENDIF.

ENDFORM.                    " CHANGE_LOG_INFO_SET
*&------------------------------------------------------------------*
*&      Form  PROC_DOUBLE_CLICK
*&------------------------------------------------------------------*
FORM PROC_DOUBLE_CLICK USING  P_COLUMN_NAME             "Column Name
                              PS_ROW_NO  LIKE SY-TABIX. "Numeric Row ID
  DATA : LW_SEL_INDEX LIKE SY-TABIX.
  DATA : BEGIN OF LS_PRUEFLOS,
          PRUEFLOS     TYPE QPLOS, "/ISIR or Regular inspection No.
          PRUEFLOS_MS  TYPE QPLOS, "/MS inspection No.
         END OF LS_PRUEFLOS.

  DATA : LW_PRUEFLOS TYPE QPLOS.  "/Selected inspection Lot No.

  CHECK P_COLUMN_NAME = 'PRUEFLOS' OR
        P_COLUMN_NAME = 'PRUEFLOS_MS'.

  LW_SEL_INDEX = PS_ROW_NO.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_SEL_INDEX.
        MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_ISP  TO LS_PRUEFLOS.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX LW_SEL_INDEX.
        MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_ISE  TO LS_PRUEFLOS.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX LW_SEL_INDEX.
      MOVE-CORRESPONDING IT_ZSQM_QNS_CH_REG   TO LS_PRUEFLOS.
  ENDCASE.

  CHECK NOT LS_PRUEFLOS IS INITIAL..
*-- Move selected inspection lot No. to local variable LW_PRUEFLOS

  IF P_COLUMN_NAME = 'PRUEFLOS'.
    LW_PRUEFLOS = LS_PRUEFLOS-PRUEFLOS.
  ELSE.
    LW_PRUEFLOS = LS_PRUEFLOS-PRUEFLOS_MS.
  ENDIF.

  CHECK NOT LW_PRUEFLOS IS INITIAL.

*- Call Transaction 'QA03' to dislplay inspection lot infomation
  SET PARAMETER ID 'QLS' FIELD LW_PRUEFLOS.

  CALL TRANSACTION 'QA03' AND SKIP FIRST SCREEN .

ENDFORM.                    " PROC_DOUBLE_CLICK
*&------------------------------------------------------------------*
*&      Form  SET_EXCLUDING_PF_STATUS
*&------------------------------------------------------------------*
FORM SET_EXCLUDING_PF_STATUS.

  REFRESH IT_EX_FUNC.

  CASE WA_MODE.
    WHEN C_CREATE.

    WHEN C_CHANGE.
      MOVE : 'UPLOAD' TO WA_EX_FUNC-FCODE.
      APPEND WA_EX_FUNC TO IT_EX_FUNC.
    WHEN C_DISPLAY.
      MOVE : 'UPLOAD' TO WA_EX_FUNC-FCODE.
      APPEND WA_EX_FUNC TO IT_EX_FUNC.
      MOVE : 'SAVE' TO WA_EX_FUNC-FCODE.
      APPEND WA_EX_FUNC TO IT_EX_FUNC.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " SET_EXCLUDING_PF_STATUS
*&------------------------------------------------------------------*
*&      Form  GET_EARLIST_LATEST_DATE
*&------------------------------------------------------------------*
FORM GET_EARLIST_LATEST_DATE.

  DATA : LW_ZSQM_QNS_IT_D LIKE ZSQM_QNS_IT_D.
  DATA : LW_DATUV   LIKE SY-DATUM, "/Earliest start date
         LW_DATUV_L LIKE SY-DATUM. "/Latest start date

  DATA : LW_COMP_INDEX TYPE I.
  FIELD-SYMBOLS : <LW_FS>.

  REFRESH IT_ZTQM_QNS_IT_D.


  LOOP AT IT_ZTQM_QNS_ITEM.

    READ TABLE IT_ZTQM_QNS_IT_MS
                     WITH KEY IYEAR      = IT_ZTQM_QNS_ITEM-IYEAR
                              KATART_VH  = IT_ZTQM_QNS_ITEM-KATART_VH
                              CODEGRP_VH = IT_ZTQM_QNS_ITEM-CODEGRP_VH
                              CODE_VH    = IT_ZTQM_QNS_ITEM-CODE_VH
                              ART        = IT_ZTQM_QNS_ITEM-ART
                              MATNR      = IT_ZTQM_QNS_ITEM-MATNR.

    CHECK SY-SUBRC = 0.
    CLEAR : IT_ZTQM_QNS_IT_D, LW_ZSQM_QNS_IT_D.

    MOVE-CORRESPONDING : IT_ZTQM_QNS_ITEM  TO LW_ZSQM_QNS_IT_D,
                         IT_ZTQM_QNS_IT_MS TO LW_ZSQM_QNS_IT_D.

*-- Get earliest start date of MIC(ISIR or Regular) for Material

    CLEAR : LW_DATUV, LW_DATUV_L, LW_COMP_INDEX.

    DO 25 TIMES.
      LW_COMP_INDEX = LW_COMP_INDEX + 1.

      ASSIGN COMPONENT  LW_COMP_INDEX OF STRUCTURE LW_ZSQM_QNS_IT_D
                                                           TO <LW_FS>.

      IF <LW_FS> IS INITIAL.
        CONTINUE.
      ENDIF.

      IF LW_DATUV   IS INITIAL AND
         LW_DATUV_L IS INITIAL.
        LW_DATUV   = <LW_FS>.
        LW_DATUV_L = <LW_FS>.
      ENDIF.

      IF <LW_FS> < LW_DATUV.
        LW_DATUV = <LW_FS>.
      ELSEIF <LW_FS> > LW_DATUV_L.
        LW_DATUV_L = <LW_FS>.
      ENDIF.

    ENDDO.

    MOVE-CORRESPONDING IT_ZTQM_QNS_ITEM TO IT_ZTQM_QNS_IT_D.
    MOVE : LW_DATUV   TO IT_ZTQM_QNS_IT_D-DATUV_FD,
           LW_DATUV_L TO IT_ZTQM_QNS_IT_D-DATUV_LD.

    APPEND IT_ZTQM_QNS_IT_D.

  ENDLOOP.


ENDFORM.                    " GET_EARLIST_LATEST_DATE
*&------------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_FOR_ALV
*&------------------------------------------------------------------*
FORM SET_FIELD_ATTR_FOR_ALV TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.
*-- Set Field attributes of ALV :

  DATA : LW_DIV_FLD(2) TYPE N.
  DATA : LW_TEXT_SYMBOL_NAME(8) TYPE C. "/Text Symbols name var.
  DATA : LW_MIC_CODE(5) TYPE C.


  DATA : BEGIN OF LT_QPMK OCCURS 30,
          MKMNR      TYPE  QMERKNR,
          KURZTEXT   TYPE  QKURZTEXT,
         END OF LT_QPMK.
  	
    DATA : LW_MKMNR TYPE QMERKNR.

  DATA : LW_FLD_N_IND(4) TYPE N,
         LW_FLD_N_DIV(3) TYPE N,
         LW_MIC_SER(2)   TYPE N.


*  FIELD-SYMBOLS:<LW_FS>.


*--  Get MIC Master text list
  SELECT A~MKMNR B~KURZTEXT  INTO CORRESPONDING FIELDS OF TABLE LT_QPMK
                        FROM QPMK AS  A INNER JOIN QPMT AS B
                           ON   A~ZAEHLER  = B~ZAEHLER
                            AND A~MKMNR    = B~MKMNR
                            AND A~VERSION  = B~VERSION
                          WHERE A~ZAEHLER = ZSQM_QNS_HDR-WERKS
                            AND A~VERSION = '000001'
                            AND A~LOEKZ   = '2'
                            AND B~SPRACHE = SY-LANGU
                            AND B~GELOESCHT = ' '.

  CHECK SY-SUBRC = 0.

  LOOP AT PT_FIELDCAT.
    IF PT_FIELDCAT-FIELDNAME = 'MATNR'.
      PT_FIELDCAT-KEY_SEL   = C_MARK.
*      PT_FIELDCAT-EMPHASIZE = C_MARK.
      PT_FIELDCAT-EMPHASIZE = 'C410'.
*      PT_FIELDCAT-KEY       = C_MARK.
      PT_FIELDCAT-FIX_COLUMN  = C_MARK.
      PT_FIELDCAT-OUTPUTLEN = 18.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'I_STAT'.
      PT_FIELDCAT-OUTPUTLEN = 3.
    ENDIF.

    IF PT_FIELDCAT-FIELDNAME = 'PRUEFLOS'.

    ELSEIF PT_FIELDCAT-FIELDNAME = 'PRUEFLOS_MS'.
      PT_FIELDCAT-EMPHASIZE = C_MARK.
      MOVE  'Insp. lot(MS)'(TZ1) TO : PT_FIELDCAT-COLTEXT,
                                      PT_FIELDCAT-SCRTEXT_L,
                                      PT_FIELDCAT-SCRTEXT_M,
                                      PT_FIELDCAT-SCRTEXT_S.
    ENDIF.

    IF PT_FIELDCAT-FIELDNAME = 'CODE_IP' OR
       PT_FIELDCAT-FIELDNAME = 'LIFNR' OR
       PT_FIELDCAT-FIELDNAME = 'EKORG' OR
       PT_FIELDCAT-FIELDNAME = 'I_STAT'.

      PT_FIELDCAT-EMPHASIZE = 'C210'.

    ENDIF.

*-- ISIR/REGULAR
    IF PT_FIELDCAT-FIELDNAME+0(4) = 'DATU' AND
       PT_FIELDCAT-FIELDNAME+6(1) = '0'.
      LW_FLD_N_IND =  PT_FIELDCAT-FIELDNAME+6(4). "/'DATUV/B_xxxx'
      LW_MIC_SER   =  LW_FLD_N_IND+1(2). "/'0xx0' "/MIC Serial
*      - Start/End date of MIC
      CASE ZSQM_QNS_HDR-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_QNS_HDR-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type

              CONCATENATE ZSQM_QNS_HDR-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_ISIR+2(2)      "/Insp. type:'QPxx'
                      '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.

            WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
              CONCATENATE ZSQM_QNS_HDR-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_ISIR+2(2)      "/Insp. type:'QPxx'
                       '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          CONCATENATE ZSQM_QNS_HDR-WERKS+0(1)  "/Plant:'x001'
                      C_CODEGRUPPE_REGU+2(2)    "/Insp. type:'QPxx'
                      '_'
                      LW_MIC_SER
                        INTO LW_MKMNR.
      ENDCASE.

*      select Column Text for MIC - 'XXX_nn'
      READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

      IF SY-SUBRC = 0.
        MOVE  LT_QPMK-KURZTEXT TO : PT_FIELDCAT-COLTEXT,
                                    PT_FIELDCAT-SCRTEXT_L,
                                    PT_FIELDCAT-SCRTEXT_M,
                                    PT_FIELDCAT-SCRTEXT_S.
      ELSE. "- 'XXXnn' "/ for Dev Test system - sllee
        REPLACE '_' WITH ' ' INTO  LW_MKMNR.
        CONDENSE LW_MKMNR NO-GAPS.
        READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

        IF SY-SUBRC = 0.
          MOVE  LT_QPMK-KURZTEXT TO : PT_FIELDCAT-COLTEXT,
                                      PT_FIELDCAT-SCRTEXT_L,
                                      PT_FIELDCAT-SCRTEXT_M,
                                      PT_FIELDCAT-SCRTEXT_S.
        ENDIF.
      ENDIF.

      IF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT-EMPHASIZE = 'C300'.
      ELSEIF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT-EMPHASIZE = 'C311'.
      ENDIF.
    ENDIF.
**-- MS
    IF PT_FIELDCAT-FIELDNAME+0(4) = 'DATU' AND
       PT_FIELDCAT-FIELDNAME+6(1) = '1'.
      LW_FLD_N_IND =  PT_FIELDCAT-FIELDNAME+6(4). "/'DATUV/B_xxxx'
      LW_MIC_SER   =  LW_FLD_N_IND+1(2). "/'0xx0' "/MIC Serial
*      - Start/End date of MIC
      CASE ZSQM_QNS_HDR-ART.
        WHEN C_INSP_TYPE_ISIR.
          CASE ZSQM_QNS_HDR-CODEGRP_VH.
            WHEN C_VE_ENG_CG_ENG. "/ HMMA QM Engine type

              CONCATENATE ZSQM_QNS_HDR-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_MS+2(2)      "/Insp. type:'QPxx'
                      '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.

            WHEN C_VE_ENG_CG_VEH. "/ HMMA QM Vehicle type
              CONCATENATE ZSQM_QNS_HDR-WERKS+0(1)  "/Plant:'x001'
                     C_CODEGRUPPE_MS+2(2)      "/Insp. type:'QPxx'
                         '_'
                          LW_MIC_SER
                            INTO LW_MKMNR.
          ENDCASE.
        WHEN C_INSP_TYPE_REGULAR.
          CONCATENATE ZSQM_QNS_HDR-WERKS+0(1)  "/Plant:'x001'
                      C_CODEGRUPPE_MS+2(2)    "/Insp. type:'QPxx'
                      '_'
                      LW_MIC_SER
                        INTO LW_MKMNR.
      ENDCASE.

*      select Column Text for MIC - 'XXX_nn'
      READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

      IF SY-SUBRC = 0.
        MOVE  LT_QPMK-KURZTEXT TO : PT_FIELDCAT-COLTEXT,
                                    PT_FIELDCAT-SCRTEXT_L,
                                    PT_FIELDCAT-SCRTEXT_M,
                                    PT_FIELDCAT-SCRTEXT_S.
      ELSE. "- 'XXXnn' "/ for Dev Test system - sllee
        REPLACE '_' WITH ' ' INTO  LW_MKMNR.
        CONDENSE LW_MKMNR NO-GAPS.
        READ TABLE LT_QPMK WITH KEY MKMNR = LW_MKMNR.

        IF SY-SUBRC = 0.
          MOVE  LT_QPMK-KURZTEXT TO : PT_FIELDCAT-COLTEXT,
                                      PT_FIELDCAT-SCRTEXT_L,
                                      PT_FIELDCAT-SCRTEXT_M,
                                      PT_FIELDCAT-SCRTEXT_S.
        ENDIF.
      ENDIF.

      IF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUV'.
        PT_FIELDCAT-EMPHASIZE = 'C400'.
      ELSEIF PT_FIELDCAT-FIELDNAME+0(5) = 'DATUB'.
        PT_FIELDCAT-EMPHASIZE = 'C411'.
      ENDIF.

    ENDIF.

    MODIFY PT_FIELDCAT.

  ENDLOOP.
ENDFORM.                    " SET_FIELD_ATTR_FOR_ALV
*&------------------------------------------------------------------*
*&      Form  FILL_IT_FOR_DELETE_ITEM
*&------------------------------------------------------------------*
FORM FILL_IT_FOR_DELETE_ITEM.
  DATA: LW_DEL_INDEX LIKE SY-TABIX.

  REFRESH : IT_ZTQM_DEL_ITEM,
            IT_ZTQM_DEL_IT_MS,
            IT_ZTQM_DEL_IT_D.

  LOOP AT IT_ZSQM_QNS_ITEM.
    READ TABLE IT_ZTQM_QNS_ITEM
                    WITH KEY MATNR       = IT_ZSQM_QNS_ITEM-MATNR.
    IF SY-SUBRC NE 0.
      CLEAR : IT_ZTQM_DEL_ITEM, IT_ZTQM_DEL_IT_MS.
      MOVE-CORRESPONDING  ZSQM_QNS_HDR TO : IT_ZTQM_DEL_ITEM,
                                            IT_ZTQM_DEL_IT_MS,
                                            IT_ZTQM_DEL_IT_D .

      MOVE  IT_ZSQM_QNS_ITEM-MATNR TO : IT_ZTQM_DEL_ITEM-MATNR,
                                        IT_ZTQM_DEL_IT_MS-MATNR,
                                        IT_ZTQM_DEL_IT_D-MATNR.
      APPEND : IT_ZTQM_DEL_ITEM, IT_ZTQM_DEL_IT_MS, IT_ZTQM_DEL_IT_D .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FOR_DELETE_ITEM
*&------------------------------------------------------------------*
*&      Form  CHECK_N_SET_HEADER_STAT
*&------------------------------------------------------------------*
FORM CHECK_N_SET_HEADER_STAT.
  CASE WA_MODE.
    WHEN C_CREATE.

    WHEN C_CHANGE.
      CASE ZSQM_QNS_HDR-ART.

        WHEN C_INSP_TYPE_ISIR.
          IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
            READ TABLE  IT_ZSQM_QNS_CH_ISP WITH KEY I_STAT = C_CREATION.
            IF SY-SUBRC = 0.
              ZSQM_QNS_HDR-H_STAT = C_CREATION.
              EXIT.
            ELSE.
             READ TABLE  IT_ZSQM_QNS_CH_ISP WITH KEY I_STAT = C_RELEASE.
              IF SY-SUBRC = 0.
                ZSQM_QNS_HDR-H_STAT = C_RELEASE.
              ELSE.
                ZSQM_QNS_HDR-H_STAT = C_DONTUSE.
              ENDIF.
            ENDIF.

          ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
            READ TABLE IT_ZSQM_QNS_CH_ISE WITH KEY I_STAT = C_CREATION.
            IF SY-SUBRC = 0.
              ZSQM_QNS_HDR-H_STAT = C_CREATION.
              EXIT.
            ELSE.
             READ TABLE  IT_ZSQM_QNS_CH_ISE WITH KEY I_STAT = C_RELEASE.
              IF SY-SUBRC = 0.
                ZSQM_QNS_HDR-H_STAT = C_RELEASE.
              ELSE.
                ZSQM_QNS_HDR-H_STAT = C_DONTUSE.
              ENDIF.
            ENDIF.
          ENDIF.
        WHEN C_INSP_TYPE_REGULAR.
          READ TABLE IT_ZSQM_QNS_CH_REG WITH KEY I_STAT = C_CREATION.
          IF SY-SUBRC = 0.
            ZSQM_QNS_HDR-H_STAT = C_CREATION.
            EXIT.
          ELSE.
            READ TABLE  IT_ZSQM_QNS_CH_REG WITH KEY I_STAT = C_RELEASE.
            IF SY-SUBRC = 0.
              ZSQM_QNS_HDR-H_STAT = C_RELEASE.
            ELSE.
              ZSQM_QNS_HDR-H_STAT = C_DONTUSE.
            ENDIF.
          ENDIF.

      ENDCASE.
  ENDCASE.

ENDFORM.                    " CHECK_N_SET_HEADER_STAT
*&------------------------------------------------------------------*
*&      Form  MOVE_EXCEL_D_TO_CH_MODE_IT
*&------------------------------------------------------------------*
FORM MOVE_EXCEL_D_TO_CH_MODE_IT.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        LOOP AT IT_ZSQM_QNS_EX_ISP.
          READ TABLE IT_ZSQM_QNS_CH_ISP WITH KEY
                                     MATNR = IT_ZSQM_QNS_EX_ISP-MATNR.
          IF SY-SUBRC = 0.
            MESSAGE E000(ZMQM) WITH IT_ZSQM_QNS_EX_ISP-MATNR
                                    'was already exist.'(E21).
            EXIT.
          ENDIF.

          CLEAR IT_ZSQM_QNS_CH_ISP.
          MOVE-CORRESPONDING ZSQM_QNS_HDR TO IT_ZSQM_QNS_CH_ISP.
          MOVE-CORRESPONDING IT_ZSQM_QNS_EX_ISP TO IT_ZSQM_QNS_CH_ISP.
          MOVE C_CREATION TO IT_ZSQM_QNS_CH_ISP-I_STAT.
          MOVE : SY-UNAME TO IT_ZSQM_QNS_CH_ISP-ERNAM,
                 SY-UZEIT TO IT_ZSQM_QNS_CH_ISP-ERZET,
                 SY-DATUM TO IT_ZSQM_QNS_CH_ISP-ERDAT.
          APPEND IT_ZSQM_QNS_CH_ISP.
        ENDLOOP.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        LOOP AT IT_ZSQM_QNS_EX_ISE.
          READ TABLE IT_ZSQM_QNS_CH_ISE WITH KEY
                                   MATNR = IT_ZSQM_QNS_EX_ISE-MATNR.
          IF SY-SUBRC = 0.
            MESSAGE E000(ZMQM) WITH IT_ZSQM_QNS_EX_ISE-MATNR
                                    'was already exist.'(E21).
            EXIT.
          ENDIF.

          CLEAR IT_ZSQM_QNS_CH_ISE.
          MOVE-CORRESPONDING ZSQM_QNS_HDR TO IT_ZSQM_QNS_CH_ISE.
          MOVE-CORRESPONDING IT_ZSQM_QNS_EX_ISE TO IT_ZSQM_QNS_CH_ISE.
          MOVE C_CREATION TO IT_ZSQM_QNS_CH_ISE-I_STAT.
          MOVE : SY-UNAME TO IT_ZSQM_QNS_CH_ISE-ERNAM,
                 SY-UZEIT TO IT_ZSQM_QNS_CH_ISE-ERZET,
                 SY-DATUM TO IT_ZSQM_QNS_CH_ISE-ERDAT.
          APPEND IT_ZSQM_QNS_CH_ISE.
        ENDLOOP.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      LOOP AT IT_ZSQM_QNS_EX_REG.
        READ TABLE IT_ZSQM_QNS_CH_REG WITH KEY
                             MATNR = IT_ZSQM_QNS_EX_REG-MATNR.
        IF SY-SUBRC = 0.
          MESSAGE E000(ZMQM) WITH IT_ZSQM_QNS_EX_REG-MATNR
                                  'was already exist.'(E21).
          EXIT.
        ENDIF.

        CLEAR IT_ZSQM_QNS_CH_REG.
        MOVE-CORRESPONDING ZSQM_QNS_HDR TO IT_ZSQM_QNS_CH_REG.
        MOVE-CORRESPONDING IT_ZSQM_QNS_EX_REG TO IT_ZSQM_QNS_CH_REG.
        MOVE C_CREATION TO IT_ZSQM_QNS_CH_REG-I_STAT.
        MOVE : SY-UNAME TO IT_ZSQM_QNS_CH_REG-ERNAM,
               SY-UZEIT TO IT_ZSQM_QNS_CH_REG-ERZET,
               SY-DATUM TO IT_ZSQM_QNS_CH_REG-ERDAT.
        APPEND IT_ZSQM_QNS_CH_REG.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " MOVE_EXCEL_D_TO_CH_MODE_IT
*&------------------------------------------------------------------*
*&      Form  CHECK_NEW_ITEM_SAVED_TO_DB
*&------------------------------------------------------------------*
FORM CHECK_NEW_ITEM_SAVED_TO_DB USING    P_ROW_INDEX
                                CHANGING PW_ERROR.
  DATA : LW_ZTQM_QNS_ITEM LIKE ZTQM_QNS_ITEM,
         LW_ZSQM_QNS_ITEM LIKE ZSQM_QNS_ITEM.
  DATA : LW_MATNR   TYPE MATNR.
  DATA : LW_INDEX LIKE SY-TABIX.
  LW_INDEX = P_ROW_INDEX.

  CHECK NOT LW_INDEX IS INITIAL.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
        MOVE-CORRESPONDING IT_ZSQM_QNS_CH_ISP  TO LW_ZSQM_QNS_ITEM.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
        MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_ISE TO LW_ZSQM_QNS_ITEM.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
      MOVE-CORRESPONDING  IT_ZSQM_QNS_CH_REG TO LW_ZSQM_QNS_ITEM.
  ENDCASE.

  SELECT SINGLE * INTO LW_ZTQM_QNS_ITEM
      FROM ZTQM_QNS_ITEM
        WHERE IYEAR      = ZSQM_QNS_HDR-IYEAR
          AND KATART_VH  = ZSQM_QNS_HDR-KATART_VH
          AND CODEGRP_VH = ZSQM_QNS_HDR-CODEGRP_VH
          AND CODE_VH    = ZSQM_QNS_HDR-CODE_VH
          AND ART        = ZSQM_QNS_HDR-ART
          AND MATNR      = LW_ZSQM_QNS_ITEM-MATNR.

  IF SY-SUBRC NE 0.
    PW_ERROR = C_MARK.
    MESSAGE W000(ZMQM) WITH
      'Please Save uploaded item before create inspection lon.'(E20).
  ENDIF.

ENDFORM.                    " CHECK_NEW_ITEM_SAVED_TO_DB
*&------------------------------------------------------------------*
*&      Form  CHECK_INSP_SETUP_MATERIAL
*&------------------------------------------------------------------*
FORM CHECK_INSP_SETUP_MATERIAL.

  DATA : LW_NOT_SETUP TYPE C,
         LW_DEL_INDEX LIKE SY-TABIX.

  REFRESH IT_MSG_CONTROL.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        LOOP AT IT_ZSQM_QNS_EX_ISP.
          LW_DEL_INDEX = SY-TABIX.
          CLEAR LW_NOT_SETUP.
          PERFORM CHECK_INSPECTION_STAT_MAT
                                    USING ZSQM_QNS_HDR-WERKS
                                          ZSQM_QNS_HDR-ART
                                          IT_ZSQM_QNS_EX_ISP-MATNR
                                CHANGING  LW_NOT_SETUP.

          IF NOT LW_NOT_SETUP IS INITIAL.
            PERFORM ADD_MSG_TO_DISPLAY_MSG USING  'E'
                                                  ''
                                                  ''
                                          IT_ZSQM_QNS_EX_ISP-MATNR
                 ',Inspection Setup not exists for Material/Plant'(EZ1)
                                                  'MATNR'
                                                  SY-TABIX.

            DELETE IT_ZSQM_QNS_EX_ISP INDEX LW_DEL_INDEX. "/Delete item
          ENDIF.
        ENDLOOP.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        LOOP AT IT_ZSQM_QNS_EX_ISE.
          LW_DEL_INDEX = SY-TABIX.
          CLEAR LW_NOT_SETUP.
          PERFORM CHECK_INSPECTION_STAT_MAT
                                    USING ZSQM_QNS_HDR-WERKS
                                          ZSQM_QNS_HDR-ART
                                          IT_ZSQM_QNS_EX_ISE-MATNR
                                CHANGING  LW_NOT_SETUP.

          IF NOT LW_NOT_SETUP IS INITIAL.
            PERFORM ADD_MSG_TO_DISPLAY_MSG USING  'E'
                                                  ''
                                                  ''
                                          IT_ZSQM_QNS_EX_ISE-MATNR
                 ',Inspection Setup not exists for Material/Plant'(EZ1)
                                                  'MATNR'
                                                  SY-TABIX.

            DELETE IT_ZSQM_QNS_EX_ISE INDEX LW_DEL_INDEX. "/Delete item
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      LOOP AT IT_ZSQM_QNS_EX_REG.
        LW_DEL_INDEX = SY-TABIX.
        CLEAR LW_NOT_SETUP.
        PERFORM CHECK_INSPECTION_STAT_MAT
                                  USING ZSQM_QNS_HDR-WERKS
                                        ZSQM_QNS_HDR-ART
                                        IT_ZSQM_QNS_EX_REG-MATNR
                              CHANGING  LW_NOT_SETUP.

        IF NOT LW_NOT_SETUP IS INITIAL.
          PERFORM ADD_MSG_TO_DISPLAY_MSG USING  'E'
                                                ''
                                                ''
                                        IT_ZSQM_QNS_EX_REG-MATNR
               ',Inspection Setup not exists for Material/Plant'(EZ1)
                                                'MATNR'
                                                SY-TABIX.

          DELETE IT_ZSQM_QNS_EX_REG INDEX LW_DEL_INDEX. "/Delete item
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " CHECK_INSP_SETUP_MATERIAL
*&------------------------------------------------------------------*
*&      Form  CHECK_INSPECTION_STAT_MAT
*&------------------------------------------------------------------*
FORM CHECK_INSPECTION_STAT_MAT USING    P_WERKS
                                        P_ART
                                        P_MATNR
                               CHANGING P_NOT_SETUP.
DATA : LW_QMATV TYPE QMATV. "/Inspection Setup Exists for Material/Plant
  CLEAR P_NOT_SETUP.

  SELECT SINGLE A~QMATV INTO LW_QMATV
    FROM MARC AS A INNER JOIN QMAT AS B
      ON   A~MATNR   = B~MATNR
       AND A~WERKS   = B~WERKS
     WHERE  A~MATNR = P_MATNR
       AND  A~WERKS = P_WERKS
       AND  B~ART   = P_ART
       AND  A~QMATV = C_MARK.

  CHECK SY-SUBRC NE 0. "/Inspection Setup not Exists for Material/Plant
  P_NOT_SETUP = C_MARK.

ENDFORM.                    " CHECK_INSPECTION_STAT_MAT
*&-----------------------------------------------------------------*
*&      Form  CHECK_DELETE_ROW_ENABLE
*&-----------------------------------------------------------------*
FORM CHECK_DELETE_ROW_ENABLE   USING   P_INDEX
                             CHANGING  P_ERROR
                                       P_MATNR TYPE MATNR.

  DATA : LW_I_STAT         TYPE ZQINSPSTATUS.
  DATA : LW_INDEX LIKE SY-TABIX.


  LW_INDEX = P_INDEX.

  CHECK NOT LW_INDEX IS INITIAL.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
        MOVE : IT_ZSQM_QNS_CH_ISP-I_STAT  TO LW_I_STAT,
               IT_ZSQM_QNS_CH_ISP-MATNR   TO P_MATNR.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
        MOVE : IT_ZSQM_QNS_CH_ISE-I_STAT TO LW_I_STAT,
               IT_ZSQM_QNS_CH_ISE-MATNR  TO P_MATNR.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
      MOVE : IT_ZSQM_QNS_CH_REG-I_STAT TO LW_I_STAT,
             IT_ZSQM_QNS_CH_REG-MATNR  TO P_MATNR.
  ENDCASE.

*  execute Function for creating inspection lot by item status.
  IF LW_I_STAT = C_RELEASE.
    P_ERROR = C_MARK.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_DELETE_ROW_ENABLE
*&---------------------------------------------------------------------*
*&      Form  DELETE_ITEM
*&---------------------------------------------------------------------*
FORM DELETE_ITEM USING    P_INDEX.
  DATA : LW_INDEX LIKE SY-TABIX.
  LW_INDEX = P_INDEX.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        DELETE IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        DELETE IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      DELETE IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
  ENDCASE.
ENDFORM.                    " DELETE_ITEM
*&---------------------------------------------------------------------*
*&      Form  DEL_ALL_ITEM_HDR_FROM_DB
*&---------------------------------------------------------------------*
FORM DEL_ALL_ITEM_HDR_FROM_DB.

  CHECK WA_MODE = C_CHANGE.

  CLEAR   : WA_ZTQM_QNS_HDR.

*-- move Header data  from structure to workarea like Header table
  MOVE-CORRESPONDING : ZSQM_QNS_HDR TO WA_ZTQM_QNS_HDR.


  PERFORM ENQUEUE_DB.
*-- Delete Header data
  DELETE ZTQM_QNS_HDR FROM WA_ZTQM_QNS_HDR.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH 'Error founded Access DB'(EE1) 'ZTQM_QNS_HDR'.
  ENDIF.

*-- Delete Item : ISIR/Regular
  DELETE FROM ZTQM_QNS_ITEM
                      WHERE IYEAR      = WA_ZTQM_QNS_HDR-IYEAR
                        AND KATART_VH  = WA_ZTQM_QNS_HDR-KATART_VH
                        AND CODEGRP_VH = WA_ZTQM_QNS_HDR-CODEGRP_VH
                        AND CODE_VH    = WA_ZTQM_QNS_HDR-CODE_VH
                        AND ART        = WA_ZTQM_QNS_HDR-ART.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH TEXT-EE1 'ZTQM_QNS_ITEM'.
  ENDIF.

*-- Delete Item : MS
  DELETE FROM ZTQM_QNS_IT_MS
                  WHERE IYEAR      = WA_ZTQM_QNS_HDR-IYEAR
                    AND KATART_VH  = WA_ZTQM_QNS_HDR-KATART_VH
                    AND CODEGRP_VH = WA_ZTQM_QNS_HDR-CODEGRP_VH
                    AND CODE_VH    = WA_ZTQM_QNS_HDR-CODE_VH
                    AND ART        = WA_ZTQM_QNS_HDR-ART.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH TEXT-EE1 'ZTQM_QNS_IT_MS'.
  ENDIF.


  DELETE FROM ZTQM_QNS_IT_D
              WHERE IYEAR      = WA_ZTQM_QNS_HDR-IYEAR
                AND KATART_VH  = WA_ZTQM_QNS_HDR-KATART_VH
                AND CODEGRP_VH = WA_ZTQM_QNS_HDR-CODEGRP_VH
                AND CODE_VH    = WA_ZTQM_QNS_HDR-CODE_VH
                AND ART        = WA_ZTQM_QNS_HDR-ART.

  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    PERFORM DEQUEUE_DB.
    MESSAGE E000(ZMQM)
          WITH TEXT-EE1 'ZTQM_QNS_IT_D'.
  ENDIF.


  CHECK SY-SUBRC = 0.
  COMMIT WORK AND WAIT.
  MESSAGE S000(ZMQM) WITH 'Successfully all items are Deleted!'(S02).
  PERFORM DEQUEUE_DB.

  WA_STATUS = C_SAVED.

  LEAVE TO SCREEN 0.

ENDFORM.                    " DEL_ALL_ITEM_HDR_FROM_DB
*&------------------------------------------------------------------*
*&      Form  CHANGE_PLANNED_DATE_ITEM
*&------------------------------------------------------------------*
FORM CHANGE_PLANNED_DATE_ITEM  USING   P_INDEX
                             CHANGING  P_ERROR
                                       P_MATNR TYPE MATNR.

  DATA : LW_I_STAT         TYPE ZQINSPSTATUS.
  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_FLD_NAME(40) TYPE C.
  FIELD-SYMBOLS : <LW_FS>.

  LW_INDEX = P_INDEX.

*- Backup selected item index of internal table to global varialbe
  MOVE : LW_INDEX TO WA_SELECTED_INDEX.

  CHECK NOT LW_INDEX IS INITIAL.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
        MOVE : IT_ZSQM_QNS_CH_ISP-I_STAT  TO LW_I_STAT,
               IT_ZSQM_QNS_CH_ISP-MATNR   TO P_MATNR.
      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
        MOVE : IT_ZSQM_QNS_CH_ISE-I_STAT TO LW_I_STAT,
               IT_ZSQM_QNS_CH_ISE-MATNR  TO P_MATNR.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
      MOVE : IT_ZSQM_QNS_CH_REG-I_STAT TO LW_I_STAT,
             IT_ZSQM_QNS_CH_REG-MATNR  TO P_MATNR.
  ENDCASE.

*  execute Function for creating inspection lot by item status.
  IF LW_I_STAT = C_RELEASE.
    P_ERROR = C_MARK.
    EXIT.
  ENDIF.

*--> Change item data : Planned date

  REFRESH IT_ZSQM_ALV_DET.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        LOOP AT IT_FIELDCAT WHERE DATATYPE = 'DATS'
                              AND FIELDNAME+0(5) = 'DATUV'.
          CLEAR IT_ZSQM_ALV_DET.

          MOVE : IT_FIELDCAT-FIELDNAME TO IT_ZSQM_ALV_DET-FIELDNAME,
                 IT_FIELDCAT-COLTEXT   TO IT_ZSQM_ALV_DET-COLUMNTEXT.

          CONCATENATE 'IT_ZSQM_QNS_CH_ISP-'
                       IT_ZSQM_ALV_DET-FIELDNAME
                         INTO LW_FLD_NAME.
          ASSIGN (LW_FLD_NAME) TO <LW_FS>.

          MOVE : <LW_FS> TO IT_ZSQM_ALV_DET-PLNDAT.

          APPEND IT_ZSQM_ALV_DET.
        ENDLOOP.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        LOOP AT IT_FIELDCAT WHERE DATATYPE = 'DATS'
                              AND FIELDNAME+0(5) = 'DATUV'.
          CLEAR IT_ZSQM_ALV_DET.

          MOVE : IT_FIELDCAT-FIELDNAME TO IT_ZSQM_ALV_DET-FIELDNAME,
                 IT_FIELDCAT-COLTEXT   TO IT_ZSQM_ALV_DET-COLUMNTEXT.

          CONCATENATE 'IT_ZSQM_QNS_CH_ISE-'
                       IT_ZSQM_ALV_DET-FIELDNAME
                         INTO LW_FLD_NAME.
          ASSIGN (LW_FLD_NAME) TO <LW_FS>.

          MOVE : <LW_FS> TO IT_ZSQM_ALV_DET-PLNDAT.

          APPEND IT_ZSQM_ALV_DET.
        ENDLOOP.
      ENDIF.
    WHEN C_INSP_TYPE_REGULAR.
      LOOP AT IT_FIELDCAT WHERE DATATYPE = 'DATS'
                            AND FIELDNAME+0(5) = 'DATUV'.
        CLEAR IT_ZSQM_ALV_DET.

        MOVE : IT_FIELDCAT-FIELDNAME TO IT_ZSQM_ALV_DET-FIELDNAME,
               IT_FIELDCAT-COLTEXT   TO IT_ZSQM_ALV_DET-COLUMNTEXT.

        CONCATENATE 'IT_ZSQM_QNS_CH_REG-'
                     IT_ZSQM_ALV_DET-FIELDNAME
                       INTO LW_FLD_NAME.
        ASSIGN (LW_FLD_NAME) TO <LW_FS>.

        MOVE : <LW_FS> TO IT_ZSQM_ALV_DET-PLNDAT.

        APPEND IT_ZSQM_ALV_DET.
      ENDLOOP.
  ENDCASE.

*-- Call Screen for changing of Planned date
  CALL SCREEN 9150 STARTING AT 20   5
                   ENDING   AT 80   25.

ENDFORM.                    " CHANGE_PLANNED_DATE_ITEM
*&------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT_9150  OUTPUT
*&------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT_9150 OUTPUT.

  IF GRID_CONTAINER_DET IS INITIAL. "/Not Created Control for ALV GRID
*- Create Container('GRID_CONTAINER') with Custom Control on screen
    CREATE OBJECT GRID_CONTAINER_DET
           EXPORTING CONTAINER_NAME = WA_CUSTOM_C_DET
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
                TXT1  = TEXT-EZY.
    ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
    CREATE OBJECT ALV_GRID_DET
           EXPORTING I_PARENT = GRID_CONTAINER_DET
                     I_APPL_EVENTS = 'X'.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID_9150.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM MASK_COL_OF_ALV_9150 TABLES IT_FIELDCAT_DET.

* Set field attribute/Text by by Inspection type(ISIR, Regular)

    PERFORM SET_FIELD_ATTR_ALV_9150  TABLES IT_FIELDCAT_DET.

*-- Set ALV to Ready to Input mode for create and change.

    CALL METHOD ALV_GRID_DET->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

*-- Set data cell attributes using Cell table for changind planned date
    PERFORM INITIAL_SET_FIELD_STYLE_DET.

**-   Set Excluding ALV toolbar Function code.
*    PERFORM SET_EXCLUDING_F_CODE  TABLES IT_TOOLBAR_EXCLUDING.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_NEW_TABLE_DATA_DET.

*--  Regist event for Edit
    IF SY-BATCH IS INITIAL.
      CALL METHOD ALV_GRID_DET->REGISTER_EDIT_EVENT
          EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.

**- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD ALV_GRID_DET->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID_DET.

  ENDIF.

*-- Refresh ALV

  IF NOT GRID_CONTAINER_DET IS INITIAL.

*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID_9150.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM MASK_COL_OF_ALV_9150 TABLES IT_FIELDCAT_DET.

* Set field attribute/Text by by Inspection type(ISIR, Regular)

    PERFORM SET_FIELD_ATTR_ALV_9150  TABLES IT_FIELDCAT_DET.

*-- Set ALV to Ready to Input mode for create and change.

    CALL METHOD ALV_GRID_DET->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

*-- Set data cell attributes using Cell table for changind planned date
    PERFORM INITIAL_SET_FIELD_STYLE_DET.

**-   Set Excluding ALV toolbar Function code.
*    PERFORM SET_EXCLUDING_F_CODE  TABLES IT_TOOLBAR_EXCLUDING.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_NEW_TABLE_DATA_DET.

*--  Regist event for Edit
    IF SY-BATCH IS INITIAL.
      CALL METHOD ALV_GRID_DET->REGISTER_EDIT_EVENT
          EXPORTING I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
    ENDIF.

**- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD ALV_GRID_DET->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID_DET.
  ENDIF.

ENDMODULE.                 " CREATE_ALV_OBJECT_9150  OUTPUT
*&------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_9150
*&------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_9150.

  CLEAR : WA_IS_LAYOUT.

*//-- Set Layout Structure

  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.    "/Style field name set
  WA_IS_LAYOUT-EDIT       = C_MARK.   "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE  = 'N'. "/mode for select col and row

  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  WA_IS_LAYOUT-NO_TOOLBAR = C_MARK.  "/No toolbar.


ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_9150
*&------------------------------------------------------------------*
*&      Form  MASK_COL_OF_ALV_9150
*&------------------------------------------------------------------*
FORM MASK_COL_OF_ALV_9150 TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.
  DATA : LW_STRUCTURE_NAME LIKE DD02L-TABNAME.


  LW_STRUCTURE_NAME = 'ZSQM_ALV_DET'.


* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = LW_STRUCTURE_NAME
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

ENDFORM.                    " MASK_COL_OF_ALV_9150
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_ATTR_ALV_9150
*&---------------------------------------------------------------------*
FORM SET_FIELD_ATTR_ALV_9150 TABLES PT_FIELDCAT TYPE LVC_T_FCAT.
*-- Set Field attributes of ALV :

  LOOP AT PT_FIELDCAT.

    IF PT_FIELDCAT-FIELDNAME = 'COLUMNTEXT'.
      PT_FIELDCAT-EMPHASIZE = 'C410'.
      PT_FIELDCAT-KEY       = C_MARK.
      MOVE  'MIC'(TZ2) TO : PT_FIELDCAT-COLTEXT,
                            PT_FIELDCAT-SCRTEXT_L,
                            PT_FIELDCAT-SCRTEXT_M,
                            PT_FIELDCAT-SCRTEXT_S.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'FIELDNAME'.
      PT_FIELDCAT-NO_OUT    = C_MARK.
    ENDIF.
    MODIFY PT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " SET_FIELD_ATTR_ALV_9150
*&-------------------------------------------------------------------*
*&      Form  INITIAL_SET_FIELD_STYLE_DET
*&------------------------------------------------------------------*
FORM INITIAL_SET_FIELD_STYLE_DET.
  DATA : LT_CELL_STYLE_TAB TYPE LVC_T_STYL.
  DATA : LW_LVC_S_STYLE     LIKE LVC_S_STYL.
  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_FLD_NAME(40) TYPE C.
  FIELD-SYMBOLS : <LW_FS>.

  LOOP AT IT_ZSQM_ALV_DET.
    LW_INDEX = SY-TABIX.
    REFRESH IT_ZSQM_ALV_DET-CELLTAB.

    LOOP AT IT_FIELDCAT_DET.
      CLEAR LW_LVC_S_STYLE.
      MOVE : IT_FIELDCAT_DET-FIELDNAME TO LW_LVC_S_STYLE-FIELDNAME.

      IF     IT_FIELDCAT_DET-FIELDNAME = 'COLUMNTEXT' .
        LW_LVC_S_STYLE-STYLE =
                        CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ELSE.
        LW_LVC_S_STYLE-STYLE =
                        CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ENDIF.

      INSERT LW_LVC_S_STYLE
                 INTO TABLE IT_ZSQM_ALV_DET-CELLTAB.
    ENDLOOP.

    MODIFY IT_ZSQM_ALV_DET INDEX LW_INDEX.
  ENDLOOP.

ENDFORM.                    " INITIAL_SET_FIELD_STYLE_DET
*&-----------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA_DET
*&------------------------------------------------------------------*
FORM SET_NEW_TABLE_DATA_DET.
  CALL METHOD ALV_GRID_DET->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING I_STRUCTURE_NAME = 'ZSQM_ALV_DET'
               IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*                       IT_TOOLBAR_EXCLUDING = IT_TOOLBAR_EXCLUDING[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT_DET[]
               IT_OUTTAB        = IT_ZSQM_ALV_DET[].

ENDFORM.                    " SET_NEW_TABLE_DATA_DET
*&------------------------------------------------------------------*
*&      Module  STATUS_9150  OUTPUT
*&-----------------------------------------------------------------*
MODULE STATUS_9150 OUTPUT.
  SET PF-STATUS '9150'.
  SET TITLEBAR  '9150'.

ENDMODULE.                 " STATUS_9150  OUTPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9150  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9150 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'ENTR'.
      PERFORM CHECK_CHAGNED_DATA_UPDATE.

      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9150  INPUT
*&------------------------------------------------------------------*
*&      Form  CHECK_CHAGNED_DATA_UPDATE
*&------------------------------------------------------------------*
FORM CHECK_CHAGNED_DATA_UPDATE.

  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_FLD_NAME(40) TYPE C.
  FIELD-SYMBOLS : <LW_FS>.

  DATA : LW_S_ISP LIKE IT_ZSQM_QNS_CH_ISP,
         LW_S_ISE LIKE IT_ZSQM_QNS_CH_ISE,
         LW_S_REG LIKE IT_ZSQM_QNS_CH_REG.

*  REFRESH IT_ZSQM_ALV_DET.

*-- Check whether any data was changed using between local workarea and
*--  selected item data. local workarea data will be filled using
*--  IT_ZSQM_ALV_DET and selected item data at internal table header

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        MOVE-CORRESPONDING : IT_ZSQM_QNS_CH_ISP TO LW_S_ISP.

        LOOP AT IT_ZSQM_ALV_DET.

          CONCATENATE 'LW_S_ISP-'
                       IT_ZSQM_ALV_DET-FIELDNAME
                                      INTO LW_FLD_NAME.
          ASSIGN (LW_FLD_NAME) TO <LW_FS>.

          MOVE : IT_ZSQM_ALV_DET-PLNDAT TO <LW_FS>.

        ENDLOOP.
*--  check data chaged using compare selected item header data and local
*--   workarea
        IF LW_S_ISP NE IT_ZSQM_QNS_CH_ISP.
          MOVE : SY-UNAME TO LW_S_ISP-AENAM,
                 SY-DATUM TO LW_S_ISP-AEDAT,
                 SY-UZEIT TO LW_S_ISP-AEZET.

          MOVE-CORRESPONDING LW_S_ISP TO IT_ZSQM_QNS_CH_ISP.

          MODIFY IT_ZSQM_QNS_CH_ISP INDEX WA_SELECTED_INDEX.

        ENDIF.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        MOVE-CORRESPONDING : IT_ZSQM_QNS_CH_ISE TO LW_S_ISE.

        LOOP AT IT_ZSQM_ALV_DET.

          CONCATENATE 'LW_S_ISE-'
                       IT_ZSQM_ALV_DET-FIELDNAME
                                      INTO LW_FLD_NAME.
          ASSIGN (LW_FLD_NAME) TO <LW_FS>.

          MOVE : IT_ZSQM_ALV_DET-PLNDAT TO <LW_FS>.

        ENDLOOP.
*--  check data chaged using compare selected item header data and local
*--   workarea
        IF LW_S_ISE NE IT_ZSQM_QNS_CH_ISE.
          MOVE : SY-UNAME TO LW_S_ISE-AENAM,
                 SY-DATUM TO LW_S_ISE-AEDAT,
                 SY-UZEIT TO LW_S_ISE-AEZET.

          MOVE-CORRESPONDING LW_S_ISE TO IT_ZSQM_QNS_CH_ISE.

          MODIFY IT_ZSQM_QNS_CH_ISE INDEX WA_SELECTED_INDEX.

        ENDIF.
      ENDIF.

    WHEN C_INSP_TYPE_REGULAR.
      MOVE-CORRESPONDING : IT_ZSQM_QNS_CH_REG TO LW_S_ISP.

      LOOP AT IT_ZSQM_ALV_DET.

        CONCATENATE 'LW_S_REG-'
                     IT_ZSQM_ALV_DET-FIELDNAME
                                    INTO LW_FLD_NAME.
        ASSIGN (LW_FLD_NAME) TO <LW_FS>.

        MOVE : IT_ZSQM_ALV_DET-PLNDAT TO <LW_FS>.

      ENDLOOP.
*--  check data chaged using compare selected item header data and local
*--   workarea
      IF LW_S_REG NE IT_ZSQM_QNS_CH_REG.
        MOVE : SY-UNAME TO LW_S_REG-AENAM,
               SY-DATUM TO LW_S_REG-AEDAT,
               SY-UZEIT TO LW_S_REG-AEZET.

        MOVE-CORRESPONDING LW_S_REG TO IT_ZSQM_QNS_CH_REG.

        MODIFY IT_ZSQM_QNS_CH_REG INDEX WA_SELECTED_INDEX.

      ENDIF.
  ENDCASE.

  WA_RENEWAL_FLG = C_MARK. "/renewal ALV data


ENDFORM.                    " CHECK_CHAGNED_DATA_UPDATE
*&------------------------------------------------------------------*
*&      Form  FILL_CHANGE_TIME_STAMP
*&------------------------------------------------------------------*
FORM FILL_CHANGE_TIME_STAMP  USING P_ROW_ID.
  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_ISP LIKE IT_ZSQM_QNS_CH_ISP,
         LW_ISE LIKE IT_ZSQM_QNS_CH_ISE,
         LW_REG LIKE IT_ZSQM_QNS_CH_REG.

  LW_INDEX = P_ROW_ID.

  CASE ZSQM_QNS_HDR-ART.

    WHEN C_INSP_TYPE_ISIR.
      IF     ZSQM_QNS_HDR-WERKS+0(1) = 'P'.
        READ TABLE  IT_ZSQM_QNS_CH_ISP INDEX LW_INDEX.
*                     INTO IT_ZSQM_QNS_CH_ISP.
        IF NOT IT_ZSQM_QNS_CH_ISP-ERDAT IS INITIAL.
          MOVE : SY-DATUM TO IT_ZSQM_QNS_CH_ISP-AEDAT,
                 SY-UNAME TO IT_ZSQM_QNS_CH_ISP-AENAM,
                 SY-UZEIT TO IT_ZSQM_QNS_CH_ISP-AEZET.
        ELSE.
          MOVE : SY-DATUM TO IT_ZSQM_QNS_CH_ISP-ERDAT,
                 SY-UNAME TO IT_ZSQM_QNS_CH_ISP-ERNAM,
                 SY-UZEIT TO IT_ZSQM_QNS_CH_ISP-ERZET.
        ENDIF.

        MODIFY IT_ZSQM_QNS_CH_ISP  "FROM IT_ZSQM_QNS_CH_ISP
                                     INDEX  LW_INDEX.

      ELSEIF ZSQM_QNS_HDR-WERKS+0(1) = 'E'.
        READ TABLE  IT_ZSQM_QNS_CH_ISE INDEX LW_INDEX.
*                     INTO IT_ZSQM_QNS_CH_ISE.
        IF NOT IT_ZSQM_QNS_CH_ISE-ERDAT IS INITIAL.
          MOVE : SY-DATUM TO IT_ZSQM_QNS_CH_ISE-AEDAT,
                 SY-UNAME TO IT_ZSQM_QNS_CH_ISE-AENAM,
                 SY-UZEIT TO IT_ZSQM_QNS_CH_ISE-AEZET.
        ELSE.
          MOVE : SY-DATUM TO IT_ZSQM_QNS_CH_ISE-ERDAT,
                 SY-UNAME TO IT_ZSQM_QNS_CH_ISE-ERNAM,
                 SY-UZEIT TO IT_ZSQM_QNS_CH_ISE-ERZET.
        ENDIF.
        MODIFY IT_ZSQM_QNS_CH_ISE  "FROM IT_ZSQM_QNS_CH_ISE
                                     INDEX  LW_INDEX.
      ENDIF.

    WHEN C_INSP_TYPE_REGULAR.
      READ TABLE  IT_ZSQM_QNS_CH_REG INDEX LW_INDEX.
*                   INTO IT_ZSQM_QNS_CH_REG.
      IF NOT IT_ZSQM_QNS_CH_REG-ERDAT IS INITIAL.
        MOVE : SY-DATUM TO IT_ZSQM_QNS_CH_REG-AEDAT,
               SY-UNAME TO IT_ZSQM_QNS_CH_REG-AENAM,
               SY-UZEIT TO IT_ZSQM_QNS_CH_REG-AEZET.
      ELSE.
        MOVE : SY-DATUM TO IT_ZSQM_QNS_CH_REG-ERDAT,
               SY-UNAME TO IT_ZSQM_QNS_CH_REG-ERNAM,
               SY-UZEIT TO IT_ZSQM_QNS_CH_REG-ERZET.
      ENDIF.
      MODIFY IT_ZSQM_QNS_CH_REG  "FROM IT_ZSQM_QNS_CH_REG
                                   INDEX  LW_INDEX.
  ENDCASE.



ENDFORM.                    " FILL_CHANGE_TIME_STAMP
