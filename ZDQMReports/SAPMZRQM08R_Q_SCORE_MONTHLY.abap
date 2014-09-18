************************************************************************
* Program Name      : SAPMZRQM08R_Q_SCORE_MONTHLY
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.29.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No :
* Addl Documentation:
* Description       : Quality Score report by monthly
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZRQM08R_Q_SCORE_MONTHLY  NO STANDARD PAGE HEADING   .


*&&& Data Declaration.  &&&*
*TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
*TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
*INCLUDE <ICON>.

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : ZTQM_Q_SCORE, "/Quality Score Table - AQM05
         MARA,         "/Material Master
         LFA1,         "/Vendor Master
         MSEG,         "/Document Segment: Material
         QMEL,         "/Quality Notification
         MKPF.         "/Header: Material Document

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_QS_MON, "/RQM08-Quality Score by Monthly Str.
         ZSQM_QS_LIST. "/Quality Score data collected list Str for SQL.

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
*             C_SAVED(8)     TYPE C VALUE 'SAVED'.


**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
DATA : WA_MODE(7) TYPE C,
       WA_STATUS(8) TYPE C.

*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*-- Work area Variables in Program.(WA_xxxx)
DATA : WA_FIRST_DATE LIKE SY-DATUM.

DATA : WA_EWBEZ	TYPE EWBEZ,  "/Ext. Material Group Text
       WA_NAME1 TYPE NAME1_GP. "/Vendor Text

DATA : WA_BUDAT_LOW  LIKE SY-DATUM,
       WA_BUDAT_HIGH LIKE SY-DATUM.

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Screnn field cursor control
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT

*/-- Internale Tables with structure as sama as DB
*- Quality Score by Vendor
DATA : IT_ZSQM_QS_MON LIKE ZSQM_QS_MON OCCURS 0 WITH HEADER LINE.
*- Internal table for collect data of GR/GI
DATA : IT_ZSQM_QS_LIST  LIKE ZSQM_QS_LIST  OCCURS 0 WITH HEADER LINE.
*- Internal table for collect data of quality score from  ZTQM_Q_SCORE
DATA :  IT_ZTQM_Q_SCORE LIKE ZTQM_Q_SCORE OCCURS 0 WITH HEADER LINE.

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

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS : LCL_EVENT_RECEIVER DEFINITION DEFERRED.

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
*- Year
PARAMETERS : P_YEAR  TYPE MJAHR OBLIGATORY DEFAULT SY-DATUM+0(4).
*- Vendor
SELECT-OPTIONS : S_LIFNR FOR LFA1-LIFNR NO-EXTENSION NO INTERVALS.
*- External Material Group
SELECT-OPTIONS : S_EXTWG  FOR MARA-EXTWG
                                      NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

AT SELECTION-SCREEN ON BLOCK BLK.
  CHECK SY-UCOMM = 'ONLI'.

  CONCATENATE  P_YEAR '0101' INTO WA_BUDAT_LOW. "/First day of year
  CONCATENATE  P_YEAR '1231' INTO WA_BUDAT_HIGH. "/Last day of year

*-- get quality score data from DB.
  PERFORM GET_DATA_FROM_DB.

  IF IT_ZSQM_QS_LIST[] IS INITIAL.
    MESSAGE E000(ZMQM) WITH 'No entries!'(E01).
    EXIT.
  ENDIF.

*-- Get text of ext. material group from table TWEWT
  PERFORM GET_EXT_MAT_GROUP_TEXT  USING S_EXTWG-LOW
                                        WA_EWBEZ.

*-- Get text of Vendor from table LFA1
  PERFORM GET_VENDOR_TEXT    USING S_LIFNR-LOW
                                   WA_NAME1.

START-OF-SELECTION.
  CHECK NOT IT_ZSQM_QS_LIST[] IS INITIAL.

*-- Collect data .
  PERFORM COLLECT_DATA_BY.

*-- Calculate Quality score
  PERFORM CALCULATE_QUAL_SCORE.


**-- End of Selection.
END-OF-SELECTION.
  CHECK NOT IT_ZSQM_QS_LIST[] IS INITIAL.

  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.

*  WA_FIRST_DATE = SY-DATUM.
*  WA_FIRST_DATE+6(2) = '01'.


INITIALIZATION.


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

*    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
*
**         append a separator('3') to normal toolbar
*        CLEAR LS_TOOLBAR.
*        MOVE 3 TO LS_TOOLBAR-BUTN_TYPE.
*        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
**         append an icon to show detail List of selected item.
*        CLEAR LS_TOOLBAR.
*        MOVE 0 TO LS_TOOLBAR-BUTN_TYPE. "/ Button Type
*        MOVE 'DETAIL'           TO LS_TOOLBAR-FUNCTION.
*        MOVE ICON_DETAIL        TO LS_TOOLBAR-ICON.
*        MOVE 'Show detail'(T12) TO LS_TOOLBAR-QUICKINFO.
*        MOVE 'Detail'(T13)      TO LS_TOOLBAR-TEXT.
*        MOVE ' '                TO LS_TOOLBAR-DISABLED.
*
*        APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  ENDMETHOD.
*-------------------------------------------------------------------

*-- / Handling User defined commands for Toolbar
  METHOD HANDLE_USER_COMMAND.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.

    DATA : LT_ROWS   TYPE LVC_T_ROW,
           LW_LINE_ROW LIKE LINE OF LT_ROWS.
    DATA : LW_LINES TYPE I.


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

*          PERFORM RETRIEV_DETAIL_DATA USING LW_LINE_ROW-INDEX.

    ENDIF.

  ENDMETHOD.                           "handle_user_command

ENDCLASS.
*
* lcl_event_receiver (Implementation)
*===================================================================

**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&-----------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&-----------------------------------------------------------------*
FORM GET_DATA_FROM_DB.
  REFRESH : IT_ZSQM_QS_MON, IT_ZSQM_QS_LIST, IT_ZTQM_Q_SCORE.

*-- Get GR/GI data from DB
  SELECT   A~BUDAT B~BWART B~MEINS
          SUM( B~MENGE ) AS MENGE_GR
          SUM( C~RKMNG ) AS RKMNG
       INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QS_LIST
         FROM ( ( ( MKPF AS A        INNER JOIN MSEG AS B
            ON   A~MBLNR = B~MBLNR
             AND A~MJAHR = B~MJAHR ) INNER JOIN MARA AS D
            ON   B~MATNR = D~MATNR ) INNER JOIN MARC AS E
            ON   D~MATNR = E~MATNR
             AND B~WERKS = E~WERKS ) LEFT OUTER JOIN QMEL AS C
            ON   B~MBLNR = C~MBLNR
             AND B~MJAHR = C~MJAHR
             AND B~ZEILE = C~MBLPO
           WHERE  A~MJAHR = P_YEAR
             AND A~BUDAT BETWEEN WA_BUDAT_LOW AND WA_BUDAT_HIGH
             AND B~LIFNR IN S_LIFNR
             AND D~EXTWG IN S_EXTWG
             AND E~QMATV = C_MARK
             AND B~BWART IN ('101', '102',            "/GR
                              '261', '901', '903',    "/GI
                              '262', '902', '904')
            GROUP BY A~BUDAT B~BWART B~MEINS .

  CHECK     SY-SUBRC = 0 AND
        NOT IT_ZSQM_QS_LIST[] IS INITIAL.
*-- Get Quality Score data from CBO Table

  SELECT  A~ISSUEDAT  A~MEINH  A~MEINS
         SUM( A~LINESTOP ) AS LINESTOP
         SUM( A~QNT_CAMP ) AS QNT_CAMP
         SUM( A~QNT_SALV ) AS QNT_SALV
         SUM( A~QNT_REPR ) AS QNT_REPR
     INTO CORRESPONDING FIELDS OF TABLE IT_ZTQM_Q_SCORE
       FROM ( ZTQM_Q_SCORE AS A INNER JOIN MARA AS B
          ON  A~MATNR = B~MATNR ) INNER JOIN MARC AS E
          ON  B~MATNR = E~MATNR
         WHERE A~ISSUEDAT BETWEEN WA_BUDAT_LOW AND WA_BUDAT_HIGH
           AND A~LIFNR    IN S_LIFNR
           AND B~EXTWG    IN S_EXTWG
          GROUP BY A~ISSUEDAT  A~MEINH  A~MEINS.



ENDFORM.                    " GET_DATA_FROM_DB
*&------------------------------------------------------------------*
*&      Form  COLLECT_DATA_BY
*&------------------------------------------------------------------*
FORM COLLECT_DATA_BY.
  DATA : LW_GRGI_INDEX LIKE SY-TABIX,
         LW_SCORE_INDEX LIKE SY-TABIX.

  DATA : LW_ZSQM_QS_LIST LIKE IT_ZSQM_QS_LIST.

  SORT IT_ZSQM_QS_LIST  BY BUDAT    ASCENDING.
  SORT IT_ZTQM_Q_SCORE  BY ISSUEDAT ASCENDING.

  REFRESH : IT_ZSQM_QS_MON.

  LOOP AT IT_ZSQM_QS_LIST.
    LW_GRGI_INDEX = SY-TABIX.

    MOVE-CORRESPONDING IT_ZSQM_QS_LIST TO LW_ZSQM_QS_LIST.

    CLEAR : IT_ZSQM_QS_MON.

    MOVE-CORRESPONDING LW_ZSQM_QS_LIST TO IT_ZSQM_QS_MON.

    IT_ZSQM_QS_MON-MONTH = LW_ZSQM_QS_LIST-BUDAT+4(2). "/Month
    IT_ZSQM_QS_MON-MEINH = 'MIN'.

*   '101', '102',            "/GR
*   '261', '901', '903',    "/GI
*   '262', '902', '904'
    CASE LW_ZSQM_QS_LIST-BWART.
      WHEN '101'.
*       N/A
      WHEN '102'.
        IT_ZSQM_QS_MON-MENGE_GR = IT_ZSQM_QS_MON-MENGE_GR * -1.
      WHEN '261' OR '901' OR '903'.
        IT_ZSQM_QS_MON-MENGE_GI = IT_ZSQM_QS_MON-MENGE_GR.
        CLEAR IT_ZSQM_QS_MON-MENGE_GR.
      WHEN '262' OR '902' OR '904'.
        IT_ZSQM_QS_MON-MENGE_GI = IT_ZSQM_QS_MON-MENGE_GR.
        IT_ZSQM_QS_MON-MENGE_GI = IT_ZSQM_QS_MON-MENGE_GI * -1.
        CLEAR IT_ZSQM_QS_MON-MENGE_GR.
    ENDCASE.


    COLLECT IT_ZSQM_QS_MON.

  ENDLOOP.

  LOOP AT IT_ZTQM_Q_SCORE.
    CLEAR : IT_ZSQM_QS_MON.

    MOVE-CORRESPONDING IT_ZTQM_Q_SCORE TO IT_ZSQM_QS_MON.

    IT_ZSQM_QS_MON-MONTH = IT_ZTQM_Q_SCORE-ISSUEDAT+4(2). "/Month

    COLLECT IT_ZSQM_QS_MON.
  ENDLOOP.

ENDFORM.                    " COLLECT_DATA_BY
*&------------------------------------------------------------------*
*&      Form  CALCULATE_QUAL_SCORE
*&------------------------------------------------------------------*
FORM CALCULATE_QUAL_SCORE.

  LOOP AT IT_ZSQM_QS_MON.

    IF NOT IT_ZSQM_QS_MON-MENGE_GI IS INITIAL.

      IT_ZSQM_QS_MON-Q_SCORE =
            ( IT_ZSQM_QS_MON-RKMNG / IT_ZSQM_QS_MON-MENGE_GI )
           * 1000000
           + (  IT_ZSQM_QS_MON-LINESTOP + IT_ZSQM_QS_MON-QNT_CAMP
              + IT_ZSQM_QS_MON-QNT_SALV + IT_ZSQM_QS_MON-QNT_REPR ).

    ENDIF.

    MODIFY IT_ZSQM_QS_MON.
  ENDLOOP.

ENDFORM.                    " CALCULATE_QUAL_SCORE

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
*  LOOP AT SCREEN.
*    IF SCREEN-GROUP1 = 'Q23'.
*      SCREEN-ACTIVE = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
*&------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Container for ALV GRID
*    CLEAR WA_RENEWAL_FLG.
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
*    WA_VARIANT-VARIANT = '/ZRQM21_CLAS'.
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZSQM_QS_MON'
                   IS_LAYOUT        = WA_IS_LAYOUT
                   I_SAVE           = WA_SAVE
                   IS_VARIANT       = WA_VARIANT
                   I_DEFAULT        = SPACE
*            IT_TOOLBAR_EXCLUDING  = <internal table of type
*                                                          UI_FUNCTIONS>
*            IT_HYPERLINK          = <internal table of type LVC_T_HYPE>
*            IT_ALV_GRAPHICS       = <internal table of type DTC_T_TC>
         CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*            IT_SORT               = <internal table of type LVC_T_SORT>
                   IT_OUTTAB        = IT_ZSQM_QS_MON[].

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

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
*//-- Set Layout Structure


  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
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
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZSQM_QS_MON'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].


* Set field attribute

  LOOP AT PT_FIELDCAT.
    IF PT_FIELDCAT-FIELDNAME = 'MONTH'.
      PT_FIELDCAT-KEY_SEL = C_MARK.
      PT_FIELDCAT-KEY     = C_MARK.
      PT_FIELDCAT-COLTEXT = 'No'(T60).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'MENGE_GR'.
      PT_FIELDCAT-COLTEXT = 'GR quantity'(T51).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'MENGE_GI'.
      PT_FIELDCAT-COLTEXT = 'GI quantity'(T52).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'RKMNG'.
      PT_FIELDCAT-COLTEXT = 'Defect'(T53).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'LINESTOP'.
      PT_FIELDCAT-COLTEXT = 'Line stop'(T54).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'QNT_CAMP'.
      PT_FIELDCAT-COLTEXT = 'Campaign'(T55).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'QNT_SALV'.
      PT_FIELDCAT-COLTEXT = 'Salvage'(T56).
    ELSEIF PT_FIELDCAT-FIELDNAME = 'QNT_REPR'.
      PT_FIELDCAT-COLTEXT = 'Repair'(T57).
    ENDIF.
    MODIFY PT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
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
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
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
              TXT1  = TEXT-E01.
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
      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&-----------------------------------------------------------------*
*&      Form  GET_EXT_MAT_GROUP_TEXT
*&-------------------------------------------------------------------*
FORM GET_EXT_MAT_GROUP_TEXT USING    P_EXTWG
                                     P_EWBEZ.

  CHECK NOT P_EXTWG IS INITIAL.

  SELECT SINGLE EWBEZ INTO P_EWBEZ
    FROM TWEWT
      WHERE EXTWG = P_EXTWG
        AND SPRAS = SY-LANGU.

ENDFORM.                    " GET_EXT_MAT_GROUP_TEXT
*&-----------------------------------------------------------------*
*&      Form  GET_VENDOR_TEXT
*&-----------------------------------------------------------------*
FORM GET_VENDOR_TEXT USING    P_LIFNR
                              P_NAME1.

CHECK NOT P_LIFNR IS INITIAL.
  SELECT SINGLE NAME1 INTO P_NAME1
    FROM LFA1
      WHERE LIFNR = P_LIFNR.
ENDFORM.                    " GET_VENDOR_TEXT
