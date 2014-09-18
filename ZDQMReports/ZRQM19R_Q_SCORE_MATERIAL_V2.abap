************************************************************************
* Program Name      : ZRQM19R_Q_SCORE_MATERIAL_V2
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.29.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No :
* Addl Documentation:
* Description       : Quality Score report by material
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 5/4/2012        t-code is deleted by APM Monitoring

************************************************************************

REPORT  ZRQM19R_Q_SCORE_MATERIAL_V2  NO STANDARD PAGE HEADING   .


*&&& Data Declaration.  &&&*
*TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
*TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : ZTQM_Q_SCORE, "/Quality Score Table - AQM05
         MARA,         "/Material Master
         LFA1,         "/Vendor Master
         MSEG,         "/Document Segment: Material
         QMEL,         "/Quality Notification
         MKPF.         "/Header: Material Document
TABLES : T001W.
*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_QS_MAT, "/Quality Score by Material Str.
         ZSQM_QS_LIST. "/Quality Score data collected list Str for SQL.


TABLES : ZSQM_QS_LIST_V2. "/Quality Score data list Str for SQL.

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
CONSTANTS : C_EXT_MAT_G(8)  TYPE C VALUE 'EXTWG',
            C_MATERIAL(8)   TYPE C VALUE 'MATERIAL'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
*DATA : WA_MODE(7) TYPE C,
*       WA_STATUS(8) TYPE C.

*-- Data Level control
DATA : WA_LEVEL(8) TYPE C   VALUE C_EXT_MAT_G,
       WA_EXTWG TYPE EXTWG. "/Selected Ext.material Group for Detail
DATA :  WA_RENEWAL_FLG.
*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*-- Work area Variables in Program.(WA_xxxx)
DATA : WA_FIRST_DATE LIKE SY-DATUM,
       WA_LAST_DATE  LIKE SY-DATUM.

DATA : WA_INT_DATE TYPE DATS. "/Date for interface

DATA : WA_EWBEZ	TYPE EWBEZ,  "/Ext. Material Group Text
       WA_NAME1 TYPE NAME1.  "/Plant name

DATA : WA_QMART  TYPE QMART VALUE 'Q2'. "/Noti Type for Defect data

*DATA : WA_BUDAT_LOW  LIKE SY-DATUM,
*       WA_BUDAT_HIGH LIKE SY-DATUM.

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?
*-- Screnn field cursor control
DATA : WA_FLDTXT    LIKE FELD-NAME,  "Field Name Variable
       WA_CUR_LINE  LIKE FELD-LINE.  "Field Line Variable

*//Internal Tables and Index Fields;(IT_), (I_)
*DATA : IT

*/-- Internale Tables with structure as sama as DB
*- Quality Score by Ext. Material Group
DATA : IT_ZSQM_QS_MAT LIKE ZSQM_QS_MAT OCCURS 0 WITH HEADER LINE.
*- Quality Score by Material of selected Ext. Material Group=>Detail
DATA : IT_ZSQM_QS_MAT_D LIKE ZSQM_QS_MAT OCCURS 0 WITH HEADER LINE.

*- Internal table for collect data of GR/GI
DATA : IT_ZSQM_QS_LIST  LIKE ZSQM_QS_LIST  OCCURS 0 WITH HEADER LINE.
*- Internal table for collect data of quality score from  ZTQM_Q_SCORE
DATA : BEGIN OF  IT_ZTQM_Q_SCORE OCCURS 0,
         EXTWG TYPE EXTWG,
         MAKTX TYPE MAKTX.
        INCLUDE STRUCTURE ZTQM_Q_SCORE.
DATA : END OF IT_ZTQM_Q_SCORE.

*-- Material Document data list
DATA : IT_ZSQM_QS_LIST_MD LIKE ZSQM_QS_LIST_V2 OCCURS 0
                                         WITH HEADER LINE.
*-- Notification Data List
DATA : IT_ZSQM_QS_LIST_NT LIKE ZSQM_QS_LIST_V2 OCCURS 0
                                         WITH HEADER LINE.
*-- Quality Score(CBO) Data List.
DATA : IT_ZSQM_QS_LIST_QS LIKE ZSQM_QS_LIST_V2 OCCURS 0
                                         WITH HEADER LINE.

*-- Quality Score(CBO) Data List_ Total(Collected data).
DATA : IT_ZSQM_QS_LIST_TT LIKE ZSQM_QS_LIST_V2 OCCURS 0
                                         WITH HEADER LINE.

*-- Quality Score BY external material Group
DATA : IT_ZSQM_QS_EXTWG LIKE ZSQM_QS_LIST_V2 OCCURS 0
                                         WITH HEADER LINE.

*--  Quality Score by Material Detail of EXTWG
DATA : IT_ZSQM_QS_MT LIKE ZSQM_QS_LIST_V2 OCCURS 0
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
*- Period
SELECT-OPTIONS : S_PERIOD  FOR ZTQM_Q_SCORE-ISSUEDAT OBLIGATORY
                            DEFAULT WA_FIRST_DATE TO WA_LAST_DATE
                                      NO-EXTENSION .
*-- Plant
SELECT-OPTIONS : S_WERKS   FOR T001W-WERKS NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

AT SELECTION-SCREEN ON BLOCK BLK.
  CHECK SY-UCOMM = 'ONLI'.

*-- get quality score data from DB.
  PERFORM GET_DATA_FROM_DB.

  IF IT_ZSQM_QS_LIST_MD[] IS INITIAL.
    MESSAGE E000(ZMQM) WITH 'No entries!'(E01).
    EXIT.
  ENDIF.

*-- Get text of Plant from T001w
  PERFORM GET_PLANT_TEXT    USING S_WERKS-LOW
                                  WA_NAME1.

START-OF-SELECTION.
  CHECK NOT IT_ZSQM_QS_LIST_MD[] IS INITIAL.

*-- Collect data .
  PERFORM COLLECT_DATA_BY_V2.

*-- BY External material Group.

  PERFORM COLLECT_BY_EXTWG.

*-- Calculate Quality score
  PERFORM CALC_QUAL_SCORE  TABLES IT_ZSQM_QS_EXTWG.

**-- End of Selection.
END-OF-SELECTION.
  CHECK NOT IT_ZSQM_QS_EXTWG[] IS INITIAL.

  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.

*-- Get Selection data for Selection from Memory ID using IMPORT
  IMPORT WA_INT_DATE  FROM MEMORY ID 'BY_MATERIAL'.

  IF SY-SUBRC = 0.  "/Import data exist.
    FREE MEMORY ID 'BY_MATERIAL'.
    WA_FIRST_DATE = WA_INT_DATE.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              DAY_IN            = WA_FIRST_DATE
         IMPORTING
              LAST_DAY_OF_MONTH = WA_LAST_DATE
         EXCEPTIONS
              DAY_IN_NO_DATE    = 1
              OTHERS            = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    WA_FIRST_DATE = SY-DATUM.
    WA_FIRST_DATE+6(2) = '01'.
    WA_LAST_DATE = SY-DATUM.
  ENDIF.


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

    PERFORM RETRIEV_DETAIL_DATA USING E_ROW-INDEX.

*-         Get text of ext. material group from table TWEWT
    PERFORM GET_EXT_MAT_GROUP_TEXT  USING WA_EXTWG
                                          WA_EWBEZ.

    WA_RENEWAL_FLG = C_MARK.
  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
    CASE WA_LEVEL.
      WHEN C_EXT_MAT_G.
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

*-         Get text of ext. material group from table TWEWT
          PERFORM GET_EXT_MAT_GROUP_TEXT  USING WA_EXTWG
                                                WA_EWBEZ.

          WA_RENEWAL_FLG = C_MARK.
        ENDIF.

      WHEN OTHERS.
*      N/A
    ENDCASE.

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
  DATA : LT_ZSQM_QS_LIST_MD  LIKE ZSQM_QS_LIST_V2 OCCURS 0
                                             WITH HEADER LINE.

*-- Get GR/GI data from DB
*-- There is no material supplied by multi vendor. "/Notice
*// Modified by sllee : Change SQL Logic by Mr Kim 02/13/2004-Start
*-   Using Purchasing source List(table : 'EORD')
*  SELECT B~BWART C~EXTWG B~MATNR E~MAKTX B~MEINS
*         B~LIFNR F~NAME1 AS NAME_LF
*        SUM( MENGE    ) AS MENGE_GR
*    INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QS_LIST_MD
*      FROM ( ( ( ( MKPF AS A INNER JOIN  MSEG AS B
*     ON   A~MBLNR = B~MBLNR
*          AND A~MJAHR = B~MJAHR ) INNER JOIN MARA AS C
*         ON   B~MATNR = C~MATNR ) INNER JOIN MARC AS D
*         ON   C~MATNR = D~MATNR
*          AND B~WERKS = D~WERKS ) INNER JOIN MAKT AS E
*         ON   C~MATNR = E~MATNR ) LEFT OUTER JOIN LFA1 AS F
*         ON   B~LIFNR = F~LIFNR
*       WHERE  A~BUDAT IN S_PERIOD
*         AND  B~WERKS IN S_WERKS
*         AND  D~QMATV = C_MARK
*         AND  E~SPRAS = SY-LANGU
*         AND  B~BWART IN ('101', '102',            "/GR
*                          '261', '901', '903',     "/GI
*                          '262', '902', '904')
*       GROUP BY B~BWART C~EXTWG B~MATNR E~MAKTX B~MEINS
*                B~LIFNR F~NAME1.

  SELECT B~BWART C~EXTWG B~MATNR E~MAKTX B~MEINS
         G~LIFNR H~NAME1 AS NAME_LF
        SUM( MENGE    ) AS MENGE_GR
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QS_LIST_MD
      FROM ( ( ( ( ( MKPF AS A INNER JOIN  MSEG AS B
     ON   A~MBLNR = B~MBLNR
          AND A~MJAHR = B~MJAHR ) INNER JOIN MARA AS C
         ON   B~MATNR = C~MATNR ) INNER JOIN MARC AS D
         ON   C~MATNR = D~MATNR
*          AND B~WERKS = D~WERKS
                                ) INNER JOIN MAKT AS E
         ON   C~MATNR = E~MATNR ) INNER JOIN EORD AS G
         ON   D~MATNR = G~MATNR
          AND D~WERKS = G~WERKS ) INNER JOIN LFA1 AS H
         ON   G~LIFNR = H~LIFNR
       WHERE  B~ZBUDAT IN S_PERIOD "A~BUDAT IN S_PERIOD
         AND  G~WERKS IN S_WERKS
         AND  D~QMATV = C_MARK
         AND  E~SPRAS = SY-LANGU
*         AND  G~ZEORD = '00001'
         AND  B~BWART IN ('101', '102',            "/GR
                          '261', '901', '903',     "/GI
                          '262', '902', '904')
       GROUP BY B~BWART C~EXTWG B~MATNR E~MAKTX B~MEINS
                G~LIFNR H~NAME1.

*// Modified by sllee : Change SQL Logic by Mr Kim 02/13/2004-End

  LT_ZSQM_QS_LIST_MD[] = IT_ZSQM_QS_LIST_MD[].

  REFRESH IT_ZSQM_QS_LIST_MD.

  LOOP AT LT_ZSQM_QS_LIST_MD.

    MOVE-CORRESPONDING LT_ZSQM_QS_LIST_MD TO IT_ZSQM_QS_LIST_MD.

*   '101', '102',            "/GR
*   '261', '901', '903',    "/GI
*   '262', '902', '904'
    CASE LT_ZSQM_QS_LIST_MD-BWART.
      WHEN '101'.
*       N/A
      WHEN '102'.
        IT_ZSQM_QS_LIST_MD-MENGE_GR = IT_ZSQM_QS_LIST_MD-MENGE_GR * -1.
      WHEN '261' OR '901' OR '903'.
        IT_ZSQM_QS_LIST_MD-MENGE_GI = IT_ZSQM_QS_LIST_MD-MENGE_GR.
        CLEAR IT_ZSQM_QS_LIST_MD-MENGE_GR.
      WHEN '262' OR '902' OR '904'.
        IT_ZSQM_QS_LIST_MD-MENGE_GI = IT_ZSQM_QS_LIST_MD-MENGE_GR.
        IT_ZSQM_QS_LIST_MD-MENGE_GI = IT_ZSQM_QS_LIST_MD-MENGE_GI * -1.
        CLEAR IT_ZSQM_QS_LIST_MD-MENGE_GR.
    ENDCASE.

    CLEAR   IT_ZSQM_QS_LIST_MD-BWART.

    COLLECT IT_ZSQM_QS_LIST_MD.
  ENDLOOP.

*-- Get Defect data from  Notification (Complaint Quantity)

  SELECT B~EXTWG A~MATNR D~MAKTX A~MGEIN AS MEINS
       SUM( A~RKMNG ) AS RKMNG
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QS_LIST_NT
       FROM ( ( QMEL AS A INNER JOIN MARA AS B
          ON   A~MATNR = B~MATNR   ) INNER JOIN MARC AS C
          ON   B~MATNR = C~MATNR
*           AND A~MAWERK = C~WERKS
                                   ) INNER JOIN MAKT AS D
          ON   C~MATNR = D~MATNR
         WHERE  C~QMATV = C_MARK
           AND A~ERDAT  IN S_PERIOD
           AND A~MAWERK IN S_WERKS
           AND A~QMART  = WA_QMART
           AND D~SPRAS  = SY-LANGU
        GROUP BY B~EXTWG A~MATNR D~MAKTX A~MGEIN .

*-- Get Quality score etc. from CBO(ZTQM_Q_SCORE).
  SELECT B~EXTWG A~MATNR F~MAKTX A~MEINH  A~MEINS
         SUM( A~LINESTOP ) AS LINESTOP
         SUM( A~QNT_CAMP ) AS QNT_CAMP
         SUM( A~QNT_SALV ) AS QNT_SALV
         SUM( A~QNT_REPR ) AS QNT_REPR
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_QS_LIST_QS
       FROM ( ( ZTQM_Q_SCORE AS A INNER JOIN MARA AS B
          ON  A~MATNR = B~MATNR ) INNER JOIN MARC AS E
          ON  B~MATNR = E~MATNR ) INNER JOIN MAKT AS F
          ON  B~MATNR = F~MATNR
         WHERE A~ISSUEDAT IN S_PERIOD
           AND E~WERKS    IN S_WERKS
           AND F~SPRAS = SY-LANGU
          GROUP BY B~EXTWG  A~MATNR F~MAKTX A~MEINH  A~MEINS.


ENDFORM.                    " GET_DATA_FROM_DB
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
  CHECK WA_LEVEL = C_EXT_MAT_G.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'DET'.
      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MODIFY_SCREEN_9000  OUTPUT
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
    PERFORM SET_SORT_TOTAL_FIELD.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
*    WA_VARIANT-VARIANT = '/ZRQM21_CLAS'.
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

  IF NOT GRID_CONTAINER IS INITIAL AND "/Created Container for ALV GRID
     NOT WA_RENEWAL_FLG IS INITIAL.
    CLEAR WA_RENEWAL_FLG.
*-- Prepare Setting Attributes and etc of ALV Object
    PERFORM SET_ATTRIBUTES_ALV_GRID.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    PERFORM MASK_COLUMNS_OF_ALV_GRID TABLES IT_FIELDCAT.

*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
    PERFORM SET_TABLE_TO_ALV.

    PERFORM REFRESH_ALV_GRID_DATA_DISP.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.

  ENDIF.

ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
*  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
*//-- Set Layout Structure


  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = C_MARK.      "/Optimize column width
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

*  IF WA_LEVEL = C_EXT_MAT_G.
  WA_IS_LAYOUT-SEL_MODE  = 'A'. "/mode for select col and row
*  ELSEIF WA_LEVEL = C_MATERIAL.
*    WA_IS_LAYOUT-SEL_MODE  = ' '. "/mode for select col and row
*  ENDIF.

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
            I_STRUCTURE_NAME = 'ZSQM_QS_LIST_V2'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

* Set field attribute

  LOOP AT PT_FIELDCAT.

    CASE WA_LEVEL.
      WHEN C_EXT_MAT_G.
        IF PT_FIELDCAT-FIELDNAME = 'EXTWG'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-KEY     = C_MARK.
          PT_FIELDCAT-OUTPUTLEN = 12.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'MATNR' OR
               PT_FIELDCAT-FIELDNAME = 'MAKTX' OR
               PT_FIELDCAT-FIELDNAME = 'BWART'.
          PT_FIELDCAT-NO_OUT = C_MARK.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'LIFNR'   OR
               PT_FIELDCAT-FIELDNAME = 'NAME_LF'.
          PT_FIELDCAT-NO_OUT = C_MARK.
        ENDIF.
      WHEN C_MATERIAL.
        IF PT_FIELDCAT-FIELDNAME = 'MATNR'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-KEY     = C_MARK.
          PT_FIELDCAT-OUTPUTLEN = 18.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'MAKTX'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-KEY     = C_MARK.
          PT_FIELDCAT-COLTEXT = 'Material Text'(T60).
        ELSEIF PT_FIELDCAT-FIELDNAME = 'EXTWG' OR
               PT_FIELDCAT-FIELDNAME = 'BWART'.
          PT_FIELDCAT-NO_OUT = C_MARK.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'LIFNR' OR
               PT_FIELDCAT-FIELDNAME = 'NAME_LF'.
          IF PT_FIELDCAT-FIELDNAME = 'LIFNR'.
          ELSEIF PT_FIELDCAT-FIELDNAME = 'NAME_LF'.
            PT_FIELDCAT-COLTEXT = 'Vendor Name'(T61).
          ENDIF.
        ENDIF.
    ENDCASE.

    IF PT_FIELDCAT-FIELDNAME = 'MENGE_GR'.
      PT_FIELDCAT-COLTEXT = 'GR quantity'(T51).
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'MENGE_GI'.
      PT_FIELDCAT-COLTEXT = 'GI quantity'(T52).
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'RKMNG'.
      PT_FIELDCAT-COLTEXT = 'Defect'(T53).
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'Q_PPM'.
      PT_FIELDCAT-EMPHASIZE = 'C711'.
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'LINESTOP'.
      PT_FIELDCAT-COLTEXT = 'Line stop'(T54).
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'QNT_CAMP'.
      PT_FIELDCAT-COLTEXT = 'Campaign'(T55).
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'QNT_SALV'.
      PT_FIELDCAT-COLTEXT = 'Salvage'(T56).
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'QNT_REPR'.
      PT_FIELDCAT-COLTEXT = 'Repair'(T57).
      PT_FIELDCAT-OUTPUTLEN = 12.
    ELSEIF PT_FIELDCAT-FIELDNAME = 'Q_SCORE'.
      PT_FIELDCAT-EMPHASIZE = 'C310'.
      PT_FIELDCAT-OUTPUTLEN = 12.
    ENDIF.

    IF     ( PT_FIELDCAT-DATATYPE = 'QUAN'    OR
             PT_FIELDCAT-DATATYPE = 'INT4'      ) AND
       NOT ( PT_FIELDCAT-FIELDNAME = 'Q_PPM'  OR
             PT_FIELDCAT-FIELDNAME = 'Q_SCORE'  ).

      PT_FIELDCAT-DO_SUM = C_MARK.
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
      IF WA_LEVEL = C_EXT_MAT_G.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN 'RW'.
      IF WA_LEVEL = C_EXT_MAT_G.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

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
      IF WA_LEVEL = C_EXT_MAT_G.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN 'REFRESH'.
      REFRESH IT_ZSQM_QS_LIST_TT.
*--   get quality score data from DB.
      PERFORM GET_DATA_FROM_DB.

*--   Get text of Plant from T001w
      PERFORM GET_PLANT_TEXT    USING S_WERKS-LOW
                                      WA_NAME1.

*--    Collect data .
      PERFORM COLLECT_DATA_BY_V2.

      CASE WA_LEVEL.
        WHEN C_EXT_MAT_G.

          REFRESH IT_ZSQM_QS_EXTWG.

*--        BY External material Group.
          PERFORM COLLECT_BY_EXTWG.

*--        Calculate Quality score
          PERFORM CALC_QUAL_SCORE  TABLES IT_ZSQM_QS_EXTWG.

        WHEN C_MATERIAL.
*--       Collect data .
          PERFORM COLLECT_DATA_BY_DETAIL USING WA_EXTWG.

*--        Calculate Quality score
          PERFORM CALC_QUAL_SCORE TABLES IT_ZSQM_QS_MT.

      ENDCASE.

      WA_RENEWAL_FLG = C_MARK.

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
*&-----------------------------------------------------------------*
*&      Form  GET_PLANT_TEXT
*&-----------------------------------------------------------------*
FORM GET_PLANT_TEXT USING    P_WERKS
                             P_NAME1.
  CHECK NOT P_WERKS IS INITIAL.

  SELECT SINGLE NAME1 INTO P_NAME1
    FROM T001W
      WHERE WERKS = P_WERKS.

ENDFORM.                    " GET_PLANT_TEXT
*&-----------------------------------------------------------------*
*&      Form  SET_TABLE_TO_ALV
*&-----------------------------------------------------------------*
FORM SET_TABLE_TO_ALV.
  CASE WA_LEVEL.
    WHEN C_EXT_MAT_G.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSQM_QS_LIST_V2'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = WA_SAVE
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                     IT_SORT          = IT_SORT[]
                     IT_OUTTAB        = IT_ZSQM_QS_EXTWG[].
    WHEN C_MATERIAL.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSQM_QS_LIST_V2'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = WA_SAVE
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                     IT_SORT          = IT_SORT[]
                     IT_OUTTAB        = IT_ZSQM_QS_MT[].
  ENDCASE.

ENDFORM.                    " SET_TABLE_TO_ALV
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
*&---------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA
*&---------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA USING    P_INDEX.
  DATA : LW_SEL_INDEX   LIKE SY-TABIX.
  DATA : LW_TABLE_LINES LIKE SY-TABIX.

  LW_SEL_INDEX = P_INDEX.

  CASE WA_LEVEL.
    WHEN C_EXT_MAT_G.
      CLEAR IT_ZSQM_QS_EXTWG.
      READ TABLE IT_ZSQM_QS_EXTWG INDEX LW_SEL_INDEX.
      CHECK SY-SUBRC  = 0.
      WA_EXTWG = IT_ZSQM_QS_EXTWG-EXTWG.

      WA_LEVEL = C_MATERIAL.

*-- Collect data .
      PERFORM COLLECT_DATA_BY_DETAIL USING WA_EXTWG.

*-- Calculate Quality score
      PERFORM CALC_QUAL_SCORE TABLES IT_ZSQM_QS_MT.

    WHEN C_MATERIAL.
*     N/A
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " RETRIEV_DETAIL_DATA
*&-----------------------------------------------------------------*
*&      Form  COLLECT_DATA_BY_DETAIL
*&-----------------------------------------------------------------*
FORM COLLECT_DATA_BY_DETAIL USING    P_EXTWG.
  DATA : LW_INDEX LIKE SY-TABIX.

  REFRESH IT_ZSQM_QS_MT.

  LOOP AT IT_ZSQM_QS_LIST_TT  WHERE EXTWG = P_EXTWG.
    CLEAR IT_ZSQM_QS_MT.

    READ TABLE IT_ZSQM_QS_MT WITH KEY MATNR = IT_ZSQM_QS_LIST_TT-MATNR.

    LW_INDEX = SY-TABIX.

    IF SY-SUBRC = 0.
      ADD : IT_ZSQM_QS_LIST_TT-MENGE_GR TO IT_ZSQM_QS_MT-MENGE_GR,
            IT_ZSQM_QS_LIST_TT-MENGE_GI TO IT_ZSQM_QS_MT-MENGE_GI,
            IT_ZSQM_QS_LIST_TT-RKMNG    TO IT_ZSQM_QS_MT-RKMNG,
            IT_ZSQM_QS_LIST_TT-LINESTOP TO IT_ZSQM_QS_MT-LINESTOP,
            IT_ZSQM_QS_LIST_TT-QNT_CAMP TO IT_ZSQM_QS_MT-QNT_CAMP,
            IT_ZSQM_QS_LIST_TT-QNT_SALV TO IT_ZSQM_QS_MT-QNT_SALV,
            IT_ZSQM_QS_LIST_TT-QNT_REPR TO IT_ZSQM_QS_MT-QNT_REPR.
      MODIFY IT_ZSQM_QS_MT INDEX LW_INDEX.
    ELSE.
      MOVE-CORRESPONDING IT_ZSQM_QS_LIST_TT TO IT_ZSQM_QS_MT.
      CLEAR IT_ZSQM_QS_MT-EXTWG.

      COLLECT IT_ZSQM_QS_MT.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " COLLECT_DATA_BY_DETAIL
*&-------------------------------------------------------------------*
*&      Form  SET_LIST_LEVEL_CONTROL
*&-------------------------------------------------------------------*
FORM SET_LIST_LEVEL_CONTROL.
  CASE WA_LEVEL.
    WHEN C_EXT_MAT_G.
    WHEN C_MATERIAL.
      WA_LEVEL = C_EXT_MAT_G.
      WA_RENEWAL_FLG = C_MARK.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " SET_LIST_LEVEL_CONTROL
*&------------------------------------------------------------------*
*&      Form  COLLECT_DATA_BY_v2
*&------------------------------------------------------------------*
FORM COLLECT_DATA_BY_V2.

  CLEAR : IT_ZSQM_QS_LIST_MD, IT_ZSQM_QS_LIST_NT, IT_ZSQM_QS_LIST_QS.

*MOVE 'MIN' TO : IT_ZSQM_QS_LIST_MD-MEINH,
*                IT_ZSQM_QS_LIST_NT-MEINH,

  APPEND LINES OF IT_ZSQM_QS_LIST_MD TO IT_ZSQM_QS_LIST_TT.
  MOVE : 'MIN' TO IT_ZSQM_QS_LIST_TT-MEINH.
  MODIFY IT_ZSQM_QS_LIST_TT TRANSPORTING MEINH
                            WHERE MATNR NE ''.

  LOOP AT IT_ZSQM_QS_LIST_NT.
    CLEAR IT_ZSQM_QS_LIST_TT.
    MOVE-CORRESPONDING IT_ZSQM_QS_LIST_NT TO IT_ZSQM_QS_LIST_TT.
    MOVE : 'MIN' TO IT_ZSQM_QS_LIST_TT-MEINH.

    COLLECT IT_ZSQM_QS_LIST_TT.
  ENDLOOP.

  LOOP AT IT_ZSQM_QS_LIST_QS.
    CLEAR IT_ZSQM_QS_LIST_TT.
    MOVE-CORRESPONDING IT_ZSQM_QS_LIST_QS TO IT_ZSQM_QS_LIST_TT.
    MOVE : 'MIN' TO IT_ZSQM_QS_LIST_TT-MEINH.

    COLLECT IT_ZSQM_QS_LIST_TT.
  ENDLOOP.

ENDFORM.                    " COLLECT_DATA_BY_v2
*&-----------------------------------------------------------------*
*&      Form  COLLECT_BY_EXTWG
*&-----------------------------------------------------------------*
FORM COLLECT_BY_EXTWG.

  LOOP AT IT_ZSQM_QS_LIST_TT.
    CLEAR IT_ZSQM_QS_EXTWG.
    MOVE-CORRESPONDING : IT_ZSQM_QS_LIST_TT TO IT_ZSQM_QS_EXTWG.

*- "for Basic List : Delete Vendor data field
    CLEAR : IT_ZSQM_QS_EXTWG-LIFNR, IT_ZSQM_QS_EXTWG-NAME_LF.

    CLEAR : IT_ZSQM_QS_EXTWG-MATNR,
            IT_ZSQM_QS_EXTWG-MAKTX.
    COLLECT IT_ZSQM_QS_EXTWG.
  ENDLOOP.

ENDFORM.                    " COLLECT_BY_EXTWG
*&-----------------------------------------------------------------*
*&      Form  CALC_QUAL_SCORE
*&-----------------------------------------------------------------*
FORM CALC_QUAL_SCORE TABLES   PT_EXTWG STRUCTURE ZSQM_QS_LIST_V2.
  DATA : LW_INDEX LIKE SY-TABIX.

  LOOP AT PT_EXTWG.
    LW_INDEX = SY-TABIX.
    IF NOT PT_EXTWG-MENGE_GI IS INITIAL.

      PT_EXTWG-Q_PPM =
            ( PT_EXTWG-RKMNG / PT_EXTWG-MENGE_GI )
           * 1000000.

    ENDIF.

    PT_EXTWG-Q_SCORE = PT_EXTWG-Q_PPM +
            ( PT_EXTWG-LINESTOP + PT_EXTWG-QNT_CAMP
            + PT_EXTWG-QNT_SALV + PT_EXTWG-QNT_REPR ).
*            * 50.  "/Remarked 02/16/2004 : requested by Mr Kim

    MODIFY PT_EXTWG INDEX LW_INDEX.
  ENDLOOP.

ENDFORM.                    " CALC_QUAL_SCORE
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SORT_TOTAL_FIELD.
*----- 01/21/2004 Append by BSBAE
  CLEAR: IT_SORT.
  MOVE: 1 TO IT_SORT-SPOS,
        'Q_SCORE'  TO IT_SORT-FIELDNAME,
        'X'        TO IT_SORT-DOWN.
  APPEND IT_SORT.
ENDFORM.                    " SET_SORT_TOTAL_FIELD
