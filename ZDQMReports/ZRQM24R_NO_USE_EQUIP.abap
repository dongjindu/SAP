************************************************************************
* Program Name      : ZRQM24R_NO_USE_EQUIP
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.31.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : No-Use Equipment Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZRQM24R_NO_USE_EQUIP     NO STANDARD PAGE HEADING .

*&&& Data Declaration.  &&&*
*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.
INCLUDE ZQM_INCLUDE_POOL03. "/Equipment Include Module

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : T001W.
TABLES : CABN,   "/Characteristic
         CAWN,   "/Characteristic values
         IHPA,   "/Plant Maintenance: Partners
         KLAH.   "/Class Header Data
*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_EQ_NOUSE, "/No-Use Equip. List for Select criterion
         ZSQM_EQ_NOUSE_CNT. "/No-Use Equip Quantity(Count)
*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)
*-- TABLE CONTROL
*CONTROLS: TC_9000  TYPE TABLEVIEW USING SCREEN 9000.

*//Type (Table Structure);(TY_ )- Table or Structure

*-- PF-Status : Excluding Function Code table
*TYPES: BEGIN OF TY_FCODE,
*        FCODE LIKE RSMPE-FUNC,
*      END OF TY_FCODE.
*DATA: IT_EX_FUNC TYPE STANDARD TABLE OF TY_FCODE WITH
*                       NON-UNIQUE DEFAULT KEY INITIAL SIZE 5,
*      WA_EX_FUNC TYPE TY_FCODE.


*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.

**-- Screen Control Mode
CONSTANTS : C_FIRST(10) TYPE C VALUE 'FIRST',
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


*/-- Internale Tables with structure as sama as DB
DATA : IT_ZSQM_EQ_NOUSE     LIKE ZSQM_EQ_NOUSE OCCURS 0
                                             WITH HEADER LINE,
       IT_ZSQM_EQ_NOUSE_CNT LIKE ZSQM_EQ_NOUSE_CNT OCCURS 50
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
*- Department(Partner Function 'AB'.
SELECT-OPTIONS : S_PARNR  FOR IHPA-PARNR .
*- Class
SELECT-OPTIONS : S_CLASS  FOR KLAH-CLASS MATCHCODE OBJECT CLAS.
*- Characteristic
SELECT-OPTIONS : S_ATINN  FOR CABN-ATINN.
*- Value
SELECT-OPTIONS : S_ATWRT  FOR CAWN-ATWRT.

SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

*--// Search Help for Department field using HR organization
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PARNR-LOW.
*-- Help for Department - Low : Partner function 'Z3'.
  PERFORM GET_SCREEN_FIELD_INFO.
  PERFORM GET_PARTNER_VALUE   USING 'Z3'  "/Partner Function:Dept.
                                    WA_FLDTXT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PARNR-HIGH.
*-- Help for Department - High : Partner function 'Z3'.
  PERFORM GET_SCREEN_FIELD_INFO.
  PERFORM GET_PARTNER_VALUE   USING 'Z3'  "/Partner Function:Dept.
                                    WA_FLDTXT.

AT SELECTION-SCREEN ON BLOCK BLK.
  CHECK SY-UCOMM = 'ONLI'.

*-- get equipment data from DB.
  PERFORM GET_DATA_FROM_DB.

  IF IT_ZSQM_EQ_NOUSE_CNT[] IS INITIAL.
    MESSAGE E000(ZMQM) WITH 'No entries!'(E01).
    EXIT.
  ENDIF.


START-OF-SELECTION.
*  CHECK NOT IT_ZSQM_EQ_NOUSE_CNT[] IS INITIAL.


**-- End of Selection.
END-OF-SELECTION.
  CHECK NOT IT_ZSQM_EQ_NOUSE_CNT[] IS INITIAL.
  WA_LEVEL = C_FIRST.
  CALL SCREEN 9000.

*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.

*-- Set class type field for Search help : '002'.
  SET PARAMETER ID 'KAR' FIELD '002'.

INITIALIZATION.


****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS:

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN ES_ROW_NO,

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
*- read selected row from internal table using INDEX E_ROW-INDEX
*Parameters.
*  E_ROW	Type	LVC_S_ROW		Row ID
*  E_COLUMN	Type	LVC_S_COL		Column Name
*  ES_ROW_NO	Type	LVC_S_ROID		Numeric Row ID
    CASE WA_LEVEL.
      WHEN C_FIRST.

      WHEN C_DETAIL.
        CHECK NOT E_ROW-INDEX IS INITIAL.

        PERFORM PROC_DOUBLE_CLICK  USING E_COLUMN-FIELDNAME
                                         ES_ROW_NO-ROW_ID.

    ENDCASE.


  ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

*This event is triggered by the ALV each time the toolbar of the control
* needs to be regenerated. To add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

    CASE WA_LEVEL.
      WHEN C_FIRST.
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

          CHECK NOT IT_ZSQM_EQ_NOUSE[] IS INITIAL.
          WA_LEVEL = C_DETAIL.

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


  DATA : LW_FROMNUMBER LIKE NRIV-FROMNUMBER,
         LW_TONUMBER   LIKE NRIV-TONUMBER.

*-   If EQUNR wsa not inputed, Get Equipment Number ranges data
*-   and It will be used in SQL. (LW_FROMNUMBER, LW_TONUMBER)
    SELECT SINGLE B~FROMNUMBER B~TONUMBER
        INTO (LW_FROMNUMBER, LW_TONUMBER)
          FROM T370T AS A INNER JOIN NRIV AS B
            ON  A~NUMKI = B~NRRANGENR
            WHERE  A~EQTYP  = C_EQTYP_CALIBRATION
               AND B~OBJECT = 'EQUIP_NR'.


  SELECT COUNT( DISTINCT A~EQUNR ) AS EQUIP_CNT "/quantity of equipment
         B~ATINN B~ATWRT
         D~CLASS
         F~PARNR AS PARNR_DEP
         J~ORGTX AS ORGTX
      INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_NOUSE_CNT
    FROM ( ( ( ( ( ( ( ( EQUI AS A INNER JOIN AUSP AS B
       ON A~EQUNR = B~OBJEK ) INNER JOIN KSML AS C
       ON B~ATINN = C~IMERK ) INNER JOIN KLAH AS D
       ON C~CLINT = D~CLINT ) INNER JOIN JEST AS E
       ON A~OBJNR = E~OBJNR ) INNER JOIN IHPA AS F  "/Department
       ON A~OBJNR = F~OBJNR ) INNER JOIN IHPA AS G  "/Person
       ON A~OBJNR = G~OBJNR ) INNER JOIN EQUZ AS H
       ON A~EQUNR = H~EQUNR ) INNER JOIN ILOA AS I
       ON H~ILOAN = I~ILOAN ) INNER JOIN T527X AS J
       ON F~PARNR = J~ORGEH
      WHERE A~EQTYP = C_EQTYP_CALIBRATION  "/Calibration Equipment.
        AND A~EQUNR  BETWEEN LW_FROMNUMBER AND LW_TONUMBER
        AND B~LKENZ = ' '
        AND C~LKENZ = ' '
        AND D~KLART = C_EQUIP_CLASS_TYPE "/Equipment Class type
        AND E~STAT  = C_EQUIP_STATUS_IDLE
        AND E~INACT = ' '
        AND F~KZLOESCH = ' '
        AND F~PARVW = C_EQUIP_PARVW_DEPT
        AND G~KZLOESCH = ' '
        AND G~PARVW = C_EQUIP_PARVW_PERSON
        AND H~LVORM = ' '
        AND J~SPRSL = SY-LANGU
        AND J~ENDDA = '99991231'
        AND I~SWERK IN S_WERKS
        AND F~PARNR IN S_PARNR
        AND D~CLASS IN S_CLASS
        AND B~ATINN IN S_ATINN
        AND B~ATWRT IN S_ATWRT
      GROUP BY   B~ATINN B~ATWRT D~CLASS F~PARNR J~ORGTX.

  CHECK NOT IT_ZSQM_EQ_NOUSE_CNT[] IS INITIAL.

*-- fill Unit of quantity(count) of Equipments
  MOVE 'EA' TO  IT_ZSQM_EQ_NOUSE_CNT-MEINS.
  MODIFY IT_ZSQM_EQ_NOUSE_CNT TRANSPORTING MEINS
                          WHERE PARNR_DEP NE ''.

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
*  CHECK WA_LEVEL = C_FIRST.
*  LOOP AT SCREEN.
*    IF SCREEN-GROUP1 = 'DET'.
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
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
*                                        IT_FIELDCAT.

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
*&----------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&-----------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES   PT_FIELDCAT TYPE LVC_T_FCAT.

  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.

* Build the fieldcat according to DDIC structure :
  CASE WA_LEVEL.
    WHEN C_FIRST.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
           EXPORTING
                I_STRUCTURE_NAME = 'ZSQM_EQ_NOUSE_CNT'
           CHANGING
                CT_FIELDCAT      = PT_FIELDCAT[].
    WHEN C_DETAIL.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
           EXPORTING
                I_STRUCTURE_NAME = 'ZSQM_EQ_NOUSE'
           CHANGING
                CT_FIELDCAT      = PT_FIELDCAT[].
  ENDCASE.

* Set field attribute

  CASE WA_LEVEL.
    WHEN C_FIRST.
      LOOP AT PT_FIELDCAT.
        IF PT_FIELDCAT-FIELDNAME = 'PARNR_DEP'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-KEY     = C_MARK.
          PT_FIELDCAT-NO_OUT  = C_MARK.
        ELSEIF PT_FIELDCAT-FIELDNAME = 'ORGTX'.
          PT_FIELDCAT-KEY_SEL = C_MARK.
          PT_FIELDCAT-KEY     = C_MARK.
        ENDIF.
        MODIFY PT_FIELDCAT.
      ENDLOOP.
    WHEN C_DETAIL.

  ENDCASE.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
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

*&-----------------------------------------------------------------*
*&      Form  SET_TABLE_TO_ALV
*&-----------------------------------------------------------------*
FORM SET_TABLE_TO_ALV.
  CASE WA_LEVEL.
    WHEN C_FIRST.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_NOUSE_CNT'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = WA_SAVE
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                     IT_OUTTAB        = IT_ZSQM_EQ_NOUSE_CNT[].
    WHEN C_DETAIL.
      CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSQM_EQ_NOUSE'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = WA_SAVE
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
                     IT_OUTTAB        = IT_ZSQM_EQ_NOUSE[].
  ENDCASE.

ENDFORM.                    " SET_TABLE_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
*//-- Set Layout Structure


  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = C_MARK.
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
*&---------------------------------------------------------------------*
*&      Module  EXIT_9000  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.
      IF WA_LEVEL = C_FIRST.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN 'RW'.
      IF WA_LEVEL = C_FIRST.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_9000  INPUT
*&------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
      IF WA_LEVEL = C_FIRST.
        PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM SET_LIST_LEVEL_CONTROL.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  RETRIEV_DETAIL_DATA
*&---------------------------------------------------------------------*
FORM RETRIEV_DETAIL_DATA USING    P_INDEX.
  DATA : LW_SEL_INDEX   LIKE SY-TABIX.
  DATA : LW_TABLE_LINES LIKE SY-TABIX.

  LW_SEL_INDEX = P_INDEX.

  REFRESH  IT_ZSQM_EQ_NOUSE.

  CASE WA_LEVEL.
    WHEN C_FIRST.
      READ TABLE IT_ZSQM_EQ_NOUSE_CNT INDEX LW_SEL_INDEX.
      CHECK SY-SUBRC = 0.

      SELECT A~EQUNR I~SWERK AS WERKS
             B~ATINN B~ATWRT
             D~CLASS
             F~PARNR AS PARNR_DEP
             G~PARNR AS PARNR_PER
          INTO CORRESPONDING FIELDS OF TABLE IT_ZSQM_EQ_NOUSE
        FROM ( ( ( ( ( ( ( EQUI AS A INNER JOIN AUSP AS B
           ON A~EQUNR = B~OBJEK ) INNER JOIN KSML AS C
           ON B~ATINN = C~IMERK ) INNER JOIN KLAH AS D
           ON C~CLINT = D~CLINT ) INNER JOIN JEST AS E
           ON A~OBJNR = E~OBJNR ) INNER JOIN IHPA AS F  "/Department
           ON A~OBJNR = F~OBJNR ) INNER JOIN IHPA AS G  "/Person
           ON A~OBJNR = G~OBJNR ) INNER JOIN EQUZ AS H
           ON A~EQUNR = H~EQUNR ) INNER JOIN ILOA AS I
           ON H~ILOAN = I~ILOAN
          WHERE A~EQTYP = C_EQTYP_CALIBRATION  "/Calibration Equipment.
            AND B~LKENZ = ' '
            AND C~LKENZ = ' '
            AND D~KLART = C_EQUIP_CLASS_TYPE "/Equipment Class type
            AND E~STAT  = C_EQUIP_STATUS_IDLE
            AND E~INACT = ' '
            AND F~KZLOESCH = ' '
            AND F~PARVW = C_EQUIP_PARVW_DEPT
            AND G~KZLOESCH = ' '
            AND G~PARVW = C_EQUIP_PARVW_PERSON
            AND H~LVORM = ' '
            AND I~SWERK IN S_WERKS
            AND F~PARNR = IT_ZSQM_EQ_NOUSE_CNT-PARNR_DEP
            AND D~CLASS = IT_ZSQM_EQ_NOUSE_CNT-CLASS
            AND B~ATINN = IT_ZSQM_EQ_NOUSE_CNT-ATINN
            AND B~ATWRT = IT_ZSQM_EQ_NOUSE_CNT-ATWRT.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " RETRIEV_DETAIL_DATA
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
*&-------------------------------------------------------------------*
*&      Form  SET_LIST_LEVEL_CONTROL
*&-------------------------------------------------------------------*
FORM SET_LIST_LEVEL_CONTROL.
  CASE WA_LEVEL.
    WHEN C_FIRST.
    WHEN C_DETAIL.
      WA_LEVEL = C_FIRST.
      WA_RENEWAL_FLG = C_MARK.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " SET_LIST_LEVEL_CONTROL
*&-----------------------------------------------------------------*
*&      Form  PROC_DOUBLE_CLICK
*&-----------------------------------------------------------------*
FORM PROC_DOUBLE_CLICK USING  P_COLUMN_NAME             "Column Name
                              PS_ROW_NO  LIKE SY-TABIX. "Numeric Row ID
  DATA : LW_SEL_INDEX LIKE SY-TABIX.

  CHECK P_COLUMN_NAME = 'EQUNR'.

  LW_SEL_INDEX = PS_ROW_NO.

*-- Get selected equipment No.
  READ TABLE IT_ZSQM_EQ_NOUSE INDEX LW_SEL_INDEX.

  CHECK SY-SUBRC = 0.

  SET PARAMETER ID 'EQN' FIELD IT_ZSQM_EQ_NOUSE-EQUNR.

  CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .

ENDFORM.                    " PROC_DOUBLE_CLICK
