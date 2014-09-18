*&---------------------------------------------------------------------*
*& Report  ZCAL_EQUIPMENT_DETAILS                                      *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
************************************************************************
* Program Name      : ZCAL_EQUIPMENT_DETAILS
* Author            : 100565
* Creation Date     : 08/14/06
* Specifications By : 100565
* Pattern           : Report 1.2 - Call Screen
* Development Request No :
* Addl Documentation:
* Description       : Calibration Equipment Details
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT  ZCAL_EQUIPMENT_DETAILS  NO STANDARD PAGE HEADING .

TABLES: MHIS,
        VIQMEL,
        EQUI,
        MPLA,
        mhio,
        Jest,
        ZSCAL_ORDER.

DATA: BEGIN OF IT_ORDER OCCURS 0,
                  MITYP LIKE MPOS-MITYP,
                  WAPOS LIKE MPOS-WAPOS,
                  WARPL LIKE MPLA-WARPL,
                  PSTXT LIKE MPOS-PSTXT,
                  EQUNR LIKE EQUI-EQUNR,
                  SERGE LIKE EQUI-SERGE,
                  eqtyp like equi-eqtyp,
                  eqktu like eqkt-eqktu,
                  gewrk like mpos-gewrk,
                  arbpl like crhd-arbpl,
                  OBJNR LIKE EQUI-OBJNR,
END OF IT_ORDER.

DATA: IT_ORDER_COMP LIKE IT_ORDER OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_ORDER_LOST OCCURS 0,
MITYP LIKE MPOS-MITYP,
                  WAPOS LIKE MPOS-WAPOS,
                  WARPL LIKE MPLA-WARPL,
                  PSTXT LIKE MPOS-PSTXT,
                  EQUNR LIKE EQUI-EQUNR,
                  SERGE LIKE EQUI-SERGE,
                  eqtyp like equi-eqtyp,
                  eqktu like eqkt-eqktu,
                  gewrk like mpos-gewrk,
                  arbpl like crhd-arbpl,
                  PRUEFLOS LIKE QAVE-PRUEFLOS,
                  AUFNR LIKE QALS-AUFNR,
END OF IT_ORDER_LOST.

*// Declare reference variables, the container and internal table
DATA: WA_CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : WA_REPID LIKE SY-REPID.
*
*--- maintenance item (MPOS)
data: BEGIN OF IT_wmpos occurs 0.
        INCLUDE STRUCTURE mpos.
        INCLUDE STRUCTURE mpos_addition.
data: END   OF IT_wmpos.

*--- location and account assignment (ILOA)
data: BEGIN OF IT_wiloa occurs 0.
        INCLUDE STRUCTURE iloa.
data: END   OF IT_wiloa.

*--- maintenance plan (MPLA)

DATA: BEGIN OF WC_WMPLA.
   INCLUDE STRUCTURE mpla.
        INCLUDE STRUCTURE mpla_addition.
DATA: END   OF WC_wmpla.



DATA : OK_CODE LIKE SY-UCOMM.
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS : LCL_EVENT_RECEIVER DEFINITION DEFERRED .

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Global variables for attributes or etc of ALV GRID
DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO. "/The Layout Structure
DATA : IT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_SORT     TYPE LVC_T_SORT WITH HEADER LINE,
 WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT


 SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

 SELECT-OPTIONS: MPTYP FOR MPLA-MPTYP NO INTERVALS,
                  WARPL FOR MPLA-WARPL,
*
                   serge for equi-serge,
                  eqtyp for equi-eqtyp,
                  EQUNR FOR EQUI-EQUNR.

 SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
Parameter: P RADIOBUTTON GROUP radi,
 Q RADIOBUTTON GROUP radi.
 SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETER: VKATART LIKE QAVE-VKATART DEFAULT '3',
           VCODEGRP LIKE QAVE-VCODEGRP DEFAULT 'Q3CAL',
           VCODE LIKE QAVE-VCODE DEFAULT '60'.
  SELECTION-SCREEN END OF BLOCK B3.


 AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

AT SELECTION-SCREEN ON BLOCK b1.
  CHECK SY-UCOMM = 'ONLI'.
AT SELECTION-SCREEN ON RADIOBUTTON GROUP radi.
*-- get scheduled orders from DB.
  PERFORM GET_DATA_FROM_DB.

*-- DISPLAY LIST
  PERFORM DISPLAY_LIST.

  LOAD-OF-PROGRAM.

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
ENDMETHOD.                           "handle_double_click

*-- / Handling Tollbar control
  METHOD HANDLE_TOOLBAR.

  ENDMETHOD.
*-------------------------------------------------------------------

*-- / Handling User defined commands for Toolbar
  METHOD HANDLE_USER_COMMAND.

*   In event handler method for event USER_COMMAND: Query your
*   function codes defined in Class Definition and react accordingly.
 ENDMETHOD.                           "handle_user_command

ENDCLASS.
*
* lcl_event_receiver (Implementation)

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_DB.


if p = 'X'.
select
 C~equnr C~SERGE C~EQTYP E~eqktu F~ARBPL C~OBJNR
into corresponding
fields OF TABLE IT_ORDER  FROM ( ( ( EQUI AS C  INNER JOIN ITOB AS D
ON
D~EQUNR = C~EQUNR ) INNER JOIN EQKT AS E ON
E~EQUNR = C~EQUNR ) INNER JOIN CRHD AS F ON
D~WKCTR = F~OBJID ) "INNER JOIN JEST AS J ON
*C~OBJNR = J~OBJNR )



WHERE

* B~WARPL IN WARPL
 C~EQTYP IN EQTYP
AND C~EQUNR IN EQUNR
AND C~SERGE IN SERGE.
*AND ( J~STAT <> 'I0076'
*AND J~INACT = ' ' ).


LOOP AT IT_ORDER.
DELETE ADJACENT DUPLICATES FROM IT_ORDER COMPARING ALL FIELDS.
ENDLOOP.

IF NOT IT_ORDER[] IS INITIAL.
LOOP AT IT_ORDER.
SELECT SINGLE * FROM JEST WHERE OBJNR = IT_ORDER-OBJNR
                            AND STAT = 'I0076'
                            AND INACT = ' '.
IF SY-SUBRC = 0.
DELETE IT_ORDER.

ENDIF.
ENDLOOP.
ENDIF.

IF NOT IT_ORDER[] IS INITIAL.
SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ORDER_COMP FROM MPOS
FOR ALL ENTRIES IN IT_ORDER WHERE EQUNR = IT_ORDER-EQUNR.
ENDIF.

LOOP AT IT_ORDER.

READ TABLE IT_ORDER_COMP WITH KEY EQUNR = IT_ORDER-EQUNR.
IF SY-SUBRC = 0.
MOVE IT_ORDER_COMP-WARPL TO IT_ORDER-WARPL.
MOVE IT_ORDER_COMP-MITYP TO IT_ORDER-MITYP.
MOVE IT_ORDER_COMP-WAPOS TO IT_ORDER-WAPOS.
MOVE IT_ORDER_COMP-PSTXT TO IT_ORDER-PSTXT.
ENDIF.
MODIFY IT_ORDER.
CLEAR: IT_ORDER, IT_ORDER_COMP.
ENDLOOP.

elseif q = 'X'.
select
 D~equnr D~SERGE D~EQTYP E~eqktu B~PRUEFLOS C~AUFNR
into corresponding
fields OF TABLE IT_ORDER_LOST  FROM ( ( ( ( QAVE AS A  INNER JOIN QALS
AS B
ON A~PRUEFLOS = B~PRUEFLOS ) INNER JOIN AFIH AS C ON
B~AUFNR = C~AUFNR ) INNER JOIN EQUI AS D ON
D~EQUNR = C~EQUNR ) INNER JOIN EQKT AS E ON
D~EQUNR = E~EQUNR )
WHERE
* D~EQTYP IN EQTYP
*AND D~EQUNR IN EQUNR
*AND D~SERGE IN SERGE
 A~VKATART = VKATART
AND A~VCODEGRP = VCODEGRP
AND A~VCODE  = VCODE.

endif.

*LOOP AT IT_ORDER.
*
*CALL FUNCTION 'IWP1_READ_MAINTENANCE_PLAN_DB'
*  EXPORTING
*    I_WARPL                  = IT_ORDER-WARPL
**   I_NOBUFFER               = ' '
**   I_DELETE                 = ' '
*    I_AKTYP                  = 'X'
**   I_INACTIV                = ' '
**   I_READ_TERM_DATA         = ' '
**   I_READ_STRATEGY          = ' '
* IMPORTING
*   E_WMPLA                  = WC_wmpla
**   E_STRAT_NOT_EXIST        =
**   E_T399W                  =
**   E_T351                   =
* TABLES
*   TAB_WMPOS                = IT_wmpos
*   TAB_WILOA                = IT_wiloa
**   TAB_WMMPT                =
**   TAB_WMHIS                =
**   TAB_WMHIO                =
**   TAB_T351P                =
** EXCEPTIONS
**   INVALID_NUMBER           = 1
**   LVORM_SET_FOR_PLAN       = 2
**   PLAN_NOT_ACTIV           = 3
**   LOKZ_SET_FOR_PLAN        = 4
**   NUMBER_INITIAL           = 5
**   OTHERS                   = 6
*          .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*move it_wmpos-equnr to it_order-equnr.
*modify it_order.
*
*ENDLOOP.
*
ENDFORM.                    " GET_DATA_FROM_DB
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LIST.
CALL SCREEN '0900'.

ENDFORM.                    " DISPLAY_LIST
*&---------------------------------------------------------------------*
*&      Module  Create_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Create_ALV_OBJECT OUTPUT.
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
*-- Display data on ALV GRID Control using method
*-- 'SET_TABLE_FOR_FIRST_DISPLAY'
*    WA_VARIANT-VARIANT = '/ZRQM21_CLAS'.
    PERFORM SET_TABLE_TO_ALV.


*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
*    CREATE OBJECT EVENT_RECEIVER.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
**-   toolbar control event
*    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
*
**- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD ALV_GRID->SET_TOOLBAR_INTERACTIVE.
*
*    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.


endif.
ENDMODULE.                 " Create_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
*  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
*//-- Set Layout Structure
  WA_IS_LAYOUT-LANGUAGE = SY-LANGU.      "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.      "/Optimize column width
*  WA_IS_LAYOUT-DETAILTITL = ''.        "/Title bar of detail screen
*  WA_IS_LAYOUT-GRID_TITLE = ''.        "/ Title bar text
*  WA_IS_LAYOUT-KEYHOT      = C_MARK.    "/ Key columns as hotspot
*  WA_IS_LAYOUT-NO_HEADERS  = C_MARK.     "/Hide column headings
*  WA_IS_LAYOUT-NO_HGRIDLN  = C_MARK.     "/Hide horizontal grid lines
*  WA_IS_LAYOUT-NO_VGRIDLN  = C_MARK.     "/Hide vertical grid lines
*  WA_IS_LAYOUT-NO_MERGING  = C_MARK.     "/Disable cell merging
*  WA_IS_LAYOUT-NO_ROWMARK  = C_MARK.     "/Disable row selections
*  WA_IS_LAYOUT-NO_TOOLBAR  = C_MARK.     "/Hide toolbar
  WA_IS_LAYOUT-NUMC_TOTAL  = 'X'. "/Allow totals for NUMC
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
*&---------------------------------------------------------------------*
*&      Form  MASK_COLUMNS_OF_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM MASK_COLUMNS_OF_ALV_GRID TABLES PT_FIELDCAT TYPE LVC_T_FCAT.


  REFRESH PT_FIELDCAT. CLEAR PT_FIELDCAT.
if P = 'X'.
* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZSCAL_EQUIPMENT'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].
elseif q = 'X'.

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZSCAL_EQUI_LOST'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

endif.

ENDFORM.                    " MASK_COLUMNS_OF_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_TABLE_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_TABLE_TO_ALV.

IF P = 'X'.
CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSCAL_EQUIPMENT'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = 'A'
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*                     IT_SORT          = IT_SORT[]
                     IT_OUTTAB        = IT_order[].
ELSEIF Q = 'X'.
CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSCAL_EQUI_LOST'
                     IS_LAYOUT        = WA_IS_LAYOUT
                     I_SAVE           = 'A'
                     IS_VARIANT       = WA_VARIANT
                     I_DEFAULT        = SPACE
           CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*                     IT_SORT          = IT_SORT[]
                     IT_OUTTAB        = IT_order_LOST[].

ENDIF.
ENDFORM.                    " SET_TABLE_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0900 OUTPUT.
  SET PF-STATUS 'ORDER'.
  SET TITLEBAR 'ORD'.

ENDMODULE.                 " STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0900 INPUT.
OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK'.
*PERFORM FREE_ALV_GRID.
        LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
    LEAVE TO SCREEN 0.
    WHEN 'RW'.
    LEAVE TO SCREEN 0.
WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0900  INPUT
