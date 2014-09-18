*&---------------------------------------------------------------------*
*& Report  ZCAL_SCHEDULE_ORDERS_ver2                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
************************************************************************
* Program Name      : ZCAL_SCHEDULE_ORDERS
* Author            : 100565
* Creation Date     : 03/17/06
* Specifications By : 100565
* Pattern           : Report 1.2 - Call Screen
* Development Request No :
* Addl Documentation:
* Description       : Schedule for Calibration Orders
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT  ZCAL_SCHEDULE_ORDERS_VER2  NO STANDARD PAGE HEADING .

TABLES: MHIS,
        VIQMEL,
        EQUI,
        MPLA,
        JEST,
        MHIO,
        AUFK,
        ZSCAL_ORDER,
        MCIPMIS,
        ADRC.

DATA: BEGIN OF IT_ORDER OCCURS 0,
                  MPTYP LIKE MPLA-MPTYP,
                  WARPL LIKE MPLA-WARPL,
                  HORDA LIKE MHIS-HORDA,
*                  STADT LIKE MHIS-STADT,
                  NPLDA  LIKE MHIS-NPLDA,
                  EQUNR LIKE EQUI-EQUNR,
                  SERGE LIKE EQUI-SERGE,
                  GSTRP LIKE MHIO-GSTRP,
                  OBJNR LIKE EQUI-OBJNR,
*                  laufn like mhio-laufn,
                  EQKTU LIKE EQKT-EQKTU,
                  GEWRK LIKE MPOS-GEWRK,
                  ARBPL LIKE CRHD-ARBPL,
                  ADDAT LIKE MHIO-ADDAT,
                  AUFNR LIKE MHIO-AUFNR,
                  EQFNR LIKE MCIPMIS-EQFNR,
                  NAME LIKE ADRC-NAME1,
END OF IT_ORDER.

*// Declare reference variables, the container and internal table
DATA: WA_CUSTOM_CONTROL    TYPE   SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : WA_REPID LIKE SY-REPID.
*
*--- maintenance item (MPOS)
DATA: BEGIN OF IT_WMPOS OCCURS 0.
        INCLUDE STRUCTURE MPOS.
        INCLUDE STRUCTURE MPOS_ADDITION.
DATA: END   OF IT_WMPOS.

*--- location and account assignment (ILOA)
DATA: BEGIN OF IT_WILOA OCCURS 0.
        INCLUDE STRUCTURE ILOA.
DATA: END   OF IT_WILOA.

*--- maintenance plan (MPLA)

DATA: BEGIN OF WC_WMPLA.
        INCLUDE STRUCTURE MPLA.
        INCLUDE STRUCTURE MPLA_ADDITION.
DATA: END   OF WC_WMPLA.



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
*                  HORDA FOR MHIS-HORDA,
*                  STADT FOR MHIS-STADT,
                  NPLDA FOR MHIS-NPLDA,
*                  S_GSTRP FOR MHIO-GSTRP,
                 EQUNR FOR EQUI-EQUNR,
                 S_EQFNR FOR MCIPMIS-EQFNR.
**                 S_ADDR FOR ADRC-ADDRNUMBER.

SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ADDR-LOW.
*  PERFORM LIST_BOX_ADDNAME.

AT SELECTION-SCREEN ON BLOCK B1.
  CHECK SY-UCOMM = 'ONLI'.

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
  DATA: L_EQFNR LIKE MCIPMIS-EQFNR,
        L_ADRNR LIKE ADRC-ADDRNUMBER,
        L_OBJNR LIKE EQUI-OBJNR.

  SELECT A~HORDA
*a~stadt
  A~NPLDA
  B~MPTYP B~WARPL C~EQUNR D~SERGE E~GSTRP
*E~laufn
  F~EQKTU
  C~GEWRK G~ARBPL
  D~OBJNR
  E~ADDAT
  E~AUFNR
  INTO CORRESPONDING
FIELDS OF TABLE IT_ORDER  FROM ( ( ( ( ( ( MHIS AS A INNER JOIN MPLA AS
  B ON
  A~WARPL = B~WARPL )
*AND A~ABNUM = B~ABNUM )
  INNER JOIN MPOS AS C
  ON C~WARPL = B~WARPL )
  INNER JOIN EQUI AS D
  ON D~EQUNR = C~EQUNR )
  INNER JOIN EQKT AS F
  ON F~EQUNR = C~EQUNR )

  INNER JOIN MHIO AS E
  ON E~WARPL = A~WARPL AND
     E~ABNUM = A~ABNUM )
  INNER JOIN CRHD AS G
  ON G~OBJID = C~GEWRK )

  WHERE B~MPTYP IN MPTYP
  AND B~WARPL IN WARPL
*AND A~HORDA IN HORDA
*AND A~STADT IN STADT
  AND A~NPLDA IN NPLDA
  AND D~EQUNR IN EQUNR
** Added by Furong on 09/21/09
  AND A~TSTAT <> 'X'.
** End of addition
*AND E~GSTRP IN S_GSTRP.

  IF NOT IT_ORDER[] IS INITIAL.
    LOOP AT IT_ORDER.
      SELECT SINGLE * FROM JEST WHERE OBJNR = IT_ORDER-OBJNR
                                  AND STAT = 'I0076'
                                  AND INACT = ' '.
      IF SY-SUBRC = 0.
        DELETE IT_ORDER.
      ENDIF.

      SELECT SINGLE * FROM AUFK WHERE AUFNR = IT_ORDER-AUFNR AND
      LOEKZ = 'X'.
      IF SY-SUBRC = 0.
        DELETE IT_ORDER.
      ENDIF.
    ENDLOOP.

** Changed by Furon on 06/25/07  "UD1K940874
** Help desk : 765F1A4113
    LOOP AT IT_ORDER.
      CLEAR: L_OBJNR, L_EQFNR.
** Changed by Furong on 07/08/09
      SELECT SINGLE OBJNR INTO L_OBJNR
        FROM EQUI
        WHERE EQUNR = IT_ORDER-EQUNR.

      SELECT SINGLE EQFNR INTO L_EQFNR
        FROM MCIPMIS
        WHERE EQUNR = IT_ORDER-EQUNR
          AND EQFNR IN S_EQFNR
          AND OBJNR = L_OBJNR.
** End of change

      IF SY-SUBRC = 0.
        IT_ORDER-EQFNR = L_EQFNR.
      ELSE.
        DELETE IT_ORDER.
        CLEAR: L_EQFNR.
        CONTINUE.
      ENDIF.


      SELECT SINGLE ADRNR INTO L_ADRNR
      FROM EQUZ AS A INNER JOIN ILOA AS B
      ON A~ILOAN = B~ILOAN
      WHERE A~EQUNR = IT_ORDER-EQUNR
        AND A~DATBI >= SY-DATUM.
      IF NOT L_ADRNR IS INITIAL.
        SELECT SINGLE NAME1 INTO IT_ORDER-NAME
        FROM ADRC
        WHERE ADDRNUMBER = L_ADRNR.
      ENDIF.
      MODIFY IT_ORDER.
      CLEAR: L_EQFNR, L_ADRNR, IT_ORDER.

*      SELECT SINGLE ADRNR INTO L_ADRNR
*      FROM EQUZ AS A INNER JOIN ILOA AS B
*      ON A~ILOAN = B~ILOAN
*      WHERE A~EQUNR = IT_ORDER-EQUNR
*        AND A~DATBI >= SY-DATUM
*        AND ADRNR IN S_ADDR.
*      IF NOT L_ADRNR IS INITIAL.
*        SELECT SINGLE NAME1 INTO IT_ORDER-NAME
*        FROM ADRC
*        WHERE ADDRNUMBER = L_ADRNR.
*        MODIFY IT_ORDER.
*        CLEAR: L_EQFNR, L_ADRNR, IT_ORDER.
*      ELSE.
*        DELETE IT_ORDER.
*        CLEAR: L_EQFNR, L_ADRNR.
*        CONTINUE.
*      ENDIF.
    ENDLOOP.
** end of change
  ENDIF.


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

  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
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

*  WA_IS_LAYOUT-NUMC_TOTAL  = 'X'. "/Allow totals for NUMC

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

* Build the fieldcat according to DDIC structure :
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZSCAL_ORDER'
       CHANGING
            CT_FIELDCAT      = PT_FIELDCAT[].

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
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
             EXPORTING I_STRUCTURE_NAME = 'ZSCAL_ORDER'
                       IS_LAYOUT        = WA_IS_LAYOUT
                       I_SAVE           = 'A'
                       IS_VARIANT       = WA_VARIANT
*                     I_DEFAULT        = SPACE
             CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
*                     IT_SORT          = IT_SORT[]
                       IT_OUTTAB        = IT_ORDER[].

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
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_addname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_BOX_ADDNAME.
  DATA: BEGIN OF VALUE_TAB OCCURS 0,
    ADDRNUMBER LIKE ADRC-ADDRNUMBER,
    NAME1 LIKE ADRC-NAME1,
  END OF VALUE_TAB.
  DATA: L_DYNAME LIKE SY-REPID.

  SELECT ADDRNUMBER NAME1 INTO TABLE VALUE_TAB
    FROM ADRC
    WHERE NAME1 <> ' '.
  L_DYNAME = SY-REPID.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      RETFIELD               = 'ADDRNUMBER'
*     PVALKEY                = ' '
      DYNPPROG               = L_DYNAME
      DYNPNR                 = '1000'
      DYNPROFIELD            = 'S_ADDR'
*     STEPL                  = 0
      WINDOW_TITLE           = 'Name'
*     VALUE                  = ' '
     VALUE_ORG              = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY                = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
    TABLES
      VALUE_TAB              = VALUE_TAB
*     FIELD_TAB              =
*     RETURN_TAB             =
*     DYNPFLD_MAPPING        =
*   EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS                 = 3
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " LIST_BOX_addname
