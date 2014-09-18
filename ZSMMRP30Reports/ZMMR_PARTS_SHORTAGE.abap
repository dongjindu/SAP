************************************************************************
* Program Name      : ZMMR_PARTS_SHORTAGE
* Author            : Furong Wang
* Creation Date     : 04/2010
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  ZMMR_PARTS_SHORTAGE   MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TABLES: MARA, MARC, PVBE, LFA1, ZTPP_INPUTPLAN_H, ZTMM_HOUR_SHORT.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF IT_DATA OCCURS 0,
     MATNR LIKE RESB-MATNR,
     SORTF LIKE RESB-SORTF,
     RP01(13),
     RP02(13),
     RP03(13),
     RP04(13),
     RP05(13),
     RP06(13),
     RP07(13),
     RP08(13),
     RP09(13),
     RP10(13),
     RP11(13),
     RP12(13),
     RP13(13),
     RP14(13),
     RP15(13),
     RP16(13),
     RP17(13),
     RP18(13),
     RP19(13),
     RP20(13),
     RP21(13),
     RP22(13),
     RP23(13),
     RP24(13),
     RP25(13),
     RP26(13),
     RP27(13),
     RP28(13),
     RP29(13),
     RP30(13),
     RP31(13),
     RP32(13),
     RP33(13),
     RP34(13),
     RP35(13),
     RP36(13),
     RP37(13),
     RP38(13),
     RP39(13),
     RP40(13),
     END OF  IT_DATA.

DATA: BEGIN OF IT_SHORT OCCURS 0.
        INCLUDE STRUCTURE ZTMM_HOUR_SHORT.
DATA: END OF IT_SHORT.

*DATA: BEGIN OF IT_SHORT OCCURS 0.
*        INCLUDE STRUCTURE ZMMS_SHORT.
*DATA: LIGHTS,
*      COLOR TYPE SLIS_T_SPECIALCOL_ALV,
*      END OF IT_SHORT.
*

DATA: IT_SHORT_BW LIKE TABLE OF ZMMS_SHORT WITH HEADER LINE.

DATA: BEGIN OF IT_INPUT_PLAN OCCURS 0.
        INCLUDE STRUCTURE ZTPP_INPUTPLAN_H.
DATA: END OF IT_INPUT_PLAN .

DATA: BEGIN OF IT_21DAY OCCURS 0,
        MATNR LIKE PLAF-MATNR,
        LIFNR LIKE ZTMM_PARTS_21DAY-LIFNR,
        PRVBE LIKE ZTMM_PARTS_21DAY-PRVBE,
        SORTF LIKE ZTMM_PARTS_21DAY-SORTF,
        MEINS LIKE ZTMM_PARTS_21DAY-MEINS,
        END OF IT_21DAY.

DATA: BEGIN OF IT_HOLD OCCURS 0.
        INCLUDE STRUCTURE ZTPP_INPUTPLAN_H.
DATA: END OF IT_HOLD.

DATA: BEGIN OF IT_VIN OCCURS 0,
     VIN(18),
     RP LIKE RESB-SORTF,
     RP01(2) TYPE N,
     RP02(2) TYPE N,
     RP03(2) TYPE N,
     RP04(2) TYPE N,
     RP05(2) TYPE N,
     RP06(2) TYPE N,
     RP07(2) TYPE N,
     RP08(2) TYPE N,
     RP09(2) TYPE N,
     RP10(2) TYPE N,
     RP11(2) TYPE N,
     RP12(2) TYPE N,
     RP13(2) TYPE N,
     RP14(2) TYPE N,
     RP15(2) TYPE N,
     RP16(2) TYPE N,
     RP17(2) TYPE N,
     RP18(2) TYPE N,
     END OF IT_VIN.

DATA: BEGIN OF IT_VIN_TACK OCCURS 0,
     STATUS LIKE ZTPP_INPUTPLAN_H-STATUS,
     NO TYPE I,
     END OF  IT_VIN_TACK.

DATA: BEGIN OF IT_RESB OCCURS 0,
     MATNR LIKE RESB-MATNR,
*     SORTF LIKE RESB-SORTF,
     PLNUM LIKE RESB-PLNUM,
*     PRVBE LIKE RESB-PRVBE,
     BDMNG  LIKE RESB-BDMNG,
     MEINS LIKE RESB-MEINS,
     VIN(18),
     HRS(2) TYPE N,
*     DATE LIKE SY-DATUM,
*     TIME LIKE SY-UZEIT,
     END OF IT_RESB.

DATA: BEGIN OF IT_HOUR OCCURS 0,
     SEQ(2),
     DATE LIKE SY-DATUM,
     TIME LIKE SY-UZEIT,
     HRS(2) TYPE N,
     END OF  IT_HOUR.

DATA: BEGIN OF IT_MARA OCCURS 0,
       MATNR LIKE RESB-MATNR,
       END OF IT_MARA.

*DATA: BEGIN OF IT_LIFNR OCCURS 0,
*      MATNR LIKE EORD-MATNR,
*      LIFNR LIKE EORD-LIFNR,
*      END OF IT_LIFNR.

** ALV
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
      IT_EXCLUDE TYPE UI_FUNCTIONS.

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL_800 TYPE SCRFNAME VALUE 'ALV_CONTAINER_800',
      ALV_GRID_800          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER_800    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: G_LIGHTS_FIELDNAME  TYPE SLIS_FIELDNAME VALUE 'LIGHTS'.

DATA: OK_CODE LIKE SY-UCOMM,
      W_CODE LIKE SY-UCOMM,
      W_OLD_CODE LIKE SY-UCOMM,
      W_CNT   TYPE   I,
      W_BASE_DATE LIKE SY-DATUM,
      W_BASE_TIME LIKE SY-UZEIT,
      L_UPH LIKE IT_SHORT-RP01,
      W_REPID LIKE SY-REPID,
      W_DYNNR LIKE SY-DYNNR.


*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER DEFINITION INHERITING FROM CL_GUI_CONTROL.

  PUBLIC SECTION.

    CONSTANTS:  EVENTID_FINISHED TYPE I VALUE 1 .

    CLASS-DATA: INTERVAL TYPE I VALUE '0'.

    EVENTS:     FINISHED .

    METHODS:
*             show_alv,
             CANCEL
                  EXCEPTIONS
                     ERROR,
             CONSTRUCTOR
                 IMPORTING
                     LIFETIME TYPE I OPTIONAL
                     VALUE(SHELLSTYLE) TYPE I OPTIONAL
                     VALUE(PARENT) TYPE REF TO CL_GUI_CONTAINER OPTIONAL
                 EXCEPTIONS
                     ERROR,
             RUN
                 EXCEPTIONS
                     ERROR,
             DISPATCH REDEFINITION.


ENDCLASS.                    "lcl_gui_timer DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
                ON_FINISHED
                       FOR EVENT FINISHED OF LCL_GUI_TIMER.

ENDCLASS.                    "lcl_event_handler DEFINITION


DATA: TIMER_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GUI_TIMER TYPE REF TO LCL_GUI_TIMER,
      EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER,
      TIMEOUT_INTERVAL TYPE I.
*      L_ALV TYPE REF TO CL_GUI_ALV_GRID,
*      FIRST_CALL(1) TYPE C,
**      ok_code     TYPE syucomm,
*      L_IS_STABLE TYPE LVC_S_STBL.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_FINISHED.

* Start Timer again

    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.

* cause PAI
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = 'REFR'.

  ENDMETHOD.                    "on_finished

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    TYPE-POOLS: SFES.

    DATA CLSID(80).
    DATA EVENT_TAB TYPE CNTL_SIMPLE_EVENTS.
    DATA EVENT_TAB_LINE TYPE CNTL_SIMPLE_EVENT.

    IF CLSID IS INITIAL.
      DATA: RETURN,
            GUITYPE TYPE I.

      GUITYPE = 0.
      CALL FUNCTION 'GUI_HAS_OBJECTS'
        EXPORTING
          OBJECT_MODEL = SFES_OBJ_ACTIVEX
        IMPORTING
          RETURN       = RETURN
        EXCEPTIONS
          OTHERS       = 1.
      IF SY-SUBRC NE 0.
        RAISE ERROR.
      ENDIF.

      IF RETURN = 'X'.
        GUITYPE = 1.
      ENDIF.
      IF GUITYPE = 0.
        CALL FUNCTION 'GUI_HAS_OBJECTS'
          EXPORTING
            OBJECT_MODEL = SFES_OBJ_JAVABEANS
          IMPORTING
            RETURN       = RETURN
          EXCEPTIONS
            OTHERS       = 1.
        IF SY-SUBRC NE 0.
          RAISE ERROR.
        ENDIF.

        IF RETURN = 'X'.
          GUITYPE = 2.
        ENDIF.
      ENDIF.

      CASE GUITYPE.
        WHEN 1.
          CLSID = 'Sapgui.InfoCtrl.1'.
        WHEN 2.
          CLSID = 'com.sap.components.controls.sapImage.SapImage'.
      ENDCASE.
    ENDIF.

    CALL METHOD SUPER->CONSTRUCTOR
      EXPORTING
        CLSID      = CLSID
        SHELLSTYLE = 0
        PARENT     = CL_GUI_CONTAINER=>DEFAULT_SCREEN
        AUTOALIGN  = SPACE
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

    CALL METHOD CL_GUI_CFW=>SUBSCRIBE
      EXPORTING
        SHELLID = H_CONTROL-SHELLID
        REF     = ME
      EXCEPTIONS
        OTHERS  = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

* Register the events
    EVENT_TAB_LINE-EVENTID = LCL_GUI_TIMER=>EVENTID_FINISHED.
    APPEND EVENT_TAB_LINE TO EVENT_TAB.

    CALL METHOD SET_REGISTERED_EVENTS
      EXPORTING
        EVENTS = EVENT_TAB.

  ENDMETHOD.                    "constructor

  METHOD CANCEL.

    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = -1
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.


  ENDMETHOD.                    "cancel

  METHOD RUN.

    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = INTERVAL
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.


  ENDMETHOD.                    "run

  METHOD DISPATCH .

    CASE EVENTID.
      WHEN EVENTID_FINISHED.
        RAISE EVENT FINISHED.
    ENDCASE.

    CLEAR TIMER_CONTAINER.

  ENDMETHOD.                    "dispatch

ENDCLASS.                    "lcl_gui_timer IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK BLOCK7 WITH FRAME TITLE TEXT-007.
PARAMETERS :
    P_HRLY AS CHECKBOX DEFAULT 'X',
    P_STK AS CHECKBOX,
    P_ALL  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BLOCK7.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS:   P_WERKS   TYPE MARC-WERKS MEMORY ID WRK OBLIGATORY
*DEFAULT
* 'P001'.
SELECT-OPTIONS :
    S_MATNR   FOR MARA-MATNR MODIF ID REQ,
    S_MTART   FOR MARA-MTART MODIF ID REQ,
    S_MATKL   FOR MARA-MATKL MODIF ID REQ,
    S_DISPO   FOR MARC-DISPO MODIF ID REQ,              "MRP controller
    S_LGPRO   FOR MARC-LGPRO MODIF ID REQ,
    S_PRVBE   FOR PVBE-PRVBE MODIF ID REQ,
    S_SORTF   FOR ZTPP_INPUTPLAN_H-STATUS MODIF ID REQ,
    S_LIFNR   FOR LFA1-LIFNR MODIF ID REQ.
*    S_MODEL   FOR ZTPP_INPUTPLAN_H-MODL,
*    S_BODYNO   FOR ZTPP_INPUTPLAN_H-BODY_SER.

SELECTION-SCREEN END OF BLOCK B1.

*SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 25(5) TEXT-ALL FOR FIELD PC_ALL .
*SELECTION-SCREEN POSITION 18.
*PARAMETERS  PC_ALL  RADIOBUTTON GROUP ICON DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 32(5) TEXT-RED FOR FIELD PC_RED .
*PARAMETERS  PC_RED  RADIOBUTTON GROUP ICON .
*SELECTION-SCREEN COMMENT 46(5) TEXT-YEL FOR FIELD PC_YEL .
*PARAMETERS  PC_YEL  RADIOBUTTON GROUP ICON .
*SELECTION-SCREEN COMMENT 60(5) TEXT-GRE FOR FIELD PC_GRE .
*PARAMETERS  PC_GRE  RADIOBUTTON GROUP ICON .
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(28) TEXT-011  .
*SELECTION-SCREEN COMMENT 31(2) TEXT-012 FOR FIELD P_RED1 .
*PARAMETERS :  P_RED1 TYPE MDKP-BERW1 DEFAULT '2'.
*SELECTION-SCREEN COMMENT 45(2) TEXT-012 FOR FIELD P_YEL1 .
*PARAMETERS :  P_YEL1 TYPE MDKP-BERW1 DEFAULT '4'.
*SELECTION-SCREEN COMMENT 59(2) TEXT-012 FOR FIELD P_GRE1 .
*PARAMETERS :  P_GRE1 TYPE MDKP-BERW1 DEFAULT '999'.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK BLOCK2.
*
*
*SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
*SELECTION-SCREEN : BEGIN OF LINE.
*
*PARAMETERS : P_TOPB AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 8(10) TEXT-TOP FOR FIELD P_TOPB.
*PARAMETERS: P_TOPV TYPE  I DEFAULT '20'.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN SKIP.
*
*SELECTION-SCREEN : BEGIN OF LINE.
*PARAMETERS : P_LESB AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 8(10) TEXT-LES FOR FIELD P_LESB.
*PARAMETERS: P_LESV TYPE  I DEFAULT '16'.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN : BEGIN OF LINE.
*PARAMETERS : P_GRTB AS CHECKBOX.
*SELECTION-SCREEN COMMENT 8(10) TEXT-GRT FOR FIELD P_GRTB.
*PARAMETERS: P_GRTV TYPE  I DEFAULT '4'.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN END OF BLOCK B4.
*

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS:  P_NON RADIOBUTTON GROUP GRP MODIF ID REQ.
SELECTION-SCREEN COMMENT 5(5)  TEXT-M15  FOR FIELD P_NON.
PARAMETERS:  P_VST RADIOBUTTON GROUP GRP MODIF ID REQ.
SELECTION-SCREEN COMMENT 15(10)  TEXT-M11  FOR FIELD P_VST.
PARAMETERS:  P_VSCH RADIOBUTTON GROUP GRP MODIF ID REQ.  "  AS CHECKBOX.
SELECTION-SCREEN COMMENT 35(12) TEXT-M12  FOR FIELD P_VSCH.
PARAMETERS   P_REQ RADIOBUTTON GROUP GRP MODIF ID REQ. "  AS CHECKBOX.
SELECTION-SCREEN COMMENT 50(10) TEXT-M13  FOR FIELD P_REQ.
PARAMETERS   P_REP RADIOBUTTON GROUP GRP MODIF ID REQ.   "AS CHECKBOX.
SELECTION-SCREEN COMMENT 62(10) TEXT-M14  FOR FIELD P_REP.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : P_SAVE AS CHECKBOX.
SELECTION-SCREEN COMMENT 8(12) TEXT-M16 FOR FIELD P_SAVE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B5.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK6 WITH FRAME TITLE TEXT-006.
PARAMETERS :
    P_INTRV TYPE I DEFAULT '10' MODIF ID REQ.
*    p_clock TYPE rlmon-clock.
SELECTION-SCREEN END OF BLOCK BLOCK6.


*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM INIT_DATA.
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF SCREEN-GROUP1 = 'REQ'.
      IF P_STK = 'X' AND P_HRLY = ' '.
*        SCREEN-INPUT = 0.
        SCREEN-ACTIVE = 0.
      ELSE.
        SCREEN-ACTIVE = 1.
      ENDIF.
    ELSE.
      SCREEN-ACTIVE = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

* Set F4 values for MRP Controller
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-LOW.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         DISPO  LIKE MARC-DISPO,
         DESC LIKE T024D-DSNAM,
         END OF VALUE_TAB.

  SELECT DISTINCT A~DISPO DSNAM
     INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T024D AS B
     ON A~DISPO = B~DISPO
     WHERE A~WERKS = 'P001'
     AND A~DISPO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DISPO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'DISPO'
      WINDOW_TITLE    = 'MRP Controller'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_DISPO-HIGH.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         DISPO  LIKE MARC-DISPO,
         DESC LIKE T024D-DSNAM,
         END OF VALUE_TAB.

  SELECT DISTINCT A~DISPO DSNAM
     INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T024D AS B
     ON A~DISPO = B~DISPO
     WHERE A~WERKS = 'P001'
     AND A~DISPO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'DISPO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'DISPO'
      WINDOW_TITLE    = 'MRP Controller'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

* Set F4 values for Prod Storage Location
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LGPRO-LOW.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         LGPRO  LIKE MARC-LGPRO,
         DESC LIKE T001L-LGOBE,
         END OF VALUE_TAB.

  SELECT DISTINCT A~LGPRO LGOBE INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T001L AS B
     ON A~LGPRO = B~LGORT
     WHERE A~WERKS = 'P001'
       AND LGPRO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'LGPRO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'LGPRO'
      WINDOW_TITLE    = 'Prod Storage Loca'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LGPRO-HIGH.
  DATA : BEGIN OF VALUE_TAB OCCURS 0,
         LGPRO  LIKE MARC-LGPRO,
         DESC LIKE T001L-LGOBE,
         END OF VALUE_TAB.

  SELECT DISTINCT A~LGPRO LGOBE INTO TABLE VALUE_TAB
     FROM MARC AS A
     INNER JOIN T001L AS B
     ON A~LGPRO = B~LGORT
     WHERE A~WERKS = 'P001'
       AND LGPRO <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'LGPRO'
      DYNPPROG        = W_REPID
      DYNPNR          = W_DYNNR
      DYNPROFIELD     = 'LGPRO'
      WINDOW_TITLE    = 'Prod Storage Loca'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = VALUE_TAB
    EXCEPTIONS
      PARAMETER_ERROR = 1.

* Set F4 values for Supply Area
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PRVBE-LOW.
*  DATA : BEGIN OF VALUE_TAB OCCURS 0,
*         PRVBE  LIKE MARC-VSPVB,
*         DESC LIKE PVBE-PVBTX,
*         END OF VALUE_TAB.
*
*  SELECT DISTINCT A~VSPVB PVBTX
*     INTO TABLE VALUE_TAB
*     FROM MARC AS A
*     INNER JOIN PVBE AS B
*     ON A~VSPVB = B~PRVBE
*     WHERE A~WERKS = 'P001'
*     and A~PRVBE <> ' '.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*       EXPORTING
*            RETFIELD        = 'PRVBE'
*            DYNPPROG        = w_REPID
*            DYNPNR          = w_DYNNR
*            DYNPROFIELD     = 'PRVBE'
*            WINDOW_TITLE    = 'Supply Area'
*            VALUE_ORG       = 'S'
*       TABLES
*            VALUE_TAB       = VALUE_TAB
*       EXCEPTIONS
*            PARAMETER_ERROR = 1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

*  PERFORM GET_INPUT_PLAN.
  PERFORM CHECKING_BATCH_JOB.
  IF P_HRLY = 'X'  OR P_ALL = 'X'.
    MESSAGE S000(ZMPP) WITH 'Reading VIN'.
    PERFORM GET_VIN_DATA TABLES IT_SHORT_BW.
    MESSAGE S000(ZMPP) WITH 'Reading RESB'.
    PERFORM GET_RESB.
    MESSAGE S000(ZMPP) WITH 'Make data'.
    PERFORM MAKE_DATA.
    MESSAGE S000(ZMPP) WITH 'Process data'.
    PERFORM PROCESS_DATA.
    MESSAGE S000(ZMPP) WITH 'Prepare to display'.

    IF SY-BATCH = ' '.
      CASE 'X'.
        WHEN P_VST .
          CALL SCREEN 0200.
        WHEN P_VSCH.
          CALL SCREEN 0210.
        WHEN P_REQ.
          CALL SCREEN 0300.
        WHEN P_REP.
          CALL SCREEN 0310.
        WHEN OTHERS.
          CALL SCREEN 0800.
      ENDCASE.
    ENDIF.
  ELSE.
    PERFORM PROCESS_STOCK.
    IF SY-BATCH = ' '.
      CALL SCREEN 0800.
    ENDIF.
  ENDIF.
  IF P_SAVE = 'X'.
    PERFORM SAVE_TO_TABLE.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_WAIT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0874   text
*      -->P_L_PRJ  text
*----------------------------------------------------------------------*
FORM GET_WAIT_TIME USING    PA_VAL  PA_RETURN  PA_MODEL.
  CLEAR: PA_RETURN .

  SELECT SINGLE ITEM4  INTO PA_RETURN
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = 'ZAPP903R_INPUT_PLAN'
     AND KEY2 = PA_VAL
     AND KEY3 = PA_MODEL.
ENDFORM.                    " GET_WAIT_TIME
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING    PA_KALID.
  SELECT SINGLE FABKL INTO PA_KALID
    FROM T001W
   WHERE WERKS = 'P001' .
ENDFORM.                    " READ_SHOP_CALID

**&---------------------------------------------------------------------
**
**&      Form  GET_MASTER
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_L_COUNT  text
**----------------------------------------------------------------------
**
*FORM GET_MASTER USING    PA_PCNT  PA_BCNT  PA_KALID
*                         PA_WBS  PA_PRJ    PA_MODEL .
*  DATA: L_DATE               TYPE D ,
*        L_CHK                TYPE P DECIMALS 3,
*        L_DAY                LIKE KAPA-TAGNR,      " Day
*        L_TIME               TYPE KAPENDZT  ,      " Times for working
*        L_UPH                TYPE ZVPP_LD-LRATE,   " UPH
*        L_COUNT              TYPE I .
*
*  L_DATE = SY-DATUM .
*  PERFORM READ_WORKING_DATE USING '+'  PA_KALID  L_DATE.
*  IF L_DATE = SY-DATUM .
*    PERFORM GET_DAY          USING L_DATE  L_DAY         .
*    PERFORM GET_WORKTIME     USING L_DATE  L_TIME  L_DAY  'B'.
*    PERFORM GET_UPH          USING L_DATE  L_UPH   'B'   .
*    L_CHK = L_TIME / 3600 .
*    PA_BCNT = CEIL( L_UPH * L_CHK )  .
*    PERFORM GET_WORKTIME     USING L_DATE  L_TIME  L_DAY  'T'.
*    PERFORM GET_UPH          USING L_DATE  L_UPH   'T'   .
*    L_CHK = L_TIME / 3600 .
*    PA_BCNT = CEIL( L_UPH * L_CHK )  .
*  ELSE.
*    PA_BCNT = PA_PCNT = 0 .
*  ENDIF.
*ENDFORM.                    " GET_MASTER

*&---------------------------------------------------------------------*
*&      Form  read_working_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1055   text
*      -->P_PA_KALID  text
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      CORRECT_OPTION               = PA_TYPE
      DATE                         = PA_WDATE
      FACTORY_CALENDAR_ID          = PA_KALID
    IMPORTING
      DATE                         = PA_WDATE
    EXCEPTIONS
      CALENDAR_BUFFER_NOT_LOADABLE = 1
      CORRECT_OPTION_INVALID       = 2
      DATE_AFTER_RANGE             = 3
      DATE_BEFORE_RANGE            = 4
      DATE_INVALID                 = 5
      FACTORY_CALENDAR_NOT_FOUND   = 6
      OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  get_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM GET_DAY USING    PA_WDATE  PA_DAY.
  DATA: L_DAY         LIKE SCAL-INDICATOR .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      DATE = PA_WDATE
    IMPORTING
      DAY  = L_DAY.

  PA_DAY = L_DAY.
ENDFORM.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  get_worktime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_TIME  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM GET_WORKTIME     USING    PA_WDATE  PA_WKTIME  PA_DAY  PA_WC.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_FLAG        TYPE C ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA       LIKE TABLE OF ZVPP_CAPACITY      WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL = PA_WC
     AND DATUB >= PA_WDATE .

  SORT LT_CAPA BY DATUB .
  READ TABLE LT_CAPA INDEX 1.
  L_DATE = LT_CAPA-DATUB    .

  LOOP AT LT_CAPA WHERE DATUB = L_DATE AND TAGNR = PA_DAY .
    CLEAR: L_EINZT.
    SELECT SINGLE EINZT INTO L_EINZT
      FROM TC37A
     WHERE SCHGRUP  = LT_CAPA-MOSID
       AND KAPTPROG = LT_CAPA-TPROG
       AND ENDDA   >= PA_WDATE
       AND BEGDA   <= PA_WDATE     .
    L_WTIME  = L_WTIME  + L_EINZT.
  ENDLOOP.
  PA_WKTIME = L_WTIME .
ENDFORM.                    " GET_WORKTIME

*&---------------------------------------------------------------------*
*&      Form  get_uph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_UPH  text
*      -->P_IT_MASTER_SHIFT  text
*----------------------------------------------------------------------*
FORM GET_UPH USING    PA_WDATE  PA_UPH  PA_WC.
  DATA: W_UPH  LIKE ZTPP_STATUS-UPH.
  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      DATE  = PA_WDATE
*     SHIFT =
      SHOP  = PA_WC
    IMPORTING
      UPH   = W_UPH.
  PA_UPH = W_UPH.

*  DATA lw_ld          LIKE zvpp_ld .
*
*    SELECT SINGLE * INTO lw_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = PA_WC     .
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_INPUT_PLAN'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER
    EXPORTING
      CONTAINER_NAME              = WA_CUSTOM_CONTROL
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = W_REPID
        TXT2  = SY-SUBRC
        TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID
    EXPORTING
      I_PARENT      = GRID_CONTAINER
      I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.
* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog' P_GUBUN P_FIELD.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME .
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
*      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = IT_EXCLUDE[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_INPUT_PLAN[]
      IT_SORT              = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
    EXPORTING
      CONTROL = ALV_GRID.

ENDFORM.                    " assign_itab1_to_alv
*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
  CONCATENATE 'Screen ' SY-DYNNR INTO WA_IS_LAYOUT-GRID_TITLE
  SEPARATED BY SPACE.

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'STATUS'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

*
* CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
**      I_INTERNAL_TABNAME     = LW_ITAB
*      I_STRUCTURE_NAME       = 'ZTPP_INPUTPLAN_H'
*    CHANGING
*      CT_FIELDCAT            = IT_FIELDCAT[]
*    EXCEPTIONS
*      INCONSISTENT_INTERFACE = 1
*      PROGRAM_ERROR          = 2
*      OTHERS                 = 3.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'STATUS'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'RP',
                                  'E' 'OUTPUTLEN'   '4',


                                  'S' 'MODL'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Model',
                                  'E' 'OUTPUTLEN'   '5',


                                  'S' 'BODY_SER'     ' ',
                                  ' ' 'COLTEXT'     'Body No',
                                  'E' 'OUTPUTLEN'   '12',
*
*                                  'S' 'RPCUR'     ' ',
*                                  ' ' 'COLTEXT'     'Time Stamp',
*                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'SERIAL'    ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Serial No',
                                  'E' 'OUTPUTLEN'   '10'.


*                                  'S' 'QTY'    ' ',
*                                  ' ' 'COLTEXT'     'Quantity',
*                                  'E' 'OUTPUTLEN'   '10',
*
*
*                                  'S' 'MESS'       ' ',
*                                  ' ' 'COLTEXT'     'Error Message',
*                                  'E' 'OUTPUTLEN'   '255'.
**

ENDFORM.                    "BUILD_FIELD_CATALOG

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS.
*  DATA LS_EXCLUDE TYPE UI_FUNC.
*
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  GET_INPUT_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_INPUT_PLAN.
*
*  FIELD-SYMBOLS: <FS>.
*  SELECT * INTO TABLE IT_INPUT_PLAN
*    FROM ZTPP_INPUTPLAN_H
*    WHERE MODL IN S_MODEL
*      AND BODY_SER IN S_BODYNO
*      AND STATUS < 18.
*
*ENDFORM.                    " GET_INPUT_PLAN
*&---------------------------------------------------------------------*
*&      Form  GET_VIN_SCHUDLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VIN_DATA TABLES IT_SHORT_BW STRUCTURE ZMMS_SHORT.
  DATA:   L_RP(2) TYPE N,
          L_TEXT(40),
          L_INDEX LIKE SY-TABID,
          L_COUNT TYPE I,
          L_HRS(2) TYPE N,
          L_VIN LIKE IT_VIN-VIN,
          L_CURR_STATUS LIKE RESB-SORTF,
          L_STATUS LIKE ZTPP_INPUTPLAN_H-STATUS.

  DATA: BEGIN OF LT_RP OCCURS 0,
        STATUS LIKE ZTPP_INPUTPLAN_H-STATUS,
        END OF LT_RP.

  DATA: BEGIN OF LT_VIN OCCURS 0,
        VIN(18),
        RP LIKE RESB-SORTF,
        HRS(2) TYPE N,
        CURR_STATUS  LIKE RESB-SORTF,
        END OF LT_VIN.


  FIELD-SYMBOLS <FS>.

  REFRESH: IT_INPUT_PLAN, IT_VIN, IT_RESB, IT_SHORT.

  TIMEOUT_INTERVAL = P_INTRV * 60.

  SELECT * INTO TABLE IT_INPUT_PLAN
     FROM ZTPP_INPUTPLAN_H
*     WHERE MODL IN S_MODEL
*       AND BODY_SER IN S_BODYNO
       WHERE STATUS < 18.

  SORT IT_INPUT_PLAN BY STATUS DESCENDING SERIAL.

  READ TABLE IT_INPUT_PLAN INDEX 1.
*  L_STATUS = IT_INPUT_PLAN-STATUS.
  W_BASE_TIME = IT_INPUT_PLAN-ZETIM.
  W_BASE_DATE = IT_INPUT_PLAN-ZEDAT.

  PERFORM GET_UPH USING W_BASE_DATE L_UPH 'T'.
  IF L_UPH IS INITIAL.
    L_UPH = 630.
  ENDIF.

  SELECT * INTO TABLE IT_HOLD
     FROM ZTPP_INPUTPLAN_H
*     WHERE MODL IN S_MODEL
*       AND BODY_SER IN S_BODYNO
       WHERE STATUS < 18
       AND T = 'HOLD'.

  IF SY-SUBRC = 0.
    PERFORM RESEQ_INPUT_PLAN.
  ENDIF.

  LOOP AT IT_INPUT_PLAN.
    LT_RP-STATUS =  L_RP = IT_INPUT_PLAN-STATUS + 1.
    COLLECT LT_RP.
  ENDLOOP.

  L_COUNT = 1.
  L_HRS = 1.

  LOOP AT LT_RP.
*    READ TABLE IT_INPUT_PLAN WITH KEY STATUS = LT_RP-STATUS.
    L_COUNT = 1.
    L_HRS = 1.

    LOOP AT IT_INPUT_PLAN WHERE STATUS < LT_RP-STATUS.
      IF L_COUNT > L_UPH.
        L_HRS = L_HRS + 1.
        L_COUNT = 1.
      ENDIF.
      IF L_HRS <= 40.
        CONCATENATE IT_INPUT_PLAN-MODL IT_INPUT_PLAN-BODY_SER
                    INTO LT_VIN-VIN.

        LT_VIN-CURR_STATUS = IT_INPUT_PLAN-STATUS.
        LT_VIN-RP = LT_RP-STATUS.
        LT_VIN-HRS = L_HRS.
        COLLECT LT_VIN.
      ENDIF.
      L_COUNT = L_COUNT + 1.
      CLEAR: LT_VIN.
    ENDLOOP.

  ENDLOOP.

  SORT LT_VIN BY VIN ASCENDING RP DESCENDING.
  READ TABLE LT_VIN INDEX 1.
  L_VIN =  LT_VIN-VIN.
  L_CURR_STATUS = LT_VIN-CURR_STATUS .
  LOOP AT LT_VIN.
    IF L_VIN <> LT_VIN-VIN.
      IT_VIN-VIN = L_VIN.
      IT_VIN-RP = L_CURR_STATUS.
      APPEND IT_VIN.
      CLEAR: IT_VIN.
      L_VIN = LT_VIN-VIN.
      L_CURR_STATUS = LT_VIN-CURR_STATUS .
    ENDIF.
    IF LT_VIN-HRS > 0.
      L_RP = LT_VIN-RP.
      CONCATENATE 'IT_VIN-RP' L_RP INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS>.
      MOVE LT_VIN-HRS TO <FS>.
    ENDIF.
  ENDLOOP.

  IT_VIN-VIN = L_VIN.
  IT_VIN-RP = L_CURR_STATUS.
  APPEND IT_VIN.

** For BW interface
*  PERFORM GET_RESB.
*  PERFORM MAKE_DATA.
*  PERFORM PROCESS_DATA.

*  LOOP AT IT_SHORT.
*    MOVE-CORRESPONDING IT_SHORT TO IT_SHORT_BW.
*    APPEND IT_SHORT_BW.
*  ENDLOOP.

ENDFORM.                    " GET_VIN_SCHUDLE
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_210 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
*    PERFORM BUILD_SORTCAT_DISPLAY.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG_210 USING 'IT_VIN'.
    PERFORM ASSIGN_ITAB_TO_ALV_210.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_210  OUTPUT
*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_210 USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,
  DATA: L_CN(2) TYPE N,
  L_RP(30).

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

*
* CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
**      I_INTERNAL_TABNAME     = LW_ITAB
*      I_STRUCTURE_NAME       = 'ZTPP_INPUTPLAN_H'
*    CHANGING
*      CT_FIELDCAT            = IT_FIELDCAT[]
*    EXCEPTIONS
*      INCONSISTENT_INTERFACE = 1
*      PROGRAM_ERROR          = 2
*      OTHERS                 = 3.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'VIN'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Vin No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'RP'    ' ',
                                  ' ' 'COLTEXT'     'Status',
                                  'E' 'OUTPUTLEN'   '5'.

*
*                                  'S' 'RP01_TIME'    ' ',
*                                  ' ' 'COLTEXT'     'Model',
*                                  'E' 'OUTPUTLEN'   '5'.
  L_CN = '00'.
  DO 18 TIMES.
    L_CN = L_CN + 1.

*    READ TABLE it_day WITH KEY seq = l_cn.


    CONCATENATE 'RP' L_CN INTO L_RP.


    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_RP        ' ',
                                   ' ' 'COLTEXT'     L_RP,
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: L_RP.
  ENDDO.
ENDFORM.                    "BUILD_FIELD_CATALOG_210
*&--------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_210
*&--------------------------------------------------------------*
*       text
*---------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_210.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = IT_EXCLUDE[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_VIN[]
      IT_SORT              = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
    EXPORTING
      CONTROL = ALV_GRID.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_210
*&----------------------------------------------------------------*
*&      Module  USER_COMMAND_0210  INPUT
*&----------------------------------------------------------------*
*       text
*-----------------------------------------------------------------*
MODULE USER_COMMAND_0210 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
*      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0210  INPUT
*&---------------------------------------------------------------*
*&      Form  GET_req
*&---------------------------------------------------------------*
*       text
*----------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------*
FORM GET_RESB.
  DATA: LT_RESB LIKE TABLE OF IT_RESB WITH HEADER LINE.
  DATA: BEGIN OF LT_TEST OCCURS 0,
       MATNR LIKE RESB-MATNR,
       END OF LT_TEST.

  DATA: L_PLNUM LIKE RESB-PLNUM,
        L_VIN(18),
        L_DATE_C(8),
        L_TIME_C(8),
        L_RP(2) TYPE N,
        L_TEXT(40),
        L_LINE TYPE I,
        L_INDEX LIKE SY-TABIX,
        L_COUNT TYPE I,
        L_MATNR LIKE RESB-MATNR.

  DATA: BEGIN OF LT_RSNUM OCCURS 0,
        RSNUM LIKE PLAF-RSNUM,
        END OF LT_RSNUM.

  RANGES: R_RSNUM FOR RESB-RSNUM,
          R_MATNR FOR RESB-MATNR.

  FIELD-SYMBOLS <FS>.
  IF IT_INPUT_PLAN[] IS INITIAL.
    MESSAGE E000(ZZ) WITH 'No input plan data'.
  ENDIF.

  REFRESH IT_21DAY.

  IF S_LIFNR[] IS INITIAL.
    SELECT A~MATNR LIFNR PRVBE SORTF A~MEINS INTO TABLE IT_21DAY
      FROM ZTMM_PARTS_21DAY AS A
       INNER JOIN MARA AS B
       ON A~MATNR = B~MATNR
       INNER JOIN MARC AS C
       ON A~MATNR = C~MATNR
       AND A~WERKS = C~WERKS
      WHERE A~WERKS = 'P001'
        AND A~MATNR IN S_MATNR
        AND MTART IN S_MTART
        AND MATKL IN S_MATKL
        AND C~DISPO IN S_DISPO
        AND LGPRO IN S_LGPRO
        AND VSPVB IN S_PRVBE
        AND LGPRO <> 'P500'.
  ELSE.
    SELECT A~MATNR LIFNR PRVBE SORTF A~MEINS INTO TABLE IT_21DAY
     FROM ZTMM_PARTS_21DAY AS A
      INNER JOIN MARA AS B
      ON A~MATNR = B~MATNR
      INNER JOIN MARC AS C
      ON A~MATNR = C~MATNR
      AND A~WERKS = C~WERKS
*      INNER JOIN EORD AS D
*      ON A~MATNR = D~MATNR
     WHERE A~WERKS = 'P001'
       AND A~MATNR IN S_MATNR
       AND MTART IN S_MTART
       AND MATKL IN S_MATKL
       AND C~DISPO IN S_DISPO
       AND LGPRO IN S_LGPRO
       AND VSPVB IN S_PRVBE
       AND LIFNR IN S_LIFNR
*       AND FLIFN = 'X'
*       AND FEBEL = 'X'
*       AND VDATU <= SY-DATUM
*       AND BDATU >= SY-DATUM
       AND LGPRO <> 'P500'.
  ENDIF.
  SORT IT_21DAY BY MATNR.
*  IF NOT S_LIFNR[] IS INITIAL.
*    SELECT MATNR LIFNR INTO TABLE IT_LIFNR
*        FROM EORD
*        WHERE LIFNR = S_LIFNR
*          AND FLIFN = 'X'
*          AND FEBEL = 'X'
*          AND VDATU <= SY-DATUM
*          AND BDATU >= SY-DATUM.
*    IF SY-SUBRC = 0.
*      LOOP AT LT_21DAY.
*        L_INDEX = SY-TABIX.
*        READ TABLE IT_LIFNR WITH KEY MATNR = LT_21DAY-MATNR.
*        IF SY-SUBRC = 0.
*        ELSE.
*          DELETE LT_21DAY INDEX L_INDEX.
*        ENDIF.
*      ENDLOOP.
*    ELSE.
*      MESSAGE E000(ZZ) WITH 'No data in source list'.
*    ENDIF.
*  ENDIF.

**Paul Comment 071111
**SELECT MATNR PLNUM BDMNG MEINS
**     INTO TABLE LT_RESB
**      FROM RESB
**      FOR ALL ENTRIES IN IT_INPUT_PLAN
**      WHERE PLNUM = IT_INPUT_PLAN-PLNUM
**        AND MATNR IN R_MATNR.
*


  SELECT RSNUM INTO TABLE LT_RSNUM
    FROM PLAF
    FOR ALL ENTRIES IN IT_INPUT_PLAN
    WHERE PLNUM = IT_INPUT_PLAN-PLNUM.

  LOOP AT IT_21DAY.
    R_MATNR-SIGN = 'I'.
    R_MATNR-OPTION = 'EQ'.
    R_MATNR-LOW = IT_21DAY-MATNR.
    APPEND R_MATNR.
  ENDLOOP.

  SORT LT_RSNUM BY RSNUM.
  DESCRIBE TABLE LT_RSNUM LINES L_LINE.

  IF L_LINE < 200.
    LOOP AT LT_RSNUM.
      R_RSNUM-SIGN = 'I'.
      R_RSNUM-OPTION = 'EQ'.
      R_RSNUM-LOW = LT_RSNUM-RSNUM.
      APPEND R_RSNUM.
    ENDLOOP.

*    SELECT MATNR SORTF PLNUM PRVBE BDMNG MEINS
    SELECT MATNR PLNUM BDMNG MEINS
     INTO TABLE LT_RESB
      FROM RESB
      WHERE RSNUM IN R_RSNUM.
** Furong on 05/21/12 for SAP tuning
*        AND MATNR IN R_MATNR.
    DELETE LT_RESB WHERE MATNR NOT IN R_MATNR.    "Addition
** end on 05/21/12
  ELSE.
    LOOP AT LT_RSNUM.
      IF L_COUNT >= 200.
*        SELECT MATNR SORTF PLNUM PRVBE BDMNG MEINS
        SELECT MATNR PLNUM BDMNG MEINS
               APPENDING TABLE LT_RESB
           FROM RESB
           WHERE RSNUM IN R_RSNUM.
** Furong on 05/21/12 for SAP tuning
*        AND MATNR IN R_MATNR.
** end on 05/21/12

        REFRESH R_RSNUM.
        CLEAR: R_RSNUM, L_COUNT.
      ELSE.
        R_RSNUM-SIGN = 'I'.
        R_RSNUM-OPTION = 'EQ'.
        R_RSNUM-LOW = LT_RSNUM-RSNUM.
        APPEND R_RSNUM.
        L_COUNT  = L_COUNT + 1.
      ENDIF.
    ENDLOOP.
** Furong on 05/21/12 for SAP tuning
    DELETE LT_RESB WHERE MATNR NOT IN R_MATNR.
** End on 05/21/12

    IF L_COUNT > 0.
*      SELECT MATNR SORTF PLNUM PRVBE BDMNG MEINS
      SELECT MATNR PLNUM BDMNG MEINS
           APPENDING TABLE LT_RESB
       FROM RESB
       WHERE RSNUM IN R_RSNUM.
** Furong on 05/21/12 for SAP tuning
*        AND MATNR IN R_MATNR.
      DELETE LT_RESB WHERE MATNR NOT IN R_MATNR.    "Addition
** end on 05/21/12

    ENDIF.
  ENDIF.
**E<



  SORT LT_RESB BY PLNUM MATNR.
  SORT IT_VIN BY VIN.

  LOOP AT LT_RESB.

    IT_RESB = LT_RESB.

    IF L_PLNUM <> LT_RESB-PLNUM.
      L_PLNUM = LT_RESB-PLNUM.
      READ TABLE IT_INPUT_PLAN WITH KEY PLNUM = IT_RESB-PLNUM.
      CONCATENATE IT_INPUT_PLAN-MODL IT_INPUT_PLAN-BODY_SER
       INTO L_VIN.
    ENDIF.
    IT_RESB-VIN = L_VIN.
    IF L_MATNR <> LT_RESB-MATNR.
      L_MATNR = LT_RESB-MATNR.
      READ TABLE IT_21DAY WITH KEY MATNR = LT_RESB-MATNR
                  BINARY SEARCH.
      L_RP = IT_21DAY-SORTF.
    ENDIF.
*    L_RP = LT_RESB-SORTF.

    READ TABLE IT_VIN WITH KEY VIN = L_VIN
                BINARY SEARCH.

    IF SY-SUBRC = 0.
      CONCATENATE 'IT_VIN-RP' L_RP INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS>.
      MOVE <FS> TO IT_RESB-HRS.
    ENDIF.
    IF IT_RESB-HRS > 0.
      APPEND IT_RESB.
    ENDIF.
    CLEAR: IT_RESB.
  ENDLOOP.

*  SORT IT_RESB BY MATNR SORTF VIN .

ENDFORM.                    " GET_req
*&--------------------------------------------------------------*
*&      Module  DISPLAY_ALV_310  OUTPUT
*&--------------------------------------------------------------*
*       text
*---------------------------------------------------------------*
MODULE DISPLAY_ALV_300 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY_300.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG_300 USING 'IT_RESB'.
    PERFORM ASSIGN_ITAB_TO_ALV_300.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_310  OUTPUT
*&---------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_300
*&---------------------------------------------------------------*
*       text
*----------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_300.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = IT_EXCLUDE[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_RESB[]
      IT_SORT              = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
    EXPORTING
      CONTROL = ALV_GRID.


ENDFORM.                    " ASSIGN_ITAB_TO_ALV_300
*&---------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_300
*&---------------------------------------------------------------*
*       text
*----------------------------------------------------------------*
*      -->P_1924   text
*----------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_300  USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'MATNR'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Part',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'SORTF'     ' ',
                                  ' ' 'COLTEXT'     'Sort Str',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'VIN'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'VIN',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'PLNUM'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Plan Order',
                                  'E' 'OUTPUTLEN'   '10'.

*                                  'S' 'BODY_SER'     ' ',
*                                  ' ' 'COLTEXT'     'Body No',
*                                  'E' 'OUTPUTLEN'   '12',
*
*                                  'S' 'RPCUR'     ' ',
*                                  ' ' 'COLTEXT'     'Time Stamp',
*                                  'E' 'OUTPUTLEN'   '18',

*                                  'S' 'QTY'    ' ',
*                                  ' ' 'COLTEXT'     'Quantity',
*                                  'E' 'OUTPUTLEN'   '10',
*
*
*                                  'S' 'MESS'       ' ',
*                                  ' ' 'COLTEXT'     'Error Message',
*                                  'E' 'OUTPUTLEN'   '255'.
**


ENDFORM.                    " BUILD_FIELD_CATALOG_300
*&---------------------------------------------------------------*
*&      Form  BUILD_SORTCAT_DISPLAY_300
*&---------------------------------------------------------------*
*       text
*----------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY_300.
  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'MATNR'.
  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

  IT_SORT-SPOS           = 2.
  IT_SORT-FIELDNAME      = 'SORTF'.
  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " BUILD_SORTCAT_DISPLAY_300
*&--------------------------------------------------------------*
*&      Form  make_data
*&--------------------------------------------------------------*
*       text
*---------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------*
FORM MAKE_DATA.
  DATA: BEGIN OF LT_RESB OCCURS 0,
        MATNR LIKE RESB-MATNR,
        SORTF LIKE RESB-SORTF,
        BDMNG  LIKE RESB-BDMNG,
        HRS(2) TYPE N,
        END OF LT_RESB.
  DATA: L_DATE LIKE SY-DATUM,
        L_TIME LIKE SY-UZEIT,
        L_SEQ(2) TYPE N,
        L_TEXT(40),
        L_SORTF LIKE LT_RESB-SORTF,
        L_MATNR LIKE RESB-MATNR.

  FIELD-SYMBOLS <FS>.

  LOOP AT IT_RESB.
    LT_RESB-MATNR = IT_RESB-MATNR.
*    LT_RESB-SORTF = IT_RESB-SORTF.
    LT_RESB-BDMNG = IT_RESB-BDMNG.
    LT_RESB-HRS   = IT_RESB-HRS.
    COLLECT LT_RESB.
  ENDLOOP.

  SORT LT_RESB BY MATNR HRS.
  READ TABLE LT_RESB INDEX 1.
  L_MATNR = LT_RESB-MATNR.
  L_SORTF = LT_RESB-SORTF.
  LOOP AT LT_RESB.
    IF LT_RESB-MATNR <> L_MATNR.
      IT_DATA-MATNR = L_MATNR.
      IT_DATA-SORTF = L_SORTF.
      L_MATNR = LT_RESB-MATNR.
      L_SORTF = LT_RESB-SORTF.
      APPEND IT_DATA.
      CLEAR: IT_DATA.
    ENDIF.
*    IT_DATA-SORTF = LT_RESB-SORTF.

    CONCATENATE 'IT_DATA-RP' LT_RESB-HRS INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS>.
    MOVE LT_RESB-BDMNG TO <FS>.

    CLEAR: L_TEXT.
  ENDLOOP.
  IT_DATA-MATNR = L_MATNR.
  IT_DATA-SORTF = L_SORTF.
  APPEND IT_DATA.

ENDFORM.  "make_data
*&--------------------------------------------------------------*
*&      Module  DISPLAY_ALV_310  OUTPUT
*&--------------------------------------------------------------*
*       text
*---------------------------------------------------------------*
MODULE DISPLAY_ALV_310 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY_310.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG_310 USING 'IT_DATA'.
    PERFORM ASSIGN_ITAB_TO_ALV_310.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_310  OUTPUT
*&--------------------------------------------------------------*
*&      Form  BUILD_SORTCAT_DISPLAY_310
*&--------------------------------------------------------------*
*       text
*---------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY_310.
  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'MATNR'.
  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

  IT_SORT-SPOS           = 2.
  IT_SORT-FIELDNAME      = 'SORTF'.
  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " BUILD_SORTCAT_DISPLAY_310
*&-------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_310
*&-------------------------------------------------------------*
*       text
*--------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_310.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = IT_EXCLUDE[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_DATA[]
      IT_SORT              = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
    EXPORTING
      CONTROL = ALV_GRID.



ENDFORM.                    " ASSIGN_ITAB_TO_ALV_310
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_310
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2539   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_310  USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,
  DATA: L_RP(4),
        L_HR(10).

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'MATNR'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Part',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'SORTF'     ' ',
                                  ' ' 'COLTEXT'     'Sort Str',
                                  'E' 'OUTPUTLEN'   '10'.

  LOOP AT IT_HOUR TO 40.
    CONCATENATE 'RP' IT_HOUR-SEQ INTO L_RP.
    CONCATENATE IT_HOUR-SEQ 'HR' INTO L_HR SEPARATED BY SPACE.
    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                    'S'  L_RP        ' ',
                                    ' ' 'COLTEXT'     L_HR,
                                    'E' 'OUTPUTLEN'   '6'.

  ENDLOOP.

ENDFORM.                    " BUILD_FIELD_CATALOG_310

*INCLUDE ZMMR_PARTS_SHORTAGE_F01.
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_0800 OUTPUT.
  IF GRID_CONTAINER_800 IS INITIAL.
    PERFORM CREATE_CONTAINER_OBJECT_800.
    PERFORM SET_ATTRIBUTES_ALV_GRID_800.
*    PERFORM BUILD_SORTCAT_DISPLAY.
*    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG_800 USING 'IT_SHORT'.
    PERFORM ASSIGN_ITAB_TO_ALV_800.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID_800->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_OBJECT_800.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER_800
    EXPORTING
      CONTAINER_NAME              = WA_CUSTOM_CONTROL_800
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = W_REPID
        TXT2  = SY-SUBRC
        TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID_800
    EXPORTING
      I_PARENT      = GRID_CONTAINER_800
      I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_800.
  DATA: L_DATE_C(10),
        L_TIME(8),
        L_UPH_C(6).

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
  CONCATENATE W_BASE_DATE+4(2) '/' W_BASE_DATE+6(2)
              '/' W_BASE_DATE+0(4) INTO L_DATE_C.
  CONCATENATE W_BASE_TIME+0(2) ':' W_BASE_TIME+2(2) ':'
              W_BASE_TIME+4(2) INTO L_TIME.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-CTAB_FNAME  = 'COLOR'.
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.
  CONCATENATE 'As of' L_DATE_C L_TIME INTO WA_IS_LAYOUT-GRID_TITLE
 SEPARATED BY SPACE.
  L_UPH_C =  L_UPH.
  IF L_UPH > 0.
    WA_IS_LAYOUT-GRID_TITLE+50(5) = 'UPH :'.
    MOVE: L_UPH_C TO WA_IS_LAYOUT-GRID_TITLE+56(10).
  ENDIF.
*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

  WA_IS_LAYOUT-ZEBRA             = 'X'.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3194   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_800 USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,
  DATA: L_CN(2) TYPE N,
  L_RP(30),
  L_HR(10).

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'MATNR'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Part',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'MAKTX'    ' ',
                                 ' ' 'KEY'         'X',
                               ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                   'S' 'HR_OH'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'COLTEXT'     'On Hand',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'LIFNR'    ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '18',

                                'S' 'RP'    ' ',
                                  ' ' 'COLTEXT'     'RP',
                                  'E' 'OUTPUTLEN'   '3',

                                 'S' 'PRVBE'    ' ',
                                 ' ' 'COLTEXT'     'Supp Area',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'DSNAM'    ' ',
                             ' ' 'COLTEXT'     'MRP Controller',
                                  'E' 'OUTPUTLEN'   '18',

                                'S' 'MATKL'    ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'LN_OHQTY'    ' ',
                                 ' ' 'COLTEXT'     'Stock',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '13'.
*
*                                 'S' 'WH_OHQTY'    ' ',
*                                  ' ' 'COLTEXT'     'WH Qty',
*                                 ' ' 'DECIMALS_O'  '0',
*                                 'E' 'OUTPUTLEN'   '13',

*                                 'S' 'ADJ_QTY'    ' ',
*                                  ' ' 'COLTEXT'     'Adj Qty',
*                                 ' ' 'DECIMALS_O'  '0',
*                                 'E' 'OUTPUTLEN'   '13'.

  L_CN = '00'.
  DO 40 TIMES.
    L_CN = L_CN + 1.

*    READ TABLE it_day WITH KEY seq = l_cn.


    CONCATENATE 'RP' L_CN INTO L_RP.
    CONCATENATE  L_CN 'Hr' INTO L_HR SEPARATED BY SPACE.

    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_RP        ' ',
                                   ' ' 'COLTEXT'     L_HR,
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '10'.
    CLEAR: L_RP.
  ENDDO.
  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                    'S' 'TOTAL'      ' ',
                                    ' ' 'COLTEXT'     'Total',
                                    ' ' 'DECIMALS_O'  '0',
                                    'E' 'OUTPUTLEN'   '10'.

ENDFORM.                    " BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_800.
  CALL METHOD ALV_GRID_800->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = IT_EXCLUDE[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_SHORT[]
      IT_SORT              = IT_SORT[].

** ENTER
  CALL METHOD ALV_GRID_800->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

* Cursor----
  CALL METHOD ALV_GRID_800->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
    EXPORTING
      CONTROL = ALV_GRID_800.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0800 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REFR'.
      SUBMIT ZMMR_PARTS_SHORTAGE
       WITH S_MATNR IN S_MATNR
       WITH S_MTART IN S_MTART
       WITH S_MATKL IN S_MATKL
       WITH S_DISPO IN S_DISPO
       WITH S_LGPRO IN S_LGPRO
       WITH S_PRVBE IN S_PRVBE
       WITH S_SORTF IN S_SORTF
       WITH S_LIFNR IN S_LIFNR.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_cell_color
*&---------------------------------------------------------------------*
*       Set Cell Color
*----------------------------------------------------------------------*
FORM SET_CELL_COLOR  USING    U_COL
                              U_INT
                              U_FIELD
                     CHANGING COLOR_TAB
                              TYPE SLIS_T_SPECIALCOL_ALV.
*----------------------------------------------------------------------*
* No  Colour
*  0  COL_BACKGROUND
*  1  COL_HEADING
*  2  COL_NORMAL
*  3  COL_TOTAL
*  4  COL_KEY
*  5  COL_POSITIVE
*  6  COL_NEGATIVE
*  7  COL_GROUP
*----------------------------------------------------------------------*
  DATA : L_COLOR TYPE SLIS_SPECIALCOL_ALV.
  L_COLOR-FIELDNAME = U_FIELD.
  L_COLOR-COLOR-COL = U_COL.
  L_COLOR-COLOR-INT = U_INT.
  APPEND L_COLOR TO COLOR_TAB.
ENDFORM.                    " set_cell_color
*---------------------------------------------------------------------*
*       FORM PROCESS_DATA                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: L_CN(2) TYPE N,
        L_OHQTY LIKE ZMMS_SHORT-RP01,
        L_QTY LIKE ZMMS_SHORT-RP01,
        L_DAY_QTY LIKE ZMMS_SHORT-RP01,
        L_TOTAL LIKE ZMMS_SHORT-RP01,
        L_DAYS LIKE ZMMS_SHORT-RP01,
        L_FLAG(1),
        L_TEXT(40),
        L_TEXT_SHORT(40),
        L_INDEX LIKE SY-TABIX,
        L_LGPRO LIKE MARC-LGPRO,
        L_GESME LIKE LQUA-GESME,
        L_LABST_P001 LIKE MARD-LABST,
        L_LABST_9999 LIKE MARD-LABST,
        L_SEQ(5) TYPE N,
        L_LGPRO_COGI LIKE MARC-LGPRO,
        L_WERKS_COGI LIKE MARC-WERKS,
        L_OHQTY_COGI LIKE ZMMS_SHORT-RP01,
        L_DAYS_COGI LIKE ZMMS_SHORT-RP01,
        L_FLAG_COGI(1).

**  DATA: LT_MDEZ LIKE TABLE OF MDEZ WITH HEADER LINE.


  DATA: BEGIN OF LT_MARD OCCURS 0,
        MATNR LIKE MARD-MATNR,
        WERKS LIKE MARD-WERKS,
        LGORT LIKE MARD-LGORT,
        LABST LIKE MARD-LABST,
        END OF LT_MARD.
  FIELD-SYMBOLS: <FS_SHORT>,
                 <FS_DATA>.

  REFRESH: IT_SHORT.
  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_SHORT.

*    SELECT SINGLE MATKL B~DISPO VSPVB MAKTX MTART LGPRO DSNAM INTO
*        (IT_SHORT-MATKL, IT_SHORT-DISPO, IT_SHORT-PRVBE,
*         IT_SHORT-MAKTX, IT_SHORT-MTART, L_LGPRO, IT_SHORT-DSNAM)
    SELECT SINGLE MATKL B~DISPO MAKTX MTART LGPRO DSNAM INTO
         (IT_SHORT-MATKL, IT_SHORT-DISPO,
          IT_SHORT-MAKTX, IT_SHORT-MTART, L_LGPRO, IT_SHORT-DSNAM)

       FROM MARA AS A
       INNER JOIN MARC AS B
       ON A~MATNR = B~MATNR
       INNER JOIN MAKT AS C
       ON A~MATNR = C~MATNR
       INNER JOIN T024D AS D
       ON B~DISPO = D~DISPO
       WHERE A~MATNR = IT_DATA-MATNR
       AND B~WERKS = 'P001'
       AND D~WERKS = 'P001'
       AND SPRAS = 'EN'.

    IF L_LGPRO IS INITIAL.
      L_LGPRO = 'P400'.
    ENDIF.

    IT_SHORT-LGPRO = L_LGPRO.

    READ TABLE IT_21DAY WITH KEY MATNR = IT_DATA-MATNR
                                 BINARY SEARCH.

    IT_SHORT-LIFNR = IT_21DAY-LIFNR.
    IT_SHORT-PRVBE = IT_21DAY-PRVBE.
    IT_SHORT-MEINS = IT_21DAY-MEINS.
*    SELECT SINGLE LIFNR INTO IT_SHORT-LIFNR
*      FROM EORD
*      WHERE MATNR = IT_DATA-MATNR
*        AND FLIFN = 'X'
*        AND VDATU <= SY-DATUM
*        AND BDATU >= SY-DATUM.

*    IT_SHORT-RP = IT_DATA-SORTF.
    IT_SHORT-RP = IT_21DAY-SORTF.

    IF P_ALL = 'X'.

** get stock

** Changed on 07/13/11

      SELECT SINGLE SUM( LABST ) INTO IT_SHORT-LN_OHQTY
          FROM MARD
          WHERE MATNR =  IT_SHORT-MATNR
             AND WERKS = 'P001'
             AND ( LGORT <> '9999' AND
                   LGORT <> 'P999' AND
                   LGORT <> 'P998' AND
                   LGORT <> 'G999' AND
                   LGORT <> 'G998' )
             GROUP BY MATNR WERKS.

** Furong on 06/08/12 for COGI stock
      CLEAR: L_LGPRO_COGI, L_WERKS_COGI.
      SELECT SINGLE LGPRO WERKS
       INTO (L_LGPRO_COGI, L_WERKS_COGI)
      FROM MARC
      WHERE MATNR =  IT_SHORT-MATNR
        AND VSPVB <> ' '.

      SELECT SINGLE LABST INTO IT_SHORT-OHQTY_COGI
                FROM MARD
                WHERE MATNR =  IT_SHORT-MATNR
                   AND WERKS = L_WERKS_COGI
                   AND LGORT = L_LGPRO_COGI.
*             GROUP BY matnr werks.

      L_OHQTY_COGI = IT_SHORT-OHQTY_COGI.
      CLEAR: L_FLAG_COGI, L_DAYS_COGI.

** End on06/08/12

*      SELECT SINGLE SUM( LABST ) INTO L_LABST_P001
*         FROM MARD
*         WHERE MATNR =  IT_SHORT-MATNR
*            AND WERKS = 'P001'
**       AND LGORT = 'P400'
*            GROUP BY MATNR WERKS.
*
*      SELECT SINGLE SUM( LABST ) INTO L_LABST_9999
*        FROM MARD
*        WHERE MATNR =  IT_SHORT-MATNR
*           AND WERKS = 'P001'
*           AND ( LGORT = '9999' OR LGORT = 'P999' )
*
*           GROUP BY MATNR WERKS.
*
*      IT_SHORT-LN_OHQTY = L_LABST_P001 - L_LABST_9999.

** End on 07/13/11

*      SELECT SINGLE SUM( LABST ) INTO IT_SHORT-LN_OHQTY
*          FROM MARD
*          WHERE MATNR =  IT_SHORT-MATNR
*            AND WERKS = 'P001'
*            AND LGORT = 'P400'
**            AND LGORT = IT_SHORT-LGPRO
**            AND LGORT <> '9999'
*          GROUP BY MATNR WERKS LGORT.
*
*      REFRESH LT_MARD.
*      SELECT MATNR WERKS LGORT LABST INTO TABLE LT_MARD
*        FROM MARD
*        WHERE MATNR =   IT_DATA-MATNR
*          AND WERKS = 'P001'
*          AND LGORT = L_LGPRO
*          AND LGORT <> '9999'.
*
*      LOOP AT LT_MARD.
**      IF LT_MARD-LGORT = L_LGPRO.
*        IT_SHORT-LN_OHQTY =  IT_SHORT-LN_OHQTY + LT_MARD-LABST.
**      ELSE.
**        IT_SHORT-WH_OHQTY =  IT_SHORT-WH_OHQTY + LT_MARD-LABST.
**      ENDIF.
*      ENDLOOP.
*    REFRESH LT_MDEZ.
*    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
*      EXPORTING
**   PLSCN                          =
*        MATNR                          = IT_DATA-MATNR
*        WERKS                          = 'P001'
**   BERID                          =
**   ERGBZ                          =
**   AFIBZ                          =
**   INPER                          =
**   DISPLAY_LIST_MDPSX             =
**   DISPLAY_LIST_MDEZX             =
**   DISPLAY_LIST_MDSUX             =
** IMPORTING
**   E_MT61D                        =
**   E_MDKP                         =
*     TABLES
**   MDPSX                          =
*       MDEZX                          = LT_MDEZ
**   MDSUX                          =
** EXCEPTIONS
**   MATERIAL_PLANT_NOT_FOUND       = 1
**   PLANT_NOT_FOUND                = 2
**   OTHERS                         = 3
*              .
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    READ TABLE LT_MDEZ WITH KEY DELB0 = 'Stock'.
*
*    IT_SHORT-LN_OHQTY = LT_MDEZ-MNG01.
*

*    L_OHQTY = IT_SHORT-LN_OHQTY + IT_SHORT-WH_OHQTY.
      L_DAY_QTY = IT_SHORT-LN_OHQTY.
      L_OHQTY = IT_SHORT-LN_OHQTY.

      CLEAR: L_GESME.


** changed on 07/14/11
*      SELECT SUM( GESME ) INTO L_GESME
*            FROM LQUA
*            WHERE MATNR = IT_DATA-MATNR
*              AND WERKS = 'P001'
*              AND LGTYP = '999'
*            GROUP BY MATNR WERKS LGTYP.
*      ENDSELECT.
** End of change
*    IF L_GESME > 0.
*      IT_SHORT-LN_OHQTY = IT_SHORT-LN_OHQTY - L_GESME.
      IT_SHORT-ADJ_QTY = L_GESME.
*      IF IT_SHORT-LN_OHQTY < 0 .
*        IT_SHORT-LN_OHQTY = 0.
*      ENDIF.
*    ENDIF.

      L_CN = '00'.
      CLEAR: L_FLAG, L_DAYS.
      DO 40 TIMES.
        L_CN = L_CN + 1.
        CONCATENATE 'IT_SHORT-RP' L_CN INTO L_TEXT_SHORT.
        ASSIGN (L_TEXT_SHORT) TO <FS_SHORT>.
        CONCATENATE 'IT_DATA-RP' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS_DATA>.

        <FS_SHORT> = <FS_DATA>.
        MOVE <FS_SHORT> TO L_QTY.
        L_TOTAL = L_TOTAL + L_QTY.

        L_OHQTY = L_OHQTY - L_QTY.


** Furong on 06/08/12 for COGI qty
        L_OHQTY_COGI = L_OHQTY_COGI - L_QTY.
** End

** Changed by Furong on 06/22/10
*        IF L_FLAG IS INITIAL AND L_DAY_QTY > 0.
*          IF L_OHQTY >= 0.
*            L_DAYS = L_DAYS + 1.
*          ELSEIF  L_QTY <> 0.
*            L_FLAG = 'X'.
*            L_OHQTY = L_OHQTY + L_QTY.
*            L_DAYS = L_DAYS + L_OHQTY / L_QTY.
*          ENDIF.
*        ENDIF.
        IF L_FLAG IS INITIAL.  " AND L_DAY_QTY > 0.
          IF L_OHQTY >= 0.
            L_DAYS = L_DAYS + 1.
          ELSEIF  L_QTY <> 0.
            L_FLAG = 'X'.
            L_OHQTY = L_OHQTY + L_QTY.
            L_DAYS = L_DAYS + L_OHQTY / L_QTY.
          ENDIF.
** Furong on 06/08/12 for COGI qty
          IF L_FLAG_COGI IS INITIAL.
            IF L_OHQTY_COGI >= 0.
              L_DAYS_COGI = L_DAYS_COGI + 1.
            ELSEIF  L_QTY <> 0.
              L_FLAG_COGI = 'X'.
              L_OHQTY_COGI = L_OHQTY_COGI + L_QTY.
              L_DAYS_COGI = L_DAYS_COGI + L_OHQTY_COGI / L_QTY.
            ENDIF.
          ENDIF.
** End on 06/08/12
        ENDIF.
** End of change

      ENDDO.
      IF L_DAYS < 0.
        CLEAR: L_DAYS.
      ENDIF.

      IT_SHORT-HR_OH = L_DAYS.

      IT_SHORT-TOTAL = L_TOTAL.

      IT_SHORT-ZSDAT = W_BASE_DATE.
      IT_SHORT-ZSTIM = W_BASE_TIME.
      IT_SHORT-ZEDAT = SY-DATUM.
      IT_SHORT-ZETIM = SY-UZEIT.

** Furong on 06/08/12 for COGI qty
      IF L_DAYS_COGI < 0.
        CLEAR: L_DAYS_COGI.
      ENDIF.

      IT_SHORT-HR_QTY_COGI = L_DAYS_COGI.
** End
      APPEND IT_SHORT.
    ELSE.
      L_CN = '00'.
      CLEAR: L_FLAG, L_DAYS.
      DO 40 TIMES.
        L_CN = L_CN + 1.
        CONCATENATE 'IT_SHORT-RP' L_CN INTO L_TEXT_SHORT.
        ASSIGN (L_TEXT_SHORT) TO <FS_SHORT>.
        CONCATENATE 'IT_DATA-RP' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS_DATA>.

        <FS_SHORT> = <FS_DATA>.
        MOVE <FS_SHORT> TO L_QTY.
        L_TOTAL = L_TOTAL + L_QTY.

      ENDDO.

      IT_SHORT-TOTAL = L_TOTAL.

      IT_SHORT-ZSDAT = W_BASE_DATE.
      IT_SHORT-ZSTIM = W_BASE_TIME.
      IT_SHORT-ZEDAT = SY-DATUM.
      IT_SHORT-ZETIM = SY-UZEIT.

      APPEND IT_SHORT.
    ENDIF.
    CLEAR: IT_SHORT, L_OHQTY, L_TOTAL,L_OHQTY_COGI,
           L_DAY_QTY, L_LABST_P001,
           L_LABST_9999.
  ENDLOOP.

*  SORT IT_SHORT BY HR_OH MATNR.
  SORT IT_SHORT BY HR_OH ASCENDING
                TOTAL DESCENDING
                RP01 DESCENDING
                MATNR.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  W_REPID = SY-REPID.
  W_DYNNR = SY-DYNNR.
ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  save_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TO_TABLE.
  DATA : L_SEQ(5) TYPE N.

  L_SEQ = '00001'.
  LOOP AT IT_SHORT.
    IT_SHORT-SEQ = L_SEQ.
    MODIFY IT_SHORT.
    L_SEQ = L_SEQ + 1.
  ENDLOOP.

  DELETE FROM ZTMM_HOUR_SHORT CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

*  IF SY-SUBRC = 0.
  INSERT ZTMM_HOUR_SHORT FROM TABLE IT_SHORT.

*  ELSE.
*    MESSAGE E000(ZZ) WITH 'Error: Z-Table deletion (ZTMM_HOUR_SHORT)'.
*  ENDIF.
ENDFORM.                    " save_to_table
*&---------------------------------------------------------------------*
*&      Form  RESEQ_INPUT_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RESEQ_INPUT_PLAN.
  DATA: L_DATE LIKE SY-DATUM,
        L_DATE_C(8),
        L_TIME_C(6),
        L_CUR_DATE LIKE SY-DATUM,
        L_KALID LIKE KAKO-KALID,
*        L_DAY LIKE KAPA-TAGNR,
*        L_DAY LIKE SCAL-INDICATOR,
        L_DAY TYPE I,
*        L_TIME TYPE KAPENDZT,
*        L_TOT_TIME TYPE KAPENDZT,
        L_TIME LIKE SY-UZEIT,
        L_TIME_BR LIKE SY-UZEIT,
*        L_TOT_TIME like sy-uzeit,
        L_SEC LIKE ZSMM_WORKING_TIME-OPSEC,
        L_TOT_SEC LIKE ZSMM_WORKING_TIME-OPSEC,
        L_HR LIKE IT_SHORT-RP01,
        L_SERIAL LIKE ZTPP_INPUTPLAN_H-SERIAL,
        L_INDEX LIKE SY-TABIX,
        L_INS_INDEX LIKE SY-TABIX,
        L_DEL_INDEX LIKE SY-TABIX,
        L_STATUS LIKE ZTPP_INPUTPLAN_H-STATUS,
        L_WOFRM LIKE ZSMM_WORKING_TIME-WOFRM.


  DATA: IT_WORKING_TIME LIKE TABLE OF ZSMM_WORKING_TIME
          WITH HEADER LINE.

  SELECT SINGLE FABKL INTO L_KALID
    FROM T001W
   WHERE WERKS = 'P001' .


  L_DATE_C = W_BASE_DATE.
  L_TIME_C = W_BASE_TIME.
  CONCATENATE L_DATE_C L_TIME_C INTO L_WOFRM.

  LOOP AT IT_HOLD.

    L_DATE = IT_HOLD-RSNUM - 1.
    PERFORM READ_WORKING_DATE USING '-'  L_KALID  L_DATE.
    IF L_DATE >= W_BASE_DATE.
      L_DAY = L_DATE - W_BASE_DATE.

      CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
        EXPORTING
          I_DATUM                    = W_BASE_DATE
          I_DAY                      = L_DAY
          I_ARBPL                    = 'T'
* IMPORTING
*   E_DATE_CURR                =
*   E_TPROG_CURR               =
*   E_DATE_NEXT                =
*   E_TPROG_NEXT               =
       TABLES
         T_WORKING_TIME             = IT_WORKING_TIME
*   T_1T                       =
* EXCEPTIONS
*   CANNOT_READ_DAYNAME        = 1
*   INCORRECT_SHIFT_INFO       = 2
*   INCORRECT_CAPA_INFO        = 3
*   OTHERS                     = 4
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      SORT IT_WORKING_TIME BY WOFRM.

      LOOP AT IT_WORKING_TIME.
        L_INDEX = SY-TABIX.
        IF  IT_WORKING_TIME-WOFRM <= L_WOFRM AND
            IT_WORKING_TIME-WOEND >= L_WOFRM.
          L_TIME = L_TIME_C = IT_WORKING_TIME-WOEND+8(6).
          IF IT_WORKING_TIME-BRSEC = 0.
            L_SEC = L_TIME - W_BASE_TIME.
          ELSE.
            IF L_WOFRM < IT_WORKING_TIME-BRFRM.
              L_SEC = L_TIME - W_BASE_TIME - IT_WORKING_TIME-BRSEC.
            ELSEIF L_WOFRM >= IT_WORKING_TIME-BRFRM AND
                   L_WOFRM <= IT_WORKING_TIME-BREND.
              L_TIME_BR = L_TIME_C = IT_WORKING_TIME-BREND+8(6).
              L_SEC = L_TIME - L_TIME_BR.
            ELSEIF L_WOFRM > IT_WORKING_TIME-BREND.
              L_SEC = L_TIME - W_BASE_TIME.
            ENDIF.
          ENDIF.
          EXIT.
        ENDIF.
      ENDLOOP.

      L_TOT_SEC = L_SEC.
      L_INDEX = L_INDEX + 1.
      LOOP AT IT_WORKING_TIME FROM L_INDEX.
        L_TOT_SEC = L_TOT_SEC + IT_WORKING_TIME-OPSEC.
      ENDLOOP.

      L_HR = L_TOT_SEC / 3600.
      IF L_TOT_SEC > 0 AND  L_HR <= 40.

        READ TABLE IT_INPUT_PLAN WITH KEY MODL = IT_HOLD-MODL
                                 BODY_SER = IT_HOLD-BODY_SER.
        L_INDEX = SY-TABIX.
        L_DEL_INDEX = L_INDEX + 1.
        L_SERIAL = IT_HOLD-SERIAL.
        LOOP AT IT_INPUT_PLAN FROM L_DEL_INDEX.
          IT_INPUT_PLAN-SERIAL = L_SERIAL.
          MODIFY IT_INPUT_PLAN.
          L_SERIAL = L_SERIAL + 1.
        ENDLOOP.

        READ TABLE IT_INPUT_PLAN WITH KEY STATUS = '05'.
        L_INDEX = SY-TABIX.
        L_INS_INDEX = L_INDEX + L_UPH * L_HR.

        READ TABLE IT_INPUT_PLAN INDEX L_INS_INDEX.

        L_SERIAL = IT_INPUT_PLAN-SERIAL.
        L_STATUS = IT_INPUT_PLAN-STATUS.

        IT_HOLD-SERIAL = L_SERIAL.
        MODIFY IT_INPUT_PLAN FROM IT_HOLD
                     TRANSPORTING SERIAL STATUS
                     WHERE MODL = IT_HOLD-MODL
                       AND BODY_SER = IT_HOLD-BODY_SER.

        L_SERIAL = L_SERIAL + 1.
        LOOP AT IT_INPUT_PLAN FROM L_INS_INDEX.
          IT_INPUT_PLAN-SERIAL = L_SERIAL.
          MODIFY IT_INPUT_PLAN.
          L_SERIAL = L_SERIAL + 1.
        ENDLOOP.
      ENDIF.
      CLEAR: L_INDEX,L_TOT_SEC, L_SEC, L_HR.
    ENDIF.
  ENDLOOP.

  SORT IT_INPUT_PLAN BY STATUS DESCENDING SERIAL.
ENDFORM.                    " RESEQ_INPUT_PLAN
*&---------------------------------------------------------------------*
*&      Module  SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TIMER OUTPUT.
  IF TIMER_CONTAINER IS INITIAL.
    CREATE OBJECT:

       TIMER_CONTAINER
             EXPORTING
                  CONTAINER_NAME = 'TI_CONTAINER',
       GUI_TIMER
             EXPORTING
                  PARENT = TIMER_CONTAINER.

    SET HANDLER EVENT_HANDLER->ON_FINISHED FOR GUI_TIMER.

    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.
  ENDIF.
ENDMODULE.                 " SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  checking_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM CHECKING_BATCH_JOB.
  DATA: L_BACKJOB LIKE  SY-REPID,
        LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.
  L_BACKJOB = SY-REPID.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      ABAP_PROGRAM_NAME             = L_BACKJOB
      DIALOG                        = 'N'
      STATUS                        = 'R'
    TABLES
      JOBLIST                       = LT_JOBLIST
    EXCEPTIONS
      NO_JOBS_FOUND                 = 1
      PROGRAM_SPECIFICATION_MISSING = 2
      INVALID_DIALOG_TYPE           = 3
      JOB_FIND_CANCELED             = 4
      OTHERS                        = 5.

  IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S999(PP) WITH TEXT-M01.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE LT_JOBLIST INDEX 1.
    IF SY-SUBRC EQ 0.
      MESSAGE E999(PP) WITH TEXT-M01.
    ENDIF.
  ENDIF.

ENDFORM.                    " checking_batch_job
*&---------------------------------------------------------------------*
*&      Form  PROCESS_STOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_STOCK.
  DATA: L_CN(2) TYPE N,
         L_OHQTY LIKE ZMMS_SHORT-RP01,
         L_QTY LIKE ZMMS_SHORT-RP01,
         L_DAY_QTY LIKE ZMMS_SHORT-RP01,
          L_DAYS LIKE ZMMS_SHORT-RP01,
         L_FLAG(1),
         L_TEXT(40),
         L_TEXT_SHORT(40),
         L_INDEX LIKE SY-TABIX,
         L_GESME LIKE LQUA-GESME,
             L_LABST_P001 LIKE MARD-LABST,
        L_LABST_9999 LIKE MARD-LABST.

  FIELD-SYMBOLS: <FS_SHORT>.


  SELECT * INTO TABLE IT_SHORT
    FROM ZTMM_HOUR_SHORT.
  READ TABLE IT_SHORT INDEX 1.

  W_BASE_TIME = IT_SHORT-ZSTIM.
  W_BASE_DATE = IT_SHORT-ZSDAT.

  LOOP AT IT_SHORT.
    CLEAR: L_LABST_P001, L_LABST_9999.

    L_INDEX = SY-TABIX.

*    SELECT SINGLE SUM( LABST ) INTO IT_SHORT-LN_OHQTY
*      FROM MARD
*      WHERE MATNR =  IT_SHORT-MATNR
*        AND WERKS = 'P001'
*        AND LGORT = IT_SHORT-LGPRO
*        AND LGORT <> '9999'
*      GROUP BY MATNR WERKS LGORT.


** Changed on 07/13/11
    SELECT SINGLE SUM( LABST ) INTO IT_SHORT-LN_OHQTY
        FROM MARD
        WHERE MATNR =  IT_SHORT-MATNR
           AND WERKS = 'P001'
           AND ( LGORT <> '9999' AND
                 LGORT <> 'P999' AND
                 LGORT <> 'P998' AND
                 LGORT <> 'G999' AND
                 LGORT <> 'G998' )
           GROUP BY MATNR WERKS.

*    SELECT SINGLE SUM( LABST ) INTO L_LABST_P001
*      FROM MARD
*      WHERE MATNR =  IT_SHORT-MATNR
*         AND WERKS = 'P001'
**       AND LGORT = 'P400'
*         GROUP BY MATNR WERKS.
*
*    SELECT SINGLE SUM( LABST ) INTO L_LABST_9999
*      FROM MARD
*      WHERE MATNR =  IT_SHORT-MATNR
*         AND WERKS = 'P001'
*         AND ( LGORT = '9999' OR LGORT = 'P999' )
*         GROUP BY MATNR WERKS.
*
*    IT_SHORT-LN_OHQTY = L_LABST_P001 - L_LABST_9999.
** End of cahnge on 07/14/2011

    L_DAY_QTY = IT_SHORT-LN_OHQTY.
    L_OHQTY = IT_SHORT-LN_OHQTY.

*    CLEAR: L_GESME.
*    SELECT SUM( GESME ) INTO L_GESME
*          FROM LQUA
*          WHERE MATNR = IT_SHORT-MATNR
*            AND WERKS = 'P001'
*            AND LGTYP = '999'
*          GROUP BY MATNR WERKS LGTYP.
*    ENDSELECT.
*    IT_SHORT-ADJ_QTY = L_GESME.

    L_CN = '00'.
    CLEAR: L_FLAG, L_DAYS.
    DO 40 TIMES.
      L_CN = L_CN + 1.
      CONCATENATE 'IT_SHORT-RP' L_CN INTO L_TEXT_SHORT.
      ASSIGN (L_TEXT_SHORT) TO <FS_SHORT>.
*        CONCATENATE 'IT_DATA-RP' L_CN INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS_DATA>.
*
*        <FS_SHORT> = <FS_DATA>.
      MOVE <FS_SHORT> TO L_QTY.

      L_OHQTY = L_OHQTY - L_QTY.

      IF L_FLAG IS INITIAL.  " AND L_DAY_QTY > 0.
        IF L_OHQTY >= 0.
          L_DAYS = L_DAYS + 1.
        ELSEIF  L_QTY <> 0.
          L_FLAG = 'X'.
          L_OHQTY = L_OHQTY + L_QTY.
          L_DAYS = L_DAYS + L_OHQTY / L_QTY.
        ENDIF.
      ENDIF.
    ENDDO.

    IF L_DAYS < 0.
      CLEAR: L_DAYS.
    ENDIF.

    IT_SHORT-HR_OH = L_DAYS.

*      IT_SHORT-ZSDAT = W_BASE_DATE.
*      IT_SHORT-ZSTIM = W_BASE_TIME.
*      IT_SHORT-ZEDAT = SY-DATUM.
*      IT_SHORT-ZETIM = SY-UZEIT.
*
    MODIFY IT_SHORT INDEX L_INDEX.

  ENDLOOP.
  SORT IT_SHORT BY HR_OH ASCENDING
                TOTAL DESCENDING
                RP01 DESCENDING
                MATNR.


ENDFORM.                    " PROCESS_STOCK
