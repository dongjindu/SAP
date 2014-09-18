************************************************************************
* Program Name      : ZRPM06_MTBT
* Author            : Myoungho Park
* Creation Date     : 2003.09.02.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is MTTR / MTBF Satus Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

************************************************************************
************************************************************************
*   Copy & Modify from Standard Program SAPRDEMOEXCELINTEGRATION2
*    (Demonstration for Excel 97 Integration (Optimized))
*   Reference : SAP Library - Desktop Office Integration (BC-CI)
*                           - Business Document Service (BC-SRV-BDS)
*
* Screen 0100 for This Report
* Screen 0101 for PMIS (SAPMZAPM10_INFO)
* Two Screen same logic exempt Status & Tile...
************************************************************************
REPORT  ZRPM06_MTBT  MESSAGE-ID DEMOOFFICEINTEGRATIO.

*----- Include Office Interface Ojbect....
INCLUDE ZRPMOFFICE_TOP.

**** Copy from SAPRDEMOEXCELINTEGRATION2
CONSTANTS: TRUE VALUE 1, FALSE VALUE 0.

TYPES: T_OI_RET_STRING TYPE SOI_RET_STRING.

DATA: CONTROL TYPE REF TO I_OI_CONTAINER_CONTROL.
DATA: CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA: DOCUMENT TYPE REF TO I_OI_DOCUMENT_PROXY.
DATA: LINK_SERVER TYPE REF TO I_OI_LINK_SERVER.
DATA: BDS_INSTANCE TYPE REF TO CL_BDS_DOCUMENT_SET.

DATA: RETCODE TYPE T_OI_RET_STRING,
      DOCUMENT_TYPE(80) VALUE SOI_DOCTYPE_EXCEL97_SHEET,
      DOCUMENT_FORMAT(80) TYPE C,
      DESCR TYPE DOCUMENT_DESCR.

DATA: DATA_TABLE TYPE SBDST_CONTENT,
      DATA_SIZE TYPE I, DOC_URL TYPE T_URL,
      HAS_CHANGED TYPE I,
      DOCUMENT_MIMETYPE TYPE BAPICOMPON-MIMETYPE.

DATA: FIRST_OPEN VALUE '1'.
DATA: OPEN_DOCUMENT(1).
**********************************************************************
**********************************************************************

*---Global Variables & Tables
FIELD-SYMBOLS: <MONTH>,
               <YEAR> .

TABLES: ZSPM_PARAM,  "//Parameters
        ZTPM_MTBT,  "//Monthly planed MTTR/MTBF
        ZTPM_BDNO,  "//Monthly  Actual No. of Breakdown
*        ZTPM_ANMT,  "//Annual average MTTR/MTBF
*        ZTPM_SHOP,
        ZTPM_OPTIME, "//Operation Time
        ZSPM_MTBT,
        T024I.         "//Shop(Maintenance planner groups)
*        T357.        "//Plant Section

*** Base Year list
DATA: BEGIN OF IT_YEAR OCCURS 0,
         YEAR LIKE ZTPM_OPTIME-AJAHR,
      END OF IT_YEAR.

*** Select Range...
DATA: BEGIN OF IT_RANGE OCCURS 0,
         ZMONTH LIKE ZSPM_PARAM-ZMONTH,
      END OF IT_RANGE.

*** MTTR / MTBF...
*DATA: BEGIN OF IT_TYPE OCCURS 0,
*       ZMTBT LIKE ZTPM_ANMT-ZMTBT,
*      END OF IT_TYPE.

*** Annual average MTTR / MTBF
DATA: IT_ANMT LIKE ZSPM_YEAR OCCURS 0 WITH HEADER LINE,
      IT_TEMP_ANMT LIKE ZSPM_YEAR OCCURS 0.

*** Monthly average MTTR / MTBF
DATA: IT_MTBT LIKE ZSPM_MONTH OCCURS 0 WITH HEADER LINE,
      IT_TEMP_MTBT LIKE ZSPM_MONTH OCCURS 0.

*** Monthly average breakdown rate
*DATA: IT_MTTR1 LIKE ZTPM_MTBT OCCURS 0 WITH HEADER LINE, "//Planed
*      IT_MTTR2 LIKE ZTPM_MTBT OCCURS 0 WITH HEADER LINE, "//Actual
*      IT_MTBF1 LIKE ZTPM_MTBT OCCURS 0 WITH HEADER LINE, "//Planed
*      IT_MTBF2 LIKE ZTPM_MTBT OCCURS 0 WITH HEADER LINE. "//Actual


*** Global variables
DATA: WA_AVRATE LIKE ZTPM_MTBT-AVRATE.
DATA: WA_BDNUM  TYPE I.                  "//No. of breakdown
DATA: WA_SHOP_NAME LIKE T024I-INNAM.
DATA: WA_COUNTS TYPE I.
DATA: WA_S_YEAR LIKE ZTPM_ANBD-AJAHR,
      WA_E_YEAR LIKE ZTPM_ANBD-AJAHR,
      WA_SHTXT LIKE  ZSPM_PARAM-SHTXT,
      WA_INIT.

*---------------------------------------------------------------------*
*       CLASS c_event_handler DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS C_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: CLOSE_EVENT_HANDLER
              FOR EVENT ON_CLOSE_DOCUMENT OF I_OI_DOCUMENT_PROXY
              IMPORTING DOCUMENT_PROXY HAS_CHANGED.

    CLASS-METHODS: CUSTOM_EVENT_HANDLER
              FOR EVENT ON_CUSTOM_EVENT OF I_OI_DOCUMENT_PROXY
              IMPORTING DOCUMENT_PROXY EVENT_NAME PARAM_COUNT
                        PARAM1 PARAM2 PARAM3.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS c_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS C_EVENT_HANDLER IMPLEMENTATION.
  METHOD CLOSE_EVENT_HANDLER.

    PERFORM SAVE_DOCUMENT TABLES DATA_TABLE
                          USING 'X' 'X'
                          CHANGING DATA_SIZE DOCUMENT_PROXY RETCODE.
    OPEN_DOCUMENT = FALSE.

  ENDMETHOD.

  METHOD CUSTOM_EVENT_HANDLER.

  ENDMETHOD.
ENDCLASS.

*********** SELECTION-SCREEN ***********************************
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
*PARAMETER : P_MONTH LIKE ZSPM_PARAM-ZMONTH DEFAULT SY-DATUM(6)
*                                           OBLIGATORY.
SELECTION-SCREEN COMMENT (5) TEXT-006.
PARAMETER : P_MONTH LIKE ZSPM_PARAM-MONTH DEFAULT SY-DATUM+4(2)
                                          OBLIGATORY.

SELECTION-SCREEN POSITION 30.
SELECTION-SCREEN COMMENT 26(5) TEXT-007.
PARAMETER : P_YEAR LIKE ZSPM_PARAM-AJAHR DEFAULT SY-DATUM(4)
                                         OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS : S_SHOP  FOR ZSPM_PARAM-SHOP  NO INTERVALS
                                              NO-EXTENSION
                                              OBLIGATORY.

*PARAMETER : P_PLACE LIKE ZSPM_PARAM-INPLACE DEFAULT 'X'
*                                            AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK BLOCK1.


******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.


***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'ONLI'.
      CLEAR: SY-UCOMM.
      CALL SCREEN '0100'.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MONTH .
*  PERFORM SELECT_MONTH.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_YEAR.
  PERFORM SELECT_YEAR.

****************START-OF-SELECTION ************************************
***********************************************************************
START-OF-SELECTION.


END-OF-SELECTION.
****************** END-OF-SELECTION **********************************
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA:  WA_YEAR(2)   TYPE N,      "LIKE ZTPM_ANBD-AJAHR,
         WA_DATE      LIKE SY-DATUM,
         CAL_DATE     LIKE SY-DATUM,
         WA_MONTH(2)  TYPE N,
         WA_COUNT     LIKE T5A4A-DLYMO.

**** Read Header info...
  PERFORM READ_HEADER_DATA.

*** Clear all internal tables...
  CLEAR: IT_ANMT, IT_ANMT[],
         IT_MTBT, IT_MTBT[],
         IT_YEAR, IT_YEAR[],
         IT_RANGE, IT_RANGE[].



**** Make IT_MTBT....
*  IT_TYPE-ZMTBT = 'MTTR'.
*  APPEND IT_MTBT.
*  IT_TYPE-ZMTBT = 'MTBF'.
*  APPEND IT_TYPE.

**** Make year range ( IT_YEAR )....
  IT_YEAR-YEAR = P_YEAR.
  WA_E_YEAR = IT_YEAR-YEAR.
  APPEND IT_YEAR.
  IT_YEAR-YEAR = P_YEAR - 1.
  APPEND IT_YEAR.
  IT_YEAR-YEAR = P_YEAR - 2.
  APPEND IT_YEAR.
  IT_YEAR-YEAR = P_YEAR - 3.
  APPEND IT_YEAR.
  WA_S_YEAR = IT_YEAR-YEAR.

  SORT  IT_YEAR BY YEAR. "DESCENDING.

*** make first day for select condition
  CONCATENATE  P_YEAR P_MONTH  '01' INTO WA_DATE.

*** make select entry (year/month)
  DO 12 TIMES.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              DATE      = WA_DATE
              DAYS      = 0
              MONTHS    = WA_COUNT
              SIGNUM    = '-'
              YEARS     = 0
         IMPORTING
              CALC_DATE = CAL_DATE.
    MOVE : CAL_DATE(6) TO IT_RANGE-ZMONTH.
    APPEND IT_RANGE.
    MOVE SY-INDEX TO WA_COUNT.
  ENDDO.

  SORT IT_RANGE BY ZMONTH ASCENDING.

*** Get Annual  MTTR / MTBF ...
*  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_ANMT
*           FROM  ZTPM_ANMT
*           WHERE SHOP  EQ P_SHOP
*           AND   AJAHR BETWEEN WA_S_YEAR AND WA_E_YEAR.
*  IF SY-SUBRC EQ 0.
*
*    LOOP AT IT_MTBT WHERE ZMTBT = IT_ZTPM_ANMT-ZMTBT.
*      LOOP AT IT_YEAR.
*        WA_YEAR = SY-TABIX.
*        READ TABLE IT_ZTPM_ANMT WITH KEY AJAHR = IT_YEAR-AJAHR.
*        IF SY-SUBRC EQ 0.
*          PERFORM SET_IT_ZTPM_ANMT  USING WA_YEAR
*                                          IT_ZTPM_ANMT-AVRATE.
*        ENDIF.
*      ENDLOOP.
*      MOVE: IT_MTBT-ZMTBT TO IT_ANMT-ZMTBT.
*      APPEND IT_ANMT.
*      CLEAR: WA_YEAR, IT_ANMT.
*    ENDLOOP.
*  ELSE.
*
*  ENDIF.

  DATA: IT_TEMP LIKE ZTPM_MTBT OCCURS 0 WITH HEADER LINE.

**** Get Annual Planed MTTR data ...
  CLEAR : IT_ANMT.
  LOOP AT IT_YEAR.
    WA_YEAR = SY-TABIX.
    CLEAR : WA_AVRATE, WA_COUNTS.


    IF IT_YEAR-YEAR = SY-DATUM(4).
      SELECT SUM( AVRATE ) COUNT( * ) INTO (WA_AVRATE, WA_COUNTS)
            FROM ZTPM_MTBT
            WHERE SHOP   = S_SHOP-LOW
            AND   AJAHR  = IT_YEAR-YEAR
            AND   ZMONTH =< P_MONTH
            AND   ZMTBT  = 'MTTR'.
    ELSE.
      SELECT SUM( AVRATE ) COUNT( * ) INTO (WA_AVRATE, WA_COUNTS)
            FROM ZTPM_MTBT
            WHERE SHOP   = S_SHOP-LOW
            AND   AJAHR  = IT_YEAR-YEAR
            AND   ZMTBT  = 'MTTR'.
    ENDIF.

    IF  WA_COUNT NE 0.
      WA_AVRATE = WA_AVRATE / WA_COUNTS.
    ENDIF.

    PERFORM SET_IT_ANMT USING WA_YEAR
                              WA_AVRATE.
  ENDLOOP.
  APPEND IT_ANMT.

**** Calculate Annual MTTR data ...
  CLEAR : IT_ANMT.
  LOOP AT IT_YEAR.
    WA_YEAR = SY-TABIX.
    CLEAR : WA_AVRATE.
    PERFORM CAL_MTTR_DATA_YEAR USING IT_YEAR-YEAR
                                     WA_AVRATE.

    PERFORM SET_IT_ANMT USING WA_YEAR
                              WA_AVRATE.

  ENDLOOP.
  APPEND IT_ANMT.

**** Get Annual Planed MTBF data ...
  CLEAR : IT_ANMT.
  LOOP AT IT_YEAR.
    WA_MONTH = SY-TABIX.
    CLEAR : WA_AVRATE, WA_COUNTS.

    IF IT_YEAR-YEAR = SY-DATUM(4).
      SELECT SUM( AVRATE ) COUNT( * ) INTO (WA_AVRATE, WA_COUNTS)
            FROM ZTPM_MTBT
            WHERE SHOP   = S_SHOP-LOW
            AND   AJAHR  = IT_YEAR-YEAR
            AND   ZMONTH =< P_MONTH
            AND   ZMTBT  = 'MTBF'.
    ELSE.
      SELECT SUM( AVRATE ) COUNT( * ) INTO (WA_AVRATE, WA_COUNTS)
             FROM ZTPM_MTBT
             WHERE SHOP   = S_SHOP-LOW
             AND   AJAHR  = IT_YEAR-YEAR(4)
             AND   ZMTBT  = 'MTBF'.
    ENDIF.

    IF  WA_COUNT NE 0.
      WA_AVRATE = WA_AVRATE / WA_COUNTS.
    ENDIF.
    PERFORM SET_IT_ANMT USING WA_YEAR
                              WA_AVRATE.
  ENDLOOP.
  APPEND IT_ANMT.

**** Calculate Annual MTBF data ...
  CLEAR : IT_ANMT.
  LOOP AT IT_YEAR.
    WA_YEAR = SY-TABIX.
    CLEAR : WA_AVRATE.
    PERFORM CAL_MTBF_DATA_YEAR USING IT_YEAR-YEAR
                                     WA_AVRATE.

    PERFORM SET_IT_ANMT USING WA_YEAR
                              WA_AVRATE.
  ENDLOOP.
  APPEND IT_ANMT.

**** Get Monthly Planed MTTR data ...
  CLEAR : IT_MTBT.
  LOOP AT IT_RANGE.
    WA_MONTH = SY-TABIX.
    CLEAR : WA_AVRATE.
    SELECT SINGLE AVRATE  INTO  WA_AVRATE
           FROM ZTPM_MTBT
           WHERE SHOP   = S_SHOP-LOW
           AND   AJAHR  = IT_RANGE-ZMONTH(4)
           AND   ZMONTH = IT_RANGE-ZMONTH+4(2)
           AND   ZMTBT  = 'MTTR'.

    PERFORM SET_IT_MTBT USING WA_MONTH
                              WA_AVRATE.

  ENDLOOP.
  APPEND IT_MTBT.

**** Calculate Monthly MTTR data ...
*  CALL FUNCTION 'Z_FPM_MTTR'
*       EXPORTING
*            E_AJAHR = WA_AJAHR
*            E_SHOP  = WA_SHOP
*       TABLES
*            IT_RATE = IT_MTTR2.

  CLEAR : IT_MTBT.
  LOOP AT IT_RANGE.
    WA_MONTH = SY-TABIX.
    CLEAR : WA_AVRATE.
    IF WA_MONTH = 11 OR WA_MONTH = 12.
      PERFORM CAL_MTTR_DATA2 USING IT_RANGE-ZMONTH
                                    WA_AVRATE.
    ELSE.
      PERFORM CAL_MTTR_DATA USING IT_RANGE-ZMONTH
                                  WA_AVRATE.
    ENDIF.
    PERFORM SET_IT_MTBT USING WA_MONTH
                              WA_AVRATE.

  ENDLOOP.
  APPEND IT_MTBT.

**** Get Monthly Planed MTBF data ...
  CLEAR : IT_MTBT.
  LOOP AT IT_RANGE.
    WA_MONTH = SY-TABIX.
    CLEAR : WA_AVRATE.
    SELECT SINGLE AVRATE  INTO  WA_AVRATE
           FROM ZTPM_MTBT
           WHERE SHOP   = S_SHOP-LOW
           AND   AJAHR  = IT_RANGE-ZMONTH(4)
           AND   ZMONTH = IT_RANGE-ZMONTH+4(2)
           AND   ZMTBT  = 'MTBF'.

    PERFORM SET_IT_MTBT USING WA_MONTH
                              WA_AVRATE.
  ENDLOOP.
  APPEND IT_MTBT.

**** Calculate Monthly MTBF data ...
*  CALL FUNCTION 'Z_FPM_MTBF'
*       EXPORTING
*            E_AJAHR = WA_AJAHR
*            E_SHOP  = WA_SHOP
*       TABLES
*            T_RATE  = IT_MTBF2.
  CLEAR : IT_MTBT.
  LOOP AT IT_RANGE.
    WA_MONTH = SY-TABIX.
    CLEAR : WA_AVRATE.
    IF WA_MONTH = 11 OR WA_MONTH = 12.
      PERFORM CAL_MTBF_DATA2 USING IT_RANGE-ZMONTH
                                    WA_AVRATE.
    ELSE.
      PERFORM CAL_MTBF_DATA USING IT_RANGE-ZMONTH
                                  WA_AVRATE.
    ENDIF.
    PERFORM SET_IT_MTBT USING WA_MONTH
                              WA_AVRATE.
  ENDLOOP.
  APPEND IT_MTBT.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
*** set Title & Status
  IF SY-DYNNR = '0100'.
    SET PF-STATUS '0100'.
    SET TITLEBAR '0100'.
  ELSE.
*** initial value...
    IF WA_INIT = ' '.
*      P_PLACE = 'X'.
      WA_INIT = 'X'.
      P_YEAR = SY-DATUM(4).
      P_MONTH = SY-DATUM+4(2).
    ENDIF.
  ENDIF.

  RETCODE = C_OI_ERRORS=>RET_OK.
*** create BusinessDocument object
  IF BDS_INSTANCE IS INITIAL.
    CREATE OBJECT BDS_INSTANCE.
  ENDIF.

  IF CONTROL IS INITIAL.

    DATA: B_HAS_ACTIVEX.
*** Test Whether ActiveX Controls are Supported
    CALL FUNCTION 'GUI_HAS_ACTIVEX'
         IMPORTING
              RETURN = B_HAS_ACTIVEX.
    IF B_HAS_ACTIVEX IS INITIAL. MESSAGE E007. ENDIF.
**** Create Container Control Factory
    CALL METHOD C_OI_CONTAINER_CONTROL_CREATOR=>GET_CONTAINER_CONTROL
                      IMPORTING CONTROL = CONTROL
                                RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
**** Create Container Control 'CONTAINER_03'
    CREATE OBJECT CONTAINER
              EXPORTING CONTAINER_NAME = 'CONTAINER_03'.
**** Creates and Initializes the Control
    CALL METHOD CONTROL->INIT_CONTROL
                        EXPORTING R3_APPLICATION_NAME =
                                  'PM Information System'   "#EC NOTEXT
                                  INPLACE_ENABLED = 'X'
                                  INPLACE_SCROLL_DOCUMENTS = 'X'
                                  PARENT = CONTAINER
                                  REGISTER_ON_CLOSE_EVENT = 'X'
                                  REGISTER_ON_CUSTOM_EVENT = 'X'
                                  NO_FLUSH = 'X'
                        IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
**** Creates an Instance for the Link Server
    CALL METHOD CONTROL->GET_LINK_SERVER
                       IMPORTING LINK_SERVER = LINK_SERVER
                                 RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
***** Activates the Link Server
****  Link Server name SUFFIX : 'MTBT'
    CALL METHOD LINK_SERVER->START_LINK_SERVER
                      EXPORTING LINK_SERVER_MODE =
                                LINK_SERVER->LINK_SERVER_CUSTOMNAME
                                SERVER_NAME_SUFFIX = 'MTBT'
                      IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
*****Creates an Instance for Document Management
    CALL METHOD CONTROL->GET_DOCUMENT_PROXY
                      EXPORTING DOCUMENT_TYPE = DOCUMENT_TYPE
                      IMPORTING DOCUMENT_PROXY = DOCUMENT
                                RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    SET HANDLER C_EVENT_HANDLER=>CLOSE_EVENT_HANDLER FOR DOCUMENT.
    SET HANDLER C_EVENT_HANDLER=>CUSTOM_EVENT_HANDLER FOR DOCUMENT.

    IF SY-DYNNR EQ '0100'.
*** Refresh Links ...
      PERFORM REFRESH_LINKS.
*** Open document...
      PERFORM OPEN_DOCUMENT.
    ENDIF.
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  REFRESH_LINKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_LINKS.

  PERFORM READ_DATA.

  DATA: WA_FIELD_NAME(30),
        WA_YEAR(4),
        WA_MONTH(10), "TYPE D,
        WA_COUNT(2) TYPE N.

  DATA: ERROR TYPE REF TO I_OI_ERROR.

  IF NOT LINK_SERVER IS INITIAL.
*** Link Year
    LOOP AT  IT_YEAR.
      MOVE SY-TABIX TO WA_COUNT.

      CONCATENATE 'Year' WA_COUNT INTO WA_FIELD_NAME.
      MOVE  IT_YEAR-YEAR TO WA_YEAR.

      CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME   = WA_FIELD_NAME
                        ITEM_VALUE  = WA_YEAR
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

      CLEAR : WA_FIELD_NAME, WA_YEAR.
    ENDLOOP.

*** Link Month
    LOOP AT  IT_RANGE.
      MOVE SY-TABIX TO WA_COUNT.

      CONCATENATE 'Month' WA_COUNT INTO WA_FIELD_NAME.
*      MOVE  IT_RANGE-ZMONTH TO WA_MONTH.

      CONCATENATE IT_RANGE-ZMONTH+4(2) '/'
                  IT_RANGE-ZMONTH(4) INTO WA_MONTH.

      CALL METHOD LINK_SERVER->ADD_STRING_ITEM
              EXPORTING ITEM_NAME   = WA_FIELD_NAME
                        ITEM_VALUE  = WA_MONTH
                        NO_FLUSH    = 'X'
              IMPORTING RETCODE = RETCODE.

      CLEAR : WA_FIELD_NAME, WA_MONTH.
    ENDLOOP.

*** Link Annual MTTR/MTBF
    IT_TEMP_ANMT[] = IT_ANMT[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Annual'
                       ITEM_TITLE  = 'Annual MTTR/MTBF'
                       DDIC_NAME   = 'ZSPM_YEAR'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_ANMT.

**** Monthly MTTR/MTBF
    IT_TEMP_MTBT[] = IT_MTBT[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Monthly'
                       ITEM_TITLE  = 'Monthly MTTR/MTBF'
                       DDIC_NAME   = 'ZSPM_MONTH'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_MTBT.

**** Report date
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
             EXPORTING ITEM_NAME   = 'report_date'
                       ITEM_VALUE  = SY-DATUM
                       NO_FLUSH    = 'X'
             IMPORTING ERROR = ERROR.

**** Shop
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
             EXPORTING ITEM_NAME   = 'Shop'
                       ITEM_VALUE  = WA_SHOP_NAME
                       NO_FLUSH    = ' '
             IMPORTING ERROR = ERROR.

*    READ TABLE IT_YEAR INDEX 1.
*    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*                EXPORTING ITEM_NAME = 'Year_01'
*                          ITEM_VALUE  = WA_YEAR
*                          NO_FLUSH    = 'X'
*                IMPORTING RETCODE = RETCODE.
*
*    READ TABLE IT_YEAR INDEX 2.
*    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*                EXPORTING ITEM_NAME = 'Year_02'
*                          ITEM_VALUE  = WA_YEAR
*                          NO_FLUSH    = 'X'
*                IMPORTING RETCODE = RETCODE.
*
*    READ TABLE IT_YEAR INDEX 3.
*    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*                EXPORTING ITEM_NAME = 'Year_03'
*                          ITEM_VALUE  = WA_YEAR
*                          NO_FLUSH    = 'X'
*                IMPORTING RETCODE = RETCODE.
*
*    READ TABLE IT_YEAR INDEX 4.
*    MOVE: IT_YEAR-AJAHR TO WA_YEAR.
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*                EXPORTING ITEM_NAME = 'Year_04'
*                          ITEM_VALUE  = WA_YEAR
*                          NO_FLUSH    = 'X'
*               IMPORTING RETCODE = RETCODE.
*
*
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*      EXPORTING ITEM_NAME = 'Short Text'                    "#EC NOTEXT
*                ITEM_VALUE  = WA_SHORT_TEXT
*                NO_FLUSH    = 'X'
*      IMPORTING RETCODE = RETCODE.
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*             EXPORTING ITEM_NAME = 'Module'                 "#EC NOTEXT
*                       ITEM_VALUE  = WA_MODULE
*                       NO_FLUSH    = 'X'
*             IMPORTING RETCODE = RETCODE.
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*             EXPORTING ITEM_NAME = 'Program Name'           "#EC NOTEXT
*                       ITEM_VALUE  = WA_PROGRAM_NAME
*                       NO_FLUSH    = 'X'
*             IMPORTING RETCODE = RETCODE.
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*             EXPORTING ITEM_NAME = 'T-Code'                 "#EC NOTEXT
*                       ITEM_VALUE  = WA_T_CODE
*                       NO_FLUSH    = 'X'
*             IMPORTING RETCODE = RETCODE.
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*             EXPORTING ITEM_NAME = 'Functional Team Member'
*                       ITEM_VALUE  = WA_FUNCTIONAL_TEAM
*                       NO_FLUSH    = 'X'
*             IMPORTING RETCODE = RETCODE.
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*             EXPORTING ITEM_NAME = 'Development Team Lead'
*                       ITEM_VALUE  = WA_TEAM_LEAD
*                       NO_FLUSH    = 'X'
*             IMPORTING RETCODE = RETCODE.
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*            EXPORTING ITEM_NAME = 'Development Team Contact'
*                      ITEM_VALUE  = WA_DEVELOPMENT_TEAM
*                       NO_FLUSH    = 'X'
*            IMPORTING RETCODE = RETCODE.
*
*    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
*            EXPORTING ITEM_NAME = 'Review Date'
*                      ITEM_VALUE  = WA_REVIEW_DATE
*                      NO_FLUSH    = ' '
*            IMPORTING RETCODE = RETCODE.

  ENDIF.
ENDFORM.                    " REFRESH_LINKS
*&---------------------------------------------------------------------*
*&      Form  OPEN_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_DOCUMENT.

  IF NOT DOCUMENT IS INITIAL.
    PERFORM SAVE_DOCUMENT TABLES DATA_TABLE
                          USING 'X' 'X'
                          CHANGING DATA_SIZE DOCUMENT RETCODE.
  ENDIF.

  IF NOT CONTROL IS INITIAL.

    PERFORM LOAD_DOCUMENT CHANGING
                                  DOCUMENT_TYPE
                                  DOCUMENT_FORMAT
                                  DOC_URL.

    IF NOT DOC_URL IS INITIAL.
      CALL METHOD DOCUMENT->OPEN_DOCUMENT
                             EXPORTING DOCUMENT_URL = DOC_URL
                                       OPEN_INPLACE = 'X' "//P_PLACE
                             IMPORTING RETCODE = RETCODE.
      CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
* Document shall also be available in ITAB for respective operations:
      CALL METHOD DOCUMENT->SAVE_DOCUMENT_TO_TABLE
                        IMPORTING RETCODE = RETCODE
                        CHANGING  DOCUMENT_TABLE = DATA_TABLE
                                  DOCUMENT_SIZE = DATA_SIZE.
      CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

      FIRST_OPEN = FALSE.
      OPEN_DOCUMENT = TRUE.

    ELSE.
      MESSAGE E010.
    ENDIF.
  ENDIF.

ENDFORM.                    " OPEN_DOCUMENT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CALL METHOD CL_GUI_CFW=>DISPATCH.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR: SY-UCOMM.
      PERFORM FREE_OBJECT.
      LEAVE TO TRANSACTION SY-TCODE.

    WHEN 'EXCEL'.
      CLEAR: SY-UCOMM.
      IF S_SHOP-LOW IS INITIAL.
        MESSAGE E000(ZDPM) WITH TEXT-008.
      ENDIF.

      PERFORM REFRESH_LINKS.
      PERFORM OPEN_DOCUMENT.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  LOAD_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_DOCUMENT_TYPE  text
*      <--P_DOCUMENT_FORMAT  text
*      <--P_DOC_URL  text
*----------------------------------------------------------------------*
FORM LOAD_DOCUMENT CHANGING DOCUMENT_TYPE   TYPE C
                            DOCUMENT_FORMAT TYPE C
                            DOC_URL         TYPE T_URL.

* Tables and WAs:
  DATA: DOC_SIGNATURE TYPE SBDST_SIGNATURE,
        WA_DOC_SIGNATURE LIKE LINE OF DOC_SIGNATURE,
        DOC_COMPONENTS TYPE SBDST_COMPONENTS,
        WA_DOC_COMPONENTS LIKE LINE OF DOC_COMPONENTS,
        DOC_PROPERTIES TYPE SBDST_PROPERTIES,
        WA_DOC_PROPERTIES LIKE LINE OF DOC_PROPERTIES,
        DOC_URIS TYPE SBDST_URI,
        WA_DOC_URIS LIKE LINE OF DOC_URIS.
* IDs:
  DATA: DOC_CLASSNAME TYPE SBDST_CLASSNAME VALUE 'PMDOCUMENT',
        DOC_CLASSTYPE TYPE SBDST_CLASSTYPE VALUE 'OT',
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'MTBT',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.


  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'MTTR-MTBF Status'.
  "'Technical Spec Template(Report).doc'.

  "//DESCR-DOCUMENT_ID.
  APPEND WA_DOC_SIGNATURE TO DOC_SIGNATURE.

  CALL METHOD BDS_INSTANCE->GET_INFO
                EXPORTING CLASSNAME = DOC_CLASSNAME
                          CLASSTYPE = DOC_CLASSTYPE
                          OBJECT_KEY = DOC_OBJECT_KEY
                CHANGING COMPONENTS = DOC_COMPONENTS
                         SIGNATURE = DOC_SIGNATURE
                  EXCEPTIONS NOTHING_FOUND = 1
                             ERROR_KPRO = 2
                             INTERNAL_ERROR = 3
                             PARAMETER_ERROR = 4
                             NOT_AUTHORIZED = 5
                             NOT_ALLOWED = 6.
  IF SY-SUBRC NE 0 AND SY-SUBRC NE 1.
    MESSAGE E016.
  ENDIF.
  IF SY-SUBRC = 1.
    MESSAGE E017.
  ENDIF.

  CALL METHOD BDS_INSTANCE->GET_WITH_URL
                     EXPORTING CLASSNAME = DOC_CLASSNAME
                               CLASSTYPE = DOC_CLASSTYPE
                               OBJECT_KEY = DOC_OBJECT_KEY
                     CHANGING URIS = DOC_URIS
                              SIGNATURE = DOC_SIGNATURE
                  EXCEPTIONS NOTHING_FOUND = 1
                             ERROR_KPRO = 2
                             INTERNAL_ERROR = 3
                             PARAMETER_ERROR = 4
                             NOT_AUTHORIZED = 5
                             NOT_ALLOWED = 6.
  IF SY-SUBRC NE 0 AND SY-SUBRC NE 1.
    MESSAGE E016.
  ENDIF.
  IF SY-SUBRC = 1.
    MESSAGE E017.
  ENDIF.

  READ TABLE DOC_COMPONENTS INTO WA_DOC_COMPONENTS INDEX 1.
  READ TABLE DOC_URIS INTO WA_DOC_URIS INDEX 1.
  DOC_MIMETYPE = WA_DOC_COMPONENTS-MIMETYPE.
  DOCUMENT_MIMETYPE = DOC_MIMETYPE.
  DOC_URL = WA_DOC_URIS-URI.

  CASE DOC_MIMETYPE.
    WHEN 'application/x-rtf' OR 'text/rtf'.
      DOCUMENT_FORMAT = SOI_DOCFORMAT_RTF.
    WHEN 'application/x-oleobject'.
      DOCUMENT_FORMAT = SOI_DOCFORMAT_COMPOUND.
    WHEN 'text/plain'.
      DOCUMENT_FORMAT = SOI_DOCFORMAT_TEXT.
    WHEN OTHERS.
      DOCUMENT_FORMAT = SOI_DOCFORMAT_NATIVE.
  ENDCASE.


ENDFORM.                    " LOAD_DOCUMENT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN '%EX'.
      CLEAR: SY-UCOMM.
      PERFORM FREE_OBJECT.
      LEAVE TO TRANSACTION SY-TCODE.

    WHEN 'RW'.
      CLEAR: SY-UCOMM.
      PERFORM FREE_OBJECT.
      LEAVE TO TRANSACTION SY-TCODE.

  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FREE_OBJECT.
  DATA: IS_CLOSED TYPE I.

  IF NOT DOCUMENT IS INITIAL.
    PERFORM SAVE_DOCUMENT TABLES DATA_TABLE
                          USING 'X' 'X'
                          CHANGING DATA_SIZE DOCUMENT RETCODE.
    FREE DOCUMENT.
  ENDIF.

  IF NOT LINK_SERVER IS INITIAL.
    CALL METHOD LINK_SERVER->STOP_LINK_SERVER
                                   IMPORTING RETCODE = RETCODE.
    FREE LINK_SERVER.
  ENDIF.

  IF NOT CONTROL IS INITIAL.
    CALL METHOD CONTROL->DESTROY_CONTROL.
    FREE CONTROL.
  ENDIF.

  IF NOT BDS_INSTANCE IS INITIAL.
    FREE BDS_INSTANCE.
  ENDIF.

ENDFORM.                    " FREE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM SAVE_DOCUMENT TABLES   DOC_TABLE TYPE TABLE
                   USING    DO_ASK TYPE C DO_RELEASE TYPE C
                   CHANGING DOC_SIZE TYPE I
                            DOCUMENT TYPE REF TO I_OI_DOCUMENT_PROXY
                            RETCODE TYPE T_OI_RET_STRING.

  DATA: IS_CLOSED TYPE I, ANSWER TYPE C, HAS_CHANGED TYPE I.

  CALL METHOD DOCUMENT->IS_DESTROYED IMPORTING RET_VALUE = IS_CLOSED.

  IF IS_CLOSED IS INITIAL.
    CALL METHOD DOCUMENT->CLOSE_DOCUMENT
                   EXPORTING DO_SAVE = 'X'
                   IMPORTING HAS_CHANGED = HAS_CHANGED
                             RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
  ENDIF.

  IF NOT DO_RELEASE IS INITIAL.
    CALL METHOD DOCUMENT->RELEASE_DOCUMENT
                                 IMPORTING RETCODE = RETCODE.
  ENDIF.

ENDFORM.                    " SAVE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  SELECT_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_MONTH.
  DATA SPMON LIKE S051-SPMON.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      ACTUAL_MONTH                     = SY-DATUM(6)
*   FACTORY_CALENDAR                 = ' '
*   HOLIDAY_CALENDAR                 = ' '
     LANGUAGE                         = SY-LANGU
     START_COLUMN                     = 8
     START_ROW                        = 5
   IMPORTING
     SELECTED_MONTH                   = SPMON
*   RETURN_CODE                      =
   EXCEPTIONS
     FACTORY_CALENDAR_NOT_FOUND       = 1
     HOLIDAY_CALENDAR_NOT_FOUND       = 2
     MONTH_NOT_FOUND                  = 3
     OTHERS                           = 4
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF NOT SPMON IS INITIAL.
    P_MONTH = SPMON.
  ENDIF.

ENDFORM.                    " SELECT_MONTH
*&---------------------------------------------------------------------*
*&      Module  SELECT_MONTH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_MONTH INPUT.
  PERFORM SELECT_MONTH.
ENDMODULE.                 " SELECT_MONTH  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_IT_ANMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_YEAR  text
*      -->P_AVRATE  text
*----------------------------------------------------------------------*
FORM SET_IT_ANMT USING    P_YEAR
                          P_AVRATE.

  DATA: WA_YEAR LIKE FELD-NAME.

*  P_AVRATE = P_AVRATE * 100.

  CONCATENATE 'IT_ANMT-YEAR' P_YEAR INTO WA_YEAR.
  ASSIGN  (WA_YEAR)  TO <YEAR>.
  MOVE  : P_AVRATE   TO <YEAR>.
ENDFORM.                    " SET_IT_ANMT
*&---------------------------------------------------------------------*
*&      Form  SET_IT_MTBT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*      -->P_WA_AVRATE  text
*----------------------------------------------------------------------*
FORM SET_IT_MTBT USING    P_ZMONTH
                          P_AVRATE.

  DATA: WA_MONTH LIKE FELD-NAME.

*  P_AVRATE = P_AVRATE * 100.

  CONCATENATE 'IT_MTBT-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN  (WA_MONTH) TO <MONTH>.
  MOVE  : P_AVRATE   TO <MONTH>.
ENDFORM.                    " SET_IT_MTBT
*&---------------------------------------------------------------------*
*&      Form  CAL_MTTR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_MTTR_DATA USING P_ZMONTH
                         P_AVRATE.

  DATA : WA_ZDOWNTIME LIKE ZTPM_MONBD-ZDOWNTIME,
         WA_ZBDNUM    LIKE ZTPM_BDNO-ZBDNUM.

  SELECT SINGLE ZDOWNTIME INTO WA_ZDOWNTIME
         FROM   ZTPM_MONBD
         WHERE  SHOP   = S_SHOP-LOW
         AND    AJAHR  = P_ZMONTH(4)
         AND    ZMONTH = P_ZMONTH+4(2).

  SELECT SINGLE ZBDNUM INTO WA_ZBDNUM
         FROM   ZTPM_BDNO
         WHERE  SHOP   = S_SHOP-LOW
         AND    AJAHR  = P_ZMONTH(4)
         AND    ZMONTH = P_ZMONTH+4(2).
  IF NOT WA_ZBDNUM IS INITIAL.
    P_AVRATE = WA_ZDOWNTIME / WA_ZBDNUM.
  ENDIF.
ENDFORM.                    " CAL_MTTR_DATA
*&---------------------------------------------------------------------*
*&      Form  CAL_MTBF_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_MTBF_DATA USING P_ZMONTH
                         P_AVRATE.

  DATA : WA_OPTIME LIKE ZTPM_OPTIME-OPTIME,
         WA_ZBDNUM    LIKE ZTPM_BDNO-ZBDNUM.

  SELECT SINGLE OPTIME INTO WA_OPTIME
         FROM   ZTPM_OPTIME
         WHERE  SHOP   = S_SHOP-LOW
         AND    AJAHR  = P_ZMONTH(4)
         AND    ZMONTH = P_ZMONTH+4(2).

  SELECT SINGLE ZBDNUM INTO WA_ZBDNUM
         FROM   ZTPM_BDNO
         WHERE  SHOP   = S_SHOP-LOW
         AND    AJAHR  = P_ZMONTH(4)
         AND    ZMONTH = P_ZMONTH+4(2).
  IF NOT WA_ZBDNUM IS INITIAL.
    P_AVRATE = WA_OPTIME / WA_ZBDNUM.
  ENDIF.
ENDFORM.                    " CAL_MTBF_DATA
*&---------------------------------------------------------------------*
*&      Form  CAL_MTTR_DATA_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_YEAR_YEAR  text
*      -->P_WA_AVRATE  text
*----------------------------------------------------------------------*
FORM CAL_MTTR_DATA_YEAR USING    P_YEAR
                                 P_AVRATE.

  DATA : WA_MONTH     TYPE MONTH.
  DATA : WA_TEMP      LIKE ZTPM_MTBT-AVRATE.
  DATA : WA_ZDOWNTIME LIKE ZTPM_MONBD-ZDOWNTIME,
         WA_ZBDNUM    LIKE ZTPM_BDNO-ZBDNUM.

  DO 12 TIMES.
    WA_MONTH = WA_MONTH + 1.

    CLEAR : WA_ZDOWNTIME, WA_ZBDNUM, WA_TEMP.
    IF P_YEAR = SY-DATUM(4) AND WA_MONTH > SY-DATUM+4(2).
      WA_MONTH = WA_MONTH - 1.
      EXIT.
    ENDIF.

    SELECT SUM( ZDOWNTIME ) INTO WA_ZDOWNTIME
           FROM   ZTPM_MONBD
           WHERE  SHOP   = S_SHOP-LOW
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = WA_MONTH.

    SELECT SUM( ZBDNUM ) INTO WA_ZBDNUM
           FROM   ZTPM_BDNO
           WHERE  SHOP   = S_SHOP-LOW
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = WA_MONTH.

    IF NOT WA_ZBDNUM IS INITIAL.
      WA_TEMP = WA_ZDOWNTIME / WA_ZBDNUM.
    ENDIF.
    P_AVRATE = P_AVRATE  + WA_TEMP.
  ENDDO.

  P_AVRATE = P_AVRATE / WA_MONTH.

ENDFORM.                    " CAL_MTTR_DATA_YEAR
*&---------------------------------------------------------------------*
*&      Form  CAL_MTBF_DATA_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_YEAR_YEAR  text
*      -->P_WA_AVRATE  text
*----------------------------------------------------------------------*
FORM CAL_MTBF_DATA_YEAR USING    P_YEAR
                                 P_AVRATE.

  DATA : WA_MONTH  TYPE MONTH.
  DATA : WA_TEMP   LIKE ZTPM_MTBT-AVRATE.
  DATA : WA_OPTIME LIKE ZTPM_OPTIME-OPTIME,
         WA_ZBDNUM LIKE ZTPM_BDNO-ZBDNUM.


  DO 12 TIMES.
    WA_MONTH = WA_MONTH + 1.

    CLEAR : WA_OPTIME, WA_ZBDNUM, WA_TEMP.
    IF P_YEAR = SY-DATUM(4) AND WA_MONTH > SY-DATUM+4(2).
      WA_MONTH = WA_MONTH - 1.
      EXIT.
    ENDIF.

    SELECT SUM( OPTIME ) INTO WA_OPTIME
           FROM   ZTPM_OPTIME
           WHERE  SHOP   = S_SHOP-LOW
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = WA_MONTH.

    SELECT SUM( ZBDNUM ) INTO WA_ZBDNUM
           FROM   ZTPM_BDNO
           WHERE  SHOP   = S_SHOP-LOW
           AND    AJAHR  = P_YEAR
           AND    ZMONTH = WA_MONTH.

    IF NOT WA_ZBDNUM IS INITIAL.
      WA_TEMP = WA_OPTIME / WA_ZBDNUM.
    ENDIF.
    P_AVRATE = P_AVRATE  + WA_TEMP.
  ENDDO.

  P_AVRATE = P_AVRATE / WA_MONTH.

ENDFORM.                    " CAL_MTBF_DATA_YEAR
*&---------------------------------------------------------------------*
*&      Form  READ_HEADER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_HEADER_DATA.
**** Read Shop name
  SELECT  SINGLE INNAM INTO WA_SHOP_NAME
          FROM  T024I
          WHERE INGRP = S_SHOP-LOW.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-005.
  ENDIF.
ENDFORM.                    " READ_HEADER_DATA
*&---------------------------------------------------------------------*
*&      Form  CAL_MTTR_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RANGE_ZMONTH  text
*      -->P_WA_AVRATE  text
*----------------------------------------------------------------------*
FORM CAL_MTTR_DATA2 USING    P_ZMONTH
                             P_AVRATE.

  DATA : IT_BDTIME LIKE ZSPM_BDTIME OCCURS 0 WITH HEADER LINE.
  DATA :  WA_S_DAY     LIKE SY-DATUM,
          WA_E_DAY     LIKE SY-DATUM.


*** Make Selection entry...
  CONCATENATE P_ZMONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.
*** Get No of breakdown
  CLEAR : WA_BDNUM.
  PERFORM GET_NO_BREAKDOWN USING WA_S_DAY
                                 WA_E_DAY
                                 WA_BDNUM.

**** Sum of Downtime
  CALL FUNCTION 'Z_FPM_CAL_BREAKDOWN_TIME_MON'
       EXPORTING
            I_MONTH = P_ZMONTH
            I_SHOP  = S_SHOP-LOW
            I_MAUEH = 'MIN'
       TABLES
            T_TIME  = IT_BDTIME.
  READ TABLE IT_BDTIME INDEX 1.
  IF SY-SUBRC EQ 0.
    IF WA_BDNUM  NE 0.
      P_AVRATE =  IT_BDTIME-ZDOWNTIME / WA_BDNUM.
    ENDIF.
  ENDIF.
ENDFORM.                    " CAL_MTTR_DATA2
*&---------------------------------------------------------------------*
*&      Form  CAL_MTBF_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RANGE_ZMONTH  text
*      -->P_WA_AVRATE  text
*----------------------------------------------------------------------*
FORM CAL_MTBF_DATA2 USING    P_ZMONTH
                             P_AVRATE.

  DATA : WA_OPTIME LIKE ZTPM_OPTIME-OPTIME.
  DATA :  WA_S_DAY     LIKE SY-DATUM,
          WA_E_DAY     LIKE SY-DATUM.

*** Make Selection entry...
  CONCATENATE P_ZMONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.
*** Get No of breakdown
  CLEAR: WA_BDNUM.
  PERFORM GET_NO_BREAKDOWN USING WA_S_DAY
                                 WA_E_DAY
                                 WA_BDNUM.
*  SELECT  COUNT( DISTINCT A~AUFNR )
*         INTO WA_BDNUM
*         FROM AFRU  AS A
*              INNER JOIN VIQMEL AS B
*              ON A~AUFNR = B~AUFNR
*         WHERE B~INGRP = S_SHOP-LOW
*         AND   A~AUERU EQ 'X'
*         AND   A~STOKZ EQ ' '
*         AND   A~STZHL EQ SPACE
*         AND   B~AUSVN BETWEEN  WA_S_DAY    "//Start of Malfunction
*                           AND  WA_E_DAY.

*** Get production operation time
  SELECT SINGLE OPTIME INTO WA_OPTIME
         FROM   ZTPM_OPTIME
         WHERE  SHOP   = S_SHOP-LOW
         AND    AJAHR  = P_ZMONTH(4)
         AND    ZMONTH = P_ZMONTH+4(2).
  IF SY-SUBRC EQ 0.
    IF WA_BDNUM  NE 0.
      P_AVRATE =  WA_OPTIME / WA_BDNUM.
    ENDIF.
  ENDIF.
ENDFORM.                    " CAL_MTBF_DATA2
*&---------------------------------------------------------------------*
*&      Module  SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_YEAR INPUT.
  PERFORM SELECT_YEAR.
ENDMODULE.                 " SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_YEAR.
  DATA: WA_SEL_YEAR LIKE  VVIS_SOPTI-SYEAR,
        WA_SEL_OK.

  CALL FUNCTION 'REAL_ESTATE_F4_YEAR'
   EXPORTING
*   I_YEAR              =
     I_POPUP_TITLE       = 'Select Year'
   IMPORTING
      E_YEAR              = WA_SEL_YEAR
      E_SEL_OK            = WA_SEL_OK
            .
  IF WA_SEL_OK EQ 'X'.
    P_YEAR = WA_SEL_YEAR.
  ENDIF.
ENDFORM.                    " SELECT_YEAR
*&---------------------------------------------------------------------*
*&      Form  GET_NO_BREAKDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_S_DAY  text
*      -->P_WA_E_DAY  text
*----------------------------------------------------------------------*
FORM GET_NO_BREAKDOWN USING    P_S_DAY
                               P_E_DAY
                               P_BDNUM.

  CASE S_SHOP-LOW.
    WHEN 'P10'.
      SELECT  COUNT( DISTINCT A~AUFNR )
             INTO P_BDNUM
             FROM AFRU  AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
             WHERE B~INGRP = S_SHOP-LOW
             AND   A~AUERU EQ 'X'
             AND   A~STOKZ EQ ' '
             AND   A~STZHL EQ SPACE
             AND   B~STORT IN ('PS1', 'PS2')
             AND   B~AUSVN BETWEEN  P_S_DAY    "//Start of Malfunction
                               AND  P_E_DAY.
    WHEN 'P20' OR 'P30' OR 'P40'.
      SELECT  COUNT( DISTINCT A~AUFNR )
             INTO P_BDNUM
             FROM AFRU  AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
             WHERE B~INGRP = S_SHOP-LOW
             AND   A~AUERU EQ 'X'
             AND   A~STOKZ EQ ' '
             AND   A~STZHL EQ SPACE
             AND   B~AUSVN BETWEEN  P_S_DAY    "//Start of Malfunction
                               AND  P_E_DAY.
    WHEN 'P50'.
      SELECT  COUNT( DISTINCT A~AUFNR )
         INTO P_BDNUM
         FROM AFRU  AS A
              INNER JOIN VIQMEL AS B
              ON A~AUFNR = B~AUFNR
         WHERE B~INGRP = S_SHOP-LOW
         AND   A~AUERU EQ 'X'
         AND   A~STOKZ EQ ' '
         AND   A~STZHL EQ SPACE
         AND   B~STORT  = 'V00'
         AND   B~AUSVN BETWEEN  P_S_DAY    "//Start of Malfunction
                           AND  P_E_DAY.
  ENDCASE.

ENDFORM.                    " GET_NO_BREAKDOWN
