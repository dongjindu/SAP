************************************************************************
* Program Name      : ZRPM07_PMRO
* Author            : Myoungho Park
* Creation Date     : 2003.10.01.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Maintenance Ratio Report
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


REPORT  ZRPM07_PMRO   MESSAGE-ID DEMOOFFICEINTEGRATIO.

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
DATA: WA_INIT.
**********************************************************************
**********************************************************************

*---Global Variables & Tables

FIELD-SYMBOLS: <MONTH> .

DATA: FIELDS_TABLE TYPE TABLE OF RFC_FIELDS.

TABLES: ZTPM_PMRO,  "//Monthly Breakdown Rate (Planed)
        ZSPM_PMRO,  "//average breakdown rate by Month
        ZSPM_MONTH,
        T024I.       "//Maintenance planner groups
*        ZTPM_SHOP.

DATA: BEGIN OF IT_ZTPM_PMRO OCCURS 0,
        MEINS   LIKE    ZTPM_PMRO-MEINS,
        SHOP    LIKE	ZTPM_PMRO-SHOP,
        AJAHR(4) TYPE C,  "//LIKE	ZTPM_PMRO-AJAHR,
        ZMONTH(2) TYPE C,  "//LIKE	ZTPM_PMRO-MONTH,
        AUART	LIKE	ZTPM_PMRO-AUART,
*          MEINS	LIKE	ZTPM_PMRO-MEINS,
        ZPLAND	LIKE	ZTPM_PMRO-ZPLAND,
        ZACTAL	LIKE	ZTPM_PMRO-ZACTAL,
        ZPTIME	LIKE	ZTPM_PMRO-ZPTIME,
      END OF IT_ZTPM_PMRO.

DATA: IT_ZTPM_PMRO2 LIKE IT_ZTPM_PMRO OCCURS 0 WITH HEADER LINE.

DATA: IT_TEMP LIKE IT_ZTPM_PMRO OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_ZVPM_PMRO OCCURS 0.
        INCLUDE STRUCTURE ZVPM_PMRO.
DATA:   MONTH(6),
      END OF IT_ZVPM_PMRO.

*DATA: IT_PLAN LIKE ZTPM_PLAN OCCURS 0 WITH HEADER LINE.
*DATA: IT_TEMP_PMRO LIKE ZSPM_PMRO OCCURS 0.
*DATA: IT_PMRO LIKE ZSPM_PMRO OCCURS 0 WITH HEADER LINE.

DATA : IT_TEMP_PMRO LIKE ZSPM_MONTH2 OCCURS 100.
DATA : IT_PMRO LIKE ZSPM_MONTH2 OCCURS 100 WITH HEADER LINE.
DATA : IT_YEAR_TEMP LIKE ZSPM_VALUE2 OCCURS 100.
DATA : IT_YEAR_PMRO LIKE ZSPM_VALUE2 OCCURS 100 WITH HEADER LINE.


DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP  LIKE ZTPM_SHOP-SHOP,
        SHTXT LIKE ZTPM_SHOP-SHTXT,
      END OF IT_SHOP.

DATA: BEGIN OF IT_RANGE OCCURS 0,
         YEAR  LIKE ZSPM_PARAM-AJAHR,
         MONTH LIKE ZSPM_PARAM-MONTH,
      END OF IT_RANGE.

DATA: BEGIN OF IT_AUART OCCURS 0,
        AUART LIKE T003O-AUART,
      END OF IT_AUART.

DATA: BEGIN OF IT_TYPE OCCURS 0,
        AUART LIKE T003O-AUART,
      END OF IT_TYPE.

DATA : WA_ZPLAND LIKE ZTPM_PMRO-ZPLAND.
DATA : WA_ZACTAL LIKE ZTPM_PMRO-ZACTAL.
DATA : WA_ZPTIME LIKE ZTPM_PMRO-ZPTIME.

DATA : WA_INTERVAL  TYPE I,
       WA_HOURS     TYPE I,
       WA_SUM_HOURS TYPE I,
       WA_DAILY    TYPE STRING,

       WA_PLANNED         TYPE STRING,
       WA_Y_TOTAL_PLANNED TYPE STRING,

       WA_ACTUAL         TYPE STRING,
       WA_Y_TOTAL_ACTUAL TYPE STRING,

       WA_VALUE         TYPE STRING,
       WA_Y_TOTAL_VALUE TYPE STRING,

       WA_BREAKDOWN         TYPE STRING,
       WA_Y_TOTAL_BREAKDOWN TYPE STRING.

DATA: WA_FIRST_DAY  LIKE SY-DATUM,
      WA_LAST_DAY  LIKE SY-DATUM,
      WA_FROM_DATE LIKE SY-DATUM,
      WA_TO_DATE   LIKE SY-DATUM,
      CAL_DATE     LIKE SY-DATUM,
      WA_COUNT     LIKE T5A4A-DLYMO,
      WA_MONTH     LIKE ZTPM_MONBD-ZMONTH,
      WA_PP_YEAR   LIKE ZSPM_PARAM-AJAHR,
      WA_PP_MONTH  LIKE ZSPM_PARAM-MONTH.


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
PARAMETER : P_YEAR LIKE ZSPM_PARAM-AJAHR DEFAULT SY-DATUM(4).
*PARAMETER : P_MONTH LIKE ZTPM_PMRO-ZMONTH DEFAULT SY-DATUM(6).
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
*      P_PLACE = ' '.
      WA_INIT = 'X'.
      P_YEAR = SY-DATUM(4).
*      P_MONTH = SY-DATUM(6).
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
**** Create Container Control 'CONTAINER_04'
    CREATE OBJECT CONTAINER
              EXPORTING CONTAINER_NAME = 'CONTAINER_04'.
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
****  Link Server name SUFFIX : 'PMRO'
    CALL METHOD LINK_SERVER->START_LINK_SERVER
                     EXPORTING  LINK_SERVER_MODE =
                                LINK_SERVER->LINK_SERVER_CUSTOMNAME
                                SERVER_NAME_SUFFIX = 'PMRO'
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

    IF SY-DYNNR = '0100'.
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

  IF NOT LINK_SERVER IS INITIAL.
    DATA: ERROR TYPE REF TO I_OI_ERROR.

    IT_TEMP_PMRO[] = IT_PMRO[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Maintenance'
                       ITEM_TITLE  = 'Maintenance Ratio'
                       DDIC_NAME   = 'ZSPM_MONTH2'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_TEMP_PMRO.

    IT_YEAR_TEMP[] = IT_YEAR_PMRO[].
    CALL METHOD LINK_SERVER->ADD_TABLE_ITEM2
             EXPORTING ITEM_NAME   = 'Prev_Year_Total'
                       ITEM_TITLE  = 'Prev Year Total'
                       DDIC_NAME   = 'ZSPM_VALUE2'
                       NO_FLUSH    = 'X'
             IMPORTING RETCODE = RETCODE
             CHANGING  DATA_TABLE   = IT_YEAR_TEMP.

**** Report date
    CALL METHOD LINK_SERVER->ADD_STRING_ITEM
             EXPORTING ITEM_NAME   = 'report_date'
                       ITEM_VALUE  = SY-DATUM
                       NO_FLUSH    = ''
             IMPORTING ERROR = ERROR.
  ENDIF.

ENDFORM.                    " REFRESH_LINKS
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
      CHECK P_YEAR NE SPACE.
*** Refresh Links ...
      PERFORM REFRESH_LINKS.
*** Open document...
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
        DOC_OBJECT_KEY TYPE SBDST_OBJECT_KEY VALUE 'MAINTENANCE',
        DOC_MIMETYPE LIKE BAPICOMPON-MIMETYPE.

  CLEAR DOC_URL.

  WA_DOC_SIGNATURE-PROP_NAME  = 'DESCRIPTION'.
  WA_DOC_SIGNATURE-PROP_VALUE = 'Maintenance Ratio'.
  APPEND WA_DOC_SIGNATURE TO DOC_SIGNATURE.

  CALL METHOD BDS_INSTANCE->GET_INFO
                EXPORTING CLASSNAME = DOC_CLASSNAME
                          CLASSTYPE = DOC_CLASSTYPE
                          OBJECT_KEY = DOC_OBJECT_KEY
                CHANGING COMPONENTS = DOC_COMPONENTS
                         SIGNATURE  = DOC_SIGNATURE
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
*&      Form  FREE_OBJECT
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
*&      Form  READ_CONDITION_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CONDITION_DATA.
  CLEAR : IT_ZTPM_PMRO, IT_ZTPM_PMRO[].
  CLEAR : IT_TEMP, IT_TEMP[].

*** get all data from accumulated table ZTPM_PMRO by year
*** Monthly Breakdown Rate

  SELECT *
         INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
         FROM  ZTPM_PMRO
         WHERE AJAHR = P_YEAR.

*** CHECK ORDER TYPE...
*** PLANNED MAINTENANCE TYPE
*** CONVERT 'PM01', 'PM03', 'PM05' TO 'PM01'
  LOOP AT IT_TEMP.
    IF IT_TEMP-AUART = 'PM01' OR
       IT_TEMP-AUART = 'PM03' OR
       IT_TEMP-AUART = 'PM05'.
      MOVE 'PM01' TO IT_TEMP-AUART.
    ENDIF.
**** Accumulate Values by Order Type and period...
    MOVE-CORRESPONDING  IT_TEMP TO IT_ZTPM_PMRO.
    COLLECT IT_ZTPM_PMRO.
  ENDLOOP.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = SY-DATUM
            DAYS      = 0
            MONTHS    = 2
            SIGNUM    = '-'
            YEARS     = 0
       IMPORTING
            CALC_DATE = CAL_DATE.

  WA_PP_YEAR  = CAL_DATE(4).
  WA_PP_MONTH = CAL_DATE+4(2).



*** Totalization vlaues and readjust datum to display...
  LOOP AT IT_SHOP.
**** Daily Inspection No. of Actual/Planned...
**** Order Type 'PM04'
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
             IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_NUMBER_OF_MAINTEANCE2  USING IT_SHOP-SHOP
                                                 IT_RANGE-YEAR
                                                 IT_RANGE-MONTH
                                                 'PM04'.
      ELSE.
        PERFORM GET_NUMBER_OF_MAINTEANCE  USING IT_SHOP-SHOP
                                                IT_RANGE-YEAR
                                                IT_RANGE-MONTH
                                                'PM04'.
      ENDIF.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.

      WA_Y_TOTAL_ACTUAL = WA_Y_TOTAL_ACTUAL  + WA_ACTUAL.
      WA_Y_TOTAL_PLANNED = WA_Y_TOTAL_PLANNED + WA_PLANNED.

    ENDLOOP.

    CONCATENATE WA_Y_TOTAL_ACTUAL '/'  WA_Y_TOTAL_PLANNED
                INTO WA_VALUE.

    PERFORM CONVERT_ROW_TO_COLUMN USING '13'
                                        WA_VALUE.
    APPEND IT_PMRO.
    CLEAR : IT_PMRO, WA_Y_TOTAL_ACTUAL, WA_Y_TOTAL_PLANNED.

**** Daily Inspection Actual Working Time....
**** Order Type 'PM04'
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
         IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_ACTUAL_WORKING_TIME2  USING IT_SHOP-SHOP
                                                 IT_RANGE-YEAR
                                                 IT_RANGE-MONTH
                                                 'PM04'.
      ELSE.
        PERFORM GET_ACTUAL_WORKING_TIME USING IT_SHOP-SHOP
                                              IT_RANGE-YEAR
                                              IT_RANGE-MONTH
                                              'PM04'.
      ENDIF.
      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.

      WA_Y_TOTAL_VALUE = WA_Y_TOTAL_VALUE + WA_VALUE.
    ENDLOOP.

    PERFORM CONVERT_ROW_TO_COLUMN USING '13'
                                    WA_VALUE.
    APPEND IT_PMRO.
    CLEAR : IT_PMRO, WA_Y_TOTAL_VALUE.

**** Planned Mainteance No. of Actual/Planned...
**** Order Type 'PM01'
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_NUMBER_OF_MAINTEANCE2  USING IT_SHOP-SHOP
                                                 IT_RANGE-YEAR
                                                 IT_RANGE-MONTH
                                                 'PM01'.
      ELSE.
        PERFORM GET_NUMBER_OF_MAINTEANCE  USING IT_SHOP-SHOP
                                                IT_RANGE-YEAR
                                                IT_RANGE-MONTH
                                                'PM01'.
      ENDIF.
      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.

      WA_Y_TOTAL_ACTUAL = WA_Y_TOTAL_ACTUAL  + WA_ACTUAL.
      WA_Y_TOTAL_PLANNED = WA_Y_TOTAL_PLANNED + WA_PLANNED.

    ENDLOOP.

    CONCATENATE WA_Y_TOTAL_ACTUAL '/'  WA_Y_TOTAL_PLANNED
                INTO WA_VALUE.

    PERFORM CONVERT_ROW_TO_COLUMN USING '13'
                                        WA_VALUE.
    APPEND IT_PMRO.
    CLEAR : IT_PMRO, WA_Y_TOTAL_ACTUAL, WA_Y_TOTAL_PLANNED.

**** Planned Mainteance working time...
**** Order Type 'PM01'
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_ACTUAL_WORKING_TIME2  USING IT_SHOP-SHOP
                                                 IT_RANGE-YEAR
                                                 IT_RANGE-MONTH
                                                 'PM01'.
      ELSE.
        PERFORM GET_ACTUAL_WORKING_TIME USING IT_SHOP-SHOP
                                                IT_RANGE-YEAR
                                                IT_RANGE-MONTH
                                                'PM01'.
      ENDIF.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.

      WA_Y_TOTAL_VALUE = WA_Y_TOTAL_VALUE + WA_VALUE.
    ENDLOOP.

    PERFORM CONVERT_ROW_TO_COLUMN USING '13'
                                    WA_VALUE.
    APPEND IT_PMRO.
    CLEAR : IT_PMRO, WA_Y_TOTAL_VALUE.

**** Breakdown Mainteance  No. of Actual....
**** Order Type 'PM02'
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_NUMBER_OF_MAINTEANCE2 USING IT_SHOP-SHOP
                                                IT_RANGE-YEAR
                                                IT_RANGE-MONTH
                                                'PM02'.
      ELSE.
        PERFORM GET_NUMBER_OF_MAINTEANCE USING IT_SHOP-SHOP
                                               IT_RANGE-YEAR
                                               IT_RANGE-MONTH
                                               'PM02'.
      ENDIF.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.
      WA_Y_TOTAL_ACTUAL = WA_Y_TOTAL_ACTUAL  + WA_ACTUAL.
      WA_Y_TOTAL_PLANNED = WA_Y_TOTAL_PLANNED + WA_PLANNED.

    ENDLOOP.

    CONCATENATE WA_Y_TOTAL_ACTUAL '/'  WA_Y_TOTAL_PLANNED
                INTO WA_VALUE.

    PERFORM CONVERT_ROW_TO_COLUMN USING '13'
                                        WA_VALUE.
    APPEND IT_PMRO.
    CLEAR : IT_PMRO, WA_Y_TOTAL_ACTUAL, WA_Y_TOTAL_PLANNED.


**** Breakdown Mainteance working time...
**** Order Type 'PM02'
    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.
      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_ACTUAL_WORKING_TIME2 USING IT_SHOP-SHOP
                                               IT_RANGE-YEAR
                                               IT_RANGE-MONTH
                                               'PM02'.
      ELSE.
        PERFORM GET_ACTUAL_WORKING_TIME USING IT_SHOP-SHOP
                                              IT_RANGE-YEAR
                                              IT_RANGE-MONTH
                                              'PM02'.
      ENDIF.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.

      WA_Y_TOTAL_VALUE = WA_Y_TOTAL_VALUE + WA_VALUE.
    ENDLOOP.

    PERFORM CONVERT_ROW_TO_COLUMN USING '13'
                                    WA_VALUE.
    APPEND IT_PMRO.
    CLEAR : IT_PMRO, WA_Y_TOTAL_VALUE.

**** Mainteance ratio = Sum of Preventive working time
****                    / Sum of Breakdown working time
**** Precentive = Sum of Daily Inspection + Sum of Planned Maintenace
**** Based on actual working time...

    LOOP AT IT_RANGE.
      WA_MONTH = SY-TABIX.

      CLEAR :WA_VALUE.

      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_ACTUAL_WORKING_TIME2 USING IT_SHOP-SHOP
                                               IT_RANGE-YEAR
                                               IT_RANGE-MONTH
                                               'PM04'.
      ELSE.
        PERFORM GET_ACTUAL_WORKING_TIME USING IT_SHOP-SHOP
                                              IT_RANGE-YEAR
                                              IT_RANGE-MONTH
                                              'PM04'.
      ENDIF.
      MOVE : WA_VALUE TO WA_DAILY.

*******
      CLEAR :WA_VALUE.

      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_ACTUAL_WORKING_TIME2 USING IT_SHOP-SHOP
                                               IT_RANGE-YEAR
                                               IT_RANGE-MONTH
                                               'PM01'.
      ELSE.
        PERFORM GET_ACTUAL_WORKING_TIME USING IT_SHOP-SHOP
                                              IT_RANGE-YEAR
                                              IT_RANGE-MONTH
                                              'PM01'.
      ENDIF.
      MOVE : WA_VALUE TO WA_PLANNED.

********
      CLEAR :WA_VALUE.
      IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
         ( IT_RANGE-YEAR > WA_PP_YEAR ).
        PERFORM GET_ACTUAL_WORKING_TIME2 USING IT_SHOP-SHOP
                                               IT_RANGE-YEAR
                                               IT_RANGE-MONTH
                                               'PM02'.
      ELSE.
        PERFORM GET_ACTUAL_WORKING_TIME USING IT_SHOP-SHOP
                                              IT_RANGE-YEAR
                                              IT_RANGE-MONTH
                                              'PM02'.
      ENDIF.
      MOVE : WA_VALUE TO WA_BREAKDOWN .

      CLEAR : WA_ACTUAL.
      WA_ACTUAL = WA_DAILY + WA_PLANNED.

      CONCATENATE  WA_ACTUAL '/' WA_BREAKDOWN INTO WA_VALUE.

      PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                          WA_VALUE.

      WA_Y_TOTAL_ACTUAL = WA_Y_TOTAL_ACTUAL  + WA_ACTUAL.
      WA_Y_TOTAL_BREAKDOWN = WA_Y_TOTAL_BREAKDOWN + WA_BREAKDOWN.

    ENDLOOP.

    CONCATENATE WA_Y_TOTAL_ACTUAL '/'  WA_Y_TOTAL_BREAKDOWN
                INTO WA_VALUE.

    PERFORM CONVERT_ROW_TO_COLUMN USING '13'
                                        WA_VALUE.
    APPEND IT_PMRO.
    CLEAR : IT_PMRO, WA_Y_TOTAL_ACTUAL, WA_Y_TOTAL_BREAKDOWN.
  ENDLOOP.

ENDFORM.                    " READ_CONDITION_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_IT_MONBD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_ZMONTH  text
*      -->P_IT_TEMP_AVRATE  text
*----------------------------------------------------------------------*
FORM SET_IT_MONBD USING    P_ZMONTH
                           P_AVRATE.
  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_MONBD-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN  (WA_MONTH) TO <MONTH>.
  MOVE  : P_AVRATE  TO <MONTH>.
ENDFORM.                    " SET_IT_MONBD
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
*&      Form  CONVERT_ROW_TO_COLUMN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MONTH  text
*      -->P_WA_VALUE  text
*----------------------------------------------------------------------*
FORM CONVERT_ROW_TO_COLUMN USING  P_MONTH
                                  P_VALUE.

  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_PMRO-MONTH' P_MONTH INTO WA_MONTH.
  ASSIGN  (WA_MONTH) TO <MONTH>.
  MOVE  :  P_VALUE  TO <MONTH>.

ENDFORM.                    " SET_PLANNED_TASK_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER_OF_MAINTEANCE
*&---------------------------------------------------------------------*
*       Number of documents occured with each order type
*       Daily Inspection -> 'PM04'
*       Planned Maintenance -> 'PM01', 'PM03', 'PM05' => 'PM01'
*       Breakdown Maintenance -> 'PM04'
*----------------------------------------------------------------------*
*      -->P_IT_RANGE_MONTH  text
*      -->P_IT_SHOP_SHOP  text
*----------------------------------------------------------------------*
FORM GET_NUMBER_OF_MAINTEANCE  USING    P_SHOP
                                        P_YEAR
                                        P_MONTH
                                        P_TYPE.

  CLEAR: WA_ACTUAL, WA_PLANNED,  WA_VALUE.
  CLEAR : IT_ZTPM_PMRO.
  READ TABLE IT_ZTPM_PMRO WITH KEY SHOP  = P_SHOP
                                  AJAHR  = P_YEAR
                                  ZMONTH = P_MONTH
                                  AUART  = P_TYPE.
*  IF P_TYPE EQ 'P02'.
*    MOVE IT_ZTPM_PMRO-ZACTAL TO WA_VALUE.
*  ELSE.
  MOVE IT_ZTPM_PMRO-ZPLAND TO WA_PLANNED.
  MOVE IT_ZTPM_PMRO-ZACTAL TO WA_ACTUAL.
*    CONCATENATE  WA_ACTUAL '/' WA_PLANNED INTO WA_VALUE.
*  ENDIF.
ENDFORM.                    "GET_NUMBER_OF_MAINTEANCE
*&---------------------------------------------------------------------*
*&      Form  set_select_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SELECT_ENTRIES.
  CLEAR : IT_SHOP, IT_SHOP[].
  CLEAR : IT_TYPE, IT_TYPE[].
  CLEAR : IT_RANGE, IT_RANGE[].

*** make Shop code selection entries
  SELECT  DISTINCT INGRP AS SHOP
                   INNAM AS SHTXT
          INTO  CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  T024I
          WHERE INGRP LIKE 'P%'
          ORDER BY SHOP.

*** make order type selection entries
  MOVE : 'PM03' TO IT_TYPE-AUART.
  APPEND IT_TYPE.
  MOVE : 'PM01' TO IT_TYPE-AUART.
  APPEND IT_TYPE.
  MOVE : 'PM02' TO IT_TYPE-AUART.
  APPEND IT_TYPE.

*** make first day of select condition
  CONCATENATE P_YEAR '0101' INTO WA_FIRST_DAY.

  MOVE : WA_FIRST_DAY(4)     TO IT_RANGE-YEAR,
         WA_FIRST_DAY+4(2)   TO IT_RANGE-MONTH.
  APPEND IT_RANGE.

*** make range select entries (year/month)
  DO 11 TIMES.
    MOVE SY-INDEX TO WA_COUNT.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              DATE      = WA_FIRST_DAY
              DAYS      = 0
              MONTHS    = WA_COUNT
              SIGNUM    = '+'
              YEARS     = 0
         IMPORTING
              CALC_DATE = CAL_DATE.
    MOVE : CAL_DATE(4)     TO IT_RANGE-YEAR,
           CAL_DATE+4(2)   TO IT_RANGE-MONTH.
    APPEND IT_RANGE.
  ENDDO.

*  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*       EXPORTING
*            DAY_IN            = CAL_DATE
*       IMPORTING
*            LAST_DAY_OF_MONTH = WA_LAST_DAY.

  SORT IT_RANGE BY MONTH ASCENDING.
ENDFORM.                    " set_select_entries
*&---------------------------------------------------------------------*
*&      Form  READ_PREV_YEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PREV_YEAR_DATA.
  CLEAR: IT_YEAR_PMRO, IT_YEAR_PMRO[].

  LOOP AT IT_SHOP.
    LOOP AT IT_TYPE.
**** Daily Inspection No. of Actual
      PERFORM GET_NUMBER_OF_MAINTEANCE_YEAR USING IT_SHOP-SHOP
                                                  IT_RANGE-YEAR
                                                  IT_TYPE-AUART.
      MOVE : WA_ZACTAL TO IT_YEAR_PMRO-ZVALUE.
      APPEND IT_YEAR_PMRO. CLEAR : IT_YEAR_PMRO.

      IF IT_TYPE-AUART NE 'PM02'.
        PERFORM GET_NUMBER_OF_MAINTEANCE_YEAR USING IT_SHOP-SHOP
                                                    IT_RANGE-YEAR
                                                    IT_TYPE-AUART.
        MOVE : WA_ZPLAND TO IT_YEAR_PMRO-ZVALUE.
        APPEND IT_YEAR_PMRO. CLEAR : IT_YEAR_PMRO.
      ENDIF.

**** Daily Inspection Actual Working Time....
      PERFORM GET_ACTUAL_WORKING_TIME_YEAR USING IT_SHOP-SHOP
                                                 IT_RANGE-YEAR
                                                 IT_TYPE-AUART.
      MOVE : WA_ZPTIME TO IT_YEAR_PMRO-ZVALUE.
      APPEND IT_YEAR_PMRO. CLEAR : IT_YEAR_PMRO.
    ENDLOOP.
  ENDLOOP.


ENDFORM.                    " READ_PREV_YEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_MONTH.
*  DATA SPMON LIKE S051-SPMON.
*
*  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
*    EXPORTING
*      ACTUAL_MONTH                     = SY-DATUM(6)
**   FACTORY_CALENDAR                 = ' '
**   HOLIDAY_CALENDAR                 = ' '
*     LANGUAGE                         = SY-LANGU
*     START_COLUMN                     = 8
*     START_ROW                        = 5
*   IMPORTING
*     SELECTED_MONTH                   = SPMON
**   RETURN_CODE                      =
*   EXCEPTIONS
*     FACTORY_CALENDAR_NOT_FOUND       = 1
*     HOLIDAY_CALENDAR_NOT_FOUND       = 2
*     MONTH_NOT_FOUND                  = 3
*     OTHERS                           = 4
*            .
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  IF NOT SPMON IS INITIAL.
*    P_MONTH = SPMON.
*  ENDIF.

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
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
*** set selection entries
  PERFORM SET_SELECT_ENTRIES.

*** get previous year data
  PERFORM READ_PREV_YEAR_DATA.

*  PERFORM READ_CONDITION_DATA.

*** get codtion data
  PERFORM READ_CONDITION_DATA2.

*  PERFORM READ_YEAR_TOTAL_DATA.

ENDFORM.                    " READ_DATA
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
*&      Form  GET_ACTUAL_WORKING_TIME
*&---------------------------------------------------------------------*
*       Number of documents occured with each order type
*       Daily Inspection -> 'PM04'
*       Planned Maintenance -> 'PM01', 'PM03', 'PM05' => 'PM01'
*       Breakdown Maintenance -> 'PM04'
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_RANGE_MONTH  text
*      -->P_1129   text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_WORKING_TIME USING    P_SHOP
                                      P_YEAR
                                      P_MONTH
                                      P_TYPE.
  CLEAR: WA_VALUE.
  CLEAR: IT_ZTPM_PMRO.


  READ TABLE IT_ZTPM_PMRO WITH KEY SHOP  = P_SHOP
                                  AJAHR  = P_YEAR
                                  ZMONTH = P_MONTH
                                  AUART  = P_TYPE.

  MOVE : IT_ZTPM_PMRO-ZPTIME TO WA_VALUE.

*  WRITE IT_ZTPM_PMRO-ZPTIME TO WA_VALUE
*                                       UNIT 'MIN'   "IT_ZTPM_PMRO-MEINS
*                                       EXPONENT 0.

ENDFORM.                    " GET_ACTUAL_WORKING_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER_OF_MAINTEANCE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_RANGE_MONTH  text
*      -->P_1159   text
*----------------------------------------------------------------------*
FORM GET_NUMBER_OF_MAINTEANCE2 USING    P_SHOP
                                        P_YEAR
                                        P_MONTH
                                        P_TYPE.

  DATA : IT_PMROX LIKE ZTPM_PMRO OCCURS 0 WITH HEADER LINE.

  CLEAR : IT_PMROX, IT_PMROX[].
  CLEAR : IT_TEMP, IT_TEMP[].
  CLEAR : IT_ZTPM_PMRO2, IT_ZTPM_PMRO2[].
  CLEAR : IT_AUART, IT_AUART[].

  MOVE : P_TYPE TO IT_AUART-AUART.
  APPEND IT_AUART.

  IF P_TYPE EQ 'PM01'.
    MOVE : 'PM03' TO IT_AUART-AUART.
    APPEND IT_AUART.
    MOVE : 'PM05' TO IT_AUART-AUART.
    APPEND IT_AUART.
  ENDIF.

  CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
       EXPORTING
            I_SHOP   = P_SHOP
            I_AJAHR  = P_YEAR
            I_ZMONTH = P_MONTH
       TABLES
            T_PMRO   = IT_PMROX
            T_AUART  = IT_AUART.

  CLEAR: WA_ACTUAL, WA_PLANNED,  WA_VALUE.
  CLEAR : IT_PMROX.
  READ TABLE IT_PMROX WITH KEY SHOP   = P_SHOP
                               AJAHR  = P_YEAR
                               ZMONTH = P_MONTH
                               AUART  = P_TYPE.

*  IF P_TYPE EQ 'P02'.
*    MOVE IT_PMROX-ZACTAL TO WA_VALUE.
*  ELSE.
  MOVE IT_PMROX-ZPLAND TO WA_PLANNED.
  MOVE IT_PMROX-ZACTAL TO WA_ACTUAL.
*    CONCATENATE  WA_ACTUAL '/' WA_PLANNED INTO WA_VALUE.
*  ENDIF.

*  LOOP AT IT_PMROX.
*    MOVE-CORRESPONDING IT_PMROX TO IT_TEMP.
*    APPEND IT_TEMP.
*  ENDLOOP.

**** CONVERT 'PM01', 'PM03', 'PM05' TO 'PM01'
*  LOOP AT IT_TEMP.
*    IF IT_TEMP-AUART = 'PM01' OR
*       IT_TEMP-AUART = 'PM03' OR
*       IT_TEMP-AUART = 'PM05'.
*      MOVE 'PM01' TO IT_TEMP-AUART.
*    ENDIF.
***** Accumulate Values by Order Type and period...
*    MOVE-CORRESPONDING  IT_TEMP TO IT_ZTPM_PMRO2.
*    COLLECT IT_ZTPM_PMRO2.
*  ENDLOOP.

*  CLEAR: WA_ACTUAL, WA_PLANNED,  WA_VALUE.
*  CLEAR : IT_ZTPM_PMRO2.
*  READ TABLE IT_ZTPM_PMRO2 WITH KEY SHOP   = P_SHOP
*                                    AJAHR  = P_YEAR
*                                    ZMONTH = P_MONTH
*                                    AUART  = P_TYPE.

*  IF P_TYPE EQ 'P02'.
*    MOVE IT_ZTPM_PMRO2-ZACTAL TO WA_VALUE.
*  ELSE.
*    MOVE IT_ZTPM_PMRO2-ZPLAND TO WA_PLANNED.
*    MOVE IT_ZTPM_PMRO2-ZACTAL TO WA_ACTUAL.
*    CONCATENATE  WA_ACTUAL '/' WA_PLANNED INTO WA_VALUE.
*  ENDIF.
ENDFORM.                    " GET_NUMBER_OF_MAINTEANCE2
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_WORKING_TIME2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_RANGE_MONTH  text
*      -->P_1276   text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_WORKING_TIME2 USING    P_SHOP
                                       P_YEAR
                                       P_MONTH
                                       P_TYPE.

  DATA : IT_PMROX LIKE ZTPM_PMRO OCCURS 0 WITH HEADER LINE.

  CLEAR : IT_PMROX, IT_PMROX[].
  CLEAR : IT_TEMP, IT_TEMP[].
  CLEAR : IT_ZTPM_PMRO2, IT_ZTPM_PMRO2[].
  CLEAR : IT_AUART, IT_AUART[].

  MOVE : P_TYPE TO IT_AUART-AUART.
  APPEND IT_AUART.

  IF P_TYPE EQ 'PM01'.
    MOVE : 'PM03' TO IT_AUART-AUART.
    APPEND IT_AUART.
    MOVE : 'PM05' TO IT_AUART-AUART.
    APPEND IT_AUART.
  ENDIF.

  CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
       EXPORTING
            I_SHOP   = P_SHOP
            I_AJAHR  = P_YEAR
            I_ZMONTH = P_MONTH
       TABLES
            T_PMRO   = IT_PMROX
            T_AUART  = IT_AUART.

  CLEAR: WA_ACTUAL, WA_PLANNED,  WA_VALUE.
  CLEAR : IT_PMROX.
  READ TABLE IT_PMROX WITH KEY SHOP   = P_SHOP
                               AJAHR  = P_YEAR
                               ZMONTH = P_MONTH
                               AUART  = P_TYPE.

  MOVE : IT_PMROX-ZPTIME TO WA_VALUE.
ENDFORM.                    " GET_ACTUAL_WORKING_TIME2
*&---------------------------------------------------------------------*
*&      Form  READ_YEAR_TOTAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_YEAR_TOTAL_DATA.

ENDFORM.                    " READ_YEAR_TOTAL_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_CONDITION_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CONDITION_DATA2.
  CLEAR : IT_ZTPM_PMRO, IT_ZTPM_PMRO[].
*  CLEAR : IT_TEMP, IT_TEMP[].

*** get all data from accumulated table ZTPM_PMRO by year
*** Monthly Breakdown Rate
  SELECT *
         INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_PMRO
         FROM  ZTPM_PMRO
         WHERE AJAHR = P_YEAR.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = SY-DATUM
            DAYS      = 0
            MONTHS    = 2
            SIGNUM    = '-'
            YEARS     = 0
       IMPORTING
            CALC_DATE = CAL_DATE.

  WA_PP_YEAR  = CAL_DATE(4).
  WA_PP_MONTH = CAL_DATE+4(2).

  LOOP AT IT_SHOP.
    LOOP AT IT_TYPE.
**** Daily Inspection No. of Actual
      LOOP AT IT_RANGE.
        WA_MONTH = SY-TABIX.
        IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
               IT_RANGE-MONTH > WA_PP_MONTH ) OR
           ( IT_RANGE-YEAR > WA_PP_YEAR ).
          PERFORM GET_NUMBER_OF_MAINTEANCE2  USING IT_SHOP-SHOP
                                                   IT_RANGE-YEAR
                                                   IT_RANGE-MONTH
                                                   IT_TYPE-AUART.
        ELSE.
          PERFORM GET_NUMBER_OF_MAINTEANCE  USING IT_SHOP-SHOP
                                                  IT_RANGE-YEAR
                                                  IT_RANGE-MONTH
                                                  IT_TYPE-AUART.
        ENDIF.
**** append Actual Daily Inspection
        PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                            WA_ACTUAL.
      ENDLOOP.
      APPEND IT_PMRO. CLEAR : IT_PMRO.

      IF IT_TYPE-AUART NE 'PM02'.
**** Daily Inspection No. of Planned...
        LOOP AT IT_RANGE.
          WA_MONTH = SY-TABIX.
          IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
                 IT_RANGE-MONTH > WA_PP_MONTH ) OR
             ( IT_RANGE-YEAR > WA_PP_YEAR ).
            PERFORM GET_NUMBER_OF_MAINTEANCE2  USING IT_SHOP-SHOP
                                                     IT_RANGE-YEAR
                                                     IT_RANGE-MONTH
                                                     IT_TYPE-AUART.
          ELSE.
            PERFORM GET_NUMBER_OF_MAINTEANCE  USING IT_SHOP-SHOP
                                                    IT_RANGE-YEAR
                                                    IT_RANGE-MONTH
                                                    IT_TYPE-AUART.
          ENDIF.
**** append Plan Daily Inspection
          PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                              WA_PLANNED.
        ENDLOOP.
        APPEND IT_PMRO. CLEAR : IT_PMRO.
      ENDIF.

**** Daily Inspection Actual Working Time....
      LOOP AT IT_RANGE.
        WA_MONTH = SY-TABIX.
        IF ( IT_RANGE-YEAR = WA_PP_YEAR AND
           IT_RANGE-MONTH > WA_PP_MONTH ) OR
           ( IT_RANGE-YEAR > WA_PP_YEAR ).
          PERFORM GET_ACTUAL_WORKING_TIME2  USING IT_SHOP-SHOP
                                                   IT_RANGE-YEAR
                                                   IT_RANGE-MONTH
                                                   IT_TYPE-AUART.
        ELSE.
          PERFORM GET_ACTUAL_WORKING_TIME USING IT_SHOP-SHOP
                                                IT_RANGE-YEAR
                                                IT_RANGE-MONTH
                                                IT_TYPE-AUART.
        ENDIF.
        PERFORM CONVERT_ROW_TO_COLUMN USING WA_MONTH
                                            WA_VALUE.

      ENDLOOP.
      APPEND IT_PMRO. CLEAR : IT_PMRO.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " READ_CONDITION_DATA2
*&---------------------------------------------------------------------*
*&      Module  SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_YEAR INPUT.
  PERFORM SELECT_YEAR.
ENDMODULE.                 " SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER_OF_MAINTEANCE_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_TYPE_AUART  text
*----------------------------------------------------------------------*
FORM GET_NUMBER_OF_MAINTEANCE_YEAR USING    P_SHOP
                                            P_YEAR
                                            P_AUART.

  CLEAR : WA_ZPLAND, WA_ZACTAL.

  SELECT SINGLE SUM( ZPLAND ) SUM( ZACTAL )
         INTO (WA_ZPLAND, WA_ZACTAL)
         FROM  ZTPM_PMRO
         WHERE SHOP  = P_SHOP
         AND   AJAHR = P_YEAR
         AND   AUART = P_AUART.

ENDFORM.                    " GET_NUMBER_OF_MAINTEANCE_YEAR
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_WORKING_TIME_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_RANGE_YEAR  text
*      -->P_IT_TYPE_AUART  text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_WORKING_TIME_YEAR USING    P_SHOP
                                           P_YEAR
                                           P_AUART.
  CLEAR : WA_ZPTIME.

  SELECT SINGLE SUM( ZPTIME )
         INTO  WA_ZPTIME
         FROM  ZTPM_PMRO
         WHERE SHOP  = P_SHOP
         AND   AJAHR = P_YEAR
         AND   AUART = P_AUART.
ENDFORM.                    " GET_ACTUAL_WORKING_TIME_YEAR
